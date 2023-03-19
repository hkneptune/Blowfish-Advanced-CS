(*
 * Copyright 1997-2005 Markus Hahn 
 * 
 * Licensed under the Apache License, Version 2.0 (the "License"); 
 * you may not use this file except in compliance with the License. 
 * You may obtain a copy of the License at 
 * 
 *     http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *)

{
  to work with .BFA files
}

unit BFAWorkWith;

interface
uses
  Windows,
  Classes,
  SysUtils,
  CallBack,
  MessageCallBack,
  StringRes,
  KeyCache,
  Favorites,
  CipherManager,
  RandomManager,
  Configuration,
  Options;


// stupid errors
type
  EBFAWorkWithError     = class(Exception);
  EBFAWorkWithWrongKey  = class(EBFAWorkWithError);
  EBFAWorkWithUserBreak = class(EBFAWorkWithError);


// our viewer class
type
  TBFAWorkWith = class
  private
    m_favorites : TFavorites;
    m_sr        : TStrRes;
    m_opts      : TOptions;
    m_config    : TConfigurationSection;
    m_cipMng    : TCipherManager;
    m_rndMng    : TRandomManager;
    m_kcache    : TKeyCache;

  public
    // constructor
    // -> string resources
    // -> options
    // -> configuration
    // -> key cache
    // -> cipher manager
    // -> random manager
    constructor Create(sr : TStrRes;
                       opts : TOptions;
                       cfg : TConfiguration;
                       kcache : TKeyCache;
                       cipMng : TCipherManager;
                       rndMng : TRandomManager);

    // destructor
    destructor Destroy; override;

    // gets the favorites
    // <- favorites (ref. only)
    function GetFavorites : TFavorites;

    // sets/gets the max. number of favorites
    // -> new max. number (ignored if -1)
    // <- max. number of favorites
    function NumOfMaxFavs(nNewMax : Integer = -1) : Integer;

    // let's the user work with a file (it's going to be decrypted and opened,
    // then edited by the user, after a confirmation reencrypted)
    // -> name of the encrypted file
    // -> callback for simple progress reporting
    // -> callback to show the wait message
    // -> callback to get a key
    // -> window handle (used for opening the decrypted file)
    // -> True: add the file to the favorites / False: do not
    // exception EBFAWorkWithWrongKey wrong key passed
    // exception EBFAWorkWithError if something other went wrong
    procedure Execute(const sFileName : String;
                      reportCB : TCallBack;
                      confirmCB : TMessageCallBack;
                      passwInput : TPasswordInput;
                      hWnd : THandle;
                      blAddToFavorites : Boolean = True);

  end;


implementation
uses
  Globals,
  BFAFile,
  FileSupp,
  SecureMem,
  Wipe,
  BFManager,
  General,
  StringPlusI;



//////////////////////////// TBFAWorkWith ////////////////////////////

const
  CONFIG_ID = 'BFAWORKWITH';
  BFAWW_CFGID_NUMOFMAXFAVS = 'NUMOFMAXFAVS';

const
  DEF_NUMOFMAXFAVS = 24;  // enough?


// configuration checker
type
  TBFAWorkWithCC = class(TConfigurationChecker)
  private
    m_parent : TBFAWorkWith;
  public
    constructor Create(parent : TBFAWorkWith);
    procedure RunCheck(section : TConfigurationSection); override;
  end;

constructor TBFAWorkWithCC.Create(parent : TBFAWorkWith);
begin
  m_parent:=parent;
end;

procedure TBFAWorkWithCC.RunCheck(section : TConfigurationSection);
begin
  CheckInt(section, BFAWW_CFGID_NUMOFMAXFAVS, DEF_NUMOFMAXFAVS);
end;


constructor TBFAWorkWith.Create(sr : TStrRes;
                                opts : TOptions;
                                cfg : TConfiguration;
                                kcache : TKeyCache;
                                cipMng : TCipherManager;
                                rndMng : TRandomManager);
var
  cc : TBFAWorkWithCC;
begin
  m_sr:=sr;
  m_opts:=opts;

  cc:=TBFAWorkWithCC.Create(Self);
  m_config:=cfg.GetSection(CONFIG_ID, cc);
  cc.Destroy;

  m_kcache:=kcache;
  m_cipMng:=cipMng;
  m_rndMng:=rndMng;

  m_favorites:=TFavorites.Create(cfg, CONFIG_ID);
end;


destructor TBFAWorkWith.Destroy;
begin
  m_favorites.Destroy;
end;

function TBFAWorkWith.GetFavorites : TFavorites;
begin
  Result:=m_favorites;
end;


function TBFAWorkWith.NumOfMaxFavs(nNewMax : Integer = -1) : Integer;
var
  nI    : Integer;
  nMax  : Integer;
  nToRm : Integer;
begin
  nMax:=m_config.GetIntegerOption(BFAWW_CFGID_NUMOFMAXFAVS);
  if (nNewMax = -1) then begin
    Result:=nMax;
    Exit;
  end;

  m_config.FixIntegerOption(BFAWW_CFGID_NUMOFMAXFAVS, nNewMax);
  Result:=nNewMax;

  nToRm:=m_favorites.GetCount - nNewMax;

  // remove the oldest entries (favorites are not sorted, so we're just
  // removing those items with the lowest indexes)
  for nI:=1 to nToRm do
    m_favorites.Remove((m_favorites.GetCount - 1), False);

  m_favorites.StoreToConfig;
end;



// progress mapper for encryption and decryption
type
  TProgressMapper = class(TBFAFileProgress)
  private
    m_nLastPercent : Integer;
    m_sr           : TStrRes;
  public
    constructor Create(callbackObj : TObject;
                       sr : TStrRes); reintroduce; overload;
    procedure ResetPercent;
    procedure CallBack; override;
  end;


constructor TProgressMapper.Create(callbackObj : TObject;
                                   sr : TStrRes);
begin
  inherited Create(callbackObj);
  m_sr:=sr;
  ResetPercent;
end;

procedure TProgressMapper.ResetPercent;
begin
  m_nLastPercent:=-1;
end;


procedure TProgressMapper.CallBack;
var
  nPercent     : Integer;
  blDoCallBack : Boolean;
  sMessID      : String;
  cb           : TCallBack;
begin
  blDoCallBack:=GetChanged;

  // wiping just causes one callback
  cb:=TCallback(GetCallBackObj);
  if (GetProgressState = BFAFILE_PROGRESS_WIPE) then begin
    if (blDoCallBack) then begin
      cb.SetMessage(Format(m_sr.Get(CONFIG_ID, 'FRMT_WIPCB'),
                           [GetInputFileName]));
      cb.CallBack;
      Exit;
    end;
  end;

  // all other ops are shown more precisely
  if (GetMaxPos = 0) then begin
    blDoCallBack:=True;
    nPercent:=0;
  end
  else begin
    nPercent:=Integer((GetActPos * 100) div GetMaxPos);
    if (nPercent > m_nLastPercent) then begin
      blDoCallBack:=True;
      m_nLastPercent:=nPercent;
    end;
  end;

  if blDoCallBack then begin
    cb:=TCallBack(GetCallBackObj);

    case GetProgressState of
      BFAFILE_PROGRESS_ENCRYPT : sMessID:='FRMT_ENCCB';
      BFAFILE_PROGRESS_DECRYPT : sMessID:='FRMT_DECCB';
    end;
    cb.SetMessage(Format(m_sr.Get(CONFIG_ID, sMessID),
                         [GetInputFileName,
                          TStrPlusI.Sepa1000(m_sr, GetFileSize),
                          nPercent]));
    // (interrupts will be tunneled through)
    cb.CallBack;
  end;

end;



procedure TBFAWorkWith.Execute(const sFileName : String;
                               reportCB : TCallBack;
                               confirmCB : TMessageCallBack;
                               passwInput : TPasswordInput;
                               hWnd : THandle;
                               blAddToFavorites : Boolean = True);
var
  blOpenError : Boolean;
  bencset     : TBFAFileEncryptSetup;
  bdecset     : TBFAFileDecryptSetup;
  pmapp       : TProgressMapper;
  bffile      : TBFAFile;
  bencres     : TBFAFileEncryptResult;
  bdecres     : TBFAFileDecryptResult;
  key         : TKeyMemory;
  wipeObj     : TWipe;

procedure CleanUp;
begin
  if (bffile <> Nil) then
    bffile.Destroy;
  if (bdecres <> Nil) then
    bdecres.Destroy;
  if (bencres <> Nil) then
    bencres.Destroy;
  if (wipeObj <> Nil) then
    wipeObj.Destroy;
  key.Destroy;
  bdecset.Destroy;
  if (bencset <> Nil) then
    bencset.Destroy;
  pmapp.Destroy;
end;

begin

  // can we deliver a key right out of the cache?
  if (m_kcache.GetLastSuccess and m_kcache.IsValid) then begin
    // (FIXME: is a timeout possible between IsValid() and GetKey() ?)
    key:=m_kcache.GetKey.CloneAsKeyMemory;
  end
  else begin
    // so we have to request a key
    key:=passwInput.Execute(PASSWINPUT_MODE_WORKWITH);
    if (key = Nil) then
      Exit;
  end;

  // assume that the key's alright
  m_kcache.SetLastSuccess(True);

  // prepare the progress mapper
  pmapp:=TProgressMapper.Create(reportCB, m_sr);

  // create the decryption setup
  bdecset:=TBFAFileDecryptSetup.Create;
  with bdecset, m_opts.GetCfg do begin
    SetPassword(key);
    SetTargetPath('');   // (stay where we are)
    SetRemoveSource(False);
    SetOverwriteExisting(False);  // (we cannot take any risks here!)
    SetNoCache(GetBooleanOption(OPTIONS_CFGID_NOCACHE));
    SetKeepDirectories(False);
    SetBasePath('');
    SetIgnoreCRC32(GetBooleanOption(OPTIONS_CFGID_IGNORECRC32));
    SetRestorePath(False);
//    SetNoKeyCheck(GetBooleanOption(OPTIONS_CFGID_NOKEYCHECK));
    SetNoKeyCheck(False);
    SetAcceptTruncated(False);  // (same here, too)
    SetFileInfoOnly(False);
    SetDirectHeaderInfo(True);  // important!
  end;

  // now decrypt that guy
  bffile:=Nil;
  bdecres:=Nil;
  bencres:=Nil;
  bencset:=Nil;
  wipeObj:=Nil;
  try
    bffile:=TBFAFile.Create(_globals.GetCipMng.GetCurrentCipher,
                            m_sr,
                            _globals.GetRndMng.GetRandomSource);
    bdecres:=bffile.Decrypt(sFileName,
                            bdecset,
                            pmapp);
  except
    on ebwp : EBFAFileWrongPassword do begin
      CleanUp;
      m_kcache.SetLastSuccess(False);
      raise EBFAWorkWithWrongKey.Create(ebwp.Message);
    end;
    on ebi : EBFAFileInterrupted do begin
      CleanUp;
      raise EBFAWorkWithUserBreak.Create(ebi.Message);
    end;
    on ebe : EBFAFileError do begin
      // does thw file not exist?
      if (not FileExists(sFileName)) then begin
        // if the file is registered in the favorites we ask to remove it
        if (m_favorites.GetIndexOf(sFileName) <> -1) then begin
          with confirmCB do begin
            SetKindOf(MCB_KINDOF_ERROR);
            SetStyle(MCB_STYLE_YESNO);
            SetMessage(Format(m_sr.Get(CONFIG_ID, 'RMFROMFAV'), [sFileName]));
            CallBack;
            if (GetResult = MCB_RES_YES) then
              m_favorites.Remove(sFileName);
          end;
          Exit;  // (take a nice exit in this case)
        end
      end;
      CleanUp;
      raise EBFAWorkWithError.Create(ebe.Message);
    end;
  end;

  // open it
  blOpenError:=False;
  try
    TFileSupport.OpenFile(bdecres.GetOriginalFileName,
                          hWnd,
                          m_sr);
  except
    on efse : EFileSupportError do begin
      blOpenError:=True;

      // better to ask what to do now
      with confirmCB do begin
        SetKindOf(MCB_KINDOF_ERROR);
        SetStyle(MCB_STYLE_YESNO);
        SetMessage(Format(m_sr.Get(CONFIG_ID, 'OPENERROR'),
                          [bdecres.GetOriginalFileName, efse.Message]));
        CallBack;
        if (GetResult = MCB_RES_NO) then begin
          CleanUp;
          raise EBFAWorkWithUserBreak.Create(efse.Message);
        end;
      end;
    end;
  end;

  // now wait until the user has finished to work with the file
  if (not blOpenError) then begin
    with confirmCB do begin
      SetKindOf(MCB_KINDOF_QUESTION);
      SetStyle(MCB_STYLE_YESNO);
      SetMessage(Format(m_sr.Get(CONFIG_ID, 'KEEPWAITING'),
                        [bdecres.GetOriginalFileName]));
      CallBack;
      if (GetResult = MCB_RES_NO) then begin
        CleanUp;
        raise EBFAWorkWithUserBreak.Create(m_sr.Get(CONFIG_ID, 'USERBREAK'));
      end;
    end;
  end;

  // time to reencrypt that thing...

  bencset:=TBFAFileEncryptSetup.Create;
  with bencset, m_opts.GetCfg do begin
    SetPassword(key);
    SetTargetPath('');   // (stay where we are)
    SetRemoveSource(False);
    SetOverwriteExisting(False);  // (we cannot take any risks here!)
    SetNoCache(GetBooleanOption(OPTIONS_CFGID_NOCACHE));
    SetKeepDirectories(False);
    SetBasePath('');
    SetRename(False);        // (doesn't matter)
    SetRandomRename(False);  // (doesn't matter)
    SetMaskName('');         // (doesn't matter)
    SetMaskNumber(0);        // (doesn't matter)
    SetMaskExt('');          // (doesn't matter)
    SetTryRename83(False);   // (doesn't matter)
    SetAddExtension(False);  // (doesn't matter)
    SetKeepDateTime(GetBooleanOption(OPTIONS_CFGID_KEEPDATETIME));
    SetKeepAttributes(GetBooleanOption(OPTIONS_CFGID_KEEPATTRIBUTES));
    SetStorePath(False);  // (doesn't matter)
    SetRelativePaths(False);   // (doesn't matter)
    SetSkipEncrypted(False);   // (maybe a nested thing)
//    SetNoKeyHash(GetBooleanOption(OPTIONS_CFGID_NOKEYHASH));
    SetNoKeyHash(False);
    SetWriteProtectAfter(GetBooleanOption(OPTIONS_CFGID_WRITEPROTECT));
    SetNoCompressTypes(Nil);  // (not relevant here)
    // use the old cryptfile name
    SetForceFileName(ExtractFileName(sFileName));
    // keep file name and compression as they were stored in the header
    SetHeaderFileName(bdecres.GetDirectHeaderInfo.GetFileName);
    SetCompress(bdecres.GetDirectHeaderInfo.GetCompression);
  end;

  try
    case m_opts.GetCfg.GetIntegerOption(OPTIONS_CFGID_WIPING) of
      BFM_WIPE_SIMPLE : wipeObj:=TWipeSimple.Create(m_rndMng.GetRandomSource);
      BFM_WIPE_DOD    : wipeObj:=TWipeDOD.Create(m_rndMng.GetRandomSource);
      BFM_WIPE_SFS    : wipeObj:=TWipeSFS.Create;
    else
      wipeObj:=TWipeDeleteOnly.Create;
    end;
  except
    on eoom : EOutOfMemory do begin
      CleanUp;
      raise EBFAWorkWithError.Create(eoom.Message);
    end;
  end;

  try
    bencres:=bffile.Encrypt(bdecres.GetOriginalFileName,
                            wipeObj,
                            bencset,
                            pmapp);
  except
    on ebi : EBFAFileInterrupted do begin
      CleanUp;
      raise EBFAWorkWithUserBreak.Create(ebi.Message);
    end;
    on ebe : EBFAFileError do begin
      CleanUp;
      raise EBFAWorkWithError.Create(ebe.Message);
    end;
  end;

  // finally done
  CleanUp;

  // add the filename to the favorites, i. n.
  if (blAddToFavorites) then begin

    m_favorites.Add(sFileName);
    // (tricky: check for maximum overflow with a logical shortfuse)
    NumOfMaxFavs(NumOfMaxFavs(-1));

  end;

end;



end.
