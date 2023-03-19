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
  viewer for .BFA files
}

unit BFAViewer;

interface
uses
  Windows,
  Classes,
  SysUtils,
  CallBack,
  SecureMem,
  MessageCallBack,
  StringRes,
  BFManager,
  PathSearch,
  KeyCache,
  Options;


// stupid errors
type
  EBFAViewerError     = class(Exception);
  EBFAViewerWrongKey  = class(EBFAViewerError);
  EBFAViewerUserBreak = class(EBFAViewerError);


// our viewer class
type
  TBFAViewer = class
  private
    m_fileList : TStringList;
    m_sr       : TStrRes;
    m_opts     : TOptions;
    m_kcache   : TKeyCache;

  public
    // constructor
    // -> string resources
    // -> options
    // -> key cache
    constructor Create(sr : TStrRes;
                       opts : TOptions;
                       kcache : TKeyCache);

    // destructor
    destructor Destroy; override;

    // clears all viewed files
    // -> callback to report the progress
    // -> confirmation callback
    // <- True: success / False: some files could not be destroyed
    function Clear(cb : TCallBack;
                   confirmCB : TMessageCallBack) : Boolean;


    // views a file
    // -> name of the encrypted file
    // -> callback for simple progress reporting
    // -> callback to get a key
    // -> window handle (used for opening the decrypted file)
    // exception EBFAUserBreak user canceled something
    // exception EBFAViewerWrongKey wrong key passed
    // exception EBFAViewError if something other went wrong
    procedure ViewFile(const sFileName : String;
                       reportCB : TCallBack;
                       passwInput : TPasswordInput;
                       hWnd : THandle);

  end;


implementation
uses
  GlobalsGUI,
  IntLists,
  WipeManager,
  BFAFile,
  FileSupp,
  StringPlusI;



//////////////////////////// TBFAViewer ////////////////////////////

const
  STRRES_ID = 'BFAViewer';


constructor TBFAViewer.Create(sr : TStrRes;
                              opts : TOptions;
                              kcache : TKeyCache);
begin
  m_sr:=sr;
  m_opts:=opts;
  m_kcache:=kcache;
  m_fileList:=TStringList.Create;
  m_fileList.Duplicates:=dupIgnore;
end;


destructor TBFAViewer.Destroy;
begin
  m_fileList.Destroy;
end;


// callback for the wipe manager
type
  TWipeProgressMapper = class(TBFWorkProgress)
  public
    procedure CallBack; override;
  end;


procedure TWipeProgressMapper.CallBack;
var
  cb : TCallback;
  sr : TStrRes;
begin
  // just show the name and size of the file which is currently wiped
  cb:=TCallBack(GetCallBackObj);
  if (GetChanged) then begin
    sr:=_globals.GetSr;
    cb.SetMessage(Format(sr.Get(STRRES_ID, 'FRMT_WIPECB'),
                         [GetFileName, TStrPlusI.Sepa1000(sr, GetFileSize)]));
  end;

  // (no exception handling here, a user break is just tunneled through)
  cb.CallBack;
end;


function TBFAViewer.Clear(cb : TCallBack;
                          confirmCB : TMessageCallBack) : Boolean;
var
  nI      : Integer;
  nUpIdx  : Integer;
  files   : TPathSearchContainer;
  fsizes  : TWORD64List;
  wpRes   : TWipeResults;
  wpSetup : TWipeSetup;
  wpMng   : TWipeManager;
  wpmapp  : TWipeProgressMapper;
begin

  // nothing to wipe?
  if (m_fileList.Count = 0) then begin
    Result:=True;
    Exit;
  end;

  // make the progress mapper
  wpmapp:=TWipeProgressMapper.Create(cb);

  // we use wipe manager (with all its overhead) to get rid of the files
  wpMng:=TWipeManager.Create(_globals.GetRndMng.GetRandomSource,
                             BFM_NOMAXERRORS,
                             wpmapp,
                             confirmCB,
                             m_sr);

  // prepare the wipe setup
  files:=TPathSearchContainer.Create(m_sr);
  fsizes:=TWORD64List.Create;
  nUpIdx:=m_fileList.Count - 1;
  for nI:=0 to nUpIdx do begin
    try
      files.AddSingleFile(m_fileList.Strings[nI], Nil, fsizes);
    except
      on EPathSearchError do begin
        // not there anymore? don't care!
      end;
    end;
  end;
  wpSetup:=TWipeSetup.Create(files,
                             fsizes,
                             m_opts.
                               GetCfg.GetIntegerOption(OPTIONS_CFGID_WIPING));

  // and the result keeper
  wpRes:=TWipeResults.Create(m_sr);

  // wipe out
  try
    wpMng.WipeFiles(wpSetup, wpRes);
    Result:=(wpRes.GetNumOfErrors = 0);
  except
    on EBFError do begin
      Result:=False;
    end;
  end;

  // cleanup
  wpRes.Destroy;
  wpSetup.Destroy;
  wpMng.Destroy;
  wpmapp.Destroy;

  // no more files to clear now
  m_fileList.Clear;
end;



// progress mapper for decryption
type
  TDecryptProgressMapper = class(TBFAFileProgress)
  private
    m_nLastPercent : Integer;
  public
    constructor Create(callbackObj : TObject); override;
    procedure ResetPercent;
    procedure CallBack; override;
  end;


constructor TDecryptProgressMapper.Create(callbackObj : TObject);
begin
  inherited Create(callbackObj);
  ResetPercent;
end;

procedure TDecryptProgressMapper.ResetPercent;
begin
  m_nLastPercent:=-1;
end;


procedure TDecryptProgressMapper.CallBack;
var
  blDoCallBack : Boolean;
  nPercent     : Integer;
  cb           : TCallBack;
  sr           : TStrRes;
begin
  blDoCallBack:=False;

  if (GetChanged) then
    blDoCallBack:=True;

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
    sr:=_globals.getSr;
    cb.SetMessage(Format(sr.Get(STRRES_ID, 'FRMT_VIEWCB'),
                         [GetInputFileName,
                          TStrPlusI.Sepa1000(sr, GetFileSize),
                          nPercent]));
    // (interrupts will be tunneled through)
    cb.CallBack;
  end;

end;



procedure TBFAViewer.ViewFile(const sFileName : String;
                              reportCB : TCallBack;
                              passwInput : TPasswordInput;
                              hWnd : THandle);
var
  nI     : Integer;
  sTemp  : String;
  bset   : TBFAFileDecryptSetup;
  dmapp  : TDecryptProgressMapper;
  bffile : TBFAFile;
  bres   : TBFAFileDecryptResult;
  key    : TKeyMemory;

procedure CleanUp;
begin
  if (bffile <> Nil) then
    bffile.Destroy;
  key.Destroy;
  bset.Destroy;
  dmapp.Destroy;
end;

begin

  // can we deliver a key right out of the cache?
  if (m_kcache.GetLastSuccess and m_kcache.IsValid) then begin
    // (FIXME: is a timeout possible between IsValid() and GetKey() ?)
    key:=m_kcache.GetKey.CloneAsKeyMemory;
  end
  else begin
    // so we have to request a key
    key:=passwInput.Execute(PASSWINPUT_MODE_VIEW);
    if (key = Nil) then
      Exit;
  end;

  // assume that the key's alright
  m_kcache.SetLastSuccess(True);

  // prepare the progress mapper
  dmapp:=TDecryptProgressMapper.Create(reportCB);

  // create the setup
  bset:=TBFAFileDecryptSetup.Create;
  with bset, m_opts.GetCfg do begin
    SetPassword(key);
    SetTargetPath(GetStringOption(OPTIONS_CFGID_TEMPVIEWPATH));
    SetRemoveSource(False);
    SetOverwriteExisting(GetBooleanOption(OPTIONS_CFGID_OVERWRITEVIEWED));
    SetNoCache(GetBooleanOption(OPTIONS_CFGID_NOCACHE));
    SetKeepDirectories(False);
    SetBasePath('');
    SetIgnoreCRC32(GetBooleanOption(OPTIONS_CFGID_IGNORECRC32));
    SetRestorePath(False);
//    SetNoKeyCheck(GetBooleanOption(OPTIONS_CFGID_NOKEYCHECK));
    SetNoKeyCheck(False);
    SetAcceptTruncated(GetBooleanOption(OPTIONS_CFGID_ACCEPTTRUNCATED));
    SetFileInfoOnly(False);
    SetDirectHeaderInfo(False);
  end;

  // now decrypt that guy
  bffile:=Nil;
  try
    bffile:=TBFAFile.Create(_globals.GetCipMng.GetCurrentCipher,
                            m_sr,
                            _globals.GetRndMng.GetRandomSource);
    bres:=bffile.Decrypt(sFileName,
                         bset,
                         dmapp);
  except
    on ebwp : EBFAFileWrongPassword do begin
      CleanUp;
      m_kcache.SetLastSuccess(False);
      raise EBFAViewerWrongKey.Create(ebwp.Message);
    end;
    on ebi : EBFAFileInterrupted do begin
      CleanUp;
      raise EBFAViewerUserBreak.Create(ebi.Message);
    end;
    on ebe : EBFAFileError do begin
      CleanUp;
      raise EBFAViewerError.Create(ebe.Message);
    end;
  end;

  // need to wipe it later
  nI:=0;
  sTemp:=AnsiUpperCase(bres.GetOriginalFileName);
  while (nI < m_fileList.Count) do begin
    if (AnsiUpperCase(m_fileList.Strings[nI]) = sTemp) then
      Break;
    Inc(nI);
  end;
  if (nI = m_fileList.Count) then
    m_fileList.Add(bres.GetOriginalFileName);

  // showing means opening it
  try
    TFileSupport.OpenFile(bres.GetOriginalFileName,
                          hWnd,
                          m_sr);
    bres.Destroy;
  except
    on efse : EFileSupportError do begin
      bres.Destroy;
      raise EBFAViewerError.Create(efse.Message);
    end;
  end;

  CleanUp;
end;



end.
