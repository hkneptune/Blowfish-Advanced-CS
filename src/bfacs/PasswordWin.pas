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


(* multi functional password input window *)

unit PasswordWin;

{$I config.inc}

interface
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Menus,
  Options,
  KeyCache,
  SecureMem,
  RandomManager,
  CipherManager,
  Configuration,
  StringRes,
  StringPlus;


// the password form itself
type
  TPasswordFormStrResListener = class;
  TPasswordForm = class(TForm)
    PWUseCachedKeySwitch: TCheckBox;
    ButtonHolder: TPanel;
    OKBtn: TButton;
    CancelBtn: TButton;
    ApplyBtn: TButton;
    HelpBtn: TButton;
    PWInputInfo: TLabel;
    PWInputBox: TEdit;
    PWHolder: TPanel;
    PWShowPasswordSwitch: TCheckBox;
    PWAutoConfSwitch: TCheckBox;
    PWCacheKeySwitch: TCheckBox;
    PWMultiKeySwitch: TCheckBox;
    PWUseKeyDiskSwitch: TCheckBox;
    TPHolder: TPanel;
    TPPathInputBox: TEdit;
    TPUseTargetPathSwitch: TCheckBox;
    TPKeepDirectoriesSwitch: TCheckBox;
    TPBrowseBtn: TButton;
    TPLine: TBevel;
    TPRemoveSourceFilesSwitch: TCheckBox;
    EncHolder: TPanel;
    EncCompressSwitch: TCheckBox;
    EncRenameFilesSwitch: TCheckBox;
    EncStorePathNamesSwitch: TCheckBox;
    EncLine: TBevel;
    DecHolder: TPanel;
    DecLine: TBevel;
    DecRestorePathNamesSwitch: TCheckBox;
    EncRelativePathsSwitch: TCheckBox;
    ReencHolder: TPanel;
    ReencTopLine: TBevel;
    ReencAlgoInfo: TLabel;
    ReencAlgoList: TComboBox;
    PWIMenu: TPopupMenu;
    PWIMenu_PasteAndClear: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure PWUseCachedKeySwitchClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TPUseTargetPathSwitchClick(Sender: TObject);
    procedure PWShowPasswordSwitchClick(Sender: TObject);
    procedure PWAutoConfSwitchClick(Sender: TObject);
    procedure PWCacheKeySwitchClick(Sender: TObject);
    procedure PWMultiKeySwitchClick(Sender: TObject);
    procedure PWUseKeyDiskSwitchClick(Sender: TObject);
    procedure TPKeepDirectoriesSwitchClick(Sender: TObject);
    procedure ApplyBtnClick(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PWInputBoxKeyPress(Sender: TObject; var Key: Char);
    procedure TPPathInputBoxChange(Sender: TObject);
    procedure TPRemoveSourceFilesSwitchClick(Sender: TObject);
    procedure EncCompressSwitchClick(Sender: TObject);
    procedure EncStorePathNamesSwitchClick(Sender: TObject);
    procedure EncRenameFilesSwitchClick(Sender: TObject);
    procedure DecRestorePathNamesSwitchClick(Sender: TObject);
    procedure EncRelativePathsSwitchClick(Sender: TObject);
    procedure ReencAlgoListChange(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure PWIMenuPopup(Sender: TObject);
    procedure PWIMenu_PasteAndClearClick(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure TPBrowseBtnClick(Sender: TObject);
    procedure PWInputBoxChange(Sender: TObject);
    function FormHelp(Command: Word; Data: Integer;
      var CallHelp: Boolean): Boolean;
    procedure PWInputBoxKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    // additional members
    m_nExecNum         : Integer;
    m_nMode            : Integer;
    m_nApplyMask       : Integer;
    m_blKeyInCache     : Boolean;
    m_blAutoConfirmed  : Boolean;
    m_blShowInProgress : Boolean;
    m_config           : TConfigurationSection;
    m_opts             : TOptions;
    m_cipMng           : TCipherManager;
    m_password         : TKeyMemory;
    m_passwStrListener : TPasswordFormStrResListener;

    // to set the possibility to fix all current options
    // -> "apply level"
    procedure ApplyEnable(nLevel : Integer);

    // to enable/disable path input dependant controls
    // -> True: enable / False: disable
    procedure StatePathInputControls(blState : Boolean);

    // to enable/disable "use cached key" dependant controls
    // -> True: enable / False: disable
    procedure StateUseCachedKeyControls(blState : Boolean);

    // to enable/disable "show password" dependant controls
    // -> True: enable / False: disable
    procedure StateShowPasswordControls(blState : Boolean);

    // to enable/disable "use key disk" dependant controls
    // -> True: enable / False: disable
    procedure StateUseKeyDiskControls(blState : Boolean);

    // to enable/disable "store path" dependant controls
    // -> True: enable / False: disable
    procedure StateStorePathControls(blState : Boolean);

  public
    // get the _reference_ to the password
    // -> the caller will keep the password and we will forget
    //    it by clearing the internal reference
    // <- password (may be Nil)
    function GetPassword(blTakeOver : Boolean = False) : TKeyMemory;

    // clears a stored password
    procedure ClearPassword;

    // by setting the number of execution we can show the user which key #
    // is entered, but mainly to disable "first time only" controls
    // -> number of execution (starting with 0)
    procedure SetExecNum(nExecNum : Integer);

    // set the mode
    // -> mode, see PASSWORDINPUT_MODE_xxx
    procedure SetMode(nMode : Integer);

    // sets some objects needed
    // -> options cfg. section
    // -> cipher manager
    procedure SetObjects(opts : TOptions;
                         cipMng : TCipherManager);

    // to detect if a key's in the cache
    // -> True: key available / False: no key at all
    procedure SetKeyInCache(blState : Boolean);

  end;

// resource listener
  TPasswordFormStrResListener = class(TStrResListener)
  private
    m_theWin : TPasswordForm;
    m_nMode  : Integer;
  public
    constructor Create(theWin : TPasswordForm);
    procedure ChangeStrings(sr : TStrRes); override;
    procedure SetMode(nMode : Integer);
  end;



// due to multi keys, confirmations, key disks and other stuff we cannot just
// call the dialog, so we have to use a special handler for it - this handler
// is designed to be a global one and only instance

type
  TPasswordInputImpl = class(TPasswordInput)
  private
    // members
    m_keyCache : TKeyCache;
    m_opts     : TOptions;
    m_passwWin : TPasswordForm;
    m_rndMng   : TRandomManager;

  public
     // sets up this guy
     // -> where to load/store the options
     // -> cipher manager
     // -> where to get or cache the key, i. n.
     // -> the password form itself
     // -> random manager
     procedure Setup(opts : TOptions;
                     cipMng : TCipherManager;
                     keyCache : TKeyCache;
                     parent : TPasswordForm;
                     rndMng  : TRandomManager); reintroduce; overload;

     // executor
     // -> mode to show the password dialog, see PASSWINPUT_MODE_xxx
     // <- the key got (Nil equals error / user interrupt)
     function Execute(nMode : Integer) : TKeyMemory; override;
  end;



var
  PasswordForm: TPasswordForm;

implementation
uses
  FileCtrl,
  General,
  HtmlHelpAPI,
  Globals,
  MessageCallBack,
  ShortcutChecker,
  BFAFile,
  KeyDisk,
  clipbrd, 
  PasswConfirm,
  DestSelect, TipWin;

{$R *.DFM}


// the "borders" between simple and advanced styles
// (used by TPasswordInputImpl and TPasswordForm)
const
  MODE_BORDER  = PASSWINPUT_MODE_REENCRYPT2;
  MODE_BORDER2 = PASSWINPUT_MODE_SCANNER;


//////////////////////// TPasswordInputImpl ////////////////////////

// string resource ID
const
  STRRES_ID = 'PASSWORDINPUT';


procedure TPasswordInputImpl.Setup(opts : TOptions;
                                   cipMng : TCipherManager;
                                   keyCache : TKeyCache;
                                   parent : TPasswordForm;
                                   rndMng  : TRandomManager);
begin
  m_opts:=opts;
  m_keyCache:=keyCache;
  m_passwWin:=parent;
  m_passwWin.SetObjects(opts, cipMng);
  m_rndMng:=rndMng;
end;

// callback for keydisk retries
type
  TKeyDiskCB = class(TMessageCallBack)
  public
    procedure CallBack; override;
  end;


procedure TKeyDiskCB.CallBack;
var
  nStyle   : Integer;
  sCaption : String;
begin
  nStyle:=MB_YESNO;
  case GetKindOf of
    MCB_KINDOF_QUESTION : begin
      nStyle:=nStyle or MB_ICONQUESTION;
      sCaption:=_globals.GetSr.Get(STRRES_ID, 'CONFIRM');
    end;
    MCB_KINDOF_STOP : begin
      nStyle:=nStyle or MB_ICONSTOP;
      sCaption:=_globals.GetSr.Get(STRRES_ID, 'ERROR');
    end;
  end;

  if (Application.MessageBox(PChar(GetMessage),
                             PChar(sCaption),
                             nStyle) = IDNO) then
    SetResult(MCB_RES_NO)
  else
    SetResult(MCB_RES_YES);
end;


function TPasswordInputImpl.Execute(nMode : Integer) : TKeyMemory;
var
  nExecNum     : Integer;
  nOldSize     : Integer;
  nModalResult : Integer;
  nKeyLen      : Integer;
  blKeyInCache : Boolean;
  blDone       : Boolean;
  blConfirmed  : Boolean;
  sKeyFile     : String;
  kdcb         : TKeyDiskCB;
  appendToThis : TKeyMemory;
  newKeyMem    : TKeyMemory;


procedure BreakCleanUp;
begin
  // resume the key cache
  if ((blKeyInCache) and
      (not m_opts.GetCfg
                 .GetBooleanOption(OPTIONS_CFGID_KEYCACHENEVEREXPIRE))) then
    m_keyCache.ResumeTimer;

  // no result available on a break
  if (Result <> Nil) then
    Result.Destroy;
  Result:=Nil;
end;

procedure SetDialogCaption;
var
  sTemp : String;
begin
  // get the right caption
  with _globals.GetSr do
    case nMode of
      PASSWINPUT_MODE_KEYINPUT   : sTemp:=Get(STRRES_ID, 'MODE_KEYINPUT');
      PASSWINPUT_MODE_VIEW       : sTemp:=Get(STRRES_ID, 'MODE_VIEW');
      PASSWINPUT_MODE_WORKWITH   : sTemp:=Get(STRRES_ID, 'MODE_WORKWITH');
      PASSWINPUT_MODE_SCANNER    : sTemp:=Get(STRRES_ID, 'MODE_SCANNER');
      PASSWINPUT_MODE_REENCRYPT1 : sTemp:=Get(STRRES_ID, 'MODE_REENCRYPT');
      PASSWINPUT_MODE_REENCRYPT2 : sTemp:=Get(STRRES_ID, 'MODE_REENCRYPT');
      PASSWINPUT_MODE_ENCRYPT    : sTemp:=Get(STRRES_ID, 'MODE_ENCRYPT');
      PASSWINPUT_MODE_DECRYPT    : sTemp:=Get(STRRES_ID, 'MODE_DECRYPT');
    else
      // (this should never happen)
      sTemp:='???';
    end;

  // append the key number
  if (nExecNum > 0) then
    sTemp:=sTemp + Format(_globals.GetSr.Get(STRRES_ID, 'CAPTION_ADDON'),
                          [nExecNum + 1]);
  m_passwWin.Caption:=sTemp;
end;

begin
  // check if it's possible to use a previously entered key
  blKeyInCache:=m_keyCache.IsValid;

  // if so stop the expiration progress during the password input
  if ((blKeyInCache) and
      (not m_opts.GetCfg
                 .GetBooleanOption(OPTIONS_CFGID_KEYCACHENEVEREXPIRE))) then
    m_keyCache.StopTimer;

  // now cycle until we're done
  blConfirmed:=True;
  blDone:=False;
  Result:=Nil;
  nExecNum:=0;

  while (not blDone) do begin

    // show the password dialog
    SetDialogCaption;
    m_passwWin.SetExecNum(nExecNum);
    m_passwWin.SetMode(nMode);
    m_passwWin.SetKeyInCache(blKeyInCache);
    nModalResult:=m_passwWin.ShowModal;

    // [Cancel] pressed?
    if (nModalResult = mrCancel) then begin
      BreakCleanUp;
      Exit;
    end;

    // use cached key? (only of interest at the first fime)
    if ((nExecNum = 0) and
        blKeyInCache and
        m_opts.GetCfg.GetBooleanOption(OPTIONS_CFGID_USECACHEDKEY)) then begin

      // just deliver copy of the cached key, transfering it from encrypted
      // memory to a minor safe secure memory area directly
      Result:=m_keyCache.GetKey.CloneAsKeyMemory;

      // refresh the expiration time, then quit
      with m_opts.GetCfg do
        if (GetBooleanOption(OPTIONS_CFGID_KEYCACHENEVEREXPIRE)) then
          m_keyCache.SetExpirationTime(KEYCACHE_EXIPRETIME_NEVER)
        else
          m_keyCache.SetExpirationTime(
            GetIntegerOption(OPTIONS_CFGID_KEYCACHEEXPIRETIME));
      Exit;
    end
    else begin

      // key disk or password?
      if (m_opts.GetCfg.GetBooleanOption(OPTIONS_CFGID_USEKEYDISK)) then begin

        // just map most of the work to the key disk loader
        kdcb:=TKeyDiskCB.Create(Self);

        // (FIXME: should we check for a valid option?)
        sKeyFile:=_globals.GetOpts.GetCfg
                                  .GetStringOption(OPTIONS_CFGID_KEYDISKPATH);

        // appending and retries are automatically provided by
        // TKeyDisk.ReadDisk()
        newKeyMem:=TKeyDisk.ReadDisk(sKeyFile,
                                     _globals.GetSr,
                                     kdcb,
                                     Result);

        // success?
        if (newKeyMem = Nil) then begin
          // no, so leave now
          BreakCleanUp;
          Exit;
         end
        else begin
          // yes, set the new key
          if (Result <> Nil) then
            Result.Destroy;
          Result:=newKeyMem;
        end;

      end
      else begin

        // one unconfirmed key toggles the flag
        if (not PasswordForm.GetPassword.GetConfirmed) then
          blConfirmed:=False;

        // append (or just set) the password got
        nKeyLen:=PasswordForm.GetPassword.GetSize;
        if (Result = Nil) then begin
          // (take over the password)
          Result:=PasswordForm.GetPassword(True)
        end
        else begin
          nOldSize:=Result.GetSize;
          appendToThis:=TKeyMemory.Create(nOldSize + nKeyLen);
          appendToThis.SetData(Result.GetPtr, 0, nOldSize);
          appendToThis.SetData(PasswordForm.GetPassword.GetPtr,
                               nOldSize,
                               nKeyLen);
          PasswordForm.ClearPassword;
          Result.Destroy;
          Result:=appendToThis;
        end;
      end;

      // ask for another key, if necessary
      if (m_opts.GetCfg.GetBooleanOption(OPTIONS_CFGID_MULTIKEY)) then begin

        case Application.MessageBox(PChar(_globals.GetSr.Get(STRRES_ID,
                                                             'MULTIKEYREQ')),
                                    PChar(_globals.GetSr.Get(STRRES_ID,
                                                             'CONFIRM')),
                                    MB_ICONQUESTION or MB_YESNOCANCEL) of
          IDYES : Inc(nExecNum);
          IDNO  : blDone:=True;
        else
          BreakCleanUp;
          Exit;
        end;
      end
      else
        blDone:=True;

    end;
  end; (* OF WHILE *)

  // we now have a valid key reference, so delete the old key, i. n.
  if (blKeyInCache) then
    m_keyCache.Expire;

  // set the confirmed flag in the new key
  Result.SetConfirmed(blConfirmed);

  // set the new key (if the user wants that)
  if (m_opts.GetCfg.GetBooleanOption(OPTIONS_CFGID_CACHEKEY)) then begin
    with m_opts.GetCfg do begin
      if (GetBooleanOption(OPTIONS_CFGID_KEYCACHENEVEREXPIRE)) then
        m_keyCache.SetKey(Result, KEYCACHE_EXIPRETIME_NEVER)
      else
        m_keyCache.SetKey(Result,
                          GetIntegerOption(OPTIONS_CFGID_KEYCACHEEXPIRETIME));
    end;
  end;

end;



//////////////////////////// TPasswordForm ////////////////////////////


// the configuration ID
const
  CONFIG_ID = 'PASSWORDFORM';

// apply levels (bitmask)
const
  APPLY_ALL = 1;
  APPLY_ENC = 2;
  APPLY_DEC = 4;



constructor TPasswordFormStrResListener.Create(theWin : TPasswordForm);
begin
  m_theWin:=theWin;
  m_nMode:=-1;  // (just to have this guy initialised)
end;

procedure TPasswordFormStrResListener.SetMode(nMode : Integer);
begin
  m_nMode:=nMode;
end;

procedure TPasswordFormStrResListener.ChangeStrings(sr : TStrRes);
var
  scc : TShortcutChecker;

// ledgible code? please!
procedure SetCaption(cmpt : TComponent; sCfgID : String; blSC : Boolean = True);
var
  sTemp : String;
begin
  sTemp:=sr.Get(CONFIG_ID, sCfgID);
  if (blSC) then
    sTemp:=scc.AddShortcut(sTemp);
  if (cmpt is TLabel) then
    TLabel(cmpt).Caption:=sTemp
  else
    if (cmpt is TButton) then
      TButton(cmpt).Caption:=sTemp
    else
      if (cmpt is TCheckBox) then
        TCheckBox(cmpt).Caption:=sTemp
      else
        if (cmpt is TMenuItem) then
          TMenuItem(cmpt).Caption:=sTemp;
end;

begin
  scc:=TShortcutChecker.Create;
  with m_theWin do begin

    SetCaption(PWIMenu_PasteAndClear, 'PASTEANDCLEAR', False);

    SetCaption(OKBtn,                      'OKBTN');
    SetCaption(CancelBtn,                  'CANCELBTN');
    SetCaption(HelpBtn,                    'HELPBTN');
    SetCaption(ApplyBtn,                   'APPLYBTN');

    SetCaption(PWUseCachedKeySwitch,       'PWUSECACHEDKEYSWITCH');
    SetCaption(PWShowPasswordSwitch,       'PWSHOWPASSWORDSWITCH');
    SetCaption(PWAutoConfSwitch,           'PWAUTOCONFSWITCH');
    SetCaption(PWCacheKeySwitch,           'PWCACHEKEYSWITCH');
    SetCaption(PWMultiKeySwitch,           'PWMULTIKEYSWITCH');
    SetCaption(PWUseKeyDiskSwitch,         'PWUSEKEYDISKSWITCH');

    case m_nMode of
      PASSWINPUT_MODE_REENCRYPT1 : SetCaption(PWInputInfo, 'PWINPUTINFO_R1');
      PASSWINPUT_MODE_REENCRYPT2 : SetCaption(PWInputInfo, 'PWINPUTINFO_R2');
    else
      SetCaption(PWInputInfo, 'PWINPUTINFO');
    end;

    if (m_nMode = PASSWINPUT_MODE_ENCRYPT) then begin
      SetCaption(EncCompressSwitch,          'ENCCOMPRESSSWITCH');
      SetCaption(EncRenameFilesSwitch,       'ENCRENAMEFILESSWITCH');
      SetCaption(EncStorePathNamesSwitch,    'ENCSTOREPATHNAMESSWITCH');
      SetCaption(EncRelativePathsSwitch,     'ENCRELATIVEPATHSSWITCH');
    end;

    if (m_nMode = PASSWINPUT_MODE_DECRYPT) then begin
      SetCaption(DecRestorePathNamesSwitch,  'DECRESTOREPATHSWITCH');
    end;

    if ((m_nMode = PASSWINPUT_MODE_ENCRYPT) or
        (m_nMode = PASSWINPUT_MODE_DECRYPT)) then begin
      SetCaption(TPUseTargetPathSwitch,      'TPTARGETPATHSWITCH');
      SetCaption(TPBrowseBtn,                'TPBROWSEBTN');
      SetCaption(TPKeepDirectoriesSwitch,    'TPKEEPDIRECTORIESSWITCH');
      SetCaption(TPRemoveSourceFilesSwitch,  'TPREMOVESOURCEFILESSWITCH');
    end;

    if (m_nMode = PASSWINPUT_MODE_REENCRYPT1) then
      SetCaption(ReencAlgoInfo, 'REENC1');
    if (m_nMode = PASSWINPUT_MODE_REENCRYPT2) then
      SetCaption(ReencAlgoInfo, 'REENC2');

  end;
  scc.Destroy;
end;


// configuration checker
type
  TPasswordFormCC = class(TConfigurationChecker)
  private
    m_parent : TPasswordForm;
  public
    constructor Create(parent : TPasswordForm);
    procedure RunCheck(section : TConfigurationSection); override;
  end;

constructor TPasswordFormCC.Create(parent : TPasswordForm);
begin
  m_parent:=parent;
end;

procedure TPasswordFormCC.RunCheck(section : TConfigurationSection);

// use a default height
const
  DEF_HEIGHT = 200;

begin
  with m_parent as TPasswordForm do begin
    // (default width and height are set by the controls placed in the IDE)
    CheckInt(section, 'LEFT', (Screen.Width - Width) shr 1);
    CheckInt(section, 'TOP', (Screen.Height - DEF_HEIGHT) shr 1);
  end;
end;



procedure TPasswordForm.FormCreate(Sender: TObject);
var
  cc : TPasswordFormCC;
begin
  // set a well adjusted width
  ClientWidth:=PWHolder.Width + (PWHolder.Left shl 1);

  // work with a valid path length
  TPPathInputBox.MaxLength:=MAX_PATH;

  // get the configuration section
  cc:=TPasswordFormCC.Create(Self);
  m_config:=_globals.GetCfg.GetSection(CONFIG_ID, cc);
  cc.Destroy;

  // set up the window's position
  with m_config do begin
    // main form borders
    Left:=GetIntegerOption('LEFT');
    Top:=GetIntegerOption('TOP');
  end;

  // init. listener stuff
  m_passwStrListener:=TPasswordFormStrResListener.Create(Self);
  // (FIXME: due to the fact that we change the strings by ourselves every time
  //  the form is shown the registration here is currently superfluous)
  // _globals.GetSr.AddListener(m_passwStrListener);
  // m_passwStrListener.ChangeStrings(_globals.GetSr);

  // empty caption
  Caption:='';

  // validate all members
  m_nExecNum:=0;
  m_nMode:=PASSWINPUT_MODE_KEYINPUT;
  m_nApplyMask:=0;
  m_blKeyInCache:=False;
  m_blShowInProgress:=False;

  // no password available now
  m_password:=Nil;
end;


procedure TPasswordForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  m_config.FixIntegerOption('LEFT', Left);
  m_config.FixIntegerOption('TOP', Top);
  PWInputBox.Clear;
end;


function TPasswordForm.GetPassword(blTakeOver : Boolean = False) : TKeyMemory;
begin
  Result:=m_password;
  if (blTakeOver) then
    // password? which password?
    m_password:=Nil;
end;


procedure TPasswordForm.ClearPassword;
begin
  if (m_password <> Nil) then begin
    m_password.Destroy;
    m_password:=Nil;
  end;
end;


procedure TPasswordForm.SetExecNum(nExecNum : Integer);
begin
  m_nExecNum:=nExecNum;
end;

procedure TPasswordForm.FormShow(Sender: TObject);
var
  nI            : Integer;
  nTop          : Integer;
  nGap          : Integer;
  nModeMask     : Integer;
  blShowTP      : Boolean;
  ciphers       : TStringList;

// to set up the algorithm list for reencryption
// -> True: set up for the first algo / False: for the second one
procedure SetupReencAlgoList(blFirst : Boolean);
var
  nI     : Integer;
  sAlgo  : String;
  sAlgo2 : String;
begin
  m_cipMng.GetReencryptionCiphers(sAlgo, sAlgo2);
  if (not blFirst) then
    sAlgo:=sAlgo2;
  for nI:=0 to (ReencAlgoList.Items.Count - 1) do begin
    if (CompareText(sAlgo, ReencAlgoList.Items.Strings[nI]) = 0) then begin
      ReencAlgoList.ItemIndex:=nI;
      Exit;
    end;
  end;
  // (we should never get right here)
end;



// just to make switch init. code more ledgible
procedure InitSwitch(cbox : TCheckBox; sCfgID : String);
begin
  cbox.Checked:=m_opts.GetCfg.GetBooleanOption(sCfgID);
end;

begin

  // show init. started
  m_blShowInProgress:=True;

  // layout the controls depending on the current mode
  nGap:=PWHolder.Left;
  nTop:=PWHolder.Top + PWHolder.Height;

  // (re)init. the algorithm list
  ReencAlgoList.Items.Clear;
  ciphers:=m_cipMng.GetCiphers;
  for nI:=0 to (ciphers.Count - 1) do
    ReencAlgoList.Items.Add(ciphers.Strings[nI]);

  // (don't show any other controls than the password related ones if the
  //  key part #2 or greater is going to be entered)
  if ((m_nMode > MODE_BORDER2) and (m_nExecNum = 0)) then begin

    // show the encryption or decryption holder
    case m_nMode of
      PASSWINPUT_MODE_ENCRYPT : begin
        ReencHolder.Visible:=False;
        DecHolder.Visible:=False;
        EncHolder.Visible:=True;
        EncHolder.Top:=nTop;
        Inc(nTop, EncHolder.Height);
        blShowTP:=True;
      end;
      PASSWINPUT_MODE_DECRYPT : begin
        ReencHolder.Visible:=False;
        EncHolder.Visible:=False;
        DecHolder.Visible:=True;
        DecHolder.Top:=nTop;
        Inc(nTop, DecHolder.Height);
        blShowTP:=True;
      end;
    else
      // PASSWINPUT_MODE_REENCRYPT1, PASSWINPUT_MODE_REENCRYPT2
      EncHolder.Visible:=False;
      DecHolder.Visible:=False;
      ReencHolder.Visible:=True;
      ReencHolder.Top:=nTop;
      Inc(nTop, ReencHolder.Height);
      blShowTP:=False;
    end;

    // show the target path holder, i. n.
    if (blShowTP) then begin
      TPHolder.Top:=nTop;
      Inc(nTop, TPHolder.Height);
      TPHolder.Visible:=True;
    end
    else begin
      TPHolder.Visible:=False;
    end;  

  end
  else begin
    ReencHolder.Visible:=False;
    TPHolder.Visible:=False;
    EncHolder.Visible:=False;
    DecHolder.Visible:=False;
  end;

  // always show the buttons, of course
  ButtonHolder.Top:=nTop + nGap;
  Inc(nTop, ButtonHolder.Height + nGap);

  // finally set the dialog height
  ClientHeight:=nTop;

  // load the current settings, must do this every time because the user
  // may have changed them and clicked on [Cancel] the last time (reuse some
  // click event handler for enabling or disabling controls)...

  // set the password controls
  InitSwitch(PWUseCachedKeySwitch, OPTIONS_CFGID_USECACHEDKEY);
  InitSwitch(PWAutoConfSwitch,     OPTIONS_CFGID_AUTOCONFIRMATION);
  InitSwitch(PWCacheKeySwitch,     OPTIONS_CFGID_CACHEKEY);
  InitSwitch(PWMultiKeySwitch,     OPTIONS_CFGID_MULTIKEY);
  InitSwitch(PWUseKeyDiskSwitch,   OPTIONS_CFGID_USEKEYDISK);
  InitSwitch(PWShowPasswordSwitch, OPTIONS_CFGID_SHOWPASSWORD);
  StateUseKeyDiskControls(PWUseKeyDiskSwitch.Checked);
  StateShowPasswordControls(PWShowPasswordSwitch.Checked);

  // set the target path area
  if (m_nMode > MODE_BORDER) then begin
    InitSwitch(TPUseTargetPathSwitch,     OPTIONS_CFGID_USETARGETPATH);
    InitSwitch(TPKeepDirectoriesSwitch,   OPTIONS_CFGID_KEEPDIRECTORIES);
    InitSwitch(TPRemoveSourceFilesSwitch, OPTIONS_CFGID_REMOVESOURCEFILES);
    with m_opts.GetCfg do begin
      TPPathInputBox.Text:=GetStringOption(OPTIONS_CFGID_LASTTARGETPATH);
    end;
    StatePathInputControls(TPUseTargetPathSwitch.Checked);
  end;

  // set the encryption area
  if (m_nMode = PASSWINPUT_MODE_ENCRYPT) then begin
    InitSwitch(EncCompressSwitch, OPTIONS_CFGID_USECOMPRESSION);
    InitSwitch(EncRelativePathsSwitch, OPTIONS_CFGID_RELATIVEPATHS);
    InitSwitch(EncStorePathNamesSwitch, OPTIONS_CFGID_STOREPATH);
    StateStorePathControls(EncStorePathNamesSwitch.Checked);
    InitSwitch(EncRenameFilesSwitch, OPTIONS_CFGID_RENAME);
  end;

  // set the decryption area
  if (m_nMode = PASSWINPUT_MODE_DECRYPT) then begin
    InitSwitch(DecRestorePathNamesSwitch, OPTIONS_CFGID_RESTOREPATH);
  end;

  // setup the reencryption stuff
  case m_nMode of
    PASSWINPUT_MODE_REENCRYPT1 : SetupReencAlgoList(True);
    PASSWINPUT_MODE_REENCRYPT2 : SetupReencAlgoList(False);
  end;

  // reload the strings
  with m_passwStrListener do begin
    SetMode(m_nMode);
    ChangeStrings(_globals.GetSr);
  end;

  // if no key is in the cache we cannot demand one
  // (this part of code must stand here to overwrite all controls ons/offs from
  //  possibly made above!)
  PWUseCachedKeySwitch.Enabled:=(m_blKeyInCache and (m_nExecNum = 0));
  StateUseCachedKeyControls(PWUseCachedKeySwitch.Checked and m_blKeyInCache);

  // reset the auto confirmed flag
  m_blAutoConfirmed:=False;

  // try to set the focus to the password input box, i. n.
  if (PWInputBox.Enabled) then
    PWInputBox.SetFocus;

  // (re)enable or disable the apply button, if necessary
  nModeMask:=APPLY_ALL;
  if (m_nExecNum = 0) then
    // (remember that the special controls are only visible at the first time)
    case m_nMode of
      PASSWINPUT_MODE_ENCRYPT : nModeMask:=nModeMask or APPLY_ENC;
      PASSWINPUT_MODE_DECRYPT : nModeMask:=nModeMask or APPLY_DEC;
    end;
  ApplyBtn.Enabled:=((nModeMask and m_nApplyMask) <> 0);

  // show init. finished
  m_blShowInProgress:=False;
end;

procedure TPasswordForm.SetKeyInCache(blState : Boolean);
begin
  m_blKeyInCache:=blState;
end;

procedure TPasswordForm.SetMode(nMode : Integer);
begin
  m_nMode:=nMode;
end;

procedure TPasswordForm.SetObjects(opts : TOptions;
                                   cipMng : TCipherManager);
begin
  m_opts:=opts;
  m_cipMng:=cipMng;
end;



procedure TPasswordForm.ApplyEnable(nLevel : Integer);
begin
  // ignore apply requests durig the show init.
  if (not m_blShowInProgress) then begin

    // set the mask
    m_nApplyMask:=m_nApplyMask or nLevel;

    // enable the apply button (that's ok because this method is always called
    // in the right mode)
    ApplyBtn.Enabled:=True;
  end;
end;

procedure TPasswordForm.StatePathInputControls(blState : Boolean);
begin
  TPPathInputBox.Enabled:=blState;
  TPPathInputBox.Enabled:=blState;
  TPBrowseBtn.Enabled:=blState;
  TPKeepDirectoriesSwitch.Enabled:=blState;
  TPRemoveSourceFilesSwitch.Enabled:=blState;
end;

procedure TPasswordForm.StateUseCachedKeyControls(blState : Boolean);
var
  blUseKeyDisk : Boolean;
begin
  blUseKeyDisk:=PWUseKeyDiskSwitch.Checked;
  PWInputBox.Enabled:=(not blState) and (not blUseKeyDisk);
  PWShowPasswordSwitch.Enabled:=(not blState) and (not blUseKeyDisk);
  PWAutoConfSwitch.Enabled:=(not blState) and (not blUseKeyDisk);
  PWCacheKeySwitch.Enabled:=(not blState) and (not blUseKeyDisk);
  PWMultiKeySwitch.Enabled:=(not blState) and (not blUseKeyDisk) and
                            (m_nExecNum = 0);
  PWUseKeyDiskSwitch.Enabled:=(not blState);
end;

procedure TPasswordForm.StateStorePathControls(blState : Boolean);
begin
  EncRelativePathsSwitch.Enabled:=blState;
end;


procedure TPasswordForm.StateShowPasswordControls(blState : Boolean);
begin
  // set the password character, i. n.
  if (blState) then
    PWInputBox.PasswordChar:=#0
  else
    PWInputBox.PasswordChar:=
      Chr(m_opts.GetCfg.GetIntegerOption(OPTIONS_CFGID_PASSWORDCHAR));
end;

procedure TPasswordForm.StateUseKeyDiskControls(blState : Boolean);
begin
  PWInputBox.Enabled:=not blState;
  PWShowPasswordSwitch.Enabled:=not blState;
  PWAutoConfSwitch.Enabled:=not blState;
  PWCacheKeySwitch.Enabled:=not blState;
  PWMultiKeySwitch.Enabled:=(not blState) and (m_nExecNum = 0);
end;

// all click handlers...

procedure TPasswordForm.PWShowPasswordSwitchClick(Sender: TObject);
begin
  StateShowPasswordControls(PWShowPasswordSwitch.Checked);
  ApplyEnable(APPLY_ALL);
end;


procedure TPasswordForm.PWUseCachedKeySwitchClick(Sender: TObject);
begin
  // en-/disable all password dependant controls
  StateUseCachedKeyControls(PWUseCachedKeySwitch.Checked);
  ApplyEnable(APPLY_ALL);
end;

procedure TPasswordForm.PWAutoConfSwitchClick(Sender: TObject);
begin
  ApplyEnable(APPLY_ALL);
end;

procedure TPasswordForm.PWCacheKeySwitchClick(Sender: TObject);
begin
  ApplyEnable(APPLY_ALL);
end;

procedure TPasswordForm.PWMultiKeySwitchClick(Sender: TObject);
begin
  ApplyEnable(APPLY_ALL);
end;

procedure TPasswordForm.PWUseKeyDiskSwitchClick(Sender: TObject);
begin
  // enabled/disable key disk dependant controls
  StateUseKeyDiskControls(PWUseKeyDiskSwitch.Checked);
  ApplyEnable(APPLY_ALL);
end;

procedure TPasswordForm.TPUseTargetPathSwitchClick(Sender: TObject);
var
  blChecked : Boolean;
begin
  // enabled/disable target path controls
  blChecked:=TPUseTargetPathSwitch.Checked;
  StatePathInputControls(blChecked);
  ApplyEnable(APPLY_ENC or APPLY_DEC);

  // focus the path input box, if selected
  if (blChecked) then
    TPPathInputBox.SetFocus;
end;

procedure TPasswordForm.TPKeepDirectoriesSwitchClick(Sender: TObject);
begin
  ApplyEnable(APPLY_ENC or APPLY_DEC);
end;

procedure TPasswordForm.TPPathInputBoxChange(Sender: TObject);
begin
  ApplyEnable(APPLY_ENC or APPLY_DEC);
end;

procedure TPasswordForm.TPRemoveSourceFilesSwitchClick(Sender: TObject);
begin
  ApplyEnable(APPLY_ENC or APPLY_DEC);
end;

procedure TPasswordForm.EncCompressSwitchClick(Sender: TObject);
begin
  ApplyEnable(APPLY_ENC);
end;

procedure TPasswordForm.EncStorePathNamesSwitchClick(Sender: TObject);
begin
  StateStorePathControls(EncStorePathNamesSwitch.Checked);
  ApplyEnable(APPLY_ENC);
end;

procedure TPasswordForm.EncRenameFilesSwitchClick(Sender: TObject);
begin
  ApplyEnable(APPLY_ENC);
end;

procedure TPasswordForm.EncRelativePathsSwitchClick(Sender: TObject);
begin
  ApplyEnable(APPLY_ENC);
end;

procedure TPasswordForm.DecRestorePathNamesSwitchClick(Sender: TObject);
begin
  ApplyEnable(APPLY_DEC);
end;

procedure TPasswordForm.ReencAlgoListChange(Sender: TObject);
begin
  ApplyEnable(APPLY_ALL);
end;


procedure TPasswordForm.ApplyBtnClick(Sender: TObject);
var
  sCip1, sCip2 : String;
begin
  with m_opts.GetCfg do begin

    // freeze all necessary options
    FixBooleanOption(OPTIONS_CFGID_USEKEYDISK,
                     PWUseKeyDiskSwitch.Checked);
    FixBooleanOption(OPTIONS_CFGID_CACHEKEY,
                     PWCacheKeySwitch.Checked);
    FixBooleanOption(OPTIONS_CFGID_MULTIKEY,
                     PWMultiKeySwitch.Checked);
    FixBooleanOption(OPTIONS_CFGID_USECACHEDKEY,
                     PWUseCachedKeySwitch.Checked);
    FixBooleanOption(OPTIONS_CFGID_AUTOCONFIRMATION,
                     PWAutoConfSwitch.Checked);
    FixBooleanOption(OPTIONS_CFGID_SHOWPASSWORD,
                     PWShowPasswordSwitch.Checked);

    // (don't store what's not visible...)

    if ((m_nMode > MODE_BORDER) and (m_nExecNum = 0)) then begin
      FixBooleanOption(OPTIONS_CFGID_USETARGETPATH,
                       TPUseTargetPathSwitch.Checked);
      FixBooleanOption(OPTIONS_CFGID_KEEPDIRECTORIES,
                       TPKeepDirectoriesSwitch.Checked);
      FixBooleanOption(OPTIONS_CFGID_REMOVESOURCEFILES,
                       TPRemoveSourceFilesSwitch.Checked);
      FixStringOption(OPTIONS_CFGID_LASTTARGETPATH,
                      TPPathInputBox.Text);
    end;

    if ((m_nMode = PASSWINPUT_MODE_ENCRYPT) and (m_nExecNum = 0)) then begin
      FixBooleanOption(OPTIONS_CFGID_USECOMPRESSION,
                       EncCompressSwitch.Checked);
      FixBooleanOption(OPTIONS_CFGID_STOREPATH,
                       EncStorePathNamesSwitch.Checked);
      FixBooleanOption(OPTIONS_CFGID_RELATIVEPATHS,
                       EncRelativePathsSwitch.Checked);
      FixBooleanOption(OPTIONS_CFGID_RENAME,
                       EncRenameFilesSwitch.Checked);
    end;

    if ((m_nMode = PASSWINPUT_MODE_DECRYPT) and (m_nExecNum = 0)) then begin
      FixBooleanOption(OPTIONS_CFGID_RESTOREPATH,
                       DecRestorePathNamesSwitch.Checked);
    end;

    if (((m_nMode = PASSWINPUT_MODE_REENCRYPT1) or
         (m_nMode = PASSWINPUT_MODE_REENCRYPT2)) and
        (m_nExecNum = 0)) then begin

      m_cipMng.GetReencryptionCiphers(sCip1, sCip2);
      case m_nMode of
        PASSWINPUT_MODE_REENCRYPT1 : sCip1:=ReencAlgoList.Text;
        PASSWINPUT_MODE_REENCRYPT2 : sCip2:=ReencAlgoList.Text;
      end;
      m_cipMng.SetReencryptionCiphers(sCip1, sCip2);
    end;

  end;

  // allow apply detection
  ApplyBtn.Enabled:=False;
  m_nApplyMask:=0;
end;


// button handlers...


procedure TPasswordForm.OKBtnClick(Sender: TObject);
var
  blUsePassword : Boolean;
  blEqualPassw  : Boolean;
  blConfirmMode : Boolean;
  nPasswLen     : Integer;
  sPassword     : String;
  sTargetPath   : String;
  sCip1, sCip2  : String;
begin

  // check if the target path exists, if necessary
  if (((m_nMode = PASSWINPUT_MODE_ENCRYPT) or
       (m_nMode = PASSWINPUT_MODE_DECRYPT)) and
       (TPUseTargetpathSwitch.Checked = True)) then begin
    sTargetPath:=TStrPlus.PurePath(Trim(TPPathInputBox.Text));
    TPPathInputBox.Text:=sTargetPath;
    if (not DirectoryExists(sTargetPath)) then begin
      if (Application.MessageBox(
            PChar(Format(_globals.GetSr.Get(CONFIG_ID, 'NOTARGET'),
                         [sTargetPath])),
            PChar(_globals.GetSr.Get(CONFIG_ID, 'WARNING')),
            MB_ICONEXCLAMATION or MB_YESNO) = IDNO) then begin
        ModalResult:=mrNone;
        Exit;
      end;
      ForceDirectories(sTargetPath);
      if (not DirectoryExists(sTargetPath)) then begin
        Application.MessageBox(
          PChar(Format(_globals.GetSr.Get(CONFIG_ID, 'TARGETERR'),
                       [sTargetPath])),
          PChar(_globals.GetSr.Get(CONFIG_ID, 'ERROR')),
          MB_ICONSTOP);
        ModalResult:=mrNone;
        Exit;
      end;
    end;
  end;

  // password used?
  blUsePassword:=(not ((PWUseCachedKeySwitch.Checked and m_blKeyInCache) or
                       PWUseKeyDiskSwitch.Checked)) or
                 m_blAutoConfirmed;
  blConfirmMode:=(m_nMode = PASSWINPUT_MODE_ENCRYPT) or
                 (m_nMode = PASSWINPUT_MODE_REENCRYPT2);
  if (blUsePassword) then begin

    // password checking isn't necessary if we got an auto confirmation signal
    // (m_password was already set by the detection handler)...

    if (not m_blAutoConfirmed) then begin

      // valid password?
      nPasswLen:=TStrPlus.GetBinStr(PWInputBox.Text, sPassword);
      if (nPasswLen = -1) then begin
        Application.MessageBox(PChar(_globals.GetSr.Get(CONFIG_ID, 'BADBIN')),
                               PChar(_globals.GetSr.Get(CONFIG_ID, 'ERROR')),
                               MB_ICONSTOP);
        ModalResult:=mrNone;
        Exit;
      end;

      // zero password?
      if (nPasswLen = 0) then begin
        Application.MessageBox(PChar(_globals.GetSr.Get(CONFIG_ID,
                                                        'ZEROPASSW')),
                               PChar(_globals.GetSr.Get(CONFIG_ID, 'ERROR')),
                               MB_ICONSTOP);
        ModalResult:=mrNone;
        Exit;
      end;

      // now put that password into secure memory
      ClearPassword;
      m_password:=TKeyMemory.Create(nPasswLen);
      m_password.SetData(@sPassword[1], 0, nPasswLen);
      TStrPlus.ClearString(sPassword);

      // do we need a confirmation?
      if (blConfirmMode and (not m_blAutoConfirmed)) then begin

        // show the confirmation dialog
        PasswConfirmWin.SetPasswordChar(PWInputBox.PasswordChar);
        PasswConfirmWin.SetMaxPasswLen(PWInputBox.MaxLength);
        if (PasswConfirmWin.ShowModal = mrCancel) then begin
          ClearPassword;
          PWInputBox.SetFocus;
          ModalResult:=mrNone;
          Exit;
        end;

        // equal passwords?
        blEqualPassw:=False;
        if (m_password.GetSize = PasswConfirmWin.GetPassword.GetSize) then
          if (CompareMem(m_password.GetPtr,
                         PasswConfirmWin.GetPassword.GetPtr,
                         m_password.GetSize)) then
            blEqualPassw:=True;
        PasswConfirmWin.ClearPassword; // (we don't longer need this)
        if (not blEqualPassw) then begin
          Application.MessageBox(PChar(_globals.GetSr.Get(CONFIG_ID,
                                                          'NOTEQUPASSW')),
                                 PChar(_globals.GetSr.Get(CONFIG_ID, 'ERROR')),
                                 MB_ICONSTOP);
          ClearPassword;
          PWInputBox.SetFocus;
          ModalResult:=mrNone;
          Exit;
        end;
      end;
    end;

    // check for a confirmed password
    m_password.SetConfirmed(m_blAutoConfirmed or blConfirmMode);

    // must we store a hash of this new password?
    if ((not m_blAutoConfirmed) and
        PWAutoConfSwitch.Checked and
        blConfirmMode) then
      _globals.GetKeyChecker.AddKey(m_password);
  end;

  // now store all necessary options
  with m_opts.GetCfg do begin

    SetBooleanOption(OPTIONS_CFGID_KEEPDIRECTORIES,
                     TPKeepDirectoriesSwitch.Checked);
    SetBooleanOption(OPTIONS_CFGID_USEKEYDISK,
                     PWUseKeyDiskSwitch.Checked);
    SetBooleanOption(OPTIONS_CFGID_CACHEKEY,
                     PWCacheKeySwitch.Checked);
    SetBooleanOption(OPTIONS_CFGID_MULTIKEY,
                     PWMultiKeySwitch.Checked);
    SetBooleanOption(OPTIONS_CFGID_USECACHEDKEY,
                     PWUseCachedKeySwitch.Checked);
    SetBooleanOption(OPTIONS_CFGID_AUTOCONFIRMATION,
                     PWAutoConfSwitch.Checked);
    SetBooleanOption(OPTIONS_CFGID_SHOWPASSWORD,
                     PWShowPasswordSwitch.Checked);

    // (don't store what wasn't visible!)

    if ((m_nMode > MODE_BORDER) and (m_nExecNum = 0)) then begin
      SetBooleanOption(OPTIONS_CFGID_USETARGETPATH,
                       TPUseTargetPathSwitch.Checked);


      SetBooleanOption(OPTIONS_CFGID_KEEPDIRECTORIES,
                       TPKeepDirectoriesSwitch.Checked);
      SetBooleanOption(OPTIONS_CFGID_REMOVESOURCEFILES,
                       TPRemoveSourceFilesSwitch.Checked);
      SetStringOption(OPTIONS_CFGID_LASTTARGETPATH,
                      TPPathInputBox.Text);
    end;

    if ((m_nMode = PASSWINPUT_MODE_ENCRYPT) and (m_nExecNum = 0)) then begin
      SetBooleanOption(OPTIONS_CFGID_USECOMPRESSION,
                       EncCompressSwitch.Checked);
      SetBooleanOption(OPTIONS_CFGID_STOREPATH,
                       EncStorePathNamesSwitch.Checked);
      SetBooleanOption(OPTIONS_CFGID_RENAME,
                       EncRenameFilesSwitch.Checked);
      SetBooleanOption(OPTIONS_CFGID_RELATIVEPATHS,
                       EncRelativePathsSwitch.Checked);
    end;

    if ((m_nMode = PASSWINPUT_MODE_DECRYPT) and (m_nExecNum = 0)) then begin
      SetBooleanOption(OPTIONS_CFGID_RESTOREPATH,
                       DecRestorePathNamesSwitch.Checked);
    end;

    if (((m_nMode = PASSWINPUT_MODE_REENCRYPT1) or
         (m_nMode = PASSWINPUT_MODE_REENCRYPT2)) and
        (m_nExecNum = 0)) then begin

      m_cipMng.GetReencryptionCiphers(sCip1, sCip2);
      case m_nMode of
        PASSWINPUT_MODE_REENCRYPT1 : sCip1:=ReencAlgoList.Text;
        PASSWINPUT_MODE_REENCRYPT2 : sCip2:=ReencAlgoList.Text;
      end;
      m_cipMng.SetReencryptionCiphers(sCip1, sCip2, False);
    end;

  end;

end;

procedure TPasswordForm.FormDestroy(Sender: TObject);
begin
  m_passwStrListener.Destroy;
  ClearPassword;
end;

procedure TPasswordForm.PWInputBoxKeyPress(Sender: TObject; var Key: Char);
begin
  // map the enter key to the OK button
  if (Key = #13) then begin
    ModalResult:=mrOk;
    OKBtnClick(Sender);
    Key:=#0;
  end;
end;


procedure TPasswordForm.HelpBtnClick(Sender: TObject);
begin
  Application.HelpCommand(HELP_CONTEXT, 505);
end;

procedure TPasswordForm.PWIMenuPopup(Sender: TObject);
var
  blGotText : Boolean;
begin
  blGotText:=Clipboard.HasFormat(CF_TEXT);
  PWIMenu_PasteAndClear.Enabled:=blGotText;
end;

procedure TPasswordForm.PWIMenu_PasteAndClearClick(Sender: TObject);
begin
  PWInputBox.PasteFromClipboard;
  Clipboard.Clear;
end;

procedure TPasswordForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = #27) then begin
    ModalResult:=mrCancel;
    Close;
    Key:=#0;
    Exit;
  end;
end;

procedure TPasswordForm.TPBrowseBtnClick(Sender: TObject);
begin
  with DestSelectWin do begin
    SetDestPath(Trim(TPPathInputBox.Text));
    Setup(_globals.GetSr.Get(STRRES_ID, 'DESTSEL'));
    if (ShowModal <> mrCancel) then
      TPPathInputBox.Text:=GetDestPath;
  end;
end;

procedure TPasswordForm.PWInputBoxChange(Sender: TObject);
var
  nLen     : Integer;
  sPassw   : String;
  newPassw : TKeyMemory;
begin
  // check if the password can be auto confirmed
  if (PWAutoConfSwitch.Checked) then begin

    // (we must append the current key because at this time the input box
    //  won't updated yet)
    nLen:=TStrPlus.GetBinStr(PWInputBox.Text, sPassw);

    // don't accept incomplete binary sequences
    if (nLen = -1) then
      Exit;
    if (nLen > 0) then begin
      newPassw:=TKeyMemory.Create(nLen);
      newPassw.SetData(@sPassw[1], 0, nLen);
      TStrPlus.ClearString(sPassw);
      if (_globals.GetKeyChecker.CheckKey(newPassw)) then begin
        // got a good one, so kill the old one, set the signal and leave
        ClearPassword;
        m_password:=newPassw;
        ModalResult:=mrOk;
        m_blAutoConfirmed:=True;
        OKBtnClick(Sender);
      end
      else
        newPassw.Destroy;
    end;
  end;
end;

function TPasswordForm.FormHelp(Command: Word; Data: Integer;
  var CallHelp: Boolean): Boolean;
begin
  CallHelp := False;
  Result:=HtmlHelp_ShowContext(Data);
end;

procedure TPasswordForm.PWInputBoxKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if ((Windows.GetKeyState(VK_CAPITAL) and 1) = 1) then
    TipForm.ShowTip(TIP_CAPSLOCKWARNING);
end;

end.
