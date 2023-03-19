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
  our good old options dialog
}

unit SettingsWin;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Configuration, ComCtrls, StdCtrls, CheckLst, ExtCtrls,
  CheckBoxList, StringRes, Mask, Menus;

type
  TOnConfigSwitch = class;
  TSettingsForm = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    ApplyBtn: TButton;
    HelpBtn: TButton;
    SheetHolder: TPageControl;
    ConfigSheet: TTabSheet;
    FileHandlingSheet: TTabSheet;
    SecuritySheet: TTabSheet;
    SecurityCipherList: TComboBox;
    SecurityCipherListInfo: TLabel;
    OpenDialog: TOpenDialog;
    ConfigScroller: TScrollBox;
    ConfigKeeper: TPanel;
    MiscSheet: TTabSheet;
    SecurityCompressListInfo: TLabel;
    SecurityCompressList: TComboBox;
    SecurityWipeInfo: TLabel;
    SecurityWipe: TComboBox;
    FileHandlingRenameSetup: TGroupBox;
    FileHandlingRenameRandomSwitch: TRadioButton;
    FileHandlingRenameMaskSwitch: TRadioButton;
    FileHandlingMaskBox: TEdit;
    FileHandlingExtBox: TEdit;
    FileHandlingMaskInfo: TLabel;
    FileHandlingExtInfo: TLabel;
    FileHandlingCompressionSetup: TGroupBox;
    FileHandlingNoCompressBox: TEdit;
    FileHandlingNoCompressInfo: TLabel;
    FileHandlingNumberInfo: TStaticText;
    MiscTempViewPathBox: TEdit;
    MiscTempViewPathInfo: TLabel;
    MiscTempViewPathBrowseBtn: TButton;
    MiscKeyDiskInfo: TLabel;
    MiscKeyDiskBox: TEdit;
    MiscMakeKeyDiskBtn: TButton;
    MiscMaxErrorsInfo: TLabel;
    MiscMaxErrorsBox: TEdit;
    MiscClearKeyHashesBtn: TButton;
    MiscMaxErrorsUpDown: TUpDown;
    SecurityKeyCacheGroup: TGroupBox;
    SecurityKeyCacheExpireTimeInfo: TLabel;
    SecurityKeyCacheNeverExpireSwitch: TCheckBox;
    SecurityKeyCacheExpireTimeBox: TEdit;
    SecurityKeyCacheExpireTimeUpDown: TUpDown;
    MiscRegGroup: TGroupBox;
    MiscRegUpdateBtn: TButton;
    MiscRegFileAssocSwitch: TCheckBox;
    MiscRegShellExtsSwitch: TCheckBox;
    SecurityMnCipher: TPopupMenu;
    SecurityMnCipherShow: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure ApplyBtnClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SecurityCompressListChange(Sender: TObject);
    procedure SecurityCipherListChange(Sender: TObject);
    procedure SecurityWipeChange(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure FileHandlingNoCompressBoxChange(Sender: TObject);
    procedure FileHandlingMaskBoxChange(Sender: TObject);
    procedure FileHandlingExtBoxChange(Sender: TObject);
    procedure FileHandlingRenameMaskSwitchClick(Sender: TObject);
    procedure FileHandlingRenameRandomSwitchClick(Sender: TObject);
    procedure MiscClearKeyHashesBtnClick(Sender: TObject);
    procedure SecurityKeyCacheNeverExpireSwitchClick(Sender: TObject);
    procedure MiscMakeKeyDiskBtnClick(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure MiscTempViewPathBrowseBtnClick(Sender: TObject);
    procedure SecurityKeyCacheExpireTimeBoxChange(Sender: TObject);
    procedure MiscMaxErrorsBoxChange(Sender: TObject);
    procedure MiscTempViewPathBoxChange(Sender: TObject);
    procedure MiscKeyDiskBoxChange(Sender: TObject);
    procedure MiscRegUpdateBtnClick(Sender: TObject);
    procedure SheetHolderChanging(Sender: TObject;
      var AllowChange: Boolean);
    procedure MiscSheetShow(Sender: TObject);
    procedure SecuritySheetShow(Sender: TObject);
    procedure ConfigSheetShow(Sender: TObject);
    procedure FileHandlingSheetShow(Sender: TObject);
    procedure MiscRegFileAssocSwitchClick(Sender: TObject);
    procedure MiscRegShellExtsSwitchClick(Sender: TObject);
    function FormHelp(Command: Word; Data: Integer;
      var CallHelp: Boolean): Boolean;
    procedure SecurityMnCipherShowClick(Sender: TObject);

  protected
    m_cfgList              : TCheckBoxList;
    m_cfgListCB            : TCheckBoxListCallBack;
    m_config               : TConfigurationSection;
    m_sr                   : TStrRes;
    m_blApplyBtnWasEnabled : Boolean;
    m_blReloading          : Boolean;
    m_blApplyPossible      : Boolean;

    // indicates that the apply button is now valid
    procedure EnableApply;

    // reloads all options
    procedure ReloadOptions;

    // stores all options
    // -> True: fix them / False: just set session-only
    procedure StoreSettings(blFix : Boolean);

    // creates the configuration list (entries with _no_ captions)
    procedure MakeConfigList;

    // checks dependencies in the cfg. list
    // -> index of the changed item (-1 equals "check all")
    procedure CheckConfigListDependencies(nChangedIdx : Integer = -1);

    // load the ciphernames into the list
    procedure LoadCipherNames;

    // check dependencies of the rename method box
    procedure CheckRenameMethodDependencies;

    // check dependencies of the key cache expire controls
    procedure CheckKeyCacheExpireDependencies;

    // to check an edit control which is associated with an updown control
    // -> edit control
    // -> ed. ctrl's patrent sheet
    // -> updown control
    // -> caption of the control (to be shown in error messages)
    // <- True: value entered is ok / False: not ok
    function ValidateEditControl(edCtrl : TEdit;
                                 sheet : TTabSheet;
                                 upDown : TUpDown;
                                 const sCaption : String) : Boolean;

    // validates all input which might contain errors
    // <- True: everything's ok / False: something's not right
    function Validate : Boolean;
  end;

  TOnConfigSwitch = class(TCheckBoxListCallBack)
  public
    // callback method (ECallbackInterrupt won't be thrown, of course)
    procedure CallBack; override;
  end;



var
  SettingsForm: TSettingsForm;

implementation
uses
  MakeKeyDiskWin,
  General,
  GlobalsGUI,
  Options,
  FileBrowser,
  CipherServer,
  CipherManager,
  StringPlus,
  StringPlusI,
  HtmlHelpAPI,
  RandomManager,
  BFManager,
  BFAFile,
  ShortcutChecker,
  FileCtrl,
  bfacslib,
  RegistryKeys,
  BrowseForFolder;

{$R *.DFM}



//////////////////////////// TOnConfigSwitch ////////////////////////////


procedure TOnConfigSwitch.CallBack;
begin
  with GetCallBackObj as TSettingsForm do begin

    CheckConfigListDependencies;
    EnableApply;
  end;
end;


//////////////////////////// TSettingsForm ////////////////////////////


// configuration and string resource ID
const
  CONFIG_ID = 'SETTINGSFORM';

// config. IDs
const
  CFGID_LEFT    = 'LEFT';
  CFGID_TOP     = 'TOP';
  CFGID_INITDIR = 'INITDIR';


// some updown borders
const
  MAXERRORS_MIN  = 0;
  MAXERRORS_MAX  = 32000;
  EXPIRETIME_MIN = 5;
  EXPIRETIME_MAX = 32000;



// internal lookup table for an easier configuration access and caption reload,
// an switch item is represented by three values: the module configuration ID
// and the option ID (both stored and linked via a TStringKeeper to each item)
// plus an string resource ID to load the right caption
const
  CONFIG_LOOKUP_COUNT = 32;
  CONFIG_IDX_MODULE = 0;  // (just to access the array)
  CONFIG_IDX_OPTION = 1;
  CONFIG_IDX_STRRES = 2;
  CONFIG_LOOKUP : array[0..CONFIG_LOOKUP_COUNT - 1, 0..2] of String = (

    (OPTS_CFG_ID, OPTIONS_CFGID_SHOWHINTS,          'CFG_SHOWHINTS'),
    (OPTS_CFG_ID, OPTIONS_CFGID_KEEPDATETIME,       'CFG_KEEPDATETIME'),
    (OPTS_CFG_ID, OPTIONS_CFGID_KEEPATTRIBUTES,     'CFG_KEEPATTRIBUTES'),
    (OPTS_CFG_ID, OPTIONS_CFGID_SKIPENCRYPTED,      'CFG_SKIPENCRYPTED'),
    (OPTS_CFG_ID, OPTIONS_CFGID_WRITEPROTECT,       'CFG_WRITEPROTECT'),
    (OPTS_CFG_ID, OPTIONS_CFGID_TRYRENAME83,        'CFG_TRYRENAME83'),
    (OPTS_CFG_ID, OPTIONS_CFGID_ADDEXTENSION,       'CFG_ADDEXTENSION'),
    (OPTS_CFG_ID, OPTIONS_CFGID_IGNORECRC32,        'CFG_IGNORECRC32'),
    (OPTS_CFG_ID, OPTIONS_CFGID_ACCEPTTRUNCATED,    'CFG_ACCEPTRUNCATED'),
    (OPTS_CFG_ID, OPTIONS_CFGID_REMOVEEMPTYDIRS,    'CFG_REMOVEEMPTYDIRS'),
    (OPTS_CFG_ID, OPTIONS_CFGID_OVERWRITEEXISTING,  'CFG_OVERWRITEEXISTING'),
    (OPTS_CFG_ID, OPTIONS_CFGID_NOCACHE,            'CFG_NOCACHE'),
    (OPTS_CFG_ID, OPTIONS_CFGID_CONFIRMOPERATIONS,  'CFG_CONFIRMOPERATIONS'),
    (OPTS_CFG_ID, OPTIONS_CFGID_SHOWJOBREPORT,      'CFG_SHOWJOBREPORT'),
    (OPTS_CFG_ID, OPTIONS_CFGID_OVERWRITEVIEWED,    'CFG_OVERWRITEVIEWED'),
    (OPTS_CFG_ID, OPTIONS_CFGID_CLOSEAFTERWORK,     'CFG_CLOSEAFTERWORK'),
    (OPTS_CFG_ID, OPTIONS_CFGID_DOUBLECLICKWORK,    'CFG_DOUBLECLICKWORK'),
    (OPTS_CFG_ID, OPTIONS_CFGID_SAVESENSSETS,       'CFG_SAVESENSSETS'),
    (OPTS_CFG_ID, OPTIONS_CFGID_COLORPROGRESS,      'CFG_COLORPROGRESS'),
    (OPTS_CFG_ID, OPTIONS_CFGID_TRAYICON,           'CFG_TRAYICON'),
    (OPTS_CFG_ID, OPTIONS_CFGID_FORCECOMPRESS,      'CFG_FORCECOMPRESS'),
    (OPTS_CFG_ID, OPTIONS_CFGID_SHOWCFGFILEERRORS,  'CFG_SHOWCFGFILEERRORS'),

    (FB_CFG_ID, FB_CFGID_HOTTRACKING,      'CFG_HOTTRACKING'),
    (FB_CFG_ID, FB_CFGID_HTMLSTYLE,        'CFG_HTMLSTYLE'),
    (FB_CFG_ID, FB_CFGID_GRIDLINES,        'CFG_GRIDLINES'),
    (FB_CFG_ID, FB_CFGID_AUTOARRANGE,      'CFG_AUTOARRANGE'),
    (FB_CFG_ID, FB_CFGID_FLATVIEW,         'CFG_FLATVIEW'),
    (FB_CFG_ID, FB_CFGID_SCANBFAFILESONLY, 'CFG_SCANBFAFILESONLY'),
    (FB_CFG_ID, FB_CFGID_REPLACESCANICONS, 'CFG_REPLACESCANICONS'),
    (FB_CFG_ID, FB_CFGID_AUTOREFRESH,      'CFG_AUTOREFRESH'),
    (FB_CFG_ID, FB_CFGID_PLACEDRIVESFIRST, 'CFG_PLACEDRIVESFIRST'),
    (FB_CFG_ID, FB_CFGID_HIDEDRIVES,       'CFG_HIDEDRIVES')
  );


// resource listener
type
  TSettingsFormStrResListener = class(TStrResListener)
  private
    m_theWin        : TSettingsForm;
    m_blNoRegUpdate : Boolean;
  public
    constructor Create(theWin : TSettingsForm);
    procedure ChangeStrings(sr : TStrRes); override;
    procedure TriggerNoRegUpdate;
  end;

constructor TSettingsFormStrResListener.Create(theWin : TSettingsForm);
begin
  m_theWin:=theWin;
  m_blNoRegUpdate:=False;
end;

procedure TSettingsFormStrResListener.TriggerNoRegUpdate;
begin
  m_blNoRegUpdate:=True;
end;

procedure TSettingsFormStrResListener.ChangeStrings(sr : TStrRes);
const
  CONTROL_GAP = 2;

var
  nI, nJ      : Integer;
  nWidth      : Integer;
  nMax        : Integer;
  nLeft       : Integer;
  blFileAssoc : Boolean;
  blShellExts : Boolean;
  sModID      : String;
  sOptID      : String;
  scc         : TShortcutChecker;

// ledgible code? please! (FIXME: Windows.SetWindowText(cmpt.handle, ...)?)
procedure SetCaption(cmpt : TComponent;
                     const sCfgID : String;
                     blAddShortCut : Boolean = True);
var
  sCaption : String;
begin
  sCaption:=sr.Get(CONFIG_ID, sCfgID);
  if (blAddShortCut) then
    sCaption:=scc.AddShortCut(sCaption);

       if (cmpt is TLabel      ) then TLabel      (cmpt).Caption:=sCaption
  else if (cmpt is TButton     ) then TButton     (cmpt).Caption:=sCaption
  else if (cmpt is TCheckBox   ) then TCheckBox   (cmpt).Caption:=sCaption
  else if (cmpt is TGroupBox   ) then TGroupBox   (cmpt).Caption:=sCaption
  else if (cmpt is TTabSheet   ) then TTabSheet   (cmpt).Caption:=sCaption
  else if (cmpt is TStaticText ) then TStaticText (cmpt).Caption:=sCaption
  else if (cmpt is TRadioButton) then TRadioButton(cmpt).Caption:=sCaption
  else if (cmpt is TMenuItem   ) then TMenuItem   (cmpt).Caption:=sCaption;
end;


begin
  with m_theWin do begin
    // label the window
    Caption:=sr.Get(CONFIG_ID, 'CAPTION');

    // prepare the shortcut checker
    scc:=TShortcutChecker.Create;
    SetCaption(OKBtn,     'OKBTN');
    SetCaption(CancelBtn, 'CANCELBTN');
    SetCaption(ApplyBtn,  'APPLYBTN');
    SetCaption(HelpBtn,   'HELPBTN');

    // add the "main" button captions to a shortcut checker
    with scc do begin
      Scan(OkBtn.Caption);    // (quicker than a second resource request)
      Scan(CancelBtn.Caption);
      Scan(ApplyBtn.Caption);
      Scan(HelpBtn.Caption);
    end;

    // setup the configuration sheet
    scc.Reset(False);
    SetCaption(ConfigSheet, 'CONFIGSHEET', False);
    // (exchange the string in the configuration list, due to sorting this
    //  requires a somewhat slow lookup for every item)
    for nI:=0 to (m_cfgList.GetCount - 1) do begin

      // get module and option IDs
      with m_cfgList.GetObject(nI) as TStringKeeper do begin
        sModID:=Get;
        sOptID:=GetSubCnt;
      end;

      // make a table lookup
      for nJ:=0 to (CONFIG_LOOKUP_COUNT - 1) do
        // (module _and_ option ID must be equal!)
        if ((CONFIG_LOOKUP[nJ, CONFIG_IDX_MODULE] = sModID) and
            (CONFIG_LOOKUP[nJ, CONFIG_IDX_OPTION] = sOptID)) then begin
          m_cfgList.Get(nI).Caption:=_globals.GetSr
                                             .Get(CONFIG_ID,
                                                  CONFIG_LOOKUP[nJ,
                                                  CONFIG_IDX_STRRES]);
          Break;
        end;
    end;
    m_cfgList.Sort;


    // setup the security sheet
    scc.Reset(False);
    SetCaption(SecuritySheet, 'SECURITYSHEET', False);
    SetCaption(SecurityCipherListInfo,  'SECURITYCIPHERLISTINFO');
    SetCaption(SecurityCompressListInfo, 'SECURITYCOMPRESSINFO');
    SetCaption(SecurityWipeInfo, 'SECURITYWIPEINFO');
    SetCaption(SecurityKeyCacheGroup, 'SECURITY_KC_GROUP', False);
    SetCaption(SecurityKeyCacheExpireTimeInfo, 'SECURITY_KC_EXPIRETIME');
    SetCaption(SecurityKeyCacheNeverExpireSwitch, 'SECURITY_KC_NEVEREXPIRES');
    SetCaption(SecurityMnCipherShow, 'SECURITY_MNCIPHER_SHOW', False);

    with SecurityCompressList.Items, sr do begin
      Clear;
      Add(Get(CONFIG_ID, 'COMPRESSNAME_LZSS'));
      Add(Get(CONFIG_ID, 'COMPRESSNAME_DEFLATE'));
      Add(Get(CONFIG_ID, 'COMPRESSNAME_BZIP2'));
    end;
    case _globals.GetOpts.GetCfg.GetIntegerOption(OPTIONS_CFGID_COMPRESS) of
      BFAFILE_COMPRESS_LZSS: SecurityCompressList.ItemIndex:=0;
      BFAFILE_COMPRESS_DEFL: SecurityCompressList.ItemIndex:=1;
      BFAFILE_COMPRESS_BZP2: SecurityCompressList.ItemIndex:=2;
    end;

    with SecurityWipe.Items, sr do begin
      Clear;
      Add(Get(CONFIG_ID, 'WIPE_DELETEONLY'));
      Add(Get(CONFIG_ID, 'WIPE_SIMPLE'));
      Add(Get(CONFIG_ID, 'WIPE_DOD'));
      Add(Get(CONFIG_ID, 'WIPE_SFS'));
    end;
    case _globals.GetOpts.GetCfg.GetIntegerOption(OPTIONS_CFGID_WIPING) of
      BFM_WIPE_DELETEONLY : SecurityWipe.ItemIndex:=0;
      BFM_WIPE_SIMPLE     : SecurityWipe.ItemIndex:=1;
      BFM_WIPE_DOD        : SecurityWipe.ItemIndex:=2;
      BFM_WIPE_SFS        : SecurityWipe.ItemIndex:=3;
    end;

    nMax:=SecurityCipherListInfo.Width;
    nWidth:=SecurityCompressListInfo.Width;
    if (nMax < nWidth) then
      nMax:=nWidth;
    nWidth:=SecurityWipeInfo.Width;
    if (nMax < nWidth) then
      nMax:=nWidth;
    SecurityCipherList.Width:=nMax;
    SecurityCompressList.Width:=nMax;
    SecurityWipe.Width:=nMax;
    nLeft:=(SecuritySheet.ClientWidth - nMax) shr 1;
    SecurityCipherListInfo.Left:=nLeft;
    SecurityCompressListInfo.Left:=nLeft;
    SecurityWipeInfo.Left:=nLeft;
    SecurityCipherList.Left:=nLeft;
    SecurityCompressList.Left:=nLeft;
    SecurityWipe.Left:=nLeft;


    // setup the file handling sheet
    scc.Reset(False);
    SetCaption(FileHandlingSheet,              'FILEHANDLINGSHEET', False);
    SetCaption(FileHandlingRenameSetup,        'FH_RENAMESETUP', False);
    SetCaption(FileHandlingRenameRandomSwitch, 'FH_RENAMERANDOMSWITCH');
    SetCaption(FileHandlingRenameMaskSwitch,   'FH_RENAMEMASKSWITCH');
    SetCaption(FileHandlingMaskInfo,           'FH_MASKINFO');
    SetCaption(FileHandlingExtInfo,            'FH_EXTINFO');
    SetCaption(FileHandlingCompressionSetup,   'FH_COMPRESSIONSETUP', False);
    SetCaption(FileHandlingNoCompressInfo,     'FH_NOCOMPRESSINFO');
    SetCaption(FileHandlingNumberInfo,         'FH_NUMINFO', False);
    FileHandlingNumberInfo.Left:=
      FileHandlingExtBox.Left - FileHandlingNumberInfo.Width - CONTROL_GAP;
    FileHandlingMaskBox.Width:=
      FileHandlingNumberInfo.Left - FileHandlingMaskBox.Left - CONTROL_GAP;


    // at last the misc sheet
    scc.Reset(False);
    SetCaption(MiscSheet,                     'MISCSHEET', False);
    SetCaption(MiscTempViewPathInfo,          'MISC_TVP_INFO');
    SetCaption(MiscTempViewPathBrowseBtn,     'MISC_TVP_BROWSE');
    SetCaption(MiscKeyDiskInfo,               'MISC_KD_INFO');
    SetCaption(MiscMakeKeyDiskBtn,            'MISC_KD_MAKE');
    SetCaption(MiscMaxErrorsInfo,             'MISC_MAXERRORS');
    SetCaption(MiscClearKeyHashesBtn,         'MISC_CLEARKEYHASHES');
    SetCaption(MiscRegGroup,                  'MISC_REGGROUP', False);
    SetCaption(MiscRegFileAssocSwitch,        'MISC_REGFILEASSOCSWITCH');
    SetCaption(MiscRegShellExtsSwitch,        'MISC_REGSHELLEXTSSWITCH');
    SetCaption(MiscRegUpdateBtn,              'MISC_REGUPDATEBTN');


    // reload the strings for the shell extensions
    // (simple solution: we just update the registry stuff completely)
    if (not m_blNoRegUpdate) then begin
      TRegistryKeys.Check(blFileAssoc, blShellExts);
      TRegistryKeys.Update(blFileAssoc, blShellExts, sr, True);
    end;

    // reset the trigger
    m_blNoRegUpdate:=False;

    // no need for a shortcut checker anymore
    scc.Destroy;
  end;
end;


// configuration checker
type
  TSettingsFormCC = class(TConfigurationChecker)
  private
    m_parent : TSettingsForm;
  public
    constructor Create(parent : TSettingsForm);
    procedure RunCheck(section : TConfigurationSection); override;
  end;


constructor TSettingsFormCC.Create(parent : TSettingsForm);
begin
  m_parent:=parent;
end;


procedure TSettingsFormCC.RunCheck(section : TConfigurationSection);
begin
  with m_parent as TSettingsForm do begin
    CheckInt(section, CFGID_LEFT, (Screen.Width - Width) shr 1);
    CheckInt(section, CFGID_TOP, (Screen.Height - Height) shr 1);
    CheckString(section, CFGID_INITDIR, ExtractFilePath(Application.ExeName));
  end;
end;



(******************** helper routines ********************)

procedure TSettingsForm.EnableApply;
begin
  // don't accept applies during reloading
  if ((not m_blReloading) and m_blApplyPossible) then
    ApplyBtn.Enabled:=True;
end;


procedure TSettingsForm.ReloadOptions;
var
  nI    : Integer;
  nTemp : Integer;

procedure LoadSwitch(cb : TCheckBox; const sCfgID : String);
begin
  cb.Checked:=_globals.GetOpts.GetCfg.GetBooleanOption(sCfgID);
end;

begin
  // start reloading
  m_blReloading:=True;

  // set cipher, compress and wipe method selections
  SecurityCipherList.ItemIndex:=
    SecurityCipherList.Items.IndexOf(
      _globals.GetCipMng.GetCurrentCipher);

  case _globals.GetOpts.GetCfg.GetIntegerOption(OPTIONS_CFGID_COMPRESS) of
    BFAFILE_COMPRESS_LZSS: SecurityCompressList.ItemIndex:=0;
    BFAFILE_COMPRESS_DEFL: SecurityCompressList.ItemIndex:=1;
    BFAFILE_COMPRESS_BZP2: SecurityCompressList.ItemIndex:=2;
  end;

  case _globals.GetOpts.GetCfg.GetIntegerOption(OPTIONS_CFGID_WIPING) of
    BFM_WIPE_DELETEONLY : SecurityWipe.ItemIndex:=0;
    BFM_WIPE_SIMPLE     : SecurityWipe.ItemIndex:=1;
    BFM_WIPE_DOD        : SecurityWipe.ItemIndex:=2;
    BFM_WIPE_SFS        : SecurityWipe.ItemIndex:=3;
  end;

  // other security stuff
  with _globals.GetCfg.GetSection(OPTS_CFG_ID) do begin
    nTemp:=GetIntegerOption(OPTIONS_CFGID_KEYCACHEEXPIRETIME);
    SecurityKeyCacheExpireTimeUpDown.Position:=nTemp;
    SecurityKeyCacheExpireTimeBox.Text:=IntToStr(nTemp);
    LoadSwitch(SecurityKeyCacheNeverExpireSwitch,
               OPTIONS_CFGID_KEYCACHENEVEREXPIRE);
    CheckKeyCacheExpireDependencies;
  end;

  // load the configuration switches (FIXME: we getting the states directly out
  // of the config., which isn't a very proper (e.g. _opts might use a
  // a different cfg.) but a very fast and clear solution)
  for nI:=0 to (CONFIG_LOOKUP_COUNT - 1) do
    with m_cfgList.GetObject(nI) as TStringKeeper do
      m_cfgList.Get(nI).Checked:=
        _globals.GetCfg.GetSection(Get).GetBooleanOption(GetSubCnt);
  // (need to update all dependencies, of course)
  CheckConfigListDependencies;


  // load the file handling options
  with _globals.GetCfg.GetSection(OPTS_CFG_ID) do begin
    if (GetBooleanOption(OPTIONS_CFGID_RANDOMRENAME)) then
      FileHandlingRenameRandomSwitch.Checked:=True
    else
      FileHandlingRenameMaskSwitch.Checked:=True;
    FileHandlingMaskBox.Text:=GetStringOption(OPTIONS_CFGID_MASKNAME);
    FileHandlingExtBox.Text:=GetStringOption(OPTIONS_CFGID_MASKEXT);

    CheckRenameMethodDependencies;

    FileHandlingNoCompressBox.Text:=
      GetStringOption(OPTIONS_CFGID_NOCOMPRESSTYPES);
  end;

  // load the misc stuff
  with _globals.GetCfg.GetSection(OPTS_CFG_ID) do begin
    MiscClearKeyHashesBtn.Enabled:=(not _globals.GetKeyChecker.IsEmpty);
    MiscTempViewPathBox.Text:=GetStringOption(OPTIONS_CFGID_TEMPVIEWPATH);
    MiscKeyDiskBox.Text:=GetStringOption(OPTIONS_CFGID_KEYDISKPATH);
    nTemp:=GetIntegerOption(OPTIONS_CFGID_MAXERRORS);
    MiscMaxErrorsUpDown.Position:=nTemp;
    MiscMaxErrorsBox.Text:=IntToStr(nTemp);
  end;

  // reloading finished
  m_blReloading:=False;
end;


procedure TSettingsForm.StoreSettings(blFix : Boolean);
var
  nI : Integer;

procedure StoreSwitch(cb : TCheckBox; const sCfgID : String);
begin
  with _globals.GetOpts.GetCfg do
    if (blFix) then
      FixBooleanOption(sCfgID, cb.Checked)
    else
      SetBooleanOption(sCfgID, cb.Checked);
end;

begin
  // store the cipher
  _globals.GetCipMng.SetCurrentCipher(
    SecurityCipherList.Items[SecurityCipherList.ItemIndex], blFix);

  // store the compression method
  with _globals.GetOpts.GetCfg do begin
     case SecurityCompressList.ItemIndex of
       1: nI:=BFAFILE_COMPRESS_DEFL;
       2: nI:=BFAFILE_COMPRESS_BZP2;
    else // = 0
       nI:=BFAFILE_COMPRESS_LZSS;
    end;
    if (blFix) then
      FixIntegerOption(OPTIONS_CFGID_COMPRESS, nI)
    else
      SetIntegerOption(OPTIONS_CFGID_COMPRESS, nI);
  end;

  // store the wiping method
  case SecurityWipe.ItemIndex of
    0 : nI:= BFM_WIPE_DELETEONLY;
    1 : nI:= BFM_WIPE_SIMPLE;
    2 : nI:= BFM_WIPE_DOD;
    3 : nI:= BFM_WIPE_SFS;
  else
    // (this should never happen)
    nI:= BFM_WIPE_SIMPLE;
  end;
  with _globals.GetOpts.GetCfg do begin
    if (blFix) then
      FixIntegerOption(OPTIONS_CFGID_WIPING, nI)
    else
      SetIntegerOption(OPTIONS_CFGID_WIPING, nI);
  end;

  // store the configuration switches (FIXME: same as in ReloadOptions())
  for nI:=0 to (CONFIG_LOOKUP_COUNT - 1) do
    with m_cfgList.GetObject(nI) as TStringKeeper do
      if (blFix) then
        _globals.GetCfg
                .GetSection(Get).FixBooleanOption(GetSubCnt,
                                                  m_cfgList.Get(nI).Checked)
      else
        _globals.GetCfg
                .GetSection(Get).SetBooleanOption(GetSubCnt,
                                                  m_cfgList.Get(nI).Checked);

  // store the file handling options
  with _globals.GetCfg.getSection(OPTS_CFG_ID) do begin
    if (blFix) then begin
      FixBooleanOption(OPTIONS_CFGID_RANDOMRENAME,
                       FileHandlingRenameRandomSwitch.Checked);
      FixStringOption(OPTIONS_CFGID_MASKNAME, Trim(FileHandlingMaskBox.Text));
      FixStringOption(OPTIONS_CFGID_MASKEXT, Trim(FileHandlingExtBox.Text));
      FixStringOption(OPTIONS_CFGID_NOCOMPRESSTYPES,
                      Trim(FileHandlingNoCompressBox.Text));
    end
    else begin
      SetBooleanOption(OPTIONS_CFGID_RANDOMRENAME,
                       FileHandlingRenameRandomSwitch.Checked);
      SetStringOption(OPTIONS_CFGID_MASKNAME, Trim(FileHandlingMaskBox.Text));
      SetStringOption(OPTIONS_CFGID_MASKEXT, Trim(FileHandlingExtBox.Text));
      SetStringOption(OPTIONS_CFGID_NOCOMPRESSTYPES,
                      Trim(FileHandlingNoCompressBox.Text));
    end;
  end;

  // store all the misc stuff
//      debd('x', MiscMaxErrorsUpDown.Position);
  with _globals.GetCfg.getSection(OPTS_CFG_ID) do begin
    StoreSwitch(SecurityKeyCacheNeverExpireSwitch,
                OPTIONS_CFGID_KEYCACHENEVEREXPIRE);
    if (blFix) then begin
      FixStringOption(OPTIONS_CFGID_TEMPVIEWPATH,
                      Trim(MiscTempViewPathBox.Text));
      FixStringOption(OPTIONS_CFGID_KEYDISKPATH, Trim(MiscKeyDiskBox.Text));
      FixIntegerOption(OPTIONS_CFGID_KEYCACHEEXPIRETIME,
                       SecurityKeyCacheExpireTimeUpDown.Position);
      FixIntegerOption(OPTIONS_CFGID_MAXERRORS, MiscMaxErrorsUpDown.Position);
    end
    else begin
      SetStringOption(OPTIONS_CFGID_TEMPVIEWPATH,
                      Trim(MiscTempViewPathBox.Text));
      SetStringOption(OPTIONS_CFGID_KEYDISKPATH, Trim(MiscKeyDiskBox.Text));
      SetIntegerOption(OPTIONS_CFGID_KEYCACHEEXPIRETIME,
                       SecurityKeyCacheExpireTimeUpDown.Position);
      SetIntegerOption(OPTIONS_CFGID_MAXERRORS, MiscMaxErrorsUpDown.Position);
    end;
  end;
end;


procedure TSettingsForm.MakeConfigList;
var
  nI : Integer;
begin
  for nI:=0 to (CONFIG_LOOKUP_COUNT - 1) do
    m_cfgList.Add(TStringKeeper.Create(CONFIG_LOOKUP[nI, CONFIG_IDX_MODULE],
                                       CONFIG_LOOKUP[nI, CONFIG_IDX_OPTION]));
  m_cfgList.ResizeParent;
end;


procedure TSettingsForm.CheckConfigListDependencies(nChangedIdx : Integer = -1);
var
  blRootState : Boolean;
  sModCfg     : String;
  sOptCfg     : String;
  sModCfgCmp  : String;
  sOptCfgCmp  : String;

// some subs to compact the code...

procedure SetCmps(const sMod, sOpt : String);
begin
  sModCfgCmp:=sMod;
  sOptCfgCmp:=sOpt;
end;

function MustCheck : Boolean;
begin
  if (nChangedIdx = -1) then
    Result:=True
  else
    Result:=((sModCfgCmp = sModCfg) and (sOptCfgCmp = sOptCfg));
end;

procedure GetRootState;
var
  nI : Integer;
begin
  // get the state of the root item
  for nI:=0 to (CONFIG_LOOKUP_COUNT - 1) do
    with m_cfgList.GetObject(nI) as TStringKeeper do
      if ((sModCfgCmp = Get) and (sOptCfgCmp = GetSubCnt)) then begin
        blRootState:=m_cfgList.Get(nI).Checked;
        Break;
      end;
end;

procedure Check(const sModCheck, sOptCheck : String);
var
  nI : Integer;
begin
  // we need to search the item to change its state
  for nI:=0 to (CONFIG_LOOKUP_COUNT - 1) do
    with m_cfgList.GetObject(nI) as TStringKeeper do
      if ((sModCheck = Get) and (sOptCheck = GetSubCnt)) then begin
        m_cfgList.Get(nI).Enabled:=blRootState;
        Break;
      end;
end;

begin
  if (nChangedIdx <> -1) then
    with m_cfgList.GetObject(nChangedIdx) as TStringKeeper do begin
      sModCfg:=Get;
      sOptCfg:=GetSubCnt;
    end;

  // step through all switches to check...

  // (currently no dependencies, but here's an example how to do...)

  {
  SetCmps(FB_CFG_ID, FB_CFGID_);
  if (MustCheck) then begin
    GetRootState;
    Check(FB_CFG_ID, FB_CFGID_);
    Check(FB_CFG_ID, FB_CFGID_);
  end;
  }

end;



procedure TSettingsForm.LoadCipherNames;
var
  nI          : Integer;
  cmanag      : TCipherManager;
  cipherNames : TStringList;
begin
  cmanag:=_globals.GetCipMng;
  cipherNames:=cmanag.GetCiphers;
  with SecurityCipherList.Items do begin
    for nI:=1 to cipherNames.Count do
      Add(cipherNames.Strings[nI - 1]);
  end;
end;



(******************** command handlers ********************)


procedure TSettingsForm.FormCreate(Sender: TObject);
var
  cc       : TSettingsFormCC;
  listener : TSettingsFormStrResListener;
begin
  // not active now
  m_blApplyPossible:=False;

  // get the configuration section
  cc:=TSettingsFormCC.Create(Self);
  m_config:=_globals.GetCfg.GetSection(CONFIG_ID, cc);
  cc.Destroy;

  // set up the window's position
  with m_config do begin
    // main form borders
    Left:=GetIntegerOption(CFGID_LEFT);
    Top:=GetIntegerOption(CFGID_TOP);
  end;

  // create the configuration list and its callback
  m_cfgListCB:=TOnConfigSwitch.Create(Self);
  m_cfgList:=TCheckBoxList.Create(ConfigKeeper,
                                  m_cfgListCB);
  MakeConfigList; // (must be be done before the string listener setup!)

  // get the string resources
  m_sr:=_globals.GetSr;

  // init. listener stuff
  listener:=TSettingsFormStrResListener.Create(Self);
  m_sr.AddListener(listener);
  listener.TriggerNoRegUpdate; // (avoid unnecessary registry modifications)
  listener.ChangeStrings(m_sr);

  // init. the cipher names
  LoadCipherNames;

  // init. the updown control systems (must be done here!)
  MiscMaxErrorsUpDown.Min:=MAXERRORS_MIN;
  MiscMaxErrorsUpDown.Max:=MAXERRORS_MAX;
  MiscMaxErrorsBox.MaxLength:=Length(IntToStr(MAXERRORS_MAX));
  SecurityKeyCacheExpireTimeUpDown.Min:=EXPIRETIME_MIN;
  SecurityKeyCacheExpireTimeUpDown.Max:=EXPIRETIME_MAX;
  SecurityKeyCacheExpireTimeBox.MaxLength:=Length(IntToStr(EXPIRETIME_MAX));

  // nothing to apply at this time
  ApplyBtn.Enabled:=False;

  // load the options (the first time)
  m_blReloading:=False; // (not really necessary)
  ReloadOptions;

  // set the open dialog to the last used path
  OpenDialog.InitialDir:=m_config.GetStringOption('INITDIR');

  // active page is always the first one
  SheetHolder.ActivePage:=SecuritySheet;

end;



procedure TSettingsForm.FormShow(Sender: TObject);
var
  blShellExts   : Boolean;
  blFileAssoc   : Boolean;
  blSaveState   : Boolean;

begin
  // save this button's enabled state
  blSaveState:=MiscRegUpdateBtn.Enabled;

  // save the state of the apply button (for the case of a close)
  m_blApplyBtnWasEnabled:=ApplyBtn.Enabled;

  // assume a cancel
  ModalResult:=mrCancel;

  // active now
  m_blApplyPossible:=True;

  // reload the registry settings
  TRegistryKeys.Check(blFileAssoc, blShellExts);
  MiscRegFileAssocSwitch.Checked:=blFileAssoc;
  MiscRegShellExtsSwitch.Checked:=blShellExts;

  // restore this button's enabled state
  MiscRegUpdateBtn.Enabled:=blSaveState;

end;


procedure TSettingsForm.OKBtnClick(Sender: TObject);
begin
  // store all changes
  if (not Validate) then begin
    ModalResult:=mrNone;
    Exit;
  end;
  StoreSettings(False);
  ModalResult:=mrOk;
end;

procedure TSettingsForm.ApplyBtnClick(Sender: TObject);
begin
  // apply after validation
  if (not Validate) then
    Exit;
  StoreSettings(True);
  ApplyBtn.Enabled:=False;
end;


procedure TSettingsForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  // store the current window position
  m_config.FixIntegerOption(CFGID_LEFT, Left);
  m_config.FixIntegerOption(CFGID_TOP, Top);

  // save the inital directory of the open dialog
  m_config.FixStringOption(CFGID_INITDIR, OpenDialog.InitialDir);

  // cancel close?
  if (ModalResult = mrCancel) then begin

    // overwrite the options changes made perhaps
    ReloadOptions;

    // disable the apply button, if necessary
    if ((not m_blApplyBtnWasEnabled) and (ApplyBtn.Enabled)) then
      ApplyBtn.Enabled:=False;
  end;

  // not active anymore
  m_blApplyPossible:=False;
end;


procedure TSettingsForm.FormDestroy(Sender: TObject);
begin
  // free the configuration switches list and its associated listener
  m_cfgList.Destroy;
  m_cfgListCB.Destroy;
end;


procedure TSettingsForm.HelpBtnClick(Sender: TObject);
begin
  Application.HelpCommand(HELP_CONTEXT, 301);
end;

procedure TSettingsForm.CheckRenameMethodDependencies;
var
  blState : Boolean;
begin
  blState:=FileHandlingRenameMaskSwitch.Checked;

  FileHandlingMaskInfo.Enabled:=blState;
  FileHandlingMaskBox.Enabled:=blState;
  FileHandlingExtInfo.Enabled:=blState;
  FileHandlingExtBox.Enabled:=blState;
  FileHandlingNumberInfo.Enabled:=blState;
end;



//////////////// simple control change checkers ////////////////

procedure TSettingsForm.SecurityCompressListChange(Sender: TObject);
begin
  EnableApply;
end;

procedure TSettingsForm.SecurityCipherListChange(Sender: TObject);
begin
  EnableApply;
end;

procedure TSettingsForm.SecurityWipeChange(Sender: TObject);
begin
  EnableApply;
end;

procedure TSettingsForm.FileHandlingNoCompressBoxChange(Sender: TObject);
begin
  EnableApply;
end;

procedure TSettingsForm.FileHandlingMaskBoxChange(Sender: TObject);
begin
  EnableApply;
end;

procedure TSettingsForm.FileHandlingExtBoxChange(Sender: TObject);
begin
  EnableApply;
end;

procedure TSettingsForm.FileHandlingRenameMaskSwitchClick(Sender: TObject);
begin
  EnableApply;
  CheckRenameMethodDependencies;
end;

procedure TSettingsForm.FileHandlingRenameRandomSwitchClick(
  Sender: TObject);
begin
  EnableApply;
  CheckRenameMethodDependencies;
end;

procedure TSettingsForm.SecurityKeyCacheExpireTimeBoxChange(Sender: TObject);
begin
  EnableApply;
end;

procedure TSettingsForm.MiscMaxErrorsBoxChange(Sender: TObject);
begin
  EnableApply;
end;

procedure TSettingsForm.MiscTempViewPathBoxChange(Sender: TObject);
begin
  EnableApply;
end;

procedure TSettingsForm.MiscKeyDiskBoxChange(Sender: TObject);
begin
  EnableApply;
end;

////////////////////////////////////////////////////////////////


procedure TSettingsForm.MiscClearKeyHashesBtnClick(Sender: TObject);
begin
  // clear the key hashes after a confirmation
  with m_sr, _globals.GetKeyChecker do
    if (Application.MessageBox(PChar(Format(
                                 Get(CONFIG_ID, 'CLEARKEYHASHESREQ'),
                                 [NumberOfHashes])),
                               PChar(Get(CONFIG_ID, 'CONFIRM')),
                               MB_ICONQUESTION or MB_YESNO) = IDYES) then begin
      ClearList;
      MiscClearKeyHashesBtn.Enabled:=False;
    end;
end;

procedure TSettingsForm.CheckKeyCacheExpireDependencies;
var
  blState : Boolean;
begin
  blState:=not SecurityKeyCacheNeverExpireSwitch.Checked;
  SecurityKeyCacheExpireTimeInfo.Enabled:=blState;
  SecurityKeyCacheExpireTimeBox.Enabled:=blState;
  SecurityKeyCacheExpireTimeUpDown.Enabled:=blState;
end;


procedure TSettingsForm.SecurityKeyCacheNeverExpireSwitchClick(
  Sender: TObject);
begin
  EnableApply;
  CheckKeyCacheExpireDependencies;
end;

procedure TSettingsForm.MiscMakeKeyDiskBtnClick(Sender: TObject);
begin
  // all is done in this external form
  with MakeKeyDiskForm do begin
    SetKeyFileName(Trim(MiscKeyDiskBox.Text));
    ShowModal;
  end;
end;

procedure TSettingsForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = #27) then begin
    ModalResult:=mrCancel;
    Close;
    Key:=#0;
  end;
end;

function TSettingsForm.ValidateEditControl(edCtrl : TEdit;
                                           sheet : TTabSheet;
                                           upDown : TUpDown;
                                           const sCaption : String) : Boolean;
var
  nValue : Integer;
  sValue : String;

function FilterCaption : String;
var
  nI, nLen : Integer;
begin
  nLen:=Length(sCaption);
  Result:='';
  for nI:=1 to nLen do
    case sCaption[nI] of
      ':' : begin end;
      '&' : begin end;
    else
      Result:=result + sCaption[nI];
    end;
end;

begin
  Result:=False;
  sValue:=Trim(edCtrl.Text);
  edCtrl.Text;
  try
    nValue:=StrToInt(sValue);
  except
    on EConvertError do begin
      Application.MessageBox(
        PChar(Format(m_sr.Get(CONFIG_ID, 'ILLEGALVALUE'), [FilterCaption])),
        PChar(m_sr.Get(CONFIG_ID, 'ERROR')),
        MB_ICONSTOP);
        SheetHolder.ActivePage:=sheet;
        edCtrl.SetFocus;
        Exit;
    end;
  end;
  if ((nValue > upDown.Max) or (nValue < upDown.Min)) then begin
    Application.MessageBox(
      PChar(Format(m_sr.Get(CONFIG_ID, 'OUTOFRANGE'),
                   [FilterCaption, upDown.Min, upDown.Max])),
      PChar(m_sr.Get(CONFIG_ID, 'ERROR')),
      MB_ICONSTOP);
      SheetHolder.ActivePage:=sheet;
      edCtrl.SetFocus;
  end
  else begin
    upDown.Position:=nValue;
    Result:=True;
  end;
end;


function TSettingsForm.Validate : Boolean;
var
  sDir : String;
begin
  Result:=False;

  // check the edit controls
  if (not ValidateEditControl(MiscMaxErrorsBox,
                              MiscSheet,
                              MiscMaxErrorsUpDown,
                              m_sr.Get(CONFIG_ID, 'MISC_MAXERRORS'))) then
    Exit;
  if (not ValidateEditControl(SecurityKeyCacheExpireTimeBox,
                              SecuritySheet,
                              SecurityKeyCacheExpireTimeUpDown,
                              m_sr.Get(CONFIG_ID,
                                      'SECURITY_KC_EXPIRETIME'))) then
    Exit;

  // check path stuff (FIXME: is this necessary or even advisable?)
  sDir:=TStrPlus.PurePath(Trim(MiscTempViewPathBox.Text));
  if (not DirectoryExists(sDir)) then begin
    Application.MessageBox(
      PChar(Format(m_sr.Get(CONFIG_ID, 'TVPATHNOTEXISTS'), [sDir])),
      PChar(m_sr.Get(CONFIG_ID, 'ERROR')),
      MB_ICONSTOP);
    Exit;
  end;
  MiscTempViewPathBox.Text:=sDir;

  // everything seems to be ok
  Result:=True;
end;


procedure TSettingsForm.MiscTempViewPathBrowseBtnClick(Sender: TObject);
var
  bff : TBrowseForFolder;
begin
  bff:=TBrowseForFolder.Create;
  with bff do begin
    Setup(TStrPlus.PurePath(MiscTempViewPathBox.Text));
    if (bff.Execute(m_sr.Get(CONFIG_ID, 'SELECTPATH'), 
                    Handle)) then
      MiscTempViewPathBox.Text:=GetFolder;
  end;
  bff.Destroy;
end;


procedure TSettingsForm.MiscRegUpdateBtnClick(Sender: TObject);
begin
  // set the new registry values
  TRegistryKeys.Update(MiscRegFileAssocSwitch.Checked,
                       MiscRegShellExtsSwitch.Checked,
                       m_sr);

  Application.MessageBox(PChar(m_sr.Get(CONFIG_ID, 'REGUPDATEMESS')),
                         PChar(PROGRAM_NAME),
                         MB_ICONINFORMATION);

  MiscRegUpdateBtn.Enabled:=False;
end;

// you're not entering the world of codemagic...

procedure TSettingsForm.SheetHolderChanging(Sender: TObject;
  var AllowChange: Boolean);
begin
  // stupid trick, but it works: to avoid an unwanted apply due controls which
  // fire events during their activation we simulate a reload operation
  m_blReloading:=True;
end;

procedure TSettingsForm.MiscSheetShow(Sender: TObject);
begin
  // (no "reloading" anymore)
  m_blReloading:=False;
end;

procedure TSettingsForm.SecuritySheetShow(Sender: TObject);
begin
  // (no "reloading" anymore)
  m_blReloading:=False;
end;

procedure TSettingsForm.ConfigSheetShow(Sender: TObject);
begin
  // (no "reloading" anymore)
  m_blReloading:=False;
end;

procedure TSettingsForm.FileHandlingSheetShow(Sender: TObject);
begin
  // (no "reloading" anymore)
  m_blReloading:=False;
end;

procedure TSettingsForm.MiscRegFileAssocSwitchClick(Sender: TObject);
begin
  MiscRegUpdateBtn.Enabled:=True;
end;

procedure TSettingsForm.MiscRegShellExtsSwitchClick(Sender: TObject);
begin
  MiscRegUpdateBtn.Enabled:=True;
end;

function TSettingsForm.FormHelp(Command: Word; Data: Integer;
  var CallHelp: Boolean): Boolean;
begin
  CallHelp := False;
  Result:=HtmlHelp_ShowContext(Data);
end;

procedure TSettingsForm.SecurityMnCipherShowClick(Sender: TObject);
var
  nCis : Integer;
  sInfo : String;
  cinf : TCipherInfo;

function PrintBytesAndBits(nVal : Integer) : String;
begin
  Result:=IntToStr(nVal) + ' (' + IntToStr(nVal shl 3) + ' ' +
    m_sr.Get(CONFIG_ID, 'SECURITY_CPHINF_BITS') + ')';
end;

begin

  cinf:=Nil;
  try
    cinf:=TCipherInfo.Create(SecurityCipherList.Text);

    sInfo:='';

    sInfo:=sInfo + m_sr.Get(CONFIG_ID, 'SECURITY_CPHINF_BLOCKSIZE') + ' ' +
           PrintBytesAndBits(cinf.GetBlockSize) + #13#10;

    sInfo:=sInfo + m_sr.Get(CONFIG_ID, 'SECURITY_CPHINF_KEYSIZE') + ' ' +
           PrintBytesAndBits(cinf.GetKeySize) + #13#10;

    sInfo:=sInfo + m_sr.Get(CONFIG_ID, 'SECURITY_CPHINF_TYPE') + ' ';

    nCis:=cinf.GetCipherIs;

         if (0 <> (nCis and CIPHER_IS_XORSTREAM)) then sInfo:=sInfo + 'stream'
    else if (0 <> (nCis and CIPHER_IS_BLOCKLINK)) then sInfo:=sInfo + 'blocklink'
    else if (0 <> (nCis and CIPHER_IS_BLOCK    )) then sInfo:=sInfo + 'block'
    else                                               sInfo:=sInfo + '???';

    Application.MessageBox(PChar(sInfo),
                           PChar(SecurityCipherList.Text),
                           0);
  except
    on e : Exception do begin
      Application.MessageBox(PChar(e.Message),
                             PChar(SecurityCipherList.Text),
                             MB_ICONSTOP);
    end;
  end;

  if (Nil <> cinf) then cinf.Destroy;
end;

end.
