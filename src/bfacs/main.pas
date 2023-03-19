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
  the main window of Blowfish Advanced CS
}

unit Main;

interface

{$I config.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, StdCtrls, ToolWin, ComCtrls, ExtCtrls, ImgList, Buttons,
  LoaderWin,
  CallBack,
  MessageCallBack,
  GUIMessageCBImpl,
  Executor,
  PasswordWin,
  KeyCache;



// our own status bar callback
type
  TMainFormSBCB = class(TCallback)
  private
    // members
    m_blShowMessageOnRefresh : Boolean;
  public
    // callback
    // exception: ECallBackInterrupt if an user break occured
    procedure CallBack; override;

    // to change the flag which will force the callback to show a message with
    // break information in the statusbar the first time a callback occurs
    // -> True: show the message / False: don't show it
    procedure ShowMessageOnRefresh(blState : Boolean);
  end;


// callback for the key cache expiration info
type
  TMainFormKCCB = class(TKeyCacheCallBack)
  public
    // callback function
    // (won't cause any interrupt currently)
    procedure CallBack; override;
  end;



// ID of the config. section
const
  MF_CFG_ID = 'MAINFORM';


// (browser) reload codes
const
  BRL_REFRESH    = 0;
  BRL_CHANGEPATH = 1;
  BRL_UPONELEVEL = 2;

// our own messages for the tray icon
const
  TASKBAR_MESSAGE  = WM_APP + 100;
  TASKBAR_SHOWMESS = WM_APP + 101;


// the GUI
type
  TMainForm = class(TForm)
    StatusBar: TStatusBar;
    Menu: TMainMenu;
    MN_File: TMenuItem;
    MN_File_Encrypt: TMenuItem;
    MN_File_Decrypt: TMenuItem;
    MN_File_WorkWith: TMenuItem;
    MN_File_N2: TMenuItem;
    MN_File_N3: TMenuItem;
    MN_File_N5: TMenuItem;
    MN_File_N16: TMenuItem;
    MN_File_Reencrypt: TMenuItem;
    MN_File_Wipe: TMenuItem;
    MN_File_Exit: TMenuItem;
    MN_File_Deslack: TMenuItem;
    MN_File_View: TMenuItem;
    MN_Browser_N6: TMenuItem;
    MN_Browser_N7: TMenuItem;
    MN_Browser_N8: TMenuItem;
    MN_Browser: TMenuItem;
    MN_Browser_Browse: TMenuItem;
    MN_Browser_Icons: TMenuItem;
    MN_Browser_SmallIcons: TMenuItem;
    MN_Browser_List: TMenuItem;
    MN_Browser_Report: TMenuItem;
    MN_Browser_Scanner: TMenuItem;
    MN_Browser_UpOneLevel: TMenuItem;
    MN_Browser_NewFolder: TMenuItem;
    MN_Browser_Refresh: TMenuItem;
    MN_Tools_N1: TMenuItem;
    MN_Tools_N9: TMenuItem;
    MN_Tools_ClearDiskSpace: TMenuItem;
    MN_Tools_Language: TMenuItem;
    MN_Tools: TMenuItem;
    MN_Tools_Options: TMenuItem;
    MN_Favorites: TMenuItem;
    MN_Favorites_AddPath: TMenuItem;
    MN_Favorites_Manage: TMenuItem;
    MN_Favorites_Separator: TMenuItem;
    MN_Help: TMenuItem;
    MN_Help_Help: TMenuItem;
    MN_Help_N10: TMenuItem;
    MN_Help_About: TMenuItem;
    PopupMenu: TPopupMenu;
    PM_N13: TMenuItem;
    PM_N14: TMenuItem;
    PM_N15: TMenuItem;
    PM_Copy: TMenuItem;
    PM_Move: TMenuItem;
    PM_Delete: TMenuItem;
    PM_Select: TMenuItem;
    PM_Select_All: TMenuItem;
    PM_Select_Files: TMenuItem;
    PM_Select_Folders: TMenuItem;
    PM_Select_Encrypted: TMenuItem;
    PM_Select_Decrypted: TMenuItem;
    PM_Select_ByString: TMenuItem;
    PM_Rename: TMenuItem;
    MN_Browser_ChangePath: TMenuItem;
    MN_Tools_ExpireKeyCache: TMenuItem;
    MN_Browser_Exclude: TMenuItem;
    MN_Browser_Exclude_Archive: TMenuItem;
    MN_Browser_Exclude_Readonly: TMenuItem;
    MN_Browser_Exclude_Hidden: TMenuItem;
    MN_Browser_Exclude_System: TMenuItem;
    MN_Tools_ShowButtons: TMenuItem;
    MN_Tools_ShowBrowserTools: TMenuItem;
    MN_Browser_N1: TMenuItem;
    MN_Browser_N3: TMenuItem;
    MN_Tools_N4: TMenuItem;
    MN_Tools_N2: TMenuItem;
    MN_Browser_ChooseFont: TMenuItem;
    FontDialog: TFontDialog;
    MN_Tools_ClearWorkWithFavs: TMenuItem;
    MN_Language_DE: TMenuItem;
    MN_Language_US: TMenuItem;
    MN_Tools_SDFC: TMenuItem;
    Herrin: TPanel;
    ButtonsBar: TPanel;
    ButtonsLine: TBevel;
    FlatSeparator: TBevel;
    BrowserTools: TPanel;
    BT_Line: TBevel;
    BT_PathInputBox: TEdit;
    Browser: TListView;
    SmallButts: TImageList;
    SmallBar: TToolBar;
    BT_UpOneLevelBtn: TToolButton;
    Seperator0: TToolButton;
    BT_RefreshBtn: TToolButton;
    BT_BrowseBtn: TToolButton;
    Seperator2: TToolButton;
    BT_MakeDirBtn: TToolButton;
    Seperator3: TToolButton;
    BT_IconsBtn: TToolButton;
    BT_SmallIconsBtn: TToolButton;
    BT_ListBtn: TToolButton;
    BT_ReportBtn: TToolButton;
    LargeBar: TToolBar;
    EncryptBtn: TToolButton;
    DecryptBtn: TToolButton;
    WipeBtn: TToolButton;
    Seperator1: TToolButton;
    WorkWithBtn: TToolButton;
    Seperator5: TToolButton;
    OptionsBtn: TToolButton;
    ExitBtn: TToolButton;
    Seperator4: TToolButton;
    AboutBtn: TToolButton;
    HelpBtn: TToolButton;
    LargePants: TImageList;
    LastWorkFilesMenu: TPopupMenu;
    TrayMenu: TPopupMenu;
    TM_N1: TMenuItem;
    TM_Show: TMenuItem;
    TM_Exit: TMenuItem;
    Seperator6: TToolButton;
    BT_HistBackBtn: TToolButton;
    BT_HistForwardBtn: TToolButton;
    PM_N1: TMenuItem;
    PM_Encrypt: TMenuItem;
    PM_Decrypt: TMenuItem;
    PM_Wipe: TMenuItem;
    PM_WorkWith: TMenuItem;
    PM_N3: TMenuItem;
    MN_Help_N11: TMenuItem;
    MN_Help_Web: TMenuItem;
    PM_N16: TMenuItem;
    PM_Format: TMenuItem;
    MN_Language_XT: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BrowserDblClick(Sender: TObject);
    procedure BrowserKeyPress(Sender: TObject; var Key: Char);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure BrowserDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure BrowserStartDrag(Sender: TObject;
      var DragObject: TDragObject);
    procedure MN_Browser_BrowseClick(Sender: TObject);
    procedure MN_Help_AboutClick(Sender: TObject);
    procedure MN_Browser_RefreshClick(Sender: TObject);
    procedure MN_Browser_UpOneLevelClick(Sender: TObject);
    procedure MN_Browser_IconsClick(Sender: TObject);
    procedure MN_Browser_SmallIconsClick(Sender: TObject);
    procedure MN_Browser_ListClick(Sender: TObject);
    procedure MN_Browser_ReportClick(Sender: TObject);
    procedure MN_Favorites_ManageClick(Sender: TObject);
    procedure MN_Favorites_AddPathClick(Sender: TObject);
    procedure PM_Select_AllClick(Sender: TObject);
    procedure PM_Select_FilesClick(Sender: TObject);
    procedure PM_Select_FoldersClick(Sender: TObject);
    procedure PM_CopyClick(Sender: TObject);
    procedure PM_MoveClick(Sender: TObject);
    procedure PM_DeleteClick(Sender: TObject);
    procedure BrowserMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure BrowserDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure PM_RenameClick(Sender: TObject);
    procedure MN_Browser_NewFolderClick(Sender: TObject);
    procedure MN_Browser_ScannerClick(Sender: TObject);
    procedure BrowserToolsResize(Sender: TObject);
    procedure BT_UpOneLevelBtnClick(Sender: TObject);
    procedure BT_RefreshBtnClick(Sender: TObject);
    procedure BT_BrowseBtnClick(Sender: TObject);
    procedure BT_MakeDirBtnClick(Sender: TObject);
    procedure BT_IconsBtnClick(Sender: TObject);
    procedure BT_SmallIconsBtnClick(Sender: TObject);
    procedure BT_ListBtnClick(Sender: TObject);
    procedure BT_ReportBtnClick(Sender: TObject);
    procedure BT_PathInputBoxKeyPress(Sender: TObject; var Key: Char);
    procedure BrowserColumnClick(Sender: TObject; Column: TListColumn);
    procedure PopupMenuPopup(Sender: TObject);
    procedure MN_Browser_ChangePathClick(Sender: TObject);
    procedure StatusBarResize(Sender: TObject);
    procedure MN_Tools_ExpireKeyCacheClick(Sender: TObject);
    procedure BrowserMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure MN_Tools_OptionsClick(Sender: TObject);
    procedure OptionsBtnClick(Sender: TObject);
    procedure MN_Browser_Exclude_ArchiveClick(Sender: TObject);
    procedure MN_Browser_Exclude_ReadonlyClick(Sender: TObject);
    procedure MN_Browser_Exclude_HiddenClick(Sender: TObject);
    procedure MN_Browser_Exclude_SystemClick(Sender: TObject);
    procedure MN_Tools_ShowButtonsClick(Sender: TObject);
    procedure MN_Tools_ShowBrowserToolsClick(Sender: TObject);
    procedure BrowserClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure ExitBtnClick(Sender: TObject);
    procedure MN_File_ExitClick(Sender: TObject);
    procedure PM_Select_EncryptedClick(Sender: TObject);
    procedure PM_Select_DecryptedClick(Sender: TObject);
    procedure PM_Select_ByStringClick(Sender: TObject);
    procedure MN_File_EncryptClick(Sender: TObject);
    procedure EncryptBtnClick(Sender: TObject);
    procedure WipeBtnClick(Sender: TObject);
    procedure MN_File_WipeClick(Sender: TObject);
    procedure DecryptBtnClick(Sender: TObject);
    procedure MN_File_DecryptClick(Sender: TObject);
    procedure MN_File_ReencryptClick(Sender: TObject);
    procedure MN_File_DeslackClick(Sender: TObject);
    procedure WorkWithBtnClick(Sender: TObject);
    procedure MN_File_WorkWithClick(Sender: TObject);
    procedure MN_Browser_ChooseFontClick(Sender: TObject);
    procedure MN_Tools_ClearWorkWithFavsClick(Sender: TObject);
    procedure AboutBtnClick(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure MN_Tools_ClearDiskSpaceClick(Sender: TObject);
    procedure MN_Language_DEClick(Sender: TObject);
    procedure MN_Language_USClick(Sender: TObject);
    procedure MN_File_ViewClick(Sender: TObject);
    procedure MN_Tools_SDFCClick(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure MN_Help_HelpClick(Sender: TObject);
    procedure BrowserSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure FormDestroy(Sender: TObject);
    procedure TM_ShowClick(Sender: TObject);
    procedure TM_ExitClick(Sender: TObject);
    procedure BT_HistBackBtnClick(Sender: TObject);
    procedure BT_HistForwardBtnClick(Sender: TObject);
    procedure PM_EncryptClick(Sender: TObject);
    procedure PM_DecryptClick(Sender: TObject);
    procedure PM_WipeClick(Sender: TObject);
    procedure PM_WorkWithClick(Sender: TObject);
    function FormHelp(Command: Word; Data: Integer;
      var CallHelp: Boolean): Boolean;
    procedure MN_Help_WebClick(Sender: TObject);
    procedure PM_FormatClick(Sender: TObject);
    procedure MN_Language_XTClick(Sender: TObject);

  private
    // tray icon visible and active?
    m_blTrayIconVisible : Boolean;
    // (the loader form is visible until the first activate event)
    m_lf : TLoaderForm;
    // startup (detection) flags
    m_blStartup : Boolean;
    // shutdown detection flag
    m_blShutdown : Boolean;
    // GUI lock flag
    m_blGUILocked : Boolean;
    // custom selection running flag
    m_blDisableSelect : Boolean;
    // to detect a previous active browser auto refresh and scanning
    m_blWasBrowserAutoRefresh : Boolean;
    m_blWasBrowserScannerActive : Boolean;
    // status bar callback
    m_sbcb : TMainFormSBCB;
    // key cache expiration callback
    m_kccb : TMainFormKCCB;
    // general message callback
    m_gmcb : TGUIMessageCBImpl;
    // password input instance
    m_passwInput : TPasswordInputImpl;
    // execution unit
    m_exec : TExecutor;

    // cursor saver
    m_savCur : TCursor;

    // to detect a pressed ESC key
    m_blESCPressed : Boolean;

    // last tick count (used for seeding)
    m_dwLastTickCount : DWORD;

    // drag+drop stuff
    m_blDragDropCopy : Boolean;

    // handler for clicks on the favorites menu
    // -> (common Delphi) sender object
    procedure FavoritesClickHandler(Sender: TObject);

    // handler for clicks on the workfiles (favorites) menu
    // -> sender object
    procedure WorkFilesMenuClick(Sender: TObject);

    // to enabled/disable the correct menu items (dependant on what's selected)
    procedure SwitchMenuItems;

    // init. the GUI controls
    procedure InitControls;

    // launch the file browser
    procedure LaunchFileBrowser;

    // updates the history (buttons)
    procedure UpdateHistoryCtrls;

    // store the main form's settings
    procedure SaveSettings;

    // prepare the GUI before any action is started
    // -> the action to show in the systray (hint)
    // -> True: lock the GUI / False: do not
    procedure PrepareAction(sAction : String = '';
                            blLockGUI : Boolean = True);

    // finishes and resets the GUI after an action
    procedure FinalizeAction;

    // shows the browser status
    // -> True: refresh all views / False: not
    procedure ShowBrowserStats(blShowAll : Boolean = True);

    // to detect if the user pressed the ulimate abotion key
    // -> True: reset the break flag / False: do not
    function WasESCPressed(blSetFalseWhenTrue : Boolean = True) : Boolean;

    // refreshes the favorites menu entries
    procedure SetFavoritesToMenu;

    // refreshes the workfiles (favorites) menu entries
    procedure RefreshWorkFileFavorites;

    // turns the hints on/off
    // -> True: hints on/ False: hints off
    procedure ShowHints(blShow : Boolean);

    // turns the toolbars on/off (dependant from the options)
    procedure VisualizeToolBars;

    // visualizes the current path
    procedure ShowCurrentPath;

    // updates all controls affecting the expiration of user keys
    // -> to detect if the key has expired (if True)
    procedure UpdateKeyCacheControls(blExpired : Boolean);

    // refreshes the file browsers
    // -> how to refresh, see BRL_xxx
    // -> the new path if we want to change
    // -> True: a break will return True / False: break = error
    // -> True: update the history / False: do not update
    // <- True: success / False: error occured
    function BrowserReload(nHowTo : Integer = BRL_REFRESH;
                           sNewPath : String = '';
                           blBreakOK : Boolean = False;
                           blUpdateHistory : Boolean = True) : Boolean;

    // lock the GUI to avoid message handling during path refreshes (the only
    // way to intercept will be the [Esc] key)
    // -> True: lock / False: unlock
    procedure LockGUI(blLock : Boolean);

    // to check if the GUI's locked
    // -> True: beeps if locked / False: no sound, please
    // <- True: locked / False: lock
    function IsGUILocked(blBeep : Boolean = True) : Boolean;

    // message handler for dropped objects
    // -> the message got
    procedure WMDropFiles(var msg : TMessage); message WM_DROPFILES;

    // universal handler for startup and dropped objects
    // -> ptr to drop message or Nil for startup
    // <- True: shutdown might be demanded / False: forget that shutdown thing
    function ProcessObjects(pDropMsg : PMessage) : Boolean;

    // shows tips at the startup
    procedure ShowStartupTips;

    // stores the current path
    procedure StoreCurrentPath;

    // shows the tray icon or removes it
    // -> True: show / False: hide/remove
    procedure TaskBarIconState(blShow : Boolean);

    // minimize handler
    // -> common handler object
    procedure AppMinimize(Sender: TObject);

    // restore handler
    // -> common handler object
    procedure AppRestore(Sender: TObject);

    // don't ask...
    // -> a message!
    function HookProc(var msg : TMessage) : Boolean;

  protected
    // message handler for tray icon events
    procedure TrayIconHandler(var msg : TMessage); message TASKBAR_MESSAGE;

  public

    // sets the tray icon caption
    // -> new caption
    procedure SetTrayIconCaption(const sCaption : String);

    // locks the tray icon menu
    // -> True: lock / False: unlock
    procedure LockTrayMenu(blLock : Boolean);

    // shows current action in the application title
    // -> the action
    procedure SetAction(sAction : String);

    // to turn on/off the browser tasks (currently scanner and auto refresh)
    // -> True: reenable / False: temporary disable
    // -> True: update the browser, if necessary / False: do not do it
    procedure EnableBrowserTasks(blEnable : Boolean;
                                 blDoUpdate : Boolean = True);
  end;




var
  MainForm: TMainForm;

implementation
uses ShellAPI,
     About,
     BrowseForFolder,
     Configuration,
     bfacslib,
     DestSelect,
     FavWin,
     FileBrowser,
     FileSupp,
     General,
     Globals,
     GlobalsGUI,
     LogWin,
     MakeDirWin,
     MessBoxYNAC,
     Options,
     ProgressWin,
     PathInput,
     RenameWin,
     SecureMem,
     SettingsWin,
     ShortcutChecker,
     StringPlusI,
     Startup,
     BFJob,
     StringRes,
     StringSearchWin,
     PathSearchWin,
     JobChooserWin,
     DiskClearWin,
     TipWin,
     HtmlHelpAPI,
     SDFCWin;


{$R *.DFM}

{$R images\cursors.res}


// cursor IDs
const
  CR_DRAGCOPY = 1;
  CR_DRAGMOVE = 2;


// panel IDs
const
  PANEL_INFO1  = 0;
  PANEL_INFO2  = 1;
  PANEL_SCAN   = 2;
  PANEL_EXPIRE = 3;



//////////////////////// TMainFormSBCB ////////////////////////

procedure TMainFormSBCB.CallBack;
var
  blNoBreak : Boolean;
begin
  with m_callBackObj as TMainForm do begin

    blNoBreak:=(FILEBROWSER_CALLBACK_NOBREAK = GetSignal(False));

    // must we show the break info?
    if (m_blShowMessageOnRefresh) then begin
      if (blNoBreak) then
        StatusBar.Panels[PANEL_INFO1].Text:=''
      else
        StatusBar.Panels[PANEL_INFO1].Text:=_globals.GetSr.Get(MF_CFG_ID,
                                                               'PRESS_ESC');
      m_blShowMessageOnRefresh:=False;
    end;

    // show the reported stuff
    StatusBar.Panels[PANEL_INFO2].Text:=GetMessage;

    // show what we got
    if (blNoBreak) then begin
      StatusBar.Refresh;
    end
    else begin
      Application.ProcessMessages;
    end;

    // ESC pressed?
    if (WasESCPressed(True) and (not m_blShutdown) and (not blNoBreak)) then
      raise ECallBackInterrupt.Create(_globals.GetSr.Get(MF_CFG_ID,
                                                         'ESC_DETECT'));
  end;
end;

procedure TMainFormSBCB.ShowMessageOnRefresh(blState : Boolean);
begin
  m_blShowMessageOnRefresh:=blState;
end;


//////////////////////// TMainFormKCCB ////////////////////////


procedure TMainFormKCCB.CallBack;
var
  nSecs : Integer;
begin
  // just show the remaining seconds in the first panel of the status bar
  with GetCallBackObj as TMainForm do begin

    nSecs:=GetRemainingSeconds;

    if (0 = nSecs) then
      UpdateKeyCacheControls(True)
    else begin
      UpdateKeyCacheControls(False);

      if (KEYCACHE_EXIPRETIME_NEVER <> nSecs) then
        Statusbar.Panels[PANEL_EXPIRE].Text:=IntToStr(GetRemainingSeconds);
    end;

  end;
end;




//////////////////////// TMainForm ////////////////////////

// configuration IDs
const
  CFGID_LEFT        = 'Left';
  CFGID_TOP         = 'Top';
  CFGID_WIDTH       = 'Width';
  CFGID_HEIGHT      = 'Height';
  CFGID_WINDOWSTATE = 'WindowState';
  CFGID_PASSWORD    = 'Password';



{**************************** helper methods ****************************}


// main form configuration checker
type
  TMainFormCC = class(TConfigurationChecker)
  private
    m_parent : TMainForm;
  public
    constructor Create(parent : TMainForm);
    procedure RunCheck(section : TConfigurationSection); override;
  end;

constructor TMainFormCC.Create(parent : TMainForm);
begin
  m_parent:=parent;
end;

procedure TMainFormCC.RunCheck(section : TConfigurationSection);
begin
  // correct borders?
  CheckInt(section,
           CFGID_WIDTH,
           500,
           0,
           CONFIGURATION_MAXINT);
  CheckInt(section,
           CFGID_HEIGHT,
           350,
           0,
           CONFIGURATION_MAXINT);
  CheckInt(section,
           CFGID_LEFT,
           (Screen.Width - section.GetIntegerOption(CFGID_WIDTH)) shr 1,
           0,
           CONFIGURATION_MAXINT);
  CheckInt(section,
           CFGID_TOP,
           (Screen.Height - section.GetIntegerOption(CFGID_HEIGHT)) shr 1,
           0,
           CONFIGURATION_MAXINT);
  CheckInt(section,
           CFGID_WINDOWSTATE,
           Integer(wsNormal));
  CheckString(section,
              CFGID_PASSWORD,
              '');
end;


procedure TMainForm.InitControls;
var
  cc  : TMainFormCC;
  vst : TViewStyle;
begin
  // init. the controls wisely and smooth
  cc:=TMainFormCC.Create(Self);
  with _globals.GetCfg.GetSection(MF_CFG_ID, cc) do begin
    // main form borders
    Left:=GetIntegerOption(CFGID_LEFT);
    Top:=GetIntegerOption(CFGID_TOP);
    Width:=GetIntegerOption(CFGID_WIDTH);
    Height:=GetIntegerOption(CFGID_HEIGHT);
    WindowState:=TWindowState(GetIntegerOption(CFGID_WINDOWSTATE));
  end;
  cc.Destroy;

  // setup the browser's (menu) options
  with _globals.GetOpts.GetCfg do begin
    MN_Browser_Exclude_Archive.Checked:=
      GetBooleanOption(OPTIONS_CFGID_EXCLUDEARCHIVE);
    MN_Browser_Exclude_Readonly.Checked:=
      GetBooleanOption(OPTIONS_CFGID_EXCLUDEREADONLY);
    MN_Browser_Exclude_Hidden.Checked:=
      GetBooleanOption(OPTIONS_CFGID_EXCLUDEHIDDEN);
    MN_Browser_Exclude_System.Checked:=
       GetBooleanOption(OPTIONS_CFGID_EXCLUDESYSTEM);
  end;

  with _globals.GetCfg.GetSection(FB_CFG_ID) do begin
    vst:=TViewStyle(GetIntegerOption(FB_CFGID_VIEWSTYLE));
    MN_Browser_Icons.Checked:=(vst = vsIcon);
    MN_Browser_SmallIcons.Checked:=(vst = vsSmallIcon);
    MN_Browser_List.Checked:=(vst = vsList);
    MN_Browser_Report.Checked:=(vst = vsReport);
    BT_IconsBtn.Down:=(vst = vsIcon);
    BT_SmallIconsBtn.Down:=(vst = vsSmallIcon);
    BT_ListBtn.Down:=(vst = vsList);
    BT_ReportBtn.Down:=(vst = vsReport);
  end;

  // setup the tool menu options
  with _globals.GetOpts.GetCfg do begin
    MN_Tools_ShowButtons.Checked:=
      GetBooleanOption(OPTIONS_CFGID_SHOWBUTTONS);
    MN_Tools_ShowBrowserTools.Checked:=
      GetBooleanOption(OPTIONS_CFGID_SHOWBROWSERTOOLS);
  end;

  // now the rest
  with _globals.GetOpts.GetCfg do begin
    ShowHints(GetBooleanOption(OPTIONS_CFGID_SHOWHINTS));
    VisualizeToolBars;
    ShowCurrentPath;
  end;
  _globals.GetFileBrowser.Layout;
end;


procedure TMainForm.LaunchFileBrowser;
begin
  _globals.GetFileBrowser.Layout;
  if (not BrowserReload(BRL_CHANGEPATH,
                        _globals.GetOpts
                                .GetCfg
                                .GetStringOption(OPTIONS_CFGID_LASTWORKPATH),
                        True)) then
    // (this should always work)
    BrowserReload(BRL_CHANGEPATH,
                  ExtractFilePath(Application.ExeName));
end;



procedure TMainForm.SaveSettings;
begin
  // store size, position and state
  with _globals.GetCfg.GetSection(MF_CFG_ID) do begin
    if (WindowState = wsNormal) then begin
      FixIntegerOption(CFGID_WIDTH, Width);
      FixIntegerOption(CFGID_HEIGHT, Height);
      FixIntegerOption(CFGID_LEFT, Left);
      FixIntegerOption(CFGID_TOP, Top);
    end;
    FixIntegerOption(CFGID_WINDOWSTATE, Integer(WindowState));
  end;
end;


function TMainForm.BrowserReload(nHowTo : Integer = BRL_REFRESH;
                                 sNewPath : String = '';
                                 blBreakOK : Boolean = False;
                                 blUpdateHistory : Boolean = True) : Boolean;
begin
  // change or just reload the current path
  PrepareAction;
  Result:=False;
  try
    // set the exclude mask every time
    with _globals.GetOpts.GetCfg do
      _globals.GetFileBrowser.SetExclAttrMask(TFileSupport.MakeExcludeAttrMask(
                            GetBooleanOption(OPTIONS_CFGID_EXCLUDEARCHIVE),
                            GetBooleanOption(OPTIONS_CFGID_EXCLUDEREADONLY),
                            GetBooleanOption(OPTIONS_CFGID_EXCLUDEHIDDEN),
                            GetBooleanOption(OPTIONS_CFGID_EXCLUDESYSTEM)));

    // reload now
    if (nHowTo = BRL_REFRESH) then
      _globals.GetFileBrowser.Refresh(blUpdateHistory)
    else begin
      if (nHowTo = BRL_CHANGEPATH) then
        _globals.GetFileBrowser.ChangePath(sNewPath,
                                           blUpdateHistory)
      else
        _globals.GetFileBrowser.UpOneLevel;
      // (got change - store the new path)
      StoreCurrentPath;

    end;
    Result:=True;
  except
    // standard browse error handling
    on EFileBrowserInterrupt do begin
      Result:=blBreakOK;
    end;
    on efbw : EFileBrowserWarning do begin
      Application.MessageBox(PChar(efbw.Message),
                             PChar(_globals.GetSr.Get(MF_CFG_ID, 'WARNING')),
                             MB_ICONEXCLAMATION);
    end;
    on efbe : EFileBrowserError do
      Application.MessageBox(PChar(efbe.Message),
                             PChar(_globals.GetSr.Get(MF_CFG_ID, 'ERROR')),
                             MB_ICONSTOP);
  end;

  if (blUpdateHistory) then
    UpdateHistoryCtrls;

  FinalizeAction;
end;


procedure TMainForm.SetFavoritesToMenu;
var
  nI         : Integer;
  nSepaLimit : Integer;
  newItem    : TMenuItem;
  favsList   : TStringList;
begin
  // remove old items (all above the separator)
  nSepaLimit:=MN_Favorites_Separator.MenuIndex + 1;
  while (MN_Favorites.Count > nSepaLimit) do
    MN_Favorites.Delete(nSepaLimit);

  // (re)set the favorites
  favsList:=_globals.GetFavorites.GetList;
  for nI:=0 to (favsList.Count - 1) do begin
    newItem:=TMenuItem.Create(Self);
    newItem.Caption:=favsList.Strings[nI];
    newItem.OnClick:=FavoritesClickHandler;
    newItem.HelpContext:=400;
    MN_Favorites.Add(newItem);
  end;

  // don't show the manage menu entry if we have no favorites
  MN_Favorites_Manage.Enabled:=(favsList.Count > 0);
end;


procedure TMainForm.ShowCurrentPath;
var
  sPath : String;
begin
  // show the path in the input box or (if it is not visible) in the caption
  if (BrowserTools.Visible) then begin
    BT_PathInputBox.Text:=_globals.GetFileBrowser.GetCurrentPath;
    Caption:=PROGRAM_NAME;
  end
  else begin
    sPath:=_globals.GetFileBrowser.GetCurrentPath;
    if (sPath = '') then
      Caption:=PROGRAM_NAME
    else
      Caption:=PROGRAM_NAME + ' - ' + sPath;
  end;
end;

function TMainForm.WasESCPressed(blSetFalseWhenTrue : Boolean = True) : Boolean;
begin
  Result:=m_blESCPressed;
  // drop the flag, if necessary
  if (blSetFalseWhenTrue and m_blESCPressed) then
    m_blESCPressed:=False;
end;

procedure TMainForm.PrepareAction(sAction : String = '';
                                  blLockGUI : Boolean = True);
begin
  m_sbcb.ShowMessageOnRefresh(True);
  m_blESCPressed:=False;
  SetAction(sAction);
  m_savCur:=Screen.Cursor;
  Screen.Cursor:=crHourGlass;

  // disable all necessary controls
  if (blLockGUI) then
    LockGUI(True);
  LockTrayMenu(True);
end;


// (call this routine after every possible browsing)
procedure TMainForm.FinalizeAction;
var
  blState : Boolean;
begin
  // enable/disable browser path dependant buttons and menu items
  // (popup menu items are set dynamically on every show)
  blState:=_globals.GetFileBrowser.CanGoUp;
  BT_UpOneLevelBtn.Enabled:=blState;
  MN_Browser_UpOneLevel.Enabled:=blState;

  blState:=not _globals.GetFileBrowser.IsReadOnlyDrive;
  BT_MakeDirBtn.Enabled:=blState;
  MN_Browser_NewFolder.Enabled:=blState;
  MN_File_Reencrypt.Enabled:=blState;
  MN_File_Wipe.Enabled:=blState;
  WipeBtn.Enabled:=blState;
  MN_File_Deslack.Enabled:=blState;

  // other stuff
  ShowBrowserStats;
  UpdateHistoryCtrls;
  SwitchMenuItems;
  SetAction('');

  // reenable all controls
  LockGUI(False);
  LockTrayMenu(False);

  // focus to the browser
  Screen.Cursor:=m_savCur;
  Browser.SetFocus;

end;


procedure TMainForm.ShowBrowserStats(blShowAll : Boolean = True);
var
  fb : TFileBrowser;
begin
  // show the number of files and folders, i. n.
  fb:=_globals.GetFileBrowser;
  if (blShowAll) then
    StatusBar.Panels[PANEL_INFO1].Text:=fb.GetCountInfo;

  // show either the selected or the path state
  if (fb.GetNumOfSelObjs = 0) then
    StatusBar.Panels[PANEL_INFO2].Text:=fb.GetContentInfo
  else
    StatusBar.Panels[PANEL_INFO2].Text:=fb.GetSelectedInfo;

  // show the new path
  ShowCurrentPath;
end;


procedure TMainForm.ShowHints(blShow : Boolean);
begin
  // turn the hints on or off
  EncryptBtn.ShowHint:=blShow;
  DecryptBtn.ShowHint:=blShow;
  WipeBtn.ShowHint:=blShow;
  WorkWithBtn.ShowHint:=blShow;
  OptionsBtn.ShowHint:=blShow;
  ExitBtn.ShowHint:=blShow;
  AboutBtn.ShowHint:=blShow;
  HelpBtn.ShowHint:=blShow;
  BT_UpOneLevelBtn.ShowHint:=blShow;
  BT_RefreshBtn.ShowHint:=blShow;
  BT_BrowseBtn.ShowHint:=blShow;
  BT_MakeDirBtn.ShowHint:=blShow;
  BT_IconsBtn.ShowHint:=blShow;
  BT_SmallIconsBtn.ShowHint:=blShow;
  BT_ListBtn.ShowHint:=blShow;
  BT_ReportBtn.ShowHint:=blShow;
  BT_HistBackBtn.ShowHint:=blShow;
  BT_HistForwardBtn.ShowHint:=blShow;
end;


procedure TMainForm.VisualizeToolBars;
begin
  with _globals.GetOpts.GetCfg do begin

    // (FIXME: seems to be the only way to keep the right order, right?)

   // Browser.Align:=alNone;
    ButtonsBar.Align:=alNone;
    BrowserTools.Align:=alNone;
    FlatSeparator.Align:=alNone;

    ButtonsBar.Visible:=GetBooleanOption(OPTIONS_CFGID_SHOWBUTTONS);
    BrowserTools.Visible:=GetBooleanOption(OPTIONS_CFGID_SHOWBROWSERTOOLS);
    FlatSeparator.Visible:=_globals.GetCfg.GetSection(FB_CFG_ID)
                                          .GetBooleanOption(FB_CFGID_FLATVIEW);

    FlatSeparator.Align:=alTop;
    BrowserTools.Align:=alTop;
    ButtonsBar.Align:=alTop;
  //  Browser.Align:=alClient;
  end;
end;


procedure TMainForm.UpdateKeyCacheControls(blExpired : Boolean);
begin
  // update all controls which depend on a valid key in the cache
  MN_Tools_ExpireKeyCache.Enabled:=(not blExpired);
  if (blExpired) then
    Statusbar.Panels[PANEL_EXPIRE].Text:='';
end;


procedure TMainForm.LockGUI(blLock : Boolean);
begin
  m_blGUILocked:=blLock;

  // en-/disable all controls
  // (Herrin is german and means mistress, she really dominates here =~~)
  Herrin.Enabled:=not blLock;

end;


function TMainForm.IsGUILocked(blBeep : Boolean = True) : Boolean;
begin
  if (m_blGUILocked) then
    Beep;
  Result:=m_blGUILocked;
end;




{**************************** event handlers ****************************}



// main form resource listener
type
  TMainFormStrResListener = class(TStrResListener)
  public
    procedure ChangeStrings(sr : TStrRes); override;
  end;

procedure TMainFormStrResListener.ChangeStrings(sr : TStrRes);
var
  scc : TShortcutChecker;

procedure SMIC(mi : TMenuItem;
               const sCfgId : String);
begin
  mi.Caption:=scc.AddShortcut(sr.Get(MF_CFG_ID, sCfgID));
end;

begin
  // link MainForm directly (not very proper, but ok for our purposes)
  with MainForm, sr do begin

    // use a shortcut checker for an easier setup
    scc:=TShortcutChecker.Create;

    // label the "main" menu items first
    SMIC(MN_File,      'MN_FILE');
    SMIC(MN_Browser,   'MN_BROWSER');
    SMIC(MN_Tools,     'MN_TOOLS');
    SMIC(MN_Favorites, 'MN_FAVORITES');
    SMIC(MN_Help,      'MN_HELP');

    // change strings in the menu
    scc.Reset;
    SMIC(MN_File_Encrypt,      'MN_FILE_ENCRYPT');
    SMIC(MN_File_Decrypt,      'MN_FILE_DECRYPT');
    SMIC(MN_File_View,         'MN_FILE_VIEW');
    SMIC(MN_File_WorkWith,     'MN_FILE_WORKWITH');
    SMIC(MN_File_Reencrypt,    'MN_FILE_REENCRYPT');
    SMIC(MN_File_Wipe,         'MN_FILE_WIPE');
    SMIC(MN_File_Deslack,      'MN_FILE_DESLACK');
    SMIC(MN_File_Exit,         'MN_FILE_EXIT');
    scc.Reset;
    SMIC(MN_Browser_Browse,     'MN_BROWSER_BROWSE');
    SMIC(MN_Browser_ChangePath, 'MN_BROWSER_CHANGEPATH');
    SMIC(MN_Browser_UpOneLevel, 'MN_BROWSER_UPONELEVEL');
    SMIC(MN_Browser_Refresh,    'MN_BROWSER_REFRESH');
    SMIC(MN_Browser_NewFolder,  'MN_BROWSER_NEWFOLDER');
    SMIC(MN_Browser_Icons,      'MN_BROWSER_ICONS');
    SMIC(MN_Browser_SmallIcons, 'MN_BROWSER_SMALLICONS');
    SMIC(MN_Browser_List,       'MN_BROWSER_LIST');
    SMIC(MN_Browser_Report,     'MN_BROWSER_REPORT');
    SMIC(MN_Browser_Exclude,    'MN_BROWSER_EXCLUDE');
    SMIC(MN_Browser_Scanner,    'MN_BROWSER_SCANNER');
    SMIC(MN_Browser_ChooseFont, 'MN_BROWSER_CHOOSEFONT');
    scc.Reset;
    SMIC(MN_Browser_Exclude_Archive,  'MN_BROWSER_EXCLUDE_ARCHIVE');
    SMIC(MN_Browser_Exclude_Readonly, 'MN_BROWSER_EXCLUDE_READONLY');
    SMIC(MN_Browser_Exclude_Hidden,   'MN_BROWSER_EXCLUDE_HIDDEN');
    SMIC(MN_Browser_Exclude_System,   'MN_BROWSER_EXCLUDE_SYSTEM');

    scc.Reset;
    SMIC(MN_Tools_Options,           'MN_TOOLS_OPTIONS');
    SMIC(MN_Tools_ShowButtons,       'MN_TOOLS_SHOWBUTTONS');
    SMIC(MN_Tools_ShowBrowserTools,  'MN_TOOLS_SHOWBROWSERTOOLS');
    SMIC(MN_Tools_ExpireKeyCache,    'MN_TOOLS_EXPIREKEYCACHE');
    SMIC(MN_Tools_SDFC,              'MN_TOOLS_SDFC');
    SMIC(MN_Tools_ClearWorkWithFavs, 'MN_TOOLS_CLEARWORKWITHFAVS');
    SMIC(MN_Tools_Language,          'MN_TOOLS_LANGUAGE');
    SMIC(MN_Tools_ClearDiskSpace,    'MN_TOOLS_CLEARDISKSPACE');
    scc.Reset;
    SMIC(MN_Favorites_AddPath, 'MN_FAVORITES_ADDPATH');
    SMIC(MN_Favorites_Manage,  'MN_FAVORITES_MANAGE');
    scc.Reset;
    SMIC(MN_Help_Help,  'MN_HELP_HELP');
    SMIC(MN_Help_Web,   'MN_HELP_WEB');
    SMIC(MN_Help_About, 'MN_HELP_ABOUT');

    // same game with the submenus
    scc.Reset;
    SMIC(PM_Encrypt,          'PM_ENCRYPT');
    SMIC(PM_Decrypt,          'PM_DECRYPT');
    SMIC(PM_Wipe,             'PM_WIPE');
    SMIC(PM_WorkWith,         'PM_WORKWITH');
    SMIC(PM_Copy,             'PM_COPY');
    SMIC(PM_Move,             'PM_MOVE');
    SMIC(PM_Rename,           'PM_RENAME');
    SMIC(PM_Delete,           'PM_DELETE');
    SMIC(PM_Select,           'PM_SELECT');
    SMIC(PM_Format,           'PM_FORMAT');
    scc.Reset;
    SMIC(PM_Select_All,       'PM_SELECT_ALL');
    SMIC(PM_Select_Files,     'PM_SELECT_FILES');
    SMIC(PM_Select_Folders,   'PM_SELECT_FOLDERS');
    SMIC(PM_Select_Encrypted, 'PM_SELECT_ENCRYPTED');
    SMIC(PM_Select_Decrypted, 'PM_SELECT_DECRYPTED');
    SMIC(PM_Select_ByString,  'PM_SELECT_BYSTRING');

    // and the tray menu
    scc.Reset;
    SMIC(TM_Show, 'TM_SHOW');
    SMIC(TM_Exit, 'TM_EXIT');

    // load the hints
    EncryptBtn.Hint       :=Get(MF_CFG_ID, 'HINT_ENCRYPT');
    DecryptBtn.Hint       :=Get(MF_CFG_ID, 'HINT_DECRYP');
    WipeBtn.Hint          :=Get(MF_CFG_ID, 'HINT_WIPE');
    WorkWithBtn.Hint      :=Get(MF_CFG_ID, 'HINT_WORKWITH');
    OptionsBtn.Hint       :=Get(MF_CFG_ID, 'HINT_OPTIONS');
    ExitBtn.Hint          :=Get(MF_CFG_ID, 'HINT_EXIT');
    AboutBtn.Hint         :=Get(MF_CFG_ID, 'HINT_ABOUT');
    HelpBtn.Hint          :=Get(MF_CFG_ID, 'HINT_HELP');
    BT_UpOneLevelBtn.Hint :=Get(MF_CFG_ID, 'HINT_BT_UPONELEVEL');
    BT_RefreshBtn.Hint    :=Get(MF_CFG_ID, 'HINT_BT_REFRESH');
    BT_BrowseBtn.Hint     :=Get(MF_CFG_ID, 'HINT_BT_BROWSE');
    BT_MakeDirBtn.Hint    :=Get(MF_CFG_ID, 'HINT_BT_NEWFOLDER');
    BT_IconsBtn.Hint      :=Get(MF_CFG_ID, 'HINT_BT_ICONS');
    BT_SmallIconsBtn.Hint :=Get(MF_CFG_ID, 'HINT_BT_SMALLICONS');
    BT_ListBtn.Hint       :=Get(MF_CFG_ID, 'HINT_BT_LIST');
    BT_ReportBtn.Hint     :=Get(MF_CFG_ID, 'HINT_BT_REPORT');
    BT_HistBackBtn.Hint   :=Get(MF_CFG_ID, 'HINT_BT_HISTBACK');
    BT_HistForwardBtn.Hint:=Get(MF_CFG_ID, 'HINT_BT_HISTFORWARDS');

    scc.Destroy;
  end;
end;



procedure TMainForm.FormCreate(Sender: TObject);
var
  loaderCB   : TLoaderCallBack;
  listener   : TMainFormStrResListener;
  srExtended : TStrRes;
begin

  // attach some handlers
  Application.OnMinimize:=AppMinimize;
  Application.OnRestore:=AppRestore;

  // necessary to avoid hard shutdowns
  Application.HookMainWindow(HookProc);

  // startup is running (will be reset on the first activate signal)
  m_blStartup:=True;

  // no shutdown, of course
  m_blShutdown:=False;

  // GUI is not locked initially
  m_blGUILocked:=False;

  // no tray icon right now
  m_blTrayIconVisible:=False;

  // no auto browser refresh and scanner detected yet
  m_blWasBrowserAutoRefresh:=False;
  m_blWasBrowserScannerActive:=False;

  // load the cursor(s)
  Screen.Cursors[CR_DRAGCOPY]:=LoadCursor(HInstance, 'CR_DRAGCOPY');
  Screen.Cursors[CR_DRAGMOVE]:=LoadCursor(HInstance, 'CR_DRAGMOVE');

  // create the statusbar callback, used by the file browser
  // and the job executor
  m_sbcb:=TMainFormSBCB.Create(Self);

  // create the key cache and the message callback
  m_kccb:=TMainFormKCCB.Create(Self);
  m_gmcb:=TGUIMessageCBImpl.Create(YNACBox);

  // make the loader window and its callback
  m_lf:=TLoaderForm.Create(Self);
  loaderCB:=TLoaderCallBack.Create(m_lf);
  m_lf.Show;

  // new password input
  m_passwInput:=TPasswordInputImpl.Create;

  // launch the globals
  try
    _globals:=TGlobalsGUI.Create(
        '',
        Browser,
        m_passwInput,
        m_sbcb,
        m_kccb,
        m_gmcb,
        loaderCB);
  except
    // launch failed?
    on ge : EGlobalsError do begin
      Application.MessageBox(
        PChar(ge.Message),
        PChar(Application.Title),
        MB_ICONERROR);
      loaderCB.Destroy;
      m_lf.Close;
      m_lf.Destroy;
      m_lf:=Nil;
      m_sbcb.Destroy;
      m_kccb.Destroy;
      m_gmcb.Destroy;
      Application.Terminate;
      raise ge;
    end;
  end;

  // no execution unit yet
  m_exec:=Nil;

  // cursor to default
  Screen.Cursor:=crArrow;
  m_savCur:=Screen.Cursor;

  // show that we're now init. the controls
  loaderCB.SetMessage(_globals.GetSr.Get(MF_CFG_ID, '000'));
  loaderCB.CallBack;
  loaderCB.Destroy;

  // ok, register us for string resource updates
  listener:=TMainFormStrResListener.Create;
  _globals.GetSr.AddListener(listener);

  // check if an (valid) extended language is available
  srExtended:=_globals.LoadStringResources(LANGUAGE_XT);
  if (Nil <> srExtended) then begin
    MN_Language_XT.Caption:=srExtended.GetCountryConfig.GetCountry;
    MN_Language_XT.Visible:=True;
    srExtended.Destroy;
  end
  else begin
    MN_Language_XT.Visible:=False;
  end;

  // call this listener directly
  listener.ChangeStrings(_globals.GetSr);

  // set the favorites menu
  SetFavoritesToMenu;

  // init. the history buttons
  UpdateHistoryCtrls;

  // set the title of this application
  Caption:=PROGRAM_NAME;

  // init. the last tick count
  m_dwLastTickCount:=GetTickCount;

  // init. custom selection run flag
  m_blDisableSelect:=False;

  // init. the controls
  InitControls;

  // activate the tray icon, if necessary
  TaskBarIconState(_globals.GetOpts
                           .GetCfg
                           .GetBooleanOption(OPTIONS_CFGID_TRAYICON));

end;


procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
  shutDownCB : TMainFormSBCB;
begin
  // shutting down
  m_blShutdown:=True;

  // the first job is to destroy the viewed files
  _globals.GetBFAViewer.Clear(m_sbcb, m_gmcb);

  // use the status bar to show the shutdown progress
  shutDownCB:=TMainFormSBCB.Create(Self);
  shutDownCB.ShowMessageOnRefresh(False);

  // save this form's settings
  SaveSettings;

  // shut down now
  // FIXME: 2 step process now, an overloaded destructor caused access
  //        violations when running the app in the IDE's debug mode!?!
  _globals.ShutDown(shutDownCB);
  _globals.Destroy;

  // loader form not needed anymore
  shutDownCB.Destroy;

  // release the statusbar callback
  m_sbcb.Destroy;
  // (no selection handling anymore now)
  m_blDisableSelect:=True;

  // destroy the other callbacks
  m_kccb.Destroy;
  m_gmcb.Destroy;

  // release the password input
  m_passwInput.Destroy;

  // no more executions
  if (m_exec <> Nil) then
    m_exec.Destroy;

  // clever, huh?
  LargePants.ShareImages:=False;
  SmallButts.ShareImages:=False;

 // dirty (but quick) shutdown of the file browser view
//  Browser.Destroy;

end;



procedure TMainForm.BrowserDblClick(Sender: TObject);
var
  cfg : TConfigurationSection;
  sl  : TStringList;

begin
  // (although we might not only browse to a new path everything must be
  //  prepared for such a case)

  UpdateWindow(Browser.Handle);

  PrepareAction('', True);

  cfg:=_globals.GetOpts.GetCfg;

  try

    // forward the double click event
    case _globals.GetFileBrowser.HandleDoubleClick of

      FILEBROWSER_DBLCLICK_CHANGEDPATH : begin

        // (store the changed path)
        StoreCurrentPath;

      end;

      FILEBROWSER_DBLCLICK_BFAREQUEST : begin

        // view or work with the file?
        FinalizeAction;
        if (cfg.GetBooleanOption(OPTIONS_CFGID_DOUBLECLICKWORK)) then
          MN_File_WorkWithClick(Sender)
        else
          MN_File_ViewClick(Sender);
        Exit;
      end;

      FILEBROWSER_DBLCLICK_BFJREQUEST : begin
        FinalizeAction;
        sl:=TStringList.Create;
        sl.Add( _globals.GetFileBrowser.GetSingleSelectedFile);
        m_exec.GUILaunchJobFiles(sl);
        sl.Destroy;
      end;

    end;

  except
    on EFileBrowserInterrupt do begin
      // (interrupts don't cause an error message)
    end;
    on efbw : EFileBrowserWarning do begin
      // (warnings do)
      Application.MessageBox(PChar(efbw.Message),
                             PChar(_globals.GetSr.Get(MF_CFG_ID, 'WARNING')),
                             MB_ICONEXCLAMATION);
    end;
    on efbe : EFileBrowserError do
      // (other errors do, too)
      Application.MessageBox(PChar(efbe.Message),
                             PChar(_globals.GetSr.Get(MF_CFG_ID, 'ERROR')),
                             MB_ICONSTOP);
  end;
  FinalizeAction;
end;

procedure TMainForm.BrowserKeyPress(Sender: TObject; var Key: Char);
begin
  // if the return key was pressed, map it to the double click handler
  if (Key = #13) then begin
    BrowserDblClick(Sender);
    Key:=#0;
  end;
end;

procedure TMainForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
  // just set the ESC flag, i. n.
  if (Key = #27) then
  begin
    m_blESCPressed:=True;
    Key:=#0;
  end;
end;



procedure TMainForm.MN_Browser_BrowseClick(Sender: TObject);
var
  bff : TBrowseForFolder;
begin
  if (IsGUILocked) then Exit;

  // let the user browse for a folder
  bff:=TBrowseForFolder.Create;
  bff.Setup(TStrPlusI.PurePath(ExtractFilePath(
                                _globals.GetFileBrowser.GetCurrentPath)));
  if (bff.Execute(_globals.GetSr.Get(MF_CFG_ID, 'SELECTPATH'),
                  Handle)) then
    BrowserReload(BRL_CHANGEPATH,
                  bff.GetFolder);
  bff.Destroy;
end;

procedure TMainForm.MN_Help_AboutClick(Sender: TObject);
begin
  // show the program information
  AboutForm.ShowModal;
end;

procedure TMainForm.MN_Browser_RefreshClick(Sender: TObject);
begin
  if (IsGUILocked) then Exit;

  // refresh the current path
  BrowserReload;
end;


procedure TMainForm.MN_Browser_UpOneLevelClick(Sender: TObject);
begin
  if (IsGUILocked) then Exit;

  // go up one level in the file browser
  m_sbcb.ShowMessageOnRefresh(True);
  BrowserReload(BRL_UPONELEVEL);
end;

procedure TMainForm.MN_Browser_IconsClick(Sender: TObject);
begin
  if (IsGUILocked) then Exit;

  // change the file browser file style to large icons
  _globals.GetCfg.GetSection('FILEBROWSER')
                 .FixIntegerOption('VIEWSTYLE', Integer(vsIcon));
  _globals.GetFileBrowser.ChangeViewStyle;
  MN_Browser_Icons.Checked:=True;
  BT_IconsBtn.Down:=True;
end;

procedure TMainForm.MN_Browser_SmallIconsClick(Sender: TObject);
begin
  if (IsGUILocked) then Exit;

  // change the file browser file style to small icons
  _globals.GetCfg.GetSection('FILEBROWSER')
                 .FixIntegerOption('VIEWSTYLE', Integer(vsSmallIcon));
  _globals.GetFileBrowser.ChangeViewStyle;
  MN_Browser_SmallIcons.Checked:=True;
  BT_SmallIconsBtn.Down:=True;
end;

procedure TMainForm.MN_Browser_ListClick(Sender: TObject);
begin
  if (IsGUILocked) then Exit;

  // change the file browser file style to list
  _globals.GetCfg.GetSection('FILEBROWSER')
                 .FixIntegerOption('VIEWSTYLE', Integer(vsList));
  _globals.GetFileBrowser.ChangeViewStyle;
  MN_Browser_List.Checked:=True;
  BT_ListBtn.Down:=True;
end;

procedure TMainForm.MN_Browser_ReportClick(Sender: TObject);
begin
  if (IsGUILocked) then Exit;

  // change the file browser file style to report
  _globals.GetCfg.GetSection('FILEBROWSER')
                 .FixIntegerOption('VIEWSTYLE', Integer(vsReport));
  _globals.GetFileBrowser.ChangeViewStyle;
  MN_Browser_Report.Checked:=True;
  BT_ReportBtn.Down:=True;
end;

procedure TMainForm.MN_Favorites_ManageClick(Sender: TObject);
begin
  // show the favorites organizer and refresh the favorites menu, if necessary
  if (FavoritesWin.ShowModal = mrOk) then
    SetFavoritesToMenu;
end;

procedure TMainForm.FavoritesClickHandler(Sender: TObject);
begin
  // change to the selected favorite path
  if (IsGUILocked) then
    Exit;
  BrowserReload(BRL_CHANGEPATH,
                TMenuItem(Sender).Caption);
end;



procedure TMainForm.MN_Favorites_AddPathClick(Sender: TObject);
begin
  if (IsGUILocked) then Exit;

  // add the current path to the favorites
  if (_globals.GetFavorites.Add(_globals.GetFileBrowser.GetCurrentPath)) then
    SetFavoritesToMenu;
end;

procedure TMainForm.BrowserColumnClick(Sender: TObject;
  Column: TListColumn);
begin
  // map the column click (will cause an interruptable sort action, so we use
  // the browsier protectors)
  PrepareAction;
  _globals.GetFileBrowser.HandleColumnClick(Column);
  FinalizeAction;
end;


procedure TMainForm.PM_Select_AllClick(Sender: TObject);
begin
  // select all files and folders
  m_blDisableSelect:=True;
  _globals.GetFileBrowser.SelectAll;
  ShowBrowserStats;
  SwitchMenuItems;
  m_blDisableSelect:=False;
end;

procedure TMainForm.PM_Select_FilesClick(Sender: TObject);
begin
  // select all files
  m_blDisableSelect:=True;
  _globals.GetFileBrowser.SelectFiles;
  ShowBrowserStats;
  SwitchMenuItems;
  m_blDisableSelect:=False;
end;

procedure TMainForm.PM_Select_FoldersClick(Sender: TObject);
begin
  // select all folders
  m_blDisableSelect:=True;
  _globals.GetFileBrowser.SelectFolders;
  ShowBrowserStats;
  SwitchMenuItems;
  m_blDisableSelect:=False;
end;

procedure TMainForm.PM_Select_EncryptedClick(Sender: TObject);
begin
  // select all encrypted files
  m_blDisableSelect:=True;
  _globals.GetFileBrowser.SelectEncrypted;
  ShowBrowserStats;
  SwitchMenuItems;
  m_blDisableSelect:=False;
end;

procedure TMainForm.PM_Select_DecryptedClick(Sender: TObject);
begin
  // select all decrypted files
  m_blDisableSelect:=True;
  _globals.GetFileBrowser.SelectDecrypted;
  ShowBrowserStats;
  SwitchMenuItems;
  m_blDisableSelect:=False;
end;

procedure TMainForm.PM_Select_ByStringClick(Sender: TObject);
begin
  // get the selection string
  StringSearchForm.SetCaption(_globals.GetSr.get(MF_CFG_ID, 'SELSEARCHCAPT'));
  if (StringSearchForm.ShowModal = mrCancel) then
    Exit;

  // now select
  m_blDisableSelect:=True;
  _globals.GetFileBrowser.SelectByString(StringSearchForm.GetSearchText,
                     StringSearchForm.GetCaseSensitive);
  ShowBrowserStats;
  SwitchMenuItems;
  m_blDisableSelect:=False;
end;


procedure TMainForm.PM_CopyClick(Sender: TObject);
var
  sDestPath : String;
begin
  // request a destination path
  DestSelectWin.Setup(_globals.GetSr.Get(MF_CFG_ID, 'COPYDESTCAPTION'));
  if (DestSelectWin.ShowModal = mrCancel) then
    Exit;

  // copy the demanded stuff
  sDestPath:=DestSelectWin.GetDestPath;
  m_blDisableSelect:=True;
  PrepareAction(_globals.GetSr.Get(MF_CFG_ID, 'COPYING'));
  try
    _globals.GetFileBrowser.CopyObjects(sDestPath, m_gmcb);
  except
    on efbi : EFileBrowserInterrupt do
      Application.MessageBox(PChar(efbi.Message),
                             PChar(_globals.GetSr.Get(MF_CFG_ID, 'ABORTED')),
                             MB_ICONEXCLAMATION);
    on efbw : EFileBrowserWarning do
      Application.MessageBox(PChar(efbw.Message),
                             PChar(_globals.GetSr.Get(MF_CFG_ID, 'WARNING')),
                             MB_ICONEXCLAMATION);
    on efbe : EFileBrowserError do
      Application.MessageBox(PChar(efbe.Message),
                             PChar(_globals.GetSr.Get(MF_CFG_ID, 'ERROR')),
                             MB_ICONSTOP);
  end;
  m_blDisableSelect:=False;
  FinalizeAction;
end;

procedure TMainForm.PM_MoveClick(Sender: TObject);
var
  sDestPath : String;
begin
  // request a destination path
  DestSelectWin.Setup(_globals.GetSr.Get(MF_CFG_ID, 'MOVEDESTCAPTION'));
  if (DestSelectWin.ShowModal = mrCancel) then
    Exit;

  // move the demanded stuff
  sDestPath:=DestSelectWin.GetDestPath;
  m_blDisableSelect:=True;
  PrepareAction(_globals.GetSr.Get(MF_CFG_ID, 'MOVING'));
  try
    _globals.GetFileBrowser.MoveObjects(sDestPath, m_gmcb);
  except
    on efbi : EFileBrowserInterrupt do
      Application.MessageBox(PChar(efbi.Message),
                             PChar(_globals.GetSr.Get(MF_CFG_ID, 'ABORTED')),
                             MB_ICONEXCLAMATION);
    on efbw : EFileBrowserWarning do
      Application.MessageBox(PChar(efbw.Message),
                             PChar(_globals.GetSr.Get(MF_CFG_ID, 'WARNING')),
                             MB_ICONEXCLAMATION);
    on efbe : EFileBrowserError do
      Application.MessageBox(PChar(efbe.Message),
                             PChar(_globals.GetSr.Get(MF_CFG_ID, 'ERROR')),
                             MB_ICONSTOP);
  end;
  m_blDisableSelect:=False;
  FinalizeAction;
end;

procedure TMainForm.PM_DeleteClick(Sender: TObject);
begin
  // need a confirmation here
  with _globals.GetSr do
    if (Application.MessageBox(PChar(Get(MF_CFG_ID, 'CONF_DELETE')),
                               PChar(Get(MF_CFG_ID, 'CONFIRM')),
                               MB_ICONQUESTION or MB_YESNO) = IDNO) then
    Exit;

  // delete the demanded stuff
  m_blDisableSelect:=True;
  PrepareAction(_globals.GetSr.Get(MF_CFG_ID, 'DELETING'));
  try
    _globals.GetFileBrowser.DeleteObjects;
  except
    on efbi : EFileBrowserInterrupt do
      Application.MessageBox(PChar(efbi.Message),
                             PChar(_globals.GetSr.Get(MF_CFG_ID, 'ABORTED')),
                             MB_ICONEXCLAMATION);
    on efbw : EFileBrowserWarning do
      Application.MessageBox(PChar(efbw.Message),
                             PChar(_globals.GetSr.Get(MF_CFG_ID, 'WARNING')),
                             MB_ICONEXCLAMATION);
    on efbe : EFileBrowserError do
      Application.MessageBox(PChar(efbe.Message),
                             PChar(_globals.GetSr.Get(MF_CFG_ID, 'ERROR')),
                             MB_ICONSTOP);
  end;
  m_blDisableSelect:=False;
  FinalizeAction;
end;

procedure TMainForm.BrowserMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  // drap and drop for copy or moving?
  m_blDragDropCopy:=(ssAlt in Shift);
end;

procedure TMainForm.BrowserDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  Accept:=_globals.GetFileBrowser.HandleDragOver(X, Y);
end;


procedure TMainForm.BrowserStartDrag(Sender: TObject;
  var DragObject: TDragObject);
begin
  // start the drag + drop operation
  if (m_blDragDropCopy) then begin
    Browser.DragCursor:=CR_DRAGCOPY;
    _globals.GetFileBrowser.HandleDragStart(FILEBROWSER_DRAG_COPY);
  end
  else begin
    Browser.DragCursor:=CR_DRAGMOVE;
    _globals.GetFileBrowser.HandleDragStart(FILEBROWSER_DRAG_MOVE);
  end;
end;


procedure TMainForm.BrowserDragDrop(Sender, Source: TObject; X, Y: Integer);
begin
  // map the drag+drop to the filebrowser
  m_blDisableSelect:=True;
  if (m_blDragDropCopy) then
    PrepareAction(_globals.GetSr.Get(MF_CFG_ID, 'COYPING'))
  else
    PrepareAction(_globals.GetSr.Get(MF_CFG_ID, 'MOVING'));
  try
    _globals.GetFileBrowser.HandleDragDrop(X, Y, m_gmcb);
  except
    on efbi : EFileBrowserInterrupt do
      Application.MessageBox(PChar(efbi.Message),
                             PChar(_globals.GetSr.Get(MF_CFG_ID, 'ABORTED')),
                             MB_ICONEXCLAMATION);
    on efbw : EFileBrowserWarning do
      Application.MessageBox(PChar(efbw.Message),
                             PChar(_globals.GetSr.Get(MF_CFG_ID, 'WARNING')),
                             MB_ICONEXCLAMATION);
    on efbe : EFileBrowserError do
      Application.MessageBox(PChar(efbe.Message),
                             PChar(_globals.GetSr.Get(MF_CFG_ID, 'ERROR')),
                             MB_ICONSTOP);
  end;
  m_blDisableSelect:=False;
  FinalizeAction;
end;


// callbacks for rename actions
type
  TMainFormFBRR = class(TFileBrowserRenameRequest)
  public
    function Request(sOld : String;
                     var vsNew : String) : Boolean; override;
  end;

  TMainFormRenErrMCB = class(TMessageCallBack)
  public
    procedure CallBack; override;
  end;


function TMainFormFBRR.Request(sOld : String;
                               var vsNew : String) : Boolean;
begin
  // get a new file/folder name
  RenameForm.SetOldName(sOld);
  if (RenameForm.ShowModal = mrCancel) then begin
    Result:=False;
    Exit;
  end;
  vsNew:=RenameForm.GetNewName;
  Result:=True;
end;

procedure TMainFormRenErrMCB.CallBack;
var
  nStyle : Integer;
begin
 if (GetStyle = MCB_STYLE_YESNOCANCEL) then
   nStyle:=MB_ICONEXCLAMATION or MB_YESNOCANCEL
 else
   nStyle:=MB_ICONEXCLAMATION or MB_YESNO;
 case (Application.MessageBox(PChar(GetMessage),
                              PChar(_globals.GetSr.Get(MF_CFG_ID, 'ERROR')),
                              nStyle)) of
   IDYES : SetResult(MCB_RES_YES);
   IDNO  : SetResult(MCB_RES_NO);
 else
   raise ECallbackInterrupt.Create('interrupted');
 end;
end;



procedure TMainForm.PM_RenameClick(Sender: TObject);
var
  fbrr   : TMainFormFBRR;
  errMCB : TMainFormRenErrMCB;
begin
  // start rename operation
  fbrr:=TMainFormFBRR.Create;
  errMCB:=TMainFormRenErrMCB.Create(Nil);
  _globals.GetFileBrowser.RenameObjects(fbrr, errMCB);
  errMCB.Destroy;
  fbrr.Destroy;
end;

procedure TMainForm.MN_Browser_NewFolderClick(Sender: TObject);
begin
  if (IsGUILocked) then Exit;

  // make new directory
  if (MakeDirForm.ShowModal = mrOk) then begin
    try
      _globals.GetFileBrowser.NewFolders(MakeDirForm.GetNewDirNames);
    except
      on efbw : EFileBrowserWarning do
        Application.MessageBox(PChar(efbw.Message),
                               PChar(_globals.GetSr.Get(MF_CFG_ID, 'WARNING')),
                               MB_ICONEXCLAMATION);
      on efbe : EFileBrowserError do
        Application.MessageBox(PChar(efbe.Message),
                               PChar(_globals.GetSr.Get(MF_CFG_ID, 'ERROR')),
                               MB_ICONSTOP);
    end;
  end;
  ShowBrowserStats;
end;


// mapped speed buttons...

procedure TMainForm.BT_UpOneLevelBtnClick(Sender: TObject);
begin
  MN_Browser_UpOneLevelClick(Sender);
end;

procedure TMainForm.BT_RefreshBtnClick(Sender: TObject);
begin
  MN_Browser_RefreshClick(Sender);
end;

procedure TMainForm.BT_BrowseBtnClick(Sender: TObject);
begin
  MN_Browser_BrowseClick(Sender);
end;

procedure TMainForm.BT_MakeDirBtnClick(Sender: TObject);
begin
  MN_Browser_NewFolderClick(Sender);
end;

procedure TMainForm.BT_IconsBtnClick(Sender: TObject);
begin
  MN_Browser_IconsClick(Sender);
end;

procedure TMainForm.BT_SmallIconsBtnClick(Sender: TObject);
begin
  MN_Browser_SmallIconsClick(Sender);
end;

procedure TMainForm.BT_ListBtnClick(Sender: TObject);
begin
  MN_Browser_ListClick(Sender);
end;

procedure TMainForm.BT_ReportBtnClick(Sender: TObject);
begin
  MN_Browser_ReportClick(Sender);
end;

procedure TMainForm.BT_PathInputBoxKeyPress(Sender: TObject;
  var Key: Char);
begin
  // on [Enter] change to the entered path, if possible
  if (Key = #13) then begin
    Key:=#0;
    if (IsGUILocked) then
      Exit;
    BrowserReload(BRL_CHANGEPATH, Trim(BT_PathInputBox.Text));
  end;
end;

procedure TMainForm.PopupMenuPopup(Sender: TObject);
var
  blState     : Boolean;
  blDirsThere : Boolean;
begin

  // show some infos about shell extensions
  TipForm.ShowTip(TIP_SHELLEXTENSION);

  // enable/disable path/selection dependant items...
  blState:=_globals.GetFileBrowser.CheckSelection(
                      FILEBROWSER_SELECTED_NOTREADONLY or
                      FILEBROWSER_SELECTED_FILES or
                      FILEBROWSER_SELECTED_FOLDERS);
  PM_Delete.Enabled:=blState;
  PM_Wipe.Enabled:=blState;
  PM_Rename.Enabled:=blState;
  PM_Encrypt.Enabled:=blState;
  PM_Decrypt.Enabled:=blState;

  blState:=_globals.GetFileBrowser.CheckSelection(FILEBROWSER_SELECTED_FILES);
  PM_Copy.Enabled:=blState;
  PM_Move.Enabled:=blState;

  // (treat the select submenu more carefully)
  blState:=(_globals.GetFileBrowser.GetNumOfFiles <> 0);
  blDirsThere:=(_globals.GetFileBrowser.GetNumOfFolders <> 0);
  if ((not blState) and (not blDirsThere)) then begin
    PM_Select.Enabled:=False;
    Exit;
  end
  else
    PM_Select.Enabled:=True;

  PM_Select_Files.Enabled:=blState;
  PM_Select_Encrypted.Enabled:=blState;
  PM_Select_Decrypted.Enabled:=blState;
  PM_Select_ByString.Enabled:=blState;
  PM_Select_All.Enabled:=blState or blDirsThere;
  PM_Select_Folders.Enabled:=blDirsThere;

  // the format item only applies to drives
  PM_Format.Enabled:=_globals.GetFileBrowser.CheckSelection(
    FILEBROWSER_SELECTED_FORMATTABLE);
end;


procedure TMainForm.MN_Browser_ChangePathClick(Sender: TObject);
begin
  if (IsGUILocked) then Exit;

  // let the user enter a new path
  PathInputWin.SetInfo(_globals.GetSr.Get(MF_CFG_ID, 'CP_CAPTION'),
                       _globals.GetSr.Get(MF_CFG_ID, 'CP_PROMPT'));
  if (PathInputWin.ShowModal = mrOk) then
    BrowserReload(BRL_CHANGEPATH,
                  PathInputWin.GetPath);
end;



procedure TMainForm.MN_Tools_ExpireKeyCacheClick(Sender: TObject);
begin
  // expire the key cache
  with  _globals.GetKeyCache do begin
    Expire;
    UpdateKeyCacheControls(not IsValid);
  end;
end;

procedure TMainForm.BrowserMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  nPos        : Integer;
  dwTickCount : DWORD;
  dwLatency   : DWORD;
begin
  with _globals.GetRndMng.GetSeedBuffer do begin

    // add x/y seed data (screen positions can be packed into 32 bits easily)
    nPos:=(X shl 16) or Y;
    AddSeed(@nPos, SizeOf(nPos));

    // add the latency time between two mouse moves, use 8, 16 or 32 bit words
    dwTickCount:=GetTickCount;
    dwLatency:=dwTickCount - m_dwLastTickCount;
    m_dwLastTickCount:=dwTickCount;
    // ignore zero latencies
    if (dwLatency > 0) then
      // thanks to little endian we can point to the right sized word directly
      if (dwLatency > 255) then
        if (dwLatency > 65535) then
          AddSeed(@dwLatency, 4)
        else
          AddSeed(@dwLatency, 2)
      else
        AddSeed(@dwLatency, 1);
  end; (* OF WITH *)
end;


procedure TMainForm.MN_Browser_ScannerClick(Sender: TObject);
var
  blScanActive : Boolean;
begin
  if (IsGUILocked) then Exit;

  // turn the scanner on/off
  blScanActive:=_globals.GetFileBrowser.IsBFAScannerReady;
  if (not blScanActive) then begin

    try
      _globals.GetFileBrowser.StartBFAScanner;
      blScanActive:=True;
    except
      on efbi : EFileBrowserInterrupt do begin
        // (no scanner wanted, so it is)
      end;
      on efbe : EFileBrowserError do begin
        Application.MessageBox(PChar(Format(_globals.GetSr.Get(MF_CFG_ID,
                                                               'SCANERR'),
                                            [efbe.Message])),
                               PChar(_globals.GetSr.Get(MF_CFG_ID, 'ERROR')),
                               MB_ICONSTOP);
      end;
    end;
    UpdateKeyCacheControls(not _globals.GetKeyCache.IsValid);

  end
  else begin
    _globals.GetFileBrowser.StopBFAScanner;
    blScanActive:=False;
  end;

  MN_Browser_Scanner.Checked:=blScanActive;
  if (blScanActive) then begin
    StatusBar.Panels[PANEL_SCAN].Text:=_globals.GetSr.Get(MF_CFG_ID, 'SCAN');

    // show info about what's going on now
    TipForm.ShowTip(TIP_SCANNER);

  end
  else
    StatusBar.Panels[PANEL_SCAN].Text:='';
end;


procedure TMainForm.BrowserToolsResize(Sender: TObject);
begin
  // adjust the path input box
  BT_PathInputBox.Width:=BrowserTools.Width - BT_PathInputBox.Left;
end;




procedure TMainForm.StatusBarResize(Sender: TObject);
var
  nNewWidth : Integer;
begin
  // the middle panel's size is dynamic
  nNewWidth:=StatusBar.Width -
             StatusBar.Panels[PANEL_INFO1].Width -
             StatusBar.Panels[PANEL_SCAN].Width -
             StatusBar.Panels[PANEL_EXPIRE].Width -
             StatusBar.Height; // (for a fine displayed size grip)
  if (nNewWidth < 0) then
    nNewWidth:=0;
  StatusBar.Panels[PANEL_INFO2].Width:=nNewWidth;
end;


procedure TMainForm.MN_Tools_OptionsClick(Sender: TObject);
var
  blSaveTrayIcon         : Boolean;
  blSaveHotTracking      : Boolean;
  blSaveHTMLStyle        : Boolean;
  blSaveGridLines        : Boolean;
  blSaveAutoArrange      : Boolean;
  blSaveFlatView         : Boolean;
  blSaveShowHints        : Boolean;
  blSaveAutoRefresh      : Boolean;
  blSavePlaceDrivesFirst : Boolean;
  blSaveHideDrives       : Boolean;
  sSaveFontName          : String;
  nSaveFontColor         : Integer;
  nSaveFontStyle         : Integer;
  nSaveFontSize          : Integer;
  blFlatViewChanged      : Boolean;
  fbCfgSect              : TConfigurationSection;

begin
  if (IsGUILocked) then Exit;

  // not very smart, but currently it's the only possibility to check for
  // necessary browser updates: save options and look for changes after
  fbCfgSect:=_globals.GetCfg.GetSection(FB_CFG_ID);
  with fbCfgSect do begin
    blSaveHotTracking     :=GetBooleanOption(FB_CFGID_HOTTRACKING);
    blSaveHTMLStyle       :=GetBooleanOption(FB_CFGID_HTMLSTYLE);
    blSaveGridLines       :=GetBooleanOption(FB_CFGID_GRIDLINES);
    blSaveAutoArrange     :=GetBooleanOption(FB_CFGID_AUTOARRANGE);
    blSaveFlatView        :=GetBooleanOption(FB_CFGID_FLATVIEW);
    blSaveAutoRefresh     :=GetBooleanOption(FB_CFGID_AUTOREFRESH);
    blSavePlaceDrivesFirst:=GetBooleanOption(FB_CFGID_PLACEDRIVESFIRST);
    blSaveHideDrives      :=GetBooleanOption(FB_CFGID_HIDEDRIVES);
    sSaveFontName         :=GetStringOption (FB_CFGID_FONTNAME);
    nSaveFontColor        :=GetIntegerOption(FB_CFGID_FONTCOLOR);
    nSaveFontStyle        :=GetIntegerOption(FB_CFGID_FONTSTYLE);
    nSaveFontSize         :=GetIntegerOption(FB_CFGID_FONTSIZE);
  end;
  with _globals.GetOpts.GetCfg do
  begin
    blSaveShowHints:=GetBooleanOption(OPTIONS_CFGID_SHOWHINTS);
    blSaveTrayIcon :=GetBooleanOption(OPTIONS_CFGID_TRAYICON);
  end;

  // (don't care about the result)
  SettingsForm.ShowModal;

  // need to layout?
  with fbCfgSect do begin

    // (turn the flatview separator on or off, if necessary)
    blFlatViewChanged:=(blSaveFlatView <> GetBooleanOption(FB_CFGID_FLATVIEW));
    if (blFlatViewChanged) then
      VisualizeToolbars;  // (the only way for a proper on/off)

    if (blFlatViewChanged or
       (blSaveHotTracking     <> GetBooleanOption(FB_CFGID_HOTTRACKING)) or
       (blSaveHTMLStyle       <> GetBooleanOption(FB_CFGID_HTMLSTYLE)) or
       (blSaveGridLines       <> GetBooleanOption(FB_CFGID_GRIDLINES)) or
       (blSaveAutoArrange     <> GetBooleanOption(FB_CFGID_AUTOARRANGE)) or
       (sSaveFontName         <> GetStringOption (FB_CFGID_FONTNAME)) or
       (nSaveFontColor        <> GetIntegerOption(FB_CFGID_FONTCOLOR)) or
       (nSaveFontStyle        <> GetIntegerOption(FB_CFGID_FONTSTYLE)) or
       (nSaveFontSize         <> GetIntegerOption(FB_CFGID_FONTSIZE))) then
    begin
      _globals.GetFileBrowser.Layout;
      Browser.Refresh;
    end
    else if (
       (blSavePlaceDrivesFirst<> GetBooleanOption(FB_CFGID_PLACEDRIVESFIRST)) or
       (blSaveHideDrives      <> GetBooleanOption(FB_CFGID_HIDEDRIVES))) then
    begin
      BrowserReload;
    end;
  end;

  // toggle browser auto refresh
  if (fbCfgSect.GetBooleanOption(FB_CFGID_AUTOREFRESH) <>
      blSaveAutoRefresh) then begin
    // (FIXME: this is some kind of a design flaw: we have to turn on/off the
    //         auto refresh manually, although the browser is able to do that
    //         almost on its own)
    with _globals.GetFileBrowser do begin
      if (not blSaveAutoRefresh) then Update;
      DoAutoRefresh(not blSaveAutoRefresh);
    end;
  end;

  with _globals.GetOpts.GetCfg do
  begin

    // hints or not?
    if (blSaveShowHints <> GetBooleanOption(OPTIONS_CFGID_SHOWHINTS)) then
      ShowHints(not blSaveShowHints);

    // tray icon or not?
    if (GetBooleanOption(OPTIONS_CFGID_TRAYICON) <> blSaveTrayIcon) then
      TaskBarIconState(not blSaveTrayIcon);

  end;
end;

procedure TMainForm.OptionsBtnClick(Sender: TObject);
begin
  // just map the call to the menu
  MN_Tools_OptionsClick(Sender);
end;

procedure TMainForm.MN_Browser_Exclude_ArchiveClick(Sender: TObject);
begin
  if (IsGUILocked) then Exit;

  with MN_Browser_Exclude_Archive do begin
    Checked:=not Checked;
    _globals.GetOpts.GetCfg
                    .FixBooleanOption(OPTIONS_CFGID_EXCLUDEARCHIVE, Checked);
    BrowserReload;
  end;
end;

procedure TMainForm.MN_Browser_Exclude_ReadonlyClick(Sender: TObject);
begin
  if (IsGUILocked) then Exit;

  with MN_Browser_Exclude_Readonly do begin
    Checked:=not Checked;
    _globals.GetOpts
            .GetCfg.FixBooleanOption(OPTIONS_CFGID_EXCLUDEREADONLY, Checked);
    BrowserReload;
  end;
end;

procedure TMainForm.MN_Browser_Exclude_HiddenClick(Sender: TObject);
begin
  if (IsGUILocked) then Exit;

  with MN_Browser_Exclude_Hidden do begin
    Checked:=not Checked;
    _globals.GetOpts
            .GetCfg.FixBooleanOption(OPTIONS_CFGID_EXCLUDEHIDDEN, Checked);
    BrowserReload;
  end;
end;

procedure TMainForm.MN_Browser_Exclude_SystemClick(Sender: TObject);
begin
  if (IsGUILocked) then Exit;

  with MN_Browser_Exclude_System do begin
    Checked:=not Checked;
    _globals.GetOpts
            .GetCfg.FixBooleanOption(OPTIONS_CFGID_EXCLUDESYSTEM, Checked);
    BrowserReload;
  end;
end;

procedure TMainForm.MN_Tools_ShowButtonsClick(Sender: TObject);
begin
  // toggle the show state of the button bar
  with MN_Tools_ShowButtons do begin
    Checked:=not Checked;
    _globals.GetOpts
            .GetCfg.FixBooleanOption(OPTIONS_CFGID_SHOWBUTTONS, Checked);
    VisualizeToolbars;
  end;
end;

procedure TMainForm.MN_Tools_ShowBrowserToolsClick(Sender: TObject);
begin
  // toggle the show state of the browser tools
  with MN_Tools_ShowBrowserTools do begin
    Checked:=not Checked;
    _globals.GetOpts
            .GetCfg.FixBooleanOption(OPTIONS_CFGID_SHOWBROWSERTOOLS, Checked);
    VisualizeToolbars;
  end;
end;

procedure TMainForm.BrowserClick(Sender: TObject);
begin
  // not very proper, but unless we're not sure what HTML style really means
  // we just route the single clicks to the doubleclick handler, if necessary
  if (_globals.GetCfg.GetSection(FB_CFG_ID)
                     .GetBooleanOption(FB_CFGID_HTMLSTYLE)) then
    BrowserDblClick(Sender);
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  // don't quit if GUI is locked
  CanClose:=not IsGUILocked;
end;

procedure TMainForm.ExitBtnClick(Sender: TObject);
begin
  MN_File_ExitClick(Sender);
end;

procedure TMainForm.MN_File_ExitClick(Sender: TObject);
begin
  if (IsGUILocked) then Exit;

  // byebye
  TaskBarIconState(False);
  Close;
end;


procedure TMainForm.EnableBrowserTasks(blEnable : Boolean;
                                       blDoUpdate : Boolean = True);
begin

  with _globals.GetFileBrowser do begin

    // it's important to first refresh...
    if (blEnable) then begin
      if (blDoUpdate) then
        Update(True);
      if (m_blWasBrowserAutoRefresh) then
        DoAutoRefresh(True);
    end
    else begin
      m_blWasBrowserAutoRefresh:=IsAutoRefreshActive;
      DoAutoRefresh(False);
    end;

    // ...and then activate the scanner
    if (blEnable) then begin
      if (m_blWasBrowserScannerActive) then
        ResumeBFAScanner;
    end
    else begin
      m_blWasBrowserScannerActive:=IsBFAScannerReady;
      SuspendBFAScanner;
    end;
  end;
end;


procedure TMainForm.EncryptBtnClick(Sender: TObject);
begin
  // just map the encryption call
  MN_File_EncryptClick(Sender);
end;


procedure TMainForm.MN_File_EncryptClick(Sender: TObject);
begin
  if (IsGUILocked) then Exit;

  // files and folders selected?
  if (not m_exec.CheckSelection) then begin
    Beep;
    Exit;
  end;

  // goto work
  EnableBrowserTasks(False);
  LockTrayMenu(True);
  m_exec.GUIEncrypt;
  LockTrayMenu(False);
  EnableBrowserTasks(True);
end;



procedure TMainForm.SwitchMenuItems;
var
  nNumOfSelFiles : Integer;
  blNoOtherSel   : Boolean;
  blNotReadOnly  : Boolean;
  blSingleFile   : Boolean;
  blState        : Boolean;
begin
  with _globals.GetFileBrowser do begin
    nNumOfSelFiles:=GetNumOfSelFiles;
    blNoOtherSel:=not (GetNumOfSelOther > 0) and
                  ((nNumOfSelFiles > 0) or (GetNumOfSelDirs > 0));
    blNotReadOnly:=not IsReadOnlyPath;
    blSingleFile:=(nNumOfSelFiles = 1) and (GetNumOfSelObjs = 1);

    blState:=blNoOtherSel and blNotReadOnly;
    MN_File_Wipe.Enabled:=blState;
    MN_File_Reencrypt.Enabled:=blState;
    MN_File_Deslack.Enabled:=blState;

    MN_File_Encrypt.Enabled:=blNoOtherSel;
    MN_File_Decrypt.Enabled:=blNoOtherSel;

    blState:=blNoOtherSel and blSingleFile;
    MN_File_View.Enabled:=blState;

    blState:=blNoOtherSel and blNotReadOnly and blSingleFile;
    MN_File_WorkWith.Enabled:=blState;

    // to in the popup event handler, but here
    PM_WorkWith.Enabled:=blState;

    MN_Tools_ClearDiskSpace.Enabled:=blNotReadOnly;
  end;
end;


procedure TMainForm.WipeBtnClick(Sender: TObject);
begin
  MN_File_WipeClick(Sender);
end;

procedure TMainForm.MN_File_WipeClick(Sender: TObject);
begin
  if (IsGUILocked) then Exit;

  // files and folders selected?
  if (not m_exec.CheckSelection) then begin
    Beep;
    Exit;
  end;

  // goto work
  EnableBrowserTasks(False);
  LockTrayMenu(True);
  m_exec.GUIWipe;
  LockTrayMenu(False);
  EnableBrowserTasks(True);
end;

procedure TMainForm.DecryptBtnClick(Sender: TObject);
begin
  MN_File_DecryptClick(Sender);
end;

procedure TMainForm.MN_File_DecryptClick(Sender: TObject);
begin
  if (IsGUILocked) then Exit;

 // files and folders selected?
  if (not m_exec.CheckSelection) then begin
    Beep;
    Exit;
  end;

  // goto work
  EnableBrowserTasks(False);
  LockTrayMenu(True);
  m_exec.GUIDecrypt;
  LockTrayMenu(False);
  EnableBrowserTasks(True);
end;

procedure TMainForm.MN_File_ReencryptClick(Sender: TObject);
begin
  if (IsGUILocked) then Exit;

  // files and folders selected?
  if (not m_exec.CheckSelection) then begin
    Beep;
    Exit;
  end;

  // goto work
  EnableBrowserTasks(False);
  LockTrayMenu(True);
  m_exec.GUIReencrypt;
  LockTrayMenu(False);
  EnableBrowserTasks(True);
end;

procedure TMainForm.MN_File_DeslackClick(Sender: TObject);
begin
  if (IsGUILocked) then Exit;

  // files and folders selected?
  if (not m_exec.CheckSelection) then begin
    Beep;
    Exit;
  end;

  // goto work
  EnableBrowserTasks(False);
  LockTrayMenu(True);
  m_exec.GUIDeslack;
  LockTrayMenu(False);
  EnableBrowserTasks(True);
end;

procedure TMainForm.WorkWithBtnClick(Sender: TObject);
begin
  MN_File_WorkWithClick(Sender);
end;

procedure TMainForm.MN_File_WorkWithClick(Sender: TObject);
begin
  if (IsGUILocked) then Exit;

  // single file selected?
  if (not m_exec.CheckSelection(True, False)) then begin
    // (FIXME: more than a beep if the path's readonly?)
    Beep;
    Exit;
  end;

  // work with the file
  PrepareAction;
  if (m_exec.GUIWorkWith) then
    RefreshWorkFileFavorites;
  FinalizeAction;
end;

procedure TMainForm.RefreshWorkFileFavorites;
var
  nI       : Integer;
  newItem  : TMenuItem;
  favsList : TStringList;
begin
  with _globals.GetBFAWorkWith.GetFavorites do begin
    // (re)set the workfile favorites
    while (LastWorkFilesMenu.Items.Count > 0) do
      LastWorkFilesMenu.Items.Delete(0);

    favsList:=GetList;
    for nI:=0 to (favsList.Count - 1) do begin
      newItem:=TMenuItem.Create(Self);
      newItem.Caption:=favsList.Strings[nI];
      newItem.OnClick:=WorkFilesMenuClick;
      newItem.HelpContext:=205;
      LastWorkFilesMenu.Items.Add(newItem);
    end;

    MN_Tools_ClearWorkWithFavs.Enabled:=(favsList.Count > 0);
  end;
end;

procedure TMainForm.WorkFilesMenuClick(Sender: TObject);
begin
  // work with the file
  PrepareAction;
  if (m_exec.GUIWorkWith(TMenuItem(Sender).Caption)) then
    RefreshWorkFileFavorites;
  FinalizeAction;
end;


procedure TMainForm.MN_Browser_ChooseFontClick(Sender: TObject);
begin
  // let the user choose a new font for the browser
  _globals.GetFileBrowser.ChooseFont(FontDialog);
end;

procedure TMainForm.MN_Tools_ClearWorkWithFavsClick(Sender: TObject);
begin
  // clear the work with favorites
  _globals.GetBFAWorkwith.GetFavorites.Clear;
  RefreshWorkFileFavorites;
end;

procedure TMainForm.AboutBtnClick(Sender: TObject);
begin
  MN_Help_AboutClick(Sender);
end;


procedure TMainForm.WMDropFiles(var msg : TMessage);
begin
  Application.BringToFront;
  ProcessObjects(@msg);
end;


function TMainForm.ProcessObjects(pDropMsg : PMessage) : Boolean;
var
  blViewJob : Boolean;
  jobReqCB  : TJobRequestCallBack;
  jobFiles  : TStringList;
  jobToDo   : TBFJob;

function CloseAfterExec : Boolean;
begin
  Result:=_globals.GetOpts
                  .GetCfg
                  .GetBooleanOption(OPTIONS_CFGID_CLOSEAFTERWORK);
end;

begin
  Result:=False;
  jobReqCB:=TJobChooserCB.Create(JobChooserForm);
  case TStartup.Execute(jobReqCB, jobFiles, jobToDo, pDropMsg) of

    STARTUP_RES_JOB : begin
      // execute the job got
      blViewJob:=(jobToDo.GetMode = BFJOB_MODE_VIEW);
      m_exec.GUIExecJob(jobToDo);
      Result:=((not blViewJob) and CloseAfterExec);
    end;

    STARTUP_RES_JOBFILES : begin
      // launch the job files
      m_exec.GUILaunchJobFiles(jobFiles);
      Result:=CloseAfterExec;
    end;

    STARTUP_RES_ERROR :
      // (this should never happen)
      RunError(RUNERROR_MAIN_STARTUPERROR);
  end;
  jobReqCB.Destroy;
end;


procedure TMainForm.SetAction(sAction : String);
begin
  if (sAction <> '') then begin
    Application.Title:=PROGRAM_NAME + ' - ' + sAction;
    SetTrayIconCaption(sAction + '...');
  end
  else begin
    Application.Title:=PROGRAM_NAME;
    SetTrayIconCaption(PROGRAM_NAME);
  end;
end;



procedure TMainForm.FormPaint(Sender: TObject);
var
  sPasswMust : String;
  sPasswIs   : String;
begin

  // check out if the program start is running
  if (m_blStartup) then begin

    // no startup anymore
    m_blStartup:=False;

    // we don't need the loader form anymore
    m_lf.Close;
    m_lf.Destroy;
    m_lf:=Nil;

    // init. the password input
    with _globals do begin
      m_passwInput.Setup(GetOpts,
                         GetCipMng,
                         GetKeyCache,
                         PasswordForm,
                         GetRndMng);
    end;

    // create the execution unit now
    m_exec:=TExecutor.Create(_globals,
                             Self,
                             ProgressForm,
                             LogForm,
                             YNACBox,
                             PathSearchForm,
                             m_sbcb);

    // request safety device for children password
    with _globals.GetCfg.GetSection(MF_CFG_ID) do begin
      sPasswMust:=GetStringOption(CFGID_PASSWORD);
      if (sPasswMust <> '') then begin
        sPasswIs:='';
        if (not SDFCForm.Query(SDFC_REQUEST, sPasswIs)) then begin
          Close;
          Application.Terminate;
          Exit;
        end
        else
          if (sPasswMust <> sPasswIs) then begin
            Close;
            Application.Terminate;
            Exit;
          end;
      end;
    end;

    // low let's see if we have a job to do
    if (ProcessObjects(Nil)) then begin
      // (shutdown at this point should be now problem, no creation code is
      //  following afterwards)
      Close;
      Application.Terminate;
      Exit;
    end;

    // now set the work file favorites
    RefreshWorkFileFavorites;

    // start the file browser
    LaunchFileBrowser;

    // check the selected langauge
    case _globals.GetLanguage of
      LANGUAGE_DE : MN_Language_DE.Checked:=True;
      LANGUAGE_US : MN_Language_US.Checked:=True;
    end;

    // show startup tips, if necessary
    ShowStartupTips;

    // dragging now allowed
    Browser.DragMode:=dmAutomatic;

    // accept dragged objects now
    DragAcceptFiles(Handle, TRUE);

    // focus to the browser
    Browser.SetFocus;

  end;
end;

procedure TMainForm.MN_Tools_ClearDiskSpaceClick(Sender: TObject);
begin
  if (IsGUILocked) then Exit;

  // the disk clear dialog does all the work for us
  DiskClearForm.Launch(Self);
end;

procedure TMainForm.MN_Language_DEClick(Sender: TObject);
begin
  if (IsGUILocked) then Exit;

  if (not MN_Language_DE.Checked) then begin
    _globals.ChangeStringResources(LANGUAGE_DE);
    MN_Language_US.Checked:=False;
    MN_Language_DE.Checked:=True;
    MN_Language_XT.Checked:=False;
    MN_Browser_RefreshClick(Sender);
    _globals.GetFileBrowser.MakeColumns;
  end;
end;

procedure TMainForm.MN_Language_USClick(Sender: TObject);
begin
  if (IsGUILocked) then Exit;

  if (not MN_Language_US.Checked) then begin
    _globals.ChangeStringResources(LANGUAGE_US);
    MN_Language_US.Checked:=True;
    MN_Language_DE.Checked:=False;
    MN_Language_XT.Checked:=False;
    MN_Browser_RefreshClick(Sender);
    _globals.GetFileBrowser.MakeColumns;
  end;
end;

procedure TMainForm.MN_Language_XTClick(Sender: TObject);
begin
  if (IsGUILocked) then Exit;

  if (not MN_Language_XT.Checked) then begin
    _globals.ChangeStringResources(LANGUAGE_XT);
    MN_Language_US.Checked:=False;
    MN_Language_DE.Checked:=False;
    MN_Language_XT.Checked:=True;
    MN_Browser_RefreshClick(Sender);
    _globals.GetFileBrowser.MakeColumns;
  end;
end;

procedure TMainForm.MN_File_ViewClick(Sender: TObject);
begin
  if (IsGUILocked) then Exit;

  // single file selected?
  if (not m_exec.CheckSelection(True)) then begin
    Beep;
    Exit;
  end;

  // view the selected file
  PrepareAction;
  m_exec.GUIView;
  FinalizeAction;
end;

procedure TMainForm.ShowStartupTips;
begin
  if (_globals.GetLangID = LANGUAGE_US) then
    TipForm.ShowTip(TIP_LANGSWITCHDE)
  else
    TipForm.ShowTip(TIP_LANGSWITCHUS);

end;

procedure TMainForm.MN_Tools_SDFCClick(Sender: TObject);
var
  sPassword : String;
begin
  // show a tip
  TipForm.Showtip(TIP_SDFC);

  // request a new password
  with _globals.GetCfg.GetSection(MF_CFG_ID) do begin

    sPassword:=GetStringOption(CFGID_PASSWORD);
    if (not SDFCForm.Query(SDFC_CHANGE,
                           sPassword)) then
      Exit
    else
      FixStringOption(CFGID_PASSWORD, sPassword);
  end;
end;

procedure TMainForm.StoreCurrentPath;
var
  sPath : String;
begin
  with _globals.GetFileBrowser, _globals.GetOpts.GetCfg do begin

    // pure network?
    sPath:=GetCurrentPath;
    if (sPath = '') then begin

      // try to get the last drive path
      sPath:=GetLastValidPath;
      if (sPath = '') then
        sPath:=GetCurrentDir;   // (better than nothing)

    end;

    FixStringOption(OPTIONS_CFGID_LASTWORKPATH, sPath);

  end;
end;


procedure TMainForm.HelpBtnClick(Sender: TObject);
begin
  MN_Help_HelpClick(Sender);
end;

procedure TMainForm.MN_Help_HelpClick(Sender: TObject);
begin

  HtmlHelpShowContents;
end;

procedure TMainForm.BrowserSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  // don't do anything if a mass selection or the shutdown is running
  if (not m_blDisableSelect) then begin

    // map the message
    _globals.GetFileBrowser.HandleSelection(Item, Selected);

    // show the new browser status
    ShowBrowserStats(False);

    // (de)select menu items
    SwitchMenuItems;
  end;

end;


procedure TMainForm.TaskBarIconState(blShow : Boolean);
var
  iconData : TNotifyIconData;
begin

  if (m_blTrayIconVisible = blShow) then
    Exit;

  FillChar(iconData, SizeOf(iconData), 0);
  iconData.cbSize:=SizeOf(iconData);
  iconData.Wnd:=Handle;
  iconData.uID:=3004;

  if (blShow) then begin
    iconData.uFlags:=NIF_MESSAGE or NIF_ICON or NIF_TIP;
    iconData.uCallbackMessage:=TASKBAR_MESSAGE;
    iconData.hIcon:=Application.Icon.Handle;
    lstrcpyn(iconData.szTip, PChar(PROGRAM_NAME), Length(PROGRAM_NAME) + 1);

    Shell_NotifyIcon(NIM_ADD, @iconData);
  end
  else begin

    Shell_NotifyIcon(NIM_DELETE, @iconData);
  end;

  m_blTrayIconVisible:=blShow;

end;


procedure TMainForm.FormDestroy(Sender: TObject);
begin
  // removing the hook to avoid leaks
  Application.UnHookMainWindow(HookProc);

  // remove any possible tray icon
  TaskBarIconState(False);
end;


procedure TMainForm.TrayIconHandler(var msg : TMessage);
var
  pos : TPoint;
begin
  case msg.LParam of

    // just show our app again?
    WM_LBUTTONDOWN : begin
      if (not Visible) then
        Show;
      Application.Restore;
      Application.BringToFront;
    end;

    // or the cool tray menu?
    WM_RBUTTONDOWN : begin
      GetCursorPos(pos);
      TrayMenu.Popup(pos.x, pos.y);
    end;

    // ready to go
    WM_QUERYENDSESSION : begin
      Msg.Result:=1;
    end;

    // Windows goes down
    WM_ENDSESSION : begin
      TaskBarIconState(False);
    end;

  end;

end;


procedure TMainForm.TM_ShowClick(Sender: TObject);
begin
  if (not Visible) then
    Show;
  Application.Restore;
  Application.BringToFront;
end;

procedure TMainForm.TM_ExitClick(Sender: TObject);
begin
  TaskBarIconState(False);
  Close;
end;


procedure TMainForm.LockTrayMenu(blLock : Boolean);
begin
//  TM_Show.Enabled:=not blLock;
  TM_Exit.Enabled:=not blLock;
end;



procedure TMainForm.AppMinimize(Sender: TObject);
begin
  // better to show an info
  if (m_blTrayIconVisible) then begin

    TipForm.ShowTip(TIP_TRAYICON);
  
    ShowWindow(Application.Handle, SW_HIDE);
//    Hide;
  end;
end;

procedure TMainForm.AppRestore(Sender: TObject);
begin
  if (m_blTrayIconVisible) then
    ShowWindow(Application.Handle, SW_RESTORE);
//    Show;
end;


function TMainForm.HookProc(var msg : TMessage) : Boolean;
var
  ca : TCloseAction;
begin
  Result := False;

  // no comment...
  if (msg.Msg = WM_ENDSESSION) then begin
     ca:=caFree;
     FormClose(Nil, ca);
     Result:=True;
  end;
end;


procedure TMainForm.SetTrayIconCaption(const sCaption : String);
var
  iconData : TNotifyIconData;
begin

  if (not m_blTrayIconVisible) then
    Exit;

  FillChar(iconData, SizeOf(iconData), 0);
  iconData.cbSize:=SizeOf(iconData);
  iconData.Wnd:=Handle;
  iconData.uID:=3004;
  iconData.uCallbackMessage:=TASKBAR_MESSAGE;
  iconData.hIcon:=Application.Icon.Handle;
  lstrcpyn(iconData.szTip, PChar(sCaption), Length(sCaption) + 1);

  if (sCaption = '') then
    iconData.uFlags:=(NIF_MESSAGE or NIF_ICON) and (not NIF_TIP)
  else
    iconData.uFlags:=NIF_MESSAGE or NIF_ICON or NIF_TIP;

  Shell_NotifyIcon(NIM_MODIFY, @iconData);
end;



procedure TMainForm.BT_HistBackBtnClick(Sender: TObject);
begin
  if (IsGUILocked) then
    Exit;
  BrowserReload(BRL_CHANGEPATH,
                _globals.GetFileBrowser.GetHistory.GoBackwards,
                False,
                False);
  UpdateHistoryCtrls;
end;


procedure TMainForm.UpdateHistoryCtrls;
begin
  with _globals.GetFileBrowser.GetHistory do begin
    BT_HistBackBtn.Enabled:=CanGoBackwards;
    BT_HistForwardBtn.Enabled:=CanGoForwards;
  end;
end;


procedure TMainForm.BT_HistForwardBtnClick(Sender: TObject);
begin
  if (IsGUILocked) then
    Exit;
  BrowserReload(BRL_CHANGEPATH,
                _globals.GetFileBrowser.GetHistory.GoForwards,
                False,
                False);
  UpdateHistoryCtrls;
end;

procedure TMainForm.PM_EncryptClick(Sender: TObject);
begin
  MN_File_EncryptClick(Sender);
end;

procedure TMainForm.PM_DecryptClick(Sender: TObject);
begin
  MN_File_DecryptClick(Sender);
end;

procedure TMainForm.PM_WipeClick(Sender: TObject);
begin
  MN_File_WipeClick(Sender);
end;

procedure TMainForm.PM_WorkWithClick(Sender: TObject);
begin
  MN_File_WorkWithClick(Sender);
end;

function TMainForm.FormHelp(Command: Word; Data: Integer;
  var CallHelp: Boolean): Boolean;
begin
  CallHelp := False;
  Result:=HtmlHelp_ShowContext(Data);
end;

procedure TMainForm.MN_Help_WebClick(Sender: TObject);
begin
  // would be nice support a special URL, so the usage of this menu item could
  // actually be determined (and distinguished from other links)

  if (32 >= ShellAPI.ShellExecute(
    THandle(0),
    'open',
    'http://maakus.dyndns.org',
    Nil,
    '',
    SW_SHOW)) then begin
    
    Application.MessageBox(
      PChar(_globals.GetSr.Get(MF_CFG_ID, 'NO_WWW')),
      PChar(Caption),
      MB_ICONERROR);
  end;
end;

procedure TMainForm.PM_FormatClick(Sender: TObject);
begin
  _globals.GetFileBrowser.FormatDrive; 
end;

end.
