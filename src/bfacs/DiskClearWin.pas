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
  the interface for our disk clearer
}

unit DiskClearWin;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls,
  Main,
  StringRes,
  MessageCallBack,
  Configuration;


type
  TDiskClearForm = class(TForm)
    StartBtn: TButton;
    CancelBtn: TButton;
    SetupGroup: TGroupBox;
    LowPrioSwitch: TCheckBox;
    SilentSwitch: TCheckBox;
    NoBufferSwitch: TCheckBox;
    StatusBar: TStatusBar;
    MethodSelect: TComboBox;
    HowToSelect: TRadioGroup;
    MethodInfo: TLabel;
    ProgressBar: TProgressBar;
    PercentView: TStaticText;
    HelpBtn: TButton;
    MinimizeBtn: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure CancelBtnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure StartBtnClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    function FormHelp(Command: Word; Data: Integer;
      var CallHelp: Boolean): Boolean;
    procedure MinimizeBtnClick(Sender: TObject);
    procedure Launch(parent : TMainForm);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  protected
    m_config     : TConfigurationSection;
    m_sr         : TStrRes;
    m_blRunning  : Boolean;
    m_blWasBreak : Boolean;
    m_msgCB      : TMessageCallBack;
    m_parent     : TMainForm;
    procedure ShowProgressCtrls(blShow : Boolean);
    procedure StoreOptions;
    procedure ReloadOptions;
    procedure Working(blState : Boolean);
    procedure Exec;
    function RequestClose : Boolean;
  end;

var
  DiskClearForm: TDiskClearForm;

implementation
uses
  GlobalsGUI,
  General,
  HtmlHelpAPI,
  Options,
  bfacslib,
  DiskClear,
  GUIMessageCBImpl,
  ProgressCallBack,
  StringPlusI,
  ShortcutChecker,
  CallBack,
  MessBoxYNAC;


{$R *.DFM}


//////////////////////////// TDiskClearForm ////////////////////////////


// the configuration ID
const
  CONFIG_ID = 'DISKCLEARFORM';


// config IDs
const
  CFGID_LEFT     = 'LEFT';
  CFGID_TOP      = 'TOP';
  CFGID_NOBUFFER = 'NOBUFFER';
  CFGID_SILENT   = 'SILENT';
  CFGID_LOWPRIO  = 'LOWPRIO';
  CFGID_METHOD   = 'METHOD';
  CFGID_HOWTO    = 'HOWTO';

  

// resource listener
type
  TDiskClearFormStrResListener = class(TStrResListener)
  private
    m_theWin : TDiskClearForm;
  public
    constructor Create(theWin : TDiskClearForm);
    procedure ChangeStrings(sr : TStrRes); override;
  end;

constructor TDiskClearFormStrResListener.Create(theWin : TDiskClearForm);
begin
  m_theWin:=theWin;
end;


procedure TDiskClearFormStrResListener.ChangeStrings(sr : TStrRes);
var
  scc : TShortcutChecker;
begin
  scc:=TShortcutChecker.Create;
  with m_theWin, sr, scc do begin
    Caption:=Get(CONFIG_ID, 'CAPTION');

    StartBtn.Caption:=AddShortcut(Get(CONFIG_ID, 'STARTBTN'));
    CancelBtn.Caption:=AddShortcut(Get(CONFIG_ID, 'CANCELBTN'));
    HelpBtn.Caption:=AddShortcut(Get(CONFIG_ID, 'HELPBTN'));
    MinimizeBtn.Caption:=AddShortcut(Get(CONFIG_ID, 'MINIMIZEBTN'));

    SetupGroup.Caption:=Get(CONFIG_ID, 'SETUPGROUP');

    HowToSelect.Caption:=Get(CONFIG_ID, 'HOWTOSELECT');
    HowToSelect.Items[0]:=AddShortcut(Get(CONFIG_ID, 'HTS_CURRDRV'));
    HowToSelect.Items[1]:=AddShortcut(Get(CONFIG_ID, 'HTS_ALLDRIVES'));
    HowToSelect.Items[2]:=AddShortcut(Get(CONFIG_ID, 'HTS_ALLFIXEDDRIVES'));

    NoBufferSwitch.Caption:=AddShortcut(Get(CONFIG_ID, 'NOBUFFERSWITCH'));
    SilentSwitch.Caption:=AddShortcut(Get(CONFIG_ID, 'SILENTSWITCH'));
    LowPrioSwitch.Caption:=AddShortcut(Get(CONFIG_ID, 'LOWPRIOSWITCH'));

    MethodInfo.Caption:=AddShortcut(Get(CONFIG_ID, 'METHODINFO'));
    with MethodSelect do begin
      Items[0]:=Get(CONFIG_ID, 'MSEL_ZEROS');
      Items[1]:=Get(CONFIG_ID, 'MSEL_RNDBLOCK');
      Items[2]:=Get(CONFIG_ID, 'MSEL_RNDSTREAM');
    end;
  end;
  scc.Destroy;
end;


// configuration checker
type
  TDiskClearFormCC = class(TConfigurationChecker)
  private
    m_parent : TDiskClearForm;
  public
    constructor Create(parent : TDiskClearForm);
    procedure RunCheck(section : TConfigurationSection); override;
  end;

constructor TDiskClearFormCC.Create(parent : TDiskClearForm);
begin
  m_parent:=parent;
end;

procedure TDiskClearFormCC.RunCheck(section : TConfigurationSection);
begin
  // check position
  CheckInt(section, CFGID_LEFT, (Screen.Width - m_parent.Width) shr 1, 0);
  CheckInt(section, CFGID_TOP, (Screen.Height - m_parent.Height) shr 1, 0);
  // check setup
  CheckBool(section, CFGID_NOBUFFER, True);
  CheckBool(section, CFGID_SILENT, False);
  CheckBool(section, CFGID_LOWPRIO, True);
  CheckInt(section, CFGID_HOWTO, 0, 0, 2);
  CheckInt(section, CFGID_METHOD, 1, 0, 2);
end;



procedure TDiskClearForm.FormCreate(Sender: TObject);
var
  cc       : TDiskClearFormCC;
  listener : TDiskClearFormStrResListener;
begin
  // get the configuration section
  cc:=TDiskClearFormCC.Create(Self);
  m_config:=_globals.GetCfg.GetSection(CONFIG_ID, cc);
  cc.Destroy;

  // set up the window's position
  Left:=m_config.GetIntegerOption(CFGID_LEFT);
  Top:=m_config.GetIntegerOption(CFGID_TOP);

  // set up the how-to and the method selector
  with HowToSelect.Items do begin Clear; Add(''); Add(''); Add(''); end;
  with MethodSelect.Items do begin Clear; Add(''); Add(''); Add(''); end;

  // init. listener stuff
  m_sr:=_globals.GetSr;
  listener:=TDiskClearFormStrResListener.Create(Self);
  m_sr.AddListener(listener);
  listener.ChangeStrings(m_sr);

  // create our message callback
  m_msgCB:=TGUIMessageCBImpl.Create(YNACBox);
end;

procedure TDiskClearForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  m_config.FixIntegerOption(CFGID_LEFT, Left);
  m_config.FixIntegerOption(CFGID_TOP, Top);

  with m_parent do begin
    EnableBrowserTasks(True);
    LockTrayMenu(False);
    SetAction('');
    Enabled:=True;
  end;
end;

procedure TDiskClearForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = #27) then begin
    Close;
    Key:=#0;
  end;
end;

procedure TDiskClearForm.CancelBtnClick(Sender: TObject);
begin
  if RequestClose then Close;
end;

procedure TDiskClearForm.FormShow(Sender: TObject);
begin
  m_blRunning:=False;
  m_blWasBreak:=False;
  ShowProgressCtrls(False);
  StatusBar.Panels[0].Text:='';
  StatusBar.Panels[1].Text:='';
  ProgressBar.Position:=0;
  ProgressBar.Max:=0;
  MinimizeBtn.Enabled:=False;
  ReloadOptions;
end;

procedure TDiskClearForm.ShowProgressCtrls(blShow : Boolean);
begin
  PercentView.Visible:=blShow;
end;

procedure TDiskClearForm.StoreOptions;
begin
  with m_config do begin
    FixBooleanOption(CFGID_NOBUFFER, NoBufferSwitch.Checked);
    FixBooleanOption(CFGID_SILENT, SilentSwitch.Checked);
    FixBooleanOption(CFGID_LOWPRIO, LowPrioSwitch.Checked);
    FixIntegerOption(CFGID_HOWTO, HowToSelect.ItemIndex);
    FixIntegerOption(CFGID_METHOD, MethodSelect.ItemIndex);
  end;
end;

procedure TDiskClearForm.ReloadOptions;
begin
  with m_config do begin
    NoBufferSwitch.Checked:=GetBooleanOption(CFGID_NOBUFFER);
    SilentSwitch.Checked:=GetBooleanOption(CFGID_SILENT);
    LowPrioSwitch.Checked:=GetBooleanOption(CFGID_LOWPRIO);
    HowToSelect.ItemIndex:=GetIntegerOption(CFGID_HOWTO);
    MethodSelect.ItemIndex:=GetIntegerOption(CFGID_METHOD);
  end;

end;

procedure TDiskClearForm.Working(blState : Boolean);
begin
  StartBtn.Enabled:=not blState;
  SetupGroup.Enabled:=not blState;
  MinimizeBtn.Enabled:=blState;
  m_blRunning:=blState;
end;


// the progress callback
type
  TProgressCB = class(TProgressCallBack)
  public
    procedure CallBack; override;
  end;


procedure TProgressCB.CallBack;
var
  nAct, nMax     : Integer;
  blShowProgress : Boolean;
  sPercent       : String;
  sDrive         : String;
begin
  with GetCallBackObj as TDiskClearForm do begin

    Application.ProcessMessages;

    blShowProgress:=(GetMaxPos <> WORD64(-1));

    // reset?
    if (GetChanged) then begin
      ShowProgressCtrls(blShowProgress);
      if (blShowProgress) then begin
        GetIntLevels(nAct, nMax);
        ProgressBar.Max:=nMax;
      end;
    end;

    if (blShowProgress) then begin
      GetIntLevels(nAct, nMax);
      ProgressBar.Position:=nAct;
      sPercent:=TStrPlusI.CalcPercent(m_sr, GetActPos, GetMaxPos, 1) + '%';
      if (sPercent <> PercentView.Caption) then // (avoid flicker)
        PercentView.Caption:=sPercent;
    end;

    sDrive:=GetMessage;
    if (Copy(sDrive, 2, 1) = ':') then
      StatusBar.Panels[0].Text:=m_sr.Get(CONFIG_ID, 'DRIVEPREFIX') +
                                Copy(sDrive, 1, 2)
    else
      StatusBar.Panels[0].Text:=sDrive;

    if (blShowProgress) then
      StatusBar.Panels[1].Text:=Format(m_sr.Get(CONFIG_ID, 'SHOWPROG0'),
                                       [TStrPlusI.Sepa1000(m_sr, GetActPos),
                                        TStrPlusI.Sepa1000(m_sr, GetMaxPos)])
    else
      StatusBar.Panels[1].Text:=Format(m_sr.Get(CONFIG_ID, 'SHOWPROG1'),
                                       [TStrPlusI.Sepa1000(m_sr, GetActPos)]);
    // break detected?
    if (m_blWasBreak) then begin
      m_blWasBreak:=False;
      raise ECallBackInterrupt.Create('user break');
    end;

    UpdateWindow(Handle);

  end;
end;


procedure TDiskClearForm.StartBtnClick(Sender: TObject);
begin
  StoreOptions;
  m_parent.SetAction(m_sr.Get(CONFIG_ID, 'ACTION_CLEAR'));
  Exec;
  Close;
end;


procedure TDiskClearForm.Exec;
var
  nMode     : Integer;
  sCurrPath : String;
  prgCB     : TProgressCB;
  dclear    : TDiskClear;
  msgCB     : TMessageCallBack;
begin

  Working(True);
  prgCB:=TProgressCB.Create(Self);

  case m_config.GetIntegerOption(CFGID_METHOD) of
    0 : nMode:=DISKCLEAR_MODE_ZEROS;
    1 : nMode:=DISKCLEAR_MODE_RANDOMBLOCK;
    2 : nMode:=DISKCLEAR_MODE_RANDOMSTREAM;
  else
    nMode:=-1; // just to please the compiler
    RunError(RUNERROR_DISKCLEARWIN_MODEERROR);
  end;

  dclear:=TDiskClear.Create(_globals.GetRndMng, m_sr);

  case m_config.GetIntegerOption(CFGID_HOWTO) of

    0 : begin

      if (m_config.GetBooleanOption(CFGID_SILENT)) then
        msgCB:=Nil
      else
        msgCB:=m_msgCB;

      sCurrPath:=_globals.GetFileBrowser.GetCurrentPath(False);
      try
        dclear.Execute(sCurrPath,
                       nMode,
                       prgCB,
                       msgCB,
                       m_config.GetBooleanOption(CFGID_NOBUFFER),
                       m_config.GetBooleanOption(CFGID_LOWPRIO));
      except
        on EDiskClearBreak do begin
          // break accepted
        end;
        on edce : EDiskClearError do begin
          with m_msgCB do begin
            SetStyle(MCB_STYLE_OK);
            SetKindOf(MCB_KINDOF_ERROR);
            SetMessage(Format(m_sr.Get(CONFIG_ID, 'ERRORMESS'),
                              [sCurrPath, edce.Message]));
            CallBack;
            if (GetResult = MCB_RES_NO) then
              Exit
          end;
        end;
      end;

    end;

    1 : begin
      dclear.DoAllDrives(nMode,
                         False,
                         prgCB,
                         m_msgCB,
                         m_config.GetBooleanOption(CFGID_SILENT),
                         m_config.GetBooleanOption(CFGID_NOBUFFER),
                         m_config.GetBooleanOption(CFGID_LOWPRIO));
    end;

    2 : begin
      dclear.DoAllDrives(nMode,
                         True,
                         prgCB,
                         m_msgCB,
                         m_config.GetBooleanOption(CFGID_SILENT),
                         m_config.GetBooleanOption(CFGID_NOBUFFER),
                         m_config.GetBooleanOption(CFGID_LOWPRIO));
    end;

  else
    RunError(RUNERROR_DISKCLEARWIN_HOWTOHUH);
  end;

  dclear.Destroy;
  prgCB.Destroy;
  Working(False);
end;


procedure TDiskClearForm.FormDestroy(Sender: TObject);
begin
  m_msgCB.Destroy;
end;


procedure TDiskClearForm.HelpBtnClick(Sender: TObject);
begin
  Application.HelpCommand(HELP_CONTEXT, 504);
end;


function TDiskClearForm.FormHelp(Command: Word; Data: Integer;
  var CallHelp: Boolean): Boolean;
begin
  CallHelp := False;
  Result:=HtmlHelp_ShowContext(Data);
end;

procedure TDiskClearForm.MinimizeBtnClick(Sender: TObject);
begin
  Application.Minimize;
end;

procedure TDiskClearForm.Launch(parent : TMainForm);
begin
  m_parent:=parent;

  with m_parent do begin
    EnableBrowserTasks(False);
    LockTrayMenu(True);
    Enabled:=False;
  end;

  Show;
end;

procedure TDiskClearForm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  CanClose:=RequestClose;
end;

function TDiskClearForm.RequestClose : Boolean;
begin
  Result:=not m_blRunning;

  if (m_blRunning) then begin
    with m_msgCB do begin
      SetStyle(MCB_STYLE_YESNO);
      SetKindOf(MCB_KINDOF_QUESTION);
      SetMessage(m_sr.Get(CONFIG_ID, 'REALLYBREAK'));
      CallBack;
      m_blWasBreak:=(GetResult = MCB_RES_YES);
    end;
  end;
end;
    
end.



