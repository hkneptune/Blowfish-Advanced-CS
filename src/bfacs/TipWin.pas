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
  common tip dialog with useful hints for the usual crowd
}

unit TipWin;

interface
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls,
  Configuration,
  StringRes;

// tip codes
const
  TIP_LANGSWITCHUS    = 0;
  TIP_LANGSWITCHDE    = 1;
  TIP_SHELLEXTENSION  = 2;
  TIP_SCANNER         = 3;
  TIP_SDFC            = 4;
  TIP_TRAYICON        = 5;
  TIP_CAPSLOCKWARNING = 6;

type
  TTipForm = class(TForm)
    LampPic: TImage;
    InfoBox: TLabel;
    NotAgainSwitch: TCheckBox;
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    function FormHelp(Command: Word; Data: Integer;
      var CallHelp: Boolean): Boolean;
  private
    m_config : TConfigurationSection;
    m_sr     : TStrRes;
  public

    // shows a tip (will do nothing if the tip was already deativated)
    // -> tip number (see TIP_xxx)
    procedure ShowTip(nCode : Integer);

  end;

var
  TipForm: TTipForm;

implementation
uses
  Options,
  HtmlHelpAPI,
  ShortcutChecker,
  Globals;

{$R *.DFM}



//////////////////////////// TTipForm ////////////////////////////


// the configuration ID
const
  CONFIG_ID = 'TIPFORM';

// string res. perfix for tips
const
  TIP_PREFIX = 'TIP';


// resource listener
type
  TTipFormStrResListener = class(TStrResListener)
  private
    m_theWin : TTipForm;
  public
    constructor Create(theWin : TTipForm);
    procedure ChangeStrings(sr : TStrRes); override;
  end;

constructor TTipFormStrResListener.Create(theWin : TTipForm);
begin
  m_theWin:=theWin;
end;


procedure TTipFormStrResListener.ChangeStrings(sr : TStrRes);
var
  scc : TShortcutChecker;
begin
  scc:=TShortcutChecker.Create;
  with m_theWin, sr, scc do begin
    Caption:=Get(CONFIG_ID, 'CAPTION');
    NotAgainSwitch.Caption:=AddShortcut(Get(CONFIG_ID, 'NOTAGAINSWITCH'));
  end;
  scc.Destroy;
end;


// configuration checker
type
  TTipFormCC = class(TConfigurationChecker)
  private
    m_parent : TTipForm;
  public
    constructor Create(parent : TTipForm);
    procedure RunCheck(section : TConfigurationSection); override;
  end;

constructor TTipFormCC.Create(parent : TTipForm);
begin
  m_parent:=parent;
end;

procedure TTipFormCC.RunCheck(section : TConfigurationSection);
begin
  CheckInt(section, 'LEFT', (Screen.Width - m_parent.Width) shr 1, 0);
  CheckInt(section, 'TOP', (Screen.Height - m_parent.Height) shr 1, 0);
  CheckInt(section, 'FLAGS', -1);
end;


procedure TTipForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = #27) then begin
    Close;
    Key:=#0;
  end;
end;

procedure TTipForm.FormCreate(Sender: TObject);
var
  cc       : TTipFormCC;
  listener : TTipFormStrResListener;
begin

  // get the configuration section
  cc:=TTipFormCC.Create(Self);
  m_config:=_globals.GetCfg.GetSection(CONFIG_ID, cc);
  cc.Destroy;

  // set up the window's position
  Left:=m_config.GetIntegerOption('LEFT');
  Top:=m_config.GetIntegerOption('TOP');

  // init. listener stuff
  m_sr:=_globals.GetSr;
  listener:=TTipFormStrResListener.Create(Self);
  m_sr.AddListener(listener);
  listener.ChangeStrings(m_sr);

  // background color's yellow "lite"
  Color:=TColor($00e0ffff);

end;

procedure TTipForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  m_config.FixIntegerOption('LEFT', Left);
  m_config.FixIntegerOption('TOP', Top);
end;


procedure TTipForm.ShowTip(nCode : Integer);
var
  nFlags : Integer;
  nBit   : Integer;

begin
  // legal tip code?
  if ((nCode < 0) or (nCode > 31)) then
    Exit;

  // must we show that tip?
  nFlags:=m_config.GetIntegerOption('FLAGS');
  nBit:=1 shl nCode;

  if ((nFlags and nBit) <> 0) then begin

    // load the tip
    InfoBox.Caption:=m_sr.Get(CONFIG_ID, TIP_PREFIX + IntToStr(nCode));

    // show the dialog in exclusive mode
    ShowModal;

    // do not show this tip the next time?
    if (NotAgainSwitch.Checked) then
      m_config.FixIntegerOption('FLAGS', (nFlags and (not nBit)));

  end;

end;


procedure TTipForm.FormShow(Sender: TObject);
var
  nGap : Integer;
begin

  // adjust the controls
  NotAgainSwitch.Checked:=False;
  nGap:=InfoBox.Top;
  NotAgainSwitch.Top:=InfoBox.Top + InfoBox.Height + nGap;
  ClientHeight:=NotAgainSwitch.Top + NotAgainSwitch.Height + nGap;
end;

function TTipForm.FormHelp(Command: Word; Data: Integer;
  var CallHelp: Boolean): Boolean;
begin
  CallHelp := False;
  Result:=HtmlHelp_ShowContext(Data);
end;

end.
