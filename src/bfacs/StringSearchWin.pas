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
  dialog for entering a search string
}


unit StringSearchWin;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls,
  Configuration, ExtCtrls;


type
  TStringSearchForm = class(TForm)
    InputBox: TEdit;
    InputInfo: TLabel;
    OKBtn: TButton;
    CancelBtn: TButton;
    CaseSensSwitch: TCheckBox;
    procedure InputBoxKeyPress(Sender: TObject; var Key: Char);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormShow(Sender: TObject);
    function FormHelp(Command: Word; Data: Integer;
      var CallHelp: Boolean): Boolean;
  private
    m_config : TConfigurationSection;
  public
    procedure SetCaption(const sCaption : String);
    function GetSearchText : String;
    function GetCaseSensitive : Boolean;
  end;

var
  StringSearchForm: TStringSearchForm;

implementation
uses
  HtmlHelpAPI,
  StringRes,
  Globals,
  ShortcutChecker,
  FileSupp;

{$R *.DFM}


//////////////////////////// TStringSearchForm ////////////////////////////


// the configuration ID
const
  CONFIG_ID = 'STRINGSEARCHFORM';

// config. IDs
const
  CFGID_LEFT     = 'LEFT';
  CFGID_TOP      = 'TOP';
  CFGID_ITEM     = 'ITEM';
  CFGID_CASESENS = 'CASESENS';

// wrapper char to store the last search item (remember that strings will
// be trimmed in the configuartion)
const
  ITEM_WRAP_CHAR = '"';


// resource listener
type
  TStringSearchFormStrResListener = class(TStrResListener)
  private
    m_theWin : TStringSearchForm;
  public
    constructor Create(theWin : TStringSearchForm);
    procedure ChangeStrings(sr : TStrRes); override;
  end;

constructor TStringSearchFormStrResListener.Create(theWin : TStringSearchForm);
begin
  m_theWin:=theWin;
end;


procedure TStringSearchFormStrResListener.ChangeStrings(sr : TStrRes);
var
  scc : TShortcutChecker;
begin
  scc:=TShortcutChecker.Create;
  with m_theWin, sr, scc do begin
    OKBtn.Caption         :=AddShortcut(Get(CONFIG_ID, 'OKBTN'));
    CancelBtn.Caption     :=AddShortcut(Get(CONFIG_ID, 'CANCELBTN'));
    CaseSensSwitch.Caption:=AddShortcut(Get(CONFIG_ID, 'CASESENS'));
    InputInfo.Caption     :=AddShortcut(Get(CONFIG_ID, 'INFO'));
  end;
  scc.Destroy;
end;


// configuration checker
type
  TStringSearchFormCC = class(TConfigurationChecker)
  private
    m_parent : TStringSearchForm;
  public
    constructor Create(parent : TStringSearchForm);
    procedure RunCheck(section : TConfigurationSection); override;
  end;

constructor TStringSearchFormCC.Create(parent : TStringSearchForm);
begin
  m_parent:=parent;
end;

procedure TStringSearchFormCC.RunCheck(section : TConfigurationSection);
var
  sTemp : String;
begin
  CheckInt(section, CFGID_LEFT, (Screen.Width - m_parent.Width) shr 1);
  CheckInt(section, CFGID_TOP, (Screen.Height - m_parent.Height) shr 1);
  CheckString(section, CFGID_ITEM, '""');
  sTemp:=section.GetStringOption(CFGID_ITEM);
  if (sTemp[1] <> ITEM_WRAP_CHAR) then
    sTemp:=ITEM_WRAP_CHAR + sTemp;
  if (sTemp[Length(sTemp)] <> ITEM_WRAP_CHAR) then
    sTemp:=sTemp + ITEM_WRAP_CHAR;
  section.FixStringOption(CFGID_ITEM, sTemp);
  CheckBool(section, CFGID_CASESENS, False);
end;



procedure TStringSearchForm.FormCreate(Sender: TObject);
var
  sTemp    : String;
  cc       : TStringSearchFormCC;
  listener : TStringSearchFormStrResListener;
begin
  // get the configuration section
  cc:=TStringSearchFormCC.Create(Self);
  m_config:=_globals.GetCfg.GetSection(CONFIG_ID, cc);
  cc.Destroy;

  // set up the window's position
  Left:=m_config.GetIntegerOption(CFGID_LEFT);
  Top:=m_config.GetIntegerOption(CFGID_TOP);

  // load the old stuff
  sTemp:=m_config.GetStringOption(CFGID_ITEM);
  InputBox.Text:=Copy(sTemp, 2, Length(sTemp) - 2);
  CaseSensSwitch.Checked:=m_config.GetBooleanOption(CFGID_CASESENS);

  // init. listener stuff
  listener:=TStringSearchFormStrResListener.Create(Self);
  _globals.GetSr.AddListener(listener);
  listener.ChangeStrings(_globals.GetSr);

  // set empty caption
  Caption:='';
end;


procedure TStringSearchForm.InputBoxKeyPress(Sender: TObject; var Key: Char);
begin
  // enter key finishes
  if (Key = #13) then begin
    ModalResult:=mrOk;
    Close;
    Key:=#0;
  end;
end;


procedure TStringSearchForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  m_config.FixIntegerOption('LEFT', Left);
  m_config.FixIntegerOption('TOP', Top);

  // store text and switch state on [OK]
  if (ModalResult = mrOk) then begin
    m_config.FixStringOption(CFGID_ITEM,
                             ITEM_WRAP_CHAR + InputBox.Text + ITEM_WRAP_CHAR);
    m_config.FixBooleanOption(CFGID_CASESENS, CaseSensSwitch.Checked);
  end;
end;


procedure TStringSearchForm.SetCaption(const sCaption : String);
begin
  Caption:=sCaption;
end;

function TStringSearchForm.GetSearchText : String;
begin
  Result:=InputBox.Text;
end;

function TStringSearchForm.GetCaseSensitive : Boolean;
begin
  Result:=CaseSensSwitch.Checked;
end;


procedure TStringSearchForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
  // [ESC] cancels
  if (Key = #27) then begin
    ModalResult:=mrCancel;
    Close;
    Key:=#0;
  end;
end;


procedure TStringSearchForm.FormShow(Sender: TObject);
begin
  InputBox.SetFocus;
end;

function TStringSearchForm.FormHelp(Command: Word; Data: Integer;
  var CallHelp: Boolean): Boolean;
begin
  CallHelp := False;
  Result:=HtmlHelp_ShowContext(Data);
end;

end.
