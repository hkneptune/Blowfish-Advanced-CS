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
  input dialog for the "safety device for children" password
}

unit SDFCWin;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls,
  Configuration;

const
  SDFC_CHANGE  = 0;
  SDFC_REQUEST = 1;


type
  TSDFCForm = class(TForm)
    PasswBox: TEdit;
    PasswInfo: TLabel;
    OKBtn: TButton;
    HelpBtn: TButton;
    WinPoo: TImage;
    CancelBtn: TButton;
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    function FormHelp(Command: Word; Data: Integer;
      var CallHelp: Boolean): Boolean;
  private
    m_config : TConfigurationSection;
  public
    function Query(nMode : Integer;
                   var vsPassword : String) : Boolean;
  end;

var
  SDFCForm: TSDFCForm;

implementation
uses
  GlobalsGUI,
  Options,
  HtmlHelpAPI,
  ShortcutChecker,
  StringRes;

{$R *.DFM}


//////////////////////////// TSDFCForm ////////////////////////////


// the configuration ID
const
  CONFIG_ID = 'SDFCFORM';


// configuration checker
type
  TSDFCFormCC = class(TConfigurationChecker)
  private
    m_parent : TSDFCForm;
  public
    constructor Create(parent : TSDFCForm);
    procedure RunCheck(section : TConfigurationSection); override;
  end;

constructor TSDFCFormCC.Create(parent : TSDFCForm);
begin
  m_parent:=parent;
end;

procedure TSDFCFormCC.RunCheck(section : TConfigurationSection);
begin
  CheckInt(section, 'LEFT', (Screen.Width - m_parent.Width) shr 1, 0);
  CheckInt(section, 'TOP', (Screen.Height - m_parent.Height) shr 1, 0);
end;



procedure TSDFCForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = #27) then begin
    ModalResult:=mrCancel;
    Close;
    Key:=#0;
  end;
end;


procedure TSDFCForm.FormCreate(Sender: TObject);
var
  cc : TSDFCFormCC;
begin
  // get the configuration section
  cc:=TSDFCFormCC.Create(Self);
  m_config:=_globals.GetCfg.GetSection(CONFIG_ID, cc);
  cc.Destroy;

  // set up the window's position
  Left:=m_config.GetIntegerOption('LEFT');
  Top:=m_config.GetIntegerOption('TOP');
end;



function TSDFCForm.Query(nMode : Integer;
                         var vsPassword : String) : Boolean;
var
  scc : TShortcutChecker;
begin

  // (no use for a listener here)

  scc:=TShortcutChecker.Create;
  with _globals.GetSr, scc do begin

    Caption:=Get(CONFIG_ID, 'CAPTION');
    OKBtn.Caption     :=AddShortcut(Get(CONFIG_ID, 'OKBTN'));
    CancelBtn.Caption :=AddShortcut(Get(CONFIG_ID, 'CANCELBTN'));
    HelpBtn.Caption   :=AddShortcut(Get(CONFIG_ID, 'HELPBTN'));

    if (nMode = SDFC_CHANGE) then begin
      PasswBox.PasswordChar:=#0;
      PasswInfo.Caption:=AddShortcut(Get(CONFIG_ID, 'CHANGE'));
      PasswBox.Text:=vsPassword;
    end
    else begin
      PasswBox.PasswordChar:='*';
      PasswInfo.Caption:=AddShortcut(Get(CONFIG_ID, 'ENTER'));
      PasswBox.Text:='';
    end;
  end;
  scc.Destroy;

  Result:=(ShowModal = mrOk);
  if (Result) then
    vsPassword:=PasswBox.Text;

end;


procedure TSDFCForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  m_config.FixIntegerOption('LEFT', Left);
  m_config.FixIntegerOption('TOP', Top);
end;

procedure TSDFCForm.FormShow(Sender: TObject);
begin
  PasswBox.SetFocus;
end;

procedure TSDFCForm.HelpBtnClick(Sender: TObject);
begin
  Application.HelpCommand(HELP_CONTEXT, 503);
end;

function TSDFCForm.FormHelp(Command: Word; Data: Integer;
  var CallHelp: Boolean): Boolean;
begin
  CallHelp := False;
  Result:=HtmlHelp_ShowContext(Data);
end;

end.
