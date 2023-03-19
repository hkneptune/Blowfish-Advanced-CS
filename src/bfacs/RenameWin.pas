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
  dialog for renaming files
}

unit RenameWin;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Configuration;


type
  TRenameForm = class(TForm)
    OldNameBox: TEdit;
    NewNameBox: TEdit;
    OldNameInfo: TLabel;
    NewNameInfo: TLabel;
    OKBtn: TButton;
    CancelBtn: TButton;
    HelpBtn: TButton;
    procedure FormShow(Sender: TObject);
    procedure NewNameBoxKeyPress(Sender: TObject; var Key: Char);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure HelpBtnClick(Sender: TObject);
    function FormHelp(Command: Word; Data: Integer;
      var CallHelp: Boolean): Boolean;
  private
    m_config : TConfigurationSection;
  public
    procedure SetOldName(sOldName : String);
    function GetNewName : String;
  end;

var
  RenameForm: TRenameForm;

implementation
uses
  StringRes,
  Globals,
  HtmlHelpAPI,
  ShortcutChecker,
  FileSupp,
  Options;

{$R *.DFM}


//////////////////////////// TRenameForm ////////////////////////////


// the configuration ID
const
  CONFIG_ID = 'RENAMEFORM';


// resource listener
type
  TRenameFormStrResListener = class(TStrResListener)
  private
    m_theWin : TRenameForm;
  public
    constructor Create(theWin : TRenameForm);
    procedure ChangeStrings(sr : TStrRes); override;
  end;

constructor TRenameFormStrResListener.Create(theWin : TRenameForm);
begin
  m_theWin:=theWin;
end;


procedure TRenameFormStrResListener.ChangeStrings(sr : TStrRes);
var
  scc : TShortcutChecker;
begin
  scc:=TShortcutChecker.Create;
  with m_theWin, sr, scc do begin
    Caption:=Get(CONFIG_ID, 'CAPTION');
    OKBtn.Caption      :=AddShortcut(Get(CONFIG_ID, 'OKBTN'));
    CancelBtn.Caption  :=AddShortcut(Get(CONFIG_ID, 'CANCELBTN'));
    HelpBtn.Caption    :=AddShortcut(Get(CONFIG_ID, 'HELPBTN'));
    OldNameInfo.Caption:=AddShortcut(Get(CONFIG_ID, 'OLDNAMEINFO'));
    NewNameInfo.Caption:=AddShortcut(Get(CONFIG_ID, 'NEWNAMEINFO'));
  end;
  scc.Destroy;
end;


// configuration checker
type
  TRenameFormCC = class(TConfigurationChecker)
  private
    m_parent : TRenameForm;
  public
    constructor Create(parent : TRenameForm);
    procedure RunCheck(section : TConfigurationSection); override;
  end;

constructor TRenameFormCC.Create(parent : TRenameForm);
begin
  m_parent:=parent;
end;

procedure TRenameFormCC.RunCheck(section : TConfigurationSection);
begin
  CheckInt(section, 'LEFT', (Screen.Width - m_parent.Width) shr 1);
  CheckInt(section, 'TOP', (Screen.Height - m_parent.Height) shr 1);
end;



procedure TRenameForm.FormCreate(Sender: TObject);
var
  cc       : TRenameFormCC;
  listener : TRenameFormStrResListener;
begin
  // get the configuration section
  cc:=TRenameFormCC.Create(Self);
  m_config:=_globals.GetCfg.GetSection(CONFIG_ID, cc);
  cc.Destroy;

  // set up the window's position
  with m_config do begin
    // main form borders
    Left:=GetIntegerOption('LEFT');
    Top:=GetIntegerOption('TOP');
  end;

  // init. listener stuff
  listener:=TRenameFormStrResListener.Create(Self);
  _globals.GetSr.AddListener(listener);
  listener.ChangeStrings(_globals.GetSr);
end;




procedure TRenameForm.SetOldName(sOldName : String);
begin
  OldNameBox.Text:=sOldName;
  NewNameBox.Text:=sOldName;
end;


function TRenameForm.GetNewName : String;
begin
  Result:=NewNameBox.Text;
end;

procedure TRenameForm.FormShow(Sender: TObject);
begin
  NewNameBox.SetFocus;
  NewNameBox.SelectAll;
end;

procedure TRenameForm.NewNameBoxKeyPress(Sender: TObject; var Key: Char);
begin
  // enter key finishes
  if (Key = #13) then begin
    ModalResult:=mrOk;
    Close;
    Key:=#0;
  end;
end;

procedure TRenameForm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
var
  cIllChar : Char;
begin
  // check the file name for illegal characters (as far as we know), i. n.
  CanClose:=True;
  if (ModalResult = mrOk) then
    if (not TFileSupport.IsLegalFileName(NewNameBox.Text,
                                         @cIllChar)) then begin
      Application.MessageBox(PChar(Format(_globals.GetSr.Get(CONFIG_ID,
                                                             'ILLEGAL_NAME'),
                                          [String(cIllChar)])),
                             PChar(_globals.GetSr.Get(CONFIG_ID, 'ERROR')),
                             MB_ICONSTOP);
      CanClose:=False;
    end;
end;


procedure TRenameForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  m_config.FixIntegerOption('LEFT', Left);
  m_config.FixIntegerOption('TOP', Top);
end;

procedure TRenameForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
  // [ESC] cancels
  if (Key = #27) then begin
    ModalResult:=mrCancel;
    Close;
    Key:=#0;
  end;
end;

procedure TRenameForm.HelpBtnClick(Sender: TObject);
begin
  Application.HelpCommand(HELP_CONTEXT, 109);
end;

function TRenameForm.FormHelp(Command: Word; Data: Integer;
  var CallHelp: Boolean): Boolean;
begin
  CallHelp := False;
  Result:=HtmlHelp_ShowContext(Data);
end;

end.
