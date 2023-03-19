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
  dialog for entering a new directory's name and attributes
}


unit MakeDirWin;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls,
  Configuration, ExtCtrls;


type
  TMakeDirForm = class(TForm)
    NewDirInfo: TLabel;
    OKBtn: TButton;
    CancelBtn: TButton;
    HelpBtn: TButton;
    InputBox: TMemo;
    procedure FormShow(Sender: TObject);
    procedure NewDirBoxKeyPress(Sender: TObject; var Key: Char);
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
    function GetNewDirNames : TStringList;
  end;

var
  MakeDirForm: TMakeDirForm;

implementation
uses
  StringRes,
  HtmlHelpAPI,
  Globals,
  Options,
  ShortcutChecker,
  FileSupp;

{$R *.DFM}


//////////////////////////// TMakeDirForm ////////////////////////////


// the configuration ID
const
  CONFIG_ID = 'MAKEDIRFORM';


// resource listener
type
  TMakeDirFormStrResListener = class(TStrResListener)
  private
    m_theWin : TMakeDirForm;
  public
    constructor Create(theWin : TMakeDirForm);
    procedure ChangeStrings(sr : TStrRes); override;
  end;

constructor TMakeDirFormStrResListener.Create(theWin : TMakeDirForm);
begin
  m_theWin:=theWin;
end;


procedure TMakeDirFormStrResListener.ChangeStrings(sr : TStrRes);
var
  scc : TShortcutChecker;
begin
  scc:=TShortcutChecker.Create;
  with m_theWin, sr, scc do begin
    Caption:=Get(CONFIG_ID, 'CAPTION');
    OKBtn.Caption     :=AddShortcut(Get(CONFIG_ID, 'OKBTN'));
    CancelBtn.Caption :=AddShortcut(Get(CONFIG_ID, 'CANCELBTN'));
    HelpBtn.Caption   :=AddShortcut(Get(CONFIG_ID, 'HELPBTN'));
    NewDirInfo.Caption:=AddShortcut(Get(CONFIG_ID, 'NEWDIRINFO'));
  end;
  scc.Destroy;
end;


// configuration checker
type
  TMakeDirFormCC = class(TConfigurationChecker)
  private
    m_parent : TMakeDirForm;
  public
    constructor Create(parent : TMakeDirForm);
    procedure RunCheck(section : TConfigurationSection); override;
  end;

constructor TMakeDirFormCC.Create(parent : TMakeDirForm);
begin
  m_parent:=parent;
end;

procedure TMakeDirFormCC.RunCheck(section : TConfigurationSection);
begin
  CheckInt(section, 'LEFT', (Screen.Width - m_parent.Width) shr 1, 0);
  CheckInt(section, 'TOP', (Screen.Height - m_parent.Height) shr 1, 0);
end;



procedure TMakeDirForm.FormCreate(Sender: TObject);
var
  cc       : TMakeDirFormCC;
  listener : TMakeDirFormStrResListener;
begin
  // get the configuration section
  cc:=TMakeDirFormCC.Create(Self);
  m_config:=_globals.GetCfg.GetSection(CONFIG_ID, cc);
  cc.Destroy;

  // set up the window's position
  Left:=m_config.GetIntegerOption('LEFT');
  Top:=m_config.GetIntegerOption('TOP');

  // init. listener stuff
  listener:=TMakeDirFormStrResListener.Create(Self);
  _globals.GetSr.AddListener(listener);
  listener.ChangeStrings(_globals.GetSr);
end;


procedure TMakeDirForm.FormShow(Sender: TObject);
begin
  // set the focus to the input box and clear it
  InputBox.SetFocus;
  InputBox.Text:='';
end;

procedure TMakeDirForm.NewDirBoxKeyPress(Sender: TObject; var Key: Char);
begin
  // enter key finishes
  if (Key = #13) then begin
    ModalResult:=mrOk;
    Close;
    Key:=#0;
  end;
end;

procedure TMakeDirForm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
var
  nI       : Integer;
  cIllChar : Char;
  newDirs  : TStringList;
begin
  // check the file name for illegal characters (as far as we know), i. n.
  CanClose:=True;
  if (ModalResult = mrOk) then begin
    newDirs:=GetNewDirNames;
    for nI:=0 to (newDirs.Count - 1) do begin

      if (not TFileSupport.IsLegalFileName(newDirs.Strings[nI],
                                           @cIllChar, True)) then begin
        Application.MessageBox(PChar(Format(_globals.GetSr.Get(CONFIG_ID,
                                                               'ILLEGAL_NAME'),
                                            [String(cIllChar)])),
                               PChar(_globals.GetSr.Get(CONFIG_ID, 'ERROR')),
                               MB_ICONSTOP);
        CanClose:=False;
        newDirs.Destroy;
        Exit;
      end;
    end;
    newDirs.Destroy;
  end;
end;


procedure TMakeDirForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  m_config.FixIntegerOption('LEFT', Left);
  m_config.FixIntegerOption('TOP', Top);
end;


function TMakeDirForm.GetNewDirNames : TStringList;
var
  nI    : Integer;
  sTemp : String;
begin
  Result:=TStringList.Create;
  for nI:=0 to (InputBox.Lines.Count - 1) do begin

    sTemp:=Trim(InputBox.Lines[nI]);
    if (sTemp <> '') then
      Result.Add(sTemp);
  end;
end;



procedure TMakeDirForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
  // [ESC] cancels
  if (Key = #27) then begin
    ModalResult:=mrCancel;
    Close;
    Key:=#0;
  end;
end;

procedure TMakeDirForm.HelpBtnClick(Sender: TObject);
begin
  Application.HelpCommand(HELP_CONTEXT, 223);
end;

function TMakeDirForm.FormHelp(Command: Word; Data: Integer;
  var CallHelp: Boolean): Boolean;
begin
  CallHelp := False;
  Result:=HtmlHelp_ShowContext(Data);
end;

end.
