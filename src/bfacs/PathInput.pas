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
  for entering a path (with browsing capabilities)
}

unit PathInput;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TPathInputWin = class(TForm)
    InputBox: TEdit;
    CancelBtn: TButton;
    OKBtn: TButton;
    SearchBtn: TButton;
    InputInfo: TLabel;
    HelpBtn: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure SearchBtnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure InputBoxKeyPress(Sender: TObject; var Key: Char);
    procedure OKBtnClick(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure HelpBtnClick(Sender: TObject);
    function FormHelp(Command: Word; Data: Integer;
      var CallHelp: Boolean): Boolean;
  private
    m_sSavePath : String;
  public
    procedure SetPath(sPath : String);
    function GetPath : String;
    procedure SetInfo(sCaption, sPrompt : String);
  end;

var
  PathInputWin: TPathInputWin;

implementation
uses FileCtrl,
     StringRes,
     Configuration,
     GlobalsGUI,
     BrowseForFolder,
     ShortcutChecker,
     Options,
     HtmlHelpAPI,
     StringPlus;

{$R *.DFM}


//////////////////////////// TPathInputWin ////////////////////////////


// the configuration ID
const
  CONFIG_ID = 'PATHINPUTWIN';


// resource listener
type
  TPathInputWinStrResListener = class(TStrResListener)
  private
    m_theWin : TPathInputWin;
  public
    constructor Create(theWin : TPathInputWin);
    procedure ChangeStrings(sr : TStrRes); override;
  end;

constructor TPathInputWinStrResListener.Create(theWin : TPathInputWin);
begin
  m_theWin:=theWin;
end;


procedure TPathInputWinStrResListener.ChangeStrings(sr : TStrRes);
var
  scc : TShortcutChecker;
begin
  scc:=TShortcutChecker.Create;
  with m_theWin, sr, scc do begin
    OkBtn.Caption    :=AddShortcut(Get(CONFIG_ID, 'OKBTN'));
    CancelBtn.Caption:=AddShortcut(Get(CONFIG_ID, 'CANCELBTN'));
    SearchBtn.Caption:=AddShortcut(Get(CONFIG_ID, 'SEARCHBTN'));
    HelpBtn.Caption  :=AddShortcut(Get(CONFIG_ID, 'HELPBTN'));
  end;
  scc.Destroy;
end;

// configuration checker
type
  TPathInputWinCC = class(TConfigurationChecker)
  private
    m_parent : TPathInputWin;
  public
    constructor Create(parent : TPathInputWin);
    procedure RunCheck(section : TConfigurationSection); override;
  end;

constructor TPathInputWinCC.Create(parent : TPathInputWin);
begin
  m_parent:=parent;
end;

procedure TPathInputWinCC.RunCheck(section : TConfigurationSection);
begin
  CheckInt(section, 'LEFT', (Screen.Width - m_parent.Width) shr 1);
  CheckInt(section, 'TOP', (Screen.Height - m_parent.Height) shr 1);
end;


procedure TPathInputWin.FormCreate(Sender: TObject);
var
  listener : TPathInputWinStrResListener;
  cc       : TPathInputWinCC;
begin
  // set up the window's position
  cc:=TPathInputWinCC.Create(Self);
  with _globals.GetCfg.GetSection(CONFIG_ID, cc) do begin
    // main form borders
    Left:=GetIntegerOption('LEFT');
    Top:=GetIntegerOption('TOP');
  end;
  cc.Destroy;

  // empty caption and prompt
  Caption:='';
  InputInfo.Caption:='';

  // init. listener stuff
  listener:=TPathInputWinStrResListener.Create(Self);
  _globals.GetSr.AddListener(listener);
  listener.ChangeStrings(_globals.GetSr);
end;


procedure TPathInputWin.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  // store the last position of that window
  with _globals.GetCfg.GetSection(CONFIG_ID) do begin
    FixIntegerOption('LEFT', Left);
    FixIntegerOption('TOP', Top);
  end;

  // restore the input box content, i. n.
  if (ModalResult <> mrOk) then
    InputBox.Text:=m_sSavePath;
end;


procedure TPathInputWin.SearchBtnClick(Sender: TObject);
var
  bff         : TBrowseForFolder;
  sBrowsePath : String;
begin
  bff:=TBrowseForFolder.Create;
  sBrowsePath:=TStrPlus.PurePath(InputBox.Text);
  if (not DirectoryExists(sBrowsePath)) then
    sBrowsePath:=TStrPlus.PurePath(ExtractFilePath(InputBox.Text));
  bff.Setup(sBrowsePath);
  if (bff.Execute(_globals.GetSr.Get(CONFIG_ID, 'SELECTPATH'),
                  Handle)) then
    InputBox.Text:=bff.GetFolder;
  bff.Destroy;
end;


procedure TPathInputWin.SetInfo(sCaption, sPrompt : String);
var
  scc : TShortcutChecker;
begin
  Caption:=sCaption;
  // get a good shortcut for the new prompt
  scc:=TShortcutChecker.Create;
  with scc do begin
    Scan(OkBtn.Caption);
    Scan(CancelBtn.Caption);
    Scan(SearchBtn.Caption);
    Scan(HelpBtn.Caption);
    InputInfo.Caption:=AddShortcut(sPrompt);
  end;  
  scc.Destroy;
end;


procedure TPathInputWin.FormShow(Sender: TObject);
begin
  // save the current path for the case that the user cancels
  m_sSavePath:=InputBox.Text;
end;

procedure TPathInputWin.InputBoxKeyPress(Sender: TObject; var Key: Char);
begin
  // enter key leaves
  if (Key = #13) then begin
    ModalResult:=mrOk;
    Close;
    Key:=#0;
  end;
end;

procedure TPathInputWin.SetPath(sPath : String);
begin
  InputBox.Text:=sPath;
end;


function TPathInputWin.GetPath : String;
begin
  Result:=InputBox.Text;
end;


procedure TPathInputWin.OKBtnClick(Sender: TObject);
begin
  if (Trim(InputBox.Text) = '') then begin
    Application.MessageBox(PChar(_globals.GetSr.Get(CONFIG_ID, 'ENTERPLEASE')),
                           PChar(_globals.GetSr.Get(CONFIG_ID, 'ERROR')),
                           MB_ICONSTOP);
    ModalResult:=mrNone;
  end
  else begin
    ModalResult:=mrOk;
  end;
end;

procedure TPathInputWin.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = #27) then begin
    ModalResult:=mrCancel;
    Close;
    Key:=#0;
  end;
end;

procedure TPathInputWin.HelpBtnClick(Sender: TObject);
begin
  Application.HelpCommand(HELP_CONTEXT, 502);
end;

function TPathInputWin.FormHelp(Command: Word; Data: Integer;
  var CallHelp: Boolean): Boolean;
begin
  CallHelp := False;
  Result:=HtmlHelp_ShowContext(Data);
end;

end.
