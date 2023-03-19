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
  simple dialog for password confirmation
}


unit PasswConfirm;

{$I config.inc}

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons,
  Configuration, SecureMem, Menus;

type
  TPasswConfirmWin = class(TForm)
    Info: TLabel;
    InputBox: TEdit;
    OKBtn: TButton;
    CancelBtn: TButton;
    PWInputMenu: TPopupMenu;
    PWIMenu_PasteAndClear: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure OKBtnClick(Sender: TObject);
    procedure InputBoxKeyPress(Sender: TObject; var Key: Char);
    procedure PWIMenu_PasteAndClearClick(Sender: TObject);
    procedure PWInputMenuPopup(Sender: TObject);
    function FormHelp(Command: Word; Data: Integer;
      var CallHelp: Boolean): Boolean;
    procedure InputBoxKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    m_config : TConfigurationSection;
    m_passw  : TKeyMemory;
  public
    // returns the _instance_ of the stored password
    // <- the password (may be Nil)
    function GetPassword : TKeyMemory;

    // clears the stored password
    procedure ClearPassword;

    // sets the password character (show/hide)
    // -> password char. (#0 means "show password")
    procedure SetPasswordChar(cPasswHide : Char);

    // sets the password limitation (show/hide)
    // -> password char. (0 equals "limited by the OS")
    procedure SetMaxPasswLen(nMaxLen : Integer);
  end;

var
  PasswConfirmWin: TPasswConfirmWin;

implementation
uses
  clipbrd,
  StringRes,
  StringPlus,
  ShortcutChecker,
  Options,
  HtmlHelpAPI,
  Globals,
  TipWin;

{$R *.DFM}


//////////////////////////// TPasswConfirmWin ////////////////////////////


// the configuration ID
const
  CONFIG_ID = 'PasswConfirm';


// resource listener
type
  TPasswConfirmWinStrResListener = class(TStrResListener)
  private
    m_theWin : TPasswConfirmWin;
  public
    constructor Create(theWin : TPasswConfirmWin);
    procedure ChangeStrings(sr : TStrRes); override;
  end;

constructor TPasswConfirmWinStrResListener.Create(theWin : TPasswConfirmWin);
begin
  m_theWin:=theWin;
end;                                                           


procedure TPasswConfirmWinStrResListener.ChangeStrings(sr : TStrRes);
var
  scc : TShortcutChecker;
begin
  scc:=TShortcutChecker.Create;
  with m_theWin, _globals.GetSr, scc do begin
    Caption:=Get(CONFIG_ID, 'CAPTION');
    Info.Caption     :=AddShortcut(Get(CONFIG_ID, 'INFO'));
    OKBtn.Caption    :=AddShortcut(Get(CONFIG_ID, 'OKBTN'));
    CancelBtn.Caption:=AddShortcut(Get(CONFIG_ID, 'CANCELBTN'));
    PWIMenu_PasteAndClear.Caption:=AddShortcut(Get(CONFIG_ID, 'PASTEANDCLEAR'));
  end;
  scc.Destroy;
end;


// configuration checker
type
  TPasswConfirmWinCC = class(TConfigurationChecker)
  private
    m_parent : TPasswConfirmWin;
  public
    constructor Create(parent : TPasswConfirmWin);
    procedure RunCheck(section : TConfigurationSection); override;
  end;

constructor TPasswConfirmWinCC.Create(parent : TPasswConfirmWin);
begin
  m_parent:=parent;
end;

procedure TPasswConfirmWinCC.RunCheck(section : TConfigurationSection);
begin
  with m_parent as TPasswConfirmWin do begin
    CheckInt(section, 'LEFT', (Screen.Width - Width) shr 1);
    CheckInt(section, 'TOP', (Screen.Height - Height) shr 1);
  end;
end;



procedure TPasswConfirmWin.FormCreate(Sender: TObject);
var
  cc       : TPasswConfirmWinCC;
  listener : TPasswConfirmWinStrResListener;
begin

  // get the configuration section
  cc:=TPasswConfirmWinCC.Create(Self);
  m_config:=_globals.GetCfg.GetSection(CONFIG_ID, cc);
  cc.Destroy;

  // set up the window's position
  with m_config do begin
    // main form borders
    Left:=GetIntegerOption('LEFT');
    Top:=GetIntegerOption('TOP');
  end;

  // init. listener stuff
  listener:=TPasswConfirmWinStrResListener.Create(Self);
  _globals.GetSr.AddListener(listener);
  listener.ChangeStrings(_globals.GetSr);

  // no password stored yet
  m_passw:=Nil;
end;

procedure TPasswConfirmWin.FormShow(Sender: TObject);
begin

  // focus always on the input box
  InputBox.SetFocus;

  // clear the input box
  InputBox.Clear;
end;

procedure TPasswConfirmWin.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  // clear the password on a break
  if (ModalResult = mrCancel) then
    InputBox.Clear;

  // store the last window position
  m_config.FixIntegerOption('LEFT', Left);
  m_config.FixIntegerOption('TOP', Top);
end;

procedure TPasswConfirmWin.FormDestroy(Sender: TObject);
begin
  InputBox.Clear;
  ClearPassword;
end;

procedure TPasswConfirmWin.FormKeyPress(Sender: TObject; var Key: Char);
begin
  // map the [Esc] key to the [Cancel] button
  if (Key = #27) then begin
    Key:=#0;
    ModalResult:=mrCancel;
    Close;
  end;
end;

function TPasswConfirmWin.GetPassword : TKeyMemory;
begin
  Result:=m_passw;
end;

procedure TPasswConfirmWin.ClearPassword;
begin
  if (m_passw <> Nil) then begin
    m_passw.Destroy;
    m_passw:=Nil;
  end;
end;

procedure TPasswConfirmWin.OKBtnClick(Sender: TObject);
var
  nLen  : Integer;
  sTemp : String;
begin
  // get the password (and possible binary sequences)
  nLen:=TStrPlus.GetBinStr(InputBox.Text, sTemp);
  if (nLen = -1) then begin
    Application.MessageBox(PChar(_globals.GetSr.Get(CONFIG_ID, 'BADBIN')),
                           PChar(_globals.GetSr.Get(CONFIG_ID, 'ERROR')),
                           MB_ICONSTOP);
    ModalResult:=mrNone;
    InputBox.SetFocus;
    Exit;
  end;

  // zero password?
  if (nLen = 0) then begin
    Application.MessageBox(PChar(_globals.GetSr.Get(CONFIG_ID, 'ZEROPASSW')),
                           PChar(_globals.GetSr.Get(CONFIG_ID, 'ERROR')),
                           MB_ICONSTOP);
    ModalResult:=mrNone;
    InputBox.SetFocus;
    Exit;
  end;

  // move it to the secure memory and kill the temporary string
  if (m_passw <> Nil) then
    m_passw.Destroy;
  m_passw:=TKeyMemory.Create(nLen);
  m_passw.SetData(@sTemp[1], 0, nLen);
  TStrPlus.ClearString(sTemp);
end;

procedure TPasswConfirmWin.InputBoxKeyPress(Sender: TObject;
  var Key: Char);
begin
  // map the enter key to the OK button
  if (Key = #13) then begin
    OkBtnClick(Sender);
    Key:=#0;
  end;
end;

procedure TPasswConfirmWin.SetPasswordChar(cPasswHide : Char);
begin
  InputBox.PasswordChar:=cPasswHide;
end;

procedure TPasswConfirmWin.SetMaxPasswLen(nMaxLen : Integer);
begin
  InputBox.MaxLength:=nMaxLen;
end;


procedure TPasswConfirmWin.PWIMenu_PasteAndClearClick(Sender: TObject);
begin
  InputBox.PasteFromClipboard;
  Clipboard.Clear;
end;

procedure TPasswConfirmWin.PWInputMenuPopup(Sender: TObject);
var
  blGotText : Boolean;
begin
  blGotText:=Clipboard.HasFormat(CF_TEXT);
  PWIMenu_PasteAndClear.Enabled:=blGotText;
end;

function TPasswConfirmWin.FormHelp(Command: Word; Data: Integer;
  var CallHelp: Boolean): Boolean;
begin
  CallHelp := False;
  Result:=HtmlHelp_ShowContext(Data);
end;

procedure TPasswConfirmWin.InputBoxKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if ((Windows.GetKeyState(VK_CAPITAL) and 1) = 1) then
    TipForm.ShowTip(TIP_CAPSLOCKWARNING);
end;

end.

