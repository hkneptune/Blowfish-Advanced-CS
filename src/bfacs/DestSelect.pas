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
  to select a destination path with favorite path storing
}

unit DestSelect;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls,
  Configuration,
  Favorites;



type
  TDestSelectWin = class(TForm)
    DestList: TListBox;
    RemoveBtn: TButton;
    OKBtn: TButton;
    CancelBtn: TButton;
    HelpBtn: TButton;
    InputBox: TEdit;
    BrowseBtn: TButton;
    InputBoxInfo: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure RemoveBtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure OKBtnClick(Sender: TObject);
    procedure DestListKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormDestroy(Sender: TObject);
    procedure InputBoxKeyPress(Sender: TObject; var Key: Char);
    procedure DestListClick(Sender: TObject);
    procedure DestListDblClick(Sender: TObject);
    procedure BrowseBtnClick(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure HelpBtnClick(Sender: TObject);
    function FormHelp(Command: Word; Data: Integer;
      var CallHelp: Boolean): Boolean;
  private
    // (no need to store the favorite destinations paths globally)
    m_destPaths : TFavorites;
    m_config : TConfigurationSection;
    m_sDestPath : String;
  public
    function GetDestPath : String;
    procedure SetDestPath(const sPath : String);
    procedure Setup(sCaption : String);
  end;

var
  DestSelectWin: TDestSelectWin;

implementation
uses FileCtrl,
     GlobalsGUI,
     HtmlHelpAPI,
     Options,
     StringRes,
     StringPlus,
     ShortcutChecker,
     BrowseForFolder;

{$R *.DFM}


//////////////////////// TDestSelectWin ////////////////////////


// configuration ID
const
  CONFIG_ID  = 'DESTSELECTWIN';


// resource listener
type
  TDestSelectWinStrResListener = class(TStrResListener)
  private
    m_theWin : TDestSelectWin;
  public
    constructor Create(theWin : TDestSelectWin);
    procedure ChangeStrings(sr : TStrRes); override;
  end;

constructor TDestSelectWinStrResListener.Create(theWin : TDestSelectWin);
begin
  m_theWin:=theWin;
end;


procedure TDestSelectWinStrResListener.ChangeStrings(sr : TStrRes);
var
  scc : TShortcutChecker;
begin
  scc:=TShortcutChecker.Create;
  with m_theWin, sr, scc do begin
    OkBtn.Caption       :=AddShortcut(Get(CONFIG_ID, 'OKBTN'));
    CancelBtn.Caption   :=AddShortcut(Get(CONFIG_ID, 'CANCELBTN'));
    RemoveBtn.Caption   :=AddShortcut(Get(CONFIG_ID, 'REMOVEBTN'));
    HelpBtn.Caption     :=AddShortcut(Get(CONFIG_ID, 'HELPBTN'));
    BrowseBtn.Caption   :=AddShortcut(Get(CONFIG_ID, 'BROWSEBTN'));
    InputBoxInfo.Caption:=AddShortcut(Get(CONFIG_ID, 'INPUTBOXINFO'));
  end;
  scc.Destroy;
end;


// configuration checker
type
  TDestSelectWinCC = class(TConfigurationChecker)
  private
    m_parent : TDestSelectWin;
  public
    constructor Create(parent : TDestSelectWin);
    procedure RunCheck(section : TConfigurationSection); override;
  end;

constructor TDestSelectWinCC.Create(parent : TDestSelectWin);
begin
  m_parent:=parent;
end;

procedure TDestSelectWinCC.RunCheck(section : TConfigurationSection);
begin
  CheckInt(section, 'LEFT', (Screen.Width - m_parent.Width) shr 1);
  CheckInt(section, 'TOP', (Screen.Height - m_parent.Height) shr 1);
end;


procedure TDestSelectWin.FormCreate(Sender: TObject);
var
  listener : TDestSelectWinStrResListener;
  cc       : TDestSelectWinCC;
begin
  // get the desination favorites
  m_destPaths:=TFavorites.Create(_globals.GetCfg, CONFIG_ID);

  // get the configuration section
  cc:=TDestSelectWinCC.Create(Self);
  m_config:=_globals.GetCfg.GetSection(CONFIG_ID, cc);
  cc.Destroy;

  // set up the window's position
  with m_config do begin
    // main form borders
    Left:=GetIntegerOption('LEFT');
    Top:=GetIntegerOption('TOP');
  end;

  // init. listener stuff
  listener:=TDestSelectWinStrResListener.Create(Self);
  _globals.GetSr.AddListener(listener);
  listener.ChangeStrings(_globals.GetSr);

  // clear the caption
  Caption:='';
end;


procedure TDestSelectWin.FormShow(Sender: TObject);
var
  nI            : Integer;
  nCount        : Integer;
  favs          : TStringList;
begin

  // get the current favorite destination paths out of the configuration
  favs:=m_destPaths.GetList;

  DestList.Clear;
  DestList.Items.BeginUpdate;

  nCount:=favs.Count;
  nI:=0;
  while (nI < nCount) do begin
    DestList.Items.Add(favs.Strings[nI]);
    Inc(nI);
  end;
  DestList.Items.EndUpdate;

  // focus initally always on the input box
  InputBox.Text:=m_sDestPath;
  InputBox.SetFocus;

  // reset the result code
  ModalResult:=mrNone;
end;

procedure TDestSelectWin.RemoveBtnClick(Sender: TObject);
var
  nI     : Integer;
  nCount : Integer;
begin
  // remove the selected items from the list
  with DestList do begin
    nCount:=Items.Count;
    nI:=0;
    while (nI < nCount) do begin
      if (Selected[nI]) then begin
        Items.Delete(nI);
        Dec(nCount);
      end
      else
        Inc(nI);
    end;
  end;
end;

procedure TDestSelectWin.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  // leave with a cancel result if the dialog was exited not via a button
  if (ModalResult <> mrOk) then
    ModalResult:=mrCancel;

  // store the last position
  m_config.FixIntegerOption('LEFT', Left);
  m_config.FixIntegerOption('TOP', Top);
end;


procedure TDestSelectWin.OKBtnClick(Sender: TObject);
var
  nI       : Integer;
  nCount   : Integer;
  sPath    : String;
begin
  // no path, no fun
  sPath:=TStrPlus.PurePath(Trim(InputBox.Text));
  if (sPath = '') then begin
    SysUtils.Beep;
    Exit;
  end;

  // check for the target path's existance
  if (not DirectoryExists(sPath)) then
    // give the user a chance to create the nonexistant path
    if (Application.MessageBox(PChar(Format(_globals.GetSr.Get(CONFIG_ID, '001'),
                                            [sPath])),
                               PChar(_globals.GetSr.get(CONFIG_ID, '000')),
                               MB_ICONSTOP or MB_YESNO) = IDNO) then
      Exit
    else begin
      ForceDirectories(sPath);
      if (not DirectoryExists(sPath)) then begin
        Application.MessageBox(PChar(Format(_globals.GetSr.get(CONFIG_ID, '002'),
                                            [sPath])),
                               PChar(_globals.GetSr.get(CONFIG_ID, '000')),
                               MB_ICONSTOP);
        Exit;
      end;
    end;

  // fix the favorite destination paths, add the new one, too
  m_destPaths.Clear;
  nI:=0;
  nCount:=DestList.Items.Count;
  while (nI < nCount) do begin
    m_destPaths.Add(DestList.Items[nI], False);
    Inc(nI);
  end;
  m_destPaths.Add(sPath, False);
  m_destPaths.StoreToConfig;

  // save the selected path
  m_sDestPath:=sPath;

  // ready to party
  ModalResult:=mrOk;
end;

procedure TDestSelectWin.DestListKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  // remove the selected items if the DEL key was pressed
  if (Key = VK_DELETE) then
    RemoveBtnClick(Sender);
end;

procedure TDestSelectWin.FormDestroy(Sender: TObject);
begin
  m_destPaths.Destroy;
end;

procedure TDestSelectWin.Setup(sCaption : String);
begin
  Caption:=sCaption;
end;


procedure TDestSelectWin.InputBoxKeyPress(Sender: TObject; var Key: Char);
begin
  // map the enter key to the ok button
  if (Key = #13) then begin
    ModalResult:=mrOk;
    OKBtnClick(Sender);
    Key:=#0;
  end;
end;


procedure TDestSelectWin.DestListClick(Sender: TObject);
begin
  // copy the selected item into the input box
  with DestList do
    if (ItemIndex <> -1) then
     InputBox.Text:=Items[ItemIndex];
end;

procedure TDestSelectWin.DestListDblClick(Sender: TObject);
begin
  // copy the selected item into the input box and try to quit
  DestListClick(Sender);
  OKBtnClick(Sender);
end;

procedure TDestSelectWin.BrowseBtnClick(Sender: TObject);
var
  bff         : TBrowseForFolder;
  sBrowsePath : String;
begin
  // let the user browse for a path
  bff:=TBrowseForFolder.Create;
  sBrowsePath:=TStrPlus.PurePath(InputBox.Text);
  if (not DirectoryExists(sBrowsePath)) then
    sBrowsePath:=TStrPlus.PurePath(ExtractFilePath(InputBox.Text));
  bff.Setup(sBrowsePath);
  if (bff.Execute(_globals.GetSr.Get(CONFIG_ID, '003'),
                  Handle)) then
    InputBox.Text:=bff.GetFolder;
  bff.Destroy;
end;


function TDestSelectWin.GetDestPath : String;
begin
  Result:=m_sDestPath;
end;

procedure TDestSelectWin.SetDestPath(const sPath : String);
begin
  m_sDestPath:=sPath;
end;

procedure TDestSelectWin.FormKeyPress(Sender: TObject; var Key: Char);
begin
  // [ESC] cancels
  if (Key = #27) then begin
    ModalResult:=mrCancel;
    Close;
    Key:=#0;
  end;
end;

procedure TDestSelectWin.HelpBtnClick(Sender: TObject);
begin
  Application.HelpCommand(HELP_CONTEXT, 506);
end;

function TDestSelectWin.FormHelp(Command: Word; Data: Integer;
  var CallHelp: Boolean): Boolean;
begin
  CallHelp := False;
  Result:=HtmlHelp_ShowContext(Data);
end;

end.
