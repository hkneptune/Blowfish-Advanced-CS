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
  "organizer" for the favorite paths
}


unit FavWin;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TFavoritesWin = class(TForm)
    FavList: TListBox;
    RemoveBtn: TButton;
    OKBtn: TButton;
    CancelBtn: TButton;
    FavListInfo: TLabel;
    HelpBtn: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure RemoveBtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure OKBtnClick(Sender: TObject);
    procedure FavListKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure HelpBtnClick(Sender: TObject);
    function FormHelp(Command: Word; Data: Integer;
      var CallHelp: Boolean): Boolean;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FavoritesWin: TFavoritesWin;

implementation
uses Globals,
     HtmlHelpAPI,
     StringRes,
     ShortcutChecker,
     Options,
     Configuration;

{$R *.DFM}


//////////////////////// TFavoritesWin ////////////////////////



// the configuration ID
const
  CONFIG_ID = 'FAVORITESWIN';


// resource listener
type
  TFavoritesWinStrResListener = class(TStrResListener)
  private
    m_theWin : TFavoritesWin;
  public
    constructor Create(theWin : TFavoritesWin);
    procedure ChangeStrings(sr : TStrRes); override;
  end;

constructor TFavoritesWinStrResListener.Create(theWin : TFavoritesWin);
begin
  m_theWin:=theWin;
end;


procedure TFavoritesWinStrResListener.ChangeStrings(sr : TStrRes);
var
  scc : TShortcutChecker;
begin
  scc:=TShortcutChecker.Create;
  with m_theWin, sr, scc do begin
    Caption:=Get(CONFIG_ID, 'CAPTION');
    OkBtn.Caption      :=AddShortcut(Get(CONFIG_ID, 'OKBTN'));
    CancelBtn.Caption  :=AddShortcut(Get(CONFIG_ID, 'CANCELBTN'));
    RemoveBtn.Caption  :=AddShortcut(Get(CONFIG_ID, 'REMOVEBTN'));
    HelpBtn.Caption    :=AddShortcut(Get(CONFIG_ID, 'HELPBTN'));
    FavListInfo.Caption:=AddShortcut(Get(CONFIG_ID, 'FAVLISTINFO'));
  end;
  scc.Destroy;
end;


// configuration checker
type
  TFavoritesWinCC = class(TConfigurationChecker)
  private
    m_parent : TFavoritesWin;
  public
    constructor Create(parent : TFavoritesWin);
    procedure RunCheck(section : TConfigurationSection); override;
  end;

constructor TFavoritesWinCC.Create(parent : TFavoritesWin);
begin
  m_parent:=parent;
end;

procedure TFavoritesWinCC.RunCheck(section : TConfigurationSection);
begin
  CheckInt(section, 'LEFT', (Screen.Width - m_parent.Width) shr 1);
  CheckInt(section, 'TOP', (Screen.Height - m_parent.Height) shr 1);
end;


procedure TFavoritesWin.FormCreate(Sender: TObject);
var
  listener : TFavoritesWinStrResListener;
  cc       : TFavoritesWinCC;
begin
  // set up the window's position
  cc:=TFavoritesWinCC.Create(Self);
  with _globals.GetCfg.GetSection(CONFIG_ID, cc) do begin
    // main form borders
    Left:=GetIntegerOption('LEFT');
    Top:=GetIntegerOption('TOP');
  end;
  cc.Destroy;

  // init. listener stuff
  listener:=TFavoritesWinStrResListener.Create(Self);
  _globals.GetSr.AddListener(listener);
  listener.ChangeStrings(_globals.GetSr);
end;



procedure TFavoritesWin.FormShow(Sender: TObject);
var
  nI            : Integer;
  nCount        : Integer;
  favs          : TStringList;
begin

  // get the current paths out of the configuration
  favs:=_globals.GetFavorites.GetList;
  FavList.Clear;
  nI:=0;
  nCount:=favs.Count;
  FavList.Items.BeginUpdate;
  while (nI < nCount) do begin
    FavList.Items.Add(favs.Strings[nI]);
    Inc(nI);
  end;
  FavList.Items.EndUpdate;
end;

procedure TFavoritesWin.RemoveBtnClick(Sender: TObject);
var
  nI     : Integer;
  nCount : Integer;
begin
  // remove the selected items from the list
  with FavList do begin
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

procedure TFavoritesWin.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  // leave with a cancel result if the dialog was exited not via a button
  if (ModalResult <> mrOk) then
    ModalResult:=mrCancel;

  // store the last position
  with _globals.GetCfg.GetSection(CONFIG_ID) do begin
    FixIntegerOption('LEFT', Left);
    FixIntegerOption('TOP', Top);
  end;  
end;

procedure TFavoritesWin.OKBtnClick(Sender: TObject);
var
  nI     : Integer;
  nCount : Integer;
begin
  // fix the new favorites
  _globals.GetFavorites.Clear;
  nI:=0;
  nCount:=FavList.Items.Count;
  while (nI < nCount) do begin
    _globals.GetFavorites.Add(FavList.Items[nI], False);
    Inc(nI);
  end;
  _globals.GetFavorites.StoreToConfig;
end;

procedure TFavoritesWin.FavListKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  // remove the selected items if the DEL key was pressed
  if (Key = VK_DELETE) then
    RemoveBtnClick(Sender);
end;

procedure TFavoritesWin.FormKeyPress(Sender: TObject; var Key: Char);
begin
  // [ESC] cancels
  if (Key = #27) then begin
    ModalResult:=mrCancel;
    Close;
    Key:=#0;
  end;
end;

procedure TFavoritesWin.HelpBtnClick(Sender: TObject);
begin
  Application.HelpCommand(HELP_CONTEXT, 400);
end;

function TFavoritesWin.FormHelp(Command: Word; Data: Integer;
  var CallHelp: Boolean): Boolean;
begin
  CallHelp := False;
  Result:=HtmlHelp_ShowContext(Data);
end;

end.
