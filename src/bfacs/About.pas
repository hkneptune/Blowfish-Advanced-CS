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
  to show infos about the program
}

unit About;

{$I config.inc}

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, ComCtrls, Configuration, StringRes, ImgList;

type
  TAboutForm = class(TForm)
    NameInfo: TStaticText;
    VersionInfo: TStaticText;
    CopyrightInfo: TStaticText;
    LicenseInfo: TStaticText;
    Logo: TImage;
    OKBtn: TButton;
    ConfigView: TListView;
    procedure FormCreate(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    function FormHelp(Command: Word; Data: Integer;
      var CallHelp: Boolean): Boolean;
    procedure FormPaint(Sender: TObject);
  private
    m_config : TConfigurationSection;
    m_sr     : TStrRes;

    // refreshes the list showing the current configuration
    procedure RefreshConfigList;
  end;

var
  AboutForm: TAboutForm;

implementation
uses
  ShortcutChecker,
  GlobalsGUI,
  Options,
  bfacslib,
  StringPlus,
  Win32Diagnosis,
  HtmlHelpAPI,
  General;

{$R *.DFM}


//////////////////////////// TAboutForm ////////////////////////////


// the configuration ID
const
  CONFIG_ID = 'ABOUTFORM';


// resource listener
type
  TAboutFormStrResListener = class(TStrResListener)
  private
    m_theWin : TAboutForm;
  public
    constructor Create(theWin : TAboutForm);
    procedure ChangeStrings(sr : TStrRes); override;
  end;

constructor TAboutFormStrResListener.Create(theWin : TAboutForm);
begin
  m_theWin:=theWin;
end;


procedure TAboutFormStrResListener.ChangeStrings(sr : TStrRes);
var
  scc : TShortcutChecker;
begin
  scc:=TShortcutChecker.Create;
  with m_theWin, sr, scc do begin
    Caption:=Get(CONFIG_ID, 'CAPTION');
    OKBtn.Caption:=AddShortcut(Get(CONFIG_ID, 'OKBTN'));

    // version number
    VersionInfo.Caption :=
      TWin32Diagnosis.GetModuleVersion(Application.ExeName);

    // copyright
    CopyrightInfo.Caption:=Get(CONFIG_ID, 'COPYRIGHT');

    // do the other stuff outside
    RefreshConfigList;
  end;
  scc.Destroy;
end;


// configuration checker
type
  TAboutFormCC = class(TConfigurationChecker)
  private
    m_parent : TAboutForm;
  public
    constructor Create(parent : TAboutForm);
    procedure RunCheck(section : TConfigurationSection); override;
  end;

constructor TAboutFormCC.Create(parent : TAboutForm);
begin
  m_parent:=parent;
end;

procedure TAboutFormCC.RunCheck(section : TConfigurationSection);
begin
   CheckInt(section, 'LEFT', (Screen.Width - m_parent.Width) shr 1, 0);
   CheckInt(section, 'TOP', (Screen.Height - m_parent.Height) shr 1, 0);
end;


procedure TAboutForm.FormCreate(Sender: TObject);
var
  cc       : TAboutFormCC;
  listener : TAboutFormStrResListener;
begin
  // get the application icon
  Logo.Picture.Icon:=Application.Icon;

  // get thestring resources
  m_sr:=_globals.GetSr;

  // load the static strings
  NameInfo.Caption:=PROGRAM_NAME;

  // get the configuration section
  cc:=TAboutFormCC.Create(Self);
  m_config:=_globals.GetCfg.GetSection(CONFIG_ID, cc);
  cc.Destroy;

  // set up the window's position
  Left:=m_config.GetIntegerOption('LEFT');
  Top:=m_config.GetIntegerOption('TOP');

  // init. listener stuff
  listener:=TAboutFormStrResListener.Create(Self);
  _globals.GetSr.AddListener(listener);
  listener.ChangeStrings(_globals.GetSr);
end;

procedure TAboutForm.OKBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TAboutForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = #27) then begin
    Close;
    Key:=#0;
  end;
end;

procedure TAboutForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  m_config.FixIntegerOption('LEFT', Left);
  m_config.FixIntegerOption('TOP', Top);
end;

procedure TAboutForm.RefreshConfigList;
var
  nClientWidth : Integer;
  nCarry       : Integer;
  sTemp        : String;
  wdiag        : TWin32Diagnosis;

procedure AddEntry(const sID : String;
                   const sCnt : String);
var
  newItem : TListItem;
begin
  with ConfigView.Items, m_sr do begin
    newItem:=Add;
    newItem.Caption:=Get(CONFIG_ID, sID);
    newItem.SubItems.Add(sCnt);
  end;
end;

begin

  // license info
  LicenseInfo.Caption:=m_sr.Get(CONFIG_ID, 'EDITION');

  // now the config. list...
  wdiag:=TWin32Diagnosis.Create(m_sr);
  with ConfigView, m_sr do begin
    Items.Clear;
    Columns[0].Caption:=Get(CONFIG_ID, 'COL_PRP');
    Columns[1].Caption:=Get(CONFIG_ID, 'COL_CNT');

    // info about bfacslib
    AddEntry('CV_BFACSLIB', TWin32Diagnosis.GetModuleVersion(BFACSLIB_MODULE));

    // some system info, please
    AddEntry('CV_WINPF', wdiag.GetPlatform);
    AddEntry('CV_WINVER', wdiag.GetVersion);
    sTemp:=wdiag.GetComputerName;
    if (sTemp <> '') then
      AddEntry('CV_CNAME', sTemp);
    sTemp:=wdiag.GetUserName;
    if (sTemp <> '') then
      AddEntry('CV_UNAME', sTemp);
    AddEntry('CV_NUMOFPR', wdiag.GetNumberOfProcessors);
    AddEntry('CV_MEMLOAD', wdiag.GetMemoryLoad);
    AddEntry('CV_PHMEM', wdiag.GetPhysicalMemory);
    AddEntry('CV_FPHMEM', wdiag.GetFreePhysicalMemory);
    AddEntry('CV_PFSIZE', wdiag.GetPagefileSize);
    AddEntry('CV_FPFSPAC', wdiag.GetFreePagefileSpace);
    AddEntry('CV_SCRRES', wdiag.GetScreenRes);
    AddEntry('CV_MOUSE', wdiag.GetMouseInfo);
    AddEntry('CV_MOUSWH', wdiag.GetMouseWheelInfo);

    Items.EndUpdate;

    // avoid horizontal scroller
    nClientWidth:=Width;
    if (VisibleRowCount <= Items.Count) then
      Dec(nClientWidth, GetSystemMetrics(SM_CXVSCROLL));
    nCarry:=nClientWidth and 1;
    nClientWidth:=nClientWidth shr 1;
    Columns[0].Width:=nClientWidth;
    Columns[1].Width:=nClientWidth + nCarry;

  end;
  wdiag.Destroy;
end;

function TAboutForm.FormHelp(Command: Word; Data: Integer;
  var CallHelp: Boolean): Boolean;
begin
  CallHelp := False;
  Result:=HtmlHelp_ShowContext(Data);
end;

procedure TAboutForm.FormPaint(Sender: TObject);
begin
  //NOP
end;

end.


