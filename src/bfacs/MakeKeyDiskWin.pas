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
  dialog to create a key disk
}

unit MakeKeyDiskWin;

{$I config.inc}

interface
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls,
  Configuration;


// the number of seed bytes we demand (we use a buffer to get as many bytes
// as possible through the reseed mechanism of a secure PRNG)
const
{$ifdef __DEBUG}
  NUM_OF_SEEDBYTES = 16;
{$else}
  NUM_OF_SEEDBYTES = 1024;
{$endif}


type
  TMakeKeyDiskForm = class(TForm)
    SizeChooser: TUpDown;
    ProgressBar: TProgressBar;
    SizeView: TStaticText;
    SizeInfo: TLabel;
    HelpBtn: TButton;
    CancelBtn: TButton;
    HowToInfo: TLabel;
    SaveDialog: TSaveDialog;
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormShow(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure HelpBtnMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure CancelBtnMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ProgressBarMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure SizeViewMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure SizeInfoMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure SizeChooserMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure HowToInfoMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure SetKeyFileName(const sKeyFileName : String);
    procedure HelpBtnClick(Sender: TObject);
    function FormHelp(Command: Word; Data: Integer;
      var CallHelp: Boolean): Boolean;
  private
    m_config        : TConfigurationSection;
    m_nSeedBytesGot : Integer;
    m_sKeyFileName  : String;
    m_seedBuffer    : array[0..NUM_OF_SEEDBYTES - 1] of Byte;
    procedure AddSeedData(pSeed : Pointer; nNumOfBytes : Integer);
    procedure AddXYSeed(nX, nY : Integer; ctrl : TControl = Nil);
    procedure Proceed;
  end;

var
  MakeKeyDiskForm: TMakeKeyDiskForm;

implementation
uses
  StringRes,
  Globals,
  HtmlHelpAPI,
  KeyDisk,
  Options,
  ShortcutChecker;

{$R *.DFM}


//////////////////////////// TMakeKeyDiskForm ////////////////////////////


// the configuration ID
const
  CONFIG_ID = 'MAKEKEYDISKFORM';


// less than 16 bits are a joke
const
  MIN_KEY_SIZE = 16;

// last should at least be allowed
const
  DEF_KEY_SIZE = 32;

// more makes now sense
const
  MAX_KEY_SIZE = 1024;


// resource listener
type
  TMakeKeyDiskFormStrResListener = class(TStrResListener)
  private
    m_theWin : TMakeKeyDiskForm;
  public
    constructor Create(theWin : TMakeKeyDiskForm);
    procedure ChangeStrings(sr : TStrRes); override;
  end;

constructor TMakeKeyDiskFormStrResListener.Create(theWin : TMakeKeyDiskForm);
begin
  m_theWin:=theWin;
end;


procedure TMakeKeyDiskFormStrResListener.ChangeStrings(sr : TStrRes);
var
  scc : TShortcutChecker;
begin
  scc:=TShortcutChecker.Create;
  with m_theWin, sr, scc do begin
    Caption:=Get(CONFIG_ID, 'CAPTION');
    CancelBtn.Caption :=AddShortcut(Get(CONFIG_ID, 'CANCELBTN'));
    HelpBtn.Caption   :=AddShortcut(Get(CONFIG_ID, 'HELPBTN'));
    SizeInfo.Caption  :=AddShortcut(Get(CONFIG_ID, 'SIZEINFO'));
    HowToInfo.Caption :=Get(CONFIG_ID, 'HOWTOINFO');
  end;
  scc.Destroy;
end;


// configuration checker
type
  TMakeKeyDiskFormCC = class(TConfigurationChecker)
  private
    m_parent : TMakeKeyDiskForm;
  public
    constructor Create(parent : TMakeKeyDiskForm);
    procedure RunCheck(section : TConfigurationSection); override;
  end;

constructor TMakeKeyDiskFormCC.Create(parent : TMakeKeyDiskForm);
begin
  m_parent:=parent;
end;

procedure TMakeKeyDiskFormCC.RunCheck(section : TConfigurationSection);
begin
  CheckInt(section, 'LEFT', (Screen.Width - m_parent.Width) shr 1, 0);
  CheckInt(section, 'TOP', (Screen.Height - m_parent.Height) shr 1, 0);
  CheckInt(section, 'WIDTH', m_parent.Constraints.MinWidth, 0);
  CheckInt(section, 'HEIGHT', m_parent.Constraints.MinHeight, 0);
  CheckInt(section, 'KEYSIZE', DEF_KEY_SIZE, 0);
end;


procedure TMakeKeyDiskForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
  // ESC cancels
  if (Key = #27) then begin
    Close;
    Key:=#0;
  end;
end;

procedure TMakeKeyDiskForm.FormShow(Sender: TObject);
var
  nKeySize : Integer;
begin
  // no seed bytes until now
  m_nSeedBytesGot:=0;
  ProgressBar.Position:=m_nSeedBytesGot;

  // we need to check the size chooser every time
  with sizeChooser do begin

    // set the max. key size possible
    Max:=MAX_KEY_SIZE;

    // current keysize above this max. level?
    nKeySize:=m_config.GetIntegerOption('KEYSIZE');
    if (nKeySize > Max) then begin
      nKeySize:=Max;
      m_config.FixIntegerOption('KEYSIZE', nKeySize)
    end;
    Position:=nKeySize;
  end;
end;


procedure TMakeKeyDiskForm.FormResize(Sender: TObject);
const
  BTN_Y_GAP = 9;
  BTN_X_GAP = 10;
  BTN_SPACE = 8;
  HTI_SPACE = 8;
var
  nBtnY      : Integer;
  nHBLeft    : Integer;
  nCBLeft    : Integer;
  nHTIWidth  : Integer;
  nHTIHeight : Integer;
  nPBTop     : Integer;
  nPBWidth   : Integer;
begin
  // adjust some controls
  nBtnY:=ClientHeight - ProgressBar.Height - CancelBtn.Height - BTN_Y_GAP;
  nHBLeft:=ClientWidth - BTN_X_GAP - HelpBtn.Width;
  nCBLeft:=nHBLeft - BTN_SPACE - CancelBtn.Width;

  nHTIWidth:=ClientWidth - BTN_X_GAP - HowToInfo.Left;
  nHTIHeight:=nBtnY - HTI_SPACE - HowToInfo.Top;

  nPBTop:=ClientHeight - ProgressBar.Height;
  nPBWidth:=ClientWidth;

  // chance all controls at once to avoid bounce
  HelpBtn.Left:=nHBLeft;
  CancelBtn.Left:=nCBLeft;
  CancelBtn.Top:=nBtnY;
  HelpBtn.Top:=nBtnY;
  HowToInfo.Width:=nHTIWidth;
  HowToInfo.Height:=nHTIHeight;

  // at last do the progress bar (FIXME: bug in the VCL, when resizing very
  // fast the bottom-aligned progress bar vanishes)
  ProgressBar.Top:=nPBTop;
  ProgressBar.Width:=nPBWidth;
end;

procedure TMakeKeyDiskForm.FormCreate(Sender: TObject);
var
  cc       : TMakeKeyDiskFormCC;
  listener : TMakeKeyDiskFormStrResListener;
begin
  // get the configuration section
  cc:=TMakeKeyDiskFormCC.Create(Self);
  m_config:=_globals.GetCfg.GetSection(CONFIG_ID, cc);
  cc.Destroy;

  // set up the window's position and measurements
  Left:=m_config.GetIntegerOption('LEFT');
  Top:=m_config.GetIntegerOption('TOP');
  Width:=m_config.GetIntegerOption('WIDTH');
  Height:=m_config.GetIntegerOption('HEIGHT');

  // init. listener stuff
  listener:=TMakeKeyDiskFormStrResListener.Create(Self);
  _globals.GetSr.AddListener(listener);
  listener.ChangeStrings(_globals.GetSr);

  // set up some controls
  with progressBar do begin
    Min:=0;
    Max:=NUM_OF_SEEDBYTES;
    Step:=1;
  end;
  with sizeChooser do begin
    Increment:=8;     // bytes only, sorry
    Min:=MIN_KEY_SIZE;
  end;
end;

procedure TMakeKeyDiskForm.CancelBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TMakeKeyDiskForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  AddSeedData(@Key, SizeOf(Key));
end;

procedure TMakeKeyDiskForm.AddSeedData(pSeed : Pointer; nNumOfBytes : Integer);
var
  nRest   : Integer;
  nToCopy : Integer;
begin

  // how many seed byts are stil needed?
  nRest:=NUM_OF_SEEDBYTES - m_nSeedBytesGot;

  if (nRest < 0) then
    Exit;

  if (nRest > 0) then begin

    // more than we need?
    nToCopy:=nNumOfBytes;
    if (nToCopy > nRest) then
      nToCopy:=nRest;

    // copy the given seed data
    Move(pSeed^, m_seedBuffer[m_nSeedBytesGot], nToCopy);
    Inc(m_nSeedBytesGot, nToCopy);

    // buffer full?
    if (m_nSeedBytesGot = NUM_OF_SEEDBYTES) then begin

      // so set the seed (will be flushed later)
      _globals.GetRndMng.AddSeed(@m_seedBuffer, NUM_OF_SEEDBYTES);

      // (this seed data is some kind of secret)
      FillChar(m_seedBuffer, NUM_OF_SEEDBYTES, 0);

    end;
  end;

  ProgressBar.Position:=m_nSeedBytesGot;

  // ready?
  nRest:=NUM_OF_SEEDBYTES - m_nSeedBytesGot;
  if (nRest <=0) then begin

    // ok, we can start now
    Proceed;
  end;
end;


procedure TMakeKeyDiskForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  with m_config do begin
    FixIntegerOption('WIDTH', Width);
    FixIntegerOption('HEIGHT', Height);
    FixIntegerOption('LEFT', Left);
    FixIntegerOption('TOP', Top);
  end;
end;

// the mouse move seeders (only the lower 4 bits(!) of X and Y are used),
// all coordinates are made relatively to the parent window

procedure TMakeKeyDiskForm.AddXYSeed(nX, nY : Integer; ctrl : TControl = Nil);
var
  bSeed : Byte;
begin
  if (ctrl <> Nil) then begin
    Inc(nX, ctrl.Left);
    Inc(nY, ctrl.Top);
  end;
  bSeed:=Byte(((nY shl 4) or (nX and $0f)) and $00ff);
  AddSeedData(@bSeed, SizeOf(bSeed));
end;


procedure TMakeKeyDiskForm.HelpBtnMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  AddXYSeed(X, Y, HelpBtn);
end;

procedure TMakeKeyDiskForm.CancelBtnMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  AddXYSeed(X, Y, CancelBtn);
end;

procedure TMakeKeyDiskForm.ProgressBarMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  AddXYSeed(X, Y, ProgressBar);
end;

procedure TMakeKeyDiskForm.SizeViewMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  AddXYSeed(X, Y, SizeView);
end;

procedure TMakeKeyDiskForm.SizeInfoMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  AddXYSeed(X, Y, SizeInfo);
end;

procedure TMakeKeyDiskForm.SizeChooserMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  AddXYSeed(X, Y, SizeChooser);
end;

procedure TMakeKeyDiskForm.HowToInfoMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  AddXYSeed(X, Y, HowToInfo);
end;

procedure TMakeKeyDiskForm.FormMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  AddXYSeed(X, Y);
end;

procedure TMakeKeyDiskForm.Proceed;
var
  nKeySize    : Integer;
  blOverwrite : Boolean;
  sFileName   : String;
  sr          : TStrRes;
begin
  // let the user select a key file
  sr:=_globals.GetSr;
  with SaveDialog do begin
    Title:=sr.Get(CONFIG_ID, 'SELFILENAME');
    FileName:=m_sKeyFileName;
    Filter:=sr.Get(CONFIG_ID, 'FILTER');
    if (not Execute) then begin
      Close;
      Exit;
    end;
    sFileName:=FileName;
  end;

  // file already exists?
  blOverwrite:=FileExists(sFileName);
  if (blOverwrite) then begin
    if (Application.MessageBox(
          PChar(Format(sr.Get(CONFIG_ID, 'OVERWRITE'), [sFileName])),
          PChar(sr.Get(CONFIG_ID, 'WARNING')),
          MB_ICONEXCLAMATION or MB_YESNO) = IDNO) then
      Exit;
  end;

  // fix the new key size
  nKeySize:=SizeChooser.Position;
  m_config.FixIntegerOption('KEYSIZE', nKeySize);

  // create the key file
  if (TKeyDisk.CreateDisk(sFileName,
                          _globals.GetRndMng,
                          nKeySize shr 3,  // (need it in bytes)
                          blOverwrite)) then begin
    Application.MessageBox(
      PChar(Format(sr.Get(CONFIG_ID, 'CANNOTCREATE'), [sFileName])),
      PChar(sr.Get(CONFIG_ID, 'WARNING')),
      MB_ICONSTOP);
  end
  else begin
    Application.MessageBox(
      PChar(Format(sr.Get(CONFIG_ID, 'CREATED'),
                   [sFileName, nKeySize shr 3, nKeySize])),
      PChar(sr.Get(CONFIG_ID, 'SUCCESS')),
      MB_ICONINFORMATION);
  end;

  // close
  Close;
end;


procedure TMakeKeyDiskForm.SetKeyFileName(const sKeyFileName : String);
begin
  m_skeyFileName:=sKeyFileName;
end;


procedure TMakeKeyDiskForm.HelpBtnClick(Sender: TObject);
begin
  Application.HelpCommand(HELP_CONTEXT, 501);
end;

function TMakeKeyDiskForm.FormHelp(Command: Word; Data: Integer;
  var CallHelp: Boolean): Boolean;
begin
  CallHelp := False;
  Result:=HtmlHelp_ShowContext(Data);
end;

end.

