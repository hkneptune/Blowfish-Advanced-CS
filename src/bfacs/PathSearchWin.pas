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
  implementation of a path serarch progress view and its associated callback,
  does a very fast drawing by direct graphic output (instead of changing
  a label caption)
}

unit PathSearchWin;

interface
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls,
  Configuration,
  PathSearch;

type
  TPathSearchForm = class(TForm)
    PaintBox: TPaintBox;
    CancelBtn: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure PaintBoxPaint(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
  private
    // members
    m_config     : TConfigurationSection;
    m_sFileName  : String;
    m_blCanceled : Boolean;
    m_parentForm : TForm;
    m_outRect    : TRect;

  public

    // sets the parent form
    // -> the parent form
    procedure Setup(parentForm : TForm);

    // shows the current file name
    // -> the file name to show
    procedure ShowFilename(const sFileName : String);

    // to check if the cancel button was pressed
    // <- True: canceled / False. not, continue
    function WasCanceled : Boolean;
  end;


var
  PathSearchForm : TPathSearchForm;


// the callback
type
  TPathSearchCBImpl = class(TPathSearchCallBack)
  public
    // constructor
    // -> the path search window
    constructor Create(psf : TPathSearchForm); reintroduce; overload;

    // callback
    // exception: ECallBackInterrupt if the user clicked on the cancel button
    procedure CallBack; override;

  end;


implementation
uses
  CallBack,
  GlobalsGUI,
  Options,
  StringRes,
  ShortCutChecker;

{$R *.DFM}



//////////////////////////// TPathSearchCBImpl ////////////////////////////

constructor TPathSearchCBImpl.Create(psf : TPathSearchForm);
begin
  inherited Create(psf);
end;


procedure TPathSearchCBImpl.CallBack;
var
  nSignal : Integer;
begin
  with GetCallBackObj as TPathSearchForm do begin

    // startup or shutdown?
    nSignal:=GetSignal;
    case nSignal of
      CALLBACK_SIGNAL_START : Show;   // pretty simple, huh?
      CALLBACK_SIGNAL_STOP  : Close;
    else
      // just show the file name
      ShowFileName(GetActFile);
      Application.ProcessMessages;
      if (WasCanceled) then
        raise ECallBackInterrupt.Create('cancel pressed');
    end;
  end;
end;



//////////////////////////// TPathSearchForm ////////////////////////////


// the configuration ID
const
  CONFIG_ID = 'PATHSEARCHFORM';


// resource listener
type
  TPathSearchFormStrResListener = class(TStrResListener)
  private
    m_theWin : TPathSearchForm;
  public
    constructor Create(theWin : TPathSearchForm);
    procedure ChangeStrings(sr : TStrRes); override;
  end;

constructor TPathSearchFormStrResListener.Create(theWin : TPathSearchForm);
begin
  m_theWin:=theWin;
end;


procedure TPathSearchFormStrResListener.ChangeStrings(sr : TStrRes);
var
  scc : TShortcutChecker;
begin
  scc:=TShortcutChecker.Create;
  with m_theWin, sr, scc do begin
    Caption:=Get(CONFIG_ID, 'CAPTION');
    CancelBtn.Caption  :=AddShortcut(Get(CONFIG_ID, 'CANCELBTN'));
  end;
  scc.Destroy;
end;


// configuration checker
type
  TPathSearchFormCC = class(TConfigurationChecker)
  private
    m_parent : TPathSearchForm;
  public
    constructor Create(parent : TPathSearchForm);
    procedure RunCheck(section : TConfigurationSection); override;
  end;

constructor TPathSearchFormCC.Create(parent : TPathSearchForm);
begin
  m_parent:=parent;
end;

procedure TPathSearchFormCC.RunCheck(section : TConfigurationSection);
begin
  CheckInt(section, 'LEFT', (Screen.Width - m_parent.Width) shr 1);
  CheckInt(section, 'TOP', (Screen.Height - m_parent.Height) shr 1);
end;


procedure TPathSearchForm.FormCreate(Sender: TObject);
var
  cc       : TPathSearchFormCC;
  listener : TPathSearchFormStrResListener;
begin
  // put the cancel button in the middle
  CancelBtn.Left:=(ClientWidth - CancelBtn.Width) shr 1;

  // get the configuration section
  cc:=TPathSearchFormCC.Create(Self);
  m_config:=_globals.GetCfg.GetSection(CONFIG_ID, cc);
  cc.Destroy;

  // set up the window measurements
  with m_config do begin
    Left:=GetIntegerOption('LEFT');
    Top:=GetIntegerOption('TOP');
  end;

  // init. listener stuff
  listener:=TPathSearchFormStrResListener.Create(Self);
  _globals.GetSr.AddListener(listener);
  listener.ChangeStrings(_globals.GetSr);

  // preset the output rectangle for the messages
  with m_outRect do begin
    Left:=0;
    Top:=0;
    Right:=PaintBox.Width;
    Bottom:=PaintBox.Height;
  end;
end;

procedure TPathSearchForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  // store the window measurements
  with m_config do begin
    FixIntegerOption('LEFT', Left);
    FixIntegerOption('TOP', Top);
  end;

  // reenable the parent form
  m_parentForm.Enabled:=True;
end;

procedure TPathSearchForm.ShowFilename(const sFileName : String);
begin
  // copy the file name for the case of a possible redraw
  m_sFileName:=sFileName;

  // draw it
  PaintBoxPaint(Nil);
end;


procedure TPathSearchForm.PaintBoxPaint(Sender: TObject);
begin
  // FIXME: the "speed" might be improved by using TextOut() and deleting
  //        former messag parts which were longer than the current one
  PaintBox.Canvas.TextRect(m_outRect, 0, 0, m_sFileName);
end;

procedure TPathSearchForm.FormShow(Sender: TObject);
begin

  m_blCanceled:=False;
  m_sFileName:='';

  // disable the parent form
  m_parentForm.Enabled:=False;
end;

procedure TPathSearchForm.CancelBtnClick(Sender: TObject);
begin
  m_blCanceled:=True;
end;

function TPathSearchForm.WasCanceled : Boolean;
begin
  Result:=m_blCanceled;
end;

procedure TPathSearchForm.Setup(parentForm : TForm);
begin
  m_parentForm:=parentForm;
end;


end.
