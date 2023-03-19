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
  the progress window with the implementations of several associated callbacks
}


unit ProgressWin;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, ExtCtrls, Buttons, ToolWin, ImgList,
  Configuration,
//  General,
  BFJob,
  BFAManager;


type
  TProgressForm = class(TForm)
    NameView: TPanel;
    InputFileInfo: TStaticText;
    OutputFileInfo: TStaticText;
    BarHolder: TPanel;
    SingleProgressBar: TProgressBar;
    AllProgressBar: TProgressBar;
    InfoHolder: TPanel;
    FileSizeInfo: TStaticText;
    FileNumberInfo: TStaticText;
    FileNumberSplitter: TStaticText;
    WipeLoopInfo: TStaticText;
    WipeLoopSplitter: TStaticText;
    ButtonHolder: TPanel;
    PauseBtn: TButton;
    MinimizeBtn: TButton;
    StopBtn: TButton;
    InputFileBox: TLabel;
    OutputFileBox: TLabel;
    PercentInfo: TStaticText;
    FileSizeBox: TLabel;
    FileNumberActBox: TLabel;
    FileNumberMaxBox: TLabel;
    WipeLoopActBox: TLabel;
    WipeLoopMaxBox: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PauseBtnClick(Sender: TObject);
    procedure StopBtnClick(Sender: TObject);
    procedure MinimizeBtnClick(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure ButtonHolderResize(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    //members
    m_config               : TConfigurationSection;
    m_job                  : TBFJob;
    m_nNumberCtrlsTop      : Integer;
    m_blPaused             : Boolean;
    m_blStopped            : Boolean;
    m_blSingleFileProgress : Boolean;
    m_parentForm           : TForm;
    m_saveParentWS         : TWindowState;

    procedure InvokeSetup;

  public
    // sets up the form
    // -> the parent form
    // -> current job
    // -> show progress for single file actions
    procedure Setup(parentForm : TForm;
                    job : TBFJob;
                    blSingleFileProgress : Boolean = True);

    // lays out the boxes
    procedure LayoutBoxes;

    // check the pause flag
    // <- True: user has paused the operation / False: no pause
    function IsPaused : Boolean;

    // check the stopped flag
    // <- True: user has stopped the operation / False: no stop
    function IsStopped : Boolean;

    // refresh the "Pause" button caption
    procedure RefreshPauseBtn;

    // resets all contols
    procedure ResetCtrls;

    // signals to close the dialog (and abort the current operation)
    // <- true: stopped / false: not, still running
    function RequestClose : Boolean;

    // (overridden for proper external closure)
    procedure Close;

  end;


// the progress callback

type
  TBFWorkProgressImpl = class(TBFAWorkProgress)
  public
    // constructor
    // -> progress form
    constructor Create(pf : TProgressForm); reintroduce; overload;

    // implemented callback
    // exception: ECallBackInterrupt if user break occured
    procedure CallBack; override;
  end;




var
  ProgressForm : TProgressForm;



implementation
uses
  StringRes,
  Options,
  StringPlusI,
  ShortcutChecker,
  BFManager,
  CallBack,
  ProgressCallBack,
  General,
  GlobalsGUI;

{$R *.DFM}


// polling time for pause(/continue) events, in millisecs
const
  PAUSE_POLLING_INT = 200;

// ultimate gap constant
const
  UNI_GAP = 8;

//////////////////////// TBFWorkProgressImpl ////////////////////////


constructor TBFWorkProgressImpl.Create(pf : TProgressForm);
begin
  inherited Create(pf);
end;


procedure TBFWorkProgressImpl.CallBack;
var
  nActPos        : Integer;
  nMaxPos        : Integer;
  nSignal        : Integer;
  nMode          : Integer;
  blIsOutputFile : Boolean;
  sTemp          : String;
begin
  with GetCallBackObj as TProgressForm do begin

    // look out for signals
    nSignal:=GetSignal;
    case nSignal of
      CALLBACK_SIGNAL_START : begin
        Visible:=True;
        Exit;
      end;
      CALLBACK_SIGNAL_STOP : begin
        Application.Restore;
        Close;
        Exit;
      end;
    end;

    // listen for events
    Application.ProcessMessages;

    // do first call stuff
    if (GetFirstCall) then begin
      GetIntLevels(nActPos, nMaxPos);
      AllProgressBar.Max:=nMaxPos;
      FileNumberMaxBox.Caption:=IntToStr(GetNumOfFiles) + ' ';
      PercentInfo.Caption:='';
    end;

    // do we have an output file?
    nMode:=GetMode;
    blIsOutputFile:=((nMode = BFM_PROGRESS_ENCRYPT) or
                     (nMode = BFM_PROGRESS_DECRYPT) or
                     (nMode = BFM_PROGRESS_REENCRYPT));

    // changed?
    if (GetChanged) then begin

      // yes, set all the new infos
      InputFileBox.Caption:=' ' + GetFileName;

      if (blIsOutputFile) then begin
        OutputFileBox.Caption:=' ' + GetOutputFileName;
      end;
      if (nMode <> BFM_PROGRESS_DESLACK) then
        FileSizeBox.Caption:=
          TStrPlusI.Sepa1000(_globals.GetSr, GetFileSize) + ' ';

      FileNumberActBox.Caption:=IntToStr(GetFileNumber) + ' ';
      GetIntBytesDone(nActPos, nMaxPos);
      SingleProgressBar.Max:=nMaxPos;

      // empty the wipe loop infos if we're not wiping
      if (nMode <> BFM_PROGRESS_WIPE) then begin
        WipeLoopActBox.Caption:='';
        WipeLoopMaxBox.Caption:='';
      end;
    end
    else if (GetWipeAfterEncryption) then begin
      // special case (until we know better): adjust for wipe after delete
      GetIntBytesDone(nActPos, nMaxPos);
      SingleProgressBar.Max:=nMaxPos;
    end;

    // update the wipe loop counters, if necessary
    if ((nMode = BFM_PROGRESS_WIPE) and (not GetDeleteOnly)) then begin
      if (GetNoWipeLoops) then begin
        // (a single wipe is shown as "1/1")
        WipeLoopActBox.Caption:='1 ';
        WipeLoopMaxBox.Caption:='1 ';
      end
      else begin
        WipeLoopActBox.Caption:=IntToStr(GetWipeLoop) + ' ';
        WipeLoopMaxBox.Caption:=IntToStr(GetNumOfWipes) + ' ';
      end;
    end;

    // update the progress bars
    if (not GetDeleteOnly) then begin
      GetIntBytesDone(nActPos, nMaxPos);
      SingleProgressBar.Position:=nActPos;
    end;

    GetIntLevels(nActPos, nMaxPos);

    // hackish safety check to avoid that the allprogress bar every bounces
    // back (since our progress code is not the cleanest right now)
    if (nActPos > AllProgressBar.Position) then
      AllProgressBar.Position:=nActPos;

    sTemp:=TStrPlusI.CalcPercent(_globals.GetSr,
                                 GetActPos,
                                 GetMaxPos,
                                 1) + '%';
    if (PercentInfo.Caption <> sTemp) then
      PercentInfo.Caption:=sTemp;

    // stopped?
    if (IsStopped) then
      raise ECallBackInterrupt.Create('interrupted');

    // paused?
    while (IsPaused) do begin

      // we poll every 0.2 secs for a user interaction, this is much simpler
      // than a special message sniffing thread and shouldn't increase the
      // processor load too much while being idle (FIMXE: a cool solution might
      // be an own message which is fired by the pause button event and
      // trapped right here)
      Application.ProcessMessages;

      // a stop request overrides a pause, of course
      if (IsStopped) then
        raise ECallBackInterrupt.Create('interrupted while paused');

      // wait
      Sleep(PAUSE_POLLING_INT);
    end;

    // enable the following line for slow-motion processing (nice for debugging) 
    //Sleep(1000);

  end;
end;


//////////////////////// TProgressForm ////////////////////////


// the configuration ID
const
  CONFIG_ID = 'ProgressForm';


// some default widths and heights
const
  DEF_WIDTH  = 360;

// resource listener
type
  TProgressFormStrResListener = class(TStrResListener)
  private
    m_theWin : TProgressForm;
  public
    constructor Create(theWin : TProgressForm);
    procedure ChangeStrings(sr : TStrRes); override;
  end;

constructor TProgressFormStrResListener.Create(theWin : TProgressForm);
begin
  m_theWin:=theWin;
end;


procedure TProgressFormStrResListener.ChangeStrings(sr : TStrRes);
var
  scc : TShortcutChecker;
begin
  scc:=TShortcutChecker.Create;
  with m_theWin, sr, scc do begin
    StopBtn.Caption       :=AddShortcut(Get(CONFIG_ID, 'STOPBTN'));
    MinimizeBtn.Caption   :=AddShortcut(Get(CONFIG_ID, 'MINIMIZEBTN'));
    PauseBtn.Caption      :=AddShortcut(Get(CONFIG_ID, 'PAUSEBTN'));
    FileSizeInfo.Caption  :=Get(CONFIG_ID, 'FILESIZEINFO');
    FileNumberInfo.Caption:=Get(CONFIG_ID, 'FILENUMBERINFO');
    WipeLoopInfo.Caption  :=Get(CONFIG_ID, 'WIPELOOPINFO');
    SingleProgressBar.Hint:=Get(CONFIG_ID, 'SINGLEPROGRESSBARHINT');
    AllProgressBar.Hint   :=Get(CONFIG_ID, 'ALLPROGRESSBARHINT');
  end;
  scc.Destroy;

  // in comparsion to other string listeners this one additionally
  // rearranges the controls because its the only code part with
  // can make changes at the right time (although not really necessary
  // because strings will be changed when the window's not possible,
  // so this saves only processor time)
  m_theWin.LayoutBoxes;
end;


// configuration checker
type
  TProgressFormCC = class(TConfigurationChecker)
  private
    m_parent : TProgressForm;
  public
    constructor Create(parent : TProgressForm);
    procedure RunCheck(section : TConfigurationSection); override;
  end;

constructor TProgressFormCC.Create(parent : TProgressForm);
begin
  m_parent:=parent;
end;

procedure TProgressFormCC.RunCheck(section : TConfigurationSection);
begin
  CheckInt(section, 'LEFT', (Screen.Width - m_parent.Width) shr 1, 0);
  CheckInt(section, 'TOP', (Screen.Height - m_parent.Height) shr 1, 0);
  CheckInt(section, 'WIDTH', DEF_WIDTH, 0);
end;


procedure TProgressForm.Setup(parentForm : TForm;
                              job : TBFJob;
                              blSingleFileProgress : Boolean = True);
begin
  m_parentForm:=parentForm;
  m_job:=job;
  m_blSingleFileProgress:=blSingleFileProgress;
end;


procedure TProgressForm.FormCreate(Sender: TObject);
var
  cc       : TProgressFormCC;
  listener : TProgressFormStrResListener;

begin

  // no parent form set until now
  m_parentForm:=Nil;

  // no job
  m_job:=Nil;
  m_blSingleFileProgress:=False;

  // we have to store that location for layout purposes
  m_nNumberCtrlsTop:=FileNumberMaxBox.Top;

  // get the configuration section
  cc:=TProgressFormCC.Create(Self);
  m_config:=_globals.GetCfg.GetSection(CONFIG_ID, cc);
  cc.Destroy;

  // set up the window measurements
  with m_config do begin
    Left  :=GetIntegerOption('LEFT');
    Top   :=GetIntegerOption('TOP');
    Width :=GetIntegerOption('WIDTH');
  end;

  // better to calculate the minimum width at runtime
  ButtonHolder.Constraints.MinWidth:=
    ((MinimizeBtn.Left - PauseBtn.Left) shl 1) +
    StopBtn.Width + PauseBtn.Left;

  // init. listener stuff
  listener:=TProgressFormStrResListener.Create(Self);
  _globals.GetSr.AddListener(listener);
  listener.ChangeStrings(_globals.GetSr);
end;

procedure TProgressForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  // store the window measurements
  with m_config do begin
    FixIntegerOption('LEFT', Left);
    FixIntegerOption('TOP', Top);
    FixIntegerOption('WIDTH', Width);
  end;

  // reenable the parent form
  if (m_parentForm <> Nil) then begin
    m_parentForm.Enabled:=True;
  end;

end;


procedure TProgressForm.FormResize(Sender: TObject);
var
  nTemp : Integer;
begin
  // rearrange or resize all necessary controls
  nTemp:=BarHolder.ClientWidth;
  Dec(nTemp, SingleProgressBar.Left shl 1);

  SingleProgressBar.Width:=nTemp;
  AllProgressBar.Width:=nTemp;

  PercentInfo.Left:=SingleProgressBar.Left + nTemp - PercentInfo.Width; 

  StopBtn.Left:=ButtonHolder.Width - StopBtn.Width - PauseBtn.Left;

  nTemp:=NameView.Width - InputFileBox.Left - PauseBtn.Left;

  InputFileBox.Width:=nTemp;
  OutputFileBox.Width:=nTemp;

  // (FIXME: this isn't necessary every time, so we check for visibility)
  if ((m_parentForm <> Nil) and Visible) then
    m_parentForm.WindowState:=m_saveParentWS;

end;


procedure TProgressForm.LayoutBoxes;
var
  nTemp           : Integer;
  nGap            : Integer;
  nShift          : Integer;
  nNewLeft        : Integer;
  nMaxCapLen      : Integer;
  blOutputVisible : Boolean;
  blWipeVisible   : Boolean;

// local helper to avoid negative values
function CheckNeg(nVal : Integer) : Integer;
begin
  if (nVal < 0) then
    nVal:=0;
  Result:=nVal;
end;

begin
  // get the some control visibilities
  blOutputVisible:=OutputFileInfo.Visible;
  blWipeVisible:=WipeLoopInfo.Visible;

  // search for the largest caption (thanks to the AutoSize property)
  nMaxCapLen:=0;
  if (InputFileInfo.Width > nMaxCapLen) then
    nMaxCapLen:=InputFileInfo.Width;
  if (blOutputVisible) then
    if (OutputFileInfo.Width > nMaxCapLen) then
      nMaxCapLen:=OutputFileInfo.Width;
  if (FileSizeInfo.Width > nMaxCapLen) then
    nMaxCapLen:=FileSizeInfo.Width;
  if (FileNumberInfo.Width > nMaxCapLen) then
    nMaxCapLen:=FileNumberInfo.Width;
   if (blWipeVisible) then
    if (WipeLoopInfo.Width > nMaxCapLen) then
      nMaxCapLen:=WipeLoopInfo.Width;

  // now shift the boxes left or right
  nNewLeft:=(UNI_GAP shl 1) + nMaxCapLen;

  // (the file name boxes need a special treatment)
  nShift:=InputFileBox.Left - nNewLeft;

  //nTemp:=InputFileBox.Width + nShift;
  //InputFileBox.Width:=CheckNeg(nTemp);
  InputFileBox.Left:=nNewLeft;

  //OutputFileBox.Width:=CheckNeg(nTemp);
  OutputFileBox.Left:=nNewLeft;

  // (the all others are easy)
  FileSizeBox.Left:=FileSizeBox.Left - nShift;
  FileNumberActBox.Left:=FileNumberActBox.Left - nShift;
  FileNumberSplitter.Left:=FileNumberSplitter.Left - nShift;
  FileNumberMaxBox.Left:=FileNumberMaxBox.Left - nShift;

  WipeLoopActBox.Left:=WipeLoopActBox.Left - nShift;
  WipeLoopSplitter.Left:=WipeLoopSplitter.Left - nShift;
  WipeLoopMaxBox.Left:=WipeLoopMaxBox.Left - nShift;

  // move the counter controls up, if the file sizes are not visible
  nGap:=WipeLoopInfo.Height + UNI_GAP;

  if (FileSizeBox.Visible) then
    nTemp:=m_nNumberCtrlsTop
  else
    nTemp:=0;

  FileNumberActBox.Top:=nTemp;
  FileNumberSplitter.Top:=nTemp;
  FileNumberMaxBox.Top:=nTemp;
  FileNumberInfo.Top:=nTemp;

  Inc(nTemp, nGap);

  WipeLoopActBox.Top:=nTemp;
  WipeLoopSplitter.Top:=nTemp;
  WipeLoopMaxBox.Top:=nTemp;
  WipeLoopInfo.Top:=nTemp;

  // update constraints
  if (blOutputVisible) then
    nTemp:=OutputFileBox.Top + (OutputFileBox.Top - InputFileBox.Top)
  else
    nTemp:=OutputFileBox.Top;

  NameView.Constraints.MinHeight:=nTemp;
  NameView.Constraints.MaxHeight:=nTemp;
  NameView.Height:=nTemp;

  if (blWipeVisible) then
    nTemp:=WipeLoopActBox.Top + (WipeLoopActBox.Top - FileNumberMaxBox.Top)
  else
    nTemp:=WipeLoopActBox.Top;

  InfoHolder.Constraints.MinHeight:=nTemp;
  InfoHolder.Constraints.MaxHeight:=nTemp;

  // move the percentage to the new position on the right
  PercentInfo.Top:=nTemp - PercentInfo.Height - UNI_GAP;

  // the progress bars might be different, too
  if m_blSingleFileProgress then begin
    SingleProgressBar.Visible:=True;
    AllProgressBar.Top:=SingleProgressBar.Top + SingleProgressBar.Height +
        UNI_GAP;
  end
  else begin
    SingleProgressBar.Visible:=False;
    AllProgressBar.Top:=SingleProgressBar.Top;
  end;
  nTemp:=AllProgressBar.Top + AllProgressBar.Height;
  BarHolder.Constraints.MinHeight:=nTemp;
  BarHolder.Constraints.MaxHeight:=nTemp;

  // a little trick to enforce the contraints (avoids jumps)
  Height:=1;

  FormResize(Nil);
end;


procedure TProgressForm.InvokeSetup;
var
  sTemp            : String;
  blShowWipeCrtls  : Boolean;
  blShowOutputFile : Boolean;
  blShowFileSize   : Boolean;
  fontStyle        : TFontStyles;
  colorForeground  : TColor;
  colorBackground  : TColor;
begin

  // best place store the parent form's state
  if (m_parentForm <> Nil) then
    m_saveParentWS:=m_parentForm.WindowState;

  // set the layout, dependant on the mode
  case m_job.GetMode of
    BFJOB_MODE_ENCRYPT : begin
      sTemp:=_globals.GetSr.Get(CONFIG_ID, 'MODE_ENCRYPT');
      blShowWipeCrtls:=m_job.WipingActive;
      blShowOutputFile:=True;
      blShowFileSize:=True;
    end;

    BFJOB_MODE_DECRYPT : begin
      sTemp:=_globals.GetSr.Get(CONFIG_ID, 'MODE_DECRYPT');
      blShowWipeCrtls:=False;
      blShowOutputFile:=True;
      blShowFileSize:=True;
    end;

    BFJOB_MODE_WIPE : begin
      sTemp:=_globals.GetSr.Get(CONFIG_ID, 'MODE_WIPE');
      blShowWipeCrtls:=m_job.WipingActive; // deleteonly is not wiping at all...
      blShowOutputFile:=False;
      blShowFileSize:=m_job.WipingActive;  //...and doesn't offer a file size
    end;

    BFJOB_MODE_REENCRYPT : begin
      sTemp:=_globals.GetSr.Get(CONFIG_ID, 'MODE_REENCRYPT');
      blShowWipeCrtls:=False;
      blShowOutputFile:=False;
      blShowFileSize:=True;
    end;

    BFJOB_MODE_DESLACK : begin
      sTemp:=_globals.GetSr.Get(CONFIG_ID, 'MODE_DESLACK');
      blShowWipeCrtls:=False;
      blShowOutputFile:=False;
      blShowFileSize:=False;
    end;
  else
    // (this should never happen)
    sTemp:='';
    blShowWipeCrtls:=False;
    blShowOutputFile:=False;
    blShowFileSize:=False;
  end;

  // set the right caption
  Caption:=sTemp;

  // set visibility of certain controls
  WipeLoopInfo.Visible:=blShowWipeCrtls;
  WipeLoopActBox.Visible:=blShowWipeCrtls;
  WipeLoopSplitter.Visible:=blShowWipeCrtls;
  WipeLoopMaxBox.Visible:=blShowWipeCrtls;
  OutputFileInfo.Visible:=blShowOutputFile;
  OutputFileBox.Visible:=blShowOutputFile;
  FileSizeInfo.Visible:=blShowFileSize;
  FileSizeBox.Visible:=blShowFileSize;

  // set the (input) file info caption(s)
  with _globals.GetSr do
    if (blShowOutputFile) then begin
      InputFileInfo.Caption :=Get(CONFIG_ID, 'INPUTFILEINFO');
      OutputFileInfo.Caption:=Get(CONFIG_ID, 'OUTPUTFILEINFO');
    end
    else begin
      InputFileInfo.Caption :=Get(CONFIG_ID, 'CURRENTFILEINFO');
    end;

  // color the display...

  if (_globals.GetOpts
              .GetCfg
              .GetBooleanOption(OPTIONS_CFGID_COLORPROGRESS)) then begin

    fontStyle:=[fsBold];

    case m_job.GetMode of
      BFJOB_MODE_ENCRYPT : begin
        colorForeground:=clWhite;
        colorBackground:=clRed;
      end;

      BFJOB_MODE_DECRYPT : begin
        colorForeground:=clWhite;
        colorBackground:=clGreen;
      end;

      BFJOB_MODE_WIPE : begin
        colorForeground:=clBlack;
        colorBackground:=clYellow;
      end;

      BFJOB_MODE_REENCRYPT : begin
        colorForeground:=clAqua;
        colorBackground:=clNavy;
      end;

      BFJOB_MODE_DESLACK : begin
        colorForeground:=clWhite;
        colorBackground:=clTeal;
      end;

    else
      // this should never happen
      colorForeground:=clWindowText;
      colorBackground:=clBtnFace;
    end;
  end
  else begin
    fontStyle:=[];
    colorForeground:=clWindowText;
    colorBackground:=clWindow;
  end;

  InputFileBox.Font.Color:=colorForeground;
  InputFileBox.Font.Style:=fontStyle;
  InputFileBox.Color:=colorBackground;

  OutputFileBox.Font.Color:=colorForeground;
  OutputFileBox.Font.Style:=fontStyle;
  OutputFileBox.Color:=colorBackground;

  FileSizeBox.Font.Color:=colorForeground;
  FileSizeBox.Font.Style:=fontStyle;
  FileSizeBox.Color:=colorBackground;

  FileNumberActBox.Font.Color:=colorForeground;
  FileNumberActBox.Font.Style:=fontStyle;
  FileNumberActBox.Color:=colorBackground;

  FileNumberMaxBox.Font.Color:=colorForeground;
  FileNumberMaxBox.Font.Style:=fontStyle;
  FileNumberMaxBox.Color:=colorBackground;

  WipeLoopActBox.Font.Color:=colorForeground;
  WipeLoopActBox.Font.Style:=fontStyle;
  WipeLoopActBox.Color:=colorBackground;

  WipeLoopMaxBox.Font.Color:=colorForeground;
  WipeLoopMaxBox.Font.Style:=fontStyle;
  WipeLoopMaxBox.Color:=colorBackground;

  // layout the boxes
  LayoutBoxes;
end;


function TProgressForm.IsPaused : Boolean;
begin
  Result:=m_blPaused;
end;

function TProgressForm.IsStopped : Boolean;
begin
  Result:=m_blStopped;
end;


procedure TProgressForm.FormShow(Sender: TObject);
var
  blShowHints : Boolean;
begin
  // now change the controls
  InvokeSetup;

  // reset the paused and stop flag
  m_blStopped:=False;
  m_blPaused:=False;
  RefreshPauseBtn;

  // reset all viewers
  ResetCtrls;

  // get the parent form state, then disable it
  if (m_parentForm <> Nil) then begin
    m_saveParentWS:=m_parentForm.WindowState;
    m_parentForm.Enabled:=False;
  end;

  // enable/disable hints
  blShowHints:=_globals.GetOpts.GetCfg
                               .GetBooleanOption(OPTIONS_CFGID_SHOWHINTS);
  StopBtn.ShowHint:=blShowHints;
  MinimizeBtn.ShowHint:=blShowHints;
  PauseBtn.ShowHint:=blShowHints;
  SingleProgressBar.ShowHint:=blShowHints;
  AllProgressBar.ShowHint:=blShowHints;

  // focus to the emergency button
  StopBtn.SetFocus;
end;


procedure TProgressForm.RefreshPauseBtn;
var
  scc : TShortcutChecker;
begin
  scc:=TShortcutChecker.Create;
  with _globals.GetSr, scc do begin
    StopBtn.Caption:=AddShortcut(Get(CONFIG_ID, 'STOPBTN'));
    MinimizeBtn.Caption:=AddShortcut(Get(CONFIG_ID, 'MINIMIZEBTN'));
    if (m_blPaused) then
      PauseBtn.Caption:=AddShortcut(Get(CONFIG_ID, 'CONTBTN'))
    else
      PauseBtn.Caption:=AddShortcut(Get(CONFIG_ID, 'PAUSEBTN'));
  end;
  scc.Destroy;
end;


procedure TProgressForm.PauseBtnClick(Sender: TObject);
begin
  // switch the pause state
  m_blPaused:=not m_blPaused;
  RefreshPauseBtn;
end;

procedure TProgressForm.StopBtnClick(Sender: TObject);
begin
  RequestClose;
end;


procedure TProgressForm.MinimizeBtnClick(Sender: TObject);
begin
  // minimize the whole application
  Application.Minimize;
end;


procedure TProgressForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
  // map [Esc] to the stop buton
  if (Key = #27) then begin
    StopBtnClick(Sender);
    Key:=#0;
  end;
end;


procedure TProgressForm.ButtonHolderResize(Sender: TObject);
begin
  StopBtn.Left:=ButtonHolder.ClientWidth - StopBtn.Width - PauseBtn.Left;
end;


procedure TProgressForm.ResetCtrls;
begin
  InputFileBox.Caption:='';
  OutputFileBox.Caption:='';
  FileSizeBox.Caption:='';
  FileNumberActBox.Caption:='';
  FileNumberMaxBox.Caption:='';
  WipeLoopActBox.Caption:='';
  WipeLoopMaxBox.Caption:='';
  SingleProgressBar.Position:=0;
  AllProgressBar.Position:=0;
end;


procedure TProgressForm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  CanClose:=RequestClose;
end;


function TProgressForm.RequestClose : Boolean;
begin
  Result:=m_blStopped;
  with _globals.GetSr do
    if (not m_blStopped) then begin
      // let the stop be confirmed
      if (Application.MessageBox(PChar(Get(CONFIG_ID, 'BREAKQUEST')),
                                 PChar(Get(CONFIG_ID, 'CONFIRM')),
                                 MB_ICONQUESTION or MB_YESNO) = IDYES) then
        m_blStopped:=True;
    end;
end;


procedure TProgressForm.Close;
begin
  // we need to assume that external stops have already done the cleanup
  m_blStopped:=True;
  inherited;  
end;


end.
