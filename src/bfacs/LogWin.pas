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

unit LogWin;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, ExtCtrls, StdCtrls,
  Configuration, BFJob, IntLists, ImgList, Menus;

type
  TLogForm = class(TForm)
    ButtonHolder: TPanel;
    SaveBtn: TButton;
    MakeJobFileBtn: TButton;
    OKBtn: TButton;
    HelpBtn: TButton;
    ViewHolder: TPanel;
    ListHolder: TPageControl;
    LogImages: TImageList;
    SaveDialog: TSaveDialog;
    LogMenu: TPopupMenu;
    LM_Sort: TMenuItem;
    procedure ButtonHolderResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure OKBtnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure SaveBtnClick(Sender: TObject);
    procedure MakeJobFileBtnClick(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure LM_SortClick(Sender: TObject);
    procedure SaveDialogShow(Sender: TObject);
    procedure SaveDialogClose(Sender: TObject);

  protected
    procedure ClearViews;

  public
    // sets report & job to view
    // -> the report (ref. only, must be kept alive while the dialog is shown)
    // -> the job (only if it's a single report)
    // <- True: report successfully set / False: there wasn't enough data to
    //    show anything (at last an error message might have been presented)
    function Setup(report : TBFJobReport;
                   job : TBFJob = Nil) : Boolean;

  private
    m_config : TConfigurationSection;
    m_report : TBFJobReport;
    m_job    : TBFJob;
    m_lists  : TList;
  end;

var
  LogForm: TLogForm;

implementation
uses
  WorkResults,
  Globals,
  StringRes,
  StringPlus,
  General,
  Options,
  ShortCutChecker;



{$R *.DFM}


//////////////////////////// TLogForm ////////////////////////////


// the configuration ID
const
  CONFIG_ID = 'LOGFORM';

// default measurements
const
  DEF_WIDTH  = 440;
  DEF_HEIGHT = 320;

// log images indexes
const
  LOGIMG_ERROR   = 2;
  LOGIMG_WARNING = 1;
  LOGIMG_SKIP    = 0;


// resource listener
type
  TLogFormStrResListener = class(TStrResListener)
  private
    m_theWin : TLogForm;
  public
    constructor Create(theWin : TLogForm);
    procedure ChangeStrings(sr : TStrRes); override;
  end;

constructor TLogFormStrResListener.Create(theWin : TLogForm);
begin
  m_theWin:=theWin;
end;


procedure TLogFormStrResListener.ChangeStrings(sr : TStrRes);
var
  scc : TShortcutChecker;
begin
  scc:=TShortcutChecker.Create;
  with m_theWin, sr, scc do begin
    Caption:=Get(CONFIG_ID, 'CAPTION');
    SaveBtn.Caption       :=AddShortcut(Get(CONFIG_ID, 'SAVEBTN'));
    MakeJobFileBtn.Caption:=AddShortcut(Get(CONFIG_ID, 'MAKEJOBFILEBTN'));
    OKBtn.Caption         :=AddShortcut(Get(CONFIG_ID, 'OKBTN'));
    HelpBtn.Caption       :=AddShortcut(Get(CONFIG_ID, 'HELPBTN'));
    LM_Sort.Caption       :=Get(CONFIG_ID, 'LM_SORT');
  end;
  scc.Destroy;
end;


// configuration checker
type
  TLogFormCC = class(TConfigurationChecker)
  private
    m_parent : TLogForm;
  public
    constructor Create(parent : TLogForm);
    procedure RunCheck(section : TConfigurationSection); override;
  end;

constructor TLogFormCC.Create(parent : TLogForm);
begin
  m_parent:=parent;
end;

procedure TLogFormCC.RunCheck(section : TConfigurationSection);
begin
  CheckInt(section, 'LEFT', (Screen.Width - m_parent.Width) shr 1, 0);
  CheckInt(section, 'TOP', (Screen.Height - m_parent.Height) shr 1, 0);
  CheckInt(section, 'WIDTH', DEF_WIDTH, 0);
  CheckInt(section, 'HEIGHT', DEF_HEIGHT, 0);
  CheckInt(section, 'SAVEDLG_WIDTH', 10, 0);
  CheckInt(section, 'SAVEDLG_HEIGHT', 10, 0);
  CheckBool(section, 'MAXIMIZED', False);
  CheckString(section, 'LAST_SAVEDIR_REPORT');
  CheckString(section, 'LAST_SAVEDIR_JOB');
end;


procedure TLogForm.ButtonHolderResize(Sender: TObject);
var
  nGap           : Integer;
  nLeft          : Integer;
  nMinLeft       : Integer;
  nSmallBtnWidth : Integer;
begin
  // rearrange some buttons
  nSmallBtnWidth:=SaveBtn.Width;
  nGap:=MakeJobFileBtn.Left - SaveBtn.Left - nSmallBtnWidth;

  nLeft:=ButtonHolder.ClientWidth - (nSmallBtnWidth shl 1) - (nGap shl 1);
  nMinLeft:=MakeJobFileBtn.Left + MakeJobFileBtn.Width + nGap;
  if (nLeft < nMinLeft) then
    nLeft:=nMinLeft;

  OKBtn.Left:=nLeft;
  HelpBtn.Left:=nLeft + nSmallBtnWidth + nGap;
end;

procedure TLogForm.FormCreate(Sender: TObject);
var
  cc       : TLogFormCC;
  listener : TLogFormStrResListener;
begin
  // get the configuration section
  cc:=TLogFormCC.Create(Self);
  m_config:=_globals.GetCfg.GetSection(CONFIG_ID, cc);
  cc.Destroy;

  // set up the window measurements
  with m_config do begin
    Left  :=GetIntegerOption('LEFT');
    Top   :=GetIntegerOption('TOP');
    Width :=GetIntegerOption('WIDTH');
    Height:=GetIntegerOption('HEIGHT');
    if (GetBooleanOption('MAXIMIZED')) then
      WindowState:=wsMaximized
    else
      WindowState:=wsNormal;
  end;

  // init. listener stuff
  listener:=TLogFormStrResListener.Create(Self);
  _globals.GetSr.AddListener(listener);
  listener.ChangeStrings(_globals.GetSr);

  m_lists:=Nil;
end;

procedure TLogForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  // store the window measurements
  with m_config do begin
    if (WindowState = wsNormal) then begin
      FixIntegerOption('LEFT', Left);
      FixIntegerOption('TOP', Top);
      FixIntegerOption('WIDTH', Width);
      FixIntegerOption('HEIGHT', Height);
      FixBooleanOption('MAXIMIZED', False);
    end
    else begin
      FixBooleanOption('MAXIMIZED', True);
    end;
  end;

  // don't waist any memory
  ClearViews;
end;

procedure TLogForm.OKBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TLogForm.FormShow(Sender: TObject);
begin

  // standard focus on the ok button
  OKBtn.SetFocus;
end;

procedure TLogForm.ClearViews;
begin
  while (ListHolder.PageCount > 0) do
    ListHolder.Pages[0].Destroy;

  if (m_lists <> Nil) then begin
    m_lists.Destroy;
    m_lists:=Nil;
  end;
end;


function TLogForm.Setup(report : TBFJobReport;
                        job : TBFJob) : Boolean;
var
  nI, nJ, nK   : Integer;
  nDim         : Integer;
  nUpIdx       : Integer;
  nErrors      : Integer;
  nWarnings    : Integer;
  nClientWidth : Integer;
  blConflicts  : Boolean;
  sTemp        : String;
  actRes       : TWorkResults;
  actList      : TListView;
  actBar       : TStatusBar;
  actSheet     : TTabSheet;
  newCol       : TListColumn;
  newItem      : TListItem;
  colInfos     : TWorkResultsColumnInfos;
  rows         : array of String;

procedure AddSummaryItem(span : TStatusPanel;
                         sItem : String;
                         blCenter : Boolean = True);
begin
  with span do begin
    if (not blCenter) then
      sItem:=' ' + sItem;
    Text:=sItem;
    if (blCenter) then
      Alignment:=taCenter;
    // (adjust the panel item width to the text it contains)
    Width:=Canvas.TextExtent(sItem).cx + 10;
  end;
end;

begin
  ClearViews;

  // something to show?
  Result:=False;
  if (report.IsVoid) then
    Exit;

  // no job, one message, one beer? (to handle this special case)
  with report do begin
    nI:=0;
    while (nI < GetNumOfJobs) do begin
      if (GetResults(0) = Nil) then
        Break;
      Inc(nI);
    end;
    if (nI < GetNumOfJobs) then begin
      if (GetNumOfJobs = 1) then begin
        Application.MessageBox(PChar(GetSummary(0).GetInfo),
                               PROGRAM_NAME, // (we don't know anything better)
                               MB_ICONSTOP);
      end;
      Exit;
    end;
  end;

  // now start to render
  Result:=True;
  m_report:=report;
  nErrors:=0;
  nWarnings:=0;
  with report do begin

    m_lists:=TList.Create;

    for nI:=0 to (GetNumOfJobs - 1) do begin

      actRes:=GetResults(nI);

      if (actRes <> Nil) then begin

        // get errors and warnings
        Inc(nErrors, actRes.GetNumOfErrors);
        Inc(nWarnings, actRes.GetNumOfWarnings);

        // create a new sheet
        actSheet:=TTabSheet.Create(Self);

        actSheet.PageControl:=ListHolder;
        sTemp:=GetSummary(nI).GetTitle;
        if (sTemp = '') then
          actSheet.Caption:=TBFJob.MakeDefaultTitle(_globals.GetSr,
                                                    GetSummary(nI).GetMode)
        else
          actSheet.Caption:=sTemp;

        // the summary is shown in a statusbar
        actBar:=TStatusBar.Create(Self);
        with actBar do begin
          Parent:=actSheet;
          SimplePanel:=False;
          Color:=clWindow;

          with Panels, GetSummary(nI) do begin
            nJ:=0;
            if (GetFiles <> '') then begin
              Add;
              AddSummaryItem(Items[nJ], GetFiles);
              Inc(nJ);
            end;
            if (GetTime <> '') then begin
              Add;
              AddSummaryItem(Items[nJ], GetTime);
              Inc(nJ);
            end;
            if (GetInfo <> '') then begin  // (always info, thus obsolete?)
              Add;
              AddSummaryItem(Items[nJ], GetInfo, False);
             // Inc(nJ);
            end;
          end;
        end;

        // the work results (if existing) are going to be put in a list
        actList:=TListView.Create(actSheet);
        m_lists.Add(actList);

        // any conflicts occured?
        blConflicts:=((actRes.GetNumOfErrors > 0) or
                      (actRes.GetNumOfWarnings > 0) or
                      (actRes.GetNumOfSkips > 0));

        LM_Sort.Enabled:=blConflicts;

        with actList do begin
          Parent:=actSheet;
          Align:=alClient;
          ViewStyle:=vsReport;
          GridLines:=True;
          ColumnClick:=False;
          ReadOnly:=True;
          RowSelect := True;
          if (blConflicts) then
            SmallImages:=LogImages;
         // BorderStyle:=bsNone;
        end;

        // make rows
        nDim:=actRes.GetDimension;
        SetLength(rows, nDim);
        nUpIdx:=actRes.GetCount - 1;
        actList.Items.BeginUpdate;
        for nJ:=0 to nUpIdx do begin
          newItem:=actList.Items.Add;
          if (blConflicts) then begin
            case actRes.GetEntryType(nJ) of
              WORKRESULTS_TYPE_ERROR   : nK:=LOGIMG_ERROR;
              WORKRESULTS_TYPE_WARNING : nK:=LOGIMG_WARNING;
              WORKRESULTS_TYPE_SKIP    : nK:=LOGIMG_SKIP;
       //       WORKRESULTS_TYPE_BREAK   : nK:=LOGIMG_ERROR;
            else
              nK:=-1;
            end;
            newItem.ImageIndex:=nK;
          end;

          actres.Render(nJ, rows);

          newItem.Caption:=rows[0];
          newItem.SubItems.BeginUpdate;

          for nK:=1 to (nDim - 1) do
            newItem.SubItems.Add(rows[nK]);

          newItem.SubItems.EndUpdate;
        end;
        actList.Items.EndUpdate;

        // create columns
        nClientWidth:=actList.ClientWidth;

     //   debd('actList.VisibleRowCount', actList.VisibleRowCount);
     //   debd('actRes.GetCount', actRes.GetCount);

        if (actList.VisibleRowCount <= actRes.GetCount) then begin
          // (perhaps a scrollbar is visible, reducing the client width)
          Dec(nClientWidth, GetSystemMetrics(SM_CXVSCROLL));
        end;
        colInfos:=actRes.GetColInfos;
        nUpIdx:=nDim - 1;
        actList.Columns.BeginUpdate;
        for nJ:=0 to nUpIdx do begin
          newCol:=actList.Columns.Add;
          newCol.Caption:=colInfos.GetCaption(nJ);
          case colInfos.GetAlignment(nJ) of
            WORKRESULTS_COLALIGN_LEFT   : newCol.Alignment:=taLeftJustify;
            WORKRESULTS_COLALIGN_RIGHT  : newCol.Alignment:=taRightJustify;
            WORKRESULTS_COLALIGN_MIDDLE : newCol.Alignment:=taCenter;
          end;
          newCol.Width:=(nClientWidth *
                         colInfos.GetWidthPercentage(nJ)) div 100;
        end;
        actList.Columns.EndUpdate;
        colInfos.Destroy;

      end;

    end;

  end;  (* OF WITH *)
  Finalize(rows);

  // show the overall error and warning counts, if any
  sTemp:=_globals.GetSr.Get(CONFIG_ID, 'CAPTION');
  if ((nErrors > 0) or (nWarnings > 0)) then begin
    Caption:=sTemp + Format(_globals.GetSr.Get(CONFIG_ID, 'ERRWAR'),
                            [nErrors, nWarnings]);
  end
  else begin
    Caption:=sTemp;
  end;

  // take over the job
  m_job:=job;

  // job files can only be created with a single result
  MakeJobFileBtn.Enabled:=(job <> Nil);
end;




procedure TLogForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
  // ESC closes
  if (Key = #27) then begin
    Key:=#0;
    Close;
  end;

end;

procedure TLogForm.SaveBtnClick(Sender: TObject);
var
  blAppend : Boolean;
  sr       : TStrRes;
begin
  // save the report into a file
  sr:=_globals.GetSr;

  with SaveDialog do begin
    Title:=sr.Get(CONFIG_ID, 'SD_REP_TITLE');
    Filter:=sr.Get(CONFIG_ID, 'SD_REP_FILTER');
    InitialDir:=m_config.GetStringOption('LAST_SAVEDIR_REPORT');
    if (not Execute) then
      Exit;
    m_config.FixStringOption('LAST_SAVEDIR_REPORT',
                             ExtractFilePath(FileName));

    if (FileExists(FileName)) then begin

      case (Application.MessageBox(
              PChar(Format(sr.Get(CONFIG_ID, 'ALREADYEXISTS'), [FileName])),
              PChar(sr.Get(CONFIG_ID, 'CONFIRM')),
              MB_YESNOCANCEL or MB_ICONQUESTION)) of
        IDYES : blAppend:=True;
        IDNO  : blAppend:=False;
      else
        Exit;
      end;
    end
    else begin
      blAppend:=False;
    end;

    if (not m_report.SaveToFile(FileName, sr, blAppend)) then begin
      Application.MessageBox(
        PChar(Format(sr.Get(CONFIG_ID, 'REPCREATERR'), [FileName])),
        PChar(sr.Get(CONFIG_ID, 'ERROR')),
        MB_ICONSTOP);
    end;
  end;
end;

procedure TLogForm.MakeJobFileBtnClick(Sender: TObject);
var
  blAppend  : Boolean;
  sTitle    : String;
  sTitleBak : String;
  sFileName : String;
  jbatch    : TBFJobBatch;
  sr        : TStrRes;
begin

  // let the user store the job
  sr:=_globals.GetSr;

  with SaveDialog do begin
    Title:=sr.Get(CONFIG_ID, 'SD_JOB_TITLE');
    Filter:=sr.Get(CONFIG_ID, 'SD_JOB_FILTER');
    InitialDir:=m_config.GetStringOption('LAST_SAVEDIR_JOB');
    if (not Execute) then
      Exit;
    sFileName:=FileName;
  end;
  m_config.FixStringOption('LAST_SAVEDIR_JOB',
                           ExtractFilePath(sFileName));

  // append the the extension, if neecssary
  if (CompareText(TStrPlus.ExtractFileExtension(sFileName),
                  BFJFILE_EXTENSION) <> 0) then
    sFileName:=sFileName + '.' + BFJFILE_EXTENSION;

  if (FileExists(sFileName)) then begin

    case (Application.MessageBox(
            PChar(Format(sr.Get(CONFIG_ID, 'JOBFILEEXISTS'), [sFileName])),
            PChar(sr.Get(CONFIG_ID, 'CONFIRM')),
            MB_YESNOCANCEL or MB_ICONQUESTION)) of
      IDYES : blAppend:=True;
      IDNO  : blAppend:=False;
    else
      Exit;
    end;
  end
  else begin
    blAppend:=False;
  end;

  // request a title for the job
  sTitle:=TBFJob.MakeDefaultTitle(sr, m_job.GetMode);
  sTitleBak:=sTitle;
  if (not InputQuery(sr.Get(CONFIG_ID, 'TITLEREQ_CAPTION'),
                     sr.Get(CONFIG_ID, 'TITLEREQ_PROMPT'),
                     sTitle)) then
    Exit;
  m_job.SetTitle(Trim(sTitle));

  jbatch:=TBFJobBatch.Create(sr, _globals.GetOpts, _globals.GetCipMng);

  try
    jbatch.Save(m_job,
                sFileName,
                blAppend);
    Application.MessageBox(PChar(Format(sr.Get(CONFIG_ID, 'JOBFILESAVED'),
                                        [sTitle, sFileName])),
                           PROGRAM_NAME,
                           MB_ICONINFORMATION);
  except
    on ebje : EBFJobError do begin
      Application.MessageBox(
        PChar(ebje.Message),
        PROGRAM_NAME,
        MB_ICONSTOP);
    end;
  end;

  jbatch.Destroy;

end;

procedure TLogForm.HelpBtnClick(Sender: TObject);
begin
  Application.HelpCommand(HELP_CONTEXT, 507);
end;


function CompareItems(item1, item2: TListItem;
                      nData : Integer) : Integer; stdcall;
begin
  // we are so smart...
  Result:=item2.ImageIndex - item1.ImageIndex;
end;


procedure TLogForm.LM_SortClick(Sender: TObject);
var
  nI : Integer;
begin
  for nI:=0 to (m_lists.Count - 1) do begin

    TListView(m_lists.Items[nI]).CustomSort(@CompareItems, 0);

  end;
end;


procedure TLogForm.SaveDialogShow(Sender: TObject);
var
  wpl : TWindowPlacement;
begin
  wpl.length:=SizeOf(TWindowPlacement);
  GetWindowPlacement(SaveDialog.Handle, @wpl);

  wpl.length:=SizeOf(TWindowPlacement);
  wpl.rcNormalPosition.Right:=wpl.rcNormalPosition.Left +
                              m_config.GetIntegerOption('SAVEDLG_WIDTH');
  wpl.rcNormalPosition.Bottom:=wpl.rcNormalPosition.Bottom +
                               m_config.GetIntegerOption('SAVEDLG_HEIGHT');
  SetWindowPlacement(SaveDialog.Handle, @wpl);
end;


procedure TLogForm.SaveDialogClose(Sender: TObject);
var
  wpl : TWindowPlacement;
begin
  wpl.length:=SizeOf(TWindowPlacement);
  GetWindowPlacement(SaveDialog.Handle, @wpl);

  with wpl.rcNormalPosition, m_config do begin
    FixIntegerOption('SAVEDLG_WIDTH', Right - Left);
    FixIntegerOption('SAVEDLG_HEIGHT', Bottom - Top);
  end;
end;

end.
