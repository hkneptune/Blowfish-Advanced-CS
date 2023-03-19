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
  the master of all units, does the main coordination of the jobs,
  heavily linked to other modules, so we keep it out of the globals unit
}

unit Executor;

interface
uses
  Forms,
  Classes,
  Globals,
  PasswordWin,
  ProgressWin,
  MessBoxYNAC,
  PathSearch,
  PathSearchWin,
  CallBack,
  BFJob,
  LogWin;

type
  TExecutor = class
  private
    // members
    m_globals      : TGlobals;
    m_parentForm   : TForm;
    m_progressForm : TProgressForm;
    m_logForm      : TLogForm;
    m_ynacBox      : TYNACBox;
    m_psearchForm  : TPathSearchForm;
    m_simpleCB     : TCallBack;

    // flushes the random seed cache
    procedure FlushRandomSeed;

    // prepares the parent form
    // -> the job which is going to be executed
    // -> True: execution starts / false: executions has stopped
    // -> True: stop (browser) Tasks (only valid if blStart equals True)
    procedure PrepareParent(job : TBFJob;
                            blStart : Boolean;
                            blStopTasks : Boolean = True);


  public
    // constructor
    // -> all global instances
    // -> password input
    // -> the parent (main) form
    // -> the progress form
    // -> the logging window
    // -> the YNAC message box
    // -> display of the pat search progress
    // -> simple callback (for progress reporting)
    constructor Create(globals : TGlobals;
                       parentForm : TForm;
                       progressForm : TProgressForm;
                       logForm : TLogForm;
                       ynacBox : TYNACBox;
                       psearchForm : TPathSearchForm;
                       simpleCB : TCallBack);

    // to precheck if only files and folders are selected
    // (to call before GUI locking and other stuff is to be done)
    // -> True: check for a single selected file / False: not that tight
    // -> True: accept readonly drivers
    // <- True: we can start / False: something wrong is selected
    function CheckSelection(blSingleFile : Boolean = False;
                            blAcceptReadOnly : Boolean = True) : Boolean;

    // encrypts selected files from the GUI
    procedure GUIEncrypt;

    // decrypts selected files from the GUI
    procedure GUIDecrypt;

    // wipes selected files from the GUI
    procedure GUIWipe;

    // reencrypts selected files from the GUI
    procedure GUIReencrypt;

    // deslacks selected files from the GUI
    procedure GUIDeslack;

    // works with a file from the GUI
    // -> file name (if empty a single selected file in the browser is choosen)
    // <- True: refresh favorites / False: not necessary
    function GUIWorkWith(sFileName : String = '') : Boolean;

    // views a file from the GUI
    // -> file name (if empty a single selected file in the browser is choosen)
    procedure GUIView(sFileName : String = '');

    // executes a job through the GUI
    // -> job to execute (destroyed afterwards)
    procedure GUIExecJob(job : TBFJob);

    // launches job files
    // -> job files to work through
    procedure GUILaunchJobFiles(jobFiles : TStringList);

  end;



implementation
uses
  Windows,
  SysUtils,
  Main,
  General,
  GUIMessageCBImpl,
  MessageCallBack,
  Options,
  BFAViewer,
  SecureMem;


//////////////////////////// TExecutor ////////////////////////////


// string resource ID
const
  STRRES_ID = 'EXECUTOR';


constructor TExecutor.Create(globals : TGlobals;
                             parentForm : TForm;
                             progressForm : TProgressForm;
                             logForm : TLogForm;
                             ynacBox : TYNACBox;
                             psearchForm : TPathSearchForm;
                             simpleCB : TCallBack);
begin
  // take over all given instances
  m_globals:=globals;
  m_parentForm:=parentForm;
  m_progressForm:=progressForm;
  m_logForm:=logForm;
  m_ynacBox:=ynacBox;
  m_psearchForm:=psearchForm;
  m_simpleCB:=simpleCB;
end;



function TExecutor.CheckSelection(blSingleFile : Boolean = False;
                                  blAcceptReadOnly : Boolean = True) : Boolean;
begin
  // only files and folders selected?
  with m_globals.GetFileBrowser do begin

    if ((not blAcceptReadOnly) and IsReadOnlyPath) then begin
      Result:=False;
      Exit;
    end;

    if (blSingleFile) then
      Result:=((GetNumOfSelFiles = 1) and (GetNumOfSelObjs = 1))
    else
      Result:=(((GetNumOfSelFiles > 0) or (GetNumOfSelDirs > 0)) and
               (GetNumOfSelDrives = 0) and
               (GetNumOfSelOther = 0));
  end;
end;



procedure TExecutor.GUIEncrypt;
var
  nExec        : Integer;
  blAnomaly    : Boolean;
  blShowReport : Boolean;
  files        : TStringList;
  folders      : TStringList;
  jobShop      : TBFJobShop;
  jobEnv       : TBFJobEnvironment;
  jobFiles     : TBFJobFileInfo;
  jobReport    : TBFJobReport;
  job          : TBFJob;
  messCB       : TGUIMessageCBImpl;
  psearchCB    : TPathSearchCBImpl;
  workProgress : TBFWorkProgressImpl;
begin
  // prepare the message callback
  messCB:=TGUIMessageCBImpl.Create(m_ynacBox);

  // prepare the path search callback
  psearchCB:=TPathSearchCBImpl.Create(m_psearchForm);

  // get the selected files and folders
  files:=TStringList.Create;
  folders:=TStringList.Create;
  m_globals.GetFileBrowser.GetSelectedFilesAndFolders(files, folders);

  // create the job shop
  jobShop:=TBFJobShop.Create(m_globals.GetRndMng,
                             messCB,
                             m_globals.GetSr,
                             psearchCB);

  // package the files, folders and the current path
  jobFiles:=TBFJobFileInfo.Create(files,
                                  folders,
                                  ExtractFilePath(
                                    m_globals.GetFileBrowser.GetCurrentPath));
  // time to make the job now
  jobEnv:=TBFJobEnvironment.Create(m_globals.GetOpts,
                                   m_globals.GetCipMng);
  job:=TBFJob.Create(BFJOB_MODE_ENCRYPT,
                     jobFiles,
                     jobEnv);

  // don't forget the progress callback
  workProgress:=TBFWorkProgressImpl.Create(m_progressForm);

  // need a job report
  jobReport:=TBFJobReport.Create;

  // at last we must setup the callback dialogs
  m_psearchForm.Setup(m_parentForm);
  m_progressForm.Setup(m_parentForm,
                       job);
  PrepareParent(job, True);

  // we need random data as good as it possibly gets
  FlushRandomSeed;

  // goto work
  nExec:=0;  // (just to please the compiler)
  blAnomaly:=True;
  try
    nExec:=jobShop.ExecuteJob(job,
                              workProgress,
                              jobReport,
                              m_globals.GetPasswordInput);
    blAnomaly:=False;
  except
    on ebjf : EBFJobFatal do begin
      // show what was fatal
      Application.MessageBox(PChar(Format(
                                     m_globals.GetSr.Get(STRRES_ID, 'FATAL'),
                                     [ebjf.Message])),
                             PROGRAM_NAME,
                             MB_ICONSTOP);

    end;
    on EBFJobInterrupt do begin
      // interrupts won't be reported
    end;
    on EBFJobCanceled do begin
      // (user break before any action was started)
      workProgress.Destroy;
      jobShop.Destroy;
      psearchCB.Destroy;
      messCB.Destroy;
      PrepareParent(job, False);
      Exit;
    end;
    on EBFJobError do begin
      // (errors will be shown by the logform setup)
    end;
  end;

  PrepareParent(job, False);

  // clean up
  workProgress.Destroy;
  jobShop.Destroy;
  psearchCB.Destroy;
  messCB.Destroy;

  // show the results, if necessary
  blShowReport:=m_globals.GetOpts.GetCfg.GetBooleanOption(
                  OPTIONS_CFGID_SHOWJOBREPORT);

  if ((((nExec = BFJOB_EXEC_SUCCESS) and blShowReport) or
       (nExec = BFJOB_EXEC_ERROR) or
       (nExec = BFJOB_EXEC_WARNING) or
       blAnomaly) and (not jobReport.IsVoid)) then begin

    if (m_logForm.Setup(jobReport, job)) then
      m_logForm.ShowModal;
  end;

  // reenable the browser tasks
  with m_parentForm as TMainForm do EnableBrowserTasks(True);

  // job (report) not longer needed
  jobReport.Destroy;
  job.Destroy;
end;




procedure TExecutor.GUIDecrypt;
var
  nExec        : Integer;
  blAnomaly    : Boolean;
  blShowReport : Boolean;
  files        : TStringList;
  folders      : TStringList;
  jobShop      : TBFJobShop;
  jobEnv       : TBFJobEnvironment;
  jobFiles     : TBFJobFileInfo;
  jobReport    : TBFJobReport;
  job          : TBFJob;
  messCB       : TGUIMessageCBImpl;
  psearchCB    : TPathSearchCBImpl;
  workProgress : TBFWorkProgressImpl;
begin

  // get the selected files and folders
  files:=TStringList.Create;
  folders:=TStringList.Create;
  m_globals.GetFileBrowser.GetSelectedFilesAndFolders(files, folders);

  // prepare some callbacks
  messCB:=TGUIMessageCBImpl.Create(m_ynacBox);
  psearchCB:=TPathSearchCBImpl.Create(m_psearchForm);

  // create the job shop
  jobShop:=TBFJobShop.Create(m_globals.GetRndMng,
                             messCB,
                             m_globals.GetSr
                             psearchCB);

  // package the files, folders and the current path
  jobFiles:=TBFJobFileInfo.Create(files,
                                  folders,
                                  ExtractFilePath(
                                    m_globals.GetFileBrowser.GetCurrentPath));
  // time to make the job now
  jobEnv:=TBFJobEnvironment.Create(m_globals.GetOpts,
                                   m_globals.GetCipMng);
  job:=TBFJob.Create(BFJOB_MODE_DECRYPT,
                     jobFiles,
                     jobEnv);

  // don't forget the progress callback
  workProgress:=TBFWorkProgressImpl.Create(m_progressForm);

  // need a job report
  jobReport:=TBFJobReport.Create;

  // at last we must setup the callback dialogs
  m_psearchForm.Setup(m_parentForm);
  m_progressForm.Setup(m_parentForm,
                       job);
  PrepareParent(job, True);

  // goto work
  nExec:=0;  // (just to please the compiler)
  blAnomaly:=True;
  try
    nExec:=jobShop.ExecuteJob(job,
                              workProgress,
                              jobReport,
                              m_globals.GetPasswordInput);
    blAnomaly:=False;
  except
    on ebjf : EBFJobFatal do begin
      // show what was fatal
      Application.MessageBox(PChar(Format(
                                     m_globals.GetSr.Get(STRRES_ID, 'FATAL'),
                                     [ebjf.Message])),
                             PROGRAM_NAME,
                             MB_ICONSTOP);

    end;
    on EBFJobInterrupt do begin
      // ...
    end;
    on EBFJobCanceled do begin
      // (user break before any action was started)
      workProgress.Destroy;
      jobShop.Destroy;
      psearchCB.Destroy;
      messCB.Destroy;
      PrepareParent(job, False);
      Exit;
    end;
    on EBFJobError do begin
      // (errors will be shown by the logform setup)
    end;
  end;

  PrepareParent(job, False);

  // clean up
  workProgress.Destroy;
  jobShop.Destroy;
  psearchCB.Destroy;
  messCB.Destroy;

  // show the results, if necessary
  blShowReport:=m_globals.GetOpts.GetCfg.GetBooleanOption(
                  OPTIONS_CFGID_SHOWJOBREPORT);

  if ((((nExec = BFJOB_EXEC_SUCCESS) and blShowReport) or
       (nExec = BFJOB_EXEC_ERROR) or
       (nExec = BFJOB_EXEC_WARNING) or
      blAnomaly) and (not jobReport.IsVoid)) then begin

    if (m_logForm.Setup(jobReport, job)) then
      m_logForm.ShowModal;
  end;

  // reenable the browser tasks
  with m_parentForm as TMainForm do EnableBrowserTasks(True);

  // job (report) not longer needed
  jobReport.Destroy;
  job.Destroy;
end;




procedure TExecutor.GUIWipe;
var
  nExec        : Integer;
  blAnomaly    : Boolean;
  blShowReport : Boolean;
  blCanceled   : Boolean;
  files        : TStringList;
  folders      : TStringList;
  jobShop      : TBFJobShop;
  jobEnv       : TBFJobEnvironment;
  jobFiles     : TBFJobFileInfo;
  jobReport    : TBFJobReport;
  job          : TBFJob;
  messCB       : TGUIMessageCBImpl;
  psearchCB    : TPathSearchCBImpl;
  workProgress : TBFWorkProgressImpl;
begin
  // get the files and folders from the browser
  files:=TStringList.Create;
  folders:=TStringList.Create;
  m_globals.GetFileBrowser.GetSelectedFilesAndFolders(files, folders);

  // create message and path search callback
  messCB:=TGUIMessageCBImpl.Create(m_ynacBox);
  psearchCB:=TPathSearchCBImpl.Create(m_psearchForm);

  // prepare all that job stuff
  jobShop:=TBFJobShop.Create(m_globals.GetRndMng,
                             messCB,
                             m_globals.GetSr,
                             psearchCB);

  jobFiles:=TBFJobFileInfo.Create(files,
                                  folders,
                                  ExtractFilePath(
                                    m_globals.GetFileBrowser.GetCurrentPath));

  jobEnv:=TBFJobEnvironment.Create(m_globals.GetOpts,
                                   m_globals.GetCipMng);
  job:=TBFJob.Create(BFJOB_MODE_WIPE,
                     jobFiles,
                     jobEnv);

  jobReport:=TBFJobReport.Create;

  // need a GUI based progress callback, too
  workProgress:=TBFWorkProgressImpl.Create(m_progressForm);

  // setup the callback windows
  m_psearchForm.Setup(m_parentForm);
  m_progressForm.Setup(m_parentForm,
                       job,
                       job.BulkActions);
  PrepareParent(job, True);

  // now wipe
  nExec:=0;
  blAnomaly:=True;
  blCanceled:=False;
  try
    nExec:=jobShop.ExecuteJob(job,
                              workProgress,
                              jobReport);
    blAnomaly:=False;
  except
    on ebjf : EBFJobFatal do begin
      Application.MessageBox(PChar(Format(
                                     m_globals.GetSr.Get(STRRES_ID, 'FATAL'),
                                     [ebjf.Message])),
                             PROGRAM_NAME,
                             MB_ICONSTOP);

    end;
    on EBFJobInterrupt do begin
      // nothing to do here
    end;
    on EBFJobCanceled do begin
      blCanceled:=True;
    end;
    on EBFJobError do begin
      // (errors will be shown by the logform setup)
    end;
  end;

  PrepareParent(job, False);

  // cleanup
  workProgress.Destroy;
  jobShop.Destroy;
  psearchCB.Destroy;
  messCB.Destroy;

  // show the report, if necessary
  blShowReport:=m_globals.GetOpts.GetCfg.GetBooleanOption(
                  OPTIONS_CFGID_SHOWJOBREPORT);

  if (((((nExec = BFJOB_EXEC_SUCCESS) and blShowReport) or
        (nExec = BFJOB_EXEC_ERROR) or
        (nExec = BFJOB_EXEC_WARNING) or
        blAnomaly)) and
       (not blCanceled) and (not jobReport.IsVoid)) then begin

    if (m_logForm.Setup(jobReport, job)) then
      m_logForm.ShowModal;
  end;

  // reenable the browser tasks
  with m_parentForm as TMainForm do EnableBrowserTasks(True);

  // done
  jobReport.Destroy;
  job.Destroy;
end;




procedure TExecutor.GUIReencrypt;
var
  nExec        : Integer;
  blAnomaly    : Boolean;
  blShowReport : Boolean;
  files        : TStringList;
  folders      : TStringList;
  jobShop      : TBFJobShop;
  jobEnv       : TBFJobEnvironment;
  jobFiles     : TBFJobFileInfo;
  jobReport    : TBFJobReport;
  job          : TBFJob;
  messCB       : TGUIMessageCBImpl;
  psearchCB    : TPathSearchCBImpl;
  workProgress : TBFWorkProgressImpl;
begin

  // get the selected files and folders
  files:=TStringList.Create;
  folders:=TStringList.Create;
  m_globals.GetFileBrowser.GetSelectedFilesAndFolders(files, folders);

  // prepare some callbacks
  messCB:=TGUIMessageCBImpl.Create(m_ynacBox);
  psearchCB:=TPathSearchCBImpl.Create(m_psearchForm);

  // create the job shop
  jobShop:=TBFJobShop.Create(m_globals.GetRndMng,
                             messCB,
                             m_globals.GetSr,
                             psearchCB);

  // package the files, folders and the current path
  jobFiles:=TBFJobFileInfo.Create(files,
                                  folders,
                                  ExtractFilePath(
                                    m_globals.GetFileBrowser.GetCurrentPath));
  // time to make the job now
  jobEnv:=TBFJobEnvironment.Create(m_globals.GetOpts,
                                   m_globals.GetCipMng);
  job:=TBFJob.Create(BFJOB_MODE_REENCRYPT,
                     jobFiles,
                     jobEnv);

  // don't forget the progress callback
  workProgress:=TBFWorkProgressImpl.Create(m_progressForm);

  // need a job report
  jobReport:=TBFJobReport.Create;

  // at last we must setup the callback dialogs
  m_psearchForm.Setup(m_parentForm);
  m_progressForm.Setup(m_parentForm,
                       job);
  PrepareParent(job, True);

  // we need as good random data as possible
  FlushRandomSeed;

  // goto work
  nExec:=0;  // (just to please the compiler)
  blAnomaly:=True;
  try
    nExec:=jobShop.ExecuteJob(job,
                              workProgress,
                              jobReport,
                              m_globals.GetPasswordInput);
    blAnomaly:=False;
  except
    on ebjf : EBFJobFatal do begin
      // show what was fatal
      Application.MessageBox(PChar(Format(
                                     m_globals.GetSr.Get(STRRES_ID, 'FATAL'),
                                     [ebjf.Message])),
                             PROGRAM_NAME,
                             MB_ICONSTOP);

    end;
    on EBFJobInterrupt do begin
      // ...
    end;
    on EBFJobCanceled do begin
      // (user break before any action was started)
      workProgress.Destroy;
      jobShop.Destroy;
      psearchCB.Destroy;
      messCB.Destroy;
      PrepareParent(job, False);
      Exit;
    end;
    on EBFJobError do begin
      // (errors will be shown by the logform setup)
    end;
  end;

  PrepareParent(job, False);

  // clean up
  workProgress.Destroy;
  jobShop.Destroy;
  psearchCB.Destroy;
  messCB.Destroy;

  // show the results, if neecssary
  blShowReport:=m_globals.GetOpts.GetCfg.GetBooleanOption(
                  OPTIONS_CFGID_SHOWJOBREPORT);

  if ((((nExec = BFJOB_EXEC_SUCCESS) and blShowReport) or
       (nExec = BFJOB_EXEC_ERROR) or
       (nExec = BFJOB_EXEC_WARNING) or
       blAnomaly) and (not jobReport.IsVoid)) then begin

    if (m_logForm.Setup(jobReport, job)) then
      m_logForm.ShowModal;
  end;

  // reenable the browser tasks
  with m_parentForm as TMainForm do EnableBrowserTasks(True);

  // job (report) not longer needed
  jobReport.Destroy;
  job.Destroy;
end;


procedure TExecutor.GUIDeslack;
var
  nExec        : Integer;
  blAnomaly    : Boolean;
  blShowReport : Boolean;
  files        : TStringList;
  folders      : TStringList;
  jobShop      : TBFJobShop;
  jobEnv       : TBFJobEnvironment;
  jobFiles     : TBFJobFileInfo;
  jobReport    : TBFJobReport;
  job          : TBFJob;
  messCB       : TGUIMessageCBImpl;
  psearchCB    : TPathSearchCBImpl;
  workProgress : TBFWorkProgressImpl;
begin

  // get the selected files and folders
  files:=TStringList.Create;
  folders:=TStringList.Create;
  m_globals.GetFileBrowser.GetSelectedFilesAndFolders(files, folders);

  // prepare some callbacks
  messCB:=TGUIMessageCBImpl.Create(m_ynacBox);
  psearchCB:=TPathSearchCBImpl.Create(m_psearchForm);

  // create the job shop
  jobShop:=TBFJobShop.Create(m_globals.GetRndMng,
                             messCB,
                             m_globals.GetSr,
                             psearchCB);

  // package the files, folders and the current path
  jobFiles:=TBFJobFileInfo.Create(files,
                                  folders,
                                  ExtractFilePath(
                                    m_globals.GetFileBrowser.GetCurrentPath));
  // time to make the job now
  jobEnv:=TBFJobEnvironment.Create(m_globals.GetOpts,
                                   m_globals.GetCipMng);
  job:=TBFJob.Create(BFJOB_MODE_DESLACK,
                     jobFiles,
                     jobEnv);

  // don't forget the progress callback
  workProgress:=TBFWorkProgressImpl.Create(m_progressForm);

  // need a job report
  jobReport:=TBFJobReport.Create;

  // at last we must setup the callback dialogs
  m_psearchForm.Setup(m_parentForm);
  m_progressForm.Setup(m_parentForm,
                       job,
                       job.BulkActions);
  PrepareParent(job, True);

  // goto work
  nExec:=0;  // (just to please the compiler)
  blAnomaly:=True;
  try
    nExec:=jobShop.ExecuteJob(job,
                              workProgress,
                              jobReport,
                              m_globals.GetPasswordInput);
    blAnomaly:=False;
  except
    on ebjf : EBFJobFatal do begin
      // show what was fatal
      Application.MessageBox(PChar(Format(
                                     m_globals.GetSr.Get(STRRES_ID, 'FATAL'),
                                     [ebjf.Message])),
                             PROGRAM_NAME,
                             MB_ICONSTOP);

    end;
    on EBFJobInterrupt do begin
      // ...
    end;
    on EBFJobCanceled do begin
      // (user break before any action was started)
      workProgress.Destroy;
      jobShop.Destroy;
      psearchCB.Destroy;
      messCB.Destroy;
      PrepareParent(job, False);
      Exit;
    end;
    on EBFJobError do begin
      // (errors will be shown by the logform setup)
    end;
  end;

  PrepareParent(job, False);

  // clean up
  workProgress.Destroy;
  jobShop.Destroy;
  psearchCB.Destroy;
  messCB.Destroy;

  // show the results, if neecssary
  blShowReport:=m_globals.GetOpts.GetCfg.GetBooleanOption(
                  OPTIONS_CFGID_SHOWJOBREPORT);

  if ((((nExec = BFJOB_EXEC_SUCCESS) and blShowReport) or
       (nExec = BFJOB_EXEC_ERROR) or
       (nExec = BFJOB_EXEC_WARNING) or
       blAnomaly) and (not jobReport.IsVoid)) then begin

    if (m_logForm.Setup(jobReport, job)) then
      m_logForm.ShowModal;
  end;

  // reenable the browser tasks (FIXME: no update flicker necessary, ok?)
  with m_parentForm as TMainForm do EnableBrowserTasks(True, False);

  // job (report) not longer needed
  jobReport.Destroy;
  job.Destroy;
end;


function TExecutor.GUIWorkWith(sFileName : String = '') : Boolean;
var
  jobShop : TBFJobShop;
  jobEnv  : TBFJobEnvironment;
  job     : TBFJob;
  messCB  : TGUIMessageCBImpl;
begin

  // if not given, get the filename from the browser
  if (sFileName = '') then begin
    sFileName:=m_globals.GetFileBrowser.GetSingleSelectedFile;
    if (sFileName = '') then begin
      // (this should never happen, but avoided in a precheck)
      Result:=False;
      Exit;
    end;
  end;

  // prepare the message callback
  messCB:=TGUIMessageCBImpl.Create(m_ynacBox);

  // make all that job stuff
  with m_globals do begin
    jobEnv:=TBFJobEnvironment.Create(GetOpts, GetCipMng);
    jobShop:=TBFJobShop.Create(GetRndMng,
                               messCB,
                               GetSr,
                               Nil,
                               Nil,
                               GetBFAWorkWith);
    job:=TBFJob.CreateSimple(BFJOB_MODE_WORKWITH,
                             sFileName,
                             jobEnv);
  end;

  // we need as good random data as possible
  FlushRandomSeed;

  // now execute the job
  PrepareParent(job, True, False);
  try
    jobShop.ExecuteSimpleJob(job,
                             m_simpleCB,
                             m_globals.GetPasswordInput,
                             m_parentForm.Handle);
    Result:=True;
  except
    on EBFJobInterrupt do begin
      Result:=False;
    end;
    on ebje : EBFJobError do begin
      // (no need for a MessageBox() call here)
      with messCB do begin
        SetStyle(MCB_STYLE_OK);
        SetKindOf(MCB_KINDOF_ERROR);
        SetMessage(Format(m_globals.GetSr.Get(STRRES_ID, 'ERROR'),
                          [ebje.Message]));
        CallBack;
      end;
      Result:=False;
    end;
  end;

  PrepareParent(job, False);

  // clean up
  job.Destroy;
  jobShop.Destroy;
  messCB.Destroy;

end;


procedure TExecutor.GUIExecJob(job : TBFJob);
var
  nExec        : Integer;
  blAnomaly    : Boolean;
  blShowReport : Boolean;
  blCanceled   : Boolean;
  jobShop      : TBFJobShop;
  jobReport    : TBFJobReport;
  messCB       : TGUIMessageCBImpl;
  psearchCB    : TPathSearchCBImpl;
  workProgress : TBFWorkProgressImpl;
begin

  // we have to care about _every_ mode here

  messCB:=TGUIMessageCBImpl.Create(m_ynacBox);
  psearchCB:=TPathSearchCBImpl.Create(m_psearchForm);
  workProgress:=TBFWorkProgressImpl.Create(m_progressForm);
  jobReport:=TBFJobReport.Create;

  with m_globals do begin
    jobShop:=TBFJobShop.Create(GetRndMng,
                               messCB,
                               GetSr,
                               psearchCB,
                               GetBFAViewer,
                               GetBFAWorkWith);
  end;

  m_psearchForm.Setup(m_parentForm);
  m_progressForm.Setup(m_parentForm,
                       job,
                       job.BulkActions);

  PrepareParent(job, True, False);

  nExec:=0;
  blAnomaly:=True;
  blCanceled:=False;
  try
    if (job.IsSimple) then
      jobShop.ExecuteSimpleJob(job,
                               m_simpleCB,
                               m_globals.GetPasswordInput,
                               m_parentForm.Handle)
    else
      nExec:=jobShop.ExecuteJob(job,
                                workProgress,
                                jobReport,
                                m_globals.GetPasswordInput);
    blAnomaly:=False;
  except
    on ebjf : EBFJobFatal do begin
      // show what was fatal
      Application.MessageBox(PChar(Format(
                                     m_globals.GetSr.Get(STRRES_ID, 'FATAL'),
                                     [ebjf.Message])),
                             PROGRAM_NAME,
                             MB_ICONSTOP);

    end;
    on EBFJobInterrupt do begin
      // ...
    end;
    on EBFJobCanceled do begin
      blCanceled:=True;
    end;
    on ebje : EBFJobError do begin
      // simple jobs will get some attention right here
      if (job.IsSimple) then begin
        with messCB do begin
          SetStyle(MCB_STYLE_OK);
          SetKindOf(MCB_KINDOF_ERROR);
          SetMessage(Format(m_globals.GetSr.Get(STRRES_ID, 'ERROR'),
                            [ebje.Message]));
          CallBack;
        end;
      end;
    end;
  end;

  PrepareParent(job, False);

  // clean up
  workProgress.Destroy;
  jobShop.Destroy;
  psearchCB.Destroy;
  messCB.Destroy;

  // show the results, if neecssary
  if (not job.IsSimple) then begin

    blShowReport:=m_globals.GetOpts.GetCfg.GetBooleanOption(
                    OPTIONS_CFGID_SHOWJOBREPORT);

    if ((((nExec = BFJOB_EXEC_SUCCESS) and blShowReport) or
         (nExec = BFJOB_EXEC_ERROR) or
         (nExec = BFJOB_EXEC_WARNING) or
         blAnomaly) and
         (not jobReport.IsVoid) and
         (not blCanceled)) then begin

      if (m_logForm.Setup(jobReport, job)) then
        m_logForm.ShowModal;
    end
  end;

  // job (report) not longer needed
  jobReport.Destroy;
  job.Destroy;

end;


procedure TExecutor.GUILaunchJobFiles(jobFiles : TStringList);
var
  nI, nJ         : Integer;
  nNumOfJobFiles : Integer;
  nExec          : Integer;
  blAnomaly      : Boolean;
  blShowReport   : Boolean;
  blWasFatal     : Boolean;
  blWasError     : Boolean;
  blWasBreak     : Boolean;
  blDoExecJob    : Boolean;
  blFirstCall    : Boolean;
  jobShop        : TBFJobShop;
  jobReport      : TBFJobReport;
  messCB         : TGUIMessageCBImpl;
  psearchCB      : TPathSearchCBImpl;
  workProgress   : TBFWorkProgressImpl;
  bjobBatch      : TBFJobBatch;
  actJob         : TBFJob;

begin

  // prepare everything for a good launch

  messCB:=TGUIMessageCBImpl.Create(m_ynacBox);
  psearchCB:=TPathSearchCBImpl.Create(m_psearchForm);
  workProgress:=TBFWorkProgressImpl.Create(m_progressForm);
  jobReport:=TBFJobReport.Create;
  with m_globals do begin
    jobShop:=TBFJobShop.Create(GetRndMng,
                               messCB,
                               GetSr,
                               psearchCB,
                               GetBFAViewer,
                               GetBFAWorkWith);
  end;
  m_psearchForm.Setup(m_parentForm);

  // now start processing the job files
  blAnomaly:=False;
  blWasFatal:=False;
  nNumOfJobFiles:=jobfiles.Count;
  blShowReport:=False;
  blFirstCall:=True;
  nI:=0;
  while (nI < nNumOfJobFiles) do begin

    // load the jobfile
    blWasError:=False;
    bjobBatch:=TBFJobBatch.Create(m_globals.GetSr,
                                  m_globals.GetOpts,
                                  m_globals.GetCipMng);
    try
      bjobBatch.Load(jobFiles.Strings[nI]);
    except
      on ebje : EBFJobError do begin
        with messCB do begin
          SetStyle(MCB_STYLE_OK);
          SetKindOf(MCB_KINDOF_ERROR);
          SetMessage(ebje.Message);
          CallBack;
        end;
        blWasError:=True;
      end;
    end;

    // execute the loaded jobs
    nJ:=0;
    blWasBreak:=False;
    while ((not blWasError) and
           (not blWasBreak) and
           (nJ < bjobBatch.GetJobCount)) do begin

      // invoke the environment for a certain job
      bjobBatch.InvokeEnvironment(nJ);

      // if at least one job demand's a report we will show it
      if (m_globals.GetOpts
                   .GetCfg
                   .GetBooleanOption(OPTIONS_CFGID_SHOWJOBREPORT)) then
        blShowReport:=True;

      // get a job (and its title)
      actJob:=bjobBatch.GetJob(nJ);

      // confirmation before every job?
      blDoExecJob:=True;
      if (m_globals.GetOpts
                   .GetCfg
                   .GetBooleanOption(OPTIONS_CFGID_CONFIRMOPERATIONS))
          then begin

        with messCB do begin
          SetStyle(MCB_STYLE_YESNOCANCEL);
          SetKindOf(MCB_KINDOF_QUESTION);
          SetMessage(Format(m_globals.getSr.Get(STRRES_ID, 'CONFJOBEXEC'),
                            [actJob.GetTitle]));
          try
            CallBack;
            blDoExecJob:=(GetResult = MCB_RES_YES);
          except
            on ECallBackInterrupt do begin
              blWasBreak:=True;
              blDoExecJob:=False;
            end;
          end;
        end;
      end;

      // if allowed, execute the job
      if (blDoExecJob) then begin

        // (the progress form must be set up for every job individually)
        m_progressForm.Setup(m_parentForm,
                             actJob,
                             actJob.BulkActions);

        PrepareParent(actJob, True, blFirstCall);
        blFirstCall:=False;

        blAnomaly:=True;
        try
          nExec:=jobShop.ExecuteJob(actJob,
                                    workProgress,
                                    jobReport,
                                    m_globals.GetPasswordInput);
          blAnomaly:=(nExec <> BFJOB_EXEC_SUCCESS);
        except
          on ebjf : EBFJobFatal do begin
            // show what was fatal
            with messCB do begin
              SetStyle(MCB_STYLE_OK);
              SetKindOf(MCB_KINDOF_ERROR);
              SetMessage(Format(m_globals.GetSr.Get(STRRES_ID, 'FATAL'),
                                [ebjf.Message]));
              CallBack;
            end;
            blWasFatal:=True;  // cancel everything
          end;
          on EBFJobInterrupt do begin
            blWasBreak:=True;
          end;
          on EBFJobCanceled do begin
            // nothing to do here
          end;
          on ebje : EBFJobError do begin

            // show the error occured
            with messCB do begin
              SetStyle(MCB_STYLE_OK);
              SetKindOf(MCB_KINDOF_ERROR);
              SetMessage(Format(m_globals.GetSr.Get(STRRES_ID, 'JOBERROR'),
                                [actJob.GetTitle, ebje.Message]));
              CallBack;
            end;

          end;
        end;
      end;

      PrepareParent(actJob, False);

      // restore the original env.
      bjobBatch.ResetEnvironment;

      // next job
      Inc(nJ);
    end; (* OF WHILE *)

    // we no longer need the batch object
    bjobBatch.Destroy;

    // break out if a (fatal) error occured internally
    if (blWasFatal) then
      Break;

    // next jobfile
    Inc(nI);
  end;

  // clean up
  workProgress.Destroy;
  jobShop.Destroy;
  psearchCB.Destroy;
  messCB.Destroy;

  if (blAnomaly or (blShowReport and (not jobReport.IsVoid(True)))) then begin

    if (m_logForm.Setup(jobReport)) then
      m_logForm.ShowModal;
  end;

  // job (report) not longer needed
  jobReport.Destroy;
end;


procedure TExecutor.GUIView(sFileName : String = '');
var
  messCB : TGUIMessageCBImpl;
begin

  // viewing, it's easy!

  if (sFileName = '') then begin
    sFileName:=m_globals.GetFileBrowser.GetSingleSelectedFile;
    if (sFileName = '') then
      Exit;
  end;

  messCB:=TGUIMessageCBImpl.Create(m_ynacBox);
  try
    m_globals.GetBFAViewer.ViewFile(sFileName,
                                    m_simpleCB,
                                    m_globals.GetPasswordInput,
                                    m_parentForm.Handle);
  except
     on ebvwk : EBFAViewerWrongKey do begin
       // it seems to be the wrong key
       with messCB do begin
         SetStyle(MCB_STYLE_OK);
         SetKindOf(MCB_KINDOF_ERROR);
         SetMessage(m_globals.GetSr.Get(STRRES_ID, 'VW_WRONGKEY'));
         Callback;
       end;
    end;
    on ebvu : EBFAViewerUserBreak do begin
      // (do nothing if a userbreak occured)
    end;
    on ebve : EBFAViewerError do begin
      with messCB do begin
        SetStyle(MCB_STYLE_OK);
        SetKindOf(MCB_KINDOF_ERROR);
        SetMessage(m_globals.GetSr.Get(STRRES_ID, 'VW_ERROR') + ebve.Message);
        Callback;
      end;
    end;
  end;
  messCB.Destroy;
end;


procedure TExecutor.FlushRandomSeed;
begin
  _globals.GetRndMng.FlushSeed;
end;


procedure TExecutor.PrepareParent(job : TBFJob;
                                  blStart : Boolean;
                                  blStopTasks : Boolean = True);
var
  sAction : String;
begin

  with m_globals.GetSr do begin
    case job.GetMode of
      BFJOB_MODE_ENCRYPT   : sAction:=Get(STRRES_ID, 'ACTION_ENCRYPT');
      BFJOB_MODE_DECRYPT   : sAction:=Get(STRRES_ID, 'ACTION_DECRYPT');
      BFJOB_MODE_WIPE      : sAction:=Get(STRRES_ID, 'ACTION_WIPE');
      BFJOB_MODE_REENCRYPT : sAction:=Get(STRRES_ID, 'ACTION_REENCRYPT');
      BFJOB_MODE_DESLACK   : sAction:=Get(STRRES_ID, 'ACTION_DESLACK');
      BFJOB_MODE_WORKWITH  : sAction:=Get(STRRES_ID, 'ACTION_WORKWITH');
    else
      sAction:=Get(STRRES_ID, 'ACTION_ERR');
    end;
  end;

  with m_parentForm as TMainForm do begin

    if (blStart) then begin
      SetAction(sAction);
      if (blStopTasks) then
        EnableBrowserTasks(False);
    end
    else
      SetAction('');
  end;

end;


end.
