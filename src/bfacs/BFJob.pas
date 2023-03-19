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
  all for the job handling stuff
}


unit BFJob;

interface
uses
  Windows,
  SysUtils,
  Classes,
  SecureMem,
  WorkResults,
  BFManager,
  RandomManager,
  CipherManager,
  Options,
  CallBack,
  PathSearch,
  Configuration,
  KeyCache,
  StringRes,
  BFAViewer,
  BFAWorkWith,
  MessageCallBack;


// currently supported jobs
const
  BFJOB_MODE_NONE      = -1;
  BFJOB_MODE_ENCRYPT   = 0;
  BFJOB_MODE_DECRYPT   = 1;
  BFJOB_MODE_WIPE      = 2;
  BFJOB_MODE_REENCRYPT = 3;
  BFJOB_MODE_DESLACK   = 4;
  BFJOB_MODE_VIEW      = 100;
  BFJOB_MODE_WORKWITH  = 101;
  BFJOB_MODE_BORDER_SIMPLE  = BFJOB_MODE_VIEW;

// own error classes
type
  EBFJobError     = class(Exception);   // (not thrown during job execution)
  EBFJobFatal     = class(EBFJobError);
  EBFJobInterrupt = class(EBFJobError);
  EBFJobCanceled  = class(EBFJobError); // (job was not started at all)


// job execution results
const
  BFJOB_EXEC_SUCCESS = 0;
  BFJOB_EXEC_WARNING = 1;
  BFJOB_EXEC_ERROR   = 2;
  BFJOB_EXEC_SKIPPED = 3;





// to declare a one and only job
const
  BFJOB_NONUMBER = -1;

// extension for jobfiles
const
  BFJFILE_EXTENSION = 'bfj';


// job summary

type
  TBFJobSummary = class
  private
    m_nMode   : Integer;
    m_sInfo   : String;
    m_sFiles  : String;
    m_sTime   : String;
    m_sTitle  : String;
  public
    // constructor
    // -> mode of job executed, see BFJOB_MODE_xxx
    // -> general info to show (error, success, ...)
    // -> number of files
    // -> time needed for job
    // -> job title
    constructor Create(nMode : Integer;
                       sInfo : String;
                       sFiles : String = '';
                       sTime : String = '';
                       sTitle : String = '');

    // getters
    function GetMode : Integer;
    function GetInfo : String;
    function GetFiles : String;
    function GetTime : String;
    function GetTitle : String;
  end;


// job report, a container to keep different work results and summaries
type
  TBFJobReport = class
  private
    m_workResults : TList;
    m_summaries   : TList;

  public

    // constructor
    constructor Create;

    // destructor
    destructor Destroy; override;

    // add a result
    // -> work results (kept and destroyed later)
    // -> summary (taken over)
    procedure Add(wres : TWorkResults;
                  summary : TBFJobSummary);

    // gets the number of jobs
    // <- number of executed jobs
    function GetNumOfJobs : Integer;

    // gets a result list
    // -> number of the executed job
    // <- result list (may be Nil if no results are available!)
    function GetResults(nIndex : Integer) : TWorkResults;

    // gets a summary of a job
    // -> number of the executed job
    // <- summary (reference only)
    function GetSummary(nIndex : Integer) : TBFJobSummary;

    // saves the whole report into a file
    // -> file name
    // -> True: append to an exsiting file / False: do not, but overwrite
    // <- True: successfully saved / False: error occured
    function SaveToFile(const sFileName : String;
                        sr : TStrRes;
                        blAppend : Boolean) : Boolean;

    // to check if the job report contains anything to view
    // -> check for Nil pointers, too
    // <- True: view it / False: forget it
    function IsVoid(blCheckNil : Boolean = False) : Boolean;

  end;


// class to encapsulate all file and folder informations
// necessary for a job into one logical unit
type
  TBFJobFileInfo = class
  private
    m_files        : TStringList;
    m_folders      : TStringList;
    m_sCurrentPath : String;

  public
    // constructor
    // -> the files (destroyed later)
    // -> the folders (destroyed later)
    // -> current path of the program (may be empty)
    constructor Create(files : TStringList;
                       folders : TStringList;
                       sCurrentPath : String = '');

    // destructor
    destructor Destroy; override;

    // get the files
    // <- the files
    function GetFiles : TStringList;

    // get the folders
    // <- the folders
    function GetFolders : TStringList;

    // get the current path
    // <- the current path (may be empty)
    function GetCurrentPath : String;
  end;


// every job changes an environment of settings temporary, all affected modules
// are bundled in the following class
type
  TBFJobEnvironment = class
  private
    m_opts   : TOptions;
    m_cipMng : TCipherManager;
  public
    // constructor
    // -> options
    // -> cipher manager
    constructor Create(opts : TOptions;
                       cipMng : TCipherManager);

    // get the options
    // <- options
    function GetOptions : TOptions;

    // get the cipher manager
    // <- cipher manager
    function GetCipMng : TCipherManager;

    // restores the environment to its original state
    procedure Restore;
  end;



// job class, designed to load, hold and store job data
type
  TBFJob = class
  private
    m_nMode     : Integer;
    m_nNumber   : Integer;
    m_sFileName : String;
    m_sTitle    : String;
    m_fileInfo  : TBFJobFileInfo;
    m_env       : TBFJobEnvironment;

  public
    // standard constructor to create a job from given files, folders and
    // current options
    // -> work mode, see BFJOB_MODE_xxx constants
    // -> files info (destroyed later)
    // -> environment (destroyed later)
    // -> job number
    constructor Create(nMode : Integer;
                       fileInfo : TBFJobFileInfo;
                       env : TBFJobEnvironment;
                       nNumber : Integer = BFJOB_NONUMBER);

    // to create a simple job
    // -> work mode, see BFJOB_MODE_xxx constants
    // -> file name
    // -> environment
    constructor CreateSimple(nMode : Integer;
                             const sFileName : String;
                             env : TBFJobEnvironment);
    // destructor
    destructor Destroy; override;

    // gets the mode of this job
    // <- mode of this job
    function GetMode : Integer;

    // gets the file info
    // <- file info
    function GetFileInfo : TBFJobFileInfo;

    // gets the environment
    // <- environment
    function GetEnvironment : TBFJobEnvironment;

    // gets the job number
    // <- number (or BFJOB_NONUMBER)
    function GetNumber : Integer;

    // gets the file name (for simple jobs)
    // <- file name
    function GetFileName : String;

    // to check if it's a simple job
    // <- True: simple / False: not
    function IsSimple : Boolean;

    // to check if wiping is active
    // <- True: wiping is done / False: no wiping at all
    function WipingActive : Boolean;

    // to check if the single file actions are just very short (e.g. deleting)
    // <- True: bulk operations / False: short file actions
    function BulkActions : Boolean;

    // sets a title for the job
    // -> titke
    procedure SetTitle(const sTitle : String);

    // gets the title of the job
    // -> title
    function GetTitle : String;

    // to create a default title for a job
    // -> string resources
    // -> the job mode for which to create the title for
    // <- title
    class function MakeDefaultTitle(sr : TStrRes;
                                    nJobMode : Integer) : String;

  end;


// the job executor
type
  TBFJobShop = class
  private
    m_rndMng   : TRandomManager;
    m_msgCB    : TMessageCallBack;
    m_sr       : TStrRes;
    m_searchCB : TPathSearchCallBack;
    m_viewer   : TBFAViewer;
    m_workWith : TBFAWorkWith;

  public

    // constructor
    // -> random manager to get the current random source
    // -> for confirmations, etc. (like a message box)
    // -> string resources
    // -> callback to show the file search progress (may be Nil)
    // -> viewer for .BFA file (may be Nil)
    // -> module to work with .BFA files (may be Nil)
    constructor Create(rndMng : TRandomManager;
                       msgCB : TMessageCallBack;
                       sr : TStrRes;
                       searchCB : TPathSearchCallBack = Nil;
                       viewer : TBFAViewer = Nil;
                       workWith : TBFAWorkWith = Nil);

    // executes a standard job
    // -> the job to execute
    // -> to report the work progress (the caller will have to pass the right
    //    inherited type, big problems otherwise)
    // -> list where to report the results
    // -> password input (if needed)
    // <- result code, see BFJOB_EXEC_xxx
    // exception: EBFJobError job is not a standard one
    // exception: EBFJobInterrupt if a user interrupt occured
    // exception: EBFJobFatal if a fatal error occured (no more jobs should be
    //                        executed after such a conflict has happened)
    // exception: EBFJobCanceled job was canceled, no action occured
    function ExecuteJob(job : TBFJob;
                        progressCB : TBFWorkProgress;
                        report : TBFJobReport;
                        passwInput : TPasswordInput = Nil) : Integer;

    // executes a simple job
    // -> the (simple) job
    // -> callback for progress reports
    // -> password input interface
    // -> window handle (necessary e.g. for viewing and working)
    // exception: EBFJobError some kind of error occured
    // exception: EBFJobInterrupt user break
    procedure ExecuteSimpleJob(job : TBFJob;
                               callBack : TCallBack;
                               passwInput : TPasswordInput;
                               hWnd : THandle);

  end;



// job batch container
type
  TBFJobBatch = class
  private
    m_sr       : TStrRes;
    m_opts     : TOptions;
    m_cipMng   : TCipherManager;

    m_blLoaded : Boolean;
    m_jobs     : TList;
    m_cfgIDs   : TList;
    m_cfgCnts  : TList;
    m_ciphers  : TStringList;
    m_ciphers2 : TStringList;

  public

    // constructor
    // -> string resource
    // -> options
    // -> cipher manager
    constructor Create(sr : TStrRes;
                       opts : TOptions;
                       cipMng : TCipherManager);

    // destructor
    destructor Destroy; override;

    // saves or appends a job to a file (attention: a loaded job file will
    // be removed!)
    // -> the job to save
    // -> file name
    // -> True: append (if file exists) / False: overwrite
    // -> title for the job (if empty a default title will be created)
    // exception: EBFJobError - if an error occured
    procedure Save(job : TBFJob;
                   const sFileName : String;
                   blAppend : Boolean);

    // reads in a job file
    // -> job file name
    // exception: EBFJobError - if an error occured
    procedure Load(const sFileName : String);

    // clears information from a loaded jobfile
    procedure Clear;

    // gets the number of jobs available
    // <- number of loaded jobs
    function GetJobCount : Integer;

    // gets a a job
    // -> job index
    // <- (ref. to) job
    function GetJob(nIndex : Integer) : TBFJob;

    // invokes a job environment
    // -> index of the job which env. to invoke
    procedure InvokeEnvironment(nIndex : Integer);

    // resets the environemnt changed by the last invocation
    procedure ResetEnvironment;

  end;






implementation
uses
  bfacslib,
  StringPlusI,
  IntLists,
  FileSupp,
  BFAFile,
  BFAManager,
  WipeManager,
  Reencryptor,
  Deslacker,
  StringPlus,
  TimeUtils,
  General,
  FileBrowser;


// to access string resources
const
  STRRES_ID = 'BFJOB';


//////////////////////////// TBFJobSummary ////////////////////////////


constructor TBFJobSummary.Create(nMode : Integer;
                                 sInfo : String;
                                 sFiles : String = '';
                                 sTime : String = '';
                                 sTitle : String = '');
begin
  m_nMode:=nMode;
  m_sInfo:=sInfo;
  m_sFiles:=sFiles;
  m_sTime:=sTime;
  m_sTitle:=sTitle;
end;

function TBFJobSummary.GetMode : Integer;
begin
  Result:=m_nMode;
end;

function TBFJobSummary.GetInfo : String;
begin
  Result:=m_sInfo;
end;

function TBFJobSummary.GetFiles : String;
begin
  Result:=m_sFiles;
end;

function TBFJobSummary.GetTime : String;
begin
  Result:=m_sTime;
end;

function TBFJobSummary.GetTitle : String;
begin
  Result:=m_sTitle;
end;


//////////////////////////// TBFJobReport ////////////////////////////


constructor TBFJobReport.Create;
begin
  m_workResults:=TList.Create;
  m_summaries:=TList.Create;
end;

destructor TBFJobReport.Destroy;
var
  nI          : Integer;
  destroyThis : TWorkResults;
begin
  for nI:=0 to (m_workResults.Count - 1) do begin
    destroyThis:=TWorkResults(m_workResults.Items[nI]);
    if (destroyThis <> Nil) then
      destroyThis.Destroy;
  end;
  m_workResults.Destroy;
  for nI:=0 to (m_summaries.Count - 1) do
    TBFJobSummary(m_summaries.Items[nI]).Destroy;
  m_summaries.Destroy;
end;

procedure TBFJobReport.Add(wres : TWorkResults;
                           summary : TBFJobSummary);
begin
  m_workresults.Add(wres);
  m_summaries.Add(summary);
end;

function TBFJobReport.GetNumOfJobs : Integer;
begin
  Result:=m_workResults.Count;
end;

function TBFJobReport.GetResults(nIndex : Integer) : TWorkResults;
begin
  Result:=m_workResults.Items[nIndex];
end;

function TBFJobReport.GetSummary(nIndex : Integer) : TBFJobSummary;
begin
  Result:=m_summaries.Items[nIndex];
end;


function TBFJobReport.SaveToFile(const sFileName : String;
                                 sr : TStrRes;
                                 blAppend : Boolean) : Boolean;
var
  nI      : Integer;
  blOpen  : Boolean;
  sAction : String;
  sTitle  : String;
  sTemp   : String;
  repFile : File;
  summary : TBFJobSummary;
  results : TWorkResults;

// use good old Pascal file i/o
{$I-}

function CleanUp(blSuccess : Boolean) : Boolean;
begin
  if (blOpen) then begin
    CloseFile(repFile);
    if (IOResult <> 0) then begin
      Result:=False;
      Exit
    end;
    if (not blSuccess) then begin
      if (not SysUtils.DeleteFile(sFileName)) then begin
        Result:=False;
        Exit
      end;
    end;
  end;
  Result:=True;
end;

function WriteToFile(const sBuf : String) : Boolean;
var
  nToWrite : Integer;
  nWritten : Integer;
begin
  nToWrite:=Length(sBuf);
  BlockWrite(repFile, sBuf[1], nToWrite, nWritten);
  Result:=((IOResult = 0) and (nWritten = nToWrite));
end;

begin
  // assume an error
  Result:=False;

  // open the file (new or append)
  blOpen:=False;
  AssignFile(repFile, sFileName);
  if (blAppend) then begin
    if (FileExists(sFileName)) then begin
      Reset(repFile, 1);
      if (IOResult <> 0) then begin
        CleanUp(False);
        Exit;
      end;
      Seek(repFile, FileSize(repFile));
    end
    else
      Rewrite(repFile, 1);
  end
  else begin
    Rewrite(repFile, 1);
  end;
  if (IOResult <> 0) then begin
    CleanUp(False);
    Exit;
  end;
  blOpen:=true;

  // put out all reports
  for nI:=0 to (GetNumOfJobs - 1) do begin

    // the header
    summary:=GetSummary(nI);
    with summary do begin
      case GetMode of
        BFJOB_MODE_ENCRYPT   : sAction:=sr.Get(STRRES_ID, 'OP_ENC');
        BFJOB_MODE_DECRYPT   : sAction:=sr.Get(STRRES_ID, 'OP_DEC');
        BFJOB_MODE_WIPE      : sAction:=sr.Get(STRRES_ID, 'OP_WIPE');
        BFJOB_MODE_REENCRYPT : sAction:=sr.Get(STRRES_ID, 'OP_REENC');
        BFJOB_MODE_DESLACK   : sAction:=sr.Get(STRRES_ID, 'OP_DSLK');
      else
        sAction:='???';
      end;

      sTemp:=Format(sr.Get(STRRES_ID, 'FMT_SUM'),
                    [sAction, GetInfo, GetTime, GetFiles]);
      sTitle:=summary.GetTitle;
      if (sTitle <> '') then
        sTemp:=Format(sr.Get(STRRES_ID, 'FMT_TITLE'), [sTitle]) + sTemp;

      if (not WriteToFile(sTemp + #13#10)) then begin
        CleanUp(False);
        Exit;
      end;
    end;

    // write the results
    results:=GetResults(nI);
    // (FIXME: should we trap-out-of-memory errors here?)
    if (not WriteToFile(results.RenderReport(' | ') + #13#10#13#10)) then begin
      CleanUp(False);
      Exit;
    end;

  end;

  // done
  Result:=CleanUp(True);
{$I-}
end;


function TBFJobReport.IsVoid(blCheckNil : Boolean = False) : Boolean;
var
  nI : Integer;
begin
  Result:=True;
  if (m_workResults.Count = 0) then
    Exit;

  if (not blCheckNil) then begin
    Result:=False;
    Exit;
  end;

  nI:=0;
  while (nI < m_workResults.Count) do begin
    if (m_workResults.Items[nI] <> Nil) then begin
      Result:=False;
      Exit;
    end;
    Inc(nI);
  end;
end;



//////////////////////////// TBFJobFileInfo ////////////////////////////


constructor TBFJobFileInfo.Create(files : TStringList;
                                  folders : TStringList;
                                  sCurrentPath : String = '');
begin
  m_files:=files;
  m_folders:=folders;
  m_sCurrentPath:=sCurrentPath;
end;

destructor TBFJobFileInfo.Destroy; 
begin
  m_files.Destroy;
  m_folders.Destroy;
end;

function TBFJobFileInfo.GetFiles : TStringList;
begin
  Result:=m_files;
end;

function TBFJobFileInfo.GetFolders : TStringList;
begin
  Result:=m_folders;
end;

function TBFJobFileInfo.GetCurrentPath : String;
begin
  Result:=m_sCurrentPath;
end;



//////////////////////////// TBFJobEnvironment ////////////////////////////


constructor TBFJobEnvironment.Create(opts : TOptions;
                                     cipMng : TCipherManager);
begin
  m_opts:=opts;
  m_cipMng:=cipMng;
end;

function TBFJobEnvironment.GetOptions : TOptions;
begin
  Result:=m_opts;
end;

function TBFJobEnvironment.GetCipMng : TCipherManager;
begin
  Result:=m_cipMng;
end;

procedure TBFJobEnvironment.Restore;
begin
  m_opts.Restore;
  m_cipMng.Restore;
end;


//////////////////////////// TBFJob ////////////////////////////


constructor TBFJob.Create(nMode : Integer;
                          fileInfo : TBFJobFileInfo;
                          env : TBFJobEnvironment;
                          nNumber : Integer = BFJOB_NONUMBER);
begin
  m_nMode:=nMode;
  m_fileInfo:=fileInfo;
  m_env:=env;
  m_nNumber:=nNumber;
  m_sTitle:='';
end;

constructor TBFJob.CreateSimple(nMode : Integer;
                                const sFileName : String;
                                env : TBFJobEnvironment);
begin
  m_nMode:=nMode;
  m_sFileName:=sFileName;
  m_env:=env;
  m_nNumber:=BFJOB_NONUMBER;  // (just to get things right)
end;

destructor TBFJob.Destroy;
begin
  if (m_fileInfo <> Nil) then
    m_fileInfo.Destroy;
  m_env.Destroy;  
end;

function TBFJob.GetMode : Integer;
begin
  Result:=m_nMode;
end;

function TBFJob.GetFileInfo : TBFJobFileInfo;
begin
  Result:=m_fileInfo;
end;

function TBFJob.GetEnvironment : TBFJobEnvironment;
begin
  Result:=m_env;
end;

function TBFJob.GetNumber : Integer;
begin
  Result:=m_nNumber;
end;

function TBFJob.GetFileName : String;
begin
  Result:=m_sFileName;
end;

function TBFJob.IsSimple : Boolean;
begin
  Result:=(m_nMode >= BFJOB_MODE_BORDER_SIMPLE);
end;

function TBFJob.WipingActive : Boolean;
begin
  case m_nMode of
    BFJOB_MODE_WIPE : begin
      Result:=(m_env.GetOptions.GetCfg.GetIntegerOption(OPTIONS_CFGID_WIPING)
               <> BFM_WIPE_DELETEONLY);
    end;
    BFJOB_MODE_ENCRYPT : begin
      // (we have to check some combinations to detect wiping during encrypt.)
      with m_env.GetOptions.GetCfg do begin
        if (GetIntegerOption(OPTIONS_CFGID_WIPING) =
            BFM_WIPE_DELETEONLY) then begin
          Result:=False;
        end
        else begin
          if (GetBooleanOption(OPTIONS_CFGID_USETARGETPATH)) then
            Result:=
              GetBooleanOption(OPTIONS_CFGID_REMOVESOURCEFILES)
          else
            Result:=True;
        end;
      end;
    end;
  else
    Result:=False; // (works for simple jobs, too)
  end;

//  if result then debm('+') else debm('-');

end;


function TBFJob.BulkActions : Boolean;
begin
  if (IsSimple) then begin
    Result:=False;
    Exit;
  end;
  case m_nMode of
    BFJOB_MODE_WIPE : begin
      Result:=(m_env.GetOptions.GetCfg.GetIntegerOption(OPTIONS_CFGID_WIPING)
               <> BFM_WIPE_DELETEONLY);
    end;
    BFJOB_MODE_DESLACK : Result:=False;
  else
    Result:=True;
  end;
end;

procedure TBFJob.SetTitle(const sTitle : String);
begin
  m_sTitle:=sTitle;
end;

function TBFJob.GetTitle : String;
begin
  Result:=m_sTitle;
end;


class function TBFJob.MakeDefaultTitle(sr : TStrRes;
                                       nJobMode : Integer) : String;
var
  sID : String;
begin

  case nJobMode of
    BFJOB_MODE_ENCRYPT   : sID:='OP_ENC';
    BFJOB_MODE_DECRYPT   : sID:='OP_DEC';
    BFJOB_MODE_WIPE      : sID:='OP_WIPE';
    BFJOB_MODE_REENCRYPT : sID:='OP_REENC';
    BFJOB_MODE_DESLACK   : sID:='OP_DSLK';
    BFJOB_MODE_VIEW      : sID:='OP_VIEW';
    BFJOB_MODE_WORKWITH  : sID:='OP_WWITH';
  else
    RunError(RUNERROR_BFJOBBATCH_UNKNOWNMODE);
  end;

  Result:=Format(sr.Get(STRRES_ID, 'DEFJOBTITLE'), [sr.Get(STRRES_ID, sID)]);
end;


//////////////////////////// TBFJobShop ////////////////////////////

constructor TBFJobShop.Create(rndMng : TRandomManager;
                              msgCB : TMessageCallBack;
                              sr : TStrRes;
                              searchCB : TPathSearchCallBack = Nil;
                              viewer : TBFAViewer = Nil;
                              workWith : TBFAWorkWith = Nil);
begin
  m_rndMng:=rndMng;
  m_msgCB:=msgCB;
  m_sr:=sr;
  m_searchCB:=searchCB;
  m_viewer:=viewer;
  m_workWith:=workWith;
end;


function TBFJobShop.ExecuteJob(job : TBFJob;
                               progressCB : TBFWorkProgress;
                               report : TBFJobReport;
                               passwInput : TPasswordInput = Nil) : Integer;
const
  MAX_MISSINGFILES_LEN   = 1024;
  MAX_MISSINGFILES_COUNT = 10;

var
  qStartTime     : WORD64;
  nI             : Integer;
  nUpIdx         : Integer;
  nMode          : Integer;
  nPasswInpMode  : Integer;
  blAddPoints    : Boolean;
  blNeedAKey     : Boolean;
  sError         : String;
  sDoing         : String;
  sNumOfFiles    : String;
  sTime          : String;
  sMissingFiles  : String;
  sOldCipher     : String;
  sNewCipher     : String;
  sCurrentCipher : String;
  key            : TKeyMemory;
  key2           : TKeyMemory;
  fileList       : TPathSearchContainer;
  fileSizes      : TWORD64List;
  fsearch        : TFileSearcher;
  ghostFiles     : TStringList;
  opts           : TConfigurationSection;
  results        : TWorkResults;
  dirRemover     : TPathToKill;
  bmanag         : TBFAManager;
  encSetup       : TBFAEncryptSetup;
  encFileSetup   : TBFAFileEncryptSetup;
  encResults     : TBFAEncryptResults;
  decSetup       : TBFADecryptSetup;
  decFileSetup   : TBFAFileDecryptSetup;
  decResults     : TBFADecryptResults;
  wmanag         : TWipeManager;
  wipeSetup      : TWipeSetup;
  wipeResults    : TWipeResults;
  reencpt        : TReencryptor;
  reencResults   : TReencryptResults;
  dslacker       : TDeslacker;
  deslackResults : TDeslackResults;

// add a report entry
procedure AddReport(wres : TWorkResults;
                    const sInfo : String);
begin
  report.Add(wres, TBFJobSummary.Create(nMode,
                                        sInfo,
                                        sNumOfFiles,
                                        sTime,
                                        job.GetTitle));
end;

// cleaner to free all necessary objects and to shutdown all dialogs
procedure CleanUp;
begin
  if (bmanag <> Nil) then
    bmanag.Destroy;
  if (wmanag <> Nil) then
    wmanag.Destroy;
  if (fileList <> Nil) then
    fileList.Destroy;
  if (fileSizes <> Nil) then
    fileSizes.Destroy;
  if (fsearch <> Nil) then
    fsearch.Destroy;
  if (ghostFiles <> Nil) then
    ghostFiles.Destroy;
  if (encSetup <> Nil) then
    encSetup.Destroy;
  if (encFileSetup <> Nil) then
    encFileSetup.Destroy;
  if (decSetup <> Nil) then
    decSetup.Destroy;
  if (decFileSetup <> Nil) then
    decFileSetup.Destroy;
  if (wipeSetup <> Nil) then
    wipeSetup.Destroy;
  if (reencpt <> Nil) then
    reencpt.Destroy;
  if (dslacker <> Nil) then
    dslacker.Destroy;
  if (key <> Nil) then
    key.Destroy;
  if (key2 <> Nil) then
    key2.Destroy;
end;

// to shutdown the search callback
procedure StopSearchCB;
begin
  m_searchCB.SetSignal(CALLBACK_SIGNAL_STOP);
  m_searchCB.CallBack;
end;

// to shutdown the progress callback
procedure StopProgressCB;
begin
  progressCB.SetSignal(CALLBACK_SIGNAL_STOP);
  progressCB.CallBack;
end;


begin

  // assume an error
 // Result:=BFJOB_EXEC_ERROR;

  // get the job mode
  nMode:=job.GetMode;

  // init. report infos
  sNumOfFiles:='';
  sTime:='';

  // to provide a proper cleanup reset all necessary references
  fileList:=Nil;
  fileSizes:=Nil;
  fsearch:=Nil;
  ghostFiles:=Nil;
  bmanag:=Nil;
  encSetup:=Nil;
  encFileSetup:=Nil;
  decSetup:=Nil;
  decFileSetup:=Nil;
  wmanag:=Nil;
  wipeSetup:=Nil;
  reencpt:=Nil;
  dslacker:=Nil;
  key:=Nil;
  key2:=Nil;

  // get the key(s), if necessary
  blNeedAKey:=True;
  case nMode of
    BFJOB_MODE_ENCRYPT   : nPasswInpMode:=PASSWINPUT_MODE_ENCRYPT;
    BFJOB_MODE_DECRYPT   : nPasswInpMode:=PASSWINPUT_MODE_DECRYPT;
    BFJOB_MODE_REENCRYPT : nPasswInpMode:=PASSWINPUT_MODE_REENCRYPT1;
  else
    blNeedAKey:=False;
    nPasswInpMode:=-1; // (just to please the compiler)
  end;
  if (blNeedAKey) then begin
    key:=passwInput.Execute(nPasswInpMode);
    if (key = Nil) then begin
      CleanUp;
      Result:=BFJOB_EXEC_SKIPPED;
      Exit;
    end;
  end;
  if (nMode = BFJOB_MODE_REENCRYPT) then begin
    key2:=passwInput.Execute(PASSWINPUT_MODE_REENCRYPT2);
    if (key2 = Nil) then begin
      CleanUp;
      Result:=BFJOB_EXEC_SKIPPED;
      Exit;
    end;
  end;

  // check for an unconfirmed keys, if necessary
  if (blNeedAKey) then begin
    if (((nMode = BFJOB_MODE_ENCRYPT) and (not key.GetConfirmed)) or
        ((nMode = BFJOB_MODE_REENCRYPT) and (not key2.GetConfirmed))) then
    with m_msgCB do begin
      SetStyle(MCB_STYLE_YESNO);
      SetKindOf(MCB_KINDOF_WARNING);
      SetMessage(m_sr.Get(STRRES_ID, 'UNCONFKEYWARN'));
      CallBack;
      if (GetResult = MCB_RES_NO) then begin
        CleanUp;
        Result:=BFJOB_EXEC_SKIPPED;
        Exit
      end;
    end;
  end;

  // prepare the file searcher
  opts:=job.GetEnvironment.GetOptions.GetCfg;
  with opts do
    fsearch:=TFileSearcher.Create(
               job.GetFileInfo.GetFiles,
               job.GetFileInfo.GetFolders,
               TFileSupport.MakeExcludeAttrMask(
                  GetBooleanOption(OPTIONS_CFGID_EXCLUDEARCHIVE),
                  GetBooleanOption(OPTIONS_CFGID_EXCLUDEREADONLY),
                  GetBooleanOption(OPTIONS_CFGID_EXCLUDEHIDDEN),
                  GetBooleanOption(OPTIONS_CFGID_EXCLUDESYSTEM)),
               m_sr);

  // bring up the search dialog
  m_searchCB.SetSignal(CALLBACK_SIGNAL_START);
  m_searchCB.CallBack;

  // search now
  fileSizes:=TWORD64List.Create;
  ghostFiles:=TStringList.Create;
  try
    fileList:=fsearch.Search(m_searchCB,
                             ghostFiles,
                             fileSizes);
  except
    on epsi : EPathSearchInterrupted do begin
      StopSearchCB;
      AddReport(Nil, epsi.Message);
      CleanUp;
      raise EBFJobCanceled.Create(epsi.Message);
    end;
    on epse : EPathSearchError do begin
      StopSearchCB;
      AddReport(Nil,
                Format(m_sr.Get(STRRES_ID, 'SEARCHERR'), [epse.Message]));
      CleanUp;
      Result:=BFJOB_EXEC_ERROR;
      Exit;
    end;
  end;
  StopSearchCB;
  fsearch.Destroy;
  fsearch:=Nil;

  // no files?
  if (fileList.GetNumOfFiles = 0) then begin
    sError:=m_sr.Get(STRRES_ID, 'NOFILES');
    AddReport(Nil, sError);
    CleanUp;
    raise EBFJobError.Create(sError);
    Exit;
  end;

  // save the number of files found in a string representation
  if (fileList.GetNumOfFiles = 1) then begin
    if (nMode = BFJOB_MODE_DESLACK) then
      sNumOfFiles:=m_sr.Get(STRRES_ID, 'NUMOFSINGLEFILE_DSL')
    else
      sNumOfFiles:=Format(m_sr.Get(STRRES_ID, 'NUMOFSINGLEFILE'),
                          [TStrPlusI.Sepa1000(m_sr, fileList.GetNumOfBytes)])
  end
  else begin
    if (nMode = BFJOB_MODE_DESLACK) then
      sNumOfFiles:=Format(m_sr.Get(STRRES_ID, 'NUMOFFILES_DSL'),
                          [TStrPlusI.Sepa1000(m_sr, fileList.GetNumOfFiles)])
    else
      sNumOfFiles:=Format(m_sr.Get(STRRES_ID, 'NUMOFFILES'),
                          [TStrPlusI.Sepa1000(m_sr, fileList.GetNumOfFiles),
                           TStrPlusI.Sepa1000(m_sr, fileList.GetNumOfBytes)]);
  end;

  // get the string representation of what we're doing now
  with m_sr do begin
    case nMode of
      BFJOB_MODE_ENCRYPT   : sDoing:=Get(STRRES_ID, 'DOING_ENC');
      BFJOB_MODE_DECRYPT   : sDoing:=Get(STRRES_ID, 'DOING_DEC');
      BFJOB_MODE_WIPE      : sDoing:=Get(STRRES_ID, 'DOING_WIPE');
      BFJOB_MODE_REENCRYPT : sDoing:=Get(STRRES_ID, 'DOING_REENC');
      BFJOB_MODE_DESLACK   : sDoing:=Get(STRRES_ID, 'DOING_DESL');
    else
      // (this should never happen)
      sDoing:='';
    end;
  end;

  // get a confirmation, if necessary
  if (opts.GetBooleanOption(OPTIONS_CFGID_CONFIRMOPERATIONS) or
      (BFJOB_MODE_WIPE = nMode) or
      (ghostFiles.Count > 0)) then begin

    with m_msgCB do begin

      // perpare missing file list, if necessary
      if (ghostFiles.Count > 0) then begin
        sMissingFiles:=Format(m_sr.Get(STRRES_ID, 'GHOSTFILES'),
                              [ghostFiles.Count]) + #13#10;

        blAddPoints:=False;
        if (ghostFiles.Count > MAX_MISSINGFILES_COUNT) then begin
          nUpIdx:=MAX_MISSINGFILES_COUNT - 1;
          blAddPoints:=True;
        end
        else
          nUpIdx:=ghostFiles.Count - 1;
        for nI:=0 to nUpIdx do begin
          sMissingFiles:=sMissingFiles + ghostFiles.Strings[nI] + #13#10;
          if (Length(sMissingFiles) >= MAX_MISSINGFILES_LEN) then begin
            blAddPoints:=True;
            Break;
          end;
        end;
        if (blAddPoints) then
          sMissingFiles:=sMissingFiles + '...' + #13#10;
        sMissingFiles:=sMissingFiles + #13#10;
      end
      else begin
        sMissingFiles:='';
      end;

      if (job.GetNumber = BFJOB_NONUMBER) then
        SetStyle(MCB_STYLE_YESNO)
      else
        SetStyle(MCB_STYLE_YESNOCANCEL);

      if (sMissingFiles <> '') then
        SetKindOf(MCB_KINDOF_WARNING)
      else
        SetKindOf(MCB_KINDOF_QUESTION);
      SetMessage(sMissingFiles +
                 Format(m_sr.Get(STRRES_ID, 'CONFIRMMESS'),
                        [sNumOfFiles, sDoing]));
    end;
    try
      m_msgCB.CallBack;
      if (m_msgCB.GetResult = MCB_RES_NO) then begin
        // skip that job
        CleanUp;
        raise EBFJobCanceled.Create(m_sr.Get(STRRES_ID, 'SKIPPED'));
        Exit;
      end;
    except
      on ecbi : ECallBackInterrupt do begin
        // send a user break signal
        CleanUp;
        raise EBFJobInterrupt.Create(m_sr.Get(STRRES_ID, 'USERBREAK'));
      end;
    end;
  end;

  // bring up the progress dialog;
  progressCB.SetSignal(CALLBACK_SIGNAL_START);
  progressCB.CallBack;

  // record the start time
  qStartTime:=TTimeUtils.GetNanoTime;

  // start the job now
  sCurrentCipher:=job.GetEnvironment.GetCipMng.GetCurrentCipher;
  results:=Nil; // (just to please the compiler)
  case nMode of

    ////////////////////////////////////////////////////////////////////////

    BFJOB_MODE_ENCRYPT : begin
      try
        // reset the result keeper
        encResults:=Nil;

        // create the encryption manager
        bmanag:=TBFAManager.Create(sCurrentCipher,
                                   m_rndMng.GetRandomSource,
                                   opts.GetIntegerOption(
                                     OPTIONS_CFGID_MAXERRORS),
                                   TBFAWorkProgress(progressCB),
                                   m_msgCB,
                                   m_sr);

        // prepare the BFA file encryption setup
        // (FIXME: this is awful copying, how can we get rid of this, perhaps
        //         by modifying TBFAFile to accept configuration sections?)
        encFileSetup:=TBFAFileEncryptSetup.Create;
        with encFileSetup, opts do begin
          // general...
          SetPassword(key);
          SetRemoveSource(GetBooleanOption(OPTIONS_CFGID_REMOVESOURCEFILES));
          SetOverwriteExisting(
            GetBooleanOption(OPTIONS_CFGID_OVERWRITEEXISTING));
          SetNoCache(GetBooleanOption(OPTIONS_CFGID_NOCACHE));
          SetKeepDirectories(GetBooleanOption(OPTIONS_CFGID_KEEPDIRECTORIES));
          if (GetBooleanOption(OPTIONS_CFGID_USETARGETPATH)) then begin
            SetTargetPath(GetStringOption(OPTIONS_CFGID_LASTTARGETPATH))
          end
          else begin
            SetTargetPath('');
          end;

          // encryption specials...
          if (GetBooleanOption(OPTIONS_CFGID_RELATIVEPATHS)) then begin
            SetBasePath(job.GetFileInfo.GetCurrentPath)
          end
          else begin
            SetBasePath('');
          end;

          // (not used right here)
          SetHeaderFileName('');
          SetForceFileName('');

          SetRename(GetBooleanOption(OPTIONS_CFGID_RENAME));
          SetRandomRename(GetBooleanOption(OPTIONS_CFGID_RANDOMRENAME));
          SetMaskName(GetStringOption(OPTIONS_CFGID_MASKNAME));
          // SetMaskNumber();    // set by the manager
          SetMaskExt(GetStringOption(OPTIONS_CFGID_MASKEXT));
          SetTryRename83(GetBooleanOption(OPTIONS_CFGID_TRYRENAME83));
          SetAddExtension(GetBooleanOption(OPTIONS_CFGID_ADDEXTENSION));
          SetKeepDateTime(GetBooleanOption(OPTIONS_CFGID_KEEPDATETIME));
          SetKeepAttributes(GetBooleanOption(OPTIONS_CFGID_KEEPATTRIBUTES));
          SetStorePath(GetBooleanOption(OPTIONS_CFGID_STOREPATH));
          SetRelativePaths(GetBooleanOption(OPTIONS_CFGID_RELATIVEPATHS));
          SetForceCompress(GetBooleanOption(OPTIONS_CFGID_FORCECOMPRESS));

          // translate compression flag to a compression type
          if (GetBooleanOption(OPTIONS_CFGID_USECOMPRESSION)) then
            SetCompress(GetIntegerOption(OPTIONS_CFGID_COMPRESS))
          else
            SetCompress(BFAFILE_COMPRESS_NONE);

          SetSkipEncrypted(GetBooleanOption(OPTIONS_CFGID_SKIPENCRYPTED));
          //SetNoKeyHash(GetBooleanOption(OPTIONS_CFGID_NOKEYHASH));
          SetNoKeyHash(False);
          SetWriteProtectAfter(GetBooleanOption(OPTIONS_CFGID_WRITEPROTECT));
          SetNoCompressTypes(TStrPlus.StrToList(
                               GetStringOption(OPTIONS_CFGID_NOCOMPRESSTYPES)));
        end;

        // now the major encryption setup
        encSetup:=TBFAEncryptSetup.Create(fileList,
                                          fileSizes,
                                          encFileSetup,
                                          opts.GetIntegerOption(
                                            OPTIONS_CFGID_WIPING));

        // encrypt the files
        bmanag.EncryptFiles(encSetup,
                            encResults);
        results:=TWorkResults(encResults);

      except

        // fatal error? (exception expresses the importance to break all ops)
        on ebffe : EBFFatalError do begin
          AddReport(encResults,
                    Format(m_sr.Get(STRRES_ID, 'FATALERR'), [ebffe.Message]));
          StopProgressCB;
          CleanUp;
          raise EBFJobFatal.Create(ebffe.Message);
        end;

        // user interrupt? (also exception to cancel all)
        on ebfi : EBFInterrupt do begin
          AddReport(encResults, m_sr.Get(STRRES_ID, 'USERBREAK'));
          StopProgressCB;
          CleanUp;
          raise EBFJobInterrupt.Create(ebfi.Message);
        end;

        // "common" error?
        on ebfe : EBFError do begin
          AddReport(encResults,
                    Format(m_sr.Get(STRRES_ID, 'ERROR_ENC'), [ebfe.Message]));
          Result:=BFJOB_EXEC_ERROR;  // (do need to surrender globally)
          StopProgressCB;
          CleanUp;
          Exit;
        end;
      end;
    end;

    ////////////////////////////////////////////////////////////////////////

    BFJOB_MODE_DECRYPT : begin

      try
        // same as above
        decResults:=Nil;

        bmanag:=TBFAManager.Create(sCurrentCipher,
                                   m_rndMng.GetRandomSource,
                                   opts.GetIntegerOption(
                                     OPTIONS_CFGID_MAXERRORS),
                                   TBFAWorkProgress(progressCB),
                                   m_msgCB,
                                   m_sr);

        decFileSetup:=TBFAFileDecryptSetup.Create;
        with decFileSetup, opts do begin
          SetPassword(key);
          SetRemoveSource(GetBooleanOption(OPTIONS_CFGID_REMOVESOURCEFILES));
          SetOverwriteExisting(
            GetBooleanOption(OPTIONS_CFGID_OVERWRITEEXISTING));
          SetNoCache(GetBooleanOption(OPTIONS_CFGID_NOCACHE));
          SetKeepDirectories(GetBooleanOption(OPTIONS_CFGID_KEEPDIRECTORIES));
          if (GetBooleanOption(OPTIONS_CFGID_USETARGETPATH)) then begin
            SetTargetPath(GetStringOption(OPTIONS_CFGID_LASTTARGETPATH))
          end
          else begin
            SetTargetPath('');
          end;

          if (GetBooleanOption(OPTIONS_CFGID_RELATIVEPATHS)) then begin
            SetBasePath(job.GetFileInfo.GetCurrentPath)
          end
          else begin
            SetBasePath('');
          end;

          SetIgnoreCRC32(GetBooleanOption(OPTIONS_CFGID_IGNORECRC32));
          SetRestorePath(GetBooleanOption(OPTIONS_CFGID_RESTOREPATH));
//          SetNoKeyCheck(GetBooleanOption(OPTIONS_CFGID_NOKEYCHECK));
          SetNoKeyCheck(False);
          SetAcceptTruncated(GetBooleanOption(OPTIONS_CFGID_ACCEPTTRUNCATED));
          SetFileInfoOnly(False);  // (ok here)
          SetDirectHeaderInfo(False);  // (ok here)
        end;

        // now the major encryption setup
        decSetup:=TBFADecryptSetup.Create(fileList,
                                          fileSizes,
                                          decFileSetup);

        // decrypt the files
        bmanag.DecryptFiles(decSetup,
                            decResults);
        results:=TWorkResults(decResults);

      except

        // fatal error? (exception expresses the importance to break all ops)
        on ebffe : EBFFatalError do begin
          AddReport(decResults,
                    Format(m_sr.Get(STRRES_ID, 'FATALERR'), [ebffe.Message]));
          StopProgressCB;
          CleanUp;
          raise EBFJobFatal.Create(ebffe.Message);
        end;

        // user interrupt? (also exception to cancel all)
        on ebfi : EBFInterrupt do begin
          AddReport(decResults, m_sr.Get(STRRES_ID, 'USERBREAK'));
          StopProgressCB;
          CleanUp;
          raise EBFJobInterrupt.Create(ebfi.Message);
        end;

        // "common" error?
        on ebfe : EBFError do begin
          AddReport(decResults,
                    Format(m_sr.Get(STRRES_ID, 'ERROR_DEC'), [ebfe.Message]));
          Result:=BFJOB_EXEC_ERROR;  // (no need to surrender globally)
          StopProgressCB;
          CleanUp;
          Exit;
        end;
      end;

    end;

    ////////////////////////////////////////////////////////////////////////

    BFJOB_MODE_WIPE : begin

      try
        wmanag:=TWipeManager.Create(m_rndMng.GetRandomSource,
                                    opts.GetIntegerOption(
                                      OPTIONS_CFGID_MAXERRORS),
                                    TBFWorkProgress(progressCB),
                                    m_msgCB,
                                    m_sr);

        wipeSetup:=TWipeSetup.Create(fileList,
                                     fileSizes,
                                     opts.GetIntegerOption(
                                       OPTIONS_CFGID_WIPING));

        wmanag.WipeFiles(wipeSetup,
                         wipeResults);
        results:=TWorkResults(wipeResults);
      except

        on ebffe : EBFFatalError do begin
          AddReport(wipeResults,
                    Format(m_sr.Get(STRRES_ID, 'FATALERR'), [ebffe.Message]));
          StopProgressCB;
          CleanUp;
          raise EBFJobFatal.Create(ebffe.Message);
        end;

        on ebfi : EBFInterrupt do begin
          AddReport(wipeResults, m_sr.Get(STRRES_ID, 'USERBREAK'));
          StopProgressCB;
          CleanUp;
          raise EBFJobInterrupt.Create(ebfi.Message);
        end;

        on ebfe : EBFError do begin
          AddReport(wipeResults,
                    Format(m_sr.Get(STRRES_ID, 'ERROR_WIPE'), [ebfe.Message]));
          Result:=BFJOB_EXEC_ERROR;
          StopProgressCB;
          CleanUp;
          Exit;
        end;
      end;

    end;

    ////////////////////////////////////////////////////////////////////////

    BFJOB_MODE_REENCRYPT : begin

      try
        job.GetEnvironment.GetCipMng.GetReencryptionCiphers(sOldCipher,
                                                            sNewCipher);

        reencpt:=TReencryptor.Create(sOldCipher,
                                     sNewCipher,
                                     m_rndMng.GetRandomSource,
                                     opts.GetIntegerOption(
                                       OPTIONS_CFGID_MAXERRORS),
                                     TBFAWorkProgress(progressCB),
                                     m_msgCB,
                                     m_sr);

        reencpt.Execute(fileList,
                        fileSizes,
                        key,
                        key2,
                        reencResults);
        results:=TWorkResults(reencResults);
      except
        on ebffe : EBFFatalError do begin
          AddReport(reencResults,
                    Format(m_sr.Get(STRRES_ID, 'FATALERR'), [ebffe.Message]));
          StopProgressCB;
          CleanUp;
          raise EBFJobFatal.Create(ebffe.Message);
        end;

        on ebfi : EBFInterrupt do begin
          AddReport(reencResults, m_sr.Get(STRRES_ID, 'USERBREAK'));
          StopProgressCB;
          CleanUp;
          raise EBFJobInterrupt.Create(ebfi.Message);
        end;

        on ebfe : EBFError do begin
          AddReport(reencResults,
                    Format(m_sr.Get(STRRES_ID, 'ERROR_REENC'), [ebfe.Message]));
          Result:=BFJOB_EXEC_ERROR;
          StopProgressCB;
          CleanUp;
          Exit;
        end;
      end;
    end;

    ////////////////////////////////////////////////////////////////////////

    BFJOB_MODE_DESLACK : begin
      try
        dslacker:=TDeslacker.Create(m_rndMng.GetRandomSource,
                                    opts.GetIntegerOption(
                                      OPTIONS_CFGID_MAXERRORS),
                                    TBFAWorkProgress(progressCB),
                                    m_msgCB,
                                    m_sr);

        dslacker.Execute(fileList,
                         deslackResults);
        results:=TWorkResults(deslackResults);
      except
        on ebffe : EBFFatalError do begin
          AddReport(deslackResults,
                    Format(m_sr.Get(STRRES_ID, 'FATALERR'), [ebffe.Message]));
          StopProgressCB;
          CleanUp;
          raise EBFJobFatal.Create(ebffe.Message);
        end;

        on ebfi : EBFInterrupt do begin
          AddReport(deslackResults, m_sr.Get(STRRES_ID, 'USERBREAK'));
          StopProgressCB;
          CleanUp;
          raise EBFJobInterrupt.Create(ebfi.Message);
        end;

        on ebfe : EBFError do begin
          AddReport(deslackResults,
                    Format(m_sr.Get(STRRES_ID, 'ERROR_REENC'), [ebfe.Message]));
          Result:=BFJOB_EXEC_ERROR;
          StopProgressCB;
          CleanUp;
          Exit;
        end;
      end;
    end;

  end;

  // remove empty directories, if allowed and necessary (we will only reach
  // this point if no errors occured), which means:
  // - user wants to remove folders
  // - the action must either been wiping or encryption/decryption with
  //   a given target path)
  // - there must be something to remove
  // (remember that we're not killing paths, if files are left they won't be
  //  touched in any way)
  if (opts.GetBooleanOption(OPTIONS_CFGID_REMOVEEMPTYDIRS) and
      ((nMode = BFJOB_MODE_WIPE) or
       (((nMode = BFJOB_MODE_ENCRYPT) or (nMode = BFJOB_MODE_DECRYPT))
        and opts.GetBooleanOption(OPTIONS_CFGID_USETARGETPATH)
        and opts.GetBooleanOption(OPTIONS_CFGID_REMOVESOURCEFILES))) and
      (job.GetFileInfo.GetFolders.Count > 0)) then begin

    // we won't report anything here because removing even many directories
    // is just a matter of a second (FIXME: am I right?)
    with job.GetFileInfo.GetFolders do begin

      for nI:=0 to (Count - 1) do begin

        dirRemover:=TPathToKill.Create(Strings[nI], PATHKILL_EMPTYDIRS, m_sr);
        try
          dirRemover.Erase;
        except
          on epse : EPathSearchError do begin
            // ok, so it is
          end;
        end;
        dirRemover.Destroy;
      end;
    end;
  end;

  // time to shutdown the progress (dialog)
  StopProgressCB;

  // time elapsed since the start
  sTime:=TTimeUtils.NanoTimeToStr(TTimeUtils.GetNanoTime - qStartTime);

  // make final and return code setting
  if (results.GetNumOfErrors > 0) then begin
    AddReport(results, m_sr.Get(STRRES_ID, 'ERRORS'));
    Result:=BFJOB_EXEC_ERROR;
  end
  else begin
    if (results.GetNumOfWarnings > 0) then begin
      AddReport(results, m_sr.Get(STRRES_ID, 'WARNINGS'));
      Result:=BFJOB_EXEC_WARNING;
    end
    else begin
      AddReport(results, m_sr.Get(STRRES_ID, 'ALLSUCCESS'));
      Result:=BFJOB_EXEC_SUCCESS;
    end;
  end;

  // add the report and quit
  CleanUp;
end;


procedure TBFJobShop.ExecuteSimpleJob(job : TBFJob;
                                      callBack : TCallBack;
                                      passwInput : TPasswordInput;
                                      hWnd : THandle);
begin
  if (not job.IsSimple) then
    Exit;

  case job.GetMode of

    //////////////////////////////////////////////////////////////////////////
    BFJOB_MODE_VIEW : begin
      try
        m_viewer.ViewFile(job.GetFileName,
                          callBack,
                          passwInput,
                          hWnd);
      except
        on ebvu : EBFAViewerUserBreak do begin
          raise EBFJobInterrupt.Create(ebvu.Message);
        end;
        on ebve : EBFAViewerError do begin
          raise EBFJobError.Create(ebve.Message);
        end;
      end;

    end;

    //////////////////////////////////////////////////////////////////////////
    BFJOB_MODE_WORKWITH : begin
      try
        m_workWith.Execute(job.GetFileName,
                           callback,
                           m_msgCB,
                           passwInput,
                           hWnd);
      except
        on ebwub : EBFAWorkWithUserBreak do begin
          raise EBFJobInterrupt.Create(ebwub.Message);
        end;
        on ebwe : EBFAWorkWithError do begin
          raise EBFJobError.Create(ebwe.Message);
        end;
      end;
    end;
  end;
end;


//////////////////////////// TBFJobBatch ////////////////////////////


// some lookup stuff to automate most of the
// environment storing (and restoring) ...

const
  JOBOPTS_ENCRYPT : packed array[0..35] of PChar = (

    OPTIONS_CFGID_CONFIRMOPERATIONS,

    OPTIONS_CFGID_USECACHEDKEY,
    OPTIONS_CFGID_SHOWPASSWORD,
    OPTIONS_CFGID_USEKEYDISK,
    OPTIONS_CFGID_CACHEKEY,
    OPTIONS_CFGID_AUTOCONFIRMATION,

    OPTIONS_CFGID_EXCLUDEARCHIVE,
    OPTIONS_CFGID_EXCLUDEREADONLY,
    OPTIONS_CFGID_EXCLUDEHIDDEN,
    OPTIONS_CFGID_EXCLUDESYSTEM,

    OPTIONS_CFGID_WIPING,
    OPTIONS_CFGID_COMPRESS,
    OPTIONS_CFGID_USECOMPRESSION,
    OPTIONS_CFGID_REMOVEEMPTYDIRS,

    OPTIONS_CFGID_RELATIVEPATHS,
    OPTIONS_CFGID_KEEPDIRECTORIES,
    OPTIONS_CFGID_USETARGETPATH,
    OPTIONS_CFGID_LASTTARGETPATH,
    OPTIONS_CFGID_SHOWJOBREPORT,
    OPTIONS_CFGID_REMOVESOURCEFILES,
    OPTIONS_CFGID_OVERWRITEEXISTING,
    OPTIONS_CFGID_NOCACHE,
    OPTIONS_CFGID_KEEPDATETIME,
    OPTIONS_CFGID_KEEPATTRIBUTES,

    OPTIONS_CFGID_RENAME,
    OPTIONS_CFGID_RANDOMRENAME,
    OPTIONS_CFGID_TRYRENAME83,
    OPTIONS_CFGID_ADDEXTENSION,
    OPTIONS_CFGID_COMPRESS,
    OPTIONS_CFGID_SKIPENCRYPTED,
    OPTIONS_CFGID_WRITEPROTECT,
    OPTIONS_CFGID_MASKNAME,
    OPTIONS_CFGID_MASKEXT,
    OPTIONS_CFGID_NOCOMPRESSTYPES,
    OPTIONS_CFGID_STOREPATH,
    OPTIONS_CFGID_FORCECOMPRESS
  );

  JOBOPTS_DECRYPT : packed array[0..23] of PChar = (

    OPTIONS_CFGID_CONFIRMOPERATIONS,

    OPTIONS_CFGID_USECACHEDKEY,
    OPTIONS_CFGID_SHOWPASSWORD,
    OPTIONS_CFGID_USEKEYDISK,
    OPTIONS_CFGID_CACHEKEY,
    OPTIONS_CFGID_AUTOCONFIRMATION,

    OPTIONS_CFGID_EXCLUDEARCHIVE,
    OPTIONS_CFGID_EXCLUDEREADONLY,
    OPTIONS_CFGID_EXCLUDEHIDDEN,
    OPTIONS_CFGID_EXCLUDESYSTEM,

    OPTIONS_CFGID_REMOVEEMPTYDIRS,

    OPTIONS_CFGID_RELATIVEPATHS,
    OPTIONS_CFGID_KEEPDIRECTORIES,
    OPTIONS_CFGID_USETARGETPATH,
    OPTIONS_CFGID_LASTTARGETPATH,
    OPTIONS_CFGID_SHOWJOBREPORT,
    OPTIONS_CFGID_REMOVESOURCEFILES,
    OPTIONS_CFGID_OVERWRITEEXISTING,
    OPTIONS_CFGID_NOCACHE,
    OPTIONS_CFGID_KEEPDATETIME,
    OPTIONS_CFGID_KEEPATTRIBUTES,

    OPTIONS_CFGID_IGNORECRC32,
    OPTIONS_CFGID_RESTOREPATH,
    OPTIONS_CFGID_ACCEPTTRUNCATED
  );

  JOBOPTS_WIPE : packed array[0..9] of PChar = (

    OPTIONS_CFGID_CONFIRMOPERATIONS,

    OPTIONS_CFGID_EXCLUDEARCHIVE,
    OPTIONS_CFGID_EXCLUDEREADONLY,
    OPTIONS_CFGID_EXCLUDEHIDDEN,
    OPTIONS_CFGID_EXCLUDESYSTEM,

    OPTIONS_CFGID_WIPING,
    OPTIONS_CFGID_REMOVEEMPTYDIRS,

    OPTIONS_CFGID_CONFIRMOPERATIONS,
    OPTIONS_CFGID_MAXERRORS,
    OPTIONS_CFGID_CLOSEAFTERWORK
  );

  JOBOPTS_REENCRYPT : packed array[0..12] of PChar = (

    OPTIONS_CFGID_CONFIRMOPERATIONS,

    OPTIONS_CFGID_USECACHEDKEY,
    OPTIONS_CFGID_SHOWPASSWORD,
    OPTIONS_CFGID_USEKEYDISK,
    OPTIONS_CFGID_CACHEKEY,
    OPTIONS_CFGID_AUTOCONFIRMATION,

    OPTIONS_CFGID_EXCLUDEARCHIVE,
    OPTIONS_CFGID_EXCLUDEREADONLY,
    OPTIONS_CFGID_EXCLUDEHIDDEN,
    OPTIONS_CFGID_EXCLUDESYSTEM,

    OPTIONS_CFGID_CONFIRMOPERATIONS,
    OPTIONS_CFGID_MAXERRORS,

    OPTIONS_CFGID_ACCEPTTRUNCATED
  );

  JOBOPTS_DESLACK : packed array[0..4] of PChar = (

    OPTIONS_CFGID_CONFIRMOPERATIONS,

    OPTIONS_CFGID_EXCLUDEARCHIVE,
    OPTIONS_CFGID_EXCLUDEREADONLY,
    OPTIONS_CFGID_EXCLUDEHIDDEN,
    OPTIONS_CFGID_EXCLUDESYSTEM
  );


type
  PLookup = ^TLookup;
  TLookup = record
    nTypeCode     : Integer;
    blNeedsCipher : Boolean;
    nNumOfOpts    : Integer;
    pOptionsTable : Pointer;
  end;

const
  JOBTYPE_ENCRYPT : TLookup = (nTypeCode     : BFJOB_MODE_ENCRYPT;
                               blNeedsCipher : True;
                               nNumOfOpts    : Length(JOBOPTS_ENCRYPT);
                               pOptionsTable : @JOBOPTS_ENCRYPT);

  JOBTYPE_DECRYPT : TLookup = (nTypeCode     : BFJOB_MODE_DECRYPT;
                               blNeedsCipher : True;
                               nNumOfOpts    : Length(JOBOPTS_DECRYPT);
                               pOptionsTable : @JOBOPTS_DECRYPT);

  JOBTYPE_WIPE : TLookup = (nTypeCode     : BFJOB_MODE_WIPE;
                            blNeedsCipher : False;
                            nNumOfOpts    : Length(JOBOPTS_WIPE);
                            pOptionsTable : @JOBOPTS_WIPE);

  JOBTYPE_REENCRYPT : TLookup = (nTypeCode     : BFJOB_MODE_REENCRYPT;
                                 blNeedsCipher : True;
                                 nNumOfOpts    : Length(JOBOPTS_REENCRYPT);
                                 pOptionsTable : @JOBOPTS_REENCRYPT);

  JOBTYPE_DESLACK : TLookup = (nTypeCode     : BFJOB_MODE_DESLACK;
                               blNeedsCipher : False;
                               nNumOfOpts    : Length(JOBOPTS_DESLACK);
                               pOptionsTable : @JOBOPTS_DESLACK);

var
  JOBTYPE_LOOKUP : array[0..4] of PLookup = (
    @JOBTYPE_ENCRYPT,
    @JOBTYPE_DECRYPT,
    @JOBTYPE_WIPE,
    @JOBTYPE_REENCRYPT,
    @JOBTYPE_DESLACK
  );



// some lookup access helpers

function GetLookup(nType : Integer) : PLookup;
begin
  case nType of
    BFJOB_MODE_ENCRYPT   : Result:=JOBTYPE_LOOKUP[0];
    BFJOB_MODE_DECRYPT   : Result:=JOBTYPE_LOOKUP[1];
    BFJOB_MODE_WIPE      : Result:=JOBTYPE_LOOKUP[2];
    BFJOB_MODE_REENCRYPT : Result:=JOBTYPE_LOOKUP[3];
    BFJOB_MODE_DESLACK   : Result:=JOBTYPE_LOOKUP[4];
  else
    Result:=Nil;
  end;
end;

function GetJobOpts(pLU : PLookup; nIndex : Integer) : String;
var
  nD1 : Integer;
  pD2 : ^Integer;
begin
  // dirty, dirty
  nD1:=Integer(pLU^.pOptionsTable);
  Inc(nD1, nIndex shl 2); // (a pointer is 32bit large)
  pD2:=Pointer(nD1);
  Result:=String(PChar(pD2^));
end;



// some IDs
const
  SECTION_HEADER = 'BFJOB_BATCH';
  SECTION_COUNT  = 'COUNT';
  JOB_PREFIX       = 'JOB';
  JOB_TITLE        = 'TITLE';
  JOB_TYPE         = 'TYPE';
  JOB_CIPHER       = 'CIPHER';
  JOB_CIPHER2      = 'CIPHER2';
  JOB_FILESCOUNT   = 'FILESCOUNT';
  JOB_FILEPREFIX   = 'FILE';
  JOB_FOLDERSCOUNT = 'FOLDERSCOUNT';
  JOB_FOLDERPREFIX = 'FOLDER';
  JOB_OPTSCOUNT    = 'OPTIONSCOUNT';



constructor TBFJobBatch.Create(sr : TStrRes;
                               opts : TOptions;
                               cipMng : TCipherManager);
begin
  m_sr:=sr;
  m_opts:=opts;
  m_cipMng:=cipMng;
  m_blLoaded:=False;
end;

destructor TBFJobBatch.Destroy;
begin
  Clear;
end;

procedure TBFJobBatch.Save(job : TBFJob;
                           const sFileName : String;
                           blAppend : Boolean);
var
  nI, nJ     : Integer;
  nNumOfJobs : Integer;
  pLkUp      : PLookup;
  sTemp      : String;
  sTemp2     : String;
  tmpList    : TStringList;
  tmpList2   : TStringList;
  cfg        : TConfiguration;
  sect       : TConfigurationSection;
begin

  // no simple jobs, please
  if (job.IsSimple) then
    RunError(RUNERROR_BFJOBBATCH_NOSIMPLEJOB);

  // toggle the append flag, if necessary
  if (blAppend) then
    blAppend:=FileExists(sFileName);

  // delete previous loaded data
  Clear;

  // load the existing job file
  if (blAppend) then begin
    try
      Load(sFileName);
    except
      on ebje : EBFJobError do begin
        raise EBFJobError.Create(
          Format(m_sr.Get(STRRES_ID, 'BATCH_APPENDLOADERR'), [ebje.Message]));
      end;
    end;
  end
  else begin

    // new jobfile needs these creations
    m_jobs:=TList.Create;
    m_cfgIDs:=TList.Create;
    m_cfgCnts:=TList.Create;
    m_ciphers:=TStringList.Create;
    m_ciphers2:=TStringList.Create;
    m_blLoaded:=True;
  end;

  // add the current job...

  // get the lookup info
  pLkUp:=GetLookup(job.GetMode);
  if (pLkUp = Nil) then
    RunError(RUNERROR_BFJOBBATCH_UNKNOWNMODE);

  // store the options and their values (as strings)
  // (FIXME: we take advantage of the configuration not to check the type of an
  //         option, so even bools and ints can be retrieved as strings, good?)
  tmpList:=TStringList.Create;
  tmpList.BeginUpdate;
  tmpList2:=TStringList.Create;
  tmpList2.BeginUpdate;
  for nI:=0 to (pLkUp^.nNumOfOpts - 1) do begin
    sTemp:=GetJobOpts(pLkUp, nI);
    tmpList.Add(sTemp);
    tmpList2.Add(m_opts.GetCfg.GetStringOption(sTemp));
  end;
  tmpList2.EndUpdate;
  tmpList.EndUpdate;
  m_cfgIDs.Add(tmpList);
  m_cfgCnts.Add(tmpList2);

  // store the current cipher (dependant on what is really needed)
  if (pLkUp^.blNeedsCipher) then
    if (job.GetMode = BFJOB_MODE_REENCRYPT) then begin
      m_cipMng.GetReencryptionCiphers(sTemp, sTemp2);
      m_ciphers.Add(sTemp);
      m_ciphers2.Add(sTemp2);
    end
    else begin
      m_ciphers.Add(m_cipMng.GetCurrentCipher);
      m_ciphers2.Add('');
    end
  else begin
    m_ciphers.Add('');
    m_ciphers2.Add('');
  end;

  // now comes the big storing...
  m_jobs.Add(job);

  // we're using a configuration object to write the data to a file
  cfg:=TConfiguration.Create;
  nNumOfJobs:=m_cfgIDs.Count;

  // create the header section
  sect:=cfg.GetSection(SECTION_HEADER);
  sect.FixIntegerOption(SECTION_COUNT, nNumOfJobs, True);

  // save all jobs
  for nI:=0 to (nNumOfJobs - 1) do begin

    // get a job
    job:=TBFJob(m_jobs.Items[nI]);

    // new section for the job
    sect:=cfg.GetSection(JOB_PREFIX + IntToStr(nI));
    with sect do begin

      // save title, type and ciphers (if necessary)
      FixStringOption(JOB_TITLE, job.GetTitle, True);
      FixIntegerOption(JOB_TYPE, job.GetMode, True);
      sTemp:=m_ciphers.Strings[nI];
      if (sTemp <> '') then begin
        FixStringOption(JOB_CIPHER, sTemp, True);
        sTemp:=m_ciphers2.Strings[nI];
        if (sTemp <> '') then
          FixStringOption(JOB_CIPHER2, sTemp, True);
      end;

      // now files and folders
      tmpList:=job.GetFileInfo.GetFiles;
      FixIntegerOption(JOB_FILESCOUNT, tmpList.Count, True);
      for nJ:=0 to (tmpList.Count - 1) do begin
        FixStringOption(JOB_FILEPREFIX + IntToStr(nJ),
                        tmpList.Strings[nJ],
                        True);
      end;
      tmpList:=job.GetFileInfo.GetFolders;
      FixIntegerOption(JOB_FOLDERSCOUNT, tmpList.Count, True);
      for nJ:=0 to (tmpList.Count - 1) do begin
        FixStringOption(JOB_FOLDERPREFIX + IntToStr(nJ),
                        tmpList.Strings[nJ],
                        True);
      end;

      // at last the options
      tmpList:=m_cfgIDs.Items[nI];
      tmpList2:=m_cfgCnts.Items[nI];
      FixIntegerOption(JOB_OPTSCOUNT, tmpList.Count, True);
      for nJ:=0 to (tmpList.Count - 1) do begin
        FixStringOption(tmpList.Strings[nJ],
                        tmpList2.Strings[nJ],
                        True);
      end;
    end;
  end;

  // wrote the whole bunch to the file
  try
    cfg.SaveToFile(sFileName);
  except
    on EConfigurationError do begin
      Clear;
      cfg.Destroy;
      raise EBFJobError.Create(m_sr.Get(STRRES_ID, 'BATCH_CANNOTSAVE'));
    end;
  end;

  // clean up
  cfg.Destroy;
  // don't remove the job passed in the aprameter, it doesn't belong to us)
  if (m_jobs.Count > 0) then
    m_jobs.Delete(m_jobs.Count - 1);
  Clear;
end;



procedure TBFJobBatch.Load(const sFileName : String);
var
  nI, nJ     : Integer;
  nTemp      : Integer;
  nType      : Integer;
  nNumOfJobs : Integer;
  sTemp      : String;
  pLkUp      : PLookUp;
  cfg        : TConfiguration;
  sect       : TConfigurationSection;
  cfgIDs     : TStringList;
  cnts       : TStringList;
  files      : TStringList;
  folders    : TStringList;
  jobToAdd   : TBFJob; 

procedure ErrorCleanUp;
begin
  if (folders <> Nil) then
    folders.Destroy;
  if (files <> Nil) then
    files.Destroy;
  cfg.Destroy;
  Clear;
end;

begin

  // init.
  files:=Nil;
  folders:=Nil;

  // delete existing stuff
  Clear;

  // load the batch file
  cfg:=TConfiguration.Create;
  try
    cfg.LoadFromFile(sFileName);
  except
    on EConfigurationError do begin
      raise EBFJobError.Create(m_sr.Get(STRRES_ID, 'BATCH_LOADERR'));
    end;
  end;

  // check the header
  if (not cfg.SectionExists(SECTION_HEADER)) then begin
    ErrorCleanUp;
    raise EBFJobError.Create(m_sr.Get(STRRES_ID, 'BATCH_NOTAJOBFILE'));
  end;

  try
    // get the number of job files
    sect:=cfg.GetSection(SECTION_HEADER);
    nNumOfJobs:=sect.GetIntegerOption(SECTION_COUNT);

    // create the lists for storing the jobs and options
    m_jobs:=TList.Create;
    m_cfgIDs:=TList.Create;
    m_cfgCnts:=TList.Create;
    m_ciphers:=TStringList.Create;
    m_ciphers2:=TStringList.Create;
    m_blLoaded:=True;

    // load the jobs
    for nI:=0 to (nNumOfJobs - 1) do begin

      sect:=cfg.GetSection(JOB_PREFIX + IntToStr(nI));
      with sect do begin

        // get the title, type and ciphers
        nType:=GetIntegerOption(JOB_TYPE);
        pLkUp:=GetLookup(nType);
        if (pLkUp^.blNeedsCipher) then begin
          if (nType = BFJOB_MODE_REENCRYPT) then begin
            m_ciphers.Add(GetStringOption(JOB_CIPHER));
            m_ciphers2.Add(GetStringOption(JOB_CIPHER2));
          end
          else begin
            m_ciphers.Add(GetStringOption(JOB_CIPHER));
            m_ciphers2.Add('');
          end
        end
        else begin
          m_ciphers.Add('');
          m_ciphers2.Add('');
        end;

        // now all options
        cfgIDs:=TStringList.Create;
        cnts:=TStringList.Create;
        m_cfgIDs.Add(cfgIDs);
        m_cfgCnts.Add(cnts);
        for nJ:=0 to (pLkUp^.nNumOfOpts - 1) do begin
          sTemp:=GetJobOpts(pLkUp, nJ);
          cfgIDs.Add(sTemp);
          cnts.Add(GetStringOption(sTemp));
        end;

        // at last the files and folders
        files:=TStringList.Create;
        files.BeginUpdate;
        nTemp:=GetIntegerOption(JOB_FILESCOUNT);
        for nJ:=0 to (nTemp - 1) do
          files.Add(GetStringOption(JOB_FILEPREFIX + IntToStr(nJ)));
        files.EndUpdate;
        folders:=TStringList.Create;
        folders.BeginUpdate;
        nTemp:=GetIntegerOption(JOB_FOLDERSCOUNT);
        for nJ:=0 to (nTemp - 1) do
          folders.Add(GetStringOption(JOB_FOLDERPREFIX + IntToStr(nJ)));
        folders.EndUpdate;

        // create the job
        jobToAdd:=TBFJob.Create(nType,
                                TBFJobFileInfo.Create(files, folders),
                                TBFJobEnvironment.Create(m_opts, m_cipMng),
                                nI);
        jobToAdd.SetTitle(GetStringOption(JOB_TITLE));
        m_jobs.Add(jobToAdd);

        files:=Nil;
        folders:=Nil;
      end;
    end;
    cfg.Destroy;

  except
    on EConfigurationError do begin
      ErrorCleanUp;
      raise EBFJobError.Create(m_sr.Get(STRRES_ID, 'BATCH_INVALIDERR'));
    end;
    on e : Exception do begin
      // (better to get all other errors, too)
      ErrorCleanUp;
      raise EBFJobError.Create(e.Message);
    end;
  end;


end;


procedure TBFJobBatch.Clear;
var
  nI : Integer;
begin
  if (m_blLoaded) then begin
    // (we might have been interrupted by an exception,
    //  so clear the lists individually)
    for nI:=0 to (m_jobs.Count - 1) do
      TBFJob(m_jobs.Items[nI]).Destroy;
    for nI:=0 to (m_cfgIDs.Count - 1) do
      TStringList(m_cfgIDs.Items[nI]).Destroy;
    for nI:=0 to (m_cfgCnts.Count - 1) do
      TStringList(m_cfgCnts.Items[nI]).Destroy;
    m_jobs.Destroy;
    m_cfgIDs.Destroy;
    m_cfgCnts.Destroy;
    m_ciphers.Destroy;
    m_ciphers2.Destroy;
    m_blLoaded:=False;
  end;
end;


function TBFJobBatch.GetJobCount : Integer;
begin
  Result:=m_jobs.Count;
end;


function TBFJobBatch.GetJob(nIndex : Integer) : TBFJob;
begin
  Result:=m_jobs.Items[nIndex];
end;


procedure TBFJobBatch.InvokeEnvironment(nIndex : Integer);
var
  nX     : Integer;
  nType  : Integer;
  cfgIDs : TStringList;
  cnts   : TStringList;
begin

  // set the cipher(s), if necessary
  nType:=TBFJob(m_jobs.items[nIndex]).GetMode;
  if (nType = BFJOB_MODE_REENCRYPT) then
    m_cipMng.SetReencryptionCiphers(m_ciphers.Strings[nIndex],
                                    m_ciphers2.Strings[nIndex],
                                    False)
  else
    if ((nType = BFJOB_MODE_ENCRYPT) or
        (nType = BFJOB_MODE_DECRYPT)) then
      m_cipMng.SetCurrentCipher(m_ciphers.Strings[nIndex], False);

  // invoke all options
  cfgIDs:=m_cfgIDs.Items[nIndex];
  cnts:=m_cfgCnts.Items[nIndex];
  for nX:=0 to (cfgIDs.Count - 1) do
    m_opts.GetCfg.SetStringOption(cfgIDs.Strings[nX], cnts.Strings[nX]);
end;


procedure TBFJobBatch.ResetEnvironment;
begin
  // it's easy!
  m_cipMng.Restore;
  m_opts.GetCfg.Restore;
end;




end.
