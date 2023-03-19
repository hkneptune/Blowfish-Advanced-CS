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
  the startup module

  25.7.04 Pamp001 build in due to correct commandline with spaces
}

unit Startup;

interface
uses
  Messages,
  Classes,
  CallBack,
  Configuration,
  BFJob;


// result codes of startup process
const
  STARTUP_RES_NOJOB    = 0;
  STARTUP_RES_JOB      = 1;
  STARTUP_RES_JOBFILES = 2;
  STARTUP_RES_ERROR    = -1;

// request callback modes
const
  JRCB_REQMODE_ONEFILE    = 0;
  JRCB_REQMODE_ONEBFAFILE = 1;
  JRCB_REQMODE_FILES      = 2;
  JRCB_REQMODE_BFAFILES   = 3;
  JRCB_REQMODE_FOLDERS    = 4;

// to request what kind of job should be created
type
  TJobRequestCallBack = class(TCallBack)
  protected
    m_nJobMode     : Integer;
    m_nRequestMode : Integer;
  public
    // gets the job mode wanted
    // <- job mode (see BFJOB_MODE_xx)
    function GetJobMode : Integer;

    // to let the callback decide to offer only such modes
    // which will make sense
    // -> request mode (see JRCB_REQMODE_xx)
    procedure SetRequestMode(nRequestMode : Integer);
  end;


type
  TStartup = class
  protected
    // separates files and folders out of an object list
    // -> objects to separate
    // -> where to create the files
    // -> where to create the folders
    class procedure SeparateFilesAndFolders(objs : TStringList;
                                            var files : TStringList;
                                            var folders : TStringList);

  public
    // the startup function
    // -> configuration
    // -> where to create the job, if necessary
    // -> where to create a list with jobfiles, if necessary
    // -> pointer to a drop message (ignored, if Nil)
    // <- result code, see STARTUP_RES_xx
    class function Execute(jobReqCB : TJobRequestCallBack;
                           var jobFiles : TStringList;
                           var newJob : TBFJob;
                           pDropMsg : PMessage = Nil) : Integer;
  end;


implementation
uses
  Windows,
  ShellAPI,
  FileCtrl,
  SysUtils,
  GlobalsGUI,
  StringPlus,
  BFAFile,
  General,
  Options,
  ShellListener;


//////////////////////////// TJobRequestCallBack ////////////////////////////

function TJobRequestCallBack.GetJobMode : Integer;
begin
  Result:=m_nJobMode;
end;

procedure TJobRequestCallBack.SetRequestMode(nRequestMode : Integer);
begin
  m_nRequestMode:=nRequestMode;
end;



//////////////////////////// TStartup ////////////////////////////

class procedure TStartup.SeparateFilesAndFolders(objs : TStringList;
                                                 var files : TStringList;
                                                 var folders : TStringList);
var
  nI   : Integer;
  sObj : String;
begin
  files:=TStringList.Create;
  folders:=TStringList.Create;
  for nI:=0 to (objs.Count - 1) do begin
    sObj:=objs.Strings[nI];
    if (DirectoryExists(sObj)) then
      folders.Add(sObj)
    else
      files.Add(sObj);
  end;
end;


class function TStartup.Execute(jobReqCB : TJobRequestCallBack;
                                var jobFiles : TStringList;
                                var newJob : TBFJob;
                                pDropMsg : PMessage = Nil) : Integer;
var
  nNumOfObjects  : Integer;
  nMode          : Integer;
  nI             : Integer;
  blSJ           : Boolean;
  blJobFilesOnly : Boolean;
  blBFAFilesOnly : Boolean;
  blSingleFile   : Boolean;
  blFolders      : Boolean;
  hDrop          : THandle;
  objects        : TStringList;
  files          : TStringList;
  folders        : TStringList;
  jobEnv         : TBFJobEnvironment;
  nameBuf        : array[0..MAX_PATH] of Char;
  opts           : TOptions;
begin

  // create a job env.
  jobEnv:=TBFJobEnvironment.Create(_globals.GetOpts, _globals.GetCipMng);

  // get the options
  opts:=_globals.GetOpts;

  // startup behaviour or handling a dropped objects?
  if (pDropMsg = Nil) then begin

    // was a shell job launched?
    if (_blShellJobCatched) then begin

      // yes, but what kind of?
      blSJ:=False;
      case _nShellJobMode of
        SHELLJOB_ENCRYPT   : nMode:=BFJOB_MODE_ENCRYPT;
        SHELLJOB_DECRYPT   : nMode:=BFJOB_MODE_DECRYPT;
        SHELLJOB_WIPE      : nMode:=BFJOB_MODE_WIPE;
        SHELLJOB_REENCRYPT : nMode:=BFJOB_MODE_REENCRYPT;
        SHELLJOB_DESLACK   : nMode:=BFJOB_MODE_DESLACK;
        SHELLJOB_WORKWITH  : begin nMode:=BFJOB_MODE_WORKWITH; blSJ:=True; end;
        SHELLJOB_VIEW      : begin nMode:=BFJOB_MODE_VIEW; blSJ:=True; end;
      else
        Result:=STARTUP_RES_ERROR;
        Exit;
      end;

      // now make a job out of it
      if (blSJ) then
        newJob:=TBFJob.CreateSimple(nMode,
                                    String(PChar(_pShellRawData)),
                                    jobEnv)
      else begin
        objects:=TStrPlus.ExtractCStrings(_pShellRawData);
        SeparateFilesAndFolders(objects, files, folders);
        newJob:=TBFJob.Create(nMode,
                              TBFJobFileInfo.Create(files, folders),
                              TBFJobEnvironment.Create(opts,
                                                       _globals.GetCipMng),
                              BFJOB_NONUMBER);
        objects.Destroy;
      end;

      Result:=STARTUP_RES_JOB;
      Exit;
    end;

    // nothing in the command line?
    nNumOfObjects:=ParamCount;
    if (nNumOfObjects = 0) then begin
      jobEnv.Destroy;
      Result:=STARTUP_RES_NOJOB;
      Exit;
    end;

    // get the objects
    objects:=TStringList.Create;

    for nI:=1 to nNumOfObjects do begin
      objects.Add(TStrPlus.LongFileName(ParamStr(nI)));
    end;
  end
  else begin

    // get the dropped objects
    hDrop:=pDropMsg^.wParam;
    objects:=TStringList.Create;
    nNumOfObjects:=DragQueryFile(hDrop, ULONG(-1), nameBuf, MAX_PATH);
    nI:=0;
    while (nI < nNumOfObjects) do begin
      DragQueryFile(hDrop, nI, nameBuf, MAX_PATH);
      objects.Add(TStrPlus.LongFileName(String(PChar(@namebuf[0]))));
      Inc(nI);
    end;
    DragFinish(hDrop);
    pDropMsg^.Result:=0;
  end;

  // separate the objects
  SeparateFilesAndFolders(objects, files, folders);
  objects.Destroy;

  // now let's see what kind of files we have
  blFolders:=(0 < folders.Count);
  if (blFolders) then begin
    blJobFilesOnly:=False;
    blBFAFilesOnly:=False;
  end
  else begin
    blJobFilesOnly:=True;
    blBFAFilesOnly:=True;
    for nI:=0 to (files.Count - 1) do begin
      if (CompareText(TStrPlus.ExtractFileExtension(files.Strings[nI]),
                      BFAFILE_EXTENSION) <> 0) then
        blBFAFilesOnly:=False;
      if (CompareText(TStrPlus.ExtractFileExtension(files.Strings[nI]),
                      BFJFILE_EXTENSION) <> 0) then
        blJobFilesOnly:=False;
    end;
  end;

  // job files only?
  if (blJobFilesOnly) then begin
    folders.Destroy;
    jobFiles:=files;
    jobEnv.Destroy;
    Result:=STARTUP_RES_JOBFILES;
    Exit;
  end;

  // only one file?
  blSingleFile:=((folders.Count = 0) and (files.Count = 1));

  // one single BFA file is treated either to be viewed for to be work with
  // (FIXME: the former solution was to let the user decide via the callback,
  //         the code is still active (see below), but this direct way is much
  //         smarter, isn't it?)

  if (blSingleFile and blBFAFilesOnly) then begin
    if (opts.GetCfg.GetBooleanOption(OPTIONS_CFGID_DOUBLECLICKWORK)) then
      nMode:=BFJOB_MODE_WORKWITH
    else
      nMode:=BFJOB_MODE_VIEW;

    newJob:=TBFJob.CreateSimple(nMode,
                                files.Strings[0],
                                jobEnv);
    files.Destroy;
    folders.Destroy;
    Result:=STARTUP_RES_JOB;
    Exit;
  end;

  // so we ask what to do now
  with jobReqCB do begin
    if (blFolders) then
      SetRequestMode(JRCB_REQMODE_FOLDERS)
    else
      if (blBFAFilesOnly) then
        if (blSingleFile) then
          SetRequestMode(JRCB_REQMODE_ONEBFAFILE)
        else
          SetRequestMode(JRCB_REQMODE_BFAFILES)
      else
        if (blSingleFile) then
          SetRequestMode(JRCB_REQMODE_ONEFILE)
        else
          SetRequestMode(JRCB_REQMODE_FILES);
    try
      CallBack;
    except
      on ECallbackInterrupt do begin
        // forget that job
        files.Destroy;
        folders.Destroy;
        jobEnv.Destroy;
        Result:=STARTUP_RES_NOJOB;
        Exit;
      end;
    end;
    nMode:=GetJobMode;
  end;

  // time to create the job now
  if (nMode >= BFJOB_MODE_BORDER_SIMPLE) then begin
    newJob:=TBFJob.CreateSimple(nMode,
                                files.Strings[0],
                                jobEnv);
    files.Destroy;
    folders.Destroy;
  end
  else begin
    newJob:=TBFJob.Create(nMode,
                          TBFJobFileInfo.Create(files, folders),
                          jobEnv,
                          BFJOB_NONUMBER);
  end;

  // done
  Result:=STARTUP_RES_JOB;
end;


end.
