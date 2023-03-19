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
  for wiping multiple files
}

unit WipeManager;

interface
uses
  BFManager,
  PathSearch,
  IntLists,
  RandomSource,
  WorkResults,
  StringRes;


// to tell us what to wipe out
type
  TWipeSetup = class
  private
    m_files      : TPathSearchContainer;
    m_fileSizes  : TWORD64List;
    m_nMode      : Integer;

  public
    // constructor
    // -> files (reference must be kept alive)
    // -> file sizes (reference must be kept alive)
    // -> wipe mode, see BFM_WIPE_xxx
    constructor Create(files : TPathSearchContainer;
                       fileSizes : TWORD64List;
                       nMode : Integer);

    // gets the files
    // <- files
    function GetFiles : TPathSearchContainer;

    // gets the file sizes
    // <- file sizes
    function GetFileSizes : TWORD64List;

    // gets the mode
    // <- mode
    function GetMode : Integer;
  end;


// we need results!
type
  TWipeResults = class (TWorkResults)
  private
    m_sr : TStrRes;

  public
    // constructor
    // -> string resources
    constructor Create(sr : TStrRes); reintroduce; overload;

    // gets the dimension (number of columns)
    // <- dimension
    function GetDimension : Integer; override;

    // gets the list column information
    // <- column infos
    function GetColInfos : TWorkResultsColumnInfos; override;

    // renders a result into a table row
    // -> number of result
    // -> array where to store the entries (correctly sized!)
    // <- table row
    procedure Render(nIndex : Integer;
                     var rows : array of String); override;
  end;


// meet the wipe manager
type
  TWipeManager = class (TBFBaseManager)
  public
    // wipes files
    // -> what and how to wipe
    // -> reference which keeps the results
    // exception: EBFError if an error occured
    // exception: EBFFatalError if a fatal error occured
    // exception: EBFInterrupt if the process was interrupted
    procedure WipeFiles(setup : TWipeSetup;
                        var vResults : TWipeResults);
  end;


implementation
uses
  bfacslib,
  SysUtils,
  Wipe,
  CallBack,
  FileSupp,
  StringPlus;


// string resources ID
const
  STRRES_ID = 'WipeManager';



//////////////////////////// TWipeSetup ////////////////////////////


constructor TWipeSetup.Create(files : TPathSearchContainer;
                              fileSizes : TWORD64List;
                              nMode : Integer);
begin
  m_files:=files;
  m_fileSizes:=fileSizes;
  m_nMode:=nMode;
end;

function TWipeSetup.GetFiles : TPathSearchContainer;
begin
  Result:=m_files;
end;

function TWipeSetup.GetFileSizes : TWORD64List;
begin
  Result:=m_fileSizes;
end;

function TWipeSetup.GetMode : Integer;
begin
  Result:=m_nMode;
end;



//////////////////////////// TWipeResults ////////////////////////////

const
  RESULT_DIMENSION = 2;
  COL_NAME   = 0;
  COL_RESULT = 1;

constructor TWipeResults.Create(sr : TStrRes);
begin
  inherited Create;
  m_sr:=sr;
end;

function TWipeResults.GetDimension : Integer;
begin
  Result:=RESULT_DIMENSION;
end;

function TWipeResults.GetColInfos : TWorkResultsColumnInfos;
begin
  Result:=TWorkResultsColumnInfos.Create;
  with Result, m_sr do begin
    AddInfo(WORKRESULTS_COLALIGN_LEFT, 70, Get(STRRES_ID, 'COL_NAME'));
    AddInfo(WORKRESULTS_COLALIGN_LEFT, 30, Get(STRRES_ID, 'COL_RESULT'));
  end;
end;

procedure TWipeResults.Render(nIndex : Integer;
                              var rows : array of String);
begin
  // there are always two of them, a filename and a result
  rows[COL_NAME]:=TStringKeeper(m_results.Items[nIndex]).Get;
  case GetEntryType(nIndex) of
    WORKRESULTS_TYPE_SUCCESS:
      rows[COL_RESULT]:=m_sr.Get(STRRES_ID, 'SUCCESS');
    WORKRESULTS_TYPE_ERROR:
      rows[COL_RESULT]:=m_sr.Get(STRRES_ID, 'ERROR') +
                        m_errorMessages.Strings[nIndex];
    WORKRESULTS_TYPE_WARNING:
      rows[COL_RESULT]:=m_sr.Get(STRRES_ID, 'WARNING') +
                        m_errorMessages.Strings[nIndex];
  else
    rows[COL_RESULT]:=m_errorMessages.Strings[nIndex];
  end;

end;


//////////////////////////// TWipeManager ////////////////////////////


// internal callback to map progress reports
type
  TProgressMapper = class(TWipeProgress)
  private
    m_qAssumedFileSize : WORD64;
    m_qStartLevel      : WORD64;
  public
    constructor Create(callBackObj : TObject); override;
    procedure SetAssumedFileSize(qSize : WORD64);
    procedure IncStartLevel(qX : WORD64);
    procedure ZeroStartLevel;
    procedure CallBack; override;
  end;

constructor TProgressMapper.Create(callBackObj : TObject);
begin
  inherited Create(callBackObj);
  ZeroStartLevel;
end;

procedure TProgressMapper.SetAssumedFileSize(qSize : WORD64);
begin
  m_qAssumedFileSize:=qSize;
end;

procedure TProgressMapper.IncStartLevel(qX : WORD64);
begin
  Inc(m_qStartLevel, qX);
  with GetCallBackObj as TBFWorkProgress do
    SetActPos(m_qStartLevel);
end;

procedure TProgressMapper.ZeroStartLevel;
begin
  m_qStartLevel:=0;
  with GetCallBackObj as TBFWorkProgress do SetActPos(0);
end;


procedure TProgressMapper.CallBack;
var
  qNewLevel : WORD64;
  nPass     : Integer;
  wp        : TBFWorkProgress;
begin
  // get the progress reporter
  wp:=TBFWorkProgress(GetCallBackObj);

  // due to the special characteristics of wipe progress callbacks the logic
  // here is some kind of complicated
  nPass:=GetPass;

  // no local progress at all?
  if (nPass <> WIPE_PASS_DELETEONLY) then begin

    qNewLevel:=GetActPos;

    // multiple  loops?
    if (nPass <> WIPE_PASS_NOLOOPS) then begin

      // we have to recalculate the current files size equivalent
      // to check for jitters
      qNewLevel:=qNewLevel div GetNumOfPasses;

      // refresh the loop numbers, if necessary
      if (GetChanged) then begin
        wp.SetWipeLoop(GetPass);
        wp.SetNumOfWipes(GetNumOfPasses);
      end;

    end;

//    debd('qNewLevel', qNewLevel);
//    debd('m_qAssumedFileSize', m_qAssumedFileSize);

    // (avoid progress jitter)
    if (qNewLevel <= m_qAssumedFileSize) then
      wp.SetActPos(m_qStartLevel + qNewLevel)
    else
      wp.SetActPos(m_qStartLevel + m_qAssumedFileSize);

    // the progress of the _real_ file size (or the bytes to wipe altogether
    // respecitvely) is separated, and thus we can always report the truth
    wp.SetBytesDone(GetActPos);

    // (same with the file size)
    if (GetChanged) then begin
      wp.SetFileSize(GetFileSize);
      wp.SetNumOfBytes(GetMaxPos);
    end;
  end;

  // call the parent back now
  wp.SetChanged(GetChanged);
  wp.SetCalled(True);
  wp.CallBack;
  wp.SetFirstCall(False);
  wp.SetChanged(False);
end;



procedure TWipeManager.WipeFiles(setup : TWipeSetup;
                                 var vResults : TWipeResults);
var
  qFileSize  : WORD64;
  qMaxPos    : WORD64;
  nI         : Integer;
  nUpIdx     : Integer;
  nMode      : Integer;
  blWasError : Boolean;
  sFileName  : String;
  pmapp      : TProgressMapper;
  wipeObj    : TWipe;
  clad       : TFileClusterAdjuster;

procedure CleanUp;
begin
  if (wipeObj <> Nil) then
    wipeObj.Destroy;
  if (clad <> Nil) then
    clad.Destroy;
  if (pmapp <> Nil) then
    pmapp.Destroy;
end;

begin

  // some zeroing first
  clad:=Nil;
  wipeObj:=Nil;
  pmapp:=Nil;

  // init the work progress
  with m_workProgress do begin

    SetNumOfFiles(setup.GetFiles.GetNumOfFiles);
    SetWipeAfterEncryption(False);

    nMode:=setup.GetMode;
    if (nMode = BFM_WIPE_DELETEONLY) then begin
      SetMaxPos(setup.GetFiles.GetNumOfBytes);
    end
    else begin
      // due to cluster adjustments we have calculate the all over size
      nUpIdx:=setup.GetFiles.GetNumOfFiles - 1;
      clad:=TFileClusterAdjuster.Create;
      qMaxPos:=0;
      for nI:=0 to nUpIdx do begin
        if (nI = 0) then
          clad.ClusterAdjust(setup.GetFiles.GetFirstFile,
                             setup.GetFileSizes.Get(nI),
                             qFileSize)
        else
          clad.ClusterAdjust(setup.GetFiles.GetNextFile,
                             setup.GetFileSizes.Get(nI),
                             qFileSize);
        Inc(qMaxPos, qFileSize);
      end;
      SetMaxPos(qMaxPos);
    end;

    case nMode of
      BFM_WIPE_DELETEONLY : begin
        SetNoWipeLoops(True);
        SetDeleteOnly(True);
      end;
      BFM_WIPE_SIMPLE : begin
        SetNoWipeLoops(True);
        SetDeleteOnly(False);
      end;
    else
      SetNoWipeLoops(False);
      SetDeleteOnly(False);
    end;

    SetFirstCall(True);
    SetMode(BFM_PROGRESS_WIPE);
    SetActPos(0);
  end;

  // make a new progress mapper
  pmapp:=TProgressMapper.Create(m_workProgress);

  // prepare the wipe object
  try
    case nMode of
      BFM_WIPE_DELETEONLY : wipeObj:=TWipeDeleteOnly.Create;
      BFM_WIPE_SIMPLE     : wipeObj:=TWipeSimple.Create(m_randomSource);
      BFM_WIPE_DOD        : wipeObj:=TWipeDOD.Create(m_randomSource);
      BFM_WIPE_SFS        : wipeObj:=TWipeSFS.Create;
    end;
  except
    on EOutOfMemory do begin
      Cleanup; 
      raise EBFFatalError.Create(m_sr.Get(STRRES_ID, 'OUTOFMEMORY'));
    end;
  end;

  // create the result keeper
  vResults:=TWipeResults.Create(m_sr);

  // wipe out
  ResetErrorCounter;
  nUpIdx:=setup.GetFiles.GetNumOfFiles;


  for nI:=1 to nUpIdx do begin

    blWasError:=False;

    if (nI = 1) then
      sFileName:=setup.GetFiles.GetFirstFile
    else
      sFileName:=setup.GetFiles.GetNextFile;

    with m_workProgress do begin
      SetFileNumber(nI);
      SetBytesDone(0);
      SetFileName(sFileName);
    end;

    qFileSize:=setup.GetFileSizes.Get(nI - 1);
    with pmapp do begin
      // (same adjustment as the one above)
      if (nMode = BFM_WIPE_DELETEONLY) then
        SetAssumedFileSize(qFileSize)
      else begin
        clad.ClusterAdjust(sFileName, qFileSize, qFileSize);
        SetAssumedFileSize(qFileSize);
      end;
    end;

    m_workProgress.SetCalled(False);

    try
      wipeObj.Execute(sFileName,
                      pmapp,
                      m_sr);
      vResults.Add(TStringKeeper.Create(sFileName),
                   WORKRESULTS_TYPE_SUCCESS);
    except

      on ewwb : EWipeWasBreak do begin
        vResults.Add(TStringKeeper.Create(sFileName),
                     WORKRESULTS_TYPE_BREAK,
                     ewwb.Message);
        CleanUp;
        raise EBFInterrupt.Create(ewwb.Message);
      end;
      on ewf : EWipeFatal do begin
        vResults.Add(TStringKeeper.Create(sFileName),
                     WORKRESULTS_TYPE_ERROR,
                     ewf.Message);
        CleanUp;
        raise EBFFatalError.Create(ewf.Message);
      end;
      on ewe : EWipeError do begin
        vResults.Add(TStringKeeper.Create(sFileName),
                     WORKRESULTS_TYPE_ERROR,
                     ewe.Message);
        blWasError:=True;
      end;
    end;

    // if no callback was done we fire at least one to keep the interrupt
    // possibility alive)
    with m_workProgress do begin
      if (not GetCalled) then begin
        pmapp.SetActPos(0);
        SetFileName(sFileName);
        SetBytesDone(0);
        SetFileSize(qFileSize);
        SetNumOfBytes(qFileSize);
        SetChanged(True);
        try
          CallBack;
        except
          on ecbi : ECallBackInterrupt do begin
            CleanUp;
            raise EBFInterrupt.Create(ecbi.Message);
          end;
        end;
        SetFirstCall(False);
      end;
    end;

    // too many errors?
    if (blWasError) then begin
      if (not IncAndCheckMaxErrors) then begin
        CleanUp;
        raise EBFInterrupt.Create(m_sr.Get(STRRES_ID, 'USERBREAK'));
      end;
    end;

    // next file
    pmapp.IncStartLevel(qFileSize);
  end;

  // done
  CleanUp;
end;



end.
