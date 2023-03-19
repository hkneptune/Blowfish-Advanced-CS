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
  everything to work with .BFA files
}

unit BFAManager;

{$I config.inc}

interface
uses classes,
     bfacslib,
     PathSearch,
     SecureMem,
     BFManager,
     RandomSource,
     MessageCallBack,
     IntLists,
     WorkResults,
     StringRes,
     BFAFile;


// base result class
type
  TBFAResults = class(TWorkResults)
  protected
    m_sr                 : TStrRes;
    m_blTargetPathExists : Boolean;

    // checks if a column for error, warnings, skips is necessary
    // True: add one / False: nope
    function DoLastCol : Boolean;

  public

    // constructor
    // -> string resources
    // -> setup context
    constructor Create(sr : TStrRes;
                       setup : TBFAFileSetup); reintroduce; overload;
  end;



// result class for encrypting files
type
  TBFAEncryptResults = class(TBFAResults)
  private
    m_blCompression : Boolean;

  public
    // constructor
    // -> the setup of the file encryption (e.g. to detect if files
    //    are renamed or not), reference isn't touched
    // -> string resources
    constructor Create(sr : TStrRes;
                       setup : TBFAFileEncryptSetup); reintroduce; overload;

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


// result class for decrypting files
type
  TBFADecryptResults = class(TBFAResults)
  public

    // gets the dimension (number of columns)
    // <- dimension
    function GetDimension : Integer; override;

    // gets the list column information
    // <- column infos
    function GetColInfos : TWorkResultsColumnInfos; override;

    // default table row renderer
    // -> number of result
    // -> array where to store the entries (correctly sized!)
    // <- table row
    procedure Render(nIndex : Integer;
                     var rows : array of String); override;
  end;




// base setup class
type
  TBFASetup = class
  protected
    m_files             : TPathSearchContainer;
    m_fileSizes         : TWORD64List;
    m_blRemoveEmptyDirs : Boolean;

  public
    // constructor
    // -> the files to add
    // -> sizes of the files to add
    constructor Create(files : TPathSearchContainer;
                       fileSizes : TWORD64List); virtual;

    // gets the single filenames
    // <- single filenames
    function GetFiles : TPathSearchContainer;

    // gets the file sizes
    // <- file sizes
    function GetFileSizes : TWORD64List;
  end;




// setup class to encrypt files
type
  TBFAEncryptSetup = class(TBFASetup)
  private
    m_options         : TBFAFileEncryptSetup;
    m_nWiping         : Integer;
    m_noCompressTypes : TStringList;

  public
    // constructor
    // -> files to add
    // -> sizes of the files to add
    // -> encryption options
    // -> wipe method (see BFM_WIPE_xxx)
    // -> list with file extensions not to compress
    constructor Create(files : TPathSearchContainer;
                       fileSizes : TWORD64List;
                       options : TBFAFileEncryptSetup;
                       nWiping : Integer); reintroduce; overload;

    // gets the (reference to the) options for encryption
    // <- options (plus the passed RNG for wiping)
    function GetOptions : TBFAFileEncryptSetup;

    // gets the wiping method
    // <- wiping method, see BFM_WIPE_xxx
    function GetWiping : Integer;

    // gets the reference to the no compression extension list
    // <- no compression extension list
    function GetNoCompressTypes : TStringList;
  end;



// setup class to decrypt files

{
  columns:
  - original file name
  - target file (plus path, if necessary)
  - target file size
  - errors, warnings, hints
}

type
  TBFADecryptSetup = class(TBFASetup)
  private
    m_options : TBFAFileDecryptSetup;

  public
    // constructor
    // -> files to add
    // -> sizes of the files to add
    // -> decryption options (destroyed later)
    constructor Create(files : TPathSearchContainer;
                       fileSizes : TWORD64List;
                       options : TBFAFileDecryptSetup); reintroduce; overload;

    // gets the (reference to the) options for decryption
    // <- options
    function GetOptions : TBFAFileDecryptSetup;
  end;


// we need to extend the standard progress callback
type
  TBFAWorkProgress = class(TBFWorkProgress)
  private
    m_sOutputFileName : String;
  public
    // gets the output file name of the currently processed file
    // <- output file name
    function GetOutputFileName : String;

    // sets the name of the currently processed file
    // -> output file name
    procedure SetOutputFileName(sOutputFileName : String);
  end;



// our manager
type
  TBFAManager = class(TBFManager)
  private
    m_BFAFile : TBFAFile;

  public

    // constructor
    // -> cipher name
    // -> random generator to set up cipher and wiping
    // -> max. number of errors before a request for abortion
    //    is sent to the user (or BFM_NOMAXERRORS)
    // -> work progress callback
    // -> confirmation callback
    // -> string resources
    // exception: EBError if an error occured
    constructor Create(sCipherName : String;
                       randomSource : TRandomSource;
                       nMaxErrors : Integer;
                       workProgress : TBFAWorkProgress;
                       confirmCB : TMessageCallBack;
                       sr : TStrRes); reintroduce; overload;

    // destructor
    destructor Destroy; override;

    // encrypts files
    // -> all the stuff needed to add files
    // -> reference which keeps the results (may be Nil on early error)
    // exception: EBFError if an error occured
    // exception: EBFFatalError if a fatal error occured
    // exception: EBFInterrupt if the process was interrupted
    procedure EncryptFiles(setup : TBFAEncryptSetup;
                           var vResults : TBFAEncryptResults);

    // decrypts files
    // -> all the stuff needed to extract files
    // -> reference which keeps the results (may be to Nil on early error)
    // exception: EBError if an error occured
    // exception: EBFFatalError if a fatal error occured
    // exception: EBFInterrupt if the process was interrupted
    procedure DecryptFiles(setup : TBFADecryptSetup;
                           var vResults : TBFADecryptResults);

  end;



implementation
uses Windows,
     SysUtils,
     StringPlus,
     StringPlusI,
     Win32Diagnosis,
     Wipe,
     General,
     CallBack;


// ID to get the strings
const
  STRRES_ID = 'BFAMANAGER';


//////////////////////////// TBFAResults ////////////////////////////


constructor TBFAResults.Create(sr : TStrRes;
                               setup : TBFAFileSetup);
begin
  inherited Create;
  m_sr:=sr;
  m_blTargetPathExists:=(setup.GetTargetPath <> '');
end;

function TBFAResults.DoLastCol : Boolean;
begin
  Result:=ErrorMessagesExist or
          (GetNumOfErrors > 0) or
          (GetNumOfWarnings > 0) or
          (GetNumOfSkips > 0);
end;



//////////////////////////// TBFAEncryptResults ////////////////////////////


{
  columns:
  - original file name
  - original file size
  - targetfile (plus path, if necessary)
  - compression ratio (if necessary)
  - errors, warnings, hints (if necessary)
}


constructor TBFAEncryptResults.Create(sr : TStrRes;
                                      setup : TBFAFileEncryptSetup);
begin
  inherited Create(sr, setup);

  // extract the necessary setup information
  m_blCompression:=(setup.GetCompress <> BFAFILE_COMPRESS_NONE);
end;


function TBFAEncryptResults.GetDimension : Integer;
begin
  Result:=3;
  if (m_blCompression) then
    Inc(Result);
  if (DoLastCol) then
    Inc(Result);
end;



function TBFAEncryptResults.GetColInfos : TWorkResultsColumnInfos;
const
  SZPERS : array [0..3, 0..4] of Integer = (
     (40, 20, 40,  0,  0),
     (35, 20, 35, 10,  0),
     (30, 20, 30,  0, 20),
     (25, 15, 25, 10, 25)
  );
var
  nT : Integer;
begin
  if (DoLastCol) then
    if (m_blCompression) then nT:=3 else nT:=2
  else
    if (m_blCompression) then nT:=1 else nT:=0;

  Result:=TWorkResultsColumnInfos.Create;

  Result.AddInfo(WORKRESULTS_COLALIGN_LEFT,
                 SZPERS[nT, 0],
                 m_sr.Get(STRRES_ID, '000'));

  Result.AddInfo(WORKRESULTS_COLALIGN_RIGHT,
                 SZPERS[nT, 1],
                 m_sr.Get(STRRES_ID, '001'));

  Result.AddInfo(WORKRESULTS_COLALIGN_LEFT,
                 SZPERS[nT, 2],
                 m_sr.Get(STRRES_ID, '002'));

  if (m_blCompression) then begin
    Result.AddInfo(WORKRESULTS_COLALIGN_RIGHT,
                   SZPERS[nT, 3],
                   m_sr.Get(STRRES_ID, '003'));
  end;

  if (DoLastCol) then begin
    Result.AddInfo(WORKRESULTS_COLALIGN_LEFT,
                   SZPERS[nT, 4],
                   m_sr.Get(STRRES_ID, '004'));
  end;
end;



procedure TBFAEncryptResults.Render(nIndex : Integer;
                                    var rows : array of String);
var
  qOrigBytes  : WORD64;
  qCompBytes  : WORD64;
  nI          : Integer;
  nPos        : Integer;
  nResultType : Integer;
  res         : TBFAFileEncryptResult;
begin

  // was the operation a success?
  nResultType:=GetEntryType(nIndex);
  if ((nResultType = WORKRESULTS_TYPE_SUCCESS) or
      (nResultType = WORKRESULTS_TYPE_WARNING)) then begin

    // yes, fill out all columns
    nPos:=0;
    res:=TBFAFileEncryptResult(m_results[nIndex]);
    rows[nPos]:=res.GetSourceFileName;
    Inc(nPos);
    qOrigBytes:=res.GetOriginalFileSize;
    rows[nPos]:=TStrPlusI.Sepa1000(m_sr, qOrigBytes);
    Inc(nPos);
    if (m_blTargetPathExists) then
      rows[nPos]:=res.GetBFAFileName
    else
      rows[nPos]:=ExtractFileName(res.GetBFAFileName);
    Inc(nPos);
    if (m_blCompression) then begin
      qCompBytes:=res.GetBytesCompressed;
      if (qCompBytes = BFAFILE_COMPRESSION_IGNORED) then
        rows[nPos]:=''
      else
        rows[nPos]:=TStrPlusI.CalcPercent(m_sr,
                                          qCompBytes,
                                          qOrigBytes,
                                          1) + '%';
      Inc(nPos);
    end;
    if (nResultType = WORKRESULTS_TYPE_WARNING) then
      rows[nPos]:=res.RenderWarnings
    else
      if (DoLastCol) then
        rows[nPos]:='';
  end
  else begin

    // no, so just the filename and the error/skip message
    rows[0]:=TStringKeeper(m_results[nIndex]).Get;
    for nI:=1 to (GetDimension - 2) do
      rows[nI]:='';
    rows[GetDimension - 1]:=m_errorMessages[nIndex];
  end;
end;




//////////////////////////// TBFADecryptResults ////////////////////////////



function TBFADecryptResults.GetDimension : Integer;
begin
  Result:=3;
  if (DoLastCol) then
    Inc(Result);
end;


function TBFADecryptResults.GetColInfos : TWorkResultsColumnInfos;
const
  SZPERS : array [0..1, 0..3] of Integer = (
     (30, 30, 15, 25),
     (40, 40, 20,  0)
  );
var
  nT : Integer;
begin
  Result:=TWorkResultsColumnInfos.Create;
  if (DoLastCol) then
    nT:=0
  else
    nT:=1;
  with Result, m_sr do begin
    AddInfo(WORKRESULTS_COLALIGN_LEFT, SZPERS[nT, 0],  Get(STRRES_ID, '005'));
    AddInfo(WORKRESULTS_COLALIGN_LEFT, SZPERS[nT, 1], Get(STRRES_ID, '006'));
    AddInfo(WORKRESULTS_COLALIGN_RIGHT, SZPERS[nT, 2],  Get(STRRES_ID, '007'));
    if (DoLastCol) then
      AddInfo(WORKRESULTS_COLALIGN_LEFT, SZPERS[nT, 3],  Get(STRRES_ID, '004'));
  end;
end;


procedure TBFADecryptResults.Render(nIndex : Integer;
                                    var rows : array of String);
var
  theEntry    : TBFAFileDecryptResult;
  nResultType : Integer;
  nPos        : Integer;
begin

  // was the operation a success?
  nResultType:=GetEntryType(nIndex);
  if ((nResultType = WORKRESULTS_TYPE_SUCCESS) or
      (nResultType = WORKRESULTS_TYPE_WARNING)) then begin

    // yes, fill out all columns
    nPos:=0;
    theEntry:=TBFAFileDecryptResult(m_results[nIndex]);
    rows[nPos]:=theEntry.GetSourceFileName;
    Inc(nPos);
    if (m_blTargetPathExists) then
      rows[nPos]:=theEntry.GetOriginalFileName
    else
      rows[nPos]:=ExtractFileName(theEntry.GetOriginalFileName);
    Inc(nPos);
    rows[nPos]:=TStrPlusI.Sepa1000(m_sr, theEntry.GetBytesWritten);
    Inc(nPos);
    if (nResultType = WORKRESULTS_TYPE_WARNING) then
      rows[nPos]:=theEntry.RenderWarnings
    else
      if (DoLastCol) then
        rows[nPos]:='';
  end
  else begin

    // no, so just the filename and the error/skip message
    nPos:=0;
    rows[nPos]:=TStringKeeper(m_results[nIndex]).Get;
    Inc(nPos);
    rows[nPos]:='';
    Inc(nPos);
    rows[nPos]:='';
    Inc(nPos);
    rows[nPos]:=m_errorMessages[nIndex];
  end;
end;


//////////////////////////// TBFASetup ////////////////////////////

constructor TBFASetup.Create(files : TPathSearchContainer;
                             fileSizes : TWORD64List);
begin
  m_files:=files;
  m_fileSizes:=fileSizes;
end;


function TBFASetup.GetFiles : TPathSearchContainer;
begin
  Result:=m_files;
end;


function TBFASetup.GetFileSizes : TWORD64List;
begin
  Result:=m_fileSizes;
end;



//////////////////////////// TBFAEncryptSetup ////////////////////////////


constructor TBFAEncryptSetup.Create(files : TPathSearchContainer;
                                    fileSizes : TWORD64List;
                                    options : TBFAFileEncryptSetup;
                                    nWiping : Integer);
begin
  inherited Create(files,
                   fileSizes);

  m_options:=options;
  m_nWiping:=nWiping;
end;


function TBFAEncryptSetup.GetOptions : TBFAFileEncryptSetup;
begin
  Result:=m_options;
end;


function TBFAEncryptSetup.GetWiping : Integer;
begin
  Result:=m_nWiping;
end;


function TBFAEncryptSetup.GetNoCompressTypes : TStringList;
begin
  Result:=m_noCompressTypes;
end;



//////////////////////////// TBFADecryptSetup ////////////////////////////


constructor TBFADecryptSetup.Create(files : TPathSearchContainer;
                                    fileSizes : TWORD64List;
                                    options : TBFAFileDecryptSetup);
begin
  inherited Create(files,
                   fileSizes);

  m_options:=options;
end;


function TBFADecryptSetup.GetOptions : TBFAFileDecryptSetup;
begin
  Result:=m_options;
end;


//////////////////////////// TBFAWorkProgress ////////////////////////////

function TBFAWorkProgress.GetOutputFileName : String;
begin
  Result:=m_sOutputFileName;
end;

procedure TBFAWorkProgress.SetOutputFileName(sOutputFileName : String);
begin
  m_sOutputFileName:=sOutputFileName;
end;




//////////////////////////// TBFAManager ////////////////////////////



constructor TBFAManager.Create(sCipherName : String;
                               randomSource : TRandomSource;
                               nMaxErrors : Integer;
                               workProgress : TBFAWorkProgress;
                               confirmCB : TMessageCallBack;
                               sr : TStrRes);
begin
  m_sr:=sr;
  inherited Create(sCipherName,
                   randomSource,
                   nMaxErrors,
                   workProgress,
                   confirmCB,
                   sr);
  m_BFAFile:=Nil;
  try
    m_BFAFile:=TBFAFile.Create(GetCipherName,
                               m_sr,
                               randomSource);
  except
    on ebfe : EBFAFileError do
      raise EBFError.Create(ebfe.Message);
  end;
end;


destructor TBFAManager.Destroy;
begin
  if (m_BFAFile <> Nil) then
    m_BFAFile.Destroy;
end;



// we need an internal callback to map progress reports from single
// encrypt/decrypt processes to the callbacks of the BFA manager
type
  TProgressMapper = class(TBFAFileProgress)
  private
    // level of the last completed file
    m_qLastLevel : WORD64;
    // max. allowed next level (to avoid progress jitter)
    m_qMaxNextLevel : WORD64;

  public
    // constructor
    // -> the callback object
    constructor Create(callBackObj : TObject); override;

    // to increase the current level
    // -> the increment
    procedure IncLastLevel(qX : WORD64);

    // to increase the max. next level
    // -> the increment
    procedure IncMaxNextLevel(qX : WORD64);

    // reset the levels
    procedure ZeroLevels;

    // reset to the last level
    procedure ResetToLastLevel;

    // the implemented callback method
    // exception: ECallBackInterrupt if interrupted
    procedure CallBack; override;
  end;

constructor TProgressMapper.Create(callBackObj : TObject);
begin
  inherited Create(callBackObj);
  ZeroLevels;
end;

procedure TProgressMapper.IncLastLevel(qX : WORD64);
begin
  Inc(m_qLastLevel, qX);
end;

procedure TProgressMapper.IncMaxNextLevel(qX : WORD64);
begin
  Inc(m_qMaxNextLevel, qX);
end;

procedure TProgressMapper.ZeroLevels;
begin
  m_qLastLevel:=0;
  m_qMaxNextLevel:=0;
end;

procedure TProgressMapper.ResetToLastLevel;
begin
  TBFWorkProgress(GetCallBackObj).SetActPos(m_qLastLevel);
end;

procedure TProgressMapper.CallBack;
var
  qNewLevel : WORD64;
  bwp       : TBFAWorkProgress;
begin

  // get the mother object
  bwp:=TBFAWorkProgress(GetCallBackObj);

  // set the current progress for the single file, independent if we're
  // encrypting, decrypting or wiping
  bwp.SetBytesDone(GetActPos);
  bwp.SetNumOfBytes(GetMaxPos);

  // set the new file size and (output) file name, if necessary
  if (bwp.GetChanged and
      (GetProgressState <> BFAFILE_PROGRESS_WIPE)) then begin
    bwp.SetFileSize(GetMaxPos);
    bwp.SetFileName(GetInputFileName);
    bwp.SetOutputFileName(GetOutputFileName);
  end;

  // now it's time to decide what's really going on
  // (FIXME: not all of below must be executed every time)
  case GetProgressState of
    BFAFILE_PROGRESS_ENCRYPT : bwp.SetMode(BFM_PROGRESS_ENCRYPT);
    BFAFILE_PROGRESS_DECRYPT : bwp.SetMode(BFM_PROGRESS_DECRYPT);
    BFAFILE_PROGRESS_WIPE    : bwp.SetMode(BFM_PROGRESS_WIPE);
  end;
  if (GetProgressState = BFAFILE_PROGRESS_WIPE) then begin
    if (GetWipeProgressObj.GetChanged) then begin
      // map the wipe state (and loop numbering, i. n.)
      if (GetWipeProgressObj.GetPass = WIPE_PASS_DELETEONLY) then
        bwp.SetDeleteOnly(True)
      else begin
        bwp.SetDeleteOnly(False);
        if (GetWipeProgressObj.GetPass = WIPE_PASS_NOLOOPS) then
          bwp.SetNoWipeLoops(True)
        else begin
          bwp.SetNoWipeLoops(False);
          bwp.SetWipeLoop(GetWipeProgressObj.GetPass);
          bwp.SetNumOfWipes(GetWipeProgressObj.GetNumOfPasses);
        end;
      end;
    end;
  end
  else begin
    // we need to calculate the all-over progress relatively and check for
    // unexpected oversizing
    qNewLevel:=m_qLastLevel + GetActPos;
    if (qNewLevel <= m_qMaxNextLevel) then
      bwp.SetActPos(qNewLevel)
    else
      bwp.SetActPos(m_qMaxNextLevel);
  end;

  // map the changed flag
  bwp.SetChanged(GetChanged);

  // report the progress
  bwp.SetCalled(True);
  bwp.CallBack;

  // now not the first call anymore
  bwp.SetFirstCall(False);
end;




procedure TBFAManager.EncryptFiles(setup : TBFAEncryptSetup;
                                   var vResults : TBFAEncryptResults);
var
  qAssumedSize   : WORD64;
  nFileNumber    : Integer;
  nNumOfFiles    : Integer;
  blDone         : Boolean;
  blOverwriteAll : Boolean;
  blWasError     : Boolean;
  sFileName      : String;
  pmapper        : TProgressMapper;
  res            : TBFAFileEncryptResult;
  wipeObj        : TWipe;

// local cleaner
procedure CleanUp;
begin
  if (pmapper <> Nil) then
    pmapper.Destroy;
  if (wipeObj <> Nil) then
    wipeObj.Destroy;
end;

begin
  // reset all instances
  pmapper:=Nil;
  wipeObj:=Nil;

  // no results (yet or never)
  vResults:=Nil;

  // create the wipe object (FIXME: we always provide one,
  // even when it might not be necessary)
  try
    case setup.GetWiping of
      BFM_WIPE_SIMPLE : wipeObj:=TWipeSimple.Create(m_randomSource);
      BFM_WIPE_DOD    : wipeObj:=TWipeDOD.Create(m_randomSource);
      BFM_WIPE_SFS    : wipeObj:=TWipeSFS.Create;
    else
      wipeObj:=TWipeDeleteOnly.Create;
    end;
  except
    on EOutOfMemory do begin
      Cleanup;
      raise EBFError.Create(m_sr.Get(STRRES_ID, '100'));
    end;
  end;

  // setup the progress callback
  nNumOfFiles:=setup.GetFiles.GetNumOfFiles;
  with m_workProgress do begin
    ZeroPos;
    SetMaxPos(setup.GetFiles.GetNumOfBytes);
    SetNumOfFiles(nNumOfFiles);
  end;

  // init. the error counter
  ResetErrorCounter;

  // create the result keeper
  vResults:=TBFAEncryptResults.Create(m_sr, setup.GetOptions);

  // prepare the progress mapper
  m_workProgress.SetFirstCall(True);
  m_workProgress.SetWipeAfterEncryption(True);
  pmapper:=TProgressMapper.Create(m_workProgress);
  pmapper.ZeroLevels;

  // we store the overwrite flag from the setup right here,
  // it might be changed after a confirmation below
  blOverwriteAll:=setup.GetOptions.GetOverwriteExisting;

  // start the encryption process
  for nFileNumber:=1 to nNumOfFiles do begin

    // get name of the file to encrypt
    if (nFileNumber = 1) then
      sFileName:=setup.GetFiles.GetFirstFile
    else
      sFileName:=setup.GetFiles.GetNextFile;

    // prepare the progress callback
    m_workProgress.SetFileNumber(nFileNumber);

    // set up the progress mapper
    pmapper.IncMaxNextLevel(setup.GetFileSizes.Get(nFileNumber - 1));

    // set the overwrite flag (might have changed, see exceptions below)
    setup.GetOptions.SetOverwriteExisting(blOverwriteAll);

    // set the current file number
    setup.GetOptions.SetMaskNumber(nFileNumber);

    // encrypt the file (we may try it multiple times in case of some errors)
    blWasError:=False;
    blDone:=False;
    m_workProgress.SetCalled(False);
    while (not blDone) do begin

      try
        blDone:=True; // (we don't repeat if not necessary)
        res:=m_BFAFile.Encrypt(sFileName,
                               wipeObj,
                               setup.GetOptions,
                               pmapper);

        // interrupted during the wiping?
        if ((res.GetWarnings and BFAFILE_WARNING_INTERRUPTED) =
            BFAFILE_WARNING_INTERRUPTED) then begin
          Cleanup;
          vResults.Add(TStringKeeper.Create(sFileName),
                       WORKRESULTS_TYPE_BREAK,
                       m_sr.Get(STRRES_ID, '101'));
          res.Destroy;
          raise EBFInterrupt.Create(m_sr.Get(STRRES_ID, '101'));
        end;

        // add the result
        if (res.GetWarnings <> 0) then
          vResults.Add(res, WORKRESULTS_TYPE_WARNING)
        else
          vResults.Add(res, WORKRESULTS_TYPE_SUCCESS);

      except
        // was the file only skipped?
        on ebfae : EBFAFileAlreadyEncrypted do begin
          vResults.Add(TStringKeeper.Create(sFileName),
                       WORKRESULTS_TYPE_SKIP,
                       ebfae.Message);
        end;

        // fatal error?
        on ebff : EBFAFileFatal do begin
          Cleanup;
          vResults.Add(TStringKeeper.Create(sFileName),
                       WORKRESULTS_TYPE_ERROR,
                       ebff.Message);
          raise EBFFatalError.Create(m_sr.Get(STRRES_ID, '102') + ebff.Message);
        end;

        // interrupted?
        on ebfi : EBFAFileInterrupted do begin
          Cleanup;
          vResults.Add(TStringKeeper.Create(sFileName),
                       WORKRESULTS_TYPE_BREAK,
                       ebfi.Message);
          raise EBFInterrupt.Create(m_sr.Get(STRRES_ID, '101'));
        end;

        // disk full?
        on ebfdf : EBFAFileDiskFull do begin

          // give the user a chance to fix this
          m_confirmCB.SetStyle(MCB_STYLE_YESNO);
          m_confirmCB.SetMessage(m_sr.Get(STRRES_ID, '104'));
          // does the user try to free some disk space?
          m_confirmCB.CallBack;
          if (m_confirmCB.GetResult = MCB_RES_YES) then begin
            // yes, we try it again
            pmapper.ResetToLastLevel;
            blDone:=False;
          end
          else begin
            // no, break with an error
            Cleanup;
            vResults.Add(TStringKeeper.Create(sFileName),
                         WORKRESULTS_TYPE_BREAK,
                         ebfdf.Message);
            raise EBFError.Create(m_sr.Get(STRRES_ID, '102') + ebfdf.Message);
          end;
        end;

        // weak key?
        on ebfdf : EBFAFileWeakKey do begin

          // we try again! (FIXME: really no danger of endless loops?)
          pmapper.ResetToLastLevel;
          blDone:=False;
        end;

        // file already exists?
        on ebfae : EBFAFileAlreadyExists do begin
          // give the user a chance to fix this
          m_confirmCB.SetStyle(MCB_STYLE_YESNOALLCANCEL);
          m_confirmCB.SetMessage(ebfae.Message +
                                 m_sr.Get(STRRES_ID, '105'));
          try
            m_confirmCB.CallBack;
          except
            on ECallBackInterrupt do begin
               // break with an interrupt, i. n.
              CleanUp;
              raise EBFInterrupt.Create(m_sr.Get(STRRES_ID, '101'));
            end;
          end;
          // does the user want to override the file?
          if ((m_confirmCB.GetResult = MCB_RES_YES) or
              (m_confirmCB.GetResult = MCB_RES_ALL)) then begin
            // yes, we try it again
            pmapper.ResetToLastLevel;
            blDone:=False;
            // set the overwrite flag
            setup.GetOptions.SetOverwriteExisting(True);
            // (and even static, i. n.)
            if (m_confirmCB.GetResult = MCB_RES_ALL) then
              blOverwriteAll:=True;
          end
          else begin
            // no, register the conflict and continue
            vResults.Add(TStringKeeper.Create(sFileName),
                         WORKRESULTS_TYPE_SKIP,
                         ebfae.Message);
            blWasError:=True;
      end;
        end;

        // ok, just another error (we continue)
        on ebfe : EBFAFileError do begin
          vResults.Add(TStringKeeper.Create(sFileName),
                       WORKRESULTS_TYPE_ERROR,
                       ebfe.Message);
          blWasError:=True;
        end;
      end;

    end; (* of WHILE *)

    // set up the progress mapper's lower level for the next file
    qAssumedSize:=setup.GetFileSizes.Get(nFileNumber - 1);
    pmapper.IncLastLevel(qAssumedSize);

    // if no callback was done we fire at least one to keep the interrupt
    // possibility alive)
    with m_workProgress as TBFAWorkProgress do begin
      if (not GetCalled) then begin
        pmapper.ResetToLastLevel;
        pmapper.SetActPos(0);
        SetFileName(sFileName);
        SetOutputFileName('');
        SetBytesDone(0);
        SetFileSize(qAssumedSize);
        SetNumOfBytes(qAssumedSize);
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
        raise EBFInterrupt.Create(m_sr.Get(STRRES_ID, '101'));
      end;
    end;

  end; (* of FOR *)

  // finished
  CleanUp;
end;



procedure TBFAManager.DecryptFiles(setup : TBFADecryptSetup;
                                   var vResults : TBFADecryptResults);
var
  qAssumedSize   : WORD64;
  nFileNumber    : Integer;
  nNumOfFiles    : Integer;
  blDone         : Boolean;
  blOverwriteAll : Boolean;
  blWasError     : Boolean;
  sFileName      : String;
  pmapper        : TProgressMapper;
  res            : TBFAFileDecryptResult;

// local cleaner
procedure CleanUp;
begin
  if (pmapper <> Nil) then
    pmapper.Destroy;
end;

begin
  // reset all instances
  pmapper:=Nil;

  // no results (yet or never)
  vResults:=Nil;

  // setup the progress callback
  nNumOfFiles:=setup.GetFiles.GetNumOfFiles;
  with m_workProgress do begin
    ZeroPos;
    SetMaxPos(setup.GetFiles.GetNumOfBytes);
    SetNumOfFiles(nNumOfFiles);
  end;

  // init. the error counter
  ResetErrorCounter;

  // create the result keeper
  vResults:=TBFADecryptResults.Create(m_sr, setup.GetOptions);

  // prepare the progress mapper
  m_workProgress.SetFirstCall(True);
  pmapper:=TProgressMapper.Create(m_workProgress);
  pmapper.ZeroLevels;

  // we store the overwrite flag from the setup right here,
  // it might be changed after a confirmation below
  blOverwriteAll:=setup.GetOptions.GetOverwriteExisting;

  // start the encryption process
  for nFileNumber:=1 to nNumOfFiles do begin

    // get name of the file to encrypt
    if (nFileNumber = 1) then
      sFileName:=setup.GetFiles.GetFirstFile
    else
      sFileName:=setup.GetFiles.GetNextFile;

    // prepare the progress callback
    m_workProgress.SetFileNumber(nFileNumber);

    // set up the progress mapper
    pmapper.IncMaxNextLevel(setup.GetFileSizes.Get(nFileNumber - 1));

    // set the overwrite flag
    setup.GetOptions.SetOverwriteExisting(blOverwriteAll);

    // decrypt the file (we may try it multiple times in case of some errors)
    blDone:=False;
    blWasError:=False;
    while (not blDone) do begin
      try
        blDone:=True; // (we don't repeat if not necessary)
        res:=m_BFAFile.Decrypt(sFileName,
                               setup.GetOptions,
                               pmapper);

        // add the result
        if (res.GetWarnings <> 0) then
          vResults.Add(res,
                       WORKRESULTS_TYPE_WARNING)
        else
          vResults.Add(res,
                       WORKRESULTS_TYPE_SUCCESS)

      except
        // was the file only skipped?
        on ebfae : EBFAFileAlreadyEncrypted do begin
          vResults.Add(TStringKeeper.Create(sFileName),
                       WORKRESULTS_TYPE_SKIP,
                       ebfae.Message);
        end;

        // fatal error?
        on ebff : EBFAFileFatal do begin
          Cleanup;
          vResults.Add(TStringKeeper.Create(sFileName),
                       WORKRESULTS_TYPE_ERROR,
                       ebff.Message);
          raise EBFFatalError.Create(m_sr.Get(STRRES_ID, '102') + ebff.Message);
        end;

        // interrupted?
        on ebfi : EBFAFileInterrupted do begin
          Cleanup;
          vResults.Add(TStringKeeper.Create(sFileName),
                       WORKRESULTS_TYPE_BREAK,
                       ebfi.Message);
          raise EBFInterrupt.Create(m_sr.Get(STRRES_ID, '101'));
        end;

        // disk full?
        on ebfdf : EBFAFileDiskFull do begin

          // give the user a chance to fix this
          m_confirmCB.SetStyle(MCB_STYLE_YESNO);
          m_confirmCB.SetMessage(m_sr.Get(STRRES_ID, '104'));
          // does the user try to free some disk space?
          m_confirmCB.CallBack;
          if (m_confirmCB.GetResult = MCB_RES_YES) then begin
            // yes, we try it again
            pmapper.ResetToLastLevel;
            blDone:=False;
          end
          else begin
            // no, break with an error
            Cleanup;
            vResults.Add(TStringKeeper.Create(sFileName),
                         WORKRESULTS_TYPE_BREAK,
                         ebfdf.Message);
            raise EBFError.Create(m_sr.Get(STRRES_ID, '102') + ebfdf.Message);
          end;
        end;

        // file already exists?
        on ebfae : EBFAFileAlreadyExists do begin
          // give the user a chance to fix this
          m_confirmCB.SetStyle(MCB_STYLE_YESNOALLCANCEL);
          m_confirmCB.SetMessage(ebfae.Message +
                                 m_sr.Get(STRRES_ID, '105'));
          try
            m_confirmCB.CallBack;
          except
            // break?
            on ECallBackInterrupt do begin
              CleanUp;
              vResults.Add(TStringKeeper.Create(sFileName),
                           WORKRESULTS_TYPE_BREAK,
                           m_sr.Get(STRRES_ID, '101'));
              raise EBFInterrupt.Create(m_sr.Get(STRRES_ID, '101'));
            end;
          end;
          // does the user want to override the file?
          if ((m_confirmCB.GetResult = MCB_RES_YES) or
              (m_confirmCB.GetResult = MCB_RES_ALL)) then begin
            // yes, we try it again
            pmapper.ResetToLastLevel;
            blDone:=False;
            // set the overwrite flag
            setup.GetOptions.SetOverwriteExisting(True);
            // (and even static, i. n.)
            if (m_confirmCB.GetResult = MCB_RES_ALL) then
              blOverwriteAll:=True;
          end
          else begin
            // no, register the conflict
            vResults.Add(TStringKeeper.Create(sFileName),
                         WORKRESULTS_TYPE_SKIP,
                         ebfae.Message);
            blWasError:=True;
          end;
        end;

        // was the file only skipped?
        on ebfnc : EBFAFileNoCryptfile do begin
          vResults.Add(TStringKeeper.Create(sFileName),
                       WORKRESULTS_TYPE_SKIP,
                       ebfnc.Message);
        end;

        // ok, just another error (we continue)
        on ebfe : EBFAFileError do begin
          vResults.Add(TStringKeeper.Create(sFileName),
                       WORKRESULTS_TYPE_ERROR,
                       ebfe.Message);
          blWasError:=True;
        end;
      end;

    end; (* of WHILE *)

    // set up the progress mapper's lower level for the next file
    qAssumedSize:=setup.GetFileSizes.Get(nFileNumber - 1);
    pmapper.IncLastLevel(qAssumedSize);

    // same as above...
    with m_workProgress as TBFAWorkProgress do begin
      if (not GetCalled) then begin
        pmapper.ResetToLastLevel;
        pmapper.SetActPos(0);
        SetFileName(sFileName);
        SetOutputFileName('');
        SetBytesDone(0);
        SetFileSize(qAssumedSize);
        SetNumOfBytes(qAssumedSize);
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

    if (blWasError) then begin
      if (not IncAndCheckMaxErrors) then begin
        CleanUp;
        raise EBFInterrupt.Create(m_sr.Get(STRRES_ID, '101'));
      end;
    end;

  end; (* of FOR *)

  // finished
  CleanUp;
end;



end.
