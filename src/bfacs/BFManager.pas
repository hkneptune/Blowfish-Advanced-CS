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
  common classes used by the manager for .BFA files and others that will follow
}

unit BFManager;

{$I config.inc}

interface
uses SysUtils, classes,
     bfacslib, PathSearch, WorkResults, MessageCallBack, FilesProgress,
     RandomSource, StringRes;



// compression codes
const
  BFM_COMPRESS_NONE = 0;
  BFM_COMPRESS_LZSS = 1;


// wipe methods
const
  BFM_WIPE_DELETEONLY = 0;
  BFM_WIPE_SIMPLE     = 1;
  BFM_WIPE_DOD        = 2;
  BFM_WIPE_SFS        = 3;


// we only provide four error codes here, the messages
// can be directly used for displaying
type
  EBFError       = class(Exception);
  EBFRecoverable = class(EBFError);
  EBFFatalError  = class(EBFError);
  EBFInterrupt   = class(EBFError);


// work progress modes
const
  BFM_PROGRESS_ENCRYPT   = 0;
  BFM_PROGRESS_DECRYPT   = 1;
  BFM_PROGRESS_WIPE      = 2;
  BFM_PROGRESS_DESLACK   = 3;
  BFM_PROGRESS_REENCRYPT = 4;


// callback for the work progress
type
  TBFWorkProgress = class(TFilesProgress)
  private

    // members
    m_qFileSize             : WORD64;
    m_qNumOfBytes           : WORD64;
    m_qBytesDone            : WORD64;
    m_nMode                 : Integer;
    m_nWipeLoop             : Integer;
    m_nNumOfWipes           : Integer;
    m_blDeleteOnly          : Boolean;
    m_blNoWipeLoops         : Boolean;
    m_blFirstCall           : Boolean;
    m_blWipeAfterEncryption : Boolean;

  public

    // gets the (real) size of the current file
    // <- file size
    function GetFileSize : WORD64;

    // sets the size of the current file
    // -> new file size
    procedure SetFileSize(qFileSize : WORD64);

    // gets the number of all bytes processed for the current file
    // <- number of all bytes
    function GetNumOfBytes : WORD64;

    // sets the number of all bytes processed for the current file
    // -> number of all bytes
    procedure SetNumOfBytes(qNumOfBytes : WORD64);

    // gets the number of bytes done of the current file
    // <- number of bytes done
    function GetBytesDone : WORD64;

    // sets the number of bytes done of the current file
    // -> new number of bytes done
    procedure SetBytesDone(qBytesDone : WORD64);

    // return the 31bit integer level of the bytes of the current file done
    // -> bytes done
    // -> all bytes
    procedure GetIntBytesDone(var vnActBytes : Integer;
                              var vnMaxBytes : Integer);

    // gets the current mode, see BFM_PROGRESS_xxx constants
    // <- current mode
    function GetMode : Integer;

    // sets the current mode, see BFM_PROGRESS_xxx constants
    // -> new mode
    procedure SetMode(nMode : Integer);

    // gets the delete only flag
    // <- delete only flag
    function GetDeleteOnly : Boolean;

    // sets the delete only flag
    // -> delete only flag
    procedure SetDeleteOnly(blDeleteOnly : Boolean);

    // gets the no wipe loops flag
    // <- no wipe loops flag
    function GetNoWipeLoops : Boolean;

    // sets the no wipe loops flag
    // -> no wipe loops flag
    procedure SetNoWipeLoops(blNoWipeLoops : Boolean);

    // gets the number of the current wipe loop (if available)
    // <- wipe loop number
    function GetWipeLoop : Integer;

    // sets the number of the current wipe loop
    // -> new wipe loop number
    procedure SetWipeLoop(nWipeLoop : Integer);

    // gets the number of wipe loops
    // <- number of wipe loops
    function GetNumOfWipes : Integer;

    // sets the number of wipe loops
    // -> number of wipe loops
    procedure SetNumOfWipes(nNumOfWipes : Integer);

    // gets the wipe after encryption (wae) flag
    // <- wipe after encryption flag
    function GetWipeAfterEncryption : Boolean;

    // sets the wipe after encryption flag
    // -> new state of the wipe after encryption flag
    procedure SetWipeAfterEncryption(blWipeAfterEncryption : Boolean);

    // checks if the callback occured the first time
    // <- True: it is the first call / False: some other call
    function GetFirstCall : Boolean;

    // sets the first call flag
    // -> this is the first callback / False: not the 1st one
    procedure SetFirstCall(blFirstCall : Boolean);

  end;





// constant to deactivate an error counting
const
  BFM_NOMAXERRORS = -1;

// base manager class
type
  TBFBaseManager = class
  protected
    m_randomSource : TRandomSource;
    m_nErrors      : Integer;
    m_nMaxErrors   : Integer;
    m_workProgress : TBFWorkProgress;
    m_confirmCB    : TMessageCallBack;
    m_sr           : TStrRes;

  public
    // constructor
    // -> random generator to set up the cipher
    // -> max. number of errors before a request for abortion
    //    is sent to the user (or BFM_NOMAXERRORS)
    // -> work progress callback
    // -> confirmation callback
    // -> string resources
    constructor Create(randomSource : TRandomSource;
                       nMaxErrors : Integer;
                       workProgress : TBFWorkProgress;
                       confirmCB : TMessageCallBack;
                       sr : TStrRes); virtual;

    // returns the random source
    // <- random source
    function GetRandomSource : TRandomSource;

    // returns the max. number of error
    // <- max. number of error
    function GetMaxErrors : Integer;

    // returns the work progress callback
    // <- work progress callback
    function GetWorkProgress : TBFWorkProgress;

    // returns the confirmation callback
    // <- confirmation callback
    function GetConfirmCB : TMessageCallBack;

    // resets the internal error counter
    procedure ResetErrorCounter;

    // increase the error counter makes a request if the maximum is reached
    // <- True: continue / False: max errors, user wants to abort
    function IncAndCheckMaxErrors : Boolean;
  end;



// standard manager class (for those jobs who need cipher support)
type
  TBFManager = class(TBFBaseManager)
  protected
    m_sCipherName  : String;

  public
    // constructor
    // -> cipher name
    // -> random generator to set up the cipher
    // -> max. number of errors before a request for abortion
    //    is sent to the user (or BFM_NOMAXERRORS)
    // -> work progress callback
    // -> confirmation callback
    // -> string resources
    constructor Create(sCipherName : String;
                       randomSource : TRandomSource;
                       nMaxErrors : Integer;
                       workProgress : TBFWorkProgress;
                       confirmCB : TMessageCallBack;
                       sr : TStrRes); reintroduce; overload;

    // returns the cipher name
    // <- cipher name
    function GetCipherName : String;
  end;



implementation

// string resources ID
const
  STRRES_ID = 'BFMANAGER';


//////////////////////////// TBFWorkProgress ////////////////////////////


function TBFWorkProgress.GetFileSize : WORD64;
begin
  Result:=m_qFileSize;
end;

procedure TBFWorkProgress.SetFileSize(qFileSize : WORD64);
begin
  m_qFileSize:=qFileSize;
end;

function TBFWorkProgress.GetNumOfBytes : WORD64;
begin
  Result:=m_qNumOfBytes;
end;

procedure TBFWorkProgress.SetNumOfBytes(qNumOfBytes : WORD64);
begin
  m_qNumOfBytes:=qNumOfBytes;
end;

function TBFWorkProgress.GetBytesDone : WORD64;
begin
  Result:=m_qBytesDone;
end;

procedure TBFWorkProgress.SetBytesDone(qBytesDone : WORD64);
begin
  m_qBytesDone:=qBytesDone;
end;

procedure TBFWorkProgress.GetIntBytesDone(var vnActBytes : Integer;
                                          var vnMaxBytes : Integer);
var
  nToShift : Integer;
begin
  nToShift:=GetShrInt31Level(m_qNumOfBytes);
  vnActBytes:=(m_qBytesDone shr nToShift) and $7fffffff;
  vnMaxBytes:=(m_qNumOfBytes shr nToShift) and $7fffffff;
end;


function TBFWorkProgress.GetMode : Integer;
begin
  Result:=m_nMode;
end;

procedure TBFWorkProgress.SetMode(nMode : Integer);
begin
  m_nMode:=nMode;
end;

function TBFWorkProgress.GetDeleteOnly : Boolean;
begin
  Result:=m_blDeleteOnly;
end;

procedure TBFWorkProgress.SetDeleteOnly(blDeleteOnly : Boolean);
begin
  m_blDeleteOnly:=m_blDeleteOnly;
end;

function TBFWorkProgress.GetNoWipeLoops : Boolean;
begin
  Result:=m_blNoWipeLoops;
end;

procedure TBFWorkProgress.SetNoWipeLoops(blNoWipeLoops : Boolean);
begin
  m_blNoWipeLoops:=blNoWipeLoops;
end;

function TBFWorkProgress.GetWipeLoop : Integer;
begin
  Result:=m_nWipeLoop;
end;

procedure TBFWorkProgress.SetWipeLoop(nWipeLoop : Integer);
begin
  m_nWipeLoop:=nWipeLoop;
end;

function TBFWorkProgress.GetNumOfWipes : Integer;
begin
  Result:=m_nNumOfWipes;
end;

procedure TBFWorkProgress.SetNumOfWipes(nNumOfWipes : Integer);
begin
  m_nNumOfWipes:=nNumOfWipes;
end;

function TBFWorkProgress.GetWipeAfterEncryption : Boolean;
begin
  Result:=m_blWipeAfterEncryption;
end;

procedure TBFWorkProgress.SetWipeAfterEncryption(
            blWipeAfterEncryption : Boolean);
begin
  m_blWipeAfterEncryption:=blWipeAfterEncryption;
end;

function TBFWorkProgress.GetFirstCall : Boolean;
begin
  Result:=m_blFirstCall;
end;

procedure TBFWorkProgress.SetFirstCall(blFirstCall : Boolean);
begin
  m_blFirstCall:=blFirstCall;
end;



//////////////////////////// TBFBaseManager ////////////////////////////

constructor TBFBaseManager.Create(randomSource : TRandomSource;
                                  nMaxErrors : Integer;
                                  workProgress : TBFWorkProgress;
                                  confirmCB : TMessageCallBack;
                                  sr : TStrRes);
begin
  m_randomSource:=randomSource;
  m_nMaxErrors:=nMaxErrors;
  m_workProgress:=workProgress;
  m_confirmCB:=confirmCB;
  m_sr:=sr;
  ResetErrorCounter;
end;


function TBFBaseManager.GetRandomSource : TRandomSource;
begin
  Result:=m_randomSource;
end;


function TBFBaseManager.GetMaxErrors : Integer;
begin
  Result:=m_nMaxErrors;
end;


function TBFBaseManager.GetWorkProgress : TBFWorkProgress;
begin
  Result:=m_workProgress;
end;


function TBFBaseManager.GetConfirmCB : TMessageCallBack;
begin
  Result:=m_confirmCB;
end;

procedure TBFBaseManager.ResetErrorCounter;
begin
  m_nErrors:=0;
end;

function TBFBaseManager.IncAndCheckMaxErrors : Boolean;
begin

  // too many errors?
  if ((m_nMaxErrors = BFM_NOMAXERRORS) or
      (m_nErrors = BFM_NOMAXERRORS)) then begin
    Result:=True;
  end
  else begin
    if (m_nErrors = m_nMaxErrors) then begin

      m_confirmCB.SetStyle(MCB_STYLE_YESNO);
      m_confirmCB.SetMessage(Format(m_sr.Get(STRRES_ID, 'MAXERR'),
                             [m_nMaxErrors]));
      m_confirmCB.CallBack;
      if (m_confirmCB.GetResult = MCB_RES_NO) then begin
        Result:=False;
      end
      else begin
        m_nErrors:=BFM_NOMAXERRORS;
        Result:=True;
      end;
    end
    else begin
      Inc(m_nErrors);
      Result:=True;
    end;
  end;

end;



//////////////////////////// TBFManager ////////////////////////////


constructor TBFManager.Create(sCipherName : String;
                              randomSource : TRandomSource;
                              nMaxErrors : Integer;
                              workProgress : TBFWorkProgress;
                              confirmCB : TMessageCallBack;
                              sr : TStrRes);

begin
  inherited Create(randomSource,
                   nMaxErrors,
                   workProgress,
                   confirmCB,
                   sr);
  m_sCipherName:=sCipherName;
end;


function TBFManager.GetCipherName : String;
begin
  Result:=m_sCipherName;
end;



end.
