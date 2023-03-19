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
  classes for secure 64bit file deleting under Win32
}

unit Wipe;

{$I config.inc}

interface
uses SysUtils,
     bfacslib,
     CallBack,
     ProgressCallBack,
     RandomSource,
     FileSupp,
     StringRes;


// exceptions for wiping
type
  EWipeError    = class(Exception);
  EWipeFatal    = class(EWipeError);
  EWipeWasBreak = class(EWipeError);


// wipe pass special values
const
  WIPE_PASS_NOLOOPS    = -1;
  WIPE_PASS_DELETEONLY = -2;


// progress callback class for wiping
type
  TWipeProgress = class(TProgressCallBack)
  protected
    m_qFileSize    : WORD64;
    m_nPass        : Integer;
    m_nNumOfPasses : Integer;
  public
    // sets the file size
    // -> file size
    procedure SetFileSize(qFileSize : WORD64); virtual;

    // gets the file size
    // <- file size
    function GetFileSize : WORD64; virtual;

    // sets the pass number
    // -> pass number
    procedure SetPass(nPass : Integer); virtual;

    // gets the current pass number
    // <- current pass number
    function GetPass : Integer; virtual;

    // gets the number of passes
    // -> number of passes
    procedure SetNumOfPasses(nNumOfPasses : Integer); virtual;

    // gets the number of passes
    // <- number of passes
    function GetNumOfPasses : Integer; virtual;

    // sets the pass number to zero
    procedure ZeroPass; virtual;

    // increases the pass number by 1
    procedure IncPass; virtual;
  end;


// the base class for wiping
type
  TWipe = class
  public
    // file wiping method
    // -> name of the file to wipe
    // -> progress callback instance
    // -> string resources
    function Execute(sFileName : String;
                     progresscall : TWipeProgress;
                     sr : TStrRes) : Boolean;
                       virtual; abstract;
  end;


// class for delete only wiping
type
  TWipeDeleteOnly = class(TWipe)
  public
    // wipes a file by just deleting it (no progress callback)
    // -> name of the file to wipe
    // -> progress callback instance
    // <- always True
    // exception:
    // EWipeError    - error occured while deleting
    // EWipeWasBreak - interruption detected
    function Execute(sFileName : String;
                     progresscall : TWipeProgress;
                     sr : TStrRes) : Boolean; override;
  end;


// class for real wiping
type
  TWipeReal = class(TWipe)
  protected
    m_clad : TFileClusterAdjuster;
  public

    // constructor
    constructor Create; virtual;

    // destructor
    destructor Destroy; override;
  end;



// class for simple (one pass) wiping
type
  TWipeSimple = class(TWipeReal)
  protected
    m_pBaseBuffer : Pointer;
  public
    // init. the base buffer with random values for one pass wiping
    // -> reference to a random source
    // exception:
    // EOutOfMemory - if out of memory
    constructor Create(rndsrc : TRandomSource); reintroduce; overload;

    // clears and destroys the base buffer
    destructor Destroy; override;

    // wipes a file in one pass
    // -> name of the file to wipe
    // -> progress callback instance
    // <- True: sector access was possible / False: weak wiping
    // exception:
    // EWipeError    - error occured while wiping
    // EWipeFatal    - fatal error occured
    // EWipeWasBreak - interruption detected
    function Execute(sFileName : String;
                     progresscall : TWipeProgress;
                     sr : TStrRes) : Boolean; override;
  end;


// class for DOD (three pass) wiping
type
  TWipeDOD = class(TWipeSimple)
  protected
    m_pSwapBuffer : Pointer;
    m_pTempBuffer : Pointer;
  public
    // creates and prepares extra buffers with random values for DOD wiping,
    // additional random seed data can be passed
    // -> reference to a random source
    // exception:
    // EOutOfMemory - if out of memory
    constructor Create(rndsrc : TRandomSource); virtual;

    // clears and destroys the three buffers
    destructor Destroy; override;

    // wipes a file in three passes
    // -> name of the file to wipe
    // -> progress callback instance
    // <- True: sector access was possible / False: weak wiping
    // exception:
    // EWipeError    - error occured while wiping
    // EWipeFatal    - fatal error occured
    // EWipeWasBreak - interruption detected
    function Execute(sFileName : String;
                     progresscall : TWipeProgress;
                     sr : TStrRes) : Boolean; override;
  end;


// class for SFS (35 pass) wiping
type
  TWipeSFS = class(TWipeReal)
  protected
    m_pBuffer : Pointer;
  public
    // create the buffer
    // exception:
    // EOutOfMemory - if out of memory
    constructor Create; override;

    // clears and destroys the buffer
    destructor Destroy; override;

    // wipes a file in 35 passes (after the SFS scheme)
    // -> name of the file to wipe
    // -> progress callback instance
    // <- True: sector access was possible / False: weak wiping
    // exception:
    // EWipeError    - error occured while wiping
    // EWipeFatal    - fatal error occured
    // EWipeWasBreak - interruption detected
    function Execute(sFileName : String;
                     progresscall : TWipeProgress;
                     sr : TStrRes) : Boolean; override;
  end;




implementation

uses Windows,
     StringPlusI,
     General,
     RandomPool;


// string resource ID
const
  STRRES_ID = 'WIPE';


// buffer size (must be a multiple of 512)
const
  BUFFERSIZE = 65536;


// special bits for SFS wiping
const
  SFS_BITS : array[0..34,0..2] of Byte = (
      (219, 89 , 86 ),
      (102, 204, 205),
      (96 , 78 , 64 ),
      (41 , 100, 66 ),
      ($55, $55, $55),
      ($aa, $aa, $aa),
      ($92, $49, $24),
      ($49, $24, $92),
      ($24, $92, $49),
      ($00, $00, $00),
      ($11, $11, $11),
      ($22, $22, $22),
      ($33, $33, $33),
      ($44, $44, $44),
      ($55, $55, $55),
      ($66, $66, $66),
      ($77, $77, $77),
      ($88, $88, $88),
      ($99, $99, $99),
      ($aa, $aa, $aa),
      ($bb, $bb, $bb),
      ($cc, $cc, $cc),
      ($dd, $dd, $dd),
      ($ee, $ee, $ee),
      ($ff, $ff, $ff),
      ($92, $49, $24),
      ($49, $24, $92),
      ($24, $92, $49),
      ($6d, $b6, $db),
      ($b6, $db, $6d),
      ($db, $6d, $b6),
      (167, 111, 101),
      (8  , 60 , 57 ),
      (166, 165, 128),
      (90 , 192, 35 )  );


//////////////////////////// TWipeProgress ////////////////////////////

procedure TWipeProgress.SetFileSize(qFileSize : WORD64);
begin
  m_qFileSize:=qFileSize;
end;


function TWipeProgress.GetFileSize : WORD64;
begin
  Result:=m_qFileSize;
end;


procedure TWipeProgress.SetNumOfPasses(nNumOfPasses : Integer);
begin
  m_nNumOfPasses:=nNumOfPasses;
end;


function TWipeProgress.GetNumOfPasses : Integer;
begin
  Result:=m_nNumOfPasses;
end;


procedure TWipeProgress.SetPass(nPass : Integer);
begin
  m_nPass:=nPass;
end;


function TWipeProgress.GetPass : Integer;
begin
  Result:=m_nPass;
end;


procedure TWipeProgress.ZeroPass;
begin
  SetPass(0);
end;

procedure TWipeProgress.IncPass;
begin
  SetPass(GetPass + 1);
end;


//////////////////////////// TWipeDeleteOnly ////////////////////////////


function TWipeDeleteOnly.Execute(sFileName : String;
                                 progresscall : TWipeProgress;
                                 sr : TStrRes) : Boolean;
var
  nError : Integer;
begin

  // send a progress message
  with progresscall do begin
    SetPass(WIPE_PASS_DELETEONLY);
    SetChanged(True);
    try
      CallBack;
    except
      on ecbi : ECallBackInterrupt do begin
        raise EWipeWasBreak.Create(sr.Get(STRRES_ID, '007'));
      end;
    end;
    SetChanged(False);
  end;

  nError:=FileSetAttr(sFileName, 0);
  if (nError <> 0) then
    raise EWipeError.Create(TStrPlusI.WinErrToStr(sr, nError));

  if (not SysUtils.DeleteFile(sFileName)) then
    raise EWipeError.Create(sr.Get(STRRES_ID, '001'));

  Result:=True
end;


//////////////////////////// TWipeReal ////////////////////////////

constructor TWipeReal.Create;
begin
  m_clad:=TFileClusterAdjuster.Create;
end;

destructor TWipeReal.Destroy;
begin
  m_clad.Destroy;
end;


//////////////////////////// TWipeSimple ////////////////////////////


constructor TWipeSimple.Create(rndsrc : TRandomSource);
begin
  inherited Create;

  // allocate the buffer
  m_pBaseBuffer:=Nil;
  GetMem(m_pBaseBuffer, BUFFERSIZE);

  // fill the buffer with random data
  rndsrc.GetBytes(m_pBaseBuffer, BUFFERSIZE);
end;



destructor TWipeSimple.Destroy;
begin
  if (m_pBaseBuffer <> Nil) then begin
    FillChar(m_pBaseBuffer^, BUFFERSIZE, 0);
    FreeMem(m_pBaseBuffer);
  end;

  inherited Destroy;
end;


function TWipeSimple.Execute(sFileName : String;
                             progresscall : TWipeProgress;
                             sr : TStrRes) : Boolean;
var
  lToWrite  : WORD32;
  lOpenMask : WORD32;
  nError    : Integer;
  dwDummy   : DWORD;
  blDone    : Boolean;
  fhandle   : THandle;
  qFileLen  : WORD64;
begin
  // reset the file attributes
  nError:=FileSetAttr(sFileName, 0);
  if (nError <> 0) then
    raise EWipeError.Create(TStrPlusI.WinErrToStr(sr, nError));

  // get the 64bit file length
  try
    qFileLen:=TFileSupport.GetFile64Len(sFilename, sr);
  except
    on efse : EFileSupportError do
      raise EWipeError.Create(efse.Message);
  end;

  // store the pure file size for the progress callback
  progressCall.SetFileSize(qFileLen);

  // adjust the file length to the next unit
  Result:=m_clad.ClusterAdjust(sFileName, qFileLen, qFileLen);

  if (Result) then
    lOpenMask:=FILE_FLAG_WRITE_THROUGH or FILE_FLAG_NO_BUFFERING
  else
    lOpenMask:=FILE_FLAG_WRITE_THROUGH;

  // open the file to write (using Win32 API calls)
  fhandle:=CreateFile(PChar(sFilename),
                      GENERIC_WRITE,
                      0,
                      Nil,
                      OPEN_EXISTING,
                      lOpenMask,
                      0);
  if (fhandle = INVALID_HANDLE_VALUE) then
    raise EWipeError.Create(sr.Get(STRRES_ID, '002'));

  // wipe the file now
  blDone:=False;
  progresscall.ZeroPos;
  progresscall.SetMaxPos(qFileLen);
  progresscall.SetPass(WIPE_PASS_NOLOOPS);
  progresscall.SetChanged(True);
  try
    while (not blDone) do begin
      // last loop?
      if (qFileLen < BUFFERSIZE) then begin
        lToWrite:=qFileLen;
        blDone:=True;
      end
      else
        lToWrite:=BUFFERSIZE;

      // send a progress message
      progresscall.CallBack;
      progresscall.SetChanged(False);

      // write the actual buffer content
      if (WriteFile(fhandle,
                    m_pBaseBuffer^,
                    lToWrite,
                    dwDummy,
                    Nil) = FALSE) then begin
        // error occured
        if (CloseHandle(fhandle) = FALSE) then
          raise EWipeFatal.Create(sr.Get(STRRES_ID, '003'))
        else
          raise EWipeError.Create(sr.Get(STRRES_ID, '004'));
      end;

      // modify the counters
      Dec(qFileLen, lToWrite);
      progresscall.IncPos(lToWrite);
    end;

    // send the last progress message
    progresscall.CallBack;

  except
    // handle the interruption
    on ECallBackInterrupt do begin
      if (CloseHandle(fhandle) = FALSE) then
        raise EWipeFatal.Create(sr.Get(STRRES_ID, '005'));
      raise EWipeWasBreak.Create(sr.Get(STRRES_ID, '007'));
    end;
  end;

  // flush all buffers
  FlushFileBuffers(fhandle);

  // close the file
  if (CloseHandle(fhandle) = FALSE) then
    raise EWipeFatal.Create(sr.Get(STRRES_ID, '008'));

  // delete the file finally
  if (Windows.DeleteFile(PChar(sFilename)) = FALSE) then
    raise EWipeError.Create(sr.Get(STRRES_ID, '001'));
end;


//////////////////////////// TWipeDOD ////////////////////////////


constructor TWipeDOD.Create(rndsrc : TRandomSource);
var
  nI    : Integer;
  pSrc  : PWORD8Buf;
  pDest : PWORD8Buf;
begin
  inherited Create(rndsrc);

  // create the additional buffers
  m_pSwapBuffer:=Nil;
  m_pTempBuffer:=Nil;
  GetMem(m_pSwapBuffer, BUFFERSIZE);
  GetMem(m_pTempBuffer, BUFFERSIZE);

  // fill the first buffer and the last buffer with random data
  rndsrc.GetBytes(m_pBaseBuffer, BUFFERSIZE);
  rndsrc.GetBytes(m_pTempBuffer, BUFFERSIZE);

  // the second buffer is the complement of the first buffer
  pSrc:=m_pBaseBuffer;
  pDest:=m_pSwapBuffer;
  for nI:=0 to (BUFFERSIZE - 1) do
    pDest^[nI]:=not pSrc^[nI];
end;



destructor TWipeDOD.Destroy;
begin
  if (m_pSwapBuffer <> Nil) then begin
    FillChar(m_pSwapBuffer^, BUFFERSIZE, 0);
    FreeMem(m_pSwapBuffer);
  end;
  if (m_pTempBuffer <> Nil) then begin
    FillChar(m_pTempBuffer^, BUFFERSIZE, 0);
    FreeMem(m_pTempBuffer);
  end;

  inherited Destroy;
end;



function TWipeDOD.Execute(sFileName : String;
                          progresscall : TWipeProgress;
                          sr : TStrRes) : Boolean;
var
  blDone       : Boolean;
  nPass        : Integer;
  nError       : Integer;
  lToWrite     : WORD32;
  lOpenMask    : WORD32;
  dwDummy      : DWORD;
  pOutBuffer   : Pointer;
  fhandle      : THandle;
  qFileLen     : WORD64;
  qSaveFileLen : WORD64;
  qTemp        : WORD64;
begin
  // reset the file attributes
  nError:=FileSetAttr(sFileName, 0);
  if (nError <> 0) then
    raise EWipeError.Create(TStrPlusI.WinErrToStr(sr, nError));

  // get the 64bit file length
  try
    qFileLen:=TFileSupport.GetFile64Len(sFilename, sr);
  except
    on efse : EFileSupportError do
      raise EWipeError.Create(efse.Message);
  end;

  // store the pure file size for the progress callback
  progressCall.SetFileSize(qFileLen);

  // adjust the file length to the next unit
  Result:=m_clad.ClusterAdjust(sFileName, qFileLen, qFileLen);

  if (Result) then
    lOpenMask:=FILE_FLAG_WRITE_THROUGH or FILE_FLAG_NO_BUFFERING
  else
    lOpenMask:=FILE_FLAG_WRITE_THROUGH;

  // open the file to write (using Win32 API calls)
  fhandle:=CreateFile(PChar(sFilename),
                      GENERIC_WRITE,
                      0,
                      Nil,
                      OPEN_EXISTING,
                      lOpenMask,
                      0);
  if (fhandle = INVALID_HANDLE_VALUE) then
    raise EWipeError.Create(sr.Get(STRRES_ID, '002'));

  // wipe the file now
  progresscall.ZeroPos;
  qTemp:=qFileLen * 3; // (3 passes, so we have to extend the progress max.,
                       //  remember that now the max. file length will "only"
                       //  be 2^62 bytes)
  progresscall.SetMaxPos(qTemp);
  progresscall.SetNumOfPasses(3);
  progresscall.ZeroPass;
  qSaveFileLen:=qFileLen;
  nPass:=0;
  try
    while (nPass < 3) do begin

      // new pass
      progresscall.SetChanged(True);
      progresscall.IncPass;

      // set the correct buffer
      case nPass of
        0: pOutBuffer:=m_pBaseBuffer;
        1: pOutBuffer:=m_pSwapBuffer;
      else
        pOutBuffer:=m_pTempBuffer;
      end;

      // overwrite
      blDone:=False;
      while (not blDone) do begin
        // last loop?
        if (qFileLen < BUFFERSIZE) then begin
          lToWrite:=qFileLen;
          blDone:=True;
        end
        else
          lToWrite:=BUFFERSIZE;

        // send a progress message
        progresscall.CallBack;
        progresscall.SetChanged(False);

        // write the actual buffer content
        if (WriteFile(fhandle,
                      pOutBuffer^,
                      lToWrite,
                      dwDummy,
                      Nil) = FALSE) then begin
          // error occured
          if (CloseHandle(fhandle) = FALSE) then
            raise EWipeFatal.Create(sr.Get(STRRES_ID, '003'))
          else
            raise EWipeError.Create(sr.Get(STRRES_ID, '004'));
        end;

        // modify the counters
        Dec(qFileLen, lToWrite);
        progresscall.IncPos(lToWrite);
      end;

      // flush all buffers
      FlushFileBuffers(fhandle);

      // close and reopen the file
      if (nPass < 2) then begin

        CloseHandle(fhandle);
        fhandle:=CreateFile(PChar(sFilename),
                            GENERIC_WRITE,
                            0,
                            Nil,
                            OPEN_EXISTING,
                            lOpenMask,
                            0);

        if (fhandle = INVALID_HANDLE_VALUE) then
          raise EWipeError.Create(sr.Get(STRRES_ID, '006'));

        qFileLen:=qSaveFileLen;
      end;

      // next pass
      Inc(nPass);
    end;

    // send the last progress message
    progresscall.CallBack;

  except
    // handle the interruption
    on ECallBackInterrupt do begin
      if (CloseHandle(fhandle) = FALSE) then
        raise EWipeFatal.Create(sr.Get(STRRES_ID, '005'));
      raise EWipeWasBreak.Create(sr.Get(STRRES_ID, '007'));
    end;
  end;

  // (just to be sure)
  FlushFileBuffers(fhandle);

  // close the file
  if (CloseHandle(fhandle) = FALSE) then begin
    raise EWipeFatal.Create(sr.Get(STRRES_ID, '008'));
  end;

  // delete the file finally
  if (Windows.DeleteFile(PChar(sFilename)) = FALSE) then
    raise EWipeError.Create(sr.Get(STRRES_ID, '001'));
end;



//////////////////////////// TWipeSFS ////////////////////////////



constructor TWipeSFS.Create;
begin
  inherited Create;

  // allocate the buffer
  m_pBuffer:=Nil;
  GetMem(m_pBuffer, BUFFERSIZE);
end;



destructor TWipeSFS.Destroy;
begin
  if (m_pBuffer <> Nil) then begin
    FillChar(m_pBuffer^, BUFFERSIZE, 0);
    FreeMem(m_pBuffer);
  end;

  inherited Destroy;
end;


function TWipeSFS.Execute(sFileName : String;
                          progresscall : TWipeProgress;
                          sr : TStrRes) : Boolean;
var
  blDone             : Boolean;
  nPass              : Integer;
  nError             : Integer;
  nI, nJ             : Integer;
  lToWrite           : WORD32;
  lOpenMask          : WORD32;
  dwDummy            : DWORD;
  pBuildBuf          : PWORD8Buf;
  fhandle            : THandle;
  qFileLen           : WORD64;
  qSaveFileLen       : WORD64;
  qTemp              : WORD64;
begin

  // reset the file attributes
  nError:=FileSetAttr(sFileName, 0);
  if (nError <> 0) then
    raise EWipeError.Create(TStrPlusI.WinErrToStr(sr, nError));

  // get the 64bit file length
  try
    qFileLen:=TFileSupport.GetFile64Len(sFilename, sr);
  except
    on efse : EFileSupportError do
      raise EWipeError.Create(efse.Message);
  end;

  // store the pure file size for the progress callback
  progressCall.SetFileSize(qFileLen);

  // adjust the file length to the next unit
  Result:=m_clad.ClusterAdjust(sFileName, qFileLen, qFileLen);

  if (Result) then
    lOpenMask:=FILE_FLAG_WRITE_THROUGH or FILE_FLAG_NO_BUFFERING
  else
    lOpenMask:=FILE_FLAG_WRITE_THROUGH;

  // open the file to write (using Win32 API calls)
  fhandle:=CreateFile(PChar(sFilename),
                      GENERIC_WRITE,
                      0,
                      Nil,
                      OPEN_EXISTING,
                      lOpenMask,
                      0);
  if (fhandle = INVALID_HANDLE_VALUE) then
    raise EWipeError.Create(sr.Get(STRRES_ID, '002'));

  // wipe the file now
  progresscall.ZeroPos;
  qTemp:=qFileLen * 35; // (35 passes, so we have to extend the progress max.,
                        //  remember that the max. possible file size is now
                        //  "only" 2^58 bytes)
  progresscall.SetMaxPos(qTemp);
  progresscall.SetNumOfPasses(35);
  progresscall.ZeroPass;
  qSaveFileLen:=qFileLen;
  nPass:=0;
  try
    while (nPass < 35) do begin

      // new pass
      progresscall.SetChanged(True);
      progresscall.IncPass;

      // build the new buffer
      pBuildBuf:=m_pBuffer;
      nI:=0;
      for nJ:=0 to (BUFFERSIZE - 1) do begin
        pBuildBuf[nJ]:=SFS_BITS[nPass, nI];
        Inc(nI);
        if (nI > 2) then
          nI:=0;
      end;

      // overwrite
      blDone:=False;
      while (not blDone) do begin

        // last loop?
        if (qFileLen < BUFFERSIZE) then begin
          lToWrite:=qFileLen;
          blDone:=True;
        end
        else
          lToWrite:=BUFFERSIZE;

        // send a progress message
        progresscall.CallBack;
        progresscall.SetChanged(False);

        // write the actual buffer content
        if (WriteFile(fhandle,
                      m_pBuffer^,
                      lToWrite,
                      dwDummy,
                      Nil) = FALSE) then begin
          // error occured
          if (CloseHandle(fhandle) = FALSE) then
            raise EWipeFatal.Create(sr.Get(STRRES_ID, '003'))
          else
            raise EWipeError.Create(sr.Get(STRRES_ID, '004'));
        end;

        // modify the counters
        Dec(qFileLen, lToWrite);
        progresscall.IncPos(lToWrite);
      end;

      // flush all buffers
      FlushFileBuffers(fhandle);

      // close and reopen the file
      if (nPass < 34) then begin

        CloseHandle(fhandle);
        fhandle:=CreateFile(PChar(sFilename),
                            GENERIC_WRITE,
                            0,
                            Nil,
                            OPEN_EXISTING,
                            lOpenMask,
                            0);

        if (fhandle = INVALID_HANDLE_VALUE) then
          raise EWipeError.Create(sr.Get(STRRES_ID, '006'));

        //if (TFileSupport.SetFileArena(fhandle)) then
        //  raise EWipeError.Create(sr.Get(STRRES_ID, '009'));

        qFileLen:=qSaveFileLen;
      end;

      // next pass
      Inc(nPass);
    end;

    // send the last progress message
    progresscall.CallBack;
  except

    // handle the interruption
    on ECallBackInterrupt do begin
      if (CloseHandle(fhandle) = FALSE) then
        raise EWipeFatal.Create(sr.Get(STRRES_ID, '005'));
      raise EWipeWasBreak.Create(sr.Get(STRRES_ID, '007'));
    end;
  end;

  // (just to be sure)
  FlushFileBuffers(fhandle);

  // close the file
  if (CloseHandle(fhandle) = FALSE) then begin
    raise EWipeFatal.Create(sr.Get(STRRES_ID, '008'));
  end;

  // delete the file finally
  if (Windows.DeleteFile(PChar(sFilename)) = FALSE) then
    raise EWipeError.Create(sr.Get(STRRES_ID, '001'));
end;


end.
