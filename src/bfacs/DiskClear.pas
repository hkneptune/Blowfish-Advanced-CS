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
  to clear the empty disk space
}

unit DiskClear;

interface
uses
  SysUtils,
  StringRes,
  ProgressCallBack,
  RandomManager,
  MessageCallBack;



// the clear modes
const
  DISKCLEAR_MODE_ZEROS        = 0;  // just write zeros
  DISKCLEAR_MODE_RANDOMBLOCK  = 1;  // static random data by a buffer
  DISKCLEAR_MODE_RANDOMSTREAM = 2;  // continous created random data


// default buffer size (1/2 MB)
const
  DISKCLEAR_DEFBUFSIZE = 524288;


// our own error
type
  EDiskClearError = class(Exception);
  EDiskClearBreak = class(EDiskClearError);


// the disk clearer
type
  TDiskClear = class
  private
    m_rndMng : TRandomManager;
    m_sr     : TStrRes;

  public

    // constructor
    // -> random manager
    // -> string resources
    constructor Create(rndMng : TRandomManager;
                       sr : TStrRes);

    // clears the empty disk space
    // -> path where to create the temporary file
    // -> the way to do it (see DISKCLEAR_MODE_xxx)
    // -> progress callback
    // -> message callback (to hold on before the temp file is deleted)
    // -> True: don't let the OS buffer data / False: don't care about caches
    // -> True: switch to low process priority / False: stay strong
    // -> buffer size
    // exception: EDiskClearError - if something went wrong
    // exception: EDiskClearBreak - a user break occured
    procedure Execute(const sDrivePath : String;
                      nMode : Integer;
                      prgCB : TProgressCallBack;
                      msgCB : TMessageCallBack = Nil;
                      blNoBuffering : Boolean = True;
                      blLowPriority : Boolean = True;
                      nBufferSize : Integer = DISKCLEAR_DEFBUFSIZE);

    // clears all drives
    // -> the way to do it (see DISKCLEAR_MODE_xxx)
    // -> True: hard drives only / False: all drives
    // -> progress callback
    // -> message callback
    // -> True: no messages in between / False: message after every drive
    // -> True: don't let the OS buffer data / False: don't care about caches
    // -> True: switch to low process priority / False: stay strong
    // -> buffer size
    procedure DoAllDrives(nMode : Integer;
                          blFixedDrivesOnly : Boolean;
                          prgCB : TProgressCallBack;
                          msgCB : TMessageCallBack;
                          blSilent : Boolean = True;
                          blNoBuffering : Boolean = True;
                          blLowPriority : Boolean = True;
                          nBufferSize : Integer = DISKCLEAR_DEFBUFSIZE);
  end;


implementation
uses
  Classes,
  bfacslib,
  General,
  StringPlusI,
  FileSupp,
  CallBack,
  Windows;


//////////////////////////// TDiskClear ////////////////////////////


const
  STRRES_ID = 'DISKCLEAR';


// max. size of a single file (2 GB currently)
// FIXME: we use this limit to be able to clear FAT32 drives with more
//        than 4 GB of free space, due to the nature of root paths one
//        cannot create more than 512 entries there, so in the best worst
//        case the max. size is MAX_CLEARFILE_SIZE * 512, which should be
//        enough for the next years...
const
  MAX_CLEARFILE_SIZE : WORD64 = 2147483648;

  

constructor TDiskClear.Create(rndMng : TRandomManager;
                              sr : TStrRes);
begin
  inherited Create;
  m_rndMng:=rndMng;
  m_sr:=sr;
end;


procedure TDiskClear.Execute(const sDrivePath : String;
                             nMode : Integer;
                             prgCB : TProgressCallBack;
                             msgCB : TMessageCallBack = Nil;
                             blNoBuffering : Boolean = True;
                             blLowPriority : Boolean = True;
                             nBufferSize : Integer = DISKCLEAR_DEFBUFSIZE);
var
  qFreeDiskSpace : WORD64;
  dwSavePrio     : DWORD;
  dwBytesWritten : DWORD;
  dwToWrite      : DWORD;
  dwError        : DWORD;
  dwLocalWritten : DWORD;
  blPrioChanged  : Boolean;
  blDone         : Boolean;
  pBuffer        : Pointer;
  hTempFile      : THandle;
  hProcess       : THandle;
  sRoot          : String;
  sClearFileName : String;
  clearFiles     : TStringList;

function DeleteClearFiles : Boolean;
var
  nF : Integer;
begin
  Result:=True;
  for nF:=0 to (clearFiles.Count - 1) do
    if (Windows.DeleteFile(PChar(clearFiles.Strings[nF])) = FALSE) then
      Result:=False;
end;

procedure ErrorCleanUp;
begin
  if (pBuffer <> Nil) then
    FreeMem(pBuffer);
  if (blPrioChanged) then
    SetPriorityClass(hProcess, dwSavePrio);
  if (hTempFile <> INVALID_HANDLE_VALUE) then
    CloseHandle(hTempFile);
  DeleteClearFiles;
  clearFiles.Destroy;  
end;

procedure NewClearFile;
var
  dwFlagsAttrs : DWORD;
begin
  // create the first clear file name
  sClearFileName:=TStrPlusI.MakeTempFileName(sDrivePath, '[-]', );

  // open the file
  if (blNoBuffering) then
    dwFlagsAttrs:=FILE_FLAG_WRITE_THROUGH
  else
    dwFlagsAttrs:=0;
  hTempFile:=CreateFile(PChar(sClearFileName),
                        GENERIC_WRITE,
                        0,
                        Nil,
                        CREATE_ALWAYS,
                        dwFlagsAttrs,
                        0);
  if (hTempFile = INVALID_HANDLE_VALUE) then begin
    dwError:=GetLastError;
    ErrorCleanUp;
    raise EDiskClearError.Create(
      Format(m_sr.Get(STRRES_ID, 'ERR_OPEN'),
                      [sClearFileName, TStrPlusI.WinErrToStr(m_sr, dwError)]));
  end;

  // new file out there
  clearFiles.Add(sClearFileName);
  dwLocalWritten:=0;
end;

procedure CloseClearFile;
begin
  FlushFileBuffers(hTempFile);
  if (CloseHandle(hTempFile) = FALSE) then begin
    dwError:=GetLastError;
    ErrorCleanUp; // (FIXME: 2nd close attempt in here wouldn't do any harm?)
    raise EDiskClearError.Create(
            Format(m_sr.Get(STRRES_ID, 'ERR_CLOSE'),
                            [sClearFileName,
                             TStrPlusI.WinErrToStr(m_sr, dwError)]));
  end;
  hTempFile:=INVALID_HANDLE_VALUE;
end;

begin
  // reset all necessary stuff
  pBuffer:=Nil;
  blPrioChanged:=False;
  hTempFile:=INVALID_HANDLE_VALUE;

  // create the file list
  clearFiles:=TStringList.Create;

  // allocate the buffer
  try
    GetMem(pBuffer, nBufferSize);
  except
    on EOutOfMemory do begin
      pBuffer:=Nil; // (just to be sure)
      ErrorCleanUp;
      raise EDiskClearError.Create(m_sr.Get(STRRES_ID, 'OUTOFMEM'));
    end;
  end;

  // open the first clear file
  NewClearFile;

  // get the size of the free disk space
  try
    qFreeDiskSpace:=TFileSupport.GetDiskFreeBytes(sDrivePath, m_sr);
  except
    on EFileSupportError do begin
      qFreeDiskSpace:=WORD64(-1);
    end;
  end;

  // prepare the progress callback
  with prgCB do begin
    ZeroPos;
    SetMaxPos(qFreeDiskSpace);
    SetChanged(True);
    SetMessage(sDrivePath);
  end;

  // lower our priority, if necessary
  if (blLowPriority) then begin
    hProcess:=GetCurrentProcess;
    dwSavePrio:=GetPriorityClass(hProcess);
    if (SetPriorityClass(hProcess, IDLE_PRIORITY_CLASS) = TRUE) then
      blPrioChanged:=True;
  end;

  // init. the buffer
  if ((nMode = DISKCLEAR_MODE_RANDOMBLOCK) or
      (nMode = DISKCLEAR_MODE_RANDOMSTREAM)) then
    m_rndMng.GetRandomSource.GetBytes(pBuffer, nBufferSize)
  else
    FillChar(pBuffer^, nBufferSize, 0);

  // start writing bytes like crazy now
  dwToWrite:=DWORD(nBufferSize);
  blDone:=False;

  repeat

    // first send a callback
    with prgCB do begin
      try
        CallBack;
        SetChanged(False);
        SetSignal(CALLBACK_SIGNAL_NULL);
      except
        on ECallBackInterrupt do begin
          ErrorCleanUp;
          raise EDiskClearBreak.Create('user break');
        end;
      end;
    end;

    // now write that thing
    if (WriteFile(hTempFile,
                  pBuffer^,
                  dwToWrite,
                  dwBytesWritten,
                  NULL) = FALSE) then begin
      dwError:=GetLastError;

      // disk full? (FIXME: got all possible error codes?)
      if ((dwError = ERROR_HANDLE_DISK_FULL) or
          (dwError = ERROR_DISK_FULL)) then begin

        // yes, so clear the rest
        try
          qFreeDiskSpace:=TFileSupport.GetDiskFreeBytes(sDrivePath, m_sr);
          dwToWrite:=DWORD(qFreeDiskSpace and (WORD64($0ffffffff)));
          // (no error checking here)
          WriteFile(hTempFile,
                    pBuffer^,
                    dwToWrite,
                    dwBytesWritten,
                    NULL);
          blDone:=True;
        except
          on EFileSupportError do begin
            // (FIXME: we don't know it better)
            dwBytesWritten:=0;
            blDone:=True;
          end;
        end;
      end
      else begin

        // that's an error so
        ErrorCleanUp;
        raise EDiskClearError.Create(
          Format(m_sr.Get(STRRES_ID, 'ERR_IO'),
                 [TStrPlusI.WinErrToStr(m_sr, dwError)]));
      end;
    end
    else begin

      // done?
      if (dwBytesWritten < dwToWrite) then
        blDone:=True;

      // need for a new file?
      Inc(dwLocalWritten, dwBytesWritten);
      if (dwLocalWritten >= MAX_CLEARFILE_SIZE) then begin
        // yes, so close the old and start with a new one
        CloseClearFile; 
        NewClearFile;
      end;

    end;

    // increase progress
    prgCB.IncPos(dwBytesWritten);

    // refill the buffer, if necessary
    if ((nMode = DISKCLEAR_MODE_RANDOMSTREAM) and (not blDone)) then
      m_rndMng.GetRandomSource.GetBytes(pBuffer, dwToWrite);

  until (blDone);

  // final callback
  with prgCB do begin
    try
      CallBack;
    except
      on ECallBackInterrupt do begin
        ErrorCleanUp;
        raise EDiskClearBreak.Create('user break');
      end;
    end;
  end;

  // close the last file
  CloseClearFile;
  hTempFile:=INVALID_HANDLE_VALUE;

  // now send a message before the files will be deleted
  if (msgCB <> Nil) then begin
    with msgCB do begin
      SetStyle(MCB_STYLE_OK);
      SetKindOf(MCB_KINDOF_INFORMATION);
      if (Copy(sDrivePath, 2, 1) = ':') then
        sRoot:=sDrivePath[1] + ':'
      else
        sRoot:=TStrPlusI.RootPath(sDrivePath);
      SetMessage(Format(m_sr.Get(STRRES_ID, 'DONEMESS'), [sRoot]));
      CallBack;
    end;
  end;

  // delete the clear files
  if (not DeleteClearFiles) then begin
    ErrorCleanUp;
    raise EDiskClearError.Create(m_sr.Get(STRRES_ID, 'ERR_DEL'));
  end;

  // clean up and quit
  FreeMem(pBuffer);
  if (blPrioChanged) then
    SetPriorityClass(hProcess, dwSavePrio);
  clearFiles.Destroy;  
end;


procedure TDiskClear.DoAllDrives(nMode : Integer;
                                 blFixedDrivesOnly : Boolean;
                                 prgCB : TProgressCallBack;
                                 msgCB : TMessageCallBack;
                                 blSilent : Boolean = True;
                                 blNoBuffering : Boolean = True;
                                 blLowPriority : Boolean = True;
                                 nBufferSize : Integer = DISKCLEAR_DEFBUFSIZE);
var
  blClearIt    : Boolean;
  cDriveLetter : Char;
  sDriveRoot   : String;
  execMsgCB    : TMessageCallBack;
begin

  // messages in between?
  if (blSilent) then
    execMsgCB:=Nil
  else
    execMsgCB:=msgCB;

  // prepare the progress callback
  prgCB.SetSignal(CALLBACK_SIGNAL_START);

  // work through all drives
  for cDriveLetter:='A' to 'Z' do begin

    // check for hard drives?
    sDriveRoot:=cDriveLetter + ':\';
    if (blFixedDrivesOnly) then
      blClearIt:=(GetDriveType(PChar(sDriveRoot)) = DRIVE_FIXED)
    else
      blClearIt:=(GetDriveType(PChar(sDriveRoot)) <> DRIVE_CDROM);

    if (blClearIt) then begin

      // the progress callback contains the drive root
      prgCB.SetMessage(sDriveRoot);

      // call the clear droide
      try
        Execute(sDriveRoot,
                nMode,
                prgCB,
                execMsgCB,
                blNoBuffering,
                blLowPriority,
                nBufferSize);
      except

        on EDiskClearBreak do begin
          // break out
          Exit;
        end;

        on edce : EDiskClearError do begin

          // ask if we should continue on an error
          with msgCB do begin
            SetStyle(MCB_STYLE_YESNO);
            SetKindOf(MCB_KINDOF_ERROR);
            SetMessage(Format(m_sr.Get(STRRES_ID, 'CONT_AFTER_ERR'),
                              [edce.Message]));
            CallBack;
            if (GetResult = MCB_RES_NO) then
              Exit
          end;
        end;

      end; (* OF TRY *)
    end; (* OF IF *)
  end; (* OF FOR *)

end;

end.
