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
  sugar and spice and everything we might need for key disk handling
}


unit KeyDisk;

interface
uses SecureMem,
     RandomManager,
     MessageCallBack,
     bfacslib,
     StringRes;


// key disk sizes
const
  KEYDISK_SIZE_RECOMMENDED = 128;
  KEYDISK_SIZE_DONTCARE    = -1;   // ("infinite" key disk size)


// key disk reader (static)
type
  TKeyDisk = class

    // reads out a key disk
    // -> the key file (and its path)
    // -> string resources
    // -> to ask for a retry
    // -> existing key to which to append the new, if wanted
    // -> max. number of bytes to read
    // <- key material, carefully wrapped (maybe Nil)
    class function ReadDisk(const sKeyFile : String;
                            sr : TStrRes;
                            retryRequestCB : TMessageCallBack = Nil;
                            appendToThis : TKeyMemory = Nil;
                            nMaxSize : Integer =
                              KEYDISK_SIZE_DONTCARE) : TKeyMemory;

    // creates a key disk
    // -> file name
    // -> random manager
    // -> size (in bytes)
    // -> True: overwrite existing file / False: do not
    // <- True: successfully create / False: error occured
    class function CreateDisk(const sKeyFile : String;
                              rndMng : TRandomManager;
                              lSize : WORD32;
                              blOverwrite : Boolean = False) : Boolean;

  end;



implementation
uses Windows,
     General,
     SysUtils,
     StringPlusI;


//////////////////////////// TKeyDisk ////////////////////////////


// string resources ID
const
  STRRES_ID = 'KEYDISK';

// max. key data used
const
  MAX_KEY_SIZE = 65520;


class function TKeyDisk.ReadDisk(const sKeyFile : String;
                                 sr : TStrRes;
                                 retryRequestCB : TMessageCallBack = Nil;
                                 appendToThis : TKeyMemory = Nil;
                                 nMaxSize : Integer = KEYDISK_SIZE_DONTCARE)
                                   : TKeyMemory;
var
  dwKeyFileSize : DWORD;
  dwBytesToRead : DWORD;
  dwBytesRead   : DWORD;
  dwLastError   : Integer;
  nOldKeySize   : Integer;
  pNewKeyAddr   : Pointer;
  blDone        : Boolean;
  sError        : String;
  hFile         : THandle;
begin

  // we allow a retry
  blDone:=False;
  Result:=Nil;
  while (not blDone) do begin

    // to read out the file secure we do it directly over the Win32 API
    dwLastError:=NO_ERROR;
    sError:=sr.Get(STRRES_ID, 'DUNNO');
    hFile:=CreateFile(PChar(sKeyFile),
                      GENERIC_READ,
                      0,        //  file should be locked!
                      Nil,
                      OPEN_EXISTING,
                      FILE_FLAG_SEQUENTIAL_SCAN,
                      0);
    if (hFile = INVALID_HANDLE_VALUE) then

      dwLastError:=GetLastError

    else begin

      // how large must the new key buffer be?
      dwKeyFileSize:=GetFileSize(hFile, Nil);
      if (dwKeyFileSize = $ffffffff) then

        dwLastError:=GetLastError
      else begin

        // (restrict, if necessary)
        if (dwKeyFileSize > MAX_KEY_SIZE) then
          dwKeyFileSize:=MAX_KEY_SIZE;

        // more than we need?
        dwBytesToRead:=dwKeyFileSize;
        if (nMaxSize <> KEYDISK_SIZE_DONTCARE) then begin

          if (dwKeyFileSize > DWORD(nMaxSize)) then begin

            // let the user know that we're not using the whole key file,
            // also provide a chance to let it be
            with retryRequestCB do begin
              SetStyle(MCB_STYLE_YESNO);
              SetKindOf(MCB_KINDOF_QUESTION);
              SetMessage(Format(sr.Get(STRRES_ID, 'KEYFILETOOLARGE'),
                                [sKeyFile,
                                 TStrPlusI.Sepa1000(sr, dwKeyFileSize),
                                 TStrPlusI.Sepa1000(sr, nMaxSize)]));
              CallBack;
              if (GetResult = MCB_RES_NO) then begin
                CloseHandle(hFile); // FIXME: check for errors here?
                Exit;
              end
              else begin
                dwBytesToRead:=DWORD(nMaxSize);
              end;  
            end;

          end;
        end;

        try
          // allocate the new key memory
          if (appendToThis = Nil) then
            nOldKeySize:=0
          else
            nOldKeySize:=appendToThis.GetSize;

          Result:=TKeyMemory.Create(nOldKeySize + Integer(dwBytesToRead));

       //   debd('x', nOldKeySize + Integer(dwBytesToRead));

          // read out the new file in one i/o operation
          pNewKeyAddr:=Pointer(DWORD(Result.GetPtr) + DWORD(nOldKeySize));
          if (ReadFile(hFile,
                       pNewKeyAddr^,
                       dwBytesToRead,
                       dwBytesRead,
                       Nil) = FALSE) then begin
            dwLastError:=GetLastError;
            Result.Destroy;
            Result:=Nil;
          end
          else begin

            // enough bytes read?
            if (dwBytesRead <> dwBytesToRead) then begin
              sError:=sr.Get(STRRES_ID, 'IOERR');
              Result.Destroy;
              Result:=Nil;
            end
            else
              // copy the old key, i. n.
              if (nOldKeySize > 0) then
                Result.SetData(appendToThis.GetPtr,
                               0,
                               appendToThis.GetSize);
          end;
        except
          on EOutOfMemory do
            // (such an error shouldn't happen in this world and dimension)
            sError:=sr.Get(STRRES_ID, 'OUTOFMEM');
        end;
      end;

      // close the key file
      if (CloseHandle(hFile) = FALSE) then begin
        if ((Result <> Nil) and (dwLastError <> NO_ERROR)) then
          dwLastError:=GetLastError;
        if (Result <> Nil) then begin
          Result.Destroy;
          Result:=Nil;
        end;
      end;
    end;

    // got a key?
    blDone:=(Result <> Nil);
    if (not blDone) then

      // nope, show the problem and offer a retry, if possible
      if (retryRequestCB <> Nil) then begin

        if (dwLastError <> NO_ERROR) then
          sError:=TStrPlusI.WinErrToStr(sr, dwLastError);

        with retryRequestCB do begin
          SetStyle(MCB_STYLE_YESNO);
          SetKindOf(MCB_KINDOF_ERROR);
          SetMessage(Format(sr.Get(STRRES_ID, 'ERRMESS'),
                            [sKeyFile, sError]));
          CallBack;
          if (GetResult = MCB_RES_NO) then
            blDone:=True;
        end;
      end;

  end; (* OF WHILE *)
end;


class function TKeyDisk.CreateDisk(const sKeyFile : String;
                                   rndMng : TRandomManager;
                                   lSize : WORD32;
                                   blOverwrite : Boolean = False) : Boolean;
var
  dwCreate  : DWORD;
  dwWritten : DWORD;
  key       : TKeyMemory;
  hFile     : THandle;

// subroutine to clean up after an error occured
procedure ErrorCleanUp;
begin
  if (hFile <> INVALID_HANDLE_VALUE) then begin
    if (CloseHandle(hFile) = TRUE) then begin
      // (don't leave any damaged key files left)
      DeleteFile(PChar(sKeyFile));
    end;
  end;
  if (key <> Nil) then
    key.Destroy;
end;

begin
  // assume an error
  Result:=False;
  key:=Nil;

  // select the right create distribution
  if (blOverwrite) then
    dwCreate:=CREATE_ALWAYS
  else
    dwCreate:=CREATE_NEW;

  // we're providing key material, so write through all caches,
  // unfort. we cannot use the most direct way, (no sector sized data)
  hFile:=CreateFile(PChar(sKeyFile),
                    GENERIC_WRITE,
                    0,
                    Nil,
                    dwCreate,
                    FILE_FLAG_WRITE_THROUGH,
                    0);
  if (hFile = INVALID_HANDLE_VALUE) then begin
    ErrorCleanUp;
    Exit;
  end;

  // make the key memory
  try
    key:=TKeyMemory.Create(lSize);
  except
    on EOutOfMemory do Exit;
  end;

  // time to get the key now (random data)
  with rndMng do begin
    FlushSeed; // (use all seed data available)
    GetRandomSource.GetBytes(key.GetPtr, lSize);
  end;

  // write the data to the file
  if (WriteFile(hFile,
                key.GetPtr^,
                lSize,
                dwWritten,
                Nil) = FALSE) then begin
    ErrorCleanUp;
    Exit;
  end;

  // key not needed anymore
  key.Destroy;
  key:=Nil;

  // all bytes written?
  if (dwWritten <> lSize) then begin
    ErrorCleanUp;
    Exit;
  end;

  // don't leave anything in the i/o queue
  FlushFileBuffers(hFile);

  // close and quit
  if (CloseHandle(hFile) = FALSE) then
    Result:=True;
end;



end.
