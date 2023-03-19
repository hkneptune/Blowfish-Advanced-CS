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
  startup module to catch jobs raised by the shell extension, must be done
  before Application.Initialize() is going to be called, otherwise the
  shared memory area will be lost
}


unit ShellListener;

interface


// constants to detect what kind of job was passed through the shell ext.
const
  SHELLJOB_ENCRYPT   = 0;
  SHELLJOB_DECRYPT   = 1;
  SHELLJOB_WIPE      = 2;
  SHELLJOB_REENCRYPT = 3;
  SHELLJOB_DESLACK   = 4;
  SHELLJOB_WORKWITH  = 5;
  SHELLJOB_VIEW      = 6;



// checks for pending jobs thrown by the shell extension and passed
// through the command and a shared memory area, sets the globals
// _blShellJobCatched, _nShellJobMode and _pShellRawData
procedure _ListenForShellJob;


// the lobals for shell job (pre)storage
var
  // True: job was passed / False: no job
  _blShellJobCatched : Boolean;

  // kind of shell job (see SHELLJOB_xxx)
  _nShellJobMode : Integer;

  // pointer to the raw data copied form the shared memory, must be parsed
  // by higher instance later when the application was started completely (and
  // be freed with FreeMem() also; due to the nature of the data we mustn't
  // store the size of the data)
  _pShellRawData : Pointer;



implementation
uses Windows;


// the parameters used by the shell extension
// to call BFACS.EXE (that's us)
const
  BFACS_OP_ENCRYPT   = '-shellencrypt';
  BFACS_OP_DECRYPT   = '-shelldecrypt';
  BFACS_OP_WIPE	     = '-shellwipe';
  BFACS_OP_REENCRYPT = '-shellreencrypt';
  BFACS_OP_DESLACK   = '-shelldeslack';
  BFACS_OP_WORKWITH  = '-shellworkwith';
  BFACS_OP_VIEW	     = '-shellview';

// identifiers to catch the job data though the shared memory
const
  BFACS_OP_SMIPREFIX = '-shelldataid';
  SHAREMEM_ID_PREFIX = 'BFACSSHID';


procedure _ListenForShellJob;
var
  dwBufLen  : DWORD;
  hShareMem : THandle;
  szRawData : PChar;
  szScan    : PChar;
  sParam1   : String;
  sParam2   : String;
  sTemp     : String;
begin
  // assume no shell job
  _blShellJobCatched:=False;

  // right number of params?
  if (ParamCount <> 2) then
    Exit;

  // shell job?
  sParam1:=ParamStr(1);
  if (sParam1 = BFACS_OP_ENCRYPT) then
    _nShellJobMode:=SHELLJOB_ENCRYPT
  else
    if (sParam1 = BFACS_OP_DECRYPT) then
      _nShellJobMode:=SHELLJOB_DECRYPT
    else
      if (sParam1 = BFACS_OP_WIPE) then
        _nShellJobMode:=SHELLJOB_WIPE
      else
        if (sParam1 = BFACS_OP_REENCRYPT) then
          _nShellJobMode:=SHELLJOB_REENCRYPT
        else
          if (sParam1 = BFACS_OP_DESLACK) then
            _nShellJobMode:=SHELLJOB_DESLACK
          else
            if (sParam1 = BFACS_OP_WORKWITH) then
              _nShellJobMode:=SHELLJOB_WORKWITH
            else
              if (sParam1 = BFACS_OP_VIEW) then
                _nShellJobMode:=SHELLJOB_VIEW
              else
                Exit;


  // seens to be a shell job, now try to build the shared memory ID
  sParam2:=ParamStr(2);
  if (Copy(sParam2,
           1,
           Length(BFACS_OP_SMIPREFIX)) <> BFACS_OP_SMIPREFIX) then
    Exit;
  sTemp:=Copy(sParam2,
              Length(BFACS_OP_SMIPREFIX) + 1,
              4);
  if (Length(sTemp) <> 4) then  // (trust noone)
    Exit;

  // get access to the shared memory
  hShareMem:=OpenFileMapping(FILE_MAP_READ,
                             TRUE,
                             PChar(SHAREMEM_ID_PREFIX + sTemp));
  if (hShareMem = 0) then
    Exit;

  szRawData:=MapViewOfFile(hShareMem,
                           FILE_MAP_READ,
                           0,
                           0,
                           0);
  if (szRawData = Nil) then begin
    CloseHandle(hShareMem);
    Exit;
  end;

  // determine the length of the shared memory
  szScan:=szRawData;
  repeat                         // (find the #0#0)
    while (szScan^ <> #0) do
      Inc(szScan);
    Inc(szScan);
  until (szScan^ = #0);
  dwBufLen:=DWORD(szScan) - DWORD(szRawData) + 1;

  // copy the shared memory content
  GetMem(_pShellRawData, dwBufLen);
  Move(szRawData^, _pShellRawData^, dwBufLen);

  // detach from the shared memory
  UnmapViewOfFile(szScan);
  CloseHandle(hShareMem);

  // success
  _blShellJobCatched:=True;

end;



end.
