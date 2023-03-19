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
  class containing support routines for file handling
}

unit FileSupp;


interface
uses Windows,
     SysUtils,
     bfacslib,
     RandomSource,
     StringRes;

// our own exception class
type
  EFileSupportError = class(Exception);


type
  TFileClusterAdjuster = class
  private
    m_clusterSizes : array[0..25] of Integer;

  public
    // constructor
    constructor Create;

    // resets the lookup table
    procedure Reset;

    // adjusts the length of a file  to the next cluster border (if possible)
    // -> file name (full qualified path)
    // -> real file size
    // -> where to put the adjusted file size
    // -> use lookup table to speed up things
    // <- True: cluster access possible / False: no access (real = adjusted)
    function ClusterAdjust(const sFileName : String;
                           qFileSize : WORD64;
                           var vqAdjustedFileSize : WORD64;
                           blUseLookup : Boolean = True) : Boolean;
  end;



// general file support stuff
type
  TFileSupport = class
  public

    // gets the 64bit length of a file
    // -> name of the file
    // -> string resources
    // <- length of the file
    // exception: EFileSupportError - if an error occured
    class function GetFile64Len(sFileName : String;
                                sr : TStrRes) : WORD64;


    // returns the number of bytes free on a drive
    // -> drive path
    // -> string resources
    // <- number of free bytes on this drive
    // exception:EFileSupportError - if an error occured
    class function GetDiskFreeBytes(sDrive : String;
                                    sr : TStrRes) : WORD64;

    // checks if a file or folder exists
    // -> file/folder
    // <- True: object exists / False: does not
    class function ObjectExists(sObject : String) : Boolean;

    // checks if a filename is legal
    // -> the filename to examine (must not contain any path!)
    // -> pointer where to return the first illegal character found
    // -> True: allow path characters / False: do not
    // <- True: legal / False: contains invalid characters
    class function IsLegalFileName(const sFileName : String;
                                   pFirstIllegalChar : PChar = Nil;
                                   blAllowPathChars : Boolean = False)
                                     : Boolean;

    // creates an attribute exclude mask
    // -> exclude archive files
    // -> exclude readonly files
    // -> exclude hidden files
    // -> exclude system files
    // -> exclude compressed files
    // -> exclude temporary files
    // -> exclude offline files
    // -> exclude directories
    // <- the exclude mask
    class function MakeExcludeAttrMask(blExcludeArchive : Boolean;
                                       blExcludeReadOnly : Boolean;
                                       blExcludeHidden : Boolean;
                                       blExcludeSystem : Boolean;
                                       blExcludeCompress : Boolean = False;
                                       blExcludeTemporary : Boolean = True;
                                       blExcludeOffLine : Boolean = True;
                                       blExcludeDirectory : Boolean = False)
                                         : Integer;

    // opens a file
    // -> filename
    // -> window handle
    // -> string resources
    // exception: EFileSupportError detailed error message delivered
    class procedure OpenFile(const sFileName : String;
                             hWnd : THandle;
                             sr : TStrRes);

    // adds padding data to an already opened and writable file, does not close
    // the file (NOTE: came out of an idea to pad cryptfiles to onfuscate the
    // real file sizes; didn't work however since the data length is in the
    // header of each encrypted file in plaintext and cannot be altered with)
    // -> absolute path of the opened file to pad
    // -> Win32 file handle
    // -> padding size (0 to use the cluster size, or 512 if not available)
    // -> secure random generator
    // <- NOERROR on success, otherwise Windows error code
    class function PadFile(const sFilePath : String;
                           hFile : THandle;
                           nPadSize : Integer;
                           rs : TRandomSource) : DWORD;
  end;


implementation
uses
  StringPlus,
  ShellApi;


//////////////////////////// TFileClusterAdjuster  ////////////////////////////

const
  FCA_UNKNOWN  = -1;
  FCA_NOACCESS = -2;

constructor TFileClusterAdjuster.Create;
begin
  Reset;
end;

procedure TFileClusterAdjuster.Reset;
var
  nI : Integer;
begin
  for nI:=0 to 25 do
    m_clusterSizes[nI]:=FCA_UNKNOWN;
end;

function TFileClusterAdjuster.ClusterAdjust(const sFileName : String;
                                            qFileSize : WORD64;
                                            var vqAdjustedFileSize : WORD64;
                                            blUseLookup : Boolean = True)
                                              : Boolean;
var
  sRoot               : String;
  cDrive              : Char;
  dwSectorsPerCluster : DWORD;
  dwBytesPerSector    : DWORD;
  dwDummy             : DWORD;
  nDrive              : Integer;
  nCSize              : Integer;

function AdjustFileSize : WORD64;
var
  qTemp : WORD64;
begin
  qTemp:=qFileSize mod nCSize;
  if (qTemp <> 0) then
    Result:=(qFileSize - qTemp) + nCSize
  else
    Result:=qFileSize;
end;

begin
  // get the root path
  sRoot:=Copy(sFileName, 1, 2) + '\';

  // drive?
  cDrive:=UpCase(sRoot[1]);
  if ((cDrive < 'A') or (cDrive > 'Z') or (sRoot[2] <> ':')) then begin
    vqAdjustedFileSize:=qFileSize;
    Result:=False;
    Exit;
  end;

  // try lookup?
  nDrive:=Ord(cDrive) - Ord('A');
  nCSize:=m_clusterSizes[nDrive];
  if (blUseLookup) then begin

    // previously failed?
    if (nCSize = FCA_NOACCESS) then begin
      vqAdjustedFileSize:=qFileSize;
      Result:=False;
      Exit;
    end;

    // cluster size not determined yet?
    if (nCSize <> FCA_UNKNOWN) then begin
      vqAdjustedFileSize:=AdjustFileSize;
      Result:=True;
      Exit;
    end;
  end;

  // check out the cluster size
  if (GetDiskFreeSpace(PChar(sRoot),
                       dwSectorsPerCluster,
                       dwBytesPerSector,
                       dwDummy,
                       dwDummy) = TRUE) then begin

    nCSize:=dwBytesPerSector * dwSectorsPerCluster;
    vqAdjustedFileSize:=AdjustFileSize;
    Result:=True;
  end
  else begin
    m_clusterSizes[nDrive]:=FCA_NOACCESS;
    vqAdjustedFileSize:=qFileSize;
    Result:=False;
  end;

end;




//////////////////////////// TFileSupport  ////////////////////////////


const
  STRRES_ID = 'FILESUPP';



// this local flag is used by TFileSupport.GetDiskFreeBytes() to detect if
// GetDiskFreeSpaceEx() is available or not
var
  _blGetDiskFreeSpaceExOK : Boolean = False;

// the type for this function
type
  TGetDiskFreeSpaceExA = function (lpDirectoryName: PAnsiChar;
                                   var lpFreeBytesAvailableToCaller,
                                     lpTotalNumberOfBytes;
                                   lpTotalNumberOfFreeBytes: PLargeInteger)
                                     : BOOL; stdcall;
var
  gdfse : TGetDiskFreeSpaceExA;


class function TFileSupport.GetFile64Len(sFileName : String;
                                         sr : TStrRes) : WORD64;
var
  hFinder : THandle;
  dta     : TWin32FindData;
begin
  // get the file information
  hFinder:=FindFirstFile(PChar(sFileName), dta);
  if (hFinder = INVALID_HANDLE_VALUE) then
    raise EFileSupportError.Create(sr.Get(STRRES_ID, '000'));
  Windows.FindClose(hFinder);

  // return the file length
  Result:=MakeWORD64(dta.nFileSizeLow, dta.nFileSizeHigh);
end;



class function TFileSupport.GetDiskFreeBytes(sDrive : String;
                                             sr : TStrRes) : WORD64;
var
  qFreeBytesAvailableToCaller : WORD64;
  qTotalNumberOfBytes         : WORD64;
  dwSectorsPerCluster         : DWORD;
  dwBytesPerSector            : DWORD;
  dwNumberOfFreeClusters      : DWORD;
  dwTotalNumberOfClusters     : DWORD;
begin
  // reduce to the root path
  sDrive:=TStrPlus.RootPath(sDrive);

  // should we try the new function?
  if (_blGetDiskFreeSpaceExOK) then begin

    if (gdfse(PChar(sDrive),
              qFreeBytesAvailableToCaller,
              qTotalNumberOfBytes,
              Nil) = FALSE) then
      raise EFileSupportError.Create(sr.Get(STRRES_ID, '001'))
    else begin
      Result:=qFreeBytesAvailableToCaller;
      Exit;
    end;
  end;

  // get the number of free disk space by the old way
  if (GetDiskFreeSpace(PChar(sDrive),
                       dwSectorsPerCluster,
                       dwBytesPerSector,
                       dwNumberOfFreeClusters,
                       dwTotalNumberOfClusters) = FALSE) then
    raise EFileSupportError.Create(sr.Get(STRRES_ID, '002'));

  Result:=WORD64(dwNumberOfFreeClusters) * WORD64(dwSectorsPerCluster) *
          WORD64(dwBytesPerSector);
end;



class function TFileSupport.ObjectExists(sObject : String) : Boolean;
var
  hSearch : THandle;
  fdata   : TWin32FindData;
begin
  // check it out directly via the Win32 API
  hSearch:=FindFirstFile(PChar(sObject), fdata);
  if (hSearch = INVALID_HANDLE_VALUE) then
    Result:=False
  else begin
    Result:=True;
    CloseHandle(hSearch);
  end;
end;


class function TFileSupport.IsLegalFileName(const sFileName : String;
                                            pFirstIllegalChar : PChar = Nil;
                                            blAllowPathChars : Boolean = False)
                                             : Boolean;
// the illegal character sets
const
  ILLCHARS_ALL  = ':~\/"<>|?*';
  ILLCHARS_PATH = '~/"<>|?*';
var
  nI        : Integer;
  nPos      : Integer;
  nLen      : Integer;
  nNameLen  : Integer;
  cActChar  : Char;
  sIllChars : String;
begin
  // assume an error
  Result:=False;

  // check the simple case
  if ((sFileName = '..') or (sFileName = '.') or (sFileName = '')) then
    Exit;

  // (speed optimized code, run over filename because in most cases it will
  //  be longer than sIllChars)
  if (blAllowPathChars) then
    sIllChars:=ILLCHARS_PATH
  else
    sIllChars:=ILLCHARS_ALL;
  nLen:=Length(sIllChars);
  nNameLen:=Length(sFileName);

  // start the test
  for nPos:=1 to nNameLen do begin
    cActChar:=sFileName[nPos];
    for nI:=1 to nLen do begin
      if (cActChar = sIllChars[nI]) then begin
        if (pFirstIllegalChar <> Nil) then
          pFirstIllegalChar^:=cActChar;
        Exit
      end;
    end;
  end;

  // test passed
  Result:=True;
end;


class function TFileSupport.MakeExcludeAttrMask(
                 blExcludeArchive : Boolean;
                 blExcludeReadOnly : Boolean;
                 blExcludeHidden : Boolean;
                 blExcludeSystem : Boolean;
                 blExcludeCompress : Boolean = False;
                 blExcludeTemporary : Boolean = True;
                 blExcludeOffLine : Boolean = True;
                 blExcludeDirectory : Boolean = False) : Integer;
begin
  Result:=0;
  if (blExcludeArchive)   then Result:=Result or FILE_ATTRIBUTE_ARCHIVE;
  if (blExcludeReadOnly)  then Result:=Result or FILE_ATTRIBUTE_READONLY;
  if (blExcludeHidden)    then Result:=Result or FILE_ATTRIBUTE_HIDDEN;
  if (blExcludeSystem)    then Result:=Result or FILE_ATTRIBUTE_SYSTEM;
  if (blExcludeCompress)  then Result:=Result or FILE_ATTRIBUTE_COMPRESSED;
  if (blExcludeTemporary) then Result:=Result or FILE_ATTRIBUTE_TEMPORARY;
  if (blExcludeOffLine)   then Result:=Result or FILE_ATTRIBUTE_OFFLINE;
  if (blExcludeDirectory) then Result:=Result or FILE_ATTRIBUTE_DIRECTORY;
end;


class procedure TFileSupport.OpenFile(const sFileName : String;
                                      hWnd : THandle;
                                      sr : TStrRes);
var
  nErr   : Integer;
  sStrID : String;
begin
  nErr:=ShellExecute(hWnd,
                     'open',
                     PChar(sFileName),
                     NULL,
                     '',
                     SW_SHOW);

  if (nErr <= 32) then begin

    case nErr of
      0	                     : sStrID:='SHEX000';
      ERROR_FILE_NOT_FOUND   : sStrID:='SHEX001';
      ERROR_PATH_NOT_FOUND   : sStrID:='SHEX002';
      ERROR_BAD_FORMAT       : sStrID:='SHEX003';
      SE_ERR_ACCESSDENIED    : sStrID:='SHEX004';
      SE_ERR_ASSOCINCOMPLETE : sStrID:='SHEX005';
      SE_ERR_DDEBUSY         : sStrID:='SHEX006';
      SE_ERR_DDEFAIL         : sStrID:='SHEX007';
      SE_ERR_DDETIMEOUT      : sStrID:='SHEX008';
      SE_ERR_DLLNOTFOUND     : sStrID:='SHEX009';
      SE_ERR_NOASSOC         : sStrID:='SHEX010';
      SE_ERR_OOM             : sStrID:='SHEX011';
      SE_ERR_SHARE           : sStrID:='SHEX012';
    end;
    raise EFileSupportError.Create(sr.Get(STRRES_ID, sStrID));
  end;

end;


class function TFileSupport.PadFile(const sFilePath : String;
                                    hFile : THandle;
                                    nPadSize : Integer;
                                    rs : TRandomSource) : DWORD;
var
  ca : TFileClusterAdjuster;
  dwSizeHi, dwSizeLo, dwToWrite, dwWritten : DWORD;
  qSize, qAdjustedSize, qRest : WORD64;
  pFillData : Pointer;
begin

  // no padding at all?
  Result:=NOERROR;
  if (0 > nPadSize) then Exit;

  // get the current file position (which is the actual file size)
  dwSizeLo:=Windows.GetFileSize(hFile, @dwSizeHi);

  // error occured?
  if ($ffffffff = dwSizeLo) then begin
    Result:=Windows.GetLastError;
    if (NOERROR <> Result) then Exit;
  end;

  // assemble the current size
  qSize:=(WORD64(dwSizeHi) shl 32) or (WORD64(dwSizeLo) and $0ffffffff);

  // do we have to adjust to the volume's cluster size?
  if (0 < nPadSize) then begin

    // no, so we have to adjust to the given value
    qRest:=qSize mod nPadSize;
    if (0 < qRest) then qAdjustedSize:=qSize + (nPadSize - qRest);
  end
  else begin
    // TODO: we could support a context kept cluster adjuster in the future
    ca:=TFileClusterAdjuster.Create;
    if (not ca.ClusterAdjust(sFilePath, qSize, qAdjustedSize, False)) then begin

      // on failure adjust to (what we think is a good) default size
      qRest:=qSize mod 512;
      if (0 < qRest) then qAdjustedSize:=qSize + (512 - qRest);
    end;

    ca.Destroy;
  end;

  // write the padding data (if necessary)
  dwToWrite:=DWORD(qAdjustedSize - qSize);
  if (0 < dwToWrite) then begin
    // allocate the padding buffer
    try
      GetMem(pFillData, dwToWrite);
    except
      on EOutOfMemory do begin
        Result:=ERROR_NOT_ENOUGH_MEMORY;
        Exit;
      end;
    end;
    // fill the padding buffer with random data
    rs.GetBytes(pFillData, dwToWrite);
    // and write the padding data
    dwWritten:=0;
    if (not Windows.WriteFile(hFile,
                              pFillData^,
                              dwToWrite,
                              dwWritten,
                              Nil)) then
      Result:=Windows.GetLastError;
    FreeMem(pFillData);
  end;
end;


// check out if GetDiskFreeSpaceEx() is available
var
  hLib      : THandle;
  pLoadFunc : Pointer;

initialization
  hLib:=LoadLibrary('KERNEL32.DLL');
  if (hLib <> 0) then begin
    pLoadFunc:=GetProcAddress(hLib, 'GetDiskFreeSpaceExA');
    if (pLoadFunc <> Nil) then begin
      // yes, it's out there
      gdfse:=pLoadFunc;
      _blGetDiskFreeSpaceExOK:=True;
    end;
    FreeLibrary(hLib);
  end;

end.

