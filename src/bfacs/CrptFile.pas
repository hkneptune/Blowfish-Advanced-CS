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
  offers symmetric encryption of binary files (streamed i/o),
  supports 64bit file handling
}

unit CrptFile;

interface
uses
  Windows,
  SysUtils,
  RandomSource,
  CipherServer,
  KeyCreator,
  SecureMem,
  KeyHash,
  bfacslib,
  StringPlus,
  StringRes;


// magic ID of a cryptfile
const
  CRYPTFILE_MAGIC = $92190824;

// salt size
const
  CRYPTFILE_SALTSIZE = 11;


// max. key size
const
  CRYPTFILE_MAXKEYSIZE = 65536 - CRYPTFILE_SALTSIZE;


// a salt is just a byte array
type
  PCRYPTFILESALT = ^TCRYPTFILESALT;
  TCRYPTFILESALT = packed array[0..CRYPTFILE_SALTSIZE - 1] of WORD8;


// i/o buffer size
const
  CRYPTFILE_IOBUFSIZE = 65536;  // 64kB i/o buffer (the max. block size)



// header of a cryptfile, not needed by the caller
type
  PCRYPTFILEHEADER = ^TCRYPTFILEHEADER;
  TCRYPTFILEHEADER = packed record
    // to identify a cryptfile
    lMagic : WORD32;
    // size of the header, for later extensions,
    // should not be larger than 16kB (depends on the i/o buffer size)
    wSizeOfHeader : WORD16;
    // version of the CryptFile library
    wVersion : WORD16;
    // number of bytes stored in the cryptfile
    lLengthLo : WORD32;
    lLengthHi : WORD32;
    // size of the init. data area
    wCipherInitDataSize : WORD16;
    // block size of used cipher
    wCipherBlockSize : WORD16;
    // the salt
    salt : TCRYPTFILESALT;
    // hash of salt and key, 32bit "folded" MD5 digest
    lKeyHash : WORD32;
  end;




// the main class
type
  TCryptFile = class
  private
    // context part...

    // cipher instance
    m_cipher : TCipher;
    // cipher information
    m_cinfo : TCipherInfo;
    // the i/o buffer, we don't use a dynamical allocation because a context
    // should only be as long active as it is really needed (and the i/o
    // buffer needs not that much of memory)
    m_iobuffer : packed array[0..CRYPTFILE_IOBUFSIZE - 1] of WORD8;

    // handle part...

    // file name and path
    m_sFilePath : String;
    // Win32 file handle
    m_hFile : THandle;
    // size of file if opened for reading
    m_qFileSize : WORD64;
    // real file size (if open to read)
    m_qRealFileSize : WORD64;
    // file open mode (used internally)
    m_nOpenMode : Integer;
    // the key salt and hash is stored here (the header will be written
    // after all other output has been done, not earlier)
    m_saveSalt : TCRYPTFILESALT;
    m_lSaveKeyHash : WORD32;
    // header size (for the case of a reset)
    m_wSaveHeaderSize : WORD16;
    // number of bytes remaining in the buffer
    m_lBytesInBuf : WORD32;
    // actual buffer start position (on CRYPTFILE_OPEN_READ)
    m_lBufPos : WORD32;
    // file bytes read or written
    m_qBytesDone : WORD64;

    // some flags
    m_blNoCache        : Boolean;
    m_blNoKeyHashCheck : Boolean;

    // string resources
    m_sr : TStrRes;

    // internal function to read out some bytes from a file with error code
    // return (to make the following routines more ledigble and compact)
    // -> pointer where to read
    // -> number of bytes to read
    // -> Win32 file handle
    // -> where to put the error code with
    // CRYPTFILE_ERROR_IOERROR       - i/o error
    // CRYPTFILE_ERROR_FILETRUNCATED - file seems to be truncated
    // <- True: success / False : error
    function ReadData(pWhereTo : Pointer;
                      lHowMany : WORD32;
                      hFile : THandle;
                      var vlErrorCode : WORD32) : Boolean;

    // internal function to write some bytes into a file with error code return
    // -> pointer from where to write
    // -> number of bytes to write
    // -> Win32 file handle
    // -> where to put the error code (if error occured) with
    // CRYPTFILE_ERROR_IOERROR  - i/o error
    // CRYPTFILE_ERROR_DISKFULL - data carrier run out of space
    // <- True: success / False : error occured
    function WriteData(pFromWhere : Pointer;
                       lHowMany : WORD32;
                       hFile : THandle;
                       var vlErrorCode : WORD32) : Boolean;

    // exception raiser
    // -> the error code uasingthe exception
    // exception: some inherited ECryptFileError
    procedure Eraiser(lErrorCode : WORD32);

  public
    // loads and tests the cipher
    // -> name of the cipher
    // -> string resources
    // -> random source reference (may be Nil)
    // -> pointer to random seed data (only used if rs is Nil,
    //    may also be Nil)
    // -> number of random seed bytes (ign. if pRandSeed = Nil)
    // exception:
    // ECryptFileFatal           - fatal error occured
    // ECryptFileCipherNotFound  - cipher not found
    // ECryptFileInvalidCipher   - invalid cipher
    // ECryptFileSelfTestFailed  - cipher selftst failed
    // ECryptFileOutOfMemory     - out of memory
    constructor Create(const sCipherName : String;
                       sr : TStrRes;
                       rs : TRandomSource = Nil;
                       pRandSeed : Pointer = Nil;
                       lRandSeedLen : WORD32 = 0); virtual;

    // releases the cipher
    // exception:
    // ECryptFileFatal - cipher could not been released
    destructor Destroy; override;

    // opens a cryptfile for reading, remember that if the file was created
    // and an error occured there might be an invalid copy left
    // -> pointer to file name (in Unicode format)
    // -> pointer to the key material
    // -> number of key bytes
    // exception:
    // ECryptFileAccessDenied    - access denied to the file
    // ECryptFileFileNotFound    - cryptfile could not be found
    // ECryptFileOpenError       - general open error
    // ECryptFileNoCryptFile     - file is not a cryptfile
    // ECryptFileOutOfMemory     - out of memory
    // ECryptFileWrongPassword   - password hash does not match
    // ECryptFileWrongCipherType - cipher does not fit to this cryptfile
    // ECryptFileInvalidOpenMode - invalid open mode
    // ECryptFileZeroKey         - zero password detected
    // ECryptFileKeyTooLarge     - key too large
    // ECryptFileAlreadyOpened   - already opened a cryptfile
    procedure OpenRead(pFileName : PWideChar;
                       pKey : Pointer;
                       wKeyLen : WORD16);

    // creates a new cryptfile, remember if an error occured there might be an
    // invalid copy left
    // -> pointer to file name (in Unicode format)
    // -> pointer to the key material
    // -> number of key bytes
    // exception:
    // ECryptFileOpenError       - general open error
    // ECryptFileOutOfMemory     - out of memory
    // ECryptFileInvalidOpenMode - invalid open mode
    // ECryptFileZeroKey         - zero password detected
    // ECryptFileKeyTooLarge     - key too large
    // ECryptFileAlreadyOpened   - already opened a cryptfile
    procedure OpenCreate(pFileName : PWideChar;
                         pKey : Pointer;
                         wKeyLen : WORD16);

    // closes a cryptfile
    // exception:
    // ECryptFileGeneralError - file couldn't be closed
    // ECryptFileIOError      - flush error, file damaged and should be erased
    // ECryptFileFatal        - on internal conflicts
    // ECryptFileNotOpened    - no cryptfile is opened
    procedure Close;

    // reads a number of bytes to a memory area
    // -> pointer to target buffer
    // -> number of bytes to read
    // <- number of bytes read
    // exception:
    // ECryptFileEof          - end of file reached
    // ECryptFileIOError      - i/o error occured
    // ECryptFileNotOpened    - no cryptfile is opened
    // ECryptFileModeConflict - wrong work mode
    function Read(pOut : Pointer;
                  lBytesToRead : WORD32) : WORD32;

    // writes a number of bytes to the cryptfile
    // -> pointer to the source
    // -> bytes to write
    // exception:
    // ECryptFileDiskFull - out of disk space
    // ECryptFileIOError  - i/o error occured
    // ECryptFileNotOpened - no cryptfile is opened
    // ECryptFileModeConflict - wrong work mode
    procedure Write(pIn : Pointer;
                    lBytesToWrite : WORD32);

    // resets the file to read or write from the beginning
    // exception:
    // ECryptFileIOError   - i/o error occured
    // ECryptFileNotOpened - no cryptfile is opened
    procedure Reset;

    // returns the 64bit size of a file (which was opened for reading)
    // -> True: get physical size / False: only the plaintext data size
    // <- 64bit file size
    // exception:
    // ECryptFileNotOpened - no cryptfile is opened
    function GetFileSize(blPhysicalSize : Boolean = False) : WORD64;

    // sets if the i/o operations should be cashed by Windows or not
    // -> True: cache / False: don't cache
    procedure SetNoCache(blSetNoCache : Boolean);

    // gets if the "no cache" flag
    // <- state of the "no cache" flag
    function GetNoCache : Boolean;

    // sets if the key hash or check should be processed
    // -> True: hash/check key / False: don't do anything
    procedure SetNoKeyHashCheck(blSetNoKeyHashCheck : Boolean);

    // gets if the "no key hash/check" flag
    // <- state of the "no key hash/check" flag
    function GetNoKeyHashCheck : Boolean;

  end;



// base class for the following exceptions
type
  ECryptFileError = class(Exception);

// exceptions thrown by TCryptFile
type
  ECryptFileGeneralError    = class(ECryptFileError);
  ECryptFileFatal           = class(ECryptFileError);
  ECryptFileCipherNotFound  = class(ECryptFileError);
  ECryptFileSelfTestFailed  = class(ECryptFileError);
  ECryptFileInvalidCipher   = class(ECryptFileError);
  ECryptFileOutOfMemory     = class(ECryptFileError);
  ECryptFileKeySetupFailed  = class(ECryptFileError);
  ECryptFileFileNotFound    = class(ECryptFileError);
  ECryptFileAccessDenied    = class(ECryptFileError);
  ECryptFileReadOnly        = class(ECryptFileError);
  ECryptFileDiskFull        = class(ECryptFileError);
  ECryptFileWeakKey         = class(ECryptFileError);
  ECryptFileIOError         = class(ECryptFileError);
  ECryptFileWrongCipherType = class(ECryptFileError);
  ECryptFileNoCryptFile     = class(ECryptFileError);
  ECryptFileWrongPassword   = class(ECryptFileError);
  ECryptFileVersionTooHigh  = class(ECryptFileError);
  ECryptFileFileTruncated   = class(ECryptFileError);
  ECryptFileOpenError       = class(ECryptFileError);
  ECryptFileModeConflict    = class(ECryptFileError);
  ECryptFileInternalError   = class(ECryptFileError);
  ECryptFileEof             = class(ECryptFileError);
  ECryptFileUnknown         = class(ECryptFileError);
  ECryptFileInvalidOpenMode = class(ECryptFileError);
  // additional exceptions
  ECryptFileUnknownRetCode  = class(ECryptFileError);
  ECryptFileZeroKey         = class(ECryptFileError);
  ECryptFileKeyTooLarge     = class(ECryptFileError);
  ECryptFileAlreadyOpened   = class(ECryptFileError);
  ECryptFileNotOpened       = class(ECryptFileError);



implementation
uses General, FileSupp;


//////////////////////////// TCryptFile ////////////////////////////


// error codes, we use them only internally for a centralized
// execption creation via the Eraiser() procedure
const
  CRYPTFILE_ERROR_NOERROR         = 0;
  CRYPTFILE_ERROR_ERROR           = 1;
  CRYPTFILE_ERROR_FATAL           = 2;
  CRYPTFILE_ERROR_CIPHERNOTFOUND  = 3;
  CRYPTFILE_ERROR_SELFTESTFAILED  = 4;
  CRYPTFILE_ERROR_INVALIDCIPHER   = 5;
  CRYPTFILE_ERROR_OUTOFMEMORY     = 6;
  CRYPTFILE_ERROR_KEYSETUPFAILED  = 7;
  CRYPTFILE_ERROR_FILENOTFOUND    = 8;
  CRYPTFILE_ERROR_ACCESSDENIED    = 9;
  CRYPTFILE_ERROR_READONLY        = 10;
  CRYPTFILE_ERROR_DISKFULL        = 11;
  CRYPTFILE_ERROR_WEAKKEY         = 12;
  CRYPTFILE_ERROR_IOERROR         = 13;
  CRYPTFILE_ERROR_WRONGCIPHERTYPE = 14;
  CRYPTFILE_ERROR_CRYPTFILE       = 15;
  CRYPTFILE_ERROR_NOCRYPTFILE     = 16;
  CRYPTFILE_ERROR_WRONGPASSWORD   = 17;
  CRYPTFILE_ERROR_VERSIONTOOHIGH  = 18;
  CRYPTFILE_ERROR_FILETRUNCATED   = 19;
  CRYPTFILE_ERROR_OPENERROR       = 20;
  CRYPTFILE_ERROR_MODECONFLICT    = 21;
  CRYPTFILE_ERROR_INTERNALERROR   = 22;
  CRYPTFILE_ERROR_EOF             = 23;
  CRYPTFILE_ERROR_UNKNOWN         = 24;
  CRYPTFILE_ERROR_INVALIDOPENMODE = 25;
  CRYPTFILE_ERROR_ALREADYOPENED   = 26;
  CRYPTFILE_ERROR_NOTOPENED       = 27;
  CRYPTFILE_ERROR_ZEROKEY         = 28;
  CRYPTFILE_ERROR_KEYTOOLARGE     = 29;


// open modes
const
  CRYPTFILE_OPEN_NOTOPEN = 0;
  CRYPTFILE_OPEN_READ    = 1;
  CRYPTFILE_OPEN_WRITE   = 2;



// version number, is represented in the "x.xx.xxx" format
const
  CRYPTFILE_VERSION_MAJOR = 1;
  CRYPTFILE_VERSION_MINOR = 21;
  CRYPTFILE_VERSION_BUILT = 5;


// string resources ID
const
  STRRES_ID = 'CRPTFILE';


procedure TCryptFile.Eraiser(lErrorCode : WORD32);
begin
  case lErrorCode of
    CRYPTFILE_ERROR_ERROR :
      raise ECryptFileGeneralError.Create(m_sr.Get(STRRES_ID, '000'));
    CRYPTFILE_ERROR_FATAL :
      raise ECryptFileFatal.Create(m_sr.Get(STRRES_ID, '001'));
    CRYPTFILE_ERROR_CIPHERNOTFOUND :
      raise ECryptFileCipherNotFound.Create(m_sr.Get(STRRES_ID, '002'));
    CRYPTFILE_ERROR_SELFTESTFAILED :
      raise ECryptFileSelfTestFailed.Create(m_sr.Get(STRRES_ID, '003'));
    CRYPTFILE_ERROR_INVALIDCIPHER :
      raise ECryptFileInvalidCipher.Create(m_sr.Get(STRRES_ID, '004'));
    CRYPTFILE_ERROR_OUTOFMEMORY :
      raise ECryptFileOutOfMemory.Create(m_sr.Get(STRRES_ID, '005'));
    CRYPTFILE_ERROR_KEYSETUPFAILED :
      raise ECryptFileKeySetupFailed.Create(m_sr.Get(STRRES_ID, '006'));
    CRYPTFILE_ERROR_FILENOTFOUND :
      raise ECryptFileFileNotFound.Create(m_sr.Get(STRRES_ID, '007'));
    CRYPTFILE_ERROR_ACCESSDENIED :
      raise ECryptFileAccessDenied.Create(m_sr.Get(STRRES_ID, '008'));
    CRYPTFILE_ERROR_READONLY :
      raise ECryptFileReadOnly.Create(m_sr.Get(STRRES_ID, '009'));
    CRYPTFILE_ERROR_DISKFULL :
      raise ECryptFileDiskFull.Create(m_sr.Get(STRRES_ID, '010'));
    CRYPTFILE_ERROR_WEAKKEY :
      raise ECryptFileWeakKey.Create(m_sr.Get(STRRES_ID, '011'));
    CRYPTFILE_ERROR_IOERROR :
      raise ECryptFileIOError.Create(m_sr.Get(STRRES_ID, '012'));
    CRYPTFILE_ERROR_WRONGCIPHERTYPE :
      raise ECryptFileWrongCipherType.Create(m_sr.Get(STRRES_ID, '013'));
    CRYPTFILE_ERROR_NOCRYPTFILE :
      raise ECryptFileNoCryptFile.Create(m_sr.Get(STRRES_ID, '014'));
    CRYPTFILE_ERROR_WRONGPASSWORD :
      raise ECryptFileWrongPassword.Create(m_sr.Get(STRRES_ID, '015'));
    CRYPTFILE_ERROR_VERSIONTOOHIGH :
      raise ECryptFileVersionTooHigh.Create(m_sr.Get(STRRES_ID, '016'));
    CRYPTFILE_ERROR_FILETRUNCATED :
      raise ECryptFileFileTruncated.Create(m_sr.Get(STRRES_ID, '017'));
    CRYPTFILE_ERROR_OPENERROR :
      raise ECryptFileOpenError.Create(m_sr.Get(STRRES_ID, '018'));
    CRYPTFILE_ERROR_MODECONFLICT :
      raise ECryptFileModeConflict.Create(m_sr.Get(STRRES_ID, '019'));
    CRYPTFILE_ERROR_INTERNALERROR :
      raise ECryptFileInternalError.Create(m_sr.Get(STRRES_ID, '020'));
    CRYPTFILE_ERROR_EOF :
      raise ECryptFileEof.Create(m_sr.Get(STRRES_ID, '021'));
    CRYPTFILE_ERROR_UNKNOWN :
      raise ECryptFileUnknown.Create(m_sr.Get(STRRES_ID, '022'));
    CRYPTFILE_ERROR_INVALIDOPENMODE :
      raise ECryptFileInvalidOpenMode.Create(m_sr.Get(STRRES_ID, '023'));
    CRYPTFILE_ERROR_ALREADYOPENED :
      raise ECryptFileAlreadyOpened.Create(m_sr.Get(STRRES_ID, '025'));
    CRYPTFILE_ERROR_ZEROKEY :
      raise ECryptFileZeroKey.Create(m_sr.Get(STRRES_ID, '026'));
    CRYPTFILE_ERROR_KEYTOOLARGE :
      raise ECryptFileKeyTooLarge.Create(m_sr.Get(STRRES_ID, '027'));
    CRYPTFILE_ERROR_NOTOPENED :
      raise ECryptFileNotOpened.Create(m_sr.Get(STRRES_ID, '028'));
  else
    // unknown error code was returned
    raise ECryptFileUnknownRetCode.Create(m_sr.Get(STRRES_ID, '024') +
                                          IntToStr(lErrorCode));
  end;
end;


constructor TCryptFile.Create(const sCipherName : String;
                              sr : TStrRes;
                              rs : TRandomSource;
                              pRandSeed : Pointer;
                              lRandSeedLen : WORD32);
begin
  m_cipher:=Nil;
  m_cinfo:=Nil;

  m_sr:=sr;

  try
    // load the cipher
    m_cipher:=TCipher.Create(sCipherName,
                             rs,
                             pRandSeed,
                             lRandSeedLen);

    // execute the (extended) selftest
    m_cipher.ExecuteSelfTest(True);

    // get the cipher information
    m_cinfo:=m_cipher.GetInfoBlock;

  except
    on ece : ECipherError do begin

      // release the cipher, i.n.
      if (m_cipher <> Nil) then begin
        try
          m_cipher.Destroy;
        except
          on ECipherError do
            Eraiser(CRYPTFILE_ERROR_FATAL);
        end;
      end;

      // throw the correct error
      case ece.GetErrorCode of
        ECDE_OUTOFMEMORY        : Eraiser(CRYPTFILE_ERROR_OUTOFMEMORY);
        ECDE_ERROR              : Eraiser(CRYPTFILE_ERROR_FATAL);
        ECDE_CIPHERNOTFOUND     : Eraiser(CRYPTFILE_ERROR_CIPHERNOTFOUND);
        ECDE_INVALIDCIPHER      : Eraiser(CRYPTFILE_ERROR_INVALIDCIPHER);
        ECDE_SELFTESTFAILED     : Eraiser(CRYPTFILE_ERROR_SELFTESTFAILED);
      else
        // (this should never happen)
        Eraiser(CRYPTFILE_ERROR_UNKNOWN);
      end;

    end;
    on EOutOfMemory do begin

      // free the cipher information, i.n.
      if (m_cinfo <> Nil) then
        m_cinfo.Destroy;

      // unload the cipher, i.n.
      if (m_cipher <> Nil) then begin
        try
          m_cipher.Destroy;
        except
          on ECipherError do
            Eraiser(CRYPTFILE_ERROR_FATAL);
        end;
      end;
      Eraiser(CRYPTFILE_ERROR_OUTOFMEMORY);
    end;
  end;

  // cache and key hashing/checking enabled for default
  m_blNoCache:=False;
  m_blNoKeyHashCheck:=False;

  // nothing opened until now
  m_nOpenMode:=CRYPTFILE_OPEN_NOTOPEN;
end;


destructor TCryptFile.Destroy;
begin

  // free the cipher information
  if (m_cinfo <> Nil) then
    m_cinfo.Destroy;

  // release the cipher
  try
    if (m_cipher <> Nil) then
      m_cipher.Destroy;
  except
    on ECipherError do
      Eraiser(CRYPTFILE_ERROR_FATAL);
  end;
end;


function TCryptFile.ReadData(pWhereTo : Pointer;
                             lHowMany : WORD32;
                             hFile : THandle;
                             var vlErrorCode : WORD32) : Boolean;
var
  lBytesRead : WORD32;
begin
  // assume an error
  Result:=False;
  if (ReadFile(hFile,
               pWhereTo^,
               lHowMany,
               lBytesRead,
               NULL) = FALSE) then
    vlErrorCode:=CRYPTFILE_ERROR_IOERROR
  else
    if (lBytesRead <> lHowMany) then
      vlErrorCode:=CRYPTFILE_ERROR_FILETRUNCATED
    else
      Result:=True;
end;



procedure TCryptFile.OpenRead(pFileName : PWideChar;
                              pKey : Pointer;
                              wKeyLen : WORD16);
var
  blWasError       : Boolean;
  lAccessMask      : WORD32;
  lError           : WORD32;
  lSizeLo, lSizeHi : WORD32;
  sASCIIFileName   : String;
  fullkey          : TKeyMemory;
  header           : TCRYPTFILEHEADER;
  kc               : TKeyCreator;
  khs              : TKeyHashSimple;

// close the file and throw error in one act
procedure CloseError(lErrCode : WORD32);
begin
  CloseHandle(m_hFile);
  Eraiser(lErrCode);
end;

begin
  // already opened a cryptfile?
  if (m_nOpenMode <> CRYPTFILE_OPEN_NOTOPEN) then
    Eraiser(CRYPTFILE_ERROR_ALREADYOPENED);

  // check the key size
  if (wKeyLen = 0) then
    Eraiser(CRYPTFILE_ERROR_ZEROKEY);
  if (wKeyLen > CRYPTFILE_MAXKEYSIZE) then
    Eraiser(CRYPTFILE_ERROR_KEYTOOLARGE);

  // direct access?
  if (m_blNoCache) then
    lAccessMask:=FILE_FLAG_WRITE_THROUGH
  else
    lAccessMask:=0;

  // open the file directly via the Win32 API
  if (TStrPlus.IsUnicodeOS) then
    m_hFile:=CreateFileW(pFileName,
                         GENERIC_READ,
                         FILE_SHARE_READ,
                         Nil,
                         OPEN_EXISTING,
                         FILE_FLAG_SEQUENTIAL_SCAN or lAccessMask,
                         0)
  else begin
    sASCIIFileName:=WideCharToString(pFileName);
    m_hFile:=CreateFileA(PChar(sASCIIFileName),
                         GENERIC_READ,
                         FILE_SHARE_READ,
                         Nil,
                         OPEN_EXISTING,
                         FILE_FLAG_SEQUENTIAL_SCAN or lAccessMask,
                         0);
  end;

  // error occured?
  if (m_hFile = INVALID_HANDLE_VALUE) then begin

    // yes, try to detect what happened
    case GetLastError of
      ERROR_SHARING_VIOLATION :
         Eraiser(CRYPTFILE_ERROR_ACCESSDENIED);
      ERROR_ACCESS_DENIED :
        Eraiser(CRYPTFILE_ERROR_ACCESSDENIED);
      ERROR_FILE_NOT_FOUND :
        Eraiser(CRYPTFILE_ERROR_FILENOTFOUND);
    else
      // otherwise return a general open error
      Eraiser(CRYPTFILE_ERROR_OPENERROR);
    end;
  end;

  // get the physical file size
  lSizeLo:=Windows.GetFileSize(m_hFile, @lSizeHi);
  m_qRealFileSize:=MakeWORD64(lSizeLo, lSizeHi);

  // try to read out the header
  blWasError:=True;
  if (not ReadData(@header,
                   SizeOf(header),
                   m_hFile,
                   lError)) then begin
    if (lError = CRYPTFILE_ERROR_FILETRUNCATED) then
      lError:=CRYPTFILE_ERROR_NOCRYPTFILE;
  end
  else
    // cryptfile?
    if (header.lMagic <> CRYPTFILE_MAGIC) then
      lError:=CRYPTFILE_ERROR_NOCRYPTFILE
    else
      // can we handle it (only the major number counts)?
      if ((header.wVersion shr 8) > CRYPTFILE_VERSION_MAJOR) then
        lError:=CRYPTFILE_ERROR_VERSIONTOOHIGH
      else
        blWasError:=False;

  // error occured above?
  if (blWasError) then
    CloseError(lError);

  // do the key check, i.n.
  if (not m_blNoKeyHashCheck) then begin
    khs:=TKeyHashSimple.Create(@header.salt,
                               CRYPTFILE_SALTSIZE,
                               pKey,
                               wKeyLen);
    if (khs.Make32 <> header.lKeyHash) then begin
      khs.Destroy;
      CloseError(CRYPTFILE_ERROR_WRONGPASSWORD);
    end;
    khs.Destroy;
  end;

  // cipher compatible to the one used for the cryptfile?
  if ((header.wCipherBlockSize <> m_cinfo.GetBlockSize) or
      (header.wCipherInitDataSize <> m_cinfo.GetInitDataSize)) then
    CloseError(CRYPTFILE_ERROR_WRONGCIPHERTYPE);

  // perhaps we have to ignore some additional header data created by
  // future versions
  if (SizeOf(TCRYPTFILEHEADER) < header.wSizeOfHeader) then
    if (not ReadData(@m_iobuffer,
                      header.wSizeOfHeader - SizeOf(TCRYPTFILEHEADER),
                      m_hFile,
                      lError)) then
       CloseError(lError);

  // store the header size for the case of a reset
  m_wSaveHeaderSize:=header.wSizeOfHeader;

  // read out the initialisation data into the i/o buffer
  if (not ReadData(@m_iobuffer,
                   header.wCipherInitDataSize,
                   m_hFile,
                   lError)) then
    CloseError(lError);

  // hash the password down for the cipher, i.n.
  kc:=Nil;
  fullkey:=Nil; // (just to please the compiler)
  try
    if (m_cinfo.IsOwnHasher) then begin
      // build the complete key (salt and password)
      fullkey:=TKeyMemory.Create(CRYPTFILE_SALTSIZE + wKeyLen);
      fullkey.SetData(@header.salt,
                      0,
                      CRYPTFILE_SALTSIZE);
      fullkey.SetData(pKey,
                      CRYPTFILE_SALTSIZE,
                      wKeyLen);
    end
    else begin
      kc:=TKeyCreator.Create(@header.salt,
                             CRYPTFILE_SALTSIZE,
                             pKey,
                             wKeyLen);
      fullkey:=kc.MakeKey(m_cinfo.GetKeySize);
      kc.Destroy;
    end;
  except
    on EOutOfMemory do begin
      if (kc <> Nil) then
         kc.Destroy;
      CloseError(CRYPTFILE_ERROR_OUTOFMEMORY);
    end;
  end;

  // start a new decryption session
  try
    m_cipher.OpenDecryptionSession(fullkey.GetPtr,
                                   fullkey.GetSize,
                                   @m_iobuffer);
  except
    on ece : ECipherError do begin
      // weak keys don't matter for decryption
      if (ece.GetErrorCode <> ECDE_WEAKKEY) then begin
        case ece.GetErrorCode of
          ECDE_ERROR         : lError:=CRYPTFILE_ERROR_KEYSETUPFAILED;
          ECDE_OUTOFMEMORY   : lError:=CRYPTFILE_ERROR_OUTOFMEMORY;
          ECDE_SESSIONACTIVE : lError:=CRYPTFILE_ERROR_FATAL;
        else
          lError:=CRYPTFILE_ERROR_UNKNOWN;
        end;
        fullkey.Destroy;
        CloseError(lError);
      end;
    end;
  end;
  fullkey.Destroy;

  // set the work mode
  m_nOpenMode:=CRYPTFILE_OPEN_READ;

  // nothing in the buffer until now
  m_lBytesInBuf:=0;
  m_lBufPos:=0;

  // calc. the file size
  m_qFileSize:=MakeWORD64(header.lLengthLo,
                          header.lLengthHi);

  // keep the file name
  m_sFilePath:=pFileName;

  // init. the read counter
  m_qBytesDone:=0;
end;



function TCryptFile.WriteData(pFromWhere : Pointer;
                              lHowMany : WORD32;
                              hFile : THandle;
                              var vlErrorCode : WORD32) : Boolean;
var
  lWritten : WORD32;
begin
  // assume an error
  Result:=False;
  if (WriteFile(hFile,
                pFromWhere^,
                lHowMany,
                lWritten,
                NULL) = FALSE) then
    // disk full error?
    if (GetLastError = ERROR_DISK_FULL) then
      vlErrorCode:=CRYPTFILE_ERROR_DISKFULL
    else
      vlErrorCode:=CRYPTFILE_ERROR_IOERROR
  else
    // to be sure check the output again
    if (lWritten <> lHowMany) then
      vlErrorCode:=CRYPTFILE_ERROR_DISKFULL
    else
      Result:=True;
end;




procedure TCryptFile.OpenCreate(pFileName : PWideChar;
                                pKey : Pointer;
                                wKeyLen : WORD16);

var
  blWasError     : Boolean;
  lAccessMask    : WORD32;
  lError         : WORD32;
  sASCIIFileName : String;
  fullkey        : TKeyMemory;
  header         : TCRYPTFILEHEADER;
  kc             : TKeyCreator;
  khs            : TKeyHashSimple;

// close the file and throw error in one act
procedure CloseError(lErrCode : WORD32);
begin
  CloseHandle(m_hFile);
  Eraiser(lErrCode);
end;

begin

  // already opened a cryptfile?
  if (m_nOpenMode <> CRYPTFILE_OPEN_NOTOPEN) then
    Eraiser(CRYPTFILE_ERROR_ALREADYOPENED);

  // check the key size
  if (wKeyLen = 0) then
    Eraiser(CRYPTFILE_ERROR_ZEROKEY);
  if (wKeyLen > CRYPTFILE_MAXKEYSIZE) then
    Eraiser(CRYPTFILE_ERROR_KEYTOOLARGE);

  // create the salt, store it for the close procedure
  m_cipher.GetRandomData(@m_savesalt,
                         CRYPTFILE_SALTSIZE);

  // create the key hash, store it for the close procedure
  if (not m_blNoKeyHashCheck) then begin
    khs:=TKeyHashSimple.Create(@m_savesalt,
                               CRYPTFILE_SALTSIZE,
                               pKey,
                               wKeyLen);
    m_lSaveKeyHash:=khs.Make32;
    khs.Destroy;
  end
  else begin
    // write a dummy key hash
    m_cipher.GetRandomData(@m_lSaveKeyHash,
                           SizeOf(m_lSaveKeyHash));
  end;

  // hash the password down for the cipher (if necessary)
  kc:=Nil;
  fullkey:=Nil; // (just to please the compiler)
  try
    if (m_cinfo.IsOwnHasher) then begin
      fullkey:=TKeyMemory.Create(CRYPTFILE_SALTSIZE + wKeyLen);
      fullkey.SetData(@m_savesalt,
                      0,
                      CRYPTFILE_SALTSIZE);
      fullkey.SetData(pKey,
                      CRYPTFILE_SALTSIZE,
                      wKeyLen);
    end
    else begin
      kc:=TKeyCreator.Create(@m_savesalt,
                             CRYPTFILE_SALTSIZE,
                             pKey,
                             wKeyLen);
      fullkey:=kc.MakeKey(m_cinfo.GetKeySize);
      kc.Destroy;
    end;
  except
    on EOutOfMemory do begin
       if (kc <> Nil) then
         kc.Destroy;
       Eraiser(CRYPTFILE_ERROR_OUTOFMEMORY);
    end;
  end;

  // start a new encryption session
  try
    m_cipher.OpenEncryptionSession(fullkey.GetPtr,
                                   fullkey.GetSize,
                                   @m_iobuffer);
  except
    on ece : ECipherError do begin
      case ece.GetErrorCode of
        ECDE_ERROR         : lError:=CRYPTFILE_ERROR_KEYSETUPFAILED;
        ECDE_OUTOFMEMORY   : lError:=CRYPTFILE_ERROR_OUTOFMEMORY;
        ECDE_WEAKKEY       : lError:=CRYPTFILE_ERROR_WEAKKEY;
        ECDE_SESSIONACTIVE : lError:=CRYPTFILE_ERROR_FATAL;
      else
        lError:=CRYPTFILE_ERROR_UNKNOWN;
      end;
      fullkey.Destroy;
      Eraiser(lError);
    end;
  end;
  fullkey.Destroy;

  // direct access?
  if (m_blNoCache) then
    lAccessMask:=FILE_FLAG_WRITE_THROUGH
  else
    lAccessMask:=0;

  // create the cryptfile directly via the Win32 API
  if (TStrPlus.IsUnicodeOS) then
    m_hFile:=CreateFileW(pFileName,
                         GENERIC_WRITE,
                         0,
                         Nil,
                         CREATE_ALWAYS,
                         lAccessMask,
                         0)
  else begin
    sASCIIFileName:=WideCharToString(pFileName);
    m_hFile:=CreateFileA(PChar(sASCIIFileName),
                         GENERIC_WRITE,
                         0,
                         Nil,
                         CREATE_ALWAYS,
                         lAccessMask,
                         0);
  end;

  // open error?
  if (m_hFile = INVALID_HANDLE_VALUE) then begin
    case GetLastError of
      ERROR_SHARING_VIOLATION : lError:=CRYPTFILE_ERROR_ACCESSDENIED;
      ERROR_ACCESS_DENIED     : lError:=CRYPTFILE_ERROR_ACCESSDENIED;
      ERROR_HANDLE_DISK_FULL  : lError:=CRYPTFILE_ERROR_DISKFULL;
      ERROR_WRITE_PROTECT     : lError:=CRYPTFILE_ERROR_READONLY;
    else
      // otherwise return a general open error
      lError:=CRYPTFILE_ERROR_OPENERROR;
    end;
    try
      m_cipher.CloseSession;
    except
      on ECipherError do
        lError:=CRYPTFILE_ERROR_FATAL;
    end;
    Eraiser(lError);
  end;

  // put out an empty header (will be filled at the end of the session)
  FillChar(header,
           SizeOf(header),
           0);
  // (errors from here are handled one block below)
  blWasError:=not WriteData(@header,
                            SizeOf(header),
                            m_hFile,
                            lError);

  // write the init. data, which was received in the i/o buffer
  if (not blWasError) then
    blWasError:=not WriteData(@m_ioBuffer,
                              m_cinfo.GetInitDataSize,
                              m_hFile,
                              lError);
  if blWasError then begin
    try
      m_cipher.CloseSession;
    except
      on ECipherError do
         lError:=CRYPTFILE_ERROR_FATAL;
    end;
    CloseError(lError);
  end;

  // store the header size for the case of a reset
  m_wSaveHeaderSize:=SizeOf(header);

  // store the file name
  m_sFilePath:=pFileName;

  // set the work mode
  m_nOpenMode:=CRYPTFILE_OPEN_WRITE;

  // nothing in the buffer until now
  m_lBytesInBuf:=0;

  // init. the 'bytes written' counter
  m_qBytesDone:=0;
end;



procedure TCryptFile.Close;
var
  lBlocksToFlush : WORD32;
  lBytesToFlush  : WORD32;
  lBlockSize     : WORD32;
  lTemp          : WORD32;
  header         : TCryptFileHeader;

// close the file and throw error in one act
procedure CloseError(lErrCode : WORD32);
begin
  CloseHandle(m_hFile);
  Eraiser(lErrCode);
end;

begin

  // nothing opened?
  if (m_nOpenMode = CRYPTFILE_OPEN_NOTOPEN) then
    Eraiser(CRYPTFILE_ERROR_NOTOPENED);

  // flush the i/o buffer, i.n.
  if ((m_nOpenMode = CRYPTFILE_OPEN_WRITE) and (m_lBytesInBuf > 0)) then begin

    // pad with zero bytes
    lBlockSize:=m_cinfo.GetBlockSize;
    if ((m_lBytesInBuf mod lBlockSize) > 0) then begin
      lBlocksToFlush:=m_lBytesInBuf div lBlockSize + 1;
      lBytesToFlush:=lBlocksToFlush * lBlockSize;
      FillChar(m_iobuffer[m_lBytesInBuf],
               lBytesToFlush - m_lBytesInBuf,
               0);
    end
    else begin
      lBytesToFlush:=m_lBytesInBuf;
      lBlocksToFlush:=lBytesToFlush div lBlockSize;
    end;
    Inc(m_qBytesDone, m_lBytesInBuf);

    // encrypt the rest of the data
    try
      m_cipher.EncryptBlocks(@m_iobuffer,
                           @m_iobuffer,
                           lBlocksToFlush);
    except
      on ece : ECipherError do
        // (this should never happen)
        CloseError(CRYPTFILE_ERROR_FATAL);
    end;

    // write the data to disk
    if (not WriteData(@m_iobuffer,
                      lBytesToFlush,
                      m_hFile,
                      lTemp)) then
      // unrecoverable error occured
      CloseError(CRYPTFILE_ERROR_IOERROR);
  end;

  // close the session
  try
    m_cipher.CloseSession;
  except
    on ECipherError do
      // (this should never happen)
      CloseError(CRYPTFILE_ERROR_FATAL);
  end;

  // write the header, i.n.
  if (m_nOpenMode = CRYPTFILE_OPEN_WRITE) then begin

    // create the header structure
    with header, m_cinfo do begin
      lMagic:=CRYPTFILE_MAGIC;
      wVersion:=(CRYPTFILE_VERSION_MAJOR shl 8) or CRYPTFILE_VERSION_MINOR;
      wSizeOfHeader:=SizeOf(TCRYPTFILEHEADER);
      lLengthLo:=GetWORD64Lo(m_qBytesDone);
      lLengthHi:=GetWORD64Hi(m_qBytesDone);
      wCipherInitDataSize:=GetInitDataSize;
      wCipherBlockSize:=GetBlockSize;
      Move(m_savesalt, salt, SizeOf(salt));
      lKeyHash:=m_lSaveKeyHash;
    end;

    // seek back to the begin of the file
    lTemp:=0;
    if (SetFilePointer(m_hFile,
                       0,
                       @lTemp,
                       FILE_BEGIN) <> 0) then
      // seek action failed
      CloseError(CRYPTFILE_ERROR_IOERROR);

    // put out the header
    if (not WriteData(@header,
                      SizeOf(header),
                      m_hFile,
                      lTemp)) then
      CloseError(CRYPTFILE_ERROR_IOERROR);
  end;

  // flush all buffers
  FlushFileBuffers(m_hFile);

  // close the cryptfile
  if (CloseHandle(m_hFile) = FALSE) then
    Eraiser(CRYPTFILE_ERROR_FATAL);

  // nothing opened anymore
  m_nOpenMode:=CRYPTFILE_OPEN_NOTOPEN;
end;



function TCryptFile.Read(pOut : Pointer;
                         lBytesToRead : WORD32) : WORD32;
var
  lError          : WORD32;
  lI              : WORD32;
  lBlockSize      : WORD32;
  lRest           : WORD32;
  lRestToRead     : WORD32;
  lBytesPerCycle  : WORD32;
  lBlocksPerCycle : WORD32;
  lCycles         : WORD32;
  pBuffer         : PWORD8Buf;
  qTemp           : WORD64;
begin

  // correct open mode?
  case m_nOpenMode of
    CRYPTFILE_OPEN_NOTOPEN : Eraiser(CRYPTFILE_ERROR_NOTOPENED);
    CRYPTFILE_OPEN_WRITE   : Eraiser(CRYPTFILE_ERROR_MODECONFLICT);
  end;

  // nothing read until now
  Result:=0;

  // nothing to read?
  if (lBytesToRead = 0) then
    Exit;

  // end of file already reached?
  if (m_qBytesDone >= m_qFileSize) then
    Eraiser(CRYPTFILE_ERROR_EOF);

  // do we have enough bytes in the file to fulfill the request?
  qTemp:=m_qBytesDone + lBytesToRead;
  if (qTemp > m_qFileSize) then begin
    // we can't deliver more bytes
    qTemp:=m_qFileSize - m_qBytesDone;
    lBytesToRead:=GetWORD64Lo(qTemp);
  end;

  // treat the target as a byte buffer
  pBuffer:=pOut;

  // is there something left in the buffer?
  if (m_lBytesInBuf > 0) then begin

    // enough bytes to avoid a physical i/o operation?
    if (m_lBytesInBuf >= lBytesToRead) then begin
      // yes, copy the demanded bytes
      Move(m_iobuffer[m_lBufPos],
           pBuffer^[0],
           lBytesToRead);
      Dec(m_lBytesInBuf, lBytesToRead);
      Inc(m_lBufPos, lBytesToRead);
      Result:=lBytesToRead;
      Inc(m_qBytesDone, lBytesToRead);
      Exit;
    end
    else begin
      // no, empty the buffer and go on
      Move(m_iobuffer[m_lBufPos],
           pBuffer^[0],
           m_lBytesInBuf);
      Result:=m_lBytesInBuf;
      Dec(lBytesToRead, m_lBytesInBuf);
    end;
  end;

  // how many bytes can be read into the buffer at once?
  lBlockSize:=m_cinfo.GetBlockSize;
  lBlocksPerCycle:=CRYPTFILE_IOBUFSIZE div lBlockSize;
  lBytesPerCycle:=lBlocksPerCycle * lBlockSize;

  // how many main cycles do we have to process?
  lCycles:=lBytesToRead div lBytesPerCycle;

  // start the decryption
  for lI:=1 to lCycles do begin
    // read a buffer content from disk
    if (not ReadData(@m_iobuffer,
                     lBytesPerCycle,
                     m_hFile,
                     lError)) then
      // unrecoverable error occured
      Eraiser(lError);

    // decrypt the buffer to the target
    m_cipher.DecryptBlocks(@m_iobuffer,
                         @pBuffer^[Result],
                         lBlocksPerCycle);
    Inc(Result, lBytesPerCycle);
  end;

  // how many rest bytes do we have to read out?
  lRest:=lBytesToRead mod lBytesPerCycle;

  // how much is this aligned to the next block border?
  lRestToRead:=lRest div lBlockSize;
  if (lRest mod lBlockSize > 0) then
    Inc(lRestToRead);
  lRestToRead:=lRestToRead * lBlockSize;
  if (lRestToRead > 0) then begin
    // read the rest
    if (not ReadData(@m_iobuffer,
                     lRestToRead,
                     m_hFile,
                     lError)) then
      Eraiser(lError);

    // decrypt the buffer
    m_cipher.DecryptBlocks(@m_iobuffer,
                         @m_iobuffer,
                         lRestToRead div lBlockSize);

    // copy the rest bytes
    Move(m_iobuffer,
         pBuffer^[Result],
         lRest);
    Inc(Result, lRest);

    // reposition and leave the rest in the i/o buffer
    m_lBytesInBuf:=lRestToRead - lRest;
    m_lBufPos:=lRest;
  end
  else begin
    // nothing in the buffer now
    m_lBytesInBuf:=0;
    m_lBufPos:=0;
  end;

  // done
  Inc(m_qBytesDone, Result);
end;


procedure TCryptFile.Write(pIn : Pointer;
                           lBytesToWrite : WORD32);
var
  lI              : WORD32;
  lSaveAmount     : WORD32;
  lRest           : WORD32;
  lBlockSize      : WORD32;
  lBlocksPerCycle : WORD32;
  lBytesPerCycle  : WORD32;
  lBytesToCopy    : WORD32;
  lCycles         : WORD32;
  lSrcPos         : WORD32;
  lError          : WORD32;
  pBuffer         : PWORD8Buf;
begin

  // correct open mode?
  case m_nOpenMode of
    CRYPTFILE_OPEN_NOTOPEN : Eraiser(CRYPTFILE_ERROR_NOTOPENED);
    CRYPTFILE_OPEN_READ    : Eraiser(CRYPTFILE_ERROR_MODECONFLICT);
  end;

  // nothing to write?
  if (lBytesToWrite = 0) then
    Exit;

  // how many bytes can be written from the buffer at once?
  lBlockSize:=m_cinfo.GetBlockSize;
  lBlocksPerCycle:=CRYPTFILE_IOBUFSIZE div lBlockSize;
  lBytesPerCycle:=lBlocksPerCycle * lBlockSize;

  // how many cycles do we have to proceed?
  lSaveAmount:=lBytesToWrite;
  Inc(lBytesToWrite, m_lBytesInBuf);
  lCycles:=lBytesToWrite div lBytesPerCycle;

  // start the encryption
  pBuffer:=pIn;
  lSrcPos:=0;
  for lI:=1 to lCycles do begin
    // fill the buffer
    lBytesToCopy:=lBytesPerCycle - m_lBytesInBuf;
    Move(pBuffer^[lSrcPos],
         m_iobuffer[m_lBytesInBuf],
         lBytesToCopy);

    // encrypt the buffer (we assume that we have done everything right
    // to adjust the buffer to the right block alignment)
    m_cipher.EncryptBlocks(@m_iobuffer,
                         @m_iobuffer,
                         lBlocksPerCycle);

    // write the buffer to disk
    if (not WriteData(@m_iobuffer,
                      lBytesPerCycle,
                      m_hFile,
                      lError)) then
      // unrecoverable error occured
      Eraiser(lError);
    Inc(lSrcPos,
        lBytesToCopy);

    // nothing in the buffer now
    m_lBytesInBuf:=0;
    Inc(m_qBytesDone, lBytesPerCycle);
  end;

  // copy the rest (or all) bytes to the buffer
  lRest:=lSaveAmount - lSrcPos;
  if (lRest > 0) then begin
    Move(pBuffer^[lSrcPos],
         m_iobuffer[m_lBytesInBuf],
         lRest);
    Inc(m_lBytesInBuf, lRest);
  end;
end;


procedure TCryptFile.Reset;
var
  lError  : WORD32;
  dwDummy : DWORD;
begin

  case m_nOpenMode of
    CRYPTFILE_OPEN_NOTOPEN :
      Eraiser(CRYPTFILE_ERROR_NOTOPENED);

    CRYPTFILE_OPEN_READ : begin

      // seek back right after the header
      dwDummy:=0;
      if (SetFilePointer(m_hFile,
                         DWORD(m_wSaveHeaderSize),
                         @dwDummy,
                         FILE_BEGIN) = $ffffffff) then
        if (GetLastError <> NO_ERROR) then
          Eraiser(CRYPTFILE_ERROR_IOERROR);

      // read out the init. data again
      if (not ReadData(@m_iobuffer,
                       m_cinfo.GetInitDataSize,
                       m_hFile,
                       lError)) then
        Eraiser(lError);

      // reset the decryption session
      try
        m_cipher.ResetSession(@m_iobuffer);
      except
        on ece : ECipherError do
          Eraiser(CRYPTFILE_ERROR_INTERNALERROR);
      end;

      // here we go again
      m_lBytesInBuf:=0;
      m_lBufPos:=0;
      m_qBytesDone:=0;
      Exit;
    end;

    CRYPTFILE_OPEN_WRITE : begin

      // cut off everything we put out right after the dummy header
      dwDummy:=0;
      if (SetFilePointer(m_hFile,
                         DWORD(m_wSaveHeaderSize),
                         @dwDummy,
                         FILE_BEGIN) = $ffffffff) then begin
        if (GetLastError <> NO_ERROR) then
          Eraiser(CRYPTFILE_ERROR_IOERROR);
      end;
      if (SetEndOfFile(m_hFile) = FALSE) then
        Eraiser(CRYPTFILE_ERROR_IOERROR);

      // reset the decryption session, get new init. data
      try
        m_cipher.ResetSession(@m_iobuffer);
      except
        on ece : ECipherError do
          Eraiser(CRYPTFILE_ERROR_INTERNALERROR);
      end;

      // put out that new init. data
      if (not WriteData(@m_iobuffer,
                        m_cinfo.GetInitDataSize,
                        m_hFile,
                        lError)) then
        Eraiser(lError);

      // ready for output again
      m_lBytesInBuf:=0;
      m_qBytesDone:=0;
    end;
  end; (* of CASE *)
end;


function TCryptFile.GetFileSize(blPhysicalSize : Boolean = False) : WORD64;
begin

  // nothing opened?
  if (m_nOpenMode = CRYPTFILE_OPEN_NOTOPEN) then
    Eraiser(CRYPTFILE_ERROR_NOTOPENED);

  // just return the file size
  if (blPhysicalSize) then
    Result:=m_qRealFileSize
  else
    Result:=m_qFileSize;
end;


procedure TCryptFile.SetNoCache(blSetNoCache : Boolean);
begin
  m_blNoCache:=blSetNoCache;
end;


function TCryptFile.GetNoCache : Boolean;
begin
  Result:=m_blNoCache;
end;


procedure TCryptFile.SetNoKeyHashCheck(blSetNoKeyHashCheck : Boolean);
begin
  m_blNoKeyHashCheck:=blSetNoKeyHashCheck;
end;


function TCryptFile.GetNoKeyHashCheck : Boolean;
begin
  Result:=m_blNoKeyHashCheck;
end;


end.

