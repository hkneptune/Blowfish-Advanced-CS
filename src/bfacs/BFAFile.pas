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
  classes for handling .BFA files
}

unit BFAFile;

{$I config.inc}

interface
uses Windows,
     SysUtils,
     Classes,
     bfacslib,
     CrptFile,
     SecureMem,
     ProgressCallBack,
     Wipe,
     RandomSource,
     StringRes;


// progress states (bitmask)
const
  BFAFILE_PROGRESS_ENCRYPT = 0;
  BFAFILE_PROGRESS_DECRYPT = 1;
  BFAFILE_PROGRESS_WIPE    = 2;


// compression codes
const
  BFAFILE_COMPRESS_NONE = 0;
  BFAFILE_COMPRESS_LZSS = 1;
  BFAFILE_COMPRESS_DEFL = 2;
  BFAFILE_COMPRESS_BZP2 = 3;


// file name extension
const
  BFAFILE_EXTENSION = 'bfa';


// detect an ignored compression
const
  BFAFILE_COMPRESSION_IGNORED = -1;


// warning flags (bitmasked)
const
  BFAFILE_WARNING_WEAKWIPING         =  1;
  BFAFILE_WARNING_REMOVEFAILED       =  2;
  BFAFILE_WARNING_SETATTRFAILED      =  4;
  BFAFILE_WARNING_SETDATETIMEFAILED  =  8;
  BFAFILE_WARNING_INTERRUPTED        = 16;
  BFAFILE_WARNING_TRUNCATED          = 32;
  BFAFILE_WARNING_USED83RENAMING     = 64;


// exception to detect setup members which were not set
type
  EBFAFileSetupError = class(Exception);


// direct header information
type
  TBFAFileDirectHeaderInfo = class
  private
    m_sFileName    : String;
    m_qFileSize    : WORD64;
    m_fileTime     : TFileTime;
    m_lAttributes  : WORD32;
    m_nCompression : Integer;
  public
    // constructor
    // -> name string
    // -> size
    // -> date+time
    // -> attributes
    // -> compression code, see BFAFILE_COMPRESS_xxx
    constructor Create(sFileName : String;
                       qFileSize : WORD64;
                       fileTime : TFileTime;
                       lAttributes : WORD32;
                       nCompression : Integer);

    // get the file name string
    // <- name string
    function GetFileName : String;

    // get the file size
    // <- file size
    function GetFileSize : WORD64;

    // get the date+time stamp
    // <- date+time
    function GetFileTime : TFileTime;

    // get the attributes
    // <- attributes
    function GetAttributes : WORD32;

    // get the compression code
    // <- compression code
    function GetCompression : Integer;
  end;



// base setup class
type
  TBFAFileSetup = class
  protected
    // password (just a reference is stored)
    m_password : TKeyMemory;
    // the target path (empty if none, must be an absolute path!)
    m_sTargetPath : String;
    m_blTargetPathSet : Boolean;
    // remove the source file?
    m_blRemoveSource : Boolean;
    m_blRemoveSourceSet : Boolean;
    // overwrite existing file?
    m_blOverwriteExisting : Boolean;
    m_blOverwriteExistingSet : Boolean;
    // buffered i/o?
    m_blNoCache : Boolean;
    m_blNoCacheSet : Boolean;
    // keep directories (if target path is given)?
    m_blKeepDirectories    : Boolean;
    m_blKeepDirectoriesSet : Boolean;
    // base path (to enable full/part path storing), will be cut off from the
    // original file path (ignored, if empty) or be used to restore files in
    // combination with the "keep directories" flag
    m_sBasePath : String;
    m_blBasePathSet : Boolean;
  public
    // constructor
    constructor Create; virtual;
    // getters and setters
    procedure SetPassword(password : TKeyMemory);
    function GetPassword : TKeyMemory;
    procedure SetTargetPath(sTargetPath : String);
    function GetTargetPath : String;
    procedure SetRemoveSource(blRemoveSource : Boolean);
    function GetRemoveSource : Boolean;
    procedure SetOverwriteExisting(blOverwriteExisting : Boolean);
    function GetOverwriteExisting : Boolean;
    procedure SetNoCache(blNoCache : Boolean);
    function GetNoCache : Boolean;
    procedure SetKeepDirectories(blKeepDirectories : Boolean);
    function GetKeepDirectories : Boolean;
    procedure SetBasePath(sBasePath : String);
    function GetBasePath : String;
  end;


// setup class for encryption
type
  TBFAFileEncryptSetup = class(TBFAFileSetup)
  protected
    // store this file name into the header (ignored, if empty)
    m_sHeaderFileName : String;
    m_blHeaderFileNameSet : Boolean;
    // force the output to a file of this name (ignored, if empty)
    m_sForceFileName : String;
    m_blForceFileNameSet : Boolean;
    // rename the file?
    m_blRename : Boolean;
    m_blRenameSet : Boolean;
    // rename with random chars?
    m_blRandomRename : Boolean;
    m_blRandomRenameSet : Boolean;
    // rename mask if the file is not be renamed by random
    m_sMaskName : String;
    m_blMaskNameSet : Boolean;
    m_nMaskNumber : Integer;
    m_blMaskNumberSet : Boolean;
    m_sMaskExt : String;
    m_blMaskExtSet : Boolean;
    // rename the files most 8.3 compatible as possible?
    m_blTryRename83 : Boolean;
    m_blTryRename83Set : Boolean;
    // append the .bfa extension?
    m_blAddExtension : Boolean;
    m_blAddExtensionSet : Boolean;
    // keep the date+time stamps?
    m_blKeepDateTime : Boolean;
    m_blKeepDateTimeSet : Boolean;
    // keep the attribute?
    m_blKeepAttributes : Boolean;
    m_blKeepAttributesSet : Boolean;
    // store the original path?
    m_blStorePath : Boolean;
    m_blStorePathSet : Boolean;
    // store the orig. path relatively to the base path?
    m_blRelativePaths : Boolean;
    m_blRelativePathsSet : Boolean;
    // compression code
    m_nCompress : Integer;
    m_blCompressSet : Boolean;
    // compression force flag
    m_blForceCompress : Boolean;
    m_blForceCompressSet : Boolean;
    // skip if the file is already encrypted?
    m_blSkipEncrypted : Boolean;
    m_blSkipEncryptedSet : Boolean;
    // no key checksum creation?
    m_blNoKeyHash : Boolean;
    m_blNoKeyHashSet : Boolean;
    // write-protect file after encryption?
    m_blWriteProtectAfter : Boolean;
    m_blWriteProtectAfterSet : Boolean;
    // list of extensions for which the files won't be compressed (may be Nil)
    // (the reference won't be touched and must be destroyed by the caller)
    m_noCompressTypes : TStringList;
    m_blNoCompressTypesSet : Boolean;
  public
    // ctor/dtor
    constructor Create; override;
    destructor Destroy; override;
    // getters and setters
    procedure SetHeaderFileName(sHeaderFileName : String);
    function GetHeaderFileName : String;
    procedure SetForceFileName(sForceFileName : String);
    function GetForceFileName : String;
    procedure SetRename(blRename : Boolean);
    function GetRename : Boolean;
    procedure SetRandomRename(blRandomRename : Boolean);
    function GetRandomRename : Boolean;
    procedure SetMaskName(sMaskName : String);
    function GetMaskName : String;
    procedure SetMaskNumber(nMaskNumber : Integer);
    function GetMaskNumber : Integer;
    procedure SetMaskExt(sMaskExt : String);
    function GetMaskExt : String;
    procedure SetTryRename83(blTryRename83 : Boolean);
    function GetTryRename83 : Boolean;
    procedure SetAddExtension(blAddExtension : Boolean);
    function GetAddExtension : Boolean;
    procedure SetKeepDateTime(blKeepDateTime : Boolean);
    function GetKeepDateTime : Boolean;
    procedure SetKeepAttributes(blKeepAttributes : Boolean);
    function GetKeepAttributes : Boolean;
    procedure SetStorePath(blStorePath : Boolean);
    function GetStorePath : Boolean;
    procedure SetRelativePaths(blRelativePaths : Boolean);
    function GetRelativePaths : Boolean;
    procedure SetCompress(nCompress : Integer);
    function GetCompress : Integer;
    procedure SetForceCompress(blForceCompress : Boolean);
    function GetForceCompress : Boolean;
    procedure SetSkipEncrypted(blSkipEncrypted : Boolean);
    function GetSkipEncrypted : Boolean;
    procedure SetNoKeyHash(blNoKeyHash : Boolean);
    function GetNoKeyHash : Boolean;
    procedure SetWriteProtectAfter(blWriteProtectAfter : Boolean);
    function GetWriteProtectAfter : Boolean;
    procedure SetNoCompressTypes(noCompressTypes : TStringList); // (taken over)
    function GetNoCompressTypes : TStringList;
  end;


// setup class for decryption
type
  TBFAFileDecryptSetup = class(TBFAFileSetup)
  protected
    // ignore CRC32 errors?
    m_blIgnoreCRC32 : Boolean;
    m_blIgnoreCRC32Set : Boolean;
    // restore the original path?
    m_blRestorePath : Boolean;
    m_blRestorePathSet : Boolean;
    // no key checksum verify?
    m_blNoKeyCheck : Boolean;
    m_blNoKeyCheckSet : Boolean;
    // accept truncated files?
    m_blAcceptTruncated : Boolean;
    m_blAcceptTruncatedSet : Boolean;
    // retrieve file information only?
    m_blFileInfoOnly : Boolean;
    m_blFileInfoOnlySet : Boolean;
    // deliver direct header information?
    m_blDirectHeaderInfo : Boolean;
    m_blDirectHeaderInfoSet : Boolean;
  public
    // constructor
    constructor Create; override;
    // getters and setters
    procedure SetIgnoreCRC32(blIgnoreCRC32 : Boolean);
    function GetIgnoreCRC32 : Boolean;
    procedure SetRestorePath(blRestorePath : Boolean);
    function GetRestorePath : Boolean;
    procedure SetNoKeyCheck(blNoKeyCheck : Boolean);
    function GetNoKeyCheck : Boolean;
    procedure SetAcceptTruncated(blAcceptTruncated : Boolean);
    function GetAcceptTruncated : Boolean;
    procedure SetFileInfoOnly(blFileInfoOnly : Boolean);
    function GetFileInfoOnly : Boolean;
    procedure SetDirectHeaderInfo(blDirectHeaderInfo : Boolean);
    function GetDirectHeaderInfo : Boolean;
  end;


// base result class
type
  TBFAFileResult = class
  protected
    // source file name (+path)
    m_sSourceFileName : String;

    // warning bitmask (see BFAFILE_WARNING_xxx constants)
    m_nWarnings : Integer;

    // string resources
    m_sr : TStrRes;

  public
    // constructor
    // -> string resources
    constructor Create(sr : TStrRes); virtual;

    // getters and setters
    procedure SetSourceFileName(sSourceFileName : String);
    function GetSourceFileName : String;
    procedure SetWarnings(nNewWarnings : Integer);
    function GetWarnings : Integer;

    // warning rendering
    // <- the rendered warnings in a string representation
    function RenderWarnings : String;
  end;


// result class for encryption
type
  TBFAFileEncryptResult = class(TBFAFileResult)
  protected
    // size of the original file
    m_qOriginalFileSize : WORD64;
    // number of compressed bytes (maybe BFAFILE_COMPRESSION_IGNORED, if
    // the compression demand was ignored due to the source file's type)
    m_qBytesCompressed : WORD64;
    // name of the encrypted file
    m_sBFAFileName : String;
  public
    // getters and setters
    procedure SetOriginalFileSize(qOriginalFileSize : WORD64);
    function GetOriginalFileSize : WORD64;
    procedure SetBytesCompressed(qBytesCompressed : WORD64);
    function GetBytesCompressed : WORD64;
    procedure SetBFAFileName(sBFAFileName : String);
    function GetBFAFileName : String;
  end;


// result class for decryption
type
  TBFAFileDecryptResult = class(TBFAFileResult)
  protected
    // number of written bytes
    m_qBytesWritten : WORD64;
    // name of the decrypted file
    m_sOriginalFileName : String;
    // header info (may be Nil, last istance set will be destroyed)
    m_dhi : TBFAFileDirectHeaderInfo;

  public
    // constructor
    // string resources
    constructor Create(sr : TStrRes); override;

    // destructor
    destructor Destroy; override;

    // getters and setters
    procedure SetBytesWritten(qBytesWritten : WORD64);
    function GetBytesWritten : WORD64;
    procedure SetOriginalFileName(sOriginalFileName : String);
    function GetOriginalFileName : String;
    procedure SetDirectHeaderInfo(dhi : TBFAFileDirectHeaderInfo);
    function GetDirectHeaderInfo : TBFAFileDirectHeaderInfo;
  end;


// our progress callback class
type
  TBFAFileProgress = class(TProgressCallBack)
  private
    // progress state code, see BFAFILE_PROGRESS_xxx constants
    m_nProgressState : Integer;
    // name of the input file
    m_sInputFileName : String;
    // name of the output file
    m_sOutputFileName : String;
    // size of the input (encryption) or output (decryption) file
    m_qFileSize : WORD64;
    // reference to a wipe progress object
    m_wipeProgressObj : TWipeProgress;
  public
    // getters and setters
    procedure SetProgressState(nProgressState : Integer);
    function GetProgressState : Integer;
    procedure SetInputFileName(sInputFileName : String);
    function GetInputFileName : String;
    procedure SetOutputFileName(sOutputFileName : String);
    function GetOutputFileName : String;
    procedure SetFileSize(qFileSize : WORD64);
    function GetFileSize : WORD64;
    procedure SetWipeProgressObj(wipeProgressObj : TWipeProgress);
    function GetWipeProgressObj : TWipeProgress;
  end;


// the file handling class
type
  TBFAFile = class(TCryptFile)
  private
    // members
    m_pIOBuffer : Pointer;
    m_sr        : TStrRes;

  public

    // loads and tests the cipher, allocates the buffers
    // -> name of the cipher
    // -> string resources
    // -> True: use legacy format (in both layers) / False: use the new one
    // -> random source reference (may be Nil for reading only purposes)
    // -> pointer to random seed data (only used if rs is Nil, may also be Nil)
    // -> number of random seed bytes (ignored if pRandSeed is Nil)
    // exception:
    // EBFAFileFatal           - fatal error occured
    // EBFAFileCipherNotFound  - cipher not found
    // EBFAFileInvalidCipher   - invalid cipher
    // EBFAFileSelfTestFailed  - cipher selftest failed
    // EBFAFileOutOfMemory     - out of memory
    constructor Create(const sCipherName : String;
                       sr : TStrRes;
                       rs : TRandomSource = Nil;
                       pRandSeed : Pointer = Nil;
                       lRandSeedLen : WORD32 = 0); override;

    // destructor
    destructor Destroy; override;

    // encrypts an original to a BFA file
    // -> name (and path) of the file to encrypt
    // -> instance to wipe the file (may be Nil for no wiping)
    // -> description what, how and where to encrypt (and wipe)
    // -> callback instance (implemented by the caller)
    // <- result instance (must be destroyed be the caller, Nil if not demanded)
    // exception:
    // EBFAFileAccessDenied     - access denied to the file
    // EBFAFileAlreadyEncrypted - source file is already encrypted
    // EBFAFileAlreadyExists    - target file already exists
    // EBFAFileAlreadyOpened    - already opened a crypt file
    // EBFAFileFatal            - on internal conflicts
    // EBFAFileFileNotFound     - crypt file not be found
    // EBFAFileGeneralError     - file couldn't be closed
    // EBFAFileIOError          - flush error, file damaged and should be erased
    // EBFAFileInterrupted      - operation was interupted
    // EBFAFileInvalidOpenMode  - invalid open mode
    // EBFAFileKeyTooLarge      - key too large
    // EBFAFileNoCryptFile      - file is not a cryptfile
    // EBFAFileNotOpened        - no cryptfile is opened
    // EBFAFileOpenError        - general open error
    // EBFAFileOutOfMemory      - out of memory
    // EBFAFileSetupError       - setup error (internal problem)
    // EBFAFileWrongPassword    - password hash does not match
    // EBFAFileWrongCipherType  - cipher does not fit to this cryptfile
    // EBFAFileZeroKey          - zero password detected
    function Encrypt(sFileName : String;
                     wipeObj : TWipe;
                     setup : TBFAFileEncryptSetup;
                     progressCall : TBFAFileProgress) : TBFAFileEncryptResult;

    // decrypts a .BFA file to its original counterpart (or just retrieves
    // information out of its header)
    // -> name (and path) of the file to decrypt
    // -> description what, how and where to decrypt
    // -> callback instance (implemented by the caller
    // <- result instance (must be destroyed be the caller, Nil if not demanded)
    // exception:
    // EBFAFileAccessDenied    - access denied to the file
    // EBFAFileAlreadyExists   - target file already exists
    // EBFAFileAlreadyOpened   - already opened a crypt file
    // EBFAFileFileNotFound    - crypt file not found
    // EBFAFileInvalidOpenMode - invalid open mode
    // EBFAFileKeyTooLarge     - key too large
    // EBFAFileNoCryptFile     - file is not a crypt file
    // EBFAFileOpenError       - general open error
    // EBFAFileOutOfMemory     - out of memory
    // EBFAFileWrongCipherType - cipher does not fit to this BFA file
    // EBFAFileWrongPassword   - password hash does not match
    // EBFAFileZeroKey         - zero password detected
    // EBFAFileUnknownFormat   - unknown (virtual) file format
    function Decrypt(sFileName : String;
                     setup : TBFAFileDecryptSetup;
                     progressCall : TBFAFileProgress = Nil)
                       : TBFAFileDecryptResult;
  end;


// base class for the following exceptions
type
  EBFAFileError = ECryptFileError;

// exceptions thrown by TBFAFile
type
  EBFAFileGeneralError    = ECryptFileGeneralError;
  EBFAFileFatal           = ECryptFileFatal;
  EBFAFileCipherNotFound  = ECryptFileCipherNotFound;
  EBFAFileSelfTestFailed  = ECryptFileSelfTestFailed;
  EBFAFileInvalidCipher   = ECryptFileInvalidCipher;
  EBFAFileOutOfMemory     = ECryptFileOutOfMemory;
  EBFAFileKeySetupFailed  = ECryptFileKeySetupFailed;
  EBFAFileFileNotFound    = ECryptFileFileNotFound;
  EBFAFileAccessDenied    = ECryptFileAccessDenied;
  EBFAFileReadOnly        = ECryptFileReadOnly;
  EBFAFileDiskFull        = ECryptFileDiskFull;
  EBFAFileWeakKey         = ECryptFileWeakKey;
  EBFAFileIOError         = ECryptFileIOError;
  EBFAFileWrongCipherType = ECryptFileWrongCipherType;
  EBFAFileNoCryptFile     = ECryptFileNoCryptFile;
  EBFAFileWrongPassword   = ECryptFileWrongPassword;
  EBFAFileVersionTooHigh  = ECryptFileVersionTooHigh;
  EBFAFileFileTruncated   = ECryptFileFileTruncated;
  EBFAFileOpenError       = ECryptFileOpenError;
  EBFAFileModeConflict    = ECryptFileModeConflict;
  EBFAFileInternalError   = ECryptFileInternalError;
  EBFAFileEof             = ECryptFileEof;
  EBFAFileUnknown         = ECryptFileUnknown;
  EBFAFileInvalidOpenMode = ECryptFileInvalidOpenMode;
  EBFAFileUnknownRetCode  = ECryptFileUnknownRetCode;
  EBFAFileZeroKey         = ECryptFileZeroKey;
  EBFAFileKeyTooLarge     = ECryptFileKeyTooLarge;
  EBFAFileAlreadyOpened   = ECryptFileAlreadyOpened;
  EBFAFileNotOpened       = ECryptFileNotOpened;
  // own exceptions
  EBFAFileAlreadyEncrypted   = class(EBFAFileError);
  EBFAFileAlreadyExists      = class(EBFAFileError);
  EBFAFileInterrupted        = class(EBFAFileError);
  EBFAFileTempFileDelError   = class(EBFAFileError);
  EBFAFileWeakWiping         = class(EBFAFileError);
  EBFAFileWipingFailed       = class(EBFAFileError);
  EBFAFileSetDateTimeError   = class(EBFAFileError);
  EBFAFileSetAttrError       = class(EBFAFileError);
  EBFAFileUnsupportedVersion = class(EBFAFileError);
  EBFAFileUnknownCompression = class(EBFAFileError);
  EBFAFileCRC32Error         = class(EBFAFileError);
  EBFAFileDamagedHeader      = class(EBFAFileError);
  EBFAFileUnknownFormat      = class(EBFAFileError);


implementation
uses FileCtrl,
     StringPlus,
     CRC32,
     LZSS,
     ZLibEx,
     General,
     Compress,
     Callback;


// prefix for the temporary file name
const
  TEMPFILENAME_PREFIX = 'TMPBFA';

// ID to access the string resources
const
  STRRES_ID = 'BFAFILE';

// length of a random renamed file name, stay 8.3 compatible
const
  STEALTH_FILE_NAME_LEN = 8;

// magic number to detect BFA files
const
  HEADER_MAGIC = $75191114;


// i/o buffer size
const
  IOBUFFER_SIZE = 65536;


// latest version for decrypt a BFA file, enumeration from 1..n
const
  LOWEST_VERSION = 1;

// virtual header type (virtual because all data disappears in the stream of
// the cryptfile)
{$ALIGN OFF}
type
  PVirtualHeader = ^TVirtualHeader;
  TVirtualHeader = packed record
    // magic to detect a header
    lMagic : WORD32;
    // size of header
    bSizeOf : WORD8;
    // lowest version number to handle this file
    bLowestVer : WORD8;
    // the filetime (Win32 compatible), 8 bytes wide
    fileTime : TFileTime;
    // the file's attributes
    lAttributes : WORD32;
    // 64bit length of the file
    lFileSizeLo : WORD32;
    lFileSizeHi : WORD32;
    // length of the unicode file name, which is appended right after the
    // virtual header
    wFileNameLen : WORD16;
    // compression code, see BFAFILE_COMPRESS_xxx constants
    bCompressCode : WORD8;
  end;
{$ALIGN ON}


// min. header size (actually no larger headers are in use)
const
  MIN_HEADER_SIZE = SizeOf(TVirtualHeader);


// internal wipe progress class
type
  TBFAFileWipeProgress = class(TWipeProgress)
  public
    procedure Callback; override;
  end;


// (just setup the parent progress callback object to report the wiping)
procedure TBFAFileWipeProgress.Callback;
var
  bfp : TBFAFileProgress;
begin
  bfp:=TBFAFileProgress(m_callBackObj);

  // map the changed flag
  bfp.SetChanged(GetChanged);

  // set the correct state
  bfp.SetProgressState(BFAFILE_PROGRESS_WIPE);

  // transfer the levels (since we now show also wipe-after progress)
  bfp.SetActPos(GetActPos);
  bfp.SetMaxPos(GetMaxPos);

  // pass this callback
  bfp.SetWipeProgressObj(Self);

  // (this may raise EProgressCallBackInterrupt)
  bfp.CallBack;
end;


//////////////////////// TBFAFileDirectHeaderInfo ////////////////////////


constructor TBFAFileDirectHeaderInfo.Create(sFileName : String;
                                            qFileSize : WORD64;
                                            fileTime : TFileTime;
                                            lAttributes : WORD32;
                                            nCompression : Integer);
begin
  m_sFileName:=sFileName;
  m_qFileSize:=qFileSize;
  m_fileTime:=fileTime;
  m_lAttributes:=lAttributes;
  m_nCompression:=nCompression;
end;

function TBFAFileDirectHeaderInfo.GetFileName : String;
begin
  Result:=m_sFileName;
end;

function TBFAFileDirectHeaderInfo.GetFileSize : WORD64;
begin
  Result:=m_qFileSize;
end;

function TBFAFileDirectHeaderInfo.GetFileTime : TFileTime;
begin
  Result:=m_fileTime;
end;

function TBFAFileDirectHeaderInfo.GetAttributes : WORD32;
begin
  Result:=m_lAttributes;
end;

function TBFAFileDirectHeaderInfo.GetCompression : Integer;
begin
  Result:=m_nCompression;
end;



//////////////////////////// TBFAFileSetup ////////////////////////////


// constructor, no setup parameters set until now
constructor TBFAFileSetup.Create;
begin
  m_password:=Nil;
  m_blTargetPathSet:=False;
  m_blRemoveSourceSet:=False;
  m_blOverwriteExistingSet:=False;
  m_blNoCacheSet:=False;
  m_blKeepDirectoriesSet:=False;
  m_blBasePathSet:=False;
end;



// getters and setters implementations of TBFAFileSetup

procedure TBFAFileSetup.SetPassword(password : TKeyMemory);
begin
  m_password:=password;
end;

function TBFAFileSetup.GetPassword : TKeyMemory;
begin
  if (m_password = Nil) then
    raise EBFAFileSetupError.Create('setup error for: ' +
                                    'm_password');
  Result:=m_password;
end;

procedure TBFAFileSetup.SetTargetPath(sTargetPath : String);
begin
  m_sTargetPath:=sTargetPath;
  m_blTargetPathSet:=True;
end;

function TBFAFileSetup.GetTargetPath : String;
begin
  if (not m_blTargetPathSet) then
    raise EBFAFileSetupError.Create('setup error for: ' +
                                    'm_sTargetPath');
  Result:=m_sTargetPath;
end;

procedure TBFAFileSetup.SetRemoveSource(blRemoveSource : Boolean);
begin
  m_blRemoveSource:=blRemoveSource;
  m_blRemoveSourceSet:=True;
end;

function TBFAFileSetup.GetRemoveSource : Boolean;
begin
  if (not m_blRemoveSourceSet) then
    raise EBFAFileSetupError.Create('setup error for: ' +
                                    'm_blRemoveSource');
  Result:=m_blRemoveSource;
end;

procedure TBFAFileSetup.SetOverwriteExisting(blOverwriteExisting : Boolean);
begin
  m_blOverwriteExisting:=blOverwriteExisting;
  m_blOverwriteExistingSet:=True;
end;

function TBFAFileSetup.GetOverwriteExisting : Boolean;
begin
  if (not m_blOverwriteExistingSet) then
    raise EBFAFileSetupError.Create('setup error for: ' +
                                    'm_blOverwriteExisting');
  Result:=m_blOverwriteExisting;
end;

procedure TBFAFileSetup.SetNoCache(blNoCache : Boolean);
begin
  m_blNoCache:=blNoCache;
  m_blNoCacheSet:=True;
end;

function TBFAFileSetup.GetNoCache : Boolean;
begin
  if (not m_blNoCacheSet) then
    raise EBFAFileSetupError.Create('setup error for: ' +
                                    'm_blNoCache');
  Result:=m_blNoCache;
end;

procedure TBFAFileSetup.SetKeepDirectories(blKeepDirectories : Boolean);
begin
  m_blKeepDirectories:=blKeepDirectories;
  m_blKeepDirectoriesSet:=True;
end;

function TBFAFileSetup.GetKeepDirectories : Boolean;
begin
  if (not m_blKeepDirectoriesSet) then
    raise EBFAFileSetupError.Create('setup error for: ' +
                                    'm_blKeepDirectories');
  Result:=m_blKeepDirectories;
end;


procedure TBFAFileSetup.SetBasePath(sBasePath : String);
begin
  m_sBasePath:=sBasePath;
  m_blBasePathSet:=True;
end;

function TBFAFileSetup.GetBasePath : String;
begin
  if (not m_blBasePathSet) then
    raise EBFAFileSetupError.Create('setup error for: ' +
                                    'm_sBasePath');
  Result:=m_sBasePath;
end;



//////////////////////////// TBFAFileEncryptSetup ////////////////////////////


// constructor, no encrypt setup parameters set until now
constructor TBFAFileEncryptSetup.Create;
begin
  inherited Create;
  m_blForceFileNameSet:=False;
  m_blRenameSet:=False;
  m_blRandomRenameSet:=False;
  m_blMaskNameSet:=False;
  m_blMaskNumberSet:=False;
  m_blMaskExtSet:=False;
  m_blTryRename83Set:=False;
  m_blAddExtensionSet:=False;
  m_blKeepDateTimeSet:=False;
  m_blKeepAttributesSet:=False;
  m_blStorePathSet:=False;
  m_blCompressSet:=False;
  m_blSkipEncryptedSet:=False;
  m_blNoKeyHashSet:=False;
  m_blWriteProtectAfterSet:=False;
  m_blNoCompressTypesSet:=False;
end;

destructor TBFAFileEncryptSetup.Destroy;
begin
  SetNoCompressTypes(Nil);
end;

// getters and setters implementations of TBFAFileEncryptSetup

procedure TBFAFileEncryptSetup.SetHeaderFileName(sHeaderFileName : String);
begin
  m_sHeaderFileName:=sHeaderFileName;
  m_blHeaderFileNameSet:=True;
end;

function TBFAFileEncryptSetup.GetHeaderFileName : String;
begin
  if (not m_blHeaderFileNameSet) then
    raise EBFAFileSetupError.Create('setup error for: ' +
                                    'm_sHeaderFileName');
  Result:=m_sHeaderFileName;
end;

procedure TBFAFileEncryptSetup.SetForceFileName(sForceFileName : String);
begin
  m_sForceFileName:=sForceFileName;
  m_blForceFileNameSet:=True;
end;

function TBFAFileEncryptSetup.GetForceFileName : String;
begin
  if (not m_blForceFileNameSet) then
    raise EBFAFileSetupError.Create('setup error for: ' +
                                    'm_sForceFileName');
  Result:=m_sForceFileName;
end;


procedure TBFAFileEncryptSetup.SetRename(blRename : Boolean);
begin
  m_blRename:=blRename;
  m_blRenameSet:=True;
end;

function TBFAFileEncryptSetup.GetRename : Boolean;
begin
  if (not m_blRenameSet) then
    raise EBFAFileSetupError.Create('setup error for: ' +
                                    'm_blRename');
  Result:=m_blRename;
end;

procedure TBFAFileEncryptSetup.SetRandomRename(blRandomRename : Boolean);
begin
  m_blRandomRename:=blRandomRename;
  m_blRandomRenameSet:=True;
end;

function TBFAFileEncryptSetup.GetRandomRename : Boolean;
begin
  if (not m_blRandomRenameSet) then
    raise EBFAFileSetupError.Create('setup error for: ' +
                                    'm_blRandomName');
  Result:=m_blRandomRename;
end;

procedure TBFAFileEncryptSetup.SetMaskName(sMaskName : String);
begin
  m_sMaskName:=sMaskName;
  m_blMaskNameSet:=True;
end;

function TBFAFileEncryptSetup.GetMaskName : String;
begin
  if (not m_blMaskNameSet) then
    raise EBFAFileSetupError.Create('setup error for: ' +
                                    'm_sMaskName');
  Result:=m_sMaskName;
end;

procedure TBFAFileEncryptSetup.SetMaskNumber(nMaskNumber : Integer);
begin
  m_nMaskNumber:=nMaskNumber;
  m_blMaskNumberSet:=True;
end;

function TBFAFileEncryptSetup.GetMaskNumber : Integer;
begin
  if (not m_blMaskNumberSet) then
    raise EBFAFileSetupError.Create('setup error for: ' +
                                    'm_nMaskNumber');
  Result:=m_nMaskNumber;
end;

procedure TBFAFileEncryptSetup.SetMaskExt(sMaskExt : String);
begin
  m_sMaskExt:=sMaskExt;
  m_blMaskExtSet:=True;
end;

function TBFAFileEncryptSetup.GetMaskExt : String;
begin
  if (not m_blMaskExtSet) then
    raise EBFAFileSetupError.Create('setup error for: ' +
                                    'm_sMaskExt');
  Result:=m_sMaskExt;
end;

procedure TBFAFileEncryptSetup.SetTryRename83(blTryRename83 : Boolean);
begin
  m_blTryRename83:=blTryRename83;
  m_blTryRename83Set:=True;
end;

function TBFAFileEncryptSetup.GetTryRename83 : Boolean;
begin
  if (not m_blTryRename83Set) then
    raise EBFAFileSetupError.Create('setup error for: ' +
                                    'm_blTryRename83');
  Result:=m_blTryRename83;
end;

procedure TBFAFileEncryptSetup.SetAddExtension(blAddExtension : Boolean);
begin
  m_blAddExtension:=blAddExtension;
  m_blAddExtensionSet:=True;
end;

function TBFAFileEncryptSetup.GetAddExtension : Boolean;
begin
  if (not m_blAddExtensionSet) then
    raise EBFAFileSetupError.Create('setup error for: ' +
                                    'm_blAddExtension');
  Result:=m_blAddExtension;
end;

procedure TBFAFileEncryptSetup.SetKeepDateTime(blKeepDateTime : Boolean);
begin
  m_blKeepDateTime:=blKeepDateTime;
  m_blKeepDateTimeSet:=True;
end;

function TBFAFileEncryptSetup.GetKeepDateTime : Boolean;
begin
  if (not m_blKeepDateTimeSet) then
    raise EBFAFileSetupError.Create('setup error for: ' +
                                    'm_blKeepDateTime');
  Result:=m_blKeepDateTime;
end;

procedure TBFAFileEncryptSetup.SetKeepAttributes(blKeepAttributes : Boolean);
begin
  m_blKeepAttributes:=blKeepAttributes;
  m_blKeepAttributesSet:=True;
end;

function TBFAFileEncryptSetup.GetKeepAttributes : Boolean;
begin
  if (not m_blKeepAttributesSet) then
    raise EBFAFileSetupError.Create('setup error for: ' +
                                    'm_blKeepAttributes');
  Result:=m_blKeepAttributes;
end;

procedure TBFAFileEncryptSetup.SetStorePath(blStorePath : Boolean);
begin
  m_blStorePath:=blStorePath;
  m_blStorePathSet:=True;
end;

function TBFAFileEncryptSetup.GetStorePath : Boolean;
begin
  if (not m_blStorePathSet) then
    raise EBFAFileSetupError.Create('setup error for: ' +
                                    'm_blStorePath');
  Result:=m_blStorePath;
end;

procedure TBFAFileEncryptSetup.SetRelativePaths(blRelativePaths : Boolean);
begin
  m_blRelativePaths:=blRelativePaths;
  m_blRelativePathsSet:=True;
end;

function TBFAFileEncryptSetup.GetRelativePaths : Boolean;
begin
  if (not m_blRelativePathsSet) then
    raise EBFAFileSetupError.Create('setup error for: ' +
                                    'm_blRelativePaths');
  Result:=m_blRelativePaths;
end;

procedure TBFAFileEncryptSetup.SetCompress(nCompress : Integer);
begin
  m_nCompress:=nCompress;
  m_blCompressSet:=True;
end;

function TBFAFileEncryptSetup.GetCompress : Integer;
begin
  if (not m_blCompressSet) then
    raise EBFAFileSetupError.Create('setup error for: ' +
                                    'm_nCompress');
  Result:=m_nCompress;
end;

procedure TBFAFileEncryptSetup.SetForceCompress(blForceCompress : Boolean);
begin
  m_blForceCompress:=blForceCompress;
  m_blForceCompressSet:=True;
end;

function TBFAFileEncryptSetup.GetForceCompress : Boolean;
begin
  if (not m_blForceCompressSet) then
    raise EBFAFileSetupError.Create('setup error for: ' +
                                    'm_blForceCompress');
  Result:=m_blForceCompress;
end;

procedure TBFAFileEncryptSetup.SetSkipEncrypted(blSkipEncrypted : Boolean);
begin
  m_blSkipEncrypted:=blSkipEncrypted;
  m_blSkipEncryptedSet:=True;
end;

function TBFAFileEncryptSetup.GetSkipEncrypted : Boolean;
begin
  if (not m_blSkipEncryptedSet) then
    raise EBFAFileSetupError.Create('setup error for: ' +
                                    'm_blSkipEncrypted');
  Result:=m_blSkipEncrypted;
end;

procedure TBFAFileEncryptSetup.SetNoKeyHash(blNoKeyHash : Boolean);
begin
  m_blNoKeyHash:=blNoKeyHash;
  m_blNoKeyHashSet:=True;
end;

function TBFAFileEncryptSetup.GetNoKeyHash : Boolean;
begin
  if (not m_blNoKeyHashSet) then
    raise EBFAFileSetupError.Create('setup error for: ' +
                                    'm_blNoKeyHash');
  Result:=m_blNoKeyHash;
end;

procedure TBFAFileEncryptSetup.SetWriteProtectAfter(blWriteProtectAfter
                                                      : Boolean);
begin
  m_blWriteProtectAfter:=blWriteProtectAfter;
  m_blWriteProtectAfterSet:=True;
end;

function TBFAFileEncryptSetup.GetWriteProtectAfter : Boolean;
begin
  if (not m_blWriteProtectAfterSet) then
    raise EBFAFileSetupError.Create('setup error for: ' +
                                    'm_blWriteProtectAfter');
  Result:=m_blWriteProtectAfter;
end;

procedure TBFAFileEncryptSetup.SetNoCompressTypes(noCompressTypes
                                                    : TStringList);
begin
  if (Nil <> m_noCompressTypes) then begin
    m_noCompressTypes.Destroy;    
  end;
  m_noCompressTypes:=noCompressTypes;
  m_blNoCompressTypesSet:=True;
end;

function TBFAFileEncryptSetup.GetNoCompressTypes : TStringList;
begin
  if (not m_blNoCompressTypesSet) then
    raise EBFAFileSetupError.Create('setup error for: ' +
                                    'm_noCompressTypes');
  Result:=m_noCompressTypes;
end;



//////////////////////////// TBFAFileDecryptSetup ////////////////////////////


// constructor, no decrypt setup parameters set until now
constructor TBFAFileDecryptSetup.Create;
begin
  inherited Create;
  m_blIgnoreCRC32Set:=False;
  m_blRestorePathSet:=False;
  m_blNoKeyCheckSet:=False;
  m_blAcceptTruncatedSet:=False;
  m_blFileInfoOnlySet:=False;
  m_blDirectHeaderInfoSet:=False;
end;


// getters and setters implementations of TBFAFileDecryptSetup

procedure TBFAFileDecryptSetup.SetIgnoreCRC32(blIgnoreCRC32 : Boolean);
begin
  m_blIgnoreCRC32:=blIgnoreCRC32;
  m_blIgnoreCRC32Set:=True;
end;

function TBFAFileDecryptSetup.GetIgnoreCRC32 : Boolean;
begin
  if (not m_blIgnoreCRC32Set) then
    raise EBFAFileSetupError.Create('setup error for: ' +
                                    'm_blIgnoreCRC32');
  Result:=m_blIgnoreCRC32;
end;

procedure TBFAFileDecryptSetup.SetRestorePath(blRestorePath : Boolean);
begin
  m_blRestorePath:=blRestorePath;
  m_blRestorePathSet:=True;
end;

function TBFAFileDecryptSetup.GetRestorePath : Boolean;
begin
  if (not m_blRestorePathSet) then
    raise EBFAFileSetupError.Create('setup error for: ' +
                                    'm_blRestorePath');
  Result:=m_blRestorePath;
end;

procedure TBFAFileDecryptSetup.SetNoKeyCheck(blNoKeyCheck : Boolean);
begin
  m_blNoKeyCheck:=blNoKeyCheck;
  m_blNoKeyCheckSet:=True;
end;

function TBFAFileDecryptSetup.GetNoKeyCheck : Boolean;
begin
  if (not m_blNoKeyCheckSet) then
    raise EBFAFileSetupError.Create('setup error for: ' +
                                    'm_blNoKeyCheck');
  Result:=m_blNoKeyCheck;
end;

procedure TBFAFileDecryptSetup.SetAcceptTruncated(blAcceptTruncated : Boolean);
begin
  m_blAcceptTruncated:=blAcceptTruncated;
  m_blAcceptTruncatedSet:=True;
end;

function TBFAFileDecryptSetup.GetAcceptTruncated : Boolean;
begin
  if (not m_blAcceptTruncatedSet) then
    raise EBFAFileSetupError.Create('setup error for: ' +
                                    'm_blAcceptTruncated');
  Result:=m_blAcceptTruncated;
end;

procedure TBFAFileDecryptSetup.SetFileInfoOnly(blFileInfoOnly : Boolean);
begin
  m_blFileInfoOnly:=blFileInfoOnly;
  m_blFileInfoOnlySet:=True;
end;

function TBFAFileDecryptSetup.GetFileInfoOnly : Boolean;
begin
  if (not m_blFileInfoOnlySet) then
    raise EBFAFileSetupError.Create('setup error for: ' +
                                    'm_blFileInfoOnly');
  Result:=m_blFileInfoOnly;
end;

procedure TBFAFileDecryptSetup.SetDirectHeaderInfo(
            blDirectHeaderInfo : Boolean);
begin
  m_blDirectHeaderInfo:=blDirectHeaderInfo;
  m_blDirectHeaderInfoSet:=True;
end;

function TBFAFileDecryptSetup.GetDirectHeaderInfo : Boolean;
begin
  if (not m_blDirectHeaderInfoSet) then
    raise EBFAFileSetupError.Create('setup error for: ' +
                                    'm_blDirectHeaderInfo');
  Result:=m_blDirectHeaderInfo;
end;




//////////////////////////// TBFAFileResult ////////////////////////////


constructor TBFAFileResult.Create(sr : TStrRes);
begin
  m_nWarnings:=0;
  m_sr:=sr;
end;


// getters and setters implementations of TBFAFileResult

procedure TBFAFileResult.SetSourceFileName(sSourceFileName : String);
begin
  m_sSourceFileName:=sSourceFileName;
end;

function TBFAFileResult.GetSourceFileName : String;
begin
  Result:=m_sSourceFileName;
end;

procedure TBFAFileResult.SetWarnings(nNewWarnings : Integer);
begin
  m_nWarnings:=nNewWarnings;
end;

function TBFAFileResult.GetWarnings : Integer;
begin
  Result:=m_nWarnings;
end;

function TBFAFileResult.RenderWarnings : String;
var
  blFirstWarning : Boolean;
  sTemp          : String;

// subroutine for warning bits extraction and string representation
procedure AddWarning(nWarningBit : Integer;
                     sWarningID : String);
begin
  if ((m_nWarnings and nWarningBit) = nWarningBit) then begin
    if (blFirstWarning) then
      blFirstWarning:=False
    else
      sTemp:=sTemp + ', ';
    sTemp:=sTemp + m_sr.Get(STRRES_ID, sWarningID);
  end;
end;

begin
  // make a nice warning string, i.n.
  if (m_nWarnings = 0) then begin
    Result:=m_sr.Get(STRRES_ID, '050');
    Exit;
  end;
  sTemp:='';
  blFirstWarning:=True;
  AddWarning(BFAFILE_WARNING_WEAKWIPING, '081');
  AddWarning(BFAFILE_WARNING_REMOVEFAILED, '082');
  AddWarning(BFAFILE_WARNING_SETATTRFAILED, '083');
  AddWarning(BFAFILE_WARNING_SETDATETIMEFAILED, '084');
  AddWarning(BFAFILE_WARNING_INTERRUPTED, '085');
  AddWarning(BFAFILE_WARNING_TRUNCATED, '086');
  AddWarning(BFAFILE_WARNING_USED83RENAMING, '035');
  Result:=sTemp;
end;


//////////////////////////// TBFAFileEncryptResult ////////////////////////////


// getters and setters implementations of TBFAFileEncryptResult

procedure TBFAFileEncryptResult.SetOriginalFileSize(qOriginalFileSize : WORD64);
begin
  m_qOriginalFileSize:=qOriginalFileSize;
end;

function TBFAFileEncryptResult.GetOriginalFileSize : WORD64;
begin
  Result:=m_qOriginalFileSize;
end;

procedure TBFAFileEncryptResult.SetBytesCompressed(qBytesCompressed
                                                    : WORD64);
begin
  m_qBytesCompressed:=qBytesCompressed;
end;

function TBFAFileEncryptResult.GetBytesCompressed : WORD64;
begin
  Result:=m_qBytesCompressed;
end;

procedure TBFAFileEncryptResult.SetBFAFileName(sBFAFileName : String);
begin
  m_sBFAFileName:=sBFAFileName;
end;

function TBFAFileEncryptResult.GetBFAFileName: String;
begin
  Result:=m_sBFAFileName;
end;


//////////////////////////// TBFAFileDecryptResult ////////////////////////////


// getters and setters implementations of TBFAFileDecryptResult

constructor TBFAFileDecryptResult.Create(sr : TStrRes);
begin
  inherited Create(sr);
  m_dhi:=Nil;
end;

destructor TBFAFileDecryptResult.Destroy;
begin
  if (m_dhi <> Nil) then
    m_dhi.Destroy;
end;


procedure TBFAFileDecryptResult.SetBytesWritten(qBytesWritten : WORD64);
begin
  m_qBytesWritten:=qBytesWritten;
end;

function TBFAFileDecryptResult.GetBytesWritten : WORD64;
begin
  Result:=m_qBytesWritten;
end;

procedure TBFAFileDecryptResult.SetOriginalFileName(sOriginalFileName : String);
begin
  m_sOriginalFileName:=sOriginalFileName;
end;

function TBFAFileDecryptResult.GetOriginalFileName: String;
begin
  Result:=m_sOriginalFileName;
end;

procedure TBFAFileDecryptResult.SetDirectHeaderInfo(
            dhi : TBFAFileDirectHeaderInfo);
begin
  m_dhi:=dhi;
end;

function TBFAFileDecryptResult.GetDirectHeaderInfo : TBFAFileDirectHeaderInfo;
begin
  Result:=m_dhi;
end;



//////////////////////////// TBFAFileProgress ////////////////////////////


// getters and setters implementations of TBFAFileProgress

procedure TBFAFileProgress.SetProgressState(nProgressState : Integer);
begin
  m_nProgressState:=nProgressState;
end;

function TBFAFileProgress.GetProgressState : Integer;
begin
  Result:=m_nProgressState;
end;

procedure TBFAFileProgress.SetInputFileName(sInputFileName : String);
begin
  m_sInputFileName:=sInputFileName;
end;

function TBFAFileProgress.GetInputFileName : String;
begin
  Result:=m_sInputFileName;
end;

procedure TBFAFileProgress.SetOutputFileName(sOutputFileName : String);
begin
  m_sOutputFileName:=sOutputFileName;
end;

function TBFAFileProgress.GetOutputFileName : String;
begin
  Result:=m_sOutputFileName;
end;

procedure TBFAFileProgress.SetFileSize(qFileSize : WORD64);
begin
  m_qFileSize:=qFileSize;
end;

function TBFAFileProgress.GetFileSize : WORD64;
begin
  Result:=m_qFileSize;
end;

procedure TBFAFileProgress.SetWipeProgressObj(wipeProgressObj : TWipeProgress);
begin
  m_wipeProgressObj:=wipeProgressObj;
end;

function TBFAFileProgress.GetWipeProgressObj : TWipeProgress;
begin
  Result:=m_wipeProgressObj;
end;


//////////////////////////// TBFAFile ////////////////////////////


constructor TBFAFile.Create(const sCipherName : String;
                            sr : TStrRes;
                            rs : TRandomSource;
                            pRandSeed : Pointer;
                            lRandSeedLen : WORD32);
begin

  // get the string resources
  m_sr:=sr;

  // load and test the cipher
  inherited Create(sCipherName,
                   sr,
                   rs,
                   pRandSeed,
                   lRandSeedLen);

  // allocate the basic i/o buffer
  GetMem(m_pIOBuffer, IOBUFFER_SIZE);
end;


// releases the cipher and frees the buffers
destructor TBFAFile.Destroy;
begin
  // clear and free the basic i/o buffer
  if (m_pIOBuffer <> Nil) then begin
    FillChar(m_pIOBuffer^, IOBUFFER_SIZE, 0);
    FreeMem(m_pIOBuffer);
  end;

  // release the cipher
  inherited Destroy;
end;




function TBFAFile.Encrypt(sFileName : String;
                          wipeObj : TWipe;
                          setup : TBFAFileEncryptSetup;
                          progressCall : TBFAFileProgress)
                            : TBFAFileEncryptResult;
var
  qFileSize          : WORD64;
  qTargetFileSize    : WORD64;
  qBytesCompressed   : WORD64;
  lPreRead           : WORD32;
  lToRead            : WORD32;
  lCheckMagic        : WORD32;
  lCRC32             : WORD32;
  dwBytesRead        : DWORD;
  dwAttributes       : DWORD;
  dwNewAttributes    : DWORD;
  dwDummy            : DWORD;
  nI                 : Integer;
  nLen               : Integer;
  nTargetPathLen     : Integer;
  nWarningBits       : Integer;
  nCompressed        : Integer;
  blEOF              : Boolean;
  blMoreToDo         : Boolean;
  blCompressIt       : Boolean;
  blWipeFailed       : Boolean;
  blUseTempFile      : Boolean;
  blFileIsThere      : Boolean;
  blRenamingFailed   : Boolean;
  blNotCompressable  : Boolean;
  blNoCompressRepeat : Boolean;
  blCompressIgnored  : Boolean;
  sSourceFileName    : String;
  sTemp              : String;
  sTargetPath        : String;
  sTempFileName      : String;
  sTargetFileName    : String;
  pDeflateBuffer     : Pointer;
  wsUnicodeName      : WideString;
  wsTemp             : WideString;
  fhandle            : THandle;
  vheader            : TVirtualHeader;
  ftCreation         : TFileTime;
  ftLastAccess       : TFileTime;
  ftLastWrite        : TFileTime;
  checksum           : TCRC32;
  compressor         : TCompressor;
  wipeProgress       : TBFAFileWipeProgress;
  throwThis          : ECryptFileError;

// local sub to create the new filename, either 8.3 compatible or not,
// 8.3 compatible means that we assume a target system where only filenames
// in the good old DOS format can be stored and that the given filename
// is already in the 8.3 format (we cannot recode the Win32 "~n" file renaming
// scheme, of course)
function MakeTargetFileName(blFormat83 : Boolean) : String;
begin
  // force a defined output file name?
  if (setup.GetForceFileName <> '') then begin
    Result:=setup.GetForceFileName;
    Exit;
  end;

  // first get the filename without any path
  Result:=ExtractFileName(sSourceFileName);

  // rename the file?
  if (setup.GetRename) then begin
    // yes, now mask or random?
    if (setup.GetRandomRename) then begin
      // this result is always 8.3 compatible
      Result:=TStrPlus.RandomStr(STEALTH_FILE_NAME_LEN);
      if (setup.GetAddExtension) then
        Result:=Result + '.' + BFAFILE_EXTENSION;
    end
    else begin
      // build the filename from the mask (make it 8.3 compatible, i.n.)
      Result:=setup.GetMaskName + IntToStr(setup.GetMaskNumber);
      if (blFormat83) then begin
        if (Length(Result) > 8) then
          // (we use the backend to avoid collisions)
          Result:=Copy(Result, Length(Result) - 8, 8);
      end;
      // add a legal extension
      if (setup.GetAddExtension) then
        if (blFormat83) then
          // (ignore the given mask extension here)
          Result:=Result + '.' + BFAFILE_EXTENSION
        else
          Result:=Result + '.' + setup.GetMaskExt + '.' + BFAFILE_EXTENSION
      else
        // (just add the mask extension)
        Result:=Result + '.' + setup.GetMaskExt;
    end;
  end
  else
    // no renaming, but an extension?
    if (setup.GetAddExtension) then begin
      // must it be 8.3 compatible?
      if (blFormat83) then
        // yes, so cut off the original extension
        Result:=Copy(Result,
                     1,
                     Length(Result) - Length(ExtractFileExt(Result)));
      Result:=Result + '.' + BFAFILE_EXTENSION
    end;
end;

// cleaners

procedure FreeStreamHandlers;
begin
  // free the CRC32
  checksum.Destroy;

  // destroy the compressor and its buffer
  if (compressor <> Nil) then
    compressor.Destroy;
  if (pDeflateBuffer <> Nil) then
    FreeMem(pDeflateBuffer);
end;


procedure CloseFiles;
begin
  // close the source file
  CloseHandle(fhandle);

  // close the cryptfile
  Close;
end;


procedure ErrorCleanup(err : EBFAFileError);
begin
  FreeStreamHandlers;

  try
    CloseFiles
  except
    on ECryptFileError do begin
      if (err <> Nil) then
        err.Destroy;
      raise;
    end;
  end;

  // remove the dead cryptfile
  if (not DeleteFile(sTargetFileName)) then begin
    if (err <> Nil) then
      err.Destroy;
    raise EBFAFileFatal.Create(m_sr.Get(STRRES_ID, '033'));
  end;

  if (err <> Nil) then
    raise err;
end;



begin
  // store the original file name
  sSourceFileName:=sFileName;

  // make the filename relative to the base path, if necessary
  if (setup.GetBasePath <> '')  then
    if (AnsiStrPos(PChar(sFileName),
                   PChar(setup.GetBasePath)) =
        PChar(sFileName)) then begin

      nLen:=Length(setup.GetBasePath);
      sFileName:=TStrPlus.RelativePath(Copy(sFileName,
                                       nLen + 1,
                                       Length(sFileName) - nLen));
    end;

  // open the source file via the Win32 API
  fhandle:=CreateFile(PChar(sSourceFileName),
                      GENERIC_READ,
                      FILE_SHARE_READ,
                      NULL,
                      OPEN_EXISTING,
                      FILE_FLAG_SEQUENTIAL_SCAN,
                      0);
  if (fhandle = INVALID_HANDLE_VALUE) then begin
    case GetLastError of
      ERROR_SHARING_VIOLATION :
        raise EBFAFileAccessDenied.Create(m_sr.Get(STRRES_ID, '022'));
      ERROR_ACCESS_DENIED :
        raise EBFAFileAccessDenied.Create(m_sr.Get(STRRES_ID, '022'));
      ERROR_FILE_NOT_FOUND :
        raise EBFAFileFileNotFound.Create(m_sr.Get(STRRES_ID, '023'));
    else
      raise EBFAFileOpenError.Create(m_sr.Get(STRRES_ID, '024'));
    end;
  end;

  // check if the file is already encrypted, i.n.
  lPreRead:=0;
  if (setup.GetSkipEncrypted) then begin
    if (ReadFile(fhandle,
                 lCheckMagic,
                 SizeOf(lCheckMagic),
                 lPreRead,
                 NULL) = FALSE) then begin
      if (CloseHandle(fhandle) = FALSE) then
        raise EBFAFileFatal.Create(m_sr.Get(STRRES_ID, '025'));
      raise EBFAFileIOError.Create(m_sr.Get(STRRES_ID, '026'))
    end;

    // enough bytes read?
    if (lPreRead = SizeOf(lCheckMagic)) then
      // magic?
      if (lCheckMagic = CRYPTFILE_MAGIC) then begin
        if (CloseHandle(fhandle) = FALSE) then
          raise EBFAFileFatal.Create(m_sr.Get(STRRES_ID, '068'));
        raise EBFAFileAlreadyEncrypted.Create(m_sr.Get(STRRES_ID, '027'));
      end;

    // copy the preread data to the i/o buffer
    Move(lCheckMagic, m_pIOBuffer^,  SizeOf(lCheckMagic));
  end;

  // get the target path and its length
  sTargetPath:=setup.GetTargetPath;
  nTargetPathLen:=Length(sTargetPath);

  // make the target file name
  sTargetFileName:=MakeTargetFileName(setup.GetTryRename83);

  // append to the target path and set the temporay file name
  blUseTempFile:=False;
  if (nTargetPathLen > 0) then begin
    // (keep the file's directory, if necessary)
    if (setup.GetKeepDirectories) then begin
      sTargetFileName:=TStrPlus.RTLPath(sTargetPath) +
                       TStrPlus.RTLPath(
                         TStrPlus.RelativePath(ExtractFilePath(sFileName))) +
                       sTargetFileName;
      sTemp:=ExtractFilePath(sTargetFileName);
      if (not DirectoryExists(sTemp)) then
        ForceDirectories(sTemp);
    end
    else begin
      sTargetFileName:=TStrPlus.RTLPath(sTargetPath) + sTargetFileName;
    end;
    sTempFileName:=sTargetFileName;
  end
  else begin
    sTemp:=TStrPlus.RTLPath(ExtractFilePath(sSourceFileName));
    sTargetFileName:=sTemp + sTargetFileName;
    if (AnsiUpperCase(ExtractFileName(sTargetFileName)) =
        AnsiUpperCase(ExtractFileName(sSourceFileName))) then begin
      sTempFileName:=TStrPlus.MakeTempFileName(TStrPlus.PurePath(sTemp),
                                               TEMPFILENAME_PREFIX);
      blUseTempFile:=True;
    end
    else begin
      sTempFileName:=sTargetFileName;
    end;
  end;


  // check if the target file already exists
  blFileIsThere:=FileExists(sTargetFileName);
  if ((not setup.GetOverwriteExisting) and (not blUseTempFile)) then
    if (blFileIsThere) then begin
      if (CloseHandle(fhandle) = FALSE) then
        raise EBFAFileFatal.Create(m_sr.Get(STRRES_ID, '068'));
      // (provide a complete error message here)
      raise EBFAFileAlreadyExists.Create(Format(m_sr.Get(STRRES_ID, '028'),
                                                [sTargetFileName]));
    end;

  // get the source file size, time and attributes into the virtual header
  with vheader do begin
    lFileSizeLo:=Windows.GetFileSize(fhandle,
                                     @lFileSizeHi);
    if (lFileSizeLo = $ffffffff) then
      if (GetLastError <> NO_ERROR) then begin
        if (CloseHandle(fhandle) = FALSE) then
          raise EBFAFileFatal.Create(m_sr.Get(STRRES_ID, '068'));
        raise EBFAFileIOError.Create(m_sr.Get(STRRES_ID, '029'));
      end;
    if (GetFileTime(fhandle,
                    @ftCreation,
                    @ftLastAccess,
                    @ftLastWrite) = FALSE) then begin
      if (CloseHandle(fhandle) = FALSE) then
        raise EBFAFileFatal.Create(m_sr.Get(STRRES_ID, '068'));
      raise EBFAFileIOError.Create(m_sr.Get(STRRES_ID, '030'));
    end;
    fileTime:=ftLastWrite;
    dwAttributes:=GetFileAttributes(PChar(sSourceFileName));
    if (dwAttributes = $ffffffff) then begin
      if (CloseHandle(fhandle) = FALSE) then
        raise EBFAFileFatal.Create(m_sr.Get(STRRES_ID, '068'));
      raise EBFAFileIOError.Create(m_sr.Get(STRRES_ID, '031'));
    end;
    lAttributes:=dwAttributes;
  end;

  // enable access to the temporary file, i.n.
  if (blFileIsThere) then begin
    if (SetFileAttributes(PChar(sTempFileName),
                          FILE_ATTRIBUTE_NORMAL) = FALSE) then begin
      if (GetLastError <> ERROR_FILE_NOT_FOUND) then begin
        if (CloseHandle(fhandle) = FALSE) then
          raise EBFAFileFatal.Create(m_sr.Get(STRRES_ID, '068'));
        raise EBFAFileIOError.Create(m_sr.Get(STRRES_ID, '032'));
      end;
    end;
  end;

  // store the rest of the static stuff into the virtual header
  with vheader do begin
    lMagic:=HEADER_MAGIC;
    bSizeOf:=SizeOf(TVirtualHeader);
    bLowestVer:=LOWEST_VERSION;
  end;

  // prepare the the original file name to be stored
  if (setup.GetStorePath) then begin

    if (setup.GetRelativePaths) then begin
      if (setup.GetBasePath <> '')  then
        sTemp:=sFileName
      else
        // (this should never happen)
        sTemp:=TStrPlus.RelativePath(sFileName);
    end
    else begin
      sTemp:=TStrPlus.RelativePath(sSourceFileName);
    end;
  end
  else begin
    sTemp:=ExtractFileName(sSourceFileName);
  end;

  if (setup.GetHeaderFileName = '') then
    wsUnicodeName:=TStrPlus.StringToWideString(sTemp)
  else
    wsUnicodeName:=TStrPlus.StringToWideString(setup.GetHeaderFileName);
  // (store the filename length in the header)
  vheader.wFileNameLen:=Length(wsUnicodeName);

  // set cache and keyhash flags
  SetNoCache(setup.GetNoCache);
  SetNoKeyHashCheck(setup.GetNoKeyHash);

  // just to please the compiler
  compressor:=Nil;
  pDeflateBuffer:=Nil;

  // create the cryptfile (first temporary)
  wsTemp:=TStrPlus.StringToWideString(sTempFileName);
  try
    OpenCreate(PWideChar(wsTemp),
               setup.GetPassword.GetPtr,
               setup.GetPassword.GetSize);
  except
    on ecfoe : ECryptFileError do begin
      if (CloseHandle(fhandle) = FALSE) then
        raise EBFAFileFatal.Create(m_sr.Get(STRRES_ID, '064'));
      // remove the existing file, if possible
      // (FIXME: did we check really all cases?)
      if (not (ecfoe is ECryptFileAccessDenied)) then
        if (blFileIsThere) then
          if (Windows.DeleteFile(PChar(sTempFileName)) = FALSE) then
            raise EBFAFileFatal.Create(m_sr.Get(STRRES_ID, '033'));
      // reraise the exception
      raise;
    end;
  end;

  // we reuse the checksum by resetting it below
  checksum:=TCRC32.Create;

  // from here we will start all over again when the compression (if demanded
  // at all) will fail below
  blNoCompressRepeat:=False;
  blCompressIgnored:=False;
  repeat

    // get the compression flag
    if (blNoCompressRepeat) then begin
      blCompressIt:=False
    end
    else begin
      // check for no compression types
      blCompressIt:=(setup.GetCompress <> BFAFILE_COMPRESS_NONE);
      if (blCompressIt and (setup.GetNoCompressTypes <> Nil)) then begin
        for nI:=0 to (setup.GetNoCompressTypes.Count - 1) do begin
          if (TStrPlus.CompareFromEnd(setup.GetNoCompressTypes.Strings[nI],
                                      sFileName,
                                      False)) then begin
            blCompressIt:=False;
            blCompressIgnored:=True;
            Break;
          end;
        end;
      end;
    end;

    // complete the virtual header
    if (blCompressIt) then
      vheader.bCompressCode:=WORD8(setup.GetCompress)
    else
      vheader.bCompressCode:=BFAFILE_COMPRESS_NONE;

    // write header and filename
    try
      Write(@vheader, SizeOf(vheader));
      Write(PWideChar(wsUnicodeName), Length(wsUnicodeName) * 2);
    except
      on ecfoe : ECryptFileError do begin
        throwThis:=EBFAFileIOError.Create(m_sr.Get(STRRES_ID, '034'));
        ErrorCleanup(throwThis);
      end;
    end;

    // initialize the CRC32
    checksum.reset;

    // prepare the compressor and its buffer, i.n.
    if (blCompressIt) then begin
      case setup.GetCompress of
        BFAFILE_COMPRESS_LZSS:
            compressor:=TLZSSCompressor.Create;
        BFAFILE_COMPRESS_DEFL:
            compressor:=TZLibExCompressor.Create(ZLIBEX_TYPE_DEFLATE);
        BFAFILE_COMPRESS_BZP2:
            compressor:=TZLibExCompressor.Create(ZLIBEX_TYPE_BZIP2);
      end;
      GetMem(pDeflateBuffer, IOBUFFER_SIZE);
    end;

    // set up the callback object (note if we repeat with no compression the
    // progress callback state is partially stalled)
    qFileSize:=MakeWORD64(vheader.lFileSizeLo, vheader.lFileSizeHi);
    with progressCall do begin
      ZeroPos;
      SetChanged(True);
      if (not blNoCompressRepeat) then begin
        SetMaxPos(qFileSize);
        SetInputFileName(sSourceFileName);
        SetOutputFileName(sTargetFileName);
        SetFileSize(qFileSize);
        SetProgressState(BFAFILE_PROGRESS_ENCRYPT);
      end;
    end;

    // now read and encrypt the file data
    blEOF:=False;
    nWarningBits:=0;
    qBytesCompressed:=0;
    blNotCompressable:=False;
    try
      while ((not blEOF) and
             (not blNotCompressable)) do begin

        // send the actual state
        progressCall.CallBack;
        progressCall.SetProgressState(BFAFILE_PROGRESS_ENCRYPT);

        // try to read a whole buffer content
        lToRead:=IOBUFFER_SIZE - lPreRead;
        if (ReadFile(fhandle,
                     PWORD8Buf(m_pIOBuffer)^[lPreRead],
                     lToRead,
                     dwBytesRead,
                     NULL) = FALSE) then begin
          throwThis:=EBFAFileIOError.Create(m_sr.Get(STRRES_ID, '065'));
          ErrorCleanup(throwThis);
        end
        else begin
          // update the counters
          progressCall.IncPos(dwBytesRead);

          // add the already read amount of data
          Inc(dwBytesRead, lPreRead);

          // end of file?
          if (dwBytesRead < IOBUFFER_SIZE) then
            blEOF:=True;

          // update the CRC32
          checksum.Update(m_pIOBuffer, dwBytesRead);

          // compress data, if necessary
          if (blCompressIt) then begin

            // loop until all data has been stuffed into the compressor
            repeat

              blMoreToDo:=compressor.ProcessData(m_pIOBuffer,
                                                 pDeflateBuffer,
                                                 dwBytesRead,
                                                 IOBUFFER_SIZE,
                                                 nCompressed);
              // eventually check if file is compressable
              Inc(qBytesCompressed, nCompressed);
              if (qBytesCompressed > qFileSize) and
                  (not setup.m_blForceCompress) then begin
                blNotCompressable:=True;
                Break;
              end
              else
                // write compressed data to cryptfile
                Write(pDeflateBuffer, nCompressed);
            until (not blMoreToDo);

          end
          else begin
            // write data to the cryptfile
            Write(m_pIOBuffer, dwBytesRead);
          end;
        end;

        // no pre-read data anymore
        lPreRead:=0;
      end;

      // don't finish if we couldn't compress the data
      if (not blNotCompressable) then begin

        // if we're at the end we need to flush (same I/O as in the loop above)
        if (blCompressIt) then begin
          repeat
            blMoreToDo:=compressor.Finalize(pDeflateBuffer,
                                            IOBUFFER_SIZE,
                                            nCompressed);
            Inc(qBytesCompressed, nCompressed);
            if (qBytesCompressed > qFileSize) and
                (not setup.m_blForceCompress) then begin
               blNotCompressable:=True;
               Break;
            end
            else
              Write(pDeflateBuffer, nCompressed);
          until (not blMoreToDo);
        end;

        // if we repeated the operation due to uncompressable data we will
        // now report the final state with the max. progress position
        //if (blNoCompressRepeat) then
        //  progressCall.SetActPos(progressCall.GetMaxPos);

        // do the final callback
        progressCall.CallBack;

        // we can exit now
        blNoCompressRepeat:=False;

        // append the CRC32
        lCRC32:=checksum.Finalize;
        Write(@lCRC32, SizeOf(lCRC32));
      end;

    except
      // interrupt detected?
      on ECallBackInterrupt do begin
        throwThis:=EBFAFileInterrupted.Create(m_sr.Get(STRRES_ID, '036'));
        ErrorCleanup(throwThis);
      end;

      // error while processing the cryptfile?
      on ECryptFileError do begin
        ErrorCleanup(Nil);
        raise;
      end;
    end;

    // can we take a second chance and try it without any compression?
    if (blNotCompressable) then begin

      // seek back in the source file to the beginning
      dwDummy:=0;
      if (SetFilePointer(fhandle,
                         0,
                         @dwDummy,
                         FILE_BEGIN) = $ffffffff) then begin
        if (GetLastError = NO_ERROR) then
          blNoCompressRepeat:=True
        else begin
          throwThis:=EBFAFileIOError.Create(m_sr.Get(STRRES_ID, '043'));
          ErrorCleanup(throwThis);
        end;
      end
      else
        // reset the encryption session
        try
          Reset;
          // (successfully reset, now we can try it again)
          blNoCompressRepeat:=True;
        except
          on ecfe : ECryptFileError do begin
            ErrorCleanup(Nil);
            raise;
          end;
        end;
    end;

  until (not blNoCompressRepeat);
  (* end of REPEAT *)

  // release stream related resources
  FreeStreamHandlers;
  
  // close the files
  CloseFiles;

  // wipe or delete the original file, i.n.
  if ((setup.GetRemoveSource or blUseTempFile or (nTargetPathLen = 0)) and
    (Nil <> wipeObj)) then begin

    // create your wipe progress callback object
    wipeProgress:=TBFAFileWipeProgress.Create(progressCall);

    // wipe the original file
    blWipeFailed:=False;
    try
      if (not wipeObj.Execute(sSourceFileName,
                              wipeProgress,
                              m_sr)) then
        nWarningBits:=nWarningBits or BFAFILE_WARNING_WEAKWIPING;
    except
      on ewfe : EWipeFatal do begin
        // quit immediately if a fatal wipe error occured
        wipeProgress.Destroy;
        raise EBFAFileFatal.Create(m_sr.Get(STRRES_ID, '038') + ewfe.Message);
      end;
      on EWipeWasBreak do
        nWarningBits:=nWarningBits or BFAFILE_WARNING_INTERRUPTED;
      on EWipeError do
        blWipeFailed:=True;
    end;
    wipeProgress.Destroy;

    if (blWipeFailed) then
      nWarningBits:=nWarningBits or BFAFILE_WARNING_REMOVEFAILED;

    // if the original file could not be wiped and if we use a temporary file
    // (which must be renamed to the original file name, of course) then we
    // really have a problem
    if (blWipeFailed and blUseTempFile) then begin

      // remove the cryptfile only when we tried to delete (not to wipe)
      // the original file, because some data might already be overwritten
      if ((wipeObj is TWipeDeleteOnly) and
          FileExists(sSourceFileName)) then begin
        DeleteFile(sTempFileName);
        raise EBFAFileFatal.Create(m_sr.Get(STRRES_ID, '039'));
      end
      else
        raise EBFAFileFatal.Create(Format(m_sr.Get(STRRES_ID, '087'),
                                          [sSourceFileName, sTempFileName]));
    end;
  end;

  // rename the cryptfile
  if (blUseTempFile) then begin
    blRenamingFailed:=(Windows.MoveFile(PChar(sTempFileName),
                                        PChar(sTargetFileName)) = FALSE);
    // don't give up, try 8.3 renaming convention (if not done already)
    if (blRenamingFailed and (not setup.GetTryRename83)) then begin
      sTargetFileName:=ExtractFilePath(sTargetFileName) +
                       MakeTargetFileName(True);
      blRenamingFailed:=(Windows.MoveFile(PChar(sTempFileName),
                                          PChar(sTargetFileName)) = FALSE);
      // we have to report that we didn't renamed it as demanded
      if (not blRenamingFailed) then
        nWarningBits:=nWarningBits or BFAFILE_WARNING_USED83RENAMING;
    end;

    // not good
    if (blRenamingFailed) then
      raise EBFAFileFatal.Create(Format(m_sr.Get(STRRES_ID, '067'),
                                        [sTempFileName]));
  end;

  // set the original date+time stamp, i.n.
  if (setup.GetKeepDateTime) then begin
    fhandle:=CreateFile(PChar(sTargetFileName),
                        GENERIC_WRITE,
                        FILE_SHARE_READ,
                        Nil,
                        OPEN_EXISTING,
                        0,
                        0);
    if (fhandle <> INVALID_HANDLE_VALUE) then begin
      if (SetFileTime(fhandle, @ftCreation, @ftLastAccess,
          @ftLastWrite) = FALSE) then
        nWarningBits:=nWarningBits or BFAFILE_WARNING_SETDATETIMEFAILED;
      if (CloseHandle(fhandle) = FALSE) then
        raise EBFAFileFatal.Create(m_sr.Get(STRRES_ID, '040'));
    end
    else
      nWarningBits:=nWarningBits or BFAFILE_WARNING_SETDATETIMEFAILED;
  end;

  // restore the original attributes and write protect the cryptfile, i.n.
  if ((setup.GetKeepAttributes) or (setup.GetWriteProtectAfter)) then begin
    if (setup.GetWriteProtectAfter) then
      dwNewAttributes:=FILE_ATTRIBUTE_READONLY
    else
      dwNewAttributes:=0;
    if (setup.GetKeepAttributes) then
    dwNewAttributes:=dwNewAttributes or dwAttributes;
    if (SetFileAttributes(PChar(sTargetFileName),
                          dwNewAttributes) = FALSE) then
      nWarningBits:=nWarningBits or BFAFILE_WARNING_SETATTRFAILED;
  end;

  // build the result object
  Result:=TBFAFileEncryptResult.Create(m_sr);
  with Result do begin
    SetSourceFileName(sSourceFileName);
    SetWarnings(nWarningBits);
    SetOriginalFileSize(qFileSize);
    if (blCompressIgnored) then
      SetBytesCompressed(BFAFILE_COMPRESSION_IGNORED)
    else
      SetBytesCompressed(qBytesCompressed);
    SetBFAFileName(sTargetFileName);
  end;
end;




function TBFAFile.Decrypt(sFileName : String;
                          setup : TBFAFileDecryptSetup;
                          progressCall : TBFAFileProgress = Nil)
                            : TBFAFileDecryptResult;
var
  nLen            : Integer;
  nRead           : Integer;
  nBytesToRead    : Integer;
  nDecompressed   : Integer;
  nWarningBits    : Integer;
  lOpenMask       : WORD32;
  lCRC32          : WORD32;
  blRepeat        : Boolean;
  blAllRead       : Boolean;
  blSourceOpened  : Boolean;
  blTruncated     : Boolean;
  blTargetOpened  : Boolean;
  blTargetExists  : Boolean;
  blUseTempFile   : Boolean;
  blCompressed    : Boolean;
  blSourceRemoved : Boolean;
  blPathToRestore : Boolean;
  blFileIsThere   : Boolean;
  pInflateBuffer  : Pointer;
  sTemp           : String;
  sHeaderFileName : String;
  sTargetFileName : String;
  sTempFileName   : String;
  qBytesToProcess : WORD64;
  qBytesWritten   : WORD64;
  wsTemp          : WideString;
  fhandle         : THandle;
  vheader         : TVirtualHeader;
  decompressor    : TDecompressor;
  checksum        : TCRC32;
  cleanupFatal    : EBFAFileFatal;

function ReadBytes(pTarget : Pointer; nNumOfBytes : Integer) : Integer;
begin
  Result:=0; // (for the case of a truncated file)
  try
    Result:=Read(pTarget,
                 nNumOfBytes);
  except
    // catch an EOF error
    on ecfoe : ECryptFileEof do begin
      // do we accept truncated files?
      if (setup.GetAcceptTruncated) then begin
        // yes, set the warning flag
        blTruncated:=True;
        nWarningBits:=nWarningBits or BFAFILE_WARNING_TRUNCATED;
      end
      else
        // no, reraise the exception
        raise;
    end;
  end;
end;

function WriteBytes(pSource : Pointer; lNumOfBytes : WORD32) : Integer;
var
  lWritten : WORD32;
begin
  if (WriteFile(fhandle,
                pSource^,
                lNumOfBytes,
                lWritten,
                Nil) = FALSE) then begin
    // disk full error?
    if (GetLastError = ERROR_DISK_FULL) then
      raise EBFAFileDiskFull.Create(m_sr.Get(STRRES_ID, '050'))
    else
      raise EBFAFileIOError.Create(m_sr.Get(STRRES_ID, '051'));
  end;

  // check again for complete data output
  if (lWritten < lNumOfBytes) then
    raise EBFAFileDiskFull.Create(m_sr.Get(STRRES_ID, '050'));

  // update the checksum, i.n.
  if (not setup.GetIgnoreCRC32) then
    checksum.Update(pSource,
                    lNumOfBytes);

  // increase the output counter
  Inc(qBytesWritten, lWritten);
  Result:=lWritten;
end;


begin
  // set up for a proper clean up if an error occured
  blSourceOpened:=False;
  blTargetOpened:=False;
  blTargetExists:=False;
  blSourceRemoved:=False;
  blTruncated:=False;
  nWarningBits:=0;
  checksum:=Nil;
  decompressor:=Nil;
  pInflateBuffer:=Nil;

  try
    // try to open the cryptfile
    SetNoCache(setup.GetNoCache);
    SetNoKeyHashCheck(setup.GetNoKeyCheck);

    wsTemp:=TStrPlus.StringToWideString(sFileName);
    OpenRead(PWideChar(wsTemp),
             setup.GetPassword.GetPtr,
             setup.GetPassword.GetSize);
    blSourceOpened:=True;

    // get the file size
    qBytesToProcess:=GetFileSize;

    // enough bytes for a header?
    if (qBytesToProcess < MIN_HEADER_SIZE) then begin
      raise EBFAFileUnknownFormat.Create(m_sr.Get(STRRES_ID, '088'))
    end
    else begin
      // clear the header storage
      FillChar(vheader, SizeOf(vheader), 0);

      // try to read the header
      nRead:=ReadBytes(@vheader, MIN_HEADER_SIZE);
      if ((nRead = MIN_HEADER_SIZE) and
          (vheader.lMagic = HEADER_MAGIC)) then begin

        // can we handle it?
        if (vheader.bLowestVer > LOWEST_VERSION) then
          raise EBFAFileUnsupportedVersion.Create(m_sr.Get(STRRES_ID, '041'));

        // skip unsupported header data, i.n.
        if (SizeOf(TVirtualHeader) < vheader.bSizeOf) then begin
          nBytesToRead:=vheader.bSizeOf - SizeOf(TVirtualHeader);
          if (ReadBytes(m_pIOBuffer, nBytesToRead) < nBytesToRead) then
            raise EBFAFileDamagedHeader.Create(m_sr.Get(STRRES_ID, '049'));
        end;

        // get the compression state
        case vheader.bCompressCode of
          BFAFILE_COMPRESS_NONE : blCompressed:=False;
          BFAFILE_COMPRESS_LZSS : blCompressed:=True;
          BFAFILE_COMPRESS_DEFL : blCompressed:=True;
          BFAFILE_COMPRESS_BZP2 : blCompressed:=True;
        else
          raise EBFAFileUnknownCompression.Create(m_sr.Get(STRRES_ID, '042'));
        end;

        // calculate the size of the compressed data, which is =
        // bytes in crypt file - header size - size of file name - CRC32
        if (blCompressed) then begin
          Dec(qBytesToProcess,
              vheader.bSizeOf + (vheader.wFileNameLen shl 1) +
              SizeOf(WORD32));
        end
        else begin
          // otherwise get the real file size
          qBytesToProcess:=MakeWORD64(vheader.lFileSizeLo,
                                      vheader.lFileSizeHi);
        end;

        // read out the original file name
        SetLength(wsTemp, vheader.wFileNameLen);
        nBytesToRead:=vheader.wFileNameLen shl 1;
        if (ReadBytes(PWideChar(wsTemp), nBytesToRead) < nBytesToRead) then
          raise EBFAFileDamagedHeader.Create(m_sr.Get(STRRES_ID, '049'));

        // get the ASCII file name
        wsTemp[vheader.wFileNameLen + 1]:=WideChar(0); // (just to be sure)
        WideCharToStrVar(PWideChar(wsTemp), sHeaderFileName);
        sTargetFileName:=sHeaderFileName;
      end
      else
        // (FIXME: is this an i/o or just an "unknown format" error?)
        raise EBFAFileUnknownFormat.Create(m_sr.Get(STRRES_ID, '088'));
    end;

    // we can now return the file information, i. n.
    if (setup.GetFileInfoOnly) then begin
      Close;
      Result:=TBFAFileDecryptResult.Create(m_sr);
      with Result do begin
        SetSourceFileName(sFileName);
        SetWarnings(nWarningBits);
        SetBytesWritten(MakeWORD64(vheader.lFileSizeLo,
                                   vheader.lFileSizeHi));
        SetOriginalFileName(sHeaderFileName);
        // (create direct header information, if necessary)
        if (setup.GetDirectHeaderInfo) then
          SetDirectHeaderInfo(TBFAFileDirectHeaderInfo.Create(
                                sHeaderFileName,
                                GetBytesWritten,
                                vheader.fileTime,
                                vheader.lAttributes,
                                Integer(vheader.bCompressCode) and $00ff));
      end;
      Exit;
    end;

    // create the target file name
    blPathToRestore:=False;

    // cut off the original path, i.n.
    if (not setup.GetRestorePath) then begin
      sTargetFileName:=ExtractFileName(sTargetFileName);
      blPathToRestore:=False;
    end
    else
      if (Pos('\', sTargetFileName) <> 0) then
        blPathToRestore:=True;

    // append to the target path and set the temporay file name
    blUseTempFile:=False;
    if (setup.GetTargetPath <> '') then begin
      // (keep the file's directory, if necessary)
      sTemp:='';
      if (setup.GetKeepDirectories) then begin
        // first make the path relatively to the base path, if necessary
        if (setup.GetBasePath <> '') then
          if (Pos(AnsiUpperCase(setup.GetBasePath),
              AnsiUpperCase(sFileName)) = 1) then begin
            nLen:=Length(setup.GetBasePath);
            sTemp:=TStrPlus.RelativePath(Copy(sFileName,
                                         nLen + 1,
                                         Length(sFileName) - nLen));
          end;
        // now add the path to the target file name
        sTargetFileName:=TStrPlus.RTLPath(
                           TStrPlus.RelativePath(ExtractFilePath(sTemp))) +
                         sTargetFileName;
      end;
      sTargetFileName:=TStrPlus.RTLPath(setup.GetTargetPath) + sTargetFileName;
      sTempFileName:=sTargetFileName;
    end
    else begin
      sTemp:=TStrPlus.RTLPath(ExtractFilePath(sFileName));
      sTargetFileName:=sTemp + sTargetFileName;
      if (AnsiUpperCase(ExtractFileName(sTargetFileName)) =
          AnsiUpperCase(ExtractFileName(sFileName))) then begin
         sTempFileName:=TStrPlus.MakeTempFileName(TStrPlus.PurePath(sTemp),
                                                  TEMPFILENAME_PREFIX);
         blUseTempFile:=True;
      end
      else
        sTempFileName:=sTargetFileName;
    end;

    // check if the target file already exists
    blFileIsThere:=FileExists(sTargetFileName);
    if ((not setup.GetOverwriteExisting) and (not blUseTempFile)) then
      if (blFileIsThere) then
        raise EBFAFileAlreadyExists.Create(Format(m_sr.Get(STRRES_ID, '028'),
                                                  [sTargetFileName]));

    // enable the targetfile to be overwritten
    if (blFileIsThere) then begin
      if (SetFileAttributes(PChar(sTempFileName),
                            FILE_ATTRIBUTE_NORMAL) = FALSE) then begin
        if (GetLastError <> ERROR_FILE_NOT_FOUND) then begin
          if (CloseHandle(fhandle) = FALSE) then
            raise EBFAFileFatal.Create(m_sr.Get(STRRES_ID, '068'));
          raise EBFAFileIOError.Create(m_sr.Get(STRRES_ID, '032'));
        end;
      end;
    end;

    // create (parts of) the targetpath, i.n.
    if (blPathToRestore or setup.GetKeepDirectories) then
      ForceDirectories(ExtractFilePath(sTargetFileName));

    // time to open the target file
    lOpenMask:=0;
    if (setup.GetNoCache) then
      lOpenMask:=FILE_FLAG_WRITE_THROUGH;
    fhandle:=CreateFile(PChar(sTempFileName),
                        GENERIC_WRITE,
                        FILE_SHARE_READ,
                        NULL,
                        CREATE_ALWAYS,
                        lOpenMask,
                        0);
    if (fhandle = INVALID_HANDLE_VALUE) then begin
      case GetLastError of
        ERROR_SHARING_VIOLATION :
          raise EBFAFileAccessDenied.Create(m_sr.Get(STRRES_ID, '048'));
        ERROR_ACCESS_DENIED :
          raise EBFAFileAccessDenied.Create(m_sr.Get(STRRES_ID, '048'));
      else
        raise EBFAFileOpenError.Create(m_sr.Get(STRRES_ID, '045'));
      end;
    end;
    blTargetOpened:=True;
    blTargetExists:=True;

    // set up the callback object
    if (progressCall <> Nil) then
      with progressCall do begin
        SetActPos(0);
        SetMaxPos(qBytesToProcess);
        SetInputFileName(sFileName);
        SetOutputFileName(sTargetFileName);
        SetFileSize(qBytesToProcess);
        SetChanged(True);
        SetProgressState(BFAFILE_PROGRESS_DECRYPT);
      end;

    // set up the decompressor and its buffer, if necessary
    if (blCompressed) then begin
      GetMem(pInflateBuffer, IOBUFFER_SIZE);
      case vheader.bCompressCode of
        BFAFILE_COMPRESS_LZSS:
            decompressor:=TLZSSDecompressor.Create;
        BFAFILE_COMPRESS_DEFL:
            decompressor:=TZLibExDecompressor.Create(ZLIBEX_TYPE_DEFLATE);
        BFAFILE_COMPRESS_BZP2:
            decompressor:=TZLibExDecompressor.Create(ZLIBEX_TYPE_BZIP2);
      end;
    end;

    // set up the checksum
    if (not setup.GetIgnoreCRC32) then
      checksum:=TCRC32.Create;

    // now decrypt the file data
    qBytesWritten:=0;
    blAllRead:=False;
    while ((not blAllRead) and (not blTruncated)) do begin

      // send the actual state
      if (progressCall <> Nil) then
       try
          progressCall.CallBack;
          progressCall.SetProgressState(BFAFILE_PROGRESS_DECRYPT);
        except
          on ECallBackInterrupt do
            raise EBFAFileInterrupted.Create(m_sr.Get(STRRES_ID, '036'));
        end;

      // try to read a whole buffer content
      if (qBytesToProcess >= IOBUFFER_SIZE) then
        nBytesToRead:=IOBUFFER_SIZE
      else
        nBytesToRead:=qBytesToProcess;
      nRead:=ReadBytes(m_pIOBuffer,
                       nBytesToRead);
      if (progressCall <> Nil) then
        progressCall.IncPos(nRead);

      // done?
      Dec(qBytesToProcess, nRead);
      if (qBytesToProcess = 0) then
        blAllRead:=True;

      // write the data to the target file
      if (blCompressed) then begin
        // repeat until all uncompressed data has been written
        repeat
          blRepeat:=decompressor.ProcessData(m_pIOBuffer,
                                             pInflateBuffer,
                                             nRead,
                                             IOBUFFER_SIZE,
                                             nDecompressed);

          // put out the decompressed data
          WriteBytes(pInflateBuffer, nDecompressed);
        until (not blRepeat);
      end
      else begin
        // put out the uncompressed data
        WriteBytes(m_pIOBuffer, nRead);
      end;
    end;

    // put out the rest of the compressed material, i.n.
    if (blCompressed) then begin
      // repeat until all uncompressed data has been written
      repeat
        blRepeat:=decompressor.Finalize(pInflateBuffer,
                                        IOBUFFER_SIZE,
                                        nDecompressed);
        // put out the uncompressed data
        WriteBytes(pInflateBuffer, nDecompressed);
      until (not blRepeat);
    end;

    // send the last progress state
    if (progressCall <> Nil) then
      try
       progressCall.CallBack;
      except
        on ECallBackInterrupt do
          raise EBFAFileInterrupted.Create(m_sr.Get(STRRES_ID, '036'));
      end;

    // read out and check the CRC32, i.n.
    if ((not setup.GetIgnoreCRC32) and (not blTruncated)) then begin
      ReadBytes(@lCRC32, SizeOf(lCRC32));
      if (not blTruncated) then
        if (checksum.Finalize <> lCRC32) then
          raise EBFAFileCRC32Error.Create(m_sr.Get(STRRES_ID, '052'));
    end;

    // set the original time stamp, i.n.
    if (not SetFileTime(fhandle,
                        @vheader.filetime,
                        @vheader.filetime,
                        @vheader.filetime)) then
      nWarningBits:=nWarningBits or BFAFILE_WARNING_SETDATETIMEFAILED;

    // close the target file
    CloseHandle(fhandle);
    blTargetOpened:=False;

    // close the cryptfile
    Close;
    blSourceOpened:=False;

    // remove the crypt file, i.n.
    if (setup.GetRemoveSource or blUseTempFile or
        (setup.GetTargetPath = '')) then begin
      SetFileAttributes(PChar(sFileName), 0);
      if (Windows.DeleteFile(PChar(sFileName)) = FALSE) then
        if (blUseTempFile) then
          raise EBFAFileFatal.Create(m_sr.Get(STRRES_ID, '054'))
        else
          nWarningBits:=nWarningBits or BFAFILE_WARNING_REMOVEFAILED;
    end;
    blSourceRemoved:=True;

    // restore the original filename, i.n.
    if (blUseTempFile) then begin
      // rename it
      if (Windows.MoveFile(PChar(sTempFileName),
                           PChar(sTargetFileName)) = FALSE) then begin

        // kill a maybe existing file with the same name
        SetFileAttributes(PChar(sTargetFileName), 0);
        Windows.DeleteFile(PChar(sTargetFileName));
        // try it again
        if (Windows.MoveFile(PChar(sTempFileName),
                             PChar(sTargetFileName)) = FALSE) then
          raise EBFAFileFatal.Create(m_sr.Get(STRRES_ID, '055'));
      end;
    end;

    // restore the original attribute
    if (SetFileAttributes(PChar(sTargetFileName),
                          vheader.lAttributes) = FALSE) then
      nWarningBits:=nWarningBits or BFAFILE_WARNING_SETATTRFAILED;

  except
    // just one trap is enough here, this will catch all errors (interrupts
    // won't be replaced except of by fata errors which will also lead to a
    // total operation abort)
    on EBFAFileError do begin
      cleanupFatal:=Nil;

      // destroy all primitive objects
      if (decompressor <> Nil) then
        decompressor.Destroy;
      if (pInflateBuffer <> Nil) then
        FreeMem(pInflateBuffer);
      if (checksum <> Nil) then
        checksum.Destroy;

      // close the source file
      if (blSourceOpened) then begin
        try
          Close;
        except
        on EBFAFileError do
          cleanupFatal:=EBFAFileFatal.Create(m_sr.Get(STRRES_ID, '068'));
        end;
      end;

      // close and delete the crashed targetfile, i.n.
      if (blTargetOpened) then
        CloseHandle(fhandle);
      if ((not blSourceRemoved) and (blTargetExists)) then
        if (Windows.DeleteFile(PChar(sTempFileName))= FALSE) then
          cleanupFatal:=EBFAFileFatal.Create(m_sr.Get(STRRES_ID, '056'));

      // rethrow the old/new exception
      if (cleanupFatal <> Nil) then
        raise cleanupFatal
      else
        raise;
    end;
  end;

  // release the checksum and the decompressor, if necessary
  if (Nil <> checksum) then checksum.Destroy;
  if (Nil <> decompressor) then begin
      decompressor.Destroy;
      FreeMem(pInflateBuffer);
  end;

  // build the result object
  Result:=TBFAFileDecryptResult.Create(m_sr);
  with Result do begin
    SetSourceFileName(sFileName);
    SetWarnings(nWarningBits);
    SetBytesWritten(qBytesWritten);
    SetOriginalFileName(sTargetFileName);
    if (setup.GetDirectHeaderInfo) then
      SetDirectHeaderInfo(TBFAFileDirectHeaderInfo.Create(
                            sHeaderFileName,
                            MakeWORD64(vheader.lFileSizeLo,
                                       vheader.lFileSizeHi),
                            vheader.fileTime,
                            vheader.lAttributes,
                            Integer(vheader.bCompressCode) and $00ff));
  end;
end;

end.

