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
  support class encapsulating the cipher server implemented in bfacslib.dll
}

unit CipherServer;


interface
uses
    SysUtils,
    Classes,
    bfacslib,
    RandomSource,
    ErrorCode;

// error codes
const
  // (these are taken just 1:1 from bfacslib)
  ECDE_NOERROR        = CIPHERSERVER_ERROR_NOERROR;
  ECDE_ERROR	      = CIPHERSERVER_ERROR_ERROR;
  ECDE_CIPHERNOTFOUND = CIPHERSERVER_ERROR_CIPHERNOTFOUND;
  ECDE_INVALIDCIPHER  = CIPHERSERVER_ERROR_INVALIDCIPHER;
  ECDE_OUTOFMEMORY	  = CIPHERSERVER_ERROR_OUTOFMEMORY;
  ECDE_WEAKKEY    	  = CIPHERSERVER_ERROR_WEAKKEY;
  // (these are new ones)
  ECDE_SELFTESTFAILED   = 1000;
  ECDE_SESSIONACTIVE    = 1001;
  ECDE_NOSESSION   	    = 1002;
  ECDE_WRONGSESSIONTYPE = 1003;



// cipher exception type
type
  ECipherError = class(EErrorCode);




// class type receiving the cipher information block
type
  TCipherInfo = class
  private
    // the cipher information block
    m_infoblock : TCIPHERINFOBLOCK;

  public
    // gets a cipher information block from a cipher
    // -> cipher name
    // exception: ECipherError with error codes
    // ECDE_ERROR	       - unloading error
    // ECDE_CIPHERNOTFOUND - cipher couldn't be found
    // ECDE_INVALIDCIPHER  - cipher isn't valid
    constructor Create(const sCipherName : String); overload;

    // sets an information block directly, title is passed additionally,
    // can be called even when a cipher is not present in memory
    // -> pointer information block
    constructor Create(pInfoBlock : PCIPHERINFOBLOCK); overload;

    // returns the block size of the cipher
    // <- block size of the cipher
    function GetBlockSize : Integer;

    // returns the key size of the cipher
    // <- key size of the cipher
    function GetKeySize : Integer;

    // returns the own hashing flag
    // <- True: cipher hashes the key own its own (key size can be anything but
    //    zero) / False: cipher doesn't (key size must fit)
    function IsOwnHasher : Boolean;

    // returns the size of the init. data area of the cipher
    // <- size of the init. data area of the cipher
    function GetInitDataSize : Integer;

    // returns the cipher's work context size
    // <- cipher's work context size
    function GetContextSize : Integer;

    // returns the size of the cipher information block
    // <- size of information
    function GetSizeOf : Integer;

    // returns the cipher type
    // <- cipher type, see CIPHER_IS_xxx above
    function GetCipherIs : Integer;
  end;


// class type for cipher handling
type
  TCipher = class
  private
    // cipher context
    m_pCtx : PCIPHERCTX;

    // session handle
    m_pCSHandle : PCIPHERSESSION;

    // random source instance
    m_rs : TRandomSource;

    // session type code
    m_nSessionType : Integer;

  public
    // opens a cipher and init. the random generator
    // -> name of the cipher to open
    // -> random source reference
    // -> pointer to random seed data
    // -> number of random seed bytes
    // exception: ECipherError with error codes
    // ECDE_OUTOFMEMORY        - not enough memory
    // ECDE_ERROR              - loading error
    // ECDE_CIPHERNOTFOUND     - cipher could not be found
    // ECDE_INVALIDCIPHER      - cipher is not valid
    constructor Create(const sCipherName : String;
                       setrs : TRandomSource = Nil;
                       pRandSeed : Pointer = Nil;
                       nRandSeedLen : Integer = 0);

    // frees cipher (closes an open session, if necessary)
    // exception: ECipherError with error code
    // ECDE_ERROR - fatal error
    destructor Destroy; override;

    // executes the selftest(s)
    // -> executes the extended selftest (if True)
    // exception: ECipherError with error codes
    // ECDE_INVALIDCIPHER - cipher is not valid
    // ECDE_OUTOFMEMORY   - not enough memory
    procedure ExecuteSelfTest(blExtended : Boolean = False);

    // gets the cipher information block (instance only valid on success)
    // <- cipher information keeper
    // exception: ECipherError with error codes
    // ECDE_ERROR         - unknown error code (invalid cipher)
    // ECDE_INVALIDCIPHER - cipher declares itself as invalid
    function GetInfoBlock : TCipherInfo;

    // opens a new encryption session
    // -> pointer to key material
    // -> number of key bytes
    // -> pointer where to copy the init.data
    // exception: ECipherError with error codes
    // ECDE_ERROR         - key setup failed
    // ECDE_OUTOFMEMORY   - not enough memory
    // ECDE_WEAKKEY       - weak key detected
    // ECDE_SESSIONACTIVE - another session is already running
    procedure OpenEncryptionSession(pKey : Pointer;
                                    nKeyLen : Integer;
                                    pInitData : Pointer);

    // opens a new decryption session
    // -> pointer to key material
    // -> number of key bytes
    // -> pointer where to retrieve the init.data
    // exception: ECipherError with error codes
    // ECDE_ERROR         - key setup failed
    // ECDE_OUTOFMEMORY   - not enough memory
    // ECDE_WEAKKEY       - weak key detected
    // ECDE_SESSIONACTIVE - another session is already running
    procedure OpenDecryptionSession(pKey : Pointer;
                                    nKeyLen : Integer;
                                    pInitData : Pointer);

    // resets a session
    // -> pointer where to copy/retrieve the init.data
    // exception: ECipherError with error code
    // ECDE_NOSESSION - no session active
    procedure ResetSession(pInitData : Pointer);

    // closes a session
    // -> pointer where to copy/retrieve the init.data
    // exception: ECipherError with error codes
    // ECDE_ERROR     - fatal error
    // ECDE_NOSESSION - no session active
    procedure CloseSession;

    // encrypts data
    // -> pointer to plaintext buffer
    // -> pointer to ciphertext buffer (may be equal to pInData)
    // -> number of blocks to encrypt
    // exception: ECipherError with error codes
    // ECDE_NOSESSION        - no session active
    // ECDE_WRONGSESSIONTYPE - decryption session active
    procedure EncryptBlocks(pInData : Pointer;
                            pOutData : Pointer;
                            nNumOfBlocks : Integer);

    // decrypts data
    // -> pointer to ciphertext buffer
    // -> pointer to plaintext buffer (may be equal to pInData)
    // -> number of blocks to decrypt
    // exception: ECipherError with error codes
    // ECDE_NOSESSION        - no session active
    // ECDE_WRONGSESSIONTYPE - encryption session active
    procedure DecryptBlocks(pInData : Pointer;
                            pOutData : Pointer;
                            nNumOfBlocks : Integer);

    // decrypts data in insertion mode (previous bock is given)
    // -> pointer to ciphertext buffer
    // -> pointer to plaintext buffer (may be equal to pInData)
    // -> number of blocks to decrypt
    // -> pointer to previous block (usually containing
    //    the CBC IV, if the cipher links the blocks)
    // exception: ECipherError with error codes
    // ECDE_NOSESSION        - no session active
    // ECDE_WRONGSESSIONTYPE - encryption session active
    procedure DecryptBlocksInsert(pInData : Pointer;
                                  pOutData : Pointer;
                                  nNumOfBlocks : Integer;
                                  pPreviousBlock : Pointer);

    // returns random data directly from the generator used in bfacs.dll (or
    // its called reference)
    // -> pointer where to write the random bytes
    // -> number of bytes to create
    // exception: ECipherError with error codes
    // ECDE_NOSESSION - no session active
    procedure GetRandomData(pTarget : Pointer;
                            nNumOfBytes: Integer);

    // creates a list with the names of all cipher available
    // -> True: ignore debug ciphers / False: include them
    // <- name list (destroyed by the caller)
    class function GetCipherNames(blIgnoreDebug : Boolean = True) : TStringList;
  end;



implementation
uses General;
   

// our internal random generator callback function
procedure RandomGenerator(pTargetBuffer : Pointer;
                          lNumOfRandomBytes : WORD32;
                          pData : Pointer); stdcall;
begin
  // just map the call
  TRandomSource(pData).GetBytes(pTargetBuffer, lNumOfRandomBytes);
end;




//////////////////// TCipherInfo ////////////////////


constructor TCipherInfo.Create(const sCipherName : String);
var
  lErrorCode   : WORD32;
begin
  lErrorCode:=CipherServer_GetCipherInfo(PChar(sCipherName),
                                         @m_infoblock);
  if (lErrorCode <> CIPHERSERVER_ERROR_NOERROR) then begin
    raise ECipherError.Create(lErrorCode);
  end;
end;

constructor TCipherInfo.Create(pInfoBlock : PCIPHERINFOBLOCK);
begin
  m_infoblock:=pInfoBlock^;
end;

function TCipherInfo.GetBlockSize : Integer;
begin
  Result:=m_infoblock.lBlockSize;
end;

function TCipherInfo.GetKeySize : Integer;
begin
  Result:=m_infoblock.lKeySize;
end;

function TCipherInfo.IsOwnHasher : Boolean;
begin
  Result:=(m_infoblock.blOwnHasher = BOOL_TRUE);
end;

function TCipherInfo.GetInitDataSize : Integer;
begin
  Result:=m_infoblock.lInitDataSize;
end;

function TCipherInfo.GetContextSize : Integer;
begin
  Result:=m_infoblock.lContextSize;
end;

function TCipherInfo.GetSizeOf : Integer;
begin
  Result:=m_infoblock.lSizeOf;
end;

function TCipherInfo.GetCipherIs : Integer;
begin
  Result:=m_infoblock.bCipherIs;
end;


//////////////////// TCipher ////////////////////


// session type codes
const
  SESSION_NONE    = 0;
  SESSION_ENCRYPT = 1;
  SESSION_DECRYPT = 2;



constructor TCipher.Create(const sCipherName : String;
                           setrs : TRandomSource;
                           pRandSeed : Pointer;
                           nRandSeedLen : Integer);
var
  lErrorCode : WORD32;
begin
  // no session active right now
  m_nSessionType:=SESSION_NONE;

  // try to open the cipher
  m_pCtx:=NULL;
  if (setrs <> Nil) then
  begin
    lErrorCode:=CipherServer_Create(PChar(sCipherName),
                                    m_pCtx,
                                    RandomGenerator,
                                    setrs,
                                    CIPHER_NULL,
                                    0);
    // (store the random source reference, although it isn't reference anymore
    //  from here it must stay alive as long as the instance is existant)
    m_rs:=setrs;
  end
  else
  begin
    lErrorCode:=CipherServer_Create(PChar(sCipherName),
                                    m_pCtx,
                                    CIPHER_NULL,
                                    CIPHER_NULL,
                                    pRandSeed,
                                    nRandSeedLen);
  end;                                    

  if (lErrorCode <> CIPHERSERVER_ERROR_NOERROR) then
    // (destructor is automatically called and will destroy rndpool then)
    raise ECipherError.Create(lErrorCode);
end;


destructor TCipher.Destroy;
var
  lErrorCode : WORD32;
begin
  // session open?
  if (m_nSessionType <> SESSION_NONE) then begin

    // close this session
    lErrorCode:=CipherServer_CloseSession(m_pCSHandle);
    if (lErrorCode <> CIPHERSERVER_ERROR_NOERROR) then begin

      // (on error try to free the cipher anyway)
      CipherServer_Destroy(m_pCtx);
      raise ECipherError.Create(lErrorCode);
    end;
  end;

  // free the cipher (if necssary)
  if (m_pCtx <> NULL) then begin
    lErrorCode:=CipherServer_Destroy(m_pCtx);
    if (lErrorCode <> CIPHERSERVER_ERROR_NOERROR) then
      raise ECipherError.Create(lErrorCode);
  end;
end;


procedure TCipher.ExecuteSelfTest(blExtended : Boolean);
var
  lErrorCode : WORD32;
begin
  if (blExtended) then
    lErrorCode:=CipherServer_ExecuteSelfTest(m_pCtx, BOOL_TRUE)
  else
    lErrorCode:=CipherServer_ExecuteSelfTest(m_pCtx, BOOL_FALSE);

  if (lErrorCode <> CIPHERSERVER_ERROR_NOERROR) then
    raise ECipherError.Create(lErrorCode);
end;


function TCipher.GetInfoBlock : TCipherInfo;
var
  lErrorCode : WORD32;
  infoblock  : TCIPHERINFOBLOCK;
begin
  lErrorCode:=CipherServer_GetInfoBlock(m_pCtx, @infoblock);

  if (lErrorCode <> CIPHERSERVER_ERROR_NOERROR) then begin
    raise ECipherError.Create(lErrorCode);
  end;

  Result:=TCipherInfo.Create(@infoblock);
end;


procedure TCipher.OpenEncryptionSession(pKey : Pointer;
                                        nKeyLen : Integer;
                                        pInitData : Pointer);
var
  lErrorCode : WORD32;
  lMode : WORD32;
begin
  // session already active?
  if (m_nSessionType <> SESSION_NONE) then
    raise ECipherError.Create(ECDE_SESSIONACTIVE);

  // set the (legacy) mode
  lMode := CIPHERSERVER_MODE_ENCRYPT or CIPHERSERVER_MODE_FLAG_LEGACY;

  // open the encryption session
  lErrorCode:=CipherServer_OpenSession(lMode,
                                       pKey,
                                       nKeyLen,
                                       m_pCtx,
                                       pInitData,
                                       m_pCSHandle);
  if (lErrorCode <> CIPHERSERVER_ERROR_NOERROR) then
    raise ECipherError.Create(lErrorCode);

  // session type if now encryption
  m_nSessionType:=SESSION_ENCRYPT;
end;


procedure TCipher.OpenDecryptionSession(pKey : Pointer;
                                        nKeyLen : Integer;
                                        pInitData : Pointer);
var
  lMode : WORD32;
  lErrorCode : WORD32;
begin
  // session already active?
  if (m_nSessionType <> SESSION_NONE) then
    raise ECipherError.Create(ECDE_SESSIONACTIVE);

  // set the (legacy) mode
  lMode := CIPHERSERVER_MODE_DECRYPT or CIPHER_MODE_FLAG_LEGACY;

  // open the decryption session
  lErrorCode:=CipherServer_OpenSession(lMode,
                                       pKey,
                                       nKeyLen,
                                       m_pCtx,
                                       pInitData,
                                       m_pCSHandle);
  if (lErrorCode <> CIPHERSERVER_ERROR_NOERROR) then
    raise ECipherError.Create(lErrorCode);

  // session type if now encryption
  m_nSessionType:=SESSION_DECRYPT;
end;


procedure TCipher.ResetSession(pInitData : Pointer);
begin
  // session active?
  if (m_nSessionType = SESSION_NONE) then
    raise ECipherError.Create(ECDE_NOSESSION);

  // reset the session
  CipherServer_ResetSession(m_pCSHandle, pInitData);
end;


procedure TCipher.CloseSession;
var
  lErrorCode : WORD32;
begin
  // session active?
  if (m_nSessionType = SESSION_NONE) then
    raise ECipherError.Create(ECDE_NOSESSION);

  lErrorCode:=CipherServer_CloseSession(m_pCSHandle);
  if (lErrorCode <> CIPHERSERVER_ERROR_NOERROR) then
    raise ECipherError.Create(lErrorCode);

  // no session now
  m_nSessionType:=SESSION_NONE;
end;


procedure TCipher.EncryptBlocks(pInData : Pointer;
                                     pOutData : Pointer;
                                     nNumOfBlocks : Integer);
begin
  // session active?
  if (m_nSessionType = SESSION_NONE) then
    raise ECipherError.Create(ECDE_NOSESSION);

  // decryption session active?
  if (m_nSessionType = SESSION_NONE) then
    raise ECipherError.Create(ECDE_WRONGSESSIONTYPE);

  // encrypt the blocks
  CipherServer_EncryptBlocks(m_pCSHandle,
                             pInData,
                             pOutData,
                             nNumOfBlocks);
end;



procedure TCipher.DecryptBlocks(pInData : Pointer;
                                     pOutData : Pointer;
                                     nNumOfBlocks : Integer);
begin
  // session active?
  if (m_nSessionType = SESSION_NONE) then
    raise ECipherError.Create(ECDE_NOSESSION);

  // decryption session active?
  if (m_nSessionType = SESSION_NONE) then
    raise ECipherError.Create(ECDE_WRONGSESSIONTYPE);

  // encrypt the blocks
  CipherServer_DecryptBlocks(m_pCSHandle,
                             pInData,
                             pOutData,
                             nNumOfBlocks,
                             CIPHER_NULL);
end;


procedure TCipher.DecryptBlocksInsert(pInData : Pointer;
                                           pOutData : Pointer;
                                           nNumOfBlocks : Integer;
                                           pPreviousBlock : Pointer);
begin
  // session active?
  if (m_nSessionType = SESSION_NONE) then
    raise ECipherError.Create(ECDE_NOSESSION);

  // decryption session active?
  if (m_nSessionType = SESSION_NONE) then
    raise ECipherError.Create(ECDE_WRONGSESSIONTYPE);

  // encrypt the blocks
  CipherServer_DecryptBlocks(m_pCSHandle,
                             pInData,
                             pOutData,
                             nNumOfBlocks,
                             pPreviousBlock);
end;


procedure TCipher.GetRandomData(pTarget : Pointer;
                                     nNumOfBytes: Integer);
begin
  // just map the call
  CipherServer_GetRandomData(m_pCtx, pTarget, nNumOfBytes);
end;


class function TCipher.GetCipherNames(blIgnoreDebug : Boolean = True)
                                        : TStringList;
type
  PPChar = ^PChar;
var
  nI     : Integer;
  nCount : Integer;
  sTemp  : String;
  ppList : PPChar;
  cinfo  : TCipherInfo;
begin
  nCount:=CipherServer_GetCipherNames(Pointer(ppList));

  Result:=TStringList.Create;

  // (ignore debug ciphers)
  nI:=0;
  while (nI < nCount) do begin

    sTemp:=String(ppList^);

    cinfo:=TCipherInfo.Create(sTemp);
    if ((cinfo.GetCipherIs and CIPHER_IS_DEBUG) = CIPHER_IS_DEBUG) then begin
      if (not blIgnoreDebug) then Result.Add(sTemp);
    end
    else begin
      Result.Add(sTemp);
    end;
    cinfo.Destroy;

    ppList:=Pointer(Integer(ppList) + SizeOf(PChar));  // (weird? works!)

    Inc(nI);
  end;

end;



end.
