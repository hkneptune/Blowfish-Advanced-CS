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

(* interface to BFACSLIB.DLL, plus some helpers *)

unit bfacslib;

interface

///////////////////////////////////////////////////////////////////////////////

const
  BFACSLIB_MODULE = 'bfacslib.dll';

///////////////////////////////////////////////////////////////////////////////

type
  WORD8  = Byte;
  WORD16 = Word;
  WORD32 = Longword;
  WORD64 = Int64;

type
  PWORD8  = ^Byte;
  PWORD16 = ^Word;
  PWORD32 = ^Longword;
  PWORD64 = ^Int64;   

function MakeWORD64(lLoWord : WORD32;
                    lHiWord : WORD32) : WORD64;

function GetWORD64Lo(lValue : WORD64) : WORD32;

function GetWORD64Hi(lValue : WORD64) : WORD32;

const
  NULL = Pointer(0);

type
  BYTEBOOL  = Byte;
  PBYTEBOOL = ^Byte;   
const
  BOOL_FALSE = 0;
  BOOL_TRUE  = 1;


const
  MAXDATA = $7fffffff;
type
  PWORD8Buf  = ^TWORD8Buf;
  PWORD16Buf = ^TWORD16Buf;
  PWORD32Buf = ^TWORD32Buf;

  TWORD8Buf  = array[0..MAXDATA - 1] of WORD8;
  TWORD16Buf = array[0..(MAXDATA div 2) - 1] of WORD16;
  TWORD32Buf = array[0..(MAXDATA div 4) - 1] of WORD32;

///////////////////////////////////////////////////////////////////////////////

const
  CIPHER_NULL = Pointer(0);


const
  CIPHER_IS_XORSTREAM = 1;
  CIPHER_IS_BLOCK     = 2;
  CIPHER_IS_BLOCKLINK = 4;
  CIPHER_IS_NOBLOCK   = 8;
  CIPHER_IS_DEBUG     = 128;


const
  CIPHER_MAX_NAME_LEN = 32;


const
  CIPHER_MODE_ENCRYPT     = 0;
  CIPHER_MODE_DECRYPT     = 1;
  CIPHER_MODE_FLAG_LEGACY = $00010000;

type
  PCIPHERINFOBLOCK = ^TCIPHERINFOBLOCK;
  TCIPHERINFOBLOCK = packed record
    lSizeOf       : WORD32;
    lBlockSize    : WORD32;
    lKeySize      : WORD32;
    lInitDataSize : WORD32;
    lContextSize  : WORD32;
    bCipherIs     : WORD8;
    blOwnHasher   : BYTEBOOL;
  end;

type

TCipher_RandomGenerator = procedure(pTargetBuffer : Pointer;
                                    lNumOfRandomBytes : WORD32;
                                    pData : Pointer); stdcall;

///////////////////////////////////////////////////////////////////////////////

function Support_GetVersion : WORD32; stdcall; external BFACSLIB_MODULE;


const CRUNCHKEY_METHOD_SHAEXTENDER   = 0;
const CRUNCHKEY_METHOD_SHAEXTXORLOOP = 1;


function Support_GetCrunchKeyBuildBufSize(lPasswLen : WORD32;
   			         	  lSaltLen : WORD32;
			    		  lOutputLen : WORD32;
  				    	  bMethod : WORD8) : WORD32;
                                            stdcall; external BFACSLIB_MODULE;

function Support_CrunchKey(pPassw : Pointer;
             	    	   lPasswLen : WORD32;
            			   pSalt : Pointer;
			               lSaltLen : WORD32;
            			   pOutput : Pointer;
			               lOutputLen : WORD32;
                           bMethod : WORD8;
            			   pBuildBuf : Pointer = Nil) : BYTEBOOL;
                             stdcall; external BFACSLIB_MODULE;



const
  MD5_DIGESTSIZE = 16;

type
  PMD5CTX = Pointer;


function MD5_SelfTest : BYTEBOOL; stdcall; external BFACSLIB_MODULE;

function MD5_Create : PMD5CTX; stdcall; external BFACSLIB_MODULE;

procedure MD5_Destroy(pCtx : PMD5CTX); stdcall; external BFACSLIB_MODULE;

procedure MD5_Reset(pCtx : PMD5CTX); stdcall; external BFACSLIB_MODULE;

procedure MD5_Update(pCtx : PMD5CTX;
                     pData : Pointer;
                     lNumOfBytes : WORD32); stdcall; external BFACSLIB_MODULE;

procedure MD5_Final(pDigest : Pointer;
                    pCtx : PMD5CTX); stdcall; external BFACSLIB_MODULE;


const
  SHA1_DIGESTSIZE = 20;

type
  PSHA1CTX = Pointer;


function SHA1_SelfTest : BYTEBOOL; stdcall; external BFACSLIB_MODULE;

function SHA1_Create : PSHA1CTX; stdcall; external BFACSLIB_MODULE;

procedure SHA1_Destroy(pCtx : PSHA1CTX); stdcall; external BFACSLIB_MODULE;

procedure SHA1_Reset(pCtx : PSHA1CTX); stdcall; external BFACSLIB_MODULE;

procedure SHA1_Update(pCtx : PSHA1CTX;
                      pData : Pointer;
                      lNumOfBytes : WORD32); stdcall; external BFACSLIB_MODULE;

procedure SHA1_Final(pDigest : Pointer;
                     pCtx : PSHA1CTX); stdcall; external BFACSLIB_MODULE;


const
  CRC32_INITVALUE = $ffffffff;
  CRC32_DONEVALUE = $ffffffff;

function CRC32_Update(lOldCRC32 : WORD32;
                      pData : Pointer;
                      lNumOfBytes : WORD32) : WORD32;
                        stdcall; external BFACSLIB_MODULE;


type
  PRANDOMPOOLCTX = Pointer;


function RandomPool_Create(pAddSeed : Pointer;
                           lAddSeedLen : WORD32) : PRANDOMPOOLCTX;
                             stdcall; external BFACSLIB_MODULE;

procedure RandomPool_Destroy(pCtx : PRANDOMPOOLCTX);
                               stdcall; external BFACSLIB_MODULE;

procedure RandomPool_Reseed(pCtx : PRANDOMPOOLCTX;
	                        pSeed : pointer;
                            lSeedLen : WORD32);
                              stdcall; external BFACSLIB_MODULE;

procedure RandomPool_GetData(pCtx : PRANDOMPOOLCTX;
            			     pDataTarget : Pointer;
			                 lNumOfBytes : WORD32);
                               stdcall; external BFACSLIB_MODULE;



type
  PYARROWCTX = Pointer;


function Yarrow_Create(pAddSeed : Pointer;
                       lAddSeedLen : WORD32) : PYARROWCTX;
                             stdcall; external BFACSLIB_MODULE;

procedure Yarrow_Destroy(pCtx : PYARROWCTX); stdcall; external BFACSLIB_MODULE;

procedure Yarrow_Reseed(pCtx : PYARROWCTX;
	                    pSeed : pointer;
                        lSeedLen : WORD32); stdcall; external BFACSLIB_MODULE;

procedure Yarrow_GetData(pCtx : PYARROWCTX;
    		    	     pDataTarget : Pointer;
	        		     lNumOfBytes : WORD32);
                               stdcall; external BFACSLIB_MODULE;

const
     LZSS_START = 1;
     LZSS_WORK  = 2;
     LZSS_STOP  = 4;


type
  PLZSSCTX = Pointer;


function LZSS_Create : PLZSSCTX; stdcall; external BFACSLIB_MODULE;

procedure LZSS_Destroy(pCtx : PLZSSCTX);  stdcall; external BFACSLIB_MODULE;

function LZSS_Compress(pCtx : PLZSSCTX;
        		       pSource : Pointer;
	                   pTarget : Pointer;
	                   lNumOfBytes : WORD32;
	                   bCondition : WORD8) : WORD32;
                         stdcall; external BFACSLIB_MODULE;

function LZSS_Decompress(pCtx : PLZSSCTX;
		         pSource : Pointer;
	                 pTarget : Pointer;
	                 lNumOfBytes : WORD32;
	                 lSizeOfOutputBuffer : WORD32;
	                 bCondition : WORD8;
                         pblRepeatMe : PBYTEBOOL) : WORD32;
                           stdcall; external BFACSLIB_MODULE;



const
  CIPHERSERVER_ERROR_NOERROR        = 0;
  CIPHERSERVER_ERROR_ERROR	        = 1;
  CIPHERSERVER_ERROR_INVALIDCIPHER  = 2;
  CIPHERSERVER_ERROR_OUTOFMEMORY	= 3;
  CIPHERSERVER_ERROR_WEAKKEY    	= 4;
  CIPHERSERVER_ERROR_CIPHERNOTFOUND = 5;


const
  CIPHERSERVER_MODE_ENCRYPT     = CIPHER_MODE_ENCRYPT;
  CIPHERSERVER_MODE_DECRYPT     = CIPHER_MODE_DECRYPT;
  CIPHERSERVER_MODE_FLAG_LEGACY = CIPHER_MODE_FLAG_LEGACY;



type
  PCIPHERCTX = Pointer;

type
  PCIPHERSESSION = Pointer;

function CipherServer_GetCipherNames(var ppList : Pointer) : WORD32;
                                       stdcall; external BFACSLIB_MODULE;

function CipherServer_GetCipherInfo(pCipherName : PChar;
	                                pInfoBlock : PCIPHERINFOBLOCK) : WORD32;
                                      stdcall; external BFACSLIB_MODULE;

function CipherServer_Create(
                        pCipherName : PChar;
   	                    var pCtxPtr : PCIPHERCTX;
			            RandGenFunc : TCipher_RandomGenerator = CIPHER_NULL;
        			    pRandGenData : Pointer = CIPHER_NULL;
                        pRandSeed : Pointer = CIPHER_NULL;
                        lRandSeedLen : WORD32 = 0) : WORD32;
                            stdcall; external BFACSLIB_MODULE;

function CipherServer_Destroy(pCtx : PCIPHERCTX) : WORD32;
                                stdcall; external BFACSLIB_MODULE;

function CipherServer_ExecuteSelfTest(pCtx : PCIPHERCTX;
	                              blExtendedTest : BYTEBOOL) : WORD32;
                                        stdcall; external BFACSLIB_MODULE;

function CipherServer_GetInfoBlock(pCtx : PCIPHERCTX;
	                               pInfoBlock : PCIPHERINFOBLOCK) : WORD32;
                                     stdcall; external BFACSLIB_MODULE;

function CipherServer_OpenSession(lMode : WORD32;
                                  pKey : Pointer;
                                  lKeyLen : WORD32;
                                  pCtx : PCIPHERCTX;
                                  pInitData : Pointer;
                                  var pSessionHandle : PCIPHERSESSION) : WORD32;
                                    stdcall; external BFACSLIB_MODULE;

procedure CipherServer_ResetSession (pSessionHandle : PCIPHERSESSION;
                                     pInitData : Pointer); stdcall;
                                       external BFACSLIB_MODULE;

function CipherServer_CloseSession (pSessionHandle : PCIPHERSESSION) : WORD32;
                                      stdcall; external BFACSLIB_MODULE;

procedure CipherServer_EncryptBlocks(pSessionHandle : PCIPHERSESSION;
                  				     pSource : Pointer;
                                     pTarget : Pointer;
                                     lNumOfBlocks : WORD32);
                                       stdcall; external BFACSLIB_MODULE;

procedure CipherServer_DecryptBlocks(pSessionHandle : PCIPHERSESSION;
			                	     pSource : Pointer;
                                     pTarget : Pointer;
                                     lNumOfBlocks : WORD32;
                                     pPreviousBlock : Pointer);
                                       stdcall; external BFACSLIB_MODULE;

procedure CipherServer_GetRandomData(pCtx : PCIPHERCTX;
                                     pTarget : Pointer;
                                     lNumOfBytes : WORD32);
                                       stdcall; external BFACSLIB_MODULE;

///////////////////////////////////////////////////////////////////////////////

const
    BFACSLIB_ERR_UNKNOWN		= -1;
    BFACSLIB_ERR_NOERROR		= 0;
    BFACSLIB_ERR_ERROR			= 1;
    BFACSLIB_ERR_NULLPTR		= 2;
    BFACSLIB_ERR_WRONGPARAM		= 3;
    BFACSLIB_ERR_NOIMPL			= 4;
    BFACSLIB_ERR_OUTOFMEMORY	= 5;
    BFACSLIB_ERR_INTERNAL		= 6;
    BFACSLIB_ERR_ZLIBEX			= 100;
    BFACSLIB_ERR_ZLIBEX_REPEAT	= 101;

///////////////////////////////////////////////////////////////////////////////

const
    ZLIBEX_TYPE_DEFLATE = 0;
    ZLIBEX_TYPE_BZIP2	= 1;

    ZLIBEX_MODE_COMPRESS   = 0;
    ZLIBEX_MODE_DECOMPRESS = 1;

    
type
    PZLIBEXCTX = Pointer;

    
function ZLibEx_Create(
    nMode : Integer;
    nType : Integer;
    nDataSize : Integer;
    var pCtx: PZLIBEXCTX) : Integer; stdcall; external BFACSLIB_MODULE;

function ZLibEx_Destroy(ctx : PZLIBEXCTX) : Integer;
    stdcall; external BFACSLIB_MODULE;

function ZLibEx_Process(
    ctx : PZLIBEXCTX;
    inp : Pointer;
    nInpSize : Integer;
    outp : Pointer;
    nOutpSize : Integer;
    var pnWritten : Integer) : Integer; stdcall; external BFACSLIB_MODULE;

function ZLibEx_Finalize(
    ctx : PZLIBEXCTX;
    outp : Pointer;
    nOutpSize : Integer;
    var pnWritten : Integer) : Integer; stdcall; external BFACSLIB_MODULE;

function ZLibEx_GetLastError(ctx : PZLIBEXCTX; var pnErr : Integer) : Integer;
     stdcall; external BFACSLIB_MODULE;

///////////////////////////////////////////////////////////////////////////////

const
    BASE64_HANDLE_INIT = 0;

function BASE64_CALCOUTP_ENC(n : Integer) : Integer;
function BASE64_CALCOUTP_DEC(n : Integer) : Integer;

function BASE64_Encode(
    var hnd : WORD32;
    inp : Pointer;
    nLen : Integer;
    outp : Pointer;
    nFlush : Integer) : Integer; stdcall; external BFACSLIB_MODULE;

function BASE64_Decode(
    var hnd : WORD32;
    inp : Pointer;
    nLen : Integer;
    outp : Pointer) : Integer; stdcall; external BFACSLIB_MODULE;

///////////////////////////////////////////////////////////////////////////////

implementation

function MakeWORD64(lLoWord : WORD32; lHiWord : WORD32) : WORD64;
begin
  Result:= (WORD64(lHiWord) shl 32) or WORD64(lLoWord);
end;

function GetWORD64Lo(lValue : WORD64) : WORD32;
begin
  Result:=lValue and $0ffffffff;
end;

function GetWORD64Hi(lValue : WORD64) : WORD32;
begin
  Result:=(lValue shr 32) and $0ffffffff;
end;

function BASE64_CALCOUTP_ENC(n : Integer) : Integer;
begin
  Result:=(n div 3 + 2) shl 2;
end;

function BASE64_CALCOUTP_DEC(n : Integer) : Integer;
begin
  Result:=((n shr 2) + 2) * 3;
end;


end.
