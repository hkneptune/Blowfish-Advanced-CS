/*
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
 */


#include "TMS.h"


typedef struct 
{
  // just one silly dummy word
  WORD32 lDummy;
} 
TMSCTX;



WORD32 TMS_GetCipherInfo
  (CIPHERINFOBLOCK* pInfo) 
{
  WORD8* pSrc;
  WORD8* pDst;
  CIPHERINFOBLOCK tempinfo;
  WORD32 lI;

  // prepare the information context
  tempinfo.lSizeOf = pInfo->lSizeOf;
  tempinfo.lBlockSize = TMS_BLOCKSIZE;
  tempinfo.lKeySize = TMS_KEYSIZE;
  tempinfo.blOwnHasher = BOOL_FALSE;
  tempinfo.lInitDataSize = 0;
  tempinfo.lContextSize = sizeof(TMSCTX);
  tempinfo.bCipherIs = CIPHER_IS_BLOCK | CIPHER_IS_DEBUG;

  // copy as many bytes of the information block as possible
  pSrc = (WORD8*) &tempinfo;
  pDst = (WORD8*) pInfo;
  for (lI = 0; lI < tempinfo.lSizeOf; lI++)
    *pDst++ = *pSrc++;
  return CIPHER_ERROR_NOERROR;
}


WORD32 TMS_SelfTest 
  (void* pTestContext) 
{
  // no selftest implemented until now,
  // so we assume a proper implementation
  return CIPHER_ERROR_NOERROR;
}


WORD32 TMS_CreateWorkContext
  (void* pContext,
   const WORD8* pKey,
   WORD32 lKeyLen, 
   WORD32 lMode,
   void* pInitData,
   Cipher_RandomGenerator GetRndBytes,
   const void *pRndGenData) 
{
  // nothing to setup
  return CIPHER_ERROR_NOERROR;
}


void TMS_ResetWorkContext
  (void* pContext, 
   WORD32 lMode,
   void* pInitData,
   Cipher_RandomGenerator GetRndBytes,
   const void *pRndGenData) 
{
  // nothing to reset
}


WORD32 TMS_DestroyWorkContext 
  (void* pContext) 
{
  // nothing to destroy 
  return CIPHER_ERROR_NOERROR;
}



void TMS_EncryptBuffer
  (void* pContext, 
   const void* pSource,
   void* pTarget,
   WORD32 lNumOfBytes) 
{
  WORD8* pDataIn = (WORD8*) pSource;
  WORD8* pDataOut = (WORD8*) pTarget;

  // just copy the bytes
  while (lNumOfBytes--) 
    *pDataOut++ = *pDataIn++;
}



void TMS_DecryptBuffer
  (void* pContext, 
   const void* pSource, 
   void* pTarget,
   WORD32 lNumOfBytes, 
   const void* pPreviousBlock) 
{
  // even simpler...
  TMS_EncryptBuffer(pContext, 
                    pSource, 
                    pTarget, 
                    lNumOfBytes);
}
