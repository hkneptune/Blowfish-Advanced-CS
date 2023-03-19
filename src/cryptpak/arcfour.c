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


#include "arcfour.h"


typedef struct 
{
	WORD32 state[256];
	WORD32 lX, lY;

	// we hold a copy of the initial state after the key setup 
	// for a fast reset
	WORD32 saveState[256];
	WORD32 lSaveX, lSaveY;
} 
ARCFOURCTX;



WORD32 ARCFOUR_GetCipherInfo
(CIPHERINFOBLOCK* pInfo) 
{
	CIPHERINFOBLOCK tempinfo;
	WORD8* pSrc;
	WORD8* pDst;
	WORD32 lI;

	// prepare the information context
	tempinfo.lSizeOf = pInfo->lSizeOf;
	tempinfo.lBlockSize = ARCFOUR_BLOCKSIZE;
	tempinfo.lKeySize = ARCFOUR_KEYSIZE;
	tempinfo.blOwnHasher = BOOL_FALSE;
	tempinfo.lInitDataSize = 0;
	tempinfo.lContextSize = sizeof(ARCFOURCTX);
	tempinfo.bCipherIs = CIPHER_IS_XORSTREAM;

	// copy as many bytes of the information block as possible
	pSrc = (WORD8*) &tempinfo;
	pDst = (WORD8*) pInfo;

	for (lI = 0; lI < tempinfo.lSizeOf; lI++)
		*pDst++ = *pSrc++;

	return CIPHER_ERROR_NOERROR;
}


static WORD8 refKey[5] = { 0x61, 0x8a, 0x63, 0xd2, 0xfb };
static WORD8 ptext[5] = { 0xdc, 0xee, 0x4c, 0xf9, 0x2c };
static WORD8 ctext[5] = { 0xf1, 0x38, 0x29, 0xc9, 0xde };

// (from the original 1994 posting)
static WORD8 refKey2[4] = { 0xef, 0x01, 0x23, 0x45 };
static WORD8 ptext2[10] = { 0x00, 0x00, 0x00, 0x00, 0x00, 
                            0x00, 0x00, 0x00, 0x00, 0x00 };
static WORD8 ctext2[10] = { 0xd6, 0xa1, 0x41, 0xa7, 0xec, 
                            0x3c, 0x38, 0xdf, 0xbd, 0x61 };

WORD32 ARCFOUR_SelfTest(
	void* pTestContext) 
{
	WORD8 etext[sizeof(ctext2)];
	WORD8 dtext[sizeof(ptext2)];

	char ctx[sizeof(ARCFOURCTX)];

	int nI;

	// test the original vector
		
	for (nI = 0; nI < sizeof(etext); nI++)
		etext[nI] = dtext[nI] = 0;

	ARCFOUR_CreateWorkContext(&ctx,	refKey,	sizeof(refKey),
		0, (void*)0, 0, (void*)0);

	ARCFOUR_EncryptBuffer(&ctx,	ptext, etext, 5);

	for (nI = 0; nI < 5; nI++)
		if (ctext[nI] != etext[nI])
			return CIPHER_ERROR_INVALID;

	ARCFOUR_ResetWorkContext(&ctx, 0, (void*)0, 0, (void*)0);

	ARCFOUR_DecryptBuffer(&ctx,	etext, dtext, 5, (void*)0);

	for (nI = 0; nI < 5; nI++)
		if (ptext[nI] != dtext[nI])
			return CIPHER_ERROR_INVALID;

	// now the 1994 reference

	for (nI = 0; nI < sizeof(etext); nI++)
		etext[nI] = dtext[nI] = 0;

	ARCFOUR_CreateWorkContext(&ctx,	refKey2, sizeof(refKey2), 
		0, (void*)0, 0, (void*)0);

	ARCFOUR_EncryptBuffer(&ctx,	ptext2, etext, sizeof(etext));

	for (nI = 0; nI < sizeof(etext); nI++)
		if (ctext2[nI] - etext[nI])
			return CIPHER_ERROR_INVALID;

	ARCFOUR_ResetWorkContext(&ctx, 0, (void*)0, 0, (void*)0);

	ARCFOUR_DecryptBuffer(&ctx,	etext, dtext, sizeof(dtext), (void*)0);

	for (nI = 0; nI < sizeof(dtext); nI++)
		if (ptext2[nI] - dtext[nI])
			return CIPHER_ERROR_INVALID;

	return CIPHER_ERROR_NOERROR;
}


WORD32 ARCFOUR_CreateWorkContext
(void* pContext,
 const WORD8* pKey,
 WORD32 lKeyLen, 
 WORD32 lMode,
 void* pInitData,
 Cipher_RandomGenerator GetRndBytes,
 const void *pRndGenData) 
{
	ARCFOURCTX* pCtx = (ARCFOURCTX*) pContext;
	int nX;
	WORD32 lKeyPos = 0;
	WORD32 lSX, lY = 0;
	WORD32* state = &pCtx->state[0];

	// we don't care about the mode at all and also don't need to change
	// to the new standard mode, since this is a stream cipher

	pCtx->lX = pCtx->lY = 0;
	for (nX = 0; nX < 256; nX++)
		state[nX] = nX;

	for (nX = 0; nX < 256; nX++) 
	{
		lSX = state[nX];
		lY += lSX + pKey[lKeyPos];
		lY &= 0x00ff;
		state[nX] = state[lY];
		state[lY] = lSX;
		if (++lKeyPos == lKeyLen)
			lKeyPos = 0;
	}

	for (nX = 0; nX < 256; nX++)
		pCtx->saveState[nX] = pCtx->state[nX];
	pCtx->lSaveX = pCtx->lX; 
	pCtx->lSaveY = pCtx->lY;

	return CIPHER_ERROR_NOERROR;
}


void ARCFOUR_ResetWorkContext
(void* pContext, 
 WORD32 lMode,
 void* pInitData,
 Cipher_RandomGenerator GetRndBytes,
 const void *pRndGenData) 
{
	ARCFOURCTX* pCtx = (ARCFOURCTX*) pContext;
	int nI;

	// just redo the key setup by copying the orginal state
	for (nI = 0; nI < 256; nI++)
		pCtx->state[nI] = pCtx->saveState[nI];

	pCtx->lX = pCtx->lSaveX; 
	pCtx->lY = pCtx->lSaveY;
}


WORD32 ARCFOUR_DestroyWorkContext 
(void* pContext) 
{
	// clear the context
	WORD8* pClearIt = (WORD8*) pContext;
	int nI;
	for (nI = 0; nI < sizeof(ARCFOURCTX); nI++)
		pClearIt[nI] = 0x00;
	return CIPHER_ERROR_NOERROR;
}



void ARCFOUR_EncryptBuffer
(void* pContext, 
 const void* pSource, 
 void* pTarget,
 WORD32 lNumOfBytes) 
{
	ARCFOURCTX* pCtx = (ARCFOURCTX*) pContext;
	WORD32 lX = pCtx->lX;
	WORD32 lY = pCtx->lY;
	WORD32* state = &pCtx->state[0];
	WORD8* pDataIn = (WORD8*) pSource;
	WORD8* pDataOut = (WORD8*) pTarget;
	WORD32 lSX, lSY;

	while (lNumOfBytes--) 
	{
		lX++;
		lX &= 0x00ff;
		lSX = state[lX];
		lY += lSX;
		lY &= 0x00ff;
		lSY = state[lY];
		state[lY] = lSX;
		state[lX] = lSY;
		*pDataOut++ = *pDataIn++ ^ (WORD8)state[(lSX + lSY) & 0x00ff];
	}

	pCtx->lX = lX;
	pCtx->lY = lY;
}



void ARCFOUR_DecryptBuffer
(void* pContext, 
 const void* pSource, 
 void* pTarget,
 WORD32 lNumOfBytes, 
 const void* pPreviousBlock) 
{
	// just map the call, previous blocks aren't significant
	ARCFOUR_EncryptBuffer(pContext, 
		pSource, 
		pTarget, 
		lNumOfBytes);
}
