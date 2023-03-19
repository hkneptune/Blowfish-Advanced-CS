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


#include "Support.h"
#include "BasicTypes.h"
#include "SHA1.h"
#include "SHA512.h"
#include "cpkernel.h"

#include <stdlib.h>
#include <memory.h>

//////////////////////////////////////////////////////////////////////////////

// the recent version of CryptPak
#define VERSION_MAJOR   5
#define VERSION_MINOR   0
#define VERSION_BUILT   0

//////////////////////////////////////////////////////////////////////////////

WORD32 CRYPTPAK_API
Support_GetVersion() 
{
	return (((WORD32) VERSION_MAJOR) << 24) |
		   (((WORD32) VERSION_MINOR) << 16) |
		    ((WORD32) VERSION_BUILT);
}

//////////////////////////////////////////////////////////////////////////////

WORD32 CRYPTPAK_API Support_GetCrunchKeyBuildBufSize(
	WORD32 lPasswLen,
	WORD32 lSaltLen,
	WORD32 lOutputLen,
	WORD8 bMethod) 
{
	switch (bMethod)
	{
		case CRUNCHKEY_METHOD_SHA512_1K:
		case CRUNCHKEY_METHOD_SHA512_100K:
			return lSaltLen + lPasswLen + SHA512_DIGESTSIZE;
		default:
			return lSaltLen + lPasswLen +       
				SHA1_DIGESTSIZE * (lOutputLen / SHA1_DIGESTSIZE + 1);
	}
}

//////////////////////////////////////////////////////////////////////////////

BYTEBOOL CRYPTPAK_API Support_CrunchKey(
	const void* pPassw, 
	WORD32 lPasswLen, 
	const void* pSalt, 
	WORD32 lSaltLen, 
	void* pOutput, 
	WORD32 lOutputLen,
	WORD8 bMethod,
	void* pBuildBuf) 
{
	int nI, nC;
	WORD8* outBuf = (WORD8*) pOutput;
	WORD8* buildBuf;
	WORD32 lBuildBufLen;
	WORD32 lBuildBufPos;
	WORD32 lRest, lToCopy;
	WORD32 lOutPos;
	BYTEBOOL blFirst = BOOL_TRUE;
	SHA512CTX ctx512;
	SHA1CTX ctx;
	WORD8 digest[SHA1_DIGESTSIZE];

	if (lOutputLen == 0) 
	{
		return BOOL_TRUE;
	}

	if (pSalt == NULL) 
	{
		lSaltLen = 0; 
	}

	// create the build buffer, if necessary

	lBuildBufLen = Support_GetCrunchKeyBuildBufSize(lPasswLen, 
		lSaltLen,
		lOutputLen, 
		bMethod);

	if (NULL == pBuildBuf)
	{
		buildBuf = (WORD8*) malloc(lBuildBufLen);
		if (buildBuf == NULL) 
		{
			return BOOL_FALSE;   
		}
	}
	else  
	{
		buildBuf = (WORD8*) pBuildBuf;
	}

	lOutPos = 0;
	lRest = lOutputLen;

	if ((CRUNCHKEY_METHOD_SHA512_1K == bMethod) ||
	    (CRUNCHKEY_METHOD_SHA512_100K == bMethod))
	{
		// initialize hash and copy password and salt to the right position
		lBuildBufLen = 0;
		
		memset(&buildBuf[lBuildBufLen], 0, SHA512_DIGESTSIZE);
		lBuildBufLen += SHA512_DIGESTSIZE;

		memcpy(&buildBuf[lBuildBufLen], pPassw, lPasswLen);
		lBuildBufLen += lPasswLen;

		if (pSalt)
		{
			memcpy(&buildBuf[lBuildBufLen], pSalt, lSaltLen);
			lBuildBufLen += lSaltLen;
		}

		// process hash, password and salt many, many times
		SHA512_Initialize(&ctx512);
		nC = (CRUNCHKEY_METHOD_SHA512_1K == bMethod) ? 1000 : 100000;
		for (nI = 0; nI < nC; nI++)
		{
			SHA512_Update(&ctx512, buildBuf, lBuildBufLen);
			SHA512_Final(&ctx512, buildBuf);
			SHA512_Reset(&ctx512);
		}
		memset(&ctx512, 0, sizeof(ctx512));
		
		// now put out the key, repeat the process above to generate more data
		// if we don't have enough material (to make the whole key look random)
		while (lRest)
		{
			lToCopy = (lRest > SHA512_DIGESTSIZE) ? SHA512_DIGESTSIZE : lRest;
			
			memcpy(&outBuf[lOutPos], buildBuf, lToCopy);

			if (lRest -= lToCopy)
			{
				lOutPos += lToCopy;
				SHA512_Update(&ctx512, buildBuf, lBuildBufLen);
				SHA512_Final(&ctx512, buildBuf);
				SHA512_Reset(&ctx512);
			}
		}
	}
	else
	{
		// concat salt and key
		if (pSalt != NULL) 
		{
			memcpy(&buildBuf[0], 
				pSalt, 
				lSaltLen);
		}
		memcpy(&buildBuf[lSaltLen], 
			pPassw, 
			lPasswLen);
		lBuildBufPos = lSaltLen + lPasswLen;

		SHA1_Initialize(&ctx);

		for (;;)
		{
			if (blFirst == BOOL_TRUE) 
			{
				blFirst = BOOL_FALSE;
			}
			else 
			{
				SHA1_Reset(&ctx);
			}

			SHA1_Update(&ctx, buildBuf, lBuildBufPos);
			SHA1_Final(digest, &ctx);

			// finished?
			if (lRest <= SHA1_DIGESTSIZE) 
			{
				memcpy(&outBuf[lOutPos], digest, lRest);

				// xor the rest of the last digest over the beginning of the
				// output buffer?
				if (bMethod == CRUNCHKEY_METHOD_SHAEXTXORLOOP) 
				{
					lOutPos = 0;

					while (lRest < SHA1_DIGESTSIZE) 
					{
						outBuf[lOutPos++] ^= digest[lRest++];
						if (lOutPos == lOutputLen) 
						{
							lOutPos = 0;
						}
					}
				}
				break; // leave do..while
			}
			else 
			{
				memcpy(&outBuf[lOutPos], digest, SHA1_DIGESTSIZE);
				lOutPos += SHA1_DIGESTSIZE; 

				// append the new digest to the build buffer
				memcpy(&buildBuf[lBuildBufPos], digest, SHA1_DIGESTSIZE);
				memset(digest, 0, SHA1_DIGESTSIZE);
				lBuildBufPos += SHA1_DIGESTSIZE;

				lRest -= SHA1_DIGESTSIZE;
			}
		}
		memset(&ctx, 0, sizeof(ctx));
	}

	memset(buildBuf, 0, lBuildBufLen);

	if (NULL == pBuildBuf) 
	{
		free(buildBuf);
	}

	return BOOL_TRUE;
}

//////////////////////////////////////////////////////////////////////////////

static char* BASE64_TAB =			\
	"ABCDEFGHIJKLMNOPQRSTUVWXYZ"	\
	"abcdefghijklmnopqrstuvwxyz"	\
	"0123456789"					\
	"+/";

#define BASE64_FILLCHAR		'='

// the last byte in the handle word contains the number of bytes in the handle
#define BASE64_COUNT_OFS	3

//////////////////////////////////////////////////////////////////////////////

int CRYPTPAK_API BASE64_Encode(
	WORD32* plHandle, 
	const void* pData, 
	int nInpSize, 
	char* outp, 
	int blFlush)
{
	int nOfs;
	const WORD8* inp;
	char* outpStart;
	WORD8 hnd[4];
	WORD32 handle;

	inp = (const WORD8*)pData;
	outpStart = outp;

	handle = *plHandle;
	WORD32_TO_BYTES(handle, hnd)

	nOfs = hnd[BASE64_COUNT_OFS];

	while (nInpSize)
	{
		hnd[nOfs++] = *inp++;
		nInpSize--;

		if (3 == nOfs)
		{
			*outp++ = BASE64_TAB[hnd[0] >> 2];
			*outp++ = BASE64_TAB[((hnd[0] & 0x03) << 4) | (hnd[1] >> 4)];
			*outp++ = BASE64_TAB[((hnd[1] & 0x0f) << 2) | (hnd[2] >> 6)];
			*outp++ = BASE64_TAB[hnd[2] & 0x3f];
			nOfs = 0;
		}
	}
	
	if (blFlush)
	{
		if (0 < nOfs)
		{
			hnd[(1 == nOfs) ? 1 : 2] = 0;

			*outp++ = BASE64_TAB[hnd[0] >> 2];
			*outp++ = BASE64_TAB[((hnd[0] & 0x03) << 4) | (hnd[1] >> 4)];

			if (1 == nOfs)
			{
				*outp++ = BASE64_FILLCHAR;
			}
			else
			{
				*outp++ = BASE64_TAB[((hnd[1] & 0x0f) << 2) | (hnd[2] >> 6)];
			}
			*outp++ = BASE64_FILLCHAR;
		}
	}
	else
	{
		hnd[BASE64_COUNT_OFS] = (WORD8)nOfs;
		*plHandle = BYTES_TO_WORD32(hnd);
	}

	return (int)(outp - outpStart);
}

//////////////////////////////////////////////////////////////////////////////

int BASE64_Lookup(
	WORD8 c)
{
	     if ('A' <= c  && 'Z' >= c)	return  c - 'A';	
	else if	('a' <= c  && 'z' >= c)	return (c - 'a') + 26;	
	else if ('0' <= c  && '9' >= c)	return (c - '0') + 52;	
	else if ('+' == c)				return 62;	
	else if ('/' == c)				return 63;
	else							return -1;
}

int CRYPTPAK_API BASE64_Decode(
	WORD32* plHandle,
	const char* inp,
	int nInpSize, 
	void* pData)
{
	char c;
	int nOfs, nRest, nHH, nHL, nLH, nLL;
	WORD32 handle;
	WORD8 hnd[4];
	WORD8 *outp, *outpStart;

	outpStart = outp = (WORD8*)pData;

	handle = *plHandle;
	WORD32_TO_BYTES(handle, hnd)

	// determine how many characters we have cached
	nOfs = hnd[BASE64_COUNT_OFS];

	nRest = 3;		// (3 = "no fill character detected yet")

	while (nInpSize)
	{
		// fill character?
		if (BASE64_FILLCHAR == (c = *inp++))
		{
			switch (nOfs)
			{
				case 2:	
					nRest = 1; 
					break;
				case 3:	
					if (3 == nRest) nRest = 2; 
					break; 
				default: 
					return -1;
			}
		}
		else
		{
			if (3 == nRest)
			{
				hnd[nOfs] = (WORD8)c;
			}
			else
			{
				return -1;	// nothing else allowed after the fillchar
			}
		}

		if (4 == ++nOfs)
		{
			// get the necessary four 6bit values 
						   if (-1 == (nHH = BASE64_Lookup(hnd[0]))) return -1;
						   if (-1 == (nHL = BASE64_Lookup(hnd[1]))) return -1;
			if (1 < nRest) if (-1 == (nLH = BASE64_Lookup(hnd[2]))) return -1;
			if (2 < nRest) if (-1 == (nLL = BASE64_Lookup(hnd[3]))) return -1;

			// reassemble up to three bytes
			               *outp++ = (WORD8)(( nHH         << 2) | (nHL >> 4));
			if (1 < nRest) *outp++ = (WORD8)(((nHL & 0x0f) << 4) | (nLH >> 2));
			if (2 < nRest) *outp++ = (WORD8)(((nLH & 0x03) << 6) |  nLL);

			nOfs = 0;
		}

		nInpSize--;
	}

	if (3 == nRest)
	{
		// (counter might have been overwritten)
		hnd[BASE64_COUNT_OFS] = (WORD8)nOfs;  

		*plHandle = BYTES_TO_WORD32(hnd);
	}

	return (int)(outp - outpStart);
}
