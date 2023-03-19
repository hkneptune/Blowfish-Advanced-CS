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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "TestUtils.h"

#define BUFSIZE     1024
#define KEYSIZE     47

#define NO_USER_INPUT

//////////////////////////////////////////////////////////////////////////////

bool TestCrunchKeyCommon(
	CTestStdOut* tso)
{
	bool blResult;
	char passw[BUFSIZE];
	char salt[BUFSIZE];
	WORD8 key[KEYSIZE];
	WORD8 key2[sizeof(key)];

#ifdef NO_USER_INPUT
	::strcpy(passw, "the password.");
	::strcpy(salt, "salt!salt!salt!");
#else
	tso->PrintF("password>");
	::gets(passw);
	tso->PrintF("salt>");
	::gets(salt);
#endif

	WORD32 lPasswLen = (WORD32)::strlen(passw);
	WORD32 lSaltLen = (WORD32)::strlen(salt);

	::Support_CrunchKey(passw,
		lPasswLen,
		salt,
		lSaltLen,
		key,
		sizeof(key),
		CRUNCHKEY_METHOD_SHAEXTXORLOOP,
		NULL);

	PrintHexBuf(tso, key, KEYSIZE);
	tso->Puts("");

	WORD32 lBuildBufLen =
		::Support_GetCrunchKeyBuildBufSize(lPasswLen,
			lSaltLen,
			KEYSIZE,
			CRUNCHKEY_METHOD_SHAEXTXORLOOP);

	void* pBuildBuf = ::malloc(lBuildBufLen);

	blResult = (BOOL_TRUE == ::Support_CrunchKey(
		passw,
		lPasswLen,
		salt,
		lSaltLen,
		key2,
		sizeof(key2),
		CRUNCHKEY_METHOD_SHAEXTXORLOOP,
		pBuildBuf));

    if (blResult)		
	{
		::PrintHexBuf(tso, key, KEYSIZE);
		tso->Puts("");
	}

	::free(pBuildBuf);

	if (blResult)
	{
		return (0 == memcmp(key, key2, sizeof(key)));
	}
	else
	{
		return blResult;
	}
}

//////////////////////////////////////////////////////////////////////////////

#define CRUNCH_METHODS_COUNT	4

static int CRUNCH_METHODS[CRUNCH_METHODS_COUNT] =
{
	CRUNCHKEY_METHOD_SHAEXTENDER,
	CRUNCHKEY_METHOD_SHAEXTXORLOOP,
	CRUNCHKEY_METHOD_SHA512_1K,
	CRUNCHKEY_METHOD_SHA512_100K
};

static WORD8 REF_PASSW[] = { 't', 'e', 's', 't', '\0', '\t', '\t' };
static WORD8 REF_SALT[] = { 0xee, 0x01, 0x00, 0xab, 0xba, 0xff };

// (use reference keys to maintain compatibility)
#define REF_KEYSIZE		29

static WORD8 REF_KEYS[CRUNCH_METHODS_COUNT][REF_KEYSIZE] = {
{ 0xb6,0x4f,0x99,0x8f,0xef,0x1a,0x30,0x22,0x49,0x8c,0xdd,0xb8,0xe3,0x6e,0xee,
  0x87,0x6e,0x3e,0xde,0xb1,0x2e,0x6f,0x0c,0x6a,0x67,0x35,0x19,0xb3,0x07 },
{ 0x45,0x88,0xed,0x9e,0x78,0xd8,0x75,0x4d,0xa5,0xaf,0x5d,0xb8,0xe3,0x6e,0xee,
  0x87,0x6e,0x3e,0xde,0xb1,0x2e,0x6f,0x0c,0x6a,0x67,0x35,0x19,0xb3,0x07 },
{ 0xcb,0x9f,0x0e,0x2e,0x12,0x76,0xc4,0x0d,0x5e,0xba,0x7d,0x09,0x3b,0x01,0xaf,
  0x4c,0x91,0x69,0xca,0x2c,0x63,0x81,0xed,0x11,0xa7,0xe4,0xac,0x13,0xdb },
{ 0xc7,0x19,0xd4,0x4f,0xe6,0x8c,0x84,0xbf,0xf3,0x15,0x66,0x18,0xd9,0xcb,0x6d,
  0xf5,0x67,0x50,0xa7,0x13,0x5f,0x74,0xa7,0x51,0xc6,0x8f,0xc7,0x8f,0x23 }
};

bool TestCrunchKeyCompatibility(
	CTestStdOut* tso)
{
	int nMethodIdx;
	WORD8 key[REF_KEYSIZE];

	for (nMethodIdx = 0; nMethodIdx < CRUNCH_METHODS_COUNT;	nMethodIdx++)
	{
		if (::Support_CrunchKey(
			REF_PASSW,
			sizeof(REF_PASSW),
			REF_SALT,
			sizeof(REF_SALT),
			key,
			sizeof(key),
			CRUNCH_METHODS[nMethodIdx],
			NULL))
		{
			::PrintHexBuf(tso, key, sizeof(key));
			tso->Puts("");

			if (memcmp(key, REF_KEYS[nMethodIdx], sizeof(key)))
			{
				return false;
			}
		}
		else
		{
			return false;
		}
	}

	return true;
}

//////////////////////////////////////////////////////////////////////////////

bool TestCrunchKey(
	CTestStdOut* tso)
{
	if (::TestCrunchKeyCommon(tso))
	if (::TestCrunchKeyCompatibility(tso))
	{
		return true;
	}
	return false;
}

//////////////////////////////////////////////////////////////////////////////

#define B64TST_BUFSIZES_C	7
int B64TST_BUFSIZES[B64TST_BUFSIZES_C] = { 0, 1, 2, 3, 4, 6, 100 };

bool TestBASE64(
	CTestStdOut* tso)
{
#define ERROR_EXIT	{ ::free(raw); ::free(enc); ::free(dec); return false; }

	int nBufSize, nBufSizeIdx, nFillStyle, nHalf;
	int nEnc, nDec, nC;
	WORD32 hnd;
	char* raw;
	char* enc;
	char* dec;


	for (nBufSizeIdx = 1; nBufSizeIdx < B64TST_BUFSIZES_C; nBufSizeIdx++)
	for (nFillStyle = 1; nFillStyle <= FILLBUF_NUMOFSTYLES; nFillStyle++)
	{
		nBufSize = B64TST_BUFSIZES[nBufSizeIdx];

		raw = (char*)::malloc(nBufSize);
		enc = (char*)::calloc(BASE64_CALCOUTP_ENC(nBufSize) + 1, 1);
		
		::FillBuffer(raw, nBufSize, nFillStyle);

		hnd = BASE64_HANDLE_INIT;
		nHalf = nBufSize >> 1;

		nEnc = ::BASE64_Encode(&hnd, raw, nHalf, enc, 0);
		nEnc += ::BASE64_Encode(&hnd, &raw[nHalf], nBufSize - nHalf, &enc[nEnc], 1);
		
		enc[nEnc] = '\0';

		::puts(enc);

		dec = (char*)::calloc(BASE64_CALCOUTP_DEC(nEnc), 1);

		hnd = BASE64_HANDLE_INIT;
		nHalf = nEnc >> 1;

		nC = ::BASE64_Decode(&hnd, enc, nHalf, dec);
		if (-1 == nC) 
		{
			::puts("error decoding 1st halve");
			ERROR_EXIT
		}
		nDec = nC;

		nC = ::BASE64_Decode(&hnd, &enc[nHalf], nEnc - nHalf, &dec[nDec]);
		if (-1 == nC) 
		{
			::puts("error decoding 2nd halve");
			ERROR_EXIT
		}
		nDec += nC;

		if (nDec != nBufSize)
		{
			::printf("%d <> %d\n", nDec, nBufSize);
			ERROR_EXIT
		}

		if (::memcmp(dec, raw, nBufSize))
		{	
			::puts("data mismatch!");
			ERROR_EXIT
		}

		::free(raw);
		::free(enc);
		::free(dec);
	}

	return true;

#undef ERROR_EXIT
}


