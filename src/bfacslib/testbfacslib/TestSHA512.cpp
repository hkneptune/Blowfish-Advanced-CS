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
#include <string.h>

#include "TestInterface.h"

#define BUFSIZE 10000

//////////////////////////////////////////////////////////////////////////////

void PrintSHA512Digest
(CTestStdOut* tso,
 WORD8* dg)
{
	for (int nI = 0; nI < SHA512_DIGESTSIZE; nI++)
	{
		tso->PrintF("%02x", dg[nI]);
	}
	tso->PrintF("\n");
}

//////////////////////////////////////////////////////////////////////////////

static WORD8 WORD64_SAMPLE_BYTES[] = { 0xaa,1,2,3,252,253,254,255 };
static WORD64 WORD64_SAMPLE = 0xaa010203fcfdfeff;

bool TestBasicTypesWORD64Macros()
{
	WORD8 buf[sizeof(WORD64)];
	
	if (WORD64_SAMPLE != BYTES_TO_WORD64(WORD64_SAMPLE_BYTES))
	{
		return false;
	}
	
	memset(buf, 0, sizeof(buf));
	WORD64_TO_BYTES(WORD64_SAMPLE, buf);

	if (memcmp(buf, WORD64_SAMPLE_BYTES, sizeof(buf)))
	{
		return false;
	}

	return true;
}	

//////////////////////////////////////////////////////////////////////////////

static char TENAS[] = { 'a','a','a','a','a','a','a','a','a','a' };
#define TENAS_LOOPS		100000
WORD8 TENAS_DIGEST[] = 
{
	0xe7,0x18,0x48,0x3d,0x0c,0xe7,0x69,0x64,0x4e,0x2e,0x42,0xc7,0xbc,0x15,
	0xb4,0x63,0x8e,0x1f,0x98,0xb1,0x3b,0x20,0x44,0x28,0x56,0x32,0xa8,0x03,
	0xaf,0xa9,0x73,0xeb,0xde,0x0f,0xf2,0x44,0x87,0x7e,0xa6,0x0a,0x4c,0xb0,
	0x43,0x2c,0xe5,0x77,0xc3,0x1b,0xeb,0x00,0x9c,0x5c,0x2c,0x49,0xaa,0x2e,
	0x4e,0xad,0xb2,0x17,0xad,0x8c,0xc0,0x9b
};

bool TestSHA512(
	CTestStdOut* tso,
	const char* fileName)
{
	int nI, nJ;
	PSHA512CTX ctx;
	WORD8 dg[SHA512_DIGESTSIZE];

	if (!TestBasicTypesWORD64Macros())
	{
		tso->PrintF("BasicTypes WORD64 macros are broken");
		return false;
	}

	tso->PrintF("running SHA-512 selftest...");

	if (BOOL_FALSE == ::SHA512_SelfTest())
	{
		tso->Puts(", FAILED");
		return false;
	}
	tso->Puts(", done.");

	tso->Puts("testing SHA-512 routines..." );

	ctx = ::SHA512_Create();

	for (nJ = 0; nJ < 2; nJ++)
	{
		for (nI = 0; nI < TENAS_LOOPS; nI++)
		{
			::SHA512_Update(ctx, TENAS, sizeof(TENAS));
		}

		::SHA512_Final(ctx, dg);

		if (memcmp(dg, TENAS_DIGEST, sizeof(TENAS_DIGEST)))
		{
			tso->PrintF("100kxTenas test failed in round %d\n", nI + 1);
			::SHA512_Destroy(ctx);
			return false;
		}
		::SHA512_Reset(ctx);
	}

	FILE* binfile;
	WORD8 testbuf[BUFSIZE];

	if (NULL == (binfile = ::fopen(fileName, "rb")))
	{
		tso->PrintF("cannot open \"%s\" for reading.\n", fileName);
		::SHA512_Destroy(ctx);
		return false;
	}

	::SHA512_Reset(ctx);

	WORD32 lRead;

	do
	{
		lRead = (WORD32)::fread(testbuf, 1, BUFSIZE, binfile);

		::SHA512_Update(ctx, testbuf, lRead);

		tso->PrintF("#");
	}
	while (BUFSIZE == lRead);

	::fclose(binfile);
	tso->Puts("");

	::SHA512_Final(ctx, dg);

	tso->PrintF("hash of \"%s\": ", fileName);
	::PrintSHA512Digest(tso, dg);
	tso->Puts("");

	::SHA512_Destroy(ctx);

	return true;
}
