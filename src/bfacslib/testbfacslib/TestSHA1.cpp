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

// reference string
static char _sha1_refstr[] = "ABCDEFG - Marshmallows for you and me!";

//////////////////////////////////////////////////////////////////////////////

void PrintSHA1Digest
(CTestStdOut* tso,
 WORD8* dg)
{
	tso->PrintF("%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x"	\
		"%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x\n",
		dg[ 0], dg[ 1], dg[ 2], dg[ 3], dg[ 4],
		dg[ 5], dg[ 6], dg[ 7], dg[ 8], dg[ 9],
		dg[10], dg[11], dg[12], dg[13], dg[14],
		dg[15], dg[16], dg[17], dg[18], dg[19]);
}

//////////////////////////////////////////////////////////////////////////////

bool TestSHA1(
	CTestStdOut* tso,
	const char* fileName)
{
	PSHA1CTX ctx;
	WORD8 dg[SHA1_DIGESTSIZE];

	// run selftest

	tso->PrintF("running SHA-1 selftest...");

	if (BOOL_FALSE == ::SHA1_SelfTest())
	{
		tso->Puts(", FAILED");
		return false;
	}
	tso->Puts(", done.");

	tso->Puts("testing SHA-1 routines..." );

	// hash reference string

	ctx = ::SHA1_Create();

	::SHA1_Update(ctx, _sha1_refstr, (WORD32)::strlen(_sha1_refstr));

	::SHA1_Final(dg, ctx);

	tso->PrintF("reference string: ");
	::PrintSHA1Digest(tso, dg);
	tso->Puts("");

	// hash nothing

	::SHA1_Reset(ctx);
	::SHA1_Final(dg, ctx);

	tso->PrintF("zero: ");
	::PrintSHA1Digest(tso, dg);
	tso->Puts("");

	// hash test file

	FILE* binfile;
	WORD8 testbuf[BUFSIZE];

	if (NULL == (binfile = ::fopen(fileName, "rb")))
	{
		tso->PrintF("cannot open \"%s\" for reading.\n", fileName);
		::SHA1_Destroy(ctx);
		return false;
	}

	::SHA1_Reset(ctx);

	WORD32 lRead;

	do
	{
		lRead = (WORD32)::fread(testbuf, 1, BUFSIZE, binfile);

		::SHA1_Update(ctx, testbuf, lRead);

		tso->PrintF("#");
	}
	while (BUFSIZE == lRead);

	::fclose(binfile);
	tso->Puts("");

	::SHA1_Final(dg, ctx);

	tso->PrintF("hash of \"%s\": ", fileName);
	::PrintSHA1Digest(tso, dg);
	tso->Puts("");

	::SHA1_Destroy(ctx);

	return true;
}
