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

#define BUFSIZE 12345

// reference string
static char _md5_refstr[] = "ABCDEFG - Marshmallows for you and me!";

//////////////////////////////////////////////////////////////////////////////

void PrintMD5Digest
(CTestStdOut* tso,
 WORD8* dg)  
{
	tso->PrintF("%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x\n",
		dg[ 0], dg[ 1],dg[ 2],dg[ 3],dg[ 4],dg[ 5],dg[ 6],dg[ 7],
		dg[ 8], dg[ 9],dg[10],dg[11],dg[12],dg[13],dg[14],dg[15]);
}

//////////////////////////////////////////////////////////////////////////////

bool TestMD5
(CTestStdOut* tso,
 const char* fileName) 
{
	PMD5CTX ctx;
	WORD8 dg[MD5_DIGESTSIZE];

	// selftest

	tso->PrintF("running selftest...");

	if (BOOL_FALSE == ::MD5_SelfTest()) 
	{
		tso->Puts(", FAILED");
		return false;
	}
	tso->Puts(", done.");

	tso->Puts("testing MD5 routines..." );

	ctx = ::MD5_Create();

	// hash reference string

	::MD5_Reset(ctx);

	::MD5_Update(ctx, _md5_refstr, (WORD32)::strlen(_md5_refstr));

	::MD5_Final(dg, ctx);

	tso->PrintF("reference string: ");
	PrintMD5Digest(tso, dg);
	tso->Puts("");

	// hash nothing

	::MD5_Reset(ctx);

	::MD5_Final(dg, ctx);

	tso->PrintF("zero: ");
	PrintMD5Digest(tso, dg);
	tso->Puts("");

	// hash test file

	FILE* binfile;
	WORD8 testbuf[BUFSIZE]; 

	if (NULL == (binfile = ::fopen(fileName, "rb"))) 
	{
		tso->PrintF("cannot open \"%s\" for reading.\n", fileName);
		::MD5_Destroy(ctx);
		return false;
	}

	::MD5_Reset(ctx);

	WORD32 lRead;

	do 
	{
		lRead = (WORD32)::fread(testbuf, 1, BUFSIZE, binfile);

		::MD5_Update(ctx, testbuf, lRead);

		tso->PrintF("#");
	}
	while (BUFSIZE == lRead);

	::fclose(binfile);
	tso->Puts("");

	::MD5_Final(dg, ctx);

	tso->PrintF("\ndigest of \"%s\": ", fileName);
	::PrintMD5Digest(tso, dg);
	tso->Puts("");

	::MD5_Destroy(ctx);

	return true;
}
