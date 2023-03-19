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
#include <memory.h>

#include "TestUtils.h"

//////////////////////////////////////////////////////////////////////////////

void FillBuffer(
	char* buf,
	int nLen,
	int nFillStyle)
{
	int nI;

	switch (nFillStyle)
	{
		case FILLBUF_STYLE_ZERO:
				::memset(buf, 0, nLen); 
				break;

		case FILLBUF_STYLE_WORD: 
				for (nI = 0; nI < nLen; nI++)
				{
					buf[nI] = "abcdefgh"[nI & 7];
				}
				break;
		
		case FILLBUF_STYLE_ALLBYTES: 
				for (nI = 0; nI < nLen; nI++)
				{
					buf[nI] = (char)nI;
				}
				break;
					
		default:	// FILLBUF_STYLE_RANDOM
			for (nI = 0; nI < nLen; nI++)
			{
				buf[nI] = (char)::rand();
			}
	}
}

//////////////////////////////////////////////////////////////////////////////

int CompareFiles(
	const char* path0, 
	const char* path1)
{
	int nRead0, nRead1, nResult;
	char buf0[1024];
	char buf1[sizeof(buf0)];
	FILE *fl0, *fl1;

	if (NULL == (fl0 = ::fopen(path0, "rb")))
	{
		return -1;
	}
	if (NULL == (fl1 = ::fopen(path1, "rb")))
	{
		::fclose(fl0);
		return -1;
	}
	
	for (;;)
	{
		nRead0 = (int)::fread(buf0, 1, sizeof(buf0), fl0);
		nRead1 = (int)::fread(buf1, 1, sizeof(buf1), fl1);

		if (nRead0 - nRead1)
		{
			nResult = (ferror(fl0) || ferror(fl1)) ? -1 : 1;
			break;
		}

		if (0 == nRead0)
		{
			nResult = (ferror(fl0) || ferror(fl1)) ? -1 : 0;
			break;
		}

		if (::memcmp(buf0, buf1, nRead0))
		{
			nResult = 1;
			break;
		}
	}

	if (::fclose(fl0)) nResult = -1;
	if (::fclose(fl1)) nResult = -1;

	return nResult;
}

//////////////////////////////////////////////////////////////////////////////

void PrintHexBuf(
	CTestStdOut* tso,
	WORD8* buf,
	int nNumOfBytes)
{
	for (int nI = 0; nI < nNumOfBytes; nI++)
	{
		tso->PrintF("%02x ", buf[nI]);
	}
}

