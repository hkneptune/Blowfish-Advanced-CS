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
#include <memory.h>

#include "TestInterface.h"


#define TESTBUFSIZE 1024

bool TestYarrow
(CTestStdOut* tso) 
{
	int nI;

	PYARROWCTX rctx1, rctx2;

	WORD8 testbuf[TESTBUFSIZE];

	char as[] = "This is not important, but just confusing...";

	tso->Puts("random, the first...");

	rctx1 = Yarrow_Create(as, sizeof(as));

	Yarrow_GetData(rctx1, testbuf, TESTBUFSIZE);

	nI = 0;
	while (nI < TESTBUFSIZE)
	{
		tso->PrintF("%02x", testbuf[nI++]);
		if (0 == (nI % 40)) tso->Puts("");
	}
	tso->Puts("\n");

	tso->Puts("random, the second...");

	rctx2 = Yarrow_Create(NULL, 0);

	Yarrow_GetData(rctx2, testbuf, TESTBUFSIZE);

	nI = 0;
	while (nI < TESTBUFSIZE)
	{
		tso->PrintF("%02x", testbuf[nI++]);
		if (0 == (nI % 40)) tso->Puts("");
	}
	tso->Puts("\n");

	Yarrow_Destroy(rctx2);
	Yarrow_Destroy(rctx1);

	return true;
}

