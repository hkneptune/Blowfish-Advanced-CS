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

//////////////////////////////////////////////////////////////////////////////

bool TestCRC32(
	CTestStdOut* tso)
{

	// selftest
	
	tso->Puts((::CRC32_Test()) ? 
		"CRC32 selftest passed" : 
		"CRC32 SELFTEST FAILED!");

	// test zero string

	char zerostring[] = "";

	WORD32 lCRC32 = CRC32_INITVALUE;
	tso->Puts("checksumming zero string...");

	CRC32_Update(lCRC32, zerostring, 0);
	lCRC32 ^= CRC32_DONEVALUE;

	tso->PrintF("CRC32 -> 0x%08x\n", lCRC32);


	// checksumming the reference string

	char refstring[] = "ABCDEFGHIJKLMNOPQRSTUVWXYZäöüÄÖÜ0123456789";

	lCRC32 = CRC32_INITVALUE;
	tso->Puts("checksumming reference string...");

	lCRC32 = ::CRC32_Update(lCRC32, refstring, (WORD32)strlen(refstring));
	lCRC32 ^= CRC32_DONEVALUE;

	tso->PrintF("CRC32 -> 0x%08x\n", lCRC32);

	return true;
}
