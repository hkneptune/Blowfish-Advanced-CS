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

#include <windows.h>

#include <stdio.h>
#include <stdarg.h>
#include <time.h>

#include "TestInterface.h"

//////////////////////////////////////////////////////////////////////////////

#define AUTOTESTFILE

#define TESTFILE	"test.dat"

// (undef this to use your own test file)
#ifdef AUTOTESTFILE

char _testFile[MAX_PATH] = { '\0' };

bool MakeTestFile();
bool MakeTestFile()
{
	::GetTempPath(sizeof(_testFile), _testFile);
	::strcat(_testFile, TESTFILE);

	FILE* tf = ::fopen(_testFile, "wb");
	if (tf)
	{
		::fputs(
			"//////////////////////////////////\n"	\
			"Something for CryptPak to test.\n"		\
			"//////////////////////////////////", 
			tf);
		::fclose(tf);
		return true;
	}
	else
	{
		::printf("cannot create test file \"%s\"\n", _testFile);
		return false;
	}
}

#endif

//////////////////////////////////////////////////////////////////////////////

// simple mapper to stdout

class CTestStdOutImpl : public CTestStdOut
{
public:

	virtual void Puts 
		(const char* txt) 
	{ 
		::puts(txt);
	}

	virtual void PrintF
		(const char* fmt, ...)
	{ 
		va_list lst;
		va_start(lst, fmt);
		::vprintf(fmt, lst);
		va_end(lst);
	}
};

//////////////////////////////////////////////////////////////////////////////

// (all the test together)

void main()
{
	CTestStdOutImpl tsoi;
	clock_t clk;

#ifdef AUTOTESTFILE
	if (!::MakeTestFile())
	{
		return;
	}
#endif

	clk = ::clock();

	if (::TestCipherServer(&tsoi))
	if (::TestCRC32       (&tsoi))
	if (::TestCrunchKey   (&tsoi))
	if (::TestBASE64      (&tsoi))
	if (::TestMD5         (&tsoi, _testFile))
	if (::TestSHA1        (&tsoi, _testFile))
	if (::TestYarrow      (&tsoi))
	if (::TestLZSS        (&tsoi, _testFile))
	if (::TestSHA512      (&tsoi, _testFile))
	if (::TestZLibEx      (&tsoi))
	{
		clk = ::clock() - clk;
		::printf("%d.%03d seconds\n", 
			clk / (clock_t)CLOCKS_PER_SEC,
			((clk % (clock_t)CLOCKS_PER_SEC) * 1000) / (clock_t)CLOCKS_PER_SEC);

		::puts("\n++++all tests succeeded++++");
		return;
	}
	::puts("\n----TESTS FAILED----");
}
