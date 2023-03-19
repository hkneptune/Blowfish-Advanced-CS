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


#ifndef __TESTINTERFACE_H
#define __TESTINTERFACE_H


#include "CryptPak.h"


// generic output interface

class CTestStdOut
{
public:

  virtual void Puts(const char*) = 0;
  virtual void PrintF(const char*, ...) = 0;
};

//////////////////////////////////////////////////////////////////////////////

// prototype of all test functions

bool TestCipherServer(CTestStdOut*);
bool TestCRC32		 (CTestStdOut*);
bool TestCrunchKey	 (CTestStdOut*);
bool TestBASE64		 (CTestStdOut*);
bool TestLZSS		 (CTestStdOut*, const char*);
bool TestYarrow		 (CTestStdOut*);
bool TestMD5		 (CTestStdOut*, const char*);
bool TestSHA1		 (CTestStdOut*, const char*);
bool TestSHA512		 (CTestStdOut*, const char*);
bool TestZLibEx		 (CTestStdOut*);

#endif
