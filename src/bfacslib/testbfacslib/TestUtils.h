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

#ifndef __TESTUTILS_H
#define __TESTUTILS_H

#include "TestInterface.h"

#define FILLBUF_STYLE_ZERO		0
#define FILLBUF_STYLE_WORD		1
#define FILLBUF_STYLE_ALLBYTES	2
#define FILLBUF_STYLE_RANDOM	3

#define FILLBUF_NUMOFSTYLES		(FILLBUF_STYLE_RANDOM + 1)

// -> buffer
// -> length
// -> style (see FILLBUF_STYLE_xxx)
void FillBuffer(char*, int, int);

//////////////////////////////////////////////////////////////////////////////

// -> first file's path
// -> second file's path
// <- 1: files are equal / 0: not equal / -1: error occured
int CompareFiles(const char*, const char*);

//////////////////////////////////////////////////////////////////////////////

// -> output handle
// -> data buffer
// -> number of bytes to print
void PrintHexBuf(CTestStdOut*, WORD8*, int);

#endif
