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

#include "bfacslib.h"

#ifdef __cplusplus
extern "C" {
#endif

// simple wrapper to make the zlib functionality fit into bfaCS

#ifndef ZLIBEX_H
#define ZLIBEX_H

//////////////////////////////////////////////////////////////////////////////

// compressor types
#define ZLIBEX_TYPE_DEFLATE		0
#define ZLIBEX_TYPE_BZIP2		1

// operation modes
#define ZLIBEX_MODE_COMPRESS	0
#define ZLIBEX_MODE_DECOMPRESS	1

//////////////////////////////////////////////////////////////////////////////

// anonymous handle
typedef struct ZLIBEXCTX  ZLIBEXCTX, *PZLIBEXCTX;  

//////////////////////////////////////////////////////////////////////////////

// Creates a new (de)compression instance.
// -> mode (see ZLIBEX_MODE_xxx)
// -> type (see ZLIBEX_TYPE_xxx)
// -> estimated total size of the data (-1 if unknown, or larger than 2^31)
// -> where to put the handle (on success)
// <- error code (see BFACSLIB_ERR_xxx)
int BFACSLIB_API ZLibEx_Create(int, int, int, PZLIBEXCTX*);

// Release an instance.
// -> instance to release
// <- error code (see BFACSLIB_ERR_xxx)
int BFACSLIB_API ZLibEx_Destroy(PZLIBEXCTX);

//////////////////////////////////////////////////////////////////////////////

// (De)compresses data. The call must be repeated until all of the data of the
// input buffer has been consumed. On the 2nd+ call the input data and the
// size does not matter, yet the buffer data still must be valid and the
// pointer to it must not be NULL.
// -> instance handle
// -> input buffer
// -> number of bytes to decompress
// -> output buffer (size must be just greater than zero)
// -> size of the output buffer
// -> where to put the number of bytes (de)compressed; if 0 is returned it
//    means that all of the data has been consumed and the cycle is over; -1
//    is the same put represents the end of the stream itself (no more data
//    follows)
// <- error code (see BFACSLIB_ERR_xxx)
int BFACSLIB_API ZLibEx_Process(PZLIBEXCTX, 
	const char*, int, char*, int, int*);

//////////////////////////////////////////////////////////////////////////////

// To be called at the end of every (de)compression process. Might have to be
// called multiple times. After the last cycle the instance is invalid and
// should be destroyed.
// -> instance handle
// -> output buffer
// -> size of the output buffer
// -> number of bytes retrieved; if 0 is returned it means that the processing
//    is done and the function shall not be called anymore
// <- error code (see BFACSLIB_ERR_xxx)
int BFACSLIB_API ZLibEx_Finalize(PZLIBEXCTX, char*, int, int*);

//////////////////////////////////////////////////////////////////////////////

// Gets the last error, meaning depends on the active internal compression
// engine, and is supposed to be used for debugging purposes only when a
// BFACSLIB_ERR_ZLIBEX_xxx error happened.
// -> instance handle
// -> where to put the error code (on success)
// <- error code (see BFACSLIB_ERR_xxx)
int BFACSLIB_API ZLibEx_GetLastError(PZLIBEXCTX, int*);


#ifdef __cplusplus
}
#endif

#endif
