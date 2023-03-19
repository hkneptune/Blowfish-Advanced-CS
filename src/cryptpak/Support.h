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


#ifndef __SUPPORT_H
#define __SUPPORT_H

#include "BasicTypes.h"
#include "cpconfig.h"


#ifdef __cplusplus
extern "C" {
#endif


/*
 * returns the version of CRYPTPAK.DLL
 * <- version number (low word: built number, high word: minor version
 *    in low byte, major version in high byte), valid in the
 *    xx.xx.xxx format
 */
WORD32 CRYPTPAK_API Support_GetVersion();

//////////////////////////////////////////////////////////////////////////////

/* 
 * key crunching methods
 */
#define CRUNCHKEY_METHOD_SHAEXTENDER    0
#define CRUNCHKEY_METHOD_SHAEXTXORLOOP  1
#define CRUNCHKEY_METHOD_SHA512_1K		2
#define CRUNCHKEY_METHOD_SHA512_100K	3


/*
 * calculates the necessary size of a buld buffer, which pointer
 * can be passed to Support_CrunchKey()
 * -> number of password bytes used
 * -> number of salt bytes
 * -> demanded key size
 * -> crunch method (see CRUNCHKEY_METHOD_xxx) 
 * <- size of the build buffer
 */
WORD32 CRYPTPAK_API Support_GetCrunchKeyBuildBufSize(WORD32, WORD32, WORD32, WORD8);


/*
 * hashes a (salt and) password down to a defined key size
 * -> pointer to password
 * -> number of password bytes used
 * -> pointer to salt bytes (may be NULL)
 * -> number of salt bytes
 * -> pointer to output buffer
 * -> demanded key size
 * -> chrunch method (see CRUNCHKEY_METHOD_xxx) 
 * -> pointer to a build buffer with the size determined by 
 *    Support_GetChrunchKeyBuildBufSize(), may be NULL
 * <- BOOL_TRUE: success / BOOL_FALSE: out of memory
 */
BYTEBOOL CRYPTPAK_API Support_CrunchKey(const void*, WORD32, const void*, WORD32, 
                                        void*, WORD32, WORD8, void*);

//////////////////////////////////////////////////////////////////////////////

// (inital handle value)
#define BASE64_HANDLE_INIT			((WORD32)0)

// calculate the sizes a safe output buffer must have based on N input bytes
#define BASE64_CALCOUTP_ENC(n)		(((n) / 3 + 2) << 2) 
#define BASE64_CALCOUTP_DEC(n)		((((n) >> 2) + 2) * 3)

// Performs BASE64 encoding.
// -> pointer to handle (BASE64_HANDLE_INIT on the first call)
// -> input buffer
// -> size of input data
// -> output buffer
// -> 1: flush the data afterwards / 0: continue
// <- number of characters written
int CRYPTPAK_API BASE64_Encode(WORD32*, const void*, int, char*, int);


// Performs BASE64 decoding.
// -> pointer to handle (BASE64_HANDLE_INIT on the first call)
// -> input buffer
// -> size of input data
// -> output buffer
// <- number of bytes written (-1 on error)
int CRYPTPAK_API BASE64_Decode(WORD32*, const char*, int, void*);


#ifdef __cplusplus
}
#endif


#endif
