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

#ifndef __ARCFOUR_H
#define __ARCFOUR_H


#ifdef __cplusplus
extern "C" {
#endif


#include "CipherDef.h"


// ARCFOUR characteristics
#define ARCFOUR_BLOCKSIZE   1
#define ARCFOUR_KEYSIZE     20
#define ARCFOUR_CIPHERNAME  "ARCFOUR"


WORD32 ARCFOUR_GetCipherInfo(CIPHERINFOBLOCK*);

WORD32 ARCFOUR_SelfTest (void*);

WORD32 ARCFOUR_CreateWorkContext(void*, const WORD8*, WORD32, WORD32, void*,
                                 Cipher_RandomGenerator, const void*);

void  ARCFOUR_ResetWorkContext(void*, WORD32, void*,
                               Cipher_RandomGenerator, const void*);

WORD32 ARCFOUR_DestroyWorkContext (void*);

void ARCFOUR_EncryptBuffer(void*, const void*, void*, WORD32);

void ARCFOUR_DecryptBuffer(void*, const void*, void*, WORD32, const void*);


#ifdef __cplusplus
}
#endif


#endif
