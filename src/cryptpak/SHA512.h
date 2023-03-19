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


#ifndef __SHA512_H
#define __SHA512_H

#include "cpconfig.h"
#include "BasicTypes.h"


#ifdef __cplusplus
extern "C" {
#endif

#define SHA512_DIGESTSIZE	64

//////////////////////////////////////////////////////////////////////////////

typedef struct
{
  WORD64 qTotalLen;		// (maximum data size is 2^64 bits!)
  WORD64 hash[8];
  WORD32 lBufLen;
  WORD8 data[128];
}
SHA512CTX, *PSHA512CTX; 

//////////////////////////////////////////////////////////////////////////////

PSHA512CTX CRYPTPAK_API SHA512_Create    ();
void	   CRYPTPAK_API SHA512_Initialize(PSHA512CTX);
void       CRYPTPAK_API SHA512_Reset     (PSHA512CTX);
void	   CRYPTPAK_API SHA512_Destroy   (PSHA512CTX);
void	   CRYPTPAK_API SHA512_Update    (PSHA512CTX, const void*, WORD32);
void       CRYPTPAK_API SHA512_Final     (PSHA512CTX, WORD8*);
BYTEBOOL   CRYPTPAK_API SHA512_SelfTest  ();

//////////////////////////////////////////////////////////////////////////////

#ifdef __cplusplus
}
#endif


#endif
