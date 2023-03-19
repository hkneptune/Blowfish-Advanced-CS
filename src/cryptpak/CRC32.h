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


#ifndef __CRC32_H
#define __CRC32_H

#include "cpconfig.h" 
#include "BasicTypes.h" 

#ifdef __cplusplus
extern "C" {
#endif


// CRC32 implementation after RFC 1662, [Page 20]


// startup value for a CRC32
#define CRC32_INITVALUE  0xffffffff

// xor this value to finish a CRC32
#define CRC32_DONEVALUE  0xffffffff


// updates an existing CRC32
// -> old CRC32
// -> pointer to (byte) buffer
// -> number of bytes to process
// <- new CRC32
WORD32 CRYPTPAK_API
	CRC32_Update (WORD32, const void*, WORD32);


// <- true: selftest OK / false: compatibility problem
BYTEBOOL CRYPTPAK_API CRC32_Test();


#ifdef __cplusplus
}
#endif


#endif
