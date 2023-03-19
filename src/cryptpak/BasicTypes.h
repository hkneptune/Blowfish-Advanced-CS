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


/*
 * defines basic primitive data types to avoid system independencies
 */

#ifndef __BASICTYPES_H
#define __BASICTYPES_H


// basic unsigned integer types
typedef unsigned __int8  WORD8;   // unsigned 8bit integer, prefix "b"
typedef unsigned __int16 WORD16;  // unsigned 8bit integer, prefix "w"
typedef unsigned __int32 WORD32;  // unsigned 8bit integer, prefix "l"
typedef unsigned __int64 WORD64;  // unsigned 8bit integer, prefix "q"


// boolean definitions
typedef unsigned __int8 BYTEBOOL;  // 8bit boolean, prefix "bl"

// our own Unicode type
typedef unsigned __int16 UNICHAR;


#define BOOL_FALSE  0
#define BOOL_TRUE   1


// some helper macros

#define MAKE_WORD64(left, right) ((((WORD64)(left)) << 32) | (WORD64)(right))

#define WORD64_LO(value) ((WORD32)(value & 0xffffffff))
#define WORD64_HI(value) ((WORD32)(value >> 32))

#define WORD32_REVERSE_ORDER(w)				     		\
	(((w) >> 24)             | (((w) >>  8) & 0xff00) |	\
	(((w) <<  8) & 0xff0000) |  ((w) << 24))

#define BYTES_TO_WORD32(b)															\
	(((WORD32)((b)[0]) << 24)           | (((WORD32)((b)[1]) << 16) & 0x00ff0000) |	\
	(((WORD32)((b)[2]) <<  8) & 0xff00) |  ((WORD32)((b)[3])        &       0xff))

#define BYTES_TO_WORD32_X86(b)														\
	(((WORD32)((b)[3]) << 24)           | (((WORD32)((b)[2]) << 16) & 0x00ff0000) |	\
	(((WORD32)((b)[1]) <<  8) & 0xff00) |  ((WORD32)((b)[0])        &       0xff))

#define WORD32_TO_BYTES(w, b)									\
	(b)[0] = (WORD8)((w) >> 24); (b)[1] = (WORD8)((w) >> 16);	\
	(b)[2] = (WORD8)((w) >>  8); (b)[3] = (WORD8) (w);

#define WORD32_TO_BYTES_X86(w, b)								\
	(b)[3] = (WORD8)((w) >> 24); (b)[2] = (WORD8)((w) >> 16);	\
	(b)[1] = (WORD8)((w) >>  8); (b)[0] = (WORD8) (w);

#define BYTES_TO_WORD64(b)																	\
 (((WORD64)(																				\
     ((WORD32)((b)[0]) << 24)           | (((WORD32)((b)[1]) << 16) & 0x00ff0000) |			\
    (((WORD32)((b)[2]) <<  8) & 0xff00) |  ((WORD32)((b)[3])        & 0x000000ff)) << 32) | \
  ((WORD64)(																				\
     ((WORD32)((b)[4]) << 24)           | (((WORD32)((b)[5]) << 16) & 0x00ff0000) |			\
    (((WORD32)((b)[6]) <<  8) & 0xff00) |  ((WORD32)((b)[7])        & 0x000000ff))))

#define WORD64_TO_BYTES(w, b)									\
	(b)[0] = (WORD8)((w) >> 56); (b)[1] = (WORD8)((w) >> 48);	\
	(b)[2] = (WORD8)((w) >> 40); (b)[3] = (WORD8)((w) >> 32);	\
	(b)[4] = (WORD8)((w) >> 24); (b)[5] = (WORD8)((w) >> 16);	\
	(b)[6] = (WORD8)((w) >>  8); (b)[7] = (WORD8) (w);


#endif

