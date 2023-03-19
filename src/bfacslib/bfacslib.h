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

#ifndef __BFACSLIB_H
#define __BFACSLIB_H

#define BFACSLIB_CALLCONV __stdcall

#ifdef EXPORT_DLL
#define BFACSLIB_API __declspec(dllexport) BFACSLIB_CALLCONV
#else
#define BFACSLIB_API __declspec(dllimport) BFACSLIB_CALLCONV
#endif

// error codes

#define BFACSLIB_ERR_UNKNOWN		-1	// never raised, just for error processing
#define BFACSLIB_ERR_NOERROR		0	// operation was successfull
#define BFACSLIB_ERR_ERROR			1	// general error, not specified futher
#define BFACSLIB_ERR_NULLPTR		2	// unexpected NULL pointer detected
#define BFACSLIB_ERR_WRONGPARAM		3	// one or more parameters are wrong
#define BFACSLIB_ERR_NOIMPL			4	// function not implemented
#define BFACSLIB_ERR_OUTOFMEMORY	5	// out of memory
#define BFACSLIB_ERR_INTERNAL		6	// implementation problem, should never happen
#define BFACSLIB_ERR_ZLIBEX			100	// zlibex: general error
#define BFACSLIB_ERR_ZLIBEX_REPEAT	101	// zlibex: function call must be repeated

#endif

