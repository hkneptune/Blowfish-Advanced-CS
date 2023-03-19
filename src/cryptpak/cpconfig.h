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

#ifndef __CPCONFIG_H
#define __CPCONFIG_H

#ifdef __cplusplus
extern "C" {
#endif

// import-export definitions, this allows CryptPak to be used in a DLL or even
// as a normal part of a program

#ifdef WIN32

#ifdef CRYPTPAK_DLL

  #define CRYPTPAK_CALLCONV __stdcall

  #ifdef EXPORT_DLL
    #define CRYPTPAK_API __declspec(dllexport) CRYPTPAK_CALLCONV
  #else
    #define CRYPTPAK_API __declspec(dllimport) CRYPTPAK_CALLCONV
  #endif

#else

  #define CRYPTPAK_CALLCONV
  #define CRYPTPAK_API

#endif

#endif


#ifdef UNIX

  #define CRYPTPAK_CALLCONV
  #define CRYPTPAK_API

#endif

#ifdef __cplusplus
}
#endif

#endif
