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
 * implementation of the context menu handler for bfaCS,
 * (parts are taken from the original Microsoft ShellExt sample)
 */

#include <stdio.h>

#include "priv.h"
#include "bfacsext.h"


// command line options to let bfaCS detect a job passed by the shell
#define BFACS_OP_ENCRYPT    "-shellencrypt"
#define BFACS_OP_DECRYPT    "-shelldecrypt"
#define BFACS_OP_WIPE       "-shellwipe"
#define BFACS_OP_REENCRYPT  "-shellreencrypt"
#define BFACS_OP_DESLACK    "-shelldeslack"
#define BFACS_OP_WORKWITH   "-shellworkwith"
#define BFACS_OP_VIEW       "-shellview"

// prefixes for the shared memory ID (option)
#define BFACS_OP_SMIPREFIX  "-shelldataid"
#define SHAREMEM_ID_PREFIX  "BFACSSHID"




// the internationalized captions for the menu items are stored 
// in the registry, here are they key and values to fetch them

#define REGKEY_ISTRINGS "*\\shellex\\ContextMenuHandlers\\bfaCSMenu"

#define REGVAL_CAPTION   "Caption"
#define REGVAL_ENCRYPT   "Encrypt"
#define REGVAL_DECRYPT   "Decrypt"
#define REGVAL_WIPE      "Wipe"
#define REGVAL_REENCRYPT "Reencrypt"
#define REGVAL_DESLACK   "Deslack"
#define REGVAL_WORKWITH  "WorkWith"
#define REGVAL_VIEW      "View"


// to get the location of BFACS.EXE
#define REGVAL_BFACSEXE "BFACSEXE"

// error message string IDs
#define REGVAL_ERRMESS1  "ERRMESS1"
#define REGVAL_ERRMESS2  "ERRMESS2"


// reg. value IDs for the help strings (see below)
#define REGVAL_HELPCAPTION   "HelpCaption"
#define REGVAL_HELPENCRYPT   "HelpEncrypt"
#define REGVAL_HELPDECRYPT   "HelpDecrypt"
#define REGVAL_HELPWIPE      "HelpWipe"
#define REGVAL_HELPREENCRYPT "HelpReencrypt"
#define REGVAL_HELPDESLACK   "HelpDeslack"
#define REGVAL_HELPWORKWITH  "HelpWorkWith"
#define REGVAL_HELPVIEW      "HelpView"


// macros to init./release the registry key 
#define OPENSTRREGKEY                               \
  HKEY strKey;                                      \
  DWORD dwValType;                                  \
  if (::RegOpenKeyEx(HKEY_CLASSES_ROOT,             \
                     REGKEY_ISTRINGS,               \
                     0,                             \
                     KEY_QUERY_VALUE,               \
                     &strKey) != ERROR_SUCCESS) {   \
     return NOERROR;                                \
  }

#define CLOSESTRREGKEY      \
    ::RegCloseKey(strKey);


// macro to fetch strings in the following method
#define GETISTR(val)                \
    dwDataSize = STRBUFSIZE;        \
    ::RegQueryValueEx(strKey,       \
                    val,            \
                    NULL,           \
                    &dwValType,     \
                    (LPBYTE)szBuf,  \
                    &dwDataSize);
      


#define STRBUFSIZE 1024

STDMETHODIMP CShellExt::QueryContextMenu(HMENU hMenu,
                                         UINT indexMenu,
                                         UINT idCmdFirst,
                                         UINT idCmdLast,
                                         UINT uFlags) {

           
    UINT idCmd = idCmdFirst;
    char szBuf[STRBUFSIZE];
    int nNumOfCmds = 6;


    // need to create a menu?
    /*
    if (((uFlags & CMF_EXPLORE) ||
         (uFlags & CMF_VERBSONLY) ||
         ((uFlags & 0x000f) == CMF_NORMAL)) &&
        (m_ulNumOfObjects > 0)) {
    */

    if (m_ulNumOfObjects > 0) { 


      // get the registry key for the strings
      OPENSTRREGKEY
      DWORD dwDataSize = STRBUFSIZE;

      // one separator at the top
      ::InsertMenu(hMenu, indexMenu++, MF_SEPARATOR|MF_BYPOSITION, 0, NULL);

      // create the submenu
      HMENU hSubMenu = ::CreatePopupMenu();
      
      MENUITEMINFO itemInfo;

      itemInfo.cbSize = sizeof(itemInfo);
      itemInfo.wID = idCmd;
      itemInfo.hSubMenu = NULL;
      itemInfo.hbmpChecked = NULL;
      itemInfo.hbmpUnchecked = NULL;
      itemInfo.dwItemData = 0;
      itemInfo.dwTypeData = szBuf;
      itemInfo.cch = 0;

      itemInfo.fType = MFT_STRING;
      itemInfo.fMask = MIIM_TYPE | MIIM_STATE | MIIM_ID;
      itemInfo.fState = MFS_ENABLED;
    
      itemInfo.wID++;
      GETISTR(REGVAL_ENCRYPT)
      ::InsertMenuItem(hSubMenu, 0xffffffff, TRUE, &itemInfo);

      itemInfo.wID++;
      GETISTR(REGVAL_DECRYPT)
      ::InsertMenuItem(hSubMenu, 0xffffffff, TRUE, &itemInfo);

      itemInfo.wID++;
      GETISTR(REGVAL_WIPE)
      ::InsertMenuItem(hSubMenu, 0xffffffff, TRUE, &itemInfo);

      itemInfo.wID++;
      GETISTR(REGVAL_REENCRYPT)
      ::InsertMenuItem(hSubMenu, 0xffffffff, TRUE, &itemInfo);

      itemInfo.wID++;
      GETISTR(REGVAL_DESLACK)
      ::InsertMenuItem(hSubMenu, 0xffffffff, TRUE, &itemInfo);

      // add single-file-only items, if necessary 
      if (m_blSingleFileClicked) {

          itemInfo.fType = MFT_SEPARATOR ;
          ::InsertMenuItem(hSubMenu, 0xffffffff, TRUE, &itemInfo);
          itemInfo.fType = MFT_STRING;

          itemInfo.wID++;
          GETISTR(REGVAL_WORKWITH)
          ::InsertMenuItem(hSubMenu, 0xffffffff, TRUE, &itemInfo);

          itemInfo.wID++;
          GETISTR(REGVAL_VIEW)
          ::InsertMenuItem(hSubMenu, 0xffffffff, TRUE, &itemInfo);

          nNumOfCmds += 2;
      } 


      GETISTR(REGVAL_CAPTION)
      ::InsertMenu(hMenu,
                 indexMenu++,
                 MF_STRING | MF_BYPOSITION | MF_POPUP,
                 (UINT_PTR)hSubMenu,
                 szBuf);

      CLOSESTRREGKEY

      return ResultFromShort(nNumOfCmds);

   } 

   return NOERROR;
}




STDMETHODIMP CShellExt::InvokeCommand(LPCMINVOKECOMMANDINFO lpcmi)
{
    DWORD dwI;

    // common command selector
    if (!HIWORD(lpcmi->lpVerb))
    {
        UINT idCmd = LOWORD(lpcmi->lpVerb);

        // get the command option
        char opPar[32];
        switch (idCmd)
        {
            case 1 : ::strcpy(opPar, BFACS_OP_ENCRYPT); break;
            case 2 : ::strcpy(opPar, BFACS_OP_DECRYPT); break;
            case 3 : ::strcpy(opPar, BFACS_OP_WIPE); break;
            case 4 : ::strcpy(opPar, BFACS_OP_REENCRYPT); break;
            case 5 : ::strcpy(opPar, BFACS_OP_DESLACK); break;
            case 6 : ::strcpy(opPar, BFACS_OP_WORKWITH); break;
            case 7 : ::strcpy(opPar, BFACS_OP_VIEW); break;
            default :
              return E_INVALIDARG;
        }

        // get the location of BFACS.EXE
        char szBuf[STRBUFSIZE];
        char szEXEPath[STRBUFSIZE];
        DWORD dwDataSize = STRBUFSIZE;
        OPENSTRREGKEY
        GETISTR(REGVAL_BFACSEXE)
        CLOSESTRREGKEY
        ::strcpy(szEXEPath, szBuf);


        // get a (16bit) random value to identify the data 
        // passed via the shared memory
        char transBufID[32];
        DWORD dwTransID = ::rand() & 0x0ffff;
        ::wsprintf(transBufID,
                 "%s%04x",
                 SHAREMEM_ID_PREFIX,
                 dwTransID);

        // common setup to get the file object data
        FORMATETC fmte = {CF_HDROP,
                          (DVTARGETDEVICE FAR *)NULL,
                          DVASPECT_CONTENT,
                          -1,
                          TYMED_HGLOBAL};
        STGMEDIUM medium; 
        medium.tymed = TYMED_HGLOBAL;
        medium.pUnkForRelease = NULL;
        medium.hGlobal = NULL;
        m_pDataObj->GetData(&fmte, &medium);

        // now put all files got into shared memory,
        // to avoid timeconsuming memory linking we first
        // determine the absolute size of the memory
        // needed by scanning all filenames
        char objBuf[MAX_PATH];
        DWORD dwMemSize = 0;
        for (dwI = 0; dwI < m_ulNumOfObjects; dwI++) {

           // (FIXME: any faster way to do this?)
           ::DragQueryFile((HDROP)medium.hGlobal, 
                         dwI, 
                         objBuf,
                         sizeof(objBuf));
           dwMemSize += (DWORD)::strlen(objBuf) + 1;
        }
        dwMemSize++;  // (closing '\0') 

        // now it's going to be interesting,
        // we allocate the shared memory which will then be passed
        // to the application
        HANDLE hTransBuf = ::CreateFileMapping((HANDLE)-1,
                                             NULL,
                                             PAGE_READWRITE,
                                             0,
                                             dwMemSize,
                                             transBufID);
        if (hTransBuf == NULL)
          return NOERROR;    
        char* transBuf = (char*) ::MapViewOfFile(hTransBuf,
                                               FILE_MAP_WRITE,
                                               0, 0, 0);
        int nBufPos = 0;
        for (dwI = 0; dwI < m_ulNumOfObjects; dwI++) {
           ::DragQueryFile((HDROP)medium.hGlobal, 
                         dwI, 
                         objBuf,
                         sizeof(objBuf));
           ::strcpy(&transBuf[nBufPos], objBuf);
           nBufPos += (DWORD)::strlen(objBuf) + 1;
        }
        transBuf[nBufPos] = '\0';

        // buffer created, no need to hold the pointer any longer
        ::UnmapViewOfFile((LPCVOID)transBuf);
    
        // build the command line string
        char exeCall[STRBUFSIZE];
        ::wsprintf(exeCall,
                 "%s %s %s%04x",
                 szEXEPath,
                 opPar,
                 BFACS_OP_SMIPREFIX,
                 dwTransID);

        // now start the process 
        STARTUPINFO sinfo;
        ::memset(&sinfo, 0, sizeof(sinfo)); // (that should be ok so)
        sinfo.cb = sizeof(sinfo);
        PROCESS_INFORMATION pinfo;
        if (::CreateProcess(NULL,
                          exeCall,
                          NULL,
                          NULL,
                          TRUE,
                          0,
                          NULL,
                          NULL,
                          &sinfo,
                          &pinfo) == FALSE) {

          // error detected, clean up...
          DWORD dwLastError = GetLastError();
          ::CloseHandle(hTransBuf);
            
          //...show what happened...
          LPVOID lpMessageBuffer;
          ::FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM,
                        NULL,
                        dwLastError,
                        MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
                        (LPSTR)&lpMessageBuffer,
                        0,
                        NULL);
          dwDataSize = STRBUFSIZE;
          OPENSTRREGKEY
          GETISTR(REGVAL_ERRMESS1)
          char szCaption[STRBUFSIZE];
          ::strcpy(szCaption, szBuf);
          dwDataSize = STRBUFSIZE;
          GETISTR(REGVAL_ERRMESS2)
          CLOSESTRREGKEY
          char szText[STRBUFSIZE];
          ::wsprintf(szText,
                   szBuf, // (btw, that's the format string :)
                   szEXEPath,
                   (char*)lpMessageBuffer);
          ::LocalFree(lpMessageBuffer); 
          ::MessageBox(lpcmi->hwnd,
                     szText,
                     szCaption,
                     MB_ICONSTOP);

          // ...and quit (no reason to panic anyway)
          return NOERROR;
        }
                      
        // wait for the process to start up and take over the 
        // content through the shared memory
        ::WaitForInputIdle(pinfo.hProcess, INFINITE);
         
        // (FIXME: we don't close the handle to the shared memory
        //  to keep it alive as long as possible, because it's not
        //  guaranteed that the other process takes it over in time)
        return NOERROR;

    }
    return E_INVALIDARG;
}



STDMETHODIMP CShellExt::GetCommandString(UINT idCmd,
                                         UINT uFlags,
                                         UINT FAR *reserved,
                                         LPSTR pszName,
                                         UINT cchMax)
{
#define GETISTREX(val)      					\
	if (ERROR_SUCCESS != RegQueryValueEx(strKey,\
		val,              						\
        NULL,             						\
        &dwValType,       						\
        (LPBYTE)cbuf,  							\
        (DWORD*)&cchMax)) cbuf[0] = '\0';		\
  	break;                                


	char cbuf[256];	// (that should be plenty)
	WCHAR wbuf[sizeof(cbuf)];
	
	if (sizeof(cbuf) <= cchMax) // no overflows please
	{
		cchMax = sizeof(cbuf) - 1; 	// count the null terminator, too
	}
	
    // get the help string and show 
    OPENSTRREGKEY

    switch (idCmd)
    {
        // NOTE: where's idCmd 0 ?
        case 1 : GETISTREX(REGVAL_HELPENCRYPT)
        case 2 : GETISTREX(REGVAL_HELPDECRYPT)
        case 3 : GETISTREX(REGVAL_HELPWIPE)
        case 4 : GETISTREX(REGVAL_HELPREENCRYPT)
        case 5 : GETISTREX(REGVAL_HELPDESLACK)
        case 6 : GETISTREX(REGVAL_HELPWORKWITH)
        case 7 : GETISTREX(REGVAL_HELPVIEW) 
        default:
        {
        	cbuf[0] = '\0';	// play it safe
        }
    }

    CLOSESTRREGKEY
    
    // Unicode or ANSI?
    
    if (GCS_UNICODE & uFlags)
    {
    	// if the conversion fails the string will be at least empty

    	int nWCharCount = ::MultiByteToWideChar(
    		CP_ACP,
    		0,
    		cbuf,
    		::strlen(cbuf),
    		wbuf,
    		cchMax - 1);
    		
    	wbuf[nWCharCount] = (WCHAR)0;
    		
		::wcscpy((wchar_t*)pszName, (wchar_t*)wbuf);
    }
    else
    {
		::strcpy((char*)pszName, cbuf);
	}
	
    return NOERROR;
    
    
#undef GETISTREX   
}
