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
 * context menu initializer
 * (parts are taken from the original Microsoft ShellExt sample)
 */


#include "priv.h"
#include "bfacsext.h"



STDMETHODIMP CShellExt::Initialize(LPCITEMIDLIST pIDFolder,
                                   LPDATAOBJECT pDataObj,
                                   HKEY hRegKey)
{

    ODS("CShellExt::Initialize()\r\n");

    // Initialize can be called more than once

    if (m_pDataObj)
    	m_pDataObj->Release();

    // duplicate the object pointer and registry handle

    if (pDataObj)
    {
    	m_pDataObj = pDataObj;
    	pDataObj->AddRef();
    }

	// how many objects did the user selected?
	m_ulNumOfObjects = 0;
	m_blSingleFileClicked = FALSE;

    FORMATETC fmte = {CF_HDROP,
        	          (DVTARGETDEVICE FAR *)NULL,
        	          DVASPECT_CONTENT,
        	          -1,
        	          TYMED_HGLOBAL 
        	         };

    STGMEDIUM medium;
	medium.tymed = TYMED_HGLOBAL;
	medium.pUnkForRelease = NULL;
	medium.hGlobal = NULL;
	if (m_pDataObj)  
      if (SUCCEEDED(m_pDataObj->GetData(&fmte, &medium))) {

		if (medium.hGlobal) {
          // get the number of objects 
          m_ulNumOfObjects = ::DragQueryFile((HDROP)medium.hGlobal, (UINT)-1, 0, 0);

          if (m_ulNumOfObjects == 1) {
           
		    // get the name of the first file or folder
		    char firstObj[MAX_PATH];
			::DragQueryFile((HDROP)medium.hGlobal, 
                          0, 
                          firstObj,
                          sizeof(firstObj));

			// is it a file?
			WIN32_FIND_DATA findData;
			HANDLE hFinder = ::FindFirstFile(firstObj,
				                           &findData);
			if (hFinder != INVALID_HANDLE_VALUE) {

              m_blSingleFileClicked = !((findData.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) == FILE_ATTRIBUTE_DIRECTORY);
              ::FindClose(hFinder);
			}
		  }
		}
	  }

    return NOERROR;
}
