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

//
// The class ID of this Shell extension class.
//
//
// {FBEA4C34-00F5-11d3-A707-0000B4432A4C}
//
//
// NOTE!!!  If you use this shell extension as a starting point, 
//          you MUST change the GUID below.  Simply run UUIDGEN.EXE
//          to generate a new GUID.
//
                                  
#define ODS(sz) OutputDebugString(sz)

#ifndef _BFACSEXT_H
#define _BFACSEXT_H


DEFINE_GUID(CLSID_ShellExtension, 
0xfbea4c34, 0xf5, 0x11d3, 0xa7, 0x7, 0x0, 0x0, 0xb4, 0x43, 0x2a, 0x4c);


void bfaCSExt_Initialize(HINSTANCE);


// this class factory object creates context menu handlers for Windows 95 shell
class CShellExtClassFactory : public IClassFactory
{
protected:
	ULONG	m_cRef;

public:
	CShellExtClassFactory();
	~CShellExtClassFactory();

	//IUnknown members
	STDMETHODIMP			QueryInterface(REFIID, LPVOID FAR *);
	STDMETHODIMP_(ULONG)	AddRef();
	STDMETHODIMP_(ULONG)	Release();

	//IClassFactory members
	STDMETHODIMP		CreateInstance(LPUNKNOWN, REFIID, LPVOID FAR *);
	STDMETHODIMP		LockServer(BOOL);

};
typedef CShellExtClassFactory *LPCSHELLEXTCLASSFACTORY;

// this is the actual OLE Shell context menu handler
class CShellExt : public IContextMenu, 
                         IShellExtInit
{

protected:
	ULONG        m_cRef;
	LPDATAOBJECT m_pDataObj;
	ULONG   	 m_ulNumOfObjects;
	BOOL	     m_blSingleFileClicked;

	STDMETHODIMP DoGAKMenu1(HWND hParent, 
	                        LPCSTR pszWorkingDir, 
	                        LPCSTR pszCmd,
                            LPCSTR pszParam, 
                            int iShowCmd);

	STDMETHODIMP DoGAKMenu2(HWND hParent, 
	                        LPCSTR pszWorkingDir, 
	                        LPCSTR pszCmd,
                            LPCSTR pszParam, 
                            int iShowCmd);

	STDMETHODIMP DoGAKMenu3(HWND hParent, 
	                        LPCSTR pszWorkingDir, 
	                        LPCSTR pszCmd,
                            LPCSTR pszParam, 
                            int iShowCmd);

	STDMETHODIMP DoGAKMenu4(HWND hParent, 
	                        LPCSTR pszWorkingDir, 
	                        LPCSTR pszCmd,
                            LPCSTR pszParam, 
                            int iShowCmd);
public:
	CShellExt();
	~CShellExt();

	//IUnknown members
	STDMETHODIMP			QueryInterface(REFIID, LPVOID FAR *);
	STDMETHODIMP_(ULONG)	AddRef();
	STDMETHODIMP_(ULONG)	Release();

	//IShell members
	STDMETHODIMP			QueryContextMenu(HMENU hMenu,
	                                         UINT indexMenu, 
	                                         UINT idCmdFirst,
                                             UINT idCmdLast, 
                                             UINT uFlags);

	STDMETHODIMP			InvokeCommand(LPCMINVOKECOMMANDINFO lpcmi);

	STDMETHODIMP			GetCommandString(UINT idCmd, 
	                                         UINT uFlags, 
	                                         UINT FAR *reserved, 
                                             LPSTR pszName, 
                                             UINT cchMax);

	//IShellExtInit methods
	STDMETHODIMP		    Initialize(LPCITEMIDLIST pIDFolder, 
	                                   LPDATAOBJECT pDataObj, 
	                                   HKEY hKeyID);


};
typedef CShellExt *LPCSHELLEXT;

#endif // _SHELLEXT_H
