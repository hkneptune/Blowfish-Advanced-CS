(*
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
 *)


{
  for Windows diagnosis (just to show the information)
}


unit Win32Diagnosis;


interface
uses Classes,
     StringRes;

type
  TWin32Diagnosis = class
  protected
    // members
    m_sVersion  : String;
    m_sPlatform : String;
    m_sr        : TStrRes;

  public
    // constructor
    constructor Create(sr : TStrRes);

    // gets the screen resolution
    // <- screen res
    function GetScreenRes : String;

    // gets the mouse information
    // <- mouse info
    function GetMouseInfo : String;

    // gets the mouse wheel information
    // <- mouse wheel info
    function GetMouseWheelInfo : String;

    // get the Windows platform
    // <- Windows platform
    function GetPlatform : String;

    // get the Windows version in an string representation
    // <- Windows version
    function GetVersion : String;

    // get computer name
    // <- computer name (empty, if unknown)
    function GetComputerName : String;

    // get user name
    // <- username (empty, if unknown)
    function GetUserName : String;

    // get system directory
    // <- system directory
    class function GetSystemDirectory : String;

    // get number of processors
    // <- number of processors
    class function GetNumberOfProcessors : String;

    // get Windows directory
    // <- Windows directory
    class function GetWindowsDirectory : String;

    // get screen saver state
    // <- screen saver state
    function GetScreenSaverEnabled : String;

    // get the boot status
    // <- the boot status
    function GetBootStatus : String;

    // get temporary directory
    // <- temporary directory
    class function GetTemporaryDirectory : String;

    // to get an environment variable
    // -> the name of the variable
    // -> here to put the content
    // <- True: success / False: variable doesn't exist
    class function GetEnvVar(sVarName : String;
                             var vsContent : String) : Boolean;

    // get memory load (in percent)
    // <- memory load
    function GetMemoryLoad : String;

    // get physical memory
    // <- pysical memory
    function GetPhysicalMemory : String;

    // get free physical memory
    // <- free physical memory
    function GetFreePhysicalMemory : String;

    // get pagefile size
    // <- pagefile size
    function GetPageFileSize : String;

    // get free pagefile space
    // <- free pagefile space
    function GetFreePagefileSpace : String;

    // get user address space
    // <- user address space
    function GetUserAddressSpace : String;

    // get free user address space
    // <- free user address space
    function GetFreeUserAddressSpace : String;

    // get registry subkeys (from HKEY_LOCAL_MACHINE)
    // -> the key path to extract the subkeys
    // <- the subkeys (Nil equals error)
    class function GetRegistrySubKeys(sKey : String) : TStringList;

    // get a registry value in string representation (from HKEY_LOCAL_MACHINE)
    // -> the key path to extract the value
    // -> value to extract
    // -> where to put the value
    // <- True: success / False: error, e.g. key not found
    class function GetRegistryStringValue(sKey : String;
                                          sValue : String;
                                          var vsContent : String) : Boolean;

    // get registry values in string representation (from HKEY_LOCAL_MACHINE)
    // -> the key path to extract the values
    // -> where to put the value descriptions
    // -> where to put the value contents
    // <- True: success / False: error, e.g. key not found
    class function GetRegistryStringValues(sKey : String;
                                           var valueDesc : TStringList;
                                           var valueCont : TStringList)
                                             : Boolean;

    // gets the version number out of a module
    // -> module (EXE/DLL)
    // <- version number in the x.xx.xx.xxx format (empty on error)
    class function GetModuleVersion(const sModule : String) : String;

  end;




implementation
uses Windows,
     SysUtils,
     StringPlus,
     StringPlusI,
     bfacslib,
     General;


//////////////////////////// TWin32Diagnosis ////////////////////////////


// string resource ID
const
  STRRES_ID = 'WINDIAG';



constructor TWin32Diagnosis.Create(sr : TStrRes);
var
  sTemp  : String;
  osInfo : TOSVersionInfo;
begin
  // get the string resources
  m_sr:=sr;

  // get Windows version and system
  osInfo.dwOSVersionInfoSize:=SizeOf(osInfo);
  GetVersionEx(osInfo);
  m_sVersion:=IntToStr(osInfo.dwMinorVersion);
  m_sVersion:=IntToStr(osInfo.dwMajorVersion) + '.' +
              TStrPlus.MulChar('0', 2 - Length(m_sVersion)) + m_sVersion + '.';
  sTemp:=IntToStr(osInfo.dwBuildNumber and $0ffff);
  m_sVersion:=m_sVersion + TStrPlus.MulChar('0', 4 - Length(sTemp)) + sTemp;
  sTemp:=String(osInfo.szCSDVersion);
  if (Length(sTemp) > 0) then
    if (sTemp[1] <> ' ') then
      sTemp:=' ' + sTemp;
  m_sVersion:=m_sVersion + sTemp;

  case osInfo.dwPlatformId of
    VER_PLATFORM_WIN32s :
      m_sPlatform:='Win32s';
    VER_PLATFORM_WIN32_WINDOWS : begin
      if (osInfo.dwMinorVersion = 0) then  // thanx to MSDN
        m_sPlatform:='Windows 95'
      else if (osInfo.dwMinorVersion = 10) then
        m_sPlatform:='Windows 98'
      else
        m_sPlatform:='Windows Me';
    end;
    VER_PLATFORM_WIN32_NT :
      case osInfo.dwMajorVersion of
        3 : m_sPlatform:='Windows NT 3';
        4 : m_sPlatform:='Windows NT 4.0';
        5 : case osInfo.dwMinorVersion of
              0: m_sPlatform:='Windows 2000';
              1: m_sPlatform:='Windows XP';
              2: m_sPlatform:='Windows Server 2003';
            else
              m_sPlatform:='Windows Longhorn+';
            end;
      end;     
  else
    m_sPlatform:=m_sr.Get(STRRES_ID, '000');
  end;
end;



function TWin32Diagnosis.GetScreenRes : String;
begin
  Result:=IntToStr(GetSystemMetrics(SM_CXSCREEN)) + ' * ' +
          IntToStr(GetSystemMetrics(SM_CYSCREEN));
end;

function TWin32Diagnosis.GetMouseInfo : String;
begin
  if (GetSystemMetrics(SM_MOUSEPRESENT) <> 0) then
    Result:=m_sr.Get(STRRES_ID, '007')
  else
    Result:=m_sr.Get(STRRES_ID, '008');
end;

function TWin32Diagnosis.GetMouseWheelInfo : String;
begin
  if (GetSystemMetrics(SM_MOUSEWHEELPRESENT) <> 0) then
    Result:=m_sr.Get(STRRES_ID, '007')
  else
    Result:=m_sr.Get(STRRES_ID, '008');
end;

function TWin32Diagnosis.GetVersion : String;
begin
  // just return the system string
  Result:=m_sVersion;
end;

function TWin32Diagnosis.GetPlatform : String;
begin
  // just return the version string
  Result:=m_sPlatform;
end;


function TWin32Diagnosis.GetComputerName : String;
var
  lLen : WORD32;
begin
  // get the computer name
  SetLength(Result, MAX_COMPUTERNAME_LENGTH);
  if (Windows.GetComputerName(PChar(Result),
                              lLen) = TRUE) then begin
    SetLength(Result, lLen);
    Result:=Result
  end
  else
    Result:='';
end;


function TWin32Diagnosis.GetUserName : String;
var
  lLen : WORD32;
begin
  // get the user name
  SetLength(Result, 2048);  // (2kB should be enough)
  if (Windows.GetUserName(PChar(Result),
                          lLen) = TRUE) then begin
    SetLength(Result, lLen - 1); // (weird: "- 1" was necessary above)
    Result:=Result
  end
  else
    Result:='';
end;


class function TWin32Diagnosis.GetSystemDirectory : String;
var
  lLen : WORD32;
begin
  // get the system dir
  SetLength(Result, MAX_PATH);
  lLen:=Windows.GetSystemDirectory(PChar(Result), MAX_PATH);
  SetLength(Result, lLen);
  Result:=Result
end;


class function TWin32Diagnosis.GetNumberOfProcessors : String;
var
  sysInfo : TSystemInfo;
begin
  // get the number of processors
  Windows.GetSystemInfo(sysInfo);
  Result:=IntToStr(sysInfo.dwNumberOfProcessors);
end;


class function TWin32Diagnosis.GetWindowsDirectory : String;
var
  lLen : WORD32;
begin
  // get the user name
  SetLength(Result, MAX_PATH);
  lLen:=Windows.GetWindowsDirectory(PChar(Result), MAX_PATH);
  SetLength(Result, lLen);
  Result:=Result
end;


function TWin32Diagnosis.GetScreenSaverEnabled : String;
var
  blSSActive : BOOL;
begin
  // screensaver enabled?
  if (Windows.SystemParametersInfo(SPI_GETSCREENSAVEACTIVE,
                                   0,
                                   @blSSActive,
                                   0) = TRUE) then
    if (blSSActive = TRUE) then
      Result:=m_sr.Get(STRRES_ID, '001')
    else
      Result:=m_sr.Get(STRRES_ID, '002')
  else
    Result:=m_sr.Get(STRRES_ID, '000');
end;


function TWin32Diagnosis.GetBootStatus : String;
begin
  case Windows.GetSystemMetrics(SM_CLEANBOOT) of
    0 : Result:=m_sr.Get(STRRES_ID, '003');
    1 : Result:=m_sr.Get(STRRES_ID, '004');
    2 : Result:=m_sr.Get(STRRES_ID, '005');
  else
    Result:=m_sr.Get(STRRES_ID, '000');
  end;
end;


class function TWin32Diagnosis.GetTemporaryDirectory : String;
var
  lLen : WORD32;
begin
  // get the computer name
  lLen:=MAX_PATH;
  SetLength(Result, lLen);
  lLen:=Windows.GetTempPath(lLen, PChar(Result));
  if (lLen <> 0) then begin
    SetLength(Result, lLen);
    Result:=Result
  end
  else
    Result:='.';
end;


class function TWin32Diagnosis.GetEnvVar(sVarName : String;
                                         var vsContent : String) : Boolean;
const
  // (should be enough)
  MAX_ENVVAR_SIZE = 32768;
var
  lLen : WORD32;
begin
  // try to get the env. variable
  SetLength(vsContent, MAX_ENVVAR_SIZE);
  lLen:=GetEnvironmentVariable(PChar(sVarName),
                               PChar(vsContent),
                               MAX_ENVVAR_SIZE);
  if (lLen > 0) then begin
    SetLength(vsContent, lLen);
    Result:=True;
  end
  else
    Result:=False;
end;

// all about getting memory information...

function TWin32Diagnosis.GetMemoryLoad : String;
var
  memState : TMemoryStatus;
begin
  GlobalMemoryStatus(memState);
  Result:=TStrPlusI.Sepa1000(m_sr, memState.dwMemoryLoad) + '%';
end;

function TWin32Diagnosis.GetPhysicalMemory : String;
var
  memState : TMemoryStatus;
begin
  GlobalMemoryStatus(memState);
  Result:=TStrPlusI.Sepa1000(m_sr, memState.dwTotalPhys) +
          m_sr.Get(STRRES_ID, '006');
end;

function TWin32Diagnosis.GetFreePhysicalMemory : String;
var
  memState : TMemoryStatus;
begin
  GlobalMemoryStatus(memState);
  Result:=TStrPlusI.Sepa1000(m_sr, memState.dwAvailPhys) +
          m_sr.Get(STRRES_ID, '006');
end;

function TWin32Diagnosis.GetPageFileSize : String;
var
  memState : TMemoryStatus;
begin
  GlobalMemoryStatus(memState);
  Result:=TStrPlusI.Sepa1000(m_sr, memState.dwTotalPageFile) +
          m_sr.Get(STRRES_ID, '006');
end;

function TWin32Diagnosis.GetFreePagefileSpace : String;
var
  memState : TMemoryStatus;
begin
  GlobalMemoryStatus(memState);
  Result:=TStrPlusI.Sepa1000(m_sr, memState.dwAvailPageFile) +
          m_sr.Get(STRRES_ID, '006');
end;

function TWin32Diagnosis.GetUserAddressSpace : String;
var
  memState : TMemoryStatus;
begin
  GlobalMemoryStatus(memState);
  Result:=TStrPlusI.Sepa1000(m_sr, memState.dwTotalVirtual) +
          m_sr.Get(STRRES_ID, '006');
end;

function TWin32Diagnosis.GetFreeUserAddressSpace : String;
var
  memState : TMemoryStatus;
begin
  GlobalMemoryStatus(memState);
  Result:=TStrPlusI.Sepa1000(m_sr, memState.dwAvailVirtual) +
          m_sr.Get(STRRES_ID, '006');
end;


// for registry examinations...

class function TWin32Diagnosis.GetRegistrySubKeys(sKey : String) : TStringList;
var
  lI            : WORD32;
  lRetCode      : WORD32;
  lBufLen       : WORD32;
  theKey        : HKEY;
  sBuf          : String;
  lastWriteTime : TFileTime;
begin

  // open the key
  if (RegOpenKeyEx(HKEY_LOCAL_MACHINE,
                   PChar(sKey),
                   0,
                   KEY_READ,
                   theKey) <> ERROR_SUCCESS) then begin
    Result:=Nil;
    Exit;
  end;

  // key exists, create the list
  Result:=TStringList.Create;
  Result.BeginUpdate;

  // now search for two subkeys
  lI:=0;
  repeat
    // get a subkey
    lBufLen:=MAX_PATH;
    SetLength(sBuf, lBufLen);
    lRetCode:=RegEnumKeyEx(theKey,
                           lI,
                           PChar(sBuf),
                           lBufLen,
                           Nil,
                           Nil,
                           Nil,
                           @lastWriteTime);
    if (lRetCode = ERROR_SUCCESS) then begin
      // add the subkey
      SetLength(sBuf, lBufLen);
      Result.Add(sBuf);
    end;

    // next subkey
    Inc(lI);
  until (lRetCode <> ERROR_SUCCESS);
  Result.EndUpdate;

  // cleanup
  RegCloseKey(theKey);
end;


class function TWin32Diagnosis.GetRegistryStringValue(sKey : String;
                                                      sValue : String;
                                                      var vsContent : String)
                                                        : Boolean;
var
  lBufLen : WORD32;
  lType   : WORD32;
  theKey  : HKEY;
  sBuf    : String;
begin
  // open the key
  if (RegOpenKeyEx(HKEY_LOCAL_MACHINE,
                   PChar(sKey),
                   0,
                   KEY_READ,
                   theKey) <> ERROR_SUCCESS) then begin
    Result:=False;
    Exit;
  end;

  // get the value
  lBufLen:=MAX_PATH;
  SetLength(sBuf, lBufLen);
  if (RegQueryValueEx(theKey,
                      PChar(sValue),
                      Nil,
                      @lType,
                      PByte(PChar(sBuf)),
                      @lBufLen) = ERROR_SUCCESS) then begin
    // correct type?
    if ((lType = REG_SZ) or (lType = REG_EXPAND_SZ)) then begin
      SetLength(sBuf, lBufLen - 1);
      vsContent:=sBuf;
      Result:=True;
    end
    else
      Result:=False;
  end
  else
    Result:=False;

  // cleanup
  RegCloseKey(theKey);
end;


class function TWin32Diagnosis.GetRegistryStringValues(
                                 sKey : String;
                                 var valueDesc : TStringList;
                                 var valueCont : TStringList) : Boolean;
begin
  Result:=False
end;


class function TWin32Diagnosis.GetModuleVersion
  (const sModule : String) : String;
var
  dwZ            : DWORD;
  dwInfoSize     : DWORD;
  dwFileInfoSize : DWORD;
  pInfo          : Pointer;
  pVffi          : PVSFIXEDFILEINFO;
begin

  dwInfoSize := GetFileVersionInfoSize(PChar(sModule), dwZ);

  if (dwInfoSize = 0) then
  begin
    Result := '';
    Exit
  end;

  GetMem(pInfo, dwInfoSize);

  GetFileVersionInfo(PChar(sModule), 0, dwInfoSize, pInfo);

  VerQueryValue(pInfo,
                '\',
                Pointer(pVffi),
                dwFileInfoSize);

  Result := TStrPlus.VersionFormatEx(pVffi.dwFileVersionMS shr 16,
                                     pVffi.dwFileVersionMS and $0ffff,
                                     pVffi.dwFileVersionLS shr 16,
                                     pVffi.dwFileVersionLS and $0ffff);

  FreeMem(pInfo);
end;


end.
