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
  everythings for registry stuff
}

unit RegistryKeys;

interface
uses
  StringRes;

// support routines for registry settings
type
  TRegistryKeys = class

    // to update all necessary registry entries
    // -> True: enable file types / False: disable
    // -> True: enable shell extensions / False: disable
    // -> string resources
    // -> TRue: update only string resource dependant keys / False: do all
    class procedure Update(blFileTypes : Boolean;
                           blShellExts : Boolean;
                           sr : TStrRes;
                           blStrResOnly : Boolean = False);

    // to check if all keys are correct
    // (<-) True: file types are enabled / False: are disabled
    // (<-) True: shell extensions are enabled / False: are disabled
    class procedure Check(var vblFileTypes : Boolean;
                          var vblShellExts : Boolean);
  end;

implementation
uses
  Forms,
  Windows,
  StringPlus,
  SysUtils,
  BFAFile,
  bfacslib,
  BFJob,
  ShortcutChecker,
  Configuration;

//////////////////////////// TBFACSRegistryKeys ////////////////////////////


const
  STRRES_ID = 'BFACSREGISTRYKEYS';


// some key IDs
const
  BFAFILE_KEY = 'BFA_FILE';
  BFJFILE_KEY = 'BFJ_FILE';

// our GUID for the shell extensions
const
  SHELLEXT_GUID = '{FBEA4C34-00F5-11d3-A707-0000B4432A4C}';

// shell extension title
const
  SHELLEXT_TITLE = 'bfaCS.ShellExtension';

// shell extension DLL name
const
  SHELLEXT_DLL = BFACSLIB_MODULE;

// shell extension menu ID
const
  SHELLEXT_MENU = 'bfaCSMenu';

// for enabling the shell extension to load internationalized strings
const
  REGVAL_CAPTION       = 'Caption';
  REGVAL_ENCRYPT       = 'Encrypt';
  REGVAL_DECRYPT       = 'Decrypt';
  REGVAL_WIPE          = 'Wipe';
  REGVAL_REENCRYPT     = 'Reencrypt';
  REGVAL_DESLACK       = 'Deslack';
  REGVAL_WORKWITH      = 'WorkWith';
  REGVAL_VIEW          = 'View';
  REGVAL_ERRMESS1      = 'ERRMESS1';
  REGVAL_ERRMESS2      = 'ERRMESS2';
  REGVAL_HELPCAPTION   = 'HelpCaption';
  REGVAL_HELPENCRYPT   = 'HelpEncrypt';
  REGVAL_HELPDECRYPT   = 'HelpDecrypt';
  REGVAL_HELPWIPE      = 'HelpWipe';
  REGVAL_HELPREENCRYPT = 'HelpReencrypt';
  REGVAL_HELPDESLACK   = 'HelpDeslack';
  REGVAL_HELPWORKWITH  = 'HelpWorkWith';
  REGVAL_HELPVIEW      = 'HelpView';
  // (here we pass the location of BFACS.EXE)
  REGVAL_BFACSEXE      = 'BFACSEXE';



class procedure TRegistryKeys.Update(blFileTypes : Boolean;
                                     blShellExts : Boolean;
                                     sr : TStrRes;
                                     blStrResOnly : Boolean = False);
var
 hRes      : HKEY;
 sProgEXE  : String;
 sProgPath : String;
 scc       : TShortcutChecker;

procedure MakeKey(hParent : HKEY; const sKey : String;
                  const sCnt : String; const sValue : String = '');
begin
  RegCreateKey(hParent, PChar(sKey), hRes);
  RegSetValueEx(hRes, PChar(sValue), 0, REG_SZ,
                PChar(sCnt), Length(sCnt) + 1);
  RegCloseKey(hRes);
end;

procedure MakeRootKey(const sKey : String; const sCnt : String;
                      const sValue : String = '');
begin
  MakeKey(HKEY_CLASSES_ROOT, sKey, sCnt, sValue);
end;

procedure SetLocalizedStringKey(const sID : String;
                                blCreateShortCut : Boolean = False);
var
  sTemp : String;
begin
  sTemp:=sr.Get(STRRES_ID, sID);
  if (blCreateShortcut) then sTemp:=scc.AddShortCut(sTemp);
  RegSetValueEx(hRes, PChar(sID), 0, REG_SZ, PChar(sTemp), Length(sTemp) + 1);
end;


begin

  // get the EXE file name with full path
  sProgEXE:=Application.ExeName;

  // extract the path, make it RTL
  sProgPath:=TStrPlus.RTLPath(ExtractFilePath(sProgEXE));

  // file types, install or uninstall?
  if (blFileTypes) then begin

    // associate .BFA files...

    // set the basic key
    if (not blStrResOnly) then
      MakeRootKey('.' + BFAFILE_EXTENSION, BFAFILE_KEY);

    // set file description
    MakeRootKey(BFAFILE_KEY, sr.Get(STRRES_ID, '000'));

    // link to icon
    if (not blStrResOnly) then
      MakeRootKey(BFAFILE_KEY + '\DefaultIcon', sProgEXE + ',1');

    // link to EXE
    if (not blStrResOnly) then
      MakeRootKey(BFAFILE_KEY + '\Shell\Open\Command', sProgEXE + ' "%1"');

    // associate .BFJ files...

    // set basic key
    if (not blStrResOnly) then
      MakeRootKey('.' + BFJFILE_EXTENSION, BFJFILE_KEY);

    // set file description
    MakeRootKey(BFJFILE_KEY, sr.Get(STRRES_ID, '001'));

    // link to icon
    if (not blStrResOnly) then
      MakeRootKey(BFJFILE_KEY + '\DefaultIcon', sProgEXE + ',2');

    // link to EXE
    if (not blStrResOnly) then
      MakeRootKey(BFJFILE_KEY + '\Shell\Open\Command', sProgEXE + ' "%1"');
  end
  else begin

    // remove all keys
    RegDeleteKey(HKEY_CLASSES_ROOT, '.' + BFAFILE_EXTENSION);
    RegDeleteKey(HKEY_CLASSES_ROOT, BFAFILE_KEY);
    RegDeleteKey(HKEY_CLASSES_ROOT, '.' + BFJFILE_EXTENSION);
    RegDeleteKey(HKEY_CLASSES_ROOT, BFJFILE_KEY);
  end;


  // shell extensions, install or uninstall?
  if (blShellExts) then begin

    // extension title
    if (not blStrResOnly) then
      MakeRootKey('CLSID\' + SHELLEXT_GUID, SHELLEXT_TITLE);

    // DLL location
    if (not blStrResOnly) then
      MakeRootKey('CLSID\' + SHELLEXT_GUID + '\InProcServer32',
                  sProgPath + SHELLEXT_DLL);

    // magic something we dunno
    if (not blStrResOnly) then
      MakeRootKey('CLSID\' + SHELLEXT_GUID + '\InProcServer32',
                  'Apartment', 'ThreadingModel');

    // add menu handler for files _and_ folders
    if (not blStrResOnly) then begin
      MakeRootKey('*\shellex\ContextMenuHandlers\' + SHELLEXT_MENU,
                  SHELLEXT_GUID);
      MakeRootKey('Folder\shellex\ContextMenuHandlers\' + SHELLEXT_MENU,
                  SHELLEXT_GUID);
    end;

    // some trust stuff
    if (not blStrResOnly) then
      MakeKey(HKEY_LOCAL_MACHINE,
              'SOFTWARE\Microsoft\Windows\CurrentVersion\Shell Extensions\' +
              'Approved\' + SHELLEXT_GUID,
              SHELLEXT_TITLE);

    // set the menu captions (do it in one direct strike)
    RegOpenKey(HKEY_CLASSES_ROOT,
               '*\shellex\ContextMenuHandlers\' + SHELLEXT_MENU,
               hRes);
    scc:=TShortcutChecker.Create;
    SetLocalizedStringKey(REGVAL_CAPTION);
    scc.Reset;
    SetLocalizedStringKey(REGVAL_ENCRYPT);
    SetLocalizedStringKey(REGVAL_DECRYPT);
    SetLocalizedStringKey(REGVAL_WIPE);
    SetLocalizedStringKey(REGVAL_REENCRYPT);
    SetLocalizedStringKey(REGVAL_DESLACK);
    SetLocalizedStringKey(REGVAL_WORKWITH);
    SetLocalizedStringKey(REGVAL_VIEW);
    SetLocalizedStringKey(REGVAL_ERRMESS1     , False);
    SetLocalizedStringKey(REGVAL_ERRMESS2     , False);
    SetLocalizedStringKey(REGVAL_HELPCAPTION  , False);
    SetLocalizedStringKey(REGVAL_HELPENCRYPT  , False);
    SetLocalizedStringKey(REGVAL_HELPDECRYPT  , False);
    SetLocalizedStringKey(REGVAL_HELPWIPE     , False);
    SetLocalizedStringKey(REGVAL_HELPREENCRYPT, False);
    SetLocalizedStringKey(REGVAL_HELPDESLACK  , False);
    SetLocalizedStringKey(REGVAL_HELPWORKWITH , False);
    SetLocalizedStringKey(REGVAL_HELPVIEW     , False);
    scc.Destroy;
    if (not blStrResOnly) then
      RegSetValueEx(hRes, REGVAL_BFACSEXE, 0, REG_SZ,
                    PChar(sProgEXE), Length(sProgEXE) + 1);
    RegCloseKey(hRes);

  end
  else begin

    RegDeleteKey(HKEY_CLASSES_ROOT, 'CLSID\' + SHELLEXT_GUID);
    RegDeleteKey(HKEY_CLASSES_ROOT,
                 '*\shellex\ContextMenuHandlers\' + SHELLEXT_MENU);
    RegDeleteKey(HKEY_CLASSES_ROOT,
                 'Folder\shellex\ContextMenuHandlers\' + SHELLEXT_MENU);
    RegDeleteKey(HKEY_LOCAL_MACHINE,
                 'SOFTWARE\Microsoft\Windows\CurrentVersion\Shell Extensions\'
                 + 'Approved\' + SHELLEXT_GUID);
  end;


end;



class procedure TRegistryKeys.Check(var vblFileTypes : Boolean;
                                    var vblShellExts : Boolean);

function KeyExists(const sKey : String;
                   hRoot : HKEY = HKEY_CLASSES_ROOT) : Boolean;
var
  hRes : HKEY;
begin
  Result:=(RegOpenKey(hRoot, PChar(sKey), hRes) = ERROR_SUCCESS);
  if (Result) then
    RegCloseKey(hRes);
end;

begin

  // (we check for the characteristoc root keys only)

  // check the file types
  vblFileTypes:=False;
  if (KeyExists('.' + BFAFILE_EXTENSION)) then
    if (KeyExists(BFAFILE_KEY)) then
      if (KeyExists('.' + BFJFILE_EXTENSION)) then
        if (KeyExists(BFJFILE_KEY)) then
          vblFileTypes:=True;

  // check the shell extension
  vblShellExts:=False;
  if (KeyExists('CLSID\' + SHELLEXT_GUID)) then
    if (KeyExists('*\shellex\ContextMenuHandlers\' + SHELLEXT_MENU)) then
      if (KeyExists('Folder\shellex\ContextMenuHandlers\' + SHELLEXT_MENU)) then
        if (KeyExists('SOFTWARE\Microsoft\Windows\CurrentVersion\' +
                      'Shell Extensions\Approved\' + SHELLEXT_GUID,
                      HKEY_LOCAL_MACHINE)) then
          vblShellExts:=True;
end;




end.
