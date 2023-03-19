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
  handles all non module specific options, they're store in the
  configuration and thus can be accessed from any other module
}

unit Options;

interface
uses Configuration;

// the configuration section ID
const
  OPTS_CFG_ID = 'OPTIONS';


// the configuration IDs
const
  OPTIONS_CFGID_KEYCACHEEXPIRETIME  = 'KeyCacheExpireTime';
  OPTIONS_CFGID_KEYCACHENEVEREXPIRE = 'KeyCacheNeverExpire';
  OPTIONS_CFGID_KEYDISKPATH         = 'KeyDiskPath';
  OPTIONS_CFGID_PASSWORDCHAR        = 'PasswordChar';
  OPTIONS_CFGID_MULTIKEY            = 'MultiKey';

  OPTIONS_CFGID_MAXERRORS           = 'MaxErrors';
  OPTIONS_CFGID_CLOSEAFTERWORK      = 'CloseAfterWork';
  OPTIONS_CFGID_DOUBLECLICKWORK     = 'DoubleClickWork';

  //////////// jobfile relevant opts ////////////

  OPTIONS_CFGID_USECACHEDKEY        = 'UseCachedKey';
  OPTIONS_CFGID_SHOWPASSWORD        = 'ShowPassword';
  OPTIONS_CFGID_USEKEYDISK          = 'UseKeyDisk';
  OPTIONS_CFGID_CACHEKEY            = 'CacheKey';
  OPTIONS_CFGID_AUTOCONFIRMATION    = 'AutoConfirmation';

  OPTIONS_CFGID_CONFIRMOPERATIONS   = 'ConfirmOperations';

  OPTIONS_CFGID_EXCLUDEARCHIVE      = 'ExcludeArchive';
  OPTIONS_CFGID_EXCLUDEREADONLY     = 'ExcludeReadOnly';
  OPTIONS_CFGID_EXCLUDEHIDDEN       = 'ExcludeHidden';
  OPTIONS_CFGID_EXCLUDESYSTEM       = 'ExcludeSystem';

  OPTIONS_CFGID_WIPING              = 'Wiping';
  OPTIONS_CFGID_REMOVEEMPTYDIRS     = 'RemoveEmptyDirs';

  OPTIONS_CFGID_RELATIVEPATHS       = 'RelativePaths';
  OPTIONS_CFGID_KEEPDIRECTORIES     = 'KeepDirectories';
  OPTIONS_CFGID_USETARGETPATH       = 'UseTargetPath';
  OPTIONS_CFGID_LASTTARGETPATH      = 'LastTargetPath';
  OPTIONS_CFGID_SHOWJOBREPORT       = 'ShowJobReport';
  OPTIONS_CFGID_REMOVESOURCEFILES   = 'RemoveSourceFiles';
  OPTIONS_CFGID_OVERWRITEEXISTING   = 'OverwriteExisting';
  OPTIONS_CFGID_NOCACHE             = 'NoCache';
  OPTIONS_CFGID_KEEPDATETIME        = 'KeepDateTime';
  OPTIONS_CFGID_KEEPATTRIBUTES      = 'KeepAttributes';

  OPTIONS_CFGID_RENAME              = 'Rename';
  OPTIONS_CFGID_RANDOMRENAME        = 'RandomRename';
  OPTIONS_CFGID_TRYRENAME83         = 'TryRename83';
  OPTIONS_CFGID_ADDEXTENSION        = 'AddExtension';
  OPTIONS_CFGID_USECOMPRESSION      = 'UseCompression';
  OPTIONS_CFGID_COMPRESS            = 'Compress';
  OPTIONS_CFGID_FORCECOMPRESS       = 'ForceCompress';
  OPTIONS_CFGID_SKIPENCRYPTED       = 'SkipEncrypted';
  OPTIONS_CFGID_WRITEPROTECT        = 'WriteProtect';
  OPTIONS_CFGID_MASKNAME            = 'MaskName';
  OPTIONS_CFGID_MASKEXT             = 'MaskExt';
  OPTIONS_CFGID_NOCOMPRESSTYPES     = 'NoCompressTypes';
  OPTIONS_CFGID_STOREPATH           = 'StorePath';

  OPTIONS_CFGID_IGNORECRC32         = 'IgnoreCRC32';
  OPTIONS_CFGID_RESTOREPATH         = 'RestorePath';
  OPTIONS_CFGID_ACCEPTTRUNCATED     = 'AcceptTruncated';

  ///////////////////////////////////////////////

  OPTIONS_CFGID_LASTWORKPATH        = 'LastWorkPath';
  OPTIONS_CFGID_TEMPVIEWPATH        = 'TempViewPath';
  OPTIONS_CFGID_OVERWRITEVIEWED     = 'OverwriteViewed';

  OPTIONS_CFGID_SHOWHINTS           = 'ShowHints';
  OPTIONS_CFGID_SHOWBUTTONS         = 'ShowButtons';
  OPTIONS_CFGID_SHOWBROWSERTOOLS    = 'ShowBrowserTools';

  OPTIONS_CFGID_SAVESENSSETS        = 'SaveSensSets';
  OPTIONS_CFGID_COLORPROGRESS       = 'ColorProgress';
  OPTIONS_CFGID_TRAYICON            = 'TrayIcon';
  OPTIONS_CFGID_SHOWCFGFILEERRORS   = 'ShowCfgFileErrors';


// the option class itself is rather simple
type
  TOptions = class
  private
    // members
    m_config : TConfigurationSection;

  public

    // constructor
    // -> the configuration where to load & store the preferences
    constructor Create(cfg : TConfiguration);

    // gets the options section _reference_
    // <- the options section
    function GetCfg : TConfigurationSection;

    // restore the fixed settings
    procedure Restore;

  end;

implementation
uses SysUtils,
     Forms,
     General,
     BFAFile,
     BFManager,
     Win32Diagnosis,
     RandomManager;


//////////////////////////// TOptions ////////////////////////////



// configuration checker, the most important part of this unit
type
  TOptionsCC = class(TConfigurationChecker)
  private
    m_parent : TOptions;
  public
    constructor Create(parent : TOptions);
    procedure RunCheck(section : TConfigurationSection); override;
  end;

constructor TOptionsCC.Create(parent : TOptions);
begin
  m_parent:=parent;  // (currently not used by the checker)
end;

procedure TOptionsCC.RunCheck(section : TConfigurationSection);
begin
  with section do begin

    CheckInt(section, OPTIONS_CFGID_KEYCACHEEXPIRETIME, 60);
    CheckBool(section, OPTIONS_CFGID_KEYCACHENEVEREXPIRE, False);
    CheckString(section,
                OPTIONS_CFGID_KEYDISKPATH,
                'A:\' + UpperCase(PROGRAM_SHORTNAME) + 'KEY');
    CheckBool(section, OPTIONS_CFGID_USEKEYDISK, False);
    CheckBool(section, OPTIONS_CFGID_USECACHEDKEY, True);
    CheckBool(section, OPTIONS_CFGID_MULTIKEY, False);
    CheckBool(section, OPTIONS_CFGID_CACHEKEY, False);
    CheckBool(section, OPTIONS_CFGID_SHOWPASSWORD, False);
    CheckBool(section, OPTIONS_CFGID_AUTOCONFIRMATION, True);
    CheckBool(section, OPTIONS_CFGID_USETARGETPATH, False);
    CheckBool(section, OPTIONS_CFGID_KEEPDIRECTORIES, True); //???
    CheckString(section, OPTIONS_CFGID_LASTTARGETPATH, '');
    CheckChar(section, OPTIONS_CFGID_PASSWORDCHAR, '*');

    CheckBool(section, OPTIONS_CFGID_REMOVEEMPTYDIRS, True);
    CheckBool(section, OPTIONS_CFGID_RELATIVEPATHS, True);
    CheckInt(section, OPTIONS_CFGID_WIPING, BFM_WIPE_SIMPLE);
    CheckInt(section, OPTIONS_CFGID_COMPRESS, BFAFILE_COMPRESS_DEFL);
    CheckBool(section, OPTIONS_CFGID_USECOMPRESSION, False);

    CheckBool(section, OPTIONS_CFGID_SHOWHINTS, True);
    CheckBool(section, OPTIONS_CFGID_SHOWBUTTONS, True);
    CheckBool(section, OPTIONS_CFGID_SHOWBROWSERTOOLS, True);

    CheckBool(section, OPTIONS_CFGID_SHOWJOBREPORT, True);

    CheckBool(section, OPTIONS_CFGID_REMOVESOURCEFILES, False);
    CheckBool(section, OPTIONS_CFGID_OVERWRITEEXISTING, False);
    CheckBool(section, OPTIONS_CFGID_NOCACHE, False);

    CheckBool(section, OPTIONS_CFGID_FORCECOMPRESS, False);
    CheckBool(section, OPTIONS_CFGID_RENAME, False);
    CheckBool(section, OPTIONS_CFGID_RANDOMRENAME, True);
    CheckBool(section, OPTIONS_CFGID_TRYRENAME83, False);
    CheckBool(section, OPTIONS_CFGID_ADDEXTENSION, True);
    CheckBool(section, OPTIONS_CFGID_KEEPDATETIME, False);
    CheckBool(section, OPTIONS_CFGID_KEEPATTRIBUTES, False);
    CheckBool(section, OPTIONS_CFGID_STOREPATH, False);
    CheckBool(section, OPTIONS_CFGID_SKIPENCRYPTED, True);
    CheckBool(section, OPTIONS_CFGID_WRITEPROTECT, False);
    CheckString(section, OPTIONS_CFGID_MASKNAME, 'file');
    CheckString(section, OPTIONS_CFGID_MASKEXT, 'dat');
    CheckString(section, OPTIONS_CFGID_NOCOMPRESSTYPES,
                'zip,arj,rar,lzh,cab,bz2,gz,gzip,jar');

    CheckBool(section, OPTIONS_CFGID_IGNORECRC32, False);
    CheckBool(section, OPTIONS_CFGID_RESTOREPATH, False);
    CheckBool(section, OPTIONS_CFGID_ACCEPTTRUNCATED, False);

    CheckString(section,
                OPTIONS_CFGID_LASTWORKPATH,
                ExtractFilePath(Application.Exename));

    CheckBool(section, OPTIONS_CFGID_EXCLUDEARCHIVE, False);
    CheckBool(section, OPTIONS_CFGID_EXCLUDEREADONLY, False);
    CheckBool(section, OPTIONS_CFGID_EXCLUDEHIDDEN, True);
    CheckBool(section, OPTIONS_CFGID_EXCLUDESYSTEM, True);

    CheckString(section, OPTIONS_CFGID_TEMPVIEWPATH,
                TWin32Diagnosis.GetTemporaryDirectory);
    CheckBool(section, OPTIONS_CFGID_OVERWRITEVIEWED, True);

    CheckBool(section, OPTIONS_CFGID_CONFIRMOPERATIONS, True);
    CheckInt(section, OPTIONS_CFGID_MAXERRORS, 10);

    CheckBool(section, OPTIONS_CFGID_CLOSEAFTERWORK, True);
    CheckBool(section, OPTIONS_CFGID_DOUBLECLICKWORK, False);

    CheckBool(section, OPTIONS_CFGID_SAVESENSSETS, True);

    CheckBool(section, OPTIONS_CFGID_COLORPROGRESS, True);

    CheckBool(section, OPTIONS_CFGID_TRAYICON, True);
    CheckBool(section, OPTIONS_CFGID_SHOWCFGFILEERRORS, True);

  end;
end;

// the rest is gonna be simple...

constructor TOptions.Create(cfg : TConfiguration);
var
  cc : TOptionsCC;
begin
  cc:=TOptionsCC.Create(Self);
  m_config:=cfg.GetSection(OPTS_CFG_ID, cc);
  cc.Destroy;
end;


function TOptions.GetCfg : TConfigurationSection;
begin
  Result:=m_config;
end;


procedure TOptions.Restore;
begin
  m_config.Restore;
end;


end.
