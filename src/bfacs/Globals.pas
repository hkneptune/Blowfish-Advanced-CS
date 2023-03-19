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
  all global stuff of the application
}


unit Globals;

{$I config.inc}

interface
uses ComCtrls,
     SysUtils,
     Windows,
     StringRes,
     Configuration,
     CallBack,
     FileBrowser,
     KeyCache,
     RandomManager,
     Options,
     KeyChecker,
     CipherManager,
     MessageCallBack,
     Win32Diagnosis,
     BFAViewer,
     BFAWorkWith,
     Favorites;


// startup exception
type
  EGlobalsError = class(Exception);


// language codes
const
  LANGUAGE_CFG = -1;
  LANGUAGE_US  = 0;
  LANGUAGE_DE  = 1;
  LANGUAGE_DEF = LANGUAGE_US;


// our global resources
type
  TGlobals = class
  private
    m_cfg    : TConfiguration;
    m_sr     : TStrRes;
    m_fb     : TFileBrowser;
    m_favs   : TFavorites;
    m_kcache : TKeyCache;
    m_rnd    : TRandomManager;
    m_opts   : TOptions;
    m_kcheck : TKeyChecker;
    m_cipMng : TCipherManager;
    m_wdiag  : TWin32Diagnosis;
    m_bview  : TBFAViewer;
    m_bwowi  : TBFAWorkWith;
    m_pinput : TPasswordInput;


  protected
    // to show a warning during the startup
    // -> the message to show
    // <- True: user said yes / False: user said no
    function ShowStartupWarning(const sMessage : String) : Boolean;

    // shows a startup abortion message
    // -> the message to show
    procedure ShowStartupAbort(sMessage : String);

    // internal routine to cleanup the global instances
    procedure Cleanup;

    // to create a default configuration
    procedure MakeDefCfg;

  public
    // constructor
    // -> list view control (for the file browser)
    // -> password input control
    // -> callback for the browser
    // -> key cache expiration progress callback
    // -> message(box) callback for the startup of the cipher manager
    // -> callback to show the startup progress (if wished to)
    // exception: EGlobalsError error occured
    constructor Create(browserView : TListView;
                       passwInput : TPasswordInput;
                       bcb : TCallBack;
                       kccb : TKeyCacheCallBack;
                       mcb : TMessageCallBack;
                       cb : TCallBack = Nil);

    // destructor
    destructor Destroy; override;

    // shutdown proc, called before the destruction
    // -> simple message callback to show the progress
    procedure ShutDown(cb : TCallBack);

    // (re)loads string resources
    // -> lang. ID (see LANGUAGE_xxx)
    // <- True: success / False: resources were not changed
    function ChangeStringResources(nLangID : Integer = LANGUAGE_CFG) : Boolean;

    // gets the current language ID
    // <- lang. ID
    function GetLangID : Integer;

    // gets the current language
    // <- language ID (see LANGUAGE_xxx)
    function GetLanguage : Integer;

    // we keep some the nams of the getters as short as possible, because they
    // are used very frequently in complex and long call expressions...

    // gets the configuration
    // <- configuration
    function GetCfg : TConfiguration;

    // gets the string resources
    // <- string resources
    function GetSr : TStrRes;

    // gets the file browser
    // <- file browser
    function GetFileBrowser : TFileBrowser;

    // gets the favorites
    // <- favorites
    function GetFavorites : TFavorites;

    // gets the key vache
    // <- key vache
    function GetKeyCache : TKeyCache;

    // gets the random manager
    // <- random manager
    function GetRndMng : TRandomManager;

    // gets the options
    // <- options
    function GetOpts : TOptions;

    // gets the key checker
    // <- key checker
    function GetKeyChecker : TKeyChecker;

    // gets the cipher manager
    // <- cipher manager
    function GetCipMng : TCipherManager;

    // gets the Win32 diagnosis unit
    function GetWinDiag : TWin32Diagnosis;

    // gets the BFA viewer
    // <- BFA viewer
    function GetBFAViewer : TBFAViewer;

    // gets the BFA-work-with module
    // <- BFA worker
    function GetBFAWorkWith : TBFAWorkWith;

    // gets the password input
    // <- password input module
    function GetPasswordInput : TPasswordInput;

  end;



// the globals are stored here, but must be instanciated from outside
var
  _globals : TGlobals;



implementation
uses Forms,
     General,
     StringPlus;


//////////////////////////// TGlobals ////////////////////////////


// our censor
type
  TGlobalCensor = class(TConfigurationCensor)
  private
    m_opts : TOptions;

  public
    // constructor
    // -> options (to get the "save sensitive settings flag)
    constructor Create(opts : TOptions);

    // censor routine
    // -> our config. instance to filter
    procedure Censor(cfg : TConfiguration); override;

  end;


constructor TGlobalCensor.Create(opts : TOptions);
begin
  m_opts:=opts;
end;


procedure TGlobalCensor.Censor(cfg : TConfiguration);
var
  cDrive : Char;
  sTemp  : String;
begin
  // do have to censor?
  if (not m_opts.GetCfg.GetBooleanOption(OPTIONS_CFGID_SAVESENSSETS)) then begin

    // clear browser settings
    with cfg.GetSection(FB_CFG_ID) do begin
      for cDrive:='A' to 'Z' do
        FixStringOption(FB_CFGID_DRIVEPATH_PREFIX + cDrive, '');
    end;

    // clear the options
    with cfg.GetSection(OPTS_CFG_ID) do begin
      FixStringOption(OPTIONS_CFGID_KEYDISKPATH, 'A:\BFACSKEY');
      FixStringOption(OPTIONS_CFGID_LASTTARGETPATH, '');
      sTemp:=TWin32Diagnosis.GetTemporaryDirectory;
      FixStringOption(OPTIONS_CFGID_LASTWORKPATH, sTemp);
      FixStringOption(OPTIONS_CFGID_TEMPVIEWPATH, sTemp);
    end;

    // reset the cipher manager (instead of changing the config. we just
    // let the manager do it by itself)
    _globals.GetCipMng.SetDefaultCiphers;
    
    // the same with the work with favorites
    _globals.getBFAWorkWith.GetFavorites.Clear;

    // (FIXME: anyone...err...anything else worth to oppress?)

  end;
end;



// the configuration file name
const
  CFGFILE_NAME = PROGRAM_SHORTNAME + '.ini';


// configuration file entries for the string resources
const
  STRRES_CFG_SECTION = 'COUNTRY_RESOURCES';
  STRRES_CFG_LANGID  = 'LANGID';

// lookup table for the string resources
const
  STRRES_FILES : array[0..1] of String = (
    PROGRAM_SHORTNAME + '_US.sr',
    PROGRAM_SHORTNAME + '_DE.sr'
  );

// default language (index)
const
  STRRES_DEF_LANGID = 0;



function TGlobals.ShowStartupWarning(const sMessage : String) : Boolean;
begin
  Result:=(Application.MessageBox(PChar(sMessage),
                                  PChar(PROGRAM_NAME + ' - Startup Warning'),
                                  MB_ICONEXCLAMATION or MB_YESNO) = IDYES);
end;


procedure TGlobals.ShowStartupAbort(sMessage : String);
begin
  Application.MessageBox(PChar(sMessage),
                         PChar(PROGRAM_NAME + ' - ABORTED'),
                         MB_ICONSTOP);
end;


procedure TGlobals.Cleanup;
begin
  if (m_fb <> Nil) then  // (must go first!)
    m_fb.Destroy;
  if (m_favs <> Nil) then
    m_favs.Destroy;
  if (m_kcache <> Nil) then
    m_kcache.Destroy;
  if (m_rnd <> Nil) then
    m_rnd.Destroy;
  if (m_opts <> Nil) then
    m_opts.Destroy;
  if (m_kcheck <> Nil) then
    m_kcheck.Destroy;
  if (m_cipMng <> Nil) then
    m_cipMng.Destroy;
  if (m_wdiag <> Nil) then
    m_wdiag.Destroy;
  if (m_bview <> Nil) then
    m_bview.Destroy;
  if (m_bwowi <> Nil) then
    m_bwowi.Destroy;
{$ifdef m_m_SHAREWARE}
  if (m_regKey <> Nil) then
    m_regKey.Destroy;
{$endif}
  if (m_cfg <> Nil) then   // (m_cfg and m_sr always at the end!)
    m_cfg.Destroy;
  if (m_sr <> Nil) then
    m_sr.Destroy;
end;



constructor TGlobals.Create(browserView : TListView;
                            passwInput : TPasswordInput;
                            bcb : TCallBack;
                            kccb : TKeyCacheCallBack;
                            mcb : TMessageCallBack;
                            cb : TCallBack);
var
  blNoCfgFile : Boolean;
  blNoResFile : Boolean;
  sEXERoot    : String;

begin
  // first reset all global instances
  m_cfg:=Nil;
  m_sr:=Nil;
  m_fb:=Nil;
  m_favs:=Nil;
  m_kcache:=Nil;
  m_rnd:=Nil;
  m_opts:=Nil;
  m_kcheck:=Nil;
  m_cipMng:=Nil;
  m_wdiag:=Nil;
  m_bview:=Nil;
  m_bwowi:=Nil;
{$ifdef m_m_SHAREWARE}
  m_regKey:=Nil;
{$endif}

  { ******************************************************************
    Don't change the order of the initialisation calls of the single
    modules. Some of them are cascaded in logical orders and dependant
    from each other!
    ******************************************************************}

  try
    // get the root of the application
    sEXERoot:=TStrPlus.RTLPath(ExtractFilePath(Application.ExeName));

    // show that we're loading the configuration
    cb.SetSimpleMessageState(True);
    cb.SetMessage('Loading configuration...');
    cb.CallBack;

    // create the configuration
    blNoCfgFile:=False;
    m_cfg:=TConfiguration.Create;
    try
      m_cfg.LoadFromFile(sEXERoot + CFGFILE_NAME);
    except
      on EConfigurationError do begin
        // check if the configuration file exists
        blNoCfgFile:=True;
        if (FileExists(sEXERoot + CFGFILE_NAME)) then begin
          // yes, we don't have a first program run, so let the user know that
          // something strange has happened
          if (not ShowStartupWarning('Error loading configuration file "' +
                                     sEXERoot + CFGFILE_NAME + '".' + #13#10 +
                                     'Continue?')) then begin
            Cleanup;
            raise EGlobalsError.Create('error loading configuration file');
          end
        end
        else begin
          // create the default resource file in the configuration
          MakeDefCfg;
        end;
      end;
    end;

    // show that we're loading the resources
    cb.SetMessage('Loading resources...');
    cb.CallBack;

    // load the string resources
    if (not ChangeStringResources) then begin
      // ask the user to take a second change and load the default strings,
      // don't do this if we already tried to load them
      blNoResFile:=True;
      if (not blNoCfgFile) then
        if (ShowStartupWarning('Error loading resources from "' +
                                sEXERoot + CFGFILE_NAME + '".' + #13#10 +
                               'Try to load default resources?')) then begin
          // ok, let's try to load the default resources (same operation as
          // above)
          MakeDefCfg;
          blNoResFile:=not ChangeStringResources;
        end;
      if (blNoResFile) then begin
        // no strings, no fun
        ShowStartupAbort('Cannot start program due to missing resources.' +
                         #13#10 + 'Please check out that there is at least ' +
                         'one valid string resource file named ' +
                         AnsiUpperCase(STRRES_FILES[0]) +
                         ' in the application path.');
        raise EGlobalsError.Create('missing resources');
      end;
    end;

    // store the given password input module
    m_pinput:=passwInput;

    // prepare the options
    m_opts:=TOptions.Create(m_cfg);

    // load the ciphers into the manager
    m_cipMng:=TCipherManager.Create(m_cfg, True);

    // init. the random generator
    m_rnd:=TRandomManager.Create(RANDOMMANAGER_SOURCE_YARROW);

    // set up the key cache
    m_kcache:=TKeyCache.Create(m_rnd,
                               kccb);

    // care for a BFA viewer
    m_bview:=TBFAViewer.Create(m_sr, m_opts, m_kcache);

    // just imagine: someone might even want to work with .BFA files!
    m_bwowi:=TBFAWorkWith.Create(m_sr,
                                 m_opts,
                                 m_cfg,
                                 m_kcache,
                                 m_cipMng,
                                 m_rnd);

    // create the file browser (no callback here, execution is just too fast)
    m_fb:=TFileBrowser.Create(browserView,
                              m_sr,
                              m_cfg,
                              mcb,
                              bcb,
                              TFileBrowserBFASupport.Create(m_cipMng,
                                                            passwInput));

    // create the favorites (we want them sorted and case insensitive and use
    // the default configuration ID for it)
    m_favs:=TFavorites.Create(m_cfg);

    // prepare the key checker
    m_kcheck:=TKeyChecker.Create(m_cfg, m_rnd);

    // make the Win32 diagnosis
    m_wdiag:=TWin32Diagnosis.Create(m_sr);

    // at last add a censor to the configuration
    m_cfg.SetCensor(TGlobalCensor.Create(m_opts));

  except
    on e: Exception do begin
      // (this should never happen)
      raise EGlobalsError.Create(e.Message);
    end;  
  end;
end;



destructor TGlobals.Destroy;
begin
  // destroy all other global instances
  CleanUp;
end;


procedure TGlobals.ShutDown(cb : TCallBack);
var
  sCfgFile : String;
begin
  // prepare the callback
  cb.SetSimpleMessageState(True);

  // let the file browser store its settings
  m_fb.FixSettings;

  // save the configuration
  cb.SetMessage(m_sr.Get('GLOBALS', '000'));
  cb.CallBack;

  sCfgFile:=TStrPlus.RTLPath(ExtractFilePath(Application.ExeName)) +
            CFGFILE_NAME;
  try
    m_cfg.SaveToFile(sCfgFile);
  except
    // warn if we cannot save to configuration
    on EConfigurationError do
      Application.MessageBox(PChar(Format(m_sr.Get('GLOBALS', '001'),
                                   [sCfgFile])),
                             PChar(PROGRAM_NAME + m_sr.Get('GLOBALS', '002')),
                             MB_ICONEXCLAMATION);
  end;
end;



function TGlobals.ChangeStringResources(nLangID : Integer = LANGUAGE_CFG)
                                          : Boolean;
var
  blStartUp : Boolean;
  sResFile  : String;
  sresSect  : TConfigurationSection;
begin

  // assume an error
  Result:=False;

  // startup?
  blStartUp:=(nLangID = LANGUAGE_CFG);
  if (blStartUp) then
    try
      nLangID:=GetLanguage;
    except
      on EConfigurationError do begin
        MakeDefCfg;
        nLangID:=LANGUAGE_DEF;
      end;
    end;
  sResFile:=TStrPlus.RTLPath(ExtractFilePath(Application.ExeName)) +
            STRRES_FILES[nLangID];

  // now load the string resources
  try
    if (blStartUp) then
      m_sr:=TStrRes.Create(sResFile)
    else
      m_sr.NewStringResource(sResFile);
  except
    on ETStrResLoadError do
      Exit;
  end;

  // set the new help file and content (full pathed)
  Application.HelpFile:=
    TStrPlus.RTLPath(ExtractFilePath(Application.EXEName)) +
    m_sr.GetCountryConfig.GetHelpFile;

  // inform all listeners, i.n.
  if (not blStartUp) then
    m_sr.InformListeners;

  // save the new resource file name in the configuration, i.n.
  if (not blStartUp) then begin
    sresSect:=m_cfg.GetSection(STRRES_CFG_SECTION);
    sresSect.FixIntegerOption(STRRES_CFG_LANGID, nLangID);
  end;

  // success
  Result:=True;
end;


function TGlobals.GetLanguage : Integer;
begin
  Result:=m_cfg.GetSection(STRRES_CFG_SECTION)
               .GetIntegerOption(STRRES_CFG_LANGID);
end;



function TGlobals.GetCfg : TConfiguration;
begin
  Result:=m_cfg;
end;

function TGlobals.GetSr : TStrRes;
begin
  Result:=m_sr;
end;

function TGlobals.GetFileBrowser : TFileBrowser;
begin
  Result:=m_fb;
end;

function TGlobals.GetFavorites : TFavorites;
begin
  Result:=m_favs;
end;

function TGlobals.GetKeyCache : TKeyCache;
begin
  Result:=m_kcache;
end;

function TGlobals.GetRndMng : TRandomManager;
begin
  Result:=m_rnd;
end;

function TGlobals.GetOpts : TOptions;
begin
  Result:=m_opts;
end;

function TGlobals.GetKeyChecker : TKeyChecker;
begin
  Result:=m_kcheck;
end;

function TGlobals.GetCipMng : TCipherManager;
begin
  Result:=m_cipMng;
end;

function TGlobals.GetWinDiag : TWin32Diagnosis;
begin
  Result:=m_wdiag;
end;

function TGlobals.GetBFAViewer : TBFAViewer;
begin
  Result:=m_bview;
end;

function TGlobals.GetBFAWorkWith : TBFAWorkWith;
begin
  Result:=m_bwowi;
end;

function TGlobals.GetPasswordInput : TPasswordInput;
begin
  Result:=m_pinput;
end;

function TGlobals.GetLangID : Integer;
begin
  Result:=m_cfg.GetSection(STRRES_CFG_SECTION)
               .GetIntegerOption(STRRES_CFG_LANGID);
end;

procedure TGlobals.MakeDefCfg;
begin
  m_cfg.GetSection(STRRES_CFG_SECTION).FixIntegerOption(STRRES_CFG_LANGID,
                                                        STRRES_DEF_LANGID,
                                                        True);
end;

initialization

end.
