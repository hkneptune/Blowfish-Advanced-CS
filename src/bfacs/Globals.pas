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
  all general global stuff, both shared between UI and the command line
}

unit Globals;

interface

uses
  sysutils,
  General,
  StringRes,
  RandomManager,
  Options,
  CipherManager,
  KeyCache,
  CallBack,  
  MessageCallBack,
  Configuration;


// language codes
const
  LANGUAGE_CFG = -1;
  LANGUAGE_US  = 0;
  LANGUAGE_DE  = 1;
  LANGUAGE_XT  = 2;     // (XT = extended, whatever language that might be)
  LANGUAGE_DEF = LANGUAGE_US;

// startup exception
type
  EGlobalsError = class(Exception);

type
  TGlobals = class
  protected
    m_cfg          : TConfiguration;
    m_sr           : TStrRes;
    m_rnd          : TRandomManager;
    m_opts         : TOptions;
    m_cipMng       : TCipherManager;
    m_pinput       : TPasswordInput;
    m_sCfgFilePath : String;

  protected

    // to show a warning during the startup
    // -> message callback
    // -> the message to show
    // <- True: user said yes / False: user said no
    class function ShowStartupWarning(
        cbMsg : TMessageCallBack;
        const sMessage : String) : Boolean;

    // shows a startup abortion message
    // -> message callback
    // -> the message to show
    class procedure ShowStartupAbort(
        cbMsg : TMessageCallBack;
        sMessage : String);
  
    // internal routine to cleanup the global instances
    procedure Cleanup; virtual;

    // to create a default configuration
    procedure MakeDefCfg;

  public

    // constructor
    // -> configuration file path (or empty for default)
    // -> password input instance
    // -> callback to show the startup progress
    // -> message callback for to handle startup descisions
    // exception: EGlobalsError error occured
    constructor Create(
        sCfgFilePath : String;
        passwInput : TPasswordInput;
        cb : TCallBack;
        cbMsg : TMessageCallBack);

    // destructor
    destructor Destroy; override;

    // loads string resources from a file (usually for examination purposes)
    // -> language ID (must _not_ be LANGUAGE_CFG!)
    // <- resources (or Nil on error)
    function LoadStringResources(nLangID : Integer) : TStrRes;

    // (re)loads global string resources
    // -> lang. ID (see LANGUAGE_xxx)
    // <- True: success / False: resources were not changed
    function ChangeStringResources(nLangID : Integer = LANGUAGE_CFG) : Boolean;

    // gets the current language ID
    // <- lang. ID
    function GetLangID : Integer;

    // gets the current language
    // <- language ID (see LANGUAGE_xxx)
    function GetLanguage : Integer;

    // we keep some the names of the getters as short as possible, because they
    // are used very frequently in complex and long call expressions...

    // gets the configuration
    // <- configuration
    function GetCfg : TConfiguration;

    // gets the string resources
    // <- string resources
    function GetSr : TStrRes;

    // gets the random manager
    // <- random manager
    function GetRndMng : TRandomManager;

    // gets the options
    // <- options
    function GetOpts : TOptions;

    // gets the cipher manager
    // <- cipher manager
    function GetCipMng : TCipherManager;

    // gets the password input
    // <- password input module
    function GetPasswordInput : TPasswordInput;

  public

    // helper to load a configuration from a file
    // -> path to configuration file
    // -> callback for simple progress messages
    // <- configuration instance
    // exception: EGlobalsError if the loading failed
    class function LoadStaticConfig(
        const sFilePath : String;
        cb : TCallBack) : TConfiguration;
  end;

implementation

uses
  Forms,
  StringPlus;

///////////////////////////////////////////////////////////////////////////////

// the configuration file name
const
  CFGFILE_NAME = PROGRAM_SHORTNAME + '.ini';


// configuration file entries for the string resources
const
  STRRES_CFG_SECTION = 'COUNTRY_RESOURCES';
  STRRES_CFG_LANGID  = 'LANGID';

// lookup table for the string resources
const
  STRRES_FILES : array[0..2] of String = (
    PROGRAM_SHORTNAME + '_US.sr',
    PROGRAM_SHORTNAME + '_DE.sr',
    PROGRAM_SHORTNAME + '_XT.sr'
  );

// default language (index)
const
  STRRES_DEF_LANGID = 0;

///////////////////////////////////////////////////////////////////////////////

constructor TGlobals.Create(
        sCfgFilePath : String;
        passwInput : TPasswordInput;
        cb : TCallBack;
        cbMsg : TMessageCallBack);
var
  blNoCfgFile : Boolean;
  blNoResFile : Boolean;
  sEXERoot    : String;

begin
  // reset all members
  m_cfg:=Nil;
  m_sr:=Nil;
  m_rnd:=Nil;
  m_opts:=Nil;
  m_cipMng:=Nil;

  try

    // get the root of the application
    sEXERoot:=TStrPlus.RTLPath(ExtractFilePath(Application.ExeName));

    // show that we're loading the configuration
    cb.SetSimpleMessageState(True);
    cb.SetMessage('Loading configuration...');
    cb.CallBack;

    // if we don't have a configuration file name we'll choose a default one
    if (0 = Length(sCfgFilePath)) then begin
      sCfgFilePath:=sEXERoot + CFGFILE_NAME;
    end;

    // store the configuration file path (we might need it later on)
    m_sCfgFilePath:=sCfgFilePath;

    // create the configuration
    blNoCfgFile:=False;
    m_cfg:=TConfiguration.Create;
    try
      m_cfg.LoadFromFile(sCfgFilePath);
    except
      on EConfigurationError do begin
        // check if the configuration file exists
        blNoCfgFile:=True;
        if (FileExists(sEXERoot + CFGFILE_NAME)) then begin
          // yes, we don't have a first program run, so let the user know that
          // something strange has happened
          if (not ShowStartupWarning(cbMsg,
                                     'Error loading configuration file "' +
                                     sCfgFilePath + '".' + #13#10 +
                                     'Continue?')) then begin
            Cleanup;
            raise EGlobalsError.Create('error loading configuration file');
          end
        end
        else begin
          // set the default resource file in the configuration
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
        if (ShowStartupWarning(
          cbMsg,
          'Error loading resources from "' +
           sEXERoot + CFGFILE_NAME + '".' + #13#10 +
           'Try to load default resources?')) then begin
          // ok, let's try to load the default resources (same operation as
          // above)
          MakeDefCfg;
          blNoResFile:=not ChangeStringResources;
        end;
      if (blNoResFile) then begin
        ShowStartupAbort(
          cbMsg,
          'Cannot start program due to missing resources.' + #13#10 +
          'Please check out that there is at least one valid string ' + 
          'resource file named ' + AnsiUpperCase(STRRES_FILES[0]) +
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

  except
    on e: Exception do begin
      // (this should never happen)
      raise EGlobalsError.Create(e.Message);
    end;  
  end;
end;

///////////////////////////////////////////////////////////////////////////////

destructor TGlobals.Destroy;
begin
  CleanUp;
end;

///////////////////////////////////////////////////////////////////////////////

class function TGlobals.LoadStaticConfig(
        const sFilePath : String;
        cb : TCallBack) : TConfiguration;
begin

  cb.SetSimpleMessageState(True);
  cb.SetMessage('Loading configuration...');
  cb.CallBack;

  Result:=TConfiguration.Create;
  try
    Result.LoadFromFile(sFilePath);
  except
    on ece : EConfigurationError do begin
      raise EGlobalsError.Create(
        'error loading configuration file "' + sFilePath + '" - ' +
        ece.Message);
    end;
  end;
end;

///////////////////////////////////////////////////////////////////////////////

function TGlobals.LoadStringResources(
        nLangID : Integer) : TStrRes;
var
  sResFile : String;        
begin
  Result:=Nil;

  sResFile:=TStrPlus.RTLPath(ExtractFilePath(Application.ExeName)) +
            STRRES_FILES[nLangID];

  try
    Result:=TStrRes.Create(sResFile)
  except
    on ETStrResLoadError do
      Exit;
  end;
end;        

///////////////////////////////////////////////////////////////////////////////

function TGlobals.ChangeStringResources(
        nLangID : Integer = LANGUAGE_CFG) : Boolean;
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

///////////////////////////////////////////////////////////////////////////////

procedure TGlobals.MakeDefCfg;
begin
  m_cfg.GetSection(STRRES_CFG_SECTION).FixIntegerOption(
      STRRES_CFG_LANGID,
      STRRES_DEF_LANGID,
      True);
end;

///////////////////////////////////////////////////////////////////////////////

class function TGlobals.ShowStartupWarning(
    cbMsg : TMessageCallBack;
    const sMessage : String) : Boolean;
begin

  cbMsg.SetKindOf(MCB_KINDOF_WARNING);
  cbMsg.SetStyle(MCB_STYLE_YESNO);
  cbMsg.SetMessage(sMessage);
  cbMsg.CallBack;

  Result:=(MCB_RES_YES = cbMsg.GetResult);
end;

///////////////////////////////////////////////////////////////////////////////

function TGlobals.GetLangID : Integer;
begin
  Result:=m_cfg.GetSection(STRRES_CFG_SECTION)
               .GetIntegerOption(STRRES_CFG_LANGID);
end;

///////////////////////////////////////////////////////////////////////////////

function TGlobals.GetLanguage : Integer;
begin
  Result:=m_cfg.GetSection(STRRES_CFG_SECTION)
               .GetIntegerOption(STRRES_CFG_LANGID);
end;

///////////////////////////////////////////////////////////////////////////////

function TGlobals.GetCfg : TConfiguration;
begin
  Result:=m_cfg;
end;

///////////////////////////////////////////////////////////////////////////////

function TGlobals.GetSr : TStrRes;
begin
  Result:=m_sr;
end;

///////////////////////////////////////////////////////////////////////////////

function TGlobals.GetPasswordInput : TPasswordInput;
begin
  Result:=m_pinput;
end;

///////////////////////////////////////////////////////////////////////////////

function TGlobals.GetRndMng : TRandomManager;
begin
  Result:=m_rnd;
end;

///////////////////////////////////////////////////////////////////////////////

function TGlobals.GetOpts : TOptions;
begin
  Result:=m_opts;
end;

///////////////////////////////////////////////////////////////////////////////

function TGlobals.GetCipMng : TCipherManager;
begin
  Result:=m_cipMng;
end;

///////////////////////////////////////////////////////////////////////////////

procedure TGlobals.Cleanup;
begin
  if (m_rnd <> Nil) then
    m_rnd.Destroy;
  if (m_opts <> Nil) then
    m_opts.Destroy;
  if (m_cipMng <> Nil) then
    m_cipMng.Destroy;
  if (m_cfg <> Nil) then   // (m_cfg and m_sr always at the end!)
    m_cfg.Destroy;
  if (m_sr <> Nil) then
    m_sr.Destroy;
end;

///////////////////////////////////////////////////////////////////////////////

class procedure TGlobals.ShowStartupAbort(
    cbMsg : TMessageCallBack;
    sMessage : String);
begin

  cbMsg.SetKindOf(MCB_KINDOF_ERROR);
  cbMsg.SetStyle(MCB_STYLE_OK);
  cbMsg.SetMessage(sMessage);
  cbMsg.CallBack;
end;

end.
