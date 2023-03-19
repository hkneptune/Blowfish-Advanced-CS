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
  all global stuff for the GUI application
}


unit GlobalsGUI;

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
     KeyChecker,
     MessageCallBack,
     Win32Diagnosis,
     BFAViewer,
     BFAWorkWith,
     Globals,
     Favorites;


type
  TGlobalsGUI = class(TGlobals)
  private
    m_fb     : TFileBrowser;
    m_favs   : TFavorites;
    m_kcache : TKeyCache;
    m_kcheck : TKeyChecker;
    m_wdiag  : TWin32Diagnosis;
    m_bview  : TBFAViewer;
    m_bwowi  : TBFAWorkWith;

  protected

    procedure Cleanup; override;

  public

    // constructor
    // -> configuration file path (or empty for default)
    // -> list view control (for the file browser)
    // -> password input control
    // -> callback for the browser
    // -> key cache expiration progress callback
    // -> message(box) callback for the startup of the cipher manager
    // -> callback to show the startup progress (if wished to)
    // exception: EGlobalsError error occured
    constructor Create(sCfgFilePath : String;
                       browserView : TListView;
                       passwInput : TPasswordInput;
                       bcb : TCallBack;
                       kccb : TKeyCacheCallBack;
                       mcb : TMessageCallBack;
                       cb : TCallBack = Nil);

    // destructor
    destructor Destroy; override;

    // shutdown proc, called before the destruction, saves the configuration
    // -> simple message callback to show the progress
    procedure ShutDown(cb : TCallBack);

    // gets the file browser
    // <- file browser
    function GetFileBrowser : TFileBrowser;

    // gets the favorites
    // <- favorites
    function GetFavorites : TFavorites;

    // gets the key vache
    // <- key vache
    function GetKeyCache : TKeyCache;

    // gets the key checker
    // <- key checker
    function GetKeyChecker : TKeyChecker;
    
    // gets the Win32 diagnosis unit
    function GetWinDiag : TWin32Diagnosis;

    // gets the BFA viewer
    // <- BFA viewer
    function GetBFAViewer : TBFAViewer;

    // gets the BFA-work-with module
    // <- BFA worker
    function GetBFAWorkWith : TBFAWorkWith;
  end;


// the globals for the GUI version are stored here, but must be instanciated
// from outside
var
  _globals : TGlobalsGUI;


implementation
uses Forms,
     General,
     Options,
     StringPlus;


//////////////////////////// TGlobalsGUI ////////////////////////////


// our censor
type
  TGlobalsGUICensor = class(TConfigurationCensor)
  private
    m_globals : TGlobalsGUI;

  public
    // constructor
    // -> globals (to get the "save sensitive settings" flag)
    constructor Create(globals : TGlobalsGUI);

    procedure Apply(cfg : TConfiguration); override;
  end;


constructor TGlobalsGUICensor.Create(globals : TGlobalsGUI);
begin
  m_globals:=globals;
end;


procedure TGlobalsGUICensor.Apply(cfg : TConfiguration);
var
  cDrive : Char;
  sTemp  : String;
begin
  // do we have to censor?
  if (not m_globals.GetOpts.GetCfg.GetBooleanOption(
    OPTIONS_CFGID_SAVESENSSETS)) then begin

    // clear browser settings
    with cfg.GetSection(FB_CFG_ID) do begin
      for cDrive:='A' to 'Z' do
        FixStringOption(FB_CFGID_DRIVEPATH_PREFIX + cDrive, '');
    end;

    // clear the options with reasonable defaults
    with cfg.GetSection(OPTS_CFG_ID) do begin
      FixStringOption(OPTIONS_CFGID_KEYDISKPATH, 'A:\BFACSKEY');
      FixStringOption(OPTIONS_CFGID_LASTTARGETPATH, '');
      sTemp:=TWin32Diagnosis.GetTemporaryDirectory;
      FixStringOption(OPTIONS_CFGID_LASTWORKPATH, sTemp);
      FixStringOption(OPTIONS_CFGID_TEMPVIEWPATH, sTemp);
    end;

    // reset the cipher manager (instead of changing the config. we just
    // let the manager do it by itself)
    m_globals.GetCipMng.SetDefaultCiphers;

    // the same with the work with favorites
    m_globals.GetBFAWorkWith.GetFavorites.Clear;
  end;
end;


procedure TGlobalsGUI.Cleanup;
begin
  if (m_fb <> Nil) then  // filebrowser must go first!
    m_fb.Destroy;
  if (m_favs <> Nil) then
    m_favs.Destroy;
  if (m_kcache <> Nil) then
    m_kcache.Destroy;
  if (m_kcheck <> Nil) then
    m_kcheck.Destroy;
  if (m_wdiag <> Nil) then
    m_wdiag.Destroy;
  if (m_bview <> Nil) then
    m_bview.Destroy;
  if (m_bwowi <> Nil) then
    m_bwowi.Destroy;

  inherited;
end;


constructor TGlobalsGUI.Create(
        sCfgFilePath : String;
        browserView : TListView;
        passwInput : TPasswordInput;
        bcb : TCallBack;
        kccb : TKeyCacheCallBack;
        mcb : TMessageCallBack;
        cb : TCallBack);
begin

  // reset all additional fields
  m_fb:=Nil;
  m_favs:=Nil;
  m_kcache:=Nil;
  m_kcheck:=Nil;
  m_wdiag:=Nil;
  m_bview:=Nil;
  m_bwowi:=Nil;

  // the parent does most of the work for us (as usual)
  inherited Create(sCfgFilePath, passwInput, cb, mcb);

  try
    // set up the key cache
    m_kcache:=TKeyCache.Create(m_rnd,
                               kccb);

    // take care about a BFA viewer
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
    m_cfg.SetCensor(TGlobalsGUICensor.Create(Self));

  except
    on e: Exception do begin
      // (this should never happen)
      raise EGlobalsError.Create(e.Message);
    end;  
  end;
end;



destructor TGlobalsGUI.Destroy;
begin
  // destroy all other global instances
  CleanUp;
end;


procedure TGlobalsGUI.ShutDown(cb : TCallBack);
begin
  // prepare the callback
  cb.SetSimpleMessageState(True);

  // let the file browser store its settings
  m_fb.FixSettings;

  // save the configuration
  cb.SetMessage(m_sr.Get('GLOBALSGUI', '000'));
  cb.CallBack;

  try
    m_cfg.SaveToFile(
      m_sCfgFilePath,
      GetOpts.GetCfg.GetBooleanOption(OPTIONS_CFGID_SHOWCFGFILEERRORS));
  except
    // warn if we cannot save the configuration
    on EConfigurationError do
      Application.MessageBox(
        PChar(Format(m_sr.Get('GLOBALSGUI', '001'), [m_sCfgFilePath])),
        PChar(PROGRAM_NAME + m_sr.Get('GLOBALSGUI', '002')),
        MB_ICONEXCLAMATION);
  end;
end;

function TGlobalsGUI.GetFileBrowser : TFileBrowser;
begin
  Result:=m_fb;
end;

function TGlobalsGUI.GetFavorites : TFavorites;
begin
  Result:=m_favs;
end;

function TGlobalsGUI.GetKeyCache : TKeyCache;
begin
  Result:=m_kcache;
end;

function TGlobalsGUI.GetKeyChecker : TKeyChecker;
begin
  Result:=m_kcheck;
end;

function TGlobalsGUI.GetWinDiag : TWin32Diagnosis;
begin
  Result:=m_wdiag;
end;

function TGlobalsGUI.GetBFAViewer : TBFAViewer;
begin
  Result:=m_bview;
end;

function TGlobalsGUI.GetBFAWorkWith : TBFAWorkWith;
begin
  Result:=m_bwowi;
end;

initialization

end.
