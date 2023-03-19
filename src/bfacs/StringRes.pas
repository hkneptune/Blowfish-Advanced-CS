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
  string resource loader
}


unit StringRes;

{$I config.inc}

interface
uses classes, SysUtils;


// string resource creation error
type
  ETStrResLoadError = class(Exception);


// country configuration storage
type
  TCountryConfig = class
  private
    // members
    m_sCountry     : String;
    m_sHelpFile    : String;

  public
    // constructor
    // -> country description
    // -> name of the help file
    constructor Create(sCountry : String;
                       sHelpFile : String);

    // gets the country description
    function GetCountry : String;

    // gets the name of the help file
    function GetHelpFile : String;

    // gets the name of the help content
    //function GetHelpContent : String;
  end;



// a listener base class, this allows all modules to register themselves
// and being called automatically when the strings have to been exchanged,
// every consumer must inherit this class and implement changeStrings()
type
  TStrRes = class;  // (forward)
  TStrResListener = class
  public

    // the method which is called backed
    // -> the string resource from which to load the strings
    procedure ChangeStrings(sr : TStrRes); virtual; abstract;
  end;


  // the string resources storage (don't prefix a "type", otherwise
  // the fowarded class reference from above will not work anymore)
  TStrRes = class
  protected

    // section names
    m_sectNames : TStringList;

    // the idents (list of TStringList)
    m_idents : TList;

    // the contents (list of TStringList)
    m_contents : TList;

    // the country configuration
    m_cntCfg : TCountryConfig;

    // the listeners
    m_listeners : TList;

    // to check if valid resources are loaded
    m_blValid : Boolean;

    // internal method to clear all lists (except of the listeners, of course)
    procedure Clear;

  public

    // constructor
    // -> the string resource file
    // exception: ETStrResLoadError no valid ressources found
    constructor Create(sResFile : String);

    // destructor
    destructor Destroy; override;

    // checks if resources were loaded successfully
    // <- true: resources ok / false: no resources at all
    function IsValid : Boolean;

    // adds another string resource, but only the one passed in
    // the constructor is used to get the country configuration
    // <- True: success / False: resource loading error
    function AddResource(sResFile : String) : Boolean;

    // adds a listener to the list (will be destroyed when the object ends)
    // -> the listener
    procedure AddListener(newListener : TStrResListener);

    // changes the string resources (country configuration will be reinit.)
    // -> the resource file with the new strings
    // <- True: success / False: resource loading error
    function NewStringResource(sNewResFile : String) : Boolean;

    // calls all listeners to refresh their strings
    procedure InformListeners;

    // gets a string from the loaded resource
    // -> resource section
    // -> string identifier
    // <- the string (or "sSection:sIdent" if String was not found)
    function Get(sSection : String;
                 sIdent : String) : String;

    // gets the _reference_ of the country configuration
    // <- the country configuration (may contain no information
    //    and is only valid after at least one resource file has been
    //    loaded successfully)
    function GetCountryConfig : TCountryConfig;
  end;






implementation
uses Configuration, StringPlus, General;



//////////////////////////////// TCountryConfig ////////////////////////////////



constructor TCountryConfig.Create(sCountry : String;
                                  sHelpFile : String);
begin
  m_sCountry:=sCountry;
  m_sHelpFile:=sHelpFile;
end;

function TCountryConfig.GetCountry : String;
begin
  Result:=m_sCountry;
end;

function TCountryConfig.GetHelpFile : String;
begin
  Result:=m_sHelpFile;
end;


//////////////////////////////// TStrRes ////////////////////////////////



// names in the special section for the country configuration
const
  CC_SECTION        = 'COUNTRY_CONFIG';
  CC_IDENT_COUNTRY  = 'Language';
  CC_IDENT_HELPFILE = 'HelpFile';


constructor TStrRes.Create(sResFile : String);
begin
  // make the lists
  m_sectNames:=TStringList.Create;
  m_idents:=TList.Create;
  m_contents:=TList.Create;
  m_listeners:=TList.Create;

  // no country information yet
  m_cntCfg:=Nil;

  // load the basic resources
  if (not NewStringResource(sResFile)) then
    raise ETStrResLoadError.Create('error in TStrRes.Create');
end;



destructor TStrRes.Destroy;
var
  nI : Integer;
begin
  Clear;
  // free all listeners
  for nI:=0 to (m_listeners.Count - 1) do
     TStrResListener(m_listeners.Items[nI]).Destroy;
  m_listeners.Destroy;
  m_contents.Destroy;
  m_idents.Destroy;
  m_sectNames.Destroy;
end;


function TStrRes.IsValid : Boolean;
begin
  Result:=m_blValid;
end;


procedure TStrRes.Clear;
var
  nI : Integer;
begin
  // clear all lists and sublists
  for nI:=0 to (m_idents.Count - 1) do begin
    TStringList(m_idents.Items[nI]).Destroy;
    TStringList(m_contents.Items[nI]).Destroy;
  end;
  m_idents.Clear;
  m_contents.Clear;
  m_sectNames.Clear;

  // no country information anymore
  if (m_cntCfg <> Nil) then begin
    m_cntCfg.Destroy;
    m_cntCfg:=Nil;
  end;

  // no resources now
  m_blValid:=False;
end;



function TStrRes.AddResource(sResFile : String) : Boolean;
var
  nI      : Integer;
  nJ      : Integer;
  sTemp   : String;
  loader  : TConfiguration;
  ids     : TStringList;
  cts     : TStringList;
  extSect : TConfigurationSection;
  extIds  : TStringList;
  extCts  : TStringList;
begin
  // "misuse" the configuration class for loading the strings
  loader:=TConfiguration.Create;
  try
    loader.LoadFromFile(sResFile);
  except
    on EConfigurationError do begin
      // no strings - no fun, I tell ya!
      loader.Destroy;
      Result:=False;
      Exit;
    end;
  end;

  // now _copy_ all the strings to our lists
  m_sectNames.BeginUpdate;
  for nI:=0 to (loader.GetNumOfSections - 1) do begin
     // new section (and sublists)
     extSect:=loader.GetSectionByIndex(nI);
     m_sectNames.Add(UpperCase(extSect.getID));
     extIds:=extSect.getIdents;
     extCts:=extSect.getContents;
     ids:=TStringList.Create;
     cts:=TStringList.Create;
     ids.BeginUpdate;
     cts.BeginUpdate;
     for nJ:=0 to (extIds.Count - 1) do begin
       ids.Add(extIds.Strings[nJ]);
       // filter out the (space) replacements, but don't accept binary stuff
       if (TStrPlus.GetBinStr(extCts.Strings[nJ], sTemp) <> -1) then
         cts.Add(TStrPlus.FilterIllegalChars(sTemp, True))
       else
         cts.Add(extCts.Strings[nJ]);
     end;
     cts.EndUpdate;
     ids.EndUpdate;
     m_idents.Add(ids);
     m_contents.Add(cts);
  end;
  m_sectNames.EndUpdate;

  // free all from the loader
  loader.Destroy;

  // now we have resources loaded
  m_blValid:=True;

  // success
  Result:=True;
end;



procedure TStrRes.AddListener(newListener : TStrResListener);
begin
  // just add the reference to the list
  m_listeners.Add(newListener);
end;



function TStrRes.NewStringResource(sNewResFile : String) : Boolean;
begin
  // clear all lists and reinit.
  Clear;
  Result:=AddResource(sNewResFile);

  // create the (new) country configuration, even when there
  // are no or incomplete resources for it
  m_cntCfg:=TCountryConfig.Create(Get(CC_SECTION, CC_IDENT_COUNTRY),
                                  Get(CC_SECTION, CC_IDENT_HELPFILE));
end;


procedure TStrRes.InformListeners;
var
  nI : Integer;
begin
  // call all listeners to let them reload their strings
  for nI:=0 to (m_listeners.Count - 1) do
    TStrResListener(m_listeners.Items[nI]).ChangeStrings(Self);
end;




function TStrRes.Get(sSection : String;
                     sIdent : String) : String;
var
  nI     : Integer;
  nJ     : Integer;
  nCount : Integer;
  idents : TStringList;
begin
  //search for the section
  sSection:=UpperCase(sSection);
  nCount:=m_sectNames.Count;
  for nI:=0 to (nCount - 1) do begin
    if (m_sectNames.Strings[nI] = sSection) then begin

      // search for the ident
      sIdent:=UpperCase(sIdent);
      idents:=TStringList(m_idents.Items[nI]);
      nCount:=idents.Count;  // (we can reuse nCount here flawlessly)
      for nJ:=0 to (nCount - 1) do begin
        if (idents.Strings[nJ] = sIdent) then begin
          Result:=TStringList(m_contents[nI]).Strings[nJ];
          Exit;
        end;
      end;

      // ident not found
      Result:=sSection + ':' + sIdent;
    end;
  end;

  // section not found found
  Result:=sSection + ':' + sIdent;
end;


function TStrRes.GetCountryConfig : TCountryConfig;
begin
  Result:=m_cntCfg;
end;



end.
