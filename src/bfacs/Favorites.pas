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
  keeper for favorite paths
}

unit Favorites;

interface
uses Classes,
     Configuration;

//default configuration ID
const
  FAVORITES_DEF_CONFIGID = 'FAVORITES';


// the favorites keeper
type
  TFavorites = class
  private
    // members (what else?)
    m_config          : TConfigurationSection;
    m_blCaseSensitive : Boolean;
    m_favsCache       : TStringList;
    m_sCfgID          : String;

  public
    // constructor
    // -> the configuration where to load & store
    // -> configuration ID
    // -> sort the favorites
    // -> True: treat them case sensitive / False: do not
    constructor Create(cfg : TConfiguration;
                       sCfgID : String = FAVORITES_DEF_CONFIGID;
                       blSorted : Boolean = True;
                       blCaseSensitive : Boolean = False); virtual;

    // destructor
    destructor Destroy; override;

    // gets the number of favorites
    // <- number of favorites
    function GetCount : Integer;

    // adds a favorite
    // -> the favorite to add
    // -> True: store to the configuration / False: do not
    // <- True: added / False: already in the list
    function Add(sToAdd : String;
                 blStore : Boolean = True) : Boolean; virtual;

    // checks if a string is stored in the favorites
    // -> string to search for
    // <- index of teh string in the list (-1 equals not in the list)
    function GetIndexOf(const sSearchFor : String) : Integer;

    // removes a favorite
    // -> the favorite to remove
    // -> True: store to the configuration / False: do not
    // <- True: removed / False: not in the list
    function Remove(sToRemove : String;
                    blStore : Boolean = True) : Boolean; overload; virtual;

    // removes a favorite by its index (does only make sense if the list is
    // not sorted in any way, of course)
    // -> index of the favorite to remove
    // -> True: store to the configuration / False: do not
    // <- True: removed / False: wrong index
    function Remove(nIndex : Integer;
                    blStore : Boolean = True) : Boolean; overload; virtual;

    // gets the _reference_ to the list with the favorites
    // <- (ref. to) the fav. list
    function GetList : TStringList; virtual;

    // gets the associated configuration section (ref.)
    // <- config. section
    function GetConfig : TConfigurationSection; virtual;

    // to store the favorites to the configuration
    procedure StoreToConfig; virtual;

    // delete all favorites
    procedure Clear; virtual;

  end;


implementation
uses SysUtils;


//////////////////////// TFavorites ////////////////////////


// other cfg. IDs
const
  COUNT_ID  = 'COUNT';


// configuration checker
type
  TFavoritesCC = class(TConfigurationChecker)
  public
    procedure RunCheck(section : TConfigurationSection); override;
  end;


procedure TFavoritesCC.RunCheck(section : TConfigurationSection);
var
  nI        : Integer;
  nCount    : Integer;
  nUpToThis : Integer;
  sID       : String;
  checkList : TStringList;

begin
  // get the number of favorites
  CheckInt(section, COUNT_ID, 0);

  // check all favorites for their existence
  nCount:=section.GetIntegerOption(COUNT_ID);
  checkList:=TStringList.Create;
  nI:=0;
  nUpToThis:=nCount;
  while (nI < nUpToThis) do begin
    sID:=IntToStr(nI);
    if (not section.OptionExists(sID)) then
      Dec(nCount)
    else begin
      checkList.Add(section.GetStringOption(sID));
      section.RemoveOption(sID);
    end;
    Inc(nI);
  end;

  // write a new list
  section.FixIntegerOption(COUNT_ID, nCount);
  for nI:=0 to (nCount - 1) do
    section.FixStringOption(IntToStr(nI),
                            checkList.Strings[nI],
                            True);
  checkList.Destroy;
end;



constructor TFavorites.Create(cfg : TConfiguration;
                              sCfgID : String = FAVORITES_DEF_CONFIGID;
                              blSorted : Boolean = True;
                              blCaseSensitive : Boolean = False);
var
  nI     : Integer;
  nCount : Integer;
  cc     : TFavoritesCC;
begin
  // get the configuration ID first
  m_sCfgID:=sCfgID;

  // get the configuration section containing the favorites
  cc:=TFavoritesCC.Create;
  m_config:=cfg.GetSection(m_sCfgID, cc);
  cc.Destroy;

  // init. the caching list
  m_favsCache:=TStringList.Create;
  m_favsCache.Sorted:=blSorted;
  nCount:=m_config.GetIntegerOption(COUNT_ID);
  for nI:=0 to (nCount - 1) do
    Add(m_config.GetStringOption(IntToStr(nI)), False);

  // now store the whole stuff back, without any doublettes
  StoreToConfig;

  // get the rest
  m_blCaseSensitive:=blCaseSensitive;
end;


destructor TFavorites.Destroy;
begin
  m_favsCache.Destroy;
end;

function TFavorites.GetCount : Integer;
begin
  Result:=m_favsCache.Count;
end;


function TFavorites.Add(sToAdd : String;
                        blStore : Boolean = True) : Boolean;
var
  nI     : Integer;
  nCount : Integer;
begin
  // search for doublettes
  Result:=False;
  nCount:=GetCount;
  if (m_blCaseSensitive) then
    for nI:=0 to (nCount - 1) do begin
      if (m_favsCache.Strings[nI] = sToAdd) then
        Exit
    end
  else
    for nI:=0 to (nCount - 1) do
      if (AnsiUpperCase(m_favsCache.Strings[nI]) = AnsiUpperCase(sToAdd)) then
        Exit;

  // no doublette, so add it
  m_favsCache.Add(sToAdd);

  // store, i. n.
  if (blStore) then
    StoreToConfig;

  // successfully added
  Result:=True;
end;


function TFavorites.GetIndexOf(const sSearchFor : String) : Integer;
begin
  // search for the entry
  Result:=0;
  while (Result < GetCount) do begin
    if (m_blCaseSensitive) then begin
      if (m_favsCache.Strings[Result] = sSearchFor) then
        Exit;
    end
    else
      if (AnsiUpperCase(m_favsCache.Strings[Result]) =
          AnsiUpperCase(sSearchFor)) then
        Exit;
    Inc(Result);
  end;
  Result:=-1;
end;


function TFavorites.Remove(sToRemove : String;
                           blStore : Boolean = True) : Boolean;
var
  nIdx : Integer;
begin
  // search for the entry
  nIdx:=GetIndexOf(sToRemove);

  // nothing found?
  Result:=(nIdx <> -1);
  if (Result) then
    // but yes, so just map the call
    Remove(nIdx, blStore);
end;


function TFavorites.Remove(nIndex : Integer;
                           blStore : Boolean = True) : Boolean;
begin

  // check the index before removing the entry
  if ((nIndex < 0) or (nIndex >= GetCount)) then
    Result:=False
  else begin
    Result:=True;
    m_favsCache.Delete(nIndex);
    if (blStore) then
      StoreToConfig;
  end;
end;


function TFavorites.GetList : TStringList;
begin
  Result:=m_favsCache;
end;

function TFavorites.GetConfig : TConfigurationSection;
begin
  Result:=m_config;
end;

procedure TFavorites.StoreToConfig;
var
  nI     : Integer;
  nCount : Integer;
begin
  with m_config do begin

    // remove all old entries
    nCount:=GetIntegerOption(COUNT_ID);
    for nI:=0 to (nCount - 1) do
      RemoveOption(IntToStr(nI));

    // write the new list
    nCount:=m_favsCache.Count;
    FixIntegerOption(COUNT_ID, nCount);
    for nI:=0 to (nCount - 1) do
      FixStringOption(IntToStr(nI),
                      m_favsCache.Strings[nI],
                      True);
  end;
end;


procedure TFavorites.Clear;
begin
  m_favsCache.Clear;
  StoreToConfig;
end;


end.
