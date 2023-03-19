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
  history implementation, mainly usable for paths and URLs
}

unit History;

interface
uses Classes, SysUtils;


// default history capacity
const
  HISTORY_DEF_CAPACITY = 128;


// add codes (bitmask)
const
  HISTORY_ADD_ALWAYS      = 0;
  HISTORY_ADD_NODOUBLE    = 1;
  HISTORY_ADD_CASESENS    = 128;


// the history keeper
type
  THistory = class
  private
    // members
    m_locations : array of String;
    m_nCapacity : Integer;
    m_nMaxPos   : Integer;
    m_nActPos   : Integer;

  public

    // constructor
    // -> capacity of the history
    constructor Create(nCapacity : Integer = HISTORY_DEF_CAPACITY);

    // destructor
    destructor Destroy; override;

    // adds a new location to the history
    // -> new location
    // -> add mode (see HISTORY_ADD_xxx)
    // <- True: location added / False: not added (already in list)
    function Add(const sLocation : String;
                 nMode : Integer) : Boolean;

    // goes forwards in the history
    // <- the next location in the history (empty if on the top)
    function GoForwards : String;

    // goes backwards in the history
    // <- the previous location in the history (empty if on the bottom)
    function GoBackwards : String;

    // checks if we can go forwards in the history
    // <- True: possible / False: no more locations to joing
    function CanGoForwards : Boolean;

    // checks if we can go backwards in the history
    // <- True: possible / False: no more locations to joing
    function CanGoBackwards : Boolean;

    // get all locations before the current one
    // -> max. number of locations to deliver
    // <- the location list (may be empty), destroyed by the caller
    function GetLocationsBefore(nMaxLocations : Integer) : TStringList;

    // get all locations after the current one
    // -> max. number of locations to deliver
    // <- the location list (may be empty), destroyed by the caller
    function GetLocationsAfter(nMaxLocations : Integer) : TStringList;

    // gets the current location
    // <- current location
    function GetCurrentLocation : String;

    // clears the whole history
    procedure Clear;
  end;


implementation


// min. capacity size
const
  MIN_CAPACITY_SIZE = 16;


//////////////////////// THistory ////////////////////////

constructor THistory.Create(nCapacity : Integer = HISTORY_DEF_CAPACITY);
begin
  // don't accept invalid capacities
  if (nCapacity < MIN_CAPACITY_SIZE) then
    m_nCapacity:=MIN_CAPACITY_SIZE
  else
    m_nCapacity:=nCapacity;

  // make the locations storage
  SetLength(m_locations, m_nCapacity);

  // nothing set yet
  Clear;
end;


destructor THistory.Destroy;
begin
  // cleanup
  Finalize(m_locations);
end;


function THistory.Add(const sLocation : String;
                      nMode : Integer) : Boolean;

var
  nI         : Integer;
  blCaseSens : Boolean;
  sCurrLoc   : String;
begin

  Result:=False;

  if (m_nMaxPos <> -1) then begin

    blCaseSens:=((nMode and HISTORY_ADD_CASESENS) = HISTORY_ADD_CASESENS);

    // no doublettes?
    if ((nMode and HISTORY_ADD_NODOUBLE) = HISTORY_ADD_NODOUBLE) then begin

      sCurrLoc:=GetCurrentLocation;

      if (blCaseSens) then begin
        if (sCurrLoc = sLocation) then
          Exit;
      end
      else begin
        if (AnsiUpperCase(sCurrLoc) = AnsiUpperCase(sLocation)) then
          Exit;
      end;
    end;

    // storage full?
    if (m_nActPos = (m_nCapacity - 1)) then begin
      // yes, so shift all entries back (FIME: ringbuffer may be faster)
      for nI:=1 to m_nActPos do
        m_locations[nI - 1]:=m_locations[nI];
      Dec(m_nActPos);
    end;
  end;

  // add the new location
  Inc(m_nActPos);
  m_locations[m_nActPos]:=sLocation;

  // that's our strategy: remove the old branch
  m_nMaxPos:=m_nActPos;

  // (release unused memory)
  for nI:=(m_nMaxPos + 1) to (m_nCapacity - 1) do
    m_locations[nI]:='';

  // we added something
  Result:=True;
end;


function THistory.GoForwards : String;
begin
  if (not CanGoForwards) then
    Result:=''
  else begin
    Inc(m_nActPos);
    Result:=m_locations[m_nActPos];
  end;
end;


function THistory.GoBackwards : String;
begin
  if (not CanGoBackwards) then
    Result:=''
  else begin
    Dec(m_nActPos);
    Result:=m_locations[m_nActPos];
  end;
end;


function THistory.CanGoForwards : Boolean;
begin
  if (m_nActPos = -1) then begin
    Result:=False;
    Exit;
  end;
  Result:=(m_nActPos < m_nMaxPos);
end;


function THistory.CanGoBackwards : Boolean;
begin
  if (m_nActPos = -1) then begin
    Result:=False;
    Exit;
  end;
  Result:=(m_nActPos > 0);
end;


function THistory.GetLocationsBefore(nMaxLocations : Integer) : TStringList;
var
  nI     : Integer;
  nCount : Integer;
begin
  Result:=TStringList.Create;
  Result.BeginUpdate;;
  if (m_nActPos = -1) then
    Exit;

  nCount:=0;
  for nI:=(m_nActPos + 1) to m_nMaxPos do
    if (nCount >= nMaxLocations) then
      Break
    else begin
      Result.Add(m_locations[nI]);
      Inc(nCount);
    end;
    
  Result.EndUpdate;;
end;


function THistory.GetLocationsAfter(nMaxLocations : Integer) : TStringList;
var
  nI     : Integer;
  nCount : Integer;
begin
  Result:=TStringList.Create;
  Result.BeginUpdate;
  if (m_nActPos = -1) then
    Exit;

  nCount:=0;
  for nI:=(m_nActPos - 1) downto 0 do
    if (nCount >= nMaxLocations) then
      Break
    else begin
      Result.Add(m_locations[nI]);
      Inc(nCount);
    end;

  Result.EndUpdate;
end;


function THistory.GetCurrentLocation : String;
begin
  if (m_nActPos = -1) then
    Result:=''
  else
    Result:=m_locations[m_nActPos];
end;

procedure THistory.Clear;
var
  nI : Integer;
begin
  // reset the positions
  m_nMaxPos:=-1;
  m_nActPos:=-1;

  // release unused memory
  for nI:=0 to (m_nCapacity - 1) do
    m_locations[nI]:='';
end;



end.
