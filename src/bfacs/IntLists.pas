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
  dynamic integer lists (close to open arrays, only 100% OOP and faster),
  remember that due to speed reasons there's no range checking!
}

unit IntLists;

{$I config.inc}

interface
uses bfacslib;


// base class, pure abstract
type
  TIntList = class
  protected
    // preload size
    m_nPreLoadSize : Integer;

    // number of entries
    m_nSize : Integer;

    // number of free entries
    m_nFreeEntries : Integer;

  public
    // default constructor
    constructor Create; overload; virtual;

    // constructor to set an own preload size
    // -> the preload size
    constructor Create(nPreLoadSize : Integer); overload; virtual;

    // gets the number of entries
    // <- number of entries
    function GetSize : Integer; virtual;

    // clears the list
    procedure Clear; virtual; abstract;
  end;


// integer list
type
  TIntegerList = class(TIntList)
  private
    // our value list
    m_entries : array of Integer;

  public
    // default constructor
    constructor Create; override;

    // constructor to set an own preload size
    // -> the preload size
    constructor Create(nPreLoadSize : Integer); override;

    // destructor
    destructor Destroy; override;

    // adds a value
    // -> new value
    // <- the index of the added value
    function Add(nVal : Integer) : Integer;

    // gets a value
    // -> index of the value
    // <- the value
    function Get(nIndex : Integer) : Integer;

    // changes a value
    // -> index of the old value
    // -> new value
    procedure Change(nIndex : Integer;
                     nVal : Integer);

    // clears the list
    procedure Clear; override;
  end;


// 64 bit (unsigned) integer list
type
  TWORD64List = class(TIntList)
  private
    // our value list
    m_entries : array of WORD64;

  public
    // default constructor
    constructor Create; override;

    // constructor to set an own preload size
    // -> the preload size
    constructor Create(nPreLoadSize : Integer); override;

    // destructor
    destructor Destroy; override;

    // adds a value
    // -> new value
    // <- the index of the added value
    function Add(qVal : WORD64) : Integer;

    // gets a value
    // -> index of the value
    // <- the value
    function Get(nIndex : Integer) : WORD64;

    // changes a value
    // -> index of the old value
    // -> new value
    procedure Change(nIndex : Integer;
                     qVal : WORD64);

    // clears the list
    procedure Clear; override;
  end;



// byte boolean list
type
  TBYTEBOOLList = class(TIntList)
  private
    // our value list
    m_entries : array of BYTEBOOL;

  public
    // default constructor
    constructor Create; override;

    // constructor to set an own preload size
    // -> the preload size
    constructor Create(nPreLoadSize : Integer); override;

    // destructor
    destructor Destroy; override;

    // adds a value
    // -> new value
    // <- the index of the added value
    function Add(blVal : BYTEBOOL) : Integer;

    // gets a value
    // -> index of the value
    // <- the value
    function Get(nIndex : Integer) : BYTEBOOL;

    // changes a value
    // -> index of the old value
    // -> new value
    procedure Change(nIndex : Integer;
                     blVal : BYTEBOOL);

    // clears the list
    procedure Clear; override;
  end;




implementation

// the generel default preload size for handling dynamic array faster
const
  DEF_DYNARRAY_PRELOAD = 128;


//////////////////////////////// TIntList ////////////////////////////////


constructor TIntList.Create;
begin
  m_nPreLoadSize:=DEF_DYNARRAY_PRELOAD;
  m_nSize:=0;
  m_nFreeEntries:=m_nPreLoadSize;
end;


constructor TIntList.Create(nPreLoadSize : Integer);
begin
  if (nPreLoadSize > 0) then
    m_nPreLoadSize:=nPreLoadSize
  else
    m_nPreLoadSize:=DEF_DYNARRAY_PRELOAD;
  m_nSize:=0;
  m_nFreeEntries:=m_nPreLoadSize;
end;


function TIntList.GetSize : Integer;
begin
  Result:=m_nSize;
end;


//////////////////////////////// TIntegerList ////////////////////////////////


constructor TIntegerList.Create;
begin
  inherited Create;
  SetLength(m_entries, m_nPreLoadSize);
end;


constructor TIntegerList.Create(nPreLoadSize : Integer);
begin
  inherited Create(nPreLoadSize);
  SetLength(m_entries, m_nPreLoadSize);
end;


destructor TIntegerList.Destroy;
begin
  Finalize(m_entries);
end;


function TIntegerList.Add(nVal : Integer) : Integer;
begin
  // reallocate?
  if (m_nFreeEntries = 0) then begin
    if (m_nPreLoadSize > 0) then
      m_nFreeEntries:=m_nPreloadSize
    else
      m_nFreeEntries:=1;
    SetLength(m_entries, Length(m_entries) + m_nFreeEntries);
  end;

  // add the value
  m_entries[m_nSize]:=nVal;
  Dec(m_nFreeEntries);
  Result:=m_nSize;
  Inc(m_nSize);
end;


function TIntegerList.Get(nIndex : Integer) : Integer;
begin
  Result:=m_entries[nIndex];
end;


procedure TIntegerList.Change(nIndex : Integer;
                              nVal : Integer);
begin
  m_entries[nIndex]:=nVal;
end;


procedure TIntegerList.Clear;
var
  nI : Integer;
begin
  for nI:=0 to (Length(m_entries) - 1) do
    m_entries[nI]:=0;
  SetLength(m_entries, m_nPreLoadSize);
  m_nSize:=0;
  m_nFreeEntries:=m_nPreLoadSize;
end;


//////////////////////////////// TWORD64List ////////////////////////////////


constructor TWORD64List.Create;
begin
  inherited Create;
  SetLength(m_entries, m_nPreLoadSize);
end;


constructor TWORD64List.Create(nPreLoadSize : Integer);
begin
  inherited Create(nPreLoadSize);
  SetLength(m_entries, m_nPreLoadSize);
end;


destructor TWORD64List.Destroy;
begin
  Finalize(m_entries);
end;


function TWORD64List.Add(qVal : WORD64) : Integer;
begin
  // reallocate?
  if (m_nFreeEntries = 0) then begin
    if (m_nPreLoadSize > 0) then
      m_nFreeEntries:=m_nPreloadSize
    else
      m_nFreeEntries:=1;
    SetLength(m_entries, Length(m_entries) + m_nFreeEntries);
  end;

  // add the value
  m_entries[m_nSize]:=qVal;
  Dec(m_nFreeEntries);
  Result:=m_nSize;
  Inc(m_nSize);
end;


function TWORD64List.Get(nIndex : Integer) : WORD64;
begin
  Result:=m_entries[nIndex];
end;


procedure TWORD64List.Change(nIndex : Integer;
                             qVal : WORD64);
begin
  m_entries[nIndex]:=qVal;
end;


procedure TWORD64List.Clear;
var
  nI : Integer;
begin
  for nI:=0 to (Length(m_entries) - 1) do
    m_entries[nI]:=WORD64(0);
  SetLength(m_entries, m_nPreLoadSize);
  m_nSize:=0;
  m_nFreeEntries:=m_nPreLoadSize;
end;


//////////////////////////////// TBYTEBOOLList ////////////////////////////////


constructor TBYTEBOOLList.Create;
begin
  inherited Create;
  SetLength(m_entries, m_nPreLoadSize);
end;


constructor TBYTEBOOLList.Create(nPreLoadSize : Integer);
begin
  inherited Create(nPreLoadSize);
  SetLength(m_entries, m_nPreLoadSize);
end;


destructor TBYTEBOOLList.Destroy;
begin
  Finalize(m_entries);
end;


function TBYTEBOOLList.Add(blVal : BYTEBOOL) : Integer;
begin
  // reallocate?
  if (m_nFreeEntries = 0) then begin
    if (m_nPreLoadSize > 0) then
      m_nFreeEntries:=m_nPreloadSize
    else
      m_nFreeEntries:=1;
    SetLength(m_entries, Length(m_entries) + m_nFreeEntries);
  end;

  // add the value
  m_entries[m_nSize]:=blVal;
  Dec(m_nFreeEntries);
  Result:=m_nSize;
  Inc(m_nSize);
end;


function TBYTEBOOLList.Get(nIndex : Integer) : BYTEBOOL;
begin
  Result:=m_entries[nIndex];
end;


procedure TBYTEBOOLList.Change(nIndex : Integer;
                               blVal : BYTEBOOL);
begin
  m_entries[nIndex]:=blVal;
end;


procedure TBYTEBOOLList.Clear;
var
  nI : Integer;
begin
  for nI:=0 to (Length(m_entries) - 1) do
    m_entries[nI]:=BYTEBOOL(0);
  SetLength(m_entries, m_nPreLoadSize);
  m_nSize:=0;
  m_nFreeEntries:=m_nPreLoadSize;
end;



end.
