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
  to store a key hashed for automatic confirmation purposes
}

unit KeyChecker;

interface
uses
  Classes,
  Configuration,
  RandomManager,
  SecureMem;

type
  TKeyChecker = class
  private
    // members
    m_cfg  : TConfigurationSection;
    m_rm   : TRandomManager;
    m_list : TList;

    // writes the list back to the configuration
    procedure StoreList;

  public
    // constructor
    // -> configuration where to load and store the hashes
    // -> random manager (to get random data for salt creation)
    constructor Create(cfg : TConfiguration;
                       rm : TRandomManager);

    // destructor
    destructor Destroy; override;

    // to check if a key (usually a password) was already stored
    // -> the key to look for
    // <- True: key is in the list / False: key seems to be unknown
    function CheckKey(key : TKeyMemory) : Boolean;

    // adds a key to the list (configuration will be updated afterwards)
    // -> the key to add
    procedure AddKey(key : TKeyMemory);

    // clears the whole list
    procedure ClearList;

    // to determine if list is empty
    // <- True: no key hashes stored / False: something is out there
    function IsEmpty : Boolean;

    // gets the number of hashes
    // <- number of hashes
    function NumberOfHashes : Integer;

  end;



implementation
uses
  SysUtils,
  StringPlus,
  bfacslib,
  MD5;


//////////////////////////// TKeyChecker ////////////////////////////


// the configuration ID
const
  CONFIG_ID = 'KeyChecker';

// size of a hash (2 times xor-folded MD5 digest, 32 bits large)
const
  HASH_SIZE = SizeOf(WORD32);

// size of the salt
const
  SALT_SIZE = 23;

// config. member ID for the number of hashes
const
  CFGID_COUNT = 'COUNT';


// internal configuration checker
type
  TKeyCheckerCC = class(TConfigurationChecker)
  public
    procedure RunCheck(section : TConfigurationSection); override;
  end;


procedure TKeyCheckerCC.RunCheck(section : TConfigurationSection);
var
  nI        : Integer;
  nCount    : Integer;
  nUpToThis : Integer;
  sID       : String;
  sItem     : String;
  checkList : TStringList;
begin
  // get the number of salt+hash combinations
  CheckInt(section, CFGID_COUNT, 0);

  // check all hashes for their existence
  nCount:=section.GetIntegerOption(CFGID_COUNT);
  checkList:=TStringList.Create;
  checkList.BeginUpdate;
  nI:=0;
  nUpToThis:=nCount;
  while (nI < nUpToThis) do begin
    sID:=IntToStr(nI + 1);
    if (not section.OptionExists(sID)) then
      Dec(nCount)
    else begin
      // check also for a correct format (before adding it)
      sItem:=section.GetStringOption(sID);
      if (TStrPlus.IsBinHexStr(sItem,
                               HASH_SIZE + SALT_SIZE)) then
        checkList.Add(section.GetStringOption(sID));

      section.RemoveOption(sID);
    end;
    Inc(nI);
  end;
  checkList.EndUpdate;

  // write a new list
  section.FixIntegerOption(CFGID_COUNT, nCount);
  for nI:=1 to nCount do
    section.FixStringOption(IntToStr(nI),
                            checkList.Strings[nI - 1],
                            True);
  checkList.Destroy;
end;



procedure TKeyChecker.StoreList;
var
  nI     : Integer;
  nCount : Integer;
begin
  // first clear the configuration
  nCount:=m_cfg.GetIntegerOption(CFGID_COUNT);
  for nI:=1 to nCount do
    m_cfg.RemoveOption(IntToStr(nI));

  // now write the new list
  nCount:=m_list.Count;
  m_cfg.FixIntegerOption(CFGID_COUNT, nCount);
  for nI:=1 to nCount do
    m_cfg.FixStringOption(IntToStr(nI),
                          TStrPlus.BytesToHexStr(m_list.Items[nI - 1],
                                                 SALT_SIZE + HASH_SIZE),
                          True);
end;


constructor TKeyChecker.Create(cfg : TConfiguration;
                               rm : TRandomManager);
var
  nI     : Integer;
  nCount : Integer;
  pEntry : Pointer;
  cc     : TKeyCheckerCC;
begin
  // get the section
  cc:=TKeyCheckerCC.Create;
  m_cfg:=cfg.GetSection(CONFIG_ID, cc);
  cc.Destroy;

  // store the random manager instance
  m_rm:=rm;

  // read out all entries (the configuration checker guarantees valid entries)
  nCount:=m_cfg.GetIntegerOption(CFGID_COUNT);
  m_list:=TList.Create;
  for nI:=1 to nCount do begin
    GetMem(pEntry, SALT_SIZE + HASH_SIZE);
    TStrPlus.HexStrToBytes(m_cfg.GetStringOption(IntToStr(nI)),
                           pEntry);
    m_list.Add(pEntry);
  end;
end;


destructor TKeyChecker.Destroy;
begin
  // release the list
  ClearList;
  m_list.Destroy;
end;


function TKeyChecker.CheckKey(key : TKeyMemory) : Boolean;
var
  nI        : Integer;
  nCount    : Integer;
  hasher    : TMD5;
  hash      : TMD5Digest;
  pActItem  : PWORD8Buf;
begin
  // search for a fitting entry
  hasher:=TMD5.Create;
  hash:=TMD5Digest.Create;
  nCount:=m_list.Count;
  for nI:=0 to (nCount - 1) do begin

    // calculate the 32bit hash from the given key
    pActItem:=m_list.items[nI];
    hasher.Reset;
    hasher.Update(pActItem,
                  SALT_SIZE);
    hasher.Update(key.GetPtr,
                  key.GetSize);
    hasher.Finalize(hash);

    // equal?
    if (hash.GetFolded32 = PWORD32(@pActItem[SALT_SIZE])^) then begin
      // yes, return with success signal (but first clean up)
      hash.Destroy;
      hasher.Destroy;
      Result:=True;
      Exit;
    end;

  end;
  hash.Destroy;
  hasher.Destroy;

  // nothing found
  Result:=False;
end;


procedure TKeyChecker.AddKey(key : TKeyMemory);
var
  lHash32  : WORD32;
  hasher   : TMD5;
  hash     : TMD5Digest;
  pNewItem : PWORD8Buf;
begin
  // create some salt
  GetMem(Pointer(pNewItem),
         SALT_SIZE + HASH_SIZE);
  m_rm.GetRandomSource.GetBytes(pNewItem, SALT_SIZE);

  // hash down salt and key to 32 bits
  hasher:=TMD5.Create;
  hasher.Update(pNewItem,
                SALT_SIZE);
  hasher.Update(key.GetPtr,
                key.GetSize);
  hash:=TMD5Digest.Create;
  hasher.Finalize(hash);
  hasher.Destroy;
  lHash32:=hash.GetFolded32;
  hash.Destroy;
  // (we store the hash in little endian byte order)
  Move(lHash32,
       pNewItem^[SALT_SIZE],
       HASH_SIZE);

  // store the new entry
  m_list.Add(pNewItem);

  // (FIXME: should we trust the integrity outside and just append the item?)
  StoreList;
end;


procedure TKeyChecker.ClearList;
var
  nI     : Integer;
  nCount : Integer;
begin
  nCount:=m_list.Count;
  nI:=0;
  while (nI < nCount) do begin
    FreeMem(m_list.Items[nI]);
    Inc(nI);
  end;
  m_list.Clear;
  StoreList;
end;


function TKeyChecker.IsEmpty : Boolean;
begin
  Result:=(NumberOfHashes = 0);
end;

function TKeyChecker.NumberOfHashes : Integer;
begin
  Result:=m_list.Count;
end;



end.
