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
  support class implementing the MD5 digest algorithm through CryptPak.dll
}

unit MD5;

{$I config.inc}

interface
uses bfacslib, Digest;


// size of an MD5 digest
const
  MD5_DIGESTSIZE = bfacslib.MD5_DIGESTSIZE;


// MD5 digest class type
type
  TMD5Digest = class(TDigest)
  private
    // members
    m_digestdata : array[0..MD5_DIGESTSIZE - 1] of WORD8;

  public
    // creates an instance with a zero buffer
    constructor Create; override;

    // clears the digest data
    destructor Destroy; override;

    // just returns the digest size
    // <- digest size
    function GetDigestSize : Integer; override;

    // delivers digest data
    // -> where to copy the digest bytes
    procedure GetData(pTarget : Pointer); override;

    // absorbes digest data
    // -> from where to copy digest data
    procedure SetData(pSource : Pointer); override;

    // copies digest data from another instance
    // -> instance from where to copy digest data
    procedure CopyFrom(source : TDigest); override;

    // returns a hexadecimal representation of the digest data
    // <- digest hex string
    function GetHexStr : String; override;

    // converts a string with hex bytes bak to digest data
    // -> the string to convert
    // <- True: conversion was successful / False: error in string
    function SetHexStr(sSource : String) : Boolean; override;

    // compares this digest with another one
    // -> the diges to compare with
    // <- True: equal / False: not equal
    function Equals(compare : TDigest) : Boolean; override;

    // returns a pointer to the digest data buffer
    // <- 32bit hash
    function GetDigestDataPtr : Pointer; override;

    // returns the folded 32bit hash
    // <- 32bit hash
    function GetFolded32 : WORD32;
  end;



// class type for encapsulating the hash generation process
type
  TMD5 = class(TDigestProducer)
  private
    // here we store the MD5 context
    m_pCtx : PMD5CTX;

  public
    // constructor, prepares the context for hashing
    constructor Create; override;

    // destructor, just clears the context (if the caller forgets to finalize)
    destructor Destroy; override;

    // resets the context for a new hash session
    procedure Reset; override;

    // hashes an amount of data
    // -> pointer to the memory block to hash
    // -> number of bytes to hash
    procedure Update(pData : Pointer; lNumOfBytes : WORD32); override;

    // finalizes and copies a digest
    // -> keeper where to put the digest
    procedure Finalize(digest : TDigest); override;

    // runs a selftest
    // <- True: selftest succeeded / False: selftest failed
    function SelfTest : Boolean; override;
  end;



implementation
uses SysUtils,
     StringPlus;


//////////////////// TMD5Digest ////////////////////

constructor TMD5Digest.Create;
begin
  FillChar(m_digestdata, MD5_DIGESTSIZE, 0);
end;

destructor TMD5Digest.Destroy;
begin
  FillChar(m_digestdata, MD5_DIGESTSIZE, 0);
end;

function TMD5Digest.GetDigestSize : Integer;
begin
  GetDigestSize:=MD5_DIGESTSIZE;
end;

procedure TMD5Digest.GetData(pTarget : Pointer);
begin
  Move(m_digestdata, pTarget^, MD5_DIGESTSIZE);
end;

procedure TMD5Digest.SetData(pSource : Pointer);
begin
  Move(pSource^, m_digestdata, MD5_DIGESTSIZE);
end;

procedure TMD5Digest.CopyFrom(source : TDigest);
begin
  SetData(@TMD5Digest(source).m_digestdata);
end;

function TMD5Digest.GetHexStr : String;
begin
  Result:=TStrPlus.BytesToHexStr(@m_digestdata, MD5_DIGESTSIZE, Chr(0));
end;

function TMD5Digest.SetHexStr(sSource : String) : Boolean;
begin
  // avoid buffer overflow by prechecking the length
  if (Length(sSource) <> MD5_DIGESTSIZE * 2) then
    Result:=False
  else
    Result:=(TStrPlus.HexStrToBytes(sSource, @m_digestdata) <> -1);
end;

function TMD5Digest.Equals(compare : TDigest) : Boolean;
var
  lI : WORD32;
begin
  for lI:=0 to (MD5_DIGESTSIZE - 1) do
    if (m_digestdata[lI] <> TMD5Digest(compare).m_digestdata[lI]) then begin
      Result:=False;
      Exit;
    end;
  Result:=True;
end;

function TMD5Digest.GetDigestDataPtr : Pointer;
begin
  Result:=@m_digestdata;
end;

function TMD5Digest.GetFolded32 : WORD32;
var
  pBuf : PWORD32Buf;
begin
  pBuf:=@m_digestdata;
  Result:=pBuf^[0] xor pBuf^[1] xor pBuf^[2] xor pBuf^[3];
end;




//////////////////// TMD5 ////////////////////


constructor TMD5.Create;
begin
  m_pCtx:=MD5_Create;
end;


destructor TMD5.Destroy;
begin
  MD5_Destroy(m_pCtx);
end;


procedure TMD5.Reset;
begin
  MD5_Reset(m_pCtx);
end;


procedure TMD5.Update(pData : Pointer; lNumOfBytes : WORD32);
begin
  MD5_Update(m_pCtx,
             pData,
             lNumOfBytes);
end;


procedure TMD5.Finalize(digest : TDigest);
begin
  MD5_Final(digest.GetDigestDataPtr,
            m_pCtx);
end;


function TMD5.SelfTest : Boolean;
begin
  Result:=(MD5_SelfTest = BOOL_TRUE);
end;



end.
