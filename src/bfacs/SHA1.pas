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
  support class implementing the SHA-1 digest algorithm through BFACSLIB.DLL
}

unit SHA1;

{$I config.inc}

interface
uses bfacslib, Digest;


// size of an SHA-1 digest
const
  SHA1_DIGESTSIZE = bfacslib.SHA1_DIGESTSIZE;


// SHA-1 digest class type
type
  TSHA1Digest = class(TDigest)
  private
    m_digestdata : array[0..SHA1_DIGESTSIZE - 1] of WORD8;

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
    // -> the source string
    // <- True: conversion was successful / False: error in string
    function SetHexStr(sSource : String) : Boolean; override;

    // compares this digest with another one
    // -> the digest to compare with
    // <- True: equal / False: not equal
    function Equals(compare : TDigest) : Boolean; override;

    // returns a pointer to the digest data buffer
    // <- pointer to the digest data buffer
    function GetDigestDataPtr : Pointer; override;
  end;



// class type for encapsulating the hash generation process
type
  TSHA1 = class(TDigestProducer)
  private
    // here we store the SHA-1 context
    m_pCtx : PSHA1CTX;

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
    // -> where to put the digest
    procedure Finalize(digest : TDigest); override;

    // runs a selftest
    // <- True: selftest succeeded / False: selftest failed
    function SelfTest : Boolean; override;
  end;



implementation
uses SysUtils,
     StringPlus;


//////////////////// TSHA1Digest ////////////////////

constructor TSHA1Digest.Create;
begin
  FillChar(m_digestdata, SHA1_DIGESTSIZE, 0);
end;

destructor TSHA1Digest.Destroy;
begin
  FillChar(m_digestdata, SHA1_DIGESTSIZE, 0);
end;

function TSHA1Digest.GetDigestSize : Integer;
begin
  GetDigestSize:=SHA1_DIGESTSIZE;
end;

procedure TSHA1Digest.GetData(pTarget : Pointer);
begin
  Move(m_digestdata, pTarget^, SHA1_DIGESTSIZE);
end;

procedure TSHA1Digest.SetData(pSource : Pointer);
begin
  Move(pSource^, m_digestdata, SHA1_DIGESTSIZE);
end;

procedure TSHA1Digest.CopyFrom(source : TDigest);
begin
  source.GetData(@m_digestdata);
end;

function TSHA1Digest.GetHexStr : String;
begin
  Result:=TStrPlus.BytesToHexStr(@m_digestdata, SHA1_DIGESTSIZE, Chr(0));
end;


function TSHA1Digest.SetHexStr(sSource : String) : Boolean;
begin
  // avoid buffer overflow by prechecking the length
  if (Length(sSource) <> SHA1_DIGESTSIZE * 2) then
    Result:=False
  else
    Result:=(TStrPlus.HexStrToBytes(sSource, @m_digestdata) <> -1);
end;

function TSHA1Digest.Equals(compare : TDigest) : Boolean;
var
  lI : WORD32;
begin
  for lI:=0 to (SHA1_DIGESTSIZE - 1) do
    if (m_digestdata[lI] <> TSHA1Digest(compare).m_digestdata[lI]) then begin
      Result:=False;
      Exit;
    end;
  Result:=True;
end;

function TSHA1Digest.GetDigestDataPtr : Pointer;
begin
  Result:=@m_digestdata;
end;



//////////////////// TSHA1 ////////////////////


constructor TSHA1.Create;
begin
  m_pCtx:=SHA1_Create;
end;


destructor TSHA1.Destroy;
begin
  SHA1_Destroy(m_pCtx);
end;


procedure TSHA1.Reset;
begin
  SHA1_Reset(m_pCtx);
end;


procedure TSHA1.Update(pData : Pointer; lNumOfBytes : WORD32);
begin
  SHA1_Update(m_pCtx, pData, lNumOfBytes);
end;


procedure TSHA1.Finalize(digest : TDigest);
begin
  SHA1_Final(digest.GetDigestDataPtr, m_pCtx);
end;


function TSHA1.SelfTest : Boolean;
begin
  Result:=(SHA1_SelfTest = BOOL_TRUE);
end;


end.

