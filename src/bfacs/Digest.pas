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
  base classes for message digest algorithms
}

unit Digest;

{$I config.inc}

interface
uses bfacslib;

// abstract digest basetype class
type
  PDigest = ^PDigest;
  TDigest = class
  public
    // creates an instance
    constructor Create; virtual; abstract;

    // just returns the digest size
    // <- digest size
    function GetDigestSize : Integer; virtual; abstract;

    // delivers digest data
    // -> where to copy the digest bytes
    procedure GetData(pTarget : Pointer); virtual; abstract;

    // absorbes digest data
    // -> from where to copy digest data
    procedure SetData(pSource : Pointer); virtual; abstract;

    // copies digest data from another instance
    // -> instance from where to copy digest data
    procedure CopyFrom(source : TDigest); virtual; abstract;

    // returns a hexadecimal representation of the digest data
    // <- digest hex string
    function GetHexStr : String; virtual; abstract;

    // converts a string with hex bytes bak to digest data
    // -> the source string
    // <- True: conversion was successful / False: error in string
    function SetHexStr(sSource : String) : Boolean; virtual; abstract;

    // compares this digest with another one
    // -> the digest to compare with
    // <- True: equal / False: not equal
    function Equals(compare : TDigest) : Boolean; virtual; abstract;

    // returns a pointer to the digest data buffer
    // <- pointer to the digest data buffer
    function GetDigestDataPtr : Pointer; virtual; abstract;
  end;



// class type for digest algorithms
type
  TDigestProducer = class
  public
    // construtor
    constructor Create; virtual; abstract;

    // resets the context for a new hash session
    procedure Reset; virtual; abstract;

    // hashes an amount of data
    // -> pointer to the memory block to hash
    // -> number of bytes to hash
    procedure Update(pData : Pointer;
                     lNumOfBytes : WORD32); virtual; abstract;

    // finalizes and copies a digest
    // -> keeper where to put the digest
    procedure Finalize(digestKeeper : TDigest); virtual; abstract;

    // runs a selftest
    // <- True: selftest succeeded / False: selftest failed
    function SelfTest : Boolean; virtual; abstract;
  end;


implementation

// nothing to implement here

end.
