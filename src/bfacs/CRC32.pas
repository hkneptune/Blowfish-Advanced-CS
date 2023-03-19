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
  CRC32 support class
}

unit CRC32;

{$I config.inc}

interface
uses bfacslib;


// class type for encapsulating the CRC32 calculation process
type
  TCRC32 = class
  private
    // here we store the CRC32 checksum
    m_lCRC32 : WORD32;
  public
    // creates the instance and resets the CRC32
    constructor Create;

    // clears the instance
    destructor Destroy; override;

    // gets the pure value of the current CRC32
    // <- current CRC32
    function GetValue : WORD32;

    // sets the internal value
    // -> new CRC32
    procedure SetValue(lNewCRC32 : WORD32);

    // returns the finalized CRC32
    // <- CRC32
    function Finalize : WORD32;

    // resets the CRC32 to the start value
    procedure Reset;

    // modifies the CRC32 with new data bytes
    // -> data to checksum
    // -> number of bytes to checksum
    procedure Update(pData : Pointer; lNumOfBytes : WORD32);
  end;

implementation


constructor TCRC32.Create;
begin
  Reset;
end;

destructor TCRC32.Destroy;
begin
  m_lCRC32:=0;
end;


function TCRC32.GetValue : WORD32;
begin
  Result:=m_lCRC32;
end;


procedure TCRC32.SetValue(lNewCRC32 : WORD32);
begin
  m_lCRC32:=lNewCRC32;
end;


function TCRC32.Finalize : WORD32;
begin
  m_lCRC32:=m_lCRC32 xor CRC32_DONEVALUE;
  Result:=m_lCRC32;
end;


procedure TCRC32.Reset;
begin
  m_lCRC32:=CRC32_INITVALUE;
end;


procedure TCRC32.Update(pData : Pointer; lNumOfBytes : WORD32);
begin
  m_lCRC32:=CRC32_Update(m_lCRC32, pData, lNumOfBytes);
end;


end.
