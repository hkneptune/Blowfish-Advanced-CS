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
  support class offering key creation through CryptPak
}

unit KeyCreator;

interface
uses bfacslib, SecureMem;


// the support class is really simple
type
  TKeyCreator = class
  private
    m_pSalt     : Pointer;
    m_nSaltLen  : Integer;
    m_pPassw    : Pointer;
    m_nPasswLen : Integer;
  public
    // sets the parameters for key creation (only pointers are given to avoid
    // unnecessary key storing)
    // -> pointer to the key salt (may be Nil, if no salt)
    // -> number of salt bytes (ignored if pSetSalt is Nil)
    // -> pointer to the key material
    // -> number of key bytes
    constructor Create(pSetSalt : Pointer;
                       nSetSaltLen : Integer;
                       pSetPassw : Pointer;
                       nSetPasswLen : Integer);

    // clears the parameters
    destructor Destroy; override;

    // generates the hashed key
    // -> number of key bytes to create
    // <- hashed key in a secure memory area
    // exception: EOutOfMemory
    function MakeKey(nKeySize : Integer) : TKeyMemory;

  end;


implementation
uses SysUtils;


constructor TKeyCreator.Create(pSetSalt : Pointer;
                               nSetSaltLen : Integer;
                               pSetPassw : Pointer;
                               nSetPasswLen : Integer);
begin
  // just copy the parameters
  m_pSalt:=pSetSalt;
  if (m_pSalt = Nil) then
    m_nSaltLen:=0
  else
    m_nSaltLen:=nSetSaltLen;
  m_pPassw:=pSetPassw;
  m_nPasswLen:=nSetPasswLen;
end;


destructor TKeyCreator.Destroy;
begin
  m_pSalt:=Nil;
  m_nSaltLen:=0;
  m_pPassw:=Nil;
  m_nPasswLen:=0;
end;


function TKeyCreator.MakeKey(nKeySize : Integer) : TKeyMemory;
var
  buildBuf : TKeyMemory;
begin

  Result:=Nil;
  try
    Result:=TKeyMemory.Create(nKeySize);
    buildBuf:=TKeyMemory.Create(
                Support_GetCrunchKeyBuildBufSize(
                   m_nPasswLen,
                   m_nSaltLen,
                   nKeySize,
                   CRUNCHKEY_METHOD_SHAEXTXORLOOP));
  except
    on EOutOfMemory do begin
      if (Result <> Nil) then
        Result.Destroy;
      raise;
    end;
  end;

  if (nKeySize = 0) then begin
    buildBuf.Destroy;
    Exit;
  end;
  if ((m_nSaltLen = 0) and (m_nPasswLen = 0)) then begin
    buildBuf.Destroy;
    Result.Clear;
    Exit;
  end;

  Support_CrunchKey(
    m_pPassw,
    m_nPasswLen,
    m_pSalt,
    m_nSaltLen,
    Result.GetPtr,
    nKeySize,
    CRUNCHKEY_METHOD_SHAEXTXORLOOP,
    buildBuf.GetPtr);

  buildBuf.Destroy;
end;


end.



