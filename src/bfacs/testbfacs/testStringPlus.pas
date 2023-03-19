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

unit testStringPlus;

interface

function TestStrPlus : Boolean;

implementation
uses StringPlus, bfacslib;

///////////////////////////////////////////////////////////////////////////////

function TestBase64 : Boolean;
var
  nLen, nDec, nChr : Integer;
  sRaw, sEnc : String;
  dec : PChar;
begin

  Result := False;

  for nLen := 0 to (256 - Ord(' ')) do begin

    SetLength(sRaw, 0);

    for nChr := Ord(' ') to (Ord(' ') + nLen - 1) do begin
      sRaw := sRaw + Char(nChr);
    end;

    sEnc := TStrPlus.Base64Encode(PChar(sRaw), nLen);

    //WriteLn(sEnc);

    dec := PChar(TStrPlus.Base64Decode(sEnc, nDec));

    if (Nil = dec) then Exit;
    if (nDec <> nLen) then Exit;
    if (String(dec) <> sRaw) then Exit;

    FreeMem(dec);

  end;

  Result := True;

  WriteLn('BASE64 test OK');
end;

///////////////////////////////////////////////////////////////////////////////

function TestStrPlus : Boolean;
begin

  Result := False;

  if (TestBase64) then
    Result := True;
end;

end.
