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
  Yarrow random number generator, wrapper around the CryptPak implementation.
}

unit Yarrow;

interface
uses
  bfacslib,
  RandomSource;

type
  TYarrow = class(TRandomSource)
  private
    m_ctx : PYARROWCTX;
  public

    constructor Create;
    destructor Destroy; override;
    procedure Reseed(pSeed : Pointer; nSeedLen : Integer); override;
    procedure GetBytes(pTarget : Pointer; nNumOfBytes : Integer); override;
  end;

implementation
uses
  Windows;

//////////////////////////// TYarrow ////////////////////////////

constructor TYarrow.Create;
begin
  m_ctx:=Yarrow_Create(Nil, 0);
end;

destructor TYarrow.Destroy;
begin
  Yarrow_Destroy(m_ctx);
end;

procedure TYarrow.GetBytes(pTarget : Pointer; nNumOfBytes : Integer);
begin
  Yarrow_GetData(m_ctx, pTarget, nNumOfBytes);
end;

procedure TYarrow.Reseed(pSeed : Pointer; nSeedLen : Integer);
begin
  Yarrow_Reseed(m_ctx, pSeed, nSeedLen);
end;

end.
