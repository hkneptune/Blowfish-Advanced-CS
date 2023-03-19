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
  secure random number generation support class
}

unit RandomPool;

{$I config.inc}

interface
uses bfacslib;


// class type for encapsulating the random generation process
type
  TRandomPool = class
  private
    // here we store the random pool context
    m_pCtx : PRANDOMPOOLCTX;

  public
    // creates a new random pool
    // -> pointer to additional seed data, ignored if Nil
    // -> number of additional seed bytes
    constructor Create(pAddSeed : Pointer; lAddSeedLen : WORD32);

    // just cleans the object
    destructor Destroy; override;

    // reseeds the random generator
    procedure Reseed(pSeed : Pointer; lSeedLen : WORD32);

    // retrieves some random bytes from the epool
    procedure GetRandomBytes(pTarget : Pointer; lNumOfBytes : WORD32);
  end;


implementation


constructor TRandomPool.Create(pAddSeed : Pointer; lAddSeedLen : WORD32);
begin
  // create new new random pool context
  m_pCtx:=RandomPool_Create(pAddSeed, lAddSeedLen);
end;


destructor TRandomPool.Destroy;
begin
  RandomPool_Destroy(m_pCtx);
end;

procedure TRandomPool.Reseed(pSeed : Pointer; lSeedLen : WORD32);
begin
  RandomPool_Reseed(m_pCtx,
                    pSeed,
                    lSeedLen);
end;


procedure TRandomPool.GetRandomBytes(pTarget : Pointer; lNumOfBytes : WORD32);
begin
  RandomPool_GetData(m_pCtx,
                     pTarget,
                     lNumOfBytes);
end;


end.
