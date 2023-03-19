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
  everything to provide a flexible and seedable system random source
}


unit RandomManager;

interface
uses
  Configuration,
  SeedBuffer,
  RandomSource;


// currently supported random generators
const
  RANDOMMANAGER_SOURCE_YARROW = 0;

  
type
  TRandomManager = class
  private
    // members
    m_rndSrc  : TRandomSource;
    m_seedBuf : TSeedBuffer;

  public

    // constructor
    // -> type (see RANDOMMANANGER_SOURCE_xxx)
    constructor Create(nType : Integer);

    // destructor
    destructor Destroy; override;

    // add seed bytes from every possible source
    // -> pointer to the byte buffer
    // -> number of bytes to add
    procedure AddSeed(pSource : Pointer;
                      nNumOfBytes : Integer); overload;

    // add a single integer seed
    // -> the seed to add
    procedure AddSeed(nSeed : Integer); overload;

    // flushes the seed buffer
    procedure FlushSeed;

    // gets the _reference_ of the current random generator
    // <- current random source
    function GetRandomSource : TRandomSource;

    // gets the _reference_ of the seed buffer (just needed for debug purposes)
    // <- seed buffer
    function GetSeedBuffer : TSeedBuffer;
  end;


implementation
uses
  Yarrow;
  

//////////////////////// TRandomManager ////////////////////////


// fixed size of the seed buffer (enough to store several mouse move events)
const
  SEED_BUF_SIZE = 512;


constructor TRandomManager.Create(nType : Integer);
begin

  // we ignore the type, Yarrow is the one and only 
  m_rndSrc:=TYarrow.Create;

  m_seedBuf:=TSeedBuffer.Create(m_rndSrc, SEED_BUF_SIZE);

  // (auto-flush rulez)
  m_seedBuf.SetAutoFlush(True);
end;


destructor TRandomManager.Destroy;
begin
  // release all instances
  m_rndSrc.Destroy;
  m_seedBuf.Destroy;
end;


procedure TRandomManager.AddSeed(pSource : Pointer;
                                 nNumOfBytes : Integer);
begin
  m_seedBuf.AddSeed(pSource, nNumOfBytes);
end;


procedure TRandomManager.AddSeed(nSeed : Integer);
begin
  m_seedBuf.AddSeed(@nSeed, SizeOf(Integer));
end;


procedure TRandomManager.FlushSeed;
begin
  m_seedBuf.Flush;
end;

function TRandomManager.GetRandomSource : TRandomSource;
begin
  Result:=m_rndSrc;
end;

function TRandomManager.GetSeedBuffer : TSeedBuffer;
begin
  Result:=m_seedBuf;
end;


end.
