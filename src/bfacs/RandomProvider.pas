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
    global random source holder, allows to exchange the random sources and
    handles the (re)seeding, too
}

unit RandomProvider;

{$I config.inc}

interface
uses RandomSource, SeedBuffer;


// random source IDs
const
  RANDOMPROVIDER_SOURCE_CRYPTPAK = 0;
  RANDOMPROVIDER_SOURCE_YARROW   = 1;
  RANDOMPROVIDER_SOURCE_EXTERNAL = 2;


// the provider class
type
  TRandomProvider = class
  private

    // the seed buffer
    m_seedBuf : TSeedBuffer;

    // the current random source
    m_randSrc : TRandomSource;

  public

    // constructor
    // -> random source ID, see RANDOMPROVIDER_SOURCE_xxx
    // -> name of the random generator DLL (only needed
    //                    if nSourceID = RANDOMPROVIDER_SOURCE_EXTERNAL)
    constructor Create(nSourceID : Integer;
                       sRandGenDLL : String = '');

    // destructor
    destructor Destroy; override;

    // get the _reference_ of the current random source
    // <- random source
    function GetRandomSource : TRandomSource;

    // get some random bytes
    // -> pointer to the destination buffer
    // -> number of random bytes
    procedure GetRandomData(pDest : Pointer;
                            nNumOfBytes : Integer);

    // reseed the generator
    // -> pointer to the seed data
    // -> number of seed bytes
    procedure AddSeed(pSeed : Pointer;
                      nNumOfBytes : Integer);

    // exchanges the random generator
    // -> random source ID, see RANDOMPROVIDER_SOURCE_xxx
    // -> name of the random generator DLL (only needed
    //    if nSourceID = RANDOMPROVIDER_SOURCE_EXTERNAL)
    constructor ExchangeGenerator(nSourceID : Integer;
                                  sRandGenDLL : String = '');
  end;


// our global provider instance
var
  _rndPrv : TRandomProvider;


// to create the global random provider
// -> random source ID, see RANDOMPROVIDER_SOURCE_xxx
// -> name of the random generator DLL (if necessary)
procedure _CreateRandomProvider(nSourceID : Integer;
                                sRandGenDLL : String = '');

// to destroy the global random provider
procedure _DestroyRandomProvider;


implementation


procedure _CreateRandomProvider(nSourceID : Integer;
                                sRandGenDLL : String = '');
begin
  _rndPrv:=TRandomProvider.Create(nSourceID,
                                  sRandGenDLL);
end;

procedure _DestroyRandomProvider;
begin
  _rndPrv.Destroy;
end;


//////////////////////////// TRandomProvider ////////////////////////////


// we flush the seed automatically every 128 bytes
const
  SEED_FLUSH_LIMIT = 128;


constructor TRandomProvider.Create(nSourceID : Integer;
                                   sRandGenDLL : String = '');
begin

  // case nSourceID of ...

  m_randSrc:=TRandomSourceImpl.Create;


  // create the seed buffer
  m_seedBuf:=TSeedBuffer.Create(m_randSrc, SEED_FLUSH_LIMIT);
  m_seedBuf.SetAutoFlush(True);
end;


destructor TRandomProvider.Destroy;
begin

  // destroy the random source, whatever it is
  m_randSrc.Destroy;

  // destroy the seed buffer
  m_seedBuf.Destroy;
end;


function TRandomProvider.GetRandomSource : TRandomSource;
begin
  Result:=m_randSrc;
end;


procedure TRandomProvider.GetRandomData(pDest : Pointer;
                                        nNumOfBytes : Integer);
begin
  m_randSrc.GetBytes(pDest, nNumOfBytes);
end;


procedure TRandomProvider.AddSeed(pSeed : Pointer;
                                  nNumOfBytes : Integer);
begin
  // just map the call to the seed buffer method
  m_seedBuf.AddSeed(pSeed, nNumOfBytes);
end;


constructor TRandomProvider.ExchangeGenerator(nSourceID : Integer;
                                              sRandGenDLL : String = '');

begin

  // case nSourceID of ...

end;



end.
