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
    thread-safe random source class, designed for quickly switching to another
    random generator (actually we're using a RandomPoolClass instance)
}

unit RandomSource;

interface
uses Windows,
     RandomPool,
     bfacslib;


// abstract base class
type
  TRandomSource = class
  public
    // reseeds the random generator
    // -> pointer to additional seed material
    // -> number of additional seed material bytes
    procedure Reseed(pSeed : Pointer;
                     nSeedLen : Integer); virtual; abstract;

    // get random bytes
    // -> pointer to the target buffer
    // -> number of random bytes to create
    procedure GetBytes(pTarget : Pointer;
                       nNumOfBytes : Integer); virtual; abstract;
  end;


// random generator implememation bases on the bfacslib.dll routines
type
  TRandomSourceImpl = class (TRandomSource)
  private
    m_thePool : TRandomPool;
    m_hSema   : THandle;
  public
    // creates a new random source
    constructor Create;

    // destroys the random source
    destructor Destroy; override;

    // reseeds the random generator
    // -> pointer to additional seed material
    // -> number of additional seed material bytes
    procedure Reseed(pSeed : Pointer;
                     nSeedLen : Integer); override;

    // get random bytes
    // -> pointer to the target buffer
    // -> number of random bytes to create
    procedure GetBytes(pTarget : Pointer;
                       nNumOfBytes : Integer); override;
  end;


// callback function type for receiving random data from an external source,
// commonly used in DLLs for building a bridge to classes
// -> where to copy the random bytes
// -> number of random bytes to deliver
// -> additional data pointer to pass (mainly to an object instance)
type
  TRandGenProc = procedure(pTargetBuffer : Pointer;
                           lNumOfRandomBytes : WORD32;
                           pData : Pointer); stdcall;



// class for random data generation through an external source procedure
type
  TProcRandSrc = class(TRandomSource)
  private
    m_RandGen      : TRandGenProc;
    m_pRandGenData : Pointer;

  public
    // constructor (just copies the random procedure reference)
    // -> address of random number generation function
    // -> pointer to random generation data
    constructor Create(SetRandGen : TRandGenProc;
                       pSetRandGenData : Pointer);

    // reseeds the random generator
    // -> pointer to additional seed material
    // -> number of additional seed material bytes
    procedure Reseed(pSeed : Pointer;
                     nSeedLen : Integer); override;

    // get random bytes
    // -> pointer to the target buffer
    // -> number of random bytes to create
    procedure GetBytes(pTarget : Pointer;
                       nNumOfBytes : Integer); override;
  end;




implementation


//////////////////////////// TRandomSourceImpl ////////////////////////////


constructor TRandomSourceImpl.Create;
begin
  // create our semaphore
  m_hSema:=CreateSemaphore(Nil, 1, 1, Nil);

  // create a new random pool
  m_thePool:=TRandomPool.Create(Nil, 0);
end;


destructor TRandomSourceImpl.Destroy;
begin
  m_thePool.Destroy;
end;


procedure TRandomSourceImpl.Reseed(pSeed : Pointer; nSeedLen : Integer);
begin
  // wait until the instance is not in usage
  WaitForSingleObject(m_hSema, INFINITE);

  // reseed
  m_thePool.Reseed(pSeed, nSeedLen);

  // free the instance
  ReleaseSemaphore(m_hSema, 1, Nil);
end;


procedure TRandomSourceImpl.GetBytes(pTarget : Pointer; nNumOfBytes : Integer);
begin
  // wait until the instance is not in usage
  WaitForSingleObject(m_hSema, INFINITE);

  // create random bytes
  m_thePool.GetRandomBytes(pTarget, nNumOfBytes);

  // free the instance
  ReleaseSemaphore(m_hSema, 1, Nil);
end;


//////////////////////////// TProcRandSrc ////////////////////////////

constructor TProcRandSrc.Create(SetRandGen : TRandGenProc;
                                pSetRandGenData : Pointer);
begin
  m_RandGen:=SetRandGen;
  m_pRandGenData:=pSetRandGenData;
end;


procedure TProcRandSrc.Reseed(pSeed : Pointer;
                              nSeedLen : Integer);
begin
  // (we need no reseed capability in this class,
  //  because all is external)
end;


procedure TProcRandSrc.GetBytes(pTarget : Pointer;
                                nNumOfBytes : Integer);
begin
  // just map the call
  m_RandGen(pTarget, nNumOfBytes, m_pRandGenData);
end;



end.
