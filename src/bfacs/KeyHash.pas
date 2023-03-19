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
  classes for creating secure key hashes
}

unit KeyHash;

interface
uses bfacslib;


// base class
type
  TKeyHash = class
  protected
    m_pSalt    : Pointer;
    m_nSaltLen : Integer;
    m_pKey     : Pointer;
    m_nKeyLen  : Integer;

  public
    // constructor
    // -> pointer to salt (may be Nil)
    // -> number of salt bytes
    // -> pointer to key material
    // -> number of key bytes
    constructor Create(pSetSalt : Pointer;
                       nSetSaltLen : Integer;
                       pSetKey : Pointer;
                       nSetKeyLen : Integer); virtual;

    // destructor
    destructor Destroy; override;
  end;


// for simple key hashing
type
  TKeyHashSimple = class(TKeyHash)
  public
    // create a 32bit hash
    // <- the hash
    function Make32 : WORD32;
  end;


// for extended key hashing (to avoid heavy brute-force attacks)
type
  TKeyHashExtended = class(TKeyHash)
  private
    nIterations : Integer;

  public
    // default constructor
    constructor Create(pSetSalt : Pointer;
                       nSetSaltLen : Integer;
                       pSetKey : Pointer;
                       nSetKeyLen : Integer); override;

    // extended constructor, allows to set the number of iterations
    // -> pointer to salt (may be Nil)
    // -> number of salt bytes
    // -> pointer to key material
    // -> number of key bytes
    // -> number of iterations
    constructor CreateEx(pSetSalt : Pointer;
                         nSetSaltLen : Integer;
                         pSetKey : Pointer;
                         nSetKeyLen : Integer;
                         nSetIterations : Integer);

    // destructor
    destructor Destroy; override;

    // create a 32bit hash
    // <- the hash
    function Make32 : WORD32;
  end;

implementation

// default number of interations for the KeyHashExtended class
const
   DEF_NUM_OF_ITERATIONS = 32768;

constructor TKeyHash.Create(pSetSalt : Pointer;
                            nSetSaltLen : Integer;
                            pSetKey : Pointer;
                            nSetKeyLen : Integer);
begin
  // copy the parameters
  m_pSalt:=pSetSalt;
  m_nSaltLen:=nSetSaltLen;
  m_pKey:=pSetKey;
  m_nKeyLen:=nSetKeyLen;
end;


destructor TKeyHash.Destroy;
begin
  // clear the parameters
  m_pSalt:=Nil;
  m_nSaltLen:=0;
  m_pKey:=Nil;
  m_nKeyLen:=0;
end;


function TKeyHashSimple.Make32 : WORD32;
var
  // due to speed reasons we use the CryptPak API directly
  pCtx : PMD5CTX;
  hash : array[0..MD5_DIGESTSIZE - 1] of WORD8;
  pBuf : PWORD32Buf;
begin

  // hash down (salt and) key
  pCtx:=MD5_Create;
  if (m_pSalt <> Nil) then
    MD5_Update(pCtx, m_pSalt, m_nSaltLen);
  MD5_Update(pCtx, m_pKey, m_nKeyLen);
  MD5_Final(@hash, pCtx);
  MD5_Destroy(pCtx);

  // return the 32bit folded hash
  pBuf:=@hash;
  Result:=pBuf^[0] xor pBuf^[1] xor pBuf^[2] xor pBuf^[3];
  FillChar(hash, MD5_DIGESTSIZE, 0);
end;



constructor TKeyHashExtended.Create(pSetSalt : Pointer;
                                    nSetSaltLen : Integer;
                                    pSetKey : Pointer;
                                    nSetKeyLen : Integer);
begin
  // init. the basics
  inherited Create(pSetSalt, nSetSaltLen, pSetKey, nSetKeyLen);

  // use the default number of iterations
  nIterations:=DEF_NUM_OF_ITERATIONS;
end;


constructor TKeyHashExtended.CreateEx(pSetSalt : Pointer;
                                      nSetSaltLen : Integer;
                                      pSetKey : Pointer;
                                      nSetKeyLen : Integer;
                                      nSetIterations : Integer);
begin
  // init. the basics
  inherited Create(pSetSalt, nSetSaltLen, pSetKey, nSetKeyLen);

  // use the given number of iterations
  nIterations:=nSetIterations;
end;


destructor TKeyHashExtended.Destroy;
begin
  // clear the iterations parameter
  nIterations:=0;

  // clear the basics
  inherited Destroy
end;


function TKeyHashExtended.Make32 : WORD32;
var
  // due to speed reasons we use the CryptPak API directly
  pCtx : PMD5CTX;
  hash : array[0..MD5_DIGESTSIZE - 1] of WORD8;
  pBuf : PWORD32Buf;
  nI   : Integer;
begin

  // hash down (salt and) key
  pCtx:=MD5_Create;
  if (m_pSalt <> Nil) then
    MD5_Update(pCtx, m_pSalt, m_nSaltLen);
  MD5_Update(pCtx, m_pKey, m_nKeyLen);
  MD5_Final(@hash, pCtx);
  MD5_Destroy(pCtx);

  // repeat this procedure plus hashing the previous digest, too
  for nI:=0 to (nIterations - 1) do begin

    pCtx:=MD5_Create;
    MD5_Update(pCtx, @hash, MD5_DIGESTSIZE);
    if (m_pSalt <> Nil) then
      MD5_Update(pCtx, m_pSalt, m_nSaltLen);
    MD5_Update(pCtx, m_pKey, m_nKeyLen);
    MD5_Final(@hash, pCtx);
    MD5_Destroy(pCtx);
  end;

  // return the 32bit folded hash
  pBuf:=@hash;
  Result:=pBuf^[0] xor pBuf^[1] xor pBuf^[2] xor pBuf^[3];
  FillChar(hash, MD5_DIGESTSIZE, 0);
end;



end.
