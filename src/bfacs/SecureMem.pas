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
  class supporting secure (nondiscardable) memory,
  especially for storing key material
}

unit SecureMem;

interface
uses bfacslib,
     RandomSource;


// base class (can, but should not be used)
type
  TSecureMem = class
  private
    m_hMappedFileMem : Integer;

  protected
    m_pMappedMem  : Pointer;
    m_nSize       : Integer;
    m_blConfirmed : Boolean;

  public
    // constructor, allocates the mapped memory area
    // -> number of bytes to allocate
    // exception: EOutOfMemory - if out of memory
    constructor Create(nAmount : Integer); overload; virtual;

    // destructor, erases and frees the allocates memory
    destructor Destroy; override;

    // just return the size of the allocated memory area
    // <- size of the memory area (may be zero of error)
    function GetSize : Integer; virtual;

    // copies some external data to our memory area
    // -> from where to copy the data
    // -> byte position from which to start copying
    // -> number of bytes to copy
    procedure SetData(pSource : Pointer;
                      nPos : Integer;
                      nNumOfBytes : Integer); virtual;

    // copies some data from the memory area to an external storage
    // -> where to copy the data
    // -> from which byte position to start copying
    // -> number of bytes to copy
    procedure GetData(pTarget : Pointer;
                      nPos : Integer;
                      nNumOfBytes : Integer); virtual;

    // clears the memory area with zero bytes
    procedure Clear; virtual;

    // to set the confirmed flag
    // -> True: key is confirmed / False: not confirmed
    procedure SetConfirmed(blState : Boolean);

    // to check if the data was confirmed
    // <- True: data was confirmed / False: was not
    function GetConfirmed : Boolean;

  end;



// use this class instead
type
  TKeyMemory = class(TSecureMem)
  public

    // just return the pointer to the allocated memory area,
    // we're allowed to access the buffer directly
    // <- pointer to the memory area (may be Nil)
    function GetPtr : Pointer; virtual;

    // creates a copy of this instance
    // <- the copy
    function Clone : TKeyMemory; virtual;
  end;


// the best, but slowest secure memory solution
type
  TEncryptedKeyMemory = class(TSecureMem)
  protected
    m_pKeyMem : Pointer;

  public
    // constructor for encrypted memory
    // -> number of bytes to allocate
    // -> instance of a random source
    // exception:
    // EOutOfMemory - if out of memory
    constructor Create(nAmount : Integer;
                       rs : TRandomSource); reintroduce; overload;

    // destructor for encrypted memory
    destructor Destroy; override;

    // copies some external data to our encrypted memory area
    // -> from where to copy the data
    // -> byte position from which to start copying
    // -> number of bytes to copy
    procedure SetData(pSource : Pointer;
                      nPos : Integer;
                      nNumOfBytes : Integer); override;

    // copies some data from the encrypted memory area to an external storage
    // -> where to copy the data
    // -> from which byte position to start copying
    // -> number of bytes to copy
    procedure GetData(pTarget : Pointer;
                      nPos : Integer;
                      nNumOfBytes : Integer); override;

    // clears the encrypted memory area
    procedure Clear; override;

    // creates a copy of this instance
    // -> instance of a random source
    // <- the copy
    function Clone(rs : TRandomSource) : TEncryptedKeyMemory;

    // creates a copy of the key, but store in standard key memory
    // <- the copy
    function CloneAsKeyMemory : TKeyMemory;

  end;


implementation
uses Windows, sysutils;


//////////////////////////// TSecureMem ////////////////////////////


// some bytes for nearly-SFS-like memory wiping

const
  NUM_OF_WIPES = 27;
  wipeMemData : array[0..(NUM_OF_WIPES - 1), 0..2] of WORD8= (
	($55, $55, $55),
	($aa, $aa, $aa),
	($92, $49, $24),
	($49, $24, $92),
	($24, $92, $49),
	($00, $00, $00),
	($11, $11, $11),
	($22, $22, $22),
	($33, $33, $33),
	($44, $44, $44),
	($55, $55, $55),
	($66, $66, $66),
	($77, $77, $77),
	($88, $88, $88),
	($99, $99, $99),
	($aa, $aa, $aa),
	($bb, $bb, $bb),
	($cc, $cc, $cc),
	($dd, $dd, $dd),
	($ee, $ee, $ee),
	($ff, $ff, $ff),
	($92, $49, $24),
	($49, $24, $92),
	($24, $92, $49),
	($6d, $b6, $db),
	($b6, $db, $6d),
	($db, $6d, $b6) );



constructor TSecureMem.Create(nAmount : Integer);
begin
  // init. the members
  m_nSize:=0;
  m_hMappedFileMem:=0;
  m_pMappedMem:=Nil;

  // avoid zero allocations
  if (nAmount= 0) then
    Exit;

  // create the (virtual) file mapping
  m_hMappedFileMem:=CreateFileMapping(THandle($ffffffff),
                                      Nil,
                                      PAGE_READWRITE,
                                      0,
                                      nAmount,
                                      Nil);
  if (m_hMappedFileMem = 0) then
    OutOfMemoryError;

  // get it into our address space
  m_pMappedMem:=MapViewOfFile(m_hMappedFileMem,
                              FILE_MAP_WRITE,
                              0,
                              0,
                              0);

  if (m_pMappedMem = Nil) then begin
    CloseHandle(m_hMappedFileMem);
    OutOfMemoryError();
  end;

  // not confirmed as default
  m_blConfirmed:=False;

  // success
  m_nSize:=nAmount;
end;


destructor TSecureMem.Destroy;
var
  nI, nJ  : Integer;
  nOffset : Integer;
  pToWipe : PWORD8Buf;
begin

  // wipe the memory block
  if (m_pMappedMem <> Nil) then begin
    pToWipe:=m_pMappedMem;
    for  nI:=0 to (NUM_OF_WIPES - 1) do begin
      nOffset:=0;
      if (m_nSize > 3) then begin
        while (nOffset < (m_nSize - 3)) do begin
          pToWipe^[nOffset]    :=wipeMemData[nI][0];
          pToWipe^[nOffset + 1]:=wipeMemData[nI][1];
          pToWipe^[nOffset + 2]:=wipeMemData[nI][2];
          Inc(nOffset, 3);
        end;
        Dec(nOffset, 3);
      end;
      nJ:=0;
      while (nJ < (m_nSize and 3)) do begin
        pToWipe^[nOffset + nJ]:=wipeMemData[nI][nJ];
        Inc(nJ);
      end;
      FlushViewOfFile(pToWipe, 0);
    end;
  end;

  // unmap it
  if (m_pMappedMem <> Nil) then
    UnmapViewOfFile(m_pMappedMem);

  // close the mapping, i.n.
  if (m_hMappedFileMem <> 0) then
    CloseHandle(m_hMappedFileMem);

  // at least erase all members
  m_hMappedFileMem:=0;
  m_pMappedMem:=Nil;
  m_nSize:=0;
end;


function TSecureMem.GetSize : Integer;
begin
  Result:=m_nSize;
end;


procedure TSecureMem.SetData(pSource : Pointer;
                             nPos : Integer;
                             nNumOfBytes : Integer);
var
  pDest : PWORD8Buf;
begin
  pDest:=m_pMappedMem;
  Move(pSource^, pDest^[nPos], nNumOfBytes);
end;


procedure TSecureMem.GetData(pTarget : Pointer;
                             nPos : Integer;
                             nNumOfBytes : Integer);
var
  pSrc : PWORD8Buf;
begin
  pSrc:=m_pMappedMem;
  Move(pSrc^[nPos], pTarget^, nNumOfBytes);
end;


procedure TSecureMem.Clear;
begin
  FillChar(m_pMappedMem^, m_nSize, 0);
end;

procedure TSecureMem.SetConfirmed(blState : Boolean);
begin
  m_blConfirmed:=blState;
end;


function TSecureMem.GetConfirmed : Boolean;
begin
  Result:=m_blConfirmed;
end;



//////////////////////////// TKeyMemory ////////////////////////////


function TKeyMemory.GetPtr : Pointer;
begin
  Result:=m_pMappedMem;
end;


function TKeyMemory.Clone : TKeyMemory;
begin
  Result:=TKeyMemory.Create(Self.GetSize);
  Result.SetData(GetPtr, 0, Self.GetSize);
  Result.m_blConfirmed:=m_blConfirmed;
end;



//////////////////////////// TEncryptedKeyMemory ////////////////////////////


constructor TEncryptedKeyMemory.Create(nAmount : Integer;
                                       rs : TRandomSource);
begin
  inherited Create(nAmount);

  // allocate the key buffer on the program heap
  GetMem(m_pKeyMem, nAmount);

  // fill the buffer with random data (like a one-time-pad)
  rs.GetBytes(m_pKeyMem, nAmount);
end;



destructor TEncryptedKeyMemory.Destroy;
begin
  // clear the "one-time-pad"
  FillChar(m_pKeyMem^, m_nSize, 0);

  // free the key buffer
  FreeMem(m_pKeyMem);

  inherited Destroy;
end;



procedure TEncryptedKeyMemory.SetData(pSource : Pointer;
                                      nPos : Integer;
                                      nNumOfBytes : Integer);
var
  nI      : Integer;
  pSrcBuf : PWORD8Buf;
  pDstBuf : PWORD8Buf;
  pKeyBuf : PWORD8Buf;
begin

  // we have to encrypt all the incoming data byte after byte
  pSrcBuf:=pSource;
  pDstBuf:=m_pMappedMem;
  pKeyBuf:=m_pKeyMem;

  // we have to encrypt all the incoming data byte after byte
  nI:=0;
  while (nI < nNumOfBytes) do begin
    pDstBuf^[nPos]:=pKeyBuf^[nPos] xor pSrcBuf^[nI];
    Inc(nI);
    Inc(nPos);
  end;
end;


procedure TEncryptedKeyMemory.GetData(pTarget : Pointer;
                                      nPos : Integer;
                                      nNumOfBytes : Integer);
var
  nI      : Integer;
  pSrcBuf : PWORD8Buf;
  pDstBuf : PWORD8Buf;
  pKeyBuf : PWORD8Buf;
begin

  // we have to encrypt all the incoming data byte after byte
  pSrcBuf:=m_pMappedMem;
  pDstBuf:=pTarget;
  pKeyBuf:=m_pKeyMem;

  // we have to decrypt all the outgoing data byte after byte
  nI:=0;
  while (nI < nNumOfBytes) do begin
    pDstBuf^[nI]:=pKeyBuf^[nPos] xor pSrcBuf^[nPos];
    Inc(nI);
    Inc(nPos);
  end;
end;


procedure TEncryptedKeyMemory.Clear;
begin

  // by just copying the key material we'll get
  // encrypted zero bytes (cool, huh? :)
  Move(m_pKeyMem^, m_pMappedMem^, m_nSize);
end;


function TEncryptedKeyMemory.Clone(rs : TRandomSource) : TEncryptedKeyMemory;
var
  nI         : Integer;
  nSize      : Integer;
  pSrcBuf    : PWORD8Buf;
  pDstBuf    : PWORD8Buf;
  pSrcKeyBuf : PWORD8Buf;
  pDstKeyBuf : PWORD8Buf;
begin
  nSize:=GetSize;
  Result:=TEncryptedKeyMemory.Create(nSize, rs);

  // we must clone the data bytewise
  nI:=0;
  pSrcBuf:=PWORD8Buf(m_pMappedMem);
  pDstBuf:=PWORD8Buf(Result.m_pMappedMem);
  pSrcKeyBuf:=PWORD8Buf(m_pKeyMem);
  pDstKeyBuf:=PWORD8Buf(Result.m_pMappedMem);
  while (nI < nSize) do begin
    // (decrypt and reencrypt)
    pDstBuf^[nI]:=(pSrcBuf^[nI] xor pSrcKeyBuf^[nI]) xor pDstKeyBuf^[nI];
    Inc(nI);
  end;

  // copy the confirmation flag directly
  Result.m_blConfirmed:=GetConfirmed;
end;


function TEncryptedKeyMemory.CloneAsKeyMemory : TKeyMemory;
begin
  Result:=TKeyMemory.Create(GetSize);
  GetData(Result.GetPtr, 0,  Result.GetSize);
  Result.m_blConfirmed:=GetConfirmed;
end;


end.




