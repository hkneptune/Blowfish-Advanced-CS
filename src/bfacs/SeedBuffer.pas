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
  buffer for collecting seed bytes with auto-flushing
}

unit SeedBuffer;

interface
uses bfacslib, RandomSource;

// default size of the (internal buffer)
const SEEDBUFFER_DEFBUFSIZE = 2048;


type
  TSeedBuffer = class
  private
    // members
    m_rs              : TRandomSource;
    m_pBuf            : PWORD8Buf;
    m_nBufSize        : Integer;
    m_nBytesInBuf     : Integer;
    m_blAutoFlush     : Boolean;
    m_qTotalSeedBytes : WORD64;

  public
    // constructor
    // -> random source to link the buffer with
    // -> size of the (internal) seed buffer
    // exception:
    // EOutOfMemory - if out of memory
    constructor Create(setrs : TRandomSource;
                       nSetBufSize : Integer = SEEDBUFFER_DEFBUFSIZE);

    // destructor
    destructor Destroy; override;

    // add seed data to the buffer
    // -> pointer where the seed data is located
    // -> number of seed bytes
    // <- number of seed bytes used
    function AddSeed(pDataBuf : Pointer;
                     nDataLen : Integer) : Integer;

    // to flush the current buffer content manually
    procedure Flush;

    // to set the auto-flush flag
    // -> new state of the auto-flush flag
    procedure SetAutoFlush(blNewState : Boolean);

    // to read the auto-flush flag
    // <- state of the auto-flush flag
    function GetAutoFlush : Boolean;

    // to read the current amount of seed bytes in the buffer
    // <- bytes in the seed buffer
    function GetBytesInBuf : Integer;

    // to get the total number of bytes which were ever passed to the seed
    // buffer since its whole existance
    // <- total bytes passed
    function GetTotalSeedBytes : WORD64;

    // changes the random source
    // -> new random source
    procedure SetRandomSource(rs : TRandomSource);
  end;



implementation


constructor TSeedBuffer.Create(setrs : TRandomSource;
                               nSetBufSize : Integer = SEEDBUFFER_DEFBUFSIZE);
begin
  // allocate the seed buffer
  m_pBuf:=Nil;
  GetMem(m_pBuf, nSetBufSize);

  // setup the members
  m_nBufSize:=nSetBufSize;
  m_nBytesInBuf:=0;
  m_rs:=setrs;
  m_blAutoFlush:=True;
  m_qTotalSeedBytes:=0;
end;


destructor TSeedBuffer.Destroy;
begin
  // just clear and free the buffer
  if (m_pBuf <> Nil) then begin
    FillChar(m_pBuf^, m_nBufSize, 0);
    FreeMem(m_pBuf);
  end;
end;


function TSeedBuffer.AddSeed(pDataBuf : Pointer; nDataLen : Integer) : Integer;
var
  nSpaceLeft   : Integer;
  nDataPos     : Integer;
  pDataByteBuf : PWORD8Buf;
begin
  // what's the left space in the buffer?
  nSpaceLeft:=m_nBufSize - m_nBytesInBuf;

  // are we able to process always every new data amount?
  if (m_blAutoFlush) then begin

    // first add to the total amount
    Inc(m_qTotalSeedBytes, nDataLen);

    // copy (and reseed i.n.) until all bytes have been catched
    pDataByteBuf:=pDataBuf;
    nDataPos:=0;
    // copy now
    while (nDataLen <> 0) do begin
      // still more data than space in buffer?
      if (nDataLen > nSpaceLeft) then begin
        // yes, fill the buffer
        Move(pDataByteBuf^[nDataPos], m_pBuf^[m_nBytesInBuf], nSpaceLeft);
        Inc(nDataPos, nSpaceLeft);
        Dec(nDataLen, nSpaceLeft);
        // reseed
        m_rs.Reseed(m_pBuf, m_nBufSize);
        // nothing in the buffer now
        nSpaceLeft:=m_nBufSize;
        m_nBytesInBuf:=0;
      end
      else begin
        // no, just copy the rest into the buffer
        Move(pDataByteBuf^[nDataPos], m_pBuf^[m_nBytesInBuf], nDataLen);
        Inc(m_nBytesInBuf, nDataLen);
        nDataLen:=0;
      end;
    end;
    Result:=0;
  end
  else begin
    // can we copy all the given bytes?
    if (nDataLen > nSpaceLeft) then begin
      // no, so get as much of the data as possible into the buffer
      Move(pDataBuf^, m_pBuf^[m_nBytesInBuf], nSpaceLeft);
      Inc(m_nBytesInBuf, nSpaceLeft);
      Inc(m_qTotalSeedBytes, nSpaceLeft);
      Result:=nSpaceLeft;
    end
    else begin
      // yes, copy all the bytes now
      Move(pDataBuf^, m_pBuf^[m_nBytesInBuf], nDataLen);
      Inc(m_nBytesInBuf, nDataLen);
      Inc(m_qTotalSeedBytes, nDataLen);
      Result:=nDataLen;
    end;
  end;
end;


procedure TSeedBuffer.Flush;
begin
  // something to flush?
  if (m_nBytesInBuf > 0) then begin
    // reseed
    m_rs.Reseed(m_pBuf, m_nBufSize);
    // nothing in the buffer now
    m_nBytesInBuf:=0;
  end;
end;


procedure TSeedBuffer.SetAutoFlush(blNewState : Boolean);
begin
  m_blAutoFlush:=blNewState;
end;


function TSeedBuffer.GetAutoFlush : Boolean;
begin
  Result:=m_blAutoFlush;
end;


function TSeedBuffer.GetBytesInBuf : Integer;
begin
  Result:=m_nBytesInBuf;
end;


function TSeedBuffer.GetTotalSeedBytes : WORD64;
begin
  Result:=m_qTotalSeedBytes;
end;


procedure TSeedBuffer.SetRandomSource(rs : TRandomSource);
begin
  m_rs:=rs;
end;


end.
