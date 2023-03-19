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
  support classes implementing the LZSS algorithm through bfacslib.dll
}

unit LZSS;

{$I config.inc}

interface
uses bfacslib, Compress;


// recommeded buffer sizes
const
  LZSS_SRCBUFSIZE = 32 * 1024; // 32 kB source buffer
  LZSS_DSTBUFSIZE = 48 * 1024; // 48 kB target buffer


// class for LZSS compression
type
  TLZSSCompressor = class(TCompressor)
  private
    // the LZSS context
    m_pCtx : PLZSSCTX;
    // the condition code
    m_bCond : WORD8;
    // intermediate buffer and its managment parameters
    m_buf : PChar;
    m_nBufSize : Integer;
    m_nDataPos : Integer;
    m_nDataLen : Integer;
  private
    class function CalcDstBufSize(nSize : Integer) : Integer;
  public
    // init. the compression context
    constructor Create;

    // just cleans the compression context
    destructor Destroy; override;

    function ProcessData(pSource : Pointer;
                         pTarget : Pointer;
                         nNumOfBytes : Integer;
                         nTargetBufSize : Integer;
                         var vnCompressed : Integer) : Boolean; override;

    function Finalize(pTarget : Pointer;
                      nTargetBufSize : Integer;
                      var vnCompressed : Integer) : Boolean; override;
  end;
  

// class for LZSS decompression
type
  TLZSSDecompressor = class(TDecompressor)
  private
    // the LZSS context
    m_pCtx : PLZSSCTX;
    // the condition code
    m_bCond : WORD8;
    // the repetition flag
    m_blRepeat : BYTEBOOL;
  public

    // init. the compression context and the repetition flag
    constructor Create;

    // just cleans the compression context
    destructor Destroy; override;

    function ProcessData(pSource : Pointer;
                         pTarget : Pointer;
                         nNumOfBytes : Integer;
                         nTargetBufSize : Integer;
                         var vnDecompressed : Integer) : Boolean; override;

    function Finalize(pTarget : Pointer;
                      nTargetBufSize : Integer;
                      var vnDecompressed : Integer) : Boolean; override;
  end;



implementation


//////////////////// TLZSSCompressor ////////////////////

class function TLZSSCompressor.CalcDstBufSize(
    nSize : Integer) : Integer;
const
  MIN_SIZE = 1024;
begin

  if (nSize < MIN_SIZE) then
    Result := MIN_SIZE
  else
    Result := nSize + (nSize shr 2);
end;

constructor TLZSSCompressor.Create;
begin
  m_pCtx:=LZSS_Create;
  m_bCond:=LZSS_START or LZSS_WORK;
  LZSS_Compress(m_pCtx, nil, nil, 0, m_bCond);
  m_bCond:=m_bcond and (not LZSS_START);
  m_nBufSize := LZSS_DSTBUFSIZE;
  GetMem(m_buf, m_nBufSize);
  m_nDataPos := 0;
  m_nDataLen := 0;
end;

destructor TLZSSCompressor.Destroy;
begin
  LZSS_Destroy(m_pCtx);
  FreeMem(m_buf);
end;

function TLZSSCompressor.ProcessData(
    pSource : Pointer;
    pTarget : Pointer;
    nNumOfBytes : Integer;
    nTargetBufSize : Integer;
    var vnCompressed : Integer) : Boolean;
var
  nDstBufSize : Integer;
begin

    if (0 < m_nDataLen) then begin

        // feed from the buffer (only on a repeated call, btw)

        if (nTargetBufSize < m_nDataLen) then
            vnCompressed := nTargetBufSize
        else
            vnCompressed := m_nDataLen;

        Move(m_buf[m_nDataPos], pTarget^, vnCompressed);

        Inc(m_nDataPos, vnCompressed);
        Dec(m_nDataLen, vnCompressed);

        // we might have to repeat again
        if (0 < m_nDataLen) then
            Result := True
        else
            Result := False;
    end
    else begin

        // target buffer large enough to fit everything?

        nDstBufSize := CalcDstBufSize(nNumOfBytes);

        if (nTargetBufSize < nDstBufSize) then begin
            // need to use the intermediate buffer (enlarge it, if necessary)
            if (m_nBufSize < nDstBufSize) then begin
                m_nBufSize := nDstBufSize;
                FreeMem(m_buf);
                GetMem(m_buf, m_nBufSize);
            end;
            m_nDataLen:=LZSS_Compress(
                m_pCtx,
                pSource,
                m_buf,
                nNumOfBytes,
                m_bCond);
            // pass along what's possible, keep any rest and signal repeat
            if (nTargetBufSize < m_nDataLen) then begin

                vnCompressed := nTargetBufSize;
                Move(m_buf[0], pTarget^, vnCompressed);

                m_nDataPos := vnCompressed;
                Dec(m_nDataLen, vnCompressed);

                Result := True;
            end
            else begin

                vnCompressed := m_nDataLen;
                Move(m_buf[0], pTarget^, vnCompressed);
                m_nDataLen := 0;

                Result := False;
            end;
        end
        else begin
            // we can perform direct output
            vnCompressed := LZSS_Compress(
                m_pCtx,
                pSource,
                pTarget,
                nNumOfBytes,
                m_bCond);
            Result := False;
        end;
    end;
end;

function TLZSSCompressor.Finalize(
    pTarget : Pointer;
    nTargetBufSize : Integer;
    var vnCompressed : Integer) : Boolean;
begin
    // (some code compaction here since things are quite similar)
    m_bCond := m_bCond or LZSS_STOP;
    Result := ProcessData(
        PChar(''),
        pTarget,
        0,
        nTargetBufSize,
        vnCompressed);
end;


//////////////////// TLZSSDecompressor ////////////////////


constructor TLZSSDecompressor.Create;
begin
  m_pCtx:=LZSS_Create;
  m_bCond:=LZSS_START or LZSS_WORK;
  m_blRepeat:=BOOL_FALSE;  // (won't be altered by the first call)
  LZSS_Decompress(m_pCtx,
                  Nil,
                  Nil,
                  0,
                  0,
                  m_bCond,
                  @m_blRepeat);
  m_bCond:=m_bcond and (not LZSS_START);
end;


destructor TLZSSDecompressor.Destroy;
begin
  LZSS_Destroy(m_pCtx);
  FillChar(m_pCtx, SizeOf(m_pCtx), 0);
end;


function TLZSSDecompressor.ProcessData(pSource : Pointer;
                                       pTarget : Pointer;
                                       nNumOfBytes : Integer;
                                       nTargetBufSize : Integer;
                                       var vnDecompressed : Integer) : Boolean;
begin
  vnDecompressed:=LZSS_Decompress(m_pCtx,
                                  pSource,
                                  pTarget,
                                  nNumOfBytes,
                                  nTargetBufSize,
                                  m_bCond,
                                  @m_blRepeat);
  if (m_blRepeat = BOOL_TRUE) then
    Result:=True
  else begin
    // it's absolutely necessary to reset the flag
    m_blRepeat:=BOOL_FALSE;
    Result:=False;
  end;
end;


function TLZSSDecompressor.Finalize(pTarget : Pointer;
                                    nTargetBufSize : Integer;
                                    var vnDecompressed : Integer) : Boolean;
begin
  vnDecompressed:=LZSS_Decompress(m_pCtx,
                                  Nil,
                                  pTarget,
                                  0,
                                  nTargetBufSize,
                                  m_bCond,
                                  @m_blRepeat);
  if (m_blRepeat = BOOL_TRUE) then
    Result:=True
  else begin
    // (see above)
    m_blRepeat:=BOOL_FALSE;
    Result:=False;
  end;
end;

end.
