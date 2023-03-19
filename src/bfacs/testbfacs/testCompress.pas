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

unit testCompress;

interface

function TestCompression: Boolean;

implementation
uses SysUtils, StringPlus, Compress, LZSS, ZLibEx, bfacslib;

const
  SIZES : array[0..6] of Integer = ( 0, 1, 8, 17, 256, 65536, 1025 * 1024 );

function TestCompression: Boolean;
const
  CYCLES = 16;
var
  blAgain : Boolean;
  nI, nC : Integer;
  nType, nSizeIdx, nFillStyle, nSize, nTotal, nPerCycle, nWritten : Integer;
  cmp : TCompressor;
  dcmp : TDecompressor;
  buf, buf2, buf3 : PChar;
begin

    Result := False;

    cmp := Nil;     // (jtptc)
    dcmp := Nil;

    for nType := 0 to 2 do begin
    for nSizeIdx := 0 to 6 do begin
    for nFillStyle := 0 to 3 do begin

        nSize := SIZES[nSizeIdx];
    
        Write(
            'type: ', IntToStr(nType),
            ', size: ', IntToStr(nSize),
            ', fillstyle: ', IntToStr(nFillStyle));

        nPerCycle := nSize div CYCLES;
        if (0 = nPerCycle) then nPerCycle := 1;

        case nType of
            0: begin
                cmp := TLZSSCompressor.Create;
                dcmp := TLZSSDecompressor.Create;
            end;
            1 : begin
                cmp := TZLibExCompressor.Create(ZLIBEX_TYPE_DEFLATE, -1);
                dcmp := TZLibExDecompressor.Create(ZLIBEX_TYPE_DEFLATE);
            end;
            2 : begin
                cmp := TZLibExCompressor.Create(ZLIBEX_TYPE_BZIP2, -1);
                dcmp := TZLibExDecompressor.Create(ZLIBEX_TYPE_BZIP2);
            end;
        end;

        try
            nC := nSize; if (0 = nC) then nC := 1;  // (prevents Nil pointers)
            GetMem(buf, nC);
            GetMem(buf2, (nC shl 1) + 1024);   // overkill, just to be safe
            GetMem(buf3, nC);

            case nFillStyle of
                0 : FillChar(buf^, nSize, 0);
                1 : FillChar(buf^, nSize, 1);
                2 : for nI := 0 to nSize - 1 do buf[nI] := Char(nI);
                3 : for nI := 0 to nSize - 1 do buf[nI] := Char(Random(256));
            end;            

            nTotal := 0;

            repeat
                blAgain := cmp.ProcessData(
                    buf,
                    @buf2[nTotal],
                    nSize,
                    nPerCycle,
                    nWritten);

                Inc(nTotal, nWritten);
            until not blAgain;

            repeat
                blAgain := cmp.Finalize(
                    @buf2[nTotal],
                    nPerCycle,
                    nWritten);

                Inc(nTotal, nWritten);
            until not blAgain;

            Write(', compressed: ', IntToStr(nTotal));

            //WriteLn(#13#10 + TStrPlus.HexDump(buf2, nTotal));

            nSize := nTotal;
            nTotal := 0;

            repeat
                blAgain := dcmp.ProcessData(
                    buf2,
                    @buf3[nTotal],
                    nSize,
                    nPerCycle,
                    nWritten);
                if (-1 = nWritten) then Break;
                Inc(nTotal, nWritten);
            until not blAgain;

            if (-1 <> nWritten) then begin
                repeat
                    blAgain := dcmp.Finalize(
                        @buf3[nTotal],
                        nPerCycle,
                        nWritten);
                    if (-1 = nWritten) then Break;
                    Inc(nTotal, nWritten);
                until not blAgain;
            end;   

            Write(', decompressed: ', IntToStr(nTotal));

            nSize := SIZES[nSizeIdx];

            if (not CompareMem(buf, buf3, nSize)) then begin
                WriteLn(#13#10, ' DATA MISMATCH!');
                Exit;
            end;

            FreeMem(buf);
            FreeMem(buf2);
            FreeMem(buf3);

        except
            on e : ECompressError do begin
                WriteLn(#13#10, e.Message);
                Exit;
            end;
        end;

        cmp.Destroy;
        dcmp.Destroy;

        WriteLn('');

    end; // of "for nFillStyle..."
    end; // of "for nSizeIdx..."
    end; // of "for nType..."

    Result := True;
end;

end.
