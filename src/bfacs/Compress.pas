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
  interfaces for (de)compressors
}

unit Compress;

{$I config.inc}

interface
uses SysUtils;

type
  ECompressError = class(Exception);

///////////////////////////////////////////////////////////////////////////////

// base class for compressors
type
  TCompressor = class
  public

    // Compresses data from an input to an output buffer, must be repeated
    // until the compressor has done its job.
    // -> pointer to the source buffer
    // -> pointer to the target buffer
    // -> number of bytes to compress
    // -> size of the target buffer
    // -> where to put the number of compressed bytes
    // <- True: more data to come, repeat the call / False: cycle done
    // exception:
    // ECompressError - if a compression error occured
    function ProcessData(pSource : Pointer;
                         pTarget : Pointer;
                         nNumOfBytes : Integer;
                         nTargetBufSize : Integer;
                         var vnCompressed : Integer) : Boolean;
                            virtual; abstract;

    // Finishes a compressions session and gets the rest of the compressed data,
    // must be repeated until all the data has been consumed.
    // -> target buffer
    // -> size of target buffer
    // -> where to put the number of compressed bytes
    // <- True: all done / False: more data to come, repeat the call
    // exception:
    // ECompressError - if a compression error occured
    function Finalize(pTarget : Pointer;
                      nTargetBufSize : Integer;
                      var vnCompressed : Integer) : Boolean; virtual; abstract;
  end;

///////////////////////////////////////////////////////////////////////////////

// base class for decompressors
type
  TDecompressor = class
  public

    // Decompresses data, must be repeated (and the target buffer flushed) as
    // long as the result is True.
    // -> pointer to the source buffer
    // -> pointer to the target buffer
    // -> number of bytes to decompress
    // -> size of the target buffer
    // -> where to put the number of decompressed bytes; -1 signals that the
    //    logical compression stream has ended, for which a call to Finalize()
    //    is then not necessary anymore
    // <- True: method must be called again to get more decompressed data /
    //    False: all compressed data is inflated now
    // exception:
    // ECompressError - if a compression error occured
    function ProcessData(pSource : Pointer;
                         pTarget : Pointer;
                         nNumOfBytes : Integer;
                         nTargetBufSize : Integer;
                         var vnDecompressed : Integer) : Boolean;
                            virtual; abstract;

    // Finishes a decompression session and gets the rest of the decompressed
    // data, must be repeated (and the target buffer flushed) as long as the
    // result is True.
    // -> pointer to the target buffer
    // -> size of the target buffer
    // -> where to put the number of decompressed bytes
    // <- True: method must be called again to get more decompressed data /
    //    False: all the rest of compressed data is inflated now
    // exception:
    // ECompressError - if a compression error occured
    function Finalize(pTarget : Pointer;
                      nTargetBufSize : Integer;
                      var vnDecompressed : Integer) : Boolean;
                        virtual; abstract;
  end;

  
implementation

// no implementation right at this particular moment

end.
