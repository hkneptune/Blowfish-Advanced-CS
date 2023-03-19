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

unit ZLibEx;

{$I config.inc}

interface
uses bfacslib, Compress;

///////////////////////////////////////////////////////////////////////////////

type
  TZLibExCompressor = class(TCompressor)
  private
    m_pCtx : PZLIBEXCTX;
  public

    constructor Create(nType : Integer; nDataSize : Integer = -1);
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


type
  TZLibExDecompressor = class(TDecompressor)
  private
    m_pCtx : PZLIBEXCTX;
  public

    constructor Create(nType : Integer);
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
uses SysUtils;

///////////////////////////////////////////////////////////////////////////////

function _ErrToStr(nErr : Integer) : String;
begin
    case nErr of
        BFACSLIB_ERR_NOERROR      : Result:='BFACSLIB_ERR_NOERROR';
        BFACSLIB_ERR_ERROR        : Result:='BFACSLIB_ERR_ERROR';
        BFACSLIB_ERR_NULLPTR      : Result:='BFACSLIB_ERR_NULLPTR';
        BFACSLIB_ERR_WRONGPARAM   : Result:='BFACSLIB_ERR_WRONGPARAM';
        BFACSLIB_ERR_NOIMPL       : Result:='BFACSLIB_ERR_NOIMPL';
        BFACSLIB_ERR_OUTOFMEMORY  : Result:='BFACSLIB_ERR_OUTOFMEMORY';
        BFACSLIB_ERR_INTERNAL     : Result:='BFACSLIB_ERR_INTERNAL';
        BFACSLIB_ERR_ZLIBEX       : Result:='BFACSLIB_ERR_ZLIBEX';
        BFACSLIB_ERR_ZLIBEX_REPEAT: Result:='BFACSLIB_ERR_ZLIBEX_REPEAT';

    else    // BFACSLIB_ERR_UNKNOWN
        Result:='unknown error ' + IntToStr(nErr);
    end;
end;

function _ErrToStr2(nErr : Integer; ctx : PZLIBEXCTX) : String;
begin
    Result:=_ErrToStr(nErr);

    if (BFACSLIB_ERR_ZLIBEX = nErr) then begin
        ZLibEx_GetLastError(ctx, nErr);
        Result:=Result + ' (' + IntToStr(nErr) + ')'
    end;
end;

///////////////////////////////////////////////////////////////////////////////

constructor TZLibExCompressor.Create(
  nType : Integer;
  nDataSize : Integer);
var
  nErr : Integer;
begin

  nErr:=ZLibEx_Create(ZLIBEX_MODE_COMPRESS, nType, nDataSize, m_pCtx);

  if (BFACSLIB_ERR_NOERROR <> nErr) then begin
    raise ECompressError.Create(_ErrToStr(nErr));
  end;
end;

destructor TZLibExCompressor.Destroy;
var
  nErr : Integer;
begin
  nErr:=ZLibEx_Destroy(m_pCtx);

  if (BFACSLIB_ERR_NOERROR <> nErr) then begin
    raise ECompressError.Create(_ErrToStr(nErr));
  end;
end;

function TZLibExCompressor.ProcessData(
    pSource : Pointer;
    pTarget : Pointer;
    nNumOfBytes : Integer;
    nTargetBufSize : Integer;
    var vnCompressed : Integer) : Boolean;
var
  nErr : Integer;
begin

  // (just to be sure: check for end of data writes)
  if (0 = nNumOfBytes) then begin
    pSource:=Nil;
  end;

  nErr:=ZLibEx_Process(
    m_pCtx,
    pSource,
    nNumOfBytes,
    pTarget,
    nTargetBufSize,
    vnCompressed);

  case nErr of
    BFACSLIB_ERR_NOERROR : Result:=False;
    BFACSLIB_ERR_ZLIBEX_REPEAT : Result:=True;
  else
    raise ECompressError.Create(_ErrToStr2(nErr, m_pCtx));
  end;
end;

function TZLibExCompressor.Finalize(
    pTarget : Pointer;
    nTargetBufSize : Integer;
    var vnCompressed : Integer) : Boolean;
var
  nErr : Integer;
begin

  nErr:=ZLibEx_Finalize(
    m_pCtx,
    pTarget,
    nTargetBufSize,
    vnCompressed);

  case nErr of
    BFACSLIB_ERR_NOERROR : Result:=False;
    BFACSLIB_ERR_ZLIBEX_REPEAT : Result:=True;
  else
    raise ECompressError.Create(_ErrToStr2(nErr, m_pCtx));
  end;
end;

///////////////////////////////////////////////////////////////////////////////

constructor TZLibExDecompressor.Create(
  nType : Integer);
var
  nErr : Integer;
begin

  nErr:=ZLibEx_Create(ZLIBEX_MODE_DECOMPRESS, nType, -1, m_pCtx);

  if (BFACSLIB_ERR_NOERROR <> nErr) then begin
    raise ECompressError.Create(_ErrToStr(nErr));
  end;
end;

destructor TZLibExDecompressor.Destroy;
var
  nErr : Integer;
begin
  nErr:=ZLibEx_Destroy(m_pCtx);

  if (BFACSLIB_ERR_NOERROR <> nErr) then begin
    raise ECompressError.Create(_ErrToStr(nErr));
  end;
end;

function TZLibExDecompressor.ProcessData(
    pSource : Pointer;
    pTarget : Pointer;
    nNumOfBytes : Integer;
    nTargetBufSize : Integer;
    var vnDecompressed : Integer) : Boolean;
var
  nErr : Integer;
begin

  nErr:=ZLibEx_Process(
    m_pCtx,
    pSource,
    nNumOfBytes,
    pTarget,
    nTargetBufSize,
    vnDecompressed);

  case nErr of
    BFACSLIB_ERR_NOERROR : Result:=False;
    BFACSLIB_ERR_ZLIBEX_REPEAT : Result:=True;
  else
    raise ECompressError.Create(_ErrToStr2(nErr, m_pCtx));
  end;
end;

function TZLibExDecompressor.Finalize(
    pTarget : Pointer;
    nTargetBufSize : Integer;
    var vnDecompressed : Integer) : Boolean;
var
  nErr : Integer;
begin

  nErr:=ZLibEx_Finalize(
    m_pCtx,
    pTarget,
    nTargetBufSize,
    vnDecompressed);

  case nErr of
    BFACSLIB_ERR_NOERROR : Result:=False;
    BFACSLIB_ERR_ZLIBEX_REPEAT : Result:=True;
  else
    raise ECompressError.Create(_ErrToStr2(nErr, m_pCtx));
  end;
end;


end.
