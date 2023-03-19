/*
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
 */

#include <stdlib.h>

#include <zlib.h>
#include <bzlib.h>

#include "zlibex.h"

//////////////////////////////////////////////////////////////////////////////

// this large output size is necessary since zlib can indeed flush out lots
// of data suddenly during compression cycles...
#define MIN_OUTP_SIZE	(66 * 1024)

//////////////////////////////////////////////////////////////////////////////

struct ZLIBEXCTX
{
	union 
	{	
		z_stream zs; 
		bz_stream bs; 
	} 
	stm;
	int mode;
	int type;
	char blFinalized;
	char blRepeat;
	int nLastError;
};

//////////////////////////////////////////////////////////////////////////////

int BFACSLIB_CALLCONV ZLibEx_Create(
	int mode,
	int type,
	int nTotalSize,
	PZLIBEXCTX* pCtx)
{
	PZLIBEXCTX ctx;
	int nBlkSz100k;

	if (NULL == pCtx)
	{
		return BFACSLIB_ERR_NULLPTR;
	}

	if ((ZLIBEX_MODE_COMPRESS != mode &&
	     ZLIBEX_MODE_DECOMPRESS != mode) ||
	    (ZLIBEX_TYPE_DEFLATE != type &&
	     ZLIBEX_TYPE_BZIP2 != type))
	{
		return BFACSLIB_ERR_WRONGPARAM;
	}

	if (NULL == (ctx = (PZLIBEXCTX)calloc(1, sizeof(*ctx))))
	{
		return BFACSLIB_ERR_OUTOFMEMORY;
	}

	if (ZLIBEX_TYPE_DEFLATE == type)
	{
		ctx->nLastError = (ZLIBEX_MODE_COMPRESS == mode) ?
			deflateInit(&ctx->stm.zs, Z_BEST_COMPRESSION) :
			inflateInit(&ctx->stm.zs);;

		if (Z_OK != ctx->nLastError)
		{
			free(ctx);
			return BFACSLIB_ERR_ZLIBEX;
		}
	}
	else	// ZLIBEX_TYPE_BZIP2
	{
		if (ZLIBEX_MODE_COMPRESS == mode)
		{
			if (-1 == nTotalSize)
			{
				nBlkSz100k = 9;
			}
			else
			{
				nBlkSz100k = nTotalSize / 100000;
					if (1 > nBlkSz100k) nBlkSz100k = 1;
				else if (9 < nBlkSz100k) nBlkSz100k = 9;
			}
		
			ctx->nLastError = BZ2_bzCompressInit(
				&ctx->stm.bs, nBlkSz100k, 0, 0);
		}
		else
		{
			ctx->nLastError = BZ2_bzDecompressInit(&ctx->stm.bs, 0, 0);
		}

		if (BZ_OK != ctx->nLastError)
		{
			free(ctx);
			return BFACSLIB_ERR_ZLIBEX;
		}
	}

	ctx->type = type;
	ctx->mode = mode;

	*pCtx = ctx;

	return BFACSLIB_ERR_NOERROR;
}

//////////////////////////////////////////////////////////////////////////////

int BFACSLIB_CALLCONV ZLibEx_Destroy(
	PZLIBEXCTX ctx)
{
	int blCompress;

	if (NULL == ctx)
	{
		return BFACSLIB_ERR_NULLPTR;
	}
	
	blCompress = (ZLIBEX_MODE_COMPRESS == ctx->mode);

	if (ZLIBEX_TYPE_DEFLATE == ctx->type) 
	{	
		if (blCompress) deflateEnd(&ctx->stm.zs);
		else			inflateEnd(&ctx->stm.zs);
	}
	else	// ZLIBEX_TYPE_BZIP2
	{
		if (blCompress) BZ2_bzCompressEnd(&ctx->stm.bs);
		else            BZ2_bzDecompressEnd(&ctx->stm.bs);
	}

	free(ctx);

	return BFACSLIB_ERR_NOERROR;
}

//////////////////////////////////////////////////////////////////////////////

int BFACSLIB_CALLCONV ZLibEx_Process(
	PZLIBEXCTX ctx,
	const char* inp, 
	int nInpSize,
	char* outp,
	int nOutpSize,
	int* pnWritten)
{
	int nErr, nFlush, nResult;
	z_streamp zsp;
	bz_stream* bsp;

	if (NULL == ctx || NULL == outp || NULL == pnWritten)
	{
		return BFACSLIB_ERR_NULLPTR;
	}

	if (ctx->blFinalized)
	{
		*pnWritten = 0;
		return BFACSLIB_ERR_NOERROR;
	}

	if (ZLIBEX_TYPE_DEFLATE == ctx->type)
	{
		if (NULL == inp && 0 == nInpSize)
		{
			inp = "";
			nFlush = Z_FINISH;
		}
		else
		{
			nFlush = Z_NO_FLUSH;
		}

		zsp = &ctx->stm.zs;

		if (!ctx->blRepeat)
		{
			zsp->next_in = (Bytef*)inp;
			zsp->avail_in = (uInt)nInpSize;
		}
		zsp->next_out = (Bytef*)outp;
		zsp->avail_out = (uInt)nOutpSize;

		if (ZLIBEX_MODE_COMPRESS == ctx->mode)
		{
			if (Z_OK != (nErr = deflate(zsp, nFlush)))
			{
				if (Z_FINISH == nFlush && Z_STREAM_END == nErr)
				{
					ctx->blFinalized = 1;
				}
				else
				{
					ctx->nLastError = nErr;
					return BFACSLIB_ERR_ZLIBEX;
				}
			}
		}
		else
		{
			if (Z_OK != (nErr = inflate(zsp, nFlush)))
			{
				if (Z_STREAM_END == nErr)
				{
					ctx->blFinalized = 1;
				}
				else
				{
					ctx->nLastError = nErr;
					return BFACSLIB_ERR_ZLIBEX;
				}
			}
		}

		if (zsp->avail_in || (Z_FINISH == nFlush && Z_OK == nErr))
		{
			ctx->blRepeat = 1;
			nResult = BFACSLIB_ERR_ZLIBEX_REPEAT;
		}
		else
		{
			ctx->blRepeat = 0;
			nResult = BFACSLIB_ERR_NOERROR;
		}

		*pnWritten = nOutpSize - (int)zsp->avail_out;
		if (ctx->blFinalized && 
			0 == *pnWritten && 
			ZLIBEX_MODE_DECOMPRESS == ctx->mode)
		{	
			*pnWritten = -1;
		}
	}
	else	// ZLIBEX_TYPE_BZIP2
	{
		if (NULL == inp && 0 == nInpSize)
		{
			inp = "";
			nFlush = BZ_FINISH;
		}
		else
		{
			if (0 == nInpSize)
			{
				*pnWritten = 0;
				return BFACSLIB_ERR_NOERROR;
			}
			nFlush = BZ_RUN;
		}

		bsp = &ctx->stm.bs;

		if (!ctx->blRepeat)
		{
			bsp->next_in = (Bytef*)inp;
			bsp->avail_in = (uInt)nInpSize;
		}

		bsp->next_out = (Bytef*)outp;
		bsp->avail_out = (uInt)nOutpSize;

		if (ZLIBEX_MODE_COMPRESS == ctx->mode)
		{
			switch (nErr = BZ2_bzCompress(bsp, nFlush))
			{
				case BZ_RUN_OK:
				case BZ_FINISH_OK: 
					break;
				case BZ_STREAM_END: 
					ctx->blFinalized = 1;
					break;
				default:
					ctx->nLastError = nErr;
					return BFACSLIB_ERR_ZLIBEX;
			}	
		}
		else
		{
			switch (nErr = BZ2_bzDecompress(bsp))
			{
				case BZ_OK:
					break;
				case BZ_STREAM_END: 
					ctx->blFinalized = 1;
					break;
				default:
					ctx->nLastError = nErr;
					return BFACSLIB_ERR_ZLIBEX;
			}	
		}

		if (bsp->avail_in || BZ_FINISH_OK == nErr)
		{
			ctx->blRepeat = 1;
			nResult = BFACSLIB_ERR_ZLIBEX_REPEAT;
		}
		else
		{
			ctx->blRepeat = 0;
			nResult = BFACSLIB_ERR_NOERROR;
		}

		*pnWritten = nOutpSize - (int)bsp->avail_out;
	}

	return nResult;
}

//////////////////////////////////////////////////////////////////////////////

int BFACSLIB_CALLCONV ZLibEx_Finalize(
	PZLIBEXCTX ctx, 
	char* outp, 
	int nOutpSize, 
	int* pnWritten)
{
	return ZLibEx_Process(
		ctx, 
		NULL, 
		0, 
		outp, 
		nOutpSize, 
		pnWritten);
}

//////////////////////////////////////////////////////////////////////////////

int BFACSLIB_API ZLibEx_GetLastError(
	PZLIBEXCTX ctx, 
	int* pnLastError)
{
	if (NULL == ctx || NULL == pnLastError)
	{
		return BFACSLIB_ERR_NULLPTR;
	}
	else
	{
		*pnLastError = ctx->nLastError;
		return BFACSLIB_ERR_NOERROR;
	}
}
