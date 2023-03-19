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

#include <windows.h>

#include <stdio.h>
#include <string.h>

#include "TestInterface.h"
#include "TestUtils.h"
#include "zlibex.h"

//////////////////////////////////////////////////////////////////////////////

#define DEF_FILL_BUF_SIZE	(256 * 1024)
#define MIN_BUF_SIZE		1

//////////////////////////////////////////////////////////////////////////////

static int FILE_SIZES[] = { 
	0, 1, 3, 16, 256, 1024, 65536, 1024 * 1024, 7654321 };

static int COMP_TYPES[] = { 
	ZLIBEX_TYPE_DEFLATE , ZLIBEX_TYPE_BZIP2  };

//////////////////////////////////////////////////////////////////////////////

bool TestZLibEx(
	CTestStdOut* tso) 
{
#define ERROR_EXIT					\
	if (inf) ::fclose(inf);			\
	if (outf) ::fclose(outf);		\
	if (ctx) ::ZLibEx_Destroy(ctx);	\
	::free(buf);					\
	return false;

#define NEW_BUFFER_SIZE								\
	nBSize = ((nBSize << 1) + ((nBSize & 1) ^ 1));	\
	nBSize %= DEF_FILL_BUF_SIZE;					\
	if (0 == nBSize) nBSize = MIN_BUF_SIZE;


	int nI, nCompIdx, nSizeIdx, nFileSize, nFillStyle, nNamePos;
	int nBSize, nRead, nTmp, nOutSize, nErr, nCompType;
	char *buf, *buf2;
	char flOrig[MAX_PATH];
	char flDefl[MAX_PATH];
	char flInfl[MAX_PATH];
	FILE *inf= NULL, *outf= NULL;
	PZLIBEXCTX ctx = NULL;


	::srand(0x12345678);

	if (NULL == (buf = (char*)::malloc(DEF_FILL_BUF_SIZE << 1)))
	{
		::puts("cannot allocate main buffers");
		ERROR_EXIT
	}
	else
	{
		buf2 = buf + DEF_FILL_BUF_SIZE;
	}

	::GetTempPath(sizeof(flOrig), flOrig);
	::strcpy(flDefl, flOrig);
	::strcpy(flInfl, flOrig);
	nNamePos = (int)::strlen(flOrig);

	// all sizes, all fill styles

	for (nCompIdx = 0; nCompIdx < sizeof(COMP_TYPES) / sizeof(int); nCompIdx++)	{
	for (nSizeIdx = 0; nSizeIdx < sizeof(FILE_SIZES) / sizeof(int); nSizeIdx++)	{
	for (nFillStyle = 0; nFillStyle < FILLBUF_NUMOFSTYLES; nFillStyle++)
	{
		nFileSize = FILE_SIZES[nSizeIdx];
		nCompType = COMP_TYPES[nCompIdx];

		::printf("type: %d, size: %d, fillstyle: %d", nCompType, nFileSize, nFillStyle);

		// create the original file

		::sprintf(&flOrig[nNamePos], 
			"zlibtest_c%d_s%d_f%d.orig", nCompType, nFileSize, nFillStyle);

		if (NULL == (outf = ::fopen(flOrig, "wb")))
		{
			::printf("\ncannot open original file \"%s\"\n", flOrig);
			ERROR_EXIT
		}

		for (nI = 0; nI < nFileSize; nI += nTmp)
		{	
			::FillBuffer(buf, DEF_FILL_BUF_SIZE, nFillStyle);

			nTmp = (nFileSize - nI);
			if (nTmp > DEF_FILL_BUF_SIZE)
			{
				nTmp = DEF_FILL_BUF_SIZE;
			}

			if (nTmp != (int)::fwrite(buf, 1, nTmp, outf))
			{
				::puts("\nerror writing to original file");
				ERROR_EXIT
			}
		}
		::fclose(outf); outf = NULL;

		// now compress the file

		if (NULL == (inf = ::fopen(flOrig, "rb")))
		{
			::printf("\ncannot open file for compression\"%s\"\n", flOrig);
			ERROR_EXIT
		}

		::sprintf(&flDefl[nNamePos], 
			"zlibtest_c%d_s%d_f%d.defl", nCompType, nFileSize, nFillStyle);

		if (NULL == (outf = ::fopen(flDefl, "wb")))
		{
			::printf("\ncannot create compressed file \"%s\"\n", flDefl);
			ERROR_EXIT
		}
		
		if (BFACSLIB_ERR_NOERROR != (nErr = ::ZLibEx_Create(
			ZLIBEX_MODE_COMPRESS, nCompType, -1, &ctx)))
		{
			::printf("\nerror creating compressor (%d)\n", nErr);
			ERROR_EXIT
		}

		// use different buffer sizes all the time, same size for input
		// and output buffers

		nOutSize = 0;
		nBSize = MIN_BUF_SIZE;

		for (;;)
		{
			if (0 == (nRead = (int)::fread(buf, 1, nBSize, inf)))
			{
				if (nBSize)	break;
			}

			do
			{
				nErr = ::ZLibEx_Process(ctx, buf, nRead, buf2, nBSize, &nTmp);

				switch (nErr)
				{
					case BFACSLIB_ERR_NOERROR:
					case BFACSLIB_ERR_ZLIBEX_REPEAT: break;
					default:
						::printf("\nerror compressing (%d)\n", nErr);
						ERROR_EXIT
				}

				nOutSize += nTmp;

				if (nTmp != (int)::fwrite(buf2, 1, nTmp, outf))
				{
					::puts("\nerror writing compressed content");
					ERROR_EXIT
				}
				
				NEW_BUFFER_SIZE
			}
			while (BFACSLIB_ERR_ZLIBEX_REPEAT == nErr);
		}

		::fclose(inf); inf = NULL;

		do
		{
			nErr = ::ZLibEx_Finalize(ctx, buf2, nBSize, &nTmp);

			switch (nErr)
			{
				case BFACSLIB_ERR_NOERROR:
				case BFACSLIB_ERR_ZLIBEX_REPEAT: break;
				default:
					::printf("\nerror finalizing (%d)\n", nErr);
					ERROR_EXIT
			};

			nOutSize += nTmp;

			if (nTmp != (int)::fwrite(buf2, 1, nTmp, outf))
			{
				::puts("\nerror writing finalized content");
				ERROR_EXIT
			}

			NEW_BUFFER_SIZE
		}
		while (BFACSLIB_ERR_ZLIBEX_REPEAT == nErr);

		::fclose(outf);	outf = NULL;
		
		if (BFACSLIB_ERR_NOERROR != (nErr = ::ZLibEx_Destroy(ctx)))
		{
			::printf("\nerror destroying compressor (%d)\n", nErr);
			ERROR_EXIT
		}

		::printf(", compressed: %d", nOutSize);

		// decompress the file

		if (NULL == (inf = ::fopen(flDefl, "rb")))
		{
			::printf("\ncannot reopen compressed file \"%s\"\n", flDefl);
			ERROR_EXIT
		}

		::sprintf(&flInfl[nNamePos], 
			"zlibtest_c%d_s%d_f%d.infl", nCompType, nFileSize, nFillStyle);

		if (NULL == (outf = ::fopen(flInfl, "wb")))
		{
			::printf("\ncannot create decompressed file \"%s\"\n", flInfl);
			ERROR_EXIT
		}
		
		if (BFACSLIB_ERR_NOERROR != (nErr = ::ZLibEx_Create(
			ZLIBEX_MODE_DECOMPRESS, nCompType, -1, &ctx)))
		{
			::printf("\nerror creating decompressor (%d)\n", nErr);
			ERROR_EXIT
		}

		nOutSize = 0;
		nBSize = MIN_BUF_SIZE;

		for (;;)
		{
			if (0 == (nRead = (int)::fread(buf, 1, nBSize, inf)))
			{
				if (nBSize)	break;
			}

			do
			{
				nErr = ::ZLibEx_Process(ctx, buf, nRead, buf2, nBSize, &nTmp);

				switch (nErr)
				{
					case BFACSLIB_ERR_NOERROR:
					case BFACSLIB_ERR_ZLIBEX_REPEAT: break;
					default:
						::printf("\nerror decompressing (%d)\n", nErr);
						ERROR_EXIT
				}

				if (-1 == nTmp)
				{
					break;
				}

				nOutSize += nTmp;

				if (nTmp != (int)::fwrite(buf2, 1, nTmp, outf))
				{
					::puts("\nerror writing compressed content");
					ERROR_EXIT
				}
				
				NEW_BUFFER_SIZE
			}
			while (BFACSLIB_ERR_ZLIBEX_REPEAT == nErr);
		}

		::fclose(inf); inf = NULL;

		do
		{
			nErr = ::ZLibEx_Finalize(ctx, buf2, nBSize, &nTmp);

			switch (nErr)
			{
				case BFACSLIB_ERR_NOERROR:
				case BFACSLIB_ERR_ZLIBEX_REPEAT: break;
				default:
					::printf("\nerror finalizing (%d)\n", nErr);
					ERROR_EXIT
			};

			if (-1 == nTmp)
			{
				break;
			}

			nOutSize += nTmp;

			if (nTmp != (int)::fwrite(buf2, 1, nTmp, outf))
			{
				::puts("\nerror writing finalized content");
				ERROR_EXIT
			}

			NEW_BUFFER_SIZE
		}
		while (BFACSLIB_ERR_ZLIBEX_REPEAT == nErr);

		::fclose(outf);	outf = NULL;
		
		if (BFACSLIB_ERR_NOERROR != (nErr = ::ZLibEx_Destroy(ctx)))
		{
			::printf("\nerror destroying decompressor (%d)\n", nErr);
			ERROR_EXIT
		}

		::printf(", decompressed: %d\n", nOutSize);

		// finally we need to compare the original and the decompressed file

		switch (::CompareFiles(flOrig, flInfl))
		{
			case 0:  break;
			case -1: ::puts("\nERROR OCCURED DURING FILE COMPARISON");
					 return false;
			default: ::puts("\nFILES ARE NOT EQUAL");
					 return false;	
		}

	}	// next compression type
	}	// next fillstyle
	}	// next size

	return true;

#undef NEW_BUFFER_SIZE
#undef ERROR_EXIT
}
