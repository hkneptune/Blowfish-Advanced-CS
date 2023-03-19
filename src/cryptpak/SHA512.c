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


#include <malloc.h>
#include <memory.h>

#include "SHA512.h"

// SHA-512, inspired by an implementation of Allan Saddi <allan@saddi.com>

//////////////////////////////////////////////////////////////////////////////

#define ROTL(x, n) (((x) << (n)) | ((x) >> (32 - (n))))
#define ROTR(x, n) (((x) >> (n)) | ((x) << (32 - (n))))
#define ROTL64(x, n) (((x) << (n)) | ((x) >> (64 - (n))))
#define ROTR64(x, n) (((x) >> (n)) | ((x) << (64 - (n))))

#define Ch(x, y, z) ((z) ^ ((x) & ((y) ^ (z))))
#define Maj(x, y, z) (((x) & ((y) | (z))) | ((y) & (z)))
#define SIGMA0(x) (ROTR64((x), 28) ^ ROTR64((x), 34) ^ ROTR64((x), 39))
#define SIGMA1(x) (ROTR64((x), 14) ^ ROTR64((x), 18) ^ ROTR64((x), 41))
#define sigma0(x) (ROTR64((x), 1) ^ ROTR64((x), 8) ^ ((x) >> 7))
#define sigma1(x) (ROTR64((x), 19) ^ ROTR64((x), 61) ^ ((x) >> 6))

#define DO_ROUND() { \
  t1 = h + SIGMA1(e) + Ch(e, f, g) + *(Kp++) + *(W++); \
  t2 = SIGMA0(a) + Maj(a, b, c); \
  h = g; \
  g = f; \
  f = e; \
  e = d + t1; \
  d = c; \
  c = b; \
  b = a; \
  a = t1 + t2; \
}

//////////////////////////////////////////////////////////////////////////////

static const WORD64 K[80] = 
{
  0x428a2f98d728ae22L, 0x7137449123ef65cdL,
  0xb5c0fbcfec4d3b2fL, 0xe9b5dba58189dbbcL,
  0x3956c25bf348b538L, 0x59f111f1b605d019L,
  0x923f82a4af194f9bL, 0xab1c5ed5da6d8118L,
  0xd807aa98a3030242L, 0x12835b0145706fbeL,
  0x243185be4ee4b28cL, 0x550c7dc3d5ffb4e2L,
  0x72be5d74f27b896fL, 0x80deb1fe3b1696b1L,
  0x9bdc06a725c71235L, 0xc19bf174cf692694L,
  0xe49b69c19ef14ad2L, 0xefbe4786384f25e3L,
  0x0fc19dc68b8cd5b5L, 0x240ca1cc77ac9c65L,
  0x2de92c6f592b0275L, 0x4a7484aa6ea6e483L,
  0x5cb0a9dcbd41fbd4L, 0x76f988da831153b5L,
  0x983e5152ee66dfabL, 0xa831c66d2db43210L,
  0xb00327c898fb213fL, 0xbf597fc7beef0ee4L,
  0xc6e00bf33da88fc2L, 0xd5a79147930aa725L,
  0x06ca6351e003826fL, 0x142929670a0e6e70L,
  0x27b70a8546d22ffcL, 0x2e1b21385c26c926L,
  0x4d2c6dfc5ac42aedL, 0x53380d139d95b3dfL,
  0x650a73548baf63deL, 0x766a0abb3c77b2a8L,
  0x81c2c92e47edaee6L, 0x92722c851482353bL,
  0xa2bfe8a14cf10364L, 0xa81a664bbc423001L,
  0xc24b8b70d0f89791L, 0xc76c51a30654be30L,
  0xd192e819d6ef5218L, 0xd69906245565a910L,
  0xf40e35855771202aL, 0x106aa07032bbd1b8L,
  0x19a4c116b8d2d0c8L, 0x1e376c085141ab53L,
  0x2748774cdf8eeb99L, 0x34b0bcb5e19b48a8L,
  0x391c0cb3c5c95a63L, 0x4ed8aa4ae3418acbL,
  0x5b9cca4f7763e373L, 0x682e6ff3d6b2b8a3L,
  0x748f82ee5defb2fcL, 0x78a5636f43172f60L,
  0x84c87814a1f0ab72L, 0x8cc702081a6439ecL,
  0x90befffa23631e28L, 0xa4506cebde82bde9L,
  0xbef9a3f7b2c67915L, 0xc67178f2e372532bL,
  0xca273eceea26619cL, 0xd186b8c721c0c207L,
  0xeada7dd6cde0eb1eL, 0xf57d4f7fee6ed178L,
  0x06f067aa72176fbaL, 0x0a637dc5a2c898a6L,
  0x113f9804bef90daeL, 0x1b710b35131c471bL,
  0x28db77f523047d84L, 0x32caab7b40c72493L,
  0x3c9ebe0a15c9bebcL, 0x431d67c49c100d4cL,
  0x4cc5d4becb3e42b6L, 0x597f299cfc657e2aL,
  0x5fcb6fab3ad6faecL, 0x6c44198c4a475817L
};

static const WORD8 PADDING[128] = 
{
  0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
};

//////////////////////////////////////////////////////////////////////////////

void SHA512_Transform(
  PSHA512CTX ctx,
  const WORD8* cbuf)
{
  WORD64 buf[80];
  WORD64 *W, *W2, *W7, *W15, *W16;
  WORD64 a, b, c, d, e, f, g, h;
  WORD64 t1, t2;
  const WORD64* Kp;
  int nI;

  W = buf;

  for (nI = 15; nI >= 0; nI--) {
    *(W++) = BYTES_TO_WORD64(cbuf);
    cbuf += sizeof(WORD64);
  }

  W16 = &buf[0];
  W15 = &buf[1];
  W7 = &buf[9];
  W2 = &buf[14];

  for (nI = 63; nI >= 0; nI--) {
    *(W++) = sigma1(*W2) + *(W7++) + sigma0(*W15) + *(W16++);
    W2++;
    W15++;
  }

  a = ctx->hash[0];
  b = ctx->hash[1];
  c = ctx->hash[2];
  d = ctx->hash[3];
  e = ctx->hash[4];
  f = ctx->hash[5];
  g = ctx->hash[6];
  h = ctx->hash[7];

  Kp = K;
  W = buf;

  for (nI = 79; nI >= 0; nI--)
    DO_ROUND();

  ctx->hash[0] += a;
  ctx->hash[1] += b;
  ctx->hash[2] += c;
  ctx->hash[3] += d;
  ctx->hash[4] += e;
  ctx->hash[5] += f;
  ctx->hash[6] += g;
  ctx->hash[7] += h;
}

//////////////////////////////////////////////////////////////////////////////

PSHA512CTX CRYPTPAK_API SHA512_Create()
{
	PSHA512CTX result = (PSHA512CTX)malloc(sizeof(SHA512CTX));
	SHA512_Initialize(result);
	return result;
}

//////////////////////////////////////////////////////////////////////////////

void CRYPTPAK_API SHA512_Initialize(
	PSHA512CTX ctx)
{
	SHA512_Reset(ctx);		
}

//////////////////////////////////////////////////////////////////////////////

void CRYPTPAK_API SHA512_Reset(
	PSHA512CTX ctx)
{
  ctx->qTotalLen = 0L;

  ctx->hash[0] = 0x6a09e667f3bcc908L;
  ctx->hash[1] = 0xbb67ae8584caa73bL;
  ctx->hash[2] = 0x3c6ef372fe94f82bL;
  ctx->hash[3] = 0xa54ff53a5f1d36f1L;
  ctx->hash[4] = 0x510e527fade682d1L;
  ctx->hash[5] = 0x9b05688c2b3e6c1fL;
  ctx->hash[6] = 0x1f83d9abfb41bd6bL;
  ctx->hash[7] = 0x5be0cd19137e2179L;

  ctx->lBufLen = 0;
}

//////////////////////////////////////////////////////////////////////////////

void CRYPTPAK_API SHA512_Destroy(
	PSHA512CTX ctx)
{
	memset(ctx, 0, sizeof(ctx));
	free(ctx);
}

//////////////////////////////////////////////////////////////////////////////

void CRYPTPAK_API SHA512_Update(
	PSHA512CTX ctx, 
	const void* buf, 
	WORD32 lLen)
{
  const WORD8* data;
  WORD32 lBufferBytesLeft, lBytesToCopy;

  data = (WORD8*)buf;

  while (lLen) 
  {
	  lBufferBytesLeft = 128L - ctx->lBufLen;

	  if (lLen < (lBytesToCopy = lBufferBytesLeft))
	  {
		  lBytesToCopy = lLen;
	  }

      ctx->qTotalLen += (WORD64)lBytesToCopy << 3;

	  memcpy(&ctx->data[ctx->lBufLen], data, lBytesToCopy);
	  ctx->lBufLen += lBytesToCopy;
	  
	  data += lBytesToCopy;
	  lLen -= lBytesToCopy;

	  if (128 == ctx->lBufLen) 
	  {
		  SHA512_Transform(ctx, ctx->data);
		  ctx->lBufLen = 0;
	  }
  }
}

//////////////////////////////////////////////////////////////////////////////

void CRYPTPAK_API SHA512_Final(
	PSHA512CTX ctx, 
	WORD8* hash)
{
	WORD32 lBytesToPad;
	WORD8 lenPad[16];
	int nI;

	lBytesToPad = 240 - ctx->lBufLen;
	if (128 < lBytesToPad)
	{
		lBytesToPad -= 128;
	}

	memset(lenPad, 0, 8);

	WORD64_TO_BYTES(ctx->qTotalLen, &lenPad[8])

	SHA512_Update(ctx, PADDING, lBytesToPad);
	SHA512_Update(ctx, lenPad, sizeof(lenPad));

    for (nI = 0; nI < 8; nI++) 
    {
        WORD64_TO_BYTES(ctx->hash[nI], hash)
        hash += 8;
	}
}

//////////////////////////////////////////////////////////////////////////////

static WORD8 REF_DIGEST[SHA512_DIGESTSIZE] =
{
	0x8e,0x95,0x9b,0x75,0xda,0xe3,0x13,0xda,0x8c,0xf4,0xf7,0x28,0x14,0xfc,0x14,0x3f, 
	0x8f,0x77,0x79,0xc6,0xeb,0x9f,0x7f,0xa1,0x72,0x99,0xae,0xad,0xb6,0x88,0x90,0x18,
	0x50,0x1d,0x28,0x9e,0x49,0x00,0xf7,0xe4,0x33,0x1b,0x99,0xde,0xc4,0xb5,0x43,0x3a, 
	0xc7,0xd3,0x29,0xee,0xb6,0xdd,0x26,0x54,0x5e,0x96,0xe5,0x5b,0x87,0x4b,0xe9,0x09
};

#define REF_DATA_STR											\
	"abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmn"	\
	"hijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu"

BYTEBOOL CRYPTPAK_API SHA512_SelfTest()
{
	PSHA512CTX ctx;
	WORD8 digest[SHA512_DIGESTSIZE];

	ctx = SHA512_Create();

	SHA512_Update(ctx, REF_DATA_STR, sizeof(REF_DATA_STR) - 1);
	SHA512_Final(ctx, digest);
	SHA512_Destroy(ctx);

	return (0 == memcmp(digest, REF_DIGEST, sizeof(digest))) ?
		BOOL_TRUE : 
	    BOOL_FALSE;
}
