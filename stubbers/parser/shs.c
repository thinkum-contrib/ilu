/** 
 BeginILUCopyright

 Copyright (c) 1991-1999 Xerox Corporation.  All Rights Reserved.

 Unlimited use, reproduction, modification, and distribution of this
 software and modified versions thereof is permitted.  Permission is
 granted to make derivative works from this software or a modified
 version thereof.  Any copy of this software, a modified version
 thereof, or a derivative work must include both the above copyright
 notice of Xerox Corporation and this paragraph.  Any distribution of
 this software, a modified version thereof, or a derivative work must
 comply with all applicable United States export control laws.  This
 software is made available AS IS, and XEROX CORPORATION DISCLAIMS ALL
 WARRANTIES, EXPRESS OR IMPLIED, INCLUDING WITHOUT LIMITATION THE
 IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 PURPOSE, AND NOTWITHSTANDING ANY OTHER PROVISION CONTAINED HEREIN, ANY
 LIABILITY FOR DAMAGES RESULTING FROM THE SOFTWARE OR ITS USE IS
 EXPRESSLY DISCLAIMED, WHETHER ARISING IN CONTRACT, TORT (INCLUDING
 NEGLIGENCE) OR STRICT LIABILITY, EVEN IF XEROX CORPORATION IS ADVISED
 OF THE POSSIBILITY OF SUCH DAMAGES.
  
 EndILUCopyright
*/

/*
$Id: shs.c,v 1.13 1999/08/03 01:50:13 janssen Exp $
*/
/* Last edited by Mike Spreitzer November 21, 1996 9:07 pm PST */

#if ((defined(WIN32) || defined(WIN16)) && defined(_WINIO))
#include <winiodef.h>
#endif /* ((defined(WIN32) || defined(WIN16)) && defined(_WINIO)) */

#include <string.h>

#include "shs.h"

static void onem(UINT4 *m, UINT4 *h);

static void intFromChars(unsigned int *i, unsigned char *p);

void SHSInit (SHS_CTX *ctx)
{
    ctx->state[0] = 0x67452301;
    ctx->state[1] = 0x0efcdab89;
    ctx->state[2] = 0x98badcfe;
    ctx->state[3] = 0x10325476;
    ctx->state[4] = 0x0c3d2e1f0;

    ctx->count[0] = 0;
    ctx->count[1] = 0;

}

void SHSUpdate (SHS_CTX *ctx, unsigned char *bytes, unsigned int nBytes)
{
    unsigned int inBuffer = (unsigned int)((ctx->count[1]/8) % 64);
        /* number of bytes left in buffer from previous call */
        
    unsigned int inp = 0;

    if ((ctx->count[1] += ((UINT4)nBytes << 3)) < ((UINT4)nBytes << 3))
          ctx->count[0]++;
    ctx->count[0] += ((UINT4)nBytes >> 29);

    if (inBuffer != 0) {
        if ((inBuffer+nBytes) >= 64) {
            memmove (&ctx->buffer[inBuffer], bytes, inp=(64-inBuffer) );
            onem((UINT4 *) ctx->buffer, ctx->state );
            inBuffer=0;
        }
    };
            
    
    while (inp+64 <= nBytes ) {
        onem( (UINT4 *) &bytes[inp], ctx->state );
        inp += 64;
        };
        
    memmove (&(ctx->buffer[inBuffer]), bytes+inp, nBytes-inp);
    
}


static void charsFromInt (unsigned char *p, unsigned int i)
{
    int j;    
    for (j = 3 ; j>=0; j--) {
        p[j] = i % 256;
        i = i / 256;
    }
}

static void intFromChars (unsigned int *i, unsigned char *p)
{
    unsigned int sum = 0;
    unsigned int j;
    
    for (j = 0 ; j<4; j++) {
        sum = (sum)*256 + p[j];
    };
    *i = sum;
}

void SHSFinal (unsigned char *hash, SHS_CTX *ctx)
{
    unsigned char countAsChars[8];
    unsigned int inBuffer; 
    unsigned char terminator = 0x80;
    
        /* number of bytes left in buffer from previous call */
        
    charsFromInt(&(countAsChars[0]), ctx->count[0]);
    charsFromInt(&(countAsChars[4]), ctx->count[1]);
    SHSUpdate( ctx, &terminator, 1 );
    inBuffer = (unsigned int)((ctx->count[1]/8) % 64);
    if (inBuffer > 64-8) {
       memset((void *) &(ctx->buffer[inBuffer]), 0, 64-inBuffer);
       onem( (UINT4 *) ctx->buffer, ctx->state );
       inBuffer = 0;
    };
    memset((void *) &(ctx->buffer[inBuffer]), 0, (64-8)-inBuffer);
    memmove (&(ctx->buffer[64-8]), countAsChars, 8);
    onem( (UINT4 *) ctx->buffer, ctx->state );

    {
        unsigned int i;
        unsigned char *p;
        
        for ((i=0, 
            p=(unsigned char *)hash); i<5; (i++, p += 4)) {
            charsFromInt( p, ctx->state[i] );
        }
    }
}

#define S5(x) (((x)<<5) | ((x)>>27))
#define S30(x) (((x)<<30) | ((x)>>2))
#define K0 0x5a827999
#define K1 0x6ed9eba1
#define K2 0x8f1bbcdc
#define K3 0x0ca62c1d6
#define AND(x,y) ((x)&(y))
#define F0(x,y,z) (AND(x,y) | AND(~(x),z))
#define F1(x,y,z) ((x)^(y)^(z))
#define F2(x,y,z) (AND(x,y) | AND(x,z) | AND(y,z))
#define word UINT4


static void onem(UINT4 *m, UINT4 *h)
{
    word w[80];
    word A, B, C, D, E;
    word *p, *q;
    word *pm3, *pm8, *pm14, *pm16;
    word temp;
    int i;
    A = h[0]; B=h[1]; C=h[2]; D=h[3]; E=h[4];
#ifndef WORDS_BIGENDIAN
    for (p = m, q = w; p < m + 16; p++, q++) {
      intFromChars((unsigned int *) q, (unsigned char *) p);
    }
#else	/* big-endian case */
    for (p = m, q = w; p < m + 16; p++, q++) {
      *q = *p;
    }
#endif	/* ndef WORDS_BIGENDIAN */
    pm3 = w+13; pm8 = w+8; pm14=w+2; pm16=w;
    for (p=w+16; p < w+80; p++, pm3++, pm8++, pm14++, pm16++) {
        *p = *pm3 ^ *pm8 ^ *pm14 ^ *pm16;
        }
    for (i=0; i<20; i++) {
        temp = S5(A) + F0(B,C,D) + E + w[i] + K0;
        E=D; D=C; C=S30(B); B=A; A=temp;
        }
    for (i=20; i<40; i++) {
        temp = S5(A) + F1(B,C,D) + E + w[i] + K1;
        E=D; D=C; C=S30(B); B=A; A=temp;
        }
    for (i=40; i<60; i++) {
        temp = S5(A) + F2(B,C,D) + E + w[i] + K2;
        E=D; D=C; C=S30(B); B=A; A=temp;
        }
    for (i=60; i<80; i++) {
        temp = S5(A) + F1(B,C,D) + E + w[i] + K3;
        E=D; D=C; C=S30(B); B=A; A=temp;
        }
    h[0]=h[0]+A;
    h[1]=h[1]+B;
    h[2]=h[2]+C;
    h[3]=h[3]+D;
    h[4]=h[4]+E;
    }
    
