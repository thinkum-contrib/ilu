/*
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

$Id: bignum.c,v 1.12 1999/08/03 01:59:57 janssen Exp $
*/

/* This file is derived from code with the following copyright: */

/***********************************************************
Copyright 1991-1995 by Stichting Mathematisch Centrum, Amsterdam,
The Netherlands.

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its
documentation for any purpose and without fee is hereby granted,
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in
supporting documentation, and that the names of Stichting Mathematisch
Centrum or CWI or Corporation for National Research Initiatives or
CNRI not be used in advertising or publicity pertaining to
distribution of the software without specific, written prior
permission.

While CWI is the initial source for this software, a modified version
is made available by the Corporation for National Research Initiatives
(CNRI) at the Internet address ftp://ftp.python.org.

STICHTING MATHEMATISCH CENTRUM AND CNRI DISCLAIM ALL WARRANTIES WITH
REGARD TO THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF
MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL STICHTING MATHEMATISCH
CENTRUM OR CNRI BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL
DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR
PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
PERFORMANCE OF THIS SOFTWARE.

******************************************************************/

/* Long (arbitrary precision) integer object implementation */

/* XXX The functional organization of this file is terrible */

#include "ilubignm.h"
#include "bignum.h"

#include <stdlib.h>
#include <math.h>
#include <assert.h>
#include <ctype.h>
#include <string.h>

#define ABS(x) ((x) < 0 ? -(x) : (x))
#define MAX(x, y) ((x) < (y) ? (y) : (x))
#define MIN(x, y) ((x) > (y) ? (y) : (x))

static ilubignum_MallocFn  MallocFn =  &malloc;
static ilubignum_FreeFn    FreeFn =    &free;
static ilubignum_ReallocFn ReallocFn = &realloc;

/* Allocate a new long int object with size digits.
   Return NULL if we run out of memory. */

ilubignum_Value
  _ilubignum_New(int size, char **excn)
{
  int realsize = (size < 1) ? 0 : (size - 1);
  ilubignum_Value v;

  v = (*MallocFn)(sizeof(struct _ilubignum_Value_s) + (realsize * sizeof(digit)));
  if (v != NULL) {
    *excn = NULL;
    v->bn_ndigits = size;
  } else
    *excn = "no memory";
  return v;
}

/* Allocate a new long int object with size digits.
   Return NULL and set exception if we run out of memory. */

ilubignum_Value
  ilubignum_FromBytes(ilubignum_card32 size, ilubignum_card8 * bytes, ilubignum_card32 negative)
{
  char *excn;
  ilubignum_int32 realsize;
  ilubignum_Value v;
  int index, i;
  unsigned bitstore, bits;

  for (i = 0;  (i < size) && (bytes[i] == 0);  i++);
  realsize = (((size - i)* 8) + (SHIFT - 1)) / SHIFT;
  v = _ilubignum_New(realsize * (negative ? -1 : 1), &excn);
  if (excn == NULL) {
    index = size - 1;
    bits = 0;
    bitstore = 0;
    for (i = 0;  i < realsize;  i += 1) {
      while ((bits < SHIFT) && (index >= 0)) {
	if (index >= 0) {
	  bitstore |= (bytes[index--] << bits);
	  bits += 8;
	}
      };
      v->bn_digit[i] = (bitstore & MASK);
      if (bits > SHIFT) {
	bitstore >>= SHIFT;
	bits -= SHIFT;
      }
    }
  };
  return v;
}

ilubignum_Value
  ilubignum_Copy (ilubignum_Value v)
{
  ilubignum_Value n;
  char *err;
  int size = (v->bn_ndigits < 0) ? -v->bn_ndigits : v->bn_ndigits;
  n = _ilubignum_New(size, &err);
  if (err != NULL)
    return NULL;
  memcpy (&n->bn_digit[0], &v->bn_digit[0], sizeof(digit) * size);
  n->bn_ndigits *= (v->bn_ndigits < 0) ? -1 : 1;
  return n;
}

void
  ilubignum_FreeValue (ilubignum_Value v)
{
  (*FreeFn)(v);
}

/* Convert a long int object to a sequence of bytes.
   Return malloc'ed storage, and flag if bytes are PASSED */
ilubignum_card8 *
  ilubignum_AsBytes (ilubignum_Value aa,	/* INPUT, bignum to convert */
		     ilubignum_card32 *len,	/* OUTPUT, number of bytes in byte-seq */
		     ilubignum_card32 *passed,	/* OUTPUT, TRUE if bytes are PASSED */
		     ilubignum_card32 *negative)/* OUTPUT, TRUE if negative */
{
  int realsize = (aa->bn_ndigits < 0) ? -aa->bn_ndigits : aa->bn_ndigits;
  twodigits bitstore;
  int bytes_needed, i, index, bits;
  ilubignum_card8 *bytes;

  bytes_needed = (((realsize * SHIFT) / 8) + (((realsize * SHIFT) % 8) != 0));
  if (bytes_needed == 0)
    bytes_needed = 1;
  bytes = (ilubignum_card8 *) (*MallocFn) (bytes_needed);
  index = 0;
  bitstore = aa->bn_digit[index++];
  bits = SHIFT;
  for (i = bytes_needed - 1;  i >= 0;  i -= 1) {
    bytes[i] = bitstore & 0xFF;
    bitstore >>= 8;
    bits -= 8;
    if ((bits < 8) && (index < realsize)) {
      bitstore |= (aa->bn_digit[index++] << bits);
      bits += SHIFT;
    }
  }
  *len = bytes_needed;
  *negative = (aa->bn_ndigits < 0);
  *passed = ilubignum_TRUE;
  return bytes;
}

int
ilubignum_Compare (ilubignum_Value a, ilubignum_Value b)
{
  int sign;
	
  if (a == NULL && b != NULL)
    return -1;
  else if (a != NULL && b == NULL)
    return 1;
  else if (a == NULL && b == NULL)
    return 0;

  if (a->bn_ndigits != b->bn_ndigits) {
    if (ABS(a->bn_ndigits) == 0 && ABS(b->bn_ndigits) == 0)
      sign = 0;
    else
      sign = a->bn_ndigits - b->bn_ndigits;
  }
  else {
    int i = ABS(a->bn_ndigits);
    while (--i >= 0 && a->bn_digit[i] == b->bn_digit[i])
      ;
    if (i < 0)
      sign = 0;
    else {
      sign = (int)a->bn_digit[i] - (int)b->bn_digit[i];
      if (a->bn_ndigits < 0)
	sign = -sign;
    }
  }
  return sign < 0 ? -1 : sign > 0 ? 1 : 0;
}

void *ilubignum_Malloc(ilubignum_size_t size)
{
  return (*MallocFn)(size);
}

void *ilubignum_Realloc(void *ptr, ilubignum_size_t size)
{
  return (*ReallocFn)(ptr, size);
}

void  ilubignum_Free(void *ptr)
{
  (*FreeFn)(ptr);
}

void
  ilubignum_SetMallocFreeAndRealloc (ilubignum_MallocFn mallocfn, ilubignum_FreeFn freefn, ilubignum_ReallocFn reallocfn)
{
  MallocFn = mallocfn;
  FreeFn = freefn;
  ReallocFn = reallocfn;
}
