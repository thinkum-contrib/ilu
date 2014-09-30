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

$Id: bigarith.c,v 1.10 1999/08/03 01:59:59 janssen Exp $
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
#include "ilubnops.h"

#include <math.h>
#include <assert.h>
#include <ctype.h>
#include <string.h>

#define ABS(x) ((x) < 0 ? -(x) : (x))

/* forward declarations */
static ilubignum_Value bignum_Normalize (ilubignum_Value);
static ilubignum_Value bignum_Mul1 (ilubignum_Value , wdigit, char **);
static ilubignum_Value bignum_Muladd1 (ilubignum_Value , wdigit, wdigit, char **);
static ilubignum_Value bignum_DivRem1 (ilubignum_Value , wdigit, digit *, char **);

/* Normalize (remove leading zeros from) a long int object.
   Doesn't attempt to free the storage--in most cases, due to the nature
   of the algorithms used, this could save at most be one word anyway. */

static ilubignum_Value
  bignum_Normalize(ilubignum_Value vv)
{
  int j = ABS(vv->bn_ndigits);
  register int i = j;
  register ilubignum_Value v = vv;
  
  while (i > 0 && v->bn_digit[i-1] == 0)
    --i;
  if (i != j)
    v->bn_ndigits = (v->bn_ndigits < 0) ? -(i) : i;
  return v;
}

/* Multiply by a single digit, ignoring the sign. */

static ilubignum_Value 
  bignum_Mul1(ilubignum_Value a, wdigit n, char **err)
{
  return bignum_Muladd1(a, n, (digit)0, err);
}

/* Multiply by a single digit and add a single digit, ignoring the sign. */

static ilubignum_Value 
  bignum_Muladd1(ilubignum_Value a, wdigit n, wdigit extra, char **err)
{
  int size_a = ABS(a->bn_ndigits);
  ilubignum_Value z = _ilubignum_New(size_a+1, err);
  twodigits carry = extra;
  int i;
  if (z == NULL) {
    return NULL;
  };  
  for (i = 0; i < size_a; ++i) {
    carry += (twodigits)a->bn_digit[i] * n;
    z->bn_digit[i] = (digit) (carry & MASK);
    carry >>= SHIFT;
  }
  z->bn_digit[i] = (digit) carry;
  return bignum_Normalize(z);
}

/* Divide a long integer by a digit, returning both the quotient
   (as function result) and the remainder (through *prem).
   The sign of a is ignored; n should not be zero. */

static ilubignum_Value 
  bignum_DivRem1(a, n, prem, err)
ilubignum_Value a;
wdigit n;
digit *prem;
char **err;
{
  int size = ABS(a->bn_ndigits);
  ilubignum_Value z;
  int i;
  twodigits rem = 0;
	
  assert(n > 0 && n <= MASK);
  z = _ilubignum_New(size, err);
  if (z == NULL)
    return NULL;
  for (i = size; --i >= 0; ) {
    rem = (rem << SHIFT) + a->bn_digit[i];
    z->bn_digit[i] = (digit) (rem/n);
    rem %= n;
  }
  *prem = (digit) rem;
  return bignum_Normalize(z);
}

/* Create a new long int object from a C long int */

ilubignum_Value
  ilubignum_FromInteger(ilubignum_MaxIntType ival,
			char **err)
{
  /* Assume a C long fits in at most 5 'digits' */
  /* Works on both 32- and 64-bit machines */
  ilubignum_Value v = _ilubignum_New(5, err);
  if (v != NULL) {
    ilubignum_MaxCardType t = ival;
    int i;
    if (ival < 0) {
      t = -ival;
      v->bn_ndigits = -(v->bn_ndigits);
    }
    for (i = 0; i < 5; i++) {
      v->bn_digit[i] = (digit) (t & MASK);
      t >>= SHIFT;
    }
    v = bignum_Normalize(v);
  }
  return v;
}

/* Create a new long int object from a C unsigned long int */

ilubignum_Value
ilubignum_FromCardinal(ilubignum_MaxCardType ival,
		       char **err)
{
  /* Assume a C long fits in at most 5 'digits' */
  /* Works on both 32- and 64-bit machines */
  ilubignum_Value v = _ilubignum_New(5, err);
  if (v != NULL) {
    ilubignum_MaxCardType t = ival;
    int i;
    for (i = 0; i < 5; i++) {
      v->bn_digit[i] = (digit) (t & MASK);
      t >>= SHIFT;
    }
    v = bignum_Normalize(v);
  }
  return v;
}

/* Create a new long int object from a C double */

ilubignum_Value
  ilubignum_FromReal(double dval, char **err)
{
  ilubignum_Value v;
  double frac;
  int i, ndig, expo, neg;

  *err = NULL;
  neg = 0;
  if (dval < 0.0) {
    neg = 1;
    dval = -dval;
  }
  frac = frexp(dval, &expo); /* dval = frac*2**expo; 0.0 <= frac < 1.0 */
  if (expo <= 0)
    return ilubignum_FromCardinal(0L, err);
  ndig = (expo-1) / SHIFT + 1; /* Number of 'digits' in result */
  v = _ilubignum_New(ndig, err);
  if (v == NULL)
    return NULL;
  frac = ldexp(frac, (expo-1) % SHIFT + 1);
  for (i = ndig; --i >= 0; ) {
    long bits = (long)frac;
    v->bn_digit[i] = (digit) bits;
    frac = frac - (double)bits;
    frac = ldexp(frac, SHIFT);
  }
  if (neg)
    v->bn_ndigits = -(v->bn_ndigits);
  return v;
}

/* Get a C long int from a long int object.
   Returns -1 and sets an error condition if overflow occurs. */

ilubignum_MaxIntType
  ilubignum_AsInteger(ilubignum_Value vv, char **err)
{
  register ilubignum_Value v = vv;
  ilubignum_MaxIntType x, prev;
  int i, sign;

  *err = NULL;
  i = v->bn_ndigits;
  sign = 1;
  x = 0;
  if (i < 0) {
    sign = -1;
    i = -(i);
  }
  while (--i >= 0) {
    prev = x;
    x = (x << SHIFT) + v->bn_digit[i];
    if ((x >> SHIFT) != prev) {
      *err = "bignum too big for ilubignum_MaxIntType";
      return -1;
    }
  }
  return x * sign;
}

/* Get a C long int from a long int object.
   Returns -1 and sets an error condition if overflow occurs. */

ilubignum_MaxCardType
  ilubignum_AsCardinal(ilubignum_Value v, char **err)
{
  ilubignum_MaxCardType x, prev;
  int i;
	
  *err = NULL;
  i = v->bn_ndigits;
  x = 0;
  if (i < 0) {
    *err = "negative bignum can't be converted to cardinal";
    return -1;
  }
  while (--i >= 0) {
    prev = x;
    x = (x << SHIFT) + v->bn_digit[i];
    if ((x >> SHIFT) != prev) {
      *err = "bignum too big for ilubignum_MaxCardType";
      return -1;
    }
  }
  return x;
}

/* Get a C double from a long int object. */

double
  ilubignum_AsReal(ilubignum_Value v, char **err)
{
  double x;
  double multiplier = (double) (1L << SHIFT);
  int i, sign;

  *err = NULL;
  i = v->bn_ndigits;
  sign = 1;
  x = 0.0;
  if (i < 0) {
    sign = -1;
    i = -(i);
  }
  while (--i >= 0) {
    x = x*multiplier + (double)v->bn_digit[i];
  }
  return x * sign;
}

ilubignum_Value
  ilubignum_FromString(char *str, char **pend, int base, char **err)
{
  int sign = 1;
  ilubignum_Value z;
	
  if ((base != 0 && base < 2) || base > 36) {
    *err = "invalid base";
    return NULL;
  };
  while (*str != '\0' && isspace(*str))
    str++;
  if (*str == '+')
    ++str;
  else if (*str == '-') {
    ++str;
    sign = -1;
  }
  while (*str != '\0' && isspace(*str))
    str++;
  if (base == 0) {
    if ((str[0] != '0') && (str[0] != '#'))
      base = 10;
    else if (str[1] == 'x' || str[1] == 'X')
      { base = 16; str += 2; }
    else if (str[1] == 'b' || str[1] == 'B')
      { base = 2; str += 2; }
    else if (str[1] == 'o' || str[1] == 'O')
      { base = 8; str += 2; }
    else if (str[1] == 'd' || str[1] == 'D')
      { base = 10; str += 2; }
    else if (isdigit(str[1]))
      { base = 8; str += 1; }
    else if (str[0] == '0') {
      if (pend != NULL)
	*pend = &str[1];
      z = _ilubignum_New(0, err);
      return z;
    } else {
      *err = "badly formed bignum string value";
      return NULL;
    }
  }
  z = _ilubignum_New(0, err);
  if (*err != NULL)
    return NULL;
  for ( ; z != NULL; ++str) {
    int k = -1;
    ilubignum_Value temp;
		
    if (*str <= '9')
      k = *str - '0';
    else if (*str >= 'a')
      k = *str - 'a' + 10;
    else if (*str >= 'A')
      k = *str - 'A' + 10;
    if (k < 0 || k >= base)
      break;
    temp = bignum_Muladd1(z, (digit)base, (digit)k, err);
    ilubignum_FreeValue(z);
    if (*err != NULL) return NULL;
    z = temp;
  }
  if (sign < 0 && z != NULL && z->bn_ndigits != 0)
    z->bn_ndigits = -(z->bn_ndigits);
  if (pend)
    *pend = str;
  return z;
}

/* Convert a long int object to a string, using a given conversion base.
   Return a string object.
   If base is 8 or 16, add the proper prefix '0' or '0x'.
   External linkage: used in bltinmodule.c by hex() and oct(). */

char *
  ilubignum_AsString (ilubignum_Value aa, int base, char **err)
{
  register ilubignum_Value a = aa;
  char *str;
  int i;
  int size_a = ABS(a->bn_ndigits);
  char *p;
  int bits;
  char sign = '\0';

  assert(base >= 2 && base <= 36);
	
  /* Compute a rough upper bound for the length of the string */
  i = base;
  bits = 0;
  while (i > 1) {
    ++bits;
    i >>= 1;
  }
  i = 5 + (size_a*SHIFT + bits-1) / bits;
  str = ilubignum_Malloc(i + 1);
  if (str == NULL)
    return str;
  p = str + i;
  *p = '\0';
  if (a->bn_ndigits < 0)
    sign = '-';
  do {
    digit rem;
    ilubignum_Value temp = bignum_DivRem1(a, (digit)base, &rem, err);
    if (temp == NULL) {
      ilubignum_Free(str);
      if (a != aa)
	ilubignum_FreeValue(a);
      return str;
    }
    if (rem < 10)
      rem += '0';
    else
      rem += 'A'-10;
    assert(p > str);
    *--p = (char) rem;
    if (a != aa)
      ilubignum_FreeValue(a);
    a = temp;
  } while (ABS(a->bn_ndigits) != 0);
  if (a != aa)
    ilubignum_FreeValue(a);
  if (base == 8) {
    if (size_a != 0)
      *--p = '0';
  }
  else if (base == 16) {
    *--p = 'x';
    *--p = '0';
  }
  else if (base != 10) {
    *--p = '#';
    *--p = '0' + base%10;
    if (base > 10)
      *--p = '0' + base/10;
  }
  if (sign)
    *--p = sign;
  if (p != str) {
    char *q = str;
    char *old = str;
    assert(p > q);
    do {
    } while ((*q++ = *p++) != '\0');
    str = ilubignum_Realloc(old, (int) (q - old));
  }
  return str;
}

static ilubignum_Value x_divrem (ilubignum_Value , ilubignum_Value , ilubignum_Value *, char **);
static ilubignum_Value long_pos (ilubignum_Value, char **);
static int long_divrem (ilubignum_Value , ilubignum_Value , ilubignum_Value *, ilubignum_Value *, char **);

/* Long division with remainder, top-level routine */

static int
  long_divrem(ilubignum_Value a, ilubignum_Value b, ilubignum_Value *pdiv, ilubignum_Value *prem, char **err)
{
  int size_a = ABS(a->bn_ndigits), size_b = ABS(b->bn_ndigits);
  ilubignum_Value z;
	
  if (size_b == 0) {
    *err = "divide by zero";
    return -1;
  };
  if (size_a < size_b ||
      (size_a == size_b &&
       a->bn_digit[size_a-1] < b->bn_digit[size_b-1])) {
    /* |a| < |b|. */
    *pdiv = _ilubignum_New(0, err);
    if (*err != NULL) return -1;
    *prem = (ilubignum_Value ) a;
    return 0;
  }
  if (size_b == 1) {
    digit rem = 0;
    z = bignum_DivRem1(a, b->bn_digit[0], &rem, err);
    if (z == NULL)
      return -1;
    *prem = ilubignum_FromInteger((ilubignum_int32)rem, err);
    if (*err != NULL) {
      ilubignum_FreeValue(z);
      return -1;
    }
  } else {
    z = x_divrem(a, b, prem, err);
    if (z == NULL)
      return -1;
  }
  /* Set the signs.
     The quotient z has the sign of a*b;
     the remainder r has the sign of a,
     so a = b*z + r. */
  if ((a->bn_ndigits < 0) != (b->bn_ndigits < 0))
    z->bn_ndigits = -(z->bn_ndigits);
  if (a->bn_ndigits < 0 && (*prem)->bn_ndigits != 0)
    (*prem)->bn_ndigits = -((*prem)->bn_ndigits);
  *pdiv = z;
  return 0;
}

/* Unsigned long division with remainder -- the algorithm */

static ilubignum_Value 
  x_divrem(ilubignum_Value v1, ilubignum_Value w1, ilubignum_Value *prem, char **err)
{
  int size_v = ABS(v1->bn_ndigits), size_w = ABS(w1->bn_ndigits);
  digit d = (digit) ((twodigits)BASE / (w1->bn_digit[size_w-1] + 1));
  ilubignum_Value v;
  ilubignum_Value w;
  ilubignum_Value a;
  int j, k;

  v = bignum_Mul1(v1, d, err);
  if (*err != NULL) return NULL;
  w = bignum_Mul1(w1, d, err);
  if (*err != NULL) { ilubignum_FreeValue(v); return NULL; };
	
  assert(size_v >= size_w && size_w > 1); /* Assert checks by div() */
  assert(size_w == ABS(w->bn_ndigits)); /* That's how d was calculated */
	
  size_v = ABS(v->bn_ndigits);
  a = _ilubignum_New(size_v - size_w + 1, err);
  if (*err != NULL) { ilubignum_FreeValue(v); ilubignum_FreeValue(w); return NULL; };
	
  for (j = size_v, k = a->bn_ndigits-1; a != NULL && k >= 0; --j, --k) {
    digit vj = (j >= size_v) ? 0 : v->bn_digit[j];
    twodigits q;
    stwodigits carry = 0;
    int i;
		
    if (vj == w->bn_digit[size_w-1])
      q = MASK;
    else
      q = (((twodigits)vj << SHIFT) + v->bn_digit[j-1]) /
	w->bn_digit[size_w-1];
		
    while (w->bn_digit[size_w-2]*q >
	   ((
	     ((twodigits)vj << SHIFT)
	     + v->bn_digit[j-1]
	     - q*w->bn_digit[size_w-1]
	     ) << SHIFT)
	   + v->bn_digit[j-2])
      --q;
		
    for (i = 0; i < size_w && i+k < size_v; ++i) {
      twodigits z = w->bn_digit[i] * q;
      digit zz = (digit) (z >> SHIFT);
      carry += v->bn_digit[i+k] - z
	+ ((twodigits)zz << SHIFT);
      v->bn_digit[i+k] = carry & MASK;
      carry = (carry >> SHIFT) - zz;
    }
		
    if (i+k < size_v) {
      carry += v->bn_digit[i+k];
      v->bn_digit[i+k] = 0;
    }
		
    if (carry == 0)
      a->bn_digit[k] = (digit) q;
    else {
      assert(carry == -1);
      a->bn_digit[k] = (digit) q-1;
      carry = 0;
      for (i = 0; i < size_w && i+k < size_v; ++i) {
	carry += v->bn_digit[i+k] + w->bn_digit[i];
	v->bn_digit[i+k] = carry & MASK;
	carry >>= SHIFT;
      }
    }
  } /* for j, k */
	
  if (a == NULL)
    *prem = NULL;
  else {
    a = bignum_Normalize(a);
    *prem = bignum_DivRem1(v, d, &d, err);
    /* d receives the (unused) remainder */
    if (*prem == NULL) {
      ilubignum_FreeValue(a);
      a = NULL;
    }
  }
  ilubignum_FreeValue(v);
  ilubignum_FreeValue(w);
  return a;
}

#if 0

/* Methods */

/* Forward */
static void long_dealloc (PyObject *);
static PyObject *long_repr (PyObject *);
static int long_compare (ilubignum_Value , ilubignum_Value );
static long long_hash (ilubignum_Value );

static PyObject *long_add (ilubignum_Value , ilubignum_Value );
static PyObject *long_sub (ilubignum_Value , ilubignum_Value );
static PyObject *long_mul (ilubignum_Value , ilubignum_Value );
static PyObject *long_div (ilubignum_Value , ilubignum_Value );
static PyObject *long_mod (ilubignum_Value , ilubignum_Value );
static PyObject *long_divmod (ilubignum_Value , ilubignum_Value );
static PyObject *long_pow
	(ilubignum_Value , ilubignum_Value , ilubignum_Value );
static PyObject *long_neg (ilubignum_Value );
static PyObject *long_pos (ilubignum_Value );
static PyObject *long_abs (ilubignum_Value );
static int long_nonzero (ilubignum_Value );
static PyObject *long_invert (ilubignum_Value );
static PyObject *long_lshift (ilubignum_Value , ilubignum_Value );
static PyObject *long_rshift (ilubignum_Value , ilubignum_Value );
static PyObject *long_and (ilubignum_Value , ilubignum_Value );
static PyObject *long_xor (ilubignum_Value , ilubignum_Value );
static PyObject *long_or (ilubignum_Value , ilubignum_Value );

static void
long_dealloc(v)
	PyObject *v;
{
	PyMem_DEL(v);
}

static long
long_hash(v)
	ilubignum_Value v;
{
	long x;
	int i, sign;

	/* This is designed so that Python ints and longs with the
	   same value hash to the same value, otherwise comparisons
	   of mapping keys will turn out weird */
	i = v->bn_ndigits;
	sign = 1;
	x = 0;
	if (i < 0) {
		sign = -1;
		i = -(i);
	}
	while (--i >= 0) {
		/* Force a 32-bit circular shift */
		x = ((x << SHIFT) & ~MASK) | ((x >> (32-SHIFT)) & MASK);
		x += v->bn_digit[i];
	}
	x = x * sign;
	if (x == -1)
		x = -2;
	return x;
}


/* Add the absolute values of two long integers. */

static ilubignum_Value x_add (ilubignum_Value , ilubignum_Value );
static ilubignum_Value 
x_add(a, b)
	ilubignum_Value a, *b;
{
	int size_a = ABS(a->bn_ndigits), size_b = ABS(b->bn_ndigits);
	ilubignum_Value z;
	int i;
	digit carry = 0;
	
	/* Ensure a is the larger of the two: */
	if (size_a < size_b) {
		{ ilubignum_Value temp = a; a = b; b = temp; }
		{ int size_temp = size_a;
		  size_a = size_b;
		  size_b = size_temp; }
	}
	z = _ilubignum_New(size_a+1);
	if (z == NULL)
		return NULL;
	for (i = 0; i < size_b; ++i) {
		carry += a->bn_digit[i] + b->bn_digit[i];
		z->bn_digit[i] = carry & MASK;
		/* The following assumes unsigned shifts don't
		   propagate the sign bit. */
		carry >>= SHIFT;
	}
	for (; i < size_a; ++i) {
		carry += a->bn_digit[i];
		z->bn_digit[i] = carry & MASK;
		carry >>= SHIFT;
	}
	z->bn_digit[i] = carry;
	return bignum_Normalize(z);
}

/* Subtract the absolute values of two integers. */

static ilubignum_Value x_sub (ilubignum_Value , ilubignum_Value );
static ilubignum_Value 
x_sub(a, b)
	ilubignum_Value a, *b;
{
	int size_a = ABS(a->bn_ndigits), size_b = ABS(b->bn_ndigits);
	ilubignum_Value z;
	int i;
	int sign = 1;
	digit borrow = 0;
	
	/* Ensure a is the larger of the two: */
	if (size_a < size_b) {
		sign = -1;
		{ ilubignum_Value temp = a; a = b; b = temp; }
		{ int size_temp = size_a;
		  size_a = size_b;
		  size_b = size_temp; }
	}
	else if (size_a == size_b) {
		/* Find highest digit where a and b differ: */
		i = size_a;
		while (--i >= 0 && a->bn_digit[i] == b->bn_digit[i])
			;
		if (i < 0)
			return _ilubignum_New(0);
		if (a->bn_digit[i] < b->bn_digit[i]) {
			sign = -1;
			{ ilubignum_Value temp = a; a = b; b = temp; }
		}
		size_a = size_b = i+1;
	}
	z = _ilubignum_New(size_a);
	if (z == NULL)
		return NULL;
	for (i = 0; i < size_b; ++i) {
		/* The following assumes unsigned arithmetic
		   works module 2**N for some N>SHIFT. */
		borrow = a->bn_digit[i] - b->bn_digit[i] - borrow;
		z->bn_digit[i] = borrow & MASK;
		borrow >>= SHIFT;
		borrow &= 1; /* Keep only one sign bit */
	}
	for (; i < size_a; ++i) {
		borrow = a->bn_digit[i] - borrow;
		z->bn_digit[i] = borrow & MASK;
		borrow >>= SHIFT;
	}
	assert(borrow == 0);
	if (sign < 0)
		z->bn_ndigits = -(z->bn_ndigits);
	return bignum_Normalize(z);
}

static PyObject *
long_add(a, b)
	ilubignum_Value a;
	ilubignum_Value b;
{
	ilubignum_Value z;
	
	if (a->bn_ndigits < 0) {
		if (b->bn_ndigits < 0) {
			z = x_add(a, b);
			if (z != NULL && z->bn_ndigits != 0)
				z->bn_ndigits = -(z->bn_ndigits);
		}
		else
			z = x_sub(b, a);
	}
	else {
		if (b->bn_ndigits < 0)
			z = x_sub(a, b);
		else
			z = x_add(a, b);
	}
	return (PyObject *)z;
}

static PyObject *
long_sub(a, b)
	ilubignum_Value a;
	ilubignum_Value b;
{
	ilubignum_Value z;
	
	if (a->bn_ndigits < 0) {
		if (b->bn_ndigits < 0)
			z = x_sub(a, b);
		else
			z = x_add(a, b);
		if (z != NULL && z->bn_ndigits != 0)
			z->bn_ndigits = -(z->bn_ndigits);
	}
	else {
		if (b->bn_ndigits < 0)
			z = x_add(a, b);
		else
			z = x_sub(a, b);
	}
	return (PyObject *)z;
}

static PyObject *
long_mul(a, b)
	ilubignum_Value a;
	ilubignum_Value b;
{
	int size_a;
	int size_b;
	ilubignum_Value z;
	int i;
	
	size_a = ABS(a->bn_ndigits);
	size_b = ABS(b->bn_ndigits);
	z = _ilubignum_New(size_a + size_b);
	if (z == NULL)
		return NULL;
	for (i = 0; i < z->bn_ndigits; ++i)
		z->bn_digit[i] = 0;
	for (i = 0; i < size_a; ++i) {
		twodigits carry = 0;
		twodigits f = a->bn_digit[i];
		int j;
		
		SIGCHECK({
			Py_DECREF(z);
			return NULL;
		})
		for (j = 0; j < size_b; ++j) {
			carry += z->bn_digit[i+j] + b->bn_digit[j] * f;
			z->bn_digit[i+j] = (digit) (carry & MASK);
			carry >>= SHIFT;
		}
		for (; carry != 0; ++j) {
			assert(i+j < z->bn_ndigits);
			carry += z->bn_digit[i+j];
			z->bn_digit[i+j] = (digit) (carry & MASK);
			carry >>= SHIFT;
		}
	}
	if (a->bn_ndigits < 0)
		z->bn_ndigits = -(z->bn_ndigits);
	if (b->bn_ndigits < 0)
		z->bn_ndigits = -(z->bn_ndigits);
	return (PyObject *) bignum_Normalize(z);
}

/* The / and % operators are now defined in terms of divmod().
   The expression a mod b has the value a - b*floor(a/b).
   The long_divrem function gives the remainder after division of
   |a| by |b|, with the sign of a.  This is also expressed
   as a - b*trunc(a/b), if trunc truncates towards zero.
   Some examples:
   	 a	 b	a rem b		a mod b
   	 13	 10	 3		 3
   	-13	 10	-3		 7
   	 13	-10	 3		-7
   	-13	-10	-3		-3
   So, to get from rem to mod, we have to add b if a and b
   have different signs.  We then subtract one from the 'div'
   part of the outcome to keep the invariant intact. */

static int l_divmod Py_PROTO((ilubignum_Value , ilubignum_Value ,
	ilubignum_Value *, ilubignum_Value *));
static int
l_divmod(v, w, pdiv, pmod)
	ilubignum_Value v;
	ilubignum_Value w;
	ilubignum_Value *pdiv;
	ilubignum_Value *pmod;
{
	ilubignum_Value div, *mod;
	
	if (long_divrem(v, w, &div, &mod) < 0)
		return -1;
	if ((mod->bn_ndigits < 0 && w->bn_ndigits > 0) ||
	    (mod->bn_ndigits > 0 && w->bn_ndigits < 0)) {
		ilubignum_Value temp;
		ilubignum_Value one;
		temp = (ilubignum_Value ) long_add(mod, w);
		Py_DECREF(mod);
		mod = temp;
		if (mod == NULL) {
			Py_DECREF(div);
			return -1;
		}
		one = (ilubignum_Value ) ilubignum_FromInteger(1L);
		if (one == NULL ||
		    (temp = (ilubignum_Value ) long_sub(div, one)) == NULL) {
			Py_DECREF(mod);
			Py_DECREF(div);
			Py_XDECREF(one);
			return -1;
		}
		Py_DECREF(one);
		Py_DECREF(div);
		div = temp;
	}
	*pdiv = div;
	*pmod = mod;
	return 0;
}

static PyObject *
long_div(v, w)
	ilubignum_Value v;
	ilubignum_Value w;
{
	ilubignum_Value div, *mod;
	if (l_divmod(v, w, &div, &mod) < 0)
		return NULL;
	Py_DECREF(mod);
	return (PyObject *)div;
}

static PyObject *
long_mod(v, w)
	ilubignum_Value v;
	ilubignum_Value w;
{
	ilubignum_Value div, *mod;
	if (l_divmod(v, w, &div, &mod) < 0)
		return NULL;
	Py_DECREF(div);
	return (PyObject *)mod;
}

static PyObject *
long_divmod(v, w)
	ilubignum_Value v;
	ilubignum_Value w;
{
	PyObject *z;
	ilubignum_Value div, *mod;
	if (l_divmod(v, w, &div, &mod) < 0)
		return NULL;
	z = PyTuple_New(2);
	if (z != NULL) {
		PyTuple_SetItem(z, 0, (PyObject *) div);
		PyTuple_SetItem(z, 1, (PyObject *) mod);
	}
	else {
		Py_DECREF(div);
		Py_DECREF(mod);
	}
	return z;
}

static PyObject *
long_pow(a, b, c)
	ilubignum_Value a;
	ilubignum_Value b;
	ilubignum_Value c;
{
	ilubignum_Value z, *div, *mod;
	int size_b, i;
	
	size_b = b->bn_ndigits;
	if (size_b < 0) {
		PyErr_SetString(PyExc_ValueError,
				"long integer to the negative power");
		return NULL;
	}
	z = (ilubignum_Value )ilubignum_FromInteger(1L);
	Py_INCREF(a);
	for (i = 0; i < size_b; ++i) {
		digit bi = b->bn_digit[i];
		int j;
	
		for (j = 0; j < SHIFT; ++j) {
			ilubignum_Value temp;
		
			if (bi & 1) {
				temp = (ilubignum_Value )long_mul(z, a);
				Py_DECREF(z);
			 	if ((PyObject*)c!=Py_None && temp!=NULL) {
			 		l_divmod(temp, c, &div, &mod);
				 	Py_XDECREF(div);
				 	Py_DECREF(temp);
				 	temp = mod;
				}
			 	z = temp;
				if (z == NULL)
					break;
			}
			bi >>= 1;
			if (bi == 0 && i+1 == size_b)
				break;
			temp = (ilubignum_Value )long_mul(a, a);
			Py_DECREF(a);
		 	if ((PyObject*)c!=Py_None && temp!=NULL) {
			 	l_divmod(temp, c, &div, &mod);
			 	Py_XDECREF(div);
			 	Py_DECREF(temp);
			 	temp = mod;
			}
			a = temp;
			if (a == NULL) {
				Py_DECREF(z);
				z = NULL;
				break;
			}
		}
		if (a == NULL || z == NULL)
			break;
	}
	Py_XDECREF(a);
	if ((PyObject*)c!=Py_None && z!=NULL) {
			l_divmod(z, c, &div, &mod);
			Py_XDECREF(div);
			Py_DECREF(z);
			z=mod;
	}
	return (PyObject *)z;
}

static PyObject *
long_invert(v)
	ilubignum_Value v;
{
	/* Implement ~x as -(x+1) */
	ilubignum_Value x;
	ilubignum_Value w;
	w = (ilubignum_Value )ilubignum_FromInteger(1L);
	if (w == NULL)
		return NULL;
	x = (ilubignum_Value ) long_add(v, w);
	Py_DECREF(w);
	if (x == NULL)
		return NULL;
	if (x->bn_ndigits != 0)
		x->bn_ndigits = -(x->bn_ndigits);
	return (PyObject *)x;
}

static PyObject *
long_pos(v)
	ilubignum_Value v;
{
	Py_INCREF(v);
	return v;
}

static PyObject *
long_neg(v)
	ilubignum_Value v;
{
	ilubignum_Value z;
	int i, n;
	n = ABS(v->bn_ndigits);
	if (n == 0) {
		/* -0 == 0 */
		Py_INCREF(v);
		return (PyObject *) v;
	}
	z = _ilubignum_New(ABS(n));
	if (z == NULL)
		return NULL;
	for (i = 0; i < n; i++)
		z->bn_digit[i] = v->bn_digit[i];
	z->bn_ndigits = -(v->bn_ndigits);
	return (PyObject *)z;
}

static PyObject *
long_abs(v)
	ilubignum_Value v;
{
	if (v->bn_ndigits < 0)
		return long_neg(v);
	else {
		Py_INCREF(v);
		return v;
	}
}

static int
long_nonzero(v)
	ilubignum_Value v;
{
	return ABS(v->bn_ndigits) != 0;
}

static PyObject *
long_rshift(a, b)
	ilubignum_Value a;
	ilubignum_Value b;
{
	ilubignum_Value z;
	long shiftby;
	int newsize, wordshift, loshift, hishift, i, j;
	digit lomask, himask;
	
	if (a->bn_ndigits < 0) {
		/* Right shifting negative numbers is harder */
		ilubignum_Value a1, *a2, *a3;
		a1 = (ilubignum_Value ) long_invert(a);
		if (a1 == NULL) return NULL;
		a2 = (ilubignum_Value ) long_rshift(a1, b);
		Py_DECREF(a1);
		if (a2 == NULL) return NULL;
		a3 = (ilubignum_Value ) long_invert(a2);
		Py_DECREF(a2);
		return (PyObject *) a3;
	}
	
	shiftby = ilubignum_AsLong((PyObject *)b);
	if (shiftby == -1L && PyErr_Occurred())
		return NULL;
	if (shiftby < 0) {
		PyErr_SetString(PyExc_ValueError, "negative shift count");
		return NULL;
	}
	wordshift = shiftby / SHIFT;
	newsize = ABS(a->bn_ndigits) - wordshift;
	if (newsize <= 0) {
		z = _ilubignum_New(0);
		return (PyObject *)z;
	}
	loshift = shiftby % SHIFT;
	hishift = SHIFT - loshift;
	lomask = ((digit)1 << hishift) - 1;
	himask = MASK ^ lomask;
	z = _ilubignum_New(newsize);
	if (z == NULL)
		return NULL;
	if (a->bn_ndigits < 0)
		z->bn_ndigits = -(z->bn_ndigits);
	for (i = 0, j = wordshift; i < newsize; i++, j++) {
		z->bn_digit[i] = (a->bn_digit[j] >> loshift) & lomask;
		if (i+1 < newsize)
			z->bn_digit[i] |=
			  (a->bn_digit[j+1] << hishift) & himask;
	}
	return (PyObject *) bignum_Normalize(z);
}

static PyObject *
long_lshift(a, b)
	ilubignum_Value a;
	ilubignum_Value b;
{
	/* This version due to Tim Peters */
	ilubignum_Value z;
	long shiftby;
	int oldsize, newsize, wordshift, remshift, i, j;
	twodigits accum;
	
	shiftby = ilubignum_AsLong((PyObject *)b);
	if (shiftby == -1L && PyErr_Occurred())
		return NULL;
	if (shiftby < 0) {
		PyErr_SetString(PyExc_ValueError, "negative shift count");
		return NULL;
	}
	if ((long)(int)shiftby != shiftby) {
		PyErr_SetString(PyExc_ValueError,
				"outrageous left shift count");
		return NULL;
	}
	/* wordshift, remshift = divmod(shiftby, SHIFT) */
	wordshift = (int)shiftby / SHIFT;
	remshift  = (int)shiftby - wordshift * SHIFT;

	oldsize = ABS(a->bn_ndigits);
	newsize = oldsize + wordshift;
	if (remshift)
		++newsize;
	z = _ilubignum_New(newsize);
	if (z == NULL)
		return NULL;
	if (a->bn_ndigits < 0)
		z->bn_ndigits = -(z->bn_ndigits);
	for (i = 0; i < wordshift; i++)
		z->bn_digit[i] = 0;
	accum = 0;	
	for (i = wordshift, j = 0; j < oldsize; i++, j++) {
		accum |= a->bn_digit[j] << remshift;
		z->bn_digit[i] = (digit)(accum & MASK);
		accum >>= SHIFT;
	}
	if (remshift)
		z->bn_digit[newsize-1] = (digit)accum;
	else	
		assert(!accum);
	return (PyObject *) bignum_Normalize(z);
}


/* Bitwise and/xor/or operations */

#define MAX(x, y) ((x) < (y) ? (y) : (x))
#define MIN(x, y) ((x) > (y) ? (y) : (x))

static PyObject *long_bitwise (ilubignum_Value , int, ilubignum_Value );
static PyObject *
long_bitwise(a, op, b)
	ilubignum_Value a;
	int op; /* '&', '|', '^' */
	ilubignum_Value b;
{
	digit maska, maskb; /* 0 or MASK */
	int negz;
	int size_a, size_b, size_z;
	ilubignum_Value z;
	int i;
	digit diga, digb;
	PyObject *v;
	
	if (a->bn_ndigits < 0) {
		a = (ilubignum_Value ) long_invert(a);
		maska = MASK;
	}
	else {
		Py_INCREF(a);
		maska = 0;
	}
	if (b->bn_ndigits < 0) {
		b = (ilubignum_Value ) long_invert(b);
		maskb = MASK;
	}
	else {
		Py_INCREF(b);
		maskb = 0;
	}
	
	size_a = a->bn_ndigits;
	size_b = b->bn_ndigits;
	size_z = MAX(size_a, size_b);
	z = _ilubignum_New(size_z);
	if (a == NULL || b == NULL || z == NULL) {
		Py_XDECREF(a);
		Py_XDECREF(b);
		Py_XDECREF(z);
		return NULL;
	}
	
	negz = 0;
	switch (op) {
	case '^':
		if (maska != maskb) {
			maska ^= MASK;
			negz = -1;
		}
		break;
	case '&':
		if (maska && maskb) {
			op = '|';
			maska ^= MASK;
			maskb ^= MASK;
			negz = -1;
		}
		break;
	case '|':
		if (maska || maskb) {
			op = '&';
			maska ^= MASK;
			maskb ^= MASK;
			negz = -1;
		}
		break;
	}
	
	for (i = 0; i < size_z; ++i) {
		diga = (i < size_a ? a->bn_digit[i] : 0) ^ maska;
		digb = (i < size_b ? b->bn_digit[i] : 0) ^ maskb;
		switch (op) {
		case '&': z->bn_digit[i] = diga & digb; break;
		case '|': z->bn_digit[i] = diga | digb; break;
		case '^': z->bn_digit[i] = diga ^ digb; break;
		}
	}
	
	Py_DECREF(a);
	Py_DECREF(b);
	z = bignum_Normalize(z);
	if (negz == 0)
		return (PyObject *) z;
	v = long_invert(z);
	Py_DECREF(z);
	return v;
}

static PyObject *
long_and(a, b)
	ilubignum_Value a;
	ilubignum_Value b;
{
	return long_bitwise(a, '&', b);
}

static PyObject *
long_xor(a, b)
	ilubignum_Value a;
	ilubignum_Value b;
{
	return long_bitwise(a, '^', b);
}

static PyObject *
long_or(a, b)
	ilubignum_Value a;
	ilubignum_Value b;
{
	return long_bitwise(a, '|', b);
}

#endif
