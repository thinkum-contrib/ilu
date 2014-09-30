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

$Id: bignum.h,v 1.9 1999/08/03 01:59:59 janssen Exp $
*/
#ifndef _ILU_BIGNUMPRV_H_
#define _ILU_BIGNUMPRV_H_
#ifdef __cplusplus
extern "C" {
#endif

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

/* The following are exported for the use of the optional Bignum
   library, which provides Bignum support for languages otherwise
   lacking it.
*/

/* Parameters of the long integer representation.
   These shouldn't have to be changed as C should guarantee that a short
   contains at least 16 bits, but it's made changeable any way.
   Note: 'digit' should be able to hold 2*MASK+1, and 'twodigits'
   should be able to hold the intermediate results in 'mul'
   (at most MASK << SHIFT).
   Also, x_sub assumes that 'digit' is an unsigned type, and overflow
   is handled by taking the result mod 2**N for some N > SHIFT.
   And, at some places it is assumed that MASK fits in an int, as well. */

typedef ilubignum_card16 digit;
typedef unsigned int wdigit; /* digit widened to parameter size */
typedef ilubignum_card32 twodigits;
typedef ilubignum_int32 stwodigits; /* signed variant of twodigits */

#define SHIFT	15
#define BASE	((digit)1 << SHIFT)
#define MASK	((int)(BASE - 1))

/* Long integer representation.
   The absolute value of a number is equal to
   	SUM(for i=0 through abs(bn_ndigits)-1) bn_digit[i] * 2**(SHIFT*i)
   Negative numbers are represented with bn_ndigits < 0;
   zero is represented by bn_ndigits == 0.
   In a normalized number, bn_digit[abs(bn_ndigits)-1] (the most significant
   digit) is never zero.  Also, in all cases, for all valid i,
   	0 <= bn_digit[i] <= MASK.
   The allocation fuction takes care of allocating extra memory
   so that bn_digit[0] ... bn_digit[abs(bn_ndigits)-1] are actually available. */

struct _ilubignum_Value_s {
  ilubignum_int32 bn_ndigits;
  digit bn_digit[1];
};

ilubignum_Value
  _ilubignum_New(int size, char **err);
/* Allocate a new long int object with size digits.
   Return NULL and set exception if we run out of memory. */

#ifdef __cplusplus
}
#endif
#endif /* !_ILU_BIGNUMPRV_H_ */
