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
*/
/*
*/
/* $Id: ilubasic.h,v 1.19 1999/08/03 01:52:55 janssen Exp $ */
/* $Locker:  $ */
/* Last tweaked by Mike Spreitzer April 30, 1998 12:10 pm PDT */

#ifdef __cplusplus
extern "C" {
#endif

#ifndef _ILUBASIC_
#define _ILUBASIC_

#ifdef WIN16
/* make sure booleans are 32 bits on win16  */
#define SIZEOF_SHORT 2
#define SIZEOF_INT   2
#define SIZEOF_LONG  4
#define SIZEOF_CHAR  1
#endif

#ifdef WIN32
#define SIZEOF_SHORT 2
#define SIZEOF_INT   4
#define SIZEOF_LONG  4
#define SIZEOF_CHAR  1
#endif

#ifdef __cplusplus
#define ILU_NIL	0
#else
#define ILU_NIL ((void*)0)
#endif
/* Sane programmers will use this instead of NULL. */

/* figure out sizes for ints */
#if (SIZEOF_SHORT != 2)
#if (SIZEOF_INT != 2)
#if (SIZEOF_CHAR != 2)
#error "Can't figure a good type for INT16_TYPE!"
#else
#define INT16_TYPE	signed char
#endif /* CHAR */
#else
#define INT16_TYPE	int
#endif /* INT */
#else
#define INT16_TYPE	short
#endif /* SHORT */

#if (SIZEOF_INT != 4)
#if (SIZEOF_SHORT != 4)
#if (SIZEOF_LONG != 4)
#error "Can't figure a good type for INT32_TYPE!"
#else
#define INT32_TYPE	long int
#endif /* LONG */
#else
#define INT32_TYPE	short int
#endif /* SHORT */
#else
#define INT32_TYPE	int
#endif /* INT */

/* test enums to see whether to make ilu_boolean an enum */
#if (defined( __cplusplus ) && defined( macintosh )) || (SIZEOF_ENUM != 4)
typedef unsigned INT32_TYPE ilu_boolean;
#define ilu_TRUE  ((ilu_boolean) 1)
#define	ilu_FALSE ((ilu_boolean) 0)
#else
typedef enum ilu_boolean_enum { ilu_TRUE = 1, ilu_FALSE = 0 } ilu_boolean;
#endif

typedef unsigned INT32_TYPE ilu_cardinal;
typedef unsigned INT16_TYPE ilu_shortcardinal;
/* figure out how to represent 64-bit cardinals */
#ifdef LONG_CARDINAL_TYPE
typedef LONG_CARDINAL_TYPE ilu_longcardinal;
#ifdef WORDS_BIGENDIAN
#define ILU_LONGCARD_HIGH_WORD(lc)	(((ilu_cardinal *)(lc))[0])
#define ILU_LONGCARD_LOW_WORD(lc)	(((ilu_cardinal *)(lc))[1])
#else
#define ILU_LONGCARD_HIGH_WORD(lc)	(((ilu_cardinal *)(lc))[1])
#define ILU_LONGCARD_LOW_WORD(lc)	(((ilu_cardinal *)(lc))[0])
#endif /* WORDS_BIGENDIAN */
#else
typedef struct {ilu_cardinal high, low;} ilu_longcardinal;
#define ILU_LONGCARD_HIGH_WORD(lc)	((lc)->high)
#define ILU_LONGCARD_LOW_WORD(lc)	((lc)->low)
#endif /* def LONG_CARDINAL_TYPE */

typedef INT16_TYPE ilu_shortinteger;
typedef INT32_TYPE ilu_integer;
/* figure out how to represent 64-bit ints */
#ifdef LONG_INTEGER_TYPE
typedef LONG_INTEGER_TYPE ilu_longinteger;
#ifdef WORDS_BIGENDIAN
#define ILU_LONGINT_HIGH_WORD(lc)	(((ilu_integer *)(lc))[0])
#define ILU_LONGINT_LOW_WORD(lc)	(((ilu_cardinal *)(lc))[1])
#else
#define ILU_LONGINT_HIGH_WORD(lc)	(((ilu_integer *)(lc))[1])
#define ILU_LONGINT_LOW_WORD(lc)	(((ilu_cardinal *)(lc))[0])
#endif /* WORDS_BIGENDIAN */
#else
typedef struct { ilu_integer high; ilu_cardinal low;} ilu_longinteger;
#define ILU_LONGINT_HIGH_WORD(lc)	((lc)->high)
#define ILU_LONGINT_LOW_WORD(lc)	((lc)->low)
#endif /* def LONG_INTEGER_TYPE */

typedef double ilu_real;
typedef float ilu_shortreal;

/*	ILU long real  (Sun "quadruple-precision" real)

The Sun standard defines the encoding for a quadruple-precision
floating-point data type (128 bits or 16 bytes), and follows the
general form of the IEEE standard for normalized floating-point
numbers [3].  That standard defines the following three fields, which
describe the floating-point number:

      S: The sign of the number.  Values 0 and 1 represent positive and
         negative, respectively.  One bit.

      E: The exponent of the number, base 2.  15 bits are devoted to
         this field.  The exponent is biased by 16383.

      F: The fractional part of the number's mantissa, base 2.  112 bits
         are devoted to this field.

   Therefore, the floating-point number is described by:

         (-1)**S * 2**(E-Bias) * 1.F

   It is declared as follows:

         quadruple identifier;

         +------+------+------+------+------+------+-...--+------+
         |byte 0|byte 1|byte 2|byte 3|byte 4|byte 5| ...  |byte15|
         S|    E       |                  F                      |
         +------+------+------+------+------+------+-...--+------+
         1|<----15---->|<-------------112 bits------------------>|
         <-----------------------128 bits------------------------>
                                      QUADRUPLE-PRECISION FLOATING-POINT

   Just as the most and least significant bytes of a number are 0 and 3,
   the most and least significant bits of a quadruple-precision floating-
   point number are 0 and 127.  The beginning bit (and most significant
   bit) offsets of S, E , and F are 0, 1, and 16, respectively.  Note
   that these numbers refer to the mathematical positions of the bits,
   and NOT to their actual physical locations (which vary from medium to
   medium).

   The IEEE specifications should be consulted concerning the encoding
   for signed zero, signed infinity (overflow), and denormalized numbers
   (underflow) [3].  According to IEEE specifications, the "NaN" (not a
   number) is system dependent and should not be interpreted within XDR
   as anything other than "NaN".

   [3]  "IEEE Standard for Binary Floating-Point Arithmetic", ANSI/IEEE
        Standard 754-1985, Institute of Electrical and Electronics
        Engineers, August 1985.
*/
#ifdef LONG_REAL_TYPE
typedef LONG_REAL_TYPE ilu_longreal;
#else
typedef struct {unsigned char b[16];} ilu_longreal;
#endif /* def LONG_REAL_TYPE */

typedef unsigned char ilu_byte;
typedef ilu_byte * ilu_opaque;
typedef ilu_byte * ilu_bytes;

typedef char ilu_shortcharacter;
typedef ilu_shortcharacter * ilu_string;

typedef ilu_shortcardinal ilu_character;
typedef ilu_character * ilu_wstring;

/*  ILU character  (16-bit Unicode)

[Internation Standards Organization]; DRAFT INTERNATIONAL STANDARD -- 
INFORMATION TECHNOLOGY -- UNIVERSAL CODED CHARACTER SET (UCS); ISO, ??, 1990, 
<ISO/IEC DIS 10646>.

*/

typedef void *ilu_refany;
typedef void *ilu_private;

#endif
#ifdef __cplusplus
}
#endif
