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

$Id: ilubnops.h,v 1.5 1999/08/03 01:59:58 janssen Exp $
*/
#ifndef _ILU_BIGNUM2_H_
#define _ILU_BIGNUM2_H_
#ifdef __cplusplus
extern "C" {
#endif

#include <ilubignm.h>

/* Create a new long int object from a C long int */
extern ilubignum_Value
  ilubignum_FromInteger(ilubignum_MaxIntType ival, char **err);

/* Get a C long int from a long int object.
   Returns -1 and sets an error condition if overflow occurs. */
extern ilubignum_MaxIntType
  ilubignum_AsInteger(ilubignum_Value vv, char **err);

/* Create a new long int object from a C unsigned long int */
extern ilubignum_Value
ilubignum_FromCardinal(ilubignum_MaxCardType ival, char **err);

/* Get a C long int from a long int object.
   Returns -1 and sets an error condition if overflow occurs. */
extern ilubignum_MaxCardType
  ilubignum_AsCardinal(ilubignum_Value v, char **err);

/* Create a new long int object from a C double */
extern ilubignum_Value
  ilubignum_FromReal(double dval, char **err);

/* Get a C double from a long int object. */
extern double
  ilubignum_AsReal(ilubignum_Value v, char **err);

/* Scan a string to get a bignum */
extern ilubignum_Value
  ilubignum_FromString(char *str, char **pend, int base, char **err);

/* Convert a long int object to a string, using a given conversion base.
   Return a string object.
   If base is 8 or 16, add the proper prefix '0' or '0x'.
   External linkage: used in bltinmodule.c by hex() and oct(). */
extern char *
  ilubignum_AsString (ilubignum_Value aa, int base, char **err);

#ifdef __cplusplus
}
#endif
#endif /* !_ILU_BIGNUM2_H_ */
