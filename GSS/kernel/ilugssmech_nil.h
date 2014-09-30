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

$Id: ilugssmech_nil.h,v 1.7 1999/08/03 01:59:39 janssen Exp $
*/
#ifndef ILUGSSMECH_NIL_H
#define ILUGSSMECH_NIL_H 1

#ifdef __cplusplus
extern "C" {
#endif

/*
 * gss_nil_scheme.c -- implementation of "nil" security layer for GSS
 */

/* OID is { iso (1) member-body (2) US (840) Xerox (113550) ILU (9) GSS (1) nil-mech (3) } */

#define ILUGSSMECH_NIL_OID 	{ 9, "\x2a\x86\x48\x86\xf7\x0e\x09\x01\x03" }

GSS_PUBLIC gss_OID_desc ilugssmech_nil_OID[];

GSS_PUBLIC void ilugssmech_nil_initialize(void);

#ifdef __cplusplus
}
#endif

#endif /* ILUGSSMECH_NIL_H */              
