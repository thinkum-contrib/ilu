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

$Id: ilugssns_anonymous.h,v 1.7 1999/08/03 01:59:35 janssen Exp $
*/
#ifndef ILUGSSNS_ANONYMOUS_H
#define ILUGSSNS_ANONYMOUS_H

#ifdef __cplusplus
extern "C" {
#endif

/*
 * ilugssns_anonymous.h -- internal data structures for representing GSS anonymous names
 */

/* OID is {2(joint-iso-ccitt) 16(country) 840(US(ANSI)) 1(organizations) 113687(OpenVision) 1(security) 2(nametypes) 1(anonymous-name)} */

#define ILUGSSNS_ANONYMOUS_OID	{10, "\x60\x86\x48\x01\x86\xf8\x17\x01\x02\x01"}

GSS_PUBLIC gss_OID_desc ilugssns_anonymous_OID[];

/* initialize routine: */
GSS_PUBLIC void ilugssns_anonymous_initialize(void);

/* fetch the only name in the namespace */
GSS_PUBLIC gss_name_t ilugssns_anonymous_default_name(void);

#ifdef __cplusplus
}
#endif

#endif /* ndef ILUGSSNS_RFC822_H */
