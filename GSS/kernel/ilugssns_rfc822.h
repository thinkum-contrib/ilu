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

$Id: ilugssns_rfc822.h,v 1.9 1999/08/03 01:59:37 janssen Exp $
*/
#ifndef ILUGSSNS_RFC822_H
#define ILUGSSNS_RFC822_H

#ifdef __cplusplus
extern "C" {
#endif

/*
 * ilugssns_rfc822.h -- internal data structures for representing RFC 822 style
 *		  names
 *
 * Antony Courtney,	14/7/95
 */

/* OID is { iso (1) member-body (2) US (840) Xerox (113550) ILU (9) GSS (1) rfc822-namespace (4) } */

#define ILUGSSNS_RFC822_OID	{ 9, "\x2a\x86\x48\x86\xf7\x0e\x09\x01\x04" }

GSS_PUBLIC gss_OID_desc ilugssns_rfc822_OID[];

typedef struct ilugssns_rfc822_name_s {
     char *rname;	/* user's real name */
     char *uname;	/* user name */
     char *dname;	/* domain name */
} ilugssns_rfc822_name_t;

/* initialize routine: */
GSS_PUBLIC void ilugssns_rfc822_initialize(void);

/* RFC-822 specific minor status codes: */

/* malformed names: */
#define ILUGSSNS_RFC822_INVALID_SPACE	1	/* whitespace occurred in a
						   name where none was
						   allowed   */
#define ILUGSSNS_RFC822_MULTIPLE_AT		2	/* multiple "@" delimiters */
#define ILUGSSNS_RFC822_NO_RDELIM		3	/* missing right-hand
						   delimiter */
#define ILUGSSNS_RFC822_MALFORMED		4	/* malformed name */

#ifdef __cplusplus
}
#endif

#endif /* ndef ILUGSSNS_RFC822_H */
