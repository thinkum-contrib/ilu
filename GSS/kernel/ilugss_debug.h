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

$Id: ilugss_debug.h,v 1.5 1999/08/03 01:59:30 janssen Exp $
*/
#ifndef DEBUG_H
#define DEBUG_H 1
/*
 * debug.h -- definitions for general debugging / tracing routines for
 *	      GSS API shell
 *
 * Antony Courtney, 8/5/95
 * (discretely pilfered from code used for other projects)
 */

#include <stdio.h>
#include <errno.h>

/* a re-definition of strerror() for silly, non-ANSI systems (like Suns!) */
char *_ilugss_sys_strerror(int errnum);

/* debugging routine...*/
void ilugss_debug(unsigned level,...);

#define DEBUG	ilugss_debug

/* packet dump routine */
void ilugss_dump_packet(void *buf,unsigned len);

/* buffer version of gss_dump_packet */
#define _ilugss_dump_buf(b)	ilugss_dump_packet((b)->value,(b)->length)
#define ilugss_dump_buf(b)	ilugss_dump_packet((b)->value,(b)->length)

/* different types of debugging levels */
#define PACKET_DB	0x0001		/* see packets */
#define INIT_DB		0x0002		/* see context initialisation */
#define PROTO_DB	0x0008		/* interp packet contents */
#define ERR_DB		0x0010		/* errors */
#define CALL_DB		0x0020		/* GSS API call traces */
#endif	/* DEBUG_H */

