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

$Id: ilugss_asn1.h,v 1.5 1999/08/03 01:59:29 janssen Exp $
*/
#ifndef _ILUGSS_ASN1_H
#define _ILUGSS_ASN1_H 1
/*
 * gss_asn1.h -- interface to routines for doing ASN.1 encoding / decoding
 *		 from elsewhere in the GSS
 * 
 * Antony Courtney,	7/7/95
 */

/* _ilugss_asn1_extract_oid() -- extract OID from start of a packet
 * returns: a pointer to an OID; modifies token_buffer to contain contents
 * of packet with the OID removed from start
 */
gss_OID _ilugss_asn1_extract_oid(gss_buffer_t token_buffer);

/* _ilugss_asn1_wrap_oid() -- creates a new packet with OID inserted at the
 * front; uses single buffer for both input and output
 */
void _ilugss_asn1_wrap_oid(gss_OID oid,gss_buffer_t token_buffer);

#endif	/* _ILUGSS_ASN1_H */
