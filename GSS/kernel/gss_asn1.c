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

$Id: gss_asn1.c,v 1.9 1999/08/03 01:59:28 janssen Exp $
*/
/*
 * gss_asn1.c -- implementation of ASN.1 decoding routines for GSS
 * 
 * Antony Courtney,	14/7/95
 */

#include <gssapi.h>
#include <string.h>
#include <assert.h>
#include "ilugss_asn1.h"

#include "ilugss_impl.h"
#include "ilugss_util.h"

/* These routines are a total fabrication right now -- they do a sensible,
 * simple encoding of the OID, instead following the real DER/BER encoding
 * rules
 * In this format, an OID is just a 1-byte length followed by the bytes of
 * the OID.
 */

/* gss_asn1_extract_oid() -- extract OID from start of a packet
 * returns: a pointer to an OID; modifies token_buffer to contain contents
 * of packet with the OID removed from start
 */
gss_OID _ilugss_asn1_extract_oid(gss_buffer_t token_buffer)
{
     OM_uint32 minor_status;
     gss_OID oidp;
     unsigned char *ucp;
     gss_buffer_desc result_buffer;

     oidp=ilugss_malloc(sizeof(gss_OID_desc));
     ucp=token_buffer->value;
     oidp->length=*ucp++;
     oidp->elements=ilugss_malloc(oidp->length);
     memcpy(oidp->elements,ucp,oidp->length);
     ucp += oidp->length;

     result_buffer.length=token_buffer->length - oidp->length - 1;
     result_buffer.value=ilugss_malloc(result_buffer.length);
     memcpy(result_buffer.value,ucp,result_buffer.length);
     
     *token_buffer=result_buffer;

     return oidp;
}

/* gss_asn1_wrap_oid() -- creates a new packet with OID inserted at the
 * front; uses single buffer for both input and output -- automatically
 * releases the buffer passed in by calling gss_release_buffer
 */
void _ilugss_asn1_wrap_oid(gss_OID oid,gss_buffer_t token_buffer)
{
     OM_uint32 minor_status;
     gss_buffer_desc result_buffer;
     unsigned char *bufp;

     result_buffer.length=token_buffer->length + oid->length + 1;
     result_buffer.value=ilugss_malloc(result_buffer.length);
     
     assert(oid->length < 256);
     bufp=result_buffer.value;
     *bufp++=(unsigned char) oid->length;
     memcpy(bufp,oid->elements,oid->length);
     bufp += oid->length;
     memcpy(bufp,token_buffer->value,token_buffer->length);

     /* now free original token buffer */
     gss_release_buffer(&minor_status,token_buffer);
     *token_buffer=result_buffer;
}
