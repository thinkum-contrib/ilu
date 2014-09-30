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

$Id: ilugss_x500_ns.h,v 1.4 1999/08/03 01:59:38 janssen Exp $
*/
#ifndef GSS_X500_NS_H
#define GSS_X500_NS_H 1
/*
 * gss_x500_ns.h -- definition of data type which defines entry points
 *		    into an implementation of a particular member of the
 *		    X.500 family of namespaces
 *
 * Antony Courtney,	8/1/95
 */

#include "ilugss_namespace.h"

/* ilugss_x500_namespace_t -- extends namespace by providing methods for
 * encoding and decoding names as ASN.1
 */
typedef struct ilugss_x500_namespace_s {
     gss_namespace_t head;

     OM_uint32 (*asn1_encode)(OM_uint32 *minor_status,
			      gss_opaque_t input_name,
			      gss_buffer_t output_buffer);
     
     OM_uint32 (*asn1_decode)(OM_uint32 *minor_status,
			      gss_buffer_t input_buffer,
			      gss_opaque_t *output_name);

} ilugss_x500_namespace_t;

/* The following is made available for implementors of new X.500 namespaces;
 * Implementors of such namespaces are expected, at a minimum, to override
 * the object identifier which identifies the default X.500 namespace.
 */
extern const ilugss_x500_namespace_t *ilugss_default_x500_namespace;

/* ilugss_x500_register_ns() -- register an X.500 namespace; should be called once
 * by implementation of namespaces in the X.500 family of namespaces
 */
void ilugss_x500_register_ns(gss_OID x500_ns_oid,ilugss_x500_namespace_t *ns, char *name);

/* Entry points for invoking methods of X.500 names, for use by security
 * mechanisms which specifically require X.500 names:
 */

/* ilugss_x500_name_is_x500() -- returns TRUE if input_name is drawn from a
 * namespace which is a member of the X.500 family of namespaces
 */
int ilugss_x500_name_is_x500(gss_name_t input_name);

/* ilugss_x500_asn1_encode_name() -- encode an X.500 name in ASN.1
 * raises: GSS_S_BAD_NAMETYPE
 */
OM_uint32 ilugss_x500_asn1_encode_name
(OM_uint32 *minor_status,
 gss_name_t input_name,
 gss_buffer_t output_buffer
 );

/* ilugss_x500_asn1_decode_name() -- decode an ASN.1 buffer which contains
 * an encoded form of an X.500 name in a particular X.500 namespace
 * raises: GSS_S_BAD_NAMETYPE, GSS_S_BAD_NAME
 */
OM_uint32 ilugss_x500_asn1_decode_name
(OM_uint32 *minor_status,
 gss_buffer_t input_buffer,
 gss_OID x500_ns_oid,		/* OID of X.500 namespace to use */
 gss_name_t *output_name
 );

#endif /* GSS_X500_NS_H */
