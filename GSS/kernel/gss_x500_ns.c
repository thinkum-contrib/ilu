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

$Id: gss_x500_ns.c,v 1.8 1999/08/03 01:59:34 janssen Exp $
*/
/*
 * gss_x500_ns.c -- implementation of GSS X.500 namespace routines
 *
 * Antony Courtney,	15/8/95
 */

#include <assert.h>
#include <stdio.h>
#include <gssapi.h>

#include "ilugss_impl.h"
#include "ilugss_x500_ns.h"

static gss_oidtbl_t x500_ns_tbl=NULL;

/* ilugss_x500_register_ns() -- register an X.500 namespace; should be called once
 * by implementation of namespaces in the X.500 family of namespaces
 */
void ilugss_x500_register_ns(gss_OID x500_ns_oid,ilugss_x500_namespace_t *ns, char *name)
{
     ilugss_register_namespace (&(ns->head), name);
     if (x500_ns_tbl==NULL) {
	  x500_ns_tbl=_ilugss_oidtbl_create();
     }
     _ilugss_oidtbl_insert(x500_ns_tbl,ns->head.oid,ns);
}

/* Entry points for invoking methods of X.500 names, for use by security
 * mechanisms which specifically require X.500 names:
 */

/* ilugss_x500_name_is_x500() -- returns TRUE if input_name is drawn from a
 * namespace which is a member of the X.500 family of namespaces
 */
int ilugss_x500_name_is_x500(gss_name_t input_name)
{
     if (x500_ns_tbl==NULL) {
	  return 0;
     }
     if (_ilugss_oidtbl_find(x500_ns_tbl,gss_name_ns(input_name)->oid)==NULL) {
	  return 0;
     }
     return 1;
}

/* ilugss_x500_asn1_encode_name() -- encode an X.500 name in ASN.1
 * raises: GSS_S_BAD_NAMETYPE
 */
OM_uint32 ilugss_x500_asn1_encode_name
(OM_uint32 *minor_status,
 gss_name_t input_name,
 gss_buffer_t output_buffer
 )
{
     ilugss_x500_namespace_t *ns;
     
     *minor_status=0;
     output_buffer->length=0;
     /* a pretty heavy-weight assert(): */
     assert(ilugss_x500_name_is_x500(input_name));
     ns=(ilugss_x500_namespace_t *) gss_name_ns(input_name);
     return (ns->asn1_encode)(minor_status,gss_name_opaque(input_name),
			      output_buffer);
}

/* ilugss_x500_asn1_decode_name() -- decode an ASN.1 buffer which contains
 * an encoded form of an X.500 name in a particular X.500 namespace
 * raises: GSS_S_BAD_NAMETYPE, GSS_S_BAD_NAME
 */
OM_uint32 ilugss_x500_asn1_decode_name
(OM_uint32 *minor_status,
 gss_buffer_t input_buffer,
 gss_OID x500_ns_oid,		/* OID of X.500 namespace to use */
 gss_name_t *output_name
 )
{
     OM_uint32 retcode;
     ilugss_x500_namespace_t *ns;
     gss_impl_name_t *name_impl;

     *minor_status=0;
     *output_name=NULL;
     if (x500_ns_tbl==NULL) {
	  return GSS_S_BAD_NAMETYPE;
     }
     if ((ns=_ilugss_oidtbl_find(x500_ns_tbl,x500_ns_oid))==NULL) {
	  return GSS_S_BAD_NAMETYPE;
     }
     name_impl=ilugss_malloc(sizeof(gss_impl_name_t));
     name_impl->ns=(gss_namespace_t *) ns;
     name_impl->ns_name = NULL;
     name_impl->refcount = 1;
     *output_name=name_impl;
     retcode=(ns->asn1_decode)(minor_status,input_buffer,
			       &(name_impl->ns_name));
     return retcode;
}
