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

$Id: gss_ext.c,v 1.9 1999/08/03 01:59:37 janssen Exp $
*/

/* GSS API extensions -- THESE ARE NOT PART OF THE GSS STANDARD, AND
 * THEREFORE SHOULD NOT BE USED IF YOU CARE ABOUT PORTABILITY TO OTHER
 * GSS IMPLEMENTATIONS.
 */

#include <gssapi.h>
#include <string.h>
#include "ilugss_util.h"
#include "ilugss_asn1.h"

static struct gss_descs *_gss_namespaces_by_name = NULL;
static struct gss_descs *_gss_schemes_by_name = NULL;

struct gss_descs {
  const char *	name;
  gss_OID	desc;
  struct gss_descs *next;
};

OM_uint32
  ilu_gssext_add_scheme_by_name (OM_uint32 *minor, char *name, gss_OID oid)
{
  struct gss_descs *n;

  n = (struct gss_descs *) ilugss_malloc(sizeof(struct gss_descs));
  n->name = (const char *) ilugss_strdup(name);
  n->desc = oid;
  n->next = _gss_schemes_by_name;
  _gss_schemes_by_name = n;
  return (GSS_S_COMPLETE);
}

OM_uint32
  ilu_gssext_find_scheme_by_name (OM_uint32 *minor, char *name, /* OUT */ gss_OID *desc)
{
  struct gss_descs *p;

  if (minor)
    *minor = 0;
  for (p = _gss_schemes_by_name;  p != NULL;  p = p->next)
    if (strcmp(name, p->name) == 0)
      {
	*desc = p->desc;
	return (GSS_S_COMPLETE);
      }
  return (GSS_S_FAILURE);
}

OM_uint32
  ilu_gssext_add_namespace_by_name (OM_uint32 *minor, char *name, gss_OID oid)
{
  struct gss_descs *n;

  n = (struct gss_descs *) ilugss_malloc(sizeof(struct gss_descs));
  n->name = (const char *) ilugss_strdup(name);
  n->desc = oid;
  n->next = _gss_namespaces_by_name;
  _gss_namespaces_by_name = n;
  return (GSS_S_COMPLETE);
}

OM_uint32
  ilu_gssext_find_namespace_by_name (OM_uint32 *minor, char *name, /* OUT */ gss_OID *desc)
{
  struct gss_descs *p;

  if (minor)
    *minor = 0;
  for (p = _gss_namespaces_by_name;  p != NULL;  p = p->next)
    if (strcmp(name, p->name) == 0)
      {
	*desc = p->desc;
	return (GSS_S_COMPLETE);
      }
  return (GSS_S_FAILURE);
}

/* ilu_gssext_insert_mech_oid() -- insert GSS-mandated mechanism object-identifier
 *			    into initial context token received from remote
 *			    peer, so that it may be passed to
 *			    gss_accept_sec_context()
 */
OM_uint32 ilu_gssext_insert_mech_oid
(OM_uint32 *minor_status,
 gss_buffer_t input_buffer,
 gss_OID mech_oid,
 gss_buffer_t output_buffer
)
{
     *minor_status=0;
     output_buffer->length = input_buffer->length;
     output_buffer->value = (void *) ilugss_malloc(output_buffer->length);
     memcpy(output_buffer->value,input_buffer->value,output_buffer->length);

     _ilugss_asn1_wrap_oid(mech_oid,output_buffer);
     return GSS_S_COMPLETE;
}

/* ilu_gssext_strip_initial_token() -- strip GSS-mandated object-identifier from
 * 				initial token, for wire-compatibility with
 *				non-GSS based protocols
 *
 * N.B. The original token_buffer is assumed to have been allocated by the
 * GSS implementation; the original storage is released and new storage is
 * allocated
 */
OM_uint32 ilu_gssext_strip_initial_token
(OM_uint32 *minor_status,
 gss_buffer_t token_buffer
)
{
  *minor_status=0;
  (void) _ilugss_asn1_extract_oid(token_buffer);
  return GSS_S_COMPLETE;
}
 

