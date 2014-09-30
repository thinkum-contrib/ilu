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

$Id: anon_ns.c,v 1.6 1999/08/03 01:59:31 janssen Exp $
*/
/*
 * rfc822_ns.c -- implementation of rfc 822 as a GSS namespace
 *
 * Antony Courtney,	14/7/95
 */

#include <gssapi.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>

#include "ilugss_opaque.h"
#include "ilugss_namespace.h"
#include "ilugss_oidtbl.h"
#include "ilugssns_anonymous.h"
#include "ilugss_util.h"
#include "ilugss_impl.h"

gss_OID_desc ilugssns_anonymous_OID[] = { ILUGSSNS_ANONYMOUS_OID };

static OM_uint32 import_name(OM_uint32 *minor_status,
			     gss_buffer_t input_name_buffer,
			     gss_opaque_t *output_name);

static OM_uint32 display_name(OM_uint32 *minor_status,
			      gss_opaque_t input_name,
			      gss_buffer_t output_name_buffer);

static OM_uint32 compare_name(OM_uint32 *minor_status,
			      gss_opaque_t name1,
			      gss_opaque_t name2,
			      int *name_equal);

static OM_uint32 release_name(OM_uint32 *minor_status,
			      gss_opaque_t input_name);

static gss_namespace_t namespace={
     ilugssns_anonymous_OID,
     compare_name,
     display_name,
     import_name,
     release_name
};

/* initialize routine: */
void ilugssns_anonymous_initialize(void)
{
  ilugss_register_namespace (&namespace, "anonymous");
}

static const char *theName = "<* anonymous *>";
static const int theNameLen = 15;

static OM_uint32 display_name(OM_uint32 *minor_status,
			      gss_opaque_t input_name,
			      gss_buffer_t output_name_buffer)
{
  int displen=0;

  if (input_name != theName)
    return GSS_S_BAD_NAME;

  output_name_buffer->value = ilugss_malloc(theNameLen);
  output_name_buffer->length = theNameLen;
  strncpy (output_name_buffer->value, theName, theNameLen);
  return GSS_S_COMPLETE;
}

static OM_uint32 compare_name(OM_uint32 *minor_status,
			      gss_opaque_t name1,
			      gss_opaque_t name2,
			      int *name_equal)
{
  if ((name1 != theName) || (name2 != theName))
    return GSS_S_BAD_NAME;

  *name_equal = 1;
  return GSS_S_COMPLETE;
}
	  
/* import_name
 */
static OM_uint32 import_name(OM_uint32 *minor_status,
			     gss_buffer_t input_name_buffer,
			     gss_opaque_t *output_name)
{
  if (input_name_buffer->length == 15 &&
      strncmp((char *) input_name_buffer->value, theName, theNameLen) == 0)
    {
      *output_name = (gss_opaque_t) theName;
      return GSS_S_COMPLETE;
    }
  else
    return GSS_S_BAD_NAME;
}

static OM_uint32 release_name(OM_uint32 *minor_status,
			      gss_opaque_t input_name)
{
  *minor_status = 0;
  if (input_name != theName)
    return GSS_S_BAD_NAME;
  else
    return GSS_S_COMPLETE;
}

gss_name_t ilugssns_anonymous_default_name (void)
{
  struct gss_impl_name_s *p = ilugss_malloc(sizeof(*p));
  if (p == NULL)
    return NULL;
  p->ns = &namespace;
  p->ns_name = (gss_opaque_t) theName;
  p->refcount = 1;
  return p;
}
