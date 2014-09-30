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

$Id: ilugss_opaque.h,v 1.7 1999/08/03 01:59:32 janssen Exp $
*/
#ifndef ILUGSS_OPAQUE_H
#define ILUGSS_OPAQUE_H 1


/*
 * ilugss_opaque.h -- partial reification of opaque data types declared in
 *		   gssapi.h
 */

#include <gssapi.h>

/* gss_opaque_t -- opaque pointer type which is managed by an underlying
 * security scheme or namespace, never directly touched by GSS shell
 */
typedef void *gss_opaque_t;

/* concrete versions of "opaque" GSS API types, with sufficient context
 * information for the generic shell API to be able to locate the scheme
 * which deals with the datum
 */

/* credentials: */
typedef struct gss_impl_cred_id_s {
     struct gss_scheme_s *scheme;
     gss_opaque_t scheme_cred_id;
} gss_impl_cred_id_t;

#define gss_cred_scheme(cid)	(((gss_impl_cred_id_t *) cid)->scheme)
#define gss_cred_opaque(cid)	(((gss_impl_cred_id_t *) cid)->scheme_cred_id)

/* context: */
typedef struct gss_impl_ctx_id_s {
     struct gss_scheme_s *scheme;
     gss_opaque_t scheme_ctx_id;
} gss_impl_ctx_id_t;

#define gss_ctx_scheme(cid)	(((gss_impl_ctx_id_t *) cid)->scheme)
#define gss_ctx_opaque(cid)	(((gss_impl_ctx_id_t *) cid)->scheme_ctx_id)


/* name: */
typedef struct gss_impl_name_s {
     struct gss_namespace_s *ns;
     gss_opaque_t ns_name;
     unsigned int refcount;
} gss_impl_name_t;

#define gss_name_ns(nm)		(((gss_impl_name_t *) nm)->ns)
#define gss_name_opaque(nm)	(((gss_impl_name_t *) nm)->ns_name)
#define gss_name_refcount(nm)	(((gss_impl_name_t *) nm)->refcount)

#endif	/* ILUGSS_OPAQUE_H */
