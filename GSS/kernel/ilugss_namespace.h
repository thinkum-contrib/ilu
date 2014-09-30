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

$Id: ilugss_namespace.h,v 1.8 1999/08/03 01:59:39 janssen Exp $
*/
#ifndef GSS_NAMESPACE_H
#define GSS_NAMESPACE_H 1
/*
 * gss_namespace.h -- definition of data type which defines entry points
 *			into an implementation of a particular namespace
 *			for gss_name_t's
 *
 */

/*
 * gss_namespace_t -- An implementation of routines required by GSS to
 * support names in a particular namespace;  Each separate namespace must
 * provide all of the routines defined here
 */
typedef struct gss_namespace_s {

     /* OID of this namespace: */
     gss_OID oid;

     /* The following routines are analogues of gss_compare_name(),
	gss_display_name(), gss_import_name(), and gss_release_name(),
	with changes as noted below.  You are referred to the C mapping
	for the GSS API, for the exact semantics of each routine. */

     /* changes:
      * name1, name2 changed from gss_name_t to gss_opaque_t
      */
     OM_uint32 (*compare_name)
	  (OM_uint32	*minor_status,
	   gss_opaque_t	name1,
	   gss_opaque_t	name2,
	   int		*name_equal);

     /* changes:
      * input_name changed from gss_name_t to gss_opaque_t
      * output_name_type omitted
      */
     OM_uint32 (*display_name)
	  (OM_uint32	*minor_status,
	   gss_opaque_t	input_name,
	   gss_buffer_t	output_name_buffer);

     /* changes:
      * output_name changed from (gss_name_t *) to (gss_opaque_t *)
      * input_name_type omitted
      */
     OM_uint32 (*import_name)
	  (OM_uint32	*minor_status,
	   gss_buffer_t input_name_buffer,
	   gss_opaque_t *output_name);

     /* changes:
      * input name changed from gss_name_t to gss_opaque_t
      */
     OM_uint32 (*release_name)
       (OM_uint32	*minor_status,
	gss_opaque_t	input_name);

} gss_namespace_t;


#endif	/* GSS_NAMESPACE_H */

