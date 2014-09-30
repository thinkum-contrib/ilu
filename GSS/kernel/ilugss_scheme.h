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

$Id: ilugss_scheme.h,v 1.11 1999/08/03 01:59:38 janssen Exp $
*/
#ifndef GSS_SCHEME_H
#define GSS_SCHEME_H 1
/*
 * gss_scheme.h -- definition of data type which defines entry points into an
 *		implementation of a single security scheme
 *
 * Antony Courtney,	15/6/95
 */

/*
 * gss_scheme_t -- An implementation of a particular security scheme must
 * provide all of the entry points defined here.
 * N.B.: The entry points declared here form a modified subset of the
 * functions declared in gssapi.h.  See the GSS API spec for further
 * explanation of them.  Changes from the corresponding GSS API
 * signatures have been noted in comments.
 */
typedef struct gss_scheme_s {

     /* OID of this scheme: */
     gss_OID oid;

     /* changes:
      * desired_mechs omitted
      * output_cred_handle changed from (gss_cred_id_t *) to (gss_opaque_t *)
      */
     OM_uint32 (*acquire_cred)
	  (OM_uint32    	*minor_status,
	   gss_name_t		desired_name,
	   OM_uint32		time_req,
	   int			cred_usage,
	   gss_opaque_t 	*output_cred_handle,
	   OM_uint32		*time_rec
	   );

     /* changes:
      * cred_handle changed from (gss_cred_id_t *) to (gss_opaque_t *)
      */
     OM_uint32	(*release_cred)
	  (OM_uint32		*minor_status,
	   gss_opaque_t		*cred_handle
	   );

     /* changes:
      * claimant_cred_handle changed from gss_cred_id_t to gss_opaque_t
      * context_handle changed from (gss_ctx_id_t *) to (gss_opaque_t *)
      * mech_type omitted
      * actual_mech_type omitted
      */
     OM_uint32	(*init_sec_context)
	  (OM_uint32		*minor_status,
	   gss_opaque_t		claimant_cred_handle,
	   gss_opaque_t		*context_handle,
	   gss_name_t		target_name,
	   OM_uint32		req_flags,
	   OM_uint32		time_req,
	   gss_channel_bindings_t input_chan_bindings,
	   gss_buffer_t		input_token,
	   gss_buffer_t		output_token,
	   OM_uint32		*ret_flags,
	   OM_uint32		*time_rec
	   );

     /* changes:
      * context_handle changed from (gss_ctx_id_t *) to (gss_opaque_t *)
      * verifier_cred_handle changed from gss_cred_id_t to gss_opaque_t
      * mech_type omitted
      * delegated_cred_handle changed from (gss_cred_id_t *) to
      * (gss_opaque_t *)
      */
     OM_uint32	(*accept_sec_context)
	  (OM_uint32		*minor_status,
	   gss_opaque_t		*context_handle,
	   gss_opaque_t		verifier_cred_handle,
	   gss_buffer_t		input_token_buffer,
	   gss_channel_bindings_t input_chan_bindings,
	   gss_name_t		*src_name,
	   gss_buffer_t		output_token,
	   OM_uint32		*ret_flags,
	   OM_uint32		*time_rec,
	   gss_opaque_t		*delegated_cred_handle
	   );

     /* changes:
      * context_handle changed from gss_ctx_id_t to gss_opaque_t
      */
     OM_uint32	(*process_context_token)
	  (OM_uint32		*minor_status,
	   gss_opaque_t		context_handle,
	   gss_buffer_t		token_buffer
	   );

     /* changes:
      * context_handle changed from (gss_ctx_id_t *) to (gss_opaque_t *)
      */
     OM_uint32	(*delete_sec_context)
	  (OM_uint32		*minor_status,
	   gss_opaque_t		*context_handle,
	   gss_buffer_t		output_token
	   );


     /* changes:
      * context_handle changed from gss_ctx_id_t to gss_opaque_t
      */
     OM_uint32	(*context_time)
	  (OM_uint32		*minor_status,
	   gss_opaque_t		context_handle,
	   OM_uint32		*time_rec
	   );

     /* changes:
      * context_handle changed from gss_ctx_id_t to gss_opaque_t
      */
     OM_uint32	(*get_mic)
	  (OM_uint32		*minor_status,
	   gss_opaque_t		context_handle,
	   gss_qop_t		qop_req,
	   gss_buffer_t		message_buffer,
	   gss_buffer_t		message_token
	   );

     /* changes:
      * context_handle changed from gss_ctx_id_t to gss_opaque_t
      */
     OM_uint32	(*verify_mic)
	  (OM_uint32		*minor_status,
	   gss_opaque_t		context_handle,
	   gss_buffer_t		message_buffer,
	   gss_buffer_t		token_buffer,
	   gss_qop_t		*qop_state
	   );

     /* changes:
      * context_handle changed from gss_ctx_id_t to gss_opaque_t
      */
     OM_uint32	(*wrap)
	  (OM_uint32		*minor_status,
	   gss_opaque_t		context_handle,
	   int			conf_req_flag,
	   gss_qop_t		qop_req,
	   gss_buffer_t		input_message_buffer,
	   int			*conf_state,
	   gss_buffer_t		output_message_buffer
	   );

     /* changes:
      * context_handle changed from gss_ctx_id_t to gss_opaque_t
      */
     OM_uint32	(*unwrap)
	  (OM_uint32		*minor_status,
	   gss_opaque_t		context_handle,
	   gss_buffer_t		input_message_buffer,
	   gss_buffer_t		output_message_buffer,
	   int			*conf_state,
	   gss_qop_t		*qop_state
	   );

     /* changes:
      * (only used to convert mechanism-specific status codes; general GSS
      *  status codes handled by GSS shell layer directly)
      *
      * status_type omitted
      * mech_type omitted
      */
     OM_uint32	(*display_status)
	  (OM_uint32		*minor_status,
	   OM_uint32		status_value,
	   OM_uint32		*message_context,
	   gss_buffer_t		status_string
	   );

     /* changes:
      * cred_handle changed from gss_cred_id_t to gss_opaque_t
      */
     OM_uint32	(*inquire_cred)
          (OM_uint32		*minor_status,
	   gss_opaque_t		cred_handle,
	   gss_name_t		*name,
	   OM_uint32		*lifetime,
	   int			*cred_usage
	   );

     /* changes:
      * context_handle changed from gss_ctx_id_t to gss_opaque_t
      * mech_type omitted
      */
     OM_uint32 (*inquire_context)
       (OM_uint32	*minor_status,
	gss_opaque_t 	context_handle,
	gss_name_t 	*src_name,
	gss_name_t 	*targ_name,
	OM_uint32 	*lifetime_rec,
	OM_uint32 	*ctx_flags,
	int 		*locally_initiated,
	int 		*open
	);

     /* changes:
      * ctx_handle changed from gss_ctx_id_t to gss_opaque_t
      */
     OM_uint32 (*wrap_size_limit)
       (OM_uint32	*minor_status,
	gss_opaque_t	context_handle,
	int        	conf_req_flag,
	gss_qop_t	qop_req,
	OM_uint32	req_output_size,
	OM_uint32	*max_input_size
	);


} gss_scheme_t;

#endif /* GSS_SCHEME_H */
