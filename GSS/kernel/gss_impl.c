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

$Id: gss_impl.c,v 1.40 1999/08/03 01:59:40 janssen Exp $
*/
/*
 * top-level implementation of GSS API shell
 */

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <gssapi.h>

#include "ilugss_impl.h"
#include "ilugss_oidtbl.h"
#include "ilugss_util.h"
#include "ilugss_asn1.h"

static char *_ilugss_strerror(OM_uint32 status_code);

/* set of default security schemes to try; should be moved into a
 * self-contained configuration module at some point
 */
/* N.B.  The ordering of the defaults in this vector matters; the first entry
 * will be used as the default mechanism in, e.g. gss_init_sec_context
 */
static gss_OID_desc default_scheme_oid_vec[]={
     /* comma-separated list of defaults: KERBEROS, PGP, etc... */
     {0, NULL}
};

static gss_OID_set_desc default_scheme_desc={
     sizeof(default_scheme_oid_vec)/sizeof(gss_OID_desc),
     (gss_OID) default_scheme_oid_vec
};

static gss_OID_set default_scheme_set=&default_scheme_desc;

static gss_OID default_scheme=default_scheme_oid_vec;

static gss_oidtbl_t _gss_schemes;
static gss_oidtbl_t _gss_namespaces;

/* initialization flag: */
static int virgin=1;

/* initialisation of GSS data structures */
/* N.B.: Only exposes for testing; should really be static */
void ilugss_initialize(void)
{
  extern void ilugssmech_nil_initialize(void);
  extern void ilugssns_rfc822_initialize(void);
  extern void ilugssns_anonymous_initialize(void);

  _gss_schemes = _ilugss_oidtbl_create();
  _gss_namespaces = _ilugss_oidtbl_create();

  virgin = 0;

  ilugssmech_nil_initialize();
  ilugssns_rfc822_initialize();
  ilugssns_anonymous_initialize();
}

/* static initialisation check: */
#define GSS_STATIC_INIT if (virgin) { ilugss_initialize(); }

void
  ilugss_register_scheme (gss_scheme_t *scheme, char *name)
{
  OM_uint32 retcode, minor;

  GSS_STATIC_INIT;
  _ilugss_oidtbl_insert (_gss_schemes, scheme->oid, scheme);
  retcode = ilu_gssext_add_scheme_by_name (&minor, name, scheme->oid);
  if (!GSS_ERROR(retcode)) {
    DEBUG(INIT_DB, "Added security scheme <%s>.\n", name);
  } else {
    DEBUG(CALL_DB|ERR_DB,"ilu_gssext_add_scheme_by_name() returning %d (%s)\n",
	  retcode,_ilugss_strerror(retcode));
  }
}

gss_scheme_t *
  _ilugss_lookup_scheme (gss_OID oid)
{
  GSS_STATIC_INIT;
  return _ilugss_oidtbl_find (_gss_schemes, oid);
}

void
  ilugss_register_namespace (gss_namespace_t *ns, char *name)
{
  OM_uint32 retcode, minor;

  GSS_STATIC_INIT;
  _ilugss_oidtbl_insert (_gss_namespaces, ns->oid, ns);
  retcode = ilu_gssext_add_namespace_by_name (&minor, name, ns->oid);
  if (!GSS_ERROR(retcode)) {
    DEBUG(INIT_DB, "Added namespace <%s>.\n", name);
  } else {
    DEBUG(CALL_DB|ERR_DB,"ilu_gssext_add_namespace_by_name() returning %d (%s)\n",
	  retcode,_ilugss_strerror(retcode));
  }
}

gss_namespace_t *
  _ilugss_lookup_namespace (gss_OID oid)
{
  GSS_STATIC_INIT;
  return _ilugss_oidtbl_find (_gss_namespaces, oid);
}

OM_uint32 gss_acquire_cred
(OM_uint32 	*minor_status,
 gss_name_t 	desired_name,
 OM_uint32 	time_req,
 gss_OID_set 	desired_mechs,
 int 		cred_usage,
 gss_cred_id_t 	*output_cred_handle,
 gss_OID_set 	*actual_mechs,
 OM_uint32 	*time_rec
 )
{
     gss_scheme_t *scm;
     OM_uint32 retcode;
     gss_opaque_t scm_cred_handle;
     gss_impl_cred_id_t *cred_result;
     int ntried;	/* number of schemes tried */
     int i;

     GSS_STATIC_INIT;
     DEBUG(CALL_DB,"gss_acquire_cred() called.\n");
     *minor_status=0;
     if (desired_mechs==GSS_C_NO_OID_SET) {
	  /* select default mechanism(s) to try */
	  desired_mechs=(gss_OID_set) default_scheme_set;
     }
     
     /* iterate over desired mechanisms, until we find one that works */
     for (i=0, ntried=0; i < (int) desired_mechs->count; i++) {
	  if ((scm=_ilugss_lookup_scheme(desired_mechs->elements + i))!=NULL) {
	       ntried++;
	       /* found a matching scheme; try and acquire a credential for
		* this scheme
		*/
	       retcode=(scm->acquire_cred)(minor_status,desired_name,
					   time_req,cred_usage,
					   &scm_cred_handle,time_rec);
	       if (!GSS_ERROR(retcode)) {
		    /* acquisition successfull! */
		    /* indicate actual mechanism to user */
		    if (actual_mechs!=NULL) {
			 if ((*actual_mechs)->count>=1) {
			      (*actual_mechs)->count=1;
			      (*actual_mechs)->elements[0]=*(scm->oid);
			 }
		    }
		    /* pair credentials with scheme for which they are valid */
		    cred_result=ilugss_malloc(sizeof(gss_impl_cred_id_t));
		    cred_result->scheme=scm;
		    cred_result->scheme_cred_id=scm_cred_handle;
		    *output_cred_handle=cred_result;
		    DEBUG(CALL_DB,"gss_acquire_cred() returning: GSS_S_COMPLETE\n");
		    return GSS_S_COMPLETE;
	       }
	  }
     }
     if (ntried==0) {
	  /* no matching mechanism was found */
	  DEBUG(CALL_DB|ERR_DB,"gss_acquire_cred() returning GSS_S_BAD_MECH\n");
	  return GSS_S_BAD_MECH;
     } else {
	  /* hmmm.  All mechanisms we tried failed;  We don't have an error
	   * code to indicate that, so we'll just:
	   */
	  DEBUG(CALL_DB|ERR_DB,"gss_acquire_cred() returning %d (%s)\n",
		retcode,_ilugss_strerror(retcode));
	  return retcode;
     }
     /* NOTREACHED */
}

OM_uint32 gss_release_cred
(OM_uint32	*minor_status,
 gss_cred_id_t	*cred_handle
 )
{
     gss_scheme_t *scm=gss_cred_scheme(*cred_handle);
     OM_uint32 retcode;

     GSS_STATIC_INIT;
     DEBUG(CALL_DB,"gss_release_cred() called\n");
     *minor_status=0;
     assert((cred_handle!=NULL) && (*cred_handle!=NULL));

     /* call scheme-specific release routine */
     retcode=(scm->release_cred)(minor_status,
				 &(gss_cred_opaque(*cred_handle)));
     if (retcode!=GSS_S_COMPLETE) {
	  DEBUG(CALL_DB|ERR_DB,"gss_release_cred() returning %d (%s)\n",
		retcode,_ilugss_strerror(retcode));
	  return retcode;
     }
     
     /* release storage */
     ilugss_free (*cred_handle);
     DEBUG(CALL_DB,"gss_release_cred() returning GSS_S_COMPLETE\n");
     return GSS_S_COMPLETE;
}

OM_uint32 gss_init_sec_context
(OM_uint32	*minor_status,
 gss_cred_id_t	claimant_cred_handle,
 gss_ctx_id_t	*context_handle,
 gss_name_t	target_name,
 gss_OID	mech_type,
 OM_uint32	req_flags,
 OM_uint32	time_req,
 gss_channel_bindings_t input_chan_bindings,
 gss_buffer_t	 input_token,
 gss_OID	*actual_mech_type,
 gss_buffer_t	output_token,
 OM_uint32	*ret_flags,
 OM_uint32	*time_rec
 )
{
     gss_scheme_t *scm;
     OM_uint32 retcode;
     gss_opaque_t scm_cred;
     gss_opaque_t scm_context;
     gss_impl_ctx_id_t *return_context;
     int initial_call;

     GSS_STATIC_INIT;
     DEBUG(CALL_DB|INIT_DB,"gss_init_sec_context() called\n");
     *minor_status=0;
     output_token->length=0;
     output_token->value=NULL;
     if (mech_type==GSS_C_NULL_OID) {
	  /* select default mechanism */
	  mech_type=default_scheme;
     }

     if ((scm=_ilugss_lookup_scheme(mech_type))==NULL) {
	  DEBUG(CALL_DB|ERR_DB,"gss_init_sec_context() returning: GSS_S_BAD_MECH\n");
	  return GSS_S_BAD_MECH;
     }
     if (actual_mech_type!=NULL) {
	  *actual_mech_type=scm->oid;
     }

     if (claimant_cred_handle==GSS_C_NO_CREDENTIAL) {
	  scm_cred=NULL;
     } else {
	  /* ensure that credential is valid for this scheme! */
	  if (gss_cred_scheme(claimant_cred_handle)!=scm) {
	       DEBUG(CALL_DB|ERR_DB,"gss_init_sec_context() returning: GSS_S_DEFECTIVE_CREDENTIAL\n");
	       return GSS_S_DEFECTIVE_CREDENTIAL;
	  }
	  scm_cred=gss_cred_opaque(claimant_cred_handle);
     }

     initial_call = (*context_handle==GSS_C_NO_CONTEXT);
     if (initial_call) {
	  scm_context=NULL;
     } else {
	  scm_context=gss_ctx_opaque(*context_handle);
     }
     if (input_token!=NULL) {
	  if (input_token->length==0) {
	       DEBUG(INIT_DB|PACKET_DB,"gss_init_sec_context(): empty input token.\n");
	  } else {
	       DEBUG(INIT_DB|PACKET_DB,"gss_init_sec_context() input_token:\n");
	       _ilugss_dump_buf(input_token);
	  }
     }
     retcode=(scm->init_sec_context)(minor_status,scm_cred,&scm_context,
				     target_name,req_flags,time_req,
				     input_chan_bindings,input_token,
				     output_token,ret_flags,time_rec);
     DEBUG(INIT_DB|PACKET_DB,"gss_init_sec_context() output_token (no OID):\n");
     _ilugss_dump_buf(output_token);
     if (GSS_ERROR(retcode) && (GSS_ROUTINE_ERROR(retcode)!=
				GSS_S_CONTINUE_NEEDED)) {
	  DEBUG(CALL_DB|ERR_DB,"gss_init_sec_context() returning: %d (%s)\n",
		retcode,_ilugss_strerror(retcode));
	  return retcode;
     }
     /* do ASN.1 wrapping of output token on first call */
     if (initial_call) {
	  _ilugss_asn1_wrap_oid(scm->oid,output_token);
	  DEBUG(CALL_DB|PACKET_DB,"gss_init_sec_context() output token (OID inserted):\n");
	  _ilugss_dump_buf(output_token);
     }

     /* wrap scheme-specific context into a gss_impl_ctx_id_t */
     return_context=ilugss_malloc(sizeof(gss_impl_ctx_id_t));
     return_context->scheme=scm;
     return_context->scheme_ctx_id=scm_context;
     *context_handle=return_context;

     DEBUG(CALL_DB|ERR_DB,"gss_init_sec_context() returning: %d (%s)\n",
	   retcode,_ilugss_strerror(retcode));
     return retcode;
}

/* N.B. mech_type is used as a read,modify parameter here */
OM_uint32 gss_accept_sec_context
(OM_uint32	*minor_status,
 gss_ctx_id_t	*context_handle,
 gss_cred_id_t	verifier_cred_handle,
 gss_buffer_t	input_token_buffer,
 gss_channel_bindings_t input_chan_bindings,
 gss_name_t	*src_name,
 gss_OID       	*mech_type,
 gss_buffer_t	output_token,
 OM_uint32	*ret_flags,
 OM_uint32	*time_rec,
 gss_cred_id_t	*delegated_cred_handle
 )
{
     OM_uint32 retcode;
     gss_scheme_t *scm;
     gss_opaque_t scm_context;
     gss_opaque_t scm_vrfy_cred;
     gss_opaque_t scm_deleg_cred;
     gss_OID packet_mech_oid;
     gss_impl_ctx_id_t *return_context;
     int initial_call;
     
     GSS_STATIC_INIT;
     DEBUG(CALL_DB,"gss_accept_sec_context() called\n");
     *minor_status=0;
     output_token->length=0;
     output_token->value=NULL;
     
     DEBUG(INIT_DB|PACKET_DB,"gss_accept_sec_context(): input token:\n");
     _ilugss_dump_buf(input_token_buffer);
     
     if (*context_handle==GSS_C_NO_CONTEXT) {
	  /* first call - extract the scheme OID from the input token buffer */
	  packet_mech_oid = _ilugss_asn1_extract_oid(input_token_buffer);
	  DEBUG(INIT_DB|PACKET_DB,"gss_accept_sec_context(): input token (no OID):\n");
	  _ilugss_dump_buf(input_token_buffer);
	  if (packet_mech_oid==NULL) {
	       DEBUG(CALL_DB|ERR_DB,"gss_accept_sec_context() returning GSS_S_DEFECTIVE_TOKEN\n");
	       return GSS_S_DEFECTIVE_TOKEN;
	  }
	  /* and make sure it's a scheme we support */
	  if ((scm=_ilugss_lookup_scheme(packet_mech_oid))==NULL) {
	       DEBUG(CALL_DB|ERR_DB,"gss_accept_sec_context() returning GSS_S_BAD_MECH\n");
	       return GSS_S_BAD_MECH;
	  }
	  scm_context=NULL;
	  initial_call=1;
     } else {
	  scm=gss_ctx_scheme(*context_handle);
	  scm_context=gss_ctx_opaque(*context_handle);
	  initial_call=0;
     }

     if (verifier_cred_handle==GSS_C_NO_CREDENTIAL) {
	  scm_vrfy_cred=NULL;
     } else {
	  scm_vrfy_cred=gss_cred_opaque(verifier_cred_handle);
     }

     retcode=(scm->accept_sec_context)(minor_status,&scm_context,
				       scm_vrfy_cred,input_token_buffer,
				       input_chan_bindings,src_name,
				       output_token,ret_flags,time_rec,
				       &scm_deleg_cred);
     DEBUG(INIT_DB|PACKET_DB,"gss_accept_sec_context(): output token:\n");
     _ilugss_dump_buf(output_token);
     if (GSS_ERROR(retcode) && (GSS_ROUTINE_ERROR(retcode)!=
				GSS_S_CONTINUE_NEEDED)) {
	  DEBUG(CALL_DB|ERR_DB,"gss_accept_sec_context() returning: %d (%s)\n",
		retcode, _ilugss_strerror(retcode));
	  return retcode;
     }

     if (initial_call) {
	  /* wrap scheme-specific context into a gss_impl_ctx_id_t */
	  return_context=ilugss_malloc(sizeof(gss_impl_ctx_id_t));
	  return_context->scheme=scm;
	  return_context->scheme_ctx_id=scm_context;
	  *context_handle=return_context;
     }

     DEBUG(CALL_DB,"gss_accept_sec_context() returning: %d (%s)\n",
	   retcode, _ilugss_strerror(retcode));
     return retcode;
}

OM_uint32 gss_process_context_token
(OM_uint32	*minor_status,
 gss_ctx_id_t	context_handle,
 gss_buffer_t	token_buffer
 )
{
     OM_uint32 retcode;
     gss_scheme_t *scm=gss_ctx_scheme(context_handle);

     GSS_STATIC_INIT;
     *minor_status=0;
     DEBUG(CALL_DB,"gss_process_context_token() called.\n");
     retcode=(scm->process_context_token)(minor_status,
					  gss_ctx_opaque(context_handle),
					  token_buffer);
     DEBUG(CALL_DB,"gss_process_context_token(): returning %d (%s) \n",
	   retcode,_ilugss_strerror(retcode));
     return retcode;
}

OM_uint32 gss_delete_sec_context
(OM_uint32	*minor_status,
 gss_ctx_id_t	*context_handle,
 gss_buffer_t	output_token
 )
{
     OM_uint32 retcode;
     gss_scheme_t *scm=gss_ctx_scheme(*context_handle);
     GSS_STATIC_INIT;

     DEBUG(CALL_DB,"gss_delete_sec_context() called.\n");
     *minor_status=0;
     retcode=(scm->delete_sec_context)(minor_status,
				       gss_ctx_opaque(*context_handle),
				       output_token);
     DEBUG(CALL_DB,"gss_delete_sec_context() returning: %d (%s)\n",
	   retcode,_ilugss_strerror(retcode));
     ilugss_free (*context_handle);
     *context_handle = NULL;
     return retcode;
}

OM_uint32 gss_context_time
(OM_uint32	*minor_status,
 gss_ctx_id_t	context_handle,
 OM_uint32	*time_rec
 )
{
     OM_uint32 retcode;
     gss_scheme_t *scm=gss_ctx_scheme(context_handle);
     GSS_STATIC_INIT;
     *minor_status=0;

     DEBUG(CALL_DB,"gss_context_time() called.\n");
     retcode=(scm->context_time)(minor_status,
				 gss_ctx_opaque(context_handle),
				 time_rec);
     DEBUG(CALL_DB,"gss_context_time() returning: %d (%s)\n",
	   retcode,_ilugss_strerror(retcode));
     return retcode;
}

OM_uint32 gss_get_mic
(OM_uint32	*minor_status,
 gss_ctx_id_t	context_handle,
 gss_qop_t	qop_req,
 gss_buffer_t	message_buffer,
 gss_buffer_t	message_token
 )
{
     gss_scheme_t *scm=gss_ctx_scheme(context_handle);
     OM_uint32 retcode;

     GSS_STATIC_INIT;
     DEBUG(CALL_DB,"gss_get_mic() called\n");
     *minor_status=0;
     retcode=(scm->get_mic)(minor_status,gss_ctx_opaque(context_handle),
			    qop_req,message_buffer,message_token);

     DEBUG(CALL_DB,"gss_get_mic() returning: %d (%s)\n",
	   retcode,_ilugss_strerror(retcode));
     return retcode;
}

OM_uint32 gss_verify_mic
(OM_uint32	*minor_status,
 gss_ctx_id_t	context_handle,
 gss_buffer_t	message_buffer,
 gss_buffer_t	token_buffer,
 gss_qop_t	*qop_state
 )
{
     gss_scheme_t *scm=gss_ctx_scheme(context_handle);
     OM_uint32 retcode;

     GSS_STATIC_INIT;
     DEBUG(CALL_DB,"gss_verify_mic() called\n");
     *minor_status=0;
     retcode=(scm->verify_mic)(minor_status,gss_ctx_opaque(context_handle),
			       message_buffer,token_buffer,qop_state);

     DEBUG(CALL_DB,"gss_verify_mic() returning: %d (%s)\n",
	   retcode,_ilugss_strerror(retcode));
     return retcode;
}

OM_uint32 gss_wrap
(OM_uint32	*minor_status,
 gss_ctx_id_t	context_handle,
 int		conf_req_flag,
 gss_qop_t	qop_req,
 gss_buffer_t	input_message_buffer,
 int		*conf_state,
 gss_buffer_t	output_message_buffer
 )
{
     gss_scheme_t *scm=gss_ctx_scheme(context_handle);
     OM_uint32 retcode;

     GSS_STATIC_INIT;
     DEBUG(CALL_DB,"gss_wrap() called\n");
     *minor_status=0;
     DEBUG(PACKET_DB,"gss_wrap() input buffer:\n");
     _ilugss_dump_buf(input_message_buffer);

     output_message_buffer->length=0;
     retcode=(scm->wrap)(minor_status,gss_ctx_opaque(context_handle),
			 conf_req_flag,qop_req,input_message_buffer,
			 conf_state,output_message_buffer);
     DEBUG(PACKET_DB,"gss_wrap() output buffer:\n");
     _ilugss_dump_buf(output_message_buffer);
     DEBUG(CALL_DB,"gss_wrap() returning: %d (%s)\n",
	   retcode,_ilugss_strerror(retcode));
     return retcode;
}

OM_uint32 gss_unwrap
(OM_uint32	*minor_status,
 gss_ctx_id_t	context_handle,
 gss_buffer_t	input_message_buffer,
 gss_buffer_t	output_message_buffer,
 int		*conf_state,
 gss_qop_t	*qop_state
 )
{
     gss_scheme_t *scm=gss_ctx_scheme(context_handle);
     OM_uint32 retcode;

     GSS_STATIC_INIT;
     DEBUG(CALL_DB,"gss_unwrap() called\n");
     *minor_status=0;
     DEBUG(PACKET_DB,"gss_unwrap() input buffer:\n");
     _ilugss_dump_buf(input_message_buffer);

     output_message_buffer->length=0;
     retcode=(scm->unwrap)(minor_status,gss_ctx_opaque(context_handle),
			   input_message_buffer,output_message_buffer,
			   conf_state,qop_state);
     DEBUG(PACKET_DB,"gss_unwrap() output buffer:\n");
     _ilugss_dump_buf(output_message_buffer);
     DEBUG(CALL_DB,"gss_unwrap() returning: %d (%s)\n",
	   retcode,_ilugss_strerror(retcode));
     return retcode;
}

/* data structures used to construct error messages in gss_display_status()
 */

/* routine errors: */
static char *routine_err_names[]={
     "no error",
     "GSS_S_BAD_MECH",
     "GSS_S_BAD_NAME",
     "GSS_S_BAD_NAMETYPE",
     "GSS_S_BAD_BINDINGS",
     "GSS_S_BAD_STATUS",
     "GSS_S_BAD_SIG",
     "GSS_S_NO_CRED",
     "GSS_S_NO_CONTEXT",
     "GSS_S_DEFECTIVE_TOKEN",
     "GSS_S_DEFECTIVE_CREDENTIAL",
     "GSS_S_CREDENTIALS_EXPIRED",
     "GSS_S_CONTEXT_EXPIRED",
     "GSS_S_FAILURE",
     "GSS_S_BAD_QOP",
};

static const OM_uint32 num_routine_errs=sizeof(routine_err_names)/sizeof(char *);

static char *routine_err_strs[]={
     "no error",
     "An unsupported mechanism was requested",
     "An invalid name was supplied",
     "A supplied name was of an unsupported type",
     "Incorrect channel bindings were supplied",
     "An invalid status code was supplied",
     "A token had an invalid signature or MIC",
     "No credentials were supplied",
     "No context has been established",
     "A token was invalid",
     "A credential was invalid",
     "The referenced credentials have expired",
     "The context has expired",
     "Miscellaneous failure",
     "The quality-of-protection requested could not be provided"
};

OM_uint32 gss_display_status
(OM_uint32	*minor_status,
 OM_uint32	status_value,
 int		status_type,
 gss_OID	mech_type,
 OM_uint32	*message_context,
 gss_buffer_t	status_string
 )
{
     OM_uint32 routine_err_idx;
     OM_uint32 retcode;
     OM_uint32 msglen;
     char *msg;
     static char *no_err_msg="no error";

     GSS_STATIC_INIT;
     *minor_status=0;

     if (status_type==GSS_C_GSS_CODE) {
	  /* just a GSS status code -- construct message here */
	  routine_err_idx=GSS_ROUTINE_ERROR(status_value) >> GSS_C_ROUTINE_ERROR_OFFSET;
	  *message_context = 0;
	  if (routine_err_idx==0) {
	       *status_string=ilugss_alloc_buffer(strlen(no_err_msg) + 1);
	       strcpy(status_string->value,no_err_msg);
	       return GSS_S_COMPLETE;
	  } else {
	       msglen  = strlen(routine_err_names[routine_err_idx]);
	       msglen += strlen(routine_err_strs[routine_err_idx]);
	       msglen += 32;	/* to account for bracketing, NUL, etc... */
	       *status_string=ilugss_alloc_buffer(msglen);
	       sprintf(status_string->value,"%s (%s)",
		       routine_err_names[routine_err_idx],
		       routine_err_strs[routine_err_idx]);
	       status_string->length=strlen(status_string->value) + 1;
	       return GSS_S_COMPLETE;
	  }
     } else {
       gss_scheme_t *scm;
       if ((scm=_ilugss_lookup_scheme(mech_type))==NULL) {
	 DEBUG(CALL_DB|ERR_DB,"gss_display_status() returning: GSS_S_BAD_MECH\n");
	 return GSS_S_BAD_MECH;
       }
       retcode=(scm->display_status)(minor_status, status_value,
				     message_context, status_string);
       DEBUG(CALL_DB,"gss_display_status() returning: %d (%s)\n",
	     retcode,_ilugss_strerror(retcode));
       return retcode;
     }
}

static void AddScheme (gss_OID oid, void *unused, void *rock)
{
  gss_OID_desc **elements = (gss_OID_desc **) rock;
  *(*elements) = *oid;
  (*elements)++;
}

OM_uint32 gss_indicate_mechs
(OM_uint32	*minor_status,
 gss_OID_set	*mech_set
 )
{
  gss_OID_set mechs;
  gss_OID_desc *ptr;

  mechs = (gss_OID_set) ilugss_malloc(sizeof(gss_OID_set_desc));
  mechs->count = _ilugss_oidtbl_size (_gss_schemes);
  mechs->elements = (gss_OID_desc *) ilugss_malloc(sizeof(gss_OID) * mechs->count);
  ptr = mechs->elements;
  _ilugss_oidtbl_enumerate (_gss_schemes, AddScheme, (void *) &ptr);
  *mech_set = mechs;
  *minor_status = 0;
  return GSS_S_COMPLETE;
}

OM_uint32 gss_compare_name
(OM_uint32	*minor_status,
 gss_name_t	name1,
 gss_name_t	name2,
 int		*name_equal
 )
{
     gss_namespace_t *ns=gss_name_ns(name1);
     OM_uint32 retcode;

     GSS_STATIC_INIT;
     DEBUG(CALL_DB,"gss_compare_name() called\n");
     *minor_status=0;
     if (ns!=gss_name_ns(name2)) {
	  /* N.B.: we just return the names as "unequal" because they
	   * are from different namespaces
	   */
	  *name_equal=0;
	  return GSS_S_COMPLETE;
     }

     retcode=(ns->compare_name)(minor_status,gss_name_opaque(name1),
				gss_name_opaque(name2),name_equal);

     DEBUG(CALL_DB,"gss_compare_name() returning: %d (%s) (name_equal==%d)\n",
	   retcode,_ilugss_strerror(retcode),*name_equal);
     return retcode;
}

OM_uint32 gss_display_name
(OM_uint32	*minor_status,
 gss_name_t	input_name,
 gss_buffer_t	output_name_buffer,
 gss_OID	*output_name_type
 )
{
     gss_namespace_t *ns=gss_name_ns(input_name);
     OM_uint32 retcode;

     GSS_STATIC_INIT;
     DEBUG(CALL_DB,"gss_display_name() called.\n");
     *minor_status=0;
     retcode=(ns->display_name)(minor_status,gss_name_opaque(input_name),
				output_name_buffer);
     if (output_name_type!=NULL) {
	  *output_name_type=ns->oid;
     }
     DEBUG(CALL_DB,"gss_display_name() returning: %d (%s)\n",
	   retcode,_ilugss_strerror(retcode));
     return retcode;
}

OM_uint32 gss_import_name
(OM_uint32	*minor_status,
 gss_buffer_t	input_name_buffer,
 gss_OID	input_name_type,
 gss_name_t	*output_name
 )
{
     gss_namespace_t *ns;
     gss_impl_name_t *out_name_impl;

     OM_uint32 retcode;

     GSS_STATIC_INIT;
     DEBUG(CALL_DB,"gss_import_name() called\n");
     *minor_status=0;
     if ((ns=_ilugss_lookup_namespace(input_name_type))==NULL) {
	  return GSS_S_BAD_NAMETYPE;
     }
     out_name_impl=ilugss_malloc(sizeof(gss_impl_name_t));
     out_name_impl->ns = ns;
     out_name_impl->refcount = 1;
     *output_name=(gss_name_t) out_name_impl;

     retcode=(ns->import_name)(minor_status,input_name_buffer,&(gss_name_opaque(out_name_impl)));

     DEBUG(CALL_DB,"gss_import_name() returning: %d (%s)\n",
	   retcode,_ilugss_strerror(retcode));
     return retcode;
}

OM_uint32 gss_release_name
(OM_uint32	*minor_status,
 gss_name_t	*input_name
 )
{
  gss_namespace_t *ns=gss_name_ns(*input_name);
  OM_uint32 retcode;

  GSS_STATIC_INIT;
  DEBUG(CALL_DB,"gss_release_name() called.\n");
  *minor_status=0;
  if (gss_name_refcount(*input_name) > 1) {
    gss_name_refcount(*input_name) -= 1;
    retcode = GSS_S_COMPLETE;
  } else {
    retcode=(ns->release_name)(minor_status,gss_name_opaque(*input_name));
    DEBUG(CALL_DB,"gss_release_name() returning: %d (%s)\n",
	  retcode,_ilugss_strerror(retcode));
    ilugss_free (*input_name);
    *input_name = NULL;
  }
  return retcode;
}

OM_uint32 gss_canonicalize_name
  (OM_uint32	*minor_status,
   gss_name_t	input_name,
   gss_OID	mech_type,
   gss_name_t *	output_name)
{
  GSS_STATIC_INIT;
  *minor_status=0;
  /* TODO: fill this in! */
  return GSS_S_FAILURE;
}

OM_uint32 gss_release_buffer
(OM_uint32	*minor_status,
 gss_buffer_t	buffer
 )
{
  GSS_STATIC_INIT;
  *minor_status=0;
  /* just free the buffer! */
  if (buffer->length > 0) {
    ilugss_free (buffer->value);
  }
  buffer->length=0;
  return GSS_S_COMPLETE;
}


OM_uint32 gss_release_oid_set
(OM_uint32	*minor_status,
 gss_OID_set	*set_p
 )
{
     gss_OID_set oid_set=*set_p;
     gss_OID oidp;

     for (oidp=oid_set->elements; oidp < oid_set->elements + oid_set->count;
	  oidp++) {
	  ilugss_free (oidp->elements);
     }
     ilugss_free (oid_set->elements);
     *set_p=NULL;
     
     return GSS_S_COMPLETE;
}


OM_uint32 gss_inquire_cred
(OM_uint32 	*minor_status,
 gss_cred_id_t	cred_handle,
 gss_name_t 	*name,
 OM_uint32 	*lifetime,
 int 		*cred_usage,
 gss_OID_set 	*mechanisms
 )
{
  gss_scheme_t *scm;
  OM_uint32 retcode;

  GSS_STATIC_INIT;
  DEBUG(CALL_DB,"gss_inquire_cred() called\n");
  *minor_status=0;

  if (cred_handle == GSS_C_NO_CREDENTIAL)
    return GSS_S_NO_CRED;

  /* call scheme-specific inquire routine */
  scm=gss_cred_scheme(cred_handle);
  retcode=(scm->inquire_cred)(minor_status,
			      gss_cred_opaque(cred_handle),
			      name, lifetime, cred_usage);
  if (retcode!=GSS_S_COMPLETE) {
    DEBUG(CALL_DB|ERR_DB,"gss_inquire_cred() returning %d (%s)\n",
	  retcode,_ilugss_strerror(retcode));
    return retcode;
  }

  if (mechanisms != NULL)
    {
      gss_OID_set mechs;
      if ((mechs = (gss_OID_set) ilugss_malloc(sizeof(gss_OID_set_desc))) == NULL)
	return GSS_S_FAILURE;
      mechs->count = 1;
      if ((mechs->elements = (gss_OID_desc *) ilugss_malloc(sizeof(gss_OID_desc) * 1)) == NULL)
	return GSS_S_FAILURE;
      mechs->elements[0] = *(scm->oid);
      *mechanisms = mechs;      
    }
  return GSS_S_COMPLETE;
}

OM_uint32 gss_inquire_context
(OM_uint32	*minor_status,
 gss_ctx_id_t 	context_handle,
 gss_name_t 	*src_name,
 gss_name_t 	*targ_name,
 OM_uint32 	*lifetime_rec,
 gss_OID 	*mech_type,
 OM_uint32 	*ctx_flags,
 int 		*locally_initiated,
 int 		*open
 )
{
  gss_scheme_t *scm;
  OM_uint32 retcode;

  GSS_STATIC_INIT;
  DEBUG(CALL_DB,"gss_inquire_context() called\n");
  *minor_status=0;

  /* call scheme-specific inquire routine */
  scm = gss_ctx_scheme(context_handle);
  retcode=(scm->inquire_context)(minor_status,
				 gss_cred_opaque(context_handle),
				 src_name, targ_name, lifetime_rec,
				 ctx_flags, locally_initiated, open);
  if (retcode!=GSS_S_COMPLETE) {
    DEBUG(CALL_DB|ERR_DB,"gss_inquire_cred() returning %d (%s)\n",
	  retcode,_ilugss_strerror(retcode));
    return retcode;
  }
  if (mech_type != NULL)
    *mech_type = scm->oid;
  return GSS_S_COMPLETE;
}


OM_uint32 gss_wrap_size_limit (
     OM_uint32		*minor_status,
     gss_ctx_id_t	context_handle,
     int	        conf_req_flag,
     gss_qop_t          qop_req,
     OM_uint32		req_output_size,
     OM_uint32 		*max_input_size
)
{
  gss_scheme_t *scm=gss_ctx_scheme(context_handle);
  OM_uint32 retcode;

  GSS_STATIC_INIT;
  DEBUG(CALL_DB,"gss_wrap_size_limit() called\n");
  *minor_status=0;

  /* call scheme-specific inquire routine */
  retcode=(scm->wrap_size_limit)(minor_status,
				 gss_cred_opaque(context_handle),
				 conf_req_flag, qop_req,
				 req_output_size, max_input_size);
  if (retcode!=GSS_S_COMPLETE) {
    DEBUG(CALL_DB|ERR_DB,"gss_wrap_size_limit() returning %d (%s)\n",
	  retcode,_ilugss_strerror(retcode));
  }
  return retcode;
}

OM_uint32 gss_import_name_object (
     OM_uint32 		*minor_status,
     void 		*input_name,
     gss_OID		input_name_type,
     gss_name_t 	*output_name
)
{
  /* This function is a very *bad* idea. -- wcj */

  gss_impl_name_t realname;
  gss_namespace_t *ns;
  gss_buffer_desc namebuf;
  gss_OID nametype;
  OM_uint32 major, minor;

  GSS_STATIC_INIT;
  DEBUG(CALL_DB,"gss_import_name_object() called\n");
  *minor_status=0;
  if ((ns=_ilugss_lookup_namespace(input_name_type))==NULL) {
    return GSS_S_BAD_NAMETYPE;
  }
  realname.ns_name = input_name;
  realname.ns = ns;
  realname.refcount = 1;

  /* since we can't trust the user to not do something stupid with the pointers,
     we now display, then import, the name */
  if ((major = gss_display_name (minor_status, &realname, &namebuf, &nametype)) != GSS_S_COMPLETE)
    return major;
  major = gss_import_name (minor_status, &namebuf, nametype, output_name);
  (void) gss_release_buffer (&minor, &namebuf);
  return major;
}


OM_uint32 gss_export_name_object (
     OM_uint32 		*minor_status,
     gss_name_t		input_name,
     gss_OID		desired_name_type,
     void 		**output_name
)
{
  /* This function is a very *bad* idea. -- wcj */

  gss_impl_name_t *realname;
  gss_namespace_t *ns;
  gss_buffer_desc namebuf;
  gss_OID nametype;
  OM_uint32 major, minor;

  GSS_STATIC_INIT;
  DEBUG(CALL_DB,"gss_import_name_object() called\n");
  *minor_status=0;
  if ((ns=_ilugss_lookup_namespace(desired_name_type))==NULL) {
    return GSS_S_BAD_NAMETYPE;
  }

  /* since we can't trust the user to not do something stupid with the pointers,
     we now display, then import, the name, before providing a pointer back to the user.
     Note potential memory leak -- better than a smash. */

  if ((major = gss_display_name (minor_status, input_name, &namebuf, &nametype)) != GSS_S_COMPLETE)
    return major;
  if (!ilugss_match_oid (nametype, desired_name_type))
    {
      (void) gss_release_buffer (&minor, &namebuf);
      return GSS_S_BAD_NAME;
    }
  major = gss_import_name (minor_status, &namebuf, desired_name_type, (gss_name_t *) &realname);
  (void) gss_release_buffer (&minor, &namebuf);
  if (major == GSS_S_COMPLETE)
    *output_name = (void *) gss_name_opaque(realname);
  return major;
}

OM_uint32 gss_add_cred (
     OM_uint32 		*minor_status,
     gss_cred_id_t	input_cred_handle,
     gss_name_t		desired_name,
     gss_OID		desired_mech,
     gss_cred_usage_t	cred_usage,
     OM_uint32		initiator_time_req,
     OM_uint32		acceptor_time_req,
     gss_cred_id_t 	*output_cred_handle,
     gss_OID_set 	*actual_mechs,
     OM_uint32 		*initiator_time_rec,
     OM_uint32 		*acceptor_time_rec
)
{
     GSS_STATIC_INIT;
     *minor_status=0;
     /* TODO: fill this in! */
     return GSS_S_FAILURE;
}


OM_uint32 gss_inquire_cred_by_mech (
     OM_uint32  	*minor_status,
     gss_cred_id_t	cred_handle,
     gss_OID		mech_type,
     gss_name_t 	*name,
     OM_uint32 		*initiator_lifetime,
     OM_uint32 		*acceptor_lifetime,
     gss_cred_usage_t 	*cred_usage
)
{
  gss_scheme_t *scm;
  OM_uint32 retcode;
  OM_uint32 lifetime;
  gss_cred_usage_t usage;

  GSS_STATIC_INIT;
  DEBUG(CALL_DB,"gss_inquire_cred_by_mech() called\n");
  *minor_status=0;

  if (cred_handle == GSS_C_NO_CREDENTIAL)
    return GSS_S_NO_CRED;

  scm = gss_cred_scheme(cred_handle);
  if (!ilugss_match_oid(scm->oid, mech_type))
    {
      DEBUG(CALL_DB|ERR_DB, "gss_inquire_cred_by_mech returning %d (%s)\n",
	    GSS_S_BAD_MECH, _ilugss_strerror(GSS_S_BAD_MECH));
      return GSS_S_BAD_MECH;
    }

  /* call scheme-specific inquire routine */
  retcode=(scm->inquire_cred)(minor_status,
			      gss_cred_opaque(cred_handle),
			      name, &lifetime, &usage);
  if (retcode!=GSS_S_COMPLETE) {
    DEBUG(CALL_DB|ERR_DB,"gss_inquire_cred() returning %d (%s)\n",
	  retcode,_ilugss_strerror(retcode));
    return retcode;
  }

  if (cred_usage != NULL)
    *cred_usage = usage;

  if (initiator_lifetime != NULL)
    {
      if (usage != GSS_C_ACCEPT)
	*initiator_lifetime = lifetime;
      else
	*initiator_lifetime = 0;
    }

  if (acceptor_lifetime != NULL)
    {
      if (usage != GSS_C_ACCEPT)
	*acceptor_lifetime = lifetime;
      else
	*acceptor_lifetime = 0;
    }

  return GSS_S_COMPLETE;
}

OM_uint32 gss_export_sec_context (
     OM_uint32 		*minor_status,
     gss_ctx_id_t 	*context_handle,
     gss_buffer_t       interprocess_token
)
{
     GSS_STATIC_INIT;
     *minor_status=0;
     /* TODO: fill this in! */
     return GSS_S_FAILURE;
}

OM_uint32 gss_import_sec_context (
     OM_uint32 		*minor_status,
     gss_buffer_t	interprocess_token,
     gss_ctx_id_t 	*context_handle
)
{
     GSS_STATIC_INIT;
     *minor_status=0;
     /* TODO: fill this in! */
     return GSS_S_FAILURE;
}


OM_uint32 gss_release_oid (
     OM_uint32		*minor_status,
     gss_OID		*oid
)
{
     GSS_STATIC_INIT;
     *minor_status=0;
     if ((*oid)->length > 0 && (*oid)->elements != NULL)
       ilugss_free ((*oid)->elements);
     ilugss_free (*oid);
     return GSS_S_COMPLETE;
}

OM_uint32 gss_create_empty_oid_set (
     OM_uint32		*minor_status,
     gss_OID_set	*oid_set
)
{
     if (*oid_set==NULL) {
	  *oid_set=ilugss_malloc(sizeof(gss_OID_set_desc));
     }
     (*oid_set)->count=0;
     (*oid_set)->elements=NULL;

     return GSS_S_COMPLETE;
}

OM_uint32 gss_add_oid_set_member (
     OM_uint32 		*minor_status,
     gss_OID		member_oid,
     gss_OID_set 	*oid_set_p
)
{
     OM_uint32 retcode;
     int present;
     gss_OID_set oid_set=*oid_set_p;
     gss_OID oidp;

     /* check if element already present in set */
     retcode=gss_test_oid_set_member(minor_status,member_oid,oid_set,&present);
     assert(!GSS_ERROR(retcode));
     if (present) {
	  /* member already in set, no need to add it */
	  return GSS_S_COMPLETE;
     }
     oid_set->count++;
     if (oid_set->count==1) {
	  oid_set->elements=ilugss_malloc(oid_set->count *
					    sizeof(gss_OID_desc));
     } else {
	  oid_set->elements=ilugss_realloc(oid_set->elements,
					     oid_set->count *
					     sizeof(gss_OID_desc));
     }
     oidp=oid_set->elements + oid_set->count - 1;
     oidp->length=member_oid->length;
     oidp->elements=ilugss_malloc(member_oid->length);
     memcpy(oidp->elements,member_oid->elements,member_oid->length);

     return GSS_S_COMPLETE;
}

OM_uint32 gss_test_oid_set_member (
     OM_uint32 		*minor_status,
     gss_OID		member,
     gss_OID_set	set,
     int 		*present
)
{
     gss_OID oidp;

     for (oidp=set->elements; oidp < set->elements + set->count; oidp++) {
	  if (oidp->length==member->length) {
	       if (memcmp(oidp->elements,member->elements,member->length)==0) {
		    *present=1;
		    return GSS_S_COMPLETE;
	       }
	  }
     }
     *present=0;
     return GSS_S_COMPLETE;
}

static void AddOther (gss_OID t, OM_uint32 *capacity, OM_uint32 value, int last)
{
  if (t->length >= *capacity)
    {
      *capacity = *capacity + 16;
      t->elements = (void *) ilugss_realloc(t->elements, *capacity);
    }
  ((unsigned char *)(t->elements))[t->length] = ((unsigned char) value) | (last ? (unsigned char) 0 : (unsigned char) 0x80);
  t->length++;
}

static OM_uint32 strchrcount (char * s, char * chars)
{
  OM_uint32 count = 0;
  OM_uint32 index = 0;

  while (s[index] != 0)
    {
      index += strcspn (s + index, chars);
      if (s[index] != 0)
	{
	  count += 1;
	  index += 1;
	}
    }
  return count;
}

static void parse_dotted_decimal_oid_string (gss_OID oid, gss_buffer_t str)
{
  OM_uint32 capacity = 0;
  OM_uint32 first, second, other;
  char *p = (char *) str->value;
  char *endp;
  int count = strchrcount ((char *) (char *) str->value, ".");

  if (count < 3)
    return;			/* all valid dotted decimal forms have at least 3 fields */
  endp = NULL;
  first = (OM_uint32) strtol (p, &endp, 10);
  if (endp <= p || (*endp == 0))
    return;			/* can't parse the number */
  p = endp + 1;
  endp = NULL;
  second = (OM_uint32) strtol (p, &endp, 10);
  if (endp <= p || (*endp == 0))
    return;			/* can't parse the number */
  oid->length = 0;
  oid->elements = (void *) ilugss_malloc(capacity = 16);
  ((unsigned char *)(oid->elements))[oid->length] = (unsigned char) first * 40 + second;
  oid->length++;
  /* convert from dotted decimal form */
  p = endp + 1;
  while (*p && (other = (OM_uint32) strtol (p, &endp, 10), (endp > p)))
    {
      if ((other & 0xF0000000) != 0)
	AddOther (oid, &capacity, (other & 0xF0000000) >> 28, 0);
      if ((other & 0xFFE00000) != 0)
	AddOther (oid, &capacity, (other & 0x0FE00000) >> 21, 0);
      if ((other & 0xFFFFC000) != 0)
	AddOther (oid, &capacity, (other & 0x001FC000) >> 14, 0);
      if ((other & 0xFFFFFF80) != 0)
	AddOther (oid, &capacity, (other & 0x00003F80) >> 7, 0);
      AddOther (oid, &capacity, other & 0x0000007F, 1);
      if (endp >= ((char *)(str->value) + str->length))
	break;
      p = endp + 1;
    }
  return;
}

static void parse_standard_oid_string (gss_OID oid, gss_buffer_t str)
{
  unsigned char *oidvec = NULL;
  unsigned long first;
  unsigned long second;
  unsigned long current;
  unsigned long element = 0;
  int ndigits;
  unsigned char tmpbuf[5];
  int length = 0;
  int vecsize = 0;
  int index;
  char *s = (char *) str->value;

  oid->length = 0;
  oid->elements = 0;

  oidvec = (unsigned char *) malloc(10 * sizeof(unsigned char));
  if (oidvec == NULL)
    return;
  length = 10;
  vecsize = 0;

  for (index = 0;  index < str->length && s[index] != '\0';)
    {
      current = 0;
      if (isdigit(s[index]))
	{
	  /* extract next number, append it to vector */
	  while (isdigit(s[index]) && (index < str->length)) {
	    current *= 10;
	    current += s[index] - '0';
	    index++;
	  }
	  printf ("element[%d] is %u\n", element, current);
	  if (element == 0)
	    first = current;
	  else if (element == 1)
	    {
	      oidvec[vecsize++] = first * 40 + current;
	    }
	  else
	    {
	      ndigits = 0;
	      while (current > 0)
		{
		  tmpbuf[ndigits++] = current & 0x7f;
		  current >>= 7;
		}
	      if ((vecsize + ndigits) >= length)
		{
		  length = vecsize + ndigits + 10;
		  oidvec = (unsigned char *) realloc(oidvec, length);
		  if (oidvec == NULL)
		    return;
		}
	      while (ndigits > 1)
		oidvec[vecsize++] = tmpbuf[--ndigits] | 0x80;
	      oidvec[vecsize++] = tmpbuf[0];
	    }
	  element += 1;
	  if (index < str->length && s[index] == '.')
	    index += 1;
	}
      else
	{
	  printf ("malformed OID buffer:  \"%s\"\n", s);
	  ilugss_free (oidvec);
	  return;
	}
    }
  oid->length = vecsize;
  oid->elements = (void *) realloc(oidvec, vecsize * sizeof(unsigned char));
}

static void parse_oid_string (gss_OID oid, gss_buffer_t str)
{
  if (str->length < 1)
    return;
  if (((char *)(str->value))[0] == '{')
    parse_standard_oid_string (oid, str);
  else if (isdigit(((char *)(str->value))[0]))
    parse_dotted_decimal_oid_string (oid, str);
  else
    DEBUG(ERR_DB, "Bad string-form OID passed:  %*.*s\n",
	  str->length, str->length, (char *) str->value);
}

OM_uint32 gss_str_to_oid (
     OM_uint32 		*minor_status,
     gss_buffer_t	oid_str,
     gss_OID 		*oid
)
{
  GSS_STATIC_INIT;
  *minor_status=0;

  *oid = (gss_OID) malloc(sizeof(gss_OID_desc));
  if (*oid == NULL) {
    return GSS_S_FAILURE;
  }
  (*oid)->length = 0;
  (*oid)->elements = 0;
  parse_oid_string (*oid, oid_str);
  if ((*oid)->length == 0)
    {
      ilugss_free (*oid);
      return GSS_S_FAILURE;
    }
  return GSS_S_COMPLETE;
}

OM_uint32 gss_oid_to_str (
     OM_uint32 		*minor_status,
     gss_OID		oid,
     gss_buffer_t	oid_str
)
{
  int length, outused, inused;
  OM_uint32 value;
  unsigned char *values;
  char *str;

  GSS_STATIC_INIT;
  *minor_status=0;
  if (oid == NULL || oid->length < 1)
    return GSS_S_FAILURE;
  values = (unsigned char *) oid->elements;
  length = 40;
  str = malloc(length);
  if (str == NULL)
    return GSS_S_FAILURE;
  sprintf(str, "%u.%u", values[0] / 40, values[0] - ((values[0] / 40) * 40));
  outused = strlen(str);
  inused = 1;

  while (inused < oid->length)
    {
      value = values[inused++] & 0x7F;
      while (values[inused-1] > 0x7f && (inused < oid->length))
	{
	  value = (value << 7) + (values[inused++] & 0x7F);
	}

      if ((length - outused) < 10)
	{
	  length = outused + 20;
	  str = realloc(str, length);
	  if (str == NULL)
	    return GSS_S_FAILURE;
	}
      sprintf (str + outused, ".%u", value);
      outused = strlen(str);
    }

  oid_str->length = outused;
  oid_str->value = str;
  return GSS_S_COMPLETE;
}

OM_uint32 gss_inquire_names_for_mech (
     OM_uint32		*minor_status,
     gss_OID		mechanism,
     gss_OID_set	*name_types
)
{
     GSS_STATIC_INIT;
     *minor_status=0;
     /* TODO: fill this in! */
     return GSS_S_FAILURE;
}


OM_uint32 gss_inquire_mechs_for_name (
     OM_uint32		*minor_status,
     gss_name_t		input_name,
     gss_OID_set	*mech_types
)
{
     GSS_STATIC_INIT;
     *minor_status=0;
     /* TODO: fill this in! */
     return GSS_S_FAILURE;
}


/* V1 implementations of various V. 2 functions: just call-throughs to the
 * real (V.2) implementations, provided for backwards linking compatibility
 */

OM_uint32 gss_sign
(OM_uint32	*minor_status,
 gss_ctx_id_t	context_handle,
 int		qop_req,
 gss_buffer_t	message_buffer,
 gss_buffer_t	message_token
 )
{
     return gss_get_mic(minor_status,context_handle,qop_req,message_buffer,
			message_token);
}

OM_uint32 gss_verify
(OM_uint32	*minor_status,
 gss_ctx_id_t	context_handle,
 gss_buffer_t	message_buffer,
 gss_buffer_t	token_buffer,
 int		*qop_state
 )
{
     return gss_verify_mic(minor_status,context_handle,message_buffer,
			   token_buffer,(gss_qop_t *) qop_state);
}

OM_uint32 gss_seal
(OM_uint32	*minor_status,
 gss_ctx_id_t	context_handle,
 int		conf_req_flag,
 int		qop_req,
 gss_buffer_t	input_message_buffer,
 int		*conf_state,
 gss_buffer_t	output_message_buffer
 )
{
     return gss_wrap(minor_status,context_handle,conf_req_flag,qop_req,
		     input_message_buffer,conf_state,output_message_buffer);
}

OM_uint32 gss_unseal
(OM_uint32	*minor_status,
 gss_ctx_id_t	context_handle,
 gss_buffer_t	input_message_buffer,
 gss_buffer_t	output_message_buffer,
 int		*conf_state,
 int		*qop_state
 )
{
     return gss_unwrap(minor_status,context_handle,input_message_buffer,
		       output_message_buffer,conf_state,
		       (gss_qop_t *) qop_state);
}

#define MAXERRLEN 512

/* _ilugss_strerror() now deprecated and provided only for backwards linker
 * compatibility
 * apps should only use gss_display_status() now.
 */
static char *_ilugss_strerror(OM_uint32 status_code)
{
     static char err_string[MAXERRLEN];
     gss_buffer_desc errbuf;
     OM_uint32 minor_code,msg_context=0;
     OM_uint32 display_status_result;
	 display_status_result = gss_display_status(&minor_code,status_code,
					  GSS_C_GSS_CODE,NULL,
					  &msg_context,
					  &errbuf);
     assert(!GSS_ERROR(display_status_result));
     assert(errbuf.length < MAXERRLEN);
     memcpy(err_string,errbuf.value,errbuf.length);
     ilugss_free (errbuf.value);

     return err_string;
}
