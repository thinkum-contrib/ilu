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

$Id: nil_scheme.c,v 1.14 1999/08/03 01:59:31 janssen Exp $
*/
/*
 * gss_nil_scheme.c -- implementation of "nil" security layer for GSS
 *
 * Note:  verify_mic() and get_mic() are not yet implemented.
 *
 * Bill Janssen, Feb 96
 */

#include <stdio.h>
#include <string.h>
#include <time.h>
#include <gssapi.h>
#include <assert.h>

#include "ilugssmech_nil.h"
#include "ilugssns_anonymous.h"

#include "ilugss_impl.h"
#include "ilugss_util.h"

typedef OM_uint32 nil_time_t;
typedef OM_uint32 nil_flagbits_t;

typedef int boolean;

#define TRUE  1
#define FALSE 0

gss_OID_desc ilugssmech_nil_OID[] = { ILUGSSMECH_NIL_OID };

typedef struct {
  nil_time_t endtime;
  gss_name_t name;
  int usage;
} nil_cred;

typedef struct {
  gss_name_t server;
  gss_name_t client;
  nil_time_t endtime;
  nil_flagbits_t flags;
  boolean locally_initiated;
  boolean complete;
} nil_context;

typedef struct {
  void *data;
  void *next;
} linked_list_elt;

static linked_list_elt *known_creds = NULL;
static linked_list_elt *known_contexts = NULL;

static int validate_handle (linked_list_elt *list, void * handle)
{
  linked_list_elt *p;

  for (p = list;  p != NULL;  p = p->next)
    if (p->data == handle)
      return 1;
  return 0;
}

#define validate_cred_handle(h)		validate_handle(known_creds, (h))
#define validate_context_handle(h)	validate_handle(known_contexts, (h))

static void *remove_elt (linked_list_elt **list, void *elt)
{
  linked_list_elt *p, *previous;
  void *data;

  for (previous = NULL, p = *list;  p != NULL;  previous = p, p = p->next)
    if (p->data == elt)
      {
	if (previous == NULL)
	  *list = p->next;
	else
	  previous->next = p->next;
	p->next = NULL;
	data = p->data;
	ilugss_free (p);
	return data;
      }
  return NULL;
}

static void *add_elt (linked_list_elt **list, void *elt)
{
  linked_list_elt *p;

  if (elt != NULL)
    {
      p = (linked_list_elt *) ilugss_malloc(sizeof(*p));
      p->data = elt;
      p->next = *list;
      *list = p;
      return elt;
    }
  else
    return NULL;
}

#define add_cred_handle(h)		add_elt(&known_creds, (h))
#define add_context_handle(h)		add_elt(&known_contexts, (h))
#define remove_cred_handle(h)		remove_elt(&known_creds, (h))
#define remove_context_handle(h)	remove_elt(&known_contexts, (h))

static OM_uint32
  acquire_cred (OM_uint32 *	minor_status,
		gss_name_t	desired_name,
		OM_uint32	time_req,
		int		cred_usage,
		gss_opaque_t *	output_cred_handle,
		OM_uint32 *	time_rec)
{
  nil_cred temp;
  nil_time_t now;
  OM_uint32 minor;

  if ((cred_usage != GSS_C_BOTH) &&
      (cred_usage != GSS_C_INITIATE) &&
      (cred_usage != GSS_C_ACCEPT))
    return GSS_S_FAILURE;

  temp.name = ilugss_copy_name(desired_name);
  if (temp.name == NULL)
    return GSS_S_FAILURE;
  if (time_req != 0 && (time_req != GSS_C_INDEFINITE))
    {
      now = time(NULL);
      temp.endtime = now + time_req;
    }
  else
    temp.endtime = GSS_C_INDEFINITE;
  temp.usage = cred_usage;

  *minor_status = 0;
  if ((*output_cred_handle = (gss_opaque_t) ilugss_malloc(sizeof(nil_cred))) == NULL)
    {
      (void) gss_release_name(&minor, &temp.name);
      return GSS_S_FAILURE;
    }

  add_cred_handle(*output_cred_handle);

  *((nil_cred *)(*output_cred_handle)) = temp;
  if (time_rec != NULL)
    {
      if (time_req != 0 && (time_req != GSS_C_INDEFINITE))
	{
	  *time_rec = temp.endtime - now;
	}
      else
	*time_rec = GSS_C_INDEFINITE;
    }
  return GSS_S_COMPLETE;
}

static OM_uint32
  release_cred (OM_uint32 *	minor_status,
		gss_opaque_t *	cred_handle)
{
  OM_uint32 major, minor;

  if (validate_cred_handle(cred_handle))
    {
      remove_cred_handle(cred_handle);
      major = gss_release_name(&minor, &((nil_cred *) *cred_handle)->name);
      ilugss_free (*cred_handle);
      *cred_handle = NULL;
      return GSS_S_COMPLETE;
    }
  else
    return GSS_S_NO_CRED;
}

typedef struct {
  unsigned char flags[4];
  unsigned char endtime[4];
  unsigned char caller_name_len[4];
  unsigned char namespace_oid_len[4];
} marshalled_context;

static void marshal (OM_uint32 value, unsigned char buf[4])
{
#ifdef WORDS_BIGENDIAN
  *((OM_uint32 *) buf) = value;
#else /* not bigendian */
  buf[0] = ((unsigned char *) &value)[3];
  buf[1] = ((unsigned char *) &value)[2];
  buf[2] = ((unsigned char *) &value)[1];
  buf[3] = ((unsigned char *) &value)[0];
#endif
}

static void unmarshal (OM_uint32 *value, unsigned char buf[4])
{
#ifdef WORDS_BIGENDIAN
  *value = *((OM_uint32 *) buf);
#else /* not bigendian */
  ((unsigned char *) value)[3] = buf[0];
  ((unsigned char *) value)[2] = buf[1];
  ((unsigned char *) value)[1] = buf[2];
  ((unsigned char *) value)[0] = buf[3];
#endif
}

static nil_cred *get_default_credentials(void)
{
  static nil_cred *the_cred = NULL;

  if (the_cred == NULL)
    {
      gss_name_t anon = ilugssns_anonymous_default_name();
      OM_uint32 major, minor;

      if (anon == NULL)
	return NULL;
      major = acquire_cred(&minor, anon, GSS_C_INDEFINITE, GSS_C_BOTH, (gss_opaque_t *) &the_cred, NULL);
      (void) gss_release_name(&minor, &anon);
      if (major != GSS_S_COMPLETE)
	return NULL;
    }
  return the_cred;
}

static OM_uint32	init_sec_context
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
     )
{
  nil_context ctx;
  nil_cred *cred;
  gss_opaque_t input_context = *context_handle;
  nil_time_t endtime;
  gss_buffer_desc namebuf;
  gss_OID namespace_oid;
  OM_uint32 minor;

  *context_handle = GSS_C_NO_CONTEXT;

  if ((input_token!=GSS_C_NO_BUFFER) && (input_token->length!=0)) {
    return GSS_S_DEFECTIVE_TOKEN;
  }

  if (input_context != GSS_C_NO_CONTEXT)
    return GSS_S_NO_CONTEXT;

  if (claimant_cred_handle == GSS_C_NO_CREDENTIAL)
    {
      if ((cred = get_default_credentials()) == NULL)
	return GSS_S_FAILURE;
    }
  else if (!validate_cred_handle(claimant_cred_handle))
    return GSS_S_DEFECTIVE_CREDENTIAL;
  else
    cred = (nil_cred *) claimant_cred_handle;

  if (cred->usage == GSS_C_ACCEPT)
    return GSS_S_NO_CRED;
  
  if ((ctx.server = ilugss_copy_name(target_name)) == NULL)
    return GSS_S_BAD_NAME;

  if ((req_flags & GSS_C_ANON_FLAG) != 0)
    ctx.client = ilugssns_anonymous_default_name();
  else
    ctx.client = ilugss_copy_name(cred->name);
  if (ctx.client == NULL)
    return GSS_S_NO_CRED;

  ctx.flags = (req_flags & (GSS_C_ANON_FLAG));

  if ((time_req != GSS_C_INDEFINITE) && (time_req != 0))
    {
      ctx.endtime = time_req + time(NULL);
      if ((cred->endtime != GSS_C_INDEFINITE) &&
	  (ctx.endtime > cred->endtime))
	ctx.endtime = cred->endtime;
    }
  else
    ctx.endtime = GSS_C_INDEFINITE;
  if (time_rec != NULL)
    {
      if (ctx.endtime != GSS_C_INDEFINITE)
	*time_rec = (OM_uint32) (ctx.endtime - time(NULL));
      else
	*time_rec = GSS_C_INDEFINITE;
    }
  if (ret_flags != NULL)
    *ret_flags = ctx.flags;

  /* now create the output token */

  if (gss_display_name (&minor, cred->name, &namebuf, &namespace_oid) != GSS_S_COMPLETE)
    return GSS_S_DEFECTIVE_CREDENTIAL;

  output_token->length = sizeof(marshalled_context) + namebuf.length + namespace_oid->length;
  if ((output_token->value = ilugss_malloc(output_token->length)) == NULL)
    {
      output_token->length = 0;
      ilugss_free (namebuf.value);
      return GSS_S_FAILURE;
    }
  marshal (ctx.flags, ((marshalled_context *)(output_token->value))->flags);
  marshal (ctx.endtime, ((marshalled_context *)(output_token->value))->endtime);
  marshal (namebuf.length, ((marshalled_context *)(output_token->value))->caller_name_len);
  marshal (namespace_oid->length, ((marshalled_context *)(output_token->value))->namespace_oid_len);
  memcpy ((char *)(output_token->value) + sizeof(marshalled_context), namebuf.value, namebuf.length);
  memcpy ((char *)(output_token->value) + sizeof(marshalled_context) + namebuf.length,
	  (char *)(namespace_oid->elements), namespace_oid->length);
  ilugss_free(namebuf.value);

  if ((*context_handle = (gss_opaque_t) ilugss_malloc(sizeof(ctx))) == NULL)
    {
      ilugss_free (output_token->value);
      return GSS_S_FAILURE;
    }
  ctx.complete = TRUE;
  ctx.locally_initiated = TRUE;
  *((nil_context *)(*context_handle)) = ctx;
  add_context_handle(*context_handle);

  return GSS_S_COMPLETE;
}

static OM_uint32	accept_sec_context
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
     )
{
  gss_opaque_t input_context = *context_handle;
  gss_name_t caller;
  gss_buffer_desc namebuf;
  gss_OID_desc namespace_oid;
  nil_context ctx;
  nil_cred *cred;
  OM_uint32 major, minor;

  *context_handle = GSS_C_NO_CONTEXT;

  if (input_context != GSS_C_NO_CONTEXT)
    return GSS_S_NO_CONTEXT;

  if ((input_token_buffer == GSS_C_NO_BUFFER) || (input_token_buffer->length < sizeof(marshalled_context)))
    return GSS_S_DEFECTIVE_TOKEN;

  if (verifier_cred_handle == GSS_C_NO_CREDENTIAL)
    {
      if ((cred = get_default_credentials()) == NULL)
	return GSS_S_FAILURE;
    }
  else if (!validate_cred_handle(verifier_cred_handle))
    return GSS_S_DEFECTIVE_CREDENTIAL;
  else
    cred = (nil_cred *) verifier_cred_handle;

  if (cred->usage == GSS_C_INITIATE)
    return GSS_S_NO_CRED;
  
  if ((ctx.server = ilugss_copy_name(cred->name)) == NULL)
    return GSS_S_DEFECTIVE_CREDENTIAL;

  unmarshal (&major, ((marshalled_context *) (input_token_buffer->value))->caller_name_len);
  namebuf.length = major;
  unmarshal (&major, ((marshalled_context *) (input_token_buffer->value))->namespace_oid_len);
  namespace_oid.length = major;
  if (input_token_buffer->length < (sizeof(marshalled_context) + namebuf.length + namespace_oid.length))
    {
      (void) gss_release_name(&minor, &ctx.server);
      return GSS_S_DEFECTIVE_TOKEN;
    }
  unmarshal (&major, ((marshalled_context *) (input_token_buffer->value))->flags);
  ctx.flags = major;
  unmarshal (&major, ((marshalled_context *) (input_token_buffer->value))->endtime);
  ctx.endtime = major;
  if ((namebuf.value = ilugss_malloc(namebuf.length)) == NULL)
    {
      (void) gss_release_name(&minor, &ctx.server);
      return GSS_S_FAILURE;
    }
  memcpy ((char *) namebuf.value,
	  ((char *) (input_token_buffer->value)) + sizeof(marshalled_context),
	  namebuf.length);
  if ((namespace_oid.elements = ilugss_malloc(namespace_oid.length)) == NULL)
    {
      (void) gss_release_name(&minor, &ctx.server);
      ilugss_free (namebuf.value);
      return GSS_S_FAILURE;
    }
  memcpy ((char *) (namespace_oid.elements),
	  ((char *) (input_token_buffer->value)) + sizeof(marshalled_context) + namebuf.length,
	  namespace_oid.length);
  ctx.client = (gss_name_t) NULL;
  if ((major = gss_import_name (&minor, &namebuf, &namespace_oid, &ctx.client)) != GSS_S_COMPLETE)
    {
      (void) gss_release_name(&minor, &ctx.server);
      ilugss_free (namebuf.value);
      ilugss_free (namespace_oid.elements);
      return GSS_S_DEFECTIVE_TOKEN;
    }
  (void) gss_release_buffer (&minor, &namebuf);
  ilugss_free (namespace_oid.elements);

  if (src_name != NULL)
    {
      if ((*src_name = ilugss_copy_name(ctx.client)) == NULL)
	{
	  (void) gss_release_name(&minor, &ctx.server);
	  (void) gss_release_name(&minor, &ctx.client);
	  return GSS_S_FAILURE;
	}
    }

  if ((*context_handle = (gss_opaque_t) ilugss_malloc(sizeof(ctx))) == NULL)
    {
      if (src_name != NULL)
	(void) gss_release_name (&minor, src_name);
      (void) gss_release_name(&minor, &ctx.server);
      (void) gss_release_name(&minor, &ctx.client);
      return GSS_S_FAILURE;
    }
  ctx.complete = TRUE;
  ctx.locally_initiated = FALSE;
  *((nil_context *)(*context_handle)) = ctx;

  add_context_handle (*context_handle);

  if (ret_flags != NULL)
    *ret_flags = ctx.flags;

  if (time_rec != NULL)
    {
      if (ctx.endtime != GSS_C_INDEFINITE)
	*time_rec = ctx.endtime - time(NULL);
      else
	*time_rec = GSS_C_INDEFINITE;
    }

  return GSS_S_COMPLETE;
}

static OM_uint32	process_context_token
(OM_uint32		*minor_status,
 gss_opaque_t		context_handle,
 gss_buffer_t		token_buffer
     )
{
  return GSS_S_FAILURE;
}

static OM_uint32	delete_sec_context
(OM_uint32		*minor_status,
 gss_opaque_t		*context_handle,
 gss_buffer_t		output_token
     )
{
  nil_context *ctx;
  OM_uint32 minor;

  if (output_token != GSS_C_NO_BUFFER)
    output_token->length = 0;
  if (*context_handle == GSS_C_NO_CONTEXT)
    return GSS_S_COMPLETE;
  if (!validate_context_handle(*context_handle))
    return GSS_S_NO_CONTEXT;
  ctx = (nil_context *) (*context_handle);
  remove_context_handle((void *) ctx);
  (void) gss_release_name (&minor, &ctx->client);
  (void) gss_release_name (&minor, &ctx->server);
  ilugss_free (ctx);
  return GSS_S_COMPLETE;
}

static OM_uint32
  check_context (OM_uint32 *minor,
		 gss_opaque_t ctx)
{
  nil_context *c;

  *minor = 0;
  if (!validate_context_handle(ctx))
    return GSS_S_NO_CONTEXT;
  c = (nil_context *) ctx;
  if (c->endtime != GSS_C_INDEFINITE)
    {
      nil_time_t now = time(NULL);
      if (c->endtime <= now)
	return GSS_S_CONTEXT_EXPIRED;
    }
  return GSS_S_COMPLETE;
}

static OM_uint32
  check_cred (OM_uint32 *minor,
	      gss_opaque_t ctx)
{
  nil_cred *c;

  *minor = 0;
  if (!validate_cred_handle(ctx))
    return GSS_S_NO_CRED;
  c = (nil_cred *) ctx;
  if (c->endtime != GSS_C_INDEFINITE)
    {
      nil_time_t now = time(NULL);
      if (c->endtime <= now)
	return GSS_S_CREDENTIALS_EXPIRED;
    }
  return GSS_S_COMPLETE;
}

static OM_uint32	context_time
(OM_uint32		*minor_status,
 gss_opaque_t		context_handle,
 OM_uint32		*time_rec
     )
{
  OM_uint32 major, minor;
  nil_context *ctx;

  *time_rec = 0;
  if ((major = check_context(&minor, context_handle)) != GSS_S_COMPLETE)
    return major;
    
  ctx = (nil_context *) context_handle;
  if (ctx->endtime != GSS_C_INDEFINITE)
    {
      nil_time_t now = time(NULL);
      *time_rec = ctx->endtime - now;
    }
  return GSS_S_COMPLETE;
}

static OM_uint32	get_mic
(OM_uint32		*minor_status,
 gss_opaque_t		context_handle,
 gss_qop_t		qop_req,
 gss_buffer_t		message_buffer,
 gss_buffer_t		message_token
     )
{
  OM_uint32 major, minor;

  if ((major = check_context(&minor, context_handle)) != GSS_S_COMPLETE)
    return major;
  return GSS_S_FAILURE;
}

static OM_uint32	verify_mic
(OM_uint32		*minor_status,
 gss_opaque_t		context_handle,
 gss_buffer_t		message_buffer,
 gss_buffer_t		token_buffer,
 gss_qop_t		*qop_state
     )
{
  OM_uint32 major, minor;

  if ((major = check_context(&minor, context_handle)) != GSS_S_COMPLETE)
    return major;
  return GSS_S_FAILURE;
}

static OM_uint32	wrap
(OM_uint32		*minor_status,
 gss_opaque_t		context_handle,
 int			conf_req_flag,
 gss_qop_t		qop_req,
 gss_buffer_t		input_message_buffer,
 int			*conf_state,
 gss_buffer_t		output_message_buffer
     )
{
  OM_uint32 major, minor;

  if ((major = check_context(&minor, context_handle)) != GSS_S_COMPLETE)
    return major;
  output_message_buffer->length=input_message_buffer->length;
  output_message_buffer->value=
    ilugss_malloc(output_message_buffer->length);
  memcpy(output_message_buffer->value,input_message_buffer->value,
	 input_message_buffer->length);
  return GSS_S_COMPLETE;
}

static OM_uint32	unwrap
(OM_uint32		*minor_status,
 gss_opaque_t		context_handle,
 gss_buffer_t		input_message_buffer,
 gss_buffer_t		output_message_buffer,
 int			*conf_state,
 gss_qop_t		*qop_state
     )
{
  OM_uint32 major, minor;

  if ((major = check_context(&minor, context_handle)) != GSS_S_COMPLETE)
    return major;
  output_message_buffer->length=input_message_buffer->length;
  output_message_buffer->value=
    ilugss_malloc(output_message_buffer->length);
  memcpy(output_message_buffer->value,input_message_buffer->value,
	 input_message_buffer->length);
  return GSS_S_COMPLETE;
}

static OM_uint32	display_status
(OM_uint32		*minor_status,
 OM_uint32		status_value,
 OM_uint32		*message_context,
 gss_buffer_t		status_string
     )
{
  return GSS_S_BAD_STATUS;
}

static OM_uint32	inquire_cred
(OM_uint32		*minor_status,
 gss_opaque_t		cred_handle,
 gss_name_t		*name,
 OM_uint32		*lifetime,
 int			*cred_usage
     )
{
  OM_uint32 major, minor;
  nil_cred *cred;

  if ((major = check_cred(&minor, cred_handle)) != GSS_S_COMPLETE)
    return major;

  cred = (nil_cred *) cred_handle;
  if (name != NULL)
    {
      if ((*name = ilugss_copy_name(cred->name)) == NULL)
	return GSS_S_FAILURE;
    }
  if (lifetime != NULL)
    {
      if (cred->endtime != GSS_C_INDEFINITE)
	*lifetime = cred->endtime - time(NULL);
      else
	*lifetime = GSS_C_INDEFINITE;
    }
  if (cred_usage != NULL)
    *cred_usage = cred->usage;

  return GSS_S_COMPLETE;
}

static OM_uint32 inquire_context
  (OM_uint32	*minor_status,
   gss_opaque_t context_handle,
   gss_name_t 	*src_name,
   gss_name_t 	*targ_name,
   OM_uint32 	*lifetime_rec,
   OM_uint32 	*ctx_flags,
   int 		*locally_initiated,
   int 		*open)
{
  OM_uint32 major, minor;
  nil_context *context;

  if ((major = check_context(&minor, context_handle)) != GSS_S_COMPLETE)
    return major;

  context = (nil_context *) context_handle;
  if (src_name != NULL)
    {
      if ((*src_name = ilugss_copy_name(context->client)) == NULL)
	return GSS_S_FAILURE;
    }
  if (targ_name != NULL)
    {
      if ((*targ_name = ilugss_copy_name(context->server)) == NULL)
	return GSS_S_FAILURE;
    }
  if (lifetime_rec != NULL)
    {
      if (context->endtime != GSS_C_INDEFINITE)
	*lifetime_rec = context->endtime - time(NULL);
      else
	*lifetime_rec = GSS_C_INDEFINITE;
    }
  if (ctx_flags != NULL)
    *ctx_flags = context->flags;
  if (locally_initiated != NULL)
    *locally_initiated = context->locally_initiated;
  if (open != NULL)
    *open = context->complete;

  return GSS_S_COMPLETE;
}

static OM_uint32 wrap_size_limit
  (OM_uint32	*minor_status,
   gss_opaque_t	context_handle,
   int        	conf_req_flag,
   gss_qop_t	qop_req,
   OM_uint32	req_output_size,
   OM_uint32	*max_input_size)
{
  /* since wrap just does a copy, we know that the sizes are the same. */
  *max_input_size = req_output_size;
  return GSS_S_COMPLETE;
}


static gss_scheme_t scm = {
     ilugssmech_nil_OID,
     acquire_cred,
     release_cred,
     init_sec_context,
     accept_sec_context,
     process_context_token,
     delete_sec_context,
     context_time,
     get_mic,
     verify_mic,
     wrap,
     unwrap,
     display_status,
     inquire_cred,
     inquire_context,
     wrap_size_limit
};
   
/* initialise routine: */
void ilugssmech_nil_initialize(void)
{
  known_creds = NULL;
  known_contexts = NULL;

  ilugss_register_scheme (&scm, "nil");
}
