/** $Id: identity.c,v 1.32 1999/08/03 01:53:02 janssen Exp $
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
*/
/* Last edited by Mike Spreitzer October 16, 1998 10:35 pm PDT */

#include "iluntrnl.h"

#include "connect.h"
#include "call.h"

struct _ilu_Passport_s {
  struct ilu_vector_s v;
};

#define MAX_IDENTITY_TYPES	10

static ilu_IdentityType IdentityTypes[MAX_IDENTITY_TYPES] = {
  &ilu_NoIdentity_s,
  &ilu_ConnectionIdentity_s,
#ifdef SECURE_TRANSPORT
  &ilu_GSSIdentity_s,
#endif /* SECURE_TRANSPORT */
#ifdef SUNRPC_PROTOCOL
  &ilu_SunRPCAuthUnixIdentity_s,
#endif /* SUNRPC_PROTOCOL */
#ifdef W3MUX_TRANSPORT
  &ilu_w3muxEndpointIdentity_s,
#endif /* W3MUX_TRANSPORT */
  NIL };

/* Locking unconstrained */

ilu_boolean
  ilu_RegisterIdentityType (struct _ilu_IdentityType_s *type,
			    ilu_Error *err)
{
  int i;

  for (i = 0;  i < MAX_IDENTITY_TYPES;  i++)
    {
      if (IdentityTypes[i] == NIL)
	{ 
	  ILU_NOTE(AUTHENTICATION_DEBUG,
		("ilu_RegisterIdentityType:  registered identity type \"%s\".\n",
		 type->it_name));
	  IdentityTypes[i] = type;
	  return ILU_CLER(*err);
	}
      else if (strcmp(IdentityTypes[i]->it_name, type->it_name) == 0)
	{
	  ILU_NOTE(AUTHENTICATION_DEBUG,
		("ilu_RegisterIdentityType:  identity type \"%s\" already registered.\n",
		 type->it_name));
	  return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_identity_type_registered, ilu_FALSE);
	}
    }
  ILU_NOTE(AUTHENTICATION_DEBUG,
	("ilu_RegisterIdentityType:  too many identity types, registration of \"%s\" failed."
	 "  Max is %d.\n",
	 type->it_name, MAX_IDENTITY_TYPES));
  return ILU_ERR_CONS1(imp_limit, err, minor, ilu_ilm_max_identity_types, ilu_FALSE);
}

ilu_IdentityType
  ilu_FindIdentityTypeByName (char *name,
			      ilu_Error *err)
{
  int i;

  ILU_CLER(*err);
  for (i = 0;  i < MAX_IDENTITY_TYPES;  i++)
    if ((IdentityTypes[i] != NIL) && (strcmp(IdentityTypes[i]->it_name, name) == 0))
      return IdentityTypes[i];
  return NIL;
}

/*======================================================================

  NoIdentity

======================================================================*/

static ilu_cardinal
  _ilu_NoIdentity_StringForm (ilu_refany info,
			      char *buffer,
			      ilu_cardinal bufferlen,
			      ILU_ERRS((bad_param, no_memory)) *err)
{
  if (bufferlen > 0)
    { *buffer = 0; ILU_CLER(*err); return 0; }
  else
    return (ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_small_buffer, 0));
}

static ilu_refany
  _ilu_NoIdentity_DuplicateData (ilu_refany info,
				 ilu_Error *err)
{
  ILU_CLER(*err);
  return NIL;
}

static void
  _ilu_NoIdentity_FreeData (ilu_refany info,
			    ilu_Error *err)
{
  ILU_CLER(*err);
}

struct _ilu_IdentityType_s ilu_NoIdentity_s = {
  "NoIdentity",
  _ilu_NoIdentity_StringForm,
  _ilu_NoIdentity_DuplicateData,
  _ilu_NoIdentity_FreeData,
  NULLFN, NULLFN };
  
/*======================================================================

  OpaqueIdentity

======================================================================*/

struct _ilu_OpaqueIdentity_Data {
  ilu_string name;
  ilu_cardinal length;
  ilu_bytes bytes;
  ilu_cardinal refcount;
};

static ilu_cardinal
  _ilu_OpaqueIdentity_StringForm (ilu_refany info,
				  char *buffer,
				  ilu_cardinal bufferlen,
				  ilu_Error *err)
{
  struct _ilu_OpaqueIdentity_Data *data = (struct _ilu_OpaqueIdentity_Data *) info;
  char buf[1000];
  ilu_cardinal min;

  ilu_cardinal crc = ilu_CRC32(data->bytes, data->length);
  sprintf (buf, "<OpaqueIdentity:%.500s:crc=%08lx:length=%u:refcount=%u>",
	   data->name, (long unsigned int) crc, data->length, data->refcount);
  min = ((strlen(buf) + 1) >= bufferlen) ? bufferlen - 2 : strlen(buf) - 1;
  strncpy(buffer, buf, min);
  buffer[min] = '>';
  buffer[min+1] = 0;
  ILU_CLER(*err);
  return min + 1;
}

static ilu_refany
  _ilu_OpaqueIdentity_DuplicateData (ilu_refany info,
				     ilu_Error *err)
{
  struct _ilu_OpaqueIdentity_Data *data = (struct _ilu_OpaqueIdentity_Data *) info;
  data->refcount++;
  ILU_CLER(*err);
  return info;
}

static void
  _ilu_OpaqueIdentity_FreeData (ilu_refany info,
				ilu_Error *err)
{
  struct _ilu_OpaqueIdentity_Data *data = (struct _ilu_OpaqueIdentity_Data *) info;
  ilu_free(data);
  ILU_CLER(*err);
}

static ilu_cardinal
  _ilu_OpaqueIdentity_Pickle (ilu_refany info,
			      ilu_bytes *output_buffer,
			      ilu_cardinal output_buffer_len,
			      ILU_ERRS((no_memory, bad_param)) *err)
{
  struct _ilu_OpaqueIdentity_Data *data = (struct _ilu_OpaqueIdentity_Data *) info;
  ilu_cardinal needed;

  needed = 5 + data->length + strlen(data->name);
  if (*output_buffer == NIL) {
    *output_buffer = ilu_MallocE(needed, err);
    if (ILU_ERRNOK(*err)) return 0;
    (*output_buffer)[0] = (data->length >> 24) & 0xFF;
    (*output_buffer)[1] = (data->length >> 16) & 0xFF;
    (*output_buffer)[2] = (data->length >> 8) & 0xFF;
    (*output_buffer)[3] = data->length & 0xFF;
    strcpy(((char *) ((*output_buffer) + 4)), data->name);
    memcpy((void *) ((*output_buffer) + 4 + strlen(data->name) + 1),
	   (void *) data->bytes, data->length);
    return needed;
  } else if (output_buffer_len < needed) {
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_small_buffer, 0);
  } else {
    (*output_buffer)[0] = (data->length >> 24) & 0xFF;
    (*output_buffer)[1] = (data->length >> 16) & 0xFF;
    (*output_buffer)[2] = (data->length >> 8) & 0xFF;
    (*output_buffer)[3] = data->length & 0xFF;
    strcpy(((char *) ((*output_buffer) + 4)), data->name);
    memcpy((void *) ((*output_buffer) + 4 + strlen(data->name) + 1),
	   (void *) data->bytes, data->length);
    ILU_CLER(*err);
    return needed;
  }
}

static ilu_refany
  _ilu_OpaqueIdentity_Unpickle (ilu_bytes data,
				ilu_cardinal datalen,
				ilu_Error *err)
{
  struct _ilu_OpaqueIdentity_Data *d;

  d = ilu_MallocE(sizeof(*d), err);
  if (ILU_ERRNOK(*err)) return NIL;
  d->length = (data[0] << 24) || (data[1] << 16) || (data[2] << 8) || (data[3]);
  if ((d->length + 4) != datalen)
    { ilu_free(d); return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_small_buffer, NIL); };
  d->refcount = 1;
  d->bytes = ilu_MallocE(d->length, err);
  if (ILU_ERRNOK(*err)) { ilu_free(d); return NIL; };
  memcpy((void *) d->bytes, (void *) (data + 4), d->length);
  ILU_CLER(*err);
  return d;
}

struct _ilu_IdentityType_s ilu_OpaqueIdentity_s = {
  "OpaqueIdentity",
  _ilu_OpaqueIdentity_StringForm,
  _ilu_OpaqueIdentity_DuplicateData,
  _ilu_OpaqueIdentity_FreeData,
  _ilu_OpaqueIdentity_Pickle,
  _ilu_OpaqueIdentity_Unpickle };  
  
ilu_IdentityInfo
  ilu_CreateOpaqueIdentity (ilu_string name,
			    ilu_bytes databytes,	/* RETAIN */
			    ilu_cardinal datalen,
			    ilu_Error *err)
{
  /* takes a sequence of bytes and a name, and returns an identity. */
  ilu_IdentityInfo i;
  struct _ilu_OpaqueIdentity_Data *data;
  ilu_cardinal size = strlen(name) + 5 + datalen;

  i = (ilu_IdentityInfo) ilu_MallocE(sizeof(i), err);
  if (ILU_ERRNOK(*err)) return NIL;
  data = (struct _ilu_OpaqueIdentity_Data *) ilu_MallocE(size, err);
  if (ILU_ERRNOK(*err)) { ilu_free(i); return NIL; }
  data->name = (ilu_string) data + 1;
  strcpy(data->name, name);
  data->length = datalen;
  data->bytes = ((ilu_bytes) (data + 1)) + strlen(name) + 1;
  memcpy((void *) data->bytes, (void *) databytes, datalen);
  data->refcount = 1;
  i->ii_type = &ilu_OpaqueIdentity_s;
  i->ii_owned_by_passport = ilu_FALSE;
  i->ii_info = (ilu_refany) data;
  return i;
}
  
ilu_string
  ilu_OpaqueIdentityName (ilu_IdentityInfo info,
			  ilu_Error *err)
{
  if (info == NIL ||
      info->ii_type != ilu_OpaqueIdentity)
    return (ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, NIL));
  ILU_CLER(*err);
  return ((ilu_OpaqueIdentityInfo)(info->ii_info))->name;
}

ilu_cardinal
  ilu_OpaqueIdentityBytes (ilu_IdentityInfo info,
			   ilu_bytes *ptr,
			   ilu_Error *err)
{
  if (info == NIL ||
      info->ii_type != ilu_OpaqueIdentity)
    return (ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0));
  ILU_CLER(*err);
  *ptr = ((ilu_OpaqueIdentityInfo)(info->ii_info))->bytes;
  return ((ilu_OpaqueIdentityInfo)(info->ii_info))->length;
}

/*======================================================================

  ConnectionIdentity

======================================================================*/

static ilu_cardinal
  _ilu_ConnectionIdentity_StringForm (ilu_refany info,
				      char *buffer,
				      ilu_cardinal bufferlen,
				      ilu_Error *err)
{
  ilu_string data = (ilu_string) info;
  ilu_cardinal needed = _ilu_SafeStrlen(data) + 1;

  if (bufferlen < needed)
    return (ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_small_buffer, needed));
  else
    { strcpy (buffer, data); ILU_CLER(*err); return needed + 1; }
}

static ilu_refany
  _ilu_ConnectionIdentity_DuplicateData (ilu_refany info,
					 ilu_Error *err)
{
  return (ilu_StrdupE((ilu_string) info, err));
}

static void
  _ilu_ConnectionIdentity_FreeData (ilu_refany info,
				    ilu_Error *err)
{
  ilu_free((ilu_string) info);
  ILU_CLER(*err);
}

struct _ilu_IdentityType_s ilu_ConnectionIdentity_s = {
  "ConnectionIdentity",
  _ilu_ConnectionIdentity_StringForm,
  _ilu_ConnectionIdentity_DuplicateData,
  _ilu_ConnectionIdentity_FreeData,
  NULLFN, NULLFN };
  

/*======================================================================

  Generic code

======================================================================*/

/*Main Invariant holds; L2 >= {call's conn's callmu}*/

void _ilu_AddConnIdentities (ilu_Call call, ilu_Error *err)
{
  ilu_Passport ppn = call->ca_caller;
  unsigned int n;
  ilu_IdentityInfo info;
  ilu_Vector vec;

  if (ppn == NIL)
    {
      ppn = ilu_CreatePassport(NIL, err);
      if (ILU_ERRNOK(*err)) return;
    }
  vec = (ilu_Vector) call_connection(call)->co_auth_info;
  if (vec != NIL) {
    for (n = 0;  n < vec->ve_size;  n++)
      {
	/* optimize use of non-owned identities by not copying them */
	if (((ilu_IdentityInfo)(vec->ve_elements[n]))->ii_owned_by_passport == ilu_FALSE)
	  info = (ilu_IdentityInfo)(vec->ve_elements[n]);
	else {
	  /* make copy of identity for this passport */
	  if ((info = ilu_CopyIdentity((ilu_IdentityInfo)(vec->ve_elements[n]), err)) == NIL)
	    goto errpoint;
	  info->ii_owned_by_passport = ilu_TRUE;
	}
	if (!ilu_AddIdentity(ppn, info, err))
	  goto errpoint;
      }
  }

  if (call->ca_caller == NIL)
    call->ca_caller = ppn;
  ILU_CLER(*err);
  return;

 errpoint:
  if ((call->ca_caller == NIL) && (ppn != NIL)) {
    ilu_Error lerr;
    ilu_DestroyPassport(ppn, &lerr);
    ILU_HANDLED(lerr);
  }
  return;
}

/*L1, L2, Main unconstrained*/

extern ilu_Passport
  ilu_CreatePassport (const struct _ilu_IdentityInfo_s * info,
		      ILU_ERRS((no_memory)) *err)
/* creates and returns a passport, optionally containing the specified identity */
{
  ilu_Passport p = (ilu_Passport) _ilu_vector_new(2, err);
  if (ILU_ERRNOK(*err)) return NIL;
  if (info != NIL)
    {
      ilu_AddIdentity (p, info, err);
      if (ILU_ERRNOK(*err))
	{
	  ilu_free(p);
	  p = NIL;
	}
    }
  return p;
}

extern ilu_IdentityInfo
  ilu_CopyIdentity (const struct _ilu_IdentityInfo_s *info,
		    ILU_ERRS((no_memory, bad_param)) *err)
/* allocates and returns a copy of the ilu_IdentityInfo parameter */
{
  ilu_IdentityInfo i;
  ilu_refany dupinfo;

  dupinfo = (*info->ii_type->it_duplicate_data)(info->ii_info, err);
  if (ILU_ERRNOK(*err))
    return NIL;

  i = (ilu_IdentityInfo) ilu_MallocE(sizeof(*i), err);
  if (ILU_ERRNOK(*err))
    {
      ilu_Error localerr;
      (*info->ii_type->it_free_data)(dupinfo, &localerr);
      return NIL;
    }
  i->ii_type = info->ii_type;
  i->ii_owned_by_passport = ilu_FALSE;
  i->ii_info = dupinfo;
  return i;
}

extern ilu_boolean
  ilu_AddIdentity (ilu_Passport p,
		   const struct _ilu_IdentityInfo_s *info,
		   ILU_ERRS((no_memory, bad_param)) *err)
/* added identity to Passport.  Only one identity of each type is allowed.
   Returns ILU_ERROK() of the error parameter. */
{
  ilu_Vector v = (ilu_Vector) p;
  ilu_cardinal i;

  for (i = 0;  i < v->ve_size;  i++)
    if (((ilu_IdentityInfo)(v->ve_elements[i]))->ii_type == info->ii_type)
      return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_idTypePresent, ilu_FALSE);
  _ilu_vector_add(v, (ilu_refany) info, err);
  return ILU_ERROK(*err);
}

extern ilu_IdentityInfo
  ilu_RemoveIdentity (ilu_Passport p,
		      ilu_IdentityType idtype,
		      ILU_ERRS((no_memory, bad_param)) *err)
/* Removes identity of type "idtype" from Passport if present,
   and returns it.  Returns NIL if not present. */
{
  ilu_Vector v = (ilu_Vector) p;
  ilu_IdentityInfo prev;
  ilu_cardinal i;

  for (i = 0, prev = ILU_NIL;  i < v->ve_size;  i++)
    if (((ilu_IdentityInfo)(v->ve_elements[i]))->ii_type == idtype) {
      prev = ((ilu_IdentityInfo)(v->ve_elements[i]));
      _ilu_vector_remove(v, prev);
    }
  ILU_CLER(*err);
  return prev;
}

extern ilu_boolean
  ilu_ReplaceIdentity (ilu_Passport p,
		       const struct _ilu_IdentityInfo_s *info,
		       ILU_ERRS((no_memory, bad_param)) *err)
/* adds identity to Passport, replacing previous identity of the same type
   if necessary.  Only one identity of each type is allowed.
   Returns ILU_ERROK() of the error parameter. */
{
  ilu_IdentityInfo prev;

  prev = ilu_RemoveIdentity(p, info->ii_type, err);
  if (ILU_ERRNOK(*err)) return ilu_FALSE;
  if (prev->ii_owned_by_passport) {
    prev->ii_owned_by_passport = ilu_FALSE;
    ilu_FreeIdentity(prev, err);
    if (ILU_ERRNOK(*err)) return ilu_FALSE;
  };
  return ilu_AddIdentity(p, info, err);
}

extern ilu_cardinal
  ilu_DisplayIdentity (ILU_PASS(struct _ilu_IdentityInfo_s *) info,
		       char *buffer, ilu_cardinal bufferlen,		       
		       ilu_Error *err)
{
  ilu_cardinal val;

  if ((info == NIL) ||
      (info->ii_type == NIL) ||
      (info->ii_type->it_string_form == 0))
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_nil, 0);
  val = (info->ii_type->it_string_form) (info->ii_info, buffer, bufferlen, err);
  if (ILU_ERRNOK(*err)) {
    return 0;
  } else {
    return val;
  }
}

extern ilu_IdentityInfo
  ilu_FindIdentity (ilu_Passport p,
		    ilu_IdentityType infotype)
/* return identity of specified type, if present.  Returns NIL if not present. */
{
  ilu_Vector v = (ilu_Vector) p;
  ilu_cardinal i;

  for (i = 0;  i < v->ve_size;  i++)
    if (((ilu_IdentityInfo)(v->ve_elements[i]))->ii_type == infotype)
      return ((ilu_IdentityInfo)(v->ve_elements[i]));
  return NIL;
}

extern ilu_cardinal
  ilu_PickleIdentity (ilu_IdentityInfo i,
		      ilu_bytes *output_buffer,
		      ilu_cardinal output_buffer_len,
		      ilu_Error *err)
{
  if (i->ii_type->it_pickle == NULLFN)
    { ILU_CLER(*err); return 0; };
  return (*i->ii_type->it_pickle)(i->ii_info, output_buffer, output_buffer_len, err);
}

extern ilu_IdentityInfo
  ilu_UnpickleIdentity (ilu_IdentityType i,
			ilu_bytes pickled_data,
			ilu_cardinal pickled_len,
			ilu_Error *err)
{
  ilu_IdentityInfo info;
  if (i->it_unpickle == NULLFN)
    { ILU_CLER(*err); return NIL; }
  info = ilu_MallocE(sizeof(*info), err);
  if (ILU_ERRNOK(*err)) return NIL;
  info->ii_type = i;
  info->ii_owned_by_passport = ilu_FALSE;
  info->ii_info = (i->it_unpickle)(pickled_data, pickled_len, err);
  if (ILU_ERRNOK(*err)) {
    ilu_free(info);
    return NIL;
  } else
    return info;
}

ilu_boolean
  ilu_FreeIdentity (ILU_PASS(struct _ilu_IdentityInfo_s *) info,
		    ilu_Error *err)
{
  if (info->ii_owned_by_passport)
    return ILU_ERR_CONS0(no_permission, err, ilu_FALSE);
  (*info->ii_type->it_free_data)(info->ii_info, err);
  ilu_free(info);
  return ILU_ERROK(*err);
}

extern ilu_boolean
  ilu_DestroyPassport (ilu_Passport passport,
		       ilu_Error * err)
/* destroys the passport and frees any associated identities */
{
  ilu_IdentityInfo info;
  ilu_Vector vec = (ilu_Vector) passport;
  ilu_cardinal n;

  ILU_CLER(*err);
  for (n = 0;  n < vec->ve_size;  n++)
    {
      info = (ilu_IdentityInfo)(vec->ve_elements[n]);
      if (!info->ii_owned_by_passport)
	continue;
      (*info->ii_type->it_free_data)(info->ii_info, err);
      ilu_free(info);
    }
  if (vec->ve_elements != NIL)
    ilu_free(vec->ve_elements);
  ilu_free(passport);
  return ILU_ERROK(*err);
}
