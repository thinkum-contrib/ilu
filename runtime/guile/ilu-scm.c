/*  -*- Mode: C; -*-
 *
 * This code contributed by Siemens Corporate Research, Inc.
 */
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
 
*/

#include <assert.h>
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <iluhash.h>
#include "ilu-scm-private.h"
#include "ilusrvr-scm.h"
#include "iluobj-scm.h"

/*********************************************************************
 *
 * swiped from class ilu
 *
 *********************************************************************/

static ilu_boolean scheme_registered = ilu_FALSE;
static ilu_cardinal scheme_lang = 2112; /* init to some bogus number */

ilu_cardinal iluguile_scheme_lang_idx()
{
  if(!scheme_registered) {
    scheme_lang = ilu_RegisterLanguage("Guile Scheme");
    scheme_registered = ilu_TRUE;
  }
  return scheme_lang;
}

SCM iluguile__scheme_lang_idx()
{
  return iluguile_value_to_scheme(cardinal, iluguile_scheme_lang_idx());
}

void iluguile_full_prefer_success(ilu_Error* e, const char* atf, unsigned int atl)
{
  const char *n, *f;
  int l;

  if (ILU_ERROK(*e))
    return;
  n = ILU_ERR_NAME(*e);
  f = ilu_ErrorFile(e);
  l = ilu_ErrorLine(e);
  ILU_ERRPRINTF("ILU warning: error %s from %s:%d simplified at %s:%d\n",
		n, f, l, atf, atl);
}


SCM iluguile__full_prefer_success(SCM _e, SCM _atf, SCM _atl)
{
  ilu_Error* e = iluguile_scheme_to_ptr(ilu_Error*, _e);
  ilu_string atf = iluguile_scheme_to_value(tmp_string, _atf);
  ilu_cardinal atl = iluguile_scheme_to_value(cardinal, _atl);
  iluguile_full_prefer_success(e, atf, atl);
  return scheme_True;
}

static ilu_Mutex otmu = ILU_NIL;

SCM iluguile__enter_ot_mu()
{
  ILU_ERRS((internal, no_memory)) lerr;
  if (otmu == ILU_NIL)
    otmu = ilu_GetOTMutex();
  ilu_EnterMutex(otmu, &lerr);
  ILU_MUST_BE_SUCCESS(lerr);
  return scheme_True;
}

SCM iluguile__exit_ot_mu()
{
#ifdef __GNUC__
  ILU_ERRS((internal, no_memory)) lerr = ILU_INIT_NO_ERR;
#else
  ILU_ERRS((internal, no_memory)) lerr = ILU_NO_ERR;
#endif

  if (otmu == ILU_NIL)
    otmu = ilu_GetOTMutex();
  ilu_ExitMutex(otmu, ilu_FALSE, &lerr);
  ILU_MUST_BE_SUCCESS(lerr);
  return scheme_True;
}


SCM iluguile__object_type_defined(SCM _t)
{
  ilu_Class t = iluguile_scheme_to_ptr(ilu_Class, _t);
  ILU_ERRS((internal, no_memory)) lerr;
  ilu_ObjectTypeDefined(t, &lerr);
  ILU_MUST_BE_SUCCESS(lerr);
  return scheme_True;
}

/* caller owns strings */
SCM iluguile__define_exception(SCM _i, SCM _e)
{
  /* this first one seems to be a leak, but not sure about deleting it */
  char* i = iluguile_scheme_to_value(string, _i);
  char* e = iluguile_scheme_to_value(string, _e);
  ILU_ERRS((internal, no_memory)) lerr;
  ilu_Exception ans = ilu_DefineException(i, e, ILU_NIL, &lerr);	/* XXX - fix arg 3 */
  ilu_free(i);
  ilu_free(e);
  ILU_MUST_BE_SUCCESS(lerr);
  return iluguile_value_to_scheme(tmp_string, ans);
}

SCM iluguile__define_object_type(SCM _cl_name,
			    SCM _cl_brand,
			    SCM _cl_unique_id,
			    SCM _cl_singleton,
			    SCM _cl_optional,
			    SCM _cl_collectible,
			    SCM _cl_authentication,
			    SCM _cl_method_count,
			    SCM _cl_scls_count,
			    SCM _cl_scls_ids)
{
  ilu_string cl_name = iluguile_scheme_to_value(string, _cl_name);
  ilu_string cl_brand = iluguile_scheme_to_value(string, _cl_brand);
  ilu_string cl_unique_id = iluguile_scheme_to_value(string, _cl_unique_id);
  ilu_string cl_singleton = iluguile_scheme_to_value(string, _cl_singleton);
  ilu_boolean cl_optional = iluguile_scheme_to_value(boolean, _cl_optional);
  ilu_boolean cl_collectible = iluguile_scheme_to_value(boolean, _cl_collectible);
  ilu_string cl_authentication = iluguile_scheme_to_value(string, _cl_authentication);
  ilu_cardinal cl_method_count = iluguile_scheme_to_value(cardinal, _cl_method_count);
  ilu_cardinal cl_scls_count = iluguile_scheme_to_value(cardinal, _cl_scls_count);
  ilu_string* cl_scls_ids = iluguile_scheme_to_vector(string, _cl_scls_ids);
  ILU_ERRS((internal, no_memory)) lerr;
  ilu_Class ans = ilu_DefineObjectType(cl_name, cl_brand, cl_unique_id,
				       cl_singleton, cl_optional,
				       cl_collectible, cl_authentication,
				       cl_method_count, cl_scls_count,
				       cl_scls_ids, &lerr);
  ILU_MUST_BE_SUCCESS(lerr);

  ilu_free(cl_name);
  ilu_free(cl_brand);
  ilu_free(cl_unique_id);
  ilu_free(cl_singleton);
  ilu_free(cl_authentication);
  iluguile_free_string_vector(cl_scls_ids, cl_scls_count);

  return iluguile_ptr_to_scheme(ilu_Class, ans);
}


/* caller owns me_name and me_exceptionVector */
SCM iluguile__define_method(SCM _c,
		       SCM _i,
		       SCM _me_name,
		       SCM _me_id,
		       SCM _me_cacheable,
		       SCM _me_asynchronous,
		       SCM _me_exceptionCount,
		       SCM _me_exceptionVector)
{
  ilu_Class c = iluguile_scheme_to_ptr(ilu_Class, _c);
  ilu_cardinal i = iluguile_scheme_to_value(cardinal, _i);
  ilu_string me_name = iluguile_scheme_to_value(tmp_string, _me_name);
  ilu_cardinal me_id = iluguile_scheme_to_value(cardinal, _me_id);
  ilu_boolean me_cacheable = iluguile_scheme_to_value(boolean, _me_cacheable);
  ilu_boolean me_asynchronous = iluguile_scheme_to_value(boolean, _me_asynchronous);
  ilu_cardinal me_exceptionCount = iluguile_scheme_to_value(cardinal, _me_exceptionCount);
  char** me_exceptionVector = iluguile_scheme_to_vector(string, _me_exceptionVector);
  ILU_ERRS((internal, no_memory)) lerr;
  unsigned int j;

  /**********************************************************
   * need to call ilu_DefineException on every member of
   * _me_exceptionVector to get the correct pointer value for
   * that exception!
   **********************************************************/
  for(j = 0; j < me_exceptionCount; j++) {
    char* tmp = me_exceptionVector[j];
    ilu_Error err;
    me_exceptionVector[j] = ilu_DefineException(0, tmp, ILU_NIL, &err);	/* XXX - fix args when ready */
    ilu_free(tmp);
  }

  (void) ilu_DefineMethod(c, i, me_name, me_id, me_cacheable,
			  me_asynchronous, me_exceptionCount,
			  me_exceptionVector, 0, ILU_NIL, &lerr);	/* XXX - fix args when ready */
  ILU_MUST_BE_SUCCESS(lerr);

  ilu_free(me_exceptionVector);

  return scheme_True;
}


/*
 * call is an output parameter (must allocate call object!)
 * returns a pair (error . call)
 */
SCM iluguile__start_call(SCM _call, SCM _s, SCM _intro_type, SCM _method)
{
  iluguile_SCMCall call = iluguile_scheme_to_ptr(iluguile_SCMCall, _call);
  ilu_Server s = iluguile_scheme_to_ptr(ilu_Server, _s);
  ilu_Class intro_type = iluguile_scheme_to_ptr(ilu_Class, _intro_type);
  ilu_Method method = iluguile_scheme_to_ptr(ilu_Method, _method);
  ilu_Connection newconn;
  ilu_boolean b;

  ilu_StartCall(scm_call_call(call),s,intro_type,method,iluguile_scheme_lang_idx(),
		(ilu_Passport) 0, &newconn, scm_call_err(call));
  if (newconn != NULL)
    iluguile_server_monitor_conn(newconn);

  b = ILU_ERROK(*scm_call_err(call));

  return scheme_cons(iluguile_ptr_to_scheme(iluguile_SCMCall, call),
		     iluguile_value_to_scheme(boolean, b));
}

/*Main Invariant holds, L2 otherwise unconstrained*/
SCM iluguile__begin_sizing_exception(SCM _call, SCM _eIndex)
{
  iluguile_SCMCall call = iluguile_scheme_to_ptr(iluguile_SCMCall, _call);
  ilu_integer Index = iluguile_scheme_to_value(integer, _eIndex);
  ilu_cardinal ans;
  ans = ilu_BeginSizingException(scm_call_call(call),Index,scm_call_err(call));
  iluguile_prefer_success(scm_call_err(call));
  return iluguile_value_to_scheme(cardinal, ans);
}


/*L1_sup < cmu*/
/*before: L2 not >=   {call's conn's iomu},
	  L2     >=   {call's conn's callmu} iff protocol not concurrent;
   after: L2     >=   {call's conn's callmu, iomu} if result is true,
	  L2 disjoint {call's conn's callmu, iomu} if result is false. */
/*If not threaded:
  before: _ILU_C_ReadServiceRequest registered iff protocol concurrent;
  after:  _ILU_C_ReadServiceRequest not registered.*/
SCM iluguile__begin_exception(SCM _call, SCM _eIndex, SCM _argSize)
{
  iluguile_SCMCall call = iluguile_scheme_to_ptr(iluguile_SCMCall, _call);
  ilu_integer eIndex = iluguile_scheme_to_value(integer, _eIndex);
  ilu_cardinal argSize = iluguile_scheme_to_value(cardinal, _argSize);
  ilu_Connection  conn = ilu_ConnectionOfCall(scm_call_call(call));
  ilu_boolean     ans;
  if (ilu_ThreadPerRequest(conn))
    iluguile_server_disable_requests(call, conn);
  ans = ilu_BeginException(scm_call_call(call), eIndex, argSize, scm_call_err(call));
  iluguile_prefer_success(scm_call_err(call));
  return iluguile_value_to_scheme(boolean, ans);
}

SCM iluguile__start_request(SCM _call, SCM _argSize)
{
  iluguile_SCMCall call = iluguile_scheme_to_ptr(iluguile_SCMCall, _call);
  ilu_cardinal argSize = iluguile_scheme_to_value(cardinal, _argSize);
  ilu_boolean ans;
  ans = ilu_StartRequest(scm_call_call(call), argSize, scm_call_err(call));
  iluguile_prefer_success(scm_call_err(call));
  return iluguile_value_to_scheme(boolean, ans);
}

/*Main Invariant holds, L2 otherwise unconstrained*/
SCM iluguile__begin_sizing_reply(SCM _call, SCM _exns_possible)
{
  iluguile_SCMCall call = iluguile_scheme_to_ptr(iluguile_SCMCall, _call);
  ilu_boolean exns_possible = iluguile_scheme_to_value(boolean, _exns_possible);
  ilu_cardinal ans;
  ans = ilu_BeginSizingReply(scm_call_call(call), exns_possible, scm_call_err(call));
  iluguile_prefer_success(scm_call_err(call));
  return iluguile_value_to_scheme(cardinal, ans);
}

/*L1_sup < cmu*/
/*before: L2 not >=   {call's conn's iomu},
	  L2     >=   {call's conn's callmu} iff protocol not concurrent;
   after: L2     >=   {call's conn's callmu, iomu} if result is true,
	  L2 disjoint {call's conn's callmu, iomu} if result is false. */
/*If not threaded:
  before: _ILU_C_ReadServiceRequest registered iff protocol concurrent;
  after:  _ILU_C_ReadServiceRequest not registered.*/
SCM iluguile__begin_reply(SCM _call, SCM _exceptions, SCM _argSize)
{
  iluguile_SCMCall call = iluguile_scheme_to_ptr(iluguile_SCMCall, _call);
  ilu_boolean exceptions = iluguile_scheme_to_value(boolean, _exceptions);
  ilu_cardinal argSize = iluguile_scheme_to_value(cardinal, _argSize);
  ilu_Connection  conn = ilu_ConnectionOfCall(scm_call_call(call));
  ilu_boolean ans;
  if (ilu_ThreadPerRequest(conn))
    iluguile_server_disable_requests(call, conn);
  ans = ilu_BeginReply(scm_call_call(call), exceptions, argSize, scm_call_err(call));
  iluguile_prefer_success(scm_call_err(call));
  return iluguile_value_to_scheme(boolean, ans);
}


SCM iluguile__end_sequence(SCM _call)
{
  iluguile_SCMCall call = iluguile_scheme_to_ptr(iluguile_SCMCall, _call);
  ilu_EndSequence(scm_call_call(call), scm_call_err(call));
  iluguile_prefer_success(scm_call_err(call));
  return scheme_True;
}


SCM iluguile__end_union(SCM _call)
{
  iluguile_SCMCall call = iluguile_scheme_to_ptr(iluguile_SCMCall, _call);
  ilu_EndUnion(scm_call_call(call), scm_call_err(call));
  iluguile_prefer_success(scm_call_err(call));
  return scheme_True;
}


SCM iluguile__end_record(SCM _call)
{
  iluguile_SCMCall call = iluguile_scheme_to_ptr(iluguile_SCMCall, _call);
  ilu_EndRecord(scm_call_call(call), scm_call_err(call));
  iluguile_prefer_success(scm_call_err(call));
  return scheme_True;
}


SCM iluguile__end_array(SCM _call)
{
  iluguile_SCMCall call = iluguile_scheme_to_ptr(iluguile_SCMCall, _call);
  ilu_EndArray(scm_call_call(call), scm_call_err(call));
  iluguile_prefer_success(scm_call_err(call));
  return scheme_True;
}

/* exceptions are strings... */
SCM iluguile__exception_of_method(SCM _method, SCM _index)
{
  ilu_Method method = iluguile_scheme_to_ptr(ilu_Method, _method);
  ilu_cardinal index = iluguile_scheme_to_value(cardinal, _index);
  ilu_Exception ex = ilu_ExceptionOfMethod(method, index);
  return iluguile_value_to_scheme(tmp_string, ex);
}

SCM iluguile__exception_count_of_method(SCM _m)
{
  ilu_Method m = iluguile_scheme_to_ptr(ilu_Method, _m);
  ilu_cardinal c = m->me_exceptionCount;
  return iluguile_value_to_scheme(cardinal, c);
}

SCM iluguile__get_object_class(SCM _obj)
{
  ilu_Object obj = iluguile_scheme_to_ptr(ilu_Object, _obj);
  return iluguile_ptr_to_scheme(ilu_Class, ilu_ClassOfObject(obj));
}

SCM iluguile__get_object_server(SCM _obj)
{
  ilu_Object obj = iluguile_scheme_to_ptr(ilu_Object, _obj);
  return iluguile_ptr_to_scheme(ilu_Server, ilu_ServerOfObject(obj));
}


/*Main Invariant holds*/
/*before: L2 >= {call's conn's callmu, iomu};
  after: success <=> L2 >= {call's conn's callmu, iomu}*/
SCM iluguile__reply_read(SCM _call)
{
  iluguile_SCMCall call = iluguile_scheme_to_ptr(iluguile_SCMCall, _call);
  ilu_boolean ok;
  ilu_ReplyRead(scm_call_call(call), scm_call_err(call));
  iluguile_prefer_success(scm_call_err(call));
  ok = ILU_ERROK(*scm_call_err(call));
  return iluguile_value_to_scheme(boolean, ok);
}

/* free call object */
SCM iluguile__finish_call(SCM _call)
{
  iluguile_SCMCall call = iluguile_scheme_to_ptr(iluguile_SCMCall, _call);
  ilu_boolean ok;
  ilu_FinishCall(scm_call_call(call), scm_call_err(call));
  iluguile_prefer_success(scm_call_err(call));
  if (scm_call_call(call)->ca_incoming && !scm_call_call(call)->ca_reqs_enabled)
    iluguile_server_enable_requests(call, ilu_ConnectionOfCall(scm_call_call(call)));
  ok = ILU_ERROK(*scm_call_err(call));
  return iluguile_value_to_scheme(boolean, ok);
}


/*Main Invariant holds*/
/*L2    >=    {call's conn's callmu, iomu} before,
 *L2 disjoint {call's conn's callmu, iomu} after*/
/*If not threaded:
  before: _ILU_C_ReadServiceRequest not registered;
  after:  _ILU_C_ReadServiceRequest registered.*/
SCM iluguile__finish_exception(SCM _call)
{
  iluguile_SCMCall call = iluguile_scheme_to_ptr(iluguile_SCMCall, _call);
  ilu_Connection conn = ilu_ConnectionOfCall(scm_call_call(call));
  ilu_boolean ans = ilu_FinishException(scm_call_call(call), scm_call_err(call));
  iluguile_prefer_success(scm_call_err(call));
  /*  iluguile_server_enable_requests(call, conn); */
  return iluguile_value_to_scheme(boolean, ans);
}


SCM iluguile__finish_request(SCM _call)
{
  iluguile_SCMCall call = iluguile_scheme_to_ptr(iluguile_SCMCall, _call);
  ilu_boolean ans = ilu_FinishRequest(scm_call_call(call), scm_call_err(call));
  iluguile_prefer_success(scm_call_err(call));
  return iluguile_value_to_scheme(boolean, ans);
}

/*Main Invariant holds*/
/*L2    >=    {call's conn's callmu, iomu} before,
 *L2 disjoint {call's conn's callmu, iomu} after*/
/*If not threaded:
  before: _ILU_C_ReadServiceRequest not registered;
  after:  _ILU_C_ReadServiceRequest registered.*/
SCM iluguile__finish_reply(SCM _call)
{
  iluguile_SCMCall call = iluguile_scheme_to_ptr(iluguile_SCMCall, _call);
  ilu_Connection conn = ilu_ConnectionOfCall(scm_call_call(call));
  ilu_boolean ans = ilu_FinishReply(scm_call_call(call), scm_call_err(call));
  iluguile_prefer_success(scm_call_err(call));
  /* iluguile_server_enable_requests(call, conn); */
  return iluguile_value_to_scheme(boolean, ans);
}


SCM iluguile__no_reply(SCM _call)
{
  iluguile_SCMCall call = iluguile_scheme_to_ptr(iluguile_SCMCall, _call);
  ilu_Connection  conn = ilu_ConnectionOfCall(scm_call_call(call));
  ilu_boolean ans = ilu_NoReply(scm_call_call(call), scm_call_err(call));
  iluguile_prefer_success(scm_call_err(call));
  /* if (!ilu_ThreadPerRequest(conn))
       iluguile_server_enable_requests(call, conn); */
  return scheme_True; /*iluguile_value_to_scheme(boolean, ans);*/
}


/*Main invariant holds
 *before: L2     >=  {call's conn's callmu, iomu}
 *after:  L2 not >= {call's conn's         iomu},
 *after:  L2     >= {call's conn's callmu} iff protocol not concurrent*/
/*If not threaded:
  before: (iluServer)ReadServiceRequest not registered;
  after:  (iluServer)ReadServiceRequest registered iff protocol concurrent.*/
SCM iluguile__finish_parameters(SCM _call, SCM _obj)
{
  iluguile_SCMCall call = iluguile_scheme_to_ptr(iluguile_SCMCall, _call);
  ilu_Connection  conn = ilu_ConnectionOfCall(scm_call_call(call));
  ilu_boolean ans = ilu_RequestRead(scm_call_call(call), scm_call_err(call));
  iluguile_prefer_success(scm_call_err(call));
  if (ilu_ThreadPerRequest(conn))
    iluguile_server_enable_requests(call, conn);
  if (_obj == scheme_False) {
    if (ilu_BeginException(scm_call_call(call), 0,
			   ilu_ProtocolException_GarbageArguments,
			   scm_call_err(call)))
      ilu_FinishException(scm_call_call(call), scm_call_err(call));
    iluguile_prefer_success(scm_call_err(call));
    return scheme_False;
  }
  return iluguile_value_to_scheme(boolean, ans);
}


SCM iluguile__input_byte(SCM _call)
{
  iluguile_SCMCall call = iluguile_scheme_to_ptr(iluguile_SCMCall, _call);
  ilu_byte b2;
  ilu_InputByte(scm_call_call(call), &b2, scm_call_err(call));
  iluguile_prefer_success(scm_call_err(call));
  return iluguile_value_to_scheme(byte, b2);
}

SCM iluguile__input_boolean(SCM _call)
{
  iluguile_SCMCall call = iluguile_scheme_to_ptr(iluguile_SCMCall, _call);
  ilu_boolean b2;
  ilu_InputBoolean(scm_call_call(call), &b2, scm_call_err(call));
  iluguile_prefer_success(scm_call_err(call));
  return iluguile_value_to_scheme(boolean, b2);
}

SCM iluguile__input_optional(SCM _call)
{
  iluguile_SCMCall call = iluguile_scheme_to_ptr(iluguile_SCMCall, _call);
  ilu_boolean b2;
  ilu_InputOptional(scm_call_call(call), &b2, ILU_NIL, scm_call_err(call));
  iluguile_prefer_success(scm_call_err(call));
  return iluguile_value_to_scheme(boolean, b2);
}


SCM iluguile__input_cardinal(SCM _call)
{
  iluguile_SCMCall call = iluguile_scheme_to_ptr(iluguile_SCMCall, _call);
  ilu_cardinal b2;
  ilu_InputCardinal(scm_call_call(call), &b2, scm_call_err(call));
  iluguile_prefer_success(scm_call_err(call));
  return iluguile_value_to_scheme(cardinal, b2);
}


SCM iluguile__input_character(SCM _call)
{
  iluguile_SCMCall call = iluguile_scheme_to_ptr(iluguile_SCMCall, _call);
  ilu_character b2;
  ilu_InputCharacter(scm_call_call(call), &b2, scm_call_err(call));
  iluguile_prefer_success(scm_call_err(call));
  return iluguile_value_to_scheme(character, b2);
}


SCM iluguile__input_enum(SCM _call)
{ 
  iluguile_SCMCall call = iluguile_scheme_to_ptr(iluguile_SCMCall, _call);
  ilu_shortcardinal b2;
  ilu_InputEnum(scm_call_call(call), &b2, ILU_NIL, scm_call_err(call));
  iluguile_prefer_success(scm_call_err(call));
  return iluguile_value_to_scheme(shortcardinal, b2);
}


SCM iluguile__input_integer(SCM _call)
{
  iluguile_SCMCall call = iluguile_scheme_to_ptr(iluguile_SCMCall, _call);
  ilu_integer b2;
  ilu_InputInteger(scm_call_call(call), &b2, scm_call_err(call));
  iluguile_prefer_success(scm_call_err(call));
  return iluguile_value_to_scheme(integer, b2);
}


SCM iluguile__input_real(SCM _call)
{
  iluguile_SCMCall call = iluguile_scheme_to_ptr(iluguile_SCMCall, _call);
  double b;
  ilu_InputReal(scm_call_call(call), &b, scm_call_err(call));
  iluguile_prefer_success(scm_call_err(call));
  return iluguile_value_to_scheme(real, (ilu_real)b);
}

SCM iluguile__input_short_cardinal(SCM _call)
{
  iluguile_SCMCall call = iluguile_scheme_to_ptr(iluguile_SCMCall, _call);
  ilu_shortcardinal b2;
  ilu_InputShortCardinal(scm_call_call(call), &b2, scm_call_err(call));
  iluguile_prefer_success(scm_call_err(call));
  return iluguile_value_to_scheme(shortcardinal, b2);
}

SCM iluguile__input_short_integer(SCM _call)
{
  iluguile_SCMCall call = iluguile_scheme_to_ptr(iluguile_SCMCall, _call);
  ilu_shortinteger b2;
  ilu_InputShortInteger(scm_call_call(call), &b2, scm_call_err(call));
  iluguile_prefer_success(scm_call_err(call));
  return iluguile_value_to_scheme(shortinteger, b2);
}

SCM iluguile__input_short_real(SCM _call)
{
  iluguile_SCMCall call = iluguile_scheme_to_ptr(iluguile_SCMCall, _call);
  float b;
  ilu_InputShortReal(scm_call_call(call), &b, scm_call_err(call));
  iluguile_prefer_success(scm_call_err(call));
  return iluguile_value_to_scheme(shortreal, (ilu_shortreal)b);
}

SCM iluguile__input_long_cardinal(SCM _call)
{
  iluguile_SCMCall call = iluguile_scheme_to_ptr(iluguile_SCMCall, _call);
  ilu_longcardinal b2;
  ilu_InputLongCardinal(scm_call_call(call), &b2, scm_call_err(call));
  iluguile_prefer_success(scm_call_err(call));
  return iluguile_value_to_scheme(longcardinal, b2);
}


SCM iluguile__input_long_integer(SCM _call)
{
  iluguile_SCMCall call = iluguile_scheme_to_ptr(iluguile_SCMCall, _call);
  ilu_longinteger b2;
  ilu_InputLongInteger(scm_call_call(call), &b2, scm_call_err(call));
  iluguile_prefer_success(scm_call_err(call));
  return iluguile_value_to_scheme(longinteger, b2);
}


SCM iluguile__input_long_real(SCM _call)
{
  iluguile_SCMCall call = iluguile_scheme_to_ptr(iluguile_SCMCall, _call);
  ilu_longreal b2;
  ilu_InputLongReal(scm_call_call(call), &b2, scm_call_err(call));
  iluguile_prefer_success(scm_call_err(call));
  return iluguile_value_to_scheme(longreal, b2);
}

SCM iluguile__input_opaque(SCM _call, SCM _len)
{
  iluguile_SCMCall call = iluguile_scheme_to_ptr(iluguile_SCMCall, _call);
  ilu_cardinal len = iluguile_scheme_to_value(cardinal, _len);
  ilu_opaque buf = 0;
  SCM ret;
  ilu_InputOpaque(scm_call_call(call), &buf, len, scm_call_err(call));
  iluguile_prefer_success(scm_call_err(call));
  ret = iluguile_vector_to_scheme(byte, buf, len);
  ilu_free(buf);
  return ret;
}

SCM iluguile__input_bytes(SCM _call, SCM _limit)
{
  iluguile_SCMCall call = iluguile_scheme_to_ptr(iluguile_SCMCall, _call);
  ilu_cardinal limit = iluguile_scheme_to_value(cardinal, _limit);
  ilu_cardinal len = 0;
  ilu_byte* buf = 0;
  SCM ret;
  ilu_InputBytes(scm_call_call(call), &buf, &len, limit, scm_call_err(call));
  iluguile_prefer_success(scm_call_err(call));
  ret = iluguile_vector_to_scheme(byte, buf, len);
  ilu_free(buf);
  return ret;
}


SCM iluguile__input_string(SCM _call, SCM _limit)
{
  iluguile_SCMCall call = iluguile_scheme_to_ptr(iluguile_SCMCall, _call);
  ilu_cardinal limit = iluguile_scheme_to_value(cardinal, _limit);
  ilu_cardinal len = 0;
  ilu_string buf = 0;
  ilu_InputString(scm_call_call(call), &buf, &len, limit, scm_call_err(call));
  iluguile_prefer_success(scm_call_err(call));
  return iluguile_value_to_scheme(string, buf);
}


SCM iluguile__input_string_vec(SCM _call, SCM _len)
{
  iluguile_SCMCall call = iluguile_scheme_to_ptr(iluguile_SCMCall, _call);
  ilu_cardinal len = iluguile_scheme_to_value(cardinal, _len);
  ilu_string buf = 0;
  SCM tmp;
  ilu_InputStringVec(scm_call_call(call), &buf, len, scm_call_err(call));
  iluguile_prefer_success(scm_call_err(call));
  tmp = iluguile_vector_to_scheme(shortcharacter, buf, len);
  ilu_free(buf);
  return tmp;
}



SCM iluguile__input_w_string(SCM _call, SCM _limit)
{
  iluguile_SCMCall call = iluguile_scheme_to_ptr(iluguile_SCMCall, _call);
  ilu_cardinal limit = iluguile_scheme_to_value(cardinal, _limit);
  ilu_cardinal len = 0;
  ilu_wstring buf = 0;
  SCM ret;
  ilu_InputWString(scm_call_call(call), &buf, &len, limit, scm_call_err(call));
  iluguile_prefer_success(scm_call_err(call));
  ret = iluguile_value_to_scheme(wstring, buf);
  ilu_free(buf);
  return ret;
}


SCM iluguile__input_w_string_vec(SCM _call, SCM _len)
{
  iluguile_SCMCall call = iluguile_scheme_to_ptr(iluguile_SCMCall, _call);
  ilu_cardinal len = iluguile_scheme_to_value(cardinal, _len);
  ilu_wstring buf = 0;
  SCM tmp;
  ilu_InputWStringVec(scm_call_call(call), &buf, len, scm_call_err(call));
  iluguile_prefer_success(scm_call_err(call));
  tmp = iluguile_vector_to_scheme(character, buf, len);
  ilu_free(buf);
  return tmp;
}

ilu_Object iluguile_input_object_id(iluguile_SCMCall call,
			       ilu_boolean discriminator_p,
			       ilu_Class putative_class)
{
  ilu_Object obj = NULL;
  ilu_InputObjectID(scm_call_call(call), &obj, discriminator_p,
		    putative_class, scm_call_err(call));
  iluguile_prefer_success(scm_call_err(call));
  /* now obj != NULL => Inside(obj->server, putative_class) */
  return obj;
}



SCM iluguile__input_object_id(SCM _call, SCM _discriminator_p, SCM _putative_class)
{
  iluguile_SCMCall call = iluguile_scheme_to_ptr(iluguile_SCMCall, _call);
  ilu_boolean discriminator_p = iluguile_scheme_to_value(boolean, _discriminator_p);
  ilu_Class putative_class = iluguile_scheme_to_ptr(ilu_Class, _putative_class);
  ilu_Object obj = iluguile_input_object_id(call, discriminator_p, putative_class);
  return iluguile_ptr_to_scheme(ilu_Object, obj);
}

SCM iluguile__input_sequence(SCM _call, SCM _limit)
{
  iluguile_SCMCall call = iluguile_scheme_to_ptr(iluguile_SCMCall, _call);
  ilu_cardinal limit = iluguile_scheme_to_value(cardinal, _limit);
  ilu_cardinal c2 = 0;
  ilu_InputSequence(scm_call_call(call), &c2, limit, ILU_NIL, scm_call_err(call));
  iluguile_prefer_success(scm_call_err(call));
  return iluguile_value_to_scheme(cardinal, c2);
}


SCM iluguile__input_union(SCM _call, SCM _limit)
{
  iluguile_SCMCall call = iluguile_scheme_to_ptr(iluguile_SCMCall, _call);
  ilu_cardinal limit = iluguile_scheme_to_value(cardinal, _limit);
  ilu_cardinal d2 = 0;
  ilu_InputUnion(scm_call_call(call), &d2, limit, ILU_NIL, scm_call_err(call));
  iluguile_prefer_success(scm_call_err(call));
  return iluguile_value_to_scheme(cardinal, d2);
}


SCM iluguile__input_record(SCM _call)
{
  iluguile_SCMCall call = iluguile_scheme_to_ptr(iluguile_SCMCall, _call);
  ilu_InputRecord(scm_call_call(call), ILU_NIL, scm_call_err(call));
  iluguile_prefer_success(scm_call_err(call));
  return scheme_True;
}

SCM iluguile__input_array(SCM _call)
{
  iluguile_SCMCall call = iluguile_scheme_to_ptr(iluguile_SCMCall, _call);
  ilu_InputArray(scm_call_call(call), ILU_NIL, scm_call_err(call));
  iluguile_prefer_success(scm_call_err(call));
  return scheme_True;
}


/*after: Inside(ks, c) */
SCM iluguile__enter_server(SCM _ks, SCM _c)
{
  ilu_Server ks = iluguile_scheme_to_ptr(ilu_Server, _ks);
  ilu_Class c = iluguile_scheme_to_ptr(ilu_Class, _c);
  ilu_EnterServer(ks, c);
  return scheme_True;
}

/*before: Inside(obj->server, obj->class) */
SCM iluguile__exit_server(SCM _ks, SCM _c)
{
  ilu_Server ks = iluguile_scheme_to_ptr(ilu_Server, _ks);
  ilu_Class c = iluguile_scheme_to_ptr(ilu_Class, _c);
  ilu_ExitServer(ks, c);
  return scheme_True;
}


SCM iluguile__sbh_to_object(SCM _sbh, SCM _c)
{
  ILU_ERRS((bad_locks, broken_locks, inv_objref, no_memory, internal)) lerr;
  ilu_string sbh = iluguile_scheme_to_value(string, _sbh);  /* tmp_string */
  ilu_Class c = iluguile_scheme_to_ptr(ilu_Class, _c);
  ilu_Object kobj;
  SCM o;

  kobj = ilu_ObjectOfSBH(sbh, c, &lerr);
  iluguile_prefer_success(&lerr);
  if (kobj == NULL)
    return scheme_False;

  /* now Inside(obj->server, c) */
  if ((o = iluguile_get_language_specific_object(kobj)) == scheme_False)
    o = iluguile_object_create_from_registry(ilu_ClassOfObject(kobj), kobj);

  ilu_ExitServer(ilu_ServerOfObject(kobj), c);

  return o;
}


/* before: Inside(obj->server, obj->class) */
SCM iluguile__sbh_of_object(SCM _obj)
{
  ilu_Object obj = iluguile_scheme_to_ptr(ilu_Object, _obj);
  ilu_Server s = ilu_ServerOfObject(obj);
  ilu_Class c = ilu_ClassOfObject(obj);
  ilu_string result = ilu_SBHOfObject(obj);
  ilu_ExitServer(s, c);
  return iluguile_value_to_scheme(tmp_string, result);
}


SCM iluguile__output_byte(SCM _call, SCM _byte)
{
  iluguile_SCMCall call = iluguile_scheme_to_ptr(iluguile_SCMCall, _call);
  ilu_byte byte = iluguile_scheme_to_value(byte, _byte);
  ilu_OutputByte(scm_call_call(call), byte, scm_call_err(call));
  iluguile_prefer_success(scm_call_err(call));
  return scheme_True;
}


SCM iluguile__output_boolean(SCM _call, SCM _b)
{
  iluguile_SCMCall call = iluguile_scheme_to_ptr(iluguile_SCMCall, _call);
  ilu_boolean b = iluguile_scheme_to_value(boolean, _b);
  ilu_OutputBoolean(scm_call_call(call), b, scm_call_err(call));
  iluguile_prefer_success(scm_call_err(call));
  return scheme_True;
}

SCM iluguile__output_optional(SCM _call, SCM _b)
{
  iluguile_SCMCall call = iluguile_scheme_to_ptr(iluguile_SCMCall, _call);
  ilu_boolean b = iluguile_scheme_to_value(boolean, _b);
  ilu_OutputOptional(scm_call_call(call), b, ILU_NIL, scm_call_err(call));
  iluguile_prefer_success(scm_call_err(call));
  return scheme_True;
}

SCM iluguile__output_cardinal(SCM _call, SCM _v)
{
  iluguile_SCMCall call = iluguile_scheme_to_ptr(iluguile_SCMCall, _call);
  ilu_cardinal v = iluguile_scheme_to_value(cardinal, _v);
  ilu_OutputCardinal(scm_call_call(call), v, scm_call_err(call));
  iluguile_prefer_success(scm_call_err(call));
  return scheme_True;
}

SCM iluguile__output_character(SCM _call, SCM _v)
{
  iluguile_SCMCall call = iluguile_scheme_to_ptr(iluguile_SCMCall, _call);
  ilu_character v = iluguile_scheme_to_value(character, _v);
  ilu_OutputCharacter(scm_call_call(call), v, scm_call_err(call));
  iluguile_prefer_success(scm_call_err(call));
  return scheme_True;
}

SCM iluguile__output_enum(SCM _call, SCM _v)
{
  iluguile_SCMCall call = iluguile_scheme_to_ptr(iluguile_SCMCall, _call);
  ilu_shortcardinal v = iluguile_scheme_to_value(shortcardinal, _v);
  ilu_OutputEnum(scm_call_call(call), v, ILU_NIL, scm_call_err(call));
  iluguile_prefer_success(scm_call_err(call));
  return scheme_True;
}


SCM iluguile__output_integer(SCM _call, SCM _v)
{
  iluguile_SCMCall call = iluguile_scheme_to_ptr(iluguile_SCMCall, _call);
  ilu_integer v = iluguile_scheme_to_value(integer, _v);
  ilu_OutputInteger(scm_call_call(call), v, scm_call_err(call));
  iluguile_prefer_success(scm_call_err(call));
  return scheme_True;
}

SCM iluguile__output_real(SCM _call, SCM _v)
{
  iluguile_SCMCall call = iluguile_scheme_to_ptr(iluguile_SCMCall, _call);
  ilu_real v = iluguile_scheme_to_value(real, _v);
  ilu_OutputReal(scm_call_call(call), v, scm_call_err(call));
  iluguile_prefer_success(scm_call_err(call));
  return scheme_True;
}


SCM iluguile__output_short_integer(SCM _call, SCM _v)
{
  iluguile_SCMCall call = iluguile_scheme_to_ptr(iluguile_SCMCall, _call);
  ilu_shortinteger v = iluguile_scheme_to_value(shortinteger, _v);
  ilu_OutputShortInteger(scm_call_call(call), v, scm_call_err(call));
  iluguile_prefer_success(scm_call_err(call));
  return scheme_True;
}


SCM iluguile__output_short_real(SCM _call, SCM _v)
{
  iluguile_SCMCall call = iluguile_scheme_to_ptr(iluguile_SCMCall, _call);
  ilu_shortreal v = iluguile_scheme_to_value(shortreal, _v);
  ilu_OutputShortReal(scm_call_call(call), v, scm_call_err(call));
  iluguile_prefer_success(scm_call_err(call));
  return scheme_True;
}


SCM iluguile__output_long_cardinal(SCM _call, SCM _v)
{
  iluguile_SCMCall call = iluguile_scheme_to_ptr(iluguile_SCMCall, _call);
  ilu_longcardinal v = iluguile_scheme_to_value(longcardinal, _v);
  ilu_OutputLongCardinal(scm_call_call(call), v, scm_call_err(call));
  iluguile_prefer_success(scm_call_err(call));
  return scheme_True;
}


SCM iluguile__output_long_integer(SCM _call, SCM _v)
{
  iluguile_SCMCall call = iluguile_scheme_to_ptr(iluguile_SCMCall, _call);
  ilu_longinteger v = iluguile_scheme_to_value(longinteger, _v);
  ilu_OutputLongInteger(scm_call_call(call), v, scm_call_err(call));
  iluguile_prefer_success(scm_call_err(call));
  return scheme_True;
}


SCM iluguile__output_long_real(SCM _call, SCM _v)
{
  iluguile_SCMCall call = iluguile_scheme_to_ptr(iluguile_SCMCall, _call);
  ilu_longreal v = iluguile_scheme_to_value(longreal, _v);
  ilu_OutputLongReal(scm_call_call(call), v, scm_call_err(call));
  iluguile_prefer_success(scm_call_err(call));
  return scheme_True;
}

SCM iluguile__output_bytes(SCM _call, SCM _buff, SCM _len, SCM _limit)
{
  iluguile_SCMCall call = iluguile_scheme_to_ptr(iluguile_SCMCall, _call);
  ilu_byte* bytes = iluguile_scheme_to_vector(byte, _buff);
  ilu_cardinal len = iluguile_scheme_to_value(cardinal, _len);
  ilu_cardinal limit = iluguile_scheme_to_value(cardinal, _limit);
  ilu_OutputBytes(scm_call_call(call), bytes, len, limit, scm_call_err(call));
  iluguile_prefer_success(scm_call_err(call));
  ilu_free(bytes);
  return scheme_True;
}

SCM iluguile__output_opaque(SCM _call, SCM _buff, SCM _len)
{
  iluguile_SCMCall call = iluguile_scheme_to_ptr(iluguile_SCMCall, _call);
  ilu_byte* bytes = iluguile_scheme_to_vector(byte, _buff);
  ilu_cardinal len = iluguile_scheme_to_value(cardinal, _len);
  ilu_OutputOpaque(scm_call_call(call), bytes, len, scm_call_err(call));
  iluguile_prefer_success(scm_call_err(call));
  ilu_free(bytes);
  return scheme_True;
}


SCM iluguile__output_string(SCM _call, SCM _buff, SCM _len, SCM _limit)
{
  iluguile_SCMCall call = iluguile_scheme_to_ptr(iluguile_SCMCall, _call);
  ilu_string buff = iluguile_scheme_to_value(tmp_string, _buff);
  ilu_cardinal len = iluguile_scheme_to_value(cardinal, _len);
  ilu_cardinal limit = iluguile_scheme_to_value(cardinal, _limit);
  ilu_OutputString(scm_call_call(call), buff, len, limit, scm_call_err(call));
  iluguile_prefer_success(scm_call_err(call));
  return scheme_True;
}

SCM iluguile__output_string_vec(SCM _call, SCM _buff, SCM _len)
{
  iluguile_SCMCall call = iluguile_scheme_to_ptr(iluguile_SCMCall, _call);
  ilu_shortcharacter* buff = iluguile_scheme_to_vector(shortcharacter, _buff);
  ilu_cardinal len = iluguile_scheme_to_value(cardinal, _len);
  ilu_OutputStringVec(scm_call_call(call), buff, len, scm_call_err(call));
  iluguile_prefer_success(scm_call_err(call));
  ilu_free(buff);
  return scheme_True;
}


SCM iluguile__output_w_string(SCM _call, SCM _buf, SCM _len, SCM _limit)
{
  iluguile_SCMCall call = iluguile_scheme_to_ptr(iluguile_SCMCall, _call);
  ilu_wstring buf = iluguile_scheme_to_value(wstring, _buf);
  ilu_cardinal len = iluguile_scheme_to_value(cardinal, _len);
  ilu_cardinal limit = iluguile_scheme_to_value(cardinal, _limit);
  ilu_OutputWString(scm_call_call(call), buf, len, limit, scm_call_err(call));
  ilu_free(buf);
  iluguile_prefer_success(scm_call_err(call));
  return scheme_True;
}

SCM iluguile__output_w_string_vec(SCM _call, SCM _buf, SCM _len)
{
  iluguile_SCMCall call = iluguile_scheme_to_ptr(iluguile_SCMCall, _call);
  ilu_character* buf = iluguile_scheme_to_vector(character, _buf);
  ilu_cardinal len = iluguile_scheme_to_value(cardinal, _len);
  ilu_OutputWStringVec(scm_call_call(call), buf, len, scm_call_err(call));
  ilu_free(buf);
  iluguile_prefer_success(scm_call_err(call));
  return scheme_True;
}


/* before: obj != NULL implies Inside(obj->server, obj->class) */
ilu_boolean iluguile_output_object_id(iluguile_SCMCall call, ilu_Object obj,
				      ilu_boolean discriminator_p,
				      ilu_Class putative_class)
{
  ilu_OutputObjectID(scm_call_call(call), obj, discriminator_p,
		     putative_class, scm_call_err(call));
  iluguile_prefer_success(scm_call_err(call));
  return ilu_TRUE;
}

/* before: obj != NULL implies Inside(obj->server, obj->class) */
SCM iluguile__output_object_id(SCM _call, SCM _obj,
			  SCM _discriminator_p,
			  SCM _putative_class)
{
  iluguile_SCMCall call = iluguile_scheme_to_ptr(iluguile_SCMCall, _call);
  ilu_Object obj = iluguile_scheme_to_ptr(ilu_Object, _obj);
  ilu_boolean discrim_p = iluguile_scheme_to_value(boolean, _discriminator_p);
  ilu_Class putative_class = iluguile_scheme_to_ptr(ilu_Class, _putative_class);
  ilu_boolean b = iluguile_output_object_id(call, obj, discrim_p, putative_class);
  return iluguile_value_to_scheme(boolean, b);
}


SCM iluguile__output_sequence(SCM _call, SCM _length, SCM _limit)
{
  iluguile_SCMCall call = iluguile_scheme_to_ptr(iluguile_SCMCall, _call);
  ilu_cardinal length = iluguile_scheme_to_value(cardinal, _length);
  ilu_cardinal limit = iluguile_scheme_to_value(cardinal, _limit);
  ilu_OutputSequence(scm_call_call(call), length, limit, ILU_NIL, scm_call_err(call));
  iluguile_prefer_success(scm_call_err(call));
  return scheme_True;
}


SCM iluguile__output_union(SCM _call, SCM _discriminator, SCM _discriminator_size)
{
  iluguile_SCMCall call = iluguile_scheme_to_ptr(iluguile_SCMCall, _call);
  ilu_cardinal discriminator = iluguile_scheme_to_value(cardinal, _discriminator);
  ilu_cardinal discriminator_size = iluguile_scheme_to_value(cardinal, _discriminator_size);
  ilu_OutputUnion(scm_call_call(call), discriminator, discriminator_size, ILU_NIL, scm_call_err(call));
  iluguile_prefer_success(scm_call_err(call));
  return scheme_True;
}


SCM iluguile__output_array(SCM _call)
{
  /***** this 0 needs to change to the actual number of elements to output */
  iluguile_SCMCall call = iluguile_scheme_to_ptr(iluguile_SCMCall, _call);
  ilu_OutputArray(scm_call_call(call), 0, ILU_NIL, scm_call_err(call));
  iluguile_prefer_success(scm_call_err(call));
  return scheme_True;
}

SCM iluguile__output_record(SCM _call)
{
  iluguile_SCMCall call = iluguile_scheme_to_ptr(iluguile_SCMCall, _call);
  ilu_OutputRecord(scm_call_call(call), ILU_NIL, scm_call_err(call));
  iluguile_prefer_success(scm_call_err(call));
  return scheme_True;
}


SCM iluguile__size_of_byte(SCM _call, SCM _byte)
{
  iluguile_SCMCall call = iluguile_scheme_to_ptr(iluguile_SCMCall, _call);
  ilu_byte byte = iluguile_scheme_to_value(byte, _byte);
  ilu_cardinal ans = ilu_SizeOfByte(scm_call_call(call), byte, scm_call_err(call));
  iluguile_prefer_success(scm_call_err(call));
  return iluguile_value_to_scheme(cardinal, ans);
}

SCM iluguile__size_of_boolean(SCM _call, SCM _b)
{
  iluguile_SCMCall call = iluguile_scheme_to_ptr(iluguile_SCMCall, _call);
  ilu_boolean b = iluguile_scheme_to_value(boolean, _b);
  ilu_cardinal ans = ilu_SizeOfBoolean(scm_call_call(call), b, scm_call_err(call));
  iluguile_prefer_success(scm_call_err(call));
  return iluguile_value_to_scheme(cardinal, ans);
}

SCM iluguile__size_of_optional(SCM _call, SCM _opt)
{
  iluguile_SCMCall call = iluguile_scheme_to_ptr(iluguile_SCMCall, _call);
  ilu_boolean opt = iluguile_scheme_to_value(boolean, _opt);
  ilu_cardinal ans = ilu_SizeOfOptional(scm_call_call(call), opt, ILU_NIL, scm_call_err(call));
  iluguile_prefer_success(scm_call_err(call));
  return iluguile_value_to_scheme(cardinal, ans);
}


SCM iluguile__size_of_cardinal(SCM _call, SCM _val)
{
  iluguile_SCMCall call = iluguile_scheme_to_ptr(iluguile_SCMCall, _call);
  ilu_cardinal val = iluguile_scheme_to_value(cardinal, _val);
  ilu_cardinal ans = ilu_SizeOfCardinal(scm_call_call(call), val, scm_call_err(call));
  iluguile_prefer_success(scm_call_err(call));
  return iluguile_value_to_scheme(cardinal, ans);
}

SCM iluguile__size_of_character(SCM _call, SCM _val)
{
  iluguile_SCMCall call = iluguile_scheme_to_ptr(iluguile_SCMCall, _call);
  ilu_character val = iluguile_scheme_to_value(character, _val);
  ilu_cardinal ans = ilu_SizeOfCharacter(scm_call_call(call), val, scm_call_err(call));
  iluguile_prefer_success(scm_call_err(call));
  return iluguile_value_to_scheme(cardinal, ans);
}


SCM iluguile__size_of_enum(SCM _call, SCM _val)
{
  iluguile_SCMCall call = iluguile_scheme_to_ptr(iluguile_SCMCall, _call);
  ilu_shortcardinal val = iluguile_scheme_to_value(shortcardinal, _val);
  ilu_cardinal ans = ilu_SizeOfEnum(scm_call_call(call), val, ILU_NIL, scm_call_err(call));
  iluguile_prefer_success(scm_call_err(call));
  return iluguile_value_to_scheme(cardinal, ans);
}


SCM iluguile__size_of_integer(SCM _call, SCM _val)
{
  iluguile_SCMCall call = iluguile_scheme_to_ptr(iluguile_SCMCall, _call);
  ilu_integer val = iluguile_scheme_to_value(integer, _val);
  ilu_cardinal ans = ilu_SizeOfInteger(scm_call_call(call), val, scm_call_err(call));
  iluguile_prefer_success(scm_call_err(call));
  return iluguile_value_to_scheme(cardinal, ans);
}


SCM iluguile__size_of_real(SCM _call, SCM _val)
{
  iluguile_SCMCall call = iluguile_scheme_to_ptr(iluguile_SCMCall, _call);
  ilu_real val = iluguile_scheme_to_value(real, _val);
  ilu_cardinal ans = ilu_SizeOfReal(scm_call_call(call), val, scm_call_err(call));
  iluguile_prefer_success(scm_call_err(call));
  return iluguile_value_to_scheme(cardinal, ans);
}


SCM iluguile__size_of_short_cardinal(SCM _call, SCM _val)
{
  iluguile_SCMCall call = iluguile_scheme_to_ptr(iluguile_SCMCall, _call);
  ilu_shortcardinal val = iluguile_scheme_to_value(shortcardinal, _val);
  ilu_cardinal ans = ilu_SizeOfShortCardinal(scm_call_call(call), val, scm_call_err(call));
  iluguile_prefer_success(scm_call_err(call));
  return iluguile_value_to_scheme(cardinal, ans);
}

SCM iluguile__size_of_short_integer(SCM _call, SCM _val)
{
  iluguile_SCMCall call = iluguile_scheme_to_ptr(iluguile_SCMCall, _call);
  ilu_shortinteger val = iluguile_scheme_to_value(shortinteger, _val);
  ilu_cardinal ans = ilu_SizeOfShortInteger(scm_call_call(call), val, scm_call_err(call));
  iluguile_prefer_success(scm_call_err(call));
  return iluguile_value_to_scheme(cardinal, ans);
}


SCM iluguile__size_of_short_real(SCM _call, SCM _val)
{
  iluguile_SCMCall call = iluguile_scheme_to_ptr(iluguile_SCMCall, _call);
  ilu_shortreal val = iluguile_scheme_to_value(shortreal, _val);
  ilu_cardinal ans = ilu_SizeOfShortReal(scm_call_call(call), val, scm_call_err(call));
  iluguile_prefer_success(scm_call_err(call));
  return iluguile_value_to_scheme(cardinal, ans);
}


SCM iluguile__size_of_long_cardinal(SCM _call, SCM _val)
{
  iluguile_SCMCall call = iluguile_scheme_to_ptr(iluguile_SCMCall, _call);
  ilu_longcardinal val = iluguile_scheme_to_value(longcardinal, _val);
  ilu_cardinal ans = ilu_SizeOfLongCardinal(scm_call_call(call), val, scm_call_err(call));
  iluguile_prefer_success(scm_call_err(call));
  return iluguile_value_to_scheme(cardinal, ans);
}

SCM iluguile__size_of_long_integer(SCM _call, SCM _val)
{
  iluguile_SCMCall call = iluguile_scheme_to_ptr(iluguile_SCMCall, _call);
  ilu_longinteger val = iluguile_scheme_to_value(longinteger, _val);
  ilu_cardinal ans = ilu_SizeOfLongInteger(scm_call_call(call), val, scm_call_err(call));
  iluguile_prefer_success(scm_call_err(call));
  return iluguile_value_to_scheme(cardinal, ans);
}

SCM iluguile__size_of_long_real(SCM _call, SCM _val)
{
  iluguile_SCMCall call = iluguile_scheme_to_ptr(iluguile_SCMCall, _call);
  ilu_longreal val = iluguile_scheme_to_value(longreal, _val);
  ilu_cardinal ans = ilu_SizeOfLongReal(scm_call_call(call), val, scm_call_err(call));
  iluguile_prefer_success(scm_call_err(call));
  return iluguile_value_to_scheme(cardinal, ans);
}


SCM iluguile__size_of_bytes(SCM _call, SCM _buf, SCM _len, SCM _limit)
{
  iluguile_SCMCall call = iluguile_scheme_to_ptr(iluguile_SCMCall, _call);
  ilu_byte* buf = iluguile_scheme_to_vector(byte, _buf);
  ilu_cardinal len = iluguile_scheme_to_value(cardinal, _len);
  ilu_cardinal limit = iluguile_scheme_to_value(cardinal, _limit);
  ilu_cardinal ans = ilu_SizeOfBytes(scm_call_call(call), buf, len, limit, scm_call_err(call));
  iluguile_prefer_success(scm_call_err(call));
  ilu_free(buf);
  return iluguile_value_to_scheme(cardinal, ans);
}

SCM iluguile__size_of_opaque(SCM _call, SCM _buf, SCM _len)
{
  iluguile_SCMCall call = iluguile_scheme_to_ptr(iluguile_SCMCall, _call);
  ilu_byte* buf = iluguile_scheme_to_vector(byte, _buf);
  ilu_cardinal len = iluguile_scheme_to_value(cardinal, _len);
  ilu_cardinal ans = ilu_SizeOfOpaque(scm_call_call(call), buf, len, scm_call_err(call));
  iluguile_prefer_success(scm_call_err(call));
  ilu_free(buf);
  return iluguile_value_to_scheme(cardinal, ans);
}


SCM iluguile__size_of_string(SCM _call, SCM _buf, SCM _len, SCM _limit)
{
  iluguile_SCMCall call = iluguile_scheme_to_ptr(iluguile_SCMCall, _call);
  ilu_string buf = iluguile_scheme_to_value(tmp_string, _buf);
  ilu_cardinal len = iluguile_scheme_to_value(cardinal, _len);
  ilu_cardinal lim = iluguile_scheme_to_value(cardinal, _limit);
  ilu_cardinal ans = ilu_SizeOfString(scm_call_call(call), buf, len, lim, scm_call_err(call));
  iluguile_prefer_success(scm_call_err(call));
  return iluguile_value_to_scheme(cardinal, ans);
}


SCM iluguile__size_of_string_vec(SCM _call, SCM _buf, SCM _len)
{
  iluguile_SCMCall call = iluguile_scheme_to_ptr(iluguile_SCMCall, _call);
  ilu_shortcharacter* buf = iluguile_scheme_to_vector(shortcharacter, _buf);
  ilu_cardinal len = iluguile_scheme_to_value(cardinal, _len);
  ilu_cardinal ans;
  ans = ilu_SizeOfStringVec(scm_call_call(call), buf, len, scm_call_err(call));
  iluguile_prefer_success(scm_call_err(call));
  ilu_free(buf);
  return iluguile_value_to_scheme(cardinal, ans);
}


SCM iluguile__size_of_w_string(SCM _call, SCM _buf, SCM _len, SCM _limit)
{
  iluguile_SCMCall call = iluguile_scheme_to_ptr(iluguile_SCMCall, _call);
  ilu_wstring buf = iluguile_scheme_to_value(wstring, _buf);
  ilu_cardinal len = iluguile_scheme_to_value(cardinal, _len);
  ilu_cardinal limit = iluguile_scheme_to_value(cardinal, _limit);
  ilu_cardinal ans;
  ans = ilu_SizeOfWString(scm_call_call(call), buf, len, limit, scm_call_err(call));
  ilu_free(buf);
  iluguile_prefer_success(scm_call_err(call));
  return iluguile_value_to_scheme(cardinal, ans);
}

SCM iluguile__size_of_w_string_vec(SCM _call, SCM _buf, SCM _len)
{
  iluguile_SCMCall call = iluguile_scheme_to_ptr(iluguile_SCMCall, _call);
  ilu_character* buf = iluguile_scheme_to_vector(character, _buf);
  ilu_cardinal len = iluguile_scheme_to_value(cardinal, _len);
  ilu_cardinal ans;
  ans = ilu_SizeOfWStringVec(scm_call_call(call), buf, len, scm_call_err(call));
  ilu_free(buf);
  iluguile_prefer_success(scm_call_err(call));
  return iluguile_value_to_scheme(cardinal, ans);
}



/* before: obj != NULL implies Inside(obj->server, obj->class) */
ilu_cardinal iluguile_size_of_object_id(iluguile_SCMCall call, ilu_Object obj,
					ilu_boolean discriminator_p,
					ilu_Class putative_class)
{
  ilu_cardinal size = ilu_SizeOfObjectID(scm_call_call(call), obj,
					 discriminator_p,
					 putative_class,
					 scm_call_err(call));
  iluguile_prefer_success(scm_call_err(call));

  if (obj != NULL)
    ilu_ExitServer(ilu_ServerOfObject(obj), ilu_ClassOfObject(obj));

  return size;
}


/* before: obj != NULL implies Inside(obj->server, obj->class) */
SCM iluguile__size_of_object_id(SCM _call, SCM _obj, SCM _discriminator_p,
			   SCM _putative_class)
{
  iluguile_SCMCall call = iluguile_scheme_to_ptr(iluguile_SCMCall, _call);
  ilu_Object obj = iluguile_scheme_to_ptr(ilu_Object, _obj);
  ilu_boolean discriminator_p = iluguile_scheme_to_value(boolean, _discriminator_p);
  ilu_Class putative_class = iluguile_scheme_to_ptr(ilu_Class, _putative_class);
  ilu_cardinal size;
  size = iluguile_size_of_object_id(call, obj, discriminator_p, putative_class);
  return iluguile_value_to_scheme(cardinal, size);
}

SCM iluguile__size_of_sequence(SCM _call, SCM _len, SCM _limit)
{
  iluguile_SCMCall call = iluguile_scheme_to_ptr(iluguile_SCMCall, _call);
  ilu_cardinal len = iluguile_scheme_to_value(cardinal, _len);
  ilu_cardinal limit = iluguile_scheme_to_value(cardinal, _limit);
  ilu_cardinal ans;
  ans = ilu_SizeOfSequence(scm_call_call(call), len, limit, ILU_NIL, scm_call_err(call));
  iluguile_prefer_success(scm_call_err(call));
  return iluguile_value_to_scheme(cardinal, ans);
}

SCM iluguile__size_of_union(SCM _call, SCM _discriminator, SCM _discriminator_size)
{
  iluguile_SCMCall call = iluguile_scheme_to_ptr(iluguile_SCMCall, _call);
  ilu_cardinal discriminator = iluguile_scheme_to_value(cardinal, _discriminator);
  ilu_cardinal discriminator_size = iluguile_scheme_to_value(cardinal, _discriminator_size);
  ilu_cardinal ans;
  ans = ilu_SizeOfUnion(scm_call_call(call), discriminator, discriminator_size, ILU_NIL, scm_call_err(call));
  iluguile_prefer_success(scm_call_err(call));
  return iluguile_value_to_scheme(cardinal, ans);
}


SCM iluguile__size_of_array(SCM _call)
{
  /****** this 0 needs to change to the actual number of elements to output */
  iluguile_SCMCall call = iluguile_scheme_to_ptr(iluguile_SCMCall, _call);
  ilu_cardinal ans = ilu_SizeOfArray(scm_call_call(call), 0, ILU_NIL, scm_call_err(call));
  iluguile_prefer_success(scm_call_err(call));
  return iluguile_value_to_scheme(cardinal, ans);
}


SCM iluguile__size_of_record(SCM _call)
{
  iluguile_SCMCall call = iluguile_scheme_to_ptr(iluguile_SCMCall, _call);
  ilu_cardinal ans = ilu_SizeOfRecord(scm_call_call(call), ILU_NIL, scm_call_err(call));
  iluguile_prefer_success(scm_call_err(call));
  return iluguile_value_to_scheme(cardinal, ans);
}

/* returns success code. */
SCM iluguile__wait_for_reply(SCM _call)
{
  ilu_Connection newconn = ILU_NIL;
  iluguile_SCMCall call = iluguile_scheme_to_ptr(iluguile_SCMCall, _call);
  ilu_ProtocolException ans;
  ilu_cardinal sc;
  ans = ilu_GetReply(scm_call_call(call), &sc, &newconn, scm_call_err(call));
  iluguile_prefer_success(scm_call_err(call));
  if (newconn != NULL)
    iluguile_server_monitor_conn(newconn);
  return scheme_cons(iluguile_value_to_scheme(cardinal, (ilu_cardinal)ans),
		     iluguile_value_to_scheme(cardinal, sc));
}

static ilu_Server DefaultServer = NULL;

SCM iluguile__set_default_server(SCM _s)
{
  ilu_Server s = iluguile_scheme_to_ptr(ilu_Server, _s);
  DefaultServer = s;
  return scheme_True;
}

SCM iluguile__get_default_server()
{
  return iluguile_ptr_to_scheme(ilu_Server, DefaultServer);
}

static char *DefaultProtocol = "sunrpc";
static ilu_string DefaultTransport[] = { "sunrpcrm", "tcp_0_0", 0 };

ilu_Port iluguile_create_port(ilu_Server server,
			      ilu_string protocolType,
			      ilu_TransportInfo transportType)
{
  ilu_TransportInfo t;
  ilu_Error err;
  ilu_string p;
  if ((p = protocolType) == NULL)
    p = DefaultProtocol;
  if ((t = transportType) == NULL || !transportType[0])
    t = DefaultTransport;
  return ilu_CreatePort(server, p, t, 0, &err);
}

/* transport info is a null terminated array of char* */
SCM iluguile__create_port(SCM _server, SCM _protocolType, SCM _transportType)
{
  ilu_Server server = iluguile_scheme_to_ptr(ilu_Server, _server);
  char* protocolType = iluguile_scheme_to_value(tmp_string, _protocolType);
  char** transportType = iluguile_scheme_to_vector0(string, _transportType);
  ilu_Port tmp = iluguile_create_port(server, protocolType, transportType);
  iluguile_free_string_vector(transportType, -1);
  return iluguile_ptr_to_scheme(ilu_Port, tmp);
}


SCM iluguile__set_server_default_port(SCM _s, SCM _p)
{
  ilu_Server s = iluguile_scheme_to_ptr(ilu_Server, _s);
  ilu_Port p = iluguile_scheme_to_ptr(ilu_Port, _p);
  ilu_SetServerDefaultPort(s, p);
  return scheme_True;
}

SCM iluguile__parse_sbh(SCM _sbh)
{
  ilu_string sbh = iluguile_scheme_to_value(tmp_string, _sbh);
  ilu_string plainInstH = 0;
  ilu_string plainServerId = 0;
  ilu_string plainMstid = 0;
  ilu_string encodedContactInfo= 0;
  ilu_boolean passEncodedContactInfo = ilu_FALSE;
  ilu_cardinal clen = 0;
  ilu_boolean ans;
  ilu_Error lerr;

  ans = ilu_ParseSBH(sbh, &plainInstH, &plainServerId, &plainMstid,
		     &encodedContactInfo, &clen, &passEncodedContactInfo, &lerr);
  iluguile_prefer_success(&lerr);

  return scheme_list(iluguile_value_to_scheme(string, plainInstH),
		     iluguile_value_to_scheme(string, plainServerId),
		     iluguile_value_to_scheme(string, plainMstid),
		     iluguile_value_to_scheme(string, encodedContactInfo),
		     iluguile_value_to_scheme(cardinal, clen),
		     scheme_undefined);
}


ilu_RcvReqStat iluguile_receive_request(ilu_Connection conn, iluguile_SCMCall call,
					ilu_boolean *initted, ilu_Class *pclass,
					ilu_Method *method, ilu_cardinal *sn)
{
  ilu_RcvReqStat ans;
  *sn = 0;
  ans = ilu_ReceiveRequest(scm_call_call(call), initted, conn, pclass,
			   method, sn, scm_call_err(call));
  iluguile_prefer_success(scm_call_err(call));
  return ans;
}


/*
 * call object must be created by receive request
 */
SCM iluguile__receive_request(SCM _conn)
{
  ilu_Connection conn = iluguile_scheme_to_ptr(ilu_Connection, _conn);
  /* can probably just stack allocate this guy? */
  iluguile_SCMCall call = ilu_malloc(sizeof(struct iluguile_SCMCall_s));
  ilu_boolean initted;
  ilu_Class pclass;
  ilu_Method method;
  ilu_cardinal serial;
  ilu_RcvReqStat ans;
  ans = iluguile_receive_request(conn, call, &initted, &pclass, &method, &serial);
  iluguile_prefer_success(scm_call_err(call));
  return iluguile_value_to_scheme(integer, (ilu_integer)ans);
}

#if 0
ilu_Connection ilu_handle_new_connection(ilu_Port port)
{
  ilu_boolean closed;
  ilu_Error err;
  return ilu_HandleNewConnection(port, &closed, &err);
}
#endif

SCM iluguile__handle_new_connection(SCM _port)
{
  ilu_Port port = iluguile_scheme_to_ptr(ilu_Port, _port);
  ilu_boolean closed;
  ilu_Error err;
  ilu_Connection c = ilu_HandleNewConnection(port, &closed, &err);
  iluguile_prefer_success(&err);
  return closed ? scheme_False : iluguile_ptr_to_scheme(ilu_Connection, c);
}

#if 0
SCM iluguile__close_connection(SCM _conn)
{
  ilu_Connection conn = iluguile_scheme_to_ptr(ilu_Connection, _conn);
  ilu_CloseConnection(conn);
  return scheme_True;
}
#endif

SCM iluguile__wait_for_port_connection_request(SCM _p)
{
  ilu_Port p = iluguile_scheme_to_ptr(ilu_Port, _p);
  ilu_Error err;
  ilu_boolean b = ilu_WaitForPortConnectionRequest(p, &err);
  iluguile_prefer_success(&err);
  return iluguile_value_to_scheme(boolean, b);
}

SCM iluguile__find_class_from_type_name(SCM type_name)
{
  char* name = iluguile_scheme_to_value(tmp_string, type_name);
  ilu_Class c = ilu_FindClassFromName(name);
  return iluguile_ptr_to_scheme(ilu_Class, c);
}

SCM iluguile__find_class_from_id(SCM _id)
{
  ilu_string id = iluguile_scheme_to_value(tmp_string, _id);
  ilu_Class c = ilu_FindClassFromID(id);
  return iluguile_ptr_to_scheme(ilu_Class, c);
}


/* Inside(s, c) */
SCM iluguile__create_true_kernel_object(SCM _ih, SCM _server, SCM _c, SCM _lspo)
{
  ilu_string ih = iluguile_scheme_to_value(string, _ih);
  ilu_Server server = iluguile_scheme_to_ptr(ilu_Server, _server);
  ilu_Class c = iluguile_scheme_to_ptr(ilu_Class, _c);
  SCM* lo = iluguile_scm_make_array(1);
  ilu_Object o;
  *lo = _lspo;
  o = ilu_FindOrCreateTrueObject(ih, server, c, (void*)lo);
  return iluguile_ptr_to_scheme(ilu_Object, o);
}

SCM iluguile_get_language_specific_object(ilu_Object obj)
{
  SCM* so = (SCM*)ilu_GetLanguageSpecificObject(obj, iluguile_scheme_lang_idx());
  return so ? *so : scheme_False;
}

/* Inside(obj->server, obj->class) */
SCM iluguile__get_language_specific_object(SCM _obj)
{
  ilu_Object obj = iluguile_scheme_to_ptr(ilu_Object, _obj);
  return iluguile_get_language_specific_object(obj);
}

/* Inside(obj->server, obj->class) */
SCM iluguile__set_language_specific_object(SCM _obj, SCM _lspo)
{
  ilu_Object obj = iluguile_scheme_to_ptr(ilu_Object, _obj);

  SCM* lo = (SCM*)ilu_GetLanguageSpecificObject(obj, iluguile_scheme_lang_idx());
  if(!lo)
    lo = iluguile_scm_make_array(1);
  *lo = _lspo;

  ilu_RegisterLanguageSpecificObject(obj, (void*)lo, iluguile_scheme_lang_idx());
  return scheme_True;
}

SCM iluguile__publish_object(SCM _obj)
{
  ilu_Object obj = iluguile_scheme_to_ptr(ilu_Object, _obj);
  ilu_string proof = ilu_PublishObject(obj);
  return iluguile_value_to_scheme(string, proof);  /* ???????? */
}


SCM iluguile__withdraw_object(SCM _obj, SCM _proof)
{
  ilu_Object obj = iluguile_scheme_to_ptr(ilu_Object, _obj);
  ilu_string proof = iluguile_scheme_to_value(string, _proof);
  ilu_boolean b = ilu_WithdrawObject(obj, proof);
  return iluguile_value_to_scheme(boolean, b);
}

SCM iluguile__server_of_object(SCM _obj)
{
  ilu_Object obj = iluguile_scheme_to_ptr(ilu_Object, _obj);
  ilu_Server server = ilu_ServerOfObject(obj);
  return iluguile_ptr_to_scheme(ilu_Server, server);
}

SCM iluguile_class__name(SCM _c)
{
  ilu_Class c = iluguile_scheme_to_ptr(ilu_Class, _c);
  ilu_string name = c ? c->cl_name : 0;
  return iluguile_value_to_scheme(tmp_string, name);
}

SCM iluguile_class__unique_id(SCM _c)
{
  ilu_Class c = iluguile_scheme_to_ptr(ilu_Class, _c);
  ilu_string uid = c ? c->cl_unique_id : 0;
  return iluguile_value_to_scheme(tmp_string, uid);
}

/**************************************************************************/

SCM iluguile__make_main_loop_id()
{
  return iluguile_ptr_to_scheme(int*, ilu_malloc(sizeof(int)));
}

SCM iluguile__free_main_loop_id(SCM _id)
{
  ilu_free(iluguile_scheme_to_ptr(int*, _id));
  return scheme_True;
}

static int ilu_stop = 0;

SCM iluguile__run_main_loop(SCM _stop)
{
  int* stop = iluguile_scheme_to_ptr(int*, _stop);
  if(!stop)
    stop = &ilu_stop;
  ilu_RunMainLoop(stop);
  return scheme_True;
}

SCM iluguile__exit_main_loop(SCM _stop)
{
  int* stop = iluguile_scheme_to_ptr(int*, _stop);
  if(!stop)
    stop = &ilu_stop;
  ilu_ExitMainLoop(stop);
  return scheme_True;
}

static ilu_HashTable ifd_table = 0;
static ilu_HashTable ofd_table = 0;
#if 1
static ilu_HashTable stub_proc_table = 0;
#endif

void iluguile_init_hash_tables()
{
  ifd_table = ilu_hash_MakeNewTable(128, ilu_hash_HashPointer,
				     ilu_hash_PointerCompare);
  ofd_table = ilu_hash_MakeNewTable(128, ilu_hash_HashPointer,
				     ilu_hash_PointerCompare);
#if 1
  stub_proc_table = ilu_hash_MakeNewTable(2048, ilu_hash_HashPointer,
					   ilu_hash_PointerCompare);
#endif
}

static void ihandler_caller(int fd, void* rock)
{
  SCM* pp = (SCM*)ilu_hash_FindInTable(ifd_table, (ilu_refany)fd);
  iluguile_scm_call0(*pp);
}

static void ohandler_caller(int fd, void* rock)
{
  SCM* pp = (SCM*)ilu_hash_FindInTable(ofd_table, (ilu_refany)fd);
  iluguile_scm_call0(*pp);
}

SCM iluguile__register_input_handler(SCM _fd, SCM _handlerProc)
{
  FILE* f = scheme_stream(_fd);
  int fd = fileno(f);
  SCM* pp = iluguile_scm_make_array(1);
  ilu_boolean b;
  *pp = _handlerProc;
  ilu_hash_AddToTable(ifd_table, (void*)fd, pp);
  b = ilu_RegisterInputSource(fd, ihandler_caller, pp);
  return iluguile_value_to_scheme(boolean, b);
}

SCM iluguile__register_output_handler(SCM _fd, SCM _handlerProc)
{
  FILE* f = scheme_stream(_fd);
  int fd = fileno(f);
  SCM* pp = iluguile_scm_make_array(1);
  ilu_boolean b;
  *pp = _handlerProc;
  ilu_hash_AddToTable(ofd_table, (void*)fd, pp);
  b = ilu_RegisterOutputSource(fd, ohandler_caller, pp);
  return iluguile_value_to_scheme(boolean, b);
}

SCM iluguile__unregister_input_handler(SCM _fd)
{
  FILE* f = scheme_stream(_fd);
  int fd = fileno(f);
  SCM* pp = (SCM*)ilu_hash_FindInTable(ifd_table, (void*)fd);
  iluguile_scm_free_array(pp);
  /* do hash remove? */
  return iluguile_value_to_scheme(boolean, ilu_UnregisterInputSource(fd));
}

SCM iluguile__unregister_output_handler(SCM _fd)
{
  FILE* f = scheme_stream(_fd);
  int fd = fileno(f);
  SCM* pp = (SCM*)ilu_hash_FindInTable(ofd_table, (void*)fd);
  iluguile_scm_free_array(pp);
  /* do hash remove? */
  return iluguile_value_to_scheme(boolean, ilu_UnregisterOutputSource(fd));
}

static void alarm_caller(void* rock)
{
  SCM* proc = (SCM*)rock;
  iluguile_scm_call0(*proc);
  iluguile_scm_free_array(proc);
}

SCM iluguile__create_alarm()
{
  ilu_refany alarm = ilu_CreateAlarm();
  return iluguile_ptr_to_scheme(void*, alarm);
}

SCM iluguile__set_alarm(SCM _alarm, SCM _time, SCM _proc)
{
  ilu_refany alarm = iluguile_scheme_to_ptr(void*, _alarm);
  ilu_real d = iluguile_scheme_to_value(real, _time);
  ilu_FineTime t = ilu_FineTime_FromDouble(d);
  SCM* pp = iluguile_scm_make_array(1);
  *pp = _proc;
  ilu_SetAlarm(alarm, t, alarm_caller, (void*)pp);
  return scheme_True;
}

/* should this delete the alarm too? */
SCM iluguile__clear_alarm(SCM _alarm)
{
  ilu_refany alarm = iluguile_scheme_to_ptr(void*, _alarm);
  ilu_UnsetAlarm(alarm);
  return scheme_True;
}

SCM iluguile__time_now()
{
  ilu_FineTime now = ilu_FineTime_Now();
  double d =  now.ft_s + now.ft_t / (1.0 * ilu_FineTimeRate);
  return iluguile_value_to_scheme(real, d);
}


/**************************************************************************/

static SCM* theML = NULL;

static void CallRun(int *stop)
{
  iluguile_scm_scall2("run", *theML, iluguile_ptr_to_scheme(int*, stop));
}

static void CallExit(int *stop)
{
  iluguile_scm_scall2("exit", *theML, iluguile_ptr_to_scheme(int*, stop));
}

static ilu_boolean CallUnregisterInput(int fd,
				       ilu_IOHandler *proc,
				       ilu_private *rock)
{
  SCM b = iluguile_scm_scall2("unregister-input-handler", *theML, iluguile_value_to_scheme(integer, (ilu_integer) fd));
  return iluguile_value_to_scheme(boolean, b);
}

static ilu_boolean CallUnregisterOutput(int fd,
					ilu_IOHandler *proc,
					ilu_private *rock)
{
  SCM b = iluguile_scm_scall2("unregister-output-handler", *theML, iluguile_value_to_scheme(integer, (ilu_integer) fd));
  return iluguile_value_to_scheme(boolean, b);
}

static ilu_refany CallCreateAlarm (void)
{
  return iluguile_scheme_to_ptr(void*, iluguile_scm_scall1("create-alarm", *theML));
}

static void CallUnsetAlarm(ilu_refany alarm)
{
  iluguile_scm_scall2("clear-alarm", *theML, iluguile_ptr_to_scheme(void*, alarm));
}

static SCM c_input_handler(SCM _proc, SCM _fd, SCM _rock)
{
  void (*proc)(int, void*) = iluguile_scheme_to_fnptr(void (*)(int, void*), _proc);
  int fd = iluguile_scheme_to_value(integer, (ilu_integer) _fd);
  void* rock = iluguile_scheme_to_ptr(void*, _rock);
  proc(fd, rock);
  return scheme_True;
}


static ilu_boolean CallRegisterInput(int fd,
				     ilu_IOHandler proc,
				     ilu_private rock)
{
  SCM _proc = scheme_lambda((SCM(*)())c_input_handler, 3, 0, 0);
  SCM b;
  _proc = iluguile_scm_curry(_proc, iluguile_fnptr_to_scheme(void*, proc));
  _proc = iluguile_scm_curry(_proc, iluguile_value_to_scheme(integer, (ilu_integer) fd));
  _proc = iluguile_scm_curry(_proc, iluguile_ptr_to_scheme(void*, rock));
  b = iluguile_scm_scall2("register-input-handler", *theML, _proc);
  return iluguile_value_to_scheme(boolean, b);
}

static ilu_boolean CallRegisterOutput(int fd,
				      ilu_IOHandler proc,
				      ilu_private rock)
{
  SCM _proc = scheme_lambda((SCM(*)())c_input_handler, 3, 0, 0);
  SCM b;
  _proc = iluguile_scm_curry(_proc, iluguile_fnptr_to_scheme(void*,  proc));
  _proc = iluguile_scm_curry(_proc, iluguile_value_to_scheme(integer, (ilu_integer) fd));
  _proc = iluguile_scm_curry(_proc, iluguile_ptr_to_scheme(void*, rock));
  b = iluguile_scm_scall2("register-output-handler", *theML, _proc);
  return iluguile_value_to_scheme(boolean, b);
}

static SCM c_alarm_handler(SCM _proc, SCM _rock)
{
  void (*proc)(void*) = iluguile_scheme_to_ptr(void (*)(void*), _proc);
  void* rock = iluguile_scheme_to_ptr(void*, _rock);
  proc(rock);
  return scheme_True;
}

static void CallSetAlarm(ilu_refany alarm, ilu_FineTime t,
			 void (*proc)(void * rock), void * rock)
{
  SCM _proc = scheme_lambda((SCM(*)())c_alarm_handler, 2, 0, 0);
  SCM _alarm, _time;
  double d;
  _proc = iluguile_scm_curry(_proc, iluguile_fnptr_to_scheme(void*, proc));
  _proc = iluguile_scm_curry(_proc, iluguile_ptr_to_scheme(void*, rock));
  _alarm = iluguile_ptr_to_scheme(void*, alarm);
  d = t.ft_s + t.ft_t / (1.0 * ilu_FineTimeRate);
  _time = iluguile_value_to_scheme(real, d);
  iluguile_scm_scall4("set-alarm", *theML, _alarm, _time, _proc);
}

static ilu_MainLoop kml = {
  CallRun, CallExit,
  CallRegisterInput, CallUnregisterInput,
  CallRegisterOutput, CallUnregisterOutput,
  CallCreateAlarm, CallSetAlarm, CallUnsetAlarm
};

SCM iluguile__set_main_loop(SCM ml)
{
  if(!theML)
    theML = iluguile_scm_make_array(1);
  *theML = ml;
  ilu_SetMainLoop(&kml);
  return scheme_True;
}

SCM iluguile__err_nok(SCM _err)
{
  ilu_Error* err = iluguile_scheme_to_ptr(ilu_Error*, _err);
  return iluguile_value_to_scheme(boolean, ILU_ERRNOK(*err));
}

SCM iluguile__get_gc_callback_class()
{
  return iluguile_ptr_to_scheme(ilu_Class, ilu_GetGcCallbackClass());
}

SCM iluguile__is_gc_client_set()
{
  return iluguile_value_to_scheme(boolean, ilu_IsGcClientSet());
}

static void call_stub_proc(ilu_Call call)
{
  ilu_Method m = call->ca_method;
  SCM _call = iluguile_ptr_to_scheme(ilu_Call, call);
#if 1
  SCM* pp = (SCM*)ilu_hash_FindInTable(stub_proc_table, (ilu_refany)m);
#else
  SCM* pp = (SCM*)m->me_private;
#endif
  iluguile_scm_call1(*pp, _call);
}

SCM iluguile__set_method_stub_proc(SCM _m, SCM _proc)
{
  ilu_Method m = iluguile_scheme_to_ptr(ilu_Method, _m);
  SCM* pp = iluguile_scm_make_array(1);
  *pp = _proc;
  ilu_SetMethodStubProc(m, call_stub_proc, iluguile_scheme_lang_idx());
#if 1
  ilu_hash_AddToTable(stub_proc_table, m, pp);
#else
  m->me_private = pp;
#endif
  return scheme_True;
}

static ilu_Object call_object_of_ih_proc(ilu_ObjectTable self, ilu_string ih)
{
  SCM _self = iluguile_ptr_to_scheme(ilu_ObjectTable, self);
  SCM _ih = iluguile_value_to_scheme(tmp_string, ih);
  SCM* pp = self->ot_rock;
  return iluguile_scheme_to_ptr(ilu_Object, iluguile_scm_call2(pp[0], _self, _ih));
}

static void call_free_self_fn_proc(ilu_ObjectTable self)
{
  SCM* pp = self->ot_rock;
  iluguile_scm_call1(pp[1], iluguile_ptr_to_scheme(ilu_ObjectTable, self));
  iluguile_scm_free_array(pp);
  ilu_free(self); /* free object table? or does kernel take care of this? */
}

SCM iluguile__create_object_table(SCM object_of_ih_fn, SCM free_self_fn)
{
  ilu_ObjectTable ot = ilu_malloc(sizeof(ilu_ObjectTable_s));
  SCM* pp = iluguile_scm_make_array(2);
  pp[0] = object_of_ih_fn;
  pp[1] = free_self_fn;
  ot->ot_object_of_ih = call_object_of_ih_proc;
  ot->ot_free_self = call_free_self_fn_proc;
  ot->ot_rock = pp;
  return iluguile_ptr_to_scheme(ilu_ObjectTable, ot);
}

/* typeid stuff */

SCM iluguile__get_ilu_corba_object_type_id()
{
  return iluguile_value_to_scheme(tmp_string, (char*)ilu_TypeID_ilu_CORBA_Object);
}


/* call stuff */

SCM iluguile_call__create()
{
  return iluguile_ptr_to_scheme(iluguile_SCMCall, ilu_malloc(sizeof(struct iluguile_SCMCall_s)));
}

SCM iluguile_call__destroy(SCM _call)
{
  ilu_free(iluguile_scheme_to_ptr(iluguile_SCMCall, _call));
  return scheme_True;
}

SCM iluguile_call__caller(SCM _call)
{
  iluguile_SCMCall call = iluguile_scheme_to_ptr(iluguile_SCMCall, _call);
  ilu_Passport p = scm_call_call(call)->ca_caller;
  return iluguile_ptr_to_scheme(ilu_Passport, p);
}

SCM iluguile_call__protocol_exception(SCM _call)
{
  iluguile_SCMCall call = iluguile_scheme_to_ptr(iluguile_SCMCall, _call);
  ilu_ProtocolException pe = scm_call_call(call)->ca_pe;
  return iluguile_value_to_scheme(integer, (ilu_integer) pe);
}

SCM iluguile_call__error(SCM _call)
{
  iluguile_SCMCall call = iluguile_scheme_to_ptr(iluguile_SCMCall, _call);
  ilu_Error* e = scm_call_err(call);
  return iluguile_ptr_to_scheme(ilu_Error*, e);
}

SCM iluguile_call__method_of_call(SCM _call)
{
  iluguile_SCMCall call = iluguile_scheme_to_ptr(iluguile_SCMCall, _call);
  ilu_Method m = scm_call_call(call)->ca_method;
  return iluguile_ptr_to_scheme(ilu_Method, m);
}

SCM iluguile_class__method(SCM _c, SCM _num)
{
  ilu_Class c = iluguile_scheme_to_ptr(ilu_Class, _c);
  ilu_cardinal num = iluguile_scheme_to_value(cardinal, _num);
  ilu_Method m = c->cl_methods + num;
  return iluguile_ptr_to_scheme(ilu_Method, m);
}

