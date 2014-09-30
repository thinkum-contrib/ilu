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
/* $Id: ilu.cpp,v 1.89 1999/08/03 01:55:46 janssen Exp $ */
/* Last edited by Mike Spreitzer June 11, 1997 10:11 am PDT */

#include "ilu.hh"
#include <stdio.h>

#if ((defined(WIN32) || defined(WIN16)) && defined(_WINIO))
#include <winiodef.h>
#endif /* ((defined(WIN32) || defined(WIN16)) && defined(_WINIO)) */

#define ILU_BDn(x) ((x)?ilu_TRUE:ilu_FALSE)

ilu_Exception ilu::Success = NULL;
ilu_Exception ilu::ProtocolError = "ilu.ProtocolError";

static ilu_boolean cpp_registered = ilu_FALSE;
static ilu_cardinal cpp_lang = 47;

ilu_cardinal ilu::CppLangIdx(void)
{
  if (!cpp_registered) {
    cpp_lang = ilu_RegisterLanguage("C++");
    cpp_registered = ilu_TRUE;
  }
  return cpp_lang;
}

void ilu::FullPreferSuccess(ilu_Error *e, const char*atf, int atl)
{
  const char     *n, *f;
  int             l;
  if (ILU_ERROK(*e))
    return;
  n = ILU_ERR_NAME(*e);
  f = ilu_ErrorFile(e);
  l = ilu_ErrorLine(e);
  ILU_ERRPRINTF("ILU warning: error %s from %s:%d simplified at %s:%d\n",
		n, f, l, atf, atl);
  return;
}

static ilu_Mutex otmu = ILU_NIL;

void ilu::EnterOTMu()
{
  ILU_ERRS((internal, no_memory)) lerr;
  if (otmu == ILU_NIL)
    otmu = ilu_GetOTMutex();
  ilu_EnterMutex(otmu, &lerr);
  ILU_MUST_BE_SUCCESS(lerr);
}

void ilu::ExitOTMu()
{
  ILU_ERRS((internal, no_memory)) lerr = ILU_INIT_NO_ERR;
  if (otmu == ILU_NIL)
    otmu = ilu_GetOTMutex();
  ilu_ExitMutex(otmu, ilu_kernelFALSE, &lerr);
  ILU_MUST_BE_SUCCESS(lerr);
}

void ilu::ObjectTypeDefined(ilu_Class t)
{
  ILU_ERRS((internal, no_memory)) lerr;
  ilu_ObjectTypeDefined(t, &lerr);
  ILU_MUST_BE_SUCCESS(lerr);
}

ilu_Exception ilu::DefineException(ilu_CString i, ilu_CString e, ilu_CString typeidstring)
{
  ILU_ERRS((internal, no_memory)) lerr;
  ilu_Exception   ans;
  ans = ilu_DefineException(i, e, typeidstring, &lerr);
  ILU_MUST_BE_SUCCESS(lerr);
  return ans;
}

ilu_Class 
ilu::DefineObjectType(ilu_CString cl_name,
		      ilu_CString cl_brand,
		      ilu_CString cl_unique_id,
		      ilu_CString cl_singleton,
		      ilu_Boolean cl_optional,
		      ilu_Boolean cl_collectible,
		      ilu_CString cl_authentication,
		      ilu_Cardinal cl_method_count,
		      ilu_Cardinal cl_scls_count,
		      ilu_CString * cl_scls_ids)
{
  ILU_ERRS((internal, no_memory)) lerr;
  ilu_Class       ans;
  ans = ilu_DefineObjectType(cl_name, cl_brand, cl_unique_id,
			     cl_singleton, ILU_BDn(cl_optional),
			     ILU_BDn(cl_collectible), cl_authentication,
			     cl_method_count, cl_scls_count,
			     cl_scls_ids, &lerr);
  ILU_MUST_BE_SUCCESS(lerr);
  return ans;
}

void 
ilu::DefineMethod(ilu_Class c,
		  ilu_Cardinal i,
		  ilu_CString me_name,
		  ilu_Cardinal me_id,
		  ilu_Boolean me_cacheable,
		  ilu_Boolean me_asynchronous,
		  ilu_Cardinal me_exceptionCount,
		  ilu_Exception * me_exceptionVector,
		  ilu_Cardinal me_nargs,
		  ilu_CString me_ret_typeid)
{
  ILU_ERRS((internal, no_memory)) lerr;
  (void) ilu_DefineMethod(c, i, me_name, me_id, ILU_BDn(me_cacheable),
			  ILU_BDn(me_asynchronous), me_exceptionCount,
			  me_exceptionVector, me_nargs, me_ret_typeid,
			  &lerr);
  ILU_MUST_BE_SUCCESS(lerr);
}

ilu_Boolean ilu::StartCall(iluCall call, ilu_Server s, ilu_Class intro_type,
			   ilu_Method method)
{
  ilu_Connection newconn;

  ilu_StartCall(&call->call, s, intro_type, method, ilu::CppLangIdx(), (ilu_Passport) 0, &newconn, &call->err);
  if (newconn != NULL)
    iluServer::MonitorConn(newconn);

  return (ILU_ERROK(call->err));
}


/*Main Invariant holds, L2 otherwise unconstrained*/
ilu_Cardinal	ilu::BeginSizingException	(iluCall call, ilu_Cardinal eIndex)
{
  ilu_cardinal    ans;
  ans = ilu_BeginSizingException(&call->call, eIndex, &call->err);
  ilu_PreferSuccess(&call->err);
  return (ans);
}

/*L1_sup < cmu*/
/*before: L2 not >=   {call's conn's iomu},
	  L2     >=   {call's conn's callmu} iff protocol not concurrent;
   after: L2     >=   {call's conn's callmu, iomu} if result is true,
	  L2 disjoint {call's conn's callmu, iomu} if result is false. */
/*If not threaded:
  before: _ILU_C_ReadServiceRequest registered iff protocol concurrent;
  after:  _ILU_C_ReadServiceRequest not registered.*/
ilu_Boolean	ilu::BeginException(iluCall call, ilu_Cardinal eIndex,
	ilu_Cardinal argSize)
{
  ilu_Connection  conn = ilu_ConnectionOfCall(&call->call);
  ilu_boolean     ans;
  if (ilu_ThreadPerRequest(conn))
    iluServer::DisableRequests(call, conn);
  ans = ilu_BeginException(&call->call, eIndex, argSize, &call->err);
  ilu_PreferSuccess(&call->err);
  return (ans != 0);
}


ilu_Boolean	ilu::StartRequest(iluCall call, ilu_Cardinal argSize)
{
  ilu_boolean     ans;
  ans = ilu_StartRequest(&call->call, argSize, &call->err);
  ilu_PreferSuccess(&call->err);
  return (ans != 0);
}

/*Main Invariant holds, L2 otherwise unconstrained*/
ilu_Cardinal	ilu::BeginSizingReply	(iluCall call, ilu_Boolean exns_possible)
{
  ilu_cardinal    ans;
  ans = ilu_BeginSizingReply(&call->call, ILU_BDn(exns_possible), &call->err);
  ilu_PreferSuccess(&call->err);
  return (ans);
}

/*L1_sup < cmu*/
/*before: L2 not >=   {call's conn's iomu},
	  L2     >=   {call's conn's callmu} iff protocol not concurrent;
   after: L2     >=   {call's conn's callmu, iomu} if result is true,
	  L2 disjoint {call's conn's callmu, iomu} if result is false. */
/*If not threaded:
  before: _ILU_C_ReadServiceRequest registered iff protocol concurrent;
  after:  _ILU_C_ReadServiceRequest not registered.*/
ilu_Boolean	ilu::BeginReply	(iluCall call, ilu_Boolean exceptions, ilu_Cardinal argSize)
{
  ilu_Connection  conn = ilu_ConnectionOfCall(&call->call);
  ilu_boolean     ans;

  if (ilu_ThreadPerRequest(conn))
    iluServer::DisableRequests(call, conn);
  ans = ilu_BeginReply(&call->call, ILU_BDn(exceptions), argSize, &call->err);
  ilu_PreferSuccess(&call->err);
  return (ans != 0);
}


void		ilu::EndSequence	(iluCall call)
{
  ilu_EndSequence(&call->call, &call->err);
  ilu_PreferSuccess(&call->err);
  return;
}


void		ilu::EndUnion	(iluCall call)
{
  ilu_EndUnion(&call->call, &call->err);
  ilu_PreferSuccess(&call->err);
  return;
}


void		ilu::EndRecord	(iluCall call)
{
  ilu_EndRecord(&call->call, &call->err);
  ilu_PreferSuccess(&call->err);
  return;
}


void		ilu::EndArray	(iluCall call)
{
  ilu_EndArray(&call->call, &call->err);
  ilu_PreferSuccess(&call->err);
  return;
}


ilu_Exception	ilu::ExceptionOfMethod	(ilu_Method method, ilu_Cardinal index)
{
  return(ilu_ExceptionOfMethod((ilu_Method)method, index));
}


ilu_Class	ilu::GetObjectClass		(ilu_KernelObject obj)
{
  return(ilu_ClassOfObject (obj));
}


ilu_Server	ilu::GetObjectServer		(ilu_KernelObject obj)
{
  return(ilu_ServerOfObject (obj));
}

/*Main Invariant holds*/
/*before: L2 >= {call's conn's callmu, iomu};
  after: success <=> L2 >= {call's conn's callmu, iomu}*/
ilu_Boolean ilu::ReplyRead(iluCall call)
{
  ilu_ReplyRead(&call->call, &call->err);
  ilu_PreferSuccess(&call->err);
  return (ILU_ERROK(call->err));
}

ilu_Boolean	ilu::FinishCall	(iluCall call)
{
  ilu_FinishCall(&call->call, &call->err);
  ilu_PreferSuccess(&call->err);
  if (call->call.ca_incoming && !call->call.ca_reqs_enabled)
    iluServer::EnableRequests(call, ilu_ConnectionOfCall(&call->call));
  return (ILU_ERROK(call->err));
}


/*Main Invariant holds*/
/*L2    >=    {call's conn's callmu, iomu} before,
 *L2 disjoint {call's conn's callmu, iomu} after*/
/*If not threaded:
  before: _ILU_C_ReadServiceRequest not registered;
  after:  _ILU_C_ReadServiceRequest registered.*/
ilu_Boolean	ilu::FinishException (iluCall call)
{
  ilu_Connection conn = ilu_ConnectionOfCall(&call->call);
  ilu_kernelBoolean     ans;

  ans = ilu_FinishException(&call->call, &call->err);
  ilu_PreferSuccess(&call->err);
  return ans;
}


ilu_Boolean	ilu::FinishRequest	(iluCall call)
{
  ilu_kernelBoolean     ans;
  ans = ilu_FinishRequest(&call->call, &call->err);
  ilu_PreferSuccess(&call->err);
  return ans;
}

/*Main Invariant holds*/
/*L2    >=    {call's conn's callmu, iomu} before,
 *L2 disjoint {call's conn's callmu, iomu} after*/
/*If not threaded:
  before: _ILU_C_ReadServiceRequest not registered;
  after:  _ILU_C_ReadServiceRequest registered.*/
ilu_Boolean	ilu::FinishReply	(iluCall call)
{
  ilu_Connection conn = ilu_ConnectionOfCall(&call->call);
  ilu_kernelBoolean     ans;

  ans = ilu_FinishReply(&call->call, &call->err);
  ilu_PreferSuccess(&call->err);
  return ans;
}


void		ilu::NoReply	(iluCall call)
{
  ilu_Connection  conn = ilu_ConnectionOfCall(&call->call);
  ilu_kernelBoolean ans;

  ans = ilu_NoReply(&call->call, &call->err);
  ilu_PreferSuccess(&call->err);
  return;
}


/*Main invariant holds
 *before: L2     >=  {call's conn's callmu, iomu}
 *after:  L2 not >= {call's conn's         iomu},
 *after:  L2     >= {call's conn's callmu} iff protocol not concurrent*/
/*If not threaded:
  before: (iluServer)ReadServiceRequest not registered;
  after:  (iluServer)ReadServiceRequest registered iff protocol concurrent.*/
ilu_Boolean	ilu::FinishParameters	(iluCall call, void *obj)
{
  ilu_Connection  conn = ilu_ConnectionOfCall(&call->call);
  ilu_kernelBoolean ans;
  ans = ilu_RequestRead(&call->call, &call->err);
  ilu_PreferSuccess(&call->err);
  if (ilu_ThreadPerRequest(conn))
    iluServer::EnableRequests(call, conn);
  if (obj == NULL) {
    if (ilu_BeginException(&call->call, 0,
			   ilu_ProtocolException_GarbageArguments,
			   &call->err))
      ilu_FinishException(&call->call, &call->err);
    ilu_PreferSuccess(&call->err);
    return ilu_FALSE;
  }
  return ans;
}


unsigned char	ilu::InputByte	(iluCall call, ilu_Byte *b)
{
  ilu_byte b2;
  ilu_InputByte(&call->call, &b2, &call->err);
  ilu_PreferSuccess(&call->err);
  if (b != NULL)
    *b = b2;
  return(b2);
}


ilu_Boolean	ilu::InputBoolean	(iluCall call, ilu_Boolean *b)
{
  ilu_boolean b2;
  ilu_InputBoolean(&call->call, &b2, &call->err);
  ilu_PreferSuccess(&call->err);
  if (b != NULL)
    *b = b2;
  return(b2);
}


ilu_Boolean	ilu::InputOptional	(iluCall call, ilu_Boolean *b)
{
  ilu_boolean b2;
  ilu_InputOptional(&call->call, &b2, (ilu_Type)ILU_NIL, &call->err);
  ilu_PreferSuccess(&call->err);
  if (b != NULL)
    *b = b2;
  return(b2);
}


ilu_Cardinal	ilu::InputCardinal	(iluCall call, ilu_Cardinal *b)
{
  ilu_cardinal b2;
  ilu_InputCardinal(&call->call, &b2, &call->err);
  ilu_PreferSuccess(&call->err);
  if (b != NULL)
    *b = b2;
  return(b2);
}


ilu_Character	ilu::InputCharacter	(iluCall call, ilu_Character *b)
{
  ilu_character b2;
  ilu_InputCharacter(&call->call, &b2, &call->err);
  ilu_PreferSuccess(&call->err);
  if (b != NULL)
    *b = b2;
  return(b2);
}


ilu_ShortCharacter	ilu::InputShortCharacter	(iluCall call, ilu_ShortCharacter *b)
{
  ilu_ShortCharacter b2;
  ilu_InputShortCharacter(&call->call, &b2, &call->err);
  ilu_PreferSuccess(&call->err);
  if (b != NULL)
    *b = b2;
  return(b2);
}


ilu_ShortCardinal	ilu::InputEnum		(iluCall call, ilu_ShortCardinal *b)
{
  ilu_shortcardinal b2;
  ilu_InputEnum(&call->call, &b2, (ilu_Type)ILU_NIL, &call->err);
  ilu_PreferSuccess(&call->err);
  if (b != NULL)
    *b = b2;
  return((ilu_ShortCardinal) b2);
}


ilu_Integer	ilu::InputInteger	(iluCall call, ilu_Integer *b)
{
  ilu_integer b2;
  ilu_InputInteger(&call->call, &b2, &call->err);
  ilu_PreferSuccess(&call->err);
  if (b != NULL)
    *b = b2;
  return((ilu_Integer) b2);
}


ilu_Real	ilu::InputReal	(iluCall call, ilu_Real *b)
{
  double b2;
  ilu_InputReal(&call->call, &b2, &call->err);
  ilu_PreferSuccess(&call->err);
  if (b != NULL)
    *b = b2;
  return(b2);
}


ilu_ShortCardinal	ilu::InputShortCardinal	(iluCall call, ilu_ShortCardinal *b)
{
  ilu_shortcardinal b2;
  ilu_InputShortCardinal(&call->call, &b2, &call->err);
  ilu_PreferSuccess(&call->err);
  if (b != NULL)
    *b = b2;
  return(b2);
}


ilu_ShortInteger	ilu::InputShortInteger	(iluCall call, ilu_ShortInteger *b)
{
  ilu_shortinteger b2;
  ilu_InputShortInteger(&call->call, &b2, &call->err);
  ilu_PreferSuccess(&call->err);
  if (b != NULL)
    *b = b2;
  return(b2);
}


ilu_ShortReal	ilu::InputShortReal	(iluCall call, ilu_ShortReal *b)
{
  ilu_shortreal b2;
  ilu_InputShortReal(&call->call, &b2, &call->err);
  ilu_PreferSuccess(&call->err);
  if (b != NULL)
    *b = b2;
  return(b2);
}


ilu_LongCardinal	ilu::InputLongCardinal	(iluCall call, ilu_LongCardinal *b)
{
  ilu_longcardinal b2;
  ilu_InputLongCardinal(&call->call, &b2, &call->err);
  ilu_PreferSuccess(&call->err);
  if (b != NULL)
    *b = b2;
  return (b2);
}


ilu_LongInteger	ilu::InputLongInteger	(iluCall call, ilu_LongInteger *b)
{
  ilu_InputLongInteger(&call->call, b, &call->err);
  ilu_PreferSuccess(&call->err);
  return *b;
}


ilu_LongReal	ilu::InputLongReal	(iluCall call, ilu_LongReal *b)
{
  ilu_InputLongReal(&call->call, b, &call->err);
  ilu_PreferSuccess(&call->err);
  return (*b);
}


unsigned char *	ilu::InputBytes	(iluCall call, unsigned char * buf, ilu_Cardinal *len, ilu_Cardinal limit)
{
  ilu_bytes       b2 = buf;
  ilu_cardinal    len2;

  ilu_InputBytes(&call->call, &b2, &len2, limit, &call->err);
  ilu_PreferSuccess(&call->err);
  *len = len2;
  return (b2);
}


unsigned char *	ilu::InputOpaque	(iluCall call, unsigned char * buf, ilu_Cardinal len)
{
  ilu_opaque      buf2 = buf;

  ilu_InputOpaque(&call->call, &buf2, len, &call->err);
  ilu_PreferSuccess(&call->err);
  return (buf2);
}


ilu_CString	ilu::InputString	(iluCall call, ilu_CString buf, ilu_Cardinal *len, ilu_Cardinal limit)
{
  ilu_cardinal    len2;
  ilu_string      buf2 = buf;

  ilu_InputString(&call->call, &buf2, &len2, limit, &call->err);
  ilu_PreferSuccess(&call->err);
  if (len)
    *len = len2;
  return (buf2);
}


ilu_CString	ilu::InputStringVec	(iluCall call, ilu_CString buf, ilu_Cardinal len)
{
  ilu_CString buf2 = buf;

  ilu_InputStringVec(&call->call, &buf2, len, &call->err);
  ilu_PreferSuccess(&call->err);
  return(buf2);
}


ilu_WString	ilu::InputWString	(iluCall call, ilu_WString buf, ilu_Cardinal *len, ilu_Cardinal limit)
{
  ilu_cardinal    len2;
  ilu_WString     buf2 = buf;
  ilu_InputWString(&call->call, &buf2, &len2, limit, &call->err);
  ilu_PreferSuccess(&call->err);
  if (len)
    *len = len2;
  return (buf2);
}


ilu_WString	ilu::InputWStringVec	(iluCall call, ilu_WString buf, ilu_Cardinal len)
{
  ilu_WString     buf2 = buf;
  ilu_InputWStringVec(&call->call, &buf2, len, &call->err);
  ilu_PreferSuccess(&call->err);
  return (buf2);
}


ilu_KernelObject	ilu::InputObjectID	(iluCall call, ilu_Boolean discriminator_p, ilu_Class putative_class)
{
  ilu_KernelObject obj = NULL;
  ilu_InputObjectID(&call->call, &obj, ILU_BDn(discriminator_p),
		    putative_class, &call->err);
  ilu_PreferSuccess(&call->err);
  /* now obj != NULL => Inside(obj->server, putative_class) */
  return obj;
}


ilu_Cardinal	ilu::InputSequence	(iluCall call, ilu_Cardinal *count, ilu_Cardinal limit)
{
  ilu_cardinal c2;
  ilu_InputSequence(&call->call, &c2, limit, (ilu_Type)ILU_NIL, &call->err);
  ilu_PreferSuccess(&call->err);
  *count = c2;
  return(c2);
}


ilu_Cardinal	ilu::InputUnion	(iluCall call, ilu_Cardinal *discriminator, ilu_TypeKind discriminator_typekind)
{
  ilu_cardinal d2;
  ilu_InputUnion(&call->call, &d2, discriminator_typekind, (ilu_Type)ILU_NIL, &call->err);
  ilu_PreferSuccess(&call->err);
  *discriminator = d2;
  return d2;
}


ilu_Boolean	ilu::InputRecord	(iluCall call)
{
  ilu_InputRecord(&call->call,(ilu_Type)ILU_NIL, &call->err);
  ilu_PreferSuccess(&call->err);
  return 1;
}


ilu_Boolean	ilu::InputArray	(iluCall call)
{
  ilu_InputArray(&call->call, (ilu_Type)ILU_NIL, &call->err);
  ilu_PreferSuccess(&call->err);
  return 1;
}


/*after: Inside(ks, c) */
void	ilu::EnterServer	(ilu_Server ks, ilu_Class c)
{
  ilu_EnterServer(ks, c);
}

/*before: Inside(obj->server, obj->class) */
void	ilu::ExitServer		(ilu_Server ks, ilu_Class c)
{
  ilu_ExitServer(ks, c);
}


void *		ilu::SBHToObject	(ilu_CString sbh, ilu_Class c)
{
  ilu_KernelObject obj;
  iluObject      *o;
  ILU_ERRS((bad_locks, broken_locks, inv_objref,
	    no_memory, internal)) lerr;

  obj = ilu_ObjectOfSBH(sbh, c, &lerr);
  ilu_PreferSuccess(&lerr);
  if (obj == NULL)
    return NULL;

  /* now Inside(obj->server, c) */
  o = (class iluObject *) ilu_GetLanguageSpecificObject(obj,
						     ilu::CppLangIdx());
  if (o == NULL)
    o = iluObject::CreateFromRegistry(ilu_ClassOfObject(obj), obj);
  ilu_ExitServer(ilu_ServerOfObject(obj), c);
  if (o == NULL)
    return (NULL);
  return ((void *) o->ILUCastDown(c));
}

/* before: Inside(obj->server, obj->class) */
ilu_CString	ilu::SBHOfObject	(ilu_KernelObject obj)
{
  ilu_Server	s	= ilu_ServerOfObject(obj);
  ilu_Class	c	= ilu_ClassOfObject(obj);
  ilu_CString	result;

  result = ilu_SBHOfObject(obj);
  ilu_ExitServer(s, c);
  return result;
}

ilu_Boolean	ilu::OutputByte	(iluCall call, unsigned char byte)
{
  ilu_OutputByte(&call->call, byte, &call->err);
  ilu_PreferSuccess(&call->err);
  return((ilu_Boolean) 1);
}

ilu_Boolean	ilu::OutputBoolean (iluCall call, ilu_Boolean b)
{
  ilu_OutputBoolean(&call->call, ILU_BDn(b), &call->err);
  ilu_PreferSuccess(&call->err);
  return((ilu_Boolean) 1);
}


ilu_Boolean	ilu::OutputOptional	(iluCall call, ilu_Boolean opt)
{
  ilu_OutputOptional(&call->call, ILU_BDn(opt), (ilu_Type)ILU_NIL, &call->err);
  ilu_PreferSuccess(&call->err);
  return((ilu_Boolean) 1);
}


ilu_Boolean	ilu::OutputCardinal	(iluCall call, ilu_Cardinal val)
{
  ilu_OutputCardinal(&call->call, val, &call->err);
  ilu_PreferSuccess(&call->err);
  return((ilu_Boolean) 1);
}


ilu_Boolean	ilu::OutputCharacter	(iluCall call, ilu_Character val)
{
  ilu_OutputCharacter(&call->call, val, &call->err);
  ilu_PreferSuccess(&call->err);
  return((ilu_Boolean) 1);
}


ilu_Boolean	ilu::OutputEnum		(iluCall call, ilu_ShortCardinal val)
{
  ilu_OutputEnum(&call->call, val, (ilu_Type)ILU_NIL, &call->err);
  ilu_PreferSuccess(&call->err);
  return((ilu_Boolean) 1);
}


ilu_Boolean	ilu::OutputInteger	(iluCall call, ilu_Integer val)
{
  ilu_OutputInteger(&call->call, val, &call->err);
  ilu_PreferSuccess(&call->err);
  return((ilu_Boolean) 1);
}


ilu_Boolean	ilu::OutputReal	(iluCall call, double val)
{
  ilu_OutputReal(&call->call, val, &call->err);
  ilu_PreferSuccess(&call->err);
  return((ilu_Boolean) 1);
}


ilu_Boolean	ilu::OutputShortCardinal	(iluCall call, unsigned short val)
{
  ilu_OutputShortCardinal(&call->call, val, &call->err);
  ilu_PreferSuccess(&call->err);
  return((ilu_Boolean) 1);
}


ilu_Boolean	ilu::OutputShortInteger	(iluCall call, short val)
{
  ilu_OutputShortInteger(&call->call, val, &call->err);
  ilu_PreferSuccess(&call->err);
  return((ilu_Boolean) 1);
}


ilu_Boolean	ilu::OutputShortReal	(iluCall call, float val)
{
  ilu_OutputShortReal(&call->call, val, &call->err);
  ilu_PreferSuccess(&call->err);
  return((ilu_Boolean) 1);
}

ilu_Boolean	ilu::OutputLongCardinal	(iluCall call, ilu_LongCardinal val)
{
  ilu_OutputLongCardinal(&call->call, val, &call->err);
  ilu_PreferSuccess(&call->err);
  return ((ilu_Boolean) 1);
}


ilu_Boolean	ilu::OutputLongInteger	(iluCall call, ilu_LongInteger val)
{
  ilu_OutputLongInteger(&call->call, val, &call->err);
  ilu_PreferSuccess(&call->err);
  return ((ilu_Boolean) 1);
}


ilu_Boolean	ilu::OutputLongReal	(iluCall call, ilu_LongReal val)
{
  ilu_OutputLongReal(&call->call, val, &call->err);
  ilu_PreferSuccess(&call->err);
  return ((ilu_Boolean) 1);
}

ilu_Boolean	ilu::OutputBytes	(iluCall call, unsigned char * bytes, ilu_Cardinal len, ilu_Cardinal limit)
{
  ilu_OutputBytes(&call->call, bytes, len, limit, &call->err);
  ilu_PreferSuccess(&call->err);
  return((ilu_Boolean) 1);
}


ilu_Boolean	ilu::OutputOpaque	(iluCall call, unsigned char * buf, ilu_Cardinal len)
{
  ilu_OutputOpaque(&call->call, buf, len, &call->err);
  ilu_PreferSuccess(&call->err);
  return((ilu_Boolean) 1);
}


ilu_Boolean	ilu::OutputString	(iluCall call, ilu_CString buf, ilu_Cardinal len, ilu_Cardinal limit)
{
  ilu_OutputString(&call->call, buf, len, limit, &call->err);
  ilu_PreferSuccess(&call->err);
  return((ilu_Boolean) 1);
}


ilu_Boolean	ilu::OutputStringVec	(iluCall call, ilu_CString buf, ilu_Cardinal len)
{
  ilu_OutputStringVec(&call->call, buf, len, &call->err);
  ilu_PreferSuccess(&call->err);
  return((ilu_Boolean) 1);
}


ilu_Boolean	ilu::OutputWString	(iluCall call, ilu_WString buf, ilu_Cardinal len, ilu_Cardinal limit)
{
  ilu_OutputWString(&call->call, buf, len, limit, &call->err);
  ilu_PreferSuccess(&call->err);
  return ((ilu_Boolean) 1);
}


ilu_Boolean	ilu::OutputWStringVec(iluCall call, ilu_WString buf, ilu_Cardinal len)
{
  ilu_OutputWStringVec(&call->call, buf, len, &call->err);
  ilu_PreferSuccess(&call->err);
  return ((ilu_Boolean) 1);
}


/* before: obj != NULL implies Inside(obj->server, obj->class) */
ilu_Boolean	ilu::OutputObjectID	(iluCall call, ilu_KernelObject obj, ilu_Boolean discriminator_p, ilu_Class putative_class)
{
  ilu_OutputObjectID(&call->call, obj, ILU_BDn(discriminator_p),
		     putative_class, &call->err);
  ilu_PreferSuccess(&call->err);
  return ((ilu_Boolean) 1);
}


ilu_Boolean	ilu::OutputSequence	(iluCall call, ilu_Cardinal length, ilu_Cardinal limit)
{
  ilu_OutputSequence(&call->call, length, limit, (ilu_Type)ILU_NIL, &call->err);
  ilu_PreferSuccess(&call->err);
  return((ilu_Boolean) 1);
}


ilu_Boolean	ilu::OutputUnion	(iluCall call, ilu_Cardinal discriminator,  ilu_TypeKind discriminator_typekind)
{
  ilu_OutputUnion(&call->call, discriminator, discriminator_typekind, (ilu_Type)ILU_NIL, &call->err);
  ilu_PreferSuccess(&call->err);
  return((ilu_Boolean) 1);
}


ilu_Boolean	ilu::OutputArray	(iluCall call)
{
	/***** this 0 needs to change to the actual number of elements to output */

  ilu_OutputArray(&call->call, 0, (ilu_Type)ILU_NIL, &call->err);
  ilu_PreferSuccess(&call->err);
  return ((ilu_Boolean) 1);
}


ilu_Boolean	ilu::OutputRecord	(iluCall call)
{
  ilu_OutputRecord(&call->call, (ilu_Type)ILU_NIL, &call->err);
  ilu_PreferSuccess(&call->err);
  return((ilu_Boolean) 1);
}


ilu_Cardinal	ilu::SizeOfByte	(iluCall call, unsigned char byte)
{
  ilu_cardinal    ans;
  ans = ilu_SizeOfByte(&call->call, byte, &call->err);
  ilu_PreferSuccess(&call->err);
  return ans;
}


ilu_Cardinal	ilu::SizeOfBoolean	(iluCall call, ilu_Boolean b)
{
  ilu_cardinal    ans;
  ans = ilu_SizeOfBoolean(&call->call, ILU_BDn(b), &call->err);
  ilu_PreferSuccess(&call->err);
  return ans;
}


ilu_Cardinal	ilu::SizeOfOptional	(iluCall call, ilu_Boolean opt)
{
  ilu_cardinal    ans;
  ans = ilu_SizeOfOptional(&call->call, ILU_BDn(opt), (ilu_Type)ILU_NIL, &call->err);
  ilu_PreferSuccess(&call->err);
  return ans;
}


ilu_Cardinal	ilu::SizeOfCardinal	(iluCall call, ilu_Cardinal val)
{
  ilu_cardinal    ans;
  ans = ilu_SizeOfCardinal(&call->call, val, &call->err);
  ilu_PreferSuccess(&call->err);
  return ans;
}


ilu_Cardinal	ilu::SizeOfCharacter	(iluCall call, ilu_Character val)
{
  ilu_cardinal    ans;
  ans = ilu_SizeOfCharacter(&call->call, val, &call->err);
  ilu_PreferSuccess(&call->err);
  return ans;
}


ilu_Cardinal	ilu::SizeOfEnum		(iluCall call, ilu_ShortCardinal val)
{
  ilu_cardinal    ans;
  ans = ilu_SizeOfEnum(&call->call, val, (ilu_Type)ILU_NIL, &call->err);
  ilu_PreferSuccess(&call->err);
  return ans;
}


ilu_Cardinal	ilu::SizeOfInteger	(iluCall call, ilu_Integer val)
{
  ilu_cardinal    ans;
  ans = ilu_SizeOfInteger(&call->call, val, &call->err);
  ilu_PreferSuccess(&call->err);
  return ans;
}


ilu_Cardinal	ilu::SizeOfReal	(iluCall call, double val)
{
  ilu_cardinal    ans;
  ans = ilu_SizeOfReal(&call->call, val, &call->err);
  ilu_PreferSuccess(&call->err);
  return ans;
}


ilu_Cardinal	ilu::SizeOfShortCardinal	(iluCall call, ilu_ShortCardinal val)
{
  ilu_cardinal    ans;
  ans = ilu_SizeOfShortCardinal(&call->call, val, &call->err);
  ilu_PreferSuccess(&call->err);
  return ans;
}


ilu_Cardinal	ilu::SizeOfShortInteger	(iluCall call, short val)
{
  ilu_cardinal    ans;
  ans = ilu_SizeOfShortInteger(&call->call, val, &call->err);
  ilu_PreferSuccess(&call->err);
  return ans;
}


ilu_Cardinal	ilu::SizeOfShortReal	(iluCall call, float val)
{
  ilu_cardinal    ans;
  ans = ilu_SizeOfShortReal(&call->call, val, &call->err);
  ilu_PreferSuccess(&call->err);
  return ans;
}


ilu_Cardinal	ilu::SizeOfLongCardinal	(iluCall call, ilu_LongCardinal val)
{
  ilu_cardinal    ans;
  ans = ilu_SizeOfLongCardinal(&call->call, val, &call->err);
  ilu_PreferSuccess(&call->err);
  return ans;
}


ilu_Cardinal	ilu::SizeOfLongInteger	(iluCall call, ilu_LongInteger val)
{
  ilu_cardinal    ans;
  ans = ilu_SizeOfLongInteger(&call->call, val, &call->err);
  ilu_PreferSuccess(&call->err);
  return ans;
}


ilu_Cardinal	ilu::SizeOfLongReal	(iluCall call, ilu_LongReal val)
{
  ilu_cardinal    ans;
  ans = ilu_SizeOfLongReal(&call->call, val, &call->err);
  ilu_PreferSuccess(&call->err);
  return ans;
}


ilu_Cardinal	ilu::SizeOfBytes	(iluCall call, unsigned char * buf, ilu_Cardinal len, ilu_Cardinal limit)
{
  ilu_cardinal    ans;
  ans = ilu_SizeOfBytes(&call->call, buf, len, limit, &call->err);
  ilu_PreferSuccess(&call->err);
  return ans;
}


ilu_Cardinal	ilu::SizeOfOpaque	(iluCall call, unsigned char * buf, ilu_Cardinal len)
{
  ilu_cardinal    ans;
  ans = ilu_SizeOfOpaque(&call->call, buf, len, &call->err);
  ilu_PreferSuccess(&call->err);
  return ans;
}


ilu_Cardinal	ilu::SizeOfString	(iluCall call, ilu_CString buf, ilu_Cardinal len, ilu_Cardinal limit)
{
  ilu_cardinal    ans;
  ans = ilu_SizeOfString(&call->call, buf, len, limit, &call->err);
  ilu_PreferSuccess(&call->err);
  return ans;
}


ilu_Cardinal	ilu::SizeOfStringVec	(iluCall call, ilu_CString buf, ilu_Cardinal len)
{
  ilu_cardinal    ans;
  ans = ilu_SizeOfStringVec(&call->call, buf, len, &call->err);
  ilu_PreferSuccess(&call->err);
  return ans;
}


ilu_Cardinal	ilu::SizeOfWString	(iluCall call, ilu_WString buf, ilu_Cardinal len, ilu_Cardinal limit)
{
  ilu_cardinal    ans;
  ans = ilu_SizeOfWString(&call->call, buf, len, limit, &call->err);
  ilu_PreferSuccess(&call->err);
  return ans;
}


ilu_Cardinal	ilu::SizeOfWStringVec	(iluCall call, ilu_WString buf, ilu_Cardinal len)
{
  ilu_cardinal    ans;
  ans = ilu_SizeOfWStringVec(&call->call, buf, len, &call->err);
  ilu_PreferSuccess(&call->err);
  return ans;
}


/* before: obj != NULL implies Inside(obj->server, obj->class) */
ilu_Cardinal	ilu::SizeOfObjectID	(iluCall call, ilu_KernelObject obj, ilu_Boolean discriminator_p, ilu_Class putative_class)
{
  ilu_Cardinal    size;

  size = ilu_SizeOfObjectID(&call->call, obj, ILU_BDn(discriminator_p),
			    putative_class, &call->err);
  ilu_PreferSuccess(&call->err);
  if (obj != NULL)
    ilu_ExitServer(ilu_ServerOfObject(obj), ilu_ClassOfObject(obj));
  return size;
}


ilu_Cardinal	ilu::SizeOfSequence	(iluCall call, ilu_Cardinal len, ilu_Cardinal limit)
{
  ilu_cardinal    ans;
  ans = ilu_SizeOfSequence(&call->call, len, limit, (ilu_Type)ILU_NIL, &call->err);
  ilu_PreferSuccess(&call->err);
  return ans;
}


ilu_Cardinal	ilu::SizeOfUnion	(iluCall call, ilu_Cardinal discriminator, ilu_TypeKind discriminator_typekind)
{
  ilu_cardinal    ans;
  ans = ilu_SizeOfUnion(&call->call, discriminator, discriminator_typekind, (ilu_Type)ILU_NIL, &call->err);
  ilu_PreferSuccess(&call->err);
  return ans;
}

ilu_Cardinal	ilu::SizeOfArray	(iluCall call)
{
	/****** this 0 needs to change to the actual number of elements to output */

  ilu_cardinal    ans;
  ans = ilu_SizeOfArray(&call->call, 0, (ilu_Type)ILU_NIL, &call->err);
  ilu_PreferSuccess(&call->err);
  return ans;
}

ilu_Cardinal	ilu::SizeOfRecord	(iluCall call)
{
  ilu_cardinal    ans;
  ans = ilu_SizeOfRecord(&call->call, (ilu_Type)ILU_NIL, &call->err);
  ilu_PreferSuccess(&call->err);
  return ans;
}

ilu_ProtocolException ilu::WaitForReply (iluCall call,
					 ilu_Cardinal *successCode)
{
  ilu_ProtocolException ans;
  ilu_cardinal    sc;
  ilu_Connection newconn;
  
  ans = ilu_GetReply(&call->call, &sc, &newconn, &call->err);
  if (newconn != NULL)
    iluServer::MonitorConn(newconn);
  ilu_PreferSuccess(&call->err);
  *successCode = sc;
  return ans;
}

static class iluServer *DefaultServer = NULL;

void ilu::SetDefaultServer(class iluServer *s)
{
  DefaultServer = s;
}

class iluServer *ilu::GetDefaultServer()
{
  return DefaultServer;
}

ilu_Port ilu::FullCreatePort (ilu_Server server, char *protocolType, ilu_TransportInfo transportType, ilu_Boolean be_private)
{
  char           *p;
  ilu_Error	err;
  ilu_TransportInfo t;
  ilu_Port	port;

  if ((p = protocolType) == NULL)
    p = ilu_DefaultProtocolInfo();
  if ((t = transportType) == NULL)
    t = ilu_DefaultTransportInfo();
  port = ilu_FullCreatePort(server, p, t, (_ilu_Passport_s*)ILU_NIL, (be_private == 0) ? ilu_TRUE : ilu_FALSE, &err);
  if (ILU_ERROK(err))
    return port;
  else
    {
      ILU_HANDLED(err);
      return NULL;
    }
}

ilu_Port ilu::CreatePort (ilu_Server server, char *protocolType, ilu_TransportInfo transportType)
{
  return ilu::FullCreatePort (server, protocolType, transportType, 0);
}

void ilu::SetServerDefaultPort(ilu_Server s, ilu_Port p)
{
  ilu_SetServerDefaultPort(s, p);
  return;
}

ilu_Boolean 
ilu::ParseSBH(ilu_CString sbh, ilu_CString * plainInstH,
	      ilu_CString * plainServerID,
	      ilu_CString * plainMstid,
	      ilu_CString * encodedContactInfo,
	      ilu_Cardinal * cinfolen)
{
  ilu_boolean     ans;
  ilu_cardinal    cinfolen2;
  ilu_Error	  lerr;
  ilu_boolean	  pass_cinfo = ilu_FALSE;

  ans = ilu_ParseSBH(sbh, plainInstH, plainServerID, plainMstid,
		     encodedContactInfo,
		     cinfolen ? &cinfolen2 : (ilu_cardinal *) 0,
		     &pass_cinfo, &lerr);
  ilu_PreferSuccess(&lerr);
  if (cinfolen)
    *cinfolen = cinfolen2;
  return ans;
}

ilu_RcvReqStat ilu::ReceiveRequest (ilu_Connection conn, iluCall call, ilu_Boolean *initted, ilu_Class *pclass, ilu_Method *method, ilu_Cardinal *serial_number)
{
  ilu_RcvReqStat  ans;
  ilu_cardinal    sn = 0;
  ilu_boolean	initted2;

  ans = ilu_ReceiveRequest(&call->call, &initted2, conn, pclass, method, &sn, &call->err);
  ilu_PreferSuccess(&call->err);
  *initted = initted2;
  *serial_number = sn;
  return ans;
}

ilu_Connection	ilu::HandleNewConnection	(ilu_Port port,
		                       ilu_boolean * closed,
		                       ILU_ERRS((IoErrs, bad_locks,
			                     no_resources)) * err)
{
  return ilu_HandleNewConnection(port, closed, err);
}

ilu_Boolean	ilu::DoneWithPort	(ilu_Port port,
		                       ILU_ERRS((bad_param, bad_locks,
					   internal)) * err)
{
  return ilu_DoneWithPort(port, err);
}

ilu_Boolean	ilu::DoneServingConnection	(ilu_Connection conn,
				ILU_ERRS((bad_param, broken_locks,
					  bad_locks, internal)) * err)
{
  return (ilu_DoneServingConnection(conn, err) != ilu_FALSE);
}

ilu_Boolean	ilu::WaitForPortConnectionRequest	(ilu_Port p,
				 ILU_ERRS((bad_locks, broken_locks,
					   interrupted)) * err)
{
  return (ilu_WaitForPortConnectionRequest(p, err) != ilu_FALSE);
}

void
ilu::CheckStubConsistency (char *interface_name, char *ilu_version, char *type_uid_version)
{
  if (strcmp(ilu_version, ilu_GetILUVersion()) != 0) {
    ilu_DebugPrintf("ILU:  Warning!  The C++ stubs for interface \"%s\" were generated for ILU version \"%s\", while the ILU kernel library you are using is version \"%s\".\n", interface_name, ilu_version, ilu_GetILUVersion());
    _ilu_Assert(strcmp(ilu_version, ilu_GetILUVersion()) == 0, "ILU C++ stub version mismatch with ILU kernel version");
  };
  if (strcmp(type_uid_version, ilu_GetILUTypeUIDVersion()) != 0) {
    ilu_DebugPrintf("ILU:  Warning!  The type UID version used in the C++ stubs for interface \"%s\" is \"%s\", while the ILU kernel library expects version \"%s\".\n", interface_name, type_uid_version, ilu_GetILUTypeUIDVersion());
    _ilu_Assert(strcmp(type_uid_version, ilu_GetILUTypeUIDVersion()) == 0, "ILU C++ stub type UID version mismatch with ILU kernel type UID version");
  };
}

/* Inside(s, c) */
ilu_KernelObject	ilu::CreateTrueKernelObject	(char *ih, ilu_Server server, ilu_Class c, void * lspo)
{
  return ilu_FindOrCreateTrueObject (ih, server, c, lspo);
}

ilu_Class	ilu::FindClassFromTypeName	(char *name)
{
  return ((ilu_Class) ilu_FindClassFromName(name));
}

ilu_Class	ilu::FindClassFromID	(char *id)
{
  return ((ilu_Class) ilu_FindClassFromID(id));
}

/* Inside(obj->server, obj->class) */
class iluObject *	ilu::GetLanguageSpecificObject	(ilu_KernelObject obj)
{
  return (class iluObject *) ilu_GetLanguageSpecificObject(obj,
						ilu::CppLangIdx());
}

/* Inside(obj->server, obj->class) */
void		ilu::SetLanguageSpecificObject	(ilu_KernelObject obj, class iluObject * lspo)
{
  ilu_RegisterLanguageSpecificObject(obj, lspo, ilu::CppLangIdx());
}

void ilu::RunMainLoop (int *stop)
{
  ilu_RunMainLoop(stop);
  return;
}

void ilu::ExitMainLoop (int *stop)
{
  ilu_ExitMainLoop (stop);
}

ilu_Boolean ilu::RegisterInputHandler (int fd,
					     void (*handlerProc)
						  (int fd, void *rock),
					     void *rock)
{
  return (ilu_RegisterInputSource (fd, handlerProc, rock));
}

ilu_Boolean ilu::UnregisterInputHandler (int fd)
{
  return (ilu_UnregisterInputSource (fd));
}

ilu_Boolean ilu::RegisterOutputHandler (int fd,
					     void (*handlerProc)
						  (int fd, void *rock),
					     void *rock)
{
  return (ilu_RegisterOutputSource (fd, handlerProc, rock));
}

ilu_Boolean ilu::UnregisterOutputHandler (int fd)
{
  return (ilu_UnregisterOutputSource (fd));
}

void ilu::InitializeRuntime(void)
{
  static int initialized = 0;
  if (initialized == 0) {
    iluServer::InitializeThreading();
    iluObject::InitializeGCCallback();
    iluObject::InitializeCORBAObject();
    initialized = 1;
  }
}
