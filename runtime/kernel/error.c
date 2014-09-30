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
/*
*/
/* $Id: error.c,v 1.56 1999/08/09 23:56:43 janssen Exp $ */
/* Last edited by Mike Spreitzer September 22, 1998 11:26 pm PDT */

/* Standard error machinery, and defs of standard errors */

#define _POSIX_SOURCE

#include "iluntrnl.h"

ilu_Error       ilu_success_err = {NIL, 0, 0};

ilu_string      ilu_RelocateScope_Names[2] = {"call", "conn"};

static ilu_ErrorTypeDetails typeDetails[ILU_ERRTYP(ErrListLen)];

ilu_ErrorTypeDetails ilu_GetErrorTypeDetails(int et)
{
  _ilu_Assert(0 <= et && et < ILU_ERRTYP(ErrListLen),
	      "uninitialized ilu_Error?");
  if (typeDetails[et] == NIL) {
#define ILU_ERRLISTELT(id) typeDetails[ILU_ERRTYP(id)]= ILU_ERRTYPDET(id);
    ILU_ERRLIST;
#undef ILU_ERRLISTELT
    _ilu_Assert(typeDetails[et] != NIL,
		"construction of error.c:typeDetails failed");
  }
  return typeDetails[et];
}

unsigned long
ilu_CORBAizeSystemErr(ilu_Error * err,
		      ilu_integer * major)
{ return ilu_FullCORBAizeSystemErr(err, major, NIL, NIL); }

static ilu_cardinal lgt32tab[] = {
#include "ilulgt32.h"
  0};

static unsigned lgt32(ilu_cardinal n)
{
  unsigned i = 893;
  for (i = 893; ; i--) {
    if (n >= lgt32tab[i])
      return i;
  }
}

unsigned long
ilu_FullCORBAizeSystemErr(ilu_Error     *err,
			  ilu_integer   *major,
			  const char   **ilusl_filename,
			  int		*ilusl_linenum)
{
  unsigned long   minor = 0;
  ilu_boolean	withLoc = ilu_FALSE;
  *major = err->ilu_type - ILU_ERRTYP(unknown);
  if (ilusl_filename) *ilusl_filename = NIL;
  if (ilusl_linenum)  *ilusl_linenum  = 0;
  ILU_ERR_SWITCH(*err) {
    ILU_ERR_CASE(unknown, v)		minor = ILU_VMCID_BASE;
    ILU_ERR_CASE(bad_param, v)		minor = v->minor;
    ILU_ERR_CASE(no_memory, v)		minor = ILU_VMCID_BASE + lgt32(v->nbytes);
    ILU_ERR_CASE(imp_limit, v)		minor = v->minor;
    ILU_ERR_CASE(comm_failure, v)	minor = v->minor;
    ILU_ERR_CASE(inv_objref, v)		minor = v->minor;
    ILU_ERR_CASE(no_permission, v)	minor = ILU_VMCID_BASE;
    ILU_ERR_CASE(marshal, v)		minor = v->minor;
    ILU_ERR_CASE(initialize, v)		minor = ILU_VMCID_BASE;
    ILU_ERR_CASE(no_implement, v)	minor = ILU_VMCID_BASE;
    ILU_ERR_CASE(bad_typecode, v)	minor = v->minor;
    ILU_ERR_CASE(bad_operation, v)	minor = v->minor;
    ILU_ERR_CASE(no_resources, v)	minor = v->minor;
    ILU_ERR_CASE(no_response, v)	minor = ILU_VMCID_BASE;
    ILU_ERR_CASE(persist_store, v)	minor = ILU_VMCID_BASE;
    ILU_ERR_CASE(bad_inv_order, v)	minor = ILU_VMCID_BASE;
    ILU_ERR_CASE(transient, v)		minor = v->minor;
    ILU_ERR_CASE(free_mem, v)		minor = ILU_VMCID_BASE;
    ILU_ERR_CASE(inv_ident, v)		minor = ILU_VMCID_BASE;
    ILU_ERR_CASE(inv_flag, v)		minor = ILU_VMCID_BASE;
    ILU_ERR_CASE(intf_repos, v)		minor = ILU_VMCID_BASE;
    ILU_ERR_CASE(bad_context, v)	minor = ILU_VMCID_BASE;
    ILU_ERR_CASE(obj_adapter, v)	minor = ILU_VMCID_BASE;
    ILU_ERR_CASE(data_conversion, v)	minor = ILU_VMCID_BASE;
    ILU_ERR_CASE(barrier, v)		minor = ILU_VMCID_BASE;
    ILU_ERR_CASE(internal, v)		minor = v->minor;
    ILU_ERR_CASE(bad_locks, v) {
      *major = ILU_ERRTYP(internal) - ILU_ERRTYP(unknown);
      minor = ilu_im_badLocks;
    }
    ILU_ERR_CASE(broken_locks, v) {
      *major = ILU_ERRTYP(internal) - ILU_ERRTYP(unknown);
      minor = ilu_im_brokenLocks;
    }
    ILU_ERR_CASE(interrupted, v) {
      *major = ILU_ERRTYP(no_response) - ILU_ERRTYP(unknown);
      minor = ILU_VMCID_BASE + v->ilu_interruptSet;
    }
    ILU_ERR_CASE(gss_security, v) {
      *major = ILU_ERRTYP(no_permission) - ILU_ERRTYP(unknown);
      minor = ILU_VMCID_BASE + v->major;
    }
    ILU_ERR_ELSE {
      ILU_HANDLED(*err);
      *major = -1;
      return 0;
    }
  }
  ILU_ERR_ENDSWITCH;
  if (err->ilu_file && err->ilu_file[0]) {
    if (ilusl_filename) *ilusl_filename = err->ilu_file;
    if (ilusl_linenum ) *ilusl_linenum  = err->ilu_line;
    withLoc = ilusl_filename && ilusl_linenum;
  }
  if (withLoc)
    minor = minor | ILU_VMCID_EXTRA;
  else
    minor = minor & ~ILU_VMCID_EXTRA;
  ILU_HANDLED(*err);
  return minor;
}

const char *ilu_DescribeCorbaMinor(ilu_integer major,
				   unsigned long minor)
{
  if (major < 0 || major >= ILU_ERRTYP(ErrListLen)-ILU_ERRTYP(unknown))
    return ILU_NIL;
  return ilu_GetMinorDescrFromCodes(major+ILU_ERRTYP(unknown), minor);
}

const char * ilu_GetMinorDescrFromCodes(unsigned et,
					unsigned long minor)
{
  ilu_ErrorTypeDetails etd;
  if (et >= ILU_ERRTYP(ErrListLen) || minor < ILU_VMCID_BASE)
    return ILU_NIL;
  minor = (minor - ILU_VMCID_BASE) & ~ILU_VMCID_EXTRA;
  etd = ilu_GetErrorTypeDetails(et);
  if (etd->minors && minor < etd->numMinors)
    return etd->minors[minor];
  return ILU_NIL;
}

void ilu_MapProtocolExceptionToError (ilu_ProtocolException v_exception, ilu_Error *p_error, ilu_Completion *completed)
{
  switch (v_exception)
    {
    case ilu_ProtocolException_Success:
      ILU_ERR_CONS1(internal, p_error, minor, ilu_im_convPESuccess, 0);
      if (completed) *completed = ILU_COMPLETED_MAYBE;
      break;
    case ilu_ProtocolException_NoSuchClassAtServer:
      ILU_ERR_CONS1(bad_typecode, p_error, minor, ilu_btm_convNoSuchClass, 0);
      if (completed) *completed = ILU_COMPLETED_NO;
      break;
    case ilu_ProtocolException_ClassVersionMismatch:
      ILU_ERR_CONS1(bad_typecode, p_error, minor, ilu_btm_convVersionMismatch, 0);
      if (completed) *completed = ILU_COMPLETED_NO;
      break;
    case ilu_ProtocolException_NoSuchMethodOnClass:
      ILU_ERR_CONS1(bad_operation, p_error, minor, ilu_bom_convProtocolExcn, 0);
      if (completed) *completed = ILU_COMPLETED_NO;
      break;
    case ilu_ProtocolException_GarbageArguments:
      ILU_ERR_CONS1(bad_param, p_error, minor, ilu_bpm_convProtocolExcn, 0);
      if (completed) *completed = ILU_COMPLETED_MAYBE;
      break;
    case ilu_ProtocolException_Unknown:
      ILU_ERR_CONS0(unknown, p_error, 0);
      if (completed) *completed = ILU_COMPLETED_MAYBE;
      break;
    case ilu_ProtocolException_LostConnection:
      ILU_ERR_CONS1(comm_failure, p_error, minor, ilu_cfm_convProtocolExcn, 0);
      if (completed) *completed = ILU_COMPLETED_MAYBE;
      break;
    case ilu_ProtocolException_RequestRejected:
      ILU_ERR_CONS0(no_permission, p_error, 0);
      if (completed) *completed = ILU_COMPLETED_NO;
      break;
    case ilu_ProtocolException_RequestTimeout:
      ILU_ERR_CONS0(no_response, p_error, 0);
      if (completed) *completed = ILU_COMPLETED_MAYBE;
      break;
    default:
      ILU_ERR_CONS1(internal, p_error, minor, ilu_im_invalidPE, 0);
      if (completed) *completed = ILU_COMPLETED_MAYBE;
      break;
    }
}

void ilu_FreeErrp(ilu_Error *e)
{
  ilu_ErrorTypeDetails td;
  _ilu_Assert(e != NIL, "ilu_FreeErrp(NIL)");
  if (ILU_ERROK(*e))
    return;
  td = ilu_GetErrorTypeDetails((int) (e->ilu_type));
  (*td->freeproc) (e);
  return;
}

void ilu_FreeNestedErr(ilu_Error *e)
{
  if (e != NIL) {
    ilu_FreeErrp(e);
    ilu_free(e);
  }
  return;
}

ilu_Error      *
ilu_ErrDup(ilu_Error * e)
{
  ilu_Error      *e2 = (ilu_Error *) ilu_malloc(sizeof(ilu_Error));
  if (e2 != NIL)
    *e2 = *e;
  return e2;

}

const char     *
ilu_ErrorFile(ilu_Error * e)
{
  _ilu_Assert(e != NIL, "ilu_ErrorFile(NIL)");
  return e->ilu_file;
}

int 
ilu_ErrorLine(ilu_Error * e)
{
  _ilu_Assert(e != NIL, "ilu_ErrorLine(NIL)");
  return e->ilu_line;
}

static ilu_RaiseDebugHook the_rdh = NULLFN;

void 
ilu_SetRaiseDebugHook(ilu_RaiseDebugHook rdh)
{
  the_rdh = rdh;
  return;
}

void
_ilu_NoteRaise(int et, const char *file,
	       int line)
{
  ilu_RaiseDebugHook rdh = the_rdh;
  if (rdh != NULLFN)
    (*rdh) (et, file, line);
  return;
}

#undef ILU_M1NOR
#undef ILU_MINOR
#undef ILU_MINOR_LAST
#define ILU_M1NOR(n,c) c,
#define ILU_MINOR(n,c) c,
#define ILU_MINOR_LAST(n) 0

ILU_DEF_ERR (success, "(the error code that signals success)") {}
ILU_DEF_ERR (unknown, "(a standard CORBA system exception)") {}
ILU_DEF_ERRM(bad_param, "(a standard CORBA system exception)", BAD_PARAM) {}
ILU_DEF_ERR (no_memory, "(a standard CORBA system exception)") {}
ILU_DEF_ERRM(imp_limit, "(a standard CORBA system exception)", IMP_LIMIT) {}
ILU_DEF_ERRM(comm_failure, "(a standard CORBA system exception)", COMM_FAILURE) {}
ILU_DEF_ERRM(inv_objref, "(a standard CORBA system exception)", INV_OBJREF) {}
ILU_DEF_ERR (no_permission, "(a standard CORBA system exception)") {}
ILU_DEF_ERRM(internal, "(a standard CORBA system exception)", INTERNAL) {}
ILU_DEF_ERRM(marshal, "(a standard CORBA system exception)", MARSHAL) {}
ILU_DEF_ERR (initialize, "(a standard CORBA system exception)") {}
ILU_DEF_ERR (no_implement, "(a standard CORBA system exception)") {}
ILU_DEF_ERRM(bad_typecode, "(a standard CORBA system exception)", BAD_TYPECODE) {}
ILU_DEF_ERRM(bad_operation, "(a standard CORBA system exception)", BAD_OPERATION) {}
ILU_DEF_ERRM(no_resources, "(a standard CORBA system exception)", NO_RESOURCES) {}
ILU_DEF_ERR (no_response, "(a standard CORBA system exception)") {}
ILU_DEF_ERR (persist_store, "(a standard CORBA system exception)") {}
ILU_DEF_ERR (bad_inv_order, "(a standard CORBA system exception)") {}
ILU_DEF_ERRM(transient, "(a standard CORBA system exception)", TRANSIENT) {}
ILU_DEF_ERR (free_mem, "(a standard CORBA system exception)") {}
ILU_DEF_ERR (inv_ident, "(a standard CORBA system exception)") {}
ILU_DEF_ERR (inv_flag, "(a standard CORBA system exception)") {}
ILU_DEF_ERR (intf_repos, "(a standard CORBA system exception)") {}
ILU_DEF_ERR (bad_context, "(a standard CORBA system exception)") {}
ILU_DEF_ERR (obj_adapter, "(a standard CORBA system exception)") {}
ILU_DEF_ERR (data_conversion, "(a standard CORBA system exception)") {}
ILU_DEF_ERR (codeset_incompatible, "(a CORBA 2.1 standard system exception)") {}
ILU_DEF_ERR (barrier, "break in guaranteed serialization") {}
ILU_DEF_ERR (bad_locks, "caller violated locking precondition") {}
ILU_DEF_ERR (broken_locks, "locking meta-object broken") {}
ILU_DEF_ERR (interrupted, "caller asked for RPC interruption") {}
ILU_DEF_ERR (gss_security, "a standard GSS security error") {}

ILU_DEF_ERR(relocate, "new location for object's server") {
  ilu_free(e->rel_pinfo);
  ilu_free(e->rel_tinfo);
}

ILU_DEF_ERR(MaxCountExceeded, "An attempt was made to fill an array or structure beyond its fixed limit") {
  /* assume that all strings are constants */
}

ILU_DEF_ERR(ProtocolAlreadyRegistered, "Specified protocol was already in registry of protocols") {
  /* assume that all strings are constants */
}

ILU_DEF_ERR(TransportAlreadyRegistered, "Specified transport was already in registry of transports") {
  /* assume that all strings are constants */
}

ILU_DEF_ERR(BadProtocolInfo, "Bogus protocol info") {
  ilu_free(e->x);
}

ILU_DEF_ERR(NoObjectForSBH, "Can't determine object") {
  ilu_free(e->sbh);
}

ILU_DEF_ERR(GcRegFailed, "[Un]RegisterGCInterest failed") {
  if (e->sub) {
    ILU_HANDLED(*e->sub);
    ilu_free(e->sub);
  }
}

#define ILU_ERRLISTELT(id) ILU_QUADEF(id);
#if defined(WIN16)

    ILU_QUADEF(success);
    ILU_QUADEF(unknown);
    ILU_QUADEF(bad_param);
    ILU_QUADEF(no_memory);
    ILU_QUADEF(imp_limit);
    ILU_QUADEF(comm_failure);
    ILU_QUADEF(inv_objref);
    ILU_QUADEF(no_permission);
    ILU_QUADEF(internal);
    ILU_QUADEF(marshal);
    ILU_QUADEF(initialize);
    ILU_QUADEF(no_implement);
    ILU_QUADEF(bad_typecode);
    ILU_QUADEF(bad_operation);
    ILU_QUADEF(no_resources);
    ILU_QUADEF(no_response);
    ILU_QUADEF(persist_store);
    ILU_QUADEF(bad_inv_order);
    ILU_QUADEF(transient);
    ILU_QUADEF(free_mem);
    ILU_QUADEF(inv_ident);
    ILU_QUADEF(inv_flag);
    ILU_QUADEF(intf_repos);
    ILU_QUADEF(bad_context);
    ILU_QUADEF(obj_adapter);
    ILU_QUADEF(data_conversion);
    ILU_QUADEF(bad_locks);
    ILU_QUADEF(broken_locks);
    ILU_QUADEF(interrupted);
    ILU_QUADEF(gss_security);
    ILU_QUADEF(MaxCountExceeded);
    ILU_QUADEF(ProtocolAlreadyRegistered);
    ILU_QUADEF(TransportAlreadyRegistered);
    ILU_QUADEF(BadProtocolInfo);
    ILU_QUADEF(GcRegFailed);
    ILU_QUADEF(NoObjectForSBH);
    ILU_QUADEF(CantCondition);
#else

ILU_ERRLIST

#endif
#undef ILU_ERRLISTELT
