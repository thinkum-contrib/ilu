/*  -*- Mode: C; -*-
 *
 * This code contributed by Siemens Corporate Research, Inc.
 *
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

#include <string.h>
#include "ilu-scm-private.h"
#include "ilusrvr-scm.h"
#include <iludebug.h>		/* for ilu_DebugLevel and LSR_DEBUG */

/* portability hack since we don't have access to kernel headers */
#ifndef ANSI_STRERROR
#ifdef SUNOS
#define ANSI_STRERROR(errnum)	(sys_errlist[errnum])
#else
#define ANSI_STRERROR	strerror
#endif
#endif

#define ILUGUILE_SCHEME_Logging (ilu_DebugLevel & LSR_DEBUG)

#define ERRFMTSIZE 128

static const char *FmtError(char buf[ERRFMTSIZE], ilu_Error * err)
{
  int             mc, est;
  char            nbuf[24], *fmt;
  const char     *errname = ILU_ERR_NAME(*err);
  const char     *errfile = ilu_ErrorFile(err);
  static char    *fmt0 = "%s from line %d of %s";
  static char    *fmt1 = "%s (minor=%s) from line %d of %s";
  static char    *fmt2 = "interrupted (bitset=%s) from line %d of %s";
  static char    *fmt3 = "no_memory (nbytes=%s) from line %d of %s";
  ILU_ERR_SWITCH(*err) {
    ILU_SUCCESS_CASE return "SUCCESS";
    ILU_ERR_CASE(no_memory, x) {
      sprintf(nbuf, "%lu", x->nbytes);
      fmt = fmt3;
    }
    ILU_ERR_CASE(interrupted, x) {
      sprintf(nbuf, "0x%x", x->ilu_interruptSet);
      fmt = fmt2;
    }
    ILU_ERR_CASE(bad_param, x) fmt = fmt1, mc = x->minor;
    ILU_ERR_CASE(imp_limit, x) fmt = fmt1, mc = x->minor;
    ILU_ERR_CASE(comm_failure, x) fmt = fmt1, mc = x->minor;
    ILU_ERR_CASE(inv_objref, x) fmt = fmt1, mc = x->minor;
    ILU_ERR_CASE(internal, x) fmt = fmt1, mc = x->minor;
    ILU_ERR_CASE(marshal, x) fmt = fmt1, mc = x->minor;
    ILU_ERR_CASE(bad_typecode, x) fmt = fmt1, mc = x->minor;
    ILU_ERR_CASE(bad_operation, x) fmt = fmt1, mc = x->minor;
    ILU_ERR_CASE(no_resources, x) fmt = fmt1, mc = x->minor;
    ILU_ERR_ELSE    fmt = fmt0;
  } ILU_ERR_ENDSWITCH;
  if (fmt == fmt1)
    sprintf(nbuf, "%d", mc);
  est = (strlen(fmt) + strlen(errname) + 20 + strlen(errfile) +
	 ((fmt != fmt0) ? strlen(nbuf) : 0));
  if (est > ERRFMTSIZE)
    return errname;
  if (fmt == fmt0)
    sprintf(buf, fmt, errname, ilu_ErrorLine(err), errfile);
  else
    sprintf(buf, fmt, errname, nbuf, ilu_ErrorLine(err), errfile);
  return buf;
}

static ilu_boolean threaded    = ilu_FALSE;
static ilu_boolean threadedSet = ilu_FALSE;

static void (*Fork)(void (*proc)(void *arg), void *arg) = 0;

/*Main invariant holds */
void iluguile_server_monitor_outgoing_connection(void *rock)
{
  ilu_Connection  conn = (ilu_Connection) rock;
  ILU_ERRS((IoErrs)) lerr;
  (void) ilu_OutgoingConnectionThreadProc(conn, &lerr);
  ILU_MUST_BE_SUCCESS(lerr);
}

/* Main invariant holds */
void iluguile_server_pass_new_connections(void *rock)
{
  ilu_Connection  nu;
  ILU_ERRS((internal)) lerr;
  while (1) {
    nu = ilu_OtherNewConnection(&lerr);
    if (nu != NULL)
      (*Fork) (iluguile_server_monitor_outgoing_connection, nu);
    ILU_MUST_BE_SUCCESS(lerr);
  }
}

/*
 * This method is used when someone outside of this module needs to use
 * the Fork routine.
 * Since it's declared static, it isn't visible (ilu-scm.c needs this)
 */
void iluguile_server_monitor_conn(ilu_Connection conn)
{
    (*Fork) (iluguile_server_monitor_outgoing_connection, conn);
}

/* Returns TRUE iff connection closed. */
/*Main Invariant holds
  before: L2 disjoint {arg's callmu, iomu}
  after:  L2     >=   {conn's callmu, iomu} if result==ilu_good_request,
  after:  L2 disjoint {conn's callmu, iomu} if result!=ilu_good_request */
/*If !threaded: ReadServiceRequest registered for fd of conn */
static ilu_boolean FinalServiceRequest (ilu_private arg)
{
  ilu_Connection  conn = (ilu_Connection) arg;
  ilu_RcvReqStat  s;
  struct iluguile_SCMCall_s call;
  ilu_Class       ptype;
  ilu_Method      method;
  ilu_boolean	  initted;
  ilu_cardinal    serial_number;

  iluguile_server_disable_requests((iluguile_SCMCall) 0, conn);
  scm_call_call(&call)->ca_reqs_enabled = ilu_FALSE;
  s = iluguile_receive_request(conn, &call, &initted,&ptype,&method,&serial_number);

  if (s == ilu_RcvReqStat_request) {
    void (*stubproc)(ilu_Call);
    stubproc = ilu_GetMethodStubProc(method, iluguile_scheme_lang_idx());
    _ilu_Assert(stubproc != NULL, "ilusrvr-scm.c:FinalServiceRequest");
    (*stubproc) (scm_call_call(&call));
  }
  if (initted) {
    ilu_FinishCall(scm_call_call(&call), scm_call_err(&call));
    iluguile_prefer_success(scm_call_err(&call));
    ILU_HANDLED(*scm_call_err(&call));
  }
  if (s == ilu_RcvReqStat_quit) {
    ILU_ERRS((bad_param, broken_locks, bad_locks, internal)) lerr;
    if (!ilu_ConnectionServingP(conn)) {
      ilu_DoneServingConnection(conn, &lerr);
      iluguile_prefer_success(&lerr);
      ILU_HANDLED(lerr);
    }
    return ilu_TRUE;
  }
  if (!scm_call_call(&call)->ca_reqs_enabled) {
    iluguile_server_enable_requests(&call, conn);
  }
  return ilu_FALSE;
}

/* This routine handles the dispatching of an incoming request */

static void ReadServiceRequest (void *arg)
{
  (void) FinalServiceRequest(arg);
}

/*L1, L2, Main unconstrained*/
ilu_boolean iluguile_server_enable_requests(iluguile_SCMCall call, ilu_Connection conn)
{
  ilu_Server s;
  ilu_string sid;
  ilu_Error lerr;
  ilu_boolean ans;
  ilu_Error *perr;

  if (threaded)
    return ilu_TRUE;

  s = ilu_ServerOfConnection(conn);
  sid = ilu_IDOfServer(s);

  if (call == ILU_NIL)
    perr = &lerr;
  else
    perr = scm_call_err(call);
  ans = ilu_SetConnectionInputHandler(conn, ReadServiceRequest, conn, perr);
  iluguile_prefer_success(perr);
  if (!ans)
    fprintf(stderr, "ilusrvr-scm.c:  Can't register ReadServiceRequest on conn %p server %s!\n", conn, sid);
  else if (call != ILU_NIL)
    scm_call_call(call)->ca_reqs_enabled = ilu_TRUE;
  return ans;
}

/*L1, L2, Main unconstrained*/
ilu_boolean iluguile_server_disable_requests(iluguile_SCMCall call, ilu_Connection conn)
{
  ilu_Server   s;
  ilu_string   sid;
  ilu_Error    lerr;
  ilu_boolean  ans;
  ilu_Error    *perr;

  if (threaded)
    return ilu_TRUE;

  s = ilu_ServerOfConnection(conn);
  sid = ilu_IDOfServer(s);

  if (call == ILU_NIL)
    perr = &lerr;
  else
    perr = scm_call_err(call);
  ans = ilu_SetConnectionInputHandler(conn, 0, NULL, perr);
  iluguile_prefer_success(perr);
  if (!ans)
    fprintf(stderr, "ilusrvr-scm.c:  bug detected when unregistering input source for conn %p server %s!\n", conn, sid);
  else if (call != ILU_NIL)
    scm_call_call(call)->ca_reqs_enabled = ilu_FALSE;
  return ans;
}



/* When a request for a new connection to the server is received,
   we have to handle it. */
static void ReadConnectionRequest (void *arg)
{
  ilu_Port        p = (ilu_Port) arg;
  ilu_Error       lerr;
  ilu_boolean     closed;
  ilu_Connection  conn;
  char            ef[ERRFMTSIZE];

  conn = ilu_HandleNewConnection(p, &closed, &lerr);
  if (closed) {
    ilu_DebugPrintf("iluServer::ReadConnectionRequest(%p) port closed.\n",
		    p);
    goto abandon;
  }
  if (conn == NULL) {
    ILU_ERR_SWITCH(lerr) {
      ILU_SUCCESS_CASE {
	if (ILUGUILE_SCHEME_Logging)
	  ilu_DebugPrintf("Spurrious input call on port %p.\n", p);
	goto ok;
      }
      ILU_ERR_CASE(no_resources, x)
	if (ILUGUILE_SCHEME_Logging)
	ilu_DebugPrintf("Connection request on port %p"
			" ran over FD limit --- port abandoned.\n",
			p);
      ILU_ERR_ELSE
	if (ILUGUILE_SCHEME_Logging)
	ilu_DebugPrintf("Got error %s"
			" trying to accept connection on port %p;"
			" abandoning port.\n",
			FmtError(ef, &lerr), p);
    } ILU_ERR_ENDSWITCH;
    ILU_HANDLED(lerr);
    goto abandon;
  }
  ilu_SetConnectionInputHandler(conn, ReadServiceRequest, conn, &lerr);
  ILU_MUST_BE_SUCCESS(lerr);
ok:
  return;
abandon:
  ilu_DoneWithPort(p, &lerr);
  iluguile_prefer_success(&lerr);
}


/* Main invariant holds */
static void RunConnection (void *arg)
{
  ilu_Connection conn   = (ilu_Connection) arg;

  for (;;) {
    if (!ilu_BlockingWaitForInputOnConnection(conn, (ilu_FineTime *) 0)) {
      ILU_ERRS((bad_param, broken_locks, bad_locks, internal)) lerr;
      ilu_DoneServingConnection(conn, &lerr);
      iluguile_prefer_success(&lerr);
      ILU_HANDLED(lerr);
      break;
    }
    if (FinalServiceRequest(conn))
      break;
  }
}

/* Main invariant holds */
static void ReadConnectionRequests (void *arg)
{
  ilu_Port        p = (ilu_Port) arg;
  ilu_boolean     closed;
  ILU_ERRS((IoErrs, bad_locks, no_resources)) lerr;
  ilu_Connection  conn;
  char            ef[ERRFMTSIZE];
  while (1) {
    if (!ilu_WaitForPortConnectionRequest(p, &lerr)) {
      if (ILUGUILE_SCHEME_Logging)
	ilu_DebugPrintf(
	  "No longer waiting for connection requests on port %p,\n"
	   "because ilu_WaitForPortConnectionRequest raised %s.\n",
			p, FmtError(ef, &lerr));
      ILU_HANDLED(lerr);
      goto dun;
    }
    conn = ilu_HandleNewConnection(p, &closed, &lerr);
    if (closed) {
      if (ILUGUILE_SCHEME_Logging)
	ilu_DebugPrintf(
	    "No longer waiting for connection requests on port %p,"
			"because it's closed.\n",
			p);
      break;
    }
    if (conn == ILU_NIL) {
      ILU_ERR_SWITCH(lerr) {
	ILU_SUCCESS_CASE {
	  if (ILUGUILE_SCHEME_Logging)
	    ilu_DebugPrintf("Spurrious input call on port %p.\n",
			    p);
	  goto nechst;
	}
	ILU_ERR_CASE(no_resources, x)
	  if (ILUGUILE_SCHEME_Logging)
	  ilu_DebugPrintf("Connection request on port %p"
			" ran over FD limit --- port abandoned.\n",
			  p);
	ILU_ERR_ELSE
	  if (ILUGUILE_SCHEME_Logging)
	  ilu_DebugPrintf("Got error %s"
			  " trying to accept connection on port %p;"
			  " abandoning port.\n",
			  FmtError(ef, &lerr), p);
      } ILU_ERR_ENDSWITCH;
      ILU_HANDLED(lerr);
      goto dun;
    } else {
      /* fork thread to run new connection */
      Fork(RunConnection, conn);
    }
nechst:0;
  }
dun:
  if (!ilu_DoneWithPort(p, &lerr))
    iluguile_prefer_success(&lerr);
}

static ilu_string inmemTinfo[] = { "inmem_", 0 };

ilu_boolean
iluguile_server_set_fork(void (*fork)(void (*proc)(void *arg), void *arg))
{
  ILU_ERRS((internal)) lerr;

  if (threadedSet)
    return ilu_FALSE;
  threadedSet = ilu_TRUE;
  Fork = fork;
  threaded = ilu_TRUE;
  (void) iluguile__scheme_lang_idx();
  (void) iluguile__get_default_server();
  (*fork)(iluguile_server_pass_new_connections, NULL);
  (void) ilu_NewConnectionGetterForked(&lerr);
  ILU_MUST_BE_SUCCESS(lerr);
  return (ilu_TRUE);
}

ilu_boolean iluguile_server_add_port(ilu_Server ks, ilu_string protocolType,
				     ilu_TransportInfo transportType,
				     ilu_boolean bdefault)
{
  ilu_Port nu = iluguile_create_port(ks, protocolType, transportType);
  ilu_Error lerr;

  if (nu == NULL) {
    fprintf(stderr, "iluguile_server_add_port:  Error:  Couldn't open port.\n");
    return ilu_FALSE;
  }

  if (bdefault)
    ilu_SetServerDefaultPort(ks, nu);

  if (threaded && transportType != inmemTinfo) {
    Fork(ReadConnectionRequests, nu);
  } else {
    ilu_SetConnectionRequestHandler(nu, ReadConnectionRequest, nu, &lerr);
    ILU_MUST_BE_SUCCESS(lerr);
  }
  return ilu_TRUE;
}

SCM iluguile_server__add_port(SCM _ks, SCM _protocolType,
			 SCM _transportType, SCM _bdefault)
{
  ilu_Server ks = iluguile_scheme_to_ptr(ilu_Server, _ks);
  char* protocolType = iluguile_scheme_to_value(tmp_string, _protocolType);
  char** transportType = iluguile_scheme_to_vector0(string, _transportType);
  ilu_boolean bdefault = iluguile_scheme_to_value(boolean, _bdefault);
  ilu_boolean b = iluguile_server_add_port(ks,protocolType,transportType,bdefault);
  iluguile_free_string_vector(transportType, -1);
  return iluguile_value_to_scheme(boolean, b);
}

SCM iluguile_server__create(SCM _serviceID, SCM _objtab)
{
  char* sid = iluguile_scheme_to_value(string, _serviceID);
  ilu_ObjectTable kot  = iluguile_scheme_to_ptr(ilu_ObjectTable, _objtab);
  SCM lss;
  ilu_Server ks;
  ilu_Error lerr;

  threadedSet = ilu_TRUE;
  if ((sid == NULL) || (strlen(sid) == 0))
    sid = ilu_InventID();
  ks = ilu_CreateTrueServer(sid, kot, iluguile_scheme_lang_idx(), &lerr);
  if (ILU_ERROK(lerr)) {
    /* XXX -- this call to ilu_SetLSS should pass a pointer to the
       LSS; instead, this will cause us to leak true servers */
    ilu_SetLSS(ks, (ilu_refany) 1, iluguile_scheme_lang_idx(), &lerr);
    ILU_HANDLED(lerr);
    lss = iluguile_ptr_to_scheme(ilu_Server, ks);
    ilu_ExitServer(ks, ilu_rootClass);
    return lss;
  } else {
    ILU_HANDLED(lerr);
    return SCM_UNSPECIFIED;
  }
}

SCM iluguile_server__id(SCM _ks)
{
  ilu_Server ks = iluguile_scheme_to_ptr(ilu_Server, _ks);
  return iluguile_value_to_scheme(tmp_string, ilu_IDOfServer(ks));
}
