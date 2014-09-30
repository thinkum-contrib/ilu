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
/* $Id: ilusrvr.cpp,v 1.64 1999/08/03 01:55:45 janssen Exp $ */
/* Last edited by Mike Spreitzer December 18, 1996 10:34 am PST */

#include <stdio.h>	/* I/O defs (including popen and pclose) */

#if ((defined(WIN32) || defined(WIN16)) && defined(_WINIO))
#include <winiodef.h>
#endif /* ((defined(WIN32) || defined(WIN16)) && defined(_WINIO)) */

#include <sys/types.h>
#include <errno.h>
#if !(defined(WIN32) || defined(WIN16))
// dll no sys/errno in VC++2
#include <sys/errno.h>
#endif /* not WIN32 */

#include "ilu.hh"

extern "C" {
#include <iludebug.h>		/* ILU_NOTE */
}

/* portability hack since we don't have access to kernel headers */
#ifndef ANSI_STRERROR
#ifdef SUNOS
#define ANSI_STRERROR(errnum)	(sys_errlist[errnum])
#else
#define ANSI_STRERROR	strerror
#endif
#endif

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

iluObjectTable::~iluObjectTable()
{
  fprintf(stderr, "default destructor for iluObjectTable was called!\n");
}

/* First, a simple input dispatch loop */

static iluMainLoop *theML = NULL;

static void CallRun(int *stop)
{
  theML->Run(stop);
  return;
}

static void CallExit (int *stop)
{
  theML->Exit(stop);
}

static ilu_boolean CallRegisterInput(int fd,
				     ilu_IOHandler proc,
				     ilu_private rock)
{
  return ((ilu_boolean) theML->RegisterInputHandler(fd, proc, rock));
}

static ilu_boolean CallUnregisterInput(int fd,
				       ilu_IOHandler *proc,
				       ilu_private *rock)
{
  theML->UnregisterInputHandler(fd, proc, rock);
  return ilu_FALSE;
}

static ilu_boolean CallRegisterOutput(int fd,
				      ilu_IOHandler proc,
				      ilu_private rock)
{
  return ((ilu_boolean) theML->RegisterOutputHandler(fd, proc, rock));
}

static ilu_boolean CallUnregisterOutput(int fd,
					ilu_IOHandler *proc,
					ilu_private *rock)
{
  theML->UnregisterOutputHandler(fd, proc, rock);
  return ilu_FALSE;
}

static ilu_Alarm CallCreateAlarm (void)
{
  return (theML->CreateAlarm());
}

static void CallSetAlarm (ilu_Alarm alarm, ilu_FineTime t, void (*proc)(void * rock), void * rock)
{
  theML->SetAlarm (alarm, t, proc, rock);
}

static void CallUnsetAlarm (ilu_Alarm alarm)
{
  theML->ClearAlarm (alarm);
}

static ilu_MainLoop kml = {
  CallRun, CallExit,
  CallRegisterInput, CallUnregisterInput,
  CallRegisterOutput, CallUnregisterOutput,
  CallCreateAlarm, CallSetAlarm, CallUnsetAlarm };

void iluServer::iluSetMainLoop(iluMainLoop *ml)
{
  theML = ml;
  ilu_SetMainLoop(&kml);
  return;
}

static ilu_Boolean threaded    = ilu_FALSE;
static ilu_Boolean threadedSet = ilu_FALSE;
static ilu_boolean threadedOther = ilu_FALSE;
/* true if another LSR has registered threads, instead of C++ runtime */

static void (*Fork)(void (*proc)(void *arg), void *arg) = 0;

/*Main invariant holds */
void iluServer::MonitorOutgoingConnection(void *rock)
{
  ilu_Connection  conn = (ilu_Connection) rock;
  ILU_ERRS((IoErrs)) lerr;
  (void) ilu_OutgoingConnectionThreadProc(conn, &lerr);
  ILU_MUST_BE_SUCCESS(lerr);
}

/* Main invariant holds */
void iluServer::PassNewConnections(void *rock)
{
  ilu_Connection  nu;
  ILU_ERRS((internal)) lerr;
  while (1) {
    nu = ilu_OtherNewConnection(&lerr);
    if (nu != NULL)
      (*Fork) (MonitorOutgoingConnection, nu);
    ILU_MUST_BE_SUCCESS(lerr);
  }
}

// This method is used when someone outside of this module needs to use the Fork routine.
// Since it's declared static, it isn't visible (ilu.cpp needs this)

void iluServer::MonitorConn(ilu_Connection conn)
{
    (*Fork) (MonitorOutgoingConnection, conn);
}

ilu_Boolean iluServer::SetFork(void (*fork)(void (*proc)(void *arg), void *arg))
{
  ILU_ERRS((internal)) lerr;

  if (threadedSet)
    return ilu_FALSE;
  threadedSet = ilu_TRUE;
  Fork = fork;
  threaded = ilu_TRUE;
  (void) ilu::CppLangIdx();
  (void) ilu::GetDefaultServer();
  (*fork)(PassNewConnections, NULL);
  (void) ilu_NewConnectionGetterForked(&lerr);
  ILU_MUST_BE_SUCCESS(lerr);
  return (ilu_TRUE);
}

/* Main Invariant holds for invocations */
static ilucpp_ForkProc *theForkProc = 0;

/* Main Invariant holds */
static void     ErrlessFork(void (*proc) (void *arg), void *arg)
{
  ILU_ERRS((no_memory, no_resources, internal)) lerr;
  if (theForkProc == ILU_NIL)
    fprintf(stderr,
       "ilusrvr.cpp::ErrlessFork invoked with theForkProc=NIL!\n");
  else {
    (*theForkProc) (proc, arg, &lerr);
    ILU_MUST_BE_SUCCESS(lerr);
  }
}

/* Main Invariant holds */
ilu_Boolean iluServer::StartThreading(ilu_ThreadSetupProc *s,
                                       ilucpp_ForkProc *f)
{
  ILU_ERRS((bad_param, no_memory, no_resources, internal)) lerr;
  if (!(*s) (&lerr)) {
    ilu_DebugPrintf("iluServer::StartThreading(%p, %p) got"
		    " kernel error %s (raised at line %d of %s)"
		    " during setup!\n",
		    s, f, ILU_ERR_NAME(lerr), ilu_ErrorLine(&lerr),
		    ilu_ErrorFile(&lerr));
    ILU_HANDLED(lerr);
    return ilu_FALSE;
  }
  theForkProc = f;
  iluServer::SetFork(ErrlessFork);
  return ilu_TRUE;
}
  

/* Returns TRUE iff connection closed. */
/*Main Invariant holds
  before: L2 disjoint {arg's callmu, iomu}
  after:  L2     >=   {conn's callmu, iomu} if result==ilu_good_request,
  after:  L2 disjoint {conn's callmu, iomu} if result!=ilu_good_request */
/*If !threaded: ReadServiceRequest registered for fd of conn */
static ilu_boolean FinalServiceRequest (ilu_Connection conn)
{
  ilu_RcvReqStat  s;
  iluCall_s	  call;
  ilu_Class       ptype;
  ilu_Method      method;
  ilu_Boolean	  initted;
  ilu_Cardinal    serial_number;

  iluServer::DisableRequests((iluCall) 0, conn);
  call.call.ca_reqs_enabled = ilu_FALSE;
  s = ilu::ReceiveRequest(conn, &call, &initted, &ptype, &method,
                          &serial_number);
  if (s == ilu_RcvReqStat_request) {
    ilu_StubProc      stubproc = ilu_GetMethodStubProc(method, ilu::CppLangIdx());
    _ilu_Assert(stubproc != ((ilu_StubProc) 0), "ilusrvr.cpp:FinalServiceRequest");
    (*(void (*) (ilu_Call)) stubproc) (&call.call);
  }
  if (initted) {
    ilu_FinishCall(&call.call, &call.err);
    ilu_PreferSuccess(&call.err);
    ILU_HANDLED(call.err);
  }
  if (s == ilu_RcvReqStat_quit) {
    ILU_ERRS((bad_param, broken_locks, bad_locks, internal)) lerr;
    if (!ilu_ConnectionServingP(conn)) {
      ilu::DoneServingConnection(conn, &lerr);
      ilu_PreferSuccess(&lerr);
      ILU_HANDLED(lerr);
    }
    return ilu_TRUE;
  }
  if (!call.call.ca_reqs_enabled) {
    iluServer::EnableRequests(&call, conn);
  }
  return ilu_FALSE;
}

/* This routine handles the dispatching of an incoming request */

static void ReadServiceRequest (void *arg)
{
  (void) FinalServiceRequest((ilu_Connection) arg);
}

/* When a request for a new connection to the server is received,
   we have to handle it. */

/*ARGSUSED*/
void ReadConnectionRequest (void *arg)
{
  ilu_Port        p = (ilu_Port) arg;
  ilu_boolean     closed;
  ilu_Error       lerr;
  ilu_Connection  conn;
  char            ef[ERRFMTSIZE];

  conn = ilu::HandleNewConnection(p, &closed, &lerr);
  if (closed) {
    ilu_DebugPrintf("iluServer::ReadConnectionRequest(%p) port closed.\n",
		    p);
    goto abandon;
  }
  if (conn == NULL) {
    ILU_ERR_SWITCH(lerr) {
      ILU_SUCCESS_CASE {
	ILU_NOTE(LSR_DEBUG, ("Spurrious input call on port %p.\n", p));
	goto ok;
      }
      ILU_ERR_CASE(no_resources, x)
	ILU_NOTE(LSR_DEBUG, ("Connection request on port %p"
			" ran over FD limit --- port abandoned.\n",
			p));
      ILU_ERR_ELSE
	ILU_NOTE(LSR_DEBUG, ("Got error %s"
			" trying to accept connection on port %p;"
			" abandoning port.\n",
			FmtError(ef, &lerr), p));
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
  ilu_PreferSuccess(&lerr);
}


/* Main invariant holds */
static void RunConnection (void *arg)
{
  ilu_Connection conn   = (ilu_Connection) arg;

  for (;;) {
    if (!ilu_BlockingWaitForInputOnConnection(conn, (ilu_FineTime *) 0)) {
      ILU_ERRS((bad_param, broken_locks, bad_locks, internal)) lerr;
      ILU_NOTE(LSR_DEBUG, ("ILU-C++ done serving connection %p.\n",
			conn));
      ilu_DoneServingConnection(conn, &lerr);
      ilu_PreferSuccess(&lerr);
      ILU_HANDLED(lerr);
      break;
    }
    if (FinalServiceRequest(conn))
      break;
  }
  return;
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
    if (!ilu::WaitForPortConnectionRequest(p, &lerr)) {
      ILU_NOTE(LSR_DEBUG, (
	  "No longer waiting for connection requests on port %p,\n"
	   "because ilu_WaitForPortConnectionRequest raised %s.\n",
			p, FmtError(ef, &lerr)));
      ILU_HANDLED(lerr);
      goto dun;
    }
    conn = ilu::HandleNewConnection(p, &closed, &lerr);
    if (closed) {
      ILU_NOTE(LSR_DEBUG, (
	    "No longer waiting for connection requests on port %p,"
			"because it's closed.\n",
			p));
      break;
    }
    if (conn == ILU_NIL) {
      ILU_ERR_SWITCH(lerr) {
	ILU_SUCCESS_CASE {
	  ILU_NOTE(LSR_DEBUG, ("Spurrious input call on port %p.\n",
			    p));
	  goto nechst;
	}
	ILU_ERR_CASE(no_resources, x)
	  ILU_NOTE(LSR_DEBUG, ("Connection request on port %p"
			" ran over FD limit --- port abandoned.\n",
			  p));
	ILU_ERR_ELSE
	  ILU_NOTE(LSR_DEBUG, ("Got error %s"
			  " trying to accept connection on port %p;"
			  " abandoning port.\n",
			  FmtError(ef, &lerr), p));
      } ILU_ERR_ENDSWITCH;
      ILU_HANDLED(lerr);
      goto dun;
    } else {
      /* fork thread to run new connection */
      Fork(RunConnection, conn);
    }
nechst:
    ;
  }
dun:
  if (!ilu_DoneWithPort(p, &lerr))
    ilu_PreferSuccess(&lerr);
}

/* Inside(obj->server, obj->class) */
static ilu_KernelObject Call_object_of_ih(ilu_ObjectTable self,
					  ilu_string ih)
{
  iluObjectTable *objtab = (iluObjectTable *) self->ot_rock;
  iluObject * obj = (iluObject *) objtab->ObjectOfIH(ih);
  if (obj == NULL)
    return (NULL);
  else
    return obj->ILUEnsureKernelObject();
}

static void Call_free_self(ilu_ObjectTable self)
{
  iluObjectTable *objtab = (iluObjectTable *) self->ot_rock;
  delete objtab;
  free((char *) self);
  return;
}

iluServer::iluServer (	char *serviceID /* = NULL */,
			iluObjectTable *objtab /* = NULL */)
{
  ilu_ObjectTable kot;
  ilu_Error       an_error;
  threadedSet = ilu_TRUE;
  if (objtab == NULL)
    kot = NULL;
  else {
    kot = (ilu_ObjectTable) ilu_must_malloc(sizeof(ilu_ObjectTable_s));
    kot->ot_object_of_ih = Call_object_of_ih;
    kot->ot_free_self = Call_free_self;
    kot->ot_rock = (void *) objtab;
  }
  if ((serviceID == NULL) || (strlen(serviceID) == 0))
    serviceID = ilu_InventID();

  ks = ilu_CreateTrueServer(serviceID, kot, ilu::CppLangIdx(), &an_error);

  if (ILU_ERRNOK(an_error)) {
    ILU_ERRPRINTF("iluServer::iluServer Error %s  calling ilu_CreateTrueServer, file %s, line %i\n", 
		  ILU_ERR_NAME(an_error), __FILE__, __LINE__);
    ILU_HANDLED(an_error); 
    ILU_CLER(an_error);	
  }

  // set the language specific server of the kernel server to this iluServer
  ilu_SetLSS(ks, this, ilu::CppLangIdx(), &an_error);

  if (ILU_ERRNOK(an_error)) {	
    ILU_ERRPRINTF("iluServer::iluServer Error %s calling ilu_SetLSS, file %s, line %i\n", 
		  ILU_ERR_NAME(an_error), __FILE__, __LINE__);
    ILU_HANDLED(an_error); 
    ILU_CLER(an_error);
  }

  ilu_ExitServer(ks, ilu_rootClass);

  (void) AddPort(ilu_DefaultProtocolInfo(), ilu_LocalTransportInfo(), 0);
  /* What to do if this fails?? */
  return;
}



iluServer::~iluServer () {

  // XXX note: really should call ilu_BankAndScanServer here to ensure all is
  // properly cleaned up, but we'll assume that the application has already deleted
  // any objects served by this server 

  // unlink this iluServer from the kernel server
  ilu_Error an_error;

  ilu_EnterServer(ks, ilu_rootClass);

  ilu_SetLSS(ks, NULL,  ilu::CppLangIdx(), &an_error);

  if (ILU_ERRNOK(an_error)) {	
    ilu_ExitServer(ks, ilu_rootClass);
    ILU_ERRPRINTF("iluServer::~iluServer Error %s calling ilu_SetLSS, file %s, line %i\n", 
		  ILU_ERR_NAME(an_error), __FILE__, __LINE__);
    ILU_HANDLED(an_error); 
    ILU_CLER(an_error);
  }
}

ilu_Boolean iluServer::AddPort(	char *protocolType,
				ilu_TransportInfo transportType,
				ilu_Boolean be_default)
{
  ilu_Error       lerr;
  ilu_Port        nu;
  nu = ilu::CreatePort(ks, protocolType, transportType);
  if (nu == NULL) {
    fprintf(stderr, "iluServer::AddPort:  Error:  Couldn't open port.\n");
    return (ilu_FALSE);
  }
  if (be_default) ilu::SetServerDefaultPort(ks, nu);
  if (threaded && (transportType != ilu_LocalTransportInfo())) {
    Fork(ReadConnectionRequests, nu);
  } else {
    ilu_SetConnectionRequestHandler(nu, ReadConnectionRequest, nu, &lerr);
    ILU_MUST_BE_SUCCESS(lerr);
  }
  return (ilu_TRUE);
}

ilu_Boolean iluServer::FullAddPort (char *protocolType,
				    ilu_TransportInfo transportType,
				    ilu_Boolean be_default,
				    ilu_Boolean be_private)
{
  ilu_Error       lerr;
  ilu_Port        nu;
  nu = ilu::FullCreatePort(ks, protocolType, transportType, be_private);
  if (nu == NULL) {
    fprintf(stderr, "iluServer::AddPort:  Error:  Couldn't open port.\n");
    return (ilu_FALSE);
  }
  if (be_default) ilu::SetServerDefaultPort(ks, nu);
  if (threaded && (transportType != ilu_LocalTransportInfo())) {
    Fork(ReadConnectionRequests, nu);
  } else {
    ilu_SetConnectionRequestHandler(nu, ReadConnectionRequest, nu, &lerr);
    ILU_MUST_BE_SUCCESS(lerr);
  }
  return (ilu_TRUE);
}

ilu_Boolean iluServer::AddCInfo (ilu_ProtocolInfo pinfo, ilu_TransportInfo tinfo)
{
  ilu_Error lerr;
  ilu_AddCInfo (ks, pinfo, tinfo, &lerr);
  if (ILU_ERRNOK(lerr)) {
    ILU_ERRPRINTF("iluServer::~iluServer Error %s calling ilu_SetLSS, file %s, line %i\n", 
		  ILU_ERR_NAME(lerr), __FILE__, __LINE__);
    ILU_HANDLED(lerr);
    return 0;
  } else {
    return 1;
  }  
}

ilu_Boolean iluServer::NativeCInfo (ilu_ProtocolInfo *pinfo, ilu_TransportInfo *tinfo, ilu_Boolean be_public)
{
  ilu_Error lerr;
  ilu_boolean status;

  ilu_EnterServerMutex(ks, ilu_FALSE, &lerr);
  if (ILU_ERRNOK(lerr)) {
    ILU_ERRPRINTF("iluServer::NativeCInfo: Error %s entering server mutex, file %s, line %i\n", 
		  ILU_ERR_NAME(lerr), __FILE__, __LINE__);
    ILU_HANDLED(lerr);
    return 0;
  }
  status = ilu_ServerCInfo (ks, (be_public ? ilu_TRUE : ilu_FALSE), pinfo, tinfo, &lerr);
  if (ILU_ERRNOK(lerr)) {
    ILU_ERRPRINTF("iluServer::NativeCInfo: Error %s calling ilu_ServerCInfo, file %s, line %i\n", 
		  ILU_ERR_NAME(lerr), __FILE__, __LINE__);
    ILU_HANDLED(lerr);
  }
  ilu_ExitServerMutex(ks, ilu_FALSE, &lerr);
  ILU_HANDLED(lerr);
  return (status ? 1 : 0);
}

void iluServer::Stoppable_Run(int *stop)
{
  ilu_RunMainLoop(stop);
}

void iluServer::Run()
{
  int stop = 0;

  threadedSet = ilu_TRUE;
  ilu_RunMainLoop(&stop);
}

ilu_Server iluServer::KernelServer()
{
  return (this->ks);
}


/*L1, L2, Main unconstrained*/
ilu_Boolean iluServer::EnableRequests(iluCall call, ilu_Connection conn)
{
  if (threaded)
    return ilu_TRUE;

  ilu_Server      s = ilu_ServerOfConnection(conn);
  ilu_string      sid = ilu_IDOfServer(s);
  ilu_Error       lerr;
  ilu_kernelBoolean ans;
  ilu_Error	*perr;

  if (call == ILU_NIL)
    perr = &lerr;
  else
    perr = &call->err;
  ans = ilu_SetConnectionInputHandler(conn, ReadServiceRequest, conn, perr);
  ilu_PreferSuccess(perr);
  if (!ans)
    fprintf(stderr,
	    "iluServer.cc:  Can't register ReadServiceRequest on conn %p server %s!\n",
	    conn, sid);
  else if (call != ILU_NIL)
    call->call.ca_reqs_enabled = ilu_TRUE;
  return ans;
}

/*L1, L2, Main unconstrained*/
ilu_Boolean iluServer::DisableRequests(iluCall call, ilu_Connection conn)
{
  if (threaded)
    return ilu_TRUE;

  ilu_Server      s = ilu_ServerOfConnection(conn);
  ilu_string      sid = ilu_IDOfServer(s);
  ilu_Error       lerr;
  ilu_kernelBoolean ans;
  ilu_Error	*perr;

  if (call == ILU_NIL)
    perr = &lerr;
  else
    perr = &call->err;
  ans = ilu_SetConnectionInputHandler(conn, 0, NULL, perr);
  ilu_PreferSuccess(perr);
  if (!ans)
    fprintf(stderr, "iluServer.cc:  bug detected when unregistering input source for conn %p server %s!\n", conn, sid);
  else if (call != ILU_NIL)
    call->call.ca_reqs_enabled = ilu_FALSE;
  return ans;
}


/* ********************************************************************** */
/* Call this to extend the dispatch in the  main loop implemented by Run. */

ilu_Boolean iluServer::RegisterInputHandler (int fd,
					     void (*handlerProc)(int,void *),
					     void *handlerArg) {

  return (ilu_RegisterInputSource (fd, handlerProc, handlerArg));
}


/* ***************************************************************************** */
/* Call this to cancel an extension installed by a call on RegisterInputHandler. */

ilu_Boolean iluServer::UnregisterInputHandler (int fd) {

  return (ilu_UnregisterInputSource (fd));
}

void iluServer::InitializeThreading (void) {

  if (!threadedSet && (threaded = ilu_KernelThreaded())) {
    theForkProc = ilu_Fork;
    Fork = ErrlessFork;
    threadedSet = ilu_TRUE;
    threadedOther = ilu_TRUE;
  }
}
