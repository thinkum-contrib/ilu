/** $Id: call.c,v 1.300 1999/08/11 20:39:33 spreitze Exp $
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
/* Last edited by Mike Spreitzer October 13, 1998 10:29 pm PDT */

#define _POSIX_SOURCE

#include <stddef.h>

#include "iluntrnl.h"
#include "iluprotocol.h"
#include "ilutransport.h"
#include "connect.h"
#include "port.h"
#include "call.h"
#include "object.h"
#include "method.h"
#include "server.h"
#include "type.h"

#define B2S(b)	((b)?"T":"F")

#if 1

#define ILU_WaitRem(call)					\
  (call->ca_disownWait						\
   ? (call->ca_connection && call->ca_connection->co_waiting	\
      && call->ca_connection->co_waiting != call)		\
   : ((call->ca_ms==ilu_cmsHi && call->ca_msInput && !call->ca_dontWait)\
      == (call->ca_connection					\
	  && call->ca_connection->co_waiting == call)))

#else

#define ILU_WaitRem(call) \
  ((call->ca_connection && call->ca_connection->co_waiting==call) \
   == \
   (call->ca_disownWait || \
    (call->ca_ms==ilu_cmsHi && call->ca_msInput && !call->ca_dontWait)))

#endif

/*L1, L2 unconstrained*/

const char     *const ilu_PENames[ilu_ProtocolException_Not + 1] = {
  "Success",
  "NoSuchClassAtServer",
  "ClassVersionMismatch",
  "NoSuchMethodOnClass",
  "GarbageArguments",
  "Unknown",
  "LostConnection",
  "RequestRejected",
  "RequestTimeout",
  "Not"
};

const char     *ilu_CompletionNames[3] = {"yes", "no", "maybe"};

ilu_cardinal _ilu_SafeStrlen(ilu_string s)
{
  if (s == NIL)
    return 0;
  else
    return (strlen(s));
}

ilu_Class ilu_IntroTypeOfCall (ilu_Call call)
{
  if (call == NIL)
    return (NIL);
  else
    return (call->ca_intro_type);
}

ilu_Connection ilu_ConnectionOfCall (ilu_Call call)
{
  if (call == NIL)
    return (NIL);
  else
    return (call->ca_connection);
}

ilu_boolean ilu_CallNeedsSizing (ilu_Call call)
{
  if (call == NIL)
    return (ilu_FALSE);
  else
    return (protocol_needs_sizing(connection_protocol(call_connection(call))));
}

ilu_Method ilu_MethodOfCall (ilu_Call call)
{
  if (call == NIL)
    return (NIL);
  else
    return (call->ca_method);
}

static void
BuildCall(ilu_Call call, ilu_Connection connection, ilu_Server server,
	  ilu_cardinal serialNumber, ilu_boolean incoming)
{
  static ilu_Call_s nocall = {0};
  *call = nocall;
  call->ca_SN = serialNumber;
  call->ca_server = server;
  call->ca_connection = connection;
  call->ca_pl = NIL;
  call->ca_si = NIL;
  call->ca_incoming = incoming;
  call->ca_ios = ilu_ciosNone;
  call->ca_ms = ilu_cmsNo;
  call->ca_pe = ilu_ProtocolException_Success;
  call->ca_tryIndex = 0;
  return;
}

/*
 * Returns ilu_TRUE iff connection should continue to be monitored.
 * Returns ilu_FALSE if (but not only if) raising an ilu_Error.
 * Caller pins (conn) into existence.
 */
/*L1 = {cmu, server}; Main Remnant; L2 >= {conn's callmu, iomu, waitmu}*/
static          ilu_boolean
ProcessExtraInput(ilu_Connection conn, ilu_Call dummy,
		  ILU_ERRS((bad_locks, IoErrs)) * err)
{
  ilu_Server      server = connection_server(conn);
  ilu_Protocol    proto = connection_protocol(conn);
  ilu_ReadHeaderResultCode rhrc;
  ilu_PacketType  packetType;
  ilu_cardinal    packetSN;
  ilu_ConnShutdownReason	eof_reason;
  ilu_cardinal	  eof_last_sn;
  if (!_ilu_ExitServerMutex(server, ilu_TRUE, err))
    return ilu_FALSE;
  if (!ilu_ExitMutex(ilu_cmu, ilu_TRUE, err))
    return ilu_FALSE;
  /* Main Invariant; Call-IHi(dummy) */
  rhrc = protocol_read_header(proto, dummy, &packetType, &packetSN,
			      &eof_reason, &eof_last_sn, err);
  switch (rhrc) {
  case ilu_rhrc_ok:
    /*
     * We know no calls are in progess, so nobody cares about this
     * reply, so flush it.  When there exist other kinds of
     * messages, this code will have to become prepared to handle
     * them.
     */
    if (!protocol_discard_input(proto, dummy, err))
      goto closeout;
  case ilu_rhrc_nothing:
  case ilu_rhrc_handled:
    if (!ilu_ReEnterMutex(ilu_cmu, err))
      return ilu_FALSE;
    if (!_ilu_EnterServerMutex(server, ilu_TRUE, err))
      return ilu_FALSE;
    return ilu_TRUE;
  case ilu_rhrc_eof:
  case ilu_rhrc_error:
closeout:
    if (ILU_ERROK(*err)) {
      ILU_NOTE(CONNECTION_DEBUG,
	       ("Server closed connection %p to <%s>.\n",
		conn, server->sr_id));
    } else {
      ILU_NOTE(CONNECTION_DEBUG,
	       ("Closing connection %p to <%s>, due to error %s"
		" (raised at %s:%d).\n",
		conn, server->sr_id, ILU_ERR_NAME(*err),
		ilu_ErrorFile(err), ilu_ErrorLine(err)));
    }
    goto dunX;
  default:
    if (!ilu_Check(0, err))
      goto dunX;
    return ilu_TRUE;
  }
dunX:
  if (!ilu_ReEnterMutex(ilu_cmu, err))
    return ilu_FALSE;
  if (!_ilu_EnterServerMutex(server, ilu_TRUE, err))
    return ilu_FALSE;
  _ilu_CloseIoingConnection(conn, ILU_ERRNOK(*err),
			    ILU_ERROK(*err) ?
			    ilu_ConnShutdownReason_ReceivedEOF :
			    ilu_ConnShutdownReason_LostProtocolSync);
  return ilu_FALSE;
}

static ilu_boolean _ilu_CloseworthyErr(ilu_Error * err)
{
  ILU_ERR_SWITCH(*err) {
    ILU_SUCCESS_CASE
      return ilu_FALSE;
    ILU_ERR_CASE2(bad_locks, broken_locks)
      return ilu_FALSE;
    ILU_ERR_ELSE
      return ilu_TRUE;
  } ILU_ERR_ENDSWITCH;
}

static ilu_ConnShutdownReason _ilu_ShutdownReasonFromErr (ilu_Error *err)
{
  ILU_ERR_SWITCH(*err) {
    ILU_ERR_CASE3(no_memory, imp_limit, no_resources)
      return ilu_ConnShutdownReason_ResourceManagement;
    ILU_ERR_ELSE
      return ilu_ConnShutdownReason_LostProtocolSync;
  } ILU_ERR_ENDSWITCH;

}

/*Main Invariant holds; L2 disjoint {conn's callmu, iomu, waitmu}*/
static void ReadExtraMsg(ilu_refany rock)
{
  ilu_Connection  conn = (ilu_Connection) rock;
  ilu_Protocol    proto = connection_protocol(conn);
  ilu_Server      server = connection_server(conn);
  ilu_Call_s      dummyCall;
  ilu_Error       lerr;
  ilu_boolean     rereg = ilu_FALSE, initted = ilu_FALSE;
  ILU_NOTE(CALL_DEBUG,
	   ("ILU runtime kernel reading unsolicited message from connection %p"
	    ", with dummy call %p.\n",
	    conn, &dummyCall));
  if (!ilu_EnterMutex(ilu_cmu, &lerr))
    goto dun0;
  if (!_ilu_EnterServerMutex(server, ilu_FALSE, &lerr))
    goto dun1;
  if (conn->co_mucall != NIL)
    /* Spurious call; stop here. */
    goto dun2;
  if (conn->co_nOuts != 0)
    goto dun2;
  if (conn->co_waiting)
    goto dun2;
  if (connection_closed(conn))
    goto dun2;
  if (!ilu_ClearConnectionInputHandler(conn, &lerr))
    goto dun2;
  BuildCall(&dummyCall, conn, server, 0, ilu_FALSE);
  dummyCall.ca_disownWait = ilu_FALSE;
  if (!(*proto->pr_init_call) (&dummyCall, &lerr))
    goto dun2;
  initted = ilu_TRUE;
  if (!_ilu_EnterConnCall(conn, &dummyCall, ilu_TRUE, &lerr))
    goto dun2;
  _ilu_TakeConnWait(conn, &dummyCall);
  if (!_ilu_EnterConnIO(conn, ilu_TRUE, &lerr))
    goto dun4;
  /* L1 = {cmu, server}; L2 >= {conn's callmu, iomu, waitmu} */
  if (connection_closed(conn))
    goto dun5;
  rereg = ProcessExtraInput(conn, &dummyCall, &lerr);
dun5:
  if (ILU_ERROK(lerr) && proto->pr_prefinish_call != NULLFN) {
    (void) (*proto->pr_prefinish_call) (&dummyCall, &lerr);
    if (_ilu_CloseworthyErr(&lerr))
      _ilu_CloseIoingConnection(conn, (rereg = ilu_FALSE),
				_ilu_ShutdownReasonFromErr(&lerr));
    else
      rereg = rereg && !connection_closed(conn);
  }
  (void) _ilu_PushAndReleaseConnIO(conn, ilu_TRUE, &lerr);
dun4:
  (void) _ilu_ReleaseConnWait(conn, &dummyCall, ilu_TRUE, &lerr);
  (void) _ilu_ReleaseConnCall(conn, &dummyCall, ilu_TRUE, &lerr);
dun2:
  if (!rereg)
    (void) _ilu_MaybeFreeConnection(conn);
  (void) _ilu_ExitServerMutex(server, ilu_TRUE, &lerr);
  ILU_NOTE(CALL_DEBUG,
	   ("ILU runtime kernel done reading unsolicited message from connection %p"
	    ", with dummy call %p.\n",
	    conn, &dummyCall));
dun1:
  (void) ilu_ExitMutex(ilu_cmu, ilu_TRUE, &lerr);
  if (rereg) {
    if (!ilu_SetConnectionInputHandler(conn, ReadExtraMsg, conn, &lerr))
      goto dun0;
  }
dun0:
  ILU_HANDLED(lerr);
  if (initted) {
    (*proto->pr_finish_call) (&dummyCall, &lerr);
    ILU_HANDLED(lerr);
  }
  return;
}

/**Main Remnant;
   Before: L1 >= {cmu, s}, (L1-{s}).sup = cmu;
   After:  res>0 <=> L1 >= {cmu}, res>1 <=> L1 >= {s} */
static int
OpenConn(ilu_string pinfo, ilu_Protocol proto,
	 ilu_TransportInfo tinfo, ilu_TransportCreator tcr,
	 ilu_Server s, ilu_Passport pp,
	 ilu_Connection * pconn, ilu_Connection * new_conn,
	 ILU_ERRS((IoErrs, bad_locks,
		   inv_objref, no_resources)) * err)
{
  ilu_Connection  conn = NIL;
  ilu_integer     dfd, cdfd;
  ilu_Transport   t;
  *pconn = NIL;
  _ilu_DHoldsTCR(tcr, 1);
  if (!ilu_DeltaServerHolds(s, 1, err))
    return 2;
  if (!_ilu_ExitServerMutex(s, ilu_TRUE, err))
    goto dun1H;
  dfd = (*tcr->tcr_dfd) (tcr, ilu_FALSE);
  if ((ilu_fdbudget < ilu_fdstaken + dfd)
      && (dfd > 0)) {
    static const ilu_FineTime tOne = {1, 0};
    ilu_FineTime    tNow, tO;
    tNow = ilu_FineTime_Now();
    tO = ilu_FineTime_Add(tNow, tOne);
    if (!_ilu_ReduceFdsTo(ilu_fdbudget - dfd, &tO, err))
      goto dun1H;
    if ((ilu_fdbudget < ilu_fdstaken + dfd)
	&& (dfd > 0)) {
      ILU_NOTE(CALL_DEBUG,
	       ("call.c:OpenConn: FD budget exhausted.\n"));
      ILU_ERR_CONS1(no_resources, err, minor, ilu_nrm_fds, 0);
      goto dun1H;
    }
  }
  ilu_DeltaFD(dfd);
  if (!ilu_ExitMutex(ilu_cmu, ilu_TRUE, err))
    return 0;
  t = (*tcr->tcr_createTransport) (tcr, ilu_TRUE, &cdfd, pp, err);
  ILU_ERR_SWITCH(*err) {
    ILU_SUCCESS_CASE
      /* statement with no effect */;
    ILU_ERR_CASE(comm_failure, x) {
      ILU_HANDLED(*err);
      ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_cant_connect,
		    (void) 6);
    }
    ILU_ERR_CASE4(imp_limit, no_memory, internal, broken_locks)
      /* statement with no effect */;
    ILU_ERR_CASE(interrupted, x)
      /* statement with no effect */;
    ILU_ERR_CASE(bad_param, x)
      /* statement with no effect */;
    ILU_ERR_CASE(no_resources, x)
      /* statement with no effect */;
    ILU_ERR_ELSE {
      ILU_NOTE(CONNECTION_DEBUG,
	       ("ILU: (call.c:OpenConn)  Unexpected error <%s> "
		"returned from tcr_createTransport\n",
		ILU_ERR_NAME(*err)));
      ILU_HANDLED(*err);
      (void) ilu_Check(ilu_FALSE, err);
    }
  } ILU_ERR_ENDSWITCH;
  if (!ilu_ReEnterMutex(ilu_cmu, err))
    return 0;
  _ilu_DHoldsTCR(tcr, -1);
  ilu_DeltaFD(cdfd - dfd);

  if (ILU_ERRNOK(*err) || !t) {
    /* got an error or NIL transport during transport creation */
    ilu_Error       lerr;

    if (_ilu_EnterServerMutex(s, ilu_FALSE, &lerr)) {
      if (ilu_DeltaServerHolds(s, -1, &lerr)) {
	if (!server_is_true(s) && _ilu_CompareTinfo(tinfo, s->sr_tinfo))
	  s->sr_cfails = ilu_TRUE;
      }
      _ilu_ExitServerMutex(s, ilu_TRUE, &lerr);
    }
    /*
     * just ignore any local errors in favor of returning the error
     * from create transport
     */
    ILU_HANDLED(lerr);

    if (ILU_ERROK(*err)) {
      /*
       * got a NIL transport but no error was raised, so set an
       * error
       */
      ILU_ERR_CONS1(internal, err, minor, ilu_im_tcCreate, 0);
    }
    return 1;
  }

  if (!_ilu_EnterServerMutex(s, ilu_FALSE, err))
    return 1;
  if (!ilu_DeltaServerHolds(s, -1, err))
    return 2;
  conn = _ilu_CreateConnection(t, tinfo, NIL, proto, pinfo, NIL, s,
			       pp, err);
  if (conn == NIL)
    return 2;
  *pconn = conn;
  if (ilu_CanCondition())
    *new_conn = conn;
  else {
    conn->co_lsrCares = ilu_FALSE;
  }
  return 3;
dun1H:
  _ilu_DHoldsTCR(tcr, -1);
  return 1;
}

/**Main Invariant holds;
   after: success => Call-Locking(call, OHi)*/

ilu_boolean
ilu_StartCall(ilu_Call_s * c, ilu_Server s, ilu_Class intro_type,
	      ilu_Method method, ilu_LanguageIndex lang,
	      ilu_Passport pp,
	      ilu_Connection * new_conn,
	      ILU_ERRS((IoErrs, barrier, bad_locks,
			inv_objref, no_resources)) * err)
{
  return ilu_FullStartCall(c, s, intro_type, method, lang, pp, NIL,
			   NIL, new_conn, err);
}

ilu_boolean
ilu_FullStartCall(ilu_Call_s * c, ilu_Server s, ilu_Class intro_type,
		  ilu_Method method, ilu_LanguageIndex lang,
		  ilu_Passport pp,
		  ilu_Serializer si,	/* OPTIONAL */
		  ilu_Pipeline pl,	/* OPTIONAL */
		  ilu_Connection * new_conn,
		  ILU_ERRS((IoErrs, barrier, bad_locks,
			    inv_objref, no_resources)) * err)
{
  ilu_Connection  conn = NIL;
  ilu_TransportInfo tinfo;
  ilu_string      pinfo;
  ilu_Protocol    proto;
  ilu_TransportCreator tcr = ILU_NIL;
  ilu_boolean     isnew = ilu_FALSE;
  ilu_Error       lerr = ILU_INIT_NO_ERR;
  int             openres;

  ILU_CLER(*err);
  *new_conn = NIL;
  if ((s == NIL) || (pl && !pl->pl_lsrCares) || (si && !si->si_lsrCares))
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, ilu_FALSE);
  if (si && si->si_server != s)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_serialVsServer,
			 ilu_FALSE);

  if ((server_is_true(s) &&
       (method_id(method) > 0xFF80))
  /* calling "internal" method on true server */
      || (server_true_language(s) == lang)
  /* calling through kernel for should-be direct call */
    ) {
    ILU_NOTE(CALL_DEBUG,
	     ("ilu_FullStartCall:  attempting mediated call on true "
	      "instance in same compatibility domain \"%s\", kernel "
	      "server \"%s\"\n", _ilu_LangNames[lang], server_id(s)));
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_true, ilu_FALSE);
  };

  if (method->me_asynchronous &&
      !(server_is_true(s) || s->sr_tcr->tcr_reliable))
    return ILU_ERR_CONS1(bad_param, err, minor,
			 ilu_bpm_asynch_unreliable, ilu_FALSE);

  BuildCall(c, NIL, s, 0, ilu_FALSE);
  c->ca_lang = lang;
  c->ca_intro_type = intro_type;
  c->ca_method = method;
  if (!ilu_EnterMutex(ilu_cmu, err))
    goto dun0;
  if (!_ilu_EnterServerMutex(s, ilu_FALSE, err))
    goto dun1;
  /* L1 = {cmu, s}; ILU_ERROK(*err) */
  if (si) {
    while (si->si_conn ? (si->si_conn->co_mucall ||
			  (si->si_conn->co_pipeline &&
			   si->si_conn->co_pipeline != pl))
	   : si->si_connecting) {
      ilu_Condition   cv;
      if (!_ilu_CanCondition()) {
	ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_serialConcurrent,
		      (void) 6);
	goto dun2x;
      }
      if (si->si_conn)
	cv = connection_cc(si->si_conn);
      else {
	if (!si->si_connChg) {
	  char            pbuf[32];
	  sprintf(pbuf, "%p", si);
	  si->si_connChg = _ilu_CreateCondition("ilu_Serializer", pbuf,
						err);
	  if (ILU_ERRNOK(*err))
	    goto dun2x;
	}
	cv = si->si_connChg;
      }
      if (!ilu_CMWait2(cv, server_lock(s), ilu_cmu, err))
	goto dun2x;
      if ((!si->si_lsrCares) || (pl && !pl->pl_lsrCares)) {
	ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, (void) 6);
	goto dun2x;
      }
    }
    si->si_nCalls += 1;
    c->ca_si = si;
    if (si->si_conn) {
      conn = si->si_conn;
      if (!ilu_Check(conn->co_serialer == si && !conn->co_mucall, err))
	goto dun2;
      if (conn->co_closed || conn->co_doomed || conn->co_closing) {
	ilu_boolean     isBarrier = conn->co_lastWorking;
	conn->co_serialer = NIL;
	si->si_conn = NIL;
	ILU_NOTE(CONNECTION_DEBUG,
		 ("ilu_StartCall: dissociating serializer %p"
		  " and connection %p.\n",
		  si, conn));
	if (isBarrier) {
	  ILU_NOTE(CALL_DEBUG,
		   ("ilu_StartCall(serializer %p, connection %p) is"
		    " barrier because conn. closed or closing.\n",
		    si, conn));
	  ILU_ERR_CONS0(barrier, err, (void) 6);
	}
	_ilu_MaybeFreeConnection(conn);
	conn = NIL;
	if (isBarrier)
	  goto dun2;
      }
    }
  }
  if (!conn) {
    for (conn = server_connections(s);
	 (conn != NIL)
	 && (conn->co_mucall || conn->co_serialer ||
	     conn->co_doomed || conn->co_closing ||
	     (connection_concurrent(conn) ? si != NIL
	      : (conn->co_pipeline && conn->co_pipeline != pl)));
	 conn = connection_next(conn))
      /* statement with no effect */;
  }
  if (conn == NIL) {
    /* conn==NIL, L1={cmu, s}, L2 as at entry */
    if (server_is_true(s)) {
      tinfo = port_tinfo(server_local_port(s));
      pinfo = port_pinfo(server_local_port(s));
      proto = port_protocol(server_local_port(s));
      tcr = port_transport_creator(server_local_port(s));
    } else {
      tinfo = s->sr_tinfo;
      tcr = s->sr_tcr;
      pinfo = s->sr_pinfo;
      proto = s->sr_protocol;
    }
    if (si && protocol_concurrent(proto)) {
      ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_conc_serial,
		    (void) 6);
      goto dun2;
    }
    if (si && !tcr->tcr_reliable) {
      ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_serialVsTransport,
		    (void) 6);
      goto dun2;
    }
    if ((proto->pr_needs_boundaried_transport && (!tcr->tcr_boundaried)) ||
	(proto->pr_needs_reliable_transport && (!tcr->tcr_reliable))) {
      ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_tinfo, (void) 6);
      goto dun2;
    };
    if (si) {
      if (!ilu_Check(!si->si_connecting, err))
	goto dun2;
      si->si_connecting = ilu_TRUE;
    }
    openres = OpenConn(pinfo, proto, tinfo, tcr, s, pp,
		       &conn, new_conn, err);
    if (si) {
      si->si_connecting = ilu_FALSE;
      if (si->si_connChg) {
	ilu_Error lerr;
	if (!ilu_CondNotify(si->si_connChg, &lerr)) {
	  if (ILU_ERROK(*err)) 	  /* set error if there isn't one already set */
	    *err = lerr;
	  else 	  /* otherwise just ignore this local error and let the previous one through */
	    ILU_HANDLED(lerr);
	  goto dun0;
	}
      }
    }
    switch (openres) {
    case 3:
      break;
    case 2:
      goto dun2;
    case 1:
      goto dun1;
    case 0:
      goto dun0;
    }
    isnew = ilu_TRUE;
  }
  /* conn != NIL && conn's IO and call mutexes not held */
  if (!ilu_Check(!connection_closed(conn), err))
    goto dun2;
  if (!ilu_Check(!connection_incoming(conn), err))
    goto dun2;
  if (!ilu_Check((conn->co_nOuts == 0 || conn->co_pipeline
		  || connection_concurrent(conn)),
		 err))
    goto dun2;
  if (!ilu_Check(conn->co_pipeline == pl || !conn->co_pipeline, err))
    goto dun2;
  if (si && !si->si_conn) {
    si->si_conn = conn;
    conn->co_serialer = si;
    ILU_NOTE(CONNECTION_DEBUG,
	     ("ilu_StartCall: associating serializer %p"
	      "and connection %p.\n",
	      si, conn));
  }
  if (pl && !connection_concurrent(conn)) {
    pl->pl_nCalls++;
    c->ca_pl = pl;
    conn->co_pipeline = pl;
  }
  c->ca_disownWait = conn->co_waiting && !_ilu_CanCondition();
  if (!_ilu_CanCondition() && conn->co_nOuts == 0 && !isnew) {
    if (!ilu_Check(!conn->co_waiting, err))
      goto dun2;
    if (!ilu_ClearConnectionInputHandler(conn, err))
      goto dun2;
  }
  if (!_ilu_EnterConnCall(conn, c, ilu_FALSE, err))
    goto dun2;
  if (!_ilu_EnterConnIO(conn, ilu_FALSE, err))
    goto dun3;
  if (!ilu_Check(!connection_closish(conn), err))
    /* XXX Why do we think this can't happen? */
    goto dun4;
  c->ca_ms = ilu_cmsHi;
  c->ca_msInput = ilu_FALSE;
  proto = conn->co_protocol;
  /* conn != NIL && conn's IO and call mutexes held */
  if (conn->co_next_sn > 0xFFFFFF)	/* why not 0xFFFF ? */
    conn->co_next_sn = 1;
  c->ca_connection = conn;
  c->ca_SN = conn->co_next_sn;	/* co_next_sn is incremented after
				   StartRequest so that we don't
				   waste serial numbers if the
				   request is never made */
  conn->co_nCalls += 1;
  if (!(*proto->pr_init_call) (c, err)) {
    conn->co_nCalls -= 1;
    goto dun2;
  }
  goto dun2x;
dun4:
  (void) _ilu_ReleaseConnIO(conn, ilu_TRUE, err);
dun3:
  (void) _ilu_QuickReleaseConnCall(conn, c, ilu_TRUE, err);
dun2:
  if (si)
    si->si_nCalls -= 1;
  if (pl && c->ca_pl == pl) {
    pl->pl_nCalls -= 1;
  }
dun2x:
  if (conn && conn->co_nCalls == 0)
    conn->co_pipeline = NIL;
  _ilu_MaybeFreePipeline(pl);
  _ilu_MaybeFreeSerializer(si);
  if (conn)
    _ilu_MaybeFreeConnection(conn);
  (void) _ilu_ExitServerMutex(s, ilu_TRUE, err);
dun1:
  (void) ilu_ExitMutex(ilu_cmu, ilu_TRUE, err);
dun0:
  if (ILU_ERROK(*err)) {
    ILU_NOTE(CALL_DEBUG,
	     ("%-20.20s(%p over %p to \"%s\" #%lu,"
	      " %s.%s, pl=%p, si=%p).\n",
	      "ilu_StartCall", c, call_connection(c),
	      call_server_id(c), call_serial_number(c),
	      class_name(intro_type), method_name(method),
	      pl, si));
    c->ca_caller = pp;
    return ilu_TRUE;
  } else {
    ILU_NOTE(CALL_DEBUG,
	     ("%-20.20s(to %s, %s.%s, pl=%p, si=%p"
	      ") raises err %s (from %s ln %d).\n",
	      "ilu_StartCall", server_id(s),
	      class_name(intro_type), method_name(method),
	      pl, si, ILU_ERR_NAME(*err),
	      ilu_ErrorFile(err), ilu_ErrorLine(err)));
    ILU_HANDLED(lerr);
    return ilu_FALSE;
  }
}

/**Before: Call-Invariant(call, err).
   After:  Main Invariant holds,
	   (L2 disjoint {call's conn's callmu, iomu}), and
	   (L2 disjoint {call's conn's waitmu} iff !call->ca_disownWait)
	   if possible (*err indicates bad_locks or broken_locks
	   when not possible).*/
void
ilu_FinishCall(ilu_Call call, ILU_ERRS((bad_locks, IoErrs)) * err)
{
  ilu_Server      s;
  ilu_Connection  conn;
  ilu_Protocol    proto;
  ILU_ERRS((bad_locks, broken_locks, internal)) lerr = ILU_INIT_NO_ERR;
  ilu_boolean     closeconn = ilu_FALSE, rewatch = ilu_FALSE;
  ilu_Serializer  si = NIL;

#ifdef ENABLE_DEBUGGING
  if (call->ca_incoming) {
    ILU_NOTE(CALL_DEBUG,
	  ("%-20.20s(%p from %s #%lu) => err=%s ca_pe=%s\n",
	   "ilu_FinishCall", call, call_connection_id(call),
	   (unsigned long) call_serial_number(call),
	   ILU_ERR_NAME(*err), ilu_PEName(call->ca_pe)
	   ));
  } else {
    ILU_NOTE(CALL_DEBUG,
	     ("%-20.20s(%p over %p to \"%s\" #%lu, pl=%p, si=%p"
	      ") => err=%s ca_pe=%s\n",
	      "ilu_FinishCall", call, call_connection(call),
	      call_server_id(call),
	      (unsigned long) call_serial_number(call),
	      call->ca_pl, call->ca_si,
	      ILU_ERR_NAME(*err), ilu_PEName(call->ca_pe)
	      ));
  }
#endif				/* ENABLE_DEBUGGING */
  ILU_ERR_SWITCH(*err) {
    ILU_SUCCESS_CASE
      /* statement with no effect */ ;
    ILU_ERR_CASE2(comm_failure, internal)
      closeconn = ilu_TRUE;
    ILU_ERR_CASE2(bad_locks, broken_locks)
    /*
     * Here we leak like a sieve, but we can't free everything
     * reliably, and so we leave everything lying around, in hopes
     * that it's useful for debugging.
     */
      return;
    ILU_ERR_ELSE
      /* statement with no effect */ ;
  } ILU_ERR_ENDSWITCH;
  /* (*err) may indicate incoming error; lerr = success */
  _ilu_Assert(closeconn || ILU_WaitRem(call),
	      "ilu_FinishCall locking wrt wait");
  s = call_server(call);
  conn = call_connection(call);
  proto = conn ? connection_protocol(conn) : NIL;
  _ilu_Assert((call_incoming(call) ||
	       call->ca_pe == ilu_ProtocolException_Success),
	      "client PE");
  if (!ilu_EnterMutex(ilu_cmu, &lerr))
    goto dun0;
  if (!_ilu_EnterServerMutex(s, ilu_FALSE, &lerr))
    goto dun1;
  if (conn && conn->co_callmuBorrowable) {
    while (conn->co_mucall && conn->co_mucall->ca_callmuBorrower) {
      if (!ilu_CMWait2(conn->co_cc, server_lock(s), ilu_cmu, &lerr))
	goto dun0;
    }
    conn->co_callmuBorrowable = ilu_FALSE;
  }
  if (call->ca_ms != ilu_cmsHi)
    call->ca_msInput = ilu_FALSE;
  /* L2 >= {waitmu} iff ca_disownWait || ca_msInput && !ca_dontWait */
  if (call->ca_ms == ilu_cmsNo && !closeconn
      && !(conn && proto->pr_prefinish_call)
      && ((call->ca_pe == ilu_ProtocolException_Success
	   && ILU_ERROK(*err))
	  || !call_incoming(call)))
    goto dun3;
  _ilu_Assert(conn != NIL, "FinishCall slow path && !conn");
  if (call->ca_ms != ilu_cmsHi) {
    /* L2 >= {waitmu} iff ca_disownWait (=> single-threaded) */
    ilu_boolean     conc = connection_concurrent(conn);
    if (!ilu_Check(call->ca_ios == ilu_ciosNone, &lerr))
      goto dun2;
    if (call->ca_ms == ilu_cmsNo || conc || conn->co_pipeline) {
      if (!_ilu_EnterConnCall(conn, call, ilu_FALSE, &lerr))
	goto dun2;
      if ((conc || conn->co_pipeline) && call->ca_ms == ilu_cmsLo) {
	if (!ilu_Check(conn->co_nOuts > 0, &lerr))
	  goto dun4;
	conn->co_nOuts--;
      }
    }
    if (!_ilu_EnterConnIO(conn, ilu_FALSE, &lerr))
      goto dun4;
    if (connection_closed(conn))
      closeconn = ilu_TRUE;
  }
  /** L2 >= {call's conn's callmu, iomu};
      L2 >= {waitmu} iff ca_disownWait || ca_msInput && !ca_dontWait */
  /* (*err) may indicate incoming error; lerr = success */
  if (closeconn)
    _ilu_CloseIoingConnection(conn, ilu_TRUE,
			  ilu_ConnShutdownReason_LostProtocolSync);
  else {
    if (call->ca_ios == ilu_ciosNone &&
	(!call_incoming(call) ||
	 ((call->ca_pe == ilu_ProtocolException_Success) &&
	  ILU_ERROK(*err))))
      goto dunZ;
    if (!_ilu_ExitServerMutex(s, ilu_TRUE, &lerr))
      return;
    if (!ilu_ExitMutex(ilu_cmu, ilu_TRUE, &lerr))
      return;
    switch (call->ca_ios) {
    case ilu_ciosIn:
      if (!ilu_Check(call->ca_msInput, &lerr))
	goto dunX;
      if (!proto->pr_discard_input(call, &lerr))
	goto dunX;
      break;
    case ilu_ciosOut:
      if (!proto->pr_discard_output(call, &lerr))
	goto dunX;
    case ilu_ciosNone:
      break;
    default:
      if (!ilu_Check(ilu_FALSE, &lerr))
	goto dunX;
    }
    /* (*err) may indicate incoming error; lerr = success */
    if (call_incoming(call) &&
	(call->ca_pe != ilu_ProtocolException_Success ||
	 ILU_ERRNOK(*err))) {
      ilu_cardinal    exsize;
      if (call->ca_pe == ilu_ProtocolException_Success)
	call->ca_pe = ilu_ProtocolException_Unknown;
      ILU_CLER(*err);
      exsize = ilu_BeginSizingException(call, -(ilu_integer) call->ca_pe,
					err);
      if (ILU_ERRNOK(*err))
	goto dunX;
      ILU_NOTE(CALL_DEBUG,
	    ("%-20.20s(%p %s #%lu, excn ilu_ProtocolException_%s)\n",
	     "pr_begin_exception", call, call_connection_id(call),
	     (unsigned long) call_serial_number(call),
	     ilu_PEName(call->ca_pe)));
      if (!protocol_begin_exception(proto, call, 0, call->ca_pe,
				    exsize, err))
	goto dunX;
      ILU_NOTE(CALL_DEBUG,
	    ("%-20.20s(%p %s #%lu)\n", "pr_finish_exception",
	     call, call_connection_id(call),
	     (unsigned long) call_serial_number(call)));
      if (!protocol_finish_exception(proto, call, ilu_TRUE, err))
	goto dunX;
    }
dunX:
    /* (*err) and lerr may each have error; (*err) has priority. */
    if (!ilu_ReEnterMutex(ilu_cmu, err))
      return;
    if (!_ilu_EnterServerMutex(s, ilu_TRUE, err))
      return;
dunZ:
    if (ILU_ERROK(*err))
      *err = lerr;
    else
      ILU_HANDLED(lerr);
    /* (*err) may have error; lerr holds trash */
    ILU_ERR_SWITCH(*err) {
      ILU_SUCCESS_CASE
	/* no effect */ ;
      ILU_ERR_CASE(comm_failure, v)
	_ilu_CloseIoingConnection(conn, ilu_TRUE,
			  ilu_ConnShutdownReason_LostProtocolSync);
      ILU_ERR_ELSE
	/* no effect */ ;
    } ILU_ERR_ENDSWITCH;
  }
  /* (*err) may have error; lerr holds trash */
  if (proto != NIL && proto->pr_prefinish_call != NULLFN)
    (void) (*proto->pr_prefinish_call) (call, &lerr);
  /* (*err) and lerr may each have error; (*err) has priority. */
  if (!_ilu_PushAndReleaseConnIO(conn, ilu_FALSE, &lerr))
    goto dun2;
dun4:
  /* (*err) and lerr may each have error; (*err) has priority. */
  (void) _ilu_ReleaseConnCall(conn, call, ilu_FALSE, &lerr);
dun3:
  /* (*err) and lerr may each have error; (*err) has priority. */
  if (call->ca_msInput && !call->ca_dontWait && !call->ca_disownWait)
    (void) _ilu_ReleaseConnWait(conn, call, ilu_FALSE, &lerr);
dun2:
  /* (*err) and lerr may each have error; (*err) has priority. */
  if (ILU_ERROK(*err))
    *err = lerr;
  else
    ILU_HANDLED(lerr);
  ILU_CLER(lerr);
  /* (*err) may have error; lerr = success */
  si = call->ca_si;
  if (si) {
    if (ilu_Check(si->si_nCalls > 0, &lerr))
      si->si_nCalls -= 1;
  }
  /* (*err) and lerr may each have error; (*err) has priority. */
  _ilu_MaybeFreeSerializer(si);
  if (call->ca_pl) {
    call->ca_pl->pl_nCalls -= 1;
    _ilu_MaybeFreePipeline(call->ca_pl);
    call->ca_pl = NIL;
  }
  if (conn) {
    conn->co_nCalls -= 1;
    if (conn->co_nCalls == 0)
      conn->co_pipeline = NIL;
    if (conn->co_doomed && conn->co_nCalls == 0 && ILU_ERROK(*err)) {
      lerr = _ilu_CloseConnection(conn,
				ilu_ConnShutdownReason_Relocating);
    }
    rewatch = (!connection_closed(conn) && !connection_incoming(conn)
	       && !_ilu_CanCondition() && conn->co_nOuts == 0);
    _ilu_MaybeFreeConnection(conn);
  }
  (void) _ilu_ExitServerMutex(s, ilu_TRUE, &lerr);
dun1:
  /* (*err) and lerr may each have error; (*err) has priority. */
  (void) ilu_ExitMutex(ilu_cmu, ilu_TRUE, &lerr);
dun0:
  /* (*err) and lerr may each have error; (*err) has priority. */
  if (ILU_ERROK(*err))
    *err = lerr;
  else
    ILU_HANDLED(lerr);
  /* (*err) may have error; lerr holds trash */
  if (proto != NIL && proto->pr_finish_call != NULLFN)
    (void) (*proto->pr_finish_call) (call, &lerr);
  if (ILU_ERROK(*err))
    *err = lerr;
  else
    ILU_HANDLED(lerr);
  /* (*err) may have error; lerr holds trash */
  if (rewatch) {
    (void) ilu_SetConnectionInputHandler(conn, ReadExtraMsg, conn, &lerr);
    if (ILU_ERROK(*err))
      *err = lerr;
    else
      ILU_HANDLED(lerr);
  }
  /* (*err) may have error; lerr holds trash */
  if (call->ca_incoming && (call->ca_caller != NIL)) {
    ilu_DestroyPassport(call->ca_caller, &lerr);
    call->ca_caller = NIL;
    if (ILU_ERROK(*err))
      *err = lerr;
    else
      ILU_HANDLED(lerr);
  }
  /* (*err) may have error; lerr holds trash */
  return;
}

/*L1, L2 unconstrained*/
static ilu_RcvReqStat Unbuild(ilu_Call call, ilu_RcvReqStat ans)
{
  return ans;
}

/*Main Invariant holds*/
/**before: L2 disjoint {conn's callmu, iomu, waitmu},
 *  after: Call-Locking(call, IHi)                   if  *initted,
 *  after: L2 disjoint {conn's callmu, iomu, waitmu} if !*initted */

ilu_RcvReqStat
ilu_ReceiveRequest(ilu_Call_s * call, ilu_boolean * initted,
		   ilu_Connection conn, ilu_Class * pit,
		   ilu_Method * meth, ilu_cardinal * sn,
		   ILU_ERRS((bad_locks, IoErrs)) * err)
{
  ilu_Server      server = connection_server(conn);
  ilu_PacketType  pkt_type;
  ilu_Error       lerr = ILU_INIT_NO_ERR;
  ilu_Protocol    proto = conn->co_protocol;
  ilu_ReadHeaderResultCode rhrc = ilu_rhrc_error;
  ilu_ConnShutdownReason eof_reason;
  ilu_cardinal	  eof_last_sn;
  ilu_boolean     stdMeth = ilu_FALSE, closed = ilu_FALSE;

  BuildCall(call, conn, server, 0, ilu_TRUE);
  call->ca_disownWait = ilu_FALSE;
  *initted = ilu_FALSE;
  if (!ilu_EnterMutex(ilu_cmu, err))
    return Unbuild(call, ilu_RcvReqStat_noop);
  if (!_ilu_EnterServerMutex(server, ilu_FALSE, err)) {
    (void) ilu_ExitMutex(ilu_cmu, ilu_TRUE, err);
    return Unbuild(call, ilu_RcvReqStat_noop);
  }
  if (connection_closed(conn)) {
    (void) _ilu_ExitServerMutex(server, ilu_TRUE, err);
    (void) ilu_ExitMutex(ilu_cmu, ilu_TRUE, err);
    return Unbuild(call, ilu_RcvReqStat_quit);
  }
  if (conn->co_waiting) {
    ILU_ERR_CONS0(bad_locks, err, (void) 6);
    (void) _ilu_ExitServerMutex(server, ilu_TRUE, err);
    (void) ilu_ExitMutex(ilu_cmu, ilu_TRUE, err);
    return Unbuild(call, ilu_RcvReqStat_noop);
  }
  if (!_ilu_EnterConnCall(conn, call, ilu_FALSE, err)) {
    (void) _ilu_ExitServerMutex(server, ilu_TRUE, err);
    (void) ilu_ExitMutex(ilu_cmu, ilu_TRUE, err);
    return Unbuild(call, ilu_RcvReqStat_noop);
  }
  _ilu_TakeConnWait(conn, call);
  if (!_ilu_EnterConnIO(conn, ilu_FALSE, err))
    goto lab2;
  if (connection_closed(conn)) {
    (void) _ilu_ReleaseConnIO(conn, ilu_TRUE, err);
    (void) _ilu_QuickReleaseConnCall(conn, call, ilu_TRUE, err);
    (void) _ilu_ReleaseConnWait(conn, call, ilu_TRUE, err);
    (void) _ilu_ExitServerMutex(server, ilu_TRUE, err);
    (void) ilu_ExitMutex(ilu_cmu, ilu_TRUE, err);
    return Unbuild(call, ilu_RcvReqStat_quit);
  }
  call->ca_ms = ilu_cmsHi;
  call->ca_msInput = ilu_TRUE;
  call->ca_dontWait = ilu_FALSE;
  if ((*proto->pr_init_call) (call, err)) {
    /* We're now obliged to eventually call pr_finish_call */
    *initted = ilu_TRUE;
    conn->co_nCalls += 1;
    goto lab0;
  }
  (void) _ilu_ReleaseConnIO(conn, ilu_TRUE, err);
lab2:
  call->ca_ms = ilu_cmsNo;
  call->ca_msInput = ilu_FALSE;
  (void) _ilu_ReleaseConnWait(conn, call, ilu_TRUE, err);
  (void) _ilu_QuickReleaseConnCall(conn, call, ilu_TRUE, err);
  closed = connection_closed(conn);
lab0:
  (void) _ilu_ExitServerMutex(server, ilu_TRUE, err);
  (void) ilu_ExitMutex(ilu_cmu, ilu_TRUE, err);
  if (closed)
    return ilu_RcvReqStat_quit;
  ILU_ERR_SWITCH(*err) {
    ILU_SUCCESS_CASE
      {};
    ILU_ERR_CASE3(comm_failure, bad_param, no_memory) {
      return ilu_RcvReqStat_quit;
    }
    ILU_ERR_ELSE
      return ilu_RcvReqStat_noop;
  } ILU_ERR_ENDSWITCH;    
  /* Call-Locking(call, IHi) */
  rhrc = protocol_read_header(proto, call, &pkt_type, sn, &eof_reason,
			      &eof_last_sn, &lerr);
  switch (rhrc) {
  case ilu_rhrc_ok:
    call->ca_ios = ilu_ciosIn;
    break;
  case ilu_rhrc_eof:
    (void) _ilu_CloseConnWithIo(conn, ilu_FALSE,
			  ilu_ConnShutdownReason_ReceivedEOF, err);
    return ilu_RcvReqStat_quit;
  case ilu_rhrc_nothing:
  case ilu_rhrc_handled:
    return ilu_RcvReqStat_noop;
  case ilu_rhrc_error:
    ILU_ERR_SWITCH(lerr) {
      ILU_ERR_CASE(comm_failure, v) {
	*err = lerr;
	if (!_ilu_CloseConnWithIo(conn, ilu_TRUE,
		     ilu_ConnShutdownReason_LostProtocolSync, err))
	  return ilu_RcvReqStat_quit;
	return ilu_RcvReqStat_quit;
      }
      ILU_ERR_CASE(bad_param, v) {
	ILU_HANDLED(lerr);
	ILU_ERR_CONS1(internal, err, minor, ilu_im_unhandled, 0);
	return ilu_RcvReqStat_noop;
      }
      ILU_ERR_CASE3(no_memory, internal, broken_locks) {
	*err = lerr;
	return ilu_RcvReqStat_noop;
      }
      ILU_ERR_CASE2(imp_limit, marshal) {
	*err = lerr;
	return ilu_RcvReqStat_noop;
      }
    } ILU_ERR_ENDSWITCH;
  default:
    if (!ilu_Check(ilu_FALSE, err))
      return ilu_RcvReqStat_noop;
  }
  if (pkt_type != ilu_PacketType_Request)
    return ilu_RcvReqStat_noop;
  call_serial_number(call) = *sn;
  if (_ilu_AddConnIdentities (call, err), ILU_ERRNOK(*err))
    {
      call->ca_caller = NIL;
      return ilu_RcvReqStat_noop;
    }
  if (protocol_interpret_request(proto, call, err)) {
    ilu_Method      m = call_method(call);
    stdMeth = m != NIL && method_id(m) >= 0xFF00 && method_id(m) < 0xFFFF;
    ILU_NOTE((CALL_DEBUG | INCOMING_DEBUG),
	  ("%-20.20s(%p over %p from %s #%lu, %s.%s)\n",
	   "ilu_ReceiveRequest", call,
	   conn, call_connection_id(call),
	   (unsigned long) call_serial_number(call),
	   class_name(call_intro_type(call)),
	   method_name(call_method(call))));
  } else {
    ILU_NOTE(INCOMING_DEBUG,
	  ("%-20.20s(%p over %p from %s #%lu) error:  *err=%s, ca_pe=%s.\n",
	   "ilu_ReceiveRequest", call, conn, call_connection_id(call),
	   (long unsigned) call_serial_number(call),
	   ILU_ERR_NAME(*err), ilu_PEName(call->ca_pe)));
    (void) ilu_Check((ILU_ERRNOK(*err) ||
		      call->ca_pe != ilu_ProtocolException_Success),
		     err);
    return ilu_RcvReqStat_noop;
  }
  if (!_ilu_EnterServerMutex(server, ilu_TRUE, err))
    return ilu_RcvReqStat_noop;
  if (conn->co_port->po_call_cache != NIL) {
    ilu_CachedCall *cc = conn->co_port->po_call_cache;
    int             i;
    for (i = 0; i < conn->co_port->po_call_cache_size; i++) {
      if ((cc[i].cc_sn == *sn)
	  && (cc[i].cc_intro_type == call->ca_intro_type)
	  && (cc[i].cc_meth == call_method(call))
	  && (cc[i].cc_replyMsg.msg_base != NIL)
	  && strcmp(cc[i].cc_peerinfo, conn_peerinfo(conn)) == 0) {
	if (!_ilu_ExitServerMutex(server, ilu_TRUE, err))
	  return ilu_RcvReqStat_noop;
	ILU_NOTE(CALL_DEBUG,
	      ("ilu_ReceiveRequest(%p): resending cached reply"
	       " to call %ld from %s.\n",
	       call, cc[i].cc_sn, cc[i].cc_peerinfo));
	call->ca_ios = ilu_ciosNone;
	if (!protocol_discard_input(proto, call, err)) {
	  return ilu_RcvReqStat_noop;
	}
	if (!transport_send_whole_message(conn->co_transport,
					  &cc[i].cc_replyMsg,
					  err))
	  return ilu_RcvReqStat_noop;
	return ilu_RcvReqStat_noop;
      }
    }
  }
  if (!_ilu_ExitServerMutex(server, ilu_TRUE, err))
    return ilu_RcvReqStat_noop;
  if (stdMeth) {
    (call_method(call)->me_stubprocs[_ilu_InternalLanguageIndex]) (call);
    return ilu_RcvReqStat_noop;
  }
  *pit = call->ca_intro_type;
  *meth = call_method(call);
  call->ca_pe = ilu_ProtocolException_GarbageArguments;
  return ilu_RcvReqStat_request;
}

#ifdef WIN32
/* AVOID COMPILER BUG by turning off global optimization for this function
  F:\ilu\src\runtime\kernel\call.c(457) : fatal error C1001: INTERNAL COMPILER ERROR
  (compiler file 'l:\b_bld\c2\P2\main.c', line 374) */
#pragma optimize("g", off)
#endif

/*L1, L2, Main unconstrained*/
static ilu_FineTime ClipAddTime(ilu_FineTime a, ilu_FineTime b,
				ilu_FineTime l)
{
  ilu_FineTime c = ilu_FineTime_Add(a, b);
  if (ilu_FineTime_Cmp(c, l) > 0)
       return l;
  else return c;
}

#ifdef WIN32
/* restore optimizations to original settings */
#pragma optimize("", on)
#endif

/*mxamu = ilu_cmu*/
typedef struct {
  /*L1 >= {ilu_cmu}; L2 unconstrained*/
  
  ilu_Alarmette_s	gra_alarmette;
  ilu_private		gra_cc;
} GetReplyAlarm;

/*L1_sup = ilu_cmu; L2 unconstrained*/

static void GRInvoke(ilu_Alarmette a);
static void GRSet(ilu_FineTime t);
static void GRCancel(void);

static ilu_Alarmette_s grHead = {&grHead, &grHead, ilu_FALSE, {0, 0}};
static ilu_AlarmRep grar = {&grHead, GRInvoke, GRSet, GRCancel};

/*Main Invariant holds*/
static void GraInvoke(ilu_private rock)
{
  ilu_FineTime now = ilu_FineTime_Now();
  _ilu_AcquireMutex(ilu_cmu);
  ilu_MXAProc(now, &grar);
  _ilu_ReleaseMutex(ilu_cmu);
  return;
}

static void GRInvoke(ilu_Alarmette a)
{
  ilu_Error err;
  GetReplyAlarm *gra = (GetReplyAlarm*) a;
  err = _ilu_NotifyCondition(gra->gra_cc);
  ILU_MUST_BE_SUCCESS(err);
  return;
}

static void GRSet(ilu_FineTime t)
{
  ilu_SetAlarm(_ilu_grAlarm, t, GraInvoke, NIL);
  return;
}

static void GRCancel(void)
{
  ilu_UnsetAlarm(_ilu_grAlarm);
  return;
}

/*L1, L2 unconstrained*/
static void FreeMessage(ilu_Message * msg)
{
  if (msg->msg_base != NIL) {
    ilu_free(msg->msg_base);
    msg->msg_base = NIL;
  }
  return;
}

/**Before: Call-Locking(call, OHi);
    After: Call-invariant(call, err) &&
	   (success => Call-Locking(call, IHi)) */
ilu_ProtocolException
ilu_GetReply(ilu_Call call, ilu_cardinal * estatus,
	     ilu_Connection * new_conn,
	     ILU_ERRS((bad_locks, IoErrs)) * err)
{
  ilu_Completion  completion;
  return ilu_FullGetReply(call, estatus, &completion, new_conn, err);
}

/**Before: Call-Locking(call, OHi);
    After: Call-invariant(call, err) &&
	   (success => Call-Locking(call, IHi)) */
ilu_ProtocolException
ilu_FullGetReply(ilu_Call call, ilu_cardinal * estatus,
		 ilu_Completion * completion,
		 ilu_Connection * new_conn,
		 ILU_ERRS((bad_locks, IoErrs)) * err)
{
  ilu_Connection  conn = call_connection(call);
  ilu_Protocol    proto = connection_protocol(conn);
  ilu_Server      server = connection_server(conn);
  ilu_Transport   trans = connection_transport(conn);
  ilu_cardinal    pktsn;
  ilu_PacketType  pkttype;
  ilu_ProtocolException ans;
  ilu_boolean     reliable = transport_reliable(trans);
  ilu_boolean     prco = protocol_concurrent(proto);
  ilu_boolean     MT = _ilu_CanCondition();
  ilu_FineTime    now;
  ilu_FineTime    timeout;
  ilu_FineTime    limit, *elimit;
  ilu_FineTime    grandLimit;
  GetReplyAlarm   gra = {{NIL, NIL, ilu_FALSE, {0, 0}}, NIL};
  const char     *reply_src = NIL;
  ilu_ProtocolInfo new_pinfo = NIL;
  ilu_Protocol    new_proto = NIL;
  ilu_TransportInfo new_tinfo = NIL;
  ilu_TransportCreator new_tcr = NIL;
  ilu_RelocateScope rel_scope;
  ilu_boolean     rewatch = ilu_FALSE;
  ilu_Connection  oldConn = conn;
  ilu_ConnShutdownReason eof_reason;
  ilu_cardinal    eof_last_sn = 0;
  ilu_boolean     last_sn_good = ilu_FALSE;

  *completion = ILU_COMPLETED_MAYBE;
  *new_conn = NIL;
  if (method_asynchronous(call_method(call)))
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh,
			 ilu_ProtocolException_Not);
  if (call->ca_dontWait)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh,
			 ilu_ProtocolException_Not);
  if (conn->co_serialer != call->ca_si)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh,
			 ilu_ProtocolException_Not);
  if (conn->co_mucall != call || !conn->co_ioing || call->ca_msInput)
    return ILU_ERR_CONS0(bad_locks, err, ilu_ProtocolException_Not);
  if (!ILU_WaitRem(call))
    return ILU_ERR_CONS0(bad_locks, err, ilu_ProtocolException_Not);

  ILU_NOTE(CALL_DEBUG, ("%-20.20s(%p over %p to \"%s\" #%lu)...\n",
			"ilu_GetReply", call, call_connection(call),
			call_server_id(call),
			(unsigned long) call_serial_number(call)));

  timeout = trans->tr_to1;
  if (!reliable) {
    now = ilu_FineTime_Now();
    limit = ilu_FineTime_Add(now, timeout);
    grandLimit = ilu_FineTime_Add(now, trans->tr_tto);
    elimit = &limit;
  } else
    elimit = NIL;
  /* Call-Locking(call, OHi) */
  /* (=> (this thread holds waitmu iff disownWait)) */
  while (1) {
    /* Call-Locking(call), IHi or OHi; !call->ca_dontWait */
    /* L2 >= {conn's waitmu} iff ca_disownWait || ca_msInput */
    ilu_ReplyList   qrl = NIL;
    if (!ilu_EnterMutex(ilu_cmu, err))
      return ilu_ProtocolException_Not;
    if (!_ilu_EnterServerMutex(server, ilu_FALSE, err))
      goto abort1;
    if (!_ilu_PushAndReleaseConnIO(conn, ilu_FALSE, err))
      goto abort2;
    if (prco || conn->co_pipeline) {	/* release the call mutex */
      conn->co_nOuts++;
      if (!_ilu_ReleaseConnCall(conn, call, ilu_FALSE, err))
	goto abort2;
    }
    call->ca_ms = ilu_cmsLo;
    if (connection_closed(conn)) {
      ILU_ERR_CONS1(comm_failure, err, minor, ilu_cfm_conn_lost,
		    (void) 6);
      goto abort2;
    }
    /* L1 = {cmu, server}; Call-Lo(call) */
    /* L2 >= {conn's waitmu} iff ca_disownWait || ca_msInput */
    if (MT) {
      if (!reliable) {
	gra.gra_cc = connection_cc(conn);
	ilu_MXASet(&grar, &gra.gra_alarmette, limit);
      }
      if (conn->co_waiting && !call->ca_msInput) {
	while (conn->co_waiting) {
	  if (!ilu_CMWait2(connection_cc(conn), server_lock(server),
			   ilu_cmu, err)) {
	    if (!reliable)
	      ilu_MXAClear(&grar, &gra.gra_alarmette);
	    goto abort2;
	  }
	  qrl = _ilu_GetQueuedReply(call);
	  if (qrl)
	    goto waitdun;
	}
	_ilu_TakeConnWait(conn, call);
	goto waitdun;
      }
    }
    if ((MT && !call->ca_msInput) || !conn->co_waiting)
      _ilu_TakeConnWait(conn, call);
    (void) _ilu_BlockingWaitForInputOnConnection(conn, elimit, ilu_TRUE,
						 err);
    ILU_ERR_SWITCH(*err) {
      ILU_SUCCESS_CASE
	/* do nothing */;
      ILU_ERR_CASE2(interrupted, internal)
	/* do nothing */;
      ILU_ERR_CASE(broken_locks, x)
	return ilu_ProtocolException_Not;
    } ILU_ERR_ENDSWITCH;
waitdun:
    /* L1 = {cmu, server}; Call-Lo(call) */
    /* L2 >= {conn's waitmu} iff ca_disownWait || !qrl */
    if (MT && !reliable)
      ilu_MXAClear(&grar, &gra.gra_alarmette);
    if (call->ca_irq || ILU_ERRNOK(*err)) {
      if (!call->ca_disownWait && !qrl
	  && !_ilu_ReleaseConnWait(conn, call, ilu_TRUE, err))
	return ilu_ProtocolException_Not;
      if (qrl) {
	qrl->rp_next = conn->co_replies;
	/*
	 * Somebody else (ilu_FinishCall?) should (but, doesn't yet)
	 * take care of reaping these.
	 */
	conn->co_replies = qrl;
      }
      if (!_ilu_ExitServerMutex(server, ilu_TRUE, err))
	return ilu_ProtocolException_Not;
      if (!ilu_ExitMutex(ilu_cmu, ilu_TRUE, err))
	return ilu_ProtocolException_Not;
      if (ILU_ERROK(*err))
	ILU_ERR_CONS1(interrupted, err, ilu_interruptSet, 0, (void) 6);
      return ilu_ProtocolException_Not;
    }
    if (prco || conn->co_pipeline) {	/* re-acquire the call mutex */
      _ilu_Assert(conn->co_nOuts > 0, "GetReply: conn->co_nOuts==0 a");
      if (call->ca_disownWait) {
	/*
	 * Violates locking order, but should be OK because we're
	 * single-threaded.
	 */
	if (!_ilu_EnterConnCall(conn, call, ilu_TRUE, err))
	  return ilu_ProtocolException_Not;
      } else {
	if (!qrl && !_ilu_ReleaseConnWait(conn, call, ilu_TRUE, err))
	  return ilu_ProtocolException_Not;
	if (!_ilu_EnterConnCallAndWait(conn, call, ilu_TRUE, ilu_FALSE,
				       ilu_TRUE, ilu_TRUE, &qrl, err))
	  return ilu_ProtocolException_Not;
      }
      _ilu_Assert(conn->co_nOuts > 0, "GetReply: conn->co_nOuts==0 b");
      conn->co_nOuts--;
    }
    if (!_ilu_TakeConnIO(conn, ilu_TRUE, err))	/* re-acquire the I/O
						 * mutex */
      return ilu_ProtocolException_Not;
    call->ca_ms = ilu_cmsHi;
    call->ca_msInput = ilu_TRUE;
    call->ca_dontWait = (qrl != NIL);
    /* L1=={cmu, server} && Call-Locking-Remnant(call, IHi) */
    if (!_ilu_PushAsNeeded(conn, err))
      goto abort2;
    if (connection_closish(conn) && !qrl) {
      last_sn_good = conn->co_lastSNgood;
      eof_last_sn = conn->co_last_sn;
      if (!_ilu_ExitServerMutex(server, ilu_TRUE, err))
	return ilu_ProtocolException_Not;
      if (!ilu_ExitMutex(ilu_cmu, ilu_TRUE, err))
	return ilu_ProtocolException_Not;
      if (last_sn_good)
	goto decideCompletion;
      return ilu_ProtocolException_LostConnection;
    }
    if ((prco || conn->co_pipeline) && !qrl) {
      qrl = _ilu_GetQueuedReply(call);
      call->ca_dontWait = (qrl != NIL);
      if (qrl && !call->ca_disownWait) {
	(void) _ilu_ReleaseConnWait(conn, call, ilu_TRUE, err);
	ILU_MUST_BE_SUCCESS(*err);
      }
    }
    if (!_ilu_ExitServerMutex(server, ilu_TRUE, err))
      return ilu_ProtocolException_Not;
    if (!ilu_ExitMutex(ilu_cmu, ilu_TRUE, err))
      return ilu_ProtocolException_Not;
    /* Call-Locking(call, IHi) */
    if (qrl) {
      FreeMessage(&call->ca_msg);
      protocol_resume_interp(proto, call, qrl->rp_queued);
      call->ca_ios = ilu_ciosIn;
      ilu_free(qrl);
      reply_src = "queue";
      goto interpd;
    }
    while (1) {
      /* Read all available replies */
      /* Call-Locking(call, IHi); !call->ca_dontWait */
      ilu_boolean     closeit = ilu_FALSE;
      ilu_ConnShutdownReason closeit_reason;
      ilu_ReadHeaderResultCode rhrc;
      rhrc = protocol_read_header(proto, call, &pkttype, &pktsn,
				  &eof_reason, &eof_last_sn, err);
      switch (rhrc) {
      case ilu_rhrc_ok:
	call->ca_ios = ilu_ciosIn;
	break;
      case ilu_rhrc_nothing:
	goto repliesRead;
      case ilu_rhrc_handled:
	break;
      case ilu_rhrc_eof:
	switch (eof_reason) {
	case ilu_ConnShutdownReason_LostProtocolSync:
	case ilu_ConnShutdownReason_MaxSerialNumber:
	case ilu_ConnShutdownReason_ResourceManagement:
	  conn->co_lastSNgood = last_sn_good = ilu_TRUE;
	  conn->co_last_sn = eof_last_sn;
	  break;
	case ilu_ConnShutdownReason_ReceivedEOF:
	case ilu_ConnShutdownReason_Relocating:
	case ilu_ConnShutdownReason_ProcessTermination:
	case ilu_ConnShutdownReason_BadEndpointID:
	default:
	  conn->co_lastSNgood = ilu_FALSE;
	}
	ILU_NOTE((CALL_DEBUG | INCOMING_DEBUG),
		 ("%-20.20s(%p over %p to \"%s\" #%lu try %u) =>"
		  " got EOF, %s=>(exec'd thru SN %lu)\n",
		  "ilu_GetReply", call, call_connection(call),
		  call_server_id(call),
		  (unsigned long) call_serial_number(call),
		  call->ca_tryIndex,
		  B2S(conn->co_lastSNgood),
		  (unsigned long) conn->co_last_sn));
	closeit = ilu_TRUE;
	closeit_reason = ilu_ConnShutdownReason_ReceivedEOF;
      case ilu_rhrc_error:
	ILU_ERR_SWITCH(*err) {
	  ILU_ERR_CASE(bad_param, v) {
	    ILU_HANDLED(*err);
	    (void) ILU_ERR_CONS1(internal, err, minor, ilu_im_unhandled,
				 0);
	  }
	  ILU_ERR_CASE(comm_failure, v) {
	    ILU_NOTE(CONNECTION_DEBUG,
		   ("%s %p to %s due to comm_failure, minor=%d.\n",
		    "Closing connection", conn,
		    server->sr_id, v->minor));
	    closeit = ilu_TRUE;
	    closeit_reason = ilu_ConnShutdownReason_LostProtocolSync;
	    ILU_HANDLED(*err);
	  }
	  ILU_ERR_ELSE
	    /* statement with no effect */ ;
	} ILU_ERR_ENDSWITCH;
	if (!closeit)
	  return ilu_ProtocolException_Not;
	else
	  break;
      default:
	_ilu_Assert(ilu_FALSE, "GetReply vs read_header");
      }
      if (closeit) {
	if (!ilu_ReEnterMutex(ilu_cmu, err))
	  return ilu_ProtocolException_Not;
	if (!_ilu_EnterServerMutex(server, ilu_TRUE, err))
	  return ilu_ProtocolException_Not;
	_ilu_CloseIoingConnection(conn, ilu_TRUE, closeit_reason);
	if (!_ilu_ExitServerMutex(server, ilu_TRUE, err))
	  return ilu_ProtocolException_Not;
	if (!ilu_ExitMutex(ilu_cmu, ilu_TRUE, err))
	  return ilu_ProtocolException_Not;
	if (last_sn_good)
	  goto decideCompletion;
	return ilu_ProtocolException_LostConnection;
      }
      if (pkttype == ilu_PacketType_Reply) {
	if (pktsn == call_serial_number(call)) {
	  FreeMessage(&call->ca_msg);
	  reply_src = "wire";
	  goto interpd;
	}
	ILU_NOTE((CALL_DEBUG | INCOMING_DEBUG),
		 ("%-20.20s(%p over %p to \"%s\" #%lu)"
		  " %s reply for SN #%lu.\n",
		  "ilu_GetReply", call, call_connection(call),
		  call_server_id(call),
		  (unsigned long) call_serial_number(call),
	   (prco || conn->co_pipeline) ? "queueing" : "discarding",
		  (unsigned long) pktsn));
	if ((prco || conn->co_pipeline)) {
	  ilu_ReplyList   r;
	  ilu_refany      queued2;
	  call->ca_ios = ilu_ciosNone;
	  queued2 = protocol_delay_interp(proto, call, err);
	  if (ILU_ERRNOK(*err))
	    return ilu_ProtocolException_Not;
	  r = (ilu_ReplyList) ilu_MallocE(sizeof(*r), err);
	  if (ILU_ERRNOK(*err))
	    return ilu_ProtocolException_Not;
	  r->rp_queued = queued2;
	  r->rp_SN = pktsn;
	  if (!_ilu_EnterServerMutex(server, ilu_TRUE, err))
	    return ilu_ProtocolException_Not;
	  r->rp_next = conn->co_replies;
	  conn->co_replies = r;
	  if (!_ilu_ExitServerMutex(server, ilu_TRUE, err))
	    return ilu_ProtocolException_Not;
	} else {
	  /* Reply to different call; discard. */
	  call->ca_ios = ilu_ciosNone;
	  if (!protocol_discard_input(proto, call, err))
	    return ilu_ProtocolException_Not;
	}
      } else {
	/* not a reply */
	call->ca_ios = ilu_ciosNone;
	if (!protocol_discard_input(proto, call, err))
	  return ilu_ProtocolException_Not;
      }
    }
repliesRead:
    /* Call-Locking(call, IHi) */
    ILU_MUST_BE_SUCCESS(*err);
    if (!reliable) {
      now = ilu_FineTime_Now();
      if (ilu_FineTime_Cmp(now, grandLimit) >= 0)
	break;
      if (ilu_FineTime_Cmp(now, limit) >= 0) {
	ILU_NOTE(CONNECTION_DEBUG,
	  ("GetReply: retransmitting request on conn %p\n", conn));
	if (!transport_send_whole_message(trans, &call->ca_msg, err))
	  return ilu_ProtocolException_Not;
	timeout = ClipAddTime(timeout, timeout, trans->tr_toN);
	limit = ClipAddTime(now, timeout, grandLimit);
      }
    }
  }
  return ilu_ProtocolException_RequestTimeout;
interpd:
  /* Call-Locking(call, IHi) */
  conn->co_lastWorking = ilu_FALSE;
  ans = protocol_interpret_reply(proto, call, estatus, err);
  ILU_ERR_SWITCH(*err) {
    ILU_ERR_CASE(relocate, rel) {
      ILU_ERRS((no_memory, internal / check)) merr;
      ilu_string      ts = _ilu_StringifyTinfo(rel->rel_tinfo, &merr);
      static char    *noT = "(not enough memory to stringify transport info)";
      *completion = ILU_COMPLETED_NO;
      if (!ILU_ERROK(merr))
	ts = noT;
      ILU_NOTE((CALL_DEBUG | INCOMING_DEBUG),
	 ("%-20.20s(%p over %p to \"%s\" #%lu try %u) => (from %s)"
	  " ans=%s estatus=%lu err=relocate(%s, %s, %s)\n",
	  "ilu_GetReply", call, call_connection(call),
	  call_server_id(call),
	  (unsigned long) call_serial_number(call),
	  call->ca_tryIndex,
	  reply_src, ilu_PEName(ans),
	  (unsigned long) *estatus,
	  ilu_RelocateScope_Name(rel->rel_scope),
	  rel->rel_pinfo, ts));
      if (ts != noT)
	ilu_free(ts);
      ILU_HANDLED(merr);
      if (call->ca_tryIndex < 4) {
	new_pinfo = rel->rel_pinfo;
	new_tinfo = rel->rel_tinfo;
	rel_scope = rel->rel_scope;
	rel->rel_pinfo = NIL;
	rel->rel_tinfo = NIL;
	ILU_HANDLED(*err);
	goto dorelocate;
      } else {
	ILU_HANDLED(*err);
	ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_relocate_loop, 0);
      }
    }
    ILU_SUCCESS_CASE {
      switch (ans) {
      case ilu_ProtocolException_Success:
	*completion = ILU_COMPLETED_YES;
	break;
      case ilu_ProtocolException_NoSuchClassAtServer:
      case ilu_ProtocolException_ClassVersionMismatch:
      case ilu_ProtocolException_NoSuchMethodOnClass:
      case ilu_ProtocolException_GarbageArguments:
      case ilu_ProtocolException_RequestRejected:
	*completion = ILU_COMPLETED_NO;
	break;
      case ilu_ProtocolException_Unknown:
      case ilu_ProtocolException_LostConnection:
      case ilu_ProtocolException_RequestTimeout:
	/* Leave it `maybe' */
	break;
      default:
	if (!ilu_Check(ilu_FALSE, err))
	  ans = ilu_ProtocolException_Not;
      }
    }
    ILU_ERR_ELSE
      /* do nothing */;
  } ILU_ERR_ENDSWITCH;
  ILU_NOTE((CALL_DEBUG | INCOMING_DEBUG),
	   ("%-20.20s(%p over %p to \"%s\" #%lu) => (from %s)"
	    " ans=%s completed=%s, estatus=%lu err=%s\n",
	    "ilu_GetReply", call, call_connection(call),
	    call_server_id(call),
	    (unsigned long) call_serial_number(call),
	    reply_src, ilu_PEName(ans),
	    ((*completion < 3) ? ilu_CompletionNames[*completion]
	     : "(invalid code!)"),
	    (unsigned long) *estatus, ILU_ERR_NAME(*err)));
  return ans;
dorelocate:
  {
    /* Call-Locking(call, IHi) */
    ilu_Connection  cur;
    int             openres;
    ilu_Protocol    old_proto = call_proto(call);
    call->ca_ios = ilu_ciosNone;
    if (!ilu_ReEnterMutex(ilu_cmu, err))
      return ilu_ProtocolException_Not;
    if (!_ilu_EnterServerMutex(server, ilu_TRUE, err))
      return ilu_ProtocolException_Not;
    if (rel_scope != ilu_relocate_call)
      conn->co_doomed = ilu_TRUE;
    if (proto != NIL && proto->pr_prefinish_call != NULLFN) {
      (void) (*proto->pr_prefinish_call) (call, err);
    }
    (void) _ilu_PushAndReleaseConnIO(conn, ilu_TRUE, err);
    (void) _ilu_QuickReleaseConnCall(conn, call, ilu_TRUE, err);
    if (!call->ca_dontWait && !call->ca_disownWait)
      (void) _ilu_ReleaseConnWait(conn, call, ilu_TRUE, err);
    /* L2 disjoint conn */
    call->ca_ms = ilu_cmsNo;
    conn->co_nCalls -= 1;
    call->ca_dontWait = ilu_FALSE;
    if (conn->co_nCalls == 0)
      conn->co_pipeline = NIL;
    if (conn->co_doomed && conn->co_nCalls == 0 && ILU_ERROK(*err)) {
      *err = _ilu_CloseConnection(conn,
				ilu_ConnShutdownReason_Relocating);
    }
    rewatch = (!connection_closed(conn) && !connection_incoming(conn)
	       && !_ilu_CanCondition() && conn->co_nOuts == 0);
    if (conn->co_serialer) {
      ilu_Serializer  si = conn->co_serialer;
      conn->co_serialer = NIL;
      si->si_conn = NIL;
      ILU_NOTE(CONNECTION_DEBUG,
	       ("ilu_GetReply(%p): dissociating serializer %p"
		" and connection %p.\n", call, si, conn));
    }
    _ilu_MaybeFreeConnection(conn);
    conn = call->ca_connection = NIL;
    call->ca_disownWait = ilu_FALSE;
    if (ILU_ERRNOK(*err))
      goto abort2;
    if (rewatch) {
      (void) _ilu_InnerSetConnectionInputHandler(oldConn, ReadExtraMsg,
						 oldConn, err);
      if (ILU_ERRNOK(*err))
	goto abort2;
    }
    for (cur = server_connections(server);
	 cur != NIL && !conn;
	 cur = connection_next(cur)) {
      if (!connection_incoming(cur) &&
	  strcmp(new_pinfo, cur->co_pinfo) == 0 &&
	  _ilu_CompareTinfo(new_tinfo, conn_tinfo(cur))) {
	if (!cur->co_mucall && !cur->co_serialer &&
	    !cur->co_doomed && !cur->co_closing &&
	    (!call->ca_si || !connection_concurrent(cur)) &&
	    (cur->co_pipeline == NIL
	     || cur->co_pipeline == call->ca_pl))
	  conn = cur;
      }
    }
    if (conn)
      openres = 3;
    else {
      new_tcr = _ilu_GetTransportCreator(new_tinfo, err);
      if (ILU_ERRNOK(*err))
	goto abort2;
      new_proto = _ilu_GetProtocolFromInfo(new_pinfo);
      if (!proto) {
	ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_ps, (void) 6);
	goto abort2;
      }
      if (new_proto != old_proto) {
	ILU_ERR_CONS1(imp_limit, err,
		      minor, ilu_ilm_redirect_cross_protocol,
		      (void) 6);
	goto abort2;
      }
      if ((new_proto->pr_needs_boundaried_transport
	   && (!new_tcr->tcr_boundaried)) ||
	  (new_proto->pr_needs_reliable_transport
	   && (!new_tcr->tcr_reliable))) {
	ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_tinfo,
		      (void) 6);
	goto abort2;
      };
      openres = OpenConn(new_pinfo, new_proto, new_tinfo, new_tcr,
			 server, call->ca_caller, &conn, new_conn,
			 err);
      if (conn && rel_scope == ilu_relocate_call)
	conn->co_doomed = ilu_TRUE;
      _ilu_CloseTCR(new_tcr);
    }
    switch (openres) {
    case 3:
      call->ca_connection = conn;
      (void) _ilu_EnterConnCall(conn, call, ilu_TRUE, err);
      (void) _ilu_TakeConnIO(conn, ilu_TRUE, err);
      if (ILU_ERROK(*err)) {
	call->ca_ms = ilu_cmsHi;
	call->ca_msInput = ilu_FALSE;
	conn->co_nCalls += 1;
	if (call->ca_si) {
	  conn->co_serialer = call->ca_si;
	  call->ca_si->si_conn = conn;
	  ILU_NOTE(CONNECTION_DEBUG,
		   ("ilu_GetReply(%p): associating serializer %p"
		    " and connection %p.\n",
		    call, call->ca_si, conn));
	}
	conn->co_pipeline = call->ca_pl;
	call->ca_tryIndex += 1;
	if ((*proto->pr_init_call) (call, err))
	  ILU_ERR_CONS1(transient, err, minor, ilu_tm_retry, (void) 0);
      }
    case 2:
      (void) _ilu_ExitServerMutex(server, ilu_TRUE, err);
    case 1:
      (void) ilu_ExitMutex(ilu_cmu, ilu_TRUE, err);
    case 0:
      return ilu_ProtocolException_Not;
    }
  }
abort2:
  (void) _ilu_ExitServerMutex(server, ilu_TRUE, err);
abort1:
  (void) ilu_ExitMutex(ilu_cmu, ilu_TRUE, err);
  return ilu_ProtocolException_Not;
decideCompletion:
  /* ILU_ERROK(*err) && Call-Invariant(call, err) */
  /* => Call-Locking(call) */
  if (call->ca_SN <= eof_last_sn) {
    ILU_NOTE((CALL_DEBUG | INCOMING_DEBUG),
	     ("%-20.20s(%p over %p to \"%s\" #%lu) =>"
	      " completed but lost connection.\n",
	      "ilu_GetReply", call, call_connection(call),
	      call_server_id(call),
	      (unsigned long) call_serial_number(call)));
    *completion = ILU_COMPLETED_YES;
    return ilu_ProtocolException_LostConnection;
  }
  /*
   * The call wasn't executed.  We want to retry, starting with
   * StartCall.  Rather than introduce a new result condition to
   * signal that, we re-use the existing result condition that
   * causes retry starting at arg sizing, doing the FinishCall and
   * StartCall right here.
   */
  *completion = ILU_COMPLETED_NO;
  ILU_NOTE((CALL_DEBUG | INCOMING_DEBUG),
	   ("%-20.20s(%p over %p to \"%s\" #%lu) =>"
	    " lost connection and not executed => retry.\n",
	    "ilu_GetReply", call, call_connection(call),
	    call_server_id(call),
	    (unsigned long) call_serial_number(call)));
  ilu_FinishCall(call, err);
  call->ca_connection = NIL;
  if (ILU_ERRNOK(*err))
    return ilu_ProtocolException_Not;
  {
    ilu_Call_s      ocall = *call;
    if (!ilu_FullStartCall(call, server, call->ca_intro_type,
			   call_method(call), call->ca_lang,
			   call->ca_caller, call->ca_si,
			   call->ca_pl, new_conn, err)) {
      *call = ocall;
      return ilu_ProtocolException_Not;
    }
  }
  return ILU_ERR_CONS1(transient, err, minor, ilu_tm_retry, ilu_ProtocolException_Not);
}


/*Main Invariant holds; L2 disjoint {conn's callmu, iomu, waitmu}*/
ilu_boolean
ilu_OutgoingConnectionThreadProc(ilu_Connection conn,
				 ILU_ERRS((IoErrs)) * err)
{
  ilu_Protocol    proto = connection_protocol(conn);
  ilu_Server      server = connection_server(conn);
  ilu_boolean     prco = protocol_concurrent(proto);
  ilu_Call_s      dummyCall;
  ILU_ERRS((interrupted)) lerr;
  ILU_ERRS((IoErrs)) ferr;
  ilu_boolean     ok, initted = ilu_FALSE;
  if (!ilu_CanCondition() || connection_incoming(conn) ||
      !conn->co_lsrCares)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, ilu_FALSE);
  ILU_NOTE(CONNECTION_DEBUG,
	   ("ilu_OutgoingConnectionThreadProc: tinfo is %s\n", conn_tinfo(conn)[0]));
  if (strncmp(conn_tinfo(conn)[0], "inmem", 5) == 0) {
    /* don't try to watch "inmem" transports */
    ILU_CLER(*err);
    return ilu_TRUE;
  }
  ILU_NOTE(CALL_DEBUG,
	   ("ILU runtime kernel starting to monitor connection %p for"
	    " unsolicited messages, with dummy call %p.\n",
	    conn, &dummyCall));
  BuildCall(&dummyCall, conn, server, 0, ilu_FALSE);
  dummyCall.ca_disownWait = ilu_FALSE;
  if (!ilu_EnterMutex(ilu_cmu, err))
    goto dun0;
  if (!_ilu_EnterServerMutex(server, ilu_FALSE, err))
    goto dun1;
  if (connection_closish(conn) || conn->co_doomed)
    goto dun2;
  if (!(*proto->pr_init_call) (&dummyCall, err))
    goto dun2;
  initted = ilu_TRUE;
  while (1) {
    /* L1 = {cmu, server}; Main Remnant; L2 disjoint conn's mutexes */
    while (1) {			/* wait for input outside of a call */
      while ((conn->co_nOuts || conn->co_waiting)
	     && !connection_closed(conn))
	if (!ilu_CMWait2(connection_cc(conn), server_lock(server),
			 ilu_cmu, &lerr))
	  goto dun2;
      if (connection_closed(conn))
	goto dun2;
      _ilu_TakeConnWait(conn, &dummyCall);
    interruptrepeat:
      ok = _ilu_BlockingWaitForInputOnConnection(conn, NIL, ilu_TRUE, &lerr);
      ILU_ERR_SWITCH(lerr) {
	ILU_SUCCESS_CASE /* no effect */ ;
	ILU_ERR_CASE(interrupted, v) {
	  /* likely due to a select generating EINTR - call the
	     _ilu_SignalCallbackHandler if one is registered, then 
	     just ignore and repeat */
	  if (_ilu_SignalCallbackHandler != NULLFN) {
	    ILU_NOTE(CONNECTION_DEBUG, ("%s", "(ilu_OutgoingConnectionThreadProc):"
					"  Calling signal handler callback\n"));
	    (*_ilu_SignalCallbackHandler)(_ilu_SignalCallbackHandlerArg);
	  }
	  ILU_HANDLED(lerr);
	  ILU_CLER(lerr);
	  goto interruptrepeat;
	}
	ILU_ERR_ELSE
	  if (!ilu_Check(ILU_ERROK(lerr), err))
	    /* LSR has no reason to abort this thread */
	    goto dun3;
      } ILU_ERR_ENDSWITCH; 
      if (!_ilu_ReleaseConnWait(conn, &dummyCall, ilu_TRUE, err))
	goto dun2;
      if (!_ilu_EnterConnCallAndWait(conn, &dummyCall, ilu_TRUE, ilu_FALSE,
				     ilu_TRUE, ilu_TRUE, NIL, err))
	goto dun2;
      if (!_ilu_EnterConnIO(conn, ilu_TRUE, err))
	goto dun4;
      if (connection_closed(conn))
	goto dun5;
      if (conn->co_nOuts == 0)
	goto doit;
      if (!_ilu_PushAndReleaseConnIO(conn, ilu_TRUE, err))
	goto dun4;
      if (!_ilu_QuickReleaseConnCall(conn, &dummyCall, ilu_TRUE, err))
	goto dun3;
      if (!_ilu_ReleaseConnWait(conn, &dummyCall, ilu_TRUE, err))
	goto dun2;
    }
  doit:
    /* L1 = {cmu, server}; L2 >= {conn's callmu, iomu, waitmu} */
    if (!ProcessExtraInput(conn, &dummyCall, err))
      goto dun5;
    if (!_ilu_PushAndReleaseConnIO(conn, ilu_TRUE, err))
      goto dun4;
    if (!_ilu_QuickReleaseConnCall(conn, &dummyCall, ilu_TRUE, err))
      goto dun3;
    if (!_ilu_ReleaseConnWait(conn, &dummyCall, ilu_TRUE, err))
      goto dun2;
  }
dun5:
  if (ILU_ERROK(*err) && proto->pr_prefinish_call != NULLFN) {
    (void) (*proto->pr_prefinish_call) (&dummyCall, err);
    ILU_ERR_SWITCH(*err) {
      ILU_SUCCESS_CASE
	/* no effect */ ;
      ILU_ERR_CASE(comm_failure, v) {
	/* just clear the error if we got a comm_failure (we'll be closing it
	   next anyway), and we don't want this error to be propogated out of this function */
	ILU_HANDLED(*err);
	ILU_CLER(*err);
      }
      ILU_ERR_ELSE
	/* no effect */ ;
    } ILU_ERR_ENDSWITCH; 
  }
#ifdef ENABLE_DEBUGGING
  {
    ilu_string      t;
    static ilu_string fallback = "(not enough memory to produce)";
    ILU_ERRS((no_memory, internal/check)) serr;
    t = _ilu_StringifyTinfo(conn_tinfo(conn), &serr);
    if (!t)
      t = fallback;
    ILU_NOTE(CONNECTION_DEBUG,
	     ("%s(%p, tr=%p, %s over %s): done, closing.\n",
	      "ilu_OutgoingConnectionThreadProc", conn,
	      conn->co_transport, conn->co_pinfo, t));
    if (t != fallback)
      ilu_free(t);
    if (ILU_ERROK(*err))
      *err = serr;
    else
      ILU_HANDLED(serr);
  }
#endif
  _ilu_CloseIoingConnection(conn, ilu_FALSE,
			    ilu_ConnShutdownReason_LostProtocolSync);
  (void) _ilu_ReleaseConnIO(conn, ilu_TRUE, err);
dun4:
  (void) _ilu_QuickReleaseConnCall(conn, &dummyCall, ilu_TRUE, err);
dun3:
  (void) _ilu_ReleaseConnWait(conn, &dummyCall, ilu_TRUE, err);
dun2:
  conn->co_lsrCares = ilu_FALSE;
  _ilu_MaybeFreeConnection(conn);
  (void) _ilu_ExitServerMutex(server, ilu_TRUE, err);
dun1:
  (void) ilu_ExitMutex(ilu_cmu, ilu_TRUE, err);
dun0:
  if (initted) {
    (*proto->pr_finish_call) (&dummyCall, &ferr);
    if (ILU_ERROK(*err))
      *err = ferr;
    else
      ILU_HANDLED(ferr);
  }
  ILU_NOTE(CALL_DEBUG,
	   ("ILU runtime kernel ceasing to monitor connection %p for"
	    " unsolicited messages, with dummy call %p.\n",
	    conn, &dummyCall));
  return ILU_ERROK(*err);
}

/* Main holds; L2 not further constrained */
ilu_boolean
ilu_InterruptCall(ilu_Call call,
		  ILU_ERRS((bad_locks, broken_locks,
			    bad_param)) * err)
{
  ilu_Connection  conn = call ? call_connection(call) : NIL;
  ilu_Server      s = call ? call_server(call) : NIL;
  ilu_boolean     ans;
  if (_ilu_CanCondition())
    return ILU_ERR_CONS1(bad_param, err, minor,
			 ilu_bpm_threading, ilu_FALSE);
  if (conn == NIL)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, ilu_FALSE);
  call->ca_irq = ilu_TRUE;
  ILU_NOTE(CALL_DEBUG, ("%-20.20s(%p over %p to \"%s\" #%lu)\n",
		     "ilu_InterruptCall", call,
		     call_connection(call), call_server_id(call),
		     (unsigned long) call_serial_number(call)));
  if (!ilu_EnterMutex(ilu_cmu, err))
    return ilu_FALSE;
  if (!_ilu_EnterServerMutex(s, ilu_FALSE, err))
    return ilu_FALSE;
  if (conn->co_closed)
    ans = ilu_TRUE;
  else {
    int             forMe = !conn->co_ioing;
    if (forMe)
      if (!_ilu_TakeConnIO(conn, ilu_TRUE, err))
	goto dun2;
    /* XXX make [un]marshalling kernel entry pts check ca_irq */
    ans = transport_interruptST(conn->co_transport, err);
    if (forMe)
      if (!_ilu_ReleaseConnIO(conn, ilu_TRUE, err))
	goto dun2;
  }
dun2:
  (void) _ilu_ExitServerMutex(s, ilu_TRUE, err);
  (void) ilu_ExitMutex(ilu_cmu, ilu_TRUE, err);
  return ans && ILU_ERROK(*err);
}

/*Call-Locking(call, OHi)*/

ilu_boolean
ilu_StartRequest(ilu_Call call, ilu_cardinal argSize,
		 ILU_ERRS((bad_param, IoErrs)) * err)
{
  ilu_Connection  conn = call_connection(call);
  ilu_boolean ans;

  if (conn == NIL)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, ilu_FALSE);
  if (!ILU_WaitRem(call))
    return ILU_ERR_CONS0(bad_locks, err, ilu_FALSE);
  ans = protocol_start_request(call_proto(call), call, argSize, err);
  ILU_NOTE(CALL_DEBUG,
	("%-20.20s(%p over %p to \"%s\" #%lu, argSize %lu) => %s\n",
	 "ilu_StartRequest", call, call_connection(call),
	 call_server_id(call),
	 (unsigned long) call_serial_number(call),
	 argSize, ILU_ERR_NAME(*err)));
  if (!ans)
    return ilu_FALSE;
  call->ca_ios = ilu_ciosOut;
  conn->co_next_sn++;	/* here, increment the SN */
  if (!ilu_Check(ILU_WaitRem(call), err))
    return ilu_FALSE;
  return ilu_TRUE;
}

/* L1 = {cmu}; Main Remnant; L2 otherwise unconstrained */
static int
PushPerConn(ilu_Connection conn, ilu_boolean forTime,
	    ILU_ERRS((IoErrs, bad_locks)) * err)
{
  ilu_Call_s      dummyCall = {0};
  ilu_boolean     tookCallmu = ilu_FALSE;
  ilu_Call        callmuDonor = NIL;
  ilu_Transport   t;
  if (!ilu_EnterServerMutex(conn->co_server, ilu_FALSE, err))
    return 1;
  ILU_NOTE(CONNECTION_DEBUG,
	   ("%-20.20s(%p, forTime=%s, mucall=%p, ioing=%s)\n",
	    "ILU ker PushPerConn", conn, B2S(forTime),
	    conn->co_mucall, B2S(conn->co_ioing)));
  t = conn->co_transport;
  if (forTime)
    conn->co_pushAlarmSet = ilu_FALSE;
  if (conn->co_mucall) {
    if (conn->co_ioing) {
      conn->co_pushme = ilu_TRUE;
      goto dun6;
    } else if (_ilu_CanCondition()) {
      /*
       * We're multi-threaded and some other thread holds the callmu
       * but not the iomu, and so is either waiting for a reply or
       * is running a method implementation.
       */
      if (connection_incoming(conn)) {
	/*
	 * We're on the server side; if possible, borrow the callmu
	 * and do the push; otherwise, set conn->co_pushme and let
	 * the server thread do it.
	 */
	if (conn->co_callmuBorrowable && conn->co_mucall
	    && !conn->co_mucall->ca_callmuBorrower) {
	  callmuDonor = conn->co_mucall;
	  conn->co_mucall = &dummyCall;
	  callmuDonor->ca_callmuBorrower = &dummyCall;
	  dummyCall.ca_callmuDonor = callmuDonor;
	} else {
	  conn->co_pushme = ilu_TRUE;
	  goto dun6;
	}
      } else {
	/*
	 * We're on the client side; make other thread go around the
	 * big loop in ilu_GetReply, which will do the push as part
	 * of releasing the iomu.
	 */
	ILU_ERRS((broken_locks, bad_param, internal)) lerr;
	conn->co_pushme = ilu_TRUE;
	if (!transport_disableWait(t, &lerr))
	  goto dun2x;
	while (conn->co_pushme && !connection_closish(conn)) {
	  if (!ilu_CMWait2(conn->co_cc, server_lock(conn->co_server),
			   ilu_cmu, err))
	    goto dun2;
	}
	if (!transport_enableWait(t, &lerr))
	  goto dun2x;
	goto dun6;
    dun2x:
	ILU_ERR_SWITCH(lerr) {
	  ILU_ERR_CASE(bad_param, x) {
	    ilu_Check(ilu_FALSE, err);
	    ILU_HANDLED(lerr);
	  }
	  ILU_ERR_CASE2(internal, broken_locks)
	    * err = lerr;
	} ILU_ERR_ENDSWITCH;
	goto dun2;
      }
    } else
      /*
       * We're single-threaded and a caller holds the callmu, but
       * not the iomu, and so is either waiting for a reply or
       * running a method implementation; go ahead and take the iomu
       * and do the push.
       */
      0;
  } else {
    conn->co_pushme = ilu_FALSE;
    if (!_ilu_EnterConnCall(conn, &dummyCall, ilu_TRUE, err))
      goto dun2;
    tookCallmu = ilu_TRUE;
  }
  /* XXX it would be better if we couldn't block here */
  if (!_ilu_EnterConnIO(conn, ilu_TRUE, err))
    goto dun3;
  conn->co_pushme = ilu_FALSE;
  if (!conn->co_closed) {
    if (!ilu_ExitServerMutex(conn->co_server, ilu_TRUE, err))
      return 1;
    if (!ilu_ExitMutex(ilu_cmu, ilu_TRUE, err))
      return 0;
    if (!(*t->tr_class->tc_push) (t, err))
      goto dun5;
    if (!ilu_ReEnterMutex(ilu_cmu, err))
      return 0;
    if (!ilu_EnterServerMutex(conn->co_server, ilu_TRUE, err))
      return 1;
  }
  if (!_ilu_PushAndReleaseConnIO(conn, ilu_TRUE, err))
    goto dun3;
  if (tookCallmu &&
      !_ilu_QuickReleaseConnCall(conn, &dummyCall, ilu_TRUE, err))
    goto dun2;
  if (callmuDonor &&
      ilu_Check(callmuDonor->ca_callmuBorrower == &dummyCall,
		err)) {
    conn->co_mucall = callmuDonor;
    callmuDonor->ca_callmuBorrower = dummyCall.ca_callmuDonor = NIL;
    if (!ilu_CondNotify(conn->co_cc, err))
      goto dun2;
  }
dun6:
  conn->co_batchCount -= !forTime;
  _ilu_MaybeFreeConnection(conn);
  goto dun2;
dun5:
  _ilu_CloseIoingConnection(conn, ilu_FALSE,
			  ilu_ConnShutdownReason_LostProtocolSync);
  (void) _ilu_ReleaseConnIO(conn, ilu_TRUE, err);
dun3:
  if (tookCallmu &&
      !_ilu_QuickReleaseConnCall(conn, &dummyCall, ilu_TRUE, err))
    goto dun2;
  if (callmuDonor &&
      ilu_Check(callmuDonor->ca_callmuBorrower == &dummyCall,
		err)) {
    conn->co_mucall = callmuDonor;
    callmuDonor->ca_callmuBorrower = dummyCall.ca_callmuDonor = NIL;
  }
dun2:
  if (!ilu_ExitServerMutex(conn->co_server, ilu_TRUE, err))
    return 1;
  return ILU_ERROK(*err) ? -1 : 1;
}

/* Main Invariant holds; L2 otherwise unconstrained */
ILU_PUBLIC      ilu_Batcher
ilu_CreateBatcher(ilu_FineTime timeout,
		  ilu_boolean pushable,
		  ILU_ERRS((internal, no_memory, no_resources,
			imp_limit, bad_locks, broken_locks)) * err)
{
  ilu_Batcher     b;
  ilu_HashTable   conns = NIL;
  ilu_Mutex       lock = ilu_CreateMutex("a", "batcher");
  ILU_ERRS((bad_locks, bad_param, internal)) lerr;
  static const ilu_FineTime zt = {0, 0};
  if (!lock)
    return ILU_ERR_CONS1(no_memory, err, nbytes, 0, NIL);
  if (pushable) {
    conns = ilu_hash_MakeNewTable(10, ilu_hash_HashPointer,
				  ilu_hash_PointerCompare);
    if (!conns) {
      ILU_ERR_CONS1(no_memory, err, nbytes, 0, NIL);
      goto dun1;
    }
  }
  if (!(b = ilu_MallocE(sizeof(*b), err)))
    goto dun2;
  b->bchr_lock = lock;
  b->bchr_TO = timeout;
  b->bchr_timed = (ilu_FineTime_Cmp(timeout, zt) > 0);
  b->bchr_pushable = pushable;
  b->bchr_lsrCares = ilu_TRUE;
  b->bchr_pushCount = 0;
  b->bchr_conns = conns;
  b->bchr_tmpVec = NIL;
  ILU_NOTE(CONNECTION_DEBUG,
	   ("ilu_CreateBatcher(%lu:%lu, %s) => %p\n",
	    (long unsigned) timeout.ft_s,
	    (long unsigned) timeout.ft_t,
	    B2S(pushable), b));
  return b;
dun2:
  if (pushable)
    ilu_hash_FreeHashTable(conns, NULLFN, NULLFN);
dun1:
  ilu_DestroyMutex(lock, &lerr);
  ILU_HANDLED(lerr);
  return NIL;
}

static ilu_boolean
  _ilu_ExitAndMaybeFreeBatcher(ilu_Batcher b,
			       ilu_boolean hard,
			       ILU_ERRS((bad_locks, broken_locks,
					 internal)) * err)
{
  ilu_boolean     killit;
  ILU_ERRS((bad_locks, bad_param, internal)) lerr;
  ilu_cardinal    nConns = 0;
  if (b->bchr_conns)
    nConns = ilu_hash_PairsInTable(b->bchr_conns);
  ILU_NOTE(CONNECTION_DEBUG | LOCK_DEBUG,
	   ("%s(%p, lsrCares=%s, pushes=%lu, conns=%lu)\n",
	    "ILU ker ExitBatcher", b, B2S(b->bchr_lsrCares),
	    (long unsigned) b->bchr_pushCount,
	    (long unsigned) nConns));
  killit = (!b->bchr_lsrCares && !b->bchr_pushCount && !nConns);
  if (!ilu_ExitMutex(b->bchr_lock, hard, err))
    return ilu_FALSE;
  if (!killit)
    return ilu_TRUE;
  if (b->bchr_conns)
    ilu_hash_FreeHashTable(b->bchr_conns, NULLFN, NULLFN);
  ilu_DestroyMutex(b->bchr_lock, &lerr);
  ILU_HANDLED(lerr);
  if (b->bchr_tmpVec)
    _ilu_vector_destroy(b->bchr_tmpVec, NULLFN);
  ilu_free(b);
  return ilu_TRUE;
}

/* Main Invariant holds */
ILU_PUBLIC      ilu_boolean
ilu_ReleaseBatcher(ilu_Batcher b,
		   ILU_ERRS((bad_locks, broken_locks,
			     bad_param)) * err)
{
  ilu_cardinal    nConns = 0;
  if (!b->bchr_lsrCares)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, ilu_FALSE);
  if (!ilu_EnterMutex(b->bchr_lock, err))
    return ilu_FALSE;
  if (b->bchr_conns)
    nConns = ilu_hash_PairsInTable(b->bchr_conns);
  ILU_NOTE(CONNECTION_DEBUG | LOCK_DEBUG,
	   ("%s(%p, pushes=%lu, conns=%lu)\n",
	    "ilu_ReleaseBatcher", b,
	    (long unsigned) b->bchr_pushCount,
	    (long unsigned) nConns));
  b->bchr_lsrCares = ilu_FALSE;
  if (!_ilu_ExitAndMaybeFreeBatcher(b, ilu_TRUE, err))
    return ilu_FALSE;
  return ILU_ERROK(*err);
}

/* Main Invariant holds; L2 otherwise unconstrained. */
ILU_PUBLIC      ilu_boolean
ilu_PushBatcher(ilu_Batcher b,
		ILU_ERRS((IoErrs, bad_locks)) * err)
{
  ilu_HashEnumerator_s he;
  ilu_refany      key, data;
  ilu_Vector      v = NIL;
  ilu_cardinal    i, n;
  if (!(b->bchr_lsrCares && b->bchr_pushable))
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, ilu_FALSE);
  if (!ilu_EnterMutex(ilu_cmu, err))
    return ilu_FALSE;
  if (!ilu_EnterMutex(b->bchr_lock, err))
    goto dun1;
  n = ilu_hash_PairsInTable(b->bchr_conns);
  ILU_NOTE(CONNECTION_DEBUG,
	   ("%-20.20s(%p, %lu conns active)\n",
	    "ilu_PushBatcher", b, (long unsigned) n));
  if (n > 0) {
    if (b->bchr_tmpVec) {
      v = b->bchr_tmpVec;
      v->ve_size = 0;
      b->bchr_tmpVec = NIL;
      if (!_ilu_vector_reserve(v, n, err))
	/* do nothing */;
    } else
      v = _ilu_vector_new(n, err);
    if (ILU_ERRNOK(*err))
      goto dun2;
    ilu_hash_BeginEnumeration(b->bchr_conns, &he);
    for (; ilu_hash_Next(&he, &key, &data);) {
      _ilu_vector_add(v, key, err);
      ILU_MUST_BE_SUCCESS(*err);
    }
    for (i = 0; i < n; i++)
      if (!ilu_Check(!!ilu_hash_RemoveFromTable(b->bchr_conns,
						v->ve_elements[i]),
		     err))
	goto dun2;
    b->bchr_pushCount += 1;
    if (!_ilu_ExitAndMaybeFreeBatcher(b, ilu_TRUE, err))
      goto dun1;
    for (i = 0; i < n; i++) {
      ilu_Connection  conn = (ilu_Connection) v->ve_elements[i];
      switch (PushPerConn(conn, ilu_FALSE, err)) {
      case -1:
	break;
      case 0:
	goto dun0;
      case 1:
	goto dun1;
      default:
	if (!ilu_Check(ilu_FALSE, err))
	  goto dun0;
      }
    }
    if (!ilu_ReEnterMutex(b->bchr_lock, err))
      goto dun1;
    b->bchr_pushCount -= 1;
    if (b->bchr_tmpVec)
      _ilu_vector_destroy(v, NULLFN);
    else
      b->bchr_tmpVec = v;
  }
dun2:
  if (!_ilu_ExitAndMaybeFreeBatcher(b, ilu_TRUE, err))
    goto dun1;
dun1:
  (void) ilu_ExitMutex(ilu_cmu, ilu_TRUE, err);
dun0:
  return ILU_ERROK(*err);
}

/* Main Invariant holds */
static void PushNow(ilu_private rock)
{
  ilu_Error       lerr = ILU_INIT_NO_ERR;
  ilu_Error      *err = &lerr;
  ilu_Connection  conn = (ilu_Connection) rock;
  if (!ilu_EnterMutex(ilu_cmu, err))
    goto dun0;
  switch (PushPerConn(conn, ilu_TRUE, err)) {
  case -1:
    break;
  case 0:
    goto dun0;
  case 1:
    goto dun1;
  default:
    if (!ilu_Check(ilu_FALSE, err))
      goto dun0;
  }
dun1:
  (void) ilu_ExitMutex(ilu_cmu, ilu_TRUE, err);
dun0:
  ILU_HANDLED(lerr);
  return;
}

ilu_boolean
ilu_FinishRequest(ilu_Call call,
		  ILU_ERRS((IoErrs)) * err)
{
  return ilu_FullFinishRequest(call, NIL, err);
}

static          ilu_boolean
FinishMessage(ilu_Call call, ilu_Batcher b, ilu_FineTime pushTime,
	      ILU_ERRS((IoErrs)) * err)
{
  ilu_Connection  conn = call_connection(call);
  if (!b->bchr_lsrCares)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, ilu_FALSE);
  if (!ilu_EnterMutex(b->bchr_lock, err))
    return ilu_FALSE;
  if (b->bchr_pushable) {
    if (!ilu_hash_FindInTable(b->bchr_conns, conn)) {
      if (!ilu_hash_AddToTable(b->bchr_conns, conn, conn)) {
	ILU_ERR_CONS1(no_memory, err, nbytes, 0, (void) 6);
	goto dun1;
      }
      conn->co_batchCount += 1;
    }
  }
  if (b->bchr_timed) {
    if (!ilu_EnterServerMutex(conn->co_server, ilu_FALSE, err))
      goto dun1;
    if (!conn->co_pushAlarm) {
      conn->co_pushAlarm = ilu_CreateAlarm();
      if (!conn->co_pushAlarm) {
	ILU_ERR_CONS1(no_memory, err, nbytes, 0, (void) 6);
	goto dun2;
      }
    }
    if (!(conn->co_pushAlarmSet &&
	  ilu_FineTime_Cmp(pushTime, conn->co_pushTime) >= 0)) {
      ilu_SetAlarm(conn->co_pushAlarm, conn->co_pushTime = pushTime,
		   PushNow, conn);
      conn->co_pushAlarmSet = ilu_TRUE;
      ILU_NOTE(CONNECTION_DEBUG,
	       ("%-20.20s(%p over %p to \"%s\" #%lu):  push alarm %p set to %ld.%lu\n",
		"ilu_FinishReq/Rep/Exn", call,
		call_connection(call), call_server_id(call),
		(unsigned long) call_serial_number(call),
		conn->co_pushAlarm, (long) conn->co_pushTime.ft_s,
		(unsigned long) conn->co_pushTime.ft_t));
    }
    if (!ilu_ExitServerMutex(conn->co_server, ilu_TRUE, err))
      goto dun1;
  }
  if (!_ilu_ExitAndMaybeFreeBatcher(b, ilu_TRUE, err))
    return ilu_FALSE;
  return ilu_TRUE;
dun2:
  if (!ilu_ExitServerMutex(conn->co_server, ilu_TRUE, err))
    goto dun1;
dun1:
  if (!ilu_ExitMutex(b->bchr_lock, ilu_TRUE, err))
    return ilu_FALSE;
  return ilu_FALSE;
}

ilu_boolean
ilu_FullFinishRequest(ilu_Call call,
		      ilu_Batcher b,	/* OPTIONAL */
		      ILU_ERRS((IoErrs)) * err)
{
  ilu_Connection  conn = call_connection(call);
  ilu_FineTime    now = {0, 0}, pushTime;
  if (conn == NIL)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, ilu_FALSE);
  if (!ILU_WaitRem(call))
    return ILU_ERR_CONS0(bad_locks, err, ilu_FALSE);
  call->ca_ios = ilu_ciosNone;
  conn->co_lastWorking = ilu_TRUE;
  if (b && b->bchr_timed) {
    now = ilu_FineTime_Now();
    pushTime = ilu_FineTime_Add(now, b->bchr_TO);
  }
  ILU_NOTE(CALL_DEBUG,
  ("%-20.20s(%p over %p to \"%s\" #%lu, b=%p(%lu:%lu) @%lu:%lu)\n",
   "ilu_FinishRequest", call, call_connection(call),
   call_server_id(call),
   (unsigned long) call_serial_number(call), b,
   (unsigned long) (b ? b->bchr_TO.ft_s : 0),
   (unsigned long) (b ? b->bchr_TO.ft_t : 0),
   (unsigned long) now.ft_s, (unsigned long) now.ft_t
   ));
  if (!protocol_finish_request(call_proto(call), call,
			       &call->ca_msg, !b, err))
    return ilu_FALSE;
  if (b && !FinishMessage(call, b, pushTime, err))
    return ilu_FALSE;
  if (!ilu_Check(ILU_WaitRem(call), err))
    return ilu_FALSE;
  return ilu_TRUE;
}

/**before: Call-Locking(call, IHi);
    after: Call-Invariant(call, err),
	   success => Call-Locking(call, VLo)*/
ilu_boolean
ilu_RequestRead(ilu_Call call,
		ILU_ERRS((IoErrs)) * err)
{
  ilu_Connection  conn = call_connection(call);
  ilu_Server      s = connection_server(conn);
  ilu_Protocol    p = call_proto(call);
  call->ca_ios = ilu_ciosNone;
  call->ca_pe = ilu_ProtocolException_Success;
  protocol_request_read(p, call, err);
  ILU_NOTE(CALL_DEBUG, ("%-20.20s(%p from %s #%lu) => %s\n",
		 "ilu_RequestRead", call, call_connection_id(call),
			(unsigned long) call_serial_number(call),
			ILU_ERR_NAME(*err)));
  if (ILU_ERRNOK(*err))
    return ilu_FALSE;
  if (!ilu_EnterMutex(ilu_cmu, err))
    goto dun0;
  if (!_ilu_EnterServerMutex(s, ilu_FALSE, err))
    goto dun1;
  if (!_ilu_PushAndReleaseConnIO(conn, ilu_FALSE, err))
    goto dun2;
  if (!ilu_Check(!(conn->co_callmuBorrowable ||
		   (conn->co_mucall &&
		    conn->co_mucall->ca_callmuBorrower)),
		 err))
    goto dun2;
  if (connection_concurrent(conn)) {
    conn->co_nOuts++;
    (void) _ilu_QuickReleaseConnCall(conn, call, ilu_FALSE, err);
  } else
    conn->co_callmuBorrowable = ilu_TRUE;
  (void) _ilu_ReleaseConnWait(conn, call, ilu_FALSE, err);
  call->ca_ms = ilu_cmsLo;
dun2:
  (void) _ilu_ExitServerMutex(s, ilu_TRUE, err);
dun1:
  (void) ilu_ExitMutex(ilu_cmu, ilu_TRUE, err);
dun0:
  return ILU_ERROK(*err);
}

/*Call-Locking(call, IHi)*/

ilu_boolean
ilu_ReplyRead(ilu_Call call,
	      ILU_ERRS((IoErrs)) * err)
{
  ilu_Protocol    p = call_proto(call);
  if (method_asynchronous(call_method(call)))
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, ilu_FALSE);
  if (!ILU_WaitRem(call))
    return ILU_ERR_CONS0(bad_locks, err, ilu_FALSE);
  call->ca_ios = ilu_ciosNone;
  protocol_reply_read(p, call, err);
  ILU_NOTE(CALL_DEBUG,
	("%-20.20s(%p over %p to \"%s\" #%lu) => %s\n",
	 "ilu_ReplyRead", call, call_connection(call),
	 call_server_id(call),
	 (unsigned long) call_serial_number(call),
	 ILU_ERR_NAME(*err)));
  if(ILU_ERRNOK(*err))
    return ilu_FALSE;
  if (!ilu_Check(ILU_WaitRem(call), err))
    return ilu_FALSE;
  return ilu_TRUE;
}

/*Main Invariant holds, L2 otherwise unconstrained*/

ilu_cardinal
ilu_BeginSizingReply(ilu_Call call,
		     ilu_boolean exns_possible,
		     ILU_ERRS((IoErrs)) * err)
{
  ilu_Protocol    proto = call_proto(call);
  if (!protocol_needs_sizing(connection_protocol(call_connection(call))))
    return (ILU_CLER(*err), 0);
  return ((*proto->pr_begin_sizing_reply) (call, exns_possible, err));
}

ilu_cardinal
ilu_BeginSizingException(ilu_Call call,
			 ilu_integer eindex,
			 ILU_ERRS((IoErrs)) * err)
{
  ilu_Protocol    proto = call_proto(call);
  _ilu_Assert(eindex != 0,
	      "BeginSizingException called with zero exceptionVal");
  if (!protocol_needs_sizing(proto))
    return (ILU_CLER(*err), 0);
  return ((*proto->pr_begin_sizing_exn)
	  (call,
	   (eindex > 0) ? eindex : 0,
	   (eindex > 0) ? ilu_ProtocolException_Success
	   : (ilu_ProtocolException) - eindex,
	   err));
}

/**before: Call-Locking(call, Lo);
    after: Call-Invariant(call, err),
	   success => Call-Locking(call, OHi). */

ilu_boolean
ilu_BeginReply(ilu_Call call, ilu_boolean exceptions_p,
	       ilu_cardinal argSize,
	       ILU_ERRS((bad_locks, IoErrs)) * err)
{
  ilu_Server      s = call_server(call);
  ilu_Connection  conn = call_connection(call);
  if (!ilu_EnterMutex(ilu_cmu, err))
    goto dun0;
  if (!_ilu_EnterServerMutex(s, ilu_FALSE, err))
    goto dun1;
  if (connection_concurrent(conn)) {
    if (!_ilu_EnterConnCall(conn, call, ilu_FALSE, err))
      goto dun2;
  } else {
    if (!ilu_Check(conn->co_callmuBorrowable, err))
      goto dun2;
    while (conn->co_mucall && conn->co_mucall->ca_callmuBorrower) {
      if (!ilu_CMWait2(conn->co_cc, server_lock(s), ilu_cmu, err))
	goto dun0;
    }
    conn->co_callmuBorrowable = ilu_FALSE;
  }
  if (!_ilu_EnterConnIO(conn, ilu_FALSE, err))
    goto dun3;
  if (connection_closed(conn)) {
    (void) ILU_ERR_CONS1(comm_failure, err, minor, ilu_cfm_conn_lost, 6);
    goto dun4;
  }
  if (connection_concurrent(conn)) {
    if (!ilu_Check(conn->co_nOuts > 0, err))
      goto dun2;
    conn->co_nOuts--;
  }
  call->ca_ms = ilu_cmsHi;
  call->ca_msInput = ilu_FALSE;
  ILU_NOTE(CALL_DEBUG, ("%-20.20s(%p from %s #%lu, argSize %lu)\n",
		     "ilu_BeginReply", call, call_connection_id(call),
		     (unsigned long) call_serial_number(call),
		     (unsigned long) argSize));
  if (protocol_begin_reply(call_proto(call), call, exceptions_p,
			   argSize, err))
    call->ca_ios = ilu_ciosOut;
  goto dun2;
dun4:
  (void) _ilu_ReleaseConnIO(conn, ilu_TRUE, err);
dun3:
  (void) _ilu_QuickReleaseConnCall(conn, call, ilu_TRUE, err);
dun2:
  (void) _ilu_ExitServerMutex(s, ilu_TRUE, err);
dun1:
  (void) ilu_ExitMutex(ilu_cmu, ilu_TRUE, err);
dun0:
  return ILU_ERROK(*err);
}

ilu_boolean
ilu_BeginException(ilu_Call call, ilu_integer exceptionVal,
		   ilu_cardinal argSize,
		   ILU_ERRS((bad_locks, IoErrs)) * err)
{
  ilu_Server      s = call_server(call);
  ilu_Connection  conn = call_connection(call);
  ilu_cardinal	exceptionCode = (exceptionVal <= 0) ? 0 : exceptionVal;
  ilu_ProtocolException sys_excn = ((exceptionVal > 0) ?
				    ilu_ProtocolException_Success :
			   (ilu_ProtocolException) - exceptionVal);
  _ilu_Assert(exceptionVal != 0,
	      "BeginException called with zero exceptionVal");
  if (conn == NIL)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, ilu_FALSE);
  if (!ilu_EnterMutex(ilu_cmu, err))
    goto dun0;
  if (!_ilu_EnterServerMutex(s, ilu_FALSE, err))
    goto dun1;
  if (connection_concurrent(conn)) {
    if (!_ilu_EnterConnCall(conn, call, ilu_FALSE, err))
      goto dun2;
  } else {
    if (!ilu_Check(conn->co_callmuBorrowable, err))
      goto dun2;
    while (conn->co_mucall && conn->co_mucall->ca_callmuBorrower) {
      if (!ilu_CMWait2(conn->co_cc, server_lock(s), ilu_cmu, err))
	goto dun0;
    }
    conn->co_callmuBorrowable = ilu_FALSE;
  }
  if (!_ilu_EnterConnIO(conn, ilu_FALSE, err))
    goto dun3;
  if (connection_closed(conn)) {
    (void) ILU_ERR_CONS1(comm_failure, err, minor, ilu_cfm_conn_lost, 6);
    goto dun4;
  }
  if (connection_concurrent(conn)) {
    if (!ilu_Check(conn->co_nOuts > 0, err))
      goto dun2;
    conn->co_nOuts--;
  }
  call->ca_ms = ilu_cmsHi;
  call->ca_msInput = ilu_FALSE;
  ILU_NOTE(CALL_DEBUG,
	("%-20.20s(%p from %s #%lu, excn %ld, argSize %lu)\n",
	 "ilu_BeginException", call, call_connection_id(call),
	 (unsigned long) call_serial_number(call),
	 (long) exceptionVal, (unsigned long) argSize));
  if (protocol_begin_exception(call_proto(call), call, exceptionCode,
			       sys_excn, argSize, err))
    call->ca_ios = ilu_ciosOut;
  goto dun2;
dun4:
  (void) _ilu_ReleaseConnIO(conn, ilu_TRUE, err);
dun3:
  (void) _ilu_QuickReleaseConnCall(conn, call, ilu_TRUE, err);
dun2:
  (void) ilu_ExitServerMutex(s, ilu_TRUE, err);
dun1:
  (void) ilu_ExitMutex(ilu_cmu, ilu_TRUE, err);
dun0:
  return ILU_ERROK(*err);
}

/* Call-Locking(call, OHi) */

ilu_boolean
ilu_FinishReply(ilu_Call call,
		ILU_ERRS((bad_locks, IoErrs)) * err)
{
  return ilu_FullFinishReply(call, NIL, err);
}

ilu_boolean
ilu_FullFinishReply(ilu_Call call,
		    ilu_Batcher b,	/* OPTIONAL */
		    ILU_ERRS((bad_locks, IoErrs)) * err)
{
  ilu_Connection  conn = call_connection(call);
  ilu_FineTime    now = {0, 0}, pushTime;
  if (conn == NIL)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, ilu_FALSE);
  call->ca_ios = ilu_ciosNone;
  if (b && b->bchr_timed) {
    now = ilu_FineTime_Now();
    pushTime = ilu_FineTime_Add(now, b->bchr_TO);
  }
  ILU_NOTE(CALL_DEBUG,
	   ("%-20.20s(%p from %s #%lu, b=%p(%lu:%lu) @%lu:%lu)\n",
	    "ilu_FinishReply", call,
	    call_connection_id(call),
	    (unsigned long) call_serial_number(call), b,
	    (unsigned long) (b ? b->bchr_TO.ft_s : 0),
	    (unsigned long) (b ? b->bchr_TO.ft_t : 0),
	    (unsigned long) now.ft_s, (unsigned long) now.ft_t
	    ));
  if (!protocol_finish_reply(call_proto(call), call, !b, err))
    return ilu_FALSE;
  if (b && !FinishMessage(call, b, pushTime, err))
    return ilu_FALSE;
  return ilu_TRUE;
}

ilu_boolean
ilu_FinishException(ilu_Call call,
		    ILU_ERRS((bad_locks, IoErrs)) * err)
{
  return ilu_FullFinishException(call, NIL, err);
}

ilu_boolean
ilu_FullFinishException(ilu_Call call,
			ilu_Batcher b,	/* OPTIONAL */
			ILU_ERRS((bad_locks, IoErrs)) * err)
{
  ilu_Connection  conn = call_connection(call);
  ilu_FineTime    now = {0, 0}, pushTime;
  if (conn == NIL)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, ilu_FALSE);
  call->ca_ios = ilu_ciosNone;
  if (b && b->bchr_timed) {
    now = ilu_FineTime_Now();
    pushTime = ilu_FineTime_Add(now, b->bchr_TO);
  }
  ILU_NOTE(CALL_DEBUG,
	   ("%-20.20s(%p from %s #%lu, b=%p(%lu:%lu) @%lu:%lu)\n",
	    "ilu_FinishException", call,
	    call_connection_id(call),
	    (unsigned long) call_serial_number(call), b,
	    (unsigned long) (b ? b->bchr_TO.ft_s : 0),
	    (unsigned long) (b ? b->bchr_TO.ft_t : 0),
	    (unsigned long) now.ft_s, (unsigned long) now.ft_t
	    ));
  if (!protocol_finish_exception(call_proto(call), call, !b, err))
    return ilu_FALSE;
  if (b && !FinishMessage(call, b, pushTime, err))
    return ilu_FALSE;
  return ilu_TRUE;
}

/*L1 >= {call's server}; L2 unconstrained */
ilu_boolean
_ilu_CacheCall(ilu_Call call, ilu_Message * reply,
	       ILU_ERRS((internal)) * err)
{
  ilu_Connection  conn = call_connection(call);
  ilu_Port        p = connection_port(conn);
  ilu_string peerinfo = conn_peerinfo(conn);
  ilu_integer     peerinfo_len = strlen(peerinfo);
  ilu_CachedCall *cc = p->po_call_cache;
  if (peerinfo_len >= MAX_CCPEER_LEN)
    return ILU_ERR_CONS1(internal, err, minor, ilu_im_tInfoLen, ilu_FALSE);
  if (cc[p->po_call_cache_finger].cc_replyMsg.msg_base != NIL)
    ilu_free(cc[p->po_call_cache_finger].cc_replyMsg.msg_base);
  strcpy(cc[p->po_call_cache_finger].cc_peerinfo, peerinfo);
  cc[p->po_call_cache_finger].cc_sn = call_serial_number(call);
  cc[p->po_call_cache_finger].cc_intro_type = call_intro_type(call);
  cc[p->po_call_cache_finger].cc_meth = call_method(call);
  cc[p->po_call_cache_finger].cc_replyMsg = *reply;
  p->po_call_cache_finger++;
  p->po_call_cache_finger %= p->po_call_cache_size;
  ILU_CLER(*err);
  return ilu_TRUE;
}

/* Call-Locking(call, Lo) */
ilu_boolean 
ilu_NoReply(ilu_Call call,
	    ILU_ERRS((bad_param, bad_locks, broken_locks)) * err)
{
  ILU_NOTE(CALL_DEBUG, ("%-20.20s(%p from %s #%lu)\n", "ilu_NoReply",
		     call, call_connection_id(call),
		     (unsigned long) call_serial_number(call)));
  return ILU_CLER(*err);
}

/**For sizing: L1, L2 unconstrained;
   For output: Call-Locking(call, OHi);
   For input:  Call-Locking(call, IHi)*/

/* ==================== optional ==================== */

void ilu_OutputOptional (ilu_Call call, ilu_boolean i,
			 ilu_Type the_type, ILU_ERRS((IoErrs)) * err)
{
  if (call->ca_irq)
    ILU_ERR_CONS1(interrupted, err, ilu_interruptSet, 0, (void) 6);
  else
    protocol_output_optional(call_proto(call), call, i, the_type, err);
  return;
}

void ilu_InputOptional (ilu_Call call, ilu_boolean *i,
			ilu_Type the_type, ILU_ERRS((IoErrs)) * err)
{
  if (call->ca_irq)
    ILU_ERR_CONS1(interrupted, err, ilu_interruptSet, 0, (void) 6);
  else
    protocol_input_optional(call_proto(call), call, i, the_type, err);
  return;
}

/*ARGSUSED*/
ilu_cardinal ilu_SizeOfOptional (ilu_Call call, ilu_boolean i,
				 ilu_Type the_type,
				 ILU_ERRS((IoErrs)) * err)
{
  ilu_cardinal    len;
  if (call->ca_irq || !protocol_needs_sizing(call_proto(call)))
    return (ILU_CLER(*err), 0);
  len = protocol_size_of_optional(call_proto(call), call, i, the_type, err);
  return (ILU_ERROK(*err) ? len : 0);
}

/* ==================== integer ==================== */

void 
ilu_OutputInteger(ilu_Call call, ilu_integer i,
		  ILU_ERRS((IoErrs)) * err)
{
  if (call->ca_irq)
    ILU_ERR_CONS1(interrupted, err, ilu_interruptSet, 0, (void) 6);
  else
    protocol_output_integer(call_proto(call), call, i, err);
  return;
}

void 
ilu_InputInteger(ilu_Call call, ilu_integer * i,
		 ILU_ERRS((IoErrs)) * err)
{
  if (call->ca_irq)
    ILU_ERR_CONS1(interrupted, err, ilu_interruptSet, 0, (void) 6);
  else
    protocol_input_integer(call_proto(call), call, i, err);
  return;
}

/*ARGSUSED*/
ilu_cardinal ilu_SizeOfInteger (ilu_Call call, ilu_integer i,
				      ILU_ERRS((IoErrs)) *err)
{
  ilu_cardinal len;
  if (call->ca_irq || !protocol_needs_sizing(call_proto(call)))
    return (ILU_CLER(*err), 0);
  len = protocol_size_of_integer(call_proto(call), call, i, err);
  return (ILU_ERROK(*err) ? len : 0);
}

/* ==================== cardinal ==================== */

void ilu_OutputCardinal (ilu_Call call, ilu_cardinal i,
		  ILU_ERRS((IoErrs)) * err)
{
  if (call->ca_irq)
    ILU_ERR_CONS1(interrupted, err, ilu_interruptSet, 0, (void) 6);
  else
    protocol_output_cardinal(call_proto(call), call, i, err);
  return;
}

void ilu_InputCardinal (ilu_Call call, ilu_cardinal *i,
		  ILU_ERRS((IoErrs)) * err)
{
  if (call->ca_irq)
    ILU_ERR_CONS1(interrupted, err, ilu_interruptSet, 0, (void) 6);
  else
    protocol_input_cardinal(call_proto(call), call, i, err);
  return;
}

/*ARGSUSED*/
ilu_cardinal ilu_SizeOfCardinal (ilu_Call call, ilu_cardinal i,
		  ILU_ERRS((IoErrs)) * err)
{
  ilu_cardinal    len;
  if (call->ca_irq || !protocol_needs_sizing(call_proto(call)))
    return (ILU_CLER(*err), 0);
  len = protocol_size_of_cardinal(call_proto(call), call, i, err);
  return (ILU_ERROK(*err) ? len : 0);
}

/* ==================== character ==================== */

void 
ilu_OutputCharacter(ilu_Call call, ilu_character i,
		    ILU_ERRS((IoErrs)) * err)
{
  if (call->ca_irq)
    ILU_ERR_CONS1(interrupted, err, ilu_interruptSet, 0, (void) 6);
  else
    protocol_output_character(call_proto(call), call, i, err);
  return;
}

void ilu_InputCharacter (ilu_Call call, ilu_character *i,
		  ILU_ERRS((IoErrs)) * err)
{
  if (call->ca_irq)
    ILU_ERR_CONS1(interrupted, err, ilu_interruptSet, 0, (void) 6);
  else
    protocol_input_character(call_proto(call), call, i, err);
  return;
}

/*ARGSUSED*/
ilu_cardinal ilu_SizeOfCharacter (ilu_Call call, ilu_character i,
		  ILU_ERRS((IoErrs)) * err)
{
  ilu_cardinal    len;
  if (call->ca_irq || !protocol_needs_sizing(call_proto(call)))
    return (ILU_CLER(*err), 0);
  len = protocol_size_of_character(call_proto(call), call, i, err);
  return (ILU_ERROK(*err) ? len : 0);
}

/* ==================== short integer ==================== */

void
ilu_OutputShortInteger(ilu_Call call, ilu_shortinteger i,
		       ILU_ERRS((IoErrs)) * err)
{
  if (call->ca_irq)
    ILU_ERR_CONS1(interrupted, err, ilu_interruptSet, 0, (void) 6);
  else
    protocol_output_short_integer(call_proto(call), call, i, err);
  return;
}

void
ilu_InputShortInteger(ilu_Call call, ilu_shortinteger * i,
		      ILU_ERRS((IoErrs)) * err)
{
  if (call->ca_irq)
    ILU_ERR_CONS1(interrupted, err, ilu_interruptSet, 0, (void) 6);
  else
    protocol_input_short_integer(call_proto(call), call, i, err);
  return;
}

/* ARGSUSED */
ilu_cardinal 
ilu_SizeOfShortInteger(ilu_Call call, ilu_shortinteger i,
		       ILU_ERRS((IoErrs)) * err)
{
  ilu_cardinal    len;
  if (call->ca_irq || !protocol_needs_sizing(call_proto(call)))
    return (ILU_CLER(*err), 0);
  len = protocol_size_of_short_integer(call_proto(call), call, i, err);
  return (ILU_ERROK(*err) ? len : 0);
}

/* ==================== long integer ==================== */

void ilu_OutputLongInteger (ilu_Call call, ilu_longinteger i,
		  ILU_ERRS((IoErrs)) * err)
{
  if (call->ca_irq)
    ILU_ERR_CONS1(interrupted, err, ilu_interruptSet, 0, (void) 6);
  else
    protocol_output_long_integer(call_proto(call), call, i, err);
  return;
}

void ilu_InputLongInteger (ilu_Call call, ilu_longinteger *i,
		  ILU_ERRS((IoErrs)) * err)
{
  if (call->ca_irq)
    ILU_ERR_CONS1(interrupted, err, ilu_interruptSet, 0, (void) 6);
  else
    protocol_input_long_integer(call_proto(call), call, i, err);
  return;
}

/*ARGSUSED*/
ilu_cardinal ilu_SizeOfLongInteger (ilu_Call call, ilu_longinteger i,
		  ILU_ERRS((IoErrs)) * err)
{
  ilu_cardinal    len;
  if (call->ca_irq || !protocol_needs_sizing(call_proto(call)))
    return (ILU_CLER(*err), 0);
  len = protocol_size_of_long_integer(call_proto(call), call, i, err);
  return (ILU_ERROK(*err) ? len : 0);
}

/* ==================== short cardinal ==================== */

void ilu_OutputShortCardinal (ilu_Call call, ilu_shortcardinal i,
		  ILU_ERRS((IoErrs)) * err)
{
  if (call->ca_irq)
    ILU_ERR_CONS1(interrupted, err, ilu_interruptSet, 0, (void) 6);
  else
    protocol_output_short_cardinal(call_proto(call), call, i, err);
  return;
}

void ilu_InputShortCardinal (ilu_Call call, ilu_shortcardinal *i,
		  ILU_ERRS((IoErrs)) * err)
{
  if (call->ca_irq)
    ILU_ERR_CONS1(interrupted, err, ilu_interruptSet, 0, (void) 6);
  else
    protocol_input_short_cardinal(call_proto(call), call, i, err);
  return;
}

/*ARGSUSED*/
ilu_cardinal ilu_SizeOfShortCardinal (ilu_Call call, ilu_shortcardinal i,
		  ILU_ERRS((IoErrs)) * err)
{
  ilu_cardinal len;
  if (call->ca_irq || !protocol_needs_sizing(call_proto(call)))
    return (ILU_CLER(*err), 0);
  len = protocol_size_of_short_cardinal(call_proto(call), call, i, err);
  return (ILU_ERROK(*err) ? len : 0);
}

/* ==================== long cardinal ==================== */

void ilu_OutputLongCardinal (ilu_Call call, ilu_longcardinal i,
		  ILU_ERRS((IoErrs)) * err)
{
  if (call->ca_irq)
    ILU_ERR_CONS1(interrupted, err, ilu_interruptSet, 0, (void) 6);
  else
    protocol_output_long_cardinal(call_proto(call), call, i, err);
  return;
}

void ilu_InputLongCardinal (ilu_Call call, ilu_longcardinal *i,
		  ILU_ERRS((IoErrs)) * err)
{
  if (call->ca_irq)
    ILU_ERR_CONS1(interrupted, err, ilu_interruptSet, 0, (void) 6);
  else
    protocol_input_long_cardinal(call_proto(call), call, i, err);
  return;
}

/*ARGSUSED*/
ilu_cardinal ilu_SizeOfLongCardinal (ilu_Call call, ilu_longcardinal i,
		  ILU_ERRS((IoErrs)) * err)
{
  ilu_cardinal len;
  if (call->ca_irq || !protocol_needs_sizing(call_proto(call)))
    return (ILU_CLER(*err), 0);
  len = protocol_size_of_long_cardinal(call_proto(call), call, i, err);
  return (ILU_ERROK(*err) ? len : 0);
}

/* ==================== enumeration ==================== */

void ilu_OutputEnum (ilu_Call call, ilu_shortcardinal i,
		     ilu_Type the_type, ILU_ERRS((IoErrs)) * err)
{
  if (call->ca_irq)
    ILU_ERR_CONS1(interrupted, err, ilu_interruptSet, 0, (void) 6);
  else
    protocol_output_enum_code(call_proto(call), call, i, the_type, err);
  return;
}

void ilu_InputEnum (ilu_Call call, ilu_shortcardinal *i,
		    ilu_Type the_type, ILU_ERRS((IoErrs)) * err)
{
  if (call->ca_irq)
    ILU_ERR_CONS1(interrupted, err, ilu_interruptSet, 0, (void) 6);
  else
    protocol_input_enum_code(call_proto(call), call, i, the_type, err);
  return;
}

/*ARGSUSED*/
ilu_cardinal ilu_SizeOfEnum (ilu_Call call, ilu_shortcardinal i,
			     ilu_Type the_type, ILU_ERRS((IoErrs)) * err)
{
  ilu_cardinal len;
  if (call->ca_irq || !protocol_needs_sizing(call_proto(call)))
    return (ILU_CLER(*err), 0);
  len = protocol_size_of_enum_code(call_proto(call), call, i, the_type, err);
  return (ILU_ERROK(*err) ? len : 0);
}

/* ==================== real ==================== */

void ilu_OutputReal (ilu_Call call, double i,
		  ILU_ERRS((IoErrs)) * err)
{
  if (call->ca_irq)
    ILU_ERR_CONS1(interrupted, err, ilu_interruptSet, 0, (void) 6);
  else
    protocol_output_real(call_proto(call), call, i, err);
  return;
}

void ilu_InputReal (ilu_Call call, double *i,
		  ILU_ERRS((IoErrs)) * err)
{
  if (call->ca_irq)
    ILU_ERR_CONS1(interrupted, err, ilu_interruptSet, 0, (void) 6);
  else
    protocol_input_real(call_proto(call), call, i, err);
  return;
}

/*ARGSUSED*/
ilu_cardinal ilu_SizeOfReal (ilu_Call call, double i,
		  ILU_ERRS((IoErrs)) * err)
{
  ilu_cardinal len;
  if (call->ca_irq || !protocol_needs_sizing(call_proto(call)))
    return (ILU_CLER(*err), 0);
  len = protocol_size_of_real(call_proto(call), call, i, err);
  return (ILU_ERROK(*err) ? len : 0);
}

/* ==================== short real ==================== */

void ilu_OutputShortReal (ilu_Call call, float i,
		  ILU_ERRS((IoErrs)) * err)
{
  if (call->ca_irq)
    ILU_ERR_CONS1(interrupted, err, ilu_interruptSet, 0, (void) 6);
  else
    protocol_output_short_real(call_proto(call), call, i, err);
  return;
}

void ilu_InputShortReal (ilu_Call call, float *i,
		  ILU_ERRS((IoErrs)) * err)
{
  if (call->ca_irq)
    ILU_ERR_CONS1(interrupted, err, ilu_interruptSet, 0, (void) 6);
  else
    protocol_input_short_real(call_proto(call), call, i, err);
  return;
}

/*ARGSUSED*/
ilu_cardinal ilu_SizeOfShortReal (ilu_Call call, float i,
		  ILU_ERRS((IoErrs)) * err)
{
  ilu_cardinal len;
  if (call->ca_irq || !protocol_needs_sizing(call_proto(call)))
    return (ILU_CLER(*err), 0);
  len = protocol_size_of_short_real(call_proto(call), call, i, err);
  return (ILU_ERROK(*err) ? len : 0);
}

/* ==================== long real ==================== */

void ilu_OutputLongReal (ilu_Call call, ilu_longreal i,
		  ILU_ERRS((IoErrs)) * err)
{
  if (call->ca_irq)
    ILU_ERR_CONS1(interrupted, err, ilu_interruptSet, 0, (void) 6);
  else
    protocol_output_long_real(call_proto(call), call, i, err);
  return;
}

void ilu_InputLongReal (ilu_Call call, ilu_longreal *i,
		  ILU_ERRS((IoErrs)) * err)
{
  if (call->ca_irq)
    ILU_ERR_CONS1(interrupted, err, ilu_interruptSet, 0, (void) 6);
  else
    protocol_input_long_real(call_proto(call), call, i, err);
  return;
}

/*ARGSUSED*/
ilu_cardinal ilu_SizeOfLongReal (ilu_Call call, ilu_longreal i,
		  ILU_ERRS((IoErrs)) * err)
{
  ilu_cardinal len;
  if (call->ca_irq || !protocol_needs_sizing(call_proto(call)))
    return (ILU_CLER(*err), 0);
  len = protocol_size_of_long_real(call_proto(call), call, i, err);
  return (ILU_ERROK(*err) ? len : 0);
}

/* ==================== byte ==================== */

void ilu_OutputByte (ilu_Call call, ilu_byte i,
		  ILU_ERRS((IoErrs)) * err)
{
  if (call->ca_irq)
    ILU_ERR_CONS1(interrupted, err, ilu_interruptSet, 0, (void) 6);
  else
    protocol_output_byte(call_proto(call), call, i, err);
  return;
}

void ilu_InputByte (ilu_Call call, ilu_byte *i,
		  ILU_ERRS((IoErrs)) * err)
{
  if (call->ca_irq)
    ILU_ERR_CONS1(interrupted, err, ilu_interruptSet, 0, (void) 6);
  else
    protocol_input_byte(call_proto(call), call, i, err);
  return;
}

/*ARGSUSED*/
ilu_cardinal ilu_SizeOfByte (ilu_Call call, ilu_byte i,
		  ILU_ERRS((IoErrs)) * err)
{
  ilu_cardinal len;
  if (call->ca_irq || !protocol_needs_sizing(call_proto(call)))
    return (ILU_CLER(*err), 0);
  len = protocol_size_of_byte(call_proto(call), call, i, err);
  return (ILU_ERROK(*err) ? len : 0);
}

/* ==================== short character ==================== */

void ilu_OutputShortCharacter (ilu_Call call, ilu_shortcharacter i,
			       ILU_ERRS((IoErrs)) * err)
{
  ilu_OutputByte (call, (ilu_byte) i, err);
}

void ilu_InputShortCharacter (ilu_Call call, ilu_shortcharacter *i,
			      ILU_ERRS((IoErrs)) * err)
{
  ilu_InputByte (call, (ilu_byte *) i, err);
}

/*ARGSUSED*/
ilu_cardinal ilu_SizeOfShortCharacter (ilu_Call call,
				       ilu_shortcharacter i,
				       ILU_ERRS((IoErrs)) * err)
{
  return (ilu_SizeOfByte (call, (ilu_byte) i, err));
}

/* ==================== boolean ==================== */

void ilu_OutputBoolean (ilu_Call call, ilu_boolean i,
		  ILU_ERRS((IoErrs)) * err)
{
  if (call->ca_irq)
    ILU_ERR_CONS1(interrupted, err, ilu_interruptSet, 0, (void) 6);
  else
    protocol_output_boolean(call_proto(call), call, i, err);
  return;
}

void ilu_InputBoolean (ilu_Call call, ilu_boolean *i,
		  ILU_ERRS((IoErrs)) * err)
{
  if (call->ca_irq)
    ILU_ERR_CONS1(interrupted, err, ilu_interruptSet, 0, (void) 6);
  else
    protocol_input_boolean(call_proto(call), call, i, err);
  return;
}

/*ARGSUSED*/
ilu_cardinal ilu_SizeOfBoolean (ilu_Call call, ilu_boolean i,
		  ILU_ERRS((IoErrs)) * err)
{
  ilu_cardinal len;
  if (call->ca_irq || !protocol_needs_sizing(call_proto(call)))
    return (ILU_CLER(*err), 0);
  len = protocol_size_of_boolean(call_proto(call), call, i, err);
  return (ILU_ERROK(*err) ? len : 0);
}

/* ==================== string ==================== */

void
ilu_OutputString(ilu_Call call, ilu_string s, ilu_cardinal len,
		 ilu_cardinal limit,
		 ILU_ERRS((IoErrs)) * err)
{
  if (call->ca_irq) {
    ILU_ERR_CONS1(interrupted, err, ilu_interruptSet, 0, (void) 6);
    return;
  }
  if ((s == NIL) || ((limit > 0) && (len > limit))) {
    ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0);
    return;
  }
#ifdef ENABLE_DEBUGGING
  if (ilu_DebugLevel != 0)
    {
      /* check to see that the string has no octet 0 chars in it,
	 as ILU string types don't allow that */
      if (len > 0 && strlen(s) < len)
	{
	  ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_string_null_char, 0);
	  return;
	}
    }
#endif
  protocol_output_string(call_proto(call), call, (void *) s, len, limit,
			 ILU_StringEncoding_latin1, ILU_StringEncoding_latin1,
			 err);
  return;
}

void
ilu_InputString(ilu_Call call, ilu_string * s, ilu_cardinal * len,
		ilu_cardinal limit,
		ILU_ERRS((IoErrs)) * err)
{
  ilu_cardinal encoding;

  if (call->ca_irq) {
    ILU_ERR_CONS1(interrupted, err, ilu_interruptSet, 0, (void) 6);
    return;
  }
  if (call_connection(call) == NIL) {
    ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0);
    return;
  }
  protocol_input_string(call_proto(call), call, (void **) s, len, limit,
			ILU_StringEncoding_latin1, &encoding, err);
#ifdef ENABLE_DEBUGGING
  if (ilu_DebugLevel != 0 && ILU_ERROK(*err))
    {
      /* check to see that the string has no octet 0 chars in it,
	 as ILU string types don't allow that */
      if (strlen(*s) < *len)
	ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_string_null_char, 0);
    }
#endif
  return;
}

/*ARGSUSED*/
ilu_cardinal ilu_SizeOfString (ilu_Call call, ilu_string i,
			       ilu_cardinal l, ilu_cardinal limit,
			       ILU_ERRS((IoErrs)) * err)
{
  ilu_cardinal    len;
  if (call_connection(call) == NIL)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0);
  else if ((i == NIL) || ((limit > 0) && (l > limit)))
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0);
  else if (call->ca_irq || !protocol_needs_sizing(call_proto(call)))
    return (ILU_CLER(*err), 0);
#ifdef ENABLE_DEBUGGING
  if (ilu_DebugLevel != 0)
    {
      /* check to see that the string has no octet 0 chars in it,
	 as ILU string types don't allow that */
      if (strlen(i) < l)
	{
	  ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_string_null_char, 0);
	  return 0;
	}
    }
#endif
  len = protocol_size_of_string(call_proto(call), call,
				(void *) i, l, limit,
				ILU_StringEncoding_latin1,
				ILU_StringEncoding_latin1,
				err);
  return (ILU_ERROK(*err) ? len : 0);
}

/* ==================== ilustring ==================== */

#define SWAP_HALFWORD(a)  ((((a) << 8) & 0xFF00) | \
			   ((((ilu_shortcardinal)(a)) >> 8) & 0x00FF))

#ifdef ILU_FIXED_POINT_SUPPORT
#ifdef LONG_CARDINAL_TYPE
#define SWAP_LONGWORD(a) ((((a) << 56)) | \
			  (((a) << 40) & 0x00ff000000000000) | \
			  (((a) << 24) & 0x0000ff0000000000) | \
			  (((a) <<  8) & 0x000000ff00000000) | \
			  ((((ilu_longcardinal)(a)) >> 56) & 0x00000000000000ff) | \
			  ((((ilu_longcardinal)(a)) >> 40) & 0x000000000000ff00) | \
			  ((((ilu_longcardinal)(a)) >> 24) & 0x0000000000ff0000) | \
			  ((((ilu_longcardinal)(a)) >>  8) & 0x00000000ff000000))
#else
#error "need way to swap longwords"
#endif
#endif /* def ILU_FIXED_POINT_SUPPORT */

#define SWAP_WORD(a) ( ((a) << 24) | \
                      (((a) << 8) & 0x00ff0000) | \
                      (((a) >> 8) & 0x0000ff00) | \
		      ((ilu_cardinal)(a) >>24) )

static          ilu_boolean
  UTF_8_to_Unicode_1_1 (ilu_shortcardinal ** tobuf,	/* OPTIONAL; if NIL no memory is allocated */
			ilu_bytes frombuf,
			ilu_cardinal *tolen, ilu_cardinal fromlen,
			ILU_ERRS((internal)) * err)
{
  ilu_cardinal count;
  ilu_bytes p2, p2lim;
  ilu_shortcardinal *p1, *p1lim;

  p2lim = frombuf + fromlen;
  for (count = 0, p2 = frombuf; p2 < p2lim;  count++) {
    if (((*p2 & 0xF8) == 0xF0)		/* not in Unicode-1-1 */
	|| ((*p2 & 0xFC) == 0xF8)
	|| ((*p2 & 0xFE) == 0xFC)) {
      return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_bad_character, ilu_FALSE);
    } else if ((*p2 & 0xF0) == 0xE0) {
      p2 += 3;
    } else if ((*p2 & 0xE0) == 0xC0) {
      p2 += 2;
    } else
      p2 += 1;
  }
  *tolen = count;
  if (tobuf == NIL)
    return ILU_CLER(*err);
  *tobuf = ilu_MallocE((count + 1) * sizeof(ilu_shortcardinal), err);
  if (ILU_ERRNOK(*err)) return ilu_FALSE;
  for (p2 = frombuf, p1 = *tobuf, p1lim = *tobuf + count;
       (p2 < p2lim) && (p1 < p1lim);) {
    if ((*p2 & 0xF0) == 0xE0) {
      *p1++ = ((p2[0] & 0x0F) << 12) | ((p2[1] & 0x3F) << 6)
	| (p2[2] & 0x3F);
      p2 += 3;
    } else if ((*p2 & 0xE0) == 0xC0) {
      *p1++ = ((p2[0] & 0x1F) << 6) | (p2[1] & 0x3F);
      p2 += 2;
    } else
      *p1++ = *p2++ & 0x7F;
  }
#ifndef WORDS_BIGENDIAN
  /* note that Unicode-1-1 is defined as big-endian */
  for (p1 = *tobuf;  p1 < p1lim;  p1++)
    *p1 = SWAP_HALFWORD(*p1);
#endif
  (*tobuf)[count] = 0;
  return ILU_CLER(*err);  
}

static          ilu_boolean
  Unicode_1_1_to_UTF_8(ilu_bytes *tobuf,		/* OPTIONAL; if NIL no memory is allocated */
		       ilu_shortcardinal * frombuf,
		       ilu_cardinal *tolen, ilu_cardinal fromlen,
		       ILU_ERRS((internal)) * err)
{
  ilu_cardinal count;
  ilu_shortcardinal val;
  ilu_shortcardinal *p2, *p2lim;
  ilu_bytes p1;

  p2lim = frombuf + fromlen;
  for (count = 0, p2 = frombuf;  p2 < p2lim;  p2++) {
    if (*p2 & 0xF800)
      count += 3;
    else if (*p2 & 0x0780)
      count += 2;
    else
      count += 1;
  }
  *tolen = count;
  if (tobuf == NIL)
    return ILU_CLER(*err);
  *tobuf = ilu_MallocE(count + 1, err);
  if (ILU_ERRNOK(*err)) return ilu_FALSE;
  for (p2 = frombuf, p1 = *tobuf; p2 < p2lim;  p2++) {
#ifdef WORDS_BIGENDIAN
    val = *p2;
#else
    val = SWAP_HALFWORD(*p2);
#endif
    if (val & 0xF800) {
      *p1++ = (char) (0xE0 | (val >> 12));
      *p1++ = (char) (0x80 | (((ilu_cardinal) (val & 0x0FC0)) >> 6));
      *p1++ = (char) (0x80 | (val & 0x3F));
    } else if (val & 0x0780) {
      *p1++ = (char) (0xC0 | (((ilu_cardinal) (val & 0x07C0)) >> 6));
      *p1++ = (char) (0x80 | (val & 0x003F));
    } else
      *p1++ = (char) (val & 0x7F);
  }
  *tobuf[count] = 0;
  return ILU_CLER(*err);  
}

void
ilu_OutputEString(ilu_Call call, ilu_bytes s, ilu_cardinal len,
		  ilu_cardinal limit,
		  ilu_cardinal expected_encoding, ilu_cardinal current_encoding,
		  ILU_ERRS((IoErrs)) * err)
{
  ilu_bytes final = s;
  ilu_cardinal finallen = len;
  ilu_cardinal encoding = current_encoding;
  if (call->ca_irq) {
    ILU_ERR_CONS1(interrupted, err, ilu_interruptSet, 0, (void) 6);
    return;
  }
  if (limit > 0 && len > limit) {
    ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0);
    return;
  }
  /* promote certain subset string encodings */
  if (((expected_encoding == ILU_StringEncoding_latin1) && (current_encoding == ILU_StringEncoding_US_ASCII)) ||
      ((expected_encoding == ILU_StringEncoding_UTF_8) && (current_encoding == ILU_StringEncoding_US_ASCII)))
    encoding = expected_encoding;
  else if ((expected_encoding == ILU_StringEncoding_UTF_8) && (current_encoding == ILU_StringEncoding_Unicode_1_1)) {
    if (!Unicode_1_1_to_UTF_8 (&final, (ilu_shortcardinal *) s, &finallen, len/sizeof(ilu_shortcardinal), err))
      return;
    encoding = expected_encoding;
  } else if ((expected_encoding == ILU_StringEncoding_Unicode_1_1) && (current_encoding == ILU_StringEncoding_UTF_8)) {
    if (!UTF_8_to_Unicode_1_1 ((ilu_shortcardinal **) &final, s, &finallen, len, err))
      return;
    finallen *= sizeof(ilu_shortcardinal);
    encoding = expected_encoding;
  }
  protocol_output_string(call_proto(call), call, final, finallen, limit,
			 expected_encoding, encoding, err);
  if (final != s)
    ilu_free(final);
  return;
}

void
  ilu_InputEString(ilu_Call call, ilu_bytes * s, ilu_cardinal * len,
		   ilu_cardinal limit,
		   ilu_cardinal expected_encoding, ilu_cardinal *actual_encoding,
		   ILU_ERRS((IoErrs)) * err)
{
  if (call->ca_irq) {
    ILU_ERR_CONS1(interrupted, err, ilu_interruptSet, 0, (void) 6);
    return;
  }
  if (call_connection(call) == NIL) {
    ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0);
    return;
  }
  protocol_input_string(call_proto(call), call, (void **) s, len, limit,
			expected_encoding, actual_encoding, err);
  if ((*actual_encoding == 0) && (expected_encoding == 0)) {
    ILU_ERR_CONS1(marshal, err, minor, ilu_mm_noCharset, 0);
    return;
  };
  return;
}

/*ARGSUSED*/
ilu_cardinal ilu_SizeOfEString (ilu_Call call, ilu_bytes s,
				ilu_cardinal l, ilu_cardinal limit,
				ilu_cardinal expected_encoding,
				ilu_cardinal current_encoding,
				ILU_ERRS((IoErrs)) * err)
{
  ilu_cardinal len = l;
  ilu_cardinal encoding = current_encoding;
  if (call_connection(call) == NIL)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0);
  else if (limit > 0 && l > limit)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0);
  else if (call->ca_irq || !protocol_needs_sizing(call_proto(call)))
    return (ILU_CLER(*err), 0);
  /* promote certain subset string encodings */
  if (((expected_encoding == ILU_StringEncoding_latin1) && (current_encoding == ILU_StringEncoding_US_ASCII)) ||
      ((expected_encoding == ILU_StringEncoding_UTF_8) && (current_encoding == ILU_StringEncoding_US_ASCII)))
    encoding = expected_encoding;
  else if ((expected_encoding == ILU_StringEncoding_UTF_8) && (current_encoding == ILU_StringEncoding_Unicode_1_1)) {
    if (!Unicode_1_1_to_UTF_8 (NIL, (ilu_shortcardinal *) s, &len, l/sizeof(ilu_shortcardinal), err))
      return 0;
    encoding = expected_encoding;
  } else if ((expected_encoding == ILU_StringEncoding_Unicode_1_1) && (current_encoding == ILU_StringEncoding_UTF_8)) {
    if (!UTF_8_to_Unicode_1_1 (NIL, s, &len, l, err))
      return 0;
    len *= sizeof(ilu_shortcardinal);
    encoding = expected_encoding;
  }
  len = protocol_size_of_string(call_proto(call), call, s, len, limit,
				expected_encoding, encoding, err);
  return (ILU_ERROK(*err) ? len : 0);
}

/* ==================== stringvec ==================== */

void 
ilu_OutputStringVec(ilu_Call call, ilu_string i, ilu_cardinal len,
		    ILU_ERRS((IoErrs)) * err)
{
  if (call->ca_irq)
    ILU_ERR_CONS1(interrupted, err, ilu_interruptSet, 0, (void) 6);
  else
    protocol_output_stringvec(call_proto(call), call, i, len, err);
  return;
}

void 
ilu_InputStringVec(ilu_Call call, ilu_string * i, ilu_cardinal len,
		   ILU_ERRS((IoErrs)) * err)
{
  if (call->ca_irq)
    ILU_ERR_CONS1(interrupted, err, ilu_interruptSet, 0, (void) 6);
  else
    protocol_input_stringvec(call_proto(call), call, i, len, err);
  return;
}

/*ARGSUSED*/
ilu_cardinal ilu_SizeOfStringVec (ilu_Call call, ilu_string i,
				  ilu_cardinal len,
				  ILU_ERRS((IoErrs)) * err)
{
  ilu_cardinal    len2;
  if (call->ca_irq || !protocol_needs_sizing(call_proto(call)))
    return (ILU_CLER(*err), 0);
  len2 = protocol_size_of_stringvec(call_proto(call), call, i, len,
				    err);
  return (ILU_ERROK(*err) ? len2 : 0);
}

/* ==================== wstring ==================== */

ilu_cardinal _ilu_SizeOfWString (ilu_Call call, ilu_wstring s,
				ilu_cardinal l1	/* size of wstring */,
				ilu_cardinal limit,
				ILU_ERRS((IoErrs)) *err)
{
  ilu_string      buf;
  register ilu_string p1;
  register ilu_character *p2;
  ilu_cardinal    len, size;
  ilu_character  *p2Limit;

  size = protocol_size_of_cardinal(call_proto(call), call, l1, err);
  if (ILU_ERRNOK(*err))
    return 0;
  p2Limit = s + l1;
  buf = ilu_malloc(l1 * 3);
  for (p1 = buf, p2 = s; p2 < p2Limit;) {
    if (*p2 & 0xF800) {
      *p1++ = (char) (0xE0 | (*p2 >> 12));
      *p1++ = (char) (0x80 | (((ilu_cardinal) (*p2 & 0x0FC0)) >> 6));
      *p1++ = (char) (0x80 | (*p2++ & 0x3F));
    } else if (*p2 & 0x0780) {
      *p1++ = (char) (0xC0 | ((ilu_cardinal) (*p2 & 0x07C0)) >> 6);
      *p1++ = (char) (0x80 | (*p2++ & 0x003F));
    } else
      *p1++ = (char) (*p2++ & 0x7F);
  }
  len = p1 - buf;
  size += protocol_size_of_bytes(call_proto(call), call,
				 (ilu_bytes) buf, len, 0, err);
  ilu_free(buf);
  return (ILU_ERROK(*err) ? size : 0);
}

void _ilu_OutputWString (ilu_Call call, ilu_wstring s, ilu_cardinal l1,
			 ilu_cardinal limit, ILU_ERRS((IoErrs)) * err)
{
  ilu_string      buf;
  register ilu_string p1;
  register ilu_character *p2, *p2Limit;
  ilu_cardinal    len;

  protocol_output_cardinal(call_proto(call), call, l1, err);
  if (ILU_ERRNOK(*err))
    return;
  p2Limit = s + l1;
  buf = (ilu_string) ilu_malloc(l1 * 3);
  for (p1 = buf, p2 = s; p2 < p2Limit;) {
    if (*p2 & 0xF800) {
      *p1++ = (char) (0xE0 | (*p2 >> 12));
      *p1++ = (char) (0x80 | (((ilu_cardinal) (*p2 & 0x0FC0)) >> 6));
      *p1++ = (char) (0x80 | (*p2++ & 0x3F));
    } else if (*p2 & 0x0780) {
      *p1++ = (char) (0xC0 | (((ilu_cardinal) (*p2 & 0x07C0)) >> 6));
      *p1++ = (char) (0x80 | (*p2++ & 0x003F));
    } else
      *p1++ = (char) (*p2++ & 0x7F);
  }
  len = p1 - buf;
  protocol_output_bytes(call_proto(call), call, (ilu_bytes) buf,
			len, 0, err);
  ilu_free(buf);
  return;
}

static          ilu_boolean
  ilu_UTF8toUnicode_1_1(ilu_shortcardinal ** tobuf, ilu_bytes frombuf,
			ilu_cardinal *tolen, ilu_cardinal fromlen,
			ILU_ERRS((internal)) * err)
{
  ilu_cardinal count;
  ilu_bytes p2, p2lim;
  ilu_shortcardinal *p1, *p1lim;

  p2lim = frombuf + fromlen;
  for (count = 0, p2 = frombuf; p2 < p2lim;  count++) {
    if (((*p2 & 0xF8) == 0xF0)		/* not in Unicode-1-1 */
	|| ((*p2 & 0xFC) == 0xF8)
	|| ((*p2 & 0xFE) == 0xFC)) {
      return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_bad_character, ilu_FALSE);
    } else if ((*p2 & 0xF0) == 0xE0) {
      p2 += 3;
    } else if ((*p2 & 0xE0) == 0xC0) {
      p2 += 2;
    } else
      p2 += 1;
  }
  *tobuf = ilu_MallocE((count + 1) * sizeof(ilu_shortcardinal), err);
  if (ILU_ERRNOK(*err)) return ilu_FALSE;
  for (p2 = frombuf, p1 = *tobuf, p1lim = *tobuf + count;
       (p2 < p2lim) && (p1 < p1lim);) {
    if ((*p2 & 0xF0) == 0xE0) {
      *p1++ = ((p2[0] & 0x0F) << 12) | ((p2[1] & 0x3F) << 6)
	| (p2[2] & 0x3F);
      p2 += 3;
    } else if ((*p2 & 0xE0) == 0xC0) {
      *p1++ = ((p2[0] & 0x1F) << 6) | (p2[1] & 0x3F);
      p2 += 2;
    } else
      *p1++ = *p2++ & 0x7F;
  }
  (*tobuf)[count] = 0;
  *tolen = count;
  return ILU_CLER(*err);  
}

static          ilu_boolean
  ilu_Unicode_1_1_to_UTF8(ilu_bytes *tobuf, ilu_shortcardinal * frombuf,
			  ilu_cardinal *tolen, ilu_cardinal fromlen,
			  ILU_ERRS((internal)) * err)
{
  ilu_cardinal count;
  ilu_shortcardinal *p2, *p2lim;
  ilu_bytes p1;

  p2lim = frombuf + fromlen;
  for (count = 0, p2 = frombuf;  p2 < p2lim;  p2++) {
    if (*p2 & 0xF800)
      count += 3;
    else if (*p2 & 0x0780)
      count += 2;
    else
      count += 1;
  }
  *tobuf = (ilu_bytes) ilu_MallocE(count + 1, err);
  if (ILU_ERRNOK(*err)) return ilu_FALSE;
  for (p2 = frombuf, p1 = *tobuf; p2 < p2lim; ) {
    if (*p2 & 0xF800) {
      *p1++ = (char) (0xE0 | (*p2 >> 12));
      *p1++ = (char) (0x80 | (((ilu_cardinal) (*p2 & 0x0FC0)) >> 6));
      *p1++ = (char) (0x80 | (*p2++ & 0x3F));
    } else if (*p2 & 0x0780) {
      *p1++ = (char) (0xC0 | (((ilu_cardinal) (*p2 & 0x07C0)) >> 6));
      *p1++ = (char) (0x80 | (*p2++ & 0x003F));
    } else
      *p1++ = (char) (*p2++ & 0x7F);
  }
  *tobuf[count] = 0;
  *tolen = count;
  return ILU_CLER(*err);  
}

static          ilu_boolean
UTF2Decode(ilu_character * tobuf, ilu_string frombuf,
	   ilu_cardinal tolen, ilu_cardinal fromlen,
	   ILU_ERRS((internal)) * err)
{
  ilu_string      p2 = frombuf, p2lim = frombuf + fromlen;
  ilu_character  *p1 = tobuf;
  ilu_cardinal    count;
  for (count = 0; count < tolen && p2 < p2lim; count++) {
    if ((*p2 & 0xF0) == 0xE0) {
      *p1++ = ((p2[0] & 0x0F) << 12) | ((p2[1] & 0x3F) << 6)
	| (p2[2] & 0x3F);
      p2 += 3;
    } else if ((*p2 & 0xE0) == 0xC0) {
      *p1++ = ((p2[0] & 0x1F) << 6) | (p2[1] & 0x3F);
      p2 += 2;
    } else
      *p1++ = *p2++ & 0x7F;
  }
  if (count == tolen && p2 == p2lim)
    return ILU_CLER(*err);
  else
    return ILU_ERR_CONS1(marshal, err, minor, ilu_mm_utf2Len, ilu_FALSE);
}

void
_ilu_InputWString(ilu_Call call, ilu_wstring * s, ilu_cardinal * l,
		  ilu_cardinal limit, ILU_ERRS((IoErrs)) * err)
{
  ilu_string      buf = NIL;
  ilu_character  *ubuf = NIL;
  ilu_cardinal    len = 0, len2;

  protocol_input_cardinal(call_proto(call), call, &len, err);
  if (ILU_ERRNOK(*err))
    return;
  protocol_input_bytes(call_proto(call), call, (ilu_bytes *) & buf,
		       &len2, 0, err);
  if (ILU_ERRNOK(*err))
    return;
  ubuf = (ilu_wstring) ilu_MallocE((len + 1) * sizeof(ilu_character),
				   err);
  if (ubuf == NIL)
    return;
  if (!UTF2Decode(ubuf, buf, len, len2, err)) {
    ilu_free(buf);
    ilu_free(ubuf);
    *s = NIL;
    *l = 0;
    return;
  }
  ilu_free(buf);
  ubuf[len] = 0;
  *s = ubuf;
  *l = len;
  return;
}

ilu_cardinal ilu_SizeOfWString (ilu_Call call, ilu_wstring s,
				ilu_cardinal l1	/* size of wstring */,
				ilu_cardinal limit,
				ILU_ERRS((IoErrs)) *err)
{
  ilu_cardinal    len;

  if (call_connection(call) == NIL)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0);
  if (limit > 0 && l1 > limit)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0);
  else if (call->ca_irq || !protocol_needs_sizing(call_proto(call)))
    return (ILU_CLER(*err), 0);
  len = protocol_size_of_wstring(call_proto(call), call, s, l1, limit,
				 err);
  return (ILU_ERROK(*err) ? len : 0);
}

void ilu_OutputWString (ilu_Call call, ilu_wstring s, ilu_cardinal l1,
			ilu_cardinal limit, ILU_ERRS((IoErrs)) * err)
{

  if (call_connection(call) == NIL) {
    ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0);
    return;
  }
  if (limit > 0 && l1 > limit) {
    ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0);
    return;
  }
  if (call->ca_irq) {
    ILU_ERR_CONS1(interrupted, err, ilu_interruptSet, 0, (void) 6);
    return;
  }
  protocol_output_wstring(call_proto(call), call, s, l1, limit, err);
  return;
}

void
ilu_InputWString(ilu_Call call, ilu_wstring * s, ilu_cardinal * l,
		 ilu_cardinal limit, ILU_ERRS((IoErrs)) * err)
{
  if (call_connection(call) == NIL) {
    ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0);
    return;
  }
  if (call->ca_irq) {
    ILU_ERR_CONS1(interrupted, err, ilu_interruptSet, 0, (void) 6);
    return;
  }
  protocol_input_wstring(call_proto(call), call, s, l, limit, err);
  return;
}

/* ==================== wstringvec ==================== */

void 
  _ilu_OutputWStringVec(ilu_Call call, ilu_wstring i,
			ilu_cardinal len, ILU_ERRS((IoErrs)) * err)
{
  ilu_OutputWString(call, i, len, len, err);
}

void
  _ilu_InputWStringVec(ilu_Call call, ilu_wstring * s,
		       ilu_cardinal len, ILU_ERRS((IoErrs)) * err)
{
  ilu_string      buf = NIL;
  ilu_cardinal    len1 = 0, len2, junk;
  ilu_boolean     isnew = (*s) == NIL;

  protocol_input_cardinal(call_proto(call), call, &len1, err);
  if (ILU_ERRNOK(*err))
    return;
  if (len1 != len) {
    ILU_ERR_CONS1(marshal, err, minor, ilu_mm_wronglen, 6);
    return;
  }
  protocol_input_string(call_proto(call), call, (void **) &buf, &len2,
			0, ILU_StringEncoding_latin1, &junk,
			err);
  if (ILU_ERRNOK(*err))
    return;
  if (isnew) {
    *s = (ilu_wstring) ilu_MallocE(sizeof(ilu_character) * len + 1,
				   err);
    if (*s == NIL)
      return;
  }
  if (!UTF2Decode(*s, buf, len, len2, err) && isnew) {
    ilu_free(buf);
    ilu_free(*s);
    *s = NIL;
  }
  ilu_free(buf);
  return;
}

ilu_cardinal
  _ilu_SizeOfWStringVec (ilu_Call call, ilu_wstring i,
			 ilu_cardinal len, ILU_ERRS((IoErrs)) * err)
{
  return (ilu_SizeOfWString (call, i, len, len, err));
}

void 
ilu_OutputWStringVec(ilu_Call call, ilu_wstring i, ilu_cardinal len,
		     ILU_ERRS((IoErrs)) * err)
{
  if (call->ca_irq)
    ILU_ERR_CONS1(interrupted, err, ilu_interruptSet, 0, (void) 6);
  else
    protocol_output_wstringvec(call_proto(call), call, i, len, err);
}

void
ilu_InputWStringVec(ilu_Call call, ilu_wstring * s, ilu_cardinal len,
		    ILU_ERRS((IoErrs)) * err)
{
  if (call->ca_irq)
    ILU_ERR_CONS1(interrupted, err, ilu_interruptSet, 0, (void) 6);
  else
    protocol_input_wstringvec(call_proto(call), call, s, len, err);
}

ilu_cardinal ilu_SizeOfWStringVec (ilu_Call call, ilu_wstring i,
				   ilu_cardinal len,
				   ILU_ERRS((IoErrs)) * err)
{
  if (call->ca_irq || !protocol_needs_sizing(call_proto(call)))
    return (ILU_CLER(*err), 0);
  return (protocol_size_of_wstringvec(call_proto(call), call, i,
				      len, err));
}

/* ==================== bytes ==================== */

void 
ilu_OutputBytes(ilu_Call call, ilu_bytes i, ilu_cardinal len,
		ilu_cardinal limit, ILU_ERRS((IoErrs)) * err)
{
  if (call_connection(call) == NIL) {
    ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0);
    return;
  }
  if (limit > 0 && len > limit)
    ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0);
  else if (call->ca_irq)
    ILU_ERR_CONS1(interrupted, err, ilu_interruptSet, 0, (void) 6);
  else
    protocol_output_bytes(call_proto(call), call, i, len, limit, err);
  return;
}

void 
ilu_InputBytes(ilu_Call call, ilu_bytes * i, ilu_cardinal * len,
	       ilu_cardinal limit, ILU_ERRS((IoErrs)) * err)
{
  if (call->ca_irq)
    ILU_ERR_CONS1(interrupted, err, ilu_interruptSet, 0, (void) 6);
  else
    protocol_input_bytes(call_proto(call), call, i, len, limit, err);
  return;
}

/*ARGSUSED*/
ilu_cardinal ilu_SizeOfBytes (ilu_Call call, ilu_bytes i,
			      ilu_cardinal len, ilu_cardinal limit,
			      ILU_ERRS((IoErrs)) * err)
{
  ilu_cardinal    len2;
  if (call_connection(call) == NIL)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0);
  if (limit > 0 && len > limit)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0);
  else if (call->ca_irq || !protocol_needs_sizing(call_proto(call)))
    return (ILU_CLER(*err), 0);
  len2 = protocol_size_of_bytes(call_proto(call), call, i, len, limit,
				err);
  return (ILU_ERROK(*err) ? len2 : 0);
}

/* ==================== opaque ==================== */

void 
ilu_OutputOpaque(ilu_Call call, ilu_opaque i,
		 ilu_cardinal len, ILU_ERRS((IoErrs)) * err)
{
  if (call->ca_irq)
    ILU_ERR_CONS1(interrupted, err, ilu_interruptSet, 0, (void) 6);
  else
    protocol_output_opaque(call_proto(call), call, i, len, err);
  return;
}

void 
ilu_InputOpaque(ilu_Call call, ilu_opaque * i,
		ilu_cardinal len, ILU_ERRS((IoErrs)) * err)
{
  if (call->ca_irq)
    ILU_ERR_CONS1(interrupted, err, ilu_interruptSet, 0, (void) 6);
  else
    protocol_input_opaque(call_proto(call), call, i, len, err);
  return;
}

/*ARGSUSED*/
ilu_cardinal ilu_SizeOfOpaque (ilu_Call call, ilu_opaque i,
			       ilu_cardinal len,
			       ILU_ERRS((IoErrs)) * err)
{
  ilu_cardinal    len2;
  if (call_connection(call) == NIL)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0);
  else if (call->ca_irq || !protocol_needs_sizing(call_proto(call)))
    return (ILU_CLER(*err), 0);
  len2 = protocol_size_of_opaque(call_proto(call), call, i, len, err);
  return (ILU_ERROK(*err) ? len2 : 0);
}

/* ==================== object ==================== */

#ifdef ILU_HTTPNG_OBJECTS

/*L2 >= {call's conn's callmu, iomu}.
  obj == NIL => Main Invariant holds.
  obj != NIL => Inside(s, cl);
*/
void
  ilu_BeginOutputObject(ilu_Call call,
			ilu_Object h,
			ilu_boolean discriminator_p,
			ilu_Class static_type,
			ilu_cardinal nstates,
			ILU_ERRS((IoErrs)) * err)
{
  /* Begin output of object "obj".  "nstates" is the count of the types
     which "obj" has which have state to be output. */

  if (call->ca_irq) {
    ILU_ERR_CONS1(interrupted, err, ilu_interruptSet, 0, (void) 6);
    if (h)
      ilu_ExitServer(h->ob_server, h->ob_class);
  } else {
    if ((h != NIL) && object_is_true(h) && object_collectible(h)) {
      ILU_ERRS((bad_locks, broken_locks, internal)) lerr;
      object_lastRemote(h) = ilu_CoarseTime_Now();
      lerr = _ilu_TouchedObj(h);
      ILU_MUST_BE_SUCCESS(lerr);
    }
    (void) protocol_begin_output_object(call_proto(call),
					call, h,
					discriminator_p,
					static_type,
					nstates,
					err);
  }
}

/*L2 >= {call's conn's callmu, iomu}.
  Inside(s, cl)
  where s = obj's server and cl = obj's type. */
void
  ilu_BeginOutputState(ilu_Call call,
		       ilu_Object obj,
		       ilu_Class state_class,
		       ILU_ERRS((IoErrs)) *err)
{
  (void) protocol_begin_output_state (call_proto(call),
				      call, obj, state_class, err);
  /* Begin output for state of type "state_class" */
}

/*L2 >= {call's conn's callmu, iomu}.
  Inside(s, cl)
  where s = obj's server and cl = obj's type. */
void
  ilu_FinishOutputState(ilu_Call call,
			ilu_Object obj,
			ilu_Class state_class,	/* output state attributes of this type */
			ILU_ERRS((IoErrs)) *err)
{
  (void) protocol_finish_output_state (call_proto(call), call,
				       obj, state_class, err);
  /* Finish output for state of type "state_class" */
}

/**L2 >= {call's conn's callmu, iomu}.
  obj == NIL => Main Invariant holds.
  obj != NIL => all the following:
  before: Inside(s, cl);
  after:				 L1 disjoint {cmu, s};
  after: cl collectible		      => L1  not >=  {gcmu};
  after: cl collectible & s surrogate => Main Invariant holds;
  where s = obj's server and cl = obj's type.
  (We don't really need to hold cmu for surrogate or non-collectible
   objects, but this is convenient because ilu_Enter/ExitServer can
   be used.)*/
void
  ilu_FinishOutputObject(ilu_Call call,
			 ilu_Object obj,
			 ilu_boolean discriminator_p,
			 ilu_Class static_type,
			 ILU_ERRS((IoErrs)) *err)
{
  /* Finish output of object "obj" */
  (void) protocol_finish_output_object (call_proto(call), call, obj,
					discriminator_p, static_type,
					err);
}

/**Main Remnant holds.
   obj!=NIL => L1 = {obj's server};
   obj==NIL => L1 = {}*/

ilu_cardinal
  ilu_BeginSizeOfObject(ilu_Call call,
			ilu_Object obj,
			ilu_boolean discriminator_p,
			ilu_Class static_type,
			ilu_cardinal nstates,
			ILU_ERRS((IoErrs)) *err)
{
  if (call_connection(call) == NIL)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0);
  if (call->ca_irq || !protocol_needs_sizing(call_proto(call)))
    return (ILU_CLER(*err), 0);
  return (protocol_begin_size_of_object(call_proto(call), call,
					obj, discriminator_p,
					static_type, nstates, err));
}

ilu_cardinal
  ilu_BeginSizeOfState(ilu_Call call,
		       ilu_Object obj,
		       ilu_Class state_class,
		       ILU_ERRS((IoErrs)) *err)
{
  if (call_connection(call) == NIL)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0);
  if (call->ca_irq || !protocol_needs_sizing(call_proto(call)))
    return (ILU_CLER(*err), 0);
  return (protocol_begin_size_of_state(call_proto(call), call, obj, state_class, err));
}

ilu_cardinal
  ilu_FinishSizeOfState(ilu_Call call,
			ilu_Object obj,
			ilu_Class state_class,
			ILU_ERRS((IoErrs)) *err)
{
  if (call_connection(call) == NIL)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0);
  if (call->ca_irq || !protocol_needs_sizing(call_proto(call)))
    return (ILU_CLER(*err), 0);
  return (protocol_finish_size_of_state (call_proto(call), call, obj, state_class, err));
}

ilu_cardinal
  ilu_FinishSizeOfObject(ilu_Call call,
			 ilu_Object obj,
			 ilu_boolean discriminator_p,
			 ilu_Class static_type,
			 ILU_ERRS((IoErrs)) *err)
{
  if (call_connection(call) == NIL)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0);
  if (call->ca_irq || !protocol_needs_sizing(call_proto(call)))
    return (ILU_CLER(*err), 0);
  return (protocol_finish_size_of_object(call_proto(call), call,
					 obj, discriminator_p,
					 static_type, err));
}

/**Main Remnant holds, Call-IHi(call);
   before: L1 = {}
*/
ilu_Class	/* non-OPTIONAL */
  ilu_BeginInputObject(ilu_Call call,
		       ilu_boolean discriminator_p,
		       ilu_Class static_type,
		       ilu_cardinal *nstates,
		       ILU_ERRS((IoErrs)) * err)
{
  ilu_Class cl;
  /* Begins process of unmarshalling object ref.  Returns actual type of object. */
  cl = NIL;
  if (call->ca_irq)
    ILU_ERR_CONS1(interrupted, err, ilu_interruptSet, 0, (void) 6);
  else
    cl = protocol_begin_input_object(call_proto(call), call,
				     discriminator_p,
				     static_type, nstates, err);
  return cl;
}

ilu_string
  ilu_BeginInputState (ilu_Call call, ILU_ERRS((IoErrs)) * err)
{
  /*
  Begins process of unmarshalling state of some object type. Called
  "nstate" times, where "nstate" is returned from ilu_BeginInputObject.
  Followed by either calls to input state attributes and final call
  to ilu_FinishInputState, or by call to ilu_SkipInputState.
  */
  if (call->ca_irq)
    ILU_ERR_CONS1(interrupted, err, ilu_interruptSet, 0, (void) 6);
  else
    return (protocol_begin_input_state (call_proto(call), call, err));
}

void
  ilu_SkipInputState (ilu_Call call,
		      ILU_ERRS((IoErrs)) * err)
{
  /* Finishes process of unmarshalling state for some object type. */
  if (call->ca_irq)
    ILU_ERR_CONS1(interrupted, err, ilu_interruptSet, 0, (void) 6);
  else
    (void) protocol_skip_input_state (call_proto(call), call, err);
}

void
  ilu_FinishInputState (ilu_Call call,
			ILU_ERRS((IoErrs)) * err)
{
  /* Finishes process of unmarshalling state for some object type. */
  if (call->ca_irq)
    ILU_ERR_CONS1(interrupted, err, ilu_interruptSet, 0, (void) 6);
  else
    (void) protocol_finish_input_state (call_proto(call), call, err);
}

/* Main Remnant holds, Call-IHi(call);
   after:  result!=NIL => Inside(result's server, static_type);
   after:  result==NIL => L1 = {};
   after:  ILU_ERRNOK(*err) => result==NIL*/
ilu_Object	/* OPTIONAL */
  ilu_FinishInputObject(ilu_Call call,
			ilu_boolean discriminator_p,
			ilu_Class static_type,
			ILU_ERRS((IoErrs)) * err)
{
  /* Afterward, if *o!=NIL && ilu_GetLanguageSpecificObject(*o)==NIL,
     the caller will invoke ilu_RegisterLanguageSpecificObject
     on *o before unlocking the server. */
  if (call->ca_irq)
    ILU_ERR_CONS1(interrupted, err, ilu_interruptSet, 0, (void) 6);
  else
    return protocol_finish_input_object(call_proto(call), call, discriminator_p,
					static_type, err);
}

#if 0

/**Main Remnant holds.
   obj!=NIL => L1 = {obj's server};
   obj==NIL => L1 = {}*/
ilu_cardinal ilu_SizeOfObjectID(ilu_Call call, ilu_Object h,
				ilu_boolean discriminator_p,
				ilu_Class static_type,
				ILU_ERRS((IoErrs)) *err)
{
  ilu_cardinal size = 0;
  if (call_connection(call) == NIL)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0);
  if (call->ca_irq || !protocol_needs_sizing(call_proto(call)))
    return (ILU_CLER(*err), 0);
  size += protocol_begin_size_of_object (call_proto(call), call, h, discriminator_p,
					 static_type, 0, err);
  if (ILU_ERRNOK(*err)) return 0;
  size += protocol_finish_size_of_object (call_proto(call), call, h, discriminator_p,
					  static_type, err);
  return ILU_ERROK(*err) ? size : 0;
}

/**L2 >= {call's conn's callmu, iomu}.
  h == NIL => Main Invariant holds.
  h != NIL => all the following:
  before: Inside(s, cl);
  after:				 L1 disjoint {cmu, s};
  after: cl collectible		      => L1  not >=  {gcmu};
  after: cl collectible & s surrogate => Main Invariant holds;
  where s = h's server and cl = h's type.
  (We don't really need to hold cmu for surrogate or non-collectible
   objects, but this is convenient because ilu_Enter/ExitServer can
   be used.)*/
void ilu_OutputObjectID (ilu_Call call, ilu_Object h,
			 ilu_boolean discriminator_p,
			 ilu_Class static_type,
			 ILU_ERRS((IoErrs)) * err)
{
  if (call->ca_irq) {
    ILU_ERR_CONS1(interrupted, err, ilu_interruptSet, 0, (void) 6);
    if (h)
      ilu_ExitServer(h->ob_server, h->ob_class);
  } else {
    if ((h != NIL) && object_is_true(h) && object_collectible(h)) {
      ILU_ERRS((bad_locks, broken_locks, internal)) lerr;
      object_lastRemote(h) = ilu_CoarseTime_Now();
      lerr = _ilu_TouchedObj(h);
      ILU_MUST_BE_SUCCESS(lerr);
    }
    (void) protocol_begin_output_object(call_proto(call), call, h,
					discriminator_p,
					static_type, 0, err);
    if (ILU_ERRNOK(*err)) return;
    (void) protocol_finish_output_object(call_proto(call), call, h,
					 discriminator_p,
					 static_type, err);
  }
}

/**Main Remnant holds, Call-IHi(call);
  before: L1 = {},
  after:  *o!=NIL => Inside(*o's server, static_type);
  after:  *o==NIL => L1 = {};
  after:  ILU_ERRNOK(*err) => *o==NIL*/
void ilu_InputObjectID (ilu_Call call, ilu_Object *o,
			ilu_boolean discriminator_p,
			ilu_Class static_type,
			ILU_ERRS((IoErrs)) * err)
{
  ilu_Object kobj;
  ilu_cardinal nstates;
  *o = NIL;
  if (call->ca_irq)
    ILU_ERR_CONS1(interrupted, err, ilu_interruptSet, 0, (void) 6);
  else {
    (void) protocol_begin_input_object(call_proto(call), call,
				       discriminator_p,
				       static_type, &nstates, err);
    if (ILU_ERRNOK(*err)) return;
    if (nstates > 0) {
      ILU_ERR_CONS1(marshal, err, minor, ilu_mm_sequenceLimit, 0);
    } else {
      kobj = protocol_finish_input_object(call_proto(call), call,
					  discriminator_p,
					  static_type, err);
      if (ILU_ERROK(*err))
	*o = kobj;
    }
  }
  return;
}

#endif /* 0 */

#endif /* def ILU_HTTPNG_OBJECTS */

/**Main Remnant holds.
   obj!=NIL => L1 = {obj's server};
   obj==NIL => L1 = {}*/
ilu_cardinal ilu_SizeOfObjectID(ilu_Call call, ilu_Object h,
				ilu_boolean discriminator_p,
				ilu_Class static_type,
				ILU_ERRS((IoErrs)) *err)
{
  if (call_connection(call) == NIL)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0);
  if (call->ca_irq || !protocol_needs_sizing(call_proto(call)))
    return (ILU_CLER(*err), 0);
  return protocol_size_of_object_id(call_proto(call), call, h,
				    discriminator_p, static_type,
				    err);
}

/**L2 >= {call's conn's callmu, iomu}.
  h == NIL => Main Invariant holds.
  h != NIL => all the following:
  before: Inside(s, cl);
  after:				 L1 disjoint {cmu, s};
  after: cl collectible		      => L1  not >=  {gcmu};
  after: cl collectible & s surrogate => Main Invariant holds;
  where s = h's server and cl = h's type.
  (We don't really need to hold cmu for surrogate or non-collectible
   objects, but this is convenient because ilu_Enter/ExitServer can
   be used.)*/
void ilu_OutputObjectID (ilu_Call call, ilu_Object h,
			 ilu_boolean discriminator_p,
			 ilu_Class static_type,
			 ILU_ERRS((IoErrs)) * err)
{
  if (call->ca_irq) {
    ILU_ERR_CONS1(interrupted, err, ilu_interruptSet, 0, (void) 6);
    if (h)
      ilu_ExitServer(h->ob_server, h->ob_class);
  } else {
    if ((h != NIL) && object_is_true(h) && object_collectible(h)) {
      ILU_ERRS((bad_locks, broken_locks, internal)) lerr;
      object_lastRemote(h) = ilu_CoarseTime_Now();
      lerr = _ilu_TouchedObj(h);
      ILU_MUST_BE_SUCCESS(lerr);
    }
    (void) protocol_output_object_id(call_proto(call), call, h,
				     discriminator_p,
				     static_type, err);
  }
}

/**Main Remnant holds, Call-IHi(call);
  before: L1 = {},
  after:  *o!=NIL => Inside(*o's server, static_type);
  after:  *o==NIL => L1 = {};
  after:  ILU_ERRNOK(*err) => *o==NIL*/
void ilu_InputObjectID (ilu_Call call, ilu_Object *o,
			ilu_boolean discriminator_p,
			ilu_Class static_type,
			ILU_ERRS((IoErrs)) * err)
{
  *o = NIL;
  if (call->ca_irq)
    ILU_ERR_CONS1(interrupted, err, ilu_interruptSet, 0, (void) 6);
  else
    (void) protocol_input_object_id(call_proto(call), call, o,
				    discriminator_p,
				    static_type, err);
  return;
}

/* ==================== generic object ==================== */

/**Call-OHi(call).
   h == NIL => Main Invariant holds.
   h != NIL => all the following:
   before: Inside(s, cl);
   after:				  L1 disjoint {cmu, s};
   after: cl collectible	       => L1  not >=  {gcmu};
   after: cl collectible & s surrogate => Main Invariant holds;
   where s = h's server and cl = h's type.
   (We don't really need to hold cmu for surrogate or non-collectible
    objects, but this is convenient because ilu_Enter/ExitServer can
    be used.)*/
ilu_boolean 
_ilu_OutputObjectID(ilu_Call call, ilu_Object h,
		    ilu_boolean discriminator_p,
		    ilu_Class static_type,
		    ILU_ERRS((IoErrs)) * err)
{
  ilu_cardinal	server_id_hash;
  ilu_string      ostr2 = NIL;
  ilu_boolean     is_nil = (h == NIL);

  if (discriminator_p) {
    if (h == NIL)
      return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, ilu_FALSE);
    server_id_hash = object_server(h)->sr_crc32;
    ostr2 = h->ob_ih;
  } else if (h == NIL) {
    if (!static_type->cl_optional)
      return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, ilu_FALSE);
    ostr2 = "";
  } else {
    ostr2 = ilu_SBHOfObject(h);
  }

  if (ostr2 == NIL) {
    ilu_ExitServer(object_server(h), object_class(h));
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_not_exported,
			 ilu_FALSE);
  }
  if (!is_nil) {
    ilu_ExitServer(object_server(h), object_class(h));
  }
  if (discriminator_p) {
    protocol_output_cardinal(call_proto(call), call, server_id_hash, err);
    if (ILU_ERRNOK(*err))
      return ilu_FALSE;
  }
  protocol_output_string(call_proto(call), call, (ilu_bytes) ostr2,
			 _ilu_SafeStrlen(ostr2), 0xFFFF,
			 ILU_StringEncoding_latin1,
			 ILU_StringEncoding_latin1, err);
  return (ILU_ERROK(*err));
}

/**before: L1 = {};
   after:  *h!=NIL => Inside(*h's server, static_type);
   after:  *h==NIL => L1 = {};
   Main Remnant holds, Call-IHi(call)*/
ilu_boolean 
_ilu_InputObjectID(ilu_Call call, ilu_Object * h,
		   ilu_boolean discriminator_p,
		   ilu_Class static_type,
		   ILU_ERRS((IoErrs)) * err)
{
  ilu_cardinal server_id_hash;
  ilu_string      istr2 = NIL;
  ilu_cardinal    istrlen2 = 0, junk;
  ilu_Server      server = connection_server(call_connection(call));

  *h = NIL;
  if (static_type == NIL) {
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, ilu_FALSE);
  }
  if (discriminator_p) {
    protocol_input_cardinal (call_proto(call),
			     call, &server_id_hash, err);
    if (ILU_ERRNOK(*err))
      return ilu_FALSE;
    ILU_NOTE(INCOMING_DEBUG | OBJECT_DEBUG,
	  ("ilu_InputObjectID:  server id hash is %lx\n",
	   server_id_hash));
  }
  protocol_input_string(call_proto(call),
			call, (void **) &istr2, &istrlen2, 0xFFFF,
			ILU_StringEncoding_latin1, &junk,
			err);
  if (ILU_ERRNOK(*err)) {
    return ilu_FALSE;
  }
  ILU_NOTE(INCOMING_DEBUG,
	("ilu_InputObjectID:  instance handle/sbh is <%s>\n",
	 istr2));
  ILU_CLER(*err);
  if (discriminator_p) {
    ilu_string      ih = istr2;
    ilu_EnterServer(server, static_type);
    if (server_id_hash != server->sr_crc32) {
      ILU_NOTE(INCOMING_DEBUG,
	    ("%s %lx is for wrong server (not expected %lx, for <%s>).\n",
	     "ilu_InputObjectID:  incoming sid hash", server_id_hash,
	     server->sr_crc32, server_id(server)));
      (void) ILU_ERR_CONS1(marshal, err, minor, ilu_mm_alien_disc, 6);
      ilu_ExitServer(server, static_type);
    } else if (server_objs(server) == NIL) {
      ILU_NOTE(INCOMING_DEBUG,
	    ("%s %s is in closed server <%s>.\n",
	     "ilu_InputObjectID:  instance", ih, server_id(server)));
      (void) ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_svr_closed, 6);
      ilu_ExitServer(server, static_type);
    } else if ((*h = _ilu_FindObjectInServer(ih, server))
	       == NIL) {
      ILU_NOTE(INCOMING_DEBUG,
	    ("%s %s not found in server <%s>.\n",
	     "ilu_InputObjectID:  instance", ih, server_id(server)));
      (void) ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_inst_nf, 6);
      ilu_ExitServer(server, static_type);
    } else if (!ilu_IsSubObjectType((*h)->ob_class, static_type)) {
      ILU_NOTE(INCOMING_DEBUG,
	    ("%s %s/%s has type %s (%s), not a subtype of %s (%s).\n",
	     "_ilu_InputObjectID: Existing object",
	     server_id(server), ih,
	     (*h)->ob_class->cl_unique_id, (*h)->ob_class->cl_name,
	     static_type->cl_unique_id, static_type->cl_name));
      (void) ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_wrong_type, 6);
      *h = NIL;
      ilu_ExitServer(server, static_type);
    }
    if (ILU_ERRNOK(*err)) {
      FREETOKEN(istr2);
      return (ilu_FALSE);
    }
  } else {
    if (istrlen2 == 0) {
      *h = NIL;
      if (static_type->cl_optional) {
	if (istr2 != NIL)
	  ilu_free(istr2);
	return ilu_TRUE;
      } else {
	ILU_NOTE(INCOMING_DEBUG,
	      ("ilu_InputObjectID:  bad NIL obj.\n"));
	return ILU_ERR_CONS1(NoObjectForSBH, err, sbh, istr2, ilu_FALSE);
      }
    } else {
      *h = ilu_ObjectOfSBH(istr2, static_type, err);
      if (ILU_ERRNOK(*err)) {
	ILU_NOTE(INCOMING_DEBUG,
	      ("ilu_InputObjectID:  error:  No object for SBH <%s>.\n",
	       istr2));
	ilu_free(istr2);
	return ilu_FALSE;
      }
    }
  }

  ilu_free(istr2);
  return (ilu_TRUE);
}

/*h!=NIL => L1 >= {h's server}, L1_sup < prmu;
  h==NIL => L1 unconstrained*/
ilu_cardinal _ilu_SizeOfObjectID(ilu_Call call, ilu_Object h,
				 ilu_boolean discriminator_p,
				 ilu_Class static_type,
				 ILU_ERRS((IoErrs)) *err)
{
  ilu_cardinal	server_id_hash;
  ilu_string      strB;		/* discriminator_p ? ih : SBH */
  ilu_cardinal    size1, size2;
  ilu_Connection  conn = call_connection(call);

  ILU_CLER(*err);		/* assume success */

  if (conn == NIL)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0);

  if (h == NIL) {
    strB = "";
  } else if (discriminator_p) {
    server_id_hash = h->ob_server->sr_crc32;
    strB = h->ob_ih;
  } else {
    strB = ilu_SBHOfObject(h);
  }
  if (strB == NIL)
    return ILU_ERR_CONS1(internal, err, minor, ilu_im_broken, 0);

  if (discriminator_p) {
    size1 = protocol_size_of_cardinal(connection_protocol(conn), call,
				      server_id_hash, err);
    if (ILU_ERRNOK(*err))
      return 0;
  } else
    size1 = 0;

  size2 = protocol_size_of_string(connection_protocol(conn), call,
				  (ilu_bytes) strB, _ilu_SafeStrlen(strB),
				  0xFFFF,
				  ILU_StringEncoding_latin1,
				  ILU_StringEncoding_latin1,
				  err);
  if (ILU_ERRNOK(*err))
    return 0;
  return (size1 + size2);
}

/**before: Call-Locking(call, IHi);
    after: Call-Remnant(call, err, ilu_TRUE);
    after: call->ca_ms==ilu_cmsHi && call->ca_msInput;
    after: result!=NIL => Inside(call->ca_server, call->ca_intro_type);
    after: result==NIL => L1 = {}*/
ilu_Object 
ilu_GetCallSingleton(ilu_Call call,
		     ILU_ERRS((bad_param)) * err)
{
  ilu_Server      s = call->ca_server;
  ilu_Class       intro_type = call->ca_intro_type;
  ilu_Object      ans = NIL;
  ilu_EnterServer(s, intro_type);
  if (server_singles(s) != NIL)
    ans = (ilu_Object) ilu_hash_FindInTable(server_singles(s),
					     intro_type);
  if (ans != NIL)
    return (ILU_CLER(*err), ans);
  ilu_ExitServer(s, intro_type);
  call->ca_pe = ilu_ProtocolException_NoSuchClassAtServer;
  return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, NIL);
}

/*************************************************************
**************************************************************
**************************************************************

  Code for sequences, records, arrays, and unions

**************************************************************
**************************************************************
*************************************************************/

void 
ilu_OutputSequence(ilu_Call call, ilu_cardinal i, ilu_cardinal limit,
		   ilu_Type the_type, ILU_ERRS((IoErrs)) * err)
{
  if (call->ca_irq) {
    ILU_ERR_CONS1(interrupted, err, ilu_interruptSet, 0, (void) 6);
    return;
  }
  if (limit > 0 && i > limit) {
    ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0);
    return;
  }
  protocol_output_sequence(call_proto(call), call, i, limit, the_type, err);
  return;
}

void ilu_OutputSequenceMark (ilu_Call call, ilu_cardinal extent,
		  ILU_ERRS((IoErrs)) * err)
{
  if (call->ca_irq)
    ILU_ERR_CONS1(interrupted, err, ilu_interruptSet, 0, (void) 6);
  else
    protocol_output_sequence_mark(call_proto(call), call, extent, err);
  return;
}

void
ilu_InputSequenceMark(ilu_Call call, ilu_cardinal extent,
		      ILU_ERRS((IoErrs)) * err)
{
  if (call->ca_irq)
    ILU_ERR_CONS1(interrupted, err, ilu_interruptSet, 0, (void) 6);
  else
    protocol_input_sequence_mark(call_proto(call), call,
				      extent, err);
  return;
}

void 
ilu_InputSequence(ilu_Call call, ilu_cardinal * i,
		  ilu_cardinal limit, ilu_Type the_type, ILU_ERRS((IoErrs)) * err)
{
  if (call->ca_irq)
    ILU_ERR_CONS1(interrupted, err, ilu_interruptSet, 0, (void) 6);
  else
    protocol_input_sequence(call_proto(call), call, i, limit, the_type, err);
  return;
}

/*ARGSUSED*/
ilu_cardinal
ilu_SizeOfSequence(ilu_Call call, ilu_cardinal i, ilu_cardinal limit,
		   ilu_Type the_type, ILU_ERRS((IoErrs)) * err)
{
  ilu_cardinal    size;
  if (call_connection(call) == NIL)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0);
  if (limit > 0 && i > limit)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0);
  else if (call->ca_irq || !protocol_needs_sizing(call_proto(call)))
    return (ILU_CLER(*err), 0);
  size = protocol_size_of_sequence(call_proto(call), call,
				   i, limit, the_type, err);
  return (ILU_ERROK(*err) ? size : 0);
}

/*L1, L2 unconstrained*/
ilu_boolean ilu_EndSequence (ilu_Call call, ILU_ERRS((IoErrs)) * err)
{
  if (call->ca_irq)
    ILU_ERR_CONS1(interrupted, err, ilu_interruptSet, 0, (void) 6);
  else
    protocol_end_sequence(call_proto(call), call, err);
  return ILU_ERROK(*err);
}

void 
ilu_OutputUnion(ilu_Call call, ilu_cardinal i,
		ilu_TypeKind typekind,
		ilu_Type the_type, ILU_ERRS((IoErrs)) * err)
{
  if (call->ca_irq)
    ILU_ERR_CONS1(interrupted, err, ilu_interruptSet, 0, (void) 6);
  else
    protocol_output_union(call_proto(call), call, i, typekind, the_type, err);
  return;
}

void 
ilu_InputUnion(ilu_Call call, ilu_cardinal * i, ilu_TypeKind typekind,
	       ilu_Type the_type, ILU_ERRS((IoErrs)) * err)
{
  if (call->ca_irq)
    ILU_ERR_CONS1(interrupted, err, ilu_interruptSet, 0, (void) 6);
  else
    protocol_input_union(call_proto(call), call, i, typekind, the_type, err);
  return;
}

/*ARGSUSED*/
ilu_cardinal 
ilu_SizeOfUnion(ilu_Call call, ilu_cardinal i, ilu_TypeKind typekind,
		ilu_Type the_type, ILU_ERRS((IoErrs)) * err)
{
  ilu_cardinal    size;
  if (call_connection(call) == NIL)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0);
  else if (call->ca_irq || !protocol_needs_sizing(call_proto(call)))
    return (ILU_CLER(*err), 0);
  size = protocol_size_of_union(call_proto(call), call, i, typekind, the_type, err);
  return (ILU_ERROK(*err) ? size : 0);
}

/*L1, L2 unconstrained*/
ilu_boolean ilu_EndUnion (ilu_Call call, ILU_ERRS((IoErrs)) * err)
{
  if (call->ca_irq)
    ILU_ERR_CONS1(interrupted, err, ilu_interruptSet, 0, (void) 6);
  else
    protocol_end_union(call_proto(call), call, err);
  return ILU_ERROK(*err);
}

void 
ilu_OutputArray(ilu_Call call, ilu_cardinal length,
		ilu_Type the_type, ILU_ERRS((IoErrs)) * err)
{
  if (call->ca_irq)
    ILU_ERR_CONS1(interrupted, err, ilu_interruptSet, 0, (void) 6);
  else
    protocol_output_array(call_proto(call), call, length, the_type, err);
  return;
}

void 
ilu_InputArray(ilu_Call call, ilu_Type the_type, ILU_ERRS((IoErrs)) * err)
{
  if (call->ca_irq)
    ILU_ERR_CONS1(interrupted, err, ilu_interruptSet, 0, (void) 6);
  else
    protocol_input_array(call_proto(call), call, the_type, err);
  return;
}

/*ARGSUSED*/
ilu_cardinal 
ilu_SizeOfArray(ilu_Call call, ilu_cardinal length,
		ilu_Type the_type, ILU_ERRS((IoErrs)) * err)
{
  ilu_cardinal    size;
  if (call_connection(call) == NIL)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0);
  else if (call->ca_irq || !protocol_needs_sizing(call_proto(call)))
    return (ILU_CLER(*err), 0);
  size = protocol_size_of_array(call_proto(call), call,
				length, the_type, err);
  return (ILU_ERROK(*err) ? size : 0);
}

/*L1, L2 unconstrained*/
ilu_boolean ilu_EndArray (ilu_Call call, ILU_ERRS((IoErrs)) * err)
{
  if (call->ca_irq)
    ILU_ERR_CONS1(interrupted, err, ilu_interruptSet, 0, (void) 6);
  else
    protocol_end_array(call_proto(call), call, err);
  return ILU_ERROK(*err);
}

void 
ilu_OutputRecord(ilu_Call call, ilu_Type the_type, ILU_ERRS((IoErrs)) * err)
{
  if (call->ca_irq)
    ILU_ERR_CONS1(interrupted, err, ilu_interruptSet, 0, (void) 6);
  else
    protocol_output_record(call_proto(call), call, the_type, err);
  return;
}

void 
ilu_InputRecord(ilu_Call call, ilu_Type the_type, ILU_ERRS((IoErrs)) * err)
{
  if (call->ca_irq)
    ILU_ERR_CONS1(interrupted, err, ilu_interruptSet, 0, (void) 6);
  else
    protocol_input_record(call_proto(call), call, the_type, err);
  return;
}

/*ARGSUSED*/
ilu_cardinal ilu_SizeOfRecord (ilu_Call call, ilu_Type the_type, ILU_ERRS((IoErrs)) * err)
{
  ilu_cardinal    size;
  if (call_connection(call) == NIL)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0);
  else if (call->ca_irq || !protocol_needs_sizing(call_proto(call)))
    return (ILU_CLER(*err), 0);
  size = protocol_size_of_record(call_proto(call), call, the_type, err);
  return (ILU_ERROK(*err) ? size : 0);
}

/*L1, L2 unconstrained*/
ilu_boolean ilu_EndRecord (ilu_Call call, ILU_ERRS((IoErrs)) *err)
{
  if (call->ca_irq)
    ILU_ERR_CONS1(interrupted, err, ilu_interruptSet, 0, (void) 6);
  else
    protocol_end_record(call_proto(call), call, err);
  return ILU_ERROK(*err);
}

/***********************************************************************/
/***********************************************************************/
/*****************  Fixed-point support  *******************************/
/***********************************************************************/
/***********************************************************************/

#ifdef ILU_FIXED_POINT_SUPPORT

void 
  ilu_InputFixedpoint (ilu_Call call,
		       ilu_Bignum *numerator,	/* OUTPUT:  NIL indicates NaN, 1 indicates Infinity */
		       ilu_Bignum min_numerator,	/* OPTIONAL */
		       ilu_Bignum max_numerator,	/* OPTIONAL */
		       ilu_Bignum denominator,
		       ilu_cardinal fixed_digits,
		       ilu_cardinal fixed_decimal_places,
		       ilu_FixedPointRangeSize rangesize,
		       ILU_ERRS((IoErrs)) * err)
{
  if (call->ca_irq)
    ILU_ERR_CONS1(interrupted, err, ilu_interruptSet, 0, (void) 6);
  else
    protocol_input_fixedpoint (call_proto(call), call, numerator, min_numerator, max_numerator, denominator, fixed_digits, fixed_decimal_places, rangesize, err);
  return;
}

/*ARGSUSED*/
ilu_cardinal
  ilu_SizeOfFixedpoint (ilu_Call call,
			ilu_Bignum numerator,	/* NIL indicates NaN, 1 indicates Infinity */
			ilu_Bignum min_numerator,	/* OPTIONAL */
			ilu_Bignum max_numerator,	/* OPTIONAL */
			ilu_Bignum denominator,	/* NIL indicates Infinity */
			ilu_cardinal fixed_digits,
			ilu_cardinal fixed_decimal_places,
			ilu_FixedPointRangeSize rangesize,
			ilu_Error *err)
{
  ilu_cardinal    len2;
  if (call_connection(call) == NIL)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0);
  else if (call->ca_irq || !protocol_needs_sizing(call_proto(call)))
    return (ILU_CLER(*err), 0);
  len2 = protocol_size_of_fixedpoint(call_proto(call), call, numerator, min_numerator, max_numerator, denominator, fixed_digits, fixed_decimal_places, rangesize, err);
  return (ILU_ERROK(*err) ? len2 : 0);
}

void
  ilu_OutputFixedpoint (ilu_Call call,
			ilu_Bignum numerator,	/* NIL indicates NaN, 1 indicates Infinity */
			ilu_Bignum min_numerator,	/* OPTIONAL */
			ilu_Bignum max_numerator,	/* OPTIONAL */
			ilu_Bignum denominator,
			ilu_cardinal fixed_digits,
			ilu_cardinal fixed_decimal_places,
			ilu_FixedPointRangeSize rangesize,
			ilu_Error *err)
{
  if (call->ca_irq)
    ILU_ERR_CONS1(interrupted, err, ilu_interruptSet, 0, (void) 6);
  else
    protocol_output_fixedpoint(call_proto(call), call, numerator, min_numerator, max_numerator, denominator, fixed_digits, fixed_decimal_places, rangesize, err);
  return;
}

ilu_boolean
  _ilu_InputFixedpoint (ilu_Call call,
			ilu_Bignum *numerator,	/* NIL indicates NaN, 1 indicates Infinity */
			ilu_Bignum min_numerator,	/* OPTIONAL */
			ilu_Bignum max_numerator,	/* OPTIONAL */
			ilu_Bignum denominator,
			ilu_cardinal fixed_digits,
			ilu_cardinal fixed_decimal_places,
			ilu_FixedPointRangeSize rangesize,
			ILU_ERRS((IoErrs)) * err)
{
  ilu_bytes val = NIL;
  ilu_byte tmpval[8];
  ilu_cardinal value, len;
  ilu_boolean negative;
  ilu_Bignum n;
  int i;

  switch (rangesize)
    {
    case ilu_fprs_byte:
      ilu_InputByte(call, &tmpval[0], err);
      if (ILU_ERRNOK(*err)) return ilu_FALSE;
      val = &tmpval[0];
      len = 1;
      break;
    case ilu_fprs_shortcardinal:
      {
	ilu_InputShortCardinal(call, (ilu_shortcardinal *) &tmpval[0], err);
	if (ILU_ERRNOK(*err)) return ilu_FALSE;
#ifdef WORDS_BIGENDIAN
	*((ilu_shortcardinal *)&tmpval[0]) = SWAP_HALFWORD(*((ilu_shortcardinal *)&tmpval[0]));
#endif
	for (i = 0;  i < 2;  i++) {
	  if (tmpval[0] == 0)
	    break;
	}
	val = &tmpval[0];
	len = i;
	break;
      }
    case ilu_fprs_cardinal:
      {
	ilu_InputCardinal(call, (ilu_cardinal *) &tmpval[0], err);
	if (ILU_ERRNOK(*err)) return ilu_FALSE;
#ifdef WORDS_BIGENDIAN
	*((ilu_cardinal *)&tmpval[0]) = SWAP_WORD(*((ilu_cardinal *)&tmpval[0]));
#endif
	for (i = 0;  i < 4;  i++) {
	  if (tmpval[0] == 0)
	    break;
	}
	val = &tmpval[0];
	len = i;
	break;
      }
    case ilu_fprs_longcardinal:
      {
	ilu_InputLongCardinal(call, (ilu_longcardinal *) &tmpval[0], err);
	if (ILU_ERRNOK(*err)) return ilu_FALSE;
#ifdef WORDS_BIGENDIAN
	*((ilu_longcardinal *)&tmpval[0]) = SWAP_LONGWORD(*((ilu_longcardinal *)&tmpval[0]));
#endif
	for (i = 0;  i < 8;  i++) {
	  if (tmpval[0] == 0)
	    break;
	}
	val = &tmpval[0];
	len = i;
	break;
      }
    case ilu_fprs_shortinteger:
      {
	ilu_InputShortInteger(call, (ilu_shortinteger *) &tmpval[0], err);
	if (ILU_ERRNOK(*err)) return ilu_FALSE;
#ifdef WORDS_BIGENDIAN
	*((ilu_shortinteger *)&tmpval[0]) = SWAP_HALFWORD(*((ilu_shortinteger *)&tmpval[0]));
#endif
	for (i = 0;  i < 2;  i++) {
	  if (tmpval[0] == 0)
	    break;
	}
	val = &tmpval[0];
	len = i;
	break;
      }
    case ilu_fprs_integer:
      {
	ilu_InputInteger(call, (ilu_integer *) &tmpval[0], err);
	if (ILU_ERRNOK(*err)) return ilu_FALSE;
#ifdef WORDS_BIGENDIAN
	*((ilu_integer *)&tmpval[0]) = SWAP_WORD(*((ilu_integer *)&tmpval[0]));
#endif
	for (i = 0;  i < 4;  i++) {
	  if (tmpval[0] == 0)
	    break;
	}
	val = &tmpval[0];
	len = i;
	break;
      }
    case ilu_fprs_longinteger:
      {
	ilu_InputLongInteger(call, (ilu_longinteger *) &tmpval[0], err);
	if (ILU_ERRNOK(*err)) return ilu_FALSE;
#ifdef WORDS_BIGENDIAN
	*((ilu_longinteger *)&tmpval[0]) = SWAP_LONGWORD(*((ilu_longinteger *)&tmpval[0]));
#endif
	for (i = 0;  i < 8;  i++) {
	  if (tmpval[0] == 0)
	    break;
	}
	val = &tmpval[0];
	len = i;
	break;
      }
    case ilu_fprs_large:
      {
	ilu_InputCardinal (call, &value, err);
	if (ILU_ERRNOK(*err)) return ilu_FALSE;
	if (value == 0x80000000) {	/* infinity */
	  *numerator = (ilu_Bignum) 1;
	} else if (value == 0x40000000) {
	  *numerator = (ilu_Bignum) 0;
	} else {
	  negative = ((value & 0x20000000) != 0);
	  len = value & 0x1FFFFFFF;
	  ilu_InputOpaque(call, &val, len, err);
	  if (ILU_ERRNOK(*err)) return ilu_FALSE;
	}
      }
    }
  n = ilubignum_FromBytes (len, val, negative);
  if (val != &tmpval[0])
    ilu_free(val);
  if (n == NULL)
    return ILU_ERR_CONS1(no_memory, err, nbytes, 4 + len, ilu_FALSE);
  if (((min_numerator != NIL) && (ilubignum_Compare(min_numerator, n) > 1)) ||
      ((max_numerator != NIL) && (ilubignum_Compare(max_numerator, n) < 1))) {
    ilubignum_FreeValue(n);
    return ILU_ERR_CONS1(marshal, err, minor, ilu_mm_fixedpoint_range, ilu_FALSE);
  } else {
    *numerator = n;
  }
  return ILU_ERROK(*err);
}

#define ABS(x) (((x) < 0)?(-(x)):(x))

/*ARGSUSED*/
ilu_cardinal
  _ilu_SizeOfFixedpoint (ilu_Call call,
			 ilu_Bignum numerator,	/* NIL indicates NaN, 1 indicates infinity */
			 ilu_Bignum min_numerator,	/* OPTIONAL */
			 ilu_Bignum max_numerator,	/* OPTIONAL */
			 ilu_Bignum denominator,
			 ilu_cardinal fixed_digits,
			 ilu_cardinal fixed_decimal_places,
			 ilu_FixedPointRangeSize rangesize,
			 ilu_Error *err)
{
  ilu_bytes b;
  ilu_cardinal len, size = 0;
  ilu_cardinal value;
  unsigned int negative = ilu_FALSE, owned = ilu_FALSE;
  int i;

  if (numerator == NIL) {
    value = 0x40000000;
    return ilu_SizeOfCardinal(call, value, err);
  } else if (numerator == (ilu_Bignum) 1) {
    value = 0x80000000;
    return ilu_SizeOfCardinal(call, value, err);
  } else {
    if (((min_numerator != NIL) && (ilubignum_Compare(min_numerator, numerator) > 1)) ||
	((max_numerator != NIL) && (ilubignum_Compare(max_numerator, numerator) < 1)))
      return ILU_ERR_CONS1(marshal, err, minor, ilu_mm_fixedpoint_range, 0);
    if ((b = ilubignum_AsBytes(numerator, &len, &owned, &negative)) == NULL)
      return ILU_ERR_CONS1(no_memory, err, nbytes, 0, 0);
    if (len > 0x1FFFFFFF) {
      ILU_ERR_CONS1(imp_limit, err, minor, ilu_ilm_bignum_size, 0);
      goto free1;
    };
    switch (rangesize)
      {
      case ilu_fprs_large:
	value = (len & 0x1FFFFFFF) | (negative ? 0x20000000 : 0);
	size = ilu_SizeOfCardinal(call, value, err);
	if (ILU_ERRNOK(*err)) goto free1;
	size += ilu_SizeOfOpaque(call, b, len, err);
	break;
      case ilu_fprs_byte:
	size = ilu_SizeOfByte(call, b[0], err);
	break;
      case ilu_fprs_shortcardinal:
	{
	  ilu_shortcardinal value;
	  for (i = 0, value = 0;  i < len;  i++)
	    value = (value << 8) | b[i];
	  size = ilu_SizeOfShortCardinal(call, value, err);
	  break;
	}
      case ilu_fprs_cardinal:
	{
	  ilu_cardinal value;
	  for (i = 0, value = 0;  i < len;  i++)
	    value = (value << 8) | b[i];
	  size = ilu_SizeOfCardinal(call, value, err);
	  break;
	}
      case ilu_fprs_longcardinal:
	{
	  ilu_longcardinal value;
	  for (i = 0, value = 0;  i < len;  i++)
	    value = (value << 8) | b[i];
	  size = ilu_SizeOfLongCardinal(call, value, err);
	  break;
	}
      case ilu_fprs_shortinteger:
	{
	  ilu_shortinteger value;
	  int sign = (len < 0) ? -1 : 1;
	  for (i = 0, value = 0;  i < ABS(len);  i++)
	    value = (value << 8) | b[i];
	  value = value * sign;
	  size = ilu_SizeOfShortInteger(call, value, err);
	  break;
	}
      case ilu_fprs_integer:
	{
	  ilu_integer value;
	  int sign = (len < 0) ? -1 : 1;
	  for (i = 0, value = 0;  i < ABS(len);  i++)
	    value = (value << 8) | b[i];
	  value = value * sign;
	  size = ilu_SizeOfInteger(call, value, err);
	  break;
	}
      case ilu_fprs_longinteger:
	{
	  ilu_longinteger value;
	  int sign = (len < 0) ? -1 : 1;
	  for (i = 0, value = 0;  i < ABS(len);  i++)
	    value = (value << 8) | b[i];
	  value = value * sign;
	  size = ilu_SizeOfLongInteger(call, value, err);
	  break;
	}
      }
  free1:
    if (owned) ilu_free(b);
    return size;
  }
}

void
  _ilu_OutputFixedpoint (ilu_Call call,
			 ilu_Bignum numerator,	/* NIL indicates NaN */
			 ilu_Bignum min_numerator,	/* OPTIONAL */
			 ilu_Bignum max_numerator,	/* OPTIONAL */
			 ilu_Bignum denominator,	/* NIL indicates Infinity */
			 ilu_cardinal fixed_digits,
			 ilu_cardinal fixed_decimal_places,
			 ilu_FixedPointRangeSize rangesize,
			 ilu_Error *err)
{
  ilu_bytes b;
  ilu_cardinal len, value;
  unsigned int negative, owned;
  int i;

  if (numerator == NIL) {
    value = 0x40000000;
    ilu_OutputCardinal(call, value, err);
  } else if (numerator == (ilu_Bignum) 1) {
    value = 0x80000000;
    ilu_OutputCardinal(call, value, err);
  } else {
    if (((min_numerator != NIL) && (ilubignum_Compare(min_numerator, numerator) > 1)) ||
	((max_numerator != NIL) && (ilubignum_Compare(max_numerator, numerator) < 1))) {
      ILU_ERR_CONS1(marshal, err, minor, ilu_mm_fixedpoint_range, 0);
      return;
    };
    if ((b = ilubignum_AsBytes(numerator, &len, &owned, &negative)) == NIL) {
      ILU_ERR_CONS1(no_memory, err, nbytes, 0, 0);
      return;
    };
    if (len > 0x1FFFFFFF) {
      ILU_ERR_CONS1(imp_limit, err, minor, ilu_ilm_bignum_size, 0);
      goto free1;
    };
    switch (rangesize)
      {
      case ilu_fprs_large:
	value = (len & 0x1FFFFFFF) | (negative ? 0x20000000 : 0);
	ilu_OutputCardinal(call, value, err);
	if (ILU_ERRNOK(*err)) goto free1;
	ilu_OutputOpaque(call, b, len, err);
	break;
      case ilu_fprs_byte:
	ilu_OutputByte(call, b[0], err);
	break;
      case ilu_fprs_shortcardinal:
	{
	  ilu_shortcardinal value;
	  for (i = 0, value = 0;  i < len;  i++)
	    value = (value << 8) | b[i];
	  ilu_OutputShortCardinal(call, value, err);
	  break;
	}
      case ilu_fprs_cardinal:
	{
	  ilu_cardinal value;
	  for (i = 0, value = 0;  i < len;  i++)
	    value = (value << 8) | b[i];
	  ilu_OutputCardinal(call, value, err);
	  break;
	}
      case ilu_fprs_longcardinal:
	{
	  ilu_longcardinal value;
	  for (i = 0, value = 0;  i < len;  i++)
	    value = (value << 8) | b[i];
	  ilu_OutputLongCardinal(call, value, err);
	  break;
	}
      case ilu_fprs_shortinteger:
	{
	  ilu_shortinteger value;
	  int sign = (len < 0) ? -1 : 1;
	  for (i = 0, value = 0;  i < ABS(len);  i++)
	    value = (value << 8) | b[i];
	  value = value * sign;
	  ilu_OutputShortInteger(call, value, err);
	  break;
	}
      case ilu_fprs_integer:
	{
	  ilu_integer value;
	  int sign = (len < 0) ? -1 : 1;
	  for (i = 0, value = 0;  i < ABS(len);  i++)
	    value = (value << 8) | b[i];
	  value = value * sign;
	  ilu_OutputInteger(call, value, err);
	  break;
	}
      case ilu_fprs_longinteger:
	{
	  ilu_longinteger value;
	  int sign = (len < 0) ? -1 : 1;
	  for (i = 0, value = 0;  i < ABS(len);  i++)
	    value = (value << 8) | b[i];
	  value = value * sign;
	  ilu_OutputLongInteger(call, value, err);
	  break;
	}
      }
  free1:
    if (owned) ilu_free(b);
    return;
  }
}

#endif /* def ILU_FIXED_POINT_SUPPORT */

#ifdef ILU_REFERENCE_TYPES

enum ScopeCheckType { ScopeSizing, ScopeOutput, ScopeInput };
struct scope_info {
  HashTable	call_scope;
  ilu_cardinal	id_counter;
  ilu_boolean	output_p;
};
struct ref_info {
  ilu_ReferenceID	ref_id;
  ilu_cardinal		wire_id;
  enum ScopeCheckType	ref_type;
};

static struct scope_info *
  GetScopeInfo (ilu_Call call, ilu_boolean output_p, ilu_Error *err)
{
  static HashTable Scopes = NULL;
  struct scope_info *call_info;

  if (Scopes == NULL) {
    Scopes = ilu_hash_MakeNewTable(23, ilu_hash_HashPointer, ilu_hash_PointerCompare);
    if (Scopes == NULL)
      return ILU_ERR_CONS1(no_memory, err, nbytes, 0, NIL);
  }
  if ((call_info = ilu_hash_FindInTable (Scopes, call)) == NIL) {
    if ((call_info = ilu_MallocE(sizeof(*call_info), err)) == NIL)
      return NIL;
    if ((call_info->call_scope = ilu_hash_MakeNewTable(23, ilu_hash_HashPointer, ilu_hash_PointerCompare)) == NIL)
      return ILU_ERR_CONS1(no_memory, err, nbytes, 0, NIL);
    call_info->id_counter = 0;
    call_info->output_p = output_p;
    ilu_hash_AddToTable (Scopes, call, call_info);
  }
  if (call_info->output_p != output_p) {
    /* was being used for input, and now need for output, or vice versa */
    ilu_hash_FreeHashTable(call_info->call_scope, NULLFN, ilu_free);
    if ((call_info->call_scope = ilu_hash_MakeNewTable(23, ilu_hash_HashPointer, ilu_hash_PointerCompare)) == NIL)
      return ILU_ERR_CONS1(no_memory, err, nbytes, 0, NIL);
    call_info->output_p = output_p;
  };
  ILU_CLER(*err);
  return call_info;
}

static ilu_boolean
  CheckOutputScope (ilu_Call call,
		    enum ScopeCheckType type,
		    ilu_ReferenceID id,
		    ilu_cardinal *ret_id,
		    ILU_ERRS((no_memory)) *err)
{
  struct scope_info *call_info;
  struct ref_info *val_info;
  ilu_boolean first;

  call_info = GetScopeInfo(call, ilu_TRUE, err);
  if (ILU_ERRNOK(*err)) return ilu_FALSE;
  if ((val_info = ilu_hash_FindInTable (call_info->call_scope, id)) == NIL) {
    if ((val_info = ilu_MallocE(sizeof(*val_info), err)) == NIL)
      return ilu_FALSE;
    val_info->ref_id = id;
    val_info->wire_id = ++(call_info->id_counter);
    val_info->ref_type = type;
    ilu_hash_AddToTable (call_info->call_scope, id, val_info);
    *ret_id = val_info->wire_id;
    return ilu_TRUE;
  } else {
    *ret_id = val_info->wire_id;
    if (val_info->ref_type == ScopeSizing && (type == ScopeOutput)) {
      first = ilu_TRUE;
      val_info->ref_type = ScopeOutput;
    } else {
      first = ilu_FALSE;
    }
    return first;
  }
}

static ilu_ReferenceID
  CheckInputScope (ilu_Call call,
		   enum ScopeCheckType type,
		   ilu_cardinal wire_id,
		   ilu_ReferenceID id,
		   ILU_ERRS((no_memory)) *err)
{
  struct scope_info *call_info;
  struct ref_info *val_info;
  ilu_boolean first;
  void *p = (void *) wire_id;

  call_info = GetScopeInfo(call, ilu_FALSE, err);
  if (ILU_ERRNOK(*err)) return ilu_FALSE;
  if ((val_info = ilu_hash_FindInTable (call_info->call_scope, p)) == NIL) {
    if ((val_info = ilu_MallocE(sizeof(*val_info), err)) == NIL)
      return ilu_FALSE;
    val_info->ref_id = id;
    val_info->wire_id = wire_id;
    val_info->ref_type = type;
    ilu_hash_AddToTable (call_info->call_scope, p, val_info);
    return id;
  } else if ((val_info->ref_id == 0) && (id != 0)) {
    val_info->ref_id = id;
    return id;
  } else {
    return val_info->ref_id;
  }
}

ilu_cardinal
  _ilu_SizeOfReference (ilu_Call call, ilu_boolean providedp,
			ilu_boolean *first, ilu_ReferenceID id,
			ILU_ERRS((IoErrs)) *err)
{
  ilu_cardinal scope_id;
  if (id == 0) {
    *first = ilu_TRUE;
    scope_id = (providedp) ? 1 : 0;
  } else
    *first = CheckOutputScope(call, ScopeSizing, id, &scope_id, err);
  if (ILU_ERRNOK(*err)) return 0;
  else return ilu_SizeOfCardinal(call, scope_id, err);
}

void
  _ilu_OutputReference (ilu_Call call, ilu_boolean providedp,
			ilu_boolean *first, ilu_ReferenceID id,
			ILU_ERRS((IoErrs)) *err)
{
  ilu_cardinal scope_id;
  if (id == 0) {
    *first = ilu_TRUE;
    scope_id = (providedp) ? 1 : 0;
  } else
    *first = CheckOutputScope(call, ScopeOutput, id, &scope_id, err);
  if (ILU_ERRNOK(*err)) return;
  else
    ilu_OutputCardinal(call, scope_id, err);
}

ilu_cardinal
  _ilu_InputReference (ilu_Call call, ilu_boolean *present,
		       ilu_ReferenceID *id,
		       ILU_ERRS((IoErrs)) *err)
{
  ilu_cardinal wire_id;
  
  ilu_InputCardinal (call, &wire_id, err);
  if (ILU_ERRNOK(*err)) return 0;
  *present = (wire_id != 0);
  if (wire_id != 0) {
    *id = CheckInputScope(call, ScopeInput, wire_id, 0, err);
    if (*id != ILU_NIL)
      wire_id = 0;
  }
  return wire_id;
}

void
  _ilu_EndInputReference (ilu_Call call, ilu_cardinal wire_id,
			  ilu_ReferenceID id,
			  ilu_Error *err)
{
  (void) CheckInputScope(call, ScopeInput, wire_id, id, err);
}

ilu_cardinal
  ilu_SizeOfReference (ilu_Call call, ilu_boolean providedp,
		       ilu_boolean *first, ilu_ReferenceID id,
		       ILU_ERRS((IoErrs)) *err)
{
  ilu_cardinal    len;
  if (call->ca_irq || !protocol_needs_sizing(call_proto(call)))
    return (ILU_CLER(*err), 0);
  len = protocol_size_of_reference(call_proto(call), call, providedp, first, id, err);
  return (ILU_ERROK(*err) ? len : 0);
}

void
  ilu_OutputReference (ilu_Call call, ilu_boolean providedp,
		       ilu_boolean *first, ilu_ReferenceID id,
		       ILU_ERRS((IoErrs)) *err)
{
  if (call->ca_irq)
    ILU_ERR_CONS1(interrupted, err, ilu_interruptSet, 0, 0);
  else
    protocol_output_reference(call_proto(call), call, providedp, first, id, err);
  return;
}

ilu_cardinal
  ilu_InputReference (ilu_Call call, ilu_boolean *present,
		      ilu_ReferenceID *id,
		      ILU_ERRS((IoErrs)) *err)
{
  if (call->ca_irq)
    return ILU_ERR_CONS1(interrupted, err, ilu_interruptSet, 0, 0);
  else
    return protocol_input_reference(call_proto(call), call, present, id, err);
}

void
  ilu_EndInputReference (ilu_Call call, ilu_cardinal wire_id,
			 ilu_ReferenceID id,
			 ilu_Error *err)
{
  if (call->ca_irq)
    ILU_ERR_CONS1(interrupted, err, ilu_interruptSet, 0, 0);
  else
    protocol_end_input_reference(call_proto(call), call, wire_id, id, err);
}

#endif /* def ILU_REFERENCE_TYPES */

/***********************************************************************/
/***********************************************************************/
/***********************  Passports  ***********************************/
/***********************************************************************/
/***********************************************************************/

/*L1, L2 unconstrained*/

ilu_Passport ilu_CallerPassportOfCall (ilu_Call call)
{
  return (call->ca_caller);
}

void ilu_SetCallerPassportOfCall (ilu_Call call, ilu_Passport pport)
{
  call->ca_caller = pport;
}
