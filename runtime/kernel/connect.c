/** $Id: connect.c,v 1.135 1999/09/20 22:43:56 spreitze Exp $
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
/* Last edited by Mike Spreitzer January 18, 1999 8:24 am PST */

/* #define _POSIX_SOURCE */

#ifdef MACOS
#pragma segment ilu
#endif

#include "iluntrnl.h"
#include "ilutransport.h"
#include "iluprotocol.h"

#include "object.h"
#include "server.h"
#include "connect.h"

/* The Solaris include files only define "struct timeval" under the
   following conditions:

   #if (!defined(_POSIX_C_SOURCE) && !defined(_XOPEN_SOURCE)) || \
     defined(__EXTENSIONS__)
   #ifndef	_ASM

   We test for them here to make sure...
*/

#if (defined(__sun) && defined(__SVR4))
#ifndef __EXTENSIONS__
#ifdef _POSIX_C_SOURCE
#error "the struct timeval will not be defined because _POSIX_C_SOURCE is defined"
#endif
#ifdef _XOPEN_SOURCE
#error "the struct timeval will not be defined because _XOPEN_SOURCE is defined"
#endif
#ifdef _ASM
#error "the struct timeval will not be defined because _ASM is defined"
#endif
#endif /* ndef __EXTENSIONS__ */
#endif /* (defined(__sun) && defined(__SVR4)) */

#ifdef HAVE_GETRLIMIT
#include <sys/time.h>
#include <sys/resource.h>
#endif /* HAVE_GETRLIMIT */

/* L1 >= {cmu}; L2 unconstrained */

static ilu_ConnLinks pendingConns = {0};
static ilu_Connection reduceFinger = NIL;
ilu_integer     _ilu_connCount = 0;
ilu_Condition   _ilu_connCountChg = {0};

#define OthIdlRngNxt(c) 		\
	((c)->co_links[ilu_otherIdle].next	\
	 ?(c)->co_links[ilu_otherIdle]	\
	 :otherIdleConns			\
	).next

ilu_integer     ilu_fdbudget = 16;	/* # FDs allowed */
ilu_integer     ilu_fdstaken = 0;	/* # FDs used (incl idle) */

static ilu_ConnLinks   otherIdleConns = {NIL, NIL};
static ilu_ConnLinks   reapableConns = {NIL, NIL};
/*
 * An idle connection c is reapable iff (c->co_transport->tr_estFDs
 * && !c->co_waiting).
 XXX ordering
 */

/*L2 unconstrained*/
/*L1 >= {conn's server if k=psl; cmu if k=lru}*/

static void 
_ilu_LinkConnection(ilu_ConnLinks * head, ilu_Connection conn,
		    ilu_ConnLinkKind k)
{
  _ilu_Assert((head->prev == NIL && head->next == NIL) ||
	      (head->prev != NIL && head->next != NIL &&
	       head->prev->co_links[k].next == NIL &&
	       head->next->co_links[k].prev == NIL),
	      "LinkConnection");
  if (head == &otherIdleConns) {
    _ilu_Assert((!reduceFinger) == !otherIdleConns.next,
		"otherIdleConns vs. reduceFinger in link");
    if (!reduceFinger)
      reduceFinger = conn;
  }
  conn->co_links[k].prev = NIL;
  conn->co_links[k].next = head->next;
  if (conn->co_links[k].next != NIL)
    conn->co_links[k].next->co_links[k].prev = conn;
  else
    head->prev = conn;
  head->next = conn;
  return;
}

static void 
_ilu_UnlinkConnection(ilu_ConnLinks * head, ilu_Connection conn,
		      ilu_ConnLinkKind k)
{
  _ilu_Assert((conn->co_links[k].prev == NIL)
	      ? head->next == conn
	      : conn->co_links[k].prev->co_links[k].next == conn,
	      "UnlinkConnection 1");
  _ilu_Assert((conn->co_links[k].next == NIL)
	      ? head->prev == conn
	      : conn->co_links[k].next->co_links[k].prev == conn,
	      "UnlinkConnection 2");
  if (conn->co_links[k].prev != NIL)
    conn->co_links[k].prev->co_links[k].next = conn->co_links[k].next;
  else
    head->next = conn->co_links[k].next;
  if (conn->co_links[k].next != NIL)
    conn->co_links[k].next->co_links[k].prev = conn->co_links[k].prev;
  else
    head->prev = conn->co_links[k].prev;
  if (head == &otherIdleConns) {
    if (conn == reduceFinger)
      reduceFinger = OthIdlRngNxt(conn);
    _ilu_Assert((!reduceFinger) == !otherIdleConns.next,
		"otherIdleConns vs. reduceFinger in link");
  }
  return;
}

/*L1, L2 unconstrained*/

ilu_Server ilu_ServerOfConnection(ilu_Connection conn)
{
  return (conn -> co_server);
}

/**L1     =    {cmu, conn's server}; Main Remnant;
   L2    >=    {conn's waitmu};
   L2 disjoint {conn's iomu}*/
ilu_boolean
_ilu_BlockingWaitForInputOnConnection(ilu_Connection conn,
				      ilu_FineTime * limit,
				      ilu_boolean retToPush,
				      ILU_ERRS((broken_locks,
						interrupted,
						internal)) * err)
{
  ilu_Transport   t = connection_transport(conn);
  ilu_Server      s = connection_server(conn);
  int             disabled = 1;
  ilu_boolean     domore = ilu_TRUE;
  if (!ilu_Check(conn->co_waiting && !conn->co_closed, err))
    return ilu_FALSE;
  while (1) {
    if (t->tr_wc) {
      ilu_WaitCohort  wc = t->tr_wc;
      while (wc->iluwc_waitsDisabled && !conn->co_closing) {
	if (retToPush && conn->co_pushme)
	  return ILU_CLER(*err);
	if (!ilu_CMWait2(wc->iluwc_change, server_lock(s), ilu_cmu, err))
	  return ilu_FALSE;
	if (!ilu_Check(conn->co_waiting && !conn->co_closed, err))
	  return ilu_FALSE;
      }
    }
    if (conn->co_closing)
      return ILU_CLER(*err);
    if (!_ilu_ExitServerMutex(s, ilu_TRUE, err))
      return ilu_FALSE;
    if (!ilu_ExitMutex(ilu_cmu, ilu_TRUE, err))
      return ilu_FALSE;
    domore = transport_wait_for_input(t, &disabled, limit, err);
    if (!ilu_ReEnterMutex(ilu_cmu, err))
      return ilu_FALSE;
    if (!_ilu_EnterServerMutex(s, ilu_TRUE, err))
      return ilu_FALSE;
    if (!(disabled && domore))
      break;
    if (retToPush && conn->co_pushme)
      return ilu_TRUE;
  }
  return domore;
}

/*Main Invariant holds; L2 disjoint conn's mutexes*/
ilu_boolean
ilu_BlockingWaitForInputOnConnection(ilu_Connection conn,
				     ilu_FineTime * limit)
{
  ilu_Server      s;
  ILU_ERRS((broken_locks, interrupted, internal)) lerr;
  ilu_boolean     ans = ilu_FALSE;
  ilu_Call_s      dummyCall = {1};
  _ilu_Assert(conn != NIL, "BlockingWaitForInputOnConnection(NIL,..)");
  s = connection_server(conn);
  _ilu_Assert(s != NIL,
	      "BlockingWaitForInputOnConnection(&{s=NIL,..},..)");
  (void) ilu_EnterMutex(ilu_cmu, &lerr);
  ILU_MUST_BE_SUCCESS(lerr);
  (void) _ilu_EnterServerMutex(s, ilu_FALSE, &lerr);
  ILU_MUST_BE_SUCCESS(lerr);
  if (connection_closed(conn))
    goto dun2;
  if (_ilu_CanCondition() && connection_concurrent(conn)) {
    _ilu_EnterConnWait(conn, &dummyCall, ilu_FALSE, &lerr);
    ILU_MUST_BE_SUCCESS(lerr);
    if (connection_closed(conn))
      goto dun3;
  } else
    _ilu_TakeConnWait(conn, &dummyCall);
  ans = _ilu_BlockingWaitForInputOnConnection(conn, limit, ilu_FALSE, &lerr);
  ILU_ERR_SWITCH(lerr) {
    ILU_SUCCESS_CASE;
    ILU_ERR_CASE(interrupted, x) ans = ilu_TRUE;
    /* i.e., assert !(broken_locks||internal) */
  } ILU_ERR_ENDSWITCH;
dun3:
  (void) _ilu_ReleaseConnWait(conn, &dummyCall, ilu_TRUE, &lerr);
dun2:
  (void) _ilu_ExitServerMutex(s, ilu_TRUE, &lerr);
  (void) ilu_ExitMutex(ilu_cmu, ilu_TRUE, &lerr);
  ILU_MUST_BE_SUCCESS(lerr);
  return (ans);
}

/*L1.sup < trmu; L2 >= {xmu, ymu}*/
ilu_boolean
_ilu_SetTransportInputHandler(ilu_Transport trans,
			      ilu_TIH tih,
			      ILU_ERRS((no_memory, no_resources,
					internal)) * err)
{
  if (tih && trans->tr_inBuff != NIL &&
      trans->tr_inNext < trans->tr_inLimit)
    return ilu_FALSE;
  return ((*trans->tr_class->tc_set_input_handler)
	  (trans, tih, err));
}

/*L1 >= {conn's server}; (L1-{conn's server}).sup = cmu*/
/*L2 disjoint {conn's iomu, callmu, waitmu}*/
extern          ilu_boolean
_ilu_InnerSetConnectionInputHandler(ilu_Connection conn,
				ilu_TransportInputHandler tih_proc,
				    ilu_refany tih_rock,
				    ILU_ERRS((no_memory, internal,
					   no_resources, bad_param,
					      bad_locks)) * err)
{
  ilu_Server      s;
  ilu_Transport   trans;
  ilu_boolean     immediate = ilu_FALSE;
  static ilu_Call_s dumy = {0};
  if (!conn)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_nil, ilu_FALSE);
  if (_ilu_CanCondition())
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_threading, ilu_FALSE);
  s = conn->co_server;
  if (!s)
    return ILU_ERR_CONS1(internal, err, minor, ilu_im_broken, ilu_FALSE);
  if (conn->co_mucall || conn->co_ioing || conn->co_waiting)
    return ILU_ERR_CONS0(bad_locks, err, ilu_FALSE);
  ILU_CLER(*err);
  if (conn->co_closed) {
    if (!tih_proc)
      return ilu_TRUE;
    immediate = ilu_TRUE;
    goto level2;
  }
  if (!_ilu_EnterConnCall(conn, &dumy, ilu_TRUE, err))
    goto level3;
  _ilu_TakeConnWait(conn, &dumy);
  if (!_ilu_TakeConnIO(conn, ilu_TRUE, err))
    goto level4;
  trans = conn->co_transport;
  conn->co_tih.proc = tih_proc;
  conn->co_tih.rock = tih_rock;
  immediate = !_ilu_SetTransportInputHandler(trans,
				  (tih_proc ? &conn->co_tih : NIL),
					     err);
  (void) _ilu_ReleaseConnIO(conn, ilu_TRUE, err);
level4:
  (void) _ilu_QuickReleaseConnCall(conn, &dumy, ilu_TRUE, err);
  (void) _ilu_ReleaseConnWait(conn, &dumy, ilu_TRUE, err);
level3:
  if (ILU_ERRNOK(*err))
    return ilu_FALSE;
  if (!immediate || !tih_proc)
    return ilu_TRUE;
level2:
  (void) _ilu_ExitServerMutex(s, ilu_FALSE, err);
  if (ILU_ERRNOK(*err))
    return ilu_FALSE;
  (void) ilu_ExitMutex(ilu_cmu, ilu_FALSE, err);
  if (ILU_ERRNOK(*err))
    goto ret1;
  (*tih_proc) (tih_rock);
  if (!ilu_ReEnterMutex(ilu_cmu, err))
    return ilu_FALSE;
ret1:
  if (!_ilu_EnterServerMutex(s, ilu_TRUE, err))
    return ilu_FALSE;
  return ILU_ERROK(*err);
}

/*Main Invariant holds; L2 disjoint {conn's iomu, callmu, waitmu}*/
extern          ilu_boolean
ilu_SetConnectionInputHandler(ilu_Connection conn,
			      ilu_TransportInputHandler tih_proc,
			      ilu_refany tih_rock,
			      ILU_ERRS((no_memory, internal,
					no_resources, bad_param,
					bad_locks)) * err)
{
  ilu_Server      s;
  ilu_Transport   trans;
  ilu_boolean     immediate = ilu_FALSE;
  static ilu_Call_s dumy = {0};
  if (!conn)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_nil, ilu_FALSE);
  s = conn->co_server;
  if (!s)
    return ILU_ERR_CONS1(internal, err, minor, ilu_im_broken, ilu_FALSE);
  if (!ilu_EnterMutex(ilu_cmu, err))
    goto dun0;
  if (!_ilu_EnterServerMutex(s, ilu_FALSE, err))
    goto dun1;
  if (conn->co_mucall || conn->co_ioing || conn->co_waiting) {
    ILU_ERR_CONS0(bad_locks, err, (void) 0);
    goto dun2;
  }
  if (conn->co_closed) {
    immediate = ilu_TRUE;
    goto dun2;
  }
  if (!_ilu_EnterConnCall(conn, &dumy, ilu_TRUE, err))
    goto dun2;
  _ilu_TakeConnWait(conn, &dumy);
  if (!_ilu_TakeConnIO(conn, ilu_TRUE, err))
    goto dun4;
  trans = conn->co_transport;
  conn->co_tih.proc = tih_proc;
  conn->co_tih.rock = tih_rock;
  immediate = !_ilu_SetTransportInputHandler(trans,
				  (tih_proc ? &conn->co_tih : NIL),
					     err);
  (void) _ilu_ReleaseConnIO(conn, ilu_TRUE, err);
dun4:
  (void) _ilu_ReleaseConnWait(conn, &dumy, ilu_TRUE, err);
  (void) _ilu_QuickReleaseConnCall(conn, &dumy, ilu_TRUE, err);
dun2:
  (void) _ilu_ExitServerMutex(s, ilu_TRUE, err);
dun1:
  (void) ilu_ExitMutex(ilu_cmu, ilu_TRUE, err);
dun0:
  if (immediate && ILU_ERROK(*err) && tih_proc)
    (*tih_proc) (tih_rock);
  return ILU_ERROK(*err);
}

/*L1 >= {cmu, conn's server}, L1.sup < trmu; L2 disjoint conn*/
extern          ilu_boolean
ilu_ClearConnectionInputHandler(ilu_Connection conn,
				ILU_ERRS((no_memory, internal,
					  no_resources)) * err)
{
  ilu_Transport   trans = conn->co_transport;
  ilu_boolean     regd = ilu_FALSE;
  static ilu_Call_s dumy = {0};
  if (conn->co_waiting || conn->co_ioing)
    return ILU_ERR_CONS0(bad_locks, err, ilu_FALSE);
  if (conn->co_closed)
    return ILU_CLER(*err);
  _ilu_TakeConnWait(conn, &dumy);
  if (!_ilu_TakeConnIO(conn, ilu_TRUE, err))
    goto dun3;
  regd = ((*trans->tr_class->tc_set_input_handler)
	  (trans, NIL, err));
  (void) _ilu_ReleaseConnIO(conn, ilu_TRUE, err);
dun3:
  (void) _ilu_ReleaseConnWait(conn, &dumy, ilu_TRUE, err);
  if (ILU_ERRNOK(*err))
    return ilu_FALSE;
  if (!ilu_Check(regd, err))
    return ilu_FALSE;
  return ilu_TRUE;
}

/*L1 >= {cmu, conn's server}; L1.sup < trmu*/
/*L2 >= {conn's iomu}*/
void 
_ilu_CloseIoingConnection(ilu_Connection conn,
			  ilu_boolean set_cfails,
			  ilu_ConnShutdownReason reason)
{
  ilu_Error       err;
  ilu_Protocol    proto = connection_protocol(conn);
  ilu_Transport   trans = conn->co_transport;
  ilu_Server      server = conn->co_server;
  ilu_integer     dfd;
  ilu_Call_s      dummycall = {0};

  _ilu_Assert(conn->co_ioing, "bad call on _ilu_CloseIoingConnection");
  if (set_cfails)
    server->sr_cfails = ilu_TRUE;
  if (connection_closed(conn))
    return;
  _ilu_Assert(_ilu_connCount>0, "connCount in _ilu_CloseIoingConnection");
#ifdef ENABLE_DEBUGGING
  if (connection_incoming(conn)) {
    ILU_NOTE(CONNECTION_DEBUG,
	  ("_ilu_CloseIoingConnection:  incoming conn %p trans %p"
	   " via %s from %s to %s; cfails=%d; waiting=%d; reason=%d;"
	   " closing was %d.\n",
	   conn, trans, conn->co_pinfo,
	   conn_peerinfo(conn),
	   server->sr_id, set_cfails != 0,
	   conn->co_waiting != 0, reason, conn->co_closing != 0));
  } else {
    ilu_string      t = _ilu_StringifyTinfo(conn_tinfo(conn), &err);
    ILU_MUST_BE_SUCCESS(err);
    ILU_NOTE(CONNECTION_DEBUG,
	   ("_ilu_CloseIoingConnection:  outgoing conn %p trans %p"
	    " via %s %s to %s; cfails=%d; waiting=%d; reason=%d;"
	    " closing was %d.\n",
	    conn, trans, conn->co_pinfo, t,
	    server->sr_id, set_cfails != 0,
	    conn->co_waiting != 0, reason, conn->co_closing != 0));
    ilu_free(t);
  }
#endif				/* ENABLE_DEBUGGING */
  if (connection_protocol(conn)->pr_conn_closing != NULLFN) {
    protocol_close_connection(connection_protocol(conn),
			      connection_protocol_data(conn),
			      reason, &err);
    ILU_HANDLED(err);
  };
  if (conn->co_waiting) {
    if (conn->co_closing)
      return;
    conn->co_closing = ilu_TRUE;
    if (_ilu_CanCondition()) {
      (void) transport_disableWait(trans, &err);
      ILU_MUST_BE_SUCCESS(err);
      /*
       * MJS, May 15, 1998: we used to notify
       * trans->tr_wc->iluwc_change here, but that seems redundat
       * with the disableWait operation above.
       */
    } else
      (void) transport_interruptST(trans, &err);
    ILU_MUST_BE_SUCCESS(err);
    return;
  }
  conn->co_waiting = &dummycall;
  conn->co_closed = ilu_TRUE;
  if (_ilu_CanCondition()) {
    if (conn->co_closing) {
      transport_enableWait(trans, &err);
      ILU_MUST_BE_SUCCESS(err);
    }
    if (trans->tr_wc) {
      ilu_CondNotify(trans->tr_wc->iluwc_change, &err);
      ILU_MUST_BE_SUCCESS(err);
    }
  }
  if ((conn->co_mucall == NIL) && (conn->co_nOuts == 0)) {
    if (conn->co_transport->tr_estFDs)
      _ilu_UnlinkConnection(&reapableConns, conn, ilu_reapable);
    else
      _ilu_UnlinkConnection(&otherIdleConns, conn, ilu_otherIdle);
  }
  if (conn->co_pending)
    _ilu_UnlinkConnection(&pendingConns, conn, ilu_pcl);
  if (connection_incoming(conn)) {
    ilu_Port        p = conn->co_port;
    _ilu_UnlinkConnection(&p->po_connHead, conn, ilu_psl);
    _ilu_LinkConnection(&p->po_closedConns, conn, ilu_psl);
  } else {
    _ilu_UnlinkConnection(&server->sr_connHead, conn, ilu_psl);
    _ilu_LinkConnection(&server->sr_closedConns, conn, ilu_psl);
  }
  _ilu_connCount--;
  transport_close(trans, &dfd, &err);
  ILU_MUST_BE_SUCCESS(err);
  ilu_DeltaFD(-dfd);
  protocol_free_data_block(proto, connection_protocol_data(conn));
  conn->co_protocol_data = NIL;
  while (conn->co_replies != NIL) {
    ilu_ReplyList   r = conn->co_replies;
    ilu_ReplyList   next = r->rp_next;
    (void) protocol_abandon_delayed_interp(proto, conn, r->rp_queued,
					   &err);
    ILU_MUST_BE_SUCCESS(err);
    ilu_free(r);
    conn->co_replies = next;
  }
  if (conn->co_auth_info != NIL)
    ilu_DestroyPassport(conn->co_auth_info, &err);
  ILU_MUST_BE_SUCCESS(err);
  if (_ilu_CanCondition()) {
    err = _ilu_NotifyCondition(conn->co_cc);
    ILU_MUST_BE_SUCCESS(err);
    err = _ilu_NotifyCondition(_ilu_connCountChg);
    ILU_MUST_BE_SUCCESS(err);
  }
  _ilu_Assert(conn->co_waiting == &dummycall,
	      "ilu_CloseIoIngConn vs. wait in connect.c");
  conn->co_waiting = NIL;
  return;
}

/*L1, L2 unconstrained*/
static struct _ilu_Call_s fauxcall = {0};

/* Usable on either incoming or outgoing connections. */
/*L1 >= {conn's server, cmu}; L2 disjoint {conn's callmu, iomu}*/
ILU_ERRS((bad_locks, broken_locks))
_ilu_CloseConnection(ilu_Connection connection,
		     ilu_ConnShutdownReason reason)
{
  ilu_Error       err;
  if (connection_closed(connection))
    return ILU_NO_ERR;
  if (!_ilu_EnterConnIO(connection, ilu_FALSE, &err))
    return err;
  _ilu_CloseIoingConnection(connection, ilu_FALSE, reason);
  (void) _ilu_ReleaseConnIO(connection, ilu_TRUE, &err);
  return err;
}

/* L1 >= {conn's server}; L2 disjoint {conn's callmu, iomu} */
void _ilu_MaybeFreeConnection(ilu_Connection conn)
{
  ilu_Server s = conn->co_server;
  if (conn->co_nOuts || conn->co_nCalls ||
      conn->co_lsrCares || !conn->co_closed || conn->co_serialer ||
      conn->co_waiting || conn->co_mucall || conn->co_ioing ||
      conn->co_batchCount || conn->co_pushAlarmSet ||
      conn->co_holds)
    return;
  if (connection_incoming(conn)) {
    ilu_free(conn_peerinfo(conn));
    conn_peerinfo(conn) = NIL;
  } else {
    ilu_free(conn_tinfo(conn));
    conn_tinfo(conn) = NIL;
  }
  ilu_free(conn->co_pinfo);
  conn->co_pinfo = NIL;
  /* if there's a pushAlarm, that's a thread that should be freed.
   */
  if (conn->co_pushAlarm != NIL) {
    ilu_DestroyAlarm (conn->co_pushAlarm);
  }
  /*
   * conn->co_transport should be closed and free by now.  What
   * about co_auth_info and co_conn_identity?  co_replies should be
   * empty by now.
   */
  if (_ilu_CanCondition())
    ilu_DestroyCondition(conn->co_cc);
  conn->co_cc = NIL;
  /*
   * Should already be unlinked from server & idle list, and be in
   * closed list.
   */
  if (conn->co_port) {
    ILU_ERRS((internal)) lerr;
    ilu_Port        p = conn->co_port;
    _ilu_UnlinkConnection(&p->po_closedConns, conn, ilu_psl);
    (void) _ilu_MaybeFreePort(p, &lerr);
    ILU_MUST_BE_SUCCESS(lerr);
  } else
    _ilu_UnlinkConnection(&conn->co_server->sr_closedConns, conn,
			  ilu_psl);
  ILU_NOTE(CONNECTION_DEBUG,
	   ("connect.c: freeing ilu_Connection %p %s %p=%s;\n"
   "\tserver's ports=%s %s %s, conns:=%s %s, objs=%d, LSSes=%d.\n",
	    conn, (connection_incoming(conn) ? "into" : "to"),
	    s, s->sr_id,
	    (server_ports(s) ? "X" : "0"),
	    (s->sr_local_port ? "X" : "0"),
	    (s->sr_closedPorts.pl_next ? "X" : "0"),
	    (s->sr_connHead.next ? "X" : "0"),
	    (s->sr_closedConns.next ? "X" : "0"),
	    (s->sr_objs ? ilu_hash_PairsInTable(s->sr_objs) : 0),
	    _ilu_ServerLSSCount(s)));
  ilu_free(conn);
  return;
}

/*L1, L2 unconstrained; call only when single-threaded */
ilu_boolean ilu_ConnectionServingP(ilu_Connection conn)
{
  return (conn->co_nCalls > 0);
}

/*L1.sup < cmu; L2 disjoint conn's mutexes*/
ilu_boolean
ilu_DoneServingConnection(ilu_Connection conn,
			  ILU_ERRS((bad_param, broken_locks,
				    bad_locks, internal)) * err)
{
  ilu_Server      s = conn->co_server;
  if (!(connection_incoming(conn) && conn->co_lsrCares)) {
   ILU_NOTE(CONNECTION_DEBUG,
	("ilu_DoneServingConnection, returning error because: incoming = %s, lsrCares = %s)\n",
	 (connection_incoming(conn) ? "True" : "False"), 
	 (conn->co_lsrCares ? "True" : "False")));
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, ilu_FALSE);
  }
  ILU_NOTE(CONNECTION_DEBUG,
	("ilu_DoneServingConnection(%p, tr=%p, %s from %s)\n",
	 conn, conn->co_transport, conn->co_pinfo,
	 conn_peerinfo(conn)));
  if (!ilu_EnterMutex(ilu_cmu, err))
    return ilu_FALSE;
  if (!_ilu_EnterServerMutex(s, ilu_FALSE, err))
    goto dun1;
  *err = _ilu_CloseConnection(conn,
			ilu_ConnShutdownReason_ProcessTermination);
  conn->co_lsrCares = ilu_FALSE;
  if (ILU_ERROK(*err))
    _ilu_MaybeFreeConnection(conn);
  (void) _ilu_ExitServerMutex(s, ilu_TRUE, err);
dun1:
  (void) ilu_ExitMutex(ilu_cmu, ilu_TRUE, err);
  return ILU_ERROK(*err);
}

/*Main Invariant, L2 >= {conn's iomu}*/
extern          ilu_boolean
_ilu_CloseConnWithIo(ilu_Connection conn, ilu_boolean set_cfails,
		     ilu_ConnShutdownReason reason, ILU_ERRS((IoErrs)) * err)
{
  ilu_Server      s = conn->co_server;
  if (!ilu_ReEnterMutex(ilu_cmu, err))
    return ilu_FALSE;
  if (!_ilu_EnterServerMutex(s, ilu_TRUE, err))
    goto dun1;
  _ilu_CloseIoingConnection(conn, set_cfails, reason);
  (void) _ilu_ExitServerMutex(s, ilu_TRUE, err);
dun1:
  (void) ilu_ExitMutex(ilu_cmu, ilu_TRUE, err);
  return ILU_ERROK(*err);
}

/*L1 >= {cmu}*/
/*L2 unconstrained*/

void 
ilu_FullDeltaFD(ilu_integer n, char *file, int line)
{
  ILU_NOTE(CONNECTION_DEBUG,
	("ilu_fdstaken(was %ld) += %ld at %s:%d\n",
	 (long int) ilu_fdstaken,
	 (long int) n, file, line));
  ilu_fdstaken += n;
  return;
}

/*L1.sup < cmu; L2 unconstrained*/
ilu_cardinal ilu_GetFDBudget(void)
{
  ilu_cardinal ans;
  ilu_AcquireMutex(ilu_cmu);
  ans = ilu_fdbudget;
  ilu_ReleaseMutex(ilu_cmu);
  return ans;
}

/*Main Invariant holds; L2 otherwise unconstrained*/
extern ilu_cardinal ilu_SetFDBudget(ilu_cardinal n)
{
  ILU_ERRS((bad_locks, broken_locks, internal)) err;
  ilu_cardinal    ans;
  static const ilu_FineTime dt = {10, 0};
  ilu_FineTime    tNow;
  ilu_FineTime    tOut;
  tNow = ilu_FineTime_Now();
  tOut = ilu_FineTime_Add(tNow, dt);
#ifdef HAVE_GETRLIMIT
  {
    struct rlimit   resourcelimit;
    int rlimit_ans, err1;
    /* restrict to no more than the processes current resource limit */
    if ((rlimit_ans = getrlimit(RLIMIT_NOFILE, &resourcelimit)) != 0) {
      err1 = errno;
      ILU_NOTE(CONNECTION_DEBUG, ("ilu_SetFDBudget couldn't determine resource limit RLIMIT_NOFILE: reval = %d, errno = %d\n", rlimit_ans, errno));
    } else {
      if (n > resourcelimit.rlim_cur - 1) {
	ILU_NOTE(CONNECTION_DEBUG, ("ilu_SetFDBudget(%d) limiting to RLIMIT_NOFILE - 1 which is %d\n", n, resourcelimit.rlim_cur - 1));
	n = resourcelimit.rlim_cur - 1;
      }
    }
  }
#else
#ifdef WIN32
  if (n > FD_SETSIZE) {
    ILU_NOTE(CONNECTION_DEBUG, ("ilu_SetFDBudget(%d) limiting to FD_SETSIZE which is %d\n", n, FD_SETSIZE));
    n = FD_SETSIZE;
  }
#endif				/* WIN32 */
#endif				/* HAVE_GETRLIMIT */

  ilu_AcquireMutex(ilu_cmu);
  (void) _ilu_ReduceFdsTo(n, &tOut, &err);
  ILU_MUST_BE_SUCCESS(err);
  ilu_fdbudget = ilu_fdstaken;
  if (ilu_fdbudget < 0 || n > (ilu_cardinal) ilu_fdbudget)
    ilu_fdbudget = n;
  ans = ilu_fdbudget;
  ilu_ReleaseMutex(ilu_cmu);
  return ans;
}

/*Main Remnant holds*/

static const ilu_FineTime tZero = {0, 0};
static const float waitFracs[2] = {0.3F, 0.45F};

/*L1.sup = cmu*/
ilu_boolean
_ilu_ReduceFdsTo(ilu_integer goal,
		 const ilu_FineTime * timeout,	/* absolute, optional */
		 ILU_ERRS((bad_locks, broken_locks,
			   internal)) * err)
{
  unsigned        iter = 0;
  ilu_Server      s;
  ilu_Connection  cur;
  _ilu_HoldMutex(ilu_cmu);
#ifdef ENABLE_DEBUGGING
  if (timeout)
    ILU_NOTE(CONNECTION_DEBUG,
	     ("_ilu_ReduceFdsTo: goal %d, ilu_fdbudget is %d, usage=%d, T.O.=%lu:%lu\n",
	      goal, ilu_fdbudget, ilu_fdstaken,
	      (long unsigned) timeout->ft_s,
	      (long unsigned) timeout->ft_t));
  else
    ILU_NOTE(CONNECTION_DEBUG,
	     ("_ilu_ReduceFdsTo: goal %d, ilu_fdbudget is %d, usage=%d.\n",
	      goal, ilu_fdbudget, ilu_fdstaken));
#endif
  while (ilu_fdstaken > goal && _ilu_connCount > 0) {
    ilu_FineTime    tNow, tRem, dTimeout, cTimeout;
    const ilu_FineTime *pdTimeout = NIL;
    ilu_Connection  urFinger;
    while ((cur = reapableConns.prev))
      /* cur := the one that's been reapable longest. */
    {
      s = cur->co_server;
      if (!_ilu_EnterServerMutex(s, ilu_FALSE, err))
	return ilu_FALSE;
      /*
       * (cur) not closed, because this thread holds cmu, which
       * another thread would need to close (cur).
       */
      if (!ilu_Check(cur->co_transport->tr_estFDs
		     && !(cur->co_waiting || cur->co_mucall
			  || cur->co_nOuts),
		     err))
	goto dun1;
      /* What about waiting input? */
      if (!_ilu_EnterConnCall(cur, &fauxcall, ilu_TRUE, err))
	goto dun1;
      if (!_ilu_EnterConnIO(cur, ilu_TRUE, err))
	goto dun2;
      if (!ilu_Check(!(cur->co_waiting || cur->co_nOuts),
		     err))
	goto dun3;
      ILU_NOTE(CONNECTION_DEBUG,
	       ("_ilu_ReduceFdsTo calling _ilu_CloseIoingConnection( %p, ilu_FALSE)\n",
		cur));
      _ilu_CloseIoingConnection(cur, ilu_FALSE,
			ilu_ConnShutdownReason_ResourceManagement);
      (void) _ilu_ReleaseConnIO(cur, ilu_TRUE, err);
      (void) _ilu_QuickReleaseConnCall(cur, &fauxcall, ilu_TRUE, err);
      if (!_ilu_ExitServerMutex(s, ilu_TRUE, err))
	return ilu_FALSE;
    }
    if (ilu_fdstaken <= goal || !_ilu_CanCondition()
	|| _ilu_connCount == 0)
      return ILU_CLER(*err);
    if (timeout) {
      tNow = ilu_FineTime_Now();
      tRem = ilu_FineTime_Sub(*timeout, tNow);
      if (ilu_FineTime_Cmp(tRem, tZero) <= 0)
	return ILU_CLER(*err);
      if (iter < 2) {
	dTimeout = ilu_FineTime_Mul(tRem, waitFracs[iter]);
	cTimeout = ilu_FineTime_Add(tNow, dTimeout);
	pdTimeout = &cTimeout;
      } else
	pdTimeout = timeout;
    }
    if ((urFinger = reduceFinger)) {
      ilu_cardinal    need = ilu_fdstaken - goal;
      while (need) {
	ilu_Transport   curT = (cur = reduceFinger)->co_transport;
	reduceFinger = OthIdlRngNxt(cur);
	s = cur->co_server;
	if (!_ilu_EnterServerMutex(s, ilu_FALSE, err))
	  return ilu_FALSE;
	if (curT->tr_estFDs && cur->co_waiting && !cur->co_closing) {
	  if (!ilu_Check(!(cur->co_mucall || cur->co_nOuts), err))
	    goto dun1;
	  ILU_NOTE(CONNECTION_DEBUG,
		   ("_ilu_ReduceFdsTo requesting connection %p to close.\n",
		    cur));
	  _ilu_LinkConnection(&pendingConns, cur, ilu_pcl);
	  cur->co_closing = cur->co_pending = ilu_TRUE;
	  if (!transport_disableWait(curT, err))
	    goto dun1;
	  need = (curT->tr_estFDs < need) ? need - curT->tr_estFDs : 0;
	}
	if (!_ilu_ExitServerMutex(s, ilu_TRUE, err))
	  return ilu_FALSE;
	if (reduceFinger == urFinger)
	  break;
      }
    }
    {
      ilu_Connection  waitco = pendingConns.prev;
      ilu_Condition   waitee = waitco ? waitco->co_cc : _ilu_connCountChg;
      if (waitco) {
	s = waitco->co_server;
	if (!ilu_EnterServerMutex(s, ilu_FALSE, err))
	  return ilu_FALSE;
	waitco->co_holds++;
      }
      (void) ilu_CMWait2TO(waitee, (waitco ? server_lock(s) : ilu_cmu),
			   ilu_cmu, pdTimeout, err);
      if (waitco) {
	waitco->co_holds--;
	_ilu_MaybeFreeConnection(waitco);
	if (!ilu_ExitServerMutex(s, ilu_TRUE, err))
	  return ilu_FALSE;
      }
      if (ILU_ERRNOK(*err))
	return ilu_FALSE;
    }
    iter++;
  }
  return ILU_CLER(*err);
dun3:
  (void) _ilu_ReleaseConnIO(cur, ilu_TRUE, err);
dun2:
  (void) _ilu_ReleaseConnCall(cur, &fauxcall, ilu_TRUE, err);
dun1:
  (void) _ilu_ExitServerMutex(s, ilu_TRUE, err);
  return ilu_FALSE;
}

/*L1.sup = s; L1 >= {cmu}*/
/*L2 unconstrained*/
ilu_Connection 
_ilu_CreateConnection(ilu_Transport bs,
		      ilu_TransportInfo tinfo,	/* NIL for incoming */
		      ilu_string peerinfo,	/* NIL for outgoing */
		      ilu_Protocol pr, ilu_string pinfo,
		      ilu_Port port,		/* NIL for outgoing */
		      ilu_Server s,
		      ilu_Passport pp,
		      ILU_ERRS((no_memory, internal/check)) * err)
{
  ilu_Error		lerr = ILU_INIT_NO_ERR;
  ilu_Connection	ans;
  static struct _ilu_Connection_s initConn = {0};

  ans = (ilu_Connection) ilu_MallocE(sizeof(*ans), err);
  if (ans == NIL) {
    if (pp) {
      ilu_Error       lerr;
      ilu_DestroyPassport(pp, &lerr);
      ILU_HANDLED(lerr);
    }
    return ans;
  }
  *ans = initConn;
  _ilu_HoldMutex(server_lock(s));
  ans->co_protocol = pr;
  ans->co_port = port;
  ans->co_protocol_data = protocol_create_data_block(pr, pinfo, err);
  if (ILU_ERRNOK(*err))
    goto faild;
  ans->co_pinfo = ilu_StrdupE(pinfo, err);
  if (ILU_ERRNOK(*err))
    goto faild;
  ans->co_transport = bs;
  if (port != NIL)
    conn_peerinfo(ans) = ilu_StrdupE(peerinfo, err);
  else
    conn_tinfo(ans) = _ilu_CopyTinfo(tinfo, err);
  if (ILU_ERRNOK(*err))
    goto faild;
  ans->co_conn_identity.ii_owned_by_passport = ilu_FALSE;
  if (port != NIL) {
    ans->co_conn_identity.ii_type = ilu_ConnectionIdentity;
    ans->co_conn_identity.ii_info = (ilu_refany) conn_peerinfo(ans);
    ans->co_auth_info = pp;
    if (!ilu_AddIdentity (pp, &ans->co_conn_identity, err))
      goto faild;
  } else {
    ans->co_conn_identity.ii_type = ilu_NoIdentity;
    ans->co_auth_info = NIL;
  };
  ans->co_server = s;
  if (_ilu_CanCondition()) {
    char            buf[24];
    sprintf(buf, "%p", ans);
    ans->co_cc = _ilu_CreateCondition(s->sr_id, buf, &lerr);
    ILU_MUST_BE_SUCCESS(lerr);
  } else
    ans->co_cc = NIL;
  ans->co_lsrCares = ilu_TRUE;
  ans->co_next_sn = 1;
  if (connection_incoming(ans)) {
    _ilu_LinkConnection(&port->po_connHead, ans, ilu_psl);
  } else {
    _ilu_LinkConnection(&s->sr_connHead, ans, ilu_psl);
  }
  if (ans->co_transport->tr_estFDs)
    _ilu_LinkConnection(&reapableConns, ans, ilu_reapable);
  else
    _ilu_LinkConnection(&otherIdleConns, ans, ilu_otherIdle);
  _ilu_connCount++;
#ifdef ENABLE_DEBUGGING
  if ((ilu_DebugLevel & CONNECTION_DEBUG) != 0) {
    if (connection_incoming(ans))
      ilu_DebugPrintf("_ilu_CreateConnection:  %p; CV %p, transport %p\n",
		      ans, ans->co_cc, bs);
    else {
      ilu_string      t = _ilu_StringifyTinfo(tinfo, err);
      if (ILU_ERRNOK(*err))
	goto faild;
      ilu_DebugPrintf("_ilu_CreateConnection:  %p; CV %p, transport %p\n",
		      ans, ans->co_cc, bs);
      ilu_free(t);
    }
  }
#endif /* ENABLE_DEBUGGING */
  return (ans);
faild:
  if (connection_incoming(ans) && conn_peerinfo(ans) != NIL)
    ilu_free(conn_peerinfo(ans));
  else if (!connection_incoming(ans) && conn_tinfo(ans) != NIL)
    ilu_free(conn_tinfo(ans));
  if (ans->co_pinfo != NIL)
    ilu_free(ans->co_pinfo);
  if (ans->co_protocol_data != NIL)
    (*pr->pr_free_data_block) (ans->co_protocol_data);
  ilu_free(ans);
  if (pp) {
    ilu_Error       lerr;
    ilu_DestroyPassport(pp, &lerr);
    ILU_HANDLED(lerr);
  }
  return NIL;
}

/*L1, L2 unconstrained*/

ilu_boolean ilu_InmemConnection (ilu_Connection conn)
{
  ilu_string tinfo;

  if (connection_incoming(conn))
    tinfo = conn_peerinfo(conn);
  else
    tinfo = conn_tinfo(conn)[0];
  return (strncmp(tinfo, "inmem", 5) == 0);
}

/*L1, L2 unconstrained*/

ilu_boolean ilu_ThreadPerRequest(ilu_Connection conn)
{
  return (connection_protocol(conn)->pr_concurrent_requests && (!ilu_InmemConnection(conn)));
}

/*L1 >= {conn's server}*/
/*L2 as implied by name*/

/*L1 >= {conn's server}*/
ilu_boolean
_ilu_TakeConnIO(ilu_Connection conn, ilu_boolean hard,
		   ILU_ERRS((bad_locks)) * err)
{
  _ilu_HoldMutex(server_lock(conn->co_server));
  if (conn->co_ioing) {
    if (hard)
      ILU_ERR_CONS0(broken_locks, err, 0);
    else
      ILU_ERR_CONS0(bad_locks, err, 0);
    return ilu_FALSE;
  }
  ILU_NOTE_AND((CONNECTION_DEBUG | LOCK_DEBUG),
	("TakeConnIO(%p) succeeds.\n", conn));
  conn->co_ioing = ilu_TRUE;
  return ILU_CLER(*err);
}

/*L1 = {conn's server, cmu}*/
ilu_boolean
_ilu_EnterConnIO(ilu_Connection conn, ilu_boolean hard,
		   ILU_ERRS((bad_locks)) * err)
{
  _ilu_HoldMutex(server_lock(conn->co_server));
  _ilu_HoldMutex(ilu_cmu);
  if (conn->co_ioing) {
    /*
     * We shouldn't actually get here unless the I/O mutex is held
     * by another thread, in which case the runtime should have
     * provided a wait-on-condition operation.  We could also get
     * here if the caller mistakenly holds the I/O mutex.
     */
    if (!_ilu_CanCondition()) {
      if (hard)
	return ILU_ERR_CONS0(broken_locks, err, ilu_FALSE);
      else
	return ILU_ERR_CONS0(bad_locks, err, ilu_FALSE);
    }
    while (conn->co_ioing) {
      ILU_NOTE_AND((CONNECTION_DEBUG | LOCK_DEBUG),
	       ("EnterConnIO(%p) waiting.\n", conn));
      (void) ilu_CMWait2(conn->co_cc, server_lock(conn->co_server),
			 ilu_cmu, err);
      ILU_NOTE_AND((CONNECTION_DEBUG | LOCK_DEBUG),
	       ("EnterConnIO(%p) resuming.\n", conn));
      if (ILU_ERRNOK(*err))
	return ilu_FALSE;
    }
  }
  ILU_NOTE_AND((CONNECTION_DEBUG | LOCK_DEBUG),
	   ("EnterConnIO(%p) succeeds.\n", conn));
  conn->co_ioing = ilu_TRUE;
  return ILU_CLER(*err);
}

/*L1 >= {cmu, conn's server}; L1.sup < trmu*/
ilu_boolean
_ilu_ReleaseConnIO(ilu_Connection conn, ilu_boolean hard,
		   ILU_ERRS((bad_locks, broken_locks)) * err)
{
  _ilu_HoldMutex(ilu_cmu);
  _ilu_HoldMutex(server_lock(conn->co_server));
  if (!conn->co_ioing) {
    if (ILU_ERRNOK(*err))
      return ilu_FALSE;
    else if (hard)
      return ILU_ERR_CONS0(broken_locks, err, ilu_FALSE);
    else
      return ILU_ERR_CONS0(bad_locks, err, ilu_FALSE);
  }
  ILU_NOTE_AND((CONNECTION_DEBUG | LOCK_DEBUG),
	   ("ReleaseConnIO(%p)\n", conn));
  if (conn->co_server->sr_closing == ilu_TRUE)
    _ilu_CloseIoingConnection(conn, ilu_FALSE,
			ilu_ConnShutdownReason_ProcessTermination);
  conn->co_ioing = ilu_FALSE;
  if (_ilu_CanCondition()) {
    ilu_Error       lerr;
    lerr = _ilu_NotifyCondition(conn->co_cc);
    ILU_ERR_SWITCH(lerr) {
      ILU_SUCCESS_CASE;
      ILU_ERR_CASE(CantCondition, v) {
	ILU_HANDLED(lerr);
	return (ILU_ERROK(*err)
		? ILU_ERR_CONS0(broken_locks, err, ilu_FALSE)
		: ilu_FALSE);
      }
    } ILU_ERR_ENDSWITCH;
  }
  return ilu_TRUE;
}

/**L1 = {cmu, conn's server}; Main Remnant holds; L2 >= {conn's iomu}*/
ilu_boolean
_ilu_PushAsNeeded(ilu_Connection conn,
		  ILU_ERRS((IoErrs, bad_locks)) * err)
{
  for (; conn->co_pushme && ILU_ERROK(*err) && !conn->co_closed;) {
    ilu_Transport   t = conn->co_transport;
    if (!ilu_ExitServerMutex(conn->co_server, ilu_TRUE, err))
      goto dun0;
    if (!ilu_ExitMutex(ilu_cmu, ilu_TRUE, err))
      goto dun0;
    if (!(*t->tr_class->tc_push) (t, err)) {
      _ilu_CloseIoingConnection(conn, ilu_FALSE,
			  ilu_ConnShutdownReason_LostProtocolSync);
    } else {
      conn->co_pushme = ilu_FALSE;
      if (_ilu_CanCondition()) {
	ilu_Error       lerr;
	lerr = _ilu_NotifyCondition(conn->co_cc);
	ILU_ERR_SWITCH(lerr) {
	  ILU_SUCCESS_CASE;
	  ILU_ERR_CASE(CantCondition, v) {
	    ILU_HANDLED(lerr);
	    return (ILU_ERROK(*err)
		    ? ILU_ERR_CONS0(broken_locks, err, ilu_FALSE)
		    : ilu_FALSE);
	  }
	} ILU_ERR_ENDSWITCH;
      }
    }
    if (!ilu_ReEnterMutex(ilu_cmu, err))
      goto dun0;
    if (!ilu_EnterServerMutex(conn->co_server, ilu_TRUE, err))
      goto dun0;
  }
  return ilu_TRUE;
dun0:
  return ilu_FALSE;
}

/**L1 = {cmu, conn's server}; Main Remnant holds;
   before: L2 >= {conn's callmu, iomu};
   after:  L2 >= {conn's callmu}, L2 disjoint {conn's iomu}*/
ilu_boolean
_ilu_PushAndReleaseConnIO(ilu_Connection conn, ilu_boolean hard,
			  ILU_ERRS((IoErrs, bad_locks)) * err)
{
  ILU_ERRS((IoErrs, bad_locks)) lerr = ILU_INIT_NO_ERR;
  ilu_boolean     failedHere = ilu_FALSE;
  _ilu_HoldMutex(ilu_cmu);
  _ilu_HoldMutex(server_lock(conn->co_server));
  if (!(conn->co_ioing && conn->co_mucall)) {
    if (ILU_ERRNOK(*err))
      return ilu_FALSE;
    else if (hard)
      return ILU_ERR_CONS0(broken_locks, err, ilu_FALSE);
    else
      return ILU_ERR_CONS0(bad_locks, err, ilu_FALSE);
  }
  ILU_NOTE_AND((CONNECTION_DEBUG | LOCK_DEBUG),
	   ("PushAndReleaseConnIO(%p)\n", conn));
  if (conn->co_server->sr_closing == ilu_TRUE)
    _ilu_CloseIoingConnection(conn, ilu_FALSE,
			ilu_ConnShutdownReason_ProcessTermination);
  if (ILU_ERROK(*err))
  (void) _ilu_PushAsNeeded(conn, &lerr);

  failedHere = ILU_ERRNOK(lerr);
  if (ILU_ERROK(*err))
    *err = lerr;
  else
    ILU_HANDLED(lerr);
  conn->co_ioing = ilu_FALSE;
  if (_ilu_CanCondition()) {
    ilu_Error       lerr;
    lerr = _ilu_NotifyCondition(conn->co_cc);
    ILU_ERR_SWITCH(lerr) {
      ILU_SUCCESS_CASE;
      ILU_ERR_CASE(CantCondition, v) {
	ILU_HANDLED(lerr);
	return (ILU_ERROK(*err)
		? ILU_ERR_CONS0(broken_locks, err, ilu_FALSE)
		: ilu_FALSE);
      }
    } ILU_ERR_ENDSWITCH;
  }
  return !failedHere;
}

/*L1 >= {server}; L2 unconstrained */
ilu_ReplyList 
_ilu_GetQueuedReply(ilu_Call call)
{
  ilu_Connection  conn = call->ca_connection;
  ilu_ReplyList   r, *pr;
  for (pr = &conn->co_replies, r = *pr;
       r != NIL;
       pr = &r->rp_next, r = *pr)
    if (r->rp_SN == call->ca_SN) {
      *pr = r->rp_next;
      return r;
    }
  return NIL;
}

/*L1 = alt?{conn's server}:{conn's server, cmu}*/
ilu_boolean
_ilu_FullEnterConnCallAndWait(ilu_Connection conn, ilu_Call call,
			      ilu_boolean hard, ilu_boolean alt,
			      ilu_boolean doCall,
			      ilu_boolean doWait,
			      ilu_ReplyList * qrl,
			      ILU_ERRS((bad_locks, broken_locks,
					internal)) * err,
			      const char *file, int line)
{
  const char     *which = doCall ? (doWait ? "CW" : "C") : "W";
  if (!ilu_Check(call, err))
    return ilu_FALSE;
  if (!ilu_Check(!alt || conn->co_nOuts > 0, err))
    return ilu_FALSE;
  _ilu_HoldMutex(server_lock(conn->co_server));
  if (!alt)
    _ilu_HoldMutex(ilu_cmu);
  if (doCall && (conn->co_mucall == call ||
		 (conn->co_mucall && !_ilu_CanCondition()))
      ||
      doWait && (conn->co_waiting == call ||
		 (conn->co_waiting && !_ilu_CanCondition()))) {
    if (hard)
      return ILU_ERR_CONS0(broken_locks, err, ilu_FALSE);
    else
      return ILU_ERR_CONS0(bad_locks, err, ilu_FALSE);
  }
  while ((doCall && conn->co_mucall) ||
	 (doWait && conn->co_waiting &&
	  !(qrl && (*qrl || (*qrl = _ilu_GetQueuedReply(call)))))) {
    ILU_NOTE_AND((CONNECTION_DEBUG | LOCK_DEBUG),
		 ("FullEnterConnCallAndWait"
		  "(%p, %p, %s)@%s:%d waits for %p and/or %p.\n",
		  conn, call, which, file, line,
		  conn->co_mucall, conn->co_waiting));
    (void) ilu_CMWait2(conn->co_cc, server_lock(conn->co_server),
		    (alt ? server_lock(conn->co_server) : ilu_cmu),
		       err);
    ILU_NOTE_AND((CONNECTION_DEBUG | LOCK_DEBUG),
		 ("FullEnterConnCallAndWait"
		  "(%p, %p, %s)@%s:%d resumes at %p and/or %p.\n",
		  conn, call, which, file, line,
		  conn->co_mucall, conn->co_waiting));
    if (ILU_ERRNOK(*err))
      return ilu_FALSE;
  }
  if (!ilu_Check(!doCall || !conn->co_pushme, err))
    return ilu_FALSE;
  ILU_NOTE_AND((CONNECTION_DEBUG | LOCK_DEBUG),
	       ("FullEnterConnCallAndWait"
		"(%p, %p, %s)@%s:%d succeeds (qrl=SN #%lu).\n",
		conn, call, which, file, line,
		(qrl && *qrl) ? (long unsigned) (*qrl)->rp_SN : 0));
  if (!conn->co_mucall && !conn->co_nOuts && !connection_closed(conn)) {
    _ilu_Assert(!alt, "FullEnterConnCall alt vs. co_nOuts");
    _ilu_HoldMutex(ilu_cmu);
    if (conn->co_transport->tr_estFDs && !conn->co_waiting)
      _ilu_UnlinkConnection(&reapableConns, conn, ilu_reapable);
    else
      _ilu_UnlinkConnection(&otherIdleConns, conn, ilu_otherIdle);
  }
  if (doCall)
    conn->co_mucall = call;
  if (doWait && !(qrl && *qrl))
    conn->co_waiting = call;
  if (!conn->co_mucall && !conn->co_nOuts && !connection_closed(conn)) {
    if (conn->co_transport->tr_estFDs && !conn->co_waiting)
      _ilu_LinkConnection(&reapableConns, conn, ilu_reapable);
    else
      _ilu_LinkConnection(&otherIdleConns, conn, ilu_otherIdle);
  }
  return ILU_CLER(*err);
}

/**L1 = {cmu, conn's server}; Main Remnant holds;
   before: L2 >= {conn's callmu}, L2 disjoint {conn's iomu};
   after:  L2 disjoint {conn's callmu, iomu}*/
ilu_boolean
_ilu_ReleaseConnCall(ilu_Connection conn, ilu_Call call,
		     ilu_boolean hard,
		     ILU_ERRS((bad_locks, broken_locks)) * err)
{
  ilu_boolean     ans = ilu_TRUE;
  _ilu_HoldMutex(ilu_cmu);
  _ilu_HoldMutex(server_lock(conn->co_server));
  if (conn->co_mucall != call) {
    if (ILU_ERRNOK(*err))
      return ilu_FALSE;
    else if (hard)
      return ILU_ERR_CONS0(broken_locks, err, ilu_FALSE);
    else
      return ILU_ERR_CONS0(bad_locks, err, ilu_FALSE);
  }
  for (; conn->co_pushme && ILU_ERROK(*err) &&
       !connection_closed(conn);) {
    ilu_Transport    t = conn->co_transport;
    /* XXX it would be better if we ensured this couldn't block */
    if (!_ilu_EnterConnIO(conn, ilu_TRUE, err))
      return ilu_FALSE;
    conn->co_pushme = ilu_FALSE;
    if (!ilu_ExitServerMutex(conn->co_server, ilu_TRUE, err))
      return ilu_FALSE;
    if (!ilu_ExitMutex(ilu_cmu, ilu_TRUE, err))
      return ilu_FALSE;
    if (!(*t->tr_class->tc_push) (t, err))
      _ilu_CloseIoingConnection(conn, (ans = ilu_FALSE),
			  ilu_ConnShutdownReason_LostProtocolSync);
    if (!ilu_ReEnterMutex(ilu_cmu, err))
      return ilu_FALSE;
    if (!ilu_EnterServerMutex(conn->co_server, ilu_TRUE, err))
      return ilu_FALSE;
    if (!_ilu_ReleaseConnIO(conn, ilu_TRUE, err))
      return ilu_FALSE;
  }
  ILU_NOTE_AND((CONNECTION_DEBUG | LOCK_DEBUG),
	   ("ReleaseConnCall(%p, %p, holder=%p)\n",
	    conn, call, conn->co_mucall));
  conn->co_mucall = NIL;
  if (conn->co_nOuts == 0 && !connection_closed(conn)) {
    if (conn->co_transport->tr_estFDs && !conn->co_waiting)
      _ilu_LinkConnection(&reapableConns, conn, ilu_reapable);
    else
      _ilu_LinkConnection(&otherIdleConns, conn, ilu_otherIdle);
  }
  if (_ilu_CanCondition()) {
    ilu_Error       lerr;
    lerr = _ilu_NotifyCondition(conn->co_cc);
    if (ILU_ERRNOK(lerr)) {
      ILU_HANDLED(lerr);
      if (ILU_ERRNOK(*err))
	return ilu_FALSE;
      return ILU_ERR_CONS0(broken_locks, err, ilu_FALSE);
    }
  }
  return ans;
}

/*L1 >= {conn's server};
  conn->co_nOuts==0 => L1 >= {cmu}*/
ilu_boolean
_ilu_QuickReleaseConnCall(ilu_Connection conn, ilu_Call call,
		     ilu_boolean hard,
		     ILU_ERRS((bad_locks, broken_locks,
		     internal)) * err)
{
  _ilu_HoldMutex(server_lock(conn->co_server));
  ILU_NOTE_AND((LOCK_DEBUG | CONNECTION_DEBUG),
	("ReleaseConnCall(%p, %p, holder=%p)\n",
	 conn, call, conn->co_mucall));
  if (conn->co_mucall != call) {
    if (ILU_ERRNOK(*err))
      return ilu_FALSE;
    else if (hard)
      return ILU_ERR_CONS0(broken_locks, err, ilu_FALSE);
    else
      return ILU_ERR_CONS0(bad_locks, err, ilu_FALSE);
  }
  if (connection_closed(conn) || ILU_ERRNOK(*err) || !conn->co_pushme)
    0;
  else
    return ilu_Check(ilu_FALSE, err);
  conn->co_mucall = NIL;
  if (conn->co_nOuts == 0 && !connection_closed(conn)) {
    _ilu_HoldMutex(ilu_cmu);
    if (conn->co_transport->tr_estFDs && !conn->co_waiting)
      _ilu_LinkConnection(&reapableConns, conn, ilu_reapable);
    else
      _ilu_LinkConnection(&otherIdleConns, conn, ilu_otherIdle);
  }
  if (_ilu_CanCondition()) {
    ilu_Error       lerr;
    lerr = _ilu_NotifyCondition(conn->co_cc);
    if (ILU_ERRNOK(lerr)) {
      ILU_HANDLED(lerr);
      if (ILU_ERRNOK(*err))
	return ilu_FALSE;
      return ILU_ERR_CONS0(broken_locks, err, ilu_FALSE);
    }
  }
  return ilu_TRUE;
}

/*L1 >= {cmu, conn's server}*/
/**before: L2 disjoint {(conn)'s waitmu};
   after:  L2    >=    {(conn)'s waitmu}*/

void 
_ilu_FullTakeConnWait(ilu_Connection conn, ilu_Call call,
		      const char *file, int line)
{
  ILU_NOTE_AND((CONNECTION_DEBUG | LOCK_DEBUG),
	       ("ilu_TakeConnWait(conn=%p, call=%p)@%s:%d\n",
		conn, call, file, line));
  _ilu_Assert(!conn->co_waiting, "ilu_TakeConnWait in connect.c");
  conn->co_waiting = call;
  if (!conn->co_mucall && !conn->co_nOuts && !connection_closed(conn)
      && conn->co_transport->tr_estFDs) {
    _ilu_UnlinkConnection(&reapableConns, conn, ilu_reapable);
    _ilu_LinkConnection(&otherIdleConns, conn, ilu_otherIdle);
  }
  return;
}

/*L1 >= {cmu, conn's server}; L1.sup < trmu*/
/**before: L2    >=    {(conn)'s waitmu};
   after:  L2 disjoint {(conn)'s waitmu}*/
ilu_boolean
_ilu_FullReleaseConnWait(ilu_Connection conn, ilu_Call call,
			 ilu_boolean hard,
			 ILU_ERRS((bad_locks, broken_locks)) * err,
			 const char *file, int line)
{
  ilu_Error       lerr;
  ilu_boolean     ans;
  ILU_NOTE_AND((CONNECTION_DEBUG | LOCK_DEBUG),
	("ilu_ReleaseConnWait(conn=%p, call=%p)@%s:%d\n",
	 conn, call, file, line));
  if (!ilu_Check(call != NIL, err))
    return ilu_FALSE;
  if (conn->co_waiting != call) {
    if (ILU_ERRNOK(*err))
      return ilu_FALSE;
    else if (hard)
      return ILU_ERR_CONS0(broken_locks, err, ilu_FALSE);
    else
      return ILU_ERR_CONS0(bad_locks, err, ilu_FALSE);
  }
  if (!conn->co_mucall && !conn->co_nOuts && !connection_closed(conn)
      && conn->co_transport->tr_estFDs) {
    _ilu_UnlinkConnection(&otherIdleConns, conn, ilu_otherIdle);
    _ilu_LinkConnection(&reapableConns, conn, ilu_reapable);
  }
  conn->co_waiting = NIL;
  if (conn->co_closing && !conn->co_closed) {
    ilu_boolean     takeio = !conn->co_ioing;
    if (takeio)
      (void) _ilu_TakeConnIO(conn, ilu_TRUE, &lerr);
    _ilu_CloseIoingConnection(conn, ilu_FALSE,
			ilu_ConnShutdownReason_ProcessTermination);
    if (takeio)
      (void) _ilu_ReleaseConnIO(conn, ilu_TRUE, &lerr);
  } else if (_ilu_CanCondition())
    (void) ilu_CondNotify(conn->co_cc, &lerr);
  else
    ILU_CLER(lerr);
  ans = ILU_ERROK(lerr);
  if (ILU_ERROK(*err))
    *err = lerr;
  else
    ILU_HANDLED(lerr);
  return ans;
}

/*L1 >= {cmu}; L2 unconstrained*/

static ilu_Connection handoff = NIL;
static ilu_boolean handoffReady = ilu_FALSE;
/* L1, L2 unconstrained */
ilu_Condition   _ilu_connHandoffChange = NIL;

/*L1.sup < cmu; L2 unconstrained*/

ilu_boolean
_ilu_HandOffNewConnection(ilu_Connection conn,
			  ILU_ERRS((bad_locks, broken_locks,
				    internal)) * err)
{
  ILU_ERRS((bad_locks, broken_locks, internal)) lerr;
  if (!ilu_Check(handoffReady, &lerr))
    goto faild;
  if (!ilu_EnterMutex(ilu_cmu, &lerr))
    goto faild;
  while (handoff != NIL) {
    if (!ilu_CMWait1(_ilu_connHandoffChange, ilu_cmu, &lerr))
      goto faild;
  }
  handoff = conn;
  if (!ilu_CondNotify(_ilu_connHandoffChange, &lerr))
    goto faild;
  if (!ilu_ExitMutex(ilu_cmu, ilu_TRUE, &lerr))
    goto faild;
  return ilu_TRUE;
faild:
  if (ILU_ERROK(*err))
    *err = lerr;
  else
    ILU_HANDLED(lerr);
  return ilu_FALSE;
}

ilu_Connection 
ilu_OtherNewConnection(ILU_ERRS((internal)) * err)
{
  ilu_Connection  ans;
  if (!ilu_Check(handoffReady, err))
    return NIL;
  if (!ilu_EnterMutex(ilu_cmu, err))
    return NIL;
  while (handoff == NIL) {
    if (!ilu_CMWait1(_ilu_connHandoffChange, ilu_cmu, err))
      return NIL;
  }
  ans = handoff;
  handoff = NIL;
  if (!ilu_CondNotify(_ilu_connHandoffChange, err))
    return NIL;
  if (!ilu_ExitMutex(ilu_cmu, ilu_TRUE, err))
    return NIL;
  return ans;
}

/*Main Invariant holds; L2 not further constrained*/
ilu_boolean
ilu_NewConnectionGetterForked(ILU_ERRS((internal)) * err)
{
  if (!ilu_Check(_ilu_connHandoffChange != NIL, err))
    return ilu_FALSE;
  handoffReady = ilu_TRUE;
  return ilu_TRUE;
}

/* Main Invariant holds; L2 no further constrained */

ilu_Pipeline
ilu_GetPipeline(ILU_ERRS((no_memory, no_resources, bad_locks,
			  broken_locks, bad_param)) * err)
{
  ilu_Pipeline    pl = ilu_MallocE(sizeof(*pl), err);
  if (!pl)
    return pl;
  pl->pl_nCalls = 0;
  pl->pl_lsrCares = ilu_TRUE;
  return pl;
}

ilu_boolean
ilu_ReleasePipeline(ilu_Pipeline pl,
		    ILU_ERRS((bad_locks, broken_locks,
			      bad_param)) *err)
{
  if (!pl)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, ilu_FALSE);
  if (!ilu_EnterMutex(ilu_cmu, err))
    return ilu_FALSE;
  pl->pl_lsrCares = ilu_FALSE;
  _ilu_MaybeFreePipeline(pl);
  if (!ilu_ExitMutex(ilu_cmu, ilu_TRUE, err))
    return ilu_FALSE;
  return ilu_TRUE;
}

/* L1 >= {cmu}; L2 unconstrained */
void _ilu_MaybeFreePipeline(ilu_Pipeline pl)
{
  if (pl && !(pl->pl_lsrCares || pl->pl_nCalls))
    ilu_free(pl);
  return;
}

/* L1 = {server}; Main Remnant holds */
ilu_Serializer
ilu_InnerGetSerializer(ilu_Server server,
		  ILU_ERRS((no_memory, no_resources, bad_locks,
			    broken_locks, bad_param)) * err)
{
  ilu_Serializer  si = ilu_MallocE(sizeof(*si), err);
  if (!si)
    return NIL;
  if (!ilu_DeltaServerHolds(server, 1, err))
    goto dun;
  si->si_server = server;
  si->si_conn = NIL;
  si->si_nCalls = 0;
  si->si_lsrCares = ilu_TRUE;
  si->si_connecting = ilu_FALSE;
  si->si_connChg = NIL;
dun:
  if (ILU_ERRNOK(*err)) {
    ilu_free(si);
    return NIL;
  }
  return si;
}

ilu_Serializer
ilu_GetSerializer(ilu_Server server,
		  ILU_ERRS((no_memory, no_resources, bad_locks,
			    broken_locks, bad_param)) * err)
{
  ilu_Serializer  ans;
  if (!ilu_EnterServerMutex(server, ilu_FALSE, err))
    return NIL;
  ans = ilu_InnerGetSerializer(server, err);
  if (!ilu_ExitServerMutex(server, ilu_TRUE, err))
    return NIL;
  return ans;
}

ilu_boolean
ilu_ReleaseSerializer(ilu_Serializer si,
		      ILU_ERRS((bad_locks, broken_locks,
				bad_param)) * err)
{
  ilu_Server      server;
  if (!si || !(server = si->si_server))
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, ilu_FALSE);
  if (!ilu_EnterServerMutex(server, ilu_FALSE, err))
    return ilu_FALSE;
  if (!ilu_DeltaServerHolds(server, -1, err))
    goto dun1;
  si->si_lsrCares = ilu_FALSE;
  _ilu_MaybeFreeSerializer(si);
dun1:
  if (!ilu_ExitServerMutex(server, ilu_TRUE, err))
    return ilu_FALSE;
  return ilu_TRUE;
}

/* L1 >= {si->si_server}; L2 unconstrained */
void _ilu_MaybeFreeSerializer(ilu_Serializer si)
{
  if (si && !(si->si_lsrCares || si->si_nCalls)) {
    if (si->si_conn) {
      si->si_conn->co_serialer = NIL;
      si->si_conn = NIL;
    }
    if (si->si_connChg) {
      ILU_ERRS((CantCondition)) lerr;
      lerr = ilu_DestroyCondition(si->si_connChg);
      ILU_MUST_BE_SUCCESS(lerr);
    }
    ilu_free(si);
  }
  return;
}
