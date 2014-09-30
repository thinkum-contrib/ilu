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
/* $Id: port.c,v 1.83 1999/08/03 01:52:49 janssen Exp $ */

#define _POSIX_SOURCE

#include "iluntrnl.h"

#include "port.h"
#include "mooring.h"
#include "iluprotocol.h"
#include "ilutransport.h"
#include "object.h"
#include "server.h"

/* L1 >= {server}; L2 unconstrained */

static          ilu_boolean
LinkPort(ilu_PortLinks * head, ilu_Port p,
	 ILU_ERRS((internal)) * err)
{
  if (!ilu_Check((head->pl_prev == NIL && head->pl_next == NIL) ||
		 (head->pl_prev != NIL && head->pl_next != NIL &&
		  head->pl_prev->po_links.pl_next == NIL &&
		  head->pl_next->po_links.pl_prev == NIL),
		 err))
    return ilu_FALSE;
  p->po_links.pl_next = NIL;
  p->po_links.pl_prev = head->pl_prev;
  if (p->po_links.pl_prev != NIL)
    p->po_links.pl_prev->po_links.pl_next = p;
  else
    head->pl_next = p;
  head->pl_prev = p;
  return ilu_TRUE;
}

int             ilu_check_UnlinkPort = 0;

static          ilu_boolean
UnlinkPort(ilu_PortLinks * head, ilu_Port p,
	   ILU_ERRS((internal)) * err)
{
  if (!ilu_Check((p->po_links.pl_prev == NIL)
		 ? head->pl_next == p
		 : p->po_links.pl_prev->po_links.pl_next == p,
		 err))
    return ilu_FALSE;
  if (!ilu_Check((p->po_links.pl_next == NIL)
		 ? head->pl_prev == p
		 : p->po_links.pl_next->po_links.pl_prev == p,
		 err))
    return ilu_FALSE;
  if (ilu_check_UnlinkPort) {
    ilu_Port        pp;
    for (pp = head->pl_next; pp; pp = pp->po_links.pl_next)
      if (pp == p)
	goto found;
    _ilu_Assert(ilu_FALSE, "port not found in UnlinkPort");
found: /* nothing */;
  }
  if (p->po_links.pl_prev != NIL)
    p->po_links.pl_prev->po_links.pl_next = p->po_links.pl_next;
  else
    head->pl_next = p->po_links.pl_next;
  if (p->po_links.pl_next != NIL)
    p->po_links.pl_next->po_links.pl_prev = p->po_links.pl_prev;
  else
    head->pl_prev = p->po_links.pl_prev;
  return ilu_TRUE;
}

/*Main Invariant holds; L2 disjoint {port's iomu, waitmu}*/

ilu_boolean
ilu_SetConnectionRequestHandler(ilu_Port port,
				ilu_TransportInputHandler proc,
				ilu_refany rock,
				ILU_ERRS((no_memory, imp_limit,
					  no_resources, bad_param,
					  bad_locks, internal,
					  broken_locks)) * err)
{
  ilu_Mooring     m = port->po_mooring;
  ilu_Server      s = port_server(port);
  ilu_boolean     immediate = ilu_FALSE;
  if (!ilu_EnterMutex(ilu_cmu, err))
    return ilu_FALSE;
  if (!_ilu_EnterServerMutex(s, ilu_FALSE, err))
    goto dun1;
  immediate = port->po_closed;
  if (immediate)
    goto dun1;
  if (!_ilu_TakePortWait(port, ilu_FALSE, err))
    goto dun2;
  if (!_ilu_TakePortIO(port, ilu_FALSE, err))
    goto dun3;
  port->po_tih.proc = proc;
  port->po_tih.rock = rock;
  immediate = !((*m->mo_set_req_handler)
		(m, (proc ? &port->po_tih : NIL), err));
  (void) _ilu_ReleasePortIO(port, ilu_TRUE, err);
dun3:
  (void) _ilu_ReleasePortWait(port, ilu_TRUE, err);
dun2:
  (void) _ilu_ExitServerMutex(s, ilu_TRUE, err);
dun1:
  (void) ilu_ExitMutex(ilu_cmu, ilu_TRUE, err);
  if (immediate && ILU_ERROK(*err))
    (*proc) (rock);
  return ILU_ERROK(*err);
}

ilu_boolean
ilu_WaitForPortConnectionRequest(ilu_Port port,
				 ILU_ERRS((bad_locks, broken_locks,
					   interrupted)) * err)
{
  ilu_Mooring     m = port->po_mooring;
  ilu_Server      s = port_server(port);
  int             disabled = 1;
  ilu_boolean     domore = ilu_TRUE;
  if (!ilu_EnterMutex(ilu_cmu, err))
    return ilu_FALSE;
  if (!_ilu_EnterServerMutex(s, ilu_FALSE, err))
    goto dun1;
  if (port->po_closed)
    goto dun2;
  if (port->po_waiting) {
    ILU_ERR_CONS0(bad_locks, err, (void) 6);
    goto dun2;
  }
  if (!ilu_Check(!port->po_closing, err))
    goto dun2;
  while (disabled && domore) {
    if (m->mo_wc) {
      ilu_WaitCohort  wc = m->mo_wc;
      while (wc->iluwc_waitsDisabled && !port->po_closed) {
	if (!ilu_CMWait2(wc->iluwc_change, server_lock(s), ilu_cmu, err))
	  goto dun2;
	if (!ilu_Check(port->po_closed || !port->po_closing, err))
	  goto dun2;
      }
    }
    if (port->po_closed)
      goto dun2;
    if (!_ilu_TakePortWait(port, ilu_FALSE, err))
      goto dun2;
    (void) _ilu_ExitServerMutex(s, ilu_TRUE, err);
    (void) ilu_ExitMutex(ilu_cmu, ilu_TRUE, err);
    if (ILU_ERRNOK(*err))
      return ilu_FALSE;
    domore = ((*m->mo_wait_for_req) (m, &disabled, err));
    ILU_ERR_SWITCH(*err) {
      ILU_SUCCESS_CASE
	ILU_HANDLED(*err);
      ILU_ERR_CASE(interrupted, x)
	ILU_HANDLED(*err);	/* ignore and repeat the loop */
      ILU_ERR_ELSE
	return ilu_FALSE;
    } ILU_ERR_ENDSWITCH;
    if (!ilu_ReEnterMutex(ilu_cmu, err))
      return ilu_FALSE;
    if (!_ilu_EnterServerMutex(s, ilu_TRUE, err))
      return ilu_FALSE;
    (void) _ilu_ReleasePortWait(port, ilu_TRUE, err);
  }
dun2:
  (void) _ilu_ExitServerMutex(s, ilu_TRUE, err);
dun1:
  (void) ilu_ExitMutex(ilu_cmu, ilu_TRUE, err);
  return ILU_ERROK(*err);
}

ilu_Connection
ilu_HandleNewConnection(ilu_Port port, ilu_boolean * closed,
			ILU_ERRS((IoErrs, bad_locks,
				  no_resources)) * err)
{
  ilu_Transport   t = NIL;
  ilu_Connection  c = NIL;
  ilu_Server      s = port->po_server;
  ilu_Mooring     m = port->po_mooring;
  ilu_integer     dfd = 0;
  ilu_Error       lerr;
  ilu_string      peerinfo = NIL;
  ilu_Passport    pp;

  *closed = ilu_FALSE;
  pp = ilu_CreatePassport(NIL, err);
  if (ILU_ERRNOK(*err))
    return NIL;

  _ilu_AcquireMutex(ilu_cmu);
  _ilu_AcquireServerMutex(s);
  *closed = port->po_closed;
  if (port->po_closed)
    goto dun2;
  if (!_ilu_TakePortWait(port, ilu_FALSE, err))
    goto dun2;
  if (!_ilu_TakePortIO(port, ilu_FALSE, err))
    goto dun3;
  dfd = (*m->mo_dfd) (m, ilu_TRUE);
  if (ilu_fdbudget < ilu_fdstaken + dfd) {
    (void) _ilu_ReleasePortIO(port, ilu_TRUE, err);
    (void) _ilu_ReleasePortWait(port, ilu_TRUE, err);
    if (ILU_ERRNOK(*err))
      goto dun2;
    _ilu_ReleaseServerMutex(s);
    if (!_ilu_ReduceFdsTo(ilu_fdbudget - dfd, NIL, err))
      goto dun1;
    if ((ilu_fdbudget < ilu_fdstaken + dfd) && (dfd > 0)) {
      ILU_NOTE(CONNECTION_DEBUG,
	    ("HandleNewConnection: FD budget exhausted.\n"));
      _ilu_ReleaseMutex(ilu_cmu);
      ilu_DestroyPassport(pp,&lerr);
      ILU_HANDLED(lerr);
      return NIL;
    }
    _ilu_AcquireServerMutex(s);
    if (!_ilu_TakePortWait(port, ilu_TRUE, err))
      goto dun2;
    if (!_ilu_TakePortIO(port, ilu_TRUE, err))
      goto dun3;
  }
  _ilu_ReleaseServerMutex(s);
  _ilu_ReleaseMutex(ilu_cmu);
  t = (*m->mo_accept_connection) (m, &peerinfo, &dfd, pp, err);
  ilu_DeltaFD(dfd);
  _ilu_AcquireMutex(ilu_cmu);
  _ilu_AcquireServerMutex(s);
  if (port->po_closed) {
    *closed = ilu_TRUE;
    if (t) {
      ilu_integer     cdfd;
      transport_close(t, &cdfd, err);
      ilu_DeltaFD(-cdfd);
    }
  } else if (t != NIL) {
    c = _ilu_CreateConnection(t, NIL, peerinfo, port_protocol(port),
			      port->po_pinfo, port,
			      port_server(port), pp, err);
    ILU_NOTE(CONNECTION_DEBUG,
	     ("ilu_HandleNewConnection(%p):  new connection %p (%s)"
	      " to server \"%s\"\n",
	      port, c,
	      (peerinfo == NIL) ? "NIL peerinfo!" : peerinfo,
	      server_id(s)));
    if (ILU_ERRNOK(*err))
      c = NIL;
  }
  ilu_free(peerinfo);
  (void) _ilu_ReleasePortIO(port, ilu_TRUE, err);
dun3:
  (void) _ilu_ReleasePortWait(port, ilu_TRUE, err);
dun2:
  _ilu_ReleaseServerMutex(s);
dun1:
  _ilu_ReleaseMutex(ilu_cmu);
  if ((pp != NIL) && (c == NIL)) {
    ilu_DestroyPassport(pp, &lerr);
    ILU_HANDLED(lerr);
  }
  return (ILU_ERROK(*err) ? c : NIL);
}

/*L1, L2 unconstrained*/
static int      call_cache_size = 5;

static ilu_boolean 
PortIsLocal(ilu_string protocolinfo,
	    ilu_TransportInfo mooringinfo)
{
  static ilu_TransportInfo local_tinfo = NIL;
  ilu_boolean     port_is_local;
  ilu_string      str;
  int             i;
  if (local_tinfo == NIL)
    local_tinfo = ilu_LocalTransportInfo();
  for (i = 0, port_is_local = ilu_TRUE;
       (local_tinfo[i] != NIL) || (mooringinfo[i] != NIL);
       i++) {
    if ((mooringinfo[i] == NIL) || (local_tinfo[i] == NIL)) {
      port_is_local = ilu_FALSE;
      break;
    }
    if (((str = strchr(local_tinfo[i], '_')) == NIL &&
	 (strcmp(local_tinfo[i], mooringinfo[i]) != 0)) ||
	((str != NIL) &&
	 ((strncmp(local_tinfo[i], mooringinfo[i],
		   str - local_tinfo[i]) != 0) ||
	  ((mooringinfo[i][str - local_tinfo[i]] != '\0') &&
	   (mooringinfo[i][str - local_tinfo[i]] != '_'))))) {
      port_is_local = ilu_FALSE;
      break;
    }
  }
  return port_is_local;
}

/*Main Invariant holds; L2 not further constrained*/

ilu_Port 
ilu_CreatePort(ilu_Server s,
	       ilu_string protocolinfo,
	       ilu_TransportInfo mooringinfo,
	       ilu_Passport pp,
	       ILU_ERRS((IoErrs, inv_objref, no_resources,
			 bad_locks)) * err)
{ return ilu_FullCreatePort(s, protocolinfo, mooringinfo, pp,
			    ilu_TRUE, err); }

ilu_Port 
ilu_FullCreatePort(ilu_Server s,
		   ilu_string protocolinfo,
		   ilu_TransportInfo mooringinfo,
		   ilu_Passport pp,
		   ilu_boolean be_public,
		   ILU_ERRS((IoErrs, inv_objref, no_resources,
			     bad_locks)) * err)
{
  ilu_Protocol    p;
  ilu_TransportCreator tcr = NIL;
  ilu_Port        ans = NIL;
  struct _ilu_Port_s p0 = {0};
  ilu_integer     dfd, cdfd;
  ilu_boolean     port_is_local;
  ilu_boolean	  prtCinfo = ilu_DebugLevel & (EXPORT_DEBUG|SERVER_DEBUG);
  ilu_string	  newCinfo = NIL;
  ilu_CharBuf	  tempCinfo = {0};

  if (protocolinfo == NIL)
    return (ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_pinfo, NIL));
  else if (mooringinfo == NIL)
    return (ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_tinfo, NIL));
  
  /* Check for local port */
  port_is_local = PortIsLocal(protocolinfo, mooringinfo);
  be_public = be_public && !port_is_local;
  
  if (!(tcr = _ilu_GetTransportCreator(mooringinfo, err)))
    goto fale0;
  if ((p = _ilu_GetProtocolFromInfo(protocolinfo)) == NIL) {
    ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_pc, (void) 6);
    goto fale0;
  }

  if ((p->pr_needs_boundaried_transport && (!tcr->tcr_boundaried)) ||
      (p->pr_needs_reliable_transport && (!tcr->tcr_reliable))) {
    ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_tinfo, 0);
    goto fale0;
  };

  dfd = (*tcr->tcr_dfd) (tcr, ilu_TRUE);

  if (!ilu_EnterMutex(ilu_cmu, err))
    goto fale0;
  if (!_ilu_ReduceFdsTo(ilu_fdbudget - dfd, NIL, err))
    goto fale1;
  if ((ilu_fdbudget < ilu_fdstaken + dfd) && (dfd > 0)) {
    ILU_ERR_CONS1(no_resources, err, minor, ilu_nrm_fds, NIL);
    ILU_ERRPRINTF("%s %s %s because over FD budget\n",
		  "ilu_CreatePort:  can't create port",
		  protocolinfo, mooringinfo[0]);
    goto fale1;
  }
  ilu_DeltaFD(dfd);
  if (!_ilu_EnterServerMutex(s, ilu_FALSE, err))
    goto fale1;
  ans = (ilu_Port) ilu_MallocE(sizeof(*ans), err);
  if (!ans)
    goto fale2;
  *ans = p0;
  ans->po_server = s;
  ans->po_tcr = tcr;
  ans->po_public = be_public;
  if (!tcr->tcr_reliable) {
    int             i;
    static const ilu_CachedCall c0 = {"\0", 0, NIL, NIL, {NIL, 0}};
    ans->po_call_cache = (ilu_CachedCall *)
      ilu_MallocE(call_cache_size * sizeof(ilu_CachedCall), err);
    if (!ans->po_call_cache)
      goto fale2;
    ans->po_call_cache_size = call_cache_size;
    for (i = 0; i < ans->po_call_cache_size; i++)
      ans->po_call_cache[i] = c0;
  } else {
    ans->po_call_cache = NIL;
    ans->po_call_cache_size = 0;
  }
  ans->po_call_cache_finger = 0;
  if (!(ans->po_pinfo = _ilu_Strdup(protocolinfo)))
    goto fale2;
  ans->po_protocol = p;
  ans->po_prot_data = protocol_create_data_block(p, protocolinfo, err);
  if (ILU_ERRNOK(*err)) goto fale2;
  ans->po_mooring = ((*tcr->tcr_createMooring)
		     (tcr, &ans->po_tinfo, ilu_TRUE, &cdfd, pp, err));
  if (ILU_ERRNOK(*err))
    goto fale2;
  if (be_public) {
    ilu_cardinal    oldCinfoLen = s->sr_cinfo.icb_len;
    if (!ilu_Append1Cinfo(&s->sr_cinfo,
			  ans->po_pinfo, ans->po_tinfo, err))
      goto fale2;
    newCinfo = s->sr_cinfo.icb_base + oldCinfoLen;
  } else {
#ifdef ENABLE_DEBUGGING
    if (prtCinfo) {
      if (!ilu_Append1Cinfo(&tempCinfo,
			    ans->po_pinfo, ans->po_tinfo, err))
	goto fale2;
      newCinfo = tempCinfo.icb_base;
    }
#endif
  }
  ilu_DeltaFD(cdfd - dfd);
  if (!port_is_local) {
    if (!LinkPort(&s->sr_ports, ans, err))
      goto fale2;
    if (be_public && !server_default_port(s))
      server_default_port(s) = ans;
  } else
    server_local_port(s) = ans;
  ans->po_lsrCares = ilu_TRUE;
#ifdef ENABLE_DEBUGGING
  ILU_NOTE((EXPORT_DEBUG | SERVER_DEBUG),
	   ("ilu_CreatePort:  new port %p, mo=%p,"
	    " cinfo=%s, %s %s%son server \"%s\".\n",
	    ans, ans->po_mooring,
	    (newCinfo ? newCinfo : "(ops, timing splinter)"),
	    (be_public?"public":"private"),
	    (port_is_local) ? "(local) " : "",
	    (server_default_port(s) == ans) ? "(default) " : "",
	    server_id(s)));
  if (newCinfo && !be_public) {
    ilu_free(newCinfo);
    tempCinfo.icb_base = NIL;
    tempCinfo.icb_len = tempCinfo.icb_size = 0;
  }
#endif				/* ENABLE_DEBUGGING */
  if (!_ilu_ExitServerMutex(s, ilu_TRUE, err))
    goto fale1;
  if (!ilu_ExitMutex(ilu_cmu, ilu_TRUE, err))
    goto fale0;
  goto dun;
fale2:
  (void) _ilu_ExitServerMutex(s, ilu_TRUE, err);
fale1:
  (void) ilu_ExitMutex(ilu_cmu, ilu_TRUE, err);
fale0:
  if (ans) {
    if (ans->po_call_cache)
      ilu_free(ans->po_call_cache);
    if (ans->po_pinfo)
      ilu_free(ans->po_pinfo);
    ilu_free(ans);
    ans = NIL;
  }
  if (tcr)
    (*tcr->tcr_close) (tcr);
dun:
  return ans;
}

/* L1, L2 unconstrained */

ilu_boolean
ilu_PortCInfo(ilu_Port p,
	      ilu_string *pinfo,
	      ilu_TransportInfo *tinfo,
	      ILU_ERRS((bad_param, internal, no_memory)) *err)
{
  if (!p || !pinfo || !tinfo)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, ilu_FALSE);
  *pinfo = p->po_pinfo;
  *tinfo = p->po_tinfo;
  return ILU_CLER(*err);
}

ilu_boolean
  ilu_InmemPort (ilu_Port p)
{
  return (strncmp(p->po_tinfo[0], "inmem", 5) == 0);
}

/* L1 >= {port's server}; L2 unconstrained */

ilu_boolean 
_ilu_MaybeFreePort(ilu_Port port,
		   ILU_ERRS((internal)) * err)
{
  ilu_Server      s = port->po_server;
  int             i;
  if (port->po_lsrCares || !port->po_closed || port->po_waiting
      || port->po_ioing || port_connections(port)
      || port->po_closedConns.next)
    return ILU_CLER(*err);
  if (!UnlinkPort(&s->sr_closedPorts, port, err))
    return ilu_FALSE;
  ILU_NOTE(EXPORT_DEBUG,
	   ("ILU:  port %p on server %p=%s being freed;\n"
   "\tserver's ports:=%s %s %s, conns=%s %s, objs=%d, LSSes=%d.\n",
	    port, s, server_id(s),
	    (server_ports(s) ? "X" : "0"),
	    (s->sr_local_port ? "X" : "0"),
	    (s->sr_closedPorts.pl_next ? "X" : "0"),
	    (s->sr_connHead.next ? "X" : "0"),
	    (s->sr_closedConns.next ? "X" : "0"),
	    (s->sr_objs ? ilu_hash_PairsInTable(s->sr_objs) : 0),
	    _ilu_ServerLSSCount(s)));
  if (port->po_call_cache != NIL)
    for (i = 0; i < port->po_call_cache_size; i++)
      if (port->po_call_cache[i].cc_replyMsg.msg_base != NIL)
	ilu_free(port->po_call_cache[i].cc_replyMsg.msg_base);
  if (port->po_prot_data != NIL)
    protocol_free_data_block(port->po_protocol, port->po_prot_data);
  ilu_free(port->po_tinfo);
  ilu_free(port->po_pinfo);
  ilu_free(port);
  return ilu_TRUE;
}


/*L1_sup < cmu; L2 unconstrained*/

void ilu_ClosePort(ilu_Port port)
{
  ilu_Server s = port->po_server;
  _ilu_AcquireMutex(ilu_cmu);
  _ilu_AcquireServerMutex(s);
  _ilu_ClosePort(port);
  _ilu_ReleaseServerMutex(s);
  _ilu_ReleaseMutex(ilu_cmu);
}

/*L1 >= {cmu, port's server}, L1.sup < trmu; L2 unconstrained*/

void _ilu_ClosePort(ilu_Port port)
{
  ILU_ERRS((no_memory, bad_locks, broken_locks,
	    internal)) lerr = ILU_INIT_NO_ERR;
  ilu_Server      s = port->po_server;
  ilu_Mooring     m = port->po_mooring;
  ilu_TransportCreator tcr = port->po_tcr;
  ilu_integer     dfd;
  if (port_closed(port))
    return;
#ifdef ENABLE_DEBUGGING
  if ((ilu_DebugLevel & CONNECTION_DEBUG) != 0) {
    ilu_string      t = _ilu_StringifyTinfo(port->po_tinfo, &lerr);
    if (ILU_ERRNOK(lerr))
      t = NIL;
    ilu_DebugPrintf("_ilu_ClosePort(%p, mo=%p, pinfo=%s, tinfo=%s,"
		    " waiting=%d)\n",
		    port, m, port->po_pinfo,
		    t ? t : "(stringify failed)",
		    port->po_waiting);
    ilu_free(t);
  }
#endif				/* ENABLE_DEBUGGING */
  if (port->po_waiting) {
    _ilu_Assert(port->po_ioing || _ilu_CanCondition(),
		"_ilu_ClosePort vs. mo threading possibilities");
    if (port->po_disabled || (port->po_closing && port->po_ioing))
      return;
    port->po_closing = ilu_TRUE;
    if (!port->po_ioing) {
      (void) _ilu_TakePortIO(port, ilu_TRUE, &lerr);
      ILU_MUST_BE_SUCCESS(lerr);
      (void) mooring_disableWait(m, &lerr);
      ILU_MUST_BE_SUCCESS(lerr);
      (void) _ilu_ReleasePortIO(port, ilu_TRUE, &lerr);
      ILU_MUST_BE_SUCCESS(lerr);
      port->po_disabled = ilu_TRUE;
    }
    return;
  }
  (void) _ilu_TakePortWait(port, ilu_TRUE, &lerr);
  ILU_MUST_BE_SUCCESS(lerr);
  (void) _ilu_TakePortIO(port, ilu_FALSE, &lerr);
  ILU_MUST_BE_SUCCESS(lerr);
  port->po_closed = ilu_TRUE;
  if (port->po_disabled) {
    mooring_enableWait(m, &lerr);
    ILU_MUST_BE_SUCCESS(lerr);
  } else if (m->mo_wc) {
    _ilu_Assert(_ilu_CanCondition(),
		"non-NIL mo_wc while ST in _ilu_ClosePort");
    (void) ilu_CondNotify(m->mo_wc->iluwc_change, &lerr);
    ILU_MUST_BE_SUCCESS(lerr);
  }
  _ilu_CloseTCR(tcr);
  (*m->mo_close) (m, &dfd, &lerr);
  ILU_MUST_BE_SUCCESS(lerr);
  ilu_DeltaFD(-dfd);
  if (s->sr_default_port == port) {
    s->sr_default_port = server_ports(s);
    while (s->sr_default_port != NIL && port_closed(s->sr_default_port))
      s->sr_default_port = s->sr_default_port->po_links.pl_next;
  }
  if (server_local_port(s) == port)
    server_local_port(s) = NIL;
  else {
    UnlinkPort(&s->sr_ports, port, &lerr);
    ILU_MUST_BE_SUCCESS(lerr);
  }
  LinkPort(&s->sr_closedPorts, port, &lerr);
  ILU_MUST_BE_SUCCESS(lerr);
  (void) _ilu_ReleasePortIO(port, ilu_TRUE, &lerr);
  ILU_MUST_BE_SUCCESS(lerr);
  (void) _ilu_ReleasePortWait(port, ilu_TRUE, &lerr);
  ILU_MUST_BE_SUCCESS(lerr);
  (void) _ilu_MaybeFreePort(port, &lerr);
  ILU_MUST_BE_SUCCESS(lerr);
  return;
}

/*L1.sup < port's server; L2 unconstrained*/
ilu_boolean
ilu_DoneWithPort(ilu_Port port,
		 ILU_ERRS((bad_param, bad_locks, internal)) * err)
{
  ilu_Server      s = port->po_server;
  if (!_ilu_EnterServerMutex(s, ilu_FALSE, err))
    return ilu_FALSE;
  port->po_lsrCares = ilu_FALSE;
#ifdef ENABLE_DEBUGGING
  if ((ilu_DebugLevel & CONNECTION_DEBUG) != 0)
    ilu_DebugPrintf("ilu_DoneWithPort(%p, server=%p)\n",
		    port, s);
#endif				/* ENABLE_DEBUGGING */
  (void) _ilu_MaybeFreePort(port, err);
  (void) _ilu_ExitServerMutex(s, ilu_TRUE, err);
  return ILU_ERROK(*err);
}

/*L2 as implied by name*/

/*L1 >= {port's server}*/
ilu_boolean
_ilu_TakePortIO(ilu_Port port, ilu_boolean hard,
		ILU_ERRS((bad_locks)) * err)
{
  _ilu_HoldMutex(server_lock(port->po_server));
  if (port->po_ioing) {
    if (hard)
      return ILU_ERR_CONS0(broken_locks, err, ilu_FALSE);
    else
      return ILU_ERR_CONS0(bad_locks, err, ilu_FALSE);
  }
  ILU_NOTE(LOCK_DEBUG,
	("TakePortIO(%p) succeeds.\n", port));
  port->po_ioing = ilu_TRUE;
  return ILU_CLER(*err);
}

/*L1 >= {cmu, port's server}; L1.sup < trmu*/
ilu_boolean
_ilu_ReleasePortIO(ilu_Port port, ilu_boolean hard,
		   ILU_ERRS((bad_locks, broken_locks)) * err)
{
  _ilu_HoldMutex(ilu_cmu);
  _ilu_HoldMutex(server_lock(port->po_server));
  if (!port->po_ioing) {
    if (hard)
      return ILU_ERR_CONS0(broken_locks, err, ilu_FALSE);
    else
      return ILU_ERR_CONS0(bad_locks, err, ilu_FALSE);
  }
  ILU_NOTE(LOCK_DEBUG,
	("ReleasePortIO(%p)\n", port));
  port->po_ioing = ilu_FALSE;
  return ilu_TRUE;
}

/*L1 >= {port's server}*/
/**before: L2 disjoint {(port)'s waitmu};
   after:  L2    >=    {(port)'s waitmu}*/
ilu_boolean 
_ilu_TakePortWait(ilu_Port port, ilu_boolean hard,
		  ILU_ERRS((bad_locks, broken_locks)) * err)
{
  if (port->po_waiting) {
    if (hard)
      return ILU_ERR_CONS0(broken_locks, err, ilu_FALSE);
    else
      return ILU_ERR_CONS0(bad_locks, err, ilu_FALSE);
  }
  ILU_NOTE(CONNECTION_DEBUG,
	("ilu_TakePortWait(port=%p)\n", port));
  port->po_waiting = ilu_TRUE;
  return ILU_CLER(*err);
}

/*L1 >= {cmu, port's server}; L1.sup < trmu*/
/**before: L2    >=    {(port)'s waitmu};
   after:  L2 disjoint {(port)'s waitmu}*/
ilu_boolean
_ilu_ReleasePortWait(ilu_Port port, ilu_boolean hard,
		     ILU_ERRS((bad_locks, broken_locks)) * err)
{
  ILU_NOTE(CONNECTION_DEBUG,
	("ilu_ReleasePortWait(port=%p)\n", port));
  if (!port->po_waiting) {
    if (ILU_ERRNOK(*err))
      return ilu_FALSE;
    else if (hard)
      return ILU_ERR_CONS0(broken_locks, err, ilu_FALSE);
    else
      return ILU_ERR_CONS0(bad_locks, err, ilu_FALSE);
  }
  port->po_waiting = ilu_FALSE;
  if (port->po_closing) {
    _ilu_ClosePort(port);
  }
  return ILU_CLER(*err);
}

