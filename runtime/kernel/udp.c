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
/* $Id: udp.c,v 1.66 1999/08/03 01:52:58 janssen Exp $ */
/* Last edited by Mike Spreitzer May 15, 1998 10:31 am PDT */

#include "iluntrnl.h"

#if 0

#include "ilutransport.h"
#include "mooring.h"

#include "ilusock.h"

#include "oscalls.h"

#define max_udp_recv 8200

/*L1, L2, Main unconstrained*/

typedef struct sockaddr_in INetSockAddr;

typedef struct MsgCons_s MsgCons, *MsgList;
struct MsgCons_s {
  /* L1 >= {trmu} */

  void           *msg;
  ilu_cardinal    len;
  MsgList         next;
};

typedef struct {
  int             fd;

  /* L1 >= {trmu} */

  HashTable       perpeers;	/* INetSockAddr* -> UDPParms */
  int             reading;	/* num _ilu_WaitForInputOnFD active */
}              *Sender;

/* L1 >= {trmu} */
static HashTable senders = NIL;	/* int*fd -> Sender */
/*
 * We can use one FD for multiple peers.  But we can't use one FD
 * for multiple connections to the same peer.
 */

typedef struct {
  /* L1, L2 unconstrained */

  INetSockAddr    addr;
  /*
   * Local address for Mooring creation, peer address for Transport
   * creation.
   */
  ilu_string      tinfo_host;
  /* host part of tinfo; never NIL; owned by this struct */

  ilu_boolean     name_given;	/* did tinfo give name or address? */
  ilu_boolean     defaulted;	/* did tinfo give null name/address? */
}              *CreatorParms;

typedef struct {
  /* L1, L2 unconstrained */

  INetSockAddr    addr;		/* this side */
  int             fd;
  ilu_TIH        *tih;		/* !=NIL when req handler reg'd */
}              *MooringParms;
/* (tih) owned by client, not mooring. */

typedef struct udpparms {
  /* L1, L2 unconstrained */

  ilu_boolean     incoming;	/* into server, or out from client? */
  INetSockAddr    addr;		/* the other side */
  ilu_boolean     busyIn, busyOut;	/* msgs open? */
  int             fd;
  ilu_boolean     inIsUsed;	/* meaningful iff incoming */

  /* The remaining members are significant only iff !incoming */

  Sender          sender;

  /* L1 >= {trmu} */

  ilu_boolean     isopen;	/* ilu_TRUE 'till close */
  int             waits;	/* num _ilu_WaitForInputOnFD active */
  int             irq;		/* abort outer this many */
  MsgList         queue;	/* incoming messages */
  ilu_Condition   change;	/* ... in queue */
}              *UDPParms;
/*
 * What goes in the data field of a UDP ilu_Transport.  The FD
 * (socket) is never "connected" to a particular peer --- it's used
 * for many.  The exposed buffers contain whole messages.  When
 * !busyOut, there is no exposed output buffer.
 * 
 * There is a new incoming Transport for each packet received at the
 * Mooring.  inIsUsed is meaningful for incoming transports with
 * !busyIn; it says whether the message is waiting or already
 * consumed.  When !inIsUsed && !busyIn, tr_inNext == tr_inLimit ==
 * length of the message, which is already stored in the exposed
 * buffer.  When inIsUsed && !busyIn, the message has been freed and
 * tr_inBuff == NIL.
 * 
 * All the outgoing Transports of a Sender write to and read from that
 * Sender's FD.  An outgoing transport is freed when it's closed and
 * (waits == 0).  A Sender is never freed.  tr_inBuff == NIL iff
 * !busyIn.
 */

static double _udp_Timeout_to1 = 0.7;
static double _udp_Timeout_toN = 5.0;
static double _udp_Timeout_tto = 20.0;

/**********************************************************************
***********************************************************************
***********************************************************************
***** First, the methods for the UDP Transport ************************
***********************************************************************
***********************************************************************
**********************************************************************/

/*L1, L2, Main unconstrained*/

#if (defined(WIN32) || defined(WIN16))

static int set_doer_non_blocking (int fd)
{
  unsigned long one = 1;
  return (OS_SOCKIOCTL(fd, FIONBIO, &one));
}
#define set_listener_non_blocking	set_doer_non_blocking

#elif (defined(_IS_POSIX) && (!defined(HAS_SOLARIS1_NONBLOCKING_BUG)))

#include <fcntl.h>
static int add_fcntl_flag (int fd, int dflags)
{
  int             flags = fcntl(fd, F_GETFL, 0);
  if (flags < 0)
    return flags;
  return fcntl(fd, F_SETFL, flags | dflags);
}
#define set_listener_non_blocking(fd)	add_fcntl_flag((fd), O_NONBLOCK)
#define set_doer_non_blocking(fd)	add_fcntl_flag((fd), O_NONBLOCK)

#elif (defined(_IS_BSD) && defined(FIONREAD))

static int set_doer_non_blocking (int fd)
{
  int one = 1;
  return (OS_SOCKIOCTL(fd, FIONBIO, &one));
}
#define set_listener_non_blocking	set_doer_non_blocking

#elif (defined(_IS_BSD) && defined(HAS_SOLARIS1_NONBLOCKING_BUG))
static int set_doer_non_blocking (int fd)
{
  int one = 1;
  return (OS_SOCKIOCTL(fd, 0x8004667e, &one));
}
#define set_listener_non_blocking	set_doer_non_blocking

#else	/* not POSIX or BSD or Windows */

#error "Don't know how to do non-blocking I/O for anything but POSIX, Windows, and BSD just now"

#endif

#define set_non_blocking	set_doer_non_blocking

static          CreatorParms
InterpretInfo(ilu_string info,
	      ILU_ERRS((no_memory, inv_objref)) * err)
{
  char            hostname[1000];
  long unsigned   port;
  struct hostent *hp;
  CreatorParms    cp;

  if (sscanf(info, "udp_%999[^_]_%lu", hostname, &port) != 2)
    return ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_ts, NIL);
  cp = (CreatorParms) ilu_MallocE(sizeof(*cp), err);
  if (ILU_ERRNOK(*err))
    return NIL;
  memset((ilu_string) & cp->addr, 0, sizeof(cp->addr));
  cp->addr.sin_family = AF_INET;
  cp->addr.sin_port = htons((ilu_shortcardinal) port);
  cp->addr.sin_addr.s_addr = inet_addr(hostname);
  cp->name_given = (cp->addr.sin_addr.s_addr == -1);
  cp->defaulted = (cp->addr.sin_addr.s_addr == 0 ||
		   strcmp(hostname, "localhost") == 0);
  if (cp->defaulted) {
    if (!ilu_EnterMutex(ilu_trmu, err))
      goto faild;
    if (cp->name_given)
      (void) _ilu_CurrentHostIPAddrString(&cp->tinfo_host,
					  &cp->addr.sin_addr, err);
    else
      cp->tinfo_host = _ilu_CurrentHostIPAddrString(NIL,
					  &cp->addr.sin_addr, err);
    if (ILU_ERRNOK(*err)) {
      /* Nothing else works; let's use the loopback address. */
      cp->addr.sin_addr.s_addr = INADDR_ANY;
      /* See comment at other use of INADDR_ANY */
      cp->tinfo_host = "127.0.0.1";
      ILU_HANDLED(*err);
      ILU_CLER(*err);
    }
    cp->tinfo_host = ilu_StrdupE(cp->tinfo_host, err);
    (void) ilu_ExitMutex(ilu_trmu, ilu_TRUE, err);
    if (ILU_ERRNOK(*err))
      goto faild;
  } else {
    if (cp->addr.sin_addr.s_addr == -1) {
      if ((hp = gethostbyname(hostname)) != NIL
	  && hp->h_addr != NIL)
	memcpy((ilu_string) & cp->addr.sin_addr, hp->h_addr,
	       hp->h_length);
      if (cp->addr.sin_addr.s_addr == -1) {
	ILU_NOTE((CONNECTION_DEBUG | UDP_DEBUG),
	      ("udp:  Invalid host name (%s).\n",
	       hostname));
	ilu_free(cp);
	return ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_ts, NIL);
      }
    }
    cp->tinfo_host = _ilu_Strdup(hostname);
    if (cp->tinfo_host == NIL) {
      ilu_free(cp);
      return ILU_ERR_CONS1(no_memory, err, nbytes, 1 + strlen(hostname),
			   NIL);
    }
  }
  ILU_CLER(*err);
  return cp;
faild:
  ilu_free(cp);
  return NIL;
}

static int Socket(ILU_ERRS((IoErrs)) * err)
{
  int             skt;
  if (OS_SOCKINV(skt = socket(AF_INET, SOCK_DGRAM, 0))) {
    int             theerr = sockerrno;
    ILU_NOTE((EXPORT_DEBUG | UDP_DEBUG),
	  ("udp: create socket failed:  %s.\n",
	   strerror(theerr)));
    switch (theerr) {
    case SOCKERRID(ACCES):		/* permission error */
      return ILU_ERR_CONS0(no_permission, err, -1);
#if !(defined(WIN32) || defined(WIN16))
    case SOCKERRID(NFILE):
      return ILU_ERR_CONS1(no_resources, err, minor, ilu_nrm_ENFILE, -1);
#endif
    case SOCKERRID(MFILE):		/* resource error */
    case SOCKERRID(NOBUFS):
      return ILU_ERR_CONS1(no_resources, err, minor,
			   ((theerr == EMFILE) ? ilu_nrm_EMFILE
			    : ilu_nrm_ENOBUFS), -1);
    case SOCKERRID(PROTONOSUPPORT):	/* type error */
    case SOCKERRID(PROTOTYPE):
      return ILU_ERR_CONS1(comm_failure, err, minor, ilu_cfm_socket_type,
			   -1);
    default:
      return ILU_ERR_CONS1(internal, err, minor, ilu_im_errno, -1);
    }
  }
  return skt;
}

/*L1, L2 unconstrained*/
static ilu_cardinal HashAddr(ilu_refany key, ilu_cardinal modulus)
{
  INetSockAddr   *addr = (INetSockAddr *) key;
  return ((addr->sin_addr.s_addr + addr->sin_port) % modulus);
}

/*L1, L2 unconstrained*/
static ilu_boolean CmpAddr(ilu_refany key1, ilu_refany key2)
{
  INetSockAddr   *addr1 = (INetSockAddr *) key1;
  INetSockAddr   *addr2 = (INetSockAddr *) key2;
  return (addr1->sin_addr.s_addr == addr2->sin_addr.s_addr
	  && addr1->sin_port == addr2->sin_port);
}

/*L1 >= {trmu}; L2 unconstrained*/
static Sender NewSender(ilu_integer *dfd, ILU_ERRS((IoErrs)) * err)
{
  int             fd;
  Sender          ans = ilu_MallocE(sizeof(*ans), err);
  if (ans == NIL)
    return NIL;
  ans->reading = 0;
  ans->perpeers = ilu_hash_MakeNewTable(5, HashAddr, CmpAddr);
  ILU_NOTE((CONNECTION_DEBUG | UDP_DEBUG),
	("udp:  Creating new outgoing socket...\n"));

  *dfd = 0;
  fd = Socket(err);
  if (ILU_ERRNOK(*err))
    return NIL;
  *dfd = 1;
#ifdef SO_REUSEADDR
  (void) setsockopt(fd, SOL_SOCKET, SO_REUSEADDR,
		    (ilu_string) NIL, 0);
#endif				/* SO_REUSEADDR */

#ifdef SO_USELOOPBACK
  (void) setsockopt(fd, SOL_SOCKET, SO_USELOOPBACK,
		    (ilu_string) NIL, 0);
#endif				/* SO_USELOOPBACK */

  if (set_non_blocking(fd) != 0) {
    ILU_NOTE((CONNECTION_DEBUG | UDP_DEBUG),
	  ("udp:  Failed to set FD %d non-blocking.\n",
	   fd));
    return ILU_ERR_CONS1(comm_failure, err, minor, ilu_cfm_nonblock, NIL);
  }
  ILU_NOTE((CONNECTION_DEBUG | UDP_DEBUG),
	("udp:  Created new outgoing socket, FD = %d\n",
	 fd));
  ans->fd = fd;
  _ilu_Assert((int) ilu_hash_AddToTable(senders, &ans->fd, ans),
	      "UDP NewSender");
  return ans;
}

/*L1 >= {trmu}*/
static ilu_boolean
  UseSenderP(ilu_refany entry_data, ilu_refany rock)
{
  Sender          sender = (Sender) entry_data;
  INetSockAddr   *paddr = (INetSockAddr *) rock;
  return (ilu_hash_FindInTable(sender->perpeers, paddr) == NIL);
}

/*L1, L2 unconstrained*/
static ilu_cardinal HashFD(ilu_refany key, ilu_cardinal modulus)
{
  int            *fd = (int *) key;
  return (((unsigned) *fd) % modulus);
}

/*L1, L2 unconstrained*/
static ilu_boolean CmpFD(ilu_refany key1, ilu_refany key2)
{
  int   *fd1 = (int *) key1;
  int   *fd2 = (int *) key2;
  return (*fd1 == *fd2);
}

/*L1_sup < trmu; L2 unconstrained*/

static ilu_integer FdUsage(ilu_TransportCreator tcr, ilu_boolean mooring)
{
  return 1;
}

static ilu_integer CloseDFd(ilu_Transport tr)
{
  UDPParms        parms = (UDPParms) transport_data(tr);
  return 0;
}

/* tih => (Main Invariant holds; L2 disjoint {conn's iomu, callmu}) */
static          ilu_boolean
SetInputHandler(ilu_Transport tr,
		ilu_TIH tih,
		ILU_ERRS((no_memory, internal, no_resources)) * err)
{
  UDPParms        parms = (UDPParms) transport_data(tr);
  if (!parms->incoming)
    return ILU_ERR_CONS1(internal, err, minor, ilu_im_tportRole, ilu_FALSE);
  if (tih)
    (*tih->proc) (tih->rock);
  ILU_CLER(*err);
  return (!tih);
}

/*mxamu = ilu_trmu*/
typedef struct {
  /*L1 >= {ilu_trmu}*/
  
  ilu_Alarmette_s	ita_alarmette;
  ilu_Condition		ita_cc;
} InputTimeoutAlarm;

/*L1_sup = ilu_trmu*/

static void ITInvoke(ilu_Alarmette a);
static void ITSet(ilu_FineTime t);
static void ITCancel(void);

static ilu_Alarmette_s itHead = {&itHead, &itHead, ilu_FALSE, {0, 0}};
static ilu_AlarmRep itar = {&itHead, ITInvoke, ITSet, ITCancel};

/*Main Invariant holds*/
static void ItaInvoke(ilu_private rock)
{
  ilu_FineTime now = ilu_FineTime_Now();
  _ilu_AcquireMutex(ilu_trmu);
  ilu_MXAProc(now, &itar);
  _ilu_ReleaseMutex(ilu_trmu);
  return;
}

static void ITInvoke(ilu_Alarmette a)
{
  ilu_Error err;
  InputTimeoutAlarm *ita = (InputTimeoutAlarm*) a;
  err = _ilu_NotifyCondition(ita->ita_cc);
  ILU_MUST_BE_SUCCESS(err);
  return;
}

static void ITSet(ilu_FineTime t)
{
  ilu_SetAlarm(_ilu_udpAlarm, t, ItaInvoke, NIL);
  return;
}

static void ITCancel(void)
{
  ilu_UnsetAlarm(_ilu_udpAlarm);
  return;
}

/*L1, L2 unconstrained*/
static ilu_boolean TFree(ilu_Transport tr)
{
  UDPParms        parms = (UDPParms) transport_data(tr);
  if (tr->tr_inBuff != NIL)
    ilu_free(tr->tr_inBuff);
  if (tr->tr_outBuff != NIL)
    ilu_free(tr->tr_outBuff);
  while (parms->queue != NIL) {
    MsgList         next = parms->queue->next;
    ilu_free(parms->queue->msg);
    ilu_free(parms->queue);
    parms->queue = next;
  }
  ilu_free(parms);
  ilu_free(tr);
  return ilu_TRUE;
}

/*Main Invariant holds; L2 unconstrained*/

static          ilu_boolean
WaitForInput(ilu_Transport tr, ilu_FineTime * limit,
	     ILU_ERRS((interrupt)) * err)
{
  UDPParms        parms = (UDPParms) transport_data(tr);
  ilu_boolean     sure;
  Sender          sender = parms->sender;
  struct sockaddr from;
  int             fromlen;
  void           *msg;
  int             msglen, res;
  MsgList         msl;
  UDPParms        rcpt;
  ilu_FineTime    now;
  ilu_boolean     threaded = ilu_CanCondition();
  InputTimeoutAlarm ita = {{NIL, NIL, ilu_FALSE, {0, 0}}, NIL};
  if (parms->incoming)
    return ILU_CLER(*err);
  if (!ilu_EnterMutex(ilu_trmu, err))
    return ilu_FALSE;
  while (parms->queue == NIL) {
    if ((sender->reading > 0) && threaded) {
      ilu_boolean     irq = ilu_FALSE;
      if (limit != NIL) {
	ita.ita_cc = parms->change;
	ilu_MXASet(&itar, &ita.ita_alarmette, *limit);
      }
      if (!ilu_CMWait1(parms->change, ilu_trmu, err))
	ILU_ERR_SWITCH(*err) {
	  ILU_SUCCESS_CASE
	    0;
	  ILU_ERR_CASE(broken_locks, v)
	    return ilu_FALSE;
	  ILU_ERR_CASE(interrupted, v)
	    irq = ilu_TRUE;
	} ILU_ERR_ENDSWITCH;
      if (limit != NIL)
	ilu_MXAClear(&itar, &ita.ita_alarmette);
      if (irq)
	goto dun;
    } else {
      int             depth = ++parms->waits;
      sender->reading++;
      if (parms->irq >= depth)
	parms->irq = depth - 1;
      if (!ilu_ExitMutex(ilu_trmu, ilu_TRUE, err)) {
	if (!threaded) {
	  parms->waits--;
	  sender->reading--;
	}
	return ilu_FALSE;
      }
      _ilu_WaitForInputOnFD(parms->fd, &sure, limit, err);
      if (!ilu_ReEnterMutex(ilu_trmu, err)) {
	if (!threaded) {
	  parms->waits--;
	  sender->reading--;
	}
	return ilu_FALSE;
      }
      sender->reading--;
      parms->waits--;
      if (parms->waits == 0 && !parms->isopen)
	return TFree(tr);
      if (ILU_ERRNOK(*err))
	goto dun;
      if (parms->irq >= depth)
	goto dun;
      if (limit != NIL) {
	now = ilu_FineTime_Now();
	if (ilu_FineTime_Cmp(now, *limit) >= 0)
	  goto dun;
      }
      msg = ilu_MallocE(msglen = max_udp_recv, err);
      if (msg == NIL)
	goto dun;
      fromlen = sizeof(from);
      res = recvfrom(sender->fd, msg, msglen, 0, &from, &fromlen);
      if (res >= 0) {
	rcpt = ilu_hash_FindInTable(sender->perpeers, &from);
	if (rcpt != NIL) {
	  msl = ilu_MallocE(sizeof(*msl), err);
	  if (msl == NIL)
	    goto dun;
	  msl->msg = msg;
	  msl->len = res;
	  msl->next = rcpt->queue;
	  rcpt->queue = msl;
	  if (threaded)
	    if (!ilu_CondNotify(rcpt->change, err))
	      goto dun;
	} else {
	  INetSockAddr   *x = (INetSockAddr *) & from;
	  ILU_NOTE(UDP_DEBUG,
		("udp.c: received reply on FD %d from unknown peer 0x%08x:%u\n",
		 sender->fd, x->sin_addr.s_addr, x->sin_port));
	  ilu_free(msg);
	}
      } else if (sockerrno == SOCKERRID(WOULDBLOCK)) {
	_ilu_Assert(!sure, "UDP sure recvfrom EWOULDBLOCK");
      } else {
	_ilu_Assert(sockerrno == SOCKERRID(INTR),
		    "UDP WaitForInput impossible errno");
      }
    }
  }
dun:
  if (!ilu_ExitMutex(ilu_trmu, ilu_TRUE, err))
    return ilu_FALSE;
  return ILU_ERROK(*err);
}

static          ilu_boolean
Interrupt(ilu_Transport tr, ILU_ERRS((bad_param)) * err)
{
  UDPParms        parms = (UDPParms) transport_data(tr);
  if (parms->incoming)
    return ILU_CLER(*err);
  if (!ilu_EnterMutex(ilu_trmu, err))
    return ilu_FALSE;
  parms->irq = parms->waits;
  if (!ilu_ExitMutex(ilu_trmu, ilu_TRUE, err))
    return ilu_FALSE;
  return _ilu_InterruptFD(parms->fd, err);
}

/*Main Invariant holds; L2 >= {conn's iomu} */

static          ilu_ReadHeaderResultCode
BeginMessage(ilu_Transport tr,
	     ilu_boolean input,
	     ILU_ERRS((IoErrs)) * err)
{
  UDPParms        parms = (UDPParms) tr->tr_data;
  ILU_NOTE(UDP_DEBUG,
	("udp(%p): BeginMessage(%s)\n",
	 tr, input ? "input" : "output"));
  if (parms->busyIn || parms->busyOut)
    return ILU_ERR_CONS1(internal, err, minor, ilu_im_beginMessage,
			 ilu_rhrc_error);
  if (input) {
    if (!parms->incoming) {
      MsgList         msl;
      parms->busyIn = ilu_TRUE;
      if (!ilu_EnterMutex(ilu_trmu, err))
	return ilu_rhrc_error;
      msl = parms->queue;
      if (msl == NIL) {
	if (!ilu_ExitMutex(ilu_trmu, ilu_TRUE, err))
	  return ilu_rhrc_error;
	return ilu_rhrc_nothing;
      }
      tr->tr_inBuff = msl->msg;
      tr->tr_inNext = 0;
      tr->tr_inLimit = msl->len;
      parms->queue = msl->next;
      ilu_free(msl);
      if (!ilu_ExitMutex(ilu_trmu, ilu_TRUE, err))
	return ilu_rhrc_error;
      return ilu_rhrc_ok;
    } else if (parms->inIsUsed) {
      ILU_CLER(*err);
      return ilu_rhrc_eof;
    } else {
      parms->busyIn = ilu_TRUE;
      tr->tr_inNext = 0;
      ILU_CLER(*err);
      return ilu_rhrc_ok;
    }
  } else {
    _ilu_Assert(tr->tr_outBuff == NIL, "udp.c:BeginMessage");
    tr->tr_outBuff = ilu_malloc(max_udp_recv);
    if (tr->tr_outBuff == NIL)
      return ILU_ERR_CONS1(no_memory, err, nbytes, max_udp_recv,
			   ilu_rhrc_error);
    parms->busyOut = ilu_TRUE;
    tr->tr_outNext = 0;
    tr->tr_outLimit = max_udp_recv;
    ILU_CLER(*err);
    return ilu_rhrc_ok;
  }
}

static          ilu_boolean
SendWholeMessage(ilu_Transport tr,
		 ilu_Message * msgh,
		 ILU_ERRS((IoErrs)) * err)
{
  UDPParms        parms = (UDPParms) tr->tr_data;
  ilu_bytes       buffer = msgh->msg_base;
  ilu_cardinal    count = msgh->msg_len;
  ilu_integer     sent;
  ILU_NOTE((CONNECTION_DEBUG | UDP_DEBUG),
	("udp(%p):  (re-)sending %lu bytes from %p to fd %d.\n",
	 tr, msgh->msg_len, msgh->msg_base, parms->fd));
  sent = sendto(parms->fd, (char *) buffer, (int) count, 0,
		(struct sockaddr *) & parms->addr,
		sizeof(parms->addr));
  if (sent < 0 || count != (ilu_cardinal) sent) {
    int             the_errno = sockerrno;
    return ILU_ERR_CONS1(comm_failure, err, minor, ilu_cfm_conn_lost,
			 ilu_FALSE);
  }
  return ILU_CLER(*err);
}

static          ilu_boolean
EndMessage(ilu_Transport tr,
	   ilu_boolean flush,
	   ilu_Message * msgh,
	   ILU_ERRS((IoErrs)) * err)
{
  UDPParms        parms = (UDPParms) tr->tr_data;
  ILU_NOTE(UDP_DEBUG,
	("udp(%p): EndMessage(%s)\n",
	 tr, (parms->busyIn ? "input"
	      : parms->busyOut ? "output"
	      : "no dir!")));
  if (parms->busyIn) {
    parms->busyIn = ilu_FALSE;
    tr->tr_inNext = tr->tr_inLimit = 0;
    ilu_free(tr->tr_inBuff);
    tr->tr_inBuff = NIL;
    parms->inIsUsed = ilu_TRUE;
    return ILU_CLER(*err);
  } else if (parms->busyOut) {
    parms->busyOut = ilu_FALSE;
    msgh->msg_base = tr->tr_outBuff;
    msgh->msg_len = tr->tr_outNext;
    tr->tr_outNext = 0;
    tr->tr_outBuff = NIL;

#ifdef ENABLE_DEBUGGING

    if ((ilu_DebugLevel & PACKET_DEBUG) != 0)
      _ilu_debug_DumpPacket(msgh->msg_base, msgh->msg_len, "outgoing UDP");

#endif /* ENABLE_DEBUGGING */

    return SendWholeMessage(tr, msgh, err);
  } else
    return ILU_ERR_CONS1(internal, err, minor, ilu_im_endMessage,
			 ilu_FALSE);
}

static          ilu_boolean
WriteBytes(ilu_Transport tr, ilu_bytes b,
	   ilu_cardinal len, ilu_boolean flush,
	   ILU_ERRS((IoErrs)) * err)
{
  /* Should only come here if exposed buffer isn't big enough. */
  return ILU_ERR_CONS1(marshal, err, minor, ilu_mm_dgramLimit, ilu_FALSE);
}

static          ilu_cardinal
ReadBytes(ilu_Transport tr, ilu_bytes b,
	  ilu_cardinal len, ilu_TransportReport * rpt,
	  ILU_ERRS((IoErrs)) * err)
{
  /* Should only come here after exposed buffer has been consumed. */
  UDPParms        parms = (UDPParms) tr->tr_data;
  ILU_CLER(*err);
  rpt->tr_eom = ilu_TRUE;
  rpt->tr_eof = parms->incoming;
  return 0;
}

/*L1, Main unconstrained; L2 >= {conn's iomu}*/

static          ilu_boolean
Close(ilu_Transport tr, ilu_integer * dfd,
      ILU_ERRS((inernal)) * err)
{
  UDPParms        parms = (UDPParms) transport_data(tr);
  *dfd = 0;
  ILU_CLER(*err);
  if (parms->incoming) {
    ILU_NOTE((CONNECTION_DEBUG | UDP_DEBUG),
	  ("udp(%p):  Close incoming.\n", tr));
    return TFree(tr);
  }
  ILU_NOTE((CONNECTION_DEBUG | UDP_DEBUG),
	("udp(%p):  Close outgoing (waits=%d).\n", tr,
	 parms->waits));
  parms->isopen = ilu_FALSE;
  if (parms->waits == 0)
    TFree(tr);
  return ilu_TRUE;
}

/*L1, L2 unconstrained*/
static struct _ilu_TransportClass_s myClass = {
  ilu_TRUE,				/* boundaried */
  ilu_FALSE,			/* reliable */
  CloseDFd,
  SetInputHandler,
  WaitForInput,
  Interrupt,
  BeginMessage,
  EndMessage,
  SendWholeMessage,
  WriteBytes,
  ReadBytes,
  Close
};

/*Main Invariant holds*/
static          ilu_Transport
CreateTransport(ilu_TransportCreator tcr,
		ilu_boolean buffer,
		ilu_integer *dfd,
		ilu_Passport pp,
		ILU_ERRS((IoErrs)) * err)
{
  CreatorParms    cp = (CreatorParms) tcr->tcr_data;
  ilu_Transport   ans;
  UDPParms        tParms;
  *dfd = 0;
  if (cp->defaulted) {
    ILU_NOTE((CONNECTION_DEBUG | UDP_DEBUG),
	  ("udp(%p):  Don't contact defaulted host.\n",
	   tcr));
    return ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_ts, NIL);
  }
  ans = (ilu_Transport) ilu_MallocE(sizeof(*ans), err);
  if (ans == NIL)
    return ans;
  tParms = (UDPParms) ilu_MallocE(sizeof(*tParms), err);
  if (tParms == NIL) {
    ilu_free(ans);
    return NIL;
  }
  ans->tr_inBuff = ilu_MallocE(max_udp_recv, err);
  if (ans->tr_inBuff == NIL) {
    ilu_free(tParms);
    ilu_free(ans);
    return NIL;
  }
  if (!ilu_EnterMutex(ilu_trmu, err))
    return ilu_FALSE;
  if (senders == NIL) {
    senders = ilu_hash_MakeNewTable(1, HashFD, CmpFD);
    if (senders == NIL)
      return ILU_ERR_CONS1(no_memory, err, nbytes, 0, NIL);
  }
  tParms->sender = ilu_hash_FindViaProc(senders, UseSenderP, &cp->addr);
  if (tParms->sender == NIL)
    tParms->sender = NewSender(dfd, err);
  if (tParms->sender != NIL) {
    tParms->addr = cp->addr;
    tParms->isopen = ilu_TRUE;
    tParms->waits = 0;
    tParms->queue = NIL;
    if (ilu_CanCondition())
      tParms->change = ilu_CreateCondition("a UDP", "transport", err);
    else
      tParms->change = NIL;
    ILU_MUST_BE_SUCCESS(*err);
    _ilu_Assert((int) ilu_hash_AddToTable(tParms->sender->perpeers,
				     &tParms->addr, tParms),
		"UDP CreateTransport hash_AddToTable");
  }
  if (!ilu_ExitMutex(ilu_trmu, ilu_TRUE, err))
    return ilu_FALSE;
  if (tParms->sender == NIL) {
    ilu_free(ans);
    ilu_free(tParms);
    ilu_free(ans->tr_inBuff);
    return NIL;
  }
  ans->tr_inNext = ans->tr_inLimit = 0;
  ans->tr_outBuff = NIL;
  ans->tr_outNext = ans->tr_outLimit = 0;
  ans->tr_class = &myClass;
  ans->tr_data = tParms;
  ans->tr_to1 = ilu_FineTime_FromDouble(_udp_Timeout_to1);
  ans->tr_toN = ilu_FineTime_FromDouble(_udp_Timeout_toN);
  ans->tr_tto = ilu_FineTime_FromDouble(_udp_Timeout_tto);
  ans->tr_wc = XXX;
  tParms->incoming = ilu_FALSE;
  tParms->busyIn = tParms->busyOut = ilu_FALSE;
  tParms->fd = tParms->sender->fd;
  ILU_NOTE((CONNECTION_DEBUG | UDP_DEBUG),
	("udp(%p):  New outgoing transport, via FD %d, to %s:%u\n",
	 ans, tParms->fd, inet_ntoa(cp->addr.sin_addr),
	 ntohs(cp->addr.sin_port)));
  return ans;
}

/**********************************************************************
***********************************************************************
***********************************************************************
**** Now the methods for the UDP Mooring ******************************
***********************************************************************
***********************************************************************
**********************************************************************/

/*L1, L2, Main unconstrained*/

void _ilu_udp_SetTimeouts (double to1, double toN, double tto)
{
  _udp_Timeout_to1 = to1;
  _udp_Timeout_toN = toN;
  _udp_Timeout_tto = tto;
}

static          ilu_string
FormHandle(ilu_string hostname, unsigned port,
	   ilu_boolean altfmt, ILU_ERRS((no_memory)) * err)
{
  char            buf[11];
  ilu_string      ans;
  sprintf(buf, "%lu", ((unsigned long) port) & 0xFFFF);
  if (altfmt)
    ans = _ilu_Strcat5("UDP", " ", hostname, " ", buf);
  else
    ans = _ilu_Strcat5("udp", "_", hostname, "_", buf);
  if (ans == NIL)
    return ILU_ERR_CONS1(no_memory, err, nbytes,
			 6 + strlen(hostname) + strlen(buf), NIL);
  ILU_CLER(*err);
  return ans;
}

/*L1_sup < trmu*/
static          ilu_Transport
AcceptClient(ilu_Mooring m, ilu_string * tinfo_out,
	     ilu_integer * dfd, ilu_Passport pp,
	     ILU_ERRS((IoErrs)) * err)
{
  MooringParms    mParms = (MooringParms) m->mo_data;
  UDPParms        tParms = NIL;
  int             buflen = max_udp_recv;
  INetSockAddr    from;
  SOCKET_SIZE_TYPE fromlen = sizeof(from);
  ilu_bytes       ibuf = NIL;
  int             ilen;
  ilu_Transport   ans = NIL;
  *dfd = 0;
  if (tinfo_out != NIL)
    *tinfo_out = NIL;
  ibuf = (unsigned char *) ilu_malloc(buflen);
  if (ibuf == NIL)
    return ILU_ERR_CONS1(no_memory, err, nbytes, buflen, NIL);
  ilen = recvfrom(mParms->fd, (char *) ibuf, buflen, 0,
		  (struct sockaddr *) & from, &fromlen);
  if (ilen < 0) {
    int             theerr = sockerrno;
    ILU_NOTE((CONNECTION_DEBUG | UDP_DEBUG),
	  ("udp(%p): recvfrom addr %s port %u failed, errno=%d.\n",
	   m, inet_ntoa(from.sin_addr),
	   ntohs(mParms->addr.sin_port), theerr));
    (void) ILU_ERR_CONS1(comm_failure, err, minor, ilu_cfm_connect_failed,
			 6);
    goto faild;
  }
  if (tinfo_out != NIL) {
    *tinfo_out = FormHandle(inet_ntoa(from.sin_addr),
			    ntohs(from.sin_port), ilu_TRUE, err);
    if (*tinfo_out == NIL)
      goto faild;
  }
  tParms = (UDPParms) ilu_malloc(sizeof(*tParms));
  if (tParms == NIL) {
    (void) ILU_ERR_CONS1(no_memory, err, nbytes, sizeof(*tParms), 6);
    goto faild;
  }
  ans = (ilu_Transport) ilu_malloc(sizeof(*ans));
  if (ans == NIL) {
    (void) ILU_ERR_CONS1(no_memory, err, nbytes, sizeof(*ans), 6);
    goto faild;
  }
  tParms->incoming = ilu_TRUE;
  tParms->addr = from;
  tParms->busyIn = tParms->busyOut = tParms->inIsUsed = ilu_FALSE;
  tParms->fd = mParms->fd;
  tParms->sender = NIL;
  tParms->isopen = tParms->waits = tParms->irq = 0;
  tParms->queue = NIL;
  tParms->change = NIL;
  ans->tr_inBuff = ibuf;
  ans->tr_inNext = 0;
  ans->tr_inLimit = ilen;
  ans->tr_outBuff = NIL;
  ans->tr_outNext = ans->tr_outLimit = 0;
  ans->tr_class = &myClass;
  ans->tr_data = tParms;
  ans->tr_to1 = ilu_FineTime_FromDouble(_udp_Timeout_to1);
  ans->tr_toN = ilu_FineTime_FromDouble(_udp_Timeout_toN);
  ans->tr_tto = ilu_FineTime_FromDouble(_udp_Timeout_tto);
  ans->tr_wc = XXX;

  ILU_NOTE((CONNECTION_DEBUG | UDP_DEBUG),
	("udp(%p): Accept request of len %d from %s:%u on port %u, FD %d, mooring %p.\n",
	 ans, ilen, inet_ntoa(from.sin_addr), ntohs(from.sin_port),
	 ntohs(mParms->addr.sin_port), mParms->fd, m));
  if ((ilu_DebugLevel & PACKET_DEBUG) != 0)
    _ilu_debug_DumpPacket(ibuf, ilen, "incoming UDP");
  return (ans);
faild:
  ilu_free(ibuf);
  if (tinfo_out != NIL) {
    ilu_free(*tinfo_out);
    *tinfo_out = NIL;
  }
  if (tParms != NIL)
    ilu_free(tParms);
  if (ans != NIL)
    ilu_free(ans);
  return NIL;
}

static ilu_boolean 
CloseMooring(ilu_Mooring m, ilu_integer * dfd,
	     ILU_ERRS((internal)) * err)
{
  MooringParms    mp = (MooringParms) m->mo_data;
  int             res;
  ILU_NOTE((EXPORT_DEBUG | UDP_DEBUG),
	("udp(%p): Close mooring on FD %d, addr %s, port %u.\n",
	 m, mp->fd, inet_ntoa(mp->addr.sin_addr),
	 ntohs(mp->addr.sin_port)));
  ilu_UnregisterInputSource(mp->fd);
  res = OS_SOCKLOSE(mp->fd);
  *dfd = 1;
  ASSERT(res == 0, buf,
	 (buf, "udp_CloseMooring: res=%d, errno=%s", res,
	  strerror(sockerrno)));
  if (mp->tih != NIL)
    mp->tih = NIL;
  ilu_free(mp);
  ilu_free(m);
  return ILU_CLER(*err);
}

/*L1.sup < trmu; L2 unconstrained*/
static ilu_integer MooringDFd(ilu_Mooring self, ilu_boolean add)
{
  return (add ? 0 : 1);
}

/*L1, L2 unconstrained*/

static          ilu_boolean
SetReqHandler(ilu_Mooring m, ilu_Server s,
	      ilu_TIH tih,
	      ILU_ERRS((no_memory, imp_limit, no_resources,
			broken_locks, internal)) * err)
{
  MooringParms    mp = (MooringParms) m->mo_data;
  _ilu_Assert(mp->tih == NIL, "udp:SetReqHandler");
  if (!ilu_RegisterInputSource(mp->fd, _ilu_InvokeTIH, tih))
    return ILU_ERR_CONS1(no_resources, err, minor, ilu_nrm_mlreg, ilu_FALSE);
  mp->tih = tih;
  return ILU_CLER(*err);
}

/*Before: L1 = {s},
          forall conn: (L2 >= {conn.iomu}) => (L2 >= {conn.callmu});
  After: Main Invariant holds*/
static          ilu_boolean
WaitForReq(ilu_Mooring m, ilu_Server s,
	   ILU_ERRS((interrupted, broken_locks)) * err)
{
  MooringParms    mp = (MooringParms) m->mo_data;
  int             fd = mp->fd;
  ilu_boolean     sure;
  if (!ilu_ExitMutex(s->sr_lock, ilu_TRUE, err))
    return ilu_FALSE;
  _ilu_WaitForInputOnFD(fd, &sure, NIL, err);
  return ILU_ERROK(*err);
}

static void CloseCreator(ilu_TransportCreator tcr)
{
  CreatorParms    cp = (CreatorParms) tcr->tcr_data;
  if (cp->tinfo_host != NIL)
    ilu_free(cp->tinfo_host);
  ilu_free(cp);
  ilu_free(tcr);
  return;
}

static struct _ilu_Mooring_s mooringProto = {
  MooringDFd,
  SetReqHandler,
  WaitForReq,
  AcceptClient,
  CloseMooring,
  NIL				/* data */
};

/*L1_sup < trmu; L2 unconstrained*/
static          ilu_Mooring
CreateMooring(ilu_TransportCreator tcr, ilu_TransportInfo * tinfo_out,
	      ilu_boolean buffer, ilu_integer *dfd, ilu_Passport pp /* unused */,
	      ILU_ERRS((IoErrs)) * err)
{
  CreatorParms    cParms = (CreatorParms) tcr->tcr_data;
  INetSockAddr    mysin = cParms->addr;
  MooringParms    mParms = NIL;
  int             skt = -1;
  SOCKET_SIZE_TYPE namelen;
  ilu_Mooring     ans = NIL;
  ilu_boolean     loopedback = ilu_FALSE;
  *dfd = 0;
  skt = Socket(err);
  if (ILU_ERRNOK(*err))
    return NIL;
  *dfd = 1;
  while (1) {
    int             err1;
    if (!OS_SOCKERR(bind(skt, (struct sockaddr *) & mysin, sizeof(mysin))))
      break;
    err1 = sockerrno;
    ILU_NOTE((EXPORT_DEBUG | UDP_DEBUG),
	  ("udp(%p): bind to port %lx:%u failed:  %s.\n",
	   tcr, ntohl(mysin.sin_addr.s_addr),
	   ntohs(cParms->addr.sin_port), strerror(err1)));
    if (err1 == SOCKERRID(ADDRNOTAVAIL) && !loopedback) {
      /*
       * Set desired address to LOOPBACK, try again.  Linux won't
       * let us bind(2) to INADDR_LOOPBACK --- but it will let us
       * bind to INADDR_ANY.  So we bind to that, and report
       * "127.0.0.1" (the loopback address) in the tinfo_out.
       */
      mysin.sin_addr.s_addr = INADDR_ANY;
      loopedback = ilu_TRUE;
      goto tryagain;
    }
    OS_SOCKLOSE(skt);
    *dfd = 0;
    return ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_ts, NIL);
tryagain:
    0;
  }
  if (mysin.sin_port == 0) {
    namelen = sizeof(mysin);
    if (OS_SOCKERR(getsockname(skt, (struct sockaddr *) & mysin,
			       &namelen))) {
      ILU_NOTE((EXPORT_DEBUG | UDP_DEBUG),
	    ("udp: getsockname failed:  %s.\n",
	     strerror(sockerrno)));
      OS_SOCKLOSE(skt);
      *dfd = 0;
      return ILU_ERR_CONS1(internal, err, minor, ilu_im_errno, NIL);
    }
  }
  if (tinfo_out != NIL) {
    ilu_string      hostname, t = NIL;
    char           *tinfo;
    if (!loopedback)
      hostname = cParms->tinfo_host;
    else if (cParms->name_given)
      hostname = "localhost";
    else
      hostname = "127.0.0.1";

    tinfo = FormHandle(hostname, ntohs(mysin.sin_port), ilu_FALSE, err);
    if (ILU_ERRNOK(*err))
      goto faild;
    *tinfo_out = _ilu_ConcatTinfo(tinfo, (ilu_TransportInfo) & t, err);
    ilu_free(tinfo);
    if (ILU_ERRNOK(*err))
      goto faild;
  }
  mParms = (MooringParms) ilu_malloc(sizeof(*mParms));
  if (mParms == NIL) {
    (void) ILU_ERR_CONS1(no_memory, err, nbytes, sizeof(*mParms), 6);
    goto faild;
  }
  ans = (ilu_Mooring) ilu_malloc(sizeof(struct _ilu_Mooring_s));
  if (ans == NIL) {
    (void) ILU_ERR_CONS1(no_memory, err, nbytes, sizeof(*ans), 6);
    goto faild;
  }
  ILU_NOTE((EXPORT_DEBUG | UDP_DEBUG),
	("udp(%p): new mooring on FD %d, addr %s, port %u.\n",
	 ans, skt, inet_ntoa(mysin.sin_addr), ntohs(mysin.sin_port)));
  *ans = mooringProto;
  ans->mo_data = mParms;
  ans->mo_wc = XXX;
  mParms->addr = mysin;
  mParms->fd = skt;
  mParms->tih = NIL;
  return (ans);
faild:
  if (skt >= 0)
    OS_SOCKLOSE(skt);
  *dfd = 0;
  if (ans != NIL)
    ilu_free(ans);
  if (mParms != NIL)
    ilu_free(mParms);
  if (tinfo_out != NIL && *tinfo_out != NIL) {
    ilu_free(*tinfo_out);
    *tinfo_out = NIL;
  }
  return NIL;
}

static struct _ilu_TransportCreator_s creatorProto = {
  ilu_TRUE,				/* boundaried */
  ilu_FALSE,			/* reliable */
  0,				/* tcr_holds */
  ilu_FALSE,			/* tcr_wantClose */
  FdUsage,
  CreateTransport,
  CreateMooring,
  CloseCreator,
  NIL				/* data */
};

/*L1_sup < trmu*/
ilu_TransportCreator 
_ilu_udp_TransportCreator(ilu_TransportInfo tinfo,
			  ILU_ERRS((no_memory, inv_objref)) * err)
{
  CreatorParms    cp;
  ilu_TransportCreator tcr;
  ILU_AUTOSETDEBUGLEVEL;
  cp = InterpretInfo(tinfo[0], err);
  if (ILU_ERRNOK(*err))
    return NIL;
  tcr = (ilu_TransportCreator) ilu_malloc(sizeof(*tcr));
  if (tcr == NIL) {
    ilu_free(cp);
    return ILU_ERR_CONS1(no_memory, err, nbytes, sizeof(*tcr), NIL);
  }
  *tcr = creatorProto;
  tcr->tcr_data = cp;
  return (tcr);
}

#else

ilu_TransportCreator
_ilu_udp_TransportCreator(ilu_TransportInfo tinfo,
			  ILU_ERRS((no_memory, inv_objref)) * err)
{
  return 0;
}

#endif
