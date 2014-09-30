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
/* $Id: newtcp.c,v 1.102 1999/09/09 19:20:07 spreitze Exp $ */
/* Last edited by Mike Spreitzer September 8, 1998 6:57 pm PDT */

#include "iluntrnl.h"

#include "ilutransport.h"
#include "mooring.h"

#include "ilusock.h"

#include "oscalls.h"

/* L1, L2 unconstrained */

#ifdef ENABLE_DEBUGGING
#ifndef ENABLE_TCP_STATS
#define ENABLE_TCP_STATS
#endif /* ENABLE_TCP_STATS */
#endif /* ENABLE_DEBUGGING */

#ifdef XEROX_FIREWALL_HANDLING
#include "xerox_firewall.c"
#define connect(a,b,c)	xerox_firewall_connect((a),(b),(c))
#endif /* XEROX_FIREWALL_HANDLING */

#if (defined(WIN32) || defined(WIN16) || defined( macintosh ))

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

#elif (defined(_IS_BSD) && defined(FIONBIO))

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
  return 0;
}
#define set_listener_non_blocking	set_doer_non_blocking

#else	/* not POSIX or BSD or Windows */

#error "Don't know how to do non-blocking I/O for anything but POSIX, Windows, and BSD just now"

#endif

/* now define macro for setting the CLOSE-ON-EXEC flag on a fd */
#if (defined(_IS_POSIX) && !defined(WIN32))
#include <fcntl.h>
#include <unistd.h>
static int set_fd_flag (int fd, int flag, ilu_boolean set)
{
  int             flags = fcntl(fd, F_GETFD, 0);
  if (flags < 0)
    return flags;
  if (set)
    flags |= flag;
  else
    flags &= ~flag;    
  return fcntl(fd, F_SETFD, flags);
}
#define set_close_on_exec(fd)		set_fd_flag((fd), FD_CLOEXEC, ilu_TRUE)
#else
#ifdef WIN32
/* return 0 to ignore errors because we expect this to fail on Windows 95 */
#define set_close_on_exec(fd) (SetHandleInformation((HANDLE) fd, HANDLE_FLAG_INHERIT, 0), 0)
#else
#define set_close_on_exec(fd)		0
#endif /* Win32 */
#endif /* (defined(_IS_POSIX) && !defined(WIN32)) */

#ifdef ILU_NEEDS_NETINET_TCP_H_FOR_NODELAY
#include <netinet/tcp.h>
#endif

#if 0
#undef XEROX_FIREWALL_HANDLING
#undef set_listener_non_blocking
#undef set_doer_non_blocking
#define set_listener_non_blocking(fd) 0
#define set_doer_non_blocking(fd) 0
#endif

typedef unsigned short ilutcpport_t;
/* WIN port needs arg of htons() to be an unsigned short */

typedef struct {
  /* L1, L2 unconstrained */

  struct sockaddr_in addr;
  /*
   * Local address for Mooring creation, peer address for Transport
   * creation.
   */
  ilu_string      tinfo_host;
  /* host part of tinfo; never NIL; owned by this struct */

  ilu_boolean     name_given;	/* did tinfo give name or address? */
  ilu_boolean     defaulted;	/* did tinfo give null name/address? */
  ilu_cardinal    buffersize;	/* did tinfo give buffersize? */
}              *CreatorParms;

typedef struct {
  /* L1, L2 unconstrained */

  int		fd;		/* listening socket */
  ilu_cardinal	buffersize;	/* buffering wanted */
  ilu_TIH       tih;		/* the reg'd req handler, if any */
}              *MooringParms;
/* (tih) owned by client, not mooring. */

typedef struct {
  /* L1, L2 unconstrained */

  int             fd;		/* file descriptor for socket */
  ilu_cardinal    inSize;	/* size of input buffer */
  ilu_cardinal    outFirst;	/* first yet to be written */
  ilu_TIH         tih;		/* the reg'd inp handler, if any */
}              *TCPParms;
/*
 * What goes in the data field of a TCP ilu_Transport. (tih) owned
 * by client, not transport.  In the exposed output buffer, if any,
 * the significant bytes are those at indices [outFirst,
 * tr_outNext).
 */

#define BUFFERSIZE	4096
#define DIRECT_THRESHOLD 1024

static ilu_cardinal DefaultBuffersize = 0;
static ilu_cardinal KernelBuffersize = 0;

/*********************************************************************/
/*********************************************************************/
/*********   Some debugging variables   ******************************/
/*********************************************************************/
/*********************************************************************/

#ifdef ENABLE_TCP_STATS

static ilu_cardinal _ilutcp_BytesSent = 0;
static ilu_cardinal _ilutcp_BytesReceived = 0;
static ilu_cardinal _ilutcp_MooringsCreated = 0;
static ilu_cardinal _ilutcp_ConnectionsReceived = 0;
static ilu_cardinal _ilutcp_ConnectionsCreated = 0;
static ilu_cardinal _ilutcp_CurrentConnections = 0;
static ilu_cardinal _ilutcp_MaxSimultaneousConnections = 0;

void
  ilu_tcp_InitializeStats (void)
{
  _ilutcp_BytesSent = 0;
  _ilutcp_BytesReceived = 0;
  _ilutcp_MooringsCreated = 0;
  _ilutcp_ConnectionsReceived = 0;
  _ilutcp_ConnectionsCreated = 0;
  _ilutcp_MaxSimultaneousConnections = _ilutcp_CurrentConnections;  
}

void
  ilu_tcp_GetStats (ilu_cardinal *bytes_sent,
		    ilu_cardinal *bytes_received,
		    ilu_cardinal *moorings_created,
		    ilu_cardinal *connections_received,
		    ilu_cardinal *connections_created,
		    ilu_cardinal *current_connections,
		    ilu_cardinal *max_connections)
{
  *bytes_sent = _ilutcp_BytesSent;
  *bytes_received = _ilutcp_BytesReceived;
  *moorings_created = _ilutcp_MooringsCreated;
  *connections_received = _ilutcp_ConnectionsReceived;
  *connections_created = _ilutcp_ConnectionsCreated;
  *current_connections = _ilutcp_CurrentConnections;
  *max_connections = _ilutcp_MaxSimultaneousConnections;
}

#endif /* ENABLE_TCP_STATS */

/**********************************************************************
***********************************************************************
***********************************************************************
***** First, the methods for the TCP Transport ************************
***********************************************************************
***********************************************************************
**********************************************************************/

ilu_cardinal
  ilu_tcp_SetDefaultBuffersize (ilu_cardinal buffersize)
{
  ilu_cardinal oldbuffersize = DefaultBuffersize;
  DefaultBuffersize = buffersize;
  return oldbuffersize;
}

/*L1.sup < trmu; L2 unconstrained*/
static          CreatorParms
_tcp_InterpretInfo(ilu_string tinfo,
		   ILU_ERRS((no_memory, inv_objref)) * err)
{
  CreatorParms    cp;
  char            hostname[1000];
  long unsigned   port, buffersize;

  if (((sscanf(tinfo, "tcp_%999[^_]_%lu_%lu", hostname, &port, &buffersize)) != 3) &&
      (buffersize = ((DefaultBuffersize == 0) ? BUFFERSIZE : DefaultBuffersize),
       sscanf(tinfo, "tcp_%999[^_]_%lu", hostname, &port) != 2))
    return (ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_ts, NIL));
  cp = (CreatorParms) ilu_MallocE(sizeof(*cp), err);
  if (cp == NIL)
    return NIL;
  memset((ilu_string) & cp->addr, 0, sizeof(cp->addr));
  cp->addr.sin_family = AF_INET;
  cp->addr.sin_port = htons((ilu_shortcardinal) port);
  cp->addr.sin_addr.s_addr = inet_addr(hostname);
  cp->name_given = (cp->addr.sin_addr.s_addr == -1);
  cp->defaulted = (cp->addr.sin_addr.s_addr == 0 ||
		   strcmp(hostname, "localhost") == 0);
  cp->buffersize = buffersize;
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
      struct hostent *hp;
      if ((hp = gethostbyname(hostname)) != NIL
	  && hp->h_addr != NIL)
	memcpy((ilu_string) & cp->addr.sin_addr, hp->h_addr,
	       hp->h_length);
      if (cp->addr.sin_addr.s_addr == -1) {
	ILU_NOTE((CONNECTION_DEBUG | TCP_DEBUG),
	      ("ILU: _tcp_InterpretInfo:  Invalid host name (%s).\n",
	       hostname));
	ilu_free(cp);
	return ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_ts, NIL);
      }
    }
    cp->tinfo_host = ilu_StrdupE(hostname, err);
    if (ILU_ERRNOK(*err)) {
      free(cp);
      return NIL;
    }
  }
  ILU_CLER(*err);
  return cp;
faild:
  ilu_free(cp);
  return NIL;
}

/* L1, L2 unconstrained */

static          ilu_integer
_tcp_FdUsage(ilu_TransportCreator self, ilu_boolean mooring)
{
  return (1);
}

/* L1.sup < trmu; L2 >= {xmu, ymu} */
static          ilu_boolean
SetInputHandler(ilu_Transport self, ilu_TIH tih,
		ILU_ERRS((no_memory, internal, no_resources)) * err)
{
  TCPParms        p = (TCPParms) self->tr_data;
  if (p->tih)
    ilu_UnregisterInputSource(p->fd);
  p->tih = NIL;
  if (tih) {
    if (!ilu_RegisterInputSource(p->fd, _ilu_InvokeTIH, tih))
      return ILU_ERR_CONS1(no_resources, err, minor, ilu_nrm_mlreg,
			   ilu_FALSE);
    p->tih = tih;
  }
  return ILU_CLER(*err);
}

/* Main Invariant holds; L2 >= {ymu} */
static          ilu_boolean
_tcp_WaitForInput(ilu_Transport t, int *disabled, ilu_FineTime * limit,
		  ILU_ERRS((broken_locks, interrupted)) * err)
{
  TCPParms        p = (TCPParms) t->tr_data;
  ilu_boolean     sure;
  if ((t->tr_inBuff != NIL) && (t->tr_inNext < t->tr_inLimit))
    return ILU_ERR_CONS1(internal, err, minor, ilu_im_tcInputSkipsBuff,
			 ilu_FALSE);
  _ilu_WaitForInputOnFD(p->fd, &sure, limit, err);
  *disabled = (t->tr_wc && t->tr_wc->iluwc_waitsDisabled > 0);
  /*
   * This may be occasionally wrong, but that's OK as long as it's
   * not systematically wrong.
   */
  return ILU_ERROK(*err);
}

/*Main Invariant holds; L2 no forther constrained*/
static          ilu_boolean
_tcp_InterruptST(ilu_Transport self, ILU_ERRS((bad_param)) * err)
{
  TCPParms        p = (TCPParms) self->tr_data;
  return _ilu_InterruptFD(p->fd, err);
}

/* L1 >= {cmu}; L1.sup < trmu; L2 unconstrained */

static          ilu_boolean
_tcp_DisableWait(ilu_Transport self, ILU_ERRS((bad_param)) * err)
{
  ILU_NOTE(TCP_DEBUG, ("ILU: _tcp_DisableWait(%p)\n", self));
  return _ilu_DisableFDWaits(err);
}

static          ilu_boolean
_tcp_EnableWait(ilu_Transport self, ILU_ERRS((bad_param)) * err)
{
  ILU_NOTE(TCP_DEBUG, ("ILU: _tcp_EnableWait(%p)\n", self));
  return _ilu_EnableFDWaits(err);
}

/*L1, L2 unconstrained*/

static          ilu_ReadHeaderResultCode
_tcp_BeginMessage(ilu_Transport self,
		  ilu_boolean input_p,
		  ILU_ERRS((IoErrs)) * err)
{
  return ILU_ERR_CONS1(internal, err, minor, ilu_im_tcNotBoundaried,
		       ilu_rhrc_error);
}

static          ilu_boolean
_tcp_EndMessage(ilu_Transport self,
		ilu_boolean flush,
		ilu_Message *msgh,
		ILU_ERRS((IoErrs)) * err)
{
  return ILU_ERR_CONS1(internal, err, minor, ilu_im_tcNotBoundaried,
		       ilu_FALSE);
}

static          ilu_boolean
_tcp_BeginOutputMessageNonblock(ilu_Transport self,
		  ILU_ERRS((IoErrs)) * err)
{
  return ILU_ERR_CONS1(internal, err, minor, ilu_im_tcNotBoundaried,
		       ilu_rhrc_error);
}

static          ilu_TransportEndReport
_tcp_EndOutputMessageNonblock(ilu_Transport self,
			      ilu_boolean flush,
			      ilu_Message * msgh,
			      ILU_ERRS((IoErrs)) * err)
{
  ilu_TransportEndReport ans = {0};
  return ILU_ERR_CONS1(internal, err, minor, ilu_im_tcNotBoundaried,
		       ans);
}

static          ilu_boolean
_tcp_SendWholeMessage(ilu_Transport self, ilu_Message * msgh,
		      ilu_Error * err)
{
  return ILU_ERR_CONS1(internal, err, minor, ilu_im_tcReliable,
		       ilu_FALSE);
}

/*Main Invariant holds; L2 >= {xmu}*/

static          ilu_cardinal
NbWrite(ilu_Transport self, ilu_bytes buf, ilu_cardinal size,
      ILU_ERRS((IoErrs)) * err)
{
  TCPParms        p = (TCPParms) self->tr_data;
  ilu_cardinal    ans = 0;
  ILU_NOTE_AND((CONNECTION_DEBUG | TCP_DEBUG),
	       ("ILU:  (newtcp.c/NbWrite) self=%p: writing up to %lu bytes from %p to FD %d.\n",
		self, size, buf, p->fd));

#ifdef ENABLE_DEBUGGING

  if (((ilu_DebugLevel & PACKET_DEBUG) != 0) &&
      (size > 0))
    _ilu_debug_DumpPacket(buf, size, "outgoing TCP");

#endif				/* ENABLE_DEBUGGING */

  ans = _ilu_NbSockWrite(p->fd, buf, size, err);

#ifdef ENABLE_TCP_STATS
  _ilutcp_BytesSent += ans;
#endif				/* ENABLE_TCP_STATS */

  return ans;
}

static          ilu_boolean
Write(ilu_Transport self, ilu_bytes buf, ilu_cardinal size,
      ILU_ERRS((IoErrs)) * err)
{
  TCPParms        p = (TCPParms) self->tr_data;
  ILU_NOTE_AND((CONNECTION_DEBUG | TCP_DEBUG),
	("ILU:  (newtcp.c/Write) self=%p: writing %lu bytes from %p to FD %d.\n",
	 self, size, buf, p->fd));

#ifdef ENABLE_DEBUGGING

  if (((ilu_DebugLevel & PACKET_DEBUG) != 0) &&
      (size > 0))
    _ilu_debug_DumpPacket(buf, size, "outgoing TCP");

#endif /* ENABLE_DEBUGGING */

  if (!_ilu_SockWrite(p->fd, buf, size, err))
    return ilu_FALSE;

#ifdef ENABLE_TCP_STATS
  _ilutcp_BytesSent += size;
#endif /* ENABLE_TCP_STATS */

  return ilu_TRUE;
}

static          ilu_boolean
_tcp_Push(ilu_Transport self,
	  ILU_ERRS((IoErrs)) * err)
{
  TCPParms        p = (TCPParms) self->tr_data;
  if (self->tr_outNext > p->outFirst) {
    if (!Write(self, self->tr_outBuff + p->outFirst,
	       self->tr_outNext - p->outFirst, err))
      return ilu_FALSE;
    self->tr_outNext = 0;
  }
  return ILU_ERROK(*err);
}

static          ilu_cardinal
_tcp_WriteBytesNonblock(ilu_Transport self, ilu_bytes b,
			ilu_cardinal bufferSize,
			ilu_boolean flush,
			ilu_boolean *flushed,
			ILU_ERRS((IoErrs)) * err)
{
  TCPParms        p = (TCPParms) self->tr_data;
  ilu_cardinal    rem = self->tr_outLimit - self->tr_outNext;
  ilu_cardinal    trem = rem + p->outFirst;
  ilu_cardinal    hedlen = self->tr_outNext - p->outFirst;
  ilu_cardinal    refill = (!flush ? self->tr_outLimit - 16 : 0);
  ilu_cardinal    ans = 0;

  *flushed = ilu_TRUE;
  if (
      (self->tr_outBuff == NIL)
  /* no internal buffer to copy into, so can't copy */

      || ((bufferSize + ((bufferSize > rem + refill) ? hedlen : 0))
	  >= DIRECT_THRESHOLD)
  /* so big that copy(s) would take longer than write() syscall */

      || (bufferSize > trem + refill)
  /* going to need two writes anyway, so avoid copy */
    ) {
    if (self->tr_outNext > p->outFirst) {
      ilu_cardinal    dStart;
      dStart = NbWrite(self, self->tr_outBuff + p->outFirst,
		       self->tr_outNext - p->outFirst, err);
      p->outFirst += dStart;
      if (ILU_ERRNOK(*err) || p->outFirst < self->tr_outNext)
	return ans;
      self->tr_outNext = p->outFirst = 0;
    }
    ans = NbWrite(self, b, bufferSize, err);
    0;
  } else {
    ilu_cardinal    l1;
    if (bufferSize > rem + refill) {
      memmove((void *) self->tr_outBuff,
	      (void *) (self->tr_outBuff + p->outFirst),
	      hedlen);
      self->tr_outNext -= p->outFirst;
      p->outFirst = 0;
    }
    l1 = MIN(self->tr_outLimit - self->tr_outNext, bufferSize);
    memcpy((void *) (self->tr_outBuff + self->tr_outNext), (void *) b,
	   l1);
    b += l1;
    bufferSize -= l1;
    self->tr_outNext += l1;
    if (flush || ((self->tr_outNext + 16) > self->tr_outLimit)) {
      ilu_cardinal    wrote, req = self->tr_outNext - p->outFirst;
      wrote = NbWrite(self, self->tr_outBuff + p->outFirst, req, err);
      p->outFirst += wrote;
      if (*flushed = (wrote == req))
	p->outFirst = self->tr_outNext = 0;
      if (wrote > hedlen)
	ans = wrote - hedlen;
      if (ILU_ERRNOK(*err) || wrote < req)
	return ans;
    } else
      ILU_CLER(*err);
    if (bufferSize > 0) {
      memcpy((void *) self->tr_outBuff, (void *) b, bufferSize);
      self->tr_outNext += bufferSize;
      ans += bufferSize;
    }
  }
  _ilu_Assert(((self->tr_outBuff == ILU_NIL) ||
	       ((self->tr_outLimit - self->tr_outNext) >= 16)),
	      "_tcp_WriteBytes returns with fewer than 16 free bytes in buffer");
  return ILU_ERROK(*err), ans;
}

static          ilu_boolean
_tcp_WriteBytes(ilu_Transport self, ilu_bytes b,
		ilu_cardinal bufferSize,
		ilu_boolean flush,
		ILU_ERRS((IoErrs)) * err)
{
  ilu_cardinal    rem = self->tr_outLimit - self->tr_outNext;

  if ((bufferSize >= DIRECT_THRESHOLD)
      /* big enough that copy would take longer than write() system call */

      || (flush && (bufferSize > rem))
      || (!flush && (bufferSize > (rem + self->tr_outLimit - 16)))
      /* going to need two writes anyway, so avoid copy */

      || (self->tr_outBuff == NIL)
      /* no internal buffer to copy into, so can't copy */
      ) {
    if (self->tr_outNext > 0) {
      if (!Write(self, self->tr_outBuff, self->tr_outNext, err))
	return ilu_FALSE;
      self->tr_outNext = 0;
    }
    if (!Write(self, b, bufferSize, err))
      return ilu_FALSE;
    /* empty exposed buffer, so at least 16 bytes */
  } else {
    ilu_cardinal    l1 = MIN(rem, bufferSize);
    memcpy((void*)(self->tr_outBuff + self->tr_outNext), (void*)b, l1);
    b += l1;
    bufferSize -= l1;
    self->tr_outNext += l1;
    if (flush || ((self->tr_outNext + 16) > self->tr_outLimit)) {
      if (!Write(self, self->tr_outBuff, self->tr_outNext, err))
	return ilu_FALSE;
      self->tr_outNext = 0;
    }
    if (bufferSize > 0) {
      memcpy((void*)self->tr_outBuff, (void*)b, bufferSize);
      self->tr_outNext += bufferSize;
    }
  }
  _ilu_Assert(((self->tr_outBuff == ILU_NIL)||
	       ((self->tr_outLimit - self->tr_outNext) >= 16)),
	      "_tcp_WriteBytes returns with fewer than 16 free bytes in buffer");
  return ILU_CLER(*err);
}

/*Main Invariant holds; L2 >= {xmu, ymu}*/

static          ilu_cardinal
_tcp_ReadBytes(ilu_Transport self,
	       ilu_bytes buffer,
	       ilu_cardinal len,
	       ilu_TransportReport * rpt,
	       ILU_ERRS((IoErrs)) * err)
{
  TCPParms        p = (TCPParms) self->tr_data;
  ilu_cardinal    lread, ans;
  ilu_bytes       actbuff;
  ilu_boolean     use_internal;
  rpt->tr_eom = rpt->tr_eof = ilu_FALSE;
  if (self->tr_inBuff != NIL && self->tr_inNext != self->tr_inLimit)
    return ILU_ERR_CONS1(internal, err, minor, ilu_im_tcInputSkipsBuff,
			 0);
  use_internal = (!buffer) || (self->tr_inBuff && (len < p->inSize));
  if (use_internal) {
    actbuff = self->tr_inBuff;
    lread = _ilu_NbSockRead(p->fd, actbuff, p->inSize, rpt, err);
    self->tr_inLimit = lread;
    if (ILU_ERROK(*err) && buffer && lread) {
      ilu_cardinal    tomove = ans = MIN(lread, len);
      memcpy((void *) buffer, (void *) self->tr_inBuff, tomove);
      self->tr_inNext = tomove;
    } else {
      ans = lread;
      self->tr_inNext = 0;
    }
  } else {
    actbuff = buffer;
    ans = lread = _ilu_NbSockRead(p->fd, actbuff, len, rpt, err);
  }

#ifdef ENABLE_DEBUGGING
  if (ILU_ERRNOK(*err)) {
    if (ilu_DebugLevel & TCP_DEBUG)
      ilu_DebugPrintf("ILU: _tcp_ReadBytes(self=%p): ReadBytes got error %s from FD %d!\n",
		    self, ILU_ERR_NAME(*err), p->fd);
  } else {
    ILU_NOTE(TCP_DEBUG,
	     ("ILU: _tcp_ReadBytes(self=%p): read %u bytes from FD %d, eof=%s\n",
	      self, lread, p->fd, rpt->tr_eof ? "T" : "F"));
    if (((ilu_DebugLevel & PACKET_DEBUG) != 0) &&
	(lread > 0))
      _ilu_debug_DumpPacket(actbuff, lread, "incoming TCP");
   
  }
#endif				/* ENABLE_DEBUGGING */

#ifdef ENABLE_TCP_STATS
 if (ILU_ERROK(*err))
   _ilutcp_BytesReceived += lread;
#endif /* ENABLE_TCP_STATS */

  return ans;
}


#if (defined (WIN32) || defined (WIN16))

/* returns 1 if the fd is ready for writing, else 0 */
/*L1, L2 unconstrained*/
static int _tcp_writeable_fd(int fd) {

	fd_set write_set;
	struct timeval time = {ILU_WIN32_CONNECT_WRITE_TIMEOUT, 0};
	
	FD_ZERO(&write_set);
	FD_SET(fd, &write_set);
		
	return select(fd + 1, NULL, &write_set, NULL, &time);
}
#endif

/*L1, L2 unconstrained*/
static int GetLocalPort(int fd)
{
  SOCKET_SIZE_TYPE namelen;
  struct sockaddr_in sinaddr;
  namelen = sizeof(sinaddr);
  if (OS_SOCKERR(getsockname(fd, (struct sockaddr *) & sinaddr,
			     &namelen)))
    return 0;
  else
    return ntohs(sinaddr.sin_port);
}

/*Main Invariant holds*/
static ilu_boolean 
_tcp_Connect(ilu_Transport self,
	     CreatorParms cp,
	     ILU_ERRS((IoErrs)) * err)
{
  TCPParms        p = (TCPParms) self->tr_data;
  int             fd, err1;
  struct sockaddr_in sinaddr = cp->addr;
  ilu_boolean     status = ilu_TRUE;
  unsigned        port;
  unsigned	  on = 1;

  ILU_CLER(*err);
  _ilu_HandleSigPIPE();

  if (self == NIL)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_nil, ilu_FALSE);

  port = ntohs(sinaddr.sin_port);
  
  ILU_NOTE((CONNECTION_DEBUG | TCP_DEBUG),
	("ILU: _tcp_Connect(self=%p):  connecting to host %s, port %u...\n",
	 self, inet_ntoa(sinaddr.sin_addr), port));

  if (OS_SOCKINV(fd = socket(AF_INET, SOCK_STREAM, 0))) {
    err1 = sockerrno;
    ILU_NOTE((CONNECTION_DEBUG | TCP_DEBUG),
	  ("ILU: _tcp_Connect(self=%p):  socket call failed:  %s.\n",
	   self, strerror(err1)));
    switch (err1) {
    case SOCKERRID(ACCES):		/* permission error */
      ILU_ERR_CONS0(no_permission, err, 0);
      break;

#if !(defined(WIN32) || defined(WIN16)|| defined( macintosh ))
    case SOCKERRID(NFILE):
      ILU_ERR_CONS1(no_resources, err, minor, ilu_nrm_ENFILE, 0);
      break;
#endif

    case SOCKERRID(MFILE):		/* resource error */
    case SOCKERRID(NOBUFS):
      ILU_ERR_CONS1(no_resources, err, minor,
		    (err1 == SOCKERRID(MFILE))
		    ? ilu_nrm_EMFILE : ilu_nrm_ENOBUFS,
		    0);
      break;

    case SOCKERRID(PROTONOSUPPORT):	/* type error */
    case SOCKERRID(PROTOTYPE):
      ILU_ERR_CONS1(comm_failure, err, minor, ilu_cfm_socket_type, 0);
      break;

    default:
      ILU_ERR_CONS1(internal, err, minor, ilu_im_errno, 0);
    };
    return ilu_FALSE;
  } else {
#ifdef SO_REUSEADDR
    (void) setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, (char *) &on, 0);
#endif				/* SO_REUSEADDR */

#ifdef SO_USELOOPBACK
    (void) setsockopt(fd, SOL_SOCKET, SO_USELOOPBACK,
		      (ilu_string) NIL, 0);
#endif				/* SO_USELOOPBACK */

#if (defined(SO_SNDBUF) && defined(SO_RCVBUF))
    if (KernelBuffersize != 0) {
      if (OS_SOCKERR(setsockopt(fd, SOL_SOCKET, SO_SNDBUF,
				(ilu_string) KernelBuffersize, 0))) {
	err1 = sockerrno;
	ILU_NOTE(TCP_DEBUG,
		 ("ILU: _tcp_Connect: Can't set kernel output buffer size to %lu for outgoing TCP conn %d, error %d, <%s>\n",
		  KernelBuffersize, fd, err1, strerror(err1)));		 
      };
      if (OS_SOCKERR(setsockopt(fd, SOL_SOCKET, SO_RCVBUF,
				(ilu_string) KernelBuffersize, 0))) {
	err1 = sockerrno;
	ILU_NOTE(TCP_DEBUG,
		 ("ILU: _tcp_Connect: Can't set kernel input buffer size to %lu for outgoing TCP conn %d, error %d, <%s>\n",
		  KernelBuffersize, fd, err1, strerror(err1)));		 
      };
    };
#endif /* (defined(SO_SNDBUF) && defined(SO_RCVBUF)) */

    /*
     * The following was suggested by Ken Birman of the Cornell ISIS
     * project. On SunOS it prevents the kernel from delaying this
     * packet in hopes of merging it with a quickly-following one.
     */
#ifdef IPPROTO_TCP
#ifdef TCP_NODELAY
/*
 * This fails when IPPROTO_TCP or TCP_NODELAY is a constant instead
 * of a macro!
 */
    {
      int             one = 1;
      setsockopt(fd, IPPROTO_TCP, TCP_NODELAY,
		 (char *) &one, sizeof(one));
    }
#else
    /* do nothing */;
#endif				/* TCP_NODELAY */
#else
    /* do nothing */;
#endif				/* IPPROTO_TCP */


    if (cp->defaulted) {
      ILU_NOTE((CONNECTION_DEBUG | TCP_DEBUG),
	    ("ILU: _tcp_Connect(self=%p):  Can't connect to defaulted host.\n",
	     self));
      ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_connToDefault, 0);
      status = ilu_FALSE;
#ifndef XEROX_FIREWALL_HANDLING
      /* firewall code doesn't work with non-blocking connect */
    } else if (OS_SOCKERR(set_doer_non_blocking(fd))) {
      err1 = sockerrno;
      ILU_NOTE((CONNECTION_DEBUG | TCP_DEBUG),
	    ("ILU: _tcp_Connect(self=%p):  Failed to set socket (FD %d, host %s)"
	     " non-blocking, error \"%s\".\n",
	     self, fd, inet_ntoa(sinaddr.sin_addr),
	     strerror(err1)));
      ILU_ERR_CONS1(comm_failure, err, minor, ilu_cfm_nonblock, 0);
      status = ilu_FALSE;
#endif /* XEROX_FIREWALL_HANDLING */
    } else {
      int             connres;
      connres = connect(fd, (struct sockaddr *) & sinaddr,
			sizeof(sinaddr));
#ifdef __DGUX__
      /* From Dr. Dieter Maurer <dieter@sz-sb.de>:
	 In some versions of DGUX "connect" returns "-1" with
	 "errno == EAGAIN", even though a connection has been
	 successfully established.
	 If the "connect" is repeated, one of
	 two erros may result:
	 "EISCONN": in this case the connection has been
		established
	 "EALREADY": in this case, the connection is still
		being established.
	 We will wait a second and then try again (for at most 30 sec).
	 This is used to circumvent the DGUX bug (bad operating system!).
	 */
      if (connres == -1 && errno == EAGAIN) {
	int trials= 30;
	for (; trials; trials--) {
	  connres = connect(fd, (struct sockaddr *) & sinaddr,
			    sizeof(sinaddr));
	  if (!connres || (connres == -1 && errno == EISCONN)) {
	    connres= 0;
	    break;
	  }
	  if (errno == EALREADY) sleep(1); else break;
	}
      }
#endif /* __DGUX__ */

      if (!OS_SOCKERR(connres)) {
	ILU_NOTE((CONNECTION_DEBUG | TCP_DEBUG),
	      ("ILU: _tcp_Connect(self=%p):  connected to %s:%u from local:%d on FD %d\n",
	       self, inet_ntoa(sinaddr.sin_addr), port,
	       GetLocalPort(fd), fd));
	p->fd = fd;
	status = ilu_TRUE;
	if (OS_SOCKERR(set_close_on_exec(fd))) {
	  int             err2 = sockerrno;
	  ilu_DebugPrintf("ILU: _tcp_Connect(self=%p):  Unable to set outgoing connection "
			  "(FD %d) close-on-exec: %s (ignoring error)\n",
			  self, fd, strerror(err2));
	}
#ifdef XEROX_FIREWALL_HANDLING
	/* do non-blocking after the connect */
	if (OS_SOCKERR(set_doer_non_blocking(fd))) {
	  err1 = sockerrno;
	  ILU_NOTE((CONNECTION_DEBUG | TCP_DEBUG),
		("ILU: _tcp_Connect(self=%p):  Failed to set socket (FD %d, host %s)"
		 " non-blocking.  Error \"%s\".\n",
		 self, fd, inet_ntoa(sinaddr.sin_addr),
		 strerror(err1)));
	  close(fd);
	  p->fd = -1;
	  ILU_ERR_CONS1(comm_failure, err, minor, ilu_cfm_nonblock, 0);
	  status = ilu_FALSE;
	}
#endif /* XEROX_FIREWALL_HANDLING */
      }
#if (defined(WIN32) || defined(WIN16))
      else if (sockerrno == SOCKERRID(INPROGRESS) ||
               sockerrno == SOCKERRID(WOULDBLOCK))
#else
      else if (sockerrno == SOCKERRID(INPROGRESS))
#endif				/* WIN32 or WIN16 */
      {
	struct sockaddr peername;
	SOCKET_SIZE_TYPE pnlen = sizeof(peername);
	ilu_boolean     sure;
	_ilu_WaitForOutputOnFD(fd, &sure, NIL, err);
	if (
#if (defined (WIN32) || defined (WIN16))
	/*
	 * getpeername is not a good test for connectivity on Win32,
	 * so check to make sure the _ilu_WaitForOutputOnFD returned
	 * because of writability, not because of an exception.
	 */
	    _tcp_writeable_fd(fd) > 0
#else
	    getpeername(fd, &peername, &pnlen) == 0
#endif
	  ) {
	  ILU_NOTE((CONNECTION_DEBUG | TCP_DEBUG),
		("ILU: _tcp_Connect(self=%p):  eventually connected to %s:%u from local:%d on FD %d\n",
		 self, inet_ntoa(sinaddr.sin_addr), port,
		 GetLocalPort(fd), fd));
	  p->fd = fd;
	  status = ilu_TRUE;
	} else {
	  ILU_NOTE((CONNECTION_DEBUG | TCP_DEBUG),
		("ILU: _tcp_Connect(self=%p): connect FD %d to %s:%u failed "
		 "(no meaningful errno available? errno = %d).\n",
		 self, fd, inet_ntoa(sinaddr.sin_addr), port, sockerrno));
	  OS_SOCKLOSE(fd);
	  ILU_ERR_CONS1(comm_failure, err, minor,
			ilu_cfm_connect_failed, 0);
	  status = ilu_FALSE;
	}
      } else {
	int erri = sockerrno;
	ILU_NOTE((CONNECTION_DEBUG | TCP_DEBUG),
	      ("ILU: _tcp_Connect(self=%p): connect FD %d to %s:%u failed,"
	       " error %d (%s)\n",
	       self, fd, inet_ntoa(sinaddr.sin_addr),
	       port, erri, strerror(erri)));
	  OS_SOCKLOSE(fd);
	switch (erri)
	  {
	  case SOCKERRID(ADDRINUSE):
	  case SOCKERRID(ADDRNOTAVAIL):
	  case SOCKERRID(AFNOSUPPORT):
	  case SOCKERRID(FAULT):
	  case SOCKERRID(NETUNREACH):
	    ILU_ERR_CONS1(comm_failure, err, minor, ilu_cfm_bad_address,
			  0);
	    break;

	  case SOCKERRID(CONNREFUSED):
	    ILU_ERR_CONS1(comm_failure, err, minor,
			  ilu_cfm_connect_refused, 0);
	    break;

	  case SOCKERRID(TIMEDOUT):
	    ILU_ERR_CONS1(comm_failure, err, minor, ilu_cfm_timeout, 0);
	    break;

	  case SOCKERRID(ALREADY):
	  case SOCKERRID(INTR):
	  case SOCKERRID(ISCONN):
	  case SOCKERRID(BADF):
	  case SOCKERRID(NOTSOCK):
	  default:
	    ILU_ERR_CONS1(comm_failure, err, minor,
			  ilu_cfm_connect_failed, 0);
	    break;
	  }
	status = ilu_FALSE;
      }
    }
  }
  return (status);
}

/*L1, L2 unconstrained */

static ilu_boolean 
_tcp_Close(ilu_Transport self, ilu_integer * dfd,
	   ILU_ERRS((internal)) * err);

static struct _ilu_TransportClass_s tcpClass = {
  ilu_FALSE,			/* not boundaried */
  ilu_TRUE,			/* reliable */
  SetInputHandler,
  _tcp_WaitForInput,
  _tcp_InterruptST,
  _tcp_DisableWait,
  _tcp_EnableWait,
  _tcp_BeginMessage,
  _tcp_EndMessage,
  _tcp_BeginOutputMessageNonblock,
  _tcp_EndOutputMessageNonblock,
  _tcp_Push,
  _tcp_SendWholeMessage,
  _tcp_WriteBytes,
  _tcp_WriteBytesNonblock,
  _tcp_ReadBytes,
  _tcp_Close
};

/* L1.sup < cmu; L2 unconstrained */
static          ilu_Transport
NewT(ilu_cardinal buffersize, ILU_ERRS((IoErrs)) * err)
{
  ilu_Transport   ans = NIL;
  TCPParms        parms;
  ilu_WaitCohort  fdwc = NIL;
  ans = (ilu_Transport) ilu_MallocE(sizeof(*ans), err);
  if (ILU_ERRNOK(*err))
    return NIL;
  parms = (TCPParms) ilu_MallocE(sizeof(*parms), err);
  if (parms == NIL) {
    ilu_free(ans);
    return NIL;
  }
  fdwc = ilu_GetFDWaitCohort(err);
  /* XXX bad_locks can be raised too */
  if (ILU_ERRNOK(*err)) {
    ilu_free(ans);
    ilu_free(parms);
    return NIL;
  }
  parms->fd = -1;
  parms->inSize = buffersize;
  parms->outFirst = 0;
  parms->tih = NIL;
  ans->tr_class = &tcpClass;
  ans->tr_data = parms;
  ans->tr_wc = fdwc;
  ans->tr_estFDs = 1;
  if (buffersize > 0) {
    ans->tr_inBuff = (ilu_bytes) ilu_MallocE(parms->inSize, err);
    if (ans->tr_inBuff)
      ans->tr_outBuff = (ilu_bytes) ilu_MallocE(buffersize, err);
    if (ans->tr_inBuff == NIL || ans->tr_outBuff == NIL) {
      ilu_Error       lerr;
      ilu_integer     cdfd;
      (void) ilu_CloseTransport(ans, &cdfd, &lerr);
      ILU_HANDLED(lerr);
      return NIL;
    }
    ans->tr_inLimit = 0;
  } else {
    ans->tr_inBuff = ans->tr_outBuff = NIL;
    ans->tr_inLimit = buffersize;	/* have no mercy on bugs */
  }
  ans->tr_outLimit = buffersize;
  ans->tr_inNext = ans->tr_outNext = 0;
  ILU_CLER(*err);
  return ans;
}

/*Main Invariant holds*/
static ilu_Transport
  _tcp_CreateTransport(ilu_TransportCreator self, ilu_boolean buffer,
		       ilu_integer * dfd, ilu_Passport pp,
		       ILU_ERRS((IoErrs)) * err)
{
  CreatorParms    cp = (CreatorParms) self->tcr_data;
  ilu_Transport   ans;
  *dfd = 0;
  ans = NewT(buffer ? cp->buffersize : 0, err);
  if (ILU_ERRNOK(*err))
    return NIL;
  if (_tcp_Connect(ans, cp, err)) {
    *dfd = 1;
#ifdef ENABLE_TCP_STATS
    _ilutcp_ConnectionsCreated += 1;
    _ilutcp_CurrentConnections += 1;
    if (_ilutcp_CurrentConnections > _ilutcp_MaxSimultaneousConnections)
      _ilutcp_MaxSimultaneousConnections = _ilutcp_CurrentConnections;
#endif
  } else {
    ilu_Error       lerr;
    ilu_integer     cdfd;
    (void) ilu_CloseTransport(ans, &cdfd, &lerr);
    ILU_HANDLED(lerr);
    return NIL;
  }
  return ans;
}

/*L1.sup < trmu; L1 >= {cmu}; L2 >= {xmu, ymu}*/
static          ilu_boolean
_tcp_Close(ilu_Transport self, ilu_integer * dfd,
	   ILU_ERRS((internal)) * err)
{
  TCPParms        parms;
  int             fd;
  *dfd = 0;
  if (self == NIL || (parms = (TCPParms) self->tr_data) == NIL)
    return ILU_ERR_CONS1(internal, err, minor, ilu_im_broken, ilu_FALSE);
#ifdef ENABLE_TCP_STATS
  _ilutcp_CurrentConnections -= 1;
#endif
  fd = parms->fd;
  ILU_NOTE(TCP_DEBUG,
	   ("ILU: tcp_Close(%p, FD %d)\n", self, fd));
  if (fd >= 0) {
    parms->fd = -1;
    _ilu_FinishInputSource(fd);
    _ilu_FinishOutputSource(fd);
    OS_SOCKLOSE(fd);
    *dfd = 1;
  }
  if (_ilu_CanCondition()) {
    if (!ilu_DeltaWaitCohortRefCount(self->tr_wc, -1, err))
      return ilu_FALSE;
  } else if (!ilu_Check(!self->tr_wc, err))
    return ilu_FALSE;
  ilu_free(self->tr_inBuff);
  ilu_free(self->tr_outBuff);
  ilu_free(parms);
  ilu_free(self);
  return ilu_TRUE;
}

/**********************************************************************
***********************************************************************
***********************************************************************
**** Now the methods for the TCP Mooring ******************************
***********************************************************************
***********************************************************************
**********************************************************************/

/*L1, L2 unconstrained*/

static ilu_string
_tcp_FormInfo(ilu_string hostname, ilutcpport_t port,
	      ilu_boolean altfmt, ILU_ERRS((no_memory)) * err)
{
  char            buf[1000];
  sprintf(buf, altfmt ? "TCP %s %u" : "tcp_%s_%u", hostname,
	  ((unsigned) port) & 0xFFFF);
  return ilu_StrdupE(buf, err);
}

/*Main Invariant holds; L2 >= {xmu, ymu}*/
static          ilu_Transport
_tcp_AcceptClient(ilu_Mooring self, ilu_string *tinfo_out,
		  ilu_integer * dfd, ilu_Passport pp,
		  ILU_ERRS((IoErrs)) * err)
{
  MooringParms    mParms;
  TCPParms        tParms;
  int             ns;
  struct sockaddr_in sinaddr;
  ilu_Transport   newT = NIL;
  SOCKET_SIZE_TYPE addrlen;
  int             mfd;
  *dfd = 0;
  if (self == NIL)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, NIL);
  mParms = (MooringParms) self->mo_data;
  mfd = mParms->fd;
  ILU_NOTE((CONNECTION_DEBUG | TCP_DEBUG),
	("ILU: _tcp_AcceptClient(self=%p): accepting connection request on FD %d.\n",
	 self, mfd));
  addrlen = sizeof(sinaddr);
  if (OS_SOCKINV(ns = OS_ACCEPT(mfd, (struct sockaddr *) & sinaddr,
				&addrlen))) {
    int             theerr = sockerrno;
    ilu_no_resources_Minor nrm;
    switch (theerr) {
    case SOCKERRID(WOULDBLOCK):
      ILU_NOTE((CONNECTION_DEBUG | TCP_DEBUG),
	    ("ILU: _tcp_AcceptClient(self=%p): Spurious call on accept(FD %d).\n",
	     self, mfd));
      ILU_CLER(*err);
      return NIL;
    case SOCKERRID(MFILE):
      nrm = ilu_nrm_EMFILE;
      goto raise_no_resources;
    case SOCKERRID(NOBUFS):
      nrm = ilu_nrm_ENOBUFS;
  raise_no_resources:
      ILU_NOTE((CONNECTION_DEBUG | TCP_DEBUG),
	    ("ILU: _tcp_AcceptClient(self=%p): accept(FD %d) => errno %d, %s.\n",
	     self, mfd, theerr, strerror(theerr)));
      return ILU_ERR_CONS1(no_resources, err, minor, nrm, NIL);
#if (defined(WIN32) || defined(WIN16))
    case WSANOTINITIALISED:
      ILU_NOTE((CONNECTION_DEBUG | TCP_DEBUG),
	    ("ILU: _tcp_AcceptClient(self=%p): Can't accept(FD %d) because WinSock not initialized.\n",
	     self, mfd));
      return ILU_ERR_CONS1(internal, err, minor, ilu_im_errno, NIL);
#endif
    default:
      ILU_NOTE((CONNECTION_DEBUG | TCP_DEBUG),
	    ("ILU: _tcp_AcceptClient(self=%p): accept(FD %d) => errno %d.\n",
	     self, mfd, theerr));
      return ILU_ERR_CONS1(internal, err, minor, ilu_im_errno, NIL);
    }
  }
  *dfd = 1;
  if (OS_SOCKERR(set_doer_non_blocking(ns))) {
    int theerr = sockerrno;
    ILU_NOTE((CONNECTION_DEBUG | TCP_DEBUG),
	  ("ILU: _tcp_AcceptClient(self=%p):  Failed to set incoming connection"
	   " (FD %d) non-blocking.  Error \"%s\".\n",
	   self, ns, strerror(theerr)));
  }
  if (OS_SOCKERR(set_close_on_exec(ns))) {
    int             err2 = sockerrno;
    ilu_DebugPrintf("ILU: _tcp_AcceptClient(self=%p):  Unable to set incoming connection "
		    "(FD %d) close-on-exec: %s (ignoring error)\n",
		    self, ns, strerror(err2));
  }

#if (defined(SO_SNDBUF) && defined(SO_RCVBUF))
    if (KernelBuffersize != 0) {
      int err1;
      if (OS_SOCKERR(setsockopt(ns, SOL_SOCKET, SO_SNDBUF,
				(ilu_string) KernelBuffersize, 0))) {
	err1 = sockerrno;
	ILU_NOTE(TCP_DEBUG,
		 ("ILU: _tcp_AcceptClient(self=%p): Can't set kernel output buffer"
		  " size to %lu for incoming TCP conn %d, error %d, <%s>\n",
		  KernelBuffersize, ns, err1, strerror(err1)));		 
      };
      if (OS_SOCKERR(setsockopt(ns, SOL_SOCKET, SO_RCVBUF,
				(ilu_string) KernelBuffersize, 0))) {
	err1 = sockerrno;
	ILU_NOTE(TCP_DEBUG,
		 ("ILU: _tcp_AcceptClient(self=%p): Can't set kernel input buffer"
		  " size to %lu for incoming TCP conn %d, error %d, <%s>\n",
		  KernelBuffersize, ns, err1, strerror(err1)));		 
      };
    };
#endif /* (defined(SO_SNDBUF) && defined(SO_RCVBUF)) */

  /* See above comment on similar code below */
#ifdef IPPROTO_TCP
#ifdef TCP_NODELAY
  {
    int             one = 1;
    setsockopt(ns, IPPROTO_TCP, TCP_NODELAY,
	       (char *) &one, sizeof(one));
  }
#endif				/* TCP_NODELAY */
#endif				/* IPPROTO_TCP */

  if (tinfo_out != NIL) {
    if (ilu_EnterMutex(ilu_trmu, err)) {
      *tinfo_out = _tcp_FormInfo(inet_ntoa(sinaddr.sin_addr),
				 ntohs(sinaddr.sin_port), ilu_TRUE, err);
      (void) ilu_ExitMutex(ilu_trmu, ilu_TRUE, err);
    }
    if (ILU_ERRNOK(*err)) {
      OS_SOCKLOSE(ns);
      *dfd = 0;
      return NIL;
    }
  }
  newT = NewT(mParms->buffersize, err);
  if (newT == NIL)
    return NIL;
#ifdef ENABLE_DEBUGGING
  ILU_NOTE((CONNECTION_DEBUG | TCP_DEBUG),
	("ILU: _tcp_AcceptClient(self=%p): new transport %p, on FD %d,"
	 " from IP host %s, port %u.\n",
	 self, newT, ns, inet_ntoa(sinaddr.sin_addr),
	 ntohs(sinaddr.sin_port)));
#endif
#ifdef ENABLE_TCP_STATS
  _ilutcp_ConnectionsReceived += 1;
  _ilutcp_CurrentConnections += 1;
  if (_ilutcp_CurrentConnections > _ilutcp_MaxSimultaneousConnections)
    _ilutcp_MaxSimultaneousConnections = _ilutcp_CurrentConnections;
#endif
  tParms = (TCPParms) newT->tr_data;
  tParms->fd = ns;
  return (newT);
}

/*L1.sup < trmu; L1 >= {cmu}; L2 >= {xmu, ymu}*/
static          ilu_boolean
_tcp_CloseMooring(ilu_Mooring m, ilu_integer * dfd,
		  ILU_ERRS((bad_param, internal)) * err)
{
  MooringParms    mp = (MooringParms) m->mo_data;
  int             fd = mp->fd, res, err1;
  ILU_NOTE((EXPORT_DEBUG | TCP_DEBUG),
	   ("ILU: _tcp_CloseMooring(mooring=%p)\n", m));
  if (fd >= 0) {
    _ilu_FinishInputSource(fd);
    *dfd = 1;
    while (((res = OS_SOCKLOSE(fd)) == -1)
	   && ((err1 = sockerrno) == SOCKERRID(INTR)))
      /* no effect */;
    if (res < 0)
      return ILU_ERR_CONS1(internal, err, minor, ilu_im_errno, ilu_FALSE);
  } else
    *dfd = 0;
  if (mp != NIL) {
    ilu_free(mp);
    m->mo_data = NIL;
  }
  if (_ilu_CanCondition()) {
    if (!ilu_DeltaWaitCohortRefCount(m->mo_wc, -1, err))
      return ilu_FALSE;
  } else if (!ilu_Check(!m->mo_wc, err))
    return ilu_FALSE;
  ilu_free(m);
  return ilu_TRUE;
}

/*L1, L2 unconstrained*/
static          ilu_boolean
SetReqHandler(ilu_Mooring self,
	      ilu_TIH tih,
	      ILU_ERRS((no_memory, imp_limit, no_resources,
			broken_locks, internal)) * err)
{
  MooringParms    mp = (MooringParms) self->mo_data;
  _ilu_Assert(mp->tih == NIL, "SetReqHandler");
  if (!ilu_RegisterInputSource(mp->fd, _ilu_InvokeTIH, tih))
    return ILU_ERR_CONS1(no_resources, err, minor, ilu_nrm_mlreg, ilu_FALSE);
  mp->tih = tih;
  return ILU_CLER(*err);
}

/*Main Invariant holds; L2 >= {ymu}*/
static          ilu_boolean
WaitForReq(ilu_Mooring self, int *disabled,
	   ILU_ERRS((interrupted, broken_locks)) * err)
{
  MooringParms    mp = (MooringParms) self->mo_data;
  int             fd = mp->fd;
  ilu_boolean     sure;
  _ilu_WaitForInputOnFD(fd, &sure, NIL, err);
  *disabled = (self->mo_wc && self->mo_wc->iluwc_waitsDisabled > 0);
  /*
   * This may be occasionally wrong, but that's OK as long as it's
   * not systematically wrong.
   */
  return ILU_ERROK(*err);
}

/* L1 >= {cmu}; L1.sup < trmu; L2 >= {xmu} */

static          ilu_boolean
DisableReqWait(ilu_Mooring self,
	       ILU_ERRS((broken_locks,
			 bad_param,
			 internal)) * err)
{
  ILU_NOTE(TCP_DEBUG, ("ILU: TCP DisableReqWait(%p)\n", self));
  return _ilu_DisableFDWaits(err);
}

static          ilu_boolean
EnableReqWait(ilu_Mooring self,
	       ILU_ERRS((broken_locks,
			 bad_param,
			 internal)) * err)
{
  ILU_NOTE(TCP_DEBUG, ("ILU: TCP EnableReqWait(%p)\n", self));
  return _ilu_EnableFDWaits(err);
}

/*L1, L2 unconstrained*/

static ilu_integer MooringDFd(ilu_Mooring self, ilu_boolean add)
{
  return 1;
}

static struct _ilu_Mooring_s mooringProto = {
  MooringDFd,
  SetReqHandler,
  WaitForReq,
  DisableReqWait,
  EnableReqWait,
  _tcp_AcceptClient,
  _tcp_CloseMooring,
  NIL				/* data */
};

/*L1 >= {cmu}, L1.sup < trmu; L2 unconstrained*/
static ilu_Mooring 
_tcp_CreateMooring(ilu_TransportCreator self,
		   ilu_TransportInfo * tinfo_out,
		   ilu_boolean buffer,
		   ilu_integer *dfd,
		   ilu_Passport pp,	/* unused here */
		   ILU_ERRS((IoErrs)) * err)
{
  CreatorParms    cp = (CreatorParms) self->tcr_data;
  MooringParms    mp = NIL;
  struct sockaddr_in sinaddr = cp->addr;
  struct linger   linger = {0};
  SOCKET_SIZE_TYPE namelen;
  int             skt = -1;
  int             on = 1;
  ilu_Mooring     ans = NIL;
  int             err1;
  ilu_boolean     loopedback = ilu_FALSE;
  ilu_WaitCohort  fdwc;

  _ilu_HandleSigPIPE();
  *dfd = 0;
  if (tinfo_out != NIL)
    *tinfo_out = NIL;

  fdwc = _ilu_GetFDWaitCohort(err);
  if (ILU_ERRNOK(*err))
    return NIL;
  
  /* setup the new server */

  if (OS_SOCKINV(skt = socket(AF_INET, SOCK_STREAM, 0))) {
    err1 = sockerrno;
    ILU_NOTE((EXPORT_DEBUG | TCP_DEBUG),
	  ("ILU: _tcp_CreateMooring(self=%p): create Mooring socket failed:  %s.\n",
	   self, strerror(err1)));
    switch (err1) {
#if !(defined(WIN32) || defined(WIN16)|| defined( macintosh ))
    case SOCKERRID(ACCES):		/* permission error */
      ILU_ERR_CONS0(no_permission, err, 0);
      break;

    case SOCKERRID(NFILE):
      ILU_ERR_CONS1(no_resources, err, minor, ilu_nrm_ENFILE, 0);
      break;
#endif

    case SOCKERRID(MFILE):		/* resource error */
    case SOCKERRID(NOBUFS):
      ILU_ERR_CONS1(no_resources, err, minor,
		    (err1 == EMFILE) ? ilu_nrm_EMFILE : ilu_nrm_ENOBUFS,
		    0);
      break;

    case SOCKERRID(PROTONOSUPPORT):	/* type error */
    case SOCKERRID(PROTOTYPE):
      ILU_ERR_CONS1(comm_failure, err, minor, ilu_cfm_socket_type, 0);
      break;

    default:
      ILU_ERR_CONS1(internal, err, minor, ilu_im_errno, 0);
    };
    return (NIL);
  }
  *dfd = 1;
  setsockopt(skt, SOL_SOCKET, SO_REUSEADDR, (char *) &on, sizeof(on));
  linger.l_onoff = 0;
  setsockopt(skt, SOL_SOCKET, SO_LINGER,
	     (char *) &linger, sizeof(struct linger));

  while (1) {
    int             err2;
    if (!OS_SOCKERR(bind(skt, (struct sockaddr *) & sinaddr,
			 sizeof(sinaddr))))
      break;
    err2 = sockerrno;
    ILU_NOTE((EXPORT_DEBUG | TCP_DEBUG),
	  ("ILU: _tcp_CreateMooring(self=%p): bind to %08lx:%u failed:  %s.\n",
	   self, ntohl(sinaddr.sin_addr.s_addr), ntohs(sinaddr.sin_port),
	   strerror(err2)));
    switch (err2) {
    case SOCKERRID(ADDRNOTAVAIL):
      /*
       * We should get this error when name-to-addr(hostname) isn't
       * a currently valid address of this machine; use the loopback
       * address instead.  Someday we'll clone the implementation of
       * `ifconfig` and go looking for a currently valid IP address.
       */
      if (!loopedback) {
	/*
	 * Set desired address to LOOPBACK, try again.  Linux won't
	 * let us bind(2) to INADDR_LOOPBACK --- but it will let us
	 * bind to INADDR_ANY.  So we bind to that, and report
	 * "127.0.0.1" (the loopback address) in the tinfo_out.
	 */
	sinaddr.sin_addr.s_addr = INADDR_ANY;
	loopedback = ilu_TRUE;
	goto tryagain;
      }
      /* else fall through */

    case SOCKERRID(ADDRINUSE):
    case SOCKERRID(FAULT):
    case SOCKERRID(INVAL):
      ILU_ERR_CONS1(comm_failure, err, minor, ilu_cfm_bad_address, 0);
      break;

    case SOCKERRID(ACCES):	/* permission error */
      ILU_ERR_CONS0(no_permission, err, 0);
      break;

    default:
      ILU_ERR_CONS1(internal, err, minor, ilu_im_errno, 0);
      break;
    };
    OS_SOCKLOSE(skt);
    *dfd = 0;
    goto failc;
tryagain:
    /* end of loop */;
  }
  
  /* If tinfo gave port 0 then discover kernel allocated port */
  if (sinaddr.sin_port == 0) {
    namelen = sizeof(sinaddr);
    if (OS_SOCKERR(getsockname(skt, (struct sockaddr *) & sinaddr,
			       &namelen))) {
      int             err2 = sockerrno;
      ILU_NOTE((EXPORT_DEBUG | TCP_DEBUG),
	 ("ILU: _tcp_CreateMooring(self=%p): getsockname failed:  %s.\n",
	  self, strerror(err2)));
      OS_SOCKLOSE(skt);
      *dfd = 0;
      switch (err2) {
      case SOCKERRID(FAULT):
	ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_badPointer, 0);
	break;

      case SOCKERRID(BADF):
      case SOCKERRID(NOTSOCK):
	ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_fd, 0);
	break;

      case SOCKERRID(NOBUFS):
	ILU_ERR_CONS1(no_resources, err, minor, ilu_nrm_ENOBUFS, 0);
	break;

      default:
	ILU_ERR_CONS1(internal, err, minor, ilu_im_errno, 0);
	break;
      };
      goto failc;
    }
  }
  if (OS_SOCKERR(listen(skt, 4))) {
    int             err2 = sockerrno;
    ILU_NOTE((EXPORT_DEBUG | TCP_DEBUG),
	  ("ILU: _tcp_CreateMooring(self=%p):  listen on port %u, FD %d failed:  %s.\n",
	   self, ntohs(sinaddr.sin_port), skt, strerror(err2)));
    OS_SOCKLOSE(skt);
    *dfd = 0;
    switch (err2) {
    case SOCKERRID(BADF):
    case SOCKERRID(NOTSOCK):
    case SOCKERRID(OPNOTSUPP):
      ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_fd, 0);
      break;

    default:
      ILU_ERR_CONS1(internal, err, minor, ilu_im_errno, 0);
      break;
    };
    goto failc;
  }
  if (OS_SOCKERR(set_close_on_exec(skt))) {
    int             err2 = sockerrno;
    ilu_DebugPrintf("ILU: _tcp_CreateMooring(self=%p):  Unable to set listening socket "
		    "(FD %d) close-on-exec: %s (ignoring error)\n",
		    self, skt, strerror(err2));
  }
  if (OS_SOCKERR(set_listener_non_blocking(skt))) {
    int             err2 = sockerrno;
    ILU_NOTE((EXPORT_DEBUG | TCP_DEBUG),
	  ("ILU: _tcp_CreateMooring(self=%p):  Unable to set listening socket (FD %d) non-blocking: %s\n",
	   self, skt, strerror(err2)));
    OS_SOCKLOSE(skt);
    *dfd = 0;
    ILU_ERR_CONS1(internal, err, minor, ilu_im_errno, 0);
    goto failc;
  }
  ans = (ilu_Mooring) ilu_MallocE(sizeof(*ans), err);
  if (ILU_ERRNOK(*err))
    goto faild;
  mp = (MooringParms) ilu_MallocE(sizeof(*mp), err);
  if (ILU_ERRNOK(*err))
    goto faild;
  *ans = mooringProto;
  ans->mo_data = mp;
  mp->tih = NIL;
  mp->fd = skt;
  mp->buffersize = (buffer ? cp->buffersize : 0);
  ILU_NOTE(TCP_DEBUG,
	("ILU: _tcp_CreateMooring(self=%p): created Mooring %p on FD %d, port %u.\n",
	 self, ans, skt, ntohs(sinaddr.sin_port)));
  if (tinfo_out != NIL) {
    ilu_string      hostname, tinfo;
    if (!loopedback)
      hostname = cp->tinfo_host;
    else if (cp->name_given)
      hostname = "localhost";
    else
      hostname = "127.0.0.1";
    tinfo = _tcp_FormInfo(hostname, ntohs(sinaddr.sin_port), ilu_FALSE, err);
    if (ILU_ERRNOK(*err))
      goto faild;
    * tinfo_out = ilu_MallocE((2 * SIZEOF_VOID_P) + strlen(tinfo) + 1,
			      err);
    if (ILU_ERRNOK(*err))
      goto faild;
    (*tinfo_out)[0] = ((char *) (*tinfo_out)) + (2 * SIZEOF_VOID_P);
    (*tinfo_out)[1] = NIL;
    strcpy((*tinfo_out)[0], tinfo);
    ilu_free(tinfo);
  }
#ifdef ENABLE_TCP_STATS
  _ilutcp_MooringsCreated += 1;
#endif
  ans->mo_wc = fdwc;
  return (ans);
faild:
  if (skt >= 0)
    OS_SOCKLOSE(skt);
  *dfd = 0;
failc:
  if (ans != NIL)
    ilu_free(ans);
  if (mp != NIL)
    ilu_free(mp);
  if (tinfo_out != NIL && *tinfo_out != NIL) {
    ilu_free(*tinfo_out);
    *tinfo_out = NIL;
  }
  return NIL;
}

static void 
_tcp_CloseCreator(ilu_TransportCreator self)
{
  CreatorParms    cp = (CreatorParms) self->tcr_data;
  ilu_free(cp->tinfo_host);
  ilu_free(cp);
  ilu_free(self);
  return;
}

static struct _ilu_TransportCreator_s creatorProto = {
  ilu_FALSE,			/* !boundaried */
  ilu_TRUE,				/* reliable */
  0,				/* tcr_holds */
  ilu_FALSE,			/* tcr_wantClose */
  _tcp_FdUsage,
  _tcp_CreateTransport,
  _tcp_CreateMooring,
  _tcp_CloseCreator,
  NIL				/* data */
};

/*L1_sup < trmu*/
ilu_TransportCreator
_ilu_tcp_TransportCreator(ilu_TransportInfo tinfo,
			  ILU_ERRS((no_memory, inv_objref)) * err)
{
  ilu_TransportCreator ans;
  CreatorParms    cp;
  char *buffersizestring;
  ilu_cardinal buffersize;
  ILU_AUTOSETDEBUGLEVEL;
#if (defined(SO_SNDBUF) && defined(SO_RCVBUF))
  {
    static ilu_boolean initialized = ilu_FALSE;
    if (!initialized) {
      if ((buffersizestring = getenv("ILU_TCP_KERNEL_BUFFERSIZE")) != NIL) {
	if (((buffersize = _ilu_atoi(buffersizestring, NIL)) != 0) &&
	    (buffersize <= 65536)) {
	  ILU_NOTE(TCP_DEBUG,
		   ("_ilu_tcp_TransportCreator: will set TCP kernel buffers"
		    " to %lu bytes\n", buffersize));
	  DefaultBuffersize = buffersize;
	} else {
	  ilu_DebugPrintf("_ilu_tcp_TransportCreator: Bad value \"%s\" for "
			  "environment variable ILU_TCP_KERNEL_BUFFERSIZE\n",
			  buffersizestring);
	}
      }
      initialized = ilu_TRUE;
    }
  }
#endif /* (defined(SO_SNDBUF) && defined(SO_RCVBUF)) */
  cp = _tcp_InterpretInfo(tinfo[0], err);
  if (ILU_ERRNOK(*err))
    return NIL;
  ans = (ilu_TransportCreator) ilu_malloc(sizeof(*ans));
  *ans = creatorProto;
  ans->tcr_data = cp;
  return (ans);
}
