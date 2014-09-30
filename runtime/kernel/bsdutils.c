/** $Id: bsdutils.c,v 1.61 1999/09/03 02:12:48 spreitze Exp $
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
/* Last edited by Mike Spreitzer November 2, 1998 10:44 pm PST */
/* Chris Jacobi, May 2, 1997 3:54 pm PDT */

#include <signal.h>	/* for SIGPIPE, SIG_IGN, SIG_DFL */
#include <limits.h>	/* for INT_MAX */

#include "iluntrnl.h"
#include "oscalls.h"
#include "ilusock.h"

/* oscalls.h will have included unistd.h, which may or may not define
 * a prototype for gethostname().  If not, define it here.
 */
#ifndef HAVE_GETHOSTNAME_PROTOTYPE
extern int gethostname(char *name, int namelen);
#endif /* HAVE_GETHOSTNAME_PROTOTYPE */

#ifdef macintosh
#define SEND_RECV_BUF_TYPE(b)	((char *)b)
#else
#define SEND_RECV_BUF_TYPE(b)	((char *)b)
#endif


/* ================ BSD/WIN Hostname ================ */

/*L1, L2, Main unconstrained*/

#if (defined(_IS_BSD) || defined(WIN32) || defined(WIN16) || defined(macintosh))

ilu_string _ilu_Hostname(void)
{
  static ilu_string name = NIL;

  if (name == NIL) {
    char            hostname[1000];
#ifdef macintosh
	if ( gethostname(hostname, sizeof(hostname)) == 0 )
	{
	}
	else
	{
		strcpy( hostname, ILU_MAC_DEFAULT_HOST );
	}

#else
    _ilu_Assert(gethostname(hostname, sizeof(hostname)) == 0,
		"gethostname failed");
#endif
    name = _ilu_Strdup(hostname);
  }
  return (name);
}

#elif defined(_IS_POSIX)

#include <sys/utsname.h>	/* for struct utsname */

ilu_string _ilu_Hostname(void)
{
  ilu_string      name = NIL;
  struct utsname  names;
  if (OS_UNAME(&names) >= 0)
    name = _ilu_Strdup(names.nodename);
  return (name);
}

#endif

/*L1 >= {trmu}; L2 unconstrained*/
ilu_string
_ilu_CurrentHostIPAddrString(ilu_string * host_out,
			     struct in_addr * addr_out,
			     ILU_ERRS((IoErrs)) * err)
{
  static ilu_string tempname, inetname = NIL;
  static struct in_addr myaddr;
  static char    *myname;
  struct hostent *he;
  struct in_addr *hea;
  if (inetname == NIL) {
    if ((myname = _ilu_Hostname()) == NIL) {
#if (defined(WIN32) || defined(WIN16) || defined(macintosh))
      ILU_ERRPRINTF("no hostname for this machine! WSAGetLastError() = %i\n",
		    WSAGetLastError());
#else
      perror("no hostname for this machine!");
#endif
      return ILU_ERR_CONS1(internal, err, minor, ilu_im_noHostName, (ilu_string) NIL);
    }
    he = gethostbyname(myname);
    if (he == NIL || he->h_addr_list == NIL || he->h_addr_list[0] == NIL)
      return ILU_ERR_CONS1(internal, err, minor, ilu_im_noHostIpAddr,
			   (ilu_string) NIL);
    hea = (struct in_addr *) (he->h_addr_list[0]);
    tempname = (ilu_string) inet_ntoa(myaddr = *hea);
    inetname = ilu_StrdupE(tempname, err);
    if (ILU_ERRNOK(*err))
      return NIL;
  }
  ILU_CLER(*err);
  if (addr_out != NIL)
    *addr_out = myaddr;
  if (host_out != NIL)
    *host_out = myname;
  return (inetname);
}

/*L1 >= {trmu}; L2 unconstrained*/
ilu_string
_ilu_CurrentHostIPAddrStringOnly(ILU_ERRS((IoErrs)) * err)
{
  return _ilu_CurrentHostIPAddrString(NIL, NIL, err);
}

/* ================ BSD/WIN ilu_FineTime ================ */

/*L1, L2, Main unconstrained*/

#if (defined(_IS_BSD) || defined(WIN32) || defined(macintosh))

/*
 * "ilu_FineTime_Now" and "ilu_CoarseTime_Now" should be implemented
 * in the same module, because {ilu_CoarseTime_Now(), 0} needs to be
 * the corresponding ilu_FineTime, and both could use a different
 * origin than 1970.0.
 */

#include <time.h>	/* ANSI C req.s this for time() */

#ifdef _IS_BSD

#include <sys/types.h>	/* for time() */
#include <sys/time.h>	/* for time(), gettimeofday */

const ilu_cardinal ilu_FineTimeRate = 1000000;

ilu_FineTime ilu_FineTime_Now()
{
  struct timeval  tv;
  ilu_FineTime    ans;
  if (gettimeofday(&tv, NIL) != 0) {
    int             theerr = errno;
    ilu_DebugLevel &= ~TIMING_DEBUG;
    ASSERT(0, buf,
	   (buf, "UNIX time.c:gettimeofday failed, errno=%d",
	    theerr));
  }
  ans.ft_s = tv.tv_sec;
  ans.ft_t = tv.tv_usec;
  return ans;
}

#elif defined(WIN32)	/* must be WIN */

#include <sys/types.h>
#include <sys/timeb.h>
#include <process.h>

const ilu_cardinal    ilu_FineTimeRate = 1000;	/* milliseconds */
/* xxx note dll - don't think there is anyway to get usec on a PC! */

ilu_FineTime ilu_FineTime_Now()
{
  struct _timeb   tv;
  ilu_FineTime    ans;
  _ftime(&tv);
  ans.ft_s = tv.time;
  ans.ft_t = tv.millitm;
  return ans;
}

#elif defined( macintosh )

#include <unistd.h>	/* for time() */
#include <types.h>	/* for time() */
#include <time.h>	/* for time(), gettimeofday */

const ilu_cardinal    ilu_FineTimeRate = 60;

ilu_FineTime ilu_FineTime_Now()
{
  ilu_FineTime    ans;
  ans.ft_s = clock() / CLOCKS_PER_SEC;
  ans.ft_t =  clock() % CLOCKS_PER_SEC;
  return ans;
}

#else

#error "Don't know how to get FineTime for this OS!"

#endif /* _IS_BSD */

ilu_integer ilu_CoarseTime_Now()
{
  return time(NIL);
}

/* ================= BSD/WIN InventID ======================== */

/*L1, L2, Main unconstrained*/

/* monotonically increasing counter used in generating unique id's */
static unsigned long l_id_counter = 0;

/*L1_sup < trmu*/
ilu_string ilu_InventID ()
{
  char			buf[1000];
  ilu_FineTime		now;
  static ilu_string	iphostname = NIL;
  ilu_Error		lerr;

  _ilu_AcquireMutex(ilu_trmu);
  if (iphostname == NIL)
    {
      /* use HostIPAddr instead of Hostname to get domain info */
      iphostname = _ilu_CurrentHostIPAddrString(NIL, NIL, &lerr);
      if (ILU_ERRNOK(lerr))
	{
	  iphostname = "unknownhost";
	  ILU_HANDLED(lerr);
	}
    }
  now = ilu_FineTime_Now();
  sprintf(buf, "%s.%lx.%lx.%lx", iphostname,
	  (long unsigned) OS_GETPID(),
	  (long unsigned) now.ft_s,
	  (long unsigned) l_id_counter);

  l_id_counter++;

  _ilu_ReleaseMutex(ilu_trmu);
  return _ilu_Strdup(buf);
}

#endif /* BSD or WIN */

/* =================== SIGPIPE handling ==================*/

/*L2, Main unconstrained*/

/*L1 >= {trmu}*/
static ilu_boolean SigPIPEHandler = ilu_FALSE;

/*L1_sup < trmu*/

void
_ilu_HandleSigPIPE(void)
{
  /*
   * Ignore SIGPIPEs, since we can occasionally get them. [Thanks
   * to Dave Nichols <nichols@parc.xerox.com> for the original idea
   * of this code.]
   */

#if defined(_IS_POSIX) && defined(SA_NOCLDSTOP)

  _ilu_AcquireMutex(ilu_trmu);
  if (!SigPIPEHandler) {

    struct sigaction old_handler;
    static struct sigaction new_handler;

    if (sigaction(SIGPIPE, NIL, &old_handler) == 0) {
      if (old_handler.sa_handler == SIG_DFL) {	/* no one's using it */
	new_handler.sa_handler = SIG_IGN;
	if (sigaction(SIGPIPE, &new_handler, NIL) != 0) {
	  ILU_ERRPRINTF("_ilu_HandleSIGPIPE:  Couldn't ignore SIGPIPE signals!\n");
	}
      }
    } else
      ILU_ERRPRINTF("_ilu_HandleSIGPIPE:  Couldn't read handler for SIGPIPE!\n");
  }
  SigPIPEHandler = ilu_TRUE;
  _ilu_ReleaseMutex(ilu_trmu);

#elif (defined(WIN32) || defined(WIN16) || defined(macintosh))

 /* do nothing for these archs as there is no SIGPIPE */

#else		/* not really POSIX -- use ANSI C form */

  typedef void    (*handler) (int);
  _ilu_AcquireMutex(ilu_trmu);
  if (!SigPIPEHandler) {
    handler         old_handler;

    if ((old_handler = (handler) signal(SIGPIPE, SIG_IGN)) != SIG_DFL)
      /* Oops!  Someone's using this one */
      signal(SIGPIPE, old_handler);

    SigPIPEHandler = ilu_TRUE;
  }
  _ilu_ReleaseMutex(ilu_trmu);

#endif				/* _IS_POSIX */
}

ilu_boolean
ilu_SIGPIPE_Handled(void)
{
  _ilu_AcquireMutex(ilu_trmu);
  if (!SigPIPEHandler) {
    SigPIPEHandler = ilu_TRUE;
    _ilu_ReleaseMutex(ilu_trmu);
    return ilu_TRUE;
  }
  _ilu_ReleaseMutex(ilu_trmu);
  return ilu_FALSE;
}

/* ================ BSD/WIN Socket I/O ================ */

/*
 * We have two function pointers which hold the actual bindings for
 * recv() and send().  We used to just write "LocalRecvProc =
 * &recv", but that blows up on bad old Solaris 1 (which doesn't
 * bother to actually declare "recv" and "send" in its header
 * files); this formulation is also better for PCR, in which
 * "recv(..)" is a macro.
 */

static int
_local_recv(int s, char* buf, int len, int flags)
{
#ifdef HAS_SOLARIS2_TCP_SOCKET_BUG
    return read(s, buf, len);
#else
    return recv(s, buf, len, flags);
#endif
}

static int
_local_send(int s, const char* msg, int len, int flags)
{
#ifdef HAS_SOLARIS2_TCP_SOCKET_BUG
    return write(s, msg, len);
#else
    return send(s, msg, len, flags);
#endif
}

static ilu_RecvProc LocalRecvProc = (ilu_RecvProc) &_local_recv;
static ilu_SendProc LocalSendProc = (ilu_SendProc) &_local_send;

/*Main Invariant holds*/

/* The Java JDK 1.0.2 code doesn't do non-blocking correctly.  So we
   need to provide a way to let the Java LSR override the system calls
   read() and write().
*/
void
  ilu_SetRecvSendProcs (ilu_RecvProc rp, ilu_SendProc sp)
{
  LocalRecvProc = rp;
  LocalSendProc = sp;
}

/*Main Invariant holds*/
/*L2 >= {fd's connection's callmu, iomu}*/

ilu_cardinal
_ilu_NbSockRead(int fd, unsigned char *buf, ilu_cardinal bufLen,
		ilu_TransportReport * rpt,
		ILU_ERRS((IoErrs)) * err)
{
  ilu_cardinal    reql = bufLen;
  int             req, got, theerr;
  if (reql > INT_MAX)
    reql = INT_MAX;
  req = (int) reql;
  rpt->tr_eof = ilu_FALSE;
  while (1) {
    got = (*LocalRecvProc)(fd, SEND_RECV_BUF_TYPE(buf), req, 0);
    theerr = sockerrno;
    if (!OS_SOCKERR(got)) {
      rpt->tr_eof = (got == 0);
      if (got == 0) {
	ILU_NOTE(CONNECTION_DEBUG,
		 ("_ilu_NbSockRead: Clean EOF detected on FD %d.\n",
		  fd));
      } else {
	ILU_NOTE(TCP_DEBUG,
		 ("_ilu_NbSockRead (%d, %lu) got %lu bytes\n",
		  fd, (unsigned long) req, (unsigned long) got));
      }
      goto dun;
    }
    switch (theerr) {
    case SOCKERRID(INPROGRESS):
      /*ilu_DebugPrintf("NbSockRead(%d) got EINPROGRESS!\n", fd);*/
#if (defined(WIN32) || defined (WIN16))	/* use winsock errno names */
#if (defined WSAEAGAIN)
    case SOCKERRID(AGAIN):
#endif
#if ((defined WSAEWOULDBLOCK) && !((defined WSAEAGAIN) && WSAEWOULDBLOCK == WSAEAGAIN))
    case SOCKERRID(WOULDBLOCK):
#endif
#else					/* use UNIX errno names */
#if (defined EAGAIN)
    case SOCKERRID(AGAIN):
#endif
#if ((defined EWOULDBLOCK) && !((defined EAGAIN) && EWOULDBLOCK == EAGAIN))
    case SOCKERRID(WOULDBLOCK):
#endif
#endif
      got = 0;
      goto dun;
    case SOCKERRID(INTR):
      break;
    case SOCKERRID(NETDOWN):
    case SOCKERRID(CONNABORTED):
    case SOCKERRID(CONNRESET):
    case SOCKERRID(SHUTDOWN):
#if !(defined(WIN32) || defined(WIN16) || defined(macintosh))
    case SOCKERRID(PIPE):
#endif
      ILU_NOTE(CONNECTION_DEBUG,
	    ("_ilu_NbSockRead: Connection failure (errno %d) from recv(%d).\n",
	     theerr, fd));
      rpt->tr_eof = ilu_TRUE;
      got = 0;
      goto dun;
    default:
      ILU_NOTE(CONNECTION_DEBUG,
	    ("_ilu_NbSockRead: Impossible errno (%d) from recv(%d).\n",
	     theerr, fd));
      return ILU_ERR_CONS1(internal, err, minor, ilu_im_errno,
			   0);
    }
  }
dun:
  ILU_CLER(*err);
  return got;
}

/*L1, L2 unconstrained*/

static          ilu_cardinal
_ilu_NbSockWriteWork(int fd, unsigned char *buf, ilu_cardinal nbytes,
		     ilu_boolean sure, ILU_ERRS((IoErrs)) * err)
{
  ilu_cardinal    reql, sofar = 0;
  int             req, took, theerr;
  while (sofar < nbytes) {
    reql = nbytes - sofar;
    if (reql > INT_MAX)
      reql = INT_MAX;
    req = (int) reql;
    took = (*LocalSendProc) (fd, SEND_RECV_BUF_TYPE(buf) + sofar, req, 0);
    theerr = sockerrno;
    if (!OS_SOCKERR(took)) {
      ILU_NOTE(TCP_DEBUG,
	       ("_ilu_SockWrite (%d, %p, %d) wrote %d bytes\n",
		fd, buf + sofar, req, took));
      sofar += (unsigned) took;
      continue;
    }
    switch (theerr) {
#if (defined(EWOULDBLOCK) || defined(WSAEWOULDBLOCK))
    case SOCKERRID(WOULDBLOCK):
#elif (defined(_IS_POSIX))
    case SOCKERRID(AGAIN):
#else
#error "no applicable errno error for 'Would Block'"
#endif
      if (sure)
	ILU_NOTE(CONNECTION_DEBUG,
	      ("_ilu_SockWrite: Sure write to FD %d WOULDBLOCK!\n",
	       fd));
      goto dun;
    case SOCKERRID(INTR):
      break;
    case SOCKERRID(NOBUFS):
    case SOCKERRID(NETRESET):
    case SOCKERRID(NETDOWN):
    case SOCKERRID(CONNABORTED):
    case SOCKERRID(CONNRESET):
    case SOCKERRID(SHUTDOWN):
#if !(defined(WIN32) || defined(WIN16) || defined(macintosh))
    case SOCKERRID(PIPE):
#endif
      ILU_NOTE(CONNECTION_DEBUG,
      ("_ilu_SockWrite: Connection failure (errno %d) on FD %d.\n",
       theerr, fd));
      return ILU_ERR_CONS1(comm_failure, err, minor, ilu_cfm_conn_lost,
			   sofar);
    default:
      ILU_NOTE(CONNECTION_DEBUG,
	     ("_ilu_SockWrite: Impossible errno (%d) for FD %d.\n",
	      theerr, fd));
      return ILU_ERR_CONS1(internal, err, minor, ilu_im_errno, sofar);
    }
  }
dun:
  return ILU_CLER(*err), sofar;
}

ilu_cardinal
_ilu_NbSockWrite(int fd, unsigned char *buf, ilu_cardinal nbytes,
		 ILU_ERRS((IoErrs)) * err)
{
  return _ilu_NbSockWriteWork(fd, buf, nbytes, ilu_FALSE, err);
}

/*Main Invariant holds; L2 not further constrained*/

ilu_boolean
_ilu_SockWrite(int fd, unsigned char *buf, ilu_cardinal nbytes,
	       ILU_ERRS((IoErrs)) * err)
{
  ilu_cardinal    took, sofar = 0;
  ilu_boolean     sure = ilu_FALSE, first = ilu_TRUE;
  while (sofar < nbytes) {
    if (first)
      first = ilu_FALSE;
    else {
      _ilu_WaitForOutputOnFD(fd, &sure, NIL, err);
      if (ILU_ERRNOK(*err))
	return ilu_FALSE;
    }
    took = _ilu_NbSockWriteWork(fd, buf + sofar, nbytes - sofar, sure,
				err);
    sofar += took;
    if (ILU_ERRNOK(*err))
      return sofar;
  }
  return ILU_CLER(*err);
}


