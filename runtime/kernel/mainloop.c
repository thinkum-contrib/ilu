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
/* $Id: mainloop.c,v 1.84 1999/08/03 01:53:05 janssen Exp $ */
/* Last edited by Mike Spreitzer September 22, 1998 3:02 pm PDT */

#include <math.h>
#include <time.h>

#include "iluntrnl.h"
#include "ilutransport.h"
#include "oscalls.h"	/* for OS_READ and OS_WRITE */

#if (defined(WIN32) || defined(WIN16)|| defined(macintosh))
#include <winsock.h>   /* for recv */
#include <ilusock.h>	/* for creating auxfds */
#endif /*  WIN32 or WIN16 */


/* ================ FineTime Utilities ================ */
/*L1, L2, Main unconstrained*/

ilu_FineTime ilu_FineTime_Add(ilu_FineTime a, ilu_FineTime b)
{
  ilu_FineTime c;
  c.ft_s = a.ft_s + b.ft_s;
  c.ft_t = a.ft_t + b.ft_t;
  if (c.ft_t >= ilu_FineTimeRate) {
      c.ft_t -= ilu_FineTimeRate;
      c.ft_s += 1;
    }
  return c;
}

ilu_FineTime ilu_FineTime_Sub(ilu_FineTime a, ilu_FineTime b)
{
  ilu_FineTime c;
  c.ft_s = a.ft_s - b.ft_s - 1;
  c.ft_t = a.ft_t + ilu_FineTimeRate - b.ft_t;
  if (c.ft_t >= ilu_FineTimeRate) {
      c.ft_t -= ilu_FineTimeRate;
      c.ft_s += 1;
    }
  return c;
}

ilu_FineTime ilu_FineTime_Mul(ilu_FineTime a, float b)
{
  double as  = a.ft_s;
  double at  = a.ft_t;
  double rate = ilu_FineTimeRate;
  double amd = as + (at/rate);
  double amp = amd * b;
  return ilu_FineTime_FromDouble(amp);
}

ilu_FineTime ilu_FineTime_FromDouble(double seconds)
{
  double rate = ilu_FineTimeRate;
  double usecs = seconds * rate;
  double csd = floor(usecs/rate);
  double ctd = usecs - (csd * rate);
  ilu_FineTime c;
  c.ft_s = (ilu_integer) csd;
  c.ft_t = (ilu_cardinal) ctd;
  return (c);
}

ilu_integer ilu_FineTime_Cmp(ilu_FineTime a, ilu_FineTime b)
{
   if (a.ft_s != b.ft_s)
        return (a.ft_s - b.ft_s);
   else return ( ((ilu_integer) a.ft_t) - ((ilu_integer) b.ft_t) );
}

ilu_cardinal ilu_rescale(ilu_cardinal n, ilu_cardinal dfrom,
					 ilu_cardinal dto)
{
  if (dfrom == dto)
      return n;
  else {
      double from = dfrom ? dfrom : (1.0 + (double) (ilu_cardinal) -1);
      double to   = dto   ? dto   : (1.0 + (double) (ilu_cardinal) -1);
      double ans = floor(to * n / from);
      int ians = (int) ans;	/* dll avoid conversion warnings */
      return (ians);
    }
}

/* ================ Main Loop ================ */

/*L1, L2, Main unconstrained*/

static ilu_MainLoop *theMainLoop = &_ilu_DefaultMainLoop;
static int mlPhase = 0;	/* !=0 => can't change theMainLoop */

void ilu_SetMainLoop(ilu_MainLoop *ml)
{
  ILU_AUTOSETDEBUGLEVEL;
  ILU_NOTE(MAINLOOP_DEBUG,
	   ("ilu_SetMainLoop:  theMainLoop = %p, ml = %p, mlPhase = %d\n",
	    theMainLoop, ml, mlPhase));
  _ilu_Assert(ml != NIL, "setting NIL MainLoop");
  _ilu_Assert(mlPhase == 0, "MainLoop already set");
  theMainLoop = ml;
  mlPhase = 1;
  _ilu_gcoAlarm = ilu_CreateAlarm();
  _ilu_gccAlarm = ilu_CreateAlarm();
  _ilu_ioTimeoutAlarm = ilu_CreateAlarm();
  _ilu_grAlarm = ilu_CreateAlarm();
  _ilu_udpAlarm = ilu_CreateAlarm();
  _ilu_soonAlarm = ilu_CreateAlarm();
  _ilu_cvtoAlarm = ilu_CreateAlarm();
  return;
}

ilu_boolean 
ilu_RegisterInputSource(int fd,
			ilu_IOHandler proc, ilu_private rock)
{
  mlPhase = 1;
  ILU_AUTOSETDEBUGLEVEL;
  ILU_NOTE(MAINLOOP_DEBUG, ("ilu_RegisterInputSource (%d, %p(%p))\n",
			    fd, proc, rock));
  _ilu_Assert(theMainLoop->ml_register_input != NULLFN,
	      "RegisterInputSource in threaded runtime");
  return ((*theMainLoop->ml_register_input) (fd, proc, rock));
}

ilu_boolean ilu_UnregisterInputSource(int fd)
{
  ilu_IOHandler   proc;
  ilu_private     rock;
  mlPhase = 1;
  ILU_NOTE(MAINLOOP_DEBUG, ("ilu_UnregisterInputSource (%d)\n", fd));
  if (theMainLoop->ml_unregister_input == NULLFN)
    return ilu_FALSE;
  return ((*theMainLoop->ml_unregister_input) (fd, &proc, &rock));
}

ilu_boolean 
ilu_UnregisterAndReturnInputSource(int fd, ilu_IOHandler * proc,
				   ilu_private * rock)
{
  mlPhase = 1;
  ILU_NOTE(MAINLOOP_DEBUG, ("ilu_UnregisterInputSource (%d)\n", fd));
  if (theMainLoop->ml_unregister_input == NULLFN) {
    *proc = NULLFN;
    *rock = NIL;
    return ilu_FALSE;
  }
  return ((*theMainLoop->ml_unregister_input) (fd, proc, rock));
}

typedef struct {
  /* L1, L2 unconstrained */

  ilu_Closure_s   c;
  ilu_IOHandler   proc;
  ilu_private     rock;
}              *ilu_IOClosure2;
/*
 * Actually a very particular formulation of this, to do
 * impedence-matching between two other particular formulations of a
 * closure.
 */

static void InvokeIO(void *rock)
{
  ilu_IOClosure2  ioc = (ilu_IOClosure2) rock;
  (*ioc->proc) (-1, ioc->rock);
  ilu_free(ioc);
  return;
}

ilu_boolean _ilu_FinishInputSource(int fd)
{
  ilu_IOHandler   proc;
  ilu_private     rock;
  if (ilu_UnregisterAndReturnInputSource(fd, &proc, &rock) && (proc != NULLFN)) {
    ILU_ERRS((internal, bad_param, bad_locks, broken_locks)) lerr;
    ilu_IOClosure2  ioc = (ilu_IOClosure2) ilu_must_malloc(sizeof(*ioc));
    ioc->c.proc = InvokeIO;
    ioc->c.rock = ioc;
    ioc->proc = proc;
    ioc->rock = rock;
    (void) ilu_DoSoon(&ioc->c, &lerr);
    ILU_MUST_BE_SUCCESS(lerr);
    return ilu_TRUE;
  }
  return ilu_FALSE;
}

ilu_boolean 
ilu_RegisterOutputSource(int fd,
			 ilu_IOHandler proc, ilu_private rock)
{
  mlPhase = 1;
  ILU_AUTOSETDEBUGLEVEL;
  ILU_NOTE(MAINLOOP_DEBUG, ("ilu_RegisterOutputSource (%d, %p(%p))\n",
			    fd, proc, rock));
  _ilu_Assert(theMainLoop->ml_register_output != NULLFN,
	      "RegisterOutputSource in threaded runtime");
  return ((*theMainLoop->ml_register_output) (fd, proc, rock));
}

ilu_boolean ilu_UnregisterOutputSource (int fd)
{
  ilu_IOHandler   proc;
  ilu_private     rock;
  mlPhase = 1;
  ILU_NOTE(MAINLOOP_DEBUG, ("ilu_UnregisterOutputSource (%d)\n", fd));
  if (theMainLoop->ml_unregister_output == NULLFN)
    return ilu_FALSE;
  return ((*theMainLoop->ml_unregister_output) (fd, &proc, &rock));
}

ilu_boolean 
ilu_UnregisterAndReturnOutputSource(int fd, ilu_IOHandler * proc,
				    ilu_private * rock)
{
  mlPhase = 1;
  ILU_NOTE(MAINLOOP_DEBUG, ("ilu_UnregisterOutputSource (%d)\n", fd));
  if (theMainLoop->ml_unregister_output == NULLFN) {
    *proc = NULLFN;
    *rock = NIL;
    return ilu_FALSE;
  }
  return ((*theMainLoop->ml_unregister_output) (fd, proc, rock));
}

ilu_boolean _ilu_FinishOutputSource(int fd)
{
  ilu_IOHandler   proc;
  ilu_private     rock;
  if (ilu_UnregisterAndReturnOutputSource(fd, &proc, &rock) && (proc != NULLFN)) {
    ILU_ERRS((internal, bad_param, bad_locks, broken_locks)) lerr;
    ilu_IOClosure2  ioc = (ilu_IOClosure2) ilu_must_malloc(sizeof(*ioc));
    ioc->c.proc = InvokeIO;
    ioc->c.rock = ioc;
    ioc->proc = proc;
    ioc->rock = rock;
    (void) ilu_DoSoon(&ioc->c, &lerr);
    ILU_MUST_BE_SUCCESS(lerr);
    return ilu_TRUE;
  }
  return ilu_FALSE;
}

ilu_SignalCallbackHandler _ilu_SignalCallbackHandler = NULLFN;
ilu_refany		  _ilu_SignalCallbackHandlerArg = NIL;

void ilu_SetSignalCallbackHandler (ilu_SignalCallbackHandler handler,
				   ilu_refany arg,
				   ilu_SignalCallbackHandler *previous,
				   ilu_refany *prev_arg,
				   ilu_Error *err)
{
  if (previous != NULLFN)
    *previous = _ilu_SignalCallbackHandler;
  if (prev_arg != NIL)
    *prev_arg = _ilu_SignalCallbackHandlerArg;

  _ilu_SignalCallbackHandler = handler;
  _ilu_SignalCallbackHandlerArg = arg;
  ILU_CLER(*err);
}

typedef struct ClosureCons_s ClosureCons, *ClosureList;

struct ClosureCons_s {
  ClosureList     next;
  ilu_ClosureProc proc;
  ilu_private     rock;
};

/*L1 >= {daimu}; L2 unconstrained */

ilu_FineTime    t0;
ilu_boolean     t0_set = ilu_FALSE;
ilu_Closure     soons = NIL;

/*Main Invariant holds; L2 no further constrained*/

static void InvokeASoon(ilu_private rock)
{
  ilu_Closure     first;
  ilu_boolean     re_set;
  ilu_FineTime    settime;
  ILU_ERRS((bad_locks, broken_locks, internal)) lerr;
  (void) ilu_EnterMutex(ilu_daimu, &lerr);
  ILU_MUST_BE_SUCCESS(lerr);
  _ilu_Assert(soons != NIL, "InvokeASoon");
  first = soons;
  soons = first->next;
  re_set = (soons != NIL);
  settime = t0;
  (void) ilu_ExitMutex(ilu_daimu, ilu_TRUE, &lerr);
  ILU_MUST_BE_SUCCESS(lerr);
  if (re_set)
    ilu_SetAlarm(_ilu_soonAlarm, settime, InvokeASoon, NIL);
  ILU_NOTE(MAINLOOP_DEBUG,
	   ("InvokeASoon %p(%p)\n", first->proc, first->rock));
  (*first->proc) (first->rock);
  return;
}

/*L1.sup < timu; L2 unconstrained*/

ilu_boolean
ilu_DoSoon(ilu_Closure c,
	   ILU_ERRS((bad_param, bad_locks, broken_locks,
		     internal)) * err)
{
  ilu_FineTime    settime;
  if (!ilu_EnterMutex(ilu_daimu, err))
    return ilu_FALSE;
  c->next = soons;
  soons = c;
  if (!t0_set)
    t0 = ilu_FineTime_Now(), t0_set = ilu_TRUE;
  settime = t0;
  if (!ilu_ExitMutex(ilu_daimu, ilu_TRUE, err))
    return ilu_FALSE;
  ilu_SetAlarm(_ilu_soonAlarm, settime, InvokeASoon, NIL);
  return ILU_CLER(*err);
}

/* timu == ilu_daimu == mxamu */

/*L1 >= {daimu}; L2, Main unconstrained*/

ilu_refany      _ilu_gcoAlarm = &_ilu_gcoDefaultAlarm_s;
ilu_refany      _ilu_gccAlarm = &_ilu_gccDefaultAlarm_s;
ilu_refany      _ilu_ioTimeoutAlarm = &_ilu_iotDefaultAlarm_s;
ilu_refany      _ilu_grAlarm = &_ilu_grDefaultAlarm_s;
ilu_refany      _ilu_udpAlarm = &_ilu_udpDefaultAlarm_s;
ilu_refany      _ilu_soonAlarm = &_ilu_soonDefaultAlarm_s;
ilu_refany      _ilu_cvtoAlarm = &_ilu_cvtoDefaultAlarm_s;


/*L1_sup < timu*/

ilu_refany ilu_CreateAlarm(void)
{
  mlPhase = 1;
  return ( (*theMainLoop->ml_create_alarm)() );
}

void ilu_SetAlarm(ilu_refany alrm, ilu_FineTime t,
		  /*for invoking: Main Invariant holds*/
		  void (*proc)(ilu_private rock),
		  ilu_private rock)
{
  mlPhase = 1;
  (*theMainLoop->ml_set_alarm)(alrm, t, proc, rock);
  return;
}

void ilu_UnsetAlarm(ilu_refany alrm)
{
  mlPhase = 1;
  (*theMainLoop->ml_unset_alarm)(alrm);
  return;
}

void ilu_DestroyAlarm(ilu_refany alrm)
{
  mlPhase = 1;
  (*theMainLoop->ml_destroy_alarm)(alrm);
  return;
}

/*Main Invariant holds; L2 otherwise unconstrained*/
void ilu_RunMainLoop(int *stop)
{
  mlPhase = 1;
  ILU_NOTE(MAINLOOP_DEBUG, ("ilu_RunMainLoop(%p): start\n", stop));
  (*theMainLoop->ml_run)(stop);
  ILU_NOTE(MAINLOOP_DEBUG, ("ilu_RunMainLoop(%p): finish\n", stop));
  return;
}

/*L1, L2 unconstrained*/

void ilu_ExitMainLoop(int *stop)
{
  mlPhase = 1;
  ILU_NOTE(MAINLOOP_DEBUG, ("ilu_ExitMainLoop(%p)\n", stop));
  (*theMainLoop->ml_exit)(stop);
  return;
}


/* ================ Other Concurrent IO Stuff ================ */


typedef struct wait_frame_s WaitFrame;
struct wait_frame_s {		/* A chained stack frame */
  /* L1, L2, Main unconstrained --- single-threaded */

  ilu_Alarmette_s wake;		/* for timing out */
  WaitFrame      *fd_next;	/* next in chain */
  WaitFrame      *hotter, *cooler;	/* stack is doubly linked */
  int             fd;		/* half of chain key */
  int             input;	/* half of chain key */
  int             stop;		/* the stacked value */
  ilu_boolean     sure;		/* was this innermost? */
  ilu_boolean     regd;		/* is FoundFD still registered? */
};

/*L1, L2, Main unconstrained --- single-threaded*/

static WaitFrame *wfs = NIL;
/*
 * A chain of stacks.  The chain runs through the hottest frame of
 * each stack.  If wf->regd, all hotter frames are registered; if
 * !wf->regd, all cooler frames are not registered.  The hottest
 * frame's regd member indicates whether FoundFD is registered as
 * the input handler for the stack's FD.
 */

static void TakeTimeout(ilu_private rock);

static void TAInvoke(ilu_Alarmette a)
{
  WaitFrame      *wf = (WaitFrame *) a;
  ilu_boolean     regd, input = wf->input;
  while (wf->cooler != NIL)
    wf = wf->cooler;
  for (wf = wf; wf != NIL; wf = wf->hotter) {
    regd = wf->regd;
    wf->sure = wf->regd = ilu_FALSE;
    ilu_ExitMainLoop(&(wf->stop));
  }
  if (regd)
    ((input ? ilu_UnregisterInputSource : ilu_UnregisterOutputSource)
     (wf->fd));
  return;
}

static void TASet(ilu_FineTime t)
{
  ilu_SetAlarm(_ilu_ioTimeoutAlarm, t, TakeTimeout, NIL);
}

static void TACancel(void)
{
  ilu_UnsetAlarm(_ilu_ioTimeoutAlarm);
}

static ilu_Alarmette_s timeHead = {&timeHead, &timeHead, ilu_FALSE, {0, 0}};
static ilu_AlarmRep timeAlarm = {&timeHead, TAInvoke, TASet, TACancel};

static void TakeTimeout(ilu_private rock)
{
  ilu_MXAProc(ilu_FineTime_Now(), &timeAlarm);
}

static void FoundFD(int x, ilu_private rock)
{
  WaitFrame      *wf = (WaitFrame *) rock;
  ilu_boolean     regd, input = wf->input;
  int             fd = wf->fd;
  while (wf->cooler != NIL)
    wf = wf->cooler;
  for (wf = wf; wf != NIL; wf = wf->hotter) {
    regd = wf->regd;
    wf->sure = (wf->hotter == NIL) && (wf->stop == 0);
    wf->regd = ilu_FALSE;
    ilu_ExitMainLoop(&(wf->stop));
  }
  if (regd)
    ((input ? ilu_UnregisterInputSource : ilu_UnregisterOutputSource)
     (fd));
  return;
}

static ilu_WaitTech *nsWT = NIL;
static int wtPhase = 0;

/* L1 >= {cmu}; L2 unconstrained */

int             auxfds[2] = {-1, -1};
#define ILU_AUXRD 0
#define ILU_AUXWR 1
/*
 * The two ends of a pipe used into interrupt input waits in a
 * multi-threaded runtime.  Each input wait actually waits on two
 * FDs: the one in question, and auxfds[ILU_AUXRD]; such a wait can
 * be interrupted by writing to auxfds[ILU_AUXWR].
 */

/*Main Invariant holds; called only from start code*/

#if (defined(WIN32) || defined(WIN16))

#include <windows.h>
#include <process.h>

typedef struct {
  struct sockaddr_in paddr;
  HANDLE          done;
}               PatchAuxArg;

static void PatchAux(void *arg)
{
  PatchAuxArg    *pa = (PatchAuxArg *) arg;
  int             res, theerr;
  BOOL            ok;
  res = connect(auxfds[ILU_AUXWR], (struct sockaddr *) & pa->paddr,
		sizeof(pa->paddr));
  theerr = sockerrno;
  _ilu_Assert(!OS_SOCKERR(res), "SetWaitTech connect a");
  ok = SetEvent(pa->done);
  _ilu_Assert(ok, "SetWaitTech notify");
  return;
}

void ilu_SetWaitTech(ilu_WaitTech *wt)
{
  ILU_ERRS((IoErrs, bad_locks, internal)) lerr;
  int             listenFD;
  struct sockaddr_in paddr = {0}, aaddr = {0};
  SOCKET_SIZE_TYPE namelen;
  unsigned long   one = 1;
  DWORD           tres;
  PatchAuxArg     pa;
  BOOL            ok;
  ilu_FineTime    tNow = ilu_FineTime_Now();
  pa.done = CreateEvent(NULL, ilu_TRUE, ilu_FALSE, NULL);
  _ilu_Assert((int)(pa.done), "SetWaitTech CreateEvent");
  _ilu_Assert(wtPhase == 0, "SetWaitTech phase");
  nsWT = wt;
  wtPhase = 1;
  _ilu_AcquireMutex(ilu_cmu);
  ilu_DeltaFD(3);
  (void) _ilu_ReduceFdsTo(ilu_fdbudget, &tNow, &lerr);
  ILU_MUST_BE_SUCCESS(lerr);
  listenFD = socket(AF_INET, SOCK_STREAM, 0);
  _ilu_Assert(!OS_SOCKINV(listenFD), "SetWaitTech socket 1");
  paddr.sin_family = AF_INET;
  paddr.sin_port = 0;
  /* both INADDR_LOOPBACK, and INADDR_ANY both eventually result in WSAEADDRNOTAVAIL  */
  /* paddr.sin_addr.s_addr = INADDR_LOOPBACK; */
  /* paddr.sin_addr.s_addr = INADDR_ANY; */
  _ilu_Assert(!!_ilu_CurrentHostIPAddrString(NULL,
		     (struct in_addr *) (&(paddr.sin_addr.s_addr)),
					     &lerr),
	      "_ilu_CurrentHostIPAddrString");
  _ilu_Assert(!OS_SOCKERR(bind(listenFD, (struct sockaddr *) & paddr,
			       sizeof(paddr))),
	      "SetWaitTech bind");
  namelen = sizeof(paddr);
  _ilu_Assert(!OS_SOCKERR(getsockname(listenFD,
				      (struct sockaddr *) & paddr,
				      &namelen)),
	      "SetWaitTech getsockname");
  _ilu_Assert(!OS_SOCKERR(listen(listenFD, 4)),
	      "SetWaitTech listen");
  auxfds[ILU_AUXWR] = socket(AF_INET, SOCK_STREAM, 0);
  _ilu_Assert(!OS_SOCKINV(auxfds[ILU_AUXWR]), "SetWaitTech socket 2");
  pa.paddr = paddr;
  tres = _beginthread(PatchAux, 0, &pa);
  _ilu_Assert(tres != (DWORD) -1, "SetWaitTech fork");
  auxfds[ILU_AUXRD] = OS_ACCEPT(listenFD, (struct sockaddr *) & aaddr,
				&namelen);
  _ilu_Assert(!OS_SOCKINV(auxfds[ILU_AUXRD]), "SetWaitTech accept");
  tres = WaitForSingleObject(pa.done, INFINITE);
  _ilu_Assert(tres == WAIT_OBJECT_0, "SetWaitTech join");
  ok = CloseHandle(pa.done);
  _ilu_Assert(ok, "SetWaitTech CloseHandle");
  OS_SOCKLOSE(listenFD);
  ilu_DeltaFD(-1);
  _ilu_ReleaseMutex(ilu_cmu);
  return;
}
#elif defined( macintosh )

void ilu_SetWaitTech(ilu_WaitTech *wt)
{
  _ilu_Assert(wtPhase == 0, "SetWaitTech");
  nsWT = wt;
  wtPhase = 1;
}

#else
void ilu_SetWaitTech(ilu_WaitTech *wt)
{
  ILU_ERRS((bad_locks, broken_locks, internal)) lerr;
  int             res;
  ilu_FineTime    tNow = ilu_FineTime_Now();
  _ilu_Assert(wtPhase == 0, "SetWaitTech phase");
  nsWT = wt;
  wtPhase = 1;
  _ilu_AcquireMutex(ilu_cmu);
  ilu_DeltaFD(2);
  (void) _ilu_ReduceFdsTo(ilu_fdbudget, &tNow, &lerr);
  ILU_MUST_BE_SUCCESS(lerr);
  res = OS_PIPE(auxfds);
  _ilu_Assert(res == 0, "SetWaitTech pipe");
  _ilu_ReleaseMutex(ilu_cmu);
  return;
}
#endif /*  WIN32 or WIN16 */

/*Main Invariant holds; L2 otherwise unconstrained*/

static void IOWait(int fd, int input, ilu_boolean *sure,
		   ilu_FineTime *limit)
{
  WaitFrame       this, **pp;
  ilu_boolean     bottom;
  static ilu_Alarmette_s nullAlarmette = {NIL, NIL, ilu_FALSE, {0, 0}};
  this.wake = nullAlarmette;
  this.hotter = NIL;
  this.regd = ilu_TRUE;
  for (pp = &wfs; (*pp) != NIL; pp = &((*pp)->fd_next)) {
    if (((*pp)->fd == fd) && ((*pp)->input == input)) {
      _ilu_Assert((*pp)->hotter == NIL,
		  "mainloop.c:IOWait (*pp)->hotter != NIL");
      this.cooler = *pp;
      (*pp)->hotter = &this;
      this.fd_next = (*pp)->fd_next;
      *pp = &this;
      bottom = ilu_FALSE;
      if (this.cooler->regd)
	goto redy;
      goto regit;
    }
  }
  this.cooler = NIL;
  this.fd_next = wfs;
  wfs = &this;
  bottom = ilu_TRUE;
regit:
  if (input)
       ilu_RegisterInputSource (fd, FoundFD, &this);
  else ilu_RegisterOutputSource(fd, FoundFD, &this);
redy:
  this.fd = fd;
  this.input = input;
  this.stop = 0;
  this.sure = 2;
#ifdef ENABLE_DEBUGGING
  if (ilu_DebugLevel & MAINLOOP_DEBUG) {
    WaitFrame      *q, *r;
    for (q = wfs; q != NIL; q = q->fd_next) {
      _ilu_Assert(q->hotter == NIL,
		  "mainloop.c:IOWait q->hotter != NIL");
      ILU_ERRPRINTF("(ILU):  wait(%d,%d):", q->fd, q->input);
      for (r = q; r != NIL; r = r->cooler) {
	ILU_ERRPRINTF(" *%p=%d, %d", &r->stop, r->stop, r->sure);
      }
      ILU_ERRPRINTF("\n");
    }
  }
#endif /* ENABLE_DEBUGGING */
  if (limit != NIL)
      ilu_MXASet(&timeAlarm, &this.wake, *limit);
  ilu_RunMainLoop(&this.stop);
  *sure = this.sure;
  if (limit != NIL)
      ilu_MXAClear(&timeAlarm, &this.wake);
  if ( bottom ) {
      _ilu_Assert(wfs == &this, "IOWait: pop new");
      wfs = this.fd_next;
    }
  else {
      _ilu_Assert(this.cooler != NIL, "IOWait: this.cooler == NIL");
      _ilu_Assert(this.fd_next == this.cooler->fd_next,
		  "IOWait: pop old");
      *pp = this.cooler;
      (*pp)->hotter = NIL;
    }
  return;
}

void 
_ilu_WaitForInputOnFD(int fd, ilu_boolean * sure,
		      ilu_FineTime * limit,
		      ILU_ERRS((interrupt)) * err)
{
  wtPhase = 1;
  if (nsWT != NIL) {
    (*nsWT->wt_read_wait) (fd, auxfds[ILU_AUXRD], sure, limit, err);
  } else {
    ILU_CLER(*err);
    IOWait(fd, 1, sure, limit);
  }
  return;
}

void
_ilu_WaitForOutputOnFD(int fd, ilu_boolean * sure,
		       ilu_FineTime * limit,
		       ILU_ERRS((interrupt)) * err)
{
  wtPhase = 1;
  if (nsWT != NIL) {
    (*nsWT->wt_write_wait) (fd, -1, sure, limit, err);
    *sure = ilu_TRUE;
  } else {
    ILU_CLER(*err);
    IOWait(fd, 0, sure, limit);
  }
  return;
}

ilu_boolean
_ilu_InterruptFD(int fd, ILU_ERRS((bad_param, internal)) * err)
{
  WaitFrame      *wf1, *wf2;
  wtPhase = 1;
  if (nsWT != NIL)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_threading, ilu_FALSE);
  for (wf1 = wfs; wf1 != NIL; wf1 = wf1->fd_next) {
    if (!ilu_Check(wf1->hotter == NIL, err))
      return ilu_FALSE;
    if (wf1->fd == fd && wf1->regd) {
      ((wf1->input ? ilu_UnregisterInputSource
	: ilu_UnregisterOutputSource)
       (fd));
      for (wf2 = wf1; wf2 != NIL; wf2 = wf2->cooler) {
	wf2->sure = ilu_FALSE;
	wf2->regd = ilu_FALSE;
	ilu_ExitMainLoop(&(wf2->stop));
      }
    }
  }
  ILU_CLER(*err);
  return ilu_TRUE;
}

/* L1 >= {cmu}; L2 unconstrained */

static ilu_boolean fdWaitCohortSet = ilu_FALSE;
static ilu_WaitCohort fdWaitCohort;

extern          ilu_WaitCohort
_ilu_GetFDWaitCohort(ILU_ERRS((no_memory, no_resources,
			       internal)) * err)
{
  if (!fdWaitCohortSet) {
    fdWaitCohort = ilu_CreateWaitCohort("fd", "wait cohort", ilu_TRUE, err);
    if (!fdWaitCohort)
      return NIL;
    fdWaitCohortSet = ilu_TRUE;
  } else
    ILU_CLER(*err);
  if (fdWaitCohort)
    if (!ilu_DeltaWaitCohortRefCount(fdWaitCohort, 1, err))
      return NIL;
  return fdWaitCohort;
}

/*L1.sup < cmu; L2 unconstrained*/
extern          ilu_WaitCohort
ilu_GetFDWaitCohort(ILU_ERRS((no_memory, no_resources, broken_locks,
			      bad_locks, internal)) * err)
{
  ilu_WaitCohort  ans;
  if (!ilu_EnterMutex(ilu_cmu, err))
    return NIL;
  ans = _ilu_GetFDWaitCohort(err);
  if (!ilu_ExitMutex(ilu_cmu, ilu_TRUE, err))
    return NIL;
  return ans;
}

/* L1 >= {cmu}; L1.sup < trmu; L2 unconstrained */

ilu_boolean
_ilu_DisableFDWaits(ILU_ERRS((broken_locks, internal)) * err)
{
  static unsigned char buf = 0;
  int             res, theerr;
  if (!ilu_Check(!!fdWaitCohort, err))
    return ilu_FALSE;
  if (!_ilu_DeltaCohortWaits(fdWaitCohort, 1, err))
    return ilu_FALSE;
  if (fdWaitCohort->iluwc_waitsDisabled == 1) {
    ++buf;
#if (defined(WIN32) || defined(WIN16))
    res = send(auxfds[ILU_AUXWR], (char *) &buf, 1, 0);
    if (OS_SOCKERR(res)) {
      theerr = sockerrno;
      return ILU_ERR_CONS1(internal, err, minor, ilu_im_errno, ilu_FALSE);
    }
#else
    res = write(auxfds[ILU_AUXWR], (char *) &buf, 1);
    if (res != 1) {
      theerr = errno;
      return ILU_ERR_CONS1(internal, err, minor, ilu_im_errno, ilu_FALSE);
    }
#endif
  }
  return ilu_TRUE;
}

ilu_boolean _ilu_EnableFDWaits(ILU_ERRS((broken_locks, internal)) * err)
{
  char            buf[4];
  int             res, theerr;
  if (!ilu_Check(!!fdWaitCohort, err))
    return ilu_FALSE;
  if (!_ilu_DeltaCohortWaits(fdWaitCohort, -1, err))
    return ilu_FALSE;
  if (fdWaitCohort->iluwc_waitsDisabled == 0) {
#if (defined(WIN32) || defined(WIN16))
    res = recv(auxfds[ILU_AUXRD], buf, 1, 0);
    if (OS_SOCKERR(res)) {
      theerr = sockerrno;
      return ILU_ERR_CONS1(internal, err, minor, ilu_im_errno, ilu_FALSE);
    }
#else
    res = read(auxfds[ILU_AUXRD], buf, 1);
    if (res != 1) {
      theerr = errno;
      return ILU_ERR_CONS1(internal, err, minor, ilu_im_errno, ilu_FALSE);
    }
#endif
  }
  return ilu_TRUE;
}

ilu_Closure
_ilu_ClosureFromTIH(ilu_TIH tih)
{
  /*
   * Same structure as far as C is concerned.  ilu_Closure has
   * stronger locking precondition, thus a (tih->proc) expecting the
   * ilu_TIH precondition will also be satisfied by the ilu_Closure
   * precondition.
   */
  return ((ilu_Closure) tih);
}

void _ilu_InvokeTIH(int x, ilu_refany y)
{
  ilu_TIH         tih = (ilu_TIH) y;
  (*tih->proc) (tih->rock);
  return;
}
