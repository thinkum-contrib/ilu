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
/* $Id: bsdmnlp.c,v 1.53 1999/08/03 01:52:45 janssen Exp $ */
/* Last edited by Mike Spreitzer September 18, 1998 1:04 pm PDT */

#include "iluntrnl.h"
#include "oscalls.h"

#include <stdio.h>	/* I/O defs (including popen and pclose) */

#if ((defined(WIN32) || defined(WIN16)) && defined(_WINIO))
#include <winiodef.h>
#endif /* ((defined(WIN32) || defined(WIN16)) && defined(_WINIO)) */

#include <sys/types.h>

#if (defined(WIN32) || defined(WIN16) || defined(macintosh))
#include <time.h>
#define FD_CAST(fd)		((unsigned int)fd)
/* DLL sez: pick up FD_ZERO, etc. */
#include <winsock.h>
#else /* (defined(WIN32) || defined(WIN16) || defined(macintosh) */
#include <sys/time.h>
#define FD_CAST(fd)		(fd)
#endif /* not WIN32 or WIN16 */

#ifndef WIN16
#ifndef WIN32
/* DLL sez: no sys/errno in VC++2 */
#include <sys/errno.h>
#endif /* not WIN32 */
#endif 

#include <math.h>

#ifdef _NEEDS_SELECT_H
#include <sys/select.h>
#endif

#ifdef ENABLE_DEBUGGING
#define MAYBE_DUMP_IO_TABLE	{ ILU_AUTOSETDEBUGLEVEL; if ((ilu_DebugLevel & MAINLOOP_DEBUG) != 0) DumpIOTab(); }
#else
#define MAYBE_DUMP_IO_TABLE
#endif /* def ENABLE_DEBUGGING */

/* ================ default Main Loop for UNIX ================ */

struct io_reg {
  /* L1, L2, Main unconstrained --- single-threaded */

  int             fd, input;
  ilu_IOHandler   proc;
  ilu_private     rock;
};

#define IOTABSZ		256

/*L1, L2, Main unconstrained*/

static struct io_reg IOTab[IOTABSZ];
static int nIdx = 0, lastIdx = 0;
static fd_set readfds, writefds, excnfds;

typedef 
ilu_boolean(*registerer) (int fd,
			  ilu_IOHandler handler, ilu_private rock);
typedef ilu_boolean(*canceller) (int fd);

static registerer extra_reg_inp = NULLFN;
static canceller  extra_can_inp = NULLFN;
static registerer extra_reg_out = NULLFN;
static canceller  extra_can_out = NULLFN;
static void (*extra_set_alarm) (ilu_FineTime t,
		                void (*proc) (ilu_FineTime t)) = NULLFN;
static void (*extra_can_alarm) (void) = NULLFN;

ilu_boolean
ilu_AddRegisterersToDefault(registerer reg_inp,
			    canceller can_inp,
			    registerer reg_out,
			    canceller can_out,
			    void (*set_alarm) (ilu_FineTime t,
					/*Main Invariant holds;
					  L2 otherwise unconstrained*/
					void (*proc) (ilu_FineTime t)),
			    void (*can_alarm) (void)
)
{
  int             i;
  _ilu_Assert(reg_inp != NULLFN && can_inp != NULLFN &&
	      reg_out != NULLFN && can_out != NULLFN &&
	      set_alarm != NULLFN && can_alarm != NULLFN,
	      "NIL extra registerer");
  if (extra_reg_inp != NULLFN)
    return ilu_FALSE;
  extra_reg_inp = reg_inp;
  extra_can_inp = can_inp;
  extra_reg_out = reg_out;
  extra_can_out = can_out;
  extra_set_alarm = set_alarm;
  extra_can_alarm = can_alarm;

  /* Externally register any previous registrations. */
  for (i = 0; i < nIdx; i += 1) {
    if (!((*(IOTab[i].input ? extra_reg_inp : extra_reg_out))
	  (IOTab[i].fd, IOTab[i].proc, IOTab[i].rock)))
      return ilu_FALSE;
  }

  return ilu_TRUE;
}

#ifdef ENABLE_DEBUGGING
static void DumpIOTab(void)
{
  int             i;
  ILU_ERRPRINTF("(ILU/MaybeDumpIOTab):  IOTab size is %d.\n", nIdx);
  for (i = 0; i < nIdx; i += 1)
    ILU_ERRPRINTF("(ILU/MaybeDumpIOTab):  IOTab[%d] ="
		  " {%d, %s, %p(%p)}\n", i,
		  IOTab[i].fd,
		  IOTab[i].input?"I":"O",
		  IOTab[i].proc, IOTab[i].rock);
  return;
}
#endif /* def ENABLE_DEBUGGING */

static          ilu_boolean
Default_UnregisterInputSource(int fd, ilu_IOHandler * proc,
			      ilu_refany * rock)
{
  register int    i;
  static ilu_boolean also = ilu_TRUE;
  if (extra_can_inp != NULLFN)
    also = (*extra_can_inp) (fd);
  FD_CLR(FD_CAST(fd), &readfds);
  FD_CLR(FD_CAST(fd), &excnfds);
  for (i = 0; i < nIdx; i += 1)
    if ((IOTab[i].fd == fd) && IOTab[i].input) {
      *proc = IOTab[i].proc;
      *rock = IOTab[i].rock;
      nIdx--;
      if (lastIdx > nIdx)
	lastIdx = nIdx;
      if (i != nIdx)
	IOTab[i] = IOTab[nIdx];
      MAYBE_DUMP_IO_TABLE;
      return (also);
    }
  ILU_NOTE(MAINLOOP_DEBUG,
     ("(ILU/Default_UnregisterInputSource): FD %d not in table!\n",
      fd));
  *proc = NULLFN;
  *rock = NIL;
  return (ilu_FALSE);
}

static          ilu_boolean
Default_RegisterInputSource(int fd,
			    ilu_IOHandler proc, ilu_private rock)
{
  if (nIdx >= IOTABSZ) {
    ILU_NOTE(MAINLOOP_DEBUG,
	     ("(ILU/Default_RegisterInputSource): table full!\n"));
    return (ilu_FALSE);
  }
  IOTab[nIdx].fd = fd;
  IOTab[nIdx].proc = proc;
  IOTab[nIdx].rock = rock;
  IOTab[nIdx].input = 1;
  nIdx++;
  MAYBE_DUMP_IO_TABLE;
  /*
   * This order has the virtue that if the extra registerer should
   * cause the handler to be called before the registerer returns,
   * we're ready for recursive invocations.
   */
  if (extra_reg_inp != NULLFN)
    if (!(*extra_reg_inp) (fd, proc, rock)) {
      ilu_IOHandler   proc2;
      ilu_private     rock2;
      Default_UnregisterInputSource(fd, &proc2, &rock2);
      return ilu_FALSE;
    }
  return (ilu_TRUE);
}

static ilu_boolean 
Default_UnregisterOutputSource(int fd, ilu_IOHandler * proc,
			       ilu_refany * rock)
{
  register int    i;
  static ilu_boolean also = ilu_TRUE;
  if (extra_can_out != NULLFN)
    also = (*extra_can_out) (fd);
  FD_CLR(FD_CAST(fd), &writefds);
  for (i = 0; i < nIdx; i += 1)
    if ((IOTab[i].fd == fd) && !IOTab[i].input) {
      *proc = IOTab[i].proc;
      *rock = IOTab[i].rock;
      nIdx--;
      if (lastIdx > nIdx)
	lastIdx = nIdx;
      if (i != nIdx) {
	IOTab[i] = IOTab[nIdx];
      }
      MAYBE_DUMP_IO_TABLE;
      return (also);
    }
  ILU_NOTE(MAINLOOP_DEBUG,
    ("(ILU/Default_UnregisterOutputSource): FD %d not in table!\n",
     fd));
  *proc = NULLFN;
  *rock = NIL;
  return (ilu_FALSE);
}

static          ilu_boolean
Default_RegisterOutputSource(int fd,
			     ilu_IOHandler proc, ilu_private rock)
{
  if (nIdx >= IOTABSZ) {
    ILU_NOTE(MAINLOOP_DEBUG,
	     ("(ILU/Default_RegisterOutputSource): table full!\n"));
    return (ilu_FALSE);
  }
  IOTab[nIdx].fd = fd;
  IOTab[nIdx].proc = proc;
  IOTab[nIdx].rock = rock;
  IOTab[nIdx].input = 0;
  nIdx++;
  MAYBE_DUMP_IO_TABLE;
  if (extra_reg_out != NULLFN)
    if (!(*extra_reg_out) (fd, proc, rock)) {
      ilu_IOHandler   proc2;
      ilu_private     rock2;
      Default_UnregisterOutputSource(fd, &proc2, &rock2);
      return ilu_FALSE;
    }
  return (ilu_TRUE);
}

/* timu == ilu_daimu == mxamu */

typedef struct _ilu_DefaultAlarm_struct {
  /*L1 >= {daimu} for access*/
  
  ilu_Alarmette_s ae;
  /*for invoking: Main Invariant holds, L2 otherwise unconstrained*/
  void (*proc)(ilu_private rock);
  ilu_private rock;
} DefaultAlarm_s, *DefaultAlarm;

/*L1 = {daimu};
  forall conn: (L2 >= {conn.iomu}) => (L2 >= {conn.callmu})*/
static void daInvoke(ilu_Alarmette a);

/*L1 >= {daimu}; L2, Main unconstrained*/

static void daUrset(ilu_FineTime t);
static void daUrcancel(void);

static ilu_FineTime alarmTime = {0, 0};
static ilu_boolean alarmSet = 0;
static ilu_Alarmette_s alarmHead = {&alarmHead, &alarmHead, ilu_FALSE, {0,0}};
static ilu_AlarmRep dar = {&alarmHead, daInvoke, daUrset, daUrcancel};

DefaultAlarm_s _ilu_gcoDefaultAlarm_s
	= {{NIL, NIL, ilu_FALSE, {0, 0}}, NULLFN, NIL};
DefaultAlarm_s _ilu_gccDefaultAlarm_s
	= {{NIL, NIL, ilu_FALSE, {0, 0}}, NULLFN, NIL};
DefaultAlarm_s _ilu_iotDefaultAlarm_s
	= {{NIL, NIL, ilu_FALSE, {0, 0}}, NULLFN, NIL};
DefaultAlarm_s _ilu_grDefaultAlarm_s
	= {{NIL, NIL, ilu_FALSE, {0, 0}}, NULLFN, NIL};
DefaultAlarm_s _ilu_udpDefaultAlarm_s
	= {{NIL, NIL, ilu_FALSE, {0, 0}}, NULLFN, NIL};
DefaultAlarm_s _ilu_soonDefaultAlarm_s
	= {{NIL, NIL, ilu_FALSE, {0, 0}}, NULLFN, NIL};
DefaultAlarm_s _ilu_cvtoDefaultAlarm_s
	= {{NIL, NIL, ilu_FALSE, {0, 0}}, NULLFN, NIL};

static void Callem(ilu_FineTime t)
{
  _ilu_AcquireMutex(ilu_daimu);
  ilu_MXAProc(t, &dar);
  _ilu_ReleaseMutex(ilu_daimu);
  return;
}

static void daUrset(ilu_FineTime t)
{
  alarmTime = t;
  alarmSet = ilu_TRUE;
  if (extra_set_alarm != NULLFN)
    (*extra_set_alarm) (t, Callem);
}

/*L1 = {daimu};
  forall conn: (L2 >= {conn.iomu}) => (L2 >= {conn.callmu})*/
static void daInvoke(ilu_Alarmette a)
{
  DefaultAlarm da = (DefaultAlarm) a;
  _ilu_ReleaseMutex(ilu_daimu);
  (*da->proc)(da->rock);
  _ilu_AcquireMutex(ilu_daimu);
  return;
}

static void daUrcancel(void)
{
  alarmSet = ilu_FALSE;
  if (extra_can_alarm != NULLFN)
    (*extra_can_alarm) ();
}

/*L1_sup < timu*/

static ilu_refany Default_CreateAlarm(void)
{
  DefaultAlarm    da = (DefaultAlarm) ilu_must_malloc(sizeof(*da));
  ilu_Alarmette_s ae = {NIL, NIL, ilu_FALSE, {0, 0}};
  da->ae = ae;
  da->proc = NULLFN;
  da->rock = NIL;
  return da;
}

static void 
Default_SetAlarm(ilu_refany thisalarm, ilu_FineTime t,
		 /* for invoking: Main Invariant holds */
		 void (*proc) (ilu_private rock),
		 ilu_private rock)
{
  DefaultAlarm    da = (DefaultAlarm) thisalarm;
  _ilu_AcquireMutex(ilu_daimu);
  da->proc = proc;
  da->rock = rock;
  ilu_MXASet(&dar, &da->ae, t);
  _ilu_ReleaseMutex(ilu_daimu);
  return;
}

static void Default_UnsetAlarm(ilu_refany thisalarm)
{
  DefaultAlarm    da = (DefaultAlarm) thisalarm;
  _ilu_AcquireMutex(ilu_daimu);
  ilu_MXAClear(&dar, &da->ae);
  _ilu_ReleaseMutex(ilu_daimu);
  return;
}

static void Default_DestroyAlarm(ilu_refany thisalarm)
{
  DefaultAlarm    da = (DefaultAlarm) thisalarm;
  _ilu_AcquireMutex(ilu_daimu);
  ilu_MXAClear(&dar, &da->ae);
  _ilu_ReleaseMutex(ilu_daimu);
  ilu_free(da);
  return;
}

/*L1, L2 unconstrained*/
static float MyRand(void)
{
  static int      initted = 0, rm, m;
  int             r1, r2, r3;
  if (!initted) {
    rm = RAND_MAX;
    m = (int) pow((double) rm, 1.0 / 3.0);
    initted = 1;
  }
  r1 = rand();
  _ilu_Assert(0 <= r1 && r1 <= rm, "MyRand");
  r2 = r1 / m;
  r3 = r2 % m;
  return (r3 / (float) m);
}

int             TestIluMainLoop = 0;
int             TestIluMainLoopSet = 0;

/*Main Invariant holds; L2 otherwise unconstrained*/
static void Default_RunMainLoop(int *stop)
{
  struct io_reg   lastreg;
  int             theerr;
  *stop = 0;
  if (!TestIluMainLoopSet) {
    TestIluMainLoopSet = 1;
    TestIluMainLoop = getenv("HACKILUMAINLOOP") != NIL;
  }
  while (!*stop) {		/* Find one thing to do, and do it. */
    struct timeval  tv, *ptv = NIL;
    int             width, status, doprints = ilu_FALSE;
    register int    i;
    ilu_FineTime    now, dt;

    /* Does the default alarm impl need processing? */
    _ilu_AcquireMutex(ilu_daimu);
    if (alarmSet) {
      now = ilu_FineTime_Now();
      if (ilu_FineTime_Cmp(now, alarmTime) >= 0) {
	alarmSet = 0;
	ILU_NOTE(MAINLOOP_DEBUG,
		 ("(ILU/Default_RunMainLoop):  calling alarms.\n"));
	ilu_MXAProc(now, &dar);
	_ilu_ReleaseMutex(ilu_daimu);
	goto next;		/* find next thing to do */
      }
      dt = ilu_FineTime_Sub(alarmTime, now);
      ptv = &tv;
    } else
      now.ft_s = now.ft_t = dt.ft_s = dt.ft_t = 0;
    _ilu_ReleaseMutex(ilu_daimu);
    if (!TestIluMainLoop)
      /* do nothing */;
    else if (ptv) {
      double          t1 = dt.ft_s + (dt.ft_t / (1.0 * ilu_FineTimeRate));
      double          t2 = t1 * (0.1 + 0.8 * MyRand());
      dt = ilu_FineTime_FromDouble(t2);
    } else {
      dt = ilu_FineTime_FromDouble(1.0 + 14.0 * MyRand());
      ptv = &tv;
    }
    if (ptv) {
      tv.tv_sec = dt.ft_s;
      tv.tv_usec = ilu_rescale(dt.ft_t, ilu_FineTimeRate, 1000000);
    }

    /* Try the results of the previous call on select */
    while (lastIdx > 0) {
      ilu_boolean     forEx, forIo;
      fd_set         *iofds;
      --lastIdx;
      forEx = (IOTab[lastIdx].fd >= 0) && (FD_ISSET(IOTab[lastIdx].fd, &excnfds) != 0);
      iofds = IOTab[lastIdx].input ? &readfds : &writefds;
      forIo = (IOTab[lastIdx].fd >= 0) && (FD_ISSET(IOTab[lastIdx].fd, iofds) != 0);
      if (forEx || forIo) {
	lastreg = IOTab[lastIdx];
	ILU_NOTE(MAINLOOP_DEBUG,
	      ("(ILU/Default_RunMainLoop):  calling IOTab[%d](%ld,%ld) = {%d, %s, %p(%p)}\n",
	       lastIdx, (long) forEx, (long) forIo,
	       lastreg.fd,
	       lastreg.input?"I":"O",
	       lastreg.proc,
	       lastreg.rock));
	if (forIo)
          FD_CLR(FD_CAST(lastreg.fd), iofds);
	(*lastreg.proc) (lastreg.fd, lastreg.rock);
	goto next;
      }
    }

    /* Make a new call on select */
    width = 0;
    FD_ZERO(&readfds);
    FD_ZERO(&writefds);
    FD_ZERO(&excnfds);
#ifdef ENABLE_DEBUGGING
    doprints = !!(ilu_DebugLevel & MAINLOOP_DEBUG);
    if (doprints)
      ilu_DebugPrintfIntro("(ILU/Default_RunMainLoop): select ");
#endif
    for (i = 0; i < nIdx; i++) {
      if (doprints)
	ilu_DebugPrintfCont("%d%s ", IOTab[i].fd,
			    IOTab[i].input ? "I" : "O");
      if (IOTab[i].fd >= 0) {
	FD_SET(IOTab[i].fd, (IOTab[i].input ? &readfds : &writefds));
	FD_SET(IOTab[i].fd, &excnfds);
      };
      if (IOTab[i].fd >= width)
	width = IOTab[i].fd + 1;
    }
    lastIdx = nIdx;
    if (ptv)
      if (doprints)
	ilu_DebugPrintfCont("%d.%06ds @ %lu:%lu ",
			    ptv->tv_sec, ptv->tv_usec,
			    (long unsigned) now.ft_s,
			    (long unsigned) now.ft_t);

    status = select(width, &readfds, &writefds, &excnfds, ptv);
    theerr = sockerrno;

#ifdef ENABLE_DEBUGGING
    if (doprints) {
      ilu_DebugPrintfCont("=> %d %d:", status, theerr);
      for (i = 0; i < width; i++) {
	if (FD_ISSET(i, &readfds))
	  ilu_DebugPrintfCont(" %dI", i);
	if (FD_ISSET(i, &writefds))
	  ilu_DebugPrintfCont(" %dO", i);
	if (FD_ISSET(i, &excnfds))
	  ilu_DebugPrintfCont(" %dE", i);
      }
      ilu_DebugPrintfFin("\n");
    }
#endif /* ENABLE_DEBUGGING */
    
    if (status == -1 && theerr == SOCKERRID(INTR) &&
	(_ilu_SignalCallbackHandler != NULLFN)) {
      ILU_NOTE(MAINLOOP_DEBUG, ("%s", "(ILU/Default_RunMainLoop):"
			     "  Calling signal handler callback\n"));
      (*_ilu_SignalCallbackHandler)(_ilu_SignalCallbackHandlerArg);
    } else {
      ASSERT(status >= 0 || theerr == SOCKERRID(INTR), buf,
            (buf, "default mainloop:select failed, errno=%d=%s",
             theerr, strerror(theerr)));
    }

    if (TestIluMainLoop) {
      int             victim = (int) (nIdx * MyRand());
      struct io_reg   vior;
      _ilu_Assert(0 <= victim && victim < nIdx, "TestIluMainLoop");
      vior = IOTab[victim];
      FD_SET(vior.fd, (vior.input ? &readfds : &writefds));
      ILU_NOTE(MAINLOOP_DEBUG,
	    ("(ILU/Default_RunMainLoop):  Hacking IOTab[%d]={fd=%d, input=%d}\n",
	     victim, vior.fd, vior.input));
    }
    
    if (status < 0)
      lastIdx = 0;
next:
    status = 0;			/* harmless; useful for breakpoint
				 * setting */
  }
  return;
}

/*L1, L2 unconstrained*/

static void Default_ExitMainLoop(int *stop)
{
  *stop = 1;
  return;
}

ilu_MainLoop    _ilu_DefaultMainLoop = {
  Default_RunMainLoop, Default_ExitMainLoop,
  Default_RegisterInputSource, Default_UnregisterInputSource,
  Default_RegisterOutputSource, Default_UnregisterOutputSource,
  Default_CreateAlarm,
  Default_SetAlarm, Default_UnsetAlarm, Default_DestroyAlarm
};
