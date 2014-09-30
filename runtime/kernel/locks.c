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
/* $Id: locks.c,v 1.66 1999/08/03 01:52:46 janssen Exp $ */
/* Last tweaked by Mike Spreitzer September 18, 1998 2:56 pm PDT */
/* Chris Jacobi, June 10, 1998 11:33 am PDT */

#define _POSIX_SOURCE

#include "iluntrnl.h"

ilu_boolean _ilu_lockTechDefined = ilu_FALSE;


typedef struct {
  ilu_Alarmette_s alarmette;
  ilu_Condition   realCond;
  ilu_integer     waits;
  ILU_ERRS((broken_locks)) lerr;
}               *GenToCv;

/*L1 not meaningful; this module implements L1 locks;
  L2 unconstrained*/


typedef struct {
  ilu_string      d1, d2;
  int             held, ownsStrings;
}               DefaultMutex;

static          ilu_Mutex
Default_CreateMutex(ilu_string d1, ilu_string d2)
{
  DefaultMutex   *dm = (DefaultMutex *) ilu_malloc(sizeof(DefaultMutex));
  if (dm == NIL)
    return NIL;
  dm->d1 = _ilu_Strdup(d1);
  dm->d2 = _ilu_Strdup(d2);
  if ((d1 != NIL && dm->d1 == NIL) || (d2 != NIL && dm->d2 == NIL)) {
    ilu_free(dm->d1);
    ilu_free(dm->d2);
    ilu_free(dm);
    return NIL;
  }
  dm->held = 0;
  dm->ownsStrings = ilu_TRUE;
  return dm;
}

static void
Default_UnconsMutex(ilu_Mutex m, ilu_string * d1, ilu_string * d2,
		    ILU_ERRS((bad_param)) * err)
{
  DefaultMutex   *dm = (DefaultMutex *) m;
  if (dm == NIL)
    ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, NIL);
  else {
    ILU_CLER(*err);
    *d1 = dm->d1;
    *d2 = dm->d2;
  }
  return;
}

static void 
Default_AcquireMutex(ilu_private m,
		     ILU_ERRS((bad_param, bad_locks)) * err)
{
  DefaultMutex   *dm = (DefaultMutex *) m;
  if (dm == NIL)
    ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0);
  else if (dm->held)
    ILU_ERR_CONS0(bad_locks, err, 0);
  else {
    ILU_CLER(*err);
    dm->held = 1;
  }
  return;
}

static void 
Default_HoldMutex(ilu_private m,
		  ILU_ERRS((bad_param, bad_locks)) * err)
{
  DefaultMutex   *dm = (DefaultMutex *) m;
  if (dm == NIL)
    ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0);
  else if (!dm->held)
    ILU_ERR_CONS0(bad_locks, err, 0);
  else
    ILU_CLER(*err);
  return;
}

static void 
Default_ReleaseMutex(ilu_private m,
		     ILU_ERRS((bad_param, bad_locks)) * err)
{
  DefaultMutex   *dm = (DefaultMutex *) m;
  if (dm == NIL)
    ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0);
  else if (!dm->held)
    ILU_ERR_CONS0(bad_locks, err, 0);
  else {
    ILU_CLER(*err);
    dm->held = 0;
  }
  return;
}

static void 
Default_DestroyMutex(ilu_private m,
		     ILU_ERRS((bad_param, bad_locks)) * err)
{
  DefaultMutex   *dm = (DefaultMutex *) m;
  if (dm == NIL)
    ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0);
  else if (dm->held)
    ILU_ERR_CONS0(bad_locks, err, 0);
  else {
    if (dm->ownsStrings) {
      ilu_free(dm->d1);
      ilu_free(dm->d2);
    }
    ilu_free(dm);
    ILU_CLER(*err);
  }
  return;
}

static ilu_ForkProc theForkProc = NULLFN;

ilu_boolean
ilu_Fork(ilu_ClosureProc proc,
	 void *rock,
	 ILU_ERRS((no_memory,
		   no_resources,
		   internal)) * err)
{
  if (!ilu_Check(!!theForkProc, err))
    return ilu_FALSE;
  else
    return (*theForkProc) (proc, rock, err);
}

ilu_boolean
  ilu_SetForkTech (ilu_ForkProc fp,
		   ILU_ERRS((internal)) *err)
{
  static ilu_boolean initialized = ilu_FALSE;
  if (initialized)
    return ILU_ERR_CONS1(internal, err, minor, ilu_im_dupForkProc, ilu_FALSE);
  initialized = ilu_TRUE;
  theForkProc = fp;
  return ILU_CLER(*err);    
}

ilu_boolean
  ilu_KernelThreaded(void)
{
  return (theForkProc != NULLFN);
}

static ilu_LockTech Default_LockTech[1] = {
  {ilu_FALSE, Default_CreateMutex, Default_UnconsMutex,
    Default_AcquireMutex, Default_HoldMutex, Default_ReleaseMutex,
    Default_DestroyMutex, NULLFN, NULLFN, NULLFN, NULLFN, NULLFN} };

static ilu_LockTech *theLockTech = Default_LockTech;
static int ltPhase = 0;
static int nIn = 0;
static int stdDumped = 0;

static DefaultMutex def_otmu = {"global ", "otmu", 0, 0};
static DefaultMutex def_cmu = {"global ", "cmu", 0, 0};
static DefaultMutex def_prmu = {"global ", "prmu", 0, 0};
static DefaultMutex def_trmu = {"global ", "trmu", 0, 0};
static DefaultMutex def_gcmu = {"global ", "gcmu", 0, 0};
static DefaultMutex def_daimu = {"global ", "daimu", 0, 0};
static DefaultMutex def_debugmu = {"global ", "debug", 0, 0};
static DefaultMutex def_cvtomu = {"global ", "CV timeout generic impl", 0, 0};

ilu_Mutex ilu_otmu = &def_otmu;
ilu_Mutex ilu_cmu = &def_cmu;
ilu_Mutex ilu_prmu = &def_prmu;
ilu_Mutex ilu_trmu = &def_trmu;
ilu_Mutex ilu_gcmu = &def_gcmu;
ilu_Mutex ilu_daimu = &def_daimu;
ilu_Mutex ilu_debugmu = &def_debugmu;
ilu_Mutex ilu_cvtomu = &def_cvtomu;

void _ilu_CommitThreadedness(void)
{
  ltPhase = 1;
  return;
}

static ilu_boolean _ilu_GetMutexNames (ilu_Mutex m, ilu_string *d1, ilu_string *d2)
{
  ilu_boolean val;
  ilu_Error err = ILU_INIT_NO_ERR;
  (*(theLockTech->lt_muncons))(m, d1, d2, &err);
  val = ILU_ERROK(err);
  ILU_HANDLED(err);
  if (!val) { *d1 = "?"; *d2 = "?"; };
  return val;
}

ilu_Mutex ilu_CreateMutex(ilu_string d1, ilu_string d2)
{ return _ilu_CreateMutex(d1, d2); }

ilu_Mutex _ilu_CreateMutex(ilu_string d1, ilu_string d2)
{
  ilu_private     m;
  ILU_AUTOSETDEBUGLEVEL;
  ltPhase = 1;
  m = (theLockTech->lt_mcreate) (d1, d2);
  if (((ilu_DebugLevel & LOCK_DEBUG) != 0) && d2 && (strcmp(d2, "debugmu") != 0)) {
	ILU_NOTE(LOCK_DEBUG, ("ilu_CreateMutex(%s %s) => %p\n",
		     d1, d2, m));
  }
  return m;
}

void ilu_AcquireMutex(ilu_Mutex m)
{ _ilu_AcquireMutex(m); }

void _ilu_AcquireMutex(ilu_Mutex m)
{
  ILU_ERRS((bad_param, bad_locks)) lerr = ILU_INIT_NO_ERR;
  ILU_AUTOSETDEBUGLEVEL;
#ifdef ENABLE_DEBUGGING
  if (((ilu_DebugLevel & LOCK_DEBUG) != 0) && (m != ilu_debugmu))
    {
      ilu_string d1, d2;
      (void) _ilu_GetMutexNames(m, &d1, &d2);
      ilu_DebugPrintf ("_ilu_AcquireMutex:  %p (%s %s)\n", m, d1, d2);
    };
#endif
  nIn++;
  if (!stdDumped && (m != ilu_debugmu)) {
    ILU_NOTE(LOCK_DEBUG, ("ilu_otmu = %p\n", ilu_otmu));
    ILU_NOTE(LOCK_DEBUG, ("ilu_cmu = %p\n", ilu_cmu));
    ILU_NOTE(LOCK_DEBUG, ("ilu_prmu = %p\n", ilu_prmu));
    ILU_NOTE(LOCK_DEBUG, ("ilu_trmu = %p\n", ilu_trmu));
    ILU_NOTE(LOCK_DEBUG, ("ilu_gcmu = %p\n", ilu_gcmu));
    ILU_NOTE(LOCK_DEBUG, ("ilu_daimu = %p\n", ilu_daimu));
    ILU_NOTE(LOCK_DEBUG, ("ilu_debugmu = %p\n", ilu_debugmu));
    ILU_NOTE(LOCK_DEBUG, ("ilu_cvtomu = %p\n", ilu_cvtomu));
    stdDumped = 1;
  }
  (theLockTech->lt_acquire) (m, &lerr);
  ILU_MUST_BE_SUCCESS(lerr);
  return;
}

ilu_boolean
ilu_EnterMutexWork(ilu_Mutex m, ilu_boolean hard,
	       ILU_ERRS((bad_locks, internal, broken_locks)) * err,
		   const char *file, int line)
{
  ILU_ERRS((bad_param, bad_locks)) lerr = ILU_INIT_NO_ERR;
  ILU_AUTOSETDEBUGLEVEL;
  if (((ilu_DebugLevel & LOCK_DEBUG) != 0) && (m != ilu_debugmu)) {
    ilu_string      d1, d2;
    (void) _ilu_GetMutexNames(m, &d1, &d2);
    ilu_DebugPrintf("ilu_EnterMutex(%p (%s %s) %s) @ %s:%d)\n",
		  m, d1, d2, (hard ? "hard" : "soft"), file, line);
  }
  nIn++;
  if (!stdDumped && (m != ilu_debugmu)) {
    ILU_NOTE(LOCK_DEBUG, ("ilu_otmu = %p\n", ilu_otmu));
    ILU_NOTE(LOCK_DEBUG, ("ilu_cmu = %p\n", ilu_cmu));
    ILU_NOTE(LOCK_DEBUG, ("ilu_prmu = %p\n", ilu_prmu));
    ILU_NOTE(LOCK_DEBUG, ("ilu_trmu = %p\n", ilu_trmu));
    ILU_NOTE(LOCK_DEBUG, ("ilu_gcmu = %p\n", ilu_gcmu));
    ILU_NOTE(LOCK_DEBUG, ("ilu_daimu = %p\n", ilu_daimu));
    ILU_NOTE(LOCK_DEBUG, ("ilu_debugmu = %p\n", ilu_debugmu));
    ILU_NOTE(LOCK_DEBUG, ("ilu_cvtomu = %p\n", ilu_cvtomu));
    stdDumped = 1;
  }
  (theLockTech->lt_acquire) (m, &lerr);
  if (ILU_ERROK(lerr)) {
    if (!hard)
      ILU_CLER(*err);
    return ilu_TRUE;
  }
  if (hard) {
    ILU_HANDLED(*err);
    ILU_ERR_FULLCONS0(broken_locks, err, 0, file, line);
  } else
    ILU_ERR_SWITCH(lerr) {
    ILU_ERR_CASE(bad_param, v)
      ILU_ERR_FULLCONS1(internal, err, minor, ilu_im_inv_mutex, 0,
			file, line);
    ILU_ERR_CASE(bad_locks, v)
      ILU_ERR_FULLCONS0(bad_locks, err, 0, file, line);
    ILU_ERR_ELSE
      ILU_ERR_FULLCONS1(internal, err, minor, ilu_im_unhandled, 0,
			file, line);
    } ILU_ERR_ENDSWITCH;
  ILU_HANDLED(lerr);
  return ilu_FALSE;
}

ilu_boolean
ilu_ExitMutexWork(ilu_Mutex m, ilu_boolean hard,
	       ILU_ERRS((bad_locks, internal, broken_locks)) * err,
		  const char *file, int line)
{
  ILU_ERRS((bad_param, bad_locks)) lerr = ILU_INIT_NO_ERR;
#ifdef ENABLE_DEBUGGING
  if (((ilu_DebugLevel & LOCK_DEBUG) != 0) && (m != ilu_debugmu)) {
    ilu_string      d1, d2;
    (void) _ilu_GetMutexNames(m, &d1, &d2);
    ilu_DebugPrintf("ilu_ExitMutex(%p (%s %s) %s) @ %s:%d\n",
		    m, d1, d2, (hard ? "hard" : "soft"),
		    file, line);
  }
#endif
  (theLockTech->lt_release) (m, &lerr);
  nIn--;
  if (ILU_ERROK(lerr))
    return ilu_TRUE;
  if (hard)
    ILU_ERR_FULLCONS0(broken_locks, err, 0, file, line);
  else
    ILU_ERR_SWITCH(lerr) {
    ILU_ERR_CASE(bad_param, v)
      ILU_ERR_FULLCONS1(internal, err, minor, ilu_im_inv_mutex, 0,
			file, line);
    ILU_ERR_CASE(bad_locks, v)
      ILU_ERR_FULLCONS0(bad_locks, err, 0, file, line);
    ILU_ERR_ELSE
      ILU_ERR_FULLCONS1(internal, err, minor, ilu_im_unhandled, 0,
			file, line);
    } ILU_ERR_ENDSWITCH;
  ILU_HANDLED(lerr);
  return ilu_FALSE;
}

void ilu_HoldMutex(ilu_Mutex m)
{
  _ilu_HoldMutex(m);
}

void _ilu_HoldMutex(ilu_Mutex m)
{
  ilu_Error lerr;
#ifdef ENABLE_DEBUGGING
  if (((ilu_DebugLevel & LOCK_DEBUG) != 0) && (m != ilu_debugmu))
    {
      ilu_string d1, d2;
      (void) _ilu_GetMutexNames(m, &d1, &d2);
      ilu_DebugPrintf ("_ilu_HoldMutex:  %p (%s %s)\n", m, d1, d2);
    };
#endif
  (theLockTech->lt_hold)(m, &lerr);
  ILU_MUST_BE_SUCCESS(lerr);
  return;
}

void ilu_ReleaseMutex(ilu_Mutex m)
{ _ilu_ReleaseMutex(m); }

void _ilu_ReleaseMutex(ilu_Mutex m)
{
  ILU_ERRS((bad_param, bad_locks)) lerr = ILU_INIT_NO_ERR;
#ifdef ENABLE_DEBUGGING
  if (((ilu_DebugLevel & LOCK_DEBUG) != 0) && (m != ilu_debugmu))
    {
      ilu_string d1, d2;
      (void) _ilu_GetMutexNames(m, &d1, &d2);
      ilu_DebugPrintf ("_ilu_ReleaseMutex:  %p (%s %s)\n", m, d1, d2);
    };
#endif
  (theLockTech->lt_release)(m, &lerr);
  ILU_MUST_BE_SUCCESS(lerr);
  nIn--;
  return;
}

/*L1 disjoint {m}*/
ilu_boolean
ilu_DestroyMutex(ilu_Mutex m,
		 ILU_ERRS((bad_locks, bad_param, internal)) * err)
{
#ifdef ENABLE_DEBUGGING
  if ((ilu_DebugLevel & LOCK_DEBUG) != 0) {
    ilu_string      d1, d2;
    (void) _ilu_GetMutexNames(m, &d1, &d2);
    ilu_DebugPrintf("_ilu_DestroyMutex(%p (%s %s))\n", m, d1, d2);
  };
#endif
  (theLockTech->lt_mdestroy) (m, err);
  return ILU_ERROK(*err);
}

ilu_boolean ilu_CanCondition(void)
{
  return _ilu_CanCondition();
}

ilu_boolean _ilu_CanCondition(void)
{
  if (theLockTech->lt_ccreate != NULLFN)
       return ilu_TRUE;
  else return ilu_FALSE;
}

ILU_DEF_ERR(CantCondition, "Condition variables not available to kernel")
{}

static          ilu_boolean
_ilu_GetConditionNames(ilu_Condition c,
		       ilu_string * d1, ilu_string * d2)
{
  ilu_boolean     val;
  ilu_Error       err = ILU_INIT_NO_ERR;
  if (!theLockTech->lt_canTimeoutWait)
    c = ((GenToCv) c)->realCond;
  (*(theLockTech->lt_cuncons)) (c, d1, d2, &err);
  val = ILU_ERROK(err);
  ILU_HANDLED(err);
  if (!val) {
    *d1 = "?";
    *d2 = "?";
  };
  return val;
}

static          ilu_Condition
FullCreateCondition(ilu_string d1, ilu_string d2,
		    ILU_ERRS((no_memory)) * err)
{
  ilu_Condition   c;
  ILU_NOTE(LOCK_DEBUG, ("ilu_CreateCondition:  %s, %s\n", d1, d2));
  c = (*theLockTech->lt_ccreate) (d1, d2);
  if (c)
    ILU_CLER(*err);
  else
    ILU_ERR_CONS1(no_memory, err, nbytes, 0, (void) 6);
  if (c && !theLockTech->lt_canTimeoutWait) {
    GenToCv         gtc = ilu_MallocE(sizeof(*gtc), err);
    if (gtc) {
      static ilu_Alarmette_s initAlarmette = {0};
      gtc->alarmette = initAlarmette;
      gtc->realCond = c;
      gtc->waits = 0;
      ILU_CLER(gtc->lerr);
      c = gtc;
    } else {
      ilu_Error       lerr;
      (*theLockTech->lt_cdestroy) (c, &lerr);
      ILU_HANDLED(lerr);
      c = NIL;
    }
  }
  ILU_NOTE(LOCK_DEBUG, ("ilu_CreateCondition:  => %p\n", c));
  return (c);
}

ilu_Condition
ilu_CreateCondition(ilu_string d1, ilu_string d2,
		    ILU_ERRS((bad_param/threading,
			      no_memory, no_resources)) * err)
{
  ltPhase = 1;
  if (!theLockTech->lt_ccreate)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_threading, NIL);
  return FullCreateCondition(d1, d2, err);
}

ilu_Condition
_ilu_CreateCondition(ilu_string d1, ilu_string d2,
		     ILU_ERRS((internal/check,
			       no_memory, no_resources)) * err)
{
  ltPhase = 1;
  if (!ilu_Check(!!theLockTech->lt_ccreate, err))
    return NIL;
  return FullCreateCondition(d1, d2, err);
}
				
ilu_boolean
ilu_CondNotify(ilu_Condition c,
	       ILU_ERRS((broken_locks)) * err)
{
  ILU_ERRS((bad_param)) lerr;
  ilu_Condition   rc;
  ltPhase = 1;
  if (theLockTech->lt_notify == NULLFN)
    return ILU_ERR_CONS0(broken_locks, err, ilu_FALSE);
  rc = theLockTech->lt_canTimeoutWait ? c : ((GenToCv) c)->realCond;
#ifdef ENABLE_DEBUGGING
  if ((ilu_DebugLevel & LOCK_DEBUG) != 0)
    {
      ilu_string d1, d2;
      (void) _ilu_GetConditionNames(c, &d1, &d2);
      ilu_DebugPrintf ("_ilu_CondNotify:  %p (%s %s)\n", c, d1, d2);
    };
#endif
  (*theLockTech->lt_notify) (rc, &lerr);
  if (ILU_ERRNOK(lerr)) {
    ILU_HANDLED(lerr);
    return ILU_ERR_CONS0(broken_locks, err, ilu_FALSE);
  }
  return ILU_CLER(*err);
}

ILU_ERRS((CantCondition)) ilu_NotifyCondition(ilu_Condition c)
{ return _ilu_NotifyCondition(c); }

ILU_ERRS((CantCondition)) _ilu_NotifyCondition(ilu_Condition c)
{
  ilu_Error       lerr;
  if (ilu_CondNotify(c, &lerr))
    return lerr;
  ILU_HANDLED(lerr);
  return ILU_ERR_CONS0(CantCondition, &lerr, lerr);
}

ILU_ERRS((CantCondition)) ilu_DestroyCondition(ilu_Condition c)
{
  ilu_Error       lerr;
  if (_ilu_CondDestroy(c, &lerr))
    return lerr;
  ILU_HANDLED(lerr);
  return ILU_ERR_CONS0(CantCondition, &lerr, lerr);
}

#define BYTESPERCARD	(sizeof(ilu_cardinal)/sizeof(unsigned char))
#define DIGITSPERCARD	(3*BYTESPERCARD)

/*Main Invariant*/
static void     cvtoActivate(void *rock);

/* L1.sup = cvtomu */

static void cvtoInvoke(ilu_Alarmette a)
{
  GenToCv           gtc = (GenToCv) a;
  (void) ilu_CondNotify(gtc, &gtc->lerr);
  return;
}

static void cvtoSet(ilu_FineTime t)
{
  ilu_SetAlarm(_ilu_cvtoAlarm, t, cvtoActivate, NIL);
  return;
}

static void cvtoClear(void)
{
  ilu_UnsetAlarm(_ilu_cvtoAlarm);
  return;
}

/*L1 >= {cvtomu}*/

static ilu_Alarmette_s cvtoHead = {&cvtoHead, &cvtoHead, ilu_FALSE, {0, 0}};
ilu_AlarmRep cvtoAM = {&cvtoHead, cvtoInvoke, cvtoSet, cvtoClear};

/*Main Invariant*/

static void cvtoActivate(void *rock)
{
  ilu_Error       lerr;
  if (ilu_EnterMutex(ilu_cvtomu, &lerr)) {
    ilu_MXAProc(ilu_FineTime_Now(), &cvtoAM);
    (void) ilu_ExitMutex(ilu_cvtomu, ilu_TRUE, &lerr);
  }
  ILU_MUST_BE_SUCCESS(lerr);
  return;
}

/* L1 mostly meaningless */

ilu_boolean
ilu_CMWait2Full(ilu_Condition c, ilu_Mutex m, ilu_Mutex m2,
		const ilu_FineTime * timeout,
		ILU_ERRS((broken_locks)) * err,
		const char *filename, int lineno)
{
  ILU_ERRS((bad_param, bad_locks)) lerr;
  ilu_boolean     genericTO = timeout && !theLockTech->lt_canTimeoutWait;
  GenToCv         gtc = NIL;
  ilu_Condition   rc = c;
  ltPhase = 1;
  if (theLockTech->lt_wait == NULLFN)
    return ILU_ERR_CONS0(broken_locks, err, ilu_FALSE);
  if (!theLockTech->lt_canTimeoutWait) {
    gtc = (GenToCv) c;
    rc = gtc->realCond;
  }
#ifdef ENABLE_DEBUGGING
  if ((ilu_DebugLevel & LOCK_DEBUG) != 0) {
    ilu_string      d1, d2;
    char            tobuf[DIGITSPERCARD * 2 + 9];
    (void) _ilu_GetConditionNames(c, &d1, &d2);
    if (timeout)
      sprintf(tobuf, "%lu:%lu", timeout->ft_s, timeout->ft_t);
    else
      strcpy(tobuf, "infinite");
    ilu_DebugPrintf("ilu_CMWait2:  c = %p (%s %s), m = %p, m2= %p, t.o.=%s at %s:%d\n",
		    c, d1, d2, m, m2, tobuf, filename, lineno);
  };
#endif
  if (genericTO) {
    ILU_CLER(gtc->lerr);
    if (!ilu_ReEnterMutex(ilu_cvtomu, err))
      return ilu_FALSE;
    ilu_MXASet(&cvtoAM, &gtc->alarmette, *timeout);
    if (!ilu_ExitMutex(ilu_cvtomu, ilu_TRUE, err)) {
      ilu_MXAClear(&cvtoAM, &gtc->alarmette);
      return ilu_FALSE;
    }
  }
  if (!theLockTech->lt_canTimeoutWait)
    gtc->waits++;
  (*theLockTech->lt_wait) (rc, m, m2, genericTO ? NIL : timeout, &lerr);
  if (!theLockTech->lt_canTimeoutWait)
    gtc->waits--;
  if (genericTO) {
    if (!ilu_ReEnterMutex(ilu_cvtomu, err))
      return ilu_FALSE;
    ilu_MXAClear(&cvtoAM, &gtc->alarmette);
    if (!ilu_ExitMutex(ilu_cvtomu, ilu_TRUE, err))
      return ilu_FALSE;
    if (ILU_ERROK(lerr))
      lerr = gtc->lerr;
    else
      ILU_HANDLED(gtc->lerr);
  }
  if (ILU_ERRNOK(lerr)) {
    ILU_HANDLED(lerr);
    return ILU_ERR_CONS0(broken_locks, err, ilu_FALSE);
  }
  return ILU_CLER(*err);
}

ilu_boolean
_ilu_CondDestroy(ilu_Condition c,
		 ILU_ERRS((internal)) * err)
{
  ILU_ERRS((bad_param)) lerr;
  ilu_Condition   rc = c;
  GenToCv         gtc = NIL;
  ltPhase = 1;
  if (!ilu_Check(!!theLockTech->lt_cdestroy, err))
    return ilu_FALSE;
  if (!theLockTech->lt_canTimeoutWait) {
    gtc = (GenToCv) c;
    if (!ilu_Check(gtc->waits == 0, err))
      return ilu_FALSE;
    rc = gtc->realCond;
  }
#ifdef ENABLE_DEBUGGING
  if ((ilu_DebugLevel & LOCK_DEBUG) != 0) {
    ilu_string      d1, d2;
    (void) _ilu_GetConditionNames(c, &d1, &d2);
    ilu_DebugPrintf("_ilu_CondDestroy:  %p (%s %s)\n", c, d1, d2);
  };
#endif
  (*theLockTech->lt_cdestroy) (rc, &lerr);
  if (!ilu_Check(ILU_ERROK(lerr), err)) {
    ILU_HANDLED(lerr);
    return ilu_FALSE;
  }
  if (!theLockTech->lt_canTimeoutWait) {
    if (!ilu_ReEnterMutex(ilu_cvtomu, err))
      return ilu_FALSE;
    ilu_MXAClear(&cvtoAM, &gtc->alarmette);
    if (!ilu_ExitMutex(ilu_cvtomu, ilu_TRUE, err))
      return ilu_FALSE;
    ilu_free(gtc);
  }
  return ilu_TRUE;
}

void
ilu_SetLockTech(ilu_LockTech * lt,
		ILU_ERRS((bad_param, no_memory)) * err)
{
  ilu_LockTech   *old = theLockTech;
  ILU_AUTOSETDEBUGLEVEL;
  _ilu_lockTechDefined = ilu_FALSE;
  ILU_NOTE(LOCK_DEBUG,
	("ilu_RegisterLockTech (%p), ltPhase == %d, nIn == %d\n",
	 lt, ltPhase, nIn));
  if (ltPhase != 0 || nIn != 0) {
    ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_late, 0);
    return;
  }
  if (lt != NIL) {
    theLockTech = lt;
  }
  {
    ilu_Mutex nu_debugmu, nu_otmu, nu_cmu, nu_prmu, nu_trmu, nu_gcmu, nu_daimu, nu_cvtomu;

    /* create debug mutex first so it exists while creating the others -- 
       that way if a problem occurs there, we have the debug mutex properly set up
       for use with debug output */
    nu_debugmu = _ilu_CreateMutex("global ", "debugmu");
    if (!nu_debugmu) {
      theLockTech = old;
      ILU_ERR_CONS1(no_memory, err, nbytes, 0, 0);
      return;
    }
    else       
      ilu_debugmu = nu_debugmu;

    nu_otmu = _ilu_CreateMutex("global ", "otmu");
    nu_cmu = _ilu_CreateMutex("global ", "cmu");
    nu_prmu = _ilu_CreateMutex("global ", "prmu");
    nu_trmu = _ilu_CreateMutex("global ", "trmu");
    nu_gcmu = _ilu_CreateMutex("global ", "gcmu");
    nu_daimu = _ilu_CreateMutex("global ", "daimu");
    nu_cvtomu = _ilu_CreateMutex("global ", "cvtomu");
    
    if (nu_otmu != NIL && nu_cmu != NIL &&
	nu_prmu != NIL && nu_trmu != NIL && nu_gcmu != NIL &&
	nu_daimu != NIL) {
      ilu_otmu = nu_otmu;
      ilu_cmu = nu_cmu;
      ilu_prmu = nu_prmu;
      ilu_trmu = nu_trmu;
      ilu_gcmu = nu_gcmu;
      ilu_daimu = nu_daimu;
      ilu_cvtomu = nu_cvtomu;
      _ilu_connHandoffChange = (ilu_CreateCondition
				("global ", "conn handoff buff chg",
				 err));
      if (ILU_ERROK(*err))
	_ilu_connCountChg = ilu_CreateCondition("global ",
						"conn count chg",
						err);
      _ilu_lockTechDefined = ilu_TRUE;
    } else {
      theLockTech = old;
      ILU_ERR_CONS1(no_memory, err, nbytes, 0, 0);
    }
  }
  return;
}

ilu_Mutex ilu_GetOTMutex(void)
{
  return ilu_otmu;
}
