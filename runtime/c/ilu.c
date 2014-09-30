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
/* $Id: ilu.c,v 1.272 1999/09/16 00:23:55 janssen Exp $ */
/* Last edited by Mike Spreitzer July 21, 1998 3:23 pm PDT */

#include <stdio.h>

#include <stdarg.h>

#if ((defined(WIN32) || defined(WIN16)) && defined(_WINIO))
#include <winiodef.h>
#endif /* ((defined(WIN32) || defined(WIN16)) && defined(_WINIO)) */

#include <string.h>
#include <stdlib.h>		/* for exit */
#include <ctype.h>		/* for isalnum() */

#include "ilucpvt.h"

#ifdef ILU_C_TIMING_STATISTICS
#include <sys/time.h>
#include <sys/resource.h>

extern int getrusage(int, struct rusage *);

#endif /* def ILU_C_TIMING_STATISTICS */

#include <oscalls.h>		/* for OS_SLEEP */
#include <iludebug.h>		/* for LSR_DEBUG */
 
#include <iluhash.h>

#define ILU_OPTIONAL(x)	x
#define ILU_PASS(x)	x
#define ILU_RETAIN(x)	x
#define ILU_GLOBAL(x)	x
 
#define NULLFN		0

#define SEQUENCE_INCREMENT 5

#define IOFNS_TABLE_SIZE	137

#define MOST_SPECIFIC_ILU_CLASS(cobj) \
	ILU_CLASS_OF_C_CLASS((cobj)->iluco_class)

#define ILU_CLASS_OF_C_CLASS(cclass) \
	(cclass)->ilucc_sections[0].ilucdts_introType

/* L1, L2 unconstrained */

#define ILU_C_Logging ((ilu_DebugLevel & LSR_DEBUG) != 0)

static ILU_C_Object	_ILU_C_ORB[1];
CORBA_Object		ILU_C_ORB = _ILU_C_ORB;

static ilu_boolean c_registered = ilu_FALSE;
static ilu_cardinal c_lang = 47;

static ILU_C_Server defaultServer = ILU_NIL;

static ILU_C_Server GetDefaultServer(void);

static ilu_boolean threaded = ilu_FALSE;
static ilu_boolean threadedSet = ilu_FALSE;
static ilu_boolean threadedOther = ilu_FALSE;
/* true if another LSR has registered threads, instead of C runtime */

static void     (*Fork) (void (*proc) (void *arg), void *arg) = 0;

static void MonitorOutgoingConnection(void *rock);

ilu_LanguageIndex _ILU_C_LanguageIndex;

static ilu_cardinal MyLangIdx(void)
{
  if (!c_registered) {
    c_lang = ilu_RegisterLanguage("ANSI-C");
    c_registered = ilu_TRUE;
  }
  return c_lang;
}

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
      sprintf(nbuf, "%lu", (long unsigned int) x->nbytes);
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

static void DisposeErrFull(ilu_Error * err, char *doing, int lineno)
{
  if (ILU_ERRNOK(*err)) {
    char            ef[ERRFMTSIZE];
    ILU_ERRPRINTF("ILU/C: Warning: %s ignored at line %d of %s while %s.\n",
		  FmtError(ef, err), lineno, __FILE__, doing);
    ILU_HANDLED(*err);
    ILU_CLER(*err);
  }
}

#define DisposeErr(e,d) DisposeErrFull((e), (d), __LINE__)

static HashTable TypeRegistry = ILU_NIL;

static void
  InitializeTypeRegistry (void)
{
  if (TypeRegistry == ILU_NIL) {
    TypeRegistry = ilu_hash_MakeNewTable (37, ilu_hash_HashPointer, ilu_hash_PointerCompare);
    if (TypeRegistry == ILU_NIL) {
      ilu_DebugPrintf ("ILU/C:  can't initialize table of surrogate types!\n");
      exit(1);
    }
  }					  
}

void
  _ILU_C_RegisterSurrogateCType (ilu_Class c, ILU_C_Class t)
{
  ILU_C_Class old = ILU_NIL;
  _ilu_Assert(c && t,
	      "Null Class passed to _ILU_C_RegisterSurrogateCType");

  if (TypeRegistry == ILU_NIL)
    InitializeTypeRegistry();
  old = ilu_hash_FindInTable (TypeRegistry, c);
  if (old == ILU_NIL)
    ilu_hash_AddToTable (TypeRegistry, c, t);
}

ILU_C_Class
  ILU_C_RegisterCustomSurrogateType (ilu_Class c, ILU_C_Class t,
				     ILU_C_ENVIRONMENT *env)
{
  ILU_C_Class old = ILU_NIL;

  if (t == ILU_NIL ||
      t->ilucc_sections == ILU_NIL ||
      t->ilucc_sections[0].ilucdts_introType == ILU_NIL) {
    ILU_C_RAISE_SYSTEM(env,BAD_PARAM,ilu_bpm_nil,NO);
    return ILU_NIL;
  };
  if (t->ilucc_sections[0].ilucdts_introType != c) {
    ILU_C_RAISE_SYSTEM(env,BAD_PARAM,ilu_bpm_broken,NO);
    return ILU_NIL;
  };

  if (TypeRegistry == ILU_NIL)
    InitializeTypeRegistry();
  old = ilu_hash_FindInTable (TypeRegistry, c);
  if (old != ILU_NIL) {
    if (!ilu_hash_RemoveFromTable (TypeRegistry, c)) {
      ILU_C_RAISE_SYSTEM(env,INTERNAL,ilu_im_callFail,NO);
      return ILU_NIL;
    }
  };
  if (!ilu_hash_AddToTable (TypeRegistry, c, t)) {
    ILU_C_RAISE_SYSTEM(env,INTERNAL,ilu_im_callFail,NO);
    return ILU_NIL;
  };
  env->_major = CORBA_NO_EXCEPTION;
  env->returnCode = ILU_NIL;
  return old;
}

/* Main Invariant holds; L2 not further constrained */
CORBA_Object
CORBA_Object_duplicate_full(CORBA_Object h, CORBA_Environment * s,
			    char *filename, int lineno)
{
  ILU_C_Server    csvr;
  ilu_Server      ksvr;
  ILU_C_Class     theclass;
  ilu_Class       ot;
  ILU_ERRS((bad_locks, internal, broken_locks, bad_param, imp_limit))
    lerr;
  CORBA_completion_status cstat = CORBA_COMPLETED_NO;
  int             newrefcnt;
  if (!h) {
    ILU_NOTE(OBJECT_DEBUG,
	     ("ILU/C: duplicating null object (at %s:%d).\n",
	      filename, lineno));
    ILU_CLER(lerr);
    cstat = CORBA_COMPLETED_YES;
    goto dun0;
  }
  if (!(theclass = h->iluco_class)
      || !(ot = ILU_CLASS_OF_C_CLASS(theclass))
      || !(csvr = h->server) || !(ksvr = csvr->ilucs_ks)) {
    ILU_ERR_CONS1(bad_param, &lerr, minor, ilu_bpm_broken, (void) 6);
    goto dun0;
  }
  ilu_EnterServer(ksvr, ot);
  if (h->iluco_refcnt <= 0) {
    ILU_ERR_CONS1(bad_param, &lerr, minor, ilu_bpm_broken, (void) 6);
    goto dun1;
  }
  newrefcnt = h->iluco_refcnt + 1;
  if (newrefcnt < h->iluco_refcnt) {
    ILU_ERR_CONS1(imp_limit, &lerr, minor, ilu_ilm_refcnt, (void) 6);
    goto dun1;
  }
  ILU_NOTE(OBJECT_DEBUG,
	   ("ILU/C: duplicating %p; refcnt := %d (at %s:%d).\n",
	    h, newrefcnt, filename, lineno));
  h->iluco_refcnt = newrefcnt;
  ILU_CLER(lerr);
  cstat = CORBA_COMPLETED_YES;
dun1:
  ilu_ExitServer(ksvr, ot);
dun0:
  _ILU_C_ConvertError(s, &lerr, cstat);
  return (h);
}

/* Inside(lspo's server, lspo's MST) */
static void DestroyCServer(ILU_C_Server csvr)
{
  int             dofree = ((ilu_DebugLevel & RELEASEOBJ_DEBUG) == 0);
  ILU_ERRS((bad_param, internal)) lerr;
  ilu_Server      ksvr = csvr->ilucs_ks;
  if (ksvr) {
    ILU_NOTE(OBJECT_DEBUG,
	     ("ILU/C: %s C server %p (ks %p, id %s.\n",
	      (dofree ? "freeing" : "would free"),
	      csvr, ksvr, ilu_IDOfServer(ksvr)));
    csvr->ilucs_ks = ILU_NIL;
    ilu_SetLSS(ksvr, ILU_NIL, MyLangIdx(), &lerr);
    ILU_MUST_BE_SUCCESS(lerr);
    if (dofree)
      ilu_free(csvr);
  }
  return;
}

/* Inside(lspo's server, lspo's MST) */
static void DestroyCObject(CORBA_Object lspo)
{
  int             dofree = ((ilu_DebugLevel & RELEASEOBJ_DEBUG) == 0);
  ILU_C_Server    csvr = lspo->server;
  ILU_C_FinalizationProc fp = lspo->iluco_class->ilucc_finalize;
  ilu_Object      ko = lspo->iluco_ko;
  if (fp)
    (*fp) (lspo->instanceData);
  ILU_NOTE(OBJECT_DEBUG,
	   ("ILU/C: %s %p (ko %p, ih %s, CS %p).\n",
	    (dofree ? "freeing" : "would free"),
	    lspo, ko, (ko ? ilu_IhOfObject(ko) : "(extinct)"),
	    csvr));
  if (dofree)
    ilu_free(lspo);
  csvr->ilucs_objcnt -= 1;
  if (!csvr->ilucs_refcnt && !csvr->ilucs_objcnt)
    DestroyCServer(csvr);
  return;
}

static void 
IncrRefcnt(CORBA_Object lspo)
{
  int             newrefcnt = lspo->iluco_refcnt + 1;
  _ilu_Assert(lspo->iluco_refcnt >= 0, "IncrRefcnt 1");
  _ilu_Assert(newrefcnt > lspo->iluco_refcnt, "IncrRefcnt 2");
  lspo->iluco_refcnt = newrefcnt;
  ILU_NOTE(OBJECT_DEBUG,
	("ILU/C: producing %p again (refcnt := %d).\n",
	 lspo, newrefcnt));
  return;
}

/* main invariant holds */
ilu_integer
ILU_C_SetObjectGCTimeout (ILU_C_Object *obj, ilu_integer timeout, ILU_C_ENVIRONMENT *env)
{
  ilu_integer old_timeout = 0;
  ilu_Error lerr;
  ilu_Object kobj = _ILU_C_KernelObjOfObj (obj);
  ILU_C_Server    csvr = obj->server;
  ilu_Server      ksvr;
  ILU_C_Class     cclass = obj->iluco_class;
  ilu_Class       ot;
  if (!csvr || !(ksvr = csvr->ilucs_ks) || !cclass
      || !(ot = ILU_CLASS_OF_C_CLASS(cclass))) {
    ILU_C_RAISE_SYSTEM(env, BAD_PARAM, ilu_bpm_broken, NO);
    return 0;
  };
  ilu_EnterServer(ksvr, ot);
  if ((kobj = obj->iluco_ko) != ILU_NIL) {
    old_timeout = ilu_SetObjectGCTimeout(kobj, timeout, &lerr);
    _ILU_C_ConvertError (env, &lerr, CORBA_COMPLETED_NO);
  } else {
    ILU_C_RAISE_SYSTEM(env, BAD_PARAM, ilu_bpm_broken, NO);
  };
  ilu_ExitServer(ksvr, ot);
  return old_timeout;
}

/* L1 < gcmu */
ilu_FineTime
ILU_C_SetDefaultGCPingPeriod (ilu_FineTime period, ILU_C_ENVIRONMENT *env)
{
  ILU_C_SET_SUCCESSFUL(env);
  return ilu_SetDefaultGCPingPeriod(period);
}

/* Main Invariant holds; L2 not further constrained */
void 
_ILU_C_Object_release_full(ILU_C_Object * o,
			   char *filename, int lineno)
{
  CORBA_Object_release_full(o, ILU_NIL, filename, lineno);
}

/* Main Invariant holds; L2 not further constrained */
void 
CORBA_Object_release_full(CORBA_Object h, CORBA_Environment * s,
			  char *filename, int lineno)
{
  ILU_C_Server    csvr;
  ilu_Server      ksvr;
  ilu_Class       mskt;
  ILU_ERRS((bad_locks, internal, broken_locks, bad_param, imp_limit))
    lerr;
  CORBA_completion_status cstat = CORBA_COMPLETED_NO;
  if (!h) {
    ILU_NOTE(OBJECT_DEBUG,
	  ("ILU/C: releasing null object (at %s:%d).\n",
	   filename, lineno));
    ILU_CLER(lerr);
    goto dun0;
  }
  if (!(csvr = h->server) || !(ksvr = csvr->ilucs_ks)
      || !(mskt = MOST_SPECIFIC_ILU_CLASS(h))) {
    ILU_ERR_CONS1(bad_param, &lerr, minor, ilu_bpm_broken, (void) 6);
    goto dun0;
  }
  ilu_EnterServer(ksvr, mskt);
  if (h->iluco_refcnt <= 0 || csvr->ilucs_objcnt <= 0) {
    ILU_ERR_CONS1(bad_param, &lerr, minor, ilu_bpm_broken, (void) 6);
    goto dun1;
  }
  h->iluco_refcnt -= 1;
  ILU_NOTE(OBJECT_DEBUG,
	("ILU/C: releasing %p; refcnt := %d (at %s:%d).\n",
	 h, h->iluco_refcnt, filename, lineno));
  if (!h->iluco_refcnt && !h->iluco_kvi) {
    ilu_Object      ko = h->iluco_ko;
    DestroyCObject(h);
    ilu_RegisterLSO(ko, mskt, ILU_NIL, MyLangIdx(), &lerr);
    if (ILU_ERRNOK(lerr)) goto dun1;
  }
  ILU_CLER(lerr);
  cstat = CORBA_COMPLETED_YES;
dun1:
  ilu_ExitServer(ksvr, mskt);
dun0:
  if (s)
    _ILU_C_ConvertError(s, &lerr, cstat);
  else
    ILU_HANDLED(lerr);
  return;
}

/* Main Invariant holds */
ILU_RUNTIME_PUBLIC ILU_C_Server
ILU_C_Server_duplicate(ILU_C_Server csvr,
		       ILU_C_ENVIRONMENT * env)
{
  ilu_Error       lerr;
  ilu_Server      ksvr;
  CORBA_completion_status cstat = CORBA_COMPLETED_NO;
  if (!csvr)
    ILU_ERR_CONS1(bad_param, &lerr, minor, ilu_bpm_duh, (void) 6);
  else if (!(ksvr = csvr->ilucs_ks))
    ILU_ERR_CONS1(bad_param, &lerr, minor, ilu_bpm_closed, (void) 6);
  else {
    if (ilu_EnterServerMutex(ksvr, ilu_FALSE, &lerr)) {
      if (!(csvr->ilucs_refcnt || csvr->ilucs_objcnt))
	ILU_ERR_CONS1(bad_param, &lerr, minor, ilu_bpm_closed, (void) 6);
      else {
	int             newcnt = csvr->ilucs_refcnt + 1;
	if (newcnt <= csvr->ilucs_refcnt)
	  ILU_ERR_CONS1(imp_limit, &lerr, minor, ilu_ilm_refcnt,
			(void) 6);
	else {
	  csvr->ilucs_refcnt = newcnt;
	  cstat = CORBA_COMPLETED_YES;
	  ILU_CLER(lerr);
	  ILU_NOTE(SERVER_DEBUG,
		   ("ILU/C: duplicating C server %p (ks %p, id %s);"
		    " refcnt := %d.\n",
		    csvr, ksvr, ilu_IDOfServer(ksvr),
		    csvr->ilucs_refcnt));
	}
      }
      (void) ilu_ExitServerMutex(ksvr, ilu_TRUE, &lerr);
    }
  }
  _ILU_C_ConvertError(env, &lerr, cstat);
  return csvr;
}

/* Main Invariant holds */
ILU_RUNTIME_PUBLIC void
ILU_C_Server_release(ILU_C_Server csvr,
		     ILU_C_ENVIRONMENT *env)
{
  ilu_Error       lerr;
  ilu_Server      ksvr;
  CORBA_completion_status cstat = CORBA_COMPLETED_NO;
  if (!csvr)
    ILU_ERR_CONS1(bad_param, &lerr, minor, ilu_bpm_duh, (void) 6);
  else if (!(ksvr = csvr->ilucs_ks))
    ILU_ERR_CONS1(bad_param, &lerr, minor, ilu_bpm_closed, (void) 6);
  else {
    if (ilu_EnterServerMutex(ksvr, ilu_FALSE, &lerr)) {
      if (!(csvr->ilucs_refcnt > 0))
	ILU_ERR_CONS1(bad_param, &lerr, minor, ilu_bpm_closed, (void) 6);
      else {
	csvr->ilucs_refcnt -= 1;
	cstat = CORBA_COMPLETED_YES;
	ILU_CLER(lerr);
	ILU_NOTE(SERVER_DEBUG,
		 ("ILU/C: releasing C server %p (ks %p, id %s);"
		  " refcnt := %d.\n",
		  csvr, ksvr, ilu_IDOfServer(ksvr),
		  csvr->ilucs_refcnt));
	if (!(csvr->ilucs_refcnt || csvr->ilucs_objcnt))
	  DestroyCServer(csvr);
      }
      ilu_ExitServerMutex(ksvr, ilu_TRUE, &lerr);
    }
  }
  _ILU_C_ConvertError(env, &lerr, cstat);
  return;
}

ILU_RUNTIME_PUBLIC char *
  ILU_C_IDOfServer (ILU_C_Server s)
{
  return ilu_IDOfServer(s->ilucs_ks);
}

/* Inside(kobj's server, kobj's MST) */
static void
InnerShutdownObject(ILU_C_OBJECT cobj, ilu_boolean tolerantly,
		    ILU_C_ENVIRONMENT * env)
{
  ilu_Object      kobj = cobj->iluco_ko;
  ILU_C_Server    csvr = cobj->server;
  ilu_Error	  lerr;
  ilu_Class kclass;

  kclass = ilu_ClassOfObject(kobj);
  if (cobj->iluco_refcnt < 0) {
    ILU_C_RAISE_SYSTEM(env, BAD_PARAM, ilu_bpm_broken, NO);
    return;
  }
  if (csvr->ilucs_refcnt < 0 || csvr->ilucs_objcnt <= 0) {
    ILU_C_RAISE_SYSTEM(env, BAD_PARAM, ilu_bpm_broken, NO);
    return;
  }
  if (cobj->iluco_refcnt == 0) {
    if (!tolerantly) {
      ILU_C_RAISE_SYSTEM(env, BAD_PARAM, ilu_bpm_closed, NO);
      return;
    }
  } else
    cobj->iluco_refcnt -= 1;
  if (kobj) {
    (void) ilu_RegisterLSO(kobj, kclass, ILU_NIL, MyLangIdx(), &lerr);
    if (ILU_ERRNOK(lerr)) {
      _ILU_C_ConvertError(env, &lerr, CORBA_COMPLETED_NO);
      cobj->iluco_refcnt += 1;
      return;
    }
    cobj->iluco_ko = ILU_NIL;
    cobj->iluco_kvi = 0;
  }
  ILU_NOTE(OBJECT_DEBUG,
	   ("ILU/C: shutdown obj %p; refcnt := %d;"
	    " former kernel obj = %p.\n",
	    cobj, cobj->iluco_refcnt, kobj));
  if (!cobj->iluco_refcnt && !cobj->iluco_kvi)
    DestroyCObject(cobj);
  ILU_C_SET_SUCCESSFUL(env);
}

/* Main Invariant holds */
void ILU_C_ShutdownObject(ILU_C_OBJECT cobj, ILU_C_ENVIRONMENT * env)
{
  if (cobj) {
    ILU_C_Server    csvr = cobj->server;
    ilu_Server      ksvr;
    ILU_C_Class     cclass = cobj->iluco_class;
    ilu_Class       ot;
    if (!csvr || !(ksvr = csvr->ilucs_ks) || !cclass
	|| !(ot = ILU_CLASS_OF_C_CLASS(cclass))) {
      ILU_C_RAISE_SYSTEM(env, BAD_PARAM, ilu_bpm_broken, NO);
      return;
    }
    ilu_EnterServer(ksvr, ot);
    InnerShutdownObject(cobj, ilu_FALSE, env);
    ilu_ExitServer(ksvr, ot);
  } else
    ILU_C_RAISE_SYSTEM(env, BAD_PARAM, 0, NO);
  return;
}

/* Inside(obj->server, obj->class) */
static ilu_boolean 
C_Noter(ilu_Object obj, int vi)
{
  CORBA_Object    lspo;
  lspo = ilu_GetLanguageSpecificObject(obj, MyLangIdx());
  if (lspo) {
    lspo->iluco_kvi = vi;
    ILU_NOTE(OBJECT_DEBUG,
	     ("ILU/C: kernel-is-very-interested(%p) := %d).\n",
	      lspo, vi));
    _ilu_Assert(lspo->server->ilucs_objcnt > 0, "C_Noter 1");
    if (!lspo->iluco_kvi && !lspo->iluco_refcnt) {
      DestroyCObject(lspo);
      return ilu_FALSE;
    }
  }
  return ilu_TRUE;
}

/*L1 >= {ksvr}; L2 unconstrained*/
static          ILU_C_Server
FindSurrogateServer(ilu_Server ksvr)
{
  ILU_C_Server    ans = ilu_GetLSS(ksvr, MyLangIdx());
  if (!ans) {
    ILU_ERRS((bad_param, internal)) lerr;
    ans = ilu_must_malloc(sizeof(*ans));
    ans->ilucs_ks = ksvr;
    ans->ilucs_objcnt = ans->ilucs_refcnt = 0;
    ILU_NOTE(OBJECT_DEBUG,
	   ("ILU_C:  Creating C Server %p for kernel server %p.\n",
	    ans, ksvr));
    ilu_SetLSS(ksvr, ans, MyLangIdx(), &lerr);
    ILU_MUST_BE_SUCCESS(lerr);
  }
  return ans;
}

/*
 * The following proc is called to get the LSO from the KO, which is
 * allowed to be true or a surrogate; in the true case, we simply
 * fail if there isn't already an associated LSO.  This proc always
 * calls ilu_RegisterLanguageSpecificObject, so it can be used to
 * satisfy the requirements of following ilu_ObjectOfSBH.
 */
/* Inside(obj->server, obj->class) */
static ILU_C_Object *
_ILU_k_CreateSurrogateFromRegistry(ilu_Class c, ilu_Object obj,
				   ILU_ERRS((marshal)) * err)
{
  ILU_C_Object   *lspo = ILU_NIL;
  ILU_C_Class	  cclass = ILU_NIL;
  ilu_Error lerr;
  ilu_Class kclass;

  kclass = ilu_ClassOfObject(obj);
  if (ilu_InstanceTrueForLangP(obj, MyLangIdx())) {
    ILU_NOTE(OBJECT_DEBUG,
	  ("ILU_C:  No true C object for kernel object %p"
	   ", of class \"%s\"!\n",
	   obj, c->cl_name));
    ilu_RegisterLSO(obj, kclass, ILU_NIL, MyLangIdx(), &lerr);
    ILU_HANDLED(lerr);
    ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_inst_nf, (void) 6);
    return ILU_NIL;
  }
  cclass = ilu_hash_FindInTable(TypeRegistry, c);
  if (cclass != ILU_NIL) {
    ilu_Server ksvr = ilu_ServerOfObject(obj);
    lspo = (ILU_C_Object *) ilu_MallocE(sizeof(ILU_C_Object), err);
    if (ILU_ERROK(*err)) {
      lspo->server = FindSurrogateServer(ksvr);
      lspo->server->ilucs_objcnt += 1;
      lspo->iluco_ko = obj;
      lspo->instanceData = 0;
      lspo->iluco_class = cclass;
      lspo->interruptH = ILU_NIL;
      lspo->iluco_refcnt = 1;
      lspo->iluco_kvi = ilu_VeryInterested(obj);
      if (ilu_RegisterLSO(obj, kclass, lspo, MyLangIdx(), err)) {
	ILU_NOTE(OBJECT_DEBUG,
		 ("ILU/C: creating LSO %p for surrogate kobj %p"
		  " (\"%s\" in \"%s\"); refcnt=1, kvi=%d.\n",
		  lspo, obj, ilu_IhOfObject(obj),
		  ilu_IDOfServer(lspo->server->ilucs_ks),
		  lspo->iluco_kvi));
	ILU_CLER(*err);
      } else {
	lspo->server->ilucs_objcnt -= 1;
	ilu_free(lspo);
	lspo = ILU_NIL;
      }
    } else {
      lspo = ILU_NIL;
    }
  } else {
    ilu_Error lerr;
    ILU_NOTE(OBJECT_DEBUG,
	     ("ILU_C:  attempt to create surrogate C instance of"
	      " kernel object %p, of class \"%s\","
	      " but C surrogate code is not loaded.\n",
	      obj, c->cl_name));
    ILU_ERR_CONS1(marshal, err, minor, ilu_mm_mst_unreg, (void) 6);
    ilu_RegisterLSO(obj, kclass, ILU_NIL, MyLangIdx(), &lerr);
    ILU_HANDLED(lerr);
  }
  return (lspo);
}

#ifdef ILU_GENERATE_SUBTYPES
 
static void
  _ILU_C_CreateCClassForPhonyKernelClass (ilu_Class, ilu_Error *);

typedef struct supertype_list_s {
  struct supertype_list_s *next;
  ilu_Class kclass;
} *supertype_list;

static ilu_boolean
  type_in_list (supertype_list list, ilu_Class c)
{
  supertype_list ptr;

  for (ptr = list;  ptr != ILU_NIL;  ptr = ptr->next)
    if (ptr->kclass == c)
      return ilu_TRUE;
  return ilu_FALSE;
}

static void
  add_type_to_list (supertype_list *list, ilu_Class c)
{
  supertype_list ptr;

  ptr = (supertype_list) ilu_malloc(sizeof(*ptr));
  ptr->next = *list;
  ptr->kclass = c;
  *list = ptr;
}

static void
  supertype_list_free (supertype_list list)
{
  if (list == ILU_NIL) return;
  supertype_list_free (list->next);
  ilu_free(list);
}

static int
  _ILU_C_SetupSupertypeMethodBlock (_ILU_C_DispatchTableSection *sections,
				    supertype_list *list,
				    ilu_Class kclass, ilu_Error *err)
{
  ILU_C_Class cclass;
  _ILU_C_DispatchTableSection *ptr;
  ilu_cardinal nsupertypes;
  ilu_Class *supertypes;
  unsigned int i, j;

  ilu_DataOfClass (kclass, ILU_NIL, ILU_NIL, ILU_NIL, ILU_NIL,
		   ILU_NIL, ILU_NIL, &nsupertypes,
		   &supertypes, ILU_NIL,
#ifdef ILU_HTTPNG_OBJECTS
		   ILU_NIL, ILU_NIL, ILU_NIL, ILU_NIL,
#endif
		   ILU_NIL);
  j = 0;
  for (i = 0;  i < nsupertypes;  i++) {
    if (type_in_list(*list, supertypes[i]))
      continue;
    cclass = ilu_hash_FindInTable (TypeRegistry, supertypes[i]);
    if ((cclass == ILU_NIL) && ilu_PhonyOfClass(supertypes[i])) {
      _ILU_C_CreateCClassForPhonyKernelClass (supertypes[i], err);
      if (ILU_ERRNOK(*err)) return -1;
      cclass = ilu_hash_FindInTable(TypeRegistry, supertypes[i]);
    };
    if (cclass == ILU_NIL) {
      ILU_ERR_CONS1(marshal, err, minor, ilu_mm_mst_unreg, (void) 6);
      return -1;
    }
    sections[j].ilucdts_introType = cclass->ilucc_sections[0].ilucdts_introType;
    sections[j].ilucdts_methods = cclass->ilucc_sections[0].ilucdts_methods;
    j += 1;
    if (ILU_ERRNOK(*err)) return -1;
    add_type_to_list(list, supertypes[i]);
    j += _ILU_C_SetupSupertypeMethodBlock (sections + j, list, supertypes[i], err);
    if (ILU_ERRNOK(*err)) return -1;
  }
  return j;
}

static unsigned int
  countSupertypes (ilu_Class kclass, supertype_list *list)
{
  ilu_cardinal nsupertypes;
  ilu_Class *supertypes;
  unsigned int i, count = 0;
  struct supertype_list *ptr;

  ilu_DataOfClass (kclass, ILU_NIL, ILU_NIL, ILU_NIL, ILU_NIL,
		   ILU_NIL, ILU_NIL, &nsupertypes,
		   &supertypes, ILU_NIL,
#ifdef ILU_HTTPNG_OBJECTS
		   ILU_NIL, ILU_NIL, ILU_NIL, ILU_NIL,
#endif
		   ILU_NIL);
  for (i = 0;  i < nsupertypes;  i++) {
    if (type_in_list (*list, supertypes[i]))
      continue;
    add_type_to_list (list, supertypes[i]);
    count += (1 + countSupertypes (supertypes[i], list));
  }
  return count;
}

static void
  _ILU_C_CreateCClassForPhonyKernelClass (ilu_Class kclass,
					  ilu_Error *err)
{
  ilu_cardinal nsupertypes;
  ILU_C_Class ans;
  int i;
  supertype_list list = ILU_NIL;

  list = ILU_NIL;
  nsupertypes = countSupertypes (kclass, &list);
  supertype_list_free(list);
  ans = ilu_MallocE(sizeof(*ans), err);
  if (ILU_ERRNOK(*err)) return;
  ans->ilucc_finalize = (ILU_C_FinalizationProc) 0;
  ans->ilucc_sections = ilu_MallocE((nsupertypes + 2) * sizeof(ans->ilucc_sections[0]), err);
  if (ILU_ERRNOK(*err)) goto free1;
  ans->ilucc_sections[0].ilucdts_introType = kclass;
  ans->ilucc_sections[0].ilucdts_methods = ILU_NIL;
  if (nsupertypes > 0) {
    list = ILU_NIL;
    i = _ILU_C_SetupSupertypeMethodBlock (ans->ilucc_sections + 1, &list, kclass, err);
    supertype_list_free(list);
    if (ILU_ERRNOK(*err)) goto free2;
    if (i != nsupertypes) {
      ILU_ERR_CONS1(internal, err, minor, 0, 0);
      return;
    };
  };
  ilu_hash_AddToTable (TypeRegistry, kclass, ans);
  return;

 free2:
  ilu_free(ans->ilucc_sections);
 free1:
  ilu_free(ans);
}

#endif /* def ILU_GENERATE_SUBTYPES */

/* Inside(obj->server, obj->class) */
static ILU_C_Object *
_ILU_C_CreateSurrogateFromRegistry(ilu_Class c, ilu_Object obj,
				   ILU_C_ENVIRONMENT * env)
{
  ILU_C_Class cclass;
  ILU_C_Object   *ans;
  ILU_ERRS((marshal)) lerr;
#ifdef ILU_GENERATE_SUBTYPES
  cclass = ilu_hash_FindInTable (TypeRegistry, c);
  if (cclass == ILU_NIL) {
    _ILU_C_CreateCClassForPhonyKernelClass(c, &lerr);
    if (ILU_ERROK(lerr)) {
    } else if (env) {
      _ILU_C_ConvertError(env, &lerr, CORBA_COMPLETED_NO);
      return ILU_NIL;
    } else {
      ILU_HANDLED(lerr);
      return ILU_NIL;
    }
  }
#endif /* def ILU_GENERATE_SUBTYPES */
  ans = _ILU_k_CreateSurrogateFromRegistry(c, obj, &lerr);
  if (ILU_ERRNOK(lerr)) {
    if (env)
      _ILU_C_ConvertError(env, &lerr, CORBA_COMPLETED_NO);
    else
      ILU_HANDLED(lerr);
    ans = ILU_NIL;
  }
  return ans;
}

ilu_string CORBA_TypeCode_id (CORBA_TypeCode tc, ILU_C_ENVIRONMENT *env)
{
  env->_major = ILU_C_NO_EXCEPTION;
  return (tc->type_id);
}

ilu_boolean CORBA_TypeCode_equal (CORBA_TypeCode tc1, CORBA_TypeCode tc2, ILU_C_ENVIRONMENT *env)
{
  env->_major = ILU_C_NO_EXCEPTION;
  return (strcmp(tc1->type_id, tc2->type_id) == 0);
}

void CORBA_TypeCode_free_value (CORBA_TypeCode tc, void *c_value, ILU_C_ENVIRONMENT *env)
{
  env->_major = ILU_C_NO_EXCEPTION;
  if (tc->type_kind == ilu_object_tk) {
  } else {
    if (tc->freeFn != ((ILU_C_FreeFn) 0)) {
      (*tc->freeFn)(c_value);
    };
    ilu_free(c_value);
  }
}

/*****  IoFns for predefined types *****/

#define OutputMacroC(fn, ktype, ctype) \
static void _ILU_C_Output ## fn (ilu_Call call, void *p, ilu_Error *e) \
{ \
    ilu_Output ## fn (call, (ktype) (*(ctype *)p), e); \
} \

#define OutputMacroS(fn, ktype, ctype) \
static void _ILU_C_Output ## fn (ilu_Call call, void *p, ilu_Error *e) \
{ \
    ilu_Output ## fn (call, (*(ktype *)p), e); \
} \

#define SizeOfMacroC(fn, ktype, ctype) \
static ilu_cardinal _ILU_C_SizeOf ## fn (ilu_Call call, void *p, ilu_Error *e) \
{ \
    return (ilu_SizeOf ## fn (call, (ktype) (*(ctype *)p), e)); \
} \

#define SizeOfMacroS(fn, ktype, ctype) \
static ilu_cardinal _ILU_C_SizeOf ## fn (ilu_Call call, void *p, ilu_Error *e) \
{ \
    return (ilu_SizeOf ## fn (call, (*(ktype *)p), e)); \
} \

#define InputMacro(fn, ktype, ctype) \
static void * _ILU_C_Input ## fn (ilu_Call call, void *p, ilu_Error * e) \
{ \
    ktype _temp1; ctype * _temp2; \
    if (p == ILU_NIL) { \
      _temp2 = (ctype *) ilu_MallocE(sizeof(ctype),e); \
      if (ILU_ERRNOK(*e)) return ILU_NIL; \
    } else { _temp2 = (ctype *) p; }; \
    ilu_Input ## fn (call, &_temp1, e); \
    if (ILU_ERROK(*e)) { *((ctype *) _temp2) = _temp1; return (void *) _temp2; }\
    else { if (((void *) _temp2) != p) ilu_free(_temp2); return ILU_NIL; } \
} \


#ifdef WIN32

/* workaround for ANSI C treating references imported from another dll as non-constant 
   and hence not usable as an initializer for a static */

#define RegisterIofnsMacro(fn, ktype, ctype) \
struct _ILU_C_IoFnsRegistration_s _ ## ctype ## __IoFns = { \
  ktype ## _tk, 0, \
  sizeof(ktype), ILU_NIL,\
  _ILU_C_SizeOf ## fn, _ILU_C_Output ## fn, _ILU_C_Input ## fn, 0 }; \

#define InitFnsRegistrationTypeIdMacro(ktype, ctype) \
	_ ## ctype ## __IoFns.type_id = (ilu_string) &ilu_TypeID_ ## ktype

#else

#define RegisterIofnsMacro(fn, ktype, ctype) \
struct _ILU_C_IoFnsRegistration_s _ ## ctype ## __IoFns = { \
  ktype ## _tk, (ilu_string) ilu_TypeID_ ## ktype, \
  { sizeof(ktype) }, ILU_NIL,\
  _ILU_C_SizeOf ## fn, _ILU_C_Output ## fn, _ILU_C_Input ## fn, 0 }; \

#endif

#define ScalarTypeMacro(fn, ktype, ctype)	OutputMacroC(fn,ktype,ctype) \
						SizeOfMacroC(fn,ktype,ctype) \
						InputMacro(fn,ktype,ctype) \
						RegisterIofnsMacro(fn,ktype,ctype) \

#define SameTypeMacro(fn, ktype, ctype)	OutputMacroS(fn,ktype,ctype) \
					SizeOfMacroS(fn,ktype,ctype) \
					InputMacro(fn,ktype,ctype) \
					RegisterIofnsMacro(fn,ktype,ctype) \

ScalarTypeMacro(Boolean,ilu_boolean,CORBA_boolean)

ScalarTypeMacro(Byte,ilu_byte,CORBA_octet)

ScalarTypeMacro(ShortCharacter,ilu_shortcharacter,CORBA_char)
ScalarTypeMacro(Character,ilu_character,CORBA_wchar)

ScalarTypeMacro(ShortCardinal,ilu_shortcardinal,CORBA_unsigned_short)
ScalarTypeMacro(Cardinal,ilu_cardinal,CORBA_unsigned_long)
SameTypeMacro(LongCardinal,ilu_longcardinal,CORBA_unsigned_long_long)

ScalarTypeMacro(ShortInteger,ilu_shortinteger,CORBA_short)
ScalarTypeMacro(Integer,ilu_integer,CORBA_long)
SameTypeMacro(LongInteger,ilu_longinteger,CORBA_long_long)

ScalarTypeMacro(ShortReal,ilu_shortreal,CORBA_float)
ScalarTypeMacro(Real,ilu_real,CORBA_double)
SameTypeMacro(LongReal,ilu_longreal,CORBA_long_double)

#if 0

static void
  _ILU_C_CString__Output (ilu_Call call,
			 ilu_string *string,
			 ilu_Error *err)
{
  ilu_OutputString(call, *string, _ILU_C_SafeStrlen(*string), 0, err);
}

static ilu_cardinal
  _ILU_C_CString__SizeOf (ilu_Call call,
			 ilu_string *string,
			 ilu_Error *err)
{
  return ilu_SizeOfString(call, *string, _ILU_C_SafeStrlen(*string), 0, err);
}

#endif

struct _ILU_C_IoFnsRegistration_s _CORBA_string__IoFns = {
	ilu_sequence_tk,
#ifdef WIN32
		/*  workaround for ANSI C treating references imported from another dll as non-constant 
                    and hence not usable as an initializer for a static  */
		0,
#else
		(ilu_string) &ilu_TypeID_ilu_CString[0],
#endif
		{ sizeof(ilu_string) },
		ILU_NIL,
		(ILU_C_SizeFn) _ilu_CString__SizeOf,
		(ILU_C_OutputFn) _ilu_CString__Output,
		(ILU_C_InputFn) _ilu_CString__Input,
		(ILU_C_FreeFn) ilu_CString__Free };

#ifdef ADD_VARIANT_SUPPORT

struct _ILU_C_IoFnsRegistration_s _CORBA_any__IoFns = {
	ilu_pickle_tk,
#ifdef WIN32
	/* workaround for ANSI C treating references imported from another dll as non-constant 
           and hence not usable as an initializer for a static */
	0,
#else
	(ilu_string) &ilu_TypeID_ilu_pickle[0],
#endif /* WIN32 */
	{ sizeof(CORBA_any) },
	ILU_NIL,
	(ILU_C_SizeFn) _CORBA_any__SizeOf,
	(ILU_C_OutputFn) _CORBA_any__Output,
	(ILU_C_InputFn) _CORBA_any__Input,
	(ILU_C_FreeFn) CORBA_any__Free };

#endif /* ADD_VARIANT_SUPPORT */

static HashTable _IoFns = ILU_NIL;

#define RegisterPrimitiveType(kname,cname)	\
  ilu_hash_AddToTable(_IoFns, (ilu_string) &ilu_TypeID_ ## kname [0], & _ ## cname ## __IoFns); \
  _ ## cname ## __IoFns.kernelType = ilu_FindTypeByUID((char *) ilu_TypeID_ ## kname , &lerr); \
  ILU_HANDLED(lerr);	\


void _ILU_C_RegisterIoFns (ILU_C_IoFnsRegistration r)
{
  if (_IoFns == ILU_NIL)
    {
      ilu_Error lerr = ILU_INIT_NO_ERR;

      _IoFns = ilu_hash_MakeNewTable (IOFNS_TABLE_SIZE,
				       ilu_hash_HashString,
				       ilu_hash_StringCompare);
      if (_IoFns == ILU_NIL)
	{
	  ilu_DebugPrintf("ILU/C:  can't create type I/O functions registration table!\n");
	  exit(1);
	}

      RegisterPrimitiveType(ilu_boolean, CORBA_boolean);

      RegisterPrimitiveType(ilu_byte, CORBA_octet);

      RegisterPrimitiveType(ilu_shortinteger, CORBA_short);
      RegisterPrimitiveType(ilu_integer, CORBA_long);
      RegisterPrimitiveType(ilu_longinteger, CORBA_long_long);

      RegisterPrimitiveType(ilu_shortcardinal, CORBA_unsigned_short);
      RegisterPrimitiveType(ilu_cardinal, CORBA_unsigned_long);
      RegisterPrimitiveType(ilu_longcardinal, CORBA_unsigned_long_long);

      RegisterPrimitiveType(ilu_shortcharacter, CORBA_char);
      RegisterPrimitiveType(ilu_character, CORBA_wchar);

      RegisterPrimitiveType(ilu_shortreal, CORBA_float);
      RegisterPrimitiveType(ilu_real, CORBA_double);
      RegisterPrimitiveType(ilu_longreal, CORBA_long_double);
      
      RegisterPrimitiveType(ilu_CString, CORBA_string);

#ifdef ADD_VARIANT_SUPPORT
      RegisterPrimitiveType(ilu_pickle, CORBA_any);
#endif /* ADD_VARIANT_SUPPORT */
    }
  if (ilu_hash_FindInTable(_IoFns, r->type_id) == ILU_NIL) {
    ilu_hash_AddToTable(_IoFns, r->type_id, r);
  };
}

ILU_C_IoFnsRegistration _ILU_C_LookupIoFns (ilu_string type_uid)
{
  if (_IoFns == ILU_NIL)
    return ILU_NIL;
  return (ILU_C_IoFnsRegistration) ilu_hash_FindInTable(_IoFns, type_uid);
}

static void
  _ILU_C_OutputValue (CORBA_TypeCode tc,
		      ilu_Call call,
		      void *value_ptr, /* <c_parameter_type(t, InOut)> */
		      ilu_Error *err)
{
  if (tc->type_kind != ilu_object_tk)
    (*tc->outFn)(call, value_ptr, err);
  else
    _ILU_C_OutputObject (call, value_ptr,
			 tc->properties.object_class,
			 ilu_FALSE, err);
}

static ilu_cardinal
  _ILU_C_SizeOfValue (CORBA_TypeCode tc,
		      ilu_Call call,
		      void *value_ptr, /* <c_parameter_type(t, InOut)> */
		      ilu_Error *err)
{
  if (tc->type_kind != ilu_object_tk)
    return ((*tc->sizeFn)(call, value_ptr, err));
  else
    return (_ILU_C_SizeOfObject (call, value_ptr,
				 tc->properties.object_class,
				 ilu_FALSE, err));
}

static void * /* <c_role_type(type, role_InpRet, ilu_FALSE)> */
  _ILU_C_InputValue (CORBA_TypeCode tc,
		     ilu_Call call,
		     void *value_ptr, /* <c_parameter_type(t, InOut)> */
		     ilu_Error *err)
{
  void *ptr;

  if (value_ptr != ILU_NIL)
    ptr = value_ptr;
  else if (tc->type_kind != ilu_object_tk) {
    ptr = ilu_MallocE (tc->properties.value_size, err);
    if (ILU_ERRNOK(*err)) return ILU_NIL;
    memset(ptr, 0, tc->properties.value_size);
  }
  if (tc->type_kind != ilu_object_tk)
    (*tc->inputFn)(call, ptr, err);
  else
    ptr = _ILU_C_InputObject (call, tc->properties.object_class, ilu_FALSE, err);
  return ptr;
}

void
  _ILU_C_CRFree (_ILU_C_CRInfo s, ilu_refany ref)
{
  if (s->cr_free_fn != NULLFN)
    (*s->cr_free_fn)(s->cr_typecode, ref);
}

void
  _ILU_C_CRPreOutput (_ILU_C_CRInfo s, ilu_refany ref, ilu_Error *err)
{
  if (s->cr_pre_output_fn != NULLFN)
    (*s->cr_pre_output_fn)(s->cr_typecode, ref, err);
}

void
  _ILU_C_CRPostOutput (_ILU_C_CRInfo s, ilu_refany ref, ilu_Error *err)
{
  if (s->cr_post_output_fn != NULLFN)
    (*s->cr_post_output_fn)(s->cr_typecode, ref, err);
}

void
  _ILU_C_CRPostInput (_ILU_C_CRInfo s, ilu_refany ref, ilu_Error *err)
{
  if (s->cr_post_input_fn != NULLFN)
    (*s->cr_post_input_fn)(s->cr_typecode, ref, err);
}

ilu_refany
  _ILU_C_CRCreate (_ILU_C_CRInfo s, ilu_cardinal size, ilu_Error *err)
{
  if (s->cr_create_fn != NULLFN)
    return (*s->cr_create_fn) (s->cr_typecode, size, err);
  return ILU_NIL;
}

static HashTable CustomRecords = ILU_NIL;

void
  ILU_C_RegisterCustomRecord (CORBA_TypeCode tc,
			      ILU_C_CRCreateFn cf,
			      ILU_C_CRFreeFn ff,
			      ILU_C_CRPreOutputFn preof,
			      ILU_C_CRPostOutputFn postof,
			      ILU_C_CRPostInputFn pif,
			      ILU_C_ENVIRONMENT *status)
{
  ILU_C_IoFnsRegistration iofns;
  _ILU_C_CRInfo s;

  if (tc->type_kind != ilu_record_tk) {
    ILU_C_RAISE_SYSTEM(status,BAD_PARAM,ilu_bpm_not_record_type,NO);
    return;
  };
  if (CustomRecords == ILU_NIL) {
    CustomRecords = ilu_hash_MakeNewTable (37, ilu_hash_HashPointer, ilu_hash_PointerCompare);
    if (CustomRecords == ILU_NIL) {
      ILU_C_RAISE_SYSTEM(status,INTERNAL,0,NO);
      return;
    };
  };
  if (ilu_hash_FindInTable(CustomRecords, (void *) tc)) {
    s = ilu_hash_RemoveFromTable(CustomRecords, (void *) tc);
    ilu_free(s);
  };
  if ((cf == NULLFN) &&
      (ff == NULLFN) &&
      (preof == NULLFN) &&
      (postof == NULLFN) &&
      (pif == NULLFN)) {
    ILU_C_SET_SUCCESSFUL(status);
    return;
  };
  if ((s = ilu_malloc(sizeof(*s))) == ILU_NIL) {
    ILU_C_RAISE_SYSTEM(status,NO_MEMORY,sizeof(*s),NO);
    return;
  };
  s->cr_typecode = tc;
  s->cr_create_fn = cf;
  s->cr_free_fn = ff;
  s->cr_pre_output_fn = preof;
  s->cr_post_output_fn = postof;
  s->cr_post_input_fn = pif;
  if (!ilu_hash_AddToTable (CustomRecords, (void *) tc, (void *) s)) {
    ilu_free(s);
    ILU_C_RAISE_SYSTEM(status,INTERNAL,0,NO);
    return;
  };
  iofns = (ILU_C_IoFnsRegistration) tc;
  iofns->properties.value_size |= 0x80000000;
  ILU_C_SET_SUCCESSFUL(status);
  return;
}

_ILU_C_CRInfo
  _ILU_C_GetCRInfo (CORBA_TypeCode tc)
{
  if (CustomRecords == ILU_NIL)
    return ILU_NIL;
  return ((_ILU_C_CRInfo) ilu_hash_FindInTable(CustomRecords, (void *) tc));
}

/* L1, L2 unconstrained */

ilu_CString ILU_C_ClassName (ILU_C_Object *o)
{
  ilu_Class c = ILU_C_ClassRecordOfInstance (o);
  if (o == ILU_NIL)
    return (ILU_NIL);
  else
    return (c->cl_name);
}

ilu_CString ILU_C_ClassID (ILU_C_Object *o)
{
  ilu_Class c = ILU_C_ClassRecordOfInstance (o);
  if (o == ILU_NIL)
    return (ILU_NIL);
  else
    return (c->cl_unique_id);
}

ilu_Class ILU_C_FindILUClassByTypeName (ilu_CString typename)
{
  return (ilu_FindClassFromName(typename));
}

ilu_Class ILU_C_FindILUClassByTypeID (ilu_CString typeID)
{
  return (ilu_FindClassFromID(typeID));
}

ilu_Class ILU_C_ClassRecordOfInstance(ILU_C_Object *o)
{
  if (o == ILU_NIL || o->iluco_class == ILU_NIL)
    return (ILU_NIL);
  return (MOST_SPECIFIC_ILU_CLASS(o));
}

_ILU_C_Method 
_ILU_C_FindMethod(ILU_C_Object * h, ilu_Class cls, int ndx)
{
  _ILU_C_DispatchTableSection *section;

  section = h->iluco_class->ilucc_sections;
  for (section = h->iluco_class->ilucc_sections;
       section->ilucdts_introType;
       section++) {
    if (section->ilucdts_introType == cls)
      return section->ilucdts_methods[ndx];
  }
  return ((_ILU_C_Method) 0);
}
 
/* Main Invariant holds */

ilu_CString ILU_C_SBHOfObject (ILU_C_Object *obj)
{
  ilu_CString     sbh = ILU_NIL;

  ilu_Object      kobj = _ILU_C_KernelObjOfObj(obj);
  if (kobj != ILU_NIL) {
    sbh = ilu_SBHOfObject(kobj);
    ilu_ExitServer(obj->server->ilucs_ks, MOST_SPECIFIC_ILU_CLASS(obj));
  }
  return (sbh);
}

ilu_boolean ILU_C_IDOfObject (ILU_C_Object *obj, char **sid, char **ih)
{
  ilu_boolean result = ilu_FALSE;
  ilu_Object kobj = _ILU_C_KernelObjOfObj (obj);
  ilu_Error lerr;

  if (kobj != ILU_NIL)
    {
      *sid = ilu_StrdupE(ilu_IDOfServer(obj->server->ilucs_ks), &lerr);
      if (ILU_ERRNOK(lerr)) {
	ILU_HANDLED(lerr);
      } else {
	*ih = ilu_StrdupE(ilu_IhOfObject(kobj), &lerr);
	if (ILU_ERRNOK(lerr)) {
	  ILU_HANDLED(lerr);
	  ilu_free(*sid);
	} else {
	  result = ilu_TRUE;
	}
      }
      ilu_ExitServer (obj->server->ilucs_ks, MOST_SPECIFIC_ILU_CLASS(obj));
    }
  return (result);
}

#ifdef IIOP_PROTOCOL

ilu_CString ILU_C_IOROfObject (ILU_C_Object *obj)
{
  ilu_Error       err;
  ilu_CString     sbh = ILU_NIL;

  ilu_Object      kobj = _ILU_C_KernelObjOfObj(obj);
  sbh = ilu_IOROfObject(kobj, &err);
  if (ILU_ERRNOK(err)) {
    DisposeErr(&err, "creating IOR (in ILU_C_IOROfObject)");
    sbh = ILU_NIL;
  }
  if (kobj != ILU_NIL)
    ilu_ExitServer(obj->server->ilucs_ks, MOST_SPECIFIC_ILU_CLASS(obj));
  return (sbh);
}

#endif

ilu_CString
ILU_C_FormSBH (ilu_CString server_id,
	       ilu_CString instance_handle,
	       ilu_Class most_specific_type,
	       ilu_ProtocolInfo pinfo,
	       ilu_TransportInfo tinfo,
	       ILU_C_ENVIRONMENT *env)
{
  ilu_Error lerr;
  ilu_string sbh;

  sbh = ilu_FormSBH (server_id, instance_handle,
		     most_specific_type->cl_unique_id,
		     pinfo, tinfo, &lerr);
  if (ILU_ERRNOK(lerr)) {
    _ILU_C_ConvertError(env, &lerr, CORBA_COMPLETED_NO);
    return ILU_NIL;
  } else {
    env->_major = ILU_C_NO_EXCEPTION;
    env->returnCode = ILU_NIL;
    return sbh;
  }
}

ILU_C_Object   *
ILU_C_SBHToObject(ilu_CString sbh,
		  ilu_Class static_type,
		  ILU_C_ENVIRONMENT * env)
{
  ILU_C_Object   *h = ILU_NIL;
  ilu_Object      obj;
  ilu_Server	server;
  ILU_ERRS((bad_locks, broken_locks, inv_objref,
	    no_memory, internal)) lerr;
  env->_major = ILU_C_NO_EXCEPTION;
  obj = ilu_ObjectOfSBH(sbh, static_type, &lerr);
  if (ILU_ERRNOK(lerr))
    _ILU_C_ConvertError(env, &lerr, CORBA_COMPLETED_NO);
  if (obj == ILU_NIL)
    return (ILU_NIL);
  server = ilu_ServerOfObject(obj);
  h = ilu_GetLanguageSpecificObject(obj, MyLangIdx());
  if (h != ILU_NIL)
    IncrRefcnt(h);
  else
    h = _ILU_C_CreateSurrogateFromRegistry(ilu_ClassOfObject(obj), obj,
					   env);
  ilu_ExitServer(server, static_type);
  return (h);
}

/*Main Invariant holds, L2 not further constrained*/
ILU_C_Server
ILU_C_ServerOfObject(ILU_C_OBJECT obj,
		     ILU_C_ENVIRONMENT * env)
{
  if (!obj) {
    ILU_C_RAISE_SYSTEM(env, BAD_PARAM, 0, NO);
    return ILU_NIL;
  }
  return ILU_C_Server_duplicate(obj->server, env);
}

void
ILU_C_PingObject(ILU_C_Object * obj, ILU_C_ENVIRONMENT * env)
{
  ilu_Object      kobj = _ILU_C_KernelObjOfObj(obj);
  ilu_Server      ksvr = obj->server->ilucs_ks;
  ilu_Class       mst;
  ILU_ERRS((GcRegFailed, bad_locks, broken_locks,
	    internal))	err = ILU_INIT_NO_ERR;
  ilu_boolean     ok;
  ilu_Connection  newconn;
  if (kobj == ILU_NIL) {
    ILU_C_RAISE_SYSTEM(env, BAD_PARAM, ilu_bpm_closed, NO);
    return;
  }
  mst = ilu_ClassOfObject(kobj);
  err = ilu_DeltaHolds(kobj, 1);
  ilu_ExitServer(ksvr, mst);
  ILU_ERR_SWITCH(err) {
    ILU_SUCCESS_CASE;
    ILU_ERR_CASE(GcRegFailed, v) {
      ILU_C_RAISE_SYSTEM(env, COMM_FAILURE, ilu_cfm_gcRegFailed, NO);
      return;
    }
    ILU_ERR_ELSE {
      ILU_C_RAISE_SYSTEM(env, INTERNAL, 0, NO);
      return;
    }
  } ILU_ERR_ENDSWITCH;
  ok = ilu_PingObject(kobj, &newconn);
  if (newconn != ILU_NIL)
    (*Fork) (MonitorOutgoingConnection, newconn);
  ilu_EnterServer(ksvr, mst);
  err = ilu_DeltaHolds(kobj, -1);
  ilu_ExitServer(ksvr, mst);
  ILU_ERR_SWITCH(err) {
    ILU_SUCCESS_CASE;
    ILU_ERR_CASE(GcRegFailed, v) {
      ILU_C_RAISE_SYSTEM(env, COMM_FAILURE, ilu_cfm_gcRegFailed, MAYBE);
      return;
    }
    ILU_ERR_ELSE {
      ILU_C_RAISE_SYSTEM(env, INTERNAL, 0, MAYBE);
      return;
    }
  } ILU_ERR_ENDSWITCH;
  if (ok)
    env->_major = ILU_C_NO_EXCEPTION;
  else
    ILU_C_RAISE_SYSTEM(env, COMM_FAILURE, ilu_cfm_pingFailed, MAYBE);
  return;
}

ILU_RUNTIME_PUBLIC ILU_C_Object *
  ILU_C_FindObject (char *sid, char *ih)
{
  ilu_Object kobj;
  ILU_C_Object *cobj = ILU_NIL;
  kobj = ilu_FindObject (sid, ih);
  if (kobj != ILU_NIL) {
    cobj = (ILU_C_Object *) ilu_GetLanguageSpecificObject(kobj, _ILU_C_LanguageIndex);
    ilu_ExitServer (ilu_ServerOfObject(kobj), ilu_rootClass);
  };
  return cobj;
}

/***********************************************************************
 *  This is support for ilu.CORBA-Object, the standard base object for
 *  all ILU objects defined in OMG IDL.
 ***********************************************************************/

ilu_Class _ilu_CORBA_Object__ILUType = NULL;

struct _ILU_C_IoFnsRegistration_s _ilu_CORBA_Object__IoFns = { ilu_object_tk, "IDL:omg.org/CORBA/Object:1.0", { 0 }, 0, 0, 0, 0 };

void ilu_CORBA_Object__Free (ilu_CORBA_Object* _val)
{
  /* What you put in the freeRoutine member of a CORBA_Environment for an exception parameterized by a ilu_CORBA_Object */
  /* frees allocated storage inside _val (if any), but does not free(_val) */
  CORBA_Environment env = {0};
  CORBA_Object_release(*_val, &env);
  *_val = 0;
}

static _ILU_C_DispatchTableSection _ilu_CORBA_Object__SurrogateDispatchTable[2] = {
  {ILU_NIL /* will be set to _ilu_CORBA_Object__ILUType */,
  ILU_NIL /* no methods introduced here */},
  {ILU_NIL, ILU_NIL} /* terminating sentinal */
  };

static _ILU_C_Class_struct _ilu_CORBA_Object__SurrogateClass = {
  _ilu_CORBA_Object__SurrogateDispatchTable,
  0	/* no finalization */
};

ilu_CORBA_Object ilu_CORBA_Object__CreateFromSBH (char *sbh, ILU_C_ENVIRONMENT * env)
{
  return (ilu_CORBA_Object) (ILU_C_SBHToObject(sbh, _ilu_CORBA_Object__ILUType, env));
}

static _ILU_C_DispatchTableSection _ilu_CORBA_Object__DefaultDispatchTable[2] = {
  {ILU_NIL /* will be set to _ilu_CORBA_Object__ILUType */,
  ILU_NIL /* no methods introduced here */},
  {ILU_NIL, ILU_NIL} /* terminating sentinal */
  };

static _ILU_C_Class_struct _ilu_CORBA_Object__DefaultClass = {
  _ilu_CORBA_Object__DefaultDispatchTable,
  0	/* no finalization, by default */
};

ILU_C_OBJECT ilu_CORBA_Object__CreateTrue  (ilu_string instance_handle, ILU_C_Server server, void * data)
{
  return (ILU_C_CreateTrueObject (&_ilu_CORBA_Object__DefaultClass, instance_handle, server, data, ilu_FALSE));
}

ILU_C_OBJECT ilu_CORBA_Object__OTCreateTrue  (ilu_string instance_handle, ILU_C_Server server, void * data)
{
  if (server == ILU_NIL || instance_handle == ILU_NIL) return ILU_NIL;
  return (ILU_C_CreateTrueObject (&_ilu_CORBA_Object__DefaultClass, instance_handle, server, data, ilu_TRUE));
}

static void init_CORBA_Object(void)
{
  ilu_Class cl;
  ilu_Error lerr;

  cl = ilu_DefineObjectType("ilu.CORBA-Object",
			    ILU_NIL,
			    (char *) ilu_TypeID_ilu_CORBA_Object,
			    ILU_NIL,	/* singleton */
			    ilu_TRUE,	/* optional */
			    ilu_FALSE,	/* collectible */
			    ILU_NIL,	/* authentication */
			    0,		/* method count */
			    0,		/* superclass count */
			    ILU_NIL,	/* superclasses */
#ifdef ILU_HTTPNG_OBJECTS
			    0, ilu_FALSE, ilu_FALSE,
#endif
			    &lerr);
  if (ILU_ERRNOK(lerr))
    goto fail2;
  _ilu_CORBA_Object__ILUType = cl;

  /* surrogate support */
  _ilu_CORBA_Object__SurrogateDispatchTable[0].ilucdts_introType = cl;
  _ILU_C_RegisterSurrogateCType (cl, &_ilu_CORBA_Object__SurrogateClass);

  /* true support */
  _ilu_CORBA_Object__DefaultDispatchTable[0].ilucdts_introType = cl;

  _ilu_CORBA_Object__IoFns.properties.object_class = cl;
  _ILU_C_RegisterIoFns (&_ilu_CORBA_Object__IoFns);

fail2:
  ILU_MUST_BE_SUCCESS(lerr);
}

static ILU_C_ErrableForkProc errableFork = 0;

static void ErrlessFork(ILU_C_WorkProc threadproc, void *threadarg)
{
  ILU_ERRS((no_memory, no_resources, internal)) err;
  (void) ((*errableFork) (threadproc, threadarg, &err));
  ILU_MUST_BE_SUCCESS(err);
}

void _ILU_C_InitializeCRuntime(void)
{
  static ilu_boolean initted = ilu_FALSE;
  int failure_action;
  char *v;
  if (!initted) {
    _ILU_C_LanguageIndex = MyLangIdx();
    ilu_SetNoter(C_Noter, _ILU_C_LanguageIndex);

    if (!threadedSet && (threaded = ilu_KernelThreaded())) {
      errableFork = ilu_Fork;
      Fork = ErrlessFork;
      threadedSet = ilu_TRUE;
      threadedOther = ilu_TRUE;
    }

#ifdef WIN32
    /*
     * workaround for ANSI C treating references imported from another dll as non-constant 
     * and hence not usable as an initializer for a static
     */
    InitFnsRegistrationTypeIdMacro(ilu_boolean, CORBA_boolean);
    InitFnsRegistrationTypeIdMacro(ilu_byte, CORBA_octet);
    InitFnsRegistrationTypeIdMacro(ilu_shortcharacter, CORBA_char);
    InitFnsRegistrationTypeIdMacro(ilu_character, CORBA_wchar);
    InitFnsRegistrationTypeIdMacro(ilu_shortcardinal, CORBA_unsigned_short);
    InitFnsRegistrationTypeIdMacro(ilu_cardinal, CORBA_unsigned_long);
    InitFnsRegistrationTypeIdMacro(ilu_longcardinal, CORBA_unsigned_long_long);
    InitFnsRegistrationTypeIdMacro(ilu_shortinteger, CORBA_short);
    InitFnsRegistrationTypeIdMacro(ilu_integer, CORBA_long);
    InitFnsRegistrationTypeIdMacro(ilu_longinteger, CORBA_long_long);
    InitFnsRegistrationTypeIdMacro(ilu_shortreal, CORBA_float);
    InitFnsRegistrationTypeIdMacro(ilu_real, CORBA_double);
    InitFnsRegistrationTypeIdMacro(ilu_longreal, CORBA_long_double);
    InitFnsRegistrationTypeIdMacro(ilu_CString, CORBA_string);
#ifdef ADD_VARIANT_SUPPORT
    InitFnsRegistrationTypeIdMacro(ilu_pickle, CORBA_any);
#endif /* ADD_VARIANT_SUPPORT */
#endif /* def WIN32 */
    InitializeTypeRegistry();

    initted = ilu_TRUE;

    init_CORBA_Object();

    if ((v = getenv("ILU_ASSERTION_FAILURE_ACTION")) != ILU_NIL) {
      failure_action = atoi(v);
      ilu_SetAssertionFailureAction(failure_action);
    }
    if ((v = getenv("ILU_MEMORY_FAILURE_ACTION")) != ILU_NIL) {
      failure_action = atoi(v);
      ilu_SetMemFailureAction(failure_action);
    }
    if ((v = getenv("ILU_CHECK_FAILURE_ACTION")) != ILU_NIL) {
      failure_action = atoi(v);
      ilu_SetCheckFailureAction(failure_action);
    }

  };
}

/***********************************************************************
 *  End of support for ilu.CORBA-Object.
 ***********************************************************************/

typedef struct {
  ilu_cardinal    nCObjs;
  ILU_C_ObjCounts counts;
  ilu_boolean     andObjects;
}               DestructCounters;

/* Inside(kobj's server, ilu_rootClass) */
static int CloseIt(ilu_Object kobj, ilu_refany rock)
{
  DestructCounters *dc = (DestructCounters *) rock;
  ilu_cardinal    mli = MyLangIdx();
  ILU_C_OBJECT    cobj = ilu_GetLanguageSpecificObject(kobj, mli);
  int             refthresh = (dc->andObjects != 0);
  if (!cobj)
    return 0;
  dc->nCObjs++;
  _ilu_Assert(cobj->iluco_refcnt || cobj->iluco_kvi,
	      "ilu.c:CloseIt 1");
  if (cobj->iluco_kvi) {
    if (cobj->iluco_refcnt > refthresh)
      dc->counts.nBoth++;
    else
      dc->counts.nVI++;
  } else {
    if (cobj->iluco_refcnt > refthresh)
      dc->counts.nRefd++;
  }
  if (dc->andObjects) {
    ILU_C_ENVIRONMENT env;
    InnerShutdownObject(cobj, ilu_TRUE, &env);
    CORBA_exception_free(&env);
  }
  return 0;
}

/* Main Invariant holds */

void
ILU_C_CloseServer(ILU_C_Server s, ilu_boolean andObjects,
		  ilu_cardinal * nCObjs, ILU_C_ObjCounts * counts,
		  ilu_cardinal * nConns, ILU_C_ENVIRONMENT * env)
{
  int             res;
  DestructCounters dc = {0};
  _ilu_Assert(s->ilucs_refcnt || s->ilucs_objcnt,
	      "ILU_C_CloseServer server counts");
  dc.andObjects = andObjects;
  res = ilu_BankAndScanServer(s->ilucs_ks, CloseIt, &dc, nConns);
  if (nCObjs)
    *nCObjs = dc.nCObjs;
  if (counts)
    *counts = dc.counts;
  env->_major = ILU_C_NO_EXCEPTION;
  return;
}

void
ILU_C_ShutdownObjectAndCloseServer(ILU_C_OBJECT obj,
				  ILU_C_ENVIRONMENT * env)
{
  ILU_C_Server    s = obj->server;
  s = ILU_C_Server_duplicate(s, env);
  if (!ILU_C_SUCCESSFUL(env))
    return;
  ILU_C_ShutdownObject(obj, env);
  if (!ILU_C_SUCCESSFUL(env))
    return;
  ILU_C_CloseServer(s, ilu_FALSE, ILU_NIL, ILU_NIL, ILU_NIL, env);
  if (!ILU_C_SUCCESSFUL(env))
    return;
  ILU_C_Server_release(s, env);
  return;
}

ilu_boolean
ILU_C_ValidateOrCloseObjSvr(ILU_C_OBJECT obj,
			      ILU_C_ENVIRONMENT * env)
{
  ILU_C_PingObject(obj, env);
  switch (env->_major) {
  case ILU_C_NO_EXCEPTION:
    return ilu_TRUE;
  case ILU_C_SYSTEM_EXCEPTION:
    if (ILU_C_EXCEPTION_ID(env) != ILU_C_STDEX(COMM_FAILURE))
      return ilu_FALSE;
    break;
  default:			/* can't happen */
    ILU_C_RAISE_SYSTEM(env, INTERNAL, 0, MAYBE);
    return ilu_FALSE;
  }
  ILU_C_EXCEPTION_FREE(env);
  ILU_C_ShutdownObjectAndCloseServer(obj, env);
  return ilu_FALSE;
}

typedef struct CallCons *CallList;
struct CallCons {
  /* L1, L2 unconstrained */

  ilu_Call        head;
  CallList        tail;
};

struct ILU_C_InterruptHandle_s {
  /* L1, L2 unconstrained */
  CallList        calls;
};

/* L1, L2 unconstrained */

static ilu_boolean AddCallToIH(ilu_Call call, ILU_C_InterruptHandle h)
{
  CallList        this = (CallList) ilu_malloc(sizeof(*this));
  if (this == ILU_NIL)
    return ilu_FALSE;
  this->head = call;
  this->tail = h->calls;
  h->calls = this;
  return ilu_TRUE;
}

static void RemCallFromIH(ilu_Call call, ILU_C_InterruptHandle h)
{
  CallList       *pcur;
  for (pcur = &(h->calls); *pcur != ILU_NIL; pcur = &((*pcur)->tail)) {
    if ((*pcur)->head == call) {
      CallList        doomed = *pcur;
      *pcur = doomed->tail;
      ilu_free(doomed);
      return;
    }
  }
}

/* Main Invariant holds */

ILU_C_InterruptHandle ILU_C_NewInterruptHandle(void)
{
  ILU_C_InterruptHandle h;
  h = (ILU_C_InterruptHandle) ilu_malloc(sizeof(*h));
  if (h != ILU_NIL)
    h->calls = ILU_NIL;
  return h;
}

void
ILU_C_SetObjectInterruptHandle(ILU_C_Object * obj,
			       ILU_C_InterruptHandle h)
{
  if (obj != ILU_NIL)
    obj->interruptH = h;
}

void ILU_C_InterruptHandleCalls(ILU_C_InterruptHandle h)
{
  CallList        cl;
  ilu_Error       lerr;
  for (cl = h->calls; cl != ILU_NIL; cl = cl->tail) {
    ilu_InterruptCall(cl->head, &lerr);
    ILU_HANDLED(lerr);
  }
}

ilu_CString ILU_C_PublishObject (ILU_C_Object *obj)
{
  ilu_Object kobj = _ILU_C_KernelObjOfObj (obj);
  if (kobj != ILU_NIL)
    return(ilu_PublishObject (kobj));
  else
    return (ILU_NIL);
}

/*before: not Inside (cobj->server, cl);
  after:				 L1 disjoint {cmu, s};
  after: cl collectible		      => L1  not >=  {gcmu};
  after: cl collectible & s surrogate => Main Invariant holds;
  where s = obj's server and cl = cobj->type->c
  (We don't really need to hold cmu for surrogate or non-collectible
   objects, but this is convenient because ilu_Enter/ExitServer can
   be used.)*/
ilu_boolean ILU_C_WithdrawObject (ILU_C_Object *obj, ilu_CString proof)
{
  ilu_Object kobj = _ILU_C_KernelObjOfObj (obj);
  if (kobj != ILU_NIL)
    return (ilu_WithdrawObject (kobj, proof));
  else
    return (ilu_FALSE);
}

ILU_OPTIONAL(ILU_GLOBAL(ILU_C_Object *))
ILU_C_LookupObject(ILU_RETAIN(char *) sid, ILU_RETAIN(char *) ih,
		   ilu_Class static_type)
{
  int             change;
  return ILU_C_ReLookupObject(sid, ih, static_type, &change);
}

ILU_OPTIONAL(ILU_GLOBAL(ILU_C_Object *))
ILU_C_ReLookupObject(ILU_RETAIN(char *) sid, ILU_RETAIN(char *) ih,
		     ilu_Class static_type, int *change)
{
  ILU_C_Object   *h = ILU_NIL;
  ilu_Object      obj;
  ilu_Server	  server;

  *change = ilu_ReLookupObject(sid, ih, static_type, &obj);
  if (obj == ILU_NIL)
    return (ILU_NIL);
  server = ilu_ServerOfObject(obj);
  h = ilu_GetLanguageSpecificObject(obj, MyLangIdx());
  if (h == ILU_NIL)
    h = _ILU_C_CreateSurrogateFromRegistry(ilu_ClassOfObject(obj), obj,
					   ILU_NIL);
  else
    IncrRefcnt(h);
  ilu_ExitServer(server, static_type);
  return (h);
}

/*L1, L2 unconstrained*/

ilu_Passport
  ILU_C_CreatePassport (ILU_C_ENVIRONMENT *env)
{
  ilu_Error err;
  ilu_Passport pp;
  pp = ilu_CreatePassport (ILU_NIL, &err);
  if (ILU_ERROK(err)) {
    ILU_C_SET_SUCCESSFUL(env);
    return pp;
  } else {
    _ILU_C_ConvertError(env, &err, CORBA_COMPLETED_NO);
    return ILU_NIL;
  }
}

#ifdef SUNRPC_PROTOCOL

ilu_IdentityInfo
  ILU_C_AcquireSunRPCAuthUnixIdentity (ilu_string hostname,
				       ilu_shortcardinal uid,
				       ilu_shortcardinal gid,
				       ilu_shortcardinal ngroups,
				       ilu_shortcardinal *groups,
				       ILU_C_ENVIRONMENT *env)
{
  ilu_SunRPCAuthUnixIdentityInfo sraui;
  ilu_IdentityInfo ii;
  ilu_Error err;
  ilu_shortcardinal i;

  ii = (ilu_IdentityInfo) ilu_MallocE(sizeof(*ii), &err);
  if (ILU_ERRNOK(err)) { _ILU_C_ConvertError(env, &err, CORBA_COMPLETED_NO); return ILU_NIL; };
  sraui = (ilu_SunRPCAuthUnixIdentityInfo) ilu_MallocE(sizeof(*sraui), &err);
  if (ILU_ERRNOK(err)) { ilu_free(ii); _ILU_C_ConvertError(env, &err, CORBA_COMPLETED_NO); return ILU_NIL; };
  ii->ii_type = ilu_SunRPCAuthUnixIdentity;
  ii->ii_owned_by_passport = ilu_FALSE;
  ii->ii_info = sraui;
  sraui->ii_UID = uid;
  sraui->ii_GID = gid;
  sraui->ii_hostname = ilu_StrdupE(hostname, &err);
  if (ILU_ERRNOK(err)) { ilu_free(ii); ilu_free(sraui); _ILU_C_ConvertError(env, &err, CORBA_COMPLETED_NO); return ILU_NIL; };
  sraui->ii_gids = (ilu_shortcardinal *) ilu_MallocE(sizeof(ilu_shortcardinal)*ngroups, &err);
  if (ILU_ERRNOK(err)) {
    ilu_free(sraui->ii_hostname);
    ilu_free(ii); ilu_free(sraui);
    _ILU_C_ConvertError(env, &err, CORBA_COMPLETED_NO);
    return ILU_NIL;
  };
  sraui->ii_ngids = ngroups;
  for (i = 0;  i < ngroups;  i++) {
    sraui->ii_gids[i] = groups[i];
  };
  ILU_C_SET_SUCCESSFUL(env);
  return ii;
}

#endif /* def SUNRPC_PROTOCOL */

#ifdef W3MUX_TRANSPORT

ilu_IdentityInfo
  ILU_C_AcquireW3muxEndpointIdentity (ilu_string uuid,
				      ILU_C_ENVIRONMENT *env)
{
  ilu_IdentityInfo info;
  ilu_Error lerr;

  info = ilu_AcquireW3muxEndpointIndentity (uuid, &lerr);
  if (ILU_ERRNOK(lerr)) {
    _ILU_C_ConvertError(env, &lerr, CORBA_COMPLETED_NO);
    return ILU_NIL;
  } else {
    ILU_C_SET_SUCCESSFUL(env);
    return info;
  }
}

#endif /* def W3MUX_TRANSPORT */

ILU_PASS(ilu_IdentityInfo)
ILU_C_CopyIdentity (ILU_RETAIN(const struct _ilu_IdentityInfo_s *) info,
		    ILU_C_ENVIRONMENT *env)
{
  ilu_IdentityInfo i;
  ilu_Error kerr;
  i = ilu_CopyIdentity(info, &kerr);
  if (ILU_ERRNOK(kerr)) {
    _ILU_C_ConvertError (env, &kerr, CORBA_COMPLETED_NO);
    return ILU_NIL;
  } else {
    ILU_C_SET_SUCCESSFUL(env);
    return i;
  }
}

ilu_boolean
ILU_C_FreeIdentity (ILU_PASS(struct _ilu_IdentityInfo_s *) info,
		    ILU_C_ENVIRONMENT *env)
{
  ilu_Error kerr;
  if (!ilu_FreeIdentity (info, &kerr)) {
    _ILU_C_ConvertError (env, &kerr, CORBA_COMPLETED_NO);
    return ilu_FALSE;
  } else {
    ILU_C_SET_SUCCESSFUL(env);
    return ilu_TRUE;
  }
}

ilu_cardinal
ILU_C_DisplayIdentity (ILU_PASS(struct _ilu_IdentityInfo_s *) info,
		       char *buffer, ilu_cardinal bufferlen,		       
		       ILU_C_ENVIRONMENT *env)
{
  ilu_cardinal val;
  ilu_Error kerr;

  if ((val = ilu_DisplayIdentity (info, buffer, bufferlen, &kerr)), ILU_ERRNOK(kerr)) {
    _ILU_C_ConvertError (env, &kerr, CORBA_COMPLETED_NO);
    return 0;
  } else {
    ILU_C_SET_SUCCESSFUL(env);
    return val;
  }
}

ilu_boolean
ILU_C_AddIdentity (ILU_RETAIN(ilu_Passport) pp,
		   ILU_PASS(const struct _ilu_IdentityInfo_s *) info,
		   ILU_C_ENVIRONMENT *env)
{
  ilu_Error kerr;
  if (!ilu_AddIdentity (pp, info, &kerr)) {
    _ILU_C_ConvertError (env, &kerr, CORBA_COMPLETED_NO);
    return ilu_FALSE;
  } else {
    ILU_C_SET_SUCCESSFUL(env);
    return ilu_TRUE;
  }
}

ILU_OPTIONAL(ILU_RETAIN(ilu_IdentityInfo))
ILU_C_FindIdentity (ILU_RETAIN(ilu_Passport) pp,
		    ilu_IdentityType type)
{
  return ilu_FindIdentity (pp, type);
}

ilu_boolean
ILU_C_DestroyPassport (ILU_PASS(ilu_Passport) pp,
		       ILU_C_ENVIRONMENT *env)
{
  ilu_Error kerr;
  if (!ilu_DestroyPassport(pp, &kerr)) {
    _ILU_C_ConvertError(env, &kerr, CORBA_COMPLETED_NO);
    return ilu_FALSE;
  } else {
    ILU_C_SET_SUCCESSFUL(env);
    return ilu_TRUE;
  }
}

#ifdef SECURE_TRANSPORT

ilu_IdentityInfo
ILU_C_AcquireGSSIdentity (gss_cred_id_t cred, ILU_C_ENVIRONMENT *env)
{
  ilu_Error kerr;
  ilu_IdentityInfo info;
  info = ilu_AcquireGSSIdentity (cred, &kerr);
  if (ILU_ERRNOK(kerr)) {
    _ILU_C_ConvertError(env, &kerr, CORBA_COMPLETED_NO);
    return ILU_NIL;    
  } else {
    ILU_C_SET_SUCCESSFUL(env);
    return info;
  }
}

ilu_boolean
ILU_C_DecodeGSSIdentity (ilu_IdentityInfo info,	/* input; retain; info to decode */
			 gss_name_t * name,	/* output; name in identity */
			 ilu_FineTime *till,	/* output; good-till; seconds past Unix epoch */
			 gss_OID mech,		/* input; actual mechanism desired; optional */
			 ilu_boolean *local,	/* if TRUE, local; otherwise remote */
			 ilu_cardinal *flags,	/* connection flags, as in gss_inquire_context */
			 ILU_C_ENVIRONMENT *env)
{
  ilu_Error kerr;
  ilu_DecodeGSSIdentity (info, name, till, mech, local, flags, &kerr);
  if (ILU_ERRNOK(kerr)) {
    _ILU_C_ConvertError(env, &kerr, CORBA_COMPLETED_NO);
    return ilu_FALSE;
  } else {
    ILU_C_SET_SUCCESSFUL(env);
    return ilu_TRUE;
  }
}

gss_cred_id_t
ILU_C_AcquireGSSCredForName (char *name,		/* name */
			     ilu_cardinal lifetime,	/* lifetime */
			     gss_OID mech,		/* secmech */
			     ilu_boolean accept_only,	/* accept_only */
			     ILU_C_ENVIRONMENT *env	/* err */)
{
  ilu_Error kerr;
  gss_cred_id_t cred;
  cred = ilu_AcquireGSSCredForName (name, lifetime, mech, accept_only, &kerr);
  if (ILU_ERRNOK(kerr)) {
    _ILU_C_ConvertError(env, &kerr, CORBA_COMPLETED_NO);
    return ILU_NIL;
  } else {
    ILU_C_SET_SUCCESSFUL(env);
    return cred;
  }
}

ILU_PASS(ilu_string)
ILU_C_GSSNameToString (gss_name_t name,
		       ILU_C_ENVIRONMENT *env)
{
  ilu_Error kerr;
  ilu_string ret;
  ret = ilu_GSSNameToString (name, &kerr);
  if (ILU_ERRNOK(kerr)) {
    _ILU_C_ConvertError(env, &kerr, CORBA_COMPLETED_NO);
    return ILU_NIL;
  } else {
    ILU_C_SET_SUCCESSFUL(env);
    return ret;
  }
}

#endif /* def SECURE_TRANSPORT */

/**before: Main Invariant, Call-Hi(call);
    after: Call-Invariant(call, err) && call->ca_ms == ilu_cmsHi*/
ILU_C_Object   *
_ILU_C_GetServerSingleton(ilu_Call call, ILU_ERRS((bad_param)) * err)
{
  ILU_C_Object   *h;
  ilu_Object      kobj;

  if ((kobj = ilu_GetCallSingleton(call, err)) != ILU_NIL) {
    h = (ILU_C_Object *) ilu_GetLanguageSpecificObject(kobj, MyLangIdx());
    if (h) {
      h->iluco_refcnt += 1;
      ILU_NOTE(OBJECT_DEBUG,
	    ("ILU/C: producing %p again in _ILU_C_GetServerSingleton;"
	     " refcnt := %d.\n",
	     h, h->iluco_refcnt));
    }
    ilu_ExitServer(ilu_ServerOfObject(kobj),
		   ilu_IntroTypeOfCall(call));
    return (h);
  }
  return (ILU_NIL);
}

/*before: not Inside (cobj->server, cobj->type->c) */
/*after:  return != ILU_NIL => Inside(cobj->server, cobj->type->c) */
ilu_Object _ILU_C_KernelObjOfObj (ILU_C_Object *cobj)
{
  ilu_EnterServer(cobj->server->ilucs_ks,
		  MOST_SPECIFIC_ILU_CLASS(cobj));
  if (cobj->iluco_ko != ILU_NIL)
    return (cobj->iluco_ko);
  else {
    ilu_ExitServer(cobj->server->ilucs_ks,
		   MOST_SPECIFIC_ILU_CLASS(cobj));
    return (ILU_NIL);
  }
}

struct _ILU_C_ObjectTable_struct {
  /* for slot access: L1, L2 unconstrained */

  ILU_C_ObjectTable_ApplyProc *ot_object_of_ih;
  ILU_C_ObjectTable_FreeProc *ot_free_ot;
  ilu_private     ot_user_data;
  ilu_ObjectTable ot_kernel_ot;
};

/**L1 >= {table's server};
   L1 >= {gcmu} if result is true and collectible;
   L2 unconstrained*/
static          ilu_Object
_ILU_C_OT_ObjectOfIh(ilu_ObjectTable self,
		     ilu_string ih)
{
  ILU_C_ObjectTable cot = (ILU_C_ObjectTable) (self->ot_rock);
  ILU_C_Object   *h;
  h = (cot->ot_object_of_ih) (ih, cot->ot_user_data);
  if (h == ILU_NIL)
    return (ILU_NIL);
  else
    return (h->iluco_ko);
}

/* L1 >= {table's server}; L2 unconstrained */
static void _ILU_C_OT_FreeSelf(ilu_ObjectTable self)
{
  ILU_C_ObjectTable cot = (ILU_C_ObjectTable) (self->ot_rock);
  (*cot->ot_free_ot) (cot->ot_user_data);
  ilu_free(cot);
}

/* L1, L2 unconstrained */

ILU_C_ObjectTable
ILU_C_CreateObjectTable(ILU_C_ObjectTable_ApplyProc * object_of_ih,
			ILU_C_ObjectTable_FreeProc * free_ot,
			ilu_private user_data)
{
  ILU_C_ObjectTable cot = ilu_must_malloc(sizeof(*cot));
  ilu_ObjectTable ot = ilu_must_malloc(sizeof(*ot));
  cot->ot_object_of_ih = object_of_ih;
  cot->ot_free_ot = free_ot;
  cot->ot_user_data = user_data;
  cot->ot_kernel_ot = ot;
  ot->ot_object_of_ih = _ILU_C_OT_ObjectOfIh;
  ot->ot_free_self = _ILU_C_OT_FreeSelf;
  ot->ot_rock = cot;
  return (cot);
}

void *_ILU_C_MallocFailure (ilu_cardinal nbytes)
{
  ilu_DebugPrintf("*** ILU/C: ilu_malloc(%u) failed.  Returning NIL.\n",
		  nbytes);
  return ILU_NIL;
}

#define BLKSIZE 4

ilu_CString ILU_C_Strdup (ilu_CString s)
{
  unsigned        len, fullen, i;
  ilu_CString     s2;
  if (s == ILU_NIL)
    return (ILU_NIL);
  len = strlen(s);
  fullen = len + BLKSIZE - (len % BLKSIZE);
  s2 = ilu_must_malloc(fullen);
  strcpy(s2, s);
  for (i = len + 1; i < fullen; i++)
    s2[i] = 0;
  return (s2);
}

ilu_boolean _ILU_C_IsSingleton (ilu_Class c)
{
  return (c->cl_singleton != ILU_NIL);
}

/* Main invariant holds */
ILU_C_Object   *
ILU_C_CreateSurrogateObject(ilu_Class type,
			    ILU_RETAIN(ilu_string) ih,
			    ILU_C_Server server,
			    ILU_C_ENVIRONMENT * env)
/*
 * Create and return an instance of the specified type, with the
 * specified ih, on the specified server
 */
{
  ILU_C_Object   *h = ILU_NIL;
  ilu_Object      obj = ILU_NIL;
  ILU_ERRS((bad_locks, broken_locks, inv_objref,
	    no_memory, bad_param, internal)) lerr;
  if (server) {
    if (!(server->ilucs_refcnt || server->ilucs_objcnt)) {
      ILU_ERR_CONS1(bad_param, &lerr, minor, ilu_bpm_closed, (void) 6);
      goto fale;
    }
  } else {
    server = GetDefaultServer();
    if (!ilu_Check(server->ilucs_refcnt || server->ilucs_objcnt, &lerr))
      goto fale;
  }
  obj = ilu_FindOrCreateSurrogate(server->ilucs_ks, ih, type, &lerr);
fale:
  _ILU_C_ConvertError(env, &lerr, CORBA_COMPLETED_NO);
  if (obj == ILU_NIL)
    return (ILU_NIL);
  h = ilu_GetLanguageSpecificObject(obj, MyLangIdx());
  if (h)
    IncrRefcnt(h);
  else
    h = _ILU_C_CreateSurrogateFromRegistry(ilu_ClassOfObject(obj), obj,
					   env);
  ilu_ExitServer(server->ilucs_ks, type);
  return (h);
}

/* insideServer => Inside(server->ilucs_ks, class) */
/* otherwise, Main invariant holds */
ILU_C_Object   *
ILU_C_CreateTrueObject(ILU_C_Class c,
		       ILU_OPTIONAL(ilu_CString) instance_handle,
		       ILU_OPTIONAL(ILU_C_Server) server,
		       void *instanceData,
		       ilu_boolean insideServer)
{
  ILU_C_Object   *lspo;
  ilu_Class       iluclass;
  ilu_CString     id;
  char            idbuf[10];
  static unsigned long idcounter = 0;

  if (server == ILU_NIL)
    server = GetDefaultServer();
  _ilu_Assert(server->ilucs_refcnt || server->ilucs_objcnt,
	      "ILU_C_CreateTrueObject server counts");
  _ilu_Assert((int)c,
	      "ILU_C_CreateTrueObject class not initialized");
  iluclass = ILU_CLASS_OF_C_CLASS(c);

  lspo = (ILU_C_Object *) ilu_must_malloc(sizeof(ILU_C_Object));
  lspo->iluco_class = c;
  lspo->server = server;
  lspo->instanceData = instanceData;
  lspo->interruptH = ILU_NIL;
  lspo->iluco_refcnt = 1;

  if (instance_handle != ILU_NIL)
    id = instance_handle;
  else {
    sprintf(idbuf, "%lu", ++idcounter);
    id = idbuf;
  }

  if (!insideServer)
    ilu_EnterServer(server->ilucs_ks, iluclass);
  lspo->iluco_ko = ilu_FindOrCreateTrueObject(id, server->ilucs_ks,
					      iluclass, lspo);
  lspo->iluco_kvi = (lspo->iluco_ko &&
		     ilu_VeryInterested(lspo->iluco_ko));
  if (lspo->iluco_ko)
    server->ilucs_objcnt += 1;
  ILU_NOTE(OBJECT_DEBUG,
	 ("ILU/C: creating LSO %p (in %p) for true kobj %p (in %p)"
	  " (\"%s\" in \"%s\"); obj refcnt=1, kvi=%d;"
	  " svr objcnt:=%d.\n",
	  lspo, server, lspo->iluco_ko, server->ilucs_ks, id,
	  ilu_IDOfServer(server->ilucs_ks), lspo->iluco_kvi,
	  server->ilucs_objcnt));
  if (!insideServer)
    ilu_ExitServer(server->ilucs_ks, iluclass);
  if (lspo->iluco_ko)
    return (lspo);
  else
    ilu_free(lspo);
  ilu_DebugPrintf("ILU/C: can't create kernel object for true object of type \"%s\".\n",
		  iluclass->cl_name);
  return (ILU_NIL);
}

/*Main invariant, Call-Hi(call)*/
ILU_C_Object   *
_ILU_C_InputObject(ilu_Call call, ilu_Class putative_class,
		   ilu_boolean discriminator,
		   ILU_ERRS((IoErrs)) * err)
{
  ilu_Object      obj = ILU_NIL;
  ILU_C_Object   *o = ILU_NIL;
  ilu_Class       c;
  ILU_C_ENVIRONMENT e;
  ilu_Server	  server;

  ilu_InputObjectID(call, &obj, discriminator,
		    putative_class, err);
  if (ILU_ERRNOK(*err))
    return ILU_NIL;
  if (obj == ILU_NIL)
    return (ILU_NIL);
  /* now Inside(obj->server, putative_class) */
  server = ilu_ServerOfObject(obj);
  o = (ILU_C_Object *) ilu_GetLanguageSpecificObject(obj, MyLangIdx());
  if (o == ILU_NIL) {
    if ((c = ilu_ClassOfObject(obj)) != ILU_NIL) {
      ILU_C_SET_SUCCESSFUL(&e);
      o = _ILU_C_CreateSurrogateFromRegistry(c, obj, &e);
      if (!ILU_C_SUCCESSFUL(&e)) {
	ILU_ERR_CONS1(marshal, err, minor, ((CORBA_ex_body *) (e.ptr))->minor, 0);
	CORBA_exception_free(&e);
      }
    }
  } else
    IncrRefcnt(o);
  ilu_ExitServer(server, putative_class);
  return (o);
}
 
/*Main invariant, Call-Hi(call)*/
ilu_boolean 
_ILU_C_OutputObject(ilu_Call call, ILU_C_Object * obj,
		    ilu_Class putative_class,
		    ilu_boolean discriminator,
		    ILU_ERRS((IoErrs)) * err)
{
  ILU_CLER(*err);
  if (obj != ILU_NIL)
    ilu_EnterServer(obj->server->ilucs_ks, MOST_SPECIFIC_ILU_CLASS(obj));
  ilu_OutputObjectID(call,
		     (obj == ILU_NIL) ? ILU_NIL : obj->iluco_ko,
		     discriminator, putative_class, err);
  return ILU_ERROK(*err);
}

/* Main invariant holds */
ilu_cardinal 
_ILU_C_SizeOfObject(ilu_Call call, ILU_C_Object * obj,
		    ilu_Class putative_class,
		    ilu_boolean discriminator,
		    ILU_ERRS((IoErrs)) *err)
{
  ilu_cardinal    sz;

  if (obj != ILU_NIL)
    ilu_EnterServer(obj->server->ilucs_ks, MOST_SPECIFIC_ILU_CLASS(obj));
  sz = ilu_SizeOfObjectID(call,
		      (obj == ILU_NIL) ? ILU_NIL : obj->iluco_ko,
			  discriminator, putative_class, err);
  if (obj != ILU_NIL)
    ilu_ExitServer(obj->server->ilucs_ks, MOST_SPECIFIC_ILU_CLASS(obj));
  return (sz);
}
 
#ifdef ILU_HTTPNG_OBJECTS

/* before:  Main invariant holds
   after:   Inside(obj's server, obj's class)
*/
ilu_cardinal
  _ILU_C_BeginSizingObject (ilu_Call call, ILU_C_Object *obj,
			    ilu_boolean discriminant_p,
			    ilu_Class static_type,
			    ilu_cardinal nstates,
			    ilu_Error *err)
{
  ilu_cardinal    sz;

  if (obj != ILU_NIL)
    ilu_EnterServer(obj->server->ilucs_ks, MOST_SPECIFIC_ILU_CLASS(obj));
  sz = ilu_BeginSizeOfObject(call,
			     (obj == ILU_NIL) ? ILU_NIL : obj->iluco_ko,
			     discriminant_p, static_type, nstates, err);
  return (sz);
}

/* Inside(obj's server, obj's class) */
ilu_cardinal
  _ILU_C_BeginSizingState (ilu_Call call, ILU_C_Object *obj,
			   ilu_Class state_class, ilu_Error *err)
{
  return ilu_BeginSizeOfState (call, obj->iluco_ko, state_class, err);
}

/* Inside(obj's server, obj's class) */
ilu_cardinal
  _ILU_C_FinishSizingState (ilu_Call call, ILU_C_Object *obj,
			    ilu_Class state_class, ilu_Error *err)
{
  return ilu_FinishSizeOfState (call, obj->iluco_ko, state_class, err);
}

/* before: Inside(obj's server, obj's class)
   after:  Main Invariant */
ilu_cardinal
  _ILU_C_FinishSizingObject (ilu_Call call, ILU_C_Object *obj,
			     ilu_boolean discriminant_p,
			     ilu_Class static_type,
			     ilu_Error *err)
{
  ilu_cardinal size;
  size = ilu_FinishSizeOfObject (call, obj->iluco_ko, discriminant_p, static_type, err);
  ilu_ExitServer(obj->server->ilucs_ks, MOST_SPECIFIC_ILU_CLASS(obj));
  if (ILU_ERROK(*err))
    return size;
  else
    return 0;
}

/* before:  Main Invariant
   after:   Inside(obj's server, obj's class) */
ilu_boolean
  _ILU_C_BeginOutputObject (ilu_Call call, ILU_C_Object *obj,
			    ilu_boolean discriminant_p,
			    ilu_Class static_type,
			    ilu_cardinal nstates,
			    ilu_Error *err)
{
  if (obj != ILU_NIL)
    ilu_EnterServer(obj->server->ilucs_ks, MOST_SPECIFIC_ILU_CLASS(obj));
  ilu_BeginOutputObject(call,
			(obj == ILU_NIL) ? ILU_NIL : obj->iluco_ko,
			discriminant_p, static_type, nstates, err);
  return ILU_ERROK(*err);
}

ilu_boolean
  _ILU_C_BeginOutputState (ilu_Call call, ILU_C_Object *obj,
			   ilu_Class state_class, ilu_Error *err)
{
  ilu_BeginOutputState (call, obj->iluco_ko, state_class, err);
  return ILU_ERROK(*err);
}

ilu_boolean
  _ILU_C_FinishOutputState (ilu_Call call, ILU_C_Object *obj,
			    ilu_Class state_class, ilu_Error *err)
{
  ilu_FinishOutputState (call, obj->iluco_ko, state_class, err);
  return ILU_ERROK(*err);
}

ilu_boolean
  _ILU_C_FinishOutputObject (ilu_Call call, ILU_C_Object *obj,
			     ilu_boolean discriminant_p,
			     ilu_Class static_type,
			     ilu_Error *err)
{
  ilu_FinishOutputObject(call, (obj == ILU_NIL) ? ILU_NIL : obj->iluco_ko,
			 discriminant_p, static_type, err);
  return ILU_ERROK(*err);
}

ilu_boolean
  _ILU_C_BeginInputObject (ilu_Call call, ilu_boolean discriminant_p,
			   ilu_Class static_type, ilu_Class *mst,
			   ilu_cardinal *nstates, ilu_Error *err)
{
  *mst = ilu_BeginInputObject(call, discriminant_p, static_type, nstates, err);
  return (ILU_ERROK(*err));
}

ilu_Class
  _ILU_C_BeginInputState (ilu_Call call, ilu_Error *err)
{
  ilu_string classid;

  classid = ilu_BeginInputState (call, err);
  if (ILU_ERRNOK(*err)) return ILU_NIL;
  return (ilu_FindClassFromID(classid));
}

void
  _ILU_C_SkipInputState (ilu_Call call, ilu_Error *err)
{
  ilu_SkipInputState(call, err);
}

void
  _ILU_C_FinishInputState (ilu_Call call, ilu_Error *err)
{
  ilu_FinishInputState (call, err);
}

ilu_Object
  _ILU_C_FinishInputObject (ilu_Call call, ilu_boolean discriminant_p,
			 ilu_Class static_type, ilu_Error *err)
{
  return ilu_FinishInputObject(call, discriminant_p, static_type, err);
}

#endif /* def ILU_HTTPNG_OBJECTS */

/***********************************************************************/
/***********************************************************************/
/***********************************************************************/
/***********************************************************************/

/*		Server code					       */

/***********************************************************************/
/***********************************************************************/
/***********************************************************************/
/***********************************************************************/

#include <stdio.h>      /* I/O defs (including popen and pclose) */

#if ((defined(WIN32) || defined(WIN16)) && defined(_WINIO))
#include <winiodef.h>
#endif /* ((defined(WIN32) || defined(WIN16)) && defined(_WINIO)) */

#include <sys/types.h>
#include <errno.h>
#if !(defined(WIN32) || defined(WIN16))
/* dll no sys/errno in VC++2 */
#include <sys/errno.h>
#endif /* not WIN32 or WIN16 */

#if (defined(WIN32) || defined(WIN16))
/* for WSAGetLastError */
#include <winsock.h>
#endif /* WIN32 or WIN16 */


/*Main Invariant; L2 disjoint {conn's callmu, iomu, waitmu}*/
static void     _ILU_C_ReadConnectionRequests(void *arg);

/* Main invariant holds; L2 disjoint {port's iomu, waitmu} */
static void     _ILU_C_ReadConnectionRequest(ilu_private);

/*Main Invariant holds; L2 disjoint {arg's waitmu, callmu, iomu}*/
static void     _ILU_C_ReadServiceRequest(ilu_private);

/*Main Invariant holds; L2 disjoint {conn's waitmu, callmu, iomu}*/
static ilu_boolean 
_ILU_C_FinalServiceRequest(ilu_Connection conn,
			   ilu_boolean single_threaded);

/*Main Invariant holds*/
static void PassNewConnections(void *rock);

/*L1, L2 unconstrained */

extern ilu_Exception	ex_ilu_ProtocolError;

/* Main invariant holds */
ilu_boolean ILU_C_SetFork(ILU_C_ForkProc fork)
{
  ILU_ERRS((internal)) lerr;
  if (threadedSet)
    return ilu_FALSE;
  threadedSet = ilu_TRUE;
  Fork = fork;
  threaded = ilu_TRUE;
  (void) MyLangIdx();
  (void) GetDefaultServer();
  (void) ilu_NewConnectionGetterForked(&lerr);
  ILU_MUST_BE_SUCCESS(lerr);
  (*fork) (PassNewConnections, ILU_NIL);
  return (ilu_TRUE);
}

typedef struct {
  ilu_Serializer  ilu_serializer;
  ilu_Pipeline    ilu_pipeline;
  ilu_Passport    ilu_passport;
  ilu_Passport    ilu_caller;
  ilu_Batcher     ilu_batcher;
}              *ILU_C_ImplicitArgs;

static void (*SetPerThreadData) (const void *, ILU_ERRS((no_memory, internal))*) = 0;
static void *(*GetPerThreadData) (void) = 0;

static void
  DisposeOfPerThreadData (void *arg)
{
  ILU_C_ImplicitArgs a = (ILU_C_ImplicitArgs) arg;
#if 0
  if (a->ilu_pipeline != ILU_NIL) {
    if (!ilu_ReleasePipeline(a->ilu_pipeline, &kerr))
      ILU_HANDLED(kerr);
    a->ilu_pipeline = ILU_NIL;
  };
  if (a->ilu_serializer != ILU_NIL) {
    if (!ilu_ReleaseSerializer(a->ilu_serializer, &kerr))
      ILU_HANDLED(kerr);
    a->ilu_serializer = ILU_NIL;
  };
  if (a->ilu_passport != ILU_NIL) {
    if (!ilu_DestroyPassport (a->ilu_passport, &kerr))
      ILU_HANDLED(kerr);
    a->ilu_passport = ILU_NIL;
  };
  if (a->ilu_caller != ILU_NIL) {
    if (!ilu_DestroyPassport (a->ilu_caller, &kerr))
      ILU_HANDLED(kerr);
    a->ilu_caller = ILU_NIL;
  };
  if (a->ilu_batcher != ILU_NIL) {
    if (!ilu_ReleaseBatcher(a->ilu_batcher, &kerr))
      ILU_HANDLED(kerr);
    a->ilu_batcher = ILU_NIL;
  };
#endif
  ilu_free(a);
}

static ILU_C_ImplicitArgs
  _ILU_C_CurrentContext (void)
{
  static ILU_C_ImplicitArgs the_args = ILU_NIL;

  if (threadedOther)
    return ILU_NIL;
  if (!threaded) {
    if (the_args == ILU_NIL) {
      the_args = ilu_must_malloc(sizeof(*the_args));
      memset((void*)the_args, 0, sizeof(*the_args));
    };
    return the_args;
  } else {
    ILU_C_ImplicitArgs ourArgs;
    ilu_Error kerr;
    ourArgs = GetPerThreadData();
    if (ourArgs == ILU_NIL) {
      ourArgs = ilu_must_malloc(sizeof(*ourArgs));
      if (ourArgs != ILU_NIL) {
	memset((void*)ourArgs, 0, sizeof(*ourArgs));
	if (!(SetPerThreadData(ourArgs, &kerr), ILU_ERROK(kerr))) {
	  ILU_HANDLED(kerr);
	  ilu_free(ourArgs);
	  ourArgs = ILU_NIL;
	}
      }
    }
    return ourArgs;
  }
}

/*
 * We don't want to mention ilu_OSForkNewThread() or
 * ilu_InitializeOSThreading explicitly, in case the application
 * isn't using them, to avoid linking that object module
 * unnecessarily.  So we indirect through ILU_C_EnableThreads. If the
 * application code uses ILU_C_USE_OS_THREADS, the right things are
 * automatically invoked.
 */

/*Main invariant holds*/
ilu_boolean
ILU_C_EnableThreads(ILU_C_ThreadSetupProc s,
		    ILU_C_ErrableForkProc f,
		    ILU_C_PerThreadDataSetupProc t)
{
  ILU_ERRS((bad_param, no_memory, no_resources,
	    internal)) err;

  if (s == 0 || f == 0) {
    ilu_DebugPrintf("ILU/C: ILU_C_EnableThreads given silly arguments!\n");
    return ilu_FALSE;
  }
  if (!(*s) (&err)) {
    DisposeErr(&err,
	       "attempting to set ILU kernel multi-threaded (in ILU_C_EnableThreads)");
    return ilu_FALSE;
  }
  if (!(*t) (DisposeOfPerThreadData, &GetPerThreadData, &SetPerThreadData, &err)) {
    DisposeErr(&err,
	       "attempting to setup per-thread-data (in ILU_C_EnableThreads)");
    return ilu_FALSE;
  }
  errableFork = f;
  ILU_C_SetFork(ErrlessFork);
  return ilu_TRUE;
}

/*Main invariant holds */

static void MonitorOutgoingConnection(void *rock)
{
  ilu_Connection  conn = (ilu_Connection) rock;
  ILU_ERRS((IoErrs)) lerr;
  (void) ilu_OutgoingConnectionThreadProc(conn, &lerr);
  ILU_MUST_BE_SUCCESS(lerr);
}

/* Main invariant holds */
static void PassNewConnections(void *rock)
{
  ilu_Connection  nu;
  ILU_ERRS((internal)) lerr;
  while (1) {
    nu = ilu_OtherNewConnection(&lerr);
    if (nu != ILU_NIL)
      (*Fork) (MonitorOutgoingConnection, nu);
    ILU_MUST_BE_SUCCESS(lerr);
  }
}

/* Main invariant holds */
ILU_C_Server
ILU_C_InitializeServer(char *serverID, ILU_C_ObjectTable obj_tab,
		       char *protocol, ilu_TransportInfo transport,
		       ilu_Passport pp, ilu_boolean createPortAnyway)
{ return ILU_C_FullInitializeServer(serverID, obj_tab, protocol,
				    transport, pp, createPortAnyway,
				    ilu_TRUE);
}
 
/*
 * Create a true server then open an ILU port so we've got
 * something to export objects on.
 */
/* Main invariant holds */
ILU_C_Server
ILU_C_FullInitializeServer(char *serverID, ILU_C_ObjectTable obj_tab,
			   char *protocol, ilu_TransportInfo transport,
			   ilu_Passport pp, ilu_boolean createPortAnyway,
			   ilu_boolean port_public)
{
  ILU_C_Server    ans = ilu_must_malloc(sizeof(*ans));
  ilu_Server      ksvr;
  ilu_Port        port;
  ilu_Error       err;
  char           *sid;
  char           *pinfo = ((protocol == ILU_NIL)
			   ? ilu_DefaultProtocolInfo()
			   : protocol);
  ilu_TransportInfo tinfo = ((transport == ILU_NIL)
			     ? ilu_DefaultTransportInfo()
			     : transport);
  threadedSet = ilu_TRUE;
  if (serverID == ILU_NIL)
    sid = ilu_InventID();
  else
    sid = ILU_C_Strdup(serverID);
  if (sid == ILU_NIL) {
    ilu_DebugPrintf (
       "ILU/C: Unable to allocate for copy of server ID \"%s\"\n",
	    serverID ? serverID : "(unknown gensymed ID)");
    exit(1);
  }
  ksvr = ilu_CreateTrueServer(sid,
			      ((obj_tab) ? obj_tab->ot_kernel_ot
			       : ILU_NIL),
			      MyLangIdx(), &err);
  if (ksvr == ILU_NIL) {
    ilu_DebugPrintf (
	"ILU/C: Couldn't create true server from (\"%s\", %p).\n",
	    sid, obj_tab);
    ILU_MUST_BE_SUCCESS(err);
    exit(1);
  }
  ans->ilucs_ks = ksvr;
  ans->ilucs_refcnt = 1;
  ans->ilucs_objcnt = 0;
  (void) ilu_SetLSS(ksvr, ans, MyLangIdx(), &err);
  ILU_MUST_BE_SUCCESS(err);
  ILU_NOTE(SERVER_DEBUG,
	   ("ILU/C: Created true server %p (ks %p, id %s).\n",
	    ans, ksvr, ilu_IDOfServer(ksvr)));
  ilu_ExitServer(ksvr, ilu_rootClass);
  if (protocol != ILU_NIL || transport != ILU_NIL || createPortAnyway) {
    port = ilu_FullCreatePort(ksvr, pinfo, tinfo, pp, port_public, &err);
    if (port == ILU_NIL) {
      char **ti;
      ilu_DebugPrintf("ILU/C: Error <%s> (from line %d of %s)"
	    " opening port with protocol = \"%s\" and transport =",
		      ILU_ERR_NAME(err), ilu_ErrorLine(&err),
		      ilu_ErrorFile(&err), pinfo);
      ILU_HANDLED(err);
      for (ti = tinfo;  *ti != ILU_NIL;  ti++)
	ilu_DebugPrintf (" \"%s\"", *ti);
      ilu_DebugPrintf (".\n");
      goto fale;
    }
    if (threaded) {
      Fork(_ILU_C_ReadConnectionRequests, port);
    } else {
      ILU_ERRS((no_memory, imp_limit,
		no_resources, bad_param,
		bad_locks, internal,
		broken_locks)) lerr;
      if (!ilu_SetConnectionRequestHandler(port,
				      _ILU_C_ReadConnectionRequest,
					   port, &lerr)) {
	ilu_DebugPrintf ("ILU/C: Can't register connection request handler "
			 " for new server; sid=%s, k-err=%s.\n",
			 sid, ILU_ERR_NAME(lerr));
	ILU_HANDLED(lerr);
	goto fale;
      }
    }
  }
  port = ilu_CreatePort(ksvr, ilu_DefaultProtocolInfo(),
			ilu_LocalTransportInfo(), pp, &err);
  if (port == ILU_NIL) {
    ilu_DebugPrintf("ILU/C: Error <%s> creating local port"
		    " (protocol=\"%s\", tinfo=\"%s\")\n",
		    ILU_ERR_NAME(err), ilu_DefaultProtocolInfo(),
		    ilu_LocalTransportInfo()[0]);
    ILU_HANDLED(err);
  } else{
    ILU_ERRS((no_memory, imp_limit,
	      no_resources, bad_param,
	      bad_locks, internal,
	      broken_locks)) lerr;
    if (!ilu_SetConnectionRequestHandler(port,
				      _ILU_C_ReadConnectionRequest,
					 port, &lerr)) {
      ilu_DebugPrintf ("ILU/C: Can't register connection request "
		       "handler for local port of new server; "
		       "sid=%s, k-err=%s\n", sid, ILU_ERR_NAME(lerr));
      ILU_HANDLED(lerr);
    }
  }
  return (ans);
fale:
  ilu_EnterServerMutex(ksvr, ilu_FALSE, &err);
  ILU_MUST_BE_SUCCESS(err);
  ilu_InnerBankServer(ksvr);
  ilu_SetLSS(ksvr, ILU_NIL, MyLangIdx(), &err);
  ILU_MUST_BE_SUCCESS(err);
  ans->ilucs_refcnt = 0;
  ans->ilucs_ks = ILU_NIL;
  ilu_ExitServerMutex(ksvr, ilu_TRUE, &err);
  ILU_MUST_BE_SUCCESS(err);
  ilu_free(ans);
  return ILU_NIL;
}

/* Main invariant holds */
ilu_boolean
ILU_C_AddPort(ILU_C_Server server,
	      ILU_OPTIONAL(ILU_RETAIN(char *)) protocol,
	      ILU_OPTIONAL(ILU_RETAIN(ilu_TransportInfo)) transport,
	      ILU_OPTIONAL(ILU_RETAIN(ilu_Passport)) pp,
	      ilu_boolean makeDefault,
	      ILU_C_ENVIRONMENT * env)
{ return ILU_C_FullAddPort(server, protocol, transport, pp,
			   makeDefault, ilu_TRUE, env); }

/* Main invariant holds */
ilu_boolean
ILU_C_FullAddPort(ILU_C_Server server,
		  ILU_OPTIONAL(ILU_RETAIN(char *)) protocol,
		  ILU_OPTIONAL(ILU_RETAIN(ilu_TransportInfo)) transport,
		  ILU_OPTIONAL(ILU_RETAIN(ilu_Passport)) pp,
		  ilu_boolean makeDefault,
		  ilu_boolean be_public,
		  ILU_C_ENVIRONMENT * env)
{
  char           *pinfo = ((protocol == ILU_NIL)
			   ? ilu_DefaultProtocolInfo()
			   : protocol);
  ilu_TransportInfo tinfo = ((transport == ILU_NIL)
			     ? ilu_DefaultTransportInfo()
			     : transport);
  ilu_Server      ksvr = server->ilucs_ks;
  ilu_Port        port;
  ilu_Error       lerr = ILU_INIT_NO_ERR;
  ILU_C_COMPLETIONSTATUS cstat = ILU_C_COMPLETION(NO);
  port = ilu_FullCreatePort(ksvr, pinfo, tinfo, pp, be_public, &lerr);
  if (!port) {
    char          **ti;
    ilu_DebugPrintf("ILU/C: Error <%s> (from line %d of %s)"
	    " opening port with protocol = \"%s\" and transport =",
		    ILU_ERR_NAME(lerr), ilu_ErrorLine(&lerr),
		    ilu_ErrorFile(&lerr), pinfo);
    ILU_HANDLED(lerr);
    for (ti = tinfo; *ti != ILU_NIL; ti++)
      ilu_DebugPrintf(" \"%s\"", *ti);
    ilu_DebugPrintf(".\n");
    goto fale;
  }
  cstat = ILU_C_COMPLETION(MAYBE);
  if (threaded) {
    Fork(_ILU_C_ReadConnectionRequests, port);
  } else {
    ILU_ERRS((no_memory, imp_limit,
	      no_resources, bad_param,
	      bad_locks, internal,
	      broken_locks)) lerr;
    if (!ilu_SetConnectionRequestHandler(port,
				      _ILU_C_ReadConnectionRequest,
					 port, &lerr)) {
      ilu_DebugPrintf("ILU/C: Can't register connection request handler "
		      "for new port; sid=%s, pinfo=%s, k-err=%s.\n",
		      ilu_IDOfServer(ksvr), pinfo, ILU_ERR_NAME(lerr));
      ILU_HANDLED(lerr);
      goto fale;
    }
  }
  if (makeDefault)
    ilu_SetServerDefaultPort(ksvr, port);
  cstat = ILU_C_COMPLETION(YES);
fale:
  _ILU_C_ConvertError(env, &lerr, cstat);
  return ILU_C_SUCCESSFUL(env);
}

/* Main Invariant holds */
ilu_boolean
ILU_C_AddCInfo(ILU_C_Server server,
	       ILU_RETAIN(ilu_CString) pinfo,
	       ILU_RETAIN(ilu_TransportInfo) tinfo,
	       ILU_C_ENVIRONMENT *env)
{
  ilu_Server ks = server->ilucs_ks;
  ILU_ERRS((inv_objref, bad_locks, internal,
	    no_memory)) lerr;
  if (!ilu_AddCInfo(ks, pinfo, tinfo, &lerr)) {
    _ILU_C_ConvertError(env, &lerr, CORBA_COMPLETED_NO);
    return ilu_FALSE;
  }
  ILU_C_SET_SUCCESSFUL(env);
  return ilu_TRUE;
}

typedef struct {
  ILU_C_ServerRelocateProc	proc;
  ILU_C_Server			server;
  ilu_private			rock;
} ServerRelocateShimRock;

static void
  ServerRelocateShim (ilu_Server server,
		      ilu_private rock,
		      ilu_Error *err)
{
  ServerRelocateShimRock *d = (ServerRelocateShimRock *) rock;
  ilu_ProtocolInfo pinfo = ILU_NIL;
  ilu_TransportInfo tinfo = ILU_NIL;

  if (!(d->proc)(d->server, d->rock, &pinfo, &tinfo))
    ILU_CLER(*err);
  else {
    ILU_ERR_CONS3(relocate,err,rel_scope,ilu_relocate_conn,rel_pinfo,pinfo,rel_tinfo,tinfo,0);
  }
}

/* Main invariant holds */
ilu_private
  ILU_C_SetServerRelocationProc (ILU_C_Server server,
				 ILU_C_ServerRelocateProc proc,
				 ilu_private rock,
				 ILU_C_ENVIRONMENT * env)
{
  ilu_Server      ksvr = server->ilucs_ks;
  ilu_Error       lerr = ILU_INIT_NO_ERR;
  ILU_C_COMPLETIONSTATUS cstat = ILU_C_COMPLETION(NO);
  ServerRelocateShimRock *d, *old;
  ilu_private old_rock = ILU_NIL;

  if ((proc == (ILU_C_ServerRelocateProc) 0) && (rock == ILU_NIL)) {
    d = ILU_NIL;
  } else {
    d = ilu_MallocE(sizeof(ServerRelocateShimRock), &lerr);
    if (ILU_ERRNOK(lerr))
      goto fale0;
    d->proc = proc;
    d->server = server;
    d->rock = rock;
  }
  ilu_EnterServerMutex(ksvr, ilu_TRUE, &lerr);
  if (ILU_ERRNOK(lerr))
    goto fale1;
  old = ilu_SetServerRelocateProc(ksvr, ServerRelocateShim, (ilu_private) d, &lerr);
  if (ILU_ERRNOK(lerr))
    goto fale2;
  if (old != ILU_NIL) {
    old_rock = old->rock;
    ilu_free(old);
  }
  ilu_ExitServerMutex(ksvr, ilu_TRUE, &lerr);
  goto fale0;

 fale2:
  {
    ilu_Error lerr2;
    ilu_ExitServerMutex(ksvr, ilu_TRUE, &lerr2);
    ILU_HANDLED(lerr2);
  }
 fale1:
  ilu_free(d);
 fale0:
  _ILU_C_ConvertError(env, &lerr, cstat);
  return old_rock;
}

/* Main Invariant holds */
ilu_boolean
ILU_C_Server_CInfo(ILU_C_Server server,
		   ilu_boolean want_public,
		   char **pinfo,
		   ilu_TransportInfo *tinfo,
		   ILU_C_ENVIRONMENT *env)
{
  char *lpinfo;
  ilu_TransportInfo ltinfo;
  ILU_ERRS((bad_param, internal, no_memory)) lerr;
  if (!ilu_ServerCInfo(server->ilucs_ks, want_public, &lpinfo, &ltinfo, &lerr))
    goto dun1;
  *tinfo = ilu_CopyTinfo(ltinfo, &lerr);
  if (ILU_ERRNOK(lerr))
    goto dun1;
  *pinfo = ilu_malloc(strlen(lpinfo)+1);
  if (! *pinfo)
    goto dun2;
  strcpy(*pinfo, lpinfo);
  ILU_C_SET_SUCCESSFUL(env);
  return ilu_TRUE;
 dun2:
  ilu_TransportInfo_Free(*tinfo);
 dun1:
    _ILU_C_ConvertError(env, &lerr, CORBA_COMPLETED_NO);
    return ilu_FALSE;
}

/*
 * Create a true server then open an ILU port so we've got
 * something to export objects on.  This function should be called
 * at most once.
 */
/* Main invariant holds */
static ILU_C_Server GetDefaultServer(void)
{
  if (defaultServer != ILU_NIL)
    return defaultServer;
  defaultServer = ILU_C_InitializeServer(ILU_NIL, ILU_NIL, ILU_NIL,
					 ILU_NIL, ILU_NIL, ilu_TRUE);
  if (defaultServer == ILU_NIL) {
    ilu_DebugPrintf("ILU/C: Error:  Couldn't create default server.\n");
    exit(1);
  }
  return defaultServer;
}

/* Main invariant holds; L2 disjoint {arg's waitmu, callmu, iomu} */
static void _ILU_C_RunConnection(void *arg)
{
  ilu_Connection  conn = (ilu_Connection) arg;
  ilu_boolean     closed = ilu_FALSE;
  while (! closed) {
    closed = ! ilu_BlockingWaitForInputOnConnection(conn, ILU_NIL);
    if (closed) {
      ILU_ERRS((bad_param, broken_locks, bad_locks, internal)) lerr;
      if (ILU_C_Logging)
	ilu_DebugPrintf("ILU/C: done serving connection %p.\n", conn);
      ilu_DoneServingConnection(conn, &lerr);
      DisposeErr(&lerr, "cleaning up after serving a connection");
    } else
      closed = _ILU_C_FinalServiceRequest(conn, ilu_FALSE);
  }
  return;
}

/*Main Invariant; L2 disjoint {conn's callmu, iomu, waitmu}*/
static void _ILU_C_ReadConnectionRequests(void *arg)
{
  ilu_Port        p = (ilu_Port) arg;
  ilu_boolean     closed = 0;
  ilu_Connection  conn;
  char            ef[ERRFMTSIZE];
  ILU_ERRS((IoErrs, bad_locks)) lerr;

  while (1) {
    if (ILU_C_Logging)
      ilu_DebugPrintf("ILU/C: Port %p waiting for connection request.\n", p);
    ilu_WaitForPortConnectionRequest(p, &lerr);
    if (ILU_ERRNOK(lerr)) {
      if (ILU_C_Logging)
	ilu_DebugPrintf("ILU/C: No longer waiting for connection requests on"
			" port %p,\n because ilu_WaitForPortConnectionRequest"
			" raised %s.\n", p, FmtError(ef, &lerr));
      ILU_HANDLED(lerr);
      break;
    }
    conn = ilu_HandleNewConnection(p, &closed, &lerr);
    if (closed) {
      if (ILU_C_Logging)
	ilu_DebugPrintf("ILU/C: No longer waiting for connection requests "
			"on port %p, because it's closed.\n", p);
      break;
    }
    if (conn == ILU_NIL) {
      ILU_ERR_SWITCH(lerr) {
	ILU_SUCCESS_CASE {
	  if (ILU_C_Logging)
	    ilu_DebugPrintf("ILU/C: Spurious input call on port %p.\n", p);
	  goto nechst;
	}
	ILU_ERR_CASE(no_resources, x)
	  if (ILU_C_Logging)
	  ilu_DebugPrintf("ILU/C: Connection request on port %p"
			" ran over FD limit --- port abandoned.\n",
			  p);
	ILU_ERR_ELSE
	  if (ILU_C_Logging)
	  ilu_DebugPrintf("ILU/C: Got error %s"
			  " trying to accept connection on port %p;"
			  " abandoning port.\n",
			  FmtError(ef, &lerr), p);
      } ILU_ERR_ENDSWITCH;
      ILU_HANDLED(lerr);
      goto dun;
    } else {
      /* fork thread to run new connection */
      Fork(_ILU_C_RunConnection, conn);
    }
nechst:
    /* dummy statement */;
  }
dun:
  if (!ilu_DoneWithPort(p, &lerr))
    DisposeErr(&lerr, "cleaning up after animating a port");
}

/*Main Invariant holds; L2 disjoint {conn's iomu, callmu, waitmu}*/
static ilu_boolean EnableRequests(ilu_Connection conn, ilu_Call call)
{
  ilu_Server      s = ilu_ServerOfConnection(conn);
  ilu_string      sid = ilu_IDOfServer(s);
  ilu_boolean     ans;
  ILU_ERRS((no_memory, no_resources)) lerr;
  call->ca_reqs_enabled = ilu_TRUE;
  ans = ilu_SetConnectionInputHandler(conn, _ILU_C_ReadServiceRequest,
				      conn, &lerr);
  if (!ans) {
    if (ILU_C_Logging)
      ilu_DebugPrintf("ILU/C: Can't register request handler for conn %p server %s!\nError = %s\n",
		      conn, sid, ILU_ERR_NAME(lerr));
    else
      ilu_DebugPrintf(
	    "ILU/C: Can't register request handler for conn %p server %s!\nError = %s\n",
	    conn, sid, ILU_ERR_NAME(lerr));
    ILU_HANDLED(lerr);
  } else if (ILU_C_Logging)
    ilu_DebugPrintf("ILU/C: Connection %p server %s awaiting requests.\n",
	   conn, sid);
  return ans;
}

/*Main Invariant holds; L2 disjoint {conn's iomu, callmu, waitmu}*/
static ilu_boolean DisableRequests(ilu_Connection conn, ilu_Call call)
{
  ilu_Server      s = ilu_ServerOfConnection(conn);
  ilu_string      sid = ilu_IDOfServer(s);
  ilu_boolean     ans;
  ILU_ERRS((no_memory, no_resources)) lerr;
  call->ca_reqs_enabled = ilu_FALSE;
  ans = ilu_SetConnectionInputHandler(conn, NULL, ILU_NIL, &lerr);
  if (!ans) {
    ilu_DebugPrintf("ILU/C: failure (%s) detected when unregistering request handler for conn %p server %s!\n",
		    ILU_ERR_NAME(lerr), conn, sid);
    ILU_HANDLED(lerr);
  } else if (ILU_C_Logging)
    ilu_DebugPrintf("ILU/C: Connection %p server %s not awaiting requests.\n",
		    conn, sid);
  return ans;
}

/* Main invariant holds; L2 disjoint {port's iomu, waitmu} */
static void
_ILU_C_ReadConnectionRequest(ilu_private arg)
{
  ilu_Port        p = (ilu_Port) arg;
  ilu_boolean     closed = 0;
  ilu_Connection  conn;
  ilu_Call_s      nocall;
  ILU_ERRS((IoErrs, bad_locks, no_resources)) lerr;
  char            ef[ERRFMTSIZE];
  if (ILU_C_Logging)
    ilu_DebugPrintf("ILU/C: Port %p handling connection request.\n", p);
  conn = ilu_HandleNewConnection(p, &closed, &lerr);
  if (closed) {
    if (ILU_C_Logging)
      ilu_DebugPrintf("ILU/C: Port %p closed.\n", p);
    goto abandon;
  }
  if (conn == ILU_NIL) {
    ILU_ERR_SWITCH(lerr) {
      ILU_SUCCESS_CASE {
	if (ILU_C_Logging)
	  ilu_DebugPrintf("ILU/C: Spurrious input call on port %p.\n", p);
	goto ok;
      }
      ILU_ERR_CASE(no_resources, x)
	if (ILU_C_Logging)
	ilu_DebugPrintf("ILU/C: Connection request on port %p"
			" ran over FD limit --- port abandoned.\n", p);
      ILU_ERR_ELSE
	if (ILU_C_Logging)
	ilu_DebugPrintf("ILU/C: Got error %s"
			" trying to accept connection on port %p;"
			" abandoning port.\n",
			FmtError(ef, &lerr), p);
    } ILU_ERR_ENDSWITCH;
    ILU_HANDLED(lerr);
    goto abandon;
  }
  EnableRequests(conn, &nocall);
ok:
  return;
abandon:
  ilu_DoneWithPort(p, &lerr);
  DisposeErr(&lerr, "ceasing to animate a port");
}

/** Main Invariant holds;
    before: Call-Locking(call, IHi);
    after:  Call-Locking(call, VNo) */
typedef void (*cstubproc) (ilu_Call, ilu_Error *);

typedef struct {
  /* L1, L2 unconstrained */

  ilu_Call_s      call_s;
  cstubproc	  stubproc;
}               C_Invokn;

/** Main Invariant holds;
    before: Call-Locking(call, IHi);
    after:  Call-Locking(call, VNo) */
static void DoInvocation(void *arg)
{
  C_Invokn       *ci = (C_Invokn *) arg;
  ILU_ERRS((bad_locks, IoErrs)) lerr;

  (*(ci->stubproc)) (&(ci->call_s), &lerr);
  ILU_HANDLED(lerr);
  ilu_free(ci);
  return;
}

/* Returns TRUE iff connection closed. */
/*Main Invariant holds; L2 disjoint {conn's waitmu, callmu, iomu}*/
/*If single_threaded: requests enabled for fd*/
static          ilu_boolean
_ILU_C_FinalServiceRequest(ilu_Connection conn,
			   ilu_boolean single_threaded)
{
  ilu_Call_s      call_s, *call;
  ilu_boolean     initted;
  ilu_Class       class;
  ilu_Method      method;
  ilu_cardinal    SN;
  ilu_RcvReqStat  stat;
  ILU_ERRS((bad_locks, IoErrs)) lerr = ILU_INIT_NO_ERR;
  C_Invokn       *ci = ILU_NIL;

  if (ILU_C_Logging)
    ilu_DebugPrintf("ILU/C: _ILU_C_FinalServiceRequest(%p, %s)\n",
		    conn, single_threaded ? "single-threaded" : "multi-threaded");
  if (single_threaded) {
    call = &call_s;
    DisableRequests(conn, call);
  } else if (ilu_ThreadPerRequest(conn)) {
    ci = ilu_MallocE(sizeof(*ci), &lerr);
    if (ILU_ERRNOK(lerr)) {	/* out of memory */
      DisposeErr(&lerr,
	   "allocating memory to fork a thread to service a call");
      ilu_DebugPrintf("ILU/C: _ILU_C_FinalServiceRequest: abandoning connection %p"
		      " for lack of memory.\n", conn);
      ilu_DoneServingConnection(conn, &lerr);
      DisposeErr(&lerr, "closing a connection in _ILU_C_FinalServiceRequest");
      return ilu_TRUE;
    }
    call = &(ci->call_s);
  } else
    call = &call_s;
  stat = ilu_ReceiveRequest(call, &initted, conn, &class, &method,
			    &SN, &lerr);
  if (ILU_C_Logging) {
    if (stat == ilu_RcvReqStat_request)
      ilu_DebugPrintf("ILU/C: _ILU_C_FinalServiceRequest:  received "
		      "request SN %lu, method \"%s\" of class \"%s\"\n",
		      (unsigned long) SN, method->me_name, class->cl_name);
    else
      ilu_DebugPrintf("ILU/C: _ILU_C_FinalServiceRequest:  ReceiveRequest "
		      "=> %s, *initted = %s, *err = %s\n",
		      ((stat == ilu_RcvReqStat_noop) ? "noop" :
		       ((stat == ilu_RcvReqStat_quit) ? "quit" :
			"invalid result code")),
		      ((initted) ? "T" : "F"),
		      (ILU_ERROK(lerr) ? "SUCCESS" : ILU_ERR_NAME(lerr)));
  }
  if (stat == ilu_RcvReqStat_request) {
    /* Call-Locking(call, IHi) */
    cstubproc sp;
    sp = (cstubproc) ilu_GetMethodStubProc(method, MyLangIdx());
    if (sp == NULLFN) {
      ILU_ERR_CONS0(no_implement, &lerr, 0);
      ILU_ERRPRINTF ("ILU/C:  Avoiding call SN %lu to unimplemented method \"%s\" of class \"%s\".\n",
		     (unsigned long) SN, method->me_name, class->cl_name);
    } else if (ci) {
      ci->stubproc = sp;
      (*Fork) (DoInvocation, ci);
      /* L2 disjoint {conn's callmu, iomu} (handed off to new thd) */
    } else {
      (*sp) (call, &lerr);
      /* L2 disjoint {conn's waitmu, callmu, iomu} */
      /* single_threaded != threaded */
      /* !threaded <=> requests enabled */
    }
    return (ilu_FALSE);
  } else if (initted) {
    ilu_FinishCall(call, &lerr);
  }
  if (ci)
    ilu_free(ci);

  if (ILU_ERRNOK(lerr) && (lerr.ilu_type == ILU_ERRTYP(comm_failure)))
    stat = ilu_RcvReqStat_quit;
  DisposeErr(&lerr,
	     "serving a call (in _ILU_C_FinalServiceRequest)");

  if (stat == ilu_RcvReqStat_quit) {
    if (ilu_ConnectionServingP(conn)) {
      if (ILU_C_Logging)
	ilu_DebugPrintf("ILU/C: _ILU_C_FinalServiceRequest: finished serving connection %p in nested call.\n",
			conn);
    } else {
      if (ILU_C_Logging)
	ilu_DebugPrintf("ILU/C: _ILU_C_FinalServiceRequest: done with %p.\n",
			conn);
      ilu_DoneServingConnection(conn, &lerr);
      DisposeErr(&lerr,
	   "closing a connection (in _ILU_C_FinalServiceRequest)");
    }
    return ilu_TRUE;
  }
  if (single_threaded || ilu_InmemConnection(conn))
    EnableRequests(conn, call);
  return ilu_FALSE;
}

/* Returns TRUE iff connection closed. */
/*Main Invariant holds; L2 disjoint {arg's waitmu, callmu, iomu}*/
static void 
_ILU_C_ReadServiceRequest(ilu_private arg)
{
  ilu_Connection  conn = (ilu_Connection) arg;
  (void) _ILU_C_FinalServiceRequest(conn, !threaded);
  return;
}


/**before: Call-Locking(call, IHi);
    after: Call-Invariant(call, err),
	   success => Call-Locking(call, VLo)*/
/*If not threaded:
  before: requests not enabled;
  after:  requests enabled iff protocol concurrent.*/
ilu_boolean
_ILU_C_FinishParameters(ilu_Call call, ILU_C_Object * obj,
			ILU_ERRS((bad_locks, broken_locks)) * err)
{
  ilu_Connection  conn;
  ilu_boolean     ans;
  conn = ilu_ConnectionOfCall(call);
  ans = ilu_RequestRead(call, err);
  if ((!threaded) && ilu_ThreadPerRequest(conn))
    EnableRequests(conn, call);
  return ans;
}

/**before: Main Invariant && Call-VLo(call);
    after: Call-Invariant(call, err),
	   success => call->ca_ms == ilu_cmsHi. */

/**If not threaded:
  before: requests enabled iff protocol concurrent;
  after:  requests not enabled.*/
ilu_boolean
_ILU_C_BeginReply(ilu_Call call, ilu_boolean exceptions,
		  ilu_cardinal argSize,
		  ILU_ERRS((bad_locks, IoErrs)) * err)
{
  ilu_Connection  conn = ilu_ConnectionOfCall(call);
  ilu_boolean     ans;
  if ((!threaded) && ilu_ThreadPerRequest(conn))
    DisableRequests(conn, call);
  ans = ilu_BeginReply(call, exceptions, argSize, err);
  return ans;
}

/*If not threaded:
  before: requests enabled iff protocol concurrent;
  after:  requests not enabled.*/
ilu_boolean
_ILU_C_BeginException(ilu_Call call, ilu_cardinal evalue,
		      ilu_cardinal argSize,
		      ILU_ERRS((bad_locks, IoErrs)) * err)
{
  ilu_Connection  conn = ilu_ConnectionOfCall(call);
  ilu_boolean     ans;
  if ((!threaded) && ilu_ThreadPerRequest(conn))
    DisableRequests(conn, call);
  ans = ilu_BeginException(call, evalue, argSize, err);
  return ans;
}

/*Main Invariant; Call-Locking(call, OHi)*/

/*If not threaded: requests not enabled.*/
ilu_boolean
_ILU_C_FinishReply(ilu_Call call, ILU_ERRS((bad_locks, IoErrs)) * err)
{
  ilu_boolean     ans;
  ILU_C_Batcher   batcher = ILU_NIL;
  ILU_C_ImplicitArgs ia;
  if ((ia = _ILU_C_CurrentContext()) != ILU_NIL) {
    batcher = ia->ilu_batcher;
  };
  ans = ilu_FullFinishReply(call, batcher, err);
  return ans;
}

/*If not threaded: requests not enabled.*/
ilu_boolean
_ILU_C_FinishException(ilu_Call call,
		       ILU_ERRS((bad_locks, IoErrs)) * err)
{
  ilu_boolean     ans;
  ILU_C_Batcher   batcher = ILU_NIL;
  ILU_C_ImplicitArgs ia;
  if ((ia = _ILU_C_CurrentContext()) != ILU_NIL) {
    batcher = ia->ilu_batcher;
  };
  ans = ilu_FullFinishException(call, batcher, err);
  return ans;
}

/**Before: Call-Invariant(call, err);
    After: Main Invariant*/
void 
_ILU_C_FinishServingCall(ilu_Call call, ilu_Error * err)
{
  ilu_Connection  conn = ilu_ConnectionOfCall(call);
  ilu_FinishCall(call, err);
  if (!threaded && !call->ca_reqs_enabled) {
    ILU_ERR_SWITCH(*err) {
      ILU_ERR_CASE(comm_failure, v) {
	if (!ilu_ConnectionServingP(conn)) {
	  ILU_ERRS((bad_param, broken_locks,
		    bad_locks, internal)) lerr;
	  (void) ilu_DoneServingConnection(conn, &lerr);
	  DisposeErr(&lerr,
		     "_ILU_C_FinishServingCall's call on ilu_DoneServingConnection");
	}
      }
      ILU_ERR_ELSE
	EnableRequests(conn, call);
    } ILU_ERR_ENDSWITCH;
  }
  DisposeErr(err, "serving a call");
  /* Should hand off to app somehow instead. */
  return;
}

/*Main Invariant; Call-Locking(call, Lo)*/
/**If not threaded:
  before: requests enabled iff protocol concurrent;
  after:  requests enabled iff protocol concurrent.*/
ilu_boolean
_ILU_C_NoReply(ilu_Call call,
	       ILU_ERRS((bad_param, bad_locks, broken_locks)) * err)
{
  ilu_boolean     ans;

  ans = ilu_NoReply(call, err);
  return ans;
}

/*Main Invariant holds; L2 otherwise unconstrained*/

void ILU_C_Stoppable_Run(int* stop) {

    threadedSet = ilu_TRUE;
    ilu_RunMainLoop( stop );
}

void ILU_C_Run(void)
{
    int		stop = 0;

    threadedSet = ilu_TRUE;
    if (threaded)
      while (1) OS_SLEEP(30);
    else
      ilu_RunMainLoop( &stop );
}

void ILU_C_StoppableRun(int *stop)
{
  threadedSet = ilu_TRUE;
  if (threaded) {
    *stop = 0;
    while (!*stop)
      OS_SLEEP(30);
  } else
    ilu_RunMainLoop(stop);
}

void ILU_C_StopRun(int *stop)
{
  if (threaded)
    *stop = 1;
  else
    ilu_ExitMainLoop(stop);
}

/* L1, L2 unrestricted */

unsigned int _ILU_C_SafeStrlen (ilu_CString str)
{
  if (str == ILU_NIL)
    return 0;
  else
    return (strlen((char *) str));
}

/* L1, L2 unrestricted */
void _ILU_C_ExtendString (CORBA_char **str, CORBA_char item, CORBA_boolean atend)
{
  CORBA_char *s2;
  ilu_cardinal size;

  if (*str == ILU_NIL)
    size = 0;
  else
    size = strlen((char *) (*str));
  s2 = ilu_must_malloc(size + 2);
  if (!atend)
    {
      s2[0] = item;
      if (*str != ILU_NIL)
	strncpy ((char *) (s2 + 1), (char *) (*str), size);
      s2[size + 1] = 0;
    }
  else
    {
      if (*str != ILU_NIL)
	strncpy ((char *) s2, (char *) (*str), size);
      s2[size] = item;
      s2[size + 1] = 0;
    }
  *str = s2;
}

/* L1, L2 unrestricted */
void _ILU_C_ExtendWString (CORBA_wchar **str, CORBA_wchar item, CORBA_boolean atend)
{
  CORBA_wchar *s2;
  ilu_cardinal size;

  size = _ILU_C_SafeWStrlen(*str);
  s2 = ilu_must_malloc((size + 2) * sizeof(CORBA_wchar));
  if (!atend)
    {
      s2[0] = item;
      if (*str != ILU_NIL)
	{
	  CORBA_wchar *p, *q;
	  ilu_cardinal i;

	  for (p = s2 + 1, q = *str, i = 0;  i < size;  i++)
	    *p++ = *q++;
	}
      s2[size + 1] = 0;
    }
  else
    {
      if (*str != ILU_NIL)
	{
	  CORBA_wchar *p, *q;
	  ilu_cardinal i;

	  for (p = s2, q = *str, i = 0;  i < size;  i++)
	    *p++ = *q++;
	}
      s2[size] = item;
      s2[size + 1] = 0;
    }
  *str = s2;
}

/*L1, L2 unrestricted*/
void _ILU_C_PopString (CORBA_char **s, CORBA_char *ret)
{
  if (s != ILU_NIL && *s != ILU_NIL && **s != 0)
    {
      *ret = **s;
      *s = *s + 1;      
    }
}

/*L1, L2 unrestricted*/
void _ILU_C_PopWString (CORBA_wchar **s, CORBA_wchar *ret)
{
  if (s != ILU_NIL && *s != ILU_NIL && **s != 0)
    {
      *ret = **s;
      *s = *s + 1;
    }
}

/*L1, L2 unrestricted*/
unsigned int _ILU_C_SafeWStrlen (CORBA_wchar *str)
{
  register CORBA_wchar *p = str;

  if (str == ILU_NIL)
    return 0;

  while (*p++ != 0)
    ;
  return (p-str);
}

void
 _ILU_C_CheckStubConsistency (char *interface_name, char *ilu_version, char *type_uid_version)
{
  if (strcmp(ilu_version, ilu_GetILUVersion()) != 0) {
    ilu_DebugPrintf("ILU:  Warning!  The C stubs for interface \"%s\" were generated for ILU version \"%s\", while the ILU kernel library you are using is version \"%s\".\n", interface_name, ilu_version, ilu_GetILUVersion());
    _ilu_Assert(strcmp(ilu_version, ilu_GetILUVersion()) == 0, "ILU C stub version mismatch with ILU kernel version");
  };
  if (strcmp(type_uid_version, ilu_GetILUTypeUIDVersion()) != 0) {
    ilu_DebugPrintf("ILU:  Warning!  The type UID version used in the C stubs for interface \"%s\" is \"%s\", while the ILU kernel library expects version \"%s\".\n", interface_name, type_uid_version, ilu_GetILUTypeUIDVersion());
    _ilu_Assert(strcmp(type_uid_version, ilu_GetILUTypeUIDVersion()) == 0, "ILU C stub type UID version mismatch with ILU kernel type UID version");
  };
}

/*
 * (*status) and (*err) are OUT parameters.  Returns with (*status)
 * ready for a call on CORBA_exception_free.  When (*err) indicates
 * an error, it's local and (*status) isn't all the thrower wanted
 * it to be.
 */
/*Main Invariant, Call-IHi(call)*/
static          ilu_boolean
_ILU_C_CatchException(ilu_Call call, ilu_Method method,
		      _ILU_C_ExceptionDescription *evec,
		      ILU_C_ENVIRONMENT * status,
		      ilu_cardinal exceptionIndex,
		      ILU_ERRS((IoErrs)) * err)
{

  if ((exceptionIndex > method->me_exceptionCount) ||
      exceptionIndex == 0) {
    ILU_C_RAISE_SYSTEM(status, UNKNOWN, 0, MAYBE);
  } else {
    unsigned int    valsize;
    ilu_Class       *valclass;
    ILU_C_InputFn   valfn;

    status->_major = ILU_C_USER_EXCEPTION;
    status->returnCode = ((ILU_C_ExceptionCode)
		   method->me_exceptionVector[exceptionIndex - 1]);
    status->freeRoutine = 0;
    valsize = evec[exceptionIndex - 1].size;
    if (valsize > 0) {
      status->ptr = (void *) ilu_MallocE(valsize, err);
      if (ILU_ERRNOK(*err))
	return ilu_FALSE;
      if (evec[exceptionIndex - 1].ed_class != ILU_NIL) {
	valclass = evec[exceptionIndex - 1].ed_class;
	*(ILU_C_Object **) status->ptr =
	  _ILU_C_InputObject(call, *valclass, ilu_FALSE, err);
      } else {
	valfn = evec[exceptionIndex - 1].inFn;
	(void) (*valfn) (call, status->ptr, err);
      }
      if (ILU_ERRNOK(*err))
	return ilu_FALSE;
      status->freeRoutine = evec[exceptionIndex - 1].freeFn;
    } else {
      status->ptr = ILU_NIL;
      ILU_CLER(*err);
    }
  }
  return ilu_TRUE;
}

ilu_cardinal ILU_C_SizeOfBoolean (ilu_Call call, CORBA_boolean b, ilu_Error *err)
{
  return (ilu_SizeOfBoolean(call, (ilu_boolean) b, err));
}

void ILU_C_OutputBoolean (ilu_Call call, CORBA_boolean b, ilu_Error *err)
{
  ilu_OutputBoolean (call, (ilu_boolean) b, err);
}

void ILU_C_InputBoolean (ilu_Call call, CORBA_boolean *b, ilu_Error *err)
{
  ilu_boolean b2;

  if (ilu_InputBoolean (call, &b2, err), ILU_ERROK(*err))
    *b = (CORBA_boolean) b2;
}

ilu_cardinal
  ILU_C_SizeOfCharacter (ilu_Call call, CORBA_wchar b, ilu_Error *err)
{
  if (b >= 0x10000)
    return ILU_ERR_CONS1(imp_limit, err, minor, ilu_ilm_unsupported_charset_encoding, 0);
  return (ilu_SizeOfCharacter(call, (ilu_character) b, err));
}

void
  ILU_C_OutputCharacter (ilu_Call call, CORBA_wchar b, ilu_Error *err)
{
  if (b >= 0x10000)
    ILU_ERR_CONS1(imp_limit, err, minor, ilu_ilm_unsupported_charset_encoding, 0);
  else
    ilu_OutputCharacter (call, (ilu_character) b, err);
}

void ILU_C_InputCharacter (ilu_Call call, CORBA_wchar *b, ilu_Error *err)
{
  ilu_character b2;

  if (ilu_InputCharacter (call, &b2, err), ILU_ERROK(*err))
    *b = (CORBA_wchar) b2;
}

ilu_boolean _ILU_C_CheckSibling (ILU_C_Object *disc,
				 ILU_C_Object *arg,
				 ILU_C_ENVIRONMENT *status)
{
  char *disc_sid = ilu_IDOfServer(disc->server->ilucs_ks);
  char *arg_sid = ilu_IDOfServer(arg->server->ilucs_ks);

  if (strcmp(disc_sid, arg_sid) != 0) {
    ILU_C_RAISE_SYSTEM(status, BAD_PARAM, (int) ilu_bpm_not_sibling, NO);
    return ilu_FALSE;
  } else {
    ILU_C_SET_SUCCESSFUL(status);
    return ilu_TRUE;
  }
}

ilu_Serializer
  ILU_C_CreateSerializationContext (ILU_C_Server lss,
				    ILU_C_ENVIRONMENT *env)
{
  ilu_Error err;
  ilu_Serializer si;
  si = ilu_GetSerializer (lss->ilucs_ks, &err);
  if (ILU_ERROK(err)) {
    ILU_C_SET_SUCCESSFUL(env);
    return si;
  } else {
    _ILU_C_ConvertError(env, &err, CORBA_COMPLETED_NO);
    return ILU_NIL;
  }
}

ilu_boolean
ILU_C_ReleaseSerializer(ILU_C_Serializer val,
			ILU_C_ENVIRONMENT * env)
{
  ilu_Error       err;
  ilu_boolean     ans;
  ans = ilu_ReleaseSerializer(val, &err);
  if (ans)
    ILU_C_SET_SUCCESSFUL(env);
  else
    _ILU_C_ConvertError(env, &err, CORBA_COMPLETED_NO);
  return ans;
}

ilu_boolean
  ILU_C_SetSerializationContext (ilu_Serializer val)
{
  ILU_C_ImplicitArgs ia = _ILU_C_CurrentContext();
  if (ia == ILU_NIL) return ilu_FALSE;
  ia->ilu_serializer = val;
  return ilu_TRUE;
}

ilu_Serializer
  ILU_C_GetSerializationContext (void)
{
  ILU_C_ImplicitArgs ia = _ILU_C_CurrentContext();
  if (ia == ILU_NIL) return ILU_NIL;
  return (ia->ilu_serializer);
}

ILU_C_Batcher
ILU_C_CreateBatcher(ilu_FineTime dt,
		    ilu_boolean pushable,
		    ILU_C_ENVIRONMENT * env)
{
  ilu_Error       err;
  ilu_Batcher     b;
  b = ilu_CreateBatcher(dt, pushable, &err);
  if (ILU_ERROK(err)) {
    ILU_C_SET_SUCCESSFUL(env);
    return b;
  } else {
    _ILU_C_ConvertError(env, &err, CORBA_COMPLETED_NO);
    return ILU_NIL;
  }
}

ilu_boolean
ILU_C_PushBatcher(ILU_C_Batcher val,
		     ILU_C_ENVIRONMENT * env)
{
  ilu_Error       err;
  ilu_boolean     ans;
  ans = ilu_PushBatcher(val, &err);
  if (ans)
    ILU_C_SET_SUCCESSFUL(env);
  else
    _ILU_C_ConvertError(env, &err, CORBA_COMPLETED_NO);
  return ans;
}

ilu_boolean
ILU_C_ReleaseBatcher(ILU_C_Batcher val,
		     ILU_C_ENVIRONMENT * env)
{
  ilu_Error       err;
  ilu_boolean     ans;
  ans = ilu_ReleaseBatcher(val, &err);
  if (ans)
    ILU_C_SET_SUCCESSFUL(env);
  else
    _ILU_C_ConvertError(env, &err, CORBA_COMPLETED_NO);
  return ans;
}

ilu_boolean
ILU_C_SetBatcherContext(ilu_Batcher val)
{
  ILU_C_ImplicitArgs ia = _ILU_C_CurrentContext();
  if (ia == ILU_NIL)
    return ilu_FALSE;
  ia->ilu_batcher = val;
  return ilu_TRUE;
}

ilu_Batcher
ILU_C_GetBatcherContext(void)
{
  ILU_C_ImplicitArgs ia = _ILU_C_CurrentContext();
  if (ia == ILU_NIL)
    return ILU_NIL;
  return (ia->ilu_batcher);
}

ilu_Pipeline
  ILU_C_CreatePipeline (ILU_C_ENVIRONMENT *env)
{
  ilu_Error err;
  ilu_Pipeline pl;
  pl = ilu_GetPipeline(&err);
  if (ILU_ERROK(err)) {
    ILU_C_SET_SUCCESSFUL(env);
    return pl;
  } else {
    _ILU_C_ConvertError(env, &err, CORBA_COMPLETED_NO);
    return ILU_NIL;
  }
}

ilu_boolean
ILU_C_ReleasePipeline(ILU_C_Pipeline val,
		      ILU_C_ENVIRONMENT * env)
{
  ilu_Error       err;
  ilu_boolean     ans;
  ans = ilu_ReleasePipeline(val, &err);
  if (ans)
    ILU_C_SET_SUCCESSFUL(env);
  else
    _ILU_C_ConvertError(env, &err, CORBA_COMPLETED_NO);
  return ans;
}

ilu_boolean
  ILU_C_SetPipelineContext (ilu_Pipeline val)
{
  ILU_C_ImplicitArgs ia = _ILU_C_CurrentContext();
  if (ia == ILU_NIL) return ilu_FALSE;
  ia->ilu_pipeline = val;
  return ilu_TRUE;
}

ilu_Pipeline
  ILU_C_GetPipelineContext (void)
{
  ILU_C_ImplicitArgs ia = _ILU_C_CurrentContext();
  if (ia == ILU_NIL) return ILU_NIL;
  return (ia->ilu_pipeline);
}

ilu_boolean
  ILU_C_SetPassportContext (ilu_Passport val)
{
  ILU_C_ImplicitArgs ia = _ILU_C_CurrentContext();
  if (ia == ILU_NIL) return ilu_FALSE;
  ia->ilu_passport = val;
  return ilu_TRUE;
}

ilu_Passport
  ILU_C_GetPassportContext (void)
{
  ILU_C_ImplicitArgs ia = _ILU_C_CurrentContext();
  if (ia == ILU_NIL) return ILU_NIL;
  return (ia->ilu_passport);
}

ilu_boolean
  _ILU_C_SetCallerContext (ilu_Passport val)
{
  ILU_C_ImplicitArgs ia = _ILU_C_CurrentContext();
  if (ia == ILU_NIL) return ilu_FALSE;
  ia->ilu_caller = val;
  return ilu_TRUE;
}

ilu_Passport
  ILU_C_CallerIdentity (void)
{
  ILU_C_ImplicitArgs ia = _ILU_C_CurrentContext();
  if (ia == ILU_NIL) return ILU_NIL;
  if (ia->ilu_caller != ILU_NIL)
    return ia->ilu_caller;
  else
    return ia->ilu_passport;
}

#ifdef ILU_C_TIMING_STATISTICS
static ilu_boolean _iluc_GatherStats = ilu_FALSE;
static ilu_cardinal _iluc_NSynchCalls = 0;
static ilu_cardinal _iluc_NAsynchCalls = 0;

/* from before ilu_FullStartCall to after ilu_FinishCall */
static ILU_C_CallStats _iluc_CallTotalTime = { 0, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0,0} };

/* from after ilu_FinishRequest to after ilu_FullGetReply */
static ILU_C_CallStats _iluc_CallLatency = { 0, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0} };

static void AddStat(ILU_C_CallStatSum * ss, double x, ilu_boolean first)
{
  if (first)
    ss->minX = ss->maxX = x;
  else {
    if (x > ss->maxX)
      ss->maxX = x;
    if (x < ss->minX)
      ss->minX = x;
  }
  ss->sumX += x;
  ss->sumXX += x * x;
  return;
}

void ILU_C_SetCallStats (ilu_boolean on_off,
			 ilu_boolean clear)
{
  if (clear) {
    _iluc_NAsynchCalls = 0;
    _iluc_NSynchCalls = 0;
    memset((void *) &_iluc_CallLatency, 0, sizeof(_iluc_CallLatency));
    memset((void *) &_iluc_CallTotalTime, 0, sizeof(_iluc_CallTotalTime));
  };
  _iluc_GatherStats = on_off;
}

void ILU_C_GetCallStats (ilu_cardinal *n_synch_calls,
			 ilu_cardinal *n_asynch_calls,
			 ILU_C_CallStats *total_call,
			 ILU_C_CallStats *call_latency)
{
  *n_synch_calls = _iluc_NSynchCalls;
  *n_asynch_calls = _iluc_NAsynchCalls;
  *total_call = _iluc_CallTotalTime;
  *call_latency = _iluc_CallLatency;
}

#endif /* def ILU_C_TIMING_STATISTICS */

static ILU_C_COMPLETIONSTATUS
  ILUCompletedToCORBACompleted (ilu_Completion c)
{
  switch (c) {
  case ILU_COMPLETED_NO:
    return CORBA_COMPLETED_NO;
  case ILU_COMPLETED_YES:
    return CORBA_COMPLETED_YES;
  case ILU_COMPLETED_MAYBE:
    return CORBA_COMPLETED_MAYBE;
  default:
    _ilu_Assert(0,"Bad value in ilu_Completion");
  }
}

/*
 * _ILU_C_VectorCall
 *
 * This routine replaces _ILU_C_GenericCall.  It builds on the type information
 * stored in typecodes to marshal and unmarshal parameters and exceptions.
 */
void
  _ILU_C_VectorCall (ilu_Class class, ilu_Method method,
		     _ILU_C_ExceptionDescription *evec,
		     ILU_C_Object *discriminant,
		     ILU_C_ENVIRONMENT *status,
		     ilu_cardinal nparms,
		     _ILU_C_ParmDesc *parms)
{
  ilu_Call_s		call_s;
  ilu_Call		call = &call_s;
  ilu_Error		err = ILU_INIT_NO_ERR;
  ilu_cardinal		size = 0, i;
  ilu_Connection	newconn = ILU_NIL;
  ilu_boolean		needs_sizing;
  ilu_Object		kobj;
  ilu_Serializer	si = ILU_NIL;
  ilu_Pipeline		pl = ILU_NIL;
  ilu_Passport		pp = ILU_NIL;
  ILU_C_Batcher		batcher = ILU_NIL;
  ILU_C_ImplicitArgs	ia;
  ILU_C_InterruptHandle intH = ILU_NIL;
  ilu_Completion	completed = ILU_COMPLETED_NO;
  ilu_ProtocolException protocolErr = ilu_ProtocolException_Success;
#ifdef ILU_C_TIMING_STATISTICS
  struct rusage		begin_rusage, end_rusage, latency_start_rusage, latency_end_rusage;
  struct timeval	begin_timeofday, end_timeofday, latency_start_timeofday, latency_end_timeofday;
  static double		tfactor = 1000.0 / 60.0;
#endif /* def ILU_C_TIMING_STATISTICS */

  ILU_C_SET_SUCCESSFUL(status);

  if ((ia = _ILU_C_CurrentContext()) != ILU_NIL) {
    si = ia->ilu_serializer;
    pl = ia->ilu_pipeline;
    pp = ia->ilu_passport;
    batcher = ia->ilu_batcher;
  };
    
#ifdef ILU_C_TIMING_STATISTICS

  if (_iluc_GatherStats) {
    if (method->me_asynchronous)
      _iluc_NAsynchCalls += 1;
    else
      _iluc_NSynchCalls += 1;
    gettimeofday(&begin_timeofday, NULL);
    getrusage(RUSAGE_SELF, &begin_rusage);
  };

#endif /* def ILU_C_TIMING_STATISTICS */

  (void) ilu_FullStartCall(call, discriminant->server->ilucs_ks, class,
			   method, MyLangIdx(), pp, si, pl, &newconn,
			   &err);
  if (newconn != ILU_NIL)
    (*Fork) (MonitorOutgoingConnection, newconn);
  if (ILU_ERRNOK(err))
    goto kerrWithoutCall;
  intH = discriminant->interruptH;
  if (intH != ILU_NIL)
    if (!AddCallToIH(call, intH)) {
      ILU_C_RAISE_SYSTEM(status, NO_MEMORY, 0, NO);
      goto SErr;
    }

  do { /* call until we don't get a transient/retry error */
    /* (*status) is success, (err) is success, (p..Err) is Success */

    needs_sizing = ilu_CallNeedsSizing(call);

    /* figure size of discriminant */
    if ((!needs_sizing) || _ILU_C_IsSingleton(class))
      size = 0;
    else {
      if ((kobj = _ILU_C_KernelObjOfObj(discriminant)) == ILU_NIL) {
	ILU_C_RAISE_SYSTEM(status, INV_OBJREF, 0, NO);
	goto SErr;
      }
      size = ilu_SizeOfObjectID(call, kobj, ilu_TRUE, class, &err);
      ilu_ExitServer(discriminant->server->ilucs_ks,
		     ilu_ClassOfObject(kobj));
      if (ILU_ERRNOK(err))
	goto KErr;
    }

    if (needs_sizing) {
      for (i = 0;  i < nparms;  i++) {
	if (parms[i].parm_in) {
	  if (parms[i].parm_needs_dereference)
	    size += _ILU_C_SizeOfValue (parms[i].parm_type, call,
					*((void **) parms[i].parm_val),
					&err);
	  else
	    size += _ILU_C_SizeOfValue (parms[i].parm_type, call,
					parms[i].parm_val, &err);
	  if (ILU_ERRNOK(err)) goto KErr;
	}
      }
    }

    if (!ilu_StartRequest(call, size, &err))
      goto KErr;

    /* marshall discriminant */

    if (!_ILU_C_IsSingleton(class))
      {
	if ((kobj = _ILU_C_KernelObjOfObj (discriminant)) == ILU_NIL)
	  {
	    ILU_C_RAISE_SYSTEM(status, INV_OBJREF, 0, NO);
	    goto SErr;
	  }
	ilu_OutputObjectID(call, kobj, ilu_TRUE, class, &err);
	if (ILU_ERRNOK(err))
	  goto KErr;
      }
  
    /* marshall other arguments */

    for (i = 0;  i < nparms;  i++) {
      if (parms[i].parm_in) {
	if (parms[i].parm_needs_dereference)
	  _ILU_C_OutputValue (parms[i].parm_type, call,
			      *((void **)parms[i].parm_val), &err);
	else
	  _ILU_C_OutputValue (parms[i].parm_type, call,
			      parms[i].parm_val, &err);
	if (ILU_ERRNOK(err)) goto KErr;
      }
    }

    /* finish the request */

    completed = ILU_COMPLETED_MAYBE;
    if (!ilu_FullFinishRequest(call, batcher, &err))
      goto KErr;

    /* is there a reply? */

    if (! method->me_asynchronous)
      {
#ifdef ILU_C_TIMING_STATISTICS
	if (_iluc_GatherStats && (!method->me_asynchronous)) {
	  gettimeofday(&latency_start_timeofday, NULL);
	  getrusage(RUSAGE_SELF, &latency_start_rusage);
	};
#endif /* def ILU_C_TIMING_STATISTICS */

	protocolErr = ilu_FullGetReply(call, &i, &completed, &newconn,
				       &err);
	if (newconn != ILU_NIL)
	  (*Fork) (MonitorOutgoingConnection, newconn);
      }
  } while (ILU_ERRNOK(err) &&
	   (err.ilu_type == ILU_ERRTYP(transient)) &&
	   (ILU_ERRSEL(transient,err).minor == ilu_tm_retry));

  /** (*status) is success;
      (err) is failure iff (p..Err) is .._Not;
      asynch => ((err) is success && (p..Err) is Success) */
  if (!method->me_asynchronous) {

#ifdef ILU_C_TIMING_STATISTICS
    if (_iluc_GatherStats && (!method->me_asynchronous)) {
      gettimeofday(&latency_end_timeofday, NULL);
      getrusage(RUSAGE_SELF, &latency_end_rusage);
      _iluc_CallLatency.ncalls += 1;
      AddStat(&_iluc_CallLatency.total,
	      (double) ((latency_end_timeofday.tv_sec * 1.0E6 + latency_end_timeofday.tv_usec)
	       - (latency_start_timeofday.tv_sec * 1.0E6 + latency_start_timeofday.tv_usec)),
	      _iluc_CallLatency.ncalls == 1);
      AddStat(&_iluc_CallLatency.user,
	      (double) ((latency_end_rusage.ru_utime.tv_sec * 1.0E6 + latency_end_rusage.ru_utime.tv_usec)
	       - (latency_start_rusage.ru_utime.tv_sec * 1.0E6 + latency_start_rusage.ru_utime.tv_usec)),
	      _iluc_CallLatency.ncalls == 1);
      AddStat(&_iluc_CallLatency.system,
	      (double) ((latency_end_rusage.ru_stime.tv_sec * 1.0E6 + latency_end_rusage.ru_stime.tv_usec)
	       - (latency_start_rusage.ru_stime.tv_sec * 1.0E6 + latency_start_rusage.ru_stime.tv_usec)),
	       _iluc_CallLatency.ncalls == 1);
    };
#endif /* def ILU_C_TIMING_STATISTICS */

    if (protocolErr == ilu_ProtocolException_Success) {
      /*
       * check to see if the user signalled Success (exceptionIndex
       * == 0)
       */
      if (i == 0) {		/* i is the exception index.  A
				 * value of 0 means "no exception". */
	status->returnCode = ILU_NIL;
	status->_major = ILU_C_NO_EXCEPTION;

	/* read in any return results */

	for (i = 0; i < nparms; i++) {
	  if (parms[i].parm_out) {
	    if (parms[i].parm_in && (parms[i].parm_type->freeFn != 0)) {
	      (*parms[i].parm_type->freeFn) (parms[i].parm_val);
	    }
	    if (parms[i].parm_needs_assignment)
	      *((void **) (parms[i].parm_val)) =
		_ILU_C_InputValue(parms[i].parm_type, call,
				  ILU_NIL, &err);
	    else
	      _ILU_C_InputValue(parms[i].parm_type, call,
				parms[i].parm_val, &err);
	    if (ILU_ERRNOK(err))
	      goto KErr;
	  }
	}
      } else
	/* indicates user signalled an expected exception */
      {
	if (!_ILU_C_CatchException(call, method, evec, status, i, &err))
	  goto KSErr;
      }
      /* (*status) is from callee; err is success; p..Err is Success */
      if (!ilu_ReplyRead(call, &err))
	goto KSErr;
    } else {
      /** (*status) is success;
	  protocolErr is not .._Success;
	  (err) is local failure iff protocolErr is .._Not */
      _ILU_C_SetProtocolError(status, protocolErr);
      /* (*status) is callee's; (err) is local; p..Err is irrelevant */
      goto KSErr;
    }
  } else {
    /* (*status) is success; (err) is success; (p..Err) is Success */
  }
  goto KSErr;

 KErr: /* (*status) is success; err is local exn; p..Err is irrelevant */
 KSErr: /* (*status) is from callee; err is local; p..Err is irrelevant */

  ilu_FinishCall(call, &err);
  if (ILU_ERRNOK(err)) {
    CORBA_exception_free(status);
    _ILU_C_ConvertError(status, &err, ILUCompletedToCORBACompleted(completed));
  }
  goto the_end;

 SErr: /* (*status) is local exn; err is success; p..Err is irrelevant */
 
  ilu_FinishCall(call, &err);
  if (ILU_C_SUCCESSFUL(status))
    _ILU_C_ConvertError(status, &err, ILUCompletedToCORBACompleted(completed));
  else
    ILU_HANDLED(err);
  goto the_end;

 kerrWithoutCall:

  _ILU_C_ConvertError (status, &err, ILUCompletedToCORBACompleted(completed));
  goto the_end;

 the_end:
  
#ifdef ILU_C_TIMING_STATISTICS
  if (_iluc_GatherStats) {
    gettimeofday(&end_timeofday, NULL);
    getrusage(RUSAGE_SELF, &end_rusage);
    _iluc_CallTotalTime.ncalls += 1;
    AddStat(&_iluc_CallTotalTime.total,
	    ((end_timeofday.tv_sec * 1.0E6 + end_timeofday.tv_usec)
	     - (begin_timeofday.tv_sec * 1.0E6 + begin_timeofday.tv_usec)),
	    _iluc_CallTotalTime.ncalls == 1);
    AddStat(&_iluc_CallTotalTime.user,
	    ((end_rusage.ru_utime.tv_sec * 1.0E6 + end_rusage.ru_utime.tv_usec)
	     - (begin_rusage.ru_utime.tv_sec * 1.0E6 + begin_rusage.ru_utime.tv_usec)),
	    _iluc_CallTotalTime.ncalls == 1);
    AddStat(&_iluc_CallTotalTime.system,
	    ((end_rusage.ru_stime.tv_sec * 1.0E6 + end_rusage.ru_stime.tv_usec)
	     - (begin_rusage.ru_stime.tv_sec * 1.0E6 + begin_rusage.ru_stime.tv_usec)),
	    _iluc_CallTotalTime.ncalls == 1);
  };
#endif /* def ILU_C_TIMING_STATISTICS */

  if (intH != ILU_NIL)
    RemCallFromIH(call, intH);
  return;
}

#ifdef ADD_VARIANT_SUPPORT

static ilu_Pickle			/* OUT, PASS */
  _ILU_C_AnyToPickle (CORBA_any *any,	/* IN, RETAIN */
		      ilu_Error *err)
{
  ilu_Pickle p;

  p.pi_len = 0;
  p.pi_bytes = ILU_NIL;
  if (any->_type == ILU_NIL) {	/* actually a pickle, just copy it */
    p.pi_bytes = ilu_MallocE(any->_pickle.pi_len, err);
    if (ILU_ERRNOK(*err)) return p;
    memcpy((void *) p.pi_bytes, (void *) any->_pickle.pi_bytes,
	   any->_pickle.pi_len);
    p.pi_len = any->_pickle.pi_len;
  } else {			/* need to marshal a pickle */
    ilu_Call_s call;
    ilu_cardinal size;
    ilu_StartPickle (&call, ILU_NIL, err);
    if (ILU_ERRNOK(*err)) return p;
    size = _ILU_C_SizeOfValue (any->_type, &call, any->_value, err);
    if (ILU_ERRNOK(*err)) return p;
    ilu_WritePickle (&call, size, any->_type->type_id, err);
    if (ILU_ERRNOK(*err)) return p;
    _ILU_C_OutputValue (any->_type, &call, any->_value, err);
    if (ILU_ERRNOK(*err)) return p;
    ilu_EndPickle (&call, &p, err);
  }
  return p;
}

static CORBA_any *
  _ILU_C_PickleToAny (ilu_Pickle p,	/* IN, PASS */
		      CORBA_any *any,	/* INOUT, RETAIN, MODIFY */
		      ilu_Error *err)
{
  ilu_string type_id;
  CORBA_TypeCode tc = ILU_NIL;
  ilu_TypeKind tk;
  ilu_string *types = ILU_NIL;
  ilu_cardinal ntypes, i;

  tk = ilu_PickleTypeKind(p, err);
  if (ILU_ERRNOK(*err)) {
    ILU_HANDLED(*err);
    tk = ilu_byte_tk;
  };
  if ((tk == ilu_record_tk) || (tk == ilu_object_tk)) {
    if (!ilu_PickleTypes (p, &types, &ntypes, err))
      return ILU_NIL;
  } else {
    type_id = ilu_PickleType (p, err);
    if (ILU_ERRNOK(*err)) return ILU_NIL;
    types = &type_id;
    ntypes = 1;
  }
  for (i = 0;  i < ntypes;  i++) {
    if ((tc = _ILU_C_LookupIoFns(types[i])) != ILU_NIL)
      break;
  };
  if ((tk == ilu_record_tk) || (tk == ilu_object_tk)) {
    for (i = 0;  i < ntypes;  i++)
      ilu_free(types[i]);
    ilu_free(types);
  }
  if (tc == ILU_NIL) {	/* this type unknown in this LSR */
    ILU_CLER(*err);
    any->_type = ILU_NIL;
    any->_value = ILU_NIL;
    any->_pickle.pi_len = p.pi_len;	/* return input pickle, but in `any' */
    any->_pickle.pi_bytes = p.pi_bytes;
    return any;
  } else {
    ilu_Call_s call;
    void *val = ILU_NIL;
    ilu_Pickle p2;

    ilu_StartPickle (&call, ILU_NIL, err);
    if (ILU_ERRNOK(*err)) return ILU_NIL;
    if (!ilu_ReadPickle(&call, p, err)) return ILU_NIL;
    val = _ILU_C_InputValue (tc, &call, ILU_NIL, err);
    if (ILU_ERRNOK(*err)) return ILU_NIL;
    ilu_EndPickle(&call, &p2, err);
    if (ILU_ERRNOK(*err)) {
      if (val != ILU_NIL && tc->freeFn != 0)
	(tc->freeFn) (val);
      return ILU_NIL;
    }
    ILU_CLER(*err);
    ilu_free(p2.pi_bytes);	/* free input pickle */
    any->_type = tc;
    any->_value = val;
    any->_pickle.pi_len = 0;
    any->_pickle.pi_bytes = ILU_NIL;
    return any;
  }
}

ilu_string
  ILU_C_Any_TypeID (CORBA_any *val, ILU_C_ENVIRONMENT *env)
{
  ilu_string type_id;
  ilu_Error lerr;

  if ((val->_type != ILU_NIL) && (val->_pickle.pi_bytes == ILU_NIL)) {
    val->_pickle = _ILU_C_AnyToPickle(val, &lerr);
    _ILU_C_ConvertError(env, &lerr, (ILU_ERROK(lerr) ? CORBA_COMPLETED_YES : CORBA_COMPLETED_NO));
    if (!ILU_C_SUCCESSFUL(env)) return ILU_NIL;
  }
  type_id = ilu_PickleType (val->_pickle, &lerr);
  _ILU_C_ConvertError(env, &lerr, (ILU_ERROK(lerr) ? CORBA_COMPLETED_YES : CORBA_COMPLETED_NO));
  if (!ILU_C_SUCCESSFUL(env)) return ILU_NIL;
  type_id = ilu_StrdupE(type_id, &lerr);
  _ILU_C_ConvertError(env, &lerr, (ILU_ERROK(lerr) ? CORBA_COMPLETED_YES : CORBA_COMPLETED_NO));
  return type_id;
}

CORBA_TypeCode
  ILU_C_Any_TypeCode (CORBA_any *val, ILU_C_ENVIRONMENT *env)
{
  ilu_string type_id;
  CORBA_TypeCode tc;
  ilu_Error lerr;

  if ((val->_type != ILU_NIL) && (val->_pickle.pi_bytes == ILU_NIL)) {
    val->_pickle = _ILU_C_AnyToPickle(val, &lerr);
    _ILU_C_ConvertError(env, &lerr, (ILU_ERROK(lerr) ? CORBA_COMPLETED_YES : CORBA_COMPLETED_NO));
    if (!ILU_C_SUCCESSFUL(env)) return ILU_NIL;
  }
  type_id = ilu_PickleType (val->_pickle, &lerr);
  _ILU_C_ConvertError(env, &lerr, (ILU_ERROK(lerr) ? CORBA_COMPLETED_YES : CORBA_COMPLETED_NO));
  if (!ILU_C_SUCCESSFUL(env)) return ILU_NIL;
  tc = _ILU_C_LookupIoFns(type_id);
  return tc;
}

void *
  ILU_C_Any_Value (CORBA_any *val, ILU_C_ENVIRONMENT *env)
{
  ilu_string type_id;
  CORBA_TypeCode tc;
  ilu_Error lerr;
  void *v = ILU_NIL;

  if ((val->_type != ILU_NIL) && (val->_pickle.pi_bytes == ILU_NIL)) {
    val->_pickle = _ILU_C_AnyToPickle(val, &lerr);
    _ILU_C_ConvertError(env, &lerr, (ILU_ERROK(lerr) ? CORBA_COMPLETED_YES : CORBA_COMPLETED_NO));
    if (!ILU_C_SUCCESSFUL(env)) return ILU_NIL;
  }
  type_id = ilu_PickleType (val->_pickle, &lerr);
  _ILU_C_ConvertError(env, &lerr, (ILU_ERROK(lerr) ? CORBA_COMPLETED_YES : CORBA_COMPLETED_NO));
  if (!ILU_C_SUCCESSFUL(env)) return ILU_NIL;
  tc = _ILU_C_LookupIoFns(type_id);
  if (tc != ILU_NIL) {	/* this type unknown in this LSR */
    ilu_Call_s call;
    ilu_Pickle p2;

    ilu_StartPickle (&call, ILU_NIL, &lerr);
    if (ILU_ERRNOK(lerr)) {
      _ILU_C_ConvertError(env, &lerr, (ILU_ERROK(lerr) ? CORBA_COMPLETED_YES : CORBA_COMPLETED_NO));
      return ILU_NIL;
    };
    if (!ilu_ReadPickle(&call, val->_pickle, &lerr)) {
      _ILU_C_ConvertError(env, &lerr, (ILU_ERROK(lerr) ? CORBA_COMPLETED_YES : CORBA_COMPLETED_NO));
      return ILU_NIL;
    };
    v = _ILU_C_InputValue (tc, &call, ILU_NIL, &lerr);
    if (ILU_ERRNOK(lerr)) {
      _ILU_C_ConvertError(env, &lerr, (ILU_ERROK(lerr) ? CORBA_COMPLETED_YES : CORBA_COMPLETED_NO));
      return ILU_NIL;
    };
    ilu_EndPickle(&call, &p2, &lerr);
    if (ILU_ERRNOK(lerr)) {
      _ILU_C_ConvertError(env, &lerr, (ILU_ERROK(lerr) ? CORBA_COMPLETED_YES : CORBA_COMPLETED_NO));
      if (v != ILU_NIL && tc->freeFn != 0)
	(tc->freeFn) (val);
      return ILU_NIL;
    }
  }
  return v;
}

static CORBA_any *
  Any_SetValue (CORBA_any *the_any, ilu_boolean initted, CORBA_TypeCode tc, void *c_val, ILU_C_ENVIRONMENT *e)
{
  CORBA_any *n;
  ilu_Error lerr;
  ilu_Call_s call;
  ilu_cardinal size;


  if (the_any != ILU_NIL) {
    if (initted)
      CORBA_any__Free(the_any);
    n = the_any;
  } else {
    n = (CORBA_any *) ilu_MallocE(sizeof(CORBA_any), &lerr);
    if (ILU_ERRNOK(lerr)) {
      _ILU_C_ConvertError(e, &lerr, (ILU_ERROK(lerr) ? CORBA_COMPLETED_YES : CORBA_COMPLETED_NO));
      return ILU_NIL;
    }
  }
  n->_type = ILU_NIL;
  n->_value = ILU_NIL;
  n->_pickle.pi_len = 0;
  n->_pickle.pi_bytes = ILU_NIL;
  ilu_StartPickle (&call, ILU_NIL, &lerr);
  if (ILU_ERRNOK(lerr)) goto errout;
  size = _ILU_C_SizeOfValue (tc, &call, c_val, &lerr);
  ilu_WritePickle (&call, size, tc->type_id, &lerr);
  if (ILU_ERRNOK(lerr)) goto errout;
  _ILU_C_OutputValue (tc, &call, c_val, &lerr);
  if (ILU_ERRNOK(lerr)) goto errout;
  ilu_EndPickle (&call, &n->_pickle, &lerr);
  if (ILU_ERRNOK(lerr)) goto errout;
  return n;

 errout:
  _ILU_C_ConvertError(e, &lerr, (ILU_ERROK(lerr) ? CORBA_COMPLETED_YES : CORBA_COMPLETED_NO));
  if (the_any == ILU_NIL)
    ilu_free(n);
  return ILU_NIL;
}

CORBA_any *
  ILU_C_Any_Create (CORBA_TypeCode tc, void *c_val, ILU_C_ENVIRONMENT *e)
{
  return (Any_SetValue (ILU_NIL, ilu_FALSE, tc, c_val, e));
}

CORBA_any *
  ILU_C_Any_Init (CORBA_any *any, CORBA_TypeCode tc, void *c_val, ILU_C_ENVIRONMENT *e)
{
  return (Any_SetValue (any, ilu_FALSE, tc, c_val, e));
}

CORBA_any *
  ILU_C_Any_ResetValue (CORBA_any *any, CORBA_TypeCode tc, void *c_val, ILU_C_ENVIRONMENT *e)
{
  return (Any_SetValue (ILU_NIL, ilu_TRUE, tc, c_val, e));
}

CORBA_any* ILU_C_Any_Duplicate (CORBA_any *a, CORBA_Environment *e)
{
  CORBA_any *r;
  ilu_Error lerr;

  if ((a->_type != ILU_NIL) && (a->_pickle.pi_bytes == ILU_NIL)) {
    a->_pickle = _ILU_C_AnyToPickle(a, &lerr);
    _ILU_C_ConvertError(e, &lerr, (ILU_ERROK(lerr) ? CORBA_COMPLETED_YES : CORBA_COMPLETED_NO));
    if (!ILU_C_SUCCESSFUL(e)) return ILU_NIL;
  }
  r = ilu_MallocE(sizeof(CORBA_any), &lerr);
  _ILU_C_ConvertError(e, &lerr, (ILU_ERROK(lerr) ? CORBA_COMPLETED_YES : CORBA_COMPLETED_NO));
  if (!ILU_C_SUCCESSFUL(e)) return ILU_NIL;
  r->_type = ILU_NIL;
  r->_value = ILU_NIL;
  r->_pickle.pi_len = a->_pickle.pi_len;
  r->_pickle.pi_bytes = ilu_MallocE(a->_pickle.pi_len, &lerr);
  if (ILU_ERRNOK(lerr)) {
    _ILU_C_ConvertError(e, &lerr, (ILU_ERROK(lerr) ? CORBA_COMPLETED_YES : CORBA_COMPLETED_NO));
    ilu_free(r);
    return ILU_NIL;
  };
  memcpy((void *) r->_pickle.pi_bytes, (void *) a->_pickle.pi_bytes,
	 a->_pickle.pi_len);
  return r;
}

void ILU_C_Any_FreeStorage (CORBA_any *a, CORBA_Environment *e)
{
  CORBA_any__Free(a);
  ILU_C_SET_SUCCESSFUL(e);
}

void _CORBA_any__Output (ilu_Call _call, CORBA_any* _val, ilu_Error *_err)
{
  ilu_Pickle p;

  if (_val == NULL)
    return;

  if (_val->_type == ILU_NIL) {
    ilu_OutputPickle (_call, _val->_pickle, ILU_NIL, _err);
  } else {
    p = _ILU_C_AnyToPickle (_val, _err);
    if (ILU_ERRNOK(*_err)) return;
    ilu_OutputPickle(_call, p, ILU_NIL, _err);
    ilu_free(p.pi_bytes);
  }
  return;
}

ilu_cardinal _CORBA_any__SizeOf (ilu_Call _call, CORBA_any* _val, ilu_Error *_err)
{
  ilu_Pickle p;
  ilu_cardinal size = 0;

  if (_val == NULL)
    return size;

  if (_val->_type == ILU_NIL) {
    size = ilu_SizeOfPickle (_call, _val->_pickle, ILU_NIL, _err);
  } else {
    p = _ILU_C_AnyToPickle (_val, _err);
    if (ILU_ERROK(*_err)) {
      size = ilu_SizeOfPickle(_call, p, ILU_NIL, _err);
      ilu_free(p.pi_bytes);
    }
  }

  return (ILU_ERROK(*_err) ? size : 0);
}

/* To be CORBA-compliant, when a pickled value is read from the wire,
   it must be unpickled into a C value.  However, we provide the following
   attribute which may be set to prevent this, so that high efficency
   apps which don't need to unpickle don't have to.  */

ilu_boolean ILU_C_AutomaticUnpickling = ilu_FALSE;

CORBA_any* _CORBA_any__Input (ilu_Call _call, CORBA_any* _ref, ilu_Error *_err)
{
  CORBA_any* _val;

  if (_ref != NULL)
    _val = _ref;
  else
    _val = (CORBA_any*) ilu_MallocE(sizeof(CORBA_any), _err);
  _val->_type = ILU_NIL;
  _val->_value = ILU_NIL;
  _val->_pickle.pi_len = 0;
  _val->_pickle.pi_bytes = ILU_NIL;
  ilu_InputPickle (_call, &_val->_pickle, ILU_NIL, _err);
  if (ILU_ERRNOK(*_err)) {
    if (_val != _ref) ilu_free(_val);
    return ILU_NIL;
  } else {
    if (ILU_C_AutomaticUnpickling) {
      _ILU_C_PickleToAny (_val->_pickle, _val, _err);
      if (ILU_ERRNOK(*_err)) {
	if (_val != _ref) ilu_free(_val);
	return ILU_NIL;
      }
    }
    return _val;
  }
}

void CORBA_any__Free (CORBA_any* _val)
{
  /* What you put in the freeRoutine member of a CORBA_Environment for an exception parameterized by a CORBA_any */
  /* frees allocated storage inside _val (if any), but does not free(_val) */

  if (_val->_type != ILU_NIL) {
    if (_val->_value != ILU_NIL) {
      if (_val->_type->freeFn != 0)
	(_val->_type->freeFn) (_val->_value);
      ilu_free(_val->_value);
    }
  } else {
    ilu_free(_val->_pickle.pi_bytes);
  }
}

CORBA_any *CORBA_sequence_CORBA_any_allocbuf (CORBA_unsigned_long _count)
{
  CORBA_any *_p;
  CORBA_unsigned_long _size = sizeof(CORBA_any) * _count;

  if ((_p = (CORBA_any *) ilu_malloc(_size)) == ILU_NIL)
    { _ILU_C_MallocFailure(_size); return 0; }
  else
    { memset((void *) _p, 0, _size);  return _p; }
}

CORBA_any* CORBA_any_alloc ()
{
  return ((CORBA_any*) CORBA_sequence_CORBA_any_allocbuf(1));
}

#if defined(ADD_TYPE_REGISTRATION_SUPPORT)
#if defined(SUNRPC_PROTOCOL)
  ILU_PUBLIC ilu_boolean _ilu_sunrpc_CanMoveAsBlock (ilu_Call, ilu_LanguageIndex, ilu_Type, ilu_cardinal, ilu_cardinal *, ilu_Error *);
#endif
#if defined(W3NG_PROTOCOL)
  ILU_PUBLIC ilu_boolean _ilu_w3ng_CanMoveAsBlock (ilu_Call, ilu_LanguageIndex, ilu_Type, ilu_cardinal, ilu_cardinal *, ilu_Error *);
#endif
#if defined(IIOP_PROTOCOL)
  ILU_PUBLIC ilu_boolean _ilu_IIOP_CanMoveAsBlock (ilu_Call, ilu_LanguageIndex, ilu_Type, ilu_cardinal, ilu_cardinal *, ilu_Error *);
  ILU_PUBLIC  ilu_cardinal _ilu_IIOP_AlignStream (ilu_Call, ilu_cardinal, ilu_Error *);
#endif
#endif

ilu_boolean
  _ILU_C_CanMoveAsBlock (ilu_Call call, ilu_Type t, ilu_cardinal m, ilu_cardinal *alignmentCode, _ILU_C_AlignmentProc *alignmentProc, ilu_Error *err)
{
#if defined(ADD_TYPE_REGISTRATION_SUPPORT)
  *alignmentProc = NULLFN;
#if defined(SUNRPC_PROTOCOL)
  if (_ilu_sunrpc_CanMoveAsBlock (call, _ILU_C_LanguageIndex, t,
				  8000, alignmentCode, err))
    return ilu_TRUE;
  if (ILU_ERRNOK(*err)) return ilu_FALSE;
#endif
#if defined(IIOP_PROTOCOL)
  if (_ilu_IIOP_CanMoveAsBlock (call, _ILU_C_LanguageIndex, t,
				8000, alignmentCode, err)) {
    *alignmentProc = _ilu_IIOP_AlignStream;
    return ilu_TRUE;
  }
  if (ILU_ERRNOK(*err)) return ilu_FALSE;
#endif
#if defined(W3NG_PROTOCOL)
  if (_ilu_w3ng_CanMoveAsBlock (call, _ILU_C_LanguageIndex, t,
				8000, alignmentCode, err))
    return ilu_TRUE;
  if (ILU_ERRNOK(*err)) return ilu_FALSE;
#endif
#endif /* def ADD_TYPE_REGISTRATION_SUPPORT */
  ILU_CLER(*err);
  return ilu_FALSE;
}

ilu_cardinal
  _ILU_C_SizeExtensibleRecord (ilu_Call call, void *val, ilu_Error *err)
{
#ifndef ADD_TYPE_REGISTRATION_SUPPORT
  ilu_DebugPrintf("ILU/C: ** Use of extensible records requires configuration of support for type registration,\n"
		  "       ** which this ILU C LSR library doesn't have!\n");
#else
  struct dummy { ILU_C_IoFnsRegistration __type; };
  CORBA_any temp;
  temp._type = (CORBA_TypeCode) ((struct dummy *) val)->__type;
  temp._value = val;
  return (_CORBA_any__SizeOf(call, &temp, err));
#endif /* ndef ADD_TYPE_REGISTRATION_SUPPORT */
}

void
  _ILU_C_OutputExtensibleRecord (ilu_Call call, void *val, ilu_Error *err)
{
#ifndef ADD_TYPE_REGISTRATION_SUPPORT
  ilu_DebugPrintf("ILU/C: ** Use of extensible records requires configuration of support for type registration,\n"
		  "       ** which this ILU C LSR library doesn't have!\n");
#else
  struct dummy { ILU_C_IoFnsRegistration __type; };
  CORBA_any temp;
  temp._type = (CORBA_TypeCode) ((struct dummy *) val)->__type;
  temp._value = val;
  _CORBA_any__Output(call, &temp, err);
#endif /* ndef ADD_TYPE_REGISTRATION_SUPPORT */
}

void
  _ILU_C_InputExtensibleRecord (ilu_Call call, void **val, ilu_Error *err)
{
#ifndef ADD_TYPE_REGISTRATION_SUPPORT
  ilu_DebugPrintf("ILU/C: ** Use of extensible records requires configuration of support for type registration,\n"
		  "       ** which this ILU C LSR library doesn't have!\n");
#else
  CORBA_any temp;

  ilu_boolean autounpickling;
  autounpickling = ILU_C_AutomaticUnpickling;
  ILU_C_AutomaticUnpickling = ilu_FALSE;
  _CORBA_any__Input(call, &temp, err);
  ILU_C_AutomaticUnpickling = autounpickling;
  if (ILU_ERRNOK(*err)) return;
  /* Check subtype relationship... */
  _ILU_C_PickleToAny (temp._pickle, &temp, err);
  if (ILU_ERRNOK(*err)) {
    CORBA_any__Free(&temp);
    return;
  };
  *val = temp._value;
  ILU_CLER(*err);
#endif /* ndef ADD_TYPE_REGISTRATION_SUPPORT */
}

#endif /* ADD_VARIANT_SUPPORT */

static ilu_boolean
  ValidExceptionID (void *p)
{
  ilu_string s;
  ilu_cardinal len;

  if (p == ILU_NIL)
    return ilu_FALSE;
  for (s = (ilu_string) p, len = 0;  *s != 0;  s++, len++) {
    if (!(isalnum(*s) || (strchr(":/.-_", *s) != NULL)))
      return ilu_FALSE;
  }
  return (len > 0);
}

void
_ILU_C_SendException(ilu_Call call, _ILU_C_ExceptionDescription *evec,
		     ILU_C_ENVIRONMENT * status, ilu_Error * err)
{
  ilu_cardinal    argSize = 0;
  ilu_cardinal    eindex = 0, limit;
  ilu_Method      method = ilu_MethodOfCall(call);
  ilu_ProtocolException pe;

  ILU_CLER(*err);

  switch (status->_major) {

  case CORBA_NO_EXCEPTION:
    return;

  case CORBA_SYSTEM_EXCEPTION:

    if (status->returnCode == ILU_C_STDEX(BAD_PARAM))
      pe = ilu_ProtocolException_GarbageArguments;
    else
      pe = ilu_ProtocolException_Unknown;
    if (!ilu_BeginException(call, -(int) pe, 0, err))
      return;
    break;

  case CORBA_USER_EXCEPTION:

    limit = method->me_exceptionCount;
    for (eindex = 0; eindex < limit; eindex++)
      if (method->me_exceptionVector[eindex] == status->returnCode)
	goto found;
    ilu_DebugPrintf("ILU/C: %s raises unexpected exception %p",
		    ilu_NameOfMethod(method), status->returnCode);
    if (ValidExceptionID(status->returnCode))
      ilu_DebugPrintf (" \"%s\"", status->returnCode);
    ilu_DebugPrintf("!\n");
    (void) ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_bogus_raise, 6);
    return;

found:

#define GENERIC_ESIZE(type,fn) argSize += fn (call, *((type *) status->ptr), err); break

    if (ilu_CallNeedsSizing(call)) {
      argSize = ilu_BeginSizingException(call, eindex + 1, err);
      if (ILU_ERRNOK(*err))
	return;

      switch (evec[eindex].size) {
	/* Integers */

      case 'a':
	GENERIC_ESIZE(ilu_shortinteger, ilu_SizeOfShortInteger);
      case 'b':
	GENERIC_ESIZE(ilu_integer, ilu_SizeOfInteger);
      case 'c':
	GENERIC_ESIZE(ilu_longinteger, ilu_SizeOfLongInteger);

	/* Cardinals */

      case 'd':
	GENERIC_ESIZE(ilu_shortcardinal, ilu_SizeOfShortCardinal);
      case 'e':
	GENERIC_ESIZE(ilu_cardinal, ilu_SizeOfCardinal);
      case 'f':
	GENERIC_ESIZE(ilu_longcardinal, ilu_SizeOfLongCardinal);

	/* Reals */

      case 'g':
	GENERIC_ESIZE(ilu_shortreal, ilu_SizeOfShortReal);
      case 'h':
	GENERIC_ESIZE(ilu_real, ilu_SizeOfReal);
      case 'i':
	GENERIC_ESIZE(ilu_longreal, ilu_SizeOfLongReal);

	/* Characters */

      case 'j':
	GENERIC_ESIZE(ilu_shortcharacter, ilu_SizeOfShortCharacter);
      case 'k':
	GENERIC_ESIZE(ilu_character, ilu_SizeOfCharacter);
	/*
	 * case 'l': GENERIC_ESIZE(ilu_longcharacter,
	 * ilu_SizeOfLongCharacter);
	 */
	/* Byte */

      case 'm':
	GENERIC_ESIZE(ilu_byte, ilu_SizeOfByte);

	/* Boolean */

      case 'n':
	GENERIC_ESIZE(CORBA_boolean, ILU_C_SizeOfBoolean);

	/* Enumeration */
      case 'o':
	argSize += ilu_SizeOfEnum(call, *((ilu_cardinal *) status->ptr),
				  ILU_NIL, err);
	break;

	/* Objects */
      case 'p':
	argSize += _ILU_C_SizeOfObject(call,
				       *(ILU_C_Object **) status->ptr,
				       *(evec[eindex].ed_class),
				       ilu_FALSE, err);
	break;

	/* C String */
      case 'q':
	argSize += ilu_SizeOfEString(call,
		      (ilu_bytes) * ((ilu_string *) (status->ptr)),
			   strlen(*((ilu_string *) (status->ptr))),
				     0,
				     ILU_StringEncoding_latin1,
				     ILU_StringEncoding_latin1,
				     err);
	break;

	/* wchar * string */
      case 'r':
	argSize += _ILU_C_SizeOfWString(call, (CORBA_wchar *) * ((ilu_string *) (status->ptr)),
					_ILU_C_SafeWStrlen((CORBA_wchar *) * ((ilu_string *) (status->ptr))),
					0, err);
	break;

      case 's':	/* optional */
      case 't':	/* reference */
	argSize += (*evec[eindex].sizeFn) (call, *(void **)(status->ptr), err);
	break;

#ifdef ILU_HTTPNG_OBJECTS
      case 'u': /* local object */
	{
	  ILU_C_LocalObjectSizingFn sizer;
	  sizer = (ILU_C_LocalObjectSizingFn) evec[eindex].sizeFn;
	  argSize += (*sizer) (call, *(ILU_C_Object **) (status->ptr),
			       *(evec[eindex].ed_class), err);
	}
	break;					     
#endif

      case 'z':	/* everything else */
	argSize += (*evec[eindex].sizeFn) (call, status->ptr, err);
	break;

      case '*':	/* no associated value */
	break;

      default:
	ILU_ERR_CONS1(internal, err, minor, ilu_im_broken, 0);
	break;
      }
      if (ILU_ERRNOK(*err))
	return;
    }
    _ILU_C_BeginException(call, eindex + 1, argSize, err);
    if (ILU_ERRNOK(*err))
      return;

#define GENERIC_EOUT(type,fn) fn (call, *((type *) status->ptr), err); break

    switch (evec[eindex].size) {
      /* Integers */

    case 'a':
      GENERIC_EOUT(ilu_shortinteger, ilu_OutputShortInteger);
    case 'b':
      GENERIC_EOUT(ilu_integer, ilu_OutputInteger);
    case 'c':
      GENERIC_EOUT(ilu_longinteger, ilu_OutputLongInteger);

      /* Cardinals */

    case 'd':
      GENERIC_EOUT(ilu_shortcardinal, ilu_OutputShortCardinal);
    case 'e':
      GENERIC_EOUT(ilu_cardinal, ilu_OutputCardinal);
    case 'f':
      GENERIC_EOUT(ilu_longcardinal, ilu_OutputLongCardinal);

      /* Reals */

    case 'g':
      GENERIC_EOUT(ilu_shortreal, ilu_OutputShortReal);
    case 'h':
      GENERIC_EOUT(ilu_real, ilu_OutputReal);
    case 'i':
      GENERIC_EOUT(ilu_longreal, ilu_OutputLongReal);

      /* Characters */

    case 'j':
      GENERIC_EOUT(ilu_shortcharacter, ilu_OutputShortCharacter);
    case 'k':
      GENERIC_EOUT(ilu_character, ilu_OutputCharacter);
      /*
       * case 'l': GENERIC_EOUT(ilu_longcharacter,
       * ilu_OutputLongCharacter);
       */
      /* Byte */

    case 'm':
      GENERIC_EOUT(ilu_byte, ilu_OutputByte);

      /* Boolean */

    case 'n':
      GENERIC_EOUT(CORBA_boolean, ILU_C_OutputBoolean);

      /* Enumeration */
    case 'o':
      ilu_OutputEnum(call, *((ilu_cardinal *) status->ptr), ILU_NIL,
		     err);
      break;

      /* Objects */
    case 'p':
      _ILU_C_OutputObject(call, *(ILU_C_Object **) status->ptr,
			  *(evec[eindex].ed_class), ilu_FALSE, err);
      break;

      /* C string */
    case 'q':
      ilu_OutputEString(call,
		      (ilu_bytes) * ((ilu_string *) (status->ptr)),
			strlen(*((ilu_string *) (status->ptr))),
			0, ILU_StringEncoding_latin1,
			ILU_StringEncoding_latin1, err);
      break;

      /* wchar * string */
    case 'r':
      _ILU_C_OutputWString(call, (CORBA_wchar *) * ((ilu_string *) (status->ptr)),
			   _ILU_C_SafeWStrlen((CORBA_wchar *) * ((ilu_string *) (status->ptr))),
			   0, err);
      break;

    case 's':	/* optional */
    case 't':	/* reference */
      (*evec[eindex].outFn) (call, *(void **)(status->ptr), err);
      break;

#ifdef ILU_HTTPNG_OBJECTS

    case 'u':
      {
	ILU_C_LocalObjectOutputFn outer;
	outer = (ILU_C_LocalObjectOutputFn) evec[eindex].outFn;
	(*outer) (call, *(ILU_C_Object **) (status->ptr),
		  *(evec[eindex].ed_class), err);
      }
      break;

#endif /* def ILU_HTTPNG_OBJECTS */

    case 'z':	/* other */
      (*evec[eindex].outFn) (call, status->ptr, err);
      break;

    case '*':	/* no associated value */
      break;

    default:
      ILU_ERR_CONS1(internal, err, minor, ilu_im_broken, 0);
      break;
    }
    break;
    
  default:
    if (!ilu_BeginException(call, -(int) ilu_ProtocolException_Unknown,
			    0, err))
      return;
  }
  if (ILU_ERRNOK(*err))
    return;

  _ILU_C_FinishException(call, err);
  return;
}

void
_ILU_C_FreeException(ilu_Method method, _ILU_C_ExceptionDescription *evec,
		     ILU_C_ENVIRONMENT * status, ilu_Error * err)
{
  ilu_cardinal    eindex = 0, limit;

  ILU_CLER(*err);

  switch (status->_major) {

  case CORBA_NO_EXCEPTION:
    return;

  case CORBA_SYSTEM_EXCEPTION:
    ilu_free(status->ptr);
    return;

  case CORBA_USER_EXCEPTION:
    limit = method->me_exceptionCount;
    for (eindex = 0; eindex < limit; eindex++)
      if (method->me_exceptionVector[eindex] == status->returnCode)
	goto found;
    ilu_DebugPrintf("ILU/C: %s tries to free unexpected exception %p",
		    ilu_NameOfMethod(method), status->returnCode);
    if (ValidExceptionID(status->returnCode))
      ilu_DebugPrintf (" \"%s\"", status->returnCode);
    ilu_DebugPrintf("!\n");
    (void) ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_bogus_raise, 6);
    return;

found:

    if (status->ptr != ILU_NIL) {
      if (evec[eindex].freeFn != (ILU_C_FreeFn) 0)
	(*evec[eindex].freeFn) (status->ptr);
      ilu_free(status->ptr);
    }
    return;

  default:
    _ilu_Assert(ilu_FALSE,
	    "malformed ILU_C_ENVIRONMENT in _ILU_C_FreeException");
  }
  return;
}

ilu_cardinal _ilu_CString__SizeOf (ilu_Call call, ilu_string str, ilu_Error *err)
{
  return (ilu_SizeOfEString(call, (ilu_bytes) str, _ILU_C_SafeStrlen(str), 0,
			    ILU_StringEncoding_latin1, ILU_StringEncoding_latin1, err));
}

void _ilu_CString__Output (ilu_Call call, ilu_string str, ilu_Error *err)
{
  ilu_OutputEString(call, (ilu_bytes) str, _ILU_C_SafeStrlen(str), 0,
		    ILU_StringEncoding_latin1, ILU_StringEncoding_latin1, err);
}

ilu_CString *_ilu_CString__Input (ilu_Call call, ilu_CString *strp, ilu_Error *err)
{
  ilu_cardinal len = 0;
  ilu_string *p;
  ilu_cardinal actual_encoding;	/* never used */

  if (strp == ILU_NIL) {
    p = ilu_MallocE(sizeof(*p), err);
    if (ILU_ERRNOK(*err)) return ILU_NIL;
    *p = ILU_NIL;
  } else {
    p = strp;
  };
  ilu_InputEString(call, (ilu_bytes *) p, &len, 0,
		   ILU_StringEncoding_latin1, &actual_encoding, err);
  if (ILU_ERROK(*err))
    return (p);
  else if (p != strp)
    ilu_free(p);
  return (ILU_NIL);
}

void ilu_CString__Free (ilu_CString *s)
{
  if (*s != ILU_NIL)
    ilu_free(*s);
}

CORBA_unsigned_long ilu_CString_Length (ilu_CString *s)
{
  return _ILU_C_SafeStrlen(*s);
}

CORBA_char * ilu_CString_Nth (ilu_CString *s, CORBA_unsigned_long n)
{
  if (_ILU_C_SafeStrlen(*s) < n)
    return ILU_NIL;
  else
    return (CORBA_char *) (*s + n);
}

void ilu_CString_Every (ilu_CString *s, void (*f)(CORBA_char, void*), void *rock)
{
  CORBA_char *p;
  for (p = *s;  p != 0;  p++) {
    (*f)(*p, rock);
  }
}

void ilu_CString_Append (ilu_CString *s, CORBA_char value)
{
  _ILU_C_ExtendString (s, value, ilu_TRUE);
}

void ilu_CString_Push (ilu_CString *s, CORBA_char value)
{
  _ILU_C_ExtendString (s, value, ilu_FALSE);
}
  
void ilu_CString_Pop (ilu_CString *s, CORBA_char *value_ptr)
{
  _ILU_C_PopString (s, value_ptr);
}

static ilu_boolean _ILU_C_CString_Init (ilu_CString *s, CORBA_unsigned_long len, CORBA_char *values)
{
  ilu_cardinal len2;
  ilu_Error lerr;

  *s = ILU_NIL;
  if (values != ILU_NIL) {
    if (len == 0)
      len2 = _ILU_C_SafeStrlen(values) + 1;
    else
      len2 = len + 1;
    *s = ilu_MallocE(len2, &lerr);
    if (ILU_ERRNOK(lerr)) {
      ILU_HANDLED(lerr);
      return ilu_FALSE;
    }
    strncpy(*s, values, len2 - 1);
    *s[len2 - 1] = 0;
  } else if (len > 0) {
    *s = ilu_MallocE(len + 1, &lerr);
    if (ILU_ERRNOK(lerr)) {
      ILU_HANDLED(lerr);
      return ilu_FALSE;
    }
    *s[0] = 0;
  }
  return ilu_TRUE;
}

void ilu_CString_Init (ilu_CString *s, CORBA_unsigned_long len, CORBA_char *values)
{
  (void) _ILU_C_CString_Init (s, len, values);
}

ilu_CString * ilu_CString_Create (CORBA_unsigned_long len, CORBA_char *values)
{
  ilu_Error lerr;
  ilu_CString *s;

  s = ilu_MallocE(sizeof(*s), &lerr);
  if (ILU_ERRNOK(lerr)) {
    ILU_HANDLED(lerr);
    return ILU_NIL;
  }
  if (_ILU_C_CString_Init (s, len, values))
    return s;
  else {
    ilu_free(s);    
    return ILU_NIL;
  }
}

ilu_CString * ilu_CString__alloc (void)
{
  return ilu_CString_Create(1, ILU_NIL);
}

/* We assume that the wchar charset is straight Unicode.  If the sizeof(wchar_t) is 2,
   and the arch is bigendian, we also assume that it's UCS-2 (ILU_StringEncoding_Unicode_1_1).
   Otherwise, we convert it to ILU_StringEncoding_Unicode_1_1. */

#define SWAP_HALFWORD(a)  ((((a) << 8) & 0xFF00) | \
			   ((((ilu_shortcardinal)(a)) >> 8) & 0x00FF))

ilu_cardinal _ILU_C_SizeOfWString (ilu_Call call, CORBA_wchar *str, ilu_cardinal len, ilu_cardinal limit, ilu_Error *err)
{
  ilu_cardinal i;
  ilu_shortcardinal *p, tmp;

#if (defined(WORDS_BIGENDIAN) && ((SIZEOF_WCHAR_T == 2) || (SIZEOF_WCHAR_T == 0)))
  return (ilu_SizeOfEString(call, (ilu_bytes) str, len * sizeof(CORBA_wchar), limit,
			    ILU_StringEncoding_Unicode_1_1, ILU_StringEncoding_Unicode_1_1, err));
#else
  /* convert it */
  p = ilu_MallocE(len * sizeof(ilu_shortcardinal), err);
  if (ILU_ERRNOK(*err)) return 0;
  for (i = 0;  i < len;  i++) {
    if (str[i] >= 0x10000) {
      ilu_free(p);
      return ILU_ERR_CONS1(imp_limit, err, minor, ilu_ilm_unsupported_charset_encoding, 0);
    };
    tmp = (ilu_shortcardinal) str[i];
#ifdef WORDS_BIGENDIAN
    p[i] = tmp;
#else
    p[i] = SWAP_HALFWORD(tmp);
#endif
  }
  i = ilu_SizeOfEString(call, (ilu_bytes) p, len * sizeof(ilu_shortcardinal), limit,
			ILU_StringEncoding_Unicode_1_1, ILU_StringEncoding_Unicode_1_1, err);
  ilu_free(p);
  return i;
#endif /* (defined(WORDS_BIGENDIAN) && ((SIZEOF_WCHAR_T == 2) || (SIZEOF_WCHAR_T == 0))) */
}

void _ILU_C_OutputWString (ilu_Call call, CORBA_wchar *str, ilu_cardinal len, ilu_cardinal limit, ilu_Error *err)
{
  ilu_cardinal i;
  ilu_shortcardinal *p, tmp;

#if (defined(WORDS_BIGENDIAN) && ((SIZEOF_WCHAR_T == 2) || (SIZEOF_WCHAR_T == 0)))
  ilu_OutputEString(call, (ilu_bytes) str, len * sizeof(CORBA_wchar), limit,
		    ILU_StringEncoding_Unicode_1_1, ILU_StringEncoding_Unicode_1_1, err);
#else
  /* convert it */
  p = ilu_MallocE(len * sizeof(ilu_shortcardinal), err);
  if (ILU_ERRNOK(*err)) return;
  for (i = 0;  i < len;  i++) {
    if (str[i] >= 0x10000) {
      ilu_free(p);
      ILU_ERR_CONS1(imp_limit, err, minor, ilu_ilm_unsupported_charset_encoding, 0);
      return;
    };
    tmp = (ilu_shortcardinal) str[i];
#ifdef WORDS_BIGENDIAN
    p[i] = str[i];
#else
    p[i] = SWAP_HALFWORD(str[i]);
#endif
  }
  ilu_OutputEString(call, (ilu_bytes) p, len * sizeof(ilu_shortcardinal), limit,
		    ILU_StringEncoding_Unicode_1_1, ILU_StringEncoding_Unicode_1_1, err);
  ilu_free(p);
#endif /* (defined(WORDS_BIGENDIAN) && ((SIZEOF_WCHAR_T == 2) || (SIZEOF_WCHAR_T == 0))) */
}

CORBA_wchar *_ILU_C_InputWString (ilu_Call call, CORBA_wchar **strp, ilu_cardinal *len, ilu_cardinal limit, ilu_Error *err)
{
  int i;
  ilu_cardinal len2 = 0;
  ilu_shortcardinal *tmp;
  CORBA_wchar *p2 = ILU_NIL;
  ilu_cardinal actual_encoding;	/* never used */
  ilu_cardinal expected_encoding;

  ilu_InputEString(call, (ilu_bytes *) &tmp, &len2, limit,
		   ILU_StringEncoding_Unicode_1_1, &actual_encoding, err);
  if (ILU_ERRNOK(*err)) return ILU_NIL;
#if defined(WORDS_BIGENDIAN)
  if (sizeof(CORBA_wchar) == 2) {
    if (strp != ILU_NIL)
      *strp = (CORBA_wchar *) tmp;
    /* p2 not bound */
    *len = len2/2;
    return (CORBA_wchar *) tmp;
  };
#endif
  /* convert it */
  if (sizeof(CORBA_wchar) != 2) {
    p2 = (CORBA_wchar *) ilu_MallocE(len2/2 * sizeof(CORBA_wchar) + 1, err);
    if (ILU_ERRNOK(*err)) { ilu_free(tmp); return ILU_NIL; };
  } else {
    p2 = (CORBA_wchar *) tmp;
  }
  len2 = len2/2;
  for (i = 0;  i < len2;  i++) {
#ifdef WORDS_BIGENDIAN
    p2[i] = tmp[i];
#else
    p2[i] = SWAP_HALFWORD(tmp[i]);
#endif
  }
  p2[len2] = 0;
  if ((void *) p2 != (void *) tmp)
    ilu_free(tmp);
  *len = len2;
  if (strp != ILU_NIL)
    *strp = (CORBA_wchar *) p2;
  return p2;
}

ilu_cardinal
  _ILU_C_Enumeration__SizeOf (ilu_Call call, void *eval, ilu_Error *err)
{
  return ilu_SizeOfEnum (call, (ilu_shortcardinal) *((CORBA_enum *)eval), ILU_NIL, err);
}

void
  _ILU_C_Enumeration__Output (ilu_Call call, void *eval, ilu_Error *err)
{
  ilu_OutputEnum (call, (ilu_shortcardinal) *((CORBA_enum *)eval), ILU_NIL, err);
}

void *
  _ILU_C_Enumeration__Input (ilu_Call call, void *eval, ilu_Error *err)
{
  ilu_shortcardinal sval;
  ilu_InputEnum (call, &sval, ILU_NIL, err);
  if (ILU_ERROK(*err)) {
    if (eval != ILU_NIL) {
      *((CORBA_enum *)eval) = sval;
      return (eval);
    } else {
      CORBA_enum *v = (CORBA_enum *) ilu_must_malloc(sizeof(CORBA_enum));
      *v = sval;
      return (void *) v;
    }
  } else
    return ILU_NIL;
}

/*============================================================*/
/*============================================================*/
/*                 Sequence operations                        */
/*============================================================*/
/*============================================================*/

#define Alloc( n, t )   ( t * ) ilu_must_malloc( n * sizeof( t ))
#define Realloc( p, n, t ) ( t * ) ilu_realloc( p, n * sizeof( t ))

void _ILU_C_AppendGeneric (ILU_C_Sequence h, char *p, int sz)
{
  char *ptr;

  /*
   ** place the item pointed to by p
   ** at the end of the sequence
   */

  if (h->_length >= h->_maximum)
    {
      h->_maximum = (h->_maximum + SEQUENCE_INCREMENT);
      if (h->_buffer != ILU_NIL)
	h->_buffer = ilu_realloc(h->_buffer, h->_maximum * sz);
      else
	h->_buffer = ilu_must_malloc(h->_maximum*sz);
    }
  ptr = h->_buffer + (h->_length * sz);
  memcpy (ptr, p, sz);
  h->_length += 1;
}

void _ILU_C_EveryElement(ILU_C_Sequence h, void (*proc)(void *,void *), int sz, void *data)
{
  int i;
  char *p;

  if (!h || h->_length <= 0)
    return;
  for(p = h->_buffer, i = 0; ((unsigned)i) < h->_length; i++, p += sz)
    (*proc)((void *) p, data);
}

void _ILU_C_PopGeneric (ILU_C_Sequence h, char *p, int sz)
{
    char	*ptr;

    /*
    ** return the top element
    ** in the sequence in p then
    ** remove it from the list.
    */

    if ( !h || h->_length <= 0 )
	return;
    memcpy( p, h->_buffer, sz );
    h->_length--;
    ptr = h->_buffer + sz;
    memmove (h->_buffer, ptr, h->_length * sz);
}

void _ILU_C_PushGeneric (ILU_C_Sequence h, char *p, int sz)
{
    int		l = h->_length;
    int		n;
    char	*ptr;

    /*
    ** place the item pointed to by p
    ** at the beginning of the sequence
    */

    h->_length++;
    n = h->_length * sz;
    if ( h->_length > h->_maximum ) {
        if ( h->_buffer )
    	    h->_buffer = Realloc( h->_buffer, n, char );
        else
    	    h->_buffer = Alloc( n, char );
	h->_maximum = h->_length;
    }
    ptr = h->_buffer + sz;

    memmove (ptr, h->_buffer, l * sz);
    memcpy (h->_buffer, p, sz);
}

/*============================================================*/
/*============================================================*/
/*                 GC callback support                        */
/*============================================================*/
/*============================================================*/


/* L1, L2, Main unconstrained (rely on correct calls) */

static _ILU_C_DispatchTableSection GcCallbackDTS[2] = {
  {ILU_NIL	/* will be set to the callback type */ ,
   ILU_NIL	/* no methods defined in ISL */ },
  {ILU_NIL, ILU_NIL}	/* the terminating sentinal */
};

static _ILU_C_Class_struct GcCallback__Class = {
	GcCallbackDTS,
	0	/* no finalization */
};

static int CallbackInited = 0;
static ILU_C_Server cbs = ILU_NIL;
static ILU_C_OBJECT cbo = ILU_NIL;

/*Main invariant holds*/
void _ILU_C_EnsureGcClient(void)
{
  if (CallbackInited || ilu_IsGcClientSet())
    /*
     * we alread initialized, or some other runtime must have
     * already set things up for this process
     */
    return;

  GcCallbackDTS[0].ilucdts_introType = ilu_GetGcCallbackClass();
  cbs = ILU_C_InitializeServer(ILU_NIL, ILU_NIL, ILU_NIL, ILU_NIL,
			       ILU_NIL, ilu_TRUE);
  cbo = ILU_C_CreateTrueObject(&GcCallback__Class,
			       "the-gc-callback", cbs, ILU_NIL,
			       ilu_FALSE);
  if (cbo != ILU_NIL) {
    ilu_EnterServer(cbs->ilucs_ks, MOST_SPECIFIC_ILU_CLASS(cbo));
    ilu_SetGcClient(cbo->iluco_ko);
    ilu_ExitServer(cbs->ilucs_ks, MOST_SPECIFIC_ILU_CLASS(cbo));
  }
  CallbackInited = 1;
  return;
}
