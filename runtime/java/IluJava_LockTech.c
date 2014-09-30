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
/* IluJava_LockTech.c */
/* Chris Jacobi, November 20, 1998 8:37 pm PST */

/*
 */
 
/* $Id: IluJava_LockTech.c,v 1.33 1999/08/03 01:54:27 janssen Exp $$ */
 
/*
 * This file defines the locking primitives used by iluxport.h
 * in terms of what is available in java  (which is sys_api.h)
 *
 * This could of course also be implemented using the real
 * thread package used.  That would certainly be more efficient,
 * probably be easier to implement but NOT portable to different
 * java implementations.
 */


#include "IluJava_Includes.h"
#include "IluJava_Common.h"

#include "IluJava_JMon.h"
#include "IluJava_JOps.h"

#define TRACE_EASY (_ilujava_lockFlag > 0)
#define TRACE_HARD (_ilujava_lockFlag > 1)
#define TRACE_FANATIC (_ilujava_lockFlag > 3)


#define ENABLE_SPECIAL_DEBUGGING_WRONGIDENTIFIER 1
    /* LockTech handles some debug output in a special way because
     * ilu_DebugPrintf actually enters a lock and causes recursion 
     * if invoked from LockTech
     */
   

static void SpecialErrPrint(char * f)
{
    ilujava_SpecialDebugPrintf("$** Ilu_Java BUG: %s \n", f);
    if (_ilujava_iluGenericFlag > 1) {
        _ilujava_PANIC(f);
    }
} /*SpecialErrPrint*/


#ifdef ENABLE_SPECIAL_DEBUGGING
   #define JsTRACE(key,code)    if (key) ilujava_SpecialDebugPrintf code
#else
   #define JsTRACE(key,code)    
#endif

#if defined(useJNIMonitors)
#include <jni.h>
/*required by IluJava_JMon.h*/
static jmethodID METHODID_NOTIFYALL;
static jmethodID METHODID_WAIT;
#endif


/* 
 * Never wait with the META lock hold.
 * Enter META first.
 * Never hold two locks unless one is the META lock.
 *
 * The reason we use separate monitors instead of doing all the waiting
 * with the meta lock is so that a single notify doesn't wake all threads.
 */

typedef struct {
    ilu_string d1;
    ilu_string d2;
    ILUJAVA_SYSTHREAD_stPtr holder;
    ILUJAVA_JMON_PTR mQueue; 
    char * stackWhenAquired;  /*optional; for debugging only*/
} ltj_Mutex_s;


/* queue element */
typedef struct R {
    struct R * prev;
    struct R * next;
    ilu_boolean disabled;
    ILUJAVA_JMON_PTR elQueue;
} ltj_QueueEl_s;


typedef struct {
    ilu_string d1;
    ilu_string d2;
    ltj_QueueEl_s * first;
    ltj_QueueEl_s * last;
} ltj_Condition_s;


static void PrintWereAquired(ltj_Mutex_s * m) 
/* prints where a mutex was aquired, if that information is available */
{
    if (m->stackWhenAquired) {
        ilujava_SpecialDebugPrintf("$ JAVA STACK WHEN LOCK WAS AQUIRED [[%s]]\n\n",
            m->stackWhenAquired);
    }
}


#define MUTEX_FREE 0

#define NO_TIMEOUT -1
    /* Stupid java runtime system uses a different encoding then the 
     * java language which uses 0...
     * Don't actually use this to avoid trouble when the java
     * implementation changes.
     */
#define BIG_TIMEOUT 120000
    /* Use this instead of NO_TIMEOUT to make sure threads
     * can die gracefully when necessary.  Use a loop...
     * (expressed in milliseconds)
     */
#define VERY_BIG_TIMEOUT 1200000
     /* Like BIG_TIMEOUT only bigger because we let conditions
      * awake spuriously without loop (Not looping in here
      * but relying on application to loop)...
      * (expressed in milliseconds)
      */
 
#define META_ENTER ILUJAVA_MON_ENTER(_ilujava_metaLock)
#define META_EXIT ILUJAVA_MON_EXIT(_ilujava_metaLock)

static ILUJAVA_JMON_PTR _ilujava_metaLock;
static ilu_LockTech _ilujava_lockTech;
static ltj_QueueEl_s * _ilujava_freeList = 0; /* of queue elements */


/* must hold meta lock */
INTERNALONLY void
free_QueueEl(ltj_QueueEl_s * el)
{
    el->prev = 0;
    el->next = _ilujava_freeList;
    _ilujava_freeList = el;
}


/* must hold meta lock */
INTERNALONLY ltj_QueueEl_s *
new_QueueEl(JENV_FORMAL_NOCOMMA)
{
    ltj_QueueEl_s * el = _ilujava_freeList;
    if (el) {
        _ilujava_freeList = el->next;
    } else {
        el = (ltj_QueueEl_s *) java_sysMalloc(sizeof(ltj_QueueEl_s));
        el->elQueue = ILUJAVA_MON_ALLOC();
    }
    el->next = 0;
    el->prev = 0;
    return el;
}


/* must hold meta lock */
INTERNALONLY void
incl_QueueEl(ltj_Condition_s * cond, ltj_QueueEl_s * el)
{
    el->next = 0;
    el->prev = cond->last;
    if (cond->last==0) {
        cond->last = el;
        cond->first = el;
        return;
    }
    cond->last->next = el; 
}


/* must hold meta lock; idempotent */
INTERNALONLY void
excl_QueueEl(ltj_Condition_s * cond, ltj_QueueEl_s * el)
{
    if (el->prev) el->prev->next = el->next;
    if (el->next) el->next->prev = el->prev;
    if (cond->last==el) cond->last = el->prev;
    if (cond->first==el) cond->first = el->next;
    el->prev = 0;
    el->next = 0;
}


/* See iluxport.h lt_mcreate for specification */
INTERNALONLY ilu_Mutex
_ilujava_mcreate (ilu_string d1, ilu_string d2)
{
    JENV_DECLARE_INITFROMTHINAIR
    ILU_ERRS((no_memory)) err = ILU_INIT_NO_ERR;
    ltj_Mutex_s * mutex; 
    JsTRACE(TRACE_FANATIC, ("$LockTech mcreate 1\n"));
    mutex = (ltj_Mutex_s *) java_sysMalloc(sizeof(ltj_Mutex_s));
    JsTRACE(TRACE_FANATIC, ("$LockTech mcreate 2\n"));
    if (mutex == 0) {
        _ilujava_PANIC("$ mcreate failed; FATAL!!!");
        return ILU_NIL;
    }
    JsTRACE(TRACE_FANATIC, ("$LockTech mcreate 3\n"));
    mutex->d1 = ilu_StrdupE(d1, &err);
    JsTRACE(TRACE_FANATIC, ("$LockTech mcreate 4\n"));
    ILU_MUST_BE_SUCCESS(err);
    JsTRACE(TRACE_FANATIC, ("$LockTech mcreate 5\n"));
    mutex->d2 = ilu_StrdupE(d2, &err);
    JsTRACE(TRACE_FANATIC, ("$LockTech mcreate 6\n"));
    ILU_MUST_BE_SUCCESS(err);
    JsTRACE(TRACE_FANATIC, ("$LockTech mcreate 7\n"));
    mutex->holder = MUTEX_FREE;
    mutex->stackWhenAquired = 0;
    JsTRACE(TRACE_FANATIC, ("$LockTech mcreate 8\n"));
    mutex->mQueue = ILUJAVA_MON_ALLOC();
    JsTRACE(TRACE_EASY, ("$LockTech mcreate m:%x %s %s\n", 
            mutex, mutex->d1, mutex->d2));
    return (ilu_Mutex) mutex;
}

INTERNALONLY void
_ilujava_mdestroy (ilu_Mutex m, ILU_ERRS((bad_param, bad_locks)) * err)
{
    JENV_DECLARE_INITFROMTHINAIR
    ltj_Mutex_s * mutex = (ltj_Mutex_s *) m;
    JsTRACE(TRACE_EASY, ("$LockTech mdestroy m:%x %s %s\n", 
            m, mutex->d1, mutex->d2));
    ILU_CLER(*err);
    if (mutex->holder != MUTEX_FREE) {
        PrintWereAquired(mutex);
        SpecialErrPrint("$ LockTech mdestroy: bad locks\n");
        ILU_ERR_CONS0(bad_locks, err, 0); 
        return; /* and leak the memory ... */
    }
    ILUJAVA_MON_FREE(mutex->mQueue);
    ilu_free(mutex->d1);
    ilu_free(mutex->d2);
    java_free(mutex);
}


/* See iluxport.h lt_ccreate for specification */
INTERNALONLY ilu_Condition
_ilujava_ccreate (ilu_string d1, ilu_string d2)
{
    ILU_ERRS((no_memory)) err = ILU_INIT_NO_ERR;
    ltj_Condition_s * cond; 
    cond = (ltj_Condition_s *)java_sysMalloc(sizeof(ltj_Condition_s));
    if (cond == 0) {
        _ilujava_PANIC("$ ccreate failed; FATAL!!!");
        return ILU_NIL;
    }
    cond->d1 = ilu_StrdupE(d1, &err);
    ILU_MUST_BE_SUCCESS(err);
    cond->d2 = ilu_StrdupE(d2, &err);
    ILU_MUST_BE_SUCCESS(err);
    cond->first = 0;
    cond->last = 0;
    JsTRACE(TRACE_EASY, ("$LockTech ccreate c:%x %s %s\n", 
            cond, cond->d1, cond->d2));
    return (ilu_Condition) cond;
}


/* See iluxport.h lt_muncons for specification */
INTERNALONLY void
_ilujava_muncons (ilu_Mutex m, 
             ilu_string * d1,
             ilu_string * d2, 
             ILU_ERRS((bad_param)) * err)
{
    ltj_Mutex_s * mutex = (ltj_Mutex_s *) m; 
    ILU_CLER(*err);
    *d1 = mutex->d1;
    *d2 = mutex->d2;
    }


/* See iluxport.h lt_cuncons for specification */
INTERNALONLY void
_ilujava_cuncons (ilu_Condition c, 
             ilu_string * d1,
             ilu_string * d2, 
             ILU_ERRS((bad_param)) * err)
{
    ltj_Condition_s * cond = (ltj_Condition_s *) c;
    ILU_CLER(*err);
    *d1 = cond->d1;
    *d2 = cond->d2;
}


/* See iluxport.h lt_cdestroy for specification */
/* Bill says we are guaranteed that no thread is using this anymore */
INTERNALONLY void
_ilujava_cdestroy (ilu_Condition c, ILU_ERRS((bad_param)) * err)
{
    ltj_Condition_s * cond = (ltj_Condition_s *) c;
    JsTRACE(TRACE_EASY, ("$LockTech cdestroy c:%x %s %s\n", 
            c, cond->d1, cond->d2));
    ILU_CLER(*err);
    if (cond->first) {
        _ilu_Assert(0, "bad cdestroy"); 
    }
    ilu_free(cond->d1);
    ilu_free(cond->d2);
    java_free(cond);
}


/* See iluxport.h lt_mdestroy for specification */

/* m1 must not be hold */
INTERNALONLY void
acquire1 (JENV_FORMAL
    ltj_Mutex_s * m1, 
    ILUJAVA_SYSTHREAD_stPtr thread, 
    ltj_Condition_s * cond, 
    ltj_QueueEl_s * el)
{
    JsTRACE(TRACE_FANATIC, ("$LockTech acquire1 1\n"));
    _ilu_Assert((m1->holder != thread), "self entering lock 1");
    JsTRACE(TRACE_FANATIC, ("$LockTech acquire1 2\n"));
    while (1) {
        JsTRACE(TRACE_FANATIC, ("$LockTech acquire1 3\n"));
        META_ENTER;
        JsTRACE(TRACE_FANATIC, ("$LockTech acquire1 4\n"));
        /* Do this here so lock is entered only once;
         * Looping is cheaper then entering the lock
         */
        if (el) {
            excl_QueueEl(cond, el);
            free_QueueEl(el);
            el = 0;
        }
        if (m1->holder == MUTEX_FREE) {
            m1->holder = thread;
            if (TRACE_FANATIC) {
                m1->stackWhenAquired = _ilujava_captureJavaStack();
            }
            META_EXIT;
            return;
        }
        META_EXIT;
        ILUJAVA_MON_ENTER(m1->mQueue); 
        if (m1->holder != MUTEX_FREE) {
            ILUJAVA_MON_WAIT(m1->mQueue, BIG_TIMEOUT);
        }
        ILUJAVA_MON_EXIT(m1->mQueue);
    }
}


/* m1 and m2 must not be hold */
INTERNALONLY void
acquire2 (JENV_FORMAL
    ltj_Mutex_s * m1, 
    ltj_Mutex_s * m2, 
    ILUJAVA_SYSTHREAD_stPtr thread, 
    ltj_Condition_s * cond, 
    ltj_QueueEl_s * el)
{
    _ilu_Assert((m1->holder != thread) && (m2->holder != thread), "self entering lock 2");
    while (1) {
        META_ENTER;
        /* do this here so lock is entered only once */
        if (el) {
            excl_QueueEl(cond, el);
            free_QueueEl(el);
            el = 0;
        }
        /* real stuff */
        if ((m1->holder == MUTEX_FREE) && (m2->holder == MUTEX_FREE)) {
            m1->holder = thread;
            m2->holder = thread;
            if (TRACE_FANATIC) {
                m1->stackWhenAquired = _ilujava_captureJavaStack();
                m2->stackWhenAquired = _ilujava_captureJavaStack();
            }
            META_EXIT;
            return;
        }
        META_EXIT;
        /*
         * We need both monitors
         * it doesn't matter which mutex we are waiting for:
         * either one will be notified on change. 
         */
        if (m1->holder != MUTEX_FREE) {
            ILUJAVA_MON_ENTER(m1->mQueue);
            if (m1->holder != MUTEX_FREE) {
                ILUJAVA_MON_WAIT(m1->mQueue, BIG_TIMEOUT);
            }
            ILUJAVA_MON_EXIT(m1->mQueue);
        } else if (m2->holder != MUTEX_FREE) {
            ILUJAVA_MON_ENTER(m2->mQueue);
            if (m2->holder != MUTEX_FREE) {
                ILUJAVA_MON_WAIT(m2->mQueue, BIG_TIMEOUT);
            }
            ILUJAVA_MON_EXIT(m2->mQueue);
        }
    }
}


/* See iluxport.h lt_acquire for specification */
INTERNALONLY void
_ilujava_acquire (ilu_Mutex m, ILU_ERRS((bad_param, bad_locks)) * err)
{
    JENV_DECLARE_INITFROMTHINAIR
    ltj_Mutex_s * mutex = (ltj_Mutex_s *) m;
    ILUJAVA_SYSTHREAD_stPtr thread = ILUJAVA_SYSTHREAD_sysThreadSelf();
    ILU_CLER(*err);
    JsTRACE(TRACE_EASY, ("$LockTech acquire m:%x t:%x\n", m, thread));
    if (mutex->holder == thread) {
        PrintWereAquired(mutex);
        SpecialErrPrint("$ LockTech acquire: bad locks\n");
        ILU_ERR_CONS0(bad_locks, err, 0);
        return;
    }
    acquire1(JENV_ACTUAL mutex, thread, 0, 0);
}


/* See iluxport.h lt_hold for specification */
INTERNALONLY void
_ilujava_hold (ilu_Mutex m, ILU_ERRS((bad_param, bad_locks)) * err)
{
    JENV_DECLARE_INITFROMTHINAIR
    ltj_Mutex_s * mutex = (ltj_Mutex_s *) m;
    ILUJAVA_SYSTHREAD_stPtr thread = ILUJAVA_SYSTHREAD_sysThreadSelf();
    ILU_CLER(*err);
    if (mutex->holder != thread) {
        PrintWereAquired(mutex);
        SpecialErrPrint("$ LockTech hold: bad locks\n");
        ILU_ERR_CONS0(bad_locks, err, 0);
        return;
    }
    JsTRACE(TRACE_HARD, ("$LockTech hold m:%x t:%x\n", m, thread));
}


/* See iluxport.h lt_release for specification */
INTERNALONLY void
_ilujava_release (ilu_Mutex m, ILU_ERRS((bad_param, bad_locks)) * err)
{
    JENV_DECLARE_INITFROMTHINAIR
    ltj_Mutex_s * mutex = (ltj_Mutex_s *) m;
    ILUJAVA_SYSTHREAD_stPtr thread = ILUJAVA_SYSTHREAD_sysThreadSelf();
    JsTRACE(TRACE_EASY, ("$LockTech release m:%x t:%x\n", m, thread));
    ILU_CLER(*err);
    if (mutex->holder != thread) {
        PrintWereAquired(mutex);
        SpecialErrPrint("$ LockTech release: bad locks\n");
        ILU_ERR_CONS0(bad_locks, err, 0);
        return;
    }
    META_ENTER;
    ILUJAVA_MON_ENTER(mutex->mQueue);
    mutex->holder = MUTEX_FREE;
    if (mutex->stackWhenAquired) {
        ilu_free(mutex->stackWhenAquired);
        mutex->stackWhenAquired = 0;
    }
    ILUJAVA_MON_NOTIFYALL(mutex->mQueue);
    ILUJAVA_MON_EXIT(mutex->mQueue);
    META_EXIT;
}


/* See iluxport.h lt_notify for specification */
INTERNALONLY void
_ilujava_notify (ilu_Condition c,
            ILU_ERRS((bad_param)) * err)
{
    JENV_DECLARE_INITFROMTHINAIR
    ltj_Condition_s * cond = (ltj_Condition_s *) c;
    ltj_QueueEl_s * el;
    JsTRACE(TRACE_HARD, ("$LockTech notify c:%x\n", c));
    ILU_CLER(*err);
    META_ENTER;
    el = cond->first;
    if (el) excl_QueueEl(cond, el);
    META_EXIT;
    if (el) {
        ILUJAVA_MON_ENTER(el->elQueue);
        ILUJAVA_MON_NOTIFYALL(el->elQueue);
        ILUJAVA_MON_EXIT(el->elQueue);
    }
}

static ilu_FineTime _ilujava_z0 = {0, 0};

/* See iluxport.h lt_wait for specification */
INTERNALONLY void 
_ilujava_wait (ilu_Condition c, ilu_Mutex m, ilu_Mutex m2,
          const ilu_FineTime * timeout,
          ILU_ERRS((bad_param, bad_locks)) * err) 
{
    JENV_DECLARE_INITFROMTHINAIR
    Jint jtimeout = VERY_BIG_TIMEOUT;
    ltj_Condition_s * cond = (ltj_Condition_s *) c;
    ltj_Mutex_s * mutex = (ltj_Mutex_s *) m;
    ltj_Mutex_s * mutex2 = (ltj_Mutex_s *) m2;
    ILUJAVA_SYSTHREAD_stPtr thread = ILUJAVA_SYSTHREAD_sysThreadSelf();
    ltj_QueueEl_s * el;
    JsTRACE(TRACE_EASY, ("$LockTech wait entered c:%x t:%x\n", c, thread));
    ILU_CLER(*err);
    if (mutex->holder != thread || mutex2->holder != thread ) {
        PrintWereAquired(mutex);
        PrintWereAquired(mutex2);
        SpecialErrPrint("$ LockTech wait: bad locks");
        ILU_ERR_CONS0(bad_locks, err, 0);
        return;
    }
    
    META_ENTER;
    if (mutex != mutex2) {
        ILUJAVA_MON_ENTER(mutex2->mQueue);
        mutex2->holder = MUTEX_FREE;
        if (mutex2->stackWhenAquired) {
            ilu_free(mutex2->stackWhenAquired);
            mutex2->stackWhenAquired = 0;
        }
        ILUJAVA_MON_NOTIFYALL(mutex2->mQueue);
        ILUJAVA_MON_EXIT(mutex2->mQueue);
    }
    ILUJAVA_MON_ENTER(mutex->mQueue);
    mutex->holder = MUTEX_FREE;
    if (mutex->stackWhenAquired) {
        ilu_free(mutex->stackWhenAquired);
        mutex->stackWhenAquired = 0;
    }
    ILUJAVA_MON_NOTIFYALL(mutex->mQueue);
    ILUJAVA_MON_EXIT(mutex->mQueue);
    el = new_QueueEl(JENV_ACTUAL_NOCOMMA);
    /* Take this lock before releasing the meta lock */
    ILUJAVA_MON_ENTER(el->elQueue);
    incl_QueueEl(cond, el);
    META_EXIT;
    JsTRACE(TRACE_HARD, ("$LockTech wait prepared c:%x t:%x\n", c, thread));
    
    if (timeout) {
        ilu_FineTime wait = ilu_FineTime_Sub(*timeout, ilu_FineTime_Now());
        if (ilu_FineTime_Cmp(wait, _ilujava_z0) > 0) {
            if (wait.ft_s > 1000000) {wait.ft_s = 1000000;}
            jtimeout = wait.ft_s * 1000 
                + ilu_rescale(wait.ft_t, ilu_FineTimeRate, 1000);
            if (jtimeout <= 0) {jtimeout = 1;}
        } else {
            jtimeout = 1;
        }
        /* avoid jtimeout==0 because that means no timeout to java */
    }
    
    ILUJAVA_MON_WAIT(el->elQueue, jtimeout);
    ILUJAVA_MON_EXIT(el->elQueue);

    /* No lock is hold here */
    JsTRACE(TRACE_HARD, ("$LockTech wait re-aquire c:%x t:%x\n", c, thread));
    
    if (mutex == mutex2) {
        acquire1(JENV_ACTUAL m, thread, cond, el);
    } else {
        acquire2(JENV_ACTUAL m, m2, thread, cond, el);
    }
    JsTRACE(TRACE_EASY, ("$LockTech wait done c:%x t:%x\n", c, thread));
}


EXPORTLIMITED void 
_ilujava_LockTechInit () 
{
    JENV_DECLARE_INITFROMTHINAIR
    ILU_ERRS((bad_param, no_memory)) err = ILU_INIT_NO_ERR;
    #if defined(useJNIMonitors)
       jclass CLASSID_java_lang_Object;
       CLASSID_java_lang_Object = (*j_env)->FindClass(j_env, "java/lang/Object");
       METHODID_NOTIFYALL = (*j_env)->GetMethodID(j_env, CLASSID_java_lang_Object, "notifyAll", "()V");
       METHODID_WAIT = (*j_env)->GetMethodID(j_env, CLASSID_java_lang_Object, "wait", "(J)V");
    #endif
    JsTRACE(TRACE_FANATIC, ("$LockTech init 1\n"));
    /* define meta lock */
    _ilujava_metaLock = ILUJAVA_MON_ALLOC();
    /* register procedures */
    _ilujava_lockTech.lt_canTimeoutWait    = ilu_TRUE;
    _ilujava_lockTech.lt_mcreate    = _ilujava_mcreate;
    _ilujava_lockTech.lt_muncons    = _ilujava_muncons;
    _ilujava_lockTech.lt_acquire    = _ilujava_acquire;
    _ilujava_lockTech.lt_hold       = _ilujava_hold;
    _ilujava_lockTech.lt_release    = _ilujava_release;
    _ilujava_lockTech.lt_mdestroy   = _ilujava_mdestroy;
    _ilujava_lockTech.lt_ccreate    = _ilujava_ccreate;
    _ilujava_lockTech.lt_cuncons    = _ilujava_cuncons;
    _ilujava_lockTech.lt_notify     = _ilujava_notify;
    _ilujava_lockTech.lt_cdestroy   = _ilujava_cdestroy;
    _ilujava_lockTech.lt_wait       = _ilujava_wait;
    JsTRACE(TRACE_FANATIC, ("$LockTech init 2\n"));
    ilu_SetLockTech(&_ilujava_lockTech, &err);
    JsTRACE(TRACE_FANATIC, ("$LockTech init 3\n"));
    if ILU_ERRNOK(err) {
        _ilujava_PANIC("$ ** LockTech LockTechInit error\n");
    }
}


#if (ILUJAVA_H_MINORVERSION <= 1)

/*
  We exclude jdk1.2 because I didn't find something for
  ILUJAVA_SYSTHREAD_javaThreadFromSysThread
*/

static char *
ThreadID(ILUJAVA_SYSTHREAD_stPtr t)
/* Returns some identification of a thread
 * This is a debugging aid only !
 * Leaks memory
 */
{
    char * cs = 0;
    JString jh_str;
    JObject jh_thr; /*actually a thread*/
    if (t) {
        JENV_DECLARE_INITFROMTHINAIR
        jh_thr = (JObject) ILUJAVA_SYSTHREAD_javaThreadFromSysThread(t);
        jh_str = (JString) JCALL_java_lang_Thread_toString(jh_thr);
        cs = IluJava_JString_toheap80(JENV_ACTUAL jh_str); /*malloc is used*/
    }
    return cs;
}

#endif


/* end */
