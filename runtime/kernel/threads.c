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
/* $Id: threads.c,v 1.58 1999/09/09 17:48:11 janssen Exp $ */
/* Last edited by Mike Spreitzer October 30, 1998 2:33 pm PST */

/* This code was kindly donated by Digital Creations, Inc. */

/* Win32 support added - larner */


#include <iluntrnl.h>
#include <iludebug.h>

#include "threads.h"

#include <stdio.h>

#ifdef ENABLE_DEBUGGING
#define ILU_THREADS_NOTE(level,code)	if(((ilu_DebugLevel & (level)) != 0)&&((ilu_DebugLevel & THREAD_DEBUG) != 0))ilu_DebugPrintf code
#else
#define ILU_THREADS_NOTE(level,code)
#endif

#ifndef WIN32
#include <sys/time.h>
#include <sys/types.h>
#endif

#include "oscalls.h"
#include <errno.h>


#ifdef WIN32

#include <process.h>
 

/* ----------------------------------------------------------------- */
/* Construct a condition out of NT thread synchronization primitives */
/* ----------------------------------------------------------------- */

#define BROADCAST_EVENT		0
#define NOTIFY_EVENT		1

typedef struct condition_struct Condition;

struct condition_struct {

	/* the number of threads waiting on the condition */
	long m_l_num_waiting_threads; 

    /* events that are signalled when the condition is 
	   Broadcast or notified.  After a broadcast, the last 
	   thread to wake up resets the manual reset event
	   m_h_occurred[BROADCAST_EVENT],  The auto reset
	   event m_h_occurred[NOTIFY_EVENT] is reset 
	   automatically after the pulse event used in notify. */
	HANDLE m_h_occurred[2];
};


/* ----------------------------------------------- */
/* initialize a condition - returns 0 on success, 
   else result of call to WIN32_GETLASTERROR()           */

DWORD condition_init (Condition* p_condition) {

	/* broadcast event is a manual reset event */
	p_condition->m_h_occurred[BROADCAST_EVENT] = CreateEvent(NULL, ilu_TRUE, ilu_FALSE, NULL);

	if (p_condition->m_h_occurred[BROADCAST_EVENT] == NULL)
		return WIN32_GETLASTERROR();

	/* notify event is a autoreset event */
	p_condition->m_h_occurred[NOTIFY_EVENT] = CreateEvent(NULL, ilu_FALSE, ilu_FALSE, NULL);

	if (p_condition->m_h_occurred[NOTIFY_EVENT] == NULL) {
		DWORD dw_retvalue = WIN32_GETLASTERROR();
		CloseHandle(p_condition->m_h_occurred[BROADCAST_EVENT]);
		return dw_retvalue;
	}

	p_condition->m_l_num_waiting_threads = 0;

	return 0;
}


/* ----------------------------------------------- */
/* destroy a condition - returns 0 on success, 
   else result of call to WIN32_GETLASTERROR()           */

DWORD condition_destroy (Condition* p_condition) {

	DWORD dw_retvalue = 0;

	if (CloseHandle(p_condition->m_h_occurred[BROADCAST_EVENT]) == ilu_FALSE)
		dw_retvalue = WIN32_GETLASTERROR();

	if (CloseHandle(p_condition->m_h_occurred[NOTIFY_EVENT]) == ilu_FALSE)
		return WIN32_GETLASTERROR();

	return dw_retvalue;
}


/* ----------------------------------------------- */
/* broadcast to all threads waiting on a condition 
   returns 0 on success, else result of call to 
   WIN32_GETLASTERROR()                                  */


DWORD condition_broadcast( Condition* p_condition ) {
	
	if ((p_condition->m_l_num_waiting_threads > 0) && 

		/* note that checking m_l_num_waiting_threads is safe here because
		all operations on it are assumed to occur under the protection of
		the mutex being used with the condition */

		(SetEvent( p_condition->m_h_occurred[BROADCAST_EVENT] ) == ilu_FALSE))
			return WIN32_GETLASTERROR();

	return 0;
}


/* ----------------------------------------------- */
/* signal to a threads waiting on a condition 
   returns 0 on success, else result of call to 
   WIN32_GETLASTERROR()                                  */


DWORD condition_signal( Condition* p_condition ) {
	
	if ((p_condition->m_l_num_waiting_threads > 0) && 

		/* note that checking m_l_num_waiting_threads is safe here because
		all operations on it are assumed to occur under the protection of
		the mutex being used with the condition */

		(SetEvent( p_condition->m_h_occurred[NOTIFY_EVENT] ) == ilu_FALSE))
			return WIN32_GETLASTERROR();

	return 0;
}


/* ----------------------------------------------- */
/* wait on a condition - returns 0 on success, 
   else result of call to WIN32_GETLASTERROR()           */

DWORD condition_wait( Condition* p_condition, HANDLE* p_h_mutex, DWORD dw_milliseconds  ) {
  
  DWORD dw_result = 0;

	/* Add this thread to the number of waiting threads. Note this is under the 
	protection of the p_h_mutex mutex.*/
	p_condition->m_l_num_waiting_threads++;

	/* Release the mutex. */
	if (ReleaseMutex( *p_h_mutex ) == ilu_FALSE) {
			p_condition->m_l_num_waiting_threads--;
			return WIN32_GETLASTERROR();
	}

	/* here is the area of vulnerability [between ReleaseMutex( *p_h_mutex ) and 
	   WaitForMultipleObjects( ...)] we are avoiding by using the events */

	/* Wait for condition_broadcast to signal the condition. */
	dw_result = WaitForMultipleObjects(2, p_condition->m_h_occurred, ilu_FALSE, dw_milliseconds );
	if (dw_result == WAIT_FAILED) {
		dw_result = WIN32_GETLASTERROR();
		WaitForSingleObject( *p_h_mutex, INFINITE ); /* always reaquire mutex */
		p_condition->m_l_num_waiting_threads--;
		return dw_result;
	}

	/* Reaquire the mutex. */
	WaitForSingleObject( *p_h_mutex, INFINITE );

	/* If this is the last thread to wake up from the broadcast, reset "occurred" */

	p_condition->m_l_num_waiting_threads--;

	if ((p_condition->m_l_num_waiting_threads <= 0) &&
		(dw_result != WAIT_TIMEOUT) &&
		(dw_result - WAIT_OBJECT_0 == BROADCAST_EVENT) &&
		(ResetEvent( p_condition->m_h_occurred[BROADCAST_EVENT] ) == ilu_FALSE))
		  return WIN32_GETLASTERROR();

	if (dw_result != WAIT_TIMEOUT)
		dw_result = 0;

	return dw_result;
}



/* ----------------------------------------------------------------- */

#endif /* WIN32 */


/* ----------------------------------------------- */
/* defines                                         */

#define PN(x) (x != NIL ? x : "(null)")

#define CAST_MUTEX(as_any, as_mutex) ilukt_Mutex *as_mutex = (ilukt_Mutex *)as_any

#define CAST_CONDITION(as_any, as_cond) ilukt_Condition *as_cond = (ilukt_Condition *)as_any

/* should more checking be done to ensure m is valid (e.g. examining the
   structure of m->mutex to make sure it looks like a viable mutex)? */
#define ASSERT_VALID_MUTEX(m, e) if (m == NIL) { ILU_ERR_CONS1(bad_param, e, minor, ilu_bpm_nil, 0); return; }

#define ASSERT_VALID_CONDITION(c, e) ASSERT_VALID_MUTEX(c, e)

#define ASSERT_HOLD_MUTEX(m, e) if (!(m->locked && (SAME_THREAD(m->owner_thread, current_thread)))) { ILU_ERR_CONS0(bad_locks, e, 0); return; }



/* ----------------------------------------------- */
/* thread value printing                           */

static char *no_mem_msg = "(insufficient memory to format thread)";
static ilu_string no_ftt = "(unformatted thread due to timing splinter)";

static void FmtBytes(char *dest, unsigned char *src, int len)
{
  static char     hexes[16] = "0123456789abcdef";
  int             i, di;
#ifdef WORDS_BIGENDIAN
  di = 1;
#else
  di = -1;
  src += len - 1;
#endif
  for (i = 0; i < len; i++) {
    unsigned char   c = *src;
    *(dest++) = hexes[c / 16];
    *(dest++) = hexes[c & 15];
    src += di;
  }
  *dest = 0;
  return;
}

#ifdef DEBUG_THREAD_REFERENT
#define THDIDSZ(t) (2 * (sizeof(t) + sizeof(*t) + 1))
#else
#define THDIDSZ(t) (2 * sizeof(t) + 1)
#endif

static void WriteThreadID(char *dest, DECLARE_THREAD(t))
{
  unsigned char  *b = (unsigned char *) &t;
  FmtBytes(dest, b, sizeof(t));
#ifdef DEBUG_THREAD_REFERENT
  dest[2 * sizeof(t)] = ':';
  FmtBytes(dest + 2 * sizeof(t) + 1, (unsigned char *) t, sizeof(*t));
#endif
}

static void PrintCurrentThreadID(ilu_MessagePrinter mp)
{
  DECLARE_THREAD(current_thread);
  char            buf[THDIDSZ(current_thread)];
  current_thread = GET_CURRENT_THREAD();
  WriteThreadID(buf, current_thread);
  (*mp) ("%s ", buf);
  return;
}

#define ILU_LOCK_MUTEX(m) LOCK_MUTEX(m)
#define ILU_UNLOCK_MUTEX(m) UNLOCK_MUTEX(m)

/* ----------------------------------------------- */
/* mutex structure                                 */

typedef struct
{
  DECLARE_MUTEX(mutex);
  DECLARE_THREAD(owner_thread);
  ilu_string d1, d2;
  ilu_boolean locked; /* Is it guaranteed that a thread ID will never be 0 
			 (or some other value)?  If so, we could elminiate
			 this locked field and just set owner_thread to some
			 impossible value when the mutex is unlocked.  Too bad 
			 there doesn't seem to be a user-level equivalent of 
			 the "mutex_owned" kernel operation.  Any 
			 suggestions?!? */
} ilukt_Mutex;


/* ----------------------------------------------- */
/* Condition structure                             */

typedef struct
{
  DECLARE_CONDITION(condition);
  ilu_string d1, d2;
} ilukt_Condition;

/* ----------------------------------------------- */
/* create a mutex                                  */

static          ilu_Mutex
ilukt_LT_mcreate(ilu_string d1, ilu_string d2)
{
  ilukt_Mutex    *new_mutex;
  new_mutex = (ilukt_Mutex *) ilu_malloc(sizeof(ilukt_Mutex));

#ifdef ENABLE_DEBUGGING
  if ((ilu_DebugLevel & LOCK_DEBUG) && (ilu_DebugLevel & THREAD_DEBUG) &&
	  (strcmp(d2, "debugmu") != 0) ) {
    ilu_DebugPrintf ("ilukt_LT_mcreate('%s','%s')\n", d1, d2);
  };
#endif /* def ENABLE_DEBUGGING */

  if (!new_mutex)
    /* Couldn't allocate space, so fail out. */
    return NIL;
  if (!INIT_MUTEX(new_mutex->mutex)) {
    /*
     * Mutex initialization failed for some reason, so clean up and
     * fail out.
     */
    ilu_free(new_mutex);
    return NIL;
  }
  if (d1 != NIL) {
    new_mutex->d1 = _ilu_Strdup(d1);
    if (new_mutex->d1 == NIL) {
      DESTROY_MUTEX(new_mutex->mutex);
      ilu_free(new_mutex);
      return NIL;
    }
  } else
    new_mutex->d1 = NIL;
  if (d2 != NIL) {
    new_mutex->d2 = _ilu_Strdup(d2);
    if (new_mutex->d2 == NIL) {
      DESTROY_MUTEX(new_mutex->mutex);
      if (new_mutex->d1 != NIL) {
	ilu_free(new_mutex->d1);
      }
      ilu_free(new_mutex);
      return NIL;
    }
  } else
    new_mutex->d2 = NIL;

  new_mutex->locked = ilu_FALSE;

#ifdef ENABLE_DEBUGGING
  if ((ilu_DebugLevel & LOCK_DEBUG) && (ilu_DebugLevel & THREAD_DEBUG) &&
	  (strcmp(d2, "debugmu") != 0)) {
    ilu_DebugPrintf ("ilukt_LT_mcreate succeeded: %p\n", new_mutex);
  };
#endif /* def ENABLE_DEBUGGING */

  return new_mutex;
}

static void 
ilukt_LT_mdestroy(ilu_Mutex m,
		  ILU_ERRS((bad_param, bad_locks)) * err)
{
  CAST_MUTEX(m, mutex_obj);
  ASSERT_VALID_MUTEX(mutex_obj, err);
  ilu_free(mutex_obj->d1);
  ilu_free(mutex_obj->d2);
  DESTROY_MUTEX(mutex_obj->mutex);
  ilu_free(mutex_obj);
  ILU_CLER(*err);
}


/* ----------------------------------------------- */
/* Reveals strings given to create                 */

static void 
  ilukt_LT_muncons(ilu_Mutex m, ilu_string *d1, ilu_string *d2, ILU_ERRS((bad_param)) *err)
{
  CAST_MUTEX(m, mutex_obj);

  ASSERT_VALID_MUTEX(mutex_obj, err);

  if (d1 == NIL || d2 == NIL)
    {
      ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_nil, 0);
      return;
    }

  *d1 = mutex_obj->d1;
  *d2 = mutex_obj->d2;

  ILU_CLER(*err);

  return;
}

/* ----------------------------------------------- */
/* Acquire mutex                                   */

static void 
  ilukt_LT_acquire(ilu_Mutex m, ILU_ERRS((bad_param, bad_locks)) *err)
{
  CAST_MUTEX(m, mutex_obj);
  DECLARE_THREAD(current_thread);
  current_thread = GET_CURRENT_THREAD();

#ifdef ENABLE_DEBUGGING
  if ((ilu_DebugLevel & LOCK_DEBUG) && (ilu_DebugLevel & THREAD_DEBUG) && (m != ilu_debugmu)) {
    ilu_DebugPrintf("ilukt_LT_acquire: waiting for (mutex [%s %s])\n",
	   mutex_obj->d1, mutex_obj->d2);
  }
#endif /* ENABLE_DEBUGGING */

  ASSERT_VALID_MUTEX(mutex_obj, err);

  if (mutex_obj->locked &&
      (SAME_THREAD(mutex_obj->owner_thread, current_thread))) {
    ILU_ERR_CONS0(bad_locks, err, 0);
    return;
  }

  ILU_LOCK_MUTEX(mutex_obj->mutex);

#ifdef ENABLE_DEBUGGING
  if ((ilu_DebugLevel & LOCK_DEBUG) && (ilu_DebugLevel & THREAD_DEBUG) && (m != ilu_debugmu)) {
    ilu_DebugPrintf("ilukt_LT_acquire: acquired (mutex [%s %s])\n",
	   mutex_obj->d1, mutex_obj->d2);
  }
#endif /* ENABLE_DEBUGGING */

  mutex_obj->owner_thread = current_thread;
  mutex_obj->locked = ilu_TRUE;

  ILU_CLER(*err);

  return;
}


/* ----------------------------------------------- */
/* Checks that the caller holds the given mutex    */

static void 
  ilukt_LT_hold(ilu_Mutex m, ILU_ERRS((bad_param, bad_locks)) *err)
{
  CAST_MUTEX(m, mutex_obj);
  DECLARE_THREAD(current_thread);
  current_thread = GET_CURRENT_THREAD();

  ASSERT_VALID_MUTEX(mutex_obj, err);

  ASSERT_HOLD_MUTEX(mutex_obj, err);

  ILU_CLER(*err);

  return;
}


/* ----------------------------------------------- */
/* Releases the mutex                              */

static void
  ilukt_LT_release(ilu_Mutex m, ILU_ERRS((bad_param, bad_locks)) *err)
{
  CAST_MUTEX(m, mutex_obj);
  DECLARE_THREAD(current_thread);
  current_thread = GET_CURRENT_THREAD();

#ifdef ENABLE_DEBUGGING
  if ((ilu_DebugLevel & LOCK_DEBUG) && (ilu_DebugLevel & THREAD_DEBUG) && (m != ilu_debugmu)) {
    ilu_DebugPrintf("ilukt_LT_release: releasing (mutex [%s %s])\n",
		    mutex_obj->d1, mutex_obj->d2);
  }
#endif /* ENABLE_DEBUGGING */

  ASSERT_VALID_MUTEX(mutex_obj, err);

  ASSERT_HOLD_MUTEX(mutex_obj, err);

  mutex_obj->locked = ilu_FALSE;

  ILU_UNLOCK_MUTEX(mutex_obj->mutex);

  ILU_CLER(*err);

  return;
}



/* ----------------------------------------------- */
/* Creates a condition                             */

static ilu_Condition
  ilukt_LT_ccreate(ilu_string d1, ilu_string d2)
{
  ilukt_Condition *new_condition;
  new_condition = (ilukt_Condition *)ilu_malloc(sizeof(ilukt_Condition));

  ILU_THREADS_NOTE(LOCK_DEBUG, ("ilukt_LT_ccreate (\"%s\", \"%s\")\n", d1, d2));

  if (new_condition == NIL)
    return NIL;

  if (!INIT_CONDITION(new_condition->condition))
    {
      ilu_free(new_condition);
      return NIL;
    }

  if (d1 != NIL)
    {
      new_condition->d1 = _ilu_Strdup(d1);
      if (new_condition->d1 == NIL)
	{
	  DESTROY_CONDITION(new_condition->condition);
	  ilu_free(new_condition);
	  return NIL;
	}
    }
  else new_condition->d1 = NIL;

  if (d2 != NIL)
    {
      new_condition->d2 = _ilu_Strdup(d2);
      if (new_condition->d2 == NIL)
	{
	  DESTROY_CONDITION(new_condition->condition);
	  if (new_condition->d1 != NIL)
	    {
	      ilu_free(new_condition->d1);
	    }
	  ilu_free(new_condition);
	  return NIL;
	}
    }
  else new_condition->d2 = NIL;

  ILU_THREADS_NOTE(LOCK_DEBUG, ("ilukt_LT_ccreate (\"%s\", \"%s\") succeeded => %p\n",
			d1, d2, new_condition));

  return new_condition;
}


/* ----------------------------------------------- */
/* Reveals strings given to create                 */

static void
  ilukt_LT_cuncons(ilu_Condition c, ilu_string *d1, ilu_string *d2, ILU_ERRS((bad_param )) *err)
{
  CAST_CONDITION(c, condition_obj);

  ASSERT_VALID_CONDITION(condition_obj, err);

  if (d1 == NIL || d2 == NIL)
    {
      ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_nil, 0);
      return;
    }

  *d1 = condition_obj->d1;
  *d2 = condition_obj->d2;

  ILU_CLER(*err);

  return;
}


/* ----------------------------------------------- */
/* Notifies the condition                          */

static void
  ilukt_LT_notify(ilu_Condition c, ILU_ERRS((bad_param)) *err)
{
  CAST_CONDITION(c, condition_obj);

#ifdef ENABLE_DEBUGGING
  if (ilu_DebugLevel & LOCK_DEBUG) {
    ILU_THREADS_NOTE(LOCK_DEBUG,
	       ("ilukt_LT_notify: notifying (condition %p[%s %s])\n",
		c,
		(condition_obj->d1 ? condition_obj->d1 : ""),
		(condition_obj->d2 ? condition_obj->d2 : "")));
  }
#endif /* ENABLE_DEBUGGING */

  ASSERT_VALID_CONDITION(condition_obj, err);

  CONDITION_BROADCAST(condition_obj->condition);

  ILU_CLER(*err);

  return;
}


/* ----------------------------------------------- */
/* Destroys the condition                          */

static void
  ilukt_LT_cdestroy(ilu_Condition c, ILU_ERRS((bad_param)) *err)
{
  CAST_CONDITION(c, condition_obj);

  ILU_THREADS_NOTE(LOCK_DEBUG,
		   ("ilukt_LT_cdestroy: condition %p[%s %s]\n",
		    condition_obj,
		    (condition_obj->d1 ? condition_obj->d1 : ""),
		    (condition_obj->d2 ? condition_obj->d2 : "")));

  ASSERT_VALID_CONDITION(condition_obj, err);

  DESTROY_CONDITION(condition_obj->condition);

  if (condition_obj->d1 != NIL)
    {
      ilu_free(condition_obj->d1);
    }
  if (condition_obj->d2 != NIL)
    {
      ilu_free(condition_obj->d2);
    }

  ilu_free(condition_obj);

  ILU_CLER(*err);

  return;
}


/* ----------------------------------------------- */
/* Waits on the condition                          */

#define BYTESPERCARD	(sizeof(ilu_cardinal)/sizeof(unsigned char))
#define DIGITSPERCARD	(3*BYTESPERCARD)

static void
ilukt_LT_wait(ilu_Condition c, ilu_Mutex m, ilu_Mutex m2,
	      const ilu_FineTime *timeout,
	      ILU_ERRS((bad_param, bad_locks)) * err)
{
  CAST_CONDITION(c, condition_obj);
  CAST_MUTEX(m, mutex_obj);
  CAST_MUTEX(m2, mutex_obj2);
  DECLARE_THREAD(current_thread);
  ilu_string      d1 = (condition_obj->d1 ? condition_obj->d1 : "");
  ilu_string      d2 = (condition_obj->d2 ? condition_obj->d2 : "");
#ifdef ENABLE_DEBUGGING
  char tobuf[2*DIGITSPERCARD+9];
#endif
#ifdef ILU_WIN32_THREADS
  DWORD           relative_time_in_msec = INFINITE;
#ifdef ENABLE_DEBUGGING
  if (((ilu_DebugLevel & (THREAD_DEBUG | LOCK_DEBUG))
       == (THREAD_DEBUG | LOCK_DEBUG))
      && timeout)
    sprintf(tobuf, ", t.o.=%lu:%lu", timeout->ft_s, timeout->ft_t);
  else
    tobuf[0] = 0;
#endif /* ENABLE_DEBUGGING */
#define timeout_val relative_time_in_msec
  if (timeout) {
    ilu_FineTime    the_time_now, delta;
    static ilu_FineTime zeroFT = {0, 0};
    the_time_now = ilu_FineTime_Now();
    delta = ilu_FineTime_Sub(*timeout, the_time_now);
    if (ilu_FineTime_Cmp(delta, zeroFT) < 0) {
      ILU_THREADS_NOTE(LOCK_DEBUG,
		       ("ilukt_LT_wait: wait on condition %p[%s %s]%s"
			" wrt mutexes %p[%s %s] & %p[%s %s]"
			" initially timed out.\n",
			c, d1, d2, tobuf,
			mutex_obj2, mutex_obj2->d1, mutex_obj2->d2,
			mutex_obj, mutex_obj->d1, mutex_obj->d2));
      ILU_CLER(*err);
      return;
    } else
      relative_time_in_msec = delta.ft_s * 1000
	+ ilu_rescale(delta.ft_t, ilu_FineTimeRate, 1000);
  }
#else /* ILU_WIN32_THREADS not def */
#ifdef ILU_SOLARIS2_THREADS
  timestruc_t abstimeout;
#else
  struct timespec abstimeout;
#endif /* ILU_SOLARIS2_THREADS */
#define timeout_val abstimeout
#ifdef ENABLE_DEBUGGING
  if (((ilu_DebugLevel & (THREAD_DEBUG | LOCK_DEBUG))
       == (THREAD_DEBUG | LOCK_DEBUG))
      && timeout)
    sprintf(tobuf, ", t.o.=%lu:%lu", timeout->ft_s, timeout->ft_t);
  else
    tobuf[0] = 0;
#endif /* ENABLE_DEBUGGING */
  if (timeout) {
    abstimeout.tv_sec = timeout->ft_s;
    abstimeout.tv_nsec = ilu_rescale(timeout->ft_t,
				     ilu_FineTimeRate, 1000000000);
  }
#endif /* ILU_WIN32_THREADS def'd */
  
  current_thread = GET_CURRENT_THREAD();


  ASSERT_VALID_CONDITION(condition_obj, err);
  ASSERT_VALID_MUTEX(mutex_obj, err);
  ASSERT_VALID_MUTEX(mutex_obj2, err);

  ASSERT_HOLD_MUTEX(mutex_obj, err);
  if (mutex_obj != mutex_obj2) {
    ASSERT_HOLD_MUTEX(mutex_obj2, err);

    ILU_THREADS_NOTE(LOCK_DEBUG,
		   ("ilukt_LT_wait: waiting on condition %p[%s %s]%s"
		    " wrt mutexes %p[%s %s] & %p[%s %s].\n",
		    c, d1, d2, tobuf,
		    mutex_obj2, mutex_obj2->d1, mutex_obj2->d2,
		    mutex_obj, mutex_obj->d1, mutex_obj->d2));

    ILU_UNLOCK_MUTEX(mutex_obj2->mutex);
  } else {
    ILU_THREADS_NOTE(LOCK_DEBUG,
		   ("ilukt_LT_wait: waiting on condition"
		    " %p[%s %s]%s wrt mutex %p[%s %s]\n",
		    c, d1, d2, tobuf,
		    mutex_obj, mutex_obj->d1, mutex_obj->d2));
  }

  if (timeout)
    CONDITION_TIMEDWAIT(condition_obj->condition, mutex_obj->mutex,
			timeout_val);
  else
    CONDITION_WAIT(condition_obj->condition, mutex_obj->mutex);

  if (mutex_obj == mutex_obj2) {
    mutex_obj->owner_thread = current_thread;
    mutex_obj->locked = ilu_TRUE;
    ILU_THREADS_NOTE(LOCK_DEBUG,
	      ("ilukt_LT_wait: resumed from wait on cond"
	       " %p[%s %s]%s wrt mutex %p[%s %s].\n",
	       c, d1, d2, tobuf,
	       mutex_obj, mutex_obj->d1, mutex_obj->d2));
  } else {
    ILU_UNLOCK_MUTEX(mutex_obj->mutex);
    ILU_THREADS_NOTE(LOCK_DEBUG,
		     ("ilukt_LT_wait: resuming from wait on cond"
		  " %p[%s %s]%s wrt mutexes %p[%s %s] & %p[%s %s];"
		      " relocking lower one.\n",
		      c, d1, d2, tobuf,
		      mutex_obj2, mutex_obj2->d1, mutex_obj2->d2,
		      mutex_obj, mutex_obj->d1, mutex_obj->d2));
    ILU_LOCK_MUTEX(mutex_obj2->mutex);
    mutex_obj2->owner_thread = current_thread;
    mutex_obj2->locked = ilu_TRUE;
    ILU_THREADS_NOTE(LOCK_DEBUG,
		     ("ilukt_LT_wait: resuming from wait on cond"
		  " %p[%s %s]%s wrt mutexes %p[%s %s] & %p[%s %s];"
		      " relocking higher one.\n",
		      c, d1, d2, tobuf,
		      mutex_obj2, mutex_obj2->d1, mutex_obj2->d2,
		      mutex_obj, mutex_obj->d1, mutex_obj->d2));
    ILU_LOCK_MUTEX(mutex_obj->mutex);
    mutex_obj->owner_thread = current_thread;
    mutex_obj->locked = ilu_TRUE;
    ILU_THREADS_NOTE(LOCK_DEBUG,
		     ("ilukt_LT_wait: resumed from wait on cond"
	       " %p[%s %s]%s wrt mutexes %p[%s %s] & %p[%s %s].\n",
		      c, d1, d2, tobuf,
		      mutex_obj2, mutex_obj2->d1, mutex_obj2->d2,
		      mutex_obj, mutex_obj->d1, mutex_obj->d2));
  }

  ILU_CLER(*err);

  return;
}


/* ----------------------------------------------- */
/* LockTech structure                              */

static ilu_LockTech
  _ilu_threaded_lt = {
    ilu_TRUE,		/* lt_canTimeoutWait */
    ilukt_LT_mcreate,
    ilukt_LT_muncons,
    ilukt_LT_acquire,
    ilukt_LT_hold,
    ilukt_LT_release,
    ilukt_LT_mdestroy,
    ilukt_LT_ccreate,
    ilukt_LT_cuncons,
    ilukt_LT_notify,
    ilukt_LT_cdestroy,
    ilukt_LT_wait };
			 
	

/* ----------------------------------------------- */
/* read wait                                       */

static void
ilukt_WT_read_wait(int fd, int auxfd, ilu_boolean * sure,
		   ilu_FineTime * limit, ILU_ERRS((interrupted)) * err)
{
  fd_set          read_set, exn_set;
  int             stat, maxfd = fd;
  struct timeval  time, *time_p;
  
  ILU_THREADS_NOTE(LOCK_DEBUG,
		   ("ilukt_WT_read_wait(%d) entered.\n", fd));

  FD_ZERO(&read_set);
  FD_SET(fd, &read_set);
  FD_ZERO(&exn_set);
  FD_SET(fd, &exn_set);
  if (auxfd >= 0) {
    FD_SET(auxfd, &read_set);
    FD_SET(auxfd, &exn_set);
    if (auxfd > fd)
      maxfd = auxfd;
  }

  if (limit != 0) {
    time.tv_sec = limit->ft_s;
    time.tv_usec = ilu_rescale(limit->ft_t, ilu_FineTimeRate, 1000000);
    time_p = &time;
  } else {
    time_p = NULL;
  }

  if ((stat = select(maxfd + 1, &read_set, NULL, &exn_set, time_p)) > 0) {
    *sure = ilu_TRUE;

    ILU_THREADS_NOTE(LOCK_DEBUG,
	  ("ilukt_WT_read_wait(%d) got input=%s, exn=%s"
	   " (aux input=%s, exn=%s).\n",
	   fd,
	   (FD_ISSET(fd, &read_set) ? "T" : "F"),
	   (FD_ISSET(fd, &exn_set) ? "T" : "F"),
	   (auxfd >= 0 && FD_ISSET(auxfd, &read_set) ? "T" : "F"),
	   (auxfd >= 0 && FD_ISSET(auxfd, &exn_set) ? "T" : "F")
	   ));

    ILU_CLER(*err);

  } else if (stat == -1) {	/* an error occured; hopefully an
				 * interrupt! */
    int             theerr = sockerrno;
    switch (theerr) {
    case SOCKERRID(INTR):
      *sure = ilu_FALSE;
      ILU_THREADS_NOTE(LOCK_DEBUG,
		       ("ilukt_WT_read_wait(%d) interrupted.\n",
			fd));
      ILU_ERR_CONS1(interrupted, err, ilu_interruptSet, 0, 0);
      break;
    default:
      ASSERT(ilu_FALSE, buf,
	     (buf,
	  "ilukt_WT_read_wait(%d) got select failure, errno=%d=%s",
	      fd, theerr, strerror(theerr)));
    }

  } else {
    *sure = ilu_FALSE;

    ILU_THREADS_NOTE(LOCK_DEBUG,
		     ("ilukt_WT_read_wait(%d) timed out.\n",
		      fd));

    ILU_CLER(*err);
  }

  return;
}


/* ----------------------------------------------- */
/* write wait                                      */

static void
ilukt_WT_write_wait(int fd, int auxfd, ilu_boolean * sure,
	       ilu_FineTime * limit, ILU_ERRS((interrupted)) * err)
{
  fd_set          read_set, write_set, exn_set;
  int             stat, maxfd = fd;
  struct timeval  time, *time_p;

  ILU_THREADS_NOTE(LOCK_DEBUG,
		   ("ilukt_WT_write_wait(%d) entered.\n", fd));

  FD_ZERO(&read_set);
  FD_ZERO(&write_set);
  FD_SET(fd, &write_set);
  FD_ZERO(&exn_set);
  FD_SET(fd, &exn_set);
  if (auxfd >= 0) {
    FD_SET(auxfd, &read_set);
    FD_SET(auxfd, &exn_set);
    if (auxfd > fd)
      maxfd = auxfd;
  }

  if (limit != 0) {
    time.tv_sec = limit->ft_s;
    time.tv_usec = ilu_rescale(limit->ft_t, ilu_FineTimeRate, 1000000);
    time_p = &time;
  } else {
    time_p = NULL;
  }

  stat = select(maxfd + 1, &read_set, &write_set, &exn_set, time_p);
  if (stat > 0) {
    *sure = ilu_TRUE;

    ILU_THREADS_NOTE(LOCK_DEBUG,
	  ("ilukt_WT_write_wait(%d) got output=%s && exn=%s"
	   " (and aux input=%s, exn=%s).\n",
	   fd,
	   (FD_ISSET(fd, &write_set) ? "T" : "F"),
	   (FD_ISSET(fd, &exn_set) ? "T" : "F"),
	   (auxfd >= 0 && FD_ISSET(auxfd, &read_set) ? "T" : "F"),
	   (auxfd >= 0 && FD_ISSET(auxfd, &exn_set) ? "T" : "F")
	   ));

    ILU_CLER(*err);
  } else if (stat == -1) {	/* an error occured; hopefully an
				 * interrupt! */
    int             theerr = sockerrno;
    switch (theerr) {
    case SOCKERRID(INTR):
      *sure = ilu_FALSE;
      ILU_THREADS_NOTE(LOCK_DEBUG,
	    ("ilukt_WT_write_wait(%d) interrupted.\n",
	     fd));
      ILU_ERR_CONS1(interrupted, err, ilu_interruptSet, 0, 0);
      break;
    default:
      ASSERT((theerr == SOCKERRID(INTR)), buf,
	(buf, "ilukt_WT_write_wait(%d):select failed, errno=%d=%s",
	 fd, theerr, strerror(theerr)));
    }

  } else {
    *sure = ilu_FALSE;

    ILU_THREADS_NOTE(LOCK_DEBUG,
		     ("ilukt_WT_write_wait(%d) timed out.\n", fd));

    ILU_CLER(*err);
  }

  return;
}


/* ----------------------------------------------- */
/* WaitTech                                        */

static ilu_WaitTech
  _ilu_threaded_wt = {
    ilukt_WT_read_wait,
    ilukt_WT_write_wait };


/* ----------------------------------------------- */
/* Alarm struct                                    */

typedef struct {
  DECLARE_MUTEX(alarm_mutex);
  DECLARE_CONDITION(alarm_condition);
  ilu_FineTime    fire_time;
  void            (*proc) (ilu_private rock);
  ilu_private     rock;
  ilu_boolean     going;
}               ilukt_Alarm;


/* ----------------------------------------------- */
/* this impedance-matching struct is needed because Solaris wants 
functions called by thr_create to have a return a void * */
typedef struct
{
  void (*proc)(void *arg);
  void *arg;
} function_and_argument;


/* ----------------------------------------------- */
/* function that basically runs the thread's procedure */

DECLARE_THREADKEY(_ilukt_implicit_args_key);

#ifndef WIN32
static void *
  run_function(void *func_and_args)
{
  function_and_argument *proc_args;
  proc_args = (function_and_argument *) func_and_args;

  ILU_NOTE(THREAD_DEBUG,
	("ILU: (threads.c/run_function)(fn = %p, arg = %p) starting.\n",
	 proc_args->proc, proc_args->arg));

  proc_args->proc(proc_args->arg);

  ILU_NOTE(THREAD_DEBUG,
	("ILU: (threads.c/run_function)(fn = %p, arg = %p) finishing.\n",
	 proc_args->proc, proc_args->arg));

  ilu_free(proc_args);
  return NULL;
}
#else
static void (*_ilukt_win32_attribute_finalizer)(void *) = 0;

static void 
  run_function(void *func_and_args)
{
  function_and_argument *proc_args;
  void *val;

  proc_args = (function_and_argument *)func_and_args;

  ILU_NOTE(THREAD_DEBUG,
	("ILU: (threads.c/run_function)(fn = %p, arg = %p) starting.\n",
	 proc_args->proc, proc_args->arg));

  proc_args->proc(proc_args->arg);

  ILU_NOTE(THREAD_DEBUG,
	("ILU: (threads.c/run_function)(fn = %p, arg = %p) finishing.\n",
	 proc_args->proc, proc_args->arg));

  ilu_free(proc_args);
  if (((val = TlsGetValue(_ilukt_implicit_args_key)) != ILU_NIL) &&
      (_ilukt_win32_attribute_finalizer != (void (*)(void *)) 0))
    (*_ilukt_win32_attribute_finalizer)(val);
}
#endif


/* ----------------------------------------------- */
/* run an alarm                                    */

#ifndef WIN32
static void *
#else
static void
#endif
ilukt_ML_run_alarm(void *alarm_in_disguise)
{
  ilukt_Alarm    *alarm = (ilukt_Alarm *) alarm_in_disguise;
#ifdef ILU_SOLARIS2_THREADS
  timestruc_t     set_time;
#endif
#if (defined(ILU_POSIX_THREADS) || defined(ILU_DCE_THREADS))
  struct timespec set_time;
  int             additional_info;
#endif
  int             err;

#ifdef ENABLE_DEBUGGING
  if ((ilu_DebugLevel & THREAD_DEBUG) && (ilu_DebugLevel & MAINLOOP_DEBUG))
  ILU_THREADS_NOTE(TIMING_DEBUG,
		   ("ilukt_ML_run_alarm(%p): started.\n", alarm));
#endif

  ILU_LOCK_MUTEX(alarm->alarm_mutex);

  while (alarm->going) {
    if (alarm->fire_time.ft_s != 0) {

#ifdef ILU_WIN32_THREADS
      ilu_FineTime    the_time_now, delta;
      static ilu_FineTime zeroFT = {0, 0};
      DWORD           relative_time_in_msec;
      the_time_now = ilu_FineTime_Now();
      delta = ilu_FineTime_Sub(alarm->fire_time, the_time_now);
      if (ilu_FineTime_Cmp(delta, zeroFT) > 0) {
	relative_time_in_msec = delta.ft_s * 1000
	  + ilu_rescale(delta.ft_t, ilu_FineTimeRate, 1000);

	ILU_THREADS_NOTE(TIMING_DEBUG,
	      ("ilukt_ML_run_alarm(%p): waiting for (%ld) milliseconds.\n",
	       alarm, relative_time_in_msec));

	err = CONDITION_TIMEDWAIT(alarm->alarm_condition,
				  alarm->alarm_mutex,
				  relative_time_in_msec);
      } else
	err = WAIT_TIMEOUT;

#else
      set_time.tv_sec = alarm->fire_time.ft_s;
      set_time.tv_nsec = ilu_rescale(alarm->fire_time.ft_t,
				     ilu_FineTimeRate, 1000000000);

      ILU_THREADS_NOTE(TIMING_DEBUG,
	    ("ilukt_ML_run_alarm(%p): waiting for t=%ld.%09ld.\n",
	     alarm, set_time.tv_sec, set_time.tv_nsec));

      err = CONDITION_TIMEDWAIT(alarm->alarm_condition,
				alarm->alarm_mutex,
				set_time);

#endif


      if (
#ifdef ILU_SOLARIS2_THREADS
	  err == ETIME
#endif
#if (defined(ILU_POSIX_THREADS) || defined(ILU_DCE_THREADS))
	  err == ETIME		/* on Solaris 2 */
	  || err == ETIMEDOUT	/* on Linux, AIX */
#endif
#ifdef ILU_WIN32_THREADS
	  err == WAIT_TIMEOUT
#endif
	) {
	/* Timeout.  Fire proc, with mutex unlocked.  */

	alarm->fire_time.ft_s = 0;
	ILU_UNLOCK_MUTEX(alarm->alarm_mutex);

	ILU_THREADS_NOTE(TIMING_DEBUG,
		     ("ilukt_ML_run_alarm(%p): firing!\n", alarm));

	alarm->proc(alarm->rock);

	ILU_LOCK_MUTEX(alarm->alarm_mutex);

	ILU_THREADS_NOTE(TIMING_DEBUG,
	    ("ilukt_ML_run_alarm(%p): mutex re-locked.\n", alarm));

      } else {
	char            buf[64];
	if (err)
	  sprintf(buf, "wait returned %d in ilukt_ML_run_alarm", err);
	_ilu_Assert(!err, buf);
	/*
	 * Normal termination; this means that the condition
	 * variable was signalled (i.e. the alarm was reset), so
	 * we'll just loop back to the top and reset the time
	 */
	ILU_THREADS_NOTE(TIMING_DEBUG,
	      ("ilukt_ML_run_alarm(%p): woken early; rethinking.\n",
	       alarm));
      }
    } else {
      /*
       * The alarm must've been unset, so wait until something else
       * happens.
       */
      ILU_THREADS_NOTE(TIMING_DEBUG,
	    ("ilukt_ML_run_alarm(%p): waiting for alarm to be set.\n",
	     alarm));
      CONDITION_WAIT(alarm->alarm_condition, alarm->alarm_mutex);
    }
  }
  if (!alarm->going) {
    DESTROY_CONDITION(alarm->alarm_condition);
    ILU_UNLOCK_MUTEX(alarm->alarm_mutex);
    DESTROY_MUTEX(alarm->alarm_mutex);
    ilu_free(alarm);
  }

#ifndef WIN32
  return NIL;
#else
#endif
}


/* ----------------------------------------------- */
/* Create an alarm                                 */

static ilu_refany  ilukt_ML_create_alarm(void)
{
  ilukt_Alarm    *new_alarm = ilu_malloc(sizeof(ilukt_Alarm));
  int             status;
  DECLARE_THREAD(thread_id);

  if (new_alarm == NIL) {
    ILU_THREADS_NOTE(TIMING_DEBUG,
	  ("ilukt_ML_create_alarm:  can't allocate space for alarm\n"));
    return NIL;			/* couldn't allocate space, so
				 * return */
  }
  memset(new_alarm, 0, sizeof(*new_alarm));

  if (!INIT_CONDITION(new_alarm->alarm_condition)) {
    ilu_free(new_alarm);
    ILU_THREADS_NOTE(TIMING_DEBUG,
	  ("ilukt_ML_create_alarm:  "
	   "something went wrong with initialization of condition variable\n"));
    return NIL;
  }
  if (!INIT_MUTEX(new_alarm->alarm_mutex)) {
    ILU_THREADS_NOTE(TIMING_DEBUG,
	  ("ilukt_ML_create_alarm:  "
	   "something went wrong with mutex initialization,"
	   " so clean up and fail\n"));
    DESTROY_CONDITION(new_alarm->alarm_condition);
    ilu_free(new_alarm);
    return NIL;
  }
  new_alarm->going = ilu_TRUE;
  status = DISPATCH_THREAD(ilukt_ML_run_alarm, (void *) new_alarm,
			   thread_id);
  if (status != 0) {
    ILU_THREADS_NOTE(TIMING_DEBUG,
	  ("ilukt_ML_create_alarm:  "
	   "couldn't spawn a new thread (errno %d)\n",
	   status));
    DESTROY_CONDITION(new_alarm->alarm_condition);
    DESTROY_MUTEX(new_alarm->alarm_mutex);
    ilu_free(new_alarm);
    return NIL;
  }
  status = DETACH_THREAD(thread_id);
  if (status != 0) {
    ILU_THREADS_NOTE(TIMING_DEBUG,
	  ("ilukt_ML_create_alarm:  "
	   "couldn't detach new thread (errno %d)\n",
	   status));
    new_alarm->going = ilu_FALSE;
    return NIL;
  }
  ILU_THREADS_NOTE(TIMING_DEBUG,
	("ilukt_ML_create_alarm:  new alarm %p\n",
	 new_alarm));
  return new_alarm;
}


/* ----------------------------------------------- */
/* Set an alarm                                    */

static void
ilukt_ML_set_alarm(ilu_refany alarm_in_disguise, ilu_FineTime t,
		   void (*proc) (ilu_private rock),
		   ilu_private rock)
{
  ilukt_Alarm    *alarm = (ilukt_Alarm *) alarm_in_disguise;

  ILU_THREADS_NOTE(TIMING_DEBUG,
		   ("ilukt_ML_set_alarm(%p): called.\n", alarm));

  _ilu_Assert(alarm->going, "ilukt_ML_set_alarm vs. going 1");
  ILU_LOCK_MUTEX(alarm->alarm_mutex);
  _ilu_Assert(alarm->going, "ilukt_ML_set_alarm vs. going 2");

  ILU_THREADS_NOTE(TIMING_DEBUG,
		   ("ilukt_ML_set_alarm(%p): mutex locked.\n",
		    alarm));

  alarm->fire_time = t;
  alarm->proc = proc;
  alarm->rock = rock;

  CONDITION_BROADCAST(alarm->alarm_condition);
  ILU_UNLOCK_MUTEX(alarm->alarm_mutex);

  ILU_THREADS_NOTE(TIMING_DEBUG,
	      ("ilukt_ML_set_alarm(%p): signalled and unlocked.\n",
	       alarm));

  return;
}


/* ----------------------------------------------- */
/* Unset an alarm                                  */

static void
  ilukt_ML_unset_alarm(ilu_refany alarm_in_disguise)
{
  ilukt_Alarm    *alarm = (ilukt_Alarm *) alarm_in_disguise;

  ILU_THREADS_NOTE(TIMING_DEBUG,
		   ("ilukt_ML_unset_alarm(%p): called.\n", alarm));

  _ilu_Assert(alarm->going, "ilukt_ML_unset_alarm vs. going 1");
  ILU_LOCK_MUTEX(alarm->alarm_mutex);
  _ilu_Assert(alarm->going, "ilukt_ML_unset_alarm vs. going 2");

  ILU_THREADS_NOTE(TIMING_DEBUG,
		   ("ilukt_ML_unset_alarm(%p): mutex locked.\n",
		    alarm));

  alarm->fire_time.ft_s = 0;

  CONDITION_BROADCAST(alarm->alarm_condition);
  ILU_UNLOCK_MUTEX(alarm->alarm_mutex);

  ILU_THREADS_NOTE(TIMING_DEBUG,
		   ("ilukt_ML_unset_alarm(%p): cond. signalled and mutex unlocked.\n",
		    alarm));

  return;
}


/* ----------------------------------------------- */
/* Destroy an alarm                                */

static void
  ilukt_ML_destroy_alarm(ilu_refany alarm_in_disguise)
{
  ilukt_Alarm    *alarm = (ilukt_Alarm *) alarm_in_disguise;

  ILU_THREADS_NOTE(TIMING_DEBUG,
	("ilukt_ML_destroy_alarm(%p): called.\n",
	 alarm));

  _ilu_Assert(alarm->going, "ilukt_ML_destroy_alarm vs. going 1");
  ILU_LOCK_MUTEX(alarm->alarm_mutex);
  _ilu_Assert(alarm->going, "ilukt_ML_destroy_alarm vs. going 2");

  ILU_THREADS_NOTE(TIMING_DEBUG,
	("ilukt_ML_destroy_alarm(%p): mutex locked.\n",
	 alarm));

  alarm->going = 0;

  CONDITION_BROADCAST(alarm->alarm_condition);
  ILU_UNLOCK_MUTEX(alarm->alarm_mutex);

  ILU_THREADS_NOTE(TIMING_DEBUG,
	("ilukt_ML_destroy_alarm(%p): cond. signalled and mutex unlocked.\n",
	 alarm));

  return;
}


/* ----------------------------------------------- */
/* ilu_MainLoop                                    */

static ilu_MainLoop
  _ilu_threaded_ml = {
    NULL, NULL, NULL, NULL, NULL, NULL, 
    ilukt_ML_create_alarm,
    ilukt_ML_set_alarm,
    ilukt_ML_unset_alarm,
    ilukt_ML_destroy_alarm };


/* ----------------------------------------------- */
/* per-thread state variables                      */

static void *
  ilukt_GetPerThreadArgs (void)
{
  void *pv_data = ILU_NIL;

  return GET_THREAD_DATA(pv_data);
}


static void
  ilukt_SetPerThreadArgs (const void * pv_args,
			  ILU_ERRS((no_memory, internal)) *err)
{
  if (SET_THREAD_DATA(pv_args)) 
    ILU_CLER(*err);
  else
    ILU_ERR_CONS1(internal, err, minor, ilu_im_threadAttribute, 0);
}


ilu_boolean
  ilu_OSThreads_GetPerThreadDataTech (void (*destructor) (void *),
				      void *(**get_attributes)(void),
				      void (**set_attributes)(const void *, ILU_ERRS((no_memory, internal)) *),
				      ILU_ERRS((no_memory, internal)) *err)
{
  static ilu_boolean initialized = ilu_FALSE;

  if (!initialized) {

    if (!CREATE_THREAD_DATA_KEY(destructor)) {
	  ILU_ERR_CONS1(internal, err, minor, ilu_im_threadAttribute, 0);
	  return ilu_FALSE;
    }
    initialized = ilu_TRUE;
  };
  *get_attributes = ilukt_GetPerThreadArgs;
  *set_attributes = ilukt_SetPerThreadArgs;
  ILU_CLER(*err);
  return ilu_TRUE;
}

/* ----------------------------------------------- */
/* Accessors for the data structures               */

void
  ilu_OSThreads_GetTech (ilu_WaitTech **waittech,
			 ilu_LockTech **locktech,
			 ilu_MainLoop **mainloop)
{
  *waittech = &_ilu_threaded_wt;
  *locktech = &_ilu_threaded_lt;
  *mainloop = &_ilu_threaded_ml;
}

/* ----------------------------------------------- */
/* ilu_InitializeOSThreading                   */

ilu_boolean
  ilu_InitializeOSThreading(ILU_ERRS((bad_param, no_memory,
				      no_resources, internal)) * err)
{
  static ilu_boolean initialized = ilu_FALSE;

  if (initialized)
    return ILU_CLER(*err);
  initialized = ilu_TRUE;

  ilu_SetWaitTech(&_ilu_threaded_wt);
  ilu_SetLockTech(&_ilu_threaded_lt, err);
  if (ILU_ERRNOK(*err)) {
    ILU_HANDLED(*err);
    return (ilu_FALSE);
  }
  ilu_SetForkTech(&ilu_OSForkNewThread, err);
  if (ILU_ERRNOK(*err)) {
    ILU_HANDLED(*err);
    return (ilu_FALSE);
  }
  ilu_SetMainLoop(&_ilu_threaded_ml);
  ilu_SetThreadPrinter(PrintCurrentThreadID);
  return ilu_TRUE;
}

/* ----------------------------------------------- */
/* ilu_OSForkNewThread                             */

ilu_boolean
  ilu_OSForkNewThread (void (*proc)(void *arg), void *arg,
		       ILU_ERRS((no_memory, no_resources,
				 internal)) *err)
{
  return ilu_OSForkNewThreadEx (proc, arg, NULL, err);
}


/* ----------------------------------------------- */
/* ilu_OSForkNewThreadEx - returns thread id in 
   p_thread if p_thread is non null and we're 
   successfull-- assumes that *pv_thread        
   is large enough to hold a thread id on the system.
*/

ilu_boolean
  ilu_OSForkNewThreadEx (void (*proc)(void *arg), void *arg,
		       void* pv_thread, 
			   ILU_ERRS((no_memory, no_resources, internal)) *err)
{
  int             status;
  DECLARE_THREAD(thread_id);
  function_and_argument *func_args;
  func_args = ilu_MallocE(sizeof(*func_args), err);
  if (ILU_ERRNOK(*err))
    return ilu_FALSE;

  func_args->proc = proc;
  func_args->arg = arg;

  status = DISPATCH_THREAD(run_function, func_args, thread_id);
  if (status != 0) {
    ILU_NOTE(THREAD_DEBUG,
	  ("ilu_OSForkNewThread: OS-dispatch(%p, %p) returns error code %d\n",
	   proc, arg, status));
    return ILU_ERR_CONS1(internal, err, minor, ilu_im_threadFork,
			 ilu_FALSE);
  }
#ifdef ENABLE_DEBUGGING
  if (ilu_DebugLevel & THREAD_DEBUG) {
    char            buf[THDIDSZ(thread_id)];
    WriteThreadID(buf, thread_id);
    ilu_DebugPrintf("ilu_OSForkNewThread(%p, %p) = %s\n",
		    proc, arg, buf);
  }
#endif
  status = DETACH_THREAD(thread_id);
  if (status != 0) {
    ILU_NOTE(THREAD_DEBUG,
	  ("ilu_OSForkNewThread: OS-detach(new thread) returns error code %d\n",
	   status));
    return ILU_ERR_CONS1(internal, err, minor, ilu_im_threadFork,
			 ilu_FALSE);
  }

  if (pv_thread) 
	  *((THREAD_TYPE*) pv_thread) = thread_id;

  return ILU_CLER(*err);
}


#if 0
/* ----------------------------------------------- */
/* ilu_OSThreadID                                  */

ilu_refany
  ilu_OSThreadID (ILU_ERRS((no_memory, no_resources,
			    internal)) *err)
{
  DECLARE_THREAD(current_thread);
  if (sizeof(current_thread) > sizeof(ilu_refany)) {
    ILU_NOTE(THREAD_DEBUG,
	     ("ilu_OSThreadID:  Can't use ILU kernel threads with this thread system implementation."));
    return ILU_ERR_CONS1(internal, err, minor, ilu_im_threadIDSize, ILU_NIL);
  } else {
    ILU_CLER(*err);
    return ((ilu_refany) GET_CURRENT_THREAD());
  }
}
#endif


#ifdef WIN32

/* ----------------------------------------------- */
/*  ShowLastError                                  */

#ifdef ENABLE_DEBUGGING
DWORD ShowLastError(char* pc_file, int i_line) {
	DWORD d_result = GetLastError();
	ILU_NOTE(THREAD_DEBUG, ("ILU: GetLastError() = 0x%x in %s, line %d\n", d_result, pc_file, i_line));
	return d_result;
}
DWORD ShowLastErrorDebugSafe(HANDLE m, char* pc_file, int i_line) {
	DWORD d_result = GetLastError();
	if (m == ((ilukt_Mutex *)ilu_debugmu)->mutex)
		return d_result;
	ILU_NOTE(THREAD_DEBUG, ("ILU: GetLastError() = 0x%x in %s, line %d\n", d_result, pc_file, i_line));
	return d_result;
}
#endif /* ENABLE_DEBUGGING */
#endif /* WIN32 */

/* ----------------------------------------------- */
/* end of file                                     */
/* ----------------------------------------------- */
