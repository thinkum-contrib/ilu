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
/* $Id: threads.h,v 1.28 1999/08/03 01:53:16 janssen Exp $ */
/* Last edited by Mike Spreitzer June 17, 1996 8:55 am PDT */

#ifndef _THREADS_H
#define _THREADS_H

#if !defined(ILU_SOLARIS2_THREADS) && !defined(ILU_POSIX_THREADS) && !defined(ILU_WIN32_THREADS) && !defined(ILU_DCE_THREADS)
#error One of { ILU_SOLARIS2_THREADS, ILU_POSIX_THREADS, ILU_WIN32_THREADS, ILU_DCE_THREADS } should be defined!
#endif


/* ************************************************************** */
/* Solaris */

#ifdef ILU_SOLARIS2_THREADS
#include <synch.h>
#include <thread.h>

#define DECLARE_MUTEX(m) mutex_t m
#define DECLARE_CONDITION(c) cond_t c
#define THREAD_TYPE thread_t
#define DECLARE_THREAD(t) thread_t t
#define DECLARE_THREADKEY(k) thread_key_t k

#define INIT_MUTEX(m) (mutex_init(&m, USYNC_THREAD, NIL) == 0)
#define DESTROY_MUTEX(m) (mutex_destroy(&m))
#define LOCK_MUTEX(m) (mutex_lock(&m))
#define UNLOCK_MUTEX(m) (mutex_unlock(&m))

#define GET_CURRENT_THREAD() thr_self()
#define SAME_THREAD(x, y) (x == y)

#define INIT_CONDITION(c) (cond_init(&c, USYNC_THREAD, 0) == 0)
#define DESTROY_CONDITION(c) (cond_destroy(&c))
#define CONDITION_BROADCAST(c) (cond_broadcast(&c))
#define CONDITION_WAIT(c, m) (cond_wait(&c, &m))
#define CONDITION_TIMEDWAIT(c, m, t) (cond_timedwait(&c, &m, &t))

#define DISPATCH_THREAD(function, args, id) (thr_create(NIL, 0, function, args, THR_DETACHED, &id))
#define DETACH_THREAD(thd) 0

#define GET_THREAD_DATA(pv_data) ((thr_getspecific(_ilukt_implicit_args_key, &pv_data) == 0) ? pv_data : ILU_NIL)
#define SET_THREAD_DATA(pv_data) (thr_setspecific(_ilukt_implicit_args_key, (void *) pv_data) == 0)
#define CREATE_THREAD_DATA_KEY(destructor) (thr_keycreate(&_ilukt_implicit_args_key, destructor) == 0)

#if defined(ILU_POSIX_THREADS) || defined(ILU_WIN32_THREADS) || defined(ILU_DCE_THREADS)
#error Only one of { ILU_POSIX_THREADS, ILU_SOLARIS2_THREADS, ILU_WIN32_THREADS, ILU_DCE_THREADS } should be defined.
#endif  /* ILU_POSIX_THREADS  || ILU_WIN32_THREADS*/

#endif /* ILU_SOLARIS2_THREADS */


/* ************************************************************** */
/* Posix */

#if (defined(ILU_POSIX_THREADS) || defined(ILU_DCE_THREADS))

/* For our purposes, POSIX threads are POSIX draft 10, and
   DCE threads are POSIX draft 4.

   Note -- this pthreads code was developed on solaris 2.5
*/

#include <pthread.h>

/* There are some minor differences between DCE threads and
   POSIX threads which we capture here */

#ifdef ILU_DCE_THREADS

#define ILU_DEFAULT_PTHREAD_ATTR	pthread_attr_default
#define ILU_DEFAULT_PTHREAD_MUTEXATTR	pthread_mutexattr_default
#define ILU_DEFAULT_PTHREAD_CONDATTR	pthread_condattr_default
#define DETACH_THREAD(thd) pthread_detach(&thd)
#define GET_THREAD_DATA(pv_data) ((void*)(pthread_getspecific(_ilukt_implicit_args_key, ((pthread_addr_t)&pv_data)) == 0) ? pv_data : ILU_NIL)
#define CREATE_THREAD_DATA_KEY(destructor) (pthread_keycreate(&_ilukt_implicit_args_key, destructor) == 0)
#define SET_THREAD_DATA(pv_data) (pthread_setspecific(_ilukt_implicit_args_key, ((pthread_addr_t)pv_data)) == 0)

#else /* def ILU_DCE_THREADS */

#define ILU_DEFAULT_PTHREAD_ATTR	NULL
#define ILU_DEFAULT_PTHREAD_MUTEXATTR	NULL
#define ILU_DEFAULT_PTHREAD_CONDATTR	NULL
#define DETACH_THREAD(thd) pthread_detach(thd)
#define GET_THREAD_DATA(pv_data) ((void*)(pthread_getspecific(_ilukt_implicit_args_key)))
#define CREATE_THREAD_DATA_KEY(destructor) (pthread_key_create(&_ilukt_implicit_args_key, destructor) == 0)
#define SET_THREAD_DATA(pv_data) (pthread_setspecific(_ilukt_implicit_args_key, pv_data) == 0)

#endif /* def ILU_DCE_THREADS */

/* These other declarations are the same for DCE (POSIX draft 4)
   and POSIX (POSIX draft 10) */

#define DECLARE_MUTEX(m) pthread_mutex_t m
#define DECLARE_CONDITION(c) pthread_cond_t c
#define THREAD_TYPE pthread_t
#define DECLARE_THREAD(t) pthread_t t
#define DECLARE_THREADKEY(k)	pthread_key_t k

#define INIT_MUTEX(m) (pthread_mutex_init(&m, ILU_DEFAULT_PTHREAD_MUTEXATTR) == 0)
#define DESTROY_MUTEX(m) (pthread_mutex_destroy(&m))
#define LOCK_MUTEX(m) (pthread_mutex_lock(&m))
#define UNLOCK_MUTEX(m) (pthread_mutex_unlock(&m))

#define GET_CURRENT_THREAD() (pthread_self())
#define SAME_THREAD(x, y) pthread_equal(x, y)

#define INIT_CONDITION(c) (pthread_cond_init(&c, ILU_DEFAULT_PTHREAD_CONDATTR) == 0)
#define DESTROY_CONDITION(c) (pthread_cond_destroy(&c))
#define CONDITION_BROADCAST(c) (pthread_cond_broadcast(&c))
#define CONDITION_WAIT(c, m) (pthread_cond_wait(&c, &m))
#define CONDITION_TIMEDWAIT(c, m, t) (pthread_cond_timedwait(&c, &m, &t))

#define DISPATCH_THREAD(function, args, id) (pthread_create(&id, ILU_DEFAULT_PTHREAD_ATTR, function, args))

#if defined(ILU_SOLARIS2_THREADS) || defined(ILU_WIN32_THREADS)
#error Only one of { ILU_POSIX_THREADS, ILU_SOLARIS2_THREADS, ILU_WIN32_THREADS, ILU_DCE_THREADS } should be defined.
#endif /* ILU_SOLARIS2_THREADS || ILU_WIN32_THREADS*/

#endif /* ILU_POSIX_THREADS */


/* ************************************************************** */
/* Win32 */


#ifdef ILU_WIN32_THREADS
#include <windows.h>

#ifdef ENABLE_DEBUGGING
DWORD ShowLastError(char* pc_file, int i_line);
DWORD ShowLastErrorDebugSafe(HANDLE m, char* pc_file, int i_line);
#define WIN32_GETLASTERROR() ShowLastError(__FILE__, __LINE__)
#define WIN32_GETLASTERROR_DEBUGSAFE(m) ShowLastErrorDebugSafe(m, __FILE__, __LINE__)
#else
#define WIN32_GETLASTERROR() GetLastError()
#define WIN32_GETLASTERROR_DEBUGSAFE(m) GetLastError()
#endif

#define DECLARE_MUTEX(m) HANDLE m
#define DECLARE_CONDITION(c) Condition c
#define THREAD_TYPE DWORD
#define DECLARE_THREAD(t) DWORD t
#define DECLARE_THREADKEY(k) DWORD k

#define INIT_MUTEX(m) ((m = CreateMutex(NULL, ilu_FALSE, NULL)) != NULL)
#define DESTROY_MUTEX(m) ((CloseHandle(m) == ilu_TRUE) ? 0 : WIN32_GETLASTERROR_DEBUGSAFE(m))
#define LOCK_MUTEX(m) ((WaitForSingleObject(m, INFINITE) != WAIT_FAILED) ? 0 : WIN32_GETLASTERROR_DEBUGSAFE(m))
#define UNLOCK_MUTEX(m) ((ReleaseMutex(m) == ilu_TRUE) ? 0 : WIN32_GETLASTERROR_DEBUGSAFE(m))

#define GET_CURRENT_THREAD() GetCurrentThreadId()
#define SAME_THREAD(x, y) (x == y)

#define INIT_CONDITION(c) (condition_init(&c) == 0)
#define DESTROY_CONDITION(c) (condition_destroy(&c))
#define CONDITION_BROADCAST(c) (condition_broadcast(&c))
#define CONDITION_SIGNAL(c) (condition_signal(&c))
#define CONDITION_WAIT(c, m) (condition_wait(&c, &m, INFINITE))
#define CONDITION_TIMEDWAIT(c, m, t) (condition_wait(&c, &m, t))

/*
 * Note that _beginthread is oddly declared to return a -1 on error
 * when it's signature says it returns an unsigned long (DWORD) !
 */
#define DISPATCH_THREAD(function, args, id) (((id = _beginthread(function, 0, args)) != ((DWORD)-1)) ? 0 : errno)

/* don't need to do anything for detach since using _beginthread to start the thread
   will cause the thread handle to be closed when the function to be run returns */
#define DETACH_THREAD(thd) 0

#define GET_THREAD_DATA(pv_data) ((void *)TlsGetValue(_ilukt_implicit_args_key))
#define SET_THREAD_DATA(pv_data) (TlsSetValue(_ilukt_implicit_args_key, (LPVOID) pv_data))
#define CREATE_THREAD_DATA_KEY(destructor) (((_ilukt_implicit_args_key = TlsAlloc()) != 0xFFFFFFFF) ? _ilukt_win32_attribute_finalizer = destructor, 1 : 0)

#if defined(ILU_POSIX_THREADS) || defined(ILU_SOLARIS2_THREADS) || defined(ILU_DCE_THREADS)
#error Only one of { ILU_POSIX_THREADS, ILU_SOLARIS2_THREADS, ILU_WIN32_THREADS } should be defined.
#endif  /* ILU_POSIX_THREADS || ILU_SOLARIS2_THREADS */

#endif /* ILU_WIN32_THREADS */


#endif /* _THREADS_H */



