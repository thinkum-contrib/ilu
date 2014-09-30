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
/* $Id: pythonthreads.h,v 1.13 1999/08/03 01:55:41 janssen Exp $ */

#ifdef ILU_PYTHON_THREADS
extern ilu_boolean ilupython_threaded_operation;

#if ((ILUPYTHON_MAJOR_VERSION == 1) && (ILUPYTHON_MINOR_VERSION >= 5))

void _ilupython_new_thread_enter_interpreter(void);
void _ilupython_dying_thread_release_interpreter(void);

#define EXIT_INTERPRETER(if_what) { PyThreadState *_save = ILU_NIL; { if (if_what) { _save = PyEval_SaveThread(); _ilu_Assert(_save!=ILU_NIL,"NIL thread state when releasing Python interpreter lock"); } }
#define ENTER_INTERPRETER(if_what) { if (if_what) { _ilu_Assert(_save != ILU_NIL,"NIL thread state when attempting to regain interpreter lock"); PyEval_RestoreThread(_save); } } }
#define NEW_THREAD_ENTER { { if (ilupython_threaded_operation) _ilupython_new_thread_enter_interpreter(); }
#define FINISHED_THREAD_EXIT { if (ilupython_threaded_operation) { _ilupython_dying_thread_release_interpreter(); } } }

#else

#define PyThread_start_new_thread start_new_thread
#define PyThread_init_thread init_thread
#define PyThread_exit_thread exit_thread
#define PyThread_get_thread_ident get_thread_ident

#define EXIT_INTERPRETER(if_what) { PyObject *_save; { if (if_what) { _save = PyEval_SaveThread();  } }
#define ENTER_INTERPRETER(if_what) { if (if_what) { PyEval_RestoreThread(_save); } } }
#define NEW_THREAD_ENTER { { if (ilupython_threaded_operation) PyEval_RestoreThread(NULL); }
#define FINISHED_THREAD_EXIT { if (ilupython_threaded_operation) { if (PyEval_SaveThread() != NULL) { fprintf(stderr, "unexpected non-NULL stack from exiting thread\n"); } } } }

#endif /* ((ILUPYTHON_MAJOR_VERSION >= 1) && (ILUPYTHON_MINOR_VERSION >= 5)) */

#define CALL_KERNEL(unblock_cond, call) { EXIT_INTERPRETER(unblock_cond); call; ENTER_INTERPRETER(unblock_cond); }
#define ILUPY_ILLEGAL_IN_THREADED(unless_what) if (ilupython_threaded_operation && !unless_what) { char buf[1000]; sprintf(buf, "illegal internal call in threaded runtime:  " # unless_what); Py_FatalError(buf); }
#define ILUPY_ILLEGAL_IN_UNTHREADED if (!ilupython_threaded_operation) _ilu_Assert(0, "illegal internal call in single-threaded runtime"); 
#else
#define ilupython_threaded_operation ilu_FALSE
#define CALL_KERNEL(unblock_cond, call) call
#define EXIT_INTERPRETER(if_what)
#define ENTER_INTERPRETER(if_what)
#define ILUPY_ILLEGAL_IN_THREADED(unless_what)
#define ILUPY_ILLEGAL_IN_UNTHREADED _ilu_Assert(0, "illegal internal call in single-threaded runtime");
#define NEW_THREAD_ENTER
#define FINISHED_THREAD_EXIT
#endif /* ILU_PYTHON_THREADS */

