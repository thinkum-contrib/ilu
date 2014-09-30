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

  $Id: iluPrmodule.c,v 1.297 1999/09/09 17:59:52 janssen Exp $
  */

#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>  /* for gid_t */
#include <ctype.h>	/* for toupper */

/* from ILU */
#include <iluxport.h>
#ifdef ILU_FIXED_POINT_SUPPORT
#include <ilubnops.h>
#endif /* def ILU_FIXED_POINT_SUPPORT */
/* local */
#include "python.h"
#include "ilualobject.h"
#include "ilucaobject.h"
#include "iluclobject.h"
#include "iluftobject.h"
#include "ilulrobject.h"
#include "ilusvobject.h"
#include "iluszobject.h"
#include "iluplobject.h"
#include "iluppobject.h"
#include "ilulpobject.h"
#include "ilugiobject.h"
#include "iohcobject.h"
#include "ivobject.h"
#include "thcobject.h"
#include "pythonthreads.h"

#define PYTHON_RETRY_CALL_CODE      0XFFFF

#ifdef ADD_TYPE_REGISTRATION_SUPPORT
#include "ilutpobject.h"
#endif /* def ADD_TYPE_REGISTRATION_SUPPORT */

#ifdef SECURE_TRANSPORT
#include "ilupygss.h"
#endif

/* from Python */
#include "sysmodule.h"
#include "intrcheck.h"  /* for PyOS_InterruptOccurred */
#include "cobject.h"	/* for PyCObject */

#define Py_COUNT(x) (((PyObject *)(x))->ob_refcnt)

static ilu_boolean readServiceRequest(ilu_refany, ilu_boolean);
static void runConnection(void *arg);
static void readConnectionRequests(void *arg);
static ilu_TransportInfo getInmemTransport(void);
static void cleanupUninterestingObjects (ilu_private junk);
static void singleThreadedReadServiceRequest (ilu_refany);
static void inmemReadServiceRequest (ilu_refany);

#define ContextSlots_Serializer	0
#define ContextSlots_Pipeline	1
#define ContextSlots_Passport	2

#define CORBA_COMPLETED_MAYBE	2

/***********************************************************************
 ************************************************************************
 ***********************************************************************/

#ifdef ILU_PYTHON_THREADS

/* 
  Python threading is achieved by adding the global interpreter lock
  to the partial order of mutexes as follows:
  X < global interpreter lock, for all X

  That is, there are no restrictions on what mutexes a thread may be holding
  when it attempts to enter the Python interpreter, but a thread may not enter
  any mutexes while holding the global interpter lock.  

  There can be no deadlocks on waiting for the global interpter lock
  because whichever thread is holding the global interpreter lock
  must relinquish it before entering any other mutexes.

  There can be no deadlocks waiting for ILU mutexes because a thread
  cannot hold the global interpter lock while trying to enter into a
  mutex, so the thread cannot prevent the thread holding these
  mutexes from proceeding.

  In addition to avoiding deadlock, we attempt to avoid long delays
  in waiting for the global interpter lock by releasing the lock
  around calls into the ILU kernel that might potentially block or
  otherwise take a long time.
  */

#include "oscalls.h" /* for OS_SLEEP */

#include "pythonthreadheaderfile.h"	/* either thread.h or pythread.h */

static void bootstrapAlarm(ilu_private);
static PyObject *ilupython_thread_push_call(PyObject *, PyObject *);
static void ilupython_thread_pop_call(PyObject *);
static PyObject *ilupython_thread_current_call(void);
static PyObject *ilupython_thread_current_context(void);
static PyObject *ilupython_thread_init_stack(PyObject *tid);
static void ilupython_collect_outgoing_connections(void *arg);
static void ilupython_watch_outgoing_connection(void *arg);

/* start_new_thread is defined by Python but has no "Py_" synonym */
#define ilupython_fork_thread(proc, arg) (PyThread_start_new_thread(proc, arg)) 
  static PyObject *thread_call_stack_map;

/* This flag is also accessed by ivobject.c */
ilu_boolean ilupython_threaded_operation = ilu_FALSE;

/* This dict is used to store Python per-thread context info */
static PyObject *ilupython_ContextDict = ILU_NIL;

/* This dict is used to store arbitrary other language per-thread context info */
static PyObject *ilupython_OtherLangDict = ILU_NIL;

typedef struct
{
  void (*real_proc)(ilu_private arg);
  ilu_private arg;
} bootstrap_rock;

static bootstrap_rock gc_alarm_rock = { cleanupUninterestingObjects, ILU_NIL };

static ilu_Mutex ilupython_MainLoopMutex = ILU_NIL;
static ilu_Condition ilupython_MainLoopCondition = ILU_NIL;

#define current_thread_id() PyInt_FromLong(PyThread_get_thread_ident())

#if ((ILUPYTHON_MAJOR_VERSION >= 1) && (ILUPYTHON_MINOR_VERSION >= 5))
  PyInterpreterState *_ilupython_interpreter_state = ILU_NIL;
#endif

#endif /* ILU_PYTHON_THREADS */

/***********************************************************************
 ************************************************************************
 ***********************************************************************/

/* DM 24.4.97 (dieter@hit.handshake.de): 
   Improved Object Management

   Object Management for Python objects is difficult and error prone.
   It is based on reference counts. There are two kinds of references
   *OWNED* references and *BORROWED* references. The reference count
   in an object *MUST* reflect the number of *OWNED* references to this
   object.

   Usually, when a function is called, it gets borrowed references as parameters.
   When a function returns an object reference, this usually is an *OWNED*
   reference. Unfortunately, there are major exceptions:
   * PyTuple_GetItem, PyList_GetItem, PyDict_GetItem[String], PySys_GetObject
   return borrowed references
   * PyTuple_SetItem, PyList_SetItem (*BUT NOT* PyDict_SetItem[String]!)
   get owned references as value parameter

   Special requirements are associated with both kinds of references.
   * Borrowed Reference: the function must ensure, that this reference
   can only be used until the original owned reference might be deleted.
   This usually requires, that borrowed references cannot be returned
   as return values nor stored in non-local data structures or variables.
   * Owned Reference: the function must ensure, that the reference becomes
   a borrowed reference before it is deleted (either explicitly through
   assignment or implicitly by leaving the scope of the containing
   variable or deletion of the containing data structure.

   A borrowed reference becomes an owned reference through "Py_INCREF" and reverts
   again through "Py_DECREF".

   When internal functions provide access to data structures, it is often
   convenient to deviate from the default reference passing conventions.
   In this case, for high quality and safe code is is essential to clearly
   document the type of reference passed. Otherwise, there is extreme danger, that
   either memory corruption occurs because borrowed references are used although
   the associated object has already been released, or that memory leaks occur
   because owned references are lost and the associated objects may never be
   released.

   We use "OWNED" and "BORROWED" to document reference passing that deviates
   from the usual conventions. More precisely:
   * when an *OWNED* reference is passed over as parameter to a function,
   then the corresponding formal parameter is marked by "OWNED".
   Passing borrowed references is the default; such parameters are not
   marked specially.
   * when a *BORROWED* reference is returned as function value, this is
   marked by "BORROWED" in the return type specification of the
   function definition.
   Returning owned references is the default and not marked specially.

   ATTENTION:
   Although I have taken great care to annotate the reference passing correctly
   and to insert appropriate "Py_INCREF" and "Py_DECREF", there are almost surely
   some bugs. The annotation and code changes have been *TESTED IN ONLY VERY
   LIMITED WAY*.
   Especially, "SetMainLoop" and alarm related functions *HAVE NOT BEEN TESTED*
   at all.
   */

#define OWNED(x)        x
#define BORROWED(x)     x

  /***********************************************************************
   ************************************************************************
   ***********************************************************************/

#if (ILUPYTHON_MAJOR_VERSION == 1 && ILUPYTHON_MINOR_VERSION >= 3)
#define PYINSTANCE_NEW(type,arg)        PyInstance_New((type),(arg),ILU_NIL)
#else
#define PYINSTANCE_NEW(type,arg)        PyInstance_New((type),(arg))
#endif  /* (ILUPYTHON_MAJOR_VERSION == 1 && ILUPYTHON_MINOR_VERSION == 3) */

#if (ILUPYTHON_MAJOR_VERSION == 1 && ILUPYTHON_MINOR_VERSION >= 2)
#define PYTHON_LONG_FROM_STRING(str,base)   PyLong_FromString (str, ILU_NIL, base)
#else
#define PYTHON_LONG_FROM_STRING(str,base)   PyLong_FromString (str, base)
#endif /* (ILUPYTHON_MAJOR_VERSION == 1 && ILUPYTHON_MINOR_VERSION == 2) */

#if (ILUPYTHON_MAJOR_VERSION == 1 && ILUPYTHON_MINOR_VERSION < 4)
#ifndef WIN32
  extern PyFile_WriteString (char *, PyObject *);
extern PyFile_WriteObject (PyObject *, PyObject *, int);
#else
#ifdef __cplusplus
extern "C" {
#endif
  DL_IMPORT(void) PyFile_WriteString (char *, PyObject *);
  DL_IMPORT(int) PyFile_WriteObject (PyObject *, PyObject *, int);
#ifdef __cplusplus
}
#endif
#endif
#endif /* (ILUPYTHON_MAJOR_VERSION == 1 && ILUPYTHON_MINOR_VERSION < 4) */


/* exceptions defined by this module */
PyObject *       _ilupython_GeneralError;
static PyObject *       ProtocolError;
static PyObject *       UnimplementedMethodError;
static PyObject *       UnknownTypeIDError;

/* strings used in exceptions */
static char strArg1ShouldBeIluCall[] = "arg1 should be ilu_Call";
static char strArg1ShouldBeSerializer[] = "arg1 should be ilu_Serializer";
static char strArg1ShouldBePipeline[] = "arg1 should be ilu_Pipeline";
static char strArg1ShouldBePassport[] = "arg1 should be ilu_Passport";
static char strArg1ShouldBeNamesTuple[] = "arg1 should be tuple of names";
static char strArg2ShouldBeIntOrLongInt[] = "arg2 should be int or long int";
static char strArg2OutOfRange[] = "arg2 is out of range";
static char strListTooLong[] = "list length exceeds limit";
static char strListDoesntMatch[] = "list length doesn't match array bound";
static char strArgs1to4ShouldBeNumbers[] = "args 1-4 should be integer numbers";

/* time interval used in GC cleanups */
static const ilu_FineTime plus1Minute = { 60, 0 };

/* names */
static char nameVarClass[]      = "_IluClass";
static char nameVarInstVars[]       = "_IluInstVars";
static char nameVarInstHandle[]     = "IluInstHandle";
static char nameVarServer[]     = "IluServer";

/* global state */
static PyObject *   defaultTrueServer;
static ilu_refany   gcAlarm = ILU_NIL;
static ilu_boolean  gcAlarmSet = ilu_FALSE;
static PyObject *   gcList = ILU_NIL;
static PyObject *   calloutExceptionHandler = ILU_NIL;
static PyObject *   NewObjects = ILU_NIL;

/* global mapping table from class ID to IluclObject * */
static PyObject *   classMap = ILU_NIL;

/* Global var used by ivobject.c */
ilu_cardinal _ilupython_LangIndex = 0;   /* obtained by initialization call to ilu_RegisterLanguage("Python"); */

/********************************/

static char *
  stringDup(char *src)
{
  char *    result;

  if ((result = PyMem_NEW(char, strlen(src) + 1)) == 0)
    {
      (void) PyErr_NoMemory();
      return 0;
    }
  strcpy(result, src);
  return result;
}

#if (ILUPYTHON_MAJOR_VERSION == 1 && ILUPYTHON_MINOR_VERSION < 5)
  char *
#else
  const char *
#endif /* (ILUPYTHON_MAJOR_VERSION == 1 && ILUPYTHON_MINOR_VERSION < 5) */
  _ilupython_formErrDescription (char buf[ERRFMTSIZE], ilu_Error *err)
{
  int             mc, est;
  char            nbuf[1024], *fmt;
  const char     *errname = ILU_ERR_NAME(*err);
  const char     *errfile = ilu_ErrorFile(err);
  static char    *fmt0 = "%s from line %d of %s";
  static char    *fmt1 = "%s (minor=%s) from line %d of %s";
  static char    *fmt2 = "interrupted (bitset=%s) from line %d of %s";
  static char    *fmt3 = "no_memory (nbytes=%s) from line %d of %s";
  ILU_ERR_SWITCH(*err) {
    ILU_SUCCESS_CASE return "SUCCESS";
    ILU_ERR_CASE(no_memory, x) {
      sprintf(nbuf, "%lu", x->nbytes);
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
    sprintf(nbuf, "0x%x -- %s", mc, ilu_GetMinorDescrFromCodes(ILU_ERR_TYPE(*err), mc));
  est = (strlen(fmt) + strlen(errname) + 20 + strlen(errfile) +
	 ((fmt != fmt0) ? strlen(nbuf) : 0));
  if (est > ERRFMTSIZE) {
#if (ILUPYTHON_MAJOR_VERSION == 1 && ILUPYTHON_MINOR_VERSION < 5)
    return (char *) errname;
#else
    return errname;
#endif /* (ILUPYTHON_MAJOR_VERSION == 1 && ILUPYTHON_MINOR_VERSION < 5) */
  }
  if (fmt == fmt0)
    sprintf(buf, fmt, errname, ilu_ErrorLine(err), errfile);
  else
    sprintf(buf, fmt, errname, nbuf, ilu_ErrorLine(err), errfile);
  return buf;
}

#ifdef ILU_CORBA_PYTHON_MAPPING
static void formSystemException (char *exception_class_name, unsigned long minor, int completed)
{
  char namebuf[1000];
  PyObject *args, *minor_code, *completion_status, *exn, *exception_class, *dict;
  static PyObject *corba_exceptions_module = ILU_NIL;
  char *p;

  strcpy (namebuf, exception_class_name);
  for (p = namebuf;  *p != 0;  p++)
    *p = toupper(*p);
  if (corba_exceptions_module == ILU_NIL) {
    corba_exceptions_module = PyImport_ImportModule("CORBAExceptions");
    if (corba_exceptions_module == NULL) {
      ilu_DebugPrintf("ILU/Python(formSystemException):  Could not import CORBAExceptions module!\n");
      return;
    }
    Py_INCREF(corba_exceptions_module);
  }
  if ((dict = PyModule_GetDict(corba_exceptions_module)) == NULL) {
    ilu_DebugPrintf("ILU/Python(formSystemException):  Could not get the dictionary for the CORBAExceptions module!\n");
    return;
  };
  if ((exception_class = PyDict_GetItemString(dict, namebuf)) == NULL) {
    ilu_DebugPrintf("ILU/Python(formSystemException):  Invalid system exception %s specified.\n", namebuf);
    return;
  };
  args = PyTuple_New(2);
  minor_code = PyInt_FromLong(minor);
  PyTuple_SetItem(args, 0, minor_code);
  completion_status = PyInt_FromLong(completed);
  PyTuple_SetItem(args, 1, completion_status);
  if ((exn = PyInstance_New(exception_class, args, NULL)) == NULL) {
    ilu_DebugPrintf("ILU/Python(formSystemException):  Could not create an instance of CORBAExceptions.%s.\n",
		    namebuf);
    return;
  }
  PyErr_SetObject (exception_class, exn);
}

PyObject *
  ilumod_RaiseSystemException (PyObject *self, PyObject *args)
{
  long minor, completion;
  int n;

  char *name;
  if (!PyArg_ParseTuple(args, "sll", &name, &minor, &completion))
    return 0;
  formSystemException(name, minor, completion);
  return 0;
}

#endif /* def ILU_CORBA_PYTHON_MAPPING */

PyObject *_ilupython_RaiseILUGeneralError (ilu_Error *err)
{
#ifdef ILU_CORBA_PYTHON_MAPPING

  ILU_ERR_SWITCH(*err) {
    ILU_SUCCESS_CASE {
      Py_INCREF(Py_None);
      return Py_None;
    }
    ILU_ERR_CASE(no_memory, x) {
      formSystemException("NO_MEMORY", x->nbytes, CORBA_COMPLETED_MAYBE);
    }
    ILU_ERR_CASE(interrupted, x) {
      formSystemException("INTERRUPTED", x->ilu_interruptSet, CORBA_COMPLETED_MAYBE);
    }
    ILU_ERR_CASE(bad_param, x) formSystemException (ILU_ERR_NAME(*err), x->minor, CORBA_COMPLETED_MAYBE);
    ILU_ERR_CASE(imp_limit, x) formSystemException (ILU_ERR_NAME(*err), x->minor, CORBA_COMPLETED_MAYBE);
    ILU_ERR_CASE(comm_failure, x) formSystemException (ILU_ERR_NAME(*err), x->minor, CORBA_COMPLETED_MAYBE);
    ILU_ERR_CASE(inv_objref, x) formSystemException (ILU_ERR_NAME(*err), x->minor, CORBA_COMPLETED_MAYBE);
    ILU_ERR_CASE(internal, x) formSystemException (ILU_ERR_NAME(*err), x->minor, CORBA_COMPLETED_MAYBE);
    ILU_ERR_CASE(marshal, x) formSystemException (ILU_ERR_NAME(*err), x->minor, CORBA_COMPLETED_MAYBE);
    ILU_ERR_CASE(bad_typecode, x) formSystemException (ILU_ERR_NAME(*err), x->minor, CORBA_COMPLETED_MAYBE);
    ILU_ERR_CASE(bad_operation, x) formSystemException (ILU_ERR_NAME(*err), x->minor, CORBA_COMPLETED_MAYBE);
    ILU_ERR_CASE(no_resources, x) formSystemException (ILU_ERR_NAME(*err), x->minor, CORBA_COMPLETED_MAYBE);
    ILU_ERR_ELSE
      formSystemException (ILU_ERR_NAME(*err), 0, CORBA_COMPLETED_MAYBE);
  } ILU_ERR_ENDSWITCH;

#else

  char errbuf[1000];
  PyErr_SetString(_ilupython_GeneralError, _ilupython_formErrDescription(errbuf, err));

#endif

  ILU_HANDLED(*err);
  return 0;
}

static PyObject *
  ilumod_SetCalloutExceptionHandler (PyObject *self, PyObject *args)
{
  PyObject *tmp = calloutExceptionHandler;
  PyObject *handler = ILU_NIL;

  if (!PyArg_Parse(args, "O", &handler))
    return 0;
  if ((handler != Py_None) && (!PyCallable_Check(handler)))
    {
      PyErr_SetString(PyExc_TypeError, "arg should be either None or a callable function of 4 args");
      return 0;
    }
  if (handler != Py_None)
    Py_INCREF(handler);
  calloutExceptionHandler = (handler != Py_None) ? handler : ILU_NIL;
  if (tmp == ILU_NIL) {
    Py_INCREF(Py_None);
    return Py_None;
  } else {
    return (tmp);
  }
}

void
  _ilupython_handleCalloutException(char *culprit, ilu_Error *err)
{
  OWNED(PyObject *)     except;
  OWNED(PyObject *)     val;
  OWNED(PyObject *)     traceback;

#if (ILUPYTHON_MAJOR_VERSION == 1 && ILUPYTHON_MINOR_VERSION >= 2)

  PyErr_Fetch (&except, &val, &traceback);

#else

  PyErr_GetAndClear(&except, &val);

#endif              /* (ILUPYTHON_MAJOR_VERSION == 1 && ILUPYTHON_MINOR_VERSION >= 2) */

  if (except == PyExc_SystemExit)
    {
      if (val == 0 || val == Py_None)
	Py_Exit(0);
      else if (PyInt_Check(val))
	Py_Exit(PyInt_AsLong(val));
      else {    /* print the object to stderr and exit with 1, as per GvR */
	PyObject_Print(val, stderr, Py_PRINT_RAW);
	Py_Exit(1);
      }
    };

#if (ILUPYTHON_MAJOR_VERSION == 1 && ILUPYTHON_MINOR_VERSION >= 2)

  if (calloutExceptionHandler != ILU_NIL) {
    PyObject *  argsTuple;
    PyObject *  result;

    if ((argsTuple = Py_BuildValue("(zOOO)", culprit, except, val, traceback)) != ILU_NIL) {
      result = PyEval_CallObject(calloutExceptionHandler, argsTuple);
      Py_DECREF(argsTuple);
      Py_DECREF(traceback);
      if (result != 0)
	Py_DECREF(result);
    }
  } else

#endif

    {
      BORROWED(PyObject *)    f;

      if ((f = PySys_GetObject("stderr")) != 0) {
	PyFile_WriteString (culprit, f);
	PyFile_WriteString (" raised exception ", f);
	PyFile_WriteObject (except, f, Py_PRINT_RAW);
	if (val && val != Py_None)
	  {
	    PyFile_WriteString (": ", f);
	    PyFile_WriteObject (val, f, Py_PRINT_RAW);
	  }
	PyFile_WriteString ("\n", f);

#if (ILUPYTHON_MAJOR_VERSION == 1 && ILUPYTHON_MINOR_VERSION >= 2)

	PyTraceBack_Print (traceback, f);
	Py_XDECREF (traceback);

#else

	PyErr_PrintTraceBack(f);

#endif              /* (ILUPYTHON_MAJOR_VERSION == 1 && ILUPYTHON_MINOR_VERSION >= 2) */

      }
    }

  /* release objects */
  Py_XDECREF(except); Py_XDECREF(val);

 endit:
  if (err != ILU_NIL && ILU_ERROK(*err))
    ILU_ERR_CONS0(unknown, err, 0);
  PyErr_Clear();
}

/********************************/

static PyObject *
  ilumod_SetDebugLevel(PyObject *self, PyObject *args)
{
  long  bits;
  char *spec;
  ilu_cardinal old_level;

  if (PyArg_Parse(args, "l", &bits))
    old_level = ilu_SetDebugLevel(bits); /* non-blocking */
  else if (PyErr_Clear(), PyArg_Parse(args, "z", &spec))
    old_level = ilu_SetDebugLevelViaString (spec); /* non-blocking */
  else return 0;
  return PyInt_FromLong(old_level);
}

/********************************/

static ilu_Class
  getKernelClass(PyClassObject *pycl)
{
  OWNED(PyObject *) cl;
  char          msg[256];

  if ((cl = PyObject_GetAttrString((PyObject *) pycl, nameVarClass)) == 0)
    {
      sprintf(msg, "class arg should have a variable named %s",
	      nameVarClass);
      PyErr_SetString(PyExc_TypeError, msg);
      return 0;
    }
  Py_DECREF(cl);
  if (!ilucl_Check(cl))
    {
      sprintf(msg, "class arg's %s variable should be ilu_Class",
	      nameVarClass);
      PyErr_SetString(PyExc_TypeError, msg);
      return 0;
    }
  return ((IluclObject *) cl)->c;
}

static BORROWED(IluclObject *)
     getPythonClass (PyClassObject *pycl)
{
  OWNED(PyObject *) cl;
  char          msg[256];

  if ((cl = PyObject_GetAttrString((PyObject *) pycl, nameVarClass)) == 0)
    {
      sprintf(msg, "class arg should have a variable named %s",
	      nameVarClass);
      PyErr_SetString(PyExc_TypeError, msg);
      return 0;
    }
  Py_DECREF(cl);  /* convert to BORROWED */
  if (!ilucl_Check(cl))
    {
      sprintf(msg, "class arg's %s variable should be ilu_Class",
	      nameVarClass);
      PyErr_SetString(PyExc_TypeError, msg);
      return 0;
    }
  return ((IluclObject *) cl);
}

/********************************/

typedef struct CrNode CrNode;
struct CrNode
{
  PyClassObject *   pycl;
  ilu_Class kclass;
  CrNode *  next;
};

static CrNode * crHead = ILU_NIL;

static int
  addToClassRegistry(PyClassObject *pycl)
{
  CrNode *  n   = PyMem_NEW(CrNode, 1);
  ilu_Class kclass;

  if ((kclass = getKernelClass(pycl)) == 0)
    return -1;
  if (n == 0)
    {
      (void) PyErr_NoMemory();
      return -1;
    }
  Py_INCREF(pycl);
  n->pycl = pycl;
  n->kclass = kclass;
  n->next = crHead;
  crHead = n;
  return 0;
}

#ifdef ILU_GENERATE_SUBTYPES

static BORROWED(PyObject *)
     addGeneratedPythonClass (ilu_Class kclass)
{
  static BORROWED(PyObject *) findInClassRegistry(ilu_Class kclass);

  /* we need to cons up a new PyClassObject, and a new iluclobject */
  IluclObject *clobj;
  PyObject *methodsTuple;
  PyObject *superclassTuple;
  char *clname, *brand, *id, *singleton;
  ilu_boolean collectible, optional;
  ilu_cardinal nsupertypes;
  ilu_Class *supertypes;
  int i;
  PyObject *bases;
  PyObject *dict;
  PyObject *name;
  PyObject *o;
  PyClassObject *pycl;

  /* first, look at the class we are creating */

  ilu_DataOfClass (kclass, &clname, &brand, &id, &singleton,
		   &collectible, ILU_NIL, &nsupertypes,
		   &supertypes, &optional,
#ifdef ILU_HTTPNG_OBJECTS
		   ILU_NIL, ILU_NIL, ILU_NIL, ILU_NIL,
#endif
		   ILU_NIL);

  /* next, assemble the supertypes */

  if ((bases = PyTuple_New(nsupertypes)) == 0)
    return 0;
  for (i = 0;  i < nsupertypes;  i++) {
    o = findInClassRegistry (supertypes[i]);
    if (o == 0) {
      Py_DECREF(bases);
      return 0;
    } else {
      Py_INCREF(o);
      PyTuple_SetItem(bases, i, o);
    }
  }

  /* next, create an instance of the IluclObject for the class,
     and put it into a new dictionary as "_IluClass" */
  if ((methodsTuple = PyTuple_New(0)) == 0)
    return 0;
  if ((superclassTuple = PyTuple_New(0)) == 0)
    return 0;
  for (i = 0;  i < nsupertypes;  i++) {
    pycl = (PyClassObject *) PyTuple_GetItem(bases, i);
    o = PyDict_GetItemString (pycl->cl_dict, "_IluClass");
    if (o == 0) {
      Py_DECREF(bases);
      Py_DECREF(methodsTuple);
      Py_DECREF(superclassTuple);
      return 0;
    };
    Py_INCREF(o);
    PyTuple_SetItem(superclassTuple, i, o);
  }
  o = ilucl_New(clname, brand, id, singleton, collectible,
		optional, ilu_DocStringOfClass(kclass),
		methodsTuple, superclassTuple,
		ilu_FALSE, ilu_FALSE, ILU_NIL);
  if (o == 0) {
    Py_DECREF(bases);
    Py_DECREF(methodsTuple);
    Py_DECREF(superclassTuple);
    return 0;
  }
  dict = PyDict_New();
  if (dict == 0) {
    Py_DECREF(o);
    Py_DECREF(bases);
    Py_DECREF(methodsTuple);
    Py_DECREF(superclassTuple);
    return 0;
  }
  PyDict_SetItemString (dict, "_IluClass", o);

  /* finally, create the name for the class */
  name = PyString_FromString (clname);
  if (name == 0) {
    Py_DECREF(dict);
    Py_DECREF(bases);
    Py_DECREF(methodsTuple);
    Py_DECREF(superclassTuple);
    return 0;
  }

  /* now that we have the components, make the PyClassObject */
  pycl = (PyClassObject *) PyClass_New(bases, dict, name);
  if ((pycl == 0) || (addToClassRegistry(pycl) < 0)) {
    Py_DECREF(name);
    Py_DECREF(dict);
    Py_DECREF(bases);
    Py_DECREF(methodsTuple);
    Py_DECREF(superclassTuple);
    return 0;
  }
  return (PyObject *) pycl;  
}

#endif /* def ILU_GENERATE_SUBTYPES */

static BORROWED(PyObject *)
     findInClassRegistry(ilu_Class kclass)
{
  CrNode *  n;

  for (n = crHead; n != 0; n = n->next)
    {
      if (n->kclass == kclass)
	return (PyObject *) n->pycl;
    }
#ifdef ILU_GENERATE_SUBTYPES
  if (ilu_PhonyOfClass(kclass)) {
    /* OK, the class is a generated kernel class, so generate and register
       a matching Python class */
    return (addGeneratedPythonClass(kclass));
  }
#endif /* def ILU_GENERATE_SUBTYPES */
  return 0;
}

static ilu_boolean
  replaceInClassRegistry(PyClassObject *pycl)
{
  ilu_Class kclass;
  CrNode *  n;

  if ((kclass = getKernelClass(pycl)) == 0)
    return ilu_FALSE;
  for (n = crHead; n != 0; n = n->next)
    {
      if (n->kclass == kclass) {
	Py_INCREF(pycl);
	Py_DECREF(n->pycl);
	n->pycl = pycl;
	return ilu_TRUE;
      };
    }
  return ((addToClassRegistry(pycl) >= 0) ? ilu_TRUE : ilu_FALSE);
}

/********************************/

static ilu_boolean CheckNames (PyObject *names)
{
  int len;

  if ((!PyTuple_Check(names)) ||
      (PyTuple_Size(names) < 2))
    return ilu_FALSE;
  for (len = 0;  len < PyTuple_Size(names);  len++)
    if (!PyString_Check(PyTuple_GetItem(names, len)))
      return ilu_FALSE;
  return ilu_TRUE;
}

/* L1 >= otmu */
char NamesBuf[2000];

/* L1 >= otmu */
static void FormNames (PyObject *names, ilu_string *type_name, ilu_string *ifc_name)
{
  char *q;
  ilu_cardinal length, i, buflen, used;
  ilu_string next;
  static ilu_string IDLModuleSeparator = "-iluIDLNamingScope-";
  ilu_Error lerr;

  *ifc_name = PyString_AS_STRING((PyStringObject *) PyTuple_GetItem(names, 0));
  for (i = 1, buflen = sizeof(NamesBuf), used = 0, q = NamesBuf, length = PyTuple_Size(names);  i < length;  i++) {
    next = PyString_AS_STRING((PyStringObject *) PyTuple_GetItem(names, i));
    while ((buflen - used) < (strlen(next) + ((i > 1) ? strlen(IDLModuleSeparator) : 0))) {
      q = (char*) ilu_ReallocE(q, buflen = buflen * 2, &lerr);
      if (ILU_ERRNOK(lerr)) {
	*type_name = ILU_NIL;
	*ifc_name = ILU_NIL;
	return;
      };
    };
    if (i > 1) {
      strcpy (q + used, IDLModuleSeparator);
      used += strlen(IDLModuleSeparator);
    };
    strcpy (q + used, next);
    used += strlen(next);
  }
  *type_name = q;
}

static void ReleaseNames (ilu_string type_name, ilu_string ifc_name)
{
  if (type_name != NamesBuf)
    ilu_free(type_name);
}

static PyObject *
  ilumod_FormClassRecord(PyObject *self, PyObject *args)
{
  char *    name;
  char *    brand;
  char *    uniqueId;
  char *    singleton;
  char      collectible;
  char      optional;
  char *    doc_string;
  PyObject *    methodTuple;
  PyObject *    superclassTuple;
  PyObject *    result;
#ifdef ILU_HTTPNG_OBJECTS
  PyObject *    stateTuple;
  char      local;
  char      sealed;
#endif

#ifdef ILU_HTTPNG_OBJECTS
  if (!PyArg_Parse(args, "(szszbbzOObbO)", &name, &brand, &uniqueId,
		   &singleton, &collectible, &optional, &doc_string,
		   &methodTuple, &superclassTuple, &local, &sealed,
		   &stateTuple))
    return 0;
#else
  if (!PyArg_Parse(args, "(szszbbzOO)", &name, &brand, &uniqueId,
		   &singleton, &collectible, &optional, &doc_string,
		   &methodTuple, &superclassTuple))
    return 0;
#endif /* def ILU_HTTPNG_OBJECTS */
  if (!PyTuple_Check(methodTuple))
    {
      PyErr_SetString(PyExc_TypeError, "arg7 should be tuple");
      return 0;
    }
  if (!PyTuple_Check(superclassTuple))
    {
      PyErr_SetString(PyExc_TypeError, "arg8 should be tuple");
      return 0;
    }
#ifdef ILU_HTTPNG_OBJECTS
  result = ilucl_New(name, brand, uniqueId, singleton, collectible,
		     optional, doc_string, methodTuple, superclassTuple,
		     local, sealed, stateTuple);
#else
  result = ilucl_New(name, brand, uniqueId, singleton, collectible,
		     optional, doc_string, methodTuple, superclassTuple,
		     0, 0, Py_None);
#endif
  return result;
}

/* Forward declarations. */
static int      ensureGcClient(void);

static PyObject *
  ilumod_RegisterClass(PyObject *self, PyObject *args)
{
  PyClassObject *   pycl;
  IluclObject *     icl;
  ilu_Class     kclass;
  ilu_Error     err;
  ilu_boolean       is_new;
  ilu_string        name;
  ilu_string        interface_name;
  ilu_string        interface_brand = ILU_NIL;
  ilu_string        doc_string = ILU_NIL;
  PyObject *        names;

  if (!PyArg_Parse(args, "(OOzz)", &pycl, &names, &interface_brand, &doc_string))
    return 0;
  if (!CheckNames(names)) {
    PyErr_SetString(PyExc_TypeError, "arg 2 should be names tuple");
    return 0;
  };
  if (!PyClass_Check(pycl))
    {
      PyErr_SetString(PyExc_TypeError, "arg1 should be class");
      return 0;
    }
  if ((kclass = getKernelClass(pycl)) == 0)
    return 0;
  if (addToClassRegistry(pycl) < 0)
    return 0;
  icl = getPythonClass(pycl);
  PyDict_SetItemString (classMap, icl->id, (PyObject *) icl);
  if (icl->collectible && ensureGcClient() < 0)
    return 0;

#ifdef ADD_TYPE_REGISTRATION_SUPPORT

  {
    ilu_Type        type;
    ilu_AcquireMutex(ilu_otmu);
    FormNames (names, &name, &interface_name);
    type = ilu_RegisterObjectType (name,
				   interface_name,
				   interface_brand,
				   ilu_IDOfClass(kclass),
				   kclass,
				   &is_new, &err);
    ReleaseNames(name, interface_name);
    ilu_ReleaseMutex(ilu_otmu);
    return (ilutp_FromType(type));
  }

#else

  Py_INCREF(Py_None);
  return (Py_None);

#endif /* ADD_TYPE_REGISTRATION_SUPPORT */

}

static PyObject *
  ilumod_RegisterCustomSurrogate(PyObject *self, PyObject *args)
{
  PyClassObject *   pycl;
  ilu_Class     kclass;
  CrNode *      n;

  if (!PyArg_Parse(args, "O", &pycl))
    return 0;
  if (!PyClass_Check(pycl))
    {
      PyErr_SetString(PyExc_TypeError, "arg1 should be class");
      return 0;
    }
  if (!replaceInClassRegistry(pycl)) {
    return 0;
  }

  Py_INCREF(Py_None);
  return (Py_None);
}

static PyObject *
  ilumod_RegisterSkeletons(PyObject *self, PyObject *args)
{
  IluclObject * cl;
  PyObject *    skelTuple;

  if (!PyArg_Parse(args, "(OO)", &cl, &skelTuple))
    return 0;
  if (!ilucl_Check(cl))
    {
      PyErr_SetString(PyExc_TypeError, "arg1 should be ilu_Class");
      return 0;
    }
  if (!PyTuple_Check(skelTuple))
    {
      PyErr_SetString(PyExc_TypeError, "arg2 should be tuple");
      return 0;
    }
  if (ilucl_RegisterSkeletons(cl, skelTuple) < 0)
    return 0;
  Py_INCREF(Py_None);
  return Py_None;
}

/********************************/

#define NullClObjectPtr         ((IluclObject *) 0)

  static ilu_boolean
  ConcurrentConn(IlucaObject *ca)
{
  if (!iluca_Check(ca))
    {
      ilu_DebugPrintf ("ConcurrentConn:  called with non-IlucaObject * obj\n");
      return ilu_FALSE;
    }
  else
    {
      return ilu_ThreadPerRequest(ilu_ConnectionOfCall(ca->call)); /* both non-blocking */
    }
}

static ilu_boolean
  setRequestHandler(ilu_Connection conn, ilu_TransportInputHandler handler)
{
  ilu_Error lerr;

  CALL_KERNEL(ilupython_threaded_operation,
	      ilu_SetConnectionInputHandler (conn,
					     handler,
					     (ilu_refany) conn,
					     &lerr));
  if (ILU_ERRNOK(lerr))
    {
      char buf[1000];
      _ilupython_formErrDescription (buf, &lerr);
      ilu_DebugPrintf ("ilu: ilu_SetConnectionInputHandler() signals <%s>.\n", buf);
      ILU_HANDLED(lerr);
      return ilu_FALSE;
    }
  return ilu_TRUE;
}

static ilu_boolean
  enableRequestsOnConn(ilu_Connection conn)
{
  ILUPY_ILLEGAL_IN_THREADED(ilu_ThreadPerRequest(conn));
  return setRequestHandler(conn, singleThreadedReadServiceRequest);
}

static ilu_boolean
  disableRequestsOnConn(ilu_Connection conn)
{
  ilu_Error lerr;
  ILUPY_ILLEGAL_IN_THREADED(ilu_ThreadPerRequest(conn));

  CALL_KERNEL(ilupython_threaded_operation,
	      ilu_SetConnectionInputHandler (conn,
					     (ilu_TransportInputHandler) 0,
					     ILU_NIL,
					     &lerr));

  if (ILU_ERRNOK(lerr))
    {
      char buf[1000];
      _ilupython_formErrDescription (buf, &lerr);
      ilu_DebugPrintf ("ilu: ilu_SetConnectionInputHandler() signals <%s>.\n", buf);
      ILU_HANDLED(lerr);
      return ilu_FALSE;
    }
  return ilu_TRUE;
}

static ilu_boolean
  enableRequestsOnCallConn(IlucaObject *ca)
{
  ilu_boolean status;

  if (status = enableRequestsOnConn(ilu_ConnectionOfCall(ca->call)))
    ca->conn_disabled = ilu_FALSE;
  return status;
}

static ilu_boolean
  disableRequestsOnCallConn(IlucaObject *ca)
{
  ilu_Connection conn = ilu_ConnectionOfCall(ca->call);
  ilu_boolean status;

  if (status = disableRequestsOnConn(ilu_ConnectionOfCall(ca->call)))
    ca->conn_disabled = ilu_TRUE;
  return status;
}

static void
  callSkeleton(ilu_Call call, ilu_Class kclass, ilu_Method meth)
{
  IluclObject * cl;
  int       methodIndex;
  PyObject *    skelFunc;
  PyObject *    skelArgs;
  IlucaObject * ca;
  PyObject *    result;
  ilu_Method    methods;
  ilu_cardinal  method_count;
  ilu_string    uid;
  ilu_Error lerr = ILU_INIT_NO_ERR;
  ilu_Connection conn;

  conn = ilu_ConnectionOfCall(call); /* non-blocking */
  if (!ilu_DataOfClass (kclass, ILU_NIL, ILU_NIL, &uid, ILU_NIL,
			ILU_NIL, &method_count, ILU_NIL, ILU_NIL,
			ILU_NIL,
#ifdef ILU_HTTPNG_OBJECTS
			ILU_NIL, ILU_NIL, ILU_NIL, ILU_NIL,
#endif /* def ILU_HTTPNG_OBJECTS */
			&methods)) /* non-blocking */
    {
      ilu_DebugPrintf ("callSkeleton:  invalid kclass, can't get DataOfClass\n");
      ILU_ERR_CONS1(internal, &lerr, minor, ilu_im_check, ILU_NIL);
    }
  else
    {
      cl = (IluclObject *) PyDict_GetItemString (classMap, uid);

      if (!ilucl_Check(cl))
	{
	  ilu_DebugPrintf ("callSkeleton:  invalid kclass, yields bad IluclObject *\n");
	  ILU_ERR_CONS1(internal, &lerr, minor, ilu_im_check, ILU_NIL);
	}
      else
	{
	  methodIndex = meth - methods;
	  if (methodIndex < 0 || method_count <= methodIndex)
	    {
	      ilu_DebugPrintf ("callSkeleton: bad method index (%d)\n",
			       methodIndex);
	      ILU_ERR_CONS1(internal, &lerr, minor, ilu_im_check, ILU_NIL);
	    }
	  else if (cl->skeletons == 0 ||
		   ((skelFunc = cl->skeletons[methodIndex]) == 0))
	    {
	      ilu_DebugPrintf (
			       "callSkeleton: no skeleton for method index (%d)\n",
			       methodIndex);
	      ILU_ERR_CONS1(internal, &lerr, minor, ilu_im_check, ILU_NIL);
	    }
	  else if (!PyCallable_Check(skelFunc))
	    {
	      ilu_DebugPrintf (
			       "callSkeleton: bad skeleton for method index (%d)\n",
			       methodIndex);
	      ILU_ERR_CONS1(internal, &lerr, minor, ilu_im_check, ILU_NIL);
	    }
	  else if ((ca = (IlucaObject *) iluca_FromCall(call)) == 0)
	    {
	      ilu_DebugPrintf ("callSkeleton: couldn't create ilu_Call\n");
	      ILU_ERR_CONS1(internal, &lerr, minor, ilu_im_check, ILU_NIL);
	    }
	  else
	    {
	      if ((skelArgs = Py_BuildValue("(O)", ca)) == 0)
		{
		  ilu_DebugPrintf ("callSkeleton: couldn't build args\n");
#if defined(WIN32)
		  ilu_Error *e;
		  e = &ca->err;
		  ILU_ERR_CONS1(internal, e, minor, ilu_im_check, ILU_NIL);
#else
		  ILU_ERR_CONS1(internal, &ca->err, minor, ilu_im_check, ILU_NIL);
#endif
		}
	      else
		{
#ifdef ILU_PYTHON_THREADS
		  if (ilupython_threaded_operation && (!ilu_InmemConnection(conn)))
		    {
		      PyObject *tid = current_thread_id();

		      if (!tid)
			{
			  /* it's probably somehow possible to figure out how many bytes we need */
#if defined(WIN32)
			  ilu_Error *e;
			  e = &ca->err;
			  ILU_ERR_CONS1(no_memory, e, nbytes, 0, ILU_NIL);
#else
			  ILU_ERR_CONS1(no_memory, &ca->err, nbytes, 0, ILU_NIL);
#endif
			}
		      else
			{
			  PyObject *call_stack;

			  if (!(call_stack = ilupython_thread_push_call(tid, (PyObject *)ca)))
			    {
#if defined(WIN32)
			      ilu_Error *e;
			      e = &ca->err;
			      ILU_ERR_CONS1(no_memory, e, nbytes, 0, ILU_NIL);
#else
			      ILU_ERR_CONS1(no_memory, &ca->err, nbytes, 0, ILU_NIL);
#endif
			    }
			  else
			    {
			      result = PyEval_CallObject(skelFunc, skelArgs);

			      if (result == 0)
				_ilupython_handleCalloutException("true method", &ca->err);
			      else
				Py_DECREF(result);

			      ilupython_thread_pop_call(call_stack);
			    }
			  Py_DECREF(tid);
			}
		    }
		  else
		    {
		      result = PyEval_CallObject(skelFunc, skelArgs);
		      
		      if (result == 0)
			_ilupython_handleCalloutException("true method", &ca->err);
		      else
			Py_DECREF(result);
		    }
#else /* !ILU_PYTHON_THREADS */
		  result = PyEval_CallObject(skelFunc, skelArgs);
		  
		  if (result == 0)
		    _ilupython_handleCalloutException("true method", &ca->err);
		  else
		    Py_DECREF(result);
#endif /* ILU_PYTHON_THREADS */
		  Py_DECREF(skelArgs);
		}
	      CALL_KERNEL(ilupython_threaded_operation, ilu_FinishCall (ca->call, &ca->err));
	      ILU_HANDLED(ca->err);
	      if (!ilupython_threaded_operation && ca->conn_disabled)
		enableRequestsOnConn(conn);
	      Py_DECREF(ca);
	      return;
	    }
	}
    }

  if (!ilupython_threaded_operation)
    enableRequestsOnConn(conn);
  CALL_KERNEL(ilupython_threaded_operation, ilu_FinishCall (call, &lerr));
  ilu_free(call);
  ILU_HANDLED(lerr);
}

/* before: conn registered
 **         (meaning readServiceRequest is registered for conn's fd)
 ** after: conn registered iff conn not closed
 */
static ilu_boolean
  readServiceRequest(ilu_refany rock, ilu_boolean single_threaded)
{
  ilu_Connection        conn    = (ilu_Connection) rock;
  ilu_RcvReqStat        stat;
  ilu_Class         kclass;
  ilu_Method            meth;
  ilu_cardinal          serialNo;
  ilu_Call_s            call;
  ilu_Error         err;
  ilu_boolean           initted;

  if (single_threaded)
    disableRequestsOnConn(conn);

  CALL_KERNEL(ilupython_threaded_operation,
	      stat = ilu_ReceiveRequest(&call, &initted, conn, &kclass, &meth, &serialNo, &err));

  if (ILU_ERRNOK(err))
    {
      char buf[ERRFMTSIZE];
      fprintf (stderr, "Error parsing or serving request on connection %p:  %s",
	       conn, _ilupython_formErrDescription(buf, &err));
      if (stat == ilu_RcvReqStat_request) {
	char *classname;
	ilu_DataOfClass (kclass, &classname, ILU_NIL, ILU_NIL, ILU_NIL, ILU_NIL,
			 ILU_NIL, ILU_NIL, ILU_NIL, ILU_NIL, 
#ifdef ILU_HTTPNG_OBJECTS
			 ILU_NIL, ILU_NIL, ILU_NIL, ILU_NIL,
#endif /* def ILU_HTTPNG_OBJECTS */
			 ILU_NIL);
	fprintf (stderr, ", serial number %ul, method \"%s.%s\"\n",
		 serialNo, classname, meth->me_name);
      }
      else
	fprintf (stderr, " (no reliable info about the request is available)\n");
      ILU_HANDLED(err);
    }

  if (stat == ilu_RcvReqStat_request)
    {
      callSkeleton(&call, kclass, meth);
      return ilu_FALSE; 
    }
  else if (initted)
    {
      CALL_KERNEL(ilupython_threaded_operation, ilu_FinishCall (&call, &err));
      ILU_HANDLED(err);
    }

  if (stat == ilu_RcvReqStat_quit) {
    /* note we don't call ilu_DoneServingConnection if we're threaded since it 
       will be called inside runConnection due ro our returning ilu_TRUE here */
    if (single_threaded && !ilu_ConnectionServingP(conn)) {
      ilu_DoneServingConnection(conn, &err);
      if (ILU_ERRNOK(err)) {
	char buf[ERRFMTSIZE];
	_ilupython_formErrDescription(buf, &err);
	ILU_ERRPRINTF("Warning: Error (%s) from ilu_DoneServingConnection ignored at line %d of %s\n", buf, __LINE__, __FILE__);
	ILU_HANDLED(err);
      }
    }
    return ilu_TRUE;
  }

  /* stat must be ilu_RcvReqStat_noop */
  if (single_threaded) 
    /* call skeleton does this for ilu_RcvReqStat_request case */
    enableRequestsOnConn(conn);
  return ilu_FALSE;
}

/* after: new connection registered */
static void
  readConnectionRequest(ilu_private rock)
{
  ilu_Port      port    = (ilu_Port) rock;
  ilu_Connection    conn;
  ilu_boolean       closed;
  ilu_Error     err;

  CALL_KERNEL(ilupython_threaded_operation,
	      conn = ilu_HandleNewConnection(port, &closed, &err));
  
  if (ILU_ERRNOK(err))
    {
      char buf[2000];

      _ilupython_formErrDescription (buf, &err);
      ilu_DebugPrintf ("ilu_HandleNewConnection failed:  %s\n", buf);
      ILU_HANDLED(err);
      return;
    }

  if (!ilupython_threaded_operation && conn)
    enableRequestsOnConn(conn);
  else if (ilu_InmemPort(port) && conn)
    setRequestHandler(conn, inmemReadServiceRequest);
}

OWNED(PyObject *)
     _ilupython_createSerializer(ilu_Server kserver)
{
  ilu_Serializer    s;
  ilu_Error err = ILU_INIT_NO_ERR;
  PyObject *    serializer;

  CALL_KERNEL(ilupython_threaded_operation, (s = ilu_GetSerializer(kserver, &err)));
  if (ILU_ERRNOK(err))
    {
      char buf[1000];
      _ilupython_formErrDescription (buf, &err);
      ilu_DebugPrintf ("ilu: ilu_GetSerializer(\"%s\") signals <%s>.\n",
		       ilu_IDOfServer(kserver), buf);
      ILU_HANDLED(err);
      return 0;
    }
  serializer = (PyObject *) ilusz_FromSerializer(s);
  return serializer;
}

ilu_Port
  _ilupython_createPort(ilu_Server kserver, ilu_TransportInfo transport, char *protocol,
			ilu_Passport pp, ilu_boolean b_public)
{
  ilu_Port  port;
  ilu_Error err = ILU_INIT_NO_ERR;
  ilu_boolean result;
  ilu_ProtocolInfo real_pinfo;
  ilu_TransportInfo real_tinfo;

  if (transport == ILU_NIL)
    real_tinfo = ilu_DefaultTransportInfo();
  else
    real_tinfo = transport;
  if (protocol == ILU_NIL)
    real_pinfo = ilu_DefaultProtocolInfo();
  else
    real_pinfo = protocol;

  CALL_KERNEL(ilupython_threaded_operation,
	      (port = ilu_FullCreatePort(kserver, real_pinfo, real_tinfo, pp, b_public, &err)));

  if (port == 0)
    {
      char buf[1000];
      _ilupython_formErrDescription (buf, &err);
      ilu_DebugPrintf ("ilu: ilu_FullCreatePort signals <%s>.\n",
		       buf);
      ILU_HANDLED(err);
      return 0;
    }

#ifdef ILU_PYTHON_THREADS
  if (ilupython_threaded_operation && (transport != getInmemTransport()))
    {
      if (!ilupython_fork_thread(readConnectionRequests, (void *)port))
	{
	  /* should I clean up the port somehow? */
	  return 0;
	}
      return port;
    }
#endif /* ILU_PYTHON_THREADS */

  CALL_KERNEL(ilupython_threaded_operation, result = ilu_SetConnectionRequestHandler(port, readConnectionRequest,
										     (ilu_refany) port, &err));
  
  if (!result)
    {
      char buf[1000];
      _ilupython_formErrDescription (buf, &err);
      ilu_DebugPrintf ("ilu: ilu_SetConnectionRequestHandler signals <%s>.\n",
		       buf);
      ILU_HANDLED(err);
      return 0;
    }
  return port;
}

static ilu_TransportInfo
  getInmemTransport(void)
{
  static ilu_TransportInfo tinfo = ILU_NIL;
  /* this should always be called inside the global interpreter lock, so the static variable
     trick, while ugly :-), should be safe -- robh */

  if (tinfo == ILU_NIL)
    tinfo = ilu_LocalTransportInfo();
  return tinfo;
}

static ilu_TransportInfo
  getDefaultTransport(void)
{
  static ilu_TransportInfo tinfo = ILU_NIL;

  /* again, the static trick is okay because this should only be called from within the global interpreter
     lock */

  if (tinfo == ILU_NIL)
    tinfo = ilu_DefaultTransportInfo();
  return tinfo;
}

static char *
  getDefaultProtocol(void)
{
  return ilu_DefaultProtocolInfo();
}

static PyObject *
  createTrueServer(char *serverId, ilu_ObjectTable objTab, ilu_TransportInfo transport,
		   char *protocol, ilu_Passport pp, ilu_boolean createPort, ilu_boolean b_public)
{
  ilu_Server    kserver;
  PyObject *    server;
  ilu_Error lerr;

  CALL_KERNEL(ilupython_threaded_operation,
	      (kserver = ilu_CreateTrueServer(serverId,
					      objTab,
					      _ilupython_LangIndex,
					      &lerr)));
  if (ILU_ERRNOK(lerr))
    return _ilupython_RaiseILUGeneralError(&lerr);

  /* Now Inside(kserver,ilu_rootClass) */
  server = ilusv_FromServer(kserver);
  if (server == 0) {
    ilu_ExitServer(kserver, ilu_rootClass);
    CALL_KERNEL(ilupython_threaded_operation, ilu_BankServer(kserver));
    return 0;
  };
  ilu_SetLSS(kserver, server, _ilupython_LangIndex, &lerr);
  ilu_ExitServer(kserver, ilu_rootClass);
  /* now outside kserver */

  if (ILU_ERRNOK(lerr))
    return _ilupython_RaiseILUGeneralError(&lerr);

  if (_ilupython_createPort(kserver, getInmemTransport(), getDefaultProtocol(), pp, ilu_FALSE) == 0)
    {
      PyErr_SetString(_ilupython_GeneralError, "unable to create local port for new true server");
      return 0;
    }

  if (createPort) {
    if (_ilupython_createPort(kserver, transport, protocol, pp, b_public) == 0)
      {
	PyErr_SetString(_ilupython_GeneralError, "unable to create specified port on new server");
	return 0;
      }
  }
  return server;
}

int
  _ilupython_convTinfo (PyObject *o, char ***tinfo)
{
  char **n = ILU_NIL;
  int count = 0, i;

  if (o == Py_None) {
    *tinfo = ILU_NIL;
    return 1;
  } else if (PyTuple_Check(o)) {
    count = PyTuple_Size(o);
    for (i = 0;  i < count;  i++)
      {
	if (!PyString_Check(PyTuple_GET_ITEM(o, i)))
	  goto err;
      }
    if ((n = (char **) ilu_malloc(sizeof(ilu_string) * (count + 1))) == ILU_NIL)
      goto err2;
    memset ((void *) n, 0, sizeof(ilu_string) * (count + 1));
    for (i = 0;  i < count;  i++)
      if ((n[i] = PyString_AsString (PyTuple_GET_ITEM(o, i))) == ILU_NIL)
	goto err2;
    *tinfo = n;
    return 1;
  } else if (PyList_Check(o)) {
    count = PyList_Size(o);
    for (i = 0;  i < count;  i++)
      {
	if (!PyString_Check(PyList_GET_ITEM((PyListObject *) o, i)))
	  goto err;
      }
    if ((n = (char **) ilu_malloc(sizeof(ilu_string) * (count + 1))) == ILU_NIL)
      goto err2;
    memset ((void *) n, 0, sizeof(ilu_string) * (count + 1));
    for (i = 0;  i < count;  i++)
      if ((n[i] = PyString_AsString (PyList_GET_ITEM((PyListObject *) o, i))) == ILU_NIL)
	goto err2;
    *tinfo = n;
    return 1;
  }
  
 err:
  PyErr_SetString(PyExc_TypeError,
		  "arg2 should be sequence of transport-info strings");
  goto errexit;

 err2:
  PyErr_NoMemory();
  goto errexit;

 errexit:
  if (n != ILU_NIL)
    {
      for (i = 0;  i < count;  i++)
	{
	  if (n[i] != ILU_NIL)
	    ilu_free(n[i]);
	}
      ilu_free(n);
    }
  return 0;
}

/* Forward declarations. */
static ilu_ObjectTable  createObjectTable(PyObject *objectOfIh);
static void     setObjectTableServer(ilu_ObjectTable self,
				     PyObject *server);
static PyObject *
  ilumod_GetPassport (PyObject *self, PyObject *args);

static PyObject *
  ilumod_CreateServer(PyObject *self, PyObject *args)
{
  char *        serverId;
  ilu_ObjectTable   objTab;
  char **       trans;
  char *        proto;
  PyObject *    objectOfIh;
  PyObject *    result;
  ilu_boolean	trans_precious = ilu_FALSE;
  PyObject *	ppobj;
  ilu_Passport	pp = ILU_NIL;

  if (PyArg_Parse(args, ""))
    {
      serverId = 0;
      trans = 0;
      proto = 0;
      objectOfIh = Py_None;
    }
  else if (PyErr_Clear(), PyArg_Parse(args, "z", &serverId))
    {
      trans = 0;
      proto = 0;
      objectOfIh = Py_None;
    }
  else if (PyErr_Clear(), PyArg_ParseTuple(args, "zO&", &serverId, _ilupython_convTinfo, &trans))
    {
      proto = 0;
      objectOfIh = Py_None;
    }
  else if (PyErr_Clear(), PyArg_ParseTuple(args, "zO&z", &serverId, _ilupython_convTinfo, &trans, &proto))
    {
      objectOfIh = Py_None;
    }
  else
    {
      PyErr_Clear();
      if (!PyArg_ParseTuple(args, "zO&zO", &serverId, _ilupython_convTinfo, &trans, &proto,
			    &objectOfIh))
	return 0;
    }
  if (objectOfIh == Py_None)
    objTab = 0;
  else
    {
      if (!PyCallable_Check(objectOfIh))
	{
	  PyErr_SetString(PyExc_TypeError,
			  "arg4 should be callable");
	  if (trans != ILU_NIL)
	    ilu_free(trans);
	  return 0;
	}
      if ((objTab = createObjectTable(objectOfIh)) == 0)
	return 0;
    }

  if (serverId == 0)
    {
      CALL_KERNEL(ilupython_threaded_operation, serverId = ilu_InventID());
    }
  else if ((serverId = stringDup(serverId)) == 0) {
    if (trans != ILU_NIL)
      ilu_free(trans);
    return 0;
  }
  if (trans == 0) {
    trans = getDefaultTransport();
    trans_precious = ilu_TRUE;
  }
  if (proto == 0)
    proto = getDefaultProtocol();
  ppobj = ilumod_GetPassport(self, Py_None);
  if (ilupp_Check(ppobj))
    pp = ilupp_AsPassport((IluppObject *) ppobj);

  if ((result = createTrueServer(serverId, objTab, trans, proto, pp, ilu_TRUE, ilu_TRUE)) == 0)
    {
      if (objTab)
	(*objTab->ot_free_self)(objTab);
      if (trans && !trans_precious)
	ilu_free(trans);
      return 0;
    }
  if (objTab)
    setObjectTableServer(objTab, result);
  if (defaultTrueServer == 0)
    {
      defaultTrueServer = result;
      Py_INCREF(result);
    }
  if (!trans_precious) ilu_free(trans);
  return result;
}

static PyObject *
  ilumod_Server(PyObject *self, PyObject *args)
{
  char *        serverId;
  ilu_ObjectTable   objTab;
  char **       trans = 0;
  char *        proto = 0;
  PyObject *    objectOfIh;
  PyObject *    result;
  ilu_boolean	trans_precious = ilu_FALSE;
  PyObject *	ppobj;
  ilu_Passport	pp = ILU_NIL;
  PyObject *	p_private = Py_None;
  PyObject *	createPort;
  PyObject *	useAsDefault;
  PyObject *	cinfo;

  if (!PyArg_ParseTuple(args, "zOOO", &serverId, &cinfo, &objectOfIh, &useAsDefault))
    return 0;
  if (PyObject_IsTrue(cinfo)) {
    if (!PyTuple_Check(cinfo)) {
      PyErr_SetString(PyExc_TypeError, "cinfo arg (arg 2) should be tuple or None");
      return 0;
    }
    if (!PyArg_ParseTuple(cinfo, "zO&|O", &proto, _ilupython_convTinfo, &trans, &p_private))
      return 0;
  }
  if (objectOfIh == Py_None)
    objTab = 0;
  else
    {
      if (!PyCallable_Check(objectOfIh))
	{
	  PyErr_SetString(PyExc_TypeError,
			  "arg3 (object table) should be callable");
	  if (trans != ILU_NIL)
	    ilu_free(trans);
	  return 0;
	}
      if ((objTab = createObjectTable(objectOfIh)) == 0)
	return 0;
    }
  if (serverId == 0)
    {
      CALL_KERNEL(ilupython_threaded_operation, serverId = ilu_InventID());
    }
  else if ((serverId = stringDup(serverId)) == 0) {
    if (trans != ILU_NIL)
      ilu_free(trans);
    return 0;
  }
  if (PyObject_IsTrue(cinfo) && (trans == 0)) {
    trans = getDefaultTransport();
    trans_precious = ilu_TRUE;
  }
  if (PyObject_IsTrue(cinfo) && (proto == 0))
    proto = getDefaultProtocol();
  ppobj = ilumod_GetPassport(self, Py_None);
  if (ilupp_Check(ppobj))
    pp = ilupp_AsPassport((IluppObject *) ppobj);

  if ((result = createTrueServer(serverId, objTab, trans, proto, pp,
				 PyObject_IsTrue(cinfo) ? ilu_TRUE : ilu_FALSE,
				 PyObject_IsTrue(p_private) ? ilu_FALSE : ilu_TRUE)) == 0)
    {
      if (objTab)
	(*objTab->ot_free_self)(objTab);
      if (trans && !trans_precious)
	ilu_free(trans);
      return 0;
    }
  if (objTab)
    setObjectTableServer(objTab, result);
  if (PyObject_IsTrue(useAsDefault))
    {
      defaultTrueServer = result;
      Py_INCREF(result);
    }
  if (!trans_precious) ilu_free(trans);
  return result;
}

static PyObject *
  createDefaultTrueServer(void)
{
  char *        serverId    = ilu_InventID();
  ilu_ObjectTable   objTab      = NULL;
  char **       transport   = getDefaultTransport();
  char *        protocol    = getDefaultProtocol();

  /*
    ilu_DebugPrintf ("creating default true server:");
    ilu_DebugPrintf (" id='%s', protocol='%s'\n",
    serverId, protocol);
    */
  return (createTrueServer(serverId, objTab, transport,
			   protocol, ILU_NIL, ilu_TRUE, ilu_TRUE));
}

static BORROWED(PyObject *)
     getDefaultTrueServer(void)
{
  /* should be safe because it takes place entirely within the global interpreter lock */
  if (defaultTrueServer == 0)
    defaultTrueServer = createDefaultTrueServer();
  return defaultTrueServer;
}

static PyObject *
  ilumod_DefaultServer(PyObject *self, PyObject *args)
{
  PyObject *    result;

  if (!PyArg_Parse(args, ""))
    return 0;
  if ((result = getDefaultTrueServer()) == 0)
    return 0;
  Py_INCREF(result);
  return result;
}

/********************************/

static BORROWED(IvObject *)
     getInstVars(PyInstanceObject *inst)
{
  PyObject *    iv;

  if ((iv = PyDict_GetItemString(inst->in_dict, nameVarInstVars)) == 0)
    {
      ilu_Class kclass;

      if ((iv = iv_New()) == 0)
	return 0;
      if ((kclass = getKernelClass(inst->in_class)) == 0)
	return 0;
      ((IvObject *) iv)->kclass = kclass;
      ((IvObject *) iv)->pyobj = (void *) inst;
      if (PyDict_SetItemString(inst->in_dict,
			       nameVarInstVars, iv) < 0)
	{
	  Py_DECREF(iv);
	  return 0;
	}
      Py_DECREF(iv);		/* let dictionary have only reference */
    }
  else if (!iv_Check(iv))
    {
      char  msg[256];

      sprintf(msg, "instance's %s should be instvars",
	      nameVarInstVars);
      PyErr_SetString(PyExc_TypeError, msg);
      return 0;
    }
  return (IvObject *) iv;
}

static char *
  getInstanceId(ilu_Class kclass, PyInstanceObject *inst)
{
  PyObject *ih;

  if (((ih = PyDict_GetItemString(inst->in_dict,
				  nameVarInstHandle)) == 0) || !PyString_Check(ih))
    {
      ih = PyString_FromString(ilu_InventID());
      if (PyDict_SetItemString(inst->in_dict, nameVarInstHandle, ih) < 0)
	{
	  /* failure; delete ih and return */
	  Py_DECREF(ih);
	  return 0;
	}
      /* decref ih because SetItemString incref'ed it */
      Py_DECREF(ih);
    }
  return (PyString_AS_STRING((PyStringObject *) ih));
}

static void
  cleanupUninterestingObjects (ilu_private junk)
{
  if (gcList != ILU_NIL && PyList_Check(gcList))
    {
      /* By deleting the list, we implicitly Py_DECREF() all the ILU objects on
	 the list, and their destructors should run. */
      if (Py_COUNT(gcList) != 1) {
	ilu_DebugPrintf("ILU/Python/cleanupUninterestingObjects:"
			"  gcList has invalid ref count of %d.\n",
			Py_COUNT(gcList));
      };
      Py_DECREF(gcList);
      gcList = ILU_NIL;
    }
  gcAlarmSet = ilu_FALSE;
}

static void
  setupGCCleanupAlarm (ilu_boolean b_calling_from_kernel)
{
  if (gcAlarmSet)
    return;

  if (gcAlarm == ILU_NIL)
    gcAlarm = ilu_CreateAlarm(); /* non-blocking (hopefully)! */
  
  _ilu_Assert(gcAlarm != ILU_NIL, "failed to create GC alarm");

#ifdef ILU_PYTHON_THREADS
  /* if we're running threaded and we're Not calling from the kernel i.e. calling from ilumod_Delete, 
     we know we should be (un)grabbing the interpreter lock via the CALL_KERNEL
     macro.  If we are calling from the kernel, i.e. from trackKernelInterest,
     we don't want to use CALL_KERNEL because it will release the lock, do the call 
     and then grab the lock, preventing anything else from using the interpreter.
     */
  if (ilupython_threaded_operation){
    if (!b_calling_from_kernel) {
      CALL_KERNEL(ilupython_threaded_operation, 
		  ilu_SetAlarm(gcAlarm, ilu_FineTime_Add(ilu_FineTime_Now(), plus1Minute), bootstrapAlarm, (void *)&gc_alarm_rock));
    }
    else 
      ilu_SetAlarm (gcAlarm, ilu_FineTime_Add(ilu_FineTime_Now(), plus1Minute), bootstrapAlarm, (void *)&gc_alarm_rock);
  }
  else
#endif /* ILU_PYTHON_THREADS */
    {
      ilu_SetAlarm (gcAlarm,
		    ilu_FineTime_Add (ilu_FineTime_Now(), plus1Minute),
		    cleanupUninterestingObjects,
		    ILU_NIL);
    }
  gcAlarmSet = ilu_TRUE;
}

static void
  removeObjFromList (PyObject *list, PyObject *inst)
{
  int len = PyList_Size(list);
  int i;

  for (i = 0;  i < len;  i++)
    if (PyList_GetItem(list, i) == inst)
      {
	PyObject *emptyList = PyList_New(0);
	PyList_SetSlice (list, i, i+1, emptyList);
	len = PyList_Size(list);
	i -= 1;
	Py_DECREF(emptyList);
      }
}

static void
  addObjToGCList (PyObject *list, PyObject *inst)
{
  int len = PyList_Size(list);
  int i;

  for (i = 0;  i < len;  i++)
    if (PyList_GetItem(list, i) == inst)
      return;   /* already there */
  PyList_Append (list, inst);
  /*
    printf ("*** Added 0x%x to gc list\n", inst);
    */
}

static ilu_boolean
  trackKernelInterest (ilu_Object kobj, int vi)
{
  /* called when ILU kernel gains or loses interest in a
     collectible object */
  PyInstanceObject *inst = (PyInstanceObject *) ilu_GetLanguageSpecificObject(kobj, _ilupython_LangIndex); /* non-blocking */
  ilu_Class cl = ilu_ClassOfObject(kobj); /* non-blocking */

  if (inst != ILU_NIL && (Py_COUNT(inst) > 0) && ((!ilu_TrueInstanceP(kobj)) || ilu_CollectibleP(cl))) /* non-blocking */
    {
      /*
	fprintf (stderr, "*** ILU kernel is %s in <%s> (0x%x, %s, %d)\n", vi ? "interested" : "not interested",
	ilu_SBHOfObject(kobj), inst, ilu_TrueInstanceP(kobj) ? "T" : "S", Py_COUNT(inst));
	*/
      if (vi)
	{
	  Py_INCREF(inst);
	  if (gcList != ILU_NIL && PyList_Check(gcList))
	    removeObjFromList (gcList, (PyObject *) inst);
	  /*
	    fprintf (stderr, "*** inst 0x%x now has ref count of %d\n", inst, Py_COUNT((PyObject *) inst));
	    */
	}
      else
	{
	  if (Py_COUNT(inst) < 2)
	    {
	      if (gcList == ILU_NIL)
		gcList = PyList_New(1);

	      addObjToGCList (gcList, (PyObject *) inst);
	      setupGCCleanupAlarm(ilu_TRUE);
	    }
	  Py_DECREF(inst);
	  /*
	    fprintf (stderr, "*** inst 0x%x now has ref count of %d\n", inst, Py_COUNT((PyObject *) inst));
	    */
	}
    }
  return ilu_TRUE;
}

static ilu_Object
  createKernelObject(IvObject *iv, PyInstanceObject *inst)
{
  char *        instId;
  ilu_Object        kobj;

  if ((instId = getInstanceId(iv->kclass, inst)) == 0)
    return 0;
  kobj = ilu_FindOrCreateTrueObject(instId, iv->kserver, iv->kclass,
				    (void *) inst); /* non-blocking */
  if (kobj == 0)
    PyErr_SetString(_ilupython_GeneralError, "FindOrCreateTrueObject failed");
#ifdef ILU_OLD_PYTHON_GC_BEHAVIOR
  if (!ilu_CollectibleP(iv->kclass)) /* non-blocking */
    Py_INCREF(inst);    /* hold onto Python true objects */
#endif
  return kobj;
}

static BORROWED(PyObject *)
     getInstanceSpecificServer(PyInstanceObject *inst)
{
  PyObject *    sv;

  if ((sv = PyObject_GetAttrString((PyObject *)inst, nameVarServer)) != 0)
    {
      Py_DECREF(sv);    /* GetAttrString increments refcount */
      if (ilusv_Check(sv))
	return sv;
    }
  else
    PyErr_Clear();
  return 0;
}

static BORROWED(IvObject *)
     getInstVarsWithKobj(PyInstanceObject *inst)
{
  IvObject *    iv;

  if ((iv = getInstVars(inst)) == 0)
    return 0;
  if (iv->kserver == 0)
    {
      PyObject *    sv;

      if ((sv = getInstanceSpecificServer(inst)) == 0 &&
	  (sv = getDefaultTrueServer()) == 0)
	return 0;
      iv->kserver = ((IlusvObject *) sv)->kserver;
    }
  CALL_KERNEL(ilupython_threaded_operation, ilu_EnterServer(iv->kserver, iv->kclass));
  if (iv->kobj == 0)
    {
      if ((iv->kobj = createKernelObject(iv, inst)) == 0)
	{
	  ilu_ExitServer(iv->kserver, iv->kclass);
	  return 0;
	}
    }
  return iv;
}

/********************************/

typedef struct
{
  PyObject *    objectOfIh;
  ilu_Server    kserver;
} OtRock;

static ilu_Object
  getKobjFromOtObject(PyInstanceObject *inst, ilu_Server kserver)
{
  IvObject *    iv;

  if (!PyInstance_Check(inst))
    {
      ilu_DebugPrintf ("object table: instance not returned\n");
      return 0;
    }

  if ((iv = getInstVars(inst)) == 0)
    {
      ilu_DebugPrintf ("object table: returned instance is bad");
      return 0;
    }
  if (iv->kserver == 0)
    iv->kserver = kserver;
  else if (iv->kserver != kserver)
    {
      ilu_DebugPrintf (
		       "object table: returned instance has different server");
      return 0;
    }
  if (iv->kobj == 0)
    iv->kobj = createKernelObject(iv, inst);
  return iv->kobj;
}

static ilu_Object
  callObjectOfIh(ilu_ObjectTable self, ilu_string ih)
{
  OtRock *  otr = (OtRock *) self->ot_rock;
  PyObject *    args;
  PyObject *    inst;
  ilu_Object    kobj;

  if ((args = Py_BuildValue("(s)", ih)) == 0)
    {
      ilu_DebugPrintf ("object table: couldn't build args\n");
      return 0;
    }
  inst = PyEval_CallObject(otr->objectOfIh, args);
  Py_DECREF(args);
  if (inst == 0)
    {
      _ilupython_handleCalloutException("object table: function", ILU_NIL);
      return 0;
    }
  kobj = getKobjFromOtObject((PyInstanceObject *) inst, otr->kserver);
  if (NewObjects == ILU_NIL)
    NewObjects = PyList_New(1);
  PyList_Append (NewObjects, inst);
  Py_DECREF(inst);
  return kobj;
}

static void
  freeObjectTable(ilu_ObjectTable self)
{
  OtRock *  otr = (OtRock *) self->ot_rock;

  Py_DECREF(otr->objectOfIh);
  PyMem_DEL(otr);
  PyMem_DEL(self);
}

static ilu_ObjectTable
  createObjectTable(PyObject *objectOfIh)
{
  ilu_ObjectTable   objTab;
  OtRock *  otr;

  if ((objTab = PyMem_NEW(ilu_ObjectTable_s, 1)) == 0)
    {
      (void) PyErr_NoMemory();
      return 0;
    }
  if ((otr = PyMem_NEW(OtRock, 1)) == 0)
    {
      PyMem_DEL(objTab);
      (void) PyErr_NoMemory();
      return 0;
    }

  Py_INCREF(objectOfIh);
  otr->objectOfIh = objectOfIh;
  otr->kserver = 0;

  objTab->ot_object_of_ih = callObjectOfIh;
  objTab->ot_free_self = freeObjectTable;
  objTab->ot_rock = (ilu_private) otr;
  return objTab;
}

static void
  setObjectTableServer(ilu_ObjectTable self, PyObject *server)
{
  IlusvObject *sv;
  if (!ilusv_Check(server)) return;
  sv = (IlusvObject *) server;
  ((OtRock *) self->ot_rock)->kserver = sv->kserver;
}

/********************************/

static PyObject *
  createSurrogateInstance(ilu_Object kobj)
{
  ilu_Class     kclass  = ilu_ClassOfObject(kobj);
  PyObject *        pycl;
  PyInstanceObject *    inst;
  IvObject *        iv;

  if ((pycl = findInClassRegistry(kclass)) == 0)
    {
      PyErr_SetString(_ilupython_GeneralError, "type not in registry");
      return 0;
    }
  if ((inst = (PyInstanceObject *) PYINSTANCE_NEW(pycl, ILU_NIL)) == 0)
    return 0;
  if ((iv = getInstVars(inst)) == 0) {
    Py_DECREF(inst);
    return 0;
  };
  iv->kserver = ilu_ServerOfObject(kobj);
  iv->kobj = kobj;
  return (PyObject *) inst;
}

static PyObject *
  getPythonObject(ilu_Object kobj, ilu_Class kclass)
{
  ilu_Error lerr;
  ilu_Server    kserver = ilu_ServerOfObject(kobj);
  PyObject *    pyobj;

  if (((pyobj = (PyObject *) ilu_GetLanguageSpecificObject(kobj, _ilupython_LangIndex)) == 0) ||
      (Py_COUNT(pyobj) < 1))
    {
      if ((pyobj = createSurrogateInstance(kobj)) == 0) {
	ilu_ExitServer(kserver, kclass);
	return 0;
      };
      CALL_KERNEL(ilupython_threaded_operation,
		  ilu_RegisterLSO(kobj, kclass, (void *) pyobj, _ilupython_LangIndex, &lerr));
      /* DM 24.4.97: although the Python reference is stored here away,
	 we do not need to increment the reference
	 count, because the object is (USUALLY) automatically
	 unregistered (in "iluiv_dealloc") when it is deleted.
	 FIXME: Unregistering is done, when the instance variables
	 of the object is deleted and *NOT* when the object
	 is deleted.
	 When the application stores a reference
	 to the instance variables and then deletes the object,
	 chaos (memory corruption) may arise, because an already
	 deleted python object may be returned!
	 The instance variables should not have life independant
	 of the object!
	 */
      if (ILU_ERRNOK(lerr)) {
	ilu_ExitServer(kserver, kclass);
	Py_DECREF(pyobj);
	return _ilupython_RaiseILUGeneralError(&lerr);
      }
    }
  else
    Py_INCREF(pyobj);
  ilu_ExitServer(kserver, kclass); /* non-blocking */
  return pyobj;
}

static PyObject *
  getPythonTrueObject(ilu_Object kobj, ilu_Class kclass)
{
  ilu_Server    kserver = ilu_ServerOfObject(kobj);
  PyObject *    pyobj;

  if ((pyobj = (PyObject *) ilu_GetLanguageSpecificObject(kobj, _ilupython_LangIndex)) == 0) /* non-blocking */
    {
      char buf[1000];
      sprintf (buf, "kernel object <%600s/%350s> without true instance", ilu_IDOfServer(kserver), ilu_IhOfObject(kobj));
      PyErr_SetString(_ilupython_GeneralError, buf);
    }
  else
    Py_INCREF(pyobj);
  ilu_ExitServer(kserver, kclass); /* non-blocking */
  return pyobj;
}

static PyObject *
  ilumod_FindObject (PyObject *self, PyObject *args)
{
  ilu_string    sid;
  ilu_string    ih;
  ilu_Object    kobj;
  PyObject *    pyobj;

  PyErr_Clear();
  if (!PyArg_Parse(args, "(ss)", &sid, &ih))
    return 0;
  CALL_KERNEL(ilupython_threaded_operation, kobj = ilu_FindObject(sid, ih));
  if (kobj == ILU_NIL) {
    Py_INCREF(Py_None);
    return Py_None;
  } else {
    if (((pyobj = (PyObject *) ilu_GetLanguageSpecificObject(kobj, _ilupython_LangIndex)) == 0)	/* non-blocking */
	|| (Py_COUNT(pyobj) < 1))
      {
	pyobj = Py_None;
      };
    Py_INCREF(pyobj);
    if ((pyobj != Py_None) && (NewObjects != ILU_NIL)) {
      removeObjFromList (NewObjects, pyobj);
    };
    ilu_ExitServer(ilu_ServerOfObject(kobj), ilu_rootClass); /* non-blocking */
    return pyobj;
  }
}

static PyObject *
  ilumod_FindOrCreateSurrogate (PyObject *self, PyObject *args)
{
  PyObject *    serverobj;
  ilu_string    ih;
  ilu_Object    kobj;
  ilu_Class kclass;
  PyObject *    classobj;
  PyObject *    pyobj;
  ilu_Error kerr = ILU_INIT_NO_ERR;

  PyErr_Clear();
  if (!PyArg_Parse(args, "(OsO)", &serverobj, &ih, &classobj))
    return 0;
  if (!ilusv_Check(serverobj)) {
    PyErr_SetString(PyExc_TypeError, "arg1 should be ilu_Server");
    return 0;
  } else if (!PyClass_Check(classobj)) {
    PyErr_SetString(PyExc_TypeError, "arg3 should be class");
    return 0;
  } else if ((kclass = getKernelClass((PyClassObject *) classobj)) == 0) {
    return 0;
  }
  CALL_KERNEL(ilupython_threaded_operation, kobj = ilu_FindOrCreateSurrogate(((IlusvObject*) serverobj)->kserver,
									     ih, kclass, &kerr));
  if (kobj == ILU_NIL) {
    char errbuf[1000];

    PyErr_SetString(_ilupython_GeneralError, _ilupython_formErrDescription(errbuf, &kerr));
    ILU_HANDLED(kerr);
    return 0;
  } else {
    if (((pyobj = (PyObject *) ilu_GetLanguageSpecificObject(kobj, _ilupython_LangIndex)) == 0) ||
	(Py_COUNT(pyobj) < 1)) {
      if ((pyobj = createSurrogateInstance(kobj)) == 0)
	return 0;
      CALL_KERNEL(ilupython_threaded_operation,
		  ilu_RegisterLSO(kobj, kclass, (void *) pyobj, _ilupython_LangIndex, &kerr));
      /* DM 24.4.97: although the Python reference is stored here away,
	 we do not need to increment the reference
	 count, because the object is (USUALLY) automatically
	 unregistered (in "iluiv_dealloc") when it is deleted.
	 FIXME: Unregistering is done, when the instance variables
	 of the object is deleted and *NOT* when the object
	 is deleted.
	 When the application stores a reference
	 to the instance variables and then deletes the object,
	 chaos (memory corruption) may arise, because an already
	 deleted python object may be returned!
	 The instance variables should not have life independant
	 of the object!
	 */
      if (ILU_ERRNOK(kerr))
	return _ilupython_RaiseILUGeneralError(&kerr);
    } else {
      Py_INCREF(pyobj);
    }
    ilu_ExitServer(((IlusvObject*) serverobj)->kserver, kclass);
    return pyobj;
  }
}

/********************************/

static int
  ensureGcClient(void)
{
  static ilu_Server gcClientKserver = 0;
  static ilu_Object gcClientKobj = 0;
  static IlusvObject *  server;

  if (ilu_IsGcClientSet())
    /* some other runtime must have already set up a callback */
    return 0;

  if (gcClientKserver == 0)
    {
      char **   trans   = getDefaultTransport();
      char *    proto   = getDefaultProtocol();
      char *    sid;

      CALL_KERNEL(ilupython_threaded_operation, sid = ilu_InventID());

      if ((server = (IlusvObject *) createTrueServer(sid, ILU_NIL,
						     trans, proto, ILU_NIL,
						     ilu_TRUE, ilu_TRUE)) == 0)
	return -1;
      gcClientKserver = server->kserver;
    }
  if (gcClientKobj == 0)
    {
      ilu_Class kclass;
      char *        kih;

      EXIT_INTERPRETER(ilupython_threaded_operation);
      kclass = ilu_GetGcCallbackClass();
      kih = ilu_InventID();

      ilu_EnterServer(gcClientKserver, kclass);

      /*
       ** We're cheating here in giving something other than a
       ** Python instance as the languageSpecificObject (4th arg).
       ** But this should be okay as long as this gc object remains
       ** inaccessible (its SBH is not public knowledge).
       */
      gcClientKobj =
	ilu_FindOrCreateTrueObject(kih,
				   gcClientKserver,
				   kclass,
				   (void *) &gcClientKobj);
      if (gcClientKobj != 0)
	ilu_SetGcClient(gcClientKobj);
      ilu_ExitServer(gcClientKserver, kclass);

      ENTER_INTERPRETER(ilupython_threaded_operation);

      if (gcClientKobj == 0)
	{
	  PyErr_SetString(_ilupython_GeneralError,
			  "failed creating gc client object");
	  return -1;
	}
    }
  return 0;
}

/********************************/

static char *form_sbh_extend_string (char *current, int separator, PyStringObject *new_string)
{
  char *p;
  char *newstring = PyString_AS_STRING(new_string);
  int i;

  if (current == ILU_NIL) {
    current = (char*) ilu_malloc(strlen(newstring) + 1);
    strcpy (current, newstring);
  } else {
    i = strlen(current);
    current = (char*) ilu_realloc(current, strlen(newstring) + i + 2);
    current[i] = separator;
    current[i + 1] = 0;
    strcat (current, newstring);
  }
  return current;
}

static PyObject *
  ilumod_FormSBH(PyObject *self, PyObject *args)
{
  PyClassObject *   pycl;
  PyObject *        sbhobject;
  ilu_string        sbh = ILU_NIL, sid = ILU_NIL, ih = ILU_NIL, pinfo = ILU_NIL;
  ilu_Class     kclass;
  PyObject *        pinfo_tuple;
  PyObject *        tinfo_tuple_tuple;
  PyStringObject *  s;
  PyObject *        tinfo_element;
  char *        t;
  ilu_Error     err = ILU_INIT_NO_ERR;
  int           i, j;
  char **       tinfo;

  PyErr_Clear();
  if (!PyArg_Parse(args, "(ssOOO)", &sid, &ih, &pycl,
		   &pinfo_tuple, &tinfo_tuple_tuple))
    return 0;
  for (i = 0;  i < PyTuple_Size(tinfo_tuple_tuple);  i++) {
    if (!PyTuple_Check(PyTuple_GET_ITEM(tinfo_tuple_tuple,i))) {
      PyErr_SetString(PyExc_TypeError, "arg 5 (tinfo) should be tuple of tuples");
      return 0;
    }
  }
  if (!PyClass_Check(pycl))
    {
      PyErr_SetString(PyExc_TypeError, "arg3 (class) should be an ILU class");
      return 0;
    }
  if ((kclass = getKernelClass(pycl)) == 0)
    return 0;
  if (!PyTuple_Check(pinfo_tuple))
    {
      PyErr_SetString(PyExc_TypeError, "arg4 (pinfo) should be a tuple of pinfo parms");
      return 0;
    }
  if (!PyTuple_Check(tinfo_tuple_tuple))
    {
      PyErr_SetString(PyExc_TypeError, "arg5 (tinfo) should be a tuple of tuples");
      return 0;
    }
  tinfo = (char**) ilu_malloc((PyTuple_Size(tinfo_tuple_tuple) + 1) * sizeof(char *));
  for (i = 0;  i < PyTuple_Size(pinfo_tuple);  i++) {
    pinfo = form_sbh_extend_string(pinfo, '_', s = (PyStringObject *)
				   PyObject_Str(PyTuple_GET_ITEM(pinfo_tuple,i)));
    Py_DECREF(s);
  };
  for (i = 0;  i < PyTuple_Size(tinfo_tuple_tuple);  i++) {
    tinfo_element = PyTuple_GET_ITEM(tinfo_tuple_tuple,i);
    for (j = 0, t = ILU_NIL;  j < PyTuple_Size(tinfo_element);  j++) {
      t = form_sbh_extend_string(t, '_',
				 s = (PyStringObject *) PyObject_Str(PyTuple_GET_ITEM(tinfo_element,j)));
      Py_DECREF(s);
    }
    tinfo[i] = t;
  }
  tinfo[i] = ILU_NIL;
  sbh = ilu_FormSBH(sid, ih, ilu_IDOfClass(kclass), pinfo, tinfo, &err);
  if (ILU_ERRNOK(err))
    return _ilupython_RaiseILUGeneralError(&err);
  sbhobject = PyString_FromString(sbh);
  ilu_free(sbh);
  for (i = 0;  tinfo[i] != ILU_NIL;  i++)
    ilu_free(tinfo[i]);
  ilu_free(tinfo);
  ilu_free(pinfo);
  return (sbhobject);
}

/********************************/

static PyObject *
  ilumod_ObjectOfSBH(PyObject *self, PyObject *args)
{
  PyClassObject *   pycl;
  PyObject *        pyobj;
  char *        sbh;
  ilu_Class     kclass;
  ilu_Object        kobj;
  ilu_Error     err = ILU_INIT_NO_ERR;
  ilu_ConsiderSbhResult result;
  int len, i;

  PyErr_Clear();
  if (!PyArg_Parse(args, "(Os)", &pycl, &sbh))
    return 0;
  if (!PyClass_Check(pycl))
    {
      PyErr_SetString(PyExc_TypeError, "arg1 should be class");
      return 0;
    }
  if ((kclass = getKernelClass(pycl)) == 0)
    return 0;
  CALL_KERNEL(ilupython_threaded_operation, result = ilu_ConsiderSBH(sbh, &err));
  if (result == ilucsr_err)
    return _ilupython_RaiseILUGeneralError(&err);

  CALL_KERNEL(ilupython_threaded_operation, kobj = ilu_ObjectOfSBH(sbh, kclass, &err));

  if (kobj == 0)
    return _ilupython_RaiseILUGeneralError(&err);
  pyobj = getPythonObject(kobj, kclass);
  if (NewObjects != ILU_NIL) {
    removeObjFromList (NewObjects, pyobj);
  };
  return pyobj;
}

static PyObject *
  ilumod_SBHOfObject(PyObject *self, PyObject *args)
{
  PyInstanceObject *    inst;
  char *        sbh;
  IvObject *        iv;

  if (!PyArg_Parse(args, "O", &inst))
    return 0;
  if (!PyInstance_Check(inst))
    {
      PyErr_SetString(PyExc_TypeError, "arg1 should be instance");
      return 0;
    }
  if ((iv = getInstVarsWithKobj(inst)) == 0)
    return 0;
  sbh = ilu_SBHOfObject(iv->kobj); /* non-blocking */
  ilu_ExitServer(iv->kserver, iv->kclass);
  if (sbh == 0)
    {
      PyErr_SetString(_ilupython_GeneralError, "ilu_SBHOfObject failed");
      return 0;
    }
  return PyString_FromString(sbh);
}

#ifdef IIOP_PROTOCOL

static PyObject *
  ilumod_IOROfObject(PyObject *self, PyObject *args)
{
  PyInstanceObject *    inst;
  char *        ior;
  IvObject *        iv;
  ilu_Error     err;
  PyObject *        ret;

  if (!PyArg_Parse(args, "O", &inst))
    return 0;
  if (!PyInstance_Check(inst))
    {
      PyErr_SetString(PyExc_TypeError, "arg1 should be instance");
      return 0;
    }
  if ((iv = getInstVarsWithKobj(inst)) == 0)
    return 0;
  ior = ilu_IOROfObject(iv->kobj, &err); /* non-blocking */
  if (iv->kobj != ILU_NIL)
    ilu_ExitServer(iv->kserver, iv->kclass); /* non-blocking */
  if (ILU_ERRNOK(err))
    return _ilupython_RaiseILUGeneralError(&err);
  ret = PyString_FromString(ior);
  ilu_free (ior);
  return ret;
}

#else

static PyObject *
  ilumod_IOROfObject(PyObject *self, PyObject *args)
{
  PyErr_SetString(_ilupython_GeneralError, "The CORBA IIOP protocol is not configured in.  No IOR support.");
  return 0;
}

#endif

static PyObject *
  ilumod_Delete (PyObject *self, PyObject *args)
{
  PyInstanceObject *    inst;
  IvObject *    iv;

  if (!PyArg_Parse(args, "O", &inst))
    return 0;
  if (!PyInstance_Check(inst))
    {
      PyErr_SetString(PyExc_TypeError, "arg1 should be instance");
      return 0;
    }
  if ((iv = getInstVars(inst)) == 0)
    return 0;
  if (Py_COUNT(inst) < 2)
    {
      if (gcList == ILU_NIL)
	gcList = PyList_New(1);

      addObjToGCList (gcList, (PyObject *) inst);
      setupGCCleanupAlarm(ilu_FALSE);
    }
#ifdef ILU_OLD_PYTHON_GC_BEHAVIOR
  Py_DECREF(inst);
#endif
  Py_INCREF(Py_None);
  return (Py_None);  
}

/********************************/

static PyObject *
  ilumod_PublishObject (PyObject *self, PyObject *args)
{
  PyInstanceObject *inst;
  IvObject *iv;

  if (!PyArg_Parse(args, "O", &inst))
    return 0;
  if (!PyInstance_Check(inst))
    {
      PyErr_SetString(PyExc_TypeError, "arg1 should be instance");
      return 0;
    }
  if ((iv = getInstVarsWithKobj(inst)) == 0)
    return 0;
  CALL_KERNEL(ilupython_threaded_operation, iv->publish_proof = ilu_PublishObject(iv->kobj));
  if (iv->publish_proof != NULL)
    {
      Py_INCREF(Py_None);
      return Py_None;
    }
  else
    {
      PyErr_SetString(_ilupython_GeneralError, "ilu_PublishObject failed");
      return 0;
    }
}

static PyObject *
  ilumod_WithdrawObject (PyObject *self, PyObject *args)
{
  PyInstanceObject *inst;
  IvObject *iv;
  ilu_boolean result;

  if (!PyArg_Parse(args, "O", &inst))
    return 0;

  if (!PyInstance_Check(inst))
    {
      PyErr_SetString(PyExc_TypeError, "arg1 should be instance");
      return 0;
    }
  if ((iv = getInstVars(inst)) == 0)
    return 0;
  if (iv->kserver == 0 || iv->kclass == 0)
    {
      PyErr_SetString(_ilupython_GeneralError, "null class or kernel server for object");
      return 0;
    }
  if (iv->publish_proof == NULL)
    {
      PyErr_SetString(_ilupython_GeneralError, "not published");
      return 0;
    }
  EXIT_INTERPRETER(ilupython_threaded_operation);
  ilu_EnterServer(iv->kserver, iv->kclass);
  result = ilu_WithdrawObject (iv->kobj, iv->publish_proof);
  ENTER_INTERPRETER(ilupython_threaded_operation);

  if (result)
    {
      free(iv->publish_proof);
      iv->publish_proof = NULL;
      Py_INCREF(Py_None);
      return Py_None;
    }
  else
    {
      PyErr_SetString(_ilupython_GeneralError, "ilu_WithdrawObject failed");
      return 0;
    }
}

static PyObject *
  ilumod_LookupObject (PyObject *self, PyObject *args)
{
  char *sid;
  char *ih;
  PyClassObject *pycl;
  ilu_Class kclass;
  ilu_Object kobj;
  PyObject *pobj = NULL;

  if (!PyArg_Parse(args, "(ssO)", &sid, &ih, &pycl))
    return 0;
  if (!PyClass_Check(pycl))
    {
      PyErr_SetString(PyExc_TypeError, "arg3 should be class");
      return 0;
    }
  if ((kclass = getKernelClass(pycl)) == 0)
    return 0;
  CALL_KERNEL(ilupython_threaded_operation, (void) ilu_ReLookupObject (sid, ih, kclass, &kobj));
  if (kobj == ILU_NIL)
    {
      Py_INCREF(Py_None);
      return Py_None;
    }
  else
    {
      return getPythonObject (kobj, kclass);
    }
}

static PyObject *
  ilumod_ParseSBH(PyObject *self, PyObject *args)
{
  char *    sbh;
  char *    ih      = 0;
  char *    sid     = 0;
  char *    mstid       = 0;
  char *    cinfo       = ILU_NIL;
  ilu_cardinal  cinfolen    = 0;
  ilu_Error err     = ILU_INIT_NO_ERR;
  PyObject *    result;
  PyObject *    ocinfo;
  ilu_boolean   pass_cinfo  = ilu_FALSE;

  if (!PyArg_Parse(args, "s", &sbh))
    return 0;
  if (!ilu_ParseSBH(sbh, &ih, &sid, &mstid, &cinfo, &cinfolen, &pass_cinfo, &err)) /* non-blocking */
    return _ilupython_RaiseILUGeneralError(&err);
  ocinfo = PyString_FromStringAndSize (cinfo, cinfolen);
  result = Py_BuildValue("(sssO)", ih, sid, mstid, ocinfo);
  PyMem_XDEL(ih);
  PyMem_XDEL(sid);
  PyMem_XDEL(mstid);
  if (pass_cinfo)
    ilu_free(cinfo);
  Py_DECREF(ocinfo);
  return result;
}

static PyObject *
  ilumod_PingObject (PyObject *self, PyObject *args)
{
  PyInstanceObject *inst;
  ilu_Error err = ILU_INIT_NO_ERR;
  ilu_boolean ok;
  IvObject *iv;
  ilu_Connection newconn;

  if (!PyArg_Parse(args, "O", &inst))
    return 0;
  if (!PyInstance_Check(inst))
    {
      PyErr_SetString(PyExc_TypeError, "arg1 should be instance");
      return 0;
    }
  if ((iv = getInstVarsWithKobj(inst)) == 0)
    return 0;

  /* now Inside(kobj->server, kobj->class) */

  CALL_KERNEL(ilupython_threaded_operation, err = ilu_DeltaHolds (iv->kobj, 1));

  ilu_ExitServer (iv->kserver, iv->kclass); /* non-blocking */
  ILU_ERR_SWITCH(err) {
    ILU_SUCCESS_CASE;
    ILU_ERR_CASE(GcRegFailed, v) {
      PyErr_SetString (_ilupython_GeneralError, "GC registration failed in ilu_DeltaHolds() call");
      return 0;
    }
    ILU_ERR_ELSE {
      PyErr_SetString (_ilupython_GeneralError, "Unknown error in ilu_DeltaHolds() call");
      return 0;
    }
  } ILU_ERR_ENDSWITCH;

  EXIT_INTERPRETER(ilupython_threaded_operation);
  ok = ilu_PingObject(iv->kobj, &newconn);
  _ilu_Assert(newconn == ILU_NIL || ilupython_threaded_operation, "non-NIL connection returned from ilu_PingObject in non-threaded runtime");

#ifdef ILU_PYTHON_THREADS
  if (newconn != ILU_NIL)
    ilupython_fork_thread(ilupython_watch_outgoing_connection, newconn);
#endif /* ILU_PYTHON_THREADS */

  ilu_EnterServer(iv->kserver, iv->kclass);
  err = ilu_DeltaHolds (iv->kobj, -1);
  ilu_ExitServer (iv->kserver, iv->kclass);
  ENTER_INTERPRETER(ilupython_threaded_operation);

  ILU_ERR_SWITCH(err) {
    ILU_SUCCESS_CASE;
    ILU_ERR_CASE(GcRegFailed, v) {
      PyErr_SetString (_ilupython_GeneralError, "GC registration failed in ilu_DeltaHolds() call");
      return 0;
    }
    ILU_ERR_ELSE {
      PyErr_SetString (_ilupython_GeneralError, "Unknown error in ilu_DeltaHolds() call");
      return 0;
    }
  } ILU_ERR_ENDSWITCH;
  if (ok)
    {
      Py_INCREF(Py_True);
      return Py_True;
    }
  else
    {
      Py_INCREF(Py_False);
      return Py_False;
    }
}

static PyObject *
  ilumod_IsA (PyObject *self, PyObject *args)
{
  PyInstanceObject *inst;
  ilu_Error err = ILU_INIT_NO_ERR;
  ilu_boolean ok;
  IvObject *iv;
  ilu_Class cl, truecl;
  ilu_Connection newconn;
  ilu_string type_id;

  if (!PyArg_ParseTuple(args, "Os", &inst, &type_id))
    return 0;
  if (!PyInstance_Check(inst))
    {
      PyErr_SetString(PyExc_TypeError, "arg1 should be instance");
      return 0;
    }
  if ((cl = ilu_FindClassFromID (type_id)) == ILU_NIL) {
    PyErr_SetString(UnknownTypeIDError, type_id);
    return 0;
  }
  if ((iv = getInstVarsWithKobj(inst)) == 0)
    return 0;

  /* now Inside(kobj->server, kobj->class) */

  truecl = ilu_ClassOfObject(iv->kobj);
  ok = ilu_IsSubObjectType (truecl, cl);

  ilu_ExitServer (iv->kserver, iv->kclass); /* non-blocking */

  if (ok) {
    Py_INCREF(Py_True);
    return Py_True;
  } else {
    Py_INCREF(Py_False);
    return Py_False;
  }
}

/********************************/

static PyObject *
  ilumod_CreateLoopHandle (PyObject *self, PyObject *args)
{
  PyObject *obj;

  if (!PyArg_Parse(args, ""))
    return 0;
  if ((obj = ilulp_Create()) == 0)
    return 0;
  Py_INCREF(obj);
  return obj;
}

struct MainLoopFrame {
  struct MainLoopFrame *next;
  int *loophandle;
};

static struct MainLoopFrame *MainLoops;

static ilu_boolean AddMainLoop (int *loophandle)
{
  struct MainLoopFrame *mlf;
  ilu_Error lerr;

  mlf = (struct MainLoopFrame *) ilu_MallocE(sizeof(*mlf), &lerr);
  if (ILU_ERRNOK(lerr))
    {
      ILU_HANDLED(lerr);
      return ilu_FALSE;
    }
  mlf->loophandle = loophandle;
  mlf->next = MainLoops;
  MainLoops = mlf;
  return ilu_TRUE;
}

static ilu_boolean SignalCheckerSet = ilu_FALSE;

static void SignalCheck (ilu_refany junk)
{
  struct MainLoopFrame *mlf;
  if (PyErr_CheckSignals() != 0) {
    for (mlf = MainLoops;  mlf != ILU_NIL;  mlf = mlf->next) {
      if (mlf->loophandle != ILU_NIL) {
	ilu_ExitMainLoop(mlf->loophandle);
	mlf->loophandle = ILU_NIL;
      }
    }
  }
}

static void PopMainLoops (int *ref)
{
  struct MainLoopFrame *mlf;

  for (mlf = MainLoops;  mlf != ILU_NIL;  mlf = mlf->next) {
    if (mlf->loophandle == ref)
      mlf->loophandle = ILU_NIL;
  }
  while (MainLoops != ILU_NIL && MainLoops->loophandle == ILU_NIL) {
    mlf = MainLoops;
    MainLoops = mlf->next;
    ilu_free(mlf);
  }
}

static PyObject *
  ilumod_RunMainLoop(PyObject *self, PyObject *args)
{
  IlulpObject *indicator;
  ilu_Error lerr;

  if (!PyArg_Parse(args, "O", &indicator))
    return 0;
  if (!ilulp_Check(indicator))
    {
      PyErr_SetString(PyExc_TypeError, "arg1 should be ILU loop handle");
      return 0;
    }
  Py_INCREF(indicator);

#ifdef ILU_PYTHON_THREADS
  if (ilupython_threaded_operation)
    {
      ilu_AcquireMutex(ilupython_MainLoopMutex);
      while (!indicator->val) {
	CALL_KERNEL(ilupython_threaded_operation,
		    ilu_CMWait1(ilupython_MainLoopCondition, ilupython_MainLoopMutex, &lerr));
	if (ILU_ERRNOK(lerr))
	  return _ilupython_RaiseILUGeneralError(&lerr);
      }
      ilu_ReleaseMutex(ilupython_MainLoopMutex);
    }
  else
#endif /* ILU_PYTHON_THREADS */
    {
      if (!AddMainLoop(&indicator->val)) {
	PyErr_SetString(PyExc_TypeError, "can't stack signal handler for loop");
	return 0;
      }
      if (!SignalCheckerSet) {
	ilu_SetSignalCallbackHandler(SignalCheck, ILU_NIL, ((ilu_SignalCallbackHandler *) 0), ILU_NIL, &lerr);
	if (ILU_ERRNOK(lerr)) {
	  PyErr_SetString(PyExc_TypeError, "can't register signal handler");
	  return 0;
	}
	SignalCheckerSet = ilu_TRUE;
      };
      ilu_RunMainLoop(&indicator->val);
      PopMainLoops(&indicator->val);
    }
  Py_DECREF(indicator);
  Py_INCREF(Py_None);
  return Py_None;
}

static PyObject *
  ilumod_ExitMainLoop(PyObject *self, PyObject *args)
{
  IlulpObject *indicator;
  ilu_Error lerr;

  if (!PyArg_Parse(args, "O", &indicator))
    return 0;
  if (!ilulp_Check(indicator))
    {
      PyErr_SetString(PyExc_TypeError, "arg1 should be ILU loop handle");
      return 0;
    }
#ifdef ILU_PYTHON_THREADS
  if (ilupython_threaded_operation) {
    /* use CALL_KERNEL in next line that acquires ilupython_MainLoopMutex to 
       prevent possible circular wait (involving ilupython_MainLoopMutex and python internal 
       interpreter lock) between here and the ilu_CMWait1(ilupython_MainLoopCondition, 
       ilupython_MainLoopMutex,... line in ilumod_RunMainLoop */
    CALL_KERNEL(ilupython_threaded_operation, ilu_AcquireMutex(ilupython_MainLoopMutex));
    indicator->val = 1;
    ilu_ReleaseMutex(ilupython_MainLoopMutex);
    ilu_CondNotify (ilupython_MainLoopCondition, &lerr);
    if (ILU_ERRNOK(lerr)) {
      char buf[1000];
      _ilupython_formErrDescription (buf, &lerr);
      ilu_DebugPrintf ("ilu: Problem notifying main loop condition variable:  %s\n", buf);
      ILU_HANDLED(lerr);
    }
  } else
#endif /* ILU_PYTHON_THREADS */
    ilu_ExitMainLoop(&indicator->val);
  Py_INCREF(Py_None);
  return Py_None;
}

/********************************/

struct DoSoonClosure {
  PyObject *    function;
  PyObject *    argsTuple;
  char *    what;
};

static void
  doSoonCallback(void *ref)
{
  PyObject *result;
  struct DoSoonClosure *cbdata = (struct DoSoonClosure *) ref;

  result = PyEval_CallObject(cbdata->function, cbdata->argsTuple);
  Py_DECREF(cbdata->argsTuple);
  if (result == 0) {
    char    culprit[1000];
    sprintf(culprit, "%s DoSoon callback", cbdata->what);
    _ilupython_handleCalloutException(culprit, ILU_NIL);
  } else {
    Py_DECREF(result);
  }
}

static PyObject *
  ilumod_DoSoon (PyObject *self, PyObject *args)
{
  PyObject *function;
  PyObject *argsTuple;
  ilu_string s;
  struct DoSoonClosure *dsc;
  ilu_Error err;
  ilu_Closure closure;
  ilu_string description;

  PyErr_Clear();
  if (!PyArg_Parse(args, "(OOs)", &function, &argsTuple, &description))
    return 0;
  if (!PyCallable_Check(function))
    {
      PyErr_SetString(PyExc_TypeError, "arg1 should be function or method");
      return 0;
    }
  if (!PyTuple_Check(args))
    {
      PyErr_SetString(PyExc_TypeError, "arg2 should be tuple of arguments to the function");
      return 0;
    }
  dsc = (struct DoSoonClosure *) ilu_MallocE(sizeof(*dsc), &err);
  if (ILU_ERRNOK(err))
    return _ilupython_RaiseILUGeneralError(&err);

  dsc->function = function;
  dsc->argsTuple = argsTuple;
  dsc->what = ilu_StrdupE(description, &err);
  if (ILU_ERRNOK(err)) {
    free (dsc);
    return _ilupython_RaiseILUGeneralError (&err);
  }
  Py_INCREF(argsTuple);
  Py_INCREF(function);

#ifdef ILU_PYTHON_THREADS
  if (ilupython_threaded_operation)
    {
      if (!ilupython_fork_thread(doSoonCallback, (void *) dsc))
	{
	  ilu_free(dsc->what);
	  ilu_free(dsc);
	  Py_DECREF(function);
	  Py_DECREF(argsTuple);
	  return 0;
	}
    }
  else
#endif /* ILU_PYTHON_THREADS */
    {
      closure = (ilu_Closure) ilu_MallocE (sizeof(*closure), &err);
      if (ILU_ERRNOK(err)) {
	ilu_free(dsc->what);
	ilu_free(dsc);
	Py_DECREF(function);
	Py_DECREF(argsTuple);
	return _ilupython_RaiseILUGeneralError(&err);
      };
      closure->proc = doSoonCallback;
      closure->rock = (void *) dsc;
      closure->next = ILU_NIL;
      ilu_DoSoon(closure, &err);
      if (ILU_ERRNOK(err)) {
	ilu_free(dsc->what);
	ilu_free(dsc);
	ilu_free(closure);
	Py_DECREF(function);
	Py_DECREF(argsTuple);
	return _ilupython_RaiseILUGeneralError(&err);
      };
    }
  Py_INCREF(Py_None);
  return Py_None;
}

/* Hmm... should we restrict switching to threaded operation after a new mainloop has been
   set? */
static PyObject *
  callbackWithOneArg(PyObject *callback, OWNED(PyObject *) arg, char *what)
{
  PyObject *    argsTuple;
  PyObject *    result;

  argsTuple = Py_BuildValue("(O)", arg);
  Py_DECREF(arg);   /* BuildValue increfs... */
  if (argsTuple == 0)
    return 0;
  
  result = PyEval_CallObject(callback, argsTuple);
  Py_DECREF(argsTuple);
  if (result == 0)
    {
      char  culprit[100];

      sprintf(culprit, "%s callback", what);
      _ilupython_handleCalloutException(culprit, ILU_NIL);
    }
  return result;
}

static PyObject *
  callbackWithTwoArgs(PyObject *callback, OWNED(PyObject *) arg1, OWNED(PyObject *) arg2, char *what)
{
  PyObject *    argsTuple;
  PyObject *    result;

  argsTuple = Py_BuildValue("(OO)", arg1, arg2);
  Py_DECREF(arg1);
  Py_DECREF(arg2);
  if (argsTuple == 0)
    return 0;
  result = PyEval_CallObject(callback, argsTuple);
  Py_DECREF(argsTuple);
  if (result == 0)
    {
      char  culprit[100];

      sprintf(culprit, "%s callback", what);
      _ilupython_handleCalloutException(culprit, ILU_NIL);
    }
  return result;
}

static ilu_boolean
  registerHandler(PyObject *callback, int fd, ilu_IOHandler handler,
		  ilu_private rock, char *what)
{
  PyObject *    arg;
  PyObject *    result;
  ilu_boolean   stat;

  if ((arg = iohc_New(fd, handler, rock)) == 0 ||
      (result = callbackWithOneArg(callback, arg, what)) == 0)
    return ilu_FALSE;
  stat = BOOLEAN((result != Py_None));
  Py_DECREF(result);
  return stat;
}

static ilu_boolean
  cancelHandler(PyObject *callback, int fd, ilu_IOHandler* p_handler,
		ilu_private* p_rock, char *what)
{
  PyObject *    arg;
  PyObject *    result;
  ilu_boolean   stat;

  *p_handler = NULL;
  *p_rock = NULL;

  if ((arg = PyInt_FromLong(fd)) == 0 ||
      (result = callbackWithOneArg(callback, arg, what)) == 0)
    return ilu_FALSE;
  stat = BOOLEAN((result != Py_None) && (iohc_Check(result)));
  if (stat) {
    IohcObject *p = (IohcObject *) result;
    /* really need to return the handler and rock that was registered */
    *p_handler = p->handler;
    *p_rock = p->rock;
  }
  Py_DECREF(result);
  /* currently have no way to return the handler and rock that was registered */
  return ilu_FALSE; 
  /* return stat; */
}

static PyObject *cbDoEvent = ILU_NIL;
static PyObject *cbRegInp = ILU_NIL;
static PyObject *cbCanInp = ILU_NIL;
static PyObject *cbRegOut = ILU_NIL;
static PyObject *cbCanOut = ILU_NIL;
static PyObject *cbCreateAlarm = ILU_NIL;
static PyObject *cbSetAlarm = ILU_NIL;
static PyObject *cbCanAlarm = ILU_NIL;

static void
  Run(int *stop)
{
  PyObject *    argsTuple;
  PyObject *    result;

  if ((argsTuple = PyTuple_New(0)) == 0)
    {
      /* error */
      return;
    }

  *stop = 0;

  while (!(*stop))
    {
      if (PyOS_InterruptOccurred ())
	{
	  PyErr_SetNone (PyExc_KeyboardInterrupt);
	  *stop = 1;
	}

      result = PyEval_CallObject(cbDoEvent, argsTuple);
      if (result == 0)
	_ilupython_handleCalloutException("run main loop", ILU_NIL);
      else
	Py_DECREF(result);
    }
  Py_DECREF(argsTuple);
  return;
}

static void
  Exit(int *stop)
{
  *stop = 1;
}

struct iohcallback_struct {
  int fd;
  PyObject *callback;
  struct iohcallback_struct *next;
};

struct iohcallback_struct *ihandlers = ILU_NIL;
struct iohcallback_struct *ohandlers = ILU_NIL;

static void
  ActualInputHandler (int fd, ilu_private rock)
{
  PyObject *argsTuple, *result;
  struct iohcallback_struct *ihs = (struct iohcallback_struct *) rock;

  if ((argsTuple = PyTuple_New(0)) == 0)
    {
      /* error */
      return;
    }
  result = PyEval_CallObject(ihs->callback, argsTuple);
  Py_DECREF(argsTuple);
  if (result == 0)
    _ilupython_handleCalloutException("input handler callback", ILU_NIL);
  else
    Py_DECREF(result);
}

static PyObject *
  ilumod_RegisterInputHandler (PyObject *self, PyObject *args)
{
  PyObject *handler;
  PyObject *fileobj;
  int fd;
  struct iohcallback_struct *ihs, *last, *next;

  if (!PyArg_Parse(args, "(OO)", &fileobj, &handler))
    return 0;
  if (PyFile_Check(fileobj))
    {
      FILE *file;
      file = PyFile_AsFile (fileobj);
      fd = fileno(file);    /* fileno is POSIX 8.2.1.1 -- should be declared in stdio.h */
    }
  else if (PyInt_Check(fileobj))
    {
      fd = (int) PyInt_AsLong(fileobj);
    }
  else
    {
      PyErr_SetString(PyExc_TypeError, "arg1 should be open readable file or file descriptor (small int)");
      return 0;
    }
  if ((handler != Py_None) && !PyCallable_Check(handler))
    {
      PyErr_SetString(PyExc_TypeError, "arg2 should be callback function or None");
      return 0;
    }
  for (last = ILU_NIL, ihs = ihandlers;  ihs != ILU_NIL;  ihs = next)
    {
      if (ihs->fd == fd)
	{
	  if (handler == Py_None)   /* doing an UNregister */
	    {
	      if (last == ILU_NIL)  /* take `ihs' off the list */
		ihandlers = ihs->next;
	      else
		last->next = ihs->next;
	      next = ihs->next;     /* advance `next' pointer */
	      /* don't advance last */
	      ilu_UnregisterInputSource (fd);
	      Py_DECREF(ihs->callback); /* this should del the callback */
	      ihs->next = ILU_NIL;  /* just for any conservative GC */
	      ilu_free(ihs);        /* ok, now get rid of this struct */
	    }
	  else
	    {
	      Py_DECREF(ihs->callback); /* remove old callback proc */
	      ihs->callback = handler;  /* add new one */
	      Py_INCREF(ihs->callback); /* and hold onto it */
	      last = ihs;       /* advance past this link */
	      next = ihs->next;
	    }
	}
      else
	{
	  last = ihs;
	  next = ihs->next;
	}
    }
  if (ihs == ILU_NIL && handler != Py_None)
    {
      if ((ihs = (struct iohcallback_struct *) ilu_malloc(sizeof(struct iohcallback_struct))) == ILU_NIL)
	{
	  (void) PyErr_NoMemory();
	  return 0;
	}
      ihs->fd = fd;
      ihs->callback = handler;
      Py_INCREF(ihs->callback);
      ihs->next = ihandlers;
      ihandlers = ihs;
      ilu_RegisterInputSource (fd, ActualInputHandler, ihs);
    }  
  Py_INCREF(Py_None);
  return (Py_None);
}

static void
  ActualOutputHandler (int fd, ilu_private rock)
{
  PyObject *argsTuple, *result;
  struct iohcallback_struct *ohs = (struct iohcallback_struct *) rock;

  if ((argsTuple = PyTuple_New(0)) == 0)
    {
      /* error */
      return;
    }
  result = PyEval_CallObject(ohs->callback, argsTuple);
  Py_DECREF(argsTuple);
  if (result == 0)
    _ilupython_handleCalloutException("output handler callback", ILU_NIL);
  else
    Py_DECREF(result);
}

static PyObject *
  ilumod_RegisterOutputHandler (PyObject *self, PyObject *args)
{
  PyObject *handler;
  PyObject *fileobj;
  int fd;
  struct iohcallback_struct *ohs, *last, *next;

  if (!PyArg_Parse(args, "(OO)", &fileobj, &handler))
    return 0;
  if (PyFile_Check(fileobj))
    {
      FILE *file;
      file = PyFile_AsFile (fileobj);
      fd = fileno(file);    /* fileno is POSIX 8.2.1.1 -- should be declared in stdio.h */
    }
  else if (PyInt_Check(fileobj))
    {
      fd = (int) PyInt_AsLong(fileobj);
    }
  else
    {
      PyErr_SetString(PyExc_TypeError, "arg1 should be open writable file or file descriptor (small int)");
      return 0;
    }
  if ((handler != Py_None) && !PyCallable_Check(handler))
    {
      PyErr_SetString(PyExc_TypeError, "arg2 should be callback function or None");
      return 0;
    }
  for (last = ILU_NIL, ohs = ohandlers;  ohs != ILU_NIL;  ohs = next)
    {
      if (ohs->fd == fd)
	{
	  if (handler == Py_None)   /* doing an UNregister */
	    {
	      if (last == ILU_NIL)  /* take `ohs' off the list */
		ohandlers = ohs->next;
	      else
		last->next = ohs->next;
	      next = ohs->next;     /* advance `next' pointer */
	      /* don't advance last */
	      ilu_UnregisterOutputSource (fd);
	      Py_DECREF(ohs->callback); /* this should del the callback */
	      ohs->next = ILU_NIL;  /* just for any conservative GC */
	      ilu_free(ohs);        /* ok, now get rid of this struct */
	    }
	  else
	    {
	      Py_DECREF(ohs->callback); /* remove old callback proc */
	      ohs->callback = handler;  /* add new one */
	      Py_INCREF(ohs->callback); /* and hold onto it */
	      last = ohs;       /* advance past this link */
	      next = ohs->next;
	    }
	}
      else
	{
	  last = ohs;
	  next = ohs->next;
	}
    }
  if (ohs == ILU_NIL && handler != Py_None)
    {
      if ((ohs = (struct iohcallback_struct *) ilu_malloc(sizeof(struct iohcallback_struct))) == ILU_NIL)
	{
	  (void) PyErr_NoMemory();
	  return 0;
	}
      ohs->fd = fd;
      ohs->callback = handler;
      Py_INCREF(ohs->callback);
      ohs->next = ohandlers;
      ohandlers = ohs;
      ilu_RegisterOutputSource (fd, ActualOutputHandler, ohs);
    }  
  Py_INCREF(Py_None);
  return (Py_None);
}

static ilu_boolean
  registerInputHandler(int fd, ilu_IOHandler handler, ilu_private rock)
{
  return registerHandler(cbRegInp, fd, handler, rock, "reg input");
}

static ilu_boolean
  cancelInputHandler(int fd, ilu_IOHandler* p_handler, ilu_private* p_rock)
{
  return cancelHandler(cbCanInp, fd, p_handler, p_rock, "cancel input");
}

static ilu_boolean
  registerOutputHandler(int fd, ilu_IOHandler handler, ilu_private rock)
{
  return registerHandler(cbRegOut, fd, handler, rock, "reg output");
}

static ilu_boolean
  cancelOutputHandler(int fd, ilu_IOHandler* p_handler, ilu_private* p_rock)
{
  return cancelHandler(cbCanOut, fd, p_handler, p_rock, "cancel output");
}

/* Main Invariant holds; L2 otherwise unconstrained */
typedef void    (*AlarmProc) (ilu_private rock);

static ilu_refany
  CreateAlarm (void)
{
  PyObject *    argsTuple;
  PyObject *    result;

  if ((argsTuple = PyTuple_New(0)) == 0)
    {
      /* error */
      return ILU_NIL;
    }
  result = PyEval_CallObject(cbCreateAlarm, argsTuple);
  Py_DECREF(argsTuple);
  if (result == 0)
    {
      _ilupython_handleCalloutException("create alarm callback", ILU_NIL);
      return 0;
    }
  else
    return ((ilu_refany) result);
}

static void
  SetAlarm(ilu_refany alarm, ilu_FineTime t,
	   AlarmProc proc, ilu_private rock)
{
  PyObject *    arg;
  PyObject *    result;
  PyObject *    a = (PyObject *) alarm;

  if ((arg = thc_New(t, proc, rock)) == 0)
    return;
  Py_INCREF(a);
  result = callbackWithTwoArgs(cbSetAlarm, a, arg, "set alarm");
  if (result == 0)
    return;
  Py_DECREF(result);
}

static void
  UnsetAlarm(ilu_refany alarm)
{
  PyObject *    result;
  PyObject *    a = (PyObject *) alarm;

  Py_INCREF(a);
  if ((result = callbackWithOneArg(cbCanAlarm, a, "cancel alarm")) == 0)
    return;
  Py_DECREF(result);
}

static ilu_MainLoop synth = {
  Run, Exit,
  registerInputHandler, cancelInputHandler,
  registerOutputHandler, cancelOutputHandler,
  CreateAlarm, SetAlarm, UnsetAlarm};

static PyObject *
  ilumod_SetMainLoop(PyObject *self, PyObject *args)
{
  PyObject *    DoEvent;
  PyObject *    regInp;
  PyObject *    canInp;
  PyObject *    regOut;
  PyObject *    canOut;
  PyObject *    createAlarm;
  PyObject *    setAlarm;
  PyObject *    cancelAlarm;
  static ilu_boolean initialized = ilu_FALSE;

  if (!PyArg_Parse(args, "(OOOOOOOO)", &DoEvent, &regInp, &canInp, &regOut, &canOut,
		   &createAlarm, &setAlarm, &cancelAlarm))
    return 0;
  if (!PyCallable_Check(DoEvent) ||
      !PyCallable_Check(regInp) || !PyCallable_Check(canInp) ||
      !PyCallable_Check(regOut) || !PyCallable_Check(canOut) ||
      !PyCallable_Check(createAlarm) || !PyCallable_Check(setAlarm) || !PyCallable_Check(cancelAlarm))
    {
      PyErr_SetString(PyExc_TypeError,
		      "all args should be callable");
      return 0;
    }

  Py_XDECREF(cbDoEvent);
  Py_XDECREF(cbRegInp);
  Py_XDECREF(cbCanInp);
  Py_XDECREF(cbRegOut);
  Py_XDECREF(cbCanOut);
  Py_XDECREF(cbCreateAlarm);
  Py_XDECREF(cbSetAlarm);
  Py_XDECREF(cbCanAlarm);

  cbDoEvent = DoEvent;
  cbRegInp = regInp;
  cbCanInp = canInp;
  cbRegOut = regOut;
  cbCanOut = canOut;
  cbCreateAlarm = createAlarm;
  cbSetAlarm = setAlarm;
  cbCanAlarm = cancelAlarm;

  Py_INCREF(cbDoEvent);
  Py_INCREF(cbRegInp);
  Py_INCREF(cbCanInp);
  Py_INCREF(cbRegOut);
  Py_INCREF(cbCanOut);
  Py_INCREF(cbCreateAlarm);
  Py_INCREF(cbSetAlarm);
  Py_INCREF(cbCanAlarm);

  if (!initialized)
    {
      ilu_SetMainLoop (&synth);
      initialized = ilu_TRUE;
    }
  Py_INCREF(Py_None);
  return Py_None;
}

/********************************/

static PyObject *
  longRealFromVector(
		     int (*size)(PyObject *),
		     PyObject *(*getItem)(PyObject *, int),
		     PyObject *seq
		     )
{
  ilu_longreal  lr;
  int       i;

  if ((*size)(seq) != ilulr_NBytesInLongReal)
    {
      PyErr_SetString(PyExc_TypeError, "arg1 has improper length");
      return 0;
    }
  for (i = 0; i < ilulr_NBytesInLongReal; i++)
    {
      PyObject *    item    = (*getItem)(seq, i);
      long      itemVal;

      if (!PyInt_Check(item))
	{
	  PyErr_SetString(PyExc_TypeError,
			  "item should be integer");
	  return 0;
	}
      itemVal = PyInt_AsLong(item);
      if ((unsigned char) itemVal != itemVal)
	{
	  PyErr_SetString(PyExc_ValueError,
			  "item exceeds range of byte");
	  return 0;
	}
      ((unsigned char *)&lr)[i] = itemVal;
    }
  return ilulr_FromLongReal(lr);
}

static PyObject *
  ilumod_LongReal(PyObject *self, PyObject *args)
{
  PyObject *    v;

  if (!PyArg_Parse(args, "O", &v))
    return 0;
  if (PyFloat_Check(v))
    return ilulr_FromDouble(PyFloat_AsDouble(v));
  else if (PyInt_Check(v))
    return ilulr_FromDouble((double) PyInt_AsLong(v));
  else if (PyList_Check(v))
    return longRealFromVector(PyList_Size, PyList_GetItem, v);
  else if (PyTuple_Check(v))
    return longRealFromVector(PyTuple_Size, PyTuple_GetItem, v);
  PyErr_SetString(PyExc_TypeError,
		  "arg1 should be int, float, or sequence of bytes");
  return 0;
}

#ifdef ILU_FIXED_POINT_SUPPORT

static void
  FixedPointDestructor (void *p)
{
  ilubignum_Value v = (ilubignum_Value) p;
  ilubignum_FreeValue(v);
}

static PyObject *
  ilumod_KernelBignumForValue (PyObject *self, PyObject *args)
{
  PyObject *    v;
  ilubignum_Value bn = ILU_NIL;
  char *err = ILU_NIL;

  if (!PyArg_Parse(args, "O", &v))
    return 0;
  else if (PyInt_Check(v))
    bn = ilubignum_FromInteger (PyInt_AsLong(v), &err);
  else if (PyLong_Check(v)) {
    PyObject *s;
    char *pend = ILU_NIL;
    s = PyObject_Str(v);
    bn = ilubignum_FromString(PyString_AS_STRING((PyStringObject *) s), &pend, 0, &err);
    if ((err == ILU_NIL) && (pend != ILU_NIL) && (*pend != 0) && (*pend != 'L'))
      err = "bad string";
  } else if (PyString_Check(v)) {
    char *pend = ILU_NIL;
    bn = ilubignum_FromString(PyString_AS_STRING((PyStringObject *) v), &pend, 0, &err);
    if ((err == ILU_NIL) && (pend != ILU_NIL) && (*pend != 0) && (*pend != 'L'))
      err = "bad string";
  }

  if ((bn == ILU_NIL) || (err != ILU_NIL)) {
    PyErr_SetString(PyExc_TypeError,
		    "arg1 should be number or string containing formatted number");
    return 0;
  }
  return PyCObject_FromVoidPtr((void *) bn, &FixedPointDestructor);
}

static PyObject *
  ilumod_OutputFixedpoint (PyObject *self, PyObject *args)
{
  IlucaObject *call;
  PyObject *numerator, *min_numerator, *max_numerator, *denominator;
  int corba_fixed_digits, corba_fixed_decimal_places, fixed_range_size_code;

  if (!PyArg_ParseTuple(args, "OOOOOiii", &call, &numerator, &min_numerator, &max_numerator, &denominator,
			&corba_fixed_digits, &corba_fixed_decimal_places, &fixed_range_size_code) ||
      !iluca_Check(call) ||
      !PyCObject_Check(numerator) ||
      !(min_numerator == Py_None || PyCObject_Check(min_numerator)) ||
      !(max_numerator == Py_None || PyCObject_Check(max_numerator)) ||
      !PyCObject_Check(denominator) ||
      (fixed_range_size_code < 0) || (fixed_range_size_code > 7)) {
    PyErr_SetString(PyExc_TypeError,
		    "args:  call, numerator, min_num (or None), max_num (or None), denominator,"
		    " CORBA 'fixed' digits, CORBA 'fixed' decimal places, size code (0-7)");
    return 0;
  }
  CALL_KERNEL(ilupython_threaded_operation, ilu_OutputFixedpoint(call->call,
								 PyCObject_AsVoidPtr(numerator),
								 (min_numerator == Py_None) ? ILU_NIL :
								 PyCObject_AsVoidPtr(min_numerator),
								 (max_numerator == Py_None) ? ILU_NIL :
								 PyCObject_AsVoidPtr(max_numerator),
								 PyCObject_AsVoidPtr(denominator),
								 corba_fixed_digits,
								 corba_fixed_decimal_places,
								 fixed_range_size_code,
								 &call->err));

  if (ILU_ERRNOK(call->err))
    return _ilupython_RaiseILUGeneralError(&call->err);
  Py_INCREF(Py_None);
  return Py_None;
}

static PyObject *
  ilumod_InputFixedpoint (PyObject *self, PyObject *args)
{
  IlucaObject *call;
  ilubignum_Value numerator = ILU_NIL;
  PyObject *min_numerator, *max_numerator, *denominator, *retval;
  int corba_fixed_digits, corba_fixed_decimal_places, fixed_range_size_code, count;
  char *s, *err;

  if (!PyArg_ParseTuple(args, "OOOOiii", &call, &min_numerator,
			&max_numerator, &denominator,
			&corba_fixed_digits, &corba_fixed_decimal_places,
			&fixed_range_size_code) ||
      !iluca_Check(call) ||
      !((min_numerator == Py_None) || PyCObject_Check(min_numerator)) ||
      !((max_numerator == Py_None) || PyCObject_Check(max_numerator)) ||
      !PyCObject_Check(denominator) ||
      (fixed_range_size_code < 0) || (fixed_range_size_code > 7)) {
    PyErr_SetString(PyExc_TypeError,
		    "args:  call, numerator, min_num (or None), max_num (or None), denominator,"
		    " CORBA 'fixed' digits, CORBA 'fixed' decimal places, size code (0-7)");
    return 0;
  }

  CALL_KERNEL(ilupython_threaded_operation,
	      ilu_InputFixedpoint (call->call,
				   &numerator,
				   (min_numerator == Py_None) ? ILU_NIL :
				   PyCObject_AsVoidPtr(min_numerator),
				   (max_numerator == Py_None) ? ILU_NIL :
				   PyCObject_AsVoidPtr(max_numerator),
				   PyCObject_AsVoidPtr(denominator),
				   corba_fixed_digits,
				   corba_fixed_decimal_places,
				   fixed_range_size_code,
				   &call->err));
  if (ILU_ERRNOK(call->err))
    return _ilupython_RaiseILUGeneralError(&call->err);
  s = ilubignum_AsString (numerator, 10, &err);
  if (err != ILU_NIL) {
    char errbuf[1000];
    sprintf(errbuf, "ilubignum_AsString raises <%s>", err);
    PyErr_SetString(_ilupython_GeneralError, errbuf);
    return 0;    
  }
  if ((retval = PyLong_FromString(s, ILU_NIL, 10)) == ILU_NIL)
    return 0;
  ilu_free(s);
  return retval;
}

static PyObject *
  ilumod_SizeOfFixedpoint (PyObject *self, PyObject *args)
{
  IlucaObject *call;
  ilu_cardinal size;
  PyObject *numerator, *min_numerator, *max_numerator, *denominator;
  int corba_fixed_digits, corba_fixed_decimal_places, fixed_range_size_code;

  if (!PyArg_ParseTuple(args, "OOOOOiii", &call, &numerator, &min_numerator, &max_numerator, &denominator,
			&corba_fixed_digits, &corba_fixed_decimal_places, &fixed_range_size_code) ||
      !iluca_Check(call) ||
      !PyCObject_Check(numerator) ||
      !(min_numerator == Py_None || PyCObject_Check(min_numerator)) ||
      !(max_numerator == Py_None || PyCObject_Check(max_numerator)) ||
      !PyCObject_Check(denominator) ||
      (fixed_range_size_code < 0) || (fixed_range_size_code > 7)) {
    PyErr_SetString(PyExc_TypeError,
		    "args:  call, numerator, min_num (or None), max_num (or None), denominator,"
		    " CORBA 'fixed' digits, CORBA 'fixed' decimal places, size code (0-7)");
    return 0;
  }
  CALL_KERNEL(ilupython_threaded_operation,
	      size = ilu_SizeOfFixedpoint(call->call,
					  PyCObject_AsVoidPtr(numerator),
					  (min_numerator == Py_None) ? ILU_NIL :
					  PyCObject_AsVoidPtr(min_numerator),
					  (max_numerator == Py_None) ? ILU_NIL :
					  PyCObject_AsVoidPtr(max_numerator),
					  PyCObject_AsVoidPtr(denominator),
					  corba_fixed_digits,
					  corba_fixed_decimal_places,
					  fixed_range_size_code,
					  &call->err));
  if (ILU_ERRNOK(call->err))
    return _ilupython_RaiseILUGeneralError(&call->err);
  return (PyInt_FromLong(size));
}

#endif /* def ILU_FIXED_POINT_SUPPORT */

/********************************/

static PyObject *
  ilumod_FineTime(PyObject *self, PyObject *args)
{
  PyObject *    value;
  ilu_FineTime  ft;

  if (!PyArg_Parse(args, "O", &value))
    return 0;
  if (PyFloat_Check(value))
    ft = ilu_FineTime_FromDouble(PyFloat_AsDouble(value)); /* non-blocking */
  else if (PyInt_Check(value))
    {
      ft.ft_s = PyInt_AsLong(value);
      ft.ft_t = 0;
    }
  else
    {
      PyErr_SetString(PyExc_TypeError, "arg1 should be int or float");
      return 0;
    }
  return iluft_FromFineTime(ft);
}

static PyObject *
  ilumod_FineTime_Now(PyObject *self, PyObject *args)
{
  ilu_FineTime  ft;

  if (!PyArg_Parse(args, ""))
    return 0;
  ft = ilu_FineTime_Now(); /* non-blocking */
  return iluft_FromFineTime(ft);
}

static PyObject *
  ilumod_CreateAlarm(PyObject *self, PyObject *args)
{
  if (!PyArg_Parse(args, ""))
    return 0;
  return ilual_New();
}

/********************************/

static char *
  protoExceptionImage(ilu_ProtocolException pe)
{
  switch (pe)
    {
    case ilu_ProtocolException_Success:
      return "success";
    case ilu_ProtocolException_NoSuchClassAtServer:
      return "no such class at server";
    case ilu_ProtocolException_ClassVersionMismatch:
      return "class version mismatch";
    case ilu_ProtocolException_NoSuchMethodOnClass:
      return "no such method on class";
    case ilu_ProtocolException_GarbageArguments:
      return "garbage arguments";
    case ilu_ProtocolException_Unknown:
      return "unknown";
    case ilu_ProtocolException_LostConnection:
      return "lost connection";
    case ilu_ProtocolException_RequestRejected:
      return "request rejected";
    case ilu_ProtocolException_RequestTimeout:
      return "request timeout";
    default:
      return "?";
    }
}

static void
  protoErr(ilu_ProtocolException pe)
{
#ifdef ILU_CORBA_PYTHON_MAPPING
  ilu_Error lerr;
  ilu_MapProtocolExceptionToError (pe, &lerr, ILU_NIL);
  _ilupython_RaiseILUGeneralError(&lerr);
#else
  PyErr_SetString(ProtocolError, protoExceptionImage(pe));
#endif
}

/********************************/

static BORROWED(PyObject *)
     CurrentContext (void)
{
#ifdef ILU_PYTHON_THREADS
  if (ilupython_threaded_operation)
    return (ilupython_thread_current_context());
  else
#endif /* ILU_PYTHON_THREADS */
    {
      static PyObject *tup = ILU_NIL;
      if (tup == ILU_NIL) {
	tup = PyList_New(3);
	if (tup == 0)
	  return 0;
	Py_INCREF(Py_None);
	PyList_SetItem(tup, ContextSlots_Serializer, Py_None);
	Py_INCREF(Py_None);
	PyList_SetItem(tup, ContextSlots_Pipeline, Py_None);
	Py_INCREF(Py_None);
	PyList_SetItem(tup, ContextSlots_Passport, Py_None);
      }
      return tup;
    }
}

static PyObject *
  ilumod_BeginCall(PyObject *self, PyObject *args)
{
  PyInstanceObject *    inst;
  ilu_Method        meth;
  IvObject *        iv;
  ilu_Call      call;
  ilu_cardinal      methodIndex = 0;
  IluclObject *     cl;
  ilu_Error     err = ILU_INIT_NO_ERR;
  ilu_Connection    newconn;
  ilu_boolean           result;
  ilu_Serializer    serializer = ILU_NIL;
  ilu_Pipeline      pipeline = ILU_NIL;
  ilu_Passport      passport = ILU_NIL;
  PyObject *        context;
  PyObject *        val;

  if (!PyArg_Parse(args, "(OOi)", &inst, &cl, &methodIndex))
    return 0;
  if (!PyInstance_Check(inst))
    {
      PyErr_SetString(PyExc_TypeError, "arg1 should be instance");
      return 0;
    }
  if ((iv = getInstVars(inst)) == 0)
    return 0;
  if (iv->kserver == 0)
    {
      PyErr_SetString(_ilupython_GeneralError, "object has no server");
      return 0;
    }
  if (!ilucl_Check(cl))
    {
      PyErr_SetString(PyExc_TypeError, "arg2 should be ilu_Class");
      return 0;
    }
  meth = ilu_MethodNOfClass (cl->c, methodIndex - 1); /* non-blocking */
  if (meth == ILU_NIL)
    {
      PyErr_SetString(PyExc_TypeError,
		      "arg3 (method index) out of bounds");
      return 0;
    }
  if ((call = (ilu_Call) ilu_malloc(sizeof(*call))) == ILU_NIL)
    {
      PyErr_NoMemory();
      return 0;
    }
  if ((context = CurrentContext()) == 0)
    return 0;
  else if (!PyList_Check(context))
    {
      PyErr_SetString(_ilupython_GeneralError, "Bad context in BeginCall");
      return 0;
    }
  val = PyList_GetItem(context, ContextSlots_Pipeline);
  if (ilupl_Check(val))
    pipeline = ilupl_AsPipeline((IluplObject *) val);
  val = PyList_GetItem(context, ContextSlots_Serializer);
  if (ilusz_Check(val))
    serializer = ilusz_AsSerializer((IluszObject *) val);
  val = PyList_GetItem(context, ContextSlots_Passport);
  if (ilupp_Check(val))
    passport = ilupp_AsPassport((IluppObject *) val);

  CALL_KERNEL(ilupython_threaded_operation, result = ilu_FullStartCall(call, iv->kserver, cl->c, meth, _ilupython_LangIndex, passport, serializer, pipeline, &newconn, &err));

  if (!result) {
    ilu_free(call);
    return _ilupython_RaiseILUGeneralError(&err);
  }
  _ilu_Assert(newconn == ILU_NIL || ilupython_threaded_operation, "non-NIL connection returned from ilu_StartCall in non-threaded runtime");
#ifdef ILU_PYTHON_THREADS
  if (newconn != ILU_NIL)
    ilupython_fork_thread(ilupython_watch_outgoing_connection, newconn);
#endif /* ILU_PYTHON_THREADS */
  return iluca_FromCall(call);
}

static PyObject *
  ilumod_FinishCall(PyObject *self, PyObject *args)
{
  IlucaObject * ca;

  if (!PyArg_Parse(args, "O", &ca))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  CALL_KERNEL(ilupython_threaded_operation, ilu_FinishCall(ca->call, &ca->err));
  ilu_free(ca->call);
  if (! ILU_ERROK(ca->err))
    return _ilupython_RaiseILUGeneralError(&ca->err);
  Py_INCREF(Py_None);
  return Py_None;
}

static PyObject *
  ilumod_BeginRequest(PyObject *self, PyObject *args)
{
  IlucaObject * ca;
  long      argSize;
  ilu_boolean   result;

  if (!PyArg_Parse(args, "(Ol)", &ca, &argSize))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }

  CALL_KERNEL(ilupython_threaded_operation, result = ilu_StartRequest(ca->call, argSize, &ca->err));

  if (result == ilu_FALSE)
    return _ilupython_RaiseILUGeneralError(&ca->err);
  Py_INCREF(Py_None);
  return Py_None;
}

static PyObject *
  ilumod_FinishRequest(PyObject *self, PyObject *args)
{
  IlucaObject * ca;
  ilu_boolean result;

  if (!PyArg_Parse(args, "O", &ca))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }

  CALL_KERNEL(ilupython_threaded_operation, result = ilu_FinishRequest(ca->call, &ca->err));

  if (result == ilu_FALSE)
    return _ilupython_RaiseILUGeneralError(&ca->err);
  Py_INCREF(Py_None);
  return Py_None;
}

static PyObject *
  ilumod_GetReply(PyObject *self, PyObject *args)
{
  IlucaObject *     ca;
  ilu_ProtocolException pe;
  ilu_cardinal      scode = 0;
  ilu_Connection    newconn;

  if (!PyArg_Parse(args, "O", &ca))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }

  CALL_KERNEL(ilupython_threaded_operation, pe = ilu_GetReply(ca->call, &scode, &newconn, &ca->err));

  if (ILU_ERRNOK(ca->err)) {
    if ((ca->err.ilu_type == ILU_ERRTYP(transient)) &&
	(ILU_ERRSEL(transient,ca->err).minor == ilu_tm_retry)) {
      scode = PYTHON_RETRY_CALL_CODE;
      ILU_HANDLED(ca->err);
      ILU_CLER(ca->err);
      pe = ilu_ProtocolException_Success;
      _ilu_Assert(newconn == ILU_NIL || ilupython_threaded_operation,
		  "non-NIL connection returned from ilu_StartCall in non-threaded runtime");
#ifdef ILU_PYTHON_THREADS
      if (newconn != ILU_NIL)
	ilupython_fork_thread(ilupython_watch_outgoing_connection, newconn);
#endif /* ILU_PYTHON_THREADS */
    } else {
      return _ilupython_RaiseILUGeneralError(&ca->err);
    }
  }
  if (pe != ilu_ProtocolException_Success)
    {
      protoErr(pe);
      return 0;
    }
  return PyInt_FromLong(scode);
}

static PyObject *
  ilumod_ReplyRead(PyObject *self, PyObject *args)
{
  IlucaObject * ca;
  ilu_boolean result;

  if (!PyArg_Parse(args, "O", &ca))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }

  CALL_KERNEL(ilupython_threaded_operation, result = ilu_ReplyRead(ca->call, &ca->err));

  if (result == ilu_FALSE)
    return _ilupython_RaiseILUGeneralError(&ca->err);
  Py_INCREF(Py_None);
  return Py_None;
}

static PyObject *
  ilumod_ExceptionName(PyObject *self, PyObject *args)
{
  IlucaObject * ca;
  IluclObject * cl;
  int       evalue;
  ilu_Method    meth;
  PyObject *    result;
  ilu_cardinal  ecount;
  ilu_Exception*evec;

  if (!PyArg_Parse(args, "(OOi)", &ca, &cl, &evalue))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  if (!ilucl_Check(cl))
    {
      PyErr_SetString(PyExc_TypeError, "arg2 should be ilu_Class");
      return 0;
    }
  meth = ilu_MethodOfCall(ca->call); /* non-blocking */
  ilu_DataOfMethod (meth, ILU_NIL, ILU_NIL, ILU_NIL, ILU_NIL,
		    &ecount, &evec, ILU_NIL); /* non-blocking */
  if (evalue <= 0 || ecount < evalue)
    {
      PyErr_SetString(_ilupython_GeneralError, "unknown exception");
      return 0;
    }
  result = PyString_FromString(evec[evalue - 1]);
  return result;
}

/********************************/

static PyObject *
  ilumod_GetSingleton(PyObject *self, PyObject *args)
{
  IlucaObject * ca;
  ilu_Object    kobj;

  if (!PyArg_Parse(args, "O", &ca))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  CALL_KERNEL(ilupython_threaded_operation, kobj = ilu_GetCallSingleton(ca->call, &ca->err));
  if (ILU_ERRNOK(ca->err))
    return _ilupython_RaiseILUGeneralError(&ca->err);
  else if (kobj == 0)
    {
      PyErr_SetString(_ilupython_GeneralError, "couldn't get singleton");
      return 0;
    }
  return getPythonTrueObject(kobj, ilu_ClassOfObject(kobj));
}

struct passport_list {
  ilu_Passport      passport;
  PyObject *        python_version;
  struct passport_list *next;
};

static struct passport_list *all_passports = ILU_NIL;

/* before: conn not registered */
/* after:  conn not registered iff protocol concurrent */
static PyObject *
  ilumod_RequestRead(PyObject *self, PyObject *args)
{
  IlucaObject *     ca;
  struct passport_list *    ppe;

  if (!PyArg_Parse(args, "O", &ca))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }

  if (!ilupython_threaded_operation)
    {
      /* assumes single threading */
      ppe = (struct passport_list *) ilu_must_malloc(sizeof(struct passport_list));
      ppe->passport = ilu_CallerPassportOfCall(ca->call);
      ppe->python_version = Py_None;
      ppe->next = all_passports;
      all_passports = ppe;
    }
  else
    { /* in a multi-threaded runtime, this is handled in callSkeleton */ }

  CALL_KERNEL(ilupython_threaded_operation, ilu_RequestRead(ca->call, &ca->err));

  if (ILU_ERRNOK(ca->err))
    return _ilupython_RaiseILUGeneralError(&ca->err);

  if (!ilupython_threaded_operation && ConcurrentConn(ca))
    enableRequestsOnCallConn(ca);

  Py_INCREF(Py_None);
  return Py_None;
}

static PyObject *
  ilumod_SetSerializer (PyObject *self, PyObject *args)
{
  PyObject *serializer;
  PyObject *context;
  PyObject *prev;
  ilu_Serializer s;

  if (!PyArg_Parse(args, "O", &serializer))
    return 0;
  if ((serializer != Py_None) && (!ilusz_Check(serializer)))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeSerializer);
      return 0;
    }
  if ((context = CurrentContext()) == 0)
    return 0;
  prev = PyList_GetItem(context, ContextSlots_Serializer);
  if (PyList_SetItem(context, ContextSlots_Serializer, serializer) < 0)
    return 0;
  else
    Py_INCREF(serializer);
  return prev;  
}

static PyObject *
  ilumod_GetSerializer (PyObject *self, PyObject *args)
{
  PyObject *context;
  PyObject *prev;

  if ((context = CurrentContext()) == 0)
    return 0;
  prev = PyList_GetItem(context, ContextSlots_Serializer);
  Py_INCREF(prev);
  return prev;  
}

static PyObject *
  ilumod_SetPipeline (PyObject *self, PyObject *args)
{
  PyObject *pipeline;
  PyObject *context;
  PyObject *prev;
  ilu_Pipeline s;

  if (!PyArg_Parse(args, "O", &pipeline))
    return 0;
  if ((pipeline != Py_None) && (!ilupl_Check(pipeline)))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBePipeline);
      return 0;
    }
  if ((context = CurrentContext()) == 0)
    return 0;
  prev = PyList_GetItem(context, ContextSlots_Pipeline);
  if (PyList_SetItem(context, ContextSlots_Pipeline, pipeline) < 0)
    return 0;
  else
    Py_INCREF(pipeline);
  return prev;  
}

static PyObject *
  ilumod_GetPipeline (PyObject *self, PyObject *args)
{
  PyObject *context;
  PyObject *prev;

  if ((context = CurrentContext()) == 0)
    return 0;
  prev = PyList_GetItem(context, ContextSlots_Pipeline);
  Py_INCREF(prev);
  return prev;  
}

static PyObject *
  ilumod_CreatePipeline(PyObject *self, PyObject *args)
{
  ilu_Pipeline  p;
  ilu_Error err = ILU_INIT_NO_ERR;
  PyObject *    pipeline;

  CALL_KERNEL(ilupython_threaded_operation, (p = ilu_GetPipeline(&err)));
  if (ILU_ERRNOK(err))
    {
      char buf[1000];
      _ilupython_formErrDescription (buf, &err);
      ilu_DebugPrintf ("ilu: ilu_GetPipeline() signals <%s>.\n", buf);
      ILU_HANDLED(err);
      return 0;
    }
  pipeline = (PyObject *) ilupl_FromPipeline(p);
  return pipeline;
}

static PyObject *
  ilumod_SetPassport (PyObject *self, PyObject *args)
{
  PyObject *passport;
  PyObject *context;
  PyObject *prev;
  PyObject *foo;
  int retval;

  if (!PyArg_Parse(args, "O", &passport))
    return 0;
  if ((passport != Py_None) && (!ilupp_Check(passport)))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBePassport);
      return 0;
    }
  if ((context = CurrentContext()) == 0)
    return 0;
  prev = PyList_GetItem(context, ContextSlots_Passport);
  if (PyList_SetItem(context, ContextSlots_Passport, passport) < 0)
    return 0;
  else
    Py_INCREF(passport);
  return prev;  
}

static PyObject *
  ilumod_GetPassport (PyObject *self, PyObject *args)
{
  PyObject *context;
  PyObject *prev;
  ilu_Passport s;

  if ((context = CurrentContext()) == 0)
    return 0;
  prev = PyList_GetItem(context, ContextSlots_Passport);
  Py_INCREF(prev);
  return prev;  
}

static PyObject *
  ilumod_CreatePassport(PyObject *self, PyObject *args)
{
  ilu_Passport  p;
  ilu_Error err = ILU_INIT_NO_ERR;
  PyObject *    passport;

  CALL_KERNEL(ilupython_threaded_operation, (p = ilu_CreatePassport(ILU_NIL, &err)));
  if (ILU_ERRNOK(err))
    {
      char buf[1000];
      _ilupython_formErrDescription (buf, &err);
      ilu_DebugPrintf ("ilu: ilu_CreatePassport() signals <%s>.\n", buf);
      ILU_HANDLED(err);
      return 0;
    }
  passport = (PyObject *) ilupp_FromPassport(p);
  return passport;
}

static PyObject *
  ilumod_CallerIdentity (PyObject *self, PyObject *args)
{
  PyObject *local_passport = Py_None;
  PyObject **py_passport;
  ilu_Passport passport;

#ifdef ILU_PYTHON_THREADS
  if (ilupython_threaded_operation)
    {
      IlucaObject *ca = (IlucaObject *)ilupython_thread_current_call();

      if (ca == 0) {
	/* error, or local call */
	return (ilumod_GetPassport(self, args));
      } else {
	/*py_passport = ((PyObject **)&ca->call->ca_private);*/
	/* since the ilu_Call object is disposed of independently of the
	   iluca object, we can't stash it in the private field */
	py_passport = &local_passport;
	passport = ilu_CallerPassportOfCall(ca->call);
      }
    }
  else
#endif /* ILU_PYTHON_THREADS */
    {
      if (all_passports == ILU_NIL)
	{
	  /* must be local call */
	  return (ilumod_GetPassport(self, args));
	}
      else
	{
	  py_passport = &all_passports->python_version;
	  passport = all_passports->passport;
	}
    }
  
  if (*py_passport == Py_None)
    *py_passport = ilupp_FromPassport(passport);

  if ((py_passport != &local_passport) || (*py_passport == Py_None))
    Py_INCREF(*py_passport);
  return (*py_passport);
}

static void PopPassport (ilu_Call call)
{
  struct passport_list *e = all_passports;
  if (e != NULL)
    {
      all_passports = e->next;
      if (e->python_version != Py_None)
	Py_DECREF(e->python_version);
      e->python_version = 0;
      ilu_free(e);
    }
}

/* before: conn registered iff protocol concurrent */
/* after:  conn registered */
static PyObject *
  ilumod_NoReply(PyObject *self, PyObject *args)
{
  IlucaObject *     ca;

  if (!PyArg_Parse(args, "O", &ca))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  if (!ilupython_threaded_operation)
    PopPassport (ca->call);

  CALL_KERNEL(ilupython_threaded_operation, ilu_NoReply(ca->call, &ca->err));

  if (ILU_ERRNOK(ca->err))
    return _ilupython_RaiseILUGeneralError(&ca->err);
  Py_INCREF(Py_None);
  return Py_None;
}

/* before: conn registered iff protocol concurrent
 ** after:  normal return   -> conn not registered,
 **    _ilupython_GeneralError    -> conn registered, or
 **    other exception -> conn registered iff protocol concurrent
 */
static PyObject *
  ilumod_BeginSizingReply(PyObject *self, PyObject *args)
{
  IlucaObject *     ca;
  int           raises;
  long          argSize;

  if (!PyArg_Parse(args, "(Oi)", &ca, &raises))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  if (!ilupython_threaded_operation && ConcurrentConn(ca))
    disableRequestsOnCallConn(ca);

  CALL_KERNEL(ilupython_threaded_operation, argSize = ilu_BeginSizingReply(ca->call, BOOLEAN(raises), &ca->err)); 

  if (ILU_ERRNOK(ca->err))
    {
      if (!ilupython_threaded_operation)
	enableRequestsOnCallConn(ca);
      return _ilupython_RaiseILUGeneralError(&ca->err);
    }
  return PyInt_FromLong(argSize);
}

/* before: conn not registered
 ** after:  normal return   -> conn not registered,
 **    _ilupython_GeneralError    -> conn registered, or
 **    other exception -> conn registered iff protocol concurrent
 */
static PyObject *
  ilumod_BeginReply(PyObject *self, PyObject *args)
{
  IlucaObject *     ca;
  int           raises;
  long          argSize;
  ilu_boolean           result;

  if (!PyArg_Parse(args, "(Oil)", &ca, &raises, &argSize))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  if (!ilupython_threaded_operation)
    PopPassport (ca->call);
  CALL_KERNEL(ilupython_threaded_operation, result = ilu_BeginReply(ca->call, BOOLEAN(raises), argSize, &ca->err));
  if (result == ilu_FALSE)
    {
      if (!ilupython_threaded_operation)
	enableRequestsOnCallConn(ca);
      return _ilupython_RaiseILUGeneralError(&ca->err);
    }
  Py_INCREF(Py_None);
  return Py_None;
}

/* before: conn not registered
 ** after:  normal return -> conn registered
 **    exception     -> conn not registered
 */
static PyObject *
  ilumod_FinishReply(PyObject *self, PyObject *args)
{
  IlucaObject *     ca;

  if (!PyArg_Parse(args, "O", &ca))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  CALL_KERNEL(ilupython_threaded_operation, ilu_FinishReply(ca->call, &ca->err));
  if (ILU_ERRNOK(ca->err))
    return _ilupython_RaiseILUGeneralError(&ca->err);
  Py_INCREF(Py_None);
  return Py_None;
}

/* before: conn registered iff protocol concurrent
 ** after:  normal return   -> conn not registered
 **    _ilupython_GeneralError    -> conn registered
 **    other exception -> conn registered iff protocol concurrent
 */
static PyObject *
  ilumod_BeginSizingException(PyObject *self, PyObject *args)
{
  IlucaObject *     ca;
  int           ecode;
  long          argSize;

  if (!PyArg_Parse(args, "(Oi)", &ca, &ecode))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  if (!ilupython_threaded_operation && ConcurrentConn(ca))
    disableRequestsOnCallConn(ca);

  CALL_KERNEL(ilupython_threaded_operation, argSize = ilu_BeginSizingException(ca->call, ecode, &ca->err));

  if (ILU_ERRNOK(ca->err))
    {
      if (!ilupython_threaded_operation && !ConcurrentConn(ca))
	enableRequestsOnCallConn(ca);
      return _ilupython_RaiseILUGeneralError(&ca->err);
    }

  return PyInt_FromLong(argSize);
}

/* before: conn not registered
 ** after:  normal return   -> conn not registered
 **    _ilupython_GeneralError    -> conn registered
 **    other exception -> conn registered iff protocol concurrent
 */
static PyObject *
  ilumod_BeginException(PyObject *self, PyObject *args)
{
  IlucaObject *     ca;
  int           ecode;
  long          argSize;
  ilu_boolean           result;

  if (!PyArg_Parse(args, "(Oil)", &ca, &ecode, &argSize))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  if (!ilupython_threaded_operation)
    PopPassport (ca->call);

  CALL_KERNEL(ilupython_threaded_operation, result = ilu_BeginException(ca->call, ecode, argSize, &ca->err));

  if (result == ilu_FALSE)
    {
      if (!ilupython_threaded_operation && !ConcurrentConn(ca))
	enableRequestsOnCallConn(ca);
      return _ilupython_RaiseILUGeneralError(&ca->err);
    }
  Py_INCREF(Py_None);
  return Py_None;
}

/* before: conn not registered
 ** after:  normal return -> conn registered
 **    exception     -> conn not registered
 */
static PyObject *
  ilumod_FinishException(PyObject *self, PyObject *args)
{
  IlucaObject *     ca;

  if (!PyArg_Parse(args, "O", &ca))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  if (!ilu_FinishException(ca->call, &ca->err)) /* non-blocking */
    return _ilupython_RaiseILUGeneralError(&ca->err);
  Py_INCREF(Py_None);
  return Py_None;
}

/* before: conn not registered iff protocol concurrent */
/* after:  conn registered (except for bad ca TypeError exception) */
static PyObject *
  ilumod_UnexpectedException(PyObject *self, PyObject *args)
{
  PyObject *        excType = PySys_GetObject("exc_type");
  PyObject *        excVal  = PySys_GetObject("exc_value");
  PyObject *        excTb   = PySys_GetObject("exc_traceback");
  IlucaObject *     ca;

  if (!PyArg_Parse(args, "O", &ca))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  ILU_ERR_CONS0(unknown, &ca->err, ILU_NIL);
  Py_XINCREF(excType);
  Py_XINCREF(excVal);
  Py_XINCREF(excTb);
  PyErr_Restore(excType, excVal, excTb);
  return 0;
}

#ifdef ILU_CORBA_PYTHON_MAPPING

/* before: conn not registered iff protocol concurrent */
/* after:  conn registered (except for bad ca TypeError exception) */
static PyObject *
  ilumod_CaughtSystemException(PyObject *self, PyObject *args)
{
  char *	excn_name;
  long		minor;
  int		completion_status;
  IlucaObject * ca;
  ilu_ProtocolException	pe;
  ilu_Error	err;

  if (!PyArg_ParseTuple(args, "Osli", &ca, &excn_name, &minor, &completion_status))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }

  ilu_DebugPrintf ("ILU/Python(iluRt.CaughtSystemException):  caught %s exception, minor = %ld, completion_status = %d\n",
		   excn_name, minor, completion_status);

  if (strcmp(excn_name, "BAD_PARAM") == 0)
    pe = ilu_ProtocolException_GarbageArguments;
  else
    pe = ilu_ProtocolException_Unknown;

  if (!ilu_BeginException(ca->call, -(int) pe, 0, &err)) {
    ilu_DebugPrintf("ILU/Python(iluRt.CaughtSystemException):  Error %s on call to ilu_BeginException.\n",
		    ILU_ERR_NAME(err));
    ILU_HANDLED(err);
  } else if (!ilu_FinishException(ca->call, &err)) {
    ilu_DebugPrintf("ILU/Python(iluRt.CaughtSystemException):  Error %s on call to ilu_FinishException.\n",
		    ILU_ERR_NAME(err));
    ILU_HANDLED(err);
  }

  Py_INCREF(Py_None);
  return Py_None;
}

#endif /* def ILU_CORBA_PYTHON_MAPPING */

/************* begin marshalling/unmarshalling routines *************/

static PyObject *
  ilumod_SizeOfObjectID(PyObject *self, PyObject *args)
{
  IluclObject *     icl;
  IlucaObject *     ca;
  PyObject *        inst;
  int           isDiscrim;
  PyClassObject *   pycl;
  IvObject *        iv;
  ilu_Object        kobj;
  ilu_Class     kclass;
  long          size;

  if (!PyArg_Parse(args, "(OOiO)", &ca, &inst, &isDiscrim, &pycl))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  if (!PyClass_Check(pycl))
    {
      PyErr_SetString(PyExc_TypeError, "arg4 should be class");
      return 0;
    }
  if ((kclass = getKernelClass(pycl)) == 0)
    return 0;
  if ((icl = getPythonClass(pycl)) == 0)
    return 0;
  if (!(PyInstance_Check(inst) ||
	(inst == Py_None && !isDiscrim && icl->optional)))
    {
      PyErr_SetString(PyExc_TypeError, "arg2 should be instance");
      return 0;
    }
  if (inst != Py_None)
    {
      if ((iv = getInstVarsWithKobj((PyInstanceObject *) inst)) == 0)
	return 0;
      kobj = iv->kobj;
    }
  else
    kobj = ILU_NIL;
  size = ilu_SizeOfObjectID(ca->call, kobj, BOOLEAN(isDiscrim), kclass, &ca->err); /* non-blocking */
  if (kobj != ILU_NIL)
    ilu_ExitServer(iv->kserver, iv->kclass); /* non-blocking */
  if (ILU_ERRNOK(ca->err))
    return _ilupython_RaiseILUGeneralError(&ca->err);
  return PyInt_FromLong(size);
}

static PyObject *
  ilumod_OutputObjectID(PyObject *self, PyObject *args)
{
  IluclObject *     icl;
  IlucaObject *     ca;
  PyObject *        inst;
  int           isDiscrim;
  PyClassObject *       pycl;
  IvObject *        iv;
  ilu_Object        kobj;
  ilu_Class     kclass;

  if (!PyArg_Parse(args, "(OOiO)", &ca, &inst, &isDiscrim, &pycl))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }

  if (!PyClass_Check(pycl))
    {
      PyErr_SetString(PyExc_TypeError, "arg4 should be class");
      return 0;
    }

  if ((kclass = getKernelClass(pycl)) == 0)
    return 0;

  if ((icl = getPythonClass(pycl)) == 0)
    return 0;

  if (!(PyInstance_Check(inst) ||
	(inst == Py_None && !isDiscrim && icl->optional)))
    {
      PyErr_SetString(PyExc_TypeError, "arg2 should be instance");
      return 0;
    }

  if (inst != Py_None)
    {
      if ((iv = getInstVarsWithKobj((PyInstanceObject *) inst)) == 0)
	return 0;
      kobj = iv->kobj;
    }
  else
    kobj = 0;

  CALL_KERNEL(ilupython_threaded_operation, ilu_OutputObjectID(ca->call, kobj, BOOLEAN(isDiscrim), kclass, &ca->err));

  if (ILU_ERRNOK(ca->err))
    return _ilupython_RaiseILUGeneralError(&ca->err);

  Py_INCREF(Py_None);
  return Py_None;
}

static PyObject *
  ilumod_InputObjectID(PyObject *self, PyObject *args)
{
  IluclObject * icl;
  IlucaObject * ca;
  int       isDiscrim;
  PyClassObject *   pycl;
  ilu_Class kclass;
  ilu_Object    kobj;
  PyObject *    pyobj;

  if (!PyArg_Parse(args, "(OiO)", &ca, &isDiscrim, &pycl))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  if (!PyClass_Check(pycl))
    {
      PyErr_SetString(PyExc_TypeError, "arg3 should be class");
      return 0;
    }
  if ((kclass = getKernelClass(pycl)) == 0)
    return 0;
  if ((icl = getPythonClass(pycl)) == 0)
    return 0;

  CALL_KERNEL(ilupython_threaded_operation, ilu_InputObjectID(ca->call, &kobj, BOOLEAN(isDiscrim), kclass, &ca->err));

  if (ILU_ERRNOK(ca->err))
    return _ilupython_RaiseILUGeneralError(&ca->err);
  if (kobj == 0)
    {
      if (isDiscrim || !icl->optional)
	{
	  PyErr_SetString(_ilupython_GeneralError,
			  "ilu_InputObjectID failed");
	  return 0;
	}
      Py_INCREF(Py_None);
      return Py_None;
    }
  if (isDiscrim)
    pyobj = getPythonTrueObject(kobj, kclass);
  else
    pyobj = getPythonObject(kobj, kclass);
  if (NewObjects != ILU_NIL) {
    removeObjFromList (NewObjects, pyobj);
  };
  return pyobj;
}

/********************************/

static PyObject *
  ilumod_SizeOfShortInteger(PyObject *self, PyObject *args)
{
  IlucaObject *     ca;
  long          value, s;

  if (!PyArg_Parse(args, "(Ol)", &ca, &value))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  if ((ilu_shortinteger) value != value)
    {
#if defined(WIN32)
      ilu_Error *e;
      e = &ca->err;
#endif
      PyErr_SetString(PyExc_ValueError, strArg2OutOfRange);
#if defined(WIN32)
      ILU_ERR_CONS1(marshal, e, minor, ilu_mm_badInteger, 0);
#else
      ILU_ERR_CONS1(marshal, &ca->err, minor, ilu_mm_badInteger, 0);
#endif
      return 0;
    }
  s = ilu_SizeOfShortInteger(ca->call, value, &ca->err); /* non-blocking */
  if (ILU_ERRNOK(ca->err))
    return _ilupython_RaiseILUGeneralError(&ca->err);
  return PyInt_FromLong(s);
}

static PyObject *
  ilumod_OutputShortInteger(PyObject *self, PyObject *args)
{
  IlucaObject * ca;
  long      value;

  if (!PyArg_Parse(args, "(Ol)", &ca, &value))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  if ((ilu_shortinteger) value != value)
    {
#if defined(WIN32)
      ilu_Error *e;
      e = &ca->err;
#endif
      PyErr_SetString(PyExc_ValueError, strArg2OutOfRange);
#if defined(WIN32)
      ILU_ERR_CONS1(marshal, e, minor, ilu_mm_badInteger, 0);
#else
      ILU_ERR_CONS1(marshal, &ca->err, minor, ilu_mm_badInteger, 0);
#endif
      return 0;
    }

  CALL_KERNEL(ilupython_threaded_operation, ilu_OutputShortInteger(ca->call, value, &ca->err));

  if (ILU_ERRNOK(ca->err))
    return _ilupython_RaiseILUGeneralError(&ca->err);
  Py_INCREF(Py_None);
  return Py_None;
}

static PyObject *
  ilumod_InputShortInteger(PyObject *self, PyObject *args)
{
  IlucaObject *     ca;
  ilu_shortinteger  value;

  if (!PyArg_Parse(args, "O", &ca))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }

  CALL_KERNEL(ilupython_threaded_operation, ilu_InputShortInteger(ca->call, &value, &ca->err));

  if (ILU_ERRNOK(ca->err))
    return _ilupython_RaiseILUGeneralError(&ca->err);
  return PyInt_FromLong(value);
}

/********************************/

static PyObject *
  ilumod_SizeOfInteger(PyObject *self, PyObject *args)
{
  IlucaObject * ca;
  long      value, s;

  if (!PyArg_Parse(args, "(Ol)", &ca, &value))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }

  s = ilu_SizeOfInteger(ca->call, value, &ca->err); /* non-blocking */

  if (ILU_ERRNOK(ca->err))
    return _ilupython_RaiseILUGeneralError(&ca->err);
  return (PyInt_FromLong(s));
}

static PyObject *
  ilumod_OutputInteger(PyObject *self, PyObject *args)
{
  IlucaObject * ca;
  long      value;

  if (!PyArg_Parse(args, "(Ol)", &ca, &value))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }

  CALL_KERNEL(ilupython_threaded_operation, ilu_OutputInteger(ca->call, value, &ca->err));

  if (ILU_ERRNOK(ca->err))
    return _ilupython_RaiseILUGeneralError(&ca->err);
  Py_INCREF(Py_None);
  return Py_None;
}

static PyObject *
  ilumod_InputInteger(PyObject *self, PyObject *args)
{
  IlucaObject * ca;
  ilu_integer   value;

  if (!PyArg_Parse(args, "O", &ca))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }

  CALL_KERNEL(ilupython_threaded_operation, ilu_InputInteger(ca->call, &value, &ca->err));
  if (ILU_ERRNOK(ca->err))
    return _ilupython_RaiseILUGeneralError(&ca->err);
  return PyInt_FromLong(value);
}

/********************************/

/* Returns divmod(v, 2**32), after checking that v is in [lo, hi]. */
static PyObject *
  decomposeLongInt(PyObject *v, char *hexLoBound, char *hexHiBound)
{
  int       isTooSmall;
  int       isTooLarge;
  PyObject *    liLo;
  PyObject *    liHi;
  PyObject *    li2_32;
  PyObject *    divmodTuple;

  if ((liLo = PYTHON_LONG_FROM_STRING(hexLoBound, 16)) == 0)
    return 0;
  isTooSmall = PyObject_Compare(v, liLo) < 0;
  Py_DECREF(liLo);
  if (isTooSmall)
    {
      PyErr_SetString(PyExc_ValueError, strArg2OutOfRange);
      return 0;
    }

  if ((liHi = PYTHON_LONG_FROM_STRING(hexHiBound, 16)) == 0)
    return 0;
  isTooLarge = PyObject_Compare(v, liHi) > 0;
  Py_DECREF(liHi);
  if (isTooLarge)
    {
      PyErr_SetString(PyExc_ValueError, strArg2OutOfRange);
      return 0;
    }

  if ((li2_32 = PYTHON_LONG_FROM_STRING("100000000", 16)) == 0)
    return 0;
  divmodTuple = (*v->ob_type->tp_as_number->nb_divmod)(v, li2_32);
  Py_DECREF(li2_32);
  return divmodTuple;
}

static int
  getIluLongInteger(PyObject *v, ilu_longinteger *pValue)
{
  if (PyInt_Check(v))
    {
      long  value   = PyInt_AsLong(v);

      ILU_LONGINT_LOW_WORD(pValue) = value & 0xffffffff;

      /* Behavior of a single 32-bit shift is not always defined. */
      ILU_LONGINT_HIGH_WORD(pValue) = value >> 16;
      ILU_LONGINT_HIGH_WORD(pValue) >>= 16;
      return 1;
    }
  if (PyLong_Check(v))
    {
      PyObject *    divmodTuple;

      if ((divmodTuple = decomposeLongInt(v,
					  "-8000000000000000", "7fffffffffffffff")) == 0)
	return 0;
      ILU_LONGINT_LOW_WORD(pValue) = PyLong_AsDouble(PyTuple_GetItem(divmodTuple, 1));
      ILU_LONGINT_HIGH_WORD(pValue) = PyLong_AsDouble(PyTuple_GetItem(divmodTuple, 0));
      Py_DECREF(divmodTuple);
      return 1;
    }
  PyErr_SetString(PyExc_TypeError, strArg2ShouldBeIntOrLongInt);
  return 0;
}

static PyObject *
  ilumod_SizeOfLongInteger(PyObject *self, PyObject *args)
{
  IlucaObject * ca;
  PyObject *        v;
  ilu_longinteger   value;
  ilu_cardinal      s;

  if (!PyArg_Parse(args, "(OO)", &ca, &v))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  if (!getIluLongInteger(v, &value))
    {
#if defined(WIN32)
      ilu_Error *e;
      e = &ca->err;
      ILU_ERR_CONS1(marshal, e, minor, ilu_mm_badInteger, 0);
#else
      ILU_ERR_CONS1(marshal, &ca->err, minor, ilu_mm_badInteger, 0);
#endif
      return 0;
    }
  s = ilu_SizeOfLongInteger(ca->call, value, &ca->err); /* non-blocking */
  if (ILU_ERRNOK(ca->err))
    return _ilupython_RaiseILUGeneralError(&ca->err);
  return PyInt_FromLong(s);
}

static PyObject *
  ilumod_OutputLongInteger(PyObject *self, PyObject *args)
{
  IlucaObject * ca;
  PyObject *    v;
  ilu_longinteger   value;

  if (!PyArg_Parse(args, "(OO)", &ca, &v))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  if (!getIluLongInteger(v, &value))
    {
#if defined(WIN32)
      ilu_Error *e;
      e = &ca->err;
      ILU_ERR_CONS1(marshal, e, minor, ilu_mm_badInteger, 0);
#else
      ILU_ERR_CONS1(marshal, &ca->err, minor, ilu_mm_badInteger, 0);
#endif
      return 0;
    }

  CALL_KERNEL(ilupython_threaded_operation, ilu_OutputLongInteger(ca->call, value, &ca->err));

  if (ILU_ERRNOK(ca->err))
    return _ilupython_RaiseILUGeneralError(&ca->err);
  Py_INCREF(Py_None);
  return Py_None;
}

static PyObject *
  ilumod_InputLongInteger(PyObject *self, PyObject *args)
{
  IlucaObject * ca;
  ilu_longinteger   value;
  int       sign;
  char      image[18];

  if (!PyArg_Parse(args, "O", &ca))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }

  CALL_KERNEL(ilupython_threaded_operation, ilu_InputLongInteger(ca->call, &value, &ca->err));

  if (ILU_ERRNOK(ca->err))
    return _ilupython_RaiseILUGeneralError(&ca->err);

  if (ILU_LONGINT_HIGH_WORD(&value) < 0)
    {
      sign = '-';
      ILU_LONGINT_LOW_WORD(&value) = 0xffffffff & - ILU_LONGINT_LOW_WORD(&value);
      ILU_LONGINT_HIGH_WORD(&value) = ~ ILU_LONGINT_HIGH_WORD(&value);
      if (ILU_LONGINT_LOW_WORD(&value) == 0)
	ILU_LONGINT_HIGH_WORD(&value) += 1;
    }
  else
    sign = '+';
  sprintf(image, "%c%08lx%08lx", sign, (unsigned long) ILU_LONGINT_HIGH_WORD(&value),
	  (unsigned long) ILU_LONGINT_LOW_WORD(&value));
  return PYTHON_LONG_FROM_STRING(image, 16);
}

/********************************/

static PyObject *
  ilumod_SizeOfShortCardinal(PyObject *self, PyObject *args)
{
  IlucaObject * ca;
  long      value;
  ilu_cardinal  s;

  if (!PyArg_Parse(args, "(Ol)", &ca, &value))
    return 0;

  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  if ((ilu_shortcardinal) value != value)
    {
#if defined(WIN32)
      ilu_Error *e;
      e = &ca->err;
#endif
      PyErr_SetString(PyExc_ValueError, strArg2OutOfRange);
#if defined(WIN32)
      ILU_ERR_CONS1(marshal, e, minor, ilu_mm_badInteger, 0);
#else
      ILU_ERR_CONS1(marshal, &ca->err, minor, ilu_mm_badInteger, 0);
#endif
      return 0;
    }
  s = ilu_SizeOfShortCardinal(ca->call, value, &ca->err); /* non-blocking */
  if (ILU_ERRNOK(ca->err))
    return _ilupython_RaiseILUGeneralError(&ca->err);
  return PyInt_FromLong(s);
}

static PyObject *
  ilumod_OutputShortCardinal(PyObject *self, PyObject *args)
{
  IlucaObject * ca;
  long      value;

  if (!PyArg_Parse(args, "(Ol)", &ca, &value))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  if ((ilu_shortcardinal) value != value)
    {
#if defined(WIN32)
      ilu_Error *e;
      e = &ca->err;
#endif
      PyErr_SetString(PyExc_ValueError, strArg2OutOfRange);
#if defined(WIN32)
      ILU_ERR_CONS1(marshal, e, minor, ilu_mm_badInteger, 0);
#else
      ILU_ERR_CONS1(marshal, &ca->err, minor, ilu_mm_badInteger, 0);
#endif
      return 0;
    }

  CALL_KERNEL(ilupython_threaded_operation, ilu_OutputShortCardinal(ca->call, value, &ca->err));

  if (ILU_ERRNOK(ca->err))
    return _ilupython_RaiseILUGeneralError(&ca->err);
  Py_INCREF(Py_None);
  return Py_None;
}

static PyObject *
  ilumod_InputShortCardinal(PyObject *self, PyObject *args)
{
  IlucaObject *     ca;
  ilu_shortcardinal value;

  if (!PyArg_Parse(args, "O", &ca))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }

  CALL_KERNEL(ilupython_threaded_operation, ilu_InputShortCardinal(ca->call, &value, &ca->err));

  if (ILU_ERRNOK(ca->err))
    return _ilupython_RaiseILUGeneralError(&ca->err);
  return PyInt_FromLong(value);
}

/********************************/

static int
  getIluCardinal(PyObject *v, ilu_cardinal *pValue)
{
  if (PyInt_Check(v))
    {
      long  value   = PyInt_AsLong(v);

      if (value < 0)
	{
	  PyErr_SetString(PyExc_ValueError, strArg2OutOfRange);
	  return 0;
	}
      *pValue = value;
      return 1;
    }
  if (PyLong_Check(v))
    {
      double    value   = PyLong_AsDouble(v);

      if (value < 0 || value > (unsigned) 0xffffffff)
	{
	  PyErr_SetString(PyExc_ValueError, strArg2OutOfRange);
	  return 0;
	}
      *pValue = value;
      return 1;
    }
  PyErr_SetString(PyExc_TypeError, strArg2ShouldBeIntOrLongInt);
  return 0;
}

static PyObject *
  ilumod_SizeOfCardinal(PyObject *self, PyObject *args)
{
  IlucaObject * ca;
  PyObject *    v;
  ilu_cardinal  value;
  ilu_cardinal  s;

  if (!PyArg_Parse(args, "(OO)", &ca, &v))
    return 0;

  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }

  if (!getIluCardinal(v, &value))
#if defined(WIN32)
    {
      ilu_Error *e;
      e = &ca->err;
      return ILU_ERR_CONS1(marshal, e, minor, ilu_mm_badInteger, (PyObject *) 0);
    }
#else
  return ILU_ERR_CONS1(marshal, &ca->err, minor, ilu_mm_badInteger, ILU_NIL);
#endif

  s = ilu_SizeOfCardinal(ca->call, value, &ca->err); /* non-blocking */

  if (ILU_ERRNOK(ca->err))
    return _ilupython_RaiseILUGeneralError(&ca->err);
  return PyInt_FromLong(s);
}

static PyObject *
  ilumod_OutputCardinal(PyObject *self, PyObject *args)
{
  IlucaObject * ca;
  PyObject *    v;
  ilu_cardinal  value;

  if (!PyArg_Parse(args, "(OO)", &ca, &v))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  if (!getIluCardinal(v, &value))
#if defined(WIN32)
    {
      ilu_Error *e;
      e = &ca->err;
      return ILU_ERR_CONS1(marshal, e, minor, ilu_mm_badInteger, (PyObject *) 0);
    }
#else
  return ILU_ERR_CONS1(marshal, &ca->err, minor, ilu_mm_badInteger, (PyObject *) 0);
#endif

  CALL_KERNEL(ilupython_threaded_operation, ilu_OutputCardinal(ca->call, value, &ca->err));

  if (ILU_ERRNOK(ca->err))
    return _ilupython_RaiseILUGeneralError(&ca->err);
  Py_INCREF(Py_None);
  return Py_None;
}

static PyObject *
  ilumod_InputCardinal(PyObject *self, PyObject *args)
{
  IlucaObject * ca;
  ilu_cardinal  value;

  if (!PyArg_Parse(args, "O", &ca))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }

  CALL_KERNEL(ilupython_threaded_operation, ilu_InputCardinal(ca->call, &value, &ca->err));

  if (ILU_ERRNOK(ca->err))
    return _ilupython_RaiseILUGeneralError(&ca->err);
  if ((long) value >= 0)
    return PyLong_FromLong(value); /* presumably faster */
  return PyLong_FromDouble((double) value);
}

/********************************/

static int
  getIluLongCardinal(PyObject *v, ilu_longcardinal *pValue)
{
  if (PyInt_Check(v))
    {
      long  value   = PyInt_AsLong(v);

      if (value < 0)
	{
	  PyErr_SetString(PyExc_ValueError, strArg2OutOfRange);
	  return 0;
	}
      ILU_LONGCARD_LOW_WORD(pValue) = value;
      ILU_LONGCARD_HIGH_WORD(pValue) = 0;
      return 1;
    }
  if (PyLong_Check(v))
    {
      PyObject *    divmodTuple;

      if ((divmodTuple = decomposeLongInt(v,
					  "0", "ffffffffffffffff")) == 0)
	return 0;
      ILU_LONGCARD_LOW_WORD(pValue) = PyLong_AsDouble(PyTuple_GetItem(divmodTuple, 1));
      ILU_LONGCARD_HIGH_WORD(pValue) = PyLong_AsDouble(PyTuple_GetItem(divmodTuple, 0));
      Py_DECREF(divmodTuple);
      return 1;
    }
  PyErr_SetString(PyExc_TypeError, strArg2ShouldBeIntOrLongInt);
  return 0;
}

static PyObject *
  ilumod_SizeOfLongCardinal(PyObject *self, PyObject *args)
{
  IlucaObject *     ca;
  PyObject *        v;
  ilu_longcardinal  value;
  ilu_cardinal  s;

  if (!PyArg_Parse(args, "(OO)", &ca, &v))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  if (!getIluLongCardinal(v, &value))
#if defined(WIN32)
    {
      ilu_Error *e;
      e = &ca->err;
      return ILU_ERR_CONS1(marshal, e, minor, ilu_mm_badInteger, (PyObject *) 0);
    }
#else
  return ILU_ERR_CONS1(marshal, &ca->err, minor, ilu_mm_badInteger, (PyObject *) 0);
#endif

  s = ilu_SizeOfLongCardinal(ca->call, value, &ca->err); /* non-blocking */
  if (ILU_ERRNOK(ca->err))
    return _ilupython_RaiseILUGeneralError(&ca->err);
  return PyInt_FromLong(s);
}

static PyObject *
  ilumod_OutputLongCardinal(PyObject *self, PyObject *args)
{
  IlucaObject *     ca;
  PyObject *        v;
  ilu_longcardinal  value;

  if (!PyArg_Parse(args, "(OO)", &ca, &v))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  if (!getIluLongCardinal(v, &value))
#if defined(WIN32)
    {
      ilu_Error *e;
      e = &ca->err;
      return ILU_ERR_CONS1(marshal, e, minor, ilu_mm_badInteger, (PyObject *) 0);
    }
#else
  return ILU_ERR_CONS1(marshal, &ca->err, minor, ilu_mm_badInteger, (PyObject *) 0);
#endif

  CALL_KERNEL(ilupython_threaded_operation, ilu_OutputLongCardinal(ca->call, value, &ca->err));
  if (ILU_ERRNOK(ca->err))
    return _ilupython_RaiseILUGeneralError(&ca->err);
  Py_INCREF(Py_None);
  return Py_None;
}

static PyObject *
  ilumod_InputLongCardinal(PyObject *self, PyObject *args)
{
  IlucaObject *     ca;
  ilu_longcardinal  value;
  char          image[17];

  if (!PyArg_Parse(args, "O", &ca))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  CALL_KERNEL(ilupython_threaded_operation, ilu_InputLongCardinal(ca->call, &value, &ca->err));
  if (ILU_ERRNOK(ca->err))
    return _ilupython_RaiseILUGeneralError(&ca->err);
  sprintf(image, "%08lx%08lx", (unsigned long) ILU_LONGCARD_HIGH_WORD(&value),
	  (unsigned long) ILU_LONGCARD_LOW_WORD(&value));
  return PYTHON_LONG_FROM_STRING(image, 16);
}

/********************************/

static PyObject *
  ilumod_SizeOfShortReal(PyObject *self, PyObject *args)
{
  IlucaObject * ca;
  float     value;
  ilu_cardinal  s;

  if (!PyArg_Parse(args, "(Of)", &ca, &value))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  s = ilu_SizeOfShortReal(ca->call, value, &ca->err); /* non-blocking */
  if (ILU_ERRNOK(ca->err))
    return _ilupython_RaiseILUGeneralError(&ca->err);
  return PyInt_FromLong(s);
}

static PyObject *
  ilumod_OutputShortReal(PyObject *self, PyObject *args)
{
  IlucaObject * ca;
  float     value;

  if (!PyArg_Parse(args, "(Of)", &ca, &value))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  CALL_KERNEL(ilupython_threaded_operation, ilu_OutputShortReal(ca->call, value, &ca->err));
  if (ILU_ERRNOK(ca->err))
    return _ilupython_RaiseILUGeneralError(&ca->err);
  Py_INCREF(Py_None);
  return Py_None;
}

static PyObject *
  ilumod_InputShortReal(PyObject *self, PyObject *args)
{
  IlucaObject * ca;
  float     value;

  if (!PyArg_Parse(args, "O", &ca))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  CALL_KERNEL(ilupython_threaded_operation, ilu_InputShortReal(ca->call, &value, &ca->err));
  if (ILU_ERRNOK(ca->err))
    return _ilupython_RaiseILUGeneralError(&ca->err);
  return PyFloat_FromDouble(value);
}

/********************************/

static PyObject *
  ilumod_SizeOfReal(PyObject *self, PyObject *args)
{
  IlucaObject * ca;
  double        value;
  ilu_cardinal  s;

  if (!PyArg_Parse(args, "(Od)", &ca, &value))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  s = ilu_SizeOfReal(ca->call, value, &ca->err); /* non-blocking */
  if (ILU_ERRNOK(ca->err))
    return _ilupython_RaiseILUGeneralError(&ca->err);
  return PyInt_FromLong(s);
}

static PyObject *
  ilumod_OutputReal(PyObject *self, PyObject *args)
{
  IlucaObject * ca;
  double        value;

  if (!PyArg_Parse(args, "(Od)", &ca, &value))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  CALL_KERNEL(ilupython_threaded_operation, ilu_OutputReal(ca->call, value, &ca->err));
  if (ILU_ERRNOK(ca->err))
    return _ilupython_RaiseILUGeneralError(&ca->err);
  Py_INCREF(Py_None);
  return Py_None;
}

static PyObject *
  ilumod_InputReal(PyObject *self, PyObject *args)
{
  IlucaObject * ca;
  double        value;

  if (!PyArg_Parse(args, "O", &ca))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  CALL_KERNEL(ilupython_threaded_operation, ilu_InputReal(ca->call, &value, &ca->err));
  if (ILU_ERRNOK(ca->err))
    return _ilupython_RaiseILUGeneralError(&ca->err);
  return PyFloat_FromDouble(value);
}

/********************************/

static PyObject *
  ilumod_SizeOfLongReal(PyObject *self, PyObject *args)
{
  IlucaObject * ca;
  IlulrObject * v;
  ilu_cardinal  s;

  if (!PyArg_Parse(args, "(OO)", &ca, &v))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  if (!ilulr_Check(v))
    {
      PyErr_SetString(PyExc_TypeError, "arg2 should be ilu_longreal");
      return 0;
    }
  s = ilu_SizeOfLongReal(ca->call, ilulr_AS_LONGREAL(v), &ca->err); /* non-blocking */
  if (ILU_ERRNOK(ca->err))
    return _ilupython_RaiseILUGeneralError(&ca->err);
  return PyInt_FromLong(s);
}

static PyObject *
  ilumod_OutputLongReal(PyObject *self, PyObject *args)
{
  IlucaObject * ca;
  IlulrObject * v;

  if (!PyArg_Parse(args, "(OO)", &ca, &v))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  if (!ilulr_Check(v))
    {
      PyErr_SetString(PyExc_TypeError, "arg2 should be ilu_longreal");
      return 0;
    }
  CALL_KERNEL(ilupython_threaded_operation, ilu_OutputLongReal(ca->call, ilulr_AS_LONGREAL(v), &ca->err));
  if (ILU_ERRNOK(ca->err))
    return _ilupython_RaiseILUGeneralError(&ca->err);
  Py_INCREF(Py_None);
  return Py_None;
}

static PyObject *
  ilumod_InputLongReal(PyObject *self, PyObject *args)
{
  IlucaObject * ca;
  ilu_longreal  value;

  if (!PyArg_Parse(args, "O", &ca))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  CALL_KERNEL(ilupython_threaded_operation, ilu_InputLongReal(ca->call, &value, &ca->err));
  if (ILU_ERRNOK(ca->err))
    return _ilupython_RaiseILUGeneralError(&ca->err);
  return ilulr_FromLongReal(value);
}

/********************************/

static PyObject *
  ilumod_SizeOfEnum(PyObject *self, PyObject *args)
{
  IlucaObject * ca;
  int       value;
  ilu_cardinal  s;

  if (!PyArg_Parse(args, "(Oi)", &ca, &value))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  s = ilu_SizeOfEnum(ca->call, value, ILU_NIL, &ca->err); /* non-blocking */
  if (ILU_ERRNOK(ca->err))
    return _ilupython_RaiseILUGeneralError(&ca->err);
  return PyInt_FromLong(s);
}

static PyObject *
  ilumod_OutputEnum(PyObject *self, PyObject *args)
{
  IlucaObject * ca;
  int       value;

  if (!PyArg_Parse(args, "(Oi)", &ca, &value))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  CALL_KERNEL(ilupython_threaded_operation, ilu_OutputEnum(ca->call, value, ILU_NIL, &ca->err));
  if (ILU_ERRNOK(ca->err))
    return _ilupython_RaiseILUGeneralError(&ca->err);
  Py_INCREF(Py_None);
  return Py_None;
}

static PyObject *
  ilumod_InputEnum(PyObject *self, PyObject *args)
{
  IlucaObject *     ca;
  ilu_shortcardinal value;

  if (!PyArg_Parse(args, "O", &ca))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  CALL_KERNEL(ilupython_threaded_operation, ilu_InputEnum(ca->call, &value, ILU_NIL, &ca->err));
  if (ILU_ERRNOK(ca->err))
    return _ilupython_RaiseILUGeneralError(&ca->err);
  return PyInt_FromLong(value);
}

/********************************/

static PyObject *
  ilumod_SizeOfCharacter(PyObject *self, PyObject *args)
{
  IlucaObject * ca;
  long      value;
  ilu_cardinal  s;

  if (!PyArg_Parse(args, "(Ol)", &ca, &value))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  if ((ilu_character) value != value)
    {
#if defined(WIN32)
      ilu_Error *e;
      e = &ca->err;
#endif
      PyErr_SetString(PyExc_ValueError, strArg2OutOfRange);
#if defined(WIN32)
      return ILU_ERR_CONS1(marshal, e, minor, ilu_mm_badInteger, (PyObject *) 0);
#else
      return ILU_ERR_CONS1(marshal, &ca->err, minor, ilu_mm_badInteger, (PyObject *) 0);
#endif
    }
  s = ilu_SizeOfCharacter(ca->call, value, &ca->err); /* non-blocking */
  if (ILU_ERRNOK(ca->err))
    return _ilupython_RaiseILUGeneralError(&ca->err);
  return PyInt_FromLong(s);
}

static PyObject *
  ilumod_OutputCharacter(PyObject *self, PyObject *args)
{
  IlucaObject * ca;
  long      value;

  if (!PyArg_Parse(args, "(Ol)", &ca, &value))
    return 0;
  if (!iluca_Check(ca))
    {
#if defined(WIN32)
      ilu_Error *e;
      e = &ca->err;
#endif
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
#if defined(WIN32)
      return ILU_ERR_CONS1(marshal, e, minor, ilu_mm_badInteger, (PyObject *) 0);
#else
      return ILU_ERR_CONS1(marshal, &ca->err, minor, ilu_mm_badInteger, (PyObject *) 0);
#endif
    }
  if ((ilu_character) value != value)
    {
      PyErr_SetString(PyExc_ValueError, strArg2OutOfRange);
      return 0;
    }

  CALL_KERNEL(ilupython_threaded_operation, ilu_OutputCharacter(ca->call, value, &ca->err));

  if (ILU_ERRNOK(ca->err))
    return _ilupython_RaiseILUGeneralError(&ca->err);

  Py_INCREF(Py_None);
  return Py_None;
}

static PyObject *
  ilumod_InputCharacter(PyObject *self, PyObject *args)
{
  IlucaObject * ca;
  ilu_character value;

  if (!PyArg_Parse(args, "O", &ca))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }

  CALL_KERNEL(ilupython_threaded_operation, ilu_InputCharacter(ca->call, &value, &ca->err));

  if (ILU_ERRNOK(ca->err))
    return _ilupython_RaiseILUGeneralError(&ca->err);
  return PyInt_FromLong(value);
}

/********************************/

static PyObject *
  ilumod_SizeOfShortCharacter(PyObject *self, PyObject *args)
{
  IlucaObject * ca;
  long      value;
  ilu_cardinal  s;
  ilu_shortcharacter    cv;

  if (!PyArg_Parse(args, "(Ol)", &ca, &value))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  cv = value;
  if (cv != value)
    {
#if defined(WIN32)
      ilu_Error *e;
      e = &ca->err;
#endif
      PyErr_SetString(PyExc_ValueError, strArg2OutOfRange);
#if defined(WIN32)
      return ILU_ERR_CONS1(marshal, e, minor, ilu_mm_badInteger, (PyObject *) 0);
#else
      return ILU_ERR_CONS1(marshal, &ca->err, minor, ilu_mm_badInteger, (PyObject *) 0);
#endif
    }
  s = ilu_SizeOfShortCharacter(ca->call, cv, &ca->err); /* non-blocking */
  if (ILU_ERRNOK(ca->err))
    return _ilupython_RaiseILUGeneralError(&ca->err);
  return PyInt_FromLong(s);
}

static PyObject *
  ilumod_OutputShortCharacter(PyObject *self, PyObject *args)
{
  IlucaObject * ca;
  long      value;
  ilu_shortcharacter    cv;

  if (!PyArg_Parse(args, "(Ol)", &ca, &value))
    return 0;
  if (!iluca_Check(ca))
    {
#if defined(WIN32)
      ilu_Error *e;
      e = &ca->err;
#endif
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
#if defined(WIN32)
      return ILU_ERR_CONS1(marshal, e, minor, ilu_mm_badInteger, (PyObject *) 0);
#else
      return ILU_ERR_CONS1(marshal, &ca->err, minor, ilu_mm_badInteger, (PyObject *) 0);
#endif
    }
  cv = value;
  if (cv != value)
    {
      PyErr_SetString(PyExc_ValueError, strArg2OutOfRange);
      return 0;
    }

  CALL_KERNEL(ilupython_threaded_operation, ilu_OutputShortCharacter(ca->call, cv, &ca->err));

  if (ILU_ERRNOK(ca->err))
    return _ilupython_RaiseILUGeneralError(&ca->err);

  Py_INCREF(Py_None);
  return Py_None;
}

static PyObject *
  ilumod_InputShortCharacter(PyObject *self, PyObject *args)
{
  IlucaObject * ca;
  ilu_character value;
  ilu_shortcharacter    cv;

  if (!PyArg_Parse(args, "O", &ca))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }

  CALL_KERNEL(ilupython_threaded_operation, ilu_InputShortCharacter(ca->call, &cv, &ca->err));

  if (ILU_ERRNOK(ca->err))
    return _ilupython_RaiseILUGeneralError(&ca->err);
  return PyInt_FromLong(value = cv);
}

/********************************/

static PyObject *
  ilumod_SizeOfByte(PyObject *self, PyObject *args)
{
  IlucaObject * ca;
  long      value;
  ilu_cardinal  s;

  if (!PyArg_Parse(args, "(Ol)", &ca, &value))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  if ((ilu_byte) value != value)
    {
#if defined(WIN32)
      ilu_Error *e;
      e = &ca->err;
#endif
      PyErr_SetString(PyExc_ValueError, strArg2OutOfRange);
#if defined(WIN32)
      return ILU_ERR_CONS1(marshal, e, minor, ilu_mm_badInteger, (PyObject *) 0);
#else
      return ILU_ERR_CONS1(marshal, &ca->err, minor, ilu_mm_badInteger, (PyObject *) 0);
#endif
    }
  s = ilu_SizeOfByte(ca->call, value, &ca->err); /* non-blocking */
  if (ILU_ERRNOK(ca->err))
    return _ilupython_RaiseILUGeneralError(&ca->err);
  return PyInt_FromLong(s);
}

static PyObject *
  ilumod_OutputByte(PyObject *self, PyObject *args)
{
  IlucaObject * ca;
  long      value;

  if (!PyArg_Parse(args, "(Ol)", &ca, &value))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  if ((ilu_byte) value != value)
    {
#if defined(WIN32)
      ilu_Error *e;
      e = &ca->err;
#endif
      PyErr_SetString(PyExc_ValueError, strArg2OutOfRange);
#if defined(WIN32)
      return ILU_ERR_CONS1(marshal, e, minor, ilu_mm_badInteger, (PyObject *) 0);
#else
      return ILU_ERR_CONS1(marshal, &ca->err, minor, ilu_mm_badInteger, (PyObject *) 0);
#endif
    }
  CALL_KERNEL(ilupython_threaded_operation, ilu_OutputByte(ca->call, value, &ca->err));
  if (ILU_ERRNOK(ca->err))
    return _ilupython_RaiseILUGeneralError(&ca->err);
  Py_INCREF(Py_None);
  return Py_None;
}

static PyObject *
  ilumod_InputByte(PyObject *self, PyObject *args)
{
  IlucaObject * ca;
  ilu_byte  value;

  if (!PyArg_Parse(args, "O", &ca))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  CALL_KERNEL(ilupython_threaded_operation, ilu_InputByte(ca->call, &value, &ca->err));
  if (ILU_ERRNOK(ca->err))
    return _ilupython_RaiseILUGeneralError(&ca->err);
  return PyInt_FromLong(value);
}

/********************************/

static PyObject *
  ilumod_SizeOfBoolean(PyObject *self, PyObject *args)
{
  IlucaObject * ca;
  long      value;
  ilu_cardinal  s;

  if (!PyArg_Parse(args, "(Ol)", &ca, &value))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  if (value != 0 && value != 1)
    {
#if defined(WIN32)
      ilu_Error *e;
      e = &ca->err;
#endif
      PyErr_SetString(PyExc_ValueError, strArg2OutOfRange);
#if defined(WIN32)
      return ILU_ERR_CONS1(marshal, e, minor, ilu_mm_badInteger, (PyObject *) 0);
#else
      return ILU_ERR_CONS1(marshal, &ca->err, minor, ilu_mm_badInteger, (PyObject *) 0);
#endif
    }
  s = ilu_SizeOfBoolean(ca->call, BOOLEAN(value), &ca->err); /* non-blocking */
  if (ILU_ERRNOK(ca->err))
    return _ilupython_RaiseILUGeneralError(&ca->err);
  return PyInt_FromLong(s);
}

static PyObject *
  ilumod_OutputBoolean(PyObject *self, PyObject *args)
{
  IlucaObject * ca;
  long      value;

  if (!PyArg_Parse(args, "(Ol)", &ca, &value))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  if (value != 0 && value != 1)
    {
#if defined(WIN32)
      ilu_Error *e;
      e = &ca->err;
#endif
      PyErr_SetString(PyExc_ValueError, strArg2OutOfRange);
#if defined(WIN32)
      return ILU_ERR_CONS1(marshal, e, minor, ilu_mm_badInteger, (PyObject *) 0);
#else
      return ILU_ERR_CONS1(marshal, &ca->err, minor, ilu_mm_badInteger, (PyObject *) 0);
#endif
    }
  CALL_KERNEL(ilupython_threaded_operation, ilu_OutputBoolean(ca->call, BOOLEAN(value), &ca->err));
  if (ILU_ERRNOK(ca->err))
    return _ilupython_RaiseILUGeneralError(&ca->err);
  Py_INCREF(Py_None);
  return Py_None;
}

static PyObject *
  ilumod_InputBoolean(PyObject *self, PyObject *args)
{
  IlucaObject * ca;
  ilu_boolean   value;

  if (!PyArg_Parse(args, "O", &ca))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  CALL_KERNEL(ilupython_threaded_operation, ilu_InputBoolean(ca->call, &value, &ca->err));
  if (ILU_ERRNOK(ca->err))
    return _ilupython_RaiseILUGeneralError(&ca->err);
  return PyInt_FromLong(value);
}

/********************************/

static ilu_bytes
  vectorToBytes(PyObject *vec, long *pLen, int *pAlloced)
{
  PyObject *    (*getItem)(PyObject *, int);
  int       len;
  ilu_bytes value;
  int       i;

  if (PyString_Check(vec))
    {
      *pLen = PyString_Size(vec);
      *pAlloced = 0;
      return (ilu_bytes) PyString_AsString(vec);
    }
  else if (PyList_Check(vec))
    {
      getItem = PyList_GetItem;
      len = PyList_Size(vec);
    }
  else if (PyTuple_Check(vec))
    {
      getItem = PyTuple_GetItem;
      len = PyTuple_Size(vec);
    }
  else
    {
      PyErr_SetString(PyExc_TypeError,
		      "arg2 should be string, list, or tuple");
      return 0;
    }
  if ((value = PyMem_NEW(ilu_byte, len)) == 0)
    {
      (void) PyErr_NoMemory();
      return 0;
    }
  for (i = 0; i < len; i++)
    {
      PyObject *    item    = (*getItem)(vec, i);
      long      itemVal;

      if (!PyInt_Check(item))
	{
	  PyErr_SetString(PyExc_TypeError,
			  "item should be integer");
	  PyMem_DEL(value);
	  return 0;
	}
      itemVal = PyInt_AsLong(item);
      if ((ilu_byte) itemVal != itemVal)
	{
	  PyErr_SetString(PyExc_ValueError,
			  "item exceeds range of byte");
	  PyMem_DEL(value);
	  return 0;
	}
      value[i] = itemVal;
    }
  *pLen = len;
  *pAlloced = 1;
  return value;
}

static PyObject *
  stringFromBytes(ilu_bytes value, int len)
{
  return PyString_FromStringAndSize((char *) value, len);
}

static PyObject *
  listFromBytes(ilu_bytes value, int len)
{
  PyObject *    list;
  int       i;

  if ((list = PyList_New(len)) == 0)
    return 0;
  for (i = 0; i < len; i++)
    {
      PyObject *    item;
      
      if ((item = PyInt_FromLong(value[i])) == 0)
	{
	  Py_DECREF(list);
	  return 0;
	}
      if (PyList_SetItem(list, i, item) < 0)
	{
	  Py_DECREF(list);
	  return 0;
	}
    }
  return list;
}

static PyObject *
  ilumod_SizeOfBytes(PyObject *self, PyObject *args)
{
  IlucaObject * ca;
  PyObject *    vec;
  long      limit;
  long      len;
  int       alloced;
  ilu_bytes value;
  long      size;

  if (!PyArg_Parse(args, "(OOl)", &ca, &vec, &limit))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  if ((value = vectorToBytes(vec, &len, &alloced)) == 0)
    return 0;
  if (limit > 0 && len > limit)
    {
      PyErr_SetString(PyExc_TypeError, strListTooLong);
      if (alloced)
	PyMem_DEL(value);
      return 0;
    }
  size = ilu_SizeOfBytes(ca->call, value, len, limit, &ca->err); /* non-blocking */
  if (alloced)
    PyMem_DEL(value);
  if (ILU_ERRNOK(ca->err))
    return _ilupython_RaiseILUGeneralError(&ca->err);
  return PyInt_FromLong(size);
}

static PyObject *
  ilumod_OutputBytes(PyObject *self, PyObject *args)
{
  IlucaObject * ca;
  PyObject *    vec;
  long      limit;
  long      len;
  int       alloced;
  ilu_bytes value;

  if (!PyArg_Parse(args, "(OOl)", &ca, &vec, &limit))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  if ((value = vectorToBytes(vec, &len, &alloced)) == 0)
    return 0;
  if (limit > 0 && len > limit)
    {
      PyErr_SetString(PyExc_TypeError, strListTooLong);
      if (alloced)
	PyMem_DEL(value);
      return 0;
    }

  CALL_KERNEL(ilupython_threaded_operation, ilu_OutputBytes(ca->call, value, len, limit, &ca->err));

  if (alloced)
    PyMem_DEL(value);

  if (ILU_ERRNOK(ca->err))
    return _ilupython_RaiseILUGeneralError(&ca->err);
  Py_INCREF(Py_None);
  return Py_None;
}

static PyObject *
  ilumod_InputBytes(PyObject *self, PyObject *args)
{
  IlucaObject * ca;
  long      limit;
  ilu_bytes value;
  ilu_cardinal  len;
  PyObject *    list;

  if (!PyArg_Parse(args, "(Ol)", &ca, &limit))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  value = 0;
  CALL_KERNEL(ilupython_threaded_operation, ilu_InputBytes(ca->call, &value, &len, limit, &ca->err));
  if (ILU_ERRNOK(ca->err))
    return _ilupython_RaiseILUGeneralError(&ca->err);
  list = stringFromBytes(value, len);
  PyMem_XDEL(value);
  return list;
}

/********************************/

static PyObject *
  ilumod_SizeOfString(PyObject *self, PyObject *args)
{
  IlucaObject * ca;
  PyObject *    str;
  long      limit;
  ilu_string    value;
  ilu_cardinal  len;
  ilu_cardinal  s;

  if (!PyArg_Parse(args, "(OOl)", &ca, &str, &limit))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  if (!PyString_Check(str))
    {
      PyErr_SetString(PyExc_TypeError, "arg2 should be string");
      return 0;
    }
  value = PyString_AsString(str);
  len = PyString_Size(str);
  s = ilu_SizeOfString(ca->call, value, len, limit, &ca->err); /* non-blocking */
  if (ILU_ERRNOK(ca->err))
    return _ilupython_RaiseILUGeneralError(&ca->err);
  return PyInt_FromLong(s);
}

static PyObject *
  ilumod_OutputString(PyObject *self, PyObject *args)
{
  IlucaObject * ca;
  PyObject *    str;
  long      limit;
  ilu_string    value;
  ilu_cardinal  len;

  if (!PyArg_Parse(args, "(OOl)", &ca, &str, &limit))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  if (!PyString_Check(str))
    {
      PyErr_SetString(PyExc_TypeError, "arg2 should be string");
      return 0;
    }
  value = PyString_AsString(str);
  len = PyString_Size(str);

  CALL_KERNEL(ilupython_threaded_operation, ilu_OutputString(ca->call, value, len, limit, &ca->err));

  if (ILU_ERRNOK(ca->err))
    return _ilupython_RaiseILUGeneralError(&ca->err);
  Py_INCREF(Py_None);
  return Py_None;
}

static PyObject *
  ilumod_InputString(PyObject *self, PyObject *args)
{
  IlucaObject * ca;
  long      limit;
  ilu_string    value;
  ilu_cardinal  len;
  PyObject *    str;

  if (!PyArg_Parse(args, "(Ol)", &ca, &limit))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  value = 0;

  CALL_KERNEL(ilupython_threaded_operation, ilu_InputString(ca->call, &value, &len, limit, &ca->err));

  if (ILU_ERRNOK(ca->err))
    return _ilupython_RaiseILUGeneralError(&ca->err);
  str = PyString_FromStringAndSize(value, len);
  if (value)
    free(value);
  return str;
}

/********************************/

static ilu_wstring
  vectorToWString(PyObject *vec, long *pLen)
{
  PyObject *    (*getItem)(PyObject *, int);
  int       len;
  ilu_wstring   value;
  int       i;

  if (PyString_Check(vec))
    {
      char *    string  = PyString_AsString(vec);

      len = PyString_Size(vec);
      if ((value = PyMem_NEW(ilu_character, len)) == 0)
	{
	  (void) PyErr_NoMemory();
	  return 0;
	}
      for (i = 0; i < len; i++)
	value[i] = string[i];
      *pLen = len;
      return value;
    }
  else if (PyList_Check(vec))
    {
      getItem = PyList_GetItem;
      len = PyList_Size(vec);
    }
  else if (PyTuple_Check(vec))
    {
      getItem = PyTuple_GetItem;
      len = PyTuple_Size(vec);
    }
  else
    {
      PyErr_SetString(PyExc_TypeError,
		      "arg2 should be string, list, or tuple");
      return 0;
    }
  if ((value = PyMem_NEW(ilu_character, len)) == 0)
    {
      (void) PyErr_NoMemory();
      return 0;
    }
  for (i = 0; i < len; i++)
    {
      PyObject *    item    = (*getItem)(vec, i);
      long      itemVal;

      if (!PyInt_Check(item))
	{
	  PyErr_SetString(PyExc_TypeError,
			  "item should be integer");
	  PyMem_DEL(value);
	  return 0;
	}
      itemVal = PyInt_AsLong(item);
      if ((ilu_character) itemVal != itemVal)
	{
	  PyErr_SetString(PyExc_ValueError,
			  "item exceeds range of ilu character");
	  PyMem_DEL(value);
	  return 0;
	}
      value[i] = itemVal;
    }
  *pLen = len;
  return value;
}

static PyObject *
  listFromWString(ilu_wstring value, int len)
{
  PyObject *    list;
  int       i;

  if ((list = PyList_New(len)) == 0)
    return 0;
  for (i = 0; i < len; i++)
    {
      PyObject *    item;
      
      if ((item = PyInt_FromLong(value[i])) == 0)
	{
	  Py_DECREF(list);
	  return 0;
	}
      if (PyList_SetItem(list, i, item) < 0)
	{
	  Py_DECREF(list);
	  return 0;
	}
    }
  return list;
}

static PyObject *
  ilumod_SizeOfWString(PyObject *self, PyObject *args)
{
  IlucaObject * ca;
  PyObject *    vec;
  long      limit;
  long      len;
  ilu_wstring   value;
  long      size;

  if (!PyArg_Parse(args, "(OOl)", &ca, &vec, &limit))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  if ((value = vectorToWString(vec, &len)) == 0)
    return 0;
  if (limit > 0 && len > limit)
    {
      PyErr_SetString(PyExc_TypeError, strListTooLong);
      PyMem_DEL(value);
      return 0;
    }
  size = ilu_SizeOfWString(ca->call, value, len, limit,
			   &ca->err); /* non-blocking */
  PyMem_DEL(value);
  if (ILU_ERRNOK(ca->err))
    return _ilupython_RaiseILUGeneralError(&ca->err);
  return PyInt_FromLong(size);
}

static PyObject *
  ilumod_OutputWString(PyObject *self, PyObject *args)
{
  IlucaObject * ca;
  PyObject *    vec;
  long      limit;
  long      len;
  ilu_wstring   value;

  if (!PyArg_Parse(args, "(OOl)", &ca, &vec, &limit))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  if ((value = vectorToWString(vec, &len)) == 0)
    return 0;
  if (limit > 0 && len > limit)
    {
      PyErr_SetString(PyExc_TypeError, strListTooLong);
      PyMem_DEL(value);
      return 0;
    }
  CALL_KERNEL(ilupython_threaded_operation, ilu_OutputWString(ca->call, value, len, limit,
							      &ca->err));
  PyMem_DEL(value);
  if (ILU_ERRNOK(ca->err))
    return _ilupython_RaiseILUGeneralError(&ca->err);
  Py_INCREF(Py_None);
  return Py_None;
}

static PyObject *
  ilumod_InputWString(PyObject *self, PyObject *args)
{
  IlucaObject * ca;
  long      limit;
  ilu_wstring   value;
  ilu_cardinal  len;
  PyObject *    list;

  if (!PyArg_Parse(args, "(Ol)", &ca, &limit))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  value = 0;

  CALL_KERNEL(ilupython_threaded_operation, ilu_InputWString(ca->call, &value, &len, limit, &ca->err));

  if (ILU_ERRNOK(ca->err))
    return _ilupython_RaiseILUGeneralError(&ca->err);
  list = listFromWString(value, len);
  PyMem_XDEL(value);
  return list;
}

/********************************/

static PyObject *
  ilumod_SizeOfOpaque(PyObject *self, PyObject *args)
{
  IlucaObject * ca;
  PyObject *    vec;
  long      bound;
  long      len;
  int       alloced;
  ilu_bytes value;
  long      size;

  if (!PyArg_Parse(args, "(OOl)", &ca, &vec, &bound))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  if ((value = vectorToBytes(vec, &len, &alloced)) == 0)
    return 0;
  if (len != bound)
    {
      PyErr_SetString(PyExc_TypeError, strListDoesntMatch);
      if (alloced)
	PyMem_DEL(value);
      return 0;
    }

  size = ilu_SizeOfOpaque(ca->call, value, bound, &ca->err); /* non-blocking */

  if (alloced)
    PyMem_DEL(value);
  if (ILU_ERRNOK(ca->err))
    return _ilupython_RaiseILUGeneralError(&ca->err);
  return PyInt_FromLong(size);
}

static PyObject *
  ilumod_OutputOpaque(PyObject *self, PyObject *args)
{
  IlucaObject * ca;
  PyObject *    vec;
  long      bound;
  long      len;
  int       alloced;
  ilu_bytes value;

  if (!PyArg_Parse(args, "(OOl)", &ca, &vec, &bound))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  if ((value = vectorToBytes(vec, &len, &alloced)) == 0)
    return 0;
  if (len != bound)
    {
      PyErr_SetString(PyExc_TypeError, strListDoesntMatch);
      if (alloced)
	PyMem_DEL(value);
      return 0;
    }
  CALL_KERNEL(ilupython_threaded_operation, ilu_OutputOpaque(ca->call, value, bound, &ca->err));
  if (alloced)
    PyMem_DEL(value);
  if (ILU_ERRNOK(ca->err))
    return _ilupython_RaiseILUGeneralError(&ca->err);
  Py_INCREF(Py_None);
  return Py_None;
}

static PyObject *
  ilumod_InputOpaque(PyObject *self, PyObject *args)
{
  IlucaObject * ca;
  long      bound;
  ilu_bytes value;
  PyObject *    list;

  if (!PyArg_Parse(args, "(Ol)", &ca, &bound))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  value = 0;
  CALL_KERNEL(ilupython_threaded_operation, ilu_InputOpaque(ca->call, &value, bound, &ca->err));
  if (ILU_ERRNOK(ca->err))
    return _ilupython_RaiseILUGeneralError(&ca->err);
  list = stringFromBytes(value, bound);
  PyMem_XDEL(value);
  return list;
}

/********************************/

static PyObject *
  ilumod_SizeOfStringVec(PyObject *self, PyObject *args)
{
  IlucaObject * ca;
  PyObject *    vec;
  long      bound;
  long      len;
  int       alloced;
  ilu_string    value;
  long      size;

  if (!PyArg_Parse(args, "(OOl)", &ca, &vec, &bound))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  if ((value = (ilu_string) vectorToBytes(vec, &len, &alloced)) == 0)
    return 0;
  if (len != bound)
    {
      PyErr_SetString(PyExc_TypeError, strListDoesntMatch);
      if (alloced)
	PyMem_DEL(value);
      return 0;
    }
  size = ilu_SizeOfStringVec(ca->call, value, bound, &ca->err); /* non-blocking */
  if (alloced)
    PyMem_DEL(value);
  if (ILU_ERRNOK(ca->err))
    return _ilupython_RaiseILUGeneralError(&ca->err);
  return PyInt_FromLong(size);
}

static PyObject *
  ilumod_OutputStringVec(PyObject *self, PyObject *args)
{
  IlucaObject * ca;
  PyObject *    vec;
  long      bound;
  long      len;
  int       alloced;
  ilu_string    value;

  if (!PyArg_Parse(args, "(OOl)", &ca, &vec, &bound))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  if ((value = (ilu_string) vectorToBytes(vec, &len, &alloced)) == 0)
    return 0;
  if (len != bound)
    {
      PyErr_SetString(PyExc_TypeError, strListDoesntMatch);
      if (alloced)
	PyMem_DEL(value);
      return 0;
    }
  CALL_KERNEL(ilupython_threaded_operation, ilu_OutputStringVec(ca->call, value, bound, &ca->err));
  if (alloced)
    PyMem_DEL(value);
  if (ILU_ERRNOK(ca->err))
    return _ilupython_RaiseILUGeneralError(&ca->err);
  Py_INCREF(Py_None);
  return Py_None;
}

static PyObject *
  ilumod_InputStringVec(PyObject *self, PyObject *args)
{
  IlucaObject * ca;
  long      bound;
  ilu_string    value;
  PyObject *    list;

  if (!PyArg_Parse(args, "(Ol)", &ca, &bound))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  value = 0;
  CALL_KERNEL(ilupython_threaded_operation, ilu_InputStringVec(ca->call, &value, bound, &ca->err));
  if (ILU_ERRNOK(ca->err))
    return _ilupython_RaiseILUGeneralError(&ca->err);
  list = stringFromBytes((ilu_bytes) value, bound);
  PyMem_XDEL(value);
  return list;
}

/********************************/

static PyObject *
  ilumod_SizeOfWStringVec(PyObject *self, PyObject *args)
{
  IlucaObject * ca;
  PyObject *    vec;
  long      bound;
  long      len;
  ilu_wstring   value;
  long      size;

  if (!PyArg_Parse(args, "(OOl)", &ca, &vec, &bound))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  if ((value = vectorToWString(vec, &len)) == 0)
    return 0;
  if (len != bound)
    {
      PyErr_SetString(PyExc_TypeError, strListDoesntMatch);
      PyMem_DEL(value);
      return 0;
    }
  size = ilu_SizeOfWStringVec(ca->call, value, bound,
			      &ca->err); /* non-blocking */
  PyMem_DEL(value);
  if (ILU_ERRNOK(ca->err))
    return _ilupython_RaiseILUGeneralError(&ca->err);
  return PyInt_FromLong(size);
}

static PyObject *
  ilumod_OutputWStringVec(PyObject *self, PyObject *args)
{
  IlucaObject * ca;
  PyObject *    vec;
  long      bound;
  long      len;
  ilu_wstring   value;

  if (!PyArg_Parse(args, "(OOl)", &ca, &vec, &bound))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  if ((value = vectorToWString(vec, &len)) == 0)
    return 0;
  if (len != bound)
    {
      PyErr_SetString(PyExc_TypeError, strListDoesntMatch);
      PyMem_DEL(value);
      return 0;
    }
  CALL_KERNEL(ilupython_threaded_operation, ilu_OutputWStringVec(ca->call, value, bound,
								 &ca->err));
  PyMem_DEL(value);
  if (ILU_ERRNOK(ca->err))
    return _ilupython_RaiseILUGeneralError(&ca->err);
  Py_INCREF(Py_None);
  return Py_None;
}

static PyObject *
  ilumod_InputWStringVec(PyObject *self, PyObject *args)
{
  IlucaObject * ca;
  long      bound;
  ilu_wstring   value;
  PyObject *    list;

  if (!PyArg_Parse(args, "(Ol)", &ca, &bound))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  value = 0;
  CALL_KERNEL(ilupython_threaded_operation, ilu_InputWStringVec(ca->call, &value, bound, &ca->err));
  if (ILU_ERRNOK(ca->err))
    {
      PyMem_XDEL(value);
      return _ilupython_RaiseILUGeneralError(&ca->err);
    }
  list = listFromWString(value, bound);
  PyMem_XDEL(value);
  return list;
}

/********************************/

static PyObject *
  ilumod_SizeOfOptional(PyObject *self, PyObject *args)
{
  IlucaObject * ca;
  int       present;
  ilu_cardinal  s;

  if (!PyArg_Parse(args, "(Oi)", &ca, &present))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  s = ilu_SizeOfOptional(ca->call, BOOLEAN(present), ILU_NIL, &ca->err); /* non-blocking */
  if (ILU_ERRNOK(ca->err))
    return _ilupython_RaiseILUGeneralError(&ca->err);
  return PyInt_FromLong(s);
}

static PyObject *
  ilumod_OutputOptional(PyObject *self, PyObject *args)
{
  IlucaObject * ca;
  int       present;

  if (!PyArg_Parse(args, "(Oi)", &ca, &present))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  CALL_KERNEL(ilupython_threaded_operation, ilu_OutputOptional(ca->call, BOOLEAN(present), ILU_NIL, &ca->err));
  if (ILU_ERRNOK(ca->err))
    return _ilupython_RaiseILUGeneralError(&ca->err);
  Py_INCREF(Py_None);
  return Py_None;
}

static PyObject *
  ilumod_InputOptional(PyObject *self, PyObject *args)
{
  IlucaObject * ca;
  ilu_boolean   present;

  if (!PyArg_Parse(args, "O", &ca))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  CALL_KERNEL(ilupython_threaded_operation, ilu_InputOptional(ca->call, &present, ILU_NIL, &ca->err));
  if (ILU_ERRNOK(ca->err))
    return _ilupython_RaiseILUGeneralError(&ca->err);
  if (present)
    return PyInt_FromLong(present);
  Py_INCREF(Py_None);
  return Py_None;
}

/********************************/

#ifdef ILU_REFERENCE_TYPES

static PyObject *
  ilumod_SizeOfReference(PyObject *self, PyObject *args)
{
  IlucaObject * ca;
  ilu_cardinal  s;
  PyObject *	ref;
  ilu_boolean	first;
  PyObject *	ret;
  PyObject *	aliased;

  if (!PyArg_Parse(args, "(OOO)", &ca, &ref, &aliased))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  s = ilu_SizeOfReference(ca->call, ref != Py_None, &first,
			  (PyObject_IsTrue(aliased) && (ref != Py_None)) ? ref : 0, &ca->err); /* non-blocking */
  if (ILU_ERRNOK(ca->err))
    return _ilupython_RaiseILUGeneralError(&ca->err);
  ret = PyTuple_New(2);
  PyTuple_SetItem(ret, 0, PyInt_FromLong(s));
  PyTuple_SetItem(ret, 1, PyInt_FromLong((first && (ref != Py_None)) ? 1 : 0));
  return ret;
}

static PyObject *
  ilumod_OutputReference(PyObject *self, PyObject *args)
{
  IlucaObject * ca;
  PyObject *	ref;
  ilu_boolean	first;
  PyObject *	ret;
  PyObject *	aliased;

  if (!PyArg_Parse(args, "(OOO)", &ca, &ref, &aliased))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  CALL_KERNEL(ilupython_threaded_operation,
	      ilu_OutputReference(ca->call, ref != Py_None,
				  &first, (PyObject_IsTrue(aliased) && (ref != Py_None)) ? ref : 0,
				  &ca->err));
  if (ILU_ERRNOK(ca->err))
    return _ilupython_RaiseILUGeneralError(&ca->err);
  return PyInt_FromLong((first && (ref != Py_None)) ? 1 : 0);
}

static PyObject *
  ilumod_InputReference(PyObject *self, PyObject *args)
{
  IlucaObject * ca;
  ilu_ReferenceID ref;
  ilu_cardinal	wire_id;
  ilu_boolean	present;
  PyObject *	ret_id;
  PyObject *	ret;

  if (!PyArg_Parse(args, "O", &ca))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  CALL_KERNEL(ilupython_threaded_operation, wire_id = ilu_InputReference(ca->call, &present, &ref, &ca->err));
  if (ILU_ERRNOK(ca->err))
    return _ilupython_RaiseILUGeneralError(&ca->err);
  ret = PyTuple_New(2);
  PyTuple_SetItem(ret, 0, PyInt_FromLong(wire_id));
  if (!present || (ref == 0)) {
    Py_INCREF(Py_None);
    ret_id = Py_None;
  } else
    ret_id = ref;
  PyTuple_SetItem(ret, 1, ret_id);
  return ret;
}

static PyObject *
  ilumod_EndInputReference(PyObject *self, PyObject *args)
{
  IlucaObject * ca;
  PyObject *	ref;
  ilu_boolean	first;
  PyObject *	ret;
  ilu_cardinal	wire_id;

  if (!PyArg_Parse(args, "(OlO)", &ca, &wire_id, &ref))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  ilu_EndInputReference(ca->call, wire_id, ref, &ca->err);
  if (ILU_ERRNOK(ca->err))
    return _ilupython_RaiseILUGeneralError(&ca->err);
  Py_INCREF(Py_None);
  return Py_None;
}

#endif /* def ILU_REFERENCE_TYPES */

/********************************/

static PyObject *
  ilumod_SizeOfUnion(PyObject *self, PyObject *args)
{
  IlucaObject * ca;
  long      discrim, discrim_kind;
  ilu_cardinal  s;

  if (!PyArg_Parse(args, "(Oll)", &ca, &discrim, &discrim_kind))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  s = ilu_SizeOfUnion(ca->call, discrim, (ilu_TypeKind) discrim_kind, ILU_NIL, &ca->err); /* non-blocking */
  if (ILU_ERRNOK(ca->err))
    return _ilupython_RaiseILUGeneralError(&ca->err);
  return PyInt_FromLong(s);
}

static PyObject *
  ilumod_OutputUnion(PyObject *self, PyObject *args)
{
  IlucaObject * ca;
  long      discrim, discrim_kind;

  if (!PyArg_Parse(args, "(Oll)", &ca, &discrim, &discrim_kind))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  CALL_KERNEL(ilupython_threaded_operation, ilu_OutputUnion(ca->call, discrim,
							    (ilu_TypeKind) discrim_kind,
							    ILU_NIL, &ca->err));
  if (ILU_ERRNOK(ca->err))
    return _ilupython_RaiseILUGeneralError(&ca->err);
  Py_INCREF(Py_None);
  return Py_None;
}

static PyObject *
  ilumod_InputUnion(PyObject *self, PyObject *args)
{
  IlucaObject *     ca;
  ilu_cardinal      discrim;
  ilu_cardinal      discrim_kind;

  if (!PyArg_Parse(args, "(Ol)", &ca, &discrim_kind))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  CALL_KERNEL(ilupython_threaded_operation, ilu_InputUnion(ca->call, &discrim,
							   (ilu_TypeKind) discrim_kind, ILU_NIL, &ca->err));
  if (ILU_ERRNOK(ca->err))
    return _ilupython_RaiseILUGeneralError(&ca->err);
  return PyInt_FromLong(discrim);
}

static PyObject *
  ilumod_EndUnion(PyObject *self, PyObject *args)
{
  IlucaObject * ca;

  if (!PyArg_Parse(args, "O", &ca))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  CALL_KERNEL(ilupython_threaded_operation, ilu_EndUnion(ca->call, &ca->err));
  if (ILU_ERRNOK(ca->err))
    return _ilupython_RaiseILUGeneralError(&ca->err);
  Py_INCREF(Py_None);
  return Py_None;
}

/********************************/

#ifdef ADD_VARIANT_SUPPORT

static PyObject *
  ilumod_SizeOfPickleBytes(PyObject *self, PyObject *args)
{
  IlucaObject * ca;
  ilu_Pickle    pickle;
  ilu_cardinal  s;

  if (!PyArg_Parse(args, "(Os#)", &ca, &pickle.pi_bytes, &pickle.pi_len))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  s = ilu_SizeOfPickle(ca->call, pickle, ILU_NIL, &ca->err); /* non-blocking */
  if (ILU_ERRNOK(ca->err))
    return _ilupython_RaiseILUGeneralError(&ca->err);
  return PyInt_FromLong(s);
}

static PyObject *
  ilumod_OutputPickleBytes(PyObject *self, PyObject *args)
{
  IlucaObject * ca;
  ilu_Pickle    pickle;

  if (!PyArg_Parse(args, "(Os#)", &ca, &pickle.pi_bytes, &pickle.pi_len))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  CALL_KERNEL(ilupython_threaded_operation,
	      ilu_OutputPickle(ca->call, pickle, ILU_NIL, &ca->err));
  if (ILU_ERRNOK(ca->err))
    return _ilupython_RaiseILUGeneralError(&ca->err);
  Py_INCREF(Py_None);
  return Py_None;
}

static PyObject *
  ilumod_InputPickleBytes(PyObject *self, PyObject *args)
{
  IlucaObject *     ca;
  ilu_Pickle        pickle;

  if (!PyArg_Parse(args, "O", &ca))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  CALL_KERNEL(ilupython_threaded_operation,
	      ilu_InputPickle(ca->call, &pickle, ILU_NIL, &ca->err));
  if (ILU_ERRNOK(ca->err)) {
    return _ilupython_RaiseILUGeneralError(&ca->err);
  }
  return (PyString_FromStringAndSize((char *) pickle.pi_bytes, pickle.pi_len));
}

static PyObject *
  ilumod_StartPickle(PyObject *self, PyObject *args)
{
  ilu_Call  call;
  ilu_Error err;

  if ((call = (ilu_Call) ilu_malloc(sizeof(*call))) == ILU_NIL)
    {
      PyErr_NoMemory();
      return 0;
    }
  ilu_StartPickle (call, ILU_NIL, &err);
  if (ILU_ERRNOK(err))
    {
      ilu_free(call);
      return _ilupython_RaiseILUGeneralError(&err);
    }
  return iluca_FromCall(call);
}

static PyObject *
  ilumod_WritePickle(PyObject *self, PyObject *args)
{
  IlucaObject * ca;
  ilu_cardinal  argSize;
  ilu_string    type_id;

  if (!PyArg_Parse(args, "(Ols)", &ca, &argSize, &type_id))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  ilu_WritePickle(ca->call, argSize, type_id, &ca->err);
  if (ILU_ERRNOK(ca->err))
    return _ilupython_RaiseILUGeneralError(&ca->err);
  Py_INCREF(Py_None);
  return Py_None;
}

static PyObject *
  ilumod_PickleTypeID(PyObject *self, PyObject *args)
{
  ilu_string    type_id = ILU_NIL;
  ilu_Pickle    pickle;
  ilu_Error lerr;

  if (!PyArg_Parse(args, "s#", &pickle.pi_bytes, &pickle.pi_len))
    return 0;
  type_id = ilu_PickleType (pickle, &lerr);
  if (ILU_ERRNOK(lerr))
    return _ilupython_RaiseILUGeneralError(&lerr);
  return (PyString_FromString(type_id));
}

static PyObject *
  ilumod_PickleTypeIDs(PyObject *self, PyObject *args)
{
  PyObject *    retval;
  PyObject *    temp;
  ilu_Pickle    pickle;
  ilu_Error lerr;
  ilu_string *  types;
  ilu_cardinal  types_count, i;

  if (!PyArg_Parse(args, "s#", &pickle.pi_bytes, &pickle.pi_len))
    return 0;
  if (!ilu_PickleTypes (pickle, &types, &types_count, &lerr))
    return _ilupython_RaiseILUGeneralError(&lerr);
  if ((retval = PyTuple_New(types_count)) == 0)
    return 0;
  for (i = 0;  i < types_count;  i++) {
    if ((temp = PyString_FromString(types[i])) == 0)
      return 0;
    ilu_free(types[i]);
    PyTuple_SetItem(retval, i, temp);
  }
  return (retval);
}

static PyObject *
  ilumod_ReadPickle(PyObject *self, PyObject *args)
{
  IlucaObject * ca;
  PyObject *    type_ids;
  PyObject *    more_args;
  ilu_TypeKind  tk;
  ilu_boolean   good_typekind = ilu_FALSE;
  ilu_Pickle    pi;
  ilu_Error lerr;

  if (!PyArg_Parse(args, "(Os#)", &ca, &pi.pi_bytes, &pi.pi_len))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  tk = ilu_PickleTypeKind(pi, &lerr);
  ILU_ERR_SWITCH(lerr) {
    ILU_SUCCESS_CASE {
      good_typekind = ilu_TRUE;
    }
    ILU_ERR_CASE(bad_typecode, e) {
      good_typekind = ilu_FALSE;
      ILU_HANDLED(lerr);
    }
    ILU_ERR_ELSE {
      return _ilupython_RaiseILUGeneralError(&ca->err);
    }
  } ILU_ERR_ENDSWITCH;
  ilu_ReadPickle(ca->call, pi, &ca->err);
  if (ILU_ERRNOK(ca->err))
    {
      return _ilupython_RaiseILUGeneralError(&ca->err);
    }
  if ((more_args = Py_BuildValue("s#", pi.pi_bytes, pi.pi_len)) == 0)
    return 0;
  if (good_typekind && ((tk == ilu_record_tk) || (tk == ilu_object_tk))) {
    type_ids = ilumod_PickleTypeIDs(self, more_args);
    Py_DECREF(more_args);
    return type_ids;
  } else {
    PyObject *type_id;
    type_id = ilumod_PickleTypeID (self, more_args);
    Py_DECREF(more_args);
    if (type_id == 0)
      return 0;
    if ((type_ids = PyTuple_New(1)) == 0)
      return 0;
    PyTuple_SetItem(type_ids, 0, type_id);
    return type_ids;
  }
}

static PyObject *
  ilumod_EndPickle(PyObject *self, PyObject *args)
{
  IlucaObject * ca;
  ilu_boolean   incoming;
  ilu_Pickle    pickle;

  if (!PyArg_Parse(args, "O", &ca))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  incoming = ((ca->call)->ca_incoming) ? ilu_TRUE : ilu_FALSE;
  ilu_EndPickle(ca->call, &pickle, &ca->err);
  ilu_free(ca->call);
  if (ILU_ERRNOK(ca->err))
    {
      return _ilupython_RaiseILUGeneralError(&ca->err);
    }
  return (PyString_FromStringAndSize((char *) pickle.pi_bytes, pickle.pi_len));
}

#endif /* ADD_VARIANT_SUPPORT */

/********************************/

static PyObject *
  ilumod_SizeOfArray(PyObject *self, PyObject *args)
{
  IlucaObject * ca;
  long      length;
  ilu_cardinal  s;

  if (!PyArg_Parse(args, "(Ol)", &ca, &length))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  s = ilu_SizeOfArray(ca->call, length, ILU_NIL, &ca->err); /* non-blocking */
  if (ILU_ERRNOK(ca->err))
    return _ilupython_RaiseILUGeneralError(&ca->err);
  return PyInt_FromLong(s);
}

static PyObject *
  ilumod_OutputArray(PyObject *self, PyObject *args)
{
  IlucaObject * ca;
  long      length;

  if (!PyArg_Parse(args, "(Ol)", &ca, &length))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  CALL_KERNEL(ilupython_threaded_operation, ilu_OutputArray(ca->call, length, ILU_NIL, &ca->err));
  if (ILU_ERRNOK(ca->err))
    return _ilupython_RaiseILUGeneralError(&ca->err);
  Py_INCREF(Py_None);
  return Py_None;
}

static PyObject *
  ilumod_InputArray(PyObject *self, PyObject *args)
{
  IlucaObject * ca;

  if (!PyArg_Parse(args, "O", &ca))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  CALL_KERNEL(ilupython_threaded_operation, ilu_InputArray(ca->call, ILU_NIL, &ca->err));
  if (ILU_ERRNOK(ca->err))
    return _ilupython_RaiseILUGeneralError(&ca->err);
  Py_INCREF(Py_None);
  return Py_None;
}

static PyObject *
  ilumod_EndArray(PyObject *self, PyObject *args)
{
  IlucaObject * ca;

  if (!PyArg_Parse(args, "O", &ca))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  CALL_KERNEL(ilupython_threaded_operation, ilu_EndArray(ca->call, &ca->err));
  if (ILU_ERRNOK(ca->err))
    return _ilupython_RaiseILUGeneralError(&ca->err);
  Py_INCREF(Py_None);
  return Py_None;
}

/********************************/

static PyObject *
  ilumod_SizeOfRecord(PyObject *self, PyObject *args)
{
  IlucaObject * ca;
  ilu_cardinal  s;

  if (!PyArg_Parse(args, "O", &ca))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  s = ilu_SizeOfRecord(ca->call, ILU_NIL, &ca->err); /* non-blocking */
  if (ILU_ERRNOK(ca->err))
    return _ilupython_RaiseILUGeneralError(&ca->err);
  return PyInt_FromLong(s);
}

static PyObject *
  ilumod_OutputRecord(PyObject *self, PyObject *args)
{
  IlucaObject * ca;

  if (!PyArg_Parse(args, "O", &ca))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }

  CALL_KERNEL(ilupython_threaded_operation, ilu_OutputRecord(ca->call, ILU_NIL, &ca->err));

  if (ILU_ERRNOK(ca->err))
    return _ilupython_RaiseILUGeneralError(&ca->err);
  Py_INCREF(Py_None);
  return Py_None;
}

static PyObject *
  ilumod_InputRecord(PyObject *self, PyObject *args)
{
  IlucaObject * ca;

  if (!PyArg_Parse(args, "O", &ca))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }

  CALL_KERNEL(ilupython_threaded_operation, ilu_InputRecord(ca->call, ILU_NIL, &ca->err));

  if (ILU_ERRNOK(ca->err))
    return _ilupython_RaiseILUGeneralError(&ca->err);
  Py_INCREF(Py_None);
  return Py_None;
}

static PyObject *
  ilumod_EndRecord(PyObject *self, PyObject *args)
{
  IlucaObject * ca;

  if (!PyArg_Parse(args, "O", &ca))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }

  CALL_KERNEL(ilupython_threaded_operation, ilu_EndRecord(ca->call, &ca->err));

  if (ILU_ERRNOK(ca->err))
    return _ilupython_RaiseILUGeneralError(&ca->err);
  Py_INCREF(Py_None);
  return Py_None;
}

/********************************/

static PyObject *
  ilumod_SizeOfSequence(PyObject *self, PyObject *args)
{
  IlucaObject * ca;
  long      len;
  long      limit;
  ilu_cardinal  s;

  if (!PyArg_Parse(args, "(Oll)", &ca, &len, &limit))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  s = ilu_SizeOfSequence(ca->call, len, limit, ILU_NIL, &ca->err); /* non-blocking */

  if (ILU_ERRNOK(ca->err))
    return _ilupython_RaiseILUGeneralError(&ca->err);
  return PyInt_FromLong(s);
}

static PyObject *
  ilumod_OutputSequence(PyObject *self, PyObject *args)
{
  IlucaObject * ca;
  long      len;
  long      limit;

  if (!PyArg_Parse(args, "(Oll)", &ca, &len, &limit))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }

  CALL_KERNEL(ilupython_threaded_operation, ilu_OutputSequence(ca->call, len, limit, ILU_NIL, &ca->err));

  if (ILU_ERRNOK(ca->err))
    return _ilupython_RaiseILUGeneralError(&ca->err);
  Py_INCREF(Py_None);
  return Py_None;
}

static PyObject *
  ilumod_InputSequence(PyObject *self, PyObject *args)
{
  IlucaObject * ca;
  ilu_cardinal  len;
  long      limit;

  if (!PyArg_Parse(args, "(Ol)", &ca, &limit))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }

  CALL_KERNEL(ilupython_threaded_operation, ilu_InputSequence(ca->call, &len, limit, ILU_NIL, &ca->err));

  if (ILU_ERRNOK(ca->err))
    return _ilupython_RaiseILUGeneralError(&ca->err);
  return PyInt_FromLong((long) len);
}

static PyObject *
  ilumod_EndSequence(PyObject *self, PyObject *args)
{
  IlucaObject * ca;

  if (!PyArg_Parse(args, "O", &ca))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }

  CALL_KERNEL(ilupython_threaded_operation, ilu_EndSequence(ca->call, &ca->err));

  if (ILU_ERRNOK(ca->err))
    return _ilupython_RaiseILUGeneralError(&ca->err);
  Py_INCREF(Py_None);
  return Py_None;
}

/************* end marshalling/unmarshalling routines *************/

static PyObject *
  ilumod_TCPStatistics (PyObject *self, PyObject *args)
{
#ifdef TCPIP_TRANSPORT
#ifdef ENABLE_DEBUGGING
  ilu_cardinal BytesSent;
  ilu_cardinal BytesReceived;
  ilu_cardinal MooringsCreated;
  ilu_cardinal ConnectionsReceived;
  ilu_cardinal ConnectionsCreated;
  ilu_cardinal CurrentConnections = 0;
  ilu_cardinal MaxSimultaneousConnections;
  PyObject *reset = Py_None;

  if (args != ILU_NIL) {
    if (!PyArg_Parse(args, "O", &reset))
      return 0;
  }
  ilu_tcp_GetStats (&BytesSent,
		    &BytesReceived,
		    &MooringsCreated,
		    &ConnectionsReceived,
		    &ConnectionsCreated,
		    &CurrentConnections,
		    &MaxSimultaneousConnections);
  if (PyObject_IsTrue(reset))
    ilu_tcp_InitializeStats();
  reset = PyDict_New();
  PyDict_SetItemString(reset, "BytesSent", PyInt_FromLong(BytesSent));
  PyDict_SetItemString(reset, "BytesReceived", PyInt_FromLong(BytesReceived));
  PyDict_SetItemString(reset, "MooringsCreated", PyInt_FromLong(MooringsCreated));
  PyDict_SetItemString(reset, "ConnectionsReceived", PyInt_FromLong(ConnectionsReceived));
  PyDict_SetItemString(reset, "ConnectionsCreated", PyInt_FromLong(ConnectionsCreated));
  PyDict_SetItemString(reset, "CurrentConnections", PyInt_FromLong(CurrentConnections));
  PyDict_SetItemString(reset, "MaxSimultaneousConnections", PyInt_FromLong(MaxSimultaneousConnections));
  return reset;
#else /* def ENABLE_DEBUGGING */
  PyErr_SetString(_ilupython_GeneralError, "Compiled without support for debugging");
  return 0;
#endif /* def ENABLE_DEBUGGING */
#else /* def TCPIP_TRANSPORT */
  PyErr_SetString(_ilupython_GeneralError, "TCP/IP support not configured in");
  return 0;  
#endif /* TCPIP_TRANSPORT */
}

static PyObject *
  ilumod_TCPDefaultBufferSize (PyObject *self, PyObject *args)
{
#ifdef TCPIP_TRANSPORT
  long int size, oldsize;

  if (!PyArg_ParseTuple(args, "l", &size))
    return 0;
  oldsize = ilu_tcp_SetDefaultBuffersize(size);
  return PyInt_FromLong(oldsize);
#else
  PyErr_SetString(_ilupython_GeneralError, "TCP/IP support not configured in");
  return 0;  
#endif /* TCPIP_TRANSPORT */
}

#ifdef ADD_TYPE_REGISTRATION_SUPPORT

/******************************************************************/
/************* type registration routines *************************/
/******************************************************************/

static PyObject *
  ilumod_RegisterSequenceType (PyObject *self, PyObject *args)
{
  ilu_string    name;
  ilu_string    interface_name;
  ilu_string    interface_brand;
  ilu_string    uid;
  ilu_Type  type;
  ilu_Error err;
  ilu_boolean   is_new;
  PyObject  *names;

  ilu_string    base_type_uid;
  ilu_cardinal  limit;

  if (!PyArg_Parse(args, "(Ozssl)", &names, &interface_brand, &uid, &base_type_uid, &limit))
    return 0;
  if (!CheckNames(names)) {
    PyErr_SetString(PyExc_TypeError, strArg1ShouldBeNamesTuple);
    return 0;
  };
  ilu_AcquireMutex(ilu_otmu);
  FormNames (names, &name, &interface_name);
  type = ilu_RegisterSequenceType (name, interface_name, interface_brand, uid, base_type_uid, limit, &is_new, &err);
  ReleaseNames (name, interface_name);
  ilu_ReleaseMutex(ilu_otmu);
  if (ILU_ERRNOK(err)) {
    char errbuf[1000];

    PyErr_SetString(_ilupython_GeneralError, _ilupython_formErrDescription(errbuf, &err));
    return 0;
  } else
    return ilutp_FromType(type);
}

static PyObject *
  ilumod_RegisterOptionalType (PyObject *self, PyObject *args)
{
  ilu_string    name;
  ilu_string    interface_name;
  ilu_string    interface_brand;
  ilu_string    uid;
  ilu_Type  type;
  ilu_Error err;
  ilu_boolean   is_new;
  PyObject  *names;

  ilu_string    base_type_uid;

  if (!PyArg_Parse(args, "(Ozss)", &names, &interface_brand, &uid, &base_type_uid))
    return 0;
  if (!CheckNames(names)) {
    PyErr_SetString(PyExc_TypeError, strArg1ShouldBeNamesTuple);
    return 0;
  };
  ilu_AcquireMutex(ilu_otmu);
  FormNames (names, &name, &interface_name);
  type = ilu_RegisterOptionalType (name, interface_name, interface_brand, uid, base_type_uid, &is_new, &err);
  ReleaseNames (name, interface_name);
  ilu_ReleaseMutex(ilu_otmu);
  if (ILU_ERRNOK(err)) {
    return _ilupython_RaiseILUGeneralError(&err);
  } else
    return ilutp_FromType(type);
}

#ifdef ILU_REFERENCE_TYPES

static PyObject *
  ilumod_RegisterReferenceType (PyObject *self, PyObject *args)
{
  ilu_string    name;
  ilu_string    interface_name;
  ilu_string    interface_brand;
  ilu_string    uid;
  ilu_Type  type;
  ilu_Error err;
  ilu_boolean   is_new;
  PyObject  *names;
  int optional, aliased;

  ilu_string    base_type_uid;

  if (!PyArg_Parse(args, "(Ozssii)", &names, &interface_brand, &uid, &base_type_uid, &optional, &aliased))
    return 0;
  if (!CheckNames(names)) {
    PyErr_SetString(PyExc_TypeError, strArg1ShouldBeNamesTuple);
    return 0;
  };
  ilu_AcquireMutex(ilu_otmu);
  FormNames (names, &name, &interface_name);
  type = ilu_RegisterReferenceType (name, interface_name, interface_brand,
				    uid, base_type_uid, (optional ? ilu_TRUE : ilu_FALSE),
				    (aliased ? ilu_TRUE : ilu_FALSE), &is_new, &err);
  ReleaseNames (name, interface_name);
  ilu_ReleaseMutex(ilu_otmu);
  if (ILU_ERRNOK(err)) {
    return _ilupython_RaiseILUGeneralError(&err);
  } else
    return ilutp_FromType(type);
}

#endif /* def ILU_REFERENCE_TYPES */

static PyObject *
  ilumod_RegisterAliasType (PyObject *self, PyObject *args)
{
  ilu_string    name;
  ilu_string    interface_name;
  ilu_string    interface_brand;
  ilu_string    uid;
  ilu_Type  type;
  ilu_Error err;
  ilu_boolean   is_new;
  PyObject *    names;
  ilu_string    base_type_uid;

  if (!PyArg_Parse(args, "(Ozss)", &names, &interface_brand, &uid, &base_type_uid))
    return 0;
  if (!CheckNames(names)) {
    PyErr_SetString(PyExc_TypeError, strArg1ShouldBeNamesTuple);
    return 0;
  };
  ilu_AcquireMutex(ilu_otmu);
  FormNames (names, &name, &interface_name);
  type = ilu_RegisterAliasType (name, interface_name, interface_brand, uid, base_type_uid, &is_new, &err);
  ReleaseNames (name, interface_name);
  ilu_ReleaseMutex(ilu_otmu);
  if (ILU_ERRNOK(err)) {
    return _ilupython_RaiseILUGeneralError(&err);
  } else
    return ilutp_FromType(type);
}

static PyObject *
  ilumod_RegisterArrayType (PyObject *self, PyObject *args)
{
  ilu_string    name;
  ilu_string    interface_name;
  ilu_string    interface_brand;
  ilu_string    uid;
  ilu_Type  type;
  ilu_Error err;
  ilu_boolean   is_new;
  PyObject *    names;
  ilu_string    base_type_uid;
  PyObject *    dims;
  ilu_cardinal *c_dims;
  ilu_cardinal  n_dims;
  int       i;

  if (!PyArg_Parse(args, "(OzssO!)", &names, &interface_brand, &uid, &base_type_uid, &PyTuple_Type, &dims))
    return 0;
  if ((n_dims = PyTuple_Size(dims)) < 1) {
    PyErr_SetString(PyExc_TypeError, "Arrays must have one or more dimensions specified.");
    return 0;
  };
  if (!CheckNames(names)) {
    PyErr_SetString(PyExc_TypeError, strArg1ShouldBeNamesTuple);
    return 0;
  };
  if ((c_dims = (ilu_cardinal *) ilu_MallocE(n_dims * sizeof(ilu_cardinal), &err)) == ILU_NIL)
    {
      ILU_HANDLED(err);
      PyErr_SetString(PyExc_MemoryError, "Can't allocate space for vector of array dimensions");
      return 0;
    }
  for (i = 0;  i < n_dims;  i++) {
    c_dims[i] = PyInt_AsLong(PyTuple_GetItem(dims, i));
  };
  ilu_AcquireMutex(ilu_otmu);
  FormNames (names, &name, &interface_name);
  type = ilu_RegisterArrayType (name, interface_name, interface_brand, uid, base_type_uid, n_dims, c_dims, &is_new, &err);
  ReleaseNames (name, interface_name);
  ilu_ReleaseMutex(ilu_otmu);
  ilu_free(c_dims);
  if (ILU_ERRNOK(err)) {
    return _ilupython_RaiseILUGeneralError(&err);
  } else
    return ilutp_FromType(type);
}

static PyObject *
  ilumod_RegisterRecordType (PyObject *self, PyObject *args)
{
  ilu_string    name;
  ilu_string    interface_name;
  ilu_string    interface_brand;
  ilu_string    uid;
  ilu_Type  type;
  ilu_Error err;
  ilu_boolean   is_new;
  PyObject *    names;
  PyObject *    fields;
  ilu_cardinal  n_fields;
  PyObject *    field;
  int       i;
  long      extensible = 0;
  ilu_string    supertype_uid = ILU_NIL;

  if (!PyArg_ParseTuple(args, "OzsO!|lz", &names, &interface_brand, &uid, &PyTuple_Type, &fields, &extensible, &supertype_uid))
    return 0;
  if ((n_fields = PyTuple_Size(fields)) < 1) {
    PyErr_SetString(PyExc_TypeError, "Records must have one or more fields.");
    return 0;
  };
  if (!CheckNames(names)) {
    PyErr_SetString(PyExc_TypeError, strArg1ShouldBeNamesTuple);
    return 0;
  };
  for (i = 0;  i < n_fields;  i++) {
    field = PyTuple_GetItem (fields, i);
    if (!PyTuple_Check(field) ||
	!PyString_Check(PyTuple_GetItem(field, 0)) ||
	!PyString_Check(PyTuple_GetItem(field, 1))) {
      PyErr_SetString(PyExc_TypeError, "Each record field must be a 2-ple of field name and field type UID");
      return 0;
    };
  };
  ilu_AcquireMutex(ilu_otmu);
  FormNames (names, &name, &interface_name);
  type = ilu_RegisterRecordType (name, interface_name, interface_brand, uid, n_fields,
				 (ilu_boolean)extensible, supertype_uid, &is_new, &err);
  ReleaseNames (name, interface_name);
  if (ILU_ERRNOK(err)) {
    ilu_ReleaseMutex(ilu_otmu);
    return _ilupython_RaiseILUGeneralError(&err);
  };
  if (is_new) {
    for (i = 0;  i < n_fields;  i++) {
      field = PyTuple_GetItem (fields, i);
      if (!ilu_RegisterRecordField(type, i, PyString_AsString(PyTuple_GetItem(field, 0)),
				   PyString_AsString(PyTuple_GetItem(field, 1)), &err)) {
	return _ilupython_RaiseILUGeneralError(&err);
      };
    }
  }
  ilu_ReleaseMutex(ilu_otmu);
  return ilutp_FromType(type);
}

static PyObject *
  ilumod_RegisterEnumerationType (PyObject *self, PyObject *args)
{
  ilu_string    name;
  ilu_string    interface_name;
  ilu_string    interface_brand;
  ilu_string    uid;
  ilu_Type  type;
  ilu_Error err;
  ilu_boolean   is_new;
  PyObject *    names;
  PyObject *    elements;
  PyObject *    element;
  ilu_cardinal  n_elements;
  int       i;

  if (!PyArg_Parse(args, "(OzsO!)", &names, &interface_brand, &uid, &PyTuple_Type, &elements))
    return 0;
  if ((n_elements = PyTuple_Size(elements)) < 1) {
    PyErr_SetString(PyExc_TypeError, "Records must have one or more elements.");
    return 0;
  };
  if (!CheckNames(names)) {
    PyErr_SetString(PyExc_TypeError, strArg1ShouldBeNamesTuple);
    return 0;
  };
  for (i = 0;  i < n_elements;  i++) {
    element = PyTuple_GetItem (elements, i);
    if (!PyTuple_Check(element) ||
	!PyString_Check(PyTuple_GetItem(element, 0)) ||
	!PyInt_Check(PyTuple_GetItem(element, 1))) {
      PyErr_SetString(PyExc_TypeError, "Each enumeration element must be a 2-ple of element name and element id");
      return 0;
    };
  };
  ilu_AcquireMutex(ilu_otmu);
  FormNames (names, &name, &interface_name);
  type = ilu_RegisterEnumerationType (name, interface_name, interface_brand, uid, n_elements, &is_new, &err);
  ReleaseNames (name, interface_name);
  if (ILU_ERRNOK(err)) {
    ilu_ReleaseMutex(ilu_otmu);
    return _ilupython_RaiseILUGeneralError(&err);
  };
  if (is_new) {
    for (i = 0;  i < n_elements;  i++) {
      element = PyTuple_GetItem (elements, i);
      if (!ilu_RegisterEnumerationElement(type, (ilu_cardinal) i,
					  PyString_AsString(PyTuple_GetItem(element, 0)),
					  (ilu_cardinal) PyInt_AsLong(PyTuple_GetItem(element, 1)),
					  &err)) {
        return _ilupython_RaiseILUGeneralError(&err);
      };
    }
  }
  ilu_ReleaseMutex(ilu_otmu);
  return ilutp_FromType(type);
}

#define INTEGER_DISCRIMINANT_TYPE_KIND(v)   (((v)==ilu_integer_tk)||((v)==ilu_shortinteger_tk))
#define CARDINAL_DISCRIMINANT_TYPE_KIND(v)  (((v)==ilu_cardinal_tk)||((v)==ilu_shortcardinal_tk)||((v)==ilu_byte_tk))
#define ENUMERATION_DISCRIMINANT_TYPE_KIND(v)   ((v)==ilu_enumeration_tk)

  static PyObject *
  ilumod_RegisterUnionType (PyObject *self, PyObject *args)
{
  ilu_string    name;
  ilu_string    interface_name;
  ilu_string    interface_brand;
  ilu_string    uid;
  ilu_Type  type;
  ilu_Error err;
  ilu_boolean   is_new;
  PyObject *    names;
  ilu_string    disc_type;
  ilu_cardinal  c_disc_type_kind1;
  ilu_TypeKind  c_disc_type_kind;
  PyObject *    others_allowed;
  ilu_cardinal  default_arm;
  PyObject *    arms;
  PyObject *    arm;
  PyObject *    arm_name;
  PyObject *    values;
  PyObject *    value;
  ilu_string    c_arm_name;
  ilu_UnionArm  c_arm;
  ilu_cardinal  n_arms;
  int       i, j;
  ilu_boolean   others_allwd;
  ilu_ConstantValue_s   cv;

  if (!PyArg_Parse(args, "(OzssllOO!)", &names, &interface_brand, &uid, &disc_type, &c_disc_type_kind1, &default_arm, &others_allowed, &PyTuple_Type, &arms))
    return 0;
  c_disc_type_kind = (ilu_TypeKind) c_disc_type_kind1;
  others_allwd = PyObject_IsTrue(others_allowed) ? ilu_TRUE : ilu_FALSE;
  if (!CheckNames(names)) {
    PyErr_SetString(PyExc_TypeError, strArg1ShouldBeNamesTuple);
    return 0;
  };
  if ((n_arms = PyTuple_Size(arms)) < 1) {
    PyErr_SetString(PyExc_TypeError, "Unions must have one or more arms.");
    return 0;
  };
  for (i = 0;  i < n_arms;  i++) {
    arm = PyTuple_GetItem (arms, i);
    if (!PyTuple_Check(arm)) {
      PyErr_SetString(PyExc_TypeError, "Each union arm must be a 3-tuple of name (or None), type, and selector values");
      return 0;
    } else {
      arm_name = PyTuple_GetItem(arm, 0);
      values = PyTuple_GetItem(arm, 2);
      if (!((arm_name == Py_None) || PyString_Check(arm_name)) ||
	  !PyString_Check(PyTuple_GetItem(arm, 1)) ||
	  !PyTuple_Check(values)) {
	ilu_DebugPrintf("Bad arm of union %s:  %s\n", PyString_AsString(PyObject_Str(names)),
			PyString_AsString(PyObject_Str(arm)));
	ilu_DebugPrintf("Union description is %s\n",
			PyString_AsString(PyObject_Str(args)));
	PyErr_SetString(PyExc_TypeError, "Each union arm must be a 3-tuple of name (or None), type, and selector values");
	return 0;
      } else {
	for (j = 0;  j < PyTuple_Size(values);  j++) {
	  value = PyTuple_GetItem(values, j);
	  if ((INTEGER_DISCRIMINANT_TYPE_KIND(c_disc_type_kind) && (!PyInt_Check(value))) ||
	      (CARDINAL_DISCRIMINANT_TYPE_KIND(c_disc_type_kind) &&
	       ((!PyInt_Check(value)) || (PyInt_AsLong(value) < 0))) ||
	      (ENUMERATION_DISCRIMINANT_TYPE_KIND(c_disc_type_kind) &&
	       (!PyString_Check(value)))) {
	    ilu_DebugPrintf("Bad value %s in arm %s (%d,%d,%d) for union type %s.\n",
			    PyString_AsString(PyObject_Str(value)),
			    PyString_AsString(PyObject_Str(arm)),
			    (INTEGER_DISCRIMINANT_TYPE_KIND(c_disc_type_kind) && (!PyInt_Check(value))),
			    (CARDINAL_DISCRIMINANT_TYPE_KIND(c_disc_type_kind) &&
			     ((!PyInt_Check(value)) || (PyInt_AsLong(value) < 0))),
			    (ENUMERATION_DISCRIMINANT_TYPE_KIND(c_disc_type_kind) &&
			     (!PyString_Check(value))), PyString_AsString(PyObject_Str(names)));
	    PyErr_SetString(PyExc_TypeError, "Each union arm selector value must be of the discriminant type");
	    return 0;
	  };
	}
      };
    }
  }
  ilu_AcquireMutex(ilu_otmu);
  FormNames (names, &name, &interface_name);
  type = ilu_RegisterUnionType (name, interface_name, interface_brand, uid, disc_type, n_arms, default_arm, others_allwd, &is_new, &err);
  ReleaseNames (name, interface_name);
  if (ILU_ERRNOK(err)) {
    ilu_ReleaseMutex(ilu_otmu);
    return _ilupython_RaiseILUGeneralError(&err);
  };
  if (is_new) {
    for (i = 0;  i < n_arms;  i++) {
      arm = PyTuple_GetItem (arms, i);
      arm_name = PyTuple_GetItem (arm, 0);
      values = PyTuple_GetItem (arm, 2);
      c_arm_name = (!PyObject_IsTrue(arm_name)) ? ILU_NIL : PyString_AsString(arm_name);
      if ((c_arm = ilu_RegisterUnionArm(type, (ilu_cardinal) i, c_arm_name,
					PyString_AsString(PyTuple_GetItem(arm, 1)),
					PyTuple_Size(values), &err)) == ILU_NIL) {
	ilu_ReleaseMutex(ilu_otmu);
	return _ilupython_RaiseILUGeneralError(&err);
      } else {
	for (j = 0;  j < PyTuple_Size(values);  j++) {
	  switch (c_disc_type_kind) {
	  case ilu_boolean_tk:
	    cv.kind = ilu_boolean_cvk;
	    cv.value.boolean_val = PyObject_IsTrue(PyTuple_GetItem(values, j)) ? ilu_TRUE : ilu_FALSE;
	    break;
	  case ilu_byte_tk:
	    cv.kind = ilu_byte_cvk;
	    cv.value.byte_val = (ilu_byte) PyInt_AsLong(PyTuple_GetItem(values, j));
	    break;
	  case ilu_integer_tk:
	    cv.kind = ilu_integer_cvk;
	    cv.value.integer_val = (ilu_integer) PyInt_AsLong(PyTuple_GetItem(values, j));
	    break;
	  case ilu_shortinteger_tk:
	    cv.kind = ilu_shortinteger_cvk;
	    cv.value.shortinteger_val = (ilu_shortinteger) PyInt_AsLong(PyTuple_GetItem(values, j));
	    break;
	  case ilu_cardinal_tk:
	    cv.kind = ilu_cardinal_cvk;
	    cv.value.cardinal_val = (ilu_cardinal) PyInt_AsLong(PyTuple_GetItem(values, j));
	    break;
	  case ilu_shortcardinal_tk:
	    cv.kind = ilu_shortcardinal_cvk;
	    cv.value.shortcardinal_val = (ilu_shortcardinal) PyInt_AsLong(PyTuple_GetItem(values, j));
	    break;
	  case ilu_enumeration_tk:
	    cv.kind = ilu_enumeration_cvk;
	    cv.value.enumeration_val = (ilu_string) PyString_AsString(PyTuple_GetItem(values, j));
	    break;
	  default:
	    ilu_ReleaseMutex(ilu_otmu);
	    PyErr_SetString(PyExc_TypeError, "Bad kernel type-kind indicated for union discriminant");
	    return 0;
	  }
	  ilu_RegisterUnionArmValue(c_arm, j, &cv, &err);
	  if (ILU_ERRNOK(err)) {
	    ilu_ReleaseMutex(ilu_otmu);
	    return _ilupython_RaiseILUGeneralError(&err);
	  }
	}
      }
    }
  }
  ilu_ReleaseMutex(ilu_otmu);
  return ilutp_FromType(type);
}

static PyObject *
  ilumod_FindTypeByUID (PyObject *self, PyObject *args)
{
  ilu_string    uid;
  ilu_Type  type;
  ilu_Error err;

  if (!PyArg_Parse(args, "s", &uid))
    return 0;
  type = ilu_FindTypeByUID(uid, &err);
  if (ILU_ERRNOK(err)) {
    return _ilupython_RaiseILUGeneralError(&err);
  } else if (type == ILU_NIL) {
    Py_INCREF(Py_None);
    return Py_None;
  } else {
    return ilutp_FromType(type);
  }
}

static PyObject *
  ilumod_FindTypeIDByName (PyObject *self, PyObject *args)
{
  ilu_string    type_name;
  ilu_Type  type;
  ilu_Error err;

  if (!PyArg_Parse(args, "s", &type_name))
    return 0;
  type = ilu_FindTypeByName(type_name, &err);
  if (ILU_ERRNOK(err)) {
    return _ilupython_RaiseILUGeneralError(&err);
  } else if (type == ILU_NIL) {
    Py_INCREF(Py_None);
    return Py_None;
  } else {
    return PyString_FromString(type_uid(type));
  }
}

#else   /* no Type registration support */

#ifdef ADD_VARIANT_SUPPORT

static PyObject *
  ilumod_FindTypeIDByName (PyObject *self, PyObject *args)
{
  ilu_string    full_name;
  ilu_string    type_name;
  char *    p;
  ilu_Error err;

  if (!PyArg_Parse(args, "s", &full_name))
    return 0;
  p = strchr(full_name, '.');
  if (p == ILU_NIL) {
    PyErr_SetString(_ilupython_GeneralError, "Bad type name passed to ilu.FindTypeIDByName");
    return 0;
  };
  type_name = p + 1;
  if (strncmp(full_name, "ilu", (p - full_name)) == 0) {
    if (strcmp(type_name, "byte") == 0) {
      return PyString_FromString((char *) &ilu_TypeID_ilu_byte[0]);
    } else if (strcmp(type_name, "boolean") == 0) {
      return PyString_FromString((char *) &ilu_TypeID_ilu_boolean[0]);
    } else if (strcmp(type_name, "pickle") == 0) {
      return PyString_FromString((char *) &ilu_TypeID_ilu_pickle[0]);
    } else if (strcmp(type_name, "shortinteger") == 0) {
      return PyString_FromString((char *) &ilu_TypeID_ilu_shortinteger[0]);
    } else if (strcmp(type_name, "integer") == 0) {
      return PyString_FromString((char *) &ilu_TypeID_ilu_integer[0]);
    } else if (strcmp(type_name, "longinteger") == 0) {
      return PyString_FromString((char *) &ilu_TypeID_ilu_longinteger[0]);
    } else if (strcmp(type_name, "shortcardinal") == 0) {
      return PyString_FromString((char *) &ilu_TypeID_ilu_shortcardinal[0]);
    } else if (strcmp(type_name, "cardinal") == 0) {
      return PyString_FromString((char *) &ilu_TypeID_ilu_cardinal[0]);
    } else if (strcmp(type_name, "longcardinal") == 0) {
      return PyString_FromString((char *) &ilu_TypeID_ilu_longcardinal[0]);
    } else if (strcmp(type_name, "shortreal") == 0) {
      return PyString_FromString((char *) &ilu_TypeID_ilu_shortreal[0]);
    } else if (strcmp(type_name, "real") == 0) {
      return PyString_FromString((char *) &ilu_TypeID_ilu_real[0]);
    } else if (strcmp(type_name, "longreal") == 0) {
      return PyString_FromString((char *) &ilu_TypeID_ilu_longreal[0]);
    } else if (strcmp(type_name, "shortcharacter") == 0) {
      return PyString_FromString((char *) &ilu_TypeID_ilu_shortcharacter[0]);
    } else if (strcmp(type_name, "character") == 0) {
      return PyString_FromString((char *) &ilu_TypeID_ilu_character[0]);
    } else if (strcmp(type_name, "CString") == 0) {
      return PyString_FromString((char *) &ilu_TypeID_ilu_CString[0]);
    }
  }
  Py_INCREF(Py_None);
  return Py_None;
}

#endif /* def ADD_VARIANT_SUPPORT */

#endif /* def ADD_TYPE_REGISTRATION_SUPPORT */

/************* end type registration routines *********************/

static PyObject *
  ilumod_CheckStubConsistency (PyObject *self, PyObject *args)
{
#ifdef ILU_CORBA_PYTHON_MAPPING

  PyErr_SetString(_ilupython_GeneralError,
		  "ILU stub file generated for non-CORBA Python mapping "
		  "used with runtime compiled for CORBA Python mapping");
  return 0;

#else

  ilu_string    stub_version, stub_uid_version;

  if (!PyArg_ParseTuple(args, "ss", &stub_version, &stub_uid_version))
    return 0;
  if (strcmp(stub_version, ilu_GetILUVersion()) != 0) {
    char buf[1000];
    sprintf (buf, "ILU stub version '%s' not consistent with ILU kernel version '%s'",
	     stub_version, ilu_GetILUVersion());
    PyErr_SetString(_ilupython_GeneralError, buf);
    return 0;
  };
  if (strcmp(stub_uid_version, ilu_GetILUTypeUIDVersion()) != 0) {
    char buf[1000];
    sprintf (buf, "ILU stub type UID version '%s' not consistent with ILU kernel type UID version '%s'",
	     stub_uid_version, ilu_GetILUTypeUIDVersion());
    PyErr_SetString(_ilupython_GeneralError, buf);
    return 0;
  };
  Py_INCREF(Py_None);
  return Py_None;

#endif /* def ILU_CORBA_PYTHON_MAPPING */
}

static PyObject *
  ilumod_CheckStubConsistency2 (PyObject *self, PyObject *args)
{
  ilu_string    stub_version, stub_uid_version;
  PyObject *	corba_mapping_obj;
  ilu_boolean	corba_mapping;
  if (!PyArg_ParseTuple(args, "ssO", &stub_version, &stub_uid_version, &corba_mapping_obj))
    return 0;
  if (strcmp(stub_version, ilu_GetILUVersion()) != 0) {
    char buf[1000];
    sprintf (buf, "ILU stub version '%s' not consistent with ILU kernel version '%s'",
	     stub_version, ilu_GetILUVersion());
    PyErr_SetString(_ilupython_GeneralError, buf);
    return 0;
  };
  if (strcmp(stub_uid_version, ilu_GetILUTypeUIDVersion()) != 0) {
    char buf[1000];
    sprintf (buf, "ILU stub type UID version '%s' not consistent with ILU kernel type UID version '%s'",
	     stub_uid_version, ilu_GetILUTypeUIDVersion());
    PyErr_SetString(_ilupython_GeneralError, buf);
    return 0;
  };
  corba_mapping = PyObject_IsTrue(corba_mapping_obj) ? ilu_TRUE : ilu_FALSE;
#ifdef ILU_CORBA_PYTHON_MAPPING
  if (!corba_mapping) {
    PyErr_SetString(_ilupython_GeneralError,
		    "ILU stub file generated for non-CORBA Python mapping "
		    "used with runtime compiled for CORBA Python mapping");
    return 0;
  };
#else
  if (corba_mapping) {
    PyErr_SetString(_ilupython_GeneralError,
		    "ILU stub file generated for CORBA Python mapping "
		    "used with runtime compiled for non-CORBA Python mapping");
    return 0;
  };
#endif
  Py_INCREF(Py_None);
  return Py_None;

}

static PyObject *
  GetILURootClass (void)
{
  PyObject *methodsTuple;
  PyObject *superclassTuple;
  PyObject *stateTuple;
  PyObject *retval;
  char *name, *brand, *id, *singleton;
  ilu_boolean collectible, optional;
  ilu_cardinal method_count, superclass_count;

  if ((methodsTuple = PyTuple_New(0)) == 0)
    return 0;
  if ((superclassTuple = PyTuple_New(0)) == 0)
    return 0;
  if ((stateTuple = PyTuple_New(0)) == 0)
    return 0;

  ilu_DataOfClass (ilu_rootClass,
		   &name, &brand, &id, &singleton,
		   &collectible, ILU_NIL, ILU_NIL, ILU_NIL, ILU_NIL,
#ifdef ILU_HTTPNG_OBJECTS
		   ILU_NIL, ILU_NIL, ILU_NIL, ILU_NIL,
#endif /* def ILU_HTTPNG_OBJECTS */
		   ILU_NIL);

  retval = ilucl_New(name, brand, id, singleton, collectible,
		     0, ilu_DocStringOfClass(ilu_rootClass),
		     methodsTuple, superclassTuple, ilu_FALSE,
		     ilu_FALSE, stateTuple);
  Py_DECREF(methodsTuple);
  Py_DECREF(superclassTuple);
  Py_DECREF(stateTuple);
  return retval;
}

static PyObject *
  ilumod_ThreadedOperation(PyObject *self, PyObject *args)
{
#ifdef ILU_PYTHON_THREADS
  ilu_Error err;

  if ((thread_call_stack_map = PyDict_New()) == NULL)
    {
      PyErr_SetString(_ilupython_GeneralError, "couldn't initialize thread call stack map");
      return 0;
    }

  if ((ilupython_ContextDict = PyDict_New()) == NULL)
    {
      PyErr_SetString(_ilupython_GeneralError, "couldn't initialize per-thread context dict");
      return 0;
    }

  if ((ilupython_OtherLangDict = PyDict_New()) == NULL)
    {
      PyErr_SetString(_ilupython_GeneralError, "couldn't initialize per-thread context dict");
      return 0;
    }

  if (!ilu_InitializeOSThreading(&err))
    {
      char msg[256];

      sprintf(msg, "failed to initialize kernel threads: %s", ILU_ERR_NAME(err));
      ILU_HANDLED(err);
      
      PyErr_SetString(_ilupython_GeneralError, msg);
      return 0;
    }

  if ((ilupython_MainLoopMutex = ilu_CreateMutex("Python", "main loop")) == ILU_NIL)
    {
      PyErr_SetString(_ilupython_GeneralError, "failed to create ilupython_MainLoopMutex");
      return 0;
    }
  if (ilupython_MainLoopCondition = ilu_CreateCondition("Python", "main loop", &err), ILU_ERRNOK(err))
    {
      char msg[1000];
      strcpy (msg, "failed to create ilupython_MainLoopCondition:  ");
      _ilupython_formErrDescription(msg + strlen(msg), &err);
      ILU_HANDLED(err);
      PyErr_SetString(_ilupython_GeneralError, msg);
      return 0;
    }

#if ((ILUPYTHON_MAJOR_VERSION >= 1) && (ILUPYTHON_MINOR_VERSION >= 5))
  {
    PyThreadState *current = PyThreadState_Get();
    _ilu_Assert(current != ILU_NIL, "NIL thread state in ilu.ThreadedOperation()");
    _ilupython_interpreter_state = current->interp;
  }
#endif

  ilupython_threaded_operation = ilu_TRUE;

  PyEval_InitThreads();
  PyThread_init_thread();

  ilu_NewConnectionGetterForked(&err);

  if (!ILU_ERROK(err)) {
    ilupython_threaded_operation = ilu_FALSE;
    return _ilupython_RaiseILUGeneralError(&err);
  };

  ilupython_fork_thread(ilupython_collect_outgoing_connections, NULL);

  Py_INCREF(Py_None);
  return Py_None;
#else
  PyErr_SetString(_ilupython_GeneralError, "iluPrmodule was compiled without thread support");
  return 0;
#endif
}

static PyObject *
  ilumod_SetFDBudget(PyObject *self, PyObject *args)
{
  int   budget;
  ilu_cardinal new_budget;

  if (PyArg_Parse(args, "i", &budget))
    {
      if (budget < 0)
	{
	  PyErr_SetString(_ilupython_GeneralError, "can't set FD budget to negative amount");
	  return 0;
	}
      else
	{
	  CALL_KERNEL(ilupython_threaded_operation, new_budget = ilu_SetFDBudget(budget)); 
	}
    }
  else return 0;
  return PyInt_FromLong(new_budget);
}


static PyObject *
  ilumod_GetFDBudget(PyObject *self, PyObject *args)
{
  ilu_cardinal current_budget;

  CALL_KERNEL(ilupython_threaded_operation, current_budget = ilu_GetFDBudget()); 

  return PyInt_FromLong(current_budget);
}


static PyObject *
  ilumod_DebugPrintf(PyObject *self, PyObject *args)
{
  char *line;

  if (PyArg_Parse(args, "s", &line)) {
    ilu_DebugPrintf(line);
    Py_INCREF(Py_None);
    return Py_None;
  } else
    return 0;
}

static PyObject *
  ilumod_CRC32 (PyObject *self, PyObject *args)
{
  char *buf;
  int inputlen;
  ilu_cardinal accum = 0xFFFFFFFF;
  unsigned long crc;

  if (!PyArg_Parse(args, "s#|l", &buf, &inputlen, &accum))
    return 0;
  crc = ilu_CRC32WithAccum((ilu_bytes) buf, inputlen, accum);
#if ILUPYTHON_MAJOR_VERSION > 1 || ILUPYTHON_MINOR_VERSION > 4
  return PyLong_FromUnsignedLong(crc);
#else
  return PyLong_FromLong((long) crc);
#endif
}

static PyMethodDef ilumod_methods[] =
{
  { "SetDebugLevel",        ilumod_SetDebugLevel        },
  { "SetDebugLevelViaString",   ilumod_SetDebugLevel        },
  { "DebugPrintf",          ilumod_DebugPrintf      },
  { "CheckStubConsistency",     ilumod_CheckStubConsistency },
  { "CheckStubConsistency2",	ilumod_CheckStubConsistency2 },

  { "FormClassRecord",      ilumod_FormClassRecord      },
  { "RegisterClass",        ilumod_RegisterClass        },
  { "RegisterSkeletons",    ilumod_RegisterSkeletons    },
  { "RegisterCustomSurrogate",  ilumod_RegisterCustomSurrogate  },
  { "CreateServer",         ilumod_CreateServer     },
  { "Server",               ilumod_Server     },
  { "DefaultServer",        ilumod_DefaultServer        },
  { "ObjectOfSBH",          ilumod_ObjectOfSBH      },
  { "SBHOfObject",          ilumod_SBHOfObject      },
  { "IOROfObject",          ilumod_IOROfObject      },
  { "Delete",               ilumod_Delete           },

  { "PublishObject",        ilumod_PublishObject        },
  { "WithdrawObject",       ilumod_WithdrawObject       },
  { "LookupObject",         ilumod_LookupObject     },
  { "PingObject",           ilumod_PingObject       },
  { "IsA",                  ilumod_IsA          },
  { "ParseSBH",         ilumod_ParseSBH         },
  { "FormSBH",          ilumod_FormSBH          },
  { "FindObject",           ilumod_FindObject       },
  { "FindOrCreateSurrogate",    ilumod_FindOrCreateSurrogate    },

  { "RegisterInputHandler",     ilumod_RegisterInputHandler },
  { "RegisterOutputHandler",    ilumod_RegisterOutputHandler },
  { "CreateLoopHandle",     ilumod_CreateLoopHandle     },
  { "RunMainLoop",          ilumod_RunMainLoop      },
  { "ExitMainLoop",         ilumod_ExitMainLoop     },
  { "DoSoon",           ilumod_DoSoon           },

  { "SetMainLoop",          ilumod_SetMainLoop      },

  { "LongReal",         ilumod_LongReal         },

  { "FineTime",         ilumod_FineTime         },
  { "FineTime_Now",         ilumod_FineTime_Now     },
  { "CreateAlarm",          ilumod_CreateAlarm      },

  { "CreatePassport",       ilumod_CreatePassport       },
  { "SetPassport",          ilumod_SetPassport      },
  { "GetPassport",          ilumod_GetPassport      },
  { "CallerIdentity",       ilumod_CallerIdentity       },
#ifdef SECURE_TRANSPORT
  { "AcquireGSSCredForName",    iluGSSCred_AcquireCredForName   },
#endif

  { "CreatePipeline",       ilumod_CreatePipeline       },
  { "SetPipeline",          ilumod_SetPipeline      },
  { "GetPipeline",          ilumod_GetPipeline      },
  { "SetSerializer",        ilumod_SetSerializer        },
  { "GetSerializer",        ilumod_GetSerializer        },

  { "BeginCall",            ilumod_BeginCall        },
  { "FinishCall",           ilumod_FinishCall       },
  { "BeginRequest",         ilumod_BeginRequest     },
  { "FinishRequest",        ilumod_FinishRequest        },
  { "GetReply",         ilumod_GetReply         },
  { "ReplyRead",            ilumod_ReplyRead        },
  { "ExceptionName",        ilumod_ExceptionName        },

  { "GetSingleton",         ilumod_GetSingleton     },
  { "RequestRead",          ilumod_RequestRead      },
  { "NoReply",          ilumod_NoReply          },
  { "BeginSizingReply",     ilumod_BeginSizingReply     },
  { "BeginReply",           ilumod_BeginReply       },
  { "FinishReply",          ilumod_FinishReply      },
  { "BeginSizingException",     ilumod_BeginSizingException },
  { "BeginException",       ilumod_BeginException       },
  { "FinishException",      ilumod_FinishException      },
  { "CaughtUnexpectedException",ilumod_UnexpectedException  },
#ifdef ILU_CORBA_PYTHON_MAPPING
  { "CaughtSystemException",	ilumod_CaughtSystemException  },
#endif /* def ILU_CORBA_PYTHON_MAPPING */
  { "SetCalloutExceptionHandler",   ilumod_SetCalloutExceptionHandler },

  { "SizeOfObjectID",       ilumod_SizeOfObjectID       },
  { "OutputObjectID",       ilumod_OutputObjectID       },
  { "InputObjectID",        ilumod_InputObjectID        },

  { "SizeOfShortInteger",       ilumod_SizeOfShortInteger   },
  { "OutputShortInteger",       ilumod_OutputShortInteger   },
  { "InputShortInteger",        ilumod_InputShortInteger    },

  { "SizeOfInteger",        ilumod_SizeOfInteger        },
  { "OutputInteger",        ilumod_OutputInteger        },
  { "InputInteger",         ilumod_InputInteger     },

  { "SizeOfLongInteger",        ilumod_SizeOfLongInteger    },
  { "OutputLongInteger",        ilumod_OutputLongInteger    },
  { "InputLongInteger",     ilumod_InputLongInteger     },

  { "SizeOfShortCardinal",      ilumod_SizeOfShortCardinal  },
  { "OutputShortCardinal",      ilumod_OutputShortCardinal  },
  { "InputShortCardinal",       ilumod_InputShortCardinal   },

  { "SizeOfCardinal",       ilumod_SizeOfCardinal       },
  { "OutputCardinal",       ilumod_OutputCardinal       },
  { "InputCardinal",        ilumod_InputCardinal        },

  { "SizeOfLongCardinal",       ilumod_SizeOfLongCardinal   },
  { "OutputLongCardinal",       ilumod_OutputLongCardinal   },
  { "InputLongCardinal",        ilumod_InputLongCardinal    },

  { "SizeOfShortReal",      ilumod_SizeOfShortReal      },
  { "OutputShortReal",      ilumod_OutputShortReal      },
  { "InputShortReal",       ilumod_InputShortReal       },

  { "SizeOfReal",           ilumod_SizeOfReal       },
  { "OutputReal",           ilumod_OutputReal       },
  { "InputReal",            ilumod_InputReal        },

  { "SizeOfLongReal",       ilumod_SizeOfLongReal       },
  { "OutputLongReal",       ilumod_OutputLongReal       },
  { "InputLongReal",        ilumod_InputLongReal        },

  { "SizeOfEnum",           ilumod_SizeOfEnum       },
  { "OutputEnum",           ilumod_OutputEnum       },
  { "InputEnum",            ilumod_InputEnum        },

  { "SizeOfCharacter",      ilumod_SizeOfCharacter      },
  { "OutputCharacter",      ilumod_OutputCharacter      },
  { "InputCharacter",       ilumod_InputCharacter       },

  { "SizeOfShortCharacter",     ilumod_SizeOfShortCharacter },
  { "OutputShortCharacter",     ilumod_OutputShortCharacter },
  { "InputShortCharacter",      ilumod_InputShortCharacter  },

  { "SizeOfByte",           ilumod_SizeOfByte       },
  { "OutputByte",           ilumod_OutputByte       },
  { "InputByte",            ilumod_InputByte        },

  { "SizeOfBoolean",        ilumod_SizeOfBoolean        },
  { "OutputBoolean",        ilumod_OutputBoolean        },
  { "InputBoolean",         ilumod_InputBoolean     },

  { "SizeOfBytes",          ilumod_SizeOfBytes      },
  { "OutputBytes",          ilumod_OutputBytes      },
  { "InputBytes",           ilumod_InputBytes       },

  { "SizeOfString",         ilumod_SizeOfString     },
  { "OutputString",         ilumod_OutputString     },
  { "InputString",          ilumod_InputString      },

  { "SizeOfWString",        ilumod_SizeOfWString        },
  { "OutputWString",        ilumod_OutputWString        },
  { "InputWString",         ilumod_InputWString     },

  { "SizeOfOpaque",         ilumod_SizeOfOpaque     },
  { "OutputOpaque",         ilumod_OutputOpaque     },
  { "InputOpaque",          ilumod_InputOpaque      },

  { "SizeOfStringVec",      ilumod_SizeOfStringVec      },
  { "OutputStringVec",      ilumod_OutputStringVec      },
  { "InputStringVec",       ilumod_InputStringVec       },

  { "SizeOfWStringVec",     ilumod_SizeOfWStringVec     },
  { "OutputWStringVec",     ilumod_OutputWStringVec     },
  { "InputWStringVec",      ilumod_InputWStringVec      },

  { "SizeOfOptional",       ilumod_SizeOfOptional       },
  { "OutputOptional",       ilumod_OutputOptional       },
  { "InputOptional",        ilumod_InputOptional        },

#ifdef ILU_REFERENCE_TYPES

  { "SizeOfReference",      ilumod_SizeOfReference      },
  { "OutputReference",      ilumod_OutputReference      },
  { "InputReference",       ilumod_InputReference       },
  { "EndInputReference",    ilumod_EndInputReference    },

#endif

  { "SizeOfUnion",          ilumod_SizeOfUnion      },
  { "OutputUnion",          ilumod_OutputUnion      },
  { "InputUnion",           ilumod_InputUnion       },
  { "EndUnion",         ilumod_EndUnion         },

  { "SizeOfArray",          ilumod_SizeOfArray      },
  { "OutputArray",          ilumod_OutputArray      },
  { "InputArray",           ilumod_InputArray       },
  { "EndArray",         ilumod_EndArray         },

  { "SizeOfRecord",         ilumod_SizeOfRecord     },
  { "OutputRecord",         ilumod_OutputRecord     },
  { "InputRecord",          ilumod_InputRecord      },
  { "EndRecord",            ilumod_EndRecord        },

  { "SizeOfSequence",       ilumod_SizeOfSequence       },
  { "OutputSequence",       ilumod_OutputSequence       },
  { "InputSequence",        ilumod_InputSequence        },
  { "EndSequence",          ilumod_EndSequence      },

#ifdef ILU_FIXED_POINT_SUPPORT

  { "KernelBignumForValue",     ilumod_KernelBignumForValue },
  { "OutputFixedpoint",     ilumod_OutputFixedpoint     },
  { "InputFixedpoint",      ilumod_InputFixedpoint      },
  { "SizeOfFixedpoint",     ilumod_SizeOfFixedpoint     },

#endif /* def ILU_FIXED_POINT_SUPPORT */

  { "TCPStatistics",        ilumod_TCPStatistics        },
  { "TCPDefaultBufferSize",     ilumod_TCPDefaultBufferSize },

#ifdef ADD_VARIANT_SUPPORT

  { "SizeOfPickleBytes",        ilumod_SizeOfPickleBytes    },
  { "OutputPickleBytes",        ilumod_OutputPickleBytes    },
  { "InputPickleBytes",     ilumod_InputPickleBytes     },

  { "PickleTypeID",         ilumod_PickleTypeID     },
  { "PickleTypeIDs",        ilumod_PickleTypeIDs        },
  { "StartPickle",          ilumod_StartPickle      },
  { "WritePickle",          ilumod_WritePickle      },
  { "ReadPickle",           ilumod_ReadPickle       },
  { "EndPickle",            ilumod_EndPickle        },

#endif /* ADD_VARIANT_SUPPORT */

#ifdef ADD_TYPE_REGISTRATION_SUPPORT

  { "RegisterSequenceType",     ilumod_RegisterSequenceType },
  { "RegisterOptionalType",     ilumod_RegisterOptionalType },
#ifdef ILU_REFERENCE_TYPES
  { "RegisterReferenceType",    ilumod_RegisterReferenceType},
#endif /* def ILU_REFERENCE_TYPES */
  { "RegisterAliasType",        ilumod_RegisterAliasType    },
  { "RegisterArrayType",        ilumod_RegisterArrayType    },
  { "RegisterRecordType",       ilumod_RegisterRecordType   },
  { "RegisterEnumerationType",  ilumod_RegisterEnumerationType  },
  { "RegisterUnionType",        ilumod_RegisterUnionType    },
  { "FindTypeByUID",        ilumod_FindTypeByUID        },

#endif /* def ADD_TYPE_REGISTRATION_SUPPORT */

#if (defined(ADD_TYPE_REGISTRATION_SUPPORT) || defined(ADD_VARIANT_SUPPORT))
  { "FindTypeIDByName",     ilumod_FindTypeIDByName     },
#endif /* (defined(ADD_TYPE_REGISTRATION_SUPPORT) || defined(ADD_VARIANT_SUPPORT)) */

#ifdef ILU_CORBA_PYTHON_MAPPING
  { "RaiseSystemException",	ilumod_RaiseSystemException,	},
#endif /* def ILU_CORBA_PYTHON_MAPPING */

  {     "ThreadedOperation",            ilumod_ThreadedOperation        },
  {     "SetFDBudget",                  ilumod_SetFDBudget              },
  {     "GetFDBudget",                  ilumod_GetFDBudget              },
  {     "CRC32",	                ilumod_CRC32                    },

  { 0                               }
};

/********************************/

static PyObject *
  newException(PyObject *dict, char *name)
{
  PyObject *    e   = PyString_FromString(name);

  if (e == 0 || PyDict_SetItemString(dict, name, e) < 0)
    Py_FatalError("ilu can't define exception");
  return e;
}

static void
  createExceptions(PyObject *modDict)
{
  _ilupython_GeneralError = newException(modDict, "IluGeneralError");
  ProtocolError = newException(modDict, "IluProtocolError");
  UnimplementedMethodError = newException(modDict,
					  "IluUnimplementedMethodError");
  UnknownTypeIDError = newException(modDict,
				    "IluUnknownTypeIDError");
}

/********************************/

static void
  createConstants(PyObject *modDict)
{
  PyObject *    FineTimeRate;
  PyObject *    Version;
  PyObject *    RootClass;
  PyObject *    RetryCall;
  PyObject *	BuildDomain;
  PyObject *    TypeUIDVersion;
  PyObject *    TypeKind_cardinal;
  PyObject *    TypeKind_integer;
  PyObject *    TypeKind_enumeration;
  PyObject *    TypeKind_shortcardinal;
  PyObject *    TypeKind_shortinteger;
  PyObject *    TypeKind_character;
  PyObject *    TypeKind_boolean;
  PyObject *    TypeKind_byte;
  PyObject *    TypeKind_shortcharacter;

  if (PyDict_SetItemString(modDict, "FALSE", Py_False) < 0 ||
      PyDict_SetItemString(modDict, "TRUE", Py_True) < 0)
    Py_FatalError("ilu can't define FALSE and TRUE");

#ifdef ADD_VARIANT_SUPPORT
  if (PyDict_SetItemString(modDict, "HasVariantSupport", Py_True) < 0)
    Py_FatalError("ilu can't define HasVariantSupport");
#else
  if (PyDict_SetItemString(modDict, "HasVariantSupport", Py_False) < 0)
    Py_FatalError("ilu can't define HasVariantSupport");
#endif /* ADD_VARIANT_SUPPORT */

#ifdef ILU_CORBA_PYTHON_MAPPING
  if (PyDict_SetItemString(modDict, "CORBAMapping", Py_True) < 0)
    Py_FatalError("ilu can't define CORBAMapping");
#else
  if (PyDict_SetItemString(modDict, "CORBAMapping", Py_False) < 0)
    Py_FatalError("ilu can't define CORBAMapping");
#endif /* def ILU_CORBA_PYTHON_MAPPING */

#ifdef ILU_PYTHON_DICTIONARIES
  if (PyDict_SetItemString(modDict, "DictionaryPassing", Py_True) < 0)
    Py_FatalError("ilu can't define DictionaryPassing");
#else
  if (PyDict_SetItemString(modDict, "DictionaryPassing", Py_False) < 0)
    Py_FatalError("ilu can't define DictionaryPassing");
#endif /* def ILU_CORBA_PYTHON_MAPPING */

  if ((FineTimeRate = PyInt_FromLong(ilu_FineTimeRate)) == 0 ||
      PyDict_SetItemString(modDict, "FineTimeRate", FineTimeRate) < 0)
    Py_FatalError("ilu can't define FineTimeRate");
  Py_DECREF(FineTimeRate);

  if ((RetryCall = PyInt_FromLong(PYTHON_RETRY_CALL_CODE)) == 0 ||
      PyDict_SetItemString(modDict, "RetryCall", RetryCall) < 0)
    Py_FatalError("ilu can't define RetryCall");
  Py_DECREF(RetryCall);

  if ((Version = PyString_FromString(ilu_GetILUVersion())) == 0 ||
      PyDict_SetItemString(modDict, "Version", Version) < 0)
    Py_FatalError("ilu can't define Version");
  Py_DECREF(Version);

#define STRINGIFIED_DOMAIN(x)	#x
#define STRINGIFIED_DOMAIN1(x)	STRINGIFIED_DOMAIN(x)
#define THIS_DOMAIN		STRINGIFIED_DOMAIN1(FULLY_QUALIFIED_DOMAIN_NAME)

  if ((BuildDomain = PyString_FromString(THIS_DOMAIN)) == 0 ||
      PyDict_SetItemString(modDict, "BuildDomain", BuildDomain) < 0)
    Py_FatalError("ilu can't define BuildDomain");
  Py_DECREF(BuildDomain);

  if ((TypeUIDVersion = PyString_FromString(ilu_GetILUTypeUIDVersion())) == 0 ||
      PyDict_SetItemString(modDict, "TypeUIDVersion", TypeUIDVersion) < 0)
    Py_FatalError("ilu can't define TypeUIDVersion");
  Py_DECREF(TypeUIDVersion);

  if ((RootClass = GetILURootClass()) == 0 ||
      PyDict_SetItemString(modDict, "ilu_rootClass", RootClass) < 0)
    Py_FatalError("ilu can't define ilu_rootClass");
  Py_DECREF(RootClass);

  if ((TypeKind_byte = PyInt_FromLong((long) ilu_byte_tk)) == 0 ||
      PyDict_SetItemString(modDict, "TypeKind_byte", TypeKind_byte) < 0)
    Py_FatalError("ilu can't define TypeKind_byte");
  Py_DECREF(TypeKind_byte);

  if ((TypeKind_shortcharacter = PyInt_FromLong((long) ilu_shortcharacter_tk)) == 0 ||
      PyDict_SetItemString(modDict, "TypeKind_shortcharacter", TypeKind_shortcharacter) < 0)
    Py_FatalError("ilu can't define TypeKind_shortcharacter");
  Py_DECREF(TypeKind_shortcharacter);

  if ((TypeKind_boolean = PyInt_FromLong((long) ilu_boolean_tk)) == 0 ||
      PyDict_SetItemString(modDict, "TypeKind_boolean", TypeKind_boolean) < 0)
    Py_FatalError("ilu can't define TypeKind_boolean");
  Py_DECREF(TypeKind_boolean);

  if ((TypeKind_character = PyInt_FromLong((long) ilu_character_tk)) == 0 ||
      PyDict_SetItemString(modDict, "TypeKind_character", TypeKind_character) < 0)
    Py_FatalError("ilu can't define TypeKind_character");
  Py_DECREF(TypeKind_character);

  if ((TypeKind_shortinteger = PyInt_FromLong((long) ilu_shortinteger_tk)) == 0 ||
      PyDict_SetItemString(modDict, "TypeKind_shortinteger", TypeKind_shortinteger) < 0)
    Py_FatalError("ilu can't define TypeKind_shortinteger");
  Py_DECREF(TypeKind_shortinteger);

  if ((TypeKind_shortcardinal = PyInt_FromLong((long) ilu_shortcardinal_tk)) == 0 ||
      PyDict_SetItemString(modDict, "TypeKind_shortcardinal", TypeKind_shortcardinal) < 0)
    Py_FatalError("ilu can't define TypeKind_shortcardinal");
  Py_DECREF(TypeKind_shortcardinal);

  if ((TypeKind_cardinal = PyInt_FromLong((long) ilu_cardinal_tk)) == 0 ||
      PyDict_SetItemString(modDict, "TypeKind_cardinal", TypeKind_cardinal) < 0)
    Py_FatalError("ilu can't define TypeKind_cardinal");
  Py_DECREF(TypeKind_cardinal);

  if ((TypeKind_integer = PyInt_FromLong((long) ilu_integer_tk)) == 0 ||
      PyDict_SetItemString(modDict, "TypeKind_integer", TypeKind_integer) < 0)
    Py_FatalError("ilu can't define TypeKind_integer");
  Py_DECREF(TypeKind_integer);

  if ((TypeKind_enumeration = PyInt_FromLong((long) ilu_enumeration_tk)) == 0 ||
      PyDict_SetItemString(modDict, "TypeKind_enumeration", TypeKind_enumeration) < 0)
    Py_FatalError("ilu can't define TypeKind_enumeration");
  Py_DECREF(TypeKind_enumeration);
}

/********************************/

void initiluPr(void);   /* added to keep gcc happy */

/* the following flag ensures that we don't initialize the
   interface to the ILU kernel more than once, even though the
   Python module may be initialized more than once. */
static ilu_boolean iluKernelInitialized = ilu_FALSE;

void
  initiluPr(void)
{
  PyObject *    mod = Py_InitModule("iluPr", ilumod_methods);
  PyObject *    dict    = PyModule_GetDict(mod);
  char *    p;

  if (!iluKernelInitialized) {
    _ilupython_LangIndex = ilu_RegisterLanguage ("Python");

    ilu_SetNoter (trackKernelInterest, _ilupython_LangIndex);

    if ((p = getenv("ILU_ASSERTION_FAILURE_ACTION")) != NULL) {
      int val;
      val = atoi(p);
      ilu_SetAssertionFailureAction(val);
    };
    if ((p = getenv("ILU_MEMORY_FAILURE_ACTION")) != NULL) {
      int val;
      val = atoi(p);
      ilu_SetMemFailureAction(val);
    };
    if ((p = getenv("ILU_CHECK_FAILURE_ACTION")) != NULL) {
      int val;
      val = atoi(p);
      ilu_SetCheckFailureAction(val);
    };
    iluKernelInitialized = ilu_TRUE;
  }
  createExceptions(dict);
  createConstants(dict);
  classMap = PyDict_New();
  if (PyDict_SetItemString(dict, "ClassMappings", classMap) < 0)
    Py_FatalError ("ilu can't define ClassMappings");
  Py_DECREF(classMap);
}

static void 
  singleThreadedReadServiceRequest(ilu_refany rock)
{
  (void)readServiceRequest(rock, ilu_TRUE);
}

static void 
  inmemReadServiceRequest(ilu_refany rock)
{
  (void)readServiceRequest(rock, !ilupython_threaded_operation ? ilu_TRUE : ilu_FALSE);
}

#ifdef ILU_PYTHON_THREADS
static void
  readConnectionRequests(void *arg)
{
  ilu_Port port = (ilu_Port) arg;
  ilu_boolean closed = ilu_FALSE;
  ilu_Connection conn;
  ilu_Error err;

  ILUPY_ILLEGAL_IN_UNTHREADED;

  while (!closed) {
    ilu_WaitForPortConnectionRequest(port, &err);
    ILU_MUST_BE_SUCCESS(err);
    conn = ilu_HandleNewConnection(port, &closed, &err);
    ILU_ERR_SWITCH(err) {
      ILU_SUCCESS_CASE {
      }
      ILU_ERR_CASE(no_resources, x) {
	ilu_DebugPrintf("ILU/Python:  Connection request on port %p"
			" ran over FD limit --- port abandoned.\n", port);
	closed = ilu_TRUE;
      }
      ILU_ERR_ELSE {
	char buf[ERRFMTSIZE];
	_ilupython_formErrDescription (buf, &err);
	ilu_DebugPrintf("ILU/Python:  Got error %s"
			" trying to accept connection on port %p;"
			" abandoning port.\n", buf, port);
	closed = ilu_TRUE;
      }
    } ILU_ERR_ENDSWITCH;
    if (conn != ILU_NIL)
      ilupython_fork_thread(runConnection, (void *)conn);
  }

  PyThread_exit_thread();
}

static void
  runConnection(void *arg)
{
  ilu_Connection conn = (ilu_Connection) arg;
  ilu_boolean closed = ilu_FALSE;
  PyObject *tid;
  PyObject *call_stack = ILU_NIL;
  ilu_Error err;

  ILUPY_ILLEGAL_IN_UNTHREADED;

  NEW_THREAD_ENTER;

  tid = current_thread_id();

  if (!tid)
    return;

  if (PyDict_GetItem(thread_call_stack_map, tid) == 0)
    {
      /* top level of the stack */

      if (!(call_stack = ilupython_thread_init_stack(tid)))
	{
	  Py_DECREF(tid);
	  return;
	}
      /* note that call_stack will be NIL in every level except
	 the top one */
    }


  while (!closed)
    {
      CALL_KERNEL(ilupython_threaded_operation, closed = BOOLEAN(!ilu_BlockingWaitForInputOnConnection(conn, ILU_NIL)));
      
      if (!closed)
	{
	  closed = readServiceRequest(conn, ilu_FALSE);
	}
    }

  if (call_stack != ILU_NIL)
    {
      _ilu_Assert(PyDict_DelItem(thread_call_stack_map, tid) == 0, "PyDict_DelItem failed");
      Py_DECREF(call_stack);
    }

  Py_DECREF(tid);

  FINISHED_THREAD_EXIT;

  if (call_stack != ILU_NIL) {
    ilu_DoneServingConnection(conn, &err);
    if (ILU_ERRNOK(err)) {
      ILU_ERRPRINTF("Warning: Error from ilu_DoneServingConnection ignored at line %d of %s\n", __LINE__, __FILE__);
      ILU_HANDLED(err);
    }
  }

  PyThread_exit_thread();
}

static BORROWED(PyObject *)
     ilupython_thread_push_call(PyObject *tid, PyObject *call)
{
  PyObject *call_list;

  ILUPY_ILLEGAL_IN_UNTHREADED;

  call_list = PyDict_GetItem(thread_call_stack_map, tid);

  _ilu_Assert(call_list != ILU_NIL, "call_list is NIL");

  return (PyList_Append(call_list, call) != 0 ? 0 : call_list);
}

static void
  ilupython_thread_pop_call(PyObject *stack)
{
  int list_size = PyList_Size(stack);

  ILUPY_ILLEGAL_IN_UNTHREADED;

  if (list_size > 0)
    PyList_SetSlice(stack, list_size - 1, list_size, NULL);
}

static BORROWED(PyObject *)
     ilupython_thread_current_context(void)
{
  PyObject *tid, *tup;

  if (!(tid = current_thread_id()))
    return 0;
  tup = PyDict_GetItem(ilupython_ContextDict, tid);
  if (tup == 0) {
    tup = PyList_New(3);
    if (tup == 0)
      return 0;
    Py_INCREF(Py_None);
    PyList_SetItem(tup, ContextSlots_Serializer, Py_None);
    Py_INCREF(Py_None);
    PyList_SetItem(tup, ContextSlots_Pipeline, Py_None);
    Py_INCREF(Py_None);
    PyList_SetItem(tup, ContextSlots_Passport, Py_None);
    PyDict_SetItem(ilupython_ContextDict, tid, tup);
  }
  return tup;
}

static BORROWED(PyObject *)
     ilupython_thread_current_call()
{
  PyObject *tid, *stack, *obj;

  ILUPY_ILLEGAL_IN_UNTHREADED;

  if (!(tid = current_thread_id()))
    return 0;

  stack = PyDict_GetItem(thread_call_stack_map, tid);
  
  Py_DECREF(tid);

  if (!stack)
    return 0;

  obj = PyList_GetItem(stack, PyList_Size(stack) - 1);

  return obj;
}

static PyObject *
  ilupython_thread_init_stack(PyObject *tid)
{
  PyObject *new_stack;

  ILUPY_ILLEGAL_IN_UNTHREADED;

  if (!(new_stack = PyList_New(0)))
    return 0;

  if (PyDict_SetItem(thread_call_stack_map, tid, new_stack) != 0)
    return 0;

  return new_stack;
}

static void
  bootstrapAlarm(ilu_private rock)
{
  bootstrap_rock *bootstrap = (bootstrap_rock *)rock;

  NEW_THREAD_ENTER; 
  bootstrap->real_proc(bootstrap->arg);

  FINISHED_THREAD_EXIT; 
}

static void
  ilupython_watch_outgoing_connection(void *arg)
{
  ilu_Connection conn = (ilu_Connection)arg;
  ilu_Error err = ILU_INIT_NO_ERR;

  ILUPY_ILLEGAL_IN_UNTHREADED;

  ilu_OutgoingConnectionThreadProc(conn, &err);
  ILU_MUST_BE_SUCCESS(err);

  PyThread_exit_thread();
}

static void
  ilupython_collect_outgoing_connections(void *arg)
{
  ilu_Error err = ILU_INIT_NO_ERR;
  ilu_Connection conn;

  ILUPY_ILLEGAL_IN_UNTHREADED;

  while (1)
    {
      conn = ilu_OtherNewConnection(&err);
      if (ILU_ERROK(err))
	{
	  ilupython_fork_thread(ilupython_watch_outgoing_connection, conn);
	}
      else
	{
	  ilu_DebugPrintf("ilu_OtherNewConnection raises error %s!\n", ILU_ERR_NAME(err));
	  ILU_MUST_BE_SUCCESS(err);
	}
    }

  /* exit_thread(); */
}

#if ((ILUPYTHON_MAJOR_VERSION >= 1) && (ILUPYTHON_MINOR_VERSION >= 5))

  void
  _ilupython_new_thread_enter_interpreter (void)
{
  PyThreadState *newstate = PyThreadState_New(_ilupython_interpreter_state);
  PyEval_AcquireLock();
  PyThreadState_Swap(newstate);
}

void
  _ilupython_dying_thread_release_interpreter (void)
{
  PyThreadState *oldstate = PyThreadState_Swap(NULL);
  PyThreadState_Delete(oldstate);
  PyEval_ReleaseLock();
}

#endif /* ((ILUPYTHON_MAJOR_VERSION >= 1) && (ILUPYTHON_MINOR_VERSION >= 5)) */

ilu_boolean
  ilupython_ForkNewThread (ilu_ClosureProc proc, void *rock,
			   ILU_ERRS((no_memory, no_resources,
				     internal)) *err)
{
  if (!ilupython_threaded_operation) {
    return ILU_ERR_CONS1(internal, err, minor, ilu_im_threading, ilu_FALSE);
  };

  ilupython_fork_thread(proc, rock);

  PyThread_exit_thread();
  return ILU_CLER(*err);
}

static void *
     thread_other_language_data(const void * val, ilu_ClosureProc destructor)
{
  PyObject *tid, *tup;

  if (!(tid = current_thread_id()))
    return 0;
  tup = PyDict_GetItem(ilupython_OtherLangDict, tid);
  if ((tup == 0) && (val == 0))
    return 0;
  if ((tup != 0) && (val == 0))
    return PyCObject_AsVoidPtr(tup);
  if (val != 0) {
    tup = PyCObject_FromVoidPtr((void *) val, destructor);
    if (tup == 0)
      return 0;
    PyDict_SetItem(ilupython_OtherLangDict, tid, tup);
  }
  return (void *) val;
}

static ilu_PerThreadDataDestructor thread_other_language_destructor = 0;

static void *
  get_per_thread_data_for_other_language (void)
{
  return thread_other_language_data(0, 0);
}

static void
  set_per_thread_data_for_other_language (const void *data, ilu_Error *err)
{
  void *val = thread_other_language_data(data, thread_other_language_destructor);
  if (val != data)
    ILU_ERR_CONS1(internal, err, minor, ilu_im_threading, 0);
  else
    ILU_CLER(*err);
}


ilu_boolean
  ilupython_GetPerThreadDataTech
  (ilu_PerThreadDataDestructor destructor,	/* IN, GLOBAL, OPTIONAL */
   ilu_PerThreadDataGetter *getter,		/* OUT, GLOBAL */
   ilu_PerThreadDataSetter *setter,		/* OUT, GLOBAL */
   ILU_ERRS((no_memory, internal)) *err)
{
  if ((destructor != 0) && (thread_other_language_destructor == 0))
    thread_other_language_destructor = destructor;
  *getter = get_per_thread_data_for_other_language;
  *setter = set_per_thread_data_for_other_language;
  ILU_CLER(*err);
  return ilu_TRUE;
}

#endif /* ILU_PYTHON_THREADS */

static void printPyObject(PyObject *o)
{
  PyObject *str = PyObject_Str(o);
  if (str != ILU_NIL)
    {
      ILU_ERRPRINTF("%s\n", PyString_AsString(str));
      Py_DECREF(str);
    }
  else
    ILU_ERRPRINTF("Error obtaining string form of object %p (refcount=%d, type=%p)\n",
		  o, o->ob_refcnt, o->ob_type);
}
