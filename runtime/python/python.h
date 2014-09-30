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

$Id: python.h,v 1.32 1999/08/03 01:55:32 janssen Exp $
*/

#ifdef __cplusplus
#undef ILU_NIL
#define ILU_NIL 0
#define BOOLEAN(x) ((x) ? ilu_TRUE : ilu_FALSE)
#else
#define BOOLEAN(x) (x)
#endif

/* Imports from Python */

#define HAVE_PROTOTYPES 1	/* We assume an ANSI C compiler */
#define HAVE_STDARG_PROTOTYPES 1	/* Again, ANSI C */

#include "pythonversion.h"

/* define dllexport to support building DLLs on Win32 */
#if defined(WIN32)
#if defined(ILU_BUILDING_RUNTIME)
#define ILU_RUNTIME_PUBLIC __declspec(dllexport) extern
#define ILU_RUNTIME_PUBLIC_CLASS  class __declspec(dllexport)
#else
#define ILU_RUNTIME_PUBLIC __declspec(dllimport) extern
#define ILU_RUNTIME_PUBLIC_CLASS class __declspec(dllimport)
#endif /* defined(ILU_BUILDING_RUNTIME) */
#else
#define ILU_RUNTIME_PUBLIC extern
#define ILU_RUNTIME_PUBLIC_CLASS class
#endif /* defined(WIN32) */

#if (ILUPYTHON_MAJOR_VERSION == 1 && ILUPYTHON_MINOR_VERSION >= 2)

#include <Python.h>
#include <traceback.h>
#include <pythonrun.h>

#else 

/* from Python */
#include "allobjects.h"
#include "rename1.h"

/* The following seem to be missing (as of Python 1.0.3) from rename1.h: */
#ifndef PyObject
typedef object PyObject;
typedef struct methodlist PyMethodDef;
#endif

#endif /* (ILUPYTHON_MAJOR_VERSION == 1 && ILUPYTHON_MINOR_VERSION >= 2) */

/* The following are for convenience. */
#define ERRFMTSIZE 512

#if (ILUPYTHON_MAJOR_VERSION == 1 && ILUPYTHON_MINOR_VERSION < 5)
extern char * _ilupython_formErrDescription (char [ERRFMTSIZE], ilu_Error *);
#else
extern const char * _ilupython_formErrDescription (char [ERRFMTSIZE], ilu_Error *);
#endif /* (ILUPYTHON_MAJOR_VERSION == 1 && ILUPYTHON_MINOR_VERSION < 5) */

extern PyObject *_ilupython_GeneralError;
extern ilu_cardinal _ilupython_LangIndex;
extern void _ilupython_handleCalloutException (char *, ilu_Error *);
extern PyObject *_ilupython_RaiseILUGeneralError(ilu_Error *);
extern PyObject *_ilupython_createSerializer(ilu_Server);
extern ilu_Port _ilupython_createPort(ilu_Server, ilu_TransportInfo, ilu_ProtocolInfo, ilu_Passport, ilu_boolean);
extern int _ilupython_convTinfo (PyObject *o, ilu_TransportInfo *);

ILU_RUNTIME_PUBLIC ilu_boolean
  ilupython_ForkNewThread (ilu_ClosureProc, void *,
			   ILU_ERRS((no_memory, no_resources,
				     internal)) *);

ILU_RUNTIME_PUBLIC ilu_boolean
  ilupython_GetPerThreadDataTech
  (ilu_PerThreadDataDestructor ,	/* IN, GLOBAL, OPTIONAL */
   ilu_PerThreadDataGetter *,		/* OUT, GLOBAL */
   ilu_PerThreadDataSetter *,		/* OUT, GLOBAL */
   ILU_ERRS((no_memory, internal)) *);




