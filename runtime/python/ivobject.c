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

$Id: ivobject.c,v 1.20 1999/08/03 01:55:29 janssen Exp $
*/

#if defined(WIN32)
#include <process.h>	/* for _getpid() */
#else
#include <unistd.h>		/* for getpid() */
#endif

/* from ILU */
#include "iluxport.h"

/* local */
#include "python.h"
#include "ivobject.h"
#include "pythonthreads.h"

#include "ilusvobject.h"

extern ilu_cardinal _ilupython_LangIndex;

static void
iluiv_dealloc(PyObject *o)
{
  IvObject *	iv	= (IvObject *) o;
  ilu_boolean	stat;

  if (iv->kserver != 0 && iv->kclass != 0)
    {
      if (iv->publish_proof != 0)
	{
	  CALL_KERNEL(ilupython_threaded_operation, ilu_EnterServer(iv->kserver, iv->kclass));
	  /* Don't withdraw the object if it was created
	   ** by another process. */
	  if (iv->kobj != 0 && iv->publish_proof != 0 &&
#if defined(WIN32)
	      iv->creatorPid == _getpid())
#else
	    iv->creatorPid == getpid())
#endif
	  {
	    CALL_KERNEL(ilupython_threaded_operation, stat = ilu_WithdrawObject(iv->kobj, iv->publish_proof));
	    if (stat)
	      iv->publish_proof = ILU_NIL; /* freed in ilu_WithDrawObject */
	  }
      else
	ilu_ExitServer(iv->kserver, iv->kclass);
      if (iv->publish_proof)
	PyMem_XDEL(iv->publish_proof);
      iv->publish_proof = 0;
    }

  CALL_KERNEL(ilupython_threaded_operation, ilu_EnterServer(iv->kserver, iv->kclass));
  if (iv->kobj != 0)
    {
      /* another thread may have already created a new Python object to replace this one,
	 and registered it with the kernel.  We don't want to overwrite the new Python object
	 with a NULL accidentally, so we check the pointers before calling ilu_RegisterLanguageSpecificObject */

      void *inst = ilu_GetLanguageSpecificObject(iv->kobj, _ilupython_LangIndex); /* non-blocking */
      if ((inst == iv->pyobj) && (iv->pyobj != NULL))
	CALL_KERNEL(ilupython_threaded_operation, ilu_RegisterLanguageSpecificObject(iv->kobj, NULL, _ilupython_LangIndex));
      iv->kobj = 0;
    }
  ilu_ExitServer(iv->kserver, iv->kclass);
}
	PyMem_DEL(o);
}

PyObject *
iv_New(void)
{
	IvObject *	v	= PyObject_NEW(IvObject, &Iv_Type);

	if (v == 0)
		return PyErr_NoMemory();
	v->kclass = 0;
	v->kserver = 0;
	v->kobj = 0;
	v->publish_proof = 0;
	v->pyobj = 0;
#if defined(WIN32)
	v->creatorPid = _getpid();
#else
	v->creatorPid = getpid();
#endif
	return (PyObject *) v;
}

static int
iluiv_print(PyObject *o, FILE *fp, int flags)
{
	IvObject *	iv	= (IvObject *) o;

	fprintf(fp, "<instvars:  kclass=0x%p  kserver=0x%p  kobj=0x%p%s>",
		iv->kclass, iv->kserver, iv->kobj,
		(iv->publish_proof != NULL) ? " P" : "");
	return 0;
}

static PyObject *
iluiv_Server(PyObject *self, PyObject *args)
{
  PyObject *	server;
  IvObject *	p	= (IvObject *) self;

  if (!PyArg_Parse(args, ""))
    return 0;
  server = ilusv_FromServer (p->kserver);
  return server;
}

static PyMethodDef iluiv_methods[] =
{
  { "Server",		iluiv_Server		},
  { 0						}
};

static PyObject *
iluiv_getattr(PyObject *self, char *name)
{
  return Py_FindMethod(iluiv_methods, self, name);
}

PyTypeObject	Iv_Type =
{
	PyObject_HEAD_INIT(&PyType_Type)
	0,
	"instvars",
	sizeof(IvObject),
	0,
	iluiv_dealloc,		/*tp_dealloc*/
	iluiv_print,		/*tp_print*/
	iluiv_getattr,		/*tp_getattr*/
	0,			/*tp_setattr*/
	0,			/*tp_compare*/
	0,			/*tp_repr*/
	0,			/*tp_as_number*/
	0,			/*tp_as_sequence*/
	0,			/*tp_as_mapping*/
	0,			/*tp_hash*/
};
