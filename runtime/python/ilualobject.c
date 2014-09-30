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

$Id: ilualobject.c,v 1.14 1999/08/03 01:55:31 janssen Exp $
*/

/* from ILU */
#include "iluxport.h"

/* local */
#include "python.h"
#include "ilualobject.h"
#include "iluftobject.h"
#include "pythonthreads.h"

static void
clearProcAndArgList(IlualObject *p)
{
	if (p->proc)
	{
		Py_DECREF(p->proc);
		p->proc = 0;
	}
	if (p->argTuple)
	{
		Py_DECREF(p->argTuple);
		p->argTuple = 0;
	}
}

static void
setProcAndArgList(IlualObject *p, PyObject *proc, PyObject *argTuple)
{
	if (p->proc)
	{
		Py_DECREF(p->proc);
		p->proc = 0;
	}
	p->proc = proc;
	Py_INCREF(p->proc);

	if (p->argTuple)
	{
		Py_DECREF(p->argTuple);
		p->argTuple = 0;
	}
	p->argTuple = argTuple;
	Py_INCREF(p->argTuple);
}

static void
ilual_dealloc(PyObject *o)
{
	IlualObject *	p	= (IlualObject *) o;

	if (p->alarm)
	{
		ilu_UnsetAlarm(p->alarm);
		/* free(p->alarm); ? */
	}
	clearProcAndArgList(p);
	PyMem_DEL(o);
}

PyObject *
ilual_New(void)
{
	IlualObject *	p	= PyObject_NEW(IlualObject, &Ilual_Type);

	if (p == 0)
		return 0;
	p->proc = 0;
	p->argTuple = 0;
	if ((p->alarm = ilu_CreateAlarm()) == 0)
	{
		Py_DECREF(p);
		PyErr_SetString(PyExc_MemoryError, "ilu_CreateAlarm failed");
		return 0;
	}
	return (PyObject *) p;
}

static void
alarmProc(ilu_private rock)
{
	IlualObject *	p	= (IlualObject *) rock;
	PyObject *	result;
	PyObject * proc = ((IlualObject *) rock)->proc;
	PyObject * args = ((IlualObject *) rock)->argTuple;

/*
	if ((proc == 0) || (args == 0))
	  return;
*/
	_ilu_Assert((proc != 0) && (args != 0),
		    "bad alarm data structure in Python runtime");
	p->proc = 0;
	p->argTuple = 0;
	NEW_THREAD_ENTER;
	result = PyEval_CallObject(proc, args);
	Py_DECREF(proc);
	Py_DECREF(args);
	FINISHED_THREAD_EXIT;
	Py_XDECREF(result);
}

static PyObject *
ilual_set(PyObject *self, PyObject *args)
{
	IlualObject *	p	= (IlualObject *) self;
	ilu_FineTime	ft;
	PyObject *	time;
	PyObject *	proc;
	PyObject *	argTuple;

	if (!PyArg_Parse(args, "(OOO)", &time, &proc, &argTuple))
		return 0;
	if (iluft_Check(time))
		ft = iluft_AsFineTime(time);
	else if (PyFloat_Check(time))
		ft = ilu_FineTime_FromDouble(PyFloat_AsDouble(time));
	else if (PyInt_Check(time))
	{
		ft.ft_s = PyInt_AsLong(time);
		ft.ft_t = 0;
	}
	else
	{
		PyErr_SetString(PyExc_TypeError,
			"arg1 should be ilu_FineTime, float, or int");
		return 0;
	}
	if (!PyCallable_Check(proc))
	{
		PyErr_SetString(PyExc_TypeError, "arg2 should be callable");
		return 0;
	}
	if (!PyTuple_Check(argTuple))
	{
		PyErr_SetString(PyExc_TypeError, "arg3 should be tuple");
		return 0;
	}
	setProcAndArgList(p, proc, argTuple);
	ilu_SetAlarm(p->alarm, ft, alarmProc, (ilu_private) p);
	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *
ilual_unset(PyObject *self, PyObject *args)
{
	IlualObject *	p	= (IlualObject *) self;

	if (!PyArg_Parse(args, ""))
		return 0;
	ilu_UnsetAlarm(p->alarm);
	clearProcAndArgList(p);
	Py_INCREF(Py_None);
	return Py_None;
}

static PyMethodDef ilual_methods[] =
{
	{ "set",		ilual_set		},
	{ "unset",		ilual_unset		},
	{ 0						}
};

static PyObject *
ilual_getattr(PyObject *self, char *name)
{
	return Py_FindMethod(ilual_methods, self, name);
}

PyTypeObject	Ilual_Type =
{
	PyObject_HEAD_INIT(&PyType_Type)
	0,
	"ilu_Alarm",
	sizeof(IlualObject),
	0,
	ilual_dealloc,		/*tp_dealloc*/
	0,			/*tp_print*/
	ilual_getattr,		/*tp_getattr*/
	0,			/*tp_setattr*/
	0,			/*tp_compare*/
	0,			/*tp_repr*/
	0,			/*tp_as_number*/
	0,			/*tp_as_sequence*/
	0,			/*tp_as_mapping*/
	0,			/*tp_hash*/
};
