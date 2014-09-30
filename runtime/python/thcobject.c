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

$Id: thcobject.c,v 1.9 1999/08/03 01:55:32 janssen Exp $
*/

/* from ILU */
#include "iluxport.h"

/* local */
#include "python.h"
#include "iluftobject.h"
#include "thcobject.h"

static void
thc_dealloc(PyObject *o)
{
	PyMem_DEL(o);
}

PyObject *
thc_New(ilu_FineTime time, TimerHandler handler, ilu_private rock)
{
	ThcObject *	p	= PyObject_NEW(ThcObject, &Thc_Type);

	if (p == 0)
		return PyErr_NoMemory();
	p->time = time;
	p->handler = handler;
	p->rock = rock;
	return (PyObject *) p;
}

static int
thc_print(PyObject *o, FILE *fp, int flags)
{
	ThcObject *	p	= (ThcObject *) o;

	fprintf(fp, "<timerHandlerClosure:  time=%ld+%lu/%lu  handler=0x%p, rock=0x%p>",
		(long) p->time.ft_s, (unsigned long) p->time.ft_t,
		(unsigned long) ilu_FineTimeRate , p->handler, p->rock);
	return 0;
}

static PyObject *
thc_call(PyObject *self, PyObject *args)
{
	ThcObject *	p = (ThcObject *) self;

	(*p->handler)(p->rock);
	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *
thc_time(PyObject *self, PyObject *args)
{
	ThcObject *	p = (ThcObject *) self;

	return iluft_FromFineTime(p->time);
}

static PyMethodDef thc_methods[] =
{
	{ "call",		thc_call		},
	{ "time",		thc_time		},
	{ NULL						}
};

static PyObject *
thc_getattr(PyObject *self, char *name)
{
	return Py_FindMethod(thc_methods, self, name);
}

PyTypeObject	Thc_Type =
{
	PyObject_HEAD_INIT(&PyType_Type)
	0,
	"timerHandlerClosure",
	sizeof(ThcObject),
	0,
	thc_dealloc,		/*tp_dealloc*/
	thc_print,		/*tp_print*/
	thc_getattr,		/*tp_getattr*/
	0,			/*tp_setattr*/
	0,			/*tp_compare*/
	0,			/*tp_repr*/
	0,			/*tp_as_number*/
	0,			/*tp_as_sequence*/
	0,			/*tp_as_mapping*/
	0,			/*tp_hash*/
};
