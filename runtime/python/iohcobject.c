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

$Id: iohcobject.c,v 1.8 1999/08/03 01:55:30 janssen Exp $
*/

/* from ILU */
#include "iluxport.h"

/* local */
#include "python.h"
#include "iohcobject.h"

static void
iohc_dealloc(PyObject *o)
{
	PyMem_DEL(o);
}

PyObject *
iohc_New(int fileno, ilu_IOHandler handler, ilu_private rock)
{
	IohcObject *	p	= PyObject_NEW(IohcObject, &Iohc_Type);

	if (p == 0)
		return 0;
	p->fileno = fileno;
	p->handler = handler;
	p->rock = rock;
	return (PyObject *) p;
}

static int
iohc_print(PyObject *o, FILE *fp, int flags)
{
	IohcObject *	p	= (IohcObject *) o;

	fprintf(fp, "<ioHandlerClosure:  fileno=%d  handler=0x%p  rock=0x%p>",
		p->fileno, p->handler, p->rock);
	return 0;
}

static PyObject *
iohc_call(PyObject *self, PyObject *args)
{
	IohcObject *	p = (IohcObject *) self;

	(*p->handler)(p->fileno, p->rock);
	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *
iohc_fileno(PyObject *self, PyObject *args)
{
	IohcObject *	p = (IohcObject *) self;

	return PyInt_FromLong(p->fileno);
}

static PyMethodDef iohc_methods[] =
{
	{ "call",		iohc_call		},
	{ "fileno",		iohc_fileno		},
	{ NULL						}
};

static PyObject *
iohc_getattr(PyObject *self, char *name)
{
	return Py_FindMethod(iohc_methods, self, name);
}

PyTypeObject	Iohc_Type =
{
	PyObject_HEAD_INIT(&PyType_Type)
	0,
	"ioHandlerClosure",
	sizeof(IohcObject),
	0,
	iohc_dealloc,		/*tp_dealloc*/
	iohc_print,		/*tp_print*/
	iohc_getattr,		/*tp_getattr*/
	0,			/*tp_setattr*/
	0,			/*tp_compare*/
	0,			/*tp_repr*/
	0,			/*tp_as_number*/
	0,			/*tp_as_sequence*/
	0,			/*tp_as_mapping*/
	0,			/*tp_hash*/
};
