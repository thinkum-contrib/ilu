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

$Id: iluszobject.c,v 1.4 1999/08/03 01:55:40 janssen Exp $
*/

/* from ILU */
#include "iluxport.h"

/* local */
#include "python.h"
#include "iluszobject.h"

PyObject *
ilusz_FromSerializer(ilu_Serializer s)
{
	IluszObject *	p	= PyObject_NEW(IluszObject, &Ilusz_Type);

	p->serializer = s;
	return (PyObject *) p;
}

ilu_Serializer
  ilusz_AsSerializer(IluszObject *p)
{
  return (p->serializer);
}

static void
ilusz_dealloc(PyObject *o)
{
  ilu_Error kerr;
  ilu_ReleaseSerializer(ilusz_AsSerializer((IluszObject *)o), &kerr);
  if (ILU_ERRNOK(kerr)) { ILU_HANDLED(kerr); };

  PyMem_DEL(o);
}

#ifdef BATCHING_TRANSPORT
static PyObject *
  ilusz_flush (PyObject *self, PyObject *args)
{
  IluszObject *		serializer = (IluszObject *) self;
  ilu_Error		lerr;

  if (!ilu_batching_Flush (ilusz_AsSerializer(serializer), &lerr)) {
    char buf[1000];
    _ilupython_formErrDescription (buf, &lerr);
    ilu_DebugPrintf ("ilu: ilu_batching_Flush (%p) signals <%s>.\n",
		     ilusz_AsSerializer(serializer), buf);
    ILU_HANDLED(lerr);
    return 0;
  } else {
    Py_INCREF(Py_None);
    return Py_None;
  }
}
#endif /* BATCHING_TRANSPORT */

static PyMethodDef ilusz_methods[] =
{
#ifdef BATCHING_TRANSPORT
	{ "flush",		ilusz_flush		},
#endif /* BATCHING_TRANSPORT */
	{ 0						}
};

static PyObject *
ilusz_getattr(PyObject *self, char *name)
{
	return Py_FindMethod(ilusz_methods, self, name);
}

PyTypeObject	Ilusz_Type =
{
	PyObject_HEAD_INIT(&PyType_Type)
	0,
	"ilu_Serializer",
	sizeof(IluszObject),
	0,
	ilusz_dealloc,		/*sz_dealloc*/
	0,			/*sz_print*/
	ilusz_getattr,		/*sz_getattr*/
	0,			/*sz_setattr*/
	0,			/*sz_compare*/
	0,			/*sz_repr*/
	0,			/*sz_as_number*/
	0,			/*sz_as_sequence*/
	0,			/*sz_as_mapping*/
	0,			/*sz_hash*/
};
