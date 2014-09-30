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

$Id: ilupiobject.c,v 1.10 1999/08/03 01:55:37 janssen Exp $
*/

/* from ILU */
#include "iluxport.h"

/* local */
#include "python.h"
#include "ilupiobject.h"
#include "ilutpobject.h"

static void
ilupi_dealloc(PyObject *o)
{
  IluPiObject *p = (IluPiObject *) o;

  if (p->pickle.pi_bytes != ILU_NIL)
    ilu_free(p->pickle.pi_bytes);
  PyMem_DEL(o);
}

PyObject *
  ilupi_FromPickle(ilu_Pickle pickle)
{
  IluPiObject *	p	= PyObject_NEW(IluPiObject, &IluPi_Type);

  p->pickle.pi_len = pickle.pi_len;
  p->pickle.pi_bytes = pickle.pi_bytes;
  return (PyObject *) p;
}

ilu_Pickle
  ilupi_AsPickle(PyObject *o)
{
  IluPiObject *	p	= (IluPiObject *) o;

  return p->pickle;
}

static PyObject *
  ilupi_getattr(PyObject *self, char *name)
{
  IluPiObject *p = (IluPiObject *) self;

  if (strcmp(name, "len") == 0)
    return PyInt_FromLong(p->pickle.pi_len);
  else if (strcmp(name, "bytes") == 0)
    return PyString_FromStringAndSize((char *) p->pickle.pi_bytes, p->pickle.pi_len);
  else if (strcmp(name, "type") == 0) {
    ilu_Error lerr;
    ilu_string type_id;
    type_id = ilu_PickleType (p->pickle, &lerr);
    if (ILU_ERRNOK(lerr)) {
      return _ilupython_RaiseILUGeneralError(&lerr);
    } else 
      return PyString_FromString(type_id);
  } else if (strcmp(name, "version") == 0) {
    long v = ((p->pickle.pi_bytes[0] >> 5) & 0x7);
    return PyInt_FromLong(v);
  } else {
    PyErr_SetString(PyExc_AttributeError, name);
    return 0;
  }
}

static int
  ilupi_compare(PyObject *self, PyObject *other)
{
  IluPiObject *p = (IluPiObject *) self;
  IluPiObject *q = (IluPiObject *) other;

  if ((!IluPi_Check(other)) || (p->pickle.pi_len != q->pickle.pi_len))
    return 1;
  return (memcmp(p->pickle.pi_bytes, q->pickle.pi_bytes, p->pickle.pi_len));
}

PyTypeObject	IluPi_Type =
{
	PyObject_HEAD_INIT(&PyType_Type)
	0,
	"ilu_Pickle",
	sizeof(IluPiObject),
	0,
	ilupi_dealloc,		/*ob_dealloc*/
	0,			/*ob_print*/
	ilupi_getattr,		/*ob_getattr*/
	0,			/*ob_setattr*/
	ilupi_compare,		/*ob_compare*/
	0,			/*ob_repr*/
	0,			/*ob_as_number*/
	0,			/*ob_as_sequence*/
	0,			/*ob_as_mapping*/
	0,			/*ob_hash*/
};
