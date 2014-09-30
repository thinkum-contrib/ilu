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

$Id: ilutpobject.c,v 1.7 1999/08/03 01:55:36 janssen Exp $
*/

/* from ILU */
#include "iluxport.h"

/* local */
#include "python.h"
#include "ilutpobject.h"

static void
ilutp_dealloc(PyObject *o)
{
	PyMem_DEL(o);
}

PyObject *
ilutp_FromType(ilu_Type t)
{
	IlutpObject *	p	= PyObject_NEW(IlutpObject, &Ilutp_Type);

	p->type = t;
	return (PyObject *) p;
}

ilu_Type
  ilutp_AsType(IlutpObject *p)
{
  return (p->type);
}

static int
ilutp_print(PyObject *o, FILE *fp, int flags)
{
	IlutpObject *	p	= (IlutpObject *) o;

	if (p->type == ILU_NIL)
	  fprintf (fp, "<ilu_Type NIL>");
	else if (type_interface_brand(p->type) != ILU_NIL)
	  fprintf (fp, "<ilu_Type %s(brand=%s).%s>",
		   type_interface_name(p->type),
		   type_interface_brand(p->type),
		   type_name(p->type));
	else
	  fprintf (fp, "<ilu_Type %s.%s>",
		   type_interface_name(p->type), type_name(p->type));
	return 0;
}

static PyObject *
ilutp_getattr(PyObject *self, char *name)
{
  IlutpObject *p = (IlutpObject *) self;

  if (strcmp(name, "uid") == 0)
    return PyString_FromString(type_uid(p->type));
  else if (strcmp(name, "name") == 0)
    return PyString_FromString(type_name(p->type));
  else if (strcmp(name, "interface") == 0) {
    if (type_interface_brand(p->type) == NULL)
      return PyString_FromString(type_interface_name(p->type));
    else {
      PyObject *	i = PyTuple_New(2);
      if (i == 0)
	return 0;
      PyTuple_SetItem(i, 0, PyString_FromString(type_interface_name(p->type)));
      PyTuple_SetItem(i, 1, PyString_FromString(type_interface_brand(p->type)));
      return i;
    }
  } else {
    PyErr_SetString(PyExc_AttributeError, name);
    return 0;
  }
}

static PyObject *
ilutp_repr(PyObject *self)
{
  char buf[1000];
  IlutpObject *p = (IlutpObject *) self;

  if (p->type == ILU_NIL)
    strcpy (buf, "<ilu_Type NIL>");
  else if (type_interface_brand(p->type) != ILU_NIL)
    sprintf (buf, "<ilu_Type %s(brand=%s).%s>",
	     type_interface_name(p->type),
	     type_interface_brand(p->type),
	     type_name(p->type));
  else
    sprintf (buf, "<ilu_Type %s.%s>",
	     type_interface_name(p->type),
	     type_name(p->type));
  return PyString_FromString(buf);
}

static int
ilutp_compare(PyObject *self, PyObject *other)
{
  IlutpObject *p = (IlutpObject *) self;
  IlutpObject *q = (IlutpObject *) other;

  if (!ilutp_Check(other))
    return 1;
  else if (p->type < q->type)
    return -1;
  else if (p->type > q->type)
    return 1;
  else
    return 0;
}

PyTypeObject	Ilutp_Type =
{
	PyObject_HEAD_INIT(&PyType_Type)
	0,
	"ilu_Type",
	sizeof(IlutpObject),
	0,
	ilutp_dealloc,		/*tp_dealloc*/
	ilutp_print,		/*tp_print*/
	ilutp_getattr,		/*tp_getattr*/
	0,			/*tp_setattr*/
	ilutp_compare,		/*tp_compare*/
	ilutp_repr,		/*tp_repr*/
	0,			/*tp_as_number*/
	0,			/*tp_as_sequence*/
	0,			/*tp_as_mapping*/
	0,			/*tp_hash*/
};
