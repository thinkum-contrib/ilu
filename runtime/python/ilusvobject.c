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

$Id: ilusvobject.c,v 1.24 1999/08/03 01:55:28 janssen Exp $
*/

/* from ILU */
#include "iluxport.h"

/* local */
#include "python.h"
#include "ilusvobject.h"
#include "iluszobject.h"

static void
ilusv_dealloc(PyObject *o)
{
	PyMem_DEL(o);
}

PyObject *
ilusv_FromServer(ilu_Server kserver)
{
	IlusvObject *	v	= PyObject_NEW(IlusvObject, &Ilusv_Type);

	if (v == 0)
		return 0;
	v->kserver = kserver;
	return (PyObject *) v;
}

static int
ilusv_print(PyObject *self, FILE *fp, int flags)
{
	IlusvObject *	v	= (IlusvObject *) self;
	char *		id	= ilu_IDOfServer(v->kserver);

	fprintf(fp, "<ilu_Server:  id=\"%s\"  kserver=0x%p>",
		id ? id : "", v->kserver);
	return 0;
}

static PyObject *
  ilusv_id(PyObject *self, PyObject *args)
{
	IlusvObject *	v	= (IlusvObject *) self;
	char *		id;

	if (!PyArg_Parse(args, ""))
		return 0;
	if ((id = ilu_IDOfServer(v->kserver)) == 0)
	{
		Py_INCREF(Py_None);
		return Py_None;
	}
	return PyString_FromString(id);
}

static PyObject *
  ilusv_AddPort(PyObject *self, PyObject *args)
{
  IlusvObject *		server = (IlusvObject *) self;
  ilu_TransportInfo	trans = ILU_NIL;
  ilu_ProtocolInfo	proto;
  PyObject *		result;
  ilu_Server		kserver;
  ilu_Port		newport;
  PyObject *		p_private;

  p_private = Py_None;
  if (!(PyErr_Clear(), PyArg_ParseTuple(args, "O&z|O", _ilupython_convTinfo, &trans, &proto, &p_private))) {
    return 0;
  }
  if (_ilupython_createPort(server->kserver, trans, proto, ILU_NIL,
			    PyObject_IsTrue(p_private) ? ilu_FALSE : ilu_TRUE) == 0) {
    PyErr_SetString(_ilupython_GeneralError, "unable to create specified port on server");
    if (trans) ilu_free(trans);
    return 0;
  } else {
    /* don't do anything with the returned port pointer */
    result = Py_None;
    Py_INCREF(result);
    if (trans) ilu_free(trans);
    return result;
  }  
}

static PyObject *
  ilusv_NativeCinfo(PyObject *self, PyObject *args)
{
  IlusvObject *		server = (IlusvObject *) self;
  ilu_TransportInfo	trans = ILU_NIL;
  ilu_ProtocolInfo	proto = ILU_NIL;
  PyObject *		p_private = Py_None;
  PyObject *		result;
  PyObject *		transTuple;
  ilu_Error		err;
  int			i;
  ilu_boolean		status;

  p_private = Py_None;
  if (args != ILU_NIL) {
    p_private = args;
  }
  ilu_EnterServerMutex(server->kserver, ilu_FALSE, &err);
  if (ILU_ERRNOK(err)) {
    return _ilupython_RaiseILUGeneralError(&err);
  }
  status = ilu_ServerCInfo(server->kserver,
			   PyObject_IsTrue(p_private) ? ilu_FALSE : ilu_TRUE,
			   &proto, &trans, &err);
  if (ILU_ERRNOK(err)) {
    return _ilupython_RaiseILUGeneralError(&err);
  }
  ilu_ExitServerMutex(server->kserver, ilu_FALSE, &err);
  ILU_HANDLED(err);
  if (status) {
    result = PyTuple_New(2);
    PyTuple_SetItem(result, 0, PyString_FromString(proto));
    for (i = 0;  trans[i] != ILU_NIL;  i++)
      ;
    transTuple = PyTuple_New(i);
    for (i = 0;  trans[i] != ILU_NIL;  i++)
      PyTuple_SetItem(transTuple, i, PyString_FromString(trans[i]));
    PyTuple_SetItem(result, 1, transTuple);
    return result;
  } else {
    Py_INCREF(Py_None);
    return Py_None;
  }
}

static PyObject *
  ilusv_AddCinfo(PyObject *self, PyObject *args)
{
  IlusvObject *		server = (IlusvObject *) self;
  ilu_TransportInfo	trans = ILU_NIL;
  ilu_ProtocolInfo	proto = ILU_NIL;
  ilu_Error		err;

  if (args == ILU_NIL) {
    PyErr_SetString(PyExc_TypeError, "no arguments!");
    return 0;
  }    
  if (!PyArg_ParseTuple(args, "sO&", &proto, _ilupython_convTinfo, &trans))
    return 0;
  ilu_AddCInfo(server->kserver, proto, trans, &err);
  if (ILU_ERRNOK(err)) {
    return _ilupython_RaiseILUGeneralError(&err);
  }
  Py_INCREF(Py_None);
  return Py_None;
}

static PyObject *
  ilusv_CreateSerializer(PyObject *self, PyObject *args)
{
  IlusvObject *		server = (IlusvObject *) self;
  PyObject *serializer;

  if ((serializer = _ilupython_createSerializer(server->kserver)) == 0) {
    PyErr_SetString(_ilupython_GeneralError, "unable to create new serializer on server");
  }
  return serializer;
}

static void
  ServerRelocateShim (ilu_Server server,
		      ilu_private rock,
		      ilu_Error *err)
{
  PyObject *callback = (PyObject *) rock;
  ilu_ProtocolInfo pinfo = ILU_NIL;
  ilu_TransportInfo tinfo = ILU_NIL;
  PyObject *result;
  PyObject *argsTuple;

  if ((argsTuple = Py_BuildValue("()")) == ILU_NIL) {
    ILU_ERR_CONS1(internal, err, minor, ilu_im_callFail, 0);
    return;
  };
  result = PyEval_CallObject(callback, argsTuple);
  Py_DECREF(argsTuple);
  if (result == 0) {
    _ilupython_handleCalloutException("server relocate procedure", ILU_NIL);
    ILU_ERR_CONS1(internal, err, minor, ilu_im_callFail, 0);
  } else {
    if (result == Py_None) {
      ILU_CLER(*err);
    } else {
      if (!PyTuple_Check(result) || PyTuple_Size(result) != 2)
	ILU_ERR_CONS1(internal, err, minor, ilu_im_badRelocVals, 0);
      else if (!PyString_Check(PyTuple_GetItem(result, 0)))
	ILU_ERR_CONS1(internal, err, minor, ilu_im_badRelocPinfo, 0);
      else if (!PyTuple_Check(PyTuple_GetItem(result, 1)))
	ILU_ERR_CONS1(internal, err, minor, ilu_im_badRelocTinfo, 0);
      else {
	pinfo = ilu_StrdupE(PyString_AsString(PyTuple_GetItem(result, 0)), err);
	if (ILU_ERROK(*err)) {
	  if ((_ilupython_convTinfo(PyTuple_GetItem(result, 1), &tinfo) == 0) ||
	      (tinfo == ILU_NIL)) {
	    _ilupython_handleCalloutException("server relocate procedure", ILU_NIL);
	    ILU_ERR_CONS1(internal, err, minor, ilu_im_callFail, 0);
	  } else {
	    ILU_ERR_CONS3(relocate,err,rel_scope,ilu_relocate_conn,rel_pinfo,pinfo,rel_tinfo,tinfo,0);
	  }
	}
      }
    }
    Py_DECREF(result);
  }
}

static PyObject *
  ilusv_SetRelocator(PyObject *self, PyObject *args)
{
  IlusvObject *		server = (IlusvObject *) self;
  PyObject *callback, *old;
  ilu_Error lerr;

  if (!PyArg_Parse(args, "O", &callback))
    return 0;
  if (callback == Py_None)
    callback = ILU_NIL;
  else if (!PyCallable_Check(callback))
    {
      PyErr_SetString(PyExc_TypeError, "arg1 should be function or method");
      return 0;
    }
  old =  (PyObject *) ilu_SetServerRelocateProc(server->kserver, ServerRelocateShim,
				  (ilu_refany) callback, &lerr);
  if (ILU_ERRNOK(lerr)) {
    return _ilupython_RaiseILUGeneralError(&lerr);
  }
  if (old != ILU_NIL)
    return old;
  else {
    Py_INCREF(Py_None);
    return Py_None;
  }
}

static PyMethodDef ilusv_methods[] =
{
	{ "id",			ilusv_id		},
	{ "addPort",		ilusv_AddPort		},
	{ "createSerializer",	ilusv_CreateSerializer	},
	{ "nativeCInfo",	ilusv_NativeCinfo	},
	{ "addCInfo",		ilusv_AddCinfo		},
	{ "setRelocator",	ilusv_SetRelocator	},
	{ 0						}
};

static PyObject *
ilusv_getattr(PyObject *self, char *name)
{
	return Py_FindMethod(ilusv_methods, self, name);
}

PyTypeObject	Ilusv_Type =
{
	PyObject_HEAD_INIT(&PyType_Type)
	0,
	"ilu_Server",
	sizeof(IlusvObject),
	0,
	ilusv_dealloc,		/*tp_dealloc*/
	ilusv_print,		/*tp_print*/
	ilusv_getattr,		/*tp_getattr*/
	0,			/*tp_setattr*/
	0,			/*tp_compare*/
	0,			/*tp_repr*/
	0,			/*tp_as_number*/
	0,			/*tp_as_sequence*/
	0,			/*tp_as_mapping*/
	0,			/*tp_hash*/
};
