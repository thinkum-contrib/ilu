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

$Id: iluclobject.c,v 1.29 1999/08/03 01:55:43 janssen Exp $
*/

/* from ILU */
#include "iluxport.h"

/* local */
#include "python.h"
#include "iluclobject.h"

static void
ilucl_deallocSkeletons(IluclObject *cl)
{
	if (cl->skeletons)
	{
	  int	i;

	  for (i = 0; i < cl->nmethods; i++)
	    Py_XDECREF(cl->skeletons[i]);
	}
	PyMem_XDEL(cl->skeletons);
}

static void
ilucl_dealloc(PyObject *o)
{
	IluclObject *	cl = (IluclObject *) o;

	ilucl_deallocSkeletons(cl);
	ilu_free(cl->id);

	PyMem_DEL(o);
}

#define CLEAR(ptr, n)	memset((char *) ptr, 0, (n) * sizeof(*(ptr)))

static char *lstrdup(char *s)
{
  char *s2;

  if (s == ILU_NIL)
    return ILU_NIL;
  s2 = (char *) ilu_must_malloc(strlen(s) + 1);
  strcpy (s2, s);
  return s2;
}

static int
  setMethodEntry(ilu_Class cl, ilu_cardinal method, PyObject *t)
{
  char *	name;
  char *	lname;
  int		id;
  char		cacheable;
  char		asynchronous;
  PyObject *	exceptionsTuple;
  PyObject *	argsTuple;
  ilu_string	return_type = ILU_NIL;
  ilu_Exception *	evec;
  int		i;
  ilu_Method	m;
  ilu_Error	err = ILU_INIT_NO_ERR;

  if (!PyArg_Parse(t, "(sibbzOO)", &name, &id, &cacheable, &asynchronous,
		   &return_type, &argsTuple, &exceptionsTuple))
    return -1;

  lname = lstrdup(name);
  evec = PyMem_NEW(char *, PyTuple_Size(exceptionsTuple));
  for (i = 0;  i < PyTuple_Size(exceptionsTuple); i++)
    {
      PyObject *	e = PyTuple_GetItem(exceptionsTuple, i);

      if (PyTuple_Check(e))
	{
	  PyObject * ename = ILU_NIL;
	  PyObject * etype = ILU_NIL;
	  Py_INCREF(e);
	  ename = PyTuple_GetItem(e, 0);
	  if (PyTuple_Size(e) > 1)
	    etype = PyTuple_GetItem(e, 1);
	  evec[i] = ilu_DefineException (ILU_NIL, PyString_AsString(ename),
					 (etype != ILU_NIL) ? PyString_AsString(etype) : ILU_NIL,
					 &err);
	  if (ILU_ERRNOK(err))
	    {
	      _ilupython_RaiseILUGeneralError(&err);
	      return -1;
	    }
	}
      else
	{
	  PyMem_XDEL(lname);
	  PyMem_XDEL(evec);
	  (void) PyErr_BadArgument();
	  return -1;
	}
    }
  m = ilu_DefineMethod (cl,
			method,
			lname,
			id,
			BOOLEAN(cacheable),
			BOOLEAN(asynchronous),
			PyTuple_Size(exceptionsTuple), evec,
			PyTuple_Size(argsTuple),
			return_type,
			&err);
  PyMem_XDEL(lname);
  ILU_ERR_SWITCH(err) {
    ILU_SUCCESS_CASE ;
    ILU_ERR_CASE(no_memory, evp)
      {
	(void) PyErr_NoMemory();
	return -1;
      }
    ILU_ERR_ELSE
      {
	_ilupython_RaiseILUGeneralError(&err);
	return -1;
      }
  } ILU_ERR_ENDSWITCH;
  
  for (i = 0;  i < PyTuple_Size(argsTuple); i++)
    {
      PyObject *	e = PyTuple_GetItem(argsTuple, i);

      if (PyTuple_Check(e))
	{
	  if (PyTuple_Size(e) != 4) {
	    (void) PyErr_BadArgument();
	    return -1;
	  } else {
	    PyObject * aname = ILU_NIL;
	    PyObject * atype = ILU_NIL;
	    PyObject * adir = ILU_NIL;
	    PyObject * asibling = ILU_NIL;
	    Py_INCREF(e);
	    aname = PyTuple_GetItem(e, 0);
	    adir = PyTuple_GetItem(e, 1);
	    asibling = PyTuple_GetItem(e, 2);
	    atype = PyTuple_GetItem(e, 3);
	    if (PyString_Check(aname) && PyString_Check(atype)
		&& PyInt_Check(adir) && PyInt_Check(asibling) &&
		((PyInt_AsLong(adir) == 0) || (PyInt_AsLong(adir) == 1) || (PyInt_AsLong(adir) == 2))) {
	      ilu_DefineMethodArg(m, i,
				  PyString_AsString(aname),
				  (PyObject_IsTrue(asibling) ? ilu_TRUE : ilu_FALSE),
				  (ilu_ArgDirection) PyInt_AsLong(adir),
				  PyString_AsString(atype),
				  &err);
	      if (ILU_ERRNOK(err))
		{
		  _ilupython_RaiseILUGeneralError(&err);
		  return -1;
		}
	    } else {
	      (void) PyErr_BadArgument();
	      return -1;
	    }
	  }
	} else {
	  (void) PyErr_BadArgument();
	  return -1;
	}
    }

  return 0;
}

PyObject *
ilucl_New(char *name, char *brand, char *uniqueId, char *singleton,
	  int collectible, int optional, char *doc_string,
	  PyObject *methodsTuple, PyObject *superclassTuple,
	  int local, int sealed, PyObject *stateTuple)
{
  IluclObject *	cl = PyObject_NEW(IluclObject, &Ilucl_Type);
  ilu_Class	c;
  int		i;
  ilu_Error	err = ILU_INIT_NO_ERR;

  if (cl == 0)
    return 0;

  CLEAR(&cl->c, 1);
  cl->skeletons = 0;

  if ((c = ilu_FindClassFromID (uniqueId)) == ILU_NIL)
    {
      char **superclass_ids;

      ilu_EnterMutex (ilu_otmu, &err);
      ILU_MUST_BE_SUCCESS(err);

      superclass_ids = PyMem_NEW(char *, PyTuple_Size(superclassTuple));
      for (i = 0; i < PyTuple_Size(superclassTuple); i++)
	{
	  IluclObject *	sc	=
	    (IluclObject *) PyTuple_GetItem(superclassTuple, i);
		
	  if (!ilucl_Check(sc))
	    {
	      ilucl_dealloc((PyObject *) cl);
	      (void) PyErr_BadArgument();
	      return 0;
	    }
	  Py_INCREF(sc);
	  superclass_ids[i] = sc->id;
	}

      /* create new class struct */
      cl->c = ilu_DefineObjectType (name,
				    brand,
				    uniqueId,
				    singleton,
				    BOOLEAN(optional),
				    BOOLEAN(collectible),
				    doc_string,
				    PyTuple_Size(methodsTuple),
				    PyTuple_Size(superclassTuple),
				    superclass_ids,
#ifdef ILU_HTTPNG_OBJECTS
				    (stateTuple == ILU_NIL) ? 0 : PyTuple_Size(stateTuple),
				    BOOLEAN(local),
				    BOOLEAN(sealed),
#endif
				    &err);
      ILU_ERR_SWITCH(err) {
	ILU_SUCCESS_CASE ;
	ILU_ERR_CASE(no_memory, evp)
	  {
	    ilucl_dealloc((PyObject *) cl);
	    (void) PyErr_NoMemory();
	    return 0;
	  }
	ILU_ERR_ELSE
	  {
	    ilucl_dealloc((PyObject *) cl);
	    return _ilupython_RaiseILUGeneralError(&err);
	  }
      } ILU_ERR_ENDSWITCH;

      for (i = 0; i < PyTuple_Size(methodsTuple); i++)
	{
	  if (setMethodEntry(cl->c, i,
			     PyTuple_GetItem(methodsTuple, i)) < 0)
	    {
	      ilucl_dealloc((PyObject *) cl);
	      return 0;
	    }
	}

#ifdef ILU_HTTPNG_OBJECTS
      for (i = 0;  (stateTuple != ILU_NIL) && (i < PyTuple_Size(stateTuple));  i++)
	{
	  PyObject *field, *name, *typeid;
	  field = PyTuple_GetItem(stateTuple, i);
	  if (!PyTuple_Check(field)) {
	    ilucl_dealloc ((PyObject *) cl);
	    return 0;
	  };
	  name = PyTuple_GetItem(field, 0);
	  if (!PyString_Check(name)) {
	    ilucl_dealloc ((PyObject *) cl);
	    return 0;
	  };
	  typeid = PyTuple_GetItem(field, 1);
	  if (!PyString_Check(typeid)) {
	    ilucl_dealloc ((PyObject *) cl);
	    return 0;
	  };
	  if (!ilu_DefineObjectState(cl->c, i,
				     PyString_AsString(name),
				     PyString_AsString(typeid),
				     &err)) {
	    ilucl_dealloc ((PyObject *) cl);
	    return 0;
	  };
	}
#endif

      if (!ilu_ObjectTypeDefined(cl->c, &err))
	{
	  ilucl_dealloc((PyObject *) cl);
	  return _ilupython_RaiseILUGeneralError(&err);
	}
      ilu_ExitMutex (ilu_otmu, ilu_FALSE, &err);
      ILU_MUST_BE_SUCCESS(err);
    }
  else
    {
      cl->c = c;
    }
  cl->id = lstrdup(uniqueId);
  cl->collectible = BOOLEAN(collectible);
  cl->optional = BOOLEAN(optional);
  cl->local = BOOLEAN(local);
  cl->sealed = BOOLEAN(sealed);
  cl->nmethods = PyTuple_Size(methodsTuple);
  return (PyObject *) cl;
}

int
ilucl_RegisterSkeletons(IluclObject *cl, PyObject *skelTuple)
{
  int	i;
  int	count = PyTuple_Size(skelTuple);

  if (count != cl->nmethods)
    {
      PyErr_SetString(PyExc_TypeError, "skel tuple wrong length");
      return -1;
    }

  ilucl_deallocSkeletons(cl);
  cl->skeletons = PyMem_NEW(PyObject *, count);
  if (cl->skeletons == 0)
    {
      (void) PyErr_NoMemory();
      return -1;
    }
  CLEAR(cl->skeletons, count);

  for (i = 0; i < count; i++)
    {
      PyObject *	skel	 = PyTuple_GetItem(skelTuple, i);

      if (!PyCallable_Check(skel))
	{
	  ilucl_deallocSkeletons(cl);
	  PyErr_SetString(PyExc_TypeError,
			  "skel tuple contains noncallable");
	  return -1;
	}
      Py_INCREF(skel);
      cl->skeletons[i] = skel;
    }

  return 0;
}

static char *
booleanImage(int bool)
{
	return bool ? "TRUE" : "FALSE";
}

static char *
notPrefix(int bool)
{
	return bool ? "" : "not ";
}

static int
ilucl_print(PyObject *o, FILE *fp, int flags)
{
	IluclObject *	cl = (IluclObject *) o;
	int		i;
	ilu_string name, brand, id, doc_string = ILU_NIL, singleton;
	ilu_boolean collectible, optional, cacheable, asynchronous, local, sealed;
	ilu_cardinal superclass_count, method_count, mid, ecount;
	ilu_Class *superclasses;
	ilu_Method	methods;
	ilu_Exception	*evec;
	ilu_StubProc	*stubprocs;

	ilu_DataOfClass (cl->c, &name, &brand, &id, &singleton,
			 &collectible, &method_count,
			 &superclass_count, &superclasses, &optional,
#ifdef ILU_HTTPNG_OBJECTS
			 ILU_NIL, ILU_NIL, &local, &sealed,
#endif /* def ILU_HTTPNG_OBJECTS */
			 &methods);
	fprintf(fp, "<ilu_Class at 0x%p\n", cl);
	fprintf(fp, "  name: '%s'\n", name);
	if (brand != ILU_NIL)
	  fprintf(fp, "  brand: '%s'\n", brand);
	fprintf(fp, "  unique_id: '%s'\n", id);

	fprintf(fp, "  singleton: ");
	if (singleton != ILU_NIL)
		fprintf(fp, "'%s'", singleton);
	else
		fprintf(fp, "null");
	putc('\n', fp);

	fprintf(fp, "  collectible: %s\n", booleanImage(collectible));
	fprintf(fp, "  optional: %s\n", booleanImage(optional));

#ifdef ILU_HTTPNG_OBJECTS
	fprintf(fp, "  local: %s\n", booleanImage(local));
	fprintf(fp, "  sealed: %s\n", booleanImage(sealed));
#endif

	fprintf(fp, "  doc_string: ");
	if (doc_string)
		fprintf(fp, "'%s'", doc_string);
	else
		fprintf(fp, "null");
	putc('\n', fp);

	for (i = 0; i < method_count; i++)
	{
	  ilu_DataOfMethod (methods + i, &name, &mid, &cacheable,
			    &asynchronous, &ecount, &evec, &stubprocs);

	  if (i == 0)
	    fprintf(fp, "  methods:\n");
	  fprintf(fp, "    '%s', %lu, %scacheable, %sasync\n",
			name, (unsigned long) mid,
			notPrefix(cacheable),
			notPrefix(asynchronous));
	  if (ecount > 0)
	    {
	      int	j;

	      fprintf(fp, "      raises ");
	      for (j = 0; j < ecount; j++)
		{
		  fprintf(fp, "%s'%s'", j == 0 ? "" : ", ", evec[j]);
		}
	      putc('\n', fp);
	    }
	}

	for (i = 0; i < superclass_count; i++)
	{
	  ilu_string name2, id2;

	  ilu_DataOfClass (superclasses[i], &name2, ILU_NIL, &id2,
			   ILU_NIL, ILU_NIL, ILU_NIL, ILU_NIL,
#ifdef ILU_HTTPNG_OBJECTS
			   ILU_NIL, ILU_NIL, ILU_NIL, ILU_NIL,
#endif
			   ILU_NIL, ILU_NIL, ILU_NIL);
	  if (i == 0)
	    fprintf(fp, "  superclasses:\n");
	  fprintf(fp, "    %s (%s)\n", name2, id2);
	}

	if (cl->skeletons)
	{
	  fprintf(fp, "  skeletons:\n");
	  for (i = 0; i < method_count; i++)
	    {
	      fprintf(fp, "    ");
	      PyObject_Print(cl->skeletons[i], fp, flags);
	      fprintf(fp, "\n");
	    }
	}

	fprintf(fp, "  >");
	return 0;
}

static PyObject *
ilucl_name(PyObject *self, PyObject *args)
{
	IluclObject *	cl	= (IluclObject *) self;
	ilu_string	name;

	if (!PyArg_Parse(args, ""))
		return 0;
	if (!ilu_DataOfClass (cl->c, &name, ILU_NIL, ILU_NIL,
			      ILU_NIL, ILU_NIL, ILU_NIL, ILU_NIL,
			      ILU_NIL, ILU_NIL,
#ifdef ILU_HTTPNG_OBJECTS
			      ILU_NIL, ILU_NIL, ILU_NIL, ILU_NIL,
#endif
			      ILU_NIL))
	  return 0;
	return PyString_FromString(name);
}

static PyObject *
ilucl_id(PyObject *self, PyObject *args)
{
	IluclObject *	cl	= (IluclObject *) self;
	ilu_string id;

	if (!PyArg_Parse(args, ""))
		return 0;
	if (!ilu_DataOfClass (cl->c, ILU_NIL, ILU_NIL, &id,
			      ILU_NIL, ILU_NIL, ILU_NIL, ILU_NIL,
			      ILU_NIL, ILU_NIL,
#ifdef ILU_HTTPNG_OBJECTS
			      ILU_NIL, ILU_NIL, ILU_NIL, ILU_NIL,
#endif
			      ILU_NIL))
	    return 0;
	return PyString_FromString(id);
}

static PyMethodDef ilucl_methods[] =
{
	{ "name",		ilucl_name		},
	{ "id",			ilucl_id		},
	{ 0						}
};

static PyObject *
ilucl_getattr(PyObject *self, char *name)
{
	return Py_FindMethod(ilucl_methods, self, name);
}

PyTypeObject	Ilucl_Type =
{
	PyObject_HEAD_INIT(&PyType_Type)
	0,
	"ilu_Class",
	sizeof(IluclObject),
	0,
	ilucl_dealloc,		/*tp_dealloc*/
	ilucl_print,		/*tp_print*/
	ilucl_getattr,		/*tp_getattr*/
	0,			/*tp_setattr*/
	0,			/*tp_compare*/
	0,			/*tp_repr*/
	0,			/*tp_as_number*/
	0,			/*tp_as_sequence*/
	0,			/*tp_as_mapping*/
	0,			/*tp_hash*/
};
