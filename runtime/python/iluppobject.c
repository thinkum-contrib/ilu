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

$Id: iluppobject.c,v 1.11 1999/08/03 01:55:38 janssen Exp $
*/

/* from ILU */
#include "iluxport.h"

/* local */
#include "python.h"
#include "iluppobject.h"

#ifdef SECURE_TRANSPORT
#include "ilugiobject.h"
#include "ilupygss.h"
#endif

/* names used in Sun RPC UNIX authentication */
static char nameSunRPCAuth[]		= "sunrpc-unix";
static char nameSunRPCAuthUID[]		= "uid";
static char nameSunRPCAuthGID[]		= "gid";
static char nameSunRPCAuthHostname[]	= "hostname";
static char nameSunRPCAuthGroups[]	= "groups";

/* name used for default peer identification */
static char nameConnectionIdentityInfo[]= "connection";

/* name used for GSS identity */
static char nameGSSIdentityInfo[]	= "GSS";

PyObject *
ilupp_FromPassport (ilu_Passport pp)
{
  IluppObject *	p	= PyObject_NEW(IluppObject, &Ilupp_Type);

  p->passport = pp;
  return (PyObject *) p;
}

ilu_Passport
  ilupp_AsPassport(IluppObject *p)
{
  return (p->passport);
}

static void
ilupp_dealloc(PyObject *o)
{
  PyMem_DEL(o);
}

static PyObject *
  _ilupython_PyIdentity (ilu_IdentityInfo ident)
{
  if (ident->ii_type == ilu_ConnectionIdentity)
    {
      return (PyString_FromString((ilu_string) (ident->ii_info)));
    }
#ifdef SUNRPC_PROTOCOL
  else if (ident->ii_type == ilu_SunRPCAuthUnixIdentity)
    {
      PyObject *pp;
      PyObject *auth;
      PyObject *groups;
      PyObject *tmp;
      int groupcount = 0;
      int i;

      /* create SunRPC identity record */

      auth = PyDict_New ();
      if (auth == ILU_NIL)
	{
	  PyErr_SetString (_ilupython_GeneralError, "Can't create dictionary object for SunRPCAuthUnix identity info");
	  return 0;
	}
      PyDict_SetItemString (auth, nameSunRPCAuthUID,
			    tmp = PyInt_FromLong((long) ((ilu_SunRPCAuthUnixIdentityInfo)(ident->ii_info))->ii_UID));
      Py_DECREF(tmp);
      PyDict_SetItemString (auth, nameSunRPCAuthGID,
			    tmp = PyInt_FromLong((long) ((ilu_SunRPCAuthUnixIdentityInfo)(ident->ii_info))->ii_GID));
      Py_DECREF(tmp);
      PyDict_SetItemString (auth, nameSunRPCAuthHostname,
			    tmp = PyString_FromString(((ilu_SunRPCAuthUnixIdentityInfo)(ident->ii_info))->ii_hostname));
      Py_DECREF(tmp);
      for (i = 0, groupcount = 0;  i < (int)((ilu_SunRPCAuthUnixIdentityInfo)(ident->ii_info))->ii_ngids;  i += 1)
	if (((ilu_SunRPCAuthUnixIdentityInfo)(ident->ii_info))->ii_gids[i] != 0)
	  groupcount += 1;
      if (groupcount == 0)
	groups = Py_None;
      else
	{
	  groups = PyTuple_New(groupcount);
	  if (groups == 0)
	    {
	      PyErr_SetString (_ilupython_GeneralError, "Couldn't create tuple of user's groups");
	      Py_DECREF(auth);
	      return 0;
	    }
	  for (i = 0, groupcount = 0;  i < (int) ((ilu_SunRPCAuthUnixIdentityInfo)(ident->ii_info))->ii_ngids;  i++)
	    {
	      if (((ilu_SunRPCAuthUnixIdentityInfo)(ident->ii_info))->ii_gids[i] != 0)
		{
		  PyTuple_SetItem (groups, groupcount,
				   PyInt_FromLong((long) ((ilu_SunRPCAuthUnixIdentityInfo)(ident->ii_info))->ii_gids[i]));
		  groupcount += 1;
		}
	    }
	}
      PyDict_SetItemString (auth, nameSunRPCAuthGroups, groups);
      Py_DECREF(groups);
      return auth;
    }
#endif /* def SUNRPC_PROTOCOL */

#ifdef SECURE_TRANSPORT
  else if (ident->ii_type == ilu_GSSIdentity)
    {
      /* create GSS identity record */
      return (ilugi_FromIdentityInfo (ident));
    }
#endif /* def SECURE_TRANSPORT */
  else {
    char buf[1000];
    ilu_cardinal len;
    ilu_Error kerr;

    len = ident->ii_type->it_string_form (ident, buf, 1000, &kerr);
    if (ILU_ERRNOK(kerr)) {
      ILU_HANDLED(kerr);
      PyErr_SetString (_ilupython_GeneralError, "Couldn't create string form for identity");
      return 0;
    } else if (len == 0) {
      PyErr_SetString (_ilupython_GeneralError, "Couldn't create string form for identity");
      return 0;
    } else
      return (PyString_FromStringAndSize(buf, len));
  }
}

static PyObject *
ilupp_lookupIdentity (PyObject *self, PyObject *args)
{
  ilu_Error kerr;
  char *idtypename;
  ilu_IdentityType idtype;
  ilu_IdentityInfo identity;

  if (!(PyErr_Clear(), PyArg_Parse(args, "s", &idtypename)))
    return 0;
  if ((idtype = ilu_FindIdentityTypeByName(idtypename, &kerr)), ILU_ERRNOK(kerr))
    {
      return _ilupython_RaiseILUGeneralError(&kerr);
    }
  else if (idtype == ILU_NIL)
    {
      PyErr_SetString(_ilupython_GeneralError, "unknown identity type");
      return 0;
    }
  else if ((identity = ilu_FindIdentity(ilupp_AsPassport((IluppObject *) self), idtype)) == ILU_NIL)
    {
      Py_INCREF(Py_None);
      return Py_None;
    }
  else
    return _ilupython_PyIdentity(identity);
}

#ifdef SECURE_TRANSPORT

static PyObject *
ilupp_addGSS (PyObject *self, PyObject *args)
{
  PyObject *gss_cred;
  ilu_IdentityInfo ii;
  ilu_Error err;
  ilu_Passport pp;
  
  if (!(PyErr_Clear(), PyArg_Parse(args, "O", &gss_cred)))
    return 0;
  if (ilugi_Check(gss_cred))
    ii = ilu_CopyIdentity(((IlugiObject *)gss_cred)->id, &err);
  else if (iluGSSCred_Check(gss_cred))
    ii = ilu_AcquireGSSIdentity(((IluGSSCredObject *)gss_cred)->cred, &err);
  else {
    PyErr_SetString(PyExc_TypeError, "arg should be a GSS credential or identity");
    return 0;
  }
  if (ILU_ERRNOK(err)) {
    return _ilupython_RaiseILUGeneralError(&err);
  }
  pp = ilupp_AsPassport((IluppObject *) self);
  ilu_AddIdentity(pp, ii, &err);
  if (ILU_ERRNOK(err)) {
    return _ilupython_RaiseILUGeneralError(&err);
  };
  Py_INCREF(Py_None);
  return Py_None;
}

#endif /* def SECURE_TRANSPORT */

#ifdef SUNRPC_PROTOCOL

static PyObject *
ilupp_addSunRPCAuthUnix (PyObject *self, PyObject *args)
{
  ilu_SunRPCAuthUnixIdentityInfo sraui;
  ilu_IdentityInfo ii;
  ilu_Error err;
  int i;
  int uid, gid;
  char *hostname;
  PyObject *groups;
  int ngroups;
  ilu_Passport pp;
  
  if (!(PyErr_Clear(), PyArg_Parse(args, "(siiO)", &hostname, &uid, &gid, &groups)))
    return 0;
  if (!PyTuple_Check(groups)) {
    PyErr_SetString(PyExc_TypeError, "arg 5 should be a tuple of group IDs");
    return 0;
  }
  for (i = 0;  i < PyTuple_Size(groups);  i++) {
    if (!PyInt_Check(PyTuple_GetItem(groups, i))) {
      PyErr_SetString(PyExc_TypeError, "arg 5 should be a tuple of group IDs");
      return 0;
    }
  }
  ii = (ilu_IdentityInfo) ilu_MallocE(sizeof(*ii), &err);
  if (ILU_ERRNOK(err)) {
    return _ilupython_RaiseILUGeneralError(&err);
  }
  sraui = (ilu_SunRPCAuthUnixIdentityInfo) ilu_MallocE(sizeof(*sraui), &err);
  if (ILU_ERRNOK(err)) {
    ilu_free(ii);
    return _ilupython_RaiseILUGeneralError(&err);
  };
  ii->ii_type = ilu_SunRPCAuthUnixIdentity;
  ii->ii_owned_by_passport = ilu_TRUE;
  ii->ii_info = sraui;
  sraui->ii_UID = uid;
  sraui->ii_GID = gid;
  sraui->ii_hostname = ilu_StrdupE(hostname, &err);
  if (ILU_ERRNOK(err)) {
    ilu_free(ii); ilu_free(sraui);
    return _ilupython_RaiseILUGeneralError(&err);
  }
  ngroups = PyTuple_Size(groups);
  sraui->ii_gids = (ilu_shortcardinal *) ilu_MallocE(sizeof(ilu_shortcardinal)*ngroups, &err);
  if (ILU_ERRNOK(err)) {
    ilu_free(sraui->ii_hostname); ilu_free(ii); ilu_free(sraui);
    return _ilupython_RaiseILUGeneralError(&err);
  };
  sraui->ii_ngids = ngroups;
  for (i = 0;  i < ngroups;  i++) {
    sraui->ii_gids[i] = (ilu_shortcardinal) PyInt_AsLong(PyTuple_GetItem(groups, i));
  };
  pp = ilupp_AsPassport((IluppObject *) self);
  ilu_AddIdentity(pp, ii, &err);
  if (ILU_ERRNOK(err)) {
    ilu_free(sraui->ii_gids); ilu_free(sraui->ii_hostname); ilu_free(ii); ilu_free(sraui);
    return _ilupython_RaiseILUGeneralError(&err);
  };
  Py_INCREF(Py_None);
  return Py_None;
}

#endif /* def SUNRPC_PROTOCOL */

static PyMethodDef ilupp_methods[] =
{
  { "lookupIdentity",	ilupp_lookupIdentity	},
#ifdef SUNRPC_PROTOCOL
  { "addSunRPCAuthUnix",ilupp_addSunRPCAuthUnix },
#endif /* SUNRPC_PROTOCOL */
#ifdef SECURE_TRANSPORT
  { "addGSS",ilupp_addGSS			},
#endif
  { 0						}
};

static PyObject *
ilupp_getattr(PyObject *self, char *name)
{
  return Py_FindMethod(ilupp_methods, self, name);
}

PyTypeObject	Ilupp_Type =
{
	PyObject_HEAD_INIT(&PyType_Type)
	0,
	"ilu_Passport",
	sizeof(IluppObject),
	0,
	ilupp_dealloc,		/*tp_dealloc*/
	0,			/*tp_print*/
	ilupp_getattr,		/*tp_getattr*/
	0,			/*tp_setattr*/
	0,			/*tp_compare*/
	0,			/*tp_repr*/
	0,			/*tp_as_number*/
	0,			/*tp_as_sequence*/
	0,			/*tp_as_mapping*/
	0,			/*tp_hash*/
};
