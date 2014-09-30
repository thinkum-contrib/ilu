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

$Id: ilupygss.c,v 1.8 1999/09/20 22:50:08 janssen Exp $
*/

/* from ILU */
#include "iluxport.h"

/* local */
#include "python.h"
#include "ilupygss.h"
#include <ctype.h>	/* for isdigit() */

#define ERRASSERT(pred,clue)	{ if (!(pred)) { ilu_DebugPrintf("runtime/python/ilupygss.c:gssStrErr:  problem decoding GSS error:  %s\n", clue); return "problem decoding GSS error"; }; }

static char *
  gssStrErr(OM_uint32 status_code, char *retbuf, ilu_cardinal len)
{
  gss_buffer_desc errbuf;
  OM_uint32 major_code,minor_code,msg_context=0;

  major_code = gss_display_status(&minor_code,status_code,
				  GSS_C_GSS_CODE,NULL,
				  &msg_context,
				  &errbuf);
  ERRASSERT (major_code == GSS_S_COMPLETE,
	     "gss_display_status failed");
  ERRASSERT (len > errbuf.length,
	     "buffer too small for error description");
  strncpy (retbuf, (char*) errbuf.value, errbuf.length);
  retbuf[errbuf.length] = 0;
  ERRASSERT (gss_release_buffer(&minor_code, &errbuf) == GSS_S_COMPLETE,
	     "release of GSS buffer failed");
  return retbuf;
}

PyObject *
  iluGSSCred_FromCred (gss_cred_id_t cred)
{
  IluGSSCredObject *	v	= PyObject_NEW(IluGSSCredObject, &IluGSSCred_Type);

  if (v == 0)
    return 0;
  v->cred = cred;
  return (PyObject *) v;
}

static PyObject *
  raiseGSSError (char *msg, OM_uint32 major, OM_uint32 minor)
{
  char errbuf[1000], tempbuf[1000];
  sprintf(errbuf, "%s: %x (%s), minor=%x", msg,
	  major, gssStrErr(major, tempbuf,sizeof(tempbuf)), minor);
  PyErr_SetString(_ilupython_GeneralError, errbuf);
  return 0;
}

/* structure to associate a name with a gss_OID_desc */
typedef struct { 
	char *name;  
	gss_OID_desc oid[1]; 
} oid_names;

/* Returns the gss_OID associated with the given id.  
If id is in string form, i.e. {... or dotted decimal form, gss_str_to_oid is used, 
else the array oid_names is searched for a matching id. */
static ilu_boolean
  lookupOID (const char *id, oid_names *known, int sizeof_known, ilu_boolean *known_p, gss_OID * out)
{
  int i;
  *known_p = ilu_FALSE;
  if (id[0] == '{' ||		/* string form of OID */
      isdigit(id[0]))		/* dotted decimal form */
    {
      gss_buffer_desc b;
      OM_uint32 major, minor = 0;
      
      b.value = (char *) id;
      b.length = strlen(id);
      major = gss_str_to_oid (&minor, &b, out);
      if (major != GSS_S_COMPLETE) {
	raiseGSSError("Can't determine OID for GSS mechanism name", major, minor);
	return ilu_FALSE;
      } else {
	*known_p = ilu_FALSE;
	return ilu_TRUE;
      }
    }
  else
    {
      /* lookup in table of names */
      for (i = 0;  i < sizeof_known;  i++)
	if (strcmp(known[i].name, id) == 0) {
	  *known_p = ilu_TRUE;
	  *out = known[i].oid;
	  return ilu_TRUE;
	}
    }
  return ilu_FALSE;
}

/* Returns the gss_OID associated with the id, which may be 
one of the Xerox.ILU.GSS.X509 or Xerox.ILU.GSS.RFC822 */
static ilu_boolean figureMech (const char *id, ilu_boolean *old, gss_OID *oid)
{
  static oid_names known[] = {
    { "Xerox.ILU.GSS.SSL", { { 9, "\x2a\x86\x48\x86\xf7\x0e\x09\x01\x01" } } },
    { "Xerox.ILU.GSS.NIL", { { 9, "\x2a\x86\x48\x86\xf7\x0e\x09\x01\x03" } } },
  };
  return lookupOID(id, known, sizeof(known)/sizeof(oid_names), old, oid);
}

PyObject *
  iluGSSCred_AcquireCredForName (PyObject *self, PyObject *args)
{
  ilu_Error kerr;
  char *name = ILU_NIL;
  char *mech = ILU_NIL;
  long int lifetime;
  PyObject *accept_only_obj;
  ilu_boolean accept_only;
  OM_uint32 major, minor;
  gss_OID mech_oid = ILU_NIL;
  gss_cred_id_t cred;
  gss_buffer_desc mech_buffer;
  PyObject *cred_obj;
  ilu_boolean constant_mech_oid;

  if (!PyArg_Parse(args, "(slsO)", &name, &lifetime, &mech, &accept_only_obj))
    return 0;
  accept_only = (PyObject_IsTrue(accept_only_obj) ? ilu_TRUE : ilu_FALSE);
  
  if (!figureMech(mech, &constant_mech_oid, &mech_oid))
    return 0;
  cred = ilu_AcquireGSSCredForName (name, (ilu_cardinal) lifetime, mech_oid, accept_only, &kerr);
  if (!constant_mech_oid)
    (void) gss_release_oid (&minor, &mech_oid);
  if (ILU_ERRNOK(kerr)) {
    return _ilupython_RaiseILUGeneralError(&kerr);
  } else {
    return ((PyObject *) iluGSSCred_FromCred(cred));
  }
}

static void
iluGSSCred_dealloc(PyObject *o)
{
  IluGSSCredObject * v	= (IluGSSCredObject *) o;
  OM_uint32 minor, returncode;

  if (v->cred != ILU_NIL) {
    returncode = gss_release_cred(&minor, &v->cred);
  }
  v->cred = ILU_NIL;
  PyMem_DEL(o);
}

PyTypeObject	IluGSSCred_Type =
{
	PyObject_HEAD_INIT(&PyType_Type)
	0,
	"gss_cred_id_t",
	sizeof(IluGSSCredObject),
	0,
	iluGSSCred_dealloc,	/*tp_dealloc*/
	0,			/*tp_print*/
	0,			/*tp_getattr*/
	0,			/*tp_setattr*/
	0,			/*tp_compare*/
	0,			/*tp_repr*/
	0,			/*tp_as_number*/
	0,			/*tp_as_sequence*/
	0,			/*tp_as_mapping*/
	0,			/*tp_hash*/
};


PyObject *
iluGSSName_FromName (gss_name_t name, ilu_cardinal good_till, ilu_boolean local)
{
  IluGSSNameObject *	v	= PyObject_NEW(IluGSSNameObject, &IluGSSName_Type);

  if (v == 0)
    return 0;
  v->name = name;
  v->good_till = good_till;
  v->local = local;
  return (PyObject *) v;
}

static void
iluGSSName_dealloc(PyObject *o)
{
  IluGSSNameObject * v	= (IluGSSNameObject *) o;
  OM_uint32 minor, returncode;

  if (v->name != ILU_NIL) {
    returncode = gss_release_name(&minor, &v->name);
  }
  v->name = ILU_NIL;
  PyMem_DEL(o);
}

static int
iluGSSName_print(PyObject *o, FILE *fp, int flags)
{
  IluGSSNameObject *v = (IluGSSNameObject *) o;
  PyObject *retval;
  ilu_string ret;
  ilu_Error kerr;

  fprintf (fp, "<ilu_GSSName");
  if (v->name == ILU_NIL)
    {
      fprintf (fp, " (uninitialized)>");
    }
  else {
    ret = ilu_GSSNameToString (v->name, &kerr);
    if (ILU_ERRNOK(kerr)) {
      fprintf (fp, " (error: %s)>", ILU_ERR_NAME(kerr));
      ILU_HANDLED(kerr);
    } else {
      fprintf (fp, " %s>", ret);
      ilu_free(ret);
    }
  }
  return 0;	  
}

PyTypeObject	IluGSSName_Type =
{
	PyObject_HEAD_INIT(&PyType_Type)
	0,
	"gss_name_t",
	sizeof(IluGSSNameObject),
	0,
	iluGSSName_dealloc,	/*tp_dealloc*/
	iluGSSName_print,	/*tp_print*/
	0,			/*tp_getattr*/
	0,			/*tp_setattr*/
	0,			/*tp_compare*/
	0,			/*tp_repr*/
	0,			/*tp_as_number*/
	0,			/*tp_as_sequence*/
	0,			/*tp_as_mapping*/
	0,			/*tp_hash*/
};




