/*  -*- Mode: C; -*-
 *
 * This code contributed by Siemens Corporate Research, Inc.
 */
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

#include "ilu-scm-private.h"
#include "iluobj-scm.h"

struct registry_record {
  ilu_Class c;
  SCM* proc;
  struct registry_record *next;
};

static struct registry_record *RegisteredClasses = NULL;

static struct registry_record * findRegistryRecordByClass (ilu_Class c)
{
  struct registry_record *p;

  for (p = RegisteredClasses;  p != NULL;  p = p->next)
    if (p->c == c)
      return (p);
  return (NULL);
}

SCM iluguile_object__register_surrogate_creator(SCM _c, SCM _proc)
{
  ilu_Class c = iluguile_scheme_to_ptr(ilu_Class, _c);
  struct registry_record *p;

  if ((p = findRegistryRecordByClass(c)) == NULL)
    {
      p = ilu_malloc(sizeof(struct registry_record));
      p->c = c;
      p->proc = iluguile_scm_make_array(1);
      p->proc[0] = _proc;
      p->next = RegisteredClasses;
      RegisteredClasses = p;
    }
  return scheme_True;
}

/* Inside(obj->server, c) */
SCM iluguile_object_create_from_registry(ilu_Class c, ilu_Object obj)
{
  struct registry_record *p;
  if((p = findRegistryRecordByClass(c)) == NULL) {
    return scheme_False;
  } else {
    SCM _obj = iluguile_ptr_to_scheme(ilu_Object, obj);
    return iluguile_scm_call1(p->proc[0], _obj);
  }
}

/* Inside(obj->server, c) */
SCM iluguile_object__create_from_registry(SCM _c, SCM _obj)
{
  ilu_Class c = iluguile_scheme_to_ptr(ilu_Class, _c);
  ilu_Object obj = iluguile_scheme_to_ptr(ilu_Object, _obj);
  return iluguile_object_create_from_registry(c, obj);
}


SCM iluguile_object__input_object(SCM _call,
			     SCM _discriminator_p,
			     SCM _putative_class)
{
  iluguile_SCMCall call = iluguile_scheme_to_ptr(iluguile_SCMCall, _call);
  ilu_boolean discriminator_p = iluguile_scheme_to_value(boolean, _discriminator_p);
  ilu_Class putative_class = iluguile_scheme_to_ptr(ilu_Class, _putative_class);
  ilu_Object obj;
  ilu_Class c;
  SCM o;

  if (discriminator_p && putative_class->cl_singleton) {
    if ((obj = ilu_GetCallSingleton(scm_call_call(call), scm_call_err(call))) == NULL) {
      iluguile_prefer_success(scm_call_err(call));
      return scheme_False;
    }
  } else {
    obj = iluguile_input_object_id(call, discriminator_p, putative_class);
    if (obj == NULL)
      return scheme_False;
  }

  /* now Inside(obj->server, putative_class) */
  o = iluguile_get_language_specific_object(obj);
  if (o == scheme_False) {
    if ((c = ilu_ClassOfObject(obj)) != NULL)
      o = iluguile_object_create_from_registry(c, obj);
  }
  ilu_ExitServer(ilu_ServerOfObject(obj), putative_class);

  return o;
}

SCM iluguile_object__output_object(SCM _call, SCM _obj, SCM _putative_class)
{
  iluguile_SCMCall call = iluguile_scheme_to_ptr(iluguile_SCMCall, _call);
  ilu_Class putative_class = iluguile_scheme_to_ptr(ilu_Class, _putative_class);
  ilu_Object kobj;
  ilu_boolean b;

  if(_obj == scheme_False) {
    kobj = NULL;
  } else {
    SCM _kobj = iluguile_scm_scall1("get-rpc-object", _obj);
    kobj = iluguile_scheme_to_ptr(ilu_Object, _kobj);
  }

  /* now kobj != NULL implies Inside(kobj->server, kobj->class) */
  b = iluguile_output_object_id(call, kobj, ilu_FALSE, putative_class);
  return iluguile_value_to_scheme(boolean, b);
}

SCM iluguile_object__size_of_object(SCM _call, SCM obj, SCM _putative_class)
{
  iluguile_SCMCall call = iluguile_scheme_to_ptr(iluguile_SCMCall, _call);
  ilu_Class putative_class = iluguile_scheme_to_ptr(ilu_Class, _putative_class);
  ilu_Object kobj;
  ilu_cardinal s;

  if(obj == scheme_False) {
    kobj = NULL;
  } else {
    SCM _kobj = iluguile_scm_scall1("get-rpc-object", obj);
    kobj = iluguile_scheme_to_ptr(ilu_Object, _kobj);
  }

  /* now kobj != NULL implies Inside(kobj->server, kobj->class) */
  s = iluguile_size_of_object_id(call, kobj, ilu_FALSE, putative_class);
  return iluguile_value_to_scheme(cardinal, s);
}

SCM iluguile_object__lookup(SCM _sid, SCM _ih, SCM _pclass)
{
  char* sid = iluguile_scheme_to_value(string, _sid);
  char* ih = iluguile_scheme_to_value(string, _ih);
  ilu_Class pclass = iluguile_scheme_to_ptr(ilu_Class, _pclass);
  ilu_Object kobj;
  SCM o;

  if ((kobj = ilu_LookupObject (sid, ih, pclass)) == NULL)
    return scheme_False;

  /* now Inside the server of the kobj */
  if ((o = iluguile_get_language_specific_object(kobj)) == scheme_False) {
    o = iluguile_object_create_from_registry(pclass, kobj);
  }
  ilu_ExitServer(ilu_ServerOfObject(kobj), pclass);

  ilu_free(sid);
  ilu_free(ih);

  return o;
}


SCM iluguile_object__register_as_gc_callback(SCM obj)
{
  SCM _kobj = iluguile_scm_scall1("get-rpc-object", obj);
  ilu_Object kobj = iluguile_scheme_to_ptr(ilu_Object, _kobj);

  if(kobj != NULL)
    {
      SCM server, cr;
      ilu_SetGcClient(kobj); /* register object with GC handler in kernel */
      server = iluguile_scm_scall1("ilu-object:rpc-server", obj);
      cr = iluguile_scm_scall1("ilu-object:instance-class-record", obj);
      iluguile__exit_server(server, cr);
    }

  return scheme_True;
}

