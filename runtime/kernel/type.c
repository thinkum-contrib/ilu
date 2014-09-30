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
 */
/* $Id: type.c,v 1.111 1999/08/03 01:52:56 janssen Exp $ */
/* Last edited by Mike Spreitzer April 30, 1998 11:30 am PDT */
/* Chris Jacobi, September 10, 1998 11:01 am PDT */

#define _POSIX_SOURCE

#include "iluntrnl.h"

#include "object.h"
#include "server.h"
#include "type.h"

#include <limits.h>		/* for ULONG_MAX */

#define ILU_TYPEID_CONST_root_obj  "ilu:root-object-type"

/* L1, L2, Main unconstrained */

static void _ilu_HandleIsA (ilu_Call call);

static struct _ilu_MethodArg_s RegisterGCInterestArgs[1] = { 
  {"object", ILU_TYPEID_CONST_root_obj, ilu_In, ilu_FALSE }};

static struct _ilu_MethodArg_s IsAArgs[1] = {
  {"typeid", ILU_TYPEID_CONST_ilu_CString, ilu_In, ilu_FALSE }};  

static struct _ilu_Method_s _ilu_InternalMethods[] = {

  {"ILUGetTypes", 0xFF83,
     ilu_TRUE, ilu_FALSE, ilu_TRUE, 0, 0, NIL, NIL, ILU_TYPEID_CONST_ilu_CString, { _ilu_HandleGetTypes, 0 }},

  {"ILURegisterGCInterest", 0xFF81,
     ilu_FALSE, ilu_FALSE, ilu_FALSE, 0, 1, NIL, RegisterGCInterestArgs, NIL, { _ilu_HandleGCInterestRegistration, 0 }},
  {"ILUUnregisterGCInterest", 0xFF82,
     ilu_FALSE, ilu_FALSE, ilu_FALSE, 0, 1, NIL, RegisterGCInterestArgs, NIL, { _ilu_HandleGCInterestDeregistration, 0 }},

  {"ILUPing", 0xFF84,
     ilu_FALSE, ilu_FALSE, ilu_FALSE, 0, 0, NIL, NIL, NIL, { _ilu_HandlePing, 0 }},

  {"-is-a", 0xFF85,		/* for CORBA support */
     ilu_FALSE, ilu_FALSE, ilu_FALSE, 0, 1, NIL, IsAArgs, ILU_TYPEID_CONST_ilu_boolean, { _ilu_HandleIsA, 0 }}
};

ilu_Method      _ilu_GetTypesMethod = &_ilu_InternalMethods[0];
ilu_Method      _ilu_RegisterGCInterestMethod = &_ilu_InternalMethods[1];
ilu_Method      _ilu_UnregisterGCInterestMethod = &_ilu_InternalMethods[2];
ilu_Method      _ilu_PingMethod = &_ilu_InternalMethods[3];
ilu_Method      _ilu_IsAMethod = &_ilu_InternalMethods[4];

static const int    nInternal = sizeof(_ilu_InternalMethods)
/ sizeof(struct _ilu_Method_s);

struct _ilu_Class_s _ilu_rootClass_s = {
  "ilu.Object",			/* name */
  "version 2",			/* brand */
  ILU_TYPEID_CONST_root_obj,	/* unique ID */
  NIL,				/* singleton? */
  ilu_TRUE,				/* collectible? */
  "internal root type for"	/* doc string */
    "all ILU objects",
  _ilu_InternalMethods,		/* methods */
  sizeof(_ilu_InternalMethods) / sizeof(struct _ilu_Method_s),
  /* method_count */
  0,				/* superclass_count */
  NIL,				/* superclass_ids */
  NIL,				/* superclasses */
#ifdef ILU_HTTPNG_OBJECTS
  0,
  NIL,				/* state */
  ilu_FALSE,       			/* local? */
  ilu_FALSE,			/* sealed? */
#endif
  ilu_FALSE,			/* shown */
  ilu_TRUE,				/* optional? */
  ilu_FALSE				/* phony? */
};

const ilu_Class _ilu_rootClass = &_ilu_rootClass_s;
const ilu_Class ilu_rootClass = &_ilu_rootClass_s;

static struct _ilu_Method_s nometh = {NIL, ULONG_MAX, ilu_FALSE,
ilu_FALSE, ilu_FALSE, 0, 0, NIL, NIL, NIL, { NULLFN }};

/* L1_sup >= otmu */
static ilu_boolean TableDumped = ilu_FALSE;

static int optstrcmp(const char *s1, const char *s2)
{
  if (s1 == s2)
    return 0;
  if (s1 == NIL)
    return -1;
  if (s2 == NIL)
    return 1;
  return strcmp(s1, s2);
}

ilu_boolean ilu_CollectibleP (ilu_Class c)
{
  return (c->cl_collectible);
}

ilu_Method ilu_MethodNOfClass (ilu_Class c, ilu_cardinal index)
{
  if (c == NIL || index >= c->cl_method_count)
    return NIL;
  return (c->cl_methods + index);
}

/* L1 >= {otmu} */
/* L2, Main unconstrained */

static HashTable ClassNameTable = NIL;
/* Object type name -> object type */

static HashTable ClassIDTable = NIL;
/* Object type id -> object type */

static HashTable UnknownTypeIDs = NIL;
/* Object type id -> ID_DAG */

static HashTable UnlinkedClasses = NIL;
/*
 * Mapping from object type id to vector of object types waiting
 * for id to be registered.
 */


static void 
_ilu_RegisterClass(ilu_Class class)
{
  int             new;
  ilu_Class       p;
  register ilu_cardinal index;
  ilu_Vector      v;
  ilu_Error	lerr;

  static ilu_boolean initialized = ilu_FALSE;

  if (class != NIL && ((ilu_cardinal) (class->cl_singleton)) == 1) {
    ILU_ERRPRINTF("\
Fatal ILU error:  Stubs for type \"%s\" were generated\n\
by a pre-1.6.4-p8 stubber.  Please re-stub, re-compile, and re-link.\n",
	    class->cl_name);
    exit(1);
  }

  ILU_NOTE(TYPE_DEBUG,
	("_ilu_RegisterClass:  Registering object type %s, id=%s, ilu_Class=%p.\n",
	 class->cl_name, class->cl_unique_id, class));
  
  if (!initialized) {
    ClassNameTable = ilu_hash_MakeNewTable(CLASS_HASHTABLESIZE,
					   ilu_hash_HashString,
					   ilu_hash_StringCompare);
    ClassIDTable = ilu_hash_MakeNewTable(CLASS_HASHTABLESIZE,
					 ilu_hash_HashString,
					 ilu_hash_StringCompare);
    UnlinkedClasses = ilu_hash_MakeNewTable(CLASS_HASHTABLESIZE,
					    ilu_hash_HashString,
					    ilu_hash_StringCompare);
    initialized = ilu_TRUE;
    /* make sure gc callback class is registered */
    _ilu_RegisterClass(_ilu_GcCallbackClass);
  }

  if (ilu_hash_FindInTable(ClassNameTable, class_name(class)) == NIL)
    _ilu_Assert((int) ilu_hash_AddToTable(ClassNameTable, class_name(class),
				     class),
		"RegisterClass AddToTable ClassNameTable");
  new = NIL == ilu_hash_FindInTable(ClassIDTable,
				      class_unique_id(class));
  if (new)
    _ilu_Assert((int) ilu_hash_AddToTable(ClassIDTable,
				   class_unique_id(class), class),
		"RegisterClass AddToTable ClassIDTable");

#ifndef ILU_GENERATE_SUBTYPES
  /*
   * this handles the case where additional class information is
   * loaded into a running program
   */
  if (new && UnknownTypeIDs != NIL
  && ilu_hash_FindInTable(UnknownTypeIDs, class_unique_id(class))
      != NIL) {		/* This shouldn't happen! */
    ASSERT(0, buf,
	   (buf, "%s %s %s previously considered unknown.\n",
       "ilu_RegisterClass:  Config bug!  Registering object type",
	    class_name(class), class_unique_id(class)));
  }
#endif /* def ILU_GENERATE_SUBTYPES */

  TableDumped = ilu_FALSE;	/* remember to redump class table */

  /*
   * this links up the superclass of "class" which have already
   * been registered
   */
  for (index = 0; index < class_superclass_count(class); index++) {
    if (class_superclass_id(class, index) != NIL) {
      p = (ilu_Class) ilu_hash_FindInTable(ClassIDTable,
			       class_superclass_id(class, index));
      if (p == NIL) {
	v = (ilu_Vector) ilu_hash_FindInTable(UnlinkedClasses,
			       class_superclass_id(class, index));
	if (v == NIL) {
	  v = _ilu_vector_new(1, &lerr);
	  ILU_MUST_BE_SUCCESS(lerr);
	  _ilu_Assert((int) ilu_hash_AddToTable(UnlinkedClasses,
				class_superclass_id(class, index),
					   v),
		      "RegisterClass AddToTable UnlinkedClasses");
	}
	_ilu_vector_add(v, (ilu_refany) class, &lerr);
	ILU_MUST_BE_SUCCESS(lerr);
      } else
	class_superclass(class, index) = p;
    }
  }

  /*
   * now link those classes which have been waiting for this
   * superclass
   */
  v = (ilu_Vector) ilu_hash_FindInTable(UnlinkedClasses,
					 class_unique_id(class));
  if (v != NIL) {
    ilu_cardinal    size = _ilu_vector_size(v);
    ilu_Class      *elements = (ilu_Class *) _ilu_vector_elements(v);
    register ilu_cardinal j;

    _ilu_Assert(ilu_hash_RemoveFromTable(UnlinkedClasses,
					  class_unique_id(class))
		== v,
		"RegisterClass RemoveFromTable UnlinkedClasses");

    for (index = 0; index < size; index++) {
      p = elements[index];
      for (j = 0; j < class_superclass_count(p); j++)
	if (strcmp(class_unique_id(class), class_superclass_id(p, j))
	    == 0) {
	  class_superclass(p, j) = class;
	  break;
	}
    }
    _ilu_vector_destroy(v, NULLFN);
  }
  return;
}

static ilu_Class ObjectTypeFromID(ilu_string id)
{
  if (strcmp(id, _ilu_rootClass->cl_unique_id) == 0)
    return _ilu_rootClass;
  else if (ClassIDTable != NIL)
    return ((ilu_Class) ilu_hash_FindInTable(ClassIDTable, id));
  else
    return NIL;
}

ilu_Class
ilu_DefineObjectType(ilu_string cl_name,
		     ilu_string cl_brand,
		     ilu_string cl_unique_id,
		     ilu_string cl_singleton,
		     ilu_boolean cl_optional,
		     ilu_boolean cl_collectible,
		     ilu_string cl_doc_string,
		     ilu_cardinal cl_method_count,
		     ilu_cardinal cl_scls_count,
		     ilu_string cl_scls_ids[],
#ifdef ILU_HTTPNG_OBJECTS
		     ilu_cardinal cl_nstate_fields,
		     ilu_boolean cl_local,
		     ilu_boolean cl_sealed,
#endif		     
		     ILU_ERRS((internal, no_memory)) * err)
{
  ilu_Class       ans = ObjectTypeFromID(cl_unique_id);
  unsigned        msize = sizeof(struct _ilu_Method_s);
  unsigned        i;
  if (ans == NIL) {
    ans = (ilu_Class) ilu_malloc(sizeof(*ans));
    if (ans == NIL)
      return ILU_ERR_CONS1(no_memory, err, nbytes, sizeof(*ans), NIL);
    ans->cl_name = _ilu_Strdup(cl_name);
    if (cl_brand != NIL && cl_brand[0] == 0)
      ans->cl_brand = NIL;
    else
      ans->cl_brand = _ilu_Strdup(cl_brand);
    ans->cl_unique_id = _ilu_Strdup(cl_unique_id);
    ans->cl_singleton = _ilu_Strdup(cl_singleton);
    ans->cl_collectible = cl_collectible;
    ans->cl_optional = cl_optional;
    ans->cl_phony = ilu_FALSE;
    ans->cl_doc_string = _ilu_Strdup(cl_doc_string);
    ans->cl_method_count = cl_method_count;
    ans->cl_methods = (ilu_Method) (msize ? ilu_malloc(cl_method_count * msize) : NIL);
    for (i = 0; i < cl_method_count; i++)
      ans->cl_methods[i] = nometh;
    ans->cl_scls_count = cl_scls_count;
    ans->cl_scls_ids = ((ilu_string *) (cl_scls_count ?
		   ilu_malloc(cl_scls_count * sizeof(ilu_string)) : NIL));
    for (i = 0; i < cl_scls_count; i++)
      ans->cl_scls_ids[i] = _ilu_Strdup(cl_scls_ids[i]);
    ans->cl_sclses = ((ilu_Class *) (cl_scls_count ?
		    ilu_malloc(cl_scls_count * sizeof(ilu_Class)) : NIL));
    for (i = 0; i < cl_scls_count; i++)
      ans->cl_sclses[i] = NIL;
#ifdef ILU_HTTPNG_OBJECTS
    ans->cl_sealed = cl_sealed;
    ans->cl_local = cl_local;
    ans->cl_nstate_fields = cl_nstate_fields;
    if (cl_nstate_fields > 0)
      ans->cl_state = ilu_malloc(sizeof(ilu_ObjectState_s) * cl_nstate_fields);
    else
      ans->cl_state = NIL;
#endif
    _ilu_RegisterClass(ans);
    ILU_CLER(*err);
    return ans;
  } else {
    if (cl_brand != NIL && cl_brand[0] == 0)
      cl_brand = NIL; /* ensure NIL matches "" */
    if (strcmp(cl_name, ans->cl_name) ||
	optstrcmp(cl_brand, ans->cl_brand) ||
	optstrcmp(cl_singleton, ans->cl_singleton))
      goto no;
    if ((cl_collectible == 0) != (ans->cl_collectible == 0))
      goto no;
    if (cl_method_count != ans->cl_method_count ||
	cl_scls_count != ans->cl_scls_count)
      goto no;
#ifdef ILU_HTTPNG_OBJECTS
    if (cl_sealed != ans->cl_sealed ||
	cl_local != ans->cl_local ||
	cl_nstate_fields != ans->cl_nstate_fields)
      goto no;
#endif
    for (i = 0; i < cl_scls_count; i++)
      if (strcmp(ans->cl_scls_ids[i], cl_scls_ids[i]))
	goto no;
    ILU_CLER(*err);
    return ans;
no:
    return ILU_ERR_CONS1(internal, err, minor, ilu_im_typeMismatch, NIL);
  }
}

ilu_Exception
ilu_DefineException(char *i, char *e, char *typeid,
		    ILU_ERRS((internal, no_memory)) * err)
{
  ilu_Exception   ans;
  char buf[1024];
  char *key;
  ilu_cardinal keylen;
  static HashTable exceptions = NIL; /* exception_id_string => C ptr */

  if (i == NIL)
    key = e;
  else {
    keylen = strlen(i) + strlen(e) + 7;
    if (keylen > sizeof(buf)) {
      key = ilu_MallocE(keylen, err);
      if (ILU_ERRNOK(*err)) return NIL;
    } else
      key = buf;
    sprintf (key, "ilu:%s.%s", i, e);
  }

  if (exceptions == NIL)
    exceptions = ilu_hash_MakeNewTable(CLASS_HASHTABLESIZE,
					ilu_hash_HashString,
					ilu_hash_StringCompare);

  ans = (ilu_Exception) ilu_hash_FindInTable(exceptions, key);
  if (ans == NIL) {
    ans = ilu_must_malloc(strlen(key) + 2 + _ilu_SafeStrlen(typeid));
    strcpy (ans, key);
    strcpy (ans + strlen(ans) + 1, (typeid == NIL) ? "" : typeid);
    _ilu_Assert((int) ilu_hash_AddToTable(exceptions, ans, ans), "errtab");
  }
  ILU_CLER(*err);
  if ((key != e) && (key != buf))
    ilu_free(key);
  return ans;
}

ilu_Method 
ilu_DefineMethod(ilu_Class c,
		 ilu_cardinal i,
		 ilu_string me_name,
		 ilu_cardinal me_id,
		 ilu_boolean me_cacheable,
		 ilu_boolean me_asynchronous,
		 ilu_cardinal me_exceptionCount,
		 ilu_Exception *me_exceptionVector,
		 ilu_cardinal me_nargs,
		 ilu_string me_rettype,
		 ILU_ERRS((internal, no_memory)) *err)
{
  ilu_Method      m = c->cl_methods + i;
  ilu_cardinal    j;
  if (m->me_name == NIL) {

    m->me_name = ilu_StrdupE(me_name, err);
    if (ILU_ERRNOK(*err)) goto free0;

    if (me_exceptionCount > 0) {
      m->me_exceptionVector = (ilu_Exception *)
	ilu_MallocE(me_exceptionCount * sizeof(ilu_Exception), err);
      if (ILU_ERRNOK(*err)) goto free1;
    } else
      m->me_exceptionVector = NIL;

    if (me_rettype != NIL) {
      m->me_returnType = ilu_StrdupE(me_rettype, err);
      if (ILU_ERRNOK(*err)) goto free2;
    } else
      m->me_returnType = NIL;

    if (me_nargs > 0) {
      m->me_argVector = (ilu_MethodArg)
	ilu_MallocE(me_nargs * sizeof(ilu_MethodArg_s), err);
      if (ILU_ERRNOK(*err)) goto free3;
      memset((void *) m->me_argVector, 0,
	     me_nargs * sizeof(ilu_MethodArg_s));
    } else
      m->me_argVector = NIL;

    m->me_id = me_id;
    m->me_cacheable = me_cacheable;
    m->me_asynchronous = me_asynchronous;
    m->me_return_vals = (me_rettype != NIL);
    m->me_exceptionCount = me_exceptionCount;
    m->me_argCount = me_nargs;
    
    for (j = 0; j < me_exceptionCount; j++) {
      if (me_exceptionVector[j] == NIL) {
	ILU_ERR_CONS1(internal, err, minor, ilu_im_typeIncomplete,
		      ilu_FALSE);
	goto free4;
      };
      m->me_exceptionVector[j] = me_exceptionVector[j];
    }
    ILU_CLER(*err);
    return m;

  free4:
    ilu_free(m->me_argVector);
  free3:
    ilu_free(m->me_returnType);
  free2:
    ilu_free(m->me_exceptionVector);
  free1:
    ilu_free(m->me_name);
  free0:
    return NIL;
  } else {
    if (strcmp(me_name, m->me_name) ||
	me_id != m->me_id ||
	(me_cacheable == 0) != (m->me_cacheable == 0) ||
	(me_asynchronous == 0) != (m->me_asynchronous == 0) ||
	me_exceptionCount != m->me_exceptionCount)
      goto no;
    for (j = 0; j < me_exceptionCount; j++)
      if (m->me_exceptionVector[j] != me_exceptionVector[j])
	goto no;
    ILU_CLER(*err);
    return m;
no:
    return ILU_ERR_CONS1(internal, err, minor, ilu_im_typeMismatch,
			 NIL);
  }
}

ilu_boolean
  ilu_DefineMethodArg(ilu_Method m,		/* m */
		      ilu_cardinal i,		/* index of arg */
		      ilu_string name,		/* arg name, RETAIN */
		      ilu_boolean sibling,	/* sibling */
		      ilu_ArgDirection dir,	/* in, out, or inout */
		      ilu_string type,		/* type ID, RETAIN */
		      ILU_ERRS((internal, no_memory)) *err)
{
  /*
   * Defines the index'th arg of method m.
   */

  if (m == NIL)
    return (ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_nil, ilu_FALSE));
  if (i >= m->me_argCount)
    return (ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_invalid_argument_index, ilu_FALSE));
  m->me_argVector[i].ma_name = ilu_StrdupE(name, err);
  if (ILU_ERRNOK(*err)) goto free0;
  m->me_argVector[i].ma_type = ilu_StrdupE(type, err);
  if (ILU_ERRNOK(*err)) goto free1;
  m->me_argVector[i].ma_dir = dir;
  m->me_return_vals = (m->me_return_vals || (dir == ilu_Out) || (dir == ilu_InOut));
  m->me_argVector[i].ma_sibling = sibling;
  ILU_CLER(*err);
  return ilu_TRUE;

 free1:
  ilu_free(m->me_argVector[i].ma_name);
 free0:
  return ilu_FALSE;
}

#ifdef ILU_HTTPNG_OBJECTS

ilu_boolean
  ilu_DefineObjectState (ilu_Class cl,		/* class */
			 ilu_cardinal i,	/* index of state field */
			 ilu_string name,	/* field name, RETAIN */
			 ilu_string type,	/* type ID, RETAIN */
			 ILU_ERRS((internal, no_memory)) *err)
{
  /*
   * Defines the index'th arg of method m.
   */

  if (cl == NIL)
    return (ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_nil, ilu_FALSE));
  if ((cl->cl_nstate_fields == 0) || (i >= cl->cl_nstate_fields))
    return (ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_invalid_argument_index, ilu_FALSE));
  cl->cl_state[i].os_name = ilu_StrdupE(name, err);
  if (ILU_ERRNOK(*err)) goto free0;
  cl->cl_state[i].os_typeuid = ilu_StrdupE(type, err);
  if (ILU_ERRNOK(*err)) goto free1;
  ILU_CLER(*err);
  return ilu_TRUE;

 free1:
  ilu_free(cl->cl_state[i].os_name);
 free0:
  return ilu_FALSE;
}

#endif /* def ILU_HTTPNG_OBJECTS */

ilu_boolean
ilu_ObjectTypeDefined(ilu_Class t,
		      ILU_ERRS((internal / typeIncomplete)) * err)
{
  unsigned        i;
  for (i = 0; i < t->cl_method_count; i++) {
    ilu_Method      m = t->cl_methods + i;
    if (m->me_name == NIL)
      goto no;
  }
  return ILU_CLER(*err);
no:
  return ILU_ERR_CONS1(internal, err, minor, ilu_im_typeIncomplete,
		       ilu_FALSE);
}

ilu_string
  ilu_TypeOfException (ilu_Exception e)
{
  ilu_string p;

  if (e == NIL)
    return NIL;
  p = e + (strlen(e) + 1);
  if (*p == 0)
    return NIL;
  else
    return p;
}

ilu_boolean
  ilu_DataOfClass (ilu_Class c,
		   char **name,
		   char **brand,
		   char **id,
		   char **singleton,
		   ilu_boolean *collectible,
		   ilu_cardinal *method_count,
		   ilu_cardinal *superclass_count,
		   ilu_Class **superclasses,
		   ilu_boolean *optional,
#ifdef ILU_HTTPNG_OBJECTS
		   ilu_cardinal *n_state_fields,
		   ilu_ObjectState *state_fields,
		   ilu_boolean *local,
		   ilu_boolean *sealed,
#endif /* ILU_HTTPNG_OBJECTS */
		   ilu_Method *methods)
{
#define CLRETURN(p,f)	if ((p) != NIL) *(p) = (f)

  CLRETURN(name,c->cl_name);
  CLRETURN(brand,c->cl_brand);
  CLRETURN(id,c->cl_unique_id);
  CLRETURN(singleton,c->cl_singleton);
  CLRETURN(collectible,c->cl_collectible);
  CLRETURN(method_count,c->cl_method_count);
  CLRETURN(superclass_count,c->cl_scls_count);
  CLRETURN(superclasses,c->cl_sclses);
  CLRETURN(optional,c->cl_optional);
#ifdef ILU_HTTPNG_OBJECTS
  CLRETURN(n_state_fields,c->cl_nstate_fields);
  CLRETURN(state_fields,c->cl_state);
  CLRETURN(local,c->cl_local);
  CLRETURN(sealed,c->cl_sealed);
#endif
  CLRETURN(methods,c->cl_methods);

#undef CLRETURN
  return (ilu_TRUE);
}

ilu_boolean
  ilu_PhonyOfClass (ilu_Class c)
{
  return (c->cl_phony ? ilu_TRUE : ilu_FALSE);
}

ilu_string
  ilu_IDOfClass (ilu_Class c)
{
  return (c->cl_unique_id);
}

ilu_string
  ilu_DocStringOfClass (ilu_Class c)
{
  return (c->cl_doc_string);
}

ilu_boolean
  ilu_DataOfMethod (ilu_Method m,
		    ilu_string *name,
		    ilu_cardinal *id,
		    ilu_boolean *cacheable,
		    ilu_boolean *asynchronous,
		    ilu_cardinal *ecount,
		    ilu_Exception **evec,
		    ilu_StubProc **stubproc)
{
#define MERETURN(p,f)	if ((p) != NIL) *(p) = (f)

  MERETURN(name, m->me_name);
  MERETURN(id,m->me_id);
  MERETURN(cacheable,m->me_cacheable);
  MERETURN(asynchronous,m->me_asynchronous);
  MERETURN(ecount,m->me_exceptionCount);
  MERETURN(evec,m->me_exceptionVector);
  MERETURN(stubproc,m->me_stubprocs);

#undef MERETURN

  return ilu_TRUE;
}

/* L1_sup < otmu */

/* L1_sup >= otmu */
static void 
PrintClassEntry(ilu_refany entry, ilu_refany junk)
{
  ilu_Class       p;

  if (entry != NIL) {
    p = (ilu_Class) entry;
    ILU_ERRPRINTF("    %s (%s)   %p\n", p->cl_name, p->cl_unique_id, p);
  } else
    ILU_ERRPRINTF("<null>\n");
}

void _ilu_EnumerateClasses (void (*proc) (ilu_Class c, ilu_refany rock), ilu_refany rock)
{
  _ilu_AcquireMutex(ilu_otmu);
  ilu_hash_TableEnumerate (ClassNameTable, (void (*) (ilu_refany, ilu_refany)) proc, rock);
  _ilu_ReleaseMutex(ilu_otmu);
  return;
}

ilu_Class 
ilu_GetGcCallbackClass(void)
{
  return _ilu_GcCallbackClass;
}

ilu_Class 
ilu_FindClassFromName(ilu_string classname)
{
  ilu_Class       c;
  _ilu_AcquireMutex(ilu_otmu);

#ifdef ENABLE_DEBUGGING

  if (((ilu_DebugLevel & TYPE_DEBUG) != 0) && 
      (!TableDumped)) {
    ILU_ERRPRINTF("ilu_FindClassFromName:  class table is %p:\n", ClassNameTable);
    ilu_hash_TableEnumerate(ClassNameTable, (void (*) (ilu_refany, ilu_refany)) PrintClassEntry, NIL);
    TableDumped = ilu_TRUE;
  };

#endif /* ENABLE_DEBUGGING */

  if (strcmp(classname, _ilu_rootClass->cl_name) == 0)
    c = _ilu_rootClass;
  else if (ClassNameTable != NIL)
    c = (ilu_Class) ilu_hash_FindInTable(ClassNameTable, classname);
  else
    c = NIL;
  ILU_NOTE(TYPE_DEBUG,
	   ("ilu_FindClassFromName (\"%s\") => %p (%s)\n",
	    classname, c, (c == NIL) ? "?" : class_unique_id(c)));
  _ilu_ReleaseMutex(ilu_otmu);
  return (c);
}

ilu_Class 
ilu_FindClassFromID(ilu_string ID)
{
  ilu_Class       c;
  _ilu_AcquireMutex(ilu_otmu);

#ifdef ENABLE_DEBUGGING

  if (((ilu_DebugLevel & TYPE_DEBUG) != 0) &&
      (!TableDumped)) {
    ILU_ERRPRINTF("ilu_FindClassFromID:  class table is %p:\n", ClassIDTable);
    ilu_hash_TableEnumerate(ClassIDTable, (void (*) (ilu_refany, ilu_refany)) PrintClassEntry, NIL);
    TableDumped = ilu_TRUE;
  };

#endif /* ENABLE_DEBUGGING */

  if (strcmp(ID, _ilu_rootClass->cl_unique_id) == 0)
    c = _ilu_rootClass;
  else if (ClassIDTable != NIL)
    c = (ilu_Class) ilu_hash_FindInTable(ClassIDTable, ID);
  else
    c = NIL;
  ILU_NOTE(TYPE_DEBUG,
	   ("ilu_FindClassFromID (\"%s\") => %p (%s)\n",
	    ID, c, (c == NIL) ? "?" : class_name(c)));
  _ilu_ReleaseMutex(ilu_otmu);
  return (c);
}

static ilu_boolean 
SeekAncestor(ilu_Class a, ilu_Class b)
{
  ilu_cardinal             j;
  ilu_Class       aa;
  if (a == b)
    return (ilu_TRUE);
  for (j = 0; j < class_superclass_count(a); j++) {
    aa = class_superclass(a, j);
    if (aa != NIL && SeekAncestor(aa, b))
      return (ilu_TRUE);
  }
  return (ilu_FALSE);
}

/* L1 > otmu */
/* To be used inside an _ilu_EnumerateClasses enumeration */
ilu_boolean 
_ilu_IsSubObjectType(ilu_Class a, ilu_Class b)
{
  ilu_boolean     ans;
  ans = (b == _ilu_rootClass) || SeekAncestor(a, b);
  return (ans);
}

ilu_boolean 
ilu_IsSubObjectType(ilu_Class a, ilu_Class b)
{
  ilu_boolean     ans;
  _ilu_AcquireMutex(ilu_otmu);
  ans = (b == _ilu_rootClass) || SeekAncestor(a, b);
  _ilu_ReleaseMutex(ilu_otmu);
  return (ans);
}

struct has_subtypes_data {
  ilu_Class theclass;
  ilu_boolean theresult;
};

static void
  testSubtype (ilu_Class c, void *rock)
{
  if (_ilu_IsSubObjectType(c, ((struct has_subtypes_data *) rock)->theclass)
      /* exclude degenerate case of being a subtype of itself */
      && (c != ((struct has_subtypes_data *) rock)->theclass))
    ((struct has_subtypes_data *) rock)->theresult = ilu_TRUE;
}

ilu_boolean
  ilu_HasSubtypes (ilu_Class a)
{
  struct has_subtypes_data data;
  data.theclass = a;
  data.theresult = ilu_FALSE;
  _ilu_EnumerateClasses(testSubtype, &data);
  return data.theresult;
}

typedef struct _iluBuffer_s {
  /* L1 unconstrained */

  char           *buf;
  unsigned long   allocated;
  unsigned long   used;
}              *iluBuffer;
#define BUF_INCREMENT 100

/* L1 unconstrained */

static void 
addBytesToBuf(iluBuffer buf, char *string, ilu_cardinal len)
{
  if (buf->allocated - buf->used < len) {
    buf->allocated += BUF_INCREMENT;
    buf->buf = (char *) ilu_realloc(buf->buf, buf->allocated);
  }
  strncpy(buf->buf + buf->used, string, len);
  buf->used += len;
}

/* L1 >= {otmu} */

static void 
UnshowAncestors(ilu_Class c)
{
  ilu_cardinal             i;
  c->cl_shown = ilu_FALSE;
  for (i = 0; i < class_superclass_count(c); i++)
    UnshowAncestors(class_superclass(c, i));
  return;
}

static void 
AddTypeName(ilu_Class type, iluBuffer buf)
{
  ilu_cardinal             i;
  int             noco = 1;
  addBytesToBuf(buf, class_unique_id(type),
		strlen(class_unique_id(type)));
  if (type->cl_shown != ilu_TRUE) {
    type->cl_shown = ilu_TRUE;
    if (class_superclass_count(type) == 0)
      return;
    addBytesToBuf(buf, "(", 1);
    for (i = 0; i < class_superclass_count(type); i++) {
      if (noco)
	noco = 0;
      else
	addBytesToBuf(buf, ",", 1);
      AddTypeName(class_superclass(type, i), buf);
    }
    addBytesToBuf(buf, ")", 1);
  }
  return;
}

/* L1_sup < otmu */
ilu_string 
_ilu_ClassToString(ilu_Class class)
{
  struct _iluBuffer_s buf;
  buf.allocated = BUF_INCREMENT;
  buf.buf = ilu_malloc(BUF_INCREMENT);
  buf.used = 0;
  _ilu_AcquireMutex(ilu_otmu);
  UnshowAncestors(class);
  AddTypeName(class, &buf);
  addBytesToBuf(&buf, "\0", 1);	/* NUL-terminate */
  _ilu_ReleaseMutex(ilu_otmu);
  return (buf.buf);
}

/* L1 >= {otmu} */

typedef struct _ilu_ID_DAG_s ID_DAG_s, *ID_DAG;

typedef struct _ilu_ID_DAG_Cons {
  /* L1 >= {otmu} */

  ID_DAG          idd;
  struct _ilu_ID_DAG_Cons *next;
}               ID_DAG_Cons, *ID_DAG_List;

struct _ilu_ID_DAG_s {
  /* L1 >= {otmu} */

  char           *id;
  ilu_Class       it;		/* non-NIL if id registered */
  ID_DAG_List     supers;
  ilu_boolean     computed;	/* ilu_TRUE => mska, saak significant */
  ilu_Class       mska;		/* the one Most Specific Known
				 * Ancestor, or NIL if not
				 * well-defined */
  ilu_boolean     saak;		/* Some Ancestors Are Known */
  ilu_boolean     visited;	/* is this an ancestor of a counted
				 * type? */
  ID_DAG          anext;	/* threaded list of most spcfc
				 * known anc'rs */
  ID_DAG          aprev;	/* it's doubly-linked */
};

static ID_DAG_s mska_head;
/*
 * Head of threaded list of most spcfc known anc'rs. No one member
 * is an ancestor of another.
 */

/* L1_sup < otmu */
ilu_Class 
_ilu_FindMSKA(ilu_string tid)
{
  ID_DAG          n;
  ilu_Class       ans;
  _ilu_AcquireMutex(ilu_otmu);
  n = (ID_DAG) ilu_hash_FindInTable(UnknownTypeIDs, tid);
  if (n != NIL && n->computed)
    ans = n->mska;
  else
    ans = NIL;
  _ilu_ReleaseMutex(ilu_otmu);
  return ans;
}

/*
 * spec ::== id [ "(" spec ("," spec)* ")" ] At entry, *names is at
 * the start of a spec; at exit, *names is at the first char past
 * that spec. Result is the ID_DAG having the id in the spec.
 * Caller owns storage for names.
 */
static ID_DAG 
ClassNamesToID_DAG(ilu_string * names)
{
  ID_DAG          n;
  char           *pl, *pr, *pc, *pe, cr;
  ilu_integer     len;

  ILU_NOTE((TYPE_DEBUG | OBJECT_DEBUG),
  ("ILU(ClassNamesToID_DAG):  Called with \"%s\"\n", *names));

  if (*names == NIL || ** names == '\0')
    return (NIL);
  len = strlen(*names);
  pl = strchr(*names, '(');
  pr = strchr(*names, ')');
  pc = strchr(*names, ',');
  if (pl == NIL)
    pl = (*names) + len;
  if (pr == NIL)
    pr = (*names) + len;
  if (pc == NIL)
    pc = (*names) + len;
  pe = (pr < pl) ? pr : pl;
  pe = (pc < pe) ? pc : pe;
  cr = *pe;
  *pe = '\0';
  n = (ID_DAG) ilu_hash_FindInTable(UnknownTypeIDs, *names);
  if (n == NIL) {
    ilu_string      id = _ilu_Strdup(*names);
    n = (ID_DAG) ilu_malloc(sizeof(ID_DAG_s));
    _ilu_Assert((int) ilu_hash_AddToTable(UnknownTypeIDs, id, n),
		"ClassNamesToID_DAG AddToTable UnknownTypeIDs");
    n->id = id;
    n->it = (ilu_Class) ilu_hash_FindInTable(ClassIDTable, id);
    n->supers = NIL;
    n->mska = n->it;
    n->computed = n->saak = n->mska != NIL;
    n->anext = n->aprev = NIL;
    if (cr == '(') {
      ID_DAG_List    *pp = &(n->supers);
      *names = pl + 1;
      while ((**names) != ')') {
	ID_DAG          d2 = ClassNamesToID_DAG(names);
	ID_DAG_List     l = (ID_DAG_List) ilu_malloc(sizeof(ID_DAG_Cons));
	l->idd = d2;
	l->next = NIL;
	*pp = l;
	pp = &(l->next);
	if ((**names) == ',')
	  (*names)++;
      }
      (*names)++;
    } else
      *names = pe;
  } else if (pl < pc) {		/* check for consistency */
    ID_DAG_List     l = n->supers;
    *names = pl + 1;
    while ((**names) != ')') {
      ID_DAG          d2 = ClassNamesToID_DAG(names);
      ASSERT(d2 == l->idd, buf,
	     (buf, "ClassNamesToID_DAG:  %s %s: %s vs. %s.\n",
	      "Disagreement on superclasses of", n->id, d2->id,
	      l->idd->id));
      l = l->next;
      if ((**names) == ',')
	(*names)++;
    }
    (*names)++;
  } else
    *names = pe;
  *pe = cr;
  return (n);
}

static void 
ClearVisited(ID_DAG t)
{
  ID_DAG_List     p;
  t->visited = ilu_FALSE;
  for (p = t->supers; p != NIL; p = p->next)
    ClearVisited(p->idd);
  return;
}

/* L1 >= {otmu} */

static void 
AddDAGNode (ID_DAG node, iluBuffer buf)
{
  ilu_cardinal	i;
  int		noco = 1;
  ID_DAG_List	p;

  addBytesToBuf(buf, node->id, strlen(node->id));
  if (node->visited != ilu_TRUE) {
    node->visited = ilu_TRUE;
    if (node->supers == NIL)
      return;
    addBytesToBuf(buf, "(", 1);
    for (p = node->supers;  p != NIL;  p = p->next) {
      if (noco)
	noco = 0;
      else
	addBytesToBuf(buf, ",", 1);
      AddDAGNode(p->idd, buf);
    }
    addBytesToBuf(buf, ")", 1);
  }
  return;
}

/* L1_sup > {otmu} */
static ilu_string 
ID_DAGtoString(ID_DAG dag)
{
  struct _iluBuffer_s buf;
  buf.allocated = BUF_INCREMENT;
  buf.buf = ilu_malloc(BUF_INCREMENT);
  buf.used = 0;
  ClearVisited(dag);
  AddDAGNode(dag, &buf);
  addBytesToBuf(&buf, "\0", 1);	/* NUL-terminate */
  return (buf.buf);
}

/*
 * Before and after: every node in the mska list is visited; for
 * each visited node n: 1. if n is known then n is an ancestor of a
 * node in the mska list; 2. every ancestor of n is visited.
 */

/*
 * As above, plus... After: all of t's ancestors are visited, and
 * none are in the list.
 */
static void 
MarkAncestors(ID_DAG t)
{
  ID_DAG_List     p;
  for (p = t->supers; p != NIL; p = p->next) {
    ID_DAG          u = p->idd;
    if (u->anext != NIL) {
      /*
       * must already be visited, and have no proper ancestors in
       * the mska list; remove from list.
       */
      u->aprev->anext = u->anext;
      u->anext->aprev = u->aprev;
    } else if (!(u->visited && (u->it != NIL))) {
      MarkAncestors(u);
      u->visited = ilu_TRUE;
    }
  }
  return;
}

static void 
FindMostSpecificType(ID_DAG t)
{
  ID_DAG_List     p;
  ILU_NOTE((TYPE_DEBUG | OBJECT_DEBUG),
	("ILU(FindMostSpecificType):  at %s, %s visited.\n",
	 t->id, (t->visited == ilu_FALSE) ? "not" : ""));
  if (t->visited == ilu_TRUE)
    return;
  if (t->it != NIL) {
    MarkAncestors(t);
    t->anext = mska_head.anext;
    t->aprev = &mska_head;
    t->anext->aprev = t;
    t->aprev->anext = t;
  } else {
    for (p = t->supers; p != NIL; p = p->next)
      FindMostSpecificType(p->idd);
  }
  t->visited = ilu_TRUE;
  return;
}

#ifdef ILU_GENERATE_SUBTYPES
/* L1_sup < otmu */
static ilu_Class
  CreateNewClass (char *typeid)
{
  ilu_string *supertype_ids;
  int i, nsupertypes;
  ilu_Error lerr;
  ilu_Class c;
  ID_DAG ptr;

  /* count the supertypes */
  for (ptr = mska_head.anext, nsupertypes = 0;  ptr != NIL && ptr != &mska_head;  ptr = ptr->anext) {
    nsupertypes++;
  }
  /* allocate the supertype type ID vector */
  supertype_ids = (ilu_string *) ilu_malloc(sizeof(ilu_string) * nsupertypes);
  if (supertype_ids == NIL)
    return NIL;
  /* fill in the allocated type ID vector */
  for (ptr = mska_head.anext, i = 0;  ptr != NIL && ptr != &mska_head;  ptr = ptr->anext, i++) {
    supertype_ids[i] = class_unique_id(ptr->it);
  }

  ILU_NOTE(TYPE_DEBUG,
	   ("ILU(type.c/CreateNewClass):  creating phony kernel class for type ID %s with %lu supertypes\n",
	    typeid, nsupertypes));

  /* register the type */
  ilu_AcquireMutex (ilu_otmu);
  c = ilu_DefineObjectType (ilu_InventID(),	/* make up name */
			    NIL,		/* no brand */
			    typeid,		/* use passed typeid */
			    NIL,		/* not singleton */
			    ilu_FALSE,		/* not optional */
			    ilu_FALSE,		/* not collectible */
			    NIL,		/* no doc string */
			    0,			/* no methods */
			    nsupertypes,
			    supertype_ids,	/* type IDs of supertypes */
#ifdef ILU_HTTPNG_OBJECTS
			    0,
			    ilu_FALSE, ilu_FALSE,
#endif /* def ILU_HTTPNG_OBJECTS */
			    &lerr);
  if (ILU_ERRNOK(lerr)) {
    ILU_HANDLED(lerr);
    ilu_free(supertype_ids);
    ilu_ReleaseMutex(ilu_otmu);
    return NIL;
  };
  c->cl_phony = ilu_TRUE;
  ilu_ObjectTypeDefined (c, &lerr);
  if (ILU_ERRNOK(lerr)) {
    ILU_HANDLED(lerr);
    ilu_ReleaseMutex(ilu_otmu);
    return NIL;
  };
  ilu_ReleaseMutex(ilu_otmu);
  return c;
}
#endif

/* L1_sup < otmu */
ilu_Class 
  _ilu_StringToClass(ilu_string s)
{
  ID_DAG          t;
  char           *q = s;
  ilu_Class       ans;
  ilu_Error	lerr;

  if (s == NIL)
    return (NIL);
  _ilu_AcquireMutex(ilu_otmu);
  if (UnknownTypeIDs == NIL)
    UnknownTypeIDs = (ilu_refany) ilu_hash_MakeNewTable(
					      CLASS_HASHTABLESIZE,
			     (ilu_hashfnptr) ilu_hash_HashString,
			 (ilu_compfnptr) ilu_hash_StringCompare);
  t = ClassNamesToID_DAG(&q);
  if (t == NIL)
    return (NIL);
  ILU_NOTE((TYPE_DEBUG | OBJECT_DEBUG),
  ("(_ilu_StringToClass):  Converted names <%s> to DAG.\n", s));
  if (t->computed) {
    ILU_NOTE((TYPE_DEBUG | OBJECT_DEBUG), ("(ID_DAGtoClass):  Old problem.\n"));
  } else {
    ClearVisited(t);
    mska_head.anext = mska_head.aprev = &mska_head;
    FindMostSpecificType(t);
    t->saak = mska_head.anext != &mska_head;
    if (t->saak && (mska_head.anext->anext == &mska_head))
      t->mska = mska_head.anext->it;
    else
      t->mska = NIL;
    t->computed = ilu_TRUE;
  }
  ans = t->mska;
  _ilu_ReleaseMutex(ilu_otmu);
  if (ans == NIL) {
#ifdef ILU_GENERATE_SUBTYPES
    /* list of supertypes is hanging off mska_head right now */
    ans = CreateNewClass (t->id);
#else
#ifdef ENABLE_DEBUGGING
    ID_DAG ptr;
    ILU_NOTE((TYPE_DEBUG | OBJECT_DEBUG),
	     ("(_ilu_StringToClass):  %s ancestors of type <%s> are known:\n",
	      t->saak ? "Multiple" : "No", t->id));
    for (ptr = &mska_head;  ptr != NIL;  ptr = ptr->anext) {
      if ((ptr->id != NIL) && (ptr->it != NIL)) {
	ILU_NOTE((TYPE_DEBUG | OBJECT_DEBUG),
		 ("       %s (%s)\n", ptr->it->cl_name, ptr->id));
      };
      if (ptr->anext == &mska_head)
	break;
    }
#endif /* def ENABLE_DEBUGGING */
#endif /* def ILU_GENERATE_SUBTYPES */
  } else {
    ILU_NOTE((TYPE_DEBUG | OBJECT_DEBUG),
	  ("(_ilu_StringToClass):  Found class %s.\n",
	   class_name(ans)));
  }
  return ans;
}

/* L1_sup < otmu */
ilu_string
  _ilu_MSTIDToStringifiedDAG (ilu_string mstid)
{
  ilu_Class c;
  ID_DAG d;
  ilu_string s = NIL;

  if ((c = ilu_FindClassFromID(mstid)) != ILU_NIL)
    return _ilu_ClassToString(c);
  else {
    ilu_AcquireMutex(ilu_otmu);
    if ((UnknownTypeIDs != NIL) &&
	((d = ilu_hash_FindInTable(UnknownTypeIDs, mstid)) != NIL))
      s = ID_DAGtoString (d);
    ilu_ReleaseMutex(ilu_otmu);
    return s;
  }
}

/* Main Invariant holds; L2 otherwise unconstrained */

static          ilu_boolean
ObtainTypes(ilu_Object o, ilu_string * types, ilu_cardinal * typeslen,
	    ILU_ERRS((bad_locks, inv_objref,
		      no_resources, IoErrs)) * err)
{
  ilu_Call_s      call_s;
  ilu_Call        call = &call_s;
  ilu_cardinal    reqSize;
  ilu_cardinal    estatus = 0;
  ilu_ProtocolException internal;
  ilu_Server      s = object_server(o);
  ilu_Connection  newconn;
  ilu_boolean     ans;
  ILU_NOTE(OBJECT_DEBUG, ("_ilu_FindClassViaRPC:  object %p...\n",
		       o));
  ans = ilu_StartCall(call, s, _ilu_rootClass, _ilu_GetTypesMethod, 0,
		      NIL, &newconn, err);
  if (newconn != NIL)
    _ilu_HandOffNewConnection(newconn, err);
  if (ILU_ERRNOK(*err))
    return ilu_FALSE;

 retry:

  _ilu_AcquireServerMutex(s);
  reqSize = ilu_SizeOfObjectID(call, o, ilu_TRUE, _ilu_rootClass, err);
  _ilu_ReleaseServerMutex(s);
  if (ILU_ERRNOK(*err))
    goto faild;
  if (!ilu_StartRequest(call, reqSize, err))
    goto faild;
  ilu_EnterServer(s, object_class(o));
  ilu_OutputObjectID(call, o, ilu_TRUE, _ilu_rootClass, err);
  if (ILU_ERRNOK(*err))
    goto faild;
  if (!ilu_FinishRequest(call, err))
    goto faild;
  internal = ilu_GetReply(call, &estatus, &newconn, err);
  if (newconn != NIL)
    _ilu_HandOffNewConnection(newconn, err);
  if (ILU_ERRNOK(*err) &&
	   (err->ilu_type == ILU_ERRTYP(transient)) &&
	   (ILU_ERRSEL(transient,*err).minor == ilu_tm_retry)) {
    ILU_HANDLED(*err);
    ILU_CLER(*err);
    goto retry;
  };
  if (ILU_ERRNOK(*err))
    goto faild;
  if (internal != ilu_ProtocolException_Success) {
    (void) ILU_ERR_CONS1(internal, err, minor, ilu_im_callFail, 0);
    goto faild;
  }
  if (estatus != 0) {
    (void) ILU_ERR_CONS1(internal, err, minor, ilu_im_callFail, 0);
    goto faild;
  }
  ilu_InputString(call, types, typeslen, 0xFFFF, err);
  if (ILU_ERRNOK(*err))
    goto faild;
  ilu_ReplyRead(call, err);
  if (ILU_ERRNOK(*err))
    goto faild;
faild:
  ilu_FinishCall(call, err);
  if (ILU_ERRNOK(*err)) {
    ILU_HANDLED(*err);
    return ilu_FALSE;
  }
  return ilu_TRUE;
}

ilu_Class 
_ilu_FindClassViaRPC(ilu_Object o)
{
  ilu_string      types = NIL;
  ilu_cardinal    typeslen;
  ilu_Class       c = NIL;
  ID_DAG	  dag = NIL;
  ilu_Class       pclass = object_class(o);
  ILU_ERRS((bad_locks, inv_objref,
		      no_resources, IoErrs)) lerr;

  if (class_singleton(pclass)) {
    ILU_NOTE(OBJECT_DEBUG,
	  ("%s %s is singleton, not attempting GetTypes RPC call.\n",
	   "_ilu_FindClassViaRPC:  pclass", class_name(pclass)));
    return (NIL);
  }

  _ilu_Assert(!server_is_true(object_server(o)), "_ilu_FindClassViaRPC: called on true object");
  
  ilu_AcquireMutex(ilu_otmu);
  
  if ((object_mstid(o) != NIL) && (UnknownTypeIDs != NIL))
    dag = ilu_hash_FindInTable (UnknownTypeIDs, object_mstid(o));
  ilu_ReleaseMutex(ilu_otmu);
  if (dag != NIL) {
    if (dag->computed)
      c = dag->mska;
  } else {
    if (!ObtainTypes(o, &types, &typeslen, &lerr)) {
      ILU_NOTE(OBJECT_DEBUG,
	       ("_ilu_FindClassViaRPC:  no types.  "
		"ILUGetTypes call raised %s from %s:%d\n",
		ILU_ERR_NAME(lerr), ilu_ErrorFile(&lerr),
		ilu_ErrorLine(&lerr)));
    } else
      ILU_NOTE(OBJECT_DEBUG,
	       ("_ilu_FindClassViaRPC:  typestring is <%s>...\n",
		types));
    
    if (types != NIL) {
      c = _ilu_StringToClass(types);
      FREETOKEN(types);
    }
  }
  ILU_NOTE(OBJECT_DEBUG,
	   ("_ilu_FindClassViaRPC:  class is \"%s\".\n",
	    (c == NIL) ? "*unknown*" : class_name(c)));
  return (c);
}

/* L2    >=    {conn's callmu, iomu} before;
 * L2 disjoint {conn's callmu, iomu} after */
void 
_ilu_HandleGetTypes(ilu_Call call)
{
  ilu_Object      disc;
  ilu_string      names = NIL;
  ilu_Error       lerr = ILU_INIT_NO_ERR;
  ilu_InputObjectID(call, &disc, ilu_TRUE, _ilu_rootClass, &lerr);
  if (ILU_ERRNOK(lerr))
    goto errout;
  if (disc != NIL) {
    lerr = _ilu_DeltaHolds(disc, 1);
    ILU_MUST_BE_SUCCESS(lerr);
    ilu_ExitServer(object_server(disc), _ilu_rootClass);
  }
  if (!ilu_RequestRead(call, &lerr))
    goto errout;
  if (disc != NIL && object_class(disc) != NIL)
    names = _ilu_ClassToString(object_class(disc));
  if (names == NIL) {
    ilu_cardinal asize =
      ilu_BeginSizingException (call, - (ilu_integer) ilu_ProtocolException_GarbageArguments, &lerr);
    if (ILU_ERRNOK(lerr)) goto errout;
    if (!ilu_BeginException(call, - (ilu_integer) ilu_ProtocolException_GarbageArguments,
			    asize, &lerr))
      goto errout;
    if (!ilu_FinishException(call, &lerr))
      goto errout;
  } else {
    ilu_cardinal    alen, len = strlen(names);
    alen = ilu_BeginSizingReply (call, ilu_FALSE, &lerr);
    if (ILU_ERRNOK(lerr))
      goto errout;
    alen += ilu_SizeOfString(call, names, len, 0xFFFF, &lerr);
    if (ILU_ERRNOK(lerr))
      goto errout;
    if (!ilu_BeginReply(call, ilu_FALSE, alen, &lerr))
      goto errout;
    ilu_OutputString(call, names, len, 0xFFFF, &lerr);
    if (ILU_ERRNOK(lerr))
      goto errout;
    if (!ilu_FinishReply(call, &lerr))
      goto errout;
  }
dun:
  ILU_HANDLED(lerr);
  if (disc != NIL) {
    ilu_Server      s = object_server(disc);
    ilu_Class       cl = object_class(disc);
    ilu_EnterServer(s, cl);
    lerr = _ilu_DeltaHolds(disc, -1);
    ILU_MUST_BE_SUCCESS(lerr);
    ilu_ExitServer(s, cl);
  }
  return;

 errout:
  ILU_ERR_SWITCH(lerr) {
    ILU_ERR_CASE2(inv_objref, marshal) {
      call->ca_pe = ilu_ProtocolException_GarbageArguments;
    }
    ILU_ERR_ELSE {
      call->ca_pe = ilu_ProtocolException_Unknown;
    }
  } ILU_ERR_ENDSWITCH;
  goto dun;
}

/* L2    >=    {conn's callmu, iomu} before;
 * L2 disjoint {conn's callmu, iomu} after */
static void 
_ilu_HandleIsA(ilu_Call call)
{
  ilu_Object      disc;
  ilu_string      typeid = NIL;
  ilu_cardinal    typeid_len = 0;
  ilu_Error       lerr = ILU_INIT_NO_ERR;
  ilu_Class       qtype;

  ilu_InputObjectID(call, &disc, ilu_TRUE, _ilu_rootClass, &lerr);
  if (ILU_ERRNOK(lerr))
    goto errout;
  if (disc != NIL) {
    lerr = _ilu_DeltaHolds(disc, 1);
    ILU_MUST_BE_SUCCESS(lerr);
    ilu_ExitServer(object_server(disc), _ilu_rootClass);
    ilu_InputString(call, &typeid, &typeid_len, 0xFFFF, &lerr);
    if (ILU_ERRNOK(lerr))
      goto errout;
  }
  if (!ilu_RequestRead(call, &lerr))
    goto errout;

  if (disc == NIL || object_class(disc) == NIL || typeid == NIL) {
    ilu_cardinal asize =
      ilu_BeginSizingException (call, - (ilu_integer) ilu_ProtocolException_GarbageArguments, &lerr);
    if (ILU_ERRNOK(lerr)) goto errout;
    if (!ilu_BeginException(call, - (ilu_integer) ilu_ProtocolException_GarbageArguments,
			    asize, &lerr))
      goto errout;
    if (!ilu_FinishException(call, &lerr))
      goto errout;
  } else {
    ilu_cardinal    alen;
    ilu_boolean result = ((qtype = ilu_FindClassFromID(typeid)) != NIL &&
			  ilu_IsSubObjectType(object_class(disc), qtype));
    alen = ilu_BeginSizingReply (call, ilu_FALSE, &lerr);
    if (ILU_ERRNOK(lerr))
      goto errout;
    alen += ilu_SizeOfBoolean(call, result, &lerr);
    if (ILU_ERRNOK(lerr))
      goto errout;
    if (!ilu_BeginReply(call, ilu_FALSE, alen, &lerr))
      goto errout;
    ilu_OutputBoolean(call, result, &lerr);
    if (ILU_ERRNOK(lerr))
      goto errout;
    if (!ilu_FinishReply(call, &lerr))
      goto errout;
  }
dun:
  if (typeid != NIL)
    ilu_free(typeid);
  ILU_HANDLED(lerr);
  if (disc != NIL) {
    ilu_Server      s = object_server(disc);
    ilu_Class       cl = object_class(disc);
    ilu_EnterServer(s, cl);
    lerr = _ilu_DeltaHolds(disc, -1);
    ILU_MUST_BE_SUCCESS(lerr);
    ilu_ExitServer(s, cl);
  }
  return;

 errout:
  ILU_ERR_SWITCH(lerr) {
    ILU_ERR_CASE2(inv_objref, marshal) {
      call->ca_pe = ilu_ProtocolException_GarbageArguments;
    }
    ILU_ERR_ELSE {
      call->ca_pe = ilu_ProtocolException_Unknown;
    }
  } ILU_ERR_ENDSWITCH;
  goto dun;
}

