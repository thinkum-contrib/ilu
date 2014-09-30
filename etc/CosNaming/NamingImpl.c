/*
 Simple Cos Naming implementation
*/
/* Chris Jacobi, October 8, 1998 2:47 pm PDT */

/*
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
*/ 


#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <errno.h>	/* for errno */

/* for gethostname */
#ifdef WIN32
#include <winsock.h>
#else
#include <unistd.h>
#endif


#include "CosNaming.h"

#include "iluhash.h"

#define PASS(x)		x
#define OPTIONAL(x)	x

#define NULLFN	0

/* number of slots in hash table (shrug) */ 
#define CONTEXT_NBUCKETS	23

static ILU_C_Server theServer;
static void UpdateBackingStore (void);
static void AddObject (CosNaming_NamingContext);
static void RemoveObject (CosNaming_NamingContext);
static ilu_boolean LocalObject (CosNaming_NamingContext);

static ilu_boolean	Verbose	= ilu_FALSE;

#ifndef HAVE_GETHOSTNAME_PROTOTYPE
extern int gethostname(char *name, int namelen);
#endif /* HAVE_GETHOSTNAME_PROTOTYPE */

void debugprint(char *formatSpec, ...)
{
  va_list ap;
  va_start (ap, formatSpec);
  if (Verbose)
    (void) vfprintf(stderr, formatSpec, ap);
  va_end(ap);
}

static void *
MyMalloc (ilu_cardinal size, ILU_C_ENVIRONMENT *env)
{
  void *ptr;

  if ((ptr = ilu_malloc(size)) == ILU_NIL)
    ILU_C_RAISE_SYSTEM(env, NO_MEMORY, size, NO);
  else
    ILU_C_SET_SUCCESSFUL(env);
  return ptr;
}

static char *
MyStrdup (char *str, ILU_C_ENVIRONMENT *env)
{
  unsigned        len, fullen, i;
  ilu_CString     s2;

  if (str == ILU_NIL)
    return (ILU_NIL);
  len = strlen(str);
  fullen = len + 4 - (len % 4);
  s2 = ilu_malloc(fullen);
  if (s2 == ILU_NIL) {
    ILU_C_RAISE_SYSTEM(env, NO_MEMORY, fullen, NO);
    return ILU_NIL;
  };
  strcpy(s2, str);
  for (i = len + 1; i < fullen; i++)
    s2[i] = 0;
  ILU_C_SET_SUCCESSFUL(env);
  return (s2);
}

static CosNaming_Name
NameFromNameComponent (CosNaming_NameComponent *s, ILU_C_ENVIRONMENT *env)
{
  CosNaming_Name newname;
  newname._maximum = 1;
  newname._length = 1;
  newname._buffer = CosNaming_NameComponent__alloc();
  if (newname._buffer == ILU_NIL) {
    ILU_C_RAISE_SYSTEM(env, NO_MEMORY, sizeof(CosNaming_NameComponent), NO);
  } else {
    if ((newname._buffer[0].id = MyStrdup(s->id, env)) != ILU_NIL) {
      newname._buffer[0].kind = MyStrdup(s->kind, env);
    }
  }
  return newname;
}

static CosNaming_Name
CopyName (CosNaming_Name *in, ilu_cardinal offset, ILU_C_ENVIRONMENT *env)
{
  CosNaming_Name newname;
  CosNaming_NameComponent *new_component, *in_component;
  ilu_cardinal i, limit;

  limit = CosNaming_Name_Length(in);
  if (offset >= limit) {
    newname._maximum = 0;
    newname._length = 0;
    newname._buffer = ILU_NIL;
    ILU_C_SET_SUCCESSFUL(env);
    return newname;
  };
  limit -= offset;
  CosNaming_Name_Init(&newname, limit, ILU_NIL);
  if (newname._buffer == ILU_NIL) {
    ILU_C_RAISE_SYSTEM(env, INTERNAL, 0, NO);
    return newname;
  } else {
    for (i = 0;  i < limit;  i++) {
      new_component = newname._buffer + i;
      in_component = CosNaming_Name_Nth(in, i + offset);
      new_component->id = MyStrdup(in_component->id, env);
      if (! ILU_C_SUCCESSFUL(env)) return newname;
      new_component->kind = MyStrdup(in_component->kind, env);
      if (! ILU_C_SUCCESSFUL(env)) return newname;
    }
    newname._length = limit;
    ILU_C_SET_SUCCESSFUL(env);
    return newname;
  }
}

/*************************************************************************/
/************        Exceptions implementation        ********************/
/*************************************************************************/

static void
RaiseNotFound (ILU_C_ENVIRONMENT *env,
	       CosNaming_Name *bad_name,
	       ilu_cardinal bad_segment,
	       CosNaming_NamingContext_NotFoundReason why)
{
  CosNaming_NamingContext_NotFound *nf;
  CORBA_Environment localenv;

  env->returnCode = ex_CosNaming_NamingContext_NotFound;
  env->_major = ILU_C_USER_EXCEPTION;
  env->freeRoutine = (ILU_C_FreeFn) CosNaming_NamingContext_NotFound__Free;
  nf = MyMalloc(sizeof(CosNaming_NamingContext_NotFound), &localenv);
  if (nf == ILU_NIL) {
    return;
  } else {
    nf->why = why;
    nf->rest_of_name = CopyName(bad_name, bad_segment, &localenv);
    env->ptr = (ilu_refany) nf;
  }
}

static void
RaiseCannotProceed (ILU_C_ENVIRONMENT *env,
		    CosNaming_Name *bad_name,
		    ilu_cardinal bad_segment,
		    CosNaming_NamingContext ctx)
{
  CosNaming_NamingContext_CannotProceed *cp;
  ILU_C_ENVIRONMENT localenv;

  env->returnCode = ex_CosNaming_NamingContext_NotFound;
  env->_major = ILU_C_USER_EXCEPTION;
  env->freeRoutine = (ILU_C_FreeFn) CosNaming_NamingContext_CannotProceed__Free;
  cp = MyMalloc(sizeof(CosNaming_NamingContext_NotFound), &localenv);
  if (cp == ILU_NIL) {
    return;
  } else {
    cp->cxt = (CosNaming_NamingContext) CORBA_Object_duplicate(ctx, &localenv);
    if (! ILU_C_SUCCESSFUL(&localenv)) {
      ilu_free(cp);
      ILU_C_RAISE_SYSTEM(env, INTERNAL, 0, NO);
      return;
    }
    cp->rest_of_name = CopyName(bad_name, bad_segment, &localenv);
    env->ptr = (ilu_refany) cp;
  }
}

static void
RaiseInvalidName (ILU_C_ENVIRONMENT *env)
{
  env->returnCode = ex_CosNaming_NamingContext_InvalidName;
  env->_major = ILU_C_USER_EXCEPTION;
  env->ptr = ILU_NIL;
  env->freeRoutine = (void (*) (void *)) 0;
}

static void
RaiseAlreadyBound (ILU_C_ENVIRONMENT *env)
{
  env->returnCode = ex_CosNaming_NamingContext_AlreadyBound;
  env->_major = ILU_C_USER_EXCEPTION;
  env->ptr = ILU_NIL;
  env->freeRoutine = (void (*) (void *)) 0;
}

static void
RaiseNotEmpty (ILU_C_ENVIRONMENT *env)
{
  env->returnCode = ex_CosNaming_NamingContext_NotEmpty;
  env->_major = ILU_C_USER_EXCEPTION;
  env->ptr = ILU_NIL;
  env->freeRoutine = (void (*) (void *)) 0;
}


/*************************************************************************/
/********        BindingIterator implementation        *******************/
/*************************************************************************/
			       
typedef struct _BIData_s {
  ilu_cardinal			index;
  CosNaming_BindingList		*list;
} BIData;

CORBA_boolean
server_CosNaming_BindingIterator_next_one(CosNaming_BindingIterator _handle,
					  CosNaming_Binding** b,
					  ILU_C_ENVIRONMENT *_status)
{
  BIData *bid = CosNaming_BindingIterator__GetUserData(_handle);
  CORBA_boolean return_status;
  ilu_cardinal n;
  CosNaming_Binding *the_element;
  CosNaming_Binding *new_binding;

  if (bid == ILU_NIL) {
    ILU_C_RAISE_SYSTEM(_status, INTERNAL, 0, NO);
    return ilu_FALSE;
  };
  new_binding = (CosNaming_Binding *) MyMalloc(sizeof(CosNaming_Binding), _status);
  if (! ILU_C_SUCCESSFUL(_status)) return ilu_FALSE;
  if (bid->index >= CosNaming_BindingList_Length(bid->list)) {
    return_status = ilu_FALSE;
    n = CosNaming_BindingList_Length(bid->list) - 1;
  } else {
    n = bid->index;
    bid->index++;
    return_status = ilu_TRUE;
  }
  the_element = CosNaming_BindingList_Nth(bid->list, n);
  new_binding->binding_type = the_element->binding_type;
  new_binding->binding_name = CopyName (&the_element->binding_name, 0, _status);
  *b = new_binding;
  return return_status;
}

CORBA_boolean
server_CosNaming_BindingIterator_next_n(CosNaming_BindingIterator _handle,
					CORBA_unsigned_long how_many,
					CosNaming_BindingList** bl,
					ILU_C_ENVIRONMENT *_status)
{
  BIData *bid = CosNaming_BindingIterator__GetUserData(_handle);
  ilu_cardinal n, size, i;
  CosNaming_Binding *newbinding, *oldbinding;
  CosNaming_BindingList *new_list;

  if (bid == ILU_NIL) {
    ILU_C_RAISE_SYSTEM(_status, INTERNAL, 0, NO);
    return ilu_FALSE;
  };
  size = CosNaming_BindingList_Length(bid->list) - bid->index;
  n = (size > how_many) ? how_many : size;
  new_list = CosNaming_BindingList_Create (n, ILU_NIL);
  for (i = 0;  i < n;  i++) {
    newbinding = CosNaming_BindingList_Nth(new_list, i);
    oldbinding = CosNaming_BindingList_Nth(bid->list, i + bid->index);
    newbinding->binding_type = oldbinding->binding_type;
    newbinding->binding_name = CopyName (&oldbinding->binding_name, 0, _status);
    if (!ILU_C_SUCCESSFUL(_status)) { CosNaming_BindingList__Free(new_list); ilu_free(new_list); return ilu_FALSE; };
  };
  new_list->_length = n;
  bid->index += n;
  *bl = new_list;
  return ilu_TRUE;
}

void
server_CosNaming_BindingIterator_destroy(CosNaming_BindingIterator _handle,
					 ILU_C_ENVIRONMENT *_status)
{
  BIData *bid = CosNaming_BindingIterator__GetUserData(_handle);

  if (bid == ILU_NIL) {
    ILU_C_RAISE_SYSTEM(_status, INTERNAL, 0, NO);
    return;
  };
  CosNaming_BindingList__Free(bid->list);
  ilu_free(bid->list);
  ilu_free(bid);
  CORBA_Object_release(_handle, _status);
}

/*************************************************************************/
/********        NamingContext implementation         ********************/
/*************************************************************************/
			       
/*
 * Each NamingContext is represented as an ILU hashtable, mapping a key,
 * which is the name, to a value, which is either a NamingContext or an
 * Object.  The struct NamedValue distinguishes the two cases.
 */

typedef struct _NamedValue_s {
  CosNaming_NameComponent	key;
  CosNaming_BindingType		kind;
  ilu_boolean			local;
  union {
    ilu_string			sbh;
    CosNaming_NamingContext	obj;
  } value;
} NamedValue;

static ilu_HashTable
NewContextHashTable (ilu_cardinal entries)
{
  return (ilu_hash_MakeNewTable ((entries == 0) ? CONTEXT_NBUCKETS : entries,
				 ilu_hash_HashString, ilu_hash_StringCompare));
}

static CosNaming_NamingContext NewNamingContext (ilu_string ih,
						 ILU_C_Server server,
						 ilu_cardinal entries,
						 ILU_C_ENVIRONMENT *_status)
{
  static unsigned long next_def_ih = 1;
  char idbuf[12];
  CosNaming_NamingContext newcontext;
  ilu_HashTable ht;

  ht = NewContextHashTable(entries);
  if (ht == ILU_NIL) {
    ILU_C_RAISE_SYSTEM(_status, NO_MEMORY, 0, NO);
    return ILU_NIL;
  };

  if (!ih) {
     /* this dance is necessary to avoid creating an instance with
      * and instance handle that already exists (read from the
      * backing file at startup)
      */
     do {
	sprintf(idbuf, "%lu", next_def_ih++);
     } while (ILU_C_FindObject(ILU_C_IDOfServer(server), idbuf));
     ih = idbuf;
  }

  newcontext = CosNaming_NamingContext__CreateTrue(ih, server, (ilu_refany) ht);
  if (newcontext == ILU_NIL) {
    ILU_C_RAISE_SYSTEM(_status, INTERNAL, 0, NO);
    return ILU_NIL;
  };
  AddObject (newcontext);
  newcontext = CORBA_Object_duplicate (newcontext, _status);
  debugprint("ILUCosNaming:  Created new NamingContext %p (%s/%s)\n",
	  newcontext, ILU_C_IDOfServer(server), ih);
  return newcontext;
}

static CosNaming_NamingContext
FindOrCreateNamingContext (ilu_string ih,
			   ILU_C_Server server,
			   ilu_cardinal entries,
			   ILU_C_ENVIRONMENT *_status)
{
  ILU_C_Object *obj;

  obj = ILU_C_FindObject (ILU_C_IDOfServer(server), ih);
  if (obj == ILU_NIL) {
    obj = NewNamingContext (ih, server, entries, _status);
  } else {
    ILU_C_SET_SUCCESSFUL(_status);
  }
  return obj;
}

static void
generic_bind(CosNaming_NamingContext _handle,
	     CosNaming_Name* n,
	     ilu_CORBA_Object obj,
	     CosNaming_BindingType type,
	     ILU_C_ENVIRONMENT *_status)
{
  ilu_HashTable ht = CosNaming_NamingContext__GetUserData(_handle);
  CosNaming_Name tempname;
  ilu_cardinal size;
  NamedValue *nv;

  if (ht == ILU_NIL) {
    debugprint("ILUCosNaming:  Context %p has no hashtable!\n", _handle);
    RaiseInvalidName(_status);
    return;
  };
  if ((size = CosNaming_Name_Length(n)) == 0) {
    debugprint("ILUCosNaming:  generic_bind:  Zero-length name\n");
    RaiseInvalidName(_status);
  } else if (strlen(n->_buffer[0].id) == 0) {
    debugprint("ILUCosNaming:  generic_bind:  Zero-length id in first element of name\n");
    RaiseInvalidName(_status);
  } else if (obj == ILU_NIL) {
    /* we don't allow binding of the NIL object */
    debugprint("ILUCosNaming:  generic_bind:  Binding of NIL object not allowed\n");
    ilu_DebugPrintf("CosNaming:  attempt to bind the NIL object -- raising IMP_LIMIT\n");
    ILU_C_RAISE_SYSTEM(_status, IMP_LIMIT, 0, NO);
  } else if (LocalObject((CosNaming_NamingContext) obj) &&
	     (ILU_C_ClassRecordOfInstance(obj) != CosNaming_NamingContext__MSType)) {
    /* we don't allow binding of the bindingiterators exported from this server */
    debugprint("ILUCosNaming:  generic_bind:  Attempt to bind local binding interator %p.\n", obj);
    ilu_DebugPrintf("CosNaming:  attempt to bind local binding iterator -- raising IMP_LIMIT\n");
    ILU_C_RAISE_SYSTEM(_status, IMP_LIMIT, 0, NO);
  } else if (size == 1) {
    if ((nv = ilu_hash_FindInTable(ht, n->_buffer[0].id)) != ILU_NIL) {
      debugprint("ILUCosNaming:  generic_bind:  Id \"%s\" already bound.\n", n->_buffer[0].id);
      RaiseAlreadyBound(_status);
    } else if ((nv = (NamedValue *) MyMalloc(sizeof(*nv), _status)) != ILU_NIL) {
      nv->kind = type;
      nv->key.id = MyStrdup(n->_buffer[0].id, _status);
      if (!ILU_C_SUCCESSFUL(_status))
	{ ilu_free(nv); return; };
      nv->key.kind = MyStrdup(n->_buffer[0].kind, _status);
      if (!ILU_C_SUCCESSFUL(_status))
	{ ilu_free(nv->key.id); ilu_free(nv); return; };
      if (LocalObject(obj)) {
	nv->local = ilu_TRUE;
	nv->value.obj = obj;
	debugprint("ILUCosNaming:  generic_bind:  Bound local object %p to \"%s\".\n",
		obj, n->_buffer[0].id);
      } else {
	ilu_string sbh;
	nv->local = ilu_FALSE;
	sbh = ILU_C_SBHOfObject(obj);
	if (sbh == ILU_NIL) {
	  ILU_C_RAISE_SYSTEM(_status,INTERNAL,0,NO);
	  ilu_free(nv->key.kind); ilu_free(nv->key.id); ilu_free(nv); return;
	};
	nv->value.sbh = MyStrdup(sbh, _status);
	if (!ILU_C_SUCCESSFUL(_status)) {
	  ilu_free(nv->key.kind); ilu_free(nv->key.id); ilu_free(nv); return;
	};
	debugprint("ILUCosNaming:  generic_bind:  Bound remote object \"%s\" to \"%s\".\n",
		sbh, nv->key.id);
      }
      if (!ilu_hash_AddToTable(ht, nv->key.id, nv)) {
	ILU_C_RAISE_SYSTEM(_status, INTERNAL, 0, NO);
	if (!nv->local)
	  ilu_free(nv->value.sbh);
	ilu_free(nv->key.kind); ilu_free(nv->key.id); ilu_free(nv); return;
      }
      UpdateBackingStore();
    } else {
      RaiseCannotProceed (_status, n, 1, _handle);
    }
  } else {
     if ((nv = ilu_hash_FindInTable(ht, n->_buffer[0].id)) == ILU_NIL) {
	RaiseNotFound (_status, n, 0, CosNaming_missing_node);
     } else if (nv->kind != CosNaming_ncontext) {
	RaiseNotFound (_status, n, 0, CosNaming_not_context);
     } else {
       CosNaming_NamingContext	context;
       CORBA_Environment local;
       if (!nv->local) {
	 context = ILU_C_SBHToObject(nv->value.sbh, CosNaming_NamingContext__MSType, _status);
	 if (!ILU_C_SUCCESSFUL(_status)) return;
       } else
	 context = nv->value.obj;
       tempname._length = size - 1;
       tempname._maximum = size - 1;
       tempname._buffer = &n->_buffer[1];
       debugprint("ILUCosNaming:  generic_bind:  Recursing through \"%s\"; context is %p (%s)\n",
	       nv->key.id, context, (nv->local) ? "local" : "remote");
       generic_bind (context, &tempname, obj, type, _status);
       if (!nv->local) {
	 CORBA_Object_release(context,&local);
	 CORBA_exception_free(&local);
       }
     }
  }
}

void
server_CosNaming_NamingContext_bind(CosNaming_NamingContext _handle,
				    CosNaming_Name* n,
				    ilu_CORBA_Object obj,
				    ILU_C_ENVIRONMENT *_status)
{
   generic_bind(_handle, n, obj, CosNaming_nobject, _status);
}

static void
generic_rebind(CosNaming_NamingContext _handle,
	       CosNaming_Name* n,
	       ilu_CORBA_Object obj,
	       CosNaming_BindingType type,
	       ILU_C_ENVIRONMENT *_status)
{
  ilu_HashTable ht = CosNaming_NamingContext__GetUserData(_handle);

  CosNaming_NamingContext_unbind (_handle, n, _status);
  if (! ILU_C_SUCCESSFUL(_status)) {
    if ((_status->_major == CORBA_USER_EXCEPTION) &&
	(strcmp(CORBA_exception_id(_status), ex_CosNaming_NamingContext_NotFound) == 0)) {
      /* not a problem */
      CORBA_exception_free(_status);
      ILU_C_SET_SUCCESSFUL(_status);
    } else {
      return;
    }
  };
  generic_bind (_handle, n, obj, type, _status);
  return;    
}

void
server_CosNaming_NamingContext_rebind(CosNaming_NamingContext _handle,
				      CosNaming_Name* n,
				      ilu_CORBA_Object obj,
				      ILU_C_ENVIRONMENT *_status)
{
   generic_rebind(_handle, n, obj, CosNaming_nobject, _status);
}

void
server_CosNaming_NamingContext_bind_context(CosNaming_NamingContext _handle,
					    CosNaming_Name* n,
					    CosNaming_NamingContext nc,
					    ILU_C_ENVIRONMENT *_status)
{
   generic_bind(_handle, n, (CORBA_Object)nc, CosNaming_ncontext, _status);
}

void
server_CosNaming_NamingContext_rebind_context(CosNaming_NamingContext _handle,
					      CosNaming_Name* n,
					      CosNaming_NamingContext nc,
					      ILU_C_ENVIRONMENT *_status)
{
  generic_rebind (_handle, n, (CORBA_Object) nc, CosNaming_ncontext, _status);
}

ilu_CORBA_Object
server_CosNaming_NamingContext_resolve(CosNaming_NamingContext _handle,
				       CosNaming_Name* n,
				       ILU_C_ENVIRONMENT *_status)
{
  ilu_HashTable ht = CosNaming_NamingContext__GetUserData(_handle);
  CosNaming_Name tempname;
  NamedValue *nv;
  ilu_cardinal size = CosNaming_Name_Length(n);
  CosNaming_NameComponent *this_component;
  ilu_CORBA_Object temp;

  if (ht == ILU_NIL || size == 0) {
    debugprint("ILUCosNaming:  resolve:  Context %p has no hashtable!\n", _handle);
    RaiseInvalidName(_status);
    return ILU_NIL;
  };
  this_component = CosNaming_Name_Nth(n, 0);
  if ((nv = ilu_hash_FindInTable(ht, this_component->id)) == ILU_NIL) {
    debugprint("ILUCosNaming:  resolve:  Element \"%s\" not found.\n",
	    this_component->id);
    RaiseNotFound(_status, n, 0, CosNaming_missing_node);
    return ILU_NIL;
  };
  if (size > 1) {
    CORBA_Environment local;
    CosNaming_NamingContext	context;
    if (nv->kind != CosNaming_ncontext) {
      RaiseNotFound (_status, n, 0, CosNaming_not_context);
      return ILU_NIL;
    };
    if (nv->local)
      context = nv->value.obj;
    else {
      context = ILU_C_SBHToObject(nv->value.sbh, CosNaming_NamingContext__MSType, _status);
      if (!ILU_C_SUCCESSFUL(_status)) return ILU_NIL;
    }
    tempname._maximum = size - 1;
    tempname._length = size - 1;
    tempname._buffer = &n->_buffer[1];
    debugprint("ILUCosNaming:  resolve:  Recursing through \"%s\"; context is %p (%s)\n",
	    nv->key.id, context, (nv->local) ? "local" : "remote");
    temp = CosNaming_NamingContext_resolve (context, &tempname, _status);
    if (!nv->local) {
      CORBA_Object_release(context, &local);
      CORBA_exception_free(&local);
    }
    return temp;
  } else {
    if (nv->local) {
      debugprint("ILUCosNaming:  resolve:  for \"%s\", found local object %p\n",
	      nv->key.id, nv->value.obj);
      return CORBA_Object_duplicate(nv->value.obj, _status);
    } else if (nv->kind == CosNaming_ncontext) {
      debugprint("ILUCosNaming:  resolve:  For \"%s\", found remote context \"%s\"\n",
	      nv->key.id, nv->value.sbh);
      return ILU_C_SBHToObject(nv->value.sbh, CosNaming_NamingContext__MSType, _status);
    } else {
      debugprint("ILUCosNaming:  resolve:  For \"%s\", found remote object \"%s\"\n",
	      nv->key.id, nv->value.sbh);
      return ILU_C_SBHToObject(nv->value.sbh, ilu_CORBA_Object__MSType, _status);
    }
  }
}

void
server_CosNaming_NamingContext_unbind(CosNaming_NamingContext _handle,
				      CosNaming_Name* n,
				      ILU_C_ENVIRONMENT *_status)
{
  ilu_HashTable ht = CosNaming_NamingContext__GetUserData(_handle);
  NamedValue *nv;
  ilu_cardinal size;
  CosNaming_Name tempname;

  if (ht == ILU_NIL) {
    debugprint("ILUCosNaming:  unbind:  Context %p has no hashtable!\n", _handle);
    RaiseInvalidName(_status);
    return;
  };
  if ((size = CosNaming_Name_Length(n)) == 0) {
    debugprint("ILUCosNaming:  unbind:  Zero-length name\n");
    RaiseInvalidName(_status);
  } else if ((nv = ilu_hash_FindInTable(ht, n->_buffer[0].id)) == ILU_NIL) {
    debugprint("ILUCosNaming:  unbind:  Id \"%s\" not in hash table\n",
	    n->_buffer[0].id);
    RaiseNotFound(_status, n, 0, CosNaming_missing_node);
    return;
  } else if (size == 1) {
    if (! ilu_hash_RemoveFromTable(ht, nv->key.id)) {
      debugprint("ILUCosNaming:  unbind:  Can't remove id \"%s\" from hash table!\n", nv->key.id);
      ILU_C_RAISE_SYSTEM(_status, INTERNAL, 0, NO);
    } else {
      debugprint("ILUCosNaming:  unbind:  Removed id \"%s\".\n", nv->key.id);
      ilu_free(nv->key.id);
      ilu_free(nv->key.kind);
      if (!nv->local)
	ilu_free(nv->value.sbh);
      ilu_free(nv);
      UpdateBackingStore();
    }
  } else if (nv->kind != CosNaming_ncontext) {
    debugprint("ILUCosNaming:  unbind:  Can't recurse through \"%s\"; not a context.\n",
	    nv->key.id);
    RaiseNotFound(_status, n, 0, CosNaming_not_context);
  } else {
    CosNaming_NamingContext context;
    CORBA_Environment local;
    if (nv->local)
      context = nv->value.obj;
    else {
      context = ILU_C_SBHToObject(nv->value.sbh, CosNaming_NamingContext__MSType, _status);
      if (!ILU_C_SUCCESSFUL(_status)) return;
    }
    tempname._length = size - 1;
    tempname._maximum = size - 1;
    tempname._buffer = &n->_buffer[1];
    debugprint("ILUCosNaming:  unbind:  Recursing through \"%s\"; context is %p (%s)\n",
	    nv->key.id, context, (nv->local) ? "local" : "remote");
    CosNaming_NamingContext_unbind (context, &tempname, _status);
    if (!nv->local) {
      CORBA_Object_release(context, &local);
      CORBA_exception_free(&local);
    }
  }
}

CosNaming_NamingContext
server_CosNaming_NamingContext_new_context(CosNaming_NamingContext _handle,
					   ILU_C_ENVIRONMENT *_status)
{
  return NewNamingContext(ILU_NIL, theServer, 0, _status);
}

void
server_CosNaming_NamingContext_destroy(CosNaming_NamingContext _handle,
				       ILU_C_ENVIRONMENT *_status)
{
  ilu_HashTable ht = CosNaming_NamingContext__GetUserData(_handle);
  if (ilu_hash_PairsInTable(ht) > 0) {
    debugprint("ILUCosNaming:  destroy:  Can't destroy context %p; not empty\n", _handle);
    RaiseNotEmpty(_status);
    return;
  };
  debugprint("ILUCosNaming:  destroy:  Destroying context %p\n", _handle);
  ilu_hash_FreeHashTable(ht, (void (*)(void *)) 0, (void (*)(void *)) 0);
  RemoveObject(_handle);
  CORBA_Object_release(_handle,_status);
  UpdateBackingStore();
  return;
}

CosNaming_NamingContext
server_CosNaming_NamingContext_bind_new_context(CosNaming_NamingContext _handle,
						CosNaming_Name* n,
						ILU_C_ENVIRONMENT *_status)
{
  CosNaming_NamingContext newcontext;
  ILU_C_ENVIRONMENT localstatus;

  newcontext = server_CosNaming_NamingContext_new_context (_handle, _status);
  if (ILU_C_SUCCESSFUL(_status)) {
    server_CosNaming_NamingContext_bind_context (_handle, n, newcontext, _status);
    if (ILU_C_SUCCESSFUL(_status)) {
      UpdateBackingStore();
      return newcontext;
    } else {
      server_CosNaming_NamingContext_destroy (newcontext, &localstatus);
      if (!ILU_C_SUCCESSFUL(&localstatus))
	CORBA_exception_free (&localstatus);
      return ILU_NIL;
    }
  } else {
    return ILU_NIL;
  }
}

void
server_CosNaming_NamingContext_list(CosNaming_NamingContext _handle,
				    CORBA_unsigned_long how_many,
				    CosNaming_BindingList** bl,
				    CosNaming_BindingIterator* bi,
				    ILU_C_ENVIRONMENT *_status)
{
  ilu_HashTable ht = CosNaming_NamingContext__GetUserData(_handle);
  CORBA_unsigned_long count, i, limit;
  CosNaming_BindingList *bindinglist;
  CosNaming_BindingList *iteratorlist;
  CosNaming_BindingIterator iterator;
  ilu_HashEnumerator_s he;
  ilu_string key;
  NamedValue *bv;

  count = ilu_hash_PairsInTable(ht);
  if (count > how_many) {
    limit = how_many;
  } else {
    limit = count;
  }
  bindinglist = CosNaming_BindingList_Create(limit, ILU_NIL);
  ilu_hash_BeginEnumeration (ht, &he);
  for (i = 0;  i < limit;  i++) {
    if (!ilu_hash_Next (&he, (ilu_refany *) &key, (ilu_refany *) &bv)) {
      CosNaming_BindingList__Free(bindinglist);
      ilu_free(bindinglist);
      ILU_C_RAISE_SYSTEM (_status, INTERNAL, 0, NO);
      return;
    };
    bindinglist->_buffer[i].binding_name = NameFromNameComponent (&bv->key, _status);
    if (! ILU_C_SUCCESSFUL(_status))
      return;
    bindinglist->_buffer[i].binding_type = bv->kind;
  }
  bindinglist->_length = limit;
  *bl = bindinglist;
  *bi = ILU_NIL;
  ILU_C_SET_SUCCESSFUL(_status);
  if (limit < count) {
    BIData *bidata = MyMalloc(sizeof(*bidata), _status);
    if (! ILU_C_SUCCESSFUL(_status)) return;
    iteratorlist = CosNaming_BindingList_Create(count - limit, ILU_NIL);
    for (i = limit;  i < count;  i++) {
      if (!ilu_hash_Next (&he, (ilu_refany *) &key, (ilu_refany *) &bv)) {
	CosNaming_BindingList__Free(iteratorlist);
	ilu_free(iteratorlist);
	ILU_C_RAISE_SYSTEM (_status, INTERNAL, 0, NO);
	return;
      };
      iteratorlist->_buffer[i].binding_name = NameFromNameComponent (&bv->key, _status);
      if (! ILU_C_SUCCESSFUL(_status))
	return;
      iteratorlist->_buffer[i].binding_type = bv->kind;
    }
    iteratorlist->_length = count - limit;
    bidata->index = 0;
    bidata->list = iteratorlist;
    if ((iterator = CosNaming_BindingIterator__CreateTrue(ILU_NIL, ILU_NIL, (ilu_refany) bidata)) == ILU_NIL) {
      ILU_C_RAISE_SYSTEM(_status, UNKNOWN, 0, NO);
      return;
    };
    *bi = CORBA_Object_duplicate(iterator, _status);
  }
}

/*************************************************************************/
/**********       Backing store implementation        ********************/
/*************************************************************************/

static char *				LocalServerID = ILU_NIL;

static char *				BackingFilename = ILU_NIL;
FILE *					BackingFP = ILU_NIL;

static CosNaming_NamingContext *	ContextObjects = ILU_NIL;
static ilu_cardinal			ContextObjectsSize = 0;
static ilu_cardinal			ContextObjectsUsed = 0;

static ilu_boolean LocalObject (CosNaming_NamingContext obj)
{
  unsigned int i;
  for (i = 0;  i < ContextObjectsUsed;  i++)
    if (obj == ContextObjects[i])
      return ilu_TRUE;
  return ilu_FALSE;
}

static void
OutputNamingContext (FILE *fp, CosNaming_NamingContext nc, char *local_sid)
{
  ilu_HashTable ht = CosNaming_NamingContext__GetUserData(nc);
  ilu_HashEnumerator_s he;
  char *sid = ILU_NIL;
  char *ih = ILU_NIL;
  char *sbh = ILU_NIL;
  ilu_cardinal sid_len, ih_len, kind_len, id_len, sbh_len;
  ilu_refany key, data;
  NamedValue *entry;
  CORBA_Object obj;

  ILU_C_IDOfObject (nc, &sid, &ih);
  sid_len = strlen(sid);
  ih_len = strlen(ih);
  fprintf (fp, "%ld %ld %*.*s %ld %*.*s\n",
	   ilu_hash_PairsInTable(ht),
	   sid_len, sid_len, sid_len, sid,
	   ih_len, ih_len, ih_len, ih);
  ilu_free(sid);
  ilu_free(ih);

  ilu_hash_BeginEnumeration(ht, &he);
  
  while(ilu_hash_Next(&he, &key, &data))
    {
      entry = (NamedValue *) data;
      id_len = strlen(entry->key.id);
      kind_len = strlen(entry->key.kind);
      if (entry->local) {
	ILU_C_IDOfObject (entry->value.obj, &sid, &ih);
	sid_len = strlen(sid);
	ih_len = strlen(ih);
	ht = (ilu_HashTable) CosNaming_NamingContext__GetUserData(entry->value.obj);
	fprintf (fp, "   %ld %*.*s %ld %*.*s L %ld %*.*s %ld %*.*s %ld\n",
		 id_len, id_len, id_len, entry->key.id,
		 kind_len, kind_len, kind_len, entry->key.kind,
		 sid_len, sid_len, sid_len, sid,
		 ih_len, ih_len, ih_len, ih, ilu_hash_PairsInTable(ht));
      } else {
	sbh = entry->value.sbh;
	sbh_len = strlen(sbh);
	fprintf (fp, "   %ld %*.*s %ld %*.*s %c %ld %*.*s\n",
		 id_len, id_len, id_len, entry->key.id,
		 kind_len, kind_len, kind_len, entry->key.kind,
		 (entry->kind == CosNaming_ncontext) ? 'C' : 'O',
		 sbh_len, sbh_len, sbh_len, sbh);
      }
    }
  fflush(fp);
}

static void UpdateBackingFile (FILE **fp, char *filename, char *sid)
{
  ilu_cardinal i;
 
  fclose(*fp);
  if ((*fp = fopen(filename, "w+")) == ILU_NIL)
    {
      fprintf (stderr, "Can't update backing file %s.\n", filename);
      exit(1);
    }
  fseek(*fp, 0, SEEK_SET);

  for (i = 0;  i < ContextObjectsUsed;  i++) {
    OutputNamingContext(*fp, ContextObjects[i], sid);
  }
}
			       
static void UpdateBackingStore (void)
{
  UpdateBackingFile (&BackingFP, BackingFilename, LocalServerID);
}

static void AddObject (CosNaming_NamingContext obj)
{
  if ((ContextObjectsSize - ContextObjectsUsed) < 1) {
    if (ContextObjectsSize == 0) {
      ContextObjectsSize = 20;
      ContextObjects = ilu_malloc(sizeof(CosNaming_NamingContext) * ContextObjectsSize);
    } else {
      ContextObjectsSize = ContextObjectsSize * 2;
      ContextObjects = ilu_realloc(ContextObjects, sizeof(CosNaming_NamingContext) * ContextObjectsSize);
    }
  }
  ContextObjects[ContextObjectsUsed] = obj;
  ContextObjectsUsed += 1;
}

static void RemoveObject (CosNaming_NamingContext obj)
{
  ilu_cardinal i, j;

  for (i = 0;  i < ContextObjectsUsed;  i++) {
    if (obj == ContextObjects[i]) {
      for (j = i+1;  j < ContextObjectsUsed; j++) {
	ContextObjects[j-1] = ContextObjects[j];
      }
      ContextObjectsUsed -= 1;
      return;
    };
  }
}

static FILE *
InitializeRoot (char *filename, char *root_sid, ILU_C_Server server)
{
  FILE *fp;
  char *sid, *ih, *id, *kind, *sbh, *lp;
  ilu_HashTable ht;
  unsigned long entries, local_entries;
  unsigned long sid_len, ih_len, id_len, kind_len, sbh_len, lineno, i;
  int count;
  int num_dropped = 0;
  char line[10000];
  char kindc;
  CosNaming_NamingContext newobj;
  ILU_C_Object *obj;
  ILU_C_ENVIRONMENT status;
  NamedValue *nv;

  if ((fp = fopen(filename, "a+")) == ILU_NIL) {
    fprintf (stderr, "Can't open backing file %s.\n", filename);
    return ILU_NIL;
  }
  fseek(fp, 0L, SEEK_END);
  if (ftell(fp) > 0) {
    /* already exists; read contents */
    fseek(fp, 0, SEEK_SET);
    lineno = 0;
    while (fgets(line, sizeof(line)-1, fp) != ILU_NIL) {
      lineno += 1;
      line[strlen(line)-1] = 0;
      lp = line;
      if (sscanf(lp, "%lu %lu %n", &entries, &sid_len, &count) != 2) goto badline;
      sid = ilu_malloc(sid_len + 1);
      lp += count;
      memcpy (sid, lp, sid_len);
      sid[sid_len] = 0;
      lp += sid_len;
      if (sscanf(lp, "%lu %n", &ih_len, &count) != 1) goto badline;
      ih = ilu_malloc(ih_len + 1);
      lp += count;
      memcpy (ih, lp, ih_len);
      ih[ih_len] = 0;
      lp += ih_len;
      if (*lp != 0) goto badline;
      if (strcmp(root_sid, sid) != 0) {
	fprintf (stderr, "%s, %d:  Bad SID in backing file:  \"%s\"  (expected \"%s\").\n",
		 filename, lineno, sid, root_sid);
	return ILU_NIL;
      };
      ilu_free(sid);
      ILU_C_SET_SUCCESSFUL(&status);
      newobj = FindOrCreateNamingContext (ih, server, entries, &status);
      if (!ILU_C_SUCCESSFUL(&status)) {
	fprintf (stderr, "Can't create naming context %s/%s with %d entries.\n",
		 root_sid, ih, entries);
	return ILU_NIL;
      }      
      ht = (ilu_HashTable) CosNaming_NamingContext__GetUserData(newobj);
      for (i = 0;  i < entries;  i++) {
	if (fgets(line, sizeof(line)-1, fp) == ILU_NIL) {
	  fprintf (stderr, "%s, %d:  Not enough entries in backing file.\n",
		   filename, lineno);
	  return ILU_NIL;
	};
	lineno += 1;
	lp = line;
	line[strlen(line)-1] = 0;
	if (sscanf(lp, "   %lu %n", &id_len, &count) != 1) goto badline;
	id = ilu_malloc(id_len + 1);
	lp += count;
	memcpy (id, lp, id_len);
	id[id_len] = 0;
	lp += id_len;
	if (sscanf(lp, " %lu %n", &kind_len, &count) != 1) goto badline;
	kind = ilu_malloc(kind_len + 1);
	lp += count;
	memcpy (kind, lp, kind_len);
	kind[kind_len] = 0;
	lp += kind_len;
	if ((sscanf(lp, " %c %lu %n", &kindc, &sbh_len, &count) != 2) ||
	    ((kindc != 'L') && (kindc != 'C') && (kindc != 'O'))) goto badline;
	if (kindc == 'L') {
	  sid_len = sbh_len;
	  sid = ilu_malloc(sid_len + 1);
	  lp += count;
	  memcpy (sid, lp, sid_len);
	  sid[sid_len] = 0;
	  lp += sid_len;
	  if (strcmp(sid, root_sid) != 0) {
	    fprintf (stderr, "%s, %d:  bad SID\n", filename, lineno);
	    return ILU_NIL;
	  };
	  if (sscanf (lp, " %lu %n", &ih_len, &count) != 1) goto badline;
	  lp += count;
	  ih = ilu_malloc(ih_len + 1);
	  memcpy (ih, lp, ih_len);
	  ih[ih_len] = 0;
	  lp += ih_len;
	  if ((sscanf (lp, " %lu%n", &local_entries, &count) != 1) ||
	      (lp[count] != 0)) goto badline;
	  ILU_C_SET_SUCCESSFUL(&status);
	  obj = FindOrCreateNamingContext (ih, server, local_entries, &status);
	  if (!ILU_C_SUCCESSFUL(&status)) {
	    fprintf (stderr,
		     "%s, %d:  Can't incarnate local context with ih=\"%s\"\n",
		     filename, lineno, ih);
	    return ILU_NIL;
	  }
	  ilu_free(ih);
	  ilu_free(sid);
	} else {
	  sbh = ilu_malloc(sbh_len + 1);
	  lp += count;
	  memcpy (sbh, lp, sbh_len);
	  sbh[sbh_len] = 0;
	  lp += sbh_len;
	  if (*lp != 0) goto badline;
	  ILU_C_SET_SUCCESSFUL(&status);
	}
	nv = (NamedValue *) ilu_malloc(sizeof(*nv));
	nv->key.id = id;
	nv->key.kind = kind;
	nv->kind = (kindc == 'O') ? CosNaming_nobject : CosNaming_ncontext;
	if (kindc == 'L') {
	  nv->local = ilu_TRUE;
	  nv->value.obj = obj;
	} else {
	  nv->local = ilu_FALSE;
	  nv->value.sbh = sbh;
	}
	if (!ilu_hash_AddToTable(ht, nv->key.id, nv)) {
	  fprintf (stderr, "%s, %d:  Can't add object <%s> to hash table!\n",
		   filename, lineno, (nv->local) ? ILU_C_SBHOfObject(obj) : sbh);
	  if (!nv->local) ilu_free(sbh);
	  ilu_free(nv);
	  return ILU_NIL;
	};
      }
    }
  }
  if (num_dropped) {
     fprintf (stderr, "Deleted %d binding%s.\n",
	      num_dropped, (num_dropped > 1 ? "s" : ""));
     UpdateBackingFile (&fp, BackingFilename, LocalServerID);
  }
  return (fp);

 badline:
  fprintf (stderr, "%s, %d:  Badly formatted line\n", filename, lineno);
  return ILU_NIL;
}

/*************************************************************************/
/********           main() implementation             ********************/
/*************************************************************************/
			       
static void usage(char *pname)
{
  fprintf (stderr, "Usage:  %s [-f BACKING-FILE] [-p PORT] [-ior] [-ior_to_file FILENAME] [-object_key OBJECT-KEY] [-sid SERVER-ID] [-ih ROOT-INST-HANDLE] [-h HOSTNAME-OR-IP-ADDR] [-sid_and_ih_from_IOR STRINGIFED-IOR] [-publish] [-verbose]\n", pname);
  exit(1);
}

static char *safe_strcpy(char *into, ilu_cardinal into_size, char *msg, char *from)
{
  ilu_cardinal len = strlen(from);
  if (len+1 > into_size) {
    fprintf (stderr, "Parameter %s is limited to %lu characters.\n", msg, into_size - 1);
    exit(1);
  };
  return strcpy (into, from);
}

int main (int ac, char **av)
{
  CosNaming_NamingContext root;
  ILU_C_ENVIRONMENT status;
  int stop;
  char *pinfofmt = "iiop_1_0_1_%s";
  char pinfo[1024];
  char *tinfo[3] = { "tcp_0_0", ILU_NIL, ILU_NIL };
  char hostname[1024];
  char tcpinfo[1100];
  char sid[1100];
  char ih[1100];
  unsigned long rawport;
  ilu_shortcardinal port = 9999;
  ilu_boolean ior = ilu_FALSE;
  ilu_boolean publish = ilu_FALSE;
  int i = 1;
  ilu_boolean sid_set = ilu_FALSE;
  ilu_boolean ih_set = ilu_FALSE;
  char *object_key = "NameService";
  char *IORFilename = ILU_NIL;

  gethostname(hostname, sizeof(hostname));

  while (i < ac) {
    if (strcmp(av[i], "-p") == 0) {
      if (i++ < ac) {
	(rawport = strtoul(av[i], ILU_NIL, 0)), i++;
	if (rawport >= 0 && rawport <= 0xFFFF)
	  port = (ilu_shortcardinal)rawport;
	else
	  usage(av[0]);
      }
      else
	usage(av[0]);
    } else if (strcmp(av[i], "-h") == 0) {
      if (i++ < ac)
	safe_strcpy (hostname, sizeof(hostname), "HOSTNAME", av[i]), i++;
      else
	usage(av[0]);
    } else if (strcmp(av[i], "-sid") == 0) {
      if (i++ < ac)
	{
	  safe_strcpy (sid, sizeof(sid), "SERVER-ID", av[i]), i++;
	  sid_set = ilu_TRUE;
	}
      else
	usage(av[0]);
    } else if (strcmp(av[i], "-sid_and_ih_from_IOR") == 0) {
      if (i++ < ac)
	{
	  ilu_string new_sid = ILU_NIL;
	  ilu_string new_ih = ILU_NIL;
	  ilu_Error lerr;
	  ilu_ParseSBH (av[i], &new_ih, &new_sid, ILU_NIL, ILU_NIL, ILU_NIL, ILU_NIL, &lerr);
	  i++;
	  if (ILU_ERRNOK(lerr)) {
	    ILU_HANDLED(lerr);
	    fprintf (stderr, "Bad -sid_and_ih_from_IOR parameter:  %s\n",
		     ILU_ERR_NAME(lerr));
	    usage(av[0]);
	  } else {
	    fprintf (stderr, "using SID=<%s>, IH=<%s>\n", new_sid, new_ih);
	    safe_strcpy (sid, sizeof(sid), "SERVER-ID", new_sid);
	    sid_set = ilu_TRUE;
	    safe_strcpy (ih, sizeof(ih), "ROOT-INSTANCE-HANDLE", new_ih);
	    ih_set = ilu_TRUE;
	  }
	}
      else
	usage(av[0]);
    } else if (strcmp(av[i], "-ih") == 0) {
      if (i++ < ac) {
	safe_strcpy (ih, sizeof(ih), "ROOT-INSTANCE-HANDLE", av[i]), i++;
	ih_set = ilu_TRUE;
      } else
	usage(av[0]);
    } else if (strcmp(av[i], "-f") == 0) {
      if (i++ < ac)
	BackingFilename = av[i++];
      else
	usage(av[0]);
    } else if (strcmp(av[i], "-ior") == 0) {
      ior = ilu_TRUE;
      i++;
    } else if (strcmp(av[i], "-ior_to_file") == 0) {
      if (i++ < ac)
	IORFilename = av[i++];
      else
	usage(av[0]);
    } else if (strcmp(av[i], "-publish") == 0) {
      publish = ilu_TRUE;
      i++;
    } else if (strcmp(av[i], "-object_key") == 0) {
      if (i++ < ac)
	object_key = av[i++];
      else
	usage(av[0]);
    } else if (strcmp(av[i], "-verbose") == 0) {
      Verbose = ilu_TRUE;
      i++;
    } else usage(av[0]);
  }    

  /* initialize simpbind interface */
  CosNaming__InitializeServer();

  sprintf (tcpinfo, "tcp_%s_%d", hostname, port);
  tinfo[0] = tcpinfo;
  sprintf (pinfo, pinfofmt, object_key);

  if ((!sid_set) && (!ih_set)) {
    /* Initialize to values compatible with INS submission */
    ilu_Error err;
    ilu_string s = ilu_IIOP_ServerIDFromObjectKey((ilu_bytes) object_key, strlen(object_key),
						  hostname, port, 1, 0, &err);
    if (ILU_ERRNOK(err)) {
      fprintf(stderr, "Can't create server ID from object key "
	      "\"%s\".  Error <%s>.\n", object_key, ILU_ERR_NAME(err));
      ILU_HANDLED(err);
      exit(1);
    }
    strcpy (sid, s);
    ilu_free(s);
    s = ilu_IIOP_IHFromObjectKey ((ilu_bytes) object_key,
				  strlen(object_key), &err);
    if (ILU_ERRNOK(err)) {
      fprintf(stderr, "Can't create instance handle for object key "
	      "\"%s\".  Error <%s>.\n", object_key, ILU_ERR_NAME(err));
      ILU_HANDLED(err);
      exit(1);
    }
    strcpy (ih, s);
    ilu_free(s);
  }
  else {
    if (!sid_set)
      sprintf (sid, "CosNaming_%s", ilu_InventID());
    if (!ih_set)
      strcpy (ih, "root");
  }
  LocalServerID = sid;

  /* make kernel server */
  theServer = ILU_C_InitializeServer(sid,
				     NULL, /* use std object table */
				     pinfo,
				     tinfo,
				     ILU_NIL, /* no identity at present */
				     ilu_TRUE /* create port anyway */
				     );
  
  if (BackingFilename == ILU_NIL)
    {
      sprintf (hostname, "/tmp/ILUCosNaming.%s", sid);
      BackingFilename = hostname;
    }

  if ((BackingFP = InitializeRoot (BackingFilename, sid, theServer)) == ILU_NIL) {
    fprintf (stderr, "Error initializing server object.\n");
    return 1;
  } else if ((root = ILU_C_FindObject (sid, ih)) == ILU_NIL) {
    ILU_C_SET_SUCCESSFUL(&status);
    root = NewNamingContext (ih, theServer, 0, &status);
    if (!ILU_C_SUCCESSFUL(&status)) {
      fprintf (stderr, "Error creating server object.\n");
      return 1;
    }
  };
  if (ior) {
    fprintf (stdout, "%s\n", ILU_C_IOROfObject(root));
    fflush (stdout);
  };
  if (IORFilename) {
    FILE *fp = fopen(IORFilename, "w");
    char *ior = ILU_C_IOROfObject(root);
    if (fp == ILU_NIL) {
      fprintf(stderr, "Can't open specified file \"%s\" to write IOR into it:  %s\n",
	      IORFilename, strerror(errno));
      return 1;
    }
    fwrite(ior, strlen(ior), 1, fp);
    fflush(fp);
    fclose(fp);
  };    
  if (publish) {
    (void) ILU_C_PublishObject(root);
  };
  ilu_RunMainLoop (&stop);
  return 0;
}
