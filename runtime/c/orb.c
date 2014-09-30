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
/* $Id: orb.c,v 1.55 1999/08/09 23:57:33 janssen Exp $ */
/* Last edited by Mike Spreitzer December 2, 1997 12:22 pm PST */

#include <stdio.h>
#include <stdlib.h>	/* for getenv */
#include <string.h>	/* for memset */

#include "ilucpvt.h"

static ILU_C_COMPLETIONSTATUS MapCompletionStatus (ilu_Completion c)
{
  switch (c) {
  case ILU_COMPLETED_NO:
    return CORBA_COMPLETED_NO;
  case ILU_COMPLETED_YES:
    return CORBA_COMPLETED_YES;
  default:
    return CORBA_COMPLETED_MAYBE;
  }
}

void _ILU_C_SetProtocolError (CORBA_Environment *status, ilu_ProtocolException perror)
{
  ilu_Error lerr;
  ilu_Completion completed;

  ilu_MapProtocolExceptionToError (perror, &lerr, &completed);
  _ILU_C_ConvertError(status, &lerr, MapCompletionStatus(completed));
}

ILU_C_ExceptionCode	ex_CORBA_UNKNOWN = "CORBA: an unknown exception was encountered";
ILU_C_ExceptionCode	ex_CORBA_BAD_PARAM = "CORBA: a bad parameter was passed";
ILU_C_ExceptionCode	ex_CORBA_NO_MEMORY = "CORBA: dynamic memory allocation failure";
ILU_C_ExceptionCode	ex_CORBA_IMP_LIMIT = "CORBA: some implementation limit exceeded";
ILU_C_ExceptionCode	ex_CORBA_COMM_FAILURE = "CORBA: communication failure";
ILU_C_ExceptionCode	ex_CORBA_INV_OBJREF = "CORBA: invalid object reference";
ILU_C_ExceptionCode	ex_CORBA_NO_PERMISSION = "CORBA: no permission for attempted operation";
ILU_C_ExceptionCode	ex_CORBA_INTERNAL = "CORBA: ORB internal error";
ILU_C_ExceptionCode	ex_CORBA_MARSHAL = "CORBA: error marshalling parameter or result";
ILU_C_ExceptionCode	ex_CORBA_INITIALIZE = "CORBA: ORB initialization failure";
ILU_C_ExceptionCode	ex_CORBA_NO_IMPLEMENT = "CORBA: operation implementation unavailable";
ILU_C_ExceptionCode	ex_CORBA_BAD_TYPECODE = "CORBA: bad typecode";
ILU_C_ExceptionCode	ex_CORBA_BAD_OPERATION = "CORBA: invalid operation";
ILU_C_ExceptionCode	ex_CORBA_NO_RESOURCES = "CORBA: insufficient resources for operation";
ILU_C_ExceptionCode	ex_CORBA_NO_RESPONSE = "CORBA: response to request not yet available";
ILU_C_ExceptionCode	ex_CORBA_PERSIST_STORE = "CORBA: persistent storage failure";
ILU_C_ExceptionCode	ex_CORBA_BAD_INV_ORDER = "CORBA: routine invocations out of order";
ILU_C_ExceptionCode	ex_CORBA_TRANSIENT = "CORBA: transient failure -- reissue request";
ILU_C_ExceptionCode	ex_CORBA_FREE_MEM = "CORBA: cannot free memory";
ILU_C_ExceptionCode	ex_CORBA_INV_IDENT = "CORBA: invalid identifier syntax";
ILU_C_ExceptionCode	ex_CORBA_INV_FLAG = "CORBA: invalid modifier or option was specified";
ILU_C_ExceptionCode	ex_CORBA_INTF_REPOS = "CORBA: error accessing interface repository";
ILU_C_ExceptionCode	ex_CORBA_BAD_CONTEXT = "CORBA: error processing context object";
ILU_C_ExceptionCode	ex_CORBA_OBJ_ADAPTER = "CORBA: error detected by object adapter";
ILU_C_ExceptionCode	ex_CORBA_DATA_CONVERSION = "CORBA: data conversion error";
ILU_C_ExceptionCode	ex_CORBA_CODESET_INCOMPATIBLE = "CORBA: codeset incompatible error";

ILU_C_ExceptionCode	ex_ILU_BARRIER = "ILU: call is barrier call";
ILU_C_ExceptionCode	ex_ILU_BAD_LOCKS = "ILU: caller held wrong mutexes";
ILU_C_ExceptionCode	ex_ILU_BROKEN_LOCKS = "ILU: mutex implementation broken";
ILU_C_ExceptionCode	ex_ILU_INTERRUPTED = "ILU: application requested interruption of call";
ILU_C_ExceptionCode	ex_ILU_GSS_SECURITY = "ILU: GSS exception";

#define SYS_EX(type,completion)	{s->_major=CORBA_SYSTEM_EXCEPTION;s->returnCode=(type);s->ptr=(void *)ilu_must_malloc(sizeof(CORBA_ex_body));s->freeRoutine=((void (*)(void *)) 0);((CORBA_ex_body *)(s->ptr))->minor=0;((CORBA_ex_body *)(s->ptr))->completed=(completion);}

ILU_C_ExceptionCode	ex_CORBA_InvalidName = "CORBA: invalid name passed to resolve_initial_references";

/*
** corba defined exception handling functions
*/

char *CORBA_exception_id (CORBA_Environment *ev)
{
  return(( char * ) ev->returnCode );
}

void *CORBA_exception_value (CORBA_Environment *ev)
{
  return(( void * ) ev->ptr );
}

void CORBA_exception_free (CORBA_Environment *ev)
{
  if (ILU_C_SUCCESSFUL(ev))
    return;
  if (ev->freeRoutine != ((void (*)(void *)) 0))
    (*ev->freeRoutine)(ev->ptr);
  ilu_free(ev->ptr);
  ev->ptr = ILU_NIL;
  ev->freeRoutine = ((void (*)(void *)) 0);
  return;
}

char *CORBA_string_alloc(CORBA_unsigned_long len)
{
  return ilu_malloc(len + 1);
}

void CORBA_free (void *ptr)
{
  if ( ptr )
    ilu_free( ptr );
  return;
}

CORBA_ORBStatus CORBA_ORB_get_default_context (CORBA_Object h, CORBA_Context *ctx, CORBA_Environment *s)
{
  if (h != ILU_C_ORB)
    SYS_EX(ex_CORBA_INV_OBJREF,CORBA_COMPLETED_NO)
  else
    SYS_EX(ex_CORBA_NO_IMPLEMENT,CORBA_COMPLETED_NO)
}
    
char *CORBA_ORB_object_to_string (CORBA_Object h, CORBA_Object o, CORBA_Environment *s)
{
  char *sbh;

#ifdef IIOP_PROTOCOL
  if (h != ILU_C_ORB || ((sbh = ILU_C_IOROfObject(o)) == ILU_NIL))
#else
  if (h != ILU_C_ORB || ((sbh = ILU_C_SBHOfObject(o)) == ILU_NIL))
#endif
    {
      SYS_EX(ex_CORBA_INV_OBJREF,CORBA_COMPLETED_NO)
      return (ILU_NIL);
    }
  else
    {
      s->_major = CORBA_NO_EXCEPTION;
      return (ILU_C_Strdup(sbh));
    }
}

static void
  RaiseCORBAInvalidName (CORBA_Environment *s, ilu_string name)
{
  ilu_Error err;

  s->_major = CORBA_USER_EXCEPTION;
  s->returnCode = ex_CORBA_InvalidName;
  s->ptr = (void *) ilu_MallocE(sizeof(ilu_CString), &err);
  if (ILU_ERRNOK(err)) {
    s->ptr = ILU_NIL;
    s->freeRoutine = (ILU_C_FreeFn) 0;
    ILU_HANDLED(err);
  } else {
    *((ilu_CString *)(s->ptr)) = ilu_StrdupE(name, &err);
    if (ILU_ERRNOK(err)) {
      ilu_free(s->ptr);
      s->ptr = ILU_NIL;
      s->freeRoutine = (ILU_C_FreeFn) 0;
      ILU_HANDLED(err);
    } else {
      s->freeRoutine = (ILU_C_FreeFn) ilu_CString__Free;
    }
  }
}

CORBA_Object CORBA_ORB_string_to_object (CORBA_Object o, char *sbh, CORBA_Environment *s)
{
  CORBA_Object h;

  if (o != ILU_C_ORB || (h = ILU_C_SBHToObject (sbh, ilu_rootClass, s)) == ILU_NIL)
    {
      SYS_EX(ex_CORBA_INV_OBJREF,CORBA_COMPLETED_NO)
      return (ILU_NIL);
    }
  else
    {
      s->_major = CORBA_NO_EXCEPTION;
      return (h);
    }
}

CORBA_boolean CORBA_Object_is_nil (CORBA_Object h, CORBA_Environment *s)
{
  s->_major = CORBA_NO_EXCEPTION;
  return (h == ILU_NIL);
}

CORBA_boolean CORBA_Object_is_a (CORBA_Object h, CORBA_string repository_id, CORBA_Environment *s)
{
  ilu_Class repclass = ilu_FindClassFromID(repository_id);
  if (repclass == ILU_NIL) {
    RaiseCORBAInvalidName(s, repository_id);
    return ilu_FALSE;
  } else {
    ILU_C_SET_SUCCESSFUL(s);
    return (ilu_IsSubObjectType(ILU_C_ClassRecordOfInstance(h), repclass));
  }
}

CORBA_boolean CORBA_Object_non_existent (CORBA_Object h, CORBA_Environment *s)
{
  ILU_C_PingObject(h, s);
  return (!ILU_C_SUCCESSFUL(s));
}

CORBA_boolean CORBA_Object_is_equivalent (CORBA_Object h1, CORBA_Object h2, CORBA_Environment *s)
{
  ilu_boolean result = ilu_FALSE;
  CORBA_Environment env;
  ILU_C_Server s1, s2;

  s1 = ILU_C_ServerOfObject(h1, s);
  if (!ILU_C_SUCCESSFUL(s))
    return ilu_FALSE;
  s2 = ILU_C_ServerOfObject(h2, s);
  if (!ILU_C_SUCCESSFUL(s))
    goto out1;
  if (s1 != s2)
    goto out2;
  result = (h1->iluco_ko == h2->iluco_ko);
 out2:
  ILU_C_Server_release (s2, &env);
  if (!ILU_C_SUCCESSFUL(&env))
    CORBA_exception_free(&env);
 out1:
  ILU_C_Server_release (s1, &env);
  if (!ILU_C_SUCCESSFUL(&env))
    CORBA_exception_free(&env);
  return result;
}

CORBA_unsigned_long CORBA_Object_hash (CORBA_Object h, CORBA_unsigned_long max_value, CORBA_Environment *s)
{
  ILU_C_SET_SUCCESSFUL(s);
  return (((CORBA_unsigned_long)h) % max_value);
}

static ilu_HashTable InitialReferences = ILU_NIL;
/* hash of NAME -> URL for initial services specified by either environment variables
   or via -ORBInitRef arg to ORB_init().
*/

static char *DefaultInitRef = ILU_NIL;
/* holds partial URL used to resolve_initial_references by appending the name
   passed in to the partial URL, then calling string_to_object.  Initialized by
   -ORBDefaultInitRef arg to ORB_init().
*/

CORBA_Object CORBA_ORB_resolve_initial_references (CORBA_Object h, CORBA_string name, CORBA_Environment *s)
{
  ilu_string url;
  ilu_CORBA_Object obj;

  if (h != ILU_C_ORB)
    {
      ILU_C_RAISE_SYSTEM(s,INV_OBJREF,0,NO);
      return (ILU_NIL);
    }
  if ((InitialReferences != ILU_NIL) &&
      ((url = ilu_hash_FindInTable(InitialReferences, name)) != ILU_NIL)) {
    if (((obj = ilu_CORBA_Object__CreateFromSBH(url, s)) == ILU_NIL) ||
	!ILU_C_SUCCESSFUL(s)) {
      ilu_DebugPrintf("ILU/C: Can't create supposed %s service at <%s>.\n", name, url);
      if (!ILU_C_SUCCESSFUL(s)) {
	ilu_DebugPrintf("       Exception is <%s>.\n", (char *) s->returnCode);
	CORBA_exception_free(s);
      };
      RaiseCORBAInvalidName(s, name);
      return (ILU_NIL);
    } else {
      return obj;
    }
  } else if (DefaultInitRef != ILU_NIL) {
    char urlbuf[3000];
    sprintf(urlbuf, "%s/%s", DefaultInitRef, name);
    if (((obj = ilu_CORBA_Object__CreateFromSBH(urlbuf, s)) == ILU_NIL) ||
	!ILU_C_SUCCESSFUL(s)) {
      ilu_DebugPrintf("ILU/C: Can't create supposed %s service at <%s>.\n", name, urlbuf);
      if (!ILU_C_SUCCESSFUL(s)) {
	ilu_DebugPrintf("       Exception is <%s>.\n", (char *) s->returnCode);
	CORBA_exception_free(s);
      };
      RaiseCORBAInvalidName(s, name);
      return (ILU_NIL);
    } else {
      return obj;
    }
  } else {
    RaiseCORBAInvalidName(s, name);
    return (ILU_NIL);
  }
}

CORBA_Object CORBA_ORB_init (int *argc, char **argv, CORBA_ORBid orb_id, CORBA_Environment *s)
{
  ILU_C_SET_SUCCESSFUL(s);
  _ILU_C_InitializeCRuntime();
#if (defined(IIOP_PROTOCOL) && (defined(ADD_IDL_SUPPORT) || defined(ADD_DIRECT_OMG_IDL_SUPPORT)))
  if (strcmp(orb_id, "ilu") != 0) {
    ilu_DebugPrintf ("ILU/C: Don't know how to initialize ORB with ID \"%s\".\n", orb_id);
    ILU_C_RAISE_SYSTEM(s,BAD_PARAM,0,NO);
  };
  if ((InitialReferences == ILU_NIL) &&
      ((InitialReferences = ilu_hash_MakeNewTable(3,
						  ilu_hash_HashString,
						  ilu_hash_StringCompare)) == ILU_NIL)) {
    ilu_DebugPrintf("ILU/C: Can't create InitialReferences hash table!\n");
    ILU_C_RAISE_SYSTEM(s,NO_MEMORY,0,NO);
    return ILU_NIL;
  };
  {
    int i, j;
    ilu_string nameservice = getenv("ILU_COS_NAMING_IOR");
    if (nameservice != ILU_NIL) {
      ilu_hash_AddToTable(InitialReferences, "NameService", nameservice);
    }
    for (i = 0;  i < *argc;  i++) {
      if (strcmp(argv[i], "-ORBInitRef") == 0) {
	i++;
	if (i < *argc) {
	  char name[1000], url[2000];
	  char *name2, *url2;
	  if (sscanf (argv[i], "%999[^=]=%1999s", name, url) != 2) {
	    ilu_DebugPrintf ("ILU/C: Bad -ORBInitRef parameter <%s>.\n", argv[i]);
	    ILU_C_RAISE_SYSTEM(s,BAD_PARAM,0,NO);
	    return ILU_NIL;
	  } else {
	    name2 = ILU_C_Strdup(name);
	    url2 = ILU_C_Strdup(url);
	    ilu_hash_AddToTable (InitialReferences, name2, url2);
	    for (j = i+1;  j <= *argc;  j++) {
	      argv[j-2] = argv[j];
	    }
	    *argc -= 2;
	  }
	} else {
	  ilu_DebugPrintf ("ILU/C: Missing -ORBInitRef parameter!\n");
	  ILU_C_RAISE_SYSTEM(s,BAD_PARAM,0,NO);
	  return ILU_NIL;
	}
      } else if (strcmp(argv[i], "-ORBDefaultInitRef") == 0) {
	i++;
	if (i < *argc) {
	  DefaultInitRef = argv[i];
	  for (j = i+1;  j <= *argc;  j++) {
	    argv[j-2] = argv[j];
	  }
	  *argc -= 2;
	} else {
	  ilu_DebugPrintf ("ILU/C: Missing -ORBDefaultInitRef parameter!\n");
	  ILU_C_RAISE_SYSTEM(s,BAD_PARAM,0,NO);
	  return ILU_NIL;
	}
      }
    }
  }
#endif
  return ILU_C_ORB;
}

CORBA_ORB_ObjectIdList
  CORBA_ORB_list_initial_services (CORBA_Object ORB,
				   CORBA_Environment *env)
{
  CORBA_ORB_ObjectIdList list;

  CORBA_ORB_ObjectIdList_Init(&list, 0, ILU_NIL);
  ILU_C_SET_SUCCESSFUL(env);

  if (InitialReferences != ILU_NIL) {
    ilu_HashEnumerator_s hash_enumerator;
    char *key;
    CORBA_ORB_ObjectId entry;
    void *data;
    int count = ilu_hash_PairsInTable(InitialReferences);
    CORBA_ORB_ObjectIdList_Init (&list, count, ILU_NIL);
    ilu_hash_BeginEnumeration (InitialReferences, &hash_enumerator);
    while (ilu_hash_Next(&hash_enumerator, (void **) &key, (void **) &data)) {
      entry = ILU_C_Strdup(key);
      CORBA_ORB_ObjectIdList_Append(&list, entry);
    }
  }			 
  return list;
}

#define NSysExns 31
static ILU_C_ExceptionCode SysExnsByIndex[NSysExns] = {0,};

#define ILU_SET_ELT(x,y) SysExnsByIndex[ILU_ERRTYP(x)-1] = ILU_C_STDEX(y)
#define ILU_SET_ALT(x,y) SysExnsByIndex[ILU_ERRTYP(x)-1] = ILU_C_ILUEX(y)

/*L1, L2 unconstrained*/
static void InitSysExns(void)
{
  if (SysExnsByIndex[NSysExns-1] == 0) {
    ILU_SET_ELT(unknown, UNKNOWN);
    ILU_SET_ELT(bad_param, BAD_PARAM);
    ILU_SET_ELT(no_memory, NO_MEMORY);
    ILU_SET_ELT(imp_limit, IMP_LIMIT);
    ILU_SET_ELT(comm_failure, COMM_FAILURE);
    ILU_SET_ELT(inv_objref, INV_OBJREF);
    ILU_SET_ELT(no_permission, NO_PERMISSION);
    ILU_SET_ELT(internal, INTERNAL);
    ILU_SET_ELT(marshal, MARSHAL);
    ILU_SET_ELT(initialize, INITIALIZE);
    ILU_SET_ELT(no_implement, NO_IMPLEMENT);
    ILU_SET_ELT(bad_typecode, BAD_TYPECODE);
    ILU_SET_ELT(bad_operation, BAD_OPERATION);
    ILU_SET_ELT(no_resources, NO_RESOURCES);
    ILU_SET_ELT(no_response, NO_RESPONSE);
    ILU_SET_ELT(persist_store, PERSIST_STORE);
    ILU_SET_ELT(bad_inv_order, BAD_INV_ORDER);
    ILU_SET_ELT(transient, TRANSIENT);
    ILU_SET_ELT(free_mem, FREE_MEM);
    ILU_SET_ELT(inv_ident, INV_IDENT);
    ILU_SET_ELT(inv_flag, INV_FLAG);
    ILU_SET_ELT(intf_repos, INTF_REPOS);
    ILU_SET_ELT(bad_context, BAD_CONTEXT);
    ILU_SET_ELT(obj_adapter, OBJ_ADAPTER);
    ILU_SET_ELT(data_conversion, DATA_CONVERSION);
    ILU_SET_ELT(codeset_incompatible, CODESET_INCOMPATIBLE);
    ILU_SET_ALT(barrier, BARRIER);
    ILU_SET_ALT(bad_locks, BAD_LOCKS);
    ILU_SET_ALT(broken_locks, BROKEN_LOCKS);
    ILU_SET_ALT(interrupted, INTERRUPTED);
    ILU_SET_ALT(gss_security, GSS_SECURITY);
  }
}

/*L1, L2 unconstrained*/
void
_ILU_C_ConvertError(ILU_C_ENVIRONMENT * env,
		    ilu_Error * err,
		    ILU_C_COMPLETIONSTATUS cstat)
{
  unsigned long   minor;
  ilu_integer     major;

  if (ILU_ERROK(*err)) {
    /* handle success case */
    env->_major = ILU_C_NO_EXCEPTION;
    env->ptr = ILU_NIL;
    env->freeRoutine = (void(*)(void *)) 0;
    env->returnCode = (ilu_Exception) 0;
    return;
  };
  InitSysExns();
  minor = ilu_CORBAizeSystemErr(err, &major);
  if (0 <= major && major < NSysExns)
    ILU_C_RaiseSysExn(env, SysExnsByIndex[major], minor, cstat,
		      err->ilu_file, err->ilu_line);
  else
    ILU_C_RAISE_SYS_EXPR(env, ILU_C_STDEX(INTERNAL),
			 ilu_im_badKernelErr, cstat);
  return;
}

/*L1, L2 unconstrained*/
static int LookupSysExn(ILU_C_ExceptionCode ec)
{
  int i;
  InitSysExns();
  for (i=0; i<NSysExns; i++)
    if (SysExnsByIndex[i] == ec)
      return i;
  return -1;
}

const char *ILU_C_SysExnMinorDescr(ILU_C_ENVIRONMENT *env)
{
  const char *ans = ILU_NIL;
  if (env && env->_major == CORBA_SYSTEM_EXCEPTION) {
    int major = LookupSysExn(env->returnCode);
    if (major >= 0 && env->ptr) {
      CORBA_ex_body *sb = (CORBA_ex_body*) env->ptr;
      ans = ilu_DescribeCorbaMinor(major, sb->minor);
    }
  }
  return ans ? ans : "no minor code/decoding available";
}

typedef struct {
  CORBA_ex_body	ilucseb_base;
  const char	*ilucseb_filename;
  int		ilucseb_linenum;
} ILU_C_sysextended_body;

ILU_C_ENVIRONMENT *ILU_C_RaiseSysExn(ILU_C_ENVIRONMENT *p,
				     ILU_C_ExceptionCode exn,
				     ilu_cardinal minor,
				     CORBA_completion_status cstat,
				     const char *src_filename,
				     int src_linenum)
{
  ILU_C_SYSEXN_BODY *sb;
  p->_major = ILU_C_SYSTEM_EXCEPTION;
  p->returnCode = exn;
  if (src_linenum && src_filename && src_filename[0]) {
    ILU_C_sysextended_body *seb = ilu_malloc(sizeof(*seb));
    seb->ilucseb_filename = src_filename;
    seb->ilucseb_linenum = src_linenum;
    sb = &seb->ilucseb_base;
    minor = minor | ILU_VMCID_EXTRA;
  } else {
    sb = ilu_malloc(sizeof(*sb));
    minor = minor & ~ILU_VMCID_EXTRA;
  }
  p->ptr = sb;
  sb->minor = minor;
  sb->completed = cstat;
  p->freeRoutine = (void (*)(void *)) 0;
  return p;
}

CORBA_boolean ILU_C_exception_source( CORBA_Environment *env,
				      const char **exnsrc_filename,
				      int *exnsrc_linenum )
{
  if (env->_major == CORBA_SYSTEM_EXCEPTION && env->ptr ) {
    ILU_C_sysextended_body *seb = (ILU_C_sysextended_body*) env->ptr;
    if (seb->ilucseb_base.minor & ILU_VMCID_EXTRA) {
      if (exnsrc_filename) *exnsrc_filename = seb->ilucseb_filename;
      if (exnsrc_linenum ) *exnsrc_linenum  = seb->ilucseb_linenum ;
      return ilu_TRUE;
    }
  }
  return ilu_FALSE;
}

const char*	ILU_C_Exception_SrcFile( CORBA_Environment *env )
{
  const char *ans;
  if (ILU_C_exception_source( env, &ans, 0))
    if (ans)
      return ans;
  return "(no source location available)";
}

int		ILU_C_Exception_SrcLine( CORBA_Environment *env )
{
  int ans;
  if (ILU_C_exception_source( env, ILU_NIL, &ans))
    return ans;
  return 0;
}

CORBA_octet *
  CORBA_sequence_octet_allocbuf(CORBA_unsigned_long l)
{
  CORBA_octet *v = ilu_malloc(sizeof(CORBA_octet) * l);
  if (v == ILU_NIL)
    return _ILU_C_MallocFailure(sizeof(CORBA_octet) * l);
  else
    return v;
}

CORBA_boolean *
  CORBA_sequence_boolean_allocbuf(CORBA_unsigned_long l)
{
  CORBA_boolean *v = ilu_malloc(sizeof(CORBA_boolean) * l);
  if (v == ILU_NIL)
    return _ILU_C_MallocFailure(sizeof(CORBA_boolean) * l);
  else
    return v;
}

CORBA_char *
  CORBA_sequence_char_allocbuf(CORBA_unsigned_long l)
{
  CORBA_char *v = ilu_malloc(sizeof(CORBA_char) * l);
  if (v == ILU_NIL)
    return _ILU_C_MallocFailure(sizeof(CORBA_char) * l);
  else
    return v;
}

CORBA_unsigned_short *
  CORBA_sequence_unsigned_short_allocbuf(CORBA_unsigned_long l)
{
  CORBA_unsigned_short *v = ilu_malloc(sizeof(CORBA_unsigned_short) * l);
  if (v == ILU_NIL)
    return _ILU_C_MallocFailure(sizeof(CORBA_unsigned_short) * l);
  else
    return v;
}

CORBA_unsigned_long *
  CORBA_sequence_unsigned_long_allocbuf(CORBA_unsigned_long l)
{
  CORBA_unsigned_long *v = ilu_malloc(sizeof(CORBA_unsigned_long) * l);
  if (v == ILU_NIL)
    return _ILU_C_MallocFailure(sizeof(CORBA_unsigned_long) * l);
  else
    return v;
}

CORBA_short *
  CORBA_sequence_short_allocbuf(CORBA_unsigned_long l)
{
  CORBA_short *v = ilu_malloc(sizeof(CORBA_short) * l);
  if (v == ILU_NIL)
    return _ILU_C_MallocFailure(sizeof(CORBA_short) * l);
  else
    return v;
}

CORBA_long *
  CORBA_sequence_long_allocbuf(CORBA_unsigned_long l)
{
  CORBA_long *v = ilu_malloc(sizeof(CORBA_long) * l);
  if (v == ILU_NIL)
    return _ILU_C_MallocFailure(sizeof(CORBA_long) * l);
  else
    return v;
}

CORBA_float *
  CORBA_sequence_float_allocbuf(CORBA_unsigned_long l)
{
  CORBA_float *v = ilu_malloc(sizeof(CORBA_float) * l);
  if (v == ILU_NIL)
    return _ILU_C_MallocFailure(sizeof(CORBA_float) * l);
  else
    return v;
}

CORBA_double *
  CORBA_sequence_double_allocbuf(CORBA_unsigned_long l)
{
  CORBA_double *v = ilu_malloc(sizeof(CORBA_double) * l);
  if (v == ILU_NIL)
    return _ILU_C_MallocFailure(sizeof(CORBA_double) * l);
  else
    return v;
}

CORBA_string *
  CORBA_sequence_string_allocbuf(CORBA_unsigned_long l)
{
  CORBA_string *v = ilu_malloc(sizeof(CORBA_string) * l);
  if (v == ILU_NIL)
    return _ILU_C_MallocFailure(sizeof(CORBA_string) * l);
  else
    return v;
}

void CORBA_ORB_ObjectIdList__Free (CORBA_ORB_ObjectIdList* _val)
{
  /* What you put in the freeRoutine member of a CORBA_Environment for an exception parameterized by a CORBA_ORB_ObjectIdList */
  /* frees allocated storage inside _val (if any), but does not free(_val) */
  if (_val == ILU_NIL) return;
  if (_val->_buffer != NULL) {
    int i;
    for (i = 0;  i < _val->_length;  i++)
      ilu_CString__Free ((CORBA_ORB_ObjectId *) &_val->_buffer[i]);
    ilu_free(_val->_buffer);
  };
}

CORBA_ORB_ObjectIdList *CORBA_sequence_CORBA_ORB_ObjectIdList_allocbuf (CORBA_unsigned_long _count)
{
  CORBA_ORB_ObjectIdList *_p;
  CORBA_unsigned_long _size = sizeof(CORBA_ORB_ObjectIdList) * _count;

  if ((_p = (CORBA_ORB_ObjectIdList *) ilu_malloc(_size)) == ILU_NIL)
    { _ILU_C_MallocFailure(_size); return 0; }
  else
    {
      memset((void *) _p, 0, _size);
      return _p;
    }
}

CORBA_ORB_ObjectIdList* CORBA_ORB_ObjectIdList__alloc (void)
{
  return ((CORBA_ORB_ObjectIdList*) CORBA_sequence_CORBA_ORB_ObjectIdList_allocbuf(1));
}

void CORBA_ORB_ObjectIdList_Every (CORBA_ORB_ObjectIdList *h, void (*f)(CORBA_ORB_ObjectId *, void *), void * data)
{
  _ILU_C_EveryElement ((ILU_C_Sequence) h, (void (*)(void *, void *)) f, sizeof(CORBA_ORB_ObjectId), (void *) data);
}

void CORBA_ORB_ObjectIdList_Append (CORBA_ORB_ObjectIdList *h, CORBA_ORB_ObjectId item)
{
  _ILU_C_AppendGeneric ((ILU_C_Sequence) h, (char *) &item, sizeof(CORBA_ORB_ObjectId));
}

void CORBA_ORB_ObjectIdList_Push (CORBA_ORB_ObjectIdList *h, CORBA_ORB_ObjectId item)
{
  _ILU_C_PushGeneric ((ILU_C_Sequence) h, (char *) &item, sizeof(CORBA_ORB_ObjectId));
}

void CORBA_ORB_ObjectIdList_Pop (CORBA_ORB_ObjectIdList *h, CORBA_ORB_ObjectId *item)
{
  _ILU_C_PopGeneric ((ILU_C_Sequence) h, (char *) item, sizeof(CORBA_ORB_ObjectId));
}

CORBA_unsigned_long CORBA_ORB_ObjectIdList_Length (CORBA_ORB_ObjectIdList *h)
{
  if (h == ILU_NIL)
    return 0;
  else return h->_length;
}

CORBA_ORB_ObjectId * CORBA_ORB_ObjectIdList_Nth (CORBA_ORB_ObjectIdList *h, CORBA_unsigned_long n)
{
  if (h == ILU_NIL || (n >= h->_length))
    return ILU_NIL;
  else return &(h->_buffer[n]);
}

CORBA_ORB_ObjectIdList * CORBA_ORB_ObjectIdList_Create (CORBA_unsigned_long sz, CORBA_ORB_ObjectId *p)
{
  CORBA_ORB_ObjectIdList *s;
  s = (CORBA_ORB_ObjectIdList *) ilu_malloc(sizeof(CORBA_ORB_ObjectIdList));
  if (s == ILU_NIL) { _ILU_C_MallocFailure(sizeof(CORBA_ORB_ObjectIdList)); return ILU_NIL; };
  s->_maximum = sz;
  s->_length = (sz > 0 && p != ILU_NIL) ? sz : 0;
  s->_buffer = (p != ILU_NIL) ? p : ((sz > 0) ? ((CORBA_ORB_ObjectId *) ilu_malloc(sz * sizeof(CORBA_ORB_ObjectId))) : ILU_NIL);
  if ((s->_buffer == ILU_NIL) && sz > 0 && p == ILU_NIL) {
    _ILU_C_MallocFailure(sz * sizeof(CORBA_ORB_ObjectId));  ilu_free(s);  return ILU_NIL; };
  return s;
}

void CORBA_ORB_ObjectIdList_Init (CORBA_ORB_ObjectIdList *s, CORBA_unsigned_long sz, CORBA_ORB_ObjectId *p)
{
  if (sz == 0 && p != NULL)
    return;
  if (sz > 0)
    s->_maximum = sz;
  else
    s->_maximum = 0;
  if (sz > 0 && p != NULL)
    s->_length = sz;
  else
    s->_length = 0;
  if (sz > 0 && p == NULL) {
    s->_buffer = (CORBA_ORB_ObjectId *) ilu_malloc (sz * sizeof (CORBA_ORB_ObjectId));
    if (s->_buffer == ILU_NIL) {
      s->_length = 0;
      s->_maximum = 0;
      _ILU_C_MallocFailure(sz * sizeof(CORBA_ORB_ObjectId)); }}
  else
    s->_buffer = p;
  return;
}
