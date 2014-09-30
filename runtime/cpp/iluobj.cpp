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
/* $Id: iluobj.cpp,v 1.58 1999/08/03 01:55:46 janssen Exp $ */
/* Last tweaked by Mike Spreitzer August 1, 1996 3:46 pm PDT */

#include "ilu.hh"

extern "C" {
#include <stdio.h>

#if ((defined(WIN32) || defined(WIN16)) && defined(_WINIO))
#include <winiodef.h>
#elif defined( macintosh )
#include	<macos.h>
#endif /* ((defined(WIN32) || defined(WIN16)) && defined(_WINIO)) */

extern char *_ilu_Strdup(const char *);
}

iluObject::iluObject()
{
  this->ILURPCObject = NULL;
  this->ILURPCServer = NULL;
  this->ILUPublishProof = NULL;
  this->ILUInstanceHandle = NULL;
  this->ILUMostSpecificObj = NULL;
}

iluObject::iluObject(char *instance_handle)
{
  this->ILURPCObject = NULL;
  this->ILURPCServer = NULL;
  this->ILUPublishProof = NULL;
  this->ILUInstanceHandle = instance_handle;
  this->ILUMostSpecificObj = NULL;
}

iluObject::iluObject(ilu_Server s)
{
  this->ILURPCObject = NULL;
  this->ILURPCServer = s;
  this->ILUPublishProof = NULL;
  this->ILUInstanceHandle = NULL;
  this->ILUMostSpecificObj = NULL;
}

iluObject::iluObject(ilu_Server s, char *instance_handle)
{
  this->ILURPCObject = NULL;
  this->ILURPCServer = s;
  this->ILUPublishProof = NULL;
  this->ILUInstanceHandle = instance_handle;
  this->ILUMostSpecificObj = NULL;
}

iluObject::~iluObject()
{
  if (this->ILUPublishProof != NULL)
    this->ILUWithdraw();
  if (this->ILURPCServer != NULL)
    {
      ilu::EnterServer(this->ILURPCServer, this->ILUInstanceClassRecord);
      if (this->ILURPCObject != NULL)
        {
          ilu::SetLanguageSpecificObject (this->ILURPCObject, NULL);
          this->ILURPCObject = NULL;
        }
      ilu::ExitServer(this->ILURPCServer, this->ILUInstanceClassRecord);
    }
  if (this->ILUInstanceHandle != NULL)
    free(this->ILUInstanceHandle);
}

struct registry_record {
  ilu_Class c;
  class iluObject * (*proc)(ilu_KernelObject);
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

void iluObject::RegisterSurrogateCreator (ilu_Class c, class iluObject * (*proc)(ilu_KernelObject))
{
  struct registry_record *p;

  if ((p = findRegistryRecordByClass(c)) == NULL)
    {
      p = (struct registry_record *) malloc(sizeof(struct registry_record));
      p->c = c;
      p->proc = proc;
      p->next = RegisteredClasses;
      RegisteredClasses = p;
    }
}

/* Inside(obj->server, c) */
class iluObject * iluObject::CreateFromRegistry (ilu_Class c, ilu_KernelObject obj)
{
  class iluObject *lspo;
  struct registry_record *p;

  if ((p = findRegistryRecordByClass(c)) == NULL)
    return (NULL);
  else
    {
      lspo = (*(p->proc))(obj);
      return(lspo);
    }
}

void *iluObject::ILUCastDown (ilu_Class cast_to)
{
  if (cast_to == NULL)
    return ((void *) this);
  else	/* don't know how to cast to anything in this method */
    return (NULL);
}

ilu_Server iluObject::ILUGetKernelServer (void)
{
  return (this->ILURPCServer);
}

ilu_Server iluObject::ILUEnsureKernelServer ()
{
  if (this->ILURPCServer == NULL)
    {
      iluServer *server = this->ILUGetServer();

      if (server != NULL)
        this->ILURPCServer = server->KernelServer();
    }
  return this->ILURPCServer;
}

/* after: return obj != 0 implies Inside(obj->server, obj->class) */
ilu_KernelObject iluObject::ILUGetRPCObject ()
{
  if (this->ILUEnsureKernelServer() == NULL)
    return 0;
  ilu_EnterServer(this->ILURPCServer, this->ILUInstanceClassRecord);
  (void) this->ILUEnsureKernelObject();
  if (this->ILURPCObject == NULL)
    ilu_ExitServer(this->ILURPCServer, this->ILUInstanceClassRecord);
  return this->ILURPCObject;
}

/* Inside(obj->server, obj->class) */
void iluObject::ILUSetRPCObject (ilu_KernelObject obj)
{
  this->ILURPCServer = ilu_ServerOfObject(obj);
  this->ILURPCObject = obj;
}

ilu_CString iluObject::ILUStringBindingHandle ()
{
  return (ilu::SBHOfObject(this->ILUGetRPCObject()));
}

char *iluObject::ILUGetInstanceHandle ()
{
  return (this->ILUInstanceHandle);
}

/* Inside(obj->server, obj->class) */
ilu_KernelObject iluObject::ILUEnsureKernelObject()
{
  if (this->ILUEnsureKernelServer() == NULL)
    return 0;
  if (this->ILURPCObject == NULL) {
    /* Should be a true object. */
    static unsigned long idcounter = 0;
    char           *id;
    ilu_Error       lerr;
    char            idbuf[10];

    if ((id = this->ILUGetInstanceHandle()) == NULL) {
      id = idbuf;
      sprintf(idbuf, "%lu", ++idcounter);
      this->ILUInstanceHandle = ilu_StrdupE(idbuf, &lerr);
      if (ILU_ERRNOK(lerr)) {
	ILU_HANDLED(lerr);
	return 0;
      }
    }

  this->ILURPCObject = ilu::CreateTrueKernelObject
      (id, this->ILURPCServer,
      this->ILUInstanceClassRecord, this);
  }
  return this->ILURPCObject;
}

class iluServer * iluObject::ILUGetServer ()
{
  return ilu::GetDefaultServer();
}

void * iluObject::InputObject (iluCall call, ilu_Boolean discriminator_p, ilu_Class putative_class)
{
  ilu_KernelObject obj;
  class iluObject *o;
  ilu_Class       c;

  if (discriminator_p && putative_class->cl_singleton) {
    if ((obj = ilu_GetCallSingleton(&call->call, &call->err)) == NULL) {
      ilu_PreferSuccess(&call->err);
      return (NULL);
    }
  } else {
    obj = ilu::InputObjectID(call, discriminator_p, putative_class);
    if (obj == NULL)
      return (NULL);
  }

  /* now Inside(obj->server, putative_class) */
  o = (class iluObject *) ilu::GetLanguageSpecificObject(obj);
  if (o == NULL) {
    if ((c = ilu::GetObjectClass(obj)) != NULL)
      o = iluObject::CreateFromRegistry(c, obj);
  }
  ilu::ExitServer(ilu::GetObjectServer(obj), putative_class);
  if (o == NULL)
    return (o);
  else
    return (o->ILUCastDown(putative_class));
}

ilu_Boolean iluObject::OutputObject (iluCall call, class iluObject *obj, ilu_Class putative_class)
{
  ilu_KernelObject kobj;
  kobj = (obj == NULL) ? ((ilu_KernelObject) 0) : obj->ILUGetRPCObject();

  /* now kobj != NULL implies Inside(kobj->server, kobj->class) */
  return (ilu::OutputObjectID(call, kobj, ilu_FALSE, putative_class));
}

ilu_Cardinal iluObject::SizeOfObject (iluCall call, class iluObject *obj, ilu_Class putative_class)
{
  ilu_KernelObject kobj;
  kobj = (obj == NULL) ? ((ilu_KernelObject) 0) : obj->ILUGetRPCObject();

  /* now kobj != NULL implies Inside(kobj->server, kobj->class) */
  return (ilu::SizeOfObjectID(call, kobj, ilu_FALSE, putative_class));
}

ilu_Boolean iluObject::ILUPublish ()
{
  ilu_KernelObject kobj = this->ILUGetRPCObject();

  /* now Inside(kobj->server, kobj->class) */
  if ((this->ILUPublishProof = ilu_PublishObject (kobj)) == NULL)
    return ilu_FALSE;
  else
    return ilu_TRUE;
}

ilu_Boolean iluObject::ILUWithdraw ()
{
  ilu_Boolean status;
  ilu_KernelObject kobj = this->ILUGetRPCObject();

  /* now Inside(kobj->server, kobj->class) */
  status = ilu_WithdrawObject (kobj, this->ILUPublishProof);
  free(this->ILUPublishProof);
  this->ILUPublishProof = NULL;
  return (status);
}

void * iluObject::Lookup (char *sid, char *ih, ilu_Class pclass)
{
  ilu_KernelObject kobj;
  class iluObject *o;

  if ((kobj = ilu_LookupObject (sid, ih, pclass)) == NULL)
    return (NULL);
  /* now Inside the server of the kobj */
  if ((o = (class iluObject *) ilu::GetLanguageSpecificObject(kobj)) == NULL)
    {
      o = iluObject::CreateFromRegistry (pclass, kobj);
    }
  ilu::ExitServer(ilu::GetObjectServer(kobj), pclass);
  if (o == NULL)
    return (o);
  else
    return o->ILUCastDown(pclass);
}


// Returns ilu_TRUE if the true object exists, and the process
// serving it can be contacted, otherwise ilu_FALSE
// *p_error is set if some error occurred.

ilu_Boolean iluObject::ILUPing(ilu_Error* p_error) {
    
    ilu_Connection  new_connnection_to_monitor = ILU_NIL;
    ilu_Boolean b_successful_ping;
    
    ILU_CLER(*p_error);
    
    if (!ILURPCObject) { // make sure we have a kernel object
        ILU_ERR_CONS1(bad_param, p_error, minor, ilu_bpm_closed, ilu_FALSE);
        return ilu_FALSE;
    }
    
    // increment the objects hold count
    *p_error = ilu_DeltaHolds(ILURPCObject, 1);
    ilu_ExitServer(ILURPCServer, ILUInstanceClassRecord);

    if (ILU_ERRNOK(*p_error)) // had some sort of delta holds problem
        return ilu_FALSE;
    
    // do the actual ping
    b_successful_ping = (ilu_PingObject(ILURPCObject, &new_connnection_to_monitor) ? ilu_TRUE : ilu_FALSE);
    
    // monitor any new connection returned
    if (new_connnection_to_monitor != ILU_NIL)
        iluServer::MonitorOutgoingConnection(new_connnection_to_monitor);
    
    // decrement the objects hold count
    ilu_EnterServer(ILURPCServer, ILUInstanceClassRecord);
    *p_error = ilu_DeltaHolds(ILURPCObject, -1);
    ilu_ExitServer(ILURPCServer, ILUInstanceClassRecord);
    if (ILU_ERRNOK(*p_error)) // had some sort of delta holds problem
        return ilu_FALSE;
    
    return b_successful_ping;
}


char * iluObject::ILUClassName (void)
{
  return (this->ILUInstanceClassRecord
	  ? this->ILUInstanceClassRecord->cl_name
	  : (char *) 0);
}

char * iluObject::ILUClassId (void)
{
  return (this->ILUInstanceClassRecord
	  ? this->ILUInstanceClassRecord->cl_unique_id
	  : (char *) 0);
}

void iluObject::_ILU_RegisterAsGCCallback (class iluObject *c)
{
  ilu_KernelObject kobj;

  if ((kobj = c->ILUGetRPCObject()) != NULL)
    {
      ilu_SetGcClient (kobj);	// register ilu_Object with GC handler in kernel
      ilu_ExitServer(c->ILURPCServer, c->ILUInstanceClassRecord);
    }
}

//////////////////////////////////////////////////////////////////////
// Implement ilu.CORBA-Object
//////////////////////////////////////////////////////////////////////

ilu_Class ilu_T_CORBA_Object::ILUClassRecord = NULL;

class ilu_T_CORBA_Object * ilu_T_CORBA_Object::ILUCreateFromSBH (ilu_CString sbh)
{
  return (ilu_T_CORBA_Object *) ilu::SBHToObject(sbh, ilu_T_CORBA_Object::ILUClassRecord);
}

class ilu_T_CORBA_Object * ilu_T_CORBA_Object::ILUQuaT (class iluObject *from)
{
  return((class ilu_T_CORBA_Object *) (from->ILUCastDown (ilu_T_CORBA_Object::ILUClassRecord)));
}

void * ilu_T_CORBA_Object::ILUCastDown (ilu_Class cast_to)
{
  if (cast_to == NULL)
    return((void *)((class iluObject *) this));
  else if (cast_to == ilu_T_CORBA_Object::ILUClassRecord)
    return ((void *) this);
  else return (NULL);
}

ilu_T_CORBA_Object::ilu_T_CORBA_Object ()
{
  this->ILUInstanceClassRecord = ilu_T_CORBA_Object::ILUClassRecord;
  this->ILUSetMostSpecificObject((void *) this);
}

ilu_T_CORBA_Object::~ilu_T_CORBA_Object ()
{
}

static class iluObject * Create_ilu_T_CORBA_Object(ilu_KernelObject obj) {
  class ilu_T_CORBA_Object *nobj = new ilu_T_CORBA_Object;
  nobj->ILUSetRPCObject(obj);
  ilu::SetLanguageSpecificObject(obj, (class iluObject *) nobj);
  return ((class iluObject *) nobj);
}

//////////////////////////////////////////////////////////////////////
// code to do the GC keepalive acknowedgements
//////////////////////////////////////////////////////////////////////

class _ilu_GCCallback : public virtual iluObject {
  public:
    _ilu_GCCallback(class iluServer *server);
    virtual ~_ilu_GCCallback();

  static class _ilu_GCCallback * ILUCreateFromSBH(ilu_CString sbh);
  static class _ilu_GCCallback * ILUQuaT (class iluObject *from);
  static ilu_Class ILUClassRecord;

  virtual void * ILUCastDown (ilu_Class cast_to);

  virtual iluServer * ILUGetServer();

  private:
   class iluServer *ourServer;
};

ilu_Class _ilu_GCCallback::ILUClassRecord = NULL;

class _ilu_GCCallback * _ilu_GCCallback::ILUCreateFromSBH (ilu_CString sbh)
{
  return (_ilu_GCCallback *) ilu::SBHToObject(sbh, _ilu_GCCallback::ILUClassRecord);
}

class _ilu_GCCallback * _ilu_GCCallback::ILUQuaT (class iluObject *from)
{
  return((class _ilu_GCCallback *) (from->ILUCastDown (_ilu_GCCallback::ILUClassRecord)));
}

void * _ilu_GCCallback::ILUCastDown (ilu_Class cast_to)
{
  if (cast_to == NULL)
    return((void *)((class iluObject *) this));
  else if (cast_to == _ilu_GCCallback::ILUClassRecord)
    return ((void *) this);
  else return (NULL);
}

_ilu_GCCallback::_ilu_GCCallback (class iluServer *server)
{
  this->ILUInstanceClassRecord = ilu_GetGcCallbackClass();
  this->ILUSetMostSpecificObject((void *) this);
  this->ourServer = server;
}

_ilu_GCCallback::~_ilu_GCCallback ()
{
}

class iluServer *_ilu_GCCallback::ILUGetServer ()
{
  return (this->ourServer);
}

static class iluObject * Create__ilu_GCCallback(ilu_KernelObject obj)
{
  class _ilu_GCCallback *nobj = new _ilu_GCCallback(NULL);
  nobj->ILUSetRPCObject(obj);
  ilu::SetLanguageSpecificObject(obj, (class iluObject *) nobj);
  return ((class iluObject *) nobj);
}

class _ilu_GCCallbackInit {
  public:
    _ilu_GCCallbackInit();
};

static _ilu_GCCallback * GCCallbackObject = NULL;

#ifdef macintosh
static _ilu_GCCallbackInit* do_init = NULL;

void ILUStartup()
{
	StartupWinsock();
	
	do_init = new _ilu_GCCallbackInit;
}

void ILUShutdown()
{
	delete do_init;
	do_init = 0;
	ShutdownWinsock();
}

#else
static _ilu_GCCallbackInit do_init;
#endif

void iluObject::InitializeGCCallback()
{
  class iluServer *server;

  if (ilu_IsGcClientSet())
	// some other runtime must have already set things up for this process
	return;

  // Initialize GCCallback class

  _ilu_GCCallback::ILUClassRecord = ilu_GetGcCallbackClass();
  iluObject::RegisterSurrogateCreator (_ilu_GCCallback::ILUClassRecord, Create__ilu_GCCallback);

  // create instance of GCCallback on server

  server = new iluServer(NULL, NULL);
  server->AddPort (NULL, NULL, ilu_TRUE);
  GCCallbackObject = new _ilu_GCCallback(server);

  iluObject::_ILU_RegisterAsGCCallback(GCCallbackObject);
}

void iluObject::InitializeCORBAObject()
{
  // Initialize CORBA-Object class
  ilu::EnterOTMu();
  {	// for definition of ilu_T_CORBA_Object
    ilu_Class t;
    ilu_T_CORBA_Object::ILUClassRecord = t = ilu::DefineObjectType(
	"ilu.CORBA-Object",	/* ILU name */
	"",	/* Brand */
	(char *) ilu_TypeID_ilu_CORBA_Object,
	NULL,	/* singleton */
	ilu_kernelTRUE,	/* optional? */
	ilu_kernelFALSE,	/* collectible? */
	NULL,	/* authentication */
	0,	/* number of methods */
	0,	/* number of superclasses */
	NULL	/* no superclass uids */);
    iluObject::RegisterSurrogateCreator(t, Create_ilu_T_CORBA_Object);
    ilu::ObjectTypeDefined(t);
  }	// end definition of ilu_T_CORBA_Object
  ilu::ExitOTMu();
}

_ilu_GCCallbackInit::_ilu_GCCallbackInit ()
{
  ilu::InitializeRuntime();
}

