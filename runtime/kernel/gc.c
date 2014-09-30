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
/* $Id: gc.c,v 1.81 1999/08/03 01:52:53 janssen Exp $ */
/* Last tweaked by Mike Spreitzer September 22, 1998 10:49 pm PDT */

#define _POSIX_SOURCE

#include "iluntrnl.h"

#include "object.h"
#include "call.h"
#include "server.h"
#include "type.h"

/* ====================== Client & Server Stuff ====================== */

/*for taking address: L1, L2, Main unconstrained*/

static struct _ilu_Class_s GCCallbackClassRecord = {
  GC_CALLBACK_CLASS_ID,	/* ILU name */
  "",			/* Brand */
  "ilu:gc-callback-class", /* MSTID */
  NIL,			/* singleton? */
  ilu_FALSE,		/* collectible? */
  NIL,			/* authentication */
  NIL,			/* methods table */
  0,			/* method count */
  0,			/* superclass count */
  NIL,			/* superclass ids base ptr */
  NIL,			/* superclass unique_id base ptr */
#ifdef ILU_HTTPNG_OBJECTS
  0,
  NIL,			/* state */
  ilu_FALSE,		/* local? */
  ilu_FALSE,		/* sealed? */
#endif
  ilu_FALSE,		/* shown */
  ilu_FALSE,		/* optional? */
  ilu_FALSE			/* phony? */
};

/*L1, L2, Main unconstrained*/

const ilu_Class _ilu_GcCallbackClass = &GCCallbackClassRecord;

/*L1 >= {obj's server};
  obj true && collectible => L1 >= {gcmu}*/
ilu_boolean ilu_VeryInterested(ilu_Object obj)
{
  if (object_is_true(obj)) {
    if (object_gclist(obj) != NIL &&
	_ilu_vector_size(VECTOR(object_gclist(obj))) != 0)
      return ilu_TRUE;
    if (class_collectible(obj->ob_class)
	&& (ilu_CoarseTime_Now()
	    < object_lastRemote(obj) + obj->ob_timeout))
      return ilu_TRUE;
  } else {
    if (object_notifying(obj) && !object_known(obj))
      return ilu_TRUE;
  }
  if (object_holds(obj) != 0)
    return ilu_TRUE;
  return ilu_FALSE;
}


/* ====================== Server Side Stuff ====================== */

static ilu_FineTime pingPeriod = {300, 0};
/* server pings callback client handles every "pingPeriod" seconds
 * to see if they are still there or have crashed.
 */

ilu_integer		  _ilu_GCTimeOut = 1200;
/* after all client registrations have been removed from a true object,
 * the server waits this number of seconds for possible new but slow
 * client registrations to reach it before losing interest in the object
 */

typedef struct {
  /*L1 >= {gcmu}; L2, Main unconstrained*/

  ilu_Alarmette_s gcc;	/* Periodically prompts liveness test of client */
  ilu_Object client;	/* callback interface to client */
  ilu_cardinal nObjs;	/* number of surrogates in client */
  ilu_FineTime period;	/* time between pings to callback object */
  ilu_boolean sweeping;	/* SweepGCCallbacks holds this outside gcmu */
} counted_client;
/* A server program has one of these structs for every client that has some collectible surrogates of this server. */

/*L1 >= {gcmu}; L2, Main unconstrained*/

static ilu_Alarmette_s gcoHead = {&gcoHead, &gcoHead, ilu_FALSE, {0,0}};
/* There's an alarm per collectible true object, which goes off timeout after max(lastLocal, lastRemote).  We multiplex these alarms into one alarm, _ilu_gcoAlarm, using mxamu==gcmu. */

static ilu_Alarmette_s gccHead = {&gccHead, &gccHead, ilu_FALSE, {0,0}};
/* There's an alarm per interested client, which goes off periodically to prompt liveness testing.  We multiplex these alarms into one alarm, _ilu_gccAlarm, using mxamu==gcmu. */

/*L1_sup = gcmu*/
static void gcoInvoke(ilu_Alarmette a);
/*L1 = {gcmu}*/
static void gccInvoke(ilu_Alarmette a);
static void gcoUrset(ilu_FineTime t);
static void gccUrset(ilu_FineTime t);
static void gcoUrcancel(void);
static void gccUrcancel(void);

static ilu_AlarmRep gcor = {&gcoHead, gcoInvoke, gcoUrset, gcoUrcancel};
static ilu_AlarmRep gccr = {&gccHead, gccInvoke, gccUrset, gccUrcancel};

static HashTable Clients = NIL;
/* Contains a pair <cc->client, cc> for every counted client of this prog.
 */

static ilu_Vector Objects = NIL;
/* The set {obj | obj is a GCed true object} */

/* gcmu invariant:
   cc->nObjs =  |{obj | cc in object_gclist(obj)}|.
   Objects = {obj | obj is true && collectible && not released}.
   Clients = {<cc->client, cc> | cc->nObjs != 0 || cc->sweeping}.
   (cc->nObjs != 0 || cc->sweeping) contributes 1 to cc->client->ob_holds.
   cc->gcc is set iff (cc->nObjs != 0 || cc->sweeping).
   forall collectible true obj:
	object_gclist(obj) is empty => lastRemote is when it became empty;
 */

/*L1 < gcmu */
ilu_FineTime
ilu_SetDefaultGCPingPeriod (ilu_FineTime period)
{
  ilu_FineTime oldPeriod;

  ilu_AcquireMutex(ilu_gcmu);
  oldPeriod= pingPeriod;
  pingPeriod = period;
  ilu_ReleaseMutex(ilu_gcmu);
  return oldPeriod;
}

/*L1 >= {gcmu, cmu, obj's server}*/
ILU_ERRS((bad_locks, broken_locks, internal))
_ilu_TouchedObj(ilu_Object obj)
{
  int             should;
  ilu_Error       ans;
  ASSERT(object_collectible(obj), buf,
	 (buf, "gc.c:TouchedObj: !collectible(%s/%s)",
	  obj->ob_server->sr_id, obj->ob_ih));
  ASSERT(object_is_true(obj), buf,
	 (buf, "gc.c:TouchedObj: surrogate(%s/%s)",
	  obj->ob_server->sr_id, obj->ob_ih));
  should = ((object_gclist(obj) == NIL)
	    || (_ilu_vector_size(VECTOR(object_gclist(obj))) == 0));
  if (should) {
    ilu_FineTime    t = {0, 0};
    t.ft_s = obj->ob_timeout + object_lastRemote(obj);
    ilu_MXASet(&gcor, &object_gco(obj), t);
    ILU_NOTE(GC_DEBUG,
	     ("_ilu_TouchedObj(%s/%s):  no refs, beginning %lu sec timeout\n",
	      server_id(object_server(obj)), object_ih(obj), t.ft_s - ilu_CoarseTime_Now()));
  } else {
    ilu_MXAClear(&gcor, &object_gco(obj));
    ILU_NOTE(GC_DEBUG,
	     ("_ilu_TouchedObj(%s/%s):  has refs, cancelling timeout\n",
	      server_id(object_server(obj)), object_ih(obj)));
  }
  ans = _ilu_VIUpdate(obj);
  ILU_ERR_SWITCH(ans) {
    ILU_SUCCESS_CASE
      return ans;
    ILU_ERR_CASE3(bad_locks, broken_locks, internal)
      return ans;
    ILU_ERR_CASE(GcRegFailed, x)
    /* This can't happen, because obj is true. */
      _ilu_Assert(0, "VIUpdate(obj) => GcRegFailed in TouchedObj");
  } ILU_ERR_ENDSWITCH;
  return ans;
}

/*Main Invariant holds*/

static void CallMXO(ilu_private rock)
{
  _ilu_AcquireMutex(ilu_gcmu);
  ilu_MXAProc(ilu_FineTime_Now(), &gcor);
  _ilu_ReleaseMutex(ilu_gcmu);
}

static void CallMXC(ilu_private rock)
{
  _ilu_AcquireMutex(ilu_gcmu);
  ilu_MXAProc(ilu_FineTime_Now(), &gccr);
  _ilu_ReleaseMutex(ilu_gcmu);
}

/*forall conn: (L2 >= {conn.iomu}) => (L2 >= {conn.callmu})*/

/*L1 >= {gcmu, cmu}*/
static 
ILU_ERRS((internal, bad_locks, broken_locks))
DecrementObjCount(counted_client * cc, ilu_boolean fullFlush)
{
  ilu_Error       err = ILU_NO_ERR;
  cc->nObjs -= 1;
  if (cc->nObjs == 0 && cc->sweeping == ilu_FALSE) {
    if (!ilu_Check(ilu_hash_RemoveFromTable(Clients, cc->client) == cc,
		   &err))
      return err;
    ilu_MXAClear(&gccr, &cc->gcc);
    if (fullFlush == ilu_TRUE) {
      ilu_Server      s = object_server(cc->client);
      _ilu_AcquireServerMutex(s);
      err = _ilu_DeltaHolds(cc->client, -1);
      /* Won't raise GcRegFailed because clients aren't GCed */
      _ilu_ReleaseServerMutex(s);
      ilu_free(cc);
    }
  }
  return err;
}

/*L1_sup = gcmu*/
static void
gcoInvoke(ilu_Alarmette a)
{
  ilu_Error       err;
  static struct _ilu_Object_s dumby;
  ilu_Object      obj = (ilu_Object) (((char *) a) -
	       (((char *) &dumby.ob_collectibleInfo.ob_true.ob_gco)
		- ((char *) &dumby)));
  ilu_Server      s = object_server(obj);
  _ilu_AcquireMutex(ilu_cmu);
  _ilu_AcquireServerMutex(s);
  ASSERT(object_collectible(obj), buf,
	 (buf, "gc.c:gcoInvoke: !collectible(%s/%s)",
	  s->sr_id, obj->ob_ih));
  ASSERT(object_is_true(obj), buf,
	 (buf, "gc.c:gcoInvoke: surrogate(%s/%s)",
	  s->sr_id, obj->ob_ih));
  err = _ilu_VIUpdate(obj);
  ILU_MUST_BE_SUCCESS(err);
  _ilu_ReleaseServerMutex(s);
  _ilu_ReleaseMutex(ilu_cmu);
  return;
}

/*L1 >= {gcmu}; L2, Main unconstrained*/

static void gcoUrset(ilu_FineTime t)
{
  ilu_SetAlarm(_ilu_gcoAlarm, t, CallMXO, NIL);
}

static void gccUrset(ilu_FineTime t)
{
  ilu_SetAlarm(_ilu_gccAlarm, t, CallMXC, NIL);
}

static void gcoUrcancel(void)
{
  ilu_UnsetAlarm(_ilu_gcoAlarm);
}

static void gccUrcancel(void)
{
  ilu_UnsetAlarm(_ilu_gccAlarm);
}

void _ilu_StartGCingTrueObj (ilu_Object obj)
{
  ilu_Error lerr;
  _ilu_HoldMutex(ilu_gcmu);
  if (Objects == NIL) {
    Objects = _ilu_vector_new(INITIAL_GCDOBJECTSET_SIZE, &lerr);
    ILU_MUST_BE_SUCCESS(lerr);
  };
  _ilu_vector_add (Objects, obj, &lerr);
  ILU_MUST_BE_SUCCESS(lerr);
  return;
}

void _ilu_StopGCingTrueObj (ilu_Object obj)
{
  _ilu_HoldMutex(ilu_gcmu);
  if (object_gclist(obj) != NIL)
      _ilu_vector_destroy(VECTOR(object_gclist(obj)),
                          NULLFN/*vector is empty*/);
  object_gclist(obj) = NIL;
  _ilu_vector_remove (Objects, obj);
  return;
}

/*Main Invariant holds*/
static
ILU_ERRS((no_memory, internal,
	  bad_locks, broken_locks))
AddGCInterest(ilu_Object obj, ilu_Object interest)
{
  counted_client *cc = NIL, **p;
  register ilu_integer i;
  ilu_integer     j;
  ilu_Server      s, is;
  ilu_Error       err = ILU_INIT_NO_ERR;

  if (obj == NIL || interest == NIL)
    return ILU_NO_ERR;
  s = object_server(obj);
  is = object_server(interest);
  if (!ilu_Check(!!Objects, &err))
    /* Because StartGCingTrueObj must have been called on obj. */
    return err;
  ILU_NOTE(GC_DEBUG,
	   ("ILU:  adding GC interest in <%s/%s> from <%s/%s>\n",
	    server_id(object_server(obj)), object_ih(obj),
	    server_id(object_server(interest)), object_ih(interest)));
  _ilu_AcquireMutex(ilu_gcmu);
  _ilu_AcquireMutex(ilu_cmu);
  _ilu_AcquireServerMutex(s);
  if (object_gclist(obj) == NIL) {
    object_gclist(obj) = (void *) _ilu_vector_new(INITIAL_GCLIST_SIZE,
						  &err);
    ILU_MUST_BE_SUCCESS(err);
  };
  if (Clients == NIL)
    Clients = ilu_hash_MakeNewTable(INITIAL_GCCLIENTS_SIZE,
		 ilu_hash_HashPointer, ilu_hash_PointerCompare);
  for (i = 0, j = _ilu_vector_size(VECTOR(object_gclist(obj))),
       p = (counted_client **)
       _ilu_vector_elements(VECTOR(object_gclist(obj)));
       i < j; i++)
    if (p[i]->client == interest) {
      _ilu_ReleaseServerMutex(s);
      _ilu_ReleaseMutex(ilu_cmu);
      _ilu_ReleaseMutex(ilu_gcmu);
      return ILU_NO_ERR;			/* already there */
    }
  cc = (counted_client *) ilu_hash_FindInTable(Clients, interest);
  if (cc == NIL) {
    ilu_Alarmette_s gcc = {NIL, NIL, ilu_FALSE, {0, 0}};
    cc = (counted_client *) ilu_malloc(sizeof(counted_client));
    cc->nObjs = 0;
    cc->client = interest;
    cc->period = pingPeriod;
    cc->sweeping = ilu_FALSE;
    cc->gcc = gcc;
    _ilu_Assert((int)ilu_hash_AddToTable(Clients, interest, cc),
		"AddGCInterest AddToTable Clients");
    ilu_MXASet(&gccr, &cc->gcc,
	       ilu_FineTime_Add(ilu_FineTime_Now(), cc->period));
  };
  cc->nObjs += 1;
  if (cc->nObjs == 1) {
    err = _ilu_DeltaHolds(interest, 1);
    ILU_ERR_SWITCH(err) {
      ILU_SUCCESS_CASE;
      ILU_ERR_CASE3(internal, bad_locks, broken_locks) goto fini;
      ILU_ERR_CASE(GcRegFailed, x)
        _ilu_Assert(0, "DeltaHolds(interest) => GcRegFailed");
    } ILU_ERR_ENDSWITCH;
  }
  _ilu_vector_add(VECTOR(object_gclist(obj)), cc, &err);
  ILU_MUST_BE_SUCCESS(err);
  if (_ilu_vector_size(VECTOR(object_gclist(obj))) == 1) {
    err = _ilu_TouchedObj(obj);
    ILU_ERR_SWITCH(err) {
      ILU_SUCCESS_CASE;
      ILU_ERR_CASE(internal, x) goto fini;
    } ILU_ERR_ENDSWITCH;
  }
fini:
  _ilu_ReleaseServerMutex(s);
  _ilu_ReleaseMutex(ilu_cmu);
  _ilu_ReleaseMutex(ilu_gcmu);
  return err;
}

/*L1 = {gcmu, cmu, obj's server} before, {gcmu, cmu} after;
  forall conn: (L2 >= {conn.iomu}) => (L2 >= {conn.callmu})*/
static
ILU_ERRS((internal, bad_locks, broken_locks))
DropGCInterest(ilu_Object obj, /* non-NIL true obj of collectible type */
	       ilu_Object interest, /* non-NIL surrogate obj of type GCCallback */
	       ilu_boolean fullFlush)
{
  register ilu_integer i, j;
  register counted_client **p;
  ILU_ERRS((internal, bad_locks, broken_locks)) err = ILU_INIT_NO_ERR;

  if (!ilu_Check(obj && interest, &err))
    return err;
  else if (object_gclist(obj) == NIL)
    {
      _ilu_ReleaseServerMutex(object_server(obj));
      return err;
    }
  ILU_NOTE(GC_DEBUG,
	   ("ILU:  dropping GC interest in <%s/%s> from <%s/%s>\n",
	    server_id(object_server(obj)), object_ih(obj),
	    server_id(object_server(interest)), object_ih(interest)));
  for (i = 0, j = _ilu_vector_size(VECTOR(object_gclist(obj))),
       p = (counted_client **)
       _ilu_vector_elements(VECTOR(object_gclist(obj)));
       i < j; i++) {
    if (p[i]->client == interest) {
      counted_client *p2 = p[i];
      _ilu_vector_remove(VECTOR(object_gclist(obj)), p2);
      if (_ilu_vector_size(VECTOR(object_gclist(obj))) == 0) {
	object_lastRemote(obj) = ilu_CoarseTime_Now();
	err = _ilu_TouchedObj(obj);
	ILU_ERR_SWITCH(err) {
	  ILU_SUCCESS_CASE;
	  ILU_ERR_CASE(internal, x) return err;
	} ILU_ERR_ENDSWITCH;
      }
      _ilu_ReleaseServerMutex(object_server(obj));

      err = DecrementObjCount(p2, fullFlush);
      return err;
    }
  }
  _ilu_ReleaseServerMutex(object_server(obj));
  return err;
}

/*Main Invariant holds; L2 otherwise unconstrained*/
  
static void StdFinish(ilu_Object obj)
{
  if (obj != NIL) {
      ilu_Error err;
      ilu_Server s = object_server(obj);
      ilu_Class cl = object_class(obj);
      ilu_EnterServer(s, cl);
      err = _ilu_DeltaHolds(obj, -1);
      ilu_ExitServer(s, cl);
      ILU_MUST_BE_SUCCESS(err);
    }
  return;
}

/*L2    >=    {conn's callmu, iomu} before,
  L2 disjoint {conn's callmu, iomu} after*/

void _ilu_HandleGCInterestRegistration(ilu_Call call)
{
  ilu_Object      disc, interest;
  ilu_Error       lerr = ILU_INIT_NO_ERR;

  ilu_InputObjectID(call, &disc, ilu_TRUE, _ilu_rootClass, &lerr);
  if (ILU_ERRNOK(lerr)) goto dun;
  if (disc != NIL) {
    lerr = _ilu_DeltaHolds(disc, 1);
    ILU_MUST_BE_SUCCESS(lerr);
    ilu_ExitServer(object_server(disc), _ilu_rootClass);
  }
  ilu_InputObjectID(call, &interest, ilu_FALSE, _ilu_GcCallbackClass, &lerr);
  if (ILU_ERRNOK(lerr)) goto dun;
  if (interest != NIL) {
    lerr = _ilu_DeltaHolds(interest, 1);
    ILU_MUST_BE_SUCCESS(lerr);
    ilu_ExitServer(object_server(interest), _ilu_GcCallbackClass);
  }
  if (!ilu_RequestRead(call, &lerr)) goto dun;

  if (interest != NIL && object_collectible(interest)) {
    StdFinish(interest);
    interest = NIL;
  }
  if (interest != NIL && disc != NIL) {
    ilu_cardinal rsize;
    if (object_collectible(disc))
      lerr = AddGCInterest(disc, interest);
    ILU_MUST_BE_SUCCESS(lerr);
    rsize = ilu_BeginSizingReply(call, ilu_FALSE, &lerr);
    if (ILU_ERRNOK(lerr)) goto dun;
    if (!ilu_BeginReply(call, ilu_FALSE, rsize, &lerr))
      goto dun;
    if (!ilu_FinishReply(call, &lerr))
      goto dun;
  } else {
    ilu_cardinal asize = ilu_BeginSizingException (call, - (ilu_integer) ilu_ProtocolException_GarbageArguments, &lerr);
    if (ILU_ERRNOK(lerr)) goto dun;
    if (!ilu_BeginException(call, - (ilu_integer) ilu_ProtocolException_GarbageArguments,
			    asize, &lerr))
      goto dun;
    if (!ilu_FinishException(call, &lerr))
      goto dun;
  }
dun:
  ILU_HANDLED(lerr);
  StdFinish(disc);
  StdFinish(interest);
  return;
}
     
void _ilu_HandleGCInterestDeregistration (ilu_Call call)
{
  ilu_Object      disc, interest;
  ilu_Server      s;
  ilu_Error       lerr = ILU_INIT_NO_ERR;

  ilu_InputObjectID(call, &disc, ilu_TRUE, _ilu_rootClass, &lerr);
  if (ILU_ERRNOK(lerr)) goto dun;
  if (disc != NIL) {
    lerr = _ilu_DeltaHolds(disc, 1);
    ILU_MUST_BE_SUCCESS(lerr);
    ilu_ExitServer(object_server(disc), _ilu_rootClass);
  }
  ilu_InputObjectID(call, &interest, ilu_FALSE, _ilu_GcCallbackClass, &lerr);
  if (ILU_ERRNOK(lerr)) goto dun;
  if (interest != NIL) {
    lerr = _ilu_DeltaHolds(interest, 1);
    ILU_MUST_BE_SUCCESS(lerr);
    ilu_ExitServer(object_server(interest), _ilu_GcCallbackClass);
  }
  if (!ilu_RequestRead(call, &lerr))
    goto dun;
  if (interest != NIL && object_collectible(interest)) {
    StdFinish(interest);
    interest = NIL;
  }
  if (disc != NIL && interest != NIL) {
    ilu_cardinal rsize;
    s = object_server(disc);
    _ilu_AcquireMutex(ilu_gcmu);
    _ilu_AcquireMutex(ilu_cmu);
    _ilu_AcquireServerMutex(s);
    lerr = DropGCInterest(disc, interest, ilu_TRUE);
    ILU_MUST_BE_SUCCESS(lerr);
    _ilu_ReleaseMutex(ilu_cmu);
    _ilu_ReleaseMutex(ilu_gcmu);
    rsize = ilu_BeginSizingReply (call, ilu_FALSE, &lerr);
    if (ILU_ERRNOK(lerr))
      goto dun;
    if (!ilu_BeginReply(call, ilu_FALSE, rsize, &lerr))
      goto dun;
    if (!ilu_FinishReply(call, &lerr))
      goto dun;
  } else {
    ilu_cardinal asize = ilu_BeginSizingException (call, - (ilu_integer) ilu_ProtocolException_GarbageArguments, &lerr);
    if (ILU_ERRNOK(lerr)) goto dun;
    if (!ilu_BeginException(call, - (ilu_integer) ilu_ProtocolException_GarbageArguments,
			    asize, &lerr))
      goto dun;
    if (!ilu_FinishException(call, &lerr))
      goto dun;
  }
dun:
  ILU_HANDLED(lerr);
  StdFinish(disc);
  StdFinish(interest);
  return;
}

/*Main Invariant holds; L2 otherwise unconstrained*/

static          ilu_boolean
TestCallback(counted_client * cc)
{
  ilu_Object      obj;
  ilu_Connection  newconn;
  ilu_boolean     ans;
  ILU_ERRS((bad_locks, broken_locks, internal)) lerr = ILU_INIT_NO_ERR;

  if (cc == NIL || (obj = cc->client) == NIL)
    return (ilu_FALSE);
  ans = ilu_PingObject(obj, &newconn);
  ILU_NOTE(GC_DEBUG,
	   ("ILU:  pinging gcCallback obj <%s/%s> => %s\n",
	    server_id(object_server(obj)), object_ih(obj),
	    ans ? "Present" : "Gone"));
  if (newconn != NIL)
    _ilu_HandOffNewConnection(newconn, &lerr);
  ILU_MUST_BE_SUCCESS(lerr);
  return ans;
}

/*L1 = {gcmu};
  for all conn: (L2 >= {conn.iomu}) => (L2 >= {conn.callmu})*/
static void gccInvoke (ilu_Alarmette a)
{
  counted_client *cc = (counted_client *) a;
  register ilu_cardinal k, m;
  ilu_Object *q;
  ilu_boolean ok;
  ilu_Error lerr;

  _ilu_Assert(cc->nObjs>0 && cc->sweeping==ilu_FALSE && cc->client!=NIL,
	      "SweepGCCallbacks: cc malformed");
  cc->sweeping = ilu_TRUE;
  _ilu_ReleaseMutex(ilu_gcmu);
  ok = TestCallback(cc);
  _ilu_AcquireMutex(ilu_gcmu);
  cc->sweeping = ilu_FALSE;
  if (ok) {
      ilu_MXASet(&gccr, &cc->gcc,
		 ilu_FineTime_Add(ilu_FineTime_Now(), cc->period));
    }
  else {
      ilu_Server cs = object_server(cc->client);
      _ilu_AcquireMutex(ilu_cmu);
      q = (ilu_Object *) _ilu_vector_elements(Objects);
      m = _ilu_vector_size(Objects);
      for ( k = 0;  k < m;  k++) {
	  ilu_Object obj = q[k];
	  _ilu_AcquireServerMutex(object_server(obj));
	  lerr = DropGCInterest (obj, cc->client, ilu_FALSE);
	  ILU_MUST_BE_SUCCESS(lerr);
	}
      ilu_MXAClear(&gccr, &cc->gcc);
      if (cc->nObjs == 0) {
          _ilu_AcquireServerMutex(cs);
          lerr = _ilu_DeltaHolds(cc->client, -1);
          ILU_MUST_BE_SUCCESS(lerr);
          _ilu_ReleaseServerMutex(cs);
          ilu_free(cc);
        }
      else
        { /* Some gcoInvoke is working on cc */ }
      _ilu_ReleaseMutex(ilu_cmu);
    }
  return;
}


/* ====================== Client Side Stuff ====================== */

/*L1, L2, Main unconstrained*/

static ilu_Object gcInterest = NIL;
/* The object representing this client program. */

void ilu_SetGcClient(ilu_Object i)
{
  _ilu_Assert(gcInterest==NIL, "SetGcClient: already set");
  gcInterest = i;
  return;
}

ilu_boolean ilu_IsGcClientSet(void) {
  return (gcInterest != NIL);
}

/*Main Invariant holds; L2 otherwise unconstrained*/

static          ilu_boolean
RegWork(ilu_Object obj, ilu_Method which,
	ILU_ERRS((GcRegFailed, bad_locks,
		  broken_locks, internal)) * err)
{
  ilu_Call_s      call_s;
  ilu_Call        call = &call_s;
  ilu_Object      gci = gcInterest;
  ilu_ProtocolException pe;
  ilu_cardinal    errorStatus;
  ilu_cardinal    reqSize = 0;
  ilu_Server      os, is;
  ILU_ERRS((IoErrs, bad_locks, inv_objref,
	    no_resources)) lerr = ILU_INIT_NO_ERR, *lerp = NIL;
  ilu_boolean     started, good = ilu_TRUE;
  ilu_Connection  newconn;
  ILU_ERRS((bad_locks, broken_locks, internal)) hoerr = ILU_INIT_NO_ERR;

  if (obj == NIL)
    return ILU_ERR_CONS2(GcRegFailed, err, why, "RegWork given NIL obj",
			 sub, NIL, ilu_FALSE);
  if (gci == NIL)
    return ILU_ERR_CONS2(GcRegFailed, err,
			 why, "no GC callback object is registered",
			 sub, NIL, ilu_FALSE);
  ILU_NOTE(GC_DEBUG,
	   ("ILU:  calling %s on <%s/%s>\n",
	    which->me_name, server_id(object_server(obj)),
	    object_ih(obj)));
  started = ilu_StartCall(call, obj->ob_server, _ilu_rootClass, which, 0,
			  NIL, &newconn, &lerr);
  if (newconn != NIL)
    _ilu_HandOffNewConnection(newconn, &hoerr);
  if (!started) {
    ILU_HANDLED(hoerr);
    if (lerp = ilu_malloc(sizeof(*lerp)))
      *lerp = lerr;
    else
      ILU_HANDLED(lerr);
    return ILU_ERR_CONS2(GcRegFailed, err, why, "BeginCall failed",
			 sub, lerp, ilu_FALSE);
  }
  os = object_server(obj);
  is = object_server(gci);

 retry:

  _ilu_AcquireServerMutex(os);
  reqSize += ilu_SizeOfObjectID(call, obj, ilu_TRUE, _ilu_rootClass, &lerr);
  _ilu_ReleaseServerMutex(os);
  if (ILU_ERRNOK(lerr))
    goto faild;
  _ilu_AcquireServerMutex(is);
  reqSize += ilu_SizeOfObjectID(call, gci, ilu_FALSE, _ilu_GcCallbackClass,
				&lerr);
  _ilu_ReleaseServerMutex(is);
  if (ILU_ERRNOK(lerr))
    goto faild;
  if (!ilu_StartRequest(call, reqSize, &lerr))
    goto faild;
  ilu_EnterServer(os, object_class(obj));
  ilu_OutputObjectID(call, obj, ilu_TRUE, _ilu_rootClass, &lerr);
  if (ILU_ERRNOK(lerr))
    goto faild;
  ilu_EnterServer(is, object_class(gci));
  ilu_OutputObjectID(call, gci, ilu_FALSE, _ilu_GcCallbackClass, &lerr);
  if (ILU_ERRNOK(lerr))
    goto faild;
  if (!ilu_FinishRequest(call, &lerr))
    goto faild;
  pe = ilu_GetReply(call, &errorStatus, &newconn, &lerr);
  if (newconn != NIL) {
    (void) _ilu_HandOffNewConnection(newconn, &hoerr);
  }
  if (ILU_ERRNOK(lerr) &&
      (lerr.ilu_type == ILU_ERRTYP(transient)) &&
      (ILU_ERRSEL(transient,lerr).minor == ilu_tm_retry)) {
    ILU_HANDLED(lerr);
    ILU_CLER(lerr);
    goto retry;
  };
  if (pe != ilu_ProtocolException_Success)
    goto faild;
  else if (errorStatus != 0) {
    good = ilu_FALSE;
    goto faild;
  }
  ilu_ReplyRead(call, &lerr);
  if (ILU_ERRNOK(lerr))
    goto faild;
faild:
  ilu_FinishCall(call, &lerr);
  if (ILU_ERRNOK(lerr) || !good) {
    ILU_HANDLED(hoerr);
    if (ILU_ERRNOK(lerr)) {
      if (lerp = ilu_malloc(sizeof(*lerp)))
	*lerp = lerr;
      else
	ILU_HANDLED(lerr);
    }
    return ILU_ERR_CONS2(GcRegFailed, err,
			 why, "error in GC (un)reg client stub",
			 sub, lerp, ilu_FALSE);
  }
  *err = hoerr;
  return ILU_ERROK(*err);
}

ilu_boolean
_ilu_RegisterGCInterest(ilu_Object obj,
			ILU_ERRS((GcRegFailed, bad_locks,
				  broken_locks, internal)) * err)
{
  if (!ilu_Check(!server_is_true(object_server(obj)), err))
    return ilu_FALSE;
  return RegWork(obj, _ilu_RegisterGCInterestMethod, err);
}

ilu_boolean
_ilu_UnregisterGCInterest(ilu_Object obj,
			  ILU_ERRS((GcRegFailed, bad_locks,
				    broken_locks, internal)) * err)
{
  if (!ilu_Check(!server_is_true(object_server(obj)), err))
    return ilu_FALSE;
  return RegWork(obj, _ilu_UnregisterGCInterestMethod, err);
}
