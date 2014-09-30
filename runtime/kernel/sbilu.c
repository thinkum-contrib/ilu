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
/* $Id: sbilu.c,v 1.39 1999/08/12 06:44:05 janssen Exp $ */
/* Last edited by Mike Spreitzer September 22, 1998 11:13 pm PDT */

/* Simple binding service, take 3.  (ILU version 2.0)
 */

#include <stdio.h>
#include <iluntrnl.h>
#include <object.h>
#include <server.h>

#define SERVER_MSTID "ilu:ilu-simple-binding-version-1"
#define SUNRPC_VERNO 0xa08b4d30
#define SUNRPC_PNUM  0x00061a79

/* simple-binding-service kernel object */
static ilu_Object _sb = NIL;  

static ilu_Exception AlreadyPublished;
static ilu_Exception BadProof;
static ilu_Exception BadSBH;
static ilu_Exception MallocFailure;
static ilu_Exception NoTable;
static ilu_Exception NotPublished;

static int langIndex = 0;

static ilu_Method publishMethod;
static ilu_Method withdrawMethod;
static ilu_Method lookupMethod;
static ilu_Method enumerateMethod;

static ilu_Class serverType = ILU_NIL;

#include <stdlib.h>
#include <string.h>
#include "iluxport.h"

/* Locate the binding realm, host, and port for this address space */

ilu_boolean ilu_GetSBServiceParms (char *realm_name, char *host, ilu_shortcardinal *port)
{
  char *envval = getenv("ILU_BINDING_SERVICE");
  if (envval == ILU_NIL)
    {
      strcpy (realm_name, ILU_BINDING_REALM);
      strcpy (host, ILU_BINDING_HOST);
      *port = ILU_BINDING_PORT;
      return ilu_TRUE;
    }
  else
    {
      char tempbuf[20];
      unsigned long temp = 0;

      if ((sscanf(envval, "%1023[^:]:%1023[^:]:%19[0-9a-fA-Fx]",
		 realm_name, host, tempbuf) != 3) ||
	  ((strlen(tempbuf) > 0) &&
	   ((temp = strtoul(tempbuf, ILU_NIL, 0)) == 0 ||
	    temp > 0xFFFF)))
	{
	  ILU_NOTE(BINDING_DEBUG,
		("ilu_GetSBServiceParms:  can't parse value of ILU_BINDING_SERVICE:  \"%s\"\n",
		 envval));
	  return ilu_FALSE;
	}
      if (strlen(realm_name) < 1)
	strcpy (realm_name, ILU_BINDING_REALM);
      if (strlen(host) < 1)
	strcpy (host, ILU_BINDING_HOST);
      if (temp == 0)
	*port = ILU_BINDING_PORT;
      else
	*port = temp;
      return ilu_TRUE;
    }
}

ilu_boolean ilu_GetSimpleBindingSBH (char *buffer, ilu_cardinal bufferlen)
{
  char realm[1024];
  char hostname[1024];
  ilu_shortcardinal port;
  ilu_ProtocolInfo default_pinfo;
  ilu_TransportInfo default_tinfo;
  char tcpinfo[1024];
  char *tinfo[10];
  char tinfo_buffer[2048];
  static const char sbhform[] = "ilusbh:ILUSimpleBindingService.%s/Server;ilu%%3A%s;%s@%s";
  ilu_Error lerr;
  int i;

  if (!ilu_GetSBServiceParms(realm, hostname, &port))
    return ilu_FALSE;
  sprintf (tcpinfo, "tcp_%s_%u", hostname, port);
  default_pinfo = ilu_DefaultProtocolInfo();
  default_tinfo = ilu_DefaultTransportInfo();
  for (i = 0;  i < 10;  i++) {
    tinfo[i] = default_tinfo[i];
    if (default_tinfo[i] == NULL)
      break;
    else if (strncmp(default_tinfo[i], "tcp_", 4) == 0) {
      tinfo[i] = tcpinfo;
    }
  }
  if (i == 10)
    return ilu_FALSE;
  _ilu_StringifyTinfoToBuffer (tinfo, tinfo_buffer, sizeof(tinfo_buffer), &lerr);
  if (ILU_ERRNOK(lerr)) {
    ILU_HANDLED(lerr);
    return ilu_FALSE;
  };
  if ((strlen(realm) + strlen(SERVER_MSTID + 4) + strlen(default_pinfo) + strlen(tinfo_buffer) + strlen(sbhform)) > bufferlen)
    return ilu_FALSE;
  sprintf (buffer, sbhform, realm, SERVER_MSTID + 4, default_pinfo, tinfo_buffer);
  return ilu_TRUE;
}

/*
 *  Uses configuration info to find the object which provides simple binding service;
 *  returns kernel object or NIL on error.
 */

/* L1 = {};
  forall conn: (L2 >= {conn.iomu}) => (L2 >= {conn.callmu});
  Server may be entered and exited within this procedure.
  */
static ILU_PASS(ILU_OPTIONAL(ilu_Object))
     FindSBServer(ilu_Error *err)
{
  ilu_Object kobj = NIL;
  char sbh[3000];
  ilu_Class pclass = ilu_FindClassFromName("ilu-simpbind.Server");

  int *fake_lspo;

  /* create string binding handle */
  if (!ilu_GetSimpleBindingSBH(sbh, sizeof(sbh)))
    return ILU_NIL;
  kobj =  ilu_ObjectOfSBH(sbh, pclass, err);
  if (kobj != NIL)
    {
      fake_lspo = (int *) ilu_must_malloc(sizeof(int));
      *fake_lspo = 1;
      langIndex = ilu_RegisterLanguage("internal");
      ilu_RegisterLanguageSpecificObject(kobj, (ilu_refany) fake_lspo, langIndex);
      ilu_ExitServer (ilu_ServerOfObject(kobj), pclass);
      ILU_NOTE(BINDING_DEBUG,
	       ("ILU (sbilu.c:FindSBServer):  Binding server is %s\n", sbh));
    }
  return(kobj);
}


static void
  InitializeSimpleBinding2 (ilu_Error *err)
{
  ilu_Class cl;

  if (serverType != ILU_NIL)
    { ILU_CLER(*err); return; }

  if (!ilu_EnterMutex(ilu_otmu, err))
    goto fail1;
  BadSBH = ilu_DefineException("ilu-simpbind", "BadSBH", NIL, err);
  if (ILU_ERRNOK(*err))
    goto fail1;
  AlreadyPublished = ilu_DefineException("ilu-simpbind", "AlreadyPublished", NIL, err);
  if (ILU_ERRNOK(*err))
    goto fail1;
  MallocFailure = ilu_DefineException("ilu-simpbind", "MallocFailure", NIL, err);
  if (ILU_ERRNOK(*err))
    goto fail1;
  NoTable = ilu_DefineException("ilu-simpbind", "NoTable", NIL, err);
  if (ILU_ERRNOK(*err))
    goto fail1;
  NotPublished = ilu_DefineException("ilu-simpbind", "NotPublished", NIL, err);
  if (ILU_ERRNOK(*err))
    goto fail1;
  BadProof = ilu_DefineException("ilu-simpbind", "BadProof", NIL, err);
  if (ILU_ERRNOK(*err))
    goto fail1;
  { ilu_string supers[] = { NULL};
    cl = ilu_DefineObjectType("ilu-simpbind.Server",	/*name*/
			      "",	/*brand*/
			      SERVER_MSTID,
			      NULL,	/*singleton*/
			      ilu_FALSE,	/* optional */
			      ilu_FALSE,	/* collectible */
			      NULL,	/*auth*/
			      4,	/*n methods*/
			      0,	/*n supers*/
			      supers,
#ifdef ILU_HTTPNG_OBJECTS
			      0, ilu_FALSE, ilu_FALSE,
#endif
			      err);
    if (ILU_ERRNOK(*err))
      goto fail2;
    serverType = cl;
  }
  { ilu_Exception	exns[3];
    exns[0] = BadSBH;
    exns[1] = AlreadyPublished;
    exns[2] = MallocFailure;
    publishMethod = ilu_DefineMethod(cl, 0,
				     "Publish",	/*name*/
				     1,	/*id*/
				     0,	/*functional*/
				     0,	/*asynch*/
				     3,	/*n exns*/
				     exns,
				     1, (char *) ilu_TypeID_ilu_CString,
				     err);
    if (ILU_ERRNOK(*err))
      goto fail2;
    (void) ilu_DefineMethodArg (publishMethod, 0,
				"sbh", ilu_FALSE, ilu_In,
				ILU_TYPEID_CONST_ilu_CString, err);
    if (ILU_ERRNOK(*err))
      goto fail2;
  }
  { ilu_Exception	exns[4];
    exns[0] = NoTable;
    exns[1] = NotPublished;
    exns[2] = BadProof;
    exns[3] = BadSBH;
    withdrawMethod = ilu_DefineMethod(cl, 1,
				      "Withdraw",	/*name*/
				      2,	/*id*/
				      0,	/*functional*/
				      0,	/*asynch*/
				      4,	/*n exns*/
				      exns,
				      2, (char *) ilu_TypeID_ilu_boolean,
				      err);
    if (ILU_ERRNOK(*err))
      goto fail2;
    (void) ilu_DefineMethodArg (withdrawMethod, 0,
				"sbh", ilu_FALSE, ilu_In,
				ILU_TYPEID_CONST_ilu_CString, err);
    if (ILU_ERRNOK(*err))
      goto fail2;
    (void) ilu_DefineMethodArg (withdrawMethod, 1,
				"cookie", ilu_FALSE, ilu_In,
				ILU_TYPEID_CONST_ilu_CString, err);
    if (ILU_ERRNOK(*err))
      goto fail2;
  }
  { ilu_Exception	exns[3];
    exns[0] = NoTable;
    exns[1] = NotPublished;
    exns[2] = MallocFailure;
    lookupMethod = ilu_DefineMethod(cl, 2,
				    "Lookup",	/*name*/
				    3,	/*id*/
				    0,	/*functional*/
				    0,	/*asynch*/
				    3,	/*n exns*/
				    exns,
				    2, (char *) ilu_TypeID_ilu_CString,
				    err);
    if (ILU_ERRNOK(*err))
      goto fail2;
    (void) ilu_DefineMethodArg (lookupMethod, 0,
				"sid", ilu_FALSE, ilu_In,
				ILU_TYPEID_CONST_ilu_CString, err);
    if (ILU_ERRNOK(*err))
      goto fail2;
    (void) ilu_DefineMethodArg (lookupMethod, 1,
				"ih", ilu_FALSE, ilu_In,
				ILU_TYPEID_CONST_ilu_CString, err);
    if (ILU_ERRNOK(*err))
      goto fail2;
  }
  { ilu_Exception	*exns = NULL;
    enumerateMethod = ilu_DefineMethod(cl, 3,
				       "Enumerate",	/*name*/
				       4,	/*id*/
				       0,	/*functional*/
				       0,	/*asynch*/
				       0,	/*n exns*/
				       exns,
				       1, "IDL:ilu.parc.xerox.com/ilu_simpbind/StringBindingHandleList:1.0",
				       err);
    if (ILU_ERRNOK(*err))
      goto fail2;
    (void) ilu_DefineMethodArg (enumerateMethod, 0,
				"pattern", ilu_FALSE, ilu_In,
				ILU_TYPEID_CONST_ilu_CString, err);
    if (ILU_ERRNOK(*err))
      goto fail2;
  }
  if (!ilu_ExitMutex(ilu_otmu, ilu_TRUE, err))
    return;

  _sb = FindSBServer(err); /* err. if nil */
  if (_sb == NIL)
    {
      ILU_NOTE(BINDING_DEBUG,
	    ("%s:  Could not find simple binding service.\n",
	     "ilu_InitializeSimpleBinding"));
      serverType = ILU_NIL;
    }
  else
    ILU_CLER(*err);
  return;

 fail2:
  serverType = ILU_NIL;
 fail1:
  ilu_ExitMutex(ilu_otmu, ilu_TRUE, err);
}

/* (kobj == NIL)=>L1 = {}, Main Remnant holds
   (kobj != NIL)=>Inside(object_server(kobj), object_class(kobj)) */
static ilu_boolean
  InitializeSimpleBinding (ilu_Object kobj)
{
  static ilu_boolean initialized = ilu_FALSE;
  ilu_Error err;

  if (initialized)
    return ilu_TRUE;
    
  if (kobj == NIL)
    {
      InitializeSimpleBinding2(&err);
      if (ILU_ERRNOK(err))
	{
	  ILU_NOTE(BINDING_DEBUG,
		("InitializeSimpleBinding:  error %s initializing simple binding\n",
		 ILU_ERR_NAME(err)));
	  ILU_HANDLED(err);
	  return ilu_FALSE;
	}
    }
  else
    {
      ilu_Server s = object_server(kobj);
      ilu_Class cl = object_class(kobj);
      ilu_string ih = ilu_StrdupE(object_ih(kobj), &err);
      ilu_Object kobj2;

      if (ILU_ERRNOK(err))
	{
	  ILU_NOTE(BINDING_DEBUG,
		("InitializeSimpleBinding:  error %s copying instance handle of object instance %p.\n",
		 ILU_ERR_NAME(err), kobj));
	  ILU_HANDLED(err);
	  return ilu_FALSE;
	}
      ilu_ExitServer(s, cl);
      InitializeSimpleBinding2(&err);
      if (ILU_ERRNOK(err))
	{
	  ILU_NOTE(BINDING_DEBUG,
		("InitializeSimpleBinding:  error %s initializing simple binding\n",
		 ILU_ERR_NAME(err)));
	  ILU_HANDLED(err);
	  ilu_free(ih);
	  return ilu_FALSE;
	}
      ilu_EnterServer(s, cl);
      kobj2 = _ilu_FindObjectInServer(ih, s);
      ilu_free(ih);
      if (kobj2 != kobj)
	{
	  ILU_NOTE(BINDING_DEBUG,
		("InitializeSimpleBinding:  object changed while binding simple binding server!\n"));
	  return ilu_FALSE;
	}
    }
  initialized = ilu_TRUE;
  return ilu_TRUE;
}

/* Begin stub code */

/*
 * Replaces use of _simpbind_SimpleBinding_Publish and C runtime.
 * Assumes _kobj will not be changed out from under it.
 */
static          ilu_string
_ilu_Publish_stub(ilu_Object kobj, ilu_string sbh, ilu_Error *err)
{
  ilu_Connection newconn = ILU_NIL;
  ilu_Server s;
  ilu_Call_s call[1];
  ilu_cardinal reqSize = 0;
  ilu_ProtocolException perror;
  ilu_cardinal status_code;
  ilu_string retvalue = ILU_NIL;

  /* assumes kobj doesn't change */

  if (kobj == NIL) {
    ILU_NOTE(BINDING_DEBUG,
	  ("_ilu_Publish_stub: nil simple binding service object\n"));
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_nil, ILU_NIL);
  }
  if ((s = ilu_ServerOfObject(kobj)) == NIL) {
    ILU_NOTE(BINDING_DEBUG,
	  ("%s: nil server for simple binding service object\n",
	   "_ilu_Publish_stub"));
    return ILU_ERR_CONS1(internal, err, minor, ilu_im_broken, ILU_NIL);
  }
  if (!ilu_StartCall(call, s, serverType, publishMethod, langIndex,
		     ILU_NIL, &newconn, err))
    {
      ILU_NOTE(BINDING_DEBUG,
	    ("_ilu_Publish_stub: ilu_StartCall failed with err <%s>\n",
	     ILU_ERR_NAME(*err)));
      return NIL;
    }
  if (newconn != NIL) {
    _ilu_HandOffNewConnection(newconn, err);
    newconn = NIL;
  };
  if (ILU_ERRNOK(*err)) goto faild;
  ilu_AcquireMutex(s->sr_lock);
  reqSize += ilu_SizeOfObjectID(call, kobj, ilu_TRUE,
			       ilu_ClassOfObject(kobj), err);
  ilu_ReleaseMutex(s->sr_lock);
  if (ILU_ERRNOK(*err))
    goto faild;
  reqSize += ilu_SizeOfString(call, sbh, _ilu_SafeStrlen(sbh), 0, err);
  if (ILU_ERRNOK(*err))
    goto faild;

  if (!ilu_StartRequest(call, reqSize, err)) {
    ILU_NOTE(BINDING_DEBUG,
	  ("_ilu_Publish_stub: ilu_StartRequest failed with err <%s>\n",
	   ILU_ERR_NAME(*err)));
    goto faild;
  }
  ilu_EnterServer(s, serverType);
  ilu_OutputObjectID(call, kobj, ilu_TRUE, serverType, err);
  if (ILU_ERRNOK(*err))
    goto faild;
  ilu_OutputString(call, sbh, _ilu_SafeStrlen(sbh), 0, err);
  if (ILU_ERRNOK(*err))
    goto faild;

  if (!ilu_FinishRequest(call, err)) {
    ILU_NOTE(BINDING_DEBUG,
	  ("_ilu_Publish_stub: ilu_FinishRequest failed with err <%s>\n",
	   ILU_ERR_NAME(*err)));
    goto faild;
  }
  perror = ilu_GetReply(call, &status_code, &newconn, err);
  if (newconn != NIL) {
    ILU_ERRS((bad_locks, broken_locks, internal)) herr;
    _ilu_HandOffNewConnection(newconn, &herr);
    ILU_MUST_BE_SUCCESS(herr);
  }
  if (perror == ilu_ProtocolException_Success)
    {
      ilu_cardinal len;

      if (ILU_ERRNOK(*err)) goto faild;
      if (status_code == 0) {
	retvalue = NIL;
	ilu_InputString(call, &retvalue, &len, 0, err);
	if (ILU_ERRNOK(*err))
	  goto faild;
      } else { /* status_code != 0 => exception was signalled */
	if (status_code > publishMethod->me_exceptionCount) {
	  ILU_NOTE(BINDING_DEBUG,
		("_ilu_Publish_stub:  unexpected error signalled by server, status_code value of %lu\n",
		 status_code));
	} else
	  ILU_NOTE(BINDING_DEBUG,
		("_ilu_Publish_stub:  server signalled error %s\n",
		 (char *) publishMethod->me_exceptionVector[status_code - 1]));
      }
      ilu_ReplyRead(call, err);
    } else {
      ILU_NOTE(BINDING_DEBUG,
	    ("_ilu_Publish_stub:  protocol error %s on GetReply\n", ilu_PEName(perror)));
      goto faild;
  }

 faild:

  ilu_FinishCall(call, err);
  return (retvalue);
}


static ilu_boolean 
  _ilu_Withdraw_stub( ilu_Object kobj, ilu_string sbh, ilu_string cookie, ilu_Error *err)
{
  ilu_Connection newconn = ILU_NIL;
  ilu_Server s;
  ilu_Call_s call[1];
  ilu_cardinal reqSize = 0;
  ilu_ProtocolException perror;
  ilu_cardinal status_code;
  ilu_boolean retvalue = ilu_FALSE;

  /* assumes kobj doesn't change */

  if (kobj == NIL) {
    ILU_NOTE(BINDING_DEBUG,
	  ("_ilu_Withdraw_stub: nil simple binding service object\n"));
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_nil, ilu_FALSE);
  }
  if ((s = ilu_ServerOfObject(kobj)) == NIL) {
    ILU_NOTE(BINDING_DEBUG,
	  ("%s: nil server for simple binding service object\n",
	   "_ilu_Withdraw_stub"));
    return ILU_ERR_CONS1(internal, err, minor, ilu_im_broken, ilu_FALSE);
  }
  if (!ilu_StartCall(call, s, serverType, withdrawMethod, langIndex,
		     ILU_NIL, &newconn, err))
    {
      ILU_NOTE(BINDING_DEBUG,
	    ("_ilu_Withdraw_stub: ilu_StartCall failed with err <%s>\n",
	     ILU_ERR_NAME(*err)));
      return ilu_FALSE;
    }
  if (newconn != NIL)
    _ilu_HandOffNewConnection(newconn, err);
  if (ILU_ERRNOK(*err)) goto faild;
  ilu_AcquireMutex(s->sr_lock);
  reqSize += ilu_SizeOfObjectID(call, kobj, ilu_TRUE,
			       ilu_ClassOfObject(kobj), err);
  ilu_ReleaseMutex(s->sr_lock);
  if (ILU_ERRNOK(*err))
    goto faild;
  reqSize += ilu_SizeOfString(call, sbh, _ilu_SafeStrlen(sbh), 0, err);
  if (ILU_ERRNOK(*err))
    goto faild;
  reqSize += ilu_SizeOfString(call, cookie, _ilu_SafeStrlen(cookie), 0, err);
  if (ILU_ERRNOK(*err))
    goto faild;

  if (!ilu_StartRequest(call, reqSize, err)) {
    ILU_NOTE(BINDING_DEBUG,
	  ("_ilu_Withdraw_stub: ilu_StartRequest failed with err <%s>\n",
	   ILU_ERR_NAME(*err)));
    goto faild;
  }
  ilu_EnterServer(s, serverType);
  ilu_OutputObjectID(call, kobj, ilu_TRUE, serverType, err);
  if (ILU_ERRNOK(*err))
    goto faild;
  ilu_OutputString(call, sbh, _ilu_SafeStrlen(sbh), 0, err);
  if (ILU_ERRNOK(*err))
    goto faild;
  ilu_OutputString(call, cookie, _ilu_SafeStrlen(cookie), 0, err);
  if (ILU_ERRNOK(*err))
    goto faild;

  if (!ilu_FinishRequest(call, err)) {
    ILU_NOTE(BINDING_DEBUG,
	  ("_ilu_Withdraw_stub: ilu_FinishRequest failed with err <%s>\n",
	   ILU_ERR_NAME(*err)));
    goto faild;
  }
  if ((perror = ilu_GetReply(call, &status_code, &newconn, err)) == ilu_ProtocolException_Success)
    {
      if (newconn != NIL)
	_ilu_HandOffNewConnection(newconn, err);
      if (ILU_ERRNOK(*err)) goto faild;
      if (status_code == 0) {
	ilu_InputBoolean(call, &retvalue, err);
	if (ILU_ERRNOK(*err))
	  goto faild;
      } else { /* status_code != 0 => exception was signalled */
	if (status_code > withdrawMethod->me_exceptionCount) {
	  ILU_NOTE(BINDING_DEBUG,
		("_ilu_Withdraw_stub:  unexpected error signalled by server, status_code value of %lu\n",
		 status_code));
	} else
	  ILU_NOTE(BINDING_DEBUG,
		("_ilu_Withdraw_stub:  server signalled error %s\n",
		 (char *) withdrawMethod->me_exceptionVector[status_code - 1]));
      }
      ilu_ReplyRead(call, err);
    } else {
      ILU_NOTE(BINDING_DEBUG,
	    ("_ilu_Withdraw_stub:  protocol error %s on GetReply\n", ilu_PEName(perror)));
      goto faild;
  }

 faild:

  ilu_FinishCall(call, err);
  return (retvalue);
}


/*
 * Replaces use of _simpbind_SimpleBinding_Lookup and C runtime.
 * Assumes kobj will not be changed out from under it.
 */
static ilu_string _ilu_Lookup_stub(ilu_Object kobj, ilu_string sid, ilu_string ih, ilu_Error *err)
{
  ilu_string retvalue = ILU_NIL;
  ilu_Call_s call[1];
  ilu_ProtocolException perror;
  ilu_cardinal scode;
  ilu_cardinal reqSize;
  ilu_Server s;
  ilu_Connection newconn;

  /* assumes kobj doesn't change */
  
  if (kobj == NIL) {
    ILU_NOTE(BINDING_DEBUG,
	  ("_ilu_Lookup_stub: nil simple binding service object\n"));
    return ILU_ERR_CONS1(internal, err, minor, ilu_im_broken, ILU_NIL);
  }
  
  if ((s = object_server(kobj)) == ILU_NIL) {
    ILU_NOTE(BINDING_DEBUG,
	  ("%s: nil server for simple binding service object\n",
	   "_ilu_Lookup_stub"));
    return ILU_ERR_CONS1(internal, err, minor, ilu_im_broken, ILU_NIL);
  }
  
  if (!ilu_StartCall (call, s, serverType, lookupMethod, langIndex, ILU_NIL, &newconn, err))
    return ILU_NIL;

  if (newconn != NIL)
    _ilu_HandOffNewConnection(newconn, err);
  if (ILU_ERRNOK(*err)) goto faild;
  ilu_AcquireMutex(s->sr_lock);
  reqSize = ilu_SizeOfObjectID(call,kobj,ilu_TRUE,ilu_ClassOfObject(kobj), err);
  ilu_ReleaseMutex(s->sr_lock);
  if (ILU_ERRNOK(*err))
    goto faild;
  reqSize += ilu_SizeOfString(call, sid, _ilu_SafeStrlen(sid), 0, err);
  if (ILU_ERRNOK(*err))
    goto faild;
  reqSize += ilu_SizeOfString(call, ih, _ilu_SafeStrlen(ih), 0, err);
  if (ILU_ERRNOK(*err))
    goto faild;
  if (! ilu_StartRequest(call, reqSize, err))
    goto faild;
  ilu_EnterServer(s, serverType);
  ilu_OutputObjectID(call, kobj, ilu_TRUE, serverType, err);
  if (ILU_ERRNOK(*err))
    goto faild;
  ilu_OutputString(call, sid, _ilu_SafeStrlen(sid), 0, err);
  if (ILU_ERRNOK(*err))
    goto faild;
  ilu_OutputString(call, ih, _ilu_SafeStrlen(ih), 0, err);
  if (ILU_ERRNOK(*err))
    goto faild;
  if (! ilu_FinishRequest(call, err))
    goto faild;
  if ((perror = ilu_GetReply(call, &scode, &newconn, err)) == ilu_ProtocolException_Success)
    {
      ilu_cardinal len;
      if (newconn != NIL)
	_ilu_HandOffNewConnection(newconn, err);
      if (ILU_ERRNOK(*err)) goto faild;
      if (scode == 0)
	{
	  ilu_InputString (call, &retvalue, &len, 0, err);
	  if (ILU_ERRNOK(*err))
	    goto faild;
	}
      else if (scode > lookupMethod->me_exceptionCount)
	{
	  ILU_NOTE(BINDING_DEBUG,
		("_ilu_Lookup_stub:  unexpected error signalled by server, scode value of %lu\n",
		 scode));
	}
      else
	ILU_NOTE(BINDING_DEBUG,
	      ("_ilu_Lookup_stub:  server signalled error %s\n",
	       (char *) lookupMethod->me_exceptionVector[scode - 1]));
      ilu_ReplyRead(call, err);
    } else {
      ILU_NOTE(BINDING_DEBUG,
	    ("_ilu_Lookup_stub:  protocol error %s on GetReply\n", ilu_PEName(perror)));
      goto faild;
    }

 faild:

  ilu_FinishCall(call, err);
  return (retvalue);
}

/* End stub code */
/* Here are the usable functions. */


/*before: Inside(s, cl)
  after:				 L1 disjoint {cmu, s};
  after: cl collectible		      => L1  not >=  {gcmu};
  after: cl collectible & s surrogate => Main Invariant holds;
  where s = obj's server and cl = obj's type.
  (We don't really need to hold cmu for surrogate or non-collectible
   objects, but this is convenient because ilu_Enter/ExitServer can
   be used.)*/
/* To make the stub call, cmu is released and reacquired.
   (Out of order.  Exit obj's server after getting the sbh, instead?)
 */
ILU_PASS(ILU_OPTIONAL(char *)) ilu_PublishObject (ilu_Object obj)
{
/*  ILU_C_ENVIRONMENT       env; */
  char                   *proof;
  char                   *sbh;
  ilu_Server		 server;
  ilu_Class		 type;
  ilu_Error err;

  if (!InitializeSimpleBinding(obj))
    return ILU_NIL;

  /* Get sbh, mstid of object-to-be-published */
  if ((sbh = ilu_SBHOfObject(obj)) == NIL) {
    ILU_NOTE(BINDING_DEBUG | EXPORT_DEBUG,
	  ("ilu_PublishObject:  object %p not exported.\n",
	   obj));
    return (NIL);
  } else {
    server = object_server(obj);
    type = object_class(obj);
    ilu_DHolds(obj, +1);
    ilu_ExitServer(server, type);
    proof = _ilu_Publish_stub(_sb, sbh, &err);
    
    if (ILU_ERRNOK(err)) {
      ILU_NOTE(BINDING_DEBUG | EXPORT_DEBUG,
	    ("ilu_PublishObject:  Publish failed on object \"%s\", error %s\n",
	     sbh, ILU_ERR_NAME(err)));
      ILU_HANDLED(err);
    }
    ilu_EnterServer(server, type);
    ilu_DeltaHolds(obj, -1);
    ilu_ExitServer(server, type);
    return (proof);
  }
}

/*before: Inside(s, cl)
  after:				 L1 disjoint {cmu, s};
  after: cl collectible		      => L1  not >=  {gcmu};
  after: cl collectible & s surrogate => Main Invariant holds;
  where s = obj's server and cl = obj's type.
  (We don't really need to hold cmu for surrogate or non-collectible
   objects, but this is convenient because ilu_Enter/ExitServer can
   be used.)*/
ilu_boolean
ilu_WithdrawObject(ilu_Object obj, ILU_PASS(char *) ownership_proof)
{
  char                   *sbh;
  ilu_boolean             result = ilu_FALSE;
  ilu_Server		 server;
  ilu_Class		 type;
  ilu_Error		 err;

  /* Get oid of published obj */
  if ((sbh = ilu_SBHOfObject(obj)) == NIL) {
    ILU_NOTE(BINDING_DEBUG | EXPORT_DEBUG,
	  ("ilu_WithdrawObject:  object %p not exported.\n",
	   obj));
    return (ilu_FALSE);
  } else {
    server = object_server(obj);
    type = object_class(obj);
    ilu_DHolds(obj, +1);
    ilu_ExitServer(server, type);
    result = _ilu_Withdraw_stub(_sb, sbh, ownership_proof, &err);

    if (ILU_ERRNOK(err)) {
      ILU_NOTE(BINDING_DEBUG | EXPORT_DEBUG,
	    ("ilu_WithdrawObject:  Withdraw failed on object \"%s\", error %s\n",
	     sbh, ILU_ERR_NAME(err)));
      ILU_HANDLED(err);
      return ilu_FALSE;
    }
    ilu_free(ownership_proof);
    ilu_EnterServer(server, type);
    ilu_DeltaHolds(obj, -1);
    ilu_ExitServer(server, type);
    return result;
  }
}


/*before: L1 = {};
  after:  result!=NIL => Inside(result's server, pclass);
  after:  result==NIL => L1 = {};
  forall conn: (L2 >= {conn.iomu}) => (L2 >= {conn.callmu});
  Main otherwise unconstrained*/
ILU_OPTIONAL(ilu_Object) ilu_LookupObject(char *sid, char *ih,
				      ilu_Class pclass)
{
  ilu_Object      ans;
  ilu_boolean     change;
  change = ilu_ReLookupObject(sid, ih, pclass, &ans);
  return ans;
}

/*before: L1 = {};
  after:  *po != NIL => Inside(result's server, pclass);
  after:  *po == NIL => L1 = {};
  Main Remnant holds*/
ilu_boolean
ilu_ReLookupObject(char *sid, char *ih,
		   ilu_Class pclass,
		   ILU_OPTIONAL(ilu_Object) * po)
{
  char                   *sbh;
  ilu_Object            obj;
  ilu_ConsiderSbhResult csr;
  ilu_boolean           ans;
  ilu_Connection	newconn;
  ILU_ERRS((BadSBH, BadURL, BadContactInfo,
	    BadTransportInfo, BadProtocolInfo)) err;
  *po = NIL;

  if (!InitializeSimpleBinding(ILU_NIL))
    return ilu_FALSE;

  /* Get the sbh and mstid from the oid */
  sbh = _ilu_Lookup_stub(_sb, sid, ih, &err);
  if (sbh == NIL) {
    ILU_NOTE(BINDING_DEBUG | INCOMING_DEBUG,
	  ("ilu_LookupObject:  Lookup failed on oid %s/%s\n",
	   sid, ih));
    return (ilu_FALSE); }
  
  /* Unchanged from the previous simpbind.c ... */
  ILU_NOTE(INCOMING_DEBUG,
	("ilu_LookupObject:  found SBH=<%s>\n", sbh));
  
  csr = ilu_ConsiderSBH(sbh, &err);
  switch (csr) {
  case ilucsr_err:
    ILU_HANDLED(err);
  case ilucsr_noProblem:
  case ilucsr_isTrue:
  case ilucsr_noNews:
    ans = ilu_FALSE;
    break;
  case ilucsr_notReified:
  case ilucsr_changed:
    ans = ilu_TRUE;
    break;
  default:
    _ilu_Assert(0, "Lookup: unexpected csr");
  }

  if ((obj = ilu_ObjectOfSBH(sbh, pclass, &err)) == NIL) {
    ILU_NOTE(INCOMING_DEBUG,
	  ("ilu_LookupObject:  Bad object info (%s).\n", sbh));
    ilu_free(sbh);
    return (ilu_FALSE);
  }
  ilu_free(sbh);
  *po = obj;
  if (ilu_TrueInstanceP(obj)) {
    ILU_NOTE(INCOMING_DEBUG,
	  ("ilu_LookupObject:  Local object.\n"));
    return (ans);
  } else if (getenv("ILU_SB_LOOKUP_NO_PING")) {
    return ans;
  } else {
    ilu_boolean     status;
    ilu_Server      s = object_server(obj);
    ILU_ERRS((internal, GcRegFailed, bad_locks, broken_locks)) err;
    err = ilu_DeltaHolds(obj, 1);
    ILU_ERR_SWITCH(err) {
      ILU_SUCCESS_CASE {
	ilu_ExitServer(s, pclass);
	status = ilu_PingObject(obj, &newconn);
	ilu_EnterServer(s, pclass);
      }
      ILU_ERR_CASE(GcRegFailed, v)
	status = ilu_FALSE;
      ILU_ERR_CASE(internal, x) {
	ilu_ExitServer(s, pclass);
	*po = NIL;
	return (ans);
      }
    } ILU_ERR_ENDSWITCH;
    if (status) {
      ilu_DHolds(obj, -1);
      return (ans);
    } else {
      int             wasonly, nobj, nconn, nL = 0;
      /* wasonly: 1 hash pair, no lspo. */
      wasonly = ilu_hash_PairsInTable(s->sr_objs) == 1;
      if (object_lspos(obj) != NIL) /* check every language */
	for (nL = 0 ; wasonly && (nL < _ilu_NLanguages) ; nL++)
	  wasonly = wasonly && object_lspo(obj,nL) == NIL;
      ILU_NOTE(INCOMING_DEBUG,
	    ("ilu_LookupObject:  Bad ping of object %s %s\n",
	     server_id(object_server(obj)), object_ih(obj)));
      (void) ilu_DeltaHolds(obj, -1); /* XXX - BUG! should pass some errs */
      if (wasonly) {
	nobj = ilu_NumObjsInServer(s);
	nconn = ilu_NumIoingConnsOfServer(s);
	if (nobj == 0 && nconn == 0)
	  ilu_InnerBankServer(s);
      }
      ilu_ExitServer(s, pclass);
      *po = NIL;
      return (ans);
    }
  }
}


