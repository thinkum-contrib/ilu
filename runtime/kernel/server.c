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
/* $Id: server.c,v 1.105 1999/08/03 01:53:15 janssen Exp $ */
/* Last tweaked by Mike Spreitzer September 22, 1998 9:48 pm PDT */
/* Chris Jacobi, April 8, 1997 0:19 am PDT */

#define _POSIX_SOURCE

#include "iluntrnl.h"
#include "server.h"
#include "object.h"
#include "type.h"
#include "connect.h"
#include "port.h"
#include "ilutransport.h"

#define NO_LANG 86

static ilu_CharBuf zCharBuf = {0, 0, 0};

/*L1 >= {cmu}*/
/*L2 unconstrained*/

static          HashTable /* sid -> ilu_Server */ Servers = NIL;
/* When sr_closing, not in table. */

/*L1, L2 unconstrained*/

ilu_LanguageIndex _ilu_InternalLanguageIndex = 0;
ilu_LanguageIndex _ilu_NLanguages = 1;
ilu_string      _ilu_LangNames[ILU_MAX_ADDR_SPACE_LANGUAGES] = { "internal", 0 };

ilu_LanguageIndex ilu_RegisterLanguage(ilu_string name)
{
  ilu_LanguageIndex li;
  ILU_AUTOSETDEBUGLEVEL;
  for (li = 0; li < _ilu_NLanguages; li++) {
    if (strcmp(name, _ilu_LangNames[li]) == 0)
      return li;
  }
  ILU_NOTE(SERVER_DEBUG,
	("ilu_RegisterLanguage(%s) returns %u.\n",
	 name, (unsigned) _ilu_NLanguages));
  _ilu_Assert(_ilu_NLanguages < ILU_MAX_ADDR_SPACE_LANGUAGES, "RegisterLanguage");
  _ilu_LangNames[_ilu_NLanguages] = name;
  return (_ilu_NLanguages++);
}

ilu_string ilu_IDOfServer ( ilu_Server s )
{
  return (s->sr_id);
}

ilu_cardinal ilu_CRC32OfIDOfServer ( ilu_Server s )
{
  return (s->sr_crc32);
}

ilu_boolean ilu_TrueServerP ( ilu_Server s )
{
  return (s->sr_true);
}

ilu_boolean ilu_TrueServerForLanguageP ( ilu_Server s , ilu_LanguageIndex i)
{
  return (s->sr_true && (i == s->sr_true_language));
}

/*L1 >= {cmu}*/
static void BeStarted(void)
{
  if (Servers == NIL)
    Servers = ilu_hash_MakeNewTable(SERVER_HASHTABLESIZE,
				     NULLFN, NULLFN);
  return;
}

/*L1_sup < cmu*/

ilu_ConsiderSbhResult
ilu_ConsiderSBH(ilu_string sbh,
		ILU_ERRS((BadProtocolInfo,
			  no_memory, inv_objref, internal)) * err)
{
  ilu_Server      s;
  ilu_string      ih = NIL, sid = NIL, cinfo = NIL, pinfo = NIL;
  ilu_TransportInfo tinfo = NIL;
  ilu_TransportCreator tcr;
  ilu_Protocol    p;
  ilu_ConsiderSbhResult ans;
  ilu_cardinal    cinfolen;
  ilu_boolean	pass_cinfo = ilu_FALSE;

  if (!ilu_ParseSBH(sbh, &ih, &sid, NIL, &cinfo, &cinfolen, &pass_cinfo, err))
    return ilucsr_err;
  _ilu_AcquireMutex(ilu_cmu);
  BeStarted();
  s = (ilu_Server) ilu_hash_FindInTable(Servers, sid);
  if (s == NIL) {
    ans = ilucsr_notReified;
    ilu_ReleaseMutex(ilu_cmu);
  } else {
    _ilu_AcquireServerMutex(s);
    if (!ilu_Check(!s->sr_closing, err))
      ans = ilucsr_err;
    else if (!s->sr_cfails)
      ans = ilucsr_noProblem;
    else if (s->sr_true) {
      ans = ilucsr_isTrue;
    } else if (!_ilu_ParseConnectInfo(cinfo, cinfolen, &pinfo, &tinfo,
				      err)) {
      if (pass_cinfo)
	ilu_free(cinfo);
      ans = ilucsr_err;
    } else if ((pass_cinfo ? (ilu_free(cinfo), 0) : 0),
	       (tcr = _ilu_GetTransportCreator(tinfo, err)) == NIL) {
      ans = ilucsr_err;
    } else if ((p = _ilu_GetProtocolFromInfo(pinfo)) == NIL) {
      ans = ILU_ERR_CONS1(BadProtocolInfo, err, x, pinfo, ilucsr_err);
      pinfo = NIL;
    } else {
      if (_ilu_CompareTinfo(s->sr_tinfo, tinfo)
	  && strcmp(s->sr_pinfo, pinfo) == 0) {
	ans = ilucsr_noNews;
      } else {
	HashEnumerator  he;
	ilu_refany      key, data;
	_ilu_CloseTCR(s->sr_tcr);
	ilu_free(s->sr_tinfo);
	ilu_free(s->sr_pinfo);
	s->sr_tinfo = tinfo;
	tinfo = NIL;
	s->sr_tcr = tcr;
	s->sr_pinfo = pinfo;
	pinfo = NIL;
	s->sr_protocol = p;
	s->sr_cfails = ilu_FALSE;
	ilu_hash_BeginEnumeration(s->sr_objs, &he);
	while (ilu_hash_Next(&he, &key, &data)) {
	  ilu_Object      obj = (ilu_Object) data;
	  if (obj->ob_sbh != NIL) {
	    ilu_free(obj->ob_sbh);
	    obj->ob_sbh = NIL;
	  }
	}
	ans = ilucsr_changed;
      }
      *err = ILU_NO_ERR;
    }
    _ilu_ReleaseServerMutex(s);
    ilu_ReleaseMutex(ilu_cmu);
  }
  ilu_free(ih);
  ilu_free(sid);
  ilu_free(tinfo);
  ilu_free(pinfo);
  return ans;
}

ilu_Server
_ilu_FindAndEnterServer(ilu_string serverID, ilu_boolean add,
			ilu_string cinfo, ilu_cardinal cinfolen,
			ilu_Class st,
			ILU_ERRS((BadProtocolInfo, internal,
				  no_memory, inv_objref)) * err)
{
  ilu_Server      s;
  ilu_string      pinfo = NIL;
  ilu_TransportInfo tinfo = NIL;
  ilu_Protocol    p;
  ilu_TransportCreator tcr;
  int             i;
  ilu_boolean     coll = class_collectible(st), isnu = ilu_FALSE;
  if (coll)
    _ilu_AcquireMutex(ilu_gcmu);
  _ilu_AcquireMutex(ilu_cmu);
  BeStarted();
  s = (ilu_Server) ilu_hash_FindInTable(Servers, serverID);
  if (s != NIL || !add)
    ILU_CLER(*err);
  else {
    if (!_ilu_ParseConnectInfo(cinfo, cinfolen, &pinfo, &tinfo, err))
      goto abbort0;
    if (!(tcr = _ilu_GetTransportCreator(tinfo, err)))
      goto abbort1;
    if (!(p = _ilu_GetProtocolFromInfo(pinfo))) {
      ILU_ERR_CONS1(BadProtocolInfo, err, x, pinfo, (void) 6);
      pinfo = NIL;
      goto abbort2;
    }
    if (!(s = (ilu_Server) ilu_MallocE(sizeof(*s), err)))
      goto abbort2;
    if (!(s->sr_lock = _ilu_CreateMutex("server ", serverID))) {
      ILU_ERR_CONS1(no_memory, err, nbytes, 0, (void) 6);
      goto abbort3;
    }
    if (!(s->sr_id = ilu_StrdupE(serverID, err)))
      goto abbort4;
    s->sr_cinfo = ilu_CharBufFromChars(cinfo, cinfolen, err);
    if (ILU_ERRNOK(*err))
      goto abbort5;
    s->sr_extCinfo = zCharBuf;
    s->sr_objs = ilu_hash_MakeNewTable(OBJECT_HASHTABLESIZE,
				       NULLFN, NULLFN);
    if (!s->sr_objs) {
      ILU_ERR_CONS1(no_memory, err, nbytes, 0, (void) 6);
      goto abbort6;
    }
    s->sr_singles = ilu_hash_MakeNewTable(3, ilu_hash_HashPointer,
					  ilu_hash_PointerCompare);
    if (!s->sr_singles) {
      ILU_ERR_CONS1(no_memory, err, nbytes, 0, (void) 6);
      goto abbort7;
    }
    s->sr_true = ilu_FALSE;
    s->sr_crc32 = ilu_CRC32((ilu_bytes) serverID,
			    (ilu_cardinal) strlen(serverID));
    s->sr_true_language = NO_LANG;
    s->sr_tinfo = tinfo;
    s->sr_tcr = tcr;
    s->sr_pinfo = pinfo;
    s->sr_protocol = p;
    for (i = 0; i < ILU_MAX_ADDR_SPACE_LANGUAGES; i++)
      s->sr_lsss[i] = NIL;
    s->sr_holds = 0;
    s->sr_closing = ilu_FALSE;
    s->sr_cfails = ilu_FALSE;
    s->sr_connHead.next = s->sr_connHead.prev = NIL;
    s->sr_closedConns.next = s->sr_closedConns.prev = NIL;
    s->sr_ports.pl_next = s->sr_ports.pl_prev = NIL;
    s->sr_local_port = NIL;
    s->sr_closedPorts.pl_next = s->sr_closedPorts.pl_prev = NIL;
    s->sr_objtab = NIL;
    s->sr_default_port = NIL;
    if (!ilu_Check((int) ilu_hash_AddToTable(Servers, server_id(s), s),
		   err))
      goto abbort8;
#ifdef ENABLE_DEBUGGING
    ILU_NOTE(CONNECTION_DEBUG,
	     ("%s %p <%s> via pinfo=<%s>, tinfo=",
	      "_ilu_FindServer:  Created new server", s, serverID,
	      s->sr_pinfo));
    if ((ilu_DebugLevel & CONNECTION_DEBUG) != 0)
      _ilu_PrintTinfo(s->sr_tinfo);
    ILU_NOTE(CONNECTION_DEBUG,
	     ("\n"));
#endif				/* def ENABLE_DEBUGGING */
    isnu = ilu_TRUE;
    goto abbort0;
abbort8:
    ilu_hash_FreeHashTable(s->sr_singles, NULLFN, NULLFN);
abbort7:
    ilu_hash_FreeHashTable(s->sr_objs, NULLFN, NULLFN);
abbort6:
    ilu_CharBufFree(s->sr_cinfo);
abbort5:
    ilu_free(s->sr_id);
abbort4:
    ilu_free(s->sr_lock);
abbort3:
    ilu_free(s);
    s = NIL;
abbort2:
    (*tcr->tcr_close) (tcr);
abbort1:
    ilu_free(pinfo);
    ilu_free(tinfo);
abbort0:
    /* do nothing */;
  }
  if (s) {
    if (coll && !s->sr_true)
      _ilu_ReleaseMutex(ilu_gcmu);
    if (isnu)
      _ilu_AcquireMutex(server_lock(s));
    else
      _ilu_AcquireServerMutex(s);
  } else {
    if (coll)
      _ilu_ReleaseMutex(ilu_gcmu);
    _ilu_ReleaseMutex(ilu_cmu);
  }
  return (s);
}

ilu_Server
ilu_CreateTrueServer(ilu_string id, ilu_ObjectTable objtab,
		     ilu_LanguageIndex language,
		     ILU_ERRS((bad_locks, broken_locks, bad_param,
			       no_memory, internal)) * err)
{
  ilu_Server      ans = NIL;
  HashTable       objs = NIL, singles = NIL;
  ilu_Mutex       sm = NIL;
  int             i;
  ilu_CharBuf     cinfo = { NIL, 0, 0 };
  if (!id)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, NIL);
  if (!(cinfo.icb_base = ilu_MallocE(200, err)))
    return NIL;
  cinfo.icb_size = 200;
  cinfo.icb_base[0] = 0;
  if (!ilu_EnterMutex(ilu_gcmu, err))
    goto dun0;
  if (!ilu_EnterMutex(ilu_cmu, err))
    goto dun1;
  BeStarted();
  ans = (ilu_Server) ilu_hash_FindInTable(Servers, id);
  if (!ilu_Check(!(ans && ans->sr_closing), err))
    goto dun2;
  if (ans != NIL) {
    ILU_NOTE(SERVER_DEBUG,
	     ("ilu_CreateTrueServer:  given non-new id %s.\n", id));
    ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_serverId, (void) 6);
    goto dun2;
  }
  objs = ilu_hash_MakeNewTable(OBJECT_HASHTABLESIZE,
			       NULLFN, NULLFN);
  singles = ilu_hash_MakeNewTable(3, ilu_hash_HashPointer,
				  ilu_hash_PointerCompare);
  if (!objs || !singles) {
    ILU_ERR_CONS1(no_memory, err, nbytes, 0, (void) 6);
    goto dun2;
  }
  sm = ilu_CreateMutex("server ", id);
  if (!sm) {
    ILU_ERR_CONS1(no_memory, err, nbytes, 0, (void) 6);
    goto dun2;
  }
  ans = (ilu_Server) ilu_MallocE(sizeof(*ans), err);
  if (!ans)
    goto dun2;
  ans->sr_lock = sm;
  ans->sr_true = ilu_TRUE;
  ans->sr_id = id;
  ans->sr_crc32 = ilu_CRC32((ilu_bytes) id, (ilu_cardinal) strlen(id));
  ans->sr_true_language = language;
  ans->sr_tinfo = NIL;
  ans->sr_pinfo = NIL;
  ans->sr_tcr = NIL;
  ans->sr_protocol = NIL;
  ans->sr_cinfo = cinfo;
  ans->sr_extCinfo = zCharBuf;
  for (i = 0; i < ILU_MAX_ADDR_SPACE_LANGUAGES; i++)
    ans->sr_lsss[i] = NIL;
  ans->sr_holds = 0;
  ans->sr_closing = ilu_FALSE;
  ans->sr_cfails = ilu_FALSE;
  ans->sr_connHead.next = ans->sr_connHead.prev = NIL;
  ans->sr_closedConns.next = ans->sr_closedConns.prev = NIL;
  ans->sr_ports.pl_next = ans->sr_ports.pl_prev = NIL;
  ans->sr_closedPorts.pl_next = ans->sr_closedPorts.pl_prev = NIL;
  ans->sr_local_port = NIL;
  ans->sr_objs = objs;
  ans->sr_singles = singles;
  ans->sr_objtab = objtab;
  ans->sr_default_port = NIL;
  ans->sr_relocate_proc = NULLFN;
  ans->sr_relocate_rock = NIL;
  if (!ilu_ReEnterMutex(server_lock(ans), err))
    goto dun2;
  if (!ilu_Check(ilu_hash_AddToTable(Servers, server_id(ans), ans),
		 err)) {
    (void) _ilu_ExitServerMutex(ans, ilu_TRUE, err);
    goto dun2;
  }
  ILU_NOTE(SERVER_DEBUG,
	   ("ilu_CreateTrueServer:  created new server \"%s\","
	    " objtable %p, true lang \"%s\".\n",
	    server_id(ans), objtab, _ilu_LangNames[language]));
dun2:
  if (ILU_ERROK(*err))
    return (ans);
  if (ans)
    ilu_free(ans);
  ans = NIL;
  if (objs)
    ilu_hash_FreeHashTable(objs, NULLFN, NULLFN);
  if (singles)
    ilu_hash_FreeHashTable(singles, NULLFN, NULLFN);
  if (sm) {
    ILU_ERRS((bad_locks, bad_param, internal)) ferr;
    (void) ilu_DestroyMutex(sm, &ferr);
    ILU_HANDLED(ferr);
  }
  if (!ilu_ExitMutex(ilu_cmu, ilu_TRUE, err))
    goto dun0;
dun1:
  if (!ilu_ExitMutex(ilu_gcmu, ilu_TRUE, err))
    goto dun0;
dun0:
  ilu_CharBufFree(cinfo);
  return (ans);
}

/*L1_sup >= s */
ilu_private
  ilu_SetServerRelocateProc(ilu_Server s,
			    ilu_ServerRelocateProc proc,
			    ilu_private rock,
			    ILU_ERRS((no_memory, internal)) *err)
{
  ilu_private old_rock = s->sr_relocate_rock;
  s->sr_relocate_proc = proc;
  s->sr_relocate_rock = rock;
  ILU_NOTE(SERVER_DEBUG,
	   ("ilu_SetServerRelocateProc:  server <%s> set to (%p, %p)\n",
	    server_id(s), proc, rock));
  ILU_CLER(*err);
  return old_rock;
}

/*L1_sup < s*/
void ilu_SetServerDefaultPort(ilu_Server s, ilu_Port p)
{
  if (port_server(p) == s) {
    _ilu_AcquireServerMutex(s);
    if (p != server_default_port(s)) {
      ilu_Port        q;
      ILU_ERRS((no_memory)) lerr;
      server_default_port(s) = p;
      s->sr_cinfo.icb_len = 0;
      ilu_Append1Cinfo(&s->sr_cinfo, p->po_pinfo, p->po_tinfo, &lerr);
      ILU_MUST_BE_SUCCESS(lerr);
      for (q = server_ports(s); q; q = port_next(q)) {
	if (p != q) {
	  ilu_Append1Cinfo(&s->sr_cinfo, q->po_pinfo, q->po_tinfo, &lerr);
	  ILU_MUST_BE_SUCCESS(lerr);
	}
      }
      if (s->sr_extCinfo.icb_len) {
	static char sepBuf[2] = { ILU_CINFO_MARKER, 0 };
	ilu_CharBufAppend(&s->sr_cinfo, sepBuf, 1, &lerr);
	ILU_MUST_BE_SUCCESS(lerr);
	ilu_CharBufAppend(&s->sr_cinfo, s->sr_extCinfo.icb_base,
			  s->sr_extCinfo.icb_len, &lerr);
	ILU_MUST_BE_SUCCESS(lerr);
      }
    }
    _ilu_ReleaseServerMutex(s);
  } else {
    /* Bitch to caller, when we get our error system. */
    return;
  }
}

/* Main Invariant Holds */

ilu_boolean ilu_AddCInfo(ilu_Server s,
			 ilu_string pinfo,
			 ilu_TransportInfo tinfo,
			 ILU_ERRS((inv_objref, bad_locks, internal,
				   no_memory)) *err)
{
  if (!ilu_EnterServerMutex(s, ilu_FALSE, err))
    return ilu_FALSE;
  if (!ilu_Append1Cinfo(&s->sr_extCinfo, pinfo, tinfo, err))
    goto dun1;
  if (!ilu_Append1Cinfo(&s->sr_cinfo, pinfo, tinfo, err))
    goto dun1;
dun1:
  if (!ilu_ExitServerMutex(s, ilu_TRUE, err))
    return ilu_FALSE;
  return ilu_TRUE;
}

/* L1 >= {server} */
ilu_boolean
ilu_ServerCInfo(ilu_Server server,           /* retain */
		ilu_boolean want_public,
		ilu_string *pinfo,
		ilu_TransportInfo *tinfo,
		ILU_ERRS((bad_param, internal, no_memory)) *err)
{
  ilu_Port	p = server->sr_ports.pl_next;
  while (p) {
    if (!p->po_public == !want_public) {
      *pinfo = p->po_pinfo;
      *tinfo = p->po_tinfo;
      return ILU_CLER(*err);
    }
    p = p->po_links.pl_next;
  }
  return (ILU_CLER(*err), ilu_FALSE);
}

/*L1 >= {cmu, s}, L1_sup < trmu; L2 unconstrained*/

static          ilu_boolean
CloseNonIoingConns(ilu_Server s, ilu_Connection first,
		   ILU_ERRS((bad_locks, broken_locks)) * err)
{
  ilu_Connection  cur = first, next;
  while (cur != NIL) {
    next = cur->co_links[ilu_psl].next;
    if (cur->co_ioing != ilu_TRUE) {
      cur->co_ioing = ilu_TRUE;
      _ilu_CloseIoingConnection(cur, ilu_FALSE,
			ilu_ConnShutdownReason_ProcessTermination);
      if (!_ilu_ReleaseConnIO(cur, ilu_TRUE, err))
	return ilu_FALSE;
    }
    cur = next;
  }
  return ILU_CLER(*err);
}

static void
DisconnectServer(ilu_Server s,
		 ILU_ERRS((bad_locks, broken_locks)) * err)
{
  ilu_Port        p = s->sr_local_port;
  if (p != NIL)
    if (!CloseNonIoingConns(s, p->po_connHead.next, err))
      return;
  p = server_ports(s);
  while (p != NIL) {
    if (!CloseNonIoingConns(s, p->po_connHead.next, err))
      return;
    p = p->po_links.pl_next;
  }
  (void) CloseNonIoingConns(s, server_connections(s), err);
  return;
}

/*L1 >= {s}*/
static void 
_ilu_RemSingleton(ilu_Server s, ilu_Class t, ilu_Object o)
{
  ilu_cardinal             i, l;
  ilu_Object      o2;
  if (!class_singleton(t))
    return;
  o2 = ilu_hash_RemoveFromTable(server_singles(s), t);
  if (o2 == NIL)
    return;			/* already visited */
  _ilu_Assert(o2 == o, "RemSingleton");
  l = class_superclass_count(t);
  for (i = 0; i < l; i++)
    _ilu_RemSingleton(s, class_superclass(t, i), o);
  return;
}


ILU_ERRS((internal, bad_locks, broken_locks))
_ilu_ServerRemoveObject(ilu_Server s, ilu_Object obj)
{
  ilu_Error       err = ILU_INIT_NO_ERR;
  unsigned int i;
  if (!ilu_Check(!!s->sr_objs, &err))
    return err;
  if (!ilu_Check((ilu_hash_RemoveFromTable(s->sr_objs, object_ih(obj))
		  == obj),
		 &err))
    return err;
  _ilu_RemSingleton(s, object_class(obj), obj);
	
  if (ilu_hash_PairsInTable(s->sr_objs) > 0)
    /* still have objects in this server, so nothing more to do */
    return err;
	
  if (s->sr_closing == ilu_TRUE) {
    if (!ilu_Check(ilu_hash_PairsInTable(s->sr_singles) == 0, &err))
      return err;
    ilu_hash_FreeHashTable(s->sr_objs, NULLFN, NULLFN);
    s->sr_objs = NIL;
    ilu_hash_FreeHashTable(s->sr_singles, NULLFN, NULLFN);
    s->sr_singles = NIL;
    return err;
  } 
	
  for (i = 0; i < _ilu_NLanguages; i++)
    /* return if any lss exist for this server */
    if (s->sr_lsss[i])
      return err;

  /* we must be empty of objects and have no lss - disconnect us */
  DisconnectServer(s, &err);
  return err;
}


/*L1 >= {s}; L2 unconstrained*/

int
ilu_ScanServerObjs(ilu_Server s,
		   ilu_objectCallback cb,
		   ilu_refany rock)
{
  HashEnumerator  he;
  ilu_refany      key, data;
  if (s->sr_objs == NIL)
    return 0;
  ilu_hash_BeginEnumeration(s->sr_objs, &he);
  while ((s->sr_objs != NIL) && (ilu_hash_Next(&he, &key, &data))) {
    ilu_Object      obj = (ilu_Object) data;
    int             ans;
    ans = (*cb)(obj, rock);
    if (ans != 0)
      return ans;
  }
  return 0;
}

ilu_cardinal ilu_NumObjsInServer(ilu_Server s)
{
  if (s->sr_objs == NIL)
    return 0;
  else
    return ilu_hash_PairsInTable(s->sr_objs);
}

static ilu_cardinal
  CountIoingConns(ilu_Server s, ilu_Connection first)
{
  ilu_cardinal    ans = 0;
  ilu_Connection  cur;
  for (cur = first; cur != NIL; cur = cur->co_links[ilu_psl].next)
    if (cur->co_ioing == ilu_TRUE)
      ans++;
  return ans;
}

ilu_cardinal ilu_NumIoingConnsOfServer(ilu_Server s)
{
  ilu_cardinal    ans = 0;
  ilu_Port        p = s->sr_local_port;
  if (p != NIL)
    ans = CountIoingConns(s, p->po_connHead.next);
  p = server_ports(s);
  while (p != NIL) {
    ans += CountIoingConns(s, p->po_connHead.next);
    p = p->po_links.pl_next;
  }
  ans += CountIoingConns(s, server_connections(s));
  return ans;
}

ilu_boolean
_ilu_ServerEmptyP(ilu_Server s)
{
  return (ilu_NumObjsInServer(s) == 0);
}

/* L1 >= {cmu, s}; L2 unconstrained */
void ilu_InnerBankServer(ilu_Server s)
{
  ilu_PreBankServer(s);
  return;
}

/* L1 >= {cmu, s}; L2 unconstrained */
void ilu_PreBankServer(ilu_Server s)
{
  ILU_ERRS((bad_locks, broken_locks)) lerr;
  if (s->sr_closing == ilu_TRUE)
    return;
  s->sr_closing = ilu_TRUE;
  if (s->sr_objtab != NIL) {
    (*s->sr_objtab->ot_free_self) (s->sr_objtab);
    s->sr_objtab = NIL;
  }
  DisconnectServer(s, &lerr);
  ILU_MUST_BE_SUCCESS(lerr);
  if (server_is_true(s)) {
    ilu_Port        cur = server_ports(s), next;
    while (cur != NIL) {
      next = cur->po_links.pl_next;
      _ilu_ClosePort(cur);
      cur = next;
    }
    if (s->sr_local_port != NIL)
      _ilu_ClosePort(s->sr_local_port);
  } else {
    _ilu_CloseTCR(s->sr_tcr);
    s->sr_tcr = NIL;
  }
  if (ilu_hash_PairsInTable(s->sr_objs) == 0) {
    _ilu_Assert(ilu_hash_PairsInTable(s->sr_singles) == 0,
		"BankServer: singles not empty too");
    ilu_hash_FreeHashTable(s->sr_objs, NULLFN, NULLFN);
    s->sr_objs = NIL;
    ilu_hash_FreeHashTable(s->sr_singles, NULLFN, NULLFN);
    s->sr_singles = NIL;
  }
  if (Servers != NIL) {
    ilu_string      sid = server_id(s);
    ilu_Server      s2 = ilu_hash_RemoveFromTable(Servers, sid);
    _ilu_Assert(s2 == s, "BankServer RemoveFromTable");
  }
  return;
}

/*L1 >= {s}; L2 unconstrained*/
ilu_refany
ilu_GetLSS(ilu_Server s, ilu_LanguageIndex language)
{
  if (language >= _ilu_NLanguages)
    return NIL;
  return s->sr_lsss[language];
}

/*L1 >= {s}; L2 unconstrained*/
ilu_boolean
ilu_SetLSS(ilu_Server s, ilu_refany lss,
	   ilu_LanguageIndex language,
	   ILU_ERRS((bad_param, internal)) * err)
{
  if (language >= _ilu_NLanguages)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, ilu_FALSE);
  s->sr_lsss[language] = lss;
  ILU_NOTE(SERVER_DEBUG,
	   ("ilu_SetLSS(%p=%s, %s, %p); "
	    "ports=%s %s %s, conns=%s %s, objs=%d, LSSes=%d.\n",
	    s, s->sr_id, _ilu_LangNames[language], lss,
	    (server_ports(s) ? "X" : "0"),
	    (s->sr_local_port ? "X" : "0"),
	    (s->sr_closedPorts.pl_next ? "X" : "0"),
	    (s->sr_connHead.next ? "X" : "0"),
	    (s->sr_closedConns.next ? "X" : "0"),
	    (s->sr_objs ? ilu_hash_PairsInTable(s->sr_objs) : 0),
	    _ilu_ServerLSSCount(s)
	    ));
  return ILU_CLER(*err);
}

/*L1 < cmu; L2 unconstrained*/

void ilu_BankServer (ilu_Server s)
{
  _ilu_AcquireMutex(ilu_cmu);
  _ilu_AcquireServerMutex(s);
  ilu_InnerBankServer(s);
  _ilu_ReleaseServerMutex(s);
  _ilu_ReleaseMutex(ilu_cmu);
  return;
}

/* L1 < gcmu; L2 unconstrained */
int
ilu_BankAndScanServer(ilu_Server s,
		      ilu_objectCallback cb,
		      ilu_refany rock,
		      ilu_cardinal * nconns)
{
  ilu_boolean     istrue = ilu_TrueServerP(s);
  int             ans;
  if (istrue)
    _ilu_AcquireMutex(ilu_gcmu);
  _ilu_AcquireMutex(ilu_cmu);
  _ilu_AcquireServerMutex(s);
  (void) ilu_InnerBankServer(s);
  ans = ilu_ScanServerObjs(s, cb, rock);
  if (nconns != NIL)
    *nconns = ilu_NumIoingConnsOfServer(s);
  _ilu_ReleaseServerMutex(s);
  _ilu_ReleaseMutex(ilu_cmu);
  if (istrue)
    _ilu_ReleaseMutex(ilu_gcmu);
  return ans;
}


/*before: 				       L1 disjoint {cmu, server};
  before: cl collectible		    => L1  not >=  {gcmu};
  before: cl collectible & server surrogate => Main Invariant holds;
  after:  Inside(server, cl)*/
void ilu_EnterServer(ilu_Server server, ilu_Class cl)
{
  if (class_collectible(cl) && server->sr_true)
      _ilu_AcquireMutex(ilu_gcmu);
  _ilu_AcquireMutex(ilu_cmu);
  _ilu_AcquireServerMutex(server);
  return;
}

static void 
DestroyServer(ilu_Server server)
{
  ilu_Server s2 = ilu_hash_FindInTable(Servers, server_id(server));
  if (s2 == server)
    ilu_hash_RemoveFromTable(Servers, server_id(server));
  if (server->sr_objs)
    ilu_hash_FreeHashTable(server->sr_objs, NULLFN, NULLFN);
  if (server->sr_singles)
    ilu_hash_FreeHashTable(server->sr_singles, NULLFN, NULLFN);
  if (server->sr_tcr)
    (*(server->sr_tcr->tcr_close)) (server->sr_tcr);
  ilu_free(server->sr_id);
  ilu_free(server->sr_tinfo);
  ilu_free(server->sr_pinfo);
  ilu_CharBufFree(server->sr_cinfo);
  ilu_CharBufFree(server->sr_extCinfo);
  ilu_free(server);
  return;
}

static ilu_boolean HasLSS(ilu_Server server)
{
  unsigned int             i;
  for (i = 0; i < _ilu_NLanguages; i++)
    if (server->sr_lsss[i])
      return ilu_TRUE;
  return ilu_FALSE;
}

int _ilu_ServerLSSCount(ilu_Server server)
{
  unsigned int i;
  int ans = 0;
  for (i = 0; i < _ilu_NLanguages; i++)
    if (server->sr_lsss[i])
      ans++;
  return ans;
}

int ilu_check_PortsStayClosed = 0;

ilu_boolean
ilu_EnterServerMutexFull(ilu_Server server, ilu_boolean hard,
			 ILU_ERRS((bad_locks, broken_locks,
				   internal)) * err,
			 char *filename, int lineno)
{
  ilu_boolean     ans;
  if (!(server &&
	((server->sr_true
	  && (server_ports(server) || server->sr_local_port
	      || server->sr_closedPorts.pl_next))
	 || server->sr_connHead.next
	 || server->sr_closedConns.next
	 || server->sr_holds
	 || (server->sr_objs &&
	     ilu_hash_PairsInTable(server->sr_objs))
	 || HasLSS(server)))) {
    if (ILU_ERROK(*err))
      (void) ilu_Check(ilu_FALSE, err);
    return ilu_FALSE;
  }
  ans = ilu_EnterMutexWork(server_lock(server), hard, err,
			   filename, lineno);
  if (ans && ilu_check_PortsStayClosed && server->sr_true) {
    ilu_Port        p;
    for (p = server->sr_closedPorts.pl_next; p; p = p->po_links.pl_next) {
      _ilu_Assert(p->po_closed, "PortsStayClosed at Enter");
    }
  }
  return ans;
}

ilu_boolean
ilu_ExitServerMutexFull(ilu_Server server, ilu_boolean hard,
			ILU_ERRS((bad_locks, broken_locks,
				  internal)) * err,
			char *filename, int lineno)
{
  ilu_Mutex       sm = server_lock(server);
  ilu_boolean     ans, killit = ilu_FALSE;
  if (ilu_check_PortsStayClosed && server->sr_true) {
    ilu_Port        p;
    for (p = server->sr_closedPorts.pl_next; p; p = p->po_links.pl_next) {
      _ilu_Assert(p->po_closed, "PortsStayClosed at Exit");
    }
  }
  if (!(server->sr_true
	&& (server_ports(server) || server->sr_local_port
	    || server->sr_closedPorts.pl_next))
      && !server->sr_connHead.next
      && !server->sr_closedConns.next
      && !server->sr_holds
      && !(server->sr_objs
	   && ilu_hash_PairsInTable(server->sr_objs))
      && !HasLSS(server)) {
    killit = ilu_TRUE;
    ILU_NOTE(SERVER_DEBUG,
	     ("server.c: Freeing server %p (%s) at %s:%d.\n",
	      server, server->sr_id, filename, lineno));
    DestroyServer(server);
  }
  ans = ilu_ExitMutexWork(sm, hard, err, filename, lineno);
  if (killit) {
    ILU_ERRS((bad_param, bad_locks)) lerr;
    if (!ilu_DestroyMutex(sm, &lerr)) {
      if (ILU_ERROK(*err)) {
	*err = lerr;
	ans = ilu_FALSE;
      } else
	ILU_HANDLED(lerr);
    }
  }
  return ans;
}

/*before: Inside(server, cl);
  after:				      L1 disjoint {cmu, server};
  after: cl collectible			   => L1  not >=  {gcmu};
  after: cl collectible & server surrogate => Main Invariant holds*/
void ilu_ExitServer(ilu_Server server, ilu_Class cl)
{
  ilu_boolean     is_true = server->sr_true;
  _ilu_ReleaseServerMutex(server);
  _ilu_ReleaseMutex(ilu_cmu);
  if (class_collectible(cl) && is_true)
    _ilu_ReleaseMutex(ilu_gcmu);
  return;
}

/*Inside(server, result's type)*/
ilu_Object _ilu_FindObjectInServer(ilu_string ih, ilu_Server s)
{
  ilu_Object      o;
  o = (ilu_Object) ilu_hash_FindInTable(server_objs(s), ih);
  if (o == NIL && server_is_true(s) && server_objtab(s) != NIL) {
    o = (*server_objtab(s)->ot_object_of_ih) (server_objtab(s), ih);
    if (o == NIL) {
    } else if (ilu_hash_FindInTable(server_objs(s), ih) != o) {
      if (strcmp(ih, object_ih(o)) != 0)
	ilu_DebugPrintf ("_ilu_FindObjectInServer(ih=\"%s\", s=\"%s\"): Error -- the object returned from the object table"
			 " does not have the specified instance handle!\n", ih, server_id(s));
      _ilu_Assert(0, "_ilu_FindObjectInServer: obj tab returned uninterned object");
    }
  }
  return (o);
}

/* before: L1 = {};
   after:  result == NIL => L1 = {}
           result != NIL => Inside(object_server(obj), object_class(obj))
 */
ilu_Object ilu_FindObject (ilu_string sid, ilu_string ih)
{
  /* Find and return specified object in specified language, or NIL
     if object doesn't exist or server doesn't exist */
  ilu_Object    kobj;
  ilu_Server	server;

  server = (ilu_Server) ilu_hash_FindInTable(Servers, sid);
  if (server == NIL)
    return NIL;
  ilu_EnterServer(server, _ilu_rootClass);
  kobj = _ilu_FindObjectInServer(ih, server);
  if (kobj == NIL) {
    ilu_ExitServer(server, _ilu_rootClass);
    return NIL;
  } else {
    return kobj;
  }
}

/*L1 >= {s}; L2 unconstrained*/
ilu_boolean
ilu_DeltaServerHolds(ilu_Server s, int dholds,
		     ILU_ERRS((bad_locks, internal/broken)) * err)
{
  s->sr_holds = s->sr_holds + dholds;
  ILU_CLER(*err);
  return ilu_TRUE;
}

