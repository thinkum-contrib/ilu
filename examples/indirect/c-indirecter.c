#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "Indirect.h"

#include <iluhash.h>

static ilu_HashTable servers = ILU_NIL; /* sid -> &CInfos */

static Indirect_TInfo CopyTInfo(Indirect_TInfo auld)
{
  Indirect_TInfo neau;
  int i;
  Indirect_TInfo_Init(&neau, auld._length+1, ILU_NIL);
  if (!neau._buffer) goto fale;
  for (i=0; i<auld._length; i++) {
    Indirect_TInfo_Append(&neau, strdup(auld._buffer[i]));
    if (!neau._buffer) goto fale;
  }
  Indirect_TInfo_Append(&neau, 0);
  if (!neau._buffer) goto fale;
  neau._length--;
  return neau;
 fale:
  fprintf(stderr, "Out of memory copying TInfo!\n");
  exit(1);
}

static ilu_boolean Relocate(ILU_C_Server s,
			    ilu_private rock,
			    ilu_ProtocolInfo *pinfo,
			    ilu_TransportInfo *tinfo)
{
  ILU_ERRS((no_memory)) lerr;
  Indirect_CInfos *cs = (Indirect_CInfos*) rock;
  ilu_FineTime now;
  unsigned i;
  now = ilu_FineTime_Now();
  i = 1 + (now.ft_s + now.ft_t) % (cs->_length - 1);
  *pinfo = strdup(cs->_buffer[i].p);
  *tinfo = ilu_CopyTinfo(cs->_buffer[i].t._buffer, &lerr);
  return (*pinfo) && ILU_ERROK(lerr);
}

Indirect_CInfo *
server_Indirect_Indirecter_register(Indirect_Indirecter self,
				    Indirect_ServerID sid,
				    Indirect_PInfo pi,
				    Indirect_TInfo *ti,
				    CORBA_Environment *env)
{
  Indirect_CInfos *cs = ILU_NIL;
  Indirect_CInfo c, myn, *ret = ILU_NIL;
  ILU_ERRS((no_memory, internal/check)) lerr;
  ilu_string sti = ILU_NIL;
  ret = (Indirect_CInfo*) ilu_malloc(sizeof(*ret));
  if (!ret) {
    fprintf(stderr, "unable to allocate register result!\n");
    exit(1);
  }
  cs = (Indirect_CInfos*) ilu_hash_FindInTable(servers, sid);
  if (cs) {
    myn = cs->_buffer[0];
  } else {
    ILU_C_Server s;
    ilu_string slti;
    ilu_string pubTinfo[] = {"tcp_0_0", ILU_NIL};
    sid = strdup(sid);
    cs = Indirect_CInfos_Create(5, ILU_NIL);
    if (!cs) {
      fprintf(stderr, "Unable to allocate new CInfos!\n");
      exit(1);
    }
    cs->_length = 0;
    s = ILU_C_InitializeServer(sid, ILU_NIL, "iiop", pubTinfo,
			       ILU_NIL, ilu_TRUE);
    if (!s) {
      fprintf(stderr, "Failed to create server \"%s\"!\n", sid);
      exit(1);
    }
    if (!ILU_C_Server_CInfo(s, ilu_TRUE, &myn.p, &myn.t._buffer, env)) {
      fprintf(stderr, "Unable to get server cinfo!\n");
      exit(1);
    }
    myn.t._length = myn.t._maximum = ilu_TransportInfo_Len(myn.t._buffer);
    Indirect_CInfos_Append(cs, &myn);
    if (!cs->_buffer) {
      fprintf(stderr, "Unable to add indirecter's cinfo to server's list!\n");
      exit(1);
    }
    (void) ILU_C_SetServerRelocationProc(s, Relocate, cs, env);
    if (!ILU_C_SUCCESSFUL(env)) {
      fprintf(stderr, "Unable to set relocation proc on server \"%s\"!\n",
	      sid);
      exit(1);
    }
    if (!ilu_hash_AddToTable(servers, sid, cs)) {
      fprintf(stderr, "Unable to add new CInfos to server table!\n");
      exit(1);
    }
    slti = ilu_TransportInfo_Stringify(myn.t._buffer, &lerr);
    if (ILU_ERRNOK(lerr)) {
      fprintf(stderr, "Unable to stringify public tinfo for new server!\n");
      exit(1);
    }
    printf("Added sid=%s, public pinfo=%s, tinfo=%s.\n", sid, myn.p, slti);
    ilu_free(slti);
  }
  c.p = strdup(pi);
  c.t = CopyTInfo(*ti);
  Indirect_CInfos_Append(cs, &c);
  if (!cs->_buffer) {
    fprintf(stderr, "Unable to add new cinfo to server's list!\n");
    exit(1);
  }
  sti = ilu_TransportInfo_Stringify(c.t._buffer, &lerr);
  if (ILU_ERRNOK(lerr)) {
    fprintf(stderr, "Unable to stringify given tinfo!\n");
    exit(1);
  }
  ret->p = strdup(myn.p);
  if (!ret->p) {
    fprintf(stderr, "failed to duplicate indirecter pinfo!\n");
    exit(1);
  }
  ret->t = CopyTInfo(myn.t);
  printf("Register(sid=%s, pinfo=%s, tinfo=%s)\n",
	 sid, c.p, sti);
  ilu_free(sti);
  return ret;
}

ILU_C_Server indirSvr = ILU_NIL;
Indirect_Indirecter indirObj = ILU_NIL;

int main(char **argv, int argc)
{
  Indirect__InitializeServer();
  servers = ilu_hash_MakeNewTable(5, ilu_hash_HashString,
				  ilu_hash_StringCompare);
  indirSvr = ILU_C_InitializeServer("indirection-test", ILU_NIL, ILU_NIL,
				    ILU_NIL, ILU_NIL, ilu_TRUE);
  if (!indirSvr) {
    fprintf(stderr, "Unable to create indirection server!\n");
    exit(1);
  }
  indirObj = Indirect_Indirecter__CreateTrue("it", indirSvr, ILU_NIL);
  if (!indirObj) {
    fprintf(stderr, "Unable to create indirection object!\n");
    exit(1);
  }
  (void) ILU_C_PublishObject(indirObj);
  printf("Starting indirection service, sbh = '%s'.\n",
	 ILU_C_SBHOfObject(indirObj));
  ILU_C_Run();
  return -1;
}
