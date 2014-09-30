#include <stdio.h>
#include <stdlib.h>

#include "Indirect.h"
#include "Svc1.h"

void server_Svc1_O_m1(Svc1_O self,
		      ilu_CString s1,
		      CORBA_Environment *env)
{
  printf("Svc1.O.m1(%s)\n", s1);
  return;
}

ILU_C_Server mySvr = ILU_NIL;
Svc1_O myObj = ILU_NIL;
Indirect_Indirecter indir = ILU_NIL;

static char mySid[] = "indir-test-svc1";

int main (char**argv, int argc)
{
  Indirect_CInfo myCInfo, *pubCInfo;
  CORBA_Environment env;
  ilu_string pvtTinfo[] = {"tcp_0_0", 0};
  Indirect__Initialize();
  Svc1__InitializeServer();
  mySvr = ILU_C_FullInitializeServer(mySid, ILU_NIL,
				     "iiop", pvtTinfo, ILU_NIL,
				     ilu_TRUE, ilu_FALSE);
  if (!mySvr) {
    fprintf(stderr, "Unable to create my server!\n");
    exit(1);
  }
  myObj = Svc1_O__CreateTrue("it", mySvr, ILU_NIL);
  if (!myObj) {
    fprintf(stderr, "Unable to create my object!\n");
    exit(1);
  }
  indir = ILU_C_LookupObject("indirection-test", "it",
			     Indirect_Indirecter__MSType);
  if (!indir) {
    fprintf(stderr, "Unable to import indirecter!\n");
    exit(1);
  }
  if (!ILU_C_Server_CInfo(mySvr, ilu_FALSE,
			  &myCInfo.p, &myCInfo.t._buffer, &env)) {
    fprintf(stderr, "Unable to get my cinfo!\n");
    exit(1);
  }
  myCInfo.t._length = ilu_TransportInfo_Len(myCInfo.t._buffer);
  myCInfo.t._maximum = myCInfo.t._length;
  pubCInfo = Indirect_Indirecter_register(indir, mySid, myCInfo.p,
					  &myCInfo.t, &env);
  if (!ILU_C_SUCCESSFUL(&env)) {
    fprintf(stderr, "registration failed!\n");
    exit(1);
  }
  if (!ILU_C_AddCInfo(mySvr, pubCInfo->p, pubCInfo->t._buffer, &env)) {
    fprintf(stderr, "Unable to add indirecting cinfo!\n");
    exit(1);
  }
  (void) ILU_C_PublishObject(myObj);
  {
    ilu_string st;
    ILU_ERRS((no_memory, internal/check)) lerr;
    st = ilu_TransportInfo_Stringify(myCInfo.t._buffer, &lerr);
    if (ILU_ERRNOK(lerr)) {
      fprintf(stderr, "Unable to stringify private tinfo!\n");
      exit(1);
    }
    printf("Running server, SBH='%s', private pinfo='%s', tinfo='%s'.\n",
	   ILU_C_SBHOfObject(myObj), myCInfo.p, st);
  }
  ILU_C_Run();
  return -1;
}
