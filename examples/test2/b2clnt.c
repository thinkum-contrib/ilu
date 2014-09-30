/** $Id: b2clnt.c,v 1.3 1999/08/03 01:57:37 janssen Exp $
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
/* Last edited by Mike Spreitzer October 9, 1998 12:06 pm PDT */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>	/* for exit() */

#ifndef WIN32
#include <unistd.h>	/* for sleep() */
#endif

#include "Batcher2.h"

Batcher2_Server svr;
Batcher2_CallBack replyTo;
float           pCallF, bTimeoutF, pPushF;
ilu_FineTime    pCallT, bTimeoutT, pPushT;
long unsigned   nCalls = 1000000000;
ilu_cardinal    callx = 0;
int             verbose = 0, retry = 0;
ilu_refany      callAlarm, pushAlarm = NULL, pauseAlarm;
ILU_C_Batcher   batcher;
ILU_C_Pipeline  pl;
Batcher2_Bytes  phil = {0};
int             stop, done = 0;

static void DoPush(void)
{
  ILU_C_ENVIRONMENT env;
  ILU_C_PushBatcher(batcher, &env);
  if (!ILU_C_SUCCESSFUL(&env)) {
    fprintf(stderr, "Push() => %s\n", ILU_C_EXCEPTION_ID(&env));
    if (!retry)
      exit(1);
  }
}

static void PushNow(ilu_private rock)
{
  ilu_FineTime    now = ilu_FineTime_Now();
  ilu_FineTime    next = ilu_FineTime_Add(now, pPushT);
  if (done)
    return;
  if (verbose)
    printf("PushNow(@%lu:%lu, next=%lu:%lu)\n",
	   (long unsigned) now.ft_s,
	   (long unsigned) now.ft_t,
	   (long unsigned) next.ft_s,
	   (long unsigned) next.ft_t);
  ilu_SetAlarm(pushAlarm, next, PushNow, NULL);
  DoPush();
  return;
}

static void CallNow(ilu_private rock)
{
  ILU_C_ENVIRONMENT env;
  ilu_FineTime    now = ilu_FineTime_Now();
  ilu_FineTime    next = ilu_FineTime_Add(now, pCallT);
  ilu_longcardinal r;
  if (callx < nCalls) {
    ilu_SetAlarm(callAlarm, next, CallNow, NULL);
    ILU_LONGCARD_HIGH_WORD(&r) = now.ft_s;
    ILU_LONGCARD_LOW_WORD(&r) = now.ft_t;
    Batcher2_Server_Request(svr, r, callx, &phil, replyTo, &env);
    callx++;
    if (!ILU_C_SUCCESSFUL(&env)) {
      fprintf(stderr, "Push() => %s\n", ILU_C_EXCEPTION_ID(&env));
      if (!retry)
	exit(1);
    }
  } else {
    done = 1;
    ILU_C_StopRun(&stop);
  }
  return;
}

static void UnPause(ilu_private rock)
{
  ILU_C_StopRun(&stop);
}

void
server_Batcher2_Server_Request(Batcher2_Server self,
			       Batcher2_Time r,
			       ilu_cardinal x,
			       Batcher2_Bytes * phil,
			       Batcher2_CallBack replyTo,
			       ILU_C_ENVIRONMENT * Env)
{
  return;
}

void
server_Batcher2_CallBack_Reply(Batcher2_CallBack self,
			       Batcher2_Time r,
			       ilu_cardinal x,
			       Batcher2_Time s,
			       ILU_C_ENVIRONMENT * Env)
{
  printf("Reply(%lu:%lu, %lu -> %lu:%lu)\n",
	 (unsigned long) ILU_LONGCARD_HIGH_WORD(&r),
	 (unsigned long) ILU_LONGCARD_LOW_WORD(&r),
	 (unsigned long) x,
	 (unsigned long) ILU_LONGCARD_HIGH_WORD(&s),
	 (unsigned long) ILU_LONGCARD_LOW_WORD(&s)
  );
  return;
}

int
main(int argc, char *argv[])
{
  ILU_C_Server    s;
  ILU_C_ENVIRONMENT env;
  int             i;
  long unsigned   len;
  if (argc < 5)
    goto usage;
  if (sscanf(argv[1], "%f", &pCallF) != 1)
    goto usage;
  if (sscanf(argv[2], "%f", &bTimeoutF) != 1)
    goto usage;
  if (sscanf(argv[3], "%f", &pPushF) != 1)
    goto usage;
  if (sscanf(argv[4], "%lu", &len) != 1)
    goto usage;
  if (argc > 5 && argv[5][0] != '-') {
    if (sscanf(argv[i = 5], "%lu", &nCalls) != 1)
      goto usage;
  } else
    i = 4;
  for (i++; i < argc; i++) {
    if (strcmp(argv[i], "-v") == 0)
      verbose = 1;
    else if (strcmp(argv[i], "-r") == 0)
      retry = 1;
    else
      goto usage;
  }
  Batcher2__Initialize();
  Batcher2__InitializeServer();
  pCallT = ilu_FineTime_FromDouble(pCallF);
  bTimeoutT = ilu_FineTime_FromDouble(bTimeoutF);
  pPushT = ilu_FineTime_FromDouble(pPushF);
  Batcher2_Bytes_Init(&phil, len, NULL);
  phil._length = len;
  s = ILU_C_InitializeServer(NULL, NULL, NULL, NULL, ILU_NIL, ilu_TRUE);
  replyTo = Batcher2_CallBack__CreateTrue(NULL, s, NULL);
  callAlarm = ilu_CreateAlarm();
  pushAlarm = ilu_CreateAlarm();
  pauseAlarm = ilu_CreateAlarm();
  svr = ILU_C_LookupObject("Batcher2-Server", "it",
			   Batcher2_Server__MSType);
  if (svr == NULL) {
    fprintf(stderr, "Unable to import server!\n");
    exit(1);
  }
  batcher = ILU_C_CreateBatcher(bTimeoutT, pPushF > 0.0, &env);
  pl = ILU_C_CreatePipeline(&env);
  ILU_C_SetBatcherContext(batcher);
  ILU_C_SetPipelineContext(pl);
  if (pPushF > 0.0)
    PushNow(NULL);
  CallNow(NULL);
  ILU_C_StoppableRun(&stop);
  ILU_C_SetBatcherContext(NULL);
  ILU_C_SetPipelineContext(NULL);
  if (pPushF > 0.0)
    DoPush();
  ILU_C_ReleaseBatcher(batcher, &env);
  ILU_C_ReleasePipeline(pl, &env);
  {
    ilu_FineTime    now, dt, fini;
    now = ilu_FineTime_Now();
    dt = ((!bTimeoutF) ? pPushT :
	  (!pPushF) ? bTimeoutT :
	  (pPushF < bTimeoutF) ? pPushT : bTimeoutT);
    dt = ilu_FineTime_Add(dt, ilu_FineTime_FromDouble(10.0));
    fini = ilu_FineTime_Add(now, dt);
    ilu_SetAlarm(pauseAlarm, fini, UnPause, NULL);
  }
  ILU_C_StoppableRun(&stop);
  return 0;
usage:
  fprintf(stderr,
	  "Usage: %s pCall bTimeout pPush len [nCalls] [-r] [-v]\n",
	  argv[0]);
  return (1);
}
