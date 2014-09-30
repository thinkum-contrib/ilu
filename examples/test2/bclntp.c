/** $Id: bclntp.c,v 1.7 1999/08/03 01:57:36 janssen Exp $
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
/* Last edited by Mike Spreitzer October 9, 1998 12:07 pm PDT */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>	/* for exit() */

#ifndef WIN32
#include <unistd.h>	/* for sleep() */
#endif

#include "Batcher.h"

static ILU_C_Pipeline pl = NULL;
static Batcher_T t = NULL;
static int      verbose = 0;
static unsigned nsyncs = 0, ndone = 0;

typedef struct {
  unsigned        i;
}               Inst;

static void DoCall(void *rock)
{
  Inst           *inst = (Inst *) rock;
  ILU_C_ENVIRONMENT env;
  ilu_FineTime    now;
  Batcher_Time    tnow;
  Batcher_TimeSeq *ans;
  unsigned        k;
  now = ilu_FineTime_Now();
  ILU_LONGCARD_HIGH_WORD(&tnow) = now.ft_s;
  ILU_LONGCARD_LOW_WORD(&tnow) = now.ft_t;
  if (verbose)
    printf("Calling Sync_%u(%lu:%lu)\n", inst->i, now.ft_s, now.ft_t);
  if (pl)
    ILU_C_SetPipelineContext(pl);
  ans = Batcher_T_Sync(t, tnow, &env);
  if (pl)
    ILU_C_SetPipelineContext(NULL);
  if (!ILU_C_SUCCESSFUL(&env)) {
    fprintf(stderr, "Sync_%u() => %s\n", inst->i,
	    ILU_C_EXCEPTION_ID(&env));
    exit(1);
  }
  printf("Sync_%u() = %d\n", inst->i, ans->_length);
  for (k = 0; k < ans->_length; k++)
    printf("\t%lu.%06lu->%lu.%06lu\n",
	(unsigned long) ILU_LONGCARD_HIGH_WORD(&ans->_buffer[k].s),
	 (unsigned long) ILU_LONGCARD_LOW_WORD(&ans->_buffer[k].s),
	(unsigned long) ILU_LONGCARD_HIGH_WORD(&ans->_buffer[k].r),
	(unsigned long) ILU_LONGCARD_LOW_WORD(&ans->_buffer[k].r));
  Batcher_TimeSeq__Free(ans);
  printf("\n");
  ndone += 1;
  if (ndone == nsyncs) {
    if (pl) {
      if (!ILU_C_ReleasePipeline(pl, &env)) {
	fprintf(stderr, "ILU_C_ReleasePipeline() => %s\n",
		ILU_C_EXCEPTION_ID(&env));
	exit(1);
      }
    }
    exit(0);
  }
}

int
main(int argc, char *argv[])
{
  unsigned        i, mt = 0;
  int             pipeit = 1;
  if (argc < 2)
    goto usage;
  if (sscanf(argv[1], "%u", &nsyncs) != 1)
    goto usage;
  for (i = 2; i < argc; i++) {
    if (strcmp(argv[i], "-v") == 0)
      verbose = 1;
    else if (strcmp(argv[i], "-pl") == 0)
      pipeit = 0;
    else if (strcmp(argv[i], "+pl") == 0)
      pipeit = 1;
    else if (strcmp(argv[i], "-mt") == 0)
      mt = 1;
    else if (strcmp(argv[i], "-st") == 0)
      mt = 0;
    else
      goto usage;
  }
  if (mt) {
#ifdef ILU_OS_THREADED
    ILU_C_USE_OS_THREADS;
#else
    fprintf(stderr, "Multi-threading not configured into ILU!\n");
    exit(1);
#endif
  }
  Batcher__Initialize();
  t = ILU_C_LookupObject("Batcher-Server", "it", Batcher_T__MSType);
  if (t == NULL) {
    fprintf(stderr, "Unable to import server!\n");
    exit(1);
  }
  printf("Doing %u syncs, %spipelined.\n", nsyncs,
	 (pipeit ? "" : "not "));
  if (pipeit) {
    ILU_C_ENVIRONMENT env;
    pl = ILU_C_CreatePipeline(&env);
    if (!ILU_C_SUCCESSFUL(&env)) {
      fprintf(stderr, "ILU_C_CreatePipeline() => %s\n",
	      ILU_C_EXCEPTION_ID(&env));
      exit(1);
    }
  }
  for (i = 0; i < nsyncs; i++) {
    Inst           *inst = (Inst *) ilu_must_malloc(sizeof(*inst));
    ilu_Closure     cl = (ilu_Closure) ilu_must_malloc(sizeof(*cl));
    ILU_ERRS((internal, bad_param)) lerr;
    inst->i = i;
    cl->proc = DoCall;
    cl->rock = inst;
#ifdef ILU_OS_THREADED
    if (mt) {
      ILU_ERRS((no_memory, no_resources, internal)) lerr;
      ilu_OSForkNewThread(DoCall, inst, &lerr);
      ILU_MUST_BE_SUCCESS(lerr);
    } else
#else
#endif
      {
	ilu_DoSoon(cl, &lerr);
	ILU_MUST_BE_SUCCESS(lerr);
      }
  }
  ILU_C_Run();
  return 0;
usage:
  fprintf(stderr, "Usage: %s n-syncs [-v] [-pl|+pl] [-mt|-st]\n",
	  argv[0]);
  return (1);
}
