/** $Id: bsvr.c,v 1.15 1999/08/03 01:57:31 janssen Exp $
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
#include <unistd.h>
#endif

#include "Batcher.h"

static Batcher_TimeSeq times;
static int verbose = 0;
static unsigned int tosleep = 0;

void
server_Batcher_T_Send(Batcher_T self,
		      Batcher_Time s, ILU_C_ENVIRONMENT * Env)
{
  ilu_FineTime    now = ilu_FineTime_Now();
  Batcher_TimeRec nu;
  if (verbose)
    printf("Send(%lu:%lu -> %lu:%lu)\n",
	   (unsigned long) ILU_LONGCARD_HIGH_WORD(&s),
	   (unsigned long) ILU_LONGCARD_LOW_WORD(&s),
	   (unsigned long) now.ft_s, (unsigned long) now.ft_t);
  nu.s = s;
  ILU_LONGCARD_HIGH_WORD(&nu.r) = now.ft_s;
  ILU_LONGCARD_LOW_WORD(&nu.r) = now.ft_t;
  Batcher_TimeSeq_Append(&times, &nu);
  ILU_C_SET_SUCCESSFUL(Env);
  return;
}

Batcher_TimeSeq *
server_Batcher_T_Sync(Batcher_T self,
		      Batcher_Time s, ILU_C_ENVIRONMENT * Env)
{
  Batcher_TimeSeq *ans = Batcher_TimeSeq__alloc();
  ilu_cardinal    i;
  server_Batcher_T_Send(self, s, Env);
  if (tosleep) {
#if defined(WIN32)
    _sleep(tosleep);
#else
    sleep(tosleep);
#endif
  }
  Batcher_TimeSeq_Init(ans, times._length, NULL);
  for (i = 0; i < times._length; i++)
    Batcher_TimeSeq_Append(ans, &times._buffer[i]);
  times._length = 0;
  return ans;
}

int
main(int argc, char **argv)
{
  ILU_C_Server s;
  Batcher_T t;
  int i;
  ilu_string progname, sbh, pinfo = NULL;
  ilu_string tinfo[10] = { "sunrpcrm", "tcp_0_0", NULL };

  Batcher__InitializeServer();
  Batcher_TimeSeq_Init(&times, 50, NULL);
  progname = argv[0];
  argv++;
  argc--;
  for (i = 0;  i < argc;  i++) {
    if (strcmp(argv[i], "-p") == 0 && ((i+1) < argc))
      pinfo = argv[++i];
    else if (strcmp(argv[i], "-s") == 0 && ((i+1) < argc))
      tosleep = atoi(argv[++i]);
    else if (strcmp(argv[i], "-t") == 0 && ((i+1) < argc)) {
      int j = 0;
      ++i;
      while ((i < argc) && (argv[i][0] != '-'))
	tinfo[j++] = argv[i++];
      tinfo[j] = NULL;
    }
    else if (strcmp(argv[i], "-v") == 0)
      verbose = 1;
    else
      goto usage;
  }
  s = ILU_C_InitializeServer("Batcher-Server", NULL, pinfo, tinfo,
			     ILU_NIL, ilu_TRUE);
  if (s == NULL) {
    fprintf(stderr, "Unable to create ILU server!\n");
    exit(1);
  }
  t = Batcher_T__CreateTrue("it", s, NULL);
  if (t == NULL) {
    fprintf(stderr, "Unable to create ILU object!\n");
    exit(1);
  }
  if (ILU_C_PublishObject(t) == NULL) {
    fprintf(stderr, "Can't publish object!\n");
    exit(1);
  }
  sbh = ILU_C_SBHOfObject(t);
  printf("tosleep = %u\n", tosleep);
  printf("SBH = '%s'\n", sbh);
  ILU_C_Run();
usage:
  fprintf(stderr,
    "Usage: %s [-v] [-s sleep] [-p pinfo] [-t tinfo [tinfo...]]\n",
	  progname);
  return 1;
}
