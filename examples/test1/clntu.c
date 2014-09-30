/** $Id: clntu.c,v 1.7 1999/08/03 01:52:17 janssen Exp $
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
/* Last edited by Mike Spreitzer October 9, 1998 3:04 pm PDT */


#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "clnt.h"

#include <iluchdrs.h>

#ifdef ILU_C_TIMING_STATISTICS
#include <math.h>	/* for sqrt() */
#define FMTSTATSBUFSIZE	512
static char    *
  FmtStats(char buf[FMTSTATSBUFSIZE], ILU_C_CallStatSum * ss, ilu_cardinal ncalls)
{
  double          avg, stdev;
  if (ncalls > 0) {
    avg = ss->sumX / ncalls;
    if (ncalls > 1) {
      stdev = (ss->sumXX - ss->sumX * ss->sumX / ncalls) / (ncalls - 1);
      if (stdev >= 0.0) {
	stdev = sqrt(stdev);
	sprintf(buf, "%10.3f / %10.3f +- %10.3f / %10.3f",
		ss->minX/1000.0, avg/1000.0, stdev/1000.0, ss->maxX/1000.0);
      } else
	sprintf(buf, "%10.3f / %10.3f +- sqrt(%10.3f) / %10.3f",
		ss->minX/1000.0, avg/1000.0, stdev/1000.0, ss->maxX/1000.0);
    } else
      sprintf(buf, "%10.3f once", ss->sumX/1000.0);
  } else
    sprintf(buf, "no samples");
  return buf;
}
#endif

int main(int ac, char **av)
{
  long reps = 1, i = 0, stat = 0;
  ilu_boolean mt = ilu_FALSE;

  for (i = 1;  i < ac;  i++) {
    if (strcmp(av[i], "-r") == 0) {
      if (i++ < ac)
	reps = atol(av[i]);
      else
	goto usage;
    } else if (strcmp(av[i], "-mt") == 0) {
      mt = ilu_TRUE;
    } else if (strcmp(av[i], "-st") == 0) {
      mt = ilu_FALSE;
    } else {
      goto usage;
    }
  };

  if (mt) {
#if (defined(ILU_OS_THREADED))
    ILU_C_USE_OS_THREADS;
#else
    fprintf (stderr, "OS-supplied thread support not configured into ILU!\n");
    exit(-1);
#endif				/* (defined(ILU_OS_THREADED)) */
  }

#ifdef ILU_C_TIMING_STATISTICS
  /* clear stats counter, and enable statistics gathering */
  ILU_C_SetCallStats (ilu_TRUE, ilu_TRUE);
#endif /* def ILU_C_TIMING_STATISTICS */
  
  for (i = 0;  i < reps;  i++) {
    stat = doit();
  };

#if defined(ILU_C_TIMING_STATISTICS)
  {
    ILU_C_CallStats total, latency;
    ilu_cardinal nsync, nasync;
    char buf[FMTSTATSBUFSIZE];
    ILU_C_GetCallStats (&nsync, &nasync, &total, &latency);
    OUTPUT("Timing stats (milliseconds/call):\n");
    OUTPUT("                     min /        avg +-      stdev /        max\n");
    FmtStats (buf, &total.total, total.ncalls);
    OUTPUT(" total time:  %s\n", buf);
    FmtStats (buf, &total.user, total.ncalls);
    OUTPUT("  user time:  %s\n", buf);
    FmtStats (buf, &total.system, total.ncalls);
    OUTPUT("system time:  %s\n", buf);
    FmtStats (buf, &latency.total, latency.ncalls);
    OUTPUT("    latency:  %s\n", buf);
  }
#endif /* defined(ILU_C_TIMING_STATISTICS) */

  return stat;

 usage:
  fprintf(stderr, "Usage:  %s [-mt | -st] [-r REPEATCOUNT]\n", av[0]);
  return 1;
}
