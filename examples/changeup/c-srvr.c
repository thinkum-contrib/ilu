/**
$Id: c-srvr.c,v 1.18 1999/08/03 01:57:28 janssen Exp $
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
/* Last edited by Mike Spreitzer October 8, 1998 10:36 pm PDT */

#include <stdio.h>
#include <string.h>
#include <time.h>
#include <unistd.h>	/* for sleep() */
#include <stdlib.h>	/* for exit() */
#include "ChgUp.h"

#define TIME ((unsigned long) time(ILU_NIL))

static unsigned int period;
static int      style;
/*
 * 0 = squeeze FD budget; 1 = change ports; 2 = close & restart
 * server
 */

static ilu_cardinal generation = 0;
static unsigned iterations = 2 * 1000 * 1000 * 1000;
static ILU_C_Server theServer = ILU_NIL;
static ChgUp_T  o1 = ILU_NIL, o2 = ILU_NIL;

static int      theStop = 0;
static ilu_refany theAlarm = ILU_NIL;
static ilu_Condition sleepCond = ILU_NIL;
static ilu_Mutex sleepMu = ILU_NIL;
static ilu_boolean mt = (
#ifdef ILU_OS_THREADED
			 ilu_TRUE
#else
			 ilu_FALSE
#endif
);

static void Wake(ilu_private rock)
{
  ILU_C_StopRun(&theStop);
}

static void MySleep(unsigned int period)
{
  ilu_FineTime    now, diff, then;
  now = ilu_FineTime_Now();
  diff = ilu_FineTime_FromDouble(period);
  then = ilu_FineTime_Add(now, diff);
  if (mt) {
    ilu_Error       lerr;
    ilu_AcquireMutex(sleepMu);
    ilu_CMWait1TO(sleepCond, sleepMu, &then, &lerr);
    ILU_MUST_BE_SUCCESS(lerr);
    ilu_ReleaseMutex(sleepMu);
  } else {
    ilu_SetAlarm(theAlarm, then, Wake, ILU_NIL);
    ILU_C_StoppableRun(&theStop);
  }
  return;
}

ChgUp_T
server_ChgUp_T_GetBrother(ChgUp_T self, ILU_C_ENVIRONMENT * env)
{
  ChgUp_T         b = (ChgUp_T) ChgUp_T__GetUserData(self);
  return CORBA_Object_duplicate(b, env);
}

ilu_cardinal
server_ChgUp_T_GetGeneration(ChgUp_T self, ILU_C_ENVIRONMENT * env)
{
  ILU_C_SET_SUCCESSFUL(env);
  return generation;
}

static char    *pinfo = ILU_NIL;
static char *  *tinfo = ILU_NIL;

static void CreateEm()
{
  ilu_string      p1 = ILU_NIL, p2 = ILU_NIL;
  theServer = ILU_C_InitializeServer("ChgUp-Server", ILU_NIL,
				     pinfo, tinfo, ILU_NIL,
				     ilu_TRUE);
  o1 = ChgUp_T__CreateTrue("A", theServer, ILU_NIL);
  o2 = ChgUp_T__CreateTrue("B", theServer, o1);
  ChgUp_T__SetUserData(o1, o2);
  p1 = ILU_C_PublishObject(o1);
  p2 = ILU_C_PublishObject(o2);
  ilu_free(p1);
  ilu_free(p2);
  printf("A's SBH is %s\n", ILU_C_SBHOfObject(o1));
}

int
main(int ac, char **av)
{
  time_t          startT;
  char           *progname = av[0];
  char           *newtinfo[10] = {ILU_NIL};
  int             state = 0;
  av++;
  ac--;
  while (ac) {
    if (av[0][0] == '-') {
      if (strcmp(av[0], "-mt") == 0)
	ac--, av++, mt = ilu_TRUE;
      else if (strcmp(av[0], "-st") == 0)
	ac--, av++, mt = ilu_FALSE;
      else if (strcmp(av[0], "-p") == 0)
	pinfo = av[1], ac -= 2, av += 2;
      else if (strcmp(av[0], "-t") == 0) {
	int             j = 0;
	av++;
	ac--;
	tinfo = newtinfo;
	while (ac && av[0][0] != '-') {
	  if (j >= 9) {
	    fprintf(stderr,
		"Sorry, tinfo must have no more than 9 layers.\n");
	    exit(1);
	  }
	  newtinfo[j++] = av[0];
	  av++;
	  ac--;
	};
	newtinfo[j] = ILU_NIL;
      } else
	goto usage;
    } else {
      switch (state++) {
      case 0:
	if (sscanf(av[0], "%d", &style) != 1 || style < 0 || style > 2) {
	  fprintf(stderr,
		  "Conversion of \"%s\" to {0, 1, 2} failed!\n",
		  av[0]);
	  return 2;
	}
	break;
      case 1:
	if (sscanf(av[0], "%u", &period) != 1) {
	  fprintf(stderr,
		  "Conversion of \"%s\" to unsigned int failed!\n",
		  av[0]);
	  return 3;
	}
	break;
      case 2:
	if (sscanf(av[0], "%u", &iterations) != 1) {
	  fprintf(stderr,
		  "Conversion of \"%s\" to unsigned int failed!\n",
		  av[0]);
	  return 4;
	}
	break;
      default:
	goto usage;
      }
      av++;
      ac--;
    }
  }
  if (state < 2)
    goto usage;
  if (mt) {
#ifdef ILU_OS_THREADED
    ILU_C_USE_OS_THREADS;
#else
    fprintf(stderr, "Multi-threading not configured into ILU!\n");
    exit(1);
#endif
  }
  ChgUp__InitializeServer();
  CreateEm();
  theAlarm = ilu_CreateAlarm();
  if (mt) {
    ilu_Error       lerr;
    sleepMu = ilu_CreateMutex("changeup", "sleep mu");
    sleepCond = ilu_CreateCondition("changeup", "sleep cond", &lerr);
    ILU_MUST_BE_SUCCESS(lerr);
  }
  printf("ChgUp server, style=%d, period=%u, mt=%d\n",
	 style, period, mt);
  startT = time(ILU_NIL);
  printf("%lu = %s", (unsigned long) startT, ctime(&startT));
  for (; generation < iterations; generation++) {
    ilu_cardinal    fdb;
    ILU_C_ENVIRONMENT env;
    MySleep(period);
    switch (style) {
    case 0:
      printf("\n");
      fdb = ilu_SetFDBudget(0);
      printf("%lu FD usage = %lu\n", TIME, fdb);
      fdb = ilu_SetFDBudget(32);
      break;
    case 1:
      fprintf(stderr, "Style 1 not implementable yet!\n");
      exit(1);
      break;
    case 2:
      printf("\n%lu Closing server...\n", TIME);
      ILU_C_CloseServer(theServer, ilu_TRUE, ILU_NIL, ILU_NIL, ILU_NIL,
			&env);
      if (!ILU_C_SUCCESSFUL(&env))
	printf("%lu close => exn %s\n", TIME, ILU_C_EXCEPTION_ID(&env));
      ILU_C_Server_release(theServer, &env);
      if (!ILU_C_SUCCESSFUL(&env))
	printf("%lu ILU_C_Server_release => exn %s\n", TIME,
	       ILU_C_EXCEPTION_ID(&env));
      CreateEm();
      printf("\n%lu Server restarted\n", TIME);
      break;
    default:
      fprintf(stderr, "Unexpected style %d\n", style);
    }
  }
  return -1;
usage:
  fprintf(stderr,
	  "Usage: %s [-mt|-st] || [-p <pinfo>] || [-t <tinfo>] ||"
	  " <style: {0, 1, 2}> <period: unsigned (secs)>"
	  " [iterations]\n",
	  progname);
  return 1;
}
