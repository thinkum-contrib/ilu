/**
$Id: c-clnt.c,v 1.16 1999/08/03 01:57:28 janssen Exp $
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
#include <stdlib.h>
#include <time.h>
#include <string.h>
#include <unistd.h>	/* for sleep() */
#include "ChgUp.h"

#define TIME ((unsigned long) time(ILU_NIL))

static int      always, destroy;

static int
ShowEnv(ILU_C_ENVIRONMENT * env, const char *what)
{
  switch (env->_major) {
  case ILU_C_NO_EXCEPTION:
    return 1;
  case ILU_C_USER_EXCEPTION:
    printf("%lu %s => user exn %s\n", TIME, what,
	   ILU_C_EXCEPTION_ID(env));
    break;
  case ILU_C_SYSTEM_EXCEPTION:{
      ILU_C_SYSEXN_BODY *stdbod;
      stdbod = (ILU_C_SYSEXN_BODY *) ILU_C_EXCEPTION_VALUE(env);
      printf("%lu %s => sys exn %s, minor=%lu, completed=%s\n",
	     TIME, what, ILU_C_EXCEPTION_ID(env),
	     stdbod->minor,
	     ((stdbod->completed < 3)
	      ? ilu_CompletionNames[stdbod->completed]
	      : "(invalid completion code)"));
      break;
    }
  default:
    printf("%lu %s => Invalid exception!\n", TIME, what);
  }
  CORBA_exception_free(env);
  return 0;
}

static void
ImportEm(ChgUp_T * o1, ChgUp_T * o2)
{
  int             chg;
  ILU_C_ENVIRONMENT env;
  while (1) {
    if (*o1) {
      if (destroy) {
	ILU_C_ShutdownObjectAndCloseServer(*o1, &env);
	ShowEnv(&env, "ShutdownObjectAndCloseServer(*o1)");
      } else {
	CORBA_Object_release(*o1, &env);
	ShowEnv(&env, "CORBA_Object_release(*o1)");
      }
      *o1 = ILU_NIL;
    }
    (*o1) = ILU_C_ReLookupObject("ChgUp-Server", "A",
				 ChgUp_T__MSType, &chg);
    if (*o1 == ILU_NIL) {
      printf("%lu Lookup 1 failed.\n", TIME);
    } else {
      printf("%lu Lookup 1 succeeded (chg=%d, sbh=%s).\n", TIME,
	     chg, ILU_C_SBHOfObject(*o1));
      if (*o2) {
	if (destroy) {
	  ILU_C_ShutdownObjectAndCloseServer(*o2, &env);
	  ShowEnv(&env, "ShutdownObjectAndCloseServer(*o2)");
	} else {
	  CORBA_Object_release(*o2, &env);
	  ShowEnv(&env, "CORBA_Object_release(*o2)");
	}
	*o2 = ILU_NIL;
      }
      (*o2) = ILU_C_ReLookupObject("ChgUp-Server", "B",
				   ChgUp_T__MSType, &chg);
      if (*o2 == ILU_NIL)
	printf("%lu Lookup 2 failed.\n", TIME);
      else
	printf("%lu Lookup 2 succeeded (chg=%d, sbh=%s).\n", TIME,
	       chg, ILU_C_SBHOfObject(*o2));
    }
    if (*o1 == ILU_NIL || *o2 == ILU_NIL)
      sleep(1);
    else
      return;
  }
}

static void
DestroyEm(ChgUp_T * o1, ChgUp_T * o2)
{
  ILU_C_ENVIRONMENT env;
  ILU_C_ShutdownObjectAndCloseServer(*o1, &env);
  ShowEnv(&env, "ShutdownObjectAndCloseServer(*o1)");
  *o1 = ILU_NIL;
  ILU_C_ShutdownObjectAndCloseServer(*o2, &env);
  ShowEnv(&env, "ShutdownObjectAndCloseServer(*o2)");
  *o2 = ILU_NIL;
  return;
}

static ilu_boolean mt = (
#ifdef ILU_OS_THREADED
			 ilu_TRUE
#else
			 ilu_FALSE
#endif
);
static ilu_refany theAlarm = ILU_NIL;
static int      theStop = 0;

static void SetStop(ilu_private rock)
{
  ilu_ExitMainLoop(&theStop);
}

static void MySleep(unsigned int period)
{
  if (mt)
    sleep(period);
  else {
    ilu_FineTime    now, diff, then;
    now = ilu_FineTime_Now();
    diff = ilu_FineTime_FromDouble(period);
    then = ilu_FineTime_Add(now, diff);
    ilu_SetAlarm(theAlarm, then, SetStop, ILU_NIL);
    ilu_RunMainLoop(&theStop);
  }
  return;
}

static ChgUp_T  o1 = ILU_NIL, o2 = ILU_NIL, b1, b2;

int
main(int ac, char **av)
{
  ILU_C_ENVIRONMENT env = {0};
  unsigned int    period;
  unsigned int    g1, g2, iter = 0, iterations = 2 * 1000 * 1000 * 1000;
  int             dosleep = 0, i, state = 0;
  time_t          startT;

  for (i = 1; i < ac; i++) {
    if (strcmp(av[i], "-mt") == 0)
      mt = ilu_TRUE;
    else if (strcmp(av[i], "-st") == 0)
      mt = ilu_FALSE;
    else if (av[i][0] == '-')
      goto usage;
    else
      switch (state) {
      case 0:
	if (sscanf(av[i], "%d", &always) != 1) {
	  fprintf(stderr,
	      "Conversion of \"%s\" to <always: C-bool> failed!\n",
		  av[i]);
	  fprintf(stderr, "A C-bool is a C int, in %%d format.\n");
	  return 2;
	}
	state = 1;
	break;
      case 1:
	if (sscanf(av[i], "%d", &destroy) != 1 ||
	    destroy < 0 || destroy > 2) {
	  fprintf(stderr,
		  "Conversion of \"%s\" to {0, 1, 2} failed!\n",
		  av[i]);
	  return 3;
	}
	state = 2;
	break;
      case 2:
	if (sscanf(av[i], "%u", &period) != 1) {
	  fprintf(stderr,
		  "Conversion of \"%s\" to unsigned int failed!\n",
		  av[i]);
	  return 4;
	}
	state = 3;
	break;
      case 3:
	if (sscanf(av[i], "%u", &iterations) != 1) {
	  fprintf(stderr,
		  "Conversion of \"%s\" to unsigned int failed!\n",
		  av[i]);
	  return 5;
	}
	state = 4;
	break;
      default:
	goto usage;
      }
  }
  if (state < 3)
    goto usage;
  if (mt) {
#ifdef ILU_OS_THREADED
    ILU_C_USE_OS_THREADS;
#else
    fprintf(stderr, "Multi-threading not configured into ILU!\n");
    exit(1);
#endif
  }
  ChgUp__Initialize();
  theAlarm = ilu_CreateAlarm();
  printf("ChgUp test, always=%s, destroy=%d, period=%u\n",
	 always ? "T" : "F", destroy, period);
  startT = time(ILU_NIL);
  printf("%lu = %s", (unsigned long) startT, ctime(&startT));

  if (!always)
    ImportEm(&o1, &o2);
  while (1) {
    if (dosleep)
      MySleep(period);
    printf("\n%lu At top of loop.\n", TIME);
    if (always) {
      int             ok;
      if (destroy == 1 && o1 != ILU_NIL && o2 != ILU_NIL) {
	ok = ILU_C_ValidateOrCloseObjSvr(o1, &env);
	ShowEnv(&env, "ValidateOrCloseObjSvr(o1)");
	if (ok) {
	  ok = ILU_C_ValidateOrCloseObjSvr(o2, &env);
	  ShowEnv(&env, "ValidateOrCloseObjSvr(o2)");
	  if (!ok)
	    o2 = ILU_NIL;
	} else
	  o1 = ILU_NIL;
      } else
	ok = 0;
      if (!ok)
	ImportEm(&o1, &o2);
    }
    b1 = ChgUp_T_GetBrother(o1, &env);
    if (!ShowEnv(&env, "GetBrother(o1)"))
      goto failed;
    b2 = ChgUp_T_GetBrother(o2, &env);
    if (!ShowEnv(&env, "GetBrother(o2)"))
      goto failed;
    g1 = ChgUp_T_GetGeneration(o1, &env);
    if (!ShowEnv(&env, "GetGeneration(o1)"))
      goto failed;
    g2 = ChgUp_T_GetGeneration(o2, &env);
    if (!ShowEnv(&env, "GetGeneration(o2)"))
      goto failed;
    printf("\n%lu Iteration %lu: g1=%lu, g2=%lu",
	   TIME, iter++, g1, g2);
    if (b1 != o2 || b2 != o1)
      printf(" (not brothers!)");
    printf("\n");
    if (iter >= iterations)
      break;
    CORBA_Object_release(b1, &env);
    if (!ShowEnv(&env, "CORBA_Object_release(b1)"))
      goto failed;
    b1 = ILU_NIL;
    CORBA_Object_release(b2, &env);
    if (!ShowEnv(&env, "CORBA_Object_release(b2)"))
      goto failed;
    b2 = ILU_NIL;
    if (always && destroy == 2)
      DestroyEm(&o1, &o2);
    dosleep = 1;
    continue;
failed:
    dosleep = 0;
    if (always)
      continue;
    if (destroy)
      DestroyEm(&o1, &o2);
    ImportEm(&o1, &o2);
  }
  return 0;
usage:
  fprintf(stderr,
	  "Usage: %s [-mt | -st] || <always: C-bool>"
	  " <destroy: {0, 1, 2}> <period: unsigned (secs)>"
	  " [iterations: unsigned]\n",
	  av[0]);
  return 1;
}
