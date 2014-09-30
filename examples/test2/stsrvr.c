/** $Id: stsrvr.c,v 1.5 1999/08/03 01:57:40 janssen Exp $
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
/* Last edited by Mike Spreitzer October 9, 1998 12:08 pm PDT */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>	/* for exit() */
#include <unistd.h>

#include "SerialTest.h"

#ifdef _IS_POSIX
#define ILU_OSLEEP	sleep
#elif (defined(WIN32) || defined (WIN16))
#define ILU_OSLEEP	_sleep
#else
#error "Ack!  Don't know how to sleep a thread!"
#endif

#define ilu_Alarm ilu_refany

static int verbose = 0;

void
server_SerialTest_T_Prep(SerialTest_T self,
			 ilu_cardinal nthr, ilu_cardinal n,
			 ILU_C_ENVIRONMENT * Env)
{
  ILU_C_SET_SUCCESSFUL(Env);
  return;
}

void
server_SerialTest_T_M(SerialTest_T self,
		      ilu_cardinal series, ilu_cardinal i,
		      ILU_C_ENVIRONMENT * Env)
{
  ilu_FineTime    now;
  now = ilu_FineTime_Now();
  printf("M(%u, %u)      at      %lu:%06lu\n", series, i,
	 (long unsigned) now.ft_s, (long unsigned) now.ft_t);
  fflush(stdout);
  if (i > 1000) {
    ILU_OSLEEP(i - 1000);
    now = ilu_FineTime_Now();
    printf("M(%u, %u) returning at %lu:%06lu\n", series, i,
	   (long unsigned) now.ft_s, (long unsigned) now.ft_t);
    fflush(stdout);
  }
  ILU_C_SET_SUCCESSFUL(Env);
  return;
}

void
server_SerialTest_T_N(SerialTest_T self,
		      ilu_cardinal series, ilu_cardinal i,
		      ILU_C_ENVIRONMENT * Env)
{
  ilu_FineTime    now;
  now = ilu_FineTime_Now();
  printf("N(%u, %u)      at      %lu:%06lu\n", series, i,
	 (long unsigned) now.ft_s, (long unsigned) now.ft_t);
  fflush(stdout);
  if (i > 1000) {
    ILU_OSLEEP(i - 1000);
    now = ilu_FineTime_Now();
    printf("N(%u, %u) returning at %lu:%06lu\n", series, i,
	   (long unsigned) now.ft_s, (long unsigned) now.ft_t);
    fflush(stdout);
  }
  ILU_C_SET_SUCCESSFUL(Env);
  return;
}

static ilu_Alarm theAlarm;
static ilu_FineTime periodFT = {45, 0};

static void PerAlarm(ilu_private rock)
{
  ilu_cardinal    fdb;
  printf("\n");
  fdb = ilu_SetFDBudget(0);
  printf("%lu FD usage = %lu\n", ((unsigned long) time(ILU_NIL)), fdb);
  fdb = ilu_SetFDBudget(32);
  ilu_SetAlarm(theAlarm, ilu_FineTime_Add(ilu_FineTime_Now(), periodFT),
	       PerAlarm, ILU_NIL);
}

int
main(int argc, char **argv)
{
  ILU_C_Server    s;
  SerialTest_T    t;
  int             i;
  ilu_string      progname, sbh, pinfo = NULL;
  ilu_string      tinfo[10] = {"sunrpcrm", "tcp_0_0", NULL};

  SerialTest__InitializeServer();
  theAlarm = ilu_CreateAlarm();
  progname = argv[0];
  argv++;
  argc--;
  for (i = 0; i < argc; i++) {
    if (strcmp(argv[i], "-p") == 0 && ((i + 1) < argc))
      pinfo = argv[++i];
    else if (strcmp(argv[i], "-t") == 0 && ((i + 1) < argc)) {
      int             j = 0;
      ++i;
      while ((i < argc) && (argv[i][0] != '-'))
	tinfo[j++] = argv[i++];
      tinfo[j] = NULL;
    } else if (strcmp(argv[i], "-v") == 0)
      verbose = 1;
    else
      goto usage;
  }
  s = ILU_C_InitializeServer("SerialTest-Server", NULL, pinfo, tinfo,
			     ILU_NIL, ilu_TRUE);
  if (s == NULL) {
    fprintf(stderr, "Unable to create ILU server!\n");
    exit(1);
  }
  t = SerialTest_T__CreateTrue("it", s, NULL);
  if (t == NULL) {
    fprintf(stderr, "Unable to create ILU object!\n");
    exit(1);
  }
  if (ILU_C_PublishObject(t) == NULL) {
    fprintf(stderr, "Can't publish object!\n");
    exit(1);
  }
  sbh = ILU_C_SBHOfObject(t);
  printf("SBH = '%s'\n", sbh);
  ilu_SetAlarm(theAlarm, ilu_FineTime_Add(ilu_FineTime_Now(), periodFT),
	       PerAlarm, ILU_NIL);
  ILU_C_Run();
usage:
  fprintf(stderr,
	  "Usage: %s [-v] [-p pinfo] [-t tinfo [tinfo...]]\n",
	  progname);
  return 1;
}
