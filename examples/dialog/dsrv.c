/** $Id: dsrv.c,v 1.8 1999/08/03 01:57:26 janssen Exp $
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
/* Last edited by Mike Spreitzer October 8, 1998 10:46 pm PDT */

#include <stdio.h>
#include <string.h>

#include "Dialog.h"

Dialog_String
server_Dialog_T_M(Dialog_T t, Dialog_String s1, ILU_C_ENVIRONMENT * Env)
{
  char            buf[1000], *ans;
  ilu_cardinal    len;
  printf("Caller sez: '%s'\n", s1);
  printf("Reply: ");
  ans = fgets(buf, 1000, stdin);
  if (ans != buf) {
    fprintf(stderr, "Error reading reply!\n");
    exit(1);
  }
  len = strlen(buf);
  ans = (char *) ilu_malloc(len);
  if (ans == NULL) {
    fprintf(stderr, "Unable to malloc reply!\n");
    exit(1);
  }
  strcpy(ans, buf);
  return ans;
}

int
main(int argc, char **argv)
{
  char           *pinfo = NULL;
  ilu_string      tinfo_space[10] = {"sunrpcrm", "tcp_0_0", ILU_NIL};
  ilu_string     *tinfo = NULL;
  ILU_C_Server    s;
  Dialog_T        t;
  ilu_string      sbh, mstid;
  int             i = 1;
  ILU_C_ENVIRONMENT env;
  Dialog__InitializeServer();
  s = ILU_C_InitializeServer("DialogTest-Server", NULL, NULL, NULL,
			     NULL, ilu_FALSE);
  if (s == NULL) {
    fprintf(stderr, "Unable to create ILU server!\n");
    exit(1);
  }
  while (i < argc) {
    if (strcmp(argv[i], "-pt") == 0) {
      char           *pinfo = NULL;
      ilu_string      tinfo_space[10] = {ILU_NIL};
      ilu_string     *tinfo = tinfo_space;
      int             j = 0;
      if (i++ + 2 >= argc)
	goto usage;
      pinfo = argv[i++];
      while ((i < argc) && (j < 9) && (argv[i][0] != '-'))
	tinfo[j++] = argv[i++];
      if (!ILU_C_AddPort(s, pinfo, tinfo, ILU_NIL, ilu_FALSE, &env)) {
	fprintf(stderr, "Unable to create port (%s, %s...): %s\n",
		pinfo, tinfo[0], ILU_C_EXCEPTION_ID(&env));
	return 3;
      }
    } else if (strcmp(argv[i], "-p") == 0) {
      if (i++ < argc)
	pinfo = argv[i++];
      else
	goto usage;
    } else if (strcmp(argv[i], "-t") == 0) {
      int             j = 0;
      tinfo = tinfo_space;
      ++i;
      while ((i < argc) && (j < 9) && (argv[i][0] != '-'))
	tinfo[j++] = argv[i++];
    } else
      goto usage;
  }
  if (!ILU_C_AddPort(s, pinfo, tinfo, ILU_NIL, ilu_FALSE, &env)) {
    fprintf(stderr, "Unable to create port (%s, %s...): %s\n",
	    pinfo, tinfo[0], ILU_C_EXCEPTION_ID(&env));
    return 3;
  }
  t = Dialog_T__CreateTrue("it", s, NULL);
  if (t == NULL) {
    fprintf(stderr, "Unable to create ILU object!\n");
    exit(1);
  }
  sbh = ILU_C_SBHOfObject(t);
  printf("SBH = '%s'\n", sbh);
  ILU_C_Run();
usage:
  fprintf(stderr, "Usage: %s [-t tinfo [tinfo...]] [-p pinfo]\n",
	  argv[0]);
  return 2;
}
