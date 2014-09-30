/** $Id: srvru.c,v 1.9 1999/08/03 01:52:17 janssen Exp $
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
/* Last edited by Mike Spreitzer October 9, 1998 11:37 am PDT */

#include "srvr.h"

#include <stdio.h>
#include <math.h>
#include <string.h>

int main (int ac, char **av)
{
  char           *pinfo = NULL;
  ilu_string      tinfo_space[10] = {ILU_NIL};
  ilu_string     *tinfo = NULL, *newti = NULL;
  ilu_boolean     sec = ilu_FALSE, cred = ilu_FALSE, mt = ilu_FALSE, ssl = ilu_FALSE;
  int             i = 1;

#if INCLUDE_SSL_SECMECH
  ssl = ilu_TRUE;
#endif

  while (i < ac) {
    if (strcmp(av[i], "-p") == 0) {
      if (i++ < ac)
	pinfo = av[i++];
      else
	goto usage;
    } else if (strcmp(av[i], "-t") == 0) {
      int             j = 0;
      tinfo = tinfo_space;
      ++i;
      while ((i < ac) && (av[i][0] != '-'))
	tinfo[j++] = av[i++];
      tinfo[j] = ILU_NIL;
    } else if (strcmp(av[i], "-st") == 0) {
      mt = 0; i++;
    } else if (strcmp(av[i], "-nossl") == 0) {
      ssl = ilu_FALSE; i++;
    } else if (strcmp(av[i], "-mt") == 0) {
      mt = 1; i++;
    } else if (strcmp(av[i], "-cred") == 0) {
      cred = 1; i++;
    } else if (strcmp(av[i], "-nosec") == 0) {
      cred = 0; i++;
      tinfo = DefaultTInfo(0, ssl);
    } else if (strcmp(av[i], "-errexit") == 0) {
      ilu_SetAssertionFailureAction(1);
      ilu_SetCheckFailureAction(1);
      i++;
    } else if (strcmp(av[i], "-sec") == 0) {
      cred = 1; i++;
      newti = DefaultTInfo(1, ssl);
      if (newti == NULL) {
	fprintf(stderr, "Security support not configured into ILU!\n");
	return -1;
      } else
	tinfo = newti;
    } else
      goto usage;
  }
  return doit(pinfo, tinfo, mt, cred, ssl);
usage:
  fprintf(stderr,
	  "Usage: %s [-t tinfo [tinfo...] [-cred] | -sec | -nosec ]"
	  " [-p pinfo] [-st | -mt]\n",
	  av[0]);
  return 2;
}
