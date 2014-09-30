/** $Id: server.c,v 1.7 1999/08/03 01:58:51 janssen Exp $
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
/* Last edited by Mike Spreitzer October 9, 1998 1:42 pm PDT */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <unistd.h>	/* for gethostname */
#include "w3muxtest.h"
#include <ilutransport.h>

typedef ilu_boolean (*ForkProc)(void (*)(void *arg), void *arg, ilu_Error *);

#ifndef HAVE_GETHOSTNAME_PROTOTYPE
extern int gethostname(char *name, int namelen);
#endif /* HAVE_GETHOSTNAME_PROTOTYPE */

#define ILU_TEST_DOMAIN "parc.xerox.com"
ilu_boolean verbose = ilu_FALSE;

static void usage (int ac, char **av, int i)
{
  fprintf (stderr, "Don't understand argument %d:  \"%s\".\n", i+1, av[i]);
  fprintf (stderr, "Usage:  %s [-mt] [-v] [-hostname HOSTNAME] [-p PROTOCOL] [-t TRANSPORTS...]\n",
	   av[0]);
  fprintf (stderr, "  Default:  %s -p sunrpc -t w3mux_7 tcp_0_0\n", av[0]);
  exit(1);
}

int main (int ac, char **av)
{
  extern void Test1__InitializeServer(void);
  ilu_Error err;
  ILU_C_Server s;
  w3muxtest_p  uc;
  char *pinfo = "sunrpc";
  char *tinfo[10] = { "w3mux_7", "tcp_0_0", ILU_NIL };
  char hostname[1000], serverid[1000];
  ilu_boolean threaded = ilu_FALSE, ans = ilu_TRUE;
  int i = 1;

  gethostname (hostname, sizeof(hostname));

  while (i < ac) {
    if (strcmp(av[i], "-p") == 0) {
      if (i++ < ac)
	pinfo = av[i++];
      else
	usage(ac, av, i);
    } else if (strcmp(av[i], "-t") == 0) {
      int j = 0;
      ++i;
      while ((i < ac) && (av[i][0] != '-'))
	tinfo[j++] = av[i++];
      tinfo[j] = ILU_NIL;
    } else if (strcmp(av[i], "-v") == 0) {
      verbose = ilu_TRUE;
      i++;
    } else if (strcmp(av[i], "-hostname") == 0) {
      strcpy (hostname, av[++i]);
      i++;
#ifdef ILU_OS_THREADED
    } else if (strcmp(av[i], "-mt") == 0) {
      threaded = ilu_TRUE;
      i++;
#endif
    } else
      usage(ac, av, i);
  }

#ifdef ILU_OS_THREADED
  if (threaded) {
    ILU_C_USE_OS_THREADS;
  };
#endif

  w3muxtest__Initialize();
  w3muxtest__InitializeServer();

  sprintf (serverid, "w3muxtest.1.%s.%s", hostname, ILU_TEST_DOMAIN);
  s = ILU_C_InitializeServer (serverid, ILU_NIL, pinfo, tinfo, ILU_NIL, ilu_TRUE);
  if (s == ILU_NIL) {
    fprintf(stderr, "Can't create server.\n");
    exit(1);
  };    
  uc = w3muxtest_p__CreateTrue ( "0", s, ILU_NIL );
  if (uc != ILU_NIL) {
    ILU_C_PublishObject (uc);
    printf ("exported %s\n", ILU_C_SBHOfObject(uc));
    ILU_C_Run( );
  } else {
    printf ("Couldn't create true instance.\n");
    exit(1);
  }
  return 1;
}

ilu_cardinal
  server_w3muxtest_p_ping1 (w3muxtest_p h, CORBA_unsigned_long p1, ILU_C_ENVIRONMENT *e)
{
  if (verbose)
    printf ("w3muxtest_p_ping1(%lu) => %lu\n", p1, p1);
  return (p1 * p1);
}

void
  server_w3muxtest_p_send (w3muxtest_p h, w3muxtest_p other, ILU_C_ENVIRONMENT *e)
{
  if (verbose)
    printf ("w3muxtest_p_send(%s)\n", ILU_C_SBHOfObject(other));
  w3muxtest_p_ping1 (other, 37, e);
}
