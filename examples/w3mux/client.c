/** $Id: client.c,v 1.12 1999/08/03 01:58:50 janssen Exp $
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

#include <unistd.h>		/* for gethostname */
#include <math.h>		/* for sqrt */

#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>
#include <sys/times.h>
#include <string.h>

#include "w3muxtest.h"
#include <ilutransport.h>

typedef ilu_boolean (*ForkProc)(void (*)(void *arg), void *arg, ilu_Error *);

#ifndef HAVE_GETHOSTNAME_PROTOTYPE
extern int gethostname(char *name, int namelen);
#endif /* HAVE_GETHOSTNAME_PROTOTYPE */

#define ILU_TEST_DOMAIN	"parc.xerox.com"

static char    *progname = NULL;

static void usage (int ac, char **av, int i)
{
  if (av[i] != NULL)
    fprintf (stderr, "Don't understand argument %d:  \"%s\".\n", i+1, av[i]);
  fprintf (stderr, "Usage:  %s -url URL [-n COUNT] "
#ifdef ILU_OS_THREADED
	   "[-mt] "
#endif
	   "[-p PROTOCOL] [-t TRANSPORT...] \n", av[0]);
  fprintf (stderr, "  Default:  %s -n 1\n", av[0]);
  exit(1);
}

int
main(int ac, char **av)
{
  char *	url = ILU_NIL;
  ilu_Error	err;
  w3muxtest_p	handle, uc;
  int		i, count = 3;
  char *	pinfo = "sunrpc";
  char *	tinfo[10] = { "w3mux_0", "tcp_0_0", ILU_NIL };
  ilu_boolean	threaded = ilu_FALSE, ans = ilu_TRUE;
  ILU_C_Server	s3;
  ILU_C_ENVIRONMENT e;
  ilu_Passport	passport;
  ilu_IdentityInfo	endpoint;
  ilu_cardinal	val1, val2, timeout = 0;

  i = 1;
  while (i < ac) {
    if (strcmp(av[i], "-p") == 0) {
      if (i++ < ac)
	pinfo = av[i++];
      else
	usage(ac, av, i);
    } else if (strcmp(av[i], "-n") == 0) {
      if (++i < ac)
	count = atoi(av[i++]);
      else
	usage(ac, av, i);
    } else if (strcmp(av[i], "-b") == 0) {
      if (++i < ac)
	timeout = atoi(av[i++]);
      else
	usage(ac, av, i);
    } else if (strcmp(av[i], "-url") == 0) {
      if (++i < ac)
	url = av[i++];
      else
	usage(ac, av, i);
    } else if (strcmp(av[i], "-t") == 0) {
      int j = 0;
      ++i;
      while ((i < ac) && (av[i][0] != '-'))
	tinfo[j++] = av[i++];
      tinfo[j] = ILU_NIL;
#ifdef ILU_OS_THREADED
    } else if (strcmp(av[i], "-mt") == 0) {
      threaded = ilu_TRUE;
      i++;
#endif
    } else
      usage(ac, av, i);
  }

  if (url == ILU_NIL) {
    fprintf (stderr, "Must specify URL of server.\n");
    usage (ac, av, i);
  };    

#ifdef ILU_OS_THREADED
  if (threaded) {
    ILU_C_USE_OS_THREADS;
  };
#endif

  w3muxtest__Initialize();
  w3muxtest__InitializeServer();
  progname = av[0];

  passport = ILU_C_CreatePassport (&e);
  if (!ILU_C_SUCCESSFUL(&e)) {
    fprintf(stderr, "Can't create passport.\n");
    exit(1);
  };    
  endpoint = ILU_C_AcquireW3muxEndpointIdentity (ILU_NIL, &e);
  if (!ILU_C_SUCCESSFUL(&e)) {
    fprintf(stderr, "Can't acquire identity for default endpoint UUID.\n");
    exit(1);
  };    
  ILU_C_AddIdentity (passport, endpoint, &e);
  if (!ILU_C_SUCCESSFUL(&e)) {
    fprintf(stderr, "Can't add endpoint identity to passport.\n");
    exit(1);
  };    
  ILU_C_SetPassportContext (passport);
  
  if (timeout != 0) {
    double d = timeout / 1000.0;
    ilu_FineTime ft;
    ILU_C_Batcher b;
    ft = ilu_FineTime_FromDouble(d);
    b = ILU_C_CreateBatcher (ft, ilu_TRUE, &e);
    if (!ILU_C_SUCCESSFUL(&e)) {
      fprintf(stderr, "Can't create batcher with period of %lu milliseconds.\n",
	      (unsigned long) timeout);
      exit(1);
    };
    ILU_C_SetBatcherContext (b);
  }

  s3 = ILU_C_InitializeServer (ILU_NIL, ILU_NIL, pinfo, tinfo, passport, ilu_TRUE);
  if (s3 == ILU_NIL) {
    fprintf(stderr, "Can't create server 3.\n");
    exit(1);
  };    
  uc = w3muxtest_p__CreateTrue ( ILU_NIL, s3, ILU_NIL );

  handle = ILU_C_SBHToObject(url, w3muxtest_p__MSType, &e);
  if (! ILU_C_SUCCESSFUL(&e)) {
    fprintf(stderr, "error:  Can't obtain object <%s>\n", url);
    exit(1);
  }

  if (val1 = 32, val2 = w3muxtest_p_ping1(handle, val1, &e), val2 != (val1 * val1))
    fprintf (stderr, "*** Bad val %d returned from ping1(%d)\n", val2, val1);

  for (i = 0;  i < count;  i++) {
    if (val1 = 456, val2 = w3muxtest_p_ping1(handle, val1, &e), val2 != (val1 * val1))
      fprintf (stderr, "*** Bad val %d returned from ping1(%d)\n", val2, val1);
    
    w3muxtest_p_send(handle, uc, &e);

    if (val1 = 7, val2 = w3muxtest_p_ping1(handle, val1, &e), val2 != (val1 * val1))
      fprintf (stderr, "*** Bad val %d returned from ping1(%d)\n", val2, val1);
  }

#if (defined(TCPIP_TRANSPORT) && defined(ENABLE_DEBUGGING))
  {
    ilu_cardinal bytes_sent;
    ilu_cardinal bytes_received;
    ilu_cardinal moorings_created;
    ilu_cardinal connections_received;
    ilu_cardinal connections_created;
    ilu_cardinal current_connections;
    ilu_cardinal max_connections;
    ilu_tcp_GetStats (&bytes_sent,
		      &bytes_received,
		      &moorings_created,
		      &connections_received,
		      &connections_created,
		      &current_connections,
		      &max_connections);
    fprintf (stdout, "TCP/IP usage stats:\n"
	     "  bytes sent:  %lu\n"
	     "  bytes read:  %lu\n"
	     "  moorings created:  %lu\n"
	     "  connections accepted:  %lu\n"
	     "  connections opened:  %lu\n"
	     "  currently open connections:  %lu\n"
	     "  max connections open during test:  %lu\n",
	     bytes_sent,
	     bytes_received,
	     moorings_created,
	     connections_received,
	     connections_created,
	     current_connections,
	     max_connections);
  }
#endif

  return 0;
}


ilu_cardinal
  server_w3muxtest_p_ping1 (w3muxtest_p h, CORBA_unsigned_long p1, ILU_C_ENVIRONMENT *e)
{
  return (p1);
}

void
  server_w3muxtest_p_send (w3muxtest_p h, w3muxtest_p p1, ILU_C_ENVIRONMENT *e)
{
  w3muxtest_p_ping1 (p1, 37, e);
}
