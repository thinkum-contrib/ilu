/** $Id: server.c,v 1.20 1999/08/03 01:57:05 janssen Exp $
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
/* Last edited by Mike Spreitzer October 9, 1998 1:40 pm PDT */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#ifdef WIN32
/* dll - to pick up gethostname */
#include <winsock.h>
#include "timeit.h"
#else
#include <unistd.h>	/* for gethostname */
#include "timeit.h"
#endif /* WIN32 */

#define ILU_TEST_DOMAIN "parc.xerox.com"
ilu_boolean verbose = ilu_FALSE;

static void usage() {
  fprintf (stderr, "Usage:  timeit-server [-verbose] [-host HOSTNAME] [-p PROTOCOL] [-t TINFO]\n");
}

int main (int ac, char **av)
{
  extern void Test1__InitializeServer(void);
  ilu_boolean mt = ilu_FALSE;		/* true for multi-threaded */
  ILU_C_Server s;
  timeit_p  uc;
  int i;
  char *pinfo = "sunrpc";
  char *tinfo[10] = { "sunrpcrm", "tcp_0_0", ILU_NIL };
  char hostname[1000], serverid[1000];

  gethostname (hostname, sizeof(hostname));

  for (i = 1;  i < ac; ) {
    if (strcmp(av[i], "-p") == 0) {
      if (i++ < ac)
	pinfo = av[i++];
      else {
	usage();
	exit(1);
      }	
    } else if (strcmp(av[i], "-t") == 0) {
      int             j = 0;
      ++i;
      while ((i < ac) && (av[i][0] != '-'))
	tinfo[j++] = av[i++];
      tinfo[j] = ILU_NIL;
    } else if (strcmp(av[i], "-st") == 0) {
      mt = 0; i++;
    } else if (strcmp(av[i], "-mt") == 0) {
      mt = 1; i++;
    } else if (strcmp(av[i], "-host") == 0) {
      i++;
      strcpy (hostname, av[i]);
      i++;
    } else if (strcmp(av[i], "-verbose") == 0) {
      verbose = ilu_TRUE;
      i++;
    } else {
      usage();
      exit(1);
    }
  }

  if (mt) {
#if (defined(ILU_OS_THREADED))
    ILU_C_USE_OS_THREADS;
#else
    fprintf (stderr, "OS-supplied thread support not configured into ILU!\n");
    exit(-1);
#endif				/* (defined(ILU_OS_THREADED)) */
  }

  timeit__InitializeServer();

  sprintf (serverid, "TimingTest.%s.%s", hostname, ILU_TEST_DOMAIN);
  s = ILU_C_InitializeServer (serverid, ILU_NIL, pinfo, tinfo, ILU_NIL, ilu_TRUE);
  uc = timeit_p__CreateTrue ( "0", s, ILU_NIL );
  ILU_C_PublishObject (uc);

  if (uc != ILU_NIL)
    {
      printf ("exported %s\n", ILU_C_SBHOfObject(uc));
      ILU_C_Run( );
    }
  else
    {
      printf ("couldn't create object\n");
      exit(1);
    }
  return 1;
}

ilu_cardinal
  server_timeit_p_ping1 (timeit_p h, CORBA_unsigned_long p1, ILU_C_ENVIRONMENT *e)
{
  if (verbose)
    printf ("timeit_p_ping1(%lu) => %lu\n", p1, p1);
  return (p1);
}

ilu_real
  server_timeit_p_ping2 (timeit_p h, CORBA_double p1, ILU_C_ENVIRONMENT *e)
{
  if (verbose)
    printf ("timeit_p_ping2(%f) => %f\n", p1, p1);
  return (p1);
}

ilu_CString
  server_timeit_p_ping3 (timeit_p h, ilu_CString p1, ILU_C_ENVIRONMENT *e)
{
  ilu_cardinal    len;
  ilu_CString     ans;
  if (verbose)
    printf("timeit_p_ping3(\"%s\") => \"%s\"\n", p1, p1);
  len = strlen(p1);
  ans = (ilu_CString) ilu_malloc(len + 1);
  if (ans != ILU_NIL)
    strcpy(ans, p1);
  return (ans);
}

static timeit_pageimg pageimg_Create (ilu_cardinal size)
{
  timeit_pageimg *p = timeit_pageimg__alloc();
  p->_maximum = size;
  p->_length = size;
  p->_buffer = CORBA_sequence_octet_allocbuf(size);
  memset(p->_buffer, (int) 'a', size);
  return *p;
}

timeit_pageSeq *
server_timeit_p_doctest(timeit_p h, ilu_CString docname,
			CORBA_unsigned_long pagecount,
			CORBA_unsigned_long pagesize,
			ILU_C_ENVIRONMENT * e)
{
  timeit_pageSeq *p = timeit_pageSeq__alloc();
  ilu_cardinal    i;

  timeit_pageSeq_Init(p, pagecount, ILU_NIL);

  for (i = 0; i < pagecount; i++)
    p->_buffer[i] = pageimg_Create(pagesize);
  p->_length = pagecount;

  if (verbose)
    printf("timeit_p_doctest(\"%s\", %u, %u) => %p\n",
	   docname, pagecount, pagesize, p);

  return (p);
}

void
  server_timeit_p_imgtest (timeit_p h,
			   timeit_img20x20x20 val,
			   ILU_C_ENVIRONMENT *e)
{
  if (verbose)
    printf("timeit_p_imgtest(%f %f %f ... %f %f)\n",
	   val[0][0][0], val[0][0][1], val[0][0][2], val[19][19][18], val[19][19][19]);
  ILU_C_SET_SUCCESSFUL(e);
}

void
  server_timeit_p_rectest (timeit_p h,
			   timeit_recseq* val,
			   ILU_C_ENVIRONMENT *e)
{
  timeit_rec p;
  if (verbose) {
    printf("timeit_p_rectest(%lu", timeit_recseq_Length(val));
    if (timeit_recseq_Length(val) > 0) {
      p = *timeit_recseq_Nth(val, 0);
      printf(":  (\"%.32s\", %s, %lu)", p.name, (p.dirty ? "dirty" : "clean"), p.create_time);
      if (timeit_recseq_Length(val) > 1) {
	p = *timeit_recseq_Nth(val, timeit_recseq_Length(val) - 1);
	printf(" ... (\"%.32s\", %s, %lu)", p.name, (p.dirty ? "dirty" : "clean"), p.create_time);
      }
    }
    printf(")\n");
  }
  ILU_C_SET_SUCCESSFUL(e);
}


