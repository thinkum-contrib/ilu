/** $Id: client.c,v 1.23 1999/08/03 01:57:04 janssen Exp $
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
/* Last edited by Mike Spreitzer October 9, 1998 1:38 pm PDT */

#include <stdlib.h>		/* for atoi */
#include <stdio.h>
#include <string.h>

#ifdef WIN32

#include <sys/types.h>
#include <sys/timeb.h>
/* dll - to pick up gethostname */
#include <winsock.h>
#include "timeit.h"

static char test_string[] = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\0";

int main(int ac, char **av)
{
  char hostname[1000], sid[1000];
  timeit_p handle;
  struct _timeb timebegin, timeend;
  char *tests[] = { "cardinal (C unsigned long)", "real (C double)", "ilu.CString (101 chars (incl NULL))", "img fetch", "matrix of doubles", "sequence of structs" };
  ilu_CString retstr;
  ilu_cardinal i, repeat, j, testtype;
  double utime, stime, cutime, cstime;
  /* xxx dll note - currently don't know how to pick out user vs. system time on NT
  struct tms beginning, ending; */
  ILU_C_ENVIRONMENT e;

  timeit__Initialize( );

  if (ac < 3)
    {
      fprintf (stderr, "Usage:  client TEST { 1=cardinal, 2=real, 3=string } REPEATCOUNT [ HOSTNAME ]\n");
      exit(1);
    }
  testtype = atoi(av[1]);
  repeat = atoi(av[2]);
  if (testtype == 0 || testtype > 3 || repeat < 1)
    {
      fprintf (stderr, "error:  testtype of %ld, repeatcount of %ld specified.\n",
	       testtype, repeat);
      exit(1);
    }
  if (ac > 3)
    strcpy (hostname, av[3]);
  else
    gethostname(hostname, sizeof(hostname));
  sprintf (sid, "TimingTest.%s.parc.xerox.com", hostname);
  if ((handle = ILU_C_LookupObject (sid, "0", timeit_p__MSType)) == NULL)
    {
      fprintf (stderr, "error:  Can't obtain object <%s>\n", sid);
      exit(1);
    }

  _ftime(&timebegin);
  switch (testtype)
    {
    case 1:

      /* times(&beginning); */
      for (i = 0;  i < repeat;  i++)
	{
	  if (j = timeit_p_ping1 (handle, i, &e), e.returnCode != NULL)
	    {
	      fprintf (stderr, "Failure %p in call %lu to ping1\n", e.returnCode, i+1);
	      exit(2);
	    }
	}
      /* times(&ending); */
      break;

    case 2:

      /* times(&beginning); */
      for (i = 0;  i < repeat;  i++)
	{
	   utime = i;
	  if (stime = timeit_p_ping2 (handle, utime, &e), e.returnCode != NULL)
	    {
	      fprintf (stderr, "Failure %p in call %lu to ping2\n", e.returnCode, i+1);
	      exit(2);
	    }
	}
      /* times(&ending); */
      break;

      break;

    case 3:

      /* times(&beginning); */
      for (i = 0;  i < repeat;  i++)
	{
	  if (retstr = timeit_p_ping3 (handle,  test_string, &e), e.returnCode != NULL)
	    {
	      fprintf (stderr, "Failure %p in call %lu to ping3\n", e.returnCode, i+1);
	      exit(2);
	    }
	}
      /* times(&ending); */
      break;

      break;

    }
  _ftime(&timeend);

  /*
  utime = ((ending.tms_utime - beginning.tms_utime) * 1000) / (repeat * 60.0);
  stime = ((ending.tms_stime - beginning.tms_stime) * 1000) / (repeat * 60.0);
  cutime = ((ending.tms_cutime - beginning.tms_cutime) * 1000) / (repeat * 60.0);
  cstime = ((ending.tms_cstime - beginning.tms_cstime) * 1000) / (repeat * 60.0);
  */

  printf ("test \"%s\", repeat %lu:\n", tests[testtype - 1], repeat);
  printf ("  elapsed time, sec per call:  %.3f      total:  %.2f sec\n",
	  ((((double) timeend.time * 1000.0) + timeend.millitm) -
	   (((double) timebegin.time * 1000.0) + timebegin.millitm))
	  / (((double) repeat) * 1000.0),
	  ((((double) timeend.time * 1000.0) + timeend.millitm) -
	   (((double) timebegin.time * 1000.0) + timebegin.millitm)) / 1000.0);
  /*
  printf ("     user time:  %3lu - %3lu = %.3f millisec/call\n", ending.tms_utime, beginning.tms_utime, utime);
  printf ("   system time:  %3lu - %3lu = %.3f millisec/call\n", ending.tms_stime, beginning.tms_stime, stime);
  */

  return 0;
}


#else /* not WIN32 */

#include <unistd.h>		/* for gethostname */
#include <math.h>		/* for sqrt */

#include <sys/time.h>
#include <sys/times.h>

#include "timeit.h"

#define ILU_TEST_DOMAIN	"parc.xerox.com"

typedef struct {
  double          minX, maxX, sumX, sumXX;
  unsigned        n;
}               StatSums;

static void AddStat(StatSums * ss, double x)
{
  if (ss->n == 0)
    ss->minX = ss->maxX = x;
  else {
    if (x > ss->maxX)
      ss->maxX = x;
    if (x < ss->minX)
      ss->minX = x;
  }
  ss->n += 1;
  ss->sumX += x;
  ss->sumXX += x * x;
  return;
}

static char    *
FmtStats(StatSums * ss)
{
  static char     buf[100];
  double          avg, stdev;
  if (ss->n > 0) {
    avg = ss->sumX / ss->n;
    if (ss->n > 1) {
      stdev = (ss->sumXX - ss->sumX * ss->sumX / ss->n) / (ss->n - 1);
      if (stdev >= 0.0) {
	stdev = sqrt(stdev);
	sprintf(buf, "%6.1f / %6.1f +- %6.1f / %6.1f",
		ss->minX, avg, stdev, ss->maxX);
      } else
	sprintf(buf, "%6.1f / %6.1f +- sqrt(%6.1f) / %6.1f",
		ss->minX, avg, stdev, ss->maxX);
    } else
      sprintf(buf, "%6.1f once", ss->sumX);
  } else
    sprintf(buf, "no samples");
  return buf;
}

#ifdef ILU_C_TIMING_STATISTICS
#define FMTSTATSBUFSIZE	512
static char    *
  FmtStats2(char buf[FMTSTATSBUFSIZE], ILU_C_CallStatSum * ss, ilu_cardinal ncalls)
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

static char    *progname = NULL;

static void
Usage()
{
  fprintf(stderr, "Usage:  %s [-mt] [-st] -test TESTTYPE [ -outercount OUTERCOUNT ] [ -innercount INNERCOUNT ] [ -hostname HOSTNAME ]\n", progname);
  fprintf(stderr, "  or :  %s [-mt] [-st] -test doctest [ -pagesize PAGESIZE ] [ -pagecount PAGECOUNT ] [ -outercount OUTERCOUNT ] [ -innercount INNERCOUNT ] [ -hostname HOSTNAME ]\n", progname);
  fprintf(stderr, "  or :  %s [-mt] [-st] -test string [ -string QUOTED-STRING ] [ -outercount OUTERCOUNT ] [ -innercount INNERCOUNT ] [ -hostname HOSTNAME ]\n", progname);
  fprintf(stderr, "  or :  %s [-mt] [-st] -test rectest [ -recseqsize RECSEQSIZE ] [ -outercount OUTERCOUNT ] [ -innercount INNERCOUNT ] [ -hostname HOSTNAME ]\n", progname);
  fprintf(stderr, "For TESTTYPE: `cardinal', `real', `imgtest'.\n");
  fprintf(stderr, "A test is repeated OUTERCOUNT*INNERCOUNT times,\n");
  fprintf(stderr, "with each block of INNERCOUNT timed.\n");
  fprintf(stderr, "INNERCOUNT and OUTERCOUNT default to 10.\n");
  fprintf(stderr, "PAGESIZE defaults to 5000 (bytes), PAGECOUNT to 50.\n"
	  "RECSEQSIZE defaults to 1000, QUOTED-STRING defaults to 50 'a' chars.\n");
  exit(1);
}

static char    *tests[] = {"cardinal (C unsigned long)", "real (C double)", "string (ilu.CString)", "doctest", "imgtest", "rectest"};

int
main(int ac, char **av)
{
  char            hostname[1000], sid[1000];
  timeit_p        handle;
  ilu_CString     retstr;
  ilu_CString	  teststring = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa";
  double          tfactor;
  timeit_recseq	  rs;
  CORBA_unsigned_long    i, outerCount = 10, k, innerCount = 10, j, recseqlen = 1000;
  CORBA_unsigned_long    pagecount = 50, pagesize = 5000;
  timeit_pageSeq *doc;
  ILU_C_ENVIRONMENT e;
  struct tms      beginning, ending;
  struct timeval  timebegin, timeend;
  StatSums        tsums = {0}, usums = {0}, ssums = {0};
  timeit_img20x20x20	val;
  ilu_boolean mt = ilu_FALSE;
  enum tests { no_Test, cardinal_Test, real_Test, string_Test, doc_Test, image_Test, record_Test };
  enum tests testtype = no_Test;

  progname = av[0];
  gethostname(hostname, sizeof(hostname));
  for (i = 1;  i < ac;  i++) {
    if (strcmp(av[i], "-test") == 0) {
      if (++i < ac) {
	if ((strcmp(av[i], "cardinal") == 0) || (strcmp(av[i], "ping1") == 0))
	  testtype = cardinal_Test;
	else if ((strcmp(av[i], "real") == 0) || (strcmp(av[i], "ping2") == 0))
	  testtype = real_Test;
	else if ((strcmp(av[i], "string") == 0) || (strcmp(av[i], "ping3") == 0))
	  testtype = string_Test;
	else if (strcmp(av[i], "doctest") == 0)
	  testtype = doc_Test;
	else if (strcmp(av[i], "imgtest") == 0)
	  testtype = image_Test;
	else if (strcmp(av[i], "rectest") == 0)
	  testtype = record_Test;
	else
	  Usage();
      } else {
	Usage();
      }
    } else if (strcmp(av[i], "-mt") == 0) {
      mt = ilu_TRUE;
    } else if (strcmp(av[i], "-st") == 0) {
      mt = ilu_FALSE;
    } else if (strcmp(av[i], "-hostname") == 0) {
      if (++i < ac) {
	strcpy (hostname, av[i]);
      } else {
	Usage();
      }
    } else if (strcmp(av[i], "-string") == 0) {
      if (++i < ac) {
	teststring = av[i];
      } else {
	Usage();
      }
    } else if (strcmp(av[i], "-innercount") == 0) {
      if ((++i < ac) && ((innerCount = atoi(av[i])) > 0))
	continue;
      else
	Usage();
    } else if (strcmp(av[i], "-outercount") == 0) {
      if ((++i < ac) && ((outerCount = atoi(av[i])) > 0))
	continue;
      else
	Usage();
    } else if (strcmp(av[i], "-pagecount") == 0) {
      if ((++i < ac) && ((pagecount = atoi(av[i])) > 0))
	continue;
      else
	Usage();
    } else if (strcmp(av[i], "-pagesize") == 0) {
      if ((++i < ac) && ((pagecount = atoi(av[i])) > 0))
	continue;
      else
	Usage();
    } else if (strcmp(av[i], "-recseqlen") == 0) {
      if ((++i < ac) && ((recseqlen = atoi(av[i])) > 0))
	continue;
      else
	Usage();
    } else {
      Usage();
    }
  };
  if (testtype == no_Test)
    Usage();

  if (mt) {
#if (defined(ILU_OS_THREADED))
    ILU_C_USE_OS_THREADS;
#else
    fprintf (stderr, "OS-supplied thread support not configured into ILU!\n");
    exit(-1);
#endif				/* (defined(ILU_OS_THREADED)) */
  }

  timeit__Initialize();
  sprintf(sid, "TimingTest.%s.%s", hostname, ILU_TEST_DOMAIN);
  handle = ILU_C_LookupObject(sid, "0", timeit_p__MSType);
  if (handle == NULL) {
    fprintf(stderr, "error:  Can't obtain object <%s>\n", sid);
    exit(1);
  }
  tfactor = 1000.0 / (innerCount * 60.0);
  if (testtype == image_Test) {
    pagecount = 1;
    for (i = 0;  i < 20;  i++)
      for (j = 0;  j < 20;  j++)
	for (k = 0;  k < 20;  k++)
	  val[i][j][k] = (CORBA_double) (pagecount++);
  };
  if (testtype == record_Test) {
    timeit_recseq_Init(&rs, recseqlen, NULL);
    rs._length = recseqlen;
  }
    
#ifdef ILU_C_TIMING_STATISTICS
  /* clear stats counter, and enable statistics gathering */
  ILU_C_SetCallStats (ilu_TRUE, ilu_TRUE);
#endif /* def ILU_C_TIMING_STATISTICS */
  
  switch (testtype) {
  case cardinal_Test:

    for (i = 0; i < outerCount; i++) {
      gettimeofday(&timebegin, NULL);
      times(&beginning);
      for (k = 0; k < innerCount; k++) {
	j = timeit_p_ping1(handle, i, &e);
	if (e.returnCode != NULL) {
	  fprintf(stderr, "Failure <%s> in call (%lu,%lu) to ping1\n",
		  e.returnCode, i, k);
	  exit(2);
	}
      }
      times(&ending);
      gettimeofday(&timeend, NULL);
      AddStat(&tsums,
	      (((timeend.tv_sec * 1.0E6 + timeend.tv_usec)
		- (timebegin.tv_sec * 1.0E6 + timebegin.tv_usec))
	       / (innerCount * 1000.0)));
      AddStat(&usums,
	      (ending.tms_utime - beginning.tms_utime) * tfactor);
      AddStat(&ssums,
	      (ending.tms_stime - beginning.tms_stime) * tfactor);
    }
    break;

  case real_Test:

    for (i = 0; i < outerCount; i++) {
      gettimeofday(&timebegin, NULL);
      times(&beginning);
      for (k = 0; k < innerCount; k++) {
	CORBA_double          x = i, y;
	y = timeit_p_ping2(handle, x, &e);
	if (e.returnCode != NULL) {
	  fprintf(stderr, "Failure <%s> in call (%lu,%lu) to ping2\n",
		  e.returnCode, i, k);
	  exit(2);
	}
      }
      times(&ending);
      gettimeofday(&timeend, NULL);
      AddStat(&tsums,
	      (((timeend.tv_sec * 1.0E6 + timeend.tv_usec)
		- (timebegin.tv_sec * 1.0E6 + timebegin.tv_usec))
	       / (innerCount * 1000.0)));
      AddStat(&usums,
	      (ending.tms_utime - beginning.tms_utime) * tfactor);
      AddStat(&ssums,
	      (ending.tms_stime - beginning.tms_stime) * tfactor);
    }
    break;

  case string_Test:

    for (i = 0; i < outerCount; i++) {
      gettimeofday(&timebegin, NULL);
      times(&beginning);
      for (k = 0; k < innerCount; k++) {
	retstr = timeit_p_ping3(handle, teststring, &e);
	if (e.returnCode != NULL) {
	  fprintf(stderr, "Failure <%s> in call (%lu,%lu) to ping3\n",
		  e.returnCode, i, k);
	  exit(2);
	}
      }
      times(&ending);
      gettimeofday(&timeend, NULL);
      AddStat(&tsums,
	      (((timeend.tv_sec * 1.0E6 + timeend.tv_usec)
		- (timebegin.tv_sec * 1.0E6 + timebegin.tv_usec))
	       / (innerCount * 1000.0)));
      AddStat(&usums,
	      (ending.tms_utime - beginning.tms_utime) * tfactor);
      AddStat(&ssums,
	      (ending.tms_stime - beginning.tms_stime) * tfactor);
    }
    break;

  case doc_Test:

    for (i = 0; i < outerCount; i++) {
      gettimeofday(&timebegin, NULL);
      times(&beginning);
      doc = timeit_p_doctest(handle,
			     "some document, possibly with search criteria",
			     pagecount, pagesize, &e);
      if (e.returnCode != NULL) {
	fprintf(stderr, "Failure <%s> in call %lu to doctest\n",
		e.returnCode, i);
	exit(2);
      } else {
	timeit_pageSeq__Free(doc);
      }
      times(&ending);
      gettimeofday(&timeend, NULL);
      AddStat(&tsums,
	      (((timeend.tv_sec * 1.0E6 + timeend.tv_usec)
		- (timebegin.tv_sec * 1.0E6 + timebegin.tv_usec))
	       / (innerCount * 1000.0)));
      AddStat(&usums,
	      (ending.tms_utime - beginning.tms_utime) * tfactor);
      AddStat(&ssums,
	      (ending.tms_stime - beginning.tms_stime) * tfactor);
    }
    break;

  case image_Test:

    val[0][0][0] = 1.1;
    val[0][0][1] = 2.2;
    val[0][0][2] = 3.3;
    for (i = 0; i < outerCount; i++) {
      gettimeofday(&timebegin, NULL);
      times(&beginning);
      for (k = 0; k < innerCount; k++) {
	timeit_p_imgtest(handle, val, &e);
	if (e.returnCode != NULL) {
	  fprintf(stderr, "Failure <%s> in call (%lu,%lu) to imgtest\n",
		  e.returnCode, i, k);
	  exit(2);
	}
      }
      times(&ending);
      gettimeofday(&timeend, NULL);
      AddStat(&tsums,
	      (((timeend.tv_sec * 1.0E6 + timeend.tv_usec)
		- (timebegin.tv_sec * 1.0E6 + timebegin.tv_usec))
	       / (innerCount * 1000.0)));
      AddStat(&usums,
	      (ending.tms_utime - beginning.tms_utime) * tfactor);
      AddStat(&ssums,
	      (ending.tms_stime - beginning.tms_stime) * tfactor);
    }
    break;

  case record_Test:

    if (recseqlen > 0) {
      strcpy(rs._buffer[0].name, "Initial");
      rs._buffer[0].dirty = ilu_TRUE;
      rs._buffer[0].create_time = 1;
      if (recseqlen > 1) {
	strcpy(rs._buffer[recseqlen-1].name, "Last");
	rs._buffer[recseqlen-1].dirty = ilu_FALSE;
	rs._buffer[recseqlen-1].create_time = recseqlen;
      }
    }
    for (i = 0; i < outerCount; i++) {
      gettimeofday(&timebegin, NULL);
      times(&beginning);
      for (k = 0; k < innerCount; k++) {
	timeit_p_rectest(handle, &rs, &e);
	if (e.returnCode != NULL) {
	  fprintf(stderr, "Failure <%s> in call (%lu,%lu) to rectest\n",
		  e.returnCode, i, k);
	  exit(2);
	}
      }
      times(&ending);
      gettimeofday(&timeend, NULL);
      AddStat(&tsums,
	      (((timeend.tv_sec * 1.0E6 + timeend.tv_usec)
		- (timebegin.tv_sec * 1.0E6 + timebegin.tv_usec))
	       / (innerCount * 1000.0)));
      AddStat(&usums,
	      (ending.tms_utime - beginning.tms_utime) * tfactor);
      AddStat(&ssums,
	      (ending.tms_stime - beginning.tms_stime) * tfactor);
    }
    break;

  }

  switch (testtype) {
  case cardinal_Test:
  case real_Test:
  case string_Test:
  case image_Test:
    printf("test \"%s\", outerCount %lu, innerCount %lu:\n",
	   tests[testtype - 1], outerCount, innerCount);
    break;
  case doc_Test:
    printf("test \"%s\", outerCount %lu, innerCount %lu, pagesize %lu, pagecount %lu:\n",
	   tests[testtype - 1], outerCount, innerCount, pagesize, pagecount);
    break;
  case record_Test:
    printf("test \"%s\", outerCount %lu, innerCount %lu, recseqlen %lu:\n",
	   tests[testtype - 1], outerCount, innerCount, recseqlen);
    break;
  default:
    break;
  }
  printf("   category:    min /    avg +-  stdev /    max milliseconds/call\n");
  printf(" total time: %s\n", FmtStats(&tsums));
  printf("  user time: %s\n", FmtStats(&usums));
  printf("system time: %s\n", FmtStats(&ssums));

#if defined(ILU_C_TIMING_STATISTICS)
  {
    ILU_C_CallStats total, latency;
    ilu_cardinal nsync, nasync;
    char buf[FMTSTATSBUFSIZE];
    ILU_C_GetCallStats (&nsync, &nasync, &total, &latency);
    printf("Timing stats using getrusage() (ms/call, %lu synch, %lu asynch):\n",
	   (unsigned long) nsync, (unsigned long) nasync);
    printf("                     min /        avg +-      stdev /        max\n");
    FmtStats2 (buf, &total.total, total.ncalls);
    printf(" total time:  %s\n", buf);
    FmtStats2 (buf, &total.user, total.ncalls);
    printf("  user time:  %s\n", buf);
    FmtStats2 (buf, &total.system, total.ncalls);
    printf("system time:  %s\n", buf);
    FmtStats2 (buf, &latency.total, latency.ncalls);
    printf("    latency:  %s\n", buf);
  }
#endif /* defined(ILU_C_TIMING_STATISTICS) */

  return 0;
}

#endif /* WIN32 */
