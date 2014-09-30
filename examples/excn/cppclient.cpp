/** $Id: cppclient.cpp,v 1.6 1999/08/06 02:15:12 janssen Exp $
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
/* Last edited by Mike Spreitzer October 8, 1998 10:48 pm PDT */

#include <unistd.h>		/* for gethostname */
#include <math.h>		/* for sqrt */

#include <stdlib.h>
#include <sys/time.h>
#include <sys/times.h>
#include <string.h>
#include <stdio.h>

#include <stdio.h>
#include <string.h>
#include <math.h>
#include <stdlib.h>	/* for exit() */

#include "ExcnTest-cpp.hpp"

#define STRINGIFIED_DOMAIN(x)	#x
#define STRINGIFIED_DOMAIN1(x)	STRINGIFIED_DOMAIN(x)
#define THIS_DOMAIN		STRINGIFIED_DOMAIN1(FULLY_QUALIFIED_DOMAIN_NAME)

#ifndef HAVE_GETHOSTNAME_PROTOTYPE
extern int gethostname(char *name, int namelen);
#endif /* HAVE_GETHOSTNAME_PROTOTYPE */

static char    *progname = NULL;

static void usage (int ac, char **av, int i)
{
  fprintf (stderr, "Don't understand argument %d:  \"%s\".\n", i+1, av[i]);
  fprintf (stderr, "Usage:  %s [-mt] [-nosys]\n", av[0]);
  fprintf (stderr, "  Default:  %s\n", av[0]);
  exit(1);
}

static void Print_CSS(const ExcnTest(CSS)& css)
{
  int             i;
  const char *s;

  printf("<");
  for (i = 0; i < css.length(); i++) {
    s = css[i];
    printf("%s\"%s\"", (i ? ", " : ""), s);
  }
  printf(">");
}

static void Print_A0(const ExcnTest(A0) a0)
{
  int i;

  printf("{");
  for (i = 0;  i < 8;  i++)
    printf("%s%u", ((i == 0) ? "" : ", "), a0[i]);
  printf("}");
}

static void Print_A1(const ExcnTest(A1) a1)
{
  int             i, j;
  printf("{");
  for (i = 0; i < 3; i++) {
    printf("{");
    for (j = 0; j < 4; j++) {
      printf("%s%d", ((j == 0) ? "" : ", "), a1[i][j]);
    }
    printf("}");
  }
  printf("}");
}

static void Print_R(const ExcnTest(R)& r)
{
  printf("[i = %lu, css = ", (long unsigned) r.i);
  Print_CSS(r.css);
  printf(", a = ");
  Print_A1(r.a);
  printf("]");
}

static void Print_RS(const ExcnTest(RS)& rs)
{
  int i;
  printf("<");
  for (i = 0;  i < rs.length();  i++) {
    if (i > 0) printf(", ");
    Print_R(rs[i]);
  }
  printf(">");
}

static void Print_RO (ExcnTest(R_var) ro)
{
  if (ro != ILU_NIL)
    Print_R(ro);
  else
    printf("nil");
}

const char *decode (CORBA(CompletionStatus) code)
{
  switch (code)
    {
    case CORBA(COMPLETED_NO):
      return "COMPLETED_NO";
    case CORBA(COMPLETED_YES):
      return "COMPLETED_YES";
    case CORBA(COMPLETED_MAYBE):
      return "COMPLETED_MAYBE";
    default:
      return "<invalid completion code>";
    }
}

int
main(int ac, char **av)
{
  char		sid[1000];
  ExcnTest(O_var) handle;
  int		i, count = 3;
  ilu_boolean	threaded = ilu_FALSE, ans;
  ilu_boolean	nosys = ilu_FALSE;

  i = 1;
  while (i < ac) {
    if (strcmp(av[i], "-nosys") == 0) {
      nosys = ilu_TRUE;
      i++;
#ifdef ILU_OS_THREADED
    } else if (strcmp(av[i], "-mt") == 0) {
      threaded = ilu_TRUE;
      i++;
#endif
    } else
      usage(ac, av, i);
  }

  iluCppRuntime::iluInitialize(threaded);

  progname = av[0];

  sprintf(sid, "ExcnTest.server.%s", THIS_DOMAIN);

  handle = ExcnTest(O)::iluLookup(CONST_CAST(iluCString, "T"),
				  CONST_CAST(iluCString, sid));
  if (handle == NULL) {
    fprintf(stderr, "error:  Can't obtain object <%s/T>\n", sid);
    exit(1);
  }

  for (i = 1;  i < 16;  i++) {
    if (nosys && (i > 11 && i < 15))
      continue;
    try {
      handle->throw_excn(i);
      if (i != 15) {
	fprintf (stderr, "ExcnTest.O.throw_excn(%d) completed successfully!\n", i);
	return 1;
      } else {
	printf ("ExcnTest.O.throw_excn(%d) completed successfully.\n", i);
      }
    }
    catch(CORBA(BAD_PARAM)& e) {
      if ((i != 12) && (i != 13)) {
	fprintf (stderr, "ExcnTest.O.throw_excn(%d) signalled CORBA System exception BAD_PARAM, minor=%d (%s), %s!\n",
		 i, e.minor(), e.minor_desc(), decode(e.completed()));
	return 1;
      } else
	printf ("ExcnTest.O.throw_excn(%d) signalled CORBA System exception BAD_PARAM, minor=%d (%s), %s.\n",
		i, e.minor(), e.minor_desc(), decode(e.completed()));
    }
    catch(CORBA(UNKNOWN)& e) {
      if ((i != 12) && (i != 13) && (i != 14)) {
	fprintf (stderr, "ExcnTest.O.throw_excn(%d) signalled CORBA System exception UNKNOWN, minor=%d (%s), %s!\n",
		 i, e.minor(), e.minor_desc(), decode(e.completed()));
	return 1;
      } else
	printf ("ExcnTest.O.throw_excn(%d) signalled CORBA System exception UNKNOWN, minor=%d (%s), %s.\n",
		i, e.minor(), e.minor_desc(), decode(e.completed()));
    }
    catch(CORBA(SystemException)& e) {
      CORBA(string) kindname = e.exception_name();
      fprintf (stderr, "ExcnTest.O.throw_excn(%d) signalled CORBA System exception %s, major=0x%x, minor=0x%x, %s, minor-desc=\"%s\"!\n",
	       i, kindname, e.exception_kind(), e.minor(), decode(e.completed()), e.minor_desc());
      return 1;
    }
    catch(ExcnTest(E1)& e) {
      printf ("ExcnTest.O.throw_excn(%d) signalled CORBA User exception E1\n", i);
      const ExcnTest(U1)& v = e._value();
      printf ("  val = ");
      switch (v._d()) {
      case 0:
	printf("R: ");
	Print_R (v._R_arm());
	break;
      case 1:
	printf("A1: ");
	Print_A1(v._A1_arm());
	break;
      default:
	fprintf(stderr, "Bad discriminant %d in value of ExcnTest.E1 exception\n", v._d());
	return 1;
      }
      printf("\n");
      if (i != 1)
	return 1;
    }
    catch(ExcnTest(E2)& e) {
      printf ("ExcnTest.O.throw_excn(%d) signalled CORBA User exception E2\n", i);
      printf ("  val = %ld\n", (long) e._value());
      if (i != 2)
	return 1;
    }
    catch(ExcnTest(E3)& e) {
      printf ("ExcnTest.O.throw_excn(%d) signalled CORBA User exception E3\n", i);
      printf ("  val = ");  Print_RO(e._value());  printf("\n");
      if (i != 3)
	return 1;
    }
    catch(ExcnTest(E4)& e) {
      printf ("ExcnTest.O.throw_excn(%d) signalled CORBA User exception E4\n", i);
      printf ("  val = <%s>\n", e._value()->iluObjectToString());
      if (i != 4)
	return 1;
    }
    catch(ExcnTest(E5)& e) {
      printf ("ExcnTest.O.throw_excn(%d) signalled CORBA User exception E5\n", i);
      printf ("  val = ");  Print_A0(e._value());  printf("\n");
      if (i != 5)
	return 1;
    }
    catch(ExcnTest(E6)& e) {
      printf ("ExcnTest.O.throw_excn(%d) signalled CORBA User exception E6\n", i);
      printf ("  val = ");  Print_RS(e._value());  printf("\n");
      if (i != 6)
	return 1;
    }
    catch(ExcnTest(E7)& e) {
      printf ("ExcnTest.O.throw_excn(%d) signalled CORBA User exception E7\n", i);
      const char *s = e._value(); printf ("  val = \"%s\"\n", s);
      if (i != 7)
	return 1;
    }
    catch(ExcnTest(E8)& e) {
      printf ("ExcnTest.O.throw_excn(%d) signalled CORBA User exception E8\n", i);
      printf ("  val = ");  Print_A1(e._value());  printf("\n");
      if (i != 8)
	return 1;
    }
    catch(ExcnTest(E9)& e) {
      printf ("ExcnTest.O.throw_excn(%d) signalled CORBA User exception E9\n", i);
      printf ("  val = ");  Print_R(e._value());  printf("\n");
      if (i != 9)
	return 1;
    }
    catch(ExcnTest(E10)& e) {
      printf ("ExcnTest.O.throw_excn(%d) signalled CORBA User exception E10\n", i);
      ExcnTest(TheE) eval = e._value();
      printf ("  val = ");
      switch (eval) {
      case ExcnTest(ev1):
	printf("ExcnTest.E.ev1");
	break;
      case ExcnTest(ev3):
	printf("ExcnTest.E.ev3");
	break;
      case ExcnTest(ev5):
	printf("ExcnTest.E.ev5");
	break;
      case ExcnTest(ev7):
	printf("ExcnTest.E.ev7");
	break;
      default:
	fprintf(stderr, "Bad enum value %d for E10\n", (int) eval);
	exit(1);
      }
      printf ("\n");
      if (i != 10)
	return 1;
    }
    catch(ExcnTest(E11)& e) {
      printf ("ExcnTest.O.throw_excn(%d) signalled CORBA User exception E11\n", i);
      const char *s = e.reason();  printf ("  val = [\"%s\"]\n", s);
      if (i != 11)
	return 1;
    }
  }
  return 0;
}
