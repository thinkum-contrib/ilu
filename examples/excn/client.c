/** $Id: client.c,v 1.11 1999/08/11 02:23:15 janssen Exp $
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

#include "ExcnTest.h"

#ifndef HAVE_GETHOSTNAME_PROTOTYPE
extern int gethostname(char *name, int namelen);
#endif /* HAVE_GETHOSTNAME_PROTOTYPE */

#define STRINGIFIED_DOMAIN(x)	#x
#define STRINGIFIED_DOMAIN1(x)	STRINGIFIED_DOMAIN(x)
#define THIS_DOMAIN		STRINGIFIED_DOMAIN1(FULLY_QUALIFIED_DOMAIN_NAME)

static char    *progname = NULL;



static void usage (int ac, char **av, int i)
{
  fprintf (stderr, "Don't understand argument %d:  \"%s\".\n", i+1, av[i]);
  fprintf (stderr, "Usage:  %s [-n COUNT] [-nobadparam] [-nosys]"
#ifdef ILU_OS_THREADED
	   "[-mt] "
#endif
	   "[-hostname HOSTNAME] [-p PROTOCOL] [-t TRANSPORT...] \n", av[0]);
  fprintf (stderr, "  Default:  %s -n 1\n", av[0]);
  exit(1);
}

static void Print_CSS(ExcnTest_CSS *css)
{
  int             i;
  printf("<");
  for (i = 0; i < css->_length; i++)
    printf("%s\"%s\"", (i ? ", " : ""), css->_buffer[i]);
  printf(">");
}

static void Print_A0(ExcnTest_A0 a0)
{
  int i;

  printf("{");
  for (i = 0;  i < 8;  i++)
    printf("%s%u", ((i == 0) ? "" : ", "), a0[i]);
  printf("}");
}

static void Print_A1(ExcnTest_A1 a1)
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

static void Print_R(ExcnTest_R *r)
{
  printf("[i = %lu, css = ", (long unsigned) r->i);
  Print_CSS(&r->css);
  printf(", a = ");
  Print_A1(r->a);
  printf("]");
}

static void Print_RS(ExcnTest_RS *rs)
{
  int i;
  printf("<");
  for (i = 0;  i < ExcnTest_RS_Length(rs);  i++) {
    if (i > 0) printf(", ");
    Print_R(ExcnTest_RS_Nth(rs, i));
  }
  printf(">");
}

static void Print_RO (ExcnTest_RO ro)
{
  if (ro != ILU_NIL)
    Print_R(ro);
  else
    printf("nil");
}

int
main(int ac, char **av)
{
  char		hostname[1000], sid[1000];
  ilu_Error	err;
  ExcnTest_O	handle;
  int		i, count = 3;
  char *	pinfo = "sunrpc";
  char *	tinfo[10] = { "w3mux_0", "tcp_0_0", ILU_NIL };
  ilu_boolean	threaded = ilu_FALSE, ans;
  ILU_C_Server	s3;
  ILU_C_ENVIRONMENT e;
  ilu_string	typestring;
  ilu_boolean	nosys = ilu_FALSE;
  ilu_boolean	nobadparam = ilu_FALSE;

  i = 1;
  gethostname(hostname, sizeof(hostname));
  while (i < ac) {
    if (strcmp(av[i], "-p") == 0) {
      if (++i < ac)
	pinfo = av[i++];
      else
	usage(ac, av, i);
    } else if (strcmp(av[i], "-n") == 0) {
      if (++i < ac)
	count = atoi(av[i++]);
      else
	usage(ac, av, i);
    } else if (strcmp(av[i], "-nosys") == 0) {
      nosys = ilu_TRUE;
      i++;
    } else if (strcmp(av[i], "-nobadparam") == 0) {
      nobadparam = ilu_TRUE;
      i++;
    } else if (strcmp(av[i], "-t") == 0) {
      int j = 0;
      ++i;
      while ((i < ac) && (av[i][0] != '-'))
	tinfo[j++] = av[i++];
      tinfo[j] = ILU_NIL;
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

  if (threaded) {
#if (defined(ILU_OS_THREADED))
    ILU_C_USE_OS_THREADS;
#else
    printf("OS-supplied thread support not configured into ILU!\n");
    exit(-1);
#endif				/* (defined(ILU_OS_THREADED)) */
  }

  ExcnTest__Initialize();
  progname = av[0];

  sprintf(sid, "ExcnTest.server.%s", THIS_DOMAIN);

  handle = ILU_C_LookupObject(sid, "T", ExcnTest_O__MSType);
  if (handle == NULL) {
    fprintf(stderr, "error:  Can't obtain object <%s/T>\n", sid);
    exit(1);
  }

  for (i = 1;  i < 16;  i++) {
    if (nosys && (i > 11 && i < 15))
      continue;
    ExcnTest_O_throw_excn (handle, i, &e);
    if (ILU_C_SUCCESSFUL(&e) && (i != 15)) {
      printf ("ExcnTest_O_throw_excn(%d) completed successfully!\n", i);
      return 1;
    } else {
      if (ILU_C_SUCCESSFUL(&e))
	printf ("ExcnTest_O_throw_excn(%d) completed successfully.\n", i);
      else {
	if (e._major == CORBA_USER_EXCEPTION)
	  typestring = "user";
	else if (e._major == CORBA_SYSTEM_EXCEPTION)
	  typestring = "system";
	else {
	  fprintf (stderr, "Bad value %d for exception's _major field\n", (int) e._major);
	  exit(1);
	};
	printf ("ExcnTest_O_throw_excn(%d) signalled %s exception %s\n",
		i, typestring, CORBA_exception_id(&e));
	switch (i) {

	case 1:
	  if (strcmp(CORBA_exception_id(&e), ex_ExcnTest_E1) != 0)
	    return 1;
	  break;

	case 2:
	  if (strcmp(CORBA_exception_id(&e), ex_ExcnTest_E2) != 0)
	    return 1;
	  break;

	case 3:
	  if (strcmp(CORBA_exception_id(&e), ex_ExcnTest_E3) != 0)
	    return 1;
	  break;

	case 4:
	  if (strcmp(CORBA_exception_id(&e), ex_ExcnTest_E4) != 0)
	    return 1;
	  break;

	case 5:
	  if (strcmp(CORBA_exception_id(&e), ex_ExcnTest_E5) != 0)
	    return 1;
	  break;

	case 6:
	  if (strcmp(CORBA_exception_id(&e), ex_ExcnTest_E6) != 0)
	    return 1;
	  break;

	case 7:
	  if (strcmp(CORBA_exception_id(&e), ex_ExcnTest_E7) != 0)
	    return 1;
	  break;

	case 8:
	  if (strcmp(CORBA_exception_id(&e), ex_ExcnTest_E8) != 0)
	    return 1;
	  break;

	case 9:
	  if (strcmp(CORBA_exception_id(&e), ex_ExcnTest_E9) != 0)
	    return 1;
	  break;

	case 10:
	  if (strcmp(CORBA_exception_id(&e), ex_ExcnTest_E10) != 0)
	    return 1;
	  break;

	case 11:
	  if (strcmp(CORBA_exception_id(&e), ex_ExcnTest_E11) != 0)
	    return 1;
	  break;

	case 12:
	  if ((strcmp(CORBA_exception_id(&e), ex_CORBA_BAD_PARAM) != 0) &&
	      (nobadparam && strcmp(CORBA_exception_id(&e), ex_CORBA_UNKNOWN) != 0))
	    return 1;
	  break;

	case 13:
	  if ((strcmp(CORBA_exception_id(&e), ex_CORBA_BAD_PARAM) != 0) &&
	      (nobadparam && strcmp(CORBA_exception_id(&e), ex_CORBA_UNKNOWN) != 0))
	    return 1;
	  break;

	case 14:
	  if (strcmp(CORBA_exception_id(&e), ex_CORBA_UNKNOWN) != 0)
	    return 1;
	  break;

	case 15:
	  if (!ILU_C_SUCCESSFUL(&e))
	    return 1;
	  break;

	}

	if (CORBA_exception_id(&e) == ex_ExcnTest_E1) {
	  ExcnTest_U1 *u1 = (ExcnTest_U1 *) CORBA_exception_value(&e);
	  printf("  val = ");
	  switch (u1->_d)
	    {
	    case 0:
	      printf("R: ");
	      Print_R (&u1->_u.R);
	      break;
	    case 1:
	      printf("A1: ");
	      Print_A1 (u1->_u.A1);
	      break;
	    default:
	      fprintf(stderr, "Bad discriminant %d in value of ExcnTest1.E1 exception\n", u1->_d);
	      exit(1);
	    }
	  printf("\n");
	  CORBA_exception_free(&e);
	} else if (CORBA_exception_id(&e) == ex_ExcnTest_E2) {
	  printf("  val = %ld\n", *(CORBA_long *) CORBA_exception_value(&e));
	  CORBA_exception_free(&e);
	} else if (CORBA_exception_id(&e) == ex_ExcnTest_E3) {
	  printf("  val = ");  Print_RO (* (ExcnTest_RO *) CORBA_exception_value(&e));  printf("\n");
	  CORBA_exception_free(&e);
	} else if (CORBA_exception_id(&e) == ex_ExcnTest_E4) {
	  printf("  val = %s\n", ILU_C_SBHOfObject(*(ILU_C_Object **) CORBA_exception_value(&e)));
	  CORBA_exception_free(&e);
	} else if (CORBA_exception_id(&e) == ex_ExcnTest_E5) {
	  printf("  val = ");  Print_A0(*(ExcnTest_A0 *) CORBA_exception_value(&e));  printf("\n");
	  CORBA_exception_free(&e);
	} else if (CORBA_exception_id(&e) == ex_ExcnTest_E6) {
	  printf("  val = ");  Print_RS((ExcnTest_RS *) CORBA_exception_value(&e));  printf("\n");
	  CORBA_exception_free(&e);
	} else if (CORBA_exception_id(&e) == ex_ExcnTest_E7) {
	  printf("  val = \"%s\"\n", *(CORBA_string *) CORBA_exception_value(&e));
	  CORBA_exception_free(&e);
	} else if (CORBA_exception_id(&e) == ex_ExcnTest_E8) {
	  printf("  val = ");  Print_A1(*(ExcnTest_A1 *) CORBA_exception_value(&e));  printf("\n");
	  CORBA_exception_free(&e);
	} else if (CORBA_exception_id(&e) == ex_ExcnTest_E9) {
	  printf("  val = ");  Print_R((ExcnTest_R *) CORBA_exception_value(&e));  printf("\n");
	  CORBA_exception_free(&e);
	} else if (CORBA_exception_id(&e) == ex_ExcnTest_E10) {
	  ExcnTest_E eval = *(ExcnTest_E *)CORBA_exception_value(&e);
	  printf("  val = ");
	  switch (eval) {
	  case ExcnTest_ev1:
	    printf("ExcnTest.E.ev1");
	    break;
	  case ExcnTest_ev3:
	    printf("ExcnTest.E.ev3");
	    break;
	  case ExcnTest_ev5:
	    printf("ExcnTest.E.ev5");
	    break;
	  case ExcnTest_ev7:
	    printf("ExcnTest.E.ev7");
	    break;
	  default:
	    fprintf(stderr, "Bad enum value %d for ExcnTest_E10\n", (int) eval);
	    exit(1);
	  }
	  printf ("\n");
	  CORBA_exception_free(&e);
	} else if (CORBA_exception_id(&e) == ex_ExcnTest_E11) {
	  printf("  val = [\"%s\"]\n", ((ExcnTest_E11 *) CORBA_exception_value(&e))->reason);
	  CORBA_exception_free(&e);
	} else if (CORBA_exception_id(&e) == ex_ExcnTest_NotUsed) {
	  CORBA_exception_free(&e);
	} else if (CORBA_exception_id(&e) == ILU_NIL) {	/* success case */
	  ;
	} else if (e._major == CORBA_SYSTEM_EXCEPTION) {
	  CORBA_ex_body *body = (CORBA_ex_body *) CORBA_exception_value(&e);
	  ilu_string comp_status;
	  if (body->completed == CORBA_COMPLETED_YES)
	    comp_status = "Yes";
	  else if (body->completed == CORBA_COMPLETED_NO)
	    comp_status = "No";
	  else if (body->completed == CORBA_COMPLETED_MAYBE)
	    comp_status = "Maybe";
	  else {
	    fprintf (stderr, "Bad completion status %d in system exception\n", (int) body->completed);
	    exit (1);
	  };
	  printf ("  minor = %ld, completion_status = %s\n", body->minor, comp_status);
	  CORBA_exception_free(&e);
	} else {
	  CORBA_exception_free(&e);
	}
      }
    }
  }
  return 0;
}
