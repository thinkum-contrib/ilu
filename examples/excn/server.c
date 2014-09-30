/** $Id: server.c,v 1.10 1999/08/03 01:58:52 janssen Exp $
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
/* Last edited by Mike Spreitzer October 8, 1998 10:49 pm PDT */

#include <stdio.h>
#include <string.h>
#include <math.h>
#include <stdlib.h>	/* for exit() */

#include "ExcnTest.h"

#define STRINGIFIED_DOMAIN(x)	#x
#define STRINGIFIED_DOMAIN1(x)	STRINGIFIED_DOMAIN(x)
#define THIS_DOMAIN		STRINGIFIED_DOMAIN1(FULLY_QUALIFIED_DOMAIN_NAME)

int main (int ac, char **av)
{
  ExcnTest_O	uc;
  char *	ltinfo[10];
  ilu_ProtocolInfo	pinfo = ILU_NIL;
  ilu_TransportInfo	tinfo = ILU_NIL;
  ILU_C_Server	s;
  char *	proof;
  char		sid[1000];
  int		i = 1;
  ilu_boolean	threaded = ilu_FALSE;

  for (i = 1;  i < ac; ) {
    if (strcmp(av[i], "-p") == 0) {
      if (++i < ac)
	pinfo = av[i++];
      else
	goto usage;
    } else if (strcmp(av[i], "-t") == 0) {
      int j = 0;
      ++i;
      while ((i < ac) && (av[i][0] != '-'))
	ltinfo[j++] = av[i++];
      ltinfo[j] = ILU_NIL;
      tinfo = ltinfo;
    } else if (strcmp(av[i], "-mt") == 0) {
      threaded = ilu_TRUE;
      ++i;
    } else
      goto usage;
  }

  if (threaded) {
#if (defined(ILU_OS_THREADED))
    ILU_C_USE_OS_THREADS;
#else
    OUTPUT("OS-supplied thread support not configured into ILU!\n");
    exit(-1);
#endif				/* (defined(ILU_OS_THREADED)) */
  }

  ExcnTest__InitializeServer();
  if (pinfo == ILU_NIL) pinfo = ilu_DefaultProtocolInfo();
  if (tinfo == ILU_NIL) tinfo = ilu_DefaultTransportInfo();
  sprintf(sid, "ExcnTest.server.%s", THIS_DOMAIN);
  s = ILU_C_InitializeServer(sid, NULL, pinfo, tinfo, NULL, ilu_TRUE);

  if (s == NULL) {
    printf("Error.  Couldn't create server.\n");
    exit(1);
  }
  uc = ExcnTest_O__CreateTrue("T", s, NULL);
  if (uc == NULL) {
    printf("Error.  Couldn't create object.\n");
    exit(1);
  }
  if ((proof = ILU_C_PublishObject(uc)) == NULL) {
    fprintf(stderr, "Error.  Couldn't publish ExcnTest object.\n");
    exit(1);
  }
  printf("exported %s\n", ILU_C_SBHOfObject(uc));
  fflush(stdout);
  ILU_C_Run();
  return 0;
usage:
  fprintf(stderr, "Usage: %s [-mt] [-t tinfo [tinfo...]] [-p pinfo]\n", av[0]);
  return 2;
}

void
  server_ExcnTest_O_throw_excn (ExcnTest_O handle, CORBA_unsigned_short w, CORBA_Environment *env)
{
  switch (w) {

  case 1:
    {
      ExcnTest_U1 u1;
      u1._d = 0;
      u1._u.R.a[0][0] = 13;
      u1._u.R.a[0][1] = 13;
      u1._u.R.a[0][2] = 13;
      u1._u.R.a[0][3] = 13;
      u1._u.R.a[1][0] = 13;
      u1._u.R.a[1][1] = 13;
      u1._u.R.a[1][2] = 13;
      u1._u.R.a[1][3] = 13;
      u1._u.R.a[2][0] = 13;
      u1._u.R.a[2][1] = 13;
      u1._u.R.a[2][2] = 13;
      u1._u.R.a[2][3] = 13;
      ExcnTest_CSS_Init(&u1._u.R.css, 3, ILU_NIL);
      u1._u.R.css._buffer[0] = ILU_C_Strdup("first");
      u1._u.R.css._buffer[1] = ILU_C_Strdup("second");
      u1._u.R.css._buffer[2] = ILU_C_Strdup("third");
      u1._u.R.css._length = 3;
      u1._u.R.i = 789;
      ExcnTest__BindExceptionValue(env, ex_ExcnTest_E1, &u1);
      break;
    }

  case 2:
    {
      ExcnTest__BindExceptionValue(env, ex_ExcnTest_E2, 13);
      break;
    }

  case 3:
    {
      ExcnTest__BindExceptionValue(env, ex_ExcnTest_E3, ILU_NIL);
      break;
    }

  case 4:
    {
      ExcnTest__BindExceptionValue(env, ex_ExcnTest_E4, handle);
      break;
    }

  case 5:
    {
      ExcnTest_A0 f = { 0, 1, 2, 3, 4, 5, 6, 7 };
      ExcnTest__BindExceptionValue(env, ex_ExcnTest_E5, &f);
      break;
    }

  case 6:
    {
      ExcnTest_RS rs;
      rs._buffer = CORBA_sequence_ExcnTest_R_allocbuf(2);
      rs._maximum = 2;
      rs._length = 2;
      ExcnTest__BindExceptionValue(env, ex_ExcnTest_E6, &rs);
      break;
    }

  case 7:
    {
      ExcnTest__BindExceptionValue(env, ex_ExcnTest_E7, ILU_C_Strdup("test-string"));
      break;
    }

  case 8:
    {
      ExcnTest_A1 a1;
      a1[0][0] = 0;
      a1[0][1] = 1;
      a1[0][2] = 2;
      a1[0][3] = 3;
      a1[1][0] = 4;
      a1[1][1] = 5;
      a1[1][2] = 6;
      a1[1][3] = 7;
      a1[2][0] = 8;
      a1[2][1] = 9;
      a1[2][2] = 10;
      a1[2][3] = 11;
      ExcnTest__BindExceptionValue(env, ex_ExcnTest_E8, &a1);
      break;
    }

  case 9:
    {
      ExcnTest_R R;
      R.a[0][0] = 9;
      R.a[0][1] = 9;
      R.a[0][2] = 9;
      R.a[0][3] = 9;
      R.a[1][0] = 9;
      R.a[1][1] = 9;
      R.a[1][2] = 9;
      R.a[1][3] = 9;
      R.a[2][0] = 9;
      R.a[2][1] = 9;
      R.a[2][2] = 9;
      R.a[2][3] = 9;
      ExcnTest_CSS_Init(&R.css, 3, ILU_NIL);
      ExcnTest_CSS_Append(&R.css, ILU_C_Strdup("red"));
      ExcnTest_CSS_Append(&R.css, ILU_C_Strdup("green"));
      ExcnTest_CSS_Append(&R.css, ILU_C_Strdup("blue"));
      R.i = 123;
      ExcnTest__BindExceptionValue(env, ex_ExcnTest_E9, &R);
      break;
    }

  default:
  case 10:
    {
      ExcnTest__BindExceptionValue(env, ex_ExcnTest_E10, ExcnTest_ev5);
      break;
    }

  case 11:
    {
      ExcnTest_E11 e;
      e.reason = ILU_C_Strdup("some nutty reason");
      ExcnTest__BindExceptionValue(env, ex_ExcnTest_E11, &e);
      break;
    }

  case 12:
    {
      ILU_C_RAISE_SYSTEM(env,BAD_PARAM,0,NO);
      break;
    }

  case 13:
    {
      ILU_C_RAISE_SYSTEM(env,BAD_PARAM,0,YES);
      break;
    }

  case 14:
    {
      ExcnTest__BindExceptionValue(env, ex_ExcnTest_NotUsed, ILU_NIL);
      break;
    }

  case 15:
    {
      ILU_C_SET_SUCCESSFUL(env);
      break;
    }

  }
}
