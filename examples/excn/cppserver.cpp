/** $Id: cppserver.cpp,v 1.10 1999/08/03 01:58:55 janssen Exp $
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

#include "ExcnTest-cpp.hpp"

#define STRINGIFIED_DOMAIN(x)	#x
#define STRINGIFIED_DOMAIN1(x)	STRINGIFIED_DOMAIN(x)
#define THIS_DOMAIN		STRINGIFIED_DOMAIN1(FULLY_QUALIFIED_DOMAIN_NAME)

class ExcnTest_O_impl : public virtual ExcnTest(O) {

public:

  ExcnTest_O_impl(char* pc_instance_handle,
		  iluServer& r_an_ilu_server= iluServer::iluGetDefaultServer(),
		  CORBA(Boolean) b_within_object_table = ILUCPP_FALSE) : 
	iluObject(ExcnTest(O)::m_ILUClassRecord, pc_instance_handle, r_an_ilu_server, b_within_object_table) {}
	  
  virtual ~ExcnTest_O_impl() {
    iluDeactivate();
  }

  virtual void throw_excn(CORBA(UShort));
};

void ExcnTest_O_impl::throw_excn (CORBA(UShort) which)
{
  switch (which) {

  case 1:
    {
      ExcnTest(R) *r = new ExcnTest(R)();
      ExcnTest(U1) *u = new ExcnTest(U1)();
      r->a[0][0] = 13;
      r->a[0][1] = 13;
      r->a[0][2] = 13;
      r->a[0][3] = 13;
      r->a[1][0] = 13;
      r->a[1][1] = 13;
      r->a[1][2] = 13;
      r->a[1][3] = 13;
      r->a[2][0] = 13;
      r->a[2][1] = 13;
      r->a[2][2] = 13;
      r->a[2][3] = 13;
      r->css.length(3);
      r->css[0] = CORBA(string_dup)("first");
      r->css[1] = CORBA(string_dup)("second");
      r->css[2] = CORBA(string_dup)("third");
      r->i = 789;
      u->_R_arm(*r);
      throw ExcnTest(E1)(*u);
      break;
    }

  case 2:
    {
      throw ExcnTest(E2)(13);
      break;
    }

  case 3:
    {
      throw ExcnTest(E3)(NULL);
      break;
    }

  case 4:
    {
      throw ExcnTest(E4)(_this());
      break;
    }

  case 5:
    {
      ExcnTest(A0) a = { 1, 2, 3, 4, 5, 6, 7, 8 };
      throw ExcnTest(E5)(a);
      break;
    }

  case 6:
    {
      ExcnTest(RS) *rs = new ExcnTest(RS)(0);
      throw ExcnTest(E6)(*rs);
      break;
    }

  case 7:
    {
      throw ExcnTest(E7)(CORBA(string_dup)("test-string"));
      break;
    }

  case 8:
    {
      ExcnTest(A1) a1;
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
      throw ExcnTest(E8)(a1);
      break;
    }

  case 9:
    {
      ExcnTest(R) *r = new ExcnTest(R)();
      r->a[0][0] = 9;
      r->a[0][1] = 9;
      r->a[0][2] = 9;
      r->a[0][3] = 9;
      r->a[1][0] = 9;
      r->a[1][1] = 9;
      r->a[1][2] = 9;
      r->a[1][3] = 9;
      r->a[2][0] = 9;
      r->a[2][1] = 9;
      r->a[2][2] = 9;
      r->a[2][3] = 9;
      r->css.length(3);
      r->css[0] = CORBA(string_dup)("red");
      r->css[1] = CORBA(string_dup)("blue");
      r->css[2] = CORBA(string_dup)("green");
      r->i = 123;
      throw ExcnTest(E9)(*r);
      break;
    }

  default:
  case 10:
    {
      throw ExcnTest(E10)(ExcnTest(ev5));
      break;
    }

  case 11:
    {
      throw ExcnTest(E11)(CORBA(string_dup)("some nutty reason"));
      break;
    }

  case 12:
    {
      throw CORBA(BAD_PARAM)(0, CORBA(COMPLETED_NO));
      break;
    }

  case 13:
    {
      throw CORBA(BAD_PARAM)(0, CORBA(COMPLETED_YES));
      break;
    }

  case 14:
    {
      throw ExcnTest(NotUsed)();
      break;
    }

  case 15:
    {
      return;
    }

  }
}

int main (int ac, char **av)
{
  ExcnTest_O_impl *	uc;
  char *	ltinfo[10];
  ilu_ProtocolInfo	pinfo = ILU_NIL;
  ilu_TransportInfo	tinfo = ILU_NIL;
  iluServer *	s;
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

  iluCppRuntime::iluInitialize(threaded);

  if (pinfo == ILU_NIL) pinfo = ilu_DefaultProtocolInfo();
  if (tinfo == ILU_NIL) tinfo = ilu_DefaultTransportInfo();
  sprintf(sid, "ExcnTest.server.%s", THIS_DOMAIN);
  s = new iluServer (sid,
		     NULL,	// object table
		     pinfo,
		     tinfo);
  if (s == NULL) {
    printf("Error.  Couldn't create server.\n");
    exit(1);
  }
  uc = new ExcnTest_O_impl((char *) "T", *s);
  if (uc == NULL) {
    printf("Error.  Couldn't create object.\n");
    exit(1);
  }
  if (!uc->iluPublish()) {
    fprintf(stderr, "Error.  Couldn't publish ExcnTest object.\n");
    exit(1);
  }
  printf("exported %s\n", uc->iluObjectToString());
  fflush(stdout);
  s->iluRun();
  return 0;
usage:
  fprintf(stderr, "Usage: %s [-mt] [-t tinfo [tinfo...]] [-p pinfo]\n", av[0]);
  return 2;
}
