/** $Id: sprogram-cpp.cpp,v 1.3 1999/08/18 03:15:48 janssen Exp $
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

#include "relocate-cpp.hpp"

#define STRINGIFIED_DOMAIN(x)	#x
#define STRINGIFIED_DOMAIN1(x)	STRINGIFIED_DOMAIN(x)
#define THIS_DOMAIN		STRINGIFIED_DOMAIN1(FULLY_QUALIFIED_DOMAIN_NAME)

class impl : public virtual relocate(Foo) {

public:

  impl(char* pc_instance_handle,
       iluServer& r_an_ilu_server= iluServer::iluGetDefaultServer(),
       CORBA(Boolean) b_within_object_table = ILUCPP_FALSE) : 
  iluObject(relocate(Foo)::m_ILUClassRecord, pc_instance_handle, r_an_ilu_server, b_within_object_table) {}
	  
  virtual ~impl() {
    iluDeactivate();
  }

  virtual relocate(Foo_ptr) dummy(void);
};

relocate(Foo_ptr) impl::dummy (void)
{
  return _this();
}

int main (int ac, char **av)
{
  impl *	uc;
  relocate(RelocationManager) * manager;
  char *	default_tinfo[2] = { "tcp_0_0", NULL };
  char *	ltinfo[10];
  ilu_ProtocolInfo	pinfo = "iiop";
  ilu_TransportInfo	tinfo = default_tinfo;
  ilu_ProtocolInfo	publicPinfo = ILU_NIL;
  ilu_TransportInfo	publicTinfo = ILU_NIL;
  ilu_ProtocolInfo	nativePinfo = ILU_NIL;
  ilu_TransportInfo	nativeTinfo = ILU_NIL;
  relocate(TransportInfo) *	relTinfo;
  iluServer *	s;
  char *	proof;
  char *	sid;
  char *	ih;
  char *	mgr_sbh;
  int		i, j;
  ilu_boolean	threaded = ilu_FALSE;

  for (i = 1;  i < ac && av[i][0] == '-'; ) {
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
  if ((ac - i) < 5)
    goto usage;

  sid = av[i++];
  ih = av[i++];
  mgr_sbh = av[i++];
  publicPinfo = av[i++];
  for (j = 0; i < ac; i++, j++) {
    ltinfo[j] = av[i];
  };
  ltinfo[j] = 0;
  publicTinfo = ltinfo;

  iluCppRuntime::iluInitialize(threaded);

  if (pinfo == ILU_NIL) pinfo = ilu_DefaultProtocolInfo();
  if (tinfo == ILU_NIL) tinfo = ilu_DefaultTransportInfo();
  s = new iluServer (sid,
		     NULL,		// object table
		     NULL, NULL,	// no pinfo or tinfo
		     NULL,		// no passport
		     ILUCPP_FALSE);	/* don't create port */
  if (s == NULL) {
    printf("Error.  Couldn't create server.\n");
    exit(1);
  }
  s->iluAddCInfo (publicPinfo, publicTinfo);
  s->iluAddPort (pinfo, tinfo, ILUCPP_TRUE, NULL, ILUCPP_FALSE);
  s->iluGetCInfo (&nativePinfo, &nativeTinfo, ILUCPP_FALSE);
  for (i = 0;  nativeTinfo[i] != NULL;  i++)
    ;
  relTinfo = new relocate(TransportInfo)((CORBA(ULong)) i, (CORBA(ULong)) i, &nativeTinfo[0]);

  printf ("** sprogram ** got cinfo set up\n");
  fflush(stdout);

  // create an instance of an impl
  uc = new impl(ih, *s);
  if (uc == NULL) {
    printf("Error.  Couldn't create object.\n");
    exit(1);
  }

  printf ("** sprogram ** created impl\n");
  fflush(stdout);

  // create a surrogate for the RelocationManager
  printf ("** sprogram ** creating manager from <%s>\n", mgr_sbh);
  fflush(stdout);

  manager = relocate(RelocationManager)::_narrow(iluObject::iluStringToObject(mgr_sbh));
  printf ("** sprogram ** got manager object\n");
  fflush(stdout);

  // and tell it that we are running
  manager->RegisterRealCinfo (sid, nativePinfo, *relTinfo);

  printf ("** sprogram ** registered with manager\n");
  fflush(stdout);
  
  // Now handle requests.
  s->iluRun();
  return 0;
usage:
  fprintf(stderr, "Usage: %s [-mt] [-t tinfo [tinfo...]] [-p pinfo] SID IH MGR_SBH MGR_PINFO MGR_TINFO\n", av[0]);
  return 2;
}
