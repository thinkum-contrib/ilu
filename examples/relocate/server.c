/** $Id: server.c,v 1.3 1999/08/03 01:59:02 janssen Exp $
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

#include <stdio.h>
#include <string.h>
#include <math.h>
#include <stdlib.h>	/* for exit() */

#include "relocate.h"

#ifndef HAVE_GETHOSTNAME_PROTOTYPE
extern int gethostname(char *name, int namelen);
#endif /* HAVE_GETHOSTNAME_PROTOTYPE */

#define STRINGIFIED_DOMAIN(x)	#x
#define STRINGIFIED_DOMAIN1(x)	STRINGIFIED_DOMAIN(x)
#define THIS_DOMAIN		STRINGIFIED_DOMAIN1(FULLY_QUALIFIED_DOMAIN_NAME)

static unsigned Port2 = 0;
static ilu_ProtocolInfo ThePinfo = ILU_NIL;
static ilu_string TheRecordMarkingTinfoLayer = ILU_NIL;
static char Hostname[100];

static ilu_boolean
  relocate (ILU_C_Server s, ilu_private rock, ilu_ProtocolInfo *new_pinfo, ilu_TransportInfo *new_tinfo)
{
  ilu_TransportInfo ntinfo;
  char tinfobuf[100];

  *new_pinfo = ILU_C_Strdup(ThePinfo);
  sprintf(tinfobuf, "tcp_%s_%u", Hostname, Port2);
  ntinfo = ilu_malloc(sizeof(char *) * ((TheRecordMarkingTinfoLayer == ILU_NIL) ? 2 : 3));
  if (TheRecordMarkingTinfoLayer == ILU_NIL) {
    ntinfo[0] = ILU_C_Strdup(tinfobuf);
    ntinfo[1] = NULL;
  } else {
    ntinfo[0] = ILU_C_Strdup(TheRecordMarkingTinfoLayer);
    ntinfo[1] = ILU_C_Strdup(tinfobuf);
    ntinfo[2] = NULL;
  }
  *new_tinfo = ntinfo;
  printf ("relocating to %s (", ThePinfo);
  if (TheRecordMarkingTinfoLayer != ILU_NIL)
    printf ("%s, ", TheRecordMarkingTinfoLayer);
  printf ("%s,)\n", tinfobuf);
  return ilu_TRUE;
}

void server_relocate_Foo_dummy (relocate_Foo self, ILU_C_ENVIRONMENT *e)
{
  ILU_C_SET_SUCCESSFUL(e);
  return;
}

int main (int ac, char **av)
{
  relocate_Foo	uc;
  char *	tinfo1[3];
  char *	tinfo2[3];
  char		tinfo1buf[100], tinfo2buf[100];
  ilu_TransportInfo	tinfo = ILU_NIL;
  ILU_C_Server	s;
  char *	proof;
  char		sid[1000];
  int		i = 1;
  ilu_boolean	threaded = ilu_FALSE;
  unsigned	port1;
  ILU_C_ENVIRONMENT e;

  while (i < ac) {
    if (strcmp(av[i], "-p") == 0) {
      if (i++ < ac)
	ThePinfo = av[i++];
      else
	goto usage;
    } else if (strcmp(av[i], "-1") == 0) {
	if (i++ < ac)
	  port1 = atoi(av[i++]);
      else
	goto usage;
    } else if (strcmp(av[i], "-2") == 0) {
      if (i++ < ac)
	Port2 = atoi(av[i++]);
      else
	goto usage;
    } else if (strcmp(av[i], "-t") == 0) {
      if (i++ < ac)
	TheRecordMarkingTinfoLayer = av[i++];
      else
	goto usage;
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

  relocate__InitializeServer();

  /* form the tinfos */
  gethostname(Hostname, sizeof(Hostname));
  sprintf(tinfo1buf, "tcp_%s_%u", Hostname, port1);
  sprintf(tinfo2buf, "tcp_%s_%u", Hostname, Port2);
  if (TheRecordMarkingTinfoLayer != NULL) {
    tinfo1[0] = TheRecordMarkingTinfoLayer;
    tinfo1[1] = tinfo1buf;
    tinfo1[2] = NULL;
    tinfo2[0] = TheRecordMarkingTinfoLayer;
    tinfo2[1] = tinfo2buf;
    tinfo2[2] = NULL;
  } else {
    tinfo1[0] = tinfo1buf;
    tinfo1[1] = NULL;
    tinfo2[0] = tinfo2buf;
    tinfo2[1] = NULL;
  }

  if (ThePinfo == ILU_NIL) ThePinfo = ilu_DefaultProtocolInfo();
  s = ILU_C_InitializeServer("relocate-server", NULL, ThePinfo, tinfo1, NULL, ilu_TRUE);
  if (s == NULL) {
    fprintf(stderr, "Error.  Couldn't create server.\n");
    exit(1);
  }
  if (!ILU_C_AddPort(s, ThePinfo, tinfo2, NULL, ilu_FALSE, &e)) {
    fprintf (stderr, "Error %s.  Couldn't add second port.\n", CORBA_exception_id(&e));
    CORBA_exception_free(&e);
    exit(1);
  };
  (void) ILU_C_SetServerRelocationProc(s, &relocate, ILU_NIL, &e);
  if (!ILU_C_SUCCESSFUL(&e)) {
    fprintf (stderr, "Error %s.  Couldn't set relocation procedure.\n", CORBA_exception_id(&e));
    CORBA_exception_free(&e);
    exit(1);
  };

  uc = relocate_Foo__CreateTrue("dummy", s, NULL);
  if (uc == NULL) {
    printf("Error.  Couldn't create object.\n");
    exit(1);
  }
#ifdef IIOP_PROTOCOL
  printf("%s\n", ILU_C_IOROfObject(uc));
#endif
  if ((proof = ILU_C_PublishObject(uc)) == NULL) {
    fprintf(stderr, "Error.  Couldn't publish ExcnTest object.\n");
    exit(1);
  }
  printf("%s\n", ILU_C_SBHOfObject(uc));
  ILU_C_Run();
  return 0;
usage:
  fprintf(stderr, "Usage: %s -1 PORT1 -2 PORT2 [-mt] [-t RECORD-MARKING-TINFO-LAYER] [-p PINFO]\n", av[0]);
  return 2;
}
