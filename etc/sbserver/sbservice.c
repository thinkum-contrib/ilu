/* Command line test of simple binding name service */
/** $Id: sbservice.c,v 1.19 1999/08/03 01:56:17 janssen Exp $
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
#include <stdlib.h>
#include <string.h>

#include "ilu_simpbind.h"

extern ilu_simpbind_Server ilu_simpbind_create_server (ILU_C_Server,	/* server */
						       ilu_string,	/* ih */
						       char *,		/* backing file name */
						       ilu_boolean);	/* protected */

static void usage(char *pname)
{
  fprintf (stderr, "Usage:  %s [-s] [-p PORT] [-h HOSTNAME-OR-IP-ADDR] [-r REALM-NAME] [-f BACKING-FILE] [-advertise]\n", pname);
  exit(1);
}

int main (int ac, char **av)
{
  ilu_simpbind_Server theSB;
  ILU_C_Server theServer;
  int stop;
  char *filename = ILU_NIL;
  char *tinfo[10];
  ilu_TransportInfo default_tinfo;
  char realm_name[1024];
  char hostname[1024];
  char tcpinfo[1100];
  char sid[1100];
  unsigned long rawport;
  ilu_shortcardinal port;
  ilu_boolean protected = ilu_FALSE;
  int i = 1;
  ilu_boolean advertise = ilu_FALSE;

  struct hostent *he;
  struct in_addr *hea;

  ilu_GetSBServiceParms(realm_name, hostname, &port);
  while (i < ac) {
    if (strcmp(av[i], "-r") == 0) {
      if (++i < ac)
	strcpy(realm_name, av[i]), i++;
      else
	usage(av[0]);
    } else if (strcmp(av[i], "-p") == 0) {
      if (++i < ac) {
	(rawport = strtoul(av[i], ILU_NIL, 0)), i++;
	if (rawport >= 0 && rawport <= 0xFFFF)
	  port = rawport;
	else
	  usage(av[0]);
      }
      else
	usage(av[0]);
    } else if (strcmp(av[i], "-h") == 0) {
      if (++i < ac)
	strcpy (hostname, av[i]), i++;
      else
	usage(av[0]);
    } else if (strcmp(av[i], "-s") == 0) {
      protected = ilu_TRUE;
      i++;
    } else if (strcmp(av[i], "-advertise") == 0) {
      advertise = ilu_TRUE;
      i++;
    } else if (strcmp(av[i], "-f") == 0) {
      if (++i < ac)
	filename = av[i++];
      else
	usage(av[0]);
    } else usage(av[0]);
  }    

  /* initialize simpbind interface */
  ilu_simpbind__InitializeServer();

  sprintf (sid, "ILUSimpleBindingService.%s", realm_name);
  sprintf (tcpinfo, "tcp_%s_%d", hostname, port);
  default_tinfo = ilu_DefaultTransportInfo();
  for (i = 0;  i < 10;  i++) {
    tinfo[i] = default_tinfo[i];
    if (default_tinfo[i] == NULL)
      break;
    else if (strncmp(default_tinfo[i], "tcp_", 4) == 0) {
      tinfo[i] = tcpinfo;
    }
  }
  if (i == 10) {
    fprintf (stderr, "Error forming tinfo for service.\n");
    return 1;
  };

  /* make kernel server */
  theServer = ILU_C_InitializeServer(sid,
				     NULL, /* use std object table */
				     ilu_DefaultProtocolInfo(),
				     tinfo,
				     ILU_NIL, /* no identity at present */
				     ilu_TRUE /* establish as default server */
				     );
  
  if (theServer == ILU_NIL) {
    fprintf (stderr, "Error creating server object group.\n");
    return 1;
  };

  if (filename == ILU_NIL)
    {
      sprintf (hostname, "/tmp/%s", sid);
      filename = hostname;
    }

  theSB = ilu_simpbind_create_server (theServer, "Server", filename, protected);
  if (theSB == ILU_NIL) {
    fprintf (stderr, "Error creating server object.\n");
    return 1;
  } else {
    if (advertise) {
      char thehost[2048];
      unsigned theport;
      char *p, *q;
      char *sbh = ILU_C_SBHOfObject(theSB);
      p = strchr(sbh, ILU_CINFO_DIVIDER);
      if (p == ILU_NIL) {
	fprintf (stderr, "Malformed SBH <%s>.  No ILU_CINFO_DIVIDER.\n", sbh);
	return 1;
      };
      q = strrchr(sbh, ILU_TINFO_DIVIDER);
      if (q == ILU_NIL)
	q = p;
      q = q+1;
      if (sscanf (q, "tcp_%[^_]_%u", &thehost, &theport) != 2) {
	fprintf (stderr, "Can't parse SBH <%s> for host and port!\n", ILU_C_SBHOfObject(theSB));
	return 1;
      };
      printf ("%s:%s:%u\n", realm_name, thehost, theport);
    } else
      printf ("Simple binding server ready; SBH is \"%s\".\n",
	      ILU_C_SBHOfObject(theSB));
    fflush(stdout);
    ilu_RunMainLoop (&stop);
  }
  return 0;
}



