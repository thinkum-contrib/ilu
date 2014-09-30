/** $Id: server.c,v 1.10 1999/08/03 01:58:02 janssen Exp $
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
/* Last edited by Mike Spreitzer October 9, 1998 10:17 am PDT */

#include <stdlib.h>
#include <stdio.h>

#ifndef WIN32
#include <unistd.h>	/* for gethostname */
#endif

#include "objtable.h"

#define ILU_TEST_DOMAIN "parc.xerox.com"

static CORBA_Object
  object_of_ih (ilu_string ih, ilu_private rock)
{
  return (objtable_file__OTCreateTrue (ih,
				       *((ILU_C_Server *) rock),
				       ILU_C_Strdup(ih)));
}

static void
  free_self (ilu_private self)
{
  return;
}

objtable_filename
  server_objtable_file_name (objtable_file self,
			     ILU_C_ENVIRONMENT *status)
{
  ILU_C_SET_SUCCESSFUL(status);
  return (ILU_C_Strdup((char *) objtable_file__GetUserData(self)));
}

objtable_file
  server_objtable_server_find_file (objtable_file self,
				    objtable_filename name,
				    ILU_C_ENVIRONMENT *status)
{
  ILU_C_SET_SUCCESSFUL(status);
  return (objtable_file__CreateTrue (name,
				     self->server,
				     ILU_C_Strdup(name)));
}

int main (int ac, char **av)
{
  static ILU_C_Server ks;
  objtable_server s;
  objtable_file f;
  char *sid;
  ILU_C_ObjectTable ot = ILU_C_CreateObjectTable (object_of_ih, free_self, (ilu_private) &ks);

  objtable__InitializeServer();

  sid = ilu_InventID();
  ks = ILU_C_InitializeServer (sid, ot, ILU_NIL, ILU_NIL, ILU_NIL, ilu_TRUE);
  s = objtable_server__CreateTrue ( "----", ks, NULL );
  ILU_C_PublishObject (s);

  if (s != NULL)
    {
      printf ("%s\n", ILU_C_SBHOfObject(s));
      fflush(stdout);
      ILU_C_Run( );
    }
  else
    {
      printf ("couldn't create server object -- exiting\n");
      exit(1);
    }
  return 1;
}

