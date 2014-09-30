/** $Id: server.c,v 1.4 1999/08/03 01:59:04 janssen Exp $
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
/* Last edited by Mike Spreitzer October 8, 1998 11:01 pm PDT */

#include <stdio.h>
#include <unistd.h>	/* for gethostname */

#include "hello_world.h"

ilu_CString
  server_hello_world_service_hello_world (hello_world_service self,
					  ILU_C_ENVIRONMENT *status)
{
  return (ILU_C_Strdup("\"Hello, World!\" from ANSI C"));
}

int main (int ac, char **av)
{
  static ILU_C_Server theServer;
  hello_world_service theObject;

  hello_world__InitializeServer();

  theServer = ILU_C_InitializeServer (NULL, NULL, NULL, NULL, NULL, ilu_TRUE);
  if (theServer != NULL) {
    theObject = hello_world_service__CreateTrue (NULL, theServer, NULL);
    if (theObject != NULL) {
      printf ("hello world server is %s\n", ILU_C_SBHOfObject(theObject));
      ILU_C_Run( );
      return 0;
    } else {
      printf ("couldn't hello world object -- exiting\n");
      return 1;
    }
  } else {
    printf ("couldn't hello world server -- exiting\n");
    return 1;
  }
}

