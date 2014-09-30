/** $Id: client.c,v 1.3 1999/08/03 01:59:03 janssen Exp $
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
/* Last edited by Mike Spreitzer October 8, 1998 10:58 pm PDT */

#include <string.h>
#include <stdio.h>

#include "hello_world.h"

int main (int ac, char **av)
{
  hello_world_service theService;
  CORBA_Environment env;
  char *response;

  if (ac < 2)
    {
      fprintf (stderr, "Usage:  client SBH-OF-SERVER\n");
      return 1;
    }

  hello_world__Initialize();

  theService = (hello_world_service) ILU_C_SBHToObject(av[1],
						       hello_world_service__MSType,
						       &env);
  if (!ILU_C_SUCCESSFUL(&env)) {
    fprintf (stderr, "Can't create object from SBH <%s>; error is <%s>\n",
	     av[1], CORBA_exception_id(&env));
    CORBA_exception_free(&env);
    return 1;
  };

  response = hello_world_service_hello_world (theService, &env);
  if (!ILU_C_SUCCESSFUL(&env)) {
    fprintf (stderr, "hello_world_service_hello_world (theService) fails with exception <%s>\n",
	     CORBA_exception_id(&env));
    CORBA_exception_free(&env);
    return 1;
  } else {
    printf ("%s\n", response);
    return 0;
  }
}

