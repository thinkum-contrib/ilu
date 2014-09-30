/** $Id: server.c,v 1.6 1999/08/03 01:57:11 janssen Exp $
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
/* Last edited by Mike Spreitzer October 24, 1998 1:23 am PDT */

#include <stdio.h>	/* for stderr and NULL */

/* Include the Tutorial header file, to get all the defined
 * types and function prototypes.
 */

#include <Tutorial.h>

int main (int ac, char **av)
{
  Tutorial_Factory theFactory;
  ILU_C_Server theServer;
  int stop;

  /* this program is to be called with one argument, the server ID
     to use ("Tutorial.foo.something.com" or something like that.)
  */
  
  if (ac < 2)
    {
      fprintf (stderr, "Usage:  server SERVER-ID\n");
      exit(1);
    }

  /* In any server program, we have to initialize each interface
     that we're providing types from, by calling the InitializeServer
     method on that interface.  In this case, it's just the
     Tutorial interface.
  */

  Tutorial__InitializeServer();

  /* We make a "kernel server", using the server ID that was
     passed in on the command line, the default "object table",
     the default protocol for data pickling and message packets,
     the default transport system for getting data back and forth,
     and we make this kernel server the default server for the
     program.
  */

  theServer = ILU_C_InitializeServer(av[1],
				     /* the server ID */
				     NULL,
				     /* use std object table */
				     NULL,
				     /* use default protocol */
				     NULL,
				     /* use default transport */
				     NULL,
				     /* don't supply any identification */
				     ilu_TRUE
				     /* establish as default server */
				     );

  /* Now that we have a server, we create an instance of the
     Factory object type, with the instance handle of "Factory",
     by calling the automatically generated procedure
     "Tutorial_Factory__CreateTrue()".
  */

  theFactory = Tutorial_Factory__CreateTrue ("theFactory",
					     /* instance handle */
					     theServer,
					     /* server to use */
					     NULL
					     /* no user data */
					     );

  /* Now make the Factory object "well-known" by publishing it.
     PublishObject will return NULL if it can't publish the
     object; otherwise it will return a pointer to a string,
     which is the "publish proof".
  */

  if (ILU_C_PublishObject(theFactory) == NULL)
    {
      fprintf (stderr, "Can't publish theFactory object.\n");
      exit(1);
    }
  else
    {
      /* Now we print the string binding handle (the object's name plus
	 its location) of the new instance.
      */

      printf ("Factory instance published.\n");
      printf ("Its SBH is \"%s\".\n", ILU_C_SBHOfObject(theFactory));

      /* ILU_C_StoppableRun() is an event dispatching loop (in
         besingle-threaded runtimes).  It may be exited by invoking
         ILU_C_StopRun() passing the same int * that ILU_C_StoppableRun
         was invoked with.  In multi-threaded runtimes (not the case for
         this example), no "main loop" needs to be invoked, and this
         procedure just blocks the calling thread until some time after
         being told to return. */

      ILU_C_StoppableRun (&stop);
    }
}

