/** $Id: simple3.c,v 1.6 1999/08/03 01:57:13 janssen Exp $
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
/* Last edited by Mike Spreitzer October 9, 1998 1:52 pm PDT */


#include <stdio.h>	/* for NULL */
#include <stdlib.h>	/* for atof */

/* Include the header file for the Tutorial interface, so that our
 * types and methods will be defined.
 */

#include <Tutorial.h>

/* We define a new routine, "Get_Tutorial_Calculator", which 
 * finds the tutorial factory, then creates a new Calculator
 * object for us.
 */

  static Tutorial_Calculator
Get_Tutorial_Calculator (char *sid, char *ih)
{
  Tutorial_Factory f;
  Tutorial_Calculator c;
  ILU_C_ENVIRONMENT env;

  /* We have to call ILU_C_LookupObject() with the object ID of
   * the factory object, and the ``type'' of the object we're looking
   * for, which is always available as "TYPENAME__MSType".
   */

  f = ILU_C_LookupObject (sid, ih, Tutorial_Factory__MSType);

  if (f == NULL)
    {
      fprintf (stderr, "Couldn't find Factory object <%s %s>.\n",
	       sid, ih);
      return (NULL);
    }

  /* Now call the CreateCalculator method on the factory, and check
   * the result...
   */

  c = Tutorial_Factory_CreateCalculator (f, &env);
  if (! ILU_C_SUCCESSFUL(&env))
    {
      fprintf (stderr, "Call to CreateCalculator failed with exception <%s>.\n",
	       ILU_C_EXCEPTION_ID(&env));
      return (NULL);
    }

  /* And return the calculator */

  return (c);
}

/* A simple program:
 *  1)  make an instance of Tutorial.Calculator
 *  2)  add all the arguments by invoking the Add method
 *  3)  print the resultant value.
 */

int main (int argc, char **argv)
{
  Tutorial_Calculator c;
  CORBA_double v;
  char *sid, *ih;
  char **arg;
  ILU_C_ENVIRONMENT env;

  if (argc < 4)
    {
      fprintf (stderr, "Usage:  %s FACTORY-OBJECT-SID FACTORY-OBJECT-IH NUMBER [NUMBER...]\n",
	       argv[0]);
      exit(1);
    }

  Tutorial__Initialize();

  sid = *++argv;
  ih = *++argv;
  if ((c = Get_Tutorial_Calculator(sid, ih)) == NULL)
    {
      fprintf (stderr, "Couldn't create calculator!\n");
      exit(1);
    }

  /* clear the calculator before using it */

  Tutorial_Calculator_SetValue (c, 0.0, &env);
  if (! ILU_C_SUCCESSFUL(&env))
    {
      fprintf (stderr, "SetValue signalled <%s>.\n",
	       ILU_C_EXCEPTION_ID(&env));
      exit(1);
    }

  /* now loop over the arguments, adding each in turn */

  for (arg = ++argv;  *arg != NULL;  arg++)
    {
      v = atof (*arg);
      Tutorial_Calculator_Add (c, v, &env);
      if (! ILU_C_SUCCESSFUL(&env))
	{
	  fprintf (stderr, "Add signalled <%s>.\n",
		   ILU_C_EXCEPTION_ID(&env));
	  exit(1);
	}
    }

  /* and print the result */

  v = Tutorial_Calculator_GetValue (c, &env);
  if (! ILU_C_SUCCESSFUL(&env))
    {
      fprintf (stderr, "GetValue signalled <%s>.\n",
	       ILU_C_EXCEPTION_ID(&env));
      exit(1);
    }
  else
    printf ("the sum is %.5e\n", v);

  exit (0);  
}
