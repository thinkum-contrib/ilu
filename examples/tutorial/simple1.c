/* simple1.c, a simple program that demonstrates the use of the
   Tutorial true module as a library.
*/
/** $Id: simple1.c,v 1.6 1999/08/03 01:57:10 janssen Exp $
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
/* Last edited by Mike Spreitzer October 9, 1998 1:50 pm PDT */

#include <stdio.h>	/* for NULL */
#include <stdlib.h>	/* for atof */

/* Include the header file for the Tutorial interface, so that our
 * types and methods will be defined.
 */

#include <Tutorial.h>

/* We should also define a prototype for the Create function
 * exported from the implementation of the Tutorial module.
 */

extern Tutorial_Calculator Create_Tutorial_Calculator(void);

/* A simple program:
 *  1)  make an instance of Tutorial.Calculator
 *  2)  add all the arguments by invoking the Add method
 *  3)  print the resultant value.
 */

int main (int argc, char **argv)
{
  Tutorial_Calculator c;
  CORBA_double v;
  char **arg;
  ILU_C_ENVIRONMENT env;

  /* Initialize the Tutorial module */

  Tutorial__InitializeServer();

  /* Create a calculator object */

  if ((c = Create_Tutorial_Calculator()) == NULL)
    {
      fprintf (stderr, "Couldn't create calculator!\n");
      exit(1);
    }

  /* clear the calculator before using it */

  Tutorial_Calculator_SetValue (c, 0.0, &env);

  /* now loop over the arguments, adding each in turn */

  for (arg = ++argv;  *arg != NULL;  arg++)
    {
      v = atof (*arg);
      Tutorial_Calculator_Add (c, v, &env);
    }

  /* and print the result */

  v = Tutorial_Calculator_GetValue (c, &env);
  printf ("the sum is %.5e\n", v);

  exit (0);  
}
