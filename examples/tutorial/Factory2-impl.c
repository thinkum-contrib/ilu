/** $Id: Factory2-impl.c,v 1.4 1999/08/03 01:57:15 janssen Exp $
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
/* Last edited by Mike Spreitzer October 9, 1998 1:45 pm PDT */

/* Include the Tutorial header file, to get all the defined
 * types and function prototypes.
 */

#include <stdio.h>
#include <stdlib.h>

#include <Tutorial2.h>

/* Code for the Factory object type */

extern Tutorial2_TapeCalculator Create_Tutorial2_TapeCalculator(void);

static Tutorial2_TapeCalculator
  NewTapeCalculator (Tutorial2_Factory f, ILU_C_ENVIRONMENT *env)
{
  Tutorial2_TapeCalculator newcalc;

  /* We want this to `work' either with non-GC'ed OMG IDL, or with GC'ed
     ILU ISL.  To do that, we look at the class to see whether we used
     OMG IDL, in which case we inherited from ilu.CORBA-Object, and can't
     use GC, or used ILU ISL, in which case we didn't inherit from
     ilu.CORBA-Object, and can use GC to clean up after ourselves.
     We only need to make this check once, so we use a Static boolean
     flag to tell us whether we need to make the check. */

  static ilu_boolean initialized = ilu_FALSE;
  static ilu_boolean using_omg_idl = ilu_FALSE;

  if (! initialized) {
    using_omg_idl = ilu_IsSubObjectType(Tutorial2_TapeCalculator__MSType, ilu_CORBA_Object__MSType);
    initialized = ilu_TRUE;
  }

  newcalc = Create_Tutorial2_TapeCalculator();

  if (using_omg_idl) {
    /* Not using normal ILU GC.  So increment the reference count
       so that the object will still be around later... */
    CORBA_Environment status;
    CORBA_Object_duplicate(newcalc, &status);
    if (!ILU_C_SUCCESSFUL(&status)) {
      fprintf (stderr, "Couldn't duplicate Tutorial2.TapeCalculator instance!  Error %s.\n",
	       CORBA_exception_id(&status));
      CORBA_exception_free(&status);
      exit(1);
    }
  }

  return (newcalc);
}

Tutorial_Calculator server_Tutorial2_Factory_CreateCalculator (Tutorial2_Factory f, ILU_C_ENVIRONMENT *env)
{
  return ((Tutorial_Calculator) NewTapeCalculator(f, env));
}

Tutorial2_TapeCalculator server_Tutorial2_Factory_CreateTapeCalculator (Tutorial2_Factory f, ILU_C_ENVIRONMENT *env)
{
  return (NewTapeCalculator(f, env));
}

