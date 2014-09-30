/** $Id: faulty-impl.c,v 1.4 1999/08/03 01:57:10 janssen Exp $
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
/* Last edited by Mike Spreitzer October 9, 1998 1:48 pm PDT */

/* The first thing we need to do is to include the generated header
 * file, which describes the types and methods used by the Tutorial
 * interface
 */

#include <Tutorial.h>

/* We'll then define a static variable of type "CORBA_double" to hold the
 * value of the calculator object, and call it "the_Value".
 */

static CORBA_double the_Value = 0.0;

/* Now to implement the method, we simply take the true prototype
 * and add whatever code is necessary to actually perform the operation.
 */

  void
server_Tutorial_Calculator_SetValue (
  Tutorial_Calculator self,
  CORBA_double v,
  ILU_C_ENVIRONMENT *env)
{
  the_Value = v;
}

   CORBA_double
server_Tutorial_Calculator_GetValue (
   Tutorial_Calculator self,
   ILU_C_ENVIRONMENT *env)
{
  return (the_Value);
}
  
  void
server_Tutorial_Calculator_Add (
  Tutorial_Calculator self,
  CORBA_double v,
  ILU_C_ENVIRONMENT *env)
{
  the_Value += v;
}

  void
server_Tutorial_Calculator_Subtract (
  Tutorial_Calculator self,
  CORBA_double v,
  ILU_C_ENVIRONMENT *env)
{
  the_Value -= v;
}

  void
server_Tutorial_Calculator_Multiply (
  Tutorial_Calculator self,
  CORBA_double v,
  ILU_C_ENVIRONMENT *env)
{
  the_Value *= v;
}

/* The Divide method gets a little trickier.  We have to compare the
 * value "v" to zero, which for floating point values actually means
 * comparing it to some epsilon to see whether it is less than that
 * epsilon, and then if it is "zero" we need to signal an error, by
 * "raising" the "DivideByZero" exception.  The way of raising exceptions
 * in ILU C is rather clumsy, so we'll define a macro to make it look
 * prettier.  We also define some macros to make testing the value
 * of "v" a  bit prettier. 
 */

#define ABS(x)	(((x)<0)?(-(x)):(x))
#define SOME_EPSILON	0.000000001	/* zero, practically speaking */

#define RAISE(env,exception) { (env)->returnCode=(exception);\
                               (env)->_major=CORBA_USER_EXCEPTION; }

  void
server_Tutorial_Calculator_Divide (
  Tutorial_Calculator self,
  CORBA_double v,
  ILU_C_ENVIRONMENT *env)
{
  if (ABS(v) < SOME_EPSILON)
    RAISE(env, ex_Tutorial_DivideByZero)
  else
    the_Value /= v;
}

