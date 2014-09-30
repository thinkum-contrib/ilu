/** $Id: TapeCalculator-impl.c,v 1.7 1999/08/03 01:57:15 janssen Exp $
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

/* The first thing we need to do is to include the generated header
 * file, which describes the types and methods used by the Tutorial
 * interface
 */

#include <Tutorial2.h>

struct calc_state {
  CORBA_double the_value;
  Tutorial2_RegisterTape *tape;
};

/* We define a function which creates a new instance of a Calculator
 * object.  
 */

  Tutorial2_TapeCalculator
Create_Tutorial2_TapeCalculator ()
{
  struct calc_state *the_state = (struct calc_state *) malloc(sizeof(struct calc_state));

  the_state->the_value = 0.0;					/* zero out our value */
  the_state->tape = (Tutorial2_RegisterTape *) malloc(sizeof(Tutorial2_RegisterTape));
  Tutorial2_RegisterTape_Init(the_state->tape, 0, ILU_NIL);	/* clear tape */

  /* The function "Tutorial2_TapeCalculator__CreateTrue" is automatically
   * generated into the file "Tutorial-true.c" by the c-stubber.
   * It takes three arguments, INSTANCE-HANDLE, SERVER, and
   * USER-DATA-FIELD, and returns a new instance of Tutorial2_TapeCalculator.
   * We don't care about what the INSTANCE-HANDLE and SERVER of Calculator
   * instances are, so we'll pass ILU_NIL (which is another name
   * for NULL) for the first two arguments, which will cause ILU
   * to choose reasonable default values for us.
   */

  return (Tutorial2_TapeCalculator__CreateTrue (ILU_NIL, ILU_NIL, the_state));
}

static Tutorial2_Operation AddOp (Tutorial2_RegisterTape *tape, Tutorial2_OpType t, CORBA_double value, CORBA_double acc)
{
  Tutorial2_Operation n;

  n.op = t;
  n.value = value;
  n.accumulator = acc;
  Tutorial2_RegisterTape_Append (tape, &n);
  return n;
}

static Tutorial2_RegisterTape *CopyTape (Tutorial2_RegisterTape *orig)
{
  unsigned long len = orig->_length;
  Tutorial2_Operation *p = orig->_buffer;
  Tutorial2_Operation *newops = ILU_NIL;

  if (len > 0)
    {
      newops = (Tutorial2_Operation *) malloc(sizeof(struct Tutorial2_Operation) * orig->_length);

      while (len-- > 0)
	newops[len] = (orig->_buffer)[len];
    }

  return (Tutorial2_RegisterTape_Create (orig->_length, newops));
}

/* Now to implement the method, we simply take the true prototype
 * and add whatever code is necessary to actually perform the operation.
 */

  void
server_Tutorial2_TapeCalculator_SetValue (
  Tutorial2_TapeCalculator self,
  CORBA_double v,
  ILU_C_ENVIRONMENT *env)
{
  /* The user data field is available as the field "void *data" of
   * any object instance, so we'll just set it to be "v".
   */

  struct calc_state *s = ((struct calc_state *)(self->instanceData));

  s->the_value = v;
  AddOp (s->tape, Tutorial2_SetValue, v, s->the_value);
}

   CORBA_double
server_Tutorial2_TapeCalculator_GetValue (
   Tutorial2_TapeCalculator self,
   ILU_C_ENVIRONMENT *env)
{
  return (((struct calc_state *) (self->instanceData))->the_value);
}
  
  void
server_Tutorial2_TapeCalculator_Add (
  Tutorial2_TapeCalculator self,
  CORBA_double v,
  ILU_C_ENVIRONMENT *env)
{
  struct calc_state *s = ((struct calc_state *)(self->instanceData));

  s->the_value += v;
  AddOp (s->tape, Tutorial2_Add, v, s->the_value);
}

  void
server_Tutorial2_TapeCalculator_Subtract (
  Tutorial2_TapeCalculator self,
  CORBA_double v,
  ILU_C_ENVIRONMENT *env)
{
  struct calc_state *s = ((struct calc_state *)(self->instanceData));

  s->the_value -= v;
  AddOp (s->tape, Tutorial2_Subtract, v, s->the_value);
}

  void
server_Tutorial2_TapeCalculator_Multiply (
  Tutorial2_TapeCalculator self,
  CORBA_double v,
  ILU_C_ENVIRONMENT *env)
{
  struct calc_state *s = ((struct calc_state *)(self->instanceData));

  s->the_value *= v;
  AddOp (s->tape, Tutorial2_Multiply, v, s->the_value);
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
server_Tutorial2_TapeCalculator_Divide (
  Tutorial2_TapeCalculator self,
  CORBA_double v,
  ILU_C_ENVIRONMENT *env)
{
  if (ABS(v) < SOME_EPSILON)
    RAISE(env, ex_Tutorial_DivideByZero)
  else
    {
      struct calc_state *s = ((struct calc_state *)(self->instanceData));

      s->the_value /= v;
      AddOp (s->tape, Tutorial2_Divide, v, s->the_value);
    }
}

Tutorial2_RegisterTape *
server_Tutorial2_TapeCalculator_GetTape(
				     Tutorial2_TapeCalculator self,
					ILU_C_ENVIRONMENT * env)
{
  return (CopyTape(((struct calc_state *) (self->instanceData))->tape));
}
