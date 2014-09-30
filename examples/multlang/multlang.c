/** $Id: multlang.c,v 1.16 1999/09/16 16:52:17 janssen Exp $
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
/* Last edited by Mike Spreitzer October 9, 1998 10:06 am PDT */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "multlang.h"

extern int Py_Initialize(void);
extern int PyRun_SimpleString(char *);

#ifdef ILU_PYTHON_HAS_SETPYTHONNAME
extern void Py_SetProgramName(char *);
#endif

ILU_C_Server theServer;

CORBA_unsigned_long
  server_multlang_Squarer_ObtainSquare (multlang_Squarer handle,
					CORBA_unsigned_long val,
					ILU_C_ENVIRONMENT *env)
{
  /* dummy routine; never called */
  fprintf (stderr, "C routine server_multlang_Squarer_ObtainSquare called!\n");
  exit(1);
  return 1;
}

CORBA_unsigned_long
  server_multlang_Multiplier_Multiply (multlang_Multiplier handle,
				       CORBA_unsigned_long val1,
				       CORBA_unsigned_long val2,
				       ILU_C_ENVIRONMENT *env)
{
  return (val1 * val2);
}

static void usage (char *pname)
{
  fprintf(stderr, "Usage:  %s [-mt]\n", pname);
  exit(1);
}

#ifdef ILU_PYTHON_HAS_THREADS
static ilu_boolean
  dummy_init_threading(ILU_ERRS((bad_param, no_memory,
				 no_resources, internal)) * err)
{
  /* we don't need to set WaitTech, MainLoop, or LockTech, as
     this has already been done in Python runtime */
  return ILU_CLER(*err);
}

#endif

int main(int ac, char **av)
{
  multlang_Squarer	squarer;
  multlang_Multiplier	mult;
  ILU_C_ENVIRONMENT	env;
  CORBA_unsigned_long	input, val;
  CORBA_boolean		stat;
  ilu_string		proof;
  ilu_boolean		threaded = ilu_FALSE;
  int			i;

  for (i = 1;  i < ac; ) {
    if (strcmp(av[i], "-mt") == 0) {
      threaded = ilu_TRUE;
      ++i;
    } else
      usage(av[0]);
  }

  /* initialize Python interpreter */

#ifdef ILU_PYTHON_HAS_SETPYTHONNAME
  Py_SetProgramName(av[0]);
#endif
  Py_Initialize();

  if (threaded) {
    /* we use Python threads in both Python and C because
       there's no way to tell Python to use C threads */

#ifdef ILU_PYTHON_HAS_THREADS
    /* start Python threads */
    PyRun_SimpleString("import ilu; ilu.ThreadedOperation()");
    /* now tell the C runtime to use the Python threads */
    {
      extern ilu_boolean
	ilupython_GetPerThreadDataTech
	  (ilu_PerThreadDataDestructor,	/* IN, GLOBAL, OPTIONAL */
	   ilu_PerThreadDataGetter *,	/* OUT, GLOBAL */
	   ilu_PerThreadDataSetter *,	/* OUT, GLOBAL */
	   ILU_ERRS((no_memory, internal)) *);
      extern ilu_boolean
	ilupython_ForkNewThread (ilu_ClosureProc, void *,
				 ILU_ERRS((no_memory, no_resources,
					   internal)) *);
      ILU_C_EnableThreads(dummy_init_threading, ilupython_ForkNewThread, ilupython_GetPerThreadDataTech);
    }
#else
    fprintf(stderr, "Python thread support not configured into ILU!\n");
    exit(1);
#endif				/* (defined(ILU_OS_THREADED)) */
  }

  /*********** set up the C side of the world */

  multlang__Initialize();
  multlang__InitializeServer();

  theServer = ILU_C_InitializeServer("Server1", NULL, NULL, NULL, NULL, ilu_TRUE);
  if (theServer == NULL)
    {
      fprintf (stderr, "Can't create a server.\n");
      exit(1);
    }
  mult = multlang_Multiplier__CreateTrue ("theMultiplierObject", theServer, NULL);
  if (mult == NULL)
    {
      fprintf (stderr, "Can't create the multiplier object\n");
      exit(1);
    }
  else
    {
      printf ("Created Multiplier object <%s>\n",
	      ILU_C_SBHOfObject(mult));
    }
  if ((proof = ILU_C_PublishObject(mult)) == NULL)
    {
      fprintf (stderr, "Can't publish the multiplier object\n");
      exit(1);
    }
  ilu_free(proof);	/* no use for this */

  /*********** now set up the Python side of the world */

  PyRun_SimpleString("import sys ; sys.path = [ '' ] + sys.path");
  PyRun_SimpleString("import multlangimpl");

  /*********** OK, now find the squarer object *********/

  squarer = ILU_C_LookupObject ("Server2", "theSquarerObject",
				multlang_Squarer__MSType);
  if (squarer == NULL)
    {
      fprintf (stderr, "Can't find local squarer object\n");
      exit(1);
    }

  /*********** OK, ready to run */

  stat = ilu_TRUE;

  input = 21;
  val = multlang_Squarer_ObtainSquare (squarer, input, &env);
  if (ILU_C_SUCCESSFUL(&env)) {
    printf ("square of %u is %u.\n", input, val);
    stat = stat && ilu_TRUE;
  } else {
    printf("exception on multlang_Squarer_ObtainSquare(%u): \"%s\"\n",
	   input, env.returnCode);
    stat = stat & ilu_FALSE;
  }

  input = 0xFFFFFFF3;
  val = multlang_Squarer_ObtainSquare (squarer, input, &env);
  if (ILU_C_SUCCESSFUL(&env)) {
    printf ("square of %u is %u.\n", input, val);
    stat = stat & ilu_FALSE;
  } else {
    printf("exception on multlang_Squarer_ObtainSquare(%u), \"%s\"\n",
	   input, env.returnCode);
    stat = stat && ilu_TRUE;
  }

  input = 0xFFF3;
  val = multlang_Squarer_ObtainSquare (squarer, input, &env);
  if (ILU_C_SUCCESSFUL(&env)) {
    printf ("square of %u is %u.\n", input, val);
    stat = stat && ilu_TRUE;
  } else {
    printf("exception on multlang_Squarer_ObtainSquare(%u), \"%s\"\n",
	   input, env.returnCode);
    stat = stat & ilu_FALSE;
  }

  if (stat)
    printf ("All calls behaved as expected.\n");
  else
    printf ("Unexpected result for some call.\n");
  return ((stat) ? 0 : 1);
}
