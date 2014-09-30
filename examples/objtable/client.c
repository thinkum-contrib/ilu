/** $Id: client.c,v 1.5 1999/08/03 01:58:05 janssen Exp $
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
/* Last edited by Mike Spreitzer October 9, 1998 10:16 am PDT */

#include <string.h>
#include <stdio.h>

#include "objtable.h"

int main (int ac, char **av)
{
  objtable_server s;
  objtable_file f1, f2, f3;
  objtable_filename name;
  char *sbh;
  ILU_C_ENVIRONMENT status;

  if (ac < 2)
    {
      fprintf (stderr, "Usage:  client SERVER-SBH\n");
      return 1;
    }

  objtable__Initialize();

  s = (objtable_server) ILU_C_SBHToObject (av[1], objtable_server__MSType, &status);
  if ((!ILU_C_SUCCESSFUL(&status)) || (s == ILU_NIL))
    {
      fprintf (stderr, "Can't create object from SBH <%s>\n", av[1]);
      return 1;
    }

  f1 = objtable_server_find_file (s, "foo", &status);
  if (!ILU_C_SUCCESSFUL(&status))
    {
      fprintf (stderr, "objtable_server_find_file(s, \"foo\") fails with exception <%s>\n",
	       ILU_C_EXCEPTION_ID(&status));
      return 1;
    }
  name = objtable_file_name (f1, &status);
  if (!ILU_C_SUCCESSFUL(&status))
    {
      fprintf (stderr, "objtable_file_name(f1) fails with exception <%s>\n",
	       ILU_C_EXCEPTION_ID(&status));
      return 1;
    }
  else
    fprintf (stderr, "name of file f1 is <%s>\n", name);

  f2 = ILU_C_CreateSurrogateObject (objtable_file__MSType,
				    "bar", f1->server, &status);
  if (!ILU_C_SUCCESSFUL(&status))
    {
      fprintf (stderr, "Can't create new surrogate object with ih \"bar\"\n, error <%s>\n",
	       ILU_C_EXCEPTION_ID(&status));
      return 1;
    }
  name = objtable_file_name (f2, &status);
  if (!ILU_C_SUCCESSFUL(&status))
    {
      fprintf (stderr, "objtable_file_name(f2) fails with exception <%s>\n",
	       ILU_C_EXCEPTION_ID(&status));
      return 1;
    }
  else
    fprintf (stderr, "name of file f2 is <%s>\n", name);

  f3 = ILU_C_CreateSurrogateObject (objtable_file__MSType,
				    "bletch", f1->server, &status);
  if (!ILU_C_SUCCESSFUL(&status))
    {
      fprintf (stderr, "Can't create new surrogate object with ih \"bletch\"\n, error <%s>\n",
	       ILU_C_EXCEPTION_ID(&status));
      return 1;
    }
  name = objtable_file_name (f3, &status);
  if (!ILU_C_SUCCESSFUL(&status))
    {
      fprintf (stderr, "objtable_file_name(f3) fails with exception <%s>\n",
	       ILU_C_EXCEPTION_ID(&status));
      return 1;
    }
  else
    fprintf (stderr, "name of file f3 is <%s>\n", name);

  return 0;
}

