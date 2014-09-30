/* Command line test of simple binding name service
   To be used only for debugging. */
/** $Id: sbenumerate.c,v 1.10 1999/08/03 01:56:16 janssen Exp $
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

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>	/* for tolower() */

#include "ilu_simpbind.h"

static int
_casefree_ncmp(const ilu_string s1, const ilu_string s2,
		   ilu_cardinal n)
/* returns 0 if s1 == s2, -1 if s1 < s2, 1 if s1 > s2 */
{
  ilu_string      p1 = s1, p2 = s2;
  char            c1, c2;
  while (n > 0) {
    c1 = tolower(*p1);
    c2 = tolower(*p2);
    if (c1 < c2)
      return (-1);
    else if (c1 > c2)
      return (1);
    else if (*p1 == (char) 0)
      return (0);
    p1++;
    p2++;
    n--;
  }
  return 0;
}
     
static void print_sbh (ilu_simpbind_StringBindingHandle *sbh, void *ignored)
{
  printf ("  %s\n", (sbh == ILU_NIL) ? "?" : *sbh);
}

int main (int argc, char **argv)
{
  ilu_simpbind_Server sb;
  ILU_C_ENVIRONMENT env;
  ilu_integer count;
  ilu_simpbind_StringBindingHandleList *theList;
  char * pattern = "";
  char mooringinfo[] = "inmem";
  ilu_shortcardinal port;
  char server_sbh[2048];
  ilu_Class serverClass;

  if (argc != 2)
    {
      if (argc == 1) 
	;  /* null pattern */
      else  {
	fprintf (stderr, "Usage:  %s <pattern>\n",argv[0]);
	exit(1);    }
    }
  else
    pattern = argv[1];

  ilu_simpbind__Initialize();
  ilu_GetSimpleBindingSBH(server_sbh, sizeof(server_sbh));
  serverClass = ILU_C_FindILUClassByTypeName("ilu-simpbind.Server");

  sb = ILU_C_SBHToObject (server_sbh, serverClass, &env);
  if ((sb == ILU_NIL) || (! ILU_C_SUCCESSFUL(&env)))
    {
      fprintf(stderr, "Enumerate failed: could not find SB service\n");
      exit(1);
    }
  theList = ilu_simpbind_Server_Enumerate(sb, pattern, &env);
  if (! ILU_C_SUCCESSFUL(&env))
    {
      fprintf(stderr, "Enumerate failed with exception <%s>.\n",
	      ILU_C_EXCEPTION_ID(&env));
      exit(1);
    }
  else if (pattern[0] == 0)
    printf ("%d object%s%s\n", theList->_length,
	      (theList->_length == 1) ? "" : "s",
	      (theList->_length > 0) ? ":" : ".");
  else
    printf("%d object%s matching \"%s\"%s\n", theList->_length,
	   (theList->_length == 1) ? "" : "s",
	   pattern,
	   (theList->_length > 0) ? ":" : ".");

  ilu_simpbind_StringBindingHandleList_Every (theList, print_sbh, ILU_NIL);
  return 0;
}
