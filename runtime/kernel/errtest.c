/*
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
/* needs to be linked with error.o */
/* Last tweaked by Mike Spreitzer September 22, 1998 11:17 pm PDT */

#include <stdio.h>

#include "iluntrnl.h"
#include "iluerrs.h"

ILU_DECL_ERR(NonOrdinal)
{
  int             val;
}
ILU_END_DECL_ERR;

ILU_DECL_ERR(TooBig)
{
  int             val;		/* the value that was too big */
  char           *s;		/* just for testing, give it a
				 * string */
}
ILU_END_DECL_ERR;

ILU_DEF_ERR(NonOrdinal, "A Non-ordinal value was passed to fib")
{
  /* no need to free anything */
}

ILU_DEF_ERR(TooBig, "Specified ordinal value was too big")
{
  free(e->s);
}

ILU_QUADEF(NonOrdinal);

void
_ilu_Assert(int v, char *msg)
{
  if (!v)
    printf("Assert failed:  message is <%s>\n", msg);
}

unsigned int
fib(int n, ilu_Error * error)
{
  *error = ILU_NO_ERR;
  if (n > 20) {
    ILU_BIND_ERR(TooBig, error, ev) {
      ev->val = n;
      ev->s = (char *) malloc(4);
      strcpy(ev->s, "foo");
    }
    ILU_END_BIND_ERR;
    return 0;
  } else if (n < 1)
    return ILU_ERR_CONS1(NonOrdinal, error, val, n, 0);
  else if (n == 1 || n == 2)
    return 1;
  else {
    return (fib(n - 1, error) + fib(n - 2, error));
  }
}

int main(int ac, char **av)
{
  ilu_Error       err1;
  unsigned int    n;
  int             i;

  if (ac < 2) {
    fprintf(stderr,
	    "Usage:  %s NUMBER => NUMBER'th Fibonacci number\n",
	    av[0]);
    exit(1);
  }
  i = atoi(av[1]);
  n = fib(i, &err1);

  ILU_ERR_SWITCH(err1) {
    ILU_SUCCESS_CASE
      printf("the %u%s fibonacci number is %u\n", i,
       (i == 1) ? "st" : (i == 2) ? "nd" : (i == 3) ? "rd" : "th",
	     n);
    ILU_ERR_CASE(NonOrdinal, ev)
      printf("Non-ordinal value %d specified\n", ev->val);
    ILU_ERR_ELSE
      printf("Unknown error <%s> -- <%s>\n",
	     ILU_ERR_NAME(err1), ILU_ERR_DESCRIPTION(err1));
  }
  ILU_ERR_ENDSWITCH;
  ilu_FreeError(err1);
  return (err1 != NULL);
}
