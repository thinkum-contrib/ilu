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

$Id: testprg.c,v 1.7 1999/08/03 01:59:58 janssen Exp $
*/
#include "ilubignm.h"

#include <stdlib.h>
#include <stdio.h>

#define TRUE  1
#define FALSE 0
typedef int Boolean;

#define AND  &&
#define OR   ||
#define NOT  !

#include "ilubnops.h"

static char rcs_id[] = "$Header: /var/tmp/tape/RCS/testprg.c,v 1.7 1999/08/03 01:59:58 janssen Exp $";
static char xerox_copyright[] = "\
\
 Copyright 1989 Xerox Corporation\
 All Rights Reserved";

main (ac, av, envp)

int ac;
char **av;
char **envp;

{
  char *err = NULL;
  ilubignum_Value a, b;
  int v;
  ilubignum_card8 * cb;
  char *pending;
  ilubignum_card32 passed, negative;
  unsigned int len;

  if (ac < 2) {
    fprintf(stderr, "Usage:  %s NUMBER\n");
    exit(1);
  };

  a = ilubignum_FromString (av[1], &pending, (ac > 2) ? atoi(av[2]) : 0, &err);
  if (err != NULL) {
    fprintf (stderr, "Error in ilubignum_FromString:  %s\n", err);
    exit(1);
  } else {
    printf ("converted \"%s\" successfully, pending is %x\n", av[1], pending);
  }
  pending = ilubignum_AsString (a, 10, &err);
  if (err != NULL) {
    fprintf (stderr, "Error in ilubignum_AsString:  %s\n", err);
    exit(1);
  } else {
    printf ("converted a successfully to \"%s\"\n", pending);
  }
  free(pending);
  cb = ilubignum_AsBytes (a, &len, &passed, &negative);
  if (cb == NULL) {
    fprintf (stderr, "Error in ilubignum_AsBytes\n");
    exit(1);
  } else {
    int i;
    printf ("%lu byte%s %s", (unsigned long) len, (len == 1) ? "" : "s", (len == 1) ? "is" : "are");
    for (i = 0;  i < len;  i++)
      printf (" 0x%02x", cb[i]);
    printf (", passed=%s, negative=%s\n",
	    passed ? "TRUE" : "FALSE",
	    negative ? "TRUE" : "FALSE");
  }
  b = ilubignum_FromBytes (len, cb, negative);
  if (b == NULL) {
    fprintf (stderr, "Error in ilubignum_FromBytes\n");
    exit(1);
  };
  pending = ilubignum_AsString (b, 10, &err);
  if (err != NULL) {
    fprintf (stderr, "Error in ilubignum_AsString:  %s\n", err);
    exit(1);
  } else {
    printf ("converted b successfully to \"%s\"\n", pending);
  }
  v = ilubignum_Compare (a, b);
  printf ("result of comparison is %d\n", v);
  return 0;
}
