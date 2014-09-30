/** $Id: client.c,v 1.7 1999/08/03 01:59:07 janssen Exp $
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
/* Last edited by Mike Spreitzer October 8, 1998 10:52 pm PDT */

#include <stdlib.h>		/* for atoi */
#include <stdio.h>
#include <string.h>

#include <unistd.h>		/* for gethostname */
#include <math.h>		/* for sqrt */

#include <ilubnops.h>

#include "fixedpoint_example.h"

static void
Usage(char *pname)
{
  fprintf(stderr, "Usage:  %s\n", pname);
  exit(1);
}

int
main(int ac, char **av)
{
  char            sid[1000];
  fixedpoint_example_obj handle;
  fixedpoint_example_length *len;
  fixedpoint_example_length3 *len3;
  ilu_CString     retstr;
  char *err, *progname;
  ILU_C_ENVIRONMENT e;
  int i;
  char seedval[1000];

  strcpy (seedval, "23");
  progname = av[0];
  for (i = 1;  i < ac;  i++) {
    if (strcmp(av[i], "-v") == 0) {
      if (++i < ac) {
	strcpy (seedval, av[i]);
      } else {
	Usage(progname);
      }
    } else {
      Usage(progname);
    }
  };

  fixedpoint_example__Initialize();
  handle = ILU_C_LookupObject("FixedPointTest", "0", fixedpoint_example_obj__MSType);
  if (handle == NULL) {
    fprintf(stderr, "error:  Can't obtain object <%s>\n", sid);
    exit(1);
  }

  len3 = CORBA_sequence_fixedpoint_example_length3_allocbuf(1);
  len3->_type = fixedpoint_example_length3__Type;
  len3->_numerator = ilubignum_FromString (seedval, ILU_NIL, 0, &err);
  if (err != ILU_NIL) {
    fprintf (stderr, "Error <%s> converting seedval \"%s\" to bignum\n",
	     err, seedval);
    exit(1);
  };
  len = fixedpoint_example_obj_bounce (handle, len3, &e);
  fixedpoint_example_length3__Free(len3);
  ilu_free(len3);
  if (ILU_C_SUCCESSFUL(&e)) {
    char * cb_num, * cb_denom, * bnerr;
    cb_num = ilubignum_AsString (len->_numerator, 10, &bnerr);
    if (bnerr != ILU_NIL) {
      fprintf (stderr, "Error <%s> converting bignum to string\n", bnerr);
      exit(1);
    };
    cb_denom = ilubignum_AsString (len->_type->denominator, 10, &bnerr);
    if (bnerr != ILU_NIL) {
      fprintf (stderr, "Error <%s> converting bignum to string\n", bnerr);
      exit(1);
    };
    printf ("fixedpoint_example_obj_bounce:  retval is %s/%s\n",
	    cb_num, cb_denom);
    ilu_free(cb_num);
    ilu_free(cb_denom);
    fixedpoint_example_length__Free(len);
    ilu_free(len);
  } else {
    fixedpoint_example_length5 *len5;
    char * cb_num, * cb_denom, * bnerr;

    if (CORBA_exception_id(&e) == ex_fixedpoint_example_e1) {
      len5 = CORBA_exception_value(&e);
      cb_num = ilubignum_AsString (len5->_numerator, 10, &bnerr);
      if (bnerr != ILU_NIL) {
	fprintf (stderr, "Error <%s> converting bignum to string\n", bnerr);
	exit(1);
      };
      cb_denom = ilubignum_AsString (len5->_type->denominator, 10, &bnerr);
      if (bnerr != ILU_NIL) {
	fprintf (stderr, "Error <%s> converting bignum to string\n", bnerr);
	exit(1);
      };
      fprintf (stderr, "bounce() raises exception fixedpoint-example.e1 with value %s/%s.\n",
	       cb_num, cb_denom);
      ilu_free(cb_num);
      ilu_free(cb_denom);
    } else {
      fprintf (stderr, "bounce() raises exception <%s>\n", CORBA_exception_id(&e));
    }
    CORBA_exception_free(&e);
  }
  return 0;
}
