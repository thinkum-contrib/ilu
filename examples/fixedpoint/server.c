/** $Id: server.c,v 1.5 1999/08/03 01:59:06 janssen Exp $
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
/* Last edited by Mike Spreitzer October 8, 1998 10:54 pm PDT */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "fixedpoint_example.h"
#include <ilubnops.h>

#define STRINGIFY(a)		#a

static void usage(char *pname) {
  fprintf (stderr, "Usage:  %s [-p PINFO] [-t TINFO]\n", pname);
}

int main (int ac, char **av)
{
  ILU_C_Server s;
  fixedpoint_example_obj obj;
  int i;
  char *pinfo = "sunrpc";
  char *tinfo[10] = { "sunrpcrm", "tcp_0_0", ILU_NIL };
  char serverid[1000];

  for (i = 1;  i < ac; ) {
    if (strcmp(av[i], "-p") == 0) {
      if (i++ < ac)
	pinfo = av[i++];
      else {
	usage(av[0]);
	exit(1);
      }	
    } else if (strcmp(av[i], "-t") == 0) {
      int             j = 0;
      ++i;
      while ((i < ac) && (av[i][0] != '-'))
	tinfo[j++] = av[i++];
      tinfo[j] = ILU_NIL;
    } else {
      usage(av[0]);
      exit(1);
    }
  }

  fixedpoint_example__InitializeServer();

  strcpy (serverid, "FixedPointTest");
  s = ILU_C_InitializeServer (serverid, ILU_NIL, pinfo, tinfo, ILU_NIL, ilu_TRUE);
  obj = fixedpoint_example_obj__CreateTrue ( "0", s, ILU_NIL );
  ILU_C_PublishObject (obj);

  if (obj != ILU_NIL)
    {
      printf ("exported fixedpoint_example_obj %s\n", ILU_C_SBHOfObject(obj));
      ILU_C_Run( );
    }
  else
    {
      printf ("couldn't create fixedpoint_example_obj\n");
      exit(1);
    }
  return 1;
}

fixedpoint_example_length*
  server_fixedpoint_example_obj_bounce (fixedpoint_example_obj _handle,
					fixedpoint_example_length3* a,
					ILU_C_ENVIRONMENT *_status)
{
  char * cb_num;
  char * cb_denom;
  ilu_Error *err;
  char *bnerr;
  ilubignum_MaxIntType val;
  fixedpoint_example_length *retval;

  cb_num = ilubignum_AsString (a->_numerator, 10, &bnerr);
  if (bnerr != ILU_NIL) {
    ILU_C_RAISE_SYSTEM(_status,UNKNOWN,0,NO);
    return retval;
  };
  cb_denom = ilubignum_AsString (a->_type->denominator, 10, &bnerr);
  if (bnerr != ILU_NIL) {
    ILU_C_RAISE_SYSTEM(_status,UNKNOWN,0,NO);
    return retval;
  };
  printf ("server_fixedpoint_example_obj_bounce:  a is %s/%s\n",
	  cb_num, cb_denom);
  val = ilubignum_AsInteger (a->_numerator, &bnerr);
  if ((bnerr == ILU_NIL) && (val == 21)) {
    fixedpoint_example_length5 len5;
    len5._type = fixedpoint_example_length5__Type;
    len5._numerator = ilubignum_FromInteger (val, &bnerr);
    if (bnerr != ILU_NIL) {
      ILU_C_RAISE_SYSTEM(_status,UNKNOWN,0,NO);
      return retval;
    };
    fixedpoint_example__BindExceptionValue (_status, ex_fixedpoint_example_e1, &len5);
  } else {
    retval = CORBA_sequence_fixedpoint_example_length_allocbuf(1);
    retval->_type = fixedpoint_example_length__Type;
    if ((retval->_numerator = ilubignum_Copy(a->_numerator)) == ILU_NIL) {
      ILU_C_RAISE_SYSTEM(_status,UNKNOWN,0,NO);
      return retval;
    };
    ILU_C_SET_SUCCESSFUL(_status);
  }
  return retval;
}
