/** $Id: server.c,v 1.27 1999/08/03 01:58:40 janssen Exp $
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
/* Last edited by Mike Spreitzer October 8, 1998 11:10 pm PDT */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>

#include "iioptest1.h"

ILU_C_Server theServer = ILU_NIL;

int main (int ac, char **av)
{
  iioptest1_ext_obj	obj;

  ilu_string	pinfo = "iiop_1_0_1";
  ilu_string	tinfo[10] = { "tcp_0_0", ILU_NIL };
  int		i = 1;

  iioptest1__InitializeServer();
  while (i < ac) {
    if (strcmp(av[i], "-p") == 0) {
      if (i++ < ac)
	pinfo = av[i++];
      else
	goto usage;
    } else if (strcmp(av[i], "-t") == 0) {
      int j = 0;
      ++i;
      while ((i < ac) && (av[i][0] != '-'))
	tinfo[j++] = av[i++];
      tinfo[j] = ILU_NIL;
    } else
      goto usage;
  }

  theServer = ILU_C_InitializeServer("IIOPTest1Server", NULL, pinfo, tinfo,
				     ILU_NIL, ilu_TRUE);

  if (theServer == NULL) {
    fprintf(stderr, "*** Error, Couldn't create server!\n");
    exit(1);
  }
  obj = iioptest1_xilu_obj__CreateTrue("obj", theServer, NULL);
  if (obj == NULL) {
   fprintf(stderr,"*** Error, couldn't create object!\n");
    exit(1);
  }
  if (ILU_C_PublishObject(obj) == NULL) {
    fprintf(stderr, "*** Error, can't publish object!\n");
    exit(1);
  }
  fprintf(stdout, "exported %s\n", ILU_C_IOROfObject(obj));
  fprintf(stdout, "also %s\n", ILU_C_SBHOfObject(obj));
  fflush(stdout);
  ILU_C_Run();
  return 1;
usage:
  fprintf(stderr, "Usage: %s [-t tinfo [tinfo...]] [-p pinfo]\n", av[0]);
  return 2;
}

void server_iioptest1_xilu_obj_test_void (iioptest1_ext_obj self, CORBA_Environment *status)
{
}

#define TEST_NUM(testname,ctype) ctype server_iioptest1_xilu_obj_test_ ## testname (iioptest1_xilu_obj self, ctype a1, ctype *a2, ctype *a3, CORBA_Environment *status) \
{ \
  *a2 = a1 * a1 * a1; \
  *a3 = (*a3) * (*a3) * (*a3); \
  return *a2; \
} \
 \

TEST_NUM(short,ilu_shortinteger)
TEST_NUM(long,ilu_integer)

ilu_longinteger server_iioptest1_xilu_obj_test_longlong (iioptest1_xilu_obj self, ilu_longinteger a1, ilu_longinteger *a2, ilu_longinteger *a3, CORBA_Environment *status)
{
  ilu_integer tmp = *a3;
  *a2 = *a3;
  *a3 = a1;
  return tmp;
}


TEST_NUM(ushort,ilu_shortcardinal)
TEST_NUM(ulong,ilu_cardinal)

ilu_longcardinal server_iioptest1_xilu_obj_test_ulonglong (iioptest1_xilu_obj self, ilu_longcardinal a1, ilu_longcardinal *a2, ilu_longcardinal *a3, CORBA_Environment *status)
{
  ilu_longcardinal tmp = *a3;
  *a2 = *a3;
  *a3 = a1;
  return tmp;
}


TEST_NUM(float,ilu_shortreal)
TEST_NUM(double,ilu_real)

ilu_longreal server_iioptest1_xilu_obj_test_longdouble (iioptest1_xilu_obj self, ilu_longreal a1, ilu_longreal *a2, ilu_longreal *a3, CORBA_Environment *status)
{
  *a2 = a1;
  *a3 = *a3;
  return a1;
}

TEST_NUM(octet,ilu_byte)

CORBA_boolean server_iioptest1_xilu_obj_test_boolean (iioptest1_xilu_obj self, CORBA_boolean a1, CORBA_boolean *a2, CORBA_boolean *a3, CORBA_Environment *status)
{
  *a2 = ! a1;
  *a3 = ! *a3;
  return (! a1);
}

static char * lstrcat (ilu_string str1, ilu_string str2)
{
  char *newstr = ilu_malloc(strlen(str1) + strlen(str2) + 1);
  if (newstr == ILU_NIL)
    return ILU_NIL;
  strcpy (newstr, str1);
  strcpy (newstr + strlen(str1), str2);
  return newstr;
}

iioptest1_string server_iioptest1_xilu_obj_test_string (iioptest1_xilu_obj self, iioptest1_string a1, iioptest1_string *a2, iioptest1_string *a3, CORBA_Environment *status)
{
  *a2 = (iioptest1_string) lstrcat((ilu_string) a1, (ilu_string) a1);
  *a3 = (iioptest1_string) lstrcat((ilu_string) *a3, (ilu_string) *a3);
  return (iioptest1_string) ILU_C_Strdup((ilu_CString) *a2);
}

CORBA_char server_iioptest1_xilu_obj_test_char (iioptest1_xilu_obj self, CORBA_char a1, CORBA_char *a2, CORBA_char *a3, CORBA_Environment *status)
{
  *a2 = a1;
  *a3 = *a3;
  return a1;
}

CORBA_wchar server_iioptest1_xilu_obj_test_wchar (iioptest1_xilu_obj self, CORBA_wchar a1, CORBA_wchar *a2, CORBA_wchar *a3, CORBA_Environment *status)
{
  *a2 = a1;
  *a3 = *a3;
  return a1;
}

static ilu_cardinal wstrlen(CORBA_wchar *  s)
{
  register int i;
  if (s == ILU_NIL)
    return 0;
  for (i = 0;  s[i] != 0;  i++)
    ;
  return (ilu_cardinal) i;
}

static void wstrcpy (CORBA_wchar *  to, CORBA_wchar *  from)
{
  register int i;
  for (i = 0;  from[i] != 0;  i++)
    to[i] = from[i];
  to[i] = 0;
}

static CORBA_wchar *  wstrdup(CORBA_wchar *  s)
{
  CORBA_wchar *  s2;
  if (s == ILU_NIL)
    return ILU_NIL;
  s2 = ilu_malloc((wstrlen(s) + 1) * sizeof(CORBA_wchar));
  if (s2 != ILU_NIL)
    wstrcpy(s2, s);
  return s2;
}

static CORBA_wchar *  wstrcat (CORBA_wchar *  s1, CORBA_wchar *  s2)
{
  ilu_cardinal len = wstrlen(s1) + wstrlen(s2) + 1;
  CORBA_wchar *  out;

  out = ilu_malloc(len * sizeof(CORBA_wchar));
  if (out == ILU_NIL)
    return out;
  out[0] = 0;
  if (s1 != ILU_NIL)
    wstrcpy (out, s1);
  if (s2 != ILU_NIL)
    wstrcpy (out + wstrlen(out), s2);
  return out;
}

iioptest1_UnicodeString server_iioptest1_xilu_obj_test_wstring (iioptest1_xilu_obj self, iioptest1_UnicodeString a1, iioptest1_UnicodeString *a2, iioptest1_UnicodeString *a3, CORBA_Environment *status)
{
  *a2 = wstrcat(a1, a1);
  *a3 = wstrcat(*a3, *a3);
  return wstrdup(*a2);
}

iioptest1_xilu_obj server_iioptest1_xilu_obj_test_obj (iioptest1_xilu_obj self, iioptest1_xilu_obj a1, iioptest1_xilu_obj *a2, iioptest1_xilu_obj *a3, CORBA_Environment *status)
{
  *a2 = (iioptest1_xilu_obj) CORBA_Object_duplicate(a1, status);
  if (!ILU_C_SUCCESSFUL(status))
    return ILU_NIL;
  *a3 = (iioptest1_xilu_obj) CORBA_Object_duplicate(*a3, status);
  if (!ILU_C_SUCCESSFUL(status))
    return ILU_NIL;
  return ((iioptest1_xilu_obj) CORBA_Object_duplicate(a1, status));
}

static void cube_rec1 (iioptest1_rec1 *in, iioptest1_rec1 *out)
{
  out->f1 = in->f1 * in->f1 * in->f1;
  out->f2 = in->f2 * in->f2 * in->f2;
  out->f3 = in->f3 * in->f3 * in->f3;
  out->f4 = in->f4 * in->f4 * in->f4;
  out->f5 = in->f5 * in->f5 * in->f5;
  out->f6 = ! in->f6;
  out->f7 = in->f7 * in->f7 * in->f7;
  out->f8 = in->f8 * in->f8 * in->f8;
  out->f9 = (iioptest1_string) lstrcat((ilu_string) (in->f9), (ilu_string) (in->f9));
}

iioptest1_rec1 *server_iioptest1_xilu_obj_test_var_record (iioptest1_xilu_obj self, iioptest1_rec1 *a1, iioptest1_rec1 **a2, iioptest1_rec1 *a3, CORBA_Environment *status)
{
  iioptest1_rec1 *retval;
  iioptest1_rec1 *a2val;
  iioptest1_string p;

  retval = (iioptest1_rec1 *) ilu_malloc(sizeof(iioptest1_rec1));
  cube_rec1(a1, retval);

  a2val  = (iioptest1_rec1 *) ilu_malloc(sizeof(iioptest1_rec1));
  cube_rec1(a1, a2val);
  *a2 = a2val;

  p = a3->f9;
  cube_rec1(a3, a3);
  ilu_free(p);

  return retval;
}

iioptest1_rec2 server_iioptest1_xilu_obj_test_fixed_record (iioptest1_xilu_obj self, iioptest1_rec2 *a1, iioptest1_rec2 *a2, iioptest1_rec2 *a3, CORBA_Environment *status)
{
  iioptest1_rec2 retval;
  iioptest1_string p;

  retval.f1 = a1->f1 * a1->f1 * a1->f1;
  retval.f2 = a1->f2 * a1->f2 * a1->f2;

  *a2 = retval;

  a3->f1 = a3->f1 * a3->f1 * a3->f1;
  a3->f2 = a3->f2 * a3->f2 * a3->f2;

  return retval;
}

iioptest1_seq1 *server_iioptest1_xilu_obj_test_sequence (iioptest1_xilu_obj self, iioptest1_seq1 *a1, iioptest1_seq1 **a2, iioptest1_seq1 *a3, CORBA_Environment *status)
{
  iioptest1_seq1 *retval, *ret_a2;
  iioptest1_rec2 junk;
  int i;

  retval = iioptest1_seq1__alloc();
  retval->_maximum = a1->_length;
  retval->_length = a1->_length;
  retval->_buffer = CORBA_sequence_iioptest1_rec2_allocbuf(a1->_length);
  *a2 = iioptest1_seq1__alloc();
  (*a2)->_maximum = a1->_length;
  (*a2)->_length = a1->_length;
  (*a2)->_buffer = CORBA_sequence_iioptest1_rec2_allocbuf(a1->_length);
  for (i = 0;  i < a1->_length;  i++)
    {
      retval->_buffer[i].f1 = a1->_buffer[i].f1 * a1->_buffer[i].f1 * a1->_buffer[i].f1;
      retval->_buffer[i].f2 = a1->_buffer[i].f2 * a1->_buffer[i].f2 * a1->_buffer[i].f2;
      (*a2)->_buffer[i].f1 = a1->_buffer[i].f1 * a1->_buffer[i].f1 * a1->_buffer[i].f1;
      (*a2)->_buffer[i].f2 = a1->_buffer[i].f2 * a1->_buffer[i].f2 * a1->_buffer[i].f2;
    }
  for (i = 0;  i < a3->_length;  i++)
    {
      a3->_buffer[i].f1 = a3->_buffer[i].f1 * a3->_buffer[i].f1 * a3->_buffer[i].f1;
      a3->_buffer[i].f2 = a3->_buffer[i].f2 * a3->_buffer[i].f2 * a3->_buffer[i].f2;
    }
  return retval;      
}

iioptest1_arr1_slice *
  server_iioptest1_xilu_obj_test_var_array (iioptest1_xilu_obj self,
					   iioptest1_arr1 a1,
					   iioptest1_arr1_slice **a2,
					   iioptest1_arr1 a3, CORBA_Environment *status)
{
  iioptest1_arr1_slice *retval, *ret_a2;
  int i, j;
  iioptest1_string p;

  retval = iioptest1_arr1__alloc();
  ret_a2 = iioptest1_arr1__alloc();
  for (i = 0;  i < 2;  i++)
    for (j = 0;  j < 3;  j++)
      {
	cube_rec1(&a1[i][j], &retval[i][j]);
	cube_rec1(&a1[i][j], &ret_a2[i][j]);
	p = a3[i][j].f9;
	cube_rec1(&a3[i][j], &a3[i][j]);
	ilu_free(p);
      }
  *a2 = ret_a2;
  return retval;
}

static void cube_rec2 (iioptest1_rec2 *input, iioptest1_rec2 *output)
{
  output->f1 = (input->f1 * input->f1 * input->f1);
  output->f2 = (input->f2 * input->f2 * input->f2);
}

iioptest1_arr4_slice *
  server_iioptest1_xilu_obj_test_fixed_array (iioptest1_xilu_obj self,
					     iioptest1_arr4 a1,
					     iioptest1_arr4 a2,
					     iioptest1_arr4 a3, CORBA_Environment *status)
{
  iioptest1_arr4_slice *retval;
  int i, j;
  iioptest1_string p;

  retval = iioptest1_arr4__alloc();
  for (i = 0;  i < 2;  i++) {
    for (j = 0;  j < 3;  j++) {
      cube_rec2 (&a1[i][j], &retval[i][j]);
      cube_rec2 (&a1[i][j], &a2[i][j]);
      cube_rec2 (&a3[i][j], &a3[i][j]);
    }
  }
  return retval;
}

static void cube_union1 (iioptest1_union1 *v1, iioptest1_union1 *v2)
{
  iioptest1_string p = ILU_NIL;

  switch (v1->_d)
    {
    case iioptest1_a:
      if (v1 == v2)
	p = v2->_u.a.f9;
      cube_rec1(&v1->_u.a, &v2->_u.a);
      if (p != ILU_NIL)
	ilu_free(p);
      break;
    case iioptest1_b:
      {
	int i, j;
	for (i = 0;  i < 2;  i++)
	  for (j = 0;  j < 3;  j++)
	    {
	      if (v1 == v2)
		p = v2->_u.b[i][j].f9;
	      cube_rec1(&v1->_u.b[i][j], &v2->_u.b[i][j]);
	      if (p != ILU_NIL)
		ilu_free(p);
	      p = ILU_NIL;
	    }
      }
      break;
    case iioptest1_c:
      {
	int i;
	CORBA_free(v2->_u.c._buffer);
	v2->_u.c._maximum = v1->_u.c._length;
	v2->_u.c._buffer = CORBA_sequence_iioptest1_rec2_allocbuf(v1->_u.c._length);
	v2->_u.c._length = v1->_u.c._length;
	for (i = 0;  i < v1->_u.c._length;  i++)
	  {
	    v2->_u.c._buffer[i].f1 = v1->_u.c._buffer[i].f1 * v1->_u.c._buffer[i].f1 * v1->_u.c._buffer[i].f1;
	    v2->_u.c._buffer[i].f2 = v1->_u.c._buffer[i].f2 * v1->_u.c._buffer[i].f2 * v1->_u.c._buffer[i].f2;
	  }
      }
      break;
    default:
      v2->_u.d = v1->_u.d;
      break;
    }
}

iioptest1_union1 *server_iioptest1_xilu_obj_test_var_union (iioptest1_xilu_obj self, iioptest1_union1 *a1, iioptest1_union1 **a2, iioptest1_union1 *a3, CORBA_Environment *status)
{
  iioptest1_union1 *retval;
  iioptest1_union1 *a2val;
  iioptest1_string p;

  retval = (iioptest1_union1 *) ilu_malloc(sizeof(iioptest1_union1));
  cube_union1(a1, retval);

  a2val  = (iioptest1_union1 *) ilu_malloc(sizeof(iioptest1_union1));
  cube_union1(a1, a2val);
  *a2 = a2val;

  cube_union1(a3, a3);

  return retval;
}

void transform_union2 (iioptest1_union2 *input, iioptest1_union2 *output)
{
  switch (input->_d) {

  case 0:
    output->_d = 1;
    output->_u.real = -1.0 * input->_u.integer;
    break;

  case 1:
    output->_d = 0;
    output->_u.integer = (ilu_integer) input->_u.real;
    break;
  }
}

iioptest1_union2 server_iioptest1_xilu_obj_test_fixed_union (iioptest1_xilu_obj self, iioptest1_union2 *a1, iioptest1_union2 *a2, iioptest1_union2 *a3, CORBA_Environment *status)
{
  transform_union2(a3, a2);
  transform_union2(a1, a3);

  return *a2;
}

iioptest1_opt1 server_iioptest1_xilu_obj_test_optional (iioptest1_xilu_obj self, iioptest1_opt1 a1, iioptest1_opt1 *a2, iioptest1_opt1 *a3, CORBA_Environment *status)
{
  iioptest1_rec2 *retval;
  iioptest1_rec2 *a2val;

  if (*a3 != ILU_NIL) {
    a2val = (iioptest1_opt1) ilu_malloc(sizeof(iioptest1_rec2));
    retval = (iioptest1_opt1) ilu_malloc(sizeof(iioptest1_rec2));
    cube_rec2(*a3, a2val);
    cube_rec2(*a3, retval);
  } else {
    a2val = ILU_NIL;
    retval = ILU_NIL;
  }
  
  if (a1 != ILU_NIL) {
    *a3 = (iioptest1_opt1) ilu_malloc(sizeof(iioptest1_rec2));
    cube_rec2(a1, *a3);
  } else {
    iioptest1_opt1__Free (a3);
    *a3 = ILU_NIL;
  }
  *a2 = a2val;
  return retval;  
}

iioptest1_enum1 server_iioptest1_xilu_obj_test_enumeration (iioptest1_xilu_obj self,
							   iioptest1_enum1 a1,
							   iioptest1_enum1 *a2,
							   iioptest1_enum1 *a3, CORBA_Environment *status)
{
  iioptest1_enum1 retval;

  retval = (iioptest1_enum1) ((((unsigned int) a1) + 1) % 5);
  if ((int) (*a3) == 0)
    *a3 = (iioptest1_enum1) 4;
  else
    *a3 = (iioptest1_enum1) (((int)(*a3)) - 1);
  *a2 = retval;

  return retval;
}

void server_iioptest1_xilu_obj_test_throw (iioptest1_xilu_obj self, ilu_integer case_num, CORBA_Environment *status)
{
  if (case_num <= 0)
    {
      iioptest1_x1_rec *r = (iioptest1_x1_rec *) ilu_must_malloc(sizeof(iioptest1_x1_rec));
      r->case_num = case_num;
      status->returnCode = ex_iioptest1_x1;
      status->_major = CORBA_USER_EXCEPTION;
      status->ptr = (void *) r;
      status->freeRoutine = (void (*)(void *)) 0;
    }
  else if ((case_num % 2) == 0)
    {
      iioptest1_x2_rec *r = (iioptest1_x2_rec *) ilu_must_malloc(sizeof(iioptest1_x2_rec));
      r->case_num = case_num;
      r->obj = CORBA_OBJECT_NIL;
      status->returnCode = ex_iioptest1_x2;
      status->_major = CORBA_USER_EXCEPTION;
      status->ptr = (void *) r;
      status->freeRoutine = (void (*)(void *)) iioptest1_x2_rec__Free;
    }
  else
    {
      iioptest1_x2_rec *r = (iioptest1_x2_rec *) ilu_must_malloc(sizeof(iioptest1_x2_rec));
      r->case_num = case_num;
      r->obj = (iioptest1_xilu_obj) CORBA_Object_duplicate(self, status);
      if (!ILU_C_SUCCESSFUL(status))
	return;
      status->returnCode = ex_iioptest1_x2;
      status->_major = CORBA_USER_EXCEPTION;
      status->ptr = (void *) r;
      status->freeRoutine = (void (*)(void *)) iioptest1_x2_rec__Free;
    }
}

void server_iioptest1_xilu_obj_please_exit (iioptest1_xilu_obj self, CORBA_Environment *status)
{
  exit(0);
}

#define SUBDECL(methodname, ctype)  ctype server_iioptest1_obj_ ## methodname (iioptest1_obj self, ctype a1, ctype *a2, ctype *a3, CORBA_Environment *status) { return server_iioptest1_xilu_obj_ ## methodname ((iioptest1_xilu_obj) self, a1, a2, a3, status); } \
ctype server_iioptest1_ext_obj_ ## methodname (iioptest1_ext_obj self, ctype a1, ctype *a2, ctype *a3, CORBA_Environment *status) { return server_iioptest1_xilu_obj_ ## methodname ((iioptest1_xilu_obj) self, a1, a2, a3, status); }

void server_iioptest1_obj_test_void (iioptest1_obj self, CORBA_Environment *status)
{
  return;
}

void server_iioptest1_ext_obj_test_void (iioptest1_ext_obj self, CORBA_Environment *status)
{
  return;
}

SUBDECL(test_short,CORBA_short)
SUBDECL(test_long,CORBA_long)
SUBDECL(test_ushort,CORBA_unsigned_short)
SUBDECL(test_ulong,CORBA_unsigned_long)
SUBDECL(test_float,CORBA_float)
SUBDECL(test_double,CORBA_double)
SUBDECL(test_boolean,CORBA_boolean)
SUBDECL(test_char,CORBA_char)
SUBDECL(test_octet,CORBA_octet)
SUBDECL(test_string,iioptest1_string)
SUBDECL(test_longlong,ilu_longinteger)
SUBDECL(test_ulonglong,ilu_longcardinal)
SUBDECL(test_wchar,CORBA_wchar)
SUBDECL(test_wstring,CORBA_wchar *)
SUBDECL(test_longdouble,ilu_longreal)

void server_iioptest1_obj_test_throw (iioptest1_obj self, ilu_integer case_num, CORBA_Environment *status)
{
  server_iioptest1_xilu_obj_test_throw ((iioptest1_xilu_obj) self, case_num, status);
}

void server_iioptest1_obj_please_exit (iioptest1_obj self, CORBA_Environment *status)
{
  server_iioptest1_xilu_obj_please_exit((iioptest1_xilu_obj) self, status);
}

void server_iioptest1_ext_obj_test_throw (iioptest1_ext_obj self, ilu_integer case_num, CORBA_Environment *status)
{
  server_iioptest1_xilu_obj_test_throw ((iioptest1_xilu_obj) self, case_num, status);
}

void server_iioptest1_ext_obj_please_exit (iioptest1_ext_obj self, CORBA_Environment *status)
{
  server_iioptest1_xilu_obj_please_exit((iioptest1_xilu_obj) self, status);
}

iioptest1_ext_obj server_iioptest1_ext_obj_test_obj (iioptest1_ext_obj self, iioptest1_obj a1, iioptest1_obj *a2, iioptest1_obj *a3, CORBA_Environment *status)
{
  return (server_iioptest1_xilu_obj_test_obj((iioptest1_xilu_obj) self, a1, a2, a3, status));
}

iioptest1_rec1 *server_iioptest1_ext_obj_test_var_record (iioptest1_ext_obj self, iioptest1_rec1 *a1, iioptest1_rec1 **a2, iioptest1_rec1 *a3, CORBA_Environment *status)
{
  return (server_iioptest1_xilu_obj_test_var_record ((iioptest1_xilu_obj) self, a1, a2, a3, status));
}

iioptest1_rec2 server_iioptest1_ext_obj_test_fixed_record (iioptest1_ext_obj self, iioptest1_rec2 *a1, iioptest1_rec2 *a2, iioptest1_rec2 *a3, CORBA_Environment *status)
{
  return (server_iioptest1_xilu_obj_test_fixed_record ((iioptest1_xilu_obj) self, a1, a2, a3, status));
}

iioptest1_seq1 *server_iioptest1_ext_obj_test_sequence (iioptest1_ext_obj self, iioptest1_seq1 *a1, iioptest1_seq1 **a2, iioptest1_seq1 *a3, CORBA_Environment *status)
{
  return (server_iioptest1_xilu_obj_test_sequence ((iioptest1_xilu_obj) self, a1, a2, a3, status));
}

iioptest1_arr1_slice *
  server_iioptest1_ext_obj_test_var_array (iioptest1_ext_obj self,
					    iioptest1_arr1 a1,
					    iioptest1_arr1_slice **a2,
					    iioptest1_arr1 a3, CORBA_Environment *status)
{
  return (server_iioptest1_xilu_obj_test_var_array ((iioptest1_xilu_obj) self, a1, a2, a3, status));
}

iioptest1_arr4_slice *
  server_iioptest1_ext_obj_test_fixed_array (iioptest1_ext_obj self,
					     iioptest1_arr4 a1,
					     iioptest1_arr4 a2,
					     iioptest1_arr4 a3, CORBA_Environment *status)
{
  return (server_iioptest1_xilu_obj_test_fixed_array ((iioptest1_xilu_obj) self, a1, a2, a3, status));
}

iioptest1_union1 *
  server_iioptest1_ext_obj_test_var_union (iioptest1_ext_obj self,
					   iioptest1_union1 *a1,
					   iioptest1_union1 **a2,
					   iioptest1_union1 *a3, CORBA_Environment *status)
{
  return (server_iioptest1_xilu_obj_test_var_union ((iioptest1_xilu_obj) self, a1, a2, a3, status));
}

iioptest1_union2
  server_iioptest1_ext_obj_test_fixed_union (iioptest1_ext_obj self,
					     iioptest1_union2 *a1,
					     iioptest1_union2 *a2,
					     iioptest1_union2 *a3, CORBA_Environment *status)
{
  return (server_iioptest1_xilu_obj_test_fixed_union ((iioptest1_xilu_obj) self, a1, a2, a3, status));
}

iioptest1_enum1
  server_iioptest1_ext_obj_test_enumeration (iioptest1_ext_obj self,
					     iioptest1_enum1 a1,
					     iioptest1_enum1 *a2,
					     iioptest1_enum1 *a3, CORBA_Environment *status)
{
  return (server_iioptest1_xilu_obj_test_enumeration ((iioptest1_xilu_obj) self, a1, a2, a3, status));
}

iioptest1_opt1
  server_iioptest1_ext_obj_test_optional (iioptest1_ext_obj self,
					  iioptest1_opt1 a1,
					  iioptest1_opt1 *a2,
					  iioptest1_opt1 *a3, CORBA_Environment *status)
{
  return (server_iioptest1_xilu_obj_test_optional ((iioptest1_xilu_obj) self, a1, a2, a3, status));
}

void
  server_iioptest1_xilu_obj_test_asynchronous (iioptest1_xilu_obj self,
					       CORBA_long a1, CORBA_Environment *status)
{
  /*
    fprintf (stdout, "server_iioptest1_xilu_obj_test_asynchronous %d\n", a1);
    */
}

void
  server_iioptest1_ext_obj_test_asynchronous (iioptest1_ext_obj self,
					      CORBA_long a1, CORBA_Environment *status)
{
  server_iioptest1_xilu_obj_test_asynchronous ((iioptest1_xilu_obj) self, a1, status);
}
