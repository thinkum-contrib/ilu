/** $Id: client.c,v 1.19 1999/08/03 01:58:40 janssen Exp $
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
/* Last edited by Mike Spreitzer October 8, 1998 11:09 pm PDT */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>

#include "iioptest1.h"

ilu_cardinal ntests = 0;
ilu_cardinal nerrs = 0;

#define TEST_NUM(testname,typename)	static void test_ ## testname (iioptest1_obj self, int i) \
{ \
  CORBA_Environment env; \
  typename val1 = (typename) i - 117; \
  typename val2 = 0; \
  typename val3 = (typename) (i + 20); \
  typename result; \
  typename expected2 = ((typename) (i - 117)) * ((typename) (i - 117)) * ((typename) (i - 117)); \
  typename expected3 = ((typename) (i + 20)) * ((typename) (i + 20)) * ((typename) (i + 20)); \
\
  ntests += 1; \
\
  result = iioptest1_obj_test_ ## testname (self, val1, &val2, &val3, &env); \
\
  if (env._major == CORBA_NO_EXCEPTION) \
    { \
      if (val2 != expected2 || val3 != expected3 || result != expected2) \
	{ \
	  fprintf (stderr, "** Error on iioptest1_obj_test_" # testname "(%d).  Unexpected values received.\n", i);  nerrs += 1; \
	} \
    } \
  else \
    { \
      fprintf (stderr, "** Operation iioptest1_obj_test_" # testname "(%d) signals %s exception <%s>.\n", \
	       i, (env._major == CORBA_SYSTEM_EXCEPTION) ? "system" : \
	       ((env._major == CORBA_USER_EXCEPTION) ? "user" : \
		((env._major == CORBA_NO_EXCEPTION) ? "non?" : "unknown?!?")), \
	       ILU_C_EXCEPTION_ID(&env)); \
      CORBA_exception_free(&env); \
      nerrs += 1; \
    } \
}

  TEST_NUM(short,CORBA_short)
  TEST_NUM(long,CORBA_long)
  TEST_NUM(ushort,CORBA_unsigned_short)
  TEST_NUM(ulong,CORBA_unsigned_long)
  TEST_NUM(octet,CORBA_octet)
  TEST_NUM(float,CORBA_float)
  TEST_NUM(double,CORBA_double)

static void test_boolean (iioptest1_obj self, int i)
{
  CORBA_Environment env;
  CORBA_boolean val1 = ((i % 2) == 0) ? ilu_TRUE : ilu_FALSE;
  CORBA_boolean val2;
  CORBA_boolean val3 = ((i % 2) == 1) ? ilu_TRUE : ilu_FALSE;
  CORBA_boolean result;
  CORBA_boolean expected2 = ((i % 2) == 1) ? ilu_TRUE : ilu_FALSE;
  CORBA_boolean expected3 = ((i % 2) == 0) ? ilu_TRUE : ilu_FALSE;

  ntests += 1;

  result = iioptest1_obj_test_boolean (self, val1, &val2, &val3, &env);

  if (env._major == CORBA_NO_EXCEPTION)
    {
      if (val2 != expected2 || val3 != expected3 || result != expected2)
	{
	  fprintf (stderr, "** Error on iioptest1_obj_test_" "boolean" "(%d).  Unexpected values received.\n", i);
	  nerrs += 1;
	}
    }
  else
    {
      fprintf (stderr, "** Operation iioptest1_obj_test_" "boolean" "(%d) signals %s exception <%s>.\n",
	       i, (env._major == CORBA_SYSTEM_EXCEPTION) ? "system" :
	       ((env._major == CORBA_USER_EXCEPTION) ? "user" :
		((env._major == CORBA_NO_EXCEPTION) ? "non?" : "unknown?!?")),
	       ILU_C_EXCEPTION_ID(&env));
      CORBA_exception_free(&env);
      nerrs += 1;
    }
}

static void test_char (iioptest1_obj self, int i)
{
  CORBA_Environment env;
  CORBA_char val1 = (i + 17) % 256;
  CORBA_char val2;
  CORBA_char val3 = (i + 17) % 256;
  CORBA_char result;
  CORBA_char expected2 = (i + 17) % 256;
  CORBA_char expected3 = (i + 17) % 256;

  ntests += 1;

  result = iioptest1_obj_test_char (self, val1, &val2, &val3, &env);

  if (env._major == CORBA_NO_EXCEPTION)
    {
      if (val2 != expected2 || val3 != expected3 || result != expected2)
	{
	  fprintf (stderr, "** Error on iioptest1_obj_test_" "char" "(%d).  Unexpected values received.\n", i);
	  nerrs += 1;
	}
    }
  else
    {
      fprintf (stderr, "** Operation iioptest1_obj_test_" "char" "(%d) signals %s exception <%s>.\n",
	       i, (env._major == CORBA_SYSTEM_EXCEPTION) ? "system" :
	       ((env._major == CORBA_USER_EXCEPTION) ? "user" :
		((env._major == CORBA_NO_EXCEPTION) ? "non?" : "unknown?!?")),
	       ILU_C_EXCEPTION_ID(&env));
      CORBA_exception_free(&env);
      nerrs += 1;
    }
}

static void test_wchar (iioptest1_obj self, int i)
{
  CORBA_Environment env;
  CORBA_wchar val, val1, val2, val3;
  CORBA_wchar result, expected2, expected3;

  if (sizeof(CORBA_wchar) == 2)
    val = (i + 17) % 0x10000;
  else
    val = (i + 17);
  val1 = val3 = expected3 = expected2 = val;

  ntests += 1;

  result = iioptest1_obj_test_wchar (self, val1, &val2, &val3, &env);

  if (env._major == CORBA_NO_EXCEPTION)
    {
      if (val2 != expected2 || val3 != expected3 || result != expected2)
	{
	  fprintf (stderr, "** Error on iioptest1_obj_test_" "wchar" "(%d).  Unexpected values received.\n", i);
	  nerrs += 1;
	}
    }
  else
    {
      fprintf (stderr, "** Operation iioptest1_obj_test_" "wchar" "(%d) signals %s exception <%s>.\n",
	       i, (env._major == CORBA_SYSTEM_EXCEPTION) ? "system" :
	       ((env._major == CORBA_USER_EXCEPTION) ? "user" :
		((env._major == CORBA_NO_EXCEPTION) ? "non?" : "unknown?!?")),
	       ILU_C_EXCEPTION_ID(&env));
      CORBA_exception_free(&env);
      nerrs += 1;
    }
}

static void test_string (iioptest1_obj self, int i)
{
  CORBA_Environment env;
  CORBA_string val1 = (CORBA_string) "foo";
  CORBA_string val2 = ILU_NIL;
  CORBA_string val3 = (CORBA_string) ILU_C_Strdup("bar");
  CORBA_string result;
  CORBA_string expected2 = (CORBA_string) "foofoo";
  CORBA_string expected3 = (CORBA_string) "barbar";

  ntests += 1;

  result = iioptest1_obj_test_string (self, val1, &val2, &val3, &env);

  if (env._major == CORBA_NO_EXCEPTION)
    {
      if (strcmp((const char *) val2, (const char *) expected2) != 0 ||
	  strcmp((const char *) val3, (const char *) expected3) != 0 ||
	  strcmp((const char *) result, (const char *) expected2) != 0)
	{
	  fprintf (stderr, "** Error on iioptest1_obj_test_" "string" "(%d).  Unexpected values received.\n", i);
	  nerrs += 1;
	}
      ilu_free(result);
      ilu_free(val2);
      ilu_free(val3);
    }
  else
    {
      fprintf (stderr, "** Operation iioptest1_obj_test_" "string" "(%d) signals %s exception <%s>.\n",
	       i, (env._major == CORBA_SYSTEM_EXCEPTION) ? "system" :
	       ((env._major == CORBA_USER_EXCEPTION) ? "user" :
		((env._major == CORBA_NO_EXCEPTION) ? "non?" : "unknown?!?")),
	       ILU_C_EXCEPTION_ID(&env));
      CORBA_exception_free(&env);
      nerrs += 1;
    }
}

static int wstrcmp (CORBA_wchar * str1, CORBA_wchar * str2)
{
  register int i = 0;
  while (1)
    {
      if (str1[i] < str2[i])
	return -1;
      else if (str1[i] > str2[i])
	return 1;
      else if (str1[i] == 0 || str2[i] == 0)
	return 0;

      i++;
    }
}

static void test_wstring (iioptest1_obj self, int i)
{
  CORBA_Environment env;
  CORBA_wchar bar[4] = { 0x0062, 0x0061, 0x0072, 0x0 };
  CORBA_wchar val1[4] = { 0x0066, 0x006F, 0x006F, 0x0 };
  CORBA_wchar * val2 = ILU_NIL;
  CORBA_wchar * val3 = ilu_malloc(4 * sizeof(CORBA_wchar));
  CORBA_wchar * result;
  CORBA_wchar expected2[7] = { 0x0066, 0x006F, 0x006F, 0x0066, 0x006F, 0x006F, 0x0 };
  CORBA_wchar expected3[7] = { 0x0062, 0x0061, 0x0072, 0x0062, 0x0061, 0x0072, 0x0 };
  int j;

  for (j = 0;  j < 4;  j++)
    val3[j] = bar[j];

  ntests += 1;

  result = iioptest1_obj_test_wstring (self, (CORBA_wchar *) val1, &val2, &val3, &env);

  if (env._major == CORBA_NO_EXCEPTION)
    {
      if (wstrcmp(val2, (CORBA_wchar *) expected2) != 0 ||
	  wstrcmp(val3, (CORBA_wchar *) expected3) != 0 ||
	  wstrcmp(result, (CORBA_wchar *) expected2) != 0)
	{
	  fprintf (stderr, "** Error on iioptest1_obj_test_" "wstring" "(%d).  Unexpected values received.\n", i);
	  nerrs += 1;
	}
      ilu_free(result);
      ilu_free(val2);
      ilu_free(val3);
    }
  else
    {
      fprintf (stderr, "** Operation iioptest1_obj_test_" "wstring" "(%d) signals %s exception <%s>.\n",
	       i, (env._major == CORBA_SYSTEM_EXCEPTION) ? "system" :
	       ((env._major == CORBA_USER_EXCEPTION) ? "user" :
		((env._major == CORBA_NO_EXCEPTION) ? "non?" : "unknown?!?")),
	       ILU_C_EXCEPTION_ID(&env));
      CORBA_exception_free(&env);
      nerrs += 1;
    }
}


#ifdef LONG_INTEGER_TYPE
#define ASSIGN_LONG_LONG_FROM_INT(to,from)	((to) = (from))
#define COMPARE_LONG_LONG(v1,v2)		((v1)==(v2))
#else
#define ASSIGN_LONG_LONG_FROM_INT(to,from)	(((to).high = (((from) < 0)?-1:0)),((to).low=(ilu_cardinal)(from)))
#define COMPARE_LONG_LONG(v1,v2)		(((v1).high==(v2).high)&&((v1).low==(v2).low))
#endif

static void test_longlong (iioptest1_obj self, int i)
{
  CORBA_Environment env;
  CORBA_long_long val1, val2, val3;
  CORBA_long_long result, expected2, expected3;

  ASSIGN_LONG_LONG_FROM_INT(val1,i + 17);
  ASSIGN_LONG_LONG_FROM_INT(val3,i - 17);
  expected3 = val1;
  expected2 = val3;

  ntests += 1;

  result = iioptest1_obj_test_longlong (self, val1, &val2, &val3, &env);

  if (env._major == CORBA_NO_EXCEPTION)
    {
      if (!COMPARE_LONG_LONG(val2,expected2) ||
	  !COMPARE_LONG_LONG(val3,expected3) ||
	  !COMPARE_LONG_LONG(result,expected2))
	{
	  fprintf (stderr, "** Error on iioptest1_obj_test_" "longlong" "(%d).  Unexpected values received.\n", i);
	  nerrs += 1;
	}
    }
  else
    {
      fprintf (stderr, "** Operation iioptest1_obj_test_" "longlong" "(%d) signals %s exception <%s>.\n",
	       i, (env._major == CORBA_SYSTEM_EXCEPTION) ? "system" :
	       ((env._major == CORBA_USER_EXCEPTION) ? "user" :
		((env._major == CORBA_NO_EXCEPTION) ? "non?" : "unknown?!?")),
	       ILU_C_EXCEPTION_ID(&env));
      CORBA_exception_free(&env);
      nerrs += 1;
    }
}


#ifdef LONG_CARDINAL_TYPE
#define ASSIGN_LONG_CARDINAL_FROM_INT(to,from)	((to) = (from))
#define COMPARE_LONG_CARDINAL(v1,v2)		((v1)==(v2))
#else
#define ASSIGN_LONG_CARDINAL_FROM_INT(to,from)	(((to).high = 0),((to).low=(ilu_cardinal)(from)))
#define COMPARE_LONG_CARDINAL(v1,v2)		(((v1).high==(v2).high)&&((v1).low==(v2).low))
#endif

static void test_ulonglong (iioptest1_obj self, int i)
{
  CORBA_Environment env;
  CORBA_unsigned_long_long val1, val2, val3;
  CORBA_unsigned_long_long result, expected2, expected3;

  ASSIGN_LONG_CARDINAL_FROM_INT(val1,i + 17);
  ASSIGN_LONG_CARDINAL_FROM_INT(val3,i - 17);
  expected3 = val1;
  expected2 = val3;

  ntests += 1;

  result = iioptest1_obj_test_ulonglong (self, val1, &val2, &val3, &env);

  if (env._major == CORBA_NO_EXCEPTION)
    {
      if (!COMPARE_LONG_CARDINAL(val2,expected2) ||
	  !COMPARE_LONG_CARDINAL(val3,expected3) ||
	  !COMPARE_LONG_CARDINAL(result,expected2))
	{
	  fprintf (stderr, "** Error on iioptest1_obj_test_" "ulonglong" "(%d).  Unexpected values received.\n", i);
	  nerrs += 1;
	}
    }
  else
    {
      fprintf (stderr, "** Operation iioptest1_obj_test_" "ulonglong" "(%d) signals %s exception <%s>.\n",
	       i, (env._major == CORBA_SYSTEM_EXCEPTION) ? "system" :
	       ((env._major == CORBA_USER_EXCEPTION) ? "user" :
		((env._major == CORBA_NO_EXCEPTION) ? "non?" : "unknown?!?")),
	       ILU_C_EXCEPTION_ID(&env));
      CORBA_exception_free(&env);
      nerrs += 1;
    }
}


static void test_throw (iioptest1_obj self, int i)
{
  CORBA_Environment env;
  CORBA_long val = i - 5;

  iioptest1_obj_test_throw (self, val, &env);
  if (env._major == CORBA_NO_EXCEPTION)
    {
      fprintf (stderr, "** Operation iioptest1_obj_test_throw(%d) signals no exception!\n", val);
      nerrs += 1;
    }
  else
    {
      if (val <= 0)
	{
	  if (env._major != CORBA_USER_EXCEPTION ||
	      env.returnCode != ex_iioptest1_x1 ||
	      ((struct iioptest1_x1_rec *) (env.ptr))->case_num != val)
	    {
	      fprintf (stderr, "** Error on iioptest1_obj_test_throw(%d).  Unexpected values received.\n",
		       val);
	      nerrs += 1;
	    }
	  CORBA_exception_free(&env);
	}
      else if ((val % 2) == 1)
	{
	  if (env._major != CORBA_USER_EXCEPTION ||
	      env.returnCode != ex_iioptest1_x2 ||
	      ((struct iioptest1_x2_rec *) (env.ptr))->case_num != val ||
	      ((struct iioptest1_x2_rec *) (env.ptr))->obj != self)
	    {
	      fprintf (stderr, "** Error on iioptest1_obj_test_throw(%d).  Unexpected values received.\n",
		       val);
	      nerrs += 1;
	    }
	  CORBA_exception_free(&env);
	}
      else
	{
	  if (env._major != CORBA_USER_EXCEPTION ||
	      env.returnCode != ex_iioptest1_x2 ||
	      ((struct iioptest1_x2_rec *) (env.ptr))->case_num != val ||
	      ((struct iioptest1_x2_rec *) (env.ptr))->obj != ILU_NIL)
	    {
	      fprintf (stderr, "** Error on iioptest1_obj_test_throw(%d).  Unexpected values received.\n",
		       val);
	      nerrs += 1;
	    }
	  CORBA_exception_free(&env);
	}
    }
}

static void test_obj (iioptest1_ext_obj self, int i)
{
  CORBA_Environment env;
  iioptest1_ext_obj val1 = self;
  iioptest1_ext_obj val2 = ILU_NIL;
  iioptest1_ext_obj val3 = self;
  iioptest1_ext_obj result;

  ntests += 1;

  result = iioptest1_ext_obj_test_obj (self, val1, &val2, &val3, &env);

  if (env._major == CORBA_NO_EXCEPTION)
    {
      if (val2 != self || val3 != self || result != self)
	{
	  fprintf (stderr, "** Error on iioptest1_ext_obj_test_" "obj" "(%d).  Unexpected values received.\n", i);
	  nerrs += 1;
	}
    }
  else
    {
      fprintf (stderr, "** Operation iioptest1_ext_obj_test_" "obj" "(%d) signals %s exception <%s>.\n",
	       i, (env._major == CORBA_SYSTEM_EXCEPTION) ? "system" :
	       ((env._major == CORBA_USER_EXCEPTION) ? "user" :
		((env._major == CORBA_NO_EXCEPTION) ? "non?" : "unknown?!?")),
	       ILU_C_EXCEPTION_ID(&env));
      CORBA_exception_free(&env);
      nerrs += 1;
    }
}

static void test_fixed_record (iioptest1_ext_obj self, int i)
{
  CORBA_Environment env;
  iioptest1_rec2 a1, a2, a3, rv;
  iioptest1_rec2 expected_a2, expected_a3;

  a1.f1 = i - 117;
  a1.f2 = i - 117;

  a2.f1 = 0;
  a2.f2 = 0;

  a3.f1 = i + 20;
  a3.f2 = i + 20;

  rv.f1 = 0;
  rv.f2 = 0;

  expected_a2.f1 = a1.f1 * a1.f1 * a1.f1;
  expected_a2.f2 = a1.f2 * a1.f2 * a1.f2;
  expected_a3.f1 = a3.f1 * a3.f1 * a3.f1;
  expected_a3.f2 = a3.f2 * a3.f2 * a3.f2;
  
  ntests += 1;

  rv = iioptest1_ext_obj_test_fixed_record (self, &a1, &a2, &a3, &env);

  if (env._major == CORBA_NO_EXCEPTION)
    {
      if (!(a2.f1 == expected_a2.f1 && a2.f2 == expected_a2.f2 &&
	    a3.f1 == expected_a3.f1 && a3.f2 == expected_a3.f2 &&
	    rv.f1 == expected_a2.f1 && rv.f2 == expected_a2.f2))
	{
	  fprintf (stderr, "** Error on iioptest1_ext_obj_test_" "fixed_record"
		   "(%d).  Unexpected values received.\n", i);
	  nerrs += 1;
	}
    }
  else
    {
      fprintf (stderr, "** Operation iioptest1_ext_obj_test_" "fixed_record"
	       "(%d) signals %s exception <%s>.\n",
	       i, (env._major == CORBA_SYSTEM_EXCEPTION) ? "system" :
	       ((env._major == CORBA_USER_EXCEPTION) ? "user" :
		((env._major == CORBA_NO_EXCEPTION) ? "non?" : "unknown?!?")),
	       ILU_C_EXCEPTION_ID(&env));
      CORBA_exception_free(&env);
      nerrs += 1;
    }
}

static void test_enum (iioptest1_ext_obj self, int i)
{
  CORBA_Environment env;
  iioptest1_enum1 val1 = (iioptest1_enum1) (((unsigned int)(i - 117)) % 5);
  iioptest1_enum1 val2;
  iioptest1_enum1 val3 = (iioptest1_enum1) (((unsigned int)(i + 20)) % 5);
  iioptest1_enum1 result;
  iioptest1_enum1 expected2 = (iioptest1_enum1) ((((unsigned int)(i - 117)) + 1) % 5);
  iioptest1_enum1 expected3 = (iioptest1_enum1) ((((unsigned int)(i + 20)) - 1) % 5);

  ntests += 1;

  result = iioptest1_ext_obj_test_enumeration (self, val1, &val2, &val3, &env);

  if (env._major == CORBA_NO_EXCEPTION)
    {
      if (val2 != expected2 || val3 != expected3 || result != expected2)
	{
	  fprintf (stderr, "** Error on iioptest1_ext_obj_test_"
		   "enumeration" "(%d).  Unexpected values received.\n", i);
	  nerrs += 1;
	}
    }
  else
    {
      fprintf (stderr, "** Operation iioptest1_ext_obj_test_"
	       "enumeration" "(%d) signals %s exception <%s>.\n",
	       i, (env._major == CORBA_SYSTEM_EXCEPTION) ? "system" :
	       ((env._major == CORBA_USER_EXCEPTION) ? "user" :
		((env._major == CORBA_NO_EXCEPTION) ? "non?" : "unknown?!?")),
	       ILU_C_EXCEPTION_ID(&env));
      CORBA_exception_free(&env);
      nerrs += 1;
    }
}

static void init_seq1 (iioptest1_seq1 *s, int seed)
{
  int i;
  s->_maximum = abs(seed) % 6;
  s->_length = abs(seed) % 6;
  s->_buffer = CORBA_sequence_iioptest1_rec2_allocbuf(s->_length);
  for (i = 0;   i < s->_length;  i++)
    {
      s->_buffer[i].f1 = seed + 1;
      s->_buffer[i].f2 = seed % 256;
    }
}

static void multiply_seq (iioptest1_seq1 *output, iioptest1_seq1 *input)
{
  int i;
  if (output->_maximum < input->_length)
    {
      if (output->_maximum > 0)
	CORBA_free(output->_buffer);
      output->_buffer = CORBA_sequence_iioptest1_rec2_allocbuf(input->_length);
      output->_maximum = input->_length;
    }
  output->_length = input->_length;
  for (i = 0;  i < input->_length;  i++)
    {
      output->_buffer[i].f1 = (input->_buffer[i].f1) * (input->_buffer[i].f1) * (input->_buffer[i].f1);
      output->_buffer[i].f2 = (input->_buffer[i].f2) * (input->_buffer[i].f2) * (input->_buffer[i].f2);
    }
}

static ilu_boolean compare_seq1 (iioptest1_seq1 *s1, iioptest1_seq1 *s2)
{
  int i;

  if (s1->_length != s2->_length)
    return ilu_FALSE;
  for (i = 0;  i < s1->_length;  i++)
    if (s1->_buffer[i].f1 != s2->_buffer[i].f1 ||
	s1->_buffer[i].f2 != s2->_buffer[i].f2)
      return ilu_FALSE;
  return ilu_TRUE;
}

static void test_seq (iioptest1_ext_obj self, int i)
{
  CORBA_Environment env;

  iioptest1_seq1 val1, val3;
  iioptest1_seq1 *val2p, *retval;
  iioptest1_seq1 expected_val2, expected_val3;

  init_seq1 (&val1, i - 117);
  val2p = ilu_malloc(sizeof(iioptest1_seq1));
  init_seq1 (&val3, i + 20);
  expected_val3._maximum = 0;
  expected_val2._maximum = 0;
  expected_val3._length = 0;
  expected_val2._length = 0;
  expected_val3._buffer = ILU_NIL;
  expected_val2._buffer = ILU_NIL;
  multiply_seq (&expected_val2, &val1);
  multiply_seq (&expected_val3, &val3);

  ntests += 1;

  retval = iioptest1_ext_obj_test_sequence (self, &val1, &val2p, &val3, &env);

  if (env._major == CORBA_NO_EXCEPTION)
    {
      if (!compare_seq1(val2p, &expected_val2) ||
	  !compare_seq1(retval, &expected_val2) ||
	  !compare_seq1(&val3, &expected_val3))
	{
	  fprintf (stderr, "** Error on iioptest1_ext_obj_test_"
		   "sequence" "(%d).  Unexpected values received.\n", i);
	  nerrs += 1;
	}
    }
  else
    {
      fprintf (stderr, "** Operation iioptest1_ext_obj_test_"
	       "sequence" "(%d) signals %s exception <%s>.\n",
	       i, (env._major == CORBA_SYSTEM_EXCEPTION) ? "system" :
	       ((env._major == CORBA_USER_EXCEPTION) ? "user" :
		((env._major == CORBA_NO_EXCEPTION) ? "non?" : "unknown?!?")),
	       ILU_C_EXCEPTION_ID(&env));
      CORBA_exception_free(&env);
      nerrs += 1;
    }

  CORBA_free(val1._buffer);
  CORBA_free(val2p->_buffer);
  CORBA_free(val2p);
  CORBA_free(val3._buffer);
  CORBA_free(retval->_buffer);
  CORBA_free(retval);
  CORBA_free(expected_val3._buffer);
  CORBA_free(expected_val2._buffer);
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

static ilu_boolean compare_rec1 (iioptest1_rec1 *v1, iioptest1_rec1 *v2)
{
  return (v1->f1 == v2->f1 &&
	  v1->f2 == v2->f2 &&
	  v1->f3 == v2->f3 &&
	  v1->f4 == v2->f4 &&
	  v1->f5 == v2->f5 &&
	  v1->f6 == v2->f6 &&
	  v1->f7 == v2->f7 &&
	  v1->f8 == v2->f8 &&
	  (strcmp((const char *) (v1->f9), (const char *) (v2->f9)) == 0));
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

static void init_rec1 (iioptest1_rec1 *in, int val)
{
  char buf[100];

  in->f1 = val;
  in->f2 = val;
  in->f3 = val;
  in->f4 = val;
  in->f5 = val;
  in->f6 = val;
  in->f7 = val;
  in->f8 = val;
  sprintf (buf, "%d", val);
  in->f9 = (iioptest1_string) ILU_C_Strdup(buf);
}

static void test_var_record (iioptest1_ext_obj self, int i)
{
  CORBA_Environment env;

  iioptest1_rec1 val1, val3;
  iioptest1_rec1 *val2p, *retval;
  iioptest1_rec1 expected_val2, expected_val3;

  init_rec1 (&val1, i - 117);
  val2p = ILU_NIL;
  init_rec1 (&val3, i + 20);
  cube_rec1 (&val1, &expected_val2);
  cube_rec1 (&val3, &expected_val3);

  ntests += 1;

  retval = iioptest1_ext_obj_test_var_record (self, &val1, &val2p, &val3, &env);

  if (env._major == CORBA_NO_EXCEPTION)
    {
      if (!compare_rec1(val2p, &expected_val2) ||
	  !compare_rec1(retval, &expected_val2) ||
	  !compare_rec1(&val3, &expected_val3))
	{
	  fprintf (stderr, "** Error on iioptest1_ext_obj_test_"
		   "var_record" "(%d).  Unexpected values received.\n", i);
	  nerrs += 1;
	}
      CORBA_free(val2p->f9);
      CORBA_free(retval->f9);
      CORBA_free(val2p);
      CORBA_free(retval);
    }
  else
    {
      fprintf (stderr, "** Operation iioptest1_ext_obj_test_"
	       "var_record" "(%d) signals %s exception <%s>.\n",
	       i, (env._major == CORBA_SYSTEM_EXCEPTION) ? "system" :
	       ((env._major == CORBA_USER_EXCEPTION) ? "user" :
		((env._major == CORBA_NO_EXCEPTION) ? "non?" : "unknown?!?")),
	       ILU_C_EXCEPTION_ID(&env));
      CORBA_exception_free(&env);
      nerrs += 1;
    }

  CORBA_free(val1.f9);
  CORBA_free(val3.f9);
  CORBA_free(expected_val2.f9);
  CORBA_free(expected_val3.f9);
}

static ilu_boolean compare_arr1 (iioptest1_arr1 v1, iioptest1_arr1 v2)
{
  int i, j;

  for (i = 0;  i < 2;  i++) {
    for (j = 0;  j < 3;  j++) {
      if (!compare_rec1(&v1[i][j], &v2[i][j]))
	return ilu_FALSE;
    }
  }
  return ilu_TRUE;
}

static void test_var_array (iioptest1_ext_obj self, int i)
{
  CORBA_Environment env;

  iioptest1_arr1 val1, val3;
  iioptest1_arr1_slice *val2p, *retval;
  iioptest1_arr1 expected_val2, expected_val3;
  int k, j;

  val2p = ILU_NIL;
  for (k = 0;  k < 2;  k++)
    for (j = 0;  j < 3;  j++)
      {
	init_rec1 (&val1[k][j], i - 117 - k * j + j);
	init_rec1 (&val3[k][j], i + 20 + k * j - k);
	cube_rec1 (&val1[k][j], &expected_val2[k][j]);
	cube_rec1 (&val3[k][j], &expected_val3[k][j]);
      }
  ntests += 1;

  retval = iioptest1_ext_obj_test_var_array (self, val1, &val2p, val3, &env);

  if (env._major == CORBA_NO_EXCEPTION)
    {
      if (!compare_arr1(val2p, expected_val2) ||
	  !compare_arr1(retval, expected_val2) ||
	  !compare_arr1(val3, expected_val3))
	{
	  fprintf (stderr, "** Error on iioptest1_ext_obj_test_"
		   "var_array" "(%d).  Unexpected values received.\n", i);
	  nerrs += 1;
	}
    }
  else
    {
      fprintf (stderr, "** Operation iioptest1_ext_obj_test_"
	       "var_array" "(%d) signals %s exception <%s>.\n",
	       i, (env._major == CORBA_SYSTEM_EXCEPTION) ? "system" :
	       ((env._major == CORBA_USER_EXCEPTION) ? "user" :
		((env._major == CORBA_NO_EXCEPTION) ? "non?" : "unknown?!?")),
	       ILU_C_EXCEPTION_ID(&env));
      CORBA_exception_free(&env);
      nerrs += 1;
    }

}

static void init_rec2 (iioptest1_rec2 *p, ilu_integer i)
{
  p->f1 = i;
  p->f2 = i;
}

static void cube_rec2 (iioptest1_rec2 *input, iioptest1_rec2 *output)
{
  output->f1 = (input->f1 * input->f1 * input->f1);
  output->f2 = (input->f2 * input->f2 * input->f2);
}

static ilu_boolean compare_arr4 (iioptest1_arr4 a1, iioptest1_arr4 a2)
{
  int i, j;

  for (i = 0;  i < 2;  i++) {
    for (j = 0;  j < 3;  j++) {
      if ((a1[i][j].f1 != a2[i][j].f1) ||
	  (a1[i][j].f2 != a2[i][j].f2))
	return ilu_FALSE;
    }
  }
  return ilu_TRUE;
}

static void test_fixed_array (iioptest1_ext_obj self, int i)
{
  CORBA_Environment env;

  iioptest1_arr4 val1, val2, val3;
  iioptest1_arr4_slice *retval;
  iioptest1_arr4 expected_val2, expected_val3;
  int k, j;

  for (k = 0;  k < 2;  k++)
    for (j = 0;  j < 3;  j++)
      {
	init_rec2 (&val1[k][j], i - 117 - k * j + j);
	init_rec2 (&val2[k][j], 0);
	init_rec2 (&val3[k][j], i + 20 + k * j - k);
	cube_rec2 (&val1[k][j], &expected_val2[k][j]);
	cube_rec2 (&val3[k][j], &expected_val3[k][j]);
      }
  ntests += 1;

  retval = iioptest1_ext_obj_test_fixed_array (self, val1, val2, val3, &env);

  if (env._major == CORBA_NO_EXCEPTION)
    {
      if (!compare_arr4(val2, expected_val2) ||
	  !compare_arr4(retval, expected_val2) ||
	  !compare_arr4(val3, expected_val3))
	{
	  fprintf (stderr, "** Error on iioptest1_ext_obj_test_"
		   "fixed_array" "(%d).  Unexpected values received.\n", i);
	  nerrs += 1;
	}
    }
  else
    {
      fprintf (stderr, "** Operation iioptest1_ext_obj_test_"
	       "fixed_array" "(%d) signals %s exception <%s>.\n",
	       i, (env._major == CORBA_SYSTEM_EXCEPTION) ? "system" :
	       ((env._major == CORBA_USER_EXCEPTION) ? "user" :
		((env._major == CORBA_NO_EXCEPTION) ? "non?" : "unknown?!?")),
	       ILU_C_EXCEPTION_ID(&env));
      CORBA_exception_free(&env);
      nerrs += 1;
    }

}

static void transform_union2 (iioptest1_union2 *input, iioptest1_union2 *output)
{
  switch (input->_d)
    {
    case 0:
      output->_d = 1;
      output->_u.real = -1.0 * input->_u.integer;
      break;

    case 1:
      output->_d = 0;
      output->_u.integer = input->_u.real;
      break;
    }
}

static ilu_boolean compare_union2 (iioptest1_union2 *u1, iioptest1_union2 *u2)
{
  return (((u1->_d == 0) && (u2->_d == 0) && (u1->_u.integer == u2->_u.integer)) ||
	  ((u1->_d == 1) && (u2->_d == 1) && ((u1->_u.real - u2->_u.real) < 0.0001)));
}

static void test_fixed_union (iioptest1_ext_obj self, int i)
{
  CORBA_Environment env;

  iioptest1_union2 val1, val2, val3, retval, expected_val2, expected_val3;
  int k, j;

  if ((i % 2) == 0) {
    val1._d = 0;
    val1._u.integer = i - 17;
    val2._d = 0;
    val2._u.integer = 0;
    val3._d = 1;
    val3._u.real = sqrt((double) i);
  } else {
    val1._d = 1;
    val1._u.real = pow((double) i, 2.5);
    val2._d = 0;
    val2._u.integer = 0;
    val3._d = 1;
    val3._u.integer = i * i * i;
  }    

  transform_union2 (&val1, &expected_val3);
  transform_union2 (&val3, &expected_val2);

  ntests += 1;

  retval = iioptest1_ext_obj_test_fixed_union (self, &val1, &val2, &val3, &env);

  if (env._major == CORBA_NO_EXCEPTION)
    {
      if (!compare_union2(&val2, &expected_val2) ||
	  !compare_union2(&retval, &expected_val2) ||
	  !compare_union2(&val3, &expected_val3))
	{
	  fprintf (stderr, "** Error on iioptest1_ext_obj_test_"
		   "fixed_union" "(%d).  Unexpected values received.\n", i);
	  nerrs += 1;
	}
    }
  else
    {
      fprintf (stderr, "** Operation iioptest1_ext_obj_test_"
	       "fixed_union" "(%d) signals %s exception <%s>.\n",
	       i, (env._major == CORBA_SYSTEM_EXCEPTION) ? "system" :
	       ((env._major == CORBA_USER_EXCEPTION) ? "user" :
		((env._major == CORBA_NO_EXCEPTION) ? "non?" : "unknown?!?")),
	       ILU_C_EXCEPTION_ID(&env));
      CORBA_exception_free(&env);
      nerrs += 1;
    }

}

static void test_optional (iioptest1_xilu_obj self, int i)
{
  CORBA_Environment env;
  iioptest1_rec2 va1, va2, *va3, vrv;
  iioptest1_rec2 expected_va2, expected_va3;
  iioptest1_opt1 a1, a2, a3, rv, expected_a2, expected_a3;

  va1.f1 = i - 117;
  va1.f2 = i - 117;

  va2.f1 = 0;
  va2.f2 = 0;

  va3 = ilu_must_malloc(sizeof(*va3));
  va3->f1 = i + 20;
  va3->f2 = i + 20;

  rv = ILU_NIL;

  cube_rec2 (va3, &expected_va2);
  cube_rec2 (&va1, &expected_va3);
  
  if ((i % 2) == 0) {
    a1 = ILU_NIL;
    a3 = va3;
    expected_a2 = &expected_va2;
    expected_a3 = ILU_NIL;
  } else {
    a3 = ILU_NIL;
    a1 = &va1;
  }
  a2 = ILU_NIL;

  ntests += 1;

  rv = iioptest1_xilu_obj_test_optional (self, a1, &a2, &a3, &env);

  if (env._major == CORBA_NO_EXCEPTION)
    {
      if (((a1 == ILU_NIL)
	   && ((a2 == ILU_NIL) || (rv == ILU_NIL) || (a3 != ILU_NIL) ||
	       (a2->f1 != expected_va2.f1) || (a2->f2 != expected_va2.f2) ||
	       (rv->f1 != expected_va2.f1) || (rv->f2 != expected_va2.f2))) ||
	  ((a1 != ILU_NIL)
	   && ((a2 != ILU_NIL) || (rv != ILU_NIL) || (a3 == ILU_NIL) ||
	       (a3->f1 != expected_va3.f1) || (a3->f2 != expected_va3.f2))))
	{
	  fprintf (stderr, "** Error on iioptest1_xilu_obj_test_" "optional"
		   "(%d).  Unexpected values received.\n", i);
	  nerrs += 1;
	}
    }
  else
    {
      fprintf (stderr, "** Operation iioptest1_xilu_obj_test_" "optional"
	       "(%d) signals %s exception <%s>.\n",
	       i, (env._major == CORBA_SYSTEM_EXCEPTION) ? "system" :
	       ((env._major == CORBA_USER_EXCEPTION) ? "user" :
		((env._major == CORBA_NO_EXCEPTION) ? "non?" : "unknown?!?")),
	       ILU_C_EXCEPTION_ID(&env));
      CORBA_exception_free(&env);
      nerrs += 1;
    }
}

static void test_asynchronous (iioptest1_xilu_obj self, int i)
{
  CORBA_Environment env;
  CORBA_long a1;

  a1 = i - 17;

  ntests += 1;

  iioptest1_xilu_obj_test_asynchronous (self, a1, &env);

  if (env._major != CORBA_NO_EXCEPTION) {
    fprintf (stderr, "** Operation iioptest1_xilu_obj_test_" "asynchronous"
	     "(%d) signals %s exception <%s>.\n",
	     i, (env._major == CORBA_SYSTEM_EXCEPTION) ? "system" :
	     ((env._major == CORBA_USER_EXCEPTION) ? "user" :
	      ((env._major == CORBA_NO_EXCEPTION) ? "non?" : "unknown?!?")),
	     ILU_C_EXCEPTION_ID(&env));
    CORBA_exception_free(&env);
    nerrs += 1;
  }
}

void dotests (iioptest1_obj self, int i)
{
  test_short (self, i);
  test_long (self, i);
  test_longlong (self, i);
  test_ushort (self, i);
  test_ulong (self, i);
  test_ulonglong (self, i);
  test_octet (self, i);
  test_float (self, i);
  test_double (self, i);

  test_char (self, i);
  test_wchar (self, i);
  test_boolean (self, i);
  test_string (self, i);
  test_wstring (self, i);

  test_throw (self, i);

  if (ilu_IsSubObjectType(ILU_C_ClassRecordOfInstance(self),
			  ILU_C_FindILUClassByTypeName("iioptest1.ext-obj")))
    {
      iioptest1_ext_obj ext = (iioptest1_ext_obj) self;

      test_obj (ext, i);
      test_fixed_record (ext, i);
      test_enum (ext, i);
      test_seq (ext, i);
      test_var_record (ext, i);
      test_var_array (ext, i);
      test_fixed_array (ext, i);
      test_fixed_union (ext, i);
/*
      test_var_union (ext, i);
*/
    };

  if (ilu_IsSubObjectType(ILU_C_ClassRecordOfInstance(self),
			  ILU_C_FindILUClassByTypeName("iioptest1.xilu-obj")))
    {
      iioptest1_xilu_obj xilu = (iioptest1_xilu_obj) self;

      test_optional (xilu, i);
      test_asynchronous (xilu, i);
    };   
}

int main(int ac, char **av)
{
  iioptest1_obj	obj;
  CORBA_Environment env;
  int i = 1;
  int erroffset = -1;
  ilu_string ior = ILU_NIL;
  long repeatcount = 1;
  int init_val = 0;

  iioptest1__Initialize();

  while (i < ac) {
    if (strcmp(av[i], "-n") == 0) {
      if (i++ < ac)
	repeatcount = atol(av[i++]);
      else
	goto usage;
    } else if (strcmp(av[i], "-e") == 0) {
      if (i++ < ac)
	erroffset = atol(av[i++]);
      else
	goto usage;
    } else if (strcmp(av[i], "-i") == 0) {
      if (i++ < ac)
	init_val = atol(av[i++]);
      else
	goto usage;
    } else if (strcmp(av[i], "-O") == 0) {
      if (i++ < ac)
	ior = av[i++];
      else
	goto usage;
    } else
      goto usage;
  }

  if (ior != ILU_NIL)
    {
      obj = iioptest1_obj__CreateFromSBH (ior, &env);
      if (!ILU_C_SUCCESSFUL(&env))
	goto noobject;
    }
  else
    {
      obj = ILU_C_LookupObject("IIOPTest1Server", "obj", iioptest1_obj__MSType);
      if (obj == ILU_NIL)
	goto noobject;
    }

  for (i = init_val;  i < (repeatcount + init_val);  i++)
    dotests(obj, i);

  fprintf (stdout, "%d tests run.  %d errors.\n", ntests, nerrs);
  return ((erroffset >= 0) ? (nerrs + erroffset) : 0);

 usage:
  fprintf (stderr, "Usage:  %s [-n REPEAT_COUNT] [-i INITVAL] [-O STRINGIFIED-OBJREF] [-e ERR-OFFSET]\n",
	   av[0]);
  return 1;

 noobject:
  fprintf(stderr, "%s:  Unable to import object!\n", av[0]);
  return 1;
}
