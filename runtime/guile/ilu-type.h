/*  -*- Mode: C; -*-
 *
 * This code contributed by Siemens Corporate Research, Inc.
 */
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

#ifndef _ILU_TYPE_H_
#define _ILU_TYPE_H_

/* mappings between ilu types and scheme types */

#include <stdio.h>
#include <stdlib.h>
#include "iluxport.h"
#include "libguile.h"

typedef struct iluguile_SCMCall_s {
  ilu_Call_s call;
  ilu_Error  err;
}* iluguile_SCMCall;

#define scm_call_call(c) (&(((iluguile_SCMCall)(c))->call))
#define scm_call_err(c) (&(((iluguile_SCMCall)(c))->err))

typedef void (*c_function_pointer) (void);

#define scheme_save_stack()
#define scheme_restore_stack()

/* scheme stuff */
extern void iluguile_scm_init(int, char*[], void (*)(int, char*[]),
			      void (*)(), char*, char*);

extern SCM* iluguile_scm_retval;

#define iluguile_define_func(func, req, opt, var)\
iluguile_define_funcp((SCM (*)())iluguile_##func, "ilu_" #func, req, opt, var)

#define scheme_True                 SCM_BOOL_T
#define scheme_False                SCM_BOOL_F
#define scheme_eol                  SCM_EOL
#define scheme_eof                  SCM_EOF_VAL
#define scheme_undefined            SCM_UNDEFINED
#define scheme_Ok                   0

#define scheme_car(s)\
   ((SCM_NIMP(s) && SCM_CONSP(s)) \
    ? SCM_CAR(s) : scm_wta (s, (char*)SCM_ARG1, "scheme_car"))

#define scheme_set_car(s, val)\
   ((SCM_NIMP(s) && SCM_CONSP(s))\
    ? (SCM_CAR(s) = val) : scm_wta (s, (char *)SCM_ARG1, "scheme_set_car"))

#define scheme_cdr(s)\
   ((SCM_NIMP(s) && SCM_CONSP(s))\
    ? SCM_CDR(s) : scm_wta (s, (char*)SCM_ARG1, "scheme_cdr"))

#define scheme_set_cdr(s, val)\
   ((SCM_NIMP(s) && SCM_CONSP(s))\
    ? (SCM_CDR(s) = val) : scm_wta (s, (char *)SCM_ARG1, "scheme_set_cdr"))

#define scheme_cons(a, b) scm_cons(a, b)

#define scheme_stream(p) ((FILE*)SCM_STREAM(p))

#define scheme_lambda(fn, req, opt, varp)\
scm_make_gsubr("*anonymous*", req, opt, varp, (SCM (*)())fn)

#define scheme_list scm_listify

extern SCM* iluguile_scm_make_array(int);
extern void iluguile_scm_free_array(SCM*);

extern void iluguile_define_funcp(SCM (*)(), char*, int, int, int);

extern SCM iluguile_scm_call0(SCM);
extern SCM iluguile_scm_call1(SCM, SCM);
extern SCM iluguile_scm_call2(SCM, SCM, SCM);

extern SCM iluguile_scm_scall1(char*, SCM);
extern SCM iluguile_scm_scall2(char*, SCM, SCM);
extern SCM iluguile_scm_scall4(char*, SCM, SCM, SCM, SCM);

extern SCM iluguile_scm_curry(SCM, SCM);

/* type conversion functions */
#define iluguile_scheme_to_ptr(type, sobj)   ((type)iluguile_scheme_to_pointer(sobj))
#define iluguile_ptr_to_scheme(type, ptr)    iluguile_pointer_to_scheme(ptr)

#define iluguile_scheme_to_fnptr(type, sobj)   ((type)iluguile_scheme_to_fn_pointer(sobj))
#define iluguile_fnptr_to_scheme(type, ptr)    iluguile_fn_pointer_to_scheme((c_function_pointer)(ptr))

#define iluguile_scheme_to_value(type, sobj) iluguile_scheme_to_##type(sobj)
#define iluguile_value_to_scheme(type, val)  iluguile_##type##_to_scheme(val)

#define iluguile_scheme_to_vector(type, sobj) iluguile_scheme_to_##type##_vector(sobj)
#define iluguile_vector_to_scheme(type, vec, num) iluguile_##type##_vector_to_scheme(vec, num)
#define iluguile_scheme_to_vector0(type, sobj) iluguile_scheme_to_##type##_vector0(sobj)

#define double_to_scheme(d)  scm_makdbl((d), 0.0)
#define scheme_to_double(s)  scm_num2dbl((s), "scheme_to_double")
#define ulong_to_scheme(l)   scm_ulong2num(l)
#define scheme_to_ulong(s)   scm_num2ulong((s), (char*)SCM_ARG1, "scheme_to_ulong")
#define long_to_scheme(l)    scm_long2num(l)
#define scheme_to_long(s)    scm_num2long(s, (char*)SCM_ARG1, "scheme_to_long")
#define int_to_scheme(i)     scm_long2num(i)
#define scheme_to_int(s)     ((int)scm_num2long((s), (char*)SCM_ARG1, "scheme_to_int"))
#define char_to_scheme(c)    SCM_MAKICHR(c)
#define scheme_to_char(s)    SCM_ICHR(s)
extern SCM iluguile_pointer_to_scheme(void*);
extern void* iluguile_scheme_to_pointer(SCM);
extern SCM iluguile_fn_pointer_to_scheme(c_function_pointer);
extern c_function_pointer iluguile_scheme_to_fn_pointer(SCM);
extern SCM iluguile_string_to_scheme(char*);
extern char* iluguile_scheme_to_string(SCM);
extern SCM iluguile_tmp_string_to_scheme(char*);
extern char* iluguile_scheme_to_tmp_string(SCM);
extern char** iluguile_scheme_to_string_vector(SCM);
extern char** iluguile_scheme_to_string_vector0(SCM);
extern void iluguile_free_string_vector(char**, int);

/* ilu specific type conversions */
#define iluguile_boolean_to_scheme(b)        ((b) ? scheme_True : scheme_False)
#define iluguile_scheme_to_boolean(s)        (((s) != scheme_False) ? ilu_TRUE : ilu_FALSE)
#define iluguile_cardinal_to_scheme(l)       ulong_to_scheme(l)
#define iluguile_scheme_to_cardinal(s)       ((ilu_cardinal)scheme_to_ulong(s))
#define iluguile_shortcardinal_to_scheme(l)  ulong_to_scheme(l)
#define iluguile_scheme_to_shortcardinal(s)  ((ilu_shortcardinal)scheme_to_ulong(s))
#define iluguile_integer_to_scheme(i)        int_to_scheme(i)
#define iluguile_scheme_to_integer(s)        ((ilu_integer)scheme_to_int(s))
#define iluguile_shortinteger_to_scheme(i)   int_to_scheme(i)
#define iluguile_scheme_to_shortinteger(s)   ((ilu_shortinteger)scheme_to_int(s))
#define iluguile_real_to_scheme(d)           double_to_scheme(d)
#define iluguile_scheme_to_real(s)           ((ilu_real)scheme_to_double(s))
#define iluguile_shortreal_to_scheme(d)      double_to_scheme(d)
#define iluguile_scheme_to_shortreal(s)      ((ilu_shortreal)scheme_to_double(s))
#define iluguile_character_to_scheme(i)      int_to_scheme(i)
#define iluguile_scheme_to_character(s)      ((ilu_character)scheme_to_int(s))
#define iluguile_shortcharacter_to_scheme(i) char_to_scheme(i)
#define iluguile_scheme_to_shortcharacter(s) ((ilu_shortcharacter)scheme_to_char(s))
#define iluguile_byte_to_scheme(i)           char_to_scheme(i)
#define iluguile_scheme_to_byte(s)           ((ilu_byte)scheme_to_char(s))

extern SCM		iluguile_longinteger_to_scheme(ilu_longinteger);
extern ilu_longinteger	iluguile_scheme_to_longinteger(SCM);
extern SCM		iluguile_longcardinal_to_scheme(ilu_longcardinal);
extern ilu_longcardinal	iluguile_scheme_to_longcardinal(SCM);
extern SCM		iluguile_longreal_to_scheme(ilu_longreal);
extern ilu_longreal	iluguile_scheme_to_longreal(SCM);
extern SCM		iluguile_wstring_to_scheme(ilu_wstring);
extern ilu_wstring	iluguile_scheme_to_wstring(SCM);

extern ilu_byte*		iluguile_scheme_to_byte_vector(SCM);
extern ilu_shortcharacter*	iluguile_scheme_to_shortcharacter_vector(SCM);
extern ilu_character*		iluguile_scheme_to_character_vector(SCM);

extern SCM iluguile_byte_vector_to_scheme(ilu_byte*, unsigned int);
extern SCM iluguile_shortcharacter_vector_to_scheme(ilu_shortcharacter*, unsigned int);
extern SCM iluguile_character_vector_to_scheme(ilu_character*, unsigned int);

#endif
