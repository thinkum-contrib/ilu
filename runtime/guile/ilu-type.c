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

#include <assert.h>
#include <string.h>
#include <signal.h>
#include "ilu-type.h"

/*************************************************************************/

#include <iluhash.h>
#include "libguile.h"

SCM iluguile_scm_make_vector(unsigned int len, SCM val)
{
  return scm_make_vector(SCM_MAKINUM(len), val, SCM_UNDEFINED);
}

SCM* iluguile_scm_make_array(int size)
{
  SCM vector = scm_make_vector(SCM_MAKINUM(size+1), SCM_BOOL_F, SCM_UNDEFINED);
  scm_protect_object (vector);
  SCM_VELTS(vector)[0] = vector;
  return SCM_VELTS(vector) + 1;
}

void iluguile_scm_free_array(SCM* arr)
{
  SCM vector = *(arr - 1);
  scm_unprotect_object(vector);
}

static void gscm_2_str(char ** out, int * len_out, SCM* sp)
{
  scm_protect_object(*sp);
  SCM_ASSERT(SCM_NIMP(*sp) && SCM_STRINGP(*sp), *sp, SCM_ARG3, "gscm_2_str");
  if (out)
    *out = SCM_CHARS (*sp);
  if (len_out)
    *len_out = SCM_LENGTH (*sp);
  scm_unprotect_object(*sp);
}

SCM iluguile_scm_seval_str(const char* str)
{
  /* Create a port that reads characters from SCHEME_CODE.  */
  SCM port = scm_mkstrport (SCM_MAKINUM (0),
			    iluguile_value_to_scheme(tmp_string, (char*)str),
			    SCM_OPN | SCM_RDNG,
			    "iluguile_scm_seval_str");
  SCM form, answer;

  /* Read expressions from that port; ignore the values.  */
  while ((form = scm_read (port)) != scheme_eof)
    answer = scm_eval_x (form);

  /* Dispose of the port when done.  (Oh icky.)  */
  scm_close_port (port);
  return answer;
}

SCM iluguile_scm_seval_file(const char* str)
{
  SCM sstr = iluguile_value_to_scheme(tmp_string, (char*)str);
  return scm_primitive_load(sstr);
}

/*************************************************************************/

#define ILU_DIGSPERLONGINT ((scm_sizet)((sizeof(ilu_longinteger)*SCM_CHAR_BIT+SCM_BITSPERDIG-1)/SCM_BITSPERDIG))

#define ILU_DIGSPERLONGCARD ((scm_sizet)((sizeof(ilu_longcardinal)*SCM_CHAR_BIT+SCM_BITSPERDIG-1)/SCM_BITSPERDIG))

#define ILU_DIGSPERINT ((scm_sizet)((sizeof(ilu_integer)*SCM_CHAR_BIT+SCM_BITSPERDIG-1)/SCM_BITSPERDIG))

#define ILU_DIGSPERCARD ((scm_sizet)((sizeof(ilu_cardinal)*SCM_CHAR_BIT+SCM_BITSPERDIG-1)/SCM_BITSPERDIG))

SCM iluguile_longinteger_to_scheme(ilu_longinteger li)
{
  ilu_integer hi = ILU_LONGINT_HIGH_WORD(&li);
  SCM ans = scm_mkbig(ILU_DIGSPERLONGINT, hi < 0);
  SCM_BIGDIG *digits = SCM_BDIGITS(ans);
  ilu_cardinal n;
  unsigned int i;

  n = (ilu_cardinal)(hi < 0 ? -hi : hi);
  for (i = 0; i < ILU_DIGSPERCARD; ++i) {
    digits[i + ILU_DIGSPERCARD] = SCM_BIGLO(n);
    n = SCM_BIGDN(n);
  }

  n = ILU_LONGINT_LOW_WORD(&li);
  for (i = 0; i < ILU_DIGSPERCARD; ++i) {
    digits[i] = SCM_BIGLO(n);
    n = SCM_BIGDN(n);
  }

  return ans;
}

ilu_longinteger iluguile_scheme_to_longinteger(SCM s)
{
  SCM_BIGDIG *digits = SCM_BDIGITS(s);
  ilu_integer hi = 0;
  ilu_cardinal lo = 0;
  ilu_longinteger li;
  int i;

  if(SCM_NUMDIGS(s) > ILU_DIGSPERLONGINT) {
    /* number too big, error */
  }

  if(SCM_NUMDIGS(s) > ILU_DIGSPERCARD) {
    for (i = SCM_NUMDIGS(s) - 1; i >= ILU_DIGSPERCARD; i--) {
      hi =  (hi << SCM_BITSPERDIG) + digits[i];
    }
    if(SCM_BIGSIGN(s))
      hi = -hi;
    ILU_LONGINT_HIGH_WORD(&li) = hi;

    for (i = ILU_DIGSPERCARD - 1; i >= 0; i--) {
      lo = (lo << SCM_BITSPERDIG) + digits[i];
    }
    ILU_LONGINT_LOW_WORD(&li) = lo;
  } else {
    ILU_LONGINT_HIGH_WORD(&li) = SCM_BIGSIGN(s) ? 0 : -1;
    for (i = SCM_NUMDIGS(s) - 1; i >= 0; i--) {
      lo = (lo << SCM_BITSPERDIG) + digits[i];
    }
    ILU_LONGINT_LOW_WORD(&li) = lo;
  }

  return li;
}

/*************************************************************************/

SCM iluguile_longcardinal_to_scheme(ilu_longcardinal c)
{
  SCM ans = scm_mkbig(ILU_DIGSPERLONGCARD, 0);
  SCM_BIGDIG *digits = SCM_BDIGITS(ans);
  ilu_cardinal n;
  unsigned int i;

  n = ILU_LONGCARD_HIGH_WORD(&c);
  for (i = 0; i < ILU_DIGSPERCARD; i++) {
    digits[i + ILU_DIGSPERCARD] = SCM_BIGLO(n);
    n = SCM_BIGDN(n);
  }

  n = ILU_LONGCARD_LOW_WORD(&c);
  for (i = 0; i < ILU_DIGSPERCARD; i++) {
    digits[i] = SCM_BIGLO(n);
    n = SCM_BIGDN(n);
  }

  return ans;
}

ilu_longcardinal iluguile_scheme_to_longcardinal(SCM s)
{
  SCM_BIGDIG *digits = SCM_BDIGITS(s);
  ilu_cardinal hi = 0, lo = 0;
  ilu_longcardinal c;
  int i;

  if(SCM_NUMDIGS(s) > ILU_DIGSPERLONGCARD || SCM_BIGSIGN(s)) {
    /* number too big or is signed, error */
  }

  if(SCM_NUMDIGS(s) > ILU_DIGSPERCARD) {
    for (i = SCM_NUMDIGS(s) - 1; i >= ILU_DIGSPERCARD; i--) {
      hi = (hi << SCM_BITSPERDIG) + digits[i];
    }
    ILU_LONGCARD_HIGH_WORD(&c) = hi;

    for (i = ILU_DIGSPERCARD - 1; i >= 0; i--) {
      lo = (lo << SCM_BITSPERDIG) + digits[i];
    }
    ILU_LONGCARD_LOW_WORD(&c) = lo;
  } else {
    ILU_LONGCARD_HIGH_WORD(&c) = 0;
    for (i = SCM_NUMDIGS(s) - 1; i >= 0; i--) {
      lo = (lo << SCM_BITSPERDIG) + digits[i];
    }
    ILU_LONGCARD_LOW_WORD(&c) = lo;
  }

  return c;
}

/*************************************************************************/

SCM iluguile_longreal_to_scheme(ilu_longreal r)
{
#ifdef LONG_REAL_TYPE
  return iluguile_value_to_scheme(real, r);
#else
  return iluguile_value_to_scheme(real, 0.0);
#endif
}

ilu_longreal iluguile_scheme_to_longreal(SCM s)
{
  ilu_longreal r;
#ifdef LONG_REAL_TYPE
  r = iluguile_scheme_to_value(longreal, s);
#else
  /* r = iluguile_scheme_to_value(double, s);*/
#endif
  return r;
}

/*************************************************************************/

static unsigned int wstrlen(ilu_wstring ws)
{
  unsigned int len = 0;
  while(ws[len])
    len++;
  return len;
}

SCM iluguile_wstring_to_scheme(ilu_wstring ws)
{
  unsigned int len = wstrlen(ws);
  SCM vec = iluguile_scm_make_vector(SCM_MAKINUM(len), scheme_False);
  unsigned int i;
  for(i = 0; i < len; i++) {
    SCM val = iluguile_value_to_scheme(shortcardinal, ws[i]);
    scm_vector_set_x(vec, SCM_MAKINUM(i), val);
  }
  return vec;
}

ilu_wstring iluguile_scheme_to_wstring(SCM vec)
{
  unsigned int len = scm_obj_length(vec);
  ilu_wstring ws = ilu_malloc(sizeof(*ws) * (len+1));
  unsigned int i;
  for(i = 0; i < len; i++) {
    SCM val = scm_vector_ref(vec, SCM_MAKINUM(i));
    ws[i] = iluguile_scheme_to_value(shortcardinal, val);
  }
  ws[i] = 0;
  return ws;
}

/*************************************************************************/

ilu_byte* iluguile_scheme_to_byte_vector(SCM svec)
{
  if(svec != scheme_False) {
    unsigned int len = scm_obj_length(svec);
    ilu_byte* vec = ilu_malloc(sizeof(*vec)*len);
    unsigned int i;
    for(i = 0; i < len; i++) {
      SCM val = scm_vector_ref(svec, SCM_MAKINUM(i)); 
      vec[i] = iluguile_scheme_to_value(byte, val);
    }
    return vec;
  } else {
    return 0;
  }
}

SCM iluguile_byte_vector_to_scheme(ilu_byte* vec, unsigned int len)
{
  SCM svec = iluguile_scm_make_vector(SCM_MAKINUM(len), scheme_False);
  unsigned int i;
  for(i = 0; i < len; i++) {
    SCM val = iluguile_value_to_scheme(byte, vec[i]);
    scm_vector_set_x(svec, SCM_MAKINUM(i), val);
  }
  return svec;
}

/*************************************************************************/

ilu_shortcharacter* iluguile_scheme_to_shortcharacter_vector(SCM svec)
{
  if(svec != scheme_False) {
    unsigned int len = scm_obj_length(svec);
    ilu_shortcharacter* vec = ilu_malloc(sizeof(*vec)*len);
    unsigned int i;
    for(i = 0; i < len; i++) {
      SCM val = scm_vector_ref(svec, SCM_MAKINUM(i));
      vec[i] = iluguile_scheme_to_value(shortcharacter, val);
    }
    return vec;
  } else {
    return 0;
  }
}

SCM iluguile_shortcharacter_vector_to_scheme(ilu_shortcharacter* vec, unsigned int len)
{
  SCM svec = iluguile_scm_make_vector(SCM_MAKINUM(len), scheme_False);
  unsigned int i;
  for(i = 0; i < len; i++) {
    SCM val = iluguile_value_to_scheme(shortcharacter, vec[i]);
    scm_vector_set_x(svec, SCM_MAKINUM(i), val);
  }
  return svec;
}

/*************************************************************************/

ilu_character* iluguile_scheme_to_character_vector(SCM svec)
{
  if(svec != scheme_False) {
    unsigned int len = scm_obj_length(svec);
    ilu_character* vec = ilu_malloc(sizeof(*vec)*len);
    unsigned int i = 0;
    for(i = 0; i < len; i++) {
      SCM val = scm_vector_ref(svec, SCM_MAKINUM(i));
      vec[i] = iluguile_scheme_to_value(character, val);
    }
    return vec;
  } else {
    return 0;
  }
}

SCM iluguile_character_vector_to_scheme(ilu_character* vec, unsigned int len)
{
  SCM svec = iluguile_scm_make_vector(SCM_MAKINUM(len), scheme_False);
  unsigned int i;
  for(i = 0; i < len; i++) {
    SCM val = iluguile_value_to_scheme(character, vec[i]);
    scm_vector_set_x(svec, SCM_MAKINUM(i), val);
  }
  return svec;
}

/*************************************************************************/

SCM iluguile_fn_pointer_to_scheme(c_function_pointer p)
{
#if (SIZEOF_FN_P > SIZEOF_VOID_P)
#error "Cannot convert C function pointers to Scheme objects using simplistic scheme"
#else
  return iluguile_pointer_to_scheme((void *) p);
#endif
}

c_function_pointer iluguile_scheme_to_fn_pointer(SCM s)
{
#if (SIZEOF_FN_P > SIZEOF_VOID_P)
#error "Cannot convert C function pointers to Scheme objects using simplistic scheme"
#else
  return (c_function_pointer)iluguile_scheme_to_pointer(s);
#endif
}

SCM iluguile_pointer_to_scheme(void* p)
{
  return p ? scm_ulong2num((unsigned long)p) : scheme_False;
}

void* iluguile_scheme_to_pointer(SCM s)
{
  if(s == scheme_False)
    return 0;
  else
    return (void*)scm_num2ulong(s, (char*)SCM_ARG1, "iluguile_scheme_to_pointer");
}

/*************************************************************************/

SCM iluguile_string_to_scheme(char* str)
{
  if(str) {
    SCM s = scm_makfrom0str(str);
    ilu_free(str);
    return s;
  } else {
    return scheme_False;
  }
}

char* iluguile_scheme_to_string(SCM s)
{
  char* str = 0;
  char* tmp;
  int len;
  if(s != scheme_False) {
    gscm_2_str(&tmp, &len, &s);
    str = strncpy(ilu_malloc(len+1), tmp, len);
    str[len] = 0;
  }
  return str;
}

/*************************************************************************/

SCM iluguile_tmp_string_to_scheme(char* str)
{
  return str ? scm_makfrom0str(str) : scheme_False;
}

char* iluguile_scheme_to_tmp_string(SCM s)
{
  static char* str = 0;
  static int len = 0;

  if(s != scheme_False) {
    int newlen;
    char* tmp;
    gscm_2_str(&tmp, &newlen, &s);
    if(newlen > len) {
      str = str ? ilu_realloc(str, newlen + 1) : ilu_malloc(newlen + 1);
      len = newlen;
    }
    strncpy(str, tmp, newlen);
    str[newlen] = 0;
    return str;
  } else {
    return 0;
  }
}

/*************************************************************************/

char** iluguile_scheme_to_string_vector(SCM svec)
{
  if(svec != scheme_False) {
    unsigned int len = scm_obj_length(svec);
    char** vec = ilu_malloc(sizeof(*vec)*len);
    unsigned int i;
    for(i = 0; i < len; i++) {
      SCM val = scm_vector_ref(svec, SCM_MAKINUM(i));
      vec[i] = iluguile_scheme_to_value(string, val);
    }
    return vec;
  } else {
    return 0;
  }
}

char** iluguile_scheme_to_string_vector0(SCM svec)
{
  if(svec != scheme_False) {
    unsigned int len = scm_obj_length(svec);
    char** vec = ilu_malloc(sizeof(*vec)*(len+1));
    unsigned int i;
    for(i = 0; i < len; i++) {
      SCM val = scm_vector_ref(svec, SCM_MAKINUM(i));
      vec[i] = iluguile_scheme_to_value(string, val);
    }
    vec[i] = 0;
    return vec;
  } else {
    return 0;
  }
}

void iluguile_free_string_vector(char** vec, int num)
{
  if(vec) {
    int i = 0;
    if(num < 0) {
      while(vec[i])
	ilu_free(vec[i++]);
    } else {
      for(i = 0; i < num; i++)
	ilu_free(vec[i]);
    }
    ilu_free(vec);
  }
}

/*************************************************************************/

void iluguile_define_funcp(SCM (*func)(), char* name, int req, int opt, int var)
{
  static char scm_name[256];
  ilu_Error err;
  char* p = scm_name;
  while(*name) {
    if(name[0] == '_' && name[1] == '_') {
      *p++ = ':';
      name += 2;
    } else if(name[0] == '_') {
      *p++ = '-';
      name++;
    } else {
      *p++ = *name++;
    }
  }
  *p++ = 0;
  p = ilu_StrdupE(scm_name, &err);  /* this is a leak but needs to be. */
  ILU_MUST_BE_SUCCESS(err);
  scm_make_gsubr(p, req, opt, var, func);
}

/*************************************************************************/

SCM iluguile_scm_apply_fn(SCM proc, SCM slist)
{
  scheme_save_stack();

  /*SCM_ALLOW_INTS;*/
  *iluguile_scm_retval = scm_apply(proc, slist, scheme_eol);
  /*SCM_DEFER_INTS;*/

  scheme_restore_stack();
  return *iluguile_scm_retval;
}

SCM iluguile_scm_symbol(char* name)
{
  return scm_eval(SCM_CAR(scm_intern(name, strlen(name))));
}

SCM iluguile_scm_call0(SCM f)
{
  return iluguile_scm_apply_fn(f, scheme_eol);
}

SCM iluguile_scm_call1(SCM f, SCM a)
{
  SCM args = scheme_list(a, scheme_undefined);
  SCM ret = iluguile_scm_apply_fn(f, args);
  return ret;
}

SCM iluguile_scm_call2(SCM f, SCM a, SCM b)
{
  SCM args = scheme_list(a, b, scheme_undefined);
  SCM ret = iluguile_scm_apply_fn(f, args);
  return ret;
}

SCM iluguile_scm_call3(SCM f, SCM a, SCM b, SCM c)
{
  SCM args = scheme_list(a, b, c, scheme_undefined);
  SCM ret = iluguile_scm_apply_fn(f, args);
  return ret;
}

SCM iluguile_scm_call4(SCM f, SCM a, SCM b, SCM c, SCM d)
{
  SCM args = scheme_list(a, b, c, d, scheme_undefined);
  SCM ret = iluguile_scm_apply_fn(f, args);
  return ret;
}


SCM iluguile_scm_scall0(char* pname)
{
  return iluguile_scm_call0(iluguile_scm_symbol(pname));
}

SCM iluguile_scm_scall1(char* pname, SCM a)
{
  return iluguile_scm_call1(iluguile_scm_symbol(pname), a);
}

SCM iluguile_scm_scall2(char* pname, SCM a, SCM b)
{
  return iluguile_scm_call2(iluguile_scm_symbol(pname), a, b);
}

SCM iluguile_scm_scall3(char* pname, SCM a, SCM b, SCM c)
{
  return iluguile_scm_call3(iluguile_scm_symbol(pname), a, b, c);
}

SCM iluguile_scm_scall4(char* pname, SCM a, SCM b, SCM c, SCM d)
{
  return iluguile_scm_call4(iluguile_scm_symbol(pname), a, b, c, d);
}

#define CURRY_PROC(cclo) (SCM_VELTS(cclo)[1])
#define CURRY_ARG1(cclo) (SCM_VELTS(cclo)[2])

static SCM curry_apply(SCM self, SCM rest)
{
  return iluguile_scm_call1(CURRY_PROC(self), scheme_cons(CURRY_ARG1(self), rest));
}

/* probably need to save stack around this guy! */
SCM iluguile_scm_curry(SCM fn, SCM arg)
{
  SCM curry_apply_fn = scheme_lambda((SCM(*)())curry_apply, 0, 0, 1);
  SCM answer = scm_makcclo(curry_apply_fn, 3L);
  CURRY_ARG1(answer) = arg;
  CURRY_PROC(answer) = fn;
  return answer;
}

/*************************************************************************/

struct iluguile_init_s {
  void (*real_main)(int, char*[]);
  void (*init)();
  char* scm_file;
  char* scm_main;
};

static void
real_init(void* clos, int argc, char* argv[])
{
  struct iluguile_init_s* closure = (struct iluguile_init_s*)clos;

  iluguile_scm_seval_str("(primitive-load-path \"ice-9/boot-9.scm\")");
  iluguile_scm_seval_str("(primitive-load-path \"ice-9/slib.scm\")");

  /* restore SIGINT to normal because of guile stupidity */
  signal(SIGINT, SIG_DFL);

  if(closure->init)
    closure->init();

  if(closure->scm_file) {
    iluguile_scm_seval_file(closure->scm_file);
  }

  if(closure->scm_main) {
    if(!strcmp(closure->scm_main, "scm-style-repl")) {
      iluguile_scm_seval_str("(scm-style-repl)");
    } else {
      static char buff[2048];
      int i;
      sprintf(buff, "(%s %d (list", closure->scm_main, argc);
      for(i = 0; i < argc; i++)
	sprintf(buff + strlen(buff), " \"%s\"", argv[i]);
      sprintf(buff + strlen(buff), "))");
      iluguile_scm_seval_str(buff);
    }
  }

  if(closure->real_main)
    closure->real_main(argc, argv);
}

void
iluguile_scm_init(int argc, char* argv[], void (*real_main)(int, char*[]),
	    void (*init)(), char* scm_file, char* scm_main)
{
  struct iluguile_init_s closure;
  closure.real_main = real_main;
  closure.init = init;
  closure.scm_file = scm_file;
  closure.scm_main = scm_main;
  scm_boot_guile(argc, argv, real_init, &closure);
}
