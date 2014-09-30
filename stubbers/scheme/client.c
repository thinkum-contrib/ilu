/*  -*- Mode: C; -*-
 *
 * Support for Guile Scheme has been contributed by Siemens Corporate Research, Inc.
 */


/** 
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

#include "scheme.h"

#ifdef MACOS
#pragma segment client
#endif

static boolean IncludeComma = FALSE;

static boolean hasMethods (Type t)
{
  return (list_size(class_object(t)->methods) > 0);
}

static void generate_methods_shorts (Type class, Context context)
{
  MethodRecordID = 0;
  DefineMethods (class, context);
}

/* done */
static void generate_InstanceFromSBH (Type type, Context context)
{
  char *tn = scheme_type_name(context->class);
  fprintf (context->file, "(define (%s:create-from-sbh sbh)\n", tn);
  fprintf (context->file, "\t(ilu:sbh-to-object sbh %s:class-record))\n\n", tn, tn);
}

/* done */
static void generate_staticCreate (Type type, Context context)
{
  char *tn = scheme_type_name (type);

  fprintf (context->file, "(define (%s:create obj)\n", tn);
  fprintf (context->file, "  (let ((nobj (make-%s)))\n", tn);
  fprintf (context->file, "    (set-rpc-object nobj obj)\n");
  fprintf (context->file, "    (ilu:set-language-specific-object obj nobj)\n");
  fprintf (context->file, "    nobj))\n\n");
}

static void InitializeCacheSlot (Procedure m, Context context)
{
  if (IsCacheableMethod(m))
    fprintf (context->file, "\t(set-%s-cache-value-bound %s #f)\n",
	     scheme_type_name(context->class),
	     class_procedure_name(m));
}

/* done */
static void generate_constructor_destructor (Type type, Context context)
{
  string tn = scheme_type_name(context->class);
  Class od = class_object(context->class);
  fprintf (context->file, "      (set-instance-class-record obj %s:class-record)\n", tn);
  list_enumerate (od->methods, (iluparser_EnumProc) InitializeCacheSlot, context);
}

static void ListArgument (Argument arg, Context context)
{
  fprintf (context->file, "-and-%s", scheme_argument_name(arg));
}

static cardinal argNum = 1;

static void BindArgument (Argument arg, Context context)
{
  fprintf(context->file, "            (%s (list-ref *args* %d))\n",
	  scheme_argument_name(arg), argNum);
  argNum++;
}

static cardinal numOutParm = 0;

static void CountOutParm(Argument arg, Context context)
{
  if(arg->direction == Out || arg->direction == InOut)
    numOutParm++;
}

static void ListOutParm(Argument arg, Context context)
{
  if(arg->direction == Out || arg->direction == InOut)
    fprintf (context->file, " %s", scheme_argument_name(arg));
}


static void generate_method_code (Procedure m, Context context)
{
  enum PrimitiveTypes t = type_ur_kind(m->returnType);
  char* tname = scheme_type_name(context->class);
  char* pname = class_procedure_name(m);

  /* print out formal params */
  fprintf(context->file, "    ((%s %s . *args*)\n", pname, tname);
  fprintf(context->file, "       (let ((call #f) (return-now #f) (retvalue #f)\n");
  fprintf(context->file, "            (status (list-ref *args* 0))\n");
  argNum = 1;
  list_enumerate(m->arguments, (iluparser_EnumProc) BindArgument, context);
  fprintf(context->file, "            )\n");
  fprintf(context->file, "         (catch 'fail\n");
  fprintf(context->file, "           (lambda ()\n");
  fprintf(context->file, "             (let ((dsize 0) (argsize 0)\n");
  if (BlockingCall(m))
    fprintf(context->file, "                   (scode 0) (perror #f)\n");
  fprintf(context->file, "                   (server (get-kernel-server %s)))\n", tname);

  /*
   * if method is functional, see if it is cached, and if so, return
   * the cached value
   */
  if (IsCacheableMethod(m)) {
    fprintf(context->file, "\t(if (get-%s-cache-value-bound %s)\n",
	    tname,  class_procedure_name(m));
    fprintf(context->file, "\t  (begin\n");
    fprintf(context->file, "\t    (set-return-code status #f)\n");
    fprintf(context->file, "\t    (set! retvalue (get-%s-cache-value %s))\n",
	    tname, class_procedure_name(m));
    fprintf(context->file, "\t    (set! return-now 'cache-return)\n");
    fprintf(context->file, "\t  ))\n");
  }

  /* start code */
  fprintf(context->file, "\t(set! call (ilu-call:create))\n");
  fprintf(context->file, "\t(if (not (ilu:start-call call server %s:class-record (ilu-class:method %s:class-record %s:%s-method-num)))\n",
	  scheme_type_name(m->object), tname, tname, pname);
  fprintf(context->file, "\t  (throw 'fail))\n");

  if (!context->class->description->structuredDes.object->singleton)
    MarshS(context, "(ilu:size-of-object-id call (get-rpc-object %s) #t #f)", tname);

  list_enumerate(m->arguments, SizeArgument, context);

  MarshO(context, "(ilu:start-request call argsize)");

  if (!context->class->description->structuredDes.object->singleton)
    MarshO(context, "(ilu:output-object-id call (get-rpc-object %s) #t #f)", tname);

  list_enumerate(m->arguments, EncodeArgument, context);

  MarshO(context, "(ilu:finish-request call)");

  if (BlockingCall(m)) {
    fprintf(context->file, "\t(set! perror (ilu:wait-for-reply call))\n");
    fprintf(context->file, "\t(set! scode (cdr perror))\n");
    fprintf(context->file, "\t(set! perror (car perror))\n");

    fprintf(context->file, "\t(if (ilu:err-nok (ilu-call:error call)) (throw 'fail))\n");
    fprintf(context->file, "\t(if (not (= perror ilu:protocol-exception-success))\n");
    fprintf(context->file, "\t  (begin\n");
    fprintf(context->file, "\t    (set-return-code status ilu:protocol-error)\n");
    fprintf(context->file, "\t    (set-status-value status perror)\n");
    fprintf(context->file, "\t    (set! return-now 'return-now)\n");
    fprintf(context->file, "\t    (throw 'fail)))\n\n");

    fprintf(context->file, "\t(if (= scode 0)\n");
    fprintf(context->file, "\t  (begin\n");
    fprintf(context->file, "\t    (set-return-code status #f)\n");

    if (t == void_Type) {
      fprintf(context->file, "\t    ;; no return value\n");
    } else {
      char            buffer[100];
      enum PrimitiveTypes t = type_ur_kind(m->returnType);
      int             ref = FALSE;

      sprintf(buffer, "    (set! retvalue");
      UnpackValue(context, m->returnType, m->def, buffer, ref, FALSE);
      fprintf(context->file, "\n");

      if (IsCacheableMethod(m)) {
	fprintf(context->file, "\t\t(if (not (get-%s-cache-value-bound %s))\n",
		tname, class_procedure_name(m));
	fprintf(context->file, "\t\t\t(begin\n");
	fprintf(context->file, "\t\t\t  (set-%s-cache-value %s retvalue)\n",
		tname, class_procedure_name(m));
	fprintf(context->file, "\t\t\t  (set-%s-cache-value-bound %s #t)\n",
		tname, class_procedure_name(m));
	fprintf(context->file, "\t\t\t))\n");
      }
    }

    list_enumerate(m->arguments, UnpackOutParm, context);
    fprintf(context->file, "\t  )"); /* should match (if (= scode 0) (begin */

    if (list_size(m->exceptions) > 0) {
      fprintf(context->file,
	      "\n\t  (%s:catch-exception call status scode))\n",
	      scheme_interface_name(context->interface));
    } else {
      fprintf(context->file, "\t);; no exceptions to catch\n");
    }

    fprintf(context->file, "\t(if (not (ilu:reply-read call)) (throw 'fail))\n");
  }

  fprintf(context->file, "\t(set! return-now 'return-now)\n");
  fprintf(context->file, "\t(throw 'fail)))\n");

  /* catch lambda */
  fprintf(context->file, "           (lambda (key)\n");
  fprintf(context->file, "             (if (not return-now)\n");
  fprintf(context->file, "               (begin\n");
  fprintf(context->file, "                 (set-return-code status ilu:protocol-error)\n");
  fprintf(context->file, "                 (set-status-value status (ilu-call:protocol-exception call))\n");
  fprintf(context->file, "               ))\n");

  fprintf(context->file, "             (if (not (eq? return-now 'cache-return))\n");
  fprintf(context->file, "               (ilu:finish-call call))\n");
  fprintf(context->file, "             (ilu-call:destroy call)\n");

  /* do return value and out parameters here!!!!!!!!! */
  /* slap all out values into a list like this (out1 out2 out3 retvalue) */
  /* if no out params then just return retvalue (not in a list!) */
  numOutParm = 0;
  list_enumerate(m->arguments, (iluparser_EnumProc) CountOutParm, context);

  if(numOutParm) {
    fprintf(context->file, "\t  (list");
    list_enumerate(m->arguments, (iluparser_EnumProc) ListOutParm, context);
    fprintf(context->file, " retvalue)))\n");
  } else {
    fprintf(context->file, "\t  retvalue))");
  }

  fprintf(context->file, "\n\t))\n\n");
}

boolean IsCacheableMethod (Procedure m)
{
  enum PrimitiveTypes t = type_basic_type(m->returnType);

  return (m->functional && list_size(m->arguments) == 0
	  && (t == object_Type || t == enumeration_Type || t == byte_Type
	      || t == integer_Type || t == shortinteger_Type
	      || t == cardinal_Type || t == shortcardinal_Type
	      || t == real_Type || t == shortreal_Type
	      || t == character_Type || t == shortcharacter_Type));
}

static cardinal numFunctional = 0;

static void CountFunctional (Procedure m, Context context)
{
  if (IsCacheableMethod(m))
    numFunctional++;
}

static void DeclareSlotForFunctional (Procedure m, Context context)
{
  if (IsCacheableMethod(m))
    {
      char* pname = class_procedure_name(m);
      fprintf (context->file, "(%s-cache-value #f)", pname);
      fprintf (context->file, "(%s-cache-value-bound #f)", pname);
    }
}

static void DeclareCacheAccessors (Procedure m, Context context)
{
  if (IsCacheableMethod(m))
    {
      char* pname = class_procedure_name(m);
      fprintf (context->file, "((get-%s-cache-value this) %s-cache-value)",
	       pname, pname);
      fprintf (context->file, "((set-%s-cache-value this val) (set! %s-cache-value val))",
	       pname, pname);
      fprintf (context->file, "((get-%s-cache-value-bound this) %s-cache-value-bound)",
	       pname, pname);
      fprintf (context->file, "((set-%s-cache-value-bound this b) (set! %s-cache-value-bound b))",
	       pname, pname);
    }
}

static void ListSupertype (Type t, Context context)
{
  Type st = ur_type(t);
  fprintf (context->file, " (%s (make-%s))",
	   scheme_type_name(st),
	   scheme_type_name(st));
}

void declare_object_type (Type type, Context c)
{
  Class od = class_object(type);

  c->class = type;

  fprintf (c->file, "\n\n;; declaration of scm class \"%s\" from ILU class \"%s:%s\"\n",
	   scheme_type_name(type), interface_name(type->interface), type_name(type));

  fprintf (c->file, "\n(define (make-%s)\n", scheme_type_name(type));

  fprintf (c->file, "\t(let* (");

  /* for any methods that are functional and have no arguments,
     generate slots in which to cache the values */
  list_enumerate (od->methods, (iluparser_EnumProc) DeclareSlotForFunctional, c);

  fprintf (c->file, "(obj (ilu-object-with-ancestors (", scheme_type_name(type));

  if (od->superclasses != NULL && list_size(od->superclasses) > 0) {
    list_enumerate (od->superclasses, (iluparser_EnumProc) ListSupertype, c);
  } else {
    fprintf (c->file, "(ilu-parent-object (make-ilu:object))");
  }
  fprintf (c->file, ")\n");

  list_enumerate (od->methods, (iluparser_EnumProc)DeclareCacheAccessors, c);
}

void generate_class_code (Type type, Context context)
{
  context->class = type;

  generate_methods_shorts(type, context);
  generate_InstanceFromSBH(type, context);
  declare_object_type(type, context);

  list_enumerate((class_object(type))->methods,
	       (iluparser_EnumProc) generate_method_code, context);

  fprintf(context->file, "\t)))\n");

  generate_constructor_destructor(type, context);

  fprintf(context->file, "    obj))\n\n");

  generate_staticCreate(type, context);
}
