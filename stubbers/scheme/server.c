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

#ifdef WIN32
/* dll - added to get declaration for getenv */
#include <stdlib.h>
#endif

#include "scheme.h"
#include <stdlib.h>	/* for getenv */

static void DeclareCalleeReturnValue (Type type, Context context)
{
  fprintf(context->file, "	    (retvalue #f)\n");
}

static void CalleeListArgument (Argument a, Context context)
{
  fprintf (context->file, " %s", scheme_argument_name(a));
}

static int gotOutArg = FALSE;

static void CalleeGetOutArgs (Argument a, Context context)
{
  enum PrimitiveTypes t = type_ur_kind (a->type);
  char* name = scheme_argument_name(a);
  int out = a->direction != In;

  if(out) {
    fprintf (context->file, "        (set! %s (car retvalue)) (set! retvalue (cdr retvalue))\n", name);
    gotOutArg = TRUE;
  }
}

void ReadLocalArg (Argument arg, Context context)
{
  if (arg->direction != Out) {
    enum PrimitiveTypes t = type_ur_kind(arg->type);
    int             allocate = t == object_Type || t == sequence_Type;
    int             ref = FALSE;
    char            buf[1000];

    if (t == array_Type)
      ref = TRUE;
    else if (t == sequence_Type) {
      TypeDescription d = type_description(arg->type);
      Type            elementType = d->structuredDes.sequence.type;
      enum PrimitiveTypes ept = type_ur_kind(elementType);

      if (ept == shortcharacter_Type || ept == byte_Type)
	ref = FALSE;
      else
	ref = TRUE;
    } else
      ref = FALSE;

    sprintf(buf, "(set! %s", scheme_argument_name(arg));
    UnpackValue(context, arg->type, arg->def, buf, ref, allocate);
  }
}

static void LocalArgExpression (Argument arg, char *expr)
{
  enum PrimitiveTypes t = type_ur_kind (arg->type);
  int takeAddress = FALSE;

#if 0
  if (t == record_Type || t == union_Type)
    takeAddress = TRUE;
  else
    takeAddress = FALSE;
#endif

  sprintf (expr, "%s%s", takeAddress ? "&" : "", scheme_argument_name(arg));
}

static void SizeLocalArg(refany elt, refany rock)
{
  Argument        arg = (Argument) elt;
  Context         context = (Context) rock;
  if (arg->direction != In) {
    char            argExpr[1000];
    LocalArgExpression(arg, argExpr);
    SizeType(arg->type, argExpr, context);
  }
}

void WriteLocalArg (refany elt, refany rock)
{
  Argument        arg = (Argument) elt;
  Context         context = (Context) rock;
  if (arg->direction != In) {
    char            argExpr[1000];
    LocalArgExpression(arg, argExpr);
    EncodeValue(arg->type, argExpr, context);
  }
}

static void DeclareLocalArgument (refany elt, refany rock)
{
  Argument        arg = (Argument) elt;
  Context         context = (Context) rock;
  fprintf(context->file, "            (%s #f)\n", scheme_argument_name(arg));
}

static void FreeLocalArg (Argument arg, Context context)
{
  if (arg->direction != In)
    {
      char argExpr[1000];

      LocalArgExpression (arg, argExpr);
      FreeValue (arg->type, argExpr, context);
    }

  if (type_ur_kind(arg->type) == sequence_Type)
    {
      enum PrimitiveTypes ept = type_ur_kind(type_description(arg->type)->structuredDes.sequence.type);

      if (ept != shortcharacter_Type && ept != byte_Type)
	fprintf (context->file, "\tfree((char *) %s);\n", scheme_argument_name(arg));
    }
}

static void GenerateCalleeStub (Procedure m, Context context)
{
  char           *retbuf;
  char           *tn = scheme_type_name(m->object);

  fprintf(context->file, "(define (%s:%s-stub call)\n", tn, scheme_simple_name(m->name));
  fprintf(context->file, "  (catch 'fail\n");
  fprintf(context->file, "    (lambda ()\n");
  fprintf(context->file, "      (let ((status #f)\n");
  DeclareCalleeReturnValue(m->returnType, context);
  list_enumerate(m->arguments, DeclareLocalArgument, context);
  fprintf(context->file, "            (realobj #f))\n");

  fprintf(context->file, "        (set! realobj (ilu-object:input-object call #t %s:class-record))\n", tn);
  list_enumerate(m->arguments, (iluparser_EnumProc) ReadLocalArg, context);
  MarshO(context, "(ilu:finish-parameters call realobj)");

  fprintf(context->file, "        (set! status (make-%s:status))\n", scheme_interface_name(context->interface));
  fprintf(context->file, "        (set-caller-passport status (ilu-call:caller call))\n");

  fprintf(context->file, "        (set! retvalue (%s realobj status", class_procedure_name(m));
  list_enumerate(m->arguments, (iluparser_EnumProc) CalleeListArgument, context);
  fprintf(context->file, "))\n\n");
  gotOutArg = FALSE;
  list_enumerate(m->arguments, (iluparser_EnumProc) CalleeGetOutArgs, context);
  if(gotOutArg) {
    fprintf(context->file, "        (set! retvalue (car retvalue))\n");
  }

  if (m->asynch) {
    fprintf(context->file, "\t;; asynchronous method -- no reply\n");
    fprintf(context->file, "\t(ilu:no-reply call)) #t)\n");
  } else {
    fprintf(context->file, "\t(if (not (get-return-code status))\n");
    fprintf(context->file, "\t  (let ((argsize 0) (dsize 0))\n");

    fprintf(context->file, "\t    (set! argsize (ilu:begin-sizing-reply");
    fprintf(context->file, " call %s))\n", (list_size(m->exceptions) > 0) ? "#t" : "#f");

    SizeType(m->returnType, "retvalue", context);
    list_enumerate(m->arguments, SizeLocalArg, context);

    MarshO(context, "(ilu:begin-reply call %s argsize)",
	(list_size(m->exceptions) > 0) ? "#t" : "#f");

    if (type_ur_kind(m->returnType) != void_Type) {
      EncodeValue(m->returnType, "retvalue", context);
    }

    list_enumerate(m->arguments, WriteLocalArg, context);

    fprintf(context->file, "\t(ilu:finish-reply call)\n\t)\n");
    fprintf(context->file, "\t(%s:send-exception call status))\n", scheme_interface_name(m->interface));
    fprintf(context->file, "      ))\n");

#if 0
    list_enumerate(m->arguments, (iluparser_EnumProc) FreeLocalArg, context);
    if (type_ur_kind(m->returnType) != void_Type)
      FreeValue(m->returnType, "retvalue", context);
#endif

  }
  fprintf(context->file, "    ;; catch failures\n");
  fprintf(context->file, "    (lambda (key) #f))\n");
  fprintf(context->file, "  )\n\n");
}

static void InitializeStubPointer (Procedure method, Context context)
{
  char* tname = scheme_type_name(method->object);
  char* mname = scheme_simple_name(method->name);

  fprintf (context->file, "    (ilu:set-method-stub-proc\n");
  fprintf (context->file, "     (ilu-class:method %s:class-record %s:%s-method-num) %s:%s-stub)\n", tname, tname, mname, tname, mname);
}

static void SetupServerStubsInMethodTable (Type type, Context context)
{
  Class od;

  if (type == NULL || type_basic_type(type) != object_Type || (od = class_object(type)) == NULL)
    return;

  list_enumerate (od->methods, (iluparser_EnumProc) InitializeStubPointer, context);
}

static void ClassSetupStubs (Type class, Context context)
{
  Class od;

  if (class == NULL || type_basic_type(class) != object_Type || (od = class_object(class)) == NULL)
    return;

  list_enumerate (od->methods, (iluparser_EnumProc) GenerateCalleeStub, context);
}

/* done */
static void generate_registration_code (Interface interface, Context context)
{
  char *interface_name = (char *) scheme_interface_name(interface);

  fprintf(context->file, "(define %s:stubs-init #f)\n", interface_name);
  fprintf(context->file, "(if (not %s:stubs-init)\n", interface_name);
  fprintf(context->file, "  (begin\n");

  list_enumerate(interface->classes,
      (iluparser_EnumProc) SetupServerStubsInMethodTable, context);

  fprintf(context->file, "     (set! %s:stubs-init #t)))\n", interface_name);
}

/* done? */
void generate_server_code (Interface parse, FILE *file)
{
  char*  pc_interfacename;
  struct context_s context;

  context.file = file;
  context.interface = parse;

  /* get any translation of what the header file for the interface is */
  pc_interfacename = interface_header_name(scheme_interface_name(parse));

  fprintf (file, "(require \"ilu\")\n");
  fprintf (file, "(require \"%s\")\n\n", pc_interfacename);

  list_enumerate(parse->classes, (iluparser_EnumProc) ClassSetupStubs, &context);
  list_enumerate(parse->classes, (iluparser_EnumProc) DefineMethods, &context);

  generate_registration_code (parse, &context);
}
