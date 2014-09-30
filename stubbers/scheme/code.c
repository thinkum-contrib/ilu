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

#ifdef MACOS
#pragma segment cplcode
#endif

#include <stdlib.h>	/* for exit() */

#include "scheme.h"

/***********************************************************************\
*************************************************************************
**
**		Global class code
**
*************************************************************************
\***********************************************************************/

/*=====================================================================*\
**=====================================================================**
**              Status
**=====================================================================**
\*=====================================================================*/

static void declare_status_struct (Interface i, Context c)
{
  char* iname = scheme_interface_name(i);
  fprintf (c->file, "(define %s:reply-success ilu:reply-success)\n\n", iname);
  fprintf (c->file, "(define (make-%s:status) (ilu-object-with-ancestors ((status (make-ilu:status #f)))))\n", iname);
}

/*=====================================================================*\
**=====================================================================**
**              Exceptions
**=====================================================================**
\*=====================================================================*/

int ExceptionNumber = 0;

static void DefineException(refany elt, refany rock)
{
  Exception       e = (Exception) elt;
  Context         context = (Context) rock;
  /* do nothing.... exceptions are now regular type of object? */
  /*declare_type(e->type, context);*/
}

static void RegisterException(refany elt, refany rock)
{
  Exception       e = (Exception) elt;
  Context         context = (Context) rock;
  if (e->interface != context->interface || e->importInterfaceName != NULL)
    return;
  fprintf(context->file, "    (vector-set! vec %d (ilu:define-exception", ExceptionNumber);
  if (e->corba_rep_id == NULL) {
    fprintf(context->file, " \"%s\" \"%s\"))\n", interface_name(e->interface), exception_name(e));
  } else {
    fprintf(context->file, " #f \"%s\"))\n", e->corba_rep_id);
  }
  ExceptionNumber++;
  return;
}

static void AliasException(refany elt, refany rock)
{
  Exception e = (Exception) elt;
  char* iname = interface_name(e->interface);
  char* ename = exception_name(e);
  Context context = (Context) rock;
  if (e->interface != context->interface || e->importInterfaceName != NULL)
    return;
  fprintf(context->file, "(define %s:%s-exception (vector-ref %s:exceptions %d))\n",
	  iname, ename, iname, ExceptionNumber);
  ExceptionNumber++;
  return;
}

/* done */
static void generate_exception_table (Interface interface, Context context)
{
  char *iname = (char *) scheme_interface_name(interface);

  /* define exception types */
  ExceptionNumber = 0;
  list_enumerate(interface->exceptions, DefineException, context);
  fprintf(context->file, "\n");

  /* register exception types */
  fprintf(context->file, "(define %s:exceptions\n", iname);
  fprintf(context->file, "  (let ((vec (make-vector %d)))\n", list_size(interface->exceptions));
  fprintf(context->file, "    (ilu:enter-ot-mu)\n");
  ExceptionNumber = 0;
  list_enumerate(interface->exceptions, RegisterException, context);
  fprintf(context->file, "    (ilu:exit-ot-mu)\n");
  fprintf(context->file, "    vec))\n\n");

  /* alias exception types */
  ExceptionNumber = 0;
  list_enumerate(interface->exceptions, AliasException, context);
  fprintf(context->file, "\n");
}

static void CatchException (Exception e, Context context)
{
  enum PrimitiveTypes t = type_ur_kind (e->type);
  character buf[1000];

  if (t != void_Type)
    {
      fprintf(context->file, "         ((equal? ex %s-exception)\n", scheme_exception_name(e));
      sprintf(buf,           "   (set-status-value val ");
      UnpackValue (context, e->type, e->def, buf, FALSE, FALSE);
      fprintf(context->file, "\n         )\n");
    }
}

static void generate_catch_exception (Context context)
{
  char* iname = scheme_interface_name(context->interface);

  fprintf(context->file, "(define (%s:catch-exception call val ecode)\n", iname);
  fprintf(context->file, "  (let* ((m (ilu-call:method-of-call call))\n");
  fprintf(context->file, "         (ec (ilu:exception-count-of-method m)))\n");
  fprintf(context->file, "   (if (or (> ecode ec) (= ecode 0))\n");
  fprintf(context->file, "     (begin\n");
  fprintf(context->file, "       (set-return-code val ilu:protocol-error)\n");
  fprintf(context->file, "       (set-status-value val ilu:protocol-exception-unknown))\n");
  fprintf(context->file, "     (let ((ex (ilu:exception-of-method m ecode)))\n");
  fprintf(context->file, "       (set-return-code val ex)\n");
  fprintf(context->file, "       (cond\n");
  fprintf(context->file, "         ((not ex))\n");
  list_enumerate(context->interface->exceptions, (iluparser_EnumProc) CatchException, context);
  fprintf(context->file, "       )\n");
  fprintf(context->file, "     ))))\n\n");
}

static void SizeException (refany elt, refany rock)
{
  Exception       e = (Exception) elt;
  Context         context = (Context) rock;
  enum PrimitiveTypes t = type_ur_kind(e->type);
  char            buf[1000];

  if (e->type != NULL) {
    fprintf(context->file, "         ((equal? code %s-exception)\n", scheme_exception_name(e));
    sprintf(buf, "val");
    SizeType(e->type, buf, context);
    fprintf(context->file, "\n         )\n");
  }
}

static void SendExceptionValue (refany elt, refany rock)
{
  Exception       e = (Exception) elt;
  Context         context = (Context) rock;
  enum PrimitiveTypes t = type_ur_kind(e->type);

  if (e->type == NULL)
    return;

  if (e->type != NULL) {
    fprintf(context->file, "         ((equal? code %s-exception)\n", scheme_exception_name(e));
    EncodeValue(e->type, "val", context);
    fprintf(context->file, "\n         )\n");
  }
}

void generate_send_exception (Context context)
{
  char* iname = scheme_interface_name(context->interface);
  fprintf(context->file, "(define (%s:send-exception call status)\n", iname);
  fprintf(context->file, "  (catch 'fail\n");
  fprintf(context->file, "    (lambda ()\n");
  fprintf(context->file, "      (let* ((argsize 0) (dsize 0) (ecode 0) (stop #f)\n");
  fprintf(context->file, "             (m (ilu-call:method-of-call call))\n");
  fprintf(context->file, "             (limit (ilu:exception-count-of-method m))\n");
  fprintf(context->file, "             (code (get-return-code status))\n");
  fprintf(context->file, "             (val (get-status-value status)))\n");
  fprintf(context->file, "        (do ((i 1 (+ i 1)))\n");
  fprintf(context->file, "            ((or (> i limit) stop))\n");
  fprintf(context->file, "          (if (equal? (ilu:exception-of-method m i) code)\n");
  fprintf(context->file, "            (begin (set! ecode i) (set! stop #t))))\n\n");
  fprintf(context->file, "        (if (<= ecode 0) (error \"%s:send-exception\"))\n", iname);
  fprintf(context->file, "        (set! argsize (ilu:begin-sizing-exception call ecode))\n");
  fprintf(context->file, "        (cond\n");
  fprintf(context->file, "          ((not code) (error \"can't happen\"))\n");
  list_enumerate(context->interface->exceptions, SizeException, context);
  fprintf(context->file, "        )\n");
  MarshO(context, "(ilu:begin-exception call ecode argsize)");
  fprintf(context->file, "        (cond\n");
  fprintf(context->file, "          ((not code) (error \"can't happen\"))\n");
  list_enumerate(context->interface->exceptions, SendExceptionValue, context);
  fprintf(context->file, "        )\n");
  fprintf(context->file, "        (ilu:finish-exception call))\n");
  fprintf(context->file, "      #t)\n");
  fprintf(context->file, "    (lambda (key) #f))\n");
  fprintf(context->file, "  )\n\n");
}

static void SetExceptionValue (Exception e, Context context)
{
  char* ename = scheme_exception_name(e);

  if (e->type != NULL) {
    fprintf(context->file, "    ((equal? exception %s-exception)\n", ename);
    fprintf(context->file, "      (set-status-value status values)\n", ename);
    fprintf(context->file, "    )\n");
  }
}

static void generate_signal_exception (Context context)
{
  char* iname = scheme_interface_name(context->interface);
  fprintf (context->file, "(define (%s:raise-exception status exception values)\n", iname);
  fprintf(context->file, "  (set-return-code status exception)\n");
  fprintf(context->file, "  (set-status-value status values)\n");
  fprintf (context->file, ")\n\n");
}

static void generate_exception_procs (Context context)
{
  generate_catch_exception (context);
  generate_signal_exception (context);
  generate_send_exception (context);
}

/*=====================================================================*\
**=====================================================================**
**              IO for non-object types
**=====================================================================**
\*=====================================================================*/

static Type foundEnum = 0;

static void lookup_enum(refany t, refany n)
{
  Type type = (Type)t;
  char* name = (char*)n;
  if(type_ur_kind (t) == enumeration_Type) {
    TypeDescription td = type_description(t);
    list fields = td->structuredDes.enumeration;
    int i;

    for(i = 0; i < list_size(fields); i++) {
      EnumField ef = (EnumField)list_ref(fields, i);
      if(!strcmp(name, ef->name))
	foundEnum = t;
    }
  }
}

static Type lookup_enumeration(Interface intf, char* field_name)
{
  foundEnum = 0;
  list_enumerate(intf->types, lookup_enum, field_name);
  return foundEnum;
}

static unsigned long DimCount;  /* uneeded? */

static void PrintDim(refany elt, refany rock)
{
  long int        d = (long int) elt;
  Context         context = (Context) rock;
  fprintf(context->file, " %lu", d);
  DimCount++;
}

struct double_s {
  Context c;
  Type t;
  Argument default_arm;
};

static void OutputRecordArg (refany elt, refany rock)
{
  Argument        arg = (Argument) elt;
  Context         context = (Context) rock;
  char            buf[1000];
  sprintf(buf, "(get-%s val)", scheme_argument_name(arg));
  EncodeValue(arg->type, buf, context);
}

static Type unionMemberType;
static int doDefaultArm = 0;

static void outputCaseConst (ConstantValue val, struct double_s *s)
{
  switch (val->type)
    {
    case integer_Type:
    case shortinteger_Type:
    case cardinal_Type:
    case shortcardinal_Type:
    case byte_Type:
      fprintf(s->c->file, "\t((eq? (car val) %s%lu)", (val->val.i.sign<0) ? "-" : "", val->val.i.value);
      EncodeValue(unionMemberType, "(cdr val)", s->c);
      fprintf(s->c->file, "\t  )\n");
      break;
    case shortcharacter_Type:
      {
	Type et = lookup_enumeration(s->c->interface, val->val.s);
	fprintf(s->c->file, "\t((eq? (car val) %s:%s)\n", scheme_type_name(et), val->val.s);
	EncodeValue(unionMemberType, "(cdr val)", s->c);
	fprintf(s->c->file, "\t  )\n");
      }
      break;
    case boolean_Type:
      fprintf(s->c->file, "\t((eq? (car val) %s)\n", val->val.b ? "#t" : "#f");
      EncodeValue(unionMemberType, "(cdr val)", s->c);
      fprintf(s->c->file, "\t  )\n");
      break;
    default:
      {
	fprintf (stderr, "illegal discriminator value\n");
	exit (1);
      }
    }
}

static void OutputUnionType (refany elt, refany rock)
{
  Argument        a = (Argument) elt;
  struct double_s *s = (struct double_s *) rock;

  if (s->default_arm == a) {
    if(doDefaultArm)
      EncodeValue(a->type, "(cdr val)", s->c);
  } else {
    if(!doDefaultArm) {
      unionMemberType = a->type;
      list_enumerate(a->values, (iluparser_EnumProc) outputCaseConst, s);
    }
  }
}

static void generate_output_code (Type type, enum PrimitiveTypes t, Context context)
{
  TypeDescription d = type_description(type);
  char* iname =  scheme_interface_name(context->interface);
  char* tname = scheme_simple_name(type->name);

  fprintf(context->file, "(define (%s:output-%s call val)\n", iname, tname);
  fprintf(context->file, "  (catch 'fail\n");
  fprintf(context->file, "    (lambda ()\n");
  if (t == array_Type) {
    MarshO(context, "(ilu:output-array call)");
    fprintf(context->file, "\t(ilu:map-rec (lambda (elem) ");
    EncodeValue(d->structuredDes.array.type, "elem", context);
    fprintf(context->file, ") val)\n");
    fprintf(context->file, "\t(ilu:end-array call)\n");
  } else if (t == sequence_Type) {
    char            buf[200];
    Type            st = d->structuredDes.sequence.type;

    if (type_ur_kind(st) == byte_Type) {
      MarshO(context, "(ilu:output-bytes call (list->vector val) (length val) %lu)",
	     d->structuredDes.sequence.limit);
    } else {
      MarshO(context, "(ilu:output-sequence call (length val) %lu)",
	     d->structuredDes.sequence.limit);
      fprintf(context->file, "\t(ilu:map-rec (lambda (elem) ");
      EncodeValue(d->structuredDes.sequence.type, "elem", context);
      fprintf(context->file, ") val)\n");
      fprintf(context->file, "\t(ilu:end-sequence call)\n");
    }
  } else if (t == union_Type) {
    struct double_s s;

    s.c = context;
    s.t = ur_type(d->structuredDes.uniond.discriminator_type);
    s.default_arm = d->structuredDes.uniond.default_arm;

    MarshO(context, "(ilu:output-union call (car val) ilu:size-of-discriminator)");
    fprintf(context->file, "\t(cond\n");
    doDefaultArm = 0;
    list_enumerate(d->structuredDes.uniond.types, OutputUnionType, &s);
    if (s.default_arm != NULL) {
      doDefaultArm = 1;
      fprintf(context->file, "\t  (#t\n");
      list_enumerate(d->structuredDes.uniond.types, OutputUnionType, &s);
      fprintf(context->file, "\t  ))\n");  /* end of cond here */
    } else if (d->structuredDes.uniond.others_allowed) {
      fprintf(context->file, "\t  (#t)\n");
      fprintf(context->file, "\t)\n");
    } else {
      fprintf(context->file, "\t  (#t\n");
      fprintf(context->file, "\t    (error \"Bad value \" (car val) \"  in discriminant of value of union type %s.\"))", scheme_type_name(type));
      fprintf(context->file, "\t)\n");
    }
    fprintf(context->file, "\t(ilu:end-union call)\n");
  } else if (t == record_Type) {
    MarshO(context, "(ilu:output-record call)");
    list_enumerate(d->structuredDes.record.fields, OutputRecordArg, context);
    fprintf(context->file, "\t(ilu:end-record call)\n");
  }
  /* else if (t == object_Type) done elsewhere */

  fprintf(context->file, "     #t)\n");               /* succeed lambda */
  fprintf(context->file, "   (lambda (key) #f))\n");  /* fail lambda */
  fprintf(context->file, " )\n\n");                   /* end of define */
}

static void InputRecordArg (Argument arg, Context context)
{
  enum PrimitiveTypes t = type_ur_kind (arg->type);
  char            buf[1000];
  sprintf(buf, "(set-%s val", scheme_argument_name(arg));
  UnpackValue (context, arg->type, 0, buf, FALSE, FALSE);
  fprintf(context->file, "\n");
}

static void inputCaseConst (ConstantValue val, struct double_s *s)
{
  switch (val->type)
    {
    case integer_Type:
    case shortinteger_Type:
    case cardinal_Type:
    case shortcardinal_Type:
    case byte_Type:
      fprintf(s->c->file, "\t((eq? (car val) %s%lu)", (val->val.i.sign<0) ? "-" : "", val->val.i.value);
      UnpackValue(s->c, unionMemberType, 0, "(set-cdr! val", FALSE, FALSE);
      fprintf(s->c->file, "\t  )\n");
      break;
    case shortcharacter_Type:
      {
	Type et = lookup_enumeration(s->c->interface, val->val.s);
	fprintf(s->c->file, "\t((eq? (car val) %s:%s)\n", scheme_type_name(et), val->val.s);
	UnpackValue(s->c, unionMemberType, 0, "(set-cdr! val", FALSE, FALSE);
	fprintf(s->c->file, "\t  )\n");
      }
      break;
    case boolean_Type:
      fprintf(s->c->file, "\t((eq? (car val) %s)\n", val->val.b ? "#t" : "#f");
      UnpackValue(s->c, unionMemberType, 0, "(set-cdr! val", FALSE, FALSE);
      fprintf(s->c->file, "\t  )\n");
      break;
    default:
      {
	fprintf (stderr, "illegal discriminator value\n");
	exit (1);
      }
    }
}

static void InputUnionType (Argument a, struct double_s *s)
{
  if (s->default_arm == a) {
    if(doDefaultArm)
      UnpackValue(s->c, a->type, 0, "(set-cdr! val", FALSE, FALSE);
  } else {
    if(!doDefaultArm) {
      unionMemberType = a->type;
      list_enumerate(a->values, (iluparser_EnumProc) inputCaseConst, s);
    }
  }
}

static void generate_input_code (Type type, enum PrimitiveTypes t, Context context)
{
  char *ret = (char *) scheme_return_type(type);
  TypeDescription d = type_description(type);
  char* iname = scheme_interface_name(context->interface);
  char* name = scheme_type_name(type);
  char* refType = t == array_Type ? scheme_parameter_type(type, In) : ret;

  context->class = type;
  fprintf (context->file, "(define (%s:input-%s call)\n", iname, scheme_simple_name(type->name));
  fprintf (context->file, "  (let ((val #f))\n");

  if (t == array_Type)
    {
      fprintf (context->file, "\t(ilu:input-array call)\n");

      fprintf (context->file, "\t(set! val (ilu:input-array-rec (lambda () (let ((tmp #f))");
      UnpackValue (context, d->structuredDes.array.type, type->def, "(set! tmp", FALSE, FALSE);
      fprintf (context->file, " tmp)) ");

      fprintf (context->file, "(list");
      list_enumerate(d->structuredDes.array.dimensions, PrintDim, context);
      fprintf (context->file, ")))");

      fprintf (context->file, "\t(ilu:end-array call)\n");
    }
  else if (t == sequence_Type)
    {
      Type et = d->structuredDes.sequence.type;
      string tname = (string) scheme_return_type(et);

      if (type_ur_kind(et) == byte_Type)
	{
	  fprintf (context->file, "\t(set! val (vector->list (ilu:input-bytes call %lu)))\n", d->structuredDes.sequence.limit);
	}
      else
	{
	  fprintf (context->file, "\t(let ((count (ilu:input-sequence call %lu)))\n",
		   d->structuredDes.sequence.limit);

	  fprintf (context->file, "\t(set! val (ilu:input-sequence-rec (lambda () (let ((tmp #f))");
	  UnpackValue (context, d->structuredDes.sequence.type, type->def, "(set! tmp", FALSE, FALSE);
	  fprintf (context->file, " tmp)) ");
	  fprintf (context->file, "(list count)))");

	  fprintf (context->file, "\t(ilu:end-sequence call))\n");
	}
    }
  else if (t == union_Type)
    {
      struct double_s s;

      s.c = context;
      s.t = ur_type(d->structuredDes.uniond.discriminator_type);
      s.default_arm = d->structuredDes.uniond.default_arm;

      fprintf (context->file, "\t(set! val (cons #t #t))\n");
      fprintf (context->file, "\t(set-car! val (ilu:input-union call ilu:size-of-discriminator))\n");
      fprintf(context->file, "\t(cond\n");
      doDefaultArm = 0;
      list_enumerate (d->structuredDes.uniond.types, (iluparser_EnumProc) InputUnionType, &s);
      if (s.default_arm != NULL) {
	doDefaultArm = 1;
	fprintf(context->file, "\t  (#t\n");
	list_enumerate (d->structuredDes.uniond.types, (iluparser_EnumProc) InputUnionType, &s);
	fprintf(context->file, "\t  ))\n");  /* end of cond here */
      } else if (d->structuredDes.uniond.others_allowed) {
	  fprintf(context->file, "\t  (#t)\n");
	  fprintf(context->file, "\t)\n");
	}
      else /* generate error on bad discriminator value */
	{
	  fprintf(context->file, "\t  (#t\n");
	  fprintf(context->file, "\t    (error \"Bad value \" (car val) \"  received for discriminant of value of union type %s.\"))", scheme_type_name(type));
	  fprintf(context->file, "\t)\n");
	}
      fprintf (context->file, "\t(ilu:end-union call)\n");
    }
  else if (t == record_Type)  /* really scheme class */
    {
      fprintf (context->file, "\t(ilu:input-record call)\n");
      fprintf (context->file, "\t(set! val (make-%s))\n", name);
      list_enumerate (d->structuredDes.record.fields, (iluparser_EnumProc) InputRecordArg, context);
      fprintf (context->file, "\t(ilu:end-record call)\n");
    }
  /* else if (t == object_Type) done elsewhere */

  fprintf (context->file, "  val))\n\n");
}

static void sizeCaseConst (ConstantValue val, struct double_s *s)
{
  switch (val->type)
    {
    case integer_Type:
    case shortinteger_Type:
    case cardinal_Type:
    case shortcardinal_Type:
    case byte_Type:
      fprintf(s->c->file, "\t((eq? (car val) %s%lu)", (val->val.i.sign<0) ? "-" : "", val->val.i.value);
      SizeType(unionMemberType, "(cdr val)", s->c);
      fprintf(s->c->file, "\t  )\n");
      break;
    case shortcharacter_Type:
      {
	Type et = lookup_enumeration(s->c->interface, val->val.s);
	fprintf(s->c->file, "\t((eq? (car val) %s:%s)\n", scheme_type_name(et), val->val.s);
	SizeType(unionMemberType, "(cdr val)", s->c);
	fprintf(s->c->file, "\t  )\n");
      }
      break;
    case boolean_Type:
      fprintf(s->c->file, "\t((eq? (car val) %s)\n", val->val.b ? "#t" : "#f");
      SizeType(unionMemberType, "(cdr val)", s->c);
      fprintf(s->c->file, "\t  )\n");
      break;
    default:
      {
	fprintf (stderr, "illegal discriminator value\n");
	exit (1);
      }
    }
}

static void SizeOfUnionType (refany elt, refany rock)
{
  Argument        a = (Argument) elt;
  struct double_s *s = (struct double_s *) rock;

  if (s->default_arm == a) {
    if(doDefaultArm)
      SizeType(a->type, "(cdr val)", s->c);
  } else {
    if(!doDefaultArm) {
      unionMemberType = a->type;
      list_enumerate(a->values, (iluparser_EnumProc) sizeCaseConst, s);
    }
  }
}

static void SizeRecordArg (refany elt, refany rock)
{
  Argument        arg = (Argument) elt;
  Context         context = (Context) rock;
  char            buf[1000];
  sprintf(buf, "(get-%s val)", scheme_argument_name(arg));
  SizeType(arg->type, buf, context);
}

static void 
generate_sizeof_code(Type type, enum PrimitiveTypes t, Context context)
{
  TypeDescription d = type_description(type);
  char* iname = scheme_interface_name(context->interface);
  char* tname = scheme_simple_name(type->name);

  fprintf(context->file, "(define (%s:size-of-%s call val)\n", iname, tname);
  fprintf(context->file, "  (catch 'fail\n");
  fprintf(context->file, "    (lambda ()\n");
  fprintf(context->file, "      (let ((dsize 0) (argsize 0))\n");

  if (t == array_Type) {
    fprintf(context->file, "\t(set! argsize (ilu:size-of-array call))\n");
    fprintf(context->file, "\t(ilu:map-rec (lambda (elem) ");
    SizeType(d->structuredDes.array.type, "elem", context);
    fprintf(context->file, ") val)\n");
    fprintf(context->file, "\t(ilu:end-array call)\n");
  } else if (t == sequence_Type) {
    Type            st = d->structuredDes.sequence.type;

    if (type_ur_kind(st) == byte_Type) {
      fprintf(context->file, "\t(set! argsize (ilu:size-of-bytes call (list->vector val) (length val) %lu))\n",
	      d->structuredDes.sequence.limit);
    } else {
      MarshS(context, "(ilu:size-of-sequence call (length val) %lu)",
	     d->structuredDes.sequence.limit);

      fprintf(context->file, "\t(ilu:map-rec (lambda (elem) ");
      SizeType(d->structuredDes.sequence.type, "elem", context);
      fprintf(context->file, ") val)\n");

      fprintf(context->file, "\t(ilu:end-sequence call)\n");
    }
  } else if (t == union_Type) {
    struct double_s s;

    s.c = context;
    s.t = ur_type(d->structuredDes.uniond.discriminator_type);
    s.default_arm = d->structuredDes.uniond.default_arm;

    MarshS(context, "(ilu:size-of-union call (car val) ilu:size-of-discriminator)");
    fprintf(context->file, "\t(cond\n");
    doDefaultArm = 0;
    list_enumerate(d->structuredDes.uniond.types, SizeOfUnionType, &s);
    if (s.default_arm != NULL) {
      doDefaultArm = 1;
      fprintf(context->file, "\t  (#t\n");
      list_enumerate(d->structuredDes.uniond.types, SizeOfUnionType, &s);
      fprintf(context->file, "\t  ))\n");  /* end of cond here */
    } else if (d->structuredDes.uniond.others_allowed) {
      fprintf(context->file, "\t  (#t)\n");
      fprintf(context->file, "\t)\n");
    } else {
      fprintf(context->file, "\t  (#t\n");
      fprintf(context->file, "\t    (error \"Bad value \" (car val) \"  in discriminant of value of union type %s.\"))", scheme_type_name(type));
      fprintf(context->file, "\t)\n");
    }
    fprintf(context->file, "\t(ilu:end-union call)\n");
  } else if (t == record_Type) {
    fprintf(context->file, "\t(set! argsize (ilu:size-of-record call))\n");
    list_enumerate(d->structuredDes.record.fields, SizeRecordArg, context);
    fprintf(context->file, "\t(ilu:end-record call)\n");
  }
  /* else if (t == object_Type) done elsewhere */

  fprintf(context->file, "        argsize))\n");
  fprintf(context->file, "    (lambda (key) 0))\n");
  fprintf(context->file, "  )\n\n");

}

static void generate_type_io_code (Type type, Context context)
{
  enum PrimitiveTypes t = type_ur_kind(type);

  if ((TypeIsNonObjectStruct(type)
       || (TypeIsArray(type)
	   && (((type_ur_kind(type_description(type)->structuredDes.array.type) != byte_Type)
		 && (type_ur_kind(type_description(type)->structuredDes.array.type) != shortcharacter_Type))
		|| (list_size(type_description(type)->structuredDes.array.dimensions) > 1)))
       || (t == sequence_Type
	   && ((type_ur_kind(type_description(type)->structuredDes.array.type) != shortcharacter_Type)
		|| (list_size(type_description(type)->structuredDes.array.dimensions) > 1))))
      && (type->importInterfaceName == NULL))
    {
      generate_output_code (type, t, context);
      generate_input_code (type, t, context);
      generate_sizeof_code (type, t, context);
    }

#if 0
  if (HasFreeRoutine(type))
    generateFreeCode (type, t, context);
#endif
}

static void generate_ios_code (Interface interface, Context context)
{
  list_enumerate (interface->types, (iluparser_EnumProc) generate_type_io_code, context);
}

void generate_global_code (Interface interface, Context context)
{
  generate_exception_table (interface, context);
  generate_exception_procs (context);
  generate_ios_code (interface, context);
}

static unsigned MethodIndex = 0;

static void FillException (refany elt, refany rock)
{
  Exception       e = (Exception) elt;
  Context         context = (Context) rock;
  fprintf(context->file, "\t\t\t%s-exception\n", scheme_exception_name(e));
}

static void GenMethDef (refany elt, refany rock)
{
  Procedure       m = (Procedure) elt;
  Context         context = (Context) rock;
  char           *tn = scheme_type_name(context->class);
  unsigned        nexns = 0;

  if (m->exceptions != NULL)
    nexns = list_size(m->exceptions);

  fprintf(context->file, "\t(ilu:define-method %s:class-record\n", tn);
  fprintf(context->file, "\t\t%u\t;; method index\n", MethodIndex);
  fprintf(context->file, "\t\t\"%s\"\t;; name\n", procedure_name(m));
  fprintf(context->file, "\t\t%u\t;; method ID\n", m->id);
  fprintf(context->file, "\t\t%s\t;; cacheable\n", m->functional ? "#t" : "#f");
  fprintf(context->file, "\t\t%s\t;; asynchronous\n", m->asynch ? "#t" : "#f");
  fprintf(context->file, "\t\t%u\t;; num. exns\n", nexns);
  if(!nexns) {
    fprintf(context->file, "\t\t#f)\n");
  } else {
    fprintf(context->file, "\t\t(vector\n");
    list_enumerate(m->exceptions, FillException, context);
    fprintf(context->file, "\t\t))\n");
  }
  MethodIndex += 1;
}

static void GenerateMethodDefs(Type t, Context context)
{
  Class           od;
  if (t == NULL || type_basic_type(t) != object_Type
      || (od = class_object(t)) == NULL)
    return;
  MethodIndex = 0;
  list_enumerate(od->methods, GenMethDef, context);
}

static void PrintTypeID (refany elt, refany rock)
{
  Type            st = (Type) elt;
  Context         context = (Context) rock;
  for (st = st; st->supertype != NULL; st = st->supertype);
  fprintf(context->file, "\t\t\t\"%s\"\n", st->uid);
}

static void RegisterClass (refany elt, refany rock)
{
  Type            class = (Type) elt;
  Context         context = (Context) rock;
  char           *tn = scheme_type_name(class);
  Class           o = class_object(class);
  unsigned        nmeth;

  if (class->builtIn || class->importInterfaceName != NULL)
    return;

  nmeth = list_size(o->methods);
  
  fprintf(context->file, "\t(set! %s:class-record (ilu:define-object-type\n", tn);
  fprintf(context->file, "\t\t\"%s.%s\"\t;; ILU name\n",
	  name_base_name(context->interface->name),
	  name_base_name(class->name));
  fprintf(context->file, "\t\t\"%s\"\t;; Brand\n", o->brand == NULL ? "" : o->brand);
  fprintf(context->file, "\t\t\"%s\"\t;; id\n", class->uid);
  fprintf(context->file, "\t\t%s%s%s\t;; singleton\n",
	  o->singleton == NULL ? "" : "\"",
	  o->singleton == NULL ? "#f" : o->singleton,
	  o->singleton == NULL ? "" : "\"");
  fprintf(context->file, "\t\t%s\t;; optional?\n", o->optional ? "#t" : "#f");
  fprintf(context->file, "\t\t%s\t;; collectible?\n", o->collectible ? "#t" : "#f");
  fprintf(context->file, "\t\t%s%s%s\t;; authentication\n",
	  o->authentication == NULL ? "" : "\"",
	  o->authentication == NULL ? "#f" : o->authentication,
	  o->authentication == NULL ? "" : "\"");

  fprintf(context->file, "\t\t%u\t;; number of methods\n", nmeth);

  if (o->superclasses == NULL || list_size(o->superclasses) == 0) {
    fprintf(context->file, "\t\t0\t;; number of superclasses\n");
    fprintf(context->file, "\t\t#f))\t;; no superclass uids\n");
  } else {
    fprintf(context->file, "\t\t%lu\t;; number of superclasses\n", list_size(o->superclasses));
    fprintf(context->file, "\t\t(vector\n");
    list_enumerate(o->superclasses, PrintTypeID, context);
    fprintf(context->file, "\t\t))) ;; superclass uids\n");
  }
  fprintf(context->file, "\t(ilu-object:register-surrogate-creator %s:class-record %s:create)\n", tn, tn);
  
  context->class = class;
  GenerateMethodDefs(class, context);
  fprintf(context->file, "\t(ilu:object-type-defined %s:class-record)\n", tn);
}


static void DeclareClassRecord(Type type, Context context)
{
  fprintf(context->file, "(define %s:class-record #f)\n", scheme_type_name(type));
}

/* done */
static void generate_registration_code (Interface interface, Context context)
{
  char *iname = (char *) scheme_interface_name(interface);

  list_enumerate(interface->classes, (iluparser_EnumProc)DeclareClassRecord, context);

  fprintf(context->file, "(define %s:did-init #f)\n", iname);
  fprintf(context->file, "(define (%s:init)\n", iname);
  fprintf(context->file, "  (if (not %s:did-init)\n", iname);
  fprintf(context->file, "    (begin\n");
  fprintf(context->file, "      (ilu:enter-ot-mu)\n");
  list_enumerate(interface->classes, RegisterClass, context);
  fprintf(context->file, "      (set! %s:did-init #t)\n", iname);
  fprintf(context->file, "      (ilu:exit-ot-mu))))\n\n");
  fprintf(context->file, "(%s:init)\n\n", iname);
}

/***********************************************************************\
*************************************************************************
**
**		Global class
**
*************************************************************************
\***********************************************************************/

extern void generate_headers (Interface interface, FILE *file);

void generate_code (Interface interface, FILE *file)
{
  struct context_s context;
  char*  pc_interfacename;

  context.file = file;
  context.interface = interface;

  fprintf (file, "(require \"ilu\")\n\n");

  declare_status_struct (interface, &context);

  generate_headers(interface, context.file);

  generate_registration_code (interface, &context);
}
