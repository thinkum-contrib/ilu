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

/*
$Id: type-basics.c,v 1.37 1999/08/03 01:50:10 janssen Exp $
*/

#include "lisp.h"

extern void GenerateClassCode (Type, Context);
extern void GenerateClassDefinition (Type, Context);

static char *OMGIDLExceptionTypePrefix = "ilu--prefix-idlExceptionType-";

void OutputTypeName (Type type, Context context)
{
  enum PrimitiveTypes t = type_ur_kind(type);

  if (t == byte_Type) 
    fprintf (context->file, " (:primitive :byte)");
  else if (t == shortinteger_Type) 
    fprintf (context->file, " (:primitive :short-integer)");
  else if (t == longinteger_Type) 
    fprintf (context->file, " (:primitive :long-integer)");
  else if (t == shortcharacter_Type) 
    fprintf (context->file, " (:primitive :short-character)");
  else if (t == character_Type) 
    fprintf (context->file, " (:primitive :character)");
  else if (t == integer_Type) 
    fprintf (context->file, " (:primitive :integer)");
  else if (t == shortcardinal_Type) 
    fprintf (context->file, " (:primitive :short-cardinal)");
  else if (t == longcardinal_Type) 
    fprintf (context->file, " (:primitive :long-cardinal)");
  else if (t == cardinal_Type) 
    fprintf (context->file, " (:primitive :cardinal)");
  else if (t == boolean_Type) 
    fprintf (context->file, " (:primitive :boolean)");
  else if (t == pickle_Type) 
    fprintf (context->file, " (:primitive :pickle)");
  else if (t == real_Type) 
    fprintf (context->file, " (:primitive :real)");
  else if (t == shortreal_Type)
    fprintf (context->file, " (:primitive :short-real)");
  else if (t == longreal_Type)
    fprintf (context->file, " (:primitive :long-real)");
  else
    fprintf (context->file, " (%s %s)",
	     (t == object_Type) ? ":object" : ":constructed",
	     lisp_type_name(ur_type(type)));
}

typedef struct unioncontext_s {
  Context c;
  Type t;
  Argument default_arm;
  cardinal index;
  cardinal default_arm_index;
} *UnionContext;

static void OutputUnionArmValue (ConstantValue val, Context context, boolean kernel)
{
  putc(' ', context->file);
  switch (val->type)
    {
    case integer_Type:
    case shortinteger_Type:
    case cardinal_Type:
    case shortcardinal_Type:
    case byte_Type:
      fprintf (context->file, "%s%ld",
	       (val->val.i.sign < 0) ? "-" : "", val->val.i.value);
      break;
    case shortcharacter_Type:
      {
	if (kernel) {
	  fprintf (context->file, "\"%s\"", val->val.s);
	} else {
	  char *fixed_up = lisp_string(val->val.s);
	  fprintf (context->file, "%s", fixed_up);
	  iluparser_Free(fixed_up);
	}
      }
      break;
    case boolean_Type:
      fprintf (context->file, "%s", val->val.b ? "cl:t" : "cl:nil");
      break;
    default:
      fprintf (stderr, "illegal discriminator value\n");
      return;
    }
}

static void OutputUnionArmValue1 (ConstantValue val, Context context)
{
  OutputUnionArmValue (val, context, 0);
}

static void OutputUnionArmValue2 (ConstantValue val, Context context)
{
  OutputUnionArmValue (val, context, 1);
}

static void OutputUnionArm (Argument arm, UnionContext s)
{
  char *arm_name;

  if (arm != s->default_arm)
    {
      arm_name = argument_name(arm);
      fprintf (s->c->file, " ((");
      list_enumerate (arm->values,
		      (iluparser_EnumProc) OutputUnionArmValue1, s->c);
      fprintf (s->c->file, ")");
      OutputTypeName (arm->type, s->c);
      fprintf (s->c->file, " %s%s%s \"%s\" (",
	       (arm_name != NULL) ? "\"" : "",
	       (arm_name != NULL) ? arm_name : "nil",
	       (arm_name != NULL) ? "\"" : "",
	       type_uid(ur_type(arm->type)));
      list_enumerate (arm->values,
		      (iluparser_EnumProc) OutputUnionArmValue2, s->c);
      fprintf (s->c->file, "))");
    }
}

static void findDefaultArm (Argument a, UnionContext s)
{
  if (a == s->default_arm)
    s->default_arm_index = s->index;
  s->index += 1;
}

static cardinal figureDefaultArmIndex (UnionContext s)
{
  s->index = 1;
  s->default_arm_index = 0;
  list_enumerate (type_description(s->t)->structuredDes.uniond.types,
		  (iluparser_EnumProc) findDefaultArm, s);
  return s->default_arm_index;
}

static void OutputEnumerationField (EnumField field, Context context)
{
  char buf[100];

  fprintf (context->file, " (%s %d \"%s\")\n", lisp_string(field->name),
	   (field->id < 0) ? -1 : field->id, field->name);
}

static void OutputFieldDescription (Argument field, Context context)
{
  fprintf (context->file, " (%s", lisp_argument_name(field));
  OutputTypeName (field->type, context);
  fprintf (context->file, " \"%s\" \"%s\")\n",
	   argument_name(field), type_uid(field->type));
}

static void OutputDimension (long int d, Context context)
{
  fprintf (context->file, " %lu", d);
}

static void GenerateTypeStubs (Type type, Context context)
{
  enum PrimitiveTypes t = type_kind(type);
  TypeDescription d = type_description(type);

  if (type->builtIn || t == invalid_Type || t == void_Type || type->importInterfaceName != NULL)
    return;

  if (strncmp(OMGIDLExceptionTypePrefix, name_base_name(type->name), strlen(OMGIDLExceptionTypePrefix)) == 0)
    return;

  switch (t)
    {
    case byte_Type:
    case shortinteger_Type:
    case shortcharacter_Type:
    case longinteger_Type:
    case character_Type:
    case integer_Type:
    case shortcardinal_Type:
    case longcardinal_Type:
    case cardinal_Type:
    case boolean_Type:
    case pickle_Type:
    case real_Type:
    case shortreal_Type:
    case longreal_Type:
      /* no methods defined on these types, no output needed here. */
      break;

    case alias_Type:
      {
	fprintf (context->file, "(ilu:define-alias %s \"%s\"",
		 lisp_type_name(type), type_uid(type));
	OutputTypeName (ur_type(type), context);
	fprintf (context->file, " \"%s\" \"%s\" %s%s%s \"%s\")\n",
		 type_name(type), interface_name(type->interface),
		 (type->interface->brand == NULL) ? "" : "\"",
		 (type->interface->brand == NULL) ? "nil" : type->interface->brand,
		 (type->interface->brand == NULL) ? "" : "\"",
		 type_uid(ur_type(type)));
      }
      break;

    case union_Type:
      {
	struct unioncontext_s s;
	int disc_type_kind;

	switch (type_ur_kind(d->structuredDes.uniond.discriminator_type)) {
	  /* this list of values must be in sync with
	     ILUSRC/runtime/kernel/iluxport.h:ilu_TypeKind */
	case byte_Type:
	  disc_type_kind = 0;
	  break;
	case boolean_Type:
	  disc_type_kind = 1;
	  break;
	case shortinteger_Type:
	  disc_type_kind = 4;
	  break;
	case integer_Type:
	  disc_type_kind = 5;
	  break;
	case shortcardinal_Type:
	  disc_type_kind = 7;
	  break;
	case cardinal_Type:
	  disc_type_kind = 8;
	  break;
	case enumeration_Type:
	  disc_type_kind = 21;
	  break;
	default:
	  fprintf(stderr, "Bad union discriminant type %s for union type %s encountered\n",
		  type_name(d->structuredDes.uniond.discriminator_type),
		  type_name(type));
	  exit(1);
	};

	fprintf (context->file, "(ilu:define-union %s \"%s\"",
		 lisp_type_name(type), type_uid(type));
	OutputTypeName (d->structuredDes.uniond.discriminator_type, context);
	fprintf (context->file, " %s",
		 d->structuredDes.uniond.others_allowed ? "t" : "nil");
	s.default_arm = d->structuredDes.uniond.default_arm;
	if (s.default_arm != NULL)
	  OutputTypeName (s.default_arm->type, context);
	else
	  fprintf (context->file, " nil");
	s.t = type;
	s.c = context;
	fprintf (context->file, " \"%s\" \"%s\" %s%s%s \"%s\" %u %u",
		 type_name(type), interface_name(type->interface),
		 (type->interface->brand == NULL) ? "" : "\"",
		 (type->interface->brand == NULL) ? "nil" : type->interface->brand,
		 (type->interface->brand == NULL) ? "" : "\"",
		 type_uid(ur_type(d->structuredDes.uniond.discriminator_type)),
		 disc_type_kind, (s.default_arm != NULL) ? figureDefaultArmIndex(&s) : 0);
	list_enumerate (d->structuredDes.uniond.types, (EnumProc) OutputUnionArm, &s);
	fprintf (context->file, ")\n");
      }
      break;

    case sequence_Type:
      fprintf (context->file, "(ilu:define-sequence %s \"%s\"", lisp_type_name(type), type_uid(type));
      OutputTypeName (d->structuredDes.sequence.type, context);
      fprintf (context->file, " %lu", d->structuredDes.sequence.limit);
      fprintf (context->file, " \"%s\" \"%s\" %s%s%s \"%s\")\n",
	       type_name(type), interface_name(type->interface),
	       (type->interface->brand == NULL) ? "" : "\"",
	       (type->interface->brand == NULL) ? "nil" : type->interface->brand,
	       (type->interface->brand == NULL) ? "" : "\"",
	       type_uid(d->structuredDes.sequence.type));
      break;

    case optional_Type:
      fprintf (context->file, "(ilu:define-optional %s \"%s\"", lisp_type_name(type), type_uid(type));
      OutputTypeName (d->structuredDes.optional, context);
      fprintf (context->file, " \"%s\" \"%s\" %s%s%s \"%s\")\n",
	       type_name(type), interface_name(type->interface),
	       (type->interface->brand == NULL) ? "" : "\"",
	       (type->interface->brand == NULL) ? "nil" : type->interface->brand,
	       (type->interface->brand == NULL) ? "" : "\"",
	       type_uid(d->structuredDes.optional));
      break;

    case record_Type:
      fprintf (context->file, "(ilu:define-record %s \"%s\"\n", lisp_type_name(type), type_uid(type));
      fprintf (context->file, " \"%s\" \"%s\" %s%s%s %s %s%s%s ",
	       type_name(type), interface_name(type->interface),
	       (type->interface->brand == NULL) ? "" : "\"",
	       (type->interface->brand == NULL) ? "nil" : type->interface->brand,
	       (type->interface->brand == NULL) ? "" : "\"",
	       d->structuredDes.record.extensible ? "t" : "nil",
	       (d->structuredDes.record.supertype == NULL) ? "" : "\"",
	       (d->structuredDes.record.supertype == NULL) ? "nil" : type_uid(d->structuredDes.record.supertype),
	       (d->structuredDes.record.supertype == NULL) ? "" : "\"");
      list_enumerate (d->structuredDes.record.fields, (EnumProc) OutputFieldDescription, context);
      fprintf (context->file, " )\n");
      break;

    case array_Type:
      fprintf (context->file, "(ilu:define-array %s \"%s\"", lisp_type_name(type), type_uid(type));
      OutputTypeName (d->structuredDes.array.type, context);
      fprintf (context->file, " \"%s\" \"%s\" %s%s%s \"%s\" ",
	       type_name(type), interface_name(type->interface),
	       (type->interface->brand == NULL) ? "" : "\"",
	       (type->interface->brand == NULL) ? "nil" : type->interface->brand,
	       (type->interface->brand == NULL) ? "" : "\"",
	       type_uid(d->structuredDes.array.type));
      list_enumerate (d->structuredDes.array.dimensions, (EnumProc) OutputDimension, context);
      fprintf (context->file, ")\n");
      break;

    case enumeration_Type:
      fprintf (context->file, "(ilu:define-enumeration %s \"%s\"\n", lisp_type_name(type), type_uid(type));
      fprintf (context->file, " \"%s\" \"%s\" %s%s%s ",
	       type_name(type), interface_name(type->interface),
	       (type->interface->brand == NULL) ? "" : "\"",
	       (type->interface->brand == NULL) ? "nil" : type->interface->brand,
	       (type->interface->brand == NULL) ? "" : "\"");
      list_enumerate (d->structuredDes.enumeration, (EnumProc) OutputEnumerationField, context);
      fprintf (context->file, " )\n");
      break;      

    case object_Type:
      GenerateClassCode (type, context);
      break;

    default:
      break;
    };
  fprintf (context->file, "\n");
}

static void GenerateTypeDefinition (Type type, Context context)
{
  enum PrimitiveTypes t = type_kind(type);
  TypeDescription d = type_description(type);

  if (type->builtIn || t == invalid_Type || t == void_Type || type->importInterfaceName != NULL)
    return;

  if (strncmp(OMGIDLExceptionTypePrefix, name_base_name(type->name), strlen(OMGIDLExceptionTypePrefix)) == 0)
    return;

  switch (t)
    {
    case byte_Type:
    case shortinteger_Type:
    case shortcharacter_Type:
    case longinteger_Type:
    case character_Type:
    case integer_Type:
    case shortcardinal_Type:
    case longcardinal_Type:
    case cardinal_Type:
    case boolean_Type:
    case pickle_Type:
    case real_Type:
    case shortreal_Type:
    case longreal_Type:
      fprintf (context->file, "(ilu:define-primitive-type %s ", lisp_type_name(type));
      OutputTypeName (type, context);
      fprintf (context->file, ")\n");
      break;

    case alias_Type:
      fprintf (context->file, "(ilu:define-alias-type %s ", lisp_type_name(type));
      OutputTypeName (ur_type(type), context);
      fprintf (context->file, ")\n");
      break;

    case union_Type:
      {
	struct unioncontext_s s;

	fprintf (context->file, "(ilu:define-union-type %s",
		 lisp_type_name(type));
	OutputTypeName (d->structuredDes.uniond.discriminator_type, context);
	fprintf (context->file, " %s",
		 d->structuredDes.uniond.others_allowed ? "t" : "nil");
	s.default_arm = d->structuredDes.uniond.default_arm;
	if (s.default_arm != NULL)
	  OutputTypeName (s.default_arm->type, context);
	else
	  fprintf (context->file, " nil");
	s.t = type;
	s.c = context;
	list_enumerate (d->structuredDes.uniond.types, (EnumProc) OutputUnionArm, &s);
	fprintf (context->file, ")\n");
      }
      break;

    case sequence_Type:
      fprintf (context->file, "(ilu:define-sequence-type %s", lisp_type_name(type));
      OutputTypeName (d->structuredDes.sequence.type, context);
      fprintf (context->file, " %lu)\n", d->structuredDes.sequence.limit);
      break;

    case optional_Type:
      fprintf (context->file, "(ilu:define-optional-type %s ", lisp_type_name(type));
      OutputTypeName (d->structuredDes.optional, context);
      fprintf (context->file, ")\n");
      break;

    case record_Type:
      fprintf (context->file, "(ilu:define-record-type %s\n", lisp_type_name(type));
      list_enumerate (d->structuredDes.record.fields, (EnumProc) OutputFieldDescription, context);
      fprintf (context->file, " )\n");
      break;

    case array_Type:
      fprintf (context->file, "(ilu:define-array-type %s", lisp_type_name(type));
      OutputTypeName (d->structuredDes.array.type, context);
      list_enumerate (d->structuredDes.array.dimensions, (EnumProc) OutputDimension, context);
      fprintf (context->file, ")\n");
      break;

    case enumeration_Type:
      fprintf (context->file, "(ilu:define-enumeration-type %s\n", lisp_type_name(type));
      list_enumerate (d->structuredDes.enumeration, (EnumProc) OutputEnumerationField, context);
      fprintf (context->file, " )\n");
      break;      

    case object_Type:
      GenerateClassDefinition (type, context);
      break;      

    default:
      break;
    };
  fprintf (context->file, "\n");
}

static void listRecordField (Argument field, Context context)
{
  fprintf (context->file, " (%s ", lisp_argument_name(field));
  OutputTypeName(field->type, context);
  fprintf (context->file, ")");
}

static void GenerateExceptionDef (Exception e, Context context)
{
  if (e->builtIn || e->importInterfaceName != NULL)
    return;

  if (e->corba_rep_id != NULL)
    fprintf (context->file, "(ilu:define-exception-type %s \"%s\" (",
	     lisp_exception_name(e), e->corba_rep_id);
  else
    fprintf (context->file, "(ilu:define-exception-type %s \"ilu:%s.%s\" (",
	     lisp_exception_name(e), interface_name(e->interface), exception_name(e));
  if (e->type == NULL)
    ;
  else if (strncmp(OMGIDLExceptionTypePrefix, name_base_name(e->type->name), strlen(OMGIDLExceptionTypePrefix)) != 0) {
    fprintf (context->file, " (value ");
    OutputTypeName (e->type, context);
    fprintf (context->file, ")");
  } else {	/* CORBA OMG IDL exception */
    Type ut = ur_type(e->type);
    if (type_kind(ut) != record_Type) {
      fprintf (stderr, "Exception %s has non-record type!\n", name_base_name(e->type->name));
      exit(1);
    }
    list_enumerate(type_description(ut)->structuredDes.record.fields, (iluparser_EnumProc) listRecordField, context);
  }
  fprintf (context->file, ")");

  if (e->doc_string == NULL)
    fprintf (context->file, " nil)\n\n");
  else
    fprintf (context->file, " \"%s\")\n\n", e->doc_string);
}

static void GenerateExceptionCode2 (Exception e, Context context)
{
  if (e->builtIn || e->importInterfaceName != NULL)
    return;

  fprintf (context->file, "(ilu:define-exception %s (", lisp_exception_name(e));
  if (e->type == NULL)
    ;
  else if (strncmp(OMGIDLExceptionTypePrefix, name_base_name(e->type->name), strlen(OMGIDLExceptionTypePrefix)) != 0) {
    fprintf (context->file, " (value ");
    OutputTypeName (e->type, context);
    fprintf (context->file, ")");
  } else {	/* CORBA OMG IDL exception */
    Type ut = ur_type(e->type);
    if (type_kind(ut) != record_Type) {
      fprintf (stderr, "Exception type %s has non-record type!\n", name_base_name(e->type->name));
      exit(1);
    }
    list_enumerate(type_description(ut)->structuredDes.record.fields, (iluparser_EnumProc) listRecordField, context);
  }
  fprintf (context->file, ")");
  fprintf (context->file, ")\n\n");
}

void GenerateTypeDefinitions (Interface interface, FILE *file)
{
  struct context_s context;

  context.file = file;
  context.interface = interface;

  list_enumerate (interface->types, (EnumProc) GenerateTypeDefinition, &context);
}

void GenerateTypeCode (Interface interface, FILE *file)
{
  struct context_s context;

  context.file = file;
  context.interface = interface;

  list_enumerate (interface->types, (EnumProc) GenerateTypeStubs, &context);
}

void GenerateExceptionDefinitions (Interface interface, FILE *file)
{
  struct context_s context;

  context.file = file;
  context.interface = interface;

  list_enumerate (interface->exceptions, (EnumProc) GenerateExceptionDef, &context);
}

void GenerateExceptionCode (Interface interface, FILE *file)
{
  struct context_s context;

  context.file = file;
  context.interface = interface;

  list_enumerate (interface->exceptions, (EnumProc) GenerateExceptionCode2, &context);
}

static void convertStringForm (char *buf, string str)
{
  register char *p;

  strcpy (buf, "#.(ilu::ascii-to-string #(");
  for (p = str;  *p != 0;  p++)
    sprintf (buf + strlen(buf), " #x%x", (unsigned char) *p);
  strcpy (buf + strlen(buf), "))");
}

static void GenerateConstant (Constant c, Context context)
{
  enum PrimitiveTypes t = type_ur_kind (c->type);

  if (c->interface != context->interface)
    return;

  fprintf(context->file, "(common-lisp:defconstant %s ", lisp_constant_name(c));

  if ((t == cardinal_Type || t == shortcardinal_Type || t == byte_Type)
      && (c->value->type == integer_Type))
    fprintf (context->file, "%lu)\n", c->value->val.i.value);
  else if ((t == integer_Type || t == shortinteger_Type)
	   && (c->value->type == integer_Type))
    fprintf (context->file, "%s%lu)\n",
	     (c->value->val.i.sign < 0) ? "-" : "", c->value->val.i.value);
  else if ((t == real_Type || t == shortreal_Type)
	   && (c->value->type == real_Type))
    fprintf (context->file, "%s%s.%s%s%ld)\n",
	     (c->value->val.r.sign < 0) ? "-" : "",
	     c->value->val.r.value,
	     (c->value->val.r.fraction == NULL) ? "0" : c->value->val.r.fraction,
	     (t == real_Type) ? "D" : "F",
	     c->value->val.r.exponent);
  else if (t == shortcharacter_Type && (c->value->type == integer_Type))
    fprintf (context->file, "#.(common-lisp:code-char #x%lx))\n",
	     c->value->val.i.value);
  else if (t == sequence_Type AND
	   type_ur_kind(type_description(c->type)->structuredDes.sequence.type)
	     == shortcharacter_Type AND
	   c->value->type == shortcharacter_Type)
    {
      char *buf;

      buf = iluparser_Malloc(100 + 4 * strlen(c->value->val.s));
      convertStringForm (buf, c->value->val.s);
      fprintf (context->file, "%s)\n", buf);
      iluparser_Free(buf);
    }
  else if (t == boolean_Type AND c->value->type == boolean_Type)
    {
      fprintf (context->file, "%s)\n", c->value->val.b ? "t" : "nil");
    }
  else
    {
      fprintf (stderr,
	       "Invalid constant, %s, encountered.  Type is %s, type of value is %d.\n",
	       name_base_name (c->name), lisp_type_name(c->type),
	       c->value->type);
    }
}

void GenerateConstants (Interface interface, FILE *file)
{
  struct context_s context;

  context.file = file;
  context.interface = interface;

  list_enumerate (interface->constants, (EnumProc) GenerateConstant, &context);
}
