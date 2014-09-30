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

#include <stdlib.h>	/* for exit() */
#include <ctype.h>	/* for isgraph() */

#include "scheme.h"


static void typedef_status_block (Context c)
{
#if 0
  fprintf (c->file, "typedef struct _%s_Status_struct %sStatus;\n\n",
	   scheme_interface_name(c->interface), scheme_interface_name(c->interface));
#endif
}

static boolean MatchPointer (refany p1, refany p2)
{
  return (p1 == p2);
}

static void sort_types_for_declaration (Type type, list sorted);

static void SortArgTypes (Argument arg, list sorted)
{
  sort_types_for_declaration (arg->type, sorted);  
}

static void SortMethodArgTypes (Argument arg, list sorted)
{
  if (!PassedByRef(arg->type))
    sort_types_for_declaration (arg->type, sorted);  
}

static void SortMethodTypes (Procedure m, list sorted)
{
  TypeKind tk = type_kind(m->returnType);

  if (!m->returnOptional &&
      !(TypeIsNonObjectStruct(ur_type(m->returnType)) ||
	tk  == sequence_Type || tk == object_Type))
    sort_types_for_declaration (m->returnType, sorted);
  list_enumerate (m->arguments, (iluparser_EnumProc) SortMethodArgTypes, sorted);
}

static void sort_types_for_declaration (Type type, list sorted)
{
  enum PrimitiveTypes t;
  static list pending = NULL;

  if (type == NULL || type->importInterfaceName != NULL)
    return;
  if (pending == NULL)
    pending = new_list();
  if (list_find (sorted, MatchPointer, type) != NULL)
    return;
  if (list_find (pending, MatchPointer, type) != NULL)
    {
      list_insert (sorted, type);
      list_remove (pending, type);
      return;
    }

  t = type_basic_type(type);
  list_insert (pending, type);

  if (type->importInterfaceName == NULL &&
      (t == record_Type ||
       t == array_Type ||
       t == union_Type ||
       t == optional_Type ||
       t == sequence_Type ||
       t == alias_Type ||
       t == object_Type))
    {
      switch (t)
	{
	case record_Type:
	      
	  list_enumerate(type_description(type)->structuredDes.record.fields, (iluparser_EnumProc) SortArgTypes, sorted);
	  break;
	      
	case union_Type:
	      
	  list_enumerate(type_description(type)->structuredDes.uniond.types, (iluparser_EnumProc) SortArgTypes, sorted);
	  break;
	      
	case optional_Type:
	  sort_types_for_declaration (type_description(type)->structuredDes.optional, sorted);
	  break;

	case array_Type:
	      
	  if (!type_description(type)->structuredDes.array.optional)
	    sort_types_for_declaration (type_description(type)->structuredDes.array.type, sorted);
	  break;

	case sequence_Type:

	  sort_types_for_declaration (type_description(type)->structuredDes.sequence.type, sorted);
	  break;

	case alias_Type:
	  sort_types_for_declaration (type->supertype, sorted);
	  break;

	case object_Type:

	  if (type->marked)
	    break;
	  list_enumerate(class_object(type)->superclasses, (iluparser_EnumProc) sort_types_for_declaration, sorted);
	  list_enumerate(class_object(type)->methods, (iluparser_EnumProc) SortMethodTypes, sorted);
	  type->marked = TRUE;
	  break;

	default:
	  break;
	}
    }
  if (list_find(sorted, MatchPointer, type) == NULL)
    list_insert(sorted, type);
  list_remove (pending, type);
}


/*************************************************************************/
static cardinal fieldNum = 0;
static char* recordName = 0;

static void GenerateRecordField (Argument a, Context context)
{
  fprintf (context->file, "(%s #f)\n", scheme_simple_name(a->name));
}

static void GenerateRecordOps (Argument a, Context context)
{
  char* name = scheme_simple_name(a->name);
  fprintf (context->file, "(if (not (defined? 'get-%s))\n", name);
  fprintf (context->file, "  (ilu-define-operation (get-%s this)))\n", name);
  fprintf (context->file, "(if (not (defined? 'set-%s))\n", name);
  fprintf (context->file, "  (ilu-define-operation (set-%s this . val)))\n", name);
  fieldNum++;
}

static void GenerateRecordFieldInit (Argument a, Context context)
{
  char* name = scheme_simple_name(a->name);
  fprintf (context->file, "  ((get-%s this) %s)\n", name, name);
  fprintf (context->file, "  ((set-%s this . val) (set! %s (car val)))\n", name, name);
  fieldNum++;
}

static void GenerateRecordDeclaration (Type t, Context context)
{
  TypeDescription td = type_description(t);

  char* tname = scheme_type_name(t);
  list_enumerate (td->structuredDes.record.fields, (iluparser_EnumProc) GenerateRecordOps, context);
  fprintf (context->file, "(define (make-%s)\n", tname);
  fprintf (context->file, "  (let (");
  list_enumerate (td->structuredDes.record.fields, (iluparser_EnumProc) GenerateRecordField, context);
  fprintf (context->file, ")\n");
  fprintf (context->file, "    (ilu-object");
  recordName = tname;
  fieldNum = 0;
  list_enumerate (td->structuredDes.record.fields, (iluparser_EnumProc) GenerateRecordFieldInit, context);
  fprintf (context->file, "     )))\n");
}

/*************************************************************************/

/* is this enough??? */
static void GenerateUnionDeclaration (Type t, Context context)
{
  fprintf (context->file, "(define (%s disc val)\n", scheme_type_name(t));
  fprintf (context->file, "  (cons disc val))\n\n");
  fprintf (context->file, "(define (%s-discriminator u)\n", scheme_type_name(t));
  fprintf (context->file, "  (car u))\n");
  fprintf (context->file, "(define (%s-value u)\n", scheme_type_name(t));
  fprintf (context->file, "  (cdr u))\n\n");
}

/*************************************************************************/

static int enumFieldId = -1;

static void PrintEnumField (EnumField e, Context context)
{
  if (e->id >= 0)
    enumFieldId = e->id;
  else
    enumFieldId++;

  fprintf (context->file, "(define %s:%s %d)\n", scheme_type_name(context->class),
	   scheme_string(e->name), enumFieldId);
}

static void GenerateEnumerationTypedefDeclaration (Type t, Context context)
{
  list e = (type_description (t))->structuredDes.enumeration;
  context->class = t;
  enumFieldId = -1;
  list_enumerate (e, (iluparser_EnumProc) PrintEnumField, context);
  fprintf(context->file, "\n");
}

/*************************************************************************/

void GenerateSequenceDeclaration (Type seq, FILE *file)
{
}

void declare_type (Type t, Context context)
{
/*
  printf ("%s %d%s  file is 0x%x\n", type_name(t), list_size(t->refs),
	  t->builtIn ? " builtin" : "", context->file);
*/
  if (t->builtIn)
    return;

  if (t->importInterfaceName != NULL)
    return;

  switch (type_basic_type(t))
    {
    case void_Type:
    case integer_Type:
    case cardinal_Type:
    case shortinteger_Type:
    case shortcardinal_Type:
    case longinteger_Type:
    case longcardinal_Type:
    case character_Type:
    case shortcharacter_Type:
    case real_Type:
    case shortreal_Type:
    case longreal_Type:
    case boolean_Type:
    case byte_Type:
    case array_Type:
    case alias_Type:
      break;

    case object_Type:
      /* see declare-object.c */
      break;

    case record_Type:
      GenerateRecordDeclaration (t, context);
      break;

    case union_Type:
      GenerateUnionDeclaration (t, context);
      break;

    case sequence_Type:
      GenerateSequenceDeclaration (t, context->file);
      break;

    case enumeration_Type:
      /* already done in typedef */
      break;

    case optional_Type:
      /* already done */
      break;

    default:
      fprintf (stderr, "Error:  Can't cope with declaration of type %s yet.\n", scheme_type_name(t));
      SEGFAULT;
    }
}

static void GenerateUnionTypedef (Type t, Context context)
{
#if 0
  fprintf (context->file, "typedef struct _%s_union %s;\n",
	   scheme_type_name(t), scheme_type_name(t));
#endif
}

static void OutputDim (long int d, Context context)
{
  fprintf (context->file, " %lu", d);
}

/* should add initializer data! */
static void GenerateArrayTypedef (Type t, Context context)
{
  TypeDescription d = type_description(t);
  char* tname = scheme_type_name(t);
  fprintf (context->file, "(define (%s) (ilu:input-array-rec (lambda () #f) (list", tname);
  list_enumerate (d->structuredDes.array.dimensions, (iluparser_EnumProc) OutputDim, context);
  fprintf (context->file, ")))\n\n");
}

/* semantics of sequence create is wrong!!!!!!!!! */
/* should make data an optional argument */
static void GenerateSequenceTypedef (Type t, Context context)
{
  TypeDescription d = type_description(t);
  if (type_basic_type(ur_type(d->structuredDes.sequence.type)) == shortcharacter_Type) {
    fprintf (context->file, "(define (%s data) (map string data))\n\n", scheme_type_name(t));
  } else {
    fprintf (context->file, "(define (%s data)\n", scheme_type_name(t));
    fprintf (context->file, "  (ilu:input-sequence-rec\n");
    fprintf (context->file, "    (lambda ()\n");
    fprintf (context->file, "      (let ((tmp (if (and data (not (null? data))) (car data) #f))\n");
    fprintf (context->file, "            (rest (if (and data (not (null? data))) (cdr data) #f)))\n");
    fprintf (context->file, "        (set! data rest)\n");
    fprintf (context->file, "        tmp))\n");
    fprintf (context->file, "    (list (length data))))\n\n");
  }
}

static void GenerateRecordTypedef (Type t, Context context)
{
#if 0
  TypeDescription type_description();
  fprintf (context->file, "typedef struct _%s_record %s;\n", scheme_type_name(t), scheme_type_name(t));
#endif
}

void typedef_type (Type t, Context context)
{
  if (t->builtIn)
    return;

  if (t->importInterfaceName != NULL)
    return;

  switch (type_basic_type(t))
    {
    case void_Type:
      break;

    case integer_Type:
      /* fprintf (context->file, "typedef ilu_Integer %s;\n", scheme_type_name(t));*/
      break;

    case cardinal_Type:
      /* fprintf (context->file, "typedef ilu_Cardinal %s;\n", scheme_type_name(t));*/
      break;

    case shortinteger_Type:
      /* fprintf (context->file, "typedef ilu_ShortInteger %s;\n", scheme_type_name(t));*/
      break;

    case shortcardinal_Type:
      /* fprintf (context->file, "typedef ilu_ShortCardinal %s;\n", scheme_type_name(t));*/
      break;

    case longinteger_Type:
      /* fprintf (context->file, "typedef ilu_LongInteger %s;\n", scheme_type_name(t));*/
      break;

    case longcardinal_Type:
      /* fprintf (context->file, "typedef ilu_LongCardinal %s;\n", scheme_type_name(t));*/
      break;

    case real_Type:
      /* fprintf (context->file, "typedef ilu_Real %s;\n", scheme_type_name(t));*/
      break;

    case shortreal_Type:
      /* fprintf (context->file, "typedef ilu_ShortReal %s;\n", scheme_type_name(t));*/
      break;

    case longreal_Type:
      /* fprintf (context->file, "typedef ilu_LongReal %s;\n", scheme_type_name(t));*/
      break;

    case boolean_Type:
      /* fprintf (context->file, "typedef ilu_Boolean %s;\n", scheme_type_name(t));*/
      break;

    case byte_Type:
      /* fprintf (context->file, "typedef ilu_Byte %s;\n", scheme_type_name(t));*/
      break;

    case character_Type:
      /* fprintf (context->file, "typedef ilu_Character %s;\n", scheme_type_name(t));*/
      break;

    case shortcharacter_Type:
      /* fprintf (context->file, "typedef ilu_ShortCharacter %s;\n", scheme_type_name(t));*/
      break;

    case alias_Type:
      if(type_ur_kind(t) == object_Type || type_ur_kind(t) == record_Type) {
	fprintf (context->file, "(define make-%s make-%s) ;; typedef\n",
		 scheme_type_name(t), scheme_type_name(ur_type(t)));
      } else {
/*
	fprintf (context->file, "(define %s %s) ;; typedef\n",
		 scheme_type_name(t), scheme_type_name(ur_type(t)));
*/
      }
      break;

    case optional_Type:
      /*
	fprintf (context->file, "typedef %s %s %s;\n", scheme_return_type(type_description(t)->structuredDes.optional),
	PassedByRef(type_description(t)->structuredDes.optional) ? "" : "*",
	scheme_type_name(t));
      */
      break;

    case array_Type:
      GenerateArrayTypedef (t, context);
      break;

    case record_Type:
      GenerateRecordTypedef (t, context);
      break;

    case object_Type:
      /* fprintf (context->file, "class %s;\n", scheme_type_name(t));*/
      break;

    case union_Type:
      GenerateUnionTypedef (t, context);
      break;

    case sequence_Type:
      GenerateSequenceTypedef (t, context);
      break;

    case enumeration_Type:
      GenerateEnumerationTypedefDeclaration (t, context);
      break;

    default:
      fprintf (stderr, "Error:  Can't cope with typedef of type %s yet.\n", scheme_type_name(t));
      exit(1);
    }
}

#if 0
static void GenerateInputOutputSizeDeclaration (Type type, Context context)
{
  Type ut = ur_type(type);
  enum PrimitiveTypes t = type_basic_type(ur_type(type));

  if (type->importInterfaceName == NULL &&
      (    TypeIsNonObjectStruct(ut)
       || (TypeIsArray(ut)
	   && (((type_ur_kind(type_description(type)->structuredDes.array.type) != byte_Type)
		 && (type_ur_kind(type_description(type)->structuredDes.array.type) != shortcharacter_Type))
		||
		list_size(type_description(type)->structuredDes.array.dimensions) > 1))
       || (t == sequence_Type
	   && (type_ur_kind(type_description(type)->structuredDes.sequence.type) != shortcharacter_Type))))
    {
      char *paramType = scheme_parameter_type(type, In);
      char *refType = t == array_Type ? paramType : scheme_return_type(type);

      fprintf (context->file, "  static ilu_Boolean Output_%s (iluCall call, %s val);\n",
	       scheme_simple_name(type->name), paramType);
      fprintf (context->file, "  static %s Input_%s (iluCall call, %s ref);\n",
	       scheme_return_type(type), scheme_simple_name(type->name), refType);
      fprintf (context->file, "  static ilu_Cardinal SizeOf_%s (iluCall call, %s val);\n\n",
	       scheme_simple_name(type->name), paramType);
    }
  if (HasFreeRoutine(type))
    fprintf (context->file, "  static void Free_%s (%s val);\n",
	     scheme_simple_name(type->name),
	     scheme_parameter_type(type, In));
}

static void generate_interface_wide_class (Interface i, Context c)
{
  char *name = (char *) scheme_interface_name(i);

  fprintf (c->file, "class %s_G {\n\n", name);
  fprintf (c->file, " public:\n\n");
  fprintf (c->file, "  static struct %s_Exceptions_s *Exceptions();\n", name);
  fprintf (c->file, "  static void RaiseException (%sStatus *status, ilu_Exception exception...);\n\n",
	   name);
  list_enumerate (i->types, (iluparser_EnumProc) GenerateInputOutputSizeDeclaration, c);

  fprintf (c->file, "/* Note:  SendException should only be used by generated code, not by user code. */\n");
  fprintf (c->file, "  static int SendException (iluCall call, %sStatus *status);\n", name);

  fprintf (c->file, "};\n\n");
}

static void declare_exception(Exception e, Context context)
{
  if (e->interface == context->interface
      && e->importInterfaceName == NULL)
    fprintf(context->file, "#define %s\t\t(%s_G::Exceptions()->%s)\n",
	    scheme_exception_name(e),
	    scheme_interface_name(context->interface),
	    scheme_simple_name(e->name));
}
#endif

static void PrintStringChar (FILE *file, int ch)
{
  if (ch == ' ')
    fprintf (file, " #\\space");
  else if (ch == '\n')
    fprintf (file, " #\\newline");
  else if(isgraph(ch))
    fprintf (file, " #\\%c", ch);
  else
    fprintf (file, " (integer->char %d)", ch);
}

static void PrintString (FILE *file, const string str)
{
  unsigned char *p = (unsigned char *) str;
  int ch;

  fprintf(file, "(string");
  while ((ch = *p++) != 0)
    PrintStringChar (file, ch);
  fprintf(file, ")");
}

static void declare_constant (Constant c, Context context)
{
  enum PrimitiveTypes t = type_ur_kind(c->type);

  if (c->interface != context->interface)
    return;

  fprintf(context->file, "(define %s ", scheme_constant_name(c));

  if ((t == cardinal_Type || t == shortcardinal_Type || t == byte_Type)
      && (c->value->type == integer_Type))
    {
      fprintf (context->file, "%lu)\n", c->value->val.i.value);
    }
  else if ((t == integer_Type || t == shortinteger_Type)
      && (c->value->type == integer_Type))
    {
      fprintf (context->file, "%s%lu)\n", (c->value->val.i.sign<0) ? "-" : "", c->value->val.i.value);
    }
  else if (t == shortcharacter_Type && c->value->type == integer_Type)
    {
      fprintf (context->file, "%s%lu)\n", (c->value->val.i.sign<0) ? "-" : "", c->value->val.i.value);
    }
  else if (t == shortcharacter_Type && c->value->type == shortcharacter_Type)
    {
      PrintStringChar(context->file, c->value->val.s[0]);
      fprintf (context->file, ")\n");
    }
  else if (t == shortreal_Type && c->value->type == integer_Type)
    {
      fprintf (context->file, "%s%lu.0s0)\n", (c->value->val.i.sign<0) ? "-":"", c->value->val.i.value);
    }
  else if (t == real_Type && c->value->type == integer_Type)
    {
      fprintf (context->file, "%s%lu.0d0)\n", (c->value->val.i.sign<0) ? "-":"", c->value->val.i.value);
    }
  else if (t == longreal_Type && c->value->type == integer_Type)
    {
      fprintf (context->file, "%s%lu.0l0)\n", (c->value->val.i.sign<0) ? "-":"", c->value->val.i.value);
    }
  else if (t == shortreal_Type)
    {
      fprintf (context->file, "%s%s.%ss%ld;\n",
	       (c->value->val.r.sign < 0) ? "-" : "",
	       c->value->val.r.value, (!c->value->val.r.fraction) ? "0" : c->value->val.r.fraction,
	       c->value->val.r.exponent);
    }
  else if (t == real_Type)
    {
      fprintf (context->file, "%s%s.%sd%ld;\n",
	       (c->value->val.r.sign < 0) ? "-" : "",
	       c->value->val.r.value, (!c->value->val.r.fraction) ? "0" : c->value->val.r.fraction,
	       c->value->val.r.exponent);
    }
  else if (t == longreal_Type)
    {
      fprintf (context->file, "%s%s.%sl%ld;\n",
	       (c->value->val.r.sign < 0) ? "-" : "",
	       c->value->val.r.value, (!c->value->val.r.fraction) ? "0" : c->value->val.r.fraction,
	       c->value->val.r.exponent);
    }
  else if (t == boolean_Type)
    {
      fprintf (context->file, "%s)\n", (c->value->val.b) ? "#t" : "#f");
    }
  else if (t == sequence_Type
	   && (type_ur_kind(type_description(c->type)->structuredDes.sequence.type) == shortcharacter_Type)
	   && c->value->type == shortcharacter_Type)
    {
      PrintString (context->file, c->value->val.s);
      fprintf (context->file, ")\n");
    }
  else
    fprintf (stderr, "Invalid constant, %s, encountered.\n", name_base_name(c->name));
}

void generate_headers (Interface interface, FILE *file)
{
  extern void generate_global_code (Interface, Context);	/* in code.c */
  struct context_s context;
  list sorted = new_list();

  context.file = file;
  context.interface = interface;
  context.class = NULL;

  GenerateNecessaryIncludes (&context);

  fprintf (file, "\n");

  typedef_status_block(&context);

  list_enumerate (interface->types, (iluparser_EnumProc) UnmarkSupertypes, NULL);
  list_enumerate (interface->types, (iluparser_EnumProc) sort_types_for_declaration, sorted);

  list_enumerate (sorted, (iluparser_EnumProc) declare_type, &context);
  fprintf (file, "\n");

  list_enumerate (interface->constants, (iluparser_EnumProc) declare_constant, &context);

  generate_global_code (interface, &context);

  list_enumerate (interface->classes, (iluparser_EnumProc) generate_class_code, &context);

  list_enumerate (sorted, (iluparser_EnumProc) typedef_type, &context);
  fprintf (file, "\n");

  /*generate_interface_wide_class (interface, &context);*/
  /*generate_exception_table (interface, &context);*/
  /*list_enumerate (interface->exceptions, (iluparser_EnumProc) declare_exception, &context);*/
}
