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
$Id: headers.c,v 1.54 1999/08/03 01:50:58 janssen Exp $
*/

#include <stdlib.h>

#include "cplusplus.h"


static void typedef_status_block (Context c)
{
  fprintf (c->file, "typedef struct _%s_Status_struct %sStatus;\n\n",
	   cplusplus_interface_name(c->interface), cplusplus_interface_name(c->interface));
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
      !(TypeIsNonObjectStruct(ur_type(m->returnType)) OR
	tk  == sequence_Type OR tk == object_Type))
    sort_types_for_declaration (m->returnType, sorted);
  list_enumerate (m->arguments, (iluparser_EnumProc) SortMethodArgTypes, sorted);
}

static void sort_types_for_declaration (Type type, list sorted)
{
  enum PrimitiveTypes t;
  static list pending = NULL;

  if (type == NULL OR type->importInterfaceName != NULL)
    return;
  if (pending == NULL)
    pending = new_list();
  if ((list_find (sorted, MatchPointer, type) != NULL) ||
      (list_find (pending, MatchPointer, type) != NULL))
    return;

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
	      
	  sort_types_for_declaration (type_description(type)->structuredDes.uniond.discriminator_type, sorted);
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

static void GenerateRecordField (Argument a, Context context)
{
  fprintf (context->file, "\t%s %s;\n",
	   (type_basic_type(ur_type(a->type)) == object_Type) ? cplusplus_return_type(a->type)
	      : cplusplus_type_name(a->type),
	   cplusplus_simple_name(a->name));
}

static void GenerateRecordDeclaration (Type t, Context context)
{
  fprintf (context->file, "struct _%s_record {\n", cplusplus_type_name(t));
  list_enumerate (type_description(t)->structuredDes.record.fields, (iluparser_EnumProc) GenerateRecordField, context);
  fprintf (context->file, "};\n");
}

static void GenerateUnionField (Argument a, Context context)
{
  char *name;

  if (a->name != NULL AND a->name->base_name != NULL)
    name = (char *) cplusplus_simple_name (a->name);
  else
    name = (char *) cplusplus_string (type_name(a->type));

  if (type_basic_type(ur_type(a->type)) == object_Type)
    fprintf (context->file, "\t\tclass %s *%s;\n", cplusplus_type_name(ur_type(a->type)), name);
  else
    fprintf (context->file, "\t\t%s %s;\n", cplusplus_type_name(a->type), name);
}

struct double_s {
  Type t;
  Context c;
  unsigned long id;
};

static void GenerateUnionDeclaration (Type t, Context context)
{
  Type d = type_description (t)->
    structuredDes.uniond.discriminator_type;
  list e = type_description(t)->structuredDes.uniond.types;

  fprintf (context->file, "struct _%s_union {\n", cplusplus_type_name(t));
  fprintf (context->file, "\t%s discriminator;\n\tunion {\n", cplusplus_type_name(d));
  list_enumerate (e, (iluparser_EnumProc) GenerateUnionField, context);
  fprintf (context->file, "\t} value;\n};\n");
}

static void PrintEnumField (EnumField e, Context context)
{
  fprintf (context->file, ", %s_%s", cplusplus_type_name(context->class), cplusplus_string(e->name));
  if (e->id >= 0)
    fprintf (context->file, " = %d", e->id);  
}

static void PrintFirstEnumField (EnumField e, Context context)
{
  fprintf (context->file, "%s_%s", cplusplus_type_name(context->class), cplusplus_string(e->name));
  if (e->id >= 0)
    fprintf (context->file, " = %d", e->id);  
}

static void GenerateEnumerationTypedefDeclaration (Type t, Context context)
{
  list e;
  EnumField ef;

  fprintf (context->file, "typedef enum _%s_enum {", cplusplus_type_name(t));
  e = (type_description (t))->structuredDes.enumeration;
  ef = (EnumField) list_car(e);
  context->class = t;
  PrintFirstEnumField (ef, context);
  if (list_size(e) > 1)
    list_enumerate (list_cdr(e), (iluparser_EnumProc) PrintEnumField, context);
  fprintf (context->file, "} %s;\n", cplusplus_type_name(t));
}

void GenerateSequenceDeclaration (Type seq, FILE *file)
{
  char *tn, *stp, *spn, *srt;
  Type sequenceType;

  if (type_basic_type(seq) != sequence_Type)
    return;

  sequenceType = type_description(seq)->structuredDes.sequence.type;

  if (type_ur_kind(sequenceType) == shortcharacter_Type)
    {
      /* no declaration, already handled in typedef */
    }

  else {
    
    tn = cplusplus_type_name(seq);
    spn = cplusplus_parameter_type(sequenceType, In);
    srt = cplusplus_return_type(sequenceType);
    stp = (type_ur_kind(sequenceType) == object_Type) ? spn : cplusplus_type_name(sequenceType);

    fprintf (file, "  class _%s_sequence {\n", tn);
    fprintf (file, "   private:\n");
    fprintf (file, "    ilu_Cardinal _maximum;\n");
    fprintf (file, "    ilu_Cardinal _length;\n");
    fprintf (file, "    %s *_buffer;\n", stp);
    fprintf (file, "   public:\n");
    fprintf (file, "    _%s_sequence ();\n", tn);
    fprintf (file, "    virtual ~_%s_sequence ();\n", tn);
    fprintf (file, "    static class _%s_sequence *Create (ilu_Cardinal initial_size, %s *initial_data);\n", tn, stp);
    fprintf (file, "    virtual void Clear(ilu_Boolean free_contents);\n");
    fprintf (file, "    virtual ilu_Cardinal Length();\n");
    fprintf (file, "    virtual void Append(%s);\n", spn);
    fprintf (file, "    virtual %s RemoveHead();\n", srt);
    fprintf (file, "    virtual %s RemoveTail();\n", srt);
    fprintf (file, "    virtual ilu_Cardinal RemoveAll(ilu_Boolean (*matchproc)(%s, void *), void *arg);\n", spn);
    fprintf (file, "    virtual %s Find(ilu_Boolean (*matchproc)(%s, void *), void *arg);\n", srt, spn);
    fprintf (file, "    virtual void Enumerate(void (*enumproc)(%s, void *), void *arg);\n", srt);
    fprintf (file, "    virtual %s * Array();\n", stp);
    fprintf (file, "    virtual %s Nth(ilu_Cardinal index);\n", srt);
    fprintf (file, "  };\n\n");
  }
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
      declare_object_type (t, context);		/* see declare-object.c */
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
      fprintf (stderr, "Error:  Can't cope with declaration of type %s yet.\n", cplusplus_type_name(t));
      SEGFAULT;
    }
}

static void GenerateUnionTypedef (Type t, Context context)
{
  fprintf (context->file, "typedef struct _%s_union %s;\n",
	   cplusplus_type_name(t), cplusplus_type_name(t));
}

static void OutputDim (long int d, Context context)
{
  fprintf (context->file, "[%lu]", d);
}

static void GenerateArrayTypedef (Type t, Context context)
{
  TypeDescription d = type_description(t);

  fprintf (context->file, "typedef %s %s", cplusplus_type_name(d->structuredDes.array.type),
	   cplusplus_type_name(t));
  list_enumerate (d->structuredDes.array.dimensions, (iluparser_EnumProc) OutputDim, context);
  fprintf (context->file, ";\n");
}

static void GenerateSequenceTypedef (Type t, Context context)
{
  TypeDescription d = type_description(t);

  if (type_basic_type(ur_type(d->structuredDes.sequence.type)) == shortcharacter_Type)
    fprintf (context->file, "typedef char * %s;\n", cplusplus_type_name(t));
  else
    fprintf (context->file, "typedef class _%s_sequence * %s;\n", cplusplus_type_name(t), cplusplus_type_name(t));
}

static void GenerateRecordTypedef (Type t, Context context)
{
  fprintf (context->file, "typedef struct _%s_record %s;\n", cplusplus_type_name(t), cplusplus_type_name(t));
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
      fprintf (context->file, "typedef ilu_Integer %s;\n", cplusplus_type_name(t));
      break;

    case cardinal_Type:
      fprintf (context->file, "typedef ilu_Cardinal %s;\n", cplusplus_type_name(t));
      break;

    case shortinteger_Type:
      fprintf (context->file, "typedef ilu_ShortInteger %s;\n", cplusplus_type_name(t));
      break;

    case shortcardinal_Type:
      fprintf (context->file, "typedef ilu_ShortCardinal %s;\n", cplusplus_type_name(t));
      break;

    case longinteger_Type:
      fprintf (context->file, "typedef ilu_LongInteger %s;\n", cplusplus_type_name(t));
      break;

    case longcardinal_Type:
      fprintf (context->file, "typedef ilu_LongCardinal %s;\n", cplusplus_type_name(t));
      break;

    case real_Type:
      fprintf (context->file, "typedef ilu_Real %s;\n", cplusplus_type_name(t));
      break;

    case shortreal_Type:
      fprintf (context->file, "typedef ilu_ShortReal %s;\n", cplusplus_type_name(t));
      break;

    case longreal_Type:
      fprintf (context->file, "typedef ilu_LongReal %s;\n", cplusplus_type_name(t));
      break;

    case boolean_Type:
      fprintf (context->file, "typedef ilu_Boolean %s;\n", cplusplus_type_name(t));
      break;

    case byte_Type:
      fprintf (context->file, "typedef ilu_Byte %s;\n", cplusplus_type_name(t));
      break;

    case character_Type:
      fprintf (context->file, "typedef ilu_Character %s;\n", cplusplus_type_name(t));
      break;

    case shortcharacter_Type:
      fprintf (context->file, "typedef ilu_ShortCharacter %s;\n", cplusplus_type_name(t));
      break;

    case alias_Type:
      fprintf (context->file, "typedef %s%s %s;\n",
	       (type_ur_kind(t) == object_Type) ? "class " : "",
	       cplusplus_type_name(ur_type(t)), cplusplus_type_name(t));
      fprintf (context->file, "#define %s %s\n",
	       cplusplus_type_name(t), cplusplus_type_name(ur_type(t)));
      break;

    case optional_Type:
      fprintf (context->file, "typedef %s %s %s;\n", cplusplus_return_type(type_description(t)->structuredDes.optional),
	       PassedByRef(type_description(t)->structuredDes.optional) ? "" : "*",
	       cplusplus_type_name(t));
      break;

    case array_Type:
      GenerateArrayTypedef (t, context);
      break;

    case record_Type:
      GenerateRecordTypedef (t, context);
      break;

    case object_Type:
      fprintf (context->file, "class %s;\n", cplusplus_type_name(t));
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
      fprintf (stderr, "Error:  Can't cope with typedef of type %s yet.\n", cplusplus_type_name(t));
      exit (1);
    }
}

static void ListException (Exception e, Context context)
{
  if (type_basic_type (ur_type(e->type)) == void_Type)
    return;
  else
    fprintf (context->file, "\t\t%s %s_Value;\n", cplusplus_return_type(e->type), cplusplus_exception_name(e));
}

static void declare_status_struct (Interface i, Context c)
{
  fprintf (c->file, "#define %sReply_Success\t\t((ilu_Exception) NULL)\n\n", cplusplus_interface_name(i));
  fprintf (c->file, "struct _%s_Status_struct {\n\tilu_Exception returnCode;\n\tilu_Passport callerPassport;\n\tunion {\n\t\tilu_Cardinal anyvalue;\n",cplusplus_interface_name(i));
  list_enumerate (i->exceptions, (iluparser_EnumProc) ListException, c);
  fprintf (c->file, "\t} values;\n};\n\n");
}

static void GenerateExceptionTable (Exception e, Context context)
{
  if (e->interface == context->interface AND e->importInterfaceName == NULL)
    fprintf (context->file, "\tilu_Exception %s;\n", cplusplus_simple_name(e->name));
}

static void generate_exception_table (Interface i, Context c)
{
  fprintf (c->file, "struct %s_Exceptions_s {\n", cplusplus_interface_name(i));
  if (list_size(i->exceptions) > 0)
    list_enumerate (i->exceptions, (iluparser_EnumProc) GenerateExceptionTable, c);
  else
    fprintf (c->file, "\tilu_CString dummyException;\n");
  fprintf (c->file, "};\n\n");
}

static void GenerateInputOutputSizeDeclaration (Type type, Context context)
{
  Type ut = ur_type(type);
  enum PrimitiveTypes t = type_basic_type(ur_type(type));

  if (type->importInterfaceName == NULL AND
      (    TypeIsNonObjectStruct(ut)
       OR (TypeIsArray(ut)
	   AND (((type_ur_kind(type_description(type)->structuredDes.array.type) != byte_Type)
		 AND (type_ur_kind(type_description(type)->structuredDes.array.type) != shortcharacter_Type))
		OR
		list_size(type_description(type)->structuredDes.array.dimensions) > 1))
       OR (t == sequence_Type
	   AND (type_ur_kind(type_description(type)->structuredDes.sequence.type) != shortcharacter_Type))))
    {
      char *paramType = cplusplus_parameter_type(type, In);
      char *refType = t == array_Type ? paramType : cplusplus_return_type(type);

      fprintf (context->file, "  static ilu_Boolean Output_%s (iluCall call, %s val);\n",
	       cplusplus_simple_name(type->name), paramType);
      fprintf (context->file, "  static %s Input_%s (iluCall call, %s ref);\n",
	       cplusplus_return_type(type), cplusplus_simple_name(type->name), refType);
      fprintf (context->file, "  static ilu_Cardinal SizeOf_%s (iluCall call, %s val);\n\n",
	       cplusplus_simple_name(type->name), paramType);
    }
  if (HasFreeRoutine(type))
    fprintf (context->file, "  static void Free_%s (%s val);\n",
	     cplusplus_simple_name(type->name),
	     cplusplus_parameter_type(type, In));
}

static void generate_interface_wide_class (Interface i, Context c)
{
  char *name = (char *) cplusplus_interface_name(i);

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
	    cplusplus_exception_name(e),
	    cplusplus_interface_name(context->interface),
	    cplusplus_simple_name(e->name));
}

static void PrintStringChar (FILE *file, int quote, int ch)
{
  if (ch < ' ' || '~' < ch)
    fprintf (file, "\\%03o", ch);
  else
    {
      if (ch == quote || ch == '\\')
        putc ('\\', file);
      putc (ch, file);
    }
}

static void PrintString (FILE *file, const string str)
{
  unsigned char *p = (unsigned char *) str;
  int ch;

  putc ('"', file);
  while ((ch = *p++) != 0)
    PrintStringChar (file, '"', ch);
  putc ('"', file);
}

#define CONSTANT_NAME(c)		(cplusplus_constant_name(c))

static void declare_constant (Constant c, Context context)
{
  enum PrimitiveTypes t = type_ur_kind(c->type);

  if (c->interface != context->interface)
    return;

  if ((t == cardinal_Type || t == shortcardinal_Type || t == byte_Type)
      && (c->value->type == integer_Type))
    {
      fprintf (context->file, "static const %s %s = %lu;\n",
	       (t == cardinal_Type) ? "ilu_Cardinal" : ((t == byte_Type) ? "ilu_Byte" : "ilu_ShortCardinal"),
	       CONSTANT_NAME(c),
	       c->value->val.i.value);
    }
  else if ((t == integer_Type || t == shortinteger_Type)
      && (c->value->type == integer_Type))
    {
      fprintf (context->file, "static const %s %s = %s%lu;\n",
	       (t == integer_Type) ? "ilu_Integer" : "ilu_ShortInteger",
	       CONSTANT_NAME(c),
	       (c->value->val.i.sign < 0) ? "-" : "",
	       c->value->val.i.value);
    }
  else if (t == shortcharacter_Type
	   && (c->value->type == integer_Type))
    {
      fprintf (context->file, "static const ilu_ShortCharacter %s = %s%lu;\n",
	       CONSTANT_NAME(c),
	       (c->value->val.i.sign < 0) ? "-" : "",
	       c->value->val.i.value);
    }
  else if (t == shortcharacter_Type
	   && (c->value->type == shortcharacter_Type))
    {
      fprintf (context->file, "static const ilu_ShortCharacter %s = '", CONSTANT_NAME(c));
      PrintStringChar (context->file, '\'', c->value->val.s[0]);
      fprintf (context->file, "';\n");
    }
  else if ((t == real_Type || t == shortreal_Type)
      && (c->value->type == real_Type))
    {
      fprintf (context->file, "static const %s %s = %s%s.%se%ld;\n",
	       (t == real_Type) ? "ilu_Real" : "ilu_ShortReal",
	       CONSTANT_NAME(c),
	       (c->value->val.r.sign < 0) ? "-" : "",
	       c->value->val.r.value, (c->value->val.r.fraction == NULL) ? "0" : c->value->val.r.fraction,
	       c->value->val.r.exponent);
    }
  else if ((t == real_Type || t == shortreal_Type)
      && (c->value->type == integer_Type))
    {
      fprintf (context->file, "static const %s %s = %s%lu.0;\n",
	       (t == real_Type) ? "ilu_Real" : "ilu_ShortReal",
	       CONSTANT_NAME(c),
	       (c->value->val.i.sign < 0) ? "-" : "",
	       c->value->val.i.value);
    }
  else if (t == boolean_Type)
    {
      fprintf (context->file, "static const ilu_Boolean %s = %s;\n",
	       CONSTANT_NAME(c),
	       (c->value->val.b) ? "ilu_TRUE" : "ilu_FALSE");
    }
  else if (t == sequence_Type
	   && (type_ur_kind(type_description(c->type)->structuredDes.sequence.type) == shortcharacter_Type)
	   && c->value->type == shortcharacter_Type)
    {
      fprintf (context->file, "static const ilu_CString %s = ", CONSTANT_NAME(c));
      PrintString (context->file, c->value->val.s);
      fprintf (context->file, ";\n");
    }
  else
    fprintf (stderr, "Invalid constant, %s, encountered.\n", name_base_name(c->name));
}

void generate_headers (Interface interface, FILE *file)
{
  struct context_s context;
  list sorted = new_list();

  context.file = file;
  context.interface = interface;
  context.class = NULL;

  fprintf (file, "#ifndef __%s_H_\n",
	   cplusplus_interface_name(interface));
  fprintf (file, "#define __%s_H_ 1\n",
	   cplusplus_interface_name(interface));
  fprintf (file, "#ifndef __ilu_H_\n");
  fprintf (file, "#include <ilu.hh>\n");
  fprintf (file, "#endif /* ndef __ilu_H_ */\n\n");

  GenerateNecessaryIncludes (&context);

/*
  list_enumerate (interface->constants, declare_constant, &context);
*/
  fprintf (file, "\n");

  typedef_status_block(&context);

  list_enumerate (interface->types, (iluparser_EnumProc) UnmarkSupertypes, NULL);
  list_enumerate (interface->types, (iluparser_EnumProc) sort_types_for_declaration, sorted);
  list_enumerate (sorted, (iluparser_EnumProc) typedef_type, &context);
  fprintf (file, "\n");

  list_enumerate (sorted, (iluparser_EnumProc) declare_type, &context);
  fprintf (file, "\n");

  list_enumerate (interface->constants, (iluparser_EnumProc) declare_constant, &context);

  generate_interface_wide_class (interface, &context);

  declare_status_struct (interface, &context);

  generate_exception_table (interface, &context);

  list_enumerate (interface->exceptions, (iluparser_EnumProc) declare_exception, &context);

  fprintf (file, "\n");
  fprintf(file, "extern void %s__Initialize(void);\t//ILU private\n\n",
	  cplusplus_interface_name(interface));
  fprintf (file, "#endif /* ndef __%s_H_ */\n",
	   cplusplus_interface_name(interface));
}
