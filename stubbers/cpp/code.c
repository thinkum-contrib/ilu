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
$Id: code.c,v 1.77 1999/08/03 01:50:57 janssen Exp $
*/

#include <stdlib.h>

#include "cplusplus.h"

#include <version.h>

/***********************************************************************\
*************************************************************************
**
**		Global class code
**
*************************************************************************
\***********************************************************************/

/*=====================================================================*\
**=====================================================================**
**              Exceptions
**=====================================================================**
\*=====================================================================*/

static void generate_exception_table (Interface interface, Context context)
{
  Exception       e;
  unsigned int    i;

  boolean         addComma = FALSE;

  if (list_size(interface->exceptions) > 0) {
    fprintf(context->file, "static struct %s_Exceptions_s exnstr = {",
	    cplusplus_interface_name(interface));
    for (i = 0; i < list_size(interface->exceptions); i++) {
      e = (Exception) list_ref(interface->exceptions, i);
      if (e->interface == interface && e->importInterfaceName == NULL) {
	fprintf(context->file, "\n\t%s(ilu_Exception) 0\t//%s",
		addComma ? "," : " ", exception_name(e));
	if (e->corba_rep_id != NULL)
	  fprintf(context->file, "(CORBA rep = \"%s\")",
		  e->corba_rep_id);
	addComma = TRUE;
      }
    }
    fprintf(context->file, "\n};\n\n");

    fprintf(context->file,
	    "struct %s_Exceptions_s * %s_G::Exceptions()\n{\n",
	    cplusplus_interface_name(interface),
	    cplusplus_interface_name(interface));
    fprintf(context->file, "\treturn(&exnstr);\n}\n\n");
  }
}

static void DefineException(refany elt, refany rock)
{
  Exception       e = (Exception) elt;
  Context         context = (Context) rock;
  if (e->interface != context->interface
      || e->importInterfaceName != NULL)
    return;
  fprintf(context->file, "  exnstr.%s = ilu::DefineException",
	  cplusplus_simple_name(e->name));
  if (e->corba_rep_id == NULL)
    fprintf(context->file, "(\"%s\", \"%s\", ",
	    interface_name(e->interface),
	    exception_name(e));
  else
    fprintf(context->file, "(NULL, \"%s\", ",
	    e->corba_rep_id);
  if (e->type == NULL)
    fprintf(context->file, "NULL");
  else
    fprintf(context->file, "\"%s\"", type_uid(e->type));
  fprintf(context->file, ");\n");
  return;
}

static void CatchException (Exception e, Context context)
{
  enum PrimitiveTypes t = type_ur_kind (e->type);
  character buf[1000];

  if (t != void_Type)
    {
      fprintf (context->file, "\t\telse if (_val->returnCode == %s)\n\t\t\t{\n\t\t\t", cplusplus_exception_name(e));
      if (TypeIsNonObjectStruct(e->type) || t == array_Type)
	fprintf (context->file, "\t_val->values.%s_Value = (%s) malloc(sizeof(%s));\n\t\t\t",
		 cplusplus_exception_name(e), cplusplus_return_type(e->type), cplusplus_type_name(e->type));
      sprintf (buf, "%s_val->values.%s_Value", t == array_Type ? "*" : "", cplusplus_exception_name(e));
      UnpackValue (context, e->type, e->def, buf,
		   (TypeIsNonObjectStruct(e->type) || t == array_Type), FALSE);
      fprintf (context->file, "\t\t\t}\n");
    }
}

static void generate_catch_exception (Context context)
{
  fprintf(context->file,
	  "static void %sCatchException (iluCall _call, %sStatus *_val, ilu_Cardinal _ecode)\n{\n",
	  cplusplus_interface_name(context->interface),
	  cplusplus_interface_name(context->interface));
  fprintf(context->file,
	  "\tif (_ecode > ilu_MethodOfCall(&_call->call)->me_exceptionCount || _ecode == 0) {\n");
  fprintf(context->file, "\t\t_val->returnCode = ilu::ProtocolError;\n");
  fprintf(context->file,
	  "\t\t_val->values.anyvalue = (ilu_Cardinal) ilu_ProtocolException_Unknown;\n");
  fprintf(context->file, "\t}\n");
  fprintf(context->file, "\telse {\n");
  fprintf(context->file, "\t\t_val->returnCode = ilu::ExceptionOfMethod(ilu_MethodOfCall(&_call->call), _ecode);\n");
  fprintf(context->file, "\t\tif (_val->returnCode == NULL)\n");
  fprintf(context->file, "\t\t\treturn;\n\n");
  list_enumerate(context->interface->exceptions,
		 (iluparser_EnumProc) CatchException, context);
  fprintf(context->file, "\n\t}\n\treturn;\n}\n\n");
}

static void SendExceptionValue (refany elt, refany rock)
{
  Exception       e = (Exception) elt;
  Context         context = (Context) rock;
  enum PrimitiveTypes t = type_ur_kind(e->type);
  char            buf[1000];

  if (e->type == NULL)
    return;

  fprintf(context->file, "  else if (stat->returnCode == %s) {\n",
	  cplusplus_exception_name(e));
  sprintf(buf, "%sstat->values.%s_Value",
	  t == array_Type ? "*" : "",
	  cplusplus_exception_name(e));
  EncodeValue(e->type, buf, context);
  fprintf(context->file, "\t}\n");
}

static void SizeException (refany elt, refany rock)
{
  Exception       e = (Exception) elt;
  Context         context = (Context) rock;
  enum PrimitiveTypes t = type_ur_kind(e->type);
  char            buf[1000];

  if (e->type == NULL)
    return;

  fprintf(context->file, "  else if (stat->returnCode == %s) {\n",
	  cplusplus_exception_name(e));
  sprintf(buf, "%sstat->values.%s_Value", t == array_Type ? "*" : "",
	  cplusplus_exception_name(e));
  SizeType(e->type, buf, context);
  fprintf(context->file, "  }\n");
}

void generate_send_exception (Context context)
{
  fprintf(context->file, "#include <stdarg.h>\n\n");
  fprintf(context->file,
      "int %s_G::SendException (iluCall _call, %sStatus *stat)\n",
	  cplusplus_interface_name(context->interface),
	  cplusplus_interface_name(context->interface));
  fprintf(context->file, "{\n  ilu_Cardinal _dSize, _argSize = 0;\n");
  fprintf(context->file, "  ilu_Cardinal eCode = 0, i, limit;\n");
  fprintf(context->file, "  ilu_Method m;\n");
  fprintf(context->file, "  m = ilu_MethodOfCall(&_call->call);\n");
  fprintf(context->file, "  limit = m->me_exceptionCount;\n");
  fprintf(context->file, "  for (i = 1;  i <= limit;  i += 1)\n");
  fprintf(context->file, "    if (ilu::ExceptionOfMethod(m, i) == stat->returnCode)\n");
  fprintf(context->file, "      {eCode = i; break;}\n");
  fprintf(context->file, "  _ilu_Assert(%s, \"%s_G::SendException\");\n",
	  "eCode > 0",
	  cplusplus_interface_name(context->interface));
  fprintf(context->file,
       "  _argSize = ilu::BeginSizingException(_call, eCode);\n");
  fprintf(context->file, "  if (stat->returnCode == NULL)\n");
  fprintf(context->file, "    0;\t/* can't happen */\n");
  list_enumerate(context->interface->exceptions, SizeException, context);
  MarshO(context, "ilu::BeginException (_call, eCode, _argSize)");
  fprintf(context->file, "  if (stat->returnCode == NULL)\n");
  fprintf(context->file, "    0;\t/* can't happen*/\n");
  list_enumerate(context->interface->exceptions, SendExceptionValue,
		 context);
  fprintf(context->file, "  ilu::FinishException (_call);\n");
  fprintf(context->file, "faild:\n");
  fprintf(context->file, "  return(0);\n");
  fprintf(context->file, "}\n\n");
}

static void SetExceptionValue (Exception e, Context context)
{
  char *vatype;
  char *cast;

  if (e->type == NULL)
    return;
  fprintf (context->file, "\telse if (stat->returnCode == %s)\n\t\t{\n", cplusplus_exception_name(e));
  if (type_ur_kind(e->type) == shortreal_Type)
    {
      vatype = "double";
      cast = "(float) ";
    }
  else
    {
      cast = "";
      vatype = cplusplus_return_type(e->type);
    }
  fprintf (context->file, "\t\t\tstat->values.%s_Value = %sva_arg(ap, %s);\n\t\t}\n",
	   cplusplus_exception_name(e), cast, vatype);
}     

static void generate_signal_exception (Context context)
{
  fprintf (context->file, "#include <stdarg.h>\n\n");
  fprintf (context->file, "void %s_G::RaiseException (%sStatus *stat, ilu_Exception exception...)\n",
	   cplusplus_interface_name(context->interface),
	   cplusplus_interface_name(context->interface));
  fprintf (context->file, "{\n\tva_list ap; va_start(ap, exception);\n");
  fprintf (context->file, "\tstat->returnCode = exception;\n");
  fprintf (context->file, "\tif (exception == NULL)\n\t\t;\n");
  list_enumerate (context->interface->exceptions, (iluparser_EnumProc) SetExceptionValue, context);
  fprintf (context->file, "\treturn;\n}\n\n");
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

static unsigned long DimCount;

static void DimHeader (refany elt, refany rock)
{
  long int        d = (long int) elt;
  Context         context = (Context) rock;
  fprintf(context->file, "\t{ register int _i%lu;\n", DimCount);
  fprintf(context->file, "\tfor (_i%lu = 0;  _i%lu < %lu;  _i%lu += 1)\n",
	  DimCount, DimCount, d, DimCount);
  DimCount += 1;
}

static void DimRef (refany elt, refany rock)
{
  long int        d = (long int) elt;
  char           *buf = (char *) rock;
  sprintf(buf + strlen(buf), "[_i%lu]", DimCount);
  DimCount += 1;
}

static void DimFooter (refany elt, refany rock)
{
  long int        d = (long int) elt;
  Context         context = (Context) rock;
  fprintf (context->file, "\t}\n");
}

struct double_s {
  Context c;
  Type t;
  Argument default_arm;
};


static char* KernelTypeKindName (Type type)
{
  switch (type_kind(type))
    {
      case byte_Type: return("ilu_byte_tk");
      case boolean_Type: return("ilu_boolean_tk");
      case character_Type: return("ilu_character_tk");
      case shortcharacter_Type: return("ilu_shortcharacter_tk");
      case shortinteger_Type: return("ilu_shortinteger_tk");
      case integer_Type: return("ilu_integer_tk");
      case longinteger_Type: return("ilu_longinteger_tk");
      case shortcardinal_Type: return("ilu_shortcardinal_tk");
      case cardinal_Type: return("ilu_cardinal_tk");
      case longcardinal_Type: return("ilu_longcardinal_tk");
      case real_Type: return("ilu_real_tk");
      case shortreal_Type: return("ilu_shortreal_tk");
      case longreal_Type: return("ilu_longreal_tk");
      case object_Type: return("ilu_object_tk");
      case optional_Type: return("ilu_optional_tk");
      case alias_Type: return("ilu_alias_tk");
      case pickle_Type: return("ilu_pickle_tk");
      case union_Type: return("ilu_union_tk");
      case sequence_Type: return("ilu_sequence_tk");
      case record_Type: return("ilu_record_tk");
      case array_Type: return("ilu_array_tk");
      case enumeration_Type: return("ilu_enumeration_tk");
      case pipe_Type: return("ilu_enumeration_tk");
      default: return NULL;
    }
}


static void OutputRecordArg (refany elt, refany rock)
{
  Argument        arg = (Argument) elt;
  Context         context = (Context) rock;
  char            buf[1000];

  sprintf(buf, "(%s_val->%s)",
	  TypeIsNonObjectStruct(arg->type) ? "&" : "",
	  cplusplus_argument_name(arg));
  EncodeValue(arg->type, buf, context);
}

static void caseConst (ConstantValue val, struct double_s *s)
{
  switch (val->type)
    {
    case integer_Type:
    case shortinteger_Type:
    case cardinal_Type:
    case shortcardinal_Type:
    case byte_Type:
      fprintf (s->c->file, "\tcase %s%lu:\n", (val->val.i.sign < 0) ? "-" : "", val->val.i.value);
      break;
    case shortcharacter_Type:
      fprintf (s->c->file, "\tcase %s_%s:\n", cplusplus_type_name (s->t), cplusplus_string(val->val.s));
      break;
    case boolean_Type:
      fprintf (s->c->file, "\tcase ilu_%s:\n", val->val.b ? "TRUE" : "FALSE");
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
  char            buffer[1000];
  char           *name;

  if (a->name->base_name)
    name = (char *) cplusplus_simple_name(a->name);
  else
    name = (char *) cplusplus_string(type_name(a->type));
  sprintf(buffer, "%s_val->value.%s",
	  TypeIsNonObjectStruct(a->type) ? "&" : "", name);
  if (s->default_arm == a)
    fprintf(s->c->file, "\tdefault:\n");
  else
    list_enumerate(a->values, (void (*) (refany, refany)) caseConst, s);
  EncodeValue(a->type, buffer, s->c);
  fprintf(s->c->file, "\tbreak;\n\n");
}

static void generate_output_code (Type type, enum PrimitiveTypes t, Context context)
{
  TypeDescription d = type_description(type);

  fprintf(context->file,
	  "ilu_Boolean %s_G::Output_%s (iluCall _call, %s _val)\n",
	  cplusplus_interface_name(context->interface),
	  cplusplus_simple_name(type->name),
	  cplusplus_parameter_type(type, In));
  fprintf(context->file, "{\n");

  if (t == array_Type) {
    char            buf[1000];

    MarshO(context, "ilu::OutputArray (_call)");
    DimCount = 0;
    list_enumerate(d->structuredDes.array.dimensions, DimHeader,
		   context);
    sprintf(buf, "_val");
    DimCount = 0;
    list_enumerate(d->structuredDes.array.dimensions, DimRef, buf);
    EncodeValue(d->structuredDes.array.type, buf, context);
    DimCount = 0;
    list_enumerate(d->structuredDes.array.dimensions, DimFooter, context);
    fprintf(context->file, "\tilu::EndArray (_call);\n");
  } else if (t == sequence_Type) {
    char            buf[200];
    Type            st = d->structuredDes.sequence.type;

    if (type_ur_kind(st) == byte_Type)
      MarshO(context,
	     "ilu::OutputBytes (_call, _val->Array(), _val->Length(), %lu)",
	     d->structuredDes.sequence.limit);
    else {
      MarshO(context,
	     "ilu::OutputSequence (_call, _val->Length(), %lu)",
	     d->structuredDes.sequence.limit);
      fprintf(context->file, "\t{\n");
      fprintf(context->file, "\t%s *data = _val->Array();\n",
	      ((type_ur_kind(st) == object_Type)
	       ? cplusplus_parameter_type(st, In)
	       : cplusplus_type_name(st)));
      fprintf(context->file, "\tilu_Cardinal i;\n\n");
      fprintf(context->file,
	      "\tfor(i = 0;  i < _val->Length();  i++)\n\t{\n");
      sprintf(buf, "%sdata[i]", TypeIsNonObjectStruct(st) ? "&" : "");
      EncodeValue(d->structuredDes.sequence.type, buf, context);
      fprintf(context->file, "\t}};\n\tilu::EndSequence(_call);\n");
    }
  } else if (t == union_Type) {
    struct double_s s;

    s.c = context;
    s.t = ur_type(d->structuredDes.uniond.discriminator_type);
    s.default_arm = d->structuredDes.uniond.default_arm;

    MarshO(context,
	    "ilu::OutputUnion (_call, (ilu_Cardinal) _val->discriminator, %s)",
	     KernelTypeKindName(s.t));
    fprintf(context->file, "\tswitch (_val->discriminator) {\n");
    list_enumerate(d->structuredDes.uniond.types, OutputUnionType, &s);
    if (s.default_arm != NULL)
      0;
    else if (d->structuredDes.uniond.others_allowed) {
      fprintf(context->file, "\t\tdefault:\n\t\t\tbreak;\n");
    } else {
      fprintf(context->file, "\t\tdefault:\n");
      fprintf(context->file,
	      "\t\t\tfprintf (stderr, \"Bad value %%d in discriminant of value of union type %s.\\n\", _val->discriminator);\n",
	      cplusplus_type_name(type));
      fprintf(context->file, "\t\t\tbreak;\n");
    }
    fprintf(context->file, "\t}\n\tilu::EndUnion(_call);\n");
  } else if (t == record_Type) {
    MarshO(context, "ilu::OutputRecord (_call)");
    list_enumerate(d->structuredDes.record.fields, OutputRecordArg, context);
    fprintf(context->file, "\tilu::EndRecord (_call);\n");
  }
  /* else if (t == object_Type) done elsewhere */

  fprintf(context->file, "\treturn 1;\n");
  fprintf(context->file, "faild:\n");
  fprintf(context->file, "\treturn 0;\n");
  fprintf(context->file, "}\n\n");
}

static void InputRecordArg (Argument arg, Context context)
{
  enum PrimitiveTypes t = type_ur_kind (arg->type);
  char buf[1000];
  int ref;

  if (t == array_Type)
    ref = TRUE;
  else
    ref = FALSE;

  sprintf (buf, "_val->%s", cplusplus_argument_name(arg));
  UnpackValue (context, arg->type, 0, buf, ref, FALSE);
}

static void InputUnionType (Argument a, struct double_s *s)
{
  char   buffer[ 1000 ];
  char  *name;

  if (a->name->base_name)
    name = (char *) cplusplus_simple_name (a->name);
  else
    name = (char *) cplusplus_string (type_name (a->type));

  if (a == s->default_arm)
    fprintf (s->c->file, "\tdefault:\n");
  else
    list_enumerate (a->values, (void (*)(refany, refany)) caseConst, s);
  sprintf (buffer, "_val->value.%s", name);
  UnpackValue (s->c, a->type, 0, buffer, FALSE, FALSE);
  fprintf (s->c->file, "\tbreak;\n\n");
}

static void generate_input_code (Type type, enum PrimitiveTypes t, Context context)
{
  char *ret = (char *) cplusplus_return_type(type);
  TypeDescription d = type_description(type);
  char *name = cplusplus_type_name(type);
  char *refType = t == array_Type ? cplusplus_parameter_type(type, In) : ret;

  context->class = type;
  fprintf (context->file, "%s %s_G::Input_%s (iluCall _call, %s _ref)\n{\n\t%s _val;\n\n",
	   ret, cplusplus_interface_name(context->interface), cplusplus_simple_name(type->name), refType, ret);
  if (t == array_Type)
    {
      char buf[1000];

      fprintf (context->file, "\tilu::InputArray (_call);\n");
      fprintf (context->file, "\tif (_ref != NULL) _val = (%s) _ref; else _val = (%s) malloc(sizeof(%s));\n",
	       ret, ret, name);
      DimCount = 0;
      list_enumerate(d->structuredDes.array.dimensions, DimHeader,
		     context);
      sprintf (buf, "(*_val)");
      DimCount = 0;
      list_enumerate (d->structuredDes.array.dimensions, DimRef, buf);
      UnpackValue (context, d->structuredDes.array.type, type->def, buf, FALSE, FALSE);
      DimCount = 0;
      list_enumerate (d->structuredDes.array.dimensions, DimFooter, context);
      fprintf (context->file, "\tilu::EndArray(_call);\n");
    }
  else if (t == sequence_Type)
    {
      Type et = d->structuredDes.sequence.type;
      string tname = (string) cplusplus_return_type(et);

      if (type_ur_kind(et) == byte_Type)
	{
	  fprintf (context->file, "\tilu_Cardinal _count;\n\tilu_Byte *_bytes;\n\n");
	  fprintf (context->file, "\t_bytes = ilu::InputBytes (_call, NULL, &_count, %lu);\n",
		   d->structuredDes.sequence.limit);
	  fprintf (context->file, "\tif (_ref != NULL) {\n\t\t_val = _ref;\n");
	  fprintf (context->file, "\t\t_val->_%s_sequence::Clear(ilu_FALSE);\n", name);
	  fprintf (context->file, "\t\tfor (ilu_Cardinal _index = 0; _index < _count; _index++)\n");
	  fprintf (context->file, "\t\t\t_val->_%s_sequence::Append(_bytes[_index]);\n", name);
	  fprintf (context->file, "\t}\n");
	  fprintf (context->file, "\telse _val = _%s_sequence::Create (_count, _bytes);\n", name);
	}
      else
	{
	  fprintf (context->file, "\tilu_Cardinal _count, _index;\n");
	  fprintf (context->file, "\t%s _tmp;\n\n", tname);
	  fprintf (context->file, "\tilu::InputSequence (_call, &_count, %lu);\n",
		   d->structuredDes.sequence.limit);
	  fprintf (context->file, "\tif (_ref != NULL) _val = _ref; else _val = new _%s_sequence;\n",
		   cplusplus_type_name(ur_type(type)));
	  fprintf (context->file, "\t_val->Clear(ilu_FALSE);\n");
	  fprintf (context->file, "\tfor (_index = 0;  _index < _count;  _index++)\n\t\t{\n\t");
	  UnpackValue (context, d->structuredDes.sequence.type, type->def, "_tmp", TypeIsNonObjectStruct(type), TRUE);
	  fprintf (context->file, "\t\t_val->Append (_tmp);\n\t};\n");
	  fprintf (context->file, "\tilu::EndSequence(_call);\n");
	}
    }
  else if (t == union_Type)
    {
      struct double_s s;

      s.c = context;
      s.t = ur_type(d->structuredDes.uniond.discriminator_type);
      s.default_arm = d->structuredDes.uniond.default_arm;

      fprintf (context->file, "\tilu_Cardinal discriminator;\n");
      fprintf (context->file, "\tilu::InputUnion (_call, &discriminator, %s);\n", KernelTypeKindName(s.t));
      fprintf (context->file, "\tif (_ref != NULL) _val = _ref; else _val = (%s) malloc(sizeof(%s));\n",
	       ret, name);
      fprintf (context->file, "\tswitch (discriminator) {\n");
      list_enumerate (d->structuredDes.uniond.types, (iluparser_EnumProc) InputUnionType, &s);
      if (s.default_arm != NULL)
	; /* handled in inputuniontype */
      else if (d->structuredDes.uniond.others_allowed)
	{
	  fprintf (context->file, "\t\tdefault:\n\t\t\tbreak;\n");
	}
      else /* generate error on bad discriminator value */
	{
	  fprintf (context->file, "\t\tdefault:\n");
	  fprintf (context->file, "\t\t\tfprintf (stderr, \"Bad value %%d received for discriminant of value of union type %s.\\n\", discriminator);\n\t\t\tbreak;\n", cplusplus_type_name(s.t));
	}
      fprintf (context->file, "\t}\n\t_val->discriminator = (%s) discriminator;\n", cplusplus_type_name(s.t));
      fprintf (context->file, "\tilu::EndUnion(_call);\n");
    }
  else if (t == record_Type)
    {
      fprintf (context->file, "\tilu::InputRecord(_call);\n");
      fprintf (context->file, "\tif (_ref != NULL) _val = _ref; else _val = (%s) malloc(sizeof(%s));\n", ret, name);
      list_enumerate (d->structuredDes.record.fields, (iluparser_EnumProc) InputRecordArg, context);
      fprintf (context->file, "\tilu::EndRecord(_call);\n");
    }
  /* else if (t == object_Type) done elsewhere */

  fprintf (context->file, "\treturn (_val);\n}\n\n");
}

static void SizeOfUnionType (refany elt, refany rock)
{
  Argument        a = (Argument) elt;
  struct double_s *s = (struct double_s *) rock;
  char            buffer[1000];
  char           *name;

  if (a->name->base_name)
    name = (char *) cplusplus_simple_name(a->name);
  else
    name = (char *) cplusplus_string(type_name(a->type));
  sprintf(buffer, "%s_val->value.%s",
	  TypeIsNonObjectStruct(a->type) ? "&" : "", name);
  if (a == s->default_arm)
    fprintf(s->c->file, "\tdefault:\n");
  else
    list_enumerate(a->values, (void (*) (refany, refany)) caseConst, s);
  SizeType(a->type, buffer, s->c);
  fprintf(s->c->file, "\tbreak;\n");
}

static void SizeRecordArg (refany elt, refany rock)
{
  Argument        arg = (Argument) elt;
  Context         context = (Context) rock;
  char            buf[1000];
  sprintf(buf, "(%s_val->%s)",
	  TypeIsNonObjectStruct(arg->type) ? "&" : "",
	  cplusplus_argument_name(arg));
  SizeType(arg->type, buf, context);
}

static void 
generate_sizeof_code(Type type, enum PrimitiveTypes t,
		     Context context)
{
  TypeDescription d = type_description(type);

  fprintf(context->file,
     "ilu_Cardinal %s_G::SizeOf_%s (iluCall _call, %s _val)\n{\n",
	  cplusplus_interface_name(context->interface),
	  cplusplus_simple_name(type->name),
	  cplusplus_parameter_type(type, In));
  if (t == array_Type) {
    char            buf[1000];

    fprintf(context->file, "\tilu_Cardinal _dSize, _argSize = 0;\n\n");
    fprintf(context->file, "\t_argSize = ilu::SizeOfArray (_call);\n");
    DimCount = 0;
    list_enumerate(d->structuredDes.array.dimensions, DimHeader,
		   context);
    sprintf(buf, "_val");
    DimCount = 0;
    list_enumerate(d->structuredDes.array.dimensions, DimRef, buf);
    SizeType(d->structuredDes.array.type, buf, context);
    DimCount = 0;
    list_enumerate(d->structuredDes.array.dimensions, DimFooter,
		   context);
    fprintf(context->file, "\tilu::EndArray(_call);\n");
    fprintf(context->file, "\treturn (_argSize);\n");
    fprintf(context->file, "faild:\n");
    fprintf(context->file, "\treturn 0;\n");
  } else if (t == sequence_Type) {
    char            buf[1000];
    Type            st = d->structuredDes.sequence.type;

    if (type_ur_kind(st) == byte_Type) {
      fprintf(context->file,
	      "\treturn(ilu::SizeOfBytes (_call, _val->Array(), _val->Length(), %lu));\n",
	      d->structuredDes.sequence.limit);
    } else {
      fprintf(context->file, "\tilu_Cardinal _dSize, _argSize = 0;\n");
      fprintf(context->file, "\tregister %s *data = _val->Array();\n",
	      ((type_ur_kind(st) == object_Type)
	       ? cplusplus_parameter_type(st, In)
	       : cplusplus_type_name(st)));
      fprintf(context->file,
	      "\tregister ilu_Cardinal i = _val->Length();\n\n");
      MarshS(context, "ilu::SizeOfSequence (_call, i, %lu)",
	     d->structuredDes.sequence.limit);
      fprintf(context->file,
	      "\tfor (i = 0;  i < _val->Length();  i++) {\n");
      sprintf(buf, "%sdata[i]", TypeIsNonObjectStruct(st) ? "&" : "");
      SizeType(d->structuredDes.sequence.type, buf, context);
      fprintf(context->file, "\t}\n");
      fprintf(context->file, "\tilu::EndSequence(_call);\n");
      fprintf(context->file, "\treturn (_argSize);\n");
      fprintf(context->file, "faild:\n");
      fprintf(context->file, "\treturn 0;\n");
    }
  } else if (t == union_Type) {
    struct double_s s;

    s.c = context;
    s.t = ur_type(d->structuredDes.uniond.discriminator_type);
    s.default_arm = d->structuredDes.uniond.default_arm;

    fprintf(context->file, "\tilu_Cardinal _dSize, _argSize = 0;\n");
    MarshS(context,
       "ilu::SizeOfUnion (_call, _val->discriminator, %s)",
	   KernelTypeKindName(s.t));
    fprintf(context->file, "\tswitch (_val->discriminator) {\n");
    list_enumerate(d->structuredDes.uniond.types, SizeOfUnionType, &s);
    if (s.default_arm != NULL)
    0;
    else if (d->structuredDes.uniond.others_allowed) {
      fprintf(context->file, "\tdefault:\n\t\tbreak;\n");
    } else {
      fprintf(context->file, "\tdefault:\n");
      fprintf(context->file, "\t\tfprintf (stderr, \"Bad value %%d in discriminant of value of union type %s.\\n\", _val->discriminator);\n", cplusplus_type_name(type));
      fprintf(context->file, "\t\tbreak;\n");
    }
    fprintf(context->file, "\t}\n");
    fprintf(context->file, "\tilu::EndUnion(_call);\n");
    fprintf(context->file, "\treturn (_argSize);\n");
    fprintf(context->file, "faild:\n");
    fprintf(context->file, "\treturn 0;\n");
  } else if (t == record_Type) {
    fprintf(context->file, "\tilu_Cardinal _dSize, _argSize = 0;\n");
    fprintf(context->file, "\t_argSize = ilu::SizeOfRecord (_call);\n");
    list_enumerate(d->structuredDes.record.fields, SizeRecordArg, context);
    fprintf(context->file, "\tilu::EndRecord(_call);\n");
    fprintf(context->file, "\treturn (_argSize);\n");
    fprintf(context->file, "faild:\n");
    fprintf(context->file, "\treturn 0;\n");
  }
  /* else if (t == object_Type) done elsewhere */
  fprintf(context->file, "}\n\n");
}

static void FreeUnionType (Argument a, struct double_s *s)
{
  char buffer[1000];
  char *name;

  if (a->name->base_name)
    name = (char *) cplusplus_simple_name (a->name);
  else
    name = (char *) cplusplus_string (type_name (a->type));
  sprintf (buffer, "%s_val->value.%s", TypeIsNonObjectStruct(a->type) ? "&" : "", name);
  if (a == s->default_arm)
    fprintf (s->c->file, "\tdefault:\n");
  else
    list_enumerate (a->values, (void (*)(refany, refany)) caseConst, s);
  FreeValue (a->type, buffer, s->c);
  fprintf (s->c->file, "\t\tbreak;\n\n");
}

static void FreeRecordArg (Argument arg, Context context)
{
  char buf[1000];

  sprintf (buf, "%s_val->%s", TypeIsNonObjectStruct(arg->type) ? "&" : "", cplusplus_argument_name(arg));
  FreeValue (arg->type, buf, context);
}

static void generateFreeCode (Type type, enum PrimitiveTypes t, Context context)
{
  TypeDescription d = type_description(type);

  fprintf (context->file, "void %s_G::Free_%s (%s _val)\n{\n",
	   cplusplus_interface_name(context->interface),
	   cplusplus_simple_name(type->name), cplusplus_parameter_type(type, In));
  if (t == array_Type)
    {
      char buf[1000];

      DimCount = 0;
      list_enumerate(d->structuredDes.array.dimensions, DimHeader,
		     context);
      sprintf (buf, "_val");
      DimCount = 0;
      list_enumerate (d->structuredDes.array.dimensions, DimRef, buf);
      FreeValue (d->structuredDes.array.type, buf, context);
      DimCount = 0;
      list_enumerate (d->structuredDes.array.dimensions, DimFooter, context);
    }
  else if (t == sequence_Type)
    {
      char buf[1000];
      Type st = d->structuredDes.sequence.type;

      fprintf (context->file, "\tregister %s *data = _val->Array();\n\n",
	       (type_ur_kind(st) == object_Type) ? cplusplus_parameter_type(st, In) : cplusplus_type_name(st));
      if (HasFreeRoutine(d->structuredDes.sequence.type))
	{
	  fprintf (context->file, "\tilu_Cardinal _size = _val->Length();\n\n");
	  fprintf (context->file, "\tregister ilu_Cardinal i;\n\n");
	  fprintf (context->file, "\tif (_size > 0) {\n");
	  fprintf (context->file, "\t\tfor (i = 0;  i < _size;  i++)\t\t");
	  sprintf (buf, "%sdata[i]", TypeIsNonObjectStruct(st) ? "&" : "");
	  FreeValue (d->structuredDes.sequence.type, buf, context);
	  fprintf (context->file, "\t};\n");
	}
      else
	{
	  fprintf (context->file, "\tfree((char *) data);\n");
	}
    }
  else if (t == union_Type)
    {
      struct double_s s;

      s.c = context;
      s.t = ur_type(d->structuredDes.uniond.discriminator_type);
      s.default_arm = d->structuredDes.uniond.default_arm;

      fprintf (context->file, "\tswitch (_val->discriminator) {\n");
      list_enumerate (d->structuredDes.uniond.types, (iluparser_EnumProc) FreeUnionType, &s);
      if (s.default_arm != NULL)
	;
      else if (d->structuredDes.uniond.others_allowed)
	{
	  fprintf (context->file, "\t\tdefault:\n\t\t\tbreak;\n");
	}
      else
	{
	  fprintf (context->file, "\t\tdefault:\n");
	  fprintf (context->file, "\t\t\tfprintf (stderr, \"Bad value %%d in discriminant of value of union type %s.\\n\", _val->discriminator);\n\t\t\tbreak;\n", cplusplus_type_name(type));
	}
      fprintf (context->file, "\t}\n");
    }
  else if (t == record_Type)
    {
      list_enumerate (d->structuredDes.record.fields, (iluparser_EnumProc) FreeRecordArg, context);
    }
  else if (t == optional_Type)
    {
      FreeValue (d->structuredDes.optional, "_val", context);
    }
  /* else if (t == object_Type) done elsewhere */
  fprintf (context->file, "}\n\n");
}

static void generate_type_io_code (Type type, Context context)
{
  enum PrimitiveTypes t = type_ur_kind(type);

  if ((TypeIsNonObjectStruct(type)
       OR (TypeIsArray(type)
	   AND (((type_ur_kind(type_description(type)->structuredDes.array.type) != byte_Type)
		 AND (type_ur_kind(type_description(type)->structuredDes.array.type) != shortcharacter_Type))
		OR (list_size(type_description(type)->structuredDes.array.dimensions) > 1)))
       OR (t == sequence_Type
	   AND ((type_ur_kind(type_description(type)->structuredDes.array.type) != shortcharacter_Type)
		OR (list_size(type_description(type)->structuredDes.array.dimensions) > 1))))
      AND (type->importInterfaceName == NULL))
    {
      generate_output_code (type, t, context);
      generate_input_code (type, t, context);
      generate_sizeof_code (type, t, context);
    }

  if (HasFreeRoutine(type))
    generateFreeCode (type, t, context);
}

static void generate_ios_code (Interface interface, Context context)
{
  list_enumerate (interface->types, (iluparser_EnumProc) generate_type_io_code, context);
}

static void generate_global_code (Interface interface, Context context)
{
  generate_exception_table (interface, context);
  generate_exception_procs (context);
  generate_ios_code (interface, context);
}

static unsigned MethodIndex = 0;
static int      exnIdx = 0;

static void FillException (refany elt, refany rock)
{
  Exception       e = (Exception) elt;
  Context         context = (Context) rock;
  fprintf(context->file, "      exns[%d] = exnstr.%s;\n", exnIdx,
	  cplusplus_simple_name(e->name));
  exnIdx++;
}

static void GenMethDef (refany elt, refany rock)
{
  Procedure       m = (Procedure) elt;
  Context         context = (Context) rock;
  unsigned        nexns = 0;
  unsigned	  nargs = list_size(m->arguments);
  fprintf(context->file, "    {\t//for method %s\n", procedure_name(m));
  if (m->exceptions != NULL && (nexns = list_size(m->exceptions)) > 0) {
    fprintf(context->file, "      ilu_Exception exns[%lu];\n",
	    list_size(m->exceptions));
    exnIdx = 0;
    list_enumerate(m->exceptions, FillException, context);
  } else
    fprintf(context->file, "      ilu_Exception *exns = NULL;\n");
  fprintf(context->file, "      ilu::DefineMethod(t,\n");
  fprintf(context->file, "\t%u,\t//method index\n", MethodIndex);
  fprintf(context->file, "\t\"%s\",\t//name\n", procedure_name(m));
  fprintf(context->file, "\t%u,\t//method ID\n", m->id);
  fprintf(context->file, "\t%s,\t//cacheable\n",
	  m->functional ? "ilu_kernelTRUE" : "ilu_kernelFALSE");
  fprintf(context->file, "\t%s,\t//asynchronous\n",
	  m->asynch ? "ilu_kernelTRUE" : "ilu_kernelFALSE");
  fprintf(context->file, "\t%u,\t//num. exns\n", nexns);
  fprintf(context->file, "\texns,\t//the exceptions\n");
  fprintf(context->file, "\t%u,\t//num. args\n", nargs);
  fprintf(context->file, "\t%s%s%s);\t//return type\n",
	  (m->returnType == NULL) ? "" : "\"",
	  (m->returnType == NULL) ? "(ilu_CString) 0" : type_uid(m->returnType),
	  (m->returnType == NULL) ? "" : "\"");
  fprintf(context->file, "    }\t//end method %s\n", procedure_name(m));
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

static boolean FirstInList = FALSE;

static void PrintTypeID (refany elt, refany rock)
{
  Type            st = (Type) elt;
  Context         context = (Context) rock;
  for (st = st; st->supertype != NULL; st = st->supertype);
  fprintf(context->file, "%s\t\"%s\"", (FirstInList ? "" : ",\n"),
	  st->uid);
  FirstInList = FALSE;
}

static void generate_superclass_records (Type type, Context context)
{
  fprintf(context->file,
	  "    static ilu_CString Superclass_IDs[%lu] = {\n",
	  list_size(class_object(type)->superclasses));
  FirstInList = TRUE;
  list_enumerate(class_object(type)->superclasses, PrintTypeID, context);
  fprintf(context->file, "\n\t};\n");
}

static void RegisterClass (refany elt, refany rock)
{
  Type            class = (Type) elt;
  Context         context = (Context) rock;
  char           *tn = cplusplus_type_name(class);
  Class           o = class_object(class);
  unsigned        nmeth;

  if (class->builtIn || class->importInterfaceName != NULL)
    return;
  fprintf(context->file, "  {\t// for definition of %s\n", tn);

  if (list_size(o->superclasses) > 0)
    generate_superclass_records(class, context);

  nmeth = list_size(o->methods);
  
  fprintf(context->file, "    ilu_Class t;\n");
  fprintf(context->file,
	  "    %s::ILUClassRecord = t = ilu::DefineObjectType(\n",
	  tn);
  fprintf(context->file, "\t\"%s.%s\",\t/* ILU name */\n",
	  name_base_name(context->interface->name),
	  name_base_name(class->name));
  fprintf(context->file, "\t\"%s\",\t/* Brand */\n",
	  o->brand == NULL ? "" : o->brand);
  fprintf(context->file, "\t\"%s\",\t/* id */\n",
	  class->uid);
  fprintf(context->file, "\t%s%s%s,\t/* singleton */\n",
	  o->singleton == NULL ? "" : "\"",
	  o->singleton == NULL ? "NULL" : o->singleton,
	  o->singleton == NULL ? "" : "\"");
  fprintf(context->file, "\t%s,\t/* optional? */\n",
	  o->optional ? "ilu_kernelTRUE" : "ilu_kernelFALSE");
  fprintf(context->file, "\t%s,\t/* collectible? */\n",
	  o->collectible ? "ilu_kernelTRUE" : "ilu_kernelFALSE");
  fprintf(context->file, "\t%s%s%s,\t/* authentication */\n",
	  o->authentication == NULL ? "" : "\"",
	  o->authentication == NULL ? "NULL" : o->authentication,
	  o->authentication == NULL ? "" : "\"");

  fprintf(context->file, "\t%u,\t/* number of methods */\n", nmeth);

  if (o->superclasses == NULL || list_size(o->superclasses) == 0) {
    fprintf(context->file, "\t0,\t/* number of superclasses */\n");
    fprintf(context->file, "\tNULL\t/* no superclass uids */);\n");
  } else {
    fprintf(context->file, "\t%lu,\t/* number of superclasses */\n",
	    list_size(o->superclasses));
    fprintf(context->file, "\tSuperclass_IDs\t/* IDs of superclasses */);\n");
  }
  fprintf(context->file,
	  "    iluObject::RegisterSurrogateCreator(t, Create_%s);\n",
	  tn);
  
  context->class = class;
  GenerateMethodDefs(class, context);
  fprintf(context->file, "    ilu::ObjectTypeDefined(t);\n");
  fprintf(context->file, "  }\t// end definition of %s\n", tn);
}

static void generate_registration_code (Interface interface, Context context)
{
  char           *ifcname = (char *) cplusplus_interface_name(interface);

  fprintf(context->file,
	  "/* the following is all done to achieve load-time module initialization.\n");
  fprintf(context->file,
	  "   We declare a private class which only has one instance, statically declared.\n");
  fprintf(context->file,
	  "   We use the constructor of the class to do all the initializations we need\n");
  fprintf(context->file,
	  "   for the module, trusting that the single static instance of the class will\n");
  fprintf(context->file,
	  "   be initialized before the user code is given control. */\n\n");

  fprintf(context->file, "class _%s_RegistrationClass {\n\n public:\n\n",
	  ifcname);
  fprintf(context->file, "  _%s_RegistrationClass();\n};\n\n",
	  ifcname);

  fprintf(context->file,
	  "#ifndef macintosh\nstatic class _%s_RegistrationClass _%s_RegistrationInstance;\n#endif\n\n",
	  ifcname, ifcname);

  fprintf(context->file, "void %s__Initialize(void)\n{\n",
	  ifcname);
  fprintf(context->file, "  static int initialized = 0;\n");
  fprintf(context->file, "  if (initialized)\n    return;\n");
  fprintf(context->file, "  ilu::CheckStubConsistency(\"%s\", \"%s\", \"%s\");\n",
	  interface_name(interface), ILU_VERSION_STRING, ILU_TYPEUID_VERSION_STRING);
  fprintf(context->file, "  initialized = 1;\n\n");
  fprintf(context->file, "  ilu::EnterOTMu();\n");
  list_enumerate(interface->exceptions, DefineException, context);
  list_enumerate(interface->classes, RegisterClass, context);
  fprintf(context->file, "  ilu::ExitOTMu();\n");
  fprintf(context->file, "}\n\n");

  fprintf(context->file,
	  "_%s_RegistrationClass::_%s_RegistrationClass()\n{\n",
	  ifcname, ifcname);
  fprintf(context->file, "\t%s__Initialize();\n}\n\n", ifcname);
}

static void generate_sequence_code (Type seq, Context context)
{
  char *tn, *spn, *stn, *st, *srt;
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
    stn = cplusplus_simple_name(seq->name);
    spn = cplusplus_parameter_type(sequenceType, In);
    st = (type_ur_kind(sequenceType) == object_Type) ? spn : cplusplus_type_name(sequenceType);
    srt = cplusplus_return_type(sequenceType);

    fprintf (context->file, "_%s_sequence::_%s_sequence ()\n{\n", tn, tn);
    fprintf (context->file, "  _maximum = 0;\n  _length = 0;\n  _buffer = NULL;\n}\n\n");

    fprintf (context->file, "_%s_sequence::~_%s_sequence ()\n{\n", tn, tn);
    fprintf (context->file, "  if (_buffer != NULL) free((char *) _buffer);\n};\n\n");

    fprintf (context->file, "%s _%s_sequence::Create (ilu_Cardinal initial_size, %s *initial_data)\n{\n",
	     tn, tn, st);
    fprintf (context->file, "  %s s = new _%s_sequence;\n", tn, tn);
    fprintf (context->file, "  if (initial_data != NULL) {\n    s->_buffer = initial_data;\n");
    fprintf (context->file, "    s->_length = initial_size;\n    s->_maximum = initial_size;\n  }\n");
    fprintf (context->file, "  else if (initial_size > 0)\n    s->_buffer = (%s *) malloc((unsigned int)(sizeof(%s) * (s->_maximum = initial_size)));\n", st, st);
    fprintf (context->file, "  return s;\n}\n\n");

    fprintf (context->file, "void _%s_sequence::Clear (ilu_Boolean free_contents)\n{\n", tn);
    fprintf (context->file, "  if (_buffer != NULL && free_contents) {\n");
    fprintf (context->file, "    free((char *) _buffer);\n");
    fprintf (context->file, "    _maximum = 0;\n");
    fprintf (context->file, "    _buffer = (%s *) 0;\n", st);
    fprintf (context->file, "  }\n");
    fprintf (context->file, "  _length = 0;\n");
    fprintf (context->file, "}\n\n");
	
    fprintf (context->file, "void _%s_sequence::Append (%s item)\n{\n", tn, spn);
    fprintf (context->file, "  if (_buffer == NULL) {\n");
    fprintf (context->file, "    _buffer = (%s *) malloc ((unsigned int)(sizeof(%s) * (_maximum = 20)));\n  }\n", st, st);
    fprintf (context->file, "  else if (_maximum <= _length) {\n");
    fprintf (context->file, "    _maximum *= 2;\n");
    fprintf (context->file, "    _buffer = (%s *) realloc ((char *) _buffer, (unsigned int)(sizeof(%s) * _maximum));\n", st, st);
    fprintf (context->file, "  };\n");
    fprintf (context->file, "  _buffer[_length] = %sitem;\n", TypeIsNonObjectStruct(sequenceType) ? "*" : "");
    fprintf (context->file, "  _length += 1;\n");
    fprintf (context->file, "}\n\n");
	
    fprintf (context->file, "%s _%s_sequence::RemoveHead ()\n{\n", srt, tn);
    fprintf (context->file, "  static %s k;\n", st);
    fprintf (context->file, "  if (_length > 0) {\n");
    fprintf (context->file, "    k = _buffer[0];\n");
    fprintf (context->file, "    if (_length > 1)\n");
    fprintf (context->file, "      memmove (&_buffer[0], &_buffer[1], (int)((_length - 1) * sizeof(%s)));\n", st);
    fprintf (context->file, "    _length -= 1;\n");
    fprintf (context->file, "    return (%sk);\n", TypeIsNonObjectStruct(sequenceType) ? "&" : "");
    fprintf (context->file, "  }\n");
    fprintf (context->file, "  else\n    return ((%s) 0);\n", srt);
    fprintf (context->file, "}\n\n");

    fprintf (context->file, "%s _%s_sequence::RemoveTail ()\n{\n", srt, tn);
    fprintf (context->file, "  static %s k;\n", st);
    fprintf (context->file, "  if (_length > 0) {\n");
    fprintf (context->file, "    k = _buffer[_length-1];\n");
    fprintf (context->file, "    _length -= 1;\n");
    fprintf (context->file, "    return (%sk);\n", TypeIsNonObjectStruct(sequenceType) ? "&" : "");
    fprintf (context->file, "  }\n");
    fprintf (context->file, "  else\n    return ((%s) 0);\n", srt);
    fprintf (context->file, "}\n\n");

    fprintf (context->file, "ilu_Cardinal _%s_sequence::RemoveAll (ilu_Boolean (*matchproc)(%s, void *), void *arg)\n{\n", tn, spn);
    fprintf (context->file, "  ilu_Cardinal i, oldLength = _length;\n");
    fprintf (context->file, "  i = 0;\n");
    fprintf (context->file, "  while (i < _length)\n");
    fprintf (context->file, "    if ((*matchproc)(%s_buffer[i], arg)) {\n", TypeIsNonObjectStruct(sequenceType) ? "&" : "");
    fprintf (context->file, "      memmove (&_buffer[i], &_buffer[i + 1], (int)((_length - i - 1) * sizeof(%s)));\n", st);
    fprintf (context->file, "      _length -= 1;\n");
    fprintf (context->file, "    }\n");
    fprintf (context->file, "    else\n");
    fprintf (context->file, "      i += 1;\n");
    fprintf (context->file, "  return (oldLength - _length);\n");
    fprintf (context->file, "}\n\n");

    fprintf (context->file, "%s _%s_sequence::Find (ilu_Boolean (*matchproc)(%s, void *), void *arg)\n{\n", srt, tn, spn);
    fprintf (context->file, "  register int i;\n");
    fprintf (context->file, "  for (i = 0;  i < _length;  i += 1)\n");
    fprintf (context->file, "    if ((*matchproc)(%s_buffer[i], arg))\n", TypeIsNonObjectStruct(sequenceType) ? "&" : "");
    fprintf (context->file, "      return (%s_buffer[i]);\n", TypeIsNonObjectStruct(sequenceType) ? "&" : "");
    fprintf (context->file, "  return ((%s) 0);\n", srt);
    fprintf (context->file, "}\n\n");

    fprintf (context->file, "void _%s_sequence::Enumerate (void (*enumproc)(%s, void *), void *arg)\n{\n", tn, spn);
    fprintf (context->file, "  register int i;\n");
    fprintf (context->file, "  for (i = 0;  i < _length;  i += 1)\n");
    fprintf (context->file, "    (*enumproc)(%s_buffer[i], arg);\n", TypeIsNonObjectStruct(sequenceType) ? "&" : "");
    fprintf (context->file, "}\n\n");

    fprintf (context->file, "ilu_Cardinal _%s_sequence::Length ()\n{\n", tn);
    fprintf (context->file, "  return _length;\n");
    fprintf (context->file, "}\n\n");

    fprintf (context->file, "%s * _%s_sequence::Array ()\n{\n", st, tn);
    fprintf (context->file, "  return _buffer;\n");
    fprintf (context->file, "}\n\n");

    fprintf (context->file, "%s _%s_sequence::Nth (ilu_Cardinal index)\n{\n", srt, tn);
    fprintf (context->file, "  if (index < _length)\n");
    fprintf (context->file, "    return (%s_buffer[index]);\n", TypeIsNonObjectStruct(sequenceType) ? "&" : "");
    fprintf (context->file, "  else\n");
    fprintf (context->file, "    return ((%s) 0);\n", srt);
    fprintf (context->file, "}\n\n");
    }
}

/***********************************************************************\
*************************************************************************
**
**		Global class
**
*************************************************************************
\***********************************************************************/

void generate_code (Interface interface, FILE *file)
{
  struct context_s context;
  char*  pc_interfacename;

  context.file = file;
  context.interface = interface;

  fprintf (file, "#include <ilu.hh>\n");
  /* get any translation of what the header file for the interface is */
  pc_interfacename = interface_header_name(cplusplus_interface_name(interface));
 
  fprintf (file, "#include \"%s.hh\"\n\n", pc_interfacename);

  fprintf (file, "extern \"C\" {\n#include <stdio.h>\n#include <string.h>\n};\n");

  generate_global_code (interface, &context);

  list_enumerate (interface->types, (iluparser_EnumProc) generate_sequence_code, &context);
  list_enumerate (interface->classes, (iluparser_EnumProc) generate_class_code, &context);

  generate_registration_code (interface, &context);
}
