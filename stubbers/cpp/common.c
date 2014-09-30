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
$Id: common.c,v 1.49 1999/08/03 01:50:57 janssen Exp $
*/
/* Last edited by Mike Spreitzer June 21, 1996 9:14 am PDT */

#ifdef MACOS
#pragma segment common
#endif

#include <stdlib.h>

#include "cplusplus.h"

void UnmarkSupertypes (Type t)
{
  Type st;

  if (t == NULL OR type_ur_kind(t) != object_Type)
    return;
  for (st = t;  st->supertype != NULL;  st = st->supertype)
    st->marked = FALSE;

  list_enumerate (class_object(st)->superclasses, (iluparser_EnumProc) UnmarkSupertypes, NULL);
  st->marked = FALSE;
}

boolean IsSequenceObject (Type type, Interface i)
{
  return ((i == NULL || type->interface == i)
	  && type->importInterfaceName == NULL
	  && type_ur_kind(type) == sequence_Type
	  && (type_ur_kind(type_description(type)->structuredDes.sequence.type) != shortcharacter_Type)
	  && (type_ur_kind(type_description(type)->structuredDes.sequence.type) != byte_Type));
}

boolean IsPipeObject (Type type, Interface i)
{
  return ((i == NULL || type->interface == i)
	  && type->importInterfaceName == NULL
	  && type_ur_kind(type) == pipe_Type);
}

boolean PassedByRef (Type type)
{
  enum PrimitiveTypes t;

  if (type == NULL)
    return (FALSE);

  t = type_ur_kind(type);
  return (t == record_Type
	  OR t == union_Type
	  OR t == array_Type
	  OR t == object_Type
	  OR t == pipe_Type
	  OR t == sequence_Type);
}

boolean TypeIsNonObjectStruct (Type type)
{
  enum PrimitiveTypes t;

  if (type == NULL)
    return (FALSE);

  t = type_ur_kind(type);
  return (t == record_Type OR t == union_Type);
}

boolean TypeIsArray (Type type)
{
  if (type == NULL)
    return (FALSE);

  return (type_ur_kind(type) == array_Type);
}

boolean TypeIsString (Type type)
{
  return (type_ur_kind(type) == sequence_Type
	  AND (type_ur_kind(type_description(type)->structuredDes.sequence.type) == shortcharacter_Type));
}

static boolean FindComplexType (Argument a, void *junk)
{
  return (HasFreeRoutine(a->type));
}

static boolean FindSameType (Argument type_in_the_list, void* pv_type_to_compare)
{
  return (type_in_the_list->type == ((Type)pv_type_to_compare));
}

boolean HasFreeRoutine (Type type)
{
  TypeKind tk = type_ur_kind(type);

  if (type_ur_kind(type) == record_Type)
    {
      return (list_find (type_description(type)->structuredDes.record.fields,
			 (iluparser_FindProc) FindComplexType, NULL) != NULL);
    }
  else if (type_ur_kind(type) == sequence_Type)
    return (NOT (TypeIsString(type)));
  else if (type_ur_kind(type) == union_Type)
    { 
      /* return true if the union has an arm of the same type, or if
	  one of the arm types is complex */
	  return ( (list_find (type_description(type)->structuredDes.uniond.types,
			 (iluparser_FindProc) FindSameType, type) != NULL) ||
		  (list_find (type_description(type)->structuredDes.uniond.types,
			 (iluparser_FindProc) FindComplexType, NULL) != NULL));
    }
  else if (type_ur_kind(type) == array_Type AND
	   (HasFreeRoutine(type_description(type)->structuredDes.array.type)))
    {
      return (TRUE);
    }
  else if (tk == optional_Type)
    {
      return (TRUE);
    }
  else
    return (FALSE);
}

static char * OriginalInterfaceNameOfType (Type type)
{
  if (type->supertype)
    return OriginalInterfaceNameOfType (type->supertype);
  return cplusplus_interface_name (type->interface);
}

static void OptionalValueExpr (TypeDescription d, string name, string valExpr)
{
  Type valType = d->structuredDes.optional;
  int deref;

  deref = NOT PassedByRef(valType) OR type_ur_kind(valType) == array_Type;
  sprintf (valExpr, "%s%s", deref ? "*" : "", name);
}

void MarshS(Context context, char *fmt,...)
{
  va_list         ap;
  fprintf(context->file, "\t_argSize += (_dSize = ");
  va_start(ap, fmt);
  vfprintf(context->file, fmt, ap);
  va_end(ap);
  fprintf(context->file, ");\n");
  fprintf(context->file, "\tif (ILU_ERRNOK(_call[0].err))\n");
  fprintf(context->file, "\t  goto faild;\n");
  return;
}

void SizeType (Type type, string name, Context context)
{
  enum PrimitiveTypes t = type_basic_type(type);
  TypeDescription d = type_description(type);

  if (t == alias_Type)
    SizeType(type->supertype, name, context);
  else if (t == integer_Type)
    MarshS(context, "ilu::SizeOfInteger(_call, %s)", name);
  else if (t == enumeration_Type)
    MarshS(context, "ilu::SizeOfEnum(_call, (ilu_ShortCardinal) %s)",
	   name);
  else if (t == cardinal_Type)
    MarshS(context, "ilu::SizeOfCardinal(_call, %s)", name);
  else if (t == shortinteger_Type)
    MarshS(context, "ilu::SizeOfShortInteger(_call, %s)", name);
  else if (t == shortcardinal_Type)
    MarshS(context, "ilu::SizeOfShortCardinal(_call, %s)", name);
  else if (t == longinteger_Type)
    MarshS(context, "ilu::SizeOfLongInteger(_call, %s)", name);
  else if (t == longcardinal_Type)
    MarshS(context, "ilu::SizeOfLongCardinal(_call, %s)", name);
  else if (t == character_Type)
    MarshS(context, "ilu::SizeOfCharacter(_call, %s)", name);
  else if (t == shortcharacter_Type)
    MarshS(context, "ilu::SizeOfByte(_call, %s)", name);
  else if (t == real_Type)
    MarshS(context, "ilu::SizeOfReal(_call, %s)", name);
  else if (t == shortreal_Type)
    MarshS(context, "ilu::SizeOfShortReal(_call, %s)", name);
  else if (t == longreal_Type)
    MarshS(context, "ilu::SizeOfLongReal(_call, %s)", name);
  else if (t == boolean_Type)
    MarshS(context, "ilu::SizeOfBoolean(_call, %s)", name);
  else if (t == byte_Type)
    MarshS(context, "ilu::SizeOfByte(_call, %s)", name);
  else if (t == optional_Type) {
    char            valExpr[1000];

    OptionalValueExpr(d, name, valExpr);
    MarshS(context, "ilu::SizeOfOptional(_call, (%s != NULL))", name);
    fprintf(context->file, "\tif (%s != NULL) {\n", name);
    SizeType(d->structuredDes.optional, valExpr, context);
    fprintf(context->file, "\t}\n");
  } else if (t == sequence_Type &&
	     (type_ur_kind(d->structuredDes.sequence.type)
	      == shortcharacter_Type))
    MarshS(context,
	   "ilu::SizeOfString(_call, %s, strlen(%s), %lu)",
	   name, name, d->structuredDes.sequence.limit);
  else if (t == array_Type
	   && type_ur_kind(d->structuredDes.array.type) == byte_Type
	   && list_size(d->structuredDes.array.dimensions) == 1)
    MarshS(context, "ilu::SizeOfOpaque(_call, %s, %lu)", name,
	   (cardinal) list_car(d->structuredDes.array.dimensions));
  else if (t == array_Type
	   && (type_ur_kind(d->structuredDes.array.type)
	       == shortcharacter_Type)
	   && list_size(d->structuredDes.array.dimensions) == 1)
    MarshS(context, "ilu::SizeOfStringVec(_call, %s, %lu)", name,
	   (cardinal) list_car(d->structuredDes.array.dimensions));
  else if (t == record_Type || t == union_Type || t == sequence_Type
	   || t == pipe_Type)
    MarshS(context, "%s_G::SizeOf_%s (_call, (%s) %s)",
	   OriginalInterfaceNameOfType(type),
	   cplusplus_simple_name(type->name),
	   cplusplus_parameter_type(type, In), name);
  else if (t == array_Type)
    MarshS(context, "%s_G::SizeOf_%s (_call, %s)",
	   OriginalInterfaceNameOfType(type),
	   cplusplus_simple_name(type->name), name);
  else if (t == pipe_Type)
    MarshS(context, "iluPipe_SizeOfObject (%s, _call, %s)",
	   cplusplus_type_name(type), name);
  else if (t == object_Type)
    MarshS(context,
    "iluObject::SizeOfObject (_call, (%s) %s, %s::ILUClassRecord)",
	   cplusplus_return_type(type),
	   name, cplusplus_type_name(type));
  else if (t == void_Type)
    fprintf(context->file, "\t_argSize += 0;\n");
  else {
    fprintf(stderr,
	    "Error:  Can't figure size of argument of type %s (line %ld) yet.\n",
	    type_name(type), type->def);
    exit(1);
  }
}

static void ArgExpression (Argument arg, char *expr)
{
  int deref;

  if (arg->direction == In)
    deref = FALSE;
  else
    {
      enum PrimitiveTypes bt = type_ur_kind(arg->type);

      /* objects, arrays, records, unions */
      if (bt == object_Type)
	deref = TRUE;

      /* arrays, records, unions */
      else if (bt == array_Type || TypeIsNonObjectStruct(arg->type))
	deref = FALSE;

      /* sequences */
      else if (bt == sequence_Type)
	{
	  TypeDescription d = type_description(arg->type);
	  Type elementType = d->structuredDes.sequence.type;
	  enum PrimitiveTypes ept = type_ur_kind(elementType);

	  if (ept == shortcharacter_Type || ept == byte_Type)
	    deref = TRUE;
	  else
	    deref = FALSE;
	}

      /* all others */
      else
	deref = TRUE;
    }
  sprintf(expr, "%s%s", deref ? "*" : "", cplusplus_argument_name(arg));
}

void SizeArgument (refany elt, refany rock)
{
  Argument        arg = (Argument) elt;
  Context         context = (Context) rock;
  if (arg->direction != Out) {
    char            argExpr[1000];
    ArgExpression(arg, argExpr);
    SizeType(arg->type, argExpr, context);
  }
}

void MarshO(Context context, char *fmt, ...)
{
  va_list         ap;
  fprintf(context->file, "\tif (!");
  va_start(ap, fmt);
  vfprintf(context->file, fmt, ap);
  va_end(ap);
  fprintf(context->file, ")\n\t  goto faild;\n");
  return;
}

static void SimpleOut(Context context, string ty, string cast, string val)
{
  MarshO(context, "ilu::Output%s(_call, %s %s)", ty, cast, val);
}

void EncodeValue (Type type, string name, Context context)
{
  enum PrimitiveTypes t = type_basic_type(type);
  TypeDescription d = type_description(type);

  if (t == integer_Type)
    SimpleOut(context, "Integer", "(ilu_Integer)", name);
  else if (t == enumeration_Type)
    SimpleOut(context, "Enum", "(unsigned short int)", name);
  else if (t == cardinal_Type)
    SimpleOut(context, "Cardinal", "(ilu_Cardinal)", name);
  else if (t == shortinteger_Type)
    SimpleOut(context, "ShortInteger", "(short int)", name);
  else if (t == shortcardinal_Type)
    SimpleOut(context, "ShortCardinal", "(unsigned short int)", name);
  else if (t == longinteger_Type)
    SimpleOut(context, "LongInteger", "(ilu_LongInteger)", name);
  else if (t == longcardinal_Type)
    SimpleOut(context, "LongCardinal", "(ilu_LongCardinal)", name);
  else if (t == character_Type)
    SimpleOut(context, "Character", "(ilu_Character)", name);
  else if (t == shortcharacter_Type)
    SimpleOut(context, "Byte", "(ilu_ShortCharacter)", name);
  else if (t == real_Type)
    SimpleOut(context, "Real", "(double)", name);
  else if (t == shortreal_Type)
    SimpleOut(context, "ShortReal", "(float)", name);
  else if (t == longreal_Type)
    SimpleOut(context, "LongReal", "(double)", name);
  else if (t == boolean_Type)
    SimpleOut(context, "Boolean", "(ilu_Boolean)", name);
  else if (t == byte_Type)
    SimpleOut(context, "Byte", "(unsigned char)", name);
  else if (t == alias_Type)
    EncodeValue(type->supertype, name, context);
  else if (t == optional_Type) {
    char            valExpr[1000];
    OptionalValueExpr(d, name, valExpr);
    MarshO(context, "ilu::OutputOptional (_call, (%s != NULL))",
	   name);
    fprintf(context->file, "\tif (%s != NULL)\n\t", name);
    EncodeValue(d->structuredDes.optional, valExpr, context);
  } else if (t == sequence_Type &&
	     (type_ur_kind(d->structuredDes.sequence.type)
	      == shortcharacter_Type))
    MarshO(context,
	   "ilu::OutputString (_call, %s, strlen(%s), %lu)",
	   name, name, d->structuredDes.sequence.limit);
  else if (t == array_Type
	   && type_ur_kind(d->structuredDes.array.type) == byte_Type
	   && list_size(d->structuredDes.array.dimensions) == 1)
    MarshO(context,
	   "ilu::OutputOpaque(_call, %s, %lu)",
	   name,
	   (cardinal) list_car(d->structuredDes.array.dimensions));
  else if (t == array_Type
	   && (type_ur_kind(d->structuredDes.array.type)
	       == shortcharacter_Type)
	   && list_size(d->structuredDes.array.dimensions) == 1)
    MarshO(context,
	   "ilu::OutputStringVec(_call, %s, %lu)",
	   name,
	   (cardinal) list_car(d->structuredDes.array.dimensions));
  else if (t == record_Type || t == union_Type || t == sequence_Type
	   || t == pipe_Type)
    MarshO(context, "%s_G::Output_%s (_call, %s)",
	   OriginalInterfaceNameOfType(type),
	   cplusplus_simple_name(type->name), name);
  else if (t == array_Type)
    MarshO(context, "%s_G::Output_%s (_call, %s)",
	   OriginalInterfaceNameOfType(type),
	   cplusplus_simple_name(type->name), name);
  else if (t == pipe_Type)
    MarshO(context, "iluPipe_OutputObject (%s, _call, %s)",
	   cplusplus_type_name(type),
	   "ilu_FALSE");
  else if (t == object_Type)
    MarshO(context,
	   "iluObject::OutputObject (_call, (%s) %s, %s::%s)",
	   cplusplus_return_type(type), name,
	   cplusplus_type_name(type), "ILUClassRecord");
  else if (t == void_Type);
  else {
    fprintf(stderr,
	    "Error:  Can't cope with argument of type %s yet.\n",
	    cplusplus_type_name(type));
    exit(1);
  }
}

void EncodeArgument (refany elt, refany rock)
{
  Argument        arg = (Argument) elt;
  Context         context = (Context) rock;
  if (arg->direction != Out)
    {
      char argExpr[1000];

      ArgExpression(arg, argExpr);
      EncodeValue (arg->type, argExpr, context);
    }
}

void UnpackValue (Context context, Type type, LineNumber line, string buffer, boolean ref, boolean allocate)
{
  enum PrimitiveTypes t = type_basic_type(type);
  TypeDescription d = type_description(type);

  if (t == integer_Type)
    fprintf (context->file, "\tilu::InputInteger (_call, %s%s);\n", ref ? "" : "&", buffer);
  else if (t == cardinal_Type)
    fprintf (context->file, "\tilu::InputCardinal (_call, %s%s);\n", ref ? "" : "&", buffer);
  else if (t == shortinteger_Type)
    fprintf (context->file, "\tilu::InputShortInteger (_call, %s%s);\n", ref ? "" : "&", buffer);
  else if (t == shortcardinal_Type)
    fprintf (context->file, "\tilu::InputShortCardinal (_call, %s%s);\n", ref ? "" : "&", buffer);
  else if (t == longinteger_Type)
    fprintf (context->file, "\tilu::InputLongInteger (_call, %s%s);\n", ref ? "" : "&", buffer);
  else if (t == longcardinal_Type)
    fprintf (context->file, "\tilu::InputLongCardinal (_call, %s%s);\n", ref ? "" : "&", buffer);
  else if (t == character_Type)
    fprintf (context->file, "\tilu::InputCharacter (_call, %s%s);\n", ref ? "" : "&", buffer);
  else if (t == shortcharacter_Type)
    fprintf (context->file, "\tilu::InputShortCharacter (_call, %s%s);\n", ref ? "" : "&", buffer);
  else if (t == real_Type)
    fprintf (context->file, "\tilu::InputReal (_call, %s%s);\n", ref ? "" : "&", buffer);
  else if (t == shortreal_Type)
    fprintf (context->file, "\tilu::InputShortReal (_call, %s%s);\n", ref ? "" : "&", buffer);
  else if (t == longreal_Type)
    fprintf (context->file, "\tilu::InputLongReal (_call, %s%s);\n", ref ? "" : "&", buffer);
  else if (t == boolean_Type)
    fprintf (context->file, "\tilu::InputBoolean (_call, %s%s);\n", ref ? "" : "&", buffer);
  else if (t == byte_Type)
    fprintf (context->file, "\tilu::InputByte (_call, %s%s);\n", ref ? "" : "&", buffer);
  else if (t == alias_Type)
    UnpackValue (context, type->supertype, line, buffer, ref, allocate);
  else if (t == optional_Type)
    {
      char valLval[1000];
      Type valType = d->structuredDes.optional;
      enum PrimitiveTypes vpt = type_ur_kind (valType);
      int valRef = NOT (vpt == object_Type OR vpt == sequence_Type);
      int valScalar = vpt == byte_Type OR vpt == boolean_Type OR
		      vpt == shortcharacter_Type OR vpt == character_Type OR
		      vpt == shortinteger_Type OR vpt == integer_Type OR vpt == longinteger_Type OR
		      vpt == shortcardinal_Type OR vpt == cardinal_Type OR vpt == longcardinal_Type OR
		      vpt == shortreal_Type OR vpt == real_Type OR vpt == longreal_Type OR
		      vpt == enumeration_Type;

      sprintf (valLval, "%s%s", ref ? "*" : "", buffer);
      fprintf (context->file, "\t{ ilu_Boolean _present; ilu::InputOptional (_call, &_present);\n");
      fprintf (context->file, "\t\tif (_present)");

      /* UnpackValue doesn't allocate storage for these, regardless of the
      ** value of its "allocate" parameter.
      */
      if (valScalar)
	fprintf (context->file, "\t%s = new %s,", valLval, cplusplus_type_name (valType));

      UnpackValue (context, valType, line, valLval, valRef, TRUE);
      fprintf (context->file, "\t\telse %s = NULL;\n", valLval);
      fprintf (context->file, "\t}\n");
    }
  else if (t == enumeration_Type)
    fprintf (context->file, "\t{ ilu_ShortCardinal _index; ilu::InputEnum (_call, &_index);  %s = (%s) _index; };\n",
	     buffer, cplusplus_type_name(type));
  else if (t == sequence_Type AND type_ur_kind (d->structuredDes.sequence.type) == shortcharacter_Type)
    fprintf (context->file, "\t%s%s = ilu::InputString(_call, NULL, NULL, %lu);\n",
	     ref ? "*" : "", buffer, d->structuredDes.sequence.limit);
  else if (t == array_Type
      AND type_ur_kind (d->structuredDes.array.type) == byte_Type
      AND list_size(d->structuredDes.array.dimensions) == 1)
    {
      if (allocate)
        fprintf (context->file, "\t%s%s = ilu::InputOpaque(_call, NULL, %lu);\n",
	     ref ? "*" : "", buffer, (cardinal) list_car(d->structuredDes.array.dimensions));
      else
	fprintf (context->file, "\tilu::InputOpaque (_call, %s%s, %lu);\n",
	     ref ? "" : "&", buffer, (cardinal) list_car(d->structuredDes.array.dimensions));
    }
  else if (t == array_Type
      AND type_ur_kind (d->structuredDes.array.type) == shortcharacter_Type
      AND list_size(d->structuredDes.array.dimensions) == 1)
    {
      if (allocate)
        fprintf (context->file, "\t%s%s = ilu::InputStringVec(_call, NULL, %lu);\n",
	     ref ? "*" : "", buffer, (cardinal) list_car(d->structuredDes.array.dimensions));
      else
        fprintf (context->file, "\tilu::InputStringVec(_call, %s%s, %lu);\n",
	     ref ? "" : "&", buffer, (cardinal) list_car(d->structuredDes.array.dimensions));
    }
  else if (t == array_Type)
    {
      if (allocate)
	fprintf (context->file, "\t%s = (%s *) %s_G::Input_%s (_call, NULL);\n",
		 buffer, cplusplus_type_name(d->structuredDes.array.type),
		 OriginalInterfaceNameOfType(type), cplusplus_simple_name(type->name));
      else
	fprintf (context->file, "\t%s_G::Input_%s (_call, (%s *) %s%s);\n",
		 OriginalInterfaceNameOfType(type), cplusplus_simple_name(type->name),
		 cplusplus_type_name(d->structuredDes.array.type), ref ? "" : "&", buffer);
    }
  else if (t == sequence_Type OR t == record_Type OR t == union_Type)
    {
      if (allocate OR (t == sequence_Type AND NOT ref))
	fprintf (context->file, "\t%s = %s_G::Input_%s (_call, NULL);\n",
		 buffer, OriginalInterfaceNameOfType(type), cplusplus_simple_name(type->name));
      else
	fprintf (context->file, "\t%s_G::Input_%s (_call, %s%s);\n",
		 OriginalInterfaceNameOfType(type), cplusplus_simple_name(type->name),
		 ref ? "" : "&", buffer);
    }
  else if (t == pipe_Type)
    fprintf (context->file, "\t%s%s = %s_iluPipe_InputObject(_call, %s);\n",
	     ref ? "*" : "", buffer, cplusplus_type_name(type), "ilu_FALSE");
  else if (t == object_Type)
    fprintf (context->file, "\t%s%s = (%s) iluObject::InputObject (_call, ilu_FALSE, %s::ILUClassRecord);\n",
	     ref ? "*" : "", buffer, cplusplus_return_type(type), cplusplus_type_name(type));
  else if (t == void_Type)
    ;
  else
    {
      fprintf (stderr, "Error:  Can't cope with procedure of type %s (line %ld) yet.\n",
	       cplusplus_type_name(type), line);
      exit (1);
    }
}

void UnpackOutParm(refany elt, refany rock)
{
  Argument        arg = (Argument) elt;
  Context         context = (Context) rock;
  if (arg->direction != In)
    UnpackValue(context, arg->type, 0, cplusplus_argument_name(arg),
		TRUE, FALSE);
}

void FreeValue (Type type, char *name, Context context)
{
  if (HasFreeRoutine(type))
    fprintf (context->file, "\t%s_G::Free_%s (%s);\n",
	     cplusplus_interface_name(type->interface),
	     cplusplus_simple_name(type->name),
	     name);
}     

boolean BlockingCall (Procedure proc)
{
  return (NOT(proc->asynch)
	  OR type_ur_kind(proc->returnType) != void_Type
	  OR list_size(proc->exceptions) > 0);
}

static list InterfacesToInclude = NULL;
static list TypesChecked = NULL;

static boolean matchString (char *s1, char *s2)
{
  return (strcmp(s1,s2) == 0);
}

static boolean matchPointer (char *s1, char *s2)
{
  return (s1 == s2);
}

static void GenerateIncludesForType (Type t, Context context);

static void GenerateIncludeForArgument (Argument a, Context context)
{
  GenerateIncludesForType (a->type, context);
}

static void GenerateIncludesForMethods (Procedure m, Context context)
{
  list_enumerate (m->arguments, (iluparser_EnumProc) GenerateIncludeForArgument, context);
  if (m->returnType != NULL && type_ur_kind(m->returnType) != void_Type)
    GenerateIncludesForType (m->returnType,context);
}

static void GenerateIncludesForType (Type t, Context context)
{
  if (t == NULL)
    return;

  if (list_find(TypesChecked, (iluparser_FindProc) matchPointer, t) != NULL)
    return;
  list_insert(TypesChecked, t);
  if (!t->builtIn && t->interface != NULL && name_base_name(t->interface->name) != NULL && (list_find(InterfacesToInclude, (iluparser_FindProc) matchString, name_base_name(t->interface->name)) == NULL))
    list_insert (InterfacesToInclude, name_base_name(t->interface->name));

  if (type_basic_type(t) == object_Type)
    {
      list_enumerate (type_description(t)->structuredDes.object->superclasses, (iluparser_EnumProc) GenerateIncludesForType, context);
      list_enumerate (type_description(t)->structuredDes.object->methods, (iluparser_EnumProc) GenerateIncludesForMethods, context);
    }
  else if (type_basic_type(t) == record_Type)
    {
      list_enumerate (type_description(t)->structuredDes.record.fields, (iluparser_EnumProc) GenerateIncludeForArgument, context);
    }
  else if (type_basic_type(t) == union_Type)
    {
      list_enumerate (type_description(t)->structuredDes.uniond.types, (iluparser_EnumProc) GenerateIncludeForArgument, context);
    }
  else if (type_basic_type(t) == sequence_Type)
    {
      GenerateIncludesForType (type_description(t)->structuredDes.sequence.type, context);
    }
  else if (type_basic_type(t) == array_Type)
    {
      GenerateIncludesForType (type_description(t)->structuredDes.array.type, context);
    }
}

static void PrintInclude (char *interfacename, Context context)
{
  char*  pc_interfacename;
 
	  char *s = cplusplus_string(interfacename);

  /* get any translation of what the header file for the interface is */
  pc_interfacename = interface_header_name(interfacename);

  if (pc_interfacename == interfacename)
	 pc_interfacename = s;

  fprintf (context->file, "#ifndef __%s_H_\n", s);
  fprintf (context->file, "#include \"%s.hh\"\n", pc_interfacename);
  fprintf (context->file, "#endif /* ndef __%s_H_ */\n\n", s);
  iluparser_Free(s);
}

static Interface CurrentInterface = NULL;

static void ListIncludes (Imported i, list l)
{
  Interface imported;

  if ((imported = GetInterface (i->name, i->filename)) != NULL
      && imported != CurrentInterface
      && (strcmp(i->name, "ilu") != 0)
      && (list_find(l, (iluparser_FindProc) matchString, name_base_name(imported->name)) == NULL))
    list_insert (l, name_base_name(imported->name));    
}

void GenerateNecessaryIncludes (Context context)
{
  if (InterfacesToInclude == NULL)
    InterfacesToInclude = (list) new_list();

  list_clear(InterfacesToInclude, FALSE);

  CurrentInterface = context->interface;
  list_enumerate (context->interface->imports, (iluparser_EnumProc) ListIncludes, InterfacesToInclude);

  list_enumerate (InterfacesToInclude, (iluparser_EnumProc) PrintInclude, context);
}

cardinal MethodRecordID;

static void DeclareMethodDefine (Procedure m, Context context)
{
  fprintf (context->file, "#define MethodRecord_%s_%s ((%s::ILUClassRecord)->cl_methods + %lu)\n",
	   cplusplus_type_name(context->class), class_procedure_name(m),
	   cplusplus_type_name(context->class), MethodRecordID);
  MethodRecordID += 1;
}

void DefineMethods2 (Type t, Context context)
{
  Class od;
  Type class;

  if (t == NULL || (class = ur_type(t)) == NULL || class->marked
      || type_basic_type(class) != object_Type || (od = class_object(class)) == NULL)
    return;

  class->marked = TRUE;

/*
  list_enumerate(od->superclasses, (iluparser_EnumProc) DefineMethods2, context);
*/

  list_enumerate (od->methods, (iluparser_EnumProc) DeclareMethodDefine, context);
}

void DefineMethods (Type t, Context c)
{
  c->class = t;
  MethodRecordID = 0;
  UnmarkSupertypes (t);
  DefineMethods2 (t, c);
  fprintf (c->file, "\n");
}
