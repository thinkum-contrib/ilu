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
#pragma segment common
#endif

#include <stdlib.h>	/* for exit() */

#include "scheme.h"

void UnmarkSupertypes (Type t)
{
  Type st;

  if (t == NULL || type_ur_kind(t) != object_Type)
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
	  || t == union_Type
	  || t == array_Type
	  || t == object_Type
	  || t == pipe_Type
	  || t == sequence_Type);
}

boolean TypeIsNonObjectStruct (Type type)
{
  enum PrimitiveTypes t;

  if (type == NULL)
    return (FALSE);

  t = type_ur_kind(type);
  return (t == record_Type || t == union_Type);
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
	  && (type_ur_kind(type_description(type)->structuredDes.sequence.type) == shortcharacter_Type));
}

static boolean FindComplexType (Argument a, void *junk)
{
  return (HasFreeRoutine(a->type));
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
    return (! (TypeIsString(type)));
  else if (type_ur_kind(type) == union_Type)
    {
      return (list_find (type_description(type)->structuredDes.uniond.types,
			 (iluparser_FindProc) FindComplexType, NULL) != NULL);
    }
  else if (type_ur_kind(type) == array_Type &&
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
  return scheme_interface_name (type->interface);
}

void MarshS(Context context, char *fmt,...)
{
  va_list         ap;
  fprintf(context->file, "\t(set! dsize ");
  va_start(ap, fmt);
  vfprintf(context->file, fmt, ap);
  va_end(ap);
  fprintf(context->file, ")\n");
  fprintf(context->file, "\t(set! argsize (+ argsize dsize))\n");
  fprintf(context->file, "\t(if (ilu:err-nok (ilu-call:error call))\n");
  fprintf(context->file, "\t  (throw 'fail))\n");
  return;
}

void SizeType (Type type, string name, Context context)
{
  enum PrimitiveTypes t = type_basic_type(type);
  TypeDescription d = type_description(type);

  if (t == alias_Type)
    SizeType(type->supertype, name, context);
  else if (t == integer_Type)
    MarshS(context, "(ilu:size-of-integer call %s)", name);
  else if (t == enumeration_Type)
    MarshS(context, "(ilu:size-of-enum call, %s)", name);
  else if (t == cardinal_Type)
    MarshS(context, "(ilu:size-of-cardinal call %s)", name);
  else if (t == shortinteger_Type)
    MarshS(context, "(ilu:size-of-short-integer call %s)", name);
  else if (t == shortcardinal_Type)
    MarshS(context, "(ilu:size-of-short-cardinal call %s)", name);
  else if (t == longinteger_Type)
    MarshS(context, "(ilu:size-of-long-integer call %s)", name);
  else if (t == longcardinal_Type)
    MarshS(context, "(ilu:size-of-long-cardinal call %s)", name);
  else if (t == character_Type)
    MarshS(context, "(ilu:size-of-character call %s)", name);
  else if (t == shortcharacter_Type)
    MarshS(context, "(ilu:size-of-byte call %s)", name);
  else if (t == real_Type)
    MarshS(context, "(ilu:size-of-real call %s)", name);
  else if (t == shortreal_Type)
    MarshS(context, "(ilu:size-of-short-real call %s)", name);
  else if (t == longreal_Type)
    MarshS(context, "(ilu:size-of-long-real call %s)", name);
  else if (t == boolean_Type)
    MarshS(context, "(ilu:size-of-boolean call %s)", name);
  else if (t == byte_Type)
    MarshS(context, "(ilu:size-of-byte call %s)", name);
  else if (t == optional_Type) {
    MarshS(context, "(ilu:size-of-optional call %s)", name);
    fprintf(context->file, "\t(if %s\n", name);
    fprintf(context->file, "\t  (begin\n");
    SizeType(d->structuredDes.optional, name, context);
    fprintf(context->file, "\t  ))\n");
  } else if (t == sequence_Type &&
	     (type_ur_kind(d->structuredDes.sequence.type)
	      == shortcharacter_Type))
    MarshS(context,
	   "(ilu:size-of-string call %s (length %s) %lu)",
	   name, name, d->structuredDes.sequence.limit);
  else if (t == array_Type
	   && type_ur_kind(d->structuredDes.array.type) == byte_Type
	   && list_size(d->structuredDes.array.dimensions) == 1)
    MarshS(context, "(ilu:size-of-opaque call %s %lu)", name,
	   (cardinal) list_car(d->structuredDes.array.dimensions));
  else if (t == array_Type
	   && (type_ur_kind(d->structuredDes.array.type) == shortcharacter_Type)
	   && list_size(d->structuredDes.array.dimensions) == 1)
    MarshS(context, "(ilu:size-of-string-vec call %s %lu)", name,
	   (cardinal) list_car(d->structuredDes.array.dimensions));
  else if (t == record_Type || t == union_Type || t == sequence_Type || t == pipe_Type)
    MarshS(context, "(%s:size-of-%s call %s)",
	   OriginalInterfaceNameOfType(type),
	   scheme_simple_name(type->name), name);
  else if (t == array_Type)
    MarshS(context, "(%s:size-of-%s call %s)", OriginalInterfaceNameOfType(type),
	   scheme_simple_name(type->name), name);
  else if (t == object_Type)
    MarshS(context,
    "(ilu-object:size-of-object call %s %s:class-record)",
	   name, scheme_type_name(type));
  else if (t == void_Type) {
  } else {
    fprintf(stderr,
	    "Error:  Can't figure size of argument of type %s (line %ld) yet.\n",
	    type_name(type), type->def);
    exit(1);
  }
}

static void ArgExpression (Argument arg, char *expr)
{
  int deref = FALSE;

#if 0
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
#endif
  sprintf(expr, "%s%s", deref ? "*" : "", scheme_argument_name(arg));
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
  va_start(ap, fmt);
  fprintf(context->file, "\t(if (not ");
  vfprintf(context->file, fmt, ap);
  fprintf(context->file, ")\n\t  (throw 'fail))\n");
  va_end(ap);
  return;
}

static void SimpleOut(Context context, string ty, string val)
{
  MarshO(context, "(ilu:output-%s call %s)", ty, val);
}

void EncodeValue (Type type, string name, Context context)
{
  enum PrimitiveTypes t = type_basic_type(type);
  TypeDescription d = type_description(type);

  if (t == integer_Type)
    SimpleOut(context, "integer", name);
  else if (t == enumeration_Type)
    SimpleOut(context, "enum", name);
  else if (t == cardinal_Type)
    SimpleOut(context, "cardinal", name);
  else if (t == shortinteger_Type)
    SimpleOut(context, "short-integer", name);
  else if (t == shortcardinal_Type)
    SimpleOut(context, "short-cardinal", name);
  else if (t == longinteger_Type)
    SimpleOut(context, "long-integer", name);
  else if (t == longcardinal_Type)
    SimpleOut(context, "long-cardinal", name);
  else if (t == character_Type)
    SimpleOut(context, "character", name);
  else if (t == shortcharacter_Type)
    SimpleOut(context, "byte", name);
  else if (t == real_Type)
    SimpleOut(context, "real", name);
  else if (t == shortreal_Type)
    SimpleOut(context, "short-real", name);
  else if (t == longreal_Type)
    SimpleOut(context, "long-real", name);
  else if (t == boolean_Type)
    SimpleOut(context, "boolean", name);
  else if (t == byte_Type)
    SimpleOut(context, "byte", name);
  else if (t == alias_Type)
    EncodeValue(type->supertype, name, context);
  else if (t == optional_Type) {
    MarshO(context, "(ilu:output-optional call %s)", name);
    fprintf(context->file, "\t(if %s\n", name);
    fprintf(context->file, "\t  (begin\n");
    EncodeValue(d->structuredDes.optional, name, context);
    fprintf(context->file, "\t  ) #f)\n");
  } else if (t == sequence_Type &&
	     (type_ur_kind(d->structuredDes.sequence.type)
	      == shortcharacter_Type)) {
    MarshO(context,
	   "(ilu:output-string call %s (length %s) %lu)",
	   name, name, d->structuredDes.sequence.limit);
  } else if (t == array_Type
	   && type_ur_kind(d->structuredDes.array.type) == byte_Type
	   && list_size(d->structuredDes.array.dimensions) == 1) {
    MarshO(context, "(ilu:output-opaque call %s %lu)", name,
	   (cardinal) list_car(d->structuredDes.array.dimensions));
  } else if (t == array_Type
	   && (type_ur_kind(d->structuredDes.array.type)
	       == shortcharacter_Type)
	   && list_size(d->structuredDes.array.dimensions) == 1) {
    MarshO(context,
	   "(ilu:output-string-vec call %s %lu)",
	   name,
	   (cardinal) list_car(d->structuredDes.array.dimensions));
  } else if (t == record_Type || t == union_Type || t == sequence_Type
	   || t == pipe_Type) {
    MarshO(context, "(%s:output-%s call %s)",
	   OriginalInterfaceNameOfType(type),
	   scheme_simple_name(type->name), name);
  } else if (t == array_Type) {
    MarshO(context, "(%s:output-%s call %s)",
	   OriginalInterfaceNameOfType(type),
	   scheme_simple_name(type->name), name);
  } else if (t == object_Type) {
    MarshO(context, "(ilu-object:output-object call %s %s:class-record)",
	   name, scheme_type_name(type));
  } else if (t == void_Type) {
  } else {
    fprintf(stderr,
	    "Error:  Can't cope with argument of type %s yet.\n",
	    scheme_type_name(type));
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
    fprintf (context->file, "%s (ilu:input-integer call))\n", buffer);
  else if (t == cardinal_Type)
    fprintf (context->file, "%s (ilu:input-cardinal call))\n", buffer);
  else if (t == shortinteger_Type)
    fprintf (context->file, "%s (ilu:input-short-integer call))\n", buffer);
  else if (t == shortcardinal_Type)
    fprintf (context->file, "%s (ilu:input-short-cardinal call))\n", buffer);
  else if (t == longinteger_Type)
    fprintf (context->file, "%s (ilu:input-long-integer call))\n", buffer);
  else if (t == longcardinal_Type)
    fprintf (context->file, "%s (ilu:input-long-cardinal call))\n", buffer);
  else if (t == character_Type)
    fprintf (context->file, "%s (ilu:input-character call))\n", buffer);
  else if (t == shortcharacter_Type)
    fprintf (context->file, "%s (ilu:input-byte call))\n", buffer);
  else if (t == real_Type)
    fprintf (context->file, "%s (ilu:input-real call))\n", buffer);
  else if (t == shortreal_Type)
    fprintf (context->file, "%s (ilu:input-short-real call))\n", buffer);
  else if (t == longreal_Type)
    fprintf (context->file, "%s (ilu:input-long-real call))\n", buffer);
  else if (t == boolean_Type)
    fprintf (context->file, "%s (ilu:input-boolean call))\n", buffer);
  else if (t == byte_Type)
    fprintf (context->file, "%s (ilu:input-byte call))\n", buffer);
  else if (t == alias_Type)
    UnpackValue (context, type->supertype, line, buffer, ref, allocate);
  else if (t == optional_Type)
    {
      char valLval[1000];
      Type valType = d->structuredDes.optional;
      enum PrimitiveTypes vpt = type_ur_kind (valType);
      int valRef = ! (vpt == object_Type || vpt == sequence_Type);
      int valScalar = vpt == byte_Type || vpt == boolean_Type ||
		      vpt == shortcharacter_Type || vpt == character_Type ||
		      vpt == shortinteger_Type || vpt == integer_Type || vpt == longinteger_Type ||
		      vpt == shortcardinal_Type || vpt == cardinal_Type || vpt == longcardinal_Type ||
		      vpt == shortreal_Type || vpt == real_Type || vpt == longreal_Type ||
		      vpt == enumeration_Type;

      fprintf (context->file, "%s", buffer);
      fprintf (context->file, "\t(let ((present (ilu:input-optional call)))\n");
      fprintf (context->file, "\t\t(if present\n");
      fprintf (context->file, "\t\t  (begin\n");
      UnpackValue (context, valType, line, "", valRef, TRUE);
      /*  fprintf (context->file, "\t\t  \n"); */
      fprintf (context->file, "\t\t  #f)))\n");
    }
  else if (t == enumeration_Type)
    fprintf (context->file, "\t%s (ilu:input-enum call))\n", buffer);
  else if (t == sequence_Type && type_ur_kind (d->structuredDes.sequence.type) == shortcharacter_Type)
    fprintf (context->file, "\t%s (ilu:input-string call %lu))\n",
	     buffer, d->structuredDes.sequence.limit);
  else if (t == array_Type
      && type_ur_kind (d->structuredDes.array.type) == byte_Type
      && list_size(d->structuredDes.array.dimensions) == 1)
    {
      fprintf (context->file, "\t%s (ilu:input-opaque call %lu))\n",
	       buffer, (cardinal) list_car(d->structuredDes.array.dimensions));
    }
  else if (t == array_Type
      && type_ur_kind (d->structuredDes.array.type) == shortcharacter_Type
      && list_size(d->structuredDes.array.dimensions) == 1)
    {
      fprintf (context->file, "\t%s (ilu:input-string-vec call %lu))\n",
	       buffer, (cardinal) list_car(d->structuredDes.array.dimensions));
    }
  else if (t == array_Type)
    {
	fprintf (context->file, "\t%s (%s:input-%s call))",
		 buffer, OriginalInterfaceNameOfType(type),
		 scheme_simple_name(type->name));
    }
  else if (t == sequence_Type || t == record_Type || t == union_Type)
    {
	fprintf (context->file, "\t%s (%s:input-%s call))",
		 buffer, OriginalInterfaceNameOfType(type),
		 scheme_simple_name(type->name));
    }
  else if (t == object_Type)
    fprintf (context->file, "\t%s (ilu-object:input-object call #f %s:class-record))\n",
	     buffer, scheme_type_name(type));
  else if (t == void_Type)
    ;
  else
    {
      fprintf (stderr, "Error:  Can't cope with procedure of type %s (line %ld) yet.\n",
	       scheme_type_name(type), line);
      exit (1);
    }
}

void UnpackOutParm(refany elt, refany rock)
{
  Argument        arg = (Argument) elt;
  Context         context = (Context) rock;
  if (arg->direction != In) {
    char buff[1000];
    sprintf(buff, "    (set! %s", scheme_argument_name(arg));
    UnpackValue(context, arg->type, 0, buff, FALSE, FALSE);
  }
}

void FreeValue (Type type, char *name, Context context)
{
  if (HasFreeRoutine(type))
    fprintf (context->file, "\t%s_G::Free_%s (%s);\n",
	     scheme_interface_name(type->interface),
	     scheme_simple_name(type->name),
	     name);
}     

boolean BlockingCall (Procedure proc)
{
  return (!(proc->asynch)
	  || type_ur_kind(proc->returnType) != void_Type
	  || list_size(proc->exceptions) > 0);
}



/**************************************
 * probably ok from here down...
 **************************************/
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
  char *s = scheme_string(interfacename);

  /* get any translation of what the header file for the interface is */
  pc_interfacename = interface_header_name(interfacename);

  if (pc_interfacename == interfacename)
	 pc_interfacename = s;

  fprintf (context->file, "(require \"%s\")\n", pc_interfacename);
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

static void ListArgument (Argument arg, Context context)
{
  fprintf (context->file, "-and-%s", scheme_argument_name(arg));
}

/* done */
static void DeclareMethodDefine (Procedure m, Context context)
{
  char* pname = class_procedure_name(m);

  fprintf (context->file, "(define %s:%s-method-num %d)\n",
	   scheme_type_name(context->class), pname, MethodRecordID);

  fprintf(context->file, "(if (not (defined? '%s))\n", pname);
  fprintf(context->file, "  (ilu-define-operation (%s this . status", pname);
  list_enumerate(m->arguments, (iluparser_EnumProc) ListArgument, context);
  fprintf(context->file, ")))\n");

  if (IsCacheableMethod(m)) {
    fprintf(context->file, "(if (not (defined? 'get-%s-cache-value))\n", pname);
    fprintf(context->file, "  (ilu-define-operation (get-%s-cache-value this)))\n", pname);
    fprintf(context->file, "(if (not (defined? 'set-%s-cache-value))\n", pname);
    fprintf(context->file, "  (ilu-define-operation (set-%s-cache-value this . val)))\n", pname);
    fprintf(context->file, "(if (not (defined? 'get-%s-cache-value-bound))\n", pname);
    fprintf(context->file, "  (ilu-define-operation (get-%s-cache-value-bound this)))\n", pname);
    fprintf(context->file, "(if (not (defined? 'set-%s-cache-value-bound))\n", pname);
    fprintf(context->file, "  (ilu-define-operation (set-%s-cache-value-bound this . b)))\n", pname);
  }

  MethodRecordID += 1;
}

/* done */
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

/* done */
void DefineMethods (Type t, Context c)
{
  c->class = t;
  MethodRecordID = 0;
  UnmarkSupertypes (t);
  DefineMethods2 (t, c);
  fprintf (c->file, "\n");
}
