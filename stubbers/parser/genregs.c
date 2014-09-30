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

#include <stdio.h>
#include <stdlib.h>
#include <malloc.h>
#include <ctype.h>

#include <string.h>

#include <iluptype.h>

#define True TRUE
#define False FALSE

#ifdef _IS_POSIX
#include <unistd.h>	/* for unlink */
#include <errno.h>	/* for errno, ENOENT */
#endif /* _IS_POSIX */

#ifdef WIN32
/* for stat */
#include <sys/types.h>
#include <sys/stat.h>
#endif /* WIN32 */

static char *ProgramName = "c-stubber";

static void generateCBoilerplate (FILE *file, Interface parse)
{
  static char *prefixes[2] = { " *", " *" };

  fprintf (file, "/*\n");
  iluparser_GenerateBoilerplate (file, parse, ProgramName, prefixes);
  fprintf (file, " */\n\n");
}

static FILE *GenerateHeaderBeginning (char *filename, Interface i, char *suffix)
{
  FILE *file;
  char *namebuf;

  namebuf = malloc(strlen(filename) + strlen(suffix) + 1);
  strcpy (namebuf, filename);
  strcat (namebuf, suffix);

  if ((file = fopen(namebuf, "w")) == NIL)
    return NULL;
  return file;
}

static FILE *GenerateCodeBeginning (char *filename, Interface i, char *suffix)
{
  FILE *file;
  char *namebuf;

  namebuf = malloc(strlen(filename) + strlen(suffix) + 1);
  strcpy (namebuf, filename);
  strcat (namebuf, suffix);

  if ((file = fopen(namebuf, "w")) == NIL)
    return NULL;

  generateCBoilerplate(file, i);
  fprintf(file, "\n\n#include \"iluntrnl.h\"\n\n");
  fprintf(file, "\n\nvoid _ilu_RegisterBuiltInTypes(void)\n{\n");
  fprintf(file, "  ilu_Error lerr;\n  ilu_boolean newreg;\n\n");
  fprintf(file, "#ifdef ADD_TYPE_REGISTRATION_SUPPORT\n\n");
  return file;
}

static string
  TypeKindName (TypeKind tk)
{
  if (tk == byte_Type) {
    return "byte";
  } else if (tk == boolean_Type) {
    return "boolean";
  } else if (tk == character_Type) {
    return "character";
  } else if (tk == shortcharacter_Type) {
    return "shortcharacter";
  } else if (tk == shortinteger_Type) {
    return "shortinteger";
  } else if (tk == integer_Type) {
    return "integer";
  } else if (tk == longinteger_Type) {
    return "longinteger";
  } else if (tk == shortcardinal_Type) {
    return "shortcardinal";
  } else if (tk == cardinal_Type) {
    return "cardinal";
  } else if (tk == longcardinal_Type) {
    return "longcardinal";
  } else if (tk == real_Type) {
    return "real";
  } else if (tk == shortreal_Type) {
    return "shortreal";
  } else if (tk == longreal_Type) {
    return "longreal";
  } else if (tk == object_Type) {
    return "object";
  } else if (tk == pipe_Type) {
    return "pipe";
#ifndef ILU_REFERENCE_TYPES_ONLY
  } else if (tk == optional_Type) {
    return "optional";
#endif
  } else if (tk == reference_Type) {
    return "reference";
  } else if (tk == alias_Type) {
    return "alias";
  } else if (tk == union_Type) {
    return "union";
  } else if (tk == sequence_Type) {
    return "sequence";
  } else if (tk == record_Type) {
    return "record";
  } else if (tk == fixedpoint_Type) {
    return "fixedpoint";
  } else if (tk == string_Type) {
    return "string";
  } else if (tk == array_Type) {
    return "array";
  } else if (tk == enumeration_Type) {
    return "enumeration";
  } else if (tk == pickle_Type) {
    return "pickle";
  } else
    return NIL;
}

static void
  printDimension (unsigned long dim, FILE *file)
{
  fprintf(file, ", %lu", dim);
}

static void GenerateObjectRegistration (Type t, FILE *file)
{
  Class co;

  if (type_ur_kind(t) != object_Type) {
    fprintf(stderr, "Bad call to GenerateObjectRegistration with non-object type!\n");
    exit(1);
  };
  co = class_object(t);
  if (list_size(co->superclasses) > 0) {
    fprintf(stderr, "Can't handle object types in the `ilu' interface with supertypes!\n");
    exit(1);
  };
  if (list_size(co->methods) > 0) {
    fprintf(stderr, "Can't handle object types in the `ilu' interface with methods!\n");
    exit(1);
  };
#ifdef ILU_HTTPNG_OBJECTS
  if (list_size(co->state) > 0) {
    fprintf(stderr, "Can't handle object types in the `ilu' interface with state!\n");
    exit(1);
  };
#endif /* def ILU_HTTPNG_OBJECTS */
  /* first, generate the ilu_Class record */
  fprintf(file, "  {\n    ilu_Class ic;\n\n");
  fprintf(file, "    ic = ilu_DefineObjectType (");
  fprintf(file, "\"%s.%s\", ", interface_name(type_interface(t)), type_name(t));
  fprintf(file, "%s%s%s, ", (co->brand == NULL) ? "" : "\"",
	  (co->brand == NULL) ? "NIL" : co->brand,
	  (co->brand == NULL) ? "" : "\"");
  fprintf(file, "\"%s\", ", type_uid(t));
  fprintf(file, "%s%s%s, ", (co->singleton == NULL) ? "" : "\"",
	  (co->singleton == NULL) ? "NIL" : co->singleton,
	  (co->singleton == NULL) ? "" : "\"");
  fprintf(file, "%s, ", (co->optional) ? "ilu_TRUE" : "ilu_FALSE");
  fprintf(file, "%s, ", (co->collectible) ? "ilu_TRUE" : "ilu_FALSE");
  fprintf(file, "%s%s%s, ", (co->doc_string == NULL) ? "" : "\"",
	  (co->doc_string == NULL) ? "NIL" : co->doc_string,
	  (co->doc_string == NULL) ? "" : "\"");
  fprintf(file, "0, "); /* no methods */
  fprintf(file, "0, "); /* no supertypes */	  
  fprintf(file, "NIL,\n"); /* no supertypes */
  fprintf(file, "#ifdef ILU_HTTPNG_OBJECTS\n");
  fprintf(file, "0, "); /* no state */
  fprintf(file, "%s, ", (co->local) ? "ilu_TRUE" : "ilu_FALSE");
  fprintf(file, "%s,\n", (co->sealed) ? "ilu_TRUE" : "ilu_FALSE");
  fprintf(file, "#endif /* def ILU_HTTPNG_OBJECTS */\n");
  fprintf(file, "&lerr);\n");
  fprintf(file, "    if (ILU_ERRNOK(lerr)) goto fail1;\n");
  /* now register the type */
  fprintf(file, "    (void) ilu_RegisterObjectType(\"%s\", \"%s\", %s%s%s, \"%s\", ic, &newreg, &lerr);\n",
	  type_name(t), interface_name(type_interface(t)),
	  (type_interface(t)->brand == NULL) ? "" : "\"",
	  (type_interface(t)->brand == NULL) ? "NIL" : type_interface(t)->brand,
	  (type_interface(t)->brand == NULL) ? "" : "\"",
	  type_uid(t));
  fprintf(file, "    if (ILU_ERRNOK(lerr)) goto fail1;\n");
  fprintf(file, "  }\n");
}

static void OutputIntegerLiteral (FILE *file, IntegerLiteral lit)
{
  if (lit->negative)
    fprintf(file, "-");
  if (lit->small)
    fprintf(file, "%lu, ", lit->val.direct);
  else
    fprintf(file, "\"%s\", ", lit->val.string);
}

static void GenerateRegistration (Type t, FILE *file)
{
  if (t->importInterfaceName != NIL || t->marked)
    return;
  t->marked = TRUE;

  switch (type_kind(t))
    {
    case invalid_Type:
    case void_Type:
    case byte_Type:
    case boolean_Type:
    case character_Type:
    case shortcharacter_Type:
    case shortinteger_Type:
    case integer_Type:
    case longinteger_Type:
    case shortcardinal_Type:
    case cardinal_Type:
    case longcardinal_Type:
    case real_Type:
    case shortreal_Type:
    case longreal_Type:
    case pipe_Type:
    case pickle_Type:
      fprintf(file, "  (void) ilu_RegisterPrimitiveType(\"%s\", \"%s\", %s%s%s, \"%s\", ilu_%s_tk, &newreg, &lerr);\n",
	      type_name(t), interface_name(type_interface(t)),
	      (type_interface(t)->brand == NULL) ? "" : "\"",
	      (type_interface(t)->brand == NULL) ? "NIL" : type_interface(t)->brand,
	      (type_interface(t)->brand == NULL) ? "" : "\"",
	      type_uid(t), TypeKindName(type_kind(t)));
      fprintf(file, "  if (ILU_ERRNOK(lerr)) goto fail1;\n");
      break;

    case object_Type:
      GenerateObjectRegistration(t, file);
      break;

#ifndef ILU_REFERENCE_TYPES_ONLY
    case optional_Type:
      fprintf(file, "  (void) ilu_RegisterOptionalType(\"%s\", "
	      "\"%s\", %s%s%s, \"%s\",\n    \"%s\", /* base type */\n    &newreg, &lerr);\n",
	      type_name(t), interface_name(type_interface(t)),
	      (type_interface(t)->brand == NULL) ? "" : "\"",
	      (type_interface(t)->brand == NULL) ? "NIL" : type_interface(t)->brand,
	      (type_interface(t)->brand == NULL) ? "" : "\"",
	      type_uid(t),
	      type_uid(type_description(t)->structuredDes.optional));
      fprintf(file, "  if (ILU_ERRNOK(lerr)) goto fail1;\n");
      break;
#endif

    case reference_Type:
      fprintf(file, "  (void) ilu_RegisterReferenceType(\"%s\", "
	      "\"%s\", %s%s%s, \"%s\",\n"
	      "%s, /* aliased? */\n    %s, /* optional? */\n"
	      "    \"%s\", /* base type */\n    &newreg, &lerr);\n",
	      type_name(t), interface_name(type_interface(t)),
	      (type_interface(t)->brand == NULL) ? "" : "\"",
	      (type_interface(t)->brand == NULL) ? "NIL" : type_interface(t)->brand,
	      (type_interface(t)->brand == NULL) ? "" : "\"",
	      type_uid(t),
	      type_description(t)->structuredDes.reference.aliased ? "ilu_TRUE" : "ilu_FALSE",
	      type_description(t)->structuredDes.reference.optional ? "ilu_TRUE" : "ilu_FALSE",
	      type_uid(type_description(t)->structuredDes.reference.base_type));
      fprintf(file, "  if (ILU_ERRNOK(lerr)) goto fail1;\n");
      break;

    case alias_Type:
      fprintf(file, "  (void) ilu_RegisterAliasType(\"%s\", \"%s\", %s%s%s, \"%s\",\n"
	      "    \"%s\", /* base type */\n"
	      "    &newreg, &lerr);\n",
	      type_name(t), interface_name(type_interface(t)),
	      (type_interface(t)->brand == NULL) ? "" : "\"",
	      (type_interface(t)->brand == NULL) ? "NIL" : type_interface(t)->brand,
	      (type_interface(t)->brand == NULL) ? "" : "\"",
	      type_uid(t),
	      type_uid(ur_type(t)));
      fprintf(file, "  if (ILU_ERRNOK(lerr)) goto fail1;\n");
      break;

    case sequence_Type:
      fprintf(file, "  (void) ilu_RegisterSequenceType(\"%s\", "
	      "\"%s\", %s%s%s, \"%s\",\n"
	      "    \"%s\", /* base type of sequence */\n"
	      "    %lu, /* limit (0 for no limit) */\n"
	      "    &newreg, &lerr);\n",
	      type_name(t), interface_name(type_interface(t)),
	      (type_interface(t)->brand == NULL) ? "" : "\"",
	      (type_interface(t)->brand == NULL) ? "NIL" : type_interface(t)->brand,
	      (type_interface(t)->brand == NULL) ? "" : "\"",
	      type_uid(t),
	      type_uid(type_description(t)->structuredDes.sequence.type),
	      type_description(t)->structuredDes.sequence.limit);
      fprintf(file, "  if (ILU_ERRNOK(lerr)) goto fail1;\n");
      break;

    case array_Type:
      fprintf(file, "  {  /* array type \"%s\" */\n    ilu_cardinal dims[] = { %lu",
	      type_name(t),
	      (unsigned) list_car(type_description(t)->structuredDes.array.dimensions));
      list_enumerate(list_cdr(type_description(t)->structuredDes.array.dimensions),
		     (iluparser_EnumProc) printDimension, file);
      fprintf(file, " };\n    (void) ilu_RegisterArrayType(\"%s\", "
	      "\"%s\", %s%s%s, \"%s\",\n    \"%s\", /* base type of array */\n"
	      "    %lu, /* number of dimensions */\n"
	      "    dims, /* actual dimensions */\n"
	      "    &newreg, &lerr);\n",
	      type_name(t), interface_name(type_interface(t)),
	      (type_interface(t)->brand == NULL) ? "" : "\"",
	      (type_interface(t)->brand == NULL) ? "NIL" : type_interface(t)->brand,
	      (type_interface(t)->brand == NULL) ? "" : "\"",
	      type_uid(t),
	      type_uid(type_description(t)->structuredDes.array.type),
	      list_size(type_description(t)->structuredDes.array.dimensions));
      fprintf(file, "    if (ILU_ERRNOK(lerr)) goto fail1;\n  }\n");
      break;

    case record_Type:
      {
	cardinal i;
	Argument a;
	
	fprintf(file, "  { /* record type */\n"
		"    ilu_Type t = ilu_RegisterRecordType(\"%s\", \"%s\", %s%s%s, \"%s\",\n"
                "      %lu, /* number of fields in the record */\n"
		"      %s,	/* whether or not it is extensible */\n"
		"      %s%s%s,	/* supertype, if any */\n"
		"      &newreg, &lerr);\n",
		type_name(t), interface_name(type_interface(t)),
		(type_interface(t)->brand == NULL) ? "" : "\"",
		(type_interface(t)->brand == NULL) ? "NIL" : type_interface(t)->brand,
		(type_interface(t)->brand == NULL) ? "" : "\"",
		type_uid(t),
		list_size(type_description(t)->structuredDes.record.fields),
		type_description(t)->structuredDes.record.extensible ? "ilu_TRUE" : "ilu_FALSE",
		(type_description(t)->structuredDes.record.supertype == NULL) ? "" : "\"",
		(type_description(t)->structuredDes.record.supertype == NULL) ?
		  "ILU_NIL" : type_uid(type_description(t)->structuredDes.record.supertype),
		(type_description(t)->structuredDes.record.supertype == NULL) ? "" : "\"");
	fprintf(file, "    if (ILU_ERRNOK(lerr)) goto fail1;\n");
	fprintf(file, "    if (newreg) {\n");
	for (i = 0;  i < list_size(type_description(t)->structuredDes.record.fields);  i++)
	  {
	    a = (Argument) list_ref(type_description(t)->structuredDes.record.fields, i);
	    fprintf(file, "      ilu_RegisterRecordField(t, %lu, /* which field */\n"
		    "        \"%s\", /* field name */\n"
		    "        \"%s\", /* UID of field type */\n"
                    "        &lerr);\n", i, argument_name(a), type_uid(argument_type(a)));
	    fprintf(file, "      if (ILU_ERRNOK(lerr)) goto fail1;\n");
	  }	
	fprintf(file, "  };}\n");
      }
      break;

    case fixedpoint_Type:
      {
	fprintf(file, "  (void) ilu_RegisterFixedpointType(\"%s\", "
		"\"%s\", %s%s%s, \"%s\",\n      ",
		type_name(t), interface_name(type_interface(t)),
		(type_interface(t)->brand == NULL) ? "" : "\"",
		(type_interface(t)->brand == NULL) ? "NIL" : type_interface(t)->brand,
		(type_interface(t)->brand == NULL) ? "" : "\"",
		type_uid(t));
	OutputIntegerLiteral (file, type_description(t)->structuredDes.fixed.min_numerator);
	OutputIntegerLiteral (file, type_description(t)->structuredDes.fixed.max_numerator);
	OutputIntegerLiteral (file, type_description(t)->structuredDes.fixed.denominator);
	fprintf(file, "&newreg, &lerr);\n");
	fprintf(file, "  if (ILU_ERRNOK(lerr)) goto fail1;\n");
      }
      break;

    case string_Type:
      {
	fprintf(file, "  (void) ilu_RegisterStringType(\"%s\", "
		"\"%s\", %s%s%s, \"%s\",\n      ",
		type_name(t), interface_name(type_interface(t)),
		(type_interface(t)->brand == NULL) ? "" : "\"",
		(type_interface(t)->brand == NULL) ? "NIL" : type_interface(t)->brand,
		(type_interface(t)->brand == NULL) ? "" : "\"",
		type_uid(t));
	fprintf(file, "%lu, %lu, &newreg, &lerr);\n",
		type_description(t)->structuredDes.string.max_length,
		type_description(t)->structuredDes.string.charset);
	fprintf(file, "  if (ILU_ERRNOK(lerr)) goto fail1;\n");
      }
      break;

    case enumeration_Type:
      {
	cardinal i;
	EnumField f;
	
	fprintf(file, "  { /* enumeration */\n"
		"    ilu_Type t = ilu_RegisterEnumerationType(\"%s\", "
		"\"%s\", %s%s%s, \"%s\",\n"
		"      %lu, /* number of elements in the enum */\n"
		"      &newreg, &lerr);\n",
		type_name(t), interface_name(type_interface(t)),
		(type_interface(t)->brand == NULL) ? "" : "\"",
		(type_interface(t)->brand == NULL) ? "NIL" : type_interface(t)->brand,
		(type_interface(t)->brand == NULL) ? "" : "\"",
		type_uid(t),
		list_size(type_description(t)->structuredDes.enumeration));
	fprintf(file, "    if (ILU_ERRNOK(lerr)) goto fail1;\n");
	fprintf(file, "    if (newreg) {\n");
	for (i = 0;  i < list_size(type_description(t)->structuredDes.enumeration);  i++)
	  {
	    f = (EnumField) list_ref(type_description(t)->structuredDes.enumeration, i);
	    fprintf(file, "      ilu_RegisterEnumerationElement(t, %lu, /* which element */\n"
		    "    \"%s\", /* element name */\n"
		    "    %lu, /* integer value for element */\n"
		    "    &lerr);\n", i, f->name, f->id);
	    fprintf(file, "      if (ILU_ERRNOK(lerr)) goto fail1;\n");
	  }
	fprintf(file, "  };}\n");
      }
      break;

    case union_Type:
      {
	cardinal i, n, j, m;
	ConstantValue cv;
	Argument f;
	/* first figure out the index of the default arm */
	if (type_description(t)->structuredDes.uniond.default_arm != NULL)
	  {
	    n = list_size(type_description(t)->structuredDes.uniond.types);
	    for (i = 0;  i < n;  i++)
	      {
		if (((Argument) list_ref(type_description(t)->structuredDes.uniond.types, i)) ==
		     type_description(t)->structuredDes.uniond.default_arm)
		  break;
	      }
	    if (i < n)
	      n = i + 1;
	    else
	      n = 0;	/* probably an error */
	  }
	else
	  n = 0;	/* no default arm */
	/* now register the type */
	fprintf(file, "  { ilu_Type t = ilu_RegisterUnionType(\"%s\", "
		"\"%s\", %s%s%s, \"%s\",\n"
		"    \"%s\", /* UID of discriminant type */\n"
		"    %lu, /* number of arms */\n"
		"    %lu, /* default arm (0 for none) */\n"
		"    %s, /* invalid discriminant values allowed? (idiot CORBA) */\n"
		"    &newreg, &lerr);\n",
		type_name(t), interface_name(type_interface(t)),
		(type_interface(t)->brand == NULL) ? "" : "\"",
		(type_interface(t)->brand == NULL) ? "NIL" : type_interface(t)->brand,
		(type_interface(t)->brand == NULL) ? "" : "\"",
		type_uid(t),
		type_uid(type_description(t)->structuredDes.uniond.discriminator_type),
		list_size(type_description(t)->structuredDes.uniond.types), n,
		type_description(t)->structuredDes.uniond.others_allowed
		? "ilu_TRUE" : "ilu_FALSE");
	fprintf(file, "  if (ILU_ERRNOK(lerr)) goto fail1;\n");
	fprintf(file, "  if (newreg) {\n    ilu_UnionArmValueValue_u v;\n");
	fprintf(file, "    ilu_TypeKind tk;\n    ilu_UnionArm arm;\n\n");
	for (i = 0, n = list_size(type_description(t)->structuredDes.uniond.types);  i < n;  i++)
	  {
	    /* register the union arm */
	    f = (Argument) list_ref(type_description(t)->structuredDes.uniond.types, i);
	    m = list_size(f->values);
	    fprintf(file, "    arm = ilu_RegisterUnionArm (t, %lu, /* which arm */\n", i);
	    /* union arms may have NIL names, so check... */
	    if (argument_name(f) == NULL)
	      fprintf(file, "      ILU_NIL, /* no name for arm */\n");
	    else
	      fprintf(file, "      \"%s\", /* name of arm */\n", argument_name(f));
	    /* now finish the arguments to ilu_RegisterUnionArm */
	    fprintf(file, "      \"%s\", /* arm type */\n"
		    "      %lu, /* number of values that can select this arm */\n"
		    "      &lerr);\n",
		    type_uid(argument_type(f)), m);
	    fprintf(file, "    if (ILU_ERRNOK(lerr)) goto fail1;\n");
	    /* register the possible values for that arm */
	    for (j = 0;  j < m;  j++)
	      {
		cv = (ConstantValue) list_ref(f->values, j);
		switch (type_ur_kind(type_description(t)->structuredDes.uniond.discriminator_type))
		  {
		  case boolean_Type:
		    fprintf(file, "    v.boolean_value = %s;\n", cv->val.b ? "ilu_TRUE" : "ilu_FALSE");
		    fprintf(file, "    tk = ilu_boolean_tk;\n");
		    break;
		  case shortinteger_Type:
		    fprintf(file, "    v.shortinteger_value = %ld;\n", cv->val.i.value * cv->val.i.sign);
		    fprintf(file, "    tk = ilu_shortinteger_tk;\n");
		    break;
		  case integer_Type:
		    fprintf(file, "    v.integer_value = %ld;\n", cv->val.i.value * cv->val.i.sign);
		    fprintf(file, "    tk = ilu_integer_tk;\n");
		    break;
		  case shortcardinal_Type:
		    fprintf(file, "    v.shortcardinal_value = %lu;\n", cv->val.i.value);
		    fprintf(file, "    tk = ilu_shortcardinal_tk;\n");
		    break;
		  case cardinal_Type:
		    fprintf(file, "    v.cardinal_value = %lu;\n", cv->val.i.value);
		    fprintf(file, "    tk = ilu_cardinal_tk;\n");
		    break;
		  case byte_Type:
		    fprintf(file, "    v.byte_value = %lu;\n", cv->val.i.value);
		    fprintf(file, "    tk = ilu_byte_tk;\n");
		    break;
		  case enumeration_Type:
		    fprintf(file, "    v.enum_value = \"%s\";\n", cv->val.s);
		    fprintf(file, "    tk = ilu_shortcharacter_tk;\n");
		    break;
		  default:
		    fprintf(stderr, "Unexpected discriminant type encountered!\n");
		    exit(1);
		  }
		fprintf(file, "    ilu_RegisterUnionArmValue (arm, %lu, /* which arm value */\n"
			"      tk, /* kind of value */\n"
			"      v, /* actual value */\n"
			"      &lerr);\n", j);
		fprintf(file, "    if (ILU_ERRNOK(lerr)) goto fail1;\n");
	      }
	  }
	fprintf(file, "  };}\n");
      }
      break;
    }
}

static char * c_type_name (Type t)
{
  char *dup = ilu_strdup(type_name(t));
  char *p;

  for (p = dup;  *p != 0;  p++) {
    if (*p == '-')
      *p = '_';
  }
  return dup;
}

static void GenerateEnding (FILE *file)
{
  fprintf(file, "#endif /* ADD_TYPE_REGISTRATION_SUPPORT */\n\n");
  fprintf(file, "  return;\n");
  fprintf(file, " fail1:\n  _ilu_Assert(0, \"Bad registration of primitive ILU types\\n\");\n");
  fprintf(file, "}\n\n");
}

static void GenerateTypeIDConstants (Type t, FILE *file)
{
  if (t->importInterfaceName != NIL || t->marked)
    return;
  t->marked = TRUE;

  switch (type_kind(t))
    {
    case invalid_Type:
    case void_Type:
    case byte_Type:
    case boolean_Type:
    case character_Type:
    case shortcharacter_Type:
    case shortinteger_Type:
    case integer_Type:
    case longinteger_Type:
    case shortcardinal_Type:
    case cardinal_Type:
    case longcardinal_Type:
    case real_Type:
    case shortreal_Type:
    case longreal_Type:
    case pipe_Type:
    case pickle_Type:
      fprintf(file, "#define ILU_TYPEID_CONST_ilu_%s \"%s\"\n\n",
	      TypeKindName(type_kind(t)), type_uid(t));
      break;

    case object_Type:
#ifndef ILU_REFERENCE_TYPES_ONLY
    case optional_Type:
#endif
    case reference_Type:
    case alias_Type:
    case sequence_Type:
    case array_Type:
    case record_Type:
    case enumeration_Type:
    case union_Type:
      fprintf(file, "#define ILU_TYPEID_CONST_ilu_%s \"%s\"\n\n",
	      c_type_name(t), type_uid(t));
      break;

    default:
      break;
    }
}

static void GenerateTypeID (Type t, FILE *file)
{
  if (t->importInterfaceName != NIL || t->marked)
    return;
  t->marked = TRUE;

  switch (type_kind(t))
    {
    case invalid_Type:
    case void_Type:
    case byte_Type:
    case boolean_Type:
    case character_Type:
    case shortcharacter_Type:
    case shortinteger_Type:
    case integer_Type:
    case longinteger_Type:
    case shortcardinal_Type:
    case cardinal_Type:
    case longcardinal_Type:
    case real_Type:
    case shortreal_Type:
    case longreal_Type:
    case pipe_Type:
    case pickle_Type:
      fprintf(file, "const char ilu_TypeID_ilu_%s[] = ILU_TYPEID_CONST_ilu_%s;\n\n",
	      TypeKindName(type_kind(t)), TypeKindName(type_kind(t)));
      break;

    case object_Type:
#ifndef ILU_REFERENCE_TYPES_ONLY
    case optional_Type:
#endif
    case reference_Type:
    case alias_Type:
    case sequence_Type:
    case array_Type:
    case record_Type:
    case enumeration_Type:
    case union_Type:
      fprintf(file, "const char ilu_TypeID_ilu_%s[] = ILU_TYPEID_CONST_ilu_%s;\n\n",
	      c_type_name(t), c_type_name(t));
      break;

    default:
      break;
    }
}

int main(int ac, char **av, char **envp)
{
  list s;
  char **interfacename;
  FILE *file;
  Interface ilu;
  char *reg_file;

  if (ac < 2)
    {
      fprintf (stderr, "Must specify name of output files, usually ../../runtime/kernel/ilutpcod.{c,h}.\n");
      return 1;
    }
  else
    reg_file = av[1];

  if ((ProgramName = iluparser_GetProgramName(*av)) == NULL)
    ProgramName = "genregs";

  if ((s = ParseFile ("./ilu.isl")) == NULL)
    {
      fprintf (stderr, "Can't parse ILU interface description file \"ilu.isl\"!\n");
      return 1;
    }
  ilu = list_car(s);

  if ((file = GenerateHeaderBeginning(reg_file, ilu, ".h")) == NULL)
    {
      fprintf (stderr, "Can't open output file \"%s.h\"!\n", reg_file);
      return 1;
    }
  iluparser_ClearMarks();
  list_enumerate(ilu->types, (iluparser_EnumProc) GenerateTypeIDConstants, file);
  fclose(file);

  if ((file = GenerateCodeBeginning(reg_file, ilu, ".c")) == NULL)
    {
      fprintf (stderr, "Can't open output file \"%s.c\"!\n", reg_file);
      return 1;
    }
  iluparser_ClearMarks();
  list_enumerate(ilu->types, (iluparser_EnumProc) GenerateRegistration, file);
  GenerateEnding(file);

  iluparser_ClearMarks();
  list_enumerate(ilu->types, (iluparser_EnumProc) GenerateTypeID, file);

  fclose(file);

  return 0;
}

