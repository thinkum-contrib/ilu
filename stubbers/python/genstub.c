
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
$Id: genstub.c,v 1.107 1999/09/02 06:08:29 janssen Exp $
*/
/* Last edited by Mike Spreitzer September 17, 1998 2:52 pm PDT */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "iluptype.h"
#include "io.h"
#include "manifest.h"
#include "name.h"
#include "util.h"
#include "package.h"
#include "genstub.h"
#include "pythonversion.h"

/************************************************************************/

static void
stImported(Imported i)
{
	printImportIfc(i->name, FALSE);
}

/************************************************************************/

static void
printString(const char *s)
{
	int	i;

	putchar('\'');
	for (i = 0; s[i]; i++)
	{
		int	ch	= (unsigned char) s[i];

		if (ch < ' ' || '~' < ch)
			printf("\\%03o", ch);
		else
		{
			if (ch == '\'' || ch == '\\')
				putchar('\\');
			putchar(ch);
		}
	}
	putchar('\'');
}

static char *
pythonString (char *s)
{
  int l = strlen(s);
  char *out;
  register char *p, *q;

  out = (char *) malloc(l+1);
  for (p = s, q = out;  *p != 0;  p++)
    {
      if (*p == '-')
	*q++ = '_';
      else
	*q++ = *p;
    }
  *q = 0;
  return (out);  
}

static void
stConstantValue(ConstantValue v, Type t)
{
	switch (v->type)
	{
	case boolean_Type:
		printf("%d", v->val.b != 0);
		break;

	case integer_Type:
		if (v->val.i.value == 0x80000000 && v->val.i.sign < 0)
			printf("(-%lu - 1)", v->val.i.value - 1);
		else
			printf("%s%lu", v->val.i.sign < 0 ? "-" : "",
				v->val.i.value);
		break;

	case real_Type:
		printf("%s%s.%se%ld", v->val.r.sign < 0 ? "-" : "",
			v->val.r.value,
			v->val.r.fraction ? v->val.r.fraction : "0",
			v->val.r.exponent);
		break;

	case shortcharacter_Type:
		if (t == NULL)
		  printString(v->val.s);
		else
		  {
		    char *p = pythonString(v->val.s);
		    Type t2 = type_description(t)->structuredDes.uniond.discriminator_type;
		    if (t2->importInterfaceName != NULL)
		      printf ("%s.", getInterfaceName(ur_type(t2)->interface));
		    printf ("%s.%s", getTypeName(ur_type(t2)), p);
		    free(p);
		  }
		break;

	default:
		fatal("internal error: unexpected constant value (%d)",
			v->type);
	}
}

static void
stConstant(Constant c, int *pCount)
{
	char *	prefix	= "";
	char *	suffix	= "";
	char	buffer[128];

	if (c->import != 0)
		return;
	if (++*pCount == 1)
		newline();
	switch (type_basic_type(c->type))
	{
	case byte_Type:
	case boolean_Type:
	case character_Type:
	case shortcharacter_Type:
	case shortinteger_Type:
	case integer_Type:
	case shortcardinal_Type:
	case enumeration_Type:
		switch (c->value->type)
		{
		case real_Type:
			prefix = "int(";
			suffix = ")";
			break;

		case shortcharacter_Type:
			prefix = "ord(";
			suffix = ")";
			break;

		default:
			/* null */
			break;
		}
		break;

	case longinteger_Type:
	case cardinal_Type:
	case longcardinal_Type:
		switch (c->value->type)
		{
		case boolean_Type:
		case integer_Type:
			suffix = "L";
			break;

		case real_Type:
			prefix = "long(";
			suffix = ")";
			break;

		case shortcharacter_Type:
			prefix = "long(ord(";
			suffix = "))";
			break;

		default:
			/* null */
			break;
		}
		break;

	case real_Type:
	case shortreal_Type:
		switch (c->value->type)
		{
		case boolean_Type:
		case integer_Type:
			suffix = ".0";
			break;

		case shortcharacter_Type:
			prefix = "float(ord(";
			suffix = "))";
			break;

		default:
			/* null */
			break;
		}
		break;

	case longreal_Type:
		prefix = buffer;
		sprintf(prefix, "%s.%s(", nameModuleIlu,
			simpleTypeName(c->type));
		suffix = ")";
		switch (c->value->type)
		{
		case shortcharacter_Type:
			strcat(prefix, "ord(");
			suffix = "))";
			break;

		default:
			/* null */
			break;
		}
		break;

	default:
		/* null */
		break;
	}
	printf("%s = %s", getConstantName(c), prefix);
	stConstantValue(c->value, NULL);
	printf("%s\n", suffix);
}

static void
printTypeTable()
{
  printf ("__types__={}\n\n");
}

/************************************************************************/

static void
stDefineIoFunc(Type t, const char *prefix)
{
	newline();
	printf("def ");
	printTypeIoFuncName(t, prefix);
	printf("(%s", nameVarCall);
	if (prefix != prefixFuncInput)
		printf(", %s", nameVarValue);
	printf("):\n");
}

static void
stRegisterIoFuncs(Type t)
{
  printf ("%s.RegisterIoFuncs('%s', ",
	  nameModuleIlu, type_uid(t));
  printTypeIoFuncName(t, prefixFuncInput);
  printf (", ");
  printTypeIoFuncName(t, prefixFuncOutput);
  printf (", ");
  printTypeIoFuncName(t, prefixFuncSizeOf);
  printf (", '%s.%s')\n", interface_name(type_interface(t)), type_name(t));
}

static void
stRegisterEnumIoFuncs(Type t)
{
  printf ("%s.RegisterIoFuncs('%s', %s.%sEnum, %s.%sEnum, %s.%sEnum, '%s.%s')\n",
	  nameModuleIlu, type_uid(t),
	  nameModuleIlu, prefixFuncInput,
	  nameModuleIlu, prefixFuncOutput,
	  nameModuleIlu, prefixFuncSizeOf,
	  interface_name(type_interface(t)), type_name(t));
}

/************************************/

static void
stDefineFixedPointLiterals (Type t)
{
  TypeDescription d = type_description(t);
  printf ("\t_denom = ");
  if (d->structuredDes.fixed.denominator->negative)
    printf ("-");
  if (d->structuredDes.fixed.denominator->small)
    printf ("%lu", d->structuredDes.fixed.denominator->val.direct);
  else
    printf ("%sL", d->structuredDes.fixed.denominator->val.direct);
  newline();
  printf ("\t_ilu_bignum_value_for_denom = %s.KernelBignumForValue (_denom)\n",
	  nameModuleIlu);

  printf ("\t_min_numerator = ");
  if (d->structuredDes.fixed.min_numerator == NULL)
    printf ("None");
  else {
    if (d->structuredDes.fixed.min_numerator->negative)
      printf ("-");
    if (d->structuredDes.fixed.min_numerator->small)
      printf ("%lu", d->structuredDes.fixed.min_numerator->val.direct);
    else
      printf ("%sL", d->structuredDes.fixed.min_numerator->val.direct);
  }
  newline();
  if (d->structuredDes.fixed.min_numerator == NULL)
    printf ("\t_ilu_bignum_value_for_min_num = None\n");
  else
    printf ("\t_ilu_bignum_value_for_min_num = %s.KernelBignumForValue (_min_numerator)\n",
	    nameModuleIlu);

  printf ("\t_max_numerator = ");
  if (d->structuredDes.fixed.max_numerator == NULL)
    printf ("None");
  else {
    if (d->structuredDes.fixed.max_numerator->negative)
      printf ("-");
    if (d->structuredDes.fixed.max_numerator->small)
      printf ("%lu", d->structuredDes.fixed.max_numerator->val.direct);
    else
      printf ("%sL", d->structuredDes.fixed.max_numerator->val.direct);
  }
  newline();
  if (d->structuredDes.fixed.max_numerator == NULL)
    printf ("\t_ilu_bignum_value_for_max_num = None\n");
  else
    printf ("\t_ilu_bignum_value_for_max_num = %s.KernelBignumForValue (_max_numerator)\n",
	    nameModuleIlu);

  printf ("\t_size = %s.FixedPointTypeSize(_min_numerator, _max_numerator)\n",
	  nameModuleIlu);
  printf ("\t_corba_fixed_digits = %lu\n", d->structuredDes.fixed.fixed_digits);
  printf ("\t_corba_fixed_decimal_places = %lu\n", d->structuredDes.fixed.fixed_decimal_places);
  newline();
}

static void
  stDefineFixedPointInit (Type t)
{
  char *name = getTypeName(t);
  TypeDescription d = type_description(t);

  printf("\tdef __init__(self, value):\n");
  if (d->structuredDes.fixed.min_numerator != NULL)
    printf ("\t\tif (value < %s._min_numerator):\n"
	    "\t\t\traise %s.BelowFixedPointMinNumerator, value\n", name, nameModuleIlu);
  if (d->structuredDes.fixed.max_numerator != NULL)
    printf ("\t\tif (value > %s._max_numerator):\n"
	    "\t\t\traise %s.AboveFixedPointMaxNumerator, value\n", name, nameModuleIlu);
  printf ("\t\tself.numerator = value\n\n");
}

static void
  stDefineInputFunction(Type t)
{
  printf ("_%s_%s = lambda call, type=%s: %s.InputFixedPoint(call, type)\n\n",
	  prefixFuncInput, getTypeName(t), getTypeName(t), nameModuleIlu);
}

static void
  stRegisterFixedPointIOFuncs (Type t)
{
  printf ("%s.RegisterIoFuncs('%s', _%s_%s, %s.%sFixedPoint, %s.%sFixedPoint, '%s.%s')\n",
	  nameModuleIlu, type_uid(t),
	  prefixFuncInput, getTypeName(t),
	  nameModuleIlu, prefixFuncOutput,
	  nameModuleIlu, prefixFuncSizeOf,
	  interface_name(type_interface(t)), type_name(t));
}

static void
stFixedpointTypeIO(Type t)
{
  stDefineInputFunction(t);
}

static void
stFixedpointTypeClass(Type t)
{
  newline();
  printf ("class %s (%s.FixedPointType):\n", getSimpleTypeName(t), nameModuleIlu);

  stDefineFixedPointLiterals(t);
  stDefineFixedPointInit(t);
}

static void
stFixedpointTypeReg(Type t)
{
  stRegisterFixedPointIOFuncs(t);

#ifdef ADD_VARIANT_SUPPORT

#if 0
  if (type_kind(t) != alias_Type)
    stFixedPointRegisterType(t);
#endif

#endif /* ADD_VARIANT_SUPPORT */
}

/************************************/

static void
stDefineEnumFieldLiteral(EnumField e, char *prefix)
{
	char	fieldName[1024];

	getEnumFieldName(e, fieldName);
	if (prefix != NULL)
	  printf("%s%s = %d;\n", prefix, fieldName, e->id);
	else
	  printf("    %s = %d;\n", fieldName, e->id);
}

static void
stDefineEnumLiterals(Type t)
{
	TypeDescription	d	= type_description(t);
	list		fields	= d->structuredDes.enumeration;

	newline();
	list_enumerate(fields, (EnumProc) stDefineEnumFieldLiteral, NULL);
}

static void
stEnumFieldCase(EnumField e, int *count)
{
	char	fieldName[1024];

	if (++(*count) > 1)
		printf(",");
	printf("\n");
	printf("  ");
	getEnumFieldName(e, fieldName);
	printf("        %s : '%s'", fieldName, fieldName);
}

static void
stDefineEnumImageDict(Type t)
{
	TypeDescription	d	= type_description(t);
	list		fields	= d->structuredDes.enumeration;
	int count = 0;

	printf("    __image__ = {");
	list_enumerate(fields, (EnumProc) stEnumFieldCase, &count);
	printf("}\n");
}

static void
stEnumerationRegisterType(Type t)
{
  list fields = type_description(t)->structuredDes.enumeration;
  EnumField field;
  int i;

  newline();
#ifdef ADD_TYPE_REGISTRATION_SUPPORT
  printf ("__types__['%s'] = %s.RegisterEnumerationType(",
	  getTypeName(t), nameModuleIlu);
  printNameScopes(t->scoping);
  printf (", %s%s%s, '%s', (",
	  (type_interface(t)->brand == NULL) ? "" : "\"\"\"",
	  (type_interface(t)->brand == NULL) ? "None" : type_interface(t)->brand,
	  (type_interface(t)->brand == NULL) ? "" : "\"\"\"",
	  type_uid(t));
  for (i = 0;  i < list_size(fields);  i++) {
    field = list_ref(fields, i);
    printf ("%s('%s', %d)",
	    (i == 0) ? "" : ", ", field->name, field->id);
  }
  /* Correct syntax for Python tuple with 1 item is "(item,)" */
  printf ("%s))\n", i == 1 ? "," : "");
#endif /* ADD_TYPE_REGISTRATION_SUPPORT */
  stRegisterEnumIoFuncs(t);
}

static void
stEnumerationTypeClass(Type t)
{
  newline();
  printf ("class %s:", getSimpleTypeName(t));
  stDefineEnumLiterals(t);
  stDefineEnumImageDict(t);

#ifdef ILU_CORBA_PYTHON_MAPPING
  list_enumerate(type_description(t)->structuredDes.enumeration,
		 (EnumProc) stDefineEnumFieldLiteral, "");
#endif /* def ILU_CORBA_PYTHON_MAPPING */
}

static void
stEnumerationTypeReg(Type t)
{
#ifdef ADD_VARIANT_SUPPORT

  if (type_kind(t) != alias_Type)
    stEnumerationRegisterType(t);

#endif /* ADD_VARIANT_SUPPORT */
}

/************************************/

static void
stRecordFieldInput(Argument f)
{
	printf("    %s.%s = ", nameVarValue, getArgumentName(f));
	ioTypeInput(f->type);
	newline();
}

static void
printNone (Argument a, int *mcount)
{
  printf ("%sNone", (*mcount > 0) ? ", " : "");
  *mcount += 1;
}

static void enumerateRecordFields (Type t, EnumProc p, void * rock)
{
  if (type_description(t)->structuredDes.record.supertype != NULL)
    enumerateRecordFields(type_description(t)->structuredDes.record.supertype, p, rock);
  list_enumerate(type_description(t)->structuredDes.record.fields, p, rock);
}

static void
stDefineRecordTypeInput(Type t)
{
	TypeDescription	d	= t->description;
	int count = 0;

	stDefineIoFunc(t, prefixFuncInput);
	printf("    %s.%sRecord(%s)\n", nameModuleIlu,
		prefixFuncInput, nameVarCall);
	printf("    %s = %s (", nameVarValue, getScopedTypeName(t));
	enumerateRecordFields (t, (EnumProc) printNone, &count);
	printf(")\n");
	enumerateRecordFields (t, (EnumProc) stRecordFieldInput, NULL);
	printf("    %s.%sRecord(%s)\n", nameModuleIlu,
		prefixFuncEnd, nameVarCall);
	printf("    return %s\n", nameVarValue);
}

static void
stRecordFieldOutSize(Argument f, const char *prefix)
{
	char	argName[1024];

	if (prefix == prefixFuncSizeOf) {
	  printf("\\\n");
	  printf("      + ");
	} else {
	  printf("\n");
	  printf("    ");
	}
	sprintf(argName, "%s['%s']", nameVarValue, getArgumentName(f));
	ioTypeOutSize(f->type, argName, prefix);
}

static void
stDefineRecordTypeOutSize(Type t, const char *prefix)
{
	TypeDescription	d	= t->description;

	stDefineIoFunc(t, prefix);
	if (prefix == prefixFuncSizeOf)
		printf("    %s = ", nameVarSize);
	else
		printf("    ");
	printf("%s.%sRecord(%s)", nameModuleIlu, prefix, nameVarCall);
	enumerateRecordFields (t, (EnumProc) stRecordFieldOutSize,
			       (void *) prefix);
	printf("\n");
	printf("    %s.%sRecord(%s)\n", nameModuleIlu,
	       prefixFuncEnd, nameVarCall);
	if (prefix == prefixFuncSizeOf)
		printf("    return %s\n", nameVarSize);
}

static void
listFieldParam (Argument f, char *prefix)
{
  printf ("%s%s", prefix, getArgumentName(f));
}

static void
listPickleFieldParam (Argument f, char *junk)
{
  printf ("self.%s, ", getArgumentName(f));
}

static void
initRecordField (Argument f, void *junk)
{
  listFieldParam (f, "        self.");
  listFieldParam (f, " = _arg_");
  printf (";\n");
}

static void
stDefineRecordTypeClass(Type t)
{
  TypeDescription	d	= t->description;
  Type			supertype = d->structuredDes.record.supertype;

  newline();
#ifdef ILU_CORBA_PYTHON_MAPPING
  if (!isPrefixOf(prefixIdlExcType, type_name(t)))
    printf ("class %s (", getSimpleTypeName(t));
  else
#endif
    printf ("class %s (", getTypeName(t));
  if (supertype != NULL) {
    if (t->interface != supertype->interface)
      printf ("%s.", getInterfaceName(supertype->interface));
    printf("%s", getTypeName(supertype));
  } else {
    printf ("%s.IluRecord", nameModuleIlu);
  }
#ifdef ILU_CORBA_PYTHON_MAPPING
  if (isPrefixOf(prefixIdlExcType, type_name(t)))
    printf (", %s['%s'].%s", nameVarImports, nameModuleCORBA, nameUserException);
#endif
  printf ("):\n");
  printf ("    __ilu_type_name__ = '%s.%s'\n",
	  interface_name(t->interface), type_name(t));
  printf ("    def __init__(self");
  enumerateRecordFields (t, (EnumProc) listFieldParam, ", _arg_");
  printf ("):\n");
  enumerateRecordFields (t, (EnumProc) initRecordField, 0);
  printf ("\n");
  printf ("    def __getinitargs__(self):\n");
  printf ("      return (");
  enumerateRecordFields (t, (EnumProc) listPickleFieldParam, 0);
  printf (")");
  printf ("\n");
}

static void
stRecordRegisterType(Type t)
{
  list fields = type_description(t)->structuredDes.record.fields;
  Argument field;
  int i;

  newline();
#ifdef ADD_TYPE_REGISTRATION_SUPPORT
  printf ("__types__['%s'] = %s.RegisterRecordType(",
	  getTypeName(t), nameModuleIlu);
  printNameScopes(t->scoping);
  printf (", %s%s%s, '%s', (",
	  (type_interface(t)->brand == NULL) ? "" : "\"\"\"",
	  (type_interface(t)->brand == NULL) ? "None" : type_interface(t)->brand,
	  (type_interface(t)->brand == NULL) ? "" : "\"\"\"",
	  type_uid(t));
  for (i = 0;  i < list_size(fields);  i++) {
    field = list_ref(fields, i);
    printf ("('%s', '%s'),",
	    argument_name(field), type_uid(argument_type(field)));
  }
  printf ("), %d, ", type_description(t)->structuredDes.record.extensible);
  if (type_description(t)->structuredDes.record.supertype != NULL)
    printf ("\"%s\")\n", type_uid(type_description(t)->structuredDes.record.supertype));
  else
    printf ("None)\n");
#endif /* ADD_TYPE_REGISTRATION_SUPPORT */
  stRegisterIoFuncs(t);
}

static void stRecordTypeClass(Type t, boolean);

static void
  stRecordSupertype (Type t, boolean do_exception_types)
{
  Type st;

  if (type_description(t)->structuredDes.record.supertype != NULL) {
    st = ur_type(type_description(t)->structuredDes.record.supertype);
    if ((st->interface == t->interface) && (st->importInterfaceName == NULL))
      stRecordTypeClass(st, do_exception_types);
  }
}

static void
stRecordTypeIO(Type t)
{
  stDefineRecordTypeInput(t);
  stDefineRecordTypeOutSize(t, prefixFuncOutput);
  stDefineRecordTypeOutSize(t, prefixFuncSizeOf);
}

static void
stRecordTypeClass(Type t, boolean do_exception_types)
{
  if (t->marked) return;

#ifdef ILU_CORBA_PYTHON_MAPPING
  if (isPrefixOf(prefixIdlExcType, type_name(t)) && (!do_exception_types))
    return;    
#endif

  stRecordSupertype(t, do_exception_types);
  stDefineRecordTypeClass(t);
  t->marked = TRUE;
}

static void
stRecordTypeReg(Type t)
{
#ifdef ADD_VARIANT_SUPPORT

  stRecordRegisterType(t);

#endif /* ADD_VARIANT_SUPPORT */
}

/************************************/

typedef struct
{
	Argument	defaultArm;
	Type		unionType;
	const char *	prefix;
	int		count;
} DutRock;

static void
stUnionCaseHeader(Argument f, DutRock *dr)
{
	int	nValues	= list_size(f->values);

	printf("    %sif %s", dr->count == 1 ? "" : "el", nameVarDiscrim);
	if (nValues == 1)
	{
		printf(" == ");
		stConstantValue(list_ref(f->values, 0), dr->unionType);
	}
	else
	{
		int	i;

		printf(" in (");
		for (i = 0; i < nValues; i++)
		{
			if (i > 0)
				printf(", ");
			stConstantValue(list_ref(f->values, i), dr->unionType);
		}
		printf(")");
	}
	printf(":\n");
}

static void
stUnionCaseBody(Argument f, DutRock *dr)
{
  printf("\t");
  if (dr->prefix == prefixFuncInput)
    {
#ifdef ILU_CORBA_PYTHON_MAPPING
      printf("%s = %s(%s, ", nameVarValue, getTypeName(dr->unionType), nameVarDiscrim);
#else
      printf("%s = (%s, ", nameVarValue, nameVarDiscrim);
#endif /* def ILU_CORBA_PYTHON_MAPPING */
      ioTypeInput(f->type);
      printf(")\n");
    }
  else
    {
      char	argName[64];

      if (dr->prefix == prefixFuncSizeOf)
	printf("%s = %s + ", nameVarSize, nameVarSize);
      sprintf(argName, "%s", nameVarValueValue);
      ioTypeOutSize(f->type, argName, dr->prefix);
      newline();
    }
}

static void
stUnionFieldInputOutSize(Argument f, DutRock *r)
{
	if (f != r->defaultArm)
	{
	  ++r->count;
	  stUnionCaseHeader(f, r);
	  stUnionCaseBody(f, r);
	}
}

static char *
unionDiscType(Type t)
{
  switch (type_ur_kind(t))
    {
    case cardinal_Type:
      return "cardinal";
    case integer_Type:
      return "integer";
    case enumeration_Type:
      return "enumeration";
    case shortcardinal_Type:
      return "shortcardinal";
    case shortinteger_Type:
      return "shortinteger";
    case character_Type:
      return "character";
    case boolean_Type:
      return "boolean";
    case byte_Type:
      return "byte";
    case shortcharacter_Type:
      return "shortcharacter";
    default:
      return NIL;
    }
}

static void
stDefineUnionTypeInputOutSize(Type t, const char *prefix)
{
	TypeDescription	d	= t->description;
	list		fields	= d->structuredDes.uniond.types;
	DutRock		rock;

	rock.defaultArm = d->structuredDes.uniond.default_arm;
	rock.prefix = prefix;
	rock.count = 0;
	rock.unionType = t;

	stDefineIoFunc(t, prefix);
#ifdef ILU_CORBA_PYTHON_MAPPING
	if (prefix != prefixFuncInput) {
	  printf("    if type(%s) == %s['%s'].TupleType:\n", nameVarValue, nameVarImports, nameModuleTypes);
	  printf("\t%s = %s[0]\n", nameVarDiscrim, nameVarValue);
	  printf("\t%s = %s[1]\n", nameVarValueValue, nameVarValue);
	  printf("    else:\n");
	  printf("\t%s = %s.d\n", nameVarDiscrim, nameVarValue);
	  printf("\t%s = %s.v\n", nameVarValueValue, nameVarValue);
	  printf("    ");
	} else {
	  printf("    %s = ", nameVarDiscrim);
	}
#else
	if (prefix != prefixFuncInput)
	{
	  printf("    %s = %s[0]\n", nameVarDiscrim, nameVarValue);
	  printf("    %s = %s[1]\n", nameVarValueValue, nameVarValue);
	  printf("    ");
	} else
	  printf("    %s = ", nameVarDiscrim);
#endif /* def ILU_CORBA_PYTHON_MAPPING */
	if (prefix == prefixFuncSizeOf) {
	    printf("%s = ", nameVarSize);
	}
	printf("%s.%sUnion(%s", nameModuleIlu, prefix, nameVarCall);
	if (prefix != prefixFuncInput)
		printf(", %s", nameVarDiscrim);
	printf(", %s.TypeKind_%s)\n", nameModuleIlu,
	       unionDiscType(d->structuredDes.uniond.discriminator_type));

	list_enumerate(fields, (EnumProc) stUnionFieldInputOutSize, &rock);
	if (rock.count == 0)
		printf("    if 1: # no case values\n");
	else
		printf("    else:\n");
	if (rock.defaultArm != 0)
		stUnionCaseBody(rock.defaultArm, &rock);
	else if (!d->structuredDes.uniond.others_allowed)
		printf("\traise TypeError\n");
	else if (prefix == prefixFuncInput) {
#ifdef ILU_CORBA_PYTHON_MAPPING
	  printf("\t%s = %s(%s, None)\n", nameVarValue, getScopedTypeName(t), nameVarDiscrim);
#else
	  printf("\t%s = (%s, None)\n", nameVarValue, nameVarDiscrim);
#endif /* def ILU_CORBA_PYTHON_MAPPING */
	} else
	  printf("\tpass\n");

	printf("    %s.%sUnion(%s)\n", nameModuleIlu, prefixFuncEnd,
		nameVarCall);
	if (prefix == prefixFuncInput)
		printf("    return %s\n", nameVarValue);
	else if (prefix == prefixFuncSizeOf)
		printf("    return %s\n", nameVarSize);
}

static void
stUnionRegisterType(Type t)
{
  int i, default_arm, n_arms, j;
  list arms;
  Argument arm;

  default_arm = 0;
  arms = type_description(t)->structuredDes.uniond.types;
  if (type_description(t)->structuredDes.uniond.default_arm != NULL) {
    n_arms = list_size(arms);
    for (i = 0;  i < n_arms;  i++) {
      if (list_ref(arms, i) == type_description(t)->structuredDes.uniond.default_arm) {
	default_arm = i+1;
	break;
      }
    }
  }
  newline();
#ifdef ADD_TYPE_REGISTRATION_SUPPORT
  n_arms = list_size(arms);
  printf ("__types__['%s'] = %s.RegisterUnionType(",
	  getTypeName(t), nameModuleIlu);
  printNameScopes(t->scoping);
  printf(", %s%s%s, '%s', ",
	  (type_interface(t)->brand == NULL) ? "" : "\"\"\"",
	  (type_interface(t)->brand == NULL) ? "None" : type_interface(t)->brand,
	  (type_interface(t)->brand == NULL) ? "" : "\"\"\"",
	  type_uid(t));
  printf ("'%s', %s.TypeKind_%s, %lu, %d, (",
	  type_uid(type_description(t)->structuredDes.uniond.discriminator_type),
	  nameModuleIlu, unionDiscType(ur_type(type_description(t)->structuredDes.uniond.discriminator_type)),
	  default_arm, (type_description(t)->structuredDes.uniond.others_allowed) ? 1 : 0);
  for (i = 0;  i < n_arms;  i++) {
    arm = list_ref(arms, i);
    printf("%s('%s', '%s', (", (i == 0) ? "" : ", ",
	   (argument_name(arm) == NULL) ? "" : argument_name(arm),
	   type_uid(ur_type(arm->type)));
    for (j = 0;  j < list_size(arm->values);  j++) {
      if (type_kind(ur_type(type_description(t)->structuredDes.uniond.discriminator_type)) == enumeration_Type)
	printf("%s'%s'", (j == 0) ? "" : ", ", ((ConstantValue) (list_ref(arm->values, j)))->val.s);
      else if (type_kind(ur_type(type_description(t)->structuredDes.uniond.discriminator_type)) == boolean_Type)
	printf("%s%d", (j == 0) ? "" : ", ", ((ConstantValue) (list_ref(arm->values, j)))->val.b ? 1 : 0);
      else
	printf("%s%s%lu", (j == 0) ? "" : ", ",
	       (((ConstantValue) (list_ref(arm->values, j)))->val.i.sign < 0) ? "-" : "",
	       ((ConstantValue) (list_ref(arm->values, j)))->val.i.value);
    }
    printf("%s))", (list_size(arm->values) == 1) ? "," : "");
  }
  printf ("%s))\n", (n_arms == 1) ? "," : "");
#endif /* ADD_TYPE_REGISTRATION_SUPPORT */
  stRegisterIoFuncs(t);
}

static void
stDefineUnionType (Type t)
{
  newline();
  printf ("class %s(%s['%s'].%s):\n", getSimpleTypeName(t), nameVarImports, nameModuleIlu, nameClassUnion);
  printf ("    def __init__(self, d, v):\n");
  printf ("        self.d = d\n");
  printf ("        self.v = v\n");
}

static void
stUnionTypeIO(Type t)
{
  stDefineUnionTypeInputOutSize(t, prefixFuncInput);
  stDefineUnionTypeInputOutSize(t, prefixFuncOutput);
  stDefineUnionTypeInputOutSize(t, prefixFuncSizeOf);
}

static void
stUnionTypeClass(Type t)
{
#ifdef ILU_CORBA_PYTHON_MAPPING

  stDefineUnionType(t);

#endif /* def ILU_CORBA_PYTHON_MAPPING */
}

static void
stUnionTypeReg(Type t)
{
#ifdef ADD_VARIANT_SUPPORT

  stUnionRegisterType(t);

#endif /* ADD_VARIANT_SUPPORT */
}

/************************************/

static void
printLoopIndent(const int level)
{
	int	i;

	for (i = 0; i < level; i++)
		printf("    ");
}

static long
totalArrayLength(list dimList)
{
	int	nDims	= list_size(dimList);
	long	result	= 1;
	int	i;

	for (i = 0; i < nDims; i++)
		result *= (long) list_ref(dimList, i);
	return result;
}

static void
stDefineArrayTypeInput(Type t, const char *suffix)
{
	TypeDescription	d	= t->description;
	Type		eType	= d->structuredDes.array.type;
	list		dimList	= d->structuredDes.array.dimensions;
	int		nDims	= list_size(dimList);
	char *		eName	= arraySpecialElemTypeName(eType);
	int		nVDims	= eName == 0 ? nDims : nDims - 1;
	int		i;

	stDefineIoFunc(t, prefixFuncInput);

	if (suffix != NULL) {	/* special case:  1-dim string, byte, or wstring */

	  printf ("    return %s.%s%s(%s, %lu);\n",
		  nameModuleIlu, prefixFuncInput, suffix, nameVarCall,
		  (unsigned long) list_ref(dimList, 0));

	} else {		/* all other array types */

	  printf("    %s.%sArray(%s)\n", nameModuleIlu, prefixFuncInput,
		 nameVarCall);

	  for (i = 0; i < nVDims; i++)
	    {
	      long	length	= (long) list_ref(dimList, i);

	      printLoopIndent(i);
	      printf("    %s%d = []\n", nameVarValue, i);
	      printLoopIndent(i);
	      printf("    for %s%d in range(0,%ld):\n", nameVarIndex, i,
		     length);
	    }
	  printLoopIndent(nVDims);
	  printf("    %s%d.append(", nameVarValue, nVDims - 1);
	  if (eName)
	    ioArraySpecialElemInput(eName,
				    (long) list_ref(dimList, nVDims));
	  else
	    ioTypeInput(eType);
	  printf(")\n");

	  for (i = nVDims - 1; i > 0; i--)
	    {
	      printLoopIndent(i);
	      printf("    %s%d.append(%s%d)\n", nameVarValue, i - 1,
		     nameVarValue, i);
	    }

	  printf("    %s.%sArray(%s)\n", nameModuleIlu,
		 prefixFuncEnd, nameVarCall);
	  printf("    return %s0\n", nameVarValue);
	}
}

static void
stDefineArrayTypeOutSize(Type t, const char *prefix, const char *suffix)
{
	TypeDescription	d	= t->description;
	Type		eType	= d->structuredDes.array.type;
	list		dimList	= d->structuredDes.array.dimensions;
	int		nDims	= list_size(dimList);
	char *		eName	= arraySpecialElemTypeName(eType);
	int		nVDims	= eName == 0 ? nDims : nDims - 1;
	long		totLen	= totalArrayLength(dimList);
	char		argName[1024];
	int		i;

	stDefineIoFunc(t, prefix);

	if (suffix != NULL) {	/* special case:  1-dim bytes, chars, or wchars */

	  printf ("    %s%s.%s%s (%s, %s, %lu)\n",
		  (prefix == prefixFuncSizeOf) ? "return " : "",
		  nameModuleIlu, prefix, suffix, nameVarCall,
		  nameVarValue, (unsigned long) list_ref(dimList, 0));

	} else {		/* all other arrays */

	  printf("    ");
	  if (prefix == prefixFuncSizeOf)
	    printf("%s = ", nameVarSize);
	  printf("%s.%sArray(%s, %ld)\n", nameModuleIlu, prefix,
		 nameVarCall, totLen);

	  for (i = 0; i < nVDims; i++)
	    {
	      long	length	= (long) list_ref(dimList, i);

	      printLoopIndent(i);
	      printf("    for %s%d in range(0,%ld):\n", nameVarIndex, i,
		     length);
	    }
	  printLoopIndent(nVDims);
	  printf("    ");
	  if (prefix == prefixFuncSizeOf)
	    printf("%s = %s + ", nameVarSize, nameVarSize);
	  strcpy(argName, nameVarValue);
	  for (i = 0; i < nVDims; i++)
	    {
	      char	subscript[64];

	      sprintf(subscript, "[%s%d]", nameVarIndex, i);
	      strcat(argName, subscript);
	    }
	  if (eName)
	    ioArraySpecialElemOutSize(eName, argName, prefix,
				      (long) list_ref(dimList, nVDims));
	  else
	    ioTypeOutSize(eType, argName, prefix);
	  newline();

	  printf("    %s.%sArray(%s)\n", nameModuleIlu,
		 prefixFuncEnd, nameVarCall);
	  if (prefix == prefixFuncSizeOf)
	    printf("    return %s\n", nameVarSize);
	}
}

static void
stArrayTypeReg(Type t)
{
  int i;
  list dims;
  int dimCount = 0;

  newline();
#ifdef ADD_TYPE_REGISTRATION_SUPPORT
  printf ("__types__['%s'] = %s.RegisterArrayType(",
	  getTypeName(t), nameModuleIlu);
  printNameScopes(t->scoping);
  printf (", %s%s%s, '%s', ",
	  (type_interface(t)->brand == NULL) ? "" : "\"\"\"",
	  (type_interface(t)->brand == NULL) ? "None" : type_interface(t)->brand,
	  (type_interface(t)->brand == NULL) ? "" : "\"\"\"",
	  type_uid(t));
  printf ("'%s', (",
	  type_uid(type_description(t)->structuredDes.array.type));
  dims = type_description(t)->structuredDes.array.dimensions;
  for (i = 0;  i < list_size(dims);  i++)
    printf ("%lu,", (unsigned long) list_ref(dims, i));
  printf ("))\n"); 
#endif /* ADD_TYPE_REGISTRATION_SUPPORT */
  stRegisterIoFuncs(t);
}

static void
stArrayTypeIO(Type t)
{
  TypeDescription	d	= baseTypeDescription(t);
  Type		eType	= d->structuredDes.array.type;
  list		dimList	= d->structuredDes.array.dimensions;
  char *		suffix;

  if (list_size(dimList) == 1)
    suffix = arraySpecialElemTypeName(eType);
  else
    suffix = NULL;	  

  stDefineArrayTypeInput(t, suffix);
  stDefineArrayTypeOutSize(t, prefixFuncOutput, suffix);
  stDefineArrayTypeOutSize(t, prefixFuncSizeOf, suffix);
}

/************************************/

#ifdef ILU_PYTHON_DICTIONARIES

static boolean
stIsDictionary (Type t)
{
  Type rt;
  TypeDescription td;
  char *tn = type_name(t);

  return ((strlen(tn) >= 4) &&
	  ((strncmp(tn + strlen(tn) - 4, "dict", 4) == 0) ||
	   (strncmp(tn + strlen(tn) - 4, "Dict", 4) == 0)) &&
	  (type_ur_kind(t) == sequence_Type) &&
	  (type_ur_kind((rt = type_description(t)->structuredDes.sequence.type)) == record_Type) &&
	  (list_size((td = type_description(rt))->structuredDes.record.fields) == 2) &&
	  (strcmp ("name", argument_name((Argument) list_ref(td->structuredDes.record.fields, 0))) == 0) &&
	  (strcmp ("value", argument_name((Argument) list_ref(td->structuredDes.record.fields, 1))) == 0) &&
	  (rt = ur_type(((Argument) list_ref(td->structuredDes.record.fields, 0))->type),
	   ((type_kind(rt) == shortcardinal_Type) ||
	    (type_kind(rt) == cardinal_Type) ||
	    (type_kind(rt) == longcardinal_Type) ||
	    (type_kind(rt) == shortinteger_Type) ||
	    (type_kind(rt) == integer_Type) ||
	    (type_kind(rt) == longinteger_Type) ||
	    (type_kind(rt) == byte_Type) ||
	    ((type_kind(rt) == sequence_Type) &&
	     ((type_ur_kind(type_description(rt)->structuredDes.sequence.type) == character_Type) ||
	      (type_ur_kind(type_description(rt)->structuredDes.sequence.type) == shortcharacter_Type) ||
	      (type_ur_kind(type_description(rt)->structuredDes.sequence.type) == byte_Type))))));
}

#endif

static void
stDefineSequenceTypeInput(Type t, const char *suffix)
{
	TypeDescription	d	= t->description;
	Type		eType	= d->structuredDes.sequence.type;
	cardinal	limit	= d->structuredDes.sequence.limit;

	stDefineIoFunc(t, prefixFuncInput);
	if (suffix != NULL) {	/* string, or bytes, or wstring, type */
	  printf ("    return %s.%s%s(%s, %lu)\n",
		  nameModuleIlu, prefixFuncInput, suffix, nameVarCall, limit);
#ifdef ILU_PYTHON_DICTIONARIES
	} else if (stIsDictionary(t)) {
	  printf("    %s = %s.%sSequence(%s, %lu)\n", nameVarLength,
		 nameModuleIlu, prefixFuncInput, nameVarCall, limit);
	  printf("    %s = {}\n", nameVarValue);
	  printf("    for %s in range(0, %s):\n", nameVarIndex, nameVarLength);
	  printf("        %s = ", nameVarTemp);
	  ioTypeInput(eType);
	  printf("\n");
	  printf("        %s[%s.name] = %s.value\n", nameVarValue, nameVarTemp, nameVarTemp);
	  printf("    %s.%sSequence(%s)\n", nameModuleIlu,
		 prefixFuncEnd, nameVarCall);
	  printf("    return %s\n", nameVarValue);
#endif
	} else {		/* normal sequence */
	  printf("    %s = %s.%sSequence(%s, %lu)\n", nameVarLength,
		 nameModuleIlu, prefixFuncInput, nameVarCall, limit);
	  printf("    %s = []\n", nameVarValue);
	  printf("    for %s in range(0, %s):\n", nameVarIndex, nameVarLength);
	  printf("        %s.append(", nameVarValue);
	  ioTypeInput(eType);
	  printf(")\n");
	  printf("    %s.%sSequence(%s)\n", nameModuleIlu,
		 prefixFuncEnd, nameVarCall);
	  printf("    return %s\n", nameVarValue);
	};
}

static void
stDefineSequenceTypeOutSize(Type t, const char *prefix, const char *suffix)
{
	TypeDescription	d	= t->description;
	Type		eType	= d->structuredDes.sequence.type;
	cardinal	limit	= d->structuredDes.sequence.limit;
	char		argName[64];

	stDefineIoFunc(t, prefix);
	if (suffix != NULL) {	/* string, or bytes, or wstring */
	  printf ("    %s%s.%s%s (%s, %s, %lu)\n",
		  (prefix == prefixFuncSizeOf) ? "return " : "",
		  nameModuleIlu, prefix, suffix, nameVarCall,
		  nameVarValue, limit);
#ifdef ILU_PYTHON_DICTIONARIES
	} else if (stIsDictionary(t)) {
	  printf("    %s = %s.keys()\n", nameVarTemp, nameVarValue);
	  printf("    ");
	  if (prefix == prefixFuncSizeOf)
	    printf("%s = ", nameVarSize);
	  printf("%s.%sSequence(%s, len(%s), %lu)\n", nameModuleIlu, prefix,
		 nameVarCall, nameVarTemp, limit);
	  printf("    for %s in %s:\n", nameVarIndex,
		 nameVarTemp);
	  printf("        ");
	  if (prefix == prefixFuncSizeOf)
	    printf("%s = %s + ", nameVarSize, nameVarSize);
	  argName[0] = 0;
	  sprintTypeRef(argName, eType);
	  sprintf(argName + strlen(argName), "(%s, %s[%s])", nameVarIndex, nameVarValue, nameVarIndex);
	  ioTypeOutSize(eType, argName, prefix);
	  newline();

	  printf("    %s.%sSequence(%s)\n", nameModuleIlu,
		 prefixFuncEnd, nameVarCall);
	  if (prefix == prefixFuncSizeOf)
	    printf("    return %s\n", nameVarSize);
#endif
	} else {
	  printf("    ");
	  if (prefix == prefixFuncSizeOf)
	    printf("%s = ", nameVarSize);
	  printf("%s.%sSequence(%s, len(%s), %lu)\n", nameModuleIlu, prefix,
		 nameVarCall, nameVarValue, limit);
	  printf("    for %s in range(0, len(%s)):\n", nameVarIndex,
		 nameVarValue);
	  printf("        ");
	  if (prefix == prefixFuncSizeOf)
	    printf("%s = %s + ", nameVarSize, nameVarSize);
	  sprintf(argName, "%s[%s]", nameVarValue, nameVarIndex);
	  ioTypeOutSize(eType, argName, prefix);
	  newline();

	  printf("    %s.%sSequence(%s)\n", nameModuleIlu,
		 prefixFuncEnd, nameVarCall);
	  if (prefix == prefixFuncSizeOf)
	    printf("    return %s\n", nameVarSize);
	}
}

static void
stSequenceRegisterType(Type t, char *suffix)
{
  newline();
#ifdef ADD_TYPE_REGISTRATION_SUPPORT
  printf ("__types__['%s'] = %s.RegisterSequenceType(",
	  getTypeName(t), nameModuleIlu);
  printNameScopes(t->scoping);
  printf (", %s%s%s, '%s', ",
	  (type_interface(t)->brand == NULL) ? "" : "\"\"\"",
	  (type_interface(t)->brand == NULL) ? "None" : type_interface(t)->brand,
	  (type_interface(t)->brand == NULL) ? "" : "\"\"\"",
	  type_uid(t));
  printf ("'%s', %lu)\n",
	  type_uid(type_description(t)->structuredDes.sequence.type),
	  type_description(t)->structuredDes.sequence.limit);
#endif /* ADD_TYPE_REGISTRATION_SUPPORT */
  stRegisterIoFuncs(t);
}

static void
stSequenceTypeIO(Type t)
{
  TypeDescription	d	= baseTypeDescription(t);
  Type		eType	= d->structuredDes.sequence.type;
  char *		suffix;

  suffix = sequenceSpecialElemTypeName(eType);
  stDefineSequenceTypeInput(t, suffix);
  stDefineSequenceTypeOutSize(t, prefixFuncOutput, suffix);
  stDefineSequenceTypeOutSize(t, prefixFuncSizeOf, suffix);
}

static void
stSequenceTypeReg(Type t)
{
  TypeDescription	d	= baseTypeDescription(t);
  Type		eType	= d->structuredDes.sequence.type;
  char *		suffix;

  suffix = sequenceSpecialElemTypeName(eType);
  
#ifdef ADD_VARIANT_SUPPORT

  stSequenceRegisterType(t, suffix);

#endif /* ADD_VARIANT_SUPPORT */
}

/************************************/

static void
stAliasRegisterType(Type t)
{
  newline();
#ifdef ADD_TYPE_REGISTRATION_SUPPORT
  printf ("__types__['%s'] = %s.RegisterAliasType(",
	  getTypeName(t), nameModuleIlu);
  printNameScopes(t->scoping);
  printf (", %s%s%s, '%s', ",
	  (type_interface(t)->brand == NULL) ? "" : "\"\"\"",
	  (type_interface(t)->brand == NULL) ? "None" : type_interface(t)->brand,
	  (type_interface(t)->brand == NULL) ? "" : "\"\"\"",
	  type_uid(t));
  printf ("'%s')\n", type_uid(ur_type(t)));
#endif /* ADD_TYPE_REGISTRATION_SUPPORT */
  printf ("%s.NoteAlias('%s.%s', '%s', '%s')\n",
	  nameModuleIlu, interface_name(type_interface(t)), type_name(t),
	  type_uid(t), type_uid(ur_type(t)));
}

static void
stAliasTypeClass(Type t)
{
	switch (type_kind(ur_type(t)))
	{
	case enumeration_Type:
		stEnumerationTypeClass(t);
		break;

	case object_Type:
		newline();
		printf("%s = ", getSimpleTypeName(t));
		printClassVarName(t->supertype, NULL);
		newline();
		break;

	default:
		/* null */
		break;
	}
      }

static void
  stAliasTypeReg (Type t)
{
#ifdef ADD_VARIANT_SUPPORT

	stAliasRegisterType(t);

#endif /* ADD_VARIANT_SUPPORT */
}

/************************************/

#ifdef ILU_REFERENCE_TYPES

static void
stDefineReferenceTypeInput(Type t)
{
  Type base_type;
  boolean optional, aliased;

  base_type = (type_ur_kind(t) == optional_Type)
    ? type_description(t)->structuredDes.optional
      : type_description(t)->structuredDes.reference.base_type;
  optional = (type_ur_kind(t) == optional_Type) ? 1 : type_description(t)->structuredDes.reference.optional;
  aliased = (type_ur_kind(t) == optional_Type) ? 0 : type_description(t)->structuredDes.reference.aliased;

  stDefineIoFunc(t, prefixFuncInput);
  printf("    %s = %s.%sReference(%s)\n", nameVarTemp,
	 nameModuleIlu, prefixFuncInput, nameVarCall);
  printf("    if %s[0] != 0:\n", nameVarTemp);
  printf("        %s = ", nameVarValue);
  ioTypeInput(base_type);
  newline();
  if (aliased)
    printf("        %s.End%sReference(%s, %s[0], %s)\n", nameModuleIlu, prefixFuncInput,
	   nameVarCall, nameVarTemp, nameVarValue);
  printf("    elif %s[1] != None:\n", nameVarTemp);
  printf("        %s = %s[1]\n", nameVarValue, nameVarTemp);
  printf("    else:\n        %s = None\n    return %s\n",
	 nameVarValue, nameVarValue);
}

static void
stDefineReferenceTypeSize(Type t)
{
  Type base_type;
  boolean optional, aliased;

  base_type = (type_ur_kind(t) == optional_Type)
    ? type_description(t)->structuredDes.optional
      : type_description(t)->structuredDes.reference.base_type;
  optional = (type_ur_kind(t) == optional_Type) ? 1 : type_description(t)->structuredDes.reference.optional;
  aliased = (type_ur_kind(t) == optional_Type) ? 0 : type_description(t)->structuredDes.reference.aliased;

  stDefineIoFunc(t, prefixFuncSizeOf);
  printf ("    %s = %s.%sReference(%s, %s, %d)\n", nameVarTemp,
	  nameModuleIlu, prefixFuncSizeOf, nameVarCall, nameVarValue,
	  aliased ? 1 : 0);
  printf ("    if %s[1]:\n", nameVarTemp);
  printf ("        return %s[0] + ", nameVarTemp);
  ioTypeOutSize(base_type, nameVarValue, prefixFuncSizeOf);
  printf ("\n    else:\n");
  printf ("        return %s[0]\n", nameVarTemp);
}

static void
stDefineReferenceTypeOut(Type t)
{
  Type base_type;
  boolean optional, aliased;

  base_type = (type_ur_kind(t) == optional_Type)
    ? type_description(t)->structuredDes.optional
      : type_description(t)->structuredDes.reference.base_type;
  optional = (type_ur_kind(t) == optional_Type) ? 1 : type_description(t)->structuredDes.reference.optional;
  aliased = (type_ur_kind(t) == optional_Type) ? 0 : type_description(t)->structuredDes.reference.aliased;

  stDefineIoFunc(t, prefixFuncOutput);
  printf ("    if %s.%sReference(%s, %s, %d):\n        ",
	  nameModuleIlu, prefixFuncOutput, nameVarCall, nameVarValue,
	  aliased ? 1 : 0);
  ioTypeOutSize(base_type, nameVarValue, prefixFuncOutput);
  printf("\n");
}

static void
  stReferenceReg(Type t)
{
  Type base_type;
  boolean optional, aliased;

  base_type = (type_ur_kind(t) == optional_Type)
    ? type_description(t)->structuredDes.optional
      : type_description(t)->structuredDes.reference.base_type;
  optional = (type_ur_kind(t) == optional_Type) ? 1 : type_description(t)->structuredDes.reference.optional;
  aliased = (type_ur_kind(t) == optional_Type) ? 0 : type_description(t)->structuredDes.reference.aliased;

  newline();
#ifdef ADD_TYPE_REGISTRATION_SUPPORT
  printf ("__types__['%s'] = %s.RegisterReferenceType(",
	  getTypeName(t), nameModuleIlu);
  printNameScopes(t->scoping);
  printf (", %s%s%s, '%s', '%s', %d, %d)\n",
	  (type_interface(t)->brand == NULL) ? "" : "\"\"\"",
	  (type_interface(t)->brand == NULL) ? "None" : type_interface(t)->brand,
	  (type_interface(t)->brand == NULL) ? "" : "\"\"\"",
	  type_uid(t), type_uid(base_type), optional ? 1 : 0, aliased ? 1 : 0);
#endif /* ADD_TYPE_REGISTRATION_SUPPORT */
  stRegisterIoFuncs(t);
}

static void	stReferenceIO(Type t)
{
  stDefineReferenceTypeInput(t);
  stDefineReferenceTypeSize(t);
  stDefineReferenceTypeOut(t);
}

#else

static void
stDefineOptionalTypeInput(Type t)
{
  stDefineIoFunc(t, prefixFuncInput);
  printf("    return %s.%sOptional(%s) and ",
	 nameModuleIlu, prefixFuncInput, nameVarCall);
  ioTypeInput(type_description(t)->structuredDes.optional);
  newline();
}

static void
stDefineOptionalTypeOutSize(Type t, const char *prefix)
{
  Type base_type = type_description(t)->structuredDes.optional;

  stDefineIoFunc(t, prefix);
  if (prefix == prefixFuncSizeOf)
    printf("    return ", nameVarSize);
  else
    printf("    ");
  printf ("%s.%sOptional(%s, (%s != None)) %s ((%s != None) and ",
	  nameModuleIlu, prefix, nameVarCall, nameVarValue,
	  (prefix == prefixFuncSizeOf) ? "+" : "or", nameVarValue);
  ioTypeOutSize(base_type, nameVarValue, prefix);
  printf(")\n");
}

static void
  stOptionalReg(Type t)
{
  newline();
#ifdef ADD_TYPE_REGISTRATION_SUPPORT
  printf ("__types__['%s'] = %s.RegisterOptionalType(",
	  getTypeName(t), nameModuleIlu);
  printNameScopes(t->scoping);
  printf (", %s%s%s, '%s', '%s')\n",
	  (type_interface(t)->brand == NULL) ? "" : "\"\"\"",
	  (type_interface(t)->brand == NULL) ? "None" : type_interface(t)->brand,
	  (type_interface(t)->brand == NULL) ? "" : "\"\"\"",
	  type_uid(t), type_uid(type_description(t)->structuredDes.optional));
#endif /* ADD_TYPE_REGISTRATION_SUPPORT */
  stRegisterIoFuncs(t);
}

static void	stOptionalIO(Type t)
{
  stDefineOptionalTypeInput(t);
  stDefineOptionalTypeOutSize(t, prefixFuncOutput);
  stDefineOptionalTypeOutSize(t, prefixFuncSizeOf);
}

#endif /* def ILU_REFERENCE_TYPES */

/************************************/

static void	stClassReg(Type t);		/* forward declaration */
static void	stClassClass(Type t);		/* forward declaration */

static void
  stTypeIO (Type t)
{
  if (t->importInterfaceName != NULL)
    return;

  switch (type_kind(t)) {
	
  case enumeration_Type:	/* no special I/O */	break;
  case fixedpoint_Type:		stFixedpointTypeIO(t);	break;
  case record_Type:		stRecordTypeIO(t);	break;
  case union_Type:		stUnionTypeIO(t);	break;
  case array_Type:		stArrayTypeIO(t);	break;
  case sequence_Type:		stSequenceTypeIO(t);	break;
  case object_Type:		/* built-in */		break;
#ifdef ILU_REFERENCE_TYPES
  case optional_Type:		stReferenceIO(t);	break;
  case reference_Type:		stReferenceIO(t);	break;
#else
  case optional_Type:		stOptionalIO(t);	break;
#endif
  case alias_Type:		/* no special I/O */	break;
  default:			/* null */		break;
  }
}

static void
  stTypeClass (Type t)
{
  if (t->importInterfaceName != NULL)
    return;

  switch (type_kind(t)) {
	
  case enumeration_Type:	stEnumerationTypeClass(t);	break;
  case fixedpoint_Type:		stFixedpointTypeClass(t);	break;
  case record_Type:		stRecordTypeClass(t,0);	break;
  case union_Type:		stUnionTypeClass(t);	break;
  case array_Type:		/* no class needed */	break;
  case sequence_Type:		/* no class needed */	break;
  case object_Type:		stClassClass(t);	break;
  case optional_Type:		/* no class needed */	break;
  case reference_Type:		/* no class needed */	break;
  case alias_Type:		stAliasTypeClass(t);	break;
  default:			/* null */		break;
  }
}

static void
  stTypeReg (Type t)
{
  if (t->importInterfaceName != NULL)
    return;

  switch (type_kind(t)) {
	
  case enumeration_Type:	stEnumerationTypeReg(t);	break;
  case fixedpoint_Type:		stFixedpointTypeReg(t);	break;
  case record_Type:		stRecordTypeReg(t);	break;
  case union_Type:		stUnionTypeReg(t);	break;
  case array_Type:		stArrayTypeReg(t);	break;
  case sequence_Type:		stSequenceTypeReg(t);	break;
  case object_Type:		stClassReg(t);		break;
#ifdef ILU_REFERENCE_TYPES
  case optional_Type:		stReferenceReg(t);	break;
  case reference_Type:		stReferenceReg(t);	break;
#else
  case optional_Type:		stOptionalReg(t);	break;
#endif
  case alias_Type:		stAliasTypeReg(t);	break;
  default:			/* null */		break;
  }
}

static void
stType(Type t)
{
  if (t->importInterfaceName == NULL)
    {
      stTypeIO (t);
      stTypeClass (t);
      stTypeReg(t);
    }
}

/************************************************************************/

static void
stExceptionID(Exception e, int *pCount)
{
  if (e->import != 0)
    return;
  if (++*pCount == 1)
    newline();
  printf ("%s__ExcnID = ", getExceptionName(e));
  printExceptionIDString(e);
  newline();
}

static void
stException(Exception e, int *pCount)
{
  if (e->import != 0)
    return;
  if (++*pCount == 1)
    newline();
#ifdef ILU_CORBA_PYTHON_MAPPING
  if ((e->type != NULL) && isPrefixOf(prefixIdlExcType, type_name(e->type))) {
    /* associated value that's a struct type */
    printf ("%s = ", getSimpleExceptionName(e));
    printTypeRef(e->type);
    printf (";\n\n");
  } else {
    /* associated value that's of an arbitrary type */
    printf ("%s = %s\n\n", getSimpleExceptionName(e), getExceptionName(e));
  }
#else
  printf ("%s = %s__ExcnID", getExceptionName(e), getExceptionName(e));
  newline();
#endif
}

/************************************************************************/

static cardinal *	methodIdTable;
static int		nMethodIds;

#ifdef ILU_CORBA_PYTHON_MAPPING
static void
stNilObject (void)
{
  printf ("    _nil = None\n\n");
}
#endif

static int
getMethodIndex(cardinal methodId)
{
	int	i;

	for (i = 0; i < nMethodIds; i++)
		if (methodIdTable[i] == methodId)
			return i + 1;
	fatal("getMethodIndex failed");
}

static void stArgumentListItem(Argument a, int *pCount)
{
  if (++*pCount > 1)
    printf (", ");
  printf("('%s', %u, %u, '%s')",
	 getArgumentName(a),
	 ((a->direction == In) ? 0 :
	  ((a->direction == Out) ? 1 :
	   ((a->direction == InOut) ? 2 : 3))),
	 a->sibling,
	 type_uid(a->type));
}

static void
stExceptionListItem(Exception e, int *pCount)
{
  if (++*pCount > 1)
    printf (", ");
  printf ("(");
  printExceptionIDString(e);
  if (exception_type(e) != NULL)
    printf(", '%s'", type_uid(exception_type(e)));
  printf(",)");
}

static void
stMethodRecord(Procedure m, int *pMethodIndex)
{
	int	count	= 0;

	methodIdTable[(*pMethodIndex)++ - 1] = m->id;
	printf("\t    ('%s', %d, %s.%s, %s.%s, %s%s%s, (",
	       name_base_name(m->name),
	       m->id,
	       nameModuleIlu, booleanImage(m->functional),
	       nameModuleIlu, booleanImage(m->asynch),
	       (m->returnType == NULL) ? "" : "\"",
	       (m->returnType == NULL) ? "None" : type_uid(m->returnType),
	       (m->returnType == NULL) ? "" : "\"");
	list_enumerate(m->arguments, (EnumProc) stArgumentListItem,
		&count);
	if (count == 1)
		printf(",");
	printf("), (");
	count = 0;
	list_enumerate(m->exceptions, (EnumProc) stExceptionListItem,
		&count);
	if (count == 1)
		printf(",");
	printf(")),\\\n");
}

static void
stSuperclassClass(Type t, int *pCount)
{
	if (++*pCount > 1)
		printf(", ");
	printClassVarName(t, nameVarClass);
}

static void
stClassRecord(Type t)
{
	Class	c		= t->description->structuredDes.object;
	int	methodIndex	= 1;
	int	nSuperclasses	= 0;

	printf("    %s = %s.FormClassRecord(\\\n", nameVarClass, nameModuleIlu);
	printf("\t'%s.%s',\\\n", getSimpleInterfaceName(type_interface(t)), type_name(t));
	printf("\t'%s',\\\n", c->brand ? c->brand : "");
	printf("\t'%s',\\\n", t->uid);
	if (c->singleton)
		printf("\t'%s',\\\n", c->singleton);
	else
		printf("\tNone,\\\n");
	printf("\t%s.%s,\\\n", nameModuleIlu, booleanImage(c->collectible));
	printf("\t%s.%s,\\\n", nameModuleIlu, booleanImage(c->optional));
	if (c->authentication)
		printf("\t'%s',\\\n", c->authentication);
	else
		printf("\tNone,\\\n");
	printf("\t(\\\n");
	list_enumerate(c->methods, (EnumProc) stMethodRecord, &methodIndex);
	printf("\t),\\\n");
	printf("\t(");
	list_enumerate(c->superclasses, (EnumProc) stSuperclassClass,
		&nSuperclasses);
	if (nSuperclasses == 1)
		printf(",");
	printf(")\\\n");
	printf("\t)\n");
}

static void
stArgInput(Argument a, int *pCount)
{
	if (a->direction == Out || a->direction == InOut)
	{
		if (++*pCount > 1) {
		  printf(",\\\n");
		  printf("\t      ");
		}
		ioTypeInput(a->type);
	}
}

static void
stArgOutSize(Argument a, const char *prefix)
{
	if (a->direction == In || a->direction == InOut)
	{
		if (prefix == prefixFuncOutput) {
		  printf("\t        ");
		} else {
		  printf("\\\n");
		  printf("\t          + ");
		}
		ioTypeOutSize(a->type, getArgumentName(a), prefix);
		if (prefix == prefixFuncOutput)
			newline();
	}
}

static void
stMethodCheckCache(Procedure m)
{
  printf("\t%s = %s.%s('%s')\n\tif %s: return %s[0];\n",
	 nameVarTemp, nameVarSelf, nameFuncCacheAccess, getProcedureName(m),
	 nameVarTemp, nameVarTemp);
}

static void
stMethodCacheResult(Procedure m)
{
  printf("\tself.%s('%s', %s);\n",
	 nameFuncCacheValue, getProcedureName(m), nameVarResult);
}

static void
stMethodSendRequest(Procedure m)
{
	Type	tClass	= m->object;
	Class	c	= tClass->description->structuredDes.object;
	
	if (!(m->asynch))
	  printf("\t    while 1:\n");
	printf("\t        %s.BeginRequest(%s,\\\n", nameModuleIlu, nameVarCall);
	printf("\t          ");
	if (c->singleton == 0)
		ioObjDiscrimOutSize(tClass, prefixFuncSizeOf);
	else
		printf("0");
	list_enumerate(m->arguments, (EnumProc) stArgOutSize,
		(void *) prefixFuncSizeOf);
	printf(")\n");

	if (c->singleton == 0)
	{
		printf("\t        ");
		ioObjDiscrimOutSize(tClass, prefixFuncOutput);
		newline();
	}
	list_enumerate(m->arguments, (EnumProc) stArgOutSize,
		(void *) prefixFuncOutput);

	printf("\t        %s.FinishRequest(%s)\n", nameModuleIlu, nameVarCall);
}

static void
stMethodGetReply(Procedure m)
{
	int	nResults	= methodResultCount(m);

	printf("\t        %s = %s.GetReply(%s)\n", nameVarExceptCode,
		nameModuleIlu, nameVarCall);
	if (!(m->asynch))
	  printf("\t        if %s != %s.%s: break\n",
		 nameVarExceptCode, nameModuleIlu, nameExceptRetry);
	printf("\t    try:\n");
	printf("\t      if %s != 0:\n", nameVarExceptCode);
	printf("\t\t%s(%s, %s.%s, %s)\n", nameFuncCatchExcept, nameVarCall,
		getTypeName(m->object), nameVarClass, nameVarExceptCode);
	if (nResults > 0)
	{
		int	count	= 0;

		printf("\t      %s = ", nameVarResult);
		if (m->returnType)
		{
			++count;
			ioTypeInput(m->returnType);
		}
		list_enumerate(m->arguments, (EnumProc) stArgInput, &count);
		newline();
		if (m->functional && (list_size(m->arguments) == 0))
		  printf("\t      %s.%s('%s', %s)\n",
			 nameVarSelf, nameFuncCacheValue,
			 getProcedureName(m), nameVarResult);
	}
	printf("\t    finally:\n");
}

static void
stCheckSibling(Argument a, refany unused)
{
  if ((type_ur_kind(a->type) == object_Type) &&
      a->sibling &&
      ((a->direction == In) || (a->direction == InOut)))
    printf ("\tiluRt.CheckSibling(%s, %s)\n",
	    nameVarSelf, getArgumentName(a));
}

static void
stCheckSiblings(Procedure m)
{
  list_enumerate(m->arguments, (EnumProc) stCheckSibling, 0);
}

static void
stMethod(Procedure m)
{
	Type	tClass	= m->object;
	Class	c	= tClass->description->structuredDes.object;

	newline();
	printf("    def %s(%s", getProcedureName(m), nameVarSelf);
	printArgList(m->arguments, 1);
	printf("):\n");
	if (m->functional && (list_size(m->arguments) == 0) && (methodResultCount(m) > 0))
	  stMethodCheckCache(m);
	if (methodResultCount(m) > 0)
	  printf("\t%s = None\n", nameVarResult);

#if (ILUPYTHON_MAJOR_VERSION >= 1 && ILUPYTHON_MINOR_VERSION >= 2)

	/* put out docstrings in Python */
	if (m->doc_string != NULL)
	  {
	    char *p;

	    printf ("        \"\"\"");
	    for (p = m->doc_string;  *p != '\0';  p++)
	      {
		putchar (*p);
		if (*p == '\n')
		  printf ("        ");
	      }
	    if (*--p != '\n')
	      printf ("\n        ");
	    printf ("\"\"\"\n");
	  }

#endif /* (ILUPYTHON_MAJOR_VERSION >= 1 && ILUPYTHON_MINOR_VERSION >= 2) */

	stCheckSiblings(m);
	printf("\t%s = %s.BeginCall(%s, %s.%s, %d)\n", nameVarCall, nameModuleIlu,
		nameVarSelf, getTypeName(m->object), nameVarClass,
		getMethodIndex(m->id));
	printf("\ttry:\n");
	stMethodSendRequest(m);
	if (m->asynch == 0)
	  {
	    stMethodGetReply(m);
	    printf("\t      %s.ReplyRead(%s)\n", nameModuleIlu, nameVarCall);
	  }
	printf("\tfinally:\n");
	printf("\t    %s.FinishCall(%s)\n", nameModuleIlu, nameVarCall);
	if (methodResultCount(m) > 0)
	  printf("\treturn %s\n", nameVarResult);
}

static void
stSuperclass(Type t, int *pCount)
{
	if (++*pCount > 1)
		printf(", ");
	printClassVarName(t, NULL);
}

static void
stClassClass(Type t)
{
	Class	c	= t->description->structuredDes.object;
	int	nSupers	= 0;

	nMethodIds = list_size(c->methods);
	if (nMethodIds > 0)
	  methodIdTable =
	    (cardinal *) iluparser_Malloc(nMethodIds * sizeof(cardinal));
	else
	  methodIdTable = NULL;

	newline();
	printf("class %s(", getTypeName(t));
	list_enumerate(c->superclasses, (EnumProc) stSuperclass, &nSupers);
	if (nSupers == 0)
		printf("%s.%s", nameModuleIlu, nameClassStub);
	printf("):\n");

#if (ILUPYTHON_MAJOR_VERSION >= 1 && ILUPYTHON_MINOR_VERSION >= 2)

	/* put out docstrings in Python */
	if (c->doc_string != NULL)
	  {
	    char *p;

	    printf ("    \"\"\"");
	    for (p = c->doc_string;  *p != '\0';  p++)
	      {
		putchar (*p);
		if (*p == '\n')
		  printf ("    ");
	      }
	    if (*--p != '\n') {
	      printf ("\n");
	      printf ("    ");
	    }
	    printf ("\"\"\"\n");
	  }

#endif /* (ILUPYTHON_MAJOR_VERSION >= 1 && ILUPYTHON_MINOR_VERSION >= 2) */

	stClassRecord(t);
#ifdef ILU_CORBA_PYTHON_MAPPING
	stNilObject();
#endif /* def ILU_CORBA_PYTHON_MAPPING */
	list_enumerate(c->methods, (EnumProc) stMethod, NULL);

	if (methodIdTable != NULL)
	  iluparser_Free(methodIdTable);
}

static void
stClassReg(Type t)
{
  Class co = class_object(t);

  newline();
  printf("__types__['%s'] = %s.RegisterObjectType(", getTypeName(t), nameModuleIlu);
  printClassVarName(t, NULL);
  printf(", ");
  printNameScopes(t->scoping);
  printf(", %s%s%s, %s%s%s)\n",
	 (type_interface(t)->brand == NULL) ? "" : "\"\"\"",
	 (type_interface(t)->brand == NULL) ? "None" : type_interface(t)->brand,
	 (type_interface(t)->brand == NULL) ? "" : "\"\"\"",
	 (co->doc_string == NULL) ? "" : "\"\"\"",
	 (co->doc_string == NULL) ? "None" : co->doc_string,
	 (co->doc_string == NULL) ? "" : "\"\"\"");
}

/************************************************************************/

static void
stCatchException(Exception e, int *pCount)
{
  Type	et	= e->import ? e->import->type : e->type;
  boolean idlish;

  idlish = (et != NULL) && isPrefixOf(prefixIdlExcType, type_name(et));
  printf("    %sif %s == %s__ExcnID:\n", ++*pCount > 1 ? "el" : "",
	 nameVarExceptName, getExceptionName(e));
#ifdef ILU_CORBA_PYTHON_MAPPING
  printf("\t%s = ", nameVarExceptValue);
  if (!idlish)
    printf("%s(", getExceptionName(e));
  if (et)
    ioTypeInput(et);
  if (!idlish)
    printf(")");
  newline();
#else
  printf("\t%s = %s", nameVarExceptName, getExceptionName(e));
  newline();
  if (et) {
    printf("\t%s = ", nameVarExceptValue);
    ioTypeInput(et);
    newline();
  }
#endif
}

static void
stFuncCatchExceptions(list exceptions)
{
	int	nExceptions	= 0;

	newline();
	printf("def %s(%s, %s, %s):\n", nameFuncCatchExcept,
		nameVarCall, nameVarClass, nameVarExceptCode);
	printf("    %s = None\n", nameVarExceptValue);
	printf("    %s = %s.ExceptionName(%s, %s, %s)\n", nameVarExceptName,
		nameModuleIlu, nameVarCall, nameVarClass, nameVarExceptCode);
	list_enumerate(exceptions, (EnumProc) stCatchException, &nExceptions);
#ifdef ILU_CORBA_PYTHON_MAPPING
	printf("    raise %s\n", nameVarExceptValue);
#else
	printf("    raise %s, %s\n", nameVarExceptName, nameVarExceptValue);
#endif /* def ILU_CORBA_PYTHON_MAPPING */
}

/************************************************************************/

#ifdef ILU_CORBA_PYTHON_MAPPING

static void
stSpace (namespace ns)
{
  int	nConstants	= 0;
  int	nExceptions	= 0;

  if (ns->type != idlInterface)
    return;
  stClassClass (ns->d.interface.ifc);
  modifyIndentLevel(+1);
  list_enumerate (ns->constants, (EnumProc) stConstant, &nConstants);
  iluparser_ClearMarks();
  list_enumerate (ns->types, (EnumProc) stTypeClass, NULL);
  list_enumerate (ns->exceptions, (EnumProc) stException, &nExceptions);
  if (list_size(ns->subspaces) > 0) {
    list_enumerate (ns->subspaces, (EnumProc) stSpace, NULL);
  }
  modifyIndentLevel(-1);
  stClassReg(ns->d.interface.ifc);
}

static void
  stExcType (Exception e)
{
  if (e->type == NULL) {
    /* no associated value */
    printf ("class %s(%s['%s'].%s):\n", getExceptionName(e),
	    nameVarImports, nameModuleCORBA, nameUserException);
    printf ("\tpass\n\n");
  } else if (isPrefixOf(prefixIdlExcType, type_name(e->type))) {
    /* associated value that's a struct type */
    stRecordTypeClass (e->type, 1);
  } else {
    /* associated value that's of an arbitrary type */
    printf ("class %s(%s['%s'].%s):\n", getExceptionName(e),
	    nameVarImports, nameModuleCORBA, nameUserException);
    printf ("\tdef __init__(self, _val):\n");
    printf ("\t\tself.value = _val;\n\n");
  }
}

static void
  genTypeIO (namespace ns)
{
  int nExceptions = 0;
  list sorted_types = new_list();

  list_enumerate (ns->types, (EnumProc) stTypeClass, NULL);
  list_enumerate (ns->types, (EnumProc) stTypeIO, NULL);
  list_enumerate (ns->types, (EnumProc) stTypeReg, NULL);
  list_enumerate (ns->exceptions, (EnumProc) stExcType, NULL);
  list_enumerate (ns->exceptions, (EnumProc) stExceptionID, &nExceptions);
  list_enumerate (ns->subspaces, (EnumProc) genTypeIO, NULL);
}

void
generateStub (namespace ns)
{
  int	nConstants	= 0;
  int	nExceptions	= 0;

  currentIfc = namespace_interface(ns);
  printBanner("Stubs", namespace_interface(ns));
  printImportTable();
  printTypeTable();
  list_enumerate (namespace_imports(ns), (EnumProc) stImported, NULL);
  list_enumerate (ns->constants, (EnumProc) stConstant, &nConstants);
  iluparser_ClearMarks();
  genTypeIO (ns);
  list_enumerate (ns->subspaces, (EnumProc) stSpace, NULL);
  list_enumerate (ns->exceptions, (EnumProc) stException, &nExceptions);
  stFuncCatchExceptions (namespace_interface(ns)->exceptions);
}

#else

void
generateStub(Interface ifc, list localtypes)
{
	int	nConstants	= 0;
	int	nExceptions	= 0;

	currentIfc = ifc;
	printBanner("Stubs", ifc);
	printImportTable();
	printTypeTable();
	list_enumerate(ifc->imports,    (EnumProc) stImported,  NULL);
	list_enumerate(ifc->constants,  (EnumProc) stConstant,  &nConstants);
	iluparser_ClearMarks();
	list_enumerate(localtypes,      (EnumProc) stType,      NULL);
	list_enumerate(ifc->exceptions, (EnumProc) stExceptionID, &nExceptions);
	list_enumerate(ifc->exceptions, (EnumProc) stException, &nExceptions);
	stFuncCatchExceptions(ifc->exceptions);
}

#endif /* def ILU_CORBA_PYTHON_MAPPING */
