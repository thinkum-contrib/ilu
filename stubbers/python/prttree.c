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
$Id: prttree.c,v 1.12 1999/08/03 01:50:35 janssen Exp $
*/
/* Last edited by Mike Spreitzer September 17, 1998 2:51 pm PDT */

#include <stdlib.h>
#include <stdio.h>
#include "iluptype.h"
#include "manifest.h"
#include "util.h"
#include "prttree.h"

static char *
primitiveTypeImage(enum PrimitiveTypes pt)
{
	switch (pt)
	{
	case invalid_Type:		return "invalid";
	case void_Type:			return "void";
	case byte_Type:			return "byte";
	case boolean_Type:		return "boolean";
	case character_Type:		return "character";
	case shortcharacter_Type:	return "shortcharacter";
	case shortinteger_Type:		return "shortinteger";
	case integer_Type:		return "integer";
	case longinteger_Type:		return "longinteger";
	case shortcardinal_Type:	return "shortcardinal";
	case cardinal_Type:		return "cardinal";
	case longcardinal_Type:		return "longcardinal";
	case real_Type:			return "real";
	case shortreal_Type:		return "shortreal";
	case longreal_Type:		return "longreal";
	case pickle_Type:		return "pickle";
	case object_Type:		return "object";
	case pipe_Type:			return "pipe";
	case optional_Type:		return "optional";
	case union_Type:		return "union";
	case sequence_Type:		return "sequence";
	case record_Type:		return "record";
	case array_Type:		return "array";
	case enumeration_Type:		return "enumeration";
	case fixedpoint_Type:		return "fixedpoint";
	case string_Type:		return "string";
	case reference_Type:		return "reference";
	default:			return "???";
	}
}

static void
ptName(Name n)
{
	printf("\"%s\"", n->base_name);
}

static void
ptTypeName(Type t)
{
	printf("\"");
	if (t->importInterfaceName)
		printf("%s.", t->importInterfaceName);
	printf("%s\"", t->name->base_name);
}

/************************************************************************/

static void
ptImported(Imported i)
{
	printf("%simported \"%s\"", sol, i->name);
	if (i->filename)
		printf(" from file \"%s\"", i->filename);
	newline();
}

/************************************************************************/

static void
ptConstantValue(ConstantValue v)
{
	switch (v->type)
	{
	case boolean_Type:
		printf(v->val.b ? "TRUE" : "FALSE");
		break;

	case integer_Type:
		printf("%s%lu", v->val.i.sign < 0 ? "-" : "", v->val.i.value);
		break;

	case real_Type:
		printf("%s%s.%se%ld", v->val.r.sign < 0 ? "-" : "",
			v->val.r.value,
			v->val.r.fraction ? v->val.r.fraction : "0",
			v->val.r.exponent);
		break;

	case shortcharacter_Type:
		printf("\"%s\"", v->val.s);
		break;

	default:
		printf("?");
	}
}

static void
ptConstant(Constant c)
{
	printf("%sconstant ", sol);
	ptName(c->name);
	printf(" : ");
	ptTypeName(c->type);
	printf(" = ");
	ptConstantValue(c->value);
	newline();
}

/************************************************************************/

static void
ptArg(Argument a)
{
	printf("%sarg ", sol);
	ptName(a->name);
	printf(" : ");
	ptTypeName(a->type);
	newline();
}

static void
ptMethod(Procedure m)
{
	printf("%smethod ", sol);
	ptName(m->name);
	if (m->returnType)
	{
		printf(" returns ");
		ptTypeName(m->returnType);
	}
	newline();

	indent(1);
	list_enumerate(m->arguments, (EnumProc) ptArg, NULL);
	indent(-1);
}

static void
ptSuperclass(Type t)
{
	printf(" ");
	ptTypeName(t);
}

static void
ptObjectTypedes(TypeDescription d)
{
	Class	c	= d->structuredDes.object;

	if (list_size(c->superclasses) != 0)
	{
		printf("%ssuperclasses", sol);
		list_enumerate(c->superclasses, (EnumProc) ptSuperclass, NULL);
		newline();
	}
	if (c->brand || c->authentication || c->singleton || c->collectible ||
	    c->optional)
	{
		const char sep[] = ", ";
		const char *pre = "";

		printf(sol);
		if (c->brand)
			printf("%sbrand \"%s\"", pre, c->brand), pre = sep;
		if (c->authentication)
			printf("%sauthentication \"%s\"", pre,
				c->authentication), pre = sep;
		if (c->singleton)
			printf("%ssingleton(%s)", pre, c->singleton), pre = sep;
		if (c->collectible)
			printf("%scollectible", pre), pre = sep;
		if (c->optional)
			printf("%soptional", pre), pre = sep;
		newline();
	}
	list_enumerate(c->methods, (EnumProc) ptMethod, NULL);
}

/************************************/

static void
ptEnumField(EnumField e)
{
  printf("%slit \"%s\" = %d\n", sol, e->name, e->id);
}

static void
ptEnumerationTypedes(TypeDescription d)
{
  
  list_enumerate(d->structuredDes.enumeration, (EnumProc) ptEnumField,
		 NULL);
}

/************************************/

static void
ptFixedpointTypedes (TypeDescription d)
{
  printf("%sdenom = %s", sol, d->structuredDes.fixed.denominator->negative ? "-" : "");
  if (d->structuredDes.fixed.denominator->small)
    printf("%lu", d->structuredDes.fixed.denominator->val.direct);
  else
    printf("%s", d->structuredDes.fixed.denominator->val.string);
  newline();

  printf("%smin_numerator = %s", sol, d->structuredDes.fixed.min_numerator->negative ? "-" : "");
  if (d->structuredDes.fixed.min_numerator->small)
    printf("%lu", d->structuredDes.fixed.min_numerator->val.direct);
  else
    printf("%s", d->structuredDes.fixed.min_numerator->val.string);
  newline();

  printf("%smax_numerator = %s", sol, d->structuredDes.fixed.max_numerator->negative ? "-" : "");
  if (d->structuredDes.fixed.max_numerator->small)
    printf("%lu", d->structuredDes.fixed.max_numerator->val.direct);
  else
    printf("%s", d->structuredDes.fixed.max_numerator->val.string);
  newline();
}

/************************************/

static void
ptRecordField(Argument f)
{
	printf("%sfield ", sol);
	ptName(f->name);
	printf(" : ");
	ptTypeName(f->type);
	newline();
}

static void
ptRecordTypedes(TypeDescription d)
{
	list_enumerate(d->structuredDes.record.fields, (EnumProc) ptRecordField, NULL);
}

/************************************/

static void
ptUnionFieldValues(ConstantValue v, int *pCount)
{
	*pCount += 1;
	printf("%s ", *pCount == 1 ? " =" : ",");
	ptConstantValue(v);
}

static void
ptUnionField(Argument f)
{
	int	count	= 0;

	printf("%sfield ", sol);
	ptName(f->name);
	printf(" : ");
	ptTypeName(f->type);
	list_enumerate(f->values, (EnumProc) ptUnionFieldValues, &count);
	newline();
}

static void
ptUnionTypedes(TypeDescription d)
{
	printf("%sdiscrim type ", sol);
	ptTypeName(d->structuredDes.uniond.discriminator_type);
	newline();

	list_enumerate(d->structuredDes.uniond.types, (EnumProc) ptUnionField,
		NULL);
}

/************************************/

static void
ptArrayDim(long dim, int *pCount)
{
	*pCount += 1;
	printf("%s%ld", *pCount > 1 ? ", " : "", dim);
}

static void
ptArrayTypedes(TypeDescription d)
{
	Type	base	= d->structuredDes.array.type;
	list	dims	= d->structuredDes.array.dimensions;
	int	count	= 0;

	printf("%sbase ", sol);
	ptTypeName(base);
	printf(" dim ");
	list_enumerate(dims, (EnumProc) ptArrayDim, &count);
	newline();
}

/************************************/

static void
ptSequenceTypedes(TypeDescription d)
{
	Type		base	= d->structuredDes.sequence.type;
	cardinal	limit	= d->structuredDes.sequence.limit;

	printf("%sbase ", sol);
	ptTypeName(base);
	if (limit)
		printf(" limit %lu", limit);
	newline();
}

/************************************/

static void
ptOptionalTypedes(TypeDescription d)
{
	Type		base	= d->structuredDes.optional;

	printf("%sbase ", sol);
	ptTypeName(base);
	newline();
}

/************************************/

static void
ptReferenceTypedes(TypeDescription d)
{
	Type		base	= d->structuredDes.reference.base_type;
	boolean		optional= d->structuredDes.reference.optional;
	boolean		aliased = d->structuredDes.reference.aliased;

	printf("%sbase ", sol);
	ptTypeName(base);
	if (optional)
	  printf(" optional");
	if (aliased)
	  printf(" aliased");
	newline();
}

/************************************/

static void
ptType(Type t)
{
	TypeDescription	d	= t->description;

	if (t->builtIn)
		return;
	printf("%stype ", sol);
	ptTypeName(t);
	if (d == 0)
	{
		printf(" = ");
		ptTypeName(t->supertype);
		newline();
		return;
	}
	printf(" %s\n", primitiveTypeImage(d->type));

	indent(1);
	switch (d->type)
	{
	case object_Type:	ptObjectTypedes(d);		break;
	case enumeration_Type:	ptEnumerationTypedes(d);	break;
	case record_Type:	ptRecordTypedes(d);		break;
	case union_Type:	ptUnionTypedes(d);		break;
	case array_Type:	ptArrayTypedes(d);		break;
	case sequence_Type:	ptSequenceTypedes(d);		break;
	case fixedpoint_Type:	ptFixedpointTypedes(d);		break;
	case optional_Type:	ptOptionalTypedes(d);		break;
	case reference_Type:	ptReferenceTypedes(d);		break;
	default:		/* null */			break;
	}
	indent(-1);
}

/************************************************************************/

static void
ptException(Exception e)
{
	printf("%sexception ", sol);
	ptName(e->name);
	if (e->type)
	{
		printf(" of type ");
		ptTypeName(e->type);
	}
	newline();
}

/************************************************************************/

static void
ptInterface(Interface ifc, list localtypes)
{
	printf("%sinterface ", sol);
	ptName(ifc->name);
	newline();

	indent(1);
	list_enumerate(ifc->imports,    (EnumProc) ptImported,  NULL);
	list_enumerate(ifc->constants,  (EnumProc) ptConstant,  NULL);
	list_enumerate(localtypes,      (EnumProc) ptType,      NULL);
	list_enumerate(ifc->exceptions, (EnumProc) ptException, NULL);
	indent(-1);
}

/************************************************************************/

void
printTree(Interface ifc, list localtypes)
{
	ptInterface(ifc, localtypes);
	newline();
}
