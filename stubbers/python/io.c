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
$Id: io.c,v 1.11 1999/08/03 01:50:34 janssen Exp $
*/

#include <stdio.h>
#include "iluptype.h"
#include "manifest.h"
#include "util.h"
#include "io.h"

static void
ioSimpleTypeInput(const char *simpleName)
{
	printf("%s.%s%s(%s)", nameModuleIlu, prefixFuncInput, simpleName,
		nameVarCall);
}

static void
ioSimpleTypeOutSize(const char *simpleName, const char *argName,
	const char *prefix)
{
	printf("%s.%s%s(%s, %s)", nameModuleIlu, prefix, simpleName,
		nameVarCall, argName);
}

/************************************/

static void
ioCompoundTypeInput(Type t)
{
  if ((type_ur_kind(t) == record_Type) &&
      type_description(t)->structuredDes.record.extensible)
    printf("%s.%sExtensibleRecord(%s, '%s')",
	   nameModuleIlu, prefixFuncInput, nameVarCall, type_uid(t));
  else {
    printTypeIoFuncName(t, prefixFuncInput);
    printf("(%s)", nameVarCall);
  }
}

static void
ioCompoundTypeOutSize(Type t, const char *argName, const char *prefix)
{
  if ((type_ur_kind(t) == record_Type) &&
      type_description(t)->structuredDes.record.extensible)
    printf("%s.%sExtensibleRecord(%s, %s, '%s')",
	   nameModuleIlu, prefix, nameVarCall, argName, type_uid(t));
  else {
    printTypeIoFuncName(t, prefix);
    printf("(%s, %s)", nameVarCall, argName);
  }
}

/************************************/

void
ioArraySpecialElemInput(const char *eName, const long length)
{
	printf("%s.%s%s(%s, %ld)", nameModuleIlu, prefixFuncInput, eName,
		nameVarCall, length);
}

void
ioArraySpecialElemOutSize(const char *eName, const char *argName,
	const char *prefix, const long length)
{
	printf("%s.%s%s(%s, %s, %ld)", nameModuleIlu, prefix, eName,
		nameVarCall, argName, length);
}

static void
ioArrayTypeInput(Type t)
{
	TypeDescription	d	= baseTypeDescription(t);
	list		dims	= d->structuredDes.array.dimensions;
	int		nDims	= list_size(dims);

	if (nDims == 1)
	{
		Type	eType	= d->structuredDes.array.type;
		char *	eName	= arraySpecialElemTypeName(eType);

		if (eName != 0)
		{
			ioArraySpecialElemInput(eName,
				(long) list_ref(dims, 0));
			return;
		}
	}
	ioCompoundTypeInput(t);
}

static void
ioArrayTypeOutSize(Type t, const char *argName, const char *prefix)
{
	TypeDescription	d	= baseTypeDescription(t);
	list		dims	= d->structuredDes.array.dimensions;
	int		nDims	= list_size(dims);

	if (nDims == 1)
	{
		Type	eType	= d->structuredDes.array.type;
		char *	eName	= arraySpecialElemTypeName(eType);

		if (eName != 0)
		{
			ioArraySpecialElemOutSize(eName, argName, prefix,
				(long) list_ref(dims, 0));
			return;
		}
	}
	ioCompoundTypeOutSize(t, argName, prefix);
}

/************************************/

static void
ioSequenceTypeInput(Type t)
{
	TypeDescription	d	= baseTypeDescription(t);
	Type		eType	= d->structuredDes.sequence.type;
	char *		eName	= sequenceSpecialElemTypeName(eType);

	if (eName != 0)
	{
		long	limit	= d->structuredDes.sequence.limit;

		printf("%s.%s%s(%s, %ld)", nameModuleIlu,
			prefixFuncInput, eName,
			nameVarCall, limit);
		return;
	}
	ioCompoundTypeInput(t);
}

static void
ioSequenceTypeOutSize(Type t, const char *argName, const char *prefix)
{
	TypeDescription	d	= baseTypeDescription(t);
	Type		eType	= d->structuredDes.sequence.type;
	char *		eName	= sequenceSpecialElemTypeName(eType);

	if (eName != 0)
	{
		long	limit	= d->structuredDes.sequence.limit;

		printf("%s.%s%s(%s, %s, %ld)", nameModuleIlu,
			prefix, eName, nameVarCall, argName, limit);
		return;
	}
	ioCompoundTypeOutSize(t, argName, prefix);
}

/************************************/

void
ioObjDiscrimInput(Type t)
{
	printf("%s.%sObjectID(%s, %s.%s, ", nameModuleIlu, prefixFuncInput,
		nameVarCall, nameModuleIlu, booleanImage(1));
	printClassVarName(t, NULL);
	printf(")");
}

void
ioObjDiscrimOutSize(Type t, const char *prefix)
{
	printf("%s.%sObjectID(%s, %s, %s.%s, ", nameModuleIlu, prefix,
		nameVarCall, nameVarSelf, nameModuleIlu, booleanImage(1));
	printClassVarName(t, NULL);
	printf(")");
}

static void
ioObjectTypeInput(Type t)
{
	printf("%s.%sObjectID(%s, %s.%s, ",
		nameModuleIlu, prefixFuncInput, nameVarCall,
		nameModuleIlu, booleanImage(0));
	printClassVarName(t, NULL);
	printf(")");
}

static void
ioObjectTypeOutSize(Type t, const char *argName, const char *prefix)
{
	printf("%s.%sObjectID(%s, %s, %s.%s, ",
		nameModuleIlu, prefix,
		nameVarCall, argName, nameModuleIlu, booleanImage(0));
	printClassVarName(t, NULL);
	printf(")");
}

/************************************/

void
ioTypeInput(Type t)
{
	char *	simpleName	= simpleTypeName(t);

	if (simpleName)
	{
		ioSimpleTypeInput(simpleName);
		return;
	}
	switch (baseTypeDescription(t)->type)
	{
	case record_Type:
	case union_Type:
	case optional_Type:
#ifdef ILU_REFERENCE_TYPES
	case reference_Type:
#endif
		ioCompoundTypeInput(t);
		break;

	case array_Type:
		ioArrayTypeInput(t);
		break;

	case sequence_Type:
		ioSequenceTypeInput(t);
		break;

	case object_Type:
		ioObjectTypeInput(t);
		break;

	case fixedpoint_Type:
		ioCompoundTypeInput(t);
		break;		

	default:
		fatal("ioTI: unexpected primitive type (%d)",
			baseTypeDescription(t)->type);
	}
}

void
ioTypeOutSize(Type t, const char *argName, const char *prefix)
{
	char *	simpleName	= simpleTypeName(t);

	if (simpleName)
	{
		ioSimpleTypeOutSize(simpleName, argName, prefix);
		return;
	}
	switch (baseTypeDescription(t)->type)
	{
	case record_Type:
	case union_Type:
	case optional_Type:
#ifdef ILU_REFERENCE_TYPES
	case reference_Type:
#endif
		ioCompoundTypeOutSize(t, argName, prefix);
		break;

	case array_Type:
		ioArrayTypeOutSize(t, argName, prefix);
		break;

	case sequence_Type:
		ioSequenceTypeOutSize(t, argName, prefix);
		break;

	case object_Type:
		ioObjectTypeOutSize(t, argName, prefix);
		break;

	case fixedpoint_Type:
		ioSimpleTypeOutSize("FixedPoint", argName, prefix);
		break;		

	default:
		fatal("ioTOS: unexpected primitive type (%d)",
			baseTypeDescription(t)->type);
	}
}
