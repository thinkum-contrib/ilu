/* genarr.c */
/* Chris Jacobi, November 13, 1998 11:59 am PST */

/*
 * Copyright (c) 1991-1999 Xerox Corporation.  All Rights Reserved.
 * 
 * Unlimited use, reproduction, modification, and distribution of this
 * software and modified versions thereof is permitted.  Permission is
 * granted to make derivative works from this software or a modified
 * version thereof.  Any copy of this software, a modified version
 * thereof, or a derivative work must include both the above copyright
 * notice of Xerox Corporation and this paragraph.  Any distribution of
 * this software, a modified version thereof, or a derivative work must
 * comply with all applicable United States export control laws.  This
 * software is made available AS IS, and XEROX CORPORATION DISCLAIMS ALL
 * WARRANTIES, EXPRESS OR IMPLIED, INCLUDING WITHOUT LIMITATION THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE, AND NOTWITHSTANDING ANY OTHER PROVISION CONTAINED HEREIN, ANY
 * LIABILITY FOR DAMAGES RESULTING FROM THE SOFTWARE OR ITS USE IS
 * EXPRESSLY DISCLAIMED, WHETHER ARISING IN CONTRACT, TORT (INCLUDING
 * NEGLIGENCE) OR STRICT LIABILITY, EVEN IF XEROX CORPORATION IS ADVISED
 * OF THE POSSIBILITY OF SUCH DAMAGES.
 */

/* $Id: genarr.c,v 1.16 1999/08/03 01:51:13 janssen Exp $ */


#include <stdio.h>
#include <string.h>
#include "iluptype.h"
#include "shared.h"
#include "stubops.h"
#include "io.h"
#include "name.h"
#include "util.h"
#include "genarr.h"
#include "context.h"
#include "genseq.h"


PUBLIC boolean ar_is_a(Type t) {
    TypeKind kind;
    t = myUrType(t);
    kind = type_kind(t);
    return (kind==array_Type);
}


PUBLIC Type ar_assert(Type t) {
    t = myUrType(t);
    if (! ar_is_a(t)) fatal("should have been an array");
    return t;
}


PRIVATE char *
arraySpecialElemTypeName(Type t)
{
    switch (baseTypeDescription(t)->type) {
	case byte8_Type:
	    return "BytesA";
	case char8_Type:
	    return "Char8Array";
	case char16_Type:
	    return "Char16Array";
	default:
	    return 0;
    }
}


PRIVATE unsigned long
totalArrayLength(list dimList)
{
    unsigned long nDims	= (unsigned long) list_size(dimList);
    unsigned long result = 1;
    unsigned long i;

    for (i = 0; i < nDims; i++) {
	result *= (unsigned long) list_ref(dimList, i);
    }
    return result;
}



PUBLIC char * ar_typeDeclarator(Type t) {
    char* 		buf;
    TypeDescription	d;
    Type		eType;
    list		dimList;
    int			nDims;
    int			i;
    t = ar_assert(t);
    d = t->description;
    eType = d->structuredDes.array.type;
    dimList = d->structuredDes.array.dimensions;
    nDims = list_size(dimList);
    buf = typeDeclarator(eType);
    for (i = 0; i < nDims; i++) {
        buf = cat2(buf, "[]");
    }
    return buf;
}


PRIVATE char * arrShortName(Type t) {
    t = ar_assert(t);
    return easyShortTypeNameCleaned(t);
}


PRIVATE char * arrJName(Type t) {
    t = ar_assert(t);
    return packageDotStringJ(getContextT(t), arrShortName(t));
}


PRIVATE void
stDefineArrayTypeInput(Type t)
{
    TypeDescription	d	= t->description;
    Type		eType	= d->structuredDes.array.type;
    list		dimList	= d->structuredDes.array.dimensions;
    unsigned long	nDims	= list_size(dimList);
    char *		eName	= arraySpecialElemTypeName(eType);
    unsigned long       nVDims	= eName == 0 ? nDims : nDims - 1;
    unsigned long	i;

    printf("  public static %s inFunc(xerox.ilu.IluCall _call)\n", typeDeclarator(t));
    printf("    throws org.omg.CORBA.SystemException {\n");    

    printf("    _call.inArray();\n");
    printf("    %s _value = new %s", 
        typeDeclarator(t), 
        typeDeclarator(eType)
        );
    for (i = 0; i < nVDims; i++) {
	 printf("[%lu]", list_ref(dimList, i));
    }
    if (eName) {
	printf("[]");     
    }
    printf(";\n");

    for (i = 0; i < nVDims; i++) {
	unsigned long length = (unsigned long) list_ref(dimList, i);
        printLoopIndent(i);
        printf("    for (int _i%lu = 0; _i%lu < %lu; _i%lu++)\n", 
	       i,
	       i, length, i);
    }
    printLoopIndent(nVDims);
    printf("    _value");
    for (i = 0; i < nVDims; i++) {
         printf("[_i%lu]", i);
    }
    printf(" = ");
    if (eName) {
        printf("%s;\n", ioSpecialElemInPiece(eName,
            (unsigned long) list_ref(dimList, nVDims)));
    } else {
        printf("%s;\n", ioInPiece(eType)); 
    }
    printf("    _call.endArray();\n");
    printf("    return _value;\n");
    printf("  }\n\n");
}

PRIVATE void
stDefineArrayTypeOutput(Type t)
{
    TypeDescription	d	= t->description;
    Type		eType	= d->structuredDes.array.type;
    list		dimList	= d->structuredDes.array.dimensions;
    unsigned long	nDims	= list_size(dimList);
    char *		eName	= arraySpecialElemTypeName(eType);
    unsigned long	nVDims	= eName == 0 ? nDims : nDims - 1;
    unsigned long	totLen	= totalArrayLength(dimList);
    char		argName[1024];
    unsigned long	i,j;
    unsigned long 	length;

    printf("  public static void outFunc(xerox.ilu.IluCall _call, %s _value)\n", 
           typeDeclarator(t));
    printf("    throws org.omg.CORBA.SystemException {\n");
  
    printLoopIndent(2);
    printf("_call.outArray(%lu);\n",  totLen);

    for (i = 0; i < nVDims; i++) {
	length = (unsigned long) list_ref(dimList, i);
	printLoopIndent(i + 2);
	printf("if (_value");
        for (j = 0; j < i; j++) {
            printf("[_i%lu]", j);
        }
        printf(".length != %lu)\n", length);
        printLoopIndent(i + 3);
        printf("throw new org.omg.CORBA.BAD_PARAM(\"bad array length\");\n");
        
        printLoopIndent(i + 2);
        printf("for (int _i%lu = 0; _i%lu < %lu; _i%lu++) {\n",  i,
		i, length, i);
        printLoopIndent(i + 3);
        
    }

    printLoopIndent(nVDims + 2);
    strcpy(argName, "_value");
    for (i = 0; i < nVDims; i++) {
	char subscript[64];

	sprintf(subscript, "[_i%lu]", i);
	strcat(argName, subscript);
    }
    if (eName) { 
	printf("%s;\n", ioSpecialElemOutPiece(eName, argName,
				  (long) list_ref(dimList, nVDims)));
    } else {
	printf("%s;\n", ioOutPiece(eType, argName));
    }
    for (i = nVDims; i > 0; i--){
	printLoopIndent(i + 1);
        printf("}\n");
    }

    printf("    _call.endArray();\n");
    printf("  }\n\n");
}

PUBLIC char * ar_ioSzPiece(Type t, const char *arg) {
    char * piece;
    TypeDescription d;
    list dimList;
    t = myUrType(t);
    d = type_description(t);
    dimList = d->structuredDes.array.dimensions;
    if (list_size(dimList) == 1) {
        piece = arraySpecialElemTypeName(d->structuredDes.array.type);
        if (piece != NULL) {
            return (ioSpecialElemSzPiece(piece, arg, 
                (unsigned long) list_ref(dimList, 0)));
        }
    }
    return cat4(arrJName(t), ".szFunc(_call, ",  arg,  ")" );
}


PUBLIC char * ar_ioOutPiece(Type t, const char *arg) {
    char * piece;
    TypeDescription d;
    list dimList;
    t = myUrType(t);
    d = type_description(t);
    dimList = d->structuredDes.array.dimensions;
    if (list_size(dimList) == 1) {
        piece = arraySpecialElemTypeName(d->structuredDes.array.type);
        if (piece != NULL) {
            return (ioSpecialElemOutPiece(piece, arg,
                (unsigned long) list_ref(dimList, 0)));
        }
    }
    return cat4(arrJName(t), ".outFunc(_call, ",  arg,  ")" );
}


PUBLIC char * ar_ioInPiece(Type t) {
    char * piece;
    TypeDescription d;
    Type elementType;
    list dimList;
    t = myUrType(t);
    d = type_description(t);
    elementType = d->structuredDes.array.type;
    dimList = d->structuredDes.array.dimensions;
    if (list_size(dimList) == 1) {
        piece = arraySpecialElemTypeName(elementType);
        if (piece != NULL) {
            return (ioSpecialElemInPiece(piece,
                (unsigned long) list_ref(dimList, 0)));
        }
    }
    return (cat2(arrJName(t), ".inFunc(_call)" ));
}


PUBLIC void ar_enumElementTypes(Type t, TypeProc tp, refany rock) {
    TypeDescription d = type_description(t);
    Type elementType = d->structuredDes.array.type;
    tp(elementType, rock);
} /*ar_enumElementTypes*/


PRIVATE void
stDefineArrayTypeSize(Type t)
{
    TypeDescription	d	= t->description;
    Type		eType	= d->structuredDes.array.type;
    list		dimList	= d->structuredDes.array.dimensions;
    unsigned long	nDims	= list_size(dimList);
    char *		eName	= arraySpecialElemTypeName(eType);
    unsigned long	nVDims	= eName == 0 ? nDims : nDims - 1;
    unsigned long	totLen	= totalArrayLength(dimList);
    char		argName[1024];
    unsigned long	i;
    unsigned long 	length;

    printf("  public static int szFunc(xerox.ilu.IluCall _call, %s _value)\n", 
           typeDeclarator(t));
    printf("    throws org.omg.CORBA.SystemException {\n");
  
    printLoopIndent(2);
    printf("int _size = _call.szArray(%lu);\n", totLen);

    for (i = 0; i < nVDims; i++) {
	length = (unsigned long) list_ref(dimList, i);
	printLoopIndent(i + 2);
	printf("for (int _i%lu = 0; _i%lu < %lu; _i%lu++) {\n", 
		i,
		i, 
		length, i);
    }

    printLoopIndent(nVDims + 2);
    printf("_size += ");
    strcpy(argName, "_value");
    for (i = 0; i < nVDims; i++) {
	char subscript[64];
	sprintf(subscript, "[_i%lu]", i);
	strcat(argName, subscript);
    }

    if (eName) {
	printf("%s;\n", ioSpecialElemSzPiece(eName, argName,
	    (unsigned long) list_ref(dimList, nVDims)));
    } else {
	printf("%s;\n", ioSzPiece(eType, argName));
    }
    
    for (i = nVDims; i > 0; i--)  {
	printLoopIndent(i + 1);
        printf("}\n");
    }

    printf("    _call.endArray();\n");
    printf("    return _size;\n");
    printf("  }\n\n");
}


PRIVATE void
registerTheType(Type t, char * baseTypeName)
/* Registers the type of the array for variant support */
{
    TypeDescription d = type_description(t);
    Type elementType = d->structuredDes.array.type;
    list dimList = d->structuredDes.array.dimensions;
    int dimCnt = list_size(dimList);
    printf("    static{\n");
    printf("        int[] dims = new int[%d];\n", dimCnt);
    C_LOOP_BEGIN(dimList, Argument, a, idx, temp)
        printf("        dims[%d] = %lu;\n", 
            idx,
            list_ref(dimList, idx)
            );
    C_LOOP_END(idx)
    printf("        xerox.ilu.IluTypeRep.registerArrayType(\n");
    printf("            %s, //name\n", qoString(unresolvedIslTypeName(t)));
    printf("            %s, //islIfName\n", 
        qoString(interface_name(currentIfc)));
    printf("            %s, //islIfBrand\n", qoString(currentIfc->brand));
    printf("            %s, //uid\n", qoString(t->uid));
    printf("            %s, //baseUID\n", qoString(elementType->uid));
    printf("            %d, //dimcnt\n", dimCnt);
    printf("            dims); //dims\n");
    printf("    }//static\n");
}


PUBLIC void
ar_defineMain(Type t)
{
    IHandle ih = getContextT(t);
    TypeDescription d = type_description(t);
    Type elementType = d->structuredDes.array.type;
    list dimList    = d->structuredDes.array.dimensions;    
    char* baseTypeName = arrShortName(t);
    boolean doTheIO = TRUE;
    
    /* "Special" array types of ISL Byte, Character and Short Character */
    /* are handled inline and do not need a utility class for io. */

    if (list_size(dimList) == 1) { 
        if (arraySpecialElemTypeName(elementType) != NULL) {
            doTheIO = FALSE;
        }
    }    
    NewJavaFile(t->interface, ih, baseTypeName);
    LoadJavaClass(baseTypeName);
    printOpenDocComment("");
    printCommentLines("Class representing an ILU array type.\n");
    printCloseComment();
    printf("public final class %s { \n", baseTypeName);
    if (doTheIO) stDefineArrayTypeSize(t);
    if (doTheIO) stDefineArrayTypeOutput(t);
    if (doTheIO) stDefineArrayTypeInput(t);
    registerTheType(t, baseTypeName);
    printf("}// %s\n", baseTypeName);
}



/* end */

