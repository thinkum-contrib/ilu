/* genseq.c */
/* Chris Jacobi, November 13, 1998 11:48 am PST */

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

/* $Id: genseq.c,v 1.20 1999/08/03 01:51:14 janssen Exp $ */


#include <stdio.h>
#include <string.h>
#include "iluptype.h"
#include "shared.h"
#include "stubops.h"
#include "io.h"
#include "name.h"
#include "util.h"
#include "genseq.h"
#include "context.h"


PUBLIC boolean sq_is_a(Type t) {
    TypeKind kind;
    t = myUrType(t);
    kind = type_kind(t);
    return (kind==sequence_Type);
}


PUBLIC void sq_assert(Type t) {
    if (! sq_is_a(t)) fatal("should have been a sequence");
}


PUBLIC char * sq_typeDeclarator(Type t) {
    TypeDescription	d;
    Type		eType;
    TypeKind            tKind;
    sq_assert(t);
    t = myUrType(t);
    d = t->description;
    eType = d->structuredDes.sequence.type;
    tKind = baseTypeDescription(eType)->type;
    if ((tKind == char8_Type) || (tKind == char16_Type))
        return ("java.lang.String");
    return cat2(typeDeclarator(eType), "[]");
}


PRIVATE char * seqClassShortName(Type t) {
    t = myUrType(t);
    return easyShortTypeNameCleaned(t);
}


PRIVATE char * seqClassXJName(Type t) {
    t = myUrType(t);
    return packageDotStringJ(getContextT(t), seqClassShortName(t));
}



PRIVATE void
stDefineSequenceTypeInput(Type t)
{
    TypeDescription	d	= t->description;
    Type		eType	= d->structuredDes.sequence.type;
    unsigned long       limit	= d->structuredDes.sequence.limit;

    printf("    public static %s _inFunc(xerox.ilu.IluCall _call)\n", 
           typeDeclarator(t));
    printf("            throws org.omg.CORBA.SystemException {\n");
  
    printf("        int _ln = _call.inSequence(%lu);\n",
           limit);

    /* Length check if bound sequence */
    if ((limit != 0) && (limit != 65535))
    {
        printf("        if (_ln > %lu)\n", limit);
        printf("            throw new xerox.ilu.IluBadParameterException(\"sequence length\");\n"); 
    }  
  
   printf("        %s _v = new %s;\n",  
           typeDeclarator(t), 
           setDimension(typeDeclarator(t), "_ln")
           );
         
      printf("        for (int _i = 0; _i < _ln; _i++) {\n");
    printf("            _v[_i] = %s;\n", ioInPiece(eType));

    printf("        }\n");
    printf("        _call.endSequence();\n");
    printf("        return _v;\n");
    printf("    } //_inFunc\n\n");  
}

PRIVATE void
stDefineSequenceTypeOutput(Type t)
{
    TypeDescription	d	= t->description;
    Type		eType	= d->structuredDes.sequence.type;
    unsigned long       limit	= d->structuredDes.sequence.limit;

    printf("    public static void _outFunc(xerox.ilu.IluCall _call, %s _v)\n", 
           typeDeclarator(t));
    printf("                throws org.omg.CORBA.SystemException {\n");

    /* Length check if bound sequence */
    if ((limit != 0) && (limit != 65535))
    {
        printf("        if (_v.length > %lu)\n", limit);
        printf("            throw new xerox.ilu.IluBadParameterException(\"bad sequence length\");\n"); 
    }  
  
    printf("        _call.outSequence(_v.length, %lu);\n",
           limit);

    printf("        for (int _i = 0; _i < _v.length; _i++) {\n");
    printf("            %s;\n", 
           ioOutPiece(eType, "_v[_i]"));

    printf("        }\n");
    printf("        _call.endSequence();\n");
    printf("    } //_outFunc\n\n"); 
}

PRIVATE void
stDefineSequenceTypeSize(Type t)
{
    TypeDescription	d	= t->description;
    Type		eType	= d->structuredDes.sequence.type;
    unsigned long       limit	= d->structuredDes.sequence.limit;

    printf("    public static int _szFunc(xerox.ilu.IluCall _call, %s _v)\n", 
           typeDeclarator(t));
    printf("                throws org.omg.CORBA.SystemException {\n");
  
    printf("        int _sz = _call.szSequence(_v.length, %lu);\n",
           limit);

    printf("        for (int _i = 0; _i < _v.length; _i++) {\n");
    printf("            _sz += %s;\n", 
           ioSzPiece(eType, "_v[_i]"));

    printf("        }\n");
    printf("        _call.endSequence();\n");
    printf("        return _sz;\n");
    printf("    } //_szFunc\n\n");
}



PUBLIC char *
ioSpecialElemSzPiece(const char *eName, const char *argName,
	                     const unsigned long length)
    /* Returns a marshaling sizing string piece for the special case of
     * one dimensional array of ISL BYTE, CHARACTER and SHORT CHARACTER.
     * Assumes arg designates the instance of T.
     */
{
    char * args = cat5("(", argName, ", ", formatLongUnsigned(length), ")");
    return cat3("_call.sz", eName, args);
}


PUBLIC char *
ioSpecialElemOutPiece(const char *eName, const char *argName,
	                   const unsigned long length)
    /* Returns a marshaling output string piece for the special case of
     * one dimensional array of ISL BYTE, CHARACTER and SHORT CHARACTER.
     * Assumes arg designates the instance of T.
     */
{
    char * args = cat5("(", argName, ", ", formatLongUnsigned(length), ")");
    return cat3("_call.out", eName, args);
}


PUBLIC char *
ioSpecialElemInPiece(const char *eName, const unsigned long length)
    /* Returns a marshaling input string piece for the special case of
     * one dimensional array of ISL BYTE, CHARACTER and SHORT CHARACTER.
     * Assumes arg designates the instance of T.
     */
{
    return cat5("_call.in", eName, "(", formatLongUnsigned(length), ")");
}


PRIVATE char *
sequenceSpecialElemTypeName(Type t)
{
    switch (baseTypeDescription(t)->type) {
	case byte8_Type:
	    return "BytesS";
	case char8_Type:
	    return "String8";
	case char16_Type:
	    return "String16";
	default:
	    return 0;
    }
}


PUBLIC char * sq_ioSzPiece(Type t, const char *arg) {
    char * piece;
    TypeDescription d;
    t = myUrType(t);
    d = type_description(t);
    piece = sequenceSpecialElemTypeName(d->structuredDes.sequence.type);
    if (piece != NULL)
        return (ioSpecialElemSzPiece(piece, arg,
            d->structuredDes.sequence.limit));
    return cat4(seqClassXJName(t), "._szFunc(_call, ",  arg,  ")" );
}


PUBLIC char * sq_ioOutPiece(Type t, const char *arg) {
    char * piece;
    TypeDescription d;
    t = myUrType(t);
    d = type_description(t);
    piece = sequenceSpecialElemTypeName(d->structuredDes.sequence.type);
    if (piece != NULL)
        return (ioSpecialElemOutPiece(piece, arg,
            d->structuredDes.sequence.limit));
    return cat4(seqClassXJName(t), "._outFunc(_call, ",  arg,  ")" );
}


PUBLIC char * sq_ioInPiece(Type t) {
    char * piece;
    TypeDescription d;
    t = myUrType(t);
    d = type_description(t);
    piece = sequenceSpecialElemTypeName(d->structuredDes.sequence.type);
    if (piece != NULL)
        return (ioSpecialElemInPiece(piece,
            d->structuredDes.sequence.limit));
    return cat2(seqClassXJName(t), "._inFunc(_call)" );
}


PUBLIC void sq_enumElementTypes(Type t, TypeProc tp, refany rock) {
    TypeDescription d = type_description(t);
    Type elementType = d->structuredDes.array.type;
    tp(elementType, rock);
} /*sq_enumElementTypes*/


PUBLIC boolean sq_isJString(Type t)
{
    TypeDescription d = type_description(t);
    Type eType = d->structuredDes.sequence.type;
    switch ( baseTypeDescription(eType)->type ) {
        case char8_Type:
        case char16_Type:
            return TRUE;
        default: 
            return FALSE;
    }
}


PRIVATE void
registerTheType(Type t)
/* Registers the type of the sequence for variant support */
{
    TypeDescription d = type_description(t);
    Type elementType = d->structuredDes.sequence.type;
    printf("    static{\n");
    printf("        xerox.ilu.IluTypeRep.registerSequenceType(\n");
    printf("            %s, //name\n", qoString(unresolvedIslTypeName(t)));
    printf("            %s, //islIfName\n", 
        qoString(interface_name(currentIfc)));
    printf("            %s, //islIfBrand\n", qoString(currentIfc->brand));
    printf("            %s, //uid\n", qoString(t->uid));
    printf("            %s, //baseUID\n", qoString(elementType->uid));
    printf("            %d); //limit\n", d->structuredDes.sequence.limit);
    printf("    }//static\n");
}


PUBLIC void
sq_defineMain(Type t)
{
    TypeDescription d = type_description(t);
    Type elementType = d->structuredDes.sequence.type;
    char* sqName = seqClassShortName(t);
    
    boolean doTheIO = TRUE;
    unsigned long limit = d->structuredDes.sequence.limit;
  
    /* "Special" sequence types of ISL Byte, Character and Short Character */
    /* are handled inline and do not need io. */

    switch (baseTypeDescription(elementType)->type) {
	case byte8_Type:
	    doTheIO = FALSE;
	    break;
	case char16_Type:
	case char8_Type:
	    doTheIO = FALSE;
	    if (limit==0) return;
	    break;
	default:
	    doTheIO = TRUE;
    }   
    NewJavaFile(t->interface, getContextT(t), sqName);
    LoadJavaClass(sqName);
    printOpenDocComment("");
    printCommentLines("Representing an ILU sequence type.\n");
    printCloseComment();
    printf("public final class %s { \n\n", sqName);
    if (doTheIO) stDefineSequenceTypeSize(t);
    if (doTheIO) stDefineSequenceTypeOutput(t);
    if (doTheIO) stDefineSequenceTypeInput(t);
    registerTheType(t);
    printf("} //%s\n", sqName);
}

