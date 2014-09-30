/* genrecord.c */
/* Chris Jacobi, December 17, 1998 8:48 am PST */
/* $Id: genrecord.c,v 1.37 1999/08/03 01:51:09 janssen Exp $ */


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


#include <stdio.h>
#include <string.h>
#include "iluptype.h"
#include "shared.h"
#include "stubops.h"
#include "io.h"
#include "context.h"
#include "name.h"
#include "util.h"
#include "genstub.h"
#include "genrecord.h"
#include "hlpcls.h"

#define recFields(td) td->structuredDes.record.fields


PUBLIC boolean rec_is_a(Type t) {
    t = myUrType(t);
    return (type_kind(t) == record_Type);
}


PUBLIC Type rec_assert(Type t) {
    t = myUrType(t);
    if (! rec_is_a(t)) fatal("should have been a record");
    return t;
}


PRIVATE char * recTypeShortName(Type t) {
    t = rec_assert(t);
    return easyShortTypeNameCleaned(t);
}


PRIVATE char * recTypeJName(Type t) {
    t = rec_assert(t);
    return packageDotStringJ(getContextT(t), recTypeShortName(t));
}


PUBLIC char * rec_typeDeclarator(Type t) {
    t = rec_assert(t);
    return recTypeJName(t);
}


PUBLIC char * rec_ioSzPiece(Type t, const char *arg) {
    t = rec_assert(t);
    return cat4(helperClassName(t), "._szFunc(_call, ",  arg,  ")" );
}


PUBLIC char * rec_ioOutPiece(Type t, const char *arg) {
    t = rec_assert(t);
    return cat4(helperClassName(t), "._outFunc(_call, ",  arg,  ")" );
}


PUBLIC char * rec_ioInPiece(Type t) {
    t = rec_assert(t);
    return cat2(helperClassName(t), "._inFunc(_call)" );
}


PUBLIC void rec_enumElementTypes(Type t, TypeProc tp, refany rock) {
    list fields;
    TypeDescription d;
    t = rec_assert(t);
    d = type_description(t);
    fields = recFields(d);
    LOOP_BEGIN(fields, Argument, a, temp)
        Type argType = a->type;
        tp(argType, rock);
    LOOP_END()
} /*rec_enumElementTypes*/


PUBLIC void
printRecordConstructorFormalArgs(Type t)
{
    TypeDescription d = type_description(t);
    list fields = recFields(d);
    C_LOOP_BEGIN(fields, Argument, a, count, temp)
        /* Print each argument to the record class constructor */
        if ( count ) printf(", ");
        printf("%s %s", typeDeclarator(a->type), argumentName(a));
    C_LOOP_END(count)
}


PUBLIC void
printRecordAssignCorresponding(Type t, char* leftPrefix, char* rightPrefix)
{
    TypeDescription d = type_description(t);
    list fields = recFields(d);
    LOOP_BEGIN(fields, Argument, a, temp)
        /* Assign each argument */
        char* aName = argumentName(a);
        printf("%s%s = %s%s;\n", leftPrefix, aName, rightPrefix, aName);
    LOOP_END()
}


static boolean subclassable = 1;

PRIVATE void
printConstructors(Type t, char* rTypeName)
{
    if (subclassable) {
        printf("    //to force ILU to allocate subclasses\n");
        printf("    protected static java.lang.Class _theClass = null;\n\n");
        
        printf("    public void _touch() {\n");
        printf("    } //_touch\n\n");
    }
    
    printf("    //avoid unless corba compatibility required\n");
    printf("    public %s() {\n", rTypeName);
    printf("    } //%s\n\n", rTypeName);

    printf("    //not standard corba\n");
    printf("    public static %s alloc_%s() {\n", rTypeName, rTypeName);
    if (subclassable) {
        printf("        if (_theClass!=null) {\n");
        printf("            try {\n");
        printf("                return (%s) _theClass.newInstance();\n", rTypeName);
        printf("            } catch (java.lang.Exception _e) {\n");
        printf("            }\n");
        printf("        }\n");
    }
    printf("        return new %s();\n", rTypeName);
    printf("    } //alloc_%s\n\n", rTypeName);
    
    printf("    //avoid unless corba compatibility required\n");
    printf("    public %s(", rTypeName);
    printRecordConstructorFormalArgs(t);
    printf(") {\n");
    printRecordAssignCorresponding(t, "        this.", "");
    printf("    } //constructor\n\n");
    
    printf("    //not standard corba\n");
    printf("    public static %s alloc_%s(", rTypeName, rTypeName);
    printRecordConstructorFormalArgs(t);
    printf(") {\n");
    printf("        %s _r = alloc_%s();\n", rTypeName, rTypeName);
    printRecordAssignCorresponding(t, "        _r.", "");
    if (subclassable) {
        printf("        _r._touch();\n");
    }
    printf("        return _r;\n");
    printf("    } //alloc_%s\n\n", rTypeName);

}


PUBLIC void
printRecordFieldDecls(Type t, boolean initializers)
{
    TypeDescription d = type_description(t);
    list fields = recFields(d);
    LOOP_BEGIN(fields, Argument, a, temp)
        /* Print the declaration of a record member in the record class */
        printf("    public %s %s", typeDeclarator(a->type), argumentName(a));
        if (initializers) {
            printf(" = %s", typeInitializer(type_kind(myUrType(a->type))));
        } 
        printf(";\n");
    LOOP_END()
    printf("\n");
}


PRIVATE void
printSzRecords(Type t, char * rTypeName)
{
    TypeDescription d = type_description(t);
    list fields = recFields(d);
    printf("    //internal to ILU only\n");
    printf("    public static int _szFunc(xerox.ilu.IluCall _call, %s _r)\n", 
                rTypeName);
    printf("                throws org.omg.CORBA.SystemException {\n");
    printf("        int _sz = _call.szRecord();\n");
    LOOP_BEGIN(fields, Argument, a, temp)
        char * szPiece;
        char * argName;
        argName = cat2("_r.", argumentName(a));
        szPiece = ioSzPiece(a->type, argName);
        printf("        _sz = _sz + %s;\n", szPiece);
    LOOP_END()
    printf("        _call.endRecord();\n");
    printf("        return _sz;\n");
    printf("    } //_szFunc\n\n");
}


PRIVATE void
printOutRecords(Type t, char * rTypeName)
{
    TypeDescription d = type_description(t);
    list fields = recFields(d);
    printf("    //internal to ILU only\n");
    printf("    public static void _outFunc(xerox.ilu.IluCall _call, %s _r)\n", 
                rTypeName);
    printf("                throws org.omg.CORBA.SystemException {\n");
    printf("        _call.outRecord();\n");
    LOOP_BEGIN(fields, Argument, a, temp)
        char * outPiece;
        char * argName;
        argName = cat2("_r.", argumentName(a));
        outPiece = ioOutPiece(a->type, argName);
        printf("        %s;\n", outPiece);
    LOOP_END()
    printf("        _call.endRecord();\n");
    printf("    } //_outFunc\n\n");
}



PRIVATE void
printInRecords(Type t, char * rTypeName)
{
    TypeDescription d = type_description(t);
    list fields = recFields(d);
    printf("    //internal to ILU only\n");
    printf("    public static %s _inFunc(xerox.ilu.IluCall _call)\n", rTypeName);
    printf("                throws org.omg.CORBA.SystemException {\n");
    printf("        %s _r = %s.alloc_%s();\n", rTypeName, rTypeName, rTypeName);
    printf("        _call.inRecord();\n");
    LOOP_BEGIN(fields, Argument, a, temp)
        char * inPiece;
        char * argName;
        argName = argumentName(a);
        inPiece = ioInPiece(a->type);
        printf("        _r.%s = %s;\n", argName, inPiece);
    LOOP_END()
    printf("        _call.endRecord();\n");
    if (subclassable) {
        printf("        _r._touch();\n");
    }
    printf("        return _r;\n");
    printf("    } //_inFunc\n\n");
}


PRIVATE void
registerRecordFields(Type t, char* tVar)
/* Registers the type of the record for variant support */
{
    TypeDescription d = type_description(t);
    C_LOOP_BEGIN(recFields(d), Argument, a, fidx, temp)
        printf("        %s.registerRecordField(%d, %s, %s);\n", 
            tVar,
            fidx,  /*index*/
            qoString(argument_name(a)),  /*fldIslName*/
            qoString(a->type->uid)  /*fldUID*/
            );
    C_LOOP_END(fidx)
}


PUBLIC void rec_helpInnerCallback(Type t)
{
    char* rTypeName;
    TypeDescription d;
    list fields;
    t = myUrType(t);
    d = type_description(t);
    fields = recFields(d);
    rTypeName = recTypeShortName(t);
    printf("    static {\n");
    printf("        xerox.ilu.IluTypeRep __t = null;\n");
    printf("        __t = xerox.ilu.IluTypeRep.registerRecordType(\n");
    printf("            %s, //name\n", qoString(unresolvedIslTypeName(t)));
    printf("            %s, //islIfName\n",
        qoString(interface_name(currentIfc)));
    printf("            %s, //islIfBrand\n",qoString(currentIfc->brand));
    printf("            %s, //uid\n", qoString(t->uid));
    printf("            %d //cnt\n", list_size(fields));
    printf("            );\n");
    registerRecordFields(t, "__t");
    printf("        __t.finish();\n");
    printf("        __t = null;\n");
    printf("    }\n\n");
    
    printSzRecords(t, rTypeName);
    printOutRecords(t, rTypeName);
    printInRecords(t, rTypeName);
    
} /* rec_helpInnerCallback */


PUBLIC void
rec_defineMain(Type t)
{
    IHandle ih = getContextT(t);
    char* rTypeName;
    TypeDescription d;
    t = myUrType(t);
    d = type_description(t);
    rTypeName = recTypeShortName(t);
    NewJavaFile(t->interface, ih, rTypeName);
    LoadJavaClass(rTypeName);
    printOpenDocComment("");
    printCommentLines("Class representing an ILU record type.\n");
    printCloseComment();
    printf("public class %s { \n\n", rTypeName);
    printRecordFieldDecls(t, TRUE);
    printConstructors(t, rTypeName);
    
    printf("} //%s\n", rTypeName);
}

/* end */
