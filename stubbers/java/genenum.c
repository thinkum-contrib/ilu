/* genenum.c */
/* Chris Jacobi, December 23, 1998 11:33 am PST */

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

/* $Id: genenum.c,v 1.31 1999/08/03 01:51:13 janssen Exp $ */


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
#include "genenum.h"
#include "hlpcls.h"


PUBLIC boolean enm_is_a(Type t) {
    t = myUrType(t);
    return (type_kind(t) == enumeration_Type);
}


PUBLIC Type enm_assert(Type t) {
    t = myUrType(t);
    if (! enm_is_a(t)) fatal("should have been an enumeration");
    return t;
}


PRIVATE char * enmShortName(Type t) {
    t = enm_assert(t);
    return easyShortTypeNameCleaned(t);
}


PRIVATE char * enmJName(Type t) {
    t = enm_assert(t);
    return packageDotStringJ(getContextT(t), enmShortName(t));
}


PUBLIC char * enumerationJavaClass(Type t) {
    return enmJName(t);
}


PUBLIC char * enm_typeDeclarator(Type t) {
    enm_assert(t);
    return enmJName(t);
}


PUBLIC char * enumerationInt(Type t, ConstantValue v) {
    TypeKind vtk = v->type; /* bad name for a TypeKind */
    TypeKind ctk = type_ur_kind(t); 
    char* baseTypeName = enmJName(t);
    enm_assert(t);
    switch (ctk) {
        case enumeration_Type:
            if (vtk != shortcharacter_Type) fatal("bad enumeration const");
            return cat3(baseTypeName, "._", javaizeIdent(v->val.s));
        default: fatal("bad enumeration const");
    }
    return 0; /* keep the compiler happy */
}


PUBLIC char * enm_ioSzPiece(Type t, const char *arg) {
    t = enm_assert(t);
    return cat3("_call.szEnum((", arg, ").value())");
}


PUBLIC char * enm_ioOutPiece(Type t, const char *arg) {
    t = enm_assert(t);
    return cat3("_call.outEnum((", arg, ").value())");
}


PUBLIC char * enm_ioInPiece(Type t) {
    t = enm_assert(t);
    return cat2(enmJName(t), ".from_int(_call.inEnum())");
}


PRIVATE int
enumMax(Type t)
{
    list enum_field = type_description(t)->structuredDes.enumeration;
    int xval = ((EnumField)list_ref(enum_field, 0))->id;
    list l = list_cdr(enum_field);
    cardinal i;
    for (i = 0; i < list_size(l); i++) 
        if (xval < ((EnumField)list_ref(l, i))->id)
            xval = ((EnumField)list_ref(l, i))->id;

    return xval;
}


PRIVATE boolean
hasHoles(Type t, int maxval)
{
    list enum_field = type_description(t)->structuredDes.enumeration;
    int sz = list_size(enum_field);
    return (maxval+1 != sz);
}


PRIVATE void
defineEnumLiterals1(Type t)
{
    TypeDescription d = type_description(t);
    list fields = d->structuredDes.enumeration;
    LOOP_BEGIN(fields, EnumField, e, temp)
        printf("    public static final int _%s = %d;\n", 
            enumFieldName(e),  
            e->id
            );
    LOOP_END()
}



PRIVATE void
defineEnumLiterals2(Type t)
{
    char * baseTypeName = enmShortName(t);
    TypeDescription d = type_description(t);
    list fields = d->structuredDes.enumeration;
    LOOP_BEGIN(fields, EnumField, e, temp)
        printf("    public static final %s %s = __define(_%s);\n", 
            baseTypeName,
            enumFieldName(e),  
            enumFieldName(e)
            );
    LOOP_END()
}


PRIVATE void
registerEnumLiterals(Type t, char* tVar)
{
    char * baseTypeName = enmShortName(t);
    TypeDescription d = type_description(t);
    list fields = d->structuredDes.enumeration;
    C_LOOP_BEGIN(fields, EnumField, e, idx, temp)
        printf("        %s.registerEnumerationElement(%d, %s, %s._%s);\n", 
            tVar,
            idx,  /*elNum*/
            qoString(e->name),  /*elIslName  (isl's name space)*/
            baseTypeName,
            enumFieldName(e)  /*elVal*/
            );
    C_LOOP_END(idx)
} /*registerEnumLiterals*/


PUBLIC void enm_helpInnerCallback(Type t)
{
    list enum_field = type_description(t)->structuredDes.enumeration;
    printf("    static {\n");
    printf("        xerox.ilu.IluTypeRep __t = null;\n");
    printf("        __t = xerox.ilu.IluTypeRep.registerEnumerationType(\n");
    printf("            %s, //name\n", qoString(unresolvedIslTypeName(t)));
    printf("            %s, //islIfName\n",
        qoString(interface_name(currentIfc)));
    printf("            %s, //islIfBrand\n",qoString(currentIfc->brand));
    printf("            %s, //uid\n", qoString(t->uid));
    printf("            %d //elCnt\n", list_size(enum_field));
    printf("            );\n");
    registerEnumLiterals(t, "__t");
    printf("        __t.finish();\n");
    printf("        __t = null;\n");
    printf("    } //static\n");
} /* enm_helpInnerCallback */ 


PUBLIC void
enm_defineMain(Type t)
{
    IHandle ih = getContextT(t);
    char* baseTypeName = enmShortName(t);
    int eMax = enumMax(t);
    boolean holes = hasHoles(t, eMax);
    
    NewJavaFile(t->interface, ih, baseTypeName);
    LoadJavaClass(baseTypeName);
    printOpenDocComment("");
    printCommentLines("Class representing an ILU enumeration type.\n");
    printCloseComment();
    printf("public final class %s implements java.io.Serializable, xerox.ilu.IluResolving {\n\n", baseTypeName);
    
    printf("    private int __val;\n");
    if (holes) {
        printf("    private static xerox.basics.IntTab __tab = new xerox.basics.IntTab();\n");
        printf("    private static int __hashOffset = __tab.hashCode();\n\n");
    } else {
        printf("    private static %s[] __a = new %s[%d];\n", 
            baseTypeName, baseTypeName, eMax+1);
        printf("    private static int __hashOffset = __a.hashCode();\n\n");
    }
    printf("    protected %s(){}\n", baseTypeName);
    printf("    private static final %s __define(int __i) {\n", baseTypeName);
    printf("        %s __x = new %s();\n", baseTypeName, baseTypeName);
    printf("        __x.__val = __i;\n");
    if (holes) {
        printf("        __tab.insert(__i, __x);\n");
    } else {
        printf("        __a[__i] = __x;\n");
    }
    printf("        return __x;\n");
    printf("    } //__define\n\n");
    
    printf("    public static final %s from_int(int __i)\n", baseTypeName);
    printf("            throws org.omg.CORBA.SystemException {\n");
    if (holes) {
        printf("        java.lang.Object __ob = __tab.unmonitoredFetch(__i);\n");
        printf("        if (__ob == null) {\n");
        printf("            throw new org.omg.CORBA.BAD_PARAM();\n");
        printf("        }\n");
        printf("        return (%s) __ob;\n", baseTypeName);
    } else {
        printf("        if (__i < 0 || __i > %d) {\n", eMax);
        printf("            throw new org.omg.CORBA.BAD_PARAM();\n");
        printf("        }\n");
        printf("        return __a[__i];\n");
    }
    printf("    } //from_int\n\n");
    
    printf("    public final int value() {\n");
    printf("        return __val;\n");
    printf("    } //value\n\n");
    
    defineEnumLiterals1(t);
    printf("\n");
    defineEnumLiterals2(t);
    printf("\n");
    
    printf("    /** Object procedure */\n");
    printf("    public int hashCode() {\n");
    /* adding an offset reduces hash collisions with different types */
    printf("        return (__val + __hashOffset);\n");
    printf("    } //hashCode\n\n");
    
    printf("    /** Object procedure */\n");
    printf("    public boolean equals(java.lang.Object __o) {\n");
    printf("        if (this == __o) return true;\n");
    printf("        if (__o instanceof %s) {\n", baseTypeName);
    printf("            return (__val == ((%s)__o).__val);\n", baseTypeName);
    printf("        }\n");
    printf("        return false;\n");
    printf("    } //equals\n\n");
    
    printf("    /** IluResolving. Used by (de) serialization */\n");
    printf("    public java.lang.Object readResolve() throws java.io.ObjectStreamException {\n");
    printf("        return from_int(__val);\n");
    printf("    } //readResolve\n\n");
    
    printf("} //%s\n", baseTypeName);

    /* no holder class */
}


/* end */
