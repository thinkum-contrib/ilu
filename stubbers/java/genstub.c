/* genstub.c */
/* Chris Jacobi, November 9, 1998 6:19 pm PST */
/* Last edited by Mike Spreitzer September 17, 1998 2:44 pm PDT */

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

/* $Id: genstub.c,v 1.72 1999/08/03 01:51:07 janssen Exp $ */


#include <stdio.h>
#include <string.h>
#include "iluptype.h"

#include "shared.h"
#include "stubops.h"
#include "io.h"
#include "name.h"
#include "util.h"
#include "genstub.h"
#include "genrecord.h"
#include "genunion.h" 
#include "context.h"
#include "genopt.h"
#include "genarr.h"
#include "genseq.h"
#include "genobj.h"
#include "genenum.h"
#include "genex.h"
#include "hlpcls.h"
 
/* I feel very uncomfortable with the printing
 * of constants.  ?? 
 */

PRIVATE void
printSQChar(const char *s)
/* Prints a character in quotes */
{
    printf("'%s'", s);
}


PRIVATE void
printString(const char *s)
/* No quotes */
{
    int i;
    for (i = 0; s[i]; i++) {
        int ch = (unsigned char) s[i];

        if (ch < ' ' || '~' < ch)
            printf("\\%03o", ch);
        else {
            if (ch == '"' || ch == '\\')
                putchar('\\');
            putchar(ch);
        }

    }
}
 

PRIVATE void
printQuotedString(const char *s)
{
    putchar('"');
    printString(s);
    putchar('"');
}


PRIVATE void
defineHolderClass(Type t)
{
    char* baseTypeName;
    char* holderClassName;
    char* argType;
    char* argInit;
    IHandle ih = getContextT(t);
    if (ih->p.genHldCls==0) return;
    
    baseTypeName = easyShortTypeNameCleaned(t);
    holderClassName = cat2(baseTypeName, ih->p.holderSuffix);
    argInit = typeInitializer(type_kind(t));
    NewJavaFile(t->interface, ih, holderClassName);
    argType = typeDeclarator(t);
    printf("/** holder class for %s */\n", baseTypeName);
    printf("public final class %s {\n", holderClassName);
    printf("    public %s value = %s;\n\n", argType, argInit);
    printf("    public %s() {\n", holderClassName);
    printf("    }\n\n");
    printf("    public %s(%s _arg) {\n", holderClassName, argType);
    printf("        this.value = _arg;\n");
    printf("    }\n\n");
    printf("}//%s\n", holderClassName);
}

 
PRIVATE void
badConst()
{
    fatal("bad const");
}


PRIVATE void
printInt32Part(ConstantValue v)
{
    if (v->val.i.sign < 0) {
        if (v->val.i.value == 0x80000000) {
            printf("-2147483648"); /* this is  -2**31 */
        } else {
            printf("-%lu", v->val.i.value);
        }
    } else {
        printf("%lu", v->val.i.value);
    }
}


PUBLIC void
printSpecialConst(ConstantValue v, Type t)
{
    /* This procedure is patterned according to the comment in 
     * the declaration of ilu_constantvalue_s in iluptype.h
     */
    TypeKind vtk = v->type; /* bad name for a TypeKind */
    TypeKind ctk = type_ur_kind(t); 
    switch (ctk) {
        case boolean_Type:
            if (vtk != boolean_Type) badConst();
            printf("%s", (v->val.b) ? "1" : "0");
            break;
        case enumeration_Type:
            if (vtk != shortcharacter_Type) badConst();
            printf("%s", qoString(v->val.s));
            break;
        case byte_Type:
            if (v->val.i.sign < 0) {
                if (v->val.i.value > 128) badConst();
                printf("(byte)-%lu", v->val.i.value);
            } else {
                if (v->val.i.value < 128) {
                    printf("(byte)%lu", v->val.i.value);
                } else {
                    printf("(byte)-%lu", (255 - v->val.i.value + 1));
                }
            }
            break;
        case int16_Type:
            if (vtk != int32_Type) badConst();
            printf("(short)");
            printInt32Part(v);
            break;
        case int32_Type:
            if (vtk != int32_Type) badConst();
            printInt32Part(v);
            break;
        case card32_Type:
        case card16_Type:
            if (vtk != int32_Type) badConst();
            if (v->val.i.value > 0x7FFF) {
                printf("(short)-%lu", (0xFFFF - v->val.i.value + 1));
            } else {
                printf("(short)%lu", v->val.i.value);
            }
            break;
        default:
            badConst();
    }
}


PUBLIC void
printConst(ConstantValue v, Type t)
{
    /* This procedure is patterned according to the comment in 
     * the declaration of ilu_constantvalue_s in iluptype.h
     */
    TypeKind vtk = v->type; /* bad name for a TypeKind */
    TypeKind ctk = type_ur_kind(t); 
    switch (ctk) {
        case boolean_Type:
            if (vtk != boolean_Type) badConst();
            printf("%s", (v->val.b) ? "true" : "false");
            break;
        case enumeration_Type:
            if (vtk != shortcharacter_Type) badConst();
            printf("%s.", enumerationJavaClass(myUrType(t))); 
                /* use class! (not type; type is int) */
            printString(javaizeIdent(v->val.s));
            break;
        case sequence_Type:
            if (vtk != shortcharacter_Type) badConst();
            printQuotedString(v->val.s);
            break;
        case real128_Type:
            fatal("real128 constants not yet impl");
        case real64_Type:
        case real32_Type:
            switch (vtk) {
                case real64_Type:
                    printf("%s%s.%se%ld", v->val.r.sign < 0 ? "-" : "",
                        v->val.r.value,
                        v->val.r.fraction ? v->val.r.fraction : "0",
                        v->val.r.exponent);
                    if (ctk == real64_Type) printf("%s", "D");
                    break;
                case int32_Type:
                    printInt32Part(v);
                    if (ctk == real64_Type) {
                        printf("%s", "D");
                    } else {
                        printf("%s", "F");
                    }
                    break;
                default:
                    badConst();
            }
            break;
        case byte_Type:
            if (v->val.i.sign < 0) {
                if (v->val.i.value > 128) badConst();
                printf("(byte)-%lu", v->val.i.value);
            } else {
                if (v->val.i.value < 128) {
                    printf("(byte)%lu", v->val.i.value);
                } else {
                    printf("(byte)-%lu", (255 - v->val.i.value + 1));
                }
            }
            break;
        case int16_Type:
            if (vtk != int32_Type) badConst();
            printf("(short)");
            printInt32Part(v);
            break;
        case int32_Type:
            if (vtk != int32_Type) badConst();
            printInt32Part(v);
            break;
        case int64_Type:
            if (vtk != int32_Type) badConst();
            printInt32Part(v);
            printf("%s", "L");
            break;
        case card32_Type:
        case card64_Type:
            if (vtk != int32_Type) badConst();
            if (v->val.i.value > 0x7FFFFFFF) {
                printf("-%lu", (0xFFFFFFFF - v->val.i.value + 1));
            } else {
                printf("%lu", v->val.i.value);
            }
            if (ctk == card64_Type) {printf("%s", "L");}
            break;
        case card16_Type:
            if (vtk != int32_Type) badConst();
            if (v->val.i.value > 0x7FFF) {
                printf("(short)-%lu", (0xFFFF - v->val.i.value + 1));
            } else {
                printf("(short)%lu", v->val.i.value);
            }
            break;
        default:
            badConst();
    }
}

PRIVATE char* 
constantClassShortName(Constant c) 
{
    while (c->import) {
        c = c->import;
    }
    return constantNameRJ(c);
}


PRIVATE void
map_constant(Constant c, void* x)
{
    char* classShortName;
    IHandle ih;
    if (c->import != 0) return;
    ih = getContext(c->interface, c->scoping);
    
    /* delay generating constants nested in idl-interfaces */
    if (ih->i.isIdlInterface) {
        if (ih->i.specialIdlConstants == 0) {
            ih->i.specialIdlConstants = new_list();
        }
        list_insert(ih->i.specialIdlConstants, c);
        return;
    }
    
    classShortName = constantClassShortName(c);
    NewJavaFile(c->interface, ih, classShortName);
    printf("public final class %s {\n", classShortName);
    printf("    public static final %s %s = ", 
           typeDeclarator(c->type), 
           "value"
           );
      printConst(c->value, c->type);
      printf(";\n");
    printf("} //%s\n", classShortName);
}

PUBLIC void
generateNestedConstants(IHandle ih, char* step) 
{
    IHandle idlfcIH = getChildContext(ih, step, 0);
    if (idlfcIH && idlfcIH->i.specialIdlConstants != 0) {
        LOOP_BEGIN(idlfcIH->i.specialIdlConstants, Constant, c, t1)
            printf("    public static final %s %s = ", 
                typeDeclarator(c->type), 
                constantClassShortName(c)
                );
            printConst(c->value, c->type);
            printf(";\n");
        LOOP_END()
    }
}

PRIVATE void
map_type(Type t)
{
    IHandle ih = getContextT(t);
    switch (type_kind(t)) {
        case enumeration_Type:
                enm_defineMain(t);
                if (ih->p.genHldCls) {defineHolderClass(t);}
                break;
        case record_Type:
                rec_defineMain(t);
                if (ih->p.genHldCls) {defineHolderClass(t);}
                break;
        case union_Type:
                un_defineMain(t);
                if (ih->p.genHldCls) {defineHolderClass(t);}
                break;
        case array_Type:
                ar_defineMain(t);
                if (ih->p.genHldCls) {defineHolderClass(t);}
                break;
        case sequence_Type:
                sq_defineMain(t);
                if (sq_isJString(t)) {
                    /* no holder class */
                    /* but maybe generate helper class */
                } else {
                    if (ih->p.genHldCls) {defineHolderClass(t);}
                }
                break;
        case object_Type:
                obj_defineMain(t);
                if (ih->p.genHldCls) {defineHolderClass(t);}
                break;
        case optional_Type:
                opt_defineMain(t);
                if (ih->p.genHldCls) {defineHolderClass(t);}
                break;
        case alias_Type:
                /* aliases are not mapped into unique types 
                 * the programmer must use the original name 
                 */
                break;
        default: 
                break;
    }
    if (useHelperClass(t)) {
       printHelperClass(t);
    }
}


PUBLIC void
generateStub(Interface ifc, list localtypes)
{
    /* do constants before types because they register nested constants */
    list_enumerate(ifc->constants, (EnumProc) map_constant, NULL); 
    list_enumerate(ifc->exceptions, (EnumProc) map_exception, NULL);
    list_enumerate(localtypes, (EnumProc) map_type, NULL); 
}


/* end */



