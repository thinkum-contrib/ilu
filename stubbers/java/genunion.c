/* genunion.c */
/* Chris Jacobi, December 17, 1998 8:54 am PST */

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

/* $Id: genunion.c,v 1.38 1999/08/03 01:51:10 janssen Exp $ */


#include <stdio.h>
#include <string.h>
#include "iluptype.h"
#include "shared.h"
#include "stubops.h"
#include "io.h"
#include "name.h"
#include "util.h"
#include "genunion.h"
#include "genenum.h"
#include "genopt.h"
#include "hlpcls.h"
#include "context.h"

 
PUBLIC boolean un_is_a(Type t) {
    TypeKind kind;
    t = myUrType(t);
    kind = type_kind(t);
    return (kind==union_Type);
}


PUBLIC Type un_assert(Type t) {
    t = myUrType(t);
    if (! un_is_a(t)) fatal("should have been an union");
    return t;
}


PRIVATE char * unShortName(Type t) {
    t = un_assert(t);
    return easyShortTypeNameCleaned(t);
}



PUBLIC char * un_typeDeclarator(Type t) {
    t = un_assert(t);
    return packageDotStringJ(getContextT(t), unShortName(t));
}


PRIVATE char * unTypeIOClassName(Type t) {
    t = un_assert(t);
    return packageDotStringJ(getContextT(t), helperClassShortName(t));
}


PRIVATE Type discriminatorType(Type ut) 
{
    TypeDescription d = type_description(un_assert(ut));
    return myUrType(d->structuredDes.uniond.discriminator_type);
}


PRIVATE Argument defaultArm(Type ut) 
{
    ut = un_assert(ut);
    return type_description(ut)->structuredDes.uniond.default_arm;
}


PRIVATE boolean othersAllowed(Type ut) 
{
    ut = un_assert(ut);
    return type_description(ut)->structuredDes.uniond.others_allowed;
}


PRIVATE list
uTypes(Type ut)
{
    ut = un_assert(ut);
    return type_description(ut)->structuredDes.uniond.types;
}


typedef struct {
    Argument defaultArm;
    Type unionType;
    int count;
    int defaultArmIdx;
    boolean isBoolCase;
    /*in bool case only*/ Argument targ, farg;
} UTRockType;


PRIVATE char*
discriminatorSize(Type ut)
{
    Type dt = discriminatorType(ut);
    switch (type_ur_kind(dt)) {
        case boolean_Type:
        case byte_Type:
        case char8_Type:
            return "1";
        case enumeration_Type:
            return "4";
        case char16_Type:
        case int16_Type:
        case card16_Type:
            return "2";
        default: 
            return "4";
    }
}


PRIVATE char*
castDiscToInt(Type ut, char* discPiece)
{
    Type dt = discriminatorType(ut);
    switch (type_ur_kind(dt)) {
        case boolean_Type:
            return cat3("(", discPiece, " ? 1 : 0)");
        case int32_Type:
            return discPiece;
        case enumeration_Type:
            return cat2(discPiece, ".value()");
        default: 
            break;
    }
    return cat2("(int)", discPiece);
}


PRIVATE char*
castIntToDisc(Type ut, char* discPiece)
{
    Type dt = discriminatorType(ut);
    switch (type_ur_kind(dt)) {
        case boolean_Type:
            return cat3("(", discPiece, " == 1)");
        case int32_Type:
            return discPiece;
        case enumeration_Type:
            return cat5("(", enm_typeDeclarator(dt), ".from_int(", discPiece, "))");
        default: 
            break;
    }
    return cat4("(", typeDeclarator(discriminatorType(ut)), ")", discPiece); 
}


PUBLIC char * un_ioSzPiece(Type ut, const char *arg) {
    ut = un_assert(ut);
    return cat4( unTypeIOClassName(ut), "._szFunc(_call, ", arg, ")");
}


PUBLIC char * un_ioOutPiece(Type ut, const char *arg) {
    ut = un_assert(ut);
    return cat4( unTypeIOClassName(ut), "._outFunc(_call, ", arg, ")");
}


PUBLIC char * un_ioInPiece(Type ut) {
    ut = un_assert(ut);
    return cat2( unTypeIOClassName(ut), "._inFunc(_call)");
}


PUBLIC void un_enumElementTypes(Type t, TypeProc tp, refany rock) 
{
    tp(discriminatorType(t), rock);
    LOOP_BEGIN(uTypes(t), Argument, arg, t0)
        Type argType = arg->type;
        tp(argType, rock);
    LOOP_END()
} /*un_enumElementTypes*/


PRIVATE void
myPrintConst(ConstantValue v, Type t) 
/* like printConst, except it uses the int type for enumerations */
/* t is discriminator type; not union type */
{
    if (type_ur_kind(t)==enumeration_Type) {
        printf("%s", enumerationInt(t, v));
    } else {
        printConst(v, t);
    }
}


PRIVATE char * mySwitch(Type discType, char * arg)
{
    if (type_ur_kind(discType)==enumeration_Type) {
        return cat2(arg, ".value()");
    } else {
        return arg;
    }
}


PRIVATE void
printCasesOnly(Argument arg, Type ut) 
{
    LOOP_BEGIN(arg->values, ConstantValue, v, t1)
        printf("            case ");
        myPrintConst(v, discriminatorType(ut)); 
        printf(":\n");
    LOOP_END()
}


PRIVATE void
printCasesNormal(Argument arg, Type ut, int i) 
{
    if (arg != defaultArm(ut)) {
        if (list_size(arg->values)) {
            printCasesOnly(arg, ut);
        } else {
            printf("            case %d: \n", i);
        }
    } else { /* default arm */
        printf("            default: \n");
    }
}


PRIVATE void
printGetMethod(Argument arg, UTRockType * rock, char * casePiece)
{
    Type ut = rock->unionType;
    printf("    public %s get_%s()\n", typeDeclarator(arg->type), casePiece);
    printf("            throws org.omg.CORBA.BAD_OPERATION {\n");
    printf("        if (!_init) {_throw();}\n");
    if (rock->isBoolCase) {
       printf("        if(%s_dis) _throw();\n",
           ( (arg == rock->targ) ? "" : "!")
           );
    } else { /* not boolean */
        printf("        switch(%s) {\n", 
            mySwitch(discriminatorType(ut), "_dis")
            );
        if (arg != rock->defaultArm) {
            if (list_size(arg->values)) {
                printCasesOnly(arg, ut);
            } else {
                printf("            case %d: \n", rock->count);
            }
            printf("                break;\n");
            printf("            default: \n");
            printf("                _throw();\n");
        } else { /* default arm */
            LOOP_BEGIN(uTypes(ut), Argument, arg1, t1)
                if (arg1!=arg) printCasesOnly(arg1, ut);
            LOOP_END()
            printf("                _throw();\n");
            printf("            default: break;\n");
        }
        printf("        }\n");
    }
    printf("        return (%s) %s;\n",
        typeDeclarator(arg->type),     
        fromObject(arg->type, "_val")
        );
    
    printf("    } //get_%s\n\n", casePiece);
}


PRIVATE void
printAllocOther(UTRockType * rock)
{
    Type ut = rock->unionType;
    char* unionName = unShortName(ut); 
    printf("    public static %s allocOther(%s _dis)\n", 
        unionName, 
        typeDeclarator(discriminatorType(ut))
        );
    printf("            throws org.omg.CORBA.BAD_OPERATION {\n");
    printf("        %s _res = new %s();\n", unionName, unionName);
    printf("        _res.setOther(_dis);\n");
    printf("        return _res;\n");
    printf("    } //allocOther\n\n");
}


PRIVATE void
printAllocMethodA(Argument arg, UTRockType * rock, char * casePiece)
{
    Type ut = rock->unionType;
    char* unionName = unShortName(ut); 
    printf("    public static %s alloc_%s(%s _dis, %s _v)\n", 
        unionName, 
        casePiece, 
        typeDeclarator(discriminatorType(ut)),
        typeDeclarator(arg->type)
        );
    printf("            throws org.omg.CORBA.BAD_OPERATION {\n");
    printf("        %s _res = new %s();\n", unionName, unionName);
    printf("        _res.set_%s(_dis, _v);\n", casePiece);
    printf("        return _res;\n");
    printf("    } //alloc_%s\n\n", casePiece);
}


PRIVATE void
printADiscValue(Argument arg, UTRockType * rock, boolean asInt)
{
    if (arg == rock->defaultArm) {
        fatal("don't know disc value for default arm");
    }
    if (list_size(arg->values)) {
        if (asInt) {
            myPrintConst(
                list_ref(arg->values, 0), 
                discriminatorType(rock->unionType));
        } else {
            printConst(
                list_ref(arg->values, 0), 
                discriminatorType(rock->unionType));
        } 
    } else {
        printf("%d", rock->count);
    }
}


PRIVATE void
printAllocMethodB(Argument arg, UTRockType * rock, char * casePiece)
{
    char* unionName = unShortName(rock->unionType); 
    if (arg != rock->defaultArm) {
        printf("    public static %s alloc_%s(%s _v)\n", 
            unionName, 
            casePiece, 
            typeDeclarator(arg->type)
            );
	printf("            throws org.omg.CORBA.BAD_OPERATION {\n");
        printf("        return alloc_%s(", casePiece);
          printADiscValue(arg, rock, 0);
          printf(", _v);\n");
        printf("    } //alloc_%s\n\n", casePiece);
    }
}


PRIVATE void
printSetOther(UTRockType * rock)
{
    Type ut = rock->unionType;
    printf("    public void setOther(%s _d)\n", 
        typeDeclarator(discriminatorType(ut))
        );
    printf("            throws org.omg.CORBA.BAD_OPERATION {\n");
    if (rock->isBoolCase) {
       if (rock->targ) printf("        if(_d) _throw();\n");
       if (rock->farg) printf("        if(!_d) _throw();\n");
    } else { /* not boolean */
        printf("        switch (%s) {\n",
            mySwitch(discriminatorType(ut), "_d")
            );
        LOOP_BEGIN(uTypes(ut), Argument, arg1, t0)
            printCasesOnly(arg1, ut);
        LOOP_END()
        printf("                _throw();\n");
        printf("            default:\n");
        printf("                break;\n");
        printf("        }\n");
    }
    printf("        _dis = _d;\n");
    printf("        _val = null;\n");
    printf("        _init = true;\n");
    printf("    } //setOther\n\n");
}


PRIVATE void
printSetMethodA(Argument arg, UTRockType * rock, char * casePiece)
{
    Type ut = rock->unionType;
    printf("    public void set_%s(%s _d, %s _v)\n", 
        casePiece,
        typeDeclarator(discriminatorType(ut)), 
        typeDeclarator(arg->type)
        );
    printf("            throws org.omg.CORBA.BAD_OPERATION {\n");
    if (rock->isBoolCase) {
       printf("        if(%s_d) _throw();\n",
           ( (arg == rock->targ) ? "!" : "")
           );
    } else { /* not boolean */
        printf("        switch (%s) {\n",
            mySwitch(discriminatorType(ut), "_d")
            );
        if (arg != rock->defaultArm) {
            if (list_size(arg->values)) {
                printCasesOnly(arg, ut);
            }
	    else {
                printf("            case %d: \n", rock->count);
	    }
	    printf("                break;\n");
	    printf("            default: _throw();\n");
	    printf("        }\n");
        } else { /* default arm */
            LOOP_BEGIN(uTypes(ut), Argument, dummy, t0)
                if (dummy!=arg) printCasesOnly(dummy, ut);
	    LOOP_END()  
	    printf("                _throw();\n");
	    printf("            default:\n");
	    printf("                break;\n");
	    printf("        }\n");
        }
    }
    printf("        _dis = _d;\n");
    printf("        _val = %s;\n",
        toObject(arg->type, "_v")
        );
    printf("        _init = true;\n");
    printf("    } //set_%s\n\n", casePiece);
}


PRIVATE void
printSetMethodB(Argument arg, UTRockType * rock, char * casePiece)
{
    if (arg != rock->defaultArm) {
        printf("    public void set_%s(%s _v) {\n", 
            casePiece, 
            typeDeclarator(arg->type));
        printf("        set_%s(", casePiece);
          printADiscValue(arg, rock, 0);
          printf(", _v);\n");
        printf("    } //set_%s\n\n", casePiece);
    }
}


PRIVATE char*
namePiece(Argument arg)
{
    char * casePiece = argumentName(arg);
    if (casePiece == NULL) {
	/* no case name; use type as case name */
	casePiece = typeNameUnresolvedButClean(arg->type);
    }
    return casePiece;
}


PRIVATE void
print1UnionMethod(Argument arg, UTRockType * rock)
/*
 * casePiece is initialized to the name of the union 
 * field being refered to. The ILU syntax allows union fields to omit a
 * field name. In such cases the name of the type is used instead.
 */
{
    char * casePiece = namePiece(arg);

    printAllocMethodA(arg, rock, casePiece);
    printAllocMethodB(arg, rock, casePiece);
    printGetMethod(arg, rock, casePiece);
    printSetMethodA(arg, rock, casePiece);
    printSetMethodB(arg, rock, casePiece);
    
    rock->count++;
}


PRIVATE void
printConstructor(char * name)
{
    printf("    public %s() {\n", name);
    printf("        _init = false;\n");
    printf("        _val = null;\n");
    printf("    } //constructor\n\n");
}


PRIVATE void
printDiscriminatorMethod(Type ut)
{
    printf("    public %s discriminator()\n",
        typeDeclarator(discriminatorType(ut))
        );
    printf("                throws org.omg.CORBA.BAD_OPERATION {\n");
    printf("        if (!_init) {\n");
    printf("            _throw();\n");
    printf("        }\n");
    printf("        return _dis;\n");
    printf("    } //discriminator\n\n");
}

PRIVATE void
printOthers(Type ut)
{
    if (othersAllowed(ut)) {
        printf("              _v = %s.allocOther(_d);\n", unShortName(ut));
    } else {
        printf("              _call.endUnion();\n");
        printf("              throw new org.omg.CORBA.BAD_OPERATION();\n");
    }
}

PRIVATE void
printInFunc(UTRockType* rock)
{
    char * classPrefix;
    Type ut = rock->unionType;
    
    classPrefix = cat2(unShortName(ut), ".");
    
    printf("    public static %s _inFunc(xerox.ilu.IluCall _call) \n", 
           un_typeDeclarator(ut)
           ); 
    printf("        throws org.omg.CORBA.SystemException\n");
    printf("    {\n");
    printf("        %s _v = null;\n", typeDeclarator(ut));
    printf("        %s _d;\n", typeDeclarator(discriminatorType(ut)));
    printf("        _d = %s;\n",
            castIntToDisc(ut, cat3("_call.inUnion(", KernelTypeKindName(discriminatorType(ut)), ")"))
        );
    if (rock->isBoolCase) {
       printf("        if(_d) {\n");
       if (rock->targ) {
            printf("            _v = %salloc_%s(true, %s);\n",
                classPrefix,
                namePiece(rock->targ),
                ioInPiece(rock->targ->type)
                );
       } else {
           printOthers(ut);
       }
       printf("        } else {\n");
       if (rock->farg) {
            printf("            _v = %salloc_%s(false, %s);\n",
                classPrefix,
                namePiece(rock->farg),
                ioInPiece(rock->farg->type)
                );
       } else {
            printOthers(ut);
       }
       printf("        }\n");
    } else { /* not boolean */
        printf("        switch(%s) {\n",
            mySwitch(discriminatorType(ut), "_d")
            );
        C_LOOP_BEGIN(uTypes(ut), Argument, arg, i, t1)
            printCasesNormal(arg, ut, i);
            printf("                _v = %salloc_%s(_d, %s);\n",
                classPrefix,
                namePiece(arg),
                ioInPiece(arg->type)
                );
            printf("                break;\n");
        C_LOOP_END(i)
        if (defaultArm(ut) == 0) {
            printf("            default:\n");
            printOthers(ut);
        }
        printf("        }\n");
    }
    printf("        _call.endUnion();\n");
    printf("        return _v;\n");
    printf("    } //_inFunc\n\n");
}


PRIVATE void
printOutFunc(UTRockType* rock)
{
    Type ut = rock->unionType;
    printf("    public static void _outFunc(xerox.ilu.IluCall _call, %s _a) \n", 
           un_typeDeclarator(ut)
           ); 
    printf("        throws org.omg.CORBA.SystemException\n");
    printf("    {\n");
    printf("        if (!_a._init) {throw new org.omg.CORBA.BAD_OPERATION();}\n");
    printf("        _call.outUnion(%s, %s);\n",
        castDiscToInt(ut, "_a._dis"),
        KernelTypeKindName(discriminatorType(ut))
        );
    if (rock->isBoolCase) {
       printf("        if(_a._dis) {\n");
       if (rock->targ) {
            printf("            %s;\n",
                ioOutPiece(rock->targ->type, 
                    fromObject(rock->targ->type, "_a._val")
                    )
            );
       }
       printf("        } else {\n");
       if (rock->farg) {
            printf("            %s;\n",
                ioOutPiece(rock->farg->type, 
                    fromObject(rock->farg->type, "_a._val")
                    )
            );
       }
       printf("        }\n");
    } else { /* not boolean */
        printf("        switch(%s) {\n",
            mySwitch(discriminatorType(ut), "_a._dis")
            );
        C_LOOP_BEGIN(uTypes(ut), Argument, arg, i, temp)
            printCasesNormal(arg, ut, i);
            printf("                %s;\n",
                ioOutPiece(arg->type, fromObject(arg->type, "_a._val"))
                );
            printf("                break;\n");
        C_LOOP_END(i)
        printf("        }\n");
    }
    printf("        _call.endUnion();\n");
    printf("    } //_outFunc\n\n");
}


PRIVATE void
printSzFunc(UTRockType* rock)
{
    Type ut = rock->unionType;
    printf("    public static int _szFunc(xerox.ilu.IluCall _call, %s _a) \n", 
           un_typeDeclarator(ut)
           ); 
    printf("        throws org.omg.CORBA.SystemException\n");
    printf("    {\n");
    printf("        int _sz;\n");
    printf("         _sz = _call.szUnion(%s, %s);\n",
        castDiscToInt(ut, "_a._dis"),
        KernelTypeKindName(discriminatorType(ut))
        );
    if (rock->isBoolCase) {
       printf("        if(_a._dis) {\n");
       if (rock->targ) {
            printf("            _sz = _sz + %s;\n",
                ioSzPiece(rock->targ->type, 
                    fromObject(rock->targ->type, "_a._val")
                    )
                );
       }
       printf("        } else {\n");
       if (rock->farg) {
            printf("            _sz = _sz + %s;\n",
                ioSzPiece(rock->farg->type, 
                    fromObject(rock->farg->type, "_a._val")
                    )
                );
       }
       printf("        }\n");
    } else { /* not boolean */
        printf("        switch(%s) {\n",
            mySwitch(discriminatorType(ut), "_a._dis")
            );
        C_LOOP_BEGIN(uTypes(ut), Argument, arg, i, t1)
            printCasesNormal(arg, ut, i);
            printf("                _sz = _sz + %s;\n",
                ioSzPiece(arg->type, fromObject(arg->type, "_a._val"))
                );
            printf("                break;\n");
        C_LOOP_END(i)
        printf("        }\n");
    }
    printf("        _call.endUnion();\n");
    printf("        return _sz;\n");
    printf("    } //_szFunc\n\n");
}

/* copied from kernel file ilutype.h and renamed */
typedef enum kernel_ilu_ConstantValueKind_e {
  ilu_byte_cvk,
  ilu_shortinteger_cvk,
  ilu_integer_cvk,
  ilu_shortcardinal_cvk,
  ilu_cardinal_cvk,
  ilu_shortreal_cvk,
  ilu_real_cvk,
  ilu_boolean_cvk,
  ilu_enumeration_cvk,
  ilu_string_cvk
} kernel_ilu_ConstantValueKind;


PRIVATE void
printTypeRegistrations(Type ut, int defaultArmIdx) 
{
    int valNum = 0;
    Type discType = discriminatorType(ut);
    char* valueKind = 0;
    boolean cvalIsString = 0;

    /*find out a few things */
    switch (type_ur_kind(discType)) {
        case byte_Type:
            valueKind = copy("xerox.ilu.IluConstantValueKind.byte_cvk");
            break;
        case boolean_Type:
            valueKind = copy("xerox.ilu.IluConstantValueKind.boolean_cvk");
            break;
        case character_Type: case shortcharacter_Type:
            fatal ("not supported March 12, 1997");
        case shortinteger_Type:
            valueKind = copy("xerox.ilu.IluConstantValueKind.shortinteger_cvk");
            break;
        case integer_Type:
            valueKind = copy("xerox.ilu.IluConstantValueKind.integer_cvk");
            break;
        case shortcardinal_Type:
            valueKind = copy("xerox.ilu.IluConstantValueKind.shortcardinal_cvk");
            break;
        case cardinal_Type:
            valueKind = copy("xerox.ilu.IluConstantValueKind.shortcardinal_cvk");
            break;
        case enumeration_Type:
            valueKind = copy("xerox.ilu.IluConstantValueKind.enumeration_cvk");
            cvalIsString = 1;
            break;
        default: 
            fatal ("not possible 2");
    }
    
    printf("    static {\n");
    printf("        int[] _ivals = null;\n");
    printf("        java.lang.String[] _svals = null;\n");
    printf("        xerox.ilu.IluTypeRep __t = null;\n");
    printf("        __t = xerox.ilu.IluTypeRep.registerUnionType(\n");
    printf("            %s, //name\n",
        qoString(unresolvedIslTypeName(ut)));
    printf("            %s, //islIfName\n",
        qoString(interface_name(currentIfc)));
    printf("            %s, //islIfBrand\n",
        qoString(currentIfc->brand));
    printf("            %s, //uid\n", qoString(ut->uid));
    printf("            %s, //discriminatUID\n",
        qoString(discType->uid));

    printf("            %d, //cnt\n", list_size(uTypes(ut)));
    printf("            %d, //defaultArm\n", defaultArmIdx);
    printf("            %s, //othersAllowed\n", 
        booleanImage(othersAllowed(ut)));
    printf("            %s); //valueKind\n", valueKind);
    printf("        %s.id(); //loads helper class\n",
        helperClassShortName(ut));
    
    C_LOOP_BEGIN(uTypes(ut), Argument, arg, vidx, t1)
        valNum = list_size(arg->values);
        if (cvalIsString) {
            printf("        _svals = new java.lang.String[%d];\n", valNum);
        } else {
            printf("        _ivals = new int[%d];\n", valNum);
        }
        C_LOOP_BEGIN(arg->values, ConstantValue, cv, cidx, t2)
            if (cvalIsString) {
                printf("        _svals[%d] = ", cidx);
            } else {
                printf("        _ivals[%d] = ", cidx);
            }
            printSpecialConst(cv, discType);
            printf(";\n");
        C_LOOP_END(cidx)
        printf("        __t.registerUnionArm(\n");
        printf("            %d, //armNum\n", vidx);
        printf("            %s, //islArmName\n", 
            qoString(argument_name(arg)));
        printf("            %s, //armUID\n", qoString(arg->type->uid));
        printf("            %d, //valNum\n", valNum);
        printf("            _ivals, //ivals\n");
        printf("            _svals); //svals\n");
    C_LOOP_END(vidx)
    
    printf("        __t.finish();\n");
    printf("        __t = null;\n");
    printf("    } //static\n");
} /*printTypeRegistrations*/


PRIVATE void setRock(UTRockType* rock, Type ut)
{
    ut = un_assert(ut);
    rock->defaultArm = defaultArm(ut);
    rock->defaultArmIdx = -1;
    rock->count = 0;
    rock->unionType = ut;
    rock->isBoolCase = 0;
    rock->targ = rock->farg = 0;
    if (type_kind(discriminatorType(ut)) == boolean_Type) {
        rock->isBoolCase = 1;
        /* pre-write with eventual default case */
        C_LOOP_BEGIN(uTypes(ut), Argument, arg, idx, t0)
            if (arg == rock->defaultArm) {
                rock->targ = rock->farg = arg;
                rock->defaultArmIdx = idx;
            }
        C_LOOP_END(idx)
        /* overwrite the real cases */
        LOOP_BEGIN(uTypes(ut), Argument, arg, t1)
            LOOP_BEGIN(arg->values, ConstantValue, cv, t2)
                if (cv->val.b) {
                    rock->targ = arg;
                } else {
                    rock->farg = arg;
                }
            LOOP_END()
        LOOP_END()
        rock->defaultArm = 0; /* since now all cases are set explicitely */
    } else {
        C_LOOP_BEGIN(uTypes(ut), Argument, arg, idx2, t0)
            if (arg == rock->defaultArm) {
                rock->defaultArmIdx = idx2;
            }
        C_LOOP_END(idx2)
    }
} /*setRock*/


PUBLIC void un_helpInnerCallback(Type t)
{
    UTRockType rock;
    setRock(&rock, t);
    
    printSzFunc(&rock);
    printOutFunc(&rock);
    printInFunc(&rock);
    
    printTypeRegistrations(t, rock.defaultArmIdx);
} /*un_helpInnerCallback*/


PUBLIC void
un_defineMain(Type ut)
{
    char* name;
    UTRockType rock; 
    IHandle ih;
      
    ut = un_assert(ut);
    ih = getContextT(ut);
    name = unShortName(ut);
    
    
    NewJavaFile(ut->interface, ih, name);
    LoadJavaClass(name);

    setRock(&rock, ut);
    
    printOpenDocComment("");
    printCommentLines("Class representing an ILU union type.\n");
    printCloseComment();
    printf("public class %s implements java.io.Serializable {\n", name);
    printf("    boolean _init = false;\n");
    printf("    %s _dis = %s;\n", 
        typeDeclarator(discriminatorType(ut)),
        typeInitializer(type_kind(discriminatorType(ut)))
        );
    printf("    java.lang.Object _val = null;\n\n");

    printf("    static void _throw()\n");
    printf("            throws org.omg.CORBA.BAD_OPERATION {\n");
    printf("        throw new org.omg.CORBA.BAD_OPERATION();\n");
    printf("    } //_throw\n\n");
    
    printConstructor(name);
    printDiscriminatorMethod(ut);

    list_enumerate(uTypes(ut), 
	  (EnumProc) print1UnionMethod, &rock
	  );
    	  
    if (othersAllowed(ut)) {
        printSetOther(&rock);
        printAllocOther(&rock);
    }
    
    printf("} //%s\n", name);
}


