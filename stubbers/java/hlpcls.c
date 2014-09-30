/* hlpcls.c */
/* Chris Jacobi, November 17, 1998 2:49 pm PST */

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

/* $Id: hlpcls.c,v 1.34 1999/08/03 01:51:13 janssen Exp $ */

#include <stdio.h>
#include <string.h>
#include "iluptype.h"

#include "genstub.h"
#include "shared.h"
#include "stubops.h"
#include "io.h"
#include "name.h"
#include "util.h"
#include "context.h"
#include "genobj.h"
#include "genenum.h"
#include "genopt.h"
#include "gencust.h"
#include "genrecord.h"
#include "genunion.h"
#include "genseq.h"
#include "genarr.h"


#define TYPECODE "xerox.ilu.IluTypeCode"
#define THROWSYS "throws org.omg.CORBA.SystemException"


PRIVATE void 
print_IOF_ImplComment()
{
    printf("    /** Not public; implements xerox.ilu.IluIOFunctions */\n");
}


PUBLIC boolean genericUseHelperClass(Type t)
{
    switch (type_kind(t)) {
        case enumeration_Type:
        case record_Type:
        case union_Type:
        case array_Type:
        case object_Type:
        case optional_Type:
        case alias_Type:
            return 1;
        case sequence_Type:
            /* generate it after all...  if (sq_isJString(t)) {return 0;} */
            return 1;
        default: 
            break;
    }
    return 0;
}

PUBLIC boolean useHelperClass(Type t)
{
    if (genericUseHelperClass(t)) {
        IHandle ih = getContextTraw(t);
        if (ih->p.genHlp) return 1;
    }
    return 0;
}


PUBLIC char*
helperClassShortName(Type rawT)
{
    char* classBaseName;
    char* classShortName;
    IHandle ih = getContextTraw(rawT);
    if (! useHelperClass(rawT)) return 0;
    classBaseName = easyShortTypeNameCleaned(rawT); 
    classShortName = cat2(classBaseName, ih->p.helperSuffix);
    return classShortName;
}


PUBLIC char*
helperClassName(Type t)
{
    IHandle ih = getContextTraw(t);
    char* name;
    if (! useHelperClass(t)) return 0;
    name = helperClassShortName(t);
    if (ih != currentIH) {
        name = dotCat(packagePrefixJ(ih), name); 
    }
    
    return name;
}



PRIVATE void
printHelperIOMethods(Type urT)
{
    boolean isOptional = FALSE;
    
    /* for objects suboptimal: Could share impl and use tc to find class */
    print_IOF_ImplComment();
    
    printf("    public int szFunc(xerox.ilu.IluCall _call, java.lang.Object _x, " TYPECODE " _tc) " THROWSYS " {\n");
    printf("        %s _xx = %s;\n", 
        typeDeclarator(urT), 
        fromObject(urT, "_x")
        );
    printf("        return %s;\n", ioSzPiece(urT, "_xx"));
    printf("    } //szFunc \n\n");
    
    print_IOF_ImplComment();
    printf("    public void outFunc(xerox.ilu.IluCall _call, java.lang.Object _x, " TYPECODE " _tc) " THROWSYS " {\n");
    printf("        %s _xx = %s;\n", 
        typeDeclarator(urT), 
        fromObject(urT, "_x")
        );
    printf("        %s;\n", ioOutPiece(urT, "_xx"));
    printf("    } //outFunc \n\n");

    print_IOF_ImplComment();
    printf("    public java.lang.Object inFunc(xerox.ilu.IluCall _call, " TYPECODE " _tc) " THROWSYS " {\n");
    printf("        return %s;\n", toObject(urT, ioInPiece(urT)));
    printf("    } //inFunc \n\n");
    
    printf("    public boolean isAFunc(java.lang.Object _x, " TYPECODE " _tc) {\n");
    
    if (obj_is_a(urT)) {
        Class cls = class_object(obj_assert(urT));
        isOptional = cls->optional;
    }
    if (opt_is_a(urT)) {
        isOptional = TRUE;
    }
    printf("        if (_x == null) return %s;\n", booleanImage(isOptional));
    printf("        return (_x instanceof %s);\n", wrapperTypeDeclarator(urT));
    printf("    } //isAFunc \n\n");
}


PUBLIC void enumElementTypes(Type t, TypeProc tp, refany rock) 
/* Shallow enumeration of element types.
 * May or may not have duplicates.
 * At todays date not yet implemented for all types.  
 */
{    switch (type_kind(t)) {
        case enumeration_Type: 
            /* has none */
            break;
        case record_Type:
            rec_enumElementTypes(t, tp, rock);
            break;
        case union_Type:
            un_enumElementTypes(t, tp, rock);
            break;
        case array_Type:
            ar_enumElementTypes(t, tp, rock);
            break;
        case sequence_Type:
            sq_enumElementTypes(t, tp, rock);
            break;
        case optional_Type:
            opt_enumElementTypes(t, tp, rock);
            break;
        default:
            break;
    }
} /*enumElementTypes*/


PRIVATE void collectHelperClassName(Type t, refany rock)
{
    if (useHelperClass(t)) {
        list handled = (list) rock;
        char* name = helperClassName(t);
        refany found;
        if (name==0) return;
        found = list_find(handled, (iluparser_FindProc) matchString, name);
        if (found==0) {
           list_insert(handled, name); 
        }
    }
}


PRIVATE void printHelperLoadees(Type t) {
    char* myName; 
    list handled = new_list();
    enumElementTypes(t, collectHelperClassName, handled);
    if (useHelperClass(t)) {
        /* remove self from list of names ... */
        myName = helperClassName(t);
        if (myName) {
            char* xx = list_find(
                handled, (iluparser_FindProc) matchString, myName);
            if (xx) {
                (void) list_remove (handled, xx);
            }
        }
    }
    if (list_size(handled)) {
        LOOP_BEGIN(handled, char*, className, temp)
        printf("        %s.id();\n", className);
        LOOP_END()
    }
} /*printHelperLoadees*/


PRIVATE void helpInnerCallback(Type t)
{
    switch (type_kind(t)) {
        case enumeration_Type:
            enm_helpInnerCallback(t);
            break;
        case record_Type:
            rec_helpInnerCallback(t);
            break;
        case object_Type:
            obj_helpInnerCallback(t); 
            break;
        case union_Type:
            un_helpInnerCallback(t);
            break;
        default: 
            break;
    }
} /*helpInnerCallback*/


PRIVATE void 
printAliasRegistrations(Type rawT)
{
    Type urT;
    urT = myUrType(rawT);
    printf("        xerox.ilu.IluTypeRep __t = null;\n");
    printf("        __t = xerox.ilu.IluTypeRep.registerAliasType(\n");
    printf("            %s, //name\n", qoString(unresolvedIslTypeName(rawT)));
    printf("            %s, //islIfName\n",
        qoString(interface_name(currentIfc)));
    printf("            %s, //islIfBrand\n",qoString(currentIfc->brand));
    printf("            %s, //uid\n", qoString(rawT->uid));
    printf("            %s); //baseUID\n", qoString(urT->uid));
    printf("        __t = null;\n");
}


PUBLIC void
printHelperClass(Type rawT)
{
    /* ONE OF THE FEW PLACES DEALING WITH ALIAS */
    char* classShortName;
    Type urT;
    IHandle rawIH = getContextTraw(rawT);
    
    if (! rawIH->p.genHlp) return;
    
    if (type_kind(rawT) == alias_Type) {
        urT = myUrType(rawT);
    } else {
        urT = myUrType(rawT);
        rawT = urT;
    }
    
    classShortName = helperClassShortName(rawT);
    NewJavaFile(
        rawT->interface, 
        getContext(rawT->interface, rawT->scoping), 
        classShortName);
    LoadJavaClass(classShortName);
    printOpenDocComment("");
    if (obj_is_a(rawT)) {
        Class c = class_object(rawT);
        if (c->doc_string) {
            printCommentLines(c->doc_string);
        }
    }
    printCommentLines("An ILU helper class.\n");
    printCloseComment();
    printf("public class %s %s {\n",
        classShortName,
        "implements xerox.ilu.IluIOFunctions "
        );
    
    printf("    private final static java.lang.String _id = \n        %s;\n",
        qoString(rawT->uid)
        );
    printf("    private static " TYPECODE " _tc = null;\n");
    printf("    static {\n");
    printf("        %s\n", stubConsistencyCheck());
    printHelperLoadees(rawT);
    printf("        _tc = " TYPECODE ".newTypeCode(id(), %s, %s);\n",
        cat3("new ", classShortName, "()"),
        corbaTCKind(type_kind(rawT))
        );
        /* SIDE EFFECT OF THIS:  An instance of the helper class itself
         * is registered with IluTypeCode. ==> helper class will not
         * be class collected by the java collector.
         */ 
    if (type_kind(rawT) == alias_Type) {
        printAliasRegistrations(rawT);
    }
    
    printf("    }\n\n");
    
    printf("    public final static java.lang.String id() {\n");
    printf("        return _id;\n");
    printf("    } //id \n\n");
    
    printf("    public final static " TYPECODE " type() {\n");
    printf("        return _tc;\n");
    printf("    } //type \n\n");
    
    
    printf("    public static void insert(%s.CORBA.Any _any, %s _x) " THROWSYS " {\n",
        orgDotOmg,
        typeDeclarator(urT)
        );
    printf("        xerox.ilu.IluAny _ia = (xerox.ilu.IluAny) _any;\n");
    printf("        _ia.assign(_tc, %s);\n", toObject(urT, "_x"));
    printf("    } //insert \n\n");

    
    printf("    public static %s extract(%s.CORBA.Any _any) " THROWSYS " {\n",
        typeDeclarator(urT),
        orgDotOmg
        );
    printf("        xerox.ilu.IluAny _ia = (xerox.ilu.IluAny) _any;\n");
    printf("        java.lang.Object _ob = _ia.cachedValue();\n");
    printf("        if (_ob == null) {_ob = _ia.value(_tc);}\n");
    printf("        return %s;\n", fromObject(urT, "_ob"));
    printf("    } //extract \n\n");
 
 
    printf("    public static xerox.ilu.IluPickle to_pickle(%s _x) " THROWSYS " {\n",
        typeDeclarator(urT)
        );
    printf("        int _sz = 0;\n");
    printf("        xerox.ilu.IluCall _call = null;\n");
    printf("        xerox.ilu.IluPickle _pickle = new xerox.ilu.IluPickle();\n");
    printf("        try {\n");
    printf("            _call = _pickle.startToPickle();\n");
    printf("            _sz = %s;\n", ioSzPiece(urT, "_x"));
    printf("            _pickle.midToPickle(_call, _sz, id());\n");
    printf("            %s;\n", ioOutPiece(urT, "_x"));
    printf("        } finally {\n");
    printf("            _pickle.endToPickle(_call);\n");
    printf("        }\n");
    printf("        return _pickle;\n");
    printf("    } //to_pickle \n\n");

    printf("    public static %s from_pickle(xerox.ilu.IluPickle _pickle) " THROWSYS " {\n",
        typeDeclarator(urT)
        );
    printf("        %s _x = %s;\n", 
        typeDeclarator(urT), 
        typeInitializer(type_kind(urT))
        );
    printf("        xerox.ilu.IluCall _call = null;\n");
    printf("        try {\n");
    printf("            _call = _pickle.startFromPickle(id());\n");
    printf("            _x = %s;\n", ioInPiece(urT));
    printf("        } finally {\n");
    printf("            _pickle.endFromPickle(_call);\n");
    printf("        }\n");
    printf("        return _x;\n");
    printf("    } //from_pickle \n\n");
    
    if (1) {
        printHelperIOMethods(urT);
    }
    
    if (custom_is_a(urT)) {
        custom_PrintHelperStubs(urT);
    }
    
    printLoadClasses(rawIH, 0);
    
    helpInnerCallback(rawT);
    
    printf("}//%s\n\n", classShortName);
}


/* end */
