/* genopt.c */
/* Chris Jacobi, November 13, 1998 11:47 am PST */

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

/* $Id: genopt.c,v 1.25 1999/08/03 01:51:14 janssen Exp $ */


#include <stdio.h>
#include <string.h>
#include "iluptype.h"
#include "shared.h"
#include "stubops.h"
#include "util.h"
#include "context.h"
#include "name.h"
#include "genopt.h"
#include "io.h"


PUBLIC boolean opt_is_a(Type t) {
    TypeKind kind;
    t = myUrType(t);
    kind = type_kind(t);
    return (kind==optional_Type);
}


PUBLIC void opt_assert(Type t) {
    if (! opt_is_a(t)) fatal("should have been an optional");
}


static Type optElType(Type t)
{
    opt_assert(t);
    t = myUrType(t);
    t = type_description(t)->structuredDes.optional;
    t = myUrType(t);
    return t;
}


static char* getUsualStubShortName(Type t)
/* Returns a name which can be used (but not necessarily is)
 * for defining a stub class.
 * (In particular there is no guarantee that there is a stub class)
 */
{
    IHandle ih = getContextT(t);
    return cat2(easyShortTypeNameCleaned(t), ih->p.stubSuffix);
}


static char* getStubNameSL(Type t)
{
    char * name = getUsualStubShortName(t);
    IHandle ih = getContextT(t);
    if (ih != currentIH) {
        name = dotCat(packagePrefixJ(ih), name);
    }
    return name;
}


PUBLIC char * opt_typeDeclarator(Type t) {
    Type eType = optElType(t);
    return wrapperTypeDeclarator(eType);
}

PUBLIC char * wrapperTypeDeclarator(Type t) {
    Type eType = myUrType(t);
    TypeKind eKind = type_kind(eType);
    switch (eKind) {
        case byte_Type:	
        case int16_Type:	
        case int32_Type:	
        case card16_Type:	
        case card32_Type:	
            return "java.lang.Integer";
        
        case int64_Type:	
        case card64_Type:	
            return "java.lang.Long";
        
        case real32_Type:	
            return "java.lang.Float";
        case real64_Type:	
            return "java.lang.Double";
        
        case boolean_Type: 
            return "java.lang.Boolean";
        
        case char16_Type: 	
        case char8_Type: 
            return "java.lang.Character";

        case enumeration_Type:
        case object_Type:
        case optional_Type:
        case array_Type:
        case record_Type:
        case sequence_Type:
        case union_Type:
        case pickle_Type:
            return typeDeclarator(eType);
        
        default:
            break;
    };
    return "??not_impl_wrapperTypeDeclarator";
}


PUBLIC char * opt_ioSzPiece(Type t, const char *arg) {
    t = myUrType(t);
    opt_assert(t);
    return cat4(getStubNameSL(t), "._szFunc(_call, ", arg, ")");
}


PUBLIC char * opt_ioOutPiece(Type t, const char *arg) {
    t = myUrType(t);
    opt_assert(t);
    return cat4(getStubNameSL(t), "._outFunc(_call, ", arg, ")");
}


PUBLIC char * opt_ioInPiece(Type t) {
    t = myUrType(t);
    opt_assert(t);
    return cat2(getStubNameSL(t), "._inFunc(_call)");
}


PUBLIC void opt_enumElementTypes(Type t, TypeProc tp, refany rock) {
    opt_assert(t);
    t = myUrType(t);
    t = type_description(t)->structuredDes.optional;
    tp(t, rock);
} /*opt_enumElementTypes*/


static boolean
optionalNeedsStubClass(Type t) 
{
    return 1;
}


PUBLIC char * fromObject(Type t, char* name)
    /* returns either the argument unchanged, or accesses a wrapper */
{
    t = myUrType(t);
    switch (type_kind(t)) {
        case byte_Type:	
            return cat3("((java.lang.Number)", name, ").byteValue()");

        case card16_Type:
        case int16_Type:	
                    return cat3("((java.lang.Number)", name, ").shortValue()");

        case int32_Type:	
        case card32_Type: 
            return cat3("((java.lang.Number)", name, ").intValue()");
        
        case int64_Type:	
        case card64_Type:	
            return cat3("((java.lang.Number)", name, ").longValue()");
        
        case real32_Type:		
            return cat3("((java.lang.Number)", name, ").floatValue()");
        case real64_Type:		
            return cat3("((java.lang.Number)", name, ").doubleValue()");
        
        case char16_Type: 	
        case char8_Type: 
            return cat3("((java.lang.Character)", name, ").charValue()");
            
        case boolean_Type: 	
            return cat3("((java.lang.Boolean)", name, ").booleanValue()");

        case enumeration_Type:	/* new mapping ! */
        case object_Type:
        case optional_Type:
        case array_Type:
        case record_Type:
        case sequence_Type:
        case union_Type:
        case pickle_Type:
            return cat5( "((", typeDeclarator(t), ")", name, ")");
             
        case invalid_Type:
        case alias_Type:
        case void_Type:
            fatal("bad type in fromObject"); break;

        case pipe_Type:
        default:
            break;
    }
    return "??_not_Impl_in_fromObject";

}


PUBLIC char * toObject(Type t, char* name)
    /* returns either the argument unchanged, or allocates a wrapper */ 
{
    t = myUrType(t);
    switch (type_kind(t)) {
        case byte_Type:	
        case int16_Type:	
        case int32_Type:	
        case card16_Type:;
        case card32_Type: 
            return cat3("new java.lang.Integer(", name, ")");
        
        case real32_Type:	
            return cat3("new java.lang.Float(", name, ")");
        case real64_Type:	
            return cat3("new java.lang.Double(", name, ")");
        
        case int64_Type:	
        case card64_Type:	
            return cat3("new java.lang.Long(", name, ")");
                    
        case boolean_Type: 	
            return cat3("new java.lang.Boolean(", name, ")");

        case char16_Type: 	
        case char8_Type: 
            return cat3("new java.lang.Character(", name, ")");

        case enumeration_Type: /* new mapping */
        case real128_Type: /* now an object type */
        case object_Type:
        case optional_Type:
        case array_Type:
        case record_Type:
        case sequence_Type:
        case union_Type:
        case pickle_Type:
            return name;
             
        case invalid_Type:
        case alias_Type:
        case void_Type:
            fatal("bad type in toObject"); 
            break;
            
        case pipe_Type:
        default:
            break;
    }
    return "??_not_Impl_in_toObject";
}


static char * leniantOptTypeDecl(Type t)
/* superclass to compensate for humans putting
 * in-precise wrapper...
 */
{
    Type eType = optElType(t);
    TypeKind eKind = type_kind(eType);
    switch (eKind) {
        case byte_Type:	
        case int16_Type:	
        case int32_Type:	
        case int64_Type:	
        case card16_Type: 
        case card32_Type: 	
        case card64_Type:	
        case real32_Type:		
        case real64_Type:		
        /* removed a case */		
            return "java.lang.Number";
        default:
            break;
    }
    return opt_typeDeclarator(t);
}


PUBLIC void
opt_defineMain(Type t)
{
   boolean needsClass = optionalNeedsStubClass(t);
   Type eType;
   char* oStubClassName;
   
   opt_assert(t);
   if (needsClass) {
       IHandle ih = getContextT(t);
       eType = optElType(t);
       oStubClassName = getUsualStubShortName(t);
       
       NewJavaFile(t->interface, ih, oStubClassName);
       LoadJavaClass(oStubClassName);
       printOpenDocComment("");
       printCommentLines("Helper class for an ILU otional type.\n");
       printCloseComment();
       printf("public final class %s { //ilu-otional\n\n", oStubClassName);
       
       printf("    public static int _szFunc(xerox.ilu.IluCall _call, %s _a) \n", 
           leniantOptTypeDecl(t)
           ); 
       printf("        throws org.omg.CORBA.SystemException\n");
       printf("    {\n");
       printf("        if (_a==null) {\n");
       printf("            return _call.szOptional(false);\n");
       printf("        } else {\n");
       printf("            return _call.szOptional(true) + %s;\n",
           ioSzPiece(eType, fromObject(optElType(t), "_a"))
           );
       printf("        }\n");
       printf("    } //_szFunc\n\n");
       
       printf("    public static void _outFunc(xerox.ilu.IluCall _call, %s _a) \n", 
           leniantOptTypeDecl(t)
           ); 
       printf("        throws org.omg.CORBA.SystemException\n");
       printf("    {\n");
       printf("        if (_a==null) {\n");
       printf("            _call.outOptional(false);\n");
       printf("        } else {\n");
       printf("            _call.outOptional(true);\n");
       printf("            %s;\n",
           ioOutPiece(eType, fromObject(optElType(t), "_a"))
           );
       printf("        }\n");
       printf("    } //_outFunc\n\n");
       
       printf("    public static %s _inFunc(xerox.ilu.IluCall _call) \n", 
           opt_typeDeclarator(t)
           ); 
       printf("        throws org.omg.CORBA.SystemException\n");
       printf("    {\n");
       printf("        if (_call.inOptional()) {\n");
       printf("            return %s;\n",
           toObject(optElType(t), ioInPiece(eType))
           );
       printf("        } else {\n");
       printf("            return (null);\n");
       printf("        }\n");
       printf("    } //_inFunc\n\n");
       
       if (ih->p.genHlp) {
           printf("    static {\n");
           printf("        xerox.ilu.IluTypeRep __t = xerox.ilu.IluTypeRep.registerOptionalType(\n");
           printf("            %s, //name\n",
               qoString(unresolvedIslTypeName(t)));
           printf("            %s, //islIfName\n",
               qoString(interface_name(currentIfc)));
           printf("            %s, //islIfBrand\n",
               qoString(currentIfc->brand));
           printf("            %s, //uid\n", qoString(t->uid));
           printf("            %s); //baseUID\n", qoString(eType->uid));
           printf("    } //static\n");
       }
       
       printf("} //%s\n", oStubClassName);
    }
}


