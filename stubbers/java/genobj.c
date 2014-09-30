/* genobj.c */
/* Chris Jacobi, January 7, 1999 8:52 am PST */

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

/* $Id: genobj.c,v 1.91 1999/08/03 01:51:15 janssen Exp $ */

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
#include "genobj.h"
#include "genex.h"
#include "hlpcls.h"

static IHandle specialContext = 0;

PUBLIC void init_genobj()
{
    specialContext = getContextC("ilu");
}


typedef struct {
    refany clientrock;
    iluparser_EnumProc clientproc;
    list classesSeen;
} RockType_enumerateSuperClasses;


PRIVATE void 
enumProc_enumerateSuperClasses(Type t, RockType_enumerateSuperClasses* myRock)
{
    Class c = class_object(t);
    if (list_insert_onceonly(&(myRock->classesSeen), c)) {
        list_enumerate(c->superclasses, 
            (EnumProc) enumProc_enumerateSuperClasses, myRock);
        myRock->clientproc(t, myRock->clientrock);
    }
}

PRIVATE void 
enumerateSuperClasses(
    Type t, 
    iluparser_EnumProc proc, /*where element=superclass, is a Type*/
    refany rock,
    boolean recurse,
    boolean self
    )
{
    Class c = class_object(t);
    if (self) {
        proc(t, rock);
    }
    if (c->superclasses==0) {
        return;
    } else if (recurse) {
        RockType_enumerateSuperClasses* myRock;
        myRock = iluparser_Malloc(sizeof(RockType_enumerateSuperClasses));
        myRock->clientrock = rock;
        myRock->clientproc = proc;
        myRock->classesSeen = new_list();
        list_enumerate(c->superclasses, 
            (iluparser_EnumProc) enumProc_enumerateSuperClasses, 
            myRock
            );
    } else {
        list_enumerate(c->superclasses, proc, rock);
    }
}


PUBLIC boolean obj_is_a(Type t) {
    TypeKind kind;
    t = myUrType(t);
    kind = type_kind(t);
    return (kind==object_Type);
}


PUBLIC Type obj_assert(Type t) {
    t = myUrType(t);
    if (! obj_is_a(t)) fatal("should have been an object");
    return t;
}

/* 
 * Returns base part of name to be concatenated with various 
 * suffixes and prefixes
 */
PRIVATE char * objectTypeBaseName(Type t) {
    t = obj_assert(t);
    return easyShortTypeNameCleaned(t);
}


PRIVATE char * refTypeSName(Type t) {
    t = obj_assert(t);
    return cat2(objectTypeBaseName(t), getContextT(t)->p.refSuffix);
}


PRIVATE char* stubClassSName(Type t)
{
    t = obj_assert(t);
    return cat2(objectTypeBaseName(t), getContextT(t)->p.stubSuffix);
}


PRIVATE boolean useOperationsInterface(Type t)
{
    IHandle ih;
    t = obj_assert(t);
    ih = getContextT(t);
    return ih->p.genPortability;
}

PRIVATE char* operationsTypeSName(Type t)
{
    t = obj_assert(t);
    if (useOperationsInterface(t)) {
        return cat2(objectTypeBaseName(t), getContextT(t)->p.operationsSuffix);
    } else {
        return refTypeSName(t);
    }
}

PRIVATE char* accessOperationsTypeName(Type t)
{
    return packageDotStringJ(getContextT(t), operationsTypeSName(t));
}


PRIVATE char* accessRefTypeName(Type t)
{
    return packageDotStringJ(getContextT(t), refTypeSName(t));
}


PRIVATE char * prependInterface(string base, Type ob) {
    string n;
    IHandle ih;
    string ifName;
    ih = getContextT(ob);
    ifName = objectTypeBaseName(ob);
    n = cat3(ifName, ih->p.methodNameSeparator, base);
    return n;
}

PRIVATE char * prependPackage(string base, Type ob) {
    string n;
    IHandle ih;
    string prefix;
    ih = getContextT(ob);
    prefix = packagePrefixJ(ih);
    n = dotCat(prefix, javaizeIdent(base));
    return n;
}


PRIVATE char* methodShortName(Procedure m)
{
    IHandle ih;
    string n;
    ih = getContextT(m->object);
    n = methodNameBase(m);
    if (ih->p.methodNamesWithInterface) {
        n = prependInterface(n, m->object);
    }
    if (ih->p.methodNamesWithPackage) {
        n = prependPackage(n, m->object);
    }
    return n;
} /*methodShortName*/


PRIVATE char* methodRepName(Procedure m) 
{
    return cat3("_", methodShortName(m), "_methodRep");
} /*methodRepName*/


PRIVATE char* shortNameOfStaticStubProc(Procedure m)
{
    char* sn = methodNameBase(m);
    return cat3("_", sn, "_S");
} /*shortNameOfStaticStubProc*/

 

PUBLIC boolean isCorbaDotObject(Type t) {
    IHandle ih = getContextT(t);
    if (ih==specialContext) {
        if (obj_is_a(t)) {
            char* sn = easyShortTypeNameCleaned(t);
            if (strcmp(sn, "CORBA_Object") == 0) {
                return TRUE;
            }
        }
    }
    return FALSE;
} /*isCorbaDotObject*/


PRIVATE void checkExtendsCorbaDotObject1(Type t, int* rock)
{
    if (isCorbaDotObject(t)) {*rock = 1;}
}


PRIVATE boolean extendsCorbaDotObject(Type t)
{
   int yep = 0;
   enumerateSuperClasses(t, 
       (EnumProc) checkExtendsCorbaDotObject1, 
       &yep, TRUE, FALSE
       ); 
   return (yep != 0);
} /*extendsCorbaDotObject*/


PUBLIC char * obj_typeDeclarator(Type t) {
    IHandle ih;
    t = obj_assert(t);
    ih = getContextT(t);
    if (ih == specialContext) {
       if (isCorbaDotObject(t)) {
           return cat2(orgDotOmg, ".CORBA.Object");
       }
    }
    return packageDotStringJ(ih, refTypeSName(t));
}


PRIVATE Class
getOClass(Type ot)
{
    ot = obj_assert(ot);
    return type_description(ot)->structuredDes.object;
}


PRIVATE boolean sunStyleHolder(Argument a)
{
     IHandle ih = getContextT(a->type);
     if (ih == specialContext) {
         if (isCorbaDotObject(a->type)) {
             return TRUE;
         }
     }
     return ih->p.genHldCls;
}


PRIVATE char* holderAccess(Argument a)
{
     if (sunStyleHolder(a)) {
        return cat2(argumentName(a), ".value");
     } else {
        return cat2(argumentName(a), "[0]");
     }
}



PRIVATE void
printSkelInElement    (Argument a, void *refany)
/* used to print the calls which unmarshall the arguments to
 * the true method on the server side
 */
{
    char* argName = argumentName(a);
    if (a->direction != In) {
        /* need to create a holder */
        char * htn = holderTypeDeclarator(a->type);
        if (sunStyleHolder(a)) {
            printf("          %s %s = new %s();\n",
                htn, argName, htn);
        } else {
            printf("          %s %s = new %s;\n",
                htn, argName, setDimension(htn, "1"));
        }
    }
    if (a->direction != Out) {
        if (a->direction == In) {
            printf("          %s %s = %s;\n", 
                typeDeclarator(a->type),
                argName, ioInPiece(a->type)
                );
        } else {
            if (sunStyleHolder(a)) {
                printf("          %s.value = %s;\n", 
                    argName, ioInPiece(a->type));
            } else {
                printf("          %s[0] = %s;\n",
                    argName, ioInPiece(a->type));
            }
        }
    }
}


typedef struct {
    boolean printExtends;     /* otherwise prints comma */
    boolean extendOperations; /* otherwise extend references */
    boolean skipCorbaObject;  /* otherwise print it like any other type */
} OperationsRockType;


typedef struct {
    Type t;
    IHandle ih;
    int methodidx;        	/* of method currently in the works */
    char* stubClassShortName; 	/* short name of stub class */
    char* iluClassRepField; 	/* name of field in stub class */
    char* otJBName;        	/* short name of reference interface [o-type] */
    TypeSpec tSpec;
} ObjRockType;


PRIVATE boolean
isVoidType(Type t)
{
    if (t) {
        return (type_ur_kind(t) == void_Type);
    } else {
        return 1;
    }
}


PRIVATE boolean
returnsVoid(Procedure m) 
{
    return isVoidType(m->returnType);
}


PRIVATE boolean
isReallyAsync(Procedure m)
{
    if (m->asynch) {
        if (! returnsVoid(m)) return FALSE;
        if (m->exceptions) return FALSE;
        LOOP_BEGIN(m->arguments, Argument, arg, t1)
            if (arg->direction != In) {return FALSE;}
        LOOP_END()
        return TRUE;
    }
    return FALSE;
}


static void
printThrowsClause(set exceptions, boolean addRemoteException)
{
    boolean addSystemExceptions = FALSE; 
    boolean first = TRUE;
    LOOP_BEGIN(exceptions, Exception, e, t1)
        if (first) printf("\n        throws "); else printf(", ");
        first = FALSE;
        printf("%s", exceptionJName(e));
    LOOP_END()
    if (addSystemExceptions) {
        if (first) printf("\n        throws "); else printf(", ");
        first = FALSE;
        printf("org.omg.CORBA.SystemException");
    }
    if (addRemoteException) {
        if (first) printf("\n        throws "); else printf(", ");
        first = FALSE;
        printf("java.rmi.RemoteException");
    }
}


static void
printActualParamList(list args)
{
    C_LOOP_BEGIN(args, Argument, a, count, t1)
        if (count) {
            printf(", ");
        }
        /* "if (a->direction == In) { " not necessary
         * name of argument independent from a->direction 
         */
        printf("%s", argumentName(a));
    C_LOOP_END(count)
}


PRIVATE char* accessStubClassName(Type t) 
    /* assumes t is an Object type,
     * returns string which accesses the stub
     */
{
    t = obj_assert(t);
    return packageDotStringJ(getContextT(t), stubClassSName(t));
}


PRIVATE char *
accessIluClass(Type t) 
    /* assumes t is an Object type,
     * returns string which accesses the IluClass
     */
{
    return cat2(accessStubClassName(t), ".iluClass()");
}

 
PRIVATE void printSiblingTest(
        Argument a, const char* arg, const char* whiteSpace)
    /* arg: accesses argument */  
{
    if (a->sibling) {
        if (obj_is_a(a->type)) {
            printf("%s_call.checkSibling(%s, %s);\n",
                whiteSpace, 
                arg, 
                accessIluClass(a->type)
                );
        }
    }
}



PRIVATE void
defineServantMethod(Procedure m, ObjRockType* r)
/* r contains the index of the method */
{
    char * typeDecl;
    IHandle ih;
    boolean async = isReallyAsync(m);

    ih = getContextT(m->object);
    typeDecl = obj_typeDeclarator(m->object);
    
    printf("      case %d: //%s\n", r->methodidx++, methodShortName(m)); 
        /*?? CHECK: how are indices the same in decl */
    printf("        {\n");
    
    if (! returnsVoid(m)) {
        Type urt = myUrType(m->returnType);
        printf("          %s _r = %s;\n",
               typeDeclarator(urt),
               typeInitializer(type_kind(urt)));
    }
    
    printf("          %s _sob;\n", typeDecl);
    printf("          _call.startReadRequest();\n");
    printf("          _sob = (%s)\n", typeDecl);
    if (getOClass(m->object)->singleton) {
        printf("              _call.getCallSingleton();\n");
    } else {
        printf("              _call.inObject(true, %s.%s);\n", 
           r->stubClassShortName,
           r->iluClassRepField
           );
    }
    list_enumerate(m->arguments, (EnumProc) printSkelInElement, NULL);
    
    printf("          _call.doneReadRequest();\n");
    if (!async) {
        printf("          try {\n");
    }
    printf("            ");
    if (! returnsVoid(m)) {
        printf("_r = ");
    }
    printf("_sob.%s(", methodShortName(m));
    printActualParamList(m->arguments);
    printf(");\n");
    
    if (async) {
        printf("      _call.noReply();\n"); 
    } else {
    
        C_LOOP_BEGIN(m->exceptions, Exception, e, count, t0)
            printf("          } catch (%s _e) {\n", exceptionJName(e));
            printf("            %s(_e, %d, _call);\n", 
                fullNameOfWriteProc(e), count+1);
            printf("            return;\n");
        C_LOOP_END(count)
        printf("          } catch (java.lang.Exception _e) {\n");
        printf("              _call.unexpectedException(_e);\n");
        printf("              return;\n");
        printf("          }\n");
        printf("          if (_call.needsSizing()) {\n");
        printf("            _sz = _call.beginSizingReply();\n");

        if (! returnsVoid(m)) {
            Type urt = myUrType(m->returnType);
            printf("            _sz += %s;\n", 
                ioSzPiece(urt, "_r"));
        }
        LOOP_BEGIN(m->arguments, Argument, a, t1)
            char* arg;
            if (a->direction == Out || a->direction == InOut) {
                arg = holderAccess(a);
                printf("            _sz += %s;\n", ioSzPiece(a->type, arg));
            }
        LOOP_END()
        printf("          }\n");
        printf("          _call.startWriteReply(_sz);\n");
        if (! returnsVoid(m)) {
            Type urt = myUrType(m->returnType);
            printf("          %s;\n", ioOutPiece(urt, "_r"));
        }
        LOOP_BEGIN(m->arguments, Argument, a, t2)
            char *arg;
            if (a->direction == Out || a->direction == InOut) {
                arg = holderAccess(a);
                if (a->sibling) {printSiblingTest(a, arg, "          ");}
                printf("          %s;\n", ioOutPiece(a->type, arg));
            }
        LOOP_END()
        printf("          _call.doneWriteReply();\n");
    }
    printf("        }\n");
    printf("        break;\n");
   
}


static void
printFormalArgList(list args)
{
    C_LOOP_BEGIN(args, Argument, a, count, t1)
        char * argName = argumentName(a);
        char * argDeclarator;
        if (count) {
            printf(", ");
        }
        if (a->direction == In) {
	    argDeclarator = typeDeclarator(a->type);
        } else {
	    argDeclarator = holderTypeDeclarator(a->type);
        }
        printf("%s %s", argDeclarator, argName);
    C_LOOP_END(count)
}



PRIVATE void
printSignatureForOperations(Procedure p, ObjRockType* rock)
/* Prints one method in the Operations interface */
{
    if (p->doc_string) {
        printDocString("    ", p->doc_string);
    }
    printf("    public ");
    if (p->returnType) {
        printf("%s ", typeDeclarator(p->returnType)); 
    } else {
        printf("void ");
    }
    printf("%s(", methodShortName(p));
    printFormalArgList(p->arguments);
    printf(")");
    printThrowsClause(p->exceptions, (rock->tSpec->javaRemote || rock->ih->p.iJavaRemote));
    printf(";\n");
}

 
/* Surrogate sizing, marshalling in and out procedures. */
PRIVATE void
printSuperClassRep(Type t, ObjRockType* rock)
/* Purpose: Generate the elements of a superClass classRep array.
 *    Input: t is the class type, rock contains an index
 *    Output: void
 *    Rock must be initialized to an appropriate value before the call.
 */
{
    printf("        ca[%d] = %s.iluClass();\n",
           rock->methodidx++, 
           accessStubClassName(t)
           );
}


PRIVATE boolean
anyMethodHasExceptions(ObjRockType* rock)
{
    Class c = class_object(rock->t);
    LOOP_BEGIN(c->methods, Procedure, m, t4)
        if (list_size(m->exceptions)) {return TRUE;}
    LOOP_END()
    return FALSE;
}


PRIVATE void
printMethodRegistration(Procedure m, ObjRockType* rock)
    /* rock contains method idx */
{
    /* define the exceptions of the method */
    if (m->exceptions) {
        cardinal i;
        printf("    _xArr = new xerox.ilu.IluExceptionRep[%d];\n", 
            list_size(m->exceptions)
            );
        for (i = 0; i < list_size(m->exceptions); i++) {
            printf("    _xArr[%d] = %s;\n", 
                i, 
                exceptionRep((Exception) list_ref(m->exceptions, i))
                );
        }
    }
    printf("    %s = ", methodRepName(m));
    printf("xerox.ilu.IluMethodRep.registerMethod(\n");
    printf("        %s, //IluClassRep\n", rock->iluClassRepField);
    printf("        %d, //local idx\n", rock->methodidx++);
    printf("        %s, //name (in isl domain)\n",
        qoString(procedure_name(m)));
    printf("        %d, //remote id\n", m->id);
    printf("        %s, //functional\n", booleanImage(m->functional));
    printf("        %s, //asynch\n", booleanImage(m->asynch));
    if (m->exceptions) {
        printf("        _xArr, //exceptions\n");
    } else {
        printf("        null, //exceptions\n");
    }
    printf("        %d, //no of args\n", list_size(m->arguments));
    if (m->returnType && m->returnType->uid) {
        printf("        %s, //return arg uuid\n", 
            qoString(m->returnType->uid));
    } else {
        printf("        null, //return arg uuid\n");
    }
    printf("        _%s_skeleton);\n", objectTypeBaseName(m->object));
    
    /* define the arguments of the method */
    C_LOOP_BEGIN(m->arguments, Argument, a, idx, tempa0)
        printf("     %s.defineArg(\n", methodRepName(m));
        printf("         %d, //argIdx\n", idx);
        printf("         %s, //argName\n", qoString(argumentName(a)));
        printf("         %s, //sibling\n", booleanImage(a->sibling));
        printf("         %d, //direction\n", (int) a->direction);
        printf("         %s); //typeUid\n", qoString(a->type->uid));
    C_LOOP_END(idx) 
    printf("\n");
}

 
PRIVATE void
printDelegationMethod(Procedure m, char* callTo, char* extraArg)
/* Prints a method declaration m so that the method calls "callTo" with 
 * exactly the same arguments, except possibly a first extra argument 
 * "extraArg"
 */
{
    boolean addRemoteException = 0;
    char* methodSName;
    char* returnTypeDecl;
    char* returnStatement; 
    TypeSpec ts = getTypeSpecT0(m->object);
    if (ts) {
        if (ts->javaRemote) addRemoteException = 1;
    }
    if (getContextT(m->object)->p.iJavaRemote) {
        addRemoteException = 1;
    }
    if (stringlength(extraArg)) {
        extraArg = cat2(extraArg, comma0(m->arguments));
    }
    if (returnsVoid(m)) {
        returnTypeDecl = "void";
        returnStatement = "";
    } else {
        returnTypeDecl = typeDeclarator(myUrType(m->returnType));
        returnStatement = "return ";
    }
    methodSName = methodShortName(m);
    printf("    public %s %s(", returnTypeDecl, methodSName);
      printFormalArgList(m->arguments);
      printf(")");
      printThrowsClause(m->exceptions, addRemoteException);
      printf(" {\n");
    printf("        %s%s(%s",
        returnStatement,
        callTo, 
        extraArg
        );
      printActualParamList(m->arguments);
      printf(");\n");
    printf("    } //%s\n\n", methodSName);
} /*printDelegationMethod*/


PRIVATE void
printMethodStub(Procedure m, ObjRockType* rock) 
{
    char* returnTypeDecl;
    char* returnTypeInit;
    char* piece;
    char* methodRep;
    boolean async;
    Type returnType;
  
    async = isReallyAsync(m);
    printDelegationMethod(m, shortNameOfStaticStubProc(m), "this");
    
    if (returnsVoid(m)) {
        returnType = 0;
        returnTypeDecl = "void";
    } else {
        returnType = myUrType(m->returnType);
        returnTypeDecl = typeDeclarator(returnType);
        returnTypeInit = typeInitializer(type_kind(returnType));
    }
    
    printf("    public static %s %s(%s self%s",
           returnTypeDecl,
           shortNameOfStaticStubProc(m), 
           typeDeclarator(m->object), 
           comma0(m->arguments)
           );

      printFormalArgList(m->arguments);
      printf(")");
      printThrowsClause(m->exceptions, (rock->tSpec->javaRemote || rock->ih->p.iJavaRemote));
      printf(" {\n");

    printf("        xerox.ilu.IluCall _call;\n");
    printf("        xerox.ilu.IluUserException _userException = null;\n");
    if (! async) {
        printf("        int _ec = 0;\n");
    }

    if (! returnsVoid(m)) {
        printf("        %s _r = %s;\n", returnTypeDecl, returnTypeInit);                    
    }

    printf("        _call = xerox.ilu.IluCall.startCall(\n");
    
    methodRep = dotCat(stubClassSName(m->object), methodRepName(m));
    printf("            %s,\n", methodRep);
    printf("            (xerox.ilu.IluSurrogateObject) self\n");
    printf("            );\n");

    printf("        try {\n");
    printf("          _retry: do {\n");
    if (getOClass(m->object)->singleton) {
        piece = "0";
    } else {
        piece = cat3(
            "_call.szObject(self, true, ",
            dotCat(
                stubClassSName(m->object),
                rock->iluClassRepField
                ),
            ")"
            );
    }
    printf("            _call.startWriteRequest(_call.needsSizing() ? (\n");
    printf("                  %s", piece);
    LOOP_BEGIN(m->arguments, Argument, a, t1)
        char* arg;
        if (a->direction == In || a->direction == InOut) {
            if (a->direction == In) {
                arg = argumentName(a);
            } else if (a->direction == InOut) {
                arg = holderAccess(a);
            }
            printf("\n                + %s", ioSzPiece(a->type, arg));
        }
    LOOP_END()
    printf("\n                ) : 0);\n");
    if (! getOClass(m->object)->singleton) {
        printf("            _call.outObject(self, true, %s.%s);\n", 
            rock->stubClassShortName, 
            rock->iluClassRepField
            );
    }
    LOOP_BEGIN(m->arguments, Argument, a, t3)
        char* arg;
        if (a->direction == In || a->direction == InOut) {
            if (a->direction == In) {
                arg = argumentName(a);
            } else if (a->direction == InOut) {
                arg = holderAccess(a);
            }
            if (a->sibling) {printSiblingTest(a, arg, "            ");} 
            printf("            %s;\n", ioOutPiece(a->type, arg));
        }
    LOOP_END()

    printf("            _call.doneWriteRequest();\n");
    
    if (!async) {
        printf("            _ec = _call.startReadReply();\n");
        printf("            if (_ec == 0) {\n");
        if (returnType) {
            printf("                _r = %s;\n", ioInPiece(returnType));
        }
        LOOP_BEGIN(m->arguments, Argument, a, t5)
            char *arg;
            if (a->direction == Out || a->direction == InOut) {
                arg = holderAccess(a);
                printf("                %s = %s;\n", arg, ioInPiece(a->type));
            }
        LOOP_END()
        printf("            } else if (_ec == xerox.ilu.IluCall.retryCode) {\n");
        printf("              continue _retry;\n");
        if (m->exceptions) {
            printf("            } else if (_ec > 0) {\n");  
            printf("                _userException = _call.readException(%s, _ec);\n",
                methodRep
                );  
        }
        printf("            }\n");
        printf("            _call.doneReadReply();\n");
    }
    printf("            break _retry;\n");
    printf("          } while (true);\n");
    printf("        } finally {\n");
    printf("            _call.finishCall();\n");
    printf("        }\n");
    if (!async) {
        printf("        if (_ec != 0) {\n");
        if (m->exceptions) {
            printf("            switch (_ec) {\n");
            C_LOOP_BEGIN(m->exceptions, Exception, e, i, t6)
                printf("              case %d: throw (%s) _userException;\n",
                   i + 1,
                   exceptionJName(e)
                   );
            C_LOOP_END(i)
            printf("            }\n");
        }
        printf("            throw xerox.ilu.IluSystemExceptionBase.fromIluProtocolException(_ec);\n");
        printf("        }\n");
        if (! returnsVoid(m)) {
            printf("        return _r;\n");
        }
    }
    printf("    } //%s\n\n", shortNameOfStaticStubProc(m));
} /*printMethodStub*/


PRIVATE void
printSuperClassMethodStub1(Procedure m, void *x)
/* Defines the java methods for a superclass */
{
    IHandle superih = getContextT(m->object);
    char* callTo = cat3(
        accessStubClassName(m->object), 
        ".", 
        shortNameOfStaticStubProc(m)
        );
    printDelegationMethod(m, callTo, "this");
}


PRIVATE void
printSuperClassMethodStubs1(Type t, ObjRockType* rock)
/* Defines the class methods for this particular superclasses
 * Reuses static stub from superclass.
 */
{
    Class c = class_object(t);
    list_enumerate(c->methods, (EnumProc) printSuperClassMethodStub1, 0);
}


PRIVATE void
printMethodRepDecl(Procedure m, void *x)
/*
 *   declare a var of type xerox.ilu.IluMethodRep for m.
 */
{
    printf("    static xerox.ilu.IluMethodRep %s;\n", 
               methodRepName(m));
}


PRIVATE void
printeSkeletonClass(ObjRockType* rock)
/* defines the skeleton class in the current stub java file
 */
{
    Type t = rock->t;
    Class c = class_object(t);

    printf("class _%s_skeletonClass ", objectTypeBaseName(t));
    printf("implements xerox.ilu.IluSkeleton {\n");
    printf("    _%s_skeletonClass() {\n", objectTypeBaseName(t));
    printf("    }\n\n");

    printf("  public void serveRequest(xerox.ilu.IluCall _call, xerox.ilu.IluMethodRep _m) \n");
    printf("            throws org.omg.CORBA.SystemException {\n");
    printf("    int _sz = 0;\n");
    printf("    switch(_m.methodIdx) {\n");
    
    rock->methodidx = 0;
    list_enumerate(c->methods, (EnumProc) defineServantMethod, rock);
    
    printf("      default:\n");
    printf("    }\n");
    printf("  } //serveRequest\n\n");
    printf("} //_%s_skeletonClass\n\n", objectTypeBaseName(t));
} /*printeSkeletonClass*/


PUBLIC char * obj_ioSzPiece(Type t, const char *arg) {
    t = obj_assert(t);
    return cat5(
        "_call.szObject(", 
        arg, 
        ", false, ", 
        accessIluClass(t), 
        ")"
        );
}


PUBLIC char * obj_ioOutPiece(Type t, const char *arg) {
    t = obj_assert(t);
    return cat5(
        "_call.outObject(", 
        arg, 
        ", false, ", 
        accessIluClass(t), 
        ")"
        );
}


PUBLIC char * obj_ioInPiece(Type t) {
    t = obj_assert(t);
    return cat5(
        "(", 
        typeDeclarator(t), 
        ") _call.inObject(false, ", 
        accessIluClass(t), 
        ")"
        );
}

PRIVATE void
printOpExtends(Type t, OperationsRockType* x)
/* for the operations class definition */
/* There is no real need to include all classes recursively, except that
 * it makes the generated java code easier to read
 */
{
    t = obj_assert(t);

    /* dont print CORBA::Object in Operations */
    if (x->skipCorbaObject) {
        if (isCorbaDotObject(t)) {return;}
    }
    
    if (x->printExtends) {
        printf("extends ");
    } else {
        printf(", ");
    }
    x->printExtends = FALSE;
    
    if (x->extendOperations) {
        printf("%s", accessOperationsTypeName(t));
    } else {
        printf("%s", accessRefTypeName(t));
    }
}


PRIVATE void
printReferenceInterface(ObjRockType* rock)
{
    OperationsRockType* oRock = iluparser_Malloc(sizeof(OperationsRockType));
    boolean separateOperations;
    Type t = rock->t;
    Class c = class_object(t);
    IHandle ih = rock->ih;
    char* refTypeName = refTypeSName(t);
    separateOperations = useOperationsInterface(t);
    NewJavaFile(t->interface, ih, refTypeName);
    printOpenDocComment("");
    if (c->doc_string) {
        printCommentLines(c->doc_string);
    } 
    printCommentLines("An ILU object type.\n");
    printCloseComment();
    printf("public interface %s ", refTypeName);
    oRock->printExtends = TRUE;
    oRock->extendOperations = FALSE;
    oRock->skipCorbaObject = TRUE;
    if (extendsCorbaDotObject(t)) {
        oRock->printExtends = FALSE;
        printf("extends %s.CORBA.Object", orgDotOmg);
    } else if (rock->tSpec->javaRemote || rock->ih->p.iJavaRemote) {
        oRock->printExtends = FALSE;
        printf("extends java.rmi.Remote");
    } else if (!(rock->tSpec->noIluObject || rock->ih->p.iNoIluObject)) {
        oRock->printExtends = FALSE;
        printf("extends %s", iluObject);
    }
    enumerateSuperClasses(t, (EnumProc) printOpExtends, oRock, TRUE, FALSE);
    if (separateOperations) {
        printf(", %s", operationsTypeSName(t));
    } 
    printf(" {\n");
    generateNestedConstants(ih, easyShortTypeNameCleaned(t));
    if (! separateOperations) {
        list_enumerate(c->methods, (EnumProc) printSignatureForOperations, rock);
    }
    printf("\n}//%s\n\n", refTypeName);
} /*printReferenceInterface*/


PRIVATE void
printOperationsInterface(ObjRockType* rock)
{
    OperationsRockType* oRock = iluparser_Malloc(sizeof(OperationsRockType));
    Type t = rock->t;
    Class c = class_object(t);
    IHandle ih = rock->ih;
    char* operationsTypeName;
    if (! useOperationsInterface(t)) {return;}
    operationsTypeName = operationsTypeSName(t);
    NewJavaFile(t->interface, ih, operationsTypeName);
    printOpenDocComment("");
    if (c->doc_string) {
        printCommentLines(c->doc_string);
    }
    printCommentLines("An operations interface for an ILU object type.\n");
    printCloseComment();
    printf("public interface %s ", operationsTypeName);
    oRock->printExtends = TRUE;
    oRock->extendOperations = TRUE;
    oRock->skipCorbaObject = TRUE;
    enumerateSuperClasses(t, (EnumProc) printOpExtends, oRock, TRUE, FALSE);
    printf(" {\n");
    list_enumerate(c->methods, (EnumProc) printSignatureForOperations, rock);
    printf("\n}//%s\n\n", operationsTypeName);
} /*printOperationsInterface*/


PRIVATE void
printDelegationStub(Procedure m, void *x)
{
    char* callTo = cat2("_delegate.", methodShortName(m));
    printDelegationMethod(m, callTo, "");
}


PRIVATE void
printSuperDelegationStub1(Type t, ObjRockType* rock)
/* Defines the class methods for this class (superclass)
 * Reuses static stub from superclass.
 */
{
    Class c = class_object(t);
    list_enumerate(c->methods, (EnumProc) printDelegationStub, 0);
}


PRIVATE void
printDelegationClass(ObjRockType* rock)
{
    boolean withAnchor = 0;
    Type t = rock->t;
    Class c = class_object(t);
    char* refTypeName;
    char* classShortName = cat2(rock->otJBName, rock->ih->p.delClassSuffix);
    NewJavaFile(t->interface, rock->ih, classShortName);
    refTypeName = obj_typeDeclarator(t);
    printOpenDocComment("");
    if (c->doc_string) {
        printCommentLines(c->doc_string);
    }
    printCommentLines("A delegation class for an ILU object type.\n");
    printCloseComment();
    printf("public class %s implements %s", classShortName, refTypeName);
        if (withAnchor) {
            printf(", xerox.ilu.IluAnchorSupport");
        }
        printf(" {\n");
    printf("    private %s _delegate = null;\n", refTypeName);
    if (withAnchor) {
        printf("    private java.lang.Object _internal = null;\n");
    }
    printf("\n");
    printf("    public %s() {\n", classShortName);
    printf("    }\n\n");
    printf("    public %s(%s _to) {\n", classShortName, refTypeName);
    printf("        __setDelegant(_to);\n");
    printf("    }\n\n");
    printf("    public void __setDelegant(%s _to) {\n", refTypeName);
    printf("        _delegate = _to;\n");
    printf("    }\n\n");
    if (withAnchor) {
        printf("    public void rememberAnchor(java.lang.Object _internal) {\n");
        printf("        this._internal = _internal;\n");
        printf("    }\n\n");
    }
    enumerateSuperClasses(t, 
        (EnumProc) printSuperDelegationStub1, rock, TRUE, TRUE
        );
    printf("} //%s\n\n", classShortName);
}


PRIVATE void
printFinishTryIluSystemException(char* otJBName)
/* code which catches an SystemException and re-throws it
 * as an to make sure it can be thrown in 
 * static initializers.  That used to be more important in
 * old ilu versions where SystemException where not runtime errors
 */
{
    printf("    } catch (org.omg.CORBA.SystemException e) {\n");
    /* Java reports this error very poorly; 
     * so we will print it ourself first.
     */
    printf("        System.err.println(\"**error registering %s: \" + e);\n",
        otJBName
        );
    printf("        e.printStackTrace(System.err);\n");
    printf("        throw e;");
    printf("    }\n");
} /*printFinishTryIluSystemException*/


PRIVATE void
printStubClass(ObjRockType* rock)
{
    Type t = rock->t;
    IHandle ih = rock->ih;
    Class c = class_object(t);
    char* otJBName = rock->otJBName;
    char* refTypeName;
    char* iluClassRepField = rock->iluClassRepField;
    char* stubClassShortName = rock->stubClassShortName;
    
    NewJavaFile(t->interface, ih, stubClassShortName);
    LoadJavaClass(stubClassShortName);
    refTypeName = obj_typeDeclarator(t);
    printOpenDocComment("");
    if (c->doc_string) {
        printCommentLines(c->doc_string);
    }
    printCommentLines("A stub class implementing surrogates for an ILU object type.\n");
    printCloseComment();
    printf("public class %s extends ", stubClassShortName);
    printf("xerox.ilu.IluSurrogateObject implements %s {\n\n", refTypeName);


    printf("    static xerox.ilu.IluClassRep %s;\n", iluClassRepField);
    

    list_enumerate(c->methods, (EnumProc) printMethodRepDecl, NULL);

    if (anyMethodHasExceptions(rock)) {
        printf("    static private xerox.ilu.IluExceptionRep[] _xArr = null;\n");
    }
    printf("    static private _%s_skeletonClass ", otJBName);
    printf("_%s_skeleton = new _%s_skeletonClass();\n\n", \
           otJBName, otJBName);

    printf("  static {\n");
    printf("    %s\n", stubConsistencyCheck());
    if (list_size(c->superclasses)>0) {
        printf("    xerox.ilu.IluClassRep[] ca = new xerox.ilu.IluClassRep[%d];\n",
           list_size(c->superclasses));
    }
    printf("    try {\n");
    
    printf("        %s = xerox.ilu.IluClassRep.setupClass(\n", iluClassRepField);
    printf("            \"%s\", //java reference interface name\n",  
        dotCat(packagePrefixJ(currentIH), otJBName)
        );
    printf("            \"%s\", //ilu object type name\n",
        dotCat(packagePrefix0(currentIH), name_base_name(t->name)) 
        );
    printf("            %s, //uuid\n", qoString(t->uid));
    printf("            %d); //method count\n", list_size(c->methods));
    
    if (list_size(c->superclasses)>0) {
        rock->methodidx = 0;
        list_enumerate(c->superclasses, (EnumProc) printSuperClassRep, rock);
        printf("        %s.setSuperClasses(ca);\n", 
            iluClassRepField);
    }
    
    if (c->optional) {
        printf("        %s.setOptional();\n", 
            iluClassRepField);
    }
    
    if (c->collectible) {
        printf("        %s.setCollectable();\n", 
            iluClassRepField);
    }
    
    if (c->brand) {
        printf("        %s.setBrand(%s);\n", 
            iluClassRepField,
            qoString(c->brand));
    }
    
    if (c->singleton) {
        printf("        %s.setSingleton(%s);\n", 
            iluClassRepField,
            qoString(c->singleton));
    }
    
    if (c->doc_string) {
        printf("        %s.setDocString(%s);\n", 
            iluClassRepField,
            qoString(c->doc_string));
    }
    
    if (1) {
        printf("        %s.setSurrClass(\"%s.%s\");\n", 
            iluClassRepField,
            packageName(t->interface, t->scoping),
            stubClassShortName
            );
    }

    if (rock->ih->p.genFactory) {
        printf("        %s.setSurrFactory(new _%s_Factory());\n", 
            iluClassRepField,
            otJBName);
    }
    
    if (ih->p.genHlp) {
        printf("        %s.setIfName(%s); \n", 
            iluClassRepField,
            qoString(interface_name(currentIfc))
            );
        if (currentIfc->brand) {
            printf("        %s.setIfBrand(%s); \n", 
                iluClassRepField,
                qoString(currentIfc->brand)
                );
        }
    }
    
    printFinishTryIluSystemException(otJBName);

    rock->methodidx = 0;
    list_enumerate(c->methods, (EnumProc) printMethodRegistration, rock);

    printf("    try {\n");
    printf("        %s.finishClass();\n", iluClassRepField);
    if (ih->p.genHlp) {
        printf("        %s.id(); //makes sure helper class is loaded\n", 
            helperClassShortName(t)
            );
    }
    printFinishTryIluSystemException(otJBName);
    printf("  }//static\n\n");

    printf("    /** returns ilu class implemented by this stub class */\n");
    printf("    static public final xerox.ilu.IluClassRep iluClass() {\n");
    printf("        return %s;\n", iluClassRepField);
    printf("    }//iluClass\n\n");
    
    list_enumerate(c->methods, (EnumProc) printMethodStub, rock);
    
    enumerateSuperClasses(t, 
        (EnumProc) printSuperClassMethodStubs1, rock, TRUE, FALSE
        );
    printf("    public static void registerTrueObject(\n");
    printf("        java.lang.String _ih,\n");
    printf("        %s%s _tob,\n", 
           otJBName,
           getContextT(t)->p.refSuffix);
    printf("        xerox.ilu.IluServer _s) ");
    printf("throws org.omg.CORBA.SystemException {\n");
    printf("        xerox.ilu.Ilu.registerTrueObject(_ih, _tob, _s, %s, 0);\n",
        iluClassRepField);
    printf("    } //registerTrueObject\n\n");

    printf("    /** ilu use only */\n");
    printf("    public %s() {\n", stubClassShortName);
    printf("    }\n");

    printf("    /** ilu use only */\n");
    printf("    public %s(java.lang.Object arg) {\n", stubClassShortName);
    printf("    }\n");

    printLoadClasses(rock->ih, 0);
    
    printf("} //%s\n\n", stubClassShortName);
    
    /* the file is not yet finished */
}


PRIVATE void
printFactoryClass(ObjRockType* rock)
/*
 * in same file as stubClass
 */
{
    Type t = rock->t;
    IHandle ih = rock->ih;
    char* otJBName = objectTypeBaseName(t);
     
    printf("class _%s_Factory extends xerox.ilu.IluFactory {\n", otJBName);
    printf("     _%s_Factory() { };\n", otJBName);
    printf("    public java.lang.Object createSurrogateObject(java.lang.Object _arg) {\n");
    printf("        return new %s(_arg);\n", stubClassSName(t));
    printf("    }\n");
    printf("} // _%s_Factory\n\n", otJBName);
}


PRIVATE void
printImplClass(ObjRockType* rock)
/* defines an impl class */
{
    boolean withAnchor = 0;
    Type t = rock->t;
    Class c = class_object(t);
    char* classShortName = 
        cat3(rock->ih->p.implPrefix, rock->otJBName, rock->ih->p.implSuffix);
    char* refTypeName;
    NewJavaFile(t->interface, rock->ih, classShortName);
    refTypeName = obj_typeDeclarator(t);
    printOpenDocComment("");
    if (c->doc_string) {
        printCommentLines(c->doc_string);
    }
    printCommentLines("A sample implementation for a true ILU object.\n");
    printCloseComment();
    printf("public abstract class %s extends %s.CORBA.portable.ObjectImpl implements %s", classShortName, orgDotOmg, refTypeName);
        if (withAnchor) {
            printf(", xerox.ilu.IluAnchorSupport");
        }
        printf(" {\n\n");
    if (withAnchor) {
        printf("    private java.lang.Object _internal = null;\n");
        printf("    /** implements xerox.ilu.IluAnchorSupport */\n");
        printf("    public void rememberAnchor(java.lang.Object _internal) {\n");
        printf("        this._internal = _internal;\n");
        printf("    }\n\n");
    }
    printf("    protected xerox.ilu.IluClassRep _implements() {\n");
    printf("        return %s;\n", accessIluClass(t));
    printf("    } //_implements\n\n");
    
    /* added June 22, 1998 7:33:55 pm PDT to support visigenics 3.2 */
    printf("    public %s _this() {\n", refTypeName);
    printf("        return this;\n");
    printf("    } //_this\n\n");
    
    /* added June 22, 1998 7:33:55 pm PDT to support visigenics 3.2 */
    printf("    public %s () {\n", classShortName);
    printf("    } //constructor\n\n");
    
    /* added June 22, 1998 7:33:55 pm PDT to support visigenics 3.2 */
    printf("    protected %s (java.lang.String name) {\n", classShortName);
    printf("    } //constructor\n\n");
    
    printf("    /** purly for reasons of corba compatibility */\n");
    printf("    public java.lang.String[] _ids() {\n");
    printf("        throw new org.omg.CORBA.NO_IMPLEMENT();\n");
    printf("    } //_ids\n\n");
    

    printf("} //%s\n\n", classShortName);
    
} /* printImplClass */


PRIVATE void
printPOAInheritClass(ObjRockType* rock)
/* defines an impl class */
{
    boolean withAnchor = 0;
    Type t = rock->t;
    Class c = class_object(t);
    char* classShortName = 
        cat2("POA_", rock->otJBName);
    NewJavaFile(t->interface, rock->ih, classShortName);
    printf("//****NOT CORBA COMPATIBLE IN ITSELF; GENERATED SO THAT\n");
    printf("//****MANUALLY WRITTEN CLIENT CODE CAN BE CORBA COMPATIBLE\n");
    printOpenDocComment("");
    if (c->doc_string) {
        printCommentLines(c->doc_string);
    }
    printCommentLines("An operations interface for an ILU object type.\n");
    printCloseComment();
    printf("public abstract class %s\n", classShortName); 
        printf("        extends %s.PortableServer.DynamicImplementation\n", orgDotOmg); 
        printf("        implements %s ", refTypeSName(t));
        if (withAnchor) {
            printf(", xerox.ilu.IluAnchorSupport");
        }
        printf(" {\n\n");
        printf("      //should implements %s\n", operationsTypeSName(t));
    if (withAnchor) {
        printf("    private java.lang.Object _internal = null;\n");
        printf("    /** implements xerox.ilu.IluAnchorSupport */\n");
        printf("    public void rememberAnchor(java.lang.Object _internal) {\n");
        printf("        this._internal = _internal;\n");
        printf("    }\n\n");
    }
    printf("    protected xerox.ilu.IluClassRep _implements() {\n");
    printf("        return %s;\n", accessIluClass(t));
    printf("    } //_implements\n\n");
    
    printf("    //A FEW CORBA METHODS MISSING\n\n");
    printf("} //%s\n\n", classShortName);
} /* printPOAInheritClass */


PRIVATE void
printPOA_TIE_Class(ObjRockType* rock)
/* defines a tie class */
{
    Type t = rock->t;
    Class c = class_object(t);
    char* refTypeName;
    char* classShortName = cat3("POA_", rock->otJBName, "_tie");
    char* operationsShortName = operationsTypeSName(t);
    NewJavaFile(t->interface, rock->ih, classShortName);
    printf("import %s.PortableServer.POA;\n\n", orgDotOmg);
    
    refTypeName = obj_typeDeclarator(t);
    printOpenDocComment("");
    if (c->doc_string) {
        printCommentLines(c->doc_string);
    }
    printCommentLines("An POA_ (Tie) class for an ILU object type.\n");
    printCommentLines("NOT CORBA COMPATIBLE IN ITSELF; GENERATED SO THAT\n");
    printCommentLines("MANUALLY WRITTEN CLIENT CODE CAN BE CORBA COMPATIBLE.\n");
    printCloseComment();
    printf("public class %s extends POA_%s {\n", classShortName, rock->otJBName);
    printf("    private %s _delegate = null;\n", operationsShortName);
    printf("    private POA _poa = null;\n\n");
    
    printf("    //ilu only\n");
    printf("    public %s() {\n", classShortName);
    printf("    }//constructor\n\n");
    
    printf("    public %s(%s delegate) {\n", 
        classShortName, operationsShortName);
    printf("        _delegate = delegate;\n");
    printf("    }//constructor\n\n");
    
    printf("    public %s(%s delegate, POA poa) {\n", 
        classShortName, operationsShortName);
    printf("        _delegate = delegate;\n");
    printf("        _poa = poa;\n");
    printf("    }//constructor\n\n");
    
    printf("    public %s _delegate() {\n", operationsShortName);
    printf("        return _delegate;\n");
    printf("    }//_delegate\n\n");
    
    printf("    public void _delegate(%s delegate) {\n", operationsShortName);
    printf("        _delegate = delegate;\n");
    printf("    }//_delegate\n\n");
    
    printf("    public POA _default_POA() {\n");
    printf("        if (_poa != null) {\n");
    printf("            return _poa;\n");
    printf("        } else {\n");
    printf("            return super._default_POA();\n");
    printf("        }\n");
    printf("    }//_default_POA\n\n");
    
    enumerateSuperClasses(t, 
        (EnumProc) printSuperDelegationStub1, rock, TRUE, TRUE
        );
    printf("} //%s\n\n", classShortName);
} /* printPOA_TIE_Class */



PRIVATE void
printVisiVarClass(ObjRockType* rock)
/* print a visigenic style var class */
{
    Type t = rock->t;
    char* classShortName = cat2(rock->otJBName, "_var");
    char* refTypeName;
    NewJavaFile(t->interface, rock->ih, classShortName);
    refTypeName = obj_typeDeclarator(t);
    printf("public class %s {\n", classShortName);
    
    printf("    public static %s narrow(java.lang.Object _obj) throws org.omg.CORBA.SystemException {\n", rock->otJBName);
    printf("        return (%s) _obj;\n", rock->otJBName);
    printf("    } //narrow\n\n");
    
    printf("    public static %s bind(java.lang.String s) throws org.omg.CORBA.SystemException {\n", rock->otJBName);
    printf("        throw new org.omg.CORBA.NO_IMPLEMENT();\n");
    printf("    } //bind\n\n");
    
    printf("    /* assert classes loaded ... */\n");
    printf("    private static xerox.ilu.IluClassRep _c = %s;\n", accessIluClass(t));
    if (rock->ih->p.genHlp) {
        printf("    private static java.lang.String _i = %s.id();\n", helperClassShortName(t));
    }
    
printf("} //%s\n\n", classShortName);
}


PUBLIC void
obj_helpInnerCallback(Type t)
{
    printf("    public static %s narrow(java.lang.Object _x) {\n",
        typeDeclarator(t)
        );
    printf("        return (%s) _x;\n", typeDeclarator(t));
    printf("    } //narrow \n\n");
} /* obj_helpInnerCallback */


PUBLIC void
obj_defineMain(Type t)
{
    ObjRockType* rock = iluparser_Malloc(sizeof(ObjRockType));
    
    rock->methodidx = 0;
    rock->t = t;
    rock->ih = getContextT(rock->t);
    rock->otJBName = objectTypeBaseName(rock->t);
    rock->stubClassShortName = stubClassSName(t);
    rock->iluClassRepField = copy("_classRep"); 
    rock->tSpec = getTypeSpecT0(t);
    if (rock->tSpec == 0) {rock->tSpec = getTypeSpec(rock->ih, "*");}
    
    if (useOperationsInterface(t)) {
        printOperationsInterface(rock);
        printPOAInheritClass(rock);
        printPOA_TIE_Class(rock);
    }
    printReferenceInterface(rock);
    
    if (rock->ih->p.genDel) {printDelegationClass(rock);}
    if (rock->ih->p.genImpl) {printImplClass(rock);}
    if (rock->ih->p.genVisiVar) {printVisiVarClass(rock);}
    
    printStubClass(rock);
    if (rock->ih->p.genFactory) {printFactoryClass(rock);}
    printeSkeletonClass(rock);
    
} /* obj_defineMain */


/* end */





