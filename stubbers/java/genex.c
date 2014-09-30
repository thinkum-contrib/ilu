/* genex.c */
/* Chris Jacobi, November 16, 1998 3:39 pm PST */

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

/* $Id: genex.c,v 1.22 1999/09/09 22:41:56 janssen Exp $ */


#include <stdio.h>
#include <string.h>

#include "iluptype.h"
#include "shared.h"
#include "stubops.h"
#include "name.h"
#include "util.h"
#include "context.h"
#include "genex.h"
#include "genrecord.h"
#include "io.h"


PRIVATE boolean extraFile(Exception e)
/* Optionally generate an extra file so that the 
 * main exception class does no ilu imports and
 * can be used stand alone
 */
{
    IHandle ih = getContext(e->interface, e->scoping);
    if (ih->p.extraFiles) {
        if (ih->p.extraSuffix) return TRUE;
    }
    return FALSE;
}


PRIVATE char* handlerClassShortName(Exception e)
{
    char * n;
    if (extraFile(e)) {
        IHandle ih = getContext(e->interface, e->scoping);
        n = cat2(exceptionShortName(e), ih->p.extraSuffix);
    } else {
        n = exceptionShortName(e);
    }
    return n;
} /*handlerClassShortName*/


PRIVATE char*
methodNameOfWriteProc(Exception e)
{
    char* shortname = exceptionShortName(e);
    return cat3("_", shortname, "_marshall"); 
}


PUBLIC char*
fullNameOfWriteProc(Exception e)
{
    Type eArgT = exception_type(e);
    if (eArgT) {
        return cat5(
            packageName(e->interface, e->scoping),	/*Package*/
            ".",
            handlerClassShortName(e),         	/*Class*/
            ".",
            methodNameOfWriteProc(e) 	/*Method*/
            ); 
    } else {
        return "xerox.ilu.IluCall.simpleWriteException";
    }
}

PRIVATE boolean
isIDLExceptionRecord(Type t)
{
    if (rec_is_a(t)) {
        char* idlTypeName = type_name(t);
        return isPrefixOf(idlExceptionPrefix, idlTypeName); 
    }
    return FALSE;
}


PRIVATE char* exceptionRepNamePiece(Exception e)
{
    char* s;
    while (e->import) {
        e = e->import;
    }
    s = cat3("_", exceptionShortName(e), "getRep()");
    return s;
}


PUBLIC char* exceptionRep(Exception e)
{
    IHandle ih;
    char* s;
    char* className;
    while (e->import) {
        e = e->import;
    }
    ih = getContext(e->interface, e->scoping);
    className = handlerClassShortName(e);
    s = dotCat(className, exceptionRepNamePiece(e));
    ih = getContext(e->interface, e->scoping);
    if (ih != currentIH) {
        s = dotCat(packagePrefixJ(ih), s);
    } 
    return s;
}

void printHandler(
    Exception e, char* shortname, char* recField, 
    boolean idlHack, char* handlerShortName)
{
    Type eArgT = exception_type(e);
    printf("    private static xerox.ilu.IluExceptionRep _%s_rep;\n", 
           shortname);
    
    printf("    public static final xerox.ilu.IluExceptionRep %s {\n", 
        exceptionRepNamePiece(e) /* know: is shortname with parenthesis */
        );
    printf("        return _%s_rep;\n", shortname);
    printf("    }\n\n");
    
    if (eArgT) {
        printf("    //NOT really public; Must be accessible by ilu...\n");
        printf("    public void readException(java.lang.Object call)\n");
        printf("            throws org.omg.CORBA.SystemException {\n");
        printf("        xerox.ilu.IluCall _call = (xerox.ilu.IluCall) call;\n");
        printf("        this.%s = %s;\n", recField, ioInPiece(eArgT));
        if (idlHack) {
            printRecordAssignCorresponding(eArgT, 
                "        this.", 
                cat3("this.", recField, ".")
                );
        }
        printf("    } //readException\n\n");
        
        
        /* This is static because the dynamic exception might be 
         * a subclass which is not supported in the ISL definition
         */
        printf("    //NOT really public; Must be accessible by ilu...\n");
        printf("    public static void %s(%s _ex, int index, xerox.ilu.IluCall _call)\n",
            methodNameOfWriteProc(e),
            shortname
            );
        printf("            throws org.omg.CORBA.SystemException {\n");
        printf("        int sz = 0;\n");
        printf("        if (_call.needsSizing()) {\n");
        printf("            sz = _call.beginSizingException(index);\n");
        if (eArgT) {
            printf("            sz += %s;\n", 
                ioSzPiece(eArgT, cat2("_ex.", recField)));
        }
        printf("        }\n");
        printf("        _call.startWriteException(index, sz);\n");
        if (eArgT) {
            printf("        %s;\n", 
                ioOutPiece(eArgT, cat2("_ex.", recField)));
        }
        printf("        _call.doneWriteException();\n");
        printf("    } //%s\n\n", methodNameOfWriteProc(e));

    }
    
    printf("    static {\n");
    printf("        _%s_rep = xerox.ilu.IluExceptionRep.defineException(\n", 
        shortname
        );
    printf("            \"%s\", //java class name\n",
        dotCat(packagePrefixJ(currentIH), handlerShortName)
        );
    if (e->corba_rep_id) {
      printf("            null,   //to indicate use of CORBA repository ID\n"
	     "            \"%s\", //CORBA repository ID\n", e->corba_rep_id);
    } else {
      printf("            \"%s\", //isl interface name\n",
	     interface_name(currentIfc)
	     );
      printf("            \"%s\", //isl exception name\n", 
	     exceptionShortName(e)
	     );
    }
    if (eArgT) {
        printf("            \"%s\"); //type uuid\n", eArgT->uid);
    } else {
        printf("            null); //type uuid\n");
    }
    
    printf("    }\n\n");

} /*printHandler*/

 
PRIVATE void printFullConstructorHead(Exception e, char* name, boolean idlHack)
{
    Type eArgT = exception_type(e);
    printf("    public %s(", name);
    if (idlHack) {
        printRecordConstructorFormalArgs(eArgT);
    } else {
        printf("%s _val", typeDeclarator(eArgT));
    }
    printf(") {\n");
    printf("        super();\n");
}


PUBLIC void
map_exception(Exception e, void* x)
{
    boolean idlHack = FALSE;
    char* shortname;
    char* handlerShortName;
    char* recField = "value";
    Type eArgT = exception_type(e);
    
    if (e->import != 0) {
        return;
    }
    shortname = exceptionShortName(e);
    handlerShortName = handlerClassShortName(e);
    if (eArgT) {
        idlHack = isIDLExceptionRecord(eArgT);
        if (idlHack) recField = "_idlrec";
    }

    NewJavaFile(e->interface, getContext(e->interface, e->scoping), shortname);
    LoadJavaClass(shortname);

    printOpenDocComment("");
    if (e->doc_string != NULL) {
        printCommentLines(e->doc_string);
    }
    printCommentLines("Class representing an ILU user exception.\n");
    printCloseComment();
    printf("public class %s extends xerox.ilu.IluUserException {\n\n", 
          shortname);
    
    if (eArgT) {
        char* declarator = typeDeclarator(eArgT);
        if (idlHack) {
            printRecordFieldDecls(eArgT, TRUE);
            printf("    %s %s = %s;\n\n", 
                declarator, recField,
                typeInitializer(type_kind(myUrType(eArgT)))
                );
            printFullConstructorHead(e, shortname, idlHack);
            printf("        %s = new %s();\n", recField, declarator);
            printRecordAssignCorresponding(eArgT, 
                cat3("        ", recField, "."), 
                "");
            printf("    }\n\n");
        } else {
            printf("    public %s %s = %s;\n\n", 
                declarator, recField, 
                typeInitializer(type_kind(myUrType(eArgT)))
                );
            printFullConstructorHead(e, shortname, idlHack);
            printf("        this.%s = _val;\n", recField);
            printf("    }\n\n");
        }
    }

    printf("    public %s() {\n", shortname);
    printf("        super();\n");
    printf("    }\n\n");
    
    if (! extraFile(e)) {
        printHandler(e, shortname, recField, idlHack, handlerShortName);
    }
    
    printf("} //%s\n", shortname);
    
    if (extraFile(e)) {
        NewJavaFile(
            e->interface, getContext(e->interface, e->scoping), handlerShortName
            );
        LoadJavaClass(handlerShortName);
        printf("public class %s extends %s {\n\n",
            handlerShortName, shortname);
        
        printf("    public %s() {\n", handlerShortName);
        printf("        super();\n");
        printf("    }\n\n");

        if (eArgT) {
            printFullConstructorHead(e, handlerShortName, idlHack);
            printf("        //not used; but required by type system\n");
            printf("        throw new org.omg.CORBA.NO_IMPLEMENT();\n");
            printf("    }\n\n");
        }
        
        printHandler(e, shortname, recField, idlHack, handlerShortName);
        printf("} //%s\n", handlerShortName);
    }
    
} /*map_exception*/


/* end */




