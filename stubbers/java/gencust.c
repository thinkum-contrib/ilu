/* gencust.c */
/* Chris Jacobi, October 8, 1998 11:07 pm PDT */

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
 
/* $Id: gencust.c,v 1.7 1999/08/03 01:51:19 janssen Exp $ */
     
#include <stdio.h>
#include <string.h>
#include "iluptype.h"
#include "shared.h"
#include "stubops.h"
#include "util.h"
#include "name.h"

#include "context.h"
#include "io.h"
#include "stubops.h"
#include "gencust.h"
#include "hlpcls.h"

int custom_disabled = 0;

PRIVATE void
enableCustom ()
{
    custom_disabled = custom_disabled - 1;
} 

PRIVATE void
disableCustom ()
{
    custom_disabled = custom_disabled + 1;
} 


PRIVATE boolean
matchCustomSpec(CustomMappingSpec* csp, char *s)
/* match proc used in list_find */
{
    return (strcmp(csp->iluTypeName, s) == 0);
} 


PRIVATE CustomMappingSpec* findCustomSpec(Type t)
/* returns the CustomMappingSpec* for full custom mapped types; 0 otherwise */
{
    IHandle ih;
    refany found = 0;
    t = myUrType(t);
    ih = getContextT(t);
    if (ih->i.customMappings) {
        found = list_find(ih->i.customMappings, 
            (iluparser_FindProc) matchCustomSpec, 
            (refany) type_name(t)
            );
    }
    return (CustomMappingSpec*) found;
} /*findCustomSpec*/
 

PRIVATE CustomMappingSpec* getCustomSpec(Type t)
/* returns the CustomMappingSpec* for full custom mapped types; error otherwise */
{
    CustomMappingSpec* csp = findCustomSpec(t);
    if (csp==0) fatal("should have been a custom object");
    return csp;
} /*getCustomSpec*/


PUBLIC boolean custom_is_a(Type t)
{
    if (custom_disabled) return FALSE;
    return (findCustomSpec(t) != 0);
} 


PUBLIC Type custom_assert(Type t) {
    t = myUrType(t);
    if (! custom_is_a(t)) fatal("should have been a custom object");
    return t;
}


PUBLIC char * custom_typeDeclarator(Type t)
{
    CustomMappingSpec* csp = getCustomSpec(t);
    return copy(csp->javaTypeName);
}


PUBLIC char * custom_holderTypeDeclarator(Type t)
    /* returns 0 if not defined. Idea is: if it returns 0
     * use the standard holder name...
     */
{
    CustomMappingSpec* csp = getCustomSpec(t);
    return copy(csp->holderClassName);
}


PRIVATE char *
accessIOHelperClass(Type t) 
    /* assumes t is an custom mapped type,
     * returns string which accesses the class with the helper methods
     */
{
    return packageDotStringJ(getContextT(t), helperClassShortName(t));
}


PUBLIC char * custom_ioSzPiece(Type t, const char *arg)
{
    CustomMappingSpec* csp = findCustomSpec(t);
    char * className = accessIOHelperClass(t);
    return cat5(
        className, 
        ".", 
        "_customSzF(_call, ",
        arg,
        ")"
        );
} /*custom_ioSzPiece*/


PUBLIC char * custom_ioOutPiece(Type t, const char *arg)
{
    CustomMappingSpec* csp = findCustomSpec(t);
    char * className = accessIOHelperClass(t);
    return cat5(
        className, 
        ".", 
        "_customOutF(_call, ",
        arg,
        ")"
        );
} /*custom_ioOutPiece*/


PUBLIC char * custom_ioInPiece(Type t)
{
    CustomMappingSpec* csp = findCustomSpec(t);
    char * className = accessIOHelperClass(t);
    return cat3(
        className, 
        ".", 
        "_customInF(_call)"
        );
} /*custom_ioInPiece*/




PUBLIC void custom_PrintHelperStubs(Type t)
{
    CustomMappingSpec* csp = findCustomSpec(t);
    char* wireJTN;
    char* customJTN;
    
    disableCustom();
    wireJTN = typeDeclarator(t);
    enableCustom();
    
    customJTN = csp->javaTypeName;
    
    
    /* print static variable */
    printf("    static xerox.ilu.IluCustomMapping _gcm = null;\n\n");
    
    
    /* print registerCustomMapping */
    printf("    public static synchronized void\n"); 
    printf("            _registerCustomMapping(xerox.ilu.IluCustomMapping cm) {\n");
    printf("        if (_gcm == null) {\n");
    printf("            _gcm = cm;\n");
    printf("        } else {;\n");
    printf("            if (_gcm != cm) {\n");
    printf("                throw new xerox.ilu.IluCustomMappingException(%s);\n",
        qoString("multiple registrations")
        );
    printf("            }\n");
    printf("        }\n");
    printf("    } //_registerCustomMapping\n\n");
    
    
    /* print getCustomMapping */
    printf("    static xerox.ilu.IluCustomMapping _getCustomMapping() {\n");
    printf("        xerox.ilu.IluCustomMapping cm = _gcm;\n");
    printf("        if (cm != null) {return cm;}\n");
    if (stringlength(csp->loadThisClassName) > 0) {
        printf("        xerox.basics.Environment.loadClasses(%s);\n",
            qoString(csp->loadThisClassName) 
            );
        printf("        cm = _gcm;\n");
        printf("        if (cm != null) {return cm;}\n");
    }
    printf("        throw new xerox.ilu.IluCustomMappingException(%s);\n",
        qoString("no custom mapping")
        );
    printf("    } //_getCustomMapping\n\n");
    
    
    /* customInF */
    printf("    public static %s _customInF(xerox.ilu.IluCall _call) {\n",
        customJTN
        );
    printf("        %s _t;\n", customJTN);
    printf("        %s _wt = null;\n", wireJTN);
    printf("        xerox.ilu.IluCustomMapping _cm = _getCustomMapping();\n");
    disableCustom();
    printf("        _wt = %s;\n", ioInPiece(t));
    enableCustom();
    printf("        _t = (%s) _cm.iluCustomMapping_customFromIlu(_wt);\n",
        customJTN
        );
    printf("        return _t;\n");
    printf("    } //_customInF\n\n");
    
    
    
    /* print customSzF */
    printf("    public static int _customSzF(xerox.ilu.IluCall _call, %s _t) {\n",
        customJTN
        );
    printf("        int _sz = 0;\n");
    printf("        %s _wt = null;\n", wireJTN);
    printf("        xerox.ilu.IluCustomMapping _cm = _getCustomMapping();\n");
    printf("        _wt = (%s) _cm.iluCustomMapping_iluFromCustom(_t);\n",
        wireJTN
        );
    disableCustom();
    printf("        _sz = %s;\n", ioSzPiece(t, "_wt"));
    enableCustom();
    printf("        return _sz;\n");
    printf("    } //_customSzF\n\n");
    
    
    /* print customOutF */
    printf("    public static void _customOutF(xerox.ilu.IluCall _call, %s _t) {\n",
        customJTN
        );
    printf("        %s _wt = null;\n", wireJTN);
    printf("        xerox.ilu.IluCustomMapping _cm = _getCustomMapping();\n");
    printf("        _wt = (%s) _cm.iluCustomMapping_iluFromCustom(_t);\n",
        wireJTN
        );
    disableCustom();
    printf("        %s;\n", ioOutPiece(t, "_wt"));
    enableCustom();
    printf("    } //_customOutF\n\n");
    

}

/* end */



