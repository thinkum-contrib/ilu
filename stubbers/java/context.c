/* context.c */
/* Chris Jacobi, February 11, 1999 9:57 am PST */

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
 
/* $Id: context.c,v 1.30 1999/08/03 01:51:18 janssen Exp $ */


#include <stdio.h>
#include <string.h>
#include "iluptype.h" 
#include "shared.h"
#include "context.h"
#include "stubops.h"
#include "name.h"

#define AND &&


/*public*/
IHandle currentIH;
Interface currentIfc;

PUBLIC void setCurrentIfc(Interface ifc)
{
    currentIfc = ifc;
}

PRIVATE IHandle rootContext = 0;
PRIVATE IHandle emptyNameContext = 0;


PRIVATE boolean ihMatch(IHandle ctx, char* b)
{
    return (strcmp(ctx->i.step0, b) == 0);
}


PRIVATE IHandle newContext(IHandle parent)
{
    IHandle c = (IHandle) iluparser_Malloc(sizeof(struct context_s));
    memset((char *)c, 0, sizeof(struct context_s));
    c->i.children = new_list();
    c->i.stuffToLoad = new_list();
    c->i.up = parent;
    return c;
} /*newContext*/


PUBLIC IHandle getContextRoot()
{
    IHandle ih = rootContext;
    if (ih==0) {
        ih = newContext(0);
        rootContext = ih;
    }
    return ih;
} /*getContextRoot*/


PRIVATE IHandle newChild(IHandle ctx)
{
    IHandle ih = newContext(ctx);
    if (ctx) {
        ih->p = ctx->p;
    }
    return ih;
} /*newChild*/


PUBLIC IHandle getChildContext(IHandle ph, char* piece, boolean create) {
    IHandle found;
    if (ph->i.children==0) {
       ph->i.children = new_list();
    }
    found = (IHandle) 
        list_find(ph->i.children, (iluparser_FindProc) ihMatch, piece);
    if ((found == 0) && create) {
        found = newChild(ph);
        found->i.step0 = piece;
        found->i.stepJ = javaizeIdentSeq(found->i.step0); 
        list_insert(ph->i.children, found);
    }
    return found;
} /*getChildContext*/


PUBLIC IHandle getContext(Interface ifc, list scoping)
{
    IHandle ih = getContextRoot();
    listElement *ptr;
    if ((scoping==0) || (scoping->head==0)) {
        if (ifc==0) {
            return getContextC(0);
        }
        ih = getChildContext(ih, interface_name(ifc), 1);
        return ih;
    }
    /* step down scoping, but ommit last element as this is a feature name
     * and not a module name
     */
    for (
            ptr = scoping->head; 
            (ptr != NULL AND ptr->next != NULL);  
            ptr = ptr->next) {
        char* piece = (char*) ptr->data;
        ih = getChildContext(ih, piece, 1);
    }
    return ih;
} /*getContext*/


PRIVATE int dotPos(const char* s)
{
    int i;
    int length = stringlength(s);
    for (i = 0; i<length; i++) {
        if (s[i] == '.') return i;
    }
    return -1;
} /*dotPos*/


PRIVATE char* leftsubstring(const char* s, int cnt)
{
    int i;
    string buff;
    buff = iluparser_Malloc(cnt+1);
    for (i = 0; i<cnt; i++) {
        buff[i] = s[i];
    }
    buff[cnt] = 0;
    return buff;
} /*leftsubstring*/


PUBLIC IHandle getContextC(char * name)
{
    char* lp;
    int dp; 
    IHandle ih = getContextRoot();
    if (name==0) {
        if (emptyNameContext==0) {
            emptyNameContext = newChild(ih);
        }
        return emptyNameContext;
    }
    while (stringlength(name) > 0) {
        dp = dotPos(name);
        if (dp<0) return getChildContext(ih, name, 1);
        lp = leftsubstring(name, dp-1);
        ih = getChildContext(ih, lp, 1);
        name = name + dp + 1;
    }
    return ih;
} /*getContextC*/


PUBLIC IHandle getContextTraw(Type t)
{
    return getContext(t->interface, t->scoping);
} /*getContextTraw*/


PUBLIC IHandle getContextT(Type t)
{
    t = ur_type(t);
    return getContext(t->interface, t->scoping);
} /*getContextT*/


PRIVATE void defineChain(IHandle ih) {
    if (ih->i.chain0 == 0) {
        if (ih->i.up) {
            defineChain(ih->i.up);
            ih->i.chain0 = dotCat(ih->i.up->i.chain0, ih->i.step0);
            ih->i.chainJ = dotCat(ih->i.up->i.chainJ, ih->i.stepJ);
        } else {
            ih->i.chain0 = ih->i.step0;
            ih->i.chainJ = ih->i.stepJ;
        }
        if (ih->i.isIdlInterface) {
            ih->i.chain0 = cat2(ih->i.chain0, "Package");
            ih->i.chainJ = cat2(ih->i.chainJ, "Package");
        }
    }
} /*defineChain*/


PUBLIC char* packagePrefixMinus1(IHandle ih)
{
    if (ih->i.packagePrefix0==0) {
        defineChain(ih);
        ih->i.packagePrefix0 = ih->i.chain0; /*no prefix in isl space*/
    }
    if (ih->i.up) {
        return ih->i.up->i.chain0;
    }
    return 0;
} /*packagePrefixMinus1*/


PUBLIC char* packagePrefix0(IHandle ih)
{
    if (ih->i.packagePrefix0==0) {
        defineChain(ih);
        ih->i.packagePrefix0 = ih->i.chain0; /*no prefix in isl space*/
    }
    return ih->i.packagePrefix0;
} /*packagePrefix0*/


PUBLIC char* packagePrefixJ(IHandle ih)
{
    if (ih->i.packagePrefixJ==0) {
        defineChain(ih);
        if (ih->p.prefix) {
            ih->i.packagePrefixJ = 
                dotCat(ih->p.prefix, ih->i.chainJ);
        } else {
            ih->i.packagePrefixJ = ih->i.chainJ;
        }
    }
    return ih->i.packagePrefixJ;
} /*packagePrefixJ*/

     
PUBLIC void setCurrentIH(IHandle ih)
{
    currentIH = ih;
    if (ih AND ih->p.forbidden) {
        defineChain(ih);
        fatalError(cat2("Must not generate stubs for %s", ih->i.chain0));
    }
} /*setCurrentIH*/



PRIVATE TypeSpec newTypeSpec(char* iluTypeName) {
    TypeSpec ts = iluparser_Malloc(sizeof(struct typeSpec_s));
    memset((char *)ts, 0, sizeof(struct typeSpec_s));
    ts->iluTypeName = copy(iluTypeName);
    return ts;
} /*newTypeSpec*/


PRIVATE boolean
matchTypeSpec(TypeSpec tsp, char *s)
/* match proc used in list_find */
{
    return (strcmp(tsp->iluTypeName, s) == 0);
}  /*matchTypeSpec*/


PRIVATE TypeSpec getTypeSpec0(IHandle ih, char* iluTypeName)
{    
    refany found = 0;
    if (ih->i.typeSpecs) {
        found = list_find(ih->i.typeSpecs, 
            (iluparser_FindProc) matchTypeSpec, 
            (refany) iluTypeName
            );
    }
    return (TypeSpec) found;
} /*getTypeSpec0*/


PUBLIC TypeSpec getTypeSpecT0(Type t)
{
    IHandle ih;
    t = ur_type(t);
    ih = getContextT(t);
    return getTypeSpec0(ih, type_name(t));
} /*getTypeSpecT0*/


PUBLIC TypeSpec getTypeSpec(IHandle ih, char* iluTypeName)
{
    TypeSpec ts = 0;
    if (ih->i.typeSpecs == 0) {
        ih->i.typeSpecs = new_list();
    }
    ts = getTypeSpec0(ih, iluTypeName);
    if (ts == 0) {
        ts = newTypeSpec(iluTypeName);
        list_insert(ih->i.typeSpecs, ts);
    }
    return ts;
} /*getTypeSpec*/


/* end */
