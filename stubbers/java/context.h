/* context.h */
/* Chris Jacobi, January 7, 1999 8:37 am PST */

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
 
/* $Id: context.h,v 1.28 1999/08/03 01:51:18 janssen Exp $ */
/* see also context.c */

#ifndef _ILUJAVA_context_
#define _ILUJAVA_context_

typedef struct contextInternals_s {
    char* step0;		/* raw; original; search string */  
    char* stepJ;		/* java cleaned */  
    char* chain0;		/* isl naming */  
    char* chainJ;		/* java cleaned */  
    char* packagePrefix0; 	/* isl naming */
    char* packagePrefixJ; 	/* java cleaned */
    list children;
    list stuffToLoad;
    list customMappings;
    list typeSpecs;
    boolean isIdlInterface;	/* idl guarantees name either ifc or module */ 
    list specialIdlConstants;	
    struct context_s* up;
} ContextInternals;

typedef struct contextSpecifics_s {
    char* stubSuffix;	/* suffix to designate stub class */
    char* refSuffix;	/* suffix to designate reference interfaces */
    char* holderSuffix;	/* suffix to designate sun style holder classes */
    char* helperSuffix;	/* suffix to designate visigenix style helper classes */
    char* operationsSuffix; /* suffix to designate separate operations class */
    char* delClassSuffix;	/* suffix to designate delegation class */
    char* implSuffix;	/* suffix for an (optional) impl class */
    char* implPrefix;	/* prefix for an (optional) impl class */
    char* exceptSuffix;	/* suffix to designate exceptions */
    char* loaderClass_Name; /* classname for extra class loading stuff */	      
    char* dirName;	/* name of directory for root of generated stubs */
    char* prefix; 	/* raw name of java prefix-package */
    char* prerem; 	/* opposite of prefix */
    char* extraSuffix; 	/* suffix to generate extra handler files */
    char* methodNameSeparator; 	
    boolean genOmgAttr;	/* overload attributes methods the omg way */
    boolean flatDir;
    boolean genHlp;	/* use visigenix style helper class. */
    boolean genHldCls;	/* use named holder classes */
    boolean noServer;	/* prevent generation of server side */
    boolean genPortability; /* generate operations, POA_X_tie class etc */
    boolean genDel;	/* generate delegation class */
    boolean genImpl;	/* generate delegation class */
    boolean methodNamesWithInterface;	
    boolean methodNamesWithPackage;	
    boolean genFullMethodNames;	/* include interface name into method nmame */
    boolean genVisiVar;	/* generate old visigenic style var class */
    boolean genFactory;	/* generate an object factory */
    boolean forbidden;	/* don't generate stubs */
    boolean extraFiles;	/* generate extra files to make ilu types stand alone */
    boolean iJavaRemote;
    boolean iNoIluObject;
} ContextSpecifics;


typedef struct typeSpec_s {
    /* ********
     * The typeSpec_s can be allocated and filled
     * before the "Type" is known
     *
     */
    char* iluTypeName; /* identifies the type */
    /* ********
     * Client data made up for whatever reasons 
     * All client data is initialized to 0
     */
    boolean javaRemote;
    boolean noIluObject;
} * TypeSpec;


typedef struct context_s {
    ContextSpecifics p;	/* specied to customize */
    ContextInternals i;	/* computed */
} * IHandle;

extern IHandle getChildContext(IHandle parent, char* step, boolean create);

extern IHandle getContextC(char* name);
    /* get or create context; name dot separated chain of "module" names */ 
extern IHandle getContext(Interface ifc, list scoping);
    /* get or create context */ 
extern IHandle getContextT(Type t);
    /* get or create context for "module" where t is declared */
extern IHandle getContextTraw(Type t);
    /* get or create context for "module" where t is declared; not urtyped */
extern IHandle getContextRoot();
    /* get or create context for global default */

/* The current context describes the "module" of the open file */ 
extern IHandle currentIH;
extern void setCurrentIH(IHandle ih);

/* The current Interface from the context of the open file */
extern Interface currentIfc;
extern void setCurrentIfc(Interface ifc);

/* Final package prefix from concatenation of prefix and context chain */
extern char* packagePrefix0(IHandle ih); /*isl name */
extern char* packagePrefixMinus1(IHandle ih);
extern char* packagePrefixJ(IHandle ih);


extern TypeSpec getTypeSpecT0(Type t);
    /* Returns TypeSpec for Type if it exists; 0 otherwise */

extern TypeSpec getTypeSpec(IHandle ih, char* iluTypeName);
    /* Returns TypeSpec for named type; create it if it doesn't yet exist */


#endif /* _ILUJAVA_context_ */
/* end */
