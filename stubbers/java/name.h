/* name.h */
/* Chris Jacobi, November 12, 1998 9:15 pm PST */

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
 
/* $Id: name.h,v 1.48 1999/08/03 01:51:05 janssen Exp $ */

#ifndef _ILUJAVA_name_
#define _ILUJAVA_name_

#include "context.h"


/* never get confused
 * 4 cases for name strings occur frequently
 * a) Syntactactily and semanticly in isl(idl) space 
 *    E.g. Used in strings passed to the runtime
 * b) Syntactactily and semanticly in Java space with
 *    E.g. Names of reference interfaces, packages and methods
 * c) Syntactactily in Java and semanticly in isl space
 *    E.g. Type cases for unions.
 * d) Syntactactily in Java and semanticly unfinished but prepared for java
 *    E.g. Suffixed classes before the suffix is added
 *
 * E.g  package prefix and suffix is a semantic addition in java space
 * E.g  Keyword avoidance is a syntactic feature for java space
 */


/*
 * contains code that sets and gets all types of names (type, exceptions etc.).
 */

extern char* javaizeIdent(char* s);
    /* returns single identifier */
    
extern char* javaizeIdentSeq(char* s);
    /* returns sequence of dot separated single identifier */

extern char* packageName(Interface i, list scoping);
    /* name of java package for isl interface or idl module */
    
extern char* packageNameOptional(Interface i, list scoping); 
    /* for all interfaces: package name or null string if local */
    
extern char* packageDotStringJ(IHandle ih, char* name); 
    /* prefixex the string with the package name in java space, if not local */


extern char* easyShortTypeNameCleaned(Type t);
/* Cleaned (syntax of a java id) original (short) 
 * name. Implemented for a subset of all types only.
 * Whether this has a semantic meaming in java for a type or not is only 
 * known by the implementation of that type.
 * Use when generating a type (known in right subset); don't use 
 * on components because suffixes etc are not added at this point.
 * This is NOT urtyped...
 * This is NOT the typename for instances !!!!!
 */

extern char* unresolvedIslTypeName(Type t);
/* Semanticly and syntacticly in isl(idl) space. 
 * Aliases are not resolved. 
 * Contains non-java characters, forbidden identifiers etc.
 */

extern char* typeNameUnresolvedButClean(Type t);
/* Semanticly in isl(idl) space. However syntacticly a java identifier
 * Aliases are not resolved; forbidden identifiers not resolved
 * Used for ilu isl extensions of unions
 */


/* Declarators are context dependent; usefull for type declarations
 * new statements etc.  Syntactically maybe not identifiers; e.g. arrays.
 */ 
extern char* typeDeclarator(Type t);
extern char* holderTypeDeclarator(Type t); 



/* 
 * Building block for method name.  Random users of method names
 * rather use the real name method out of genobj.c  (not yet exported)
 * (Mangles attribute names)
 */
extern char* methodNameBase(Procedure p); 

/* The following procedures return syntacticly javaized names */
extern char* argumentName(Argument a);
extern char* exceptionShortName(Exception e);
extern char* exceptionJName(Exception e);
extern char* constantNameRJ(Constant c); /* raw but syntacticly javaized */
extern char* enumFieldName(EnumField e);

extern void initName(); /*initialize the file name.c*/

#define idlExceptionPrefix "ilu--prefix-idlExceptionType-"
#define idlExceptionPrefixCleaned "ilu__prefix_idlExceptionType_"
#define idlExceptionSuffix "__omgidl_exctype"

#define idlAttributePrefixCleaned "ilu__prefix_idlAttribute__"
#define idlAttributeSetterPartCleaned "set_"
#define idlAttributeGetterPartCleaned "get_"

#define orgDotOmg "org.omg"
#define iluObject "org.omg.CORBA.Object"


#endif /* _ILUJAVA_name_ */
/* end */
