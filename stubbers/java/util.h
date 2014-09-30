/* util.h */
/* Chris Jacobi, January 4, 1999 11:47 am PST */

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
 
/* $Id: util.h,v 1.44 1999/08/03 01:51:06 janssen Exp $ */

#ifndef _ILUJAVA_util_
#define _ILUJAVA_util_

#include "context.h"

typedef iluparser_EnumProc	EnumProc;

extern char * programName;
extern boolean generatingSkeleton;

extern char * booleanImage(boolean value);
extern TypeDescription	baseTypeDescription(Type t);

extern char * typeInitializer(TypeKind tk);
/* returns a string containing the initializer for a type */ 
    
extern void NewJavaFile(Interface interface, IHandle ih, const char * className);
/* Creates a new file. redirects stdout to the file */

extern void LoadJavaClass(const char * className);
/* Adds this class to list of classes to be loaded into the current IH */

extern void printConst(ConstantValue v, Type t);
    /* prints the constant; t is its type (not v->type) */


extern void printSpecialConst(ConstantValue v, Type t);
    /* prints the constant in the format used by the
     * type registrations of unions.  YUK.
     */


extern Type myUrType(Type t);
    /* eats usage and aliases and leaves a definition */

extern void printLoopIndent(const unsigned long level);

extern void printOpenDocComment(const char* indent);
    /* prints the beginning of a doc comment.  
     * indent is template for indentation
     */
     
extern void printCloseComment();
    /* prints the closing of a comment.  
     * (use indent from opening)
     */
     
extern void printCommentLines(const char* s);
    /* prints center part of a comment 
     * (use indent from opening)
     */

extern void printDocString(const char* indent, const char* s);
    /* prints a comment with a doc string s; 
     * indent is template for indentation
     */
     
extern char *
formatLongUnsigned(const unsigned long l);
/* Returns argument as a string. */  

extern char *
cleanString(const char *s);
/* makes string printable. */  

extern char *
quoteString(const char *s);
/* makes string printable and quote it. */  

extern char *
qoString(const char *s);
/* like quoteString: quotes and makes string printable
 * except: returns "null" if s==NIL. 
 */  

extern char *
setDimension(const char *declarator, const char *idx);
/* Assumes declarator is an array declaration with one
 * dimension unspecified, written like ...[].  Returns  
 * a copy with idx filled into that dimension. 
 */

extern char*
comma0(list l);
/* Returns a comma if l not empty; an empty string otherwise. */


extern char* corbaTCKind(TypeKind tk);
/* Returns corba TCKind
 * 0 on failure 
 */
 
void ReportFiles(const char* name);
/* Creates file naming all other generated files */

extern char*
  KernelTypeKindName (Type type);
/* returns kernel type identifier for the Type */

extern void printLoadClasses(IHandle ih, boolean alreadyInStatic);

extern char* stubConsistencyCheck();
/* Returns string which invokes a stub consistency check */

#endif /* _ILUJAVA_util_ */
/* end */
