/* stubops.h */
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
 
/* $Id: stubops.h,v 1.20 1999/08/03 01:51:09 janssen Exp $ */
/* see stubops.c for implementation */

/* 
 * Collection of procedures which might as well
 * be used by stubbers for other languages. 
 */
 
 
/* 
 * String handling.
 * For all string handling routines: Caller owns returned string.
 * Routines may return 0 instead of an empty string.
 */

extern char *
copy(const char * s);
/* Returns copy of string. */

extern char *
cat2(const char * prefix, const char * postfix);
/* Concatenates two strings. */

extern char *
cat3(const char * prefix, const char * middle, const char * postfix);
/* Concatenates three strings. */

extern char *
cat4(const char * a, const char * b, const char * c, const char * d);
/* Concatenates four strings. */

extern char *
cat5(const char * a, const char * b, const char * c, 
    const char * d, const char * e);
/* Concatenates five strings. */

extern char *
cat6(const char * a, const char * b, const char * c, 
    const char * d, const char * e, const char * f);
/* Concatenates six strings. */

extern int stringlength(const char* s);
/* like strlen, except supports 0 string. */

extern char *
dotCat(const char * prefix, const char * postfix);
/* Builds "prefix.postfix" with a separating dot if both parts non null. */  

extern char *
parentize(const char * x);
/* Returns argument within parens "(x)". */  

extern boolean 
isPrefixOf(const char *prefix, const char *base);
/* returns whether prefix is a prefix of base.
 * Ignore case (to avoid filename conflicts on Windows) 
 */

extern boolean 
isPostfixOf(const char *postfix, const char *base);
/* returns whether postfix is a postfix of base 
 * Ignore case (to avoid filename conflicts on Windows) 
 */


/*
 * list processing macros
 */

#define LOOP_BEGIN(list, elType, elName, temp) 	\
{                        	\
    listElement* temp;      	\
    if (list) {          	\
        for (temp = list->head;  temp != NULL;  temp = temp->next) {	\
            elType elName = ( elType ) temp->data;

#define LOOP_END()      	\
        }                   	\
    }                    	\
}
 
 
#define C_LOOP_BEGIN(list, elType, elName, counter, temp)       	\
{                        	\
    cardinal counter = 0;	\
    listElement* temp;    	\
    if (list) {         	\
        for (temp = list->head;  temp != NULL;  temp = temp->next) {	\
            elType elName = ( elType ) temp->data;

#define C_LOOP_END(counter) 	\
        counter++;          	\
        }                   	\
    }                   	\
}


extern boolean matchPointer(void *p1, void *p2);
/* This is a iluparser_FindProc for pointer-EQ */
  
extern boolean matchString(char *s1, char *s2);
/* This is a iluparser_FindProc for string equalness (cap-matters) */
  

extern boolean list_insert_onceonly(list* lst, refany x);
/* Adds x to list if it is not already included.
 * Returns whether x has been added
 * Creates list if necessary
 */



/*
 * Stubber self checkking
 */

extern boolean debugFlag;
/* if this is TRUE, fatalError will cause entering the debugger */

extern void
fatal(const char *fmt,...);
/* Report a fatal stubber internal error and abort process. */  

extern void
fatalError(const char *fmt,...);
/* Report a fatal user error and abort process. */  


/* end stubops.h */
