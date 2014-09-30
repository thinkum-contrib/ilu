/* stubops.c  (implements stubops.h) */
/* Chris Jacobi, January 4, 1999 11:49 am PST */

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
 
/* $Id: stubops.c,v 1.21 1999/08/03 01:51:08 janssen Exp $ */

#include <stdio.h>
#include <string.h>
#include <ctype.h> /* for tolower */
#include <stdlib.h>
#include <stdarg.h>
#include "iluptype.h"
#include "shared.h"


PUBLIC char * copy(const char *s)
{
    if (s==0) return 0;
    return ilu_strdup( (char *) s);
}


PUBLIC char *
cat2(const char * prefix, const char * postfix)
/* Concatenates two strings.
 */
{
    string buff;
    int len;
    if (prefix==0) return (char *) copy(postfix);
    if (postfix==0) return (char *) copy(prefix);
    len = strlen(prefix) + strlen(postfix) + 1; /*add terminating 0 */
    buff = iluparser_Malloc(len);
    buff[0] = 0;
    sprintf(buff, "%s%s", prefix, postfix);
    return buff;
}


PUBLIC char *
cat3(const char * prefix, const char * middle, const char * postfix)
/* Concatenates three strings.
 */
{
    string buff;
    int len;
    if (prefix==0) return cat2(middle, postfix);
    if (postfix==0) return cat2(prefix, middle);
    if (middle==0) return cat2(prefix, postfix);
    len = strlen(prefix) + strlen(middle) + strlen(postfix) + 1; /*add terminating 0 */
    buff = iluparser_Malloc(len);
    buff[0] = 0;
    sprintf(buff, "%s%s%s", prefix, middle, postfix);
    return buff;
}


PUBLIC char *
cat4(const char * a, const char * b, const char * c, const char * d)
/* Concatenates four strings.
 */
{
    char * temp = cat3(b, c, d);
    char * buf = cat2(a, temp);
    iluparser_Free(temp);
    return buf;
}


PUBLIC char *
cat5(const char * a, const char * b, const char * c, const char * d, const char * e)
/* Concatenates five strings.
 */
{
    char * temp = cat3(c, d, e);
    char * buf = cat3(a, b, temp);
    iluparser_Free(temp);
    return buf;
}

PUBLIC char *
cat6(const char * a, const char * b, const char * c, const char * d, const char * e, const char * f)
/* Concatenates five strings.
 */
{
    char * temp = cat4(c, d, e, f);
    char * buf = cat3(a, b, temp);
    iluparser_Free(temp);
    return buf;
}




PUBLIC const char *
dotCat(const char * prefix, const char * postfix)
/* builds "prefix.postfix" with a separating dot if both parts non null
 */  
{
    string buff;
    int len;
    if (prefix==0) return (char *) copy(postfix);
    if (postfix==0) return (char *) copy(prefix);
    len = strlen(prefix) + strlen(postfix) + 2; /*add dot and terminating 0 */
    buff = iluparser_Malloc(len);
    buff[0] = 0;
    sprintf(buff, "%s.%s", prefix, postfix);
    return buff;
} /* dotCat */


PUBLIC char *
parentize(const char * x)
/* Returns argument within parens "(x)".
 */  
 {
     return cat3( "(", x, ")" );
 } /* parentize */


PUBLIC int stringlength(const char* s)
/* like strlen, except supports 0 string. */
{
    if (s) {
        return strlen(s);
    } else {
        return 0;
    }
} /* stringlength */


PUBLIC boolean
isPrefixOf(const char *prefix, const char *base)
    /* case independent */
{
    int i;
    int len = stringlength(prefix);
    for (i = 0; i<len; i++) {
	if (tolower(prefix[i]) != tolower(base[i])) return FALSE;
    }
    return TRUE;
} /* isPrefixOf */


PUBLIC boolean
isPostfixOf(const char *postfix, const char *base)
    /* case independent */
{
    int i;
    int postfixLength = stringlength(postfix);
    int offset = stringlength(base)-postfixLength;
    if (postfixLength==0) return TRUE;
    if (offset<0) return FALSE;
    for (i = 0; i<postfixLength; i++) {
	if (tolower(postfix[i]) != tolower(base[offset+i])) return FALSE;
    }
    return TRUE;
} /* isPostfixOf */


PUBLIC boolean
matchPointer(void *p1, void *p2)
{
    return p1 == p2;
} /* matchPointer */


PUBLIC boolean
matchString(char *s1, char *s2)
{
    return (strcmp(s1, s2) == 0);
} /* matchString */


PUBLIC boolean list_insert_onceonly(list* lst, refany x)
{
    if (*lst == 0) {
	*lst = new_list();
    }
    if (list_find(*lst, matchPointer, x) != 0) {
	return 0; /* already inserted */
    }
    list_insert(*lst, x);
    return 1;
} /* list_insert_onceonly */


PRIVATE void crash() 
/* crash really horribly so dbx will enable debugging */
{
    int dontcare;
    int * badpointer = 0;
    for (;;) {
           dontcare = *badpointer;
    }
} /* crash */


PUBLIC boolean debugFlag = FALSE;

PUBLIC void
fatal(const char *fmt,...)
/* Report a fatal stubber internal error and abort.
 */  
{
    va_list args;
    va_start(args, fmt);
    fprintf(stderr, "Fatal error: ");
    vfprintf(stderr, fmt, args);
    va_end(args);
    putc('\n', stderr);
    crash();
    exit(1);
}

PUBLIC void
fatalError(const char *fmt,...)
/* Report a fatal user error and abort.
 */  
{
    va_list args;
    va_start(args, fmt);
    fprintf(stderr, "Fatal error: ");
    vfprintf(stderr, fmt, args);
    va_end(args);
    putc('\n', stderr);
    if (debugFlag) crash();
    exit(1);
}


/* end stubops.c */
