/* genopt.h (definitions for genopt.c) */
/* Chris Jacobi, October 8, 1998 11:03 pm PDT */

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
 
/* $Id: genopt.h,v 1.6 1999/08/03 01:51:17 janssen Exp $ */

#include "genstub.h"
     
extern boolean opt_is_a(Type t);
extern void opt_assert(Type t);

extern char * opt_typeDeclarator(Type t);

extern char * opt_ioSzPiece(Type t, const char *arg);
extern char * opt_ioOutPiece(Type t, const char *arg);
extern char * opt_ioInPiece(Type t);
extern void opt_enumElementTypes(Type t, TypeProc tp, refany rock);


extern void opt_defineMain(Type t);

/* the following routines are provided for unions and typedcodes... */

extern char* wrapperTypeDeclarator(Type t);
    /* returns either the typeDeclarator or a wrapper if not object typed */

extern char* fromObject(Type t, char* name);
    /* returns either the argument unchanged, or accesses a wrapper */

extern char* toObject(Type t, char* name);
    /* returns either the argument unchanged, or allocates a wrapper */ 

/* end */
