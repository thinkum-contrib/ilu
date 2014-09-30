/* genseq.h (definitions for genseq.c) */
/* Chris Jacobi, October 8, 1998 11:04 pm PDT */

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
 
/* $Id: genseq.h,v 1.5 1999/08/03 01:51:17 janssen Exp $ */

#include "genstub.h"
     
extern boolean sq_is_a(Type t);
extern void sq_assert(Type t);

extern char * sq_typeDeclarator(Type t);

extern char * sq_ioSzPiece(Type t, const char *arg);
extern char * sq_ioOutPiece(Type t, const char *arg);
extern char * sq_ioInPiece(Type t);
extern void sq_enumElementTypes(Type t, TypeProc tp, refany rock);

extern void sq_defineMain(Type t);

extern boolean sq_isJString(Type t);


/* The following procedures are shared
 * between genarr.c and genseq.c,
 * but they are not really of general interest */

extern char *
ioSpecialElemSzPiece(const char *eName, const char *argName,
	                     const unsigned long length);
    /* Returns a marshaling sizing string piece for the special case of
     * one dimensional array of ISL BYTE, CHARACTER and SHORT CHARACTER.
     * Assumes arg designates the instance of T.
     */

extern char * 
ioSpecialElemOutPiece(const char *eName, const char *argName,
	                   const unsigned long length);

extern char * 
ioSpecialElemInPiece(const char *eName, const unsigned long length);
    


/* end */
