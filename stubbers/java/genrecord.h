/* genrecord.h */
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
 
/* $Id: genrecord.h,v 1.9 1999/08/03 01:51:05 janssen Exp $ */

#include "genstub.h"

extern boolean rec_is_a(Type t);
extern Type rec_assert(Type t);

extern char * rec_typeDeclarator(Type t);

extern char * rec_ioSzPiece(Type t, const char *arg);
extern char * rec_ioOutPiece(Type t, const char *arg);
extern char * rec_ioInPiece(Type t);

extern void rec_enumElementTypes(Type t, TypeProc tp, refany rock);
extern void rec_helpInnerCallback(Type t);

extern void rec_defineMain(Type t);

/* for the benefit of idl exceptions which can be records */
extern void printRecordFieldDecls(Type t, boolean initializers);
extern void printRecordConstructorFormalArgs(Type t);
extern void printRecordAssignCorresponding(Type t, 
    char* leftPrefix, char* rightPrefix
    );

/* end */
