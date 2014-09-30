/* genpick.c */
/* Chris Jacobi, October 8, 1998 11:09 pm PDT */

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

/* $Id: genpick.c,v 1.5 1999/08/03 01:51:12 janssen Exp $ */


#include <stdio.h>
#include <string.h>
#include "iluptype.h"
#include "shared.h"
#include "stubops.h"
#include "io.h"
#include "context.h"
#include "name.h"
#include "util.h" 
#include "genstub.h"


PUBLIC boolean pick_is_a(Type t) {
    t = myUrType(t);
    return (type_kind(t)==pickle_Type);
}


PUBLIC Type pick_assert(Type t) {
    t = myUrType(t);
    if (! pick_is_a(t)) fatal("should have been a pickle");
    return t;
}


PUBLIC char * pick_typeDeclarator() {
    return copy("xerox.ilu.IluAny");
}


PUBLIC char * pick_ioSzPiece(const char *arg) {
    return cat2(arg, "._szAny(_call)");
}


PUBLIC char * pick_ioOutPiece(const char *arg) {
    return cat2(arg, "._outAny(_call)");
}


PUBLIC char * pick_ioInPiece() {
    return copy("xerox.ilu.IluAny._inAny(_call)");
}


/* end */
