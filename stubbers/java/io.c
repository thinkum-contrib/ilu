/* io.c (implementation for io.h) */
/* Chris Jacobi, October 8, 1998 11:10 pm PDT */

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
 
/* $Id: io.c,v 1.38 1999/08/03 01:51:03 janssen Exp $ */

 
#include <stdio.h>
#include <string.h>
#include "iluptype.h"
#include "shared.h"
#include "stubops.h"
#include "util.h"
#include "name.h"
#include "io.h"
#include "context.h"
#include "genopt.h"
#include "genarr.h"
#include "genrecord.h"
#include "genseq.h"
#include "genunion.h"
#include "genobj.h"
#include "genenum.h"
#include "genpick.h"
#include "gencust.h"


PRIVATE char *
simpleTypeNamePiece(TypeKind tk)
/* Returns name part used in the marshalling routines
 * if tk is sufficiently simple,
 * 0 otherwise
 */
{
    switch (tk) {
	case byte8_Type:
	    return "Byte";
	case boolean_Type:
	    return "Bool";
	case char16_Type:
	    return "Char16";
	case char8_Type:
	    return "Char8";
	case int16_Type:
	    return "Int16";
	case int32_Type:
	    return "Int32";
	case int64_Type:
	    return "Int64";
	case card16_Type:
	    return "Card16";
	case card32_Type:
	    return "Card32";
	case card64_Type:
	    return "Card64";
	case real64_Type:
	    return "Real64";
	case real32_Type:
	    return "Real32";
	case real128_Type:
	    return "Real128";
	case object_Type:
	case union_Type:
	case sequence_Type:
	case record_Type:
	case array_Type:
	case enumeration_Type:
	    return 0;
	default: 
	    return 0;
    }
}


PUBLIC char * 
ioSzPiece(Type t, const char *arg)
    /* Returns a string piece for the szForT call.
     * Assumes name of call is "_call" and arg designates the instance of T
     */
{
    char * piece;
    TypeKind tk1 = type_ur_kind(t); 
    if (custom_is_a(t)) {
        return custom_ioSzPiece(t, arg);
    }
    switch (tk1) {
        case array_Type:
            return ar_ioSzPiece(t, arg);
        case enumeration_Type:
            return enm_ioSzPiece(t, arg);
        case object_Type:
            return obj_ioSzPiece(t, arg);
        case record_Type:
            return rec_ioSzPiece(t, arg);
        case sequence_Type:
            return sq_ioSzPiece(t, arg);
        case optional_Type:
            return opt_ioSzPiece(t, arg);
        case union_Type:
            return un_ioSzPiece(t, arg);
        case pickle_Type:
            return pick_ioSzPiece(arg);
        default:
            break;
    }
    piece = simpleTypeNamePiece(tk1);
    if (piece) {
        return cat3("_call.sz", piece, parentize(arg));
    }
    return "??_call.sz_NOTIMPL_x";
}
    
        
PUBLIC char * 
ioOutPiece(Type t, const char *arg)
    /* Returns a string piece for the szForT call.
     * Assumes name of call is "_call" and arg designates the instance of T
     */
{
    char * piece;
    TypeKind tk1 = type_ur_kind(t);
    if (custom_is_a(t)) {
        return custom_ioOutPiece(t, arg);
    }
    switch (tk1) {
        case array_Type:
            return ar_ioOutPiece(t, arg);
        case enumeration_Type:
            return enm_ioOutPiece(t, arg);
        case object_Type:
            return obj_ioOutPiece(t, arg);
        case record_Type:
            return rec_ioOutPiece(t, arg);
        case sequence_Type:
            return sq_ioOutPiece(t, arg);
        case optional_Type:
            return opt_ioOutPiece(t, arg);
        case union_Type:
            return un_ioOutPiece(t, arg);
        case pickle_Type:
            return pick_ioOutPiece(arg);
        default:
            break;
    }
    piece = simpleTypeNamePiece(tk1);
    if (piece) {
        return cat3("_call.out", piece, parentize(arg));
    }
    return "??_call.out_NOTIMPL_x";
}


PUBLIC char * 
ioInPiece(Type t)
    /* Returns a string piece for the inT call.
     * Assumes name of call is "_call" 
     */
{
    char * piece;
    TypeKind tk1 = type_ur_kind(t);
    if (custom_is_a(t)) {
        return custom_ioInPiece(t);
    }
    switch (tk1) {
        case array_Type:
            return ar_ioInPiece(t);
        case enumeration_Type:
            return enm_ioInPiece(t);
        case object_Type:
            return obj_ioInPiece(t);
        case record_Type:
            return rec_ioInPiece(t);
        case sequence_Type:
            return sq_ioInPiece(t);
        case union_Type:
            return un_ioInPiece(t);
        case optional_Type:
            return opt_ioInPiece(t);
        case pickle_Type:
            return pick_ioInPiece();
        default:
            break;
    }
    piece = simpleTypeNamePiece(tk1);
    if (piece) {
        return cat3("_call.in", piece, "()");
    }
    return "??_call.in_NOTIMPL_x";
}

/* end */
