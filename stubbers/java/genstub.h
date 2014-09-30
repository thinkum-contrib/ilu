/* genstub.h (definition for genstub.c) */
/* Chris Jacobi, October 26, 1998 4:36 pm PST */
/* Last edited by Mike Spreitzer September 17, 1998 2:44 pm PDT */

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
 
/* $Id: genstub.h,v 1.22 1999/08/03 01:51:03 janssen Exp $ */

#ifndef _ILUJAVA_genstub_
#define _ILUJAVA_genstub_

#include "context.h"

extern void generateStub(Interface ifc, list localtypes);
extern void generateNestedConstants(IHandle ih, char* step);

typedef void (*TypeProc) (Type t, refany rock);

extern void enumElementTypes(Type t, TypeProc tp, refany rock);
/* Shallow enumeration of element types.
 * (element types are defined as "types necessary to make
 *  stubs work" [= classes which need to be loaded])   
 * May or may not have duplicates.
 * At todays date not yet implemented for all types.  
 */

#endif /* _ILUJAVA_genstub_ */
/* end */
