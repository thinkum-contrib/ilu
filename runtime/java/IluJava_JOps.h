/*
 BeginILUCopyright
 
 Copyright (c) 1991-1999 Xerox Corporation.  All Rights Reserved.
 
 Unlimited use, reproduction, modification, and distribution of this
 software and modified versions thereof is permitted.  Permission is
 granted to make derivative works from this software or a modified
 version thereof.  Any copy of this software, a modified version
 thereof, or a derivative work must include both the above copyright
 notice of Xerox Corporation and this paragraph.  Any distribution of
 this software, a modified version thereof, or a derivative work must
 comply with all applicable United States export control laws.  This
 software is made available AS IS, and XEROX CORPORATION DISCLAIMS ALL
 WARRANTIES, EXPRESS OR IMPLIED, INCLUDING WITHOUT LIMITATION THE
 IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 PURPOSE, AND NOTWITHSTANDING ANY OTHER PROVISION CONTAINED HEREIN, ANY
 LIABILITY FOR DAMAGES RESULTING FROM THE SOFTWARE OR ITS USE IS
 EXPRESSLY DISCLAIMED, WHETHER ARISING IN CONTRACT, TORT (INCLUDING
 NEGLIGENCE) OR STRICT LIABILITY, EVEN IF XEROX CORPORATION IS ADVISED
 OF THE POSSIBILITY OF SUCH DAMAGES.
 
 EndILUCopyright
*/
/* IluJava_JOps.h */
/* Chris Jacobi, January 8, 1998 3:51 pm PST */

/*
 */
 
/* $Id: IluJava_JOps.h,v 1.12 1999/08/03 01:54:31 janssen Exp $ */

/*
 * Handy java access operations.
 */

#ifndef _ILUJAVA_JOps_
#define _ILUJAVA_JOps_

#include "IluJava_Includes.h"
#include "IluJava_JTypes.h"


extern JString
IluJava_JString_fromA0(JENV_FORMAL char* data);
    /* Assumes data to be null terminated ascii string. 
     * Create java string
     */

extern JString
IluJava_JString_fromAX(JENV_FORMAL char* data, int leng);
    /* Assumes data to be ascii string. 
     * Create java string
     */


extern JArrayOfByte 
IluJava_JAoB_from8(JENV_FORMAL char * data, unsigned len); 
    /* Allocates and initializes a Java array of byte.
     * data: initial data interpreted as 8 bit bytes 
     * len: length of data (or array) in 8 bit units
     * Caller owns data. 
     */


extern JArrayOfChar 
IluJava_JAoC_from8(JENV_FORMAL char * data, unsigned len); 
    /* Allocates and initializes a Java array of char
     * data: initial data interpreted as 8 bit characters 
     * len: length of data in 8 bit units
     * Caller owns data 
     */


extern JArrayOfChar 
IluJava_JAoC_from16(JENV_FORMAL char * data, unsigned len); 
    /* Allocates and initializes a Java array of char.
     * data: initial data interpreted as 16 bit characters 
     * len: length of data in 16 bit units
     * Caller owns data 
     */


     
extern char * 
IluJava_JString_toheap8x(JENV_FORMAL JString js); 
    /* Takes java string; Convert to a heap allocated 
     * null terminated C string. 
     * Special case: Converts java null to C 0. 
     */

     
extern char * 
IluJava_JString_toheap80(JENV_FORMAL JString js); 
    /* Takes java string; ignores high bytes and copies low bytes
     * to a newly allocated null terminated C string (in the C heap).
     * Caller receives ownership of returned string. 
     * Special case: Converts java null to C 0. 
     */

     
extern void 
IluJava_JAoC_toC8(JENV_FORMAL JArrayOfChar ja, char * buffer, unsigned cnt); 
    /* Copies java array of char into a C string buffer.
     * Restricted to 8 bit units of the java chars. 
     * buffer: string buffer; must be long enough to receive 
     * all cnt chars plus terminating 0C char.
     * cnt: Number of 8 bit units to copy  (Must be >= 1 )
     * Caller owns buffer; 
     */

#endif /* _ILUJAVA_JOps_ */

/* end */

