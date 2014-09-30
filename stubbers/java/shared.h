/* shared.h */
/* Chris Jacobi, October 8, 1998 11:06 pm PDT */

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
 
 
/* $Id: shared.h,v 1.5 1999/08/03 01:51:08 janssen Exp $ */


/* 
 * Collection of declarations which might as well
 * be used by stubbers for other languages. 
 */



#define PUBLIC
/* Use as documentation in front of a procedure to indicate it is 
 * "defined" here but also exported via a .h file
 */
 
#define PRIVATE static
/* Use as documentation in front of a procedure to indicate it is 
 * not used outside the defining file
 */

#define FORWARD
/* Use as documentation in front of a procedure to indicate it 
 * will be defined later in the same file
 */

#define FORWARDED
/* Use as documentation in front of a procedure to indicate it 
 * had been FORWARD declared in the same file.
 * Warning: this makes the procedure non-static
 */



/* The following definitions are only better names for 
 * enumeration values defined in iluptype.h::PrimitiveTypes
 */
#define byte8_Type byte_Type
#define char8_Type shortcharacter_Type
#define char16_Type character_Type
#define int16_Type shortinteger_Type
#define int32_Type integer_Type
#define int64_Type longinteger_Type
#define card16_Type shortcardinal_Type
#define card32_Type cardinal_Type
#define card64_Type longcardinal_Type
#define real32_Type shortreal_Type
#define real64_Type real_Type
#define real128_Type longreal_Type


/*end*/
