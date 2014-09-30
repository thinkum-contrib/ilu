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
/* IluJava_CallMacros.h */
/* Chris Jacobi, February 2, 1998 6:06 pm PST */

/*
 */
 
/* $Id: IluJava_CallMacros.h,v 1.15 1999/08/03 01:54:27 janssen Exp $ */

/*
 * Macros shared by files implementic IluCall procedures
 */

#ifndef _ILUJAVA_CallMacros_
#define _ILUJAVA_CallMacros_

/* for Microsoft Visual C turn off warning 
   "not enough actual parameters for macro 'ILUJAVA_EXPECT'" */
#ifdef _MSC_VER
#pragma warning( disable : 4003)
#endif


#include "IluJava_Includes.h"
#include "IluJava_Common.h"

#include <string.h>

#include "IluJava_JStubs.h"
#include "IluJava_JOps.h"
#include "IluJava_Ops.h"

#define ILUJAVA_VOID

/* Self checking codes to enforce legal transitions */
#define CALL_INACTIVE 0
#define CALL_FINISHED 0
    /* Not yet initialized, or, done for now.
     * The point is that these codes must not be encountered at random
     * times. 
     */
#define CALL_IN 1
    /* Input operations are expected. 
     */
#define CALL_SZ 2
    /* Sizing operations are expected. 
     */
#define CALL_OUT 3
    /* Output operations are expected. 
     */
#define CALL_INBETWEEN 4
    /* Done with writing or reading.
     * Controlled state change expected. 
     */
#define CALL_FINALIZED 99
    /* Marked to be finalized.  We set this state but NEVER want 
     * to encounter it. 
     */
     
#define EXPECT_UNFINISHED() \
    switch (GET_IluCall_selfCheck(JGC_WP_REVEAL(jwp_call))) {\
        case CALL_SZ:\
        case CALL_OUT:\
        case CALL_IN:\
        case CALL_INBETWEEN:\
            break;\
        default:\
            ilu_DebugPrintf("$ IluCall: bad internal mode: %d\n",\
                GET_IluCall_selfCheck(JGC_WP_REVEAL(jwp_call)));\
	    _ilujava_SignalInconsistency("");\
            return;\
    } 
    
#define EXPECT_UNFINISHED_QUIET() \
    switch (GET_IluCall_selfCheck(JGC_WP_REVEAL(jwp_call))) {\
        case CALL_SZ:\
        case CALL_OUT:\
        case CALL_IN:\
        case CALL_INBETWEEN:\
            break;\
        default:\
            return;\
    } 
    
#define EXPECT_LIFE() \
    switch (GET_IluCall_selfCheck(JGC_WP_REVEAL(jwp_call))) { \
        case CALL_SZ: \
        case CALL_OUT: \
        case CALL_IN: \
            break; \
        default:  \
            ilu_DebugPrintf("$IluCall: not alive\n"); \
	    _ilujava_SignalInconsistency(""); \
            return;\
    }
        
#define EXPECT_SZ_OR_INBETWEEN(RETURNS) \
    switch (GET_IluCall_selfCheck(JGC_WP_REVEAL(jwp_call))) { \
        case CALL_SZ: \
        case CALL_INBETWEEN: \
            break; \
        default:  \
            ilu_DebugPrintf("$IluCall: wrong internal mode\n"); \
	    _ilujava_SignalInconsistency(""); \
            return RETURNS;\
    } 

#define ILUJAVA_EXPECT(XX, RETURNS) \
    if (GET_IluCall_selfCheck(JGC_WP_REVEAL(jwp_call)) != XX) { \
        ilu_DebugPrintf("$IluCall_EXPECT (should %d was %d) %d\n",\
            XX, GET_IluCall_selfCheck(JGC_WP_REVEAL(jwp_call)), __LINE__); \
	_ilujava_SignalInconsistency("");  \
        return RETURNS; \
    }

#define TRANSITION(XX) PUT_IluCall_selfCheck(JGC_WP_REVEAL(jwp_call), XX);

#define PROLOG(XX, RETURNS) \
    ILU_ERRS((IoErrs, bad_param)) ioerrs = ILU_INIT_NO_ERR; \
    ilu_Call cCall = GET_IluCall_yCall(JGC_WP_REVEAL(jwp_call)); \
    ILUJAVA_EXPECT(XX, RETURNS)
    
#define PROLOG_SZ() \
    ilu_cardinal sz = 0; \
    PROLOG(CALL_SZ, 0)
    
#define ERRCHECK(RETURNS) \
    if (ILU_ERRNOK(ioerrs)) {\
        _ilujava_IluErrorToException(&ioerrs, "IluCall: ilu IO errors"); \
        return RETURNS; \
    }

#endif /* _ILUJAVA_CallMacros_ */

/* end */

