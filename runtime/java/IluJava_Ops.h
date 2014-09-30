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
/* IluJava_Ops.h */
/* Chris Jacobi, December 31, 1997 6:41 pm PST */

/*
 */
 
/* $Id: IluJava_Ops.h,v 1.6 1999/08/03 01:54:19 janssen Exp $ */

#ifndef _ILUJAVA_OPS_
#define _ILUJAVA_OPS_

#include "IluJava_JTypes.h"

/* ================ Server ================ */


extern void _ilujava_IluServerInit();
/* Initialization for IluJava_IluServer.c */


extern ilu_Server
_ilujava_EnterServer(JENV_FORMAL JIluOInt jh_oi, ilu_Class cActualClass);
/* if returns 0: no lock entered
 * if returns ilu_Server: server lock entered
 */

extern ilu_Server
_ilujava_EnterServer2(JENV_FORMAL JIluServer jh_IluServer, ilu_Class cActualClass);
/* if returns 0: no lock entered
 * if returns ilu_Server: server lock entered
 */

extern ilu_Server
_ilujava_EnterServerDisable(JENV_FORMAL JIluOInt jh_oi, ilu_Class cActualClass);
/* Enters the server lock but disables further entries
 * if returns 0: no lock entered
 * if returns ilu_Server: server lock entered and disabled
 */

/* enters and exits the server lock */
extern ilu_Server
_ilujava_ServerEnterHolds(JENV_FORMAL JIluOInt jh_oi);
/* increases holds on server with appropriate locks.
 * if returns 0: no holds changed
 * if returns ilu_Server: server enter+exit and holds changed
 */

/* enters and exits the server lock */
extern void
_ilujava_ServerExitHolds(JENV_FORMAL ilu_Server cServer);
/* decreases holds on server with appropriate locks.
 */


/* ================ locking server ================ */

typedef struct _KInfo {
    ilu_Object cIluObject;
    ilu_Server cServer;
    ilu_Class cClass;
} KInfo;

extern ilu_Object 
_ilujava_getSetCIluObject(JENV_FORMAL 
    JIluOInt jh_oi, KInfo* kinfo, ilu_boolean raiseErrs
    );
/* Assigns values to kinfo* and returns kernel object from an IluOInt
 *
 * before: not Inside (kinfo->cServer, kinfo->cClass)
 * after:  return != ILU_NIL => Inside(kinfo->cServer, kinfo->cClass)
 * after:  return == ILU_NIL => not Inside (kinfo->cServer, kinfo->cClass)
 * gc: java gc may move objects
 */


#endif /* _ILUJAVA_OPS_ */

/* end */

