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
/* IluJava_GCClient.c */
/* Chris Jacobi, December 29, 1997 5:28 pm PST */

/*
 */
 
/* $Id: IluJava_GCClient.c,v 1.16 1999/08/03 01:54:32 janssen Exp $ */
 
/*
 * C side for GCClient.java
 */
 
#include "IluJava_Includes.h"
#include "IluJava_Common.h"
#include "IluJava_JStubs.h"
#include "xerox_ilu_IluGCClient.h"


JAVAEXPORT(IluGCClient_nativeIsGcClientSet, Jboolean)
    JIluGCClient unused
    ENDJAVAEXPORT 
{
    long isSet = 0;
    if (ilu_IsGcClientSet()) isSet = 1; 
    return isSet;
}


JAVAEXPORT(IluGCClient_finishSpecialGCClass, void)
    JIluGCClient unused,
    JIluClassRep jh_iluClass
    ENDJAVAEXPORT 
{
    ilu_Class cClass = ilu_GetGcCallbackClass();
    PUT_IluClassRep_yIluClass(jh_iluClass, cClass);
    JTRACE(_ilujava_definitionsFlag>0,
        ("$ GCClient: jh_iluClass=<%x>, cClass=<%x>\n", jh_iluClass, cClass));
}


JAVAEXPORT(IluGCClient_setupSpecialGCObj, void)
    JIluGCClient unused,
    JIluOInt jh_oi
    ENDJAVAEXPORT 
{
    ilu_Object cGCObj = GET_IluOInt_yIluKernelObject(jh_oi);
    JTRACE(_ilujava_definitionsFlag>0, ("$ GCClient: cGCObj =<%x>\n", cGCObj));
    ilu_SetGcClient(cGCObj);
}

/* end */
