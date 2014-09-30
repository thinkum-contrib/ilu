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
/* IluJava_IluConnOps.c */
/* Chris Jacobi, January 7, 1999 3:08 pm PST */

/*
 */
 
/* $Id: IluJava_IluConnOps.c,v 1.42 1999/08/03 01:54:18 janssen Exp $ */
 
/* 
 * C side for IluSurrogateConnection.java, IluServerConnection.java
 */
  
#include "IluJava_Includes.h"
#include "IluJava_Common.h"
#include "IluJava_JTypes.h"
#include "IluJava_JGC.h"


#include "xerox_ilu_IluSurrogateConnection.h"
#include "xerox_ilu_IluServerConnection.h"
#include "xerox_ilu_IluPort.h"
#include "xerox_ilu_IluCall.h"
#include "xerox_ilu_IluClassRep.h"
#include "xerox_ilu_IluMethodRep.h"
#ifdef RNI
#include "xerox_ilu_IluWPBase.h"
#endif


#define TRACE0 _ilujava_pctFlag > 0
#define TRACE1 _ilujava_pctFlag > 1

static JGC_GLOBALOBJ_DECL(jsp_protoCFInstance);
    /* Some good instance from initialization because we failed 
     * calling static methods from native code.
     */


#define PORTWATCHER_LOOP 0
#define PORTWATCHER_FORK 1
#define PORTWATCHER_CLOSE 2
#define PORTWATCHER_DONE 3

JAVAEXPORT(IluPort_nativeWatchPort, Jint) 
    	JIluPort jh_Port
    	ENDJAVAEXPORT
{
    JGC_WP_TYPE(JIluPort) jwp_Port = JGC_GET_WP_FROM_JAVA_OBJECT(jh_Port);
    Jint	retVal = 0;
    ilu_boolean     ret;
    ilu_boolean     closed = ilu_FALSE;
    ILU_ERRS((IoErrs, bad_locks, broken_locks, no_resources, interrupted)) err;
    ilu_Connection  conn;
    ilu_Port cPort = GET_IluPort_yPort(JGC_WP_REVEAL(jwp_Port));
    JTRACE(TRACE0,
        ("$ IluConnOps_nativeWatchPort\n   jjPort: %x, cPort: %x \n", 
            (int)JGC_WP_REVEAL(jwp_Port), (int)cPort));
    ILU_CLER(err);
    JGC_ENABLE
    ret = ilu_WaitForPortConnectionRequest(cPort, &err);
    JGC_DISABLE
    if (ret==0) {
        PUT_IluPort_yConn(JGC_WP_REVEAL(jwp_Port), 0);
        ILU_ERR_SWITCH(err) {
            ILU_SUCCESS_CASE
	  	/* port is closed */
	  	JTRACE(TRACE0, ("$ .._nativeWatchPort port closed\n"));
                retVal = PORTWATCHER_DONE;
                ILU_CLER(err);
                goto done;
            ILU_ERR_CASE(interrupted, v)
	  	/* interrupted; retry */
	  	JTRACE(TRACE0, ("$ .._nativeWatchPort interrupted\n"));
                retVal = PORTWATCHER_LOOP;
                ILU_CLER(err);
                goto done;
            ILU_ERR_ELSE
	  	/* error; give up */
                JTRACE(TRACE0, ("$ .._nativeWatchPort ERR1\n"));
                retVal = PORTWATCHER_DONE;
                ILU_CLER(err);
                goto done;
        } ILU_ERR_ENDSWITCH;
    }
    ILU_CLER(err);
    JGC_ENABLE
    conn = ilu_HandleNewConnection(cPort, &closed, &err);
    JGC_DISABLE
    if (closed) {
        JTRACE(TRACE0, ("$ .._nativeWatchPort: closed \n"));
        PUT_IluPort_yConn(JGC_WP_REVEAL(jwp_Port), 0);
        retVal = PORTWATCHER_DONE;
        ILU_CLER(err);
        goto done;
    }
    if (conn==0) {
        ILU_ERR_SWITCH(err) {
            ILU_SUCCESS_CASE
	  	/* spurious wakeup; retry */
	  	JTRACE(TRACE0, ("$ .._nativeWatchPort sprs\n"));
                PUT_IluPort_yConn(JGC_WP_REVEAL(jwp_Port), 0);
                retVal = PORTWATCHER_LOOP;
                ILU_CLER(err);
                goto done;
            ILU_ERR_ELSE
	  	/* error; give up */
                JTRACE(TRACE0, ("$ .._nativeWatchPort ERR2\n"));
                if ((int)GET_IluPort_yConn(JGC_WP_REVEAL(jwp_Port)) == -1) {
                    /*prevent infinite loop*/
                    retVal = PORTWATCHER_DONE;
                    ILU_CLER(err);
                    goto done;
                }
                PUT_IluPort_yConn(
                    JGC_WP_REVEAL(jwp_Port), ((ilu_Connection)-1));
                retVal = PORTWATCHER_CLOSE;
                ILU_CLER(err);
                goto done;
        } ILU_ERR_ENDSWITCH;
    } else {
        JTRACE(TRACE0, ("$ .._nativeWatchPort: scss:<%p>\n", conn));
        PUT_IluPort_yConn(JGC_WP_REVEAL(jwp_Port), conn);
        /* Requirement: ilu_DoneServingConnection must called eventually.
         * Loop reading requests will do so.
         */
        retVal = PORTWATCHER_FORK;
        ILU_CLER(err);
        goto done;
    }
    done: 
        return retVal;
} /* nativeWatchPort */


JAVAEXPORT(IluSurrogateConnection_nativeOtherNewConnection, Jboolean) 
	JIluSurrogateConnection f_jjSurrConn
	ENDJAVAEXPORT
{
    ilu_Connection cConn; 
    ILU_ERRS((IoErrs, internal)) err = ILU_INIT_NO_ERR;
    struct {
       JIluSurrogateConnection jjSurrConn;
       } argCopy;
    JGC_FRAME_DECL(framename)  
    JGC_FRAME_PUSH(framename, &argCopy, sizeof(argCopy))
    argCopy.jjSurrConn = f_jjSurrConn;
    JTRACE(TRACE0, ("$ IluConnOps_nativeOtherNewConnection"));
    JGC_ENABLE
    cConn = ilu_OtherNewConnection(&err);
    JGC_DISABLE
    if (ILU_ERRNOK(err)) {
        _ilujava_IluErrorToException(&err, "OtherNewConnection");
    }
    PUT_IluSurrogateConnection_ySurrConn(argCopy.jjSurrConn, cConn);
    JGC_FRAME_POP(framename)
    return (cConn != 0);
} /* nativeOtherNewConnection */


JAVAEXPORT(IluSurrogateConnection_nativeNewConnGetterForked, void) 
	JIluSurrogateConnection jjSurrConn
	ENDJAVAEXPORT
{
    ILU_ERRS((IoErrs, internal)) err = ILU_INIT_NO_ERR;
    JTRACE(TRACE0, ("$ IluConnOps_nativeNewConnGetterForked"));
    JGC_GLOBALOBJ_ASSIGNTO(jsp_protoCFInstance, jjSurrConn);
    ilu_NewConnectionGetterForked(&err);
    ILU_MUST_BE_SUCCESS(err);
} /* nativeNewConnGetterForked */


JAVAEXPORT(
    IluSurrogateConnection_nativeOutgoingConnectionThreadProc, void) 
	JIluSurrogateConnection jjSurrConn
    ENDJAVAEXPORT
/* Called through forked Java when ilu_StartCall did require  
 * forking a connection reader handling one surrogate connection.
 * Uses "this.ySurrCon"
 */ 
{
    ILU_ERRS((IoErrs, internal)) err = ILU_INIT_NO_ERR;
    ilu_Connection cConn;
    cConn = GET_IluSurrogateConnection_ySurrConn(jjSurrConn);
    JTRACE(TRACE0, ("$ IluConnOps_NOCTP enter jjSurrConn %x cConn %x\n", 
            (int) jjSurrConn, cConn));
    JGC_ENABLE
    ilu_OutgoingConnectionThreadProc(cConn, &err);
    JGC_DISABLE
    JTRACE(TRACE0, ("$ IluConnOps_NOCTP exit jjSurrConn %x cConn %x\n", 
            (int) jjSurrConn, cConn));
    ILU_MUST_BE_SUCCESS(err);
} /* nativeOutgoingConnectionThreadProc */


JAVAEXPORT(IluServerConnection_receiveRequest, Jboolean)
    	JIluServerConnection jjIluServerConnection,
    	JIluCall jh_call
    	ENDJAVAEXPORT
/* servicing requests for a true servers connection */
{
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    ilu_boolean     initted, ret;
    ilu_Call cCall = GET_IluCall_yCall(JGC_WP_REVEAL(jwp_call));
    ilu_Class       cIluClass;
    ilu_Method      cIluMethod;
    ilu_RcvReqStat  stat;
    ilu_cardinal    SN;
    ILU_ERRS((bad_locks, IoErrs, bad_param, broken_locks, internal)) lerr;
    ilu_boolean     closed = ilu_FALSE;
    ilu_Connection cConn = GET_IluServerConnection_yConn(jjIluServerConnection);
    JTRACE(TRACE0, ("$ IluConnOps_receiveRequest cConn %p \n", cConn));
    if (GET_IluCall_needFinish(JGC_WP_REVEAL(jwp_call))) {
        PRINTRAISE("$ error with call");
        return 0; /* ? abandon connection */
    }
    PUT_IluCall_jjMethodRep(JGC_WP_REVEAL(jwp_call), 0);
    JGC_ENABLE
    ret = ilu_BlockingWaitForInputOnConnection(cConn, ILU_NIL);
    if (! ret) {
        /*connection should be abandoned*/
        JTRACE(TRACE0, ("$ IluConnOps_receiveRequest abandon conn\n"));
        ilu_DoneServingConnection(cConn, &lerr);
        JGC_DISABLE
        return 0;
    }
    JTRACE(TRACE1, ("$ IluConnOps_receiveRequest_1 \n"));
    stat = ilu_ReceiveRequest(cCall, &initted, cConn, &cIluClass, &cIluMethod,
			    &SN, &lerr);
    JGC_DISABLE
    if (stat == ilu_RcvReqStat_request) {
	JGC_WP_TYPE(JIluMethodRep) jwp_MethRep;
	JIluMethodRep jh_MethRep;
        /*initted is true*/
        JTRACE(TRACE1, ("$ IluConnOps_receiveRequest request \n"));
        PUT_IluCall_jMustFork(JGC_WP_REVEAL(jwp_call), 
            (Jboolean) ilu_ThreadPerRequest(cConn)
            );
        PUT_IluCall_jjClassRep(JGC_WP_REVEAL(jwp_call), 
            (JIluClassRep) _ilujava_findJavaClass(cIluClass)
            );
	jwp_MethRep = (JGC_WP_TYPE(JIluMethodRep)) ilu_GetMethodStubProc(cIluMethod, JAVALANGIDX);
	jh_MethRep  = (JIluMethodRep) JGC_WP_REVEAL(jwp_MethRep);
	PUT_IluCall_jjMethodRep(JGC_WP_REVEAL(jwp_call), jh_MethRep); 
        PUT_IluCall_jNeedsSizing(JGC_WP_REVEAL(jwp_call), 
            (Jboolean) ilu_CallNeedsSizing(cCall)
            );
        JTRACE(TRACE1, ("$ IluConnOps_receiveRequest got request\n"));
        PUT_IluCall_needFinish(JGC_WP_REVEAL(jwp_call), 1); 
                /* enforces ilu_FinishCall */
        goto Loop; 
    } else if (stat == ilu_RcvReqStat_quit) {
        JTRACE(TRACE1, ("$ IluConnOps_receiveRequest quit \n"));
        if (initted) ilu_FinishCall(cCall, &lerr);
        ilu_DoneServingConnection(cConn, &lerr);
        goto Exit;
    } else if (stat == ilu_RcvReqStat_noop) {
        JTRACE(TRACE1, ("$ IluConnOps_receiveRequest noop \n"));
        if (initted) ilu_FinishCall(cCall, &lerr);
        goto Loop;
    } else {
        JTRACE(TRACE1, ("$ IluConnOps_receiveRequest other \n"));
        if (initted) ilu_FinishCall(cCall, &lerr);
        goto Loop;
    }
    Loop:
        return 1; /*keep looping*/
    Exit:
        return 0; /*exit loop*/
} /* receiveRequest */


/* GC: calling into java, may internally re-enable gc */
EXPORTLIMITED void
_ilujava_forkConnectionHandler(JENV_FORMAL ilu_Connection newConnection)
{
    Jlong conn;
    if (newConnection) {
        JObject jj_proto;
        *(ilu_Connection*)&conn = newConnection;
        JGC_GLOBALOBJ_GETFROM(jj_proto, jsp_protoCFInstance);
        JCALL_IluSurrogateConnection_dynamicForkSC(jj_proto, conn);
    }
}


/* end */
