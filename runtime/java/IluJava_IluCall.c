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
/* IluJava_IluCall.c */ 
/* Chris Jacobi, November 25, 1998 5:39 pm PST */

/*
 */
 
/* $Id: IluJava_IluCall.c,v 1.103 1999/08/03 01:55:19 janssen Exp $ */

/*
 * C side for IluCall.java
 */


#include "IluJava_Includes.h"
#include "IluJava_JGC.h"
#include "IluJava_CallMacros.h"
#include "IluJava_JMon.h"
#include "IluJava_JArrays.h"
#include "IluJava_Common.h"
#include "IluJava_JStubs.h"

#include "xerox_ilu_IluPassport.h"
#include "xerox_ilu_IluPipeline.h"
#include "xerox_ilu_IluSerializationContext.h"
#include "xerox_ilu_IluBatcher.h"


static int nullBuffer = 0;  /* "placeholder" where empty strings can point */


/* The CALLOCK is s static global lock.
 * Never enter any other locks while CALLOCK is held.
 */
static ILUJAVA_JMON_PTR callLock = 0;
#define ENTER_CALL_LOCK() ILUJAVA_MON_ENTER(callLock)
#define EXIT_CALL_LOCK() ILUJAVA_MON_EXIT(callLock)

static void InitCallLock(JENV_FORMAL_NOCOMMA)
{
    callLock = ILUJAVA_MON_ALLOC();       
}



/*
 * If only correct stubs were used, jh_call->needFinish
 * wouldn't need to be locked. Correct stubs would guarantee
 * sequential use and finish call before finalization.
 *
 * However, there are bad stubs, and programming errors which
 * are just not worth spending all the time thinking about. 
 */
 
static void SetNeedFinish(JENV_FORMAL JGC_WP_TYPE(JIluCall) jwp_call)
{
   ENTER_CALL_LOCK();
   PUT_IluCall_needFinish(JGC_WP_REVEAL(jwp_call), 1);
   EXIT_CALL_LOCK();
} /*SetNeedFinish*/


static Jint GetClearNeedFinish(JENV_FORMAL JGC_WP_TYPE(JIluCall) jwp_call)
/* Actual ilu_FinishCall is conditional and monitored because we 
 * don't really trust the stubs (in case of applets); (nor do we
 * really trust our own error handling to be really correct).
 */
{
   Jint nf;
   ENTER_CALL_LOCK();
   nf = GET_IluCall_needFinish( (JIluCall) JGC_WP_REVEAL(jwp_call));
   PUT_IluCall_needFinish( (JIluCall) JGC_WP_REVEAL(jwp_call), 0);
   EXIT_CALL_LOCK();
   return nf;
}  /*GetClearNeedFinish*/



/* called from static block to initialize the J-class */
JAVAEXPORT(IluCall_givePrototype, void) 
        JIluCall jh_call_unused
        ENDJAVAEXPORT
{
    if (callLock == 0) {
        InitCallLock(JENV_ACTUAL_NOCOMMA);
    }
}


JAVAEXPORT(IluCall_nativeFinalize, void) 
        JIluCall jh_call
        ENDJAVAEXPORT
/* Called by the java finalize procedure.
 * The need to monitor locking is weak, but there was too much
 * debugging action going on, so lets synchronize.  
 */
{
    ilu_Call cCall;
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    ENTER_CALL_LOCK();
    TRANSITION(CALL_FINALIZED);
    if (GET_IluCall_needFinish(JGC_WP_REVEAL(jwp_call))) {
        ilu_DebugPrintf("$ERROR: IluCall_nativeFinalize call not 'finished'\n");
    }
    cCall = GET_IluCall_yCall(JGC_WP_REVEAL(jwp_call));
    if (cCall) {
        PUT_IluCall_yCall(JGC_WP_REVEAL(jwp_call), 0);
        ilu_free(cCall);
    }
    EXIT_CALL_LOCK();
} /*IluCall_nativeFinalize*/


JAVAEXPORT(IluCall_initCallShell, void)
        JIluCall jh_call
        ENDJAVAEXPORT
    /* Native side for makeCallShell.
     * Trusts "jh_call" to be freshly allocated and therefore may 
     * avoid monitor locking.
     */
{
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    ilu_Call cCall = ilu_must_malloc(sizeof(struct _ilu_Call_s));
    TRANSITION(CALL_INACTIVE);
    PUT_IluCall_yCall(JGC_WP_REVEAL(jwp_call), cCall);
} /*IluCall_initCallShell*/


JAVAEXPORT(IluCall_nativeStartCall, void) 
        JIluCall jh_call,
        JIluMethodRep jh_methodRep,
        JIluOInt jh_discriminantOI,
        JIluPassport jh_passport,
        JIluPipeline jh_pipeline,
        JIluSerializationContext jh_serializer
        ENDJAVAEXPORT
/* (surrogate) stub initiating a call */
{
    ILU_ERRS((IoErrs)) errs = ILU_INIT_NO_ERR;
    ilu_Connection newConnection;
    ilu_boolean success;
    ilu_Call cCall;
    ilu_Server cServer;    	
    ilu_Pipeline cPipeline = 0;    	
    ilu_Serializer cSerializer = 0;    	
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    JGC_WP_TYPE(JIluClassRep) jwp_staticClass = 
        JGC_GET_WP_FROM_JAVA_OBJECT(
            GET_IluMethodRep_jjClassRep(jh_methodRep)
        ); 
        /* methods carry static class */
    JGC_WP_TYPE(JIluMethodRep) jwp_methodRep = 
        JGC_GET_WP_FROM_JAVA_OBJECT(jh_methodRep);
    JGC_WP_TYPE(JIluPassport) jwp_passport = 
        JGC_GET_WP_FROM_JAVA_OBJECT(jh_passport);
    JGC_WP_TYPE(JIluOInt) jwp_discriminantOI = 
        JGC_GET_WP_FROM_JAVA_OBJECT(jh_discriminantOI);

    if (jh_pipeline) {
        cPipeline = GET_IluPipeline_yIluPipeline(jh_pipeline);
    }
    if (jh_serializer) {
        cSerializer = GET_IluSerializationContext_ySerializationContext(jh_serializer);
    }
    cCall = GET_IluCall_yCall(jh_call);
    
    ILUJAVA_EXPECT(CALL_INACTIVE, ILUJAVA_VOID)
    /* PUT_IluCall_jDontGCcall1(jh_call, ((HObject*) jh_discriminantOI)); */
        /* Semi paranoya: we are also saving the internal object which has
         * a reference to the object itself; by as well saving it directly
         * we prevent gc even in case of nasty accident to internal object.
         */
    PUT_IluCall_jjMethodRep(
       JGC_WP_REVEAL(jwp_call), JGC_WP_REVEAL(jwp_methodRep));
    PUT_IluCall_jjClassRep(
        JGC_WP_REVEAL(jwp_call), JGC_WP_REVEAL(jwp_staticClass));
    PUT_IluCall_jMustFork(JGC_WP_REVEAL(jwp_call), 0);
    JTRACE(_ilujava_callsFlag>1,
        ("$ IluCall_StartCall j:%x c:%x\n", JGC_WP_REVEAL(jwp_call), cCall));
    cServer = _ilujava_ServerEnterHolds(JENV_ACTUAL
        (JIluOInt)JGC_WP_REVEAL(jwp_discriminantOI)
        );
    if (cServer == 0) {
        _ilujava_IluErrorToException(0, "ilu_StartCall without server");
        return;
    }
    success = ilu_FullStartCall(
    	cCall,
    	cServer, 
        GET_IluClassRep_yIluClass(JGC_WP_REVEAL(jwp_staticClass)),
        GET_IluMethodRep_yIluMethod(JGC_WP_REVEAL(jwp_methodRep)),
        JAVALANGIDX,
        _ilujava_getIluPassport(JENV_ACTUAL 
            (ilu_refany) JGC_WP_REVEAL(jwp_passport)), 
        cSerializer,
        cPipeline,
        &newConnection, 
        &errs
    	);
    _ilujava_ServerExitHolds(JENV_ACTUAL cServer);
    if (newConnection) {
        JTRACE(_ilujava_callsFlag>1,
            ("$ IluCall_StartCall: new connection %x\n", newConnection));
        PUT_IluCall_yCallSurrConn(JGC_WP_REVEAL(jwp_call), newConnection);
        PUT_IluCall_jMustFork(JGC_WP_REVEAL(jwp_call), 1);
    }
    if (success) {
        TRANSITION(CALL_SZ)
        SetNeedFinish(JENV_ACTUAL jwp_call);
        PUT_IluCall_jNeedsSizing(
            JGC_WP_REVEAL(jwp_call), (Jboolean) ilu_CallNeedsSizing(cCall));
        JTRACE(_ilujava_callsFlag>1,
            ("$ IluCall_StartCall success j:%x\n", JGC_WP_REVEAL(jwp_call)));
    } else {
        JTRACE(_ilujava_callsFlag>1,
            ("$ IluCall_StartCall failure j:%x\n", JGC_WP_REVEAL(jwp_call)));
        _ilujava_IluErrorToException(&errs, "failed ilu_StartCall");
    }
} /* nativeStartCall */


static void 
finishCall_DontRaiseErrors0(JENV_FORMAL JIluCall jh_call)
/* Like ..._IluCall_finishCall but for internal use in 
 * cases where errors are reported anyway and we don't
 * want to report more or different errors.
 *
 * Do the transition self checking
 */
{
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    Jint needF = GetClearNeedFinish(JENV_ACTUAL jwp_call);
    JTRACE(_ilujava_callsFlag>1, ("$ IluCall_finishCall_DRE0 j:%x, n:%d\n", 
            JGC_WP_REVEAL(jwp_call), needF));
    if (needF) {
        ILU_ERRS((bad_locks, IoErrs)) err = ILU_INIT_NO_ERR;
        ilu_Call cCall;
        EXPECT_UNFINISHED_QUIET()
        cCall = GET_IluCall_yCall(JGC_WP_REVEAL(jwp_call));
        if (cCall) {
            ilu_FinishCall(cCall, &err);
            /* ignore ilu errors  */
            ILU_HANDLED(err);
        }
        TRANSITION(CALL_FINISHED)
    }
} /*finishCall_DontRaiseErrors0*/


static void 
finishCall_DontRaiseErrors1(JENV_FORMAL JIluCall jh_call)
/* Like finishCall_DontRaiseErrors0 but doesn't deal with
 * self checking.
 */
{
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    Jint needF = GetClearNeedFinish(JENV_ACTUAL  jwp_call);
    JTRACE(_ilujava_callsFlag>1, ("$ IluCall_finishCall_DRE1 j:%x, n:%d\n", 
            JGC_WP_REVEAL(jwp_call), needF));
    if (needF) {
        ilu_Call cCall;
        ILU_ERRS((bad_locks, IoErrs)) err = ILU_INIT_NO_ERR;
        cCall = GET_IluCall_yCall(JGC_WP_REVEAL(jwp_call));
        if (cCall) {
            ilu_FinishCall(cCall, &err);
            /* ignore ilu errors  */
            ILU_HANDLED(err);
        }
    }
}



JAVAEXPORT(IluCall_nFinishCall, Jint) 
        JIluCall jh_call
        ENDJAVAEXPORT
{
    Jint retval = 0;
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    Jint needF = GetClearNeedFinish(JENV_ACTUAL jwp_call);
    JTRACE(_ilujava_callsFlag>1, ("$ IluCall_finishCall j:%x, n:%d\n", 
            JGC_WP_REVEAL(jwp_call), needF));
    /* don't check state as this is used in try-finally */
    if (needF) {
        ilu_Call cCall;
        ILU_ERRS((bad_locks, IoErrs)) err = ILU_INIT_NO_ERR;
        cCall = GET_IluCall_yCall(JGC_WP_REVEAL(jwp_call));
        if (cCall) {
            ilu_FinishCall(cCall, &err);
            if (ILU_ERRNOK(err)) {
                /* it would be nice to know if we are in the 
                 * try-finally because of an error or not
                 */
                _ilujava_IluErrorToException(&err, "finishCall"); 
            } else {
             retval = 1;
            }
        }
    }
    TRANSITION(CALL_FINISHED)
    return retval;
} /*IluCall_nFinishCall*/



JAVAEXPORT(IluCall_startWriteRequest, void)
        JIluCall jh_call, Jint sz
        ENDJAVAEXPORT
    /* used by surrogates */
{
    ilu_Call cCall;
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    ILU_ERRS((IoErrs)) err = ILU_INIT_NO_ERR;
    EXPECT_SZ_OR_INBETWEEN(ILUJAVA_VOID)
    cCall = GET_IluCall_yCall(JGC_WP_REVEAL(jwp_call));
    if (!ilu_StartRequest(cCall, (ilu_cardinal) sz, &err)) {
        /* error */
        finishCall_DontRaiseErrors0(JENV_ACTUAL
            (JIluCall)JGC_WP_REVEAL(jwp_call));
        _ilujava_IluErrorToException(&err, "startWriteRequest");
        return;
    }
    TRANSITION(CALL_OUT)
}


JAVAEXPORT(IluCall_nDoneWriteRequest,  void)
        JIluCall jh_call,
        JIluBatcher jh_batcher
        ENDJAVAEXPORT
    /* used by surrogates */
{
    ilu_Batcher cbatcher = 0;
    ILU_ERRS((IoErrs)) err = ILU_INIT_NO_ERR;
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    ilu_Call cCall = GET_IluCall_yCall(JGC_WP_REVEAL(jwp_call));
    if (jh_batcher) {
        cbatcher = GET_IluBatcher_yBatcher(jh_batcher);
    }
    ILUJAVA_EXPECT(CALL_OUT, ILUJAVA_VOID)
    if (!ilu_FullFinishRequest(cCall, cbatcher, &err)) {
        /* Error. 
         * Call is over; call into java anyway in case data structures
         * would need to be returned.
         */
        finishCall_DontRaiseErrors0(JENV_ACTUAL 
            (JIluCall)JGC_WP_REVEAL(jwp_call));
        _ilujava_IluErrorToException(&err, "nDoneWriteRequest");
        return;
    }
    TRANSITION(CALL_INBETWEEN)
    if (cCall->ca_method->me_asynchronous) {
        finishCall_DontRaiseErrors1(
            JENV_ACTUAL (JIluCall)JGC_WP_REVEAL(jwp_call));
    }
    return;
} /*IluCall_nDoneWriteRequest*/


#define JAVA_RETRY_CODE -9999
JAVAEXPORT(IluCall_startReadReply, Jint)
        JIluCall jh_call
        ENDJAVAEXPORT
    /* used by surrogates */
{
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    ilu_Connection newConnection = 0;
    ilu_cardinal errorStat;
    ilu_ProtocolException protoErr;
    ilu_Completion completion;
    ILU_ERRS((bad_locks, IoErrs, transient / retry)) err = ILU_INIT_NO_ERR;
    ilu_Call cCall = GET_IluCall_yCall(JGC_WP_REVEAL(jwp_call));
    ILUJAVA_EXPECT(CALL_INBETWEEN, 0)
    protoErr = ilu_FullGetReply(cCall, &errorStat, &completion, &newConnection, &err);
    if (newConnection) {
        _ilujava_forkConnectionHandler(JENV_ACTUAL newConnection);
        /* enables java gc */
    }
    JTRACE(_ilujava_callsFlag>1, 
        ("$ IluCall_startReadReply proto: %d user: %d\n", 
            (int) protoErr, (int) errorStat));
    if (protoErr == ilu_ProtocolException_Success) {
        TRANSITION(CALL_IN)
        return errorStat;
    } else {
        int asint = (int) protoErr;
        if (ILU_ERRNOK(err)) {
            if ((err.ilu_type == ILU_ERRTYP(transient)) &&
                    (ILU_ERRSEL(transient,err).minor == ilu_tm_retry)) {
                ILU_HANDLED(err);
                return JAVA_RETRY_CODE;
            }
        }
        finishCall_DontRaiseErrors0(JENV_ACTUAL 
            (JIluCall)JGC_WP_REVEAL(jwp_call));
        TRANSITION(CALL_FINISHED)
        if (protoErr == ilu_ProtocolException_Not) {
            _ilujava_IluErrorToException(&err, "ProtocolException_Not");
        } else {
            ILU_HANDLED(err);
        }
        switch (completion) {
             case ILU_COMPLETED_YES:
                 break;
             case ILU_COMPLETED_NO:
                 asint = asint + 100000;
                 break;
             default: /*ILU_COMPLETED_MAYBE*/
                 asint = asint + 200000;
                 break;
        }
        return (Jint) (- asint); /* now returns a negative number */
    }
} /*startReadReply*/



JAVAEXPORT(IluCall_doneReadReply, void)
        JIluCall jh_call
        ENDJAVAEXPORT
/* used by surrogates */
{
    ILU_ERRS((IoErrs)) err = ILU_INIT_NO_ERR;
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    ilu_Call cCall = GET_IluCall_yCall(JGC_WP_REVEAL(jwp_call));
    if (GET_IluCall_needFinish(JGC_WP_REVEAL(jwp_call))) {
        ILUJAVA_EXPECT(CALL_IN, ILUJAVA_VOID)
        if ( ! ilu_ReplyRead(cCall, &err)) {
            /* Error; Call finishCall anyway. */
            _ilujava_Report(&err, "doneReadReply"); 
            finishCall_DontRaiseErrors0(JENV_ACTUAL (JIluCall)JGC_WP_REVEAL(jwp_call));
            _ilujava_IluErrorToException(&err, "doneReadReply");
            return;
        }
    }
    TRANSITION(CALL_INBETWEEN)
} /*IluCall_doneReadReply*/



JAVAEXPORT(IluCall_startReadRequest, Jint)
        JIluCall jh_call
        ENDJAVAEXPORT
    /* used by true objects */
{
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    TRANSITION(CALL_IN)
    /* I think there is nothing to do */
    return 0;
} /*IluCall_startReadRequest*/



JAVAEXPORT(IluCall_doneReadRequest, Jboolean)
        JIluCall jh_call
        ENDJAVAEXPORT
    /* used by true objects */
{
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    ILU_ERRS((IoErrs)) err = ILU_INIT_NO_ERR;
    ilu_boolean     ans;
    ilu_Call cCall = GET_IluCall_yCall(JGC_WP_REVEAL(jwp_call));
    ILUJAVA_EXPECT(CALL_IN, 0)
    ans = ilu_RequestRead(cCall, &err);
    if (ILU_ERRNOK(err)) {
        finishCall_DontRaiseErrors0(JENV_ACTUAL (JIluCall)JGC_WP_REVEAL(jwp_call));
        ILU_HANDLED(err);
    }
    TRANSITION(CALL_INBETWEEN)
    return (ans!=0);
} /*IluCall_doneReadRequest*/


#define hasExceptions(jh_call) ((GET_IluMethodRep_jjExceptions(GET_IluCall_jjMethodRep(jh_call))) !=0 )


JAVAEXPORT(IluCall_beginSizingReply, Jint)
    JIluCall jh_call
    ENDJAVAEXPORT
    /* used by true objects */
{
     JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    ILU_ERRS((IoErrs)) err = ILU_INIT_NO_ERR;
    ilu_cardinal ret; 
    ilu_Call cCall = GET_IluCall_yCall(JGC_WP_REVEAL(jwp_call));
    ILUJAVA_EXPECT(CALL_INBETWEEN, 0)
    ret = ilu_BeginSizingReply(cCall, 
        hasExceptions(JGC_WP_REVEAL(jwp_call)), &err
        );
    if (ILU_ERRNOK(err)) {
        ILU_HANDLED(err);
    }
    TRANSITION(CALL_SZ)
    return (Jint) ret;
} /*IluCall_beginSizingReply*/


JAVAEXPORT(IluCall_startWriteReply, Jboolean)
        JIluCall jh_call, Jint argSize
        ENDJAVAEXPORT
    /* used by true objects */
{
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    ILU_ERRS((bad_locks, IoErrs)) err = ILU_INIT_NO_ERR;
    ilu_boolean ans; 
    ilu_Call cCall = GET_IluCall_yCall(JGC_WP_REVEAL(jwp_call));
    EXPECT_SZ_OR_INBETWEEN(0)
    ans = ilu_BeginReply(
        cCall, 
        hasExceptions(JGC_WP_REVEAL(jwp_call)), (ilu_cardinal) argSize, &err
        );
    if (! ans) {
        finishCall_DontRaiseErrors1(JENV_ACTUAL (JIluCall)JGC_WP_REVEAL(jwp_call));
        ILU_HANDLED(err);
    }
    TRANSITION(CALL_OUT)
    return (ans != 0);
} /* IluCall_startWriteReply */


JAVAEXPORT(IluCall_beginSizingException, Jint)
        JIluCall jh_call, Jint eindex
        ENDJAVAEXPORT
    /* used by true objects */
{
     JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    ILU_ERRS((IoErrs)) err = ILU_INIT_NO_ERR;
    ilu_cardinal ret; 
    ilu_Call cCall = GET_IluCall_yCall(JGC_WP_REVEAL(jwp_call));
    ILUJAVA_EXPECT(CALL_INBETWEEN, 0)
    ret = ilu_BeginSizingException(cCall, (ilu_integer) eindex, &err);
    ILU_HANDLED(err);
    TRANSITION(CALL_SZ)
    return (Jint) ret;
} /* IluCall_beginSizingException */


JAVAEXPORT(IluCall_startWriteException, Jboolean)
        JIluCall jh_call, Jint evalu, Jint argSize
        ENDJAVAEXPORT
    /* used by true objects */
{
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    ILU_ERRS((bad_locks, IoErrs)) err = ILU_INIT_NO_ERR;
    ilu_boolean ans; 
    ilu_Call cCall;
    cCall = GET_IluCall_yCall(JGC_WP_REVEAL(jwp_call));
    
    EXPECT_SZ_OR_INBETWEEN(0)
    TRANSITION(CALL_OUT)
    ans = ilu_BeginException(
        cCall, (ilu_integer) evalu, (ilu_cardinal) argSize, &err);
    ILU_HANDLED(err);
    if (! ans) {
        finishCall_DontRaiseErrors1(JENV_ACTUAL 
            (JIluCall)JGC_WP_REVEAL(jwp_call));
    }
    return (ans!=0);
} /* IluCall_startWriteException */


JAVAEXPORT(IluCall_noReply, Jboolean)
        JIluCall jh_call
        ENDJAVAEXPORT
    /* used by true objects */
{
     JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    ILU_ERRS((bad_param, bad_locks, broken_locks)) err = ILU_INIT_NO_ERR;
    ilu_boolean ans; 
    ilu_Call cCall = GET_IluCall_yCall(JGC_WP_REVEAL(jwp_call));
    ILUJAVA_EXPECT(CALL_INBETWEEN, 0)
    ans = ilu_NoReply(cCall, &err);
    ILU_HANDLED(err);
    finishCall_DontRaiseErrors1(JENV_ACTUAL (JIluCall)JGC_WP_REVEAL(jwp_call));
    TRANSITION(CALL_INBETWEEN)
    return (ans!=0);
} /* IluCall_noReply */


JAVAEXPORT(IluCall_doneWriteReply, Jboolean)
        JIluCall jh_call
        ENDJAVAEXPORT
    /* used by true objects */
{
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
        ILU_ERRS((bad_locks, IoErrs)) err = ILU_INIT_NO_ERR;
    ilu_boolean ans; 
    ilu_Call cCall = GET_IluCall_yCall(JGC_WP_REVEAL(jwp_call));
    ILUJAVA_EXPECT(CALL_OUT, 0)
    ans = ilu_FinishReply(cCall, &err);
    ILU_HANDLED(err);
    finishCall_DontRaiseErrors1(JENV_ACTUAL (JIluCall)JGC_WP_REVEAL(jwp_call));
    TRANSITION(CALL_INBETWEEN)
    return (ans!=0);
}


JAVAEXPORT(IluCall_doneWriteException, Jboolean)
        JIluCall jh_call
        ENDJAVAEXPORT
    /* used by true objects */
{
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    ILU_ERRS((bad_locks, IoErrs)) err = ILU_INIT_NO_ERR;
    ilu_boolean ans; 
    ilu_Call cCall = GET_IluCall_yCall(JGC_WP_REVEAL(jwp_call));
    ILUJAVA_EXPECT(CALL_OUT, 0)
    ans = ilu_FinishException(cCall, &err);
    ILU_HANDLED(err);
    TRANSITION(CALL_INBETWEEN)
    return (ans!=0);
}


JAVAEXPORT(IluCall_szInt32, Jint)
    JIluCall jh_call, Jint ival
    ENDJAVAEXPORT
{
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    PROLOG_SZ()
    sz = ilu_SizeOfInteger(cCall, (ilu_integer) ival, &ioerrs);
    ERRCHECK(0)
    return (Jint) sz;
}


JAVAEXPORT(IluCall_outInt32, void)
    JIluCall jh_call, Jint ival
    ENDJAVAEXPORT
{
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    PROLOG(CALL_OUT, ILUJAVA_VOID)
    ilu_OutputInteger(cCall, (ilu_integer) ival, &ioerrs);
    ERRCHECK(ILUJAVA_VOID)
}


JAVAEXPORT(IluCall_inInt32, Jint)
    JIluCall jh_call
    ENDJAVAEXPORT
{
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    ilu_integer value;
    PROLOG(CALL_IN, 0)
    ilu_InputInteger(cCall, &value, &ioerrs);
    ERRCHECK(0)
    return (Jint) value;
}


JAVAEXPORT(IluCall_szBool, Jint)
    JIluCall jh_call, Jboolean ival
    ENDJAVAEXPORT
{
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    PROLOG_SZ()
    sz = ilu_SizeOfBoolean(cCall, (ival != 0), &ioerrs);
    ERRCHECK(0)
    return (Jint) sz;
}


JAVAEXPORT(IluCall_outBool, void)
    JIluCall jh_call, Jboolean ival
    ENDJAVAEXPORT
{
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    PROLOG(CALL_OUT, ILUJAVA_VOID)
    ilu_OutputBoolean(cCall, (ival != 0), &ioerrs);
    ERRCHECK(ILUJAVA_VOID)
}


JAVAEXPORT(IluCall_inBool, Jboolean)
    JIluCall jh_call
    ENDJAVAEXPORT
{
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    ilu_boolean value;
    PROLOG(CALL_IN, 0)
    ilu_InputBoolean(cCall, &value, &ioerrs);
    ERRCHECK(0)
    return (Jboolean) value;
}


JAVAEXPORT(IluCall_szInt16, Jint)
    JIluCall jh_call, Jshort ival
    ENDJAVAEXPORT
{
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    PROLOG_SZ()
    sz = ilu_SizeOfShortInteger(cCall, (ilu_shortinteger) ival, &ioerrs);
    ERRCHECK(0)
    return (Jint) sz;
}


JAVAEXPORT(IluCall_outInt16, void)
    JIluCall jh_call, Jshort ival
    ENDJAVAEXPORT
{
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    PROLOG(CALL_OUT, ILUJAVA_VOID)
    ilu_OutputShortInteger(cCall, (ilu_shortinteger) ival, &ioerrs);
    ERRCHECK(ILUJAVA_VOID)
}

#ifdef RNI
/* get around msjavah generating a long return type */
JAVAEXPORT(IluCall_inInt16, Jint)
    JIluCall jh_call
    ENDJAVAEXPORT
{
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    ilu_shortinteger value;
    PROLOG(CALL_IN, 0)
    ilu_InputShortInteger(cCall, &value, &ioerrs);
    ERRCHECK(0)
    return (Jint) value;
}
#else
JAVAEXPORT(IluCall_inInt16, Jshort)
    JIluCall jh_call
    ENDJAVAEXPORT
{
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    ilu_shortinteger value;
    PROLOG(CALL_IN, 0)
    ilu_InputShortInteger(cCall, &value, &ioerrs);
    ERRCHECK(0)
    return (Jshort) value;
}
#endif

JAVAEXPORT(IluCall_szByte, Jint)
    JIluCall jh_call, Jbyte ival
    ENDJAVAEXPORT
{
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    PROLOG_SZ()
    sz = ilu_SizeOfByte(cCall, (ilu_byte) ival, &ioerrs);
    ERRCHECK(0)
    return (Jint) sz;
}


JAVAEXPORT(IluCall_outByte, void)
    JIluCall jh_call, Jbyte ival
    ENDJAVAEXPORT
{
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    PROLOG(CALL_OUT, ILUJAVA_VOID)
    ilu_OutputByte(cCall, (ilu_byte) ival, &ioerrs);
    ERRCHECK(ILUJAVA_VOID)
}


#ifdef RNI
/* get around msjavah generating a long return type */
JAVAEXPORT(IluCall_inByte, Jint)
    JIluCall jh_call
    ENDJAVAEXPORT
{
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    ilu_byte value;
    PROLOG(CALL_IN, 0)
    ilu_InputByte(cCall, &value, &ioerrs);
    ERRCHECK(0)
    return (Jint) value;
}
#else
JAVAEXPORT(IluCall_inByte, Jbyte)
    JIluCall jh_call
    ENDJAVAEXPORT
{
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    ilu_byte value;
    PROLOG(CALL_IN, 0)
    ilu_InputByte(cCall, &value, &ioerrs);
    ERRCHECK(0)
    return (Jbyte) value;
}
#endif


JAVAEXPORT(IluCall_szChar16, Jint)
    JIluCall jh_call, Jchar ival
    ENDJAVAEXPORT
{
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    PROLOG_SZ()
    sz = ilu_SizeOfCharacter(cCall, (ilu_character) ival, &ioerrs);
    ERRCHECK(0)
    return (Jint) sz;
}


JAVAEXPORT(IluCall_outChar16x, void)
    JIluCall jh_call, Jchar ival
    ENDJAVAEXPORT
{
   JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
   PROLOG(CALL_OUT, ILUJAVA_VOID)
    ilu_OutputCharacter(cCall, (ilu_character) ival, &ioerrs);
    ERRCHECK(ILUJAVA_VOID)
}

#ifdef RNI
/* get around msjavah generating a long return type */
JAVAEXPORT(IluCall_inChar16, Jint)
    JIluCall jh_call
    ENDJAVAEXPORT
{
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    ilu_character value;
    PROLOG(CALL_IN, 0)
    ilu_InputCharacter(cCall, &value, &ioerrs);
    ERRCHECK(0)
    return (Jint) value;
}
#else
JAVAEXPORT(IluCall_inChar16, Jchar)
    JIluCall jh_call
    ENDJAVAEXPORT
{
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    ilu_character value;
    PROLOG(CALL_IN, 0)
    ilu_InputCharacter(cCall, &value, &ioerrs);
    ERRCHECK(0)
    return (Jchar) value;
}
#endif

JAVAEXPORT(IluCall_szChar8, Jint)
    JIluCall jh_call, Jchar ival
    ENDJAVAEXPORT
{
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    PROLOG_SZ()
    sz = ilu_SizeOfShortCharacter(cCall, (ilu_shortcharacter) ival, &ioerrs);
    ERRCHECK(0)
    return (Jint) sz;
}


JAVAEXPORT(IluCall_outChar8x, void)
    JIluCall jh_call, Jchar ival
    ENDJAVAEXPORT
{
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    PROLOG(CALL_OUT, ILUJAVA_VOID)
    ilu_OutputShortCharacter(cCall, (ilu_shortcharacter) ival, &ioerrs);
    ERRCHECK(ILUJAVA_VOID)
}

#ifdef RNI
/* get around msjavah generating a long return type */
JAVAEXPORT(IluCall_inChar8, Jint)
    JIluCall jh_call
    ENDJAVAEXPORT
{
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    ilu_shortcharacter value;
    PROLOG(CALL_IN, 0)
    ilu_InputShortCharacter(cCall, &value, &ioerrs);
    ERRCHECK(0)
    return (Jint) value;
}
#else
JAVAEXPORT(IluCall_inChar8, Jchar)
    JIluCall jh_call
    ENDJAVAEXPORT
{
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    ilu_shortcharacter value;
    PROLOG(CALL_IN, 0)
    ilu_InputShortCharacter(cCall, &value, &ioerrs);
    ERRCHECK(0)
    return (Jchar) value;
}
#endif

JAVAEXPORT(IluCall_szObjectx, Jint)
    JIluCall jh_call,
    JIluOInt jh_oi, 
    Jboolean discriminant,
    JIluClassRep jh_staticClass
    ENDJAVAEXPORT
{  
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    JGC_WP_TYPE(JIluOInt) jwp_oi = JGC_GET_WP_FROM_JAVA_OBJECT(jh_oi);
    JGC_WP_TYPE(JIluClassRep) jwp_staticClass = 
        JGC_GET_WP_FROM_JAVA_OBJECT(jh_staticClass);
    JIluClassRep jh_actualClass;
    ilu_Object cObject = 0;
    ilu_Class cStaticClass = 0;
    ilu_Class cActualClass = 0;
    ilu_Server cServer = 0;
    PROLOG_SZ()
    if (JGC_WP_REVEAL(jwp_staticClass)==0) {
        _ilujava_IluErrorToCallException(0, "xxx-21a");
        return 0;
    }
    cStaticClass = GET_IluClassRep_yIluClass(JGC_WP_REVEAL(jwp_staticClass));
    if (JGC_WP_REVEAL(jwp_oi)) {
        jh_actualClass = GET_IluOInt_jjClassRep(JGC_WP_REVEAL(jwp_oi));
        if (jh_actualClass==0) {
            _ilujava_IluErrorToCallException(0, "xxx-23a");
            return 0;
        }
        cActualClass = GET_IluClassRep_yIluClass(jh_actualClass);
        /* Use Object's server; not call's server */
        cServer = _ilujava_EnterServer(JENV_ACTUAL
            (JIluOInt)JGC_WP_REVEAL(jwp_oi), cActualClass);
        if (cServer==0) {
            _ilujava_IluErrorToCallException(0, "xxx-23b");
            return 0;
        }
        cObject = GET_IluOInt_yIluKernelObject(JGC_WP_REVEAL(jwp_oi)); 
        if (cObject==0) {
            /*we freed the object right before entering the monitor*/
            ilu_ExitServer(cServer, cActualClass);
            _ilujava_IluErrorToCallException(0, "xxx-23b");
            return 0;
        }
    }
    sz = ilu_SizeOfObjectID(
        cCall, cObject, discriminant!=0, cStaticClass, &ioerrs);
    if (JGC_WP_REVEAL(jwp_oi)) ilu_ExitServer(cServer, cActualClass); 
    ERRCHECK(0)
    return (Jint) sz;
}


JAVAEXPORT(IluCall_outObjectx, void)
    JIluCall jh_call,
        JIluOInt jh_oi, 
        Jboolean discriminant,
        JIluClassRep jh_staticClass
        ENDJAVAEXPORT
{
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    JGC_WP_TYPE(JIluOInt) jwp_oi = JGC_GET_WP_FROM_JAVA_OBJECT(jh_oi);
    JGC_WP_TYPE(JIluClassRep) jwp_staticClass =     
        JGC_GET_WP_FROM_JAVA_OBJECT(jh_staticClass);
    JIluClassRep jh_actualClass;
    ilu_Object cObject = 0;
    ilu_Class cStaticClass = 0;
    ilu_Class cActualClass = 0;
    ilu_Server cServer;
    PROLOG(CALL_OUT, ILUJAVA_VOID)
    if (JGC_WP_REVEAL(jwp_staticClass)==0) {
        _ilujava_IluErrorToCallException(&ioerrs, "outObject: no static class");
    }
    if (JGC_WP_REVEAL(jwp_oi)) {
        jh_actualClass = GET_IluOInt_jjClassRep(JGC_WP_REVEAL(jwp_oi));
        if (jh_actualClass==0) {
            _ilujava_IluErrorToCallException(0, "xxx-23d");
            return;
        }
        /* Use Object's server; not call's server */
        cActualClass = GET_IluClassRep_yIluClass(jh_actualClass);
        cServer = _ilujava_EnterServer(JENV_ACTUAL
            (JIluOInt)JGC_WP_REVEAL(jwp_oi), cActualClass);
        if (cServer==0) {
            _ilujava_IluErrorToCallException(0, "xxx-23f bad server");
            return;
        }
        cObject = GET_IluOInt_yIluKernelObject(JGC_WP_REVEAL(jwp_oi));
        if (cObject==0) {
            /* we freed the object right before entering the monitor */
            ilu_ExitServer(cServer, cActualClass);
            _ilujava_IluErrorToCallException(0, 
                "xxx-23e Object has been unregistered");
            return;
        }
    }
    JTRACE(_ilujava_callsFlag>0, ("$ IluCall_outObjectx c<%x>\n", cObject));
    cStaticClass = GET_IluClassRep_yIluClass(JGC_WP_REVEAL(jwp_staticClass));
    ilu_OutputObjectID(
        cCall, cObject, discriminant != 0, cStaticClass, &ioerrs
        );
    /* I believe the locking comment means that doing nothing is the 
     * right thing to do 
     */
    ERRCHECK(ILUJAVA_VOID)
}


JAVAEXPORT(IluCall_inOI, JIluOInt)
    JIluCall jh_call,
    Jboolean discriminant, 
    JIluClassRep jh_staticClass
    ENDJAVAEXPORT
{
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    JGC_WP_TYPE(JIluClassRep) jwp_staticClass =     
        JGC_GET_WP_FROM_JAVA_OBJECT(jh_staticClass);
    JGC_WP_TYPE(JIluOInt) jwp_oi = 0; 
    ilu_Server cServer;
    ilu_Object cIluObject;
    ilu_Class cStaticClass;
    ilu_refany lso_of_kernel_object;

    PROLOG(CALL_IN, 0)
    JTRACE(_ilujava_callsFlag>1, ("$ IluCall_inOI\n"));
    if (JGC_WP_REVEAL(jwp_staticClass)==0) {
        _ilujava_IluErrorToCallException(&ioerrs, "inOI: no static class");
    }
    cStaticClass = GET_IluClassRep_yIluClass(JGC_WP_REVEAL(jwp_staticClass));
    ilu_InputObjectID(cCall, &
        cIluObject, discriminant!=0, cStaticClass, &ioerrs
        );
    /*o!=NIL => Inside(*o's server...); ILU_ERRNOK(*err) => *o==NIL*/ 
    ERRCHECK(0)
    JTRACE(_ilujava_callsFlag>0, ("$ IluCall_inOI got c<%x>\n", cIluObject));
    if (cIluObject==0) {
        return 0;
    }
    cServer = ilu_ServerOfObject(cIluObject); /* inside server lock ! */
    lso_of_kernel_object = ilu_GetLanguageSpecificObject(cIluObject, JAVALANGIDX);
    jwp_oi = (JGC_WP_TYPE(JIluOInt)) lso_of_kernel_object;
    if (jwp_oi) {
        JTRACE(_ilujava_callsFlag>0, 
            ("$ IluCall_inOI: got j<%x>\n", JGC_WP_REVEAL( jwp_oi)));
    } else {
        JTRACE(_ilujava_callsFlag>0,
            ("$ IluCall_inOI: got null\n"));
    }
    if (jwp_oi == 0) {
        /* Object is not/no-more known; create a surrogate */
        JTRACE(_ilujava_callsFlag>0,
            ("$ IluCall_inOI: needs to create ob\n"));
        if (ilu_TrueServerForLanguageP(cServer, JAVALANGIDX)) {
            ilu_ExitServer(cServer, cStaticClass);
            _ilujava_IluErrorToCallException(0, "True object has disappeared");
            return 0;
        } else {
            /* may java GC */
            JIluOInt jh_oi = (JIluOInt) _ilujava_creatSurrOIFromRegistry(
            	JENV_ACTUAL
            	cIluObject, 
            	(ilu_refany) JGC_WP_REVEAL(jwp_staticClass)
            	);
            if (jh_oi) {
                 jwp_oi = JGC_GET_WP_FROM_JAVA_OBJECT(jh_oi);
            }
        }
    } else {
        /* got an object */
        JIluOInt jh_oi = (JIluOInt) JGC_WP_REVEAL(jwp_oi);
        if (jh_oi == 0) {
             /* impossible */
             JTRACE(1, ("$ IluCall_inOI: IMPOSSIBLE/n"));
             _ilujava_IluErrorToCallException(0, "HUH-IMPOSSIBLE");
             return 0;
        }
        if (GET_IluOInt_retained(jh_oi) != 0) {
            PUT_IluOInt_ghost(jh_oi, 1);
        }
    }
    /* Need object's server; not call's server */
    ilu_ExitServer(cServer, cStaticClass); /*iluxport.h says: "static_type"*/
    if (jwp_oi) {
        return (JIluOInt) JGC_WP_REVEAL(jwp_oi);
    } else {
        return 0;
    }
}


JAVAEXPORT(IluCall_getCallSingletonOIx, JIluOInt)
    JIluCall jh_call
    ENDJAVAEXPORT
{
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    ilu_Server cServer;
    ilu_Object cIluObject;
    JIluOInt jh_oi = 0; 
    ilu_refany lso_of_kernel_object;
    PROLOG(CALL_IN, 0);
    cIluObject = ilu_GetCallSingleton(cCall, &ioerrs);
    ERRCHECK(0)
    if (cIluObject!=0) {
        cServer = ilu_ServerOfObject(cIluObject);
        lso_of_kernel_object = ilu_GetLanguageSpecificObject(cIluObject, JAVALANGIDX);
        if (lso_of_kernel_object) {
            jh_oi = (JIluOInt) 
                JGC_WP_REVEAL( (JGC_WP_TYPE(JIluOInt)) lso_of_kernel_object);
        } else {
            jh_oi = 0;
        }
        if (jh_oi == 0) {
            ilu_ExitServer(cServer, ilu_IntroTypeOfCall(cCall));
            _ilujava_IluErrorToCallException(0, "yyy-2");
            return 0;
        }
        ilu_ExitServer(cServer, ilu_IntroTypeOfCall(cCall));
        /*see ilu.c to figure out whether to use static or actual class...*/
        }
    return jh_oi;
}


JAVAEXPORT(IluCall_szOptional, Jint)
    JIluCall jh_call, Jboolean stat
    ENDJAVAEXPORT
{
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    ilu_boolean istat = (stat != 0);
    PROLOG_SZ()
    sz = ilu_SizeOfOptional(cCall, istat, (ilu_Type)ILU_NIL, &ioerrs);
    ERRCHECK(0)
    return (Jint) sz;
}


JAVAEXPORT(IluCall_outOptional, void)
    JIluCall jh_call, Jboolean stat
    ENDJAVAEXPORT
{
     JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    ilu_boolean istat = (stat != 0);
    PROLOG(CALL_OUT, ILUJAVA_VOID)
    ilu_OutputOptional(cCall, istat, (ilu_Type)ILU_NIL, &ioerrs);
    ERRCHECK(ILUJAVA_VOID)
}


JAVAEXPORT(IluCall_inOptional, Jboolean)
    JIluCall jh_call
    ENDJAVAEXPORT
{
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    ilu_boolean optionalStatus;
    PROLOG(CALL_IN, 0)
    ilu_InputOptional(cCall, &optionalStatus, (ilu_Type)ILU_NIL, &ioerrs);
    ERRCHECK(0)
    return (Jboolean) optionalStatus;
}


JAVAEXPORT(IluCall_szArray, Jint)
    JIluCall jh_call, Jint elementCount
    ENDJAVAEXPORT
{
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    ilu_cardinal length = (ilu_cardinal) elementCount;
    PROLOG_SZ()
    sz = ilu_SizeOfArray(cCall, length, (ilu_Type)ILU_NIL, &ioerrs);
    ERRCHECK(0)
    return (Jint) sz;
}


JAVAEXPORT(IluCall_outArray, void)
    JIluCall jh_call, Jint elementCount
    ENDJAVAEXPORT
{
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    ilu_cardinal length = (ilu_cardinal) elementCount;
    PROLOG(CALL_OUT, ILUJAVA_VOID)
    ilu_OutputArray(cCall, length, (ilu_Type)ILU_NIL, &ioerrs);
    ERRCHECK(ILUJAVA_VOID)
}


JAVAEXPORT(IluCall_inArray, void)
    JIluCall jh_call
    ENDJAVAEXPORT
{
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    PROLOG(CALL_IN, ILUJAVA_VOID)
    ilu_InputArray(cCall, (ilu_Type)ILU_NIL, &ioerrs);
    ERRCHECK(ILUJAVA_VOID)
}


JAVAEXPORT(IluCall_endArray, void)
    JIluCall jh_call
    ENDJAVAEXPORT
{
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    ILU_ERRS((IoErrs)) ioerrs = ILU_INIT_NO_ERR;
    ilu_boolean x;
    ilu_Call cCall = GET_IluCall_yCall(JGC_WP_REVEAL(jwp_call));
    EXPECT_LIFE()
    x = ilu_EndArray(cCall, &ioerrs);
    ERRCHECK(ILUJAVA_VOID)
}


JAVAEXPORT(IluCall_szRecord, Jint)
    JIluCall jh_call
    ENDJAVAEXPORT
{
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    PROLOG_SZ()
    sz = ilu_SizeOfRecord(cCall, (ilu_Type)ILU_NIL, &ioerrs);
    ERRCHECK(0)
    return (Jint) sz;
}


JAVAEXPORT(IluCall_outRecord, void)
    JIluCall jh_call
    ENDJAVAEXPORT
{
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    PROLOG(CALL_OUT, ILUJAVA_VOID)
    ilu_OutputRecord(cCall, (ilu_Type)ILU_NIL, &ioerrs);
    ERRCHECK(ILUJAVA_VOID)
}


JAVAEXPORT(IluCall_inRecord, void)
    JIluCall jh_call
    ENDJAVAEXPORT
{
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    PROLOG(CALL_IN, ILUJAVA_VOID)
    ilu_InputRecord(cCall, (ilu_Type)ILU_NIL, &ioerrs);
    ERRCHECK(ILUJAVA_VOID)
}


JAVAEXPORT(IluCall_endRecord, void)
    JIluCall jh_call
    ENDJAVAEXPORT
{
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    ILU_ERRS((IoErrs)) ioerrs = ILU_INIT_NO_ERR;
    ilu_boolean x;
    ilu_Call cCall = GET_IluCall_yCall(JGC_WP_REVEAL(jwp_call));
    EXPECT_LIFE()
    x = ilu_EndRecord(cCall, &ioerrs);
    ERRCHECK(ILUJAVA_VOID)
}


JAVAEXPORT(IluCall_szSequence, Jint)
    JIluCall jh_call, Jint leng, Jint limit
    ENDJAVAEXPORT
{
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    ilu_cardinal len = (ilu_cardinal) leng;
    ilu_cardinal lim = (ilu_cardinal) limit;
    PROLOG_SZ()
    sz = ilu_SizeOfSequence(cCall, len, lim, (ilu_Type)ILU_NIL, &ioerrs);
    ERRCHECK(0)
    return (Jint) sz;
}


JAVAEXPORT(IluCall_outSequence, void)
    JIluCall jh_call, Jint leng, Jint limit
    ENDJAVAEXPORT
{
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    ilu_cardinal len = (ilu_cardinal) leng;
    ilu_cardinal lim = (ilu_cardinal) limit;
    PROLOG(CALL_OUT, ILUJAVA_VOID)
    ilu_OutputSequence(cCall, len, lim, (ilu_Type)ILU_NIL, &ioerrs);
    ERRCHECK(ILUJAVA_VOID)
}


JAVAEXPORT(IluCall_outSequenceMark, void)
    JIluCall jh_call, Jint extent
    ENDJAVAEXPORT
{
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    PROLOG(CALL_OUT, ILUJAVA_VOID)
    ilu_OutputSequenceMark(cCall, (ilu_cardinal) extent, &ioerrs);
    ERRCHECK(ILUJAVA_VOID)
}


JAVAEXPORT(IluCall_inSequence, Jint)
    JIluCall jh_call, Jint limit
    ENDJAVAEXPORT
{
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    ilu_cardinal len;
    ilu_cardinal lim = (ilu_cardinal) limit;
    PROLOG(CALL_IN, 0)
    ilu_InputSequence(cCall, &len, lim, (ilu_Type)ILU_NIL, &ioerrs);
    ERRCHECK(0)
    return (Jint) len;
}


JAVAEXPORT(IluCall_inSequenceMark, void)
    JIluCall jh_call, Jint extent
    ENDJAVAEXPORT
{
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    PROLOG(CALL_IN, ILUJAVA_VOID)
    ilu_InputSequenceMark(cCall, (ilu_cardinal) extent, &ioerrs);
    ERRCHECK(ILUJAVA_VOID)
}


JAVAEXPORT(IluCall_endSequence, void)
    JIluCall jh_call
    ENDJAVAEXPORT
{
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    ILU_ERRS((IoErrs)) ioerrs = ILU_INIT_NO_ERR;
    ilu_boolean x;
    ilu_Call cCall = GET_IluCall_yCall(JGC_WP_REVEAL(jwp_call));
    EXPECT_LIFE()
    x = ilu_EndSequence(cCall, &ioerrs);
    ERRCHECK(ILUJAVA_VOID)
}


JAVAEXPORT(IluCall_nSzUnion, Jint)
    JIluCall jh_call, Jint discrim, Jint discrimKind
    ENDJAVAEXPORT
{
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    PROLOG_SZ()
    sz = ilu_SizeOfUnion(
        cCall, (ilu_cardinal) discrim, (ilu_TypeKind) discrimKind, (ilu_Type)ILU_NIL, &ioerrs);
    ERRCHECK(0)
    return (Jint) sz;
}


JAVAEXPORT(IluCall_nOutUnion, void)
    JIluCall jh_call, Jint discrim, Jint discrimKind
    ENDJAVAEXPORT
{
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    PROLOG(CALL_OUT, ILUJAVA_VOID)
    ilu_OutputUnion(
        cCall, (ilu_cardinal) discrim, (ilu_TypeKind) discrimKind, (ilu_Type)ILU_NIL, &ioerrs);
    ERRCHECK(ILUJAVA_VOID)
}


JAVAEXPORT(IluCall_nInUnion, Jint)
    JIluCall jh_call, Jint discrimKind
    ENDJAVAEXPORT
{
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    ilu_cardinal discriminator;
    PROLOG(CALL_IN, 0)
    ilu_InputUnion(cCall, &discriminator, (ilu_TypeKind) discrimKind, (ilu_Type)ILU_NIL, &ioerrs);
    ERRCHECK(0)
    return (Jint) discriminator;
}


JAVAEXPORT(IluCall_endUnion, void)
    JIluCall jh_call
    ENDJAVAEXPORT
{
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    ILU_ERRS((IoErrs)) ioerrs = ILU_INIT_NO_ERR;
    ilu_boolean x;
    ilu_Call cCall = GET_IluCall_yCall(JGC_WP_REVEAL(jwp_call));
    EXPECT_LIFE()
    x = ilu_EndUnion(cCall, &ioerrs);
    ERRCHECK(ILUJAVA_VOID)
}


JAVAEXPORT(IluCall_szInt64, Jint)
    JIluCall jh_call, Jlong ival
    ENDJAVAEXPORT
{
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    ilu_longinteger i = TOIluInt64(ival);
    PROLOG_SZ()
    sz = ilu_SizeOfLongInteger(cCall, i, &ioerrs);
    ERRCHECK(0)
    return (Jint) sz;
}


JAVAEXPORT(IluCall_outInt64, void)
    JIluCall jh_call, Jlong ival
    ENDJAVAEXPORT
{
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    ilu_longinteger i = TOIluInt64(ival);
    PROLOG(CALL_OUT, ILUJAVA_VOID)
    ilu_OutputLongInteger(cCall, i, &ioerrs);
    ERRCHECK(ILUJAVA_VOID)
}


JAVAEXPORT(IluCall_inInt64, Jlong)
    JIluCall jh_call
    ENDJAVAEXPORT
{
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    Jlong jResult = 0;
    ilu_longinteger cResult;
    PROLOG(CALL_IN, jResult)
    ilu_InputLongInteger(cCall, &cResult, &ioerrs);
    ERRCHECK(jResult)
    jResult = TOJavaIC64(cResult);
    return jResult;
}


JAVAEXPORT(IluCall_szCard64, Jint)
    JIluCall jh_call, Jlong v
    ENDJAVAEXPORT
{
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    PROLOG_SZ()
    sz = ilu_SizeOfLongCardinal(cCall, TOIluCard64(v), &ioerrs);
    ERRCHECK(0)
    return (Jint) sz;
}


JAVAEXPORT(IluCall_outCard64, void)
    JIluCall jh_call, Jlong v
    ENDJAVAEXPORT
{
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    PROLOG(CALL_OUT, ILUJAVA_VOID)
    ilu_OutputLongCardinal(cCall, TOIluCard64(v), &ioerrs);
    ERRCHECK(ILUJAVA_VOID)
}


JAVAEXPORT(IluCall_inCard64, Jlong)
    JIluCall jh_call
    ENDJAVAEXPORT
{
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    Jlong jResult = 0;
    ilu_longcardinal cResult;
    PROLOG(CALL_IN, jResult)
    ilu_InputLongCardinal(cCall, &cResult, &ioerrs);
    ERRCHECK(jResult)
    jResult = TOJavaIC64(cResult);
    return jResult;
}


JAVAEXPORT(IluCall_szReal64, Jint)
    JIluCall jh_call, Jdouble val
    ENDJAVAEXPORT
{
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    PROLOG_SZ()
    sz = ilu_SizeOfReal(cCall, (double) val, &ioerrs);
    ERRCHECK(0)
    return (Jint) sz;
}


JAVAEXPORT(IluCall_outReal64, void)
    JIluCall jh_call, Jdouble val
    ENDJAVAEXPORT
{
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    PROLOG(CALL_OUT, ILUJAVA_VOID)
    ilu_OutputReal(cCall, (double) val, &ioerrs);
    ERRCHECK(ILUJAVA_VOID)
}


JAVAEXPORT(IluCall_inReal64, Jdouble)
    JIluCall jh_call
    ENDJAVAEXPORT
{
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    double value;
    PROLOG(CALL_IN, 0)
    ilu_InputReal(cCall, &value, &ioerrs);
    ERRCHECK(0)
    return (Jdouble) value;
}


JAVAEXPORT(IluCall_szReal128Buff, Jint)
    JIluCall jh_call, JArrayOfByte jh_bytes
    ENDJAVAEXPORT
{
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    JGC_WP_TYPE(JArrayOfByte) jwp_bytes = JGC_WP_MAKE(jh_bytes);
    char *buf;
    PROLOG_SZ()
    if (jh_bytes==0 
            || ARRAY_LENGTH(JGC_WP_REVEAL(jwp_bytes)) != sizeof(ilu_longreal)) {
        JGC_WP_RELEASE(jwp_bytes);
        _ilujava_IluErrorToCallException(0, "xxx_xxx101");
        return 0;
    }
    buf = (char*) ARRAY_byte_GET((JArrayOfByte)JGC_WP_REVEAL(jwp_bytes));
    /* what about endian-ness? */
    sz = ilu_SizeOfLongReal(cCall, * (ilu_longreal*) buf, &ioerrs);
    ARRAY_byte_RELEASE((JArrayOfByte)JGC_WP_REVEAL(jwp_bytes), buf);
    JGC_WP_RELEASE(jwp_bytes);
    ERRCHECK(0)
    return (Jint) sz;
}

JAVAEXPORT(IluCall_outReal128Buff, void)
    JIluCall jh_call, JArrayOfByte jh_bytes
    ENDJAVAEXPORT
{
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    JGC_WP_TYPE(JArrayOfByte) jwp_bytes = JGC_WP_MAKE(jh_bytes);
    char *buf;
    PROLOG(CALL_OUT, ILUJAVA_VOID)
    if ((jh_bytes==0) 
            || (ARRAY_LENGTH(JGC_WP_REVEAL(jwp_bytes)) != sizeof(ilu_longreal))) {
        JGC_WP_RELEASE(jwp_bytes);
        _ilujava_IluErrorToCallException(0, "xxx_xxx103");
        return;
    }
    buf = (char*) ARRAY_byte_GET((JArrayOfByte)JGC_WP_REVEAL(jwp_bytes));
    /* what about endian-ness ? */
    ilu_OutputLongReal(cCall, * (ilu_longreal*) buf, &ioerrs);
    ARRAY_byte_RELEASE((JArrayOfByte)JGC_WP_REVEAL(jwp_bytes), buf);
    JGC_WP_RELEASE(jwp_bytes);
    ERRCHECK(ILUJAVA_VOID)
}


JAVAEXPORT(IluCall_inReal128Buff, void)
    JIluCall jh_call, JArrayOfByte jh_bytes
    ENDJAVAEXPORT
{
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    JGC_WP_TYPE(JArrayOfByte) jwp_bytes = JGC_WP_MAKE(jh_bytes);
    char *buf;
    PROLOG(CALL_IN, ILUJAVA_VOID)
    if (jh_bytes==0 || ARRAY_LENGTH(JGC_WP_REVEAL(jwp_bytes)) != sizeof(ilu_longreal)) {
        JGC_WP_RELEASE(jwp_bytes);
        _ilujava_IluErrorToCallException(0, "xxx_xxx104");
        return;
    };
    buf = (char*) ARRAY_byte_GET((JArrayOfByte)JGC_WP_REVEAL(jwp_bytes));
    /* what about endian-ness? */
    ilu_InputLongReal(cCall, (ilu_longreal*) &buf, &ioerrs);
    ARRAY_byte_RELEASE_COPYBACK((JArrayOfByte)JGC_WP_REVEAL(jwp_bytes), buf);
    JGC_WP_RELEASE(jwp_bytes);
    ERRCHECK(ILUJAVA_VOID)
}


JAVAEXPORT(IluCall_szReal32, Jint)
    JIluCall jh_call, Jfloat f
    ENDJAVAEXPORT
{
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    PROLOG_SZ()
    sz = ilu_SizeOfShortReal(cCall, (float) f, &ioerrs);
    ERRCHECK(0)
    return (Jint) sz;
}


JAVAEXPORT(IluCall_outReal32, void)
    JIluCall jh_call, Jfloat f
    ENDJAVAEXPORT
{
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    PROLOG(CALL_OUT, ILUJAVA_VOID)
    ilu_OutputShortReal(cCall, (float) f, &ioerrs);
    ERRCHECK(ILUJAVA_VOID)
}


JAVAEXPORT(IluCall_inReal32, Jfloat)
    JIluCall jh_call
    ENDJAVAEXPORT
{
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    float f;
    PROLOG(CALL_IN, 0)
    ilu_InputShortReal(cCall, &f, &ioerrs);
    ERRCHECK(0)
    return (Jfloat) f;
}


JAVAEXPORT(IluCall_szEnum, Jint)
    JIluCall jh_call, Jint v
    ENDJAVAEXPORT
{
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    PROLOG_SZ()
    sz = ilu_SizeOfEnum(cCall, (ilu_shortcardinal) v, (ilu_Type)ILU_NIL, &ioerrs);
    ERRCHECK(0)
    return (Jint) sz;
}


JAVAEXPORT(IluCall_outEnum, void)
    JIluCall jh_call, Jint v
    ENDJAVAEXPORT
{
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    PROLOG(CALL_OUT, ILUJAVA_VOID)
    ilu_OutputEnum(cCall, (ilu_shortcardinal) v, (ilu_Type)ILU_NIL, &ioerrs);
    ERRCHECK(ILUJAVA_VOID)
}


JAVAEXPORT(IluCall_inEnum, Jint)
    JIluCall jh_call
    ENDJAVAEXPORT
{
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    ilu_shortcardinal v;
    PROLOG(CALL_IN, 0)
    ilu_InputEnum(cCall, &v, (ilu_Type)ILU_NIL, &ioerrs);
    ERRCHECK(0)
    return (Jint) v;
}


JAVAEXPORT(IluCall_szCard16, Jint)
    JIluCall jh_call, Jshort v
    ENDJAVAEXPORT
{
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    PROLOG_SZ()
    sz = ilu_SizeOfShortCardinal(cCall, (ilu_shortcardinal) v, &ioerrs);
    ERRCHECK(0)
    return (Jint) sz;
}


JAVAEXPORT(IluCall_outCard16, void)
    JIluCall jh_call, Jshort v
    ENDJAVAEXPORT
{
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    PROLOG(CALL_OUT, ILUJAVA_VOID)
    ilu_OutputShortCardinal(cCall, (ilu_shortcardinal) v, &ioerrs);
    ERRCHECK(ILUJAVA_VOID)
}

#ifdef RNI
/* get around msjavah generating a long return type */
JAVAEXPORT(IluCall_inCard16, Jint)
    JIluCall jh_call
    ENDJAVAEXPORT
{
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    ilu_shortcardinal v;
    PROLOG(CALL_IN, 0)
    ilu_InputShortCardinal(cCall, &v, &ioerrs);
    ERRCHECK(0)
    return (Jint) v;
}
#else
JAVAEXPORT(IluCall_inCard16, Jshort)
    JIluCall jh_call
    ENDJAVAEXPORT
{
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    ilu_shortcardinal v;
    PROLOG(CALL_IN, 0)
    ilu_InputShortCardinal(cCall, &v, &ioerrs);
    ERRCHECK(0)
    return (Jshort) v;
}
#endif

JAVAEXPORT(IluCall_szCard32, Jint)
    JIluCall jh_call, Jint v
    ENDJAVAEXPORT
{
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    PROLOG_SZ()
    sz = ilu_SizeOfCardinal(cCall, (ilu_cardinal) v, &ioerrs);
    ERRCHECK(0)
    return (Jint) sz;
}


JAVAEXPORT(IluCall_outCard32, void)
    JIluCall jh_call, Jint v
    ENDJAVAEXPORT
{
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    PROLOG(CALL_OUT, ILUJAVA_VOID)
    ilu_OutputCardinal(cCall, (ilu_cardinal) v, &ioerrs);
    ERRCHECK(ILUJAVA_VOID)
}


JAVAEXPORT(IluCall_inCard32, Jint)
    JIluCall jh_call
    ENDJAVAEXPORT
{
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    ilu_cardinal v;
    PROLOG(CALL_IN, 0)
    ilu_InputCardinal(cCall, &v, &ioerrs);
    ERRCHECK(0)
    return (Jint) v;
}


JAVAEXPORT(IluCall_szString8, Jint)
    JIluCall jh_call, JString s, Jint limit
    ENDJAVAEXPORT
    /* Variable-length array of short character (String) */
{   
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    Jint len;
    char * cString;
    PROLOG_SZ()
    if (s) { 
        len = JSTRING_LENGTH(s);
        if ((len > limit) && (limit != 0)) {
            _ilujava_IluErrorToCallException(0, "string length exceeds limit");
            return 0;
        }
        cString = IluJava_JString_toheap80(JENV_ACTUAL s);  
    } else { 
        len = 0; cString = (char *) &nullBuffer;
    }
    sz = ilu_SizeOfString(cCall, cString, 
    	(ilu_cardinal) len, (ilu_cardinal) limit, &ioerrs);
    if (s) {java_free(cString);}
    ERRCHECK(0)
    return (Jint) sz;
}


JAVAEXPORT(IluCall_outString8x, void)
    JIluCall jh_call, 
    JString s, 
    Jint limit
    ENDJAVAEXPORT
    /* Variable-length array of short character (String) */
{   
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    Jint len;
    char * cString;
    PROLOG(CALL_OUT, ILUJAVA_VOID)
    if (s) { 
        len = JSTRING_LENGTH(s);
        if ((len > limit) && (limit != 0)) {
            _ilujava_IluErrorToCallException(0, "outString8x: length exceeds limit"); return;
        }
        cString = IluJava_JString_toheap80(JENV_ACTUAL s); 
    } else { 
        len = 0; cString = (char *) &nullBuffer;
    }
    JTRACE(_ilujava_callsFlag>0, 
        ("$ outString8x len=%d  s=<%s> \n", len, cString));
    ilu_OutputString(cCall, cString, (ilu_cardinal) len, 
    	(ilu_cardinal) limit, &ioerrs
    	);
    if (s) {java_free(cString);}
    ERRCHECK(ILUJAVA_VOID)
}


JAVAEXPORT(IluCall_inString8, JString)
    JIluCall jh_call, Jint limit
    ENDJAVAEXPORT
{
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    JString s;
    char * cString = 0;
    ilu_cardinal len = 0;
    PROLOG(CALL_IN, 0)
    ilu_InputString(cCall, &cString, &len, (ilu_cardinal) limit, &ioerrs);
    if (ILU_ERRNOK(ioerrs)) {
        ilu_free(cString);
        _ilujava_IluErrorToException(&ioerrs, "IluCall.inString8");
        return 0;
    }
    JTRACE(_ilujava_callsFlag>0, 
        ("$ inString8 len=%d  s=<%s> \n", len, cString));
    s = IluJava_JString_fromAX(JENV_ACTUAL cString, (int) len);
    ilu_free(cString);
    return s;
}


JAVAEXPORT(IluCall_szBytesSx, Jint)
    JIluCall jh_call, 
    JArrayOfByte jh_bytes, 
    Jint start, Jint len, Jint limit
    ENDJAVAEXPORT
    /* Variable-length array of byte */
{
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    JGC_WP_TYPE(JArrayOfByte) jwp_bytes = JGC_WP_MAKE(jh_bytes);
    char *data;
    Jint dataLen;
    PROLOG_SZ()
    if ((len > limit) && (limit != 0)) {
        _ilujava_IluErrorToCallException(0, "szBytesSx: length exceeds limit");
        JGC_WP_RELEASE(jwp_bytes);
        return 0;
    }
    if (jh_bytes == 0) {
        dataLen = 0;
        start  = 0;
    } else {
        dataLen = ARRAY_LENGTH(JGC_WP_REVEAL(jwp_bytes));
    }
    if ((start>dataLen) || (len>(dataLen-start))) {
        /* data structure too small to contain len bytes */
        _ilujava_IluErrorToCallException(0, "szBytesSx: bad start offset");
        JGC_WP_RELEASE(jwp_bytes);
        return 0;
    }
    if (dataLen) {
        data = (char *) ARRAY_byte_GET((JArrayOfByte)JGC_WP_REVEAL(jwp_bytes));
    } else {
        data = (char *) &nullBuffer;
    }
    sz = ilu_SizeOfBytes(cCall, (ilu_bytes) data+start, 
    	(ilu_cardinal)len, (ilu_cardinal)limit, 
    	&ioerrs
    	);
    if (dataLen) {
        ARRAY_byte_RELEASE((JArrayOfByte)JGC_WP_REVEAL(jwp_bytes), data);
    }
    JGC_WP_RELEASE(jwp_bytes);
    ERRCHECK(0)
    return (Jint) sz;
}


JAVAEXPORT(IluCall_outBytesSx, void)
    JIluCall jh_call, 
    JArrayOfByte jh_bytes, 
    Jint start, Jint len, Jint limit
    ENDJAVAEXPORT
    /* Variable-length array of byte */
{
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    JGC_WP_TYPE(JArrayOfByte) jwp_bytes = JGC_WP_MAKE(jh_bytes);
    char *data;
    Jint dataLen;
    PROLOG(CALL_OUT, ILUJAVA_VOID)
    if ((len > limit) && (limit != 0)) {
        _ilujava_IluErrorToCallException(0, "length exceeds limit");
       JGC_WP_RELEASE(jwp_bytes);
       return;
    }
    if (jh_bytes == 0) {
        dataLen = 0;
        start  = 0;
    } else {
        dataLen = ARRAY_LENGTH(JGC_WP_REVEAL(jwp_bytes));
    }
    if ((start > dataLen) || (len > (dataLen-start))) {
        /* data structure too small to contain len bytes */
        _ilujava_IluErrorToCallException(0, "outBytesSx: bad start offset");
        JGC_WP_RELEASE(jwp_bytes);
        return;
    }
    if (dataLen) {
        data = (char *) ARRAY_byte_GET((JArrayOfByte)JGC_WP_REVEAL(jwp_bytes));
    } else {
        data = (char *) &nullBuffer;
    }
    ilu_OutputBytes(cCall, (ilu_bytes) data+start, 
    	(ilu_cardinal)len, (ilu_cardinal)limit, 
    	&ioerrs
    	);
    if (dataLen) {
        ARRAY_byte_RELEASE((JArrayOfByte)JGC_WP_REVEAL(jwp_bytes), data);
    }
    JGC_WP_RELEASE(jwp_bytes);
    ERRCHECK(ILUJAVA_VOID)
}


JAVAEXPORT(IluCall_inBytesS, JArrayOfByte)
    JIluCall jh_call, Jint limit
    ENDJAVAEXPORT
    /* Variable-length array of byte */
{
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    JArrayOfByte jh_bytes = 0;
    ilu_bytes data = 0;
    ilu_cardinal len;
    PROLOG(CALL_IN, 0)
    ilu_InputBytes(cCall, &data, 
        &len, (ilu_cardinal) limit, 
    	&ioerrs
    	);
    if (ILU_ERRNOK(ioerrs)) {
        if (data) ilu_free(data);
        _ilujava_IluErrorToException(&ioerrs, "IluCall.inBytesS"); 
        return 0; 
    }
    if (data) {
        jh_bytes = IluJava_JAoB_from8(JENV_ACTUAL (char *)data, len);
        ilu_free(data);
    }
    return jh_bytes;
}


JAVAEXPORT(IluCall_szBytesAx, Jint)
    JIluCall jh_call, 
    JArrayOfByte jh_bytes, 
    Jint start, Jint limit
    ENDJAVAEXPORT
    /* Fixed-length array of byte */
{
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    JGC_WP_TYPE(JArrayOfByte) jwp_bytes = JGC_WP_MAKE(jh_bytes);
    char *data;
    long dataLen;
    long cstart = (long) start;
    long climit = (long) limit;
    PROLOG_SZ()
    if (jh_bytes == 0) {
        if (climit>0) {
            _ilujava_NullPointerException("IluCall");
            JGC_WP_RELEASE(jwp_bytes);
            return 0;
        }
        dataLen = 0;
    } else {
        dataLen = ARRAY_LENGTH(JGC_WP_REVEAL(jwp_bytes));
    }
    if ((cstart>dataLen) || (climit>(dataLen-cstart))) {
        _ilujava_IluErrorToCallException(0, "szBytesAx: length exceeds limit");
        return 0;
    }
    if (dataLen) {
        data = (char *) ARRAY_byte_GET((JArrayOfByte)JGC_WP_REVEAL(jwp_bytes));
    } else {
        data = (char *) &nullBuffer;
    }
    sz = ilu_SizeOfOpaque(cCall, (ilu_opaque) data+cstart, 
    	(ilu_cardinal) climit,  
    	&ioerrs
    	);
    if (dataLen) {
        ARRAY_byte_RELEASE((JArrayOfByte)JGC_WP_REVEAL(jwp_bytes), data);
    }
    JGC_WP_RELEASE(jwp_bytes);
    ERRCHECK(0)
    return (Jint) sz;
}


JAVAEXPORT(IluCall_outBytesAx, void)
    JIluCall jh_call, 
    JArrayOfByte jh_bytes, 
    Jint start, Jint limit
    ENDJAVAEXPORT
    /* Fixed-length array of byte */
{
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    JGC_WP_TYPE(JArrayOfByte) jwp_bytes = JGC_WP_MAKE(jh_bytes);
    char *data;
    long dataLen;
    long cstart = (long) start;
    long climit = (long) limit;
    PROLOG(CALL_OUT, ILUJAVA_VOID)
    if (jh_bytes == 0) {
        if (climit>0) {
            _ilujava_NullPointerException("IluCall");
            return;
        }
        dataLen = 0;
    } else {
        dataLen = ARRAY_LENGTH(JGC_WP_REVEAL(jwp_bytes));
    }
    if ((cstart>dataLen) || (limit>(dataLen-cstart))) {
        _ilujava_IluErrorToCallException(0, "outBytesAx: length exceeds limit");
        JGC_WP_RELEASE(jwp_bytes);
        return;
    }
    if (dataLen) {
        data = (char *) ARRAY_byte_GET((JArrayOfByte)JGC_WP_REVEAL(jwp_bytes));
    } else {
        data = (char *) &nullBuffer;
    }
    ilu_OutputOpaque(cCall, (ilu_opaque) data+cstart, 
    	(ilu_cardinal) climit, 
    	&ioerrs
    	);
    if (dataLen) {
        ARRAY_byte_RELEASE((JArrayOfByte)JGC_WP_REVEAL(jwp_bytes), data);
    }
    JGC_WP_RELEASE(jwp_bytes);
    ERRCHECK(ILUJAVA_VOID)
}


JAVAEXPORT(IluCall_inBytesA, JArrayOfByte)
    JIluCall jh_call, Jint len
    ENDJAVAEXPORT
    /* Fixed-length array of byte */
{
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    JArrayOfByte jh_bytes;
    ilu_opaque data = 0;
    PROLOG(CALL_IN, 0)
    ilu_InputOpaque(cCall, &data, 
        (ilu_cardinal) len, &ioerrs
    	);
    if (ILU_ERRNOK(ioerrs)) {
        if (data) ilu_free(data);
        _ilujava_IluErrorToException(&ioerrs, "IluCall.inBytesA"); 
        return 0; 
    }
    jh_bytes = IluJava_JAoB_from8(JENV_ACTUAL (char *)data, len);
    ilu_free(data);
    return jh_bytes;
}


JAVAEXPORT(IluCall_szString16, Jint)
    JIluCall jh_call, 
    JString jh_string, 
    Jint limit
    ENDJAVAEXPORT
    /* Variable-length array of 16 bit character (WString) */
{
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    JGC_WP_TYPE(JString) jwp_string = JGC_WP_MAKE(jh_string);
    Jchar* char16Ptr;
    ilu_cardinal climit = limit;
    ilu_cardinal len;
    PROLOG_SZ()
    if (jh_string == 0) {
        len = 0;
        char16Ptr = (Jchar*) &nullBuffer;
    } else { 
        len = (ilu_cardinal) JSTRING_LENGTH((JString)JGC_WP_REVEAL(jwp_string));
        char16Ptr = (Jchar*) JSTRING_GETCHARPTR((JString)JGC_WP_REVEAL(jwp_string));
    }
    if ((len>climit) && (climit!=0)) {len = climit;}
    sz = ilu_SizeOfWString(cCall, (ilu_wstring) char16Ptr, 
    	(ilu_cardinal) len, (ilu_cardinal) climit, 
    	&ioerrs
    	);
    if (JGC_WP_REVEAL(jwp_string)) {
        JSTRING_RELEASECHARPTR((JString)JGC_WP_REVEAL(jwp_string), char16Ptr);
    }
    JGC_WP_RELEASE(jwp_string);
    ERRCHECK(0)
    return (Jint) sz;
}


JAVAEXPORT(IluCall_outString16x, void)
    JIluCall jh_call, 
    JString jh_string, 
    Jint limit
    ENDJAVAEXPORT
    /* Variable-length array of 16 bit character (WString) */
{
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    JGC_WP_TYPE(JString) jwp_string = JGC_WP_MAKE(jh_string);
    Jchar* char16Ptr;
    ilu_cardinal climit = limit;
    ilu_cardinal len;
    PROLOG(CALL_OUT, ILUJAVA_VOID)
    if (jh_string == 0) {
        len = 0;
        char16Ptr = (Jchar*) &nullBuffer;
    } else {
        len = (ilu_cardinal) JSTRING_LENGTH((JString)JGC_WP_REVEAL(jwp_string));
        char16Ptr = (Jchar*) JSTRING_GETCHARPTR((JString)JGC_WP_REVEAL(jwp_string));
    }
    if ((len>climit) && (climit!=0)) {len = climit;}
    ilu_OutputWString(cCall,  (ilu_wstring) char16Ptr, 
    	(ilu_cardinal) len, (ilu_cardinal) climit, 
    	&ioerrs
    	);
    if (JGC_WP_REVEAL(jwp_string)) {
        JSTRING_RELEASECHARPTR((JString)JGC_WP_REVEAL(jwp_string), char16Ptr);
    }
    JGC_WP_RELEASE(jwp_string);
    ERRCHECK(ILUJAVA_VOID)
}


JAVAEXPORT(IluCall_inString16AsArray, JArrayOfChar)
    JIluCall jh_call, Jint limit
    ENDJAVAEXPORT
    /* Variable-length array of character (WString) */
{
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    JArrayOfChar jja;
    ilu_wstring data = 0;
    ilu_cardinal len;
    PROLOG(CALL_IN, 0)
    ilu_InputWString(cCall, &data, 
        &len, (ilu_cardinal) limit, 
    	&ioerrs
    	);
    if (ILU_ERRNOK(ioerrs)) {
        if (data) ilu_free(data);
        _ilujava_IluErrorToException(&ioerrs, "IluCall.inString16"); 
        return 0; 
    }
    jja = IluJava_JAoC_from16(JENV_ACTUAL (char *)data, len);
    ilu_free(data);
    return jja;
}


JAVAEXPORT(IluCall_szChar8Array, Jint)
    JIluCall jh_call, JArrayOfChar jja, Jint lim
    ENDJAVAEXPORT
    /* Fix-length array of short character (StringVec) */
{
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    Jint len;
    char * cbuffer;
    PROLOG_SZ()
    if (jja == 0) {
        if (lim>0) {
            _ilujava_NullPointerException("IluCall");
            return 0;
        }
        len = 0;
        cbuffer = ilu_malloc(2); cbuffer[0] = 0;
    } else {
        len = ARRAY_LENGTH(jja);
        if (len<lim) {
            _ilujava_IluErrorToCallException(&ioerrs, "IluCall.szChar8Array");
            return 0;
        }
        cbuffer = ilu_malloc(len + 2 /* 0C + safety */ );
        IluJava_JAoC_toC8(JENV_ACTUAL jja, cbuffer, len);
    }
    sz = ilu_SizeOfStringVec(
        cCall, (ilu_string) cbuffer, (ilu_cardinal) lim, &ioerrs);
    ilu_free(cbuffer);
    ERRCHECK(0)
    return (Jint) sz;
} /*szChar8Array*/


JAVAEXPORT(IluCall_outChar8Arrayx, void)
    JIluCall jh_call, JArrayOfChar jja, Jint lim
    ENDJAVAEXPORT
{
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    char * cbuffer;
    Jint len; 
    PROLOG(CALL_OUT, ILUJAVA_VOID)
    if (jja == 0) {
        if (lim>0) {
            _ilujava_NullPointerException("IluCall");
            return;
        }
        len = 0;
        cbuffer = ilu_malloc(2); cbuffer[0] = 0;
    } else {
        len = ARRAY_LENGTH(jja);
        if (len<lim) {
            _ilujava_IluErrorToCallException(&ioerrs, "IluCall.outChar8Arrayx");
            return;
        }
        cbuffer = ilu_malloc(len + 2 /* 0C + safety */ );
        IluJava_JAoC_toC8(JENV_ACTUAL jja, cbuffer, len);
    }
    ilu_OutputStringVec(
        cCall, (ilu_string) cbuffer, (ilu_cardinal) lim, &ioerrs
        );
    ilu_free(cbuffer);
    ERRCHECK(ILUJAVA_VOID)
} /*outChar8Arrayx*/


JAVAEXPORT(IluCall_inChar8Array, JArrayOfChar)
    JIluCall jh_call, Jint lim
    ENDJAVAEXPORT
{
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    JArrayOfChar jja;
    char * cString;
    PROLOG(CALL_IN, 0)
    ilu_InputStringVec(cCall, &cString, (ilu_cardinal) lim, &ioerrs);
    if (ILU_ERRNOK(ioerrs)) {
        ilu_free(cString);
        _ilujava_IluErrorToException(&ioerrs, "IluCall.inChar8Array");
        return 0;
    }
    jja = IluJava_JAoC_from8(JENV_ACTUAL cString, lim);
    ilu_free(cString);
    return jja;
} /*inChar8Array*/


JAVAEXPORT(IluCall_szChar16Array, Jint)
    JIluCall jh_call, 
    JArrayOfChar jh_a, 
    Jint limit
    ENDJAVAEXPORT
    /* Fix-length array of character (WStringVec) */
{
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    JGC_WP_TYPE(JArrayOfChar) jwp_a = JGC_GET_WP_FROM_JAVA_OBJECT(jh_a);
    Jchar* char16Ptr;
    Jint len;
    PROLOG_SZ()
    if (jh_a == 0) {
        if (limit>0) {
            _ilujava_NullPointerException("IluCall");
            return 0;
        }
        len = 0;
        char16Ptr = (Jchar*) &nullBuffer;
    } else {
        len = ARRAY_LENGTH(JGC_WP_REVEAL(jwp_a));
        if (len<limit) {
            _ilujava_IluErrorToCallException(&ioerrs, "IluCall.szChar8Array");
            return 0;
        }
        char16Ptr = ARRAY_char_GET((JArrayOfChar)JGC_WP_REVEAL(jwp_a));
    }
    sz = ilu_SizeOfWStringVec(cCall, (ilu_wstring)char16Ptr, 
    	(ilu_cardinal)limit,  
    	&ioerrs
    	);
    if (JGC_WP_REVEAL(jwp_a)) {ARRAY_char_RELEASE((JArrayOfChar)JGC_WP_REVEAL(jwp_a), char16Ptr);}
    ERRCHECK(0)
        JGC_WP_RELEASE(jwp_a);
    return (Jint) sz;
} /*szChar16Array*/


JAVAEXPORT(IluCall_outChar16Arrayx, void)
    JIluCall jh_call, 
    JArrayOfChar jh_a, 
    Jint limit
    ENDJAVAEXPORT
    /* Fix-length array of character (WStringVec) */
{
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    JGC_WP_TYPE(JArrayOfChar) jwp_a = JGC_GET_WP_FROM_JAVA_OBJECT(jh_a);
    Jchar* char16Ptr;
    Jint len;
    PROLOG(CALL_OUT, ILUJAVA_VOID)
    if (jh_a == 0) {
        if (limit>0) {
            _ilujava_NullPointerException("IluCall");
            return;
        }
        len = 0;
        char16Ptr = (Jchar*) &nullBuffer;
    } else {
        len = ARRAY_LENGTH(JGC_WP_REVEAL(jwp_a));
        if (len<limit) {
            _ilujava_IluErrorToCallException(&ioerrs, "outChar16Arrayx");
            return;
        }
        char16Ptr = ARRAY_char_GET((JArrayOfChar)JGC_WP_REVEAL(jwp_a));
    }
    ilu_OutputWStringVec(cCall, (ilu_wstring)char16Ptr, 
    	(ilu_cardinal) limit, 
    	&ioerrs
    	);
    if (JGC_WP_REVEAL(jwp_a)) {
        ARRAY_char_RELEASE((JArrayOfChar)JGC_WP_REVEAL(jwp_a), char16Ptr);
    }
    JGC_WP_RELEASE(jwp_a);
    ERRCHECK(ILUJAVA_VOID)
} /*outChar16Array*/
 

JAVAEXPORT(IluCall_inChar16Array, JArrayOfChar) 
    JIluCall jh_call, Jint len
    ENDJAVAEXPORT
    /* Fix-length array of character (WStringVec) */
{
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    JArrayOfChar jja;
    ilu_wstring data = 0;
    PROLOG(CALL_IN, 0)
    ilu_InputWStringVec(cCall, &data, 
        (ilu_cardinal)len, &ioerrs
    	);
    if (ILU_ERRNOK(ioerrs)) {
        if (data) ilu_free(data);
        _ilujava_IluErrorToException(&ioerrs, "IluCall.inChar16Array"); 
        return 0; 
    }
    jja = IluJava_JAoC_from16(JENV_ACTUAL (char *)data, len);
    ilu_free(data);
    return jja;
} /*inChar16Array*/


JAVAEXPORT(IluCall_nativeMarkCallFailure, void)
    JIluCall jh_call, Jint protocolException
    ENDJAVAEXPORT
{
    ilu_Call cCall = GET_IluCall_yCall(jh_call);
    if (cCall) {
        if (cCall->ca_pe == ilu_ProtocolException_Success) {
            cCall->ca_pe = (ilu_ProtocolException) protocolException;
        }
    }
} /*nativeMarkCallFailure*/


/* end */
