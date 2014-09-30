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
/* IluJava_IluOInt.c */ 
/* Chris Jacobi, November 25, 1998 5:41 pm PST */

 
/* 
 * $Id: IluJava_IluOInt.c,v 1.57 1999/08/03 01:54:21 janssen Exp $ 
 *
 *
 * This class helps in the mapping from real Java objects to kernel object 
 * Native side for IluOInt.java
 */

 
#include "IluJava_Includes.h"
#include "IluJava_Common.h"
#include "IluJava_JGC.h"
#include "IluJava_JOps.h"

#include "IluJava_JStubs.h"

#include "IluJava_JTypes.h"
#include <iluhash.h>
#include "IluJava_Ops.h"

#include "IluJava_JMon.h"


static ILUJAVA_JMON_PTR iluoint_LOCK; 
    /* enter this lock after the server lock */

static JGC_GLOBALOBJ_DECL(jsp_iluOIntProto);

JAVAEXPORT(IluOInt_reportIluOIntInst, Void)
	JIluOInt jh_oi
	ENDJAVAEXPORT 
    /* 
     * Report the prototipical instance.
     */
{
    JGC_GLOBALOBJ_ASSIGNTO(jsp_iluOIntProto, jh_oi);
    JTRACE(_ilujava_gcFlag>0, ("$ IluOInt reportIluOIntInst: j<%x> \n", jh_oi));
} /*IluOInt_reportIluOIntInst*/


EXPORTLIMITED ilu_refany _ilujava_newIluOInt (JENV_FORMAL_NOCOMMA)
{
    ilu_refany x;
    JIluOInt jh_proto;
    JGC_GLOBALOBJ_GETFROM(jh_proto, jsp_iluOIntProto)
    x = (ilu_refany) JCALL_IluOInt_allocateOI(jh_proto);
    return x;
} /*_ilujava_newIluOInt*/


 
#define INTERNAL_EXC "org/omg/CORBA/INTERNAL"

EXPORTLIMITED void _ilujava_throwException (JENV_FORMAL int cde, char* clzz, char* msg)
{
  #ifdef ILUJAVA_H_MINORVERSION_EQUALS_2_BUT_DONT
    /* jdk-1.2beta4 always failed raising ilu exceptions with SignalError */
    JString jmsg = 0;
    JString jclzz = 0;
    JIluOInt jh_proto;
    if (cde>0 && cde<10) {
        /* exceptions from primordial classes did work correctly however 
         */
        if (msg==0) {msg = "";}
        SignalError(EE(), clzz, msg);
        return;
    }
    if (msg) {
        jmsg = IluJava_JString_fromA0(JENV_ACTUAL msg);
    }
    if (clzz) {
        jclzz = IluJava_JString_fromA0(JENV_ACTUAL clzz);
    }
    JGC_GLOBALOBJ_GETFROM(jh_proto, jsp_iluOIntProto)
    JCALL_IluOInt_throwThisException(jh_proto, (Jint) cde, jclzz, jmsg);
 #endif
 #ifdef ORIGINAL_VERSION_BUT_DONT
     /* this always worked fine with jdk-1.1 */
     if (msg==0) {msg = "";}
     SignalError(EE(), clzz, msg);
 #endif
    /* now always use the common slow path; its easier to debug... */
    JString jmsg = 0;
    JString jclzz = 0;
    JIluOInt jh_proto;
    if (msg) {
        jmsg = IluJava_JString_fromA0(JENV_ACTUAL msg);
    }
    if (clzz) {
        jclzz = IluJava_JString_fromA0(JENV_ACTUAL clzz);
    }
    JGC_GLOBALOBJ_GETFROM(jh_proto, jsp_iluOIntProto)
    JCALL_IluOInt_throwThisException(jh_proto, (Jint) cde, jclzz, jmsg);
} /* _ilujava_throwException */


/* Assigns values to kinfo* and returns kernel object from an IluOInt
 *
 * before: not Inside (kinfo->cServer, kinfo->cClass)
 * after:  return != ILU_NIL => Inside(kinfo->cServer, kinfo->cClass)
 * after:  return == ILU_NIL => not Inside (kinfo->cServer, kinfo->cClass)
 * gc: may move objects
 */
EXPORTLIMITED ilu_Object 
_ilujava_getSetCIluObject(JENV_FORMAL
    JIluOInt jh_oi, 
    KInfo* kinfo, 
    ilu_boolean raiseErrs
    )
{
    JGC_WP_TYPE(JIluOInt) jwp_oi = JGC_GET_WP_FROM_IluOInt(jh_oi);
    JIluClassRep jh_class = GET_IluOInt_jjClassRep(jh_oi);
    if (jh_class) {
        kinfo->cClass = GET_IluClassRep_yIluClass(jh_class);
    } else {
        kinfo->cClass = ilu_rootClass;
    }
    if (kinfo->cClass == 0) {
        kinfo->cIluObject = 0;
        if (raiseErrs) {
            _ilujava_throwException(JENV_ACTUAL 10, INTERNAL_EXC, 
                "no ilu_Class");
        }
        return 0;
    }
    kinfo->cServer = _ilujava_EnterServer(JENV_ACTUAL jh_oi, kinfo->cClass);
    /* gc may move objects */
    if (kinfo->cServer == 0) {
        kinfo->cIluObject = 0;
        if (raiseErrs) {
            _ilujava_throwException(JENV_ACTUAL 10, INTERNAL_EXC, 
                "no ilu_Server");
        }
        return 0;
    }
    kinfo->cIluObject = GET_IluOInt_yIluKernelObject(JGC_WP_REVEAL(jwp_oi));
    if (kinfo->cIluObject == 0) {
        ilu_ExitServer(kinfo->cServer, kinfo->cClass);
        if (raiseErrs) {
            _ilujava_throwException(JENV_ACTUAL 10, INTERNAL_EXC, 
                "no ilu kernel object");
        }
        return 0;
    }
    return kinfo->cIluObject;
} /*_ilujava_getSetCIluObject */



JAVAEXPORT(IluOInt_nDestroyRudeOI, void)
    JIluOInt jh_oi
    ENDJAVAEXPORT
{
    JGC_WP_TYPE(JIluOInt) jwp_oi = JGC_GET_WP_FROM_IluOInt(jh_oi);
    JIluClassRep jh_class = 0;		  
    ilu_Class cClass = 0;
    ilu_Server cServer;
    ilu_Error err = ILU_INIT_NO_ERR;
     
    jh_class = GET_IluOInt_jjClassRep(jh_oi);
    if (jh_class) {
         cClass = GET_IluClassRep_yIluClass(jh_class);
    }
    if (cClass == 0) {
         /* not raising error in case of race condition in
          * multi thread destruction; we haven't entered monitor yet
          */
         JTRACE(_ilujava_gcFlag>2, 
             ("$ nDestroyRudeOI j<%x> already destroyed\n", 
             JGC_WP_REVEAL(jwp_oi)));
         return;
    }
     
    JTRACE(_ilujava_gcFlag>0, ("$ nDestroyRudeOI enter j<%x> \n", 
             JGC_WP_REVEAL(jwp_oi)));
    if (GET_IluOInt_destroyed(JGC_WP_REVEAL(jwp_oi))) {
         JTRACE(_ilujava_gcFlag>2, 
             ("$ nDestroyRudeOI was previously destroyed \n"));
         return;
    }
    cServer = _ilujava_EnterServerDisable(JENV_ACTUAL
         (JIluOInt) JGC_WP_REVEAL(jwp_oi), cClass);
    if (cServer) {
         ilu_Object cIluObject;
         ILUJAVA_MON_ENTER(iluoint_LOCK);
         jh_oi = (JIluOInt) JGC_WP_REVEAL(jwp_oi);
         cIluObject = GET_IluOInt_yIluKernelObject(jh_oi);
         PUT_IluOInt_destroyed(jh_oi, 1);
         PUT_IluOInt_yIluKernelObject(jh_oi, 0);
         ILUJAVA_MON_EXIT(iluoint_LOCK);
         if (cIluObject) {
             JTRACE(_ilujava_gcFlag>0, ("$ nDestroyRudeOI doit j<%x> c<%x> \n",
                     JGC_WP_REVEAL(jwp_oi), cIluObject));
             ilu_RegisterLSO(cIluObject, cClass, 0, JAVALANGIDX, &err);
         }
         ilu_ExitServer(cServer, cClass);
    }
    JTRACE(_ilujava_gcFlag>2, ("$ nDestroyRudeOI exit j<%x> \n", 
         JGC_WP_REVEAL(jwp_oi)));
    if (ILU_ERRNOK(err)) {
        _ilujava_IluErrorToCallException(&err, "nDestroyRudeOI");
    }
} /* IluOInt_nDestroyRudeOI */



JAVAEXPORT(IluOInt_destroyGentleOI, Jboolean) 
	JIluOInt jh_oi
	ENDJAVAEXPORT 
{
     JGC_WP_TYPE(JIluOInt) jwp_oi = JGC_GET_WP_FROM_IluOInt(jh_oi);
     JIluClassRep jh_class = GET_IluOInt_jjClassRep(jh_oi);
     ilu_Class cClass = GET_IluClassRep_yIluClass(jh_class);
     ilu_Server cServer = 0;
     ilu_Object cIluObject;
     JTRACE(_ilujava_gcFlag>0,  ("$ destroyGentleOI: enter j<%x> \n", 
             JGC_WP_REVEAL(jwp_oi)));
     if (GET_IluOInt_yServer(JGC_WP_REVEAL(jwp_oi)) == 0) {
         JTRACE(_ilujava_gcFlag>0,  ("$ destroyGentleOI: j<%x> no server A \n", 
                 JGC_WP_REVEAL(jwp_oi)));   
         return 0; /*don't re-finalize*/
     }
     jh_oi = (JIluOInt) JGC_WP_REVEAL(jwp_oi);
     if (GET_IluOInt_veryInterest(jh_oi) ||  
             GET_IluOInt_retained(jh_oi) || 
             GET_IluOInt_ghost(jh_oi)) {
         /* Not monitored, must be conservative */
         JTRACE(_ilujava_gcFlag>0,  
               ("$ destroyGentleOI: re-finalize A j<%x> \n", 
                   JGC_WP_REVEAL(jwp_oi)));   
         return 1; /*re-finalize*/
     }
     /* use the server lock */  
     cServer = _ilujava_EnterServer(JENV_ACTUAL
         (JIluOInt)JGC_WP_REVEAL(jwp_oi), cClass);
     if (cServer == 0) {
         JTRACE(_ilujava_gcFlag>0, ("$ destroyGentleOI: j<%x> no server B \n", 
                 JGC_WP_REVEAL(jwp_oi)));   
         return 0; /*don't re-finalize*/
     }
     cIluObject = GET_IluOInt_yIluKernelObject(JGC_WP_REVEAL(jwp_oi));
     if (cIluObject != 0) {
         if (ilu_VeryInterested(cIluObject) || GET_IluOInt_ghost(JGC_WP_REVEAL(jwp_oi))) {
             ilu_ExitServer(cServer, cClass);
             JTRACE(_ilujava_gcFlag>0, 
                 ("$ destroyGentleOI: re-finalize B j<%x> \n", 
                     JGC_WP_REVEAL(jwp_oi)));   
             return 1; /*re-finalize*/
         }
     }
    ILUJAVA_MON_ENTER(iluoint_LOCK);
    if (GET_IluOInt_veryInterest(JGC_WP_REVEAL(jwp_oi)) ||  
            GET_IluOInt_retained(JGC_WP_REVEAL(jwp_oi)) ||  
            GET_IluOInt_ghost(JGC_WP_REVEAL(jwp_oi))) {
        ILUJAVA_MON_EXIT(iluoint_LOCK);
        ilu_ExitServer(cServer, cClass);
        JTRACE(_ilujava_gcFlag>0,  ("$ destroyGentleOI: re-finalize C j<%x> \n", 
                 JGC_WP_REVEAL(jwp_oi)));   
        return 1; /*re-finalize*/
     }
     ILUJAVA_MON_EXIT(iluoint_LOCK);
     ilu_ExitServer(cServer, cClass);
     /* Exit the locks and re-enter them in nDestroyRudeOI
      * This is necessary to null-out the yServer field with the
      * global server lock held again.
      */
     CALLJAVAEXPORT(IluOInt_nDestroyRudeOI)
         (JIluOInt)JGC_WP_REVEAL(jwp_oi)
         ENDCALLJAVAEXPORT;
     return 0; /*don't re-finalize*/
} /* IluOInt_destroyGentleOI */


JAVAEXPORT(IluOInt_withdrawOI, void)
    JIluOInt jh_oi
    ENDJAVAEXPORT
{
    JGC_WP_TYPE(JIluOInt) jwp_oi = JGC_GET_WP_FROM_IluOInt(jh_oi);
    KInfo kinfo;
    char * ckey;
    ilu_boolean huh;
    _ilujava_getSetCIluObject(JENV_ACTUAL jh_oi, &kinfo, ilu_TRUE);
    if (kinfo.cIluObject) {
         ckey = GET_IluOInt_yOwnerKey(JGC_WP_REVEAL(jwp_oi));
         if (ckey) {
             PUT_IluOInt_yOwnerKey(JGC_WP_REVEAL(jwp_oi), 0);
             huh = ilu_WithdrawObject(kinfo.cIluObject, ckey);
         } else {
             ilu_ExitServer(kinfo.cServer, kinfo.cClass);
         }
    }
} /*IluOInt_withdrawOI*/


JAVAEXPORT(IluOInt_publishOI, void)
    JIluOInt jh_oi
    ENDJAVAEXPORT
{
    JGC_WP_TYPE(JIluOInt) jwp_oi = JGC_GET_WP_FROM_IluOInt(jh_oi);
    KInfo kinfo;
    char * ckey;
    _ilujava_getSetCIluObject(JENV_ACTUAL jh_oi, &kinfo, ilu_TRUE);
    if (kinfo.cIluObject) {
        ckey = GET_IluOInt_yOwnerKey(JGC_WP_REVEAL(jwp_oi));
        if (ckey) {
            ilu_ExitServer(kinfo.cServer, kinfo.cClass);
            _ilujava_throwException(JENV_ACTUAL 10, INTERNAL_EXC, "xxx-204");
            return;
        }
        ckey = ilu_PublishObject(kinfo.cIluObject);
        PUT_IluOInt_yOwnerKey(JGC_WP_REVEAL(jwp_oi), ckey);
    }
} /*IluOInt_publishOI*/


/* called within server lock */
static ilu_boolean 
_ilujava_noter(ilu_Object cobj, int vi)
{
    JGC_WP_TYPE(JIluOInt) jwp_oi = 
        ilu_GetLanguageSpecificObject(cobj, JAVALANGIDX);

    JTRACE(_ilujava_gcFlag>0, ("$ IluOInt noter: %d j<%x> c<%x> \n", vi, 
            JGC_WP_REVEAL(jwp_oi), cobj));

    if (JGC_WP_REVEAL(jwp_oi) != ILU_NIL && ilu_TrueInstanceP(cobj)) {
        ilu_Class cl = ilu_ClassOfObject(cobj);
        JTRACE(_ilujava_gcFlag>1, ("$ IluOInt noter: obj-true\n"));
        if ( ilu_CollectibleP(cl)) {
            JENV_DECLARE_INITFROMTHINAIR
            if (vi) {
                JTRACE(_ilujava_gcFlag>1, ("$ IluOInt noter: showInterest\n"));
                JCALL_IluOInt_showInterest(JGC_WP_REVEAL(jwp_oi));
                /* no java exceptions */
            } else {
                JTRACE(_ilujava_gcFlag>1, ("$ IluOInt noter: remInterest\n"));
                JCALL_IluOInt_removeInterest(JGC_WP_REVEAL(jwp_oi));
                /* no java exceptions */
            }
        }
    }
    return ilu_TRUE;
} /*_ilujava_noter*/				


EXPORTLIMITED void
_ilujava_IluOIntInit()
{
    JENV_DECLARE_INITFROMTHINAIR
    iluoint_LOCK = ILUJAVA_MON_ALLOC();
    ilu_SetNoter(_ilujava_noter, JAVALANGIDX);
} /*_ilujava_IluOIntInit*/



JAVAEXPORT(IluOInt_sbhOfOI, JString)
    JIluOInt jh_oi
    ENDJAVAEXPORT
{
    KInfo kinfo;
    JString jsbh = 0;
    char * csbh;
    _ilujava_getSetCIluObject(JENV_ACTUAL jh_oi, &kinfo, ilu_TRUE);
    if (kinfo.cIluObject) {
        csbh = ilu_SBHOfObject(kinfo.cIluObject);
        if (csbh) {
            jsbh = IluJava_JString_fromA0(JENV_ACTUAL csbh);
        }
        ilu_ExitServer(kinfo.cServer, kinfo.cClass);
    }
    return jsbh;
} /*IluOInt_sbhOfOI*/


JAVAEXPORT(IluOInt_iorOfOI, JString)
    JIluOInt jh_oi
    ENDJAVAEXPORT
{
    JString jsbh = 0;
#ifdef IIOP_PROTOCOL
    ilu_Error err = ILU_INIT_NO_ERR;
    KInfo kinfo;
    char * csbh;
    _ilujava_getSetCIluObject(JENV_ACTUAL jh_oi, &kinfo, ilu_TRUE);
    if (kinfo.cIluObject) {
        csbh = ilu_IOROfObject(kinfo.cIluObject, &err);
        if (csbh) {
            jsbh = IluJava_JString_fromA0(JENV_ACTUAL csbh);
        }
        ilu_ExitServer(kinfo.cServer, kinfo.cClass);
    }
    if (ILU_ERRNOK(err)) {
        _ilujava_IluErrorToCallException(&err, "iorOfOI");
    }
#endif /* IIOP_PROTOCOL */
    return jsbh;
} /*IluOInt_iorOfOI*/



JAVAEXPORT(IluOInt_nativePingOI, void)
    JIluOInt jh_oi
    ENDJAVAEXPORT
{
    KInfo kinfo;
    _ilujava_getSetCIluObject(JENV_ACTUAL jh_oi, &kinfo, ilu_TRUE);
    if (kinfo.cIluObject) {
        ilu_Error err = ILU_INIT_NO_ERR;
        ilu_boolean ok;
        ilu_Connection newConnection;
        err = ilu_DeltaHolds(kinfo.cIluObject, 1); 
                /* as side effect also helds server */
        ilu_ExitServer(kinfo.cServer, kinfo.cClass);
        if (ILU_ERRNOK(err)) {
            _ilujava_IluErrorToCallException(&err, "ping 0");
            return;
        }
        ok = ilu_PingObject(kinfo.cIluObject, &newConnection);
        if (newConnection) {
            /* need to monitor outgoing connection */
            _ilujava_forkConnectionHandler(JENV_ACTUAL newConnection);
            /* enables java gc */
        }
        ilu_EnterServer(kinfo.cServer, kinfo.cClass);
        err = ilu_DeltaHolds(kinfo.cIluObject, -1);
        ilu_ExitServer(kinfo.cServer, kinfo.cClass);
        if (ILU_ERRNOK(err)) {
            _ilujava_IluErrorToCallException(&err, "ping 1");
        } else if (!ok) {
            _ilujava_IluErrorToCallException(0, "ping 2");
        }
    }
} /*IluOInt_nativePingOI*/



JAVAEXPORT(IluOInt_nativeURLOfObject, JString)
    JIluOInt jh_oi
    ENDJAVAEXPORT
{
    JString jh_url = 0;
  #if defined(HTTP_PROTOCOL) 
    ilu_Error err = ILU_INIT_NO_ERR;
    char * c_url = 0;
    KInfo kinfo;
    _ilujava_getSetCIluObject(JENV_ACTUAL jh_oi, &kinfo, ilu_FALSE);
    if (kinfo.cIluObject) {
        c_url = ilu_URLOfObject(kinfo.cIluObject, &err);
        ilu_ExitServer(kinfo.cServer, kinfo.cClass);
    }
    if (ILU_ERRNOK(err)) {
        _ilujava_IluErrorToCallException(&err, "URLOfObject");
    }
    if (c_url) {
        jh_url = IluJava_JString_fromA0(JENV_ACTUAL c_url);
        ilu_free(c_url);
    }
  #endif /* HTTP_PROTOCOL */
    return jh_url;
} /*IluOInt_nativeURLOfObject*/


/* end */



