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
/* IluJava_IluServer.c */
/* Chris Jacobi, November 20, 1998 1:40 pm PST */

/*
 */
 
/* $Id: IluJava_IluServer.c,v 1.35 1999/08/03 01:54:22 janssen Exp $ */
 
/* 
 * C side for IluServer.java 
 * See also IluObjectTable.java
 */

#include "IluJava_Includes.h"
#include "IluJava_Common.h"
#include "IluJava_JGC.h"
#include "IluJava_JOps.h"
#include "xerox_ilu_IluServerRelocationInfo.h"


#include <iluerror.h>
/* for debugging only */
#include <iluntrnl.h>

#include "IluJava_JStubs.h"
#include "IluJava_JMon.h"

/* The Global server lock protects existence of ilu_server
 * before an ilu_server is used for locking operations..
 * Enter JGServerlock before entering server locks.
 */
static ILUJAVA_JMON_PTR gServerLock = 0;
#define ENTER_GSERVER_LOCK() ILUJAVA_MON_ENTER(gServerLock)
#define EXIT_GSERVER_LOCK() ILUJAVA_MON_EXIT(gServerLock)


static JGC_GLOBALOBJ_DECL(jsp_iluServerProto);

JAVAEXPORT(IluServer_reportIluServerInst, Void)
	JIluServer jh_s
	ENDJAVAEXPORT 
    /* 
     * Report the prototipical instance.
     */
{
    JGC_GLOBALOBJ_ASSIGNTO(jsp_iluServerProto, jh_s);
}


EXPORTLIMITED void _ilujava_IluServerInit()
{
    JENV_DECLARE_INITFROMTHINAIR
    gServerLock = ILUJAVA_MON_ALLOC();
}


EXPORTLIMITED ilu_Server
_ilujava_EnterServer(JENV_FORMAL JIluOInt jh_oi, ilu_Class cActualClass)
/* return 0: no lock entered
 * return ilu_Server: server lock entered
 */
{
    ilu_Server cServer;
 	JGC_WP_TYPE(JIluOInt) jwp_oi = JGC_GET_WP_FROM_JAVA_OBJECT(jh_oi);
    ENTER_GSERVER_LOCK();
    cServer = GET_IluOInt_yServer(JGC_WP_REVEAL(jwp_oi)); 
    if (cServer) {
        ilu_EnterServer(cServer, cActualClass);
    }
    EXIT_GSERVER_LOCK();
    return cServer;
}


EXPORTLIMITED ilu_Server
_ilujava_EnterServer2(
    JENV_FORMAL JIluServer jh_IluServer, ilu_Class cActualClass
    )
/* return 0: no lock entered
 * return ilu_Server: server lock entered
 */
{
    JGC_WP_TYPE(JIluServer) jwp_IluServer = 
        JGC_GET_WP_FROM_JAVA_OBJECT(jh_IluServer);
    ilu_Server cServer;
    ENTER_GSERVER_LOCK();
    cServer = GET_IluServer_yIluServer(JGC_WP_REVEAL(jwp_IluServer)); 
    if (cServer) {
        ilu_EnterServer(cServer, cActualClass);
    }
    EXIT_GSERVER_LOCK();
    return cServer;
}


EXPORTLIMITED ilu_Server
_ilujava_EnterServerDisable(JENV_FORMAL JIluOInt jh_oi, ilu_Class cActualClass)
/* return 0: no lock entered
 * return ilu_Server: server lock entered
 */
{
    JGC_WP_TYPE(JIluOInt) jwp_oi = JGC_GET_WP_FROM_JAVA_OBJECT(jh_oi);
    ilu_Server cServer;
    ENTER_GSERVER_LOCK();
    cServer = GET_IluOInt_yServer(JGC_WP_REVEAL(jwp_oi)); 
    if (cServer) {
        ilu_EnterServer(cServer, cActualClass);
        PUT_IluOInt_yServer(JGC_WP_REVEAL(jwp_oi), 0);
    }
    EXIT_GSERVER_LOCK();
    return cServer;
}


/* enters and exits the server lock */
EXPORTLIMITED ilu_Server
_ilujava_ServerEnterHolds(JENV_FORMAL JIluOInt jh_oi)
/* increases holds on server with appropriate locks.
 * if returns 0: no holds changed
 * if returns ilu_Server: server enter+exit and holds changed
 */
{
    JGC_WP_TYPE(JIluOInt) jwp_oi = JGC_GET_WP_FROM_JAVA_OBJECT(jh_oi);
    ILU_ERRS((bad_locks, internal/broken)) err = ILU_INIT_NO_ERR;
    ilu_Server cServer;
    ENTER_GSERVER_LOCK();
    cServer = GET_IluOInt_yServer(JGC_WP_REVEAL(jwp_oi)); 
    if (cServer) {
        ilu_EnterServer(cServer, ilu_rootClass);
        ilu_DeltaServerHolds(cServer, 1, &err);
        ilu_ExitServer(cServer, ilu_rootClass);
    }
    EXIT_GSERVER_LOCK();
    ILU_MUST_BE_SUCCESS(err);
    return cServer;
}


/* enters and exits the server lock */
EXPORTLIMITED void
_ilujava_ServerExitHolds(JENV_FORMAL ilu_Server cServer)
/* decreases holds on server with appropriate locks.
 */
{
    ILU_ERRS((bad_locks, internal/broken)) err= ILU_INIT_NO_ERR;
    if (cServer) {
        ilu_EnterServer(cServer, ilu_rootClass);
        ilu_DeltaServerHolds(cServer, -1, &err);
        ilu_ExitServer(cServer, ilu_rootClass);
        ILU_MUST_BE_SUCCESS(err);
    }
}


/* L1 >= {server}; L2, Main unconstrained */
INTERNALONLY ilu_Object
_ilujava_ObjectTableCreateObj(ilu_ObjectTable cot, ilu_string cih)
/* Upcalled by kernel to request creation of an object */
{
    JENV_DECLARE_INITFROMTHINAIR
    JGC_WP_TYPE(JIluOInt) jwp_oi;
    JGC_WP_TYPE(JIluServer) jwp_IluServer;
    JGC_WP_TYPE(JIluObjectTable) jwp_ObjTab;
    JString jh_ih = 0; /* not used after GC enabled */
    JIluClassRep jjIluClass; /* not used before GC disabld */
    ilu_Server cServer;
    ilu_Class cClass;
    ilu_Object cObject;
    jwp_ObjTab = (JGC_WP_TYPE(JIluObjectTable)) cot->ot_rock;
    jwp_IluServer = JGC_GET_WP_FROM_JAVA_OBJECT(
        GET_IluObjectTable_jjServer(JGC_WP_REVEAL(jwp_ObjTab))
        );
    JTRACE(_ilujava_objectsFlag>1,
        ("$ ObjectTableCreateObj for %s jjServer %x\n", 
            cih, JGC_WP_REVEAL(jwp_IluServer)));
    jh_ih = IluJava_JString_fromA0(JENV_ACTUAL (char *) cih);
    jwp_oi = JGC_GET_WP_FROM_JAVA_OBJECT( (JIluOInt) 
        JCALL_IluObjectTable_doCreateTrueObject(
            JGC_WP_REVEAL(jwp_ObjTab), jh_ih)
        );
    if (JPENDINGEXCEPTION()) {
        JCLEAREXCEPTION();
        return 0;
    }
    JTRACE(_ilujava_objectsFlag>1,
        ("$ ObjectTableCreateObj jjoi=<%x>\n", 
            JGC_WP_REVEAL(jwp_oi)));
    /* conceptionally similar (locking would be wrong) to 
     * xerox_ilu_IluRT0_registerTrue(0, ac.jjoi, jh_ih, ac.jjServer);
     */
    if (JGC_WP_REVEAL(jwp_oi)) {
        jjIluClass = GET_IluOInt_jjClassRep(JGC_WP_REVEAL(jwp_oi));
        cClass = GET_IluClassRep_yIluClass(jjIluClass);
        cServer = GET_IluServer_yIluServer(JGC_WP_REVEAL(jwp_IluServer)); 
            /* hold lock */
        PUT_IluOInt_yServer(JGC_WP_REVEAL(jwp_oi), cServer); /* hold lock */
        JTRACE(_ilujava_objectsFlag>1,
            ("$ ObjectTableCreateObj before FOCTrueObject cServer=<%x>\n", 
                cServer));
        cObject = ilu_FindOrCreateTrueObject(
            cih, cServer, cClass, JGC_WP_REVEAL(jwp_oi)
            );
        PUT_IluOInt_yIluKernelObject(JGC_WP_REVEAL(jwp_oi), cObject);
        JTRACE(_ilujava_objectsFlag>1,
            ("$ ObjectTableCreateObj cObject=<%x>\n", cObject));
        return cObject;
    }
    return 0;
}


INTERNALONLY void
_ilujava_ObjectTableFreeSelf(ilu_ObjectTable cot)
/* Upcalled by kernel to request deletion of table */
{
    JENV_DECLARE_INITFROMTHINAIR
    JObject ot = JGC_WP_REVEAL((JGC_WP_TYPE(JObject)) cot->ot_rock);
    JGC_WP_RELEASE((JGC_WP_TYPE(JObject)) cot->ot_rock);
    JCALL_IluObjectTable_doObjectTableFreed(ot);
    ilu_free(cot);
}


INTERNALONLY ilu_ObjectTable _ilujava_createOT(
	JIluObjectTable jjObjTab
	)
/* Create the native side of an object table */
{
    JENV_DECLARE_INITFROMTHINAIR
    ilu_ObjectTable cot;
    if (jjObjTab==0) return 0;
    cot = ilu_must_malloc(sizeof(ilu_ObjectTable_s));
    cot->ot_object_of_ih = _ilujava_ObjectTableCreateObj;
    cot->ot_free_self = _ilujava_ObjectTableFreeSelf;
    cot->ot_rock = (ilu_private) JGC_WP_MAKE(jjObjTab); 
            /*not GC'd: server references it*/
    return cot;
}


static void
  ServerRelocateShim (ilu_Server cserver,
		      ilu_private rock,
		      ilu_Error *err)
{
    JENV_DECLARE_INITFROMTHINAIR
    char * cpInfo;
    ilu_TransportInfo c_transportInfo;
    JIluTransportInfo jh_transportInfo; 
    JString jh_pInfo;
    JGC_WP_TYPE(JIluServer) jwp_IluServer = (JGC_WP_TYPE(JIluServer)) rock;
    JIluServerRelocationInfo jh_sri;
    jh_sri = (JIluServerRelocationInfo) 
         JCALL_IluServer_mustCheckRelocate(JGC_WP_REVEAL(jwp_IluServer));
    if (jh_sri) {
        /* Now don't enable any garbage collection while we keep working 
         * on jh_sri
         */
        jh_transportInfo = 
            GET_IluServerRelocationInfo_jjSaveTransportInfo(jh_sri);
        c_transportInfo = 
            GET_IluTransportInfo_yIluTransportInfo(jh_transportInfo);
        jh_pInfo = GET_IluServerRelocationInfo_jjSaveProtocolInfo(jh_sri);
        cpInfo = IluJava_JString_toheap80(JENV_ACTUAL jh_pInfo);
        /* Stop using java handles as this might have done fancy stuff 
         * enabling gc..
         */
 ILU_ERR_CONS3(relocate,err,rel_scope,ilu_relocate_conn,rel_pinfo,cpInfo,rel_tinfo,c_transportInfo,0);
        ilu_free(cpInfo);
    } else {
        ILU_CLER(*err);
    }
} 
 
JAVAEXPORT(IluServer_nativeOfCreateServer, void) 
	JIluServer jh_IluServer, 
	JString jjID
	ENDJAVAEXPORT    
{
    JGC_WP_TYPE(JIluServer) jwp_IluServer =
        JGC_GET_WP_FROM_JAVA_OBJECT(jh_IluServer);
    ILU_ERRS((bad_locks, broken_locks, bad_param, no_memory, internal)) 
        err = ILU_INIT_NO_ERR;
    ilu_ObjectTable cObjtab = 0; 
    ilu_Server cIluServer;
    char * cId ; 
    JIluObjectTable jjObjTab;
    cId = IluJava_JString_toheap8x(JENV_ACTUAL jjID);
    jjObjTab = GET_IluServer_jjObjTab(JGC_WP_REVEAL(jwp_IluServer));
    if (jjObjTab) {
        cObjtab = _ilujava_createOT(jjObjTab);
    }
    cIluServer = ilu_CreateTrueServer(cId, cObjtab, JAVALANGIDX, &err);
    if (cIluServer==0) {
        ILU_HANDLED(err);
        PRINTRAISE("$ Couldn't create true server");
        return;
    }
    ILU_MUST_BE_SUCCESS(err);
    PUT_IluServer_yIluServer(JGC_WP_REVEAL(jwp_IluServer), cIluServer);
    PUT_IluServer_yPtr(JGC_WP_REVEAL(jwp_IluServer), cId);
    (void) ilu_SetLSS(cIluServer, jwp_IluServer, JAVALANGIDX, &err);
        /* NOTE: Use the WEAK ptr for LSS */
    ILU_MUST_BE_SUCCESS(err);
    if (GET_IluServer_jjRelocatonInfo(JGC_WP_REVEAL(jwp_IluServer))) {
        ILU_CLER(err);
        ilu_SetServerRelocateProc(
            cIluServer, ServerRelocateShim, (ilu_private) jwp_IluServer, &err);
        ILU_MUST_BE_SUCCESS(err);
    }
    ilu_ExitServer(cIluServer, ilu_rootClass);
    JTRACE(_ilujava_pctFlag>0,
        ("$ IluServer_nativeOfCreateServer: true server creat j<%x> c<%x>\n", 
            JGC_WP_REVEAL(jwp_IluServer), 
            cIluServer));
} /*nativeOfCreateServer*/


JAVAEXPORT(IluServer_nativeOfDestroyServer, void) 
    JIluServer jh_IluServer
    ENDJAVAEXPORT
{
    JGC_WP_TYPE(JIluServer) jwp_IluServer = 
        JGC_GET_WP_FROM_JAVA_OBJECT(jh_IluServer);
    ILU_ERRS((bad_param, internal)) err = ILU_INIT_NO_ERR;
    ilu_Server cIluServer;
    JTRACE(_ilujava_pctFlag>0, ("$ IluServer_nativeOfDestroyServer; j<%x>\n", 
            JGC_WP_REVEAL(jwp_IluServer)));
    ENTER_GSERVER_LOCK();
    cIluServer = GET_IluServer_yIluServer(JGC_WP_REVEAL(jwp_IluServer));
    PUT_IluServer_yIluServer(JGC_WP_REVEAL(jwp_IluServer), 0);
    if (cIluServer) {
        ilu_EnterServer(cIluServer, ilu_rootClass);
    }
    EXIT_GSERVER_LOCK();
    if (cIluServer) {
        ilu_InnerBankServer(cIluServer);
        ilu_SetLSS(cIluServer, 0, JAVALANGIDX, &err);
        ILU_MUST_BE_SUCCESS(err);
        /* note that we are not freeing the SetServerRelocateProc
         * we don't have to as it didn't allocate any resource...
         */
        ilu_ExitServer(cIluServer, ilu_rootClass);
    }
} /*nativeOfDestroyServer*/


JAVAEXPORT(IluServer_nativeOfFreeServer, void) 
    JIluServer jjIluServer
    ENDJAVAEXPORT
{
    char * cId = GET_IluServer_yPtr(jjIluServer);
    PUT_IluServer_yPtr(jjIluServer, 0);
    ilu_free(cId);
} /*nativeOfFreeServer*/


/* end */
