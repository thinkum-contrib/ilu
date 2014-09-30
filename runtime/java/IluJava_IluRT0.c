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
/* IluJava_IluRT0.c */ 
/* Chris Jacobi, November 25, 1998 5:41 pm PST */

/*
 */
 
/* $Id: IluJava_IluRT0.c,v 1.68 1999/08/03 01:54:17 janssen Exp $ */

/*
 * C side of IluRT0.java
 */

#include "IluJava_Includes.h"
#include "IluJava_Common.h"
#include "IluJava_JGC.h"

#include <iluhash.h>
#include <iluntrnl.h>
#include <iluerrs.h>

#include "IluJava_JStubs.h"
#include "IluJava_JTypes.h"
#include "IluJava_JOps.h"
#include "IluJava_Ops.h"

#define CORBAPREFIX(some_string)  "org/omg/CORBA/" ## some_string


/* ================ Registry ================ */


static HashTable c2j_tab; /* key ilu_Class; val is WEAK java IluClassRep */


EXPORTLIMITED void
_ilujava_IluRt0InitHashTable()
{
    c2j_tab = ilu_hash_MakeNewTable(111, 
    	ilu_hash_HashPointer, 
    	ilu_hash_PointerCompare
    	);
}

 
/* locking by caller */
EXPORTLIMITED void 
_ilujava_registerJavaClass (
	ilu_Class cIluClass, 
	/* JIluClassRep */ ilu_refany jjClassRep
	)
/* Register java class necessary to CREATE java surrogate object */
{
  JENV_DECLARE_INITFROMTHINAIR
  ilu_hash_AddToTable(c2j_tab, 
  	(ilu_refany) cIluClass, 
  	(ilu_refany) JGC_GET_WP_FROM_JAVA_OBJECT(jjClassRep)
  	);
}


EXPORTLIMITED ilu_refany   /* JIluClassRep */
_ilujava_findJavaClass(ilu_Class cIluClass)
/* Get java class necessary to CREATE java surrogate object */
{
    return JGC_WP_REVEAL( 
        (JGC_WP_TYPE(JIluClassRep)) ilu_hash_FindInTable(c2j_tab, cIluClass)
        );
}


static JGC_WP_TYPE(JIluClassRep)
_ilujava_findJavaClassWeak(ilu_Class cIluClass)
/* Get java class necessary to CREATE java surrogate object */
{
    return (JGC_WP_TYPE(JIluClassRep)) ilu_hash_FindInTable(c2j_tab, cIluClass);
}


/* ================ ======== ================ */
 
static void javathrow(int cde, char * x, char * t)
{
    JENV_DECLARE_INITFROMTHINAIR
    _ilujava_throwException(JENV_ACTUAL cde, x, t);
}

  
/* Inside(cIluObject->server, staticClass) */
/* oi must be finished */
/* calls back into jave => GC! */
/* Inside(cIluObject->server, staticClass) */
/* oi must be finished */
/* calls back into java => GC! */
EXPORTLIMITED ilu_refany /* JIluOInt */
    _ilujava_creatSurrOIFromRegistry(
	JENV_FORMAL
	ilu_Object cIluObject, 
	ilu_refany jh_staticClass   /* (NIL IS OK) */
	)
{
    /* 
        JGC_WP_TYPE(JIluClassRep) jwp_ExpectedClass = 
            ...(jh_staticClass)
    */
    JGC_WP_TYPE(JIluClassRep) jwp_ActualClass;
    JGC_WP_TYPE(JIluOInt) jwp_oi;
    JIluOInt jh_oi;
    ilu_Class cActualIluClass;
    ilu_Server cServer;
    ilu_Error err = ILU_INIT_NO_ERR;
    cActualIluClass = ilu_ClassOfObject(cIluObject);
    cServer = ilu_ServerOfObject(cIluObject);
    jwp_ActualClass = _ilujava_findJavaClassWeak(cActualIluClass);
    if (jwp_ActualClass==0) {
        CONDPRINT("$ IluJava failure: surrogate class not found... \n");
        javathrow(14, CORBAPREFIX("UNKNOWN"),
            "surrogate class not registered"
            );
        return 0;
    }
/*
    if ( JGC_WP_REVEAL(jwp_ActualClass) != JGC_WP_REVEAL(jwp_ExpectedClass)) {
        CONDPRINT("$ IluJava failure: class missmatch... \n");
        javathrow(15, CORBAPREFIX("UNKNOWN"), 
            "ilu-class missmatch"
            );
        return 0;
    }
*/
    /* May cause a java GC */
    jh_oi = (JIluOInt) _ilujava_newIluOInt(JENV_ACTUAL_NOCOMMA);
    if (jh_oi == 0) {
       /* failed somehow in java */
       _ilujava_PANIC("$ IluJava: IluOInt creation failed... \n");
       return 0;
    }
    jwp_oi = JGC_GET_WP_FROM_IluOInt(jh_oi);
    PUT_IluOInt_jjClassRep(jh_oi, (JIluClassRep)JGC_WP_REVEAL(jwp_ActualClass));
    PUT_IluOInt_yServer(jh_oi, cServer);
    PUT_IluOInt_yIluKernelObject(jh_oi, cIluObject);   
    ilu_RegisterLSO(cIluObject, cActualIluClass, jwp_oi, JAVALANGIDX, &err );
    /* We just created jh_oi. Don't declare it a ghost; if it is found
     * by the collector, so be it.
     */
    if (ILU_ERRNOK(err)) {
        _ilujava_IluErrorToCallException(&err, "creatSurrOIFromRegistry");
    }
    return (ilu_refany) JGC_WP_REVEAL(jwp_oi);
}


/* compare this to /tilde/jacobi/ilus/runtime/c/ilu.c 
 * and look at /tilde/jacobi/ilus/runtime/kernel/iluerrs.h
 */ 

EXPORTLIMITED void 
_ilujava_Report(ilu_Error* errPtr, char * t)
/* OPTIONALLY (according to command line options)
 * makes a call to ilu_DebugPrintf.
 * Does not raise any errors or exceptions.
 * Explicitely also supports errPtr==0.
 */
{
    if (_ilujava_iluGenericFlag > 0) {
        if (errPtr) {
            ilu_DebugPrintf("$ Report: %s %s %s \n", 
                t, ILU_ERR_NAME(*errPtr), ILU_ERR_DESCRIPTION(*errPtr)
                );
        } else {
            ilu_DebugPrintf("$ Report: %s \n", t);
        }
    }
}


EXPORTLIMITED void 
_ilujava_IluErrorToCallException(ilu_Error* errPtr, char* t)
/* Like IluErrorToException except that there is no switch
 * on the exception but allways IluCallException is (now UNKNOWN) raised.
 */
{
    if (t==0) {
        t = "unspecified";
    }
    _ilujava_Report(errPtr, t);
    javathrow(16, CORBAPREFIX("UNKNOWN"), t);
    if (errPtr) {
        ILU_HANDLED(*errPtr);
    }
}
 

/* Shorthand used in IluErrorToException */
#define E_SWITCH(errkind, errocode, exc) 		\
    ILU_ERR_CASE(errkind, v) {				\
        javathrow(errocode, exc, t); 			\
    }

/* Shorthand used in IluErrorToException
 * For CORBA exceptions
 */
#define E_SWITCH_C(errkind, errocode, exc) 		\
    ILU_ERR_CASE(errkind, v) {				\
        javathrow(errocode, exc, t); 			\
    }


/* Shorthand used in IluErrorToException 
 * IN CASE THE EXCEPTION IS NOT YET IMPLEMENTED
 */
#define UNIMPL_SWITCH(errkind, ignored) 			\
    ILU_ERR_CASE(errkind, v) {					\
        javathrow(17, CORBAPREFIX("NO_IMPLEMENT"), t);		\
    }




EXPORTLIMITED void 
_ilujava_IluErrorToException(ilu_Error* errPtr, char * t)
{
    _ilujava_Report(errPtr, t);
    if (errPtr == 0) {
        javathrow(18, CORBAPREFIX("UNKNOWN"), t);
        return;
    }
    ILU_ERR_SWITCH(*errPtr) {
        ILU_SUCCESS_CASE return;

        /* .../ilu/runtime/kernel/iluerrs.h calls these public */
        E_SWITCH_C(unknown, 21, CORBAPREFIX("UNKNOWN"))
        E_SWITCH_C(bad_param, 22, CORBAPREFIX("BAD_PARAM"))
        E_SWITCH_C(no_memory, 23, CORBAPREFIX("NO_MEMORY"))
        E_SWITCH_C(imp_limit, 24, CORBAPREFIX("IMP_LIMIT"))
        E_SWITCH_C(comm_failure, 25, CORBAPREFIX("COMM_FAILURE"))
        E_SWITCH_C(inv_objref, 26, CORBAPREFIX("INV_OBJREF"))
        E_SWITCH_C(no_permission, 27, CORBAPREFIX("NO_PERMISSION"))
        E_SWITCH_C(internal, 28, CORBAPREFIX("INTERNAL"))
        E_SWITCH_C(marshal, 29, CORBAPREFIX("MARSHAL"))
        E_SWITCH_C(initialize, 30, CORBAPREFIX("INITIALIZE"))
        E_SWITCH_C(no_implement,31, CORBAPREFIX("NO_IMPLEMENT"))
        E_SWITCH_C(bad_typecode, 32, CORBAPREFIX("BAD_TYPECODE"))
        E_SWITCH_C(bad_operation, 33, CORBAPREFIX("BAD_OPERATION"))
        E_SWITCH_C(no_resources, 34, CORBAPREFIX("NO_RESOURCES"))
        E_SWITCH_C(no_response, 35, CORBAPREFIX("NO_RESPONSE"))
        E_SWITCH_C(persist_store, 36, CORBAPREFIX("PERSIST_STORE"))
        E_SWITCH_C(bad_inv_order, 37, CORBAPREFIX("BAD_INV_ORDER")) 
        E_SWITCH_C(transient, 38, CORBAPREFIX("TRANSIENT"))
        E_SWITCH_C(free_mem, 39, CORBAPREFIX("FREE_MEM"))
        E_SWITCH_C(inv_ident, 40, CORBAPREFIX("INV_IDENT"))
        E_SWITCH_C(inv_flag, 41, CORBAPREFIX("INV_FLAG"))
        E_SWITCH_C(intf_repos, 42, CORBAPREFIX("INTF_REPOS"))
        E_SWITCH_C(bad_context, 43, CORBAPREFIX("BAD_CONTEXT"))
        E_SWITCH_C(obj_adapter, 44, CORBAPREFIX("OBJ_ADAPTER"))
        E_SWITCH_C(data_conversion, 45, CORBAPREFIX("DATA_CONVERSION"))
        UNIMPL_SWITCH(bad_locks, CORBAPREFIX("BAD_LOCKS")) 
        UNIMPL_SWITCH(broken_locks, CORBAPREFIX("BROKEN_LOCKS")) 
        UNIMPL_SWITCH(interrupted, CORBAPREFIX("INTERRUPTED")) 
        UNIMPL_SWITCH(gss_security, CORBAPREFIX("GSS_SECURITY")) 
        
        /* .../ilu/runtime/kernel/iluerrs.h calls these unresolved */
        
        E_SWITCH(MaxCountExceeded, 50, CORBAPREFIX("NO_RESOURCES"))
        E_SWITCH(ProtocolAlreadyRegistered, 51, CORBAPREFIX("INTF_REPOS"))
        E_SWITCH(TransportAlreadyRegistered, 52, CORBAPREFIX("INTF_REPOS"))
        E_SWITCH(BadProtocolInfo, 53, CORBAPREFIX("INTF_REPOS"))
        E_SWITCH(GcRegFailed, 54, "xerox/ilu/IluGcRegFailedException")
        E_SWITCH(NoObjectForSBH, 55, CORBAPREFIX("OBJECT_NOT_EXIST"))
        E_SWITCH(CantCondition, 56, CORBAPREFIX("INTERNAL"))
        
        ILU_ERR_ELSE {
            javathrow(19, CORBAPREFIX("UNKNOWN"), t);
        }
    } ILU_ERR_ENDSWITCH;
	ILU_HANDLED(*errPtr);
	return;
}


EXPORTLIMITED void
_ilujava_NullPointerException(char* t)
/* Causes a NullPointerException to be reported when returned into java.
 */
{
    _ilujava_Report(0, "NullPointerException");
    javathrow(1, "java/lang/NullPointerException", t);
}
 
 
EXPORTLIMITED void
_ilujava_SignalInconsistency(char* t)
/* Causes a serious internal error to be reported when returned into java.
 */
{
    if (t==0) t = "unspecified";
    ilu_DebugPrintf("$ IluCall inconsistency\n");
    javathrow(12, "xerox/ilu/IluInconsistentCallException", t);
}


/* ================  ================ */


JAVAEXPORT(IluRT0_oiFromSBH, JIluOInt)
	JIluRT0 unused,
	JString jSBH,
	JIluClassRep jh_ClassRep
	ENDJAVAEXPORT 
{
    JGC_WP_TYPE(JIluClassRep) jwp_ClassRep =
        JGC_GET_WP_FROM_JAVA_OBJECT(jh_ClassRep);
    JIluOInt jh_oi; 
    ilu_Object cIluObject;
    ilu_Error errs = ILU_INIT_NO_ERR;
    ilu_Class cIluClass;
    char * cSBH;
    ilu_ConsiderSbhResult result;
    JGC_WP_TYPE(JIluOInt) jwp_oi;

    if (jSBH == 0) {
        return 0;
    }

    if (JGC_WP_REVEAL(jwp_ClassRep) == 0) {
        cIluClass = ilu_rootClass;
    } else {
        cIluClass = GET_IluClassRep_yIluClass(JGC_WP_REVEAL(jwp_ClassRep));
    }
        
    cSBH = IluJava_JString_toheap8x (JENV_ACTUAL jSBH); 
    
    result = ilu_ConsiderSBH(cSBH, &errs);
    if (result == ilucsr_err) {
        java_free(cSBH);
        _ilujava_IluErrorToException(&errs, "ilu_ConsiderSBH");
        return 0;
    }
    
    cIluObject = ilu_ObjectOfSBH(cSBH, cIluClass, &errs);
    if (ILU_ERRNOK(errs)) {
        java_free(cSBH);
        _ilujava_IluErrorToException(&errs, "ilu_ObjectOfSBH");
        return 0;
    }
    if (cIluObject == 0) {
        java_free(cSBH);
        return 0;
    }
    jwp_oi = ilu_GetLanguageSpecificObject(cIluObject, JAVALANGIDX);
    /* true or surrogate at this point */
    if (jwp_oi == 0) {
        /* but we do not create true objects here */
        jh_oi = (JIluOInt) _ilujava_creatSurrOIFromRegistry(
            JENV_ACTUAL
            cIluObject, 
            JGC_WP_REVEAL(jwp_ClassRep)
            );
        jwp_oi = JGC_GET_WP_FROM_IluOInt(jh_oi);
        /* java GC may have occurred */
    } else {
        jh_oi = (JIluOInt) JGC_WP_REVEAL(jwp_oi);
        if (jh_oi==0) {
            _ilujava_IluErrorToException(0, "IluRT0_oiFromSBH: IMPOSSIBLE");
            return 0;
        }
        if (GET_IluOInt_retained(jh_oi)) {
            PUT_IluOInt_ghost(jh_oi, 1);
        }
    }
    ilu_ExitServer(
    	ilu_ServerOfObject(cIluObject),
    	cIluClass
    	);
    java_free(cSBH);
    jh_oi = (JIluOInt) JGC_WP_REVEAL(jwp_oi);
    if (jh_oi == 0) {
        _ilujava_IluErrorToException(0, "IluRT0_oiFromSBH: IMPOSSIBLE");
    }
    return jh_oi; 
}


JAVAEXPORT(IluRT0_oiFromLookup, JIluOInt)
	JIluRT0 unused,
	JString jh_sid,
	JString jh_ih,
	JIluClassRep jh_ClassRep
	ENDJAVAEXPORT
{
    JGC_WP_TYPE(JIluClassRep) jwp_ClassRep = 
        JGC_GET_WP_FROM_JAVA_OBJECT(jh_ClassRep);
    JIluOInt jh_oi; 
    char * csid;
    char * cih;
    ilu_Object cIluObject = ILU_NIL;  
    ilu_Class cIluClass;
    ilu_boolean change;
	JGC_WP_TYPE(JIluOInt) jwp_oi;
    if ((jh_sid == 0) || (jh_ih == 0)) {
        return 0;
    }
    csid = IluJava_JString_toheap8x (JENV_ACTUAL jh_sid); 
    cih = IluJava_JString_toheap8x (JENV_ACTUAL jh_ih); 
    if (jh_ClassRep == 0) {
        cIluClass = ilu_rootClass;
    } else if (JGC_WP_REVEAL(jwp_ClassRep) == 0) {
        cIluClass = ilu_rootClass;
    } else {
        cIluClass = GET_IluClassRep_yIluClass(JGC_WP_REVEAL(jwp_ClassRep));
    }
    change = ilu_ReLookupObject(csid, cih, cIluClass, &cIluObject);
    if (cIluObject == 0) {
        java_free(csid);
        java_free(cih);
        return 0;
    }
    jwp_oi = ilu_GetLanguageSpecificObject(cIluObject, JAVALANGIDX);		
    if (jwp_oi == 0) {
        jh_oi = (JIluOInt) _ilujava_creatSurrOIFromRegistry(
            JENV_ACTUAL
            cIluObject, 
            JGC_WP_REVEAL(jwp_ClassRep)
            );
        jwp_oi = JGC_GET_WP_FROM_IluOInt(jh_oi);
        /* java GC may have occurred */
    } else {
        jh_oi = (JIluOInt) JGC_WP_REVEAL(jwp_oi);
        if (GET_IluOInt_retained(jh_oi)) {
            PUT_IluOInt_ghost(jh_oi, 1);
        }
    }
    ilu_ExitServer(
    	ilu_ServerOfObject(cIluObject),
    	cIluClass
    	);
    java_free(csid);
    java_free(cih);
    return ((JIluOInt)JGC_WP_REVEAL(jwp_oi));
}


JAVAEXPORT(IluRT0_inventID, JString)
    JIluRT0 unused
    ENDJAVAEXPORT
{
    JString jjstring;
    ilu_string cstring = ilu_InventID();
    jjstring = IluJava_JString_fromA0(JENV_ACTUAL cstring);
    ilu_free(cstring);
    return jjstring;
}


JAVAEXPORT(IluRT0_iluVersion, JString)
    JIluRT0 unused
    ENDJAVAEXPORT
{
    JString jjstring;
    ilu_string cstring = ilu_GetILUVersion();
    jjstring = IluJava_JString_fromA0(JENV_ACTUAL cstring);
    ilu_free(cstring);
    return jjstring;
}


JAVAEXPORT(IluRT0_iluConfigurationData, Jint)
    JIluRT0 unused,
    Jint inkey
    ENDJAVAEXPORT
{
    Jint doesnt = -1;
    switch (inkey) {
    #ifdef ENABLE_DEBUGGING
        case 1: return 1;
    #endif
    #ifdef ADD_VARIANT_SUPPORT
        case 2: return 1;
    #endif 
    #ifdef ADD_TYPE_REGISTRATION_SUPPORT
        case 3: return 1;
    #endif
    #ifdef SECURE_TRANSPORT
        case 10: return 1;
    #endif
    #ifdef TCPIP_TRANSPORT
        case 11: return 1;
    #endif
    #ifdef BATCHING_TRANSPORT
        case 12: return 1;
    #endif
    #ifdef W3MUX_TRANSPORT
        case 13: return 1;
    #endif
    #ifdef TCPIP_PROTOCOL
        case 20: return 1;
    #endif
    #ifdef SUNRPC_PROTOCOL
        case 21: return 1;
    #endif
    #ifdef IIOP_PROTOCOL
        case 22: return 1;
    #endif
    #ifdef HTTP_PROTOCOL
        case 23: return 1;
    #endif
    #ifdef COURIER_PROTOCOL
        case 24: return 1;
    #endif
    #ifdef W3NG_PROTOCOL
        case 25: return 1;
    #endif
    #ifdef JAVARMI_PROTOCOL
        case 26: return 1;
    #endif
    default: break;
    }
    return doesnt;
}


JAVAEXPORT(IluRT0_getFDBudget, Jint)
    JIluRT0 unused
    ENDJAVAEXPORT
{
    Jint n = -1;
    n = (Jint) ilu_GetFDBudget();
    return n;
}


JAVAEXPORT(IluRT0_nSetFDBudget, Jint)
    JIluRT0 unused,
    Jint n
    ENDJAVAEXPORT
{
    n = (Jint) ilu_SetFDBudget((ilu_cardinal) n);
    return n;
}


JAVAEXPORT(IluRT0_registerTrue, void)
	JIluRT0 unused,
	JIluOInt jh_oi,
	JString jh_ih,
	JIluServer jh_IluServer
	ENDJAVAEXPORT
/* NOTE: 
 * similar code in (IluJava_IluServer.c)_ilujava_ObjectTableCreateObj
 */
{
    ilu_Object cObject;
    ilu_string cih = 0;
    ilu_Server cServer;
    ilu_Class cClass;
    JGC_WP_TYPE(JIluClassRep) jwp_ClassRep;
    JGC_WP_TYPE(JIluOInt) jwp_oi = JGC_GET_WP_FROM_IluOInt(jh_oi);
    JGC_WP_TYPE(JIluServer) jwp_IluServer = 
        JGC_GET_WP_FROM_JAVA_OBJECT(jh_IluServer);
    if ((JGC_WP_REVEAL(jwp_oi) == 0) | (JGC_WP_REVEAL(jwp_IluServer) == 0) | (jh_ih == 0)) {
        PRINTRAISE("$ ** IluRT0_registerTrue ERROR BAD ARGUMENTS\n");
        return;
    }
    jwp_ClassRep = JGC_GET_WP_FROM_JAVA_OBJECT(
        GET_IluOInt_jjClassRep(JGC_WP_REVEAL(jwp_oi))
        );
    if (JGC_WP_REVEAL(jwp_ClassRep) == 0) {
        PRINTRAISE("$ ** IluRT0_registerTrue ERROR no ilu-class\n");
        return;
    }
    cClass = GET_IluClassRep_yIluClass(JGC_WP_REVEAL(jwp_ClassRep));
    if (cClass == 0) {
        PRINTRAISE("$ ** IluRT0_registerTrue ERROR bad ilu-class\n");
        return;
    }
    cih = IluJava_JString_toheap8x (JENV_ACTUAL jh_ih);  
    cServer = _ilujava_EnterServer2(JENV_ACTUAL
        (JIluServer)JGC_WP_REVEAL(jwp_IluServer), cClass);
    if (cServer==0) {
        PRINTRAISE("$ ** IluRT0_registerTrue ERROR server destroyed\n");
        java_free(cih);
        return;
    }
    PUT_IluOInt_yServer(JGC_WP_REVEAL(jwp_oi), cServer);
    
    cObject = ilu_FindOrCreateTrueObject(cih, cServer, cClass, jwp_oi);
    java_free(cih);
    if (cObject) {
        JGC_WP_TYPE(JIluOInt) jwp_alt;
        if (_ilujava_gcFlag > 1) {
            /* temporary to make debugging collectables faster */
            cObject->ob_timeout = 120; /*for debugging: gc after two minutes*/
        }

        jwp_alt = ilu_GetLanguageSpecificObject(cObject, JAVALANGIDX);

        if (jwp_alt != jwp_oi) {
            /* This can happen: 
             * jh_oi has been created outside of the server lock.... 
             */
            JTRACE(_ilujava_objectsFlag>1,
                ("$ registerTrue: object creation conflict\n"));   
            cObject = 0;
        }
    }
    JTRACE(_ilujava_objectsFlag>1,
        ("$ IluRT0_registerTrue: found %x \n", cObject));   
    PUT_IluOInt_yIluKernelObject(JGC_WP_REVEAL(jwp_oi), cObject);
    ilu_ExitServer(cServer, cClass);
}


/* start export IluWPBase.java */

JAVAEXPORT(IluWPBase_finalize, void)
	JIluWPBase jh_self
	ENDJAVAEXPORT 
{
  #if (defined (ONI))
    /* nothing to do */
  #elif (defined (RNI))
    /* free the weak pointer we allocated during construction */
    JGC_WP_TYPE(JIluWPBase) jwp_self;
    jwp_self = JGC_GET_WP_FROM_JAVA_OBJECT(jh_self);
    PUT_IluWPBase_ywpx(jh_self, 0);	  
    JGC_WP_RELEASE(jwp_self);	  
  #elif (defined (JNI))
    JGC_WP_TYPE(JIluWPBase) jwp_self;
    jwp_self = JGC_GET_WP_FROM_JAVA_OBJECT(jh_self);
    PUT_IluWPBase_ywpx(jh_self, 0);	  
    JGC_WP_RELEASE(jwp_self);	  
  #else
    #error "finalize not yet defined for this native architecure"
  #endif
}


JAVAEXPORT(IluWPBase_setupIluWPBase, void)
	JIluWPBase jh_self
	ENDJAVAEXPORT 
{
  #if (defined (ONI))
    /* don't really need to do this for ONI since we never use  
     * the weak pointer
     */
  #elif (defined (RNI))
    /* allocate a weak pointer and store it into the java object itself */
    JGC_WP_TYPE(JIluWPBase) weak_pointer;
    weak_pointer = JGC_WP_MAKE(jh_self);
    PUT_IluWPBase_ywpx(jh_self, weak_pointer);	  
  #elif (defined (JNI))
    JGC_WP_TYPE(JIluWPBase) weak_pointer;
    weak_pointer = JGC_WP_MAKE(jh_self);
    PUT_IluWPBase_ywpx(jh_self, weak_pointer);	  
  #else
    #error "setupIluWPBase not yet defined for this native architecure"
  #endif
}


JAVAEXPORT(IluOInt_finalizeIluOInt, void)
	JIluOInt jh_self
	ENDJAVAEXPORT 
{
  #if (defined (ONI))
    /* nothing to do */
  #elif (defined (RNI))
    /* free the weak pointer we allocated during construction */
    JGC_WP_TYPE(JIluOInt) jwp_self;
    jwp_self = JGC_GET_WP_FROM_IluOInt(jh_self);
    PUT_IluOInt_ywpxIluOInt(jh_self, 0);	  
    JGC_WP_RELEASE(jwp_self);	  
  #elif (defined (JNI))
    JGC_WP_TYPE(JIluOInt) jwp_self;
    jwp_self = JGC_GET_WP_FROM_IluOInt(jh_self);
    PUT_IluOInt_ywpxIluOInt(jh_self, 0);	  
    JGC_WP_RELEASE(jwp_self);	  
  #else
    #error "finalize_IluOInt not yet defined for this native architecure"
  #endif
}


JAVAEXPORT(IluOInt_setupIluWPBaseIluOInt, void)
	JIluOInt jh_self
	ENDJAVAEXPORT 
{
  #if (defined (ONI))
    /* don't really need to do this for ONI since we never use  
     * the weak pointer
     */
  #elif (defined (RNI))
    /* allocate a weak pointer and store it into the java object itself */
    JGC_WP_TYPE(JIluOInt) weak_pointer;
    weak_pointer = JGC_WP_MAKE(jh_self);
    PUT_IluOInt_ywpxIluOInt(jh_self, weak_pointer);	  
  #elif (defined (JNI))
    JGC_WP_TYPE(JIluOInt) weak_pointer;
    weak_pointer = JGC_WP_MAKE(jh_self);
    PUT_IluOInt_ywpxIluOInt(jh_self, weak_pointer);	  
  #else
    #error "setupIluWPBaseIluOInt not yet defined for this native architecure"
  #endif
}


/* end export IluWPBase.java */


/* end IluJava_IluRT0.c */



