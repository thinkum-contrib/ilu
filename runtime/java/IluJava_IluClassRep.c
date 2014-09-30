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
/* IluJava_IluClassRep.c */
/* Chris Jacobi, June 17, 1998 8:29 am PDT */

/*
 */
 
/* $Id: IluJava_IluClassRep.c,v 1.44 1999/09/09 22:43:00 janssen Exp $ */
 
/*
 * C side of IluClassRep.java
 * also accessing IluMethodRep.java and IluMethodArgRep.java
 */

#include "IluJava_Includes.h"
#include "IluJava_Common.h"

#include "IluJava_JTypes.h"
#include "IluJava_JArrays.h"
#include "IluJava_JStubs.h"
#include "IluJava_JGC.h"
#include "IluJava_JOps.h"
#include "xerox_ilu_IluMethodArgRep.h"
#include "xerox_ilu_IluExceptionRep.h"

/* Compare to /tilde/jacobi/ilu/runtime/python/iluclobject.c */

static void
freeSuperClasses(ilu_string *cSuperClassArray, int sccnt)
    /* specifically used in nativeFinishClass */
{
    if (cSuperClassArray) {
        int sx;
        for (sx = 0; sx < sccnt; sx = sx+1) {
            /* use java's free because strings were allocated by java */
            java_free(cSuperClassArray[sx]);
        }
        /* ilu's free as this was allocated with ilu */
        ilu_free(cSuperClassArray);
    }
}


JAVAEXPORT(IluClassRep_nativeFinishClass, void) 
		JIluClassRep jh_IluClass,
		Jint isRootClass
		ENDJAVAEXPORT
{
    JGC_WP_TYPE(JIluClassRep) jwp_IluClass = 
        JGC_GET_WP_FROM_JAVA_OBJECT(jh_IluClass);
    int mx, sx, ex, ax; /*loop indices*/
    ilu_Error err = ILU_INIT_NO_ERR;
    ilu_Error err2 = ILU_INIT_NO_ERR;
    ilu_Class cIluClass;
    ilu_string *cSuperClassArray = 0;
    ilu_string cIluClassName;
    ilu_string cUID;
    JArrayOfObject jh_methods = 0;
    JGC_WP_TYPE(JArrayOfObject) jwp_methods = 0;
    JArrayOfObject jh_supers;
    JGC_WP_TYPE(JArrayOfObject) jwp_supers = 0;
    JArrayOfObject jh_args;
    JGC_WP_TYPE(JArrayOfObject) jwp_args = 0;
    int sccnt = 0;
    int methcnt = 0;
    JString jh_singleton;
    JString jh_temp;
    ilu_string cSingleton;
    ilu_string cBrand;
    ilu_string cDoc;
    int argCnt;
    
    if (isRootClass) {
        cIluClass = (ilu_Class) ilu_rootClass;
        PUT_IluClassRep_yIluClass(jh_IluClass, cIluClass);
        _ilujava_registerJavaClass(cIluClass, jh_IluClass);
        goto return_andfree;
    }
    
    jh_temp = GET_IluClassRep_jjIluClassName(JGC_WP_REVEAL(jwp_IluClass));
    cIluClassName = IluJava_JString_toheap8x(JENV_ACTUAL jh_temp);  
    jh_temp = GET_IluClassRep_jjuid(JGC_WP_REVEAL(jwp_IluClass));
    cUID = IluJava_JString_toheap8x(JENV_ACTUAL jh_temp);  
    jh_methods = GET_IluClassRep_jjMethods(JGC_WP_REVEAL(jwp_IluClass));
    if (jh_methods) {jwp_methods = JGC_WP_MAKE(jh_methods);} 
    
    /* superclass array */
    jh_supers = GET_IluClassRep_jjIluSuperClasses(JGC_WP_REVEAL(jwp_IluClass));
    if (jh_supers) {
        sccnt = ARRAY_LENGTH(jh_supers);
        jwp_supers = JGC_WP_MAKE(jh_supers);
    }
    JTRACE(_ilujava_definitionsFlag>0,
        ("$ DefineClass %s sccnt: %d\n", cIluClassName, sccnt));
    if (sccnt) {
        cSuperClassArray = ilu_must_malloc(sccnt * sizeof(ilu_string));
        for (sx = 0; sx < sccnt; sx++) {
            ilu_string sClassId; 
            JIluClassRep jh_thisSuper;
            jh_thisSuper = (JIluClassRep) 
                ARRAY_object_GET(((JArrayOfObject)JGC_WP_REVEAL(jwp_supers)), sx);
            jh_temp =  (JString) GET_IluClassRep_jjuid(jh_thisSuper);
            sClassId = IluJava_JString_toheap8x(JENV_ACTUAL jh_temp);  
            cSuperClassArray[sx] = sClassId;
            JTRACE(_ilujava_definitionsFlag>0, 
                ("$ Superclass id [%s] \n", sClassId));
        }
    }

    /* Start the kernel registrations */
    ilu_EnterMutex(ilu_otmu, &err);
    if (ILU_ERRNOK(err)) {
        java_free(cIluClassName);
        java_free(cUID);
        freeSuperClasses(cSuperClassArray, sccnt);
        _ilujava_IluErrorToException(&err, completedNo "reallyFinish0");
        goto return_andfree;
    }
    
    /* The object class */
    jh_singleton = GET_IluClassRep_jjsingleton(JGC_WP_REVEAL(jwp_IluClass));
    cSingleton = ( jh_singleton == 0 ? 
        0 : IluJava_JString_toheap8x(JENV_ACTUAL  jh_singleton)
        );
    jh_temp = GET_IluClassRep_jjbrand(JGC_WP_REVEAL(jwp_IluClass));
    cBrand = IluJava_JString_toheap8x(JENV_ACTUAL jh_temp);  
    jh_temp = GET_IluClassRep_docString(JGC_WP_REVEAL(jwp_IluClass));
    cDoc = IluJava_JString_toheap8x(JENV_ACTUAL jh_temp);  
    cIluClass = ilu_DefineObjectType(
        cIluClassName, 	/*cl_name*/
        cBrand,	/*cl_brand*/
	cUID,   	/*cl_unique_id*/
    	cSingleton,
    	GET_IluClassRep_optional(JGC_WP_REVEAL(jwp_IluClass)) != 0,
    	GET_IluClassRep_collectible(JGC_WP_REVEAL(jwp_IluClass)) != 0,
    	cDoc,
    	(ilu_cardinal) GET_IluClassRep_method_count(JGC_WP_REVEAL(jwp_IluClass)), 
    	(ilu_cardinal) sccnt, 
	cSuperClassArray, 
	&err
    	);
    java_free(cSingleton);
    java_free(cBrand);	
    java_free(cDoc);
    PUT_IluClassRep_yIluClass(JGC_WP_REVEAL(jwp_IluClass), cIluClass);
    if (ILU_ERRNOK(err)) {
        java_free(cIluClassName);
        java_free(cUID);
        freeSuperClasses(cSuperClassArray, sccnt);
        _ilujava_IluErrorToException(&err, "reallyFinish1");
        goto return_andfree;
    }

    /* loop to register methods */
    methcnt = GET_IluClassRep_method_count(JGC_WP_REVEAL(jwp_IluClass));
    for (mx = 0; mx < methcnt; mx = mx+1) 
    {
        int exceptCnt = 0;
        ilu_Exception *cExceptionVec = 0;
        ilu_Method cIluMethod;
        ilu_string cMethodName;
        ilu_string cReturnType;
        JIluMethodRep jh_methRep = (JIluMethodRep) 
            ARRAY_object_GET(((JArrayOfObject)JGC_WP_REVEAL(jwp_methods)), mx);
        JGC_WP_TYPE(JIluMethodRep) jwp_methRep = 
            JGC_GET_WP_FROM_JAVA_OBJECT(jh_methRep);
        JArrayOfObject jh_exceptions 
        	= GET_IluMethodRep_jjExceptions(jh_methRep);
        JString jh_retValUid;
        /* build exception array for this method */
        if (jh_exceptions != 0) exceptCnt = ARRAY_LENGTH(jh_exceptions);
        if (exceptCnt) {
            cExceptionVec = 
        	ilu_must_malloc(exceptCnt * sizeof(ilu_Exception));
            for (ex = 0; ex < exceptCnt; ex = ex+1) 
            {
                JIluExceptionRep jExc = (JIluExceptionRep) 
                    ARRAY_object_GET(jh_exceptions, ex);
                cExceptionVec[ex] = GET_IluExceptionRep_yIluException(jExc);
                    /* not nil: ed in java */
            }
        }
        /* register method */
        ILU_CLER(err);
        jh_methRep = (JIluMethodRep) JGC_WP_REVEAL(jwp_methRep);
        argCnt = (int) GET_IluMethodRep_argCnt(jh_methRep);
        cMethodName = IluJava_JString_toheap8x(JENV_ACTUAL  
            GET_IluMethodRep_jjIluMethodName(jh_methRep)
            );
        jh_retValUid = GET_IluMethodRep_jjRetValUid(jh_methRep);
        cReturnType = (jh_retValUid == 0 ? (char*)0 : 
            IluJava_JString_toheap8x(JENV_ACTUAL  jh_retValUid)
            );
        cIluMethod = ilu_DefineMethod(
        	cIluClass,
        	(ilu_cardinal) mx,  /* i */
        	cMethodName, 
        	GET_IluMethodRep_id(jh_methRep),
                GET_IluMethodRep_cacheable(jh_methRep) != 0,
		GET_IluMethodRep_asynchronous(jh_methRep) != 0,
		(ilu_cardinal) exceptCnt, 
		cExceptionVec, 
		argCnt, 
		cReturnType, 
		&err
        	);
        java_free(cMethodName);
        java_free(cReturnType);
        if (ILU_ERRNOK(err)) {
            ilu_free(cExceptionVec);
            java_free(cIluClassName);
            java_free(cUID);
            freeSuperClasses(cSuperClassArray, sccnt);
            _ilujava_IluErrorToException(&err, "reallyFinish2");
            goto return_andfree;
        }
        jh_methRep = (JIluMethodRep) JGC_WP_REVEAL(jwp_methRep);
        PUT_IluMethodRep_yIluMethod(jh_methRep, cIluMethod);
        
        /* Loop for defining the arguments of the method */
        jh_args = GET_IluMethodRep_jjArgs(jh_methRep);
        if (jh_args) {jwp_args = JGC_WP_MAKE(jh_args);}
        for (ax = 0; ax < argCnt; ax = ax+1) 
        {
            ilu_boolean b;
            ilu_string cArgName;
            ilu_string cTypeID;
            ilu_ArgDirection cArgDir;
            ilu_boolean cSibling;
            
            JIluMethodArgRep jh_methArg;
            jh_methArg = (JIluMethodArgRep) ARRAY_object_GET(
                ((JArrayOfObject)JGC_WP_REVEAL(jwp_args)), ax
                );
            cArgName = IluJava_JString_toheap8x(JENV_ACTUAL  
                GET_IluMethodArgRep_argName(jh_methArg)
                );
            cTypeID = IluJava_JString_toheap8x(JENV_ACTUAL  
                GET_IluMethodArgRep_typeUid(jh_methArg)
                );
            cArgDir = GET_IluMethodArgRep_argDirection(jh_methArg);
            cSibling = GET_IluMethodArgRep_sibling(jh_methArg);
            
            ILU_CLER(err);
            b = ilu_DefineMethodArg(
                cIluMethod,
                (ilu_cardinal) ax,
                cArgName,
                cSibling,
                cArgDir,
                cTypeID,
                &err
                );
            java_free(cArgName);
            java_free(cTypeID);
            if (ILU_ERRNOK(err)) {
                ilu_free(cExceptionVec);
                java_free(cIluClassName);
                java_free(cUID);
                freeSuperClasses(cSuperClassArray, sccnt);
                _ilujava_IluErrorToException(&err, "reallyFinish3");
                /* leaking memory used for string arguments 
                 * of ilu_DefineMethodArg; I see no alternative.
                 */
                goto return_andfree;
            }
        }
        /* 
         * Terrible loophole: 
         * Assigns a java object into a field declared as procedure 
         */
        ilu_SetMethodStubProc(
            cIluMethod, (ilu_StubProc) jwp_methRep, JAVALANGIDX
            );
        ilu_free(cExceptionVec);
    }
    
    _ilujava_registerJavaClass(cIluClass, JGC_WP_REVEAL(jwp_IluClass)); /*IluJava_Common.h*/
    
    ilu_ObjectTypeDefined(cIluClass, &err2);
    if (ILU_ERRNOK(err2)) {
        _ilujava_IluErrorToException(&err2, "reallyFinish98");
        goto return_exitMutexAndFreeSuperClasses;
    }

#ifdef IIOP_PROTOCOL
    {
        JString jh_ifBrand; 
        JString jh_ifName; 
        /*L1 < otmu required and no lock is held ... */
        ilu_boolean new;
        ilu_string cIfName = 0;
        ilu_string cIfBrand = 0;
        int classNameLen = 0;
        int ifNameLen = 0;
        classNameLen = strlen(cIluClassName);
        jh_ifName = GET_IluClassRep_jjIfName(JGC_WP_REVEAL(jwp_IluClass));
        if (jh_ifName) {
            cIfName = IluJava_JString_toheap8x(JENV_ACTUAL  jh_ifName);
            if (cIfName) {
                ifNameLen = strlen(cIfName);
                if (ifNameLen) {
                    ifNameLen = ifNameLen + 1; /* dot */
                    if (ifNameLen >= classNameLen) {ifNameLen = 0;}
                }
            }
        }
        jh_ifBrand = GET_IluClassRep_jjIfBrand(JGC_WP_REVEAL(jwp_IluClass));
        if (jh_ifBrand) {
            cIfBrand = IluJava_JString_toheap8x(JENV_ACTUAL  jh_ifBrand);
        }
        ILU_CLER(err);
        ilu_RegisterObjectType(
            &cIluClassName[ifNameLen], 
            cIfName, cIfBrand, cUID, cIluClass, &new, &err
            );
        java_free(cIfName);
        if (cIfBrand) java_free(cIfBrand);
    }
#endif /* def IIOP_PROTOCOL */
    
    /* fall through */
return_exitMutexAndFreeSuperClasses:
        ILU_CLER(err);
        ilu_ExitMutex(ilu_otmu, ilu_FALSE, &err);
        if (ILU_ERRNOK(err)) {
            /*raise java exception only if no other exception is pending */ 
            if (JPENDINGEXCEPTION() != 0) {
                _ilujava_IluErrorToException(&err, "reallyFinish99");
            } else {
                ILU_HANDLED(err);
            }
        }
        freeSuperClasses(cSuperClassArray, sccnt);
        java_free(cIluClassName);
        java_free(cUID);
        /* fall through */
return_andfree:
        if (jwp_methods) {JGC_WP_RELEASE(jwp_methods);}
        if (jwp_supers) {JGC_WP_RELEASE(jwp_supers);}
        if (jwp_args) {JGC_WP_RELEASE(jwp_args);}
        return;
} /* nativeFinishClass */


JAVAEXPORT(IluExceptionRep_registerException, void) 
		JIluExceptionRep jh_iluExceptionRep
		ENDJAVAEXPORT
{
    JGC_WP_TYPE(JIluExceptionRep) jwp_iluExceptionRep = 
        JGC_GET_WP_FROM_JAVA_OBJECT(jh_iluExceptionRep);
    ilu_Exception cIluException;
    ilu_Error err = ILU_INIT_NO_ERR;
    /* 
     * (javaString.h)makeCString converts unicode to char's 
     * and returns a garbage collectable C string
     * (ilu_DefineException says: caller owns data) 
     */
    JString jh_interfacename;
    char* i = 0;
    char* e = IluJava_JString_toheap8x(JENV_ACTUAL  
        GET_IluExceptionRep_e(JGC_WP_REVEAL(jwp_iluExceptionRep))
        ); 
    char* valUid = 0; 
    JString jh_uuid;

    jh_interfacename = GET_IluExceptionRep_i(JGC_WP_REVEAL(jwp_iluExceptionRep));
    if (jh_interfacename) {
      i = IluJava_JString_toheap8x(JENV_ACTUAL jh_interfacename);
    }
    jh_uuid = GET_IluExceptionRep_jtuuid(JGC_WP_REVEAL(jwp_iluExceptionRep));
    if (jh_uuid) {
      valUid = IluJava_JString_toheap8x(JENV_ACTUAL  jh_uuid);
    }
    cIluException = ilu_DefineException(i, e, valUid, &err);
    java_free(i);
    java_free(e);
    java_free(valUid);
    ILU_MUST_BE_SUCCESS(err);
    PUT_IluExceptionRep_yIluException(JGC_WP_REVEAL(jwp_iluExceptionRep), cIluException);
}


/* end */

