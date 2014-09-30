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
/* IluJava_IluPickle.c */
/* Chris Jacobi, January 7, 1999 3:13 pm PST */
/* $Id: IluJava_IluPickle.c,v 1.20 1999/08/03 01:54:29 janssen Exp $ */

/*
 * C side for IluPickle.java, IluTypeCode.java, and, IluTypeRep.java
 */
 
#include "IluJava_Includes.h"
#include "IluJava_CallMacros.h"
#include "IluJava_JArrays.h"
#include "IluJava_JGC.h"
#include "IluJava_JOps.h"
#include "IluJava_Common.h"


#define INTERNAL_EXC "org/omg/CORBA/INTERNAL"
#define INITIALIZE_EXC "org/omg/CORBA/INITIALIZE"
#define ADAPTER_EXC "org/omg/CORBA/OBJ_ADAPTER"
#define PARAM_EXC "org/omg/CORBA/BAD_PARAM"


static char*
J2Cstring8X(JENV_FORMAL JString jh_string)
/* restrict to 8 bits; allocated in ilu heap; return "" if jh_s is null */
{
    char* cstring = IluJava_JString_toheap80(JENV_ACTUAL jh_string);
    if (cstring == 0) {
       cstring = ilu_must_malloc(1);
       *cstring = 0;
    }
    return cstring;
} /*J2Cstring8X*/


JAVAEXPORT(IluPickle_nativeOutPickle, void)
    JIluPickle jh_pickle, 
    JIluCall jh_call  /* jh_call is null tested in java */
    ENDJAVAEXPORT
{
  #if (defined(ADD_VARIANT_SUPPORT) || defined(IIOP_PROTOCOL))
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    ilu_Pickle* cPicklePtr;
    PROLOG(CALL_OUT, ILUJAVA_VOID)
    cPicklePtr = GET_IluPickle_ypickleptr(jh_pickle);
    if (cPicklePtr == 0) {
        _ilujava_throwException(JENV_ACTUAL 30, INITIALIZE_EXC, "pickle not initialized");
        return;
    }
    ilu_OutputPickle(cCall, *cPicklePtr, (ilu_Type)ILU_NIL, &ioerrs);
    ERRCHECK(ILUJAVA_VOID)
  #else
    BADCASE;
  #endif /* ADD_VARIANT_SUPPORT || IIOP_PROTOCOL */
}
 

JAVAEXPORT(IluPickle_nativeSzPickle, Jint)
    JIluPickle jh_pickle, 
    JIluCall jh_call  /* jh_call is null tested in java */
    ENDJAVAEXPORT
{
  #if (defined(ADD_VARIANT_SUPPORT) || defined(IIOP_PROTOCOL))
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    ilu_Pickle* cPicklePtr;
    PROLOG_SZ()
    cPicklePtr = GET_IluPickle_ypickleptr(jh_pickle);
    if (cPicklePtr == 0) {
        _ilujava_throwException(JENV_ACTUAL 30, INITIALIZE_EXC, "pickle not initialized");
        return 0;
    }
    sz = ilu_SizeOfPickle(cCall, *cPicklePtr, (ilu_Type)ILU_NIL, &ioerrs);
    ERRCHECK(0)
    return (Jint) sz;
  #else
    return 0;
  #endif /* ADD_VARIANT_SUPPORT || IIOP_PROTOCOL */
} /*IluPickle_nativeSzPickle*/


static ilu_Pickle* allocPickle(JENV_FORMAL JIluPickle jh_pickle)
{
  #if (defined(ADD_VARIANT_SUPPORT) || defined(IIOP_PROTOCOL))
    ilu_Pickle* cPicklePtr = ilu_must_malloc(sizeof(ilu_Pickle));
    cPicklePtr->pi_len = 0;
    cPicklePtr->pi_bytes = 0;
    PUT_IluPickle_ypickleptr(jh_pickle, cPicklePtr);
    return cPicklePtr;
  #else
    return 0;
  #endif /* ADD_VARIANT_SUPPORT || IIOP_PROTOCOL */
} /*allocPickle*/


JAVAEXPORT(IluPickle_nativeInPickle, void)
    JIluPickle jh_pickle,
    JIluCall jh_call  /* jh_call is null tested in java */
    ENDJAVAEXPORT
{
  #if (defined(ADD_VARIANT_SUPPORT) || defined(IIOP_PROTOCOL))
    ilu_Pickle* cPicklePtr;
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    PROLOG(CALL_IN, ILUJAVA_VOID)
    cPicklePtr = allocPickle(JENV_ACTUAL jh_pickle);
    if (cPicklePtr==0) {
        _ilujava_throwException(JENV_ACTUAL 30, INITIALIZE_EXC, "pickle not initialized");
        return;
    }
    ilu_InputPickle(cCall, cPicklePtr, (ilu_Type)ILU_NIL, &ioerrs);
    ERRCHECK(ILUJAVA_VOID)
  #else
    BADCASE;
  #endif /* ADD_VARIANT_SUPPORT || IIOP_PROTOCOL */
} /*IluPickle_nativeInPickle*/


JAVAEXPORT(IluPickle_nativeFinalizePickle, void)
    JIluPickle jh_pickle, Jlong savedYpickleptr
    ENDJAVAEXPORT
{
  #if (defined(ADD_VARIANT_SUPPORT) || defined(IIOP_PROTOCOL))
    ilu_Error errs = ILU_INIT_NO_ERR;
    ilu_Pickle* cPicklePtr;
    PUT_IluPickle_ypickleptr(jh_pickle, 0);
    cPicklePtr =  * (ilu_Pickle**) &savedYpickleptr;
    if (cPicklePtr) {
        ilu_FreePickle(cPicklePtr, &errs);
        ilu_free(cPicklePtr);
        ILU_HANDLED(errs);
    }
    /* don't finalize the JIluCall here: it has its own finalization */
  #endif /* ADD_VARIANT_SUPPORT || IIOP_PROTOCOL */
} /*IluPickle_nativeFinalizePickle*/


JAVAEXPORT(IluPickle_nativeStartPickleInsert, void)
    JIluPickle jh_IluPickle, 
    JIluCall jh_call
    ENDJAVAEXPORT
{
  #if (defined(ADD_VARIANT_SUPPORT) || defined(IIOP_PROTOCOL))
    JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    JGC_WP_TYPE(JIluPickle) jwp_pickle;
    ilu_Error errs = ILU_INIT_NO_ERR;
    ilu_boolean success;
    ilu_Call cCall;
    ilu_Pickle* cPicklePtr;
    jwp_pickle = JGC_GET_WP_FROM_JAVA_OBJECT(jh_IluPickle);
    cPicklePtr = GET_IluPickle_ypickleptr(JGC_WP_REVEAL(jwp_pickle));
    if (cPicklePtr) {
        _ilujava_throwException(JENV_ACTUAL 30, INITIALIZE_EXC, "IluPickle already defined");
        return;
    }
    CALLJAVAEXPORT(IluCall_initCallShell) 
        (JIluCall)JGC_WP_REVEAL(jwp_call)
        ENDCALLJAVAEXPORT;
    cCall = GET_IluCall_yCall(JGC_WP_REVEAL(jwp_call));
    success = ilu_StartPickle((ilu_Call_s *)cCall, (ilu_Type)ILU_NIL, &errs);
    if (ILU_ERRNOK(errs)) {
        _ilujava_IluErrorToException(&errs, "nativeStartPickle1"); 
        return;
    }
    allocPickle(JENV_ACTUAL (JIluPickle)JGC_WP_REVEAL(jwp_pickle));
    PUT_IluCall_jNeedsSizing(JGC_WP_REVEAL(jwp_call), (Jboolean) ilu_CallNeedsSizing(cCall));
    TRANSITION(CALL_SZ)
    return;
  #else
    BADCASE;
  #endif /* ADD_VARIANT_SUPPORT || IIOP_PROTOCOL */
} /*IluPickle_nativeStartPickleInsert*/


JAVAEXPORT(IluPickle_nativeStartPickleExtract, void)
    JIluPickle jh_IluPickle, 
    JIluCall jh_call, 
    JString jh_uid_expected
    ENDJAVAEXPORT
{
  #if (defined(ADD_VARIANT_SUPPORT) || defined(IIOP_PROTOCOL))
    JGC_WP_TYPE(JIluCall) jwp_call;
    JGC_WP_TYPE(JIluPickle) jwp_pickle;
    char* cuid_expected = 0;
    ilu_Error errs = ILU_INIT_NO_ERR;
    ilu_boolean success;
    ilu_Call cCall;
    ilu_Pickle* cPicklePtr;
    
    jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    jwp_pickle = JGC_GET_WP_FROM_JAVA_OBJECT(jh_IluPickle);
    if (jh_uid_expected) {
        cuid_expected = J2Cstring8X(JENV_ACTUAL jh_uid_expected);
    }
    CALLJAVAEXPORT(IluCall_initCallShell) 
        (JIluCall)JGC_WP_REVEAL(jwp_call) 
        ENDCALLJAVAEXPORT;
    cCall = GET_IluCall_yCall(JGC_WP_REVEAL(jwp_call));
    success = ilu_StartPickle((ilu_Call_s *)cCall, (ilu_Type)ILU_NIL, &errs);
    if (ILU_ERRNOK(errs)) {
        _ilujava_IluErrorToException(&errs, "nativeStartPickle1"); 
        goto freeAndReturn;
    }
    cPicklePtr = GET_IluPickle_ypickleptr(JGC_WP_REVEAL(jwp_pickle));
    if (cPicklePtr == 0) {
        _ilujava_throwException(JENV_ACTUAL 30, INITIALIZE_EXC, "pickle finalized");
        return;
    }
    if (cuid_expected) {
        ilu_string cuid_found;
        if (cPicklePtr == 0) {
            _ilujava_IluErrorToException(0, "IluPickle not defined");
            return;
        }
        cuid_found = GET_IluPickle_yuuid(JGC_WP_REVEAL(jwp_pickle));
        if (cuid_found == 0) {
            ILU_CLER(errs);
            cuid_found = ilu_PickleType(*cPicklePtr, &errs);
            if (ILU_ERRNOK(errs)) {
                _ilujava_IluErrorToException(&errs, "nativeStartPickle2");
                return;
            }
        }
        if (strcmp(cuid_expected, cuid_found))  {
            ilu_Class cClassFound;
            ilu_Class cClassExcpected;
            /* type uuid's are not equal */
            cClassExcpected = ilu_FindClassFromID(cuid_expected);
            if (cClassExcpected == 0) {
                /* CONFUSING BUT OK
                 * If the expected class came from the program
                 *    it is perfectly ok to raise an exception if its not known
                 * If this is simply from reading the pickle's uuid field
                 *    it would have matched the strcmp
                 */
                _ilujava_throwException(JENV_ACTUAL 44, ADAPTER_EXC, "expected class unknown");
                goto freeAndReturn;
            }
            cClassFound = ilu_FindClassFromID(cuid_found);
            if (cClassFound==0) {
                /* ?? UNFORTUNATE !!
                 * We would like to be able to accept some unknown subclass.
                 * Reading of objects of unknown classes is possible, but
                 * I don't know how to test 
                 *   a) whether it is an object type
                 *   b) whether the object type is a sub type
                 */
                _ilujava_throwException(JENV_ACTUAL 44, ADAPTER_EXC, "found class unknown");
                goto freeAndReturn;
            }
            if (!ilu_IsSubObjectType(cClassExcpected, cClassFound)) {
                _ilujava_throwException(JENV_ACTUAL 44, ADAPTER_EXC, "not a subclass");
                goto freeAndReturn;
            }            
        }
        ilu_free(cuid_expected);
    }
    ILU_CLER(errs);
    success = ilu_ReadPickle(cCall, *cPicklePtr, &errs); 
       /* passes pickle ownership */
    if (!success) {
       _ilujava_IluErrorToException(&errs, "failed");
       return;
    }
    TRANSITION(CALL_IN)
    return;
    
    /* error returns */
    freeAndReturn:
        if (cuid_expected) {
            ilu_free(cuid_expected);
        }
  #else
    BADCASE;
  #endif /* ADD_VARIANT_SUPPORT || IIOP_PROTOCOL */
} /*IluPickle_nativeStartPickleExtract*/


JAVAEXPORT(IluPickle_nativeEndPickleWork, void)
    JIluPickle jh_pickle, 
    JIluCall jh_call
    ENDJAVAEXPORT
    /* doesn't raise exceptions (if call!=null); important ! */
{
  #if (defined(ADD_VARIANT_SUPPORT) || defined(IIOP_PROTOCOL))
    ilu_Error errs = ILU_INIT_NO_ERR;
    ilu_boolean success = ilu_FALSE;
    if (jh_pickle && jh_call) {
        ilu_Call cCall;
        ilu_Pickle* cPicklePtr;
        cPicklePtr = GET_IluPickle_ypickleptr(jh_pickle);
        cCall = GET_IluCall_yCall(jh_call);
        if (cCall && cPicklePtr) {
           /* don't free pickle memory here; free in finalization */
           success = ilu_EndPickle(cCall, cPicklePtr, &errs);
           ILU_HANDLED(errs);
        }
    }
  #endif /* ADD_VARIANT_SUPPORT || IIOP_PROTOCOL */
} /*IluPickle_nativeEndPickleWork*/


JAVAEXPORT(IluPickle_nativeWritePickle, void)
    JIluPickle jh_pickle, 
    JIluCall jh_call, 
    Jint sz, 
    JString jh_uid
    ENDJAVAEXPORT
    /* no changes about memory ownership */
{
  #if (defined(ADD_VARIANT_SUPPORT) || defined(IIOP_PROTOCOL))
  	JGC_WP_TYPE(JIluCall) jwp_call = JGC_GET_WP_FROM_JAVA_OBJECT(jh_call);
    ilu_Error errs = ILU_INIT_NO_ERR;
    ilu_boolean success;
    ilu_Call cCall;
    ilu_string cuuid = J2Cstring8X(JENV_ACTUAL jh_uid);
    cCall = GET_IluCall_yCall(JGC_WP_REVEAL(jwp_call));
    JTRACE(_ilujava_pickleFlag>0, 
        ("$ nativeWritePickle %d<%s>\n", strlen(cuuid), cuuid));
    success = ilu_WritePickle(cCall, (ilu_cardinal) sz, cuuid, &errs);
    ilu_free(cuuid);
    if (ILU_ERRNOK(errs)) {
        _ilujava_IluErrorToException(&errs, "nativeWritePickle"); 
        return;
    }
    TRANSITION(CALL_OUT)
  #else
    BADCASE;
  #endif /* ADD_VARIANT_SUPPORT || IIOP_PROTOCOL */
} /*IluPickle_nativeWritePickle*/


JAVAEXPORT(IluPickle_nativeUuidPickle, JString)
    JIluPickle jh_pickle
    ENDJAVAEXPORT
{
  #if (defined(ADD_VARIANT_SUPPORT) || defined(IIOP_PROTOCOL))
    JString jh_string;
    ilu_Error errs = ILU_INIT_NO_ERR;
    ilu_string cuuid;
    ilu_Pickle* cPicklePtr;
    cPicklePtr = GET_IluPickle_ypickleptr(jh_pickle);
    if (cPicklePtr == 0) {
        _ilujava_throwException(JENV_ACTUAL 10, INTERNAL_EXC, "pickle falsely finalized");
        return 0;
    }
    cuuid = ilu_PickleType(*cPicklePtr, &errs);
        /* if the memory would have been freed: no smash, only wrong result */
    if (ILU_ERRNOK(errs)) {
        _ilujava_IluErrorToException(&errs, "nativeUuidPickle"); 
        return 0;
    }
    PUT_IluPickle_yuuid(jh_pickle, cuuid); 
        /* out parameter belongs to the pickle */
    JTRACE(_ilujava_pickleFlag>0, 
        ("$ nativeUuidPickle %d<%s>\n", strlen(cuuid), cuuid));
    jh_string = IluJava_JString_fromA0(JENV_ACTUAL (char*) cuuid);
    return jh_string;
  #else
    BADCASE1;
  #endif /* ADD_VARIANT_SUPPORT || IIOP_PROTOCOL */
} /*IluPickle_nativeUuidPickle*/


JAVAEXPORT(IluTypeCode_nativeInitTypeCode, void)
    JIluTypeCode jh_typeCode, 
    JString jh_uid
    ENDJAVAEXPORT
{
  #if (defined(ADD_VARIANT_SUPPORT) || defined(IIOP_PROTOCOL))
    ilu_string cuuid;
    cuuid = IluJava_JString_toheap8x(JENV_ACTUAL jh_uid);
    PUT_IluTypeCode_yuuid(jh_typeCode, cuuid); 
    JTRACE(_ilujava_pickleFlag>0,
            ("$ nativeInitTypeCode %d<%s>\n", strlen(cuuid), cuuid));
  #endif /* ADD_VARIANT_SUPPORT || IIOP_PROTOCOL */
} /*IluPickle_nativeInitTypeCode*/


typedef enum {
    ctk_null, ctk_void,
    ctk_short, ctk_long, ctk_ushort, ctk_ulong,
    ctk_float, ctk_double, ctk_boolean, ctk_char,
    ctk_octet, ctk_any, ctk_TypeCode, ctk_Principal, ctk_objref,
    ctk_struct, ctk_union, ctk_enum, ctk_string,
    ctk_sequence, ctk_array, ctk_alias, ctk_except,
    ctk_longlong, ctk_ulonglong, ctk_longdouble,
    ctk_wchar, ctk_wstring, ctk_fixed
    } corba_TypeKind;
    
/* Not implemented in ILU for varying reasons:
 * 	null, void, TypeCode, Principal, except, fixed
 * implemented specially: wstring
 */
 
JAVAEXPORT(IluTypeCode_nativeSpecialInitTypeCode, void)
    JIluTypeCode jh_iluTypeCode, 
    Jint corbaTypeKind
    ENDJAVAEXPORT
{
  #if (defined(ADD_VARIANT_SUPPORT) || defined(IIOP_PROTOCOL))
    JGC_WP_TYPE(JIluTypeCode) jwp_iluTypeCode = JGC_WP_MAKE(jh_iluTypeCode);
    ilu_string cuuid;
    JString jh_uid;
    switch ((int) corbaTypeKind) {
        case (int) ctk_short:
            cuuid = (char*) ilu_TypeID_ilu_shortinteger;
            break;
        case (int) ctk_long:
            cuuid = (char*) ilu_TypeID_ilu_integer;
            break;
        case (int) ctk_ushort:
            cuuid = (char*) ilu_TypeID_ilu_shortcardinal;
            break;
        case (int) ctk_ulong:
            cuuid = (char*) ilu_TypeID_ilu_cardinal;
            break;
        case (int) ctk_string:
            cuuid = (char*) ilu_TypeID_ilu_CString;
            break;
        case (int) ctk_boolean:
            cuuid = (char*) ilu_TypeID_ilu_boolean;
            break;
        case (int) ctk_char:
            cuuid = (char*) ilu_TypeID_ilu_shortcharacter;
            break;
        case (int) ctk_wchar:
            cuuid = (char*) ilu_TypeID_ilu_character;
            break;
        case (int) ctk_float:
            cuuid = (char*) ilu_TypeID_ilu_shortreal;
            break;
        case (int) ctk_double:
            cuuid = (char*) ilu_TypeID_ilu_real;
            break;
        case (int) ctk_octet:
            cuuid = (char*) ilu_TypeID_ilu_byte;
            break;
        case (int) ctk_longlong:
            cuuid = (char*) ilu_TypeID_ilu_longinteger;
            break;
        case (int) ctk_ulonglong:
            cuuid = (char*) ilu_TypeID_ilu_longcardinal;
            break;
        case (int) ctk_objref:
            cuuid = (char*) ilu_TypeID_ilu_CORBA_Object;
            break;
        case (int) ctk_any:
            cuuid = (char*) ilu_TypeID_ilu_pickle;
            break;
    }
    jh_uid = IluJava_JString_fromA0(JENV_ACTUAL (char*) cuuid);
    PUT_IluTypeCode_yuuid(JGC_WP_REVEAL(jwp_iluTypeCode), cuuid); 
    PUT_IluTypeCode_jjuid(JGC_WP_REVEAL(jwp_iluTypeCode), jh_uid); 
    JGC_WP_RELEASE(jwp_iluTypeCode);
  #endif /* ADD_VARIANT_SUPPORT || IIOP_PROTOCOL */
} /* IluTypeCode_nativeSpecialInitTypeCode */


JAVAEXPORT(IluTypeRep_nativeRegisterSome, void)
    JIluTypeRep jh_TypeRep, 
    JString jh_name, 
    JString jh_islIfName,
    JString jh_islIfBrand, 
    JString jh_uid, 
    Jint elCnt, 
    Jint ilutk, 
    JString jh_baseUid, 
    JArrayOfInt jh_dims
    ENDJAVAEXPORT
{
  #ifdef IIOP_PROTOCOL
    JGC_WP_TYPE(JArrayOfInt) jwp_dims = 0;
    JGC_WP_TYPE(JIluTypeRep) jwp_TypeRep = 0;
    ilu_Error errs = ILU_INIT_NO_ERR;
    ilu_Error err0 = ILU_INIT_NO_ERR;
    ilu_boolean new = ilu_FALSE;
    ilu_boolean freemutex = ilu_FALSE;
    ilu_Type cIluType;
    ilu_string cName = (ilu_string) 
        IluJava_JString_toheap80(JENV_ACTUAL jh_name);
    ilu_string cIslIfBrand = 
        (ilu_string) IluJava_JString_toheap80(JENV_ACTUAL jh_islIfBrand);
    ilu_string cIslIfName = 
        (ilu_string) IluJava_JString_toheap80(JENV_ACTUAL jh_islIfName);
    ilu_string cuid = (ilu_string) IluJava_JString_toheap80(JENV_ACTUAL jh_uid);
    ilu_string cBaseUID = 0;
    if (jh_uid) {
        cBaseUID = (ilu_string) IluJava_JString_toheap80(JENV_ACTUAL jh_baseUid);
    }
    if (jh_dims) {jwp_dims = JGC_WP_MAKE(jh_dims);}
    if (jh_TypeRep) jwp_TypeRep = JGC_WP_MAKE(jh_TypeRep);
    ilu_EnterMutex(ilu_otmu, &errs);
    if (ILU_ERRNOK(err0)) {
        _ilujava_IluErrorToException(&err0, completedNo "RegisterEnumeration");
        goto return_free;
    }
    if (ilutk == 15 /*ilu_optional_tk*/ ) {
        cIluType = ilu_RegisterOptionalType(
            cName,
            cIslIfName,
            cIslIfBrand,
            cuid,
            cBaseUID,
            &new, 
            &errs
            );
        freemutex = ilu_TRUE;
    } else if (ilutk == 16 /*ilu_alias_tk*/ ) {
        cIluType = ilu_RegisterAliasType(
            cName,
            cIslIfName,
            cIslIfBrand,
            cuid,
            cBaseUID,
            &new, 
            &errs
            );
        freemutex = ilu_TRUE;
    } else if (ilutk == 18 /*ilu_sequence_tk*/ ) {
        cIluType = ilu_RegisterSequenceType(
            cName,
            cIslIfName,
            cIslIfBrand,
            cuid,
            cBaseUID,
            elCnt, /*limit*/
            &new, 
            &errs
            );
        freemutex = ilu_TRUE;
    } else if (ilutk == 19 /*ilu_record_tk*/ ) {
        cIluType = ilu_RegisterRecordType(
            cName,
            cIslIfName,
            cIslIfBrand,
            cuid,
            elCnt, (ilu_boolean) 0, (ilu_string) 0,
            &new, 
            &errs
            );
        /* dont freemutex on success */
    } else if (ilutk == 20 /*ilu_array_tk*/ ) {
        Jint* arrayPtr;
        int i;
        ilu_cardinal* x;
        freemutex = ilu_TRUE;
        if (elCnt == 0) {
            _ilujava_throwException(JENV_ACTUAL 22, PARAM_EXC, "0 elements");
            goto return_free_mutex; 
        }
        x = ilu_malloc(elCnt*sizeof(ilu_cardinal));
        arrayPtr = ARRAY_int_GET((JArrayOfInt)JGC_WP_REVEAL(jwp_dims));
        for (i = 0; i < elCnt; i = i+1) {
            x[i] = arrayPtr[i];
        }
        ARRAY_int_RELEASE((JArrayOfInt)JGC_WP_REVEAL(jwp_dims), arrayPtr);
        cIluType = ilu_RegisterArrayType(
            cName,
            cIslIfName,
            cIslIfBrand,
            cuid,
            cBaseUID,
            elCnt, /*no of dimensions*/
            x,
            &new, 
            &errs
            );
        ilu_free(x);
    } else if (ilutk == 21 /*ilu_enumeration_tk*/ ) {
        cIluType = ilu_RegisterEnumerationType(
            cName,
            cIslIfName,
            cIslIfBrand,
            cuid,
            elCnt,
            &new, 
            &errs
            );
        /* dont freemutex on success */
    }
    if (new) {
        if (ILU_ERROK(errs)) {
            PUT_IluTypeRep_yIluType(JGC_WP_REVEAL(jwp_TypeRep), cIluType);
            /* in success case keep the mutex on return
             * for some types but not all !
             */
        } else {
            ILU_HANDLED(errs);
            freemutex = ilu_TRUE;
        }
    } else {
        freemutex = ilu_TRUE;
        ILU_HANDLED(errs);
    }
    return_free_mutex:
    if (freemutex) {
        ILU_CLER(errs);
        ilu_ExitMutex(ilu_otmu, ilu_TRUE, &errs);
        ILU_HANDLED(errs);
    }
    /*fall through*/
    return_free:
        if (cName) {ilu_free(cName);}
        if (cIslIfBrand) {ilu_free(cIslIfBrand);}
        if (cIslIfName) {ilu_free(cIslIfName);}
        if (cuid) {ilu_free(cuid);}
        if (cBaseUID) {ilu_free(cBaseUID);}
        if (jwp_dims) {JGC_WP_RELEASE(jwp_dims);}
        if (jwp_TypeRep) {JGC_WP_RELEASE(jwp_TypeRep);}
  #endif /* def IIOP_PROTOCOL */
} /* IluTypeRep_nativeRegisterSome */


JAVAEXPORT(IluTypeRep_nativeRegisterUnionBase, void)
    JIluTypeRep jh_TypeRep, 
    JString jh_name, 
    JString jh_islIfName,
    JString jh_islIfBrand, 
    JString jh_uid, 
    JString jh_discrUID, 
    Jint armCnt, 
    Jint defaultArm, 
    Jint othersAllowed, 
    Jint valueKind
    ENDJAVAEXPORT
{
  #ifdef IIOP_PROTOCOL
    ilu_cardinal funnyIluDefaultArm = 0;
    ilu_Error errs = ILU_INIT_NO_ERR;
    ilu_Error err0 = ILU_INIT_NO_ERR;
    ilu_boolean new = ilu_FALSE;
    ilu_boolean freemutex = ilu_FALSE;
    ilu_Type cIluType;
    ilu_string cName = 
        (ilu_string) IluJava_JString_toheap80(JENV_ACTUAL jh_name);
    ilu_string cIslIfBrand = 
        (ilu_string) IluJava_JString_toheap80(JENV_ACTUAL jh_islIfBrand);
    ilu_string cIslIfName = 
        (ilu_string) IluJava_JString_toheap80(JENV_ACTUAL jh_islIfName);
    ilu_string cuid = (ilu_string) IluJava_JString_toheap80(JENV_ACTUAL jh_uid);
    ilu_string cDiscUID = 
        (ilu_string) IluJava_JString_toheap80(JENV_ACTUAL jh_discrUID);
    JGC_WP_TYPE(JIluTypeRep) jwp_TypeRep = JGC_WP_MAKE(jh_TypeRep);
    ilu_EnterMutex(ilu_otmu, &errs);
    if (ILU_ERRNOK(err0)) {
        _ilujava_IluErrorToException(&err0, completedNo "RegisterEnumeration");
        goto return_free;
    }
    if ((defaultArm >= 0) && (defaultArm < armCnt)) {
        funnyIluDefaultArm = defaultArm+1;
    }
    cIluType = ilu_RegisterUnionType(
            cName,
            cIslIfName,
            cIslIfBrand,
            cuid,
            cDiscUID,
            armCnt,
            funnyIluDefaultArm, 
            (othersAllowed>0), 
            &new, 
            &errs
            );
    /* dont freemutex on success */
    if (new) {
        if (ILU_ERROK(errs)) {
            PUT_IluTypeRep_yIluType(JGC_WP_REVEAL(jwp_TypeRep), cIluType);
            /* in success case keep the mutex on return
             * for some types but not all !
             */
        } else {
            ILU_HANDLED(errs);
            freemutex = ilu_TRUE;
        }
    } else {
        ILU_HANDLED(errs);
        freemutex = ilu_TRUE;
    }
    if (freemutex) {
        ILU_CLER(errs);
        ilu_ExitMutex(ilu_otmu, ilu_TRUE, &errs);
        ILU_HANDLED(errs);
    }
    /*fall through*/
    return_free:
        if (cName) {ilu_free(cName);}
        if (cDiscUID) {ilu_free(cDiscUID);}
        if (cIslIfBrand) {ilu_free(cIslIfBrand);}
        if (cIslIfName) {ilu_free(cIslIfName);}
        if (cuid) {ilu_free(cuid);}
        JGC_WP_RELEASE(jwp_TypeRep);
  #endif /* def IIOP_PROTOCOL */
} /*IluTypeRep_nativeRegisterUnionBase*/


JAVAEXPORT(IluTypeRep_nativeRegisterEnumerationElement, void)
    JIluTypeRep jh_TypeRep, 
    Jint elNum, 
    JString elIslName, 
    Jint elVal
    ENDJAVAEXPORT
{
  #ifdef IIOP_PROTOCOL
    JGC_WP_TYPE(JIluTypeRep) jwp_TypeRep = JGC_WP_MAKE(jh_TypeRep);
    ilu_Error errs = ILU_INIT_NO_ERR;
    if (((int)elNum) < 0) {
        /*special locking case */
        ilu_ExitMutex(ilu_otmu, ilu_FALSE, &errs);
    } else {
        /* normal case */
        ilu_boolean success;
        ilu_string cName;
        ilu_Type cIluType = GET_IluTypeRep_yIluType(JGC_WP_REVEAL(jwp_TypeRep));
        if (! cIluType) return;  /*impossible*/
        cName = (ilu_string) IluJava_JString_toheap80(JENV_ACTUAL elIslName);
        success = ilu_RegisterEnumerationElement(cIluType, 
            elNum, cName, elVal, &errs);
        ilu_free(cName);
    }
    ILU_HANDLED(errs);
    JGC_WP_RELEASE(jwp_TypeRep);
  #endif /* def IIOP_PROTOCOL */
} /* IluTypeRep_nativeRegisterEnumerationElement */


JAVAEXPORT(IluTypeRep_nativeRegisterRecordField, void)
    JIluTypeRep jh_TypeRep, 
    Jint index, 
    JString jh_fldIslName, 
    JString jh_IslIfBrand
    ENDJAVAEXPORT
{
  #ifdef IIOP_PROTOCOL
    JGC_WP_TYPE(JIluTypeRep) jwp_TypeRep = 0;
    JGC_WP_TYPE(JString) jwp_IslIfBrand = 0;
    JGC_WP_TYPE(JString) jwp_fldIslName = 0;
    ilu_Error errs = ILU_INIT_NO_ERR;
    
    if (jh_TypeRep) {jwp_TypeRep = JGC_WP_MAKE(jh_TypeRep);}
    if (jh_IslIfBrand) {jwp_IslIfBrand = JGC_WP_MAKE(jh_IslIfBrand);}
    if (jh_fldIslName) {jwp_fldIslName = JGC_WP_MAKE(jh_fldIslName);}
    
    if (((int)index) < 0) {
        /*special locking case */
        ilu_ExitMutex(ilu_otmu, ilu_FALSE, &errs);
    } else {
        /* normal case */
        ilu_boolean success;
        ilu_string cName, cBrand;
        ilu_Type cIluType = GET_IluTypeRep_yIluType(JGC_WP_REVEAL(jwp_TypeRep));
        if (! cIluType) return;  /*impossible*/
        cName = (ilu_string) IluJava_JString_toheap80(JENV_ACTUAL (
            JString)JGC_WP_REVEAL(jwp_fldIslName)
            );
        cBrand = (ilu_string) 
            IluJava_JString_toheap80(JENV_ACTUAL 
                (JString)JGC_WP_REVEAL(jwp_IslIfBrand)
                );
        success = ilu_RegisterRecordField(cIluType, 
            index, cName, cBrand, &errs);
        ilu_free(cName);
        ilu_free(cBrand);
    }
    ILU_HANDLED(errs);
    if (jwp_TypeRep) {JGC_WP_RELEASE(jwp_TypeRep);}
    if (jwp_fldIslName) {JGC_WP_RELEASE(jwp_fldIslName);}
    if (jwp_IslIfBrand) {JGC_WP_RELEASE(jwp_IslIfBrand);}
  #endif /* def IIOP_PROTOCOL */
} /* IluTypeRep_nativeRegisterRecordField */


JAVAEXPORT(IluTypeRep_nativeRegisterUnionArm, void)
    JIluTypeRep jh_typeRep, 
    Jint armNum, 
    JString jh_islArmName, 
    JString jh_armUid,
    Jint valnum, 
    JArrayOfInt jh_Ivals, 
    JArrayOfString jh_Svals, 
    Jint valueKind
    ENDJAVAEXPORT
{
  #ifdef IIOP_PROTOCOL
    ilu_Error errs = ILU_INIT_NO_ERR;
    if (((int)valueKind) < 0) {
        /*special locking case */
        ilu_ExitMutex(ilu_otmu, ilu_FALSE, &errs);
        ILU_MUST_BE_SUCCESS(errs);
    } else {
        /* normal case */
        JGC_WP_TYPE(JArrayOfInt) jwp_Ivals = 0;
        JGC_WP_TYPE(JArrayOfString) jwp_Svals = 0;
        Jint* iarrayPtr = 0;
        ilu_ConstantValue_s cvMemory;
        ilu_ConstantValue cv;
        int i;
        int constval;
        ilu_string cstring = 0;
        ilu_string cArmName, cArmTypeUID;
        ilu_Type cIluType = GET_IluTypeRep_yIluType(jh_typeRep);
        ilu_UnionArm arm;
        if (! cIluType) return;  /*impossible*/
        cv = &cvMemory;
        cv->kind = (ilu_ConstantValueKind) valueKind;
        /* error checking */
        switch (cv->kind) {
            case ilu_byte_cvk: 
            case ilu_shortinteger_cvk: 
            case ilu_integer_cvk: 
            case ilu_shortcardinal_cvk: 
            case ilu_cardinal_cvk: 
            case ilu_boolean_cvk: 
                if (jh_Ivals == 0) /*error*/ return;
                if (valnum > (Jint) ARRAY_LENGTH(jh_Ivals)) /*error*/ return;
                jwp_Ivals = JGC_WP_MAKE(jh_Ivals);
                break;
            case ilu_enumeration_cvk: 
                if (jh_Svals == 0) /*error*/ return;
                if (valnum > (Jint) ARRAY_LENGTH(jh_Svals))  /*error*/ return;
                jwp_Svals = JGC_WP_MAKE(jh_Svals);
                break;
            default: /*error*/ 
                /*error*/ return;
        }
        cArmName = 
            (ilu_string) IluJava_JString_toheap80(JENV_ACTUAL jh_islArmName);
        cArmTypeUID = 
            (ilu_string) IluJava_JString_toheap80(JENV_ACTUAL jh_armUid);
        
        arm = ilu_RegisterUnionArm(cIluType, 
            armNum, cArmName, cArmTypeUID, valnum, &errs);
        ilu_free(cArmName);
        ilu_free(cArmTypeUID);
        if (ILU_ERRNOK(errs)) {
            ILU_HANDLED(errs);
            goto any_return;
        }
        for (i = 0; i < valnum; i = i+1) {
            ILU_CLER(errs);
            switch (cv->kind) {
                case ilu_byte_cvk: 
                case ilu_shortinteger_cvk: 
                case ilu_integer_cvk: 
                case ilu_shortcardinal_cvk: 
                case ilu_cardinal_cvk: 
                case ilu_boolean_cvk: 
                    iarrayPtr = 
                        ARRAY_int_GET((JArrayOfInt) JGC_WP_REVEAL(jwp_Ivals));
                    /* recompute each time in loop in case of gc compaction */
                    break;
                case ilu_enumeration_cvk: 
                    break;
                default: /*impossible*/ 
                    break;
            }
            switch (cv->kind) {
                case ilu_byte_cvk: 
                    constval = iarrayPtr[i];
                    cv->value.byte_val = constval; 
                    break;
                case ilu_shortinteger_cvk: 
                    constval = iarrayPtr[i];
                    cv->value.shortinteger_val = constval; 
                    break;
                case ilu_integer_cvk: 
                    constval = iarrayPtr[i];
                    cv->value.integer_val = constval; 
                    break;
                case ilu_shortcardinal_cvk: 
                    constval = iarrayPtr[i];
                    cv->value.shortcardinal_val = constval; 
                    break;
                case ilu_cardinal_cvk: 
                    constval = iarrayPtr[i];
                    cv->value.cardinal_val = constval; 
                    break;
                case ilu_boolean_cvk: 
                    constval = iarrayPtr[i];
                    cv->value.boolean_val = (constval!=0); 
                    break;
                case ilu_enumeration_cvk: 
                    {
                        JString jh_s;
                        jh_s = ARRAY_object_GET(
                            ((JArrayOfString) JGC_WP_REVEAL(jwp_Svals)), 
                            i);
                        cstring = (ilu_string) 
                            IluJava_JString_toheap80(JENV_ACTUAL jh_s);
                        cv->value.enumeration_val = cstring; 
                    }
                    break;
                case ilu_shortreal_cvk: /*huh?*/
                case ilu_string_cvk: /*huh?*/
                case ilu_real_cvk: /*huh?*/
                default: /*impossible*/
                    break;
            }
            if (iarrayPtr) {
                ARRAY_int_RELEASE( 
                    (JArrayOfInt) JGC_WP_REVEAL(jwp_Ivals), iarrayPtr
                    );
                iarrayPtr = 0;
            }
            ilu_RegisterUnionArmValue(arm, i, cv, &errs);
            if (cstring) {ilu_free(cstring);}
            ILU_HANDLED(errs);
        }
        /* fall through */
        any_return: 
            if (jwp_Svals) {
                JGC_WP_RELEASE(jwp_Svals);
            }
            if (jwp_Ivals) {
                JGC_WP_RELEASE(jwp_Ivals);
            }
            return;
    }
    return;
  #endif /* def IIOP_PROTOCOL */
} /* IluTypeRep_nativeRegisterUnionArm */


/* end */

