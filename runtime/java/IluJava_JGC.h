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
/* IluJava_JGC.h */
/* Chris Jacobi, November 18, 1998 2:01 pm PST */

/*
 */
 
/* $Id: IluJava_JGC.h,v 1.16 1999/08/03 01:54:16 janssen Exp $ */

/*
 * Macros to make it possible to switch the implementation
 * of java garbage collection primitives.
 */

#ifndef _ILUJAVA_JGC_
#define _ILUJAVA_JGC_

#include "IluJava_Includes.h"


/* Stuffing Java refs into the C heap is not extremely
 * portable. These macros or routines should make this
 * undertaking a little bit more portable.
 *
 * In Sun's JDK with ONI these are no-ops; nevertheless the 
 * REVEAL operations carries the risk of accessing
 * an object which has already been collected.
 *
 * With Microsoft Java or with the Boehm collector, these 
 * operations will be real.
 *
 * With Microsoft, even stuffing Java refs on the C STACK
 * will cause problems.
 */
 
/* 
 * About ILUOInt's
 * The LSR keeps the invariant that a LSO is not garbage collected
 * while the kernel hangs on to a KO-LSO pair.  This pair is
 * modified only with the server lock held.
 *
 * Note that the invariant is about garbage collection, not about
 * finalization.  All thinkable implementation with finalization
 * will provide the further invariant that an object is not garbage 
 * collected before it is finalized.
 */



/* 
 * JGC_ENABLE, JGC_DISABLE.
 *
 * JGC_ENABLE, JGC_DISABLE are originally modelled
 * to be a thin veneer over RNI's mechanisms and noops otherwise.
 * 
 * The idea is that GC is normally disabled* while in native
 * code.  These calls allow to temporarily enable* and then re-disable*
 * garbage collection.  (But doesn't work the other way!).
 * The asterix in disabled* means that we are not strictly
 * disabling gc, but we are only disabling gc which wouldn't
 * update local object refs. (This makes it useful in JNI also)
 *
 *
 * JGC_FRAME
 *
 * Likewise, JGC_FRAME are modelled to be a thin veneer 
 * over RNI's mechanisms and noops otherwise.
 *
 * The idea is that objects in a frame are updated when
 * the collector does compaction.  (That is the frame
 * semantics in RNI. In JNI, frames simply act as local
 * object refs; update happens even if the macros are noops.
 *
 * JGC_FRAMEs must remain on stack and must not be stuffed
 * int the C heap and must not be passed to other thread.
 * JGC_FRAMEs may or may not prevent garbage collection; ilu
 * restricts itself to requirements about compaction.
 */



#if (defined(RNI))
    
    /*commenbts above*/
    
    #define JGC_ENABLE \
        GCEnable();
    #define JGC_DISABLE \
        GCDisable();

    #define JGC_FRAME_DECL(frameName) \
        GCFrame frameName;
    #define JGC_FRAME_PUSH(frameName, addr, sz) \
        GCFramePush(& frameName, addr, sz);
    #define JGC_FRAME_POP(frameName)  \
        GCFramePop(& frameName);

#else

    /*commenbts above*/
    
    #define JGC_ENABLE 
    #define JGC_DISABLE 

    #define JGC_FRAME_DECL(frameName)  
    #define JGC_FRAME_PUSH(frameName, addr, sz)
    #define JGC_FRAME_POP(frameName)

#endif



/*
 * GLOBALOBJ is used for permanent static objects. 
 * The macros keep track of code moving but do not
 * necessarily prevent GC; this must be done on the 
 * java side.
 * Consider GLOBALOBJ to be assign-once. 
 * ASSIGNTO and GETFROM may need an implied JENV parameter.
 * GLOBALOBJs may or may not prevent garbage collection; ilu
 * restricts itself to requirements about compaction.
 *
 * JGC_GLOBALOBJ_TOJ doesn't need to be local (not directly assignable  
 * to the java side) but can be used like local from within native 
 * code. (rarely used, check usage)
 */
 

#if (defined(RNI))

    #define JGC_GLOBALOBJ_DECL(name) \
        HObject** name = 0
    #define JGC_GLOBALOBJ_ASSIGNTO(name, jh_obj) \
        name = GCGetPtr((HObject *) jh_obj);
    #define JGC_GLOBALOBJ_GETFROM(jh_obj, name) \
         ((HObject*)jh_obj) = *name;
    #define JGC_GLOBALOBJ_TOJ(name) \
         ((HObject*) *((HObject**)(name)))

#elif (defined(ONI))

    #define JGC_GLOBALOBJ_DECL(name) \
        HObject* name = 0
    #define JGC_GLOBALOBJ_ASSIGNTO(name, jh_obj) \
        name = (HObject*) jh_obj;
    #define JGC_GLOBALOBJ_GETFROM(jh_obj, name) \
        ( * (HObject**) & jh_obj ) = name;
    #define JGC_GLOBALOBJ_TOJ(name) \
         ((HObject*) (name))

#elif (defined(JNI))

    #define JGC_GLOBALOBJ_DECL(name) \
        jobject name = 0
    #define JGC_GLOBALOBJ_ASSIGNTO(name, jh_obj) \
        name = (*JENV_ACTUAL_NOCOMMA)->NewGlobalRef(JENV_ACTUAL jh_obj);
    #define JGC_GLOBALOBJ_GETFROM(jh_obj, name) \
        jh_obj = (jobject) name;
        /* jni accepts global refs wherever it takes local refs */
    #define JGC_GLOBALOBJ_TOJ(name) \
        ((jobject) (name))
        /* jni accepts global refs wherever it takes local refs */


#else

    #error "None of RNI, ONI or JNI defined"

#endif




/*
 * Weak pointers
 * (More correctly: disguised pointers)
 * Ilu needs the compacting guarantee. And has two cases of weak
 * pointers.  Referee is always java object.  There is always
 * means to detect referee disapearing; no 0 test is done.
 *
 * Case1:  Sole purpose is update on compaction.  
 *         These weak pointer are in java heap itself
 *         and should not prevent garbage collection any more then other
 *         circular references.  These are never dereferenced
 *         after the memory has been reclaimed: either because
 *         they are circular to the object thei reference too
 *         or because there are other references to the object.   
 * Case2:  Real honest disguised pointers:  Must not prevent
 *         garbage collection.
 *
 * In a future version of ilu we might use different sets
 * of macros to distinguish the two cases.   
 *
 *
 * JGC_WP_TYPE(javatype)
 *     Expands to the type used for weak pointers.
 *
 * JGC_WP_REVEAL(jwp_obj)
 *     Expands to an expression revealing the contents of a weak 
 *     pointer.
 *     NIL is legal but I prefer explicite NIL tests
 *     Returned value needs to be cast in some runtime impls.
 *    
 * JGC_WP_MAKE(jh_obj)
 *     Expands to an expression creating a weak pointer to 
 *     the argument.
 *     NIL is legal
 *    
 * JGC_WP_RELEASE(jwp_obj) 
 *     Expands to an statement telling language vm that this weak 
 *     pointer doesn't need to be updated anymore.
 *     NIL is legal.
 *
 * JGC_GET_WP_FROM_JAVA_OBJECT(jh_obj)
 *     Special convention:  For java objects extending IluWPBase only.
 *     Gets the weak pointer allocated on object creation
 *     NIL is legal.
 *
 * JGC_GET_WP_FROM_JAVA_IluOInt(jh_obj)
 *     like JGC_GET_WP_FROM_JAVA_OBJECT but used if
 *     object is an IluOInt.
 *     Gets the weak pointer allocated on object creation
 *     NIL is legal.
 */
 
#if (defined(RNI))

    /* rni is actually compacting */

    #define JGC_WP_TYPE(javatype) \
        HObject**
    #define JGC_WP_MAKE(jh_obj) \
		(jh_obj ? GCGetPtr((HObject*)jh_obj) : (HObject**) 0)
    #define JGC_WP_REVEAL(jwp_obj) \
		( (HObject*) (jwp_obj ? *jwp_obj : 0) )
    #define JGC_WP_RELEASE(jwp_obj) \
		(jwp_obj ? GCFreePtr(((HObject**)jwp_obj)) : 0)
    #define JGC_GET_WP_FROM_JAVA_OBJECT(jh_obj) \
		(jh_obj ? GET_IluWPBase_ywpx(jh_obj) : 0)
    #define JGC_GET_WP_FROM_IluOInt(jh_obj) \
		(jh_obj ? GET_IluOInt_ywpxIluOInt(jh_obj) : 0)

#elif (defined (ONI))
    
    /* oni is NOT compacting */
    
    #define JGC_WP_TYPE(javatype) \
        javatype
    #define JGC_WP_MAKE(jh_obj) \
        jh_obj
    #define JGC_WP_REVEAL(jwp_obj) \
        (jwp_obj)
    #define JGC_WP_RELEASE(jwp_obj) \
        {/* noop */}
    #define JGC_GET_WP_FROM_JAVA_OBJECT(jh_obj) \
        jh_obj
    #define JGC_GET_WP_FROM_IluOInt(jh_obj) \
        jh_obj

#elif (defined (JNI_NON_COMPACTING) || (defined (JNI) && (ILUJAVA_H_MINORVERSION < 2)))

    /*  Assuming no no memory compaction and that
     *  "local" refs may be used like global refs
     *  in heap and other threads
     */

    #include <jni.h>
    
    #define JGC_WP_TYPE(javatype) \
        jobject
    #define JGC_WP_MAKE(jh_obj) \
        jh_obj
    #define JGC_WP_REVEAL(jwp_obj) \
        (jwp_obj)
    #define JGC_WP_RELEASE(jwp_obj) \
        {/* noop */}
    #define JGC_GET_WP_FROM_JAVA_OBJECT(jh_obj) \
        jh_obj
    #define JGC_GET_WP_FROM_IluOInt(jh_obj) \
        jh_obj

#elif (defined (JNI))

    #if (ILUJAVA_H_MINORVERSION < 2)
       huh ?
       THIS WILL NOT WORK BECAUSE NATIVE WEAK REFS HAVE NOT BEEN INTRODUCED
       BEFORE jdk1.2
    #endif

    #include <jni.h>
    
    #define JGC_WP_TYPE(javatype) \
        jobject
    #define JGC_WP_MAKE(jh_obj) \
        (jh_obj ? (*JENV_ACTUAL_NOCOMMA)->NewWeakGlobalRef(JENV_ACTUAL jh_obj): (jobject) 0)
    #define JGC_WP_REVEAL(jwp_obj) \
        (jwp_obj)
    #define JGC_WP_RELEASE(jwp_obj) \
        if (jwp_obj) { \
            (*JENV_ACTUAL_NOCOMMA)->DeleteWeakGlobalRef(JENV_ACTUAL (jwp_obj));\
        }
    #define JGC_GET_WP_FROM_JAVA_OBJECT(jh_obj) \
        (jh_obj ? GET_IluWPBase_ywpx(jh_obj) : (jobject) 0)
    #define JGC_GET_WP_FROM_IluOInt(jh_obj) \
        (jh_obj ? GET_IluOInt_ywpxIluOInt(jh_obj) : (jobject) 0)

#else
  
    huh ?

#endif


#endif /* _ILUJAVA_JGC_ */
