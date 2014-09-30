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
/* IluJava_JMon.h */
/* $Id: IluJava_JMon.h,v 1.34 1999/08/03 01:54:15 janssen Exp $ */
/* Chris Jacobi, December 1, 1998 1:47 pm PST */

 

#ifndef _ILUJAVA_JMON_H_
#define _ILUJAVA_JMON_H_

#include "IluJava_Includes.h"
#include "IluJava_JGC.h"

#if (ILUJAVA_H_MAJORVERSION != 1)
    fail ; unknown java version
#endif


/* 
 * Macros to make it possible to switch the implementation
 * of monitors without disrupting the actual usage code.
 *
 * All the monitor locking of the ilu java runtime support uses 
 * this file.  Each monitor is either only used from java, or, 
 * used only from the native side.  Keeping monitor use strictly   
 * on one side allows to use different implementations for monitors 
 * on each side.  In the some implementations, the native
 * side monitors are system monitors and not java objects!
 *
 * Unlike java monitors, these native monitors may or may not be 
 * implemented thread re-entrantly; correct ilu use will never reenter   
 * a monitor held by the own thread.
 */


/*
    PUBLIC DEFINITIONS
    
    void ilujava_SpecialDebugPrintf(char *formatSpec, ...)
    
    typedef xxxxx* ILUJAVA_JMON_PTR;
    ILUJAVA_JMON_PTR ILUJAVA_MON_ALLOC()
    void ILUJAVA_MON_FREE(ILUJAVA_JMON_PTR m) /+statement; not expression+/
    
    xxx ILUJAVA_MON_ENTER(ILUJAVA_JMON_PTR m)
    xxx ILUJAVA_MON_EXIT(ILUJAVA_JMON_PTR m)
    xxx ILUJAVA_MON_NOTIFY(ILUJAVA_JMON_PTR m)
    xxx ILUJAVA_MON_WAIT(ILUJAVA_JMON_PTR m)
    
    typedef xxxxx* ILUJAVA_SYSTHREAD_stPtr;
    ILUJAVA_SYSTHREAD_stPtr ILUJAVA_SYSTHREAD_sysThreadSelf()
    
    /+optional; only for debugging+/
    void* ILUJAVA_SYSTHREAD_javaThreadFromSysThread(ILUJAVA_SYSTHREAD_stPtr)
    
*/



/* Consider includes to be private */
#include "IluJava_Includes.h"
#include "IluJava_JMem.h"


/* public */
#define ILUJAVA_MON_ALLOC() \
    ilujava_mon_allocate(JENV_ACTUAL_NOCOMMA)

/* public */
#define ILUJAVA_MON_FREE(monitorHandle) \
    if ( monitorHandle ) { \
        ilujava_mon_destroyDeallocate( JENV_ACTUAL  monitorHandle ); \
    } \
    monitorHandle = 0; 

/* public */
extern void ilujava_SpecialDebugPrintf(char *formatSpec, ...);
    /* Like ilu_DebugPrintf but much more direct and without locking
     * so it could be used to debug the locking code.  Of course
     * use is ugly and unlocked...
     */


/* public abstract types for external use...
 */
typedef struct ilujava_sys_mon_size_unknown ilujava_sys_mon_t;
typedef ilujava_sys_mon_t* ILUJAVA_JMON_PTR;




/* Private concrete types for internal use only.
 * Shared by implementations which use lists; 
 * not used by other implementations...
 */
typedef struct _ilujava_MonitorRecS_s* ilujava_MonitorRef;
typedef struct _ilujava_MonitorRecS_s {
    char* theRealLock;
    ilujava_MonitorRef next;
} ilujava_MonitorRecS;
  


/*
 * Main switch, defining internal symbols which guide the actual
 * implementation used afterwards.
 */
#if (defined(NetscapeVM_ONI))
    /* With netscape 4.05 vm on solaris 2.5.1 I got 
     * a "symbol not found: _CurrentThread" error
     *
     * Note that this is for the VM FROM Netscape; not tha java plugin
     * or activator running WITHIN Netscape.  This has been tried
     * only on a sparc station with solaris 2.5.1 and netscape 4.05.
     *
     * This symbol was used because of expanding the sysThreadSelf macro.
     * Therefore we don't use systhreads...
     */
    #define useONIWithFakeJavaMonitors 1
#elif (defined(JNI))  
    #define useJNIMonitors 1
#elif (! defined(WIN32))  
    #ifdef ILUJAVA_H_JDK12POSTBETA
        /* (undef symbol) #define useONI2WithSysMonitors 1 */
        #define useONIWithRealJavaMonitors 1
    #else
        #define useONIWithSysMonitors 1
    #endif
    /*#define useONI2WithSysMonitors 1*/
    /*#define useONIWithRealJavaMonitors 1  */
#else   
    #ifdef RNI
        #define useWin32WithRNI 1
    #else
        #define useONIWithRealJavaMonitors 1
    #endif  
#endif  



/*
 * We know the major branch;
 * Switch according to internal symbol preset above
 * and set symbol for c side
 */


#if defined(useONI2WithSysMonitors)
    #define IluJava_JMon_WITH_ONI_AND_SYSMONITORS 1

    #include <sys_api.h>
    #include <interpreter.h>  
    typedef struct sys_thread java_sys_thread_t;
    typedef java_sys_thread_t* ILUJAVA_SYSTHREAD_stPtr;
  

    /* Outside this header file and its implementation
     * don't use ilujava_mon_allocate directly;  
     * but use the ILUJAVA_MON_ALLOC macro instead
     */
    extern ILUJAVA_JMON_PTR ilujava_mon_allocate();


    /* Outside this header file and its implementation
     * don't use ilujava_mon_destroyDeallocate directly; 
     * but use the ILUJAVA_MON_FREE macro instead
     */
    extern void ilujava_mon_destroyDeallocate(ILUJAVA_JMON_PTR);


    /* private */
    #define TheRealLock(monitorHandle) \
        (sys_mon_t *) ((ilujava_MonitorRef) monitorHandle)->theRealLock

    #define ILUJAVA_MON_ENTER(monitorHandle) \
        (int) sysMonitorEnter( sysThreadSelf(), TheRealLock(monitorHandle) ) 

    #define ILUJAVA_MON_EXIT(monitorHandle) \
        (int) sysMonitorExit( sysThreadSelf(), TheRealLock(monitorHandle) )

    #define ILUJAVA_MON_NOTIFY(monitorHandle) \
        (int) sysMonitorNotify( sysThreadSelf(), TheRealLock(monitorHandle) )

    #define ILUJAVA_MON_NOTIFYALL(monitorHandle) \
        (int) sysMonitorNotifyAll( sysThreadSelf(), TheRealLock(monitorHandle) )


    #define ILUJAVA_MON_WAIT(monitorHandle, timeout) \
        (int) sysMonitorWait( sysThreadSelf(), TheRealLock(monitorHandle), timeout)

  
    /*
     * Threading primitives for LockTech.
     * Sys threads and java threads may or may not be the same thing.
     * Very few primitives; most thread operations are done by
     * calling back into java.
     *
     * Outside this header file, this is used for lock teck, but not 
     * for JNI environment
     */
    
    #define ILUJAVA_SYSTHREAD_sysThreadSelf() \
        (ILUJAVA_SYSTHREAD_stPtr) sysThreadSelf()

    #define ILUJAVA_SYSTHREAD_javaThreadFromSysThread(sysThread) \
        ILUJAVA_SYSTHREAD_javaThreadFromSysThread_not_defined()

#endif  /* useONI2WithSysMonitors */


#if defined(useONIWithSysMonitors)
    #define IluJava_JMon_WITH_ONI_AND_SYSMONITORS 1

    #include <sys_api.h>
    /*
     *  jdk-1.0.2 WARNING
     *  ***********
     * 
     *  sys_api.h as we got it here at xerox, in
     *  /project/java/jdk-1.0.2/include/sys_api.h
     *  has a bug:  It imports sysmacros_md.h
     *  which is not available.  We have made a
     *  a private copy of sysmacros_md to fix this
     *  java bug.  Here at Xerox we do not use 
     *  Sun's version which might be available in a 
     *  source release.
     *
     *  See sysmacros_md.h
     *
     * This has been fixed on later jdk releases
     */
   
    /* Needed to access SysThread2EE */
    #if (ILUJAVA_H_MINORVERSION >= 2)
        #include <interpreter.h>
    #endif

  
    typedef struct sys_thread java_sys_thread_t;
    typedef java_sys_thread_t* ILUJAVA_SYSTHREAD_stPtr;
  

    /* Outside this header file and its implementation
     * don't use ilujava_mon_allocate directly;  
     * but use the ILUJAVA_MON_ALLOC macro instead
     */
    extern ILUJAVA_JMON_PTR ilujava_mon_allocate();


    /* Outside this header file and its implementation
     * don't use ilujava_mon_destroyDeallocate directly; 
     * but use the ILUJAVA_MON_FREE macro instead
     */
    extern void ilujava_mon_destroyDeallocate(ILUJAVA_JMON_PTR);

  
    /* Defining an optional environment which has been 
     * introduced with java jdk1.2 
     * (This is used sofar only within this header file)
     */
    #if (ILUJAVA_H_MINORVERSION < 2)
          #define OPTENV
    #else
        #ifdef ILUJAVA_H_JDK12BETA2 
            #ifdef ILUJAVA_H_NATIVE_THREADS
                /* native threads */
                #define OPTENV SysThread2EE(sysThreadSelf()),
            #else
                /* green threads */
                #define OPTENV SysThread2EE(greenThreadSelf()),
            #endif 
        #else
            /*jdk1.2beta3, jdk1.2beta4 */
            #define OPTENV sysThreadSelf(),
        #endif
    #endif /* ILUJAVA_H_MINORVERSION */


    /* private */
    #define TheRealLock(monitorHandle) \
        (sys_mon_t *) ((ilujava_MonitorRef) monitorHandle)->theRealLock

    #define ILUJAVA_MON_ENTER(monitorHandle) \
        (int) sysMonitorEnter( OPTENV TheRealLock(monitorHandle) )

    #define ILUJAVA_MON_EXIT(monitorHandle) \
        (int) sysMonitorExit(OPTENV TheRealLock(monitorHandle) )

    #define ILUJAVA_MON_NOTIFY(monitorHandle) \
        (int) sysMonitorNotify(OPTENV TheRealLock(monitorHandle) )

    #define ILUJAVA_MON_NOTIFYALL(monitorHandle) \
        (int) sysMonitorNotifyAll(OPTENV TheRealLock(monitorHandle) )


    #if (ILUJAVA_H_MINORVERSION == 0)
        #define ILUJAVA_MON_WAIT(monitorHandle, timeout) \
            (int) sysMonitorWait( TheRealLock(monitorHandle) , timeout);
    #elif (ILUJAVA_H_MINORVERSION == 1)
        #define ILUJAVA_MON_WAIT(monitorHandle, timeout) \
            (int) sysMonitorWait( \
            OPTENV TheRealLock(monitorHandle), timeout , FALSE)
    #elif defined(ILUJAVA_H_JDK12BETA2)
        /*jdk1.2beta2*/
        #define ILUJAVA_MON_WAIT(monitorHandle, timeout) \
            (int) sysMonitorWait( \
            OPTENV TheRealLock(monitorHandle), timeout , FALSE)
    #elif defined(ILUJAVA_H_JDK12BETA3)
        /*jdk1.2beta3*/
        #define ILUJAVA_MON_WAIT(monitorHandle, timeout) \
            (int) sysMonitorWait(OPTENV TheRealLock(monitorHandle), timeout)
    #elif (ILUJAVA_H_MINORVERSION == 2)
        /*jdk1.2beta4 jdk1.2fcs*/
        #define ILUJAVA_MON_WAIT(monitorHandle, timeout) \
            (int) sysMonitorWait(OPTENV TheRealLock(monitorHandle), timeout)
    #else
         huh?
    #endif

  
    /*
     * Threading primitives for LockTech.
     * Sys threads and java threads may or may not be the same thing.
     * Very few primitives; most thread operations are done by
     * calling back into java.
     *
     * Outside this header file, this is used for lock teck, but not 
     * for JNI environment
     */
    #if (ILUJAVA_H_MINORVERSION <= 1)
        
        /* jdk1.1 on unix */
    
        #define ILUJAVA_SYSTHREAD_sysThreadSelf() \
            (ILUJAVA_SYSTHREAD_stPtr) sysThreadSelf()

        #define ILUJAVA_SYSTHREAD_javaThreadFromSysThread(sysThread) \
            (void *) sysThreadGetBackPtr((sys_thread_t *) sysThread)

    #else /*ILUJAVA_H_MINORVERSION 2 or larger */
        
        /* jdk1.2xx on unix */

        #if (defined(ILUJAVA_H_NATIVE_THREADS) || defined (ILUJAVA_H_JDK12POSTBETA))
            /* native threads */
            #define ILUJAVA_SYSTHREAD_sysThreadSelf() \
                (ILUJAVA_SYSTHREAD_stPtr) sysThreadSelf()
        #else  
            /* green threads */
            #define ILUJAVA_SYSTHREAD_sysThreadSelf() \
                (ILUJAVA_SYSTHREAD_stPtr) greenThreadSelf()
        #endif

        #define ILUJAVA_SYSTHREAD_javaThreadFromSysThread(sysThread) \
            ILUJAVA_SYSTHREAD_javaThreadFromSysThread_not_defined()

    #endif /* ILUJAVA_H_MINORVERSION */

#endif  /* useONIWithSysMonitors */




#if defined(useONIWithFakeJavaMonitors)

    #define IluJava_JMon_WITH_ONI_AND_FAKEJAVAMONITORS 1
    #include <sys_api.h>
    typedef struct sys_thread java_sys_thread_t;
    typedef java_sys_thread_t* ILUJAVA_SYSTHREAD_stPtr;


    /* Portable apps don't use ilujava_mon_allocate directly;  
     * use the ILUJAVA_MON_ALLOC macro instead 
     */
    extern ILUJAVA_JMON_PTR ilujava_mon_allocate();

    /* Portable apps don't use ilujava_mon_destroyDeallocate directly; 
     * use the ILUJAVA_MON_FREE macro instead 
     */
    extern void ilujava_mon_destroyDeallocate(ILUJAVA_JMON_PTR);

    /* private */
    #define TheRealLock(monitorHandle) \
        (unsigned int) ((ilujava_MonitorRef) monitorHandle)->theRealLock

    #define ILUJAVA_MON_ENTER(monitorHandle) \
        monitorEnter( TheRealLock(monitorHandle) )

    #define ILUJAVA_MON_EXIT(monitorHandle) \
        monitorExit( TheRealLock(monitorHandle) )

    #define ILUJAVA_MON_NOTIFY(monitorHandle) \
        monitorNotify( TheRealLock(monitorHandle) )


    #define ILUJAVA_MON_NOTIFYALL(monitorHandle) \
        monitorNotifyAll( TheRealLock(monitorHandle) )


    #define ILUJAVA_MON_WAIT(monitorHandle, timeout) \
        monitorWait( TheRealLock(monitorHandle), (int) timeout )


    /*
     * Threading primitives.
     * Sys threads and java threads may or may not be the same thing.
     * Very few primitives; most thread operations are done by
     * calling back into java.
     */

    #include <threads.h>
  
    /*private*/
    /*but don't for netscape*/
    int  threadSelf(void);
  
    /* public: We need to check thread identitries */
    #define ILUJAVA_SYSTHREAD_sysThreadSelf() \
        (ILUJAVA_SYSTHREAD_stPtr) threadSelf()
    
    /* semi-public: We need this for debugging */
    #define ILUJAVA_SYSTHREAD_javaThreadFromSysThread(sysThread) \
        (void *) (sysThread)

#endif  /* useONIWithFakeJavaMonitors */




#if defined(useWin32WithRNI)
    /* ******** we're using Microsoft's Raw Native Interface ******** */
    #define IluJava_JMon_WITH_RNI 1
    
    typedef long ILUJAVA_SYSTHREAD_stPtr;

    /* Portable apps don't use ilujava_mon_allocate directly;  
     * use the ILUJAVA_MON_ALLOC macro instead
     */
    extern ILUJAVA_JMON_PTR ilujava_mon_allocate();

    /* Portable apps don't use ilujava_mon_destroyDeallocate directly; 
     * use the ILUJAVA_MON_FREE macro instead
     */
    extern void ilujava_mon_destroyDeallocate(ILUJAVA_JMON_PTR);


    #define TheRealLock(monitorHandle) \
        (unsigned int) * ((HObject**) monitorHandle)

    #define ILUJAVA_MON_ENTER(monitorHandle) \
        monitorEnter( TheRealLock(monitorHandle) )


    #define ILUJAVA_MON_EXIT(monitorHandle) \
        monitorExit( TheRealLock(monitorHandle) )


    #define ILUJAVA_MON_NOTIFY(monitorHandle) \
        monitorNotify( TheRealLock(monitorHandle) )


    #define ILUJAVA_MON_NOTIFYALL(monitorHandle) \
        monitorNotifyAll( TheRealLock(monitorHandle) )


    #define ILUJAVA_MON_WAIT(monitorHandle, timeout) \
        monitorWait( TheRealLock(monitorHandle), (int) timeout )

    /*
     * Threading primitives.
     * Sys threads and java threads may or may not be the same thing.
     * Very few primitives; most thread operations are done by
     * calling back into java.
     */

    /* public: We need to check thread identitries */
    // xxx dll temp try out extern struct 
    // Hjava_lang_Thread 
    // *java_lang_Thread_currentThread(struct Hjava_lang_Thread *);
    extern long rni_current_thread();
    #define ILUJAVA_SYSTHREAD_sysThreadSelf() \
	rni_current_thread()
        //(ILUJAVA_SYSTHREAD_stPtr) java_lang_Thread_currentThread(0)

    /* semi-public: We need this for debugging */
    #define ILUJAVA_SYSTHREAD_javaThreadFromSysThread(sysThread) \
        (void *) (sysThread)

#endif /*useWin32WithRNI*/



#if defined(useONIWithRealJavaMonitors)

    #define IluJava_JMon_WITH_ONI_AND_REALJAVAMONITORS 1
  
    typedef struct sys_thread java_sys_thread_t;
    typedef java_sys_thread_t* ILUJAVA_SYSTHREAD_stPtr;

    /* Portable apps don't use ilujava_mon_allocate directly;  
     * use the ILUJAVA_MON_ALLOC macro instead 
     */
    extern ILUJAVA_JMON_PTR ilujava_mon_allocate();

    /* Portable apps don't use ilujava_mon_destroyDeallocate directly; 
     * use the ILUJAVA_MON_FREE macro instead 
     */
    extern void ilujava_mon_destroyDeallocate(ILUJAVA_JMON_PTR);
    
    #define TheRealLock(monitorHandle) \
        obj_monitor( (HObject*) \
            JGC_GLOBALOBJ_TOJ(  \
                ((ilujava_MonitorRef) monitorHandle)->theRealLock \
            ) \
        )

    #define ILUJAVA_MON_ENTER(monitorHandle) \
        monitorEnter( TheRealLock( monitorHandle ) )

    #define ILUJAVA_MON_EXIT(monitorHandle) \
        monitorExit( TheRealLock(monitorHandle) )

    #define ILUJAVA_MON_NOTIFY(monitorHandle) \
        monitorNotify( TheRealLock(monitorHandle) )
        
    #define ILUJAVA_MON_NOTIFYALL(monitorHandle) \
        monitorNotifyAll( TheRealLock(monitorHandle) )

    #define ILUJAVA_MON_WAIT(monitorHandle, timeout) \
        monitorWait( TheRealLock(monitorHandle), (int) timeout )

    /*
     * Threading primitives.
     * Sys threads and java threads may or may not be the same thing.
     * Very few primitives; most thread operations are done by
     * calling back into java.
     */

    #include <threads.h>
  
    /*private; can't define because wrong on half the systems*/
    /* int threadSelf(void); */
    /* sys_thread_t * threadSelf(void);*/ 	/* from sys_api.h */
    /* TID threadSelf(void); */ 		/* from threads.h */
  
    /* public: We need to check thread identitries */
    #define ILUJAVA_SYSTHREAD_sysThreadSelf() \
        (ILUJAVA_SYSTHREAD_stPtr) threadSelf()
    
    /* semi-public: We need this for debugging */
    #define ILUJAVA_SYSTHREAD_javaThreadFromSysThread(sysThread) \
        (void *) (sysThread)

#endif  /* useONIWithRealJavaMonitors */


#if defined(useJNIMonitors)
    #define IluJava_JMon_WITH_JNI 1
    #include <jni.h>
    
    /* Outside this header file and its implementation
     * don't use ilujava_mon_allocate directly;  
     * but use the ILUJAVA_MON_ALLOC macro instead
     */
    extern ILUJAVA_JMON_PTR ilujava_mon_allocate(JENV_FORMAL_NOCOMMA);

    /* Outside this header file and its implementation
     * don't use ilujava_mon_destroyDeallocate directly; 
     * but use the ILUJAVA_MON_FREE macro instead
     */
    extern void ilujava_mon_destroyDeallocate(JENV_FORMAL ILUJAVA_JMON_PTR);

    /* Private to this header file and LockTech */
    typedef JNIEnv* ILUJAVA_SYSTHREAD_stPtr;

    /* Private to this header file */
    #define TheRealLock(monitorHandle) \
        (jobject) ((ilujava_MonitorRef) monitorHandle)->theRealLock
    
    /* Public */
    #define ILUJAVA_MON_ENTER(monitorHandle) \
        (*j_env)->MonitorEnter(j_env, TheRealLock(monitorHandle))

    /* Public */
    #define ILUJAVA_MON_EXIT(monitorHandle) \
        (*j_env)->MonitorExit(j_env, TheRealLock(monitorHandle))
    
    /* nobody uses it 
     *
     * #define ILUJAVA_MON_NOTIFY ....
     */
    
    /* Semi Public; requires definition of METHODID_NOTIFYALL */
    #define ILUJAVA_MON_NOTIFYALL(monitorHandle) \
        (*j_env)->CallVoidMethod(j_env, TheRealLock(monitorHandle), METHODID_NOTIFYALL)
    
    /* Semi Public; requires definition of METHODID_WAIT */
    #define ILUJAVA_MON_WAIT(monitorHandle, timeout) \
        (*j_env)->CallVoidMethod(j_env, TheRealLock(monitorHandle), METHODID_WAIT, (jlong) timeout)
    
    /* public: We need to check thread identitries */
    #define ILUJAVA_SYSTHREAD_sysThreadSelf() \
        (ILUJAVA_SYSTHREAD_stPtr) j_env

#endif  /* useJNIMonitors */


#endif  /* _ILUJAVA_JMON_H_ */
