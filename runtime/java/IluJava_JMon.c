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
/* Chris Jacobi, November 25, 1998 5:45 pm PST */
/* $Id: IluJava_JMon.c,v 1.11 1999/08/03 01:54:15 janssen Exp $ */


#include "IluJava_JMon.h"
#include "IluJava_JTypes.h"
#include "IluJava_Common.h"


    /* handles some debug output in a special way because
     * ilu_DebugPrintf actually enters a lock and causes recursion 
     * if invoked from LockTech
     */

#define DEBUGGINGTHIS 0
#define DEBUGPRINT(x) if (DEBUGGINGTHIS) { \
    ilujava_SpecialDebugPrintf("$ IluJava_JMon " x "\n"); \
    }

extern void ilujava_SpecialDebugPrintf(char *formatSpec, ...)
{
  /* vfprintf is ANSI C, section 4.9.6.7 */
#if (! defined(WIN32))
  va_list ap;
  va_start(ap, formatSpec);
  (void) vfprintf (stderr, formatSpec, ap);
  va_end(ap);
#endif
} /*ilujava_SpecialDebugPrintf*/


#ifdef  IluJava_JMon_WITH_ONI_AND_SYSMONITORS

    #define IluJava_JMon_defineThisOnlyOnce 1
    #define IluJava_JMon_NeedGenericMonLists 1

    /*used by IluJava_JMon_NeedGenericMonLists*/
    extern void ilujava_JMon_allocTheLock(JENV_FORMAL ilujava_MonitorRef container)
    {
        container->theRealLock = ilu_malloc(sysMonitorSizeof());
        memset(container->theRealLock, 0, sysMonitorSizeof());
        #if (ILUJAVA_H_MINORVERSION < 1)
            sysMonitorInit((sys_mon_t *) (container->theRealLock), ilu_FALSE);
        #else
            sysMonitorInit((sys_mon_t *) (container->theRealLock));
        #endif
    } /*ilujava_JMon_allocTheLock */

#endif /* IluJava_JMon_WITH_ONI_AND_SYSMONITORS */




#ifdef IluJava_JMon_WITH_RNI

    #define IluJava_JMon_defineThisOnlyOnce 1

    ILUJAVA_JMON_PTR ilujava_mon_allocate() 
    {
        /* Create an java Object, and get a strong reference on it */
        HObject* p_newObject;
        HObject** pp_newObject;
        DEBUGPRINT("RNI allocate");
        p_newObject = execute_java_constructor(
            EE(), "java/lang/Object", NULL, "()"
            );
        pp_newObject = GCNewHandle(p_newObject);
        return (ILUJAVA_JMON_PTR) pp_newObject;
    } /*ilujava_mon_allocate*/


    void ilujava_mon_destroyDeallocate(ILUJAVA_JMON_PTR m)
    {
        /* remove the strong reference to the java object */
        HObject** pp_obj = (HObject**) m;
        DEBUGPRINT("RNI destroyDeallocate");
        GCFreeHandle(pp_obj);
    }

#endif /* IluJava_JMon_WITH_RNI */



#ifdef IluJava_JMon_WITH_ONI_AND_FAKEJAVAMONITORS

    #define IluJava_JMon_defineThisOnlyOnce 1
    #define IluJava_JMon_NeedGenericMonLists 1
    
    #define _ILUJAVA_MONSZ 8

    /*used by IluJava_JMon_NeedGenericMonLists*/
    extern void ilujava_JMon_allocTheLock(JENV_FORMAL ilujava_MonitorRef container)
    {
        container->theRealLock = ilu_malloc(_ILUJAVA_MONSZ);
        memset(container->theRealLock, 0, _ILUJAVA_MONSZ);
    } /*ilujava_JMon_allocTheLock */

#endif /* IluJava_JMon_WITH_ONI_AND_FAKEJAVAMONITORS */



#if (defined(IluJava_JMon_WITH_ONI_AND_REALJAVAMONITORS) || defined(IluJava_JMon_WITH_JNI))

    /* using ONI with java monitors (e.g on Win32) */
    /* using JNI monitors */

    #define IluJava_JMon_defineThisOnlyOnce 1
    #define IluJava_JMon_NeedGenericMonLists 1

    /*used by IluJava_JMon_NeedGenericMonLists*/
    extern void ilujava_JMon_allocTheLock(JENV_FORMAL ilujava_MonitorRef container)
    {
        JGC_GLOBALOBJ_DECL(glockholder);
        JObject jh_lock;
        jh_lock = _ilujava_createPermanentJavaObject();
        JGC_GLOBALOBJ_ASSIGNTO(glockholder, jh_lock)
        container->theRealLock = (char*) glockholder;
    } /*ilujava_JMon_allocTheLock */
    
#endif 



#ifdef IluJava_JMon_NeedGenericMonLists
    
    /* NEEDS
     * void ilujava_JMon_allocTheLock(ilujava_MonitorRef container);
     * to abstract over different implementations 
     */
    
    static ILUJAVA_JMON_PTR metaLock = 0;
    static ilujava_MonitorRef freeList = 0; /*protect with metaLock */
    
    #define ILUJAVA_META_ENTER()\
        ILUJAVA_MON_ENTER(metaLock)

    #define ILUJAVA_META_EXIT()\
        ILUJAVA_MON_EXIT(metaLock)
        
    ILUJAVA_JMON_PTR ilujava_mon_allocate(JENV_FORMAL_NOCOMMA) 
    {
        ilujava_MonitorRef cm = 0;
        DEBUGPRINT("JMON_LIST allocate 1");
        if (freeList) {
            DEBUGPRINT("JMON_LIST freeList exists");
            /* if a free list exist, a meta must previously 
             * have been allocated.
             */
            ILUJAVA_META_ENTER();
            cm = freeList;
            if (cm) {
                freeList = cm->next;
                cm->next = 0;
            }
            ILUJAVA_META_EXIT();
            if (cm) {
                DEBUGPRINT("JMON_LIST allocated from free list");
                return (ILUJAVA_JMON_PTR) cm;
            }
        }
        DEBUGPRINT("JMON_LIST allocate 3");
        cm = (ilujava_MonitorRef) ilu_malloc(sizeof(ilujava_MonitorRecS));
        cm->next = 0;
        ilujava_JMon_allocTheLock(JENV_ACTUAL cm);
        DEBUGPRINT("JMON_LIST allocate 4");
        if (metaLock == 0) {
            /* No meta lock yet.
             * Race condition if first calls to this are concurrent.
             * No problem because ilu doesn't fork before some monitors 
             * have been created. 
             */
            DEBUGPRINT("JMON_LIST recursively alloc metalock");
            metaLock = (ILUJAVA_JMON_PTR) cm;
            return ilujava_mon_allocate(JENV_ACTUAL_NOCOMMA);
        }
        return (ILUJAVA_JMON_PTR) cm;
    } /*ilujava_mon_allocate*/

    void ilujava_mon_destroyDeallocate(JENV_FORMAL ILUJAVA_JMON_PTR xm)
    {
        /* 
         * Basicly because I don't reliably understand how to destroy monitors
         * (do they need to be locked or not?) we simply put them onto a 
         * free list and re-use them.
         *
         * Rely on ilu that it never destroys a monitor while in use
         */
        ilujava_MonitorRef cm = (ilujava_MonitorRef) xm;
        DEBUGPRINT("JMON_LIST destroyDeallocate");
        ILUJAVA_META_ENTER();
        cm->next = freeList;
        freeList = cm;
        ILUJAVA_META_EXIT();
    } /*ilujava_mon_destroyDeallocate*/

#endif /* IluJava_JMon_NeedGenericMonLists */

/* end */


