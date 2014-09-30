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
/* IluJava_IluAlarmTech.c */
/* Chris Jacobi, June 12, 1998 3:47 pm PDT */

/*
 */
 
/* 
 * $Id: IluJava_IluAlarmTech.c,v 1.27 1999/09/10 16:49:14 larner Exp $ 
 *
 *
 * IluAlarmTech and IluForkTech implementation for Ilu-Java.
 * Corresponding Java side in xerox.ilu.IluAlarmTech.java and
 * xerox.ilu.IluForkTech.java
 *
 * Alarms are in a hash table on the java side so that
 * we can use weak pointers here.  
 */
 
#include "IluJava_Includes.h"
#include "IluJava_Common.h"
#include "IluJava_JGC.h"
#include "xerox_ilu_IluAlarmTech.h"
#include "xerox_ilu_IluForkTech.h"

typedef void (*alarmProc)(ilu_private rock);

struct pair_s {
  ilu_integer f1;	
  ilu_integer f2;	
};
typedef struct pair_s pair;


/* Originally I used a dynamic_method simply because I couldn't get
 * static methods to work correctly.  In the mean time I'm glad of
 * using a smaller number of concepts.
 */ 

static JGC_GLOBALOBJ_DECL(jsp_iluAlarmTechProto);
static JGC_GLOBALOBJ_DECL(jsp_iluForkTechProto);

JAVAEXPORT(IluAlarmTech_giveInstance, void) 
    JIluAlarmTech jh_self /*must not be garbage collected*/
    ENDJAVAEXPORT
{
    JGC_GLOBALOBJ_ASSIGNTO(jsp_iluAlarmTechProto, jh_self);
}


JAVAEXPORT(IluForkTech_registerForkTechProto, void) 
    JIluForkTech jh_self /*must not be garbage collected*/
    ENDJAVAEXPORT
{
    JGC_GLOBALOBJ_ASSIGNTO(jsp_iluForkTechProto, jh_self);
}



/* GC: Calls into java */
/* returns a java weak pointer (jwp_...) which will not be collected */
INTERNALONLY ilu_refany /* actually a  */ 
_ilujava_CreateAlarm(void)
/* provided to the ilu kernel */
{
    JENV_DECLARE_INITFROMTHINAIR
    JIluAlarmTech jh_proto;
    ilu_refany jwp_x = 0;
    if (jsp_iluAlarmTechProto==0) {
        _ilujava_PANIC("$ IluAlarmTech CreateAlarm: BAD INSTANCE");
    }    
    JTRACE(_ilujava_alarmFlag>0, ("$ IluAlarmTech CreateAlarm\n"));
    JGC_GLOBALOBJ_GETFROM(jh_proto, jsp_iluAlarmTechProto);
    /* use weak pointer; alarms are stored in hash table */  
    jwp_x = (ilu_refany) JGC_WP_MAKE(JCALL_IluAlarmTech_createAlarm(jh_proto));
    if (JPENDINGEXCEPTION()) {
        _ilujava_PANIC("$ IluAlarmTech CreateAlarm: JAVA EXCEPTION\n");
    }
    return jwp_x;
}


JAVAEXPORT(IluAlarmTech_wakeIlu, void) 
    JIluAlarmTech unused, 
    Jint x1, Jint x2, Jint x3, Jint x4
    ENDJAVAEXPORT
/* Called from Java to invoke the alarm */
{
    union tag_proc64 {
        pair p;
        void (*proc)(ilu_private rock);
    } proc64;
    union tag_rock64 {
        pair p;
        ilu_private rock;
    } rock64;
    proc64.p.f1 = (ilu_integer) x1;
    proc64.p.f2 = (ilu_integer) x2;
    rock64.p.f1 = (ilu_integer) x3;
    rock64.p.f2 = (ilu_integer) x4;
    JTRACE(_ilujava_alarmFlag>1, ("$ IluAlarmTech wakeIlu\n"));
    proc64.proc(rock64.rock);
}


/* Calls into java */
INTERNALONLY void 
_ilujava_SetAlarm(ilu_refany jwp_alarm,
	ilu_FineTime t,
	void (*proc)(ilu_private rock),
	ilu_private rock
	)
/* Procedure provided to the ilu kernel */
{
    JENV_DECLARE_INITFROMTHINAIR
    ilu_cardinal milli;
    union tag0_proc64 {
        pair p;
        void (*proc)(ilu_private rock);
    } proc64 = {0};
    union tag0_rock64 {
        pair p;
        ilu_private rock;
    } rock64 = {0};	
    /* convert ilu-time to duration in milliseconds */
    ilu_FineTime now = ilu_FineTime_Now();
    ilu_FineTime length;
    length = ilu_FineTime_Sub(t, now);
    /* simplistic way to handle negative times */
    if (ilu_FineTime_Cmp(now, t)>=0) {
        length.ft_s = 0; milli = 0;
    } else {
        milli = ilu_rescale(t.ft_t, ilu_FineTimeRate, 1000);
    }
    /* convert arguments and call into java */
    proc64.proc = proc;
    rock64.rock = rock;
    JTRACE(_ilujava_alarmFlag>1,
        ("$ IluAlarmTech SetAlarm %d %d\n", (int) length.ft_s, milli));
    JCALL_IluAlarmTech_setAlarm(
        JGC_WP_REVEAL( (JGC_WP_TYPE(JIluAlarmTech)) jwp_alarm),
        (Jint) length.ft_s, (Jint) milli,
        (Jint) proc64.p.f1, (Jint) proc64.p.f2, 
        (Jint) rock64.p.f1, (Jint) rock64.p.f2
        );
}


/* Calls into java */
INTERNALONLY void 
_ilujava_UnsetAlarm(ilu_refany jwp_alarm)
/* Procedure provided to the ilu kernel */
{
    JENV_DECLARE_INITFROMTHINAIR
    JObject jh_alarm;
    JTRACE(_ilujava_alarmFlag>1, ("$ IluAlarmTech UnsetAlarm\n"));
    jh_alarm = JGC_WP_REVEAL( (JGC_WP_TYPE(JObject)) jwp_alarm);
    JCALL_IluAlarmTech_unsetAlarm(jh_alarm);
}

/* Calls into java */
INTERNALONLY void 
_ilujava_DestroyAlarm(ilu_refany jwp_alarm)
/* Procedure provided to the ilu kernel */
{
    JENV_DECLARE_INITFROMTHINAIR
    JGC_WP_TYPE(JIluAlarmTech) jwp = (JGC_WP_TYPE(JIluAlarmTech)) jwp_alarm;
    JIluAlarmTech jh_alarm;
    JTRACE(_ilujava_alarmFlag>1, ("$ IluAlarmTech provided\n"));
    jh_alarm = (JIluAlarmTech) JGC_WP_REVEAL(jwp);
    JCALL_IluAlarmTech_destroyAlarm(jh_alarm);
    jh_alarm = (JIluAlarmTech) JGC_WP_REVEAL(jwp);
    JGC_WP_RELEASE(jh_alarm);
}


JAVAEXPORT(IluForkTech_callForkee, void) 
    JIluForkTech unused, 
    Jint x1, Jint x2, Jint x3, Jint x4
    ENDJAVAEXPORT
/* Called from Java to invoke the actual forked proc */
{
    union tag2_proc64 {
        pair p;
        void (*proc)(void *arg);
    } proc64;
    union tag2_arg64 {
        pair p;
        void* arg;
    } arg64;
    proc64.p.f1 = (ilu_integer) x1;
    proc64.p.f2 = (ilu_integer) x2;
    arg64.p.f1 = (ilu_integer) x3;
    arg64.p.f2 = (ilu_integer) x4;
    proc64.proc(arg64.arg);
}


extern JObject _ilujava_createPermanentJavaObject()
/* Used for locking purposes */
{
    JENV_DECLARE_INITFROMTHINAIR
    JObject jh_ft_proto;
    JObject jh_ob;
    JGC_GLOBALOBJ_GETFROM(jh_ft_proto, jsp_iluForkTechProto);
    jh_ob = (JObject) JCALL_IluForkTech_givePermanentObject(jh_ft_proto);
    return jh_ob;
}


static ilu_boolean
    MyForkerProc (
        void (*proc)(void *arg), 
        void *arg,
        ILU_ERRS((no_memory, no_resources, internal)) *errs
        )
/* Calls into java to fork, to get a real java thread */
{
    JENV_DECLARE_INITFROMTHINAIR
    JObject jh_proto;
    union tag1_proc64 {
        pair p;
        void (*proc)(void *arg);
    } proc64 = {0};
    union tag1_arg64 {
        pair p;
        void* arg;
    } arg64 = {0};
    proc64.proc = proc;
    arg64.arg = arg;
    JGC_GLOBALOBJ_GETFROM(jh_proto, jsp_iluForkTechProto);
    JCALL_IluForkTech_forkRequest(jh_proto, 
        (Jint) proc64.p.f1, (Jint) proc64.p.f2, 
        (Jint) arg64.p.f1, (Jint) arg64.p.f2
        );
    ILU_CLER(*errs);
    return ilu_TRUE;
}


static ilu_MainLoop alarmData;

EXPORTLIMITED void 
_ilujava_AlarmTechInit()
/* call this to initialize this file */
{
    ILU_ERRS((no_memory, no_resources, internal)) errs;
    alarmData.ml_run         	= 0;
    alarmData.ml_exit         	= 0;
    alarmData.ml_register_input	= 0;
    alarmData.ml_unregister_input	= 0;
    alarmData.ml_register_output	= 0;
    alarmData.ml_unregister_output	= 0;
    alarmData.ml_create_alarm	= _ilujava_CreateAlarm;
    alarmData.ml_set_alarm    	= _ilujava_SetAlarm;
    alarmData.ml_unset_alarm	= _ilujava_UnsetAlarm;
    alarmData.ml_destroy_alarm	= _ilujava_DestroyAlarm;
    ilu_SetMainLoop(&alarmData);
    ilu_SetForkTech(&MyForkerProc, &errs);
    ILU_MUST_BE_SUCCESS(errs);
}


/* end */
