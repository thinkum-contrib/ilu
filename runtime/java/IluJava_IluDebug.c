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
/* IluJava_IluDebug.c */
/* Chris Jacobi, November 20, 1998 9:45 pm PST */

/*
 */
 
/* $Id: IluJava_IluDebug.c,v 1.42 1999/08/12 06:07:54 janssen Exp $ */
 
/*
 * Native impl for IluDebug.java, IluDebugWriter.java, IluDebugHooks.java
 * including a few native utilities...
 */


#include "IluJava_Includes.h"
#include "IluJava_Common.h"

#include "xerox_ilu_IluDebugHooks.h"
#include "xerox_ilu_IluDebug.h"
#include "xerox_ilu_IluDebugWriter.h"
#include "xerox_ilu_IluRT0.h"
#include "xerox_ilu_IluOInt.h"
#include "xerox_ilu_IluClassRep.h"
#include "xerox_ilu_IluServer.h"

#include <stdlib.h>
#include "IluJava_JTypes.h"
#include "IluJava_JArrays.h"
#include "IluJava_JGC.h"
#include "IluJava_JOps.h"


EXPORTLIMITEDVAR ilu_LanguageIndex _ilujava_langidx = 0;
static ilu_boolean _ilujava_langRegistered = ilu_FALSE;


static ilu_LanguageIndex _ilujava_LangIdx(void)
{
    if (! _ilujava_langRegistered) {
        _ilujava_langidx = ilu_RegisterLanguage("Java");
        _ilujava_langRegistered = ilu_TRUE;
    }
    return _ilujava_langidx;
}


EXPORTLIMITED void
_ilujava_DebugPrint(char * f)
{
    ilu_DebugPrintf("$** Ilu_Java BUG: %s \n", f);
    if (_ilujava_iluGenericFlag > 1) {
        _ilujava_PANIC(f);
    }
}


EXPORTLIMITED void
_ilujava_DebugPrintAndSignal(char * f)
{
    JENV_DECLARE_INITFROMTHINAIR
    _ilujava_DebugPrint(f);
    _ilujava_throwException(JENV_ACTUAL 10, "org/omg/CORBA/INTERNAL", "**_ilujava_bug");
}


EXPORTLIMITED void
_ilujava_PANIC(char * f)
{
    ilu_DebugPrintf("$** Ilu_Java PANIC: %s \n", f);
    _ilu_Assert(0, f);
}


/* immutable instances from set up */
static JGC_WP_TYPE(JIluDebug) _ilujava_jjIluDebugTheInst = 0;
static JGC_WP_TYPE(JIluDebugHooks) _ilujava_jjIluDebugHooksTheInst = 0;

/* dynamicly registered instances */
static JGC_WP_TYPE(JIluDebugHooks) _ilujava_jjAssertionFailure = 0;
static JGC_WP_TYPE(JIluDebugHooks) _ilujava_jjCheckFailure     = 0;
static JGC_WP_TYPE(JIluDebugHooks) _ilujava_jjDebugHook        = 0;  


/* Calls into Java */
INTERNALONLY void 
_ilujava_RaiseDebugHook(ilu_ErrorType et, const char *file, int line)
/* Register this with Ilu;
 * It is ok here to do no action if we are not debugging
 */
{
    JIluDebugHooks jjdh = (JIluDebugHooks) JGC_WP_REVEAL(_ilujava_jjDebugHook);
    if (jjdh) {
        JENV_DECLARE_INITFROMTHINAIR
        JString js = 0;
        long li = line;
        long let = (long) et;
        if (file) {
            js = IluJava_JString_fromA0(JENV_ACTUAL (char *) file);
        }
        JCALL_IluDebugHooks_reportDebugHook(jjdh, js, li, let);
    }
}


/* Calls into Java */
INTERNALONLY void 
_ilujava_AssertionFailConsumer(const char *file, int line)
/* Register this with Ilu;
 * Always do something
 */
{
    JIluDebugHooks jjaf = 
        (JIluDebugHooks) JGC_WP_REVEAL(_ilujava_jjAssertionFailure);
    if (jjaf) {
        JENV_DECLARE_INITFROMTHINAIR
        JString js = 0;
        long li = line;
        if (file) {
            js = IluJava_JString_fromA0(JENV_ACTUAL (char *) file);
        }
        JCALL_IluDebugHooks_reportAssertionFailure(jjaf, js, li);
    } else {
        _ilujava_DebugPrintAndSignal("ASSERTION FAILURE");
    }
}


/* Calls into Java */
INTERNALONLY void 
_ilujava_CheckFailureConsumer(const char *file, int line)
/* Register this with Ilu;
 * Always do something
 */
{
    JIluDebugHooks jjdh = 
        (JIluDebugHooks) JGC_WP_REVEAL(_ilujava_jjCheckFailure);
    if (jjdh) {
        JENV_DECLARE_INITFROMTHINAIR
        JString js = 0;
        long li = line;
        if (file) {
            js = IluJava_JString_fromA0(JENV_ACTUAL (char *) file);
        }
        JCALL_IluDebugHooks_reportCheckFailure(jjdh, js, li);
    } else {
        _ilujava_DebugPrintAndSignal("CONSISTENCY CHECK");
    }
}


JAVAEXPORT(IluDebug_registerTheInst, void) 
    JIluDebug jjd
    ENDJAVAEXPORT
/* Called once at initialization */
{
    if (_ilujava_jjIluDebugTheInst) {
        ilu_DebugPrintf("***BAD IluDebug initialization");
        return;
    }
    _ilujava_jjIluDebugTheInst = JGC_WP_MAKE((JGC_WP_TYPE(JIluDebug))jjd);
    _ilujava_langidx = _ilujava_LangIdx();
}


JAVAEXPORT(IluDebugHooks_registerTheInst, void) 
    JIluDebugHooks jjdh
    ENDJAVAEXPORT
/* Called once at initialization */
{
   if (_ilujava_jjIluDebugHooksTheInst) {
        ilu_DebugPrintf("***BAD IluDebugHooks initialization");
        return;
   }
   _ilujava_jjIluDebugHooksTheInst = 
       JGC_WP_MAKE((JGC_WP_TYPE(JIluDebugHooks))jjdh);
    /* set a few defaults */
    if (_ilujava_jjAssertionFailure==0) {
        _ilujava_jjAssertionFailure = 
            JGC_WP_MAKE((JGC_WP_TYPE(JIluDebugHooks))jjdh);
    }
    if (_ilujava_jjCheckFailure==0) {     
        _ilujava_jjCheckFailure = 
            JGC_WP_MAKE((JGC_WP_TYPE(JIluDebugHooks))jjdh);
    }
    /* but don't automatically set jjDebugHook */
    _ilujava_langidx = _ilujava_LangIdx();
}


JAVAEXPORT(IluDebugHooks_setRaiseDebugHook, void) 
    JIluDebugHooks unused, 
    JIluDebugHooks jdbh
    ENDJAVAEXPORT
/* Java client registers an IluDebugHooks instance (or subclass).
 * For ILU debugging; not for production use...
 */
{
    if (jdbh) {
         /* if _ilujava_jjDebugHook is already assigned, so be it. We 
          * overwrite the global ref, and therefore leak memory.  Its 
          * a debugging aid only and not used in real production.
          */
         _ilujava_jjDebugHook = JGC_WP_MAKE((JGC_WP_TYPE(JIluDebugHooks))jdbh);
         ilu_SetRaiseDebugHook(&_ilujava_RaiseDebugHook);
    } else {
         ilu_SetRaiseDebugHook(0);
    }
} /*setRaiseDebugHook*/


JAVAEXPORT(IluDebugHooks_defineAssertionFailure, void)
    JIluDebugHooks unused, 
    JIluDebugHooks jjaf
    ENDJAVAEXPORT
/* Java client registers an IluDebugHooks instance (or subclass).
 * It makes no sense to do nothing.
 */
{
    if (jjaf==0) {
        jjaf = (JIluDebugHooks) JGC_WP_REVEAL(_ilujava_jjIluDebugHooksTheInst);
    }
    _ilujava_jjAssertionFailure = 
        JGC_WP_MAKE((JGC_WP_TYPE(JIluDebugHooks))jjaf);
    ilu_SetAssertionFailConsumer(&_ilujava_AssertionFailConsumer);
}
        

JAVAEXPORT(IluDebugHooks_defineCheckFailure, void) 
    JIluDebugHooks unused, 
    JIluDebugHooks jjcf
    ENDJAVAEXPORT
/* Java client registers an IluDebugHooks instance (or subclass).
 * It makes no sense to do nothing.
 */
{
    if (jjcf==0) {
        jjcf = (JIluDebugHooks) JGC_WP_REVEAL(_ilujava_jjIluDebugHooksTheInst);
    }
    /* Possible leak: as we don't release previous global ref.
     * Since this is for debugging only, safety is more important then space.
     */
    _ilujava_jjCheckFailure = JGC_WP_MAKE((JGC_WP_TYPE(JIluDebugHooks))jjcf);
    ilu_SetCheckFailureConsumer(&_ilujava_CheckFailureConsumer);
}


EXPORTLIMITEDVAR int _ilujava_iluGenericFlag   = 99;
EXPORTLIMITEDVAR int _ilujava_iluSpare1Flag    = 99;
EXPORTLIMITEDVAR int _ilujava_iluSpare2Flag    = 99;
EXPORTLIMITEDVAR int _ilujava_objectsFlag      = 99;
EXPORTLIMITEDVAR int _ilujava_pickleFlag       = 99;
EXPORTLIMITEDVAR int _ilujava_callsFlag        = 99;
EXPORTLIMITEDVAR int _ilujava_definitionsFlag  = 99;
EXPORTLIMITEDVAR int _ilujava_pctFlag          = 99;
EXPORTLIMITEDVAR int _ilujava_traceFlag        = 99;
EXPORTLIMITEDVAR int _ilujava_gcFlag           = 99;
EXPORTLIMITEDVAR int _ilujava_lockFlag         =  0;
EXPORTLIMITEDVAR int _ilujava_alarmFlag        =  0;


JAVAEXPORT(IluDebug_ngetenv, JString)
    JIluDebug unused, 
    JString jh_Key
    ENDJAVAEXPORT
{
    JString jh_val = 0;
    char* cVal;
    char* cKey;
    if (jh_Key == 0) {return 0;}
    cKey = IluJava_JString_toheap80(JENV_ACTUAL jh_Key);
    cVal = getenv(cKey);
    ilu_free(cKey);
    if (cVal) {
        jh_val = IluJava_JString_fromA0(JENV_ACTUAL cVal);
        /* don't free cVal; it is shared */
    }
    return jh_val;
}


JAVAEXPORT(IluDebug_nputenv, Jint)
    JIluDebug unused, 
    JString jh_Key,
    JString jh_Val
    ENDJAVAEXPORT
{
    JString jh_val = 0;
    char* cVal;
    char* cKey;
    char* cBuf;
    if (jh_Key == 0) {return 0;}
#ifdef HAVE_PUTENV
    cKey = IluJava_JString_toheap80(JENV_ACTUAL jh_Key);
    cVal = IluJava_JString_toheap80(JENV_ACTUAL jh_Val);
    cBuf = ilu_malloc(strlen(cKey) + strlen(cVal) + 2);
    sprintf(cBuf, "%s=%s", cKey, cVal);
    ilu_free(cKey);
    ilu_free(cVal);
    if (putenv (cBuf) == 0)
      return 1;
    else
      return 0;
    /* don't free cBuf; it is now part of the environment */
#else
    return 0;
#endif
}


JAVAEXPORT(IluDebug_reportFlags, void)
    JIluDebug unused, 
    Jint iluGenericFlag,
    Jint iluSpare1Flag,
    Jint iluSpare2Flag,
    Jint objectsFlag,
    Jint pickleFlag,
    Jint callsFlag,
    Jint definitionsFlag,
    Jint pctFlag,
    Jint gcFlag,
    Jint lockFlag,
    Jint alarmFlag
    ENDJAVAEXPORT
{
    _ilujava_iluGenericFlag     = iluGenericFlag;
    _ilujava_iluSpare1Flag      = iluSpare1Flag;
    _ilujava_iluSpare2Flag      = iluSpare2Flag;
    _ilujava_objectsFlag        = objectsFlag;
    _ilujava_pickleFlag         = pickleFlag;
    _ilujava_callsFlag          = callsFlag;
    _ilujava_definitionsFlag    = definitionsFlag;
    _ilujava_pctFlag            = pctFlag;
    _ilujava_gcFlag             = gcFlag;
    _ilujava_lockFlag           = lockFlag;
    _ilujava_alarmFlag          = alarmFlag;
}


JAVAEXPORT(IluDebug_nSetDebugLevel, void)
    JIluDebug unused,
    JString js
    ENDJAVAEXPORT
{
    char* cs;
    if (js) {
        cs = IluJava_JString_toheap80(JENV_ACTUAL js);
        ilu_SetDebugLevelViaString(cs);
        ilu_free(cs);
    }
}


JAVAEXPORT(IluDebugWriter_debugWrite1, void)
    JIluDebugWriter unused, 
    Jint b
    ENDJAVAEXPORT
{
    ilu_DebugPrintf("%c", b);
}


EXPORTLIMITED char * 
_ilujava_captureJavaStack()
{
    JENV_DECLARE_INITFROMTHINAIR
    JObject jh_dbInst;
    char* pc_stacktrace = ILU_NIL;
    jh_dbInst = (JObject)JGC_WP_REVEAL(_ilujava_jjIluDebugTheInst);
    if (jh_dbInst) {
        JString js;
        js = (JString) JCALL_IluDebug_dynamicCaptureStackTrace(jh_dbInst);
        if (js) {
            pc_stacktrace = IluJava_JString_toheap80(JENV_ACTUAL js);
        }
    } 
    return pc_stacktrace;
}


/* Calls into Java */
EXPORTLIMITED int 
_ilujava_getIntProp(char * cKey, int deflt)
/* Read a java property value */
{
    JENV_DECLARE_INITFROMTHINAIR
    int val = deflt;
    if (cKey) {
        long ldeflt = deflt;
        JString jh_key = IluJava_JString_fromA0(JENV_ACTUAL cKey);
        JIluDebug jh_dbInst;
        jh_dbInst = (JIluDebug) JGC_WP_REVEAL(_ilujava_jjIluDebugTheInst);
        if (jh_dbInst) {
            val = (int) JCALL_IluDebug_dynamicGetIntProp(jh_dbInst, jh_key, ldeflt);
        }
    } 
    return val;
}


/* end */

