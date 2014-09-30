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
/* IluJava_IluInit.c */
/* Chris Jacobi, December 10, 1998 3:36 pm PST */

/* 
 * $Id: IluJava_IluInit.c,v 1.40 1999/08/03 01:54:23 janssen Exp $ 
 *
 *
 * See IluInit.java, IluInit2.java
 */


#include "IluJava_Includes.h"
#include "IluJava_Common.h"
#include "xerox_ilu_IluInit2.h"

#ifdef JNI

#include <jni.h>

static JavaVM*  myJavaVM = 0;

#if (defined(ILUJAVA_H_JDK12POSTBETA))

extern JNIEnv * _ilujava_ComputeTheJNIEnv()
{
    JNIEnv * j_env;
    (void) (*myJavaVM)->GetEnv(myJavaVM, (void **)&j_env, 0x00010002);
    return j_env;
}

#else 

extern JNIEnv * _ilujava_ComputeTheJNIEnv()
{
    JNIEnv * j_env;
    (void) (*myJavaVM)->AttachCurrentThread(myJavaVM, (void **)&j_env, &attachArgs);
    return j_env;
}

#endif


static void initComputeTheJNIEnv(JNIEnv * j_env) 
{
    (*j_env)->GetJavaVM(j_env, &myJavaVM);
}


#endif /* ifdef JNI */


/* ---------------------------------------------------------
 * For RNI, we are required to export this function.
 * Perhaps there is a better file for this?
 */
  
#ifdef RNI
 
__declspec(dllexport) DWORD __cdecl RNIGetCompatibleVersion() {
	return RNIVER;
}
 
/* ---------------------------------------------------------
 */

#endif
#define DEBUGGINGTHIS 0
#define DEBUGPRINT(x) if (DEBUGGINGTHIS) \
    ilu_DebugPrintf("$ IluJava_IluInit2 " x "\n");

 
/* ---------------------------------------------------------
 * Version checking
 */

JAVAEXPORT(IluInit2_checkLibrary12x9x98, Jint)
	JIluInit2 unused,
	Jint key
	ENDJAVAEXPORT
{
    int x = 9999;
    /* we go this far; that means the library has been loaded */
    switch (key) {
        case -1:
            /* Some real library initialization */
            #if (defined (JNI))
                #if (ILUJAVA_H_MINORVERSION >= 2) 
                    (*JENV_ACTUAL_NOCOMMA)->
                        EnsureLocalCapacity(JENV_ACTUAL_NOCOMMA, 50);
                #endif
            initComputeTheJNIEnv(JENV_ACTUAL_NOCOMMA);
            #endif
            JinitNativeHeaders(JENV_ACTUAL_NOCOMMA);
            DEBUGPRINT("did JinitNativeHeaders");
            return 0;
        case 0:
            x = 19;
            /* This is a very simple and artificial test:  it is all
             * manual and allows to increase the number when doing
             * semantic changes to the library.
             */
            /* These excessive comments make  */
            /* applying multiple patches */
            /* more easy */
            x = x + 1;
            /* by providing */
            /* multiple lines */
            /* which */
            x = x + 1;
            /* are patchable */
            /* independently */
            /* from each other */
            x = x + 1;
            return x;
        case 1:
            /* Make sure we are using the right java version... */
            return ILUJAVA_H_MAJORVERSION;
        case 2:
            /* Make sure we are using the right java version... */
            return ILUJAVA_H_MINORVERSION;
        case 3:
            /* Check native versus green threads... */
            #ifdef ILUJAVA_H_NATIVE_THREADS
                return 1;
            #else
                return 2;
            #endif
    }
    return x;
}


/* ---------------------------------------------------------
 * Main initialization 
 */

    /* 
     * The current java IO doesn't provide non blocking
     * calls which actually work.  Java does convert the
     * IO into non-blocking, but it blocks the java thread.
     *
     * Registering the real system calls will work simply
     * because we trust that in ilu all read and write are 
     * non blocking 
     */
#if (defined(__sgi) || defined(__SGI) || defined(__FreeBSD__) || defined(linux))

#include <dlfcn.h>

typedef int (*My_ReadProc)(int,void*,int);
typedef int (*My_WriteProc)(int,const void *,int);

static My_ReadProc _ilujava_read; 
static My_WriteProc _ilujava_write; 

static int
_ilujava_recv(int fd, char *buf, int nbytes, int flags)
{
    return _ilujava_read(fd, buf, nbytes);
}

static int
_ilujava_send(int fd, const char *buf, int nbytes, int flags)
{
    return _ilujava_write(fd, buf, nbytes);
}

static void 
fixIO() 
{
    #if (defined(__FreeBSD__))
    void *handle = dlopen("libc.so", 1);
    #else
    void *handle = dlopen("libc.so", RTLD_LAZY);
    #endif
  #if (defined(linux))
    /* as reported for RedHat 4.2 based on libc version 5 */
    _ilujava_read = (My_ReadProc) dlsym(handle, "__libc_read");
    _ilujava_write = (My_WriteProc) dlsym(handle, "__libc_write");
  #else
    _ilujava_read = (My_ReadProc) dlsym(handle, "_read");
    _ilujava_write = (My_WriteProc) dlsym(handle, "_write");
  #endif
    ilu_SetRecvSendProcs(&_ilujava_recv, &_ilujava_send);
}

#else
#ifndef WIN32

extern int _read(int,char*,int);
extern int _write(int,const char*,int);

static int
_ilujava_recv(int fd, char *buf, int nbytes, int flags)
{
    return _read(fd, buf, nbytes);
}

static int
_ilujava_send(int fd, const char *buf, int nbytes, int flags)
{
    return _write(fd, buf, nbytes);
}

static void 
fixIO() 
{
    ilu_SetRecvSendProcs(&_ilujava_recv, &_ilujava_send);
}

#endif
#endif


JAVAEXPORT(IluInit2_nInitLibrary1, void)
	JIluInit2 unused
	ENDJAVAEXPORT
{
    DEBUGPRINT("start IluInit2_nInitLibrary1");
#ifndef WIN32
    fixIO();
    DEBUGPRINT("did fixIO");
#endif
    _ilujava_LockTechInit(); /*ilu wants this early*/
    DEBUGPRINT("did LockTechInit");
    _ilujava_WaitTechInit();
    DEBUGPRINT("did WaitTechInit");
    _ilujava_AlarmTechInit();
    DEBUGPRINT("did AlarmTechInit");
    _ilujava_IluRt0InitHashTable();
    DEBUGPRINT("did IluRt0InitHashTable");
    _ilujava_PassportInit();
    DEBUGPRINT("did PassportInit");
    DEBUGPRINT("IluInit2_nInitLibrary1 done");
}


JAVAEXPORT(IluInit2_nInitLibrary2, void)
	JIluInit2 unused
	ENDJAVAEXPORT
{
    DEBUGPRINT("start IluInit2_nInitLibrary2");
    _ilujava_IluServerInit();
    DEBUGPRINT("did IluServerInit");
    DEBUGPRINT("IluInit2_nInitLibrary2 done");
}


JAVAEXPORT(IluInit2_nInitLibrary3, void)
	JIluInit2 unused
	ENDJAVAEXPORT
{
    DEBUGPRINT("start IluInit2_nInitLibrary3");
    _ilujava_IluOIntInit();
    DEBUGPRINT("did IluOIntInit");
    DEBUGPRINT("IluInit2_nInitLibrary3 done");
}



#ifdef  ILUJAVA_H_thisIsJdk12beta4Neverdefined

#include <jni.h>

jint JNI_OnLoad(JavaVM *vm, void *reserved) {
    return JNI_VERSION_1_2;
} 

void JNI_OnUnload(JavaVM *vm, void *reserved) {
} 

#endif


/* end */
