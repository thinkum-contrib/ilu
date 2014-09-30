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
/* IluJava_Includes.h */
/* Chris Jacobi, December 1, 1998 3:42 pm PST */
/* $Id: IluJava_Includes.h,v 1.28 1999/08/03 01:54:24 janssen Exp $ */

#ifndef _ILUJAVA_INCLUDES_
#define _ILUJAVA_INCLUDES_

#include "./config/ilujava_conf.h"


/****************************************************
 * Get the java native environment dependencies.
 * Include this first into all files. 
 *
 *
 * ONI is my own acronym for Sun's "OLD native interface"
 * JNI Sun's new portable "java native interface"
 * RNI Microsofts "raw native interface"
 * JRI is Netscape's "Java Runtime Interface" 
 ****************************************************/

/****************************************************
 * Native dynamic methods 
 *   (ILU does not overload native methods)
 *   (ILU does not use non ascii characters in names of native methods)
 *
 * @ Native dynamic methods are declared like this
 *
 *   EXPORTJAVA(procname, returntype)
 *       otherarguments...
 *       ENDEXPORTJAVA
 *   If necessary this will generate a formal environment argument.
 *
 *    
 * @ Native dynamic methods can be called directly like this
 *     
 *   CALLJAVAEXPORT(procname) otherargs ENDCALLJAVAEXPORT 
 *
 * If necessary this will include an actual environment argument;
 * the environment argument must have been either defined implicitly,
 * passed through or declared from thin air. 
 *
 ****************************************************
 * The environment parameter can be handled this way:
 * 
 * @ Passing the environment as formal argument to a procedure
 *   which doesn't do this automatically:
 *   void proc_abc(JENV_FORMAL otherargs) {
 *
 * @ Passing the environment as actual argument to a procedure
 *   which doesn't do this automatically:
 *   proc_abc(JENV_ACTUAL otherargs);
 *   proc_abc(JENV_ACTUAL_NOCOMMA);
 *
 * @ Declaring an environment variable and initializing it:
 *   JENV_DECLARE_INITFROMTHINAIR
 *
 * No commas or semicolons from caller as it may be implemented by empty 
 * macros on some architectures
 *
 *
 ****************************************************
 * Native static methods 
 *     are not yet ifdef'd usefully.
 *     Note: ONI and RNI pass a zero first arg while JNI passes the  
 *         class as first argument.
 *     ILU defines the first argument like ONI but will never use 
 *         it..
 *     
 * Ilu defines but never calls native static methods from the native side
 *     (There were too many difficulties so that life is easier
 *      by simply using a smaller subset of features)
 ****************************************************/
 
 /****************************************************
  * Exceptions.
  * Ilu has no place where it needs to clear java exceptions.
  * Ilu mostly invokes java procedures which don't raise
  * exceptions.
  *
  * To test whether a native call did generate an exceptions:
  *   if (JPENDINGEXCEPTION()) {...
  *   If necessary this will provide an actual environment argument.
  *   Return of 0 means no exception; <>0 means unspecified excption.
  ****************************************************/



/*
 * Definition samples...
 *
 * #define JAVAEXPORT(procname, returntype) \
 *     extern returntype xerox_ilu_ ## procname (
 *
 * 
 * #define CALLJAVAEXPORT(procname) \
 *     xerox_ilu_ ## procname (
 *
 */


/*
 * Generated stub header files 
 *     ONI: Prototypes are generated.
 *     JNI: No prototypes.
 *     RNI: Prototypes are generated.
 */

/* In some cases, before we've included iluxport.h, we end up including a
   file like IluJava_RNI.h or IluJava_JOps.h which in turn include
   IluJava_JTypes.h which does some ifdefing on things defined in iluwin.h.
   So we need to ensure that iluwin.h is loaded up.
*/

#if (defined(WIN32)||defined(WIN16))
#include <iluwin.h>
#elif defined( macintosh )
#include <ilumac.h>
extern void ILUStartup( void );
extern void ILUShutdown( void );
#else
#include <iluconf.h>
#endif 


/* default to ONI */
#if (! (defined(ONI) || defined(JNI) || defined(RNI) || defined(JRI)))
#define ONI
#endif

#if (defined(ONI))
  /* 
   * Old native interface.
   * No problem; that is how ilu was developed
   */


  #include <StubPreamble.h>
  #include <interpreter.h>
  #include <javaString.h>
  #include <sys_api.h>
  #include <monitor.h>
  #include <typecodes.h>
  #include <java_lang_Thread.h>


  /* This may be ridiculous, but I have no idea who defined
   * the symbol "RNI" and this has made builds awfully painfull
   * on NT systems.  Take the easy out...
   */
  #if (defined(RNI))
    #undef RNI
  #endif


  #define JAVAEXPORT(procname, returntype) \
       extern returntype xerox_ilu_ ## procname (
  #define ENDJAVAEXPORT )

  #define CALLJAVAEXPORT(procname) \
       xerox_ilu_ ## procname (
  #define ENDCALLJAVAEXPORT )

  #define JENV_FORMAL 
  #define JENV_FORMAL_NOCOMMA 
  #define JENV_ACTUAL 
  #define JENV_ACTUAL_NOCOMMA 
  #define JENV_DECLARE_INITFROMTHINAIR 


  #define JPENDINGEXCEPTION() \
      exceptionOccurred(EE())

  #define JCLEAREXCEPTION() \
      exceptionClear(EE())

  #define COPY_TO_FREEABLE_C_STRING(s) \
	allocCString(s)

#endif /*ONI*/




#if (defined(JNI))

  /*
   * JNI usage in ILU has been developped in small step, first
   * using JNI for declaring native methods, later only using
   * JNI weak pointers and only last getting rid of non-JNI features
   * if compiled for JNI.
   *
   * Once JNI became functional Ilu depricates partial use of ONI
   * and JNI but allows only global switching.
   *
   * This historical fact explains some residous conditional code snippets
   * with more detailed combinations, as well as why more detailed combinations
   * are no more supported in other files. 
   */
   
  #include <jni.h>
  #include <stdlib.h>
  #include <string.h>

  #define JAVAEXPORT(procname, returntype) \
     JNIEXPORT returntype JNICALL Java_xerox_ilu_ ## procname (JNIEnv * j_env,
  #define ENDJAVAEXPORT )

  #define CALLJAVAEXPORT(procname) \
       Java_xerox_ilu_ ## procname (j_env, 
  #define ENDCALLJAVAEXPORT )

  #define JENV_FORMAL JNIEnv * j_env,
  #define JENV_FORMAL_NOCOMMA JNIEnv * j_env
  #define JENV_ACTUAL j_env,
  #define JENV_ACTUAL_NOCOMMA j_env 
  
/* Outdated method to acquire JNIEnv
 * found in jdk1.2beta2/include/interpreter.h; changed for jdk1.2beta3
 * removed for jdk1.2rc2
 *
 *  #define JENV_DECLARE_INITFROMTHINAIR JNIEnv * j_env  \
 *     = EE2JNIEnv(SysThread2EE(sysThreadSelf()));
 */
       
  extern JNIEnv * _ilujava_ComputeTheJNIEnv();
  
  #define JENV_DECLARE_INITFROMTHINAIR JNIEnv * j_env  \
      = _ilujava_ComputeTheJNIEnv();

  #define JPENDINGEXCEPTION() \
      (*j_env)->ExceptionOccurred(j_env)

  #define JCLEAREXCEPTION() \
      (*j_env)->ExceptionClear(j_env)

  #include "IluJava_JOps.h"
      /* for IluJava_JString_toheap8x */
      
  #define COPY_TO_FREEABLE_C_STRING(s) \
      IluJava_JString_toheap8x((jobject)s)

  #endif /*JNI*/



#if (defined(RNI))
  /* 
   * Microsoft "raw native interface"
   */
 
  #include <windows.h>
  #include <native.h> 
  #include <stdio.h>
  #include <stdlib.h>
  #include <string.h>
  #include "IluJava_RNI.h"


  #define JAVAEXPORT(procname, returntype) \
     extern returntype xerox_ilu_ ## procname (
  #define ENDJAVAEXPORT )
  
  #define CALLJAVAEXPORT(procname) \
       xerox_ilu_ ## procname (
  #define ENDCALLJAVAEXPORT )
  
  #define JENV_FORMAL 
  #define JENV_FORMAL_NOCOMMA 
  #define JENV_ACTUAL 
  #define JENV_ACTUAL_NOCOMMA 
  #define JENV_DECLARE_INITFROMTHINAIR 
  
  #define JPENDINGEXCEPTION() \
      exceptionOccurred(0)

  #define JCLEAREXCEPTION() \
      exceptionClear(0)


  #define COPY_TO_FREEABLE_C_STRING(s) \
      rni_copy_to_freeable_c_string(s)


#endif /*RNI*/



#if (defined(JRI))
  /* 
   * JRI is Netscape's predecessor of RNI
   * JRI for ilu is not likely to get supported, unless contributions 
   * are received
   */
  
  #error "JRI not supported" 
  
#endif


/* these ilu defs are always needed */
#include <iluxport.h>
#include <iluerror.h>


#ifdef ENABLE_DEBUGGING
   #define JTRACE(key,code)    if (key) ilu_DebugPrintf code
#else
   #define JTRACE(key,code)    
#endif


#endif /* _ILUJAVA_INCLUDES_ */


