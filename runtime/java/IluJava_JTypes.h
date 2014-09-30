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
/* IluJava_JTypes.h */
/* Chris Jacobi, December 1, 1998 6:25 pm PST */
/* $Id: IluJava_JTypes.h,v 1.24 1999/08/03 01:54:20 janssen Exp $ */

/*
 * Macros to make the java stuff more portable
 */

#include "IluJava_Includes.h"

#ifndef _ILUJAVA_JTypes_
#define _ILUJAVA_JTypes_

/*
 * These macros define c types to be used in the ilu runtime to 
 * represent whatever the javah ntive stubber generates for the
 * hinted java type.
 * 
 * See IluConfigureSample.java for a sample java program to
 * run javah on and determine the right types for here.
 */


#if defined(JNI)

  #define Jint jint
  #define Jshort jshort
  #define Jboolean jboolean
  #define Jbyte jbyte
  #define Jlong jlong
  #define Jfloat jfloat
  #define Jdouble jdouble
  #define Jchar jchar
  
  #define JObject jobject
  #define JString jstring
  #define JArrayOfChar jcharArray
  #define JArrayOfByte jbyteArray
  #define JArrayOfString jobjectArray
  #define JArrayOfInt jintArray
  #define JArrayOfObject jobjectArray
  
#else

 #if (defined(linux))
   #define Jint int32_t
 #else
   #define Jint long
       /* surprize... but javah says long */ 
 #endif
 
  #define Jshort short
  
 #if (defined(linux))
   #define Jboolean int32_t
 #else
   #define Jboolean long
      /* surprize... but javah says long */ 
 #endif
 
  #define Jbyte char
  #define Jlong int64_t
  #define Jfloat float
  #define Jdouble double
  #define Jchar unicode
  
  #define JObject Hjava_lang_Object*
  #define JString Hjava_lang_String*
  #define JArrayOfChar HArrayOfChar*
  #define JArrayOfByte HArrayOfByte*
  #define JArrayOfString HArrayOfString*
  #define JArrayOfInt HArrayOfInt*
  #define JArrayOfObject HArrayOfObject*
 
#endif 


/*
 * Conversion macros
 */


#if (defined(LONG_INTEGER_TYPE))

#define TOIluInt64(i) ( * (ilu_longinteger *) &(i))
#define TOIluCard64(i) ( * (ilu_longcardinal *) &(i))
#define TOJavaIC64(i) ( * (Jlong *) &(i))

#else
  
  please fill it in
  
#endif


/*
 * Ilu classes
 *
 * Originally we had only defined classes here if used pervasively.
 *
 * However, need for conditional definitions has convinced use to also
 * define rarely used conceptionally local classes in this public
 * place.
 *
 * Nevertheless these are only defines; when used it is the using
 * files responsibility to import the defining .h file.
 */

#if (defined(JNI))
  #define JIluClassRep 	jobject
  #define JIluCall 	jobject
  #define JIluMethodRep	jobject
  #define JIluRT0 	jobject
  #define JIluOInt 	jobject
  #define JIluServer 	jobject
  #define JIluWPBase 	jobject
  #define JIluExceptionRep	jobject
  #define JIluAlarmTech jobject
  #define JIluPassport 	jobject
  #define JIluGCClient 	jobject
  #define JIluForkTech 	jobject
  #define JIluSerializationContext	jobject
  #define JIluPipeline 	jobject
  #define JIluMethodArgRep	jobject
  #define JIluServerConnection	jobject
  #define JIluSurrogateConnection	jobject
  #define JIluPort 	jobject
  #define JIluDebug 	jobject
  #define JIluDebugHooks	jobject
  #define JIluDebugWriter	jobject
  #define JIluInit2 	jobject
  #define JIluPickle  	jobject
  #define JIluTypeCode	jobject
  #define JIluTypeRep	jobject
  #define JIluTransportInfo	jobject
  #define JIluSBH	jobject
  #define JIluIdentity	jobject
  #define JIluIdentityType	jobject
  #define JIluObjectTable	jobject
  #define JIluServerRelocationInfo	jobject
  #define JIluGssCred jobject
  #define JIluGssOid jobject
  #define JIluBatcher jobject
#else
  #define JIluClassRep Hxerox_ilu_IluClassRep*
  #define JIluCall Hxerox_ilu_IluCall*
  #define JIluMethodRep Hxerox_ilu_IluMethodRep*
  #define JIluRT0 Hxerox_ilu_IluRT0*
  #define JIluOInt Hxerox_ilu_IluOInt*
  #define JIluServer Hxerox_ilu_IluServer*
  #define JIluWPBase Hxerox_ilu_IluWPBase*
  #define JIluExceptionRep Hxerox_ilu_IluExceptionRep*
  #define JIluAlarmTech Hxerox_ilu_IluAlarmTech*
  #define JIluPassport Hxerox_ilu_IluPassport*
  #define JIluGCClient Hxerox_ilu_IluGCClient*
  #define JIluForkTech Hxerox_ilu_IluForkTech*
  #define JIluSerializationContext Hxerox_ilu_IluSerializationContext*
  #define JIluPipeline Hxerox_ilu_IluPipeline*
  #define JIluMethodArgRep Hxerox_ilu_IluMethodArgRep*
  #define JIluServerConnection Hxerox_ilu_IluServerConnection*
  #define JIluSurrogateConnection Hxerox_ilu_IluSurrogateConnection*
  #define JIluPort Hxerox_ilu_IluPort*
  #define JIluDebug Hxerox_ilu_IluDebug*
  #define JIluDebugHooks Hxerox_ilu_IluDebugHooks*
  #define JIluDebugWriter Hxerox_ilu_IluDebugWriter*
  #define JIluInit2 Hxerox_ilu_IluInit2*
  #define JIluPickle  Hxerox_ilu_IluPickle*
  #define JIluTypeCode  Hxerox_ilu_IluTypeCode*
  #define JIluTypeRep  Hxerox_ilu_IluTypeRep*
  #define JIluTransportInfo Hxerox_ilu_IluTransportInfo*
  #define JIluSBH Hxerox_ilu_IluSBH*
  #define JIluIdentity Hxerox_ilu_IluIdentity*
  #define JIluIdentityType Hxerox_ilu_IluIdentityType*
  #define JIluObjectTable Hxerox_ilu_IluObjectTable*
  #define JIluServerRelocationInfo Hxerox_ilu_IluServerRelocationInfo*
  #define JIluGssCred Hxerox_ilu_IluGssCred*
  #define JIluGssOid Hxerox_ilu_IluGssOid*
  #define JIluBatcher Hxerox_ilu_IluBatcher*
#endif

#endif /* _ILUJAVA_JTypes_ */
