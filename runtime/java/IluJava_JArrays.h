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
/* IluJava_JArrays.h */
/* Chris Jacobi, January 8, 1998 4:59 pm PST */

/*
 */
 
/* $Id: IluJava_JArrays.h,v 1.11 1999/08/03 01:54:33 janssen Exp $ */

/*
 * Macros to access java arrays in a portable way
 */

#ifndef _ILUJAVA_JArrays_
#define _ILUJAVA_JArrays_

#include "IluJava_Includes.h"
#include "IluJava_JTypes.h"

/* 
 * Sofar we do not yet have writing of arrays but only reading.
 * Do not modify the array; it may be the original; it may be a copy.
 * One exception:  Macros named *_COPYBACK will modify
 * the array. 
 *
 * Notice the difference between arrays of objects and 
 * arrays primitive types.
 *
 * Array operations on primitive types "lock" a pointer and then 
 * release the pointer when done.
 */

#if defined(JNI)

  #define ARRAY_LENGTH(jh_arr) \
      (*JENV_ACTUAL_NOCOMMA)->GetArrayLength(JENV_ACTUAL jh_arr)
  
  #define ARRAY_object_GET(jh_arr, index) \
      (*JENV_ACTUAL_NOCOMMA)->GetObjectArrayElement(JENV_ACTUAL jh_arr, index)
  #define ARRAY_object_PUT(jh_arr, index, jh_el) \
      (*JENV_ACTUAL_NOCOMMA)->) \
      SetObjectArrayElement(JENV_ACTUAL jh_arr, index, jh_el);


  #define ARRAY_int_GET(jh_arr) \
      (*JENV_ACTUAL_NOCOMMA)->GetIntArrayElements(JENV_ACTUAL jh_arr, 0)
  #define ARRAY_byte_GET(jh_arr) \
      (*JENV_ACTUAL_NOCOMMA)->GetByteArrayElements(JENV_ACTUAL jh_arr, 0)
  #define ARRAY_char_GET(jh_arr) \
      (*JENV_ACTUAL_NOCOMMA)->GetCharArrayElements(JENV_ACTUAL jh_arr, 0)
      
  #define ARRAY_int_RELEASE(jh_arr, ptr) \
      (*JENV_ACTUAL_NOCOMMA)-> \
      ReleaseIntArrayElements(JENV_ACTUAL jh_arr, ptr, JNI_ABORT);
  #define ARRAY_byte_RELEASE(jh_arr, ptr) \
      (*JENV_ACTUAL_NOCOMMA)-> \
      ReleaseByteArrayElements(JENV_ACTUAL jh_arr, (jbyte*) ptr, JNI_ABORT);
  #define ARRAY_byte_RELEASE_COPYBACK(jh_arr, ptr) \
      (*JENV_ACTUAL_NOCOMMA)-> \
      ReleaseByteArrayElements(JENV_ACTUAL jh_arr, (jbyte*) ptr, 0);
  #define ARRAY_char_RELEASE(jh_arr, ptr) \
      (*JENV_ACTUAL_NOCOMMA)-> \
      ReleaseCharArrayElements(JENV_ACTUAL jh_arr, ptr, JNI_ABORT);
      
  #define JSTRING_LENGTH(jh_s) \
      (*JENV_ACTUAL_NOCOMMA)->GetStringLength(JENV_ACTUAL jh_s)
  #define JSTRING_GETCHARPTR(jh_s) \
      (*JENV_ACTUAL_NOCOMMA)->GetStringChars(JENV_ACTUAL jh_s, 0)
  #define JSTRING_RELEASECHARPTR(jh_s, ptr) \
      (*JENV_ACTUAL_NOCOMMA)-> \
      ReleaseStringChars(JENV_ACTUAL jh_s, ptr);

#elif defined(ONI)

  #define ARRAY_LENGTH(jh_arr) \
      obj_length(jh_arr)
  
  #define ARRAY_object_GET(jh_arr, index) \
      (unhand(jh_arr)->body[index])
  #define ARRAY_object_PUT(jh_arr, index, jh_el) \
      unhand(jh_arr)->body[index] = jh_el;

  #define ARRAY_int_GET(jh_arr) \
      (Jint*) &(unhand(jh_arr)->body[0])
  #define ARRAY_byte_GET(jh_arr) \
      (Jbyte*) &(unhand(jh_arr)->body[0])
  #define ARRAY_char_GET(jh_arr) \
      (Jchar*) &(unhand(jh_arr)->body[0])
      
  #define ARRAY_int_RELEASE(jh_arr, ptr) \
      /* no-op */
  #define ARRAY_byte_RELEASE(jh_arr, ptr) \
      /* no-op */
  #define ARRAY_byte_RELEASE_COPYBACK(jh_arr, ptr) \
      /* no-op */
  #define ARRAY_char_RELEASE(jh_arr, ptr) \
      /* no-op */

  #define JSTRING_LENGTH(jh_s) \
      javaStringLength(jh_s)
  #define JSTRING_GETCHARPTR(jh_s) \
      (Jchar*) &(unhand(unhand(jh_s)->value)->body[unhand(jh_s)->offset])
  #define JSTRING_RELEASECHARPTR(jh_s, ptr) \
      /* no-op */

#elif defined(RNI)

  #define ARRAY_LENGTH(jh_arr) \
      obj_length(jh_arr)
  
  #define ARRAY_object_GET(jh_arr, index) \
      (jh_arr->body[index])
  #define ARRAY_object_PUT(jh_arr, index, jh_el) \
      jh_arr->body[index] = jh_el;

  #define ARRAY_int_GET(jh_arr) \
      rni_array_int_get(jh_arr)
  #define ARRAY_byte_GET(jh_arr) \
      rni_array_byte_get(jh_arr)
  #define ARRAY_char_GET(jh_arr) \
      rni_array_char_get(jh_arr)
      
  #define ARRAY_int_RELEASE(jh_arr, ptr) \
      rni_array_int_release(jh_arr, ptr)
  #define ARRAY_byte_RELEASE(jh_arr, ptr) \
      rni_array_byte_release(jh_arr, ptr)
  #define ARRAY_byte_RELEASE_COPYBACK(jh_arr, ptr) \
      rni_array_byte_release_copyback(jh_arr, ptr)
  #define ARRAY_char_RELEASE(jh_arr, ptr) \
      rni_array_char_release(jh_arr, ptr)

  #define JSTRING_LENGTH(jh_s) \
      javaStringLength(jh_s)
  #define JSTRING_GETCHARPTR(jh_s) \
      rni_jstring_get(jh_s)
  #define JSTRING_RELEASECHARPTR(jh_s, ptr) \
      rni_jstring_release(jh_s, ptr)

#else
#error "None of RNI, ONI or JNI defined"
#endif 

#endif /* _ILUJAVA_JArrays_ */
