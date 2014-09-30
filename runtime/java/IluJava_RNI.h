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
/* IluJava_RNI.h */
/* Chris Jacobi, November 25, 1998 5:43 pm PST */

/*
 */
 
/* 
 * $Id: IluJava_RNI.h,v 1.6 1999/08/03 01:54:25 janssen Exp $ 
 *
 * Implements various abstractions for Microsoft's Raw Native Interface
 */

#include "IluJava_JTypes.h"

#ifdef RNI
#ifndef _ILUJAVA_RNI_H_
#define _ILUJAVA_RNI_H_



/* *************************************************************** */
/* declares a cached pointer to a class' field  - assumes class is properly found */

#define RNI_DECLARE_OBJECT_FIELD(xerox_ilu_class_name, field_name) \
	extern struct fieldblock* gpfield_xerox_ilu_ ## xerox_ilu_class_name ## _ ## field_name



/* *************************************************************** */
/* defines a cached pointer to a class' field */

#define RNI_DEFINE_OBJECT_FIELD(xerox_ilu_class_name, field_name) \
	struct fieldblock* gpfield_xerox_ilu_ ## xerox_ilu_class_name ## _ ## field_name 



/* *************************************************************** */
/* Inits a cached pointer to a class' field  - assumes class is properly found */

#define RNI_INIT_OBJECT_FIELD(xerox_ilu_class_name, field_name) \
	gpfield_xerox_ilu_ ## xerox_ilu_class_name ## _ ## field_name = \
		Class_GetField( FindClass(NULL, "xerox/ilu/" #xerox_ilu_class_name , ilu_TRUE), #field_name )



/* *************************************************************** */
/* defines get method for a class' field */

#define RNI_GET_OBJECT_FIELD(java_object, xerox_ilu_class_name, field_name) \
	Field_GetObject((HObject*) java_object,  \
		gpfield_xerox_ilu_ ## xerox_ilu_class_name ## _ ## field_name )



/* *************************************************************** */
/* defines get method for a class' field */

#define RNI_SET_OBJECT_FIELD(java_object, value_java_object, xerox_ilu_class_name, field_name) \
	Field_SetObject((HObject*)java_object,  \
		gpfield_xerox_ilu_ ## xerox_ilu_class_name ## _ ## field_name, \
		value_java_object )



/* *************************************************************** */
/* Returns a C copy of the java string - caller is responsible for
   freeing the returned storage (equivalent of ONI's allocCString) */

extern char *rni_copy_to_freeable_c_string(Hjava_lang_String * str_a_java_string);


/* returns an ID for the current thread by hashing on the thread's name */

extern long rni_current_thread();



/* functions to get a copy of and copy back arrays - note that we use copies
since Ilu uses Non blocking IO, which means that any ILU IO operation could
end up putting us back into java */

extern Jint* rni_array_int_get(JArrayOfInt jh_intarray);

extern void rni_array_int_release(JArrayOfInt jh_intarray, Jint* p_jintarray_copy);

extern Jbyte* rni_array_byte_get(JArrayOfByte jh_bytearray);

extern void rni_array_byte_release(JArrayOfByte jh_bytearray, Jbyte* p_jbytearray_copy);

extern void rni_array_byte_release_copyback(JArrayOfByte jh_bytearray, Jbyte* p_jbytearray_copy);

extern Jchar* rni_array_char_get(JArrayOfChar jh_chararray);

extern void rni_array_char_release(JArrayOfChar jh_chararray, Jchar* p_jchararray_copy);

extern Jchar* rni_jstring_get(JString jh_string);

extern void rni_jstring_release(JString jh_string, Jchar* p_jstring_copy);


#endif
#endif


