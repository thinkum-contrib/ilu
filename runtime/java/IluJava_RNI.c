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
/* IluJava_RNI.c */
/* Chris Jacobi, November 25, 1998 5:45 pm PST */

/*
 */
 
/* 
 * $Id: IluJava_RNI.c,v 1.8 1999/08/03 01:54:23 janssen Exp $ 
 *
 * Implements various abstractions for Microsoft's Raw Native Interface
 */

#ifdef RNI

#include "IluJava_Includes.h"
#include "IluJava_Common.h"
#include "IluJava_JArrays.h"


/* *************************************************************** */
/* Returns a C copy of the java string - caller is responsible for
   freeing the returned storage (equivalent of ONI's allocCString) */

char *rni_copy_to_freeable_c_string(Hjava_lang_String * str_a_java_string) {

	int i_string_allocation_length;	/* size of the C buffer to allocate */
	char* pc_c_copy;				/* buffer into which copy is made */
	
	/* allocate the C space */
	i_string_allocation_length = javaStringLength(str_a_java_string) + 1;
	pc_c_copy = (char*) malloc(i_string_allocation_length);
	
	/* return a copy */
	return javaString2CString(str_a_java_string, pc_c_copy, i_string_allocation_length);
}


/* returns an ID for the current thread by hashing on the thread's name */
long rni_current_thread() {

	extern struct Hjava_lang_Thread *java_lang_Thread_currentThread(struct Hjava_lang_Thread *);
	long l_thread_id = 0;

/*	Following had problems with calling hashCode - it there something wierd about strings?
    Abandoned in favor of a utility function (threadId) in Java
	struct Hjava_lang_Thread * p_current_thread;
	Hjava_lang_String* p_thread_name;
 
	p_current_thread = java_lang_Thread_currentThread(0);
	if (! p_current_thread) {
		ilu_DebugPrintf("$ rni_current_thread - couldn't get current thread\n");
		goto finish;
	}

	p_thread_name = (Hjava_lang_String*) execute_java_dynamic_method(EE(), (HObject*) p_current_thread, "getName", "()Ljava/lang/String;");
	if (! p_thread_name) {
		ilu_DebugPrintf("$ rni_current_thread - couldn't get current thread name\n");
		goto finish;
	}

	l_thread_id = execute_java_dynamic_method(EE(), (HObject*) p_thread_name, "hashCode", "()I");
	
finish:

*/
	// xxx assumes one-to-one correspondence between java threads and os threads
	__declspec(dllimport) DWORD __stdcall GetCurrentThreadId(void);
	l_thread_id = GetCurrentThreadId(); // make OS call to get OS thread id

	// l_thread_id = execute_java_static_method(EE(), FindClass(EE(), "xerox/ilu/IluInit", ilu_TRUE), "threadId", "()I");

	/* ilu_DebugPrintf("$ rni_current_thread - id = %d\n", l_thread_id); */

	return l_thread_id; /* return the hash code as the thread's ID */
}

/* functions to get a copy of and copy back arrays - note that we use copies
since Ilu uses Non blocking IO, which means that any ILU IO operation could
end up putting us back into java */

Jint* rni_array_int_get(JArrayOfInt jh_intarray) {
	
	unsigned long l_array_length_in_bytes = ARRAY_LENGTH(jh_intarray) * sizeof(Jint);
	Jint* p_jintarray_copy = ilu_malloc(l_array_length_in_bytes);
	memcpy (p_jintarray_copy, &(jh_intarray->body[0]), l_array_length_in_bytes);
	return p_jintarray_copy;
}


void rni_array_int_release(JArrayOfInt jh_intarray, Jint* p_jintarray_copy) {
    /* no need to copy back
     * unsigned long l_array_length_in_bytes = ARRAY_LENGTH(jh_intarray) * sizeof(Jint);
     * memcpy (&(jh_intarray->body[0]), p_jintarray_copy, l_array_length_in_bytes);
     */
    ilu_free(p_jintarray_copy);
    return;
}


Jbyte* rni_array_byte_get(JArrayOfByte jh_bytearray) {
	
	unsigned long l_array_length_in_bytes = ARRAY_LENGTH(jh_bytearray) * sizeof(Jbyte);
	Jbyte* p_jbytearray_copy = ilu_malloc(l_array_length_in_bytes);
	memcpy (p_jbytearray_copy, &(jh_bytearray->body[0]), l_array_length_in_bytes);
	return p_jbytearray_copy;
}


void rni_array_byte_release_copyback(
        JArrayOfByte jh_bytearray, Jbyte* p_jbytearray_copy)
{
    unsigned long l_array_length_in_bytes 
        = ARRAY_LENGTH(jh_bytearray) * sizeof(Jbyte);
    memcpy (&(jh_bytearray->body[0]), p_jbytearray_copy, l_array_length_in_bytes);
    ilu_free(p_jbytearray_copy);
    return;
}


void rni_array_byte_release(JArrayOfByte jh_bytearray, Jbyte* p_jbytearray_copy) {
	
    ilu_free(p_jbytearray_copy);
    return;
}


Jchar* rni_array_char_get(JArrayOfChar jh_chararray) {
	
	unsigned long l_array_length_in_chars = ARRAY_LENGTH(jh_chararray) * sizeof(Jchar);
	Jchar* p_jchararray_copy = ilu_malloc(l_array_length_in_chars);
	memcpy (p_jchararray_copy, &(jh_chararray->body[0]), l_array_length_in_chars);
	return p_jchararray_copy;
}


void rni_array_char_release(JArrayOfChar jh_chararray, Jchar* p_jchararray_copy) {
    /* no need to copy back
     * unsigned long l_array_length_in_chars = ARRAY_LENGTH(jh_chararray) * sizeof(Jchar);
     * memcpy (&(jh_chararray->body[0]), p_jchararray_copy, l_array_length_in_chars);
     */
    ilu_free(p_jchararray_copy);
    return;
}


Jchar* rni_jstring_get(JString jh_string) {
	
	unsigned long l_array_length_in_chars = (JSTRING_LENGTH(jh_string) + 1) * sizeof(Jchar);
	Jchar* p_jstring_copy = ilu_malloc(l_array_length_in_chars);
	memcpy (p_jstring_copy, javaStringStart(jh_string), l_array_length_in_chars);
	return p_jstring_copy;
}


void rni_jstring_release(JString jh_string, Jchar* p_jstring_copy) {
	
	unsigned long l_array_length_in_chars = (JSTRING_LENGTH(jh_string) + 1) * sizeof(Jchar);
	memcpy (javaStringStart(jh_string), p_jstring_copy, l_array_length_in_chars);
	ilu_free(p_jstring_copy);
	return;
}



#endif

/* ********** End of file ********************************************* */

