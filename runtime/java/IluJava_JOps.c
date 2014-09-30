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
/* IluJava_JOps.c */
/* Chris Jacobi, November 20, 1998 9:41 pm PST */

/*
 */
 
/* $Id: IluJava_JOps.c,v 1.18 1999/08/03 01:54:30 janssen Exp $ */

/*
 * Handy java access operations.
 * Implementation makes disgusting use of java implementation functionality
 */

#include "IluJava_Includes.h"
#include "IluJava_JOps.h"
#include "IluJava_JArrays.h"
#include "IluJava_Common.h"

#if defined(JNI)
/*
    #include <typecodes.h>
*/
    #include <string.h>
    #include <stdlib.h>
    typedef unsigned short unicode;
#endif


static void
ilujava_copy8to16(char* src, unicode* dst, unsigned len)
/* strictly converts and copies len units; no
 * fancy terminator charactors.
 */
{
    while (len) {
        *dst++ = *src++;
        len--;
    }
}
   
     
static void
ilujava_copy16to8(const unicode *src, char *dst, unsigned len)
/* strictly converts and copies len units; no
 * fancy terminator charactors.
 */
{
    while (len) {
        *dst++ = (char) *src++;
        len--;
    }
}
     

extern JArrayOfByte 
IluJava_JAoB_from8(JENV_FORMAL char* data, unsigned len) 
    /* Allocates and initializes a Java array of byte.
     * data: initial data interpreted as 8 bit bytes 
     * len: length of data (or array) in 8 bit units
     * Caller owns data. 
     */
{
    JArrayOfByte jja;
    #if defined(JNI)
        jja = (*JENV_ACTUAL_NOCOMMA)->NewByteArray(JENV_ACTUAL (jint) len);
        (*JENV_ACTUAL_NOCOMMA)->SetByteArrayRegion(JENV_ACTUAL
            jja,
            0,
            (jint) len,
            (jbyte*) data
            );
    #else
        jja = (JArrayOfByte) ArrayAlloc(T_BYTE, len);
        if (jja == 0) {
            _ilujava_throwException(JENV_ACTUAL 2, "java/lang/OutOfMemoryError", 0);
        } else if (len) {
            memcpy((char*) unhand(jja)->body, (char*) data, len);
        }
    #endif
    return jja;
} /*IluJava_JAoB_from8*/


extern JArrayOfChar 
IluJava_JAoC_from8(JENV_FORMAL char * data, unsigned len) 
    /* Allocates and initializes a Java array of char
     * data: initial data interpreted as 8 bit characters 
     * len: length of data in 8 bit units
     * Caller owns data 
     */
{
    JArrayOfChar jja;
    #if defined(JNI)
        jchar* cArray;
        jja = (*JENV_ACTUAL_NOCOMMA)->NewCharArray(JENV_ACTUAL (jint) len);
        /*I'm not using SetCharArrayRegion because I don't know whether
         *the input is contigous or not; with GetCharArrayElements we know
         */
        cArray = (*JENV_ACTUAL_NOCOMMA)->
            GetCharArrayElements(JENV_ACTUAL jja, 0);
        ilujava_copy8to16((char*) data, (unicode*) cArray, len);
        (*JENV_ACTUAL_NOCOMMA)->
            ReleaseCharArrayElements(JENV_ACTUAL jja, cArray, 0);
    #else
        jja = (JArrayOfChar) ArrayAlloc(T_CHAR, len);
        if (jja == 0) {
            _ilujava_throwException(JENV_ACTUAL 2, "java/lang/OutOfMemoryError", 0);
        } else if (len) {
            ilujava_copy8to16((char*) data, (unicode*) unhand(jja)->body, len);
        }
    #endif
    return jja;
} /*IluJava_JAoC_from8*/


extern JString
IluJava_JString_fromA0(JENV_FORMAL char* data)
    /* Assumes input data to be null terminated ascii string. 
     * Create java string
     */
{
    JString jh_string;
    #if defined(JNI) 
        jh_string = (*JENV_ACTUAL_NOCOMMA)->
            NewStringUTF(JENV_ACTUAL (char*) data);
    #else
        jh_string = makeJavaString(JENV_ACTUAL (char*) data, strlen(data));
    #endif
    return jh_string;
}


extern JString
IluJava_JString_fromAX(JENV_FORMAL char* data, int leng)
    /* Assumes input data to be ascii string. 
     * Create java string
     */
{
    JString jh_string;
    #if defined(JNI) 
        /* make a null terminated copy ... */
        char* cstring;
        cstring = ilu_malloc(leng+1);
        memcpy(cstring, data, leng);
        cstring[leng] = 0;
        jh_string = (*JENV_ACTUAL_NOCOMMA)->
            NewStringUTF(JENV_ACTUAL cstring);
        free(cstring);
    #else
        jh_string = makeJavaString(JENV_ACTUAL (char*) data, leng);
    #endif
    return jh_string;
}


extern JArrayOfChar 
IluJava_JAoC_from16(JENV_FORMAL char * data, unsigned len) 
    /* Allocates and initializes a Java array of char.
     * data: initial data interpreted as 16 bit characters 
     * len: length of data in 16 bit units
     * Caller owns data 
     */
{
    JArrayOfChar jja;
    #if defined(JNI)
        jja = (*JENV_ACTUAL_NOCOMMA)->NewCharArray(JENV_ACTUAL (jint) len);
        if (jja) {
            (*JENV_ACTUAL_NOCOMMA)->SetByteArrayRegion(JENV_ACTUAL
                jja,
                0,
                (jint) len,
                (jbyte*) data
                );
        }
    #else
        jja = (JArrayOfChar) ArrayAlloc(T_CHAR, len);
        if (jja == 0) {
            _ilujava_throwException(JENV_ACTUAL 2, "java/lang/OutOfMemoryError", 0);
        } else if (len) {
            memcpy((char*) unhand(jja)->body, (char*) data, len*2);
        }
    #endif
    return jja;
}


/*not used anymore */ void 
IluJava_JString_toC16(JENV_FORMAL JString jjs, char * buffer, unsigned len) 
    /* Copies 16 bit characters from a java String.
     * buffer: "array" of 16 bit character units 
     * len: length of buffer in 16 bit units
     * Caller owns buffer  
     */
{
    #if defined(ONI)
        /* javaString.h */
        javaString2unicode(jjs, (unicode *) buffer, len);
    #elif defined(RNI)
        unicode * ptr16;
        ptr16 = javaStringStart(jjs);
        memcpy(buffer, ptr16, len*2);
    #else
        /* including JNI */
        /*
        Jchar* char16Ptr;
        char16Ptr = JSTRING_GETCHARPTR(jjs);
        memcpy(buffer, char16Ptr, len*2);
        JSTRING_RELEASECHARPTR(jjs, char16Ptr)
        */
    #endif
}
  
     
extern char * 
IluJava_JString_toheap8x(JENV_FORMAL JString jjs) 
    /* Takes java string; Convert to a heap allocated 
     * null terminated C string. 
     * Special case: Converts java null to C 0. 
     */
{
    char* cstring = 0;
    if (jjs == 0) {return 0;}
    #if defined(JNI)
    {
        int leng = (*JENV_ACTUAL_NOCOMMA)->GetStringUTFLength(JENV_ACTUAL jjs);
        char* ctemp;
        ctemp = (char*)
            (*JENV_ACTUAL_NOCOMMA)->GetStringUTFChars(JENV_ACTUAL jjs, 0);
        cstring = ilu_malloc(leng+1);
        memcpy(cstring, ctemp, leng);
        cstring[leng] = 0;
        (*JENV_ACTUAL_NOCOMMA)->ReleaseStringUTFChars(JENV_ACTUAL jjs, ctemp);
    }
    #else
        cstring = COPY_TO_FREEABLE_C_STRING(jjs);
    #endif
    return cstring;
}

extern char * 
IluJava_JString_toheap80(JENV_FORMAL JString jjs) 
    /* Takes java string; ignores high bytes and copies low bytes
     * to a newly allocated null terminated C string (in the C heap).
     * Caller receives ownership of returned string. 
     * Special case: Converts java null to C 0. 
     */
{
    char* cstring = 0;
    if (jjs != 0) {
        int leng = 0;
        Jchar* char16Ptr;
        leng = JSTRING_LENGTH(jjs);
        cstring = ilu_malloc(leng+1);
        char16Ptr = (Jchar*) JSTRING_GETCHARPTR(jjs);
        ilujava_copy16to8(char16Ptr, cstring, leng);
        JSTRING_RELEASECHARPTR(jjs, char16Ptr);
        cstring[leng] = 0;
    }
    return cstring;
}


/* NOT USED ANYMORE */ 
void 
IluJava_JAoC_toC16(JENV_FORMAL JArrayOfChar jja, char * buffer, unsigned cnt) 
    /* Copies java array of char into a C 16-bit buffer.
     * buffer: "array" of 16 bit character units; must be long 
     * enough to receive all cnt chars plus terminating 0C unit. 
     * cnt: Number of 8 bit units to copy  (Must be >= 1 )
     * Caller owns buffer  
     * Ugly but checked November 26, 1996 11:07:36 pm PST 
     */
{
    /* compare to string.c::javaString2unicode */
    Jchar* char16Ptr;
    char16Ptr = ARRAY_char_GET(jja);
    memcpy(buffer, char16Ptr, cnt*2);
    buffer[cnt*2] = 0;
    buffer[cnt*2+1] = 0;
    ARRAY_char_RELEASE(jja, char16Ptr);
}



extern void 
IluJava_JAoC_toC8(JENV_FORMAL JArrayOfChar jja, char * buffer, unsigned cnt) 
    /* Copies java array of char into a C string buffer.
     * Restricted to 8 bit units of the java chars. 
     * buffer: string buffer; must be long enough to receive 
     * all cnt chars plus terminating 0C char.
     * cnt: Number of 8 bit units to copy  (Must be >= 1 )
     * Caller owns buffer; 
     */
/* ALL USES December 30, 1997 ALLOCATE THE ARRAY JUST
 * BEFORE CALLING THIS
 */
{
    /* compare to string.c::javaString2CString */
    Jchar* char16Ptr;
    char16Ptr = ARRAY_char_GET(jja);
    ilujava_copy16to8(char16Ptr, buffer, cnt);
    buffer[cnt] = 0;
    ARRAY_char_RELEASE(jja, char16Ptr);
}	

/* end */






