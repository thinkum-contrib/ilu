/*  -*- Mode: C; -*-
 *
 * Support for Guile Scheme has been contributed by Siemens Corporate Research, Inc.
 */

/** 
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

#include <stdio.h>

#if (defined(WIN32) && defined(_WINIO))
#include <winiodef.h>
extern int vfprintf_winio(FILE*, const char*, va_list);
/* defunct extern int g_i_on_win32s; */
/* if we're using winio, trap exits to let the user see any output 
   rather than just exiting */
extern void cschemewinio_exit(int i_status);
#define exit schemewinio_exit
#endif /* (defined(WIN32) && defined(_WINIO)) */

#define TRUE  1
#define FALSE 0
typedef int Boolean;

#include <string.h>
#include <iluptype.h>
#include <stdarg.h>

#define SEGFAULT	*((char *)1)='a'

typedef struct context_s {
  Interface interface;
  FILE *file;
  Type class;
} * Context;

string type_name(Type);
string scheme_return_type(Type type);
string scheme_type_name(Type t);
string scheme_simple_name(Name name);
string scheme_parameter_type(Type type, ArgDirection passingMode);
list new_list(void);
boolean HasFreeRoutine (Type type);
boolean IsSequenceObject(Type type, Interface i);
boolean IsPipeObject(Type type, Interface i);
boolean TypeIsStruct(Type);
boolean TypeIsArray(Type type);
boolean TypeIsString(Type type);
boolean TypeIsNonObjectStruct(Type type);
void SizeType (Type type, string name, Context context);
void EncodeValue (Type type, string name, Context context);
void UnpackValue (Context context, Type type, LineNumber line, string buffer, boolean ref, boolean allocate);
void FreeValue (Type type, char *name, Context context);
boolean BlockingCall (Procedure proc);
void generate_headers (Interface interface, FILE *file);
void generate_code (Interface interface, FILE *file);
void generate_server_code (Interface parse, FILE *file);
string scheme_constant_name (Constant e);
string scheme_interface_name (Interface i);
string scheme_string (string s);
boolean PassedByRef (Type type);
string scheme_exception_name (Exception e);
void GenerateNecessaryIncludes (Context context);
string scheme_argument_name(Argument a);
string class_procedure_name (Procedure p);
boolean IsCacheableMethod (Procedure m);

extern void SizeArgument(refany elt, refany rock);
extern void EncodeArgument(refany elt, refany rock);
extern void UnpackOutParm(refany elt, refany rock);
extern void UnmarkSupertypes(Type t);
extern void DefineMethods (Type t, Context c);
extern void GenerateNecessaryIncludes (Context context);
extern boolean IsCacheableMethod(Procedure m);
extern void generate_class_code(Type type, Context context);
extern char* interface_header_name(char* pc_interfacename);
extern boolean ReadSynonyms (string filename);
extern void declare_object_type(Type type, Context c);

extern cardinal MethodRecordID;

extern void MarshO(Context context, char *fmt, ...);
extern void MarshS(Context context, char *fmt,...);

