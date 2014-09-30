/** $Id: nglib.h,v 1.11 1999/08/03 01:58:12 janssen Exp $
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
/* Last edited by Mike Spreitzer October 9, 1998 2:01 pm PDT */

#ifndef __nglib_h_
#define __nglib_h_

#ifdef __cplusplus
extern "C" {
#endif


#include <stdio.h>
#include <time.h>

/* for file information */
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/types.h>

#if (defined WIN32 || defined WIN16)
#include <winsock.h>
#include <io.h>
#include <direct.h>
#define CHDIR_FUNTION _chdir
#define GETCWD_FUNTION _getcwd
#define FSTAT_FUNCTION _fstat
#define STAT_STRUCT struct _stat
#define OPEN_FUNCTION _open
#define READ_FUNCTION _read
#define SEEK_FUNCTION _lseek
#define CLOSE_FUNCTION _close
#define RDONLY_MODE _O_RDONLY
#define BINARY_MODE _O_BINARY
#define PUTENV_FUNCTION _putenv
#define MILLISEC_SLEEP_FUNCTION Sleep
#else
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <ctype.h>
#define CHDIR_FUNTION chdir
#define GETCWD_FUNTION getcwd
#define FSTAT_FUNCTION fstat
#define STAT_STRUCT struct stat
#define OPEN_FUNCTION open
#define READ_FUNCTION read
#define SEEK_FUNCTION lseek
#define CLOSE_FUNCTION close
#define RDONLY_MODE O_RDONLY
#define BINARY_MODE 0
#define PUTENV_FUNCTION putenv
extern void nglib_microsleep(int);
#define MILLISEC_SLEEP_FUNCTION(msecs) nglib_microsleep(msecs * 1000)
#endif


/* include ILU header */
#include <iluxport.h>

/* include Web interfaces */
#include "NgBasic.h"
#include "IANA_Charsets_Registry.h"
#include "NgCache.h"
#include "NgStream.h"
#include "NgRendering.h"
#include "iluhttp.h"
#include "NgProperty.h"
#include "NgFormProcessor.h"
#include "NgDocument.h"



#ifndef HAVE_GETHOSTNAME_PROTOTYPE
extern int gethostname(char *name, int namelen);
#endif /* ifndef HAVE_GETHOSTNAME_PROTOTYPE */



/* ********************************************************* */
/* Macros  Functions                                         */
/* ********************************************************* */


#define NGLIB_NYI(what_is_not_implemented) { \
	ilu_DebugPrintf("%s - Not Yet Implemented in %s, line %i\n", what_is_not_implemented, __FILE__,  __LINE__); \
*((int*)0) = 0;}


#define nglib_min(x, y) ( x < y ? x : y)
#define nglib_max(x, y) ( x > y ? x : y)


/* ********************************************************* */
/* typedefs                                                  */

/* the kind of procedure ilu wants for forking */
typedef void (*nglib_fork_procedure)(void *arg);



/* ********************************************************* */
/* initialization and shutdown - init needs to be called if
serializers are going to be used
*/

extern void nglib_init();

extern void nglib_shutdown();


/* ********************************************************* */
/* ********************************************************* */
/* Serializer and related utility functions                  */


/* return the Serializer that should be used for calls on the 
given object.  It creates a serializer if need be, and increments
the number of objects using this serializer. */

extern ILU_C_Serializer nglib_get_object_serializer (ILU_C_OBJECT object);

/* Called to release the given object's serializer. It decrements the number of objects 
having gotten the  object's server's serializer and also destroys the serializer if there are 
that reaches zero - hence the number of calls to  release_object_serializer should 
equal the number of calls to get_object_serializer */

extern void nglib_release_object_serializer (ILU_C_OBJECT object);


/* nglib_rejuvenate_objects_server can be used to boost an object's
server reference count for a short period of time, to try to take advantage
of temporal locality.  When a server becomes empty of objects, it and
all it's connections get garbage collected -- the intent of this function
to keep the server 'in-use' for a short period of time (currently 5 seconds)
after becoming empty, on the bet that we'll soon be using another object in 
that server and would like to have connections still set up and ready to be used. */
extern void nglib_rejuvenate_objects_server(ILU_C_OBJECT object);


/* ********************************************************* */
/* General Utility Functions                                 */
/* ********************************************************* */


/* ********************************************************* */
/* show a CORBA_Environment exception if we have one         */

extern ilu_boolean nglib_show_and_clear_if_exception (CORBA_Environment* p_ilu_env, 
											   char* pc_situation, FILE* p_file);


/* ********************************************************* */
/* creates and returns a duplicate of the NULL-terminated
   string parameter                                          */

char* nglib_duplicate_c_string (char *input_string);


/* ********************************************************* */
/* file extension to content-type mapping                    */
/* ********************************************************* */


/* ********************************************************* */
/* returns the appropriate Content-Type string for the given 
file extension, xxx does this by brute force linear search   */

extern char* nglib_content_type_from_extension (char* pc_extension);


/* ********************************************************* */
/* Return a pointer to the start of the file extension in pc_path,
   or NULL if none found, If i_length is positive, it should indicate
   the number of characters in pc_path */
extern char* nglib_extension_in_pathname(char* pc_path, int i_length);


/* ********************************************************* */
/* path validation                                           */
/* ********************************************************* */

/*
Returns ilu_TRUE if canonicalized pc_pathname (sans the drive letter 
on windows) begins with pc_prefix 
Assumes that pc_pathname has at least one directory separator in it. 
Trys to rely on getcwd to provide pathname canonicalization */

extern ilu_boolean nglib_validate_path (char* pc_pathname, char* pc_prefix);



/* ********************************************************* */
/* Time related                                              */
/* ********************************************************* */

/* ********************************************************* */
/* return a char* containing the UTC time in asctime format */

extern char* nglib_get_time_string (time_t* p_timer);

/* ********************************************************* */
/* return a char* containing the current UTC time in asctime format */

extern char* nglib_get_current_time_string ();


/* ********************************************************* */
/* NG structure related                                      */
/* ********************************************************* */

/* ************************************************************ */
/* cleanly initialize a NgRendering_RenderingPreferences struct */

extern NgRendering_RenderingPreferences*  
nglib_NgRendering_RenderingPreferences_clean_assign(NgRendering_RenderingPreferences* p_preferences);


/* ********************************************************* */
/* return a pointer to a cleanly initialized 
   NgRendering_RenderingPreferences struct */

extern NgRendering_RenderingPreferences*  
nglib_NgRendering_RenderingPreferences_clean_allocate();


/* ********************************************************* */
/* makes the NgRendering_Rendering struct cleanly initialized */

extern NgRendering_Rendering* 
nglib_NgRendering_Rendering_clean_assign(NgRendering_Rendering* p_rendering);


/* ********************************************************* */
/* return a pointer to a cleanly initialized 
   NgRendering_Rendering struct */

extern NgRendering_Rendering* 
nglib_NgRendering_Rendering_clean_allocate();


/* ********************************************************* */
/* NG exception related                                      */
/* ********************************************************* */

/* ********************************************************* */
/* sets the ilu_env to an _NgBasic__Exception_ObjectNotExist exception with
   the specified reason and specifics info. pc_reason owned by caller */
extern void nglib_assign_ObjectNotExist(ILU_C_ENVIRONMENT* ilu_env, char* pc_reason, 
						   NgBasic_OptionalPickle specifics_pickle);


/* sets the ilu_env to an _NgRendering__Exception_NoRenderingMatch exception with
   the specified preferences */
extern void nglib_assign_NoRenderingMatch(ILU_C_ENVIRONMENT* ilu_env, 
						   NgRendering_RenderingPreferences* p_preferences);


/* ********************************************************* */
/* NG utility                                                */
/* ********************************************************* */


/* returns TRUE if the rendering types match - 
xxx very simple and assuming implementation
(assumes rendering type is of form type/subtype )   */
extern ilu_boolean nglib_rendering_types_match (NgRendering_RenderingType pc_stringA, 
								   NgRendering_RenderingType pc_stringB);


/* returns TRUE if pc_string has a match in the sequence, else FALSE */

extern ilu_boolean nglib_is_rendering_type_member (NgRendering_RenderingTypeSequence* p_sequence, 
									  NgRendering_RenderingType pc_string);


/* returns TRUE if i_value has a match in the sequence, else FALSE */
extern ilu_boolean nglib_is_IANA_Charsets_Registry_CharsetMIBEnumValueSequence_member 
(IANA_Charsets_Registry_CharsetMIBEnumValueSequence* p_sequence, 
 IANA_Charsets_Registry_CharsetMIBEnumValue i_value);


/* ********************************************************* */
/* printout a sequence of strings from a buffer              */

extern void nglib_print_string_sequence (unsigned long i_length, 
										 char** ppc_strings, FILE* p_file);

/* ********************************************************* */
/* NG String related                                         */
/* ********************************************************* */

/* returns TRUE if i_value has a match in the sequence, else FALSE */
extern ilu_boolean nglib_is_NgBasic_StringSequence_member  
(NgBasic_StringSequence* p_sequence, NgBasic_String pc_string);


/* ********************************************************* */
/* Basic Exception related                                   */
/* ********************************************************* */

extern void nglib_print_basic_exception_info(NgBasic_ExceptionInformation* p_exception_info, 
											 FILE* p_file);

/* ********************************************************* */
/* ILU HTTP related                                          */
/* ********************************************************* */

/* ********************************************************* */
/* printout an iluhttp request                               */

extern void nglib_print_iluhttp_request (iluhttp_Request* p_the_request, FILE* p_file);


/* ********************************************************* */
/* printout an iluhttp response                              */

extern void nglib_print_iluhttp_response (iluhttp_Response* p_the_response, 
										  int i_show_body_too, FILE* p_file);


/* ********************************************************* */
/* NgRendering related                                       */
/* ********************************************************* */

/* ********************************************************* */
/* printout an NgRendering Preferences                       */

extern void nglib_print_ngrendering_preferences(NgRendering_RenderingPreferences* p_preferences, 
								   FILE* p_file);


/* ********************************************************* */
/* printout an NgRendering Rendering                         */

extern void nglib_print_ngrendering_rendering(NgRendering_Rendering* p_rendering, 
								  int i_renderingbytes_too, FILE* p_file);


/* ********************************************************* */
/* printout an NgRendering RenderingChunk                    */

extern void nglib_print_ngrendering_rendering_chunk(NgRendering_RenderingChunk* p_chunk, 
								  int i_renderingbytes_too, FILE* p_file);


/* ********************************************************* */
/* prints out a NgRendering_RenderingProblemReport           */

extern void nglib_print_rendering_problem_report(NgRendering_RenderingProblemReport* p_report, FILE* p_file);


/* ********************************************************* */
/*  returns count of how many exceptions occurred (as seen by 
    nglib_show_and_clear_if_exception) */

extern int nglib_exception_count();


/* ********************************************************* */
/* calls the function on args no sooner than i_num_seconds + i_num_milliseconds 
from now */
extern void do_seconds_msecs_from_now(int i_num_seconds, int i_num_milliseconds,
			 void (*function_to_call)(void* p_function_args),
			 void* p_function_args);
			 
/* ********************************************************* */
/* calls the function on args no sooner than i_num_seconds from now */

extern void do_seconds_from_now(int i_num_seconds,
			 void (*function_to_call)(void* p_function_args),
			 void* p_function_args);


#ifdef __cplusplus
}
#endif

#endif /* ifndef __nglib_h_ */

/* ********************************************************* */
/* END                                                       */
/* ********************************************************* */
