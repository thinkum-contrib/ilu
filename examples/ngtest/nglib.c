/** $Id: nglib.c,v 1.13 1999/08/03 01:58:13 janssen Exp $
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
/* Last edited by Mike Spreitzer October 9, 1998 2:00 pm PDT */

#include "nglib.h"

#ifdef __osf__
/* hack for nanosleep */
#define nanosleep(x,y) nanosleep_d9(x,y)
#endif

/* iluntrnl and protocol for use in get_object_serializer */
#include <iluntrnl.h>
#include <iluprotocol.h>

/* ********************************************************* */
/* globals */

/* The serializer_hash_table is used to keep
   track of existing serializers and how many objects we have 
   in the server the serializer is associated with */
static ilu_HashTable g_serializer_hash_table = NULL;
static ilu_Mutex g_serializer_hash_table_mutex = NULL;

/* used to ensure exclusive access to directory functions used as
part of validating paths */
static ilu_Mutex g_chdir_mutex = NULL;

/* count of how many exceptions occurred (as seen by nglib_show_and_clear_if_exception) */
static int g_i_exception_count = 0;

/* ********************************************************* */
/* initialization and shutdown - init needs to be called if
serializers are going to be used
*/

void nglib_init() {
	static int i_initted = 0;
	if (i_initted) return;
	
	g_serializer_hash_table_mutex = ilu_CreateMutex("nglib", "serializer_hash_table");
	g_serializer_hash_table = ilu_hash_MakeNewTable(251, ilu_hash_HashPointer, ilu_hash_PointerCompare);
	g_chdir_mutex = ilu_CreateMutex("nglib", "chdir");
	i_initted = 1;
}


void nglib_shutdown() {
	ilu_Error an_error;

	if (g_serializer_hash_table_mutex) {
		ilu_DestroyMutex(g_serializer_hash_table_mutex, &an_error);
		if (ILU_ERRNOK(an_error)) {
			fprintf(stderr, "Error destroying g_serializer_hash_table_mutex mutex\n");
			ILU_HANDLED(an_error);
			ILU_CLER(an_error);
		}
	}
	
	if (g_serializer_hash_table) 
		ilu_hash_FreeHashTable(g_serializer_hash_table, NULL, free);

	if (g_chdir_mutex) {
		ilu_DestroyMutex(g_chdir_mutex, &an_error);
		if (ILU_ERRNOK(an_error)) {
			fprintf(stderr, "Error destroying g_chdir_mutex mutex\n");
			ILU_HANDLED(an_error);
			ILU_CLER(an_error);
		}
	}
}


/* ********************************************************* */
/* ********************************************************* */
/* Serializer and related utility functions                  */
/* ********************************************************* */

/* structures placed in the serializer_hash_table, used to keep
   track of existing serializers and how many objects we have 
   in the server the serializer is associated with */

typedef struct nglib_serializer_refcount_pair_s {
	ILU_C_Serializer m_serializer;
	ILU_C_Server m_cserver;
	int m_i_count;
} nglib_serializer_refcount_pair;


/* utility function to set the pointed to ilu_FineTime to a time i_secs + i_msecs in the future */
static void set_finetime_from_now (ilu_FineTime* p_finetime, ilu_integer i_secs, ilu_cardinal i_msecs) {

  *p_finetime = ilu_FineTime_Now();		/* set to current time */

  p_finetime->ft_s = p_finetime->ft_s + i_secs;	    /* add seconds from now */

  if (i_msecs + p_finetime->ft_t > ilu_FineTimeRate) { /* if overflow on msec */
    (p_finetime->ft_s)++;
    p_finetime->ft_t = i_msecs + p_finetime->ft_t - ilu_FineTimeRate;
  }
  else p_finetime->ft_t = p_finetime->ft_t + i_msecs;	/* add milliseconds from now*/
}


/* used to pass and alarm and the function to run and its args to the 
alarm worker */

typedef struct _do_seconds_from_now_args_s {
  void* m_ilu_alarm;
  void (*m_function_to_call)(void* p_function_args);
  void* m_p_function_args;
} * p_do_seconds_from_now_args;


/* function ilu calls when the alarm goes off - it calls the actual function,
then destoys the alarm */
static void do_seconds_from_now_worker(void* pv_from_now_args) {

  p_do_seconds_from_now_args p_seconds_from_now_args = (p_do_seconds_from_now_args) pv_from_now_args;

  (p_seconds_from_now_args->m_function_to_call)(p_seconds_from_now_args->m_p_function_args);

  ilu_DestroyAlarm(p_seconds_from_now_args->m_ilu_alarm);
}


/* calls the function on args no sooner than i_num_seconds + i_num_milliseconds from now */
void do_seconds_msecs_from_now(int i_num_seconds, int i_num_milliseconds,
			 void (*function_to_call)(void* p_function_args),
			 void* p_function_args){

  void* the_ilu_alarm = ilu_CreateAlarm();
  ilu_FineTime run_time;

  p_do_seconds_from_now_args p_from_now_args = (p_do_seconds_from_now_args) malloc(sizeof(struct _do_seconds_from_now_args_s));
  p_from_now_args->m_ilu_alarm = the_ilu_alarm;
  p_from_now_args->m_function_to_call = function_to_call;
  p_from_now_args->m_p_function_args = p_function_args;

  set_finetime_from_now(&run_time, i_num_seconds, i_num_milliseconds);

  ilu_SetAlarm(the_ilu_alarm, run_time, do_seconds_from_now_worker, p_from_now_args);
}


/* calls the function on args no sooner than i_num_seconds from now */
void do_seconds_from_now(int i_num_seconds,
			 void (*function_to_call)(void* p_function_args),
			 void* p_function_args){

	do_seconds_msecs_from_now(i_num_seconds, 0, function_to_call, p_function_args);

}


/* used as the function to be called by an alarm - release the C server */

static void do_server_release(void* p_the_c_server) {
  CORBA_Environment ilu_env;
  ILU_C_Server_release((ILU_C_Server)p_the_c_server, &ilu_env);
  nglib_show_and_clear_if_exception(&ilu_env, "ILU_C_Server_release problem", stderr);
}


/* return the Serializer that should be used for calls on the 
given object.  It creates a serializer if need be, and increments
the number of objects using this serializer. */

ILU_C_Serializer nglib_get_object_serializer (ILU_C_OBJECT object) {
	
	nglib_serializer_refcount_pair* p_pair;
	CORBA_Environment ilu_env;					/* used to get potential error info */
	ILU_C_Serializer serializer_to_return = ILU_NIL;
	ilu_boolean b_can_serialize = ilu_FALSE;


	/* first make sure that the object is a surrogate and that it's
	   to be contacted with a non-concurrent protocol */
	ilu_EnterServer(object->server->ilucs_ks, object->iluco_ko->ob_class);
	if ((!object->server->ilucs_ks->sr_true) &&
		(!object->server->ilucs_ks->sr_protocol->pr_concurrent_requests))
		b_can_serialize = ilu_TRUE;
	ilu_ExitServer(object->server->ilucs_ks, object->iluco_ko->ob_class);

	if (!b_can_serialize) 
		return NULL;

	ilu_AcquireMutex(g_serializer_hash_table_mutex);

	/* see if we already have a serializer for the object's server */
	p_pair = ilu_hash_FindInTable(g_serializer_hash_table, object->server);
	
	if (p_pair) { 
		/* we do, so just bump up it's usage count and return the serializer */
		p_pair->m_i_count++;
		ilu_ReleaseMutex(g_serializer_hash_table_mutex);
		serializer_to_return = p_pair->m_serializer;
		goto returnit;
	}
	
	/* otherwise we need a new one */
	p_pair = malloc(sizeof(nglib_serializer_refcount_pair));
	p_pair->m_i_count = 1;
	p_pair->m_cserver = object->server;
	p_pair->m_serializer = ILU_C_CreateSerializationContext(object->server, &ilu_env);
	nglib_show_and_clear_if_exception(&ilu_env, "ILU_C_CreateSerializationContext problem", stderr);
	
	if (!p_pair->m_serializer) {
		/* had a problem creating the serializer */
		ilu_ReleaseMutex(g_serializer_hash_table_mutex);
		free(p_pair);
		serializer_to_return =  ILU_NIL;
		goto returnit;
	}

	/* now add the new pairing to the hash table */
	ilu_hash_AddToTable(g_serializer_hash_table, object->server, p_pair);

	/* When using serializers, the intent is to get all the requests down
	   one connection.  But, if we release all the objects in the ILU_C_Server
	   then the ILU_C_Server will destruct, destroying its connection. This would cause
	   the next request for an object on the same origin server to need to create a new
	   ILU_C_Server and a new connection. Under the expectation of temporal locality
	   of reference, we'll increment the ILU_C_Server's refcount here to ensure it's kept
	   in memory, and when the serializer is released, we'll schedule a deferred release.
	   That way, if a request needs to get issued before the deferred relase has
	   taken place, the server will already be there. */
	ILU_C_Server_duplicate(p_pair->m_cserver, &ilu_env);
	nglib_show_and_clear_if_exception(&ilu_env, 
				    "ILU_C_CreateSerializationContext: ILU_C_Server_duplicate problem", stderr);


	ilu_ReleaseMutex(g_serializer_hash_table_mutex);

	serializer_to_return =  p_pair->m_serializer;

returnit: /* note using goto label only to allow next debug line */
	/* fprintf(stderr, "\nGET_OBJECT_SERIALIZER returning %p\n", serializer_to_return); */
	return serializer_to_return;
}


/* Called to release the given object's serializer. It decrements the number of objects 
having gotten the  object's server's serializer and also destroys the serializer if there are 
that reaches zero - hence the number of calls to  release_object_serializer should 
equal the number of calls to get_object_serializer */

void nglib_release_object_serializer (ILU_C_OBJECT object) {
	
	nglib_serializer_refcount_pair* p_pair;
	ILU_C_Server c_server;
	CORBA_Environment ilu_env;					/* used to get potential error info */
	
	ilu_AcquireMutex(g_serializer_hash_table_mutex);
	
	/* see if we already have a serializer for the object's server */
	p_pair = ilu_hash_FindInTable(g_serializer_hash_table, object->server);
	
	if (! p_pair) { /* just ignore if we didn't have one */
		ilu_ReleaseMutex(g_serializer_hash_table_mutex);
		return;
	}
	
	/* decrement it's usage count */
	p_pair->m_i_count--;
	
	if (p_pair->m_i_count > 0) {/* still some possible use, just return */
		ilu_ReleaseMutex(g_serializer_hash_table_mutex);
		return;
	}
	
	/* fprintf(stderr, "\nRELEASE_OBJECT_SERIALIZER on %p\n", p_pair->m_serializer); */
	c_server = p_pair->m_cserver;
	ILU_C_ReleaseSerializer(p_pair->m_serializer, &ilu_env);
	nglib_show_and_clear_if_exception(&ilu_env, "ILU_C_ReleaseSerializer problem", stderr);
	ilu_hash_RemoveFromTable(g_serializer_hash_table, object->server);
	free(p_pair);

	/* arrange the release on the C server to take place later (betting on temporal locality) */
	do_seconds_from_now(5, do_server_release, c_server);

	ilu_ReleaseMutex(g_serializer_hash_table_mutex);

}



/* nglib_rejuvenate_objects_server can be used to boost an object's
server reference count for a short period of time, to try to take advantage
of temporal locality.  When a server becomes empty of objects, it and
all it's connections get garbage collected -- the intent of this function
to keep the server 'in-use' for a short period of time (currently 5 seconds)
after becoming empty, on the bet that we'll soon be using another object in 
that server and would like to have connections still set up and ready to be used. */
void nglib_rejuvenate_objects_server(ILU_C_OBJECT object) {
	CORBA_Environment ilu_env;					/* used to get potential error info */
	
	/* bump the reference count on the object's server */
	ILU_C_Server_duplicate(object->server, &ilu_env);
	if (!nglib_show_and_clear_if_exception(&ilu_env, 
				    "nglib_rejuvenate_objects_server: ILU_C_Server_duplicate problem", stderr))
					
					/* arrange the release on the C server to take place later 
					   (betting on temporal locality) */
					do_seconds_from_now(5, do_server_release, object->server);
}



/* ********************************************************* */
/* General Utility Functions                                 */
/* ********************************************************* */

#if (!(defined WIN32 || defined WIN16))

/* ********************************************************* */
/* sleeps for the specified number of milliseconds           */
/* xxx would really like this to sleep just the calling thread
   not the whole process, and in between this functions calls
   to usleep, other threads in the process may get to do something 
*/

void nglib_microsleep (int i_microseconds) {

  struct timespec sleeptime;
  struct timespec remainingtime;
  int i_sleep_result;

  sleeptime.tv_sec =  i_microseconds / 1000000;
  sleeptime.tv_nsec = (i_microseconds % 1000000) * 1000;

  while (( i_sleep_result = nanosleep(&sleeptime, &remainingtime)) != 0) {
    if (i_sleep_result == ENOSYS) {
      fprintf(stderr, "This implementation does not support nanosleep()\n");
      return;
    }
    else {
      sleeptime.tv_sec = remainingtime.tv_sec; 
      sleeptime.tv_nsec = remainingtime.tv_nsec;
    }
  }
}

#endif /* (!(defined WIN32 || defined WIN16))  */

/* ********************************************************* */
/* show a CORBA_Environment exception if we have one         */

ilu_boolean nglib_show_and_clear_if_exception (CORBA_Environment* p_ilu_env, 
											   char* pc_situation, FILE* p_file) {
	
	if (p_ilu_env->returnCode == ILU_NIL) 
		return ilu_FALSE;		/* just return false on no exception set */
	
	if (p_file) {
		
		/* show exception if we got one */
		fprintf (p_file, "\nException: %s\n", pc_situation ? pc_situation : "");
		
		switch (p_ilu_env->_major) {
			
		case CORBA_NO_EXCEPTION:		
			fprintf (p_file, "\t_major = CORBA_NO_EXCEPTION\n"); 
			break;
			
		case CORBA_USER_EXCEPTION:		
			fprintf (p_file, "\t_major = CORBA_USER_EXCEPTION\n"); 
			break;
			
		case CORBA_SYSTEM_EXCEPTION:	
			fprintf (p_file, "\t_major = CORBA_SYSTEM_EXCEPTION\n"); 
			break;
			
		default:	
			fprintf (p_file, "\t_major = ??? Unknown _major !! ???\n"); 
			break;
		}
		
		fprintf (p_file, "\treturnCode = %s\n", p_ilu_env->returnCode);
	}
	/* clean things up */
	if (p_ilu_env->freeRoutine && p_ilu_env->ptr)
		(p_ilu_env->freeRoutine)(p_ilu_env->ptr);
	p_ilu_env->_major = CORBA_NO_EXCEPTION;
	p_ilu_env->returnCode = ILU_NIL;
	p_ilu_env->ptr = NULL;

	/* increment our count of exceptions */
	g_i_exception_count++;
	
	return ilu_TRUE;
}


/* ********************************************************* */
/* creates and returns a duplicate of the NULL-terminated
string parameter                                          */

char* nglib_duplicate_c_string (char *input_string)
{
	char *output_string;
	if (input_string == NULL)
		return NULL;
	else
    {
		output_string = (char *)malloc(strlen(input_string) + 1);
		if (output_string != NULL)
			strcpy(output_string, input_string);
		return output_string;
    }
}


/* ********************************************************* */
/* file extension to content-type mapping                    */
/* ********************************************************* */

typedef struct {		/* pairs a file name extension with its MIME type */
	char* pc_file_extension;
	char* pc_content_type;
}	extension_type_pair;


/* brute force representation of all the file extension to 
  MIME types we know about, arranged, initially, in rough order of 
  commonality */

static extension_type_pair g_extension_type_pairs[] =  


{
	{	"html",			"text/html"							},
	{	"htm",			"text/html"							},
	{	"txt",			"text/plain"						},
	{	"text",			"text/plain"						},

	{	"gif",			"image/gif"							},
	{	"jpeg",			"image/jpeg"						},
	{	"jpg",			"image/jpeg"						},
	{	"jpe",			"image/jpeg"						},

	{	"ps",			"application/postscript"			},
	{	"ppt",			"application/powerpoint"			},
	{	"doc",			"application/msword"				},
	{	"pdf",			"application/pdf"					},
	{	"exe",			"application/octet-stream"			},
	{	"bin",			"application/octet-stream"			},
	{	"class",		"application/octet-stream"			},

	{	"zip",			"application/zip"					},
	{	"gz",			"application/x-gzip"				},
	{	"Z",			"application/x-compress"			},
	{	"js",			"application/x-javascript"			},
	{	"ls",			"application/x-javascript"			},
	{	"mocha",		"application/x-javascript"			},

	{	"wav",			"audio/x-wav"						},
	{	"au",			"audio/basic"						},
	{	"snd",			"audio/basic"						},
	{	"mp2",			"audio/mpeg"						},

	{	"mpeg",			"video/mpeg"						},
	{	"mpg",			"video/mpeg"						},
	{	"mpe",			"video/mpeg"						},
	{	"qt",			"video/quicktime"					},
	{	"mov",			"video/quicktime"					},

 /* end of initally commonality ordering guesses */

	{	"rtx",			"text/richtext"						},
	{	"tsv",			"text/tab-separated-values"			},
	{	"etx",			"text/x-setext"						},
	{	"sgml",			"text/x-sgml"						},
	{	"sgm",			"text/x-sgml"						},

	{	"tiff",			"image/tiff"						},
	{	"tif",			"image/tiff"						},
	{	"bmp",			"image/x-MS-bmp"					},
	{	"rgb",			"image/x-rgb"						},
	{	"ppm",			"image/x-portable-pixmap"			},
	{	"pgm",			"image/x-portable-graymap"			},
	{	"pbm",			"image/x-portable-bitmap"			},
	{	"pnm",			"image/x-portable-anymap"			},
	{	"xwd",			"image/x-xwindowdump"				},
	{	"xpm",			"image/x-xpixmap"					},
	{	"xbm",			"image/x-xbitmap"					},
	{	"ras",			"image/x-cmu-raster"				},
	{	"ief",			"image/ief"							},
	
	{	"aif",			"audio/x-aiff"						},
	{	"aiff",			"audio/x-aiff"						},
	{	"aifc",			"audio/x-aiff"						},
	{	"mid",			"audio/midi"						},
	{	"midi",			"audio/midi"						},
	{	"kar",			"audio/midi"						},
	{	"mpga",			"audio/mpeg"						},
	{	"ram",			"audio/x-pn-realaudio"				},
	{	"rpm",			"audio/x-pn-realaudio-plugin"		},
	{	"ra",			"audio/x-realaudio"					},

	
	{	"eps",			"application/postscript"			},
	{	"ai",			"application/postscript"			},
	{	"lzh",			"application/octet-stream"			},
	{	"lha",			"application/octet-stream"			},
	{	"dms",			"application/octet-stream"			},
	{	"pac",			"application/x-ns-proxy-autoconfig"	},
	{	"tcl",			"application/x-tcl"					},
	{	"sh",			"application/x-sh"					},
	{	"csh",			"application/x-csh"					},
	{	"cpio",			"application/x-cpio"				},
	{	"gtar",			"application/x-gtar"				},
	{	"tar",			"application/x-tar"					},
	{	"shar",			"application/x-shar"				},
	{	"sit",			"application/x-stuffit"				},
	{	"hqx",			"application/mac-binhex40"			},
	{	"fif",			"application/fractals"				},
	{	"texi",			"application/x-texinfo"				},
	{	"texinfo",		"application/x-texinfo"				},
	{	"dvi",			"application/x-dvi"					},
	{	"latex",		"application/x-latex"				},
	{	"tex",			"application/x-tex"					},
	{	"rtf",			"application/rtf"					},
	{	"cpt",			"application/mac-compactpro"		},
	{	"oda",			"application/oda"					},
	{	"bcpio",		"application/x-bcpio"				},
	{	"vcd",			"application/x-cdlink"				},
	{	"csh",			"application/x-csh"					},
	{	"dcr",			"application/x-director"			},
	{	"dir",			"application/x-director"			},
	{	"dxr",			"application/x-director"			},
	{	"hdf",			"application/x-hdf"					},
	{	"skp",			"application/x-koan"				},
	{	"skd",			"application/x-koan"				},
	{	"skt",			"application/x-koan"				},
	{	"skm",			"application/x-koan"				},
	{	"mif",			"application/x-mif"					},
	{	"nc",			"application/x-netcdf"				},
	{	"cdf",			"application/x-netcdf"				},
	{	"t",			"application/x-troff"				},
	{	"tr",			"application/x-troff"				},
	{	"roff",			"application/x-troff"				},
	{	"man",			"application/x-troff-man"			},
	{	"me",			"application/x-troff-me"			},
	{	"ms",			"application/x-troff-ms"			},
	{	"ustar",		"application/x-ustar"				},
	{	"src",			"application/x-wais-source"			},

	{	"avi",			"video/x-msvideo"					},
	{	"movie",		"video/x-sgi-movie"					},

	{	"ice",			"x-conference/x-cooltalk"			},

	{	"vrml",			"x-world/x-vrml"					},
	{	"wrl",			"x-world/x-vrml"					},
	
	{	NULL,			NULL								}
};



/* ********************************************************* */
/* returns the appropriate Content-Type string for the given 
file extension, xxx does this by brute force linear search   */

char* nglib_content_type_from_extension (char* pc_extension) {
	
	extension_type_pair* p_pair;
	
	p_pair = g_extension_type_pairs;
	
	/* search all our pairs looking for a match */
	while (p_pair->pc_file_extension) {
		if (strcmp(pc_extension, p_pair->pc_file_extension) == 0)
			return p_pair->pc_content_type;
		else p_pair++;
	}
	
	return NULL;
}


/* ********************************************************* */
/* Return a pointer to the start of the file extension in pc_path,
   or NULL if none found, If i_length is positive, it should indicate
   the number of characters in pc_path */
char* nglib_extension_in_pathname(char* pc_path, int i_length) {
	char* pc_walker;
	if (!pc_path) 
		return NULL;
	/* set pc_walker to point to last char */
	pc_walker = pc_path + (i_length <= 0 ? strlen(pc_path) : i_length) - 1;
	while (pc_walker >= pc_path ) {
		if (*pc_walker == '.')
			return pc_walker + 1;
		if (*pc_walker == '/') /* hit a directory separator */
			return NULL;
		pc_walker--;
	}
	return NULL;
}



/* ********************************************************* */
/* path validation                                           */
/* ********************************************************* */

/*
Returns ilu_TRUE if canonicalized pc_pathname (sans the drive letter 
on windows) begins with pc_prefix 
Assumes that pc_pathname has at least one directory separator in it. 
Trys to rely on getcwd to provide pathname canonicalization */

ilu_boolean nglib_validate_path (char* pc_pathname, char* pc_prefix) {
	
	char c_working_dir_save[2048];
	char c_new_working_dir[2048];
	char* pc_pathname_copy = nglib_duplicate_c_string(pc_pathname);
	char* pc_copy_walker;
	ilu_boolean b_result = ilu_TRUE;
	
	/* null terminate our copy at the directory point */
	pc_copy_walker = pc_pathname_copy + strlen(pc_pathname_copy) - 1;
	while (pc_copy_walker > pc_pathname_copy && 
		*pc_copy_walker != '/'
#if (defined (WIN32) || defined(WIN16))
		&& *pc_copy_walker != '\\'
#endif
		) {
		pc_copy_walker--;
	}
	*pc_copy_walker = '\0';


	ilu_AcquireMutex(g_chdir_mutex);

	GETCWD_FUNTION(c_working_dir_save, 2048); /* save where we currently are */
	CHDIR_FUNTION(pc_pathname_copy);		 /* go to the new location */
	GETCWD_FUNTION(c_new_working_dir, 2048); /* get where we are now */
	CHDIR_FUNTION(c_working_dir_save); /* restore to where we were originally */

	ilu_ReleaseMutex(g_chdir_mutex);

	
#if (defined (WIN32) || defined(WIN16))
	/* ignore drive letter on windows systems */
	if (strstr(c_new_working_dir + 2, pc_prefix) != c_new_working_dir + 2) 
#else
		if (strstr(c_new_working_dir, pc_prefix) != c_new_working_dir) 
#endif
		{
			fprintf (stderr, "Validation failure: directory %s of %s - not rooted at %s\n", 
				c_new_working_dir, pc_pathname, pc_prefix);
			b_result = ilu_FALSE;
		}
		
		free(pc_pathname_copy);
		
		return b_result;
}




/* ********************************************************* */
/* Time related                                              */
/* ********************************************************* */

/* ********************************************************* */
/* return a char* containing the UTC time in asctime format */

char* nglib_get_time_string (time_t* p_timer) {
	struct tm* p_newtime;
	char* pc_timestring;
	
	p_newtime = gmtime( p_timer );			/* convert to UTC structure */
	pc_timestring = asctime( p_newtime); /* get char string representation */
	pc_timestring[strlen(pc_timestring) - 1] = '\0';	/* knock off \n */
	return nglib_duplicate_c_string(pc_timestring);
}


/* ********************************************************* */
/* return a char* containing the current UTC time in asctime format */

char* nglib_get_current_time_string () {
	time_t ltime;
	time( &ltime );						/* get the time */
	return nglib_get_time_string( &ltime );	/* convert to UTC string */
}


/* ********************************************************* */
/* NG structure related                                      */
/* ********************************************************* */

/* ************************************************************ */
/* cleanly initialize a NgRendering_RenderingPreferences struct */

NgRendering_RenderingPreferences*  
nglib_NgRendering_RenderingPreferences_clean_assign(NgRendering_RenderingPreferences* p_preferences) {
	
	p_preferences->allowContentTypes._maximum = 0;
	p_preferences->allowContentTypes._length = 0;
	p_preferences->allowContentTypes._buffer = NULL;
	
	p_preferences->disallowContentTypes._maximum = 0;
	p_preferences->disallowContentTypes._length = 0;
	p_preferences->disallowContentTypes._buffer = NULL;
	
	p_preferences->allowEncodings._maximum = 0;
	p_preferences->allowEncodings._length = 0;
	p_preferences->allowEncodings._buffer = NULL;
	
	p_preferences->disallowEncodings._maximum = 0;
	p_preferences->disallowEncodings._length = 0;
	p_preferences->disallowEncodings._buffer = NULL;
	
	p_preferences->acceptCharsets._maximum = 0;
	p_preferences->acceptCharsets._length = 0;
	p_preferences->acceptCharsets._buffer = NULL;
	
	p_preferences->acceptLocales._maximum = 0;
	p_preferences->acceptLocales._length = 0;
	p_preferences->acceptLocales._buffer = NULL;
	
	p_preferences->range = NULL;
	
	p_preferences->userAgent = NULL;

	return p_preferences;
}

/* ********************************************************* */
/* return a pointer to a cleanly initialized 
   NgRendering_RenderingPreferences struct */

NgRendering_RenderingPreferences*  
nglib_NgRendering_RenderingPreferences_clean_allocate() {

	NgRendering_RenderingPreferences* p_preferences;
	
	p_preferences = (NgRendering_RenderingPreferences*)
		ilu_malloc(sizeof(NgRendering_RenderingPreferences));
	return nglib_NgRendering_RenderingPreferences_clean_assign(p_preferences);
}


/* ********************************************************* */
/* makes the NgRendering_Rendering struct cleanly initialized */

NgRendering_Rendering* 
nglib_NgRendering_Rendering_clean_assign(NgRendering_Rendering* p_rendering) {

	p_rendering->contentType = NULL;
	p_rendering->contentEncoding._maximum = 0;
	p_rendering->contentEncoding._length = 0;
	p_rendering->contentEncoding._buffer = NULL;
	p_rendering->contentRange = NULL;
	p_rendering->rangeEncoded = ilu_FALSE;
	p_rendering->contentCharSet = IANA_Charsets_Registry_US_ASCII;
	p_rendering->contentLocale = NULL;

	p_rendering->renderingBytes._maximum = 0;
	p_rendering->renderingBytes._length = 0;
	p_rendering->renderingBytes._buffer = NULL;

	return p_rendering;
}

/* ********************************************************* */
/* return a pointer to a cleanly initialized 
   NgRendering_Rendering struct */

NgRendering_Rendering* 
nglib_NgRendering_Rendering_clean_allocate() {

	NgRendering_Rendering* p_rendering;
	
	p_rendering = (NgRendering_Rendering*)
		ilu_malloc(sizeof(NgRendering_Rendering));

	nglib_NgRendering_Rendering_clean_assign(p_rendering);

	return p_rendering;
}

/* ********************************************************* */
/* NG exception related                                      */
/* ********************************************************* */

/* ********************************************************* */
/* sets the ilu_env to an _NgBasic__Exception_ObjectNotExist exception with
   the specified reason and specifics info. pc_reason owned by caller */
void nglib_assign_ObjectNotExist(ILU_C_ENVIRONMENT* ilu_env, char* pc_reason, 
						   NgBasic_OptionalPickle specifics_pickle) {
	
	NgBasic_ExceptionInformation* p_exception_info;
	p_exception_info = (NgBasic_ExceptionInformation*) ilu_malloc(sizeof(NgBasic_ExceptionInformation));
	
	p_exception_info->reasonPhrase = nglib_duplicate_c_string(pc_reason);
	p_exception_info->specificsData = specifics_pickle;
	
    ilu_env->_major = ILU_C_USER_EXCEPTION;
    ilu_env->returnCode = _NgBasic__Exception_ObjectNotExist;
    ilu_env->ptr = (void*) p_exception_info;
		
	return;
}


/* sets the ilu_env to an _NgRendering__Exception_NoRenderingMatch exception with
   the specified preferences */
void nglib_assign_NoRenderingMatch(ILU_C_ENVIRONMENT* ilu_env, 
						   NgRendering_RenderingPreferences* p_preferences) {

    ilu_env->_major = ILU_C_USER_EXCEPTION;
    ilu_env->returnCode = _NgRendering__Exception_NoRenderingMatch;
    ilu_env->ptr = (void*) p_preferences;
		
	return;
}

/* ********************************************************* */
/* NG utility                                                */
/* ********************************************************* */


/* returns TRUE if the rendering types match - 
xxx very simple and assuming implementation
(assumes rendering type is of form type/subtype )   */
ilu_boolean nglib_rendering_types_match (NgRendering_RenderingType pc_stringA, 
								   NgRendering_RenderingType pc_stringB) {
	
	register char* pc_subtypeA;
	register char* pc_subtypeB;
	
	/* identity and null checks */
	if (pc_stringA == pc_stringB)
		return ilu_TRUE;
	
	if (!pc_stringA || !pc_stringB)
		return ilu_FALSE;
	
	/* get pointers to subtypes */
	pc_subtypeA = pc_stringA;
	while (pc_subtypeA && *pc_subtypeA != '/') pc_subtypeA++;
	pc_subtypeA++;
	
	pc_subtypeB = pc_stringB;
	while (pc_subtypeB && *pc_subtypeB != '/') pc_subtypeB++;
	pc_subtypeB++;
	
	/* check for type wildcard */
	if (*pc_stringA != '*' && *pc_stringB != '*') {
		/* neither one is a wildcard */
		
		if ((pc_subtypeA - pc_stringA) != (pc_subtypeB - pc_stringB))
			/* different type lengths must be mismatch */
			return ilu_FALSE;
		
		if (strncmp(pc_stringA, pc_stringB, pc_subtypeB - pc_stringB) != 0)
			/* type mismatch */
			return ilu_FALSE;
	}
	
	/* check subtypes */
	if (*pc_subtypeA != '*' && *pc_subtypeB != '*') {
		/* neither one is a wildcard */
		
		if (strcmp(pc_subtypeA, pc_subtypeB) != 0)
			/* subtype mismatch */
			return ilu_FALSE;
	}
	
	return ilu_TRUE;
}



/* returns TRUE if pc_string has a match in the sequence, else FALSE */

ilu_boolean nglib_is_rendering_type_member (NgRendering_RenderingTypeSequence* p_sequence, 
									  NgRendering_RenderingType pc_string) {
	register unsigned int ui_index;

	for (ui_index = 0; ui_index < p_sequence->_length; ui_index++) 
			if (nglib_rendering_types_match(pc_string, p_sequence->_buffer[ui_index]))
				return ilu_TRUE;
	return ilu_FALSE;
}



/* returns TRUE if i_value has a match in the sequence, else FALSE */
ilu_boolean nglib_is_IANA_Charsets_Registry_CharsetMIBEnumValueSequence_member 
(IANA_Charsets_Registry_CharsetMIBEnumValueSequence* p_sequence, 
 IANA_Charsets_Registry_CharsetMIBEnumValue i_value) {
	register unsigned int ui_index;
	
	for (ui_index = 0; ui_index < p_sequence->_length; ui_index++) 
		if (i_value == p_sequence->_buffer[ui_index])
			return ilu_TRUE;
		return ilu_FALSE;
}


/* ********************************************************* */
/* printout a sequence of strings from a buffer              */

void nglib_print_string_sequence (unsigned long i_length, char** ppc_strings, FILE* p_file) {

	unsigned long ul_index;				/* used as index  */

	if (i_length == 0)
		fprintf(p_file, " (empty)");
	else 
		for (ul_index = 0; ul_index < i_length; ul_index++) {
			fprintf(p_file, " %s", ppc_strings[ul_index]);
	}
	fprintf(p_file, "\n"); 
}

/* ********************************************************* */
/* NG String related                                         */
/* ********************************************************* */

/* returns TRUE if i_value has a match in the sequence, else FALSE */
ilu_boolean nglib_is_NgBasic_StringSequence_member  
(NgBasic_StringSequence* p_sequence, NgBasic_String pc_string) {
	register unsigned int ui_index;
	
	for (ui_index = 0; ui_index < p_sequence->_length; ui_index++) 
		if (strcmp(pc_string, p_sequence->_buffer[ui_index]) == 0)
			return ilu_TRUE;
		return ilu_FALSE;
}

/* ********************************************************* */
/* Basic Exception related                                   */
/* ********************************************************* */

void nglib_print_basic_exception_info(NgBasic_ExceptionInformation* p_exception_info, FILE* p_file) {

	fprintf(p_file, "NgBasic_ExceptionInformation:\n");
	fprintf(p_file, "Reason:\n", p_exception_info->reasonPhrase  ? p_exception_info->reasonPhrase : "NULL");
	fprintf(p_file, "Specifics:\n", p_exception_info->specificsData  ? "Present" : "Not Present");
}


/* ********************************************************* */
/* ILU HTTP related                                          */
/* ********************************************************* */

/* ********************************************************* */
/* printout an iluhttp request                               */

void nglib_print_iluhttp_request (iluhttp_Request* p_the_request, FILE* p_file) {
	
	iluhttp_Header* p_the_header;			/* used to point to headers */
	unsigned long ul_index;				/* used as index into sequences */
	
	fprintf(p_file, "iluhttp Get Request: (Note all values are shown between >< s)\n");
	
	if (p_the_request == ILU_NIL) {
		fprintf(p_file, "NIL\n");
		return;
	}
	
	fprintf(p_file, "URI = >%s<\n", p_the_request->URI);	/* show URI */ 
	
	p_the_header = p_the_request->headers._buffer; /* show the headers */
	fprintf(p_file, "Number of headers = >%lu<\n", p_the_request->headers._length);
	for (ul_index = 0; 
	ul_index < p_the_request->headers._length; 
	ul_index++, p_the_header++) {
		
		fprintf(p_file, "Header %lu\n\tfield-name = >%s<", ul_index, p_the_header->name);
		fprintf(p_file, "\n\toptional-field-value = >%s<\n", (p_the_header->value ? p_the_header->value : "NIL"));
	}
	
	/* show the entity body - assume it's all printable */
	fprintf(p_file, "Body is:\n>");
	if (p_the_request->body == ILU_NIL)
		fprintf(p_file, "NIL");
	else 
		for (ul_index = 0; ul_index < p_the_request->body->_length; ul_index++)
			fprintf(p_file, "%hc", p_the_request->body->_buffer[ul_index]);
		fprintf(p_file, "<\n");
}


/* ********************************************************* */
/* printout an iluhttp response                              */

void nglib_print_iluhttp_response (iluhttp_Response* p_the_response, int i_show_body_too, FILE* p_file) {
	
	iluhttp_Header* p_the_header;			/* used to point to headers */
	unsigned long ul_index;				/* used as index into sequences */
	
	fprintf(p_file, "iluhttp Get Response: (Note all values are shown between >< s)\n");
	
	if (p_the_response == ILU_NIL) {
		fprintf(p_file, "NIL\n");
		return;
	}
	
	fprintf(p_file, "Status = >%d<\n", p_the_response->status);	/* show status */ 
	
	p_the_header = p_the_response->headers._buffer; /* show the headers */
	fprintf(p_file, "Number of headers = >%lu<\n", p_the_response->headers._length);
	for (ul_index = 0; 
	ul_index < p_the_response->headers._length; 
	ul_index++, p_the_header++) {
		
		fprintf(p_file, "Header %lu\n\tfield-name = >%s<", ul_index, p_the_header->name);
		fprintf(p_file, "\n\toptional-field-value = >%s<\n", (p_the_header->value ? p_the_header->value : "NIL"));
	}
	
	if (i_show_body_too) {
		/* show the entity body - assume it's all printable */
		fprintf(p_file, "Body is:\n>");
		if (p_the_response->body == ILU_NIL)
			fprintf(p_file, "NIL");
		else 
			for (ul_index = 0; ul_index < p_the_response->body->_length; ul_index++)
				fprintf(p_file, "%hc", p_the_response->body->_buffer[ul_index]);
			fprintf(p_file, "<\n");
	}
}



/* ********************************************************* */
/* NgRendering related                                          */
/* ********************************************************* */

/* ********************************************************* */
/* printout an NgRendering Preferences                       */

void nglib_print_ngrendering_preferences(NgRendering_RenderingPreferences* p_preferences, 
								   FILE* p_file) {

	unsigned long ul_index;				/* used as index into sequences */

	fprintf(p_file, "NgRendering_RenderingPreferences:\n");
	
	if (p_preferences == ILU_NIL) {
		fprintf(p_file, "NIL\n");
		return;
	}
	
	fprintf(p_file, "allowContentTypes =");
	nglib_print_string_sequence(p_preferences->allowContentTypes._length,
		p_preferences->allowContentTypes._buffer, p_file);

	fprintf(p_file, "disallowContentTypes =");
	nglib_print_string_sequence(p_preferences->disallowContentTypes._length,
		p_preferences->disallowContentTypes._buffer, p_file);

	fprintf(p_file, "allowEncodings =");
	nglib_print_string_sequence(p_preferences->allowEncodings._length,
		p_preferences->allowEncodings._buffer, p_file);

	fprintf(p_file, "disallowEncodings =");
	nglib_print_string_sequence(p_preferences->disallowEncodings._length,
		p_preferences->disallowEncodings._buffer, p_file);

	fprintf(p_file, "acceptCharsets =");
	if (p_preferences->acceptCharsets._length == 0)
		fprintf(p_file, " (empty)");
	else 
		for (ul_index = 0; ul_index < p_preferences->acceptCharsets._length; ul_index++) {
			fprintf(p_file, " %hu", p_preferences->acceptCharsets._buffer[ul_index]);
	}
	fprintf(p_file, "\n");

	fprintf(p_file, "acceptLocales =");
	nglib_print_string_sequence(p_preferences->acceptLocales._length,
		p_preferences->acceptLocales._buffer, p_file);

	fprintf(p_file, "range : ");
	if  (p_preferences->range)
		fprintf(p_file, "start = %u, end = %u\n", p_preferences->range->startValue, p_preferences->range->endValue);
	else fprintf(p_file, "NULL\n");

	fprintf(p_file, "userAgent = %s\n", (p_preferences->userAgent ? p_preferences->userAgent : "NULL"));

}




/* ********************************************************* */
/* printout an NgRendering Rendering                       */

void nglib_print_ngrendering_rendering(NgRendering_Rendering* p_rendering, 
								  int i_renderingbytes_too, FILE* p_file) {

	unsigned int ul_index;

	fprintf(p_file, "contentType = %s\n", p_rendering->contentType);
	fprintf(p_file, "contentEncoding =");
	nglib_print_string_sequence(p_rendering->contentEncoding._length,
		p_rendering->contentEncoding._buffer, p_file);

	fprintf(p_file, "contentRange : ");
	if  (p_rendering->contentRange)
		fprintf(p_file, "start = %u, end = %u\n", p_rendering->contentRange->startValue, p_rendering->contentRange->endValue);
	else fprintf(p_file, "NULL\n");


	fprintf(p_file, "rangeEncoded = %s\n", p_rendering->rangeEncoded ? "True" : "False");
	fprintf(p_file, "contentCharSet = %hu\n", p_rendering->contentCharSet);
	fprintf(p_file, "contentLocale = %s\n", p_rendering->contentLocale ? p_rendering->contentLocale : "NULL");


	if (i_renderingbytes_too) {
		/* show the renderingbytes - assume it's all printable */
		fprintf(p_file, "renderingBytes are:\n");
		if (!(p_rendering->renderingBytes._buffer))
			fprintf(p_file, "NULL\n");
		else 
			for (ul_index = 0; ul_index < p_rendering->renderingBytes._length; ul_index++)
				fprintf(p_file, "%hc", p_rendering->renderingBytes._buffer[ul_index]);
			fprintf(p_file, "\n");
	}
}

/* ********************************************************* */
/* printout an NgRendering RenderingChunk                    */

void nglib_print_ngrendering_rendering_chunk(NgRendering_RenderingChunk* p_chunk, 
											 int i_renderingbytes_too, FILE* p_file) {
	
	fprintf(p_file, "NgRendering_RenderingChunk:\n");
	
	if (p_chunk->contentRange)
		fprintf(p_file, "Range start %u, Range end %u\n", 
		p_chunk->contentRange->startValue, p_chunk->contentRange->endValue);
	else 
		fprintf(p_file, "Range not present\n");
	
	if (i_renderingbytes_too) {
		unsigned long ul_index;
		/* show the renderingbytes - assume it's all printable */
		fprintf(p_file, "renderingBytes are:\n");
		if (!(p_chunk->renderingBytes._buffer))
			fprintf(p_file, "NULL\n");
		else 
			for (ul_index = 0; ul_index < p_chunk->renderingBytes._length; ul_index++)
				fprintf(p_file, "%hc", p_chunk->renderingBytes._buffer[ul_index]);
			fprintf(p_file, "\n");
	}
}



/* ********************************************************* */
/* prints out a NgRendering_RenderingProblemReport*/

void nglib_print_rendering_problem_report(NgRendering_RenderingProblemReport* p_report, 
										  FILE* p_file) {
	
	fprintf(p_file, "NgRendering_RenderingProblemReport:\n");
	switch (p_report->_d) {
	case 0:
		fprintf(p_file, "No Match\n");
		nglib_print_ngrendering_preferences((NgRendering_RenderingPreferences*)&(p_report->_u), p_file);
		break;
		
	case 1:
		fprintf(p_file, "Would Block\n");
		nglib_print_basic_exception_info((NgBasic_ExceptionInformation*)&(p_report->_u), p_file);
		break;
		
	case 2:
		fprintf(p_file, "Object Not Exist\n");
		nglib_print_basic_exception_info((NgBasic_ExceptionInformation*)&(p_report->_u), p_file);
		break;
		
	default:
		fprintf(p_file, "Unknown discriminator\n");
		break;
		
	}
}


/* ********************************************************* */
/*  returns count of how many exceptions occurred (as seen by 
    nglib_show_and_clear_if_exception) */

int nglib_exception_count() {
  return g_i_exception_count;
}


/* ********************************************************* */
/* END                                                       */
/* ********************************************************* */
