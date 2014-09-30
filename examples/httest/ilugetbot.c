/** $Id: ilugetbot.c,v 1.27 1999/08/03 01:58:25 janssen Exp $
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
/* Last edited by Mike Spreitzer October 8, 1998 11:07 pm PDT */

/* ilu-getbot: ILU URL getter program

 Dan Larner, larner@parc.xerox.com
 10-31-97
 
  
   Notes:  
   
	   In the comments below, the term url is used to generically
	   mean an http style url, or ilu SBH.
	   
		NYI means Not Yet Implemented.
		
*/

/* ********************************************************* */
/* includes                                                  */
/* ********************************************************* */

#include <stdio.h>
#include <math.h>
#include <time.h>

/* pick up gethostname, etc. */
#if (defined WIN32 || defined WIN16)
#include <winsock.h>
#else
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
extern char *duplicate_c_string(char *);
#endif


#include "iluhttp.h"

#ifndef HAVE_GETHOSTNAME_PROTOTYPE
extern int gethostname(char *name, int namelen);
#endif /* ndef HAVE_GETHOSTNAME_PROTOTYPE */


/* so we can change the default http scheme */
ILU_PUBLIC ilu_string ilu_http_default_scheme;



/* ********************************************************* */
/* defines, globals, and typedefs                            */
/* ********************************************************* */

#ifdef ILU_OS_THREADED
/* for argument passing when doing a threaded get */
typedef struct forked_get_args {
	char* m_pc_url;
	int m_i_depth;
} forked_get_args;
#endif /* ILU_OS_THREADED */


/* structures placed in the serializer_hash_table, used to keep
   track of existing serializers and how many objects we have 
   in the server the serializer is associated with */
typedef struct serializer_refcount_pair {
	ILU_C_Serializer m_serializer;
	ILU_C_Server m_cserver;
	int m_i_count;
} serializer_refcount_pair;


/* for statistics gathering */
typedef struct {
	double          minX, maxX, sumX, sumXX;
	unsigned        n;
}            StatSums;


/* the kind of procedure ilu wants for forking */
typedef void (*fork_procedure)(void *arg);


#define MAX_VERBOSITY_LEVEL 4
#define LOG_NOTHING	0
#define LOG_SUMMARY     1
#define LOG_TIMING	2
#define LOG_BASIC	3
#define LOG_FULL	4

/* makes a log entry if appropriate */

#define LOG_ENTER() if (g_logfile_mutex) ilu_AcquireMutex(g_logfile_mutex)
#define LOG_EXIT() if (g_logfile_mutex) ilu_ReleaseMutex(g_logfile_mutex)

#define ACCUM_TIME_ENTER() if (g_timeaccumulated_mutex) ilu_AcquireMutex(g_timeaccumulated_mutex)
#define ACCUM_TIME_EXIT() if (g_timeaccumulated_mutex) ilu_ReleaseMutex(g_timeaccumulated_mutex)

#define DO_LOG_BASIC() g_p_logfile && g_i_verbosity >= LOG_BASIC 	
#define DO_LOG_SUMMARY() g_p_logfile && g_i_verbosity >= LOG_SUMMARY 	
#define DO_LOG_TIMING() g_p_logfile && g_i_verbosity >= LOG_TIMING 	
#define DO_LOG_FULL() g_p_logfile && g_i_verbosity >= LOG_FULL 	

/* how many get threads are available */
int g_i_number_of_get_threads = 0;
int g_i_max_number_of_get_threads = 0; /* 0 means no limit */
ilu_Mutex g_get_thread_count_mutex = NULL;
ilu_Condition g_get_thread_count_condition = NULL;
	
/* verbosity level */
int g_i_verbosity = 0;

/* how deep to go on each retrieval */
int g_i_depth = 0;

/* how many times to get each URL */
int g_i_repeats = 1;

/* whether or not to use ILU pipeline */
int g_b_pipeline = 0;
ILU_C_Pipeline g_pipeline = NULL;

/* whether or not to use ILU batching */
int g_b_batching = 0;
unsigned int g_ui_batching_period;
ILU_C_Batcher g_batcher = NULL;

/* where to write bodies retrieved from Gets */
char* g_pc_body_filename = NULL;
FILE* g_p_body_file = NULL;
ilu_Mutex g_body_file_mutex = NULL;

/* what URL to Get */
char* g_pc_url_to_get = NULL;
char* g_pc_urlfilename = NULL;
FILE* g_p_urlfile = NULL;
ilu_Mutex g_urlfile_mutex = NULL;

/* name of logfile */
char* g_pc_logfilename = NULL;
FILE* g_p_logfile = NULL;
ilu_Mutex g_logfile_mutex = NULL;

/* whether to run ILU multithreaded */
int g_b_multithreaded = 0;		

/* total time for all Get method calls */
StatSums g_get_timesums = {0};
ilu_Mutex g_timeaccumulated_mutex = NULL;
ilu_FineTime g_last_get_end_time; /* set to the end time of the last get that completed */

/* holds the name of this host */
char g_pc_hostname[1024] = "";			


/* whether or not to use ILU serializers */
int g_b_serial = 0;
/* The serializer_hash_table is used to keep
   track of existing serializers and how many objects we have 
   in the server the serializer is associated with */
ilu_HashTable g_serializer_hash_table = NULL;
ilu_Mutex g_serializer_hash_table_mutex = NULL;


/* ********************************************************* */
/* ********************************************************* */
/* Forward Declarations                                      */

int do_get (char* pc_url, iluhttp_Request* p_http_req, int i_repeats, int i_depth);

#ifdef ILU_OS_THREADED
void fork_do_threaded_get(char* pc_url, int i_depth);
#endif /* ILU_OS_THREADED */

ilu_boolean show_and_clear_if_exception (CORBA_Environment* p_ilu_env, char* pc_situation, FILE* p_file);


/* ********************************************************* */
/* ********************************************************* */
/* Statistics Functions  (from ILU's timeit)                 */



static void AddStat(StatSums * ss, ilu_FineTime time_interval) {
	
	double x; /* interval in milliseconds */
	
	x = ((time_interval.ft_s * ((double)ilu_FineTimeRate)) + time_interval.ft_t) * (1000.0 / ilu_FineTimeRate);
	
	if (ss->n == 0)
		ss->minX = ss->maxX = x;
	else {
		if (x > ss->maxX)
			ss->maxX = x;
		if (x < ss->minX)
			ss->minX = x;
	}
	ss->n += 1;
	ss->sumX += x;
	ss->sumXX += x * x;
	return;
}

static char* FmtStats(StatSums * ss) {
	static char     buf[512];
	double          avg, stdev;
	if (ss->n > 0) {
		avg = ss->sumX / ss->n;
		if (ss->n > 1) {
			stdev = (ss->sumXX - ss->sumX * ss->sumX / ss->n) / (ss->n - 1);
			if (stdev >= 0.0) {
				stdev = sqrt(stdev);
				sprintf(buf, "%d Samples\n", ss->n);
				sprintf(buf, "%min %6.1f msec, avg. %6.1f msec, stddev %6.1f , max %6.1f msec",
					ss->minX, avg, stdev, ss->maxX);
			} 
			else
				sprintf(buf, "min %6.1f msec, avg. %6.1f msec, stddev sqrt(%6.1f) , max %6.1f msec",
				ss->minX, avg, stdev, ss->maxX);
		} 
		else
			sprintf(buf, "%6.1f msec once", ss->sumX);
	} 
	else
		sprintf(buf, "no samples");
	return buf;
}



/* ********************************************************* */
/* ********************************************************* */
/* Serializer and related utility functions                  */

/* utility function to sets the pointed to ilu_FineTime to a time i_secs + i_msecs in the future */
void set_finetime_from_now (ilu_FineTime* p_finetime, ilu_integer i_secs, ilu_cardinal i_msecs) {

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



/* function ilu calls when the alarm goes off - it calles the actual function,
then destoys the alarm */
void do_seconds_from_now_worker(void* pv_from_now_args) {

  p_do_seconds_from_now_args p_seconds_from_now_args = (p_do_seconds_from_now_args) pv_from_now_args;

  (p_seconds_from_now_args->m_function_to_call)(p_seconds_from_now_args->m_p_function_args);

  ilu_DestroyAlarm(p_seconds_from_now_args->m_ilu_alarm);
}


/* calls the function on args no sooner than i_num_seconds from now */
void do_seconds_from_now(int i_num_seconds,
			 void (*function_to_call)(void* p_function_args),
			 void* p_function_args){

  void* the_ilu_alarm = ilu_CreateAlarm();
  ilu_FineTime run_time;

  p_do_seconds_from_now_args p_from_now_args = (p_do_seconds_from_now_args) malloc(sizeof(struct _do_seconds_from_now_args_s));
  p_from_now_args->m_ilu_alarm = the_ilu_alarm;
  p_from_now_args->m_function_to_call = function_to_call;
  p_from_now_args->m_p_function_args = p_function_args;

  set_finetime_from_now(&run_time, i_num_seconds, 0);

  ilu_SetAlarm(the_ilu_alarm, run_time, do_seconds_from_now_worker, p_from_now_args);
}



/* used as the function to be called by an alarm - release the C server */

void do_server_release(void* p_the_c_server) {
  CORBA_Environment ilu_env;
  ILU_C_Server_release((ILU_C_Server)p_the_c_server, &ilu_env);
  show_and_clear_if_exception(&ilu_env, "ILU_C_Server_release problem", stderr);
}


/* return the Serializer that should be used for calls on the 
given object.  It creates a serializer if need be, and increments
the number of objects using this serializer. */

ILU_C_Serializer get_object_serializer (ILU_C_OBJECT object) {
	
	serializer_refcount_pair* p_pair;
	CORBA_Environment ilu_env;					/* used to get potential error info */
	ILU_C_Serializer serializer_to_return = ILU_NIL;

	if (g_b_serial)
		ilu_AcquireMutex(g_serializer_hash_table_mutex);
	else 
		return ILU_NIL;

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
	p_pair = malloc(sizeof(serializer_refcount_pair));
	p_pair->m_i_count = 1;
	p_pair->m_cserver = object->server;
	p_pair->m_serializer = ILU_C_CreateSerializationContext(object->server, &ilu_env);
	show_and_clear_if_exception(&ilu_env, "ILU_C_CreateSerializationContext problem", stderr);
	
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
	show_and_clear_if_exception(&ilu_env, 
				    "ILU_C_CreateSerializationContext: ILU_C_Server_duplicate  problem", stderr);


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

void release_object_serializer (ILU_C_OBJECT object) {
	
	serializer_refcount_pair* p_pair;
	ILU_C_Server c_server;
	CORBA_Environment ilu_env;					/* used to get potential error info */
	
	if (g_b_serial)
		ilu_AcquireMutex(g_serializer_hash_table_mutex);
	else 
		return;
	
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
	show_and_clear_if_exception(&ilu_env, "ILU_C_ReleaseSerializer problem", stderr);
	ilu_hash_RemoveFromTable(g_serializer_hash_table, object->server);
	free(p_pair);

	/* arrange the release on the C server to take place later (betting on temporal locality) */
	do_seconds_from_now(5, do_server_release, c_server);

	ilu_ReleaseMutex(g_serializer_hash_table_mutex);

}




/* ********************************************************* */
/* Utility Functions                                         */



/* ********************************************************* */
/* creates and returns a duplicate of the NUL-terminated
string parameter                                          */

char *duplicate_c_string (char *input_string)
{
	char *output_string;
	if (input_string == ILU_NIL)
		return ILU_NIL;
	else
    {
		output_string = (char *)malloc(strlen(input_string) + 1);
		if (output_string != ILU_NIL)
			strcpy(output_string, input_string);
		return output_string;
    }
}


/* ********************************************************* */
/* return char* containing the current UTC time in asctime format */

char* get_time_string (int b_duplicate) {
	struct tm *newtime;
	long ltime;
	char* pc_timestring;
	
	time( &ltime );						/* get the time */
	newtime = gmtime( &ltime );			/* convert to UTC structure */
	pc_timestring = asctime( newtime); /* get char string represnentation */
	pc_timestring[strlen(pc_timestring) - 1] = '\0';	/* knock off \n */
	if (b_duplicate)
		return duplicate_c_string(pc_timestring);
	else 
		return pc_timestring;
}


/* ********************************************************* */
/* printout a request                                        */

void print_request (iluhttp_Request* p_the_request, FILE* p_file) {
	
	iluhttp_Header* p_the_header;			/* used to point to headers */
	unsigned long ul_index;				/* used as index into sequences */
	
	fprintf(p_file, "Request: (Note all values are shown between >< s)\n");
	
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
/* printout response                                         */

void print_response (iluhttp_Response* p_the_response, int i_show_body_too, FILE* p_file) {
	
	iluhttp_Header* p_the_header;			/* used to point to headers */
	unsigned long ul_index;				/* used as index into sequences */
	
	fprintf(p_file, "Response: (Note all values are shown between >< s)\n");
	
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
/* show an exception if we have one                          */

ilu_boolean show_and_clear_if_exception (CORBA_Environment* p_ilu_env, char* pc_situation, FILE* p_file) {
	
	if (p_ilu_env->returnCode == ILU_NIL) 
		return ilu_FALSE;		/* just return false on no exception set */
	
	/* show exception if we got one */
	fprintf (p_file, "\nException: %s\n", pc_situation);
	
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
	
	/* clean things up */
	if (p_ilu_env->freeRoutine && p_ilu_env->ptr)
		(p_ilu_env->freeRoutine)(p_ilu_env->ptr);
	p_ilu_env->_major = CORBA_NO_EXCEPTION;
	p_ilu_env->returnCode = ILU_NIL;
	p_ilu_env->ptr = NULL;
	
	return ilu_TRUE;
}


/* ********************************************************* */
/* (not very smart) routine like strtokr only or getting 
url's from a http entity body.  Returns an allocated char*
holding the full url of the child.                       */

char* get_next_url_in_body(CORBA_octet** pc_where_to_start, iluhttp_EntityBody* p_body,
						   char* pc_parent_url) {
	CORBA_octet* p_walker;
	fprintf(stderr, "Warning: get_next_url_in_body not yet implemented!\n");
	
	if (*pc_where_to_start == NULL)/* must be first entry */
		*pc_where_to_start = p_body->_buffer;
	
	p_walker = p_body->_buffer;
	return NULL;
	
	
	
}


/* ********************************************************* */
/* returns 1 if the response has content type of text/html   */

int is_body_text_html(iluhttp_Response* p_http_resp) {
	unsigned int i_header_index;
	iluhttp_HTTPHeader* p_header = p_http_resp->headers._buffer;
	for (i_header_index = 0; i_header_index < p_http_resp->headers._length; i_header_index++) {
		if ((strcmp(p_header->name, "Content-Type") == 0) &&
			p_header->value &&
			(strstr(p_header->value, "text/html") != 0))
			return 1;
		p_header++;
	}
	return 0;
}




/* ********************************************************* */
/* ********************************************************* */
/* Get functions                                             */


/* ********************************************************* */
/* make Get calls on a iluhttp.Resource object               */
/* when called in the threaded situation, i_repeats should always be 1 */

int do_get_work (iluhttp_Resource http_obj, iluhttp_Request* p_http_req, int i_repeats, int i_depth) {
	
	CORBA_Environment ilu_env;					/* used to get potential error info */
	iluhttp_Response* p_http_resp;				/* will hold the response returned from http__obj */
	int i_count;
	ilu_FineTime  timebegin, timeend, timeinterval;
	ilu_FineTime  timeaccumulated = {0};
	StatSums timesums = {0};
	
	/* make any logfile entries */
	LOG_ENTER();
	if (DO_LOG_BASIC()) 	
		print_request(p_http_req, g_p_logfile);
	
	if (DO_LOG_TIMING()) {
		fprintf (g_p_logfile, "Start %d Get Requests on %s\n", i_repeats, p_http_req->URI);
	}
	LOG_EXIT();

	/* do the Get i_repeats times */	
	for (i_count = 0; i_count < i_repeats; i_count++) {
		
		timebegin = ilu_FineTime_Now(); /* get start time */
		
		/* xxx should we bother to update the date header here ? */
		/* call the GET method on the http_obj */
		p_http_resp = iluhttp_Resource_GET (http_obj, p_http_req, &ilu_env);
		
		/* get end time, and accumulate */
		timeend = ilu_FineTime_Now();
		timeinterval = ilu_FineTime_Sub(timeend, timebegin);
		timeaccumulated = ilu_FineTime_Add(timeaccumulated, timeinterval);

		AddStat(&timesums, timeinterval);

		/* add to the complete Get time total */
		ACCUM_TIME_ENTER();
		AddStat(&g_get_timesums, timeinterval);
		g_last_get_end_time = timeend;
		ACCUM_TIME_EXIT();
	

		/* make any logfile entries */
		LOG_ENTER();
		
		if (DO_LOG_BASIC()) {
			if (show_and_clear_if_exception(&ilu_env, p_http_req->URI, g_p_logfile)) {
				LOG_EXIT();
				return -1;
			}
		}
		else
			if (show_and_clear_if_exception(&ilu_env, p_http_req->URI, stderr))
				return -1;
		
		if (DO_LOG_FULL())
			print_response(p_http_resp, 1, g_p_logfile );
		else if (DO_LOG_BASIC())
			print_response(p_http_resp, 0, g_p_logfile );
		
		LOG_EXIT();
		
		/* write the body if there is one to the body file if it's set up */
		if (g_p_body_file && p_http_resp->body) {
			if (g_body_file_mutex) ilu_AcquireMutex(g_body_file_mutex);
			
			fwrite(p_http_resp->body->_buffer, sizeof(CORBA_octet), 
				p_http_resp->body->_length, g_p_body_file);
			if (g_body_file_mutex) ilu_ReleaseMutex(g_body_file_mutex);
		}
		
		/* free response's storage unless it's the last time (when we'll want to 
		scan the body for other child URLs to retrieve when depth is > 0 )*/
		if (i_count != i_repeats - 1) {
			iluhttp_Response__Free (p_http_resp);
			ilu_free(p_http_resp);
		}		
	}
	
		
	/* make any logfile entries */
	if (DO_LOG_TIMING()) {
		LOG_ENTER();
		fprintf (g_p_logfile, "End %d Get Requests on %s\n", 
			i_repeats, p_http_req->URI);
		fprintf (g_p_logfile, "\tElapsed time, total:  %.2f seconds\n",
			((double) timeaccumulated.ft_s * ilu_FineTimeRate + timeaccumulated.ft_t) / ilu_FineTimeRate);
		fprintf(g_p_logfile, "\t%s\n", FmtStats(&timesums));
		LOG_EXIT();
	}
	
	/* if we're supposed to do Gets on the bodies child urls */
	if (i_depth > 0 && 
		p_http_resp->body &&
		is_body_text_html(p_http_resp)) {
		char* pc_childurl;
		CORBA_octet* pc_where_to_start = NULL;
		pc_childurl = get_next_url_in_body(&pc_where_to_start, p_http_resp->body, p_http_req->URI);
		if (!g_b_multithreaded) {
			while (pc_childurl) {
				do_get(pc_childurl, p_http_req, i_repeats, i_depth - 1);
				free(pc_childurl);
				get_next_url_in_body(&pc_where_to_start, p_http_resp->body, p_http_req->URI);
			}
		}
#ifdef ILU_OS_THREADED
		else { /* doing this in a threaded manner (repeats should always be one here) */
			while (pc_childurl) {
				for (i_count = 0; i_count < i_repeats; i_count++)
					fork_do_threaded_get(pc_childurl, i_depth - 1);
				free(pc_childurl);
				get_next_url_in_body(&pc_where_to_start, p_http_resp->body, p_http_req->URI);
			}
		}
#endif /* ILU_OS_THREADED */
	}
	
	/* free the saved response's storage*/
	iluhttp_Response__Free (p_http_resp);	
	ilu_free(p_http_resp);

	return 1;
	
}



/* ********************************************************* */
/* make the calls on a Resource object                       */

int do_get (char* pc_url, iluhttp_Request* p_http_req, int i_repeats, int i_depth) {
	
	CORBA_Environment ilu_env;					/* used to get potential error info */
	iluhttp_Resource http_obj;					/* will be an 'object' from an existing httpd */

	/* try to get an iluhttp.Resource object to represent pc_url by parsing the pc_url */
	if ((http_obj = ILU_C_SBHToObject (pc_url, iluhttp_Resource__MSType, &ilu_env)) != ILU_NIL) {

		p_http_req->URI = pc_url;	/* assign the the URI in the request */ 
		
		do_get_work(http_obj, p_http_req, i_repeats, i_depth);
		
		/* no longer need this surrogate, cleanup */
		CORBA_Object_release(http_obj, &ilu_env);
		if (DO_LOG_BASIC()) {
			LOG_ENTER();
			show_and_clear_if_exception(&ilu_env, "CORBA_Object_release problem", g_p_logfile);
			LOG_EXIT();
		}
		
		return 1;
	}
	
	if (DO_LOG_BASIC()) {
		LOG_ENTER();
		fprintf (g_p_logfile, "Error: Can't obtain any known object types from %s\n", pc_url);
		show_and_clear_if_exception(&ilu_env, "ILU_C_SBHToObject", g_p_logfile);
		LOG_EXIT();
	}
	
	return -1;
	
}


#ifdef ILU_OS_THREADED

/* ********************************************************* */
/* called when we're doing gets in a threaded manner - does 1 get */

void do_threaded_get (forked_get_args* args) {

    char* pc_url = args->m_pc_url;
	int i_depth = args->m_i_depth;
	CORBA_Environment ilu_env;					/* used to get potential error info */
	iluhttp_Resource http_obj;					/* will be an 'object' from an existing httpd */
	ilu_Error an_error;
	iluhttp_Request http_req;					/* will be a Request to pass to http objects */
	free(args);

	/* make some simple headers */
	http_req.headers._maximum = 3;						
	http_req.headers._length = 3;
	http_req.headers._buffer = (iluhttp_HTTPHeader*) ilu_malloc ( http_req.headers._maximum * 
		sizeof(iluhttp_HTTPHeader));
	
	http_req.headers._buffer[0].name = "User-Agent";
	http_req.headers._buffer[0].value = "ILUGetBot/1.0";
	
	http_req.headers._buffer[1].name = "Date";
	http_req.headers._buffer[1].value = get_time_string(1);
	
	http_req.headers._buffer[2].name = "Host";
	http_req.headers._buffer[2].value = g_pc_hostname;
		
	http_req.body = ILU_NIL; /* no body */	

	if (g_b_pipeline && g_pipeline) 
		ILU_C_SetPipelineContext(g_pipeline);

	if (g_b_batching && g_batcher) 
		ILU_C_SetBatcherContext(g_batcher);

	/* try to get an iluhttp.Resource object to represent pc_url by parsing the pc_url */
	if ((http_obj = ILU_C_SBHToObject (pc_url, iluhttp_Resource__MSType, &ilu_env)) != ILU_NIL) {

		if (g_b_serial) {
			/* using intermediate serializer variable for debugging purposes */
			ILU_C_Serializer the_serializer = get_object_serializer(http_obj);
			ILU_C_SetSerializationContext(the_serializer);
		}

		http_req.URI = pc_url;	/* assign the the URL in the request */ 

		do_get_work(http_obj, &http_req, 1, i_depth);

		free(http_req.headers._buffer[1].value); /* free time string */

		if (g_b_serial) {
			ILU_C_SetSerializationContext(NULL);
			release_object_serializer(http_obj);
		}

		/* no longer need this surrogate, cleanup */
		CORBA_Object_release(http_obj, &ilu_env);
		if (DO_LOG_BASIC()) {
			LOG_ENTER();
			show_and_clear_if_exception(&ilu_env, "CORBA_Object_release problem", g_p_logfile);
			LOG_EXIT();
		}
		else show_and_clear_if_exception(&ilu_env, "CORBA_Object_release problem", stderr);

		goto finish_thread;
	}
	
	if (DO_LOG_BASIC()) {
		LOG_ENTER();
		fprintf (g_p_logfile, "Error: Can't obtain any known object types from %s\n", pc_url);
		LOG_EXIT();
	}

finish_thread:


	if (g_b_pipeline) 
		ILU_C_SetPipelineContext(NULL); 

	if (g_batcher) 
		ILU_C_SetBatcherContext(NULL);


	/* free url and headers */
	free(pc_url);
	ilu_free(http_req.headers._buffer);

	/* notify condition so a waiter might proceed */
	ilu_AcquireMutex(g_get_thread_count_mutex);
	g_i_number_of_get_threads--;
	ilu_CondNotify(g_get_thread_count_condition, &an_error);
	if (ILU_ERRNOK(an_error)) {
		fprintf(stderr, "Error calling ilu_CondNotify on g_get_thread_count_condition - proceeding anyway\n");
		ILU_HANDLED(an_error);
		ILU_CLER(an_error);
	}
	
	ilu_ReleaseMutex(g_get_thread_count_mutex);

	return;
	
}

/* ********************************************************* */
/* forks a thread doing a threaded get */

void fork_do_threaded_get(char* pc_url, int i_depth) {
	
	forked_get_args* p_get_args;
	ilu_Error an_error;
		
	/* do conditional wait stuff here so as not to exceed max threads */
	ilu_AcquireMutex(g_get_thread_count_mutex);
	while(1) {
		if ((g_i_max_number_of_get_threads != 0) && 
			(g_i_number_of_get_threads >= g_i_max_number_of_get_threads)) {
			ilu_CMWait1(g_get_thread_count_condition, g_get_thread_count_mutex, &an_error);
			if (ILU_ERRNOK(an_error)) {
				fprintf(stderr, "Error calling ilu_CMWait1 on g_get_thread_count_condition - skipping thread\n");
				ILU_HANDLED(an_error);
				ILU_CLER(an_error);
				ilu_ReleaseMutex(g_get_thread_count_mutex);
				return;
			}
			
		}
		else break;
	}
	g_i_number_of_get_threads++;
	
	ilu_ReleaseMutex(g_get_thread_count_mutex);

	p_get_args = (forked_get_args*) malloc(sizeof(forked_get_args));
	/* set up args */
	p_get_args->m_pc_url = duplicate_c_string(pc_url);
	p_get_args->m_i_depth = i_depth;

	if (!ilu_Fork ((fork_procedure)do_threaded_get, p_get_args, &an_error))
		fprintf(stderr, "fork_do_threaded_get - Couldn't fork thread\n");
	
}

#endif

/* ********************************************************* */
/* ********************************************************* */
/* startup and cleanup functions                             */


/* ********************************************************* */
/* Usage string                                              */

char g_c_usage[] = 
"Usage:  ilugetbot -url url | -urls urlFilename [-mt maxthreads] [-v level] [-d depth] [-r repeat] \n\
[-pipeline] [-batch period] [-serial] [-http_1_X] [-file bodyfilename]\n\
[-logfile logfilename]\n\
Note: To get 'Pipelining' behavior as conceptualized in in Http_1_1, use -pipeline, -serial, and -batch\n\n\
\t-url url              url of object to do Get on\n\
\t-urls urlFilename     name of file containing URL's (one per line) to be retrieved. (can be stdin)\n\
\t-mt maxthreads        run ILU in multi threaded mode (default is single)\n\
\t                      maxthreads = max number of worker threads, 0 = No Limit\n\
\t-pipeline             use ILU Pipeline - implies -mt\n\
\t-batch usec           use ILU Batching, period in microseconds\n\
\t-serial               use ILU Serializer - implies -mt\n\
\t-v level              set verbosity to integer level (default is 0)\n\
\t                      0 = no output\n\
\t                      1 = shows summary timing\n\
\t                      2 = shows basic timing\n\
\t                      3 = shows basic timing, plus requests and response headers\n\
\t                      4 = shows  basic timing, plus requests and complete response\n\
\t-r repeat             how many times to do the Get on each URL (default is 1)\n\
\t-d depth              Not Yet implemented - for each URL, get it's children up to a depth (default is 0)\n\
\t-http_1_0             for http URLs, what http version to use when calling\n\
\t-http_1_0p\n\
\t-http_1_1\n\
\t-file bodyfilename    name of file where body from Get should be written (can be stdout)\n\
\t-logfile logfilename  name of logfile where verbose output goes (can be stdout)\n";


/* Not Yet Implemented 
*/


/* ********************************************************* */
/* cleanup - does any necessary cleanup work                 */

void cleanup() {
	
	CORBA_Environment ilu_env;			/* used to get potential error info */
	ilu_Error an_error;
	
	/* close any files */
	if (g_p_body_file && g_p_body_file != stdout)
		fclose (g_p_body_file);
	
	if (g_p_logfile && g_p_logfile != stdout)
		fclose (g_p_logfile);
	
	if (g_p_urlfile && g_p_urlfile != stdin)
		fclose (g_p_urlfile);
	
	if (g_pipeline) {
		ILU_C_SetPipelineContext(NULL);
		ILU_C_ReleasePipeline(g_pipeline, &ilu_env);
		show_and_clear_if_exception(&ilu_env, "calling ILU_C_ReleasePipeline", stderr);
	}

	if (g_batcher) {
		ILU_C_SetBatcherContext(NULL);
		ILU_C_ReleaseBatcher(g_batcher, &ilu_env);
		show_and_clear_if_exception(&ilu_env, "calling ILU_C_ReleaseBatcher", stderr);
	}

	if (g_body_file_mutex) {
		ilu_DestroyMutex(g_body_file_mutex, &an_error);
		if (ILU_ERRNOK(an_error)) {
			fprintf(stderr, "Error destroying body file mutex\n");
			ILU_HANDLED(an_error);
			ILU_CLER(an_error);
		}
	}
	
	if (g_logfile_mutex) {
		ilu_DestroyMutex(g_logfile_mutex, &an_error);
		if (ILU_ERRNOK(an_error)) {
			fprintf(stderr, "Error destroying logfile mutex\n");
			ILU_HANDLED(an_error);
			ILU_CLER(an_error);
		}
	}
	
	if (g_urlfile_mutex) {
		ilu_DestroyMutex(g_urlfile_mutex, &an_error);
		if (ILU_ERRNOK(an_error)) {
			fprintf(stderr, "Error destroying urlfile mutex\n");
			ILU_HANDLED(an_error);
			ILU_CLER(an_error);
		}
	}

	if (g_timeaccumulated_mutex) {
		ilu_DestroyMutex(g_timeaccumulated_mutex, &an_error);
		if (ILU_ERRNOK(an_error)) {
			fprintf(stderr, "Error destroying timeaccumulated mutex\n");
			ILU_HANDLED(an_error);
			ILU_CLER(an_error);
		}
	}

	if (g_get_thread_count_mutex) {
		ilu_DestroyMutex(g_get_thread_count_mutex, &an_error);
		if (ILU_ERRNOK(an_error)) {
			fprintf(stderr, "Error destroying get_thread_count mutex\n");
			ILU_HANDLED(an_error);
			ILU_CLER(an_error);
		}
	}

	if (g_get_thread_count_condition) {
		ilu_DestroyCondition(g_get_thread_count_condition);
	}

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
	
}



/* ********************************************************* */
/* process command line                                      */

int process_command_line_and_setup(int i_argcount, char** ppc_argvalue) {
	
	int i_argindex = 1;				/* for walking through command line arguments */
	CORBA_Environment ilu_env;			/* used to get potential error info */
	ilu_Error an_error;
	ilu_cardinal card_budget;

	/* setup based on command line arguments */
	while (i_argindex < i_argcount) {
		
		/* check for threading */		
		if (strcmp(ppc_argvalue[i_argindex], "-mt") == 0) {
#ifndef	ILU_OS_THREADED
			fprintf (stderr, "OS-supplied thread support not configured into ILU!\n");
			return -1;
#else
			g_b_multithreaded = 1;
			ILU_C_USE_OS_THREADS;
			i_argindex++;
			if (i_argindex >= i_argcount ||
				sscanf(ppc_argvalue[i_argindex], "%i", &g_i_max_number_of_get_threads) != 1) {
				fprintf(stderr, "Invalid maximum threads %s\n", ppc_argvalue[i_argindex]);
				return -1;
			}
			/* ensure our fdbudget is boosted appropriately for the number of threads 
			   -- just some 'guestimation' here. If we run out we'll get resource exceptions */
	       card_budget = ilu_GetFDBudget() + 
	                     (2 * (g_i_max_number_of_get_threads ? g_i_max_number_of_get_threads : 64)) + 16;
			ilu_SetFDBudget(card_budget);
			goto nextarg;
#endif				/* ILU_OS_THREADED */
		} 
		
		/* check for pipeline use */
		if (strcmp(ppc_argvalue[i_argindex], "-pipeline") == 0) {
			g_b_pipeline = 1;
			goto nextarg;
		} 

		/* check for serializer use */
		if (strcmp(ppc_argvalue[i_argindex], "-serial") == 0) {
			g_b_serial = 1;
			goto nextarg;
		} 
		
		/* check for batching use */
		if (strcmp(ppc_argvalue[i_argindex], "-batch") == 0) {
			g_b_batching = 1;
			i_argindex++;
			if (i_argindex >= i_argcount ||
				sscanf(ppc_argvalue[i_argindex], "%i", &g_ui_batching_period) != 1) {
				fprintf(stderr, "Invalid batching period %s\n", ppc_argvalue[i_argindex]);
				return -1;
			}
			goto nextarg;
		} 
		
		/* check for verbosity settings */
		if (strcmp(ppc_argvalue[i_argindex], "-v") == 0) {
			i_argindex++;
			if (i_argindex >= i_argcount ||
				(sscanf(ppc_argvalue[i_argindex], "%i", &g_i_verbosity) != 1) ||
				(g_i_verbosity > MAX_VERBOSITY_LEVEL)) {
				fprintf(stderr, "Invalid verbosity level %s\n", ppc_argvalue[i_argindex]);
				return -1;
			}
			goto nextarg;
		} 
		
		/* check for body file settings */
		if (strcmp(ppc_argvalue[i_argindex], "-file") == 0) {
			i_argindex++;
			if (i_argindex >= i_argcount) {
				fprintf(stderr, "No body file name provided\n");
				return -1;
			}
				g_pc_body_filename = ppc_argvalue[i_argindex];
			goto nextarg;
		} 
		
		/* check for url file settings */
		if (strcmp(ppc_argvalue[i_argindex], "-urls") == 0) {
			i_argindex++;
			if (i_argindex >= i_argcount) {
				fprintf(stderr, "No url file name provided\n");
				return -1;
			}
			g_pc_urlfilename = ppc_argvalue[i_argindex];
			goto nextarg;
		} 
		
		/* check for url specified */
		if (strcmp(ppc_argvalue[i_argindex], "-url") == 0) {
			i_argindex++;
			if (i_argindex >= i_argcount) {
				fprintf(stderr, "No url provided\n");
				return -1;
			}
			g_pc_url_to_get = ppc_argvalue[i_argindex];
			goto nextarg;
		} 
		
		/* check for log file settings */
		if (strcmp(ppc_argvalue[i_argindex], "-logfile") == 0) {
			i_argindex++;
			if (i_argindex >= i_argcount) {
				fprintf(stderr, "No logfile name provided\n");
				return -1;
			}
			g_pc_logfilename = ppc_argvalue[i_argindex];
			goto nextarg;
		} 
		
		/* check for depth setting */
		if (strcmp(ppc_argvalue[i_argindex], "-d") == 0) {
			i_argindex++;
			if (i_argindex >= i_argcount ||
				sscanf(ppc_argvalue[i_argindex], "%i", &g_i_depth) != 1) {
				fprintf(stderr, "Invalid depth level %s\n", ppc_argvalue[i_argindex]);
				return -1;
			}
			fprintf(stderr, "Warning, depth not yet implemented - defaulting to 0\n");
			g_i_depth = 0;
			goto nextarg;
		} 
		
		/* check for repeats setting */
		if (strcmp(ppc_argvalue[i_argindex], "-r") == 0) {
			i_argindex++;
			if (i_argindex >= i_argcount ||
				(sscanf(ppc_argvalue[i_argindex], "%i", &g_i_repeats) != 1) ||
				(g_i_repeats <= 0)) {
				fprintf(stderr, "Invalid repeats %s\n", ppc_argvalue[i_argindex]);
				return -1;
			}
			goto nextarg;
		} 
		
		/* check for http version to use when calling */
		if ((strcmp(ppc_argvalue[i_argindex], "-http_1_1") == 0) ||
			(strcmp(ppc_argvalue[i_argindex], "-http_1_0p") == 0) ||
			(strcmp(ppc_argvalue[i_argindex], "-http_1_0") == 0)) {
			ilu_http_default_scheme = (ppc_argvalue[i_argindex]) + 1;
			goto nextarg;
		}
		
		
		/* unknown command line argument */
		fprintf (stderr, "Invalid argument %s\n%s\n", ppc_argvalue[i_argindex], g_c_usage);
		return -1;
		
nextarg:
		/* advance to the next command line argument */
		i_argindex++;
	}
	
	
	if (!g_pc_url_to_get && !g_pc_urlfilename) {
		fprintf (stderr, "Nothing to get!\n%s\n", g_c_usage);
		return -1;
	}
	
	if((g_b_pipeline || g_b_serial) && !g_b_multithreaded) {
		fprintf (stderr, "Need to specify -mt with either -pipeline or -serial\n");
		return -1;
	}

	/* set up  required files */
	if (g_pc_urlfilename) {
		if (strcmp(g_pc_urlfilename, "stdin") == 0)
			g_p_urlfile = stdin;
		if ((g_p_urlfile = fopen(g_pc_urlfilename, "r")) == NULL) {
			fprintf (stderr, "Couldn't open %s\n", g_pc_urlfilename);
			return -1;
		}
#ifdef ILU_OS_THREADED
		if (g_b_multithreaded)
			g_urlfile_mutex = ilu_CreateMutex("ilugetbot", "urlfile");
#endif /* ILU_OS_THREADED */
	}
	
	if (g_pc_body_filename) {
		if (strcmp(g_pc_body_filename, "stdout") == 0)
			g_p_body_file = stdout;
		else
			if ((g_p_body_file = fopen(g_pc_body_filename, "wb")) == NULL) {
				fprintf (stderr, "Couldn't open %s\n", g_pc_body_filename);
				return -1;
			}
#ifdef ILU_OS_THREADED
		if (g_b_multithreaded)
			g_body_file_mutex = ilu_CreateMutex("ilugetbot", "body_file");
#endif /* ILU_OS_THREADED */
	}
	
	if (g_pc_logfilename) {
		if (strcmp(g_pc_logfilename, "stdout") == 0)
			g_p_logfile = stdout;
		else 
			if ((g_p_logfile = fopen(g_pc_logfilename, "w")) == NULL) {
				fprintf (stderr, "Couldn't open %s\n", g_pc_logfilename);
				return -1;
			}
#ifdef ILU_OS_THREADED
		if (g_b_multithreaded)
			g_logfile_mutex = ilu_CreateMutex("ilugetbot", "logfile");
#endif /* ILU_OS_THREADED */
	}
	
#ifdef ILU_OS_THREADED
	if (g_b_multithreaded) {
		g_timeaccumulated_mutex = ilu_CreateMutex("ilugetbot", "timeaccumulated");
		g_get_thread_count_mutex = ilu_CreateMutex("ilugetbot", "get_thread_count");
		g_get_thread_count_condition = ilu_CreateCondition("ilugetbot", "get_thread_count", &an_error);
		if (ILU_ERRNOK(an_error)) {
			fprintf(stderr, "Error Creating get_thread_count condition\n");
			ILU_HANDLED(an_error);
			ILU_CLER(an_error);
		}
		g_serializer_hash_table_mutex = ilu_CreateMutex("ilugetbot", "serializer_hash_table");
	}
#endif /* ILU_OS_THREADED */

	/* set up pipeline if requested */
	if (g_b_pipeline) {
		g_pipeline = ILU_C_CreatePipeline(&ilu_env);
		if (!show_and_clear_if_exception(&ilu_env, "calling ILU_C_CreatePipeline", stderr))
		ILU_C_SetPipelineContext(g_pipeline);
	}
	
	/* set up batcher if requested */
	if (g_b_batching) {
		ilu_FineTime batching_time;
		batching_time = ilu_FineTime_FromDouble(((double)g_ui_batching_period) / 1000000.0);
		g_batcher = ILU_C_CreateBatcher(batching_time, ilu_FALSE, &ilu_env);
		if (!show_and_clear_if_exception(&ilu_env, "calling ILU_C_CreateBatcher", stderr))
			ILU_C_SetBatcherContext(g_batcher);
	}

	/* set up for serializers if requested */
	if (g_b_serial) {
	/* create the hash table that we use to keep track of serializers */
		g_serializer_hash_table = ilu_hash_MakeNewTable(251, ilu_hash_HashPointer, ilu_hash_PointerCompare);
	}

	/* get our hostname */
	gethostname(g_pc_hostname, sizeof(g_pc_hostname));

	return 0;
}



/* ********************************************************* */
/* MAIN                                                      */


int main(int i_argcount, char** ppc_argvalue) {
	
	iluhttp_Request http_req;		/* will be a Request to pass to http objects */
	int i_count;
	ilu_FineTime  timebegin, timeinterval;  /* vars for statistics */
	ilu_cardinal  card_bytes_sent;
	ilu_cardinal  card_bytes_received;
	ilu_cardinal  card_moorings_created;
	ilu_cardinal  card_connections_received;
	ilu_cardinal  card_connections_created;
	ilu_cardinal  card_current_connections;
	ilu_cardinal  card_max_connections;
	double        d_elapsed_time;

	 /* _putenv("ILU_DEBUG=");    debugging only */

	/* setup from the command line */
	if (process_command_line_and_setup(i_argcount, ppc_argvalue) != 0) {
		cleanup();
		return -1;
	}
	
	
	iluhttp__Initialize();				/* perform required initialization */
	ilu_tcp_InitializeStats();          /* ensure tcp stats are zeroed */

	/* create a skeleton based  */ 
	
	/* make some simple headers */
	http_req.headers._maximum = 3;						
	http_req.headers._length = 3;
	http_req.headers._buffer = (iluhttp_HTTPHeader*) ilu_malloc ( http_req.headers._maximum * 
		sizeof(iluhttp_HTTPHeader));
	
	http_req.headers._buffer[0].name = "User-Agent";
	http_req.headers._buffer[0].value = "ILUGetBot/1.0";
	
	http_req.headers._buffer[1].name = "Date";
	http_req.headers._buffer[1].value = get_time_string(0);
	
	http_req.headers._buffer[2].name = "Host";
	http_req.headers._buffer[2].value = g_pc_hostname;
	
	/* used for testing only to force use of a particular proxy server */
	/* _putenv("ILU_HTTP_PROXY_INFO=wwwproxy.parc.xerox.com:8000"); */
	
	http_req.body = ILU_NIL; /* no body */
	
	/* note that ilu's http will put in Content-Length header if an
	Entity body is supplied, and we haven't put in our own content length header */
	
	timebegin = ilu_FineTime_Now(); /* get start time */

	/* do the actual Get(s) */
	
	if (g_pc_url_to_get) {
		if (!g_b_multithreaded)
		/* just a single url to get */
		do_get(g_pc_url_to_get, &http_req, g_i_repeats, g_i_depth);
#ifdef ILU_OS_THREADED
		else 
			for (i_count = 0; i_count < g_i_repeats; i_count++) {
				fork_do_threaded_get(g_pc_url_to_get, g_i_depth);
			}
#endif /* ILU_OS_THREADED */
	}
	else {
		/* go through all the urls in the file */
		char c_url_to_get[512];
		int i_fscanresult;
		int i_linenumber = 1;
		if (g_urlfile_mutex) ilu_AcquireMutex(g_urlfile_mutex);
		i_fscanresult = fscanf(g_p_urlfile, "%s\n", &c_url_to_get);
		if (g_urlfile_mutex) ilu_ReleaseMutex(g_urlfile_mutex);
		while (i_fscanresult != EOF) {
			if (i_fscanresult != 1) {
				fprintf (stderr, "Problem reading from %s, line %d\n", g_pc_urlfilename, i_linenumber);
				break;
			}
			else {
				i_linenumber++;
				if (!g_b_multithreaded)
					do_get(c_url_to_get, &http_req, g_i_repeats, g_i_depth);
#ifdef ILU_OS_THREADED
				else 
					for (i_count = 0; i_count < g_i_repeats; i_count++)
						fork_do_threaded_get(c_url_to_get, g_i_depth);
#endif /* ILU_OS_THREADED */
			}
			if (g_urlfile_mutex) ilu_AcquireMutex(g_urlfile_mutex);
			i_fscanresult = fscanf(g_p_urlfile, "%s\n", &c_url_to_get);
			if (g_urlfile_mutex) ilu_ReleaseMutex(g_urlfile_mutex);
		}
	}
	
#ifdef ILU_OS_THREADED
	/* wait for all threads to finish here */

	if (g_b_multithreaded) {
		while (1) {
			
#ifdef WIN32
			Sleep(1000);
#else
			sleep(1);
#endif
			
			ilu_AcquireMutex(g_get_thread_count_mutex);
			if (g_i_number_of_get_threads <= 0) 
				break;
			
			ilu_ReleaseMutex(g_get_thread_count_mutex);	
		}
		ilu_ReleaseMutex(g_get_thread_count_mutex);
	}
#endif /* ILU_OS_THREADED */
	
		/* get end time, and interval */
		timeinterval = ilu_FineTime_Sub(g_last_get_end_time, timebegin);

	if (DO_LOG_SUMMARY()) {
		LOG_ENTER();
		ACCUM_TIME_ENTER();

		d_elapsed_time = ((double) timeinterval.ft_s * ilu_FineTimeRate + timeinterval.ft_t) / ilu_FineTimeRate;
		ilu_tcp_GetStats (&card_bytes_sent, 
				  &card_bytes_received,
				  &card_moorings_created,
				  &card_connections_received,
				  &card_connections_created,
				  &card_current_connections,
				  &card_max_connections);

		fprintf (g_p_logfile, "\nTOTALS: %d Gets in %.2f seconds, %.2f bytes/second\n",
			 g_get_timesums.n, d_elapsed_time, 
			 (card_bytes_sent + card_bytes_received)  / d_elapsed_time );


		fprintf (g_p_logfile, "Statistics for Get Times:\n");

		fprintf(g_p_logfile, "\t%s\n", FmtStats(&g_get_timesums));

		/* show tcp stats */

		fprintf (g_p_logfile, "TCP Stats:\n\tbytes_sent %lu\n\tbytes_received %lu\n\tmoorings_created %lu\n\
\tconnections_received %lu\n\tconnections_created %lu\n\tcurrent_connections %lu\n\tmax_connections %lu\n",
			 card_bytes_sent, 
			 card_bytes_received,
			 card_moorings_created,
			 card_connections_received,
			 card_connections_created,
			 card_current_connections,
			 card_max_connections);
		
		ACCUM_TIME_EXIT();
		LOG_EXIT();
	}
	
	
	/* end gracefully */
	ilu_free(http_req.headers._buffer);
	cleanup();
	
	return 0;
}



/* ********************************************************* */
/* End of file                                               */
/* ********************************************************* */
