/** $Id: nggetbot.c,v 1.37 1999/08/03 01:58:13 janssen Exp $
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

/* nggetbot: HTTP-NG URL getter program

 Dan Larner, larner@parc.xerox.com
 03-30-98
 
  
   Notes:  
   
	   In the comments below, the term url is used to generically
	   mean an http style url, or ilu SBH.
	   
		NYI means Not Yet Implemented.
		
*/

/* ********************************************************* */
/* includes                                                  */
/* ********************************************************* */

#include "nglib.h"
#include "nggetbot.h"
#include <math.h>
#include "ngrendering_getbot_impl.h"


/* undefine THREAD_POOL if you want a new thread for every 
request, as opposed to a pool of worker threads */

#define THREAD_POOL 1 

#ifndef THREAD_POOL
#ifndef THREAD_FOR_EVERY_REQUEST 
#define THREAD_FOR_EVERY_REQUEST 1
#endif
#else
#ifdef THREAD_FOR_EVERY_REQUEST 
#undef THREAD_FOR_EVERY_REQUEST 
#endif
#endif

/* so we can change the default http scheme */
ILU_PUBLIC ilu_string ilu_http_default_scheme;

/* so we can force connection close behavior when using http 1.0 on 
   non iluhttp.Resource objects */
ILU_PUBLIC ilu_boolean ilu_http_force_close_on_ilu_1_0;


/* ********************************************************* */
/* defines, globals, and typedefs                            */
/* ********************************************************* */

#ifdef ILU_OS_THREADED
/* for argument passing when doing a threaded get per request */
typedef struct forked_get_args {
	char* m_pc_url;
	int m_i_depth;
} forked_get_args;
#endif /* ILU_OS_THREADED */


/* how many get threads are available */
int g_i_number_of_get_threads = 0;
int g_i_max_number_of_get_threads = 0; /* 0 means no limit */
ilu_Mutex g_get_thread_count_mutex = NULL;
ilu_Condition g_get_thread_count_condition = NULL;
	
/* verbosity level */
int g_i_verbosity = 0;

/* how deep to go on each retrieval */
int g_i_depth = 0;

/* whether or not to boost priority of threads */
int g_b_thread_boost = 0;

/* how many times to get each URL */
int g_i_repeats = 1;

/* whether or not to use ILU pipeline */
int g_b_pipeline = 0;
ILU_C_Pipeline g_pipeline = NULL;

/* whether or not to use ILU batching */
int g_b_batching = 0;
unsigned int g_ui_batching_period;
ILU_C_Batcher g_batcher = NULL;

/* passport used for w3mux */
ilu_Passport g_passport = NULL;

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

/* When using Sinks, time from issuing Send Rendering to first rendering byte received. */
StatSums g_send_to_first_byte_timesums = {0};
ilu_Mutex g_send_to_first_byte_mutex = NULL;


/* holds the name of this host */
char g_pc_hostname[1024] = "";			


/* whether or not to use ILU serializers */
int g_b_serial = 0;

/* whether or not to use a RenderingSink to receive NGRenderings */
int g_b_rendersink = 0;

/* string used as suffix to our ilu server names */
ilu_string g_pc_nggetbot_suffix = XEROX_PARC_DOMAIN;  


/* default contact info to export iluhttp_Resource objects with */
char* g_pc_http_pinfo = "http_1_1";
char* g_ppc_http_tinfo [8] = 
{ "tcp_0_80", ILU_NIL, ILU_NIL, ILU_NIL, ILU_NIL, ILU_NIL, ILU_NIL, ILU_NIL };


/* default contact info to deal with non iluhttp_Resource objects  */
char* g_pc_nonhttp_pinfo = "w3ng_1.0";  
char* g_ppc_nonhttp_tinfo [8] = 
/* { "sunrpcrm", ILU_NIL, ILU_NIL, ILU_NIL, ILU_NIL, ILU_NIL, ILU_NIL, ILU_NIL }; */
{ "w3mux_16_NGGetbotEndpoint", ILU_NIL, ILU_NIL, ILU_NIL, ILU_NIL, ILU_NIL, ILU_NIL, ILU_NIL };

char g_pc_w3mux_Endpoint_Name[128] = {0};

/* port to start creating ng servers at */
unsigned short g_us_ngtcpport = 2728;

/* when using w3mux, channel to start creating ng servers at */
unsigned short g_us_ngmuxchannel = 0;

/* ilu fd budget */
static int g_i_fd_budget = -1;	


/* ********************************************************* */
/* ********************************************************* */
/* Forward Declarations                                      */

int do_get (char* pc_url, int i_repeats, int i_depth);

#ifdef ILU_OS_THREADED
void fork_do_threaded_get(char* pc_url, int i_depth);
#endif /* ILU_OS_THREADED */

ilu_boolean nglib_show_and_clear_if_exception (CORBA_Environment* p_ilu_env, char* pc_situation, FILE* p_file);


/* ********************************************************* */
/* ********************************************************* */
/* Statistics Functions  (from ILU's timeit)                 */



void AddStat(StatSums * ss, ilu_FineTime time_interval) {
	
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


#ifdef ILU_C_TIMING_STATISTICS
static char* FmtCRuntimeStats(ILU_C_CallStatSum * ss, ilu_cardinal ncalls) {
	static char     buf[512];
	double          avg, stdev;
	if (ncalls > 0) {
		avg = ss->sumX / ncalls;
		if (ncalls > 1) {
			stdev = (ss->sumXX - ss->sumX * ss->sumX / ncalls) / (ncalls - 1);
			if (stdev >= 0.0) {
				stdev = sqrt(stdev);
				sprintf(buf, "%d Samples\n", ncalls);
				sprintf(buf, "%min %6.1f msec, avg. %6.1f msec, stddev %6.1f , max %6.1f msec",
					ss->minX/1000.0, avg/1000.0, stdev/1000.0, ss->maxX/1000.0);
			} 
			else
				sprintf(buf, "min %6.1f msec, avg. %6.1f msec, stddev sqrt(%6.1f) , max %6.1f msec",
				ss->minX/1000.0, avg/1000.0, stdev/1000.0, ss->maxX/1000.0);
		} 
		else
			sprintf(buf, "%6.1f msec once", ss->sumX/1000.0);
	} 
	else
		sprintf(buf, "no samples");
	return buf;
}
#endif /* def ILU_C_TIMING_STATISTICS */







/* ********************************************************* */
/* Utility Functions                                         */


/* forks a thread and tries to boost its priority (currently only win32) */

ilu_boolean OSForkNewThread (void (*proc)(void *arg), void *arg,
			     ILU_ERRS((no_memory, no_resources, internal)) *err) {

#ifndef ILU_WIN32_THREADS
  return ilu_OSForkNewThread (proc, arg, err);

#else

DWORD thread_id;

if (!ilu_OSForkNewThreadEx (proc, arg, &thread_id, err))
    return ilu_FALSE;

if (!g_b_thread_boost)
    return ilu_TRUE;

/* thread is forked, bump the priority up if we can */
if (!SetThreadPriority((HANDLE)thread_id, THREAD_PRIORITY_HIGHEST)) {
  fprintf(stderr, "Couldn't boost thread 0x%x priority - Error 0x%x\n", 
	  thread_id, GetLastError());
}

return ilu_TRUE;
#endif

}




/* ********************************************************* */
/* returns g_ppc_nonhttp_tinfo set up with tcp_0_<value of g_us_ngtcpport++>
on each call, or if using w3mux, setup with w3mux_<value of g_us_ngmuxchannel++>
tcp_0_<value of g_us_ngtcpport>*/
/* xxx note hardcoded array size */

char** get_ngtinfo() {
	
	static int s_i_using_mux = 0;  /* 0 = not yet known, 1 = yes, 2 = no */
	static char* s_pc_channel_start = NULL;
	static char* s_pc_endpoint_start = NULL;
	int i_index;
	
	switch (s_i_using_mux) {
	case 1: /* using mux */
		for (i_index = 0; i_index < 8; i_index++) {
			if (strncmp("w3mux", g_ppc_nonhttp_tinfo[i_index], 5) == 0) {
				/* we found the mux entry */
				if (s_pc_endpoint_start) 
					sprintf(s_pc_channel_start, "%hu_%s", g_us_ngmuxchannel, s_pc_endpoint_start);
				else 
					sprintf(s_pc_channel_start, "%hu", g_us_ngmuxchannel);
				if (g_us_ngmuxchannel) g_us_ngmuxchannel++;
				break;
			}
		}
		
		/* fall through to ensure tcp_ is there -- xxx does some needless benign 
		work after the first time */
		
	case 2: /* not using mux */
		/* scan down g_ppc_nonhttp_tinfo till we encounter a NULL or a tcp entry */
		for (i_index = 0; i_index < 8; i_index++) {
			if (!(g_ppc_nonhttp_tinfo[i_index]) || 
				(strstr(g_ppc_nonhttp_tinfo[i_index], "tcp_") == g_ppc_nonhttp_tinfo[i_index])) {
				if (!(g_ppc_nonhttp_tinfo[i_index])) /* must be first time, make some space */
					g_ppc_nonhttp_tinfo[i_index] = ilu_malloc(32);
				sprintf (g_ppc_nonhttp_tinfo[i_index], "tcp_0_%hu", g_us_ngtcpport);
				if (s_i_using_mux == 2) /* must not be the fallthrough from case 1 */
					g_us_ngtcpport++;
				return g_ppc_nonhttp_tinfo;
			}
		}
		return NULL;
		
	case 0: /* don't yet know whether w3mux is in g_ppc_nonhttp_tinfo */
	default:
		for (i_index = 0; i_index < 8; i_index++) {
			if (!(g_ppc_nonhttp_tinfo[i_index])) { /* hit (null) end */
				s_i_using_mux = 2; /* not using mux */
				return get_ngtinfo(); /* now that we know recall ourselves */
			}
			if (strncmp("w3mux", g_ppc_nonhttp_tinfo[i_index], 5) == 0) {
				char* pc_mux_copy;
				char* pc_walker = (g_ppc_nonhttp_tinfo[i_index]) + 5;
				s_i_using_mux = 1; /* we're using w3mux */
				/* get the channel number to start with */
				/* walk to just past the underscore */
				while (*pc_walker && (*pc_walker) != '_') pc_walker++;
				if (*pc_walker) pc_walker++;
				if (!(*pc_walker)) { /* hmmm, no channel number present */
					/* let it default to g_us_ngmuxchannel (0) */
					fprintf (stderr, "Unspecified w3mux channel number - defaulting to %hu\n", g_us_ngmuxchannel);
				}
				else {
					char* pc_endpoint_walker;
					if (sscanf(pc_walker, "%hu", &g_us_ngmuxchannel) != 1) {
						fprintf (stderr, "Invalid w3mux channel number - defaulting to %hu\n", g_us_ngmuxchannel);
					}
					/* see if there's an endpoint specified */
					pc_endpoint_walker = pc_walker;
					while (*pc_endpoint_walker && (*pc_endpoint_walker) != '_') pc_endpoint_walker++;
					if (*pc_endpoint_walker) pc_endpoint_walker++;
					if ((*pc_endpoint_walker))  /* channel number present */
						s_pc_endpoint_start = pc_endpoint_walker;
				}
				/* now make a copy to put into the tinfo that we can change the channel number in,
				and save a pointer to where the channel number begins */
				pc_mux_copy = ilu_malloc(128);
				strcpy(pc_mux_copy, g_ppc_nonhttp_tinfo[i_index]);
				s_pc_channel_start = pc_mux_copy + (pc_walker - g_ppc_nonhttp_tinfo[i_index]);
				g_ppc_nonhttp_tinfo[i_index] = pc_mux_copy;
				return get_ngtinfo(); /* now that we know recall ourselves */
			}
		}
		/* hit the end! must not be using mux*/
		s_i_using_mux = 2;
		return get_ngtinfo(); /* now that we know recall ourselves */
	}
}


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
/* (not very smart) routine like strtokr only or getting 
url's from a http entity body.  Returns an allocated char*
holding the full url of the child.                       */

char* get_next_url_in_body(CORBA_octet** pc_where_to_start, CORBA_octet* p_body,
						   char* pc_parent_url) {
	CORBA_octet* p_walker;
	fprintf(stderr, "Warning: get_next_url_in_body not yet implemented!\n");
	
	if (!pc_where_to_start || !p_body) 
		return NULL;

	if (*pc_where_to_start == NULL)/* must be first entry */
		*pc_where_to_start = p_body;
	
	p_walker = p_body;
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

int do_get_work (iluhttp_Resource http_obj, iluhttp_Request* p_http_req,
				 int i_repeats, int i_depth) {
	
	CORBA_Environment ilu_env;					/* used to get potential error info */
	iluhttp_Response* p_http_resp;				/* will hold the response returned from http__obj */
	int i_count;
	ilu_FineTime  timebegin, timeend, timeinterval;
	ilu_FineTime  timeaccumulated = {0};
	StatSums timesums = {0};
	
	/* make any logfile entries */
	LOG_ENTER();
	if (DO_LOG_BASIC()) 	
		nglib_print_iluhttp_request(p_http_req, g_p_logfile);
	
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
			if (nglib_show_and_clear_if_exception(&ilu_env, p_http_req->URI, g_p_logfile)) {
				LOG_EXIT();
				return -1;
			}
		}
		else
			if (nglib_show_and_clear_if_exception(&ilu_env, p_http_req->URI, stderr)) {
				LOG_EXIT();
				return -1;
			}
		
		if (DO_LOG_FULL())
			nglib_print_iluhttp_response(p_http_resp, 1, g_p_logfile );
		else if (DO_LOG_BASIC())
			nglib_print_iluhttp_response(p_http_resp, 0, g_p_logfile );
		
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
		pc_childurl = get_next_url_in_body(&pc_where_to_start, p_http_resp->body->_buffer, p_http_req->URI);
		if (!g_b_multithreaded) {
			while (pc_childurl) {
				do_get(pc_childurl, i_repeats, i_depth - 1);
				free(pc_childurl);
				get_next_url_in_body(&pc_where_to_start, p_http_resp->body->_buffer, p_http_req->URI);
			}
		}
#ifdef ILU_OS_THREADED
		else { /* doing this in a threaded manner (repeats should always be one here) */
			while (pc_childurl) {
				for (i_count = 0; i_count < i_repeats; i_count++)
					fork_do_threaded_get(pc_childurl, i_depth - 1);
				free(pc_childurl);
				get_next_url_in_body(&pc_where_to_start, p_http_resp->body->_buffer, p_http_req->URI);
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
/* make Get calls on a Renderable object                     */
/* when called in the thread for each request situation, 
   i_repeats should always be 1 
	returns -1 on a problem, else 1
To make this function useful for actually retrieving a rendering:
When pp_where_to_put_rendering is non NULL, and we are NOT doing
a stream style retrieval, and depth and repeats are both 1, 
a pointer to the retrieved NgRendering_Rendering is put into 
*pp_where_to_put_rendering, and the caller assumes ownership of it 
*/

int do_ngget_work (	NgDocument_WebDocument ngdocument, NgRendering_RenderingPreferences* p_ng_prefs,
				   char* pc_url, int i_repeats, int i_depth, NgRendering_Rendering** pp_where_to_put_rendering) {
	
	CORBA_Environment ilu_env;					/* used to get potential error info */
	NgRendering_Rendering* p_rendering;			/* will hold the rendering returned from ngdocument */
	int i_count;
	ilu_FineTime  timebegin, timeend, timeinterval;
	ilu_FineTime  timeaccumulated = {0};
	StatSums timesums = {0};

	if (pp_where_to_put_rendering) 
		*pp_where_to_put_rendering = NULL;

	/* make any logfile entries */
	LOG_ENTER();
	if (DO_LOG_BASIC()) 	
		nglib_print_ngrendering_preferences(p_ng_prefs, g_p_logfile);
	
	if (DO_LOG_TIMING()) {
		fprintf (g_p_logfile, "Start %d %s on %s\n", i_repeats,  
			g_b_rendersink ? "SendRenderings" : "GetRenderings", pc_url);
	}
	LOG_EXIT();
	
	/* do the Get i_repeats times */	
	for (i_count = 0; i_count < i_repeats; i_count++) {
		
		timebegin = ilu_FineTime_Now(); /* get start time */
		
		if (g_b_rendersink) {
		/* xxx problem here in that the time measured will also include the logging
			time that occurs */
			
			char* pc_target_server_id;
			char* pc_target_object_id;
			RenderingSinkObjectData* p_sink_data;
			NgRendering_RenderingSink our_sink;
			ilu_Error an_error;
			ilu_cardinal card_chunk_size = NGGETBOT_DEFAULT_CHUNK_SIZE;
			
			/* get a hold of the instance ID */
			if (!ILU_C_IDOfObject( ngdocument, &pc_target_server_id, &pc_target_object_id)) {
				fprintf(stderr, "ILU_C_IDOfObject error\n");
			}				
			
			ilu_free(pc_target_server_id);  /* don't need this anymore */
			
			/* make a Rendering Sink */
			p_sink_data = MallocRenderingSinkObjectData(pc_target_object_id, timebegin);
			
			our_sink = NgRendering_RenderingSink__CreateTrue (NULL, g_stream_server, p_sink_data);
			
			NgRendering_Renderable_SendRendering(ngdocument, p_ng_prefs, NULL, 
				our_sink, &card_chunk_size, &ilu_env);

/*			NgDocument_WebDocument_SendRendering(ngdocument, p_ng_prefs, NULL, 
				our_sink, &card_chunk_size, &ilu_env);
				*/
			
			/* do conditional wait here for sink to finish */
			ilu_AcquireMutex(p_sink_data->m_access_mutex);
			ilu_CMWait1(p_sink_data->m_done_condition, p_sink_data->m_access_mutex, &an_error);
			if (ILU_ERRNOK(an_error)) {
				fprintf(stderr, "Error calling ilu_CMWait1 on p_sink_data->m_access_mutex\n");
				ILU_HANDLED(an_error);
				ILU_CLER(an_error);
				ilu_ReleaseMutex(p_sink_data->m_access_mutex);
				return -1;
			}
			ilu_ReleaseMutex(p_sink_data->m_access_mutex);
			
			/* we can now get rid of the sink */
			CORBA_Object_release(our_sink, &ilu_env);
			if (DO_LOG_BASIC()) {
				LOG_ENTER();
				nglib_show_and_clear_if_exception(&ilu_env, "CORBA_Object_release problem", g_p_logfile);
				LOG_EXIT();
			}			
			ilu_free(p_sink_data);
		} /* end SendRendering Work */

		else {
			/* xxx ignoring caching control here */
			/* call the GetRendering method on the ngdocument */
			p_rendering = NgRendering_Renderable_GetRendering (ngdocument, p_ng_prefs, NULL, NULL, &ilu_env);
		}
		
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
			if (nglib_show_and_clear_if_exception(&ilu_env, pc_url, g_p_logfile)) {
				LOG_EXIT();
				return -1;
			}
		}
		else if (nglib_show_and_clear_if_exception(&ilu_env, pc_url, stderr)) {
			LOG_EXIT();
			return -1;
		}
		
		if (!g_b_rendersink) {	/* renderSink does this itself */		
			if (DO_LOG_FULL())
				nglib_print_ngrendering_rendering(p_rendering, 1, g_p_logfile );
			else if (DO_LOG_BASIC())
				nglib_print_ngrendering_rendering(p_rendering, 0, g_p_logfile );
		}
		
		LOG_EXIT();
		
		if (!g_b_rendersink) {	/* renderSink doesn't currently piece together the body */		
			/* write the rendering bytes if there are some to the body file if it's set up */
			if (g_p_body_file && p_rendering->renderingBytes._length != 0) {
				if (g_body_file_mutex) ilu_AcquireMutex(g_body_file_mutex);
				
				fwrite(p_rendering->renderingBytes._buffer, sizeof(CORBA_octet), 
					p_rendering->renderingBytes._length, g_p_body_file);
				if (g_body_file_mutex) ilu_ReleaseMutex(g_body_file_mutex);
			}
			
			if (pp_where_to_put_rendering && i_repeats == 1 && i_depth == 1) {
				/* if we're supposed to save the rendering */
				*pp_where_to_put_rendering = p_rendering;
			}
			else {
				
			/* free renderings storage unless it's the last time (when we'll want to 
				scan the body for other child URLs to retrieve when depth is > 0 ) */
				if (i_count != i_repeats - 1) {
					NgRendering_Rendering__Free (p_rendering);
					ilu_free(p_rendering);
				}	
			}
		}
	}
	
	
	/* make any logfile entries */
	if (DO_LOG_TIMING()) {
		LOG_ENTER();
		fprintf (g_p_logfile, "End %d Requests on %s\n", 
			i_repeats, g_b_rendersink ? "SendRendering" : "GetRendering", pc_url);
		fprintf (g_p_logfile, "\tElapsed time, total:  %.2f seconds\n",
			((double) timeaccumulated.ft_s * ilu_FineTimeRate + timeaccumulated.ft_t) / ilu_FineTimeRate);
		fprintf(g_p_logfile, "\t%s\n", FmtStats(&timesums));
		LOG_EXIT();
	}
	
	/* if we're supposed to do Gets on the bodies child urls xxx depth not actually implemented
	since we don't have a working function to parse any body content */
	if (i_depth > 0 && 
		p_rendering->renderingBytes._buffer &&
		strcmp(p_rendering->contentType, "text/html") == 0) {
		char* pc_childurl;
		CORBA_octet* pc_where_to_start = NULL;
		pc_childurl = get_next_url_in_body(&pc_where_to_start, p_rendering->renderingBytes._buffer, pc_url);
		if (!g_b_multithreaded) {
			while (pc_childurl) {
				do_get(pc_childurl, i_repeats, i_depth - 1);
				free(pc_childurl);
				get_next_url_in_body(&pc_where_to_start, p_rendering->renderingBytes._buffer, pc_url);
			}
		}
#ifdef ILU_OS_THREADED
		else { /* doing this in a threaded manner (repeats should always be one here) */
			while (pc_childurl) {
				for (i_count = 0; i_count < i_repeats; i_count++)
					fork_do_threaded_get(pc_childurl, i_depth - 1);
				free(pc_childurl);
				get_next_url_in_body(&pc_where_to_start, p_rendering->renderingBytes._buffer, pc_url);
			}
		}
#endif /* ILU_OS_THREADED */
	}
	
	if (!g_b_rendersink) {
		if (!(pp_where_to_put_rendering && i_repeats == 1 && i_depth == 1)) {
			/* if we're NOT supposed to save the rendering */
			/* free the saved rendering storage*/
			NgRendering_Rendering__Free (p_rendering);
			ilu_free(p_rendering);
		}
	}
	
	return 1;
	
}

/* if the object denoted by pc_url is a NgDocument_WebDocument, it retrieves the rendering
from it, (putting it in *pp_where_to_put_rendering if i_repeats == 1 and i_depth == 1 and
we're not using the sink approach) and returns 1 if successful, else -1, 
if the object denoted by pc_url is NOT a NgDocument_WebDocument it returns 0 */

int try_ngget(char* pc_url, int i_repeats, int i_depth,  
			  NgRendering_Rendering** pp_where_to_put_rendering) {

	CORBA_Environment ilu_env;					/* used to get potential error info */
	NgDocument_WebDocument ngdocument;			/* can be an NgDocument_WebDocument */
	
	/* try to get a NgDocument_WebDocument object to represent pc_url by parsing the pc_url */
	if ((ngdocument = ILU_C_SBHToObject (pc_url, _NgDocument_WebDocument__ILUType, &ilu_env)) != ILU_NIL){
		/* Rendering preferences to pass to httpng renderable objects */
		NgRendering_RenderingPreferences ng_prefs;
		int i_result;
		nglib_NgRendering_RenderingPreferences_clean_assign(&ng_prefs);
		
		/* Make up some simple preferences for use with ng renderable objects */
		/* we assume everything we deal with US_ASCII, and en locale */
		IANA_Charsets_Registry_CharsetMIBEnumValueSequence_Push(&(ng_prefs.acceptCharsets), 
			IANA_Charsets_Registry_US_ASCII); 			
		
		NgBasic_StringSequence_Push(&(ng_prefs.acceptLocales), nglib_duplicate_c_string("en"));
		
		ng_prefs.userAgent = nglib_duplicate_c_string("NGGetBot/1.0");
		
		NgBasic_URISequence_Push(&(ng_prefs.allowContentTypes), nglib_duplicate_c_string("*/*"));

		if (g_b_serial) 
			ILU_C_SetSerializationContext(nglib_get_object_serializer(ngdocument));

		i_result = do_ngget_work(ngdocument, &ng_prefs, pc_url, i_repeats, i_depth, pp_where_to_put_rendering);

		if (g_b_serial) {
			ILU_C_SetSerializationContext(NULL);
			nglib_release_object_serializer(ngdocument);
		}
		else 
			nglib_rejuvenate_objects_server(ngdocument);

		/* no longer need this surrogate, cleanup */
		CORBA_Object_release(ngdocument, &ilu_env);
		if (DO_LOG_BASIC()) {
			LOG_ENTER();
			nglib_show_and_clear_if_exception(&ilu_env, "CORBA_Object_release problem", g_p_logfile);
			LOG_EXIT();
		}
		NgRendering_RenderingPreferences__Free(&ng_prefs);
		
		return i_result;
	}
	else return 0;
}


/* ********************************************************* */
/* make the calls on a Resource or renderable object         */

int do_get (char* pc_url, int i_repeats, int i_depth) {
	
	CORBA_Environment ilu_env;					/* used to get potential error info */
	iluhttp_Resource http_obj;					/* can be an 'object' from an existing httpd */
	int i_result;
	
	if (i_result = try_ngget(pc_url, i_repeats, i_depth, NULL)) {
		/* must have been an NgDocument_WebDocument */		
		return i_result;
	}
	else 
		/* try to get an iluhttp.Resource object to represent pc_url by parsing the pc_url */
		if ((http_obj = ILU_C_SBHToObject (pc_url, iluhttp_Resource__MSType, &ilu_env)) != ILU_NIL) {

			iluhttp_Request http_req;	
			/* make a simple http_req for use with iluhttp.recource calls */
			http_req.headers._maximum = 3;						
			http_req.headers._length = 3;
			http_req.headers._buffer = (iluhttp_HTTPHeader*) ilu_malloc ( http_req.headers._maximum * 
				sizeof(iluhttp_HTTPHeader));
			
			http_req.headers._buffer[0].name = "User-Agent";
			http_req.headers._buffer[0].value = "NGGetBot/1.0";
			
			http_req.headers._buffer[1].name = "Date";
			http_req.headers._buffer[1].value = get_time_string(1);
			
			http_req.headers._buffer[2].name = "Host";
			http_req.headers._buffer[2].value = g_pc_hostname;
			
			/* used for testing only to force use of a particular proxy server */
			/* PUTENV_FUNCTION("ILU_HTTP_PROXY_INFO=wwwproxy.parc.xerox.com:8000"); */
			
			http_req.body = ILU_NIL; /* no body */
			
			/* note that ilu's http will put in Content-Length header if an
			Entity body is supplied, and we haven't put in our own content length header */
			
			http_req.URI = pc_url;	/* assign the the URI in the request */ 
			
			if (g_b_serial) 
				ILU_C_SetSerializationContext(nglib_get_object_serializer(http_obj));
			
			i_result = do_get_work(http_obj, &http_req, i_repeats, i_depth);
			
			if (g_b_serial) {
				ILU_C_SetSerializationContext(NULL);
				nglib_release_object_serializer(http_obj);
			}
			else 
				nglib_rejuvenate_objects_server(http_obj);

			free(http_req.headers._buffer[1].value); /* free time string */

			/* no longer need this surrogate, cleanup */
			CORBA_Object_release(http_obj, &ilu_env);
			if (DO_LOG_BASIC()) {
				LOG_ENTER();
				nglib_show_and_clear_if_exception(&ilu_env, "CORBA_Object_release problem", g_p_logfile);
				LOG_EXIT();
			}
			ilu_free(http_req.headers._buffer);
			
			return i_result;
		}
		
		if (DO_LOG_BASIC()) {
			LOG_ENTER();
			fprintf (g_p_logfile, "Error: Can't obtain any known object types from %s\n", pc_url);
			LOG_EXIT();
		}
		
		return -1;	
}


#ifdef ILU_OS_THREADED

/* sets up or takes down the default pipeline batching and passport */
void set_default_pipeline_batching_and_passport (int i_on_or_off) {

	if (i_on_or_off) {
		if (g_b_pipeline && g_pipeline) 
			ILU_C_SetPipelineContext(g_pipeline);
		
		if (g_b_batching && g_batcher) 
			ILU_C_SetBatcherContext(g_batcher);

		if (g_passport) 
			ILU_C_SetPassportContext(g_passport);
	}
	else {
		if (g_b_pipeline) 
			ILU_C_SetPipelineContext(NULL); 
		
		if (g_batcher) 
			ILU_C_SetBatcherContext(NULL);

		if (g_passport) 
			ILU_C_SetPassportContext(NULL);
	}
}


/* ********************************************************* */
/* called when we're doing gets in a threaded manner, with a 
new thread for each request - does 1 get */

void do_threaded_get (forked_get_args* args) {
	
    char* pc_url = args->m_pc_url;
	int i_depth = args->m_i_depth;
	ilu_Error an_error;
	
	free(args);
	
	set_default_pipeline_batching_and_passport(1);
	
	do_get(pc_url, 1, i_depth);
	
	set_default_pipeline_batching_and_passport(0);
	
	/* free url  */
	free(pc_url);
	
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
/* forks a thread doing a threaded get - used when we create a
new thread for every request  */

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

	if (!OSForkNewThread ((nglib_fork_procedure)do_threaded_get, p_get_args, &an_error)) {
		fprintf(stderr, "fork_do_threaded_get - Couldn't fork thread\n");
		ILU_HANDLED(an_error);
		ILU_CLER(an_error);
	}
	
}

/* processes the urls in the url file */
static void process_url_file() {
	
	/* grab and get urls in the file */
	char c_url_to_get[512];
	int i_fscanresult;
	int i_milliseconds_delay;
	int b_processing = 1;
	int i_count;
	
	do {
		if (g_urlfile_mutex) ilu_AcquireMutex(g_urlfile_mutex);
		i_fscanresult = fscanf(g_p_urlfile, "%s %d\n", &c_url_to_get, &i_milliseconds_delay);
		
		if (i_fscanresult != 2) {
			if (g_urlfile_mutex) ilu_ReleaseMutex(g_urlfile_mutex);	
		}
		else { /* a delay was specified */
			if (i_milliseconds_delay == 0) {
				/* treat 0 delay as 'just a url' */
				if (g_urlfile_mutex) ilu_ReleaseMutex(g_urlfile_mutex);	
				i_fscanresult = 1;
			}
		}
			
			switch (i_fscanresult) {
			case 1: /* just a url */
				if (c_url_to_get[0] != '#')  /* skip comment lines */
#ifdef THREAD_FOR_EVERY_REQUEST
					if (!g_b_multithreaded)
						do_get(c_url_to_get, g_i_repeats, g_i_depth);
#ifdef ILU_OS_THREADED
					else 
						for (i_count = 0; i_count < g_i_repeats; i_count++)
							fork_do_threaded_get(c_url_to_get, g_i_depth);
#endif /* ILU_OS_THREADED */
#else  /* THREAD_POOL */
						do_get(c_url_to_get, g_i_repeats, g_i_depth);
#endif
						break;
			case 2:
				if (c_url_to_get[0] != '#') { /* skip comment lines */
					/* must have a url and a timeout */
#ifdef THREAD_FOR_EVERY_REQUEST
					if (!g_b_multithreaded)
						do_get(c_url_to_get, g_i_repeats, g_i_depth);
#ifdef ILU_OS_THREADED
					else 
						for (i_count = 0; i_count < g_i_repeats; i_count++)
							fork_do_threaded_get(c_url_to_get, g_i_depth);
#endif /* ILU_OS_THREADED */
#else  /* THREAD_POOL */
						do_get(c_url_to_get, g_i_repeats, g_i_depth);
#endif
					/* release the g_urlfile_mutex after the specified number of milliseconds */
					MILLISEC_SLEEP_FUNCTION(i_milliseconds_delay);
				}
				if (g_urlfile_mutex) ilu_ReleaseMutex(g_urlfile_mutex);	
				break;
			case EOF:
				b_processing = 0;
				break;
			default:
				b_processing = 0;
				fprintf (stderr, "Problem reading from %s\n", g_pc_urlfilename);
				break;
				
			}
	}
	while (b_processing);
}


#ifdef THREAD_POOL

/* used when we have a fixed pool of worker threads */
void worker_thread (void* worker_thread_args) {
	
	ilu_Error an_error;

	/* set the pipeline and batching for this thread as appropriate */
	set_default_pipeline_batching_and_passport(1);

	process_url_file();
		
	set_default_pipeline_batching_and_passport(0);
	
	/* decrement the thread count */
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
}
#endif /* THREAD_POOL */

#endif /* ILU_OS_THREADED */



/* ********************************************************* */
/* ********************************************************* */
/* startup and cleanup functions                             */


/* ********************************************************* */
/* Usage string                                              */

char g_c_usage[] = 
"Usage:  nggetbot -url url | -urls urlFilename  [-mt maxthreads] [-v level]\n\
\t[-r repeat] [-pipeline] [-batch period] [-serial] [-httppinfo (http_1_0 | http_1_0p | http_1_1)]\n\
\t[-httptinfo tinfo] [-file bodyfilename] [-ngpinfo pinfo] [-ngtinfo tinfo] [-ngport portnum]\n\
\t[-filebase pathname] [-suffix serversuffix] [-fdbudget budget] [-boost] [-logfile logfilename] [-sink]\n\
Note: For Http_1_1 Pipelining behavior, use -pipeline, -serial, and -batch\n\n\
\t-url        url of object to do Get on\n\
\t-urls       name of file containing URL's (one per line) to be retrieved (can be stdin)\n\
\t-mt         run ILU multi threaded (default single)\n\
\t            maxthreads = max number of worker threads, 0 = No Limit when compiled thread per every request\n\
\t-pipeline   use ILU Pipeline - implies -mt\n\
\t-batch      use ILU Batching, period in microseconds\n\
\t-serial     use ILU Serializer - implies -mt\n\
\t-v level    verbosity level (default is 0), 0 = no output, 1 = shows summary timing,\n\
\t            2 = basic timing, 3 = basic timing, plus requests & response headers\n\
\t            4 = basic timing, plus requests & complete response\n\
\t-r repeat   how many times to Get each URL (default 1)\n\
\t-httppinfo  http version, one of http_1_0 or http_1_0p or http_1_1 - default http_1_1\n\
\t-httptinfo  transport for http ports - default tcp_0_80\n\
\t-file       filename where body from Get should be written (can be stdout)\n\
\t-logfile    name of logfile where verbose output goes (can be stdout)\n\
\t-ngpinfo    protocol for non http ports - default w3ng_1.0\n\
\t-ngtinfo    transports over tcp for non http ports - default w3mux_16_NGGetbotEndpoint\n\
\t-ngport     tcp port to start making ng servers with, default 2728\n\
\t-suffix     string used as suffix to ILU server names - default .parc.xerox.com\n\
\t-fdbudget   ILU file descriptor budget - defaults to ILU's default\n\
\t-boost      Boosts nggetbot worker thread pool priorities (Win32 only)\n\
\t-sink       Use a RenderingSink to asynchronously receive NG Renderings\n";

/* 
we currently don't have a parser to look for embedded links
 [-d depth]
\t-d depth    Not Yet implemented - for each URL, get children to a depth (default 0)\n\
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
		nglib_show_and_clear_if_exception(&ilu_env, "calling ILU_C_ReleasePipeline", stderr);
	}

	if (g_batcher) {
		ILU_C_SetBatcherContext(NULL);
		ILU_C_ReleaseBatcher(g_batcher, &ilu_env);
		nglib_show_and_clear_if_exception(&ilu_env, "calling ILU_C_ReleaseBatcher", stderr);
	}

	if (g_passport) {
		ILU_C_SetPassportContext(NULL);
		ILU_C_DestroyPassport(g_passport, &ilu_env);
		nglib_show_and_clear_if_exception(&ilu_env, "calling ILU_C_DestroyPassport", stderr);
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

	if (g_send_to_first_byte_mutex) {
		ilu_DestroyMutex(g_send_to_first_byte_mutex, &an_error);
		if (ILU_ERRNOK(an_error)) {
			fprintf(stderr, "Error destroying g_send_to_first_byte_mutex mutex\n");
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

	nglib_shutdown();
	
}



/* ********************************************************* */
/* process command line                                      */

int process_command_line_and_setup(int i_argcount, char** ppc_argvalue) {
	
	int i_argindex = 1;				/* for walking through command line arguments */
	CORBA_Environment ilu_env;			/* used to get potential error info */
	ilu_Error an_error;
	int i_temp;
	extern void ngstream_impl_setup_server();
	
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
				(sscanf(ppc_argvalue[i_argindex], "%i", &g_i_max_number_of_get_threads) != 1)
#ifdef THREAD_POOL
#ifndef SURGE_HTTPNG_BUILD
				|| (g_i_max_number_of_get_threads <= 0)
#endif
#endif				
				
				) {
				fprintf(stderr, "Invalid maximum threads %s\n", ppc_argvalue[i_argindex]);
				return -1;
			}
			goto nextarg;
#endif				/* ILU_OS_THREADED */
		} 
		
		/* check for pipeline use */
		if (strcmp(ppc_argvalue[i_argindex], "-pipeline") == 0) {
			g_b_pipeline = 1;
			goto nextarg;
		} 
		
		/* check for thread priority boost */
		if (strcmp(ppc_argvalue[i_argindex], "-boost") == 0) {
			g_b_thread_boost = 1;
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
		
		/* check for http version to use */
		if (strcmp(ppc_argvalue[i_argindex], "-httppinfo") == 0) {
			i_argindex++;
			if (i_argindex >= i_argcount) {
				fprintf(stderr, "No http version provided\n");
				return -1;
			}
			if ((strcmp(ppc_argvalue[i_argindex], "http_1_0") != 0 ) &&
				(strcmp(ppc_argvalue[i_argindex], "http_1_0p") != 0 ) &&
				(strcmp(ppc_argvalue[i_argindex], "http_1_1") != 0 )) {
				fprintf(stderr, "Invalid httppinfo provided\n");
				return -1;
			}
			g_pc_http_pinfo = ppc_argvalue[i_argindex];
			goto nextarg;
		} 
		
		/* check for http tinfo to use */
		if (strcmp(ppc_argvalue[i_argindex], "-httptinfo") == 0) {
			int i_tinfo_index = 0;
			i_argindex++;
			if (i_argindex >= i_argcount) {
				fprintf(stderr, "No http tinfo provided\n");
				return -1;
			}
			while (i_argindex < i_argcount &&
				strncmp(ppc_argvalue[i_argindex], "-", 1) != 0) {
				g_ppc_http_tinfo[i_tinfo_index] = ppc_argvalue[i_argindex];
				i_tinfo_index++;
				i_argindex++;
			}
			g_ppc_http_tinfo[i_tinfo_index] = ILU_NIL;
			i_argindex--; /* backup - we hit another option */
			goto nextarg;
		} 		
		/* check for ng protocol to use */
		if (strcmp(ppc_argvalue[i_argindex], "-ngpinfo") == 0) {
			i_argindex++;
			if (i_argindex >= i_argcount) {
				fprintf(stderr, "No ng pinfo provided\n");
				return -1;
			}
			g_pc_nonhttp_pinfo = ppc_argvalue[i_argindex];
			goto nextarg;
		} 
		
		/* check for ng tinfo to use */
		if (strcmp(ppc_argvalue[i_argindex], "-ngtinfo") == 0) {
			int i_tinfo_index = 0;
			i_argindex++;
			if (i_argindex >= i_argcount) {
				fprintf(stderr, "No ng tinfo provided\n");
				return -1;
			}
			while (i_argindex < i_argcount &&
				strncmp(ppc_argvalue[i_argindex], "-", 1) != 0) {
				g_ppc_nonhttp_tinfo[i_tinfo_index] = ppc_argvalue[i_argindex];
				i_tinfo_index++;
				i_argindex++;
			}
			g_ppc_nonhttp_tinfo[i_tinfo_index] = ILU_NIL;
			i_argindex--; /* backup - we hit another option */
			goto nextarg;
		} 

		/* check for ngport  */
		if (strcmp(ppc_argvalue[i_argindex], "-ngport") == 0) {
			i_argindex++;
			if (i_argindex >= i_argcount ||
				(sscanf(ppc_argvalue[i_argindex], "%hu", &g_us_ngtcpport) != 1)) {
				fprintf(stderr, "Invalid ngport %s\n", ppc_argvalue[i_argindex]);
				return -1;
			}
			goto nextarg;
		} 
		
		/* check for server suffix */
		if (strcmp(ppc_argvalue[i_argindex], "-suffix") == 0) {
			i_argindex++;
			if (i_argindex >= i_argcount) {
				fprintf(stderr, "No suffix provided\n");
				return -1;
			}
			g_pc_nggetbot_suffix = ppc_argvalue[i_argindex];
			goto nextarg;
		} 
		
		/* check for fdbudget settings */
		if (strcmp(ppc_argvalue[i_argindex], "-fdbudget") == 0) {
			i_argindex++;
			if (i_argindex >= i_argcount ||
				(sscanf(ppc_argvalue[i_argindex], "%i", &g_i_fd_budget) != 1)) {
				fprintf(stderr, "Invalid fdbudget %s\n", ppc_argvalue[i_argindex]);
				return -1;
			}
			goto nextarg;
		} 

		/* check for RenderingSink use */
		if (strcmp(ppc_argvalue[i_argindex], "-sink") == 0) {
			g_b_rendersink = 1;
			goto nextarg;
		} 

		/* ignore any args that are empty stings */
		if (strcmp(ppc_argvalue[i_argindex], "") == 0)
			goto nextarg;

		/* unknown command line argument */
		fprintf (stderr, "Invalid argument %s\n%s\n", ppc_argvalue[i_argindex], g_c_usage);
		return -1;
		
nextarg:
		/* advance to the next command line argument */
		i_argindex++;
	}
	
#ifndef SURGE_HTTPNG_BUILD
	if (!g_pc_url_to_get && !g_pc_urlfilename) {
		fprintf (stderr, "Nothing to get!\n%s\n", g_c_usage);
		return -1;
	}
#endif
	
	if((g_b_pipeline || g_b_serial) && !g_b_multithreaded) {
		fprintf (stderr, "Need to specify -mt with either -pipeline or -serial\n");
		return -1;
	}
	
	/* setup default http version to use */
	ilu_http_default_scheme = g_pc_http_pinfo;
	
	/* force 1.0 connection closure for non iluhttp.resource objects accessed
	   with http_1_0 */
	if (strcmp(g_pc_http_pinfo, "http_1_0") == 0)
	  ilu_http_force_close_on_ilu_1_0 = ilu_TRUE;
	
	/* assign fd budget if specified */
	if (g_i_fd_budget > 0)
		g_i_fd_budget = ilu_SetFDBudget((ilu_cardinal)g_i_fd_budget);
	else if (g_b_multithreaded) {
	/* ensure our fdbudget is boosted appropriately for the number of threads 
		-- just some 'guestimation' here. If we run out we'll get resource exceptions */
	       g_i_fd_budget = ilu_GetFDBudget() + 
			   (2 * (g_i_max_number_of_get_threads ? g_i_max_number_of_get_threads : 64)) + 16;
		   g_i_fd_budget = ilu_SetFDBudget(g_i_fd_budget);
	}
	else g_i_fd_budget = ilu_GetFDBudget();
	
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
		if (g_b_rendersink)
			g_send_to_first_byte_mutex = ilu_CreateMutex("ilugetbot", "send_to_first_byte");
		if (ILU_ERRNOK(an_error)) {
			fprintf(stderr, "Error Creating get_thread_count condition\n");
			ILU_HANDLED(an_error);
			ILU_CLER(an_error);
		}
	}
#endif /* ILU_OS_THREADED */
	
	/* set up pipeline if requested */
	if (g_b_pipeline) {
		g_pipeline = ILU_C_CreatePipeline(&ilu_env);
		if (!nglib_show_and_clear_if_exception(&ilu_env, "calling ILU_C_CreatePipeline", stderr))
			ILU_C_SetPipelineContext(g_pipeline);
	}

	/* set up batcher if requested */
	if (g_b_batching) {
		ilu_FineTime batching_time;
		batching_time = ilu_FineTime_FromDouble(((double)g_ui_batching_period) / 1000000.0);
		g_batcher = ILU_C_CreateBatcher(batching_time, ilu_FALSE, &ilu_env);
		if (!nglib_show_and_clear_if_exception(&ilu_env, "calling ILU_C_CreateBatcher", stderr))
			ILU_C_SetBatcherContext(g_batcher);
	}
		
	/* if w3mux is being used, set up callback identity */
	for (i_argindex = 0; g_ppc_nonhttp_tinfo[i_argindex]; i_argindex++) {
	  if (sscanf(g_ppc_nonhttp_tinfo[i_argindex], "w3mux_%d_%s", &i_temp, g_pc_w3mux_Endpoint_Name) == 2)
		  break;
	}
	if (g_pc_w3mux_Endpoint_Name[0]) {

		ilu_IdentityInfo endpoint;
		
		if (!(g_passport = ILU_C_GetPassportContext())) {
			g_passport = ILU_C_CreatePassport (&ilu_env);
			
			if (nglib_show_and_clear_if_exception(&ilu_env, "Can't create passport", stderr))
				goto passport_over;
			
			else ILU_C_SetPassportContext (g_passport);
		}
		
		endpoint = ILU_C_AcquireW3muxEndpointIdentity (g_pc_w3mux_Endpoint_Name, &ilu_env);
		
		if (nglib_show_and_clear_if_exception(&ilu_env, "Can't acquire identity for default endpoint UUID", stderr))
			goto passport_over;
		
		ILU_C_AddIdentity (g_passport, endpoint, &ilu_env);

		nglib_show_and_clear_if_exception(&ilu_env, "Can't add endpoint identity to passport", stderr);
	}

passport_over:

	/* get our hostname */
	gethostname(g_pc_hostname, sizeof(g_pc_hostname));

	/* initialize the ng library */
	nglib_init();
	
	/* initialize all the interfaces */
	NgBasic__Initialize();
	NgStream__Initialize();
	NgRendering__Initialize();
	iluhttp__Initialize();
	NgProperty__Initialize();
	NgFormProcessor__Initialize();
	NgDocument__Initialize();

	NgBasic__InitializeServer();
	NgStream__InitializeServer();
	NgRendering__InitializeServer();
	
	/* create object tables and ILU servers*/
	ngstream_impl_setup_server();
	
	if (!g_stream_server)
		return (-1);

	ilu_tcp_InitializeStats();          /* ensure tcp stats are zeroed */

#ifdef ILU_C_TIMING_STATISTICS
	/* clear stats counter, and enable statistics gathering */
	ILU_C_SetCallStats (ilu_TRUE, ilu_TRUE);
#endif /* def ILU_C_TIMING_STATISTICS */

	/* log startup info */
	if (DO_LOG_SUMMARY()) {
		char* pc_start_time;
		int i_walker = 0;
		LOG_ENTER();
		pc_start_time = nglib_get_current_time_string();
		fprintf(g_p_logfile, "nggetbot started on %s at %s\n", g_pc_hostname, pc_start_time);
#ifndef SURGE_HTTPNG_BUILD
		if (g_pc_url_to_get)
			fprintf(g_p_logfile, "\tretrieving %s\n", g_pc_url_to_get);
		else 
			fprintf(g_p_logfile, "\tretrieving URLs in %s\n", g_pc_urlfilename);
#endif
		fprintf(g_p_logfile, "\tpipeline  = %s\n", (g_b_pipeline ? "Yes" : "No"));
		fprintf(g_p_logfile, "\tserialize = %s\n", (g_b_serial ? "Yes" : "No"));
		
		fprintf(g_p_logfile, "\tbatching  = %s ", (g_b_batching ? "Yes" : "No"));
		if (g_b_batching)
			fprintf(g_p_logfile, "period = %u microseconds\n", g_ui_batching_period);
		else 
			fprintf(g_p_logfile, "\n");
		fprintf(g_p_logfile, "\thttppinfo = %s\n", g_pc_http_pinfo);
		fprintf(g_p_logfile, "\thttptinfo = " );
		while (g_ppc_http_tinfo[i_walker]) 
			fprintf(g_p_logfile, "%s ", g_ppc_http_tinfo[i_walker++]);
		i_walker = 0;
		fprintf(g_p_logfile, "\n\tngpinfo   = %s\n", g_pc_nonhttp_pinfo);
		fprintf(g_p_logfile, "\tngtinfo   = ");
		while (g_ppc_nonhttp_tinfo[i_walker]) 
			fprintf(g_p_logfile, "%s ", g_ppc_nonhttp_tinfo[i_walker++]);
		fprintf(g_p_logfile, "\n\tsuffix    = %s\n", g_pc_nggetbot_suffix);
		if (g_b_multithreaded)
			fprintf(g_p_logfile, "\tthreading = Multithreaded, max threads = %d\n", g_i_max_number_of_get_threads );
		else
			fprintf(g_p_logfile, "\tthreading = Singlethreaded\n");
		fprintf(g_p_logfile, "\tfdbudget  = %i\n", g_i_fd_budget);
		fprintf(g_p_logfile, "\tlogfile   = %s\n", (g_pc_logfilename ? g_pc_logfilename : "None"));
		fprintf(g_p_logfile, "\tfile      = %s\n", (g_pc_body_filename ? g_pc_body_filename : "None"));
		fprintf(g_p_logfile, "\tverbose   = %i\n", g_i_verbosity);		
		fprintf(g_p_logfile, "\tdepth     = %i\n", g_i_depth);		
		fprintf(g_p_logfile, "\trepeat    = %i\n", g_i_repeats);		
		fprintf(g_p_logfile, "-------------------------------------------------\n");		
		LOG_EXIT();
		free(pc_start_time);
	}
	
	return 0;
}



/* do not supply a main if we're being used as part of SURGE */

#ifndef SURGE_HTTPNG_BUILD

/* ********************************************************* */
/* MAIN                                                      */



int main(int i_argcount, char** ppc_argvalue) {

	int i_count;
	ilu_FineTime  timebegin, timeinterval;  /* vars for statistics */
	ilu_cardinal  card_bytes_sent;
	ilu_cardinal  card_bytes_received;
	ilu_cardinal  card_moorings_created;
	ilu_cardinal  card_connections_received;
	ilu_cardinal  card_connections_created;
	ilu_cardinal  card_current_connections;
	ilu_cardinal  card_max_connections;
	int           i_number_worker_threads;
	double        d_elapsed_time;
	int           i_linenumber = 1;
	
	
	/* PUTENV_FUNCTION("ILU_DEBUG=connection:lsr:packet:call");      debugging only */
	/* PUTENV_FUNCTION("ILU_DEBUG=most");    debugging only */
	
	/* setup from the command line */
	if (process_command_line_and_setup(i_argcount, ppc_argvalue) != 0) {
		cleanup();
		return -1;
	}	
	
	
#ifdef THREAD_POOL
	/* thread pool approach  - create a pool of worker threads if we're
	going to be reading urls from a file and we're to run multithreaded */
	if ((!g_pc_url_to_get) && g_b_multithreaded) { 
		ilu_Error an_error;
		
		/* if we're reading URLS from a file (instead of just getting a single URL) */
		/* first grab the file mutex so no threads can actually start working yet */
		if (g_urlfile_mutex) ilu_AcquireMutex(g_urlfile_mutex);
		
		/* start up the worker threads */
		for (i_number_worker_threads = 0; i_number_worker_threads < g_i_max_number_of_get_threads;
		i_number_worker_threads++){

			if (!OSForkNewThread ((nglib_fork_procedure)worker_thread, NULL, &an_error)) {
				fprintf(stderr, "Couldn't create worker thread\n");
				ILU_HANDLED(an_error);
				ILU_CLER(an_error);
				cleanup();
				return -1;
			}
		}
		g_i_number_of_get_threads = g_i_max_number_of_get_threads;
		timebegin = ilu_FineTime_Now(); /* get start time */
		if (g_urlfile_mutex) ilu_ReleaseMutex(g_urlfile_mutex); /* let the workers begin */
		goto wait_label;
	}
	else
		timebegin = ilu_FineTime_Now(); /* get start time */
#else
	timebegin = ilu_FineTime_Now(); /* get start time */
#endif
	
	
	/* do the actual Get(s) */
	
	if (g_pc_url_to_get) { 
		/* just a single url to get */
		if (!g_b_multithreaded) {
			/* just get it if we're single threaded */
			do_get(g_pc_url_to_get, g_i_repeats, g_i_depth);
			goto done_label;
		}
		/* otherwise we're multithreaded */
#ifdef ILU_OS_THREADED
#ifdef THREAD_FOR_EVERY_REQUEST
		/* we fork off a thread for every request */
		for (i_count = 0; i_count < g_i_repeats; i_count++) {
			fork_do_threaded_get(g_pc_url_to_get, g_i_depth);
		}
		goto wait_label;
#else
		/* use just one (this) thread */
		do_get(g_pc_url_to_get, g_i_repeats, g_i_depth);
		goto done_label;
		
#endif /* THREAD_FOR_EVERY_REQUEST */
#endif /* ILU_OS_THREADED */
	}
	
	
	/* if we get here, we need go through all the urls in the file */
	
	process_url_file();
	
#ifdef ILU_OS_THREADED
	/* wait for all threads to finish here */
wait_label:
	if (g_b_multithreaded) {
		ilu_Error an_error;
		
		ilu_AcquireMutex(g_get_thread_count_mutex);
		while(1) {
			if (g_i_number_of_get_threads > 0) {
				ilu_CMWait1(g_get_thread_count_condition, g_get_thread_count_mutex, &an_error);
				if (ILU_ERRNOK(an_error)) {
					fprintf(stderr, "Error calling ilu_CMWait1 on g_get_thread_count_condition - skipping thread\n");
					ILU_HANDLED(an_error);
					ILU_CLER(an_error);
					ilu_ReleaseMutex(g_get_thread_count_mutex);
					return -1;
				}
			}
			else break;
		}
		ilu_ReleaseMutex(g_get_thread_count_mutex);
	}
#endif /* ILU_OS_THREADED */
	
done_label:
	/* get end time, and interval */
	timeinterval = ilu_FineTime_Sub(g_last_get_end_time, timebegin);
	
	if (DO_LOG_SUMMARY()) {
		LOG_ENTER();
		ACCUM_TIME_ENTER();
		SEND_TIME_ENTER();
		
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

		if (g_b_rendersink) {
			fprintf (g_p_logfile, "Statistics for Send Rendering to First Rendering Byte Received Times:\n");
			fprintf(g_p_logfile, "\t%s\n", FmtStats(&g_send_to_first_byte_timesums));
		}

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


#if defined(ILU_C_TIMING_STATISTICS)
		{
			ILU_C_CallStats stats_total, stats_latency;
			ilu_cardinal card_nsync, card_nasync;
			ILU_C_GetCallStats (&card_nsync, &card_nasync, &stats_total, &stats_latency);
			fprintf (g_p_logfile, "C Runtime Timing stats:\n\tSynchronous calls %lu\n\tAsynchronous calls %lu\n",
				card_nsync, card_nasync);			
			fprintf (g_p_logfile, "\ttotal time: %s\n", FmtCRuntimeStats (&stats_total.total, stats_total.ncalls));
			fprintf (g_p_logfile, "\tuser time: %s\n", FmtCRuntimeStats (&stats_total.user, stats_total.ncalls));
			fprintf (g_p_logfile, "\tsystem time: %s\n", FmtCRuntimeStats (&stats_total.system, stats_total.ncalls));
			fprintf (g_p_logfile, "\tlatency: %s\n", FmtCRuntimeStats (&stats_latency.total, stats_latency.ncalls));
		}
#endif /* defined(ILU_C_TIMING_STATISTICS) */


		SEND_TIME_EXIT();		
		ACCUM_TIME_EXIT();
		LOG_EXIT();
	}
	
	
	/* end gracefully */
	cleanup();

	/* return how many exceptions occurred (as seen by nglib_show_and_clear_if_exception) */
	return nglib_exception_count();
}

#endif /* SURGE_HTTPNG_BUILD */

/* ********************************************************* */
/* End of file                                               */
/* ********************************************************* */
