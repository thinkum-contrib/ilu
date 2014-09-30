/** $Id: ngwebserver.c,v 1.17 1999/08/03 01:58:08 janssen Exp $
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
/* Last edited by Mike Spreitzer October 9, 1998 2:02 pm PDT */

/* very simple brute force web server program using the HTTP-NG interfaces

  Dan Larner, larner@parc.xerox.com
  4-25-98
  */

/* ********************************************************* */
/* includes and defines                                      */
/* ********************************************************* */

#include "nglib.h"

#ifndef WIN32
#include <signal.h>
#endif

#include <math.h>

#include "ngwebserver.h"

/* so we can change the default http scheme */
ILU_PUBLIC ilu_string ilu_http_default_scheme;

/* so we can force connection close behavior when using http 1.0 on 
   non iluhttp.Resource objects */
ILU_PUBLIC ilu_boolean ilu_http_force_close_on_ilu_1_0;


/* ********************************************************* */
/* some globals                                              */
/* ********************************************************* */


/* level of output info */
int g_i_verbosity = LOG_NOTHING;		

/* base location of files */
char* g_pc_file_base = DEFAULT_FILE_BASE;


/* string used as suffix to our ilu server names */
ilu_string g_pc_ngwebserver_suffix = XEROX_PARC_DOMAIN;  


/* default contact info to export iluhttp_Resource objects with */
char* g_pc_http_pinfo = "http_1_1";
char* g_ppc_http_tinfo [8] = 
{ "tcp_0_80", ILU_NIL, ILU_NIL, ILU_NIL, ILU_NIL, ILU_NIL, ILU_NIL, ILU_NIL };


/* default contact info to export non iluhttp_Resource objects with */
char* g_pc_nonhttp_pinfo = "w3ng_1.0"; 
char* g_ppc_nonhttp_tinfo [8] = 
/* { "sunrpcrm", ILU_NIL, ILU_NIL, ILU_NIL, ILU_NIL, ILU_NIL, ILU_NIL, ILU_NIL }; */
{ "w3mux_16_NGWebServerEndpoint", ILU_NIL, ILU_NIL, ILU_NIL, ILU_NIL, ILU_NIL, ILU_NIL, ILU_NIL };


/* port to start creating ng servers at */
unsigned short g_us_ngtcpport = 2718;

/* when using w3mux, channel to start creating ng servers at */
unsigned short g_us_ngmuxchannel = 0;

/* whether to run ILU multithreaded */
int g_b_multithreaded = ilu_TRUE;	

/* whether or not to use ILU batching */
int g_b_batching = 0;
unsigned int g_ui_batching_period;
ILU_C_Batcher g_batcher = NULL;

/* ilu fd budget */
static int g_i_fd_budget = -1;	

/* name of logfile */
static char* g_pc_logfilename = DEFAULT_LOG_FILENAME;
FILE* g_p_logfile = NULL;
ilu_Mutex g_logfile_mutex = NULL;

/* simple total of how many WebDocument:GetRendering,
WebDocument:SendRendering, and iluhttp.Resource Get calls
were performed.  Increment under protection of g_logfile_mutex */
int g_i_request_count = 0; 

/* holds the name of this host */
char g_pc_hostname[1024] = "";			


/* Usage string */
static char g_c_usage[] =							
"Usage:  ngwebserver [-httppinfo (http_1_0 | http_1_0p | http_1_1)] \n\
                     [-httptinfo tinfo]\n\
                     [-ngpinfo pinfo]\n\
                     [-ngtinfo tinfo]\n\
                     [-ngport portnum]\n\
                     [-batch period]\n\
                     [-filebase pathname]\n\
                     [-suffix serversuffix]\n\
                     [-fdbudget budget]\n\
                     [-logfile filename]\n\
                     [-verbose level]\n\
\t-httppinfo - protocol for iluhttp.Resource, typically one of http_1_0, http_1_0p or http_1_1(default)\n\
\t-httptinfo - transport for iluhttp.Resource - default tcp_0_80\n\
\t-ngpinfo   - protocol for non iluhttp.Resource ports - default w3ng_1.0\n\
\t-ngtinfo   - transports over tcp for non iluhttp.Resource ports - default w3mux_16_NGWebServerEndpoint\n\
\t-ngport    - tcp port to start making ng servers with, default 2718\n\
\t-batch     - use ILU Batching, period in microseconds\n\
\t-filebase  - root location of where server files are stored - default \\htmldocs \n\
\t-suffix    - string used as suffix to ILU server names - default .parc.xerox.com \n\
\t-fdbudget  - ILU file descriptor budget - defaults to ILU's default\n\
\t-logfile   - name of logfile where verbose output goes - default stdout\n\
\t-verbose   - output to stdout information about what's going, \n\
\t             level 0 = nothing, 1 = summary, 2 = basic, 3 = full - default 0";

/* we can't really run single threaded since we currently fork a
thread to run DataSources 
                     [-sthread]\n\
\t-sthread   - run single threaded, default is multithreaded\n\
*/

/* used to stop the server */
static int g_i_stop;


/* for C runtime timeing statistics */
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
/* process command line                                      */

static int process_command_line(int i_argcount, char** ppc_argvalue) {
	
	int i_argindex = 1;				/* for walking through command line arguments */
	ilu_boolean b_single_threaded = ilu_FALSE;
	char* pc_start_time;
	
	/* setup based on command line arguments */
	while (i_argindex < i_argcount) {
		
		/* check for http version to use */
		if (strcmp(ppc_argvalue[i_argindex], "-httppinfo") == 0) {
			i_argindex++;
			if (i_argindex >= i_argcount) {
				fprintf(stderr, "No iluhttp.Resource pinfo provided\n");
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
		
		/* check for filebase */
		if (strcmp(ppc_argvalue[i_argindex], "-filebase") == 0) {
			i_argindex++;
			if (i_argindex >= i_argcount) {
				fprintf(stderr, "No filebase provided\n");
				return -1;
			}
			g_pc_file_base = ppc_argvalue[i_argindex];
			goto nextarg;
		} 
		
		/* check for server suffix */
		if (strcmp(ppc_argvalue[i_argindex], "-suffix") == 0) {
			i_argindex++;
			if (i_argindex >= i_argcount) {
				fprintf(stderr, "No suffix provided\n");
				return -1;
			}
			g_pc_ngwebserver_suffix = ppc_argvalue[i_argindex];
			goto nextarg;
		} 
		
		/* check for single threading */		
		if (strcmp(ppc_argvalue[i_argindex], "-sthread") == 0) {
			g_b_multithreaded = ilu_FALSE;
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
		if (strcmp(ppc_argvalue[i_argindex], "-verbose") == 0) {
			i_argindex++;
			if (i_argindex >= i_argcount ||
				(sscanf(ppc_argvalue[i_argindex], "%i", &g_i_verbosity) != 1) ||
				(g_i_verbosity > MAX_VERBOSITY_LEVEL)) {
				fprintf(stderr, "Invalid verbosity level %s\n", ppc_argvalue[i_argindex]);
				return -1;
			}
			goto nextarg;
		} 
		
		/* unknown command line argument */
		fprintf (stderr, "Invalid argument %s\n%s\n", ppc_argvalue[i_argindex], g_c_usage);
		return -1;
		
nextarg:
		/* advance to the next command line argument */
		i_argindex++;
	}
	

	/* setup default http version to use */
	ilu_http_default_scheme = g_pc_http_pinfo;

	/* force 1.0 connection closure for non iluhttp.resource objects accessed
	   with http_1_0 */
	if (strcmp(g_pc_http_pinfo, "http_1_0") == 0)
	  ilu_http_force_close_on_ilu_1_0 = ilu_TRUE;

	/* multithread check */
	if (g_b_multithreaded == ilu_TRUE) {
#ifndef	ILU_OS_THREADED
		fprintf (stderr, "OS-supplied thread support not configured into ILU!\n");
		return -1;
#else
		ILU_C_USE_OS_THREADS;
#endif		/* ILU_OS_THREADED */
	}
	
	/* assign fd budget if specified */
	if (g_i_fd_budget > 0)
		g_i_fd_budget = ilu_SetFDBudget((ilu_cardinal)g_i_fd_budget);
	else g_i_fd_budget = ilu_GetFDBudget();
	
	/* set up log file */
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
				g_logfile_mutex = ilu_CreateMutex("ngwebserver", "logfile");
#endif /* ILU_OS_THREADED */
	}
	

	/* set up batcher if requested */
	if (g_b_batching) {
		CORBA_Environment ilu_env;
		ilu_FineTime batching_time;
		batching_time = ilu_FineTime_FromDouble(((double)g_ui_batching_period) / 1000000.0);
		g_batcher = ILU_C_CreateBatcher(batching_time, ilu_FALSE, &ilu_env);
		if (!nglib_show_and_clear_if_exception(&ilu_env, "calling ILU_C_CreateBatcher", stderr))
			ILU_C_SetBatcherContext(g_batcher);
	}

	/* get our hostname */
	gethostname(g_pc_hostname, sizeof(g_pc_hostname));
	
	
	/* log startup info */
	if (DO_LOG_SUMMARY()) {
		int i_walker = 0;
		LOG_ENTER();
		pc_start_time = nglib_get_current_time_string();
		fprintf(g_p_logfile, "ngwebserver started on %s at %s\n", g_pc_hostname, pc_start_time);
		fprintf(g_p_logfile, "\thttppinfo = %s\n", g_pc_http_pinfo);
		fprintf(g_p_logfile, "\thttptinfo = " );
		while (g_ppc_http_tinfo[i_walker]) 
			fprintf(g_p_logfile, "%s ", g_ppc_http_tinfo[i_walker++]);
		i_walker = 0;
		fprintf(g_p_logfile, "\n\tngpinfo   = %s\n", g_pc_nonhttp_pinfo);
		fprintf(g_p_logfile, "\tngtinfo   = ");
		while (g_ppc_nonhttp_tinfo[i_walker]) 
			fprintf(g_p_logfile, "%s ", g_ppc_nonhttp_tinfo[i_walker++]);
		fprintf(g_p_logfile, "\n\tbatching  = %s ", (g_b_batching ? "Yes" : "No"));
		if (g_b_batching)
			fprintf(g_p_logfile, "period = %u microseconds\n", g_ui_batching_period);
		else 
			fprintf(g_p_logfile, "\n");
		fprintf(g_p_logfile, "\tfilebase  = %s\n", g_pc_file_base);
		fprintf(g_p_logfile, "\tsuffix    = %s\n", g_pc_ngwebserver_suffix);
		fprintf(g_p_logfile, "\tthreading = %s\n", g_b_multithreaded ? "Multithreaded" : "Singlethreaded");
		fprintf(g_p_logfile, "\tfdbudget  = %i\n", g_i_fd_budget);
		fprintf(g_p_logfile, "\tlogfile   = %s\n", g_pc_logfilename);
		fprintf(g_p_logfile, "\tverbose   = %i\n", g_i_verbosity);		
		fprintf(g_p_logfile, "-------------------------------------------------\n");		
		LOG_EXIT();
		free(pc_start_time);
	}
	
	return 0;
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
/* initialization of interfaces and ILU servers              */

static int setup_ng_servers() {
	
	static ilu_boolean b_already_setup = ilu_FALSE;
	extern void iluhttp_impl_setup_server();
	extern void ngdocument_impl_setup_server();
	extern void ngform_impl_setup_server();
	extern void ngproperty_impl_setup_server();
	extern void ngstream_impl_setup_server();

	if (b_already_setup)
		return ilu_TRUE;
	
	/* first initialize all the interfaces */
	NgBasic__Initialize();
	NgStream__Initialize();
	NgRendering__Initialize();

	NgBasic__InitializeServer();
	NgStream__InitializeServer();
	NgRendering__InitializeServer();
	iluhttp__InitializeServer();
	NgProperty__InitializeServer();
	NgFormProcessor__InitializeServer();
	NgDocument__InitializeServer();
	
	/* create object tables and ILU servers*/
	iluhttp_impl_setup_server();
	ngdocument_impl_setup_server();
	ngform_impl_setup_server();
	ngproperty_impl_setup_server();
	ngstream_impl_setup_server();

	if (!(g_http_server && g_form_server && g_document_server && 
		g_property_server && g_stream_server))
		return (-1);

	return 0;
}




/* ********************************************************* */
/* cleanup - does any necessary cleanup work                 */

void cleanup(void) {
	
	ilu_Error an_error;
	
	/* log shutdown info */
	if (DO_LOG_SUMMARY()) {
		char* pc_stop_time;
		ilu_cardinal  card_bytes_sent;
		ilu_cardinal  card_bytes_received;
		ilu_cardinal  card_moorings_created;
		ilu_cardinal  card_connections_received;
		ilu_cardinal  card_connections_created;
		ilu_cardinal  card_current_connections;
		ilu_cardinal  card_max_connections;
		
		LOG_ENTER();
		pc_stop_time = nglib_get_current_time_string();
		fprintf(g_p_logfile, "ngwebserver stopped on %s at %s\n", g_pc_hostname, pc_stop_time);
		fprintf(g_p_logfile, "%d requests (WebDocument:Get/SendRendering, and iluhttp.Resource Get) served\n", g_i_request_count);
		
		/* get ILU tcp stats */
		ilu_tcp_GetStats (&card_bytes_sent, 
			&card_bytes_received,
			&card_moorings_created,
			&card_connections_received,
			&card_connections_created,
			&card_current_connections,
			&card_max_connections);
		
			fprintf (g_p_logfile, "TCP Stats:\n\tbytes_sent %lu\n\tbytes_received %lu\n\tmoorings_created %lu\n\
\tconnections_received %lu\n\tconnections_created %lu\n\tcurrent_connections %lu\n\tmax_connections %lu\n",
				card_bytes_sent, 
				card_bytes_received,
				card_moorings_created,
				card_connections_received,
				card_connections_created,
				card_current_connections,
				card_max_connections);		
			LOG_EXIT();
			free(pc_stop_time);
	}
	
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
		

	if (g_p_logfile && g_p_logfile != stdout)
		fclose (g_p_logfile);
	
	if (g_logfile_mutex) {
		ilu_DestroyMutex(g_logfile_mutex, &an_error);
		if (ILU_ERRNOK(an_error)) {
			fprintf(stderr, "Error destroying logfile mutex\n");
			ILU_HANDLED(an_error);
			ILU_CLER(an_error);
		}
	}
	
	/* xxx Should call any cleanup functions in the various interfaces */
	if (g_batcher) {
		CORBA_Environment ilu_env;
		ILU_C_SetBatcherContext(NULL);
		ILU_C_ReleaseBatcher(g_batcher, &ilu_env);
		nglib_show_and_clear_if_exception(&ilu_env, "calling ILU_C_ReleaseBatcher", stderr);
	}
	
}




/* ********************************************************* */
/* main                                                      */
/* ********************************************************* */

#ifndef WIN32
void set_stop(int sig) {
	g_i_stop = 1;
}
#else
BOOL __stdcall set_stop( DWORD dwCtrlType ) {
	
	switch (dwCtrlType) {
	case CTRL_C_EVENT :
		g_i_stop = 1;
		return TRUE;
	default:
		return FALSE;
	}
}
#endif

/* xxx unrelated but convenient to play with here */
char* ILU_C_URLOfObject(ILU_C_Object *obj) {
	ilu_Error an_error;
	char* pc_url;

	pc_url = ilu_URLOfObject(obj->iluco_ko, &an_error);
	if (ILU_ERROK(an_error))
		return pc_url;
	ILU_HANDLED(an_error);
	ILU_CLER(an_error);
	return NULL;
}



int main(int ac, char **av) {

	NgDocument_WebDocument webdocobj;
	iluhttp_Resource http_obj;
	char* pc_temp;
	char* pc_start_time;

/*	PUTENV_FUNCTION("ILU_DEBUG=connection:call:packet:lsr:object");   debugging only */
	/* PUTENV_FUNCTION("ILU_DEBUG=");   debugging only */

	/* process the command line */
	if (process_command_line(ac, av) != 0)
		return(1);

	if (setup_ng_servers() != 0)  {		
		fprintf (stderr, "Couldn't setup ILU servers\n");
		return(1);
	}

	nglib_init();

	/* xxx testing - create a top level WebDocument  */
	webdocobj = NgDocument_WebDocument__CreateTrue ("WebDocument:", g_document_server, NULL);
	printf("webdocobj SBH = %s\n", ILU_C_SBHOfObject(webdocobj));
	http_obj = iluhttp_Resource__CreateTrue ("/bio.txt", g_http_server, NULL);
	printf("http_obj SBH = %s\n", ILU_C_SBHOfObject(http_obj));
	pc_temp = ILU_C_URLOfObject(http_obj);
	printf("http_obj URL = %s\n", (pc_temp ? pc_temp : "NULL"));
	ilu_free(pc_temp);
	/* xxx end testing */

	ilu_tcp_InitializeStats();          /* ensure tcp stats are zeroed */

#ifdef ILU_C_TIMING_STATISTICS
	/* clear stats counter, and enable statistics gathering */
	ILU_C_SetCallStats (ilu_TRUE, ilu_TRUE);
#endif /* def ILU_C_TIMING_STATISTICS */

	/* arrange for control C to set g_i_stop */
#ifndef WIN32
	signal(SIGINT, set_stop);
	signal(SIGTERM, set_stop);
#else
	SetConsoleCtrlHandler( set_stop, TRUE);
#endif

	if (DO_LOG_SUMMARY()) {
		pc_start_time = nglib_get_current_time_string();
		LOG_ENTER();
		fprintf(g_p_logfile, "ngwebserver started on %s at %s\n", g_pc_hostname, pc_start_time);
		LOG_EXIT();
		ilu_free(pc_start_time);
	}

	/* run the server */
	ILU_C_StoppableRun(&g_i_stop);

	nglib_shutdown();
	cleanup();

	return 0;
}





/* ********************************************************* */
/* end of file */
/* ********************************************************* */
