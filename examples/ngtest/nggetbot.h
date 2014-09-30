/** $Id: nggetbot.h,v 1.8 1999/08/03 01:58:16 janssen Exp $
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
/* Last edited by Mike Spreitzer October 9, 1998 1:59 pm PDT */

/* very simple brute force web client program yfor testing HTTP-NG

  Dan Larner, larner@parc.xerox.com
  4-23-98
  */

/* ********************************************************* */
/* includes and defines                                      */
/* ********************************************************* */

#ifndef __nggetbot_h__
#define __nggetbot_h__

#ifdef __cplusplus
extern "C" {
#endif


#include "nglib.h"

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

#define SEND_TIME_ENTER() if (g_send_to_first_byte_mutex) ilu_AcquireMutex(g_send_to_first_byte_mutex)
#define SEND_TIME_EXIT() if (g_send_to_first_byte_mutex) ilu_ReleaseMutex(g_send_to_first_byte_mutex)

#define DO_LOG_BASIC() g_p_logfile && g_i_verbosity >= LOG_BASIC 	
#define DO_LOG_SUMMARY() g_p_logfile && g_i_verbosity >= LOG_SUMMARY 	
#define DO_LOG_TIMING() g_p_logfile && g_i_verbosity >= LOG_TIMING 	
#define DO_LOG_FULL() g_p_logfile && g_i_verbosity >= LOG_FULL 	

/* default string used as suffix to our ilu server names */
#define XEROX_PARC_DOMAIN ".parc.xerox.com"	

/* the file name where log output goes */
#define DEFAULT_LOG_FILENAME "stdout"	

#define NGGETBOT_DEFAULT_CHUNK_SIZE 2048

/* ********************************************************* */
/* some globals                                              */
/* ********************************************************* */

/* for statistics gathering */
typedef struct {
	double          minX, maxX, sumX, sumXX;
	unsigned        n;
}            StatSums;



/* level of output info */
extern int g_i_verbosity;

/* string used as suffix to our ilu server names */
extern ilu_string g_pc_nggetbot_suffix;


/* default contact info to export iluhttp_Resource objects with */
extern char* g_pc_http_pinfo;
extern char* g_ppc_http_tinfo [];


/* default contact info to export non iluhttp_Resource objects with */
extern char* g_pc_nonhttp_pinfo;
extern char* g_ppc_nonhttp_tinfo [];

/* port to start creating ng servers at */
extern unsigned short g_us_ngtcpport;

/* whether to run ILU multithreaded */
extern int g_b_multithreaded;	

/* logfile */
extern FILE* g_p_logfile;
extern ilu_Mutex g_logfile_mutex;

/* When using Sinks, time from issuing Send Rendering to first rendering byte received. */
extern StatSums g_send_to_first_byte_timesums;
extern ilu_Mutex g_send_to_first_byte_mutex;


/* holds the name of this host */
extern char g_pc_hostname[];			

/* ********************************************************* */
/* ILU Servers                                               */
/* ********************************************************* */


extern ILU_C_Server g_stream_server;


/* ********************************************************* */
/* Misc exported functions                                   */
/* ********************************************************* */

/* returns g_ppc_nonhttp_tinfo set up with tcp_0_<value of g_us_ngtcpport++>
on each call, or if using w3mux, setup with w3mux_<value of g_us_ngmuxchannel++>
tcp_0_<value of g_us_ngtcpport>*/
extern char** get_ngtinfo();

/* if the object denoted by pc_url is a NgDocument_WebDocument, it retrieves the rendering
from it, (putting it in *pp_where_to_put_rendering if i_repeats == 1 and i_depth == 1 and
we're not using the sink approach) and returns 1 if successful, else -1, 
if the object denoted by pc_url is NOT a NgDocument_WebDocument it returns 0 */

int try_ngget(char* pc_url, int i_repeats, int i_depth,  
			  NgRendering_Rendering** pp_where_to_put_rendering);

/* the nggetbot startup function */
extern int process_command_line_and_setup(int i_argcount, char** ppc_argvalue);

/* used to add to statistics */
extern void AddStat(StatSums * ss, ilu_FineTime time_interval);


#ifdef __cplusplus
}
#endif


#endif /* __nggetbot_h__ */
