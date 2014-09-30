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
/*
*/
/* Last edited by Mike Spreitzer September 11, 1997 10:51 am PDT */

/* $Id: http.h,v 1.23 1999/08/03 01:53:09 janssen Exp $ */


/* Provide HTTP Protocol for ILU

  Dan Larner, larner@parc.xerox.com
  4-4-96
  */



/* ********************************************************* */
/* prevent more than one inclusion of this header file       */
/* ********************************************************* */

#ifndef _HTTP_PROT_H
#define _HTTP_PROT_H



/* ********************************************************* */
/* Includes                                                  */ 
/* ********************************************************* */

#include <stdio.h>

#if ((defined(WIN32) || defined(WIN16)) && defined(_WINIO))
#include <winiodef.h>
#endif /* ((defined(WIN32) || defined(WIN16)) && defined(_WINIO)) */

#include <sys/types.h>

#include <fcntl.h>


/* ********************************************************* */
/* Defines                                                   */ 
/* ********************************************************* */

/* Assert when an unexpected http state is encountered */
#define HTTP_UNEXPECTED_STATE_ASSERT() _http_unexpected_state_assert(p_call, __FILE__, __LINE__)

/* Assert when an unexpected http call type is encountered */
#define HTTP_UNEXPECTED_CALL_TYPE_ASSERT() _http_unexpected_call_type_assert(p_call, __FILE__, __LINE__)

/* default port number to use for http if it's not specified */
#define DEFAULT_HTTP_PORT_NUMBER 80

#ifndef DEFAULT_HTTP_SCHEME
/* default httpversion to use (when blingly parsing a URL), should be one of http_1_0, http_1_0p, http_1_1 */
#define DEFAULT_HTTP_SCHEME "http_1_0p"
#endif 

/* for research purposes, we can change the default scheme used 
   just be sure that what you set it to isn't stack allocated or freed */
ILU_PUBLIC ilu_string ilu_http_default_scheme;


/* If we're doing http 1.0 call, we normally keep the connection open if 
the call wasn't on a direct instances of iluhttp.Resource since we know 
it must be ILU on the other end.  In some cases, (e.g. http-ng testing)
we may actually want force true 1.0 connection closure behavior for calls on
types other than just iluhttp.Resource. So, if we're http 1.0 and 
ilu_http_force_close_on_ilu_1_0 is set, we close the connection. 
Note that there's no lock around this global - it's intended to
typically be changed only during an apps initialization.*/
ILU_PUBLIC ilu_boolean ilu_http_force_close_on_ilu_1_0;


/* the type id of a http_resource_object
   NOTE HTTP_RESOURCE_OBJECT_TYPE_ID MUST AGREE with the TYPEID for http.Resource objects
   in the file src/stubbers/parser/iluhttp.isl   */
#define HTTP_RESOURCE_OBJECT_TYPE_ID "ilu:Ilu_Http_1_0_resource_object"
#define HTTP_1_1_RESOURCE_OBJECT_TYPE_ID "ilu:Ilu_Http_1_1_resource_object"

/* name of the environment variable that may be used to set http 
   proxy server information 
  ILU_HTTP_PROXY_INFO is of the form proxy.host.name:portnumber */
#define ILU_HTTP_PROXY_INFO_ENV_VAR "ILU_HTTP_PROXY_INFO"


/* Maximum number of retries (due to 302 "Moved Temporarily" status)
to perform per call */
#define ILU_HTTP_MAXIMUM_RETRIES 5

/* used to differentiate between get, head and other methods */
#define ILU_HTTP_UNKNOWN_OR_OTHER_METHOD 0
#define ILU_HTTP_GET_METHOD 1
#define ILU_HTTP_HEAD_METHOD 2


/* ********************************************************* */
/* Simple singly linked list                                 */
/* ********************************************************* */

/* a node in a list */
typedef struct _ilu_list_node_s  * _ilu_p_list_node;

typedef struct _ilu_list_node_s {
	ilu_refany			m_p_node_contents;
	/* intent is that m_p_node_into_contents
	   points into m_p_node_contents - thus it should never
	   be freed */
	ilu_refany			m_p_node_into_contents; 
	_ilu_p_list_node	m_p_next_list_node;
} _ilu_list_node_s;


/* a list */
typedef struct _ilu_list_s * _ilu_p_list;

typedef struct _ilu_list_s {
	_ilu_p_list_node	m_p_start_list_node;
	_ilu_p_list_node	m_p_last_list_node;
	ilu_cardinal		m_card_num_added; 
} _ilu_list_s;



/* ********************************************************* */
/* type safe lists for htpp header lines                     */

typedef _ilu_p_list			_http_p_header_list;
typedef _ilu_p_list_node	_http_p_list_node;




/* ********************************************************* */
/* contains any (member-var like) attributes specific to the 
   protocol - currently specifies batching and concurrency
   but batching probably really doesn't apply for http       */
/* ********************************************************* */

typedef struct _http_members_s http_members_s;

#define BUILT_IN_SERIAL_NUM_QUEUE_SIZE 16

struct _http_members_s {

  ilu_boolean     m_b_batching;
  ilu_boolean     m_b_concurrent;
  ilu_boolean     m_b_persistent;			/* true if this is http_1_0p */
  ilu_shortcardinal m_scard_major_version;
  ilu_shortcardinal m_scard_minor_version;
  ilu_cardinal*	  m_serial_num_queue;		/* serial number queue */
  ilu_cardinal    m_serial_num_queue_head;	/* indices into m_serial_num_queue */
  ilu_cardinal	  m_serial_num_queue_tail;
  ilu_cardinal	  m_serial_num_queue_size;
  ilu_cardinal    m_built_in_serial_num_queue[BUILT_IN_SERIAL_NUM_QUEUE_SIZE];
  ilu_cardinal	  m_card_serial_number_counter; /* monotonically increasing counter used to create serial numbers */
};
/*
 * m_serial_num_queue[m_serial_num_queue_tail] is the Serial Number of the next reply; 
 * m_serial_num_queue[m_serial_num_queue_head] is where to store the Serial Number of the next request. 
 * Queue is empty when m_serial_num_queue_head == m_serial_num_queue_tail.  
 * Buffer is circular, mod m_serial_num_queue_size.
 * m_serial_num_queue may point to either m_built_in_serial_num_queue or ilu_malloc'd storage.
 */


/*
Http Protocol state description
-------------------------------

Capitalized (sub)terms refer to http ISL concepts, e.g. 'Request' is the
ISL concept (that is an ILU record) that a method gets as an argument.
'request' is an ilu message that comes into a server.  This distinction
using capitalization is to help disambiguate between similar HTTP and 
ILU terms.

clnt2http_  prefix denotes a state that occurs on the client (surrogate) side
when performing a GET HEAD or POST call to a httpResource (or derived)
object.

clnt2ilu_  prefix denotes a state that occurs on the client (surrogate) side
when performing a method that is NOT a GET HEAD or POST call to a 
httpResource (or derived) Object.

srvr4http_  prefix denotes a state that occurs on the server (true) side
when performing a GET HEAD or POST call to a httpResource (or derived)
Object.

srvr4ilu_  prefix denotes a state that occurs on the server (true) side
when performing a method that is NOT a GET HEAD or POST call to a 
httpResource (or derived) Object.

in or out denotes which way bits are flowing with respect to the
side whose state it is.  e.g. clnt2http_out_... means bits
going out of a client.



	PROTOCOL FUNCTION			STATE (state represents what we're expecting to do next)
*/

typedef enum _http_enum_call_state {

	/* _http_init_call				initialization (the start state of a call) */ http_init_state = 0,
	

	/* *************************************************************** */
	/* *******************  begin clnt2http states ******************* */
	/* *************************************************************** */

	/* send the ilu request */
	/* _http_start_request			*/ clnt2http_out_method_name = 100,

	/* _http_output_object_id		*/ clnt2http_out_URL_path,
	/* _http_output_record			*/ clnt2http_out_Request_record,
	/* _http_output_string			*/ clnt2http_out_Request_URI,
	/* _http_output_sequence		*/ clnt2http_out_Header_sequence,
	/* _http_output_record			*/ clnt2http_out_Header_record,
	/* _http_output_string			*/ clnt2http_out_Header_Name,
	/* _http_output_optional		*/ clnt2http_out_Header_Value_present,
	/* _http_output_string			*/ clnt2http_out_Header_value,
	/* _http_end_record				*/ clnt2http_out_end_Header_record,
	/* _http_end_sequence			*/ clnt2http_out_end_Header_sequence,
	/* _http_output_optional		*/ clnt2http_out_Body_present,
	/* _http_output_bytes			*/ clnt2http_out_Body,
	/* _http_end_record				*/ clnt2http_out_end_Request_record,

	/* _http_finish_request			*/ clnt2http_out_finish_request,

	/* read in the ilu reply */
	/* _http_read_header			*/ clnt2http_in_read_reply_header = 200,
	/* _http_interpret_reply		*/ clnt2http_in_interpret_reply,

	/* _http_input_record			*/ clnt2http_in_Response_record,
	/* _http_input_enum_code		*/ clnt2http_in_Status_Line,
	/* _http_input_sequence			*/ clnt2http_in_Header_sequence,
	/* _http_input_record			*/ clnt2http_in_Header_record,
	/* _http_input_string			*/ clnt2http_in_Header_Name,
	/* _http_input_optional			*/ clnt2http_in_Header_Value_present,
	/* _http_input_string			*/ clnt2http_in_Header_value,
	/* _http_end_record				*/ clnt2http_in_end_Header_record,
	/* _http_end_sequence			*/ clnt2http_in_end_Header_sequence,
	/* _http_input_optional			*/ clnt2http_in_Body_present,
	/* _http_input_bytes			*/ clnt2http_in_Body,
	/* _http_end_record				*/ clnt2http_in_end_Response_record,
	/* _http_reply_read				*/ clnt2http_in_reply_read,

	/* finish up */
	/* _http_prefinish_call
	   _http_finish_call			*/ clnt2http_finish_call,

	/* _http_prefinish_call (redirection) */ clnt2http_redirect_call,


	/* *************************************************************** */
	/* *******************  begin clnt2ilu states ******************* */
	/* *************************************************************** */

	/* send the ilu request */
	/* _http_start_request			*/ clnt2ilu_out_method_name = 300,

	/* _http_output_object_id		*/ clnt2ilu_out_object_id,
	/* _http_output_VARIOUS			*/ clnt2ilu_out_arguments,
	/* _http_finish_request			*/ 

	/* read in the ilu reply */
	/* _http_read_header			*/ clnt2ilu_in_read_reply_header,
	/* _http_interpret_reply		*/ clnt2ilu_in_interpret_reply,

	/* note that state clnt2ilu_in_return_values is used in both
	the situation where we're reading return values, as well as when
	we're actually reading in exception values */
	/* _http_input_VARIOUS			*/ clnt2ilu_in_return_values,
	/* _http_reply_read				*/ 

	/* finish up */
	/* _http_prefinish_call
	   _http_finish_call			*/ clnt2ilu_finish_call,

	/* _http_prefinish_call (redirection) */ clnt2ilu_redirect_call,

	/* *************************************************************** */
	/* *******************  begin srvr4http states ******************* */
	/* *************************************************************** */

	/* read in the ilu request */
	/* note srvr_in_read_header is used for both srvr4http and srvr4ilu */
	/* _http_read_header			*/ srvr_in_read_header = 400,
	/* _http_interpret_request		*/ srvr4http_in_interpret_request,	
	/* _http_input_string			*/ srvr4http_in_server_id,
	/* _http_input_string			*/ srvr4http_in_discriminator_id,

	/* _http_input_record			*/ srvr4http_in_Request_record,
	/* _http_input_string			*/ srvr4http_in_Request_URI,
	/* _http_input_sequence			*/ srvr4http_in_Header_sequence,
	/* _http_input_record			*/ srvr4http_in_Header_record,
	/* _http_input_string			*/ srvr4http_in_Header_Name,
	/* _http_input_optional			*/ srvr4http_in_Header_Value_present,
	/* _http_input_string			*/ srvr4http_in_Header_value,
	/* _http_end_record				*/ srvr4http_in_end_Header_record,
	/* _http_end_sequence			*/ srvr4http_in_end_Header_sequence,
	/* _http_input_optional			*/ srvr4http_in_Body_present,
	/* _http_input_bytes			*/ srvr4http_in_Body,
	/* _http_end_record				*/ srvr4http_in_end_Request_record,
	/* _http_request_read			*/ srvr4http_in_request_read,

	/* send the ilu reply */
	/* _http_begin_reply			*/ srvr4http_out_begin_reply = 500,

	/* _http_output_record			*/ srvr4http_out_Response_record,
	/* _http_output_enum			*/ srvr4http_out_Status_Line,
	/* _http_output_sequence		*/ srvr4http_out_Header_sequence,
	/* _http_output_record			*/ srvr4http_out_Header_record,
	/* _http_output_string			*/ srvr4http_out_Header_Name,
	/* _http_output_optional		*/ srvr4http_out_Header_Value_present,
	/* _http_output_string			*/ srvr4http_out_Header_value,
	/* _http_end_record				*/ srvr4http_out_end_Header_record,
	/* _http_end_sequence			*/ srvr4http_out_end_Header_sequence,
	/* _http_output_optional		*/ srvr4http_out_Body_present,
	/* _http_output_bytes			*/ srvr4http_out_Body,
	/* _http_end_record				*/ srvr4http_out_end_Response_record,

	/* _http_finish_reply			*/ srvr4http_out_finish_reply,

	/* finish up */
	/* _http_prefinish_call
	   _http_finish_call			*/ srvr4http_finish_call,

	/* _http_output_VARIOUS			*/ srvr4http_out_exception,
									   srvr4http_missing_1_1_host_header,
	/* _http_finish_exception		sets state to srvr4http_finish_call */ 


	/* *************************************************************** */
	/* *******************  begin srvr4ilu states ******************** */
	/* *************************************************************** */

	/* read in the ilu request */
	/* _http_read_header			 srvr_in_read_header, delcared in srvr4http section */
	/* _http_interpret_request		*/ srvr4ilu_in_interpret_request = 600,	
	/* _http_input_string			*/ srvr4ilu_in_server_id,
	/* _http_input_string			*/ srvr4ilu_in_discriminator_id,

	/* _http_input_VARIOUS			*/ srvr4ilu_in_arguments,
	/* _http_request_read			*/ 

	/* send the ilu reply */
	/* _http_begin_reply			*/ srvr4ilu_out_begin_reply,

	/* _http_output_VARIOUS			*/ srvr4ilu_out_return_values,
	/* _http_finish_reply			*/ 

	/* finish up */
	/* _http_prefinish_call
	   _http_finish_call			*/ srvr4ilu_finish_call,	
			

	/* _http_output_VARIOUS			*/ srvr4ilu_out_exception,
	/* _http_finish_exception		sets state to srvr4ilu_finish_call */ 


  	/* *************************************************************** */
	/* *******************  bogus state ****************************** */
	/* *************************************************************** */

	/* used for development / debuging only */  http_bogus_state = 9999

	} _http_call_state ; /* end http_enum_call_state */



/* an enum denoting which side of / which kind of the call we're on */

typedef enum _http_enum_call_type {
	unknowncalltype,
	clnt2http,
	clnt2ilu,
	srvr4http,
	srvr4ilu,
	srvr4something
} _http_call_type;


/* **************************************************************** */
/* http call specific data put into a http call's ca_prdata2 member */
/* **************************************************************** */

typedef struct _http_call_info_s http_call_info_s;

struct _http_call_info_s {

	/* note that note all members may be used in any particular call - the
	   pointer ones that are not are guaranteed to be nil */

	/* the state of call processing we're in */
	_http_call_state m_call_state;

	/* which side of / which kind of the call we're on */
	_http_call_type m_call_type;

	/* contains number of headers yet to be processed on outgoing calls */
	ilu_cardinal m_card_num_headers_to_process;

	/* indicates whether this is a GET, HEAD, or other method call - we need to know HEAD
	so we'll know that the Content-Length field is to be ignored when 
	it comes to reading in any entity body in a reply, we need to know if
	it's get or head so we can determine whether or not to do automatic redirection
	in the event of receiving a 302 Moved Temporarily status code */
	ilu_cardinal m_card_method_kind;

	/* indicates whether the user supplied their own content-length header */
	ilu_boolean m_b_user_supplied_content_length;

	/* indicates whether the user supplied their own Host header (http 1.1) */
	ilu_boolean m_b_user_supplied_host;

	/* list used to hold header lines as we read them in, in either a reply back to 
	   a client or in a request to a server */
	_http_p_header_list m_p_http_headers_list;
	_http_p_list_node m_p_http_headers_current_node;

	/* holds request or status lines for server incoming requests and client incoming replies */
	ilu_string m_pc_request_or_status_line;

	/* note that none the next three strings  should be freed as they are merely pointers 
	   into other allocated spaces */
	ilu_string m_pc_method_name;	/* in requests, where in m_pc_request_or_status_line method name is */
	ilu_string m_pc_uri;			/* in requests, where in m_pc_request_or_status_line the Request-URI is*/
	ilu_string m_pc_object_id;		/* in requests, where in m_pc_request_or_status_line object_id is (usually part of,
										or same as Request-URI)*/
	ilu_string m_pc_params_queries;	/* in requests, where in m_pc_request_or_status_line params&queries (if any) is */
	char m_c_params_queries_delim;	/* in requests, what the delimiting char was if params&queries were present */
	ilu_string m_pc_server_id;		/* in requests, where in the headers the server_id is */
	ilu_string m_pc_version;		/* in requests and resonses, where in m_pc_request_or_status_line the version string is */
	ilu_string m_pc_status_code;	/* in responses, where in the headers the status code string is */
	ilu_string m_pc_reason_phrase;	/* in responses, where in the headers the reason_phrase string is */

	ilu_shortcardinal m_scard_major;	/* version numbers in an servers incoming request or a clients incoming reply */
	ilu_shortcardinal m_scard_minor;

	ilu_shortcardinal m_scard_status_code;	/* in a reply, what the status code is */

	ilu_cardinal m_card_body_length;	/* how long the body is according to the Content-Length header */

	ilu_boolean m_b_keep_alive;	/* whether this is a keep alive the connection sort of call */

    ilu_boolean m_b_persistent;     /* whether we're supposed to be supporting (http 1.0) persistent connections (http_1_0p) */

	/* when a redirection occurs, these members hold the new instance handle, server ID 
	and sbh use during the retry */
	ilu_string m_str_redirected_instance_handle;
	ilu_string m_str_redirected_server_id;
	ilu_string m_str_redirected_sbh;

	ilu_Transport m_delayed_interp_transport;  /* used during delayed interpretation to hold the remainder
											   of the reply message */

	ilu_boolean m_b_set_to_buffered_transport; /* true if the call's transport is currently set up to be an in memory
												 buffer (used in clnt2ilu and srvr4ilu) in order to be able to
												 generate a content length header */

}; /* end struct _http_call_info_s */



/* ************************************************************** */
/* #defined accessors for members of the _http_call_info_s struct */
/* ************************************************************** */

/* ********************************************************* */
/* get and set state and type of the call on an object */

#ifdef ENABLE_DEBUGGING

#define _http_get_call_state(p_call)  \
	(((ilu_DebugLevel & HTTP_DEBUG) ? \
		ilu_DebugPrintf("_http_get_call_state: call 0x%x state is %hd in %s line %d\n", \
		p_call, (( http_call_info_s *)(p_call->ca_prdata2))->m_call_state, __FILE__, __LINE__ ), 0 : 0), \
	((http_call_info_s*)(p_call->ca_prdata2))->m_call_state)


#define _http_set_call_state(p_call, newstate) _http_set_state_of_call(p_call, newstate, __FILE__, __LINE__)

#define _http_get_call_type(p_call)  \
	(((ilu_DebugLevel & HTTP_DEBUG) ? \
		ilu_DebugPrintf("_http_get_call_type: call 0x%x type is %hd in %s line %d\n", \
		p_call, (( http_call_info_s *)(p_call->ca_prdata2))->m_call_type, __FILE__, __LINE__ ), 0 : 0), \
	((http_call_info_s*)(p_call->ca_prdata2))->m_call_type)

#define _http_set_call_type(p_call, newtype) _http_set_type_of_call(p_call, newtype, __FILE__, __LINE__)

#else

#define _http_get_call_state(p_call) ((http_call_info_s*)(p_call->ca_prdata2))->m_call_state
#define _http_set_call_state(p_call, newstate) ((http_call_info_s*)(p_call->ca_prdata2))->m_call_state = newstate

#define _http_get_call_type(p_call) ((http_call_info_s*)(p_call->ca_prdata2))->m_call_type
#define _http_set_call_type(p_call, newtype) ((http_call_info_s*)(p_call->ca_prdata2))->m_call_type = newtype

#endif /* ENABLE_DEBUGGING */


/* ********************************************************* */
/* returns (lvalue) number of headers yet to be processed on outgoing calls */

#define _http_num_headers_to_process(p_call) \
	((( http_call_info_s *)(p_call->ca_prdata2))->m_card_num_headers_to_process)


/* ********************************************************* */
/* returns whether this is a GET method call */

#define _http_is_GET_method(p_call) \
	(((( http_call_info_s *)(p_call->ca_prdata2))->m_card_method_kind) == ILU_HTTP_GET_METHOD)

/* ********************************************************* */
/* returns whether this is a HEAD method call */

#define _http_is_HEAD_method(p_call) \
	(((( http_call_info_s *)(p_call->ca_prdata2))->m_card_method_kind) == ILU_HTTP_HEAD_METHOD)


/* ********************************************************* */
/* returns (lvalue) to set method kind */

#define _http_method_kind(p_call) \
	((( http_call_info_s *)(p_call->ca_prdata2))->m_card_method_kind)


/* ********************************************************* */
/* returns (lvalue) whether the user supplied their own content-length header */

#define _http_user_supplied_content_length(p_call) \
	((( http_call_info_s *)(p_call->ca_prdata2))->m_b_user_supplied_content_length)


/* ********************************************************* */
/* returns (lvalue) whether the user supplied their own host header */

#define _http_user_supplied_host(p_call) \
	((( http_call_info_s *)(p_call->ca_prdata2))->m_b_user_supplied_host)


/* ********************************************************* */
/* returns (lvalue) pointer to list where we read in our headers in an
   incoming request   */

#define _http_headers_list(p_call) \
	((( http_call_info_s *)(p_call->ca_prdata2))->m_p_http_headers_list)


/* ********************************************************* */
/* returns (lvalue) pointer to next node to process in the header list */

#define _http_headers_current_node(p_call) \
	((( http_call_info_s *)(p_call->ca_prdata2))->m_p_http_headers_current_node)


/* ********************************************************* */
/* returns (lvalue) buffer used for a reply status line      */

#define _http_status_line(p_call) \
	((( http_call_info_s *)(p_call->ca_prdata2))->m_pc_request_or_status_line)


/* ********************************************************* */
/* returns (lvalue) buffer used for a request request line   */

#define _http_request_line(p_call) \
	((( http_call_info_s *)(p_call->ca_prdata2))->m_pc_request_or_status_line)


/* ********************************************************* */
/* returns (lvalue) where in m_pc_request_or_status_line method name is */

#define _http_method_name(p_call) \
	((( http_call_info_s *)(p_call->ca_prdata2))->m_pc_method_name)


/* ********************************************************* */
/* returns (lvalue) where in m_pc_request_or_status_line uri is */

#define _http_request_uri(p_call) \
	((( http_call_info_s *)(p_call->ca_prdata2))->m_pc_uri)

/* ********************************************************* */
/* returns (lvalue) where in m_pc_request_or_status_line object_id is */

#define _http_object_id(p_call) \
	((( http_call_info_s *)(p_call->ca_prdata2))->m_pc_object_id)


/* ********************************************************* */
/* in requests, (lvalue) where in m_pc_request_or_status_line params&queries (if any) is */

#define _http_params_queries(p_call) \
	((( http_call_info_s *)(p_call->ca_prdata2))->m_pc_params_queries)


/* ********************************************************* */
/* in requests, (lvalue) what the delimiting char was if params&queries were present */

#define _http_params_queries_delim(p_call) \
	((( http_call_info_s *)(p_call->ca_prdata2))->m_c_params_queries_delim)


/* ********************************************************* */
/* returns (lvalue) where in m_pc_request_or_status_line version string is */

#define _http_version_string(p_call) \
	((( http_call_info_s *)(p_call->ca_prdata2))->m_pc_version)


/* ********************************************************* */
/* returns (lvalue) where in m_pc_request_or_status_line status code string is */

#define _http_status_code_string(p_call) \
	((( http_call_info_s *)(p_call->ca_prdata2))->m_pc_status_code)


/* ********************************************************* */
/* returns (lvalue) status code                              */

#define _http_status_code(p_call) \
	((( http_call_info_s *)(p_call->ca_prdata2))->m_scard_status_code)


/* ********************************************************* */
/* returns (lvalue) where in m_pc_request_or_status_line reason phrase string is */

#define _http_version_reason_phrase(p_call) \
	((( http_call_info_s *)(p_call->ca_prdata2))->m_pc_reason_phrase)



/* ********************************************************* */
/* returns (lvalue) where in m_pc_request_or_status_line server_id is */

#define _http_server_id(p_call) \
	((( http_call_info_s *)(p_call->ca_prdata2))->m_pc_server_id)


/* ********************************************************* */
/* returns (lvalue) what the major version (read in) is        */

#define _http_major_version(p_call) \
	((( http_call_info_s *)(p_call->ca_prdata2))->m_scard_major)


/* ********************************************************* */
/* returns (lvalue) what the minor version (read in) is        */

#define _http_minor_version(p_call) \
	((( http_call_info_s *)(p_call->ca_prdata2))->m_scard_minor)


/* ********************************************************* */
/* returns (lvalue) how long the body is according to 
   any Content-Length header                                 */

#define _http_body_length(p_call) \
	((( http_call_info_s *)(p_call->ca_prdata2))->m_card_body_length)


/* ********************************************************* */
/* returns (lvalue) whether this is a keep alive connection  */

#define _http_keep_alive(p_call) \
	((( http_call_info_s *)(p_call->ca_prdata2))->m_b_keep_alive)


/* ********************************************************* */
/* returns (lvalue)  whether we're supposed to be supporting persistent connections */

#define _http_persistent(p_call) \
	((( http_call_info_s *)(p_call->ca_prdata2))->m_b_persistent)


/* *********************************************************   */
/* returns (lvalue) for remembering redirected instance handle */

#define _http_redirected_instance_handle(p_call) \
	((( http_call_info_s *)(p_call->ca_prdata2))->m_str_redirected_instance_handle)


/* ********************************************************* */
/* returns (lvalue) for remembering redirected server id     */

#define _http_redirected_server_id(p_call) \
	((( http_call_info_s *)(p_call->ca_prdata2))->m_str_redirected_server_id)

/* ********************************************************* */
/* returns (lvalue) for remembering redirected sbh     */

#define _http_redirected_sbh(p_call) \
	((( http_call_info_s *)(p_call->ca_prdata2))->m_str_redirected_sbh)


/* ********************************************************* */
/* ensures no compiler complaints about passing
	transport_write_bytes a char* where it expects ilu_bytes */

#define _http_transport_write_bytes(the_transport, the_bytes, the_length, the_error) \
		transport_write_bytes(the_transport, (ilu_bytes) the_bytes, the_length, the_error)


/* ********************************************************* */
/* ensures no compiler complaints about passing
	transport_read_bytes a char* where it expects ilu_bytes */

#define _http_transport_read_bytes(the_transport, the_bytes, the_length, the_error) \
		transport_read_bytes(the_transport, (ilu_bytes) the_bytes, the_length, the_error)

#define _http_transport_read_onebyte(bs, buf, err)                     \
	(((bs)->tr_inBuff != NIL && (bs)->tr_inNext < (bs)->tr_inLimit   \
	&& 1 <= (bs)->tr_inLimit - (bs)->tr_inNext)                \
	? 	(  (*((ilu_bytes)buf) = *((ilu_bytes)((bs)->tr_inBuff + (bs)->tr_inNext))), \
	((bs)->tr_inNext)++, ILU_CLER(*(err)), 1)             \
	: _ilu_transportReadBytes(bs, (ilu_bytes)buf, 1, err))
/* Efficient wrapper for 1 byte */


#define _http_transport_read_upto_one_byte(bs,buf,rpt,err)			\
(((bs)->tr_inBuff != NIL && (bs)->tr_inNext < (bs)->tr_inLimit		\
  && 1 <= (bs)->tr_inLimit - (bs)->tr_inNext)			\
 ? ( (*((ilu_bytes)buf) = *((ilu_bytes)((bs)->tr_inBuff + (bs)->tr_inNext))),	\
    (rpt)->tr_eom = (rpt)->tr_eof = ilu_FALSE,				\
    (bs)->tr_inNext++, ILU_CLER(*(err)), (1))			\
 : _ilu_transportReadUpToBytes(bs, buf, 1, rpt, err))
/* Efficient wrapper for upto 1 byte. */


/* ********************************************************* */
/* returns (lvalue)  major version of the protocol for this call */

#define _http_protocol_major_version(p_call) \
	(((http_members_s*)(connection_protocol_data(call_connection(p_call))))->m_scard_major_version)


/* ********************************************************* */
/* returns (lvalue)  minor version of the protocol for this call */

#define _http_protocol_minor_version(p_call) \
	(((http_members_s*)(connection_protocol_data(call_connection(p_call))))->m_scard_minor_version)


/* ********************************************************* */
/* returns (lvalue)  persistence of the protocol for this call */

#define _http_protocol_persistence(p_call) \
	(((http_members_s*)(connection_protocol_data(call_connection(p_call))))->m_b_persistent)


/* ********************************************************* */
/* sets a new serial number to use in this call */

#define _http_generate_serial_number(p_call) \
	call_serial_number(p_call) = \
		(((http_members_s*)(connection_protocol_data(call_connection(p_call))))->m_card_serial_number_counter++)


/* ********************************************************* */
/* returns (lvalue)  delayed interpretation transport */

#define _http_delayed_interp_transport(p_call) \
	((( http_call_info_s *)(p_call->ca_prdata2))->m_delayed_interp_transport)

/* ********************************************************* */
/* returns (lvalue)  whether true if the call's transport is 
currently set up to be an in memory buffer (used in clnt2ilu and 
srvr4ilu) in order to be able to generate a content length header */

#define _http_set_to_buffered_transport(p_call) \
	((( http_call_info_s *)(p_call->ca_prdata2))->m_b_set_to_buffered_transport)

/* ********************************************************* */
#endif /* not _HTTP_PROT_H */



/* ********************************************************* */
/* end of file                                               */
/* ********************************************************* */


