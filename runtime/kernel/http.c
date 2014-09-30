/** $Id: http.c,v 1.103 1999/08/03 01:53:06 janssen Exp $
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
/* Last edited by Mike Spreitzer January 19, 1999 1:43 pm PST */

/* provide  http protocol 

  Dan Larner, larner@parc.xerox.com
  4-4-96
*/

/* ********************************************************* */
/* Notes

	If there is an entity body, and no content-length header was supplied
	by the user, then the protocol will automatically add one.  Note that
	when responding to a Head method then (since there is no body) the 
	user should supply a Content-Length header.

	When using the persistent connection version (e.g. http_1_0p), a
	Connection: Keep-Alive header is automatically added to requests
	and responses as appropriate.

	Any chars in the URL's params and or queries (if present) that are required by 
	http to be %%HEX or otherwise encoded, are assumed to be already in that form.

	Assumes the request URI is well formed. Eventually check on this and error if not.		

	Originally this code only addressed HTTP 1.0, (nothing earlier, nothing later except connection keep-alive)
	Now the following changes have been made to begin supporting 1.1:
		Code is much more forgiving w.r.t minor version differences
		Server will now accept full URI in a request (not just relative)
	 	For 1.1, ILU will add a Host header if one is not supplied by the client
		Added http 1.1 error phrases.
		Receiving a 1.1 request without a host header should return 400 (Bad Request)
		Persistence is the default behavior for 1.1 and Closures signalled with Connection close headers
		Continue status - a	client can send expect header, which must be responded to with continue or failed
			ILU just always blindly sends continue, since only the app's implementation
			knows whether or not it would fail, and we have no ways of asking it,  if ILU receives a
			100 continue status line, it simply ignores it



*/


/* ********************************************************* */
/* Includes                                                  */ 
/* ********************************************************* */

#include <string.h>
#include <ctype.h>

#if (defined WIN32 || defined WIN16 || defined( macintosh ) )
#include <winsock.h>
#else
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#endif

#include <iluntrnl.h>
#include <ilutransport.h>
#include <iluprotocol.h>
#include <call.h>
#include <connect.h>
#include <object.h>
#include <method.h>
#include <server.h>
#include <type.h>

#include <http.h>

/* ********************************************************* */
/* Forward Declarations                                      */ 
/* ********************************************************* */

ilu_boolean _ilu_Parse_HTTP_URL (ilu_string    istr_encodedSBH, 
				 ilu_string*   p_str_plainInstanceHandle, 
				 ilu_string*   p_str_plainServerID,
				 ilu_string*   p_str_plainMstid, 
				 ilu_string*   p_str_encodedContactInfo, 
				 ilu_cardinal* p_card_encodedContactInfoLen,
				 ilu_boolean * p_boolean_passEncodedCinfo,
				 ilu_Error*    p_error);

static ilu_string _http_SBH_to_URL (ilu_string pc_objects_sbh, ilu_Error* p_error);

static void _http_freeup_call (ilu_Call p_call);

static void _http_freeup_call_infostruct (ilu_Call p_call, ilu_boolean b_part_of_redirection);


/* ********************************************************* */
/* Defines                                                   */ 
/* ********************************************************* */

/* uncomment HTTP_STRICT_END_OF_LINE if you want end of line to be 
CRLF as it's specified by the protocol, leave commented out if
just LF is acceptable This is needed to deal with some http servers -  
at least one existing http demon (NCSA version 1.3) has been 
encountered that only sends out LF in spots where CRLF is required by the
protocol.  Encountering such a server from this code will show up as a 
communication exception if HTTP_STRICT_END_OF_LINE is defined.
(Note that some version between NCSA 1.3 and NCSA 1.5.1 corrected this.) */
/* #define HTTP_STRICT_END_OF_LINE */

/* during reading in lines, how much to increase allocations each time */
#define HTTP_READLINE_ALLOC_SIZE		128

/* during reading in bodies, how much to increase allocations each time */
#define HTTP_BODY_ALLOC_SIZE			2048

/* used for non native http calls to contain the server id in a http header */
#define ILU_SERVER_ID_HEADER_NAME "ILU_ServerID"
#define ILU_SERVER_ID_HEADER_NAME_LENGTH 12

/* debugging measure only */
#define FAULT ASSERT(ilu_FALSE, pc_buffer, (pc_buffer, "Bad HTTP State %d\n", _http_get_call_state(p_call)))

/* advance a pointer to next non space or tab character */
#define skipwhite(pc) while((*pc == ' ') || (*pc == '\t')) pc++;

/* advance a pointer to next space tab or null character */
#define nextwhite(pc) while(*pc && (*pc != ' ') && (*pc != '\t')) pc++;

#define http_call_tport(pc)	((pc)->ca_prTrans)

/* ********************************************************* */
/* Globals and Constants                                     */ 
/* ********************************************************* */


/* for research purposes, we can change the default scheme used 
   just be sure that what you set it to isn't stack allocated or freed */
ilu_string ilu_http_default_scheme = DEFAULT_HTTP_SCHEME;


/* If we're doing http 1.0 call, we normally keep the connection open if 
the call wasn't on a direct instances of iluhttp.Resource since we know 
it must be ILU on the other end.  In some cases, (e.g. http-ng testing)
we may actually want force true 1.0 connection closure behavior for calls on
types other than just iluhttp.Resource. So, if we're http 1.0 and 
ilu_http_force_close_on_ilu_1_0 is set, we close the connection. 
Note that there's no lock around this global - it's intended to
typically be changed only during an apps initialization.*/
ilu_boolean ilu_http_force_close_on_ilu_1_0 = 0;


/* carriage return linefeed combinations */
static char g_c_CRLF[] = "\r\n";

/* represents an optional object that's NIL */
static char g_c_NILOBJ [] = "NIL_OBJ";

/* represents that an optional is Present */
static char g_c_OPTIONAL_PRESENT [] = "OPT_PRESENT";

/* represents that an optional is Present */
static char g_c_OPTIONAL_NOT_PRESENT [] = "OPT_NOT_PRESENT";

/* a space */
static char g_c_SP[] = " ";

/* spaces and tabs */
static char g_c_White[] = { ' ', '\t', '\0' };

/* spaces and tabs and URL param/query separators */
static char g_c_objid_ends[] = { ' ', '\t', ';', '?', '\0' };

/* spaces, tabs, and colons */
static char g_c_NameValueDelims[] = { ':', ' ', '\t', '\0' };

/* reason phrases sent back in status lines when an exception occurs */
static char g_c_protocol_exception[] = "Protocol_Exception";
static char g_c_non_protocol_exception[] = "Non_Protocol_Exception";


/* ********************************************************* */
/* Simple singly linked list Operations                      */
/* ********************************************************* */

/* make a list - return new list or NIL on error */
static _ilu_p_list _ilu_make_list (ILU_ERRS((IoErrs)) *p_error) {

	_ilu_p_list p_the_list = ilu_MallocE(sizeof(_ilu_list_s), p_error);
	if (p_the_list == NIL)
		return NIL;

	p_the_list->m_p_start_list_node = NIL;
	p_the_list->m_p_last_list_node = NIL;
	p_the_list->m_card_num_added = 0; 

	return p_the_list;
}


/* add a refany to the list - returns new node or NIL on error */
static _ilu_p_list_node _ilu_add_to_list (_ilu_p_list p_the_list, ilu_refany p_node_contents,
										  ilu_refany p_node_into_contents,
									 ILU_ERRS((IoErrs)) *p_error) {

	_ilu_p_list_node p_the_node = ilu_MallocE(sizeof(_ilu_list_node_s), p_error);
	if (p_the_node == NIL) 
		return NIL;

	p_the_node->m_p_node_contents = p_node_contents;
	p_the_node->m_p_node_into_contents = p_node_into_contents;
	p_the_node->m_p_next_list_node = NIL;

	if (p_the_list->m_p_start_list_node == NIL) { /* first node */
		p_the_list->m_p_start_list_node = p_the_node;
		p_the_list->m_p_last_list_node = p_the_node;
	}
	else {
		p_the_list->m_p_last_list_node->m_p_next_list_node = p_the_node;
		p_the_list->m_p_last_list_node = p_the_node;
	}

	p_the_list->m_card_num_added++;

	return p_the_node;	
}


/* frees a list and everything in it */
static void _ilu_free_list_and_contents (_ilu_p_list p_the_list) {

	_ilu_p_list_node p_next_node;
	_ilu_p_list_node p_a_node = p_the_list->m_p_start_list_node;

	while (p_a_node) {
		p_next_node = p_a_node->m_p_next_list_node;
		ilu_free(p_a_node->m_p_node_contents);
		ilu_free(p_a_node);
		p_a_node = p_next_node;
	}

	ilu_free(p_the_list);
}

/* returns size of the list */
static ilu_cardinal _ilu_list_size (_ilu_p_list p_the_list) {
	return p_the_list->m_card_num_added;
}

/* returns the contents of a node */
static ilu_refany _ilu_list_node_contents (_ilu_p_list_node p_the_node) {
	return p_the_node->m_p_node_contents;
}

/* returns the 'into' contents of a node */
static ilu_refany _ilu_list_node_into_contents (_ilu_p_list_node p_the_node) {
	return p_the_node->m_p_node_into_contents;
}

/* set the contents of a node */
static void _ilu_set_list_node_contents (_ilu_p_list_node p_the_node, ilu_refany p_new_value) {
	p_the_node->m_p_node_contents = p_new_value;
}

/* set the 'into' contents of a node */
static void _ilu_set_list_node_into_contents (_ilu_p_list_node p_the_node, ilu_refany p_new_value) {
	p_the_node->m_p_node_into_contents = p_new_value;
}


/* ********************************************************* */
/* type safe lists for htpp header lines */

static _http_p_header_list _http_make_list (ILU_ERRS((IoErrs)) *p_error) {
	return ((_http_p_header_list)_ilu_make_list(p_error));
}


static _http_p_list_node _http_add_to_list (_http_p_header_list p_the_list, 
											ilu_string p_node_contents,
											ilu_string p_node_into_contents,
											ILU_ERRS((IoErrs)) *p_error) {
	return ((_http_p_list_node)_ilu_add_to_list((_ilu_p_list)p_the_list,
				(ilu_refany)p_node_contents, (ilu_refany)p_node_into_contents, p_error));
}


static void _http_free_list_and_contents (_http_p_header_list p_the_list) {
	_ilu_free_list_and_contents((_ilu_p_list)p_the_list);
}


static ilu_cardinal _http_list_size (_http_p_header_list p_the_list) {
	return (_ilu_list_size((_ilu_p_list)p_the_list));
}


static ilu_string _http_list_node_contents (_http_p_list_node p_the_node) {
	return (ilu_string)(_ilu_list_node_contents((_ilu_p_list_node)p_the_node));
}

static ilu_string _http_list_node_into_contents (_http_p_list_node p_the_node) {
	return (ilu_string)(_ilu_list_node_into_contents((_ilu_p_list_node)p_the_node));
}

/* concatenates the pc_buffer onto the contents of the last node in the list */
static void _http_add_to_last_list_node(_http_p_header_list p_the_list, ilu_string pc_buffer, 
										ILU_ERRS((IoErrs)) *p_error) {

	ilu_cardinal card_into_offset;
	ilu_string pc_contents = p_the_list->m_p_last_list_node->m_p_node_contents;
	ilu_string pc_into = p_the_list->m_p_last_list_node->m_p_node_into_contents;
	ilu_cardinal pc_contents_length = strlen(pc_contents);
	ilu_cardinal pc_buffer_length = strlen(pc_buffer);

	if (pc_into != NIL)
		card_into_offset = pc_into - pc_contents;

	/* increase the size */
	pc_contents = ilu_ReallocE(pc_contents, pc_contents_length + pc_buffer_length + 1, p_error);
	if (ILU_ERRNOK(*p_error))
		return;

	p_the_list->m_p_last_list_node->m_p_node_contents = pc_contents;

	strcat(pc_contents, pc_buffer);  /* append the string */

	if (pc_into != NIL) /* reset the into pointer in case we're in a different spot in memory */
		p_the_list->m_p_last_list_node->m_p_node_into_contents = pc_contents + card_into_offset;
}



/* ********************************************************* */
/* Utility                                                   */
/* ********************************************************* */

/* Asserts where an unexpected http state occurs */
static void _http_unexpected_state_assert(ilu_Call p_call, char* pc_file, int i_line) {
	char c_assert_message_buffer[256];
	sprintf(c_assert_message_buffer, "Unexpected http state %hd for call 0x%p at %s line %d\n",
		(( http_call_info_s *)(p_call->ca_prdata2))->m_call_state, 
		p_call, pc_file, i_line);
	_ilu_Assert(ilu_FALSE, c_assert_message_buffer);
}

/* Asserts where an unexpected http state occurs */
static void _http_unexpected_call_type_assert(ilu_Call p_call, char* pc_file, int i_line) {
	char c_assert_message_buffer[256];
	sprintf(c_assert_message_buffer, "Unexpected http call type %hd for call 0x%p at %s line %d\n",
		(( http_call_info_s *)(p_call->ca_prdata2))->m_call_type, 
		p_call, pc_file, i_line);
	_ilu_Assert(ilu_FALSE, c_assert_message_buffer);
}

#ifdef ENABLE_DEBUGGING
static _http_call_state _http_get_state_of_call(ilu_Call p_call, char* pc_file, int i_line) {
	if (ilu_DebugLevel & HTTP_DEBUG)
		ilu_DebugPrintf("_http_get_call_state: call 0x%x state is %hd in %s line %d\n",
		p_call, (( http_call_info_s *)(p_call->ca_prdata2))->m_call_state, pc_file, i_line );
	return ((http_call_info_s*)(p_call->ca_prdata2))->m_call_state;
}

static _http_call_state _http_set_state_of_call(ilu_Call p_call, _http_call_state new_state, char* pc_file, int i_line) {
	if (ilu_DebugLevel & HTTP_DEBUG)
		ilu_DebugPrintf("_http_set_call_state: call 0x%x from %hd to %hd in %s line %d \n",
		p_call, (( http_call_info_s *)(p_call->ca_prdata2))->m_call_state, new_state, pc_file, i_line );
	return ((http_call_info_s*)(p_call->ca_prdata2))->m_call_state = new_state;
}


static _http_call_type _http_get_type_of_call(ilu_Call p_call, char* pc_file, int i_line) {
	if (ilu_DebugLevel & HTTP_DEBUG)
		ilu_DebugPrintf("_http_get_call_type: call 0x%x type is %hd in %s line %d\n",
		p_call, (( http_call_info_s *)(p_call->ca_prdata2))->m_call_type, pc_file, i_line );
	return ((http_call_info_s*)(p_call->ca_prdata2))->m_call_type;
}

static _http_call_type _http_set_type_of_call(ilu_Call p_call, _http_call_type new_type, char* pc_file, int i_line) {
	if (ilu_DebugLevel & HTTP_DEBUG)
		ilu_DebugPrintf("_http_set_call_type: call 0x%x from %hd to %hd in %s line %d \n",
		p_call, (( http_call_info_s *)(p_call->ca_prdata2))->m_call_type, new_type, pc_file, i_line );
	return ((http_call_info_s*)(p_call->ca_prdata2))->m_call_type = new_type;
}

#endif /* ENABLE_DEBUGGING */


/* ********************************************************* */
/* Set up an in memory transport buffer into which the rest of the call will
be written so that we can determine the body length in order to add a 
content length header */ 

static void _http_switch_to_buffering_body(ilu_Call p_call, ilu_Error* p_error) {
	
	ilu_Transport p_buffered_body_transport = _ilu_BufferTransport_Create(HTTP_BODY_ALLOC_SIZE, NIL, p_error);
	if (ILU_ERRNOK(*p_error))
		return;
	
	/* set it to be the transport used for this call */
	p_call->ca_prTrans = p_buffered_body_transport;
	
	_http_set_to_buffered_transport(p_call) = ilu_TRUE;
}


/* ********************************************************* */
/* Takes the bytes out of our memory transport buffer, send an appropriate
content-length header, and then actually send out the body bytes */ 

static void _http_send_buffered_body(ilu_Call p_call, ilu_Error* p_error) {

	ilu_cardinal card_length;
	ilu_bytes p_body_bytes;
	char c_buffer[64];

	/* get the bytes and length out of the body buffer transport */
	_ilu_BufferTransport_Destroy(http_call_tport(p_call), &card_length, &p_body_bytes, p_error);
	if (ILU_ERRNOK(*p_error))
		return;
	
	/* set the transport back to the real one used for this call */
	p_call->ca_prTrans = connection_transport(p_call->ca_connection);
	_http_set_to_buffered_transport(p_call) = ilu_FALSE;
	
	/* write out a content length header based on what's in the transport buffer */
	/* auto generate a content length header */
	
	sprintf(c_buffer, "Content-Length: %lu\r\n\r\n", (unsigned long int) card_length);
	
	_http_transport_write_bytes(http_call_tport(p_call), c_buffer, strlen(c_buffer), p_error);
	if (ILU_ERRNOK(*p_error))
		return;
	
	/* write out the body bytes we had saved in the transport buffer */
	_http_transport_write_bytes(http_call_tport(p_call), p_body_bytes, card_length, p_error);
	if (ILU_ERRNOK(*p_error))
		return;
	
	ilu_free(p_body_bytes);
	
	return;
}


/* ********************************************************* */
/* strtok_r'ish - written because visual c++ runtime doesn't have strtok_r 
pc_string_to_search is searched looking for the first token in it that
is deliminated by characters in pc_delim_chars.  The token is null terminated
and a pointer to where it starts returned.  *ppc_where_to_next_search
is set to where the search for the next token should begin.  NIL is returned
when there is nothing left to find. If p_the_delim is not NIL, then the
char where it points is set to the deliminator that was actually found - 
note that this can be '\0' when there were no more tokens.
*/

static char* _http_strtok_r (char* pc_string_to_search, char* pc_delim_chars, 
							 char** ppc_where_to_next_search, char* p_the_delim) {

	char* pc_start = pc_string_to_search;
	char* pc_end;

	if (!pc_string_to_search || !ppc_where_to_next_search || !pc_delim_chars)
		return NIL;

	while (*pc_start && strchr(pc_delim_chars, *pc_start) != NULL)
		pc_start++;

	if (*pc_start == '\0') /* nothing but delims */
		return NIL;

	pc_end = pc_start;	/* search for end */

	while (*pc_end && strchr(pc_delim_chars, *pc_end) == NULL)
		pc_end++;

	if (p_the_delim) /* make note of deliminator if we have a valid pointer */
		*p_the_delim = *pc_end;

	if (*pc_end == '\0')  /* reached end of string to search */
		*ppc_where_to_next_search = NIL;
	else {	/* null terminate and set where to start looking for the next token */
		*pc_end = '\0';
		*ppc_where_to_next_search = pc_end + 1;
	}

	return pc_start;
}


/* ********************************************************* */
/* returns the reason phrase associated with the status code */
static ilu_string _http_phrase_of_status_code(ilu_shortcardinal scard_enum_code) {
	
	switch (scard_enum_code) {
	case 100:   return "Continue";
	case 101:   return "Switching Protocols";
	case 200:   return "OK";
	case 201:   return "Created";
	case 202:   return "Accepted";
	case 203:   return "Non-Authoritative Information";
	case 204:   return "No Content";
	case 205:   return "Reset Content";
	case 206:   return "Partial Content";
	case 300:   return "Multiple Choices";
	case 301:   return "Moved Permanently";
	case 302:   return "Moved Temporarily";
	case 303:   return "See Other";
	case 304:   return "Not Modified";
	case 305:   return "Use Proxy";
	case 400:   return "Bad Request";
	case 401:   return "Unauthorized";
	case 402:   return "Payment Required";
	case 403:   return "Forbidden";
	case 404:   return "Not Found";
	case 405:   return "Method Not Allowed";
	case 406:   return "Not Acceptable";
	case 407:   return "Proxy Authentication Required";
	case 408:   return "Request Time-out";
	case 409:   return "Conflict";
	case 410:   return "Gone";
	case 411:   return "Length Required";
	case 412:   return "Precondition Failed";
	case 413:   return "Request Entity Too Large";
	case 414:   return "Request-URI Too Large";
	case 415:   return "Unsupported Media Type";
	case 500:   return "Internal Server Error";
	case 501:   return "Not Implemented";
	case 502:   return "Bad Gateway";
	case 503:   return "Service Unavailable";
	case 504:   return "Gateway Time-out";
	case 505:   return "HTTP Version not supported";
	default :	return "extension-code";
	}
	
}




/* ********************************************************* */
/* returns a pointer to the first occurrance of pc_substr in pc_to_search 
  assumes pc_to_search is at least as (validly) long as ul_search_length,
  and that pc_substr is NIL terminated. If b_case_sensitive is ilu_FALSE
  then for alphabetic characters, case doesn't matter in the comparison. */

static char* _http_strnstr ( ilu_bytes pc_to_search, 
							 char* pc_substr,
							 unsigned long ul_search_length,
							 ilu_boolean b_case_sensitive) {

	ilu_bytes pc_search_walker;
	ilu_bytes pc_search_runner;
	ilu_bytes pc_substr_runner;
	unsigned long ul_last_position;
	unsigned long ul_substr_length = strlen(pc_substr);

	if ((pc_to_search == NIL) || (ul_substr_length > ul_search_length))
		return (NIL); /* substring couldn't possibly fit */

	/* try each position along the string to search in turn */
	pc_search_walker = pc_to_search;
	for (ul_last_position = ul_search_length - ul_substr_length + 1;
		 ul_last_position > 0; 
		 ul_last_position--) {

		/* at this position, see if there's a match */
		pc_search_runner = pc_search_walker;
		pc_substr_runner = (ilu_bytes) pc_substr;

		if (b_case_sensitive == ilu_TRUE) {
			while (*pc_substr_runner) { 
				if (*pc_substr_runner != *pc_search_runner)
					break;
				pc_search_runner++;
				pc_substr_runner++;
			}
		}
		else {
			while (*pc_substr_runner) { 
				if (isalpha(*pc_substr_runner) && isalpha(*pc_search_runner) &&
					(tolower(*pc_substr_runner) == tolower(*pc_search_runner))) ;
				else if (*pc_substr_runner == *pc_search_runner) ;
				else break;
			pc_search_runner++;
			pc_substr_runner++;
			}
		}

		/* if we reached the end - they must be equal */
		if (!*pc_substr_runner) 
			return((char*)pc_search_walker);

		pc_search_walker++;
	}

	return(NIL);
}



/* ********************************************************* */
/* inefficient way to read in a line but necessary since
   there's no way to know in advance how much a browser will be sending
   in its request.  We cant take a larger grained 'chunking' approach 
   because we could end up trying to read more than what was
   sent, and hang there since we don't get an eof because the connection 
   is still open - sigh...           */
/* Returns a null terminated buffer containing all the bytes from the
call's transport upto but not including the next end of line encountered.  
The end of line is discarded;  NIL is returned on any error and p_error may be set
accordingly.  if p_first_byte is non NIL it is assumed to point the a
byte that is to be considered the first byte read. */

static ilu_string _http_readline(ilu_Call p_call, ilu_byte* pc_first_byte, ILU_ERRS((IoErrs)) *p_error) {

	ilu_cardinal card_num_chars_read;
	ilu_cardinal card_space_left;
	ilu_string pc_buffer;
	ilu_string pc_newbuffer;
	ilu_string pc_where;
#ifdef HTTP_STRICT_END_OF_LINE
	ilu_boolean b_found_cr = ilu_FALSE;
#endif
	ilu_Transport p_transport = http_call_tport(p_call);

	/* allocate some initial buffer space */
	if (pc_first_byte) {
		if ((pc_buffer = (ilu_string) ilu_MallocE( HTTP_READLINE_ALLOC_SIZE + 1, p_error))
			== NIL)
			return NIL;
		card_num_chars_read = 1;
		pc_where = pc_buffer + 1;
		*pc_buffer = *pc_first_byte;
	}
	else {
		if ((pc_buffer = (ilu_string) ilu_MallocE( HTTP_READLINE_ALLOC_SIZE, p_error))
			== NIL)
			return NIL;
		card_num_chars_read = 0;
		pc_where = pc_buffer;
		
	}
	card_space_left = HTTP_READLINE_ALLOC_SIZE;


	while (1) {	/* loop reading chars, looking for end of line */

		if (card_space_left == 0) { /* if we're out of space, grow */
			pc_newbuffer = ilu_ReallocE(pc_buffer, 
				card_num_chars_read + HTTP_READLINE_ALLOC_SIZE, p_error);
			if (pc_newbuffer == NIL) {
				ilu_free(pc_buffer);
				return NIL;
			}
			else { 
				pc_buffer = pc_newbuffer;
				pc_where = pc_buffer + card_num_chars_read;
				card_space_left = HTTP_READLINE_ALLOC_SIZE;
			}
		}

		/* get our single byte */
		_http_transport_read_onebyte(p_transport, pc_where, p_error);
		if (ILU_ERRNOK(*p_error)) {
			ilu_free(pc_buffer);
			return NIL;
		}

		card_space_left--;		/* adjust counts */
		card_num_chars_read++;

#ifdef HTTP_STRICT_END_OF_LINE

		/* end of line is a CRLF */
		switch (*pc_where) {

		case '\r':
			b_found_cr = ilu_TRUE;
			break;

		case '\n':
			if (b_found_cr != ilu_TRUE)
				break;

			/* found crlf, null terminate */ 
			*(pc_where - 1) = '\0';

			return pc_buffer;

		default:
			b_found_cr = ilu_FALSE;
		}

#else
		/* allow just a lf to indicate end of line ( to accomodate some http servers) */

		if (*pc_where == '\n') { /* found end of line */

			if ((pc_where != pc_buffer) && (*(pc_where - 1) == '\r')) {
				/* drop back to any any preceeding CR */
				card_num_chars_read--;
				pc_where--;
			}

			*pc_where = '\0';	/* null terminate */

			return pc_buffer;
		}

#endif 

		pc_where++;
	}
}


/* ********************************************************* */
/* Put the request line into the m_pc_request_or_status_line
   in the call's _http_call_info_s structure, and fill in all the
   other parts of the _http_call_info_s structure that can be 
   deduced from it, i.e. method name, object id and version.
   Returns ilu_TRUE if there were no errors, else false.  
   errors may be set if they occur */

static ilu_boolean _http_get_request_line(ilu_Call p_call, ilu_byte* pc_first_byte,
											ILU_ERRS((IoErrs)) *p_error) {

	ilu_string pc_next_token;
	char c_delim_found;

	/* read in a line */
	if ((_http_request_line(p_call) = _http_readline(p_call, pc_first_byte, p_error)) == NIL)
		goto bad_request_line;

	/* get the method name */
	if ((_http_method_name(p_call) = 
		_http_strtok_r(_http_request_line(p_call), g_c_White, &pc_next_token, NIL)) == NIL)
		goto bad_request_line;

	/* make note if it's a GET or HEAD method */
	if (strcmp("GET", _http_method_name(p_call)) == 0)
		_http_method_kind(p_call) = ILU_HTTP_GET_METHOD;
	else if (strcmp("HEAD", _http_method_name(p_call)) == 0)
		_http_method_kind(p_call) = ILU_HTTP_HEAD_METHOD;

	/* get the object id */
	if ((_http_request_uri(p_call) = 
		_http_strtok_r(pc_next_token, g_c_objid_ends, &pc_next_token, &c_delim_found)) == NIL)
		goto bad_request_line;

	/* for 1.1, or 1.0 implementing a proxy, allow full URI to come in */
	if (_http_strnstr((ilu_bytes)(_http_request_uri(p_call)), "http://", 7, ilu_FALSE) == _http_request_uri(p_call)) {
		
		/* get some memory into which to extract out just the id */
		ilu_string pc_just_object_id;
		
		/* parse out just the id part from the full url */
		if (!_ilu_Parse_HTTP_URL (_http_request_uri(p_call), &pc_just_object_id, NIL, NIL, NIL, NIL, NIL, p_error)) {
			ilu_free(pc_just_object_id);
			return ilu_FALSE;
		}
		
		/* set location of just object ID */ 
		
		_http_object_id(p_call) = strstr(_http_request_uri(p_call), pc_just_object_id);
		ilu_free(pc_just_object_id);
		
	}
	else { /* must not be an absolute uri */
		_http_object_id(p_call) = _http_request_uri(p_call);
	}
	

	if ((c_delim_found == ';') || (c_delim_found == '?')) {
		/* the deliminator indicates that there were some params or queries */

		/* save what the found delimator was - we'll need it later when we 
		reconstruct the RequestURI part of the Request */
		_http_params_queries_delim(p_call) = c_delim_found;

		/* get the param or queries */
		if ((_http_params_queries(p_call) = 
			_http_strtok_r(pc_next_token, g_c_White, &pc_next_token, NIL)) == NIL)
			goto bad_request_line;
	}

	/* get the version */
	if ((_http_version_string(p_call) = 
		_http_strtok_r(pc_next_token, g_c_White, &pc_next_token, NIL)) == NIL)
		goto bad_request_line;

	/* check the version of the incoming call against this call's protocol version */
	if ((sscanf(_http_version_string(p_call), "HTTP/%hu.%hu", &(_http_major_version(p_call)), 
				&(_http_minor_version(p_call))) != 2) ||
		(_http_major_version(p_call) != _http_protocol_major_version(p_call)))
		return ILU_ERR_CONS1(marshal, p_error, minor, ilu_mm_versionMismatch, ilu_FALSE);

	/* never respond with a minor version higher that the protocol we're doing */
	if (_http_minor_version(p_call) > _http_protocol_minor_version(p_call))
			_http_minor_version(p_call) = _http_protocol_minor_version(p_call);

	/* ensure keep alive initialized properly according to version */
	if (_http_minor_version(p_call) == 1 &&
		_http_major_version(p_call) == 1 )
		_http_keep_alive(p_call) = ilu_TRUE;
	else _http_keep_alive(p_call) = ilu_FALSE;

	return ilu_TRUE;


	bad_request_line:

	if (ILU_ERRNOK(*p_error)) /* return any already set error */
		return ilu_FALSE;

	return ILU_ERR_CONS1(marshal, p_error, minor, ilu_mm_msgTypeUnknown, ilu_FALSE);
}



/* ********************************************************* */
/* Put the status line into the m_pc_request_or_status_line
   in the call's _http_call_info_s structure, and fill in all the
   other parts of the _http_call_info_s structure that can be 
   deduced from it, i.e. statuscode and version.
   Returns ilu_TRUE if the line was not a 'simple-status',
   else false.  errors may be set if they occur */

static ilu_boolean _http_get_status_line(ilu_Call p_call, ilu_byte* pc_first_byte,
											ILU_ERRS((IoErrs)) *p_error) {

	ilu_string pc_next_token;

ignored_100_status_line_loop:

	/* read in a line */
	if ((_http_status_line(p_call) = _http_readline(p_call, pc_first_byte, p_error)) == NIL) {
		pc_first_byte = NIL;
		goto bad_status_line;
	}

	pc_first_byte = NIL;

	/* get the version string */
	if ((_http_version_string(p_call) = 
		_http_strtok_r(_http_status_line(p_call), g_c_White, &pc_next_token, NIL)) == NIL)
		goto bad_status_line;

	/* check for valid version */
	if ((sscanf(_http_version_string(p_call), "HTTP/%hu.%hu", &(_http_major_version(p_call)), 
				&(_http_minor_version(p_call))) != 2) ||
		(_http_major_version(p_call) != _http_protocol_major_version(p_call)))
		return ILU_ERR_CONS1(marshal, p_error, minor, ilu_mm_versionMismatch, ilu_FALSE);

	/* ensure keep alive initialized properly */
	if (_http_minor_version(p_call) == 1 &&
		_http_major_version(p_call) == 1 )
		_http_keep_alive(p_call) = ilu_TRUE;
	else _http_keep_alive(p_call) = ilu_FALSE;


	/* get the status code string */
	if (( _http_status_code_string(p_call) = 
		_http_strtok_r(pc_next_token, g_c_White, &pc_next_token, NIL)) == NIL)
		goto bad_status_line;

	/* assign the status code */
	if (sscanf(_http_status_code_string(p_call), "%hd", &(_http_status_code(p_call))) != 1)
		goto bad_status_line;

	/* xxx if it's a 1.1 response that's a 100 (Continue), then simply ignore it */
	if (_http_status_code(p_call) == 100 &&
		_http_minor_version(p_call) == 1 &&
		_http_major_version(p_call) == 1) {
		ilu_free(_http_status_line(p_call));
		_http_status_line(p_call) = NIL;
		goto ignored_100_status_line_loop;
	}

	/* get the reason phrase */
	_http_version_reason_phrase(p_call) = _http_strtok_r(pc_next_token, g_c_White, &pc_next_token, NIL);

	return ilu_TRUE;


	bad_status_line:

	if (ILU_ERRNOK(*p_error)) /* return any already set error */
		return ilu_FALSE;

	return ILU_ERR_CONS1(marshal, p_error, minor, ilu_mm_msgTypeUnknown, ilu_FALSE);
}



/* ********************************************************* */
/* Put all the incoming headers of a request or reply into the 
   _http_p_header_list in the call's _http_call_info_s structure.
   Also sets content length and server id if these headers are present;
   Returns number of headers */

static ilu_cardinal _http_fill_headers_list(ilu_Call p_call, 
											ILU_ERRS((IoErrs)) *p_error) {

	ilu_string pc_buffer;
	ilu_string pc_value_start;
	ilu_string pc_next_token;
	ilu_boolean b_keep_alive_header_force_close = ilu_FALSE;

	if ((_http_headers_list(p_call) = _http_make_list(p_error)) == NIL)
			return 0;

	while (1) {
			
		/* read in a line */
		if ((pc_buffer = _http_readline(p_call, NIL, p_error)) == NIL)
			goto bad_header;

		if (*pc_buffer == '\0') { /* if we hit the body 
			(note - transport is now ready to read the first byte of the body if any) */
			ilu_free(pc_buffer);
			_http_headers_current_node(p_call) = _http_headers_list(p_call)->m_p_start_list_node;
			return _http_list_size(_http_headers_list(p_call));
		}
		
		if (*pc_buffer == ' ' || *pc_buffer == '\t') { 
			/* must be continuation of previous header's value */
			_http_add_to_last_list_node(_http_headers_list(p_call), pc_buffer, p_error);
			if (ILU_ERRNOK(*p_error)) 
				goto bad_header;
			continue;
		}

		/* null terminate the name  */
		if (_http_strtok_r(pc_buffer, g_c_NameValueDelims, &pc_next_token, NIL) == NIL)
			goto bad_header;

		/* find the value  */
		pc_value_start = pc_next_token;
		if (pc_value_start)
		  skipwhite(pc_value_start);

		/* see if the header is anything we want to take note of */

		/* look for ILU server ID */
		if (strcmp(pc_buffer, ILU_SERVER_ID_HEADER_NAME) == 0) {
			_http_server_id(p_call) = pc_value_start;
			goto header_checked;
		}
		
		/* look for content length header */
		if (_http_strnstr((ilu_bytes)pc_buffer, "Content-Length", strlen(pc_buffer), ilu_FALSE) != NULL) {
			unsigned long int val;
			if (sscanf(pc_value_start, "%lu", &val) != 1) {
			  _http_body_length(p_call) = 0;
			  goto bad_header;
			} else
			  _http_body_length(p_call) = val;
			goto header_checked;
		}
		
		/* check for Connection headers */
		if ((_http_strnstr((ilu_bytes)pc_buffer, "Connection", strlen(pc_buffer), ilu_FALSE) != NULL) ||
		    (_http_strnstr((ilu_bytes)pc_buffer, "Proxy-Connection", strlen(pc_buffer), ilu_FALSE) != NULL)) {
			
			
		  if (_http_strnstr((ilu_bytes)pc_value_start, "Keep-Alive", strlen(pc_value_start), ilu_FALSE) != NULL) {
		    /* if we're 1_0p or 1.1, accept keep-alive */
		    if ( _http_persistent(p_call) || 
			 (_http_protocol_minor_version(p_call) == 1 && _http_protocol_major_version(p_call) == 1) )
		      /* in case a Keep-Alive header appeared before the connection header */
		      if (!b_keep_alive_header_force_close) 
			_http_keep_alive(p_call) = ilu_TRUE;
		  }
		  else if (_http_strnstr((ilu_bytes)pc_value_start, "close", strlen(pc_value_start), ilu_FALSE) != NULL) {
		    /* 1.1 defaults to keep alive unless there's a close */ 
		    if (_http_minor_version(p_call) == 1 && _http_major_version(p_call) == 1 )
		      _http_keep_alive(p_call) = ilu_FALSE;				
		  }
			
		  goto header_checked;
		}

		/* check for Keep-Alive headers */
		if ((_http_strnstr((ilu_bytes)pc_buffer, "Keep-Alive", strlen(pc_buffer), ilu_FALSE) != NULL) ||
		    (_http_strnstr((ilu_bytes)pc_buffer, "Proxy-Keep-Alive", strlen(pc_buffer), ilu_FALSE) != NULL)) {

		  /* in ilu, there's nothing we can do to deal with the timeout and/or max parameters of
		     a Keep-Alive header in 1.0, so if one's specified, we just set up to have the connection close */

		  if (_http_minor_version(p_call) == 0 && _http_major_version(p_call) == 1 ) {
		    _http_keep_alive(p_call) = ilu_FALSE;
		    /* in case a Keep-Alive header appeared before the connection header */
		    b_keep_alive_header_force_close = ilu_TRUE;
		  }
			
		  goto header_checked;
		}
		
		/* look for expect headers */
		if (_http_strnstr((ilu_bytes)pc_buffer, "Expect", strlen(pc_buffer), ilu_FALSE) != NULL) {
			
			if (_http_minor_version(p_call) == 1 && _http_major_version(p_call) == 1) {
				
				if ((_http_strnstr((ilu_bytes)pc_value_start, "100-continue", strlen(pc_value_start), ilu_FALSE) != NULL) &&
					_http_get_call_type(p_call) == srvr4something) {
					char c_buffer[128];
					/* xxx send a continue header since we currently have no way of asking the true object 
					if it would process this*/
					sprintf(c_buffer, "HTTP/%d.%d %hd %s\r\n", _http_major_version(p_call), _http_minor_version(p_call),
						100, _http_phrase_of_status_code(100));					
					_http_transport_write_bytes(http_call_tport(p_call), c_buffer, strlen(c_buffer), p_error);					
				}
				
				/* we don't understand any other sort of expectation perhaps the ilu user does so we'll let it pass */
				
				goto header_checked;
			}
		}

header_checked:
				
		/* stick the header into the list */
		_http_add_to_list(_http_headers_list(p_call), pc_buffer, pc_value_start, p_error);
		if (ILU_ERRNOK(*p_error)) 
			goto bad_header;
	}

	bad_header :

	if (pc_buffer)				/* cleanup */
		ilu_free(pc_buffer);

	if (ILU_ERRNOK(*p_error)) /* return any already set error */
		return _http_list_size(_http_headers_list(p_call));

	return ILU_ERR_CONS1(marshal, p_error, minor, ilu_mm_msgTypeUnknown, 
		_http_list_size(_http_headers_list(p_call)));
}


/* ********************************************************* */
/*  if the named header is found sets value pointer to the value
 of the header and returns ilu_TRUE, else returns ilu_FALSE and
 sets *ppc_value to NIL */

static ilu_boolean _http_find_header_value(ilu_Call p_call, char* pc_header_name, char** ppc_value) {
	
	_http_p_header_list p_header_list = _http_headers_list(p_call);
	_http_p_list_node p_list_node;
	
	if (!p_header_list) 
		return ilu_FALSE;
	
	/* walk down the nodes in the list, checking for the name */
	p_list_node = p_header_list->m_p_start_list_node;
	while (p_list_node) {
		if (_http_list_node_contents(p_list_node) &&
			strcmp(pc_header_name, _http_list_node_contents(p_list_node)) == 0) {
			/* found it so return the values */
			*ppc_value = _http_list_node_into_contents(p_list_node);
			return ilu_TRUE;
		}
		p_list_node = p_list_node->m_p_next_list_node;
	}
	
	*ppc_value = NIL;
	return ilu_FALSE;
	
}


/* ********************************************************* */
/* writes a cardinal out followed by a a crlf          
   returns ilu_TRUE on success, else ilu_FALSE               */

static ilu_boolean _http_write_cardinal_line(ilu_Call p_call, 
									   ilu_cardinal card_to_write, 
									   ILU_ERRS((IoErrs)) * p_error) {

	char c_buffer [16];

	ILU_CLER(*p_error);

	/* write out the cardinal value */
	sprintf(c_buffer, "%lu\r\n", (unsigned long int) card_to_write);
	_http_transport_write_bytes(http_call_tport(p_call), (ilu_bytes) c_buffer, strlen(c_buffer), p_error);

	if (ILU_ERRNOK(*p_error))
		return ilu_FALSE;

	return ilu_TRUE;
}


/* ********************************************************* */
/* reads a line and sets a cardinal based on it's contents          
   returns ilu_TRUE on success, else ilu_FALSE               */

static ilu_boolean _http_read_cardinal_line(ilu_Call p_call, 
					    ilu_cardinal* p_card_to_set, 
					    ILU_ERRS((IoErrs)) * p_error) {

	ilu_string pc_buffer;
	unsigned long int val;

	ILU_CLER(*p_error);

	if ((pc_buffer = _http_readline(p_call, NIL, p_error)) == NIL)
		return ilu_FALSE;

	/* readin the cardinal value */
	if (sscanf(pc_buffer, "%lu\r\n", &val) != 1) {
		ilu_free(pc_buffer);
		return ilu_FALSE;
	} else
	  *p_card_to_set = val;

	ilu_free(pc_buffer);
	return ilu_TRUE;
}


/* ********************************************************* */
/* writes a integer out followed by a a crlf          
   returns ilu_TRUE on success, else ilu_FALSE               */

static ilu_boolean _http_write_integer_line(ilu_Call p_call, 
									   ilu_integer integer_to_write, 
									   ILU_ERRS((IoErrs)) * p_error) {

	char c_buffer [16];

	ILU_CLER(*p_error);

	/* write out the integer value */
	sprintf(c_buffer, "%ld\r\n", (long int) integer_to_write);
	_http_transport_write_bytes(http_call_tport(p_call), (ilu_bytes) c_buffer, strlen(c_buffer), p_error);

	if (ILU_ERRNOK(*p_error))
		return ilu_FALSE;

	return ilu_TRUE;
}


/* ********************************************************* */
/* reads a line and sets a integer based on it's contents          
   returns ilu_TRUE on success, else ilu_FALSE               */

static ilu_boolean _http_read_integer_line(ilu_Call p_call, 
					   ilu_integer* p_integer_to_set, 
					   ILU_ERRS((IoErrs)) * p_error) {

	ilu_string pc_buffer;
	long int val;

	ILU_CLER(*p_error);

	if ((pc_buffer = _http_readline(p_call, NIL, p_error)) == NIL)
		return ilu_FALSE;

	/* readin the integer value */
	if (sscanf(pc_buffer, "%ld\r\n", &val) != 1) {
		ilu_free(pc_buffer);
		return ilu_FALSE;
	} else
	  *p_integer_to_set = val;

	ilu_free(pc_buffer);
	return ilu_TRUE;
}



/* ********************************************************* */
/* Uses p_transport to read bytes in chunks of card_chunk_size, to into a buffer 
pointed to by * ppc_buffer_loc, until at least pc_termination_string is seen.
NIL is put into the buffer to terminate where the pc_termination_string was found.
If card_num_initial_bytes is non zero, then pc_initial_bytes indicates card_num_initial_bytes
initial bytes to treat as if they were read from the transport first.  If
p_card_leftover_length is non zero, ppc_leftover_start points to p_card_leftover_length
bytes that were read past the termination string . If b_read_till_eof is true, 
pc_termination_string is ignored and eof is considered to be the stopping condition.
*p_card_total_read gets set to num bytes in allocated buffer before termination reached.
ilu_FALSE is returned if any error (except eof) is encountered.

ilu_Transport p_transport,		- pointer to the transport from which to read bytes
char** ppc_buffer_loc,			- where to put the address of the allocated buffer
ilu_cardinal card_chunk_size,	- size of allocations to use
ilu_bytes pc_termination_string,		- what denotes the end of the thing we're trying to get
ilu_bytes pc_initial_bytes,		- some initial bytes to put in
ilu_cardinal card_num_initial_bytes,	- how many initial bytes there are
ilu_bytes* ppc_leftover_start,			- where to set pointer to start of any leftover bytes from the read
ilu_cardinal* p_card_leftover_length,	- where to write how many leftover bytes there are
ilu_boolean b_read_till_eof,			- if true, igores pc_termination_string and considers eof to be the stopping condition
ilu_cardinal* p_card_total_read			- get set to num bytes in allocated buffer before termination reached
ILU_ERRS((IoErrs)) *p_error)		- set if an error

*/

static ilu_boolean 
_http_fill_buffer_till (ilu_Transport p_transport,
						ilu_bytes* ppc_buffer_loc,
						ilu_cardinal card_chunk_size,
						ilu_string pc_termination_string,
						ilu_bytes pc_initial_bytes,
						ilu_cardinal card_num_initial_bytes,
						ilu_bytes* ppc_leftover_start,
						ilu_cardinal* p_card_leftover_length,
						ilu_boolean b_read_till_eof,
						ilu_cardinal* p_card_total_read,
						ILU_ERRS((IoErrs)) *p_error) {

	ilu_bytes pc_end;				/* points at end of what we're looking for */
	ilu_bytes pc_where;				/* where we're beginning search for pc_termination_string*/
	ilu_cardinal card_num_read;		/* how may bytes the transport read returned */
	ilu_cardinal card_where_offset;	/* offset from start of buffer where we search */
	ilu_cardinal card_term_str_length;	/* length of termination string */
	ilu_cardinal card_last_chunk_size;	/* size of last allocation - to deal with initial bytes*/
	ilu_boolean b_hit_eof = ilu_FALSE;	/* true if we hit the end of file */

	ILU_CLER(*p_error);

	*p_card_total_read = 0;		/* nothing read so far */

	/* get some initial  memory */
	if (card_num_initial_bytes > 0) {
		if (((*ppc_buffer_loc) = 
			(ilu_bytes) ilu_MallocE(card_num_initial_bytes, p_error)) == NIL)
			return ilu_FALSE; /* no memory */
		memcpy((void *) *ppc_buffer_loc, (void *) pc_initial_bytes, card_num_initial_bytes);
		card_last_chunk_size = card_num_initial_bytes;
	}
	else {
		if (((*ppc_buffer_loc) = 
			(ilu_bytes) ilu_MallocE(card_chunk_size, p_error)) == NIL)
			return ilu_FALSE; /* no memory */
		card_last_chunk_size = card_chunk_size;
	}

	card_term_str_length = strlen(pc_termination_string);

	pc_where = (*ppc_buffer_loc);	/* init our search pointer */
	while (ilu_TRUE) { /* keep looking for pc_termination_string */

		/* read in chunk  */
		if (card_num_initial_bytes == 0) {
			/* we're done dealing with any initial bytes */
			card_num_read = _http_transport_read_bytes(p_transport, pc_where, 
				card_chunk_size, p_error);
		}
		else {
			card_num_read = card_num_initial_bytes;
			card_num_initial_bytes = 0;
		}

		*p_card_total_read = *p_card_total_read + card_num_read; /* add to count of bytes read */
		
		ILU_ERR_SWITCH(*p_error) {
			ILU_SUCCESS_CASE ;
			ILU_ERR_CASE(comm_failure, p_the_err_struct) {
				if (p_the_err_struct->minor == ilu_cfm_eof)
						b_hit_eof = ilu_TRUE;
				else return ilu_FALSE;
			}
			ILU_ERR_ELSE
				 /* had a non eof problem reading, we're just hosed */
				return ilu_FALSE;
		} ILU_ERR_ENDSWITCH;

		/* see if there was a pc_termination_string in what we read */
		if (b_read_till_eof == ilu_TRUE)
			pc_end = NIL;
		else 
			pc_end = (ilu_bytes)_http_strnstr ( pc_where, pc_termination_string, card_num_read, ilu_TRUE );
							 
		if (pc_end != NIL) {	/* found it - no need to read in any more */

			*pc_end = '\0'; /* NIL terminate where the pc_termination_string was found */

			 /* leftovers start just after the pc_termination_string */
			*ppc_leftover_start = pc_end + card_term_str_length;

			/* and we have this many bytes of them */
			*p_card_leftover_length = card_num_read - 
				(pc_end + card_term_str_length - pc_where);

			/* reset number read to reflect count of bytes before termination string */
			*p_card_total_read = pc_end - (*ppc_buffer_loc);

			/* clear any eof error since we got what we were after */
			if (b_hit_eof == ilu_TRUE)	
				ILU_CLER(*p_error);

			return ilu_TRUE;
		}
		else if (b_hit_eof == ilu_TRUE) { 
			if (b_read_till_eof == ilu_TRUE) {
				/* we were supposed to read to eof, so things are cool */
				ILU_CLER(*p_error);
				return ilu_TRUE;
			}
			/* hit end of file and still haven't found it */
			else
				return ilu_FALSE;
		}
		else {	/* haven't got all we need yet */ 

			/* determine how far into what we've read so far we've searched */
			card_where_offset = pc_where + card_last_chunk_size -
				(*ppc_buffer_loc);

			/* increase our buffer size by another chunk */
			if (((*ppc_buffer_loc) = 
				(ilu_bytes) ilu_ReallocE((*ppc_buffer_loc),
					card_where_offset + card_chunk_size, p_error)) == NIL)
				return ilu_FALSE; /* no memory */

			card_last_chunk_size = card_chunk_size; /* for initial bytes case */

			/* set where we are to continue searching */
			pc_where = (*ppc_buffer_loc) + card_where_offset;
		}
	}
}


/* ********************************************************* */
/* returns the method from the root class that has the given 
   name (if any)  */

static ilu_Method _http_root_method_from_name (ilu_string pc_the_method_name) {

	ilu_cardinal card_method_index;
	ilu_Method p_the_method;

	/* search internal (ilu_rootClass) methods */
	p_the_method = class_methods(ilu_rootClass);

	for(card_method_index = 0; card_method_index < class_method_count(ilu_rootClass);
		card_method_index++) {

		if (strcmp(method_name(p_the_method), pc_the_method_name) == 0) 
			return p_the_method;

		p_the_method++;
	}

	return NIL;
}

	 
/* ********************************************************* */
/* returns the method and sets class that has the given method name */

 static ilu_Method _http_inherited_method_from_name (ilu_Class* pp_class, ilu_string pc_the_method_name) {

	ilu_cardinal card_method_index;
	ilu_Method p_the_method;
	ilu_cardinal card_index_super_classes;
	ilu_cardinal card_super_classes_count;
	ilu_Class* pp_super_class;

	/* search this class */
	p_the_method = class_methods(*pp_class);
	for(card_method_index = 0; card_method_index < class_method_count(*pp_class);
		card_method_index++) {

		if (strcmp(method_name(p_the_method), pc_the_method_name) == 0) 
			return p_the_method;

	p_the_method++;
	}

	/* now (recursively depth first) search the super classes */
	pp_super_class = class_superclasses(*pp_class);
	card_super_classes_count = class_superclass_count(*pp_class);
	for (card_index_super_classes = 0; 
		 card_index_super_classes < card_super_classes_count;
		 card_index_super_classes++) {

		/* set the class to where to search next */
		*pp_class = *pp_super_class;
		p_the_method = _http_inherited_method_from_name(pp_class, pc_the_method_name);

		if (p_the_method != NIL) 
			return p_the_method;

		pp_super_class++; /* advance to next super class */
	}


	/* didn't find one */
	return NIL;
}


/* ********************************************************* */
/* returns the method from the root or class that has the given name */

 static ilu_Method _http_method_from_name (ilu_Class* pp_class, ilu_string pc_the_method_name) {

	ilu_Method p_the_method;

	/* search internal (ilu_rootClass) methods */
	p_the_method = _http_root_method_from_name(pc_the_method_name);
	if (p_the_method != NIL) {
		*pp_class = ilu_rootClass;
		return p_the_method;
	}

	/* search inherited methods */
	return _http_inherited_method_from_name(pp_class, pc_the_method_name);
 }


/* ********************************************************* */
/* fills in a Host header appropriate for use with http 1.1 */

static void _http_create_host_header(ilu_Call p_call, ilu_string pc_host_header_buffer) {

	
	ilu_string pc_hostpinfo = (call_connection(p_call))->co_pinfo;
	ilu_TransportInfo ppc_hosttinfo = conn_tinfo((call_connection(p_call)));
	ilu_TransportInfo ppc_hosttinfowalker;
	ilu_string pc_past_third_underscore = pc_hostpinfo;
	ilu_string pc_last_underscore;
	int i_underscore_count = 0;

	pc_host_header_buffer[0] = '\0';

	/* find the character after the third underscore, that's the end host followed
	by _ end port */
	while (i_underscore_count < 3) {
		if ((*pc_past_third_underscore) == '_')
			i_underscore_count++;
		else if (*pc_past_third_underscore == '\0')
			break;
		pc_past_third_underscore++;
	}
	
	if (i_underscore_count < 3) {
		/* there was no host information stashed in the pinfo, make one from the tinfo 
		search backward through the tinfo to find the tcp entry */

		/* get the last non null one */
		ppc_hosttinfowalker = ppc_hosttinfo;
		while (*ppc_hosttinfowalker) ppc_hosttinfowalker++;
		ppc_hosttinfowalker--;

		/* now scan backward */
		while (ppc_hosttinfowalker >= ppc_hosttinfo) {
			if (strstr(*ppc_hosttinfo, "tcp_") == *ppc_hosttinfo) {
				pc_past_third_underscore = (*ppc_hosttinfo) + 4;
				break;
			}
		ppc_hosttinfowalker--;
		}				

		if (ppc_hosttinfowalker < ppc_hosttinfo) {
			/* xxx couldn't find tcp transport??? Send a non-valued host header and hopw the 
			other end really doesn't care */
			sprintf(pc_host_header_buffer, "Host: ");
			return;			
		}		
	}

	/* write the initial header */
	sprintf(pc_host_header_buffer, "Host: %s", pc_past_third_underscore);

	/* overwrite last underscore with a colon */
	pc_last_underscore = pc_host_header_buffer + strlen(pc_host_header_buffer);
	while (((*pc_last_underscore) != '_') && (pc_last_underscore >= pc_host_header_buffer))
		pc_last_underscore--;
	if (pc_last_underscore >= pc_host_header_buffer) 
		*pc_last_underscore = ':';

	return;
 }


/* ********************************************************* */
/* returns a string that's used to form the protocol info part of a 
   string binding handle */

 static ilu_string _http_form_protocol_handle (ilu_refany protocol_instance_data, ilu_Object obj) {
	 
	 char buf[32];
	 ilu_Error an_error;
	 ilu_string pc_protohandle;
	 http_members_s* p_data_block = (http_members_s*) protocol_instance_data;
	 
	 if (!p_data_block)
		 /* if we have no info (as when blindly parsing a URL), use 1.1 */
		 sprintf (buf, ilu_http_default_scheme);
	 else if (p_data_block->m_b_persistent)
		 /* if persistent, add 'p' suffix */
		 sprintf (buf, "http_%hu_%hup", p_data_block->m_scard_major_version, p_data_block->m_scard_major_version);
	 else
		 sprintf (buf, "http_%hu_%hu", p_data_block->m_scard_major_version, p_data_block->m_scard_major_version);
	 
	 pc_protohandle = ilu_StrdupE(buf, &an_error);
	 
	 if (ILU_ERRNOK(an_error)) {
		 ILU_NOTE(MALLOC_DEBUG, ("_http_form_protocol_handle couldn't ilu_StrdupE\n"));
		 ILU_HANDLED(an_error);
		 pc_protohandle = ILU_NIL;
	 }
	 
	 return (pc_protohandle); 
 }
     
/* ********************************************************* */
/* create and free a http data block - contains any (member-var like)
   attributes specific to the protocol - currently specifies 
   batching and concurrency but batching probably really doesn't 
   apply for http                                            */

static ilu_refany _http_create_non_batching_non_concurrent_data_block(ilu_ProtocolInfo pinfo,
								      ILU_ERRS((no_memory)) * p_error) {

	http_members_s* p_new_member_struct = 
		( http_members_s *) ilu_MallocE(sizeof(http_members_s), p_error);
	if (ILU_ERRNOK(*p_error))
	return NIL;

	/* set up defaults */
	p_new_member_struct->m_b_batching = ilu_FALSE;
	p_new_member_struct->m_b_concurrent = ilu_FALSE;
	p_new_member_struct->m_b_persistent = ilu_FALSE; /* default to non persistent */
	p_new_member_struct->m_scard_major_version = 1;	/* default to 1.0 */
	p_new_member_struct->m_scard_minor_version = 0;
		
	/* make note of whether we're doing persistent connections */
	if (strstr( pinfo, "http_1_0p") == pinfo) {
		p_new_member_struct->m_b_persistent = ilu_TRUE;
	}

	if  (strstr(pinfo, "http_1_1") == pinfo) {
		/* make note of what version this is */
		p_new_member_struct->m_scard_major_version = 1;
		p_new_member_struct->m_scard_minor_version = 1;
	}
	
	/* set up to use the built in serial number queue */
	p_new_member_struct->m_serial_num_queue = p_new_member_struct->m_built_in_serial_num_queue;
	p_new_member_struct->m_serial_num_queue_size = BUILT_IN_SERIAL_NUM_QUEUE_SIZE;
	p_new_member_struct->m_serial_num_queue_head = 0;
	p_new_member_struct->m_serial_num_queue_tail = 0;

	/* init counter used for serial number generation */
	p_new_member_struct->m_card_serial_number_counter = 0;

	return ((ilu_refany) p_new_member_struct);
}


static void _http_free_data_block (ilu_refany p_block) {
	http_members_s* p_member_struct = (http_members_s *) p_block;

	/* free the serial number queue if the built in one wasn't the one bein used */
	if (p_member_struct->m_serial_num_queue != p_member_struct->m_built_in_serial_num_queue)
	  ilu_free(p_member_struct->m_serial_num_queue);

	ilu_free(p_member_struct);
}


/* ********************************************************* */
/* returns true if the method name is one of the http1.0 method names 
- i.e. GET, HEAD or POST  */

static ilu_boolean _http_is_http_method_name(ilu_string pc_method_name) {

	return ((strcmp("GET", pc_method_name) == 0)  ||
			(strcmp("HEAD", pc_method_name) == 0) ||
			(strcmp("POST", pc_method_name) == 0));
}


/* ********************************************************* */
/* returns ilu_TRUE if the call is on an object of HTTP_RESOURCE_OBJECT_TYPE_ID
or one of its subtypes, and the method is a normal http1.0 method */

static ilu_boolean _http_is_http_resource_object_and_method (ilu_Call p_call) {

	/* get the classes of the object involved and the class with the 
	   id HTTP_RESOURCE_OBJECT_TYPE_ID */
	static ilu_Class p_http_resource_object_class = NIL;
	ilu_Class p_called_object_class = call_intro_type(p_call);

	if (!p_http_resource_object_class)  
	  p_http_resource_object_class = 
		ilu_FindClassFromID(HTTP_RESOURCE_OBJECT_TYPE_ID);

	if (!p_http_resource_object_class || !p_called_object_class)
		return ilu_FALSE;

	/* ilu_IsSubObjectType Returns ilu_TRUE iff p_called_object_class is a subtype 
	of p_http_resource_object_class (including the degenerate case of when 
	they're the same). */

	return (ilu_IsSubObjectType(p_called_object_class, p_http_resource_object_class) &&
			_http_is_http_method_name(method_name(call_method(p_call))));
}



/* ********************************************************* */
/* Call initialization and finialization                     */
/* ********************************************************* */


/* flush output if needed, or tell boundaried transport to end the message */

static void _http_end_message(ilu_Call p_call,
			      ilu_boolean b_is_input,
			      ilu_boolean push,
			      ilu_Error* p_error) {

	ilu_Transport the_transport;

	ILU_CLER(*p_error);

	the_transport = http_call_tport(p_call);

	if (the_transport->tr_class->tc_boundaried)
		transport_end_message(the_transport, push, NIL, p_error);

	else if (push && ! b_is_input)
		the_transport->tr_class->tc_push (the_transport, p_error);
}



/* Perform any call info struct specific initialization */

static void _http_init_info_struct(ilu_Call p_call, ilu_boolean b_reinitting) {

	/* get pointer to the data block of the protocol in this connection */
	http_members_s* p_data_block = (http_members_s*)(connection_protocol_data(call_connection(p_call)));

	/* init to no buffers, etc. */
	_http_set_call_state(p_call, http_init_state);
	_http_set_call_type(p_call, unknowncalltype);
	_http_num_headers_to_process(p_call) = 0;
	_http_method_kind(p_call) = ILU_HTTP_UNKNOWN_OR_OTHER_METHOD;
	_http_user_supplied_content_length(p_call) = ilu_FALSE;
	_http_user_supplied_host(p_call) = ilu_FALSE;
	_http_headers_list(p_call) = NIL;
	_http_headers_current_node(p_call) = NIL;
	_http_status_line(p_call) = NIL;
	_http_method_name(p_call) = NIL;	
	_http_object_id(p_call) = NIL;
	_http_params_queries(p_call) = NIL;
	_http_params_queries_delim(p_call) = '\0';
	_http_server_id(p_call) = NIL;		
	_http_version_string(p_call) = NIL;		
	_http_status_code_string(p_call) = NIL;	
	_http_version_reason_phrase(p_call) = NIL;	
	_http_major_version(p_call) = _http_protocol_major_version(p_call);
	_http_minor_version(p_call) = _http_protocol_minor_version(p_call);
	_http_status_code(p_call) = 0;
	_http_body_length(p_call) = 0;
	_http_keep_alive(p_call) = ((_http_protocol_major_version(p_call) == 1) && 
								(_http_protocol_major_version(p_call) == 1)) ? ilu_TRUE : ilu_FALSE;
	_http_persistent(p_call) = p_data_block->m_b_persistent;
	_http_delayed_interp_transport(p_call) = NIL;
	_http_set_to_buffered_transport(p_call) = ilu_FALSE;

	/* put a serial number on the call */
	if (! b_reinitting)
	 _http_generate_serial_number(p_call);

	return;
}



/* Perform any call specific initialization needed when reinitting call for
reuse after a delayed interpretation situation*/

static ilu_boolean _http_reused_call_init(ilu_Call p_call, ILU_ERRS((no_memory))* p_error) {
	
	http_call_info_s* p_http_call_info_s;

	/* make a new call info struct */
	p_http_call_info_s = 
		( http_call_info_s *) ilu_MallocE(sizeof(http_call_info_s), p_error);
	
	if (ILU_ERRNOK(*p_error))
		return ilu_FALSE;
	
	/* hang the new call specific information structure off the call */
	p_call->ca_prdata2 = p_http_call_info_s;

	/* initialize our information structure */
	_http_init_info_struct(p_call, ilu_TRUE);
	
	_http_redirected_instance_handle(p_call) = NIL;
	_http_redirected_server_id(p_call) = NIL;
	_http_redirected_sbh(p_call) = NIL;
	
	/* set try index back to 0; */
	p_call->ca_tryIndex = 0;
	
	return ilu_TRUE;
}



/* used after initialization to make a determination as to what
sort of call this is */

static void _http_adjust_state_from_raw_init(ilu_Call p_call, ilu_boolean b_request_starting) {
	
	/* adjust state from raw init */
	if (_http_get_call_state(p_call) == http_init_state) {
		if (!call_incoming(p_call)) { /* outgoing call */
			
			if (_http_is_http_resource_object_and_method (p_call)) {
				/* this is a http1.x call on a HTTP_RESOURCE_OBJECT */
				/* set to next state accordingly */
				_http_set_call_type(p_call, clnt2http);
				if (b_request_starting)
					_http_set_call_state(p_call, clnt2http_out_method_name);
				else /* being used to read in a reply */
					_http_set_call_state(p_call, clnt2http_in_read_reply_header);
			}
			else {
			/* this is not a http1.x call on a HTTP_RESOURCE_OBJECT we use 
				general purpose marshalling */
				_http_set_call_type(p_call, clnt2ilu);
				if (b_request_starting)
					_http_set_call_state(p_call, clnt2ilu_out_method_name);
				else /* being used to read in a reply */
					_http_set_call_state(p_call, clnt2ilu_in_read_reply_header);
			}
		}
		else { 
			/* don't yet have a way of knowing what type the object is */
			/* so just set to next state */
			_http_set_call_type(p_call, srvr4something);
			_http_set_call_state(p_call, srvr_in_read_header);
		}
	}
}


/* Perform any protocol specific initialization              */

static ilu_boolean _http_init_call(ilu_Call p_call, ILU_ERRS((IoErrs)) * p_error) {

	http_call_info_s* p_http_call_info_s;

	ILU_CLER(*p_error);

	/* if p_call->ca_tryIndex is not 0, then we must be re-trying a redirected call,
	so we'll already have a http_call_info_s, that's holding the object id and server id
	to be used in the retry */
	if (p_call->ca_tryIndex == 0) {
		/* create a new call specific information structure and hang it off the call */
		p_http_call_info_s = 
			( http_call_info_s *) ilu_MallocE(sizeof(http_call_info_s), p_error);
		if (ILU_ERRNOK(*p_error))
			return ilu_FALSE;
		/* hang the new call specific information structure off the call */
		p_call->ca_prdata2 = p_http_call_info_s;
		
		/* only init these in the 'fresh' case, otherwise we'd stomp on the
		object id and server id	to be used in the retry */
		_http_redirected_instance_handle(p_call) = NIL;
		_http_redirected_server_id(p_call) = NIL;
		_http_redirected_sbh(p_call) = NIL;
	}
	
	/* give up if we've reached our maximum retry count */
	if (p_call->ca_tryIndex > ILU_HTTP_MAXIMUM_RETRIES) {	
		ILU_ERR_CONS1(inv_objref, p_error, minor, ilu_iom_relocate_loop, ilu_FALSE);
		return ilu_FALSE;
	}

	/* set the transport used for this call */
	p_call->ca_prTrans = connection_transport(p_call->ca_connection);

	/* initialize our information structure */
	_http_init_info_struct(p_call, ilu_FALSE);

	return ilu_TRUE;
}


/* ********************************************************* */
/* frees up a calls's http_call_info_s structure members     */

static void _http_freeup_call_infostruct (ilu_Call p_call, ilu_boolean b_part_of_redirection) {

	/* delete any buffers used for incoming bytes, etc. */
	ilu_free(_http_status_line(p_call));

	/* free any list we may have used */
	if (_http_headers_list(p_call) != NIL)
		_http_free_list_and_contents(_http_headers_list(p_call));

	/* ensure any unused redirection members are freed if we're not doing this 
		as a part of redirection (during redirects, this information needs to be 
		retained across the attempts) */
	if (!b_part_of_redirection) {
		ilu_free(_http_redirected_instance_handle(p_call));
		ilu_free(_http_redirected_server_id(p_call));
		ilu_free(_http_redirected_sbh(p_call));
		_http_redirected_instance_handle(p_call) = NIL;
		_http_redirected_server_id(p_call) = NIL;
		_http_redirected_sbh(p_call) = NIL;
	}
	
	
	/* if we have a immem transport used for delayed reply interpretation, destroy it*/
	if (_http_delayed_interp_transport(p_call)) {
		ilu_Error an_error;
		ILU_CLER(an_error); 
		_ilu_BufferTransport_Destroy(_http_delayed_interp_transport(p_call), NIL, NIL, &an_error);
		if (ILU_ERRNOK(an_error)) {
			ILU_NOTE(HTTP_DEBUG, ("_http_freeup_call got error destroying m_delayed_interp_transport\n"));
			/* nothing we can really do about an error at this point */
			ILU_HANDLED(an_error);
		}
	}
	
	if (_http_set_to_buffered_transport(p_call)) {
	/* must've gotten an error somewhere along the way for this
		to still be set (implying our call's transport is set to the in memory buffer transport) */
		ilu_Error an_error;
		ILU_CLER(an_error); 
		
		_ilu_BufferTransport_Destroy(http_call_tport(p_call), NIL, NIL, &an_error);
		if (ILU_ERRNOK(an_error)){ 	/* nothing we can really do about an error at this point */
			ILU_NOTE(HTTP_DEBUG, ("_http_freeup_call got error destroying in memory buffer transport\n"));
			ILU_HANDLED(an_error);
		}
		p_call->ca_prTrans = connection_transport(p_call->ca_connection);
	}

	return;
}


/* ********************************************************* */
/* frees up a calls's data structures                       */

static void _http_freeup_call (ilu_Call p_call) {
	
	
	/* frees up calls's http_call_info_s structure members */
	_http_freeup_call_infostruct (p_call, ilu_FALSE);
		
	/* delete our call private structure */
	ilu_free(p_call->ca_prdata2);
}


/* ********************************************************* */
/* pre finishes a call                                       */

/* take a serial number off the serial number queue for this connection and return it */

static ilu_boolean _http_dequeue_serial_number(ilu_Call p_call, ilu_cardinal* p_card_serial_num,
											   ILU_ERRS((internal)) * p_error) {
	
	http_members_s *p_data_block = ((http_members_s *) (connection_protocol_data(call_connection(p_call))));
	
	/* ensure we have something to dequeue */
	if (!ilu_Check(p_data_block->m_serial_num_queue_head != p_data_block->m_serial_num_queue_tail, p_error))
		return ilu_FALSE;
	
	/* get the one at the tail of the queue */
	*p_card_serial_num = p_data_block->m_serial_num_queue[p_data_block->m_serial_num_queue_tail];
	
	/* and reset the tail index properly */
	p_data_block->m_serial_num_queue_tail = ((p_data_block->m_serial_num_queue_tail + 1)
			     % p_data_block->m_serial_num_queue_size);
	
	return ILU_CLER(*p_error);
}


/* put a serial number into the serial number queue for this connection and return it */

static ilu_boolean _http_queue_serial_number(ilu_Call p_call, ilu_cardinal card_serial_num,
											 ILU_ERRS((internal)) * p_error) {
	
	ilu_cardinal card_nextHead;
	http_members_s *p_data_block;
	p_data_block = ((http_members_s *)(connection_protocol_data(call_connection(p_call))));


	/* calculate what that new queue head index will be */
	card_nextHead = ((p_data_block->m_serial_num_queue_head + 1)
		% p_data_block->m_serial_num_queue_size);
	
	/* grow the queue if we ran out of room */
	if (card_nextHead == p_data_block->m_serial_num_queue_tail) {
		ilu_cardinal    i, j, *newQ;
		
		/* allocate double the current size */
		ilu_cardinal    newSize = p_data_block->m_serial_num_queue_size * 2;
		newQ = ilu_MallocE(newSize * sizeof(ilu_cardinal), p_error);
		if (ILU_ERRNOK(*p_error))
			return ilu_FALSE;
		
		/* copy the current queue to the new queue */
		for (i = 0, j = p_data_block->m_serial_num_queue_tail;
	       j != p_data_block->m_serial_num_queue_head;
		   i++, j = (j + 1) % p_data_block->m_serial_num_queue_size)
			   newQ[i] = p_data_block->m_serial_num_queue[j];
		   
		   /* if we were not using the build in queue, free up the old one */
		   if (p_data_block->m_serial_num_queue != p_data_block->m_built_in_serial_num_queue)
			   ilu_free(p_data_block->m_serial_num_queue);
		   
		   /* set up to use the new queue */
		   p_data_block->m_serial_num_queue = newQ;
		   p_data_block->m_serial_num_queue_size = newSize;
		   p_data_block->m_serial_num_queue_tail = 0;
		   p_data_block->m_serial_num_queue_head = i;
		   card_nextHead = i + 1;
	}
	
	/* stash the serial number in the queue */
	p_data_block->m_serial_num_queue[p_data_block->m_serial_num_queue_head] = card_serial_num;
	p_data_block->m_serial_num_queue_head = card_nextHead;
	
	return ilu_TRUE;
}


static  ilu_boolean _http_prefinish_call (ilu_Call p_call,
					  ILU_ERRS((IoErrs)) * p_error)
{
  _http_call_state state;
  ilu_Method themeth;

  ILU_CLER(*p_error);

  state = _http_get_call_state(p_call);
  switch (state) {

  case clnt2http_finish_call:
  case srvr4http_finish_call:

    /* If we're not in a connection keep-alive situation
       close the call's connection so we don't try to reuse it (since
       the httpd on the other end will have closed as per current practice,
       resulting in an	unusable connection). (or we are acting as the httpd
       and need to close it per current practice) */

    if (! _http_keep_alive(p_call) ) {
      ILU_NOTE(HTTP_DEBUG, ("_http_prefinish_call closing connection\n"));
      _ilu_CloseIoingConnection(call_connection(p_call), ilu_FALSE, ilu_ConnShutdownReason_ResourceManagement);
    }
    break;


  case clnt2http_redirect_call:
		
    /* we've been redirected */
    if (! _http_keep_alive(p_call) ) {
      ILU_NOTE(HTTP_DEBUG, ("_http_prefinish_call closing connection - redirect\n"));
      _ilu_CloseIoingConnection(call_connection(p_call), ilu_FALSE, ilu_ConnShutdownReason_ResourceManagement);
    }
    /* fall through to next case */

  case clnt2ilu_redirect_call:
		
    /* clean up space used in our call's http_call_info_s since the next 
       protocol entry should be to _http_init_call, which will reinit (the same) info structure 
       We do this because _http_setup_redirection hangs the server and object id
       to use during the retry in the info structure */
    _http_freeup_call_infostruct(p_call, ilu_TRUE);
		
    break;

  case clnt2ilu_finish_call:

	/* If we made a http 1.0 call, we normally keep the connection open if 
	  the call wasn't on a direct instances of iluhttp.Resource since we know 
	  it must be ILU on the other end.  In some cases, (e.g. http-ng testing)
	  we may actually want force true 1.0 connection closure behavior for calls on
	  types other than just iluhttp.Resource. So, if we're strict http 1.0 and 
	  ilu_http_force_close_on_ilu_1_0 is set, we close the connection. */

    if (ilu_http_force_close_on_ilu_1_0 && 
	_http_protocol_minor_version(p_call) == 0 && 
	(!_http_protocol_persistence(p_call)) && 
	_http_protocol_major_version(p_call) == 1) {
      /* we made the call with strictly http_1_0 (not http_1_0p) */
      ILU_NOTE(HTTP_DEBUG, ("_http_prefinish_call closing connection - ilu_http_force_close_on_ilu_1_0\n"));
      _ilu_CloseIoingConnection(call_connection(p_call), ilu_FALSE, ilu_ConnShutdownReason_ResourceManagement);
    }

    break;

  case srvr4ilu_finish_call:

    /* If we received a http 1.0 call, we normally keep the connection open if 
	  the call wasn't on a direct instances of iluhttp.Resource since we know 
	  it must be ILU on the other end.  In some cases, (e.g. http-ng testing)
	  we may actually want force true 1.0 connection closure behavior for calls on
	  types other than just iluhttp.Resource. So, if we're strict http 1.0 and 
	  ilu_http_force_close_on_ilu_1_0 is set, we close the connection. */

    if (ilu_http_force_close_on_ilu_1_0 && 
	_http_minor_version(p_call) == 0 && 
	(!_http_persistent(p_call)) && 
	_http_major_version(p_call) == 1) {
      /* we received the call with strictly http_1_0 (not http_1_0p) */
      ILU_NOTE(HTTP_DEBUG, ("_http_prefinish_call closing connection - ilu_http_force_close_on_ilu_1_0\n"));
      _ilu_CloseIoingConnection(call_connection(p_call), ilu_FALSE, ilu_ConnShutdownReason_ResourceManagement);
    }
	  
    break;

  case srvr_in_read_header:
	  /* can happen when a monitored outgoing connection closes */
	  break;

  case clnt2ilu_in_read_reply_header:
    themeth = call_method(p_call);
    if (!(themeth && method_asynchronous(themeth))) {
		/* note we can also end up here when ilu_OutgoingConnectionThreadProc calls
		ProcessExtraInput */
      ILU_NOTE(HTTP_DEBUG, ("_http_prefinish_call in state clnt2ilu_in_read_reply_header, setting comm_failure\n"));
      /* close the call's connection so we don't try to reuse it (since
	 the httpd on the other end will have closed as per current practice,
	 resulting in an	unusable connection). (or we are acting as the httpd
	 and need to close it per current practice) */
      ILU_ERR_CONS1(comm_failure, p_error, minor, ilu_cfm_protocol_sync_lost, 0);
    }
    break;

  case http_init_state:
    /* we can get here from ilu_OutgoingConnectionThreadProc when a connection closes */
    break; 

  case srvr4ilu_out_begin_reply:
    themeth = call_method(p_call);
    if (themeth && method_asynchronous(themeth))
      break;
    /* fall through to error case for non-asynch methods */

  default:
    /* somthing really bad must have happened to get us here prematurely, so
       blow everything away to be on the safe side */
    ILU_NOTE(HTTP_DEBUG, ("_http_prefinish_call closing connection - error\n"));
    /* close the call's connection so we don't try to reuse it (since
       the httpd on the other end will have closed as per current practice,
       resulting in an	unusable connection). (or we are acting as the httpd
       and need to close it per current practice) */
    ILU_ERR_CONS1(comm_failure, p_error, minor, ilu_cfm_protocol_sync_lost, 0);
    break;

  }

  return ILU_ERROK(*p_error);
}


/* ********************************************************* */
/* finishes a call                                           */

static  ilu_boolean _http_finish_call (ilu_Call p_call,
		      ILU_ERRS((IoErrs)) * p_error) {
  _http_call_state state;

  ILU_CLER(*p_error);

  switch ((state = _http_get_call_state(p_call))) {

  case clnt2http_finish_call:
  case srvr4http_finish_call:
  case clnt2ilu_finish_call:
  case srvr4ilu_finish_call:
  case srvr_in_read_header: /* can happen when a monitored outgoing connection closes */
  default:

    /* free up the data structures, etc. used for this call */
    _http_freeup_call (p_call);

    break;
  }

  return ilu_TRUE;
}



/* ********************************************************* */
/* reading headers                                           */
/* ********************************************************* */


/* ********************************************************* */
/* reads an incoming header                                  */

static ilu_ReadHeaderResultCode
  _http_read_header  (ilu_Call p_call, ilu_PacketType* p_packetType,
		      ilu_cardinal* p_card_packetSN,
		      ilu_ConnShutdownReason *reason,
		      ilu_cardinal *lastSN,
		      ILU_ERRS((IoErrs)) * p_error) {
	
	ilu_Transport the_transport;
	ilu_byte c_firstbyte;
	ilu_cardinal card_firstbytes_read = 0;

	ILU_CLER(*p_error);

	/* adjust state from raw init */
	_http_adjust_state_from_raw_init(p_call, ilu_FALSE);

	the_transport = http_call_tport(p_call);

	/* if we're on a boundaried transport */
	if (the_transport->tr_class->tc_boundaried) { 
		
		ilu_ReadHeaderResultCode read_header_result;
		
		read_header_result = transport_begin_message(the_transport, ilu_TRUE, p_error);
		
		switch (read_header_result) {
			
		case ilu_rhrc_ok: break;
			
		case ilu_rhrc_error:
			ILU_NOTE((INCOMING_DEBUG), ("%s:  error %s on transport_begin_message (input)\n",
				"_http_read_header", ILU_ERR_NAME(*p_error)));
			ILU_HANDLED(*p_error);
			return ILU_ERR_CONS1(comm_failure, p_error, minor, ilu_cfm_protocol_sync_lost, ilu_rhrc_error);
			
		case ilu_rhrc_eof:
		  *reason = ilu_ConnShutdownReason_ReceivedEOF;
		  *lastSN = 0;
		case ilu_rhrc_nothing:
		case ilu_rhrc_handled:
			return (read_header_result);
			
		default: _ilu_Assert(ilu_FALSE, "http.c:ReadHeader -- bad ilu_ReadHeaderResultCode value");
		}
	}
	else {
		/* first ensure there's really something there to read */
		ilu_TransportReport transport_report;
		card_firstbytes_read = _http_transport_read_upto_one_byte(the_transport, &c_firstbyte, 
			&transport_report, p_error);
		if (ILU_ERRNOK(*p_error))
			return ilu_rhrc_error;
		if (card_firstbytes_read == 0) {
			/* there wasn't something there, check the report */
			if (transport_report.tr_eof) {
			  *reason = ilu_ConnShutdownReason_ReceivedEOF;
			  *lastSN = 0;
			  return ilu_rhrc_eof;
			} else
			  return ilu_rhrc_nothing;
		}
	}

	switch (_http_get_call_state(p_call)) { 

	case clnt2http_in_read_reply_header:

		/* read in status line and any headers */
		if (_http_get_status_line(p_call, (card_firstbytes_read ? &c_firstbyte : NIL), 
				p_error) == ilu_FALSE)
			goto reading_error_label;

		_http_fill_headers_list(p_call, p_error);
		if (ILU_ERRNOK(*p_error))
			goto reading_error_label;

		if (!_http_dequeue_serial_number(p_call, p_card_packetSN, p_error))
		  return ilu_rhrc_error;

		/* can only get replies from an httpd */
		*p_packetType = ilu_PacketType_Reply;

		/* set to next state */
		_http_set_call_state(p_call, clnt2http_in_interpret_reply);
			
		break;

	case clnt2ilu_in_read_reply_header:

		/* read in status line and any headers */
		if (_http_get_status_line(p_call, (card_firstbytes_read ? &c_firstbyte : NIL),
				p_error) == ilu_FALSE)
			goto reading_error_label;

		_http_fill_headers_list(p_call, p_error);
		if (ILU_ERRNOK(*p_error))
			goto reading_error_label;

		if (!_http_dequeue_serial_number(p_call, p_card_packetSN, p_error))
		  return ilu_rhrc_error;

		/* can only get replies from an httpd */
		*p_packetType = ilu_PacketType_Reply;

		/* set to next state */
		_http_set_call_state(p_call, clnt2ilu_in_interpret_reply);

		break;

	case srvr_in_read_header:

	  /* read in request line and any headers */
	  if (_http_get_request_line(p_call, (card_firstbytes_read ? &c_firstbyte : NIL), p_error) == ilu_FALSE) {

	    /* see if we might have failed due to  an empty request line - if so, 
	       just ignore probably trailing garbage */

	    if (_http_request_line(p_call) && _http_request_line(p_call)[0] == '\0') {
	      ILU_ERR_SWITCH(*p_error) {
		ILU_ERR_CASE(marshal, v) {
		  ILU_HANDLED(*p_error);
		  ILU_CLER(*p_error);
		  ILU_NOTE(HTTP_DEBUG, ("_http_read_header Ignoring empty request line\n"));
		  return ilu_rhrc_handled;
		}
		ILU_ERR_ELSE
		  } ILU_ERR_ENDSWITCH;
	    }
	    goto reading_error_label;
	  }
		  
	  _http_fill_headers_list(p_call, p_error);
		if (ILU_ERRNOK(*p_error))
			goto reading_error_label;

		/* we have no explicit packet types or serial numbers - so just set to request because of 
		how control flow got us here, fill in serial number for the heck of it */
		*p_packetType = ilu_PacketType_Request;
		*p_card_packetSN = 0;

		/* set to next state (if there's a server id, then it must be a general ilu call) */
		/* Any call that comes from ilu and isn't a GET HEAD or POST call on an object that 
		is of HTTP_RESOURCE_OBJECT_TYPE will have a server id header*/
		if (_http_server_id(p_call) == NIL) {
			_http_set_call_type(p_call, srvr4http);
			_http_set_call_state(p_call, srvr4http_in_interpret_request);
		}
		else {
			_http_set_call_type(p_call, srvr4ilu);
			_http_set_call_state(p_call, srvr4ilu_in_interpret_request);
		}

		break;

	default:

		/* we may have got here due to activity (e.g. a close) on a outgoing 
		connection e.g. via ReadExtraMsg */
		switch (_http_get_call_type(p_call)) {

		case clnt2http:
		case clnt2ilu: /* activity must be because connection closed or if some wacko decides to 
						send stuff on this outgoing (from us) connection that isn't a reply, return 
						that we're at end of file */
		  *reason = ilu_ConnShutdownReason_LostProtocolSync;
		  *lastSN = 0;
		  return ilu_rhrc_eof;

		default:
			HTTP_UNEXPECTED_STATE_ASSERT();
		}
	}

	return ilu_rhrc_ok;

	reading_error_label:

	/* if we had an error and it's due to eof, clear the error and return ilu_rhrc_eof 
	This is because _http_read_header can be called when some activity occurs on the 
	connection - one of these kinds of activity is closing, which is really not an error */
	ILU_ERR_SWITCH(*p_error) {
		ILU_SUCCESS_CASE return ilu_rhrc_ok;
		ILU_ERR_CASE(comm_failure, p_the_err_struct) {
			if (p_the_err_struct->minor == ilu_cfm_eof) {
					ILU_CLER(*p_error);
					*reason = ilu_ConnShutdownReason_LostProtocolSync;
					*lastSN = 0;
					return ilu_rhrc_eof;
			}
			else return ilu_rhrc_error;
		}
		ILU_ERR_ELSE
			 /* had a non eof problem reading, we're just hosed */
			return ilu_rhrc_error;
	} ILU_ERR_ENDSWITCH;

}


/* ********************************************************* */
/* request initialization and finialization, and interpretation */
/* ********************************************************* */


/* ********************************************************* */
/* start actually sending a request (puts out method name SP)*/

static ilu_boolean _http_start_request (ilu_Call p_call, 
									   ilu_cardinal card_arg_size, 
									   ILU_ERRS((IoErrs)) * p_error) {
	ilu_Transport the_transport;

	ILU_CLER(*p_error);

	/* adjust state from raw init */
	_http_adjust_state_from_raw_init(p_call, ilu_TRUE);

	the_transport = http_call_tport(p_call);

	/* if we're on a boundaried transport */
	if ((the_transport->tr_class->tc_boundaried) && 
	    (transport_begin_message(the_transport, ilu_FALSE, p_error) != ilu_rhrc_ok))
		return ilu_FALSE;

	switch (_http_get_call_state(p_call)) {

	case clnt2http_out_method_name:

		/* if it's a HEAD method, then make note of it so that when we get
		back the response headers, we'll know that the Content-Length field
		is to be ignored when it comes to reading in any entity body.  We
		also want to know if it's a get to know whether or not we should
		automatically redirect in the event of receiving a 302 Moved Temporarily status */
		if (strcmp("GET", method_name(call_method(p_call))) == 0) 
			_http_method_kind(p_call) = ILU_HTTP_GET_METHOD;	
		else if (strcmp("HEAD", method_name(call_method(p_call))) == 0) 
			_http_method_kind(p_call) = ILU_HTTP_HEAD_METHOD;	
		
		/* set to next state */
		_http_set_call_state(p_call, clnt2http_out_URL_path);
			
		break;

	case clnt2ilu_out_method_name:
		/* set to next state */
		_http_set_call_state(p_call, clnt2ilu_out_object_id);

		break;

	default:
		HTTP_UNEXPECTED_STATE_ASSERT();
	}

	/* have the transport write out the method name*/
	_http_transport_write_bytes(http_call_tport(p_call), method_name(call_method(p_call)), 
		strlen(method_name(call_method(p_call))), p_error);

	if (ILU_ERRNOK(*p_error))
		return ilu_FALSE;

	/* have the transport write out a space */
	_http_transport_write_bytes(http_call_tport(p_call), g_c_SP, 1, p_error);

	if (ILU_ERRNOK(*p_error))
		return ilu_FALSE;

	return ilu_TRUE;
}


/* ********************************************************* */
/* finishes sending a request                                */

static  ilu_boolean _http_finish_request(ilu_Call p_call,
		      ilu_Message * p_msg,
		      ilu_boolean push, ILU_ERRS((IoErrs)) * p_error) {
	ilu_Method themeth = call_method(p_call);

	ILU_CLER(*p_error);

	switch (_http_get_call_state(p_call)) {

	case clnt2http_out_finish_request:
		/* set to next state */
		_http_set_call_state(p_call, clnt2http_in_read_reply_header);
		break;
		
	case clnt2ilu_out_arguments:
		
		/* send out a content length header and the buffered body bytes */
		_http_send_buffered_body(p_call, p_error) ;

		if (ILU_ERRNOK(*p_error))
			return ilu_FALSE;
				
		/* set to next state */
		_http_set_call_state(p_call, clnt2ilu_in_read_reply_header);
		
		break;

	default:
		HTTP_UNEXPECTED_STATE_ASSERT();
	}
	
	/* Put call serial number into FIFO */
	if (themeth && !method_asynchronous(themeth)) {
	  if (!_http_queue_serial_number(p_call,
					 call_serial_number(p_call),
					 p_error))
	    return ilu_FALSE;
	}

	/* have the transport flush itself */
	_http_end_message(p_call, ilu_FALSE, push, p_error);

	if (ILU_ERRNOK(*p_error))
		return ilu_FALSE;

	return ilu_TRUE;
}



/* ********************************************************* */
/* 	interpret an incoming request                            */

static ilu_boolean _http_interpret_request (ilu_Call p_call, 
											ILU_ERRS((IoErrs)) * p_error) {

	ilu_Object p_the_object;
	ilu_Class p_the_class;
	ilu_Method p_the_method;

	ILU_CLER(*p_error);

	switch (_http_get_call_state(p_call)) {

	case srvr4http_in_interpret_request:
		/* set to next state */
		_http_set_call_state(p_call, srvr4http_in_server_id);
		break;

	case srvr4ilu_in_interpret_request:

		if ((strcmp(_http_server_id(p_call), server_id(call_server(p_call))) != 0)) 
		/* we're in the wrong server ! */
		return ILU_ERR_CONS1(marshal, p_error, minor, ilu_mm_alien_disc, ilu_FALSE);

		/* set to next state */
		_http_set_call_state(p_call, srvr4ilu_in_server_id);

		break;

	default:
		HTTP_UNEXPECTED_STATE_ASSERT();
	}

	/* find the object for this object id in the server -
	use ilu_rootClass to enter server since we have no idea at this
	point what the class is.  XXX eventually for ilu-ilu situations - have
	client send information about the object type */
	ilu_EnterServer(call_server(p_call), ilu_rootClass);
	if ((p_the_object = _ilu_FindObjectInServer(_http_object_id(p_call), 
			call_server(p_call))) == NIL) {
		ilu_ExitServer(call_server(p_call), ilu_rootClass);
		return ILU_ERR_CONS1(bad_param, p_error, minor, ilu_bpm_OID, ilu_FALSE);
	}
	ilu_ExitServer(call_server(p_call), ilu_rootClass);

	/* get and set the object's (most specific) class */
	p_the_class = object_class(p_the_object);
	call_intro_type(p_call) = p_the_class;

	/* get and set the method and class (potentially a superclass) with the given name */
	if ((p_the_method = _http_method_from_name(&(call_intro_type(p_call)), _http_method_name(p_call)))
			== NIL)
		return ILU_ERR_CONS1(bad_param, p_error, minor, ilu_bpm_OID, ilu_FALSE);
	call_method(p_call) = p_the_method;

	return ilu_TRUE; 
}


/* ********************************************************* */
/* Do anything that needs to be done after 
   arguments have been unmarshalled */

static void _http_request_read (ilu_Call p_call,
				    ILU_ERRS((IoErrs)) * p_error) {

	ilu_Error local_error;

	ILU_CLER(*p_error);

	switch (_http_get_call_state(p_call)) {

	case srvr4http_in_request_read:

		/* if we're version 1.1, ensure we got a host header */
		if (_http_minor_version(p_call) == 1 &&
			_http_major_version(p_call) == 1 &&
			!_http_user_supplied_host(p_call)) {
			
			/* set an error so we'll end up in finish call next */
			ILU_ERR_CONS1(bad_param, p_error, minor, ilu_bpm_broken, ilu_FALSE);

			/* set to next state */
			_http_set_call_state(p_call, srvr4http_missing_1_1_host_header);
		}
		
		
		/* set to next state */
		else 
			_http_set_call_state(p_call, srvr4http_out_begin_reply);
		break;

	case srvr4ilu_in_arguments: /* done reading in request arguments */

		/* set to next state */
		_http_set_call_state(p_call, srvr4ilu_out_begin_reply);

		break;

	default:
		HTTP_UNEXPECTED_STATE_ASSERT();
	}

	_http_end_message(p_call, ilu_TRUE, ilu_TRUE, &local_error);
	if (ILU_ERRNOK(local_error)) {
	  if (ILU_ERRNOK(*p_error)) /* preserve any previously set error */
	      ILU_HANDLED(local_error);
	      else /* otherwise return this error */
	      *p_error = local_error;
	}
}



/* ********************************************************* */
/* reply beginning, interpreting, finishing, read            */
/* ********************************************************* */

/* ********************************************************* */
/* Begin a reply                                             */

static ilu_boolean _http_begin_reply (ilu_Call p_call, 
									  ilu_boolean b_exceptions_possible,
									  ilu_cardinal reply_size, 
									  ILU_ERRS((IoErrs)) * p_error) {
	char c_buffer[64];
	ilu_Transport the_transport;

	ILU_CLER(*p_error);

	the_transport = http_call_tport(p_call);

	/* if we're on a boundaried transport */
	if ((the_transport->tr_class->tc_boundaried) && 
	    (transport_begin_message(the_transport, ilu_FALSE, p_error) != ilu_rhrc_ok))
		return ilu_FALSE;


	switch (_http_get_call_state(p_call)) {

	case srvr4http_out_begin_reply:

		/* set to next state */
		_http_set_call_state(p_call, srvr4http_out_Response_record);
		break;

	case srvr4ilu_out_begin_reply:

		/* have the transport write out the version (corresponding to the incoming request) and an OK status */
		sprintf(c_buffer, "HTTP/%d.%d 200 OK\r\n", _http_major_version(p_call), _http_minor_version(p_call));
		_http_transport_write_bytes(http_call_tport(p_call), c_buffer, strlen(c_buffer), p_error);
		if (ILU_ERRNOK(*p_error))
			return ilu_FALSE;
		
		/* Create an in memory transport buffer into which the rest of the call will
		be written so that we can determine the body length in order to add a 
		content length header */ 
		_http_switch_to_buffering_body(p_call, p_error);
		if (ILU_ERRNOK(*p_error))
			return ilu_FALSE;
		
		/* set to next state */
		_http_set_call_state(p_call, srvr4ilu_out_return_values);

		break;

	default:
		HTTP_UNEXPECTED_STATE_ASSERT();
	}

	return ilu_TRUE;
}


/* ********************************************************* */
/* Finish a reply                                            */

static ilu_boolean _http_finish_reply (ilu_Call p_call, 
				       ilu_boolean b_push,
				       ILU_ERRS((IoErrs)) * p_error)
{
  ILU_CLER(*p_error);
  
  switch (_http_get_call_state(p_call)) {

  case srvr4http_out_finish_reply:

    /* set to next state */
    _http_set_call_state(p_call, srvr4http_finish_call);
    break;

  case srvr4ilu_out_return_values:

    /* send out a content length header and the buffered body bytes */
    _http_send_buffered_body(p_call, p_error) ;

    if (ILU_ERRNOK(*p_error))
      return ilu_FALSE;

    /* set to next state */
    _http_set_call_state(p_call, srvr4ilu_finish_call);

    break;

  default:
    HTTP_UNEXPECTED_STATE_ASSERT();
  }


  /* have the transport flush itself */
  _http_end_message(p_call, ilu_FALSE, b_push, p_error);

  return ILU_ERROK(*p_error);
}


/* ********************************************************* */
/* take care of end of reply                                 */

static void _http_reply_read (ilu_Call p_call, ILU_ERRS((IoErrs)) * p_error) {

	ILU_CLER(*p_error);

	switch (_http_get_call_state(p_call)) {

	case clnt2http_in_reply_read:

		/* set to next state */
		_http_set_call_state(p_call, clnt2http_finish_call);
		break;

	case clnt2ilu_in_return_values:

		/* set to next state */
		_http_set_call_state(p_call, clnt2ilu_finish_call);

		break;

	default:
		HTTP_UNEXPECTED_STATE_ASSERT();
	}

	_http_end_message(p_call, ilu_TRUE, ilu_TRUE, p_error);
}


/* ********************************************************* */
/* setup to handle redirects                                 */

static ilu_ProtocolException _http_setup_redirection 
(ilu_Call p_call, ilu_cardinal* p_card_exception_code, ILU_ERRS((IoErrs)) * p_error)
{
	
  char* pc_new_location;
  char* pc_new_serverid;
  ilu_string pc_encodedContactInfo = NIL;
  ilu_cardinal card_encodedContactInfo_length;
  ilu_ProtocolInfo	pc_new_pinfo;
  ilu_TransportInfo	pc_new_tinfo;
  ilu_boolean		boolean_passEncodedCinfo = ilu_FALSE;
	
  ILU_CLER(*p_error);
	
  /* find the Location value */
  if (!_http_find_header_value(p_call, "Location", &pc_new_location)) {
    /* indicate problem - redirects must supply Location header - xxx appropriate error?*/
    *p_card_exception_code = 0;
    return ilu_ProtocolException_Unknown;	
  }

  /* find the server id value (would only happen if came from ilu http server */
  _http_find_header_value(p_call, ILU_SERVER_ID_HEADER_NAME, &pc_new_serverid);
	
  if (pc_new_serverid) {  
    /* we got a new server id in the redirect, so use it instead of the concocted 
       one _ilu_Parse_HTTP_URL'would normally give us */
    _http_redirected_server_id(p_call) = ilu_StrdupE(pc_new_serverid, p_error);
    if (ILU_ERRNOK(*p_error)) {
      *p_card_exception_code = 0;
      return ilu_ProtocolException_Unknown;
    }		
  }
	
  /* extract out the new protocol and transport information */
  if (_ilu_Parse_HTTP_URL(pc_new_location, 
			  &(_http_redirected_instance_handle(p_call)), 
			  (pc_new_serverid ? NIL : &(_http_redirected_server_id(p_call))), 
			  NIL, 
			  &pc_encodedContactInfo, &card_encodedContactInfo_length, 
			  &boolean_passEncodedCinfo, p_error)) {
		
    if (_ilu_ParseConnectInfo(pc_encodedContactInfo, card_encodedContactInfo_length, 
			      &pc_new_pinfo, &pc_new_tinfo, p_error)) {
      if (boolean_passEncodedCinfo)
	ilu_free(pc_encodedContactInfo);
      /* hang on to full sbh too in case we're dealing with a proxy server */
      _http_redirected_sbh(p_call) = ilu_StrdupE(pc_new_location, p_error);
      if (ILU_ERRNOK(*p_error)) {
	ilu_free(_http_redirected_instance_handle(p_call));
	_http_redirected_instance_handle(p_call) = NIL;
	ilu_free(_http_redirected_server_id(p_call));
	_http_redirected_server_id(p_call) = NIL;
      }
      else				
	/* set up the ilu_Error to indicate redirection */ 
	ILU_ERR_CONS3(relocate,p_error,rel_scope,ilu_relocate_call,rel_pinfo,pc_new_pinfo,rel_tinfo,pc_new_tinfo,0);
    } else {
      if (boolean_passEncodedCinfo) ilu_free(pc_encodedContactInfo);
      ilu_free(_http_redirected_instance_handle(p_call));
      _http_redirected_instance_handle(p_call) = NIL;
      ilu_free(_http_redirected_server_id(p_call));
      _http_redirected_server_id(p_call) = NIL;
    }		
  }
	
  ilu_free(pc_encodedContactInfo);
	
  /* indicate success */
  *p_card_exception_code = 0;
  return ilu_ProtocolException_Not;
}


	
/* ********************************************************* */
/* interprets http reply                                     */

static ilu_ProtocolException _http_interpret_reply 
	(ilu_Call p_call, ilu_cardinal* p_card_exception_code, ILU_ERRS((IoErrs)) * p_error) {

	ILU_CLER(*p_error);

	switch (_http_get_call_state(p_call)) {

	case clnt2http_in_interpret_reply:

		/* note that any returned error in the status line will be placed
		in the Response Record's Status Line for examination by the client,
		so we don't tell ilu that there was any problem */

		/* check for temporary redirection */
		if (_http_status_code(p_call) == 302) { /* "Moved Temporarily" */

			/* only do automatic redirects for GET or HEAD operations - per spec*/
			if (_http_is_GET_method(p_call) || _http_is_HEAD_method(p_call)) {

				/* set to redirect state (should end up in _http_prefinish_call next) */
				_http_set_call_state(p_call, clnt2http_redirect_call);
				
				return _http_setup_redirection(p_call, p_card_exception_code, p_error);
			}
		}


		/* set to next state */
		_http_set_call_state(p_call, clnt2http_in_Response_record);

		/* indicate success */
		*p_card_exception_code = 0;
		return ilu_ProtocolException_Success;


	case clnt2ilu_in_interpret_reply:

		/* take a look at the status code and reason phrase 
		   to look for exceptions */
		if (strcmp(_http_version_reason_phrase(p_call),
				g_c_protocol_exception) == 0) {
			/* had a protocol exception */

			/* just treat as if we were reading in return values */
			_http_set_call_state(p_call, clnt2ilu_in_return_values);

			*p_card_exception_code = 0;
			return _http_status_code(p_call);

		}
		else if (strcmp(_http_version_reason_phrase(p_call),
				g_c_non_protocol_exception) == 0) {
				/* had a non protocol exception */

				/* just treat as if we were reading in return values */
				_http_set_call_state(p_call, clnt2ilu_in_return_values);

				*p_card_exception_code = _http_status_code(p_call);
				return ilu_ProtocolException_Success;
		}	
		/* check for temporary redirection */
		else if (_http_status_code(p_call) == 302) { /* "Moved Temporarily" */

			/* set to redirect state (should end up in _http_prefinish_call next) */
			_http_set_call_state(p_call, clnt2ilu_redirect_call);

			return _http_setup_redirection(p_call, p_card_exception_code, p_error);
		}
		else {
			/* everything must have been OK */

			/* set to next state */
			_http_set_call_state(p_call, clnt2ilu_in_return_values);

			/* indicate success */
			*p_card_exception_code = 0;
			return ilu_ProtocolException_Success;
		}


	default:
		HTTP_UNEXPECTED_STATE_ASSERT();
	}

	/* indicate problem (never really reached) */
	*p_card_exception_code = 0;
	return ilu_ProtocolException_Unknown;
}

/* ********************************************************* */
/* finishes off record and sequence                          */
/* ********************************************************* */

/* ********************************************************* */
/* finishes off a record                                     */

static void _http_end_record (ilu_Call p_call,
				    ILU_ERRS((IoErrs)) * p_error) {

	ILU_CLER(*p_error);

	/* mostly merely progress to the next state */
	switch (_http_get_call_state(p_call)) {

	case clnt2http_in_end_Header_record :

		/* advance to next header in list */
		_http_headers_current_node(p_call) = 
			_http_headers_current_node(p_call)->m_p_next_list_node;

		if (_http_headers_current_node(p_call) == NIL)
			/* we're done with the headers */
			_http_set_call_state(p_call, clnt2http_in_end_Header_sequence);
			/* else we have more headers to deal with  */
		else _http_set_call_state(p_call, clnt2http_in_Header_record);

		break;

	case srvr4http_in_end_Header_record :

		/* advance to next header in list */
		_http_headers_current_node(p_call) = 
			_http_headers_current_node(p_call)->m_p_next_list_node;

		if (_http_headers_current_node(p_call) == NIL)
			/* we're done with the headers */
			_http_set_call_state(p_call, srvr4http_in_end_Header_sequence);
			/* else we have more headers to deal with  */
		else _http_set_call_state(p_call, srvr4http_in_Header_record);

		break;

	case clnt2http_out_end_Header_record :

		/* write out trailing cr lf */
		_http_transport_write_bytes(http_call_tport(p_call), g_c_CRLF, 2, p_error);
		if (ILU_ERRNOK(*p_error))
			return;

		/* advance to next header */
		_http_num_headers_to_process(p_call)--;

		if (_http_num_headers_to_process(p_call) == 0)
			/* we're done with the headers */
			_http_set_call_state(p_call, clnt2http_out_end_Header_sequence);
			/* else we have more headers to deal with  */
		else _http_set_call_state(p_call, clnt2http_out_Header_record);

		break;

	case srvr4http_out_end_Header_record :

		/* write out trailing cr lf */
		_http_transport_write_bytes(http_call_tport(p_call), g_c_CRLF, 2, p_error);
		if (ILU_ERRNOK(*p_error))
			return;

		/* advance to next header */
		_http_num_headers_to_process(p_call)--;

		if (_http_num_headers_to_process(p_call) == 0)
			/* we're done with the headers */
			_http_set_call_state(p_call, srvr4http_out_end_Header_sequence);
			/* else we have more headers to deal with  */
		else _http_set_call_state(p_call, srvr4http_out_Header_record);
		break;

	case clnt2http_in_end_Response_record :
		_http_set_call_state(p_call, clnt2http_in_reply_read);
		break;

	case srvr4http_in_end_Request_record :
		_http_set_call_state(p_call, srvr4http_in_request_read);
		break;

	case clnt2http_out_end_Request_record :
		_http_set_call_state(p_call, clnt2http_out_finish_request);
		break;

	case srvr4http_out_end_Response_record :
		_http_set_call_state(p_call, srvr4http_out_finish_reply);
		break;

	case clnt2ilu_in_return_values :
		break;

	case srvr4ilu_in_arguments :
		break;

	case clnt2ilu_out_arguments :
		break;

	case srvr4ilu_out_return_values :
		break;

	case srvr4ilu_out_exception:
		break;

	case clnt2http_out_method_name: /* may end up here in these states because of */
	case clnt2ilu_out_method_name:	/* the way some lang mappings make their sizing calls */
	case srvr4http_out_begin_reply:
	case srvr4ilu_out_begin_reply:
	      case http_init_state:	/* Possible because of sizing before start_request */
		break;

	default:
		HTTP_UNEXPECTED_STATE_ASSERT();
	}
}


/* ********************************************************* */
/* finishes off a sequence                                   */

static void _http_end_sequence (ilu_Call p_call,
				    ILU_ERRS((IoErrs)) * p_error) {
	_http_call_state state;
	ILU_CLER(*p_error);

	/* merely progress to the next state */
	switch ((state = _http_get_call_state(p_call))) {

	case clnt2http_in_end_Header_sequence:
		_http_set_call_state(p_call, clnt2http_in_Body_present);
		break;

	case clnt2http_out_end_Header_sequence:

		/* send indication that we're willing to keep the connection alive if we're http_1_0p 
		but never send this to a proxy since if it doesn't understand connection headers
		it may pass this on to the actual server, resulting in a hung proxy (waiting for
		the server to close the connection (xxx how can we know if it understands connection headers? ) */
		 if ( _http_persistent(p_call) &&
			 (getenv(ILU_HTTP_PROXY_INFO_ENV_VAR) == NIL)) {
	
			_http_transport_write_bytes(http_call_tport(p_call), "Connection: Keep-Alive", 22, p_error);
			if (ILU_ERRNOK(*p_error))
			  return;

			_http_transport_write_bytes(http_call_tport(p_call), g_c_CRLF, 2, p_error);
			if (ILU_ERRNOK(*p_error))
				return;
		  }
		
		 /* version 1.1 of http requires that a Host header is sent */
		 if (_http_protocol_minor_version(p_call) == 1 &&
			 _http_protocol_major_version(p_call) == 1 &&
			 !_http_user_supplied_host(p_call)) {
			 
			 /* add a Host header */  

			char c_host_header_buffer[1024];
			_http_create_host_header(p_call, c_host_header_buffer);

			_http_transport_write_bytes(http_call_tport(p_call), c_host_header_buffer, strlen(c_host_header_buffer), p_error);
			if (ILU_ERRNOK(*p_error))
			  return;

			_http_transport_write_bytes(http_call_tport(p_call), g_c_CRLF, 2, p_error);
			if (ILU_ERRNOK(*p_error))
				return;
		 }


		/* we would normally think we should write out the header's
		trailing crlf here, but we won't because we want to ensure
		that there is a Content-Length header that reflects the length
		of the body (if there is one).  So, in state, clnt2http_out_Body_present
		we'll write out the trailing cr lf if there's no body, and in 
		clnt2http_out_Body we'll write out a Content-Length header,
		the trailing crlf, and then the body bytes */
		
		_http_set_call_state(p_call, clnt2http_out_Body_present);
		break;

	case srvr4http_in_end_Header_sequence:
		_http_set_call_state(p_call, srvr4http_in_Body_present);
		break;

	case srvr4http_out_end_Header_sequence:
				
	/* send back to the 1.0 client that we're willing to keep the connection alive if we're 
		http_1_0p or http_1_1 and this was asked for */
		if ( _http_minor_version(p_call) == 0 &&
			_http_major_version(p_call) == 1 && _http_keep_alive(p_call) &&
			(_http_protocol_persistence(p_call) || 
			( _http_protocol_minor_version(p_call) == 1 && _http_protocol_major_version(p_call) == 1) )
			) {
			
			_http_transport_write_bytes(http_call_tport(p_call), "Connection: Keep-Alive", 22, p_error);
			if (ILU_ERRNOK(*p_error))
				return;
			
			_http_transport_write_bytes(http_call_tport(p_call), g_c_CRLF, 2, p_error);
			if (ILU_ERRNOK(*p_error))
				return;
		}
		
		/* if we're 1.1 and shouldn't keep the connection alive, we need to send the Connection: close header */
		if (_http_minor_version(p_call) == 1 &&
			_http_major_version(p_call) == 1 && 
			!_http_keep_alive(p_call)) {
			
			_http_transport_write_bytes(http_call_tport(p_call), "Connection: close", 17, p_error);
			if (ILU_ERRNOK(*p_error))
				return;
			
			_http_transport_write_bytes(http_call_tport(p_call), g_c_CRLF, 2, p_error);
			if (ILU_ERRNOK(*p_error))
				return;
		}
		
		/* we would normally think we should write out the header's
		trailing crlf here, but we won't because we want to ensure
		that there is a Content-Length header that reflects the length
		of the body (if there is one).  So, in state, srvr4http_out_Body_present
		we'll write out the trailing cr lf if there's no body, and in 
		srvr4http_out_Body we'll write out a Content-Length header,
		the trailing crlf, and then the body bytes */
		
		_http_set_call_state(p_call, srvr4http_out_Body_present);
		break;
		
	case clnt2ilu_in_return_values :
		break;

	case srvr4ilu_in_arguments :
		break;

	case clnt2ilu_out_arguments :
		break;

	case srvr4ilu_out_return_values :
		break;

	case srvr4ilu_out_exception:
		break;

	case clnt2http_out_method_name: /* may end up here in these states because of */
	case clnt2ilu_out_method_name:	/* the way some lang mappings make their sizing calls */
	case srvr4http_out_begin_reply:
	case srvr4ilu_out_begin_reply:
	      case http_init_state:
		break;

	default:
		HTTP_UNEXPECTED_STATE_ASSERT();
	}
}


/* ********************************************************* */
/* begin and finish an exception                             */
/* ********************************************************* */

static ilu_boolean _http_begin_exception (ilu_Call p_call, 
										  ilu_cardinal card_exception_code, 
										  ilu_ProtocolException sys_ex_index, 
										  ilu_cardinal card_reply_size,
										  ILU_ERRS((IoErrs)) * p_error) {
	ilu_cardinal card_code;
	ilu_string pc_reason_phrase;
	char c_buffer[128];
	ilu_Transport the_transport;
	ilu_shortcardinal scard_reason_code;

	ILU_CLER(*p_error);

	the_transport = http_call_tport(p_call);

	/* if we're on a boundaried transport */
	if ((the_transport->tr_class->tc_boundaried) && 
	    (transport_begin_message(the_transport, ilu_FALSE, p_error) != ilu_rhrc_ok))
		return ilu_FALSE;


	switch (_http_get_call_type(p_call)) {

	case srvr4something:
	case srvr4http:

		if (_http_get_call_state(p_call) != srvr4http_missing_1_1_host_header)
			scard_reason_code = 500;
		else 
			scard_reason_code = 400;

		/* send back an internal server error since any other http'ish response code would
		simply be in the status line of the Response's Status Line 
		xxx note should make the 'phrase' be more indicative of the actual error */
		sprintf(c_buffer, "HTTP/%d.%d %hd %s\r\n", _http_major_version(p_call), _http_minor_version(p_call),
		scard_reason_code, _http_phrase_of_status_code(scard_reason_code));

		_http_transport_write_bytes(http_call_tport(p_call), c_buffer, strlen(c_buffer), p_error);
		
		if (ILU_ERRNOK(*p_error)) 
			return ilu_FALSE;

		/* Now all the output functions check to see if we're in the srvr4http_out_exception
		state, and if so, actually output nothing */

		_http_set_call_state(p_call, srvr4http_out_exception);
		break;

	case srvr4ilu:

		/* if "card_exception_code" == 0, then sys_ex_index contains a protocol 
		    exception detail code */

		if (card_exception_code == 0) {
			card_code = sys_ex_index;
			pc_reason_phrase = g_c_protocol_exception;
		}
		else {
			card_code = card_exception_code;
			pc_reason_phrase = g_c_non_protocol_exception;
		}

		/* send back an error code and phrase indicating that an exception occurred */
		sprintf(c_buffer, "HTTP/%d.%d %lu %s\r\n", 
			_http_major_version(p_call), _http_minor_version(p_call),
			(long unsigned int) card_code, pc_reason_phrase);

		_http_transport_write_bytes(http_call_tport(p_call), c_buffer, strlen(c_buffer), p_error);
		
		if (ILU_ERRNOK(*p_error)) 
			return ilu_FALSE;

	   /* Create an in memory transport buffer into which the rest of the call will
		be written so that we can determine the body length in order to add a 
		content length header */ 
		_http_switch_to_buffering_body(p_call, p_error);

		if (ILU_ERRNOK(*p_error))
			return ilu_FALSE;
			
		/* now just proceed basically as if we were sending back return values */
		_http_set_call_state(p_call, srvr4ilu_out_exception);

		break;

	default:
		HTTP_UNEXPECTED_STATE_ASSERT();
	}

	return ilu_TRUE;
}



static ilu_boolean 
_http_finish_exception(ilu_Call p_call,
		       ilu_boolean b_push,
		       ILU_ERRS((IoErrs)) * p_error) {
	ILU_CLER(*p_error);

	switch (_http_get_call_state(p_call)) {

	case srvr4http_out_exception:

		/* put out final crlf (after status line) */
		_http_transport_write_bytes(http_call_tport(p_call), g_c_CRLF, 2, p_error);
		
		if (ILU_ERRNOK(*p_error)) 
			return ilu_FALSE;

		_http_set_call_state(p_call, srvr4http_finish_call);

		break;

	case srvr4ilu_out_exception: 

		/* send out a content length header and the buffered body bytes */
		_http_send_buffered_body(p_call, p_error) ;

		if (ILU_ERRNOK(*p_error))
			return ilu_FALSE;		

		/* get here after sending back exception members */
		_http_set_call_state(p_call, srvr4ilu_finish_call);
		break;

	default:
		HTTP_UNEXPECTED_STATE_ASSERT();
	}

	/* have the transport flush itself */
	_http_end_message(p_call, ilu_FALSE, b_push, p_error);

	return ILU_ERROK(*p_error);
}




/* ********************************************************* */
/* output and input functions                                */
/* ********************************************************* */


/* ********************************************************* */
/* outputs starting of a record (actually outputs nothing)   */

static void _http_output_record (ilu_Call p_call,
				 ilu_Type the_type,
				 ILU_ERRS((IoErrs)) * p_error) {

	ILU_CLER(*p_error);

	/* not much to do except advance state */
	switch (_http_get_call_state(p_call)) {

	case clnt2http_out_Request_record:

		/* set to next state */
		_http_set_call_state(p_call, clnt2http_out_Request_URI);
		break;

	case clnt2http_out_Header_record:

		/* set to next state */
		_http_set_call_state(p_call, clnt2http_out_Header_Name);

		break;

	case srvr4http_out_Response_record:

		/* set to next state */
		_http_set_call_state(p_call, srvr4http_out_Status_Line);

		break;

	case srvr4http_out_Header_record:

		/* set to next state */
		_http_set_call_state(p_call, srvr4http_out_Header_Name);

		break;

	case clnt2ilu_out_arguments: /* stay in same state */
		break;

	case srvr4ilu_out_return_values: /* stay in same state */
		break;

	case srvr4ilu_out_exception:
		break;

	case srvr4http_out_exception: 
		break;

	default:
		HTTP_UNEXPECTED_STATE_ASSERT();
	}

}

/* ********************************************************* */
/* inputs begining of a record                               */

static void _http_input_record (ilu_Call p_call, ilu_Type the_type,
				ILU_ERRS((IoErrs)) * p_error) {

	ILU_CLER(*p_error);

	switch (_http_get_call_state(p_call)) {

	case clnt2http_in_Response_record:

		/* set to next state */
		_http_set_call_state(p_call, clnt2http_in_Status_Line);

		break;

	case clnt2http_in_Header_record:

		/* set to next state */
		_http_set_call_state(p_call, clnt2http_in_Header_Name);

		break;

	case srvr4http_in_Request_record:

		/* set to next state */
		_http_set_call_state(p_call, srvr4http_in_Request_URI);

		break;

	case srvr4http_in_Header_record:

		/* set to next state */
		_http_set_call_state(p_call, srvr4http_in_Header_Name);

		break;


	case srvr4ilu_in_arguments:
	case clnt2ilu_in_return_values:

		/* just stay in same state */

		break;

	default:
		HTTP_UNEXPECTED_STATE_ASSERT();
	}
}


/* ********************************************************* */
/* outputs an enum                                           */

static void _http_output_enum_code (ilu_Call p_call, ilu_shortcardinal scard_enum,
				    ilu_Type the_type, ILU_ERRS((IoErrs)) * p_error) {

	char c_buffer[64];

	ILU_CLER(*p_error);

	switch (_http_get_call_state(p_call)) {

	case srvr4http_out_Status_Line:

		/* we're sending a status line in a reply */
		/* have the transport write out the version, code, phrase, and crlf */
		sprintf(c_buffer, "HTTP/%d.%d %hd ", _http_major_version(p_call), _http_minor_version(p_call),
			scard_enum);

		_http_transport_write_bytes(http_call_tport(p_call), c_buffer, strlen(c_buffer), p_error);
		
		if (ILU_ERRNOK(*p_error)) 
			return;

		_http_transport_write_bytes(http_call_tport(p_call), _http_phrase_of_status_code(scard_enum), 
			strlen(_http_phrase_of_status_code(scard_enum)), p_error);
		
		if (ILU_ERRNOK(*p_error)) 
			return;

		_http_transport_write_bytes(http_call_tport(p_call), g_c_CRLF, 2, p_error);
		
		if (ILU_ERRNOK(*p_error)) 
			return;

		/* set to next state */
		_http_set_call_state(p_call, srvr4http_out_Header_sequence);

		break;

	case clnt2ilu_out_arguments:
	case srvr4ilu_out_return_values:
	case srvr4ilu_out_exception:

		if (_http_write_cardinal_line(p_call, scard_enum, p_error) == ilu_FALSE)
			return;

		/* stay in same state */

		break;

	case srvr4http_out_exception: 
		/* don't send back exception members to existing http clients */
		break;

	default:
		HTTP_UNEXPECTED_STATE_ASSERT();
	}
}


/* ********************************************************* */
/* inputs an enumeration code                                */

static void _http_input_enum_code (ilu_Call p_call, ilu_shortcardinal* p_scard_code,
				   ilu_Type the_type, ILU_ERRS((IoErrs)) * p_error) {

	ilu_string pc_enum_line;

	ILU_CLER(*p_error);

	switch (_http_get_call_state(p_call)) {

	case clnt2http_in_Status_Line:

		/* return the code we extracted during get_status_line */
		*p_scard_code = _http_status_code(p_call);

		/* set to next state */
		_http_set_call_state(p_call, clnt2http_in_Header_sequence);

		return;

	case srvr4ilu_in_arguments:
	case clnt2ilu_in_return_values:

		pc_enum_line = _http_readline(p_call, NIL, p_error);
		if (pc_enum_line == NIL)
			return;

		if (sscanf(pc_enum_line, "%hd", p_scard_code) !=  1) 
			ILU_ERR_CONS1(marshal, p_error, minor, ilu_mm_badInteger, 0);

		ilu_free(pc_enum_line);

		return;

	default:
		HTTP_UNEXPECTED_STATE_ASSERT();
	}
}


/* ********************************************************* */
/*  outputs a string                                         */

static void _http_output_string (ilu_Call p_call, void * p_string,
				 ilu_cardinal card_strlength,
				 ilu_cardinal card_strlimit,
				 ilu_cardinal expected_encoding,
				 ilu_cardinal current_encoding,
				 ILU_ERRS((IoErrs)) * p_error) {

  	ilu_string pc_params_queries;
	char c_buffer[32];

	if ((expected_encoding != ILU_StringEncoding_latin1) ||
	    (current_encoding != ILU_StringEncoding_latin1)) {
	  ILU_ERR_CONS1(imp_limit, p_error, minor, ilu_ilm_unsupported_charset_encoding, 0);
	  return;
	};

	ILU_CLER(*p_error);

	if ((card_strlimit > 0) && (card_strlength > card_strlimit)) {
		ILU_ERR_CONS1(bad_param, p_error, minor, ilu_bpm_sequenceLimit, 0);
		return;
	}

	switch (_http_get_call_state(p_call)) {

	case clnt2http_out_Request_URI:

		/* Note that back in state clnt2http_out_URL_path we didn't finish off the 
		request line.  We waited till we got here, because at this point
		we have access to any params or queries that should be placed after the
		object id. */  

		/* find where params or queries (if any) start */
		pc_params_queries = (ilu_string) p_string;
		while (*pc_params_queries && (*pc_params_queries != ';') && (*pc_params_queries != '?'))
			pc_params_queries++;

		if (*pc_params_queries) {  /* some params or queries were present, write them out */
			_http_transport_write_bytes(http_call_tport(p_call), pc_params_queries, 
				strlen(pc_params_queries), p_error);
			if (ILU_ERRNOK(*p_error))
				return;
		}

		/* have the transport write out the space and version */
		sprintf(c_buffer, " HTTP/%d.%d\r\n", _http_major_version(p_call), _http_minor_version(p_call));

		_http_transport_write_bytes(http_call_tport(p_call), c_buffer, strlen(c_buffer), p_error);
		if (ILU_ERRNOK(*p_error))
			return ;

		/* NOTE That we never actually send the Request_URI over the wire - this will be
		just picked out of our http request line and reconstructed on the server side
		in the case of an ilu http server, and it wouldn't be recognized by an existing 
		http server daemon*/

		/* set to next state */
		_http_set_call_state(p_call, clnt2http_out_Header_sequence);
		break;

	case clnt2http_out_Header_Name:

		if (_http_strnstr(p_string, "Content-Length", card_strlength, ilu_FALSE) != NULL) {
			/* the user specified their own content-length header - make note of this
			so we don't automatically generate one later on */
			_http_user_supplied_content_length(p_call) = ilu_TRUE;
		}

		if (_http_strnstr(p_string, "Host", card_strlength, ilu_FALSE) != NULL) {
			/* the user specified their own host header - make note of this
			so we don't automatically generate one later on */
			_http_user_supplied_host(p_call) = ilu_TRUE;
		}

		/* have the transport write out the header, colon*/
		_http_transport_write_bytes(http_call_tport(p_call), (ilu_string) p_string, card_strlength, p_error);
		if (ILU_ERRNOK(*p_error))
			return ;

		_http_transport_write_bytes(http_call_tport(p_call), ":", 1, p_error);
		if (ILU_ERRNOK(*p_error))
			return ;

		/* set to next state */
		_http_set_call_state(p_call, clnt2http_out_Header_Value_present);

		return ;

	case clnt2http_out_Header_value:

		/* have the transport write out the space, value */
		_http_transport_write_bytes(http_call_tport(p_call), g_c_SP, 1, p_error);
		if (ILU_ERRNOK(*p_error))
			return ;

		_http_transport_write_bytes(http_call_tport(p_call), p_string, card_strlength, p_error);
		if (ILU_ERRNOK(*p_error))
			return ;

		/* set to next state */
		_http_set_call_state(p_call, clnt2http_out_end_Header_record);

		return ;

	case srvr4http_out_Header_Name:

		if (_http_strnstr((ilu_bytes)p_string, "Content-Length", card_strlength, ilu_FALSE) != NULL) {
			/* the user specified their own content-length header - make note of this
			so we don't automatically generate one later on */
			_http_user_supplied_content_length(p_call) = ilu_TRUE;
		}

		/* have the transport write out the header, colon*/
		_http_transport_write_bytes(http_call_tport(p_call), p_string, card_strlength, p_error);
		if (ILU_ERRNOK(*p_error))
			return ;

		_http_transport_write_bytes(http_call_tport(p_call), ":", 1, p_error);
		if (ILU_ERRNOK(*p_error))
			return ;

		/* set to next state */
		_http_set_call_state(p_call, srvr4http_out_Header_Value_present);

		return ;

	case srvr4http_out_Header_value:

		/* have the transport write out the space, value */
		_http_transport_write_bytes(http_call_tport(p_call), g_c_SP, 1, p_error);
		if (ILU_ERRNOK(*p_error))
			return ;

		_http_transport_write_bytes(http_call_tport(p_call), p_string, card_strlength, p_error);
		if (ILU_ERRNOK(*p_error))
			return ;

		/* set to next state */
		_http_set_call_state(p_call, srvr4http_out_end_Header_record);
		
		return ;

	case clnt2ilu_out_arguments:
	case srvr4ilu_out_return_values:
	case srvr4ilu_out_exception:
										
		/* write out the string length  - this is how much
		 space the receiver should allocate to input the string, -1 */
		if (_http_write_cardinal_line(p_call, card_strlength, p_error) == ilu_FALSE)
			return;

		/* have the transport write out the string followed by cr lf */
		_http_transport_write_bytes(http_call_tport(p_call), p_string, card_strlength, p_error);
		if (ILU_ERRNOK(*p_error))
			return ;

		_http_transport_write_bytes(http_call_tport(p_call), g_c_CRLF, 2, p_error);
		if (ILU_ERRNOK(*p_error))
			return ;

		/* stay in same state */

		break;

	case srvr4http_out_exception: 
		/* don't send back exception members to existing http clients */
		break;

	default:
		HTTP_UNEXPECTED_STATE_ASSERT();
	}
}


/* ********************************************************* */
/* inputs a string                                           */

static void _http_input_string (ilu_Call p_call, void ** ppc_string,
				ilu_cardinal* p_card_length,
				ilu_cardinal card_limit,
				ilu_cardinal expected_encoding,
				ilu_cardinal *current_encoding,
				ILU_ERRS((IoErrs)) * p_error) {
					
	ilu_string pc_string;
	ilu_cardinal card_size;
	char c_junk[8];

	if (expected_encoding != ILU_StringEncoding_latin1) {
	  ILU_ERR_CONS1(imp_limit, p_error, minor, ilu_ilm_unsupported_charset_encoding, 0);
	  return;
	};

	ILU_CLER(*p_error);
	*current_encoding = ILU_StringEncoding_latin1;

	*ppc_string = NIL;
	*p_card_length = 0;
	pc_string = NIL;

	switch (_http_get_call_state(p_call)) {

	case clnt2http_in_Header_Name:

		card_size = strlen(_http_list_node_contents(_http_headers_current_node(p_call)));

		if (card_limit > 0 && card_size > card_limit) {
			ILU_ERR_CONS1(marshal, p_error, minor, ilu_mm_sequenceLimit, 0);
			return;
		}

		/* just duplicate the header name we parsed our during _http_fill_headers_list */
		*ppc_string = (ilu_bytes) ilu_StrdupE(_http_list_node_contents(_http_headers_current_node(p_call)),
			p_error);
		if (ILU_ERRNOK(*p_error)) return; 
		*p_card_length = card_size;

		/* set to next state */
		_http_set_call_state(p_call, clnt2http_in_Header_Value_present);

		break;

	case clnt2http_in_Header_value:

		card_size = strlen(_http_list_node_into_contents(_http_headers_current_node(p_call)));

		if (card_limit > 0 && card_size > card_limit) {
			ILU_ERR_CONS1(marshal, p_error, minor, ilu_mm_sequenceLimit, 0);
			return;
		}

		/* just duplicate the header value we parsed our during _http_fill_headers_list */
		*ppc_string = (ilu_bytes) ilu_StrdupE(_http_list_node_into_contents(_http_headers_current_node(p_call)),
			p_error);
		if (ILU_ERRNOK(*p_error)) return; 
		*p_card_length = card_size;

		/* set to next state */
		_http_set_call_state(p_call, clnt2http_in_end_Header_record);

		break;

	case srvr4http_in_server_id:

		card_size = strlen(server_id(call_server(p_call)));

		if (card_limit > 0 && card_size > card_limit) {
			ILU_ERR_CONS1(marshal, p_error, minor, ilu_mm_sequenceLimit, 0);
			return;
		}

		/* since there is no notion of a server id for straight http, we just
		   return the server's id string */

		*ppc_string = (ilu_bytes) ilu_StrdupE(server_id(call_server(p_call)), p_error);
		if (ILU_ERRNOK(*p_error)) { 
			*ppc_string = NIL;
			return ;
		}
		*p_card_length = card_size;

		/* set to next state */
		_http_set_call_state(p_call, srvr4http_in_discriminator_id);

		break;

	case srvr4http_in_discriminator_id:

		card_size = strlen(_http_object_id(p_call));

		if (card_limit > 0 && card_size > card_limit) {
			ILU_ERR_CONS1(marshal, p_error, minor, ilu_mm_sequenceLimit, 0);
			return;
		}

		/* return a copy of the object id we saved during get_request_line */
		*ppc_string = (ilu_bytes) ilu_StrdupE(_http_object_id(p_call), p_error);
		if (ILU_ERRNOK(*p_error)) { 
			*ppc_string = NIL;
			return ;
		}
		*p_card_length = card_size;

		/* set to next state */
		_http_set_call_state(p_call, srvr4http_in_Request_record);

		break;

	case srvr4http_in_Request_URI:

		/* we already have this stashed (with null terminated parts) in our
		call data m_pc_request_or_status_line from when we did get_request_line */

		pc_string = ilu_MallocE(strlen(_http_request_uri(p_call)) + 
			(_http_params_queries(p_call) ? strlen(_http_params_queries(p_call)) + 1 : 0) +
			1, p_error);

		if (pc_string == NIL) 
			return;

		if (_http_params_queries(p_call)) 
			sprintf(pc_string, "%s%c%s", _http_request_uri(p_call), 
				_http_params_queries_delim(p_call),
				_http_params_queries(p_call));
		else
			sprintf(pc_string, "%s", _http_request_uri(p_call));
		
		card_size = strlen(pc_string);

		if (card_limit > 0 && card_size > card_limit) {
			ilu_free(pc_string);
			ILU_ERR_CONS1(marshal, p_error, minor, ilu_mm_sequenceLimit, 0);
			return;
		}

		*p_card_length = card_size;
		*ppc_string = (ilu_bytes) pc_string;

		/* set to next state */
		_http_set_call_state(p_call, srvr4http_in_Header_sequence);

		break;

	case srvr4http_in_Header_Name:

		/* just duplicate the header name we parsed our during _http_fill_headers_list */
		pc_string = ilu_StrdupE(_http_list_node_contents(_http_headers_current_node(p_call)),
			p_error);
		if (ILU_ERRNOK(*p_error)) return; 

		card_size = strlen(pc_string);

		if (card_limit > 0 && card_size > card_limit) {
			ilu_free(pc_string);
			ILU_ERR_CONS1(marshal, p_error, minor, ilu_mm_sequenceLimit, 0);
			return;
		}

		*p_card_length = card_size;
		*ppc_string = (ilu_bytes) pc_string;

		/* if we're version 1.1, make a note if we got a host header */
		if (_http_minor_version(p_call) == 1 &&
			_http_major_version(p_call) == 1 &&
			!_http_user_supplied_host(p_call)) {
			char* pc_content = _http_list_node_contents(_http_headers_current_node(p_call));
			/* we have a host header it it says Host and there is some value */
			if ((strstr(pc_content, "Host") == pc_content) &&
				_http_list_node_into_contents(_http_headers_current_node(p_call)))
				_http_user_supplied_host(p_call) = ilu_TRUE;
		}

		/* set to next state */
		_http_set_call_state(p_call, srvr4http_in_Header_Value_present);

		break;

	case srvr4http_in_Header_value:

		/* just duplicate the header value we parsed our during _http_fill_headers_list */
		pc_string = ilu_StrdupE(_http_list_node_into_contents(_http_headers_current_node(p_call)),
			p_error);
		if (ILU_ERRNOK(*p_error)) return; 

		card_size = strlen(pc_string);

		if (card_limit > 0 && card_size > card_limit) {
			ilu_free(pc_string);
			ILU_ERR_CONS1(marshal, p_error, minor, ilu_mm_sequenceLimit, 0);
			return;
		}

		*p_card_length = card_size;
		*ppc_string = (ilu_bytes) pc_string;

		/* set to next state */
		_http_set_call_state(p_call, srvr4http_in_end_Header_record);

		break;

	case srvr4ilu_in_server_id:

		/* just duplicate the server id we parsed our during _http_fill_headers_list */
		pc_string = ilu_StrdupE(_http_server_id(p_call), p_error);
		if (ILU_ERRNOK(*p_error)) 
			return; 

		card_size = strlen(pc_string);

		if (card_limit > 0 && card_size > card_limit) {
			ilu_free(pc_string);
			ILU_ERR_CONS1(marshal, p_error, minor, ilu_mm_sequenceLimit, 0);
			return;
		}

		*p_card_length = card_size;
		*ppc_string = (ilu_bytes) pc_string;

		/* set to next state */
		_http_set_call_state(p_call, srvr4ilu_in_discriminator_id);

		break;

	case srvr4ilu_in_discriminator_id:

		/* just duplicate the object id we parsed our during get_request_line */
		pc_string = ilu_StrdupE(_http_object_id(p_call), p_error);
		if (ILU_ERRNOK(*p_error)) 
			return; 

		card_size = strlen(pc_string);

		if (card_limit > 0 && card_size > card_limit) {
			ilu_free(pc_string);
			ILU_ERR_CONS1(marshal, p_error, minor, ilu_mm_sequenceLimit, 0);
			return;
		}

		*p_card_length = card_size;
		*ppc_string = (ilu_bytes) pc_string;

		/* set to next state */
		_http_set_call_state(p_call, srvr4ilu_in_arguments);

		break;

	case srvr4ilu_in_arguments:
	case clnt2ilu_in_return_values:

		/* read in the string size */
		if (_http_read_cardinal_line(p_call, &card_size, p_error) == ilu_FALSE) {
			ILU_ERR_CONS1(marshal, p_error, minor, ilu_mm_badInteger, 0);
			goto bad_string_label;
		}

		if (card_limit > 0 && card_size > card_limit) {
			ILU_ERR_CONS1(marshal, p_error, minor, ilu_mm_sequenceLimit, 0);
			return;
		}

		*p_card_length = card_size;

		/* alloc space to hold the string, plus a null to terminate it in case that's what the
		 language expects */
		*ppc_string = ilu_MallocE((*p_card_length) + 1, p_error);
		if (ILU_ERRNOK(*p_error))
			goto bad_string_label;

		/* read in the string */
		_http_transport_read_bytes(http_call_tport(p_call), *ppc_string, *p_card_length, p_error);
		if (ILU_ERRNOK(*p_error))
			goto bad_string_label;

		/* null terminate it */
		*(((ilu_byte *)(*ppc_string)) + (*p_card_length)) = '\0';

		/* read up the trailing crlf we put there */
		_http_transport_read_bytes(http_call_tport(p_call), c_junk, 2, p_error);
		if (ILU_ERRNOK(*p_error)) 
			goto bad_string_label;

		return;

		bad_string_label:

		ilu_free(*ppc_string);
		*ppc_string = NIL;
		*p_card_length = 0;
		return;

	default:
		HTTP_UNEXPECTED_STATE_ASSERT();
	}
}


/* ********************************************************* */
/* output an object id                                       */
/* note that when the function is called, we have 'entered the 
server', and must 'exit it' before leaving the function, or 
the lock will be still on. */

static ilu_boolean _http_output_object_id (ilu_Call p_call, ilu_Object p_object,
										   ilu_boolean b_discriminator_p,
										   ilu_Class p_class,
										   ILU_ERRS((IoErrs)) * p_error) {
	   char c_buffer[32];
	   char* pc_object_ih = NIL;
	   char* pc_server_id = NIL;
	   unsigned int ui_length_object_ih = 0;
	   
	   ILU_CLER(*p_error);
	   
	   /* ensure we have valid args */
	   if (call_connection(p_call) == NIL) {
		   if (p_object != NIL)	/* release the lock if we had bad args */
			   ilu_ExitServer(object_server(p_object), object_class(p_object));
		   return ILU_ERR_CONS1(comm_failure, p_error, minor, ilu_cfm_conn_lost, ilu_FALSE);
	   }

	   /* first get a hold of a copy of the objects instance handle w
	      while we have the server lock */ 
	   if (p_object != NIL) {
		   pc_object_ih = ilu_StrdupE(object_ih(p_object), p_error);
		   if (ILU_ERRNOK(*p_error)) {
			   ilu_ExitServer(object_server(p_object), object_class(p_object));
			   return ilu_FALSE;	
		   }
		   ui_length_object_ih = strlen(pc_object_ih);
	   }	   
	   
	   switch (_http_get_call_state(p_call)) {
		   
	   case clnt2http_out_URL_path:
		   
		   if (!b_discriminator_p) {
		   /* we should never get a non descriminator here if this 
			   is a call to a http_resource_object */
			   ilu_ExitServer(object_server(p_object), object_class(p_object));
			   ILU_ERR_CONS1(bad_param, p_error, minor, ilu_bpm_OID, 0);
			   goto cleanup_label;
		   }
		   
		   /* write out the object identifier as the URL path */
		   
		   /* xxx *really* need to better determine if this method is being sent to a 
		   proxy server, and if so, need to output full sbh instead
		   of just object_id this way has problems if ILU_HTTP_PROXY_INFO_ENV_VAR
		   is set dynamically */
		   if (_http_redirected_instance_handle(p_call) == NIL) {
			   /* normal non-redirected call */
			   
			   if (getenv(ILU_HTTP_PROXY_INFO_ENV_VAR) == NIL) {
				   /* assume we're not using any proxy */
				   ilu_ExitServer(object_server(p_object), object_class(p_object)); 
				   _http_transport_write_bytes(http_call_tport(p_call), pc_object_ih, 
					   ui_length_object_ih, p_error);
			   }
			   else {
				   char* pc_absurl;
				   char* pc_sbh;
				   char* pc_sbh_dup;
				   pc_sbh = ilu_SBHOfObject(p_object);
				   ilu_ExitServer(object_server(p_object), object_class(p_object)); 
				   if (pc_sbh == ILU_NIL) {
					   ILU_NOTE(HTTP_DEBUG,
						   ("_http_output_object_id: can't form SBH for object %s/%s.\n",
						   server_id(object_server(p_object)), pc_object_ih));
					   ILU_ERR_CONS1(bad_param, p_error, minor, ilu_bpm_not_exported, NIL);
					   goto cleanup_label;
				   };
				   pc_sbh_dup = ilu_StrdupE(pc_sbh, p_error);
				   if (ILU_ERRNOK(*p_error))
					   goto cleanup_label;
				   pc_absurl = _http_SBH_to_URL(pc_sbh_dup, p_error);
				   if (ILU_ERRNOK(*p_error)) {
					   ilu_free(pc_absurl);
					   goto cleanup_label;
				   }
				   if (!pc_absurl) {
					   ILU_ERR_CONS1(bad_param, p_error, minor, ilu_bpm_URL, 0);
					   goto cleanup_label;
				   }
				   _http_transport_write_bytes(http_call_tport(p_call), pc_absurl, strlen(pc_absurl), p_error);
				   ilu_free(pc_absurl);
			   }
		   }
		   
		   else { /* we're doing a redirect */
			   
			   ilu_ExitServer(object_server(p_object), object_class(p_object)); 
			   
			   if (getenv(ILU_HTTP_PROXY_INFO_ENV_VAR) == NIL)  {
				   /* assume we're not using any proxy */
				   _http_transport_write_bytes(http_call_tport(p_call), _http_redirected_instance_handle(p_call), 
					   strlen(_http_redirected_instance_handle(p_call)), p_error);
			   }
			   else
				   _http_transport_write_bytes(http_call_tport(p_call), _http_redirected_sbh(p_call) , 
				   strlen(_http_redirected_sbh(p_call)), p_error);
			   
			   /* free up the redirection info */
			   ilu_free(_http_redirected_instance_handle(p_call));
			   _http_redirected_instance_handle(p_call) = NIL;
			   ilu_free(_http_redirected_server_id(p_call));
			   _http_redirected_server_id(p_call) = NIL;
			   ilu_free(_http_redirected_sbh(p_call));
			   _http_redirected_sbh(p_call) = NIL;
		   }
		   
		   
		   if (ILU_ERRNOK(*p_error))
			   goto cleanup_label;
		   
			   /* Note that at this point we don't yet finish off the request line.  We
			   wait till we get to state clnt2http_out_Request_URI, because at that point
			   we'll have access to any params or queries that should be placed after the
		   object id. */  
		   
		   /* set to next state */
		   _http_set_call_state(p_call, clnt2http_out_Request_record);
		   
		   break;
		   
	   case clnt2ilu_out_object_id:
		   
		   if (b_discriminator_p) {	/* if we're the descriminator object */
			   
			   if (p_object == NIL) {	/* ensure object isn't nil */
				   ILU_ERR_CONS1(bad_param, p_error, minor, ilu_bpm_OID, ilu_FALSE);
				   goto cleanup_label;
			   }
			   
			   pc_server_id = ilu_StrdupE(server_id(call_server(p_call)), p_error);
			   if (ILU_ERRNOK(*p_error)) {
				   ilu_ExitServer(object_server(p_object), object_class(p_object));
				   return ilu_FALSE;	
			   }

			   ilu_ExitServer(object_server(p_object), object_class(p_object)); 
			   
			   /* have the transport write out object id, the space and version */
			   if (_http_redirected_instance_handle(p_call) == NIL) { /* normal call */
				   
				   _http_transport_write_bytes(http_call_tport(p_call), pc_object_ih, 
					   ui_length_object_ih, p_error);
			   }
			   else  {/* redirected call */
				   _http_transport_write_bytes(http_call_tport(p_call), _http_redirected_instance_handle(p_call), 
					   strlen(_http_redirected_instance_handle(p_call)), p_error);
			   }
			   if (ILU_ERRNOK(*p_error))
				   goto cleanup_label;
			   
			   sprintf(c_buffer, " HTTP/%d.%d\r\n", _http_major_version(p_call), _http_minor_version(p_call));
			   _http_transport_write_bytes(http_call_tport(p_call), c_buffer, strlen(c_buffer), p_error);
			   if (ILU_ERRNOK(*p_error))
				   goto cleanup_label;
			   
			   /* now write out a single http header that contains the server id */
			   _http_transport_write_bytes(http_call_tport(p_call), ILU_SERVER_ID_HEADER_NAME, 
				   ILU_SERVER_ID_HEADER_NAME_LENGTH, p_error);
			   if (ILU_ERRNOK(*p_error))
				   goto cleanup_label;
			   
			   /* write out separating colon and space */
			   _http_transport_write_bytes(http_call_tport(p_call), ": ", 2, p_error);
			   if (ILU_ERRNOK(*p_error))
				   goto cleanup_label;
			   
			   if (_http_redirected_server_id(p_call) == NIL) /* normal call */
				   _http_transport_write_bytes(http_call_tport(p_call), pc_server_id, 
				   strlen(pc_server_id), p_error);
			   else /* redirected call */
				   _http_transport_write_bytes(http_call_tport(p_call), _http_redirected_server_id(p_call), 
				   strlen(_http_redirected_server_id(p_call)), p_error);
			   if (ILU_ERRNOK(*p_error))
				   goto cleanup_label;
			   
			   /* write out the crlf we need at the end of the header */
			   _http_transport_write_bytes(http_call_tport(p_call), g_c_CRLF, 2, p_error);
			   if (ILU_ERRNOK(*p_error))
				   goto cleanup_label;
			   
				   /* Create an in memory transport buffer into which the rest of the call will
				   be written so that we can determine the body length in order to add a 
			   content length header */ 
			   _http_switch_to_buffering_body(p_call, p_error);
			   if (ILU_ERRNOK(*p_error))
				   goto cleanup_label;
			   
			   /* set to next state */
			   _http_set_call_state(p_call, clnt2ilu_out_arguments);
		   }
		   else /* shouldn't be in this state if we're not the discriminant */
			   HTTP_UNEXPECTED_STATE_ASSERT();
		   
		   break;
		   
	   case clnt2ilu_out_arguments: 
		   
		   if (!b_discriminator_p) { /* we're not the discrimnator object */
			   
			   /* else if we're nil, make sure we're of a class that's OPTIONAL */
			   if (p_object == NIL && !p_class->cl_optional) {
				   ILU_ERR_CONS1(bad_param, p_error, minor, ilu_bpm_OID, ilu_FALSE);
				   goto cleanup_label;
			   }
			   
			   if (p_object == NIL) { 
				   /* write out our nil object indicator */
				   _http_output_string(p_call, (ilu_bytes) g_c_NILOBJ, strlen(g_c_NILOBJ), 0xFFFF,
					   ILU_StringEncoding_latin1, ILU_StringEncoding_latin1, p_error);
				   if (ILU_ERRNOK(*p_error))
					   goto cleanup_label;
			   }
			   else { /* write out the object's sbh */
				   ilu_string pc_sbh =  ilu_SBHOfObject(p_object);
				   ilu_ExitServer(object_server(p_object), object_class(p_object)); 
				   if (pc_sbh == ILU_NIL) {
					   ILU_NOTE(HTTP_DEBUG,
						   ("_http_output_object_id: can't form SBH for object %s/%s.\n",
						   server_id(object_server(p_object)), pc_object_ih));
					   ILU_ERR_CONS1(bad_param, p_error, minor, ilu_bpm_not_exported, NIL);
					   goto cleanup_label;
				   };
				   _http_output_string(p_call, (ilu_bytes) pc_sbh, strlen(pc_sbh), 0xFFFF,
					   ILU_StringEncoding_latin1, ILU_StringEncoding_latin1, p_error);
				   if (ILU_ERRNOK(*p_error))
					   goto cleanup_label;
			   }
		   }
		   else /* shouldn't be in this state if we're not the descriminant */
			   HTTP_UNEXPECTED_STATE_ASSERT();
		   
		   /* stay in same state */
		   
		   break;
		   
	   case srvr4http_out_exception: 
		   /* don't send back exception members to existing http clients */
		   break;
		   
	   case srvr4ilu_out_exception:
	   case srvr4ilu_out_return_values:
		   
		   /* if we're nil, make sure we're of a class that's OPTIONAL */
		   if (p_object == NIL && !p_class->cl_optional) {
			   ILU_ERR_CONS1(bad_param, p_error, minor, ilu_bpm_OID, ilu_FALSE);
			   goto cleanup_label;
		   }
		   
		   if (p_object == NIL) { 
			   /* write out our nil object indicator */
			   _http_output_string(p_call, (ilu_bytes) g_c_NILOBJ, strlen(g_c_NILOBJ), 0xFFFF,
				   ILU_StringEncoding_latin1, ILU_StringEncoding_latin1, p_error);
			   if (ILU_ERRNOK(*p_error))
				   goto cleanup_label;
		   }
		   else { /* write out the object's sbh */
			   ilu_string pc_sbh =  ilu_SBHOfObject(p_object);
			   ilu_ExitServer(object_server(p_object), object_class(p_object)); 
			   if (pc_sbh == ILU_NIL) {
				   ILU_NOTE(HTTP_DEBUG,
					   ("_http_output_object_id:  can't form SBH for object %s/%s.\n",
					   server_id(object_server(p_object)), pc_object_ih));
				   ILU_ERR_CONS1(bad_param, p_error, minor, ilu_bpm_not_exported, NIL);
				   goto cleanup_label;
			   };
			   _http_output_string(p_call, (ilu_bytes) pc_sbh, strlen(pc_sbh), 0xFFFF,
				   ILU_StringEncoding_latin1, ILU_StringEncoding_latin1, p_error);
			   if (ILU_ERRNOK(*p_error))
				   goto cleanup_label;
		   }
		   
		   break;
		   
	   default:
		   HTTP_UNEXPECTED_STATE_ASSERT();
	}
	
cleanup_label:
	
	/* clean up if we were the discriminator of a redirected call */
	if (b_discriminator_p && _http_redirected_instance_handle(p_call)) {
		/* ensure any redirection members are freed */
		ilu_free(_http_redirected_instance_handle(p_call));
		_http_redirected_instance_handle(p_call) = NIL;
		ilu_free(_http_redirected_server_id(p_call));
		_http_redirected_server_id(p_call) = NIL;
		ilu_free(_http_redirected_sbh(p_call));
		_http_redirected_sbh(p_call) = NIL;
	}
	ilu_free(pc_object_ih);
	ilu_free(pc_server_id);
	
	
	if (ILU_ERRNOK(*p_error))
		return ilu_FALSE;
	
	return ilu_TRUE;
}


/* ********************************************************* */
/* input an object id                                        */

/* a mildly reworked version of _ilu_InputObjectID as it stood
at the time this was written */

static ilu_boolean _http_input_object_id(ilu_Call p_call, ilu_Object * p_object,
										 ilu_boolean b_discriminator_p,
										 ilu_Class p_class,
										 ILU_ERRS((IoErrs)) * p_error) {

	ilu_string      pc_server_id = NIL;
	ilu_string		pc_object_id = NIL;
	ilu_cardinal    card_server_id_length = 0;
	ilu_cardinal	card_object_id_length = 0, card_junk;
	ilu_Server      server = connection_server(call_connection(p_call));

	ILU_CLER(*p_error);

	*p_object = NIL;

	/* ensure valid params */
	if ((call_connection(p_call) == NIL) || (p_class == NIL))
		return ILU_ERR_CONS1(bad_param, p_error, minor, ilu_bpm_duh, ilu_FALSE);

	if (b_discriminator_p) {

		/* input the server id as a string */
		_http_input_string (p_call, (void **) &pc_server_id, &card_server_id_length, 0xFFFF,
				    ILU_StringEncoding_latin1, &card_junk, p_error);
		if (ILU_ERRNOK(*p_error))
			return ilu_FALSE;
	}

	/* input the object id */
	_http_input_string(p_call, (void **) &pc_object_id, &card_object_id_length, 0xFFFF,
			   ILU_StringEncoding_latin1, &card_junk, p_error);
	if (ILU_ERRNOK(*p_error)) {
		ilu_free(pc_server_id);
		return ilu_FALSE;
	}

	if (b_discriminator_p) {

		ilu_EnterServer(server, p_class);

		/* check to see if we're in the right server */
		if (strcmp(pc_server_id, server_id(server)) != 0) {
			(void) ILU_ERR_CONS1(marshal, p_error, minor, ilu_mm_alien_disc, 6);
			ilu_ExitServer(server, p_class);
		}

		/* check to see if we're in a closed server */
		else if (server_objs(server) == NIL) {
			(void) ILU_ERR_CONS1(inv_objref, p_error, minor, ilu_iom_svr_closed, 6);
			ilu_ExitServer(server, p_class);
		} 
		
		/* check to see that this object is in this server */
		else if ((*p_object = _ilu_FindObjectInServer(pc_object_id, server)) == NIL) {
			(void) ILU_ERR_CONS1(inv_objref, p_error, minor, ilu_iom_inst_nf, 6);
			ilu_ExitServer(server, p_class);
		} 
		
		/* check that the object is of the class */
		else if (!ilu_IsSubObjectType((*p_object)->ob_class, p_class)) {
			(void) ILU_ERR_CONS1(inv_objref, p_error, minor, ilu_iom_wrong_type, 6);
			*p_object = NIL;
			ilu_ExitServer(server, p_class);
		}

		/* cleanup */
		ilu_free(pc_server_id);
		if (ILU_ERRNOK(*p_error)) {
			ilu_free(pc_object_id);
			return ilu_FALSE;
		}
	}

	else { /* not the discriminator */
		if (strcmp(g_c_NILOBJ, pc_object_id) == 0) { /* really got nil obj */
			*p_object = NIL;
			ilu_free(pc_object_id);

			if (p_class->cl_optional) { /* if it's an optional, we're OK */
				return ilu_TRUE;
			} 
			else /* NIL when it shouldn't have been */
				return ILU_ERR_CONS1(NoObjectForSBH, p_error, sbh, pc_object_id, ilu_FALSE);
		} 
		
		else { /* we got something, get the object - note that ilu_ObjectOfSBH puts us in 
			      the objects server */
			*p_object = ilu_ObjectOfSBH(pc_object_id, p_class, p_error);
			if (ILU_ERRNOK(*p_error)) { /* couldn't find the object! */
				ilu_free(pc_object_id);
				return ilu_FALSE;
			}
		} 
	} /* end of not the discriminator */

	ilu_free(pc_object_id);
	return ilu_TRUE;
}


/* ********************************************************* */
/*  starting to send a sequence                              */

static void _http_output_sequence (ilu_Call p_call, ilu_cardinal card_length,
				   ilu_cardinal card_limit,
				   ilu_Type the_type,
				   ILU_ERRS((IoErrs)) * p_error) {

	ILU_CLER(*p_error);

	if ((card_limit > 0) && (card_length > card_limit)) {
		ILU_ERR_CONS1(bad_param, p_error, minor, ilu_bpm_sequenceLimit, 0);
		return;
    }

	switch (_http_get_call_state(p_call)) {

	case clnt2http_out_Header_sequence:

		_http_num_headers_to_process(p_call) = card_length;

		/* set to next state */
		if (card_length > 0)
			_http_set_call_state(p_call, clnt2http_out_Header_record);
		else 
			_http_set_call_state(p_call, clnt2http_out_end_Header_sequence);
		break;

	case srvr4http_out_Header_sequence:

		_http_num_headers_to_process(p_call) = card_length;

		/* set to next state */
		if (card_length > 0)
			_http_set_call_state(p_call, srvr4http_out_Header_record);
		else 
			_http_set_call_state(p_call, srvr4http_out_end_Header_sequence);

		break;

	case clnt2ilu_out_arguments:
	case srvr4ilu_out_return_values:
	case srvr4ilu_out_exception:

		/* have the transport write out the sequence length followed by cr lf */
		_http_write_cardinal_line(p_call, card_length, p_error);
		return ;

		/* stay in same state */

	case srvr4http_out_exception: 
		/* don't send back exception members to existing http clients */
		break;

	default:
		HTTP_UNEXPECTED_STATE_ASSERT();
	}

}


/* ********************************************************* */
/* inputs an sequence                                        */

static void _http_input_sequence (ilu_Call p_call, ilu_cardinal* pcard_length,
				  ilu_cardinal card_limit, ilu_Type the_type, 
				  ILU_ERRS((IoErrs)) * p_error) {
					
	ilu_cardinal card_size;

	ILU_CLER(*p_error);

	switch (_http_get_call_state(p_call)) {

	case clnt2http_in_Header_sequence:

		card_size = _http_list_size(_http_headers_list(p_call));

		if ((card_limit > 0) && (card_size > card_limit)) {
			ILU_ERR_CONS1(marshal, p_error, minor, ilu_mm_sequenceLimit, 0);
			return;
		}

		if ((*pcard_length = card_size) > 0) {
			_http_set_call_state(p_call, clnt2http_in_Header_record);
		}
		else 
			_http_set_call_state(p_call, clnt2http_in_end_Header_sequence);
		break;

	case srvr4http_in_Header_sequence:

		card_size = _http_list_size(_http_headers_list(p_call));

		if ((card_limit > 0) && (card_size > card_limit)) {
			ILU_ERR_CONS1(marshal, p_error, minor, ilu_mm_sequenceLimit, 0);
			return;
		}

		if ((*pcard_length = card_size) > 0) {
			_http_set_call_state(p_call, srvr4http_in_Header_record);
		}
		else 
			_http_set_call_state(p_call, srvr4http_in_end_Header_sequence);
		break;

	case clnt2ilu_in_return_values:
	case srvr4ilu_in_arguments:

		if (_http_read_cardinal_line(p_call, &card_size, p_error) == ilu_FALSE) 
			ILU_ERR_CONS1(marshal, p_error, minor, ilu_mm_badInteger, 0);

		if ((card_limit > 0) && (card_size > card_limit)) {
			ILU_ERR_CONS1(marshal, p_error, minor, ilu_mm_sequenceLimit, 0);
			return;
		}

		*pcard_length = card_size;

		break;

	default:
		HTTP_UNEXPECTED_STATE_ASSERT();

	}
}


/* ********************************************************* */
/*  sending an optional                                      */

static void _http_output_optional (ilu_Call p_call, ilu_boolean b_falseifnil,
				   ilu_Type the_type,
				   ILU_ERRS((IoErrs)) * p_error) {

	char c_buffer[32];

	ILU_CLER(*p_error);

	switch (_http_get_call_state(p_call)) {

	case clnt2http_out_Header_Value_present:

		/* set to next state */
		if (b_falseifnil)
			_http_set_call_state(p_call, clnt2http_out_Header_value);
		else 
			_http_set_call_state(p_call, clnt2http_out_end_Header_record);

		break;

	case clnt2http_out_Body_present:

		/* set to next state */
		if (b_falseifnil)
			_http_set_call_state(p_call, clnt2http_out_Body);

		else {

			/* write out trailing cr lf - see state clnt2http_out_end_Header_sequence
			for explantion of why we're doing this here */
			_http_transport_write_bytes(http_call_tport(p_call), g_c_CRLF, 2, p_error);

			_http_set_call_state(p_call, clnt2http_out_end_Request_record);
		}

		break;

	case srvr4http_out_Header_Value_present:

		/* set to next state */
		if (b_falseifnil)
			_http_set_call_state(p_call, srvr4http_out_Header_value);
		else 
			_http_set_call_state(p_call, srvr4http_out_end_Header_record);
		break;


	case srvr4http_out_Body_present:

		/* set to next state */
		if (b_falseifnil)
			_http_set_call_state(p_call, srvr4http_out_Body);

		else {

			/* write out trailing cr lf - see state srvr4http_out_end_Header_sequence
			for explantion of why we're doing this here */
			_http_transport_write_bytes(http_call_tport(p_call), g_c_CRLF, 2, p_error);

			_http_set_call_state(p_call, srvr4http_out_end_Response_record);
		}

		break;

	case clnt2ilu_out_arguments:
	case srvr4ilu_out_return_values:
	case srvr4ilu_out_exception:

		/* have the transport write out a presence indication */
		if (b_falseifnil)
			sprintf(c_buffer, "%s\r\n", g_c_OPTIONAL_PRESENT);
		else
			sprintf(c_buffer, "%s\r\n", g_c_OPTIONAL_NOT_PRESENT);

		_http_transport_write_bytes(http_call_tport(p_call), c_buffer, strlen(c_buffer), p_error);
		if (ILU_ERRNOK(*p_error))
			return ;

		/* stay in same state */
		break;

	case srvr4http_out_exception: 
		/* don't send back exception members to existing http clients */
		break;

	default:
		HTTP_UNEXPECTED_STATE_ASSERT();
	}
}


/* ********************************************************* */
/* inputs an optional                                        */

static void _http_input_optional (ilu_Call p_call, ilu_boolean * p_b_present,
				  ilu_Type the_type, ILU_ERRS((IoErrs)) * p_error) {
					
	char* pc_line;

	ILU_CLER(*p_error);

	switch (_http_get_call_state(p_call)) {

	case clnt2http_in_Header_Value_present:

		/* if our into contents is non nil (as established during fill_headers_list) then
		   there's something there */
		*p_b_present = _http_list_node_into_contents(_http_headers_current_node(p_call)) ? 
						ilu_TRUE : ilu_FALSE;
		
		/* set to next state */
		if (*p_b_present)
			_http_set_call_state(p_call, clnt2http_in_Header_value);
		else
			_http_set_call_state(p_call, clnt2http_in_end_Header_record);
		break;

	case clnt2http_in_Body_present:

		if (_http_is_HEAD_method(p_call))  /* head method responses never have bodies */
			*p_b_present = ilu_FALSE;

		else { /* not a HEAD method */

			if (!_http_keep_alive(p_call)) 
			/* http1.0 (non persistent) or 1.1 with Connection: close doesn't require a content 
			    length field (end denoted by connection close) - so we'll just say it's there 
				and read till eof */
				*p_b_present = ilu_TRUE;

			else /* we're persistent, so we can't possibly indicate end of response by connection close,
				 so a length header must have been supplied */ 
				 *p_b_present = (_http_body_length(p_call) > 0) ? ilu_TRUE : ilu_FALSE;
		}


		/* set to next state */
		if (*p_b_present)
			_http_set_call_state(p_call, clnt2http_in_Body);
		else
			_http_set_call_state(p_call, clnt2http_in_end_Response_record);
		break;

	case srvr4http_in_Header_Value_present:

		/* if our into contents is non nil (as established during fill_headers_list) then
		   there's something there */
		*p_b_present = _http_list_node_into_contents(_http_headers_current_node(p_call)) ? 
						ilu_TRUE : ilu_FALSE;
		
		/* set to next state */
		if (*p_b_present)
			_http_set_call_state(p_call, srvr4http_in_Header_value);
		else
			_http_set_call_state(p_call, srvr4http_in_end_Header_record);
		break;

	case srvr4http_in_Body_present:

		/* if there was a content length header, we know there's a body */
		*p_b_present = (_http_body_length(p_call) > 0) ? ilu_TRUE : ilu_FALSE;

		/* set to next state */
		if (*p_b_present)
			_http_set_call_state(p_call, srvr4http_in_Body);
		else
			_http_set_call_state(p_call, srvr4http_in_end_Request_record);
		break;


	case srvr4ilu_in_arguments:
	case clnt2ilu_in_return_values:

		/* read in the line containing the presence indication */
		if ((pc_line = _http_readline(p_call, NIL, p_error)) == NIL)
			return;

		*p_b_present = (strcmp(pc_line, g_c_OPTIONAL_PRESENT) == 0);
		ilu_free(pc_line);

		/* stay in same state */
		break;

	default:
		HTTP_UNEXPECTED_STATE_ASSERT();
	}
}


/* ********************************************************* */
/*  sending a bunch of bytes                                 */

static void _http_output_bytes(ilu_Call p_call, ilu_bytes p_the_bytes,
				   ilu_cardinal card_length,
				   ilu_cardinal card_limit,
				   ILU_ERRS((IoErrs)) * p_error) {


	char c_buffer[64];

	ILU_CLER(*p_error);

	if ((card_limit > 0) && (card_length > card_limit)) {
		ILU_ERR_CONS1(bad_param, p_error, minor, ilu_bpm_sequenceLimit, 0);
		return;
	}

	switch (_http_get_call_state(p_call)) {

	case clnt2http_out_Body:

		/* write out Content-length header and trailing cr lf - see state 
		clnt2http_out_end_Header_sequence for explantion of why we're 
		doing this here */

		if (_http_user_supplied_content_length(p_call)) {
			/* user already supplied a content-length header, so don't
			auto generate, just put out trailing crlf to indicate end of headers */
			_http_transport_write_bytes(http_call_tport(p_call), g_c_CRLF, 2, p_error);
			if (ILU_ERRNOK(*p_error))
				return;
		}
		else {
			/* auto generate a content length header */
			sprintf(c_buffer, "Content-Length: %lu\r\n\r\n", (long unsigned int) card_length);

			_http_transport_write_bytes(http_call_tport(p_call), c_buffer, strlen(c_buffer), p_error);
			if (ILU_ERRNOK(*p_error))
				return;
		}

		/* write out the body bytes */
		_http_transport_write_bytes(http_call_tport(p_call), p_the_bytes, card_length, p_error);
		if (ILU_ERRNOK(*p_error))
			return;

		/* set to next state */
		_http_set_call_state(p_call, clnt2http_out_end_Request_record);

		break;

	case srvr4http_out_Body:
		
		/* write out Content-length header and trailing cr lf - see state 
		srvr4http_out_end_Header_sequence for explanation of why we're 
		doing this here */

		if (_http_user_supplied_content_length(p_call)) {
			/* user already supplied a content-length header, so don't
			auto generate, just put out trailing crlf to indicate end of headers */
			_http_transport_write_bytes(http_call_tport(p_call), g_c_CRLF, 2, p_error);
			if (ILU_ERRNOK(*p_error))
				return;
		}
		else {
			/* auto generate a content length header */
			sprintf(c_buffer, "Content-Length: %lu\r\n\r\n", (long unsigned int) card_length);

			_http_transport_write_bytes(http_call_tport(p_call), c_buffer, strlen(c_buffer), p_error);
			if (ILU_ERRNOK(*p_error))
				return;
		}

		/* write out the body bytes */
		_http_transport_write_bytes(http_call_tport(p_call), p_the_bytes, card_length, p_error);
		if (ILU_ERRNOK(*p_error))
			return;

		/* set to next state */
		_http_set_call_state(p_call, srvr4http_out_end_Response_record);

		break;

	case clnt2ilu_out_arguments:
	case srvr4ilu_out_return_values:
	case srvr4ilu_out_exception:

		/* write out count of bytes */
		if (_http_write_cardinal_line(p_call, card_length, p_error) == ilu_FALSE)
			return;

		/* write out the bytes themselves, and add a crlf */
		_http_transport_write_bytes(http_call_tport(p_call), p_the_bytes, card_length, p_error);
		if (ILU_ERRNOK(*p_error))
			return;

		_http_transport_write_bytes(http_call_tport(p_call), g_c_CRLF, 2, p_error);
		if (ILU_ERRNOK(*p_error))
			return ;

		break;

	case srvr4http_out_exception: 
		/* don't send back exception members to existing http clients */
		break;

	default:
		HTTP_UNEXPECTED_STATE_ASSERT();
	}
}


/* ********************************************************* */
/* inputs bytes                                              */

static void _http_input_bytes (ilu_Call p_call, ilu_bytes* ppc_bytes,
				ilu_cardinal* p_card_length,
				ilu_cardinal card_limit,
				ILU_ERRS((IoErrs)) * p_error) {

	char c_junk[8];
	ilu_cardinal card_size;
	ilu_boolean b_did_malloc_bytes = ilu_FALSE;

	ILU_CLER(*p_error);

	switch (_http_get_call_state(p_call)) {

	case clnt2http_in_Body:

	  card_size = _http_body_length(p_call);

	  if (card_limit > 0 && card_size > card_limit) {
	    ILU_ERR_CONS1(marshal, p_error, minor, ilu_mm_sequenceLimit, 0);
	    return;
	  }

	  *p_card_length = card_size;


	  if (_http_keep_alive(p_call)) {
	  /* if we got a keep alive from the server, we know that they won't 
	  be relying on closing the connection to signal the end of the body, so 
		  they must have sent a content length header */
		  
		if (card_size == 0) {
		/* xxx must mod something here if eventually support chunked or mime transfers of version 1.1 */
			ILU_ERR_CONS1(comm_failure, p_error, minor, ilu_cfm_protocol_sync_lost, NIL);
			return;
		}

	    if (*ppc_bytes == NIL) {
	      *ppc_bytes = (ilu_bytes) ilu_MallocE(*p_card_length, p_error);
	      b_did_malloc_bytes = ilu_TRUE;
	    }
	    if (*ppc_bytes == NIL) {
	      *p_card_length = 0;
	      return;
	    }
	    _http_transport_read_bytes(http_call_tport(p_call), *ppc_bytes, *p_card_length, p_error);
	    if (ILU_ERRNOK(*p_error)) {
	      *p_card_length = 0;
	      if (b_did_malloc_bytes) {
		ilu_free(*ppc_bytes);
		*ppc_bytes = NIL;
	      }
	      return;
	    }
	  }
	  else {
	    /* since http1.0 responses are not required to have a content length header
	       we'll read till eof read till end of file (i.e. connection closes) */
	    if (_http_fill_buffer_till (http_call_tport(p_call), ppc_bytes, HTTP_BODY_ALLOC_SIZE,
					"", NIL, 0, NIL, NIL, ilu_TRUE, p_card_length, p_error) == ilu_FALSE) {
	      *p_card_length = 0;
	      if (b_did_malloc_bytes) {
		ilu_free(*ppc_bytes);
		*ppc_bytes = NIL;
	      }
	      return;
	    } 
	  }

	  /* set to next state */
	  _http_set_call_state(p_call, clnt2http_in_end_Response_record);

	  break;

	case srvr4http_in_Body:

		card_size = _http_body_length(p_call);

		if (card_size == 0) {
		/* xxx must mod something here if eventually support chunked or mime transfers of version 1.1 */
			ILU_ERR_CONS1(comm_failure, p_error, minor, ilu_cfm_protocol_sync_lost, NIL);
			return;
		}

		if (card_limit > 0 && card_size > card_limit) {
			ILU_ERR_CONS1(marshal, p_error, minor, ilu_mm_sequenceLimit, 0);
			return;
		}

		*p_card_length = card_size;

		/* get right amount of space */
		if (*ppc_bytes == NIL) {
		  *ppc_bytes = (ilu_bytes) ilu_MallocE(*p_card_length, p_error);
		  b_did_malloc_bytes = ilu_TRUE;
		}
		if (*ppc_bytes == NIL) {
			*p_card_length = 0;
			return;
		}

		_http_transport_read_bytes(http_call_tport(p_call), *ppc_bytes, *p_card_length, p_error);
		if (ILU_ERRNOK(*p_error)) {
			*p_card_length = 0;
			if (b_did_malloc_bytes) {
			  ilu_free(*ppc_bytes);
			  *ppc_bytes = NIL;
			}
			return;
		}

		/* set to next state */
		_http_set_call_state(p_call, srvr4http_in_end_Request_record);

		break;

	case srvr4ilu_in_arguments:
	case clnt2ilu_in_return_values:

		/* get count of bytes */
		if (_http_read_cardinal_line(p_call, &card_size, p_error) == ilu_FALSE) {
			ILU_ERR_CONS1(marshal, p_error, minor, ilu_mm_badInteger, 0);
			*p_card_length = 0;
			return;
		}

		if (card_limit > 0 && card_size > card_limit) {
			ILU_ERR_CONS1(marshal, p_error, minor, ilu_mm_sequenceLimit, 0);
			return;
		}

		*p_card_length = card_size;

		/* get right amount of space */
		if (*ppc_bytes == NIL) {
		  *ppc_bytes = (ilu_bytes) ilu_MallocE(*p_card_length, p_error);
		  b_did_malloc_bytes = ilu_TRUE;
		}
		if (*ppc_bytes == NIL) {
			*p_card_length = 0;
			return;
		}

		/* read in the bytes and our trailing crlf */
		_http_transport_read_bytes(http_call_tport(p_call), *ppc_bytes, *p_card_length, p_error);
		
		if (ILU_ERRNOK(*p_error)) {
			if (b_did_malloc_bytes) {
			  ilu_free(*ppc_bytes);
			  *ppc_bytes = NIL;
			}
			*p_card_length = 0;
			return;
		}

		_http_transport_read_bytes(http_call_tport(p_call), c_junk, 2, p_error);
		if (ILU_ERRNOK(*p_error)) {
			if (b_did_malloc_bytes) {
			  ilu_free(*ppc_bytes);
			  *ppc_bytes = NIL;
			}
			*p_card_length = 0;
			return;
		}

		return;

	default:
		HTTP_UNEXPECTED_STATE_ASSERT();
	}
}


/* ********************************************************* */
/* outputs a readable representation of the bytes            */

static void _http_output_readable_bytes(ilu_Call p_call, ilu_bytes p_the_bytes,
				   ilu_cardinal card_length,
				   ilu_cardinal card_limit,
				   ILU_ERRS((IoErrs)) * p_error) {

	ilu_cardinal card_index;
	ilu_integer i_temp;
	ilu_bytes pc_source = p_the_bytes;
	char c_buffer [16];

	ILU_CLER(*p_error);

	if ((card_limit > 0) && (card_length > card_limit)) {
		ILU_ERR_CONS1(bad_param, p_error, minor, ilu_bpm_sequenceLimit, 0);
		return;
	}

	/* write out count of bytes */
	if (_http_write_cardinal_line(p_call, card_length, p_error) == ilu_FALSE)
		return;

	/* write out a readable representation */
	for (card_index = 0; card_index < card_length; card_index++) {

		i_temp = (ilu_integer)(*pc_source);
		sprintf(c_buffer, "%x", i_temp);
		_http_transport_write_bytes(http_call_tport(p_call), c_buffer, strlen(c_buffer), p_error);
		if (ILU_ERRNOK(*p_error))
			return;
		pc_source++;
	}

	/* write out trailing cr lf */
	_http_transport_write_bytes(http_call_tport(p_call), g_c_CRLF, 2, p_error);
}


/* ********************************************************* */
/* inputs a readable representation of the bytes             */

static void _http_input_readable_bytes (ilu_Call p_call, ilu_bytes* ppc_bytes,
				ilu_cardinal* p_card_length,
				ilu_cardinal card_limit,
				ILU_ERRS((IoErrs)) * p_error) {

	ilu_cardinal card_index;
	ilu_cardinal card_size;
	ilu_integer i_temp;
	ilu_bytes pc_dest;
	char c_buffer [16];

	ILU_CLER(*p_error);

	/* get count of bytes */
	if (_http_read_cardinal_line(p_call, &card_size, p_error) == ilu_FALSE) {
		ILU_ERR_CONS1(marshal, p_error, minor, ilu_mm_badInteger, 0);
		*p_card_length = 0;
		*ppc_bytes = NIL;
		return;
	}

	if (card_limit > 0 && card_size > card_limit) {
		ILU_ERR_CONS1(marshal, p_error, minor, ilu_mm_sequenceLimit, 0);
		return;
	}

	*p_card_length = card_size;

	/* get right amount of space */
	*ppc_bytes = (ilu_bytes) ilu_MallocE(*p_card_length, p_error);
	if (*ppc_bytes == NIL) {
		*p_card_length = 0;
		return;
	}

	/* read in the readable bytes */
	pc_dest = *ppc_bytes;
	c_buffer[2] = '\0';
	for (card_index = 0; card_index < *p_card_length; card_index++) {

		_http_transport_read_bytes(http_call_tport(p_call), c_buffer, 2, p_error);

		if (ILU_ERRNOK(*p_error) || (sscanf(c_buffer, "%x", &i_temp) != 1)) {
			ilu_free(*ppc_bytes);
			*p_card_length = 0;
			*ppc_bytes = NIL;
			return;
		}

		*pc_dest = (ilu_byte)i_temp;
		pc_dest++;
	}
	
	/* read in the trailing crlf */
	_http_transport_read_bytes(http_call_tport(p_call), c_buffer, 2, p_error);
	if (ILU_ERRNOK(*p_error)) {
		ilu_free(*ppc_bytes);
		*p_card_length = 0;
		*ppc_bytes = NIL;
		return;
	}
}


/* ********************************************************* */
/* output a cardinal                                         */

static void _http_output_cardinal (ilu_Call p_call, ilu_cardinal card,
			                 ILU_ERRS((IoErrs)) * p_error) {

	if (_http_get_call_state(p_call) == srvr4http_out_exception) {
		ILU_CLER(*p_error);
		/* don't send back exception members to existing http clients */
		return;
	}

	_http_write_cardinal_line(p_call, card, p_error);
}


/* ********************************************************* */
/* input a cardinal                                          */

static void _http_input_cardinal (ilu_Call p_call, ilu_cardinal * p_card,
			                 ILU_ERRS((IoErrs)) * p_error) {

	_http_read_cardinal_line(p_call, p_card, p_error);
}


/* ********************************************************* */
/* output a short cardinal                                   */

static void _http_output_shortcardinal (ilu_Call p_call, ilu_shortcardinal scard,
			                 ILU_ERRS((IoErrs)) * p_error) {

	ilu_cardinal card_temp = scard;

	if (_http_get_call_state(p_call) == srvr4http_out_exception) {
		ILU_CLER(*p_error);
		/* don't send back exception members to existing http clients */
		return;
	}

	_http_write_cardinal_line(p_call, card_temp, p_error);
}


/* ********************************************************* */
/* input a short cardinal                                    */

static void _http_input_shortcardinal (ilu_Call p_call, ilu_shortcardinal * p_scard,
			                 ILU_ERRS((IoErrs)) * p_error) {
	ilu_cardinal card_temp;

	_http_read_cardinal_line(p_call, &card_temp, p_error);

	*p_scard = (ilu_shortcardinal)card_temp;
}


/* ********************************************************* */
/* output a integer                                         */

static void _http_output_integer (ilu_Call p_call, ilu_integer card,
			                 ILU_ERRS((IoErrs)) * p_error) {

	if (_http_get_call_state(p_call) == srvr4http_out_exception) {
		ILU_CLER(*p_error);
		/* don't send back exception members to existing http clients */
		return;
	}

	_http_write_integer_line(p_call, card, p_error);
}


/* ********************************************************* */
/* input a integer                                          */

static void _http_input_integer (ilu_Call p_call, ilu_integer * p_card,
			                 ILU_ERRS((IoErrs)) * p_error) {

	_http_read_integer_line(p_call, p_card, p_error);
}


/* ********************************************************* */
/* output a short integer                                   */

static void _http_output_shortinteger (ilu_Call p_call, ilu_shortinteger scard,
			                 ILU_ERRS((IoErrs)) * p_error) {

	ilu_integer card_temp = scard;

	if (_http_get_call_state(p_call) == srvr4http_out_exception) {
		ILU_CLER(*p_error);
		/* don't send back exception members to existing http clients */
		return;
	}

	_http_write_integer_line(p_call, card_temp, p_error);
}


/* ********************************************************* */
/* input a short integer                                    */

static void _http_input_shortinteger (ilu_Call p_call, ilu_shortinteger * p_scard,
			                 ILU_ERRS((IoErrs)) * p_error) {
	ilu_integer card_temp;

	_http_read_integer_line(p_call, &card_temp, p_error);

	*p_scard = (ilu_shortinteger)card_temp;
}


/* ********************************************************* */
/* output a byte                                         */

static void _http_output_byte (ilu_Call p_call, ilu_byte abyte,
			                 ILU_ERRS((IoErrs)) * p_error) {

	char c_buffer [32];
	ilu_integer i_temp = (ilu_integer)abyte;

	ILU_CLER(*p_error);

	if (_http_get_call_state(p_call) == srvr4http_out_exception) 
		/* don't send back exception members to existing http clients */
		return;

	/* write out the byte value */
	sprintf(c_buffer, "0x%x\r\n", i_temp);
	_http_transport_write_bytes(http_call_tport(p_call), c_buffer, strlen(c_buffer), p_error);
}


/* ********************************************************* */
/* input a byte                                          */

static void _http_input_byte (ilu_Call p_call, ilu_byte * p_abyte,
			                 ILU_ERRS((IoErrs)) * p_error) {

	ilu_string pc_buffer;
	ilu_integer i_temp;

	ILU_CLER(*p_error);

	if ((pc_buffer = _http_readline(p_call, NIL, p_error)) == NIL)
		return ;

	/* readin the integer value */
	if (sscanf(pc_buffer, "0x%x\r\n", &i_temp) != 1) {
		ILU_ERR_CONS1(marshal, p_error, minor, ilu_mm_badInteger, 0);
		ilu_free(pc_buffer);
		return ;
	}

	*p_abyte = (ilu_byte)i_temp;

	ilu_free(pc_buffer);
}


/* ********************************************************* */
/* output a shortcharacter                                   */

static void _http_output_shortchar (ilu_Call p_call, ilu_shortcharacter schar,
			                 ILU_ERRS((IoErrs)) * p_error) {

	if (_http_get_call_state(p_call) == srvr4http_out_exception) {
		ILU_CLER(*p_error);
		/* don't send back exception members to existing http clients */
		return;
	}

	_http_output_byte(p_call, (ilu_byte)schar, p_error);
}


/* ********************************************************* */
/* input a shortcharacter                                    */

static void _http_input_shortchar (ilu_Call p_call, ilu_shortcharacter * p_schar,
			                 ILU_ERRS((IoErrs)) * p_error) {

	_http_input_byte(p_call, (ilu_byte*)p_schar, p_error);
}


/* ********************************************************* */
/* output a boolean                                         */

static void _http_output_boolean (ilu_Call p_call, ilu_boolean aboolean,
			                 ILU_ERRS((IoErrs)) * p_error) {

	char c_buffer [32];

	ILU_CLER(*p_error);

	if (_http_get_call_state(p_call) == srvr4http_out_exception) 
		/* don't send back exception members to existing http clients */
		return;

	/* write out the boolean value */
	sprintf(c_buffer, "%s\r\n", aboolean ? "ilu_TRUE" : "ilu_FALSE");
	_http_transport_write_bytes(http_call_tport(p_call), c_buffer, strlen(c_buffer), p_error);
}


/* ********************************************************* */
/* input a boolean                                          */

static void _http_input_boolean (ilu_Call p_call, ilu_boolean * p_boolean,
			                 ILU_ERRS((IoErrs)) * p_error) {

	ilu_string pc_buffer;

	ILU_CLER(*p_error);

	if ((pc_buffer = _http_readline(p_call, NIL, p_error)) == NIL)
		return ;

	if (strcmp(pc_buffer, "ilu_TRUE") == 0)
		*p_boolean = ilu_TRUE;
	else
		*p_boolean = ilu_FALSE;

	ilu_free(pc_buffer);
}


/* ********************************************************* */
/* output opaque                                             */

static void _http_output_opaque (ilu_Call p_call, ilu_opaque an_opaque,
				  ilu_cardinal card_length,
				  ILU_ERRS((IoErrs)) * p_error) {

	if (_http_get_call_state(p_call) == srvr4http_out_exception) {
		ILU_CLER(*p_error);
		/* don't send back exception members to existing http clients */
		return;
	}

	_http_output_bytes(p_call, (ilu_bytes)an_opaque, 
		card_length, card_length, p_error);
}


/* ********************************************************* */
/* input opaque                                              */

static void _http_input_opaque (ilu_Call p_call, ilu_opaque * p_an_opaque,
				 ilu_cardinal card_length,
				 ILU_ERRS((IoErrs)) * p_error) {

	ilu_cardinal card_num_read;

	_http_input_bytes (p_call, (ilu_bytes*)p_an_opaque,
				   &card_num_read, card_length, p_error);

	if (card_num_read != card_length)
		ILU_ERR_CONS1(bad_param, p_error, minor, ilu_bpm_duh, ilu_FALSE);
}


/* ********************************************************* */
/* output union                                              */

 static void _http_output_union (ilu_Call p_call,
				 ilu_cardinal card_discrim,
				 ilu_TypeKind discriminator_kind,
				 ilu_Type the_type,
				 ILU_ERRS((IoErrs)) * p_error) {

	if (_http_get_call_state(p_call) == srvr4http_out_exception) {
		ILU_CLER(*p_error);
		/* don't send back exception members to existing http clients */
		return;
	}

	 _http_output_cardinal(p_call, card_discrim, p_error);
 }


/* ********************************************************* */
/* input union                                               */

 static void _http_input_union (ilu_Call p_call,
				ilu_cardinal * p_card_typeIndex,
				ilu_TypeKind discriminator_kind,
				ilu_Type the_type,
				ILU_ERRS((IoErrs)) * p_error) {


	 _http_input_cardinal(p_call, p_card_typeIndex, p_error);
}


/* ********************************************************* */
/* end union                                                 */

 static void _http_end_union (ilu_Call p_call, 
							  ILU_ERRS((IoErrs)) * p_error) {

  ILU_CLER(*p_error); /* nothing to do */
}


/* ********************************************************* */
/* output, input, end array - really do nothing              */

static void _http_output_array(ilu_Call p_call, 
			       ilu_cardinal card_len,
			       ilu_Type the_type,
			       ILU_ERRS((IoErrs)) * p_error) {
  ILU_CLER(*p_error);
}

static void _http_input_array(ilu_Call p_call,
			      ilu_Type the_type,
			      ILU_ERRS((IoErrs)) * p_error) {
  ILU_CLER(*p_error);
}

static void _http_end_array (ilu_Call p_call,
			     ILU_ERRS((IoErrs)) * p_error) {
  ILU_CLER(*p_error);
}

/* ********************************************************* */
/* output, input, sequence mark - really do nothing          */

static void _http_output_sequence_mark (ilu_Call p_call,
										ilu_cardinal card_extent,
										ILU_ERRS((IoErrs)) * p_error) {
  ILU_CLER(*p_error);
}

static void _http_input_sequence_mark(ilu_Call p_call, 
									  ilu_cardinal card_extent,
									  ILU_ERRS((IoErrs)) * p_error) {
  ILU_CLER(*p_error);
}


/* ********************************************************* */
/* output stringvec                                          */

static void _http_output_stringvec (ilu_Call p_call, ilu_string astring,
									ilu_cardinal card_length,
									ILU_ERRS((IoErrs)) * p_error) {

	if (_http_get_call_state(p_call) == srvr4http_out_exception) {
		ILU_CLER(*p_error);
		/* don't send back exception members to existing http clients */
		return;
	}

	_http_output_opaque (p_call, (ilu_opaque)astring, card_length, p_error);
 }


/* ********************************************************* */
/* input stringvec                                           */

static void _http_input_stringvec (ilu_Call p_call, ilu_string * p_astring,
								   ilu_cardinal card_length,
								   ILU_ERRS((IoErrs)) * p_error) {

	_http_input_opaque (p_call, (ilu_opaque*)p_astring, card_length, p_error);
 }


/* ********************************************************* */
/* output longcardinal                                       */

static void _http_output_longcardinal (ilu_Call p_call, ilu_longcardinal longcard_i,
			   ILU_ERRS((IoErrs)) * p_error) {

	if (_http_get_call_state(p_call) == srvr4http_out_exception) {
		ILU_CLER(*p_error);
		/* don't send back exception members to existing http clients */
		return;
	}

	_http_output_cardinal(p_call, ILU_LONGCARD_HIGH_WORD(&longcard_i), p_error);

  if (ILU_ERROK(*p_error))
	_http_output_cardinal(p_call, ILU_LONGCARD_LOW_WORD(&longcard_i), p_error);
}


/* ********************************************************* */
/* input longcardinal                                        */

static void _http_input_longcardinal(ilu_Call p_call,
									 ilu_longcardinal * p_longcard_i,
									 ILU_ERRS((IoErrs)) * p_error) {

	_http_input_cardinal (p_call, &ILU_LONGCARD_HIGH_WORD(p_longcard_i), p_error);

	if (ILU_ERROK(*p_error))
		_http_input_cardinal (p_call, &ILU_LONGCARD_LOW_WORD(p_longcard_i), p_error);
}


/* ********************************************************* */
/* output longinteger                                       */

static void _http_output_longinteger (ilu_Call p_call, ilu_longinteger longint_i,
			   ILU_ERRS((IoErrs)) * p_error) {

	if (_http_get_call_state(p_call) == srvr4http_out_exception) {
		ILU_CLER(*p_error);
		/* don't send back exception members to existing http clients */
		return;
	}

	_http_output_integer(p_call, ILU_LONGINT_HIGH_WORD(&longint_i), p_error);

  if (ILU_ERROK(*p_error))
	_http_output_integer(p_call, ILU_LONGINT_LOW_WORD(&longint_i), p_error);
}


/* ********************************************************* */
/* input longinteger                                        */

static void _http_input_longinteger(ilu_Call p_call,
									 ilu_longinteger * p_longint_i,
									 ILU_ERRS((IoErrs)) * p_error) {

	_http_input_integer (p_call, &ILU_LONGINT_HIGH_WORD(p_longint_i), p_error);

	if (ILU_ERROK(*p_error))
		_http_input_integer (p_call, (int*)(&ILU_LONGINT_LOW_WORD(p_longint_i)), p_error);
}



/* ********************************************************* */
/* output real                                       */

static void _http_output_real (ilu_Call p_call, 
							   ilu_real real_i,
							   ILU_ERRS((IoErrs)) * p_error) {

	char c_buffer[128];

	ILU_CLER(*p_error);

	if (_http_get_call_state(p_call) == srvr4http_out_exception) 
		/* don't send back exception members to existing http clients */
		return;

	sprintf(c_buffer, "%.64g\r\n", real_i); 
	_http_transport_write_bytes(http_call_tport(p_call), c_buffer, strlen(c_buffer), p_error);
}


/* ********************************************************* */
/* input real                                        */

static void _http_input_real(ilu_Call p_call, 
							 ilu_real * p_real_i, 
							 ILU_ERRS((IoErrs)) * p_error) {

	ilu_string pc_buffer;

	ILU_CLER(*p_error);

	if ((pc_buffer = _http_readline(p_call, NIL, p_error)) == NIL)
		return;

	/* readin the real value */
	if (sscanf(pc_buffer, "%lg", p_real_i) != 1) {
		ilu_free(pc_buffer);
		return;
	}

	ilu_free(pc_buffer);
}


/* ********************************************************* */
/* output longreal                                       */

static void _http_output_longreal (ilu_Call p_call, ilu_longreal longreal_i,
			   ILU_ERRS((IoErrs)) * p_error) {

	if (_http_get_call_state(p_call) == srvr4http_out_exception) {
		ILU_CLER(*p_error);
		/* don't send back exception members to existing http clients */
		return;
	}

	_http_output_readable_bytes(p_call, (ilu_bytes)&longreal_i, sizeof(ilu_longreal),
		sizeof(ilu_longreal), p_error);
}


/* ********************************************************* */
/* input longreal                                            */

static void _http_input_longreal(ilu_Call p_call,
									 ilu_longreal * p_longreal_i,
									 ILU_ERRS((IoErrs)) * p_error) {

	ilu_cardinal card_length;
	ilu_bytes pc_buffer;

	_http_input_readable_bytes(p_call, &pc_buffer, &card_length, sizeof(ilu_longreal), p_error);

	if (ILU_ERROK(*p_error))
		memcpy((void *) p_longreal_i, (void *) pc_buffer, sizeof(ilu_longreal));

	ilu_free(pc_buffer);
}


/* ********************************************************* */
/* output shortreal                                       */

static void _http_output_shortreal (ilu_Call p_call, ilu_shortreal shortreal_i,
			   ILU_ERRS((IoErrs)) * p_error) {

	if (_http_get_call_state(p_call) == srvr4http_out_exception) {
		ILU_CLER(*p_error);
		/* don't send back exception members to existing http clients */
		return;
	}

	_http_output_real (p_call, (ilu_real) shortreal_i,  p_error); 

}


/* ********************************************************* */
/* input shortreal                                            */

static void _http_input_shortreal(ilu_Call p_call,
									 ilu_shortreal * p_shortreal_i,
									 ILU_ERRS((IoErrs)) * p_error) {

	ilu_real real_temp;

	_http_input_real(p_call, &real_temp, p_error); 

	*p_shortreal_i = (ilu_shortreal)real_temp;
}


/* ********************************************************* */
/* output character                                          */

static void _http_output_character (ilu_Call p_call, 
									ilu_character char_i, 
									ILU_ERRS((IoErrs)) * p_error) {

	if (_http_get_call_state(p_call) == srvr4http_out_exception) {
		ILU_CLER(*p_error);
		/* don't send back exception members to existing http clients */
		return;
	}

	_http_output_shortcardinal(p_call, (ilu_shortcardinal) char_i, p_error);
}


/* ********************************************************* */
/* input character                                           */

static void _http_input_character (ilu_Call p_call, 
								   ilu_character * p_char_i, 
								   ILU_ERRS((IoErrs)) * p_error) {

	ilu_shortcardinal scard_temp;

	_http_input_shortcardinal(p_call, &scard_temp, p_error);

	*p_char_i = (ilu_character)scard_temp;
}



/* ********************************************************* */
/* output wide string                                        */

static void _http_output_wstring (ilu_Call p_call, 
								  ilu_wstring astring, 
								  ilu_cardinal card_length, 
								  ilu_cardinal card_limit, 
								  ILU_ERRS((IoErrs)) * p_error) {
	ilu_cardinal card_index;
	ilu_shortcardinal* p_scard;

	ILU_CLER(*p_error);

	if (card_limit > 0 && card_length > card_limit) {
		ILU_ERR_CONS1(bad_param, p_error, minor, ilu_bpm_sequenceLimit, 0);
		return;
	}

	if (_http_get_call_state(p_call) == srvr4http_out_exception) {
		/* don't send back exception members to existing http clients */
		return;
	}

	/* write out the length */
	_http_output_cardinal(p_call, card_length, p_error);
	if (ILU_ERRNOK(*p_error))
		return;

	/* put out all the wide chars */
	p_scard = (ilu_shortcardinal*)astring;
	for (card_index = 0; card_index < card_length; card_index++) {
		_http_output_shortcardinal(p_call, *p_scard, p_error);
		if (ILU_ERRNOK(*p_error))
			return;
		p_scard++;
	}
}


/* ********************************************************* */
/* input wide string                                         */

static void _http_input_wstring (ilu_Call p_call, 
								 ilu_wstring * p_string, 
								 ilu_cardinal * p_card_length, 
								 ilu_cardinal card_limit, 
								 ILU_ERRS((IoErrs)) * p_error) {
	ilu_cardinal card_index;
	ilu_cardinal card_size;
	ilu_shortcardinal* p_scard;

	/* read in the length */
	_http_input_cardinal(p_call, &card_size, p_error);

	if (card_limit > 0 && card_size > card_limit) {
		ILU_ERR_CONS1(marshal, p_error, minor, ilu_mm_sequenceLimit, 0);
		return;
	}

	*p_card_length = card_size;

	/* get right amount of space */
	*p_string = (ilu_wstring) ilu_MallocE((((*p_card_length) + 1) * sizeof(ilu_character)),
											p_error);
	if (*p_string == NIL) {
		*p_card_length = 0;
		return;
	}

	/* read in the chars */
	p_scard = (ilu_shortcardinal*)(*p_string);
	for (card_index = 0; card_index < *p_card_length; card_index++) {
		_http_input_shortcardinal(p_call, p_scard, p_error);
		if (ILU_ERRNOK(*p_error))
			return;
		p_scard++;
	}

	/* null terminate */
	*p_scard = 0;

}


/* ********************************************************* */
/* output wide char vector                                   */

static void _http_output_wstringvec (ilu_Call p_call, 
									 ilu_wstring astring, 
									 ilu_cardinal card_length, 
									 ILU_ERRS((IoErrs)) * p_error) {
	ilu_cardinal card_index;
	ilu_shortcardinal* p_scard;

	if (_http_get_call_state(p_call) == srvr4http_out_exception) {
		ILU_CLER(*p_error);
		/* don't send back exception members to existing http clients */
		return;
	}

	/* put out all the wide chars */
	p_scard = (ilu_shortcardinal*)astring;
	for (card_index = 0; card_index < card_length; card_index++) {
		_http_output_shortcardinal(p_call, *p_scard, p_error);
		if (ILU_ERRNOK(*p_error))
			return;
		p_scard++;
	}
}


/* ********************************************************* */
/* input wide char vector                                    */

static void _http_input_wstringvec (ilu_Call p_call, 
									ilu_wstring * p_string, 
									ilu_cardinal card_length, 
									ILU_ERRS((IoErrs)) * p_error) {
	ilu_cardinal card_index;
	ilu_shortcardinal* p_scard;
	ilu_boolean b_did_malloc_chars = ilu_FALSE;

	/* get right amount of space if none already supplied*/
	if (*p_string == NIL) {
	  *p_string = (ilu_wstring) ilu_MallocE(card_length * sizeof(ilu_character), p_error);
	  b_did_malloc_chars = ilu_TRUE;
	}

	if (*p_string == NIL)
		return;

	/* read in the chars */
	p_scard = (ilu_shortcardinal*)(*p_string);
	for (card_index = 0; card_index < card_length; card_index++) {
		_http_input_shortcardinal(p_call, p_scard, p_error);
		if (ILU_ERRNOK(*p_error)) {
		  if (b_did_malloc_chars) {
		    ilu_free(*p_string);
		    *p_string = NIL;
		  }
		  return;
		}
		p_scard++;
	}
}


/* ********************************************************* */
/* delay, resume abandon interp functions                    */
/* ********************************************************* */


/* _http_create_transport_remainder attempts to create and return a transport 
that contains the remainder of the reply. (Note that at this point
the status line and headers have already been read in.)
If unsuccessful, sets p_error.   [can currently happen if no Content-length
header was present in the reply, and we're not 1.0] 
*/


static ilu_Transport _http_create_transport_remainder(ilu_Call p_call, ILU_ERRS((IoErrs)) * p_error) {
  ilu_Transport p_transport_containing_remainder;
  ilu_cardinal card_body_size;
  ilu_bytes p_rest_of_message = NIL;
  ilu_boolean b_did_malloc_bytes = ilu_FALSE;
  ilu_byte c_dummy_buf[1];
	
  ILU_CLER(*p_error);
	
  if (_http_is_HEAD_method(p_call)) /* There will be nothing more to read anyhow */
    goto return_empty_transport;
	
  card_body_size = _http_body_length(p_call);
	
  if (_http_keep_alive(p_call) || _http_get_call_type(p_call) == clnt2ilu) {
    /* if we're in a keep alive situation, or we're dealing with ilu on the other
       side, we know that the connection won't close to indicate the end of the body so 
       they must have sent a content length header */
		
    if (card_body_size == 0) { 
			
      ilu_cardinal card_status_code = _http_status_code(p_call);
			
      if ((_http_get_call_type(p_call) == clnt2ilu) ||
	  /* it's quite possible to have no body if we're talking to an ilu server */
	  (card_status_code >= 100 && card_status_code <= 199) || /* informational have no body */
	  (card_status_code == 204) || (card_status_code == 205) || /* No Content, or (letting Reset Content be ok too)*/
	  (card_status_code == 304) /* Not Modified */
	  /* xxx Note that Apache 1.2.4 has been seen to not send a body with a 404 - Not Found status ! 
	     This doesn't conform to the http spec. */
	  ) 
	goto return_empty_transport;
			
      /* xxx must mod something here if eventually support chunked or mime transfers of version 1.1 */
      else 
	return ILU_ERR_CONS1(comm_failure, p_error, minor, ilu_cfm_protocol_sync_lost, NIL);
    }		
		
    /* allocate a buffer large enough to hold the remainder of the message */
    p_rest_of_message = (ilu_bytes) ilu_MallocE(card_body_size, p_error);
    b_did_malloc_bytes = ilu_TRUE;
		
    if (ILU_ERRNOK(*p_error))  /* must be no memory error */
      return NIL;
		
    /* read the remaiing bytes in */
    _http_transport_read_bytes(http_call_tport(p_call), p_rest_of_message, card_body_size, p_error);
    if (ILU_ERRNOK(*p_error)) {
      if (b_did_malloc_bytes)
	ilu_free(p_rest_of_message);
      return NIL;
    }
  }
  else {
    if (_http_get_call_type(p_call) != clnt2http) {
      HTTP_UNEXPECTED_CALL_TYPE_ASSERT();
    }
		
    /* since http1.0 responses are not required to have a content length header
       we'll read till eof read till end of file (i.e. connection closes) */
    if (_http_fill_buffer_till (http_call_tport(p_call), &p_rest_of_message, HTTP_BODY_ALLOC_SIZE,
				"", NIL, 0, NIL, NIL, ilu_TRUE, &card_body_size, p_error) == ilu_FALSE) 
      return NIL;
  }
	
  /* if we're on a boundaried transport, end the message  */
  _http_end_message(p_call, ilu_TRUE, ilu_TRUE, p_error);
  if (ILU_ERRNOK(*p_error)) {
    if (b_did_malloc_bytes)
      ilu_free(p_rest_of_message);
    return NIL;
  }
	
  /* create a buffering transport based on what we read in */
  p_transport_containing_remainder = _ilu_BufferTransport_Create(card_body_size, p_rest_of_message, p_error);
  if (ILU_ERRNOK(*p_error)) {
    if (b_did_malloc_bytes)
      ilu_free(p_rest_of_message);
    return NIL;
  }
	
  return p_transport_containing_remainder;


return_empty_transport:
  /* fake up an (empty transport) - allocate a 1 byte buffer  */
  p_rest_of_message = (ilu_bytes) ilu_MallocE(1, p_error);
  b_did_malloc_bytes = ilu_TRUE;		
  if (ILU_ERRNOK(*p_error))  /* must be no memory error */
      return NIL;
  
  p_transport_containing_remainder = _ilu_BufferTransport_Create(1, p_rest_of_message, p_error);
  if (ILU_ERRNOK(*p_error)) {
	  if (b_did_malloc_bytes)
		  ilu_free(p_rest_of_message);
	  return NIL;
  }
  
  /* now read the single byte out of it so it's empty */
  _http_transport_read_onebyte(p_transport_containing_remainder, c_dummy_buf, p_error);
  if (ILU_ERRNOK(*p_error))
	  return NIL;
  
  return p_transport_containing_remainder;

}



static ilu_refany _http_delay_interp(ilu_Call p_call, ILU_ERRS((IoErrs)) * p_error) {

	http_call_info_s* p_call_info_struct = ( http_call_info_s *)(p_call->ca_prdata2);

	/* check for a state we expect */
	switch (_http_get_call_state(p_call)) { 

	case clnt2http_in_interpret_reply:
	case clnt2ilu_in_interpret_reply:
		break;

	default:
			HTTP_UNEXPECTED_STATE_ASSERT();
	}

	/* try and get an (in-mem) transport that contains the rest of the reply */
	_http_delayed_interp_transport(p_call) = _http_create_transport_remainder(p_call, p_error);
	if (ILU_ERRNOK(*p_error))
		return NIL;

	/* set up the call to be re-initialized to the state it is in after InitCall
	has been called on it */
	_http_reused_call_init(p_call, p_error);
	if (ILU_ERRNOK(*p_error))
		return NIL;

	return p_call_info_struct;  /* return all our bound up state */
}


static void _http_resume_interp (ilu_Call p_call, ilu_refany pv_refany) {

  /* free up whatever infostruct might currently be in the call */
  _http_freeup_call(p_call);

  /* restore the call's http_call_info_struct */
  p_call->ca_prdata2 = pv_refany;

  /* set the call's transport to be what we stashed away */
  p_call->ca_prTrans = _http_delayed_interp_transport(p_call);

  return;
}




static ilu_boolean _http_abandon_delayed_interp(ilu_Connection conn, ilu_refany pv_refany,
			     ILU_ERRS((internal)) * p_error) {

	/* make up a dummy call to enable _http_freeup_call_infostruct to be called
	   to free things in the call's infostruct (hack) */
	ilu_Call_s a_call;  
	a_call.ca_prdata2 = pv_refany;
	_http_freeup_call_infostruct(&a_call, ilu_FALSE);

  return ilu_TRUE;		/* to satisfy compiler only */
}


/* ********************************************************* */
/* discard functions                                         */
/* really don't do anything - they rely on finish call being 
   entered in an unexpected state, in which case it just blows 
   away the whole connection. */
/* ********************************************************* */

/* ********************************************************* */
/* 	discard output                                           */

static ilu_boolean _http_discard_output (ilu_Call p_call, 
										 ILU_ERRS((IoErrs)) * p_error) {
	/* haven't yet determined what the best thing to do here is */
    return ILU_ERR_CONS1(comm_failure, p_error, minor, ilu_cfm_protocol_sync_lost, ilu_FALSE);
}

/* ********************************************************* */
/* 	discard input                                            */

static ilu_boolean _http_discard_input (ilu_Call p_call, 
										  ILU_ERRS((IoErrs)) * p_error) {
	/* haven't yet determined what the best thing to do here is */
    return ILU_ERR_CONS1(comm_failure, p_error, minor, ilu_cfm_protocol_sync_lost, ilu_FALSE);
}



/* ********************************************************* */
/* creates and returns an ilu_Protocol for http              */
/* ********************************************************* */

ilu_Protocol _ilu_http_Protocol (ilu_ProtocolInfo pinfo,
				 ilu_Error *err) {

	static ilu_Protocol p_http_protocol = ILU_NIL;

	ILU_CLER(*err);

	if (p_http_protocol)
	  return p_http_protocol;

	/* first create a husk containing all NILs so we'll 
	   seg fault if anything unimplemented is called */

	p_http_protocol = ilu_MallocE(sizeof(struct _ilu_Protocol_s), err);
	if (p_http_protocol == ILU_NIL)
	  return ILU_NIL;
	memset((void *) p_http_protocol, 0, sizeof(struct _ilu_Protocol_s));

	/* now fill in the actual implementations */

	/* don't need sizing information */
	p_http_protocol->pr_sizing_required = ilu_FALSE;

	p_http_protocol->pr_needs_boundaried_transport = ilu_FALSE;
	p_http_protocol->pr_needs_reliable_transport = ilu_TRUE;

	/* returns a string that's used to form the protocol info part of a string binding handle */
	p_http_protocol->pr_form_handle = _http_form_protocol_handle;

	/* no special things to do on connection close */
	p_http_protocol->pr_conn_closing = NULLFN;

	/* create and free a http data block - contains any (member-var like)
	   attributes specific to the protocol */
	p_http_protocol->pr_create_data_block = _http_create_non_batching_non_concurrent_data_block;
	p_http_protocol->pr_free_data_block = _http_free_data_block;
	p_http_protocol->pr_concurrent_requests = ilu_FALSE; /* must agree with type of data block */

	/* protocol specific initialization */
	p_http_protocol->pr_init_call = _http_init_call;

	/* finishing send of a record */
	p_http_protocol->pr_end_record = _http_end_record;

	/* finishing send of a sequence */
	p_http_protocol->pr_end_sequence = _http_end_sequence;

	/* start actually sending a request (puts out method name, SP) */
	p_http_protocol->pr_start_request = _http_start_request;

	/* function that will output an objects id as it's passed across the wire */
	p_http_protocol->pr_output_object_id = _http_output_object_id;

	/* inputs an object id */
	p_http_protocol->pr_input_object_id = _http_input_object_id;

	/* outputs starting of a record (actually outputs nothing) */
	p_http_protocol->pr_output_record = _http_output_record;

	/* outputs a string */	
	p_http_protocol->pr_output_string = _http_output_string;

	/* send start of a sequence */	
	p_http_protocol->pr_output_sequence = _http_output_sequence;

	/* sending an optional */
	p_http_protocol->pr_output_optional = _http_output_optional;

	/* sends a bunch of bytes */
	p_http_protocol->pr_output_bytes = _http_output_bytes;

	/* finishes sending a request */
	p_http_protocol->pr_finish_request = _http_finish_request;

	/* reads in a header */
	p_http_protocol->pr_read_header = _http_read_header;

	/* interprets http reply */
	p_http_protocol->pr_interpret_reply = _http_interpret_reply; 

	/* inputs begining of a record */
	p_http_protocol->pr_input_record = _http_input_record;

	/* inputs an enumeration code */
	p_http_protocol->pr_input_enum_code = _http_input_enum_code;

	/* inputs a sequence */
	p_http_protocol->pr_input_sequence = _http_input_sequence;

	/* input a string */
	p_http_protocol->pr_input_string = _http_input_string;

	/* input an optional */
	p_http_protocol->pr_input_optional = _http_input_optional;

	/* input an bytes */
	p_http_protocol->pr_input_bytes = _http_input_bytes;

	/* what to do at end of receiving reply */
	p_http_protocol->pr_reply_read = _http_reply_read;

	/* discard output */
	p_http_protocol->pr_discard_output = _http_discard_output;

	/* discard input */
	p_http_protocol->pr_discard_input = _http_discard_input;

	/*pre finishes and finishes a call */
	p_http_protocol->pr_finish_call = _http_finish_call;
	p_http_protocol->pr_prefinish_call = _http_prefinish_call;

	/* interprets a request */
	p_http_protocol->pr_interpret_request = _http_interpret_request;
	
	/* Do what needs to be done after arguments unmarshalling */
	p_http_protocol->pr_request_read = _http_request_read;

	/* begins a reply */
	p_http_protocol->pr_begin_reply = _http_begin_reply;

	/* outputs an enum value */
	p_http_protocol->pr_output_enum_code = _http_output_enum_code;

	/* finishes a reply */
	p_http_protocol->pr_finish_reply = _http_finish_reply;

	/* output and input of a cardinal */
	p_http_protocol->pr_output_cardinal = _http_output_cardinal;
	p_http_protocol->pr_input_cardinal = _http_input_cardinal;

	/* output and input of a short cardinal */
	p_http_protocol->pr_output_shortcardinal = _http_output_shortcardinal;
	p_http_protocol->pr_input_shortcardinal = _http_input_shortcardinal;

	/* output and input of a integer */
	p_http_protocol->pr_output_integer = _http_output_integer;
	p_http_protocol->pr_input_integer = _http_input_integer;

	/* output and input of a short integer */
	p_http_protocol->pr_output_shortinteger = _http_output_shortinteger;
	p_http_protocol->pr_input_shortinteger = _http_input_shortinteger;

	/* output and input of a byte */
	p_http_protocol->pr_output_byte = _http_output_byte;
	p_http_protocol->pr_input_byte = _http_input_byte;

	/* output and input of a shortchar */
	p_http_protocol->pr_output_shortchar = _http_output_shortchar;
	p_http_protocol->pr_input_shortchar = _http_input_shortchar;

	/* output and input of a boolean */
	p_http_protocol->pr_output_boolean = _http_output_boolean;
	p_http_protocol->pr_input_boolean = _http_input_boolean;

	/* output and input of an opaque */
	p_http_protocol->pr_output_opaque = _http_output_opaque;
	p_http_protocol->pr_input_opaque = _http_input_opaque;

	/* output and input of a union */
	p_http_protocol->pr_output_union = _http_output_union;
	p_http_protocol->pr_input_union = _http_input_union;
	p_http_protocol->pr_end_union = _http_end_union;

	/* output, input and end of an array */
	p_http_protocol->pr_output_array = _http_output_array;
	p_http_protocol->pr_input_array = _http_input_array;
	p_http_protocol->pr_end_array = _http_end_array;

	/* output and input of a sequence mark */
	p_http_protocol->pr_output_sequence_mark = _http_output_sequence_mark;
	p_http_protocol->pr_input_sequence_mark = _http_input_sequence_mark;

	/* output and input of a stringvec */
	p_http_protocol->pr_output_stringvec = _http_output_stringvec;
	p_http_protocol->pr_input_stringvec = _http_input_stringvec;

	/* output and input of a longcardinal */
	p_http_protocol->pr_output_longcardinal = _http_output_longcardinal;
	p_http_protocol->pr_input_longcardinal = _http_input_longcardinal;

	/* output and input of a longinteger */
	p_http_protocol->pr_output_longinteger = _http_output_longinteger;
	p_http_protocol->pr_input_longinteger = _http_input_longinteger;

	/* output and input of a real */
	p_http_protocol->pr_output_real = _http_output_real;
	p_http_protocol->pr_input_real = _http_input_real;

	/* output and input of a longreal */
	p_http_protocol->pr_output_longreal = _http_output_longreal;
	p_http_protocol->pr_input_longreal = _http_input_longreal;

	/* output and input of a shortreal */
	p_http_protocol->pr_output_shortreal = _http_output_shortreal;
	p_http_protocol->pr_input_shortreal = _http_input_shortreal;

	/* output and input of a character */
	p_http_protocol->pr_output_character = _http_output_character;
	p_http_protocol->pr_input_character = _http_input_character;

	/* output and input of a wstring */
	p_http_protocol->pr_output_wstring = _http_output_wstring;
	p_http_protocol->pr_input_wstring = _http_input_wstring;

	/* output and input of a wstringvec */
	p_http_protocol->pr_output_wstringvec = _http_output_wstringvec;
	p_http_protocol->pr_input_wstringvec = _http_input_wstringvec;

#ifdef ADD_VARIANT_SUPPORT

	/* input and output of an ilu_Type type description */
	p_http_protocol->pr_output_pickle = _ilu_OutputPickle;
	p_http_protocol->pr_input_pickle = _ilu_InputPickle;

#endif /* ADD_VARIANT_SUPPORT */

#ifdef ILU_FIXED_POINT_SUPPORT

	p_http_protocol->pr_output_fixedpoint = _ilu_OutputFixedpoint;
	p_http_protocol->pr_input_fixedpoint = _ilu_InputFixedpoint;
	p_http_protocol->pr_size_of_fixedpoint = _ilu_SizeOfFixedpoint;

#endif /* ILU_FIXED_POINT_SUPPORT */

#ifdef ILU_REFERENCE_TYPES

	p_http_protocol->pr_size_of_reference = _ilu_SizeOfReference;
	p_http_protocol->pr_output_reference = _ilu_OutputReference;
	p_http_protocol->pr_input_reference = _ilu_InputReference;
	p_http_protocol->pr_end_input_reference = _ilu_EndInputReference;

#endif /* def ILU_REFERENCE_TYPES */

	/* delay, resume, abandon interp - do nothing because we're not concurrent */
	p_http_protocol->pr_delay_interp = _http_delay_interp;
	p_http_protocol->pr_resume_interp = _http_resume_interp;
	p_http_protocol->pr_abandon_delayed_interp = _http_abandon_delayed_interp;

	/* begin and finish exception */
	p_http_protocol->pr_begin_exception = _http_begin_exception;
	p_http_protocol->pr_finish_exception = _http_finish_exception;

	/* return back our filled in protocol structure */
	ILU_CLER(*err);
	return p_http_protocol;
}


/* ********************************************************* */
/* if we're supposed to be using a proxy server, copys the 
proxy hostname into *pc_proxyname, puts the port numbers
into p_ul_proxy_port, and returns ilu_TRUE, else leaves the args
alone and returns ilu_FALSE   */


static ilu_boolean _http_get_proxy_name_and_port(char* pc_proxyname, 
												 unsigned long* p_ul_proxy_port) {

	char* pc_the_info;

	/* ILU_HTTP_PROXY_INFO is of the form proxy.host.name:portnumber */

	if ((pc_the_info = getenv(ILU_HTTP_PROXY_INFO_ENV_VAR)) == NIL)
		return ilu_FALSE; /* no proxy info set */

	/* copy the proxy host name */
	while (*pc_the_info && (*pc_the_info != ':')) {
		*pc_proxyname = *pc_the_info;
		pc_proxyname++;
		pc_the_info++;
	}

	*pc_proxyname = '\0';

	if (*pc_the_info != ':') /* must specify port number */
		return ilu_FALSE;

	pc_the_info++; /* advance to start of port number */

	/* convert it */
	if (sscanf(pc_the_info, "%lu", p_ul_proxy_port) != 1)
		return ilu_FALSE;

	return ilu_TRUE;
}


/* ********************************************************* */
/* returns ilu_TRUE if the two hostnames are in the same 
domain - i.e. they don't need a proxy to talk, else ilu_FALSE */

static ilu_boolean _http_same_domain(char* pc_proxyname, char* pc_hostname) {

	/* XXX I dont' believe there is a general solution to this question  -
	I think it's site specific */

	return ilu_FALSE;
}


/* ********************************************************* */
/* if we're supposed to be using a proxy server to talk to the
host in pc_hostname, sets the args appropriately and returns 
ilu_TRUE, else leaves the args alone and returns ilu_FALSE   */

static ilu_boolean _http_proxy_contact_info(char** ppc_proxy_name,
											unsigned long* p_ul_port,
											char* pc_hostname,
											ilu_Error* p_error) {

	char c_proxyname[1024];
	static char* s_pc_proxy_name = NIL;	    /* cache name of proxy server */
	static unsigned long s_ul_proxy_port = 8000;    /* cache port of proxy server */

	ILU_CLER(*p_error);

	/* if we're not using a proxy, just return */
	if (!_http_get_proxy_name_and_port(c_proxyname, &s_ul_proxy_port))
		return ilu_FALSE;

	if (_http_same_domain(c_proxyname, pc_hostname))
		/* we're in the same domain, so don't need to use proxy */
		return ilu_FALSE;

	if ((s_pc_proxy_name == NIL) || 
		(strcmp(s_pc_proxy_name, c_proxyname) != 0)) {

		/* first time or proxy name changed */
		ilu_free(s_pc_proxy_name);
		s_pc_proxy_name = ilu_StrdupE(c_proxyname, p_error);
		if (ILU_ERRNOK(*p_error))
			goto error_cleanup_label;
	}

	/* return cached values */
	*ppc_proxy_name = s_pc_proxy_name;
	*p_ul_port = s_ul_proxy_port;

	return ilu_TRUE;
	
error_cleanup_label:

	ilu_free(s_pc_proxy_name);
	s_pc_proxy_name = NIL;
	return ilu_FALSE;
}


/* ********************************************************* */
/* sets the encoded constact information for an object, 
   taking potential use of http proxy servers into account 
   returns false on error  */

static ilu_boolean _http_generate_contact_info (ilu_string* p_str_encodedContactInfo, 
												char* pc_hostname,
												unsigned long ul_port,
												char* pc_extra_tinfo,
												ilu_Error* p_error) {

	ilu_string str_protohandle;
	char* pc_immediate_contact_name = pc_hostname; /* name of host we directly communicate with (end server or proxy) */
	unsigned long ul_immediate_port_number = ul_port;
	ILU_CLER(*p_error);

	str_protohandle = _http_form_protocol_handle(NIL,NIL);		/* get protocol information */

	/* see if we're supposed to be using a proxy server to talk to the host in pc_hostname*/
	if (_http_proxy_contact_info(&pc_immediate_contact_name, &ul_immediate_port_number, pc_hostname, p_error) == ilu_FALSE) {

		if (ILU_ERRNOK(*p_error)) /* if we returned false because of an error */
			return ilu_FALSE;
	}

	*p_str_encodedContactInfo  = ilu_MallocE(strlen(str_protohandle) + 
									1 +		/* _ */
									strlen(pc_hostname) + /* end host */
									1 +		/* _ */
									10 +	/* end port */
									1 +		/* ILU_CINFO_DIVIDER */
									(pc_extra_tinfo ? strlen(pc_extra_tinfo) : 0) + /* any extra tinfo */
									4 +		/* tcp_ */
									strlen(pc_immediate_contact_name) + /* immediate host */
									1 +		/* _ */
									10 +	/* immediate port */
									1,		/* null */ 
									p_error);
	if (ILU_ERRNOK(*p_error))  
		return ilu_FALSE;

	/* cons them together */
	if (pc_extra_tinfo)
		sprintf(*p_str_encodedContactInfo, "%s_%s_%lu%c%s%ctcp_%s_%lu", str_protohandle, pc_hostname, ul_port,
		ILU_CINFO_DIVIDER, pc_extra_tinfo, ILU_TINFO_DIVIDER, pc_immediate_contact_name, ul_immediate_port_number);
	else
		sprintf(*p_str_encodedContactInfo, "%s_%s_%lu%ctcp_%s_%lu", str_protohandle, pc_hostname, ul_port,
		ILU_CINFO_DIVIDER, pc_immediate_contact_name, ul_immediate_port_number);
	ilu_free(str_protohandle);

	return ilu_TRUE;
}



/* ********************************************************* */
/* function to parse up a http url                           */

ilu_boolean _ilu_Parse_HTTP_URL (ilu_string    istr_encodedSBH, 
				 ilu_string*   p_str_plainInstanceHandle, 
				 ilu_string*   p_str_plainServerID,
				 ilu_string*   p_str_plainMstid, 
				 ilu_string*   p_str_encodedContactInfo, 
				 ilu_cardinal* p_card_encodedContactInfoLen,
				 ilu_boolean*  p_boolean_passEncodedContactInfo,
				 ilu_Error*    p_error)
{

  /* Parse HTTP style URL: http://<host>[:<port>]/<path> */

  unsigned long ul_port;
  ilu_string pc_walker;
  char* pc_hostname;
  char* pc_extra_tinfo;
  char* pc_extra_tinfo_end;
  char* pc_hostname_end;
  char* pc_hostname_copy;
  char* pc_path;
  char* pc_path_end;
  ilu_cardinal card_size;
  char c_path_default[] = "/";


  ILU_CLER(*p_error);

  /* init to nils */
  if (p_str_plainInstanceHandle != NIL) *p_str_plainInstanceHandle = NIL;
  if (p_str_plainServerID != NIL) *p_str_plainServerID = NIL;
  if (p_str_plainMstid != NIL) *p_str_plainMstid = NIL;
  if (p_str_encodedContactInfo != NIL) *p_str_encodedContactInfo = NIL;
  if (p_card_encodedContactInfoLen != NIL) *p_card_encodedContactInfoLen = 0;
  if (p_boolean_passEncodedContactInfo) *p_boolean_passEncodedContactInfo = ilu_FALSE;

  /* ensure that the istr_encodedSBH is for http */
  pc_walker = istr_encodedSBH;
  if (_http_strnstr((ilu_bytes)pc_walker, "http://", 7, ilu_FALSE) != pc_walker)
    return ILU_ERR_CONS1(inv_objref, p_error, minor, ilu_iom_sbh, ilu_FALSE);

  /* we go though the following parsing & copying contortions because we don't want to make any
     modifications to the contents of istr_encodedSBH, and we don't want to try using
     any fixed sized buffers (e.g. with sscanf) because the passed istr_encodedSBH could
     be quite huge */

  /* point to where the hostname should start */
  pc_walker = pc_walker + 7;
  pc_hostname = pc_walker;
  while (*pc_walker && (*pc_walker != ':') && (*pc_walker != '/'))
    pc_walker++;		/* advance to end of hostname */

  pc_hostname_end = pc_walker;	/* save where a null in the hostname would be */
  ul_port = DEFAULT_HTTP_PORT_NUMBER; /*  default the port to start with */

  switch (*pc_walker) {

  case '/':			/* no port specified - use default port */
    pc_path = pc_walker;	/* save where the path starts */
    break;

  case ':':			/* maybe found a port number */
    while (*pc_walker && (*pc_walker != '/'))
      pc_walker++;		/* advance to end of port number */

    switch (*pc_walker) {

    case '/':			/* a port with some following path */

      if (sscanf (pc_hostname_end + 1, "%lu/", &ul_port) != 1)
	/* bad somehow, return malformed SBH error */
	return ILU_ERR_CONS1(inv_objref, p_error, minor, ilu_iom_sbh, ilu_FALSE);

      pc_path = pc_walker;	/* save where the path starts */

      break;

    case '\0':			/* just a port with no training / for a path */

      if (sscanf (pc_hostname_end + 1, "%lu", &ul_port) != 1)
	/* bad somehow, return malformed SBH error */
	return ILU_ERR_CONS1(inv_objref, p_error, minor, ilu_iom_sbh, ilu_FALSE);

      pc_path = c_path_default;	/* set to default path */
	
      break;

    default:			/* what else could it be??? */
      return ILU_ERR_CONS1(inv_objref, p_error, minor, ilu_iom_sbh, ilu_FALSE);
    }

    break;

  case '\0':			/* must have been just hostname without port or path  */
    pc_path = c_path_default;	/* set to default path */
    break;

  default:			/* what else could it be??? */
    return ILU_ERR_CONS1(inv_objref, p_error, minor, ilu_iom_sbh, ilu_FALSE);
  }

  /* find the end of the actual path (dont' want any params or queries in it) */
  pc_path_end = pc_path;
  while (*pc_path_end && (*pc_path_end != ';') && (*pc_path_end != '?'))
    pc_path_end++;

  /* so now, pc_hostname points to the start of the hostname, pc_hostname_end points
     to where a null would be in a null terminated version of the hostname, and
     pc_path points to the path, and pc_path_end points to where a null would be 
     in a null terminated version of the path */

  /* if contact info is wanted, see if any tinfo= param was present - if so, it 
     indicates some transports to be placed in between http and tcp */
  if (p_str_encodedContactInfo) {
    pc_extra_tinfo = strstr(pc_path_end, ";ilu_extra_tinfo=");
    if (pc_extra_tinfo) {
      char* pc_temp;
      pc_extra_tinfo = pc_extra_tinfo + 17 /* strlen of ilu_extra_tinfo= */ ;
      pc_extra_tinfo_end = pc_extra_tinfo;
      while (*pc_extra_tinfo_end && (*pc_extra_tinfo_end != ';') && (*pc_extra_tinfo_end != '?'))
	pc_extra_tinfo_end++;
			
      /* now pc_extra_tinfo points to start of extra transport info and pc_extra_tinfo_end
	 points to where a null would be in a null terminated version of the extra tinfo */
      pc_temp =  ilu_MallocE(pc_extra_tinfo_end - pc_extra_tinfo + 1, p_error);
      if (ILU_ERRNOK(*p_error))  
	return ilu_FALSE;
      memcpy(pc_temp, pc_extra_tinfo, pc_extra_tinfo_end - pc_extra_tinfo);
      *(pc_temp + (pc_extra_tinfo_end - pc_extra_tinfo)) = '\0'; /* null terminate */
      pc_extra_tinfo = pc_temp;
      /* pc_extra_tinfo is now a copy of the extra tinfo that was supplied */
    }
  }
	
  /* copy the path in as the instance handle */
  card_size = pc_path_end - pc_path + 1;

  if (p_str_plainInstanceHandle) { /* instance handle is wanted */
    *p_str_plainInstanceHandle = ilu_MallocE(pc_path_end - pc_path + 1, p_error);
    if (ILU_ERRNOK(*p_error))  
      return ilu_FALSE;
	
    memcpy(*p_str_plainInstanceHandle, pc_path, pc_path_end - pc_path);
    *(*p_str_plainInstanceHandle + (pc_path_end - pc_path)) = '\0'; /* null terminate */
  }

  /* make a convenient copy of the hostname */
  pc_hostname_copy =  ilu_MallocE(pc_hostname_end - pc_hostname + 1, p_error);
  if (ILU_ERRNOK(*p_error)) {
    if (p_str_plainInstanceHandle) ilu_free(*p_str_plainInstanceHandle);
    return ilu_FALSE;
  }
  memcpy(pc_hostname_copy, pc_hostname, pc_hostname_end - pc_hostname);
  *(pc_hostname_copy + (pc_hostname_end - pc_hostname)) = '\0';

  /* invent a server id based on target host name and port */
  card_size =	6 +		/* http. */
    pc_hostname_end - pc_hostname + /* hostname */
      1 +			/* underscore */
	10 +			/* portnumber */
	  1;			/* null terminate */

  if (p_str_plainServerID) {
    *p_str_plainServerID = ilu_MallocE(card_size, p_error);
    if (ILU_ERRNOK(*p_error)) {
      ilu_free(pc_hostname_copy);
      if (p_str_plainInstanceHandle) ilu_free(*p_str_plainInstanceHandle);
      return ilu_FALSE;
    }

    sprintf(*p_str_plainServerID, "%s%s_%lu", "httpd.", pc_hostname_copy, ul_port);
  }

  /* we know that http SBH's are for this type */
  if (p_str_plainMstid) {
    *p_str_plainMstid = ilu_StrdupE(HTTP_RESOURCE_OBJECT_TYPE_ID, p_error);
    if (ILU_ERRNOK(*p_error)) { 
      ilu_free(pc_hostname_copy);
      if (p_str_plainInstanceHandle) ilu_free(*p_str_plainInstanceHandle);
      if (p_str_plainServerID) ilu_free(*p_str_plainServerID);
      return ilu_FALSE;
    }
  }

  /* cons up the contact information */
  if (p_str_encodedContactInfo) {
    if (!_http_generate_contact_info (p_str_encodedContactInfo, pc_hostname_copy, ul_port, pc_extra_tinfo, p_error)) {
      ilu_free(pc_hostname_copy);
      ilu_free(pc_extra_tinfo);
      if (p_str_plainInstanceHandle) ilu_free(*p_str_plainInstanceHandle);
      if (p_str_plainServerID) ilu_free(*p_str_plainServerID);
      if (p_str_plainMstid) ilu_free(*p_str_plainMstid);
      return ilu_FALSE;
    } else {
      if (p_boolean_passEncodedContactInfo)
	*p_boolean_passEncodedContactInfo = ilu_TRUE;
    }    
    ilu_free(pc_extra_tinfo);
  }

  ilu_free(pc_hostname_copy);

  if (p_card_encodedContactInfoLen && p_str_encodedContactInfo)
    *p_card_encodedContactInfoLen = strlen(*p_str_encodedContactInfo);

  return ilu_TRUE;
}


/* ********************************************************* */
/* Returns the http URL form of the SBH, or NIL if not attainable
- caller owns the returned string.  Ownership of pc_objects_sbh
is passed to this function */

static ilu_string _http_SBH_to_URL (ilu_string pc_objects_sbh, ilu_Error* p_error) {
	
	ilu_string pc_objects_ih = NIL;
	ilu_string pc_objects_url = NIL;
	ilu_string pc_encodedContactInfo = NIL;
	ilu_string pc_tcp_info;
	ilu_boolean pass;
	char* pc_walker;
	
	ILU_CLER(*p_error);
	if (!pc_objects_sbh)
		goto cleanup;
	
	/* see if it's already in URL form */
	if ((strncmp("http:", pc_objects_sbh, 5) == 0) || (strncmp("HTTP:", pc_objects_sbh, 5) == 0))
		return pc_objects_sbh;
	
	/* must try and construct a URL */
	if (!ilu_ParseSBH(pc_objects_sbh /* URL (encoded, of course) */ ,
	       &pc_objects_ih  /* plainInstH (opt) */ ,
		   NIL /* plainServerID (opt) */ ,
		   NIL /* plainMstid (opt) */ ,
		   &pc_encodedContactInfo /* encodedContactInfo (opt) */ ,
		   NIL /* encodedContactInfoLen (opt) */ ,
			  &pass,
		   p_error) 
		   || ILU_ERRNOK(*p_error)) {
		ilu_free(pc_objects_sbh);
		return NIL;
	}
	
	/* see if it's got http as its protocol */
	if (strncmp("http_", pc_encodedContactInfo, 5) != 0)
		goto cleanup;
	
	/* determine where the tcp address will start */
	pc_tcp_info = strstr(pc_encodedContactInfo, "@tcp_");
	if (!pc_tcp_info)
		goto cleanup;
	pc_tcp_info = pc_tcp_info + 5;
	
	/* replace the next underscore with a colon so we have ip address : port */
	pc_walker = pc_tcp_info;
	while (1) {
		if (*pc_walker == '_') {
			*pc_walker = ':';
			break;
		}
		if (!pc_walker)
			goto cleanup;
		pc_walker++;
	}
	
	/* allocate space to hold the url */
	pc_objects_url = ilu_MallocE(7 + /* http:// */
		strlen(pc_tcp_info) + /* ipaddress and port */
		strlen(pc_objects_ih) + 1, /* instance handle and terminating NULL */
		p_error);
	
	/* write it together */
	if (ILU_ERROK(*p_error))
		sprintf(pc_objects_url, "http://%s%s", pc_tcp_info, pc_objects_ih);
	
cleanup:
	if (pass)
	  ilu_free(pc_encodedContactInfo);
	ilu_free(pc_objects_ih);
	ilu_free(pc_objects_sbh);
	return (pc_objects_url);
}


/* ********************************************************* */
/* Returns the http URL of the object, or NIL if not attainable
- caller owns the returned string */

ILU_PUBLIC ilu_string ilu_URLOfObject (ilu_Object p_object, ilu_Error* p_error) {
	
	ilu_string pc_objects_sbh_orig;
	ilu_string pc_objects_sbh;
	
	if (!p_object)
		return NIL;
	
	/* get a duplicate of the object's sbh */
    ilu_EnterServer(p_object->ob_server, p_object->ob_class);
	pc_objects_sbh_orig = ilu_SBHOfObject(p_object);
    if (pc_objects_sbh_orig)
		pc_objects_sbh = ilu_StrdupE(pc_objects_sbh_orig, p_error);
    ilu_ExitServer(p_object->ob_server, p_object->ob_class);
	
	if (ILU_ERRNOK(*p_error) || (!pc_objects_sbh_orig))
		return NIL;
	
	return _http_SBH_to_URL(pc_objects_sbh, p_error);
}


#if 0 /* defunct */

/* ********************************************************* */
/* setup functionality for http                              */

ILU_PUBLIC ILU_ERRS((ProtocolAlreadyRegistered, MaxCountExceeded)) 
setup_http_protocol (void) {

	ilu_RegisterSBHParser ("http", _ilu_Parse_HTTP_URL);
	return ilu_RegisterProtocol("http", _ilu_http_Protocol, ilu_FALSE);
}

#endif


/* ********************************************************* */
/* end of file                                               */
/* ********************************************************* */
