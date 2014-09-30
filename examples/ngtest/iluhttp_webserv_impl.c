/** $Id: iluhttp_webserv_impl.c,v 1.9 1999/08/03 01:58:17 janssen Exp $
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
/* Last edited by Mike Spreitzer October 9, 1998 1:56 pm PDT */

#include "nglib.h"
#include "ngwebserver.h"

/* ILU server */
ILU_C_Server g_http_server     = NULL;



/* ********************************************************* */
/* object table and setup functions                          */
/* ********************************************************* */

static CORBA_Object object_of_http_handle (ilu_string str_object_id, 
										   ilu_private p_iluserver) {
	
	/* object id's for iluhttp.Resources are required to begin with a / */
	if (*str_object_id == '/')
		
		/* simply return a new true object with this id  */
		return (iluhttp_Resource__OTCreateTrue (str_object_id,
		*((ILU_C_Server *) p_iluserver), NULL)); 
	
	return NULL;
}


static void free_http_object_table_storage (ilu_private p_iluserver) {
  return; /* nothing to do yet */
}




void iluhttp_impl_setup_server() {

	static ilu_boolean b_already_setup = ilu_FALSE;
	ILU_C_ObjectTable object_table;
	char pc_serverid[1024];						/* holds a server id */

	if (b_already_setup)
		return ;
	
	/* create object table and ILU server*/

	/* HTTPServer */
	object_table = ILU_C_CreateObjectTable (object_of_http_handle, 
			free_http_object_table_storage, (ilu_private) &g_http_server);

	sprintf (pc_serverid, "HTTPServer.%s%s", g_pc_hostname, g_pc_ngwebserver_suffix);

	g_http_server = ILU_C_InitializeServer ( pc_serverid, object_table, 
		g_pc_http_pinfo, g_ppc_http_tinfo, (ilu_Passport) ILU_NIL, ilu_TRUE );

	b_already_setup = ilu_TRUE;

}



/* ********************************************************* */
/* build up a simple response                                */

iluhttp_Response* build_simple_response (iluhttp_StatusCode status, char* pc_body_contents, 
									  ilu_cardinal card_body_length) {

	iluhttp_Response* p_the_response;			/* holds our response */ 
	iluhttp_Header* p_date_header;				/* holds our Date header */
	iluhttp_Header* p_length_header;			/* holds Content-length header */
	char c_buffer[64];

	p_the_response = (iluhttp_Response*) ilu_malloc(sizeof(iluhttp_Response));	/* create empty response */

	p_the_response->status = status;					/* set it to indicate the passed status */ 

	if (card_body_length != 0) { /* we don't want ilu http to auto generate content-length */

		p_date_header = (iluhttp_Header*) ilu_malloc(2*sizeof(iluhttp_Header));		/* create headers */

		p_date_header->name = nglib_duplicate_c_string("Date");				/* make a General header containing Date */
		p_date_header->value = nglib_get_current_time_string();

		p_length_header = p_date_header + 1;				/* create Content-length header */

		p_length_header->name = nglib_duplicate_c_string("Content-length");
		sprintf(c_buffer, "%lu", card_body_length);
		p_length_header->value = nglib_duplicate_c_string(c_buffer);

		p_the_response->headers._maximum = 2;				/* put our header into the header sequence */
		p_the_response->headers._length = 2;
		p_the_response->headers._buffer = p_date_header;
	}
	else { /* let ilu http auto generate content-length header */

		p_date_header = (iluhttp_Header*) ilu_malloc(sizeof(iluhttp_Header));		/* create headers */

		p_date_header->name = nglib_duplicate_c_string("Date");				/* make a General header containing Date */
		p_date_header->value = nglib_get_current_time_string();

		p_the_response->headers._maximum = 1;				/* put our header into the header sequence */
		p_the_response->headers._length = 1;
		p_the_response->headers._buffer = p_date_header;
	}


	if (pc_body_contents != NULL) {
		p_the_response->body = (iluhttp_OptionalEntityBody) ilu_malloc (sizeof(iluhttp_EntityBody));
		p_the_response->body->_buffer = (ilu_bytes) nglib_duplicate_c_string(pc_body_contents);
		p_the_response->body->_length = strlen(pc_body_contents);
		p_the_response->body->_maximum = p_the_response->body->_length;
	}
	else 
		p_the_response->body = NULL;

	
	return p_the_response;
}



/* ********************************************************* */
/* 'Very SIMPLE' Methods                                     */
/* ********************************************************* */


/* ********************************************************* */
/*  (brute force) Routine used when the file being retrieved has the extension
    iluhttpresponse (a file containing the complete http response)

Line format of an .iluhttpresponse file:

Status
Number of headers
headername headervalue
...
headername headervalue
body



For example:

302
2
Location: http://pundit.parc.xerox.com/foo.html
Content-Length: 62
<a href=http://pundit.parc.xerox.com/foo.html>New Location</a>


*/

iluhttp_Response* build_response_from_file(char* pc_filename, ilu_boolean b_include_body) {
	
	iluhttp_Response* p_the_response;	/* for building up our response */
	int i_response_code;
	int i_num_headers;
	iluhttp_Header* p_header;
	int i_index;
	char c_header_name[128];
	char c_header_value[256];
	int i_num_scanned;
	char* pc_value;
	FILE* p_file;
	int i_temp;
	fpos_t filepos_current;
	long l_current_pos;
	long l_end_pos;
	long l_body_size;
	unsigned char* pc_body_bytes;
	char* pc_walker;
				
	if ((p_file = fopen(pc_filename, "rb")) == NULL)
		return NULL;
	
	/* get response code and number of headers */
	if (fscanf(p_file, "%i\n%i\n", &i_response_code, &i_num_headers) != 2) {
		fclose(p_file);
		return NULL;
	}
	
	/* build up our response */
	p_the_response = (iluhttp_Response*) ilu_malloc(sizeof(iluhttp_Response));	/* create empty response */
	
	p_the_response->status = (iluhttp_StatusCode)i_response_code;
	
	
	if (i_num_headers > 0) /* create space for headers */
		p_the_response->headers._buffer = (iluhttp_Header*) ilu_malloc(i_num_headers * sizeof(iluhttp_Header));
	else 
		p_the_response->headers._buffer = NULL;
	
	p_the_response->headers._maximum = i_num_headers;
	p_the_response->headers._length = i_num_headers;
	p_header = p_the_response->headers._buffer;
	
	/* read in the headers */
	for (i_index = 0; i_index < i_num_headers; i_index++) {
		c_header_name[0] = '\0';
		c_header_value[0] = '\0';
		
		/* read in header name */
		i_num_scanned = fscanf(p_file, "%[^:] :", c_header_name);
		
		if (i_num_scanned != 1) { /* problem - free things up */
			for (i_temp = 0; i_temp < i_index; i_temp++) {
				ilu_free(((p_the_response->headers._buffer)[i_temp]).name);
				ilu_free(((p_the_response->headers._buffer)[i_temp]).value);
			}
			ilu_free(p_the_response->headers._buffer);
			ilu_free(p_the_response);
			fclose (p_file);
			return NULL;
		}
		
		/* read in header value if any */
		pc_value = fgets(c_header_value, 256, p_file);
		if (pc_value) /* non empty header value */ {
			c_header_value[strlen(c_header_value) - 1] = '\0';  /* remove \n */
			while (isspace(*pc_value)) /* remove any leading whitespace */
				pc_value++;
			if (pc_value[0] == '\0')
				pc_value = NULL;
			if (pc_value) { /* remove any trailing whitespace */
				pc_walker = pc_value + strlen(pc_value) - 1;
				while (isspace(*pc_walker))
					pc_walker--;
				*(pc_walker + 1) = '\0';
			}
		}
		
		/* assign copies of name and value to header */
		p_header->name = nglib_duplicate_c_string(c_header_name);
		p_header->value = pc_value ? nglib_duplicate_c_string(c_header_value) : NULL;
		p_header++;
	}
	
	if (b_include_body) { /* if we should add the body */
		/* get a buffer of the file contents */
		
		l_current_pos = ftell(p_file);
		fgetpos(p_file, &filepos_current);
		fseek(p_file, 0, SEEK_END);
		l_end_pos = ftell(p_file);
		fsetpos(p_file, &filepos_current);
		l_body_size = l_end_pos - l_current_pos;	
		
		pc_body_bytes = ilu_malloc(l_body_size);
		fread(pc_body_bytes, l_body_size, l_body_size, p_file);
		
		/* and stuff on our body bytes */
		p_the_response->body = (iluhttp_OptionalEntityBody) ilu_malloc (sizeof(iluhttp_EntityBody));
		p_the_response->body->_buffer = pc_body_bytes;
		p_the_response->body->_length = l_body_size;
		p_the_response->body->_maximum = l_body_size;
	}
	else p_the_response->body = NULL;
	
	/* close the file */
	fclose(p_file);

	return p_the_response;
}



/* ********************************************************* */
/* this function basically does the real work for either get or head */

iluhttp_Response* server_iluhttp_Resource_GETorHEAD (iluhttp_Resource http_obj, 
												  iluhttp_Request* p_http_req,
												  ilu_boolean b_include_body,
												  ILU_C_ENVIRONMENT* ilu_env) {

	char c_filename[2048];			/* holds the name of of the referenced file */
	int h_file;						/* for obtaining file information */
	STAT_STRUCT stat_struct;
	char c_buffer[256];				/* temporary and utility */
	char* pc_walker;
	char* pc_type_string;			/* points to MIME type string */
	char* pc_extension;				/* points to file extension */
	ilu_bytes pc_body_bytes;		/* points to body buffer */
	iluhttp_Response* p_the_response;	/* for building up our response */
	iluhttp_Header* p_header;

	/* make up the full file name, defaulting to the default file name if necessary */
	if (strcmp(p_http_req->URI, "/") == 0) /*  */
		sprintf(c_filename, "%s%s", g_pc_file_base, DEFAULT_SERVER_FILE_NAME);
	else
		sprintf(c_filename, "%s%s", g_pc_file_base, p_http_req->URI);

	/* terminate properly if params or queries are present*/
	pc_walker = c_filename;
	while (*pc_walker && (*pc_walker != ';') && (*pc_walker != '?'))
		pc_walker++;
	*pc_walker = '\0';

	if (nglib_validate_path(c_filename, g_pc_file_base) == ilu_FALSE) {
		/* return not found if we've gone outside our root */
		return build_simple_response(iluhttp_NotFound, NULL, 0);
	}

	/* open up the file */
	h_file = OPEN_FUNCTION(c_filename, RDONLY_MODE); 
	if ((h_file == -1) || (FSTAT_FUNCTION(h_file, &stat_struct) == -1)) {
		/* return not found if there's a problem opening the file */
		return build_simple_response(iluhttp_NotFound, NULL, 0);
	}

	/* determine file extension */
	pc_extension = c_filename + strlen(c_filename) - 1;
	while (pc_extension > c_filename) {
		if (*pc_extension == '.') {
			pc_extension++;
			break;
		}
		pc_extension--;
	}

	if (pc_extension == c_filename)  /* ensure we didn't walk all the way back */
		pc_extension = NULL;


	if (pc_extension && strcmp(pc_extension, "iluhttpresponse") == 0) {
		/* it's a file containing the complete response to send back */
		
		/* close the file */
		CLOSE_FUNCTION(h_file);
		
		p_the_response = build_response_from_file(c_filename, b_include_body);

		if (!p_the_response)  /* if had an error - respond appropriate error iluhttp_NoContent? */
			return  build_simple_response(iluhttp_NoContent, NULL, 0);

		/* return the response the file contained */
		return p_the_response;
	}


	 /* build up our response */
	p_the_response = (iluhttp_Response*) ilu_malloc(sizeof(iluhttp_Response));	/* create empty response */

	p_the_response->status = iluhttp_OK;	/* set it to indicate success */ 

	/* create space for headers */
	p_the_response->headers._buffer = (iluhttp_Header*) ilu_malloc(5*sizeof(iluhttp_Header));
	p_the_response->headers._maximum = 5;
	p_the_response->headers._length = 5;
	p_header = p_the_response->headers._buffer;

	p_header->name = nglib_duplicate_c_string("Date");		/* make a General header containing Date */
	p_header->value = nglib_get_current_time_string();
	p_header++;

	p_header->name = nglib_duplicate_c_string("Server");			/* create Server header */
	p_header->value = nglib_duplicate_c_string("HTTPNG-Object-Server/1.0");
	p_header++;

	p_header->name = nglib_duplicate_c_string("Content-Length");	/* create Content-Length header */
	sprintf(c_buffer, "%ld", stat_struct.st_size);
	p_header->value = nglib_duplicate_c_string(c_buffer);
	p_header++;

	p_header->name = nglib_duplicate_c_string("Last-Modified");	/* create Last-Modified header */
	p_header->value = nglib_get_time_string(&(stat_struct.st_mtime));
	p_header++;

	if ((pc_extension != NULL) &&		/* create Content-Type header if known */
		(pc_type_string = nglib_content_type_from_extension(pc_extension)) != NULL) {
		p_header->name = nglib_duplicate_c_string("Content-Type");
		p_header->value = nglib_duplicate_c_string(pc_type_string);
	}
	else {	/* forget about content-type - we don't know */
		p_the_response->headers._maximum = 4;
		p_the_response->headers._length = 4;
	}

	if (b_include_body) { /* if we should add the body */
		/* get a buffer of the file contents */
		pc_body_bytes = ilu_malloc(stat_struct.st_size);
		READ_FUNCTION(h_file, pc_body_bytes, stat_struct.st_size);

		/* and stuff on our body bytes */
		p_the_response->body = (iluhttp_OptionalEntityBody) ilu_malloc (sizeof(iluhttp_EntityBody));
		p_the_response->body->_buffer = pc_body_bytes;
		p_the_response->body->_length = stat_struct.st_size;
		p_the_response->body->_maximum = stat_struct.st_size;
	}
	else p_the_response->body = NULL;

	/* close the file */
	CLOSE_FUNCTION(h_file);

	/* return our generated response */
	return p_the_response;
}



/* ********************************************************* */
/* GET                                                       */

iluhttp_Response* server_iluhttp_Resource_GET (iluhttp_Resource http_obj, 
												  iluhttp_Request* p_http_req, 
												  ILU_C_ENVIRONMENT* ilu_env) {

	/* log appropriately */
	if (DO_LOG_BASIC()) {
		LOG_ENTER();
		g_i_request_count++;
		fprintf(g_p_logfile, "\n------------------------------------------\n");
		fprintf(g_p_logfile, "GET called on %s\n", p_http_req->URI);
		if (DO_LOG_FULL()) {
			nglib_print_iluhttp_request (p_http_req, g_p_logfile);
		}
		LOG_EXIT();
	}
	else if (DO_LOG_SUMMARY()) {
		LOG_ENTER();
		g_i_request_count++;
		LOG_EXIT();
	}

	if (g_batcher) /* ensure batching if appropriate */
		ILU_C_SetBatcherContext(g_batcher); 

	return server_iluhttp_Resource_GETorHEAD(http_obj, p_http_req, ilu_TRUE, ilu_env);
}


/* ********************************************************* */
/* HEAD                                                      */

iluhttp_Response* server_iluhttp_Resource_HEAD (iluhttp_Resource http_obj, 
												   iluhttp_Request* p_http_req, 
												   ILU_C_ENVIRONMENT* ilu_env) {
	/* log appropriately */
	if (DO_LOG_BASIC()) {
		LOG_ENTER();
		fprintf(g_p_logfile, "\n------------------------------------------\n");
		fprintf(g_p_logfile, "HEAD called on %s\n", p_http_req->URI);
		if (DO_LOG_FULL()) {
			nglib_print_iluhttp_request (p_http_req, g_p_logfile);
		}
		LOG_EXIT();
	}

	if (g_batcher) /* ensure batching if appropriate */
		ILU_C_SetBatcherContext(g_batcher); 

	return server_iluhttp_Resource_GETorHEAD(http_obj, p_http_req, ilu_FALSE, ilu_env);
}


/* ********************************************************* */
/* POST                                                      */

iluhttp_Response* server_iluhttp_Resource_POST (iluhttp_Resource http_obj, 
												   iluhttp_Request* p_http_req, 
												   ILU_C_ENVIRONMENT* ilu_env) {
	char c_filename[2048];			/* holds the name of of the referenced file */
	int h_file;						/* for obtaining file information */
	STAT_STRUCT stat_struct;
	char* pc_walker;
	char* pc_extension;				/* points to file extension */
	iluhttp_Response* p_the_response;	/* for building up our response */
	
	/* log appropriately */
	if (DO_LOG_FULL()) {
		LOG_ENTER();
		fprintf(g_p_logfile, "\n------------------------------------------\n");
		fprintf(g_p_logfile, "POST called on %s\n", p_http_req->URI);
		nglib_print_iluhttp_request (p_http_req, g_p_logfile);	
		LOG_EXIT();
	}

	if (g_batcher) /* ensure batching if appropriate */
		ILU_C_SetBatcherContext(g_batcher); 

	/* make up the full file name, defaulting to the default file name if necessary */
	if (strcmp(p_http_req->URI, "/") == 0) /*  */
		sprintf(c_filename, "%s%s", g_pc_file_base, DEFAULT_SERVER_FILE_NAME);
	else
		sprintf(c_filename, "%s%s", g_pc_file_base, p_http_req->URI);

	/* terminate properly if params or queries are present*/
	pc_walker = c_filename;
	while (*pc_walker && (*pc_walker != ';') && (*pc_walker != '?'))
		pc_walker++;
	*pc_walker = '\0';

	if (nglib_validate_path(c_filename, g_pc_file_base) == ilu_FALSE) {
		/* return not found if we've gone outside our root */
		return build_simple_response(iluhttp_NotFound, NULL, 0);
	}

	/* open up the file */
	h_file = OPEN_FUNCTION(c_filename, RDONLY_MODE); 
	if ((h_file == -1) || (FSTAT_FUNCTION(h_file, &stat_struct) == -1)) {
		/* return not found if there's a problem opening the file */
		return build_simple_response(iluhttp_NotFound, NULL, 0);
	}
	
	/* close the file */
	CLOSE_FUNCTION(h_file);

	/* determine file extension */
	pc_extension = c_filename + strlen(c_filename) - 1;
	while (pc_extension > c_filename) {
		if (*pc_extension == '.') {
			pc_extension++;
			break;
		}
		pc_extension--;
	}

	if (pc_extension == c_filename)  /* ensure we didn't walk all the way back */
		pc_extension = NULL;

	if (pc_extension && strcmp(pc_extension, "iluhttpresponse") == 0) {
		/* it's a file containing the complete response to send back */
		
		p_the_response = build_response_from_file(c_filename, ilu_TRUE);

		if (!p_the_response)  /* if had an error - respond appropriate error iluhttp_NoContent? */
			return  build_simple_response(iluhttp_NoContent, NULL, 0);

		/* return the response the file contained */
		return p_the_response;
	}


	return build_simple_response(iluhttp_NotImplemented, "POST not implemented", 0);
}

