/** $Id: webserver.c,v 1.17 1999/08/03 01:58:24 janssen Exp $
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

/* very simple brute force web server program

  Dan Larner, larner@parc.xerox.com
  4-4-96
  */

/* ********************************************************* */
/* includes and defines                                      */
/* ********************************************************* */

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
#define CLOSE_FUNCTION _close
#define RDONLY_MODE _O_RDONLY
#else
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
#define CLOSE_FUNCTION close
#define RDONLY_MODE O_RDONLY
#endif


/* include internals header (for NIL) */
#include <iluntrnl.h>

#ifndef HAVE_GETHOSTNAME_PROTOTYPE
extern int gethostname(char *name, int namelen);
#endif /* ndef HAVE_GETHOSTNAME_PROTOTYPE */


/* include header(s) defining the interfaces */
#include "iluhttp.h"


#define ILU_TEST_DOMAIN "parc.xerox.com"	/* string used in our ilu server name */

/* the file name we use if just a slash is specified in the URL */
#define DEFAULT_SERVER_FILE_NAME "/index.html"	

/* ********************************************************* */
/* some globals                                              */
/* ********************************************************* */

ilu_boolean g_b_verbose = ilu_FALSE;		/* whether to output info */

char gc_file_base[1024];					/* base location of files */

char g_c_usage[] =							/* Usage string */
"Usage:  webserver [port_number [file_base [pinfo [verbose] ] ] ]\n\n\
\tport_number - tcp port the webserver should use - default 80\n\
\tfile_base - Root location of where server files are stored - default \\htmldocs \n\n\
\tpinfo - one of http_1_0 (default), or http_1_0p (persistent connections)\n\
\tverbose - if anthing there as 2nd arg, the program\n\
\toutputs information about what's going on.";



/* ********************************************************* */
/* file extension to content-type mapping                    */
/* ********************************************************* */


typedef struct {		/* pairs a file name extension with its MIME type */
	char* pc_file_extension;
	char* pc_content_type;
}	extension_type_pair;


/* all the file extension to MIME types we know about */

extension_type_pair g_extension_type_pairs[] = 

{
	{	"html",			"text/html"	},
	{	"htm",			"text/html"							},
	{	"txt",			"text/plain"						},
	{	"text",			"text/plain"						},

	{	"gif",			"image/gif"							},
	{	"jpeg",			"image/jpeg"						},
	{	"jpg",			"image/jpeg"						},
	{	"jpe",			"image/jpeg"						},
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

	{	"wav",			"audio/x-wav"						},
	{	"au",			"audio/basic"						},
	{	"snd",			"audio/basic"						},
	{	"aif",			"audio/x-aiff"						},
	{	"aiff",			"audio/x-aiff"						},
	{	"aifc",			"audio/x-aiff"						},

	{	"ai",			"application/postscript"			},
	{	"eps",			"application/postscript"			},
	{	"ps",			"application/postscript"			},
	{	"exe",			"application/octet-stream"			},
	{	"bin",			"application/octet-stream"			},
	{	"zip",			"application/x-zip-compressed"		},
	{	"gz",			"application/x-gzip"				},
	{	"Z",			"application/x-compress"			},
	{	"pac",			"application/x-ns-proxy-autoconfig"	},
	{	"js",			"application/x-javascript"			},
	{	"ls",			"application/x-javascript"			},
	{	"mocha",		"application/x-javascript"			},
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

	{	"mpeg",			"video/mpeg"						},
	{	"mpg",			"video/mpeg"						},
	{	"mpe",			"video/mpeg"						},
	{	"qt",			"video/quicktime"					},
	{	"mov",			"video/quicktime"					},
	{	"avi",			"video/x-msvideo"					},

	{	NULL,			NULL								}
};

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
/* returns the appropriate Content-Type string for the given 
   file extension                                            */

char* content_type_from_extension (char* pc_extension) {

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
/* object table functions                                    */
/* ********************************************************* */

static CORBA_Object object_of_instance_handle (ilu_string str_object_id, 
											   ilu_private p_iluserver) {

	/* simply return a new true object with this id  */
	return (iluhttp_Resource__OTCreateTrue (str_object_id,
				       *((ILU_C_Server *) p_iluserver),
				       NIL)); 
}


static void free_object_table_storage (ilu_private p_iluserver) {
  return; /* nothing to do */
}



/* ********************************************************* */
/* main                                                      */
/* ********************************************************* */

int main(int ac, char **av) {

	char pc_hostname[1024];						/* holds the name of the host the server is on */
	char pc_serverid[1024];						/* holds the server id */
	ILU_C_Server webserver;						/* server for http web objects */
	char* pc_protocol_info_default = "http_1_0p";		/* denotes use of http (with persistence) as the protocol */
	char* pc_protocol_info = pc_protocol_info_default;
	ILU_C_ObjectTable object_table;				/* will be our object table */

	/* webserver defaults to run at port 80 on top of tcp/ip */
	char* ppc_base_transport_info[2] = { "tcp_0_80", ILU_NIL };
	char  c_alternative_tcp_port_info[64];
	
	if (ac > 5) {			/* check for proper command line args */
		fprintf (stderr, "%s\n", g_c_usage);
		exit(1);
	}

	/* take care of command line arguments apporpriately */
	if (ac > 1) { /* port number supplied */
		sprintf(c_alternative_tcp_port_info, "tcp_0_%s", av[1]);
		ppc_base_transport_info[0] = c_alternative_tcp_port_info;
	}

	if (ac > 2) { /* file base specified */
		strcpy(gc_file_base, av[2]);
	}
	else strcpy(gc_file_base, "\\htmldocs");

	if (ac > 3) /* pinfo */
		pc_protocol_info = av[3];

	gethostname (pc_hostname, sizeof(pc_hostname));

	if (ac > 4)
		g_b_verbose = ilu_TRUE;


	iluhttp__InitializeServer();	/* perform server initialization */

	/* make up an object table we can use to create up objects representing
	the resources (usually documents stored in files) on the fly */
	object_table = 
		ILU_C_CreateObjectTable (object_of_instance_handle, 
			free_object_table_storage, (ilu_private) &webserver);

	/* create a server id for our web server, and create the webserver itself */
	sprintf (pc_serverid, "webserver.%s.%s", pc_hostname, ILU_TEST_DOMAIN);
	webserver = 
		ILU_C_InitializeServer (
			pc_serverid,				/* how our server is identified */
			object_table,				/* use our object table */
			pc_protocol_info,			/* this will be http */
			ppc_base_transport_info,	/* this is over tcp/ip */
			(ilu_Passport) ILU_NIL,
			ilu_FALSE					/* don't make it the default port */
			);

	if (webserver == ILU_NIL) {
		fprintf (stderr, "Couldn't create web server\n");
		exit(1);
	}

	if (g_b_verbose)
		printf("web server created\n");

	/* run the server */
	ILU_C_Run( );

	return 1;
}


/* ********************************************************* */
/* Utility Functions                                         */
/* ********************************************************* */

/* path validation
   Returns ilu_TRUE if canonicalized pc_pathname (sans the drive letter 
   on windows) begins with pc_prefix 
   Assumes that pc_pathname has at least one directory separator in it. 
   Trys to rely on getcwd to provide pathname canonicalization */
static ilu_boolean validate_path (char* pc_pathname, char* pc_prefix) {

	char c_working_dir_save[2048];
	char c_new_working_dir[2048];
	char* pc_pathname_copy = duplicate_c_string(pc_pathname);
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

	GETCWD_FUNTION(c_working_dir_save, 2048); /* save where we currently are */

	CHDIR_FUNTION(pc_pathname_copy);		 /* go to the new location */
	GETCWD_FUNTION(c_new_working_dir, 2048); /* get where we are now */

#if (defined (WIN32) || defined(WIN16))
	/* ignore drive letter on windows systems */
	if (strstr(c_new_working_dir + 2, pc_prefix) != c_new_working_dir + 2) {
#else
	if (strstr(c_new_working_dir, pc_prefix) != c_new_working_dir) {
#endif
	if (g_b_verbose)
		printf ("Validation failure: directory %s of %s - not rooted at %s\n", 
			c_new_working_dir, pc_pathname, pc_prefix);
		b_result = ilu_FALSE;
	}

	free(pc_pathname_copy);

	/* restore to where we were originally */
	CHDIR_FUNTION(c_working_dir_save);

	return b_result;
}



/* ********************************************************* */
/* return a char* containing the current UTC time in asctime format */

char* get_time_string (time_t* p_timer) {
	struct tm* p_newtime;
	char* pc_timestring;

	p_newtime = gmtime( p_timer );			/* convert to UTC structure */
	pc_timestring = asctime( p_newtime); /* get char string representation */
	pc_timestring[strlen(pc_timestring) - 1] = '\0';	/* knock off \n */
	return duplicate_c_string(pc_timestring);
}


/* ********************************************************* */
/* return a char* containing the current UTC time in asctime format */

char* get_current_time_string () {
	time_t ltime;
	time( &ltime );						/* get the time */
	return get_time_string( &ltime );	/* convert to UTC string */
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

		p_date_header->name = duplicate_c_string("Date");				/* make a General header containing Date */
		p_date_header->value = get_current_time_string();

		p_length_header = p_date_header + 1;				/* create Content-length header */

		p_length_header->name = duplicate_c_string("Content-length");
		sprintf(c_buffer, "%lu", card_body_length);
		p_length_header->value = duplicate_c_string(c_buffer);

		p_the_response->headers._maximum = 2;				/* put our header into the header sequence */
		p_the_response->headers._length = 2;
		p_the_response->headers._buffer = p_date_header;
	}
	else { /* let ilu http auto generate content-length header */

		p_date_header = (iluhttp_Header*) ilu_malloc(sizeof(iluhttp_Header));		/* create headers */

		p_date_header->name = duplicate_c_string("Date");				/* make a General header containing Date */
		p_date_header->value = get_current_time_string();

		p_the_response->headers._maximum = 1;				/* put our header into the header sequence */
		p_the_response->headers._length = 1;
		p_the_response->headers._buffer = p_date_header;
	}


	if (pc_body_contents != NIL) {
		p_the_response->body = (iluhttp_OptionalEntityBody) ilu_malloc (sizeof(iluhttp_EntityBody));
		p_the_response->body->_buffer = (ilu_bytes) duplicate_c_string(pc_body_contents);
		p_the_response->body->_length = strlen(pc_body_contents);
		p_the_response->body->_maximum = p_the_response->body->_length;
	}
	else 
		p_the_response->body = NIL;

	
	return p_the_response;
}


/* ********************************************************* */
/* printout a request                                        */

void print_request (iluhttp_Request* p_the_request) {

	iluhttp_Header* p_the_header;			/* used to point to headers */
	unsigned long ul_index;				/* used as index into sequences */

	printf("Request: (Note all values are shown between >< s)\n");

	if (p_the_request == NIL) {
		printf("NIL\n");
		return;
	}

	printf("URI = >%s<\n", p_the_request->URI);	/* show URI */ 

	p_the_header = p_the_request->headers._buffer; /* show the headers */
	printf("Number of headers = >%lu<\n", p_the_request->headers._length);
	for (ul_index = 0; 
		 ul_index < p_the_request->headers._length; 
		 ul_index++, p_the_header++) {

		printf("Header %lu\n\tfield-name = >%s<", ul_index, p_the_header->name);
		printf("\n\toptional-field-value = >%s<\n", (p_the_header->value ? p_the_header->value : "NIL"));
	}

	/* show the entity body - assume it's all printable */
	printf("Body is:\n>");
	if (p_the_request->body == NIL)
		printf("NIL");
	else 
		for (ul_index = 0; ul_index < p_the_request->body->_length; ul_index++)
			 printf("%hc", p_the_request->body->_buffer[ul_index]);
	printf("<\n");
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
		p_header->name = duplicate_c_string(c_header_name);
		p_header->value = pc_value ? duplicate_c_string(c_header_value) : NULL;
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
	else p_the_response->body = NIL;
	
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
		sprintf(c_filename, "%s%s", gc_file_base, DEFAULT_SERVER_FILE_NAME);
	else
		sprintf(c_filename, "%s%s", gc_file_base, p_http_req->URI);

	/* terminate properly if params or queries are present*/
	pc_walker = c_filename;
	while (*pc_walker && (*pc_walker != ';') && (*pc_walker != '?'))
		pc_walker++;
	*pc_walker = '\0';

	if (validate_path(c_filename, gc_file_base) == ilu_FALSE) {
		/* return not found if we've gone outside our root */
		return build_simple_response(iluhttp_NotFound, NIL, 0);
	}

	/* open up the file */
	h_file = OPEN_FUNCTION(c_filename, RDONLY_MODE); 
	if ((h_file == -1) || (FSTAT_FUNCTION(h_file, &stat_struct) == -1)) {
		/* return not found if there's a problem opening the file */
		return build_simple_response(iluhttp_NotFound, NIL, 0);
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
			return  build_simple_response(iluhttp_NoContent, NIL, 0);

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

	p_header->name = duplicate_c_string("Date");		/* make a General header containing Date */
	p_header->value = get_current_time_string();
	p_header++;

	p_header->name = duplicate_c_string("Server");			/* create Server header */
	p_header->value = duplicate_c_string("ILU-HTTP-Object-Server/1.0");
	p_header++;

	p_header->name = duplicate_c_string("Content-Length");	/* create Content-Length header */
	sprintf(c_buffer, "%ld", stat_struct.st_size);
	p_header->value = duplicate_c_string(c_buffer);
	p_header++;

	p_header->name = duplicate_c_string("Last-Modified");	/* create Last-Modified header */
	p_header->value = get_time_string(&(stat_struct.st_mtime));
	p_header++;

	if ((pc_extension != NULL) &&		/* create Content-Type header if known */
		(pc_type_string = content_type_from_extension(pc_extension)) != NULL) {
		p_header->name = duplicate_c_string("Content-Type");
		p_header->value = duplicate_c_string(pc_type_string);
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
	else p_the_response->body = NIL;

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

	if (g_b_verbose) {
		printf("\n------------------------------------------\n");
		printf("GET called on %s\n", p_http_req->URI);
		print_request (p_http_req);
	}

	return server_iluhttp_Resource_GETorHEAD(http_obj, p_http_req, ilu_TRUE, ilu_env);
}


/* ********************************************************* */
/* HEAD                                                      */

iluhttp_Response* server_iluhttp_Resource_HEAD (iluhttp_Resource http_obj, 
												   iluhttp_Request* p_http_req, 
												   ILU_C_ENVIRONMENT* ilu_env) {
	if (g_b_verbose) {
		printf("\n------------------------------------------\n");
		printf("HEAD called on %s\n", p_http_req->URI);
		print_request (p_http_req);
	}

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
	
	if (g_b_verbose) {
		printf("\n------------------------------------------\n");
		printf("POST called on %s\n", p_http_req->URI);
		print_request (p_http_req);
	}
	/* make up the full file name, defaulting to the default file name if necessary */
	if (strcmp(p_http_req->URI, "/") == 0) /*  */
		sprintf(c_filename, "%s%s", gc_file_base, DEFAULT_SERVER_FILE_NAME);
	else
		sprintf(c_filename, "%s%s", gc_file_base, p_http_req->URI);

	/* terminate properly if params or queries are present*/
	pc_walker = c_filename;
	while (*pc_walker && (*pc_walker != ';') && (*pc_walker != '?'))
		pc_walker++;
	*pc_walker = '\0';

	if (validate_path(c_filename, gc_file_base) == ilu_FALSE) {
		/* return not found if we've gone outside our root */
		return build_simple_response(iluhttp_NotFound, NIL, 0);
	}

	/* open up the file */
	h_file = OPEN_FUNCTION(c_filename, RDONLY_MODE); 
	if ((h_file == -1) || (FSTAT_FUNCTION(h_file, &stat_struct) == -1)) {
		/* return not found if there's a problem opening the file */
		return build_simple_response(iluhttp_NotFound, NIL, 0);
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
			return  build_simple_response(iluhttp_NoContent, NIL, 0);

		/* return the response the file contained */
		return p_the_response;
	}


	return build_simple_response(iluhttp_NotImplemented, "POST not implemented", 0);
}



/* ********************************************************* */
/* end of file */
/* ********************************************************* */
