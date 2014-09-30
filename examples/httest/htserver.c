/** $Id: htserver.c,v 1.15 1999/08/03 01:58:22 janssen Exp $
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
/* Last edited by Mike Spreitzer October 8, 1998 11:06 pm PDT */

/* simple brute force server program used to test the http protocol 

  Dan Larner, larner@parc.xerox.com
  4-4-96
  */

#include <stdio.h>
#include <time.h>

/* pick up gethostname */
#if (defined WIN32 || defined WIN16)
#include <winsock.h>
#else
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <ctype.h>
#include <string.h>
#ifndef HAVE_GETHOSTNAME_PROTOTYPE
extern int gethostname(char *name, int namelen);
#endif /* ndef HAVE_GETHOSTNAME_PROTOTYPE */
extern char *duplicate_c_string(char *);
#endif

/* include internals header (for NIL) */
#include <iluntrnl.h>

/* include header(s) defining the interfaces */
#include "httest.h"

/* include functions to deal with http protocol */
/* was needed for dynamic protocol addition 
   before http was built into kernel
   #include <httpprot.h>
   */

#define ILU_TEST_DOMAIN "parc.xerox.com"
ilu_boolean g_b_verbose = ilu_FALSE;



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
/* create a http_Resource                           */

iluhttp_Resource create_http_resource (ILU_C_Server httest_server) {

	iluhttp_Resource http_obj;		/* an ilu object we implement */
	ilu_string ilustr_sbh;		/* holds an SBH */

	/* create the actual ('true') object that implements the object's methods */
	http_obj =
		iluhttp_Resource__CreateTrue ( 
			"/http_obj0",			/* give it an instance id of /http_obj0 */
			httest_server,			/* let it be served by the server that was passed */
			NIL						/* no user specified data to associate with it */
			);

	if (http_obj == NIL) {
		fprintf (stderr, "Couldn't create true http_Resource object\n");
		exit(1);
    }
	
	if (g_b_verbose)
		printf("%s created\n", "/http_obj");
	
	/* if we were successful in creating the true object */
	/* let the world know of it's existence */
	if (! ILU_C_PublishObject (http_obj)) {
		fprintf (stderr, "Couldn't publish http_Resource object\n");
		exit(1);
	}

	if (g_b_verbose)
		printf("%s published\n", "/http_obj");

	/* get the string binding handle for the object */
	ilustr_sbh = ILU_C_SBHOfObject(http_obj);

	if (g_b_verbose)
		printf("%s SBH obtained\n", "/http_obj");

	/* show the sbh and then release its storage*/
	printf ("Exported %s\n", ilustr_sbh);
	ilu_free(ilustr_sbh);

	/* return the new object */
	return http_obj;
}


/* ********************************************************* */
/* create a httest_DerivedResource                           */

httest_DerivedResource create_derived_resource (ILU_C_Server httest_server) {

	httest_DerivedResource httest_derived_obj;	/* an ilu object we implement */
	ilu_string ilustr_sbh;						/* holds an SBH */

	/* create the actual ('true') object that implements the object's methods */
	httest_derived_obj =
		httest_DerivedResource__CreateTrue ( 
			"/httpderived_obj0",	/* give it an instance id of /httpderived_obj0 */
			httest_server,			/* let it be served by the server that was passed */
			NIL						/* no user specified data to associate with it */
			);

	if (httest_derived_obj == NIL) {
		fprintf (stderr, "Couldn't create true httest_DerivedResource object\n");
		exit(1);
    }
	
	if (g_b_verbose)
		printf("%s created\n", "httest_derived_obj");
	
	/* if we were successful in creating the true object */
	/* let the world know of it's existence */
	if (! ILU_C_PublishObject (httest_derived_obj)) {
		fprintf (stderr, "Couldn't publish httest_DerivedResource object\n");
		exit(1);
	}

	if (g_b_verbose)
		printf("%s published\n", "httest_derived_obj");

	/* get the string binding handle for the object */
	ilustr_sbh = ILU_C_SBHOfObject(httest_derived_obj);

	if (g_b_verbose)
		printf("%s SBH obtained\n", "httest_derived_obj");

	/* show the sbh and then release its storage*/
	printf ("Exported %s\n", ilustr_sbh);
	ilu_free(ilustr_sbh);

	/* return the new object */
	return httest_derived_obj;
}


/* ********************************************************* */
/* Usage string                                              */

char g_c_usage[] = 
"Usage:  htserver [port_number [pinfo [ HOSTNAME [verbose] ] ] ]\n\n\
\tport_number - tcp port the base_server should use - default 80\n\
\tpinfo - one of http_1_0 (default), or http_1_0p (persistent connections)\n\
\tHOSTNAME - used as part of the id for the server\n\
\tof the httest_DerivedResource - defaults to this host.\n\n\
\tverbose - if anthing there as 4th arg, the program\n\
\t\toutputs information about what's going on.";


/* ********************************************************* */
/* main                                                      */

int main(int ac, char **av) {

	char pc_hostname[1024];						/* holds the name of the host the server is on */
	char pc_serverid[1024];						/* holds the server id */
	ILU_C_Server httest_base_server;				/* server for http obj */
	ILU_C_Server httest_derived_server;			/* server for httest_derived_obj */
	iluhttp_Resource http_obj;				/* an ilu object we implement */
	httest_DerivedResource httest_derived_obj;	/* an ilu object we implement */
	char* pc_protocol_info_default = "http_1_0";		/* denotes use of http as the protocol */
	char* pc_protocol_info = pc_protocol_info_default;

	/* http for base defaults to run at port 80 on top of tcp/ip */
	char* ppc_base_transport_info[2] = { "tcp_0_80", ILU_NIL };
	char  c_alternative_tcp_port_info[64];
	
	/* http for derived uses unspecified port on top of tcp/ip -  */
	char* ppc_derived_transport_info[2] = { "tcp_0_0", ILU_NIL };	

	if (ac > 5) {			/* check for proper command line args */
		fprintf (stderr, "%s\n", g_c_usage);
		exit(1);
	}

	/* was needed for dynamic protocol addition 
	   before http was built into kernel
	setup_http_protocol();	*/		/* setup to use http protocol */

	iluhttp__InitializeServer();	/* perform server initialization */
	httest__InitializeServer();	/* perform server initialization */

	/* take care of command line arguments apporpriately */
	if (ac > 1) { /* port number supplied */
		sprintf(c_alternative_tcp_port_info, "tcp_0_%s", av[1]);
		ppc_base_transport_info[0] = c_alternative_tcp_port_info;
	}

	if (ac > 2) 
		pc_protocol_info = av[2];

	if (ac > 3) 
		strcpy (pc_hostname, av[3]);
	else 
		gethostname (pc_hostname, sizeof(pc_hostname));

	if (ac > 4)
		g_b_verbose = ilu_TRUE;


	/* create a server id for our base server, and create the base server itself */
	sprintf (pc_serverid, "httpbase.%s.%s", pc_hostname, ILU_TEST_DOMAIN);
	httest_base_server = 
		ILU_C_InitializeServer (
			pc_serverid,		/* how our server is identified */
			NIL,				/* use default object hash table */
			pc_protocol_info,	/* this will be http */
			ppc_base_transport_info,	/* this is over tcp/ip */
			(ilu_Passport) ILU_NIL,
			ilu_FALSE			/* dont' make it the default port of the server */
			);

	if (httest_base_server == ILU_NIL) {
		fprintf (stderr, "Couldn't create server for http_Resource\n");
		exit(1);
	}

	if (g_b_verbose)
		printf("%s created\n", "httest_base_server");


	/* create a server id for our derived server, and create the derived server itself */
	sprintf (pc_serverid, "httpderived.%s.%s", pc_hostname, ILU_TEST_DOMAIN);
	httest_derived_server = 
		ILU_C_InitializeServer (
			pc_serverid,		/* how our server is identified */
			NIL,				/* use default object hash table */
			pc_protocol_info,	/* this will be http_1_0 */
			ppc_derived_transport_info,	/* this is over tcp/ip */
			(ilu_Passport) ILU_NIL,
			ilu_TRUE			/* make it the default port or of the server */
			);

	if (httest_derived_server == ILU_NIL) {
		fprintf (stderr, "Couldn't create server for httest_DerivedResource\n");
		exit(1);
	}

	if (g_b_verbose)
		printf("%s created\n", "httest_derived_server");


	/* create and serve up a base and a derived resource object */
	http_obj = create_http_resource (httest_base_server);
	if (http_obj == ILU_NIL)
		exit(1);

	httest_derived_obj = create_derived_resource (httest_derived_server);
	if (httest_derived_obj == ILU_NIL)
		exit(1);

	/* run the server */
	ILU_C_Run( );

	return 1;
}


/* ********************************************************* */
/* Utility Functions                                         */

/* return a char* containing the current UTC time in asctime format */
char* get_time_string () {
	struct tm *newtime;
	long ltime;
	char* pc_timestring;

	time( &ltime );						/* get the time */
	newtime = gmtime( &ltime );			/* convert to UTC structure */
	pc_timestring = asctime( newtime); /* get char string represnentation */
	pc_timestring[strlen(pc_timestring) - 1] = '\0';	/* knock off \n */
	return duplicate_c_string(pc_timestring);
}


/* build up a simple response */
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
		p_date_header->value = get_time_string();

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
		p_date_header->value = get_time_string();

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


/* printout request */
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

iluhttp_Response* server_iluhttp_Resource_GET (iluhttp_Resource http_obj, 
					       iluhttp_Request* p_http_req, 
					       ILU_C_ENVIRONMENT* ilu_env)
{
  if (g_b_verbose) {
    printf("\n------------------------------------------\n");
    printf("%s called\n", "_server_iluhttp_Resource_GET");
    print_request (p_http_req);
  }
  
  return build_simple_response(iluhttp_OK, "server_iluhttp_Resource_GET", 0);
}

iluhttp_Response* server_iluhttp_Resource_HEAD (iluhttp_Resource http_obj, 
						iluhttp_Request* p_http_req, 
						ILU_C_ENVIRONMENT* ilu_env)
{
  if (g_b_verbose) {
    printf("\n------------------------------------------\n");
    printf("%s called\n", "_server_iluhttp_Resource_HEAD");
    print_request (p_http_req);
  }
  
  return build_simple_response(iluhttp_OK, NIL, 24);
}

iluhttp_Response* server_iluhttp_Resource_POST (iluhttp_Resource http_obj, 
						iluhttp_Request* p_http_req, 
						ILU_C_ENVIRONMENT* ilu_env)
{
  if (g_b_verbose) {
    printf("\n------------------------------------------\n");
    printf("%s called\n", "_server_iluhttp_Resource_POST");
    print_request (p_http_req);
  }
  
  return build_simple_response(iluhttp_OK, "server_iluhttp_Resource_POST", 0);
}

/* *********************************************************** */

iluhttp_Response* server_httest_DerivedResource_GET (httest_DerivedResource httest_derived_obj, 
						     iluhttp_Request* p_http_req, 
						     ILU_C_ENVIRONMENT* ilu_env)
{
  if (g_b_verbose) {
    printf("\n------------------------------------------\n");
    printf("%s called\n", "server_httest_DerivedResource_GET");
    print_request (p_http_req);
  }
  
  return build_simple_response(iluhttp_OK, "server_httest_DerivedResource_GET", 0);
}


iluhttp_Response* server_httest_DerivedResource_HEAD (httest_DerivedResource httest_derived_obj, 
						      iluhttp_Request* p_http_req, 
						      ILU_C_ENVIRONMENT* ilu_env)
{
  if (g_b_verbose) {
    printf("\n------------------------------------------\n");
    printf("%s called\n", "server_httest_DerivedResource_HEAD");
    print_request (p_http_req);
  }
  
  return build_simple_response(iluhttp_OK, NIL, 33);
}

iluhttp_Response* server_httest_DerivedResource_POST (httest_DerivedResource httest_derived_obj, 
						      iluhttp_Request* p_http_req, 
						      ILU_C_ENVIRONMENT* ilu_env)
{
  if (g_b_verbose) {
    printf("\n------------------------------------------\n");
    printf("%s called\n", "server_httest_DerivedResource_POST");
    print_request (p_http_req);
  }
  
  return build_simple_response(iluhttp_OK, "server_httest_DerivedResource_POST", 0);
}


ilu_CString server_httest_DerivedResource_flipcase (httest_DerivedResource httest_derived_obj, 
						    ilu_CString ilu_strtoflipcase, 
						    ILU_C_ENVIRONMENT* ilu_env)
{
  char* pc_str_to_return;	/* points to string to return */
  char* pc_orig;		/* walks down the input string */
  char* pc_ret;			/* walks down the return string */
  int i_length;			/* length of the string */

  if (g_b_verbose) {
    printf("\n------------------------------------------\n");
    printf("%s called\n", "server_httest_DerivedResource_flipcase");
  }

  if (strcmp("raiseerror", ilu_strtoflipcase) == 0) {
    /* we should return an exception instead - set to size of string*/
    ilu_env->_major = ILU_C_USER_EXCEPTION;
    ilu_env->returnCode = ex_httest_FLIPEXCEP;
    ilu_env->ptr = (void*) ilu_malloc(sizeof(ilu_integer));
    *((ilu_integer*)(ilu_env->ptr)) = strlen(ilu_strtoflipcase);
    return NIL;
  }

  i_length = strlen(ilu_strtoflipcase);	/* get the length */
  pc_str_to_return = (char*) ilu_malloc(i_length + 1); /* allocate space for the return string */
	
  /* Reverse case of input string into return string */
  pc_ret = pc_str_to_return;
  for( pc_orig = ilu_strtoflipcase; pc_orig < ilu_strtoflipcase + i_length; pc_orig++ ) {
    if( islower( *pc_orig ) )
      *pc_ret = (char)(toupper( *pc_orig ));
    else if( isupper( *pc_orig ) )
      *pc_ret = (char)(tolower( *pc_orig )) ;
    else
      *pc_ret = *pc_orig;
    pc_ret++;
  }

  *pc_ret = '\0';

  return pc_str_to_return;

}



/* ********************************************************* */
/* end of file */
/* ********************************************************* */
