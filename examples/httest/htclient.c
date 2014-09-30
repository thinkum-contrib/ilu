/** $Id: htclient.c,v 1.14 1999/08/03 01:58:23 janssen Exp $
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

/* simple brute force client program used to test the http protocol 

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

/* ********************************************************* */
/* Utility                                                   */

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


/* printout response */
void print_response (iluhttp_Response* p_the_response) {

	iluhttp_Header* p_the_header;			/* used to point to headers */
	unsigned long ul_index;				/* used as index into sequences */

	printf("Response: (Note all values are shown between >< s)\n");

	if (p_the_response == NIL) {
		printf("NIL\n");
		return;
	}

	printf("Status = >%d<\n", p_the_response->status);	/* show status */ 

	p_the_header = p_the_response->headers._buffer; /* show the headers */
	printf("Number of headers = >%lu<\n", p_the_response->headers._length);
	for (ul_index = 0; 
		 ul_index < p_the_response->headers._length; 
		 ul_index++, p_the_header++) {

		printf("Header %lu\n\tfield-name = >%s<", ul_index, p_the_header->name);
		printf("\n\toptional-field-value = >%s<\n", (p_the_header->value ? p_the_header->value : "NIL"));
	}

	/* show the entity body - assume it's all printable */
	printf("Body is:\n>");
	if (p_the_response->body == NIL)
		printf("NIL");
	else 
		for (ul_index = 0; ul_index < p_the_response->body->_length; ul_index++)
			 printf("%hc", p_the_response->body->_buffer[ul_index]);
	printf("<\n");
}


/* show an exception if we have one */
ilu_boolean show_if_exception (CORBA_Environment* p_ilu_env, char* pc_situation) {

	if (p_ilu_env->returnCode == NIL) 
		return ilu_FALSE;		/* just return false on no exception set */

	/* show exception if we got one */
	fprintf (stderr, "\nException: %s\n", pc_situation);

	switch (p_ilu_env->_major) {

		case CORBA_NO_EXCEPTION:		
			fprintf (stderr, "\t_major = CORBA_NO_EXCEPTION\n"); 
			break;

		case CORBA_USER_EXCEPTION:		
			fprintf (stderr, "\t_major = CORBA_USER_EXCEPTION\n"); 
			break;

		case CORBA_SYSTEM_EXCEPTION:	
			fprintf (stderr, "\t_major = CORBA_SYSTEM_EXCEPTION\n"); 
			break;

		default:	
			fprintf (stderr, "\t_major = ??? Unknown _major !! ???\n"); 
			break;
	}

	fprintf (stderr, "\treturnCode = %s\n", p_ilu_env->returnCode);

	/* we know about specifics of flipcase */
	if (strcmp("ilu:httest.FLIPEXCEP", p_ilu_env->returnCode) == 0)
		fprintf (stderr, "\t**ptr = %d\n", *((ilu_integer*)(p_ilu_env->ptr)));
	else 
		fprintf(stderr, "\tptr to ???\n");

	return ilu_TRUE;
}


/* ********************************************************* */
/* make the calls on a Resource object                       */

void do_resource_test (char* pc_url, iluhttp_Request* p_http_req, char* pc_hostname) {

	CORBA_Environment ilu_env;					/* used to get potential error info */
	iluhttp_Resource http_obj;						/* will be an 'object' from an existing httpd */
	iluhttp_Response* p_http_resp;					/* will hold the response returned from http__obj */

	if (strcmp("lookup", pc_url) == 0) {
		/* we should get the object via an ILU Lookup */

		char pc_serverid[1024];						/* gets set to id of server program */
		
		/* construct the id for our base server */
		sprintf (pc_serverid, "httpbase.%s.parc.xerox.com", pc_hostname);

		http_obj = ILU_C_LookupObject (pc_serverid, "/http_obj0", iluhttp_Resource__MSType);
	}
	else {
		/* get an object to represent pc_url by parsing the pc_url */
		http_obj = ILU_C_SBHToObject (pc_url, iluhttp_Resource__MSType, &ilu_env);
	}
	

	if (http_obj == NIL) {
		fprintf (stderr, "Error: Can't obtain iluhttp_Resource object %s\n", pc_url);
		exit(1);
	}
	
	printf ("iluhttp_Resource object obtained, SBH is %s\n\n", ILU_C_SBHOfObject(http_obj));

	p_http_req->URI = pc_url;			/* assign the the URI in the request */ 
	
	printf("---------------- Resource Test --------------------\n");

	print_request(p_http_req);

	printf("\n---------------------------------------------------");
	printf("\nCalling GET on http_obj ---------------------------\n");

	/* call the GET method on the http_derived_obj */
	p_http_resp = iluhttp_Resource_GET (http_obj, p_http_req, &ilu_env);

	if (show_if_exception(&ilu_env, "calling httest_Resource_GET"))
		exit(1);
	
	/* show the response and free its associated storage */
	print_response(p_http_resp);
	iluhttp_Response__Free (p_http_resp);
	ilu_free(p_http_resp);

	printf("\n----------------------------------------------------");
	printf("\nCalling HEAD on http_obj ---------------------------\n");

	/* call the HEAD method on the http_derived_obj */
	p_http_resp = iluhttp_Resource_HEAD (http_obj, p_http_req, &ilu_env);

	if (show_if_exception(&ilu_env, "calling httest_Resource_HEAD"))
		exit(1);
	
	/* show the response and free its associated storage */
	print_response(p_http_resp);
	iluhttp_Response__Free (p_http_resp);
	ilu_free(p_http_resp);

	printf("\n----------------------------------------------------");
	printf("\nCalling POST on http_obj ---------------------------\n");

	/* call the POST method on the http_derived_obj */
	p_http_resp = iluhttp_Resource_POST (http_obj, p_http_req, &ilu_env);

	if (show_if_exception(&ilu_env, "calling httest_Resource_POST"))
		exit(1);
	
	/* show the response and free its associated storage */
	print_response(p_http_resp);
	iluhttp_Response__Free (p_http_resp);
	ilu_free(p_http_resp);

}


/* ********************************************************* */
/* make the calls on a Derived Resource object               */

void do_derived_resource_test (iluhttp_Request* p_http_req, char* pc_hostname, 
							   char* pc_string_to_flip) {

	CORBA_Environment ilu_env;					/* used to get potential error info */
	httest_DerivedResource http_derived_obj;	/* will be an ilu object from htserver test program */
	iluhttp_Response* p_http_resp;					/* will hold the response returned from http__obj */
	ilu_CString ilu_str_flipped;				/* will get the returned flipped case string */
	char pc_serverid[1024];						/* gets set to id of server program */

	/* construct the id for the server of the httest_DerivedResource*/
	sprintf (pc_serverid, "httpderived.%s.parc.xerox.com", pc_hostname);

	/* lookup the httest_DerivedResource, instance id "/httpderived_obj0" */
	http_derived_obj = ILU_C_LookupObject (pc_serverid, "/httpderived_obj0", httest_DerivedResource__MSType);
	if (http_derived_obj == NIL) {
		fprintf (stderr, "Error:  Can't obtain httest_DerivedResource object <%s>\n", pc_serverid);
		exit(1);
	}


	p_http_req->URI = "/httpderived_obj0"; /* set the request uri appropriately */
	printf("\n------------ Derived Resource Test ---------------\n");

	print_request(p_http_req);

	printf("\n-------------------------------------------------");
	printf("\nCalling GET  on http_derived_obj ----------------\n");

	/* call the GET method on the http_derived_obj */
	p_http_resp = iluhttp_Resource_GET (http_derived_obj, p_http_req, &ilu_env);

	if (show_if_exception(&ilu_env, "calling httest_Resource_GET"))
		exit(1);
	
	/* show the response and free its associated storage */
	print_response(p_http_resp);
	iluhttp_Response__Free (p_http_resp);
	ilu_free(p_http_resp);

	printf("\n--------------------------------------------------");
	printf("\nCalling HEAD  on http_derived_obj ----------------\n");

	/* call the HEAD method on the http_derived_obj */
	p_http_resp = iluhttp_Resource_HEAD (http_derived_obj, p_http_req, &ilu_env);

	if (show_if_exception(&ilu_env, "calling httest_Resource_HEAD"))
		exit(1);
	
	/* show the response and free its associated storage */
	print_response(p_http_resp);
	iluhttp_Response__Free (p_http_resp);
	ilu_free(p_http_resp);

	printf("\n----------------------------------------------------");
	printf("\nCalling POST on http_derived_obj -------------------\n");

	/* call the POST method on the http_derived_obj */
	p_http_resp = iluhttp_Resource_POST (http_derived_obj, p_http_req, &ilu_env);

	if (show_if_exception(&ilu_env, "calling httest_Resource_POST"))
		exit(1);
	
	/* show the response and free its associated storage */
	print_response(p_http_resp);
	iluhttp_Response__Free (p_http_resp);
	ilu_free(p_http_resp);

	printf("\n-----------------------------------------------------");
	printf("\nCalling flipcase on http_derived_obj ----------------\n");

	/* call the flipcase method on the http_derived_obj */
	ilu_str_flipped = httest_DerivedResource_flipcase (http_derived_obj, pc_string_to_flip, &ilu_env);

	if (!show_if_exception(&ilu_env, "calling httest_DerivedResource_flipcase"))
		printf("\n\tFlipped string is %s\n", ilu_str_flipped);

}


/* ********************************************************* */
/* Usage string                                              */

char g_c_usage[] = 
"Usage:  htclient HttpURL [[string_to_flipcase] [ HOSTNAME ]]\n\n\
\tHttpURL - URL of object to try Get Head and Post on.\n\
\tIf it's the word NIL, then the http_Resource test is skipped.\n\n\
\tIf it's the word lookup, then an ILU Lookup is used to find\n\
\tthe object instead of parsing the URL\n\n\
\tstring_to_flipcase - a string to have the httest_DerivedResource\n\
\tflip the case of.  If it's \"raiseerror\" then a user\n\
\texception containing the length of string_to_flipcase\n\
\t (i.e. 10 is the length of \"raiseerror\" will \n\
\tbe generated and returned.\n\n\
\tHOSTNAME - used as part of the id for the server\n\
\tof the httest_DerivedResource - defaults to this host";


/* ********************************************************* */
/* MAIN                                                      */


int main(int ac, char **av) {

	char pc_hostname[1024];			/* holds the name of the host the server is on */
	iluhttp_Request http_req;			/* will be a Request to pass to http objects */

	if ((ac > 4) || (ac < 2)) {		/* check for proper command line args */
		fprintf (stderr, "%s\n", g_c_usage);
		exit(1);
	}

	/* was needed for dynamic protocol addition 
	   before http was built into kernel
	setup_http_protocol();	*/		/* setup to use http protocol */

	iluhttp__Initialize();				/* perform required initialization */
	httest__Initialize( );

	if (ac > 3)				/* get hostname */
		strcpy (pc_hostname, av[3]);
	else
		gethostname(pc_hostname, sizeof(pc_hostname));

	/* create a request based on av[1] */ 

	/* make some headers */
	http_req.headers._maximum = 1;						
	http_req.headers._length = 1;
	http_req.headers._buffer = (iluhttp_HTTPHeader*) ilu_malloc ( 1 * sizeof(iluhttp_HTTPHeader));
	http_req.headers._buffer[0].name = "User-Agent";
	http_req.headers._buffer[0].value = "ILU-HTTP-Object-Client/1.0";

	/*
	http_req.headers._buffer[1].name = "Date";
	http_req.headers._buffer[1].value = get_time_string();
	http_req.headers._buffer[2].name = "Host";
	http_req.headers._buffer[2].value = "pundit.parc.xerox.com";
	*/

	/* Warning: note that if a body is sent in a Get request to a http proxy server, 
	  (at least the one in use here at parc), no response comes back! */

	/* used for testing only to force use of a particular proxy server */
	/* _putenv("ILU_HTTP_PROXY_INFO=wwwproxy.parc.xerox.com:8000"); */
	

	if (getenv("ILU_HTTP_PROXY_INFO") != NIL)
		http_req.body = NIL; /* no body */
	else {
		/* make a simple body */ 
		http_req.body = (iluhttp_OptionalEntityBody) ilu_malloc (sizeof(iluhttp_EntityBody));
		http_req.body->_buffer = (ilu_bytes)("Sample Request Body Bytes");
		http_req.body->_length = strlen((char*)(http_req.body->_buffer));
		http_req.body->_maximum = http_req.body->_length;
	}

	/* note that ilu's http will put in Content-Length header if an
	Entity body is supplied, and we haven't put in our own content length header */


	if (strcmp("NIL", av[1]) != 0) /* if non NIL URL, do the resource test */
		do_resource_test (av[1], &http_req, pc_hostname);


	/* ---------------------------------- */
	/* now try calls on a derived object */

	if (ac <= 2)	/* if we were not given a string to flipcase on */
		return 0;

	do_derived_resource_test (&http_req, pc_hostname, av[2]);

	return 0;
}



/* ********************************************************* */
/* End of file                                               */
/* ********************************************************* */
