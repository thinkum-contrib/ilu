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

$Id: nilsrvr.c,v 1.8 1999/08/03 01:59:36 janssen Exp $
*/
/*
 * nilsrvr.c -- An example server program illustrating use of NIL mechanism
 *		of the GSS API
 *
 * Antony Courtney,	8/5/95
 */

#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <string.h>
#include <sys/time.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <signal.h>
#include <sys/stat.h>
#include <assert.h>

#include <gssapi.h>
#include <ilugssmech_nil.h>
#include <ilugssns_rfc822.h>
#include <ilugss_ext.h>

/* we shouldn't have this here, but we do: */
#include "ilugss_util.h"	/* for ilugss_malloc */
#include "ilugss_debug.h"

static char *_ilugss_strerror(OM_uint32 status_code)
{
     static char err_string[2048];
     gss_buffer_desc errbuf;
     OM_uint32 minor_code,msg_context=0;
     OM_uint32 display_status_result;
	 display_status_result = gss_display_status(&minor_code,status_code,
					  GSS_C_GSS_CODE,NULL,
					  &msg_context,
					  &errbuf);
     assert(!GSS_ERROR(display_status_result));
     assert(errbuf.length < 2048);
     memcpy(err_string,errbuf.value,errbuf.length);
     ilugss_free (errbuf.value);

     return err_string;
}

/* N.B. The following macro evaluates its arguments multiple times! */
#define ERRCHK(rc,ms)	if (GSS_ERROR(rc)) { \
			   fprintf(stderr,"Unexpected GSS error: \"%s\", line %d: \n\t%s\n\t(Calling Error==%d, Routine Error==%d, Supp. Info==0x%x, \n\tMinor Status==%d)\n", \
				   __FILE__,__LINE__, \
				   _ilugss_strerror(rc), \
				   GSS_CALLING_ERROR(rc), \
				   GSS_ROUTINE_ERROR(rc), \
				   GSS_SUPPLEMENTARY_INFO(rc),ms); \
			   exit(1); \
			}
#define TRUE 1
#define FALSE 0

#define SRVRNAME "nilserver@holmes.parc.xerox.com"

/* namespace to test: */
#define NAMESPACE ilugssns_rfc822_OID

/* send_buffer() -- write a buffer on an fd */
static void send_buffer(int fd,gss_buffer_t bufp)
{
     if (bufp->length==0) {
	  return;
     }
     /* for now: just write length, followed by buffer contents */
     if (write(fd,&(bufp->length),sizeof(size_t)) < 0) {
	  perror("write");
	  exit(1);
     }
     if (write(fd,bufp->value,bufp->length) < 0) {
	  perror("write");
	  exit(1);
     }
}

/* read_buffer() -- read a single buffer off the given fd into caller
 * allocated storage
 */
/* returns 0 on eof or error */
int read_buffer(int fd,gss_buffer_t bufp)
{
     int nbytes;

     /* read length first */
     if (read(fd,&(bufp->length),sizeof(size_t)) <= 0) {
	  return 0;
     }
     bufp->value = ilugss_malloc(bufp->length);
     if (read(fd,bufp->value,bufp->length) <= 0) {
	  return 0;
     }
}
     
/* read_file_buf() -- read given file into given freshly allocated buffer */
void read_file_buf(char *path,gss_buffer_t buf)
{
     struct stat stbuf;
     FILE *fp;

     if (stat(path,&stbuf) < 0) {
	  perror("stat");
	  exit(1);
     }
     buf->length=stbuf.st_size;
     buf->value=ilugss_malloc(buf->length);

     if ((fp=fopen(path,"r"))==NULL) {
	  perror("fopen");
	  exit(1);
     }	

     if (fread(buf->value,1,buf->length,fp) < buf->length) {
	  perror("fread");
	  exit(1);
     }
     fclose(fp);
}

void srvr_handshake(int fd,gss_ctx_id_t *context_handle)
{
     int first_call=1;
     gss_name_t server_name, client_name;
     OM_uint32 retcode,retcode2,minor_status,minor_status2;
     gss_cred_id_t cred_handle = GSS_C_NO_CREDENTIAL;
     gss_buffer_desc cert_buf,key_buf;
     gss_buffer_desc namebuf,input_buffer,output_buffer;
     gss_OID namespace;

     /* import server name into namespace */
     namebuf.length=sizeof(SRVRNAME);
     namebuf.value=SRVRNAME;
     retcode=gss_import_name(&minor_status,&namebuf,NAMESPACE,&server_name);
     ERRCHK(retcode,minor_status);

     input_buffer.length=0;
     input_buffer.value=NULL;

     do {
	  if (input_buffer.value!=NULL) {
	       ilugss_free (input_buffer.value);
	       input_buffer.value=NULL;
	  }
	  read_buffer(fd,&input_buffer);

	  retcode=gss_accept_sec_context(&minor_status,context_handle,
					 cred_handle,
					 &input_buffer,
					 GSS_C_NO_CHANNEL_BINDINGS,
					 &client_name, NULL, &output_buffer,
					 NULL, NULL, NULL);
	  /* N.B. we check output buffer length _before_ checking error
	   * code!
	   */
	  if (output_buffer.length > 0) {
	       send_buffer(fd,&output_buffer);
	  }
	  ERRCHK(retcode,minor_status);
     } while (GSS_SUPPLEMENTARY_INFO(retcode) & GSS_S_CONTINUE_NEEDED);
     retcode = gss_display_name (&minor_status, client_name, &namebuf, &namespace);
     ERRCHK(retcode,minor_status);
     retcode = gss_oid_to_str (&minor_status, namespace, &output_buffer);
     ERRCHK(retcode,minor_status);
     printf ("client is %*.*s:%*.*s\n",
	     output_buffer.length, output_buffer.length, output_buffer.value,
	     namebuf.length, namebuf.length, namebuf.value);     
}

static void echo_loop(int sockfd,gss_ctx_id_t context_handle)
{
     OM_uint32 retcode,minor_status;
     gss_buffer_desc input_buffer,output_buffer;

     while (1) {
	  read_buffer(sockfd,&input_buffer);
	  /* unwrap it */
	  retcode=gss_unwrap(&minor_status,context_handle,&input_buffer,
			     &output_buffer,NULL,NULL);
	  ERRCHK(retcode,minor_status);
	  input_buffer=output_buffer;
	  /* wrap it back up */
	  retcode=gss_wrap(&minor_status,context_handle,1,1,&input_buffer,NULL,
			   &output_buffer);
	  ERRCHK(retcode,minor_status);
	  send_buffer(sockfd,&output_buffer);

	  retcode=gss_release_buffer(&minor_status,&input_buffer);
	  ERRCHK(retcode,minor_status);

	  retcode=gss_release_buffer(&minor_status,&output_buffer);
	  ERRCHK(retcode,minor_status);
     }
}


#define BUFSIZE 256

int main(int argc,char *argv[])
{
     int servfd, clifd;	/* server and client fd's */
     struct sockaddr_in serv_addr, cli_addr;
     int clilen, servlen;
     gss_ctx_id_t context_handle=GSS_C_NO_CONTEXT;
     char buf[BUFSIZE];

     /* ignore SIGPIPE */
     signal(SIGPIPE,SIG_IGN);

     /* create a server socket */
     if ((servfd=socket(AF_INET,SOCK_STREAM,0)) < 0) {
	  perror("socket");
	  exit(1);
     }

     /* bind our local address so */
     memset(&serv_addr,0,sizeof(serv_addr));
     serv_addr.sin_family=AF_INET;
     serv_addr.sin_addr.s_addr=htonl(INADDR_ANY);
     serv_addr.sin_port=0;

     if (bind(servfd,(struct sockaddr *) &serv_addr,sizeof(serv_addr)) < 0) {
	  perror("bind");
	  exit(1);
     }
     
     /* find out which port we actually bound to */
     servlen=sizeof(serv_addr);
     if (getsockname(servfd,(struct sockaddr *) &serv_addr,
		     &servlen) < 0) {
	  perror("getsockname");
	  exit(1);
     }
     printf("server bound to port %d\n", ntohs(serv_addr.sin_port));
     
     listen(servfd,5);
     
     while (1) {
	  clilen=sizeof(cli_addr);

	  if ((clifd=accept(servfd,(struct sockaddr *) &cli_addr,
			    &clilen)) < 0) {
	       perror("accept");
	       return 1;
	  }
	  printf("server accepted connection...\n");

	  srvr_handshake(clifd,&context_handle);
	  printf("server handshake successful!\n");

	  echo_loop(clifd,context_handle);
     }
}
     
