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

$Id: nilclnt.c,v 1.8 1999/08/03 01:59:35 janssen Exp $
*/
/*
 * nilclnt.c -- An example program illustrating use of the NIL mechanism
 *		of the GSS API
 *
 * Antony Courtney,	8/5/95
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <netdb.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <string.h>
#include <sys/time.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <assert.h>

#include <gssapi.h>
#include <ilugssmech_nil.h>
#include <ilugssns_rfc822.h>
#include <ilugss_ext.h>

/* we shouldn't have this here, but we do: */
#include "ilugss_util.h"	/* for ilugss_malloc */
#include "ilugss_debug.h"


/* my machine's IP address -- change to whatever site you're testing on... */
#define SERV_ADDR "134.226.86.64"

#define BUFSIZE 256

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
#define CLIENTNAME "nilclient@holmes.parc.xerox.com"

/* namespace to test: */
#define NAMESPACE ilugssns_rfc822_OID

static gss_OID_set_desc client_mechs = {
  1, ilugssmech_nil_OID };

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

void cli_handshake(int sockfd,gss_ctx_id_t *context_handle)
{
     int first_call=1;
     gss_name_t remote_name, local_name;
     OM_uint32 retcode,retcode2,minor_status,minor_status2;
     gss_buffer_desc namebuf,output_buffer,input_buffer;
     gss_cred_id_t mycred;
     
     /* import server name into namespace */
     namebuf.length=sizeof(SRVRNAME);
     namebuf.value=SRVRNAME;
     retcode=gss_import_name(&minor_status,&namebuf,NAMESPACE,&remote_name);
     ERRCHK(retcode,minor_status);
     
     /* import client name into namespace */
     namebuf.length = sizeof(CLIENTNAME);
     namebuf.value = CLIENTNAME;
     retcode = gss_import_name(&minor_status, &namebuf, NAMESPACE, &local_name);
     ERRCHK(retcode,minor_status);

     retcode=gss_acquire_cred(&minor_status, local_name, GSS_C_INDEFINITE,
			      &client_mechs, GSS_C_INITIATE, &mycred, NULL, NULL);
     ERRCHK(retcode,minor_status);
     
     input_buffer.length=0;
     input_buffer.value=NULL;
     do {
	  output_buffer.length=0;
	  retcode=gss_init_sec_context(&minor_status,mycred,
				       context_handle,remote_name,ilugssmech_nil_OID,
				       0,0,GSS_C_NO_CHANNEL_BINDINGS,
				       &input_buffer,NULL,
				       &output_buffer,NULL,NULL);
	  if (output_buffer.length > 0) {
	       send_buffer(sockfd,&output_buffer);
	  }
	  /* N.B. error check moved to here, so that gss_init_sec_context()
	   * may place an error message in output_buffer
	   */
	  ERRCHK(retcode,minor_status);
	  if (input_buffer.value!=NULL) {
	       ilugss_free (input_buffer.value);
	       input_buffer.value=NULL;
	  }
	  if (GSS_SUPPLEMENTARY_INFO(retcode) & GSS_S_CONTINUE_NEEDED) {
	       read_buffer(sockfd,&input_buffer);
	  }
     } while (GSS_SUPPLEMENTARY_INFO(retcode) & GSS_S_CONTINUE_NEEDED);
}
				  

static void send_bytes(int sockfd,gss_ctx_id_t context_handle,
		       unsigned char *echobuf,int nbytes)
{
     OM_uint32 retcode,minor_status;
     gss_buffer_desc input_buffer,output_buffer;

     input_buffer.length=nbytes;
     input_buffer.value=echobuf;
     retcode=gss_wrap(&minor_status,
		      context_handle,
		      1, 1,
		      &input_buffer,
		      NULL,
		      &output_buffer);
     ERRCHK(retcode,minor_status);
     send_buffer(sockfd,&output_buffer);

     retcode=gss_release_buffer(&minor_status,&output_buffer);
     ERRCHK(retcode,minor_status);
}


static void client_loop(int sockfd,gss_ctx_id_t context_handle)
{
     OM_uint32 retcode,minor_status;
     unsigned char echobuf[BUFSIZE];
     fd_set readfds;
     gss_buffer_desc input_buffer,output_buffer;
     int nbytes;

     while (1) {
	  FD_ZERO(&readfds);
	  FD_SET(0,&readfds);
	  FD_SET(sockfd,&readfds);

	  if (select(32,&readfds,NULL,NULL,NULL) < 0) {
	       perror("select");
	       exit(1);
	  }
	  if (FD_ISSET(0,&readfds)) {
	       switch (nbytes=read(0,echobuf,BUFSIZE)) {
	       case -1:
		    perror("read");
		    exit(1);
		    break;
	       case 0:
		    exit(0);
	       default:
		    break;
	       }
	       send_bytes(sockfd,context_handle,echobuf,nbytes);
	  }
	  if (FD_ISSET(sockfd,&readfds)) {
	       read_buffer(sockfd,&input_buffer);
	       /* unwrap it and print it... */
	       retcode=gss_unwrap(&minor_status,context_handle,
				  &input_buffer,&output_buffer,NULL,NULL);
	       ERRCHK(retcode,minor_status);
	       write(1,output_buffer.value,output_buffer.length);
	       retcode=gss_release_buffer(&minor_status,&output_buffer);
	       ERRCHK(retcode,minor_status);
	  }
     }
}


int main(int argc,char *argv[])
{
     int sockfd;
     struct sockaddr_in serv_addr;
     struct hostent *hp;
     char hostname[BUFSIZE];
     unsigned short portno;

     /* initialize security context: */
     gss_ctx_id_t context_handle=GSS_C_NO_CONTEXT;

     if (argc < 2) {
	  fprintf(stderr,"usage: %s port [host]\n",argv[0]);
	  exit(1);
     }
     portno=(short) atoi(argv[1]);

     if (argc==3) {
	  strcpy(hostname,argv[2]);
     } else {
	  if (gethostname(hostname,BUFSIZE) < 0) {
	       perror("gethostname");
	       exit(1);
	  }
     }
     if ((hp=gethostbyname(hostname))==NULL) {
	  perror("gethostbyname");
	  exit(1);
     }
     printf("attempting to connect to server %s:%d\n", hostname,portno);

     memset((void *) &serv_addr,0,sizeof(struct sockaddr_in));
     serv_addr.sin_family=AF_INET;
     memcpy((void *) &(serv_addr.sin_addr),hp->h_addr,hp->h_length);
     serv_addr.sin_port=htons(portno);

     if ((sockfd=socket(AF_INET,SOCK_STREAM,0)) < 0) {
	  perror("socket");
	  exit(1);
     }
     
     /* connect to server */
     if (connect(sockfd,(struct sockaddr *) &serv_addr,
		 sizeof(serv_addr)) < 0) {
	  perror("connect");
	  exit(1);
     }

     cli_handshake(sockfd,&context_handle);
     printf("client handshake succesfull!!\n\n");

     client_loop(sockfd,context_handle);
     return 0;
}
     
