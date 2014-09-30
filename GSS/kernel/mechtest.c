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

$Id: mechtest.c,v 1.9 1999/08/03 01:59:28 janssen Exp $
*/
/*
 * mech_test.c -- test of GSS mechanisms
 *
 * Antony Courtney,   24/7/95
 */

#include <stdio.h>
#include <stdlib.h>
#include <gssapi.h>
#include <signal.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <string.h>
#include <assert.h>
#include <errno.h>

#include <ilugssns_anonymous.h>
#include <ilugssmech_nil.h>
#include <ilugss_util.h>		/* for ilugss_free() */

/* test-case constants: */
#define TEST_MECHANISM ilugssmech_nil_OID
#define DATAFILE "test.data"

#define SBUFSIZE 256

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

static int debug=0;

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

/* initiator() -- create a security context, and send the output token to the
 * acceptor
 */
int initiator(int readfd,int writefd)
{
     OM_uint32 minor_status,retcode;
     gss_ctx_id_t context=GSS_C_NO_CONTEXT;
     gss_buffer_desc data_buffer,output_buffer;
     FILE *fp;
     unsigned char sbuf[SBUFSIZE];
     unsigned long nbytes;
     gss_name_t target_name;
     
     target_name = ilugssns_anonymous_default_name();

     /* create a new security context */
     retcode=gss_init_sec_context(&minor_status,
				  GSS_C_NO_CREDENTIAL,
				  &context,
				  target_name,
				  TEST_MECHANISM,
				  0, 0, GSS_C_NO_CHANNEL_BINDINGS,
				  GSS_C_NO_BUFFER,
				  NULL,
				  &output_buffer,
				  NULL,
				  NULL);
     ERRCHK(retcode,minor_status);
     printf("initiator: gss_init_sec_context() successful.\n");

     /* send over the output token */
     send_buffer(writefd,&output_buffer);

     /* transmit file across pipe, using gss_wrap() to hide the data */
     if ((fp=fopen(DATAFILE,"r"))==NULL) {
	  fprintf(stderr,"error: initiator could not open file %s",DATAFILE);
	  perror("");
	  exit(1);
     }
     while ((nbytes=fread(sbuf,1,SBUFSIZE,fp)) > 0) {
	  data_buffer.length=nbytes;
	  data_buffer.value=sbuf;
	  output_buffer.length=0;
	  /* wrap buffer: */
	  retcode=gss_wrap(&minor_status,context,TRUE,GSS_C_QOP_DEFAULT,
			   &data_buffer,NULL,&output_buffer);
	  ERRCHK(retcode,minor_status);
	  
	  /* send length followed by buffer */
	  send_buffer(writefd,&output_buffer);

	  /* release buffer */
	  retcode=gss_release_buffer(&minor_status,&output_buffer);
	  ERRCHK(retcode,minor_status);
     }
     if (ferror(fp)) {
	  perror("error reading test data file");
	  exit(1);
     }
     printf("initiator: finished writing data, closing connection.\n");
     fclose(fp);
     close(writefd);

     return 0;
}

/* recv_buffer() -- read a single buffer off the given fd into caller
 * allocated storage
 */
/* returns 0 on eof or error */
int recv_buffer(int fd,gss_buffer_t bufp)
{
     int nbytes;

     /* read length first */
     if (read(fd,&(bufp->length),sizeof(size_t)) <= 0) {
       printf ("acceptor:  error reading buffersize, errno=%s\n",
		strerror(errno));
       return 0;
     }
     if (read(fd,bufp->value,bufp->length) <= 0) {
       printf ("acceptor:  error reading buffer, errno=%s\n",
		strerror(errno));
       return 0;
     }
     return bufp->length;
}
     
/* acceptor() -- accept a security context from the initiator, and read/write some
 * data
 */
int acceptor(int readfd,int writefd)
{
     OM_uint32 retcode, minor_status;
     gss_ctx_id_t context=GSS_C_NO_CONTEXT;
     gss_buffer_desc input_buffer,output_buffer;
     unsigned char sbuf[SBUFSIZE];
     char cmdbuf[SBUFSIZE];
     char *outfname;
     int status;
     FILE *fp;
     
     /* accept a security context from remote peer */
     input_buffer.length=0;
     input_buffer.value=sbuf;

     /* read initial context token */
     printf("acceptor: reading initial context token off input buffer.\n");
     if (recv_buffer(readfd,&input_buffer)==0) {
	  fprintf(stderr,"acceptor: unable to read initial context token.\n");
	  exit(1);
     }
     printf("acceptor: initial context token read successfully.\n");
     retcode=gss_accept_sec_context(&minor_status,
				    &context,
				    GSS_C_NO_CREDENTIAL,
				    &input_buffer,
				    GSS_C_NO_CHANNEL_BINDINGS,
				    NULL, NULL, &output_buffer,
				    NULL, NULL, NULL);
     ERRCHK(retcode,minor_status);
     printf("acceptor: gss_accept_sec_context() successful.\n");

     outfname=tmpnam(NULL);
     if ((fp=fopen(outfname,"w"))==NULL) {
	  perror("fopen");
	  exit(1);
     }

     input_buffer.value=sbuf;

     /* continually read records off pipe */
     while (recv_buffer(readfd,&input_buffer)) {
	  retcode=gss_unwrap(&minor_status,context,&input_buffer,
			     &output_buffer,NULL,NULL);
	  ERRCHK(retcode,minor_status);
	  if (fwrite(output_buffer.value,1,output_buffer.length,fp) < 0) {
	       perror("fwrite");
	       exit(1);
	  }
	  retcode=gss_release_buffer(&minor_status,&output_buffer);
	  ERRCHK(retcode,minor_status);
     }
     printf("acceptor: remote side closed connection.\n");
     fclose(fp);
     close(readfd);

     printf("acceptor: comparing original file \"%s\" with output file \"%s\":\n",
	    DATAFILE,outfname);
     sprintf(cmdbuf,"diff %s %s",DATAFILE,outfname);
     status=system(cmdbuf);
     if (status!=0) {
	  fprintf(stderr,"non-zero status code %d from [%s]\n",
		  status,cmdbuf);
	  exit(1);
     }
     printf("acceptor: files are identical, test successful.\n");
     sprintf(cmdbuf,"rm -f %s\n",outfname);
     system(cmdbuf);
     return 0;
}

void parse_args(int argc,char *argv[])
{
     char *optstring;

     while (--argc > 0) {
	  optstring=argv[argc];
	  if (*optstring++=='-') {
	       switch (*optstring) {
	       case 'd':
		    debug++;
		    break;
	       default:
		    fprintf(stderr,"unknown option: -%c\n",*optstring);
		    exit(1);
	       }
	  }
     }
}

int main(int argc,char *argv[])
{
     int downpipe[2] /* initiator -> acceptor */, uppipe[2] /* acceptor -> initiator */;
     int x=1;
     
     parse_args(argc,argv);
     signal(SIGPIPE,SIG_IGN);

     printf ("acceptor is child, initiator is parent\n");

     if (pipe(downpipe) < 0) {
	  perror("pipe");
	  exit(1);
     }
     if (pipe(uppipe) < 0) {
	  perror("pipe");
	  exit(1);
     }

     switch (fork()) {
     case -1:
	  perror("fork");
	  exit(1);
	  break;
     case 0:
	  /* acceptor */
	  if (debug) {
	       while (x==1)
		    ;
	  }
	  close(downpipe[1]);
	  close(uppipe[0]);
	  if (acceptor(downpipe[0],uppipe[1]) < 0) {
	       exit(1);
	  }
	  printf("acceptor completed successfully.\n");
	  break;
     default:
	  /* initiator */
	  close(downpipe[0]);
	  close(uppipe[1]);
	  if (initiator(uppipe[0],downpipe[1]) < 0) {
	       exit(1);
	  }
	  printf("initiator completed succesfully.\n");
	  wait(NULL);
	  printf("initiator: exiting...\n");
	  break;
     }
     return 0;
}
