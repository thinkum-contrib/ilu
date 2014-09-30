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

$Id: nstest.c,v 1.10 1999/08/03 01:59:36 janssen Exp $
*/
/*
 * nstest -- test of GSS namespaces using RFC822 names
 *
 * Antony Courtney,	13/7/95
 */

#include <stdlib.h>
#include <stdio.h>
#include <gssapi.h>
#include <assert.h>
#include <string.h>

#include "gssapi.h"
#include "ilugss_util.h"

#include "ilugssns_rfc822.h"

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
				   _ilugss_strerror(rc),\
				   GSS_CALLING_ERROR(rc), \
				   GSS_ROUTINE_ERROR(rc), \
				   GSS_SUPPLEMENTARY_INFO(rc),ms); \
			   exit(1); \
			}

/* some name strings: */
#define NSTR1	"Antony Courtney <antony@parc.xerox.com>"
#define NSTR2	"antony@parc.xerox.com (Antony Courtney)"
#define NSTR3	"Bill_Janssen@acm.org (Bill Janssen)"

/* namespace to test: */
#define NAMESPACE ilugssns_rfc822_OID

#define STRBUFSIZE	256

main(int argc,char *argv[])
{
     gss_name_t n1, n2, n3;
     gss_buffer_desc buf1, buf2, buf3;
     gss_buffer_t bp1, bp2, bp3;
     char s1[STRBUFSIZE], s2[STRBUFSIZE], s3[STRBUFSIZE];
     gss_OID result_oid;
     OM_uint32 retcode,minor_status;
     int cmp_val;

     
     /* import all three names */
     printf("test 1: gss_import_name():\n");
     buf1.length=sizeof(NSTR1);
     buf1.value=NSTR1;
     retcode=gss_import_name(&minor_status,&buf1,NAMESPACE,&n1);
     ERRCHK(retcode,minor_status);
     printf("test 1: passed.\n\n");

     printf("test 2: gss_import_name():\n");
     buf2.length=sizeof(NSTR2);
     buf2.value=NSTR2;
     retcode=gss_import_name(&minor_status,&buf2,NAMESPACE,&n2);
     ERRCHK(retcode,minor_status);
     printf("test 2: passed.\n\n");

     buf3.length=sizeof(NSTR3);
     buf3.value=NSTR3;
     retcode=gss_import_name(&minor_status,&buf3,NAMESPACE,&n3);
     ERRCHK(retcode,minor_status);

     printf("test 3: gss_display_name():\n");
     buf1.length=STRBUFSIZE;
     buf1.value=s1;
     retcode=gss_display_name(&minor_status,n1,&buf1,&result_oid);
     ERRCHK(retcode,minor_status);
     printf("original name: [%s]\n\tcanonical name: [%s]\n",
	    NSTR1,buf1.value);
     printf("test 3: passed.\n\n");

     printf("test 4: gss_display_name():\n");
     buf2.length=STRBUFSIZE;
     buf2.value=s2;
     retcode=gss_display_name(&minor_status,n2,&buf2,&result_oid);
     ERRCHK(retcode,minor_status);
     printf("original name: [%s]\n\tcanonical name: [%s]\n",
	    NSTR2,buf2.value);
     printf("test 4: passed.\n\n");

     printf("test 5: gss_display_name():\n");
     buf3.length=STRBUFSIZE;
     buf3.value=s3;
     retcode=gss_display_name(&minor_status,n3,&buf3,&result_oid);
     ERRCHK(retcode,minor_status);
     printf("original name: [%s]\n\tcanonical name: [%s]\n",
	    NSTR3,buf3.value);
     printf("test 5: passed.\n\n");

     printf("test 6: gss_compare_name(): equality test:\n");
     retcode=gss_compare_name(&minor_status,n1,n2,&cmp_val);
     ERRCHK(retcode,minor_status);
     if (!cmp_val) {
	  printf("test 6: names should have compared equal, but didn't!\n");
	  exit(1);
     }
     printf("test 6: passed.\n\n");

     printf("test 7: gss_compare_name(): inequality test:\n");
     retcode=gss_compare_name(&minor_status,n1,n3,&cmp_val);
     ERRCHK(retcode,minor_status);
     if (cmp_val) {
	  printf("test 7: gss_compare_name() returned TRUE for non-equivalent names!\n");
	  exit(1);
     }
     printf("test 7: passed.\n\n");

     printf("%s: all tests passed.\n",argv[0]);
     return 0;
}
