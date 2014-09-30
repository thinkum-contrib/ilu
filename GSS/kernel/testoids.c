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

$Id: testoids.c,v 1.4 1999/08/03 01:59:36 janssen Exp $
*/
/*
 * encode_oids.c -- utility program to generate DER encoded ASN.1 OIDs from
 * ISOs dotted decimal notation
 *
 * Antony Courtney, 8/22/95
 */

#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>

#include <gssapi.h> /* just for OM_uint32 */

#define BUFSIZE 256
#define MAXOID	32	/* maximum number of numbers in an OID */

void encode_oid(gss_OID oid,OM_uint32 oidvec[],OM_uint32 vecsize)
{
     unsigned char *oidp=oid->elements;
     OM_uint32 v_n;
     unsigned char b128_rep[BUFSIZE];
     int ndigits;
     int i,j;

/* TAKE_SIX() -- extract the least-significant six digits from a word */
#define TAKE_SEVEN(v)	(v & 0x7f)

     oid->length=0;
     assert(vecsize >= 2);

     /* do the stupid trick for encoding first 2 values */
     assert(oidvec[0] <= 2);
     oidp[oid->length++]=(unsigned char) (oidvec[0]*40 + oidvec[1]);

     /* now encode the rest of the values... */
     for (i=2; i < vecsize; i++) {
	  v_n = oidvec[i];

	  /* construct the base-128 representation, starting with the
	   * least-significant digits
	   */
	  assert(v_n!=0);
	  ndigits=0;
	  while (v_n > 0) {
	       b128_rep[ndigits++]=TAKE_SEVEN(v_n);
	       v_n = v_n >> 7;
	  }

	  /* now place digits in oidp in reverse order */
	  for (j=ndigits-1; j > 0; j--) {
	       oidp[oid->length++]=0x80 | b128_rep[j];
	  }

	  /* deal with last base-128 digit -- leave high-order bit off */
	  oidp[oid->length++]=b128_rep[0];
     }
}

void print_oid(gss_OID oid)
{
     unsigned char *oidp=oid->elements;
     int i;

     printf("{ %d, \"",oid->length);
     for (i=0; i < oid->length; i++) {
	  printf("\\x%02x",oidp[i]);
     }
     printf("\" }\n");
}

int main(int argc,char *argv[])
{
     char strbuf[BUFSIZE];
     OM_uint32 oidvec[MAXOID];
     OM_uint32 oidvecsize;
     unsigned char enc_oid_buf[BUFSIZE];
     gss_OID oid = NULL;
     gss_buffer_desc buf;
     OM_uint32 stat, minor;

     while (fgets(strbuf,BUFSIZE,stdin)!=NULL) {
	  if ((strbuf[0]!='#') && (strlen(strbuf) > 1)) {
	       buf.length = strlen(strbuf) - 1;
	       buf.value = (void *) strbuf;
	       stat = gss_str_to_oid (&minor, &buf, &oid);
	       printf ("gss_str_to_oid returns %u, %u\n", stat, minor);
	       print_oid (oid);
	       stat = gss_oid_to_str (&minor, oid, &buf);
	       printf ("gss_oid_to_str returns %u, %u\n", stat, minor);
	       printf ("string form of OID is \"%*.*s\"\n", buf.length, buf.length, buf.value);
	       ilugss_free (oid);
	  }
     }
}
 
