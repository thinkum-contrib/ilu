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

$Id: rfc822_ns.c,v 1.14 1999/08/03 01:59:34 janssen Exp $
*/
/*
 * rfc822_ns.c -- implementation of rfc 822 as a GSS namespace
 *
 * Antony Courtney,	14/7/95
 */

#include <gssapi.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>

#include "ilugss_opaque.h"
#include "ilugss_namespace.h"
#include "ilugss_oidtbl.h"
#include "ilugssns_rfc822.h"
#include "ilugss_util.h"
#include "ilugss_impl.h"

gss_OID_desc ilugssns_rfc822_OID[] = { ILUGSSNS_RFC822_OID };

static OM_uint32 import_name(OM_uint32 *minor_status,
			     gss_buffer_t input_name_buffer,
			     gss_opaque_t *output_name);

static OM_uint32 display_name(OM_uint32 *minor_status,
			      gss_opaque_t input_name,
			      gss_buffer_t output_name_buffer);

static OM_uint32 compare_name(OM_uint32 *minor_status,
			      gss_opaque_t name1,
			      gss_opaque_t name2,
			      int *name_equal);

static OM_uint32 release_name(OM_uint32 *minor_status,
			      gss_opaque_t input_name);

static gss_namespace_t namespace={
     ilugssns_rfc822_OID,
     compare_name,
     display_name,
     import_name,
     release_name
};

/* initialize routine: */
void ilugssns_rfc822_initialize(void)
{
  ilugss_register_namespace (&namespace, "rfc822");
}

static OM_uint32 display_name(OM_uint32 *minor_status,
			      gss_opaque_t input_name,
			      gss_buffer_t output_name_buffer)
{
     ilugssns_rfc822_name_t *true_name=(ilugssns_rfc822_name_t *) input_name;
     int displen=0;

/* maximum extra characters inserted during formatting: */
#define MAX_FMT_CHARS 32

     /* ensure that there is adequate space in output buffer */
     displen += true_name->rname==NULL ? 0 : strlen(true_name->rname);
     assert(true_name->uname!=NULL);
     displen += strlen(true_name->uname);
     displen += true_name->dname==NULL ? 0 : strlen(true_name->dname);

     output_name_buffer->value=ilugss_malloc(displen + MAX_FMT_CHARS);

     if (true_name->rname!=NULL) {
	  strcpy(output_name_buffer->value,true_name->rname);
	  strcat(output_name_buffer->value," ");
     } else {
	  *((char *) output_name_buffer->value)='\0';
     }
     strcat(output_name_buffer->value,"<");
     strcat(output_name_buffer->value,true_name->uname);
     if (true_name->dname!=NULL) {
	  strcat(output_name_buffer->value,"@");
	  strcat(output_name_buffer->value,true_name->dname);
     }
     strcat(output_name_buffer->value,">");

     output_name_buffer->length=strlen(output_name_buffer->value);
     return GSS_S_COMPLETE;
}

static OM_uint32 compare_name(OM_uint32 *minor_status,
			      gss_opaque_t name1,
			      gss_opaque_t name2,
			      int *name_equal)
{
     ilugssns_rfc822_name_t *ni1=(ilugssns_rfc822_name_t *) name1,
	  *ni2=(ilugssns_rfc822_name_t *) name2;
     
     /* we only look at uname and dname fields */

     /* compare domain names */
     if (ni1->dname!=ni2->dname) {
	  /* is one of them NULL? */
	  if ((ni1->dname==NULL) || (ni2->dname==NULL)) {
	       *name_equal=0;
	       return GSS_S_COMPLETE;
	  }
	  if (strcmp(ni1->dname,ni2->dname)!=0) {
	       *name_equal=0;
	       return GSS_S_COMPLETE;
	  }
     }
     
     /* domain names are eq, now check user-name: */
     /* neither uname is permitted to be NULL */
     if (strcmp(ni1->uname,ni2->uname)!=0) {
	  *name_equal=0;
	  return GSS_S_COMPLETE;
     }

     *name_equal=1;
     return GSS_S_COMPLETE;
}
	  
/* some string utility functions */
static int has_no_delims(char *s)
{
     return (strcspn(s,"<>()")==strlen(s));
}

static int has_space(char *s)
{
     while ((!isspace(*s)) && (*s!='\0')) {
	  *s++;
     }
     if (isspace(*s)) {
	  return 1;
     }
     return 0;
}

/* delete trailing whitespace from a string */
static void del_wsp(char *s)
{
     char *start=s;

     if (strlen(s) < 1)
	  return;

     for (s+=strlen(s)-1; s >= start && isspace(*s); s--)
	  ;
     *(s+1)='\0';
}

/* import_name -- parse an RFC-822 name, and store it in an internal
 * canonical form
 */
static OM_uint32 import_name(OM_uint32 *minor_status,
			     gss_buffer_t input_name_buffer,
			     gss_opaque_t *output_name)
{
     ilugssns_rfc822_name_t *np;
     char *atp, *addrp, *namep, *delimp;
     char *namebuf;
     
     namebuf=ilugss_malloc(input_name_buffer->length + 1);
     memcpy(namebuf,input_name_buffer->value,input_name_buffer->length);
     namebuf[input_name_buffer->length]='\0';

     /* For purposes of illustration, we'll just take a limited form of
      * RFC-822 names
      */
     /* check for just an email address */
     if (has_no_delims(namebuf)) {
	  /* ensure that there is no whitespace in name */
	  if (has_space(namebuf)) {
	       ilugss_free (namebuf);
	       *minor_status=ILUGSSNS_RFC822_INVALID_SPACE;
	       return GSS_S_BAD_NAME;
	  }
	  addrp=namebuf;
	  namep=NULL;
     } else {
	  /* So we either have xxx <address> or address (xxx) */
	  if ((addrp=strchr(namebuf,'<'))!=NULL) {
	       namep=namebuf;
	       *addrp++='\0';
	       if ((delimp=strchr(addrp,'>'))==NULL) {
		    ilugss_free (namebuf);
		    *minor_status=ILUGSSNS_RFC822_NO_RDELIM;
		    return GSS_S_BAD_NAME;
	       }
	       *delimp='\0';
	  } else {
	       addrp=namebuf;
	       while (isspace(*addrp)) {
		    addrp++;
	       }
	       if ((namep=strchr(addrp,'('))==NULL) {
		    ilugss_free (namebuf);
		    *minor_status=ILUGSSNS_RFC822_MALFORMED;
		    return GSS_S_BAD_NAME;
	       }
	       *namep++='\0';
	       if ((delimp=strchr(namep,')'))==NULL) {
		    ilugss_free (namebuf);
		    *minor_status=ILUGSSNS_RFC822_NO_RDELIM;
	       }
	       *delimp='\0';
	  }
     }
     /* at this point, addrp points to our email address, namep to real
      * name
      */
     np=ilugss_malloc(sizeof(ilugssns_rfc822_name_t));
     
     /* split email address into user and domain name parts */
     if ((atp=strchr(addrp,'@'))==NULL) {
	  /* no domain part */
	  np->dname=NULL;
     } else {
	  /* has a domain part: */
	  *atp++='\0';
	  np->dname=ilugss_strdup(atp);

	  /* delete any trailing whitespace: */
	  del_wsp(np->dname);
     }

     np->uname=ilugss_strdup(addrp);
     if (namep!=NULL) {
	  np->rname=ilugss_strdup(namep);

	  /* delete any trailing whitespace: */
	  del_wsp(np->rname);
     } else {
	  np->rname=NULL;
     }
     
     ilugss_free(namebuf);
     *output_name=np;
     return GSS_S_COMPLETE;
}

static OM_uint32 release_name(OM_uint32 *minor_status,
			      gss_opaque_t input_name)
{
  ilugssns_rfc822_name_t *true_name=(ilugssns_rfc822_name_t *) input_name;
  int displen=0;
  if (true_name->rname != NULL)
    ilugss_free (true_name->rname);
  if (true_name->uname != NULL)
    ilugss_free (true_name->uname);
  if (true_name->dname != NULL)
    ilugss_free (true_name->dname);
  ilugss_free (true_name);
  *minor_status = 0;
  return GSS_S_COMPLETE;
}

