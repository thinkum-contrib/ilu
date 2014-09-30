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

$Id: gss_util.c,v 1.12 1999/08/03 01:59:33 janssen Exp $
*/
/*
 * util.c -- implementation of general-purpose utility routines for GSS
 * shell implementation
 */

#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <stdio.h>
#include <gssapi.h>

#include "ilugss_util.h"
#include "ilugss_opaque.h"	/* for name def */

void *ilugss_full_malloc(unsigned long nbytes, char *file, int line)
{
  void *p;

  p=malloc(nbytes);
  assert(p!=NULL);

  return p;
}

void *ilugss_full_realloc(void *p,unsigned long nbytes, char *file, int line)
{
  p=realloc(p,nbytes);
  assert(p!=NULL);

  return p;
}

char *ilugss_full_strdup(char *src, char *file, int line)
{
  char *dst;

  dst=ilugss_full_malloc(strlen(src)+1, file, line);
  strcpy(dst,src);

  return dst;
}

void ilugss_full_free (void *p, char *file, int line)
{
  if (p != NULL)
    free(p);
}

gss_buffer_desc ilugss_full_alloc_buffer(unsigned long size, char *file, int line)
{
     gss_buffer_desc bd;

     bd.length=size;
     bd.value=ilugss_full_malloc(size, file, line);

     return bd;
}

/* ilugss_match_oid(): returns true if both OIDs match */
int ilugss_match_oid(gss_OID oid1,gss_OID oid2)
{
     if (oid1->length!=oid2->length) {
	  return 0;
     }
     if (memcmp(oid1->elements,oid2->elements,oid1->length)!=0) {
	  return 0;
     }
     return 1;
}

int _ilugss_compare_buffers(gss_buffer_t buf1,gss_buffer_t buf2)
{
     /* TODO */
     assert(0);
     return 0;
}

/* ilugss_full_append_buffer() -- append buffer src to dst, and free src */
extern void ilugss_full_append_buffer(gss_buffer_t dst,gss_buffer_t src, char *file, int line)
{
     OM_uint32 origlen;

     origlen=dst->length;

     dst->length += src->length;
     if (origlen==0) {
	  dst->value=ilugss_full_malloc(dst->length, file, line);
     } else {
	  dst->value = ilugss_full_realloc(dst->value,dst->length, file, line);
     }
     memcpy((unsigned char *) dst->value + origlen,src->value,src->length);

     ilugss_full_free (src->value, file, line);
}

extern gss_name_t
  ilugss_full_copy_name (gss_name_t name, char *file, int line)
{
  if (name != NULL)
    {
      gss_name_refcount(name) += 1;
      return name;
    }
  else
    return NULL;
}
