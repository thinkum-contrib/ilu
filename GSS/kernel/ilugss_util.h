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

$Id: ilugss_util.h,v 1.7 1999/08/03 01:59:34 janssen Exp $
*/
#ifndef ILUGSS_UTIL_H
#define ILUGSS_UTIL_H 1

/*
 * util.h -- utility routines for GSS API implementation
 */

extern void *ilugss_full_malloc(unsigned long nbytes, char *file, int line);
#define ilugss_malloc(x)	ilugss_full_malloc((x),__FILE__,__LINE__)

extern void *ilugss_full_realloc(void *p,unsigned long nbytes, char *file, int line);
#define ilugss_realloc(x1,x2)	ilugss_full_realloc((x1),(x2),__FILE__,__LINE__)

extern char *ilugss_full_strdup(char *src, char *file, int line);
#define ilugss_strdup(x)	ilugss_full_strdup((x),__FILE__,__LINE__)

extern void  ilugss_full_free(void *p, char *file, int line);
#define ilugss_free(x)		ilugss_full_free((x),__FILE__,__LINE__)

extern gss_buffer_desc ilugss_full_alloc_buffer(unsigned long size, char *file, int line);
#define ilugss_alloc_buffer(x)	ilugss_full_alloc_buffer((x),__FILE__,__LINE__)

extern int _ilugss_compare_buffers(gss_buffer_t buf1,gss_buffer_t buf2);

/* ilugss_append_buffer() -- append buffer src to dst, and free src */
extern void ilugss_full_append_buffer(gss_buffer_t dst,gss_buffer_t src, char *file, int line);
#define ilugss_append_buffer(x,y)	ilugss_full_append_buffer((x),(y),__FILE__,__LINE__)

/* ilugss_match_oid(): returns true if both OIDs match */
extern int ilugss_match_oid(gss_OID oid1,gss_OID oid2);

extern gss_name_t ilugss_full_copy_name (gss_name_t, char *file, int line);
#define ilugss_copy_name(x)	ilugss_full_copy_name((x),__FILE__,__LINE__)
/* returns NULL if some error occurs */

#endif /* ndef ILUGSS_UTIL_H */
