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

$Id: ilugss_oidtbl.h,v 1.6 1999/08/03 01:59:38 janssen Exp $
*/
#ifndef OIDTBL_H
#define OIDTBL_H 1
/*
 * oidtbl.h -- implementation of OID tables -- mappings between OIDs and
 *	       (void *)s
 *
 * Antony Courtney,	23/6/95
 */

/* gss_oidtbl_t -- OID table abstract type */
typedef void *gss_oidtbl_t;

/* _ilugss_oidtbl_create() -- create an OID table, mapping keys (OIDs) to (void *)s
 */
gss_oidtbl_t _ilugss_oidtbl_create(void);

/* _ilugss_oidtbl_insert() -- insert a mapping into an OID table */
void _ilugss_oidtbl_insert(gss_oidtbl_t tbl,gss_OID key,void *val);

/* _ilugss_oidtbl_find() -- lookup a value for a particular OID
 * returns: pointer to which OID is mapped on success, NULL if OID is not in
 *	    table
 */
void *_ilugss_oidtbl_find(gss_oidtbl_t tbl,gss_OID key);

void _ilugss_oidtbl_enumerate(gss_oidtbl_t, void(*)(gss_OID,void *,void *), void *rock);
/* call func on each member of the table with the key value, the data value,
   and the user-specified rock */

int _ilugss_oidtbl_size(gss_oidtbl_t);

/* _ilugss_oidtbl_destroy() -- destroy an OID table */
void _ilugss_oidtbl_destroy(gss_oidtbl_t tbl);

#endif	/* OIDTBL_H */
