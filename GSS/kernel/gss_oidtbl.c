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

$Id: gss_oidtbl.c,v 1.12 1999/08/03 01:59:31 janssen Exp $
*/
/*
 * oidtbl.c -- braindead implementation of OID tables
 *
 * Antony Courtney, 14/7/95
 */

#include <gssapi.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "ilugss_oidtbl.h"
#include "ilugss_util.h"

typedef struct oid_tbl_entry_s {
     gss_OID key;
     void *val;
} oid_tbl_entry_t;

typedef struct oid_tbl_impl_s {
     int size;	/* number of entries in table */
     int limit;	/* number of entries allocated */
     oid_tbl_entry_t *vec;
} oid_tbl_impl_t;

#define OT_BRKSIZE 8

/* _ilugss_oidtbl_create() -- create an OID table, mapping keys (OIDs) to (void *)s
 */
gss_oidtbl_t _ilugss_oidtbl_create(void)
{
     oid_tbl_impl_t *ti;

     ti=ilugss_malloc(sizeof(oid_tbl_impl_t));
     ti->size=0;
     ti->limit=OT_BRKSIZE;
     ti->vec=ilugss_malloc(sizeof(oid_tbl_entry_t)*ti->limit);

     return ti;
}

/* _ilugss_oidtbl_insert() -- insert a mapping into an OID table */
void _ilugss_oidtbl_insert(gss_oidtbl_t tbl,gss_OID key,void *val)
{
     oid_tbl_impl_t *ti=(oid_tbl_impl_t *) tbl;
     oid_tbl_entry_t *ent;

     assert(ti->size <= ti->limit);
     if (ti->size==ti->limit) {
	  ti->limit += OT_BRKSIZE;
	  ti->vec = ilugss_realloc(ti->vec,
				     sizeof(oid_tbl_entry_t)*ti->limit);
     }
     ent=ti->vec + ti->size;
     ent->key=key;
     ent->val=val;
     ti->size++;
}
     

/* _ilugss_oidtbl_find() -- lookup a value for a particular OID
 * returns: pointer to which OID is mapped on success, NULL if OID is not in
 *	    table
 */
void *_ilugss_oidtbl_find(gss_oidtbl_t tbl,gss_OID key)
{
     oid_tbl_impl_t *ti=(oid_tbl_impl_t *) tbl;
     oid_tbl_entry_t *ent;

     for (ent=ti->vec; ent < ti->vec + ti->size; ent++) {
	  if ((ent->key->length==key->length) &&
	      (memcmp(ent->key->elements,key->elements,key->length)==0)) {
	       return ent->val;
	  }
     }
     return NULL;
}

/* _ilugss_oidtbl_enumerate() -- apply the specified func to each member of the table
 * returns: void
 */
void _ilugss_oidtbl_enumerate(gss_oidtbl_t tbl, void (*func)(gss_OID key, void *data, void *rock), void *rock)
{
     oid_tbl_impl_t *ti=(oid_tbl_impl_t *) tbl;
     oid_tbl_entry_t *ent;

     for (ent=ti->vec; ent < ti->vec + ti->size; ent++) {
       (*func)(ent->key, ent->val, rock);
     }
}

int _ilugss_oidtbl_size(gss_oidtbl_t tbl)
{
     oid_tbl_impl_t *ti=(oid_tbl_impl_t *) tbl;
     return ti->size;
}

/* _ilugss_oidtbl_destroy() -- destroy an OID table */
void _ilugss_oidtbl_destroy(gss_oidtbl_t tbl)
{
     oid_tbl_impl_t *ti=(oid_tbl_impl_t *) tbl;

     ilugss_free (ti->vec);
     ilugss_free (ti);
}
