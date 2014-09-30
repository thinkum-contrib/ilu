/*
 Simple binding service interface implementation
*/
/** $Id: sbimpl.c,v 1.18 1999/08/03 01:56:17 janssen Exp $
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
*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "ilu_simpbind.h"

#include "iluhash.h"	/* deprecated, but we wrote the damn thing, so... */

#define PASS(x)		x
#define OPTIONAL(x)	x

#define NULLFN	0

/* number of slots in hash table (shrug) */ 
#define HTSIZE 100

#define RAISE_USER_EX(env,ex)   { (env)->returnCode=(ex); (env)->_major=ILU_C_USER_EXCEPTION; }

/* datatype stored in hash table.  not seen outside this file. */

typedef struct sbhtval_s {
  ilu_CString sid;
  ilu_CString ih;
  ilu_simpbind_StringBindingHandle sbh;
  char * cookie;
} sbhtval;


typedef struct {
  ilu_boolean protected;
  ilu_refany ht;
  char *filename;
  FILE *fp;
} ServerState;


#define server_state(obj)	((ServerState *)(ilu_simpbind_Server__GetUserData(obj)))
#define hash_table(obj)		((ilu_HashTable)(server_state(obj))->ht)
#define protected(obj)		(server_state(obj)->protected)
#define file_pointer(obj)	(server_state(obj)->fp)
#define backing_filename(obj)	(server_state(obj)->filename)


ilu_boolean ilusb_verbose = ilu_FALSE;


/*****************************************************************************/

static ilu_boolean SbhtvalCompare (ilu_refany key1, ilu_refany key2)
{
  return ((strcmp(((sbhtval *)key1)->sid, ((sbhtval *)key2)->sid) == 0) &&
	  (strcmp(((sbhtval *)key1)->ih, ((sbhtval *)key2)->ih) == 0));
}

static ilu_cardinal SbhtvalHash (ilu_refany key, ilu_cardinal size)
{
  ilu_cardinal crc;
  static ilu_byte nullbyte = 0;

  crc = ilu_CRC32((ilu_bytes) (((sbhtval *) key)->sid), strlen(((sbhtval *) key)->sid));
  crc = ilu_CRC32WithAccum(&nullbyte, 1, crc);
  crc = ilu_CRC32WithAccum((ilu_bytes) (((sbhtval *) key)->ih), strlen(((sbhtval *) key)->ih), crc);

  return (crc % size);
}

static ilu_boolean CheckHashTable (ilu_simpbind_Server self)
{
  return ((server_state(self) != ILU_NIL) && (hash_table(self) != ILU_NIL));
}

ilu_simpbind_Server
  ilu_simpbind_create_server (ILU_C_Server s,
			      ilu_string ih,
			      char *filename,	/* where to keep stable storage */
			      ilu_boolean protected)
{
  FILE *fp;
  ServerState *ss;
  sbhtval *entry;

  if ((fp = fopen(filename, "a+")) == ILU_NIL) {
    fprintf (stderr, "Can't open backing file %s.\n", filename);
    return ILU_NIL;
  }
  fseek(fp, 0L, SEEK_END);
  if ((ss = ilu_malloc(sizeof(ServerState))) == ILU_NIL)
    goto mallocfailure;
  ss->protected = protected;
  ss->fp = fp;
  if ((ss->filename = ILU_C_Strdup(filename)) == ILU_NIL)
    goto mallocfailure;
  if ((ss->ht = (ilu_refany)ilu_hash_MakeNewTable (HTSIZE, SbhtvalHash, SbhtvalCompare)) == ILU_NIL)
    goto mallocfailure;
  if (ftell(fp) > 0) {	/* already exists, read contents */
    char line[5000];
    fseek(fp, 0, SEEK_SET);
    while (fgets(line, sizeof(line)-1, fp) != ILU_NIL)
      {
	line[strlen(line)-1] = 0;	/* clobber trailing newline */
	if ((entry = (sbhtval *) ilu_malloc(sizeof(sbhtval))) == ILU_NIL)
	  goto mallocfailure;
	entry->sbh = ILU_NIL;
	entry->sid = ILU_NIL;
	entry->ih = ILU_NIL;
	entry->cookie = ILU_NIL;
	if ((entry->sbh = ILU_C_Strdup(line)) == ILU_NIL)
	  goto mallocfailure;
	if (fgets(line, sizeof(line)-1, fp) == ILU_NIL)
	  goto readfailure;
	line[strlen(line)-1] = 0;	/* clobber trailing newline */
	if ((entry->sid = ILU_C_Strdup(line)) == ILU_NIL)
	  goto mallocfailure;
	if (fgets(line, sizeof(line)-1, fp) == ILU_NIL)
	  goto readfailure;
	line[strlen(line)-1] = 0;	/* clobber trailing newline */
	if ((entry->ih = ILU_C_Strdup(line)) == ILU_NIL)
	  goto mallocfailure;
	if (fgets(line, sizeof(line)-1, fp) == ILU_NIL)
	  goto readfailure;
	line[strlen(line)-1] = 0;	/* clobber trailing newline */
	if ((entry->cookie = ILU_C_Strdup(line)) == ILU_NIL)
	  goto mallocfailure;
	if (!ilu_hash_AddToTable((ilu_HashTable) (ss->ht), entry, entry))
	  goto addfailure;
      }
  }
  return ilu_simpbind_Server__CreateTrue (ih, s, (ilu_refany) ss);

 mallocfailure:
  fprintf (stderr, "Couldn't allocate memory for binding server.\n");
  return ILU_NIL;

 readfailure:
  fprintf (stderr, "Backing file %s has bad contents.\n");
  return ILU_NIL;

 addfailure:
  fprintf (stderr, "Couldn't add entry to hash table.\n");
  return ILU_NIL;
}

static void UpdateBackingFile (ilu_simpbind_Server self)
{
  FILE *		fp = file_pointer(self);
  ilu_HashEnumerator_s	he;
  ilu_refany            key, data;
  sbhtval *             entry;

  if (ilusb_verbose)
    fprintf (stderr, "Updating backing file with %d entries.\n",
	     ilu_hash_PairsInTable(hash_table(self)));

  fclose(fp);
  if ((fp = fopen(backing_filename(self), "w+")) == ILU_NIL)
    {
      fprintf (stderr, "Can't update backing file %s.\n", backing_filename(self));
      exit(1);
    }
  file_pointer(self) = fp;

  fseek(fp, 0, SEEK_SET);
  ilu_hash_BeginEnumeration(hash_table(self), &he);
  
  while(ilu_hash_Next(&he, &key, &data))
    {
      entry = (sbhtval *) data;
      fputs(entry->sbh, file_pointer(self));  fputc('\n', file_pointer(self));
      fputs(entry->sid, file_pointer(self));  fputc('\n', file_pointer(self));
      fputs(entry->ih, file_pointer(self));  fputc('\n', file_pointer(self));
      fputs(entry->cookie, file_pointer(self));  fputc('\n', file_pointer(self));
    }
  fflush(file_pointer(self));
}
			       
/**********************************************************************/

/* Publish
   Adds sbh and mstid to hash table, indexed by oid.
   Returns proof-of-publication string if successful,
   signals error and returns ILU_NIL if unsuccessful.
   (sbh, mstid unaltered.)
*/

ilu_simpbind_CookieType
  server_ilu_simpbind_Server_Publish (ilu_simpbind_Server self,
				      ilu_simpbind_StringBindingHandle sbh,
				      ILU_C_ENVIRONMENT *ev)
/*	RAISES BadSbh, AlreadyPublished, MallocFailure END */
{
  char *sid = ILU_NIL;
  char *ih = ILU_NIL;
  char *newsbh;
  ilu_simpbind_CookieType proof, cookie;
  sbhtval *the_entry;
  sbhtval temp_entry;
  ilu_Error	err;
  ilu_boolean newentry;

  if (!CheckHashTable(self))
    {
      RAISE_USER_EX(ev,ex_ilu_simpbind_MallocFailure);
      return ILU_NIL;
    }

  if (ilu_ParseSBH(sbh, &ih, &sid, ILU_NIL, ILU_NIL, ILU_NIL, ILU_NIL, &err), ILU_ERRNOK(err))
    {
      RAISE_USER_EX(ev,ex_ilu_simpbind_BadSBH);
      if (ilusb_verbose) fprintf (stderr,
				  "server_ilu_simpbind_Server_Publish:  can't parse sbh <%s>.  "
				  "Error %s\n",
				  sbh, ILU_ERR_DESCRIPTION(err));
      ILU_HANDLED(err);
      return (ILU_NIL);
    }

  temp_entry.sid = sid;
  temp_entry.ih = ih;
  the_entry = ilu_hash_FindInTable(hash_table(self), &temp_entry);
  if (protected(self) && (the_entry != ILU_NIL))
    {
      /* can't arbitrarily replace existing entries... */
      RAISE_USER_EX(ev,ex_ilu_simpbind_AlreadyPublished);    
      ilu_free(sid);
      ilu_free(ih);
      return ILU_NIL;
    }
  else if (the_entry == ILU_NIL)
    {
      newentry = ilu_TRUE;
      if ((the_entry = (sbhtval *) ilu_malloc (sizeof(struct sbhtval_s))) == ILU_NIL)
	{
	  RAISE_USER_EX(ev,ex_ilu_simpbind_MallocFailure);
	  ilu_free(sid);
	  ilu_free(ih);
	  return (ILU_NIL);
	}
      the_entry->sid = ILU_NIL;
      the_entry->ih = ILU_NIL;
      the_entry->cookie = ILU_NIL;
      the_entry->sbh = ILU_NIL;
    }
  else
    {
      newentry = ilu_FALSE;
    }

  /* make table entry... */
  if ((proof = ILU_C_Strdup(ilu_InventID())) == ILU_NIL)
    {
      RAISE_USER_EX(ev,ex_ilu_simpbind_MallocFailure);
      ilu_free(sid);
      ilu_free(ih);
      if (newentry)
	ilu_free(the_entry);
      return (ILU_NIL);
    }
  else if ((newsbh = ILU_C_Strdup(sbh)) == ILU_NIL)
    {
      RAISE_USER_EX(ev,ex_ilu_simpbind_MallocFailure);
      ilu_free(sid);
      ilu_free(ih);
      ilu_free(proof);
      if (newentry)
	ilu_free(the_entry);
      return (ILU_NIL);
    }
  else if ((cookie = ILU_C_Strdup(proof)) == ILU_NIL)
    {
      RAISE_USER_EX(ev,ex_ilu_simpbind_MallocFailure);
      ilu_free(sid);
      ilu_free(ih);
      ilu_free(proof);
      ilu_free(newsbh);
      if (newentry)
	ilu_free(the_entry);
      return (ILU_NIL);
    }
  ilu_free(the_entry->sid);
  the_entry->sid = sid;
  ilu_free(the_entry->ih);
  the_entry->ih = ih;
  ilu_free(the_entry->sbh);
  the_entry->sbh = newsbh;
  ilu_free(the_entry->cookie);
  the_entry->cookie = cookie;

  if (newentry &&
      (!ilu_hash_AddToTable(hash_table(self), the_entry, the_entry)))
    {
      RAISE_USER_EX(ev,ex_ilu_simpbind_MallocFailure);
      ilu_free(sid);
      ilu_free(ih);
      ilu_free(proof);
      ilu_free(newsbh);
      ilu_free(cookie);
      ilu_free(the_entry);
      return (ILU_NIL);
    }
  else
    {
      UpdateBackingFile(self);
      return(proof);
    }
}


/* Withdraw
   Returns ilu_TRUE if successful,
   signals error and returns ilu_FALSE if unsuccessful.
   (oid, cookie unaltered.)
*/

CORBA_boolean
server_ilu_simpbind_Server_Withdraw (ilu_simpbind_Server self,
				     ilu_simpbind_StringBindingHandle sbh,
				     ilu_simpbind_CookieType cookie,
				     ILU_C_ENVIRONMENT *ev)
     /*	RAISES NoTable, NotPublished, BadProof, BadSBH END */
{
  sbhtval key;
  char *proof;
  char *ih = ILU_NIL;
  char *sid = ILU_NIL;
  sbhtval *real_entry;
  ilu_boolean status;
  ilu_Error err;

  if (!CheckHashTable(self)){
    RAISE_USER_EX(ev,ex_ilu_simpbind_NoTable);
    return (ilu_FALSE);  }

  if (ilu_ParseSBH(sbh, &ih, &sid, ILU_NIL, ILU_NIL, ILU_NIL, ILU_NIL, &err), ILU_ERRNOK(err))
    {
      RAISE_USER_EX(ev,ex_ilu_simpbind_BadSBH);
      if (ilusb_verbose) fprintf (stderr,
				  "server_ilu_simpbind_Server_Publish:  can't parse sbh <%s>.  "
				  "Error %s\n",
				  sbh, ILU_ERR_DESCRIPTION(err));
      ILU_HANDLED(err);
      return (ilu_FALSE);
    }

  key.sid = sid;
  key.ih = ih;

  /* First make sure it's in the table, and check the proof. */
  if ( (real_entry = ilu_hash_FindInTable (hash_table(self), &key)) == ILU_NIL )
    {
      RAISE_USER_EX(ev,ex_ilu_simpbind_NotPublished);
      ilu_free(sid);
      ilu_free(ih);
      return (ilu_FALSE);
    }

  ilu_free(sid);
  ilu_free(ih);

  if (strcmp(real_entry->cookie, cookie) != 0 ) {
    RAISE_USER_EX(ev,ex_ilu_simpbind_BadProof);
    return ilu_FALSE; }

  if (status = (ilu_hash_RemoveFromTable(hash_table(self), real_entry) == real_entry))
    UpdateBackingFile(self);

  ilu_free(real_entry->cookie);
  ilu_free(real_entry->sid);
  ilu_free(real_entry->ih);
  ilu_free(real_entry->sbh);
  ilu_free(real_entry);

  if (!status) {
    /* hm, it was right there a minute ago.. */
    RAISE_USER_EX(ev,ex_ilu_simpbind_NotPublished);
    return (ilu_FALSE);  }

  /* No casualties.. */
  return (ilu_TRUE);
}


/* Lookup
   Returns struct containing copy of sbh and mstid if successful,
   signals error and returns ILU_NIL if unsuccessful.
   (oid unaltered.)
*/
ilu_simpbind_StringBindingHandle
server_ilu_simpbind_Server_Lookup (ilu_simpbind_Server self,
				   ilu_CString sid, ilu_CString ih,
				   ILU_C_ENVIRONMENT *ev)
     /*	RAISES NoTable, NotPublished, MallocFailure END */
{
  char *proof;
  ilu_simpbind_StringBindingHandle retval;
  sbhtval key;
  sbhtval *real_entry;

  if (!CheckHashTable(self)) {
    RAISE_USER_EX(ev,ex_ilu_simpbind_NotPublished);
    return (ILU_NIL);  }

  key.sid = sid;
  key.ih = ih;

  if ( (real_entry = ilu_hash_FindInTable (hash_table(self), &key)) == ILU_NIL ) {
    RAISE_USER_EX(ev,ex_ilu_simpbind_NotPublished);
    return (ILU_NIL);  }

  /* Copy the sbh */
  if ((retval = ILU_C_Strdup(real_entry->sbh)) == ILU_NIL) {
    RAISE_USER_EX(ev,ex_ilu_simpbind_MallocFailure);
    return (ILU_NIL);  }

  return(retval);
}


/* Enumerate: to be used for debugging only.
   Returns struct containing sbh and mstid, for
   every sbh which contains the substring <pattern>.
*/

ilu_simpbind_StringBindingHandleList *
server_ilu_simpbind_Server_Enumerate (ilu_simpbind_Server self,
				      ilu_CString pattern,
				      ILU_C_ENVIRONMENT *ev)
{
  ilu_HashEnumerator_s    he;
  ilu_refany              key, data;
  sbhtval                *entry;
  char                   *sbh;
  ilu_simpbind_StringBindingHandleList *result;

  if (!CheckHashTable(self))
      return (ilu_simpbind_StringBindingHandleList_Create(0, ILU_NIL));

  result = ilu_simpbind_StringBindingHandleList_Create(ilu_hash_PairsInTable(hash_table(self)), ILU_NIL);

  ilu_hash_BeginEnumeration(hash_table(self), &he);
  
  while(ilu_hash_Next(&he, &key, &data))
    {
      entry = (sbhtval *) data;
      /* Minimalist pattern matching on string binding handle. */
      if (strstr(entry->sbh, pattern) != ILU_NIL)
	{
	  /* add sbh and mstid to list. */
	  sbh = ILU_C_Strdup(entry->sbh);
	  ilu_simpbind_StringBindingHandleList_Append(result, sbh);
	}
    }

  return (result);
}

