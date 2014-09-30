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
*/
/*
*/
/* $Id: hash.c,v 1.54 1999/08/03 01:52:45 janssen Exp $ */

/* void *ilu_malloc (unsigned long size); */
/* void *ilu_realloc (void *p, unsigned long size); */
/* void ilu_free (void *p); */
/* The ILU runtime (kernel and LS (if appropriate)) use these procedures
   to manage dynamic memory.  ilu_malloc and ilu_realloc call malloc
   and realloc, respectively.  If the basic procedure (malloc or
   realloc) fails, the ILU version then tries to free up some memory,
   perhaps even calling application-specified procs to free memory,
   and then tries again.  These procedures return ILU_NIL if the
   requested amount of memory still can't be allocated. */
/* These are now prototyped in iluxport.h */

#include <string.h>
#include <stdio.h>

#if ((defined(WIN32) || defined(WIN16)) && defined(_WINIO))
#include <winiodef.h>
#endif /* ((defined(WIN32) || defined(WIN16)) && defined(_WINIO)) */

#include "iluntrnl.h"

#include "iluhash.h"

#define NULLFN 0

struct hashEntry {
  /*L1 >= {some mutex that protects the table}*/

  ilu_string he_key;
  ilu_refany he_data;
};

struct hashTableSlot {
  /*L1 >= {some mutex that protects the table}*/

  ilu_shortcardinal hs_count;
  ilu_shortcardinal hs_allocated;
  struct hashEntry *hs_entries;
};

struct _ilu_HashTable_s {
  /*L1 >= {some mutex that protects the table}*/

  ilu_cardinal ht_size, ht_nPairs;
  ilu_cardinal (*ht_hashfn)(ilu_refany key, ilu_cardinal ht_size);
  ilu_boolean  (*ht_compfn)(ilu_refany key1, ilu_refany key2);
  struct hashTableSlot *ht_slots;
};

#define HASH_INCREMENT_SIZE	5

/*L1 >= {some mutex that protects the table}*/

#define hashtable_slot(ht,index)	(&((ht)->ht_slots[index]))
#define hashtable_hash_index(ht,key)	((*((ht)->ht_hashfn))(key,(ht)->ht_size))
#define hashtable_compare_keys(ht, key1, key2)	((*((ht)->ht_compfn))((key1),(key2)))

/*L2, Main unconstrained*/
/*L1 >= {some mutex that protects the table}*/

ilu_refany ilu_hash_FindInTable(HashTable ht, ilu_refany key)
{
  struct hashTableSlot *slot;
  ilu_shortcardinal count;

  if (ht == ILU_NIL)
    return (ILU_NIL);
  slot = hashtable_slot(ht, hashtable_hash_index(ht, key));
  for (count = slot->hs_count; count > 0; count -= 1)
    if (hashtable_compare_keys(ht, key,
			       slot->hs_entries[count - 1].he_key))
      return (slot->hs_entries[count - 1].he_data);
  return (ILU_NIL);
}

ilu_refany ilu_hash_RemoveFromTable (HashTable ht, ilu_refany key)
{
  struct hashTableSlot *slot;
  ilu_shortcardinal count;

  if (ht == ILU_NIL)
    return (ILU_NIL);

  slot = hashtable_slot(ht,hashtable_hash_index(ht, key));
  for (count = 0;  count  < slot->hs_count;  count += 1)
    if (hashtable_compare_keys(ht, slot->hs_entries[count].he_key, key))
      {
	ilu_refany data = slot->hs_entries[count].he_data;
	if ((1 + (unsigned) count) < (unsigned) slot->hs_count)
	    slot->hs_entries[count] = slot->hs_entries[slot->hs_count-1];
	--ht->ht_nPairs;
	if (--slot->hs_count <= 0)
	  {
	    ilu_free(slot->hs_entries);
	    slot->hs_entries = ILU_NIL;
	    slot->hs_allocated = 0;
	    slot->hs_count = 0;
	  }
	return (data);
      }
  return (ILU_NIL);
}

ilu_boolean 
ilu_hash_AddToTable(HashTable ht, ilu_refany key, ilu_refany obj)
{
  struct hashTableSlot *slot;
  ilu_shortcardinal count;

  if (ht == ILU_NIL)
    return (ilu_FALSE);

  slot = hashtable_slot(ht, hashtable_hash_index(ht, key));

  for (count = slot->hs_count; count > 0; count -= 1)
    if (hashtable_compare_keys(ht, key,
			       slot->hs_entries[count - 1].he_key))
      return (ilu_FALSE);

  if (slot->hs_allocated == 0) {
    slot->hs_allocated = HASH_INCREMENT_SIZE;
    slot->hs_entries = (struct hashEntry *) ilu_malloc(
		   sizeof(struct hashEntry) * slot->hs_allocated);
    slot->hs_count = 0;
  } else if (slot->hs_count >= slot->hs_allocated)
    slot->hs_entries = (struct hashEntry *) ilu_realloc(slot->hs_entries,
		       (slot->hs_allocated += HASH_INCREMENT_SIZE)
				      * sizeof(struct hashEntry));

  slot->hs_entries[slot->hs_count].he_key = key;
  slot->hs_entries[slot->hs_count].he_data = obj;
  slot->hs_count += 1;
  ht->ht_nPairs += 1;
  return (ilu_TRUE);
}

void ilu_hash_BeginEnumeration (HashTable ht, HashEnumerator *he)
{
  he->hn_ht = ht;
  he->hn_index = 0;
  he->hn_count = ht->ht_slots[0].hs_count;
  return;
}

ilu_boolean ilu_hash_Next(HashEnumerator *he, ilu_refany *key,
					       ilu_refany *data)
{
  if (he->hn_index >= he->hn_ht->ht_size)
      return(ilu_FALSE);
  if (he->hn_ht->ht_slots[he->hn_index].hs_count < he->hn_count)
      he->hn_count = he->hn_ht->ht_slots[he->hn_index].hs_count;
  while (he->hn_count <= 0) {
      if (++he->hn_index >= he->hn_ht->ht_size)
          return (ilu_FALSE);
      he->hn_count = he->hn_ht->ht_slots[he->hn_index].hs_count;
    }
  he->hn_count -= 1;
  *key  = he->hn_ht->ht_slots[he->hn_index].hs_entries[he->hn_count].he_key;
  *data = he->hn_ht->ht_slots[he->hn_index].hs_entries[he->hn_count].he_data;
  return (ilu_TRUE);
}

#ifdef ENABLE_DEBUGGING
/* This function is provided solely for use with a debugger.  It is
   never called by the ILU runtime. */
static void ilu_hash_PrintHashTable (HashTable ht)
{
  HashEnumerator he;
  ilu_refany key, data;

  if (ht == (HashTable) 0)
    ilu_DebugPrintf ("NIL specified for hash table\n");
  else
    {
      ilu_hash_BeginEnumeration (ht, &he);
      while (ilu_hash_Next(&he, &key, &data))
	{
	  if (ht->ht_compfn == ilu_hash_StringCompare)
	    ilu_DebugPrintf ("  %s (%p) : %p\n", (ilu_string) key, key, data);
	  else
	    ilu_DebugPrintf ("  %p : %p\n", key, data);
	}
    }
}
#endif /* ENABLE_DEBUGGING */

void 
ilu_hash_TableEnumerate(HashTable ht,
	    void (*proc) (ilu_refany entry_data, ilu_refany rock),
			 ilu_refany rock)
{
  ilu_cardinal    i;

  if (ht == ILU_NIL)
    return;

  for (i = 0; i < ht->ht_size; i += 1)
    if (ht->ht_slots[i].hs_count > 0) {
      ilu_shortcardinal j;

      for (j = 0; j < ht->ht_slots[i].hs_count; j += 1)
	(*proc) (ht->ht_slots[i].hs_entries[j].he_data, rock);
    }
}

ilu_refany 
ilu_hash_FindViaProc(HashTable ht,
      ilu_boolean(*proc) (ilu_refany entry_data, ilu_refany rock),
		      ilu_refany rock)
{
  ilu_cardinal    i;

  if (ht == ILU_NIL)
    return (ILU_NIL);

  for (i = 0; i < ht->ht_size; i += 1)
    if (ht->ht_slots[i].hs_count > 0) {
      ilu_shortcardinal j;

      for (j = 0; j < ht->ht_slots[i].hs_count; j += 1)
	if ((*proc) (ht->ht_slots[i].hs_entries[j].he_data, rock))
	  return (ht->ht_slots[i].hs_entries[j].he_data);
    }
  return (ILU_NIL);
}

/*L1 unconstrained*/

ilu_boolean ilu_hash_StringCompare (ilu_refany key1, ilu_refany key2)
{
  ilu_string str1 = (ilu_string) key1;
  ilu_string str2 = (ilu_string) key2;
  return (strcmp(str1, str2) == 0);
}

ilu_boolean ilu_hash_PointerCompare (ilu_refany key1, ilu_refany key2)
{
  return (key1 == key2);
}

ilu_cardinal ilu_hash_HashString (ilu_refany key, ilu_cardinal size)
{
  ilu_string      str = (ilu_string) key;
#if 0
  ilu_cardinal    val = 0, m = 1;
  /*
   * Interpret the string as a radix-256 number; compute remainder
   * wrt 0xFFFFFD, the largest prime < 256^3.
   */
  while (*str != '\0') {
    val = (val + (*str++) * m) % 0xFFFFFD;
    m = (m * 256) % 0xFFFFFD;
  }
#else
  ilu_cardinal crc = ilu_CRC32 ((ilu_bytes) str, (ilu_cardinal) strlen(str));
  return (crc % size);
#endif
}

ilu_cardinal ilu_hash_HashPointer (ilu_refany key, ilu_cardinal size)
{
  ilu_cardinal i = 0;
  ilu_cardinal val = 0;

  while (i < sizeof(ilu_refany))
    val += ((ilu_string )&key)[i++];
  return (val % size);
}

HashTable ilu_hash_MakeNewTable (ilu_cardinal size,
	ilu_hashfnptr hashfn, ilu_compfnptr compfn)
{
  HashTable       new = (HashTable) ilu_malloc(sizeof(struct _ilu_HashTable_s));
  new->ht_size = size;
  new->ht_nPairs = 0;
  new->ht_slots = (struct hashTableSlot *) ilu_malloc(
			     sizeof(struct hashTableSlot) * size);
  if (hashfn == NULLFN)
    new->ht_hashfn = ilu_hash_HashString;
  else
    new->ht_hashfn = hashfn;
  if (compfn == NULLFN)
    new->ht_compfn = ilu_hash_StringCompare;
  else
    new->ht_compfn = compfn;
  memset((void *) (new->ht_slots), 0,
	 sizeof(struct hashTableSlot) * ((SIZE_T) size));
  return (new);
}

/*L1 >= {some mutex that protects the table}*/

void ilu_hash_FreeHashTable (HashTable ht, void (*freeKey)(ilu_refany),
					    void (*freeData)(ilu_refany))
{
  register ilu_cardinal i;

  if (ht == ILU_NIL)
    return;

  for (i = 0; i < ht->ht_size; i++) {
    if (ht->ht_slots[i].hs_count > 0
	&& ht->ht_slots[i].hs_entries != ILU_NIL) {
      if (freeData != NULLFN || freeKey != NULLFN) {
	register ilu_cardinal j;

	for (j = 0; j < ht->ht_slots[i].hs_count; j++) {
	  if (freeKey != NULLFN
	      && ht->ht_slots[i].hs_entries[j].he_key != ILU_NIL)
	    (*freeKey) (ht->ht_slots[i].hs_entries[j].he_key);
	  if (freeData != NULLFN
	      && ht->ht_slots[i].hs_entries[j].he_data != ILU_NIL)
	    (*freeData) (ht->ht_slots[i].hs_entries[j].he_data);
	}
      }
      ilu_free(ht->ht_slots[i].hs_entries);
    }
  }
  ilu_free(ht->ht_slots);
  ilu_free(ht);
  return;
}

ilu_cardinal ilu_hash_PairsInTable (HashTable ht)
{
  if (ht == ILU_NIL)
       return (0);
  else return (ht->ht_nPairs);
}
