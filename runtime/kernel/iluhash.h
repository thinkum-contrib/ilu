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
/* $Id: iluhash.h,v 1.25 1999/08/03 01:52:57 janssen Exp $ */
/* Last tweaked by Mike Spreitzer February 9, 1995 12:32 pm PST */

#ifndef _ILUHASH
#define _ILUHASH

#include <iluxport.h>

/*L2, Main unconstrained*/

typedef struct _ilu_HashTable_s *ilu_HashTable;
#define HashTable ilu_HashTable

typedef struct {
  /*L1 >= {some mutex that protects the table}*/

  ilu_HashTable hn_ht;
  ilu_cardinal hn_index, hn_count;
} ilu_HashEnumerator_s;
#define HashEnumerator ilu_HashEnumerator_s

/*L1 >= {some mutex that protects the table}*/

typedef 
ilu_cardinal(*ilu_hashfnptr) (ilu_refany /* key */ ,
			      ilu_cardinal /* modulus */ );

typedef 
ilu_boolean(*ilu_compfnptr) (ilu_refany /* key1 */ ,
			     ilu_refany /* key2 */ );

/*L1, L2, Main unconstrained*/

ILU_PUBLIC ilu_boolean     ilu_hash_PointerCompare(ilu_refany, ilu_refany);
ILU_PUBLIC ilu_cardinal    ilu_hash_HashPointer(ilu_refany, ilu_cardinal);

ILU_PUBLIC ilu_boolean     ilu_hash_StringCompare(ilu_refany, ilu_refany);
ILU_PUBLIC ilu_cardinal    ilu_hash_HashString(ilu_refany, ilu_cardinal);

ILU_PUBLIC ilu_HashTable   ilu_hash_MakeNewTable (ilu_cardinal size,
						  ilu_hashfnptr hashfn,
						  ilu_compfnptr compfn);

/*L1 >= {some mutex that protects the table}*/

ILU_PUBLIC ilu_refany ilu_hash_FindInTable (ilu_HashTable ht, ilu_refany key);
/* Returns ILU_NIL if ht==ILU_NIL or key not found. */

ILU_PUBLIC ilu_boolean ilu_hash_AddToTable (ilu_HashTable ht,
					    ilu_refany key /* PASS */,
					    ilu_refany obj /* PASS */);
/* If ht==ILU_NIL or key already in table, returns ilu_FALSE;
   otherwise adds <key, obj> to table and returns ilu_TRUE. */

ILU_PUBLIC ilu_refany ilu_hash_RemoveFromTable (HashTable ht, ilu_refany key);
/* If ht==ILU_NIL or key not in table, returns ILU_NIL;
   otherwise removes key & returns its formerly associated datum. */

ILU_PUBLIC void ilu_hash_FreeHashTable (ilu_HashTable ht,
					void (*freeKey)(ilu_refany),
					void (*freeData)(ilu_refany));

ILU_PUBLIC ilu_cardinal ilu_hash_PairsInTable (ilu_HashTable ht);

ILU_PUBLIC void 
ilu_hash_TableEnumerate(ilu_HashTable ht,
			void (*proc) (ilu_refany entry_data,
				      ilu_refany rock),
			ilu_refany rock);

ILU_PUBLIC ilu_refany 
ilu_hash_FindViaProc(ilu_HashTable ht,
		     ilu_boolean(*proc) (ilu_refany entry_data,
					 ilu_refany rock),
		     ilu_refany rock);

ILU_PUBLIC void ilu_hash_BeginEnumeration (ilu_HashTable ht, ilu_HashEnumerator_s *he);
/* Initialize an enumeration state.  "he" must point to a HashEnumerator struct. */

ILU_PUBLIC ilu_boolean ilu_hash_Next(ilu_HashEnumerator_s *he,
				     ilu_refany *key,
				     ilu_refany *data);
/* If there's another pair to enumerate, stores the next pair to be enumerated in *key and *data, and returns ilu_TRUE; otherwise returns ilu_FALSE.  Unless the protecting mutex is held throughout the enumeration, pairs added or removed during the enumeration may or may not be enumerated, and if any pairs are removed, other pairs may be enumerated more than once.  A pair is in the table when it is enumerated. */


#endif
