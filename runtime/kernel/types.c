/** $Id: types.c,v 1.50 1999/08/11 02:11:45 janssen Exp $
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
/* Last edited by Mike Spreitzer October 23, 1998 2:35 pm PDT */

#include "iluntrnl.h"
#include "ilutypes.h"
#include "call.h"
#include "connect.h"
#include "iluprotocol.h"


/* Locking:  L1 >= {otmu}

   When calling a series of these functions around one logical type,
   as in

   ilu_Type t = ilu_RegisterRecordType (...);
   ilu_RegisterRecordTypeField (t, ...);
   ilu_RegisterRecordTypeField (t, ...);
   ilu_RegisterRecordTypeField (t, ...);

   the lock should be held across the set of calls.
*/

#define ILU_TYPE_HASHTABLE_SIZE		137

static HashTable	TypeTableByUID = NIL;	/* key is UID string, data is ilu_Type */

static HashTable	TypeTableByName = NIL;	/* key is "interface_name.type_name",
						   data is ilu_Type */

typedef struct {
  ilu_TypeUID	uid;	/* uid (owned by this struct) */
  ilu_Vector	refs;	/* list of "ilu_Type *" values which need this type */
} UnsatisfiedRef;

static HashTable	UnsatisfiedTypeRefs = NIL;	/* key is UID, data is UnsatisfiedRef* */

struct name_record {
  ilu_Interface_s	interface;
  ilu_string		name;
};

static ilu_string
  TypeKindName (ilu_TypeKind tk)
{
  if (tk == ilu_byte_tk) {
    return "byte";
  } else if (tk == ilu_boolean_tk) {
    return "boolean";
  } else if (tk == ilu_character_tk) {
    return "character";
  } else if (tk == ilu_shortcharacter_tk) {
    return "shortcharacter";
  } else if (tk == ilu_shortinteger_tk) {
    return "shortinteger";
  } else if (tk == ilu_integer_tk) {
    return "integer";
  } else if (tk == ilu_longinteger_tk) {
    return "longinteger";
  } else if (tk == ilu_shortcardinal_tk) {
    return "shortcardinal";
  } else if (tk == ilu_cardinal_tk) {
    return "cardinal";
  } else if (tk == ilu_longcardinal_tk) {
    return "longcardinal";
  } else if (tk == ilu_real_tk) {
    return "real";
  } else if (tk == ilu_shortreal_tk) {
    return "shortreal";
  } else if (tk == ilu_longreal_tk) {
    return "longreal";
  } else if (tk == ilu_object_tk) {
    return "object";
  } else if (tk == ilu_string_tk) {
    return "string";
  } else if (tk == ilu_pipe_tk) {
    return "pipe";
  } else if (tk == ilu_optional_tk) {
    return "optional";
#ifdef ILU_REFERENCE_TYPES
  } else if (tk == ilu_reference_tk) {
    return "reference";
#endif
  } else if (tk == ilu_alias_tk) {
    return "alias";
  } else if (tk == ilu_union_tk) {
    return "union";
  } else if (tk == ilu_sequence_tk) {
    return "sequence";
  } else if (tk == ilu_record_tk) {
    return "record";
  } else if (tk == ilu_array_tk) {
    return "array";
  } else if (tk == ilu_enumeration_tk) {
    return "enumeration";
  } else if (tk == ilu_pickle_tk) {
    return "pickle";
#ifdef ILU_FIXED_POINT_SUPPORT
  } else if (tk == ilu_fixedpoint_tk) {
    return "fixedpoint";
#endif
  } else
    return NIL;
}

/* L1 >= otmu */
static void
  EnsureHashTables (void)
{
  static ilu_boolean initialized = ilu_FALSE;

  if (initialized)
    return;

#ifdef ENABLE_DEBUGGING
  ILU_AUTOSETDEBUGLEVEL;
#endif /* ENABLE_DEBUGGING */

  if (TypeTableByUID == NIL)
    TypeTableByUID = ilu_hash_MakeNewTable(ILU_TYPE_HASHTABLE_SIZE,
					    ilu_hash_HashString,
					    ilu_hash_StringCompare);
  _ilu_Assert(TypeTableByUID != NIL, "creating TypeTableByUID");

  if (TypeTableByName == NIL)
    TypeTableByName = ilu_hash_MakeNewTable(ILU_TYPE_HASHTABLE_SIZE,
					    ilu_hash_HashString,
					    ilu_hash_StringCompare);
  _ilu_Assert(TypeTableByUID != NIL, "creating TypeTableByName");

  if (UnsatisfiedTypeRefs == NIL)
    UnsatisfiedTypeRefs = ilu_hash_MakeNewTable(ILU_TYPE_HASHTABLE_SIZE,
						 ilu_hash_HashString,
						 ilu_hash_StringCompare);
  _ilu_Assert(TypeTableByUID != NIL, "creating UnsatisfiedTypeRefs");

  initialized = ilu_TRUE;

  _ilu_RegisterBuiltInTypes();
}

/* Locking:  L1 >= {otmu} */

static ilu_Type
  _ilu_FindTypeByUID (ilu_TypeUID uid,		/* UID to search for */
		      ILU_ERRS((internal)) * err)
{
  ilu_Type n;

  EnsureHashTables();

  n = ilu_hash_FindInTable (TypeTableByUID, uid);
  ILU_CLER(*err);
  return n;
}


/* Locking:  L1 < otmu */

ilu_Type
  ilu_FindTypeByUID (ilu_TypeUID uid,		/* UID to search for */
		     ILU_ERRS((internal)) * err)
{
  ilu_Type n;

  ilu_AcquireMutex(ilu_otmu);
  n = _ilu_FindTypeByUID(uid, err);
  ilu_ReleaseMutex(ilu_otmu);
  return n;
}


/* find type by interface name and type name */
/* OPTIONAL */ static ilu_Type
  _ilu_FindTypeByName (ilu_string name,			/* "interface.type" */
		       ILU_ERRS((internal)) * err)
{
  ilu_Type n;

  EnsureHashTables();

  n = ilu_hash_FindInTable (TypeTableByName, name);

  ILU_CLER(*err);
  return n;
}


/* find type by interface name and type name */
/* OPTIONAL */ ilu_Type
  ilu_FindTypeByName (ilu_string name,			/* "interface.type" */
		      ILU_ERRS((internal)) * err)
{
  ilu_Type n;

  ilu_AcquireMutex(ilu_otmu);
  n = _ilu_FindTypeByName(name, err);
  ilu_ReleaseMutex(ilu_otmu);
  return n;
}


/* L1 >= otmu */
static void _ilu_update_unsatisfied_types(ilu_Type the_type)

     /* link all type pointers which have been waiting for this type */

{
  UnsatisfiedRef *r;
  ilu_cardinal i, size;
  ilu_Type ** refs;

  if ((ilu_hash_PairsInTable(UnsatisfiedTypeRefs) > 0) &&
      ((r = (UnsatisfiedRef *) ilu_hash_FindInTable(UnsatisfiedTypeRefs, type_uid(the_type))) != NIL))
    {
      refs = (ilu_Type **) _ilu_vector_elements(r->refs);
      size = _ilu_vector_size(r->refs);
      for (i = 0;  i < size;  i++)
	{
	  _ilu_Assert(*(refs[i]) == NIL, "_ilu_update_unsatisfied_types:  non-NIL unsatisfied type ref");
	  *(refs[i]) = the_type;
	}
      _ilu_vector_destroy (r->refs, NULLFN);
      ilu_hash_RemoveFromTable (UnsatisfiedTypeRefs, r->uid);
      ilu_free(r->uid);
      ilu_free(r);
    }
}


/* L1 >= otmu */
static ilu_boolean
  RegisterType(ilu_Type type, ilu_Error *err)
{
  char           *key;
  ilu_Type        t2;
  ILU_NOTE(TYPE_DEBUG,
	   ("_ilu_RegisterType:  Registering %s type %s%s%s%s.%s, id=%s, ptr=0x%p.\n",
	    TypeKindName(type_kind(type)),
	    type_interface_name(type),
	    (type_interface_brand(type) != NIL) ? "(brand=" : "",
	    (type_interface_brand(type) != NIL) ? type_interface_brand(type) : "",
	    (type_interface_brand(type) != NIL) ? ")" : "",
	    type_name(type), type_uid(type), (void *) type));

  /* make hash tables if they don't exist yet */

  EnsureHashTables();

  /* if type has already been registered, signal an error */

  if (ilu_hash_FindInTable(TypeTableByUID, type_uid(type)) != NIL) {
    ILU_NOTE(TYPE_DEBUG,
	  ("_ilu_RegisterType:  %s%s%s%s.%s already registered.\n",
	   type_interface_name(type),
	   (type_interface_brand(type) != NIL) ? "(brand=" : "",
	   ((type_interface_brand(type) != NIL)
	    ? type_interface_brand(type) : ""),
	   (type_interface_brand(type) != NIL) ? ")" : "",
	   type_name(type)));
    return ILU_ERR_CONS1(internal, err, minor, ilu_im_typeDuplicated,
			 ilu_FALSE);
  }
  key = ilu_Strcat3E(type_interface_name(type), ".", type_name(type),
		     err);
  if (ILU_ERRNOK(*err))
    return ilu_FALSE;
  t2 = (ilu_Type) ilu_hash_FindInTable(TypeTableByName, key);
  if (t2) {
    char           *b2 = type_interface_brand(t2);
    char           *b1 = type_interface_brand(type);
    ILU_NOTE(TYPE_DEBUG,
	     ("_ilu_RegisterType:  Collision at name %s.%s, between types of id %s (brand %s) and %s (brand %s).\n",
	      type_interface_name(type), type_name(type),
	      type_uid(type), (b1 ? b1 : "(null)"),
	      type_uid(t2), (b2 ? b2 : "(null)")));
    return ILU_ERR_CONS1(internal, err, minor, ilu_im_typeNameCollision,
			 ilu_FALSE);
  }
  /* add type to tables */

  _ilu_Assert(!!ilu_hash_AddToTable(TypeTableByUID, type_uid(type), type),
	      "_ilu_RegisterType AddToTable TypeTableByUID");
  _ilu_Assert(!!ilu_hash_AddToTable(TypeTableByName, key, type),
	      "_ilu_RegisterType AddToTable TypeTableByName");

  /*
   * now link all type pointers which have been waiting for this
   * type
   */
  _ilu_update_unsatisfied_types(type);

  ILU_CLER(*err);
  return ilu_TRUE;
}

/* this routine will take the "uid" and find out if there is a registered
   type corresponding to it.  If so, it will place an ilu_Type value for
   that registered type in "ptr"; otherwise it will register an UnsatisfiedRef
   for "uid" and "ptr".

   Locking:  L1 >= otmu
*/
static ilu_boolean
  AssignTypeForUID (ilu_TypeUID uid,
		    ilu_Type *  ptr,
		    ILU_ERRS((no_memory)) *err)
{
  ilu_Type t;
  UnsatisfiedRef *r;

  EnsureHashTables();
  ILU_CLER(*err);

  if ((t = (ilu_Type) ilu_hash_FindInTable (TypeTableByUID, uid)) == NIL)
    
    /* not found:  set the pointer to NIL, and add a reference to the pointer
       so that it can be filled in when the type is registered */

    {
      *ptr = NIL;
      r = (UnsatisfiedRef *) ilu_hash_FindInTable(UnsatisfiedTypeRefs, uid);
      if (r == NIL)
	{
	  r = (UnsatisfiedRef *) ilu_MallocE(sizeof(*r), err);
	  if (ILU_ERRNOK(*err))
	    return ilu_FALSE;
	  r->uid = ilu_StrdupE(uid, err);
	  if (ILU_ERRNOK(*err))
	    return ilu_FALSE;
	  r->refs = _ilu_vector_new(2, err);
	  if (ILU_ERRNOK(*err)) return ilu_FALSE;
	  _ilu_Assert(ilu_hash_AddToTable(UnsatisfiedTypeRefs, r->uid, r),
		      "adding unsatisfied ref to UnsatisfiedTypeRefs");
	}
      _ilu_vector_add (r->refs, ptr, err);
      if (ILU_ERRNOK(*err)) return ilu_FALSE;
    }

  else

    /* found:  set the pointer to the appropriate value */

    *ptr = t;

  return ilu_TRUE;
}

#ifdef ENABLE_DEBUGGING
static void printRef (ilu_refany entry_data, ilu_refany rock)
{
  ilu_DebugPrintf("  %s (%d)\n", ((UnsatisfiedRef *) entry_data)->uid,
		  _ilu_vector_size(((UnsatisfiedRef *) entry_data)->refs));
}
#endif /* ENABLE_DEBUGGING */

ilu_boolean
  ilu_CheckForUnsatisfiedTypeRefs (void)
{
  EnsureHashTables();
#ifdef ENABLE_DEBUGGING
  if ((ilu_DebugLevel & TYPE_DEBUG) != 0)
    {
      int n = ilu_hash_PairsInTable(UnsatisfiedTypeRefs);
      ilu_DebugPrintf("ilu_CheckForUnsatisfiedTypeRefs:  %d unsatisfied type refs%s\n", n, (n == 0) ? "." : ":");
      ilu_hash_TableEnumerate (UnsatisfiedTypeRefs, printRef, NIL);
    }
#endif /* ENABLE_DEBUGGING */
  return (ilu_hash_PairsInTable(UnsatisfiedTypeRefs) > 0);
}

static ilu_Type
  build_new_type (ilu_TypeKind kind,
		  ilu_string name,
		  ilu_string interface_name,
		  ilu_string interface_brand,
		  ilu_string uid,
		  ilu_Error *err)
{
  ilu_Type n;

  n = (ilu_Type) ilu_MallocE(sizeof(*n), err);
  if (ILU_ERRNOK(*err)) return NIL;
  n->kind = kind;
  type_names(n) = ilu_MallocE(sizeof(ilu_TypeName_s), err);
  type_names_count(n) = 1;
  if (ILU_ERRNOK(*err)) { goto err1; };
  type_name(n) = ilu_StrdupE(name, err);
  if (ILU_ERRNOK(*err)) { goto err2; };
  type_interface_name(n) = ilu_StrdupE(interface_name, err);
  if (ILU_ERRNOK(*err)) { goto err3; };
  type_interface_brand(n) = ilu_StrdupE(interface_brand, err);
  if (ILU_ERRNOK(*err)) { goto err4; };
  type_uid(n) = ilu_StrdupE(uid, err);
  if (ILU_ERRNOK(*err)) { goto err5; };
  return n;

 err5:
  ilu_free(type_interface_brand(n));
 err4:
  ilu_free(type_interface_name(n));
 err3:
  ilu_free(type_name(n));
 err2:
  ilu_free(type_names(n));
 err1:
  ilu_free(n);
  return NIL;
}

static void
  free_new_type (ilu_Type n)
{
  ilu_free(type_name(n));
  ilu_free(type_interface_name(n));
  ilu_free(type_interface_brand(n));
  ilu_free(type_uid(n));
  ilu_free(type_names(n));
  ilu_free(n);
}

/*
   Note that RECORD types have "fields", UNION types have "arms",
   ENUMERATION types have "elements"
*/

/* register a new sequence type */
ilu_Type
  ilu_RegisterSequenceType (ilu_string name,		/* name, RETAIN */
			    ilu_string interface_name,	/* interface-name, RETAIN */
			    ilu_string interface_brand,	/* interface-brand, OPTIONAL, RETAIN */
			    ilu_string uid,		/* UID, RETAIN */
			    ilu_TypeUID base_type_uid,	/* base type, RETAIN */
			    ilu_cardinal limit,		/* limit (0 if none) */
			    ilu_boolean *new,		/* OUT:  ilu_TRUE if new registration */
			    ILU_ERRS((internal, no_memory)) * err)
{
  ilu_Type n;

  ilu_HoldMutex(ilu_otmu);

  if (((n = _ilu_FindTypeByUID(uid, err)) != NIL) && ILU_ERROK(*err))
    { *new = ilu_FALSE ; return n; }
  if (ILU_ERRNOK(*err))
    return NIL;

  n = build_new_type(ilu_sequence_tk, name, interface_name, interface_brand, uid, err);
  if (ILU_ERRNOK(*err)) return NIL;
  AssignTypeForUID (base_type_uid, &n->desc.Sequence.type, err);
  if (ILU_ERRNOK(*err) || !RegisterType(n, err)) {
    free_new_type(n);
    return NIL;
  }
  n->desc.Sequence.limit = limit;

  *new = ilu_TRUE;
  return n;
}

#ifdef ILU_FIXED_POINT_SUPPORT

/* register a new fixed-point type */
ilu_Type
  ilu_RegisterFixedpointType (ilu_string name,		/* name, RETAIN */
			      ilu_string interface_name,	/* interface-name, RETAIN */
			      ilu_string interface_brand,	/* interface-brand, OPTIONAL, RETAIN */
			      ilu_string uid,		/* UID, RETAIN */
			      ilu_Bignum min_numerator,	/* OPTIONAL, RETAIN */
			      ilu_Bignum max_numerator,	/* OPTIONAL, RETAIN */
			      ilu_Bignum denominator,	/* RETAIN */
			      ilu_cardinal fixed_digits,/* 0 if not applicable */
			      ilu_cardinal fixed_decimal_places,
			      ilu_FixedPointRangeSize range_size,
			      ilu_boolean *new,		/* OUT:  ilu_TRUE if new registration */
			      ILU_ERRS((internal, no_memory)) * err)
{
  ilu_Type n;
  ilu_cardinal size = (unsigned long) range_size;

  ilu_HoldMutex(ilu_otmu);

  if (((n = _ilu_FindTypeByUID(uid, err)) != NIL) && ILU_ERROK(*err))
    { *new = ilu_FALSE ; return n; }
  if (ILU_ERRNOK(*err))
    return NIL;

  n = build_new_type(ilu_fixedpoint_tk, name, interface_name, interface_brand, uid, err);
  if (ILU_ERRNOK(*err) || !RegisterType(n, err))
    goto free1;
  if ((n->desc.Fixedpoint.denominator = ilubignum_Copy(denominator)) == NIL) goto err1;
  if (min_numerator != NIL) {
    if ((n->desc.Fixedpoint.min_numerator = ilubignum_Copy(min_numerator)) == NIL) goto err2;
  };
  if (max_numerator != NIL) { 
    if ((n->desc.Fixedpoint.max_numerator = ilubignum_Copy(max_numerator)) == NIL) goto err3;
  };
  n->desc.Fixedpoint.corba_fixed_digits = fixed_digits;
  n->desc.Fixedpoint.corba_fixed_decimal_places = fixed_decimal_places;
  if (size > 7) {
    ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0);
    goto free3;
  }
  n->desc.Fixedpoint.size_in_bytes = range_size;
  *new = ilu_TRUE;
  return n;

 err3:
  ILU_ERR_CONS1(no_memory, err, nbytes, 0, 0);
  goto free3;
 err2:
  ILU_ERR_CONS1(no_memory, err, nbytes, 0, 0);
  goto free2;
 err1:
  ILU_ERR_CONS1(no_memory, err, nbytes, 0, 0);
  goto free1;

 free3:
  if (n->desc.Fixedpoint.min_numerator) ilubignum_Free(n->desc.Fixedpoint.min_numerator);
 free2:
  ilubignum_Free(n->desc.Fixedpoint.denominator);
 free1:
  free_new_type(n);
  return NIL;
}

#endif /* def ILU_FIXED_POINT_SUPPORT */

/* register a new string type */
ilu_Type
  ilu_RegisterStringType (ilu_string name,		/* name, RETAIN */
			  ilu_string interface_name,	/* interface-name, RETAIN */
			  ilu_string interface_brand,	/* interface-brand, OPTIONAL, RETAIN */
			  ilu_string uid,		/* UID, RETAIN */
			  ilu_string language,		/* language of string, RETAIN */
			  ilu_cardinal limit,		/* limit (0 if none) */
			  ilu_cardinal char_set,	/* char set ID */
			  ilu_boolean *new,		/* OUT:  ilu_TRUE if new registration */
			  ILU_ERRS((internal, no_memory)) * err)
{
  ilu_Type n;
  ilu_string lang;

  ilu_HoldMutex(ilu_otmu);

  if (((n = _ilu_FindTypeByUID(uid, err)) != NIL) && ILU_ERROK(*err))
    { *new = ilu_FALSE ; return n; }
  if (ILU_ERRNOK(*err))
    return NIL;

  n = build_new_type(ilu_string_tk, name, interface_name, interface_brand, uid, err);
  if (ILU_ERRNOK(*err) || !RegisterType(n, err)) {
    free_new_type(n);
    return NIL;
  }
  lang = ilu_StrdupE(language, err);
  if (ILU_ERRNOK(*err)) {
    free_new_type(n);
    return NIL;
  };
  n->desc.String.limit = limit;
  n->desc.String.char_set = char_set;
  n->desc.String.language = lang;
  *new = ilu_TRUE;
  return n;
}


/* Register a new array type */
ilu_Type
  ilu_RegisterArrayType (ilu_string name,		/* name, RETAIN */
			 ilu_string interface_name,	/* interface-name, RETAIN */
			 ilu_string interface_brand,	/* interface-brand, OPTIONAL, RETAIN */
			 ilu_string uid,		/* UID, RETAIN */
			 ilu_TypeUID base_type_uid,	/* base type, RETAIN */
			 ilu_cardinal n_dims,		/* number of dimensions */
			 ilu_cardinal *dims,		/* pointer to sequence of dimensions */
			 ilu_boolean *new,		/* OUT:  ilu_TRUE if new registration */
			 ILU_ERRS((internal, no_memory)) *err)
{
  ilu_Type n;
  unsigned int i;

  ilu_HoldMutex(ilu_otmu);

  if (((n = _ilu_FindTypeByUID(uid, err)) != NIL) && ILU_ERROK(*err))
    { *new = ilu_FALSE; return n; };
  if (ILU_ERRNOK(*err))
    return NIL;

  n = build_new_type(ilu_array_tk, name, interface_name, interface_brand, uid, err);
  if (ILU_ERRNOK(*err)) return NIL;
  n->kind = ilu_array_tk;
  n->desc.Array.n_dims = n_dims;
  n->desc.Array.dims = (ilu_cardinal *) ilu_MallocE(sizeof(ilu_cardinal) * n_dims, err);
  if (ILU_ERRNOK(*err)) {
    free_new_type(n);
    return NIL;
  }
  for (i = 0;  i < n_dims;  i++)
    n->desc.Array.dims[i] = dims[i];
  AssignTypeForUID (base_type_uid, &n->desc.Array.type, err);
  if (ILU_ERRNOK(*err) || !RegisterType(n, err)) {
    ilu_free(n->desc.Array.dims);
    free_new_type(n);
    return NIL;
  }
  *new = ilu_TRUE;
  return n;
}


/* call this function to begin registering a new record type */
ilu_Type
  ilu_RegisterRecordType (ilu_string name,		/* name, RETAIN */
			  ilu_string interface_name,	/* interface-name, RETAIN */
			  ilu_string interface_brand,	/* interface-brand, OPTIONAL, RETAIN */
			  ilu_string uid,		/* UID, RETAIN */
			  ilu_cardinal n_fields,	/* number of fields */
			  ilu_boolean extensible,	/* can it be extended? */
			  ilu_string supertype,		/* OPTIONAL, RETAIN */
			  ilu_boolean *new,		/* OUT:  ilu_TRUE if new registration */
			  ILU_ERRS((internal, no_memory)) * err)
{
  ilu_Type n;

  ilu_HoldMutex(ilu_otmu);

  if (((n = _ilu_FindTypeByUID(uid, err)) != NIL) && ILU_ERROK(*err))
    { *new = ilu_FALSE; return n; };
  if (ILU_ERRNOK(*err))
    return NIL;

  n = build_new_type(ilu_record_tk, name, interface_name, interface_brand, uid, err);
  if (ILU_ERRNOK(*err)) return NIL;
  n->desc.Record.extensible = extensible;
  n->desc.Record.n_fields = n_fields;
  n->desc.Record.supertype = NIL;
  n->desc.Record.fields = (ilu_RecordField_s *) ilu_MallocE(sizeof(ilu_RecordField_s) * n_fields, err);
  if (ILU_ERRNOK(*err)) {
    free_new_type(n);
    return NIL;
  } else if ((supertype != NIL) && (AssignTypeForUID(supertype, &(n->desc.Record.supertype), err), ILU_ERRNOK(*err))) {
    ilu_free(n->desc.Record.fields);
    free_new_type(n);
    return NIL;
  } else if (!RegisterType(n, err)) {
    ilu_free(n->desc.Record.fields);
    free_new_type(n);
    return NIL;
  }
  memset ((void *) n->desc.Record.fields, 0, sizeof(ilu_RecordField_s) * n_fields);
  *new = ilu_TRUE;
  return n;  
}


/* call this function repeatedly to fill in the fields of a newly registered record type */
ilu_boolean
  ilu_RegisterRecordField (ilu_Type n,			/* record type */
			   ilu_cardinal field_index,	/* which field in record */
			   ilu_string field_name,	/* field name, RETAIN */
			   ilu_TypeUID field_type_uid,	/* field type, RETAIN */
			   ILU_ERRS((internal, no_memory, bad_param)) *err)
{
  ilu_HoldMutex(ilu_otmu);

  if (n == NIL || n->kind != ilu_record_tk)
    return ILU_ERR_CONS1(bad_param, err, minor, ((n == NIL) ? ilu_bpm_nil : ilu_bpm_not_record_type), ilu_FALSE);
  if (n->desc.Record.n_fields <= field_index)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_invalid_record_field_index, ilu_FALSE);
  n->desc.Record.fields[field_index].base.name = ilu_StrdupE(field_name, err);
  if (ILU_ERRNOK(*err)) return ilu_FALSE;
  AssignTypeForUID (field_type_uid, &(n->desc.Record.fields[field_index].base.type), err);
  if (ILU_ERRNOK(*err)) { ilu_free(n->desc.Record.fields[field_index].base.name); return ilu_FALSE; }
  return ilu_TRUE;
}


/* call this function to begin registering a new enumeration type */
ilu_Type
  ilu_RegisterEnumerationType (ilu_string name,		/* name, RETAIN */
			       ilu_string interface_name,	/* interface-name, RETAIN */
			       ilu_string interface_brand,	/* interface-brand, OPTIONAL, RETAIN */
			       ilu_string uid,		/* UID, RETAIN */
			       ilu_cardinal n_elements,	/* number of elements */
			       ilu_boolean *new,	/* OUT:  ilu_TRUE if new registration */
			       ILU_ERRS((internal, no_memory)) *err)
{
  ilu_Type n;

  ilu_HoldMutex(ilu_otmu);

  if (((n = _ilu_FindTypeByUID(uid, err)) != NIL) && ILU_ERROK(*err))
    { *new = ilu_FALSE; return n; };
  if (ILU_ERRNOK(*err))
    return NIL;

  n = build_new_type(ilu_enumeration_tk, name, interface_name, interface_brand, uid, err);
  if (ILU_ERRNOK(*err)) return NIL;
  n->desc.Enumeration.n_elements = n_elements;
  n->desc.Enumeration.elements = (ilu_EnumElement_s *) ilu_MallocE(sizeof(ilu_EnumElement_s) * n_elements, err);
  if (ILU_ERRNOK(*err)) {
    free_new_type(n);
    return NIL;
  } else if (!RegisterType(n, err)) {
    ilu_free(n->desc.Enumeration.elements);
    free_new_type(n);
    return NIL;
  }
  memset ((void *) n->desc.Enumeration.elements, 0, sizeof(ilu_EnumElement_s) * n_elements);
  *new = ilu_TRUE;
  return n;  
}


/* call this function repeatedly to fill in the fields of a newly registered enum type */
ilu_boolean
  ilu_RegisterEnumerationElement (ilu_Type n,			/* enumeration type */
				  ilu_cardinal element_index,	/* which element */
				  ilu_string element_name,	/* element name, RETAIN */
				  ilu_cardinal element_value,	/* element value */
				  ILU_ERRS((internal, no_memory, bad_param)) * err)
{
  ilu_HoldMutex(ilu_otmu);

  if (n == NIL || n->kind != ilu_enumeration_tk)
    return ILU_ERR_CONS1(bad_param, err, minor, ((n == NIL) ? ilu_bpm_nil : ilu_bpm_not_enumeration_type), ilu_FALSE);
  if (n->desc.Enumeration.n_elements <= element_index)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_invalid_enum_element_index, ilu_FALSE);
  n->desc.Enumeration.elements[element_index].name = ilu_StrdupE(element_name, err);
  if (ILU_ERRNOK(*err)) return ilu_FALSE;
  n->desc.Enumeration.elements[element_index].value = element_value;
  ILU_CLER(*err);
  return ilu_TRUE;
}


/* call this function to begin registering a new union type */
ilu_Type
  ilu_RegisterUnionType (ilu_string name,		/* name, RETAIN */
			 ilu_string interface_name,	/* interface-name, RETAIN */
			 ilu_string interface_brand,	/* interface-brand, OPTIONAL, RETAIN */
			 ilu_string uid,		/* UID, RETAIN */
			 ilu_TypeUID disc_type_uid,	/* discriminant type, RETAIN */
			 ilu_cardinal n_arms,		/* number of arms */
			 ilu_cardinal default_arm,	/* default arm (0 for none; 1-n otherwise) */
			 ilu_boolean others_allowed,	/* others allowed */
			 ilu_boolean *new,		/* OUT:  ilu_TRUE if new registration */
			 ILU_ERRS((internal, no_memory)) * err)
{
  ilu_Type n;

  ilu_HoldMutex(ilu_otmu);

  if (((n = _ilu_FindTypeByUID(uid, err)) != NIL) && ILU_ERROK(*err))
    { *new = ilu_FALSE; return n; };
  if (ILU_ERRNOK(*err))
    return NIL;

  if (n_arms > ILU_MAX_UNION_ARMS)
    return ILU_ERR_CONS1(imp_limit, err, minor, ilu_ilm_max_union_arms, NIL);

  n = build_new_type(ilu_union_tk, name, interface_name, interface_brand, uid, err);
  if (ILU_ERRNOK(*err)) return NIL;
  n->desc.Union.n_arms = n_arms;
  n->desc.Union.others_allowed = others_allowed;
  n->desc.Union.default_arm = default_arm;
  AssignTypeForUID (disc_type_uid, &n->desc.Union.discriminant, err);
  if (ILU_ERRNOK(*err)) {
    free_new_type(n);
    return NIL;
  }
  n->desc.Union.arms = (ilu_UnionArm_s *) ilu_MallocE(sizeof(ilu_UnionArm_s) * n_arms, err);
  if (ILU_ERRNOK(*err)) {
    free_new_type(n);
    return NIL;
  }
  if (!RegisterType(n, err)) {
    ilu_free(n->desc.Union.arms);
    free_new_type(n);
    return NIL;
  }
  memset ((void *) n->desc.Union.arms, 0, sizeof(ilu_UnionArm_s) * n_arms);
  *new = ilu_TRUE;
  return n;  
}


/* call this function repeatedly to fill in the fields of a newly registered record type */
ilu_UnionArm
  ilu_RegisterUnionArm (ilu_Type n,		/* record type */
			ilu_cardinal arm_index,	/* which arm */
			ilu_string arm_name,	/* arm name, RETAIN */
			ilu_TypeUID arm_type_uid,/* arm type, RETAIN */
			ilu_cardinal n_values,	/* number of values */
			ILU_ERRS((internal, no_memory, bad_param)) * err)
{
  ilu_HoldMutex(ilu_otmu);

  if (n == NIL || n->kind != ilu_union_tk)
    return ILU_ERR_CONS1(bad_param, err, minor, ((n == NIL) ? ilu_bpm_nil : ilu_bpm_not_union_type), NIL);
  if (n->desc.Union.n_arms <= arm_index)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_invalid_union_arm_index, NIL);
  n->desc.Union.arms[arm_index].base.name = ilu_StrdupE(arm_name, err);
  if (ILU_ERRNOK(*err)) return (ilu_UnionArm)ILU_NIL;
  n->desc.Union.arms[arm_index].n_values = n_values;
  if (n_values > 0)
    n->desc.Union.arms[arm_index].values =
      (ilu_ConstantValue_s *) ilu_MallocE(sizeof(ilu_ConstantValue_s) * n_values, err);
  else
    n->desc.Union.arms[arm_index].values = NIL;
  if (ILU_ERRNOK(*err)) { ilu_free(n->desc.Union.arms[arm_index].base.name); return (ilu_UnionArm)ILU_NIL; };
  AssignTypeForUID (arm_type_uid, &(n->desc.Union.arms[arm_index].base.type), err);
  if (ILU_ERRNOK(*err)) {
    ilu_free(n->desc.Union.arms[arm_index].base.name);
    ilu_free(n->desc.Union.arms[arm_index].values);
    return (ilu_UnionArm)ILU_NIL;
  }
  memset ((void *) n->desc.Union.arms[arm_index].values, 0, sizeof(ilu_ConstantValue_s) * n_values);
  ILU_CLER(*err);
  return (&(n->desc.Union.arms[arm_index]));
}


/* if there was more than one possible value for a union arm, fill them in here */
ilu_boolean
  ilu_RegisterUnionArmValue (ilu_UnionArm arm,		/* arm to fill in */
			     ilu_cardinal val_index,	/* which value */
			     ilu_ConstantValue value,	/* value */
			     ILU_ERRS((internal, no_memory, bad_param)) * err)
{
  ilu_HoldMutex(ilu_otmu);

  if (arm->n_values <= val_index)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, ilu_FALSE);
  if (value->kind == ilu_string_cvk)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, ilu_FALSE);
  arm->values[val_index] = *value;
  if (value->kind == ilu_enumeration_cvk)	/* make copy of arg string */
    arm->values[val_index].value.enumeration_val = ilu_StrdupE(value->value.enumeration_val, err);
  else
    ILU_CLER(*err);
  return ILU_ERROK(*err);
}


/* register an optional type */
ilu_Type
  ilu_RegisterOptionalType (ilu_string name,		/* name, RETAIN */
			    ilu_string interface_name,	/* interface-name, RETAIN */
			    ilu_string interface_brand,	/* interface-brand, OPTIONAL, RETAIN */
			    ilu_string uid,		/* UID, RETAIN */
			    ilu_TypeUID base_type_uid,	/* base type for optional, RETAIN */
			    ilu_boolean *new,		/* OUT:  ilu_TRUE if new registration */
			    ILU_ERRS((internal, no_memory)) * err)
{
  ilu_Type n;

  ilu_HoldMutex(ilu_otmu);

  if (((n = _ilu_FindTypeByUID(uid, err)) != NIL) && ILU_ERROK(*err))
    { *new = ilu_FALSE; return n; };
  if (ILU_ERRNOK(*err))
    return NIL;

  n = build_new_type(ilu_optional_tk, name, interface_name, interface_brand, uid, err);
  if (ILU_ERRNOK(*err)) return NIL;
  AssignTypeForUID (base_type_uid, &n->desc.Optional.type, err);
  if (ILU_ERRNOK(*err) || !RegisterType(n, err)) {
    free_new_type(n);
    return NIL;
  };
  *new = ilu_TRUE;
  return n;
}

#ifdef ILU_REFERENCE_TYPES

/* register an optional type */
ilu_Type
  ilu_RegisterReferenceType (ilu_string name,		/* name, RETAIN */
			     ilu_string interface_name,	/* interface-name, RETAIN */
			     ilu_string interface_brand,/* interface-brand, OPTIONAL, RETAIN */
			     ilu_string uid,		/* UID, RETAIN */
			     ilu_TypeUID base_type_uid,	/* base type for ref type, RETAIN */
			     ilu_boolean optionalp,	/* TRUE if optional */
			     ilu_boolean aliasedp,	/* TRUE if aliased */
			     ilu_boolean *new,		/* OUT:  ilu_TRUE if new registration */
			     ILU_ERRS((internal, no_memory)) * err)
{
  ilu_Type n;

  ilu_HoldMutex(ilu_otmu);

  if (((n = _ilu_FindTypeByUID(uid, err)) != NIL) && ILU_ERROK(*err))
    { *new = ilu_FALSE; return n; };
  if (ILU_ERRNOK(*err))
    return NIL;

  n = build_new_type(ilu_reference_tk, name, interface_name, interface_brand, uid, err);
  if (ILU_ERRNOK(*err)) return NIL;
  AssignTypeForUID (base_type_uid, &n->desc.Reference.type, err);
  if (ILU_ERRNOK(*err) || !RegisterType(n, err)) {
    free_new_type(n);
    return NIL;
  };
  n->desc.Reference.optionalp = optionalp;
  n->desc.Reference.aliasedp = aliasedp;
  *new = ilu_TRUE;
  return n;
}

#endif /* def ILU_REFERENCE_TYPES */

/* register an optional type */
ilu_Type
  ilu_RegisterObjectType (ilu_string name,		/* name, RETAIN */
			  ilu_string interface_name,		/* interface-name, RETAIN */
			  ilu_string interface_brand,		/* interface-brand, OPTIONAL, RETAIN */
			  ilu_string uid,		/* UID, RETAIN */
			  ilu_Class class_ptr,		/* class ptr from ilu_DefineObjectType */
			  ilu_boolean *new,		/* OUT:  ilu_TRUE if new registration */
			  ILU_ERRS((internal, no_memory)) * err)
{
  ilu_Type n;

  ilu_HoldMutex(ilu_otmu);

  if (((n = _ilu_FindTypeByUID(uid, err)) != NIL) && ILU_ERROK(*err))
    { *new = ilu_FALSE; return n; };
  if (ILU_ERRNOK(*err))
    return NIL;

  n = build_new_type(ilu_object_tk, name, interface_name, interface_brand, uid, err);
  if (ILU_ERRNOK(*err)) return NIL;
  type_desc(n).Object.objdesc = class_ptr;
  if (!RegisterType(n, err)) {
    free_new_type(n);
    return NIL;
  }
  *new = ilu_TRUE;
  return n;
}

/* register an alias for a type */
ilu_Type
  ilu_RegisterAliasType (ilu_string name,	/* name, RETAIN */
			 ilu_string interface_name,	/* interface-name, RETAIN */
			 ilu_string interface_brand,	/* interface-brand, OPTIONAL, RETAIN */
			 ilu_string uid,	/* UID, RETAIN */
			 ilu_TypeUID base_type_uid,/* base type, RETAIN */
			 ilu_boolean *new,		/* OUT:  ilu_TRUE if new registration */
			 ILU_ERRS((internal, no_memory)) * err)
{
  ilu_Type        n;
  char *key;

  ilu_HoldMutex(ilu_otmu);

  EnsureHashTables();

  key = ilu_Strcat3E(interface_name, ".", name, err);
  if (ILU_ERRNOK(*err)) return NIL;
  n = _ilu_FindTypeByName(key, err);
  if ((n != NIL) && ILU_ERROK(*err)) {
    *new = ilu_FALSE;
    ilu_free(key);
    return n;
  };
  if (ILU_ERRNOK(*err))
    { ilu_free(key); return NIL; };

  n = _ilu_FindTypeByUID(uid, err);
  if (ILU_ERRNOK(*err))
    { ilu_free(key) ; return NIL; };
  if (n == NIL) {
    n = build_new_type(ilu_alias_tk, name, interface_name,
		       interface_brand, uid, err);
    AssignTypeForUID(base_type_uid, &n->desc.Alias.type, err);
    if (ILU_ERRNOK(*err)) {
      free_new_type(n);
      ilu_free(key);
      return NIL;
    } else if (strcmp(uid, base_type_uid) != 0) {
      /* new UID, so register it fully */
      ilu_free(key);
      if (!RegisterType(n, err)) {
	free_new_type(n);
	return NIL;
      }
    } else {
      if (!ilu_hash_AddToTable(TypeTableByName,
				key, n)) {
	free_new_type(n);
	ilu_free(key);
	return ILU_ERR_CONS1(internal, err, minor, ilu_im_broken, NIL);
      }
    }
    *new = ilu_TRUE;
  } else {
    ilu_TypeName_s *ptr;
    type_names(n) = ilu_ReallocE(type_names(n),
		sizeof(ilu_TypeName_s) * (1 + type_names_count(n)),
				 err);
    if (ILU_ERRNOK(*err))
      { ilu_free(key); return NIL; };
    ptr = &type_names(n)[type_names_count(n)];
    type_names_count(n) += 1;
    ptr->name = ilu_StrdupE(name, err);
    if (ILU_ERRNOK(*err)) {
      ilu_free(key); return NIL;
    }
    ptr->interface_name.name = ilu_StrdupE(interface_name, err);
    if (ILU_ERRNOK(*err)) {
      ilu_free(key); return NIL;
    }
    ptr->interface_name.brand = ilu_StrdupE(interface_brand, err);
    if (ILU_ERRNOK(*err)) {
      ilu_free(key); return NIL;
    }
    if (!ilu_hash_AddToTable(TypeTableByName, key, n)) {
      ilu_free(key);
      ilu_free(ptr);
      return ILU_ERR_CONS1(internal, err, minor, ilu_im_broken, NIL);
    }
    *new = ilu_FALSE;
  }

  /* now link all type pointers which have been waiting for this type */
  if (strcmp(uid, base_type_uid) != 0)	/* so we don't point to self! */
    _ilu_update_unsatisfied_types(n);
  /* else, wait for base type to be registered to resolve pointers */

  return n;
}

/* register an alias for a type */
ilu_Type
  ilu_RegisterPrimitiveType (ilu_string name,	/* name, RETAIN */
			     ilu_string interface_name,	/* interface-name, RETAIN */
			     ilu_string interface_brand,/* interface-brand, OPTIONAL, RETAIN */
			     ilu_string uid,	/* UID, RETAIN */
			     ilu_TypeKind kind,	/* kind of primitive type */
			     ilu_boolean *new,	/* OUT:  true if new registration */
			     ILU_ERRS((internal, no_memory, bad_param)) * err)
{
  ilu_Type n;

  ilu_HoldMutex(ilu_otmu);

  if (((n = _ilu_FindTypeByUID(uid, err)) != NIL) && ILU_ERROK(*err))
    { *new = ilu_FALSE; return n; };
  if (ILU_ERRNOK(*err))
    return NIL;

  n = build_new_type(kind, name, interface_name, interface_brand, uid, err);
  if (!RegisterType(n, err)) {
    free_new_type(n);
    return NIL;
  }
  *new = ilu_TRUE;
  return n;
}


/* Locking:  L1 < otmu */
ilu_boolean
  ilu_CheckSubtype (ilu_Type sub, ilu_Type super)
{
  ilu_boolean returnval = ilu_FALSE;

  if (sub->kind != super->kind)
    return ilu_FALSE;
  ilu_AcquireMutex(ilu_otmu);
  if (sub == super) {
    returnval = ilu_FALSE;
  } else if ((sub->kind == ilu_object_tk) && (super->kind == ilu_object_tk)) {
    returnval = ilu_IsSubObjectType(sub->desc.Object.objdesc, super->desc.Object.objdesc);
  } else if ((sub->kind == ilu_record_tk) && (sub->desc.Record.supertype != NIL)) {
    ilu_Type t = sub;
    for (t = sub, returnval = ilu_FALSE;  t != NIL;  t = t->desc.Record.supertype)
      if (t == super) {
	returnval = ilu_TRUE; break;
      };
  } else {
    returnval = ilu_FALSE;
  }
  ilu_ReleaseMutex(ilu_otmu);
  return returnval;
}

