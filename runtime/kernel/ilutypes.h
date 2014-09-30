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

$Id: ilutypes.h,v 1.51 1999/08/03 01:53:02 janssen Exp $
*/
/* Last edited by Mike Spreitzer September 8, 1997 8:28 am PDT */

#ifndef _ILUTYPES_H_
#define _ILUTYPES_H_

/* This file defines the abstractions needed by the fuller notion of
   types required for support of ILU variant types. */

#ifdef ADD_TYPE_REGISTRATION_SUPPORT

/***********************************************************************/
/***  Types  ***********************************************************/
/***********************************************************************/

#define ILU_MAX_UNION_ARMS 32766	/* for memory efficiency in implementation */

typedef struct _ilu_Type_s *			ilu_Type;
typedef ilu_string				ilu_TypeUID;
typedef struct _ilu_Procedure_s *		ilu_Procedure;
typedef struct _ilu_ProcedureArgument_s *	ilu_ProcedureArgument;
typedef struct _ilu_RecordField_s *		ilu_RecordField;
typedef struct _ilu_UnionArm_s *		ilu_UnionArm;
typedef struct _ilu_EnumElement_s *		ilu_EnumElement;
typedef struct _ilu_BasicField_s *		ilu_VariantRevelation;
typedef struct _ilu_ObjectType_s *		ilu_ObjectType;
typedef union _ilu_TypeDescription_u *		ilu_TypeDescription;
typedef struct _ilu_ConstantValue_s *		ilu_ConstantValue;
typedef struct _ilu_Constant_s *		ilu_Constant;
typedef struct _ilu_ExceptionType_s *		ilu_ExceptionType;
typedef struct _ilu_OpaqueValue_s *		ilu_OpaqueValue;	
typedef struct _ilu_Interface_s *		ilu_Interface;

typedef struct _ilu_BasicField_s {
  ilu_string		name;
  ilu_Type		type;
} ilu_BasicField_s;

typedef struct _ilu_BasicField_s ilu_VariantRevelation_s;  

typedef enum {In, Out, InOut} ArgDirection;

#if 0
typedef struct _ilu_Procedure_s {
  ilu_string		name;		/* procedure name */
  ilu_Type		class;		/* class on which this was originally defined */
  ilu_Vector		arguments;	/* list of ProcedureArgument */
  ilu_Type		returnType;	/* NIL (if void) or the return type */
  ilu_Vector		exceptions;	/* list of Exception */
  ilu_cardinal		wire_id;	/* value to be sent on the wire to identify this procedure */
  ilu_boolean		functional : 1;	/* 1 if "functional", 0 otherwise (see ILU docs) */
  ilu_boolean		asynch : 1;	/* 1 if "asynchronous", 0 otherwise (see ILU docs) */
  ilu_string		doc_string;	/* NIL or documentation string */
} ilu_Procedure_s;

typedef struct _ilu_ProcedureArgument_s {
  ilu_BasicField_s	base;		/* basic name and type */
  ilu_ArgDirection	direction : 16;	/* In, Out, or InOut */
  ilu_boolean		sibling : 1;	/* 1 for sibling, 0 for not */	
} ilu_Procedure_s;
#endif

typedef struct _ilu_RecordField_s {
  ilu_BasicField_s	base;		/* name and type */
} ilu_RecordField_s;

typedef struct _ilu_UnionArm_s {
  ilu_BasicField_s	base;		/* name and type */
  ilu_cardinal		n_values;	/* number of values */
  struct _ilu_ConstantValue_s *	values;	/* list of ilu_ConstantValue_s; values for which this arm selected */
} ilu_UnionArm_s;

typedef struct _ilu_EnumElement_s {
  ilu_string		name;		/* the name */
  ilu_cardinal		value;		/* the value to associate with it */
} ilu_EnumElement_s;

#if 0
typedef struct _ilu_ObjectType_s {
  ilu_Vector		superclasses;	/* list of supertypes */
  ilu_string		brand;		/* NIL or brand for type */
  ilu_string		corba_rep_id;	/* CORBA RepositoryID, if any */
  ilu_string		singleton;	/* NIL or singleton information */
  ilu_boolean		collectible : 1;/* 1 for collectible, 0 otherwise */
  ilu_boolean		optional : 1;	/* 1 for optional, 0 otherwise */
  ilu_Vector		methods;	/* list of Procedure */
  ilu_string		doc_string;	/* NIL or doc string */
} ilu_ObjectType_s;
#endif

typedef struct _ilu_Interface_s {
  ilu_string		name;
  ilu_string		brand;
} ilu_Interface_s;

typedef union _ilu_TypeDescription_u {

#ifdef ILU_FIXED_POINT_SUPPORT

  struct {
    ilu_Bignum		min_numerator;	/* NIL to represent none */
    ilu_Bignum		max_numerator;	/* NIL to represent none */
    ilu_Bignum		denominator;

    /* we cache some information from the ISL compiler here; it could be recomputed
       in the kernel, but at the expense of marshalling computations */
    unsigned int	corba_fixed_digits : 15;
    unsigned int	corba_fixed_decimal_places : 14;
    unsigned int	size_in_bytes : 3;	/* actually ilu_FixedPointRangeSize */
  } Fixedpoint;

#endif /* def ILU_FIXED_POINT_SUPPORT */

  struct {
    ilu_Type		type;		/* the base type for the sequence */
    ilu_cardinal	limit;		/* any upper limit on the number of elements */
  } Sequence;

  struct {
    ilu_Type		type;		/* the base type for the array */
    ilu_cardinal	n_dims;		/* number of dimensions */
    ilu_cardinal *	dims;		/* dimensions */
  } Array;

  struct {
    ilu_Type		supertype;	/* supertype, if any */
    unsigned int	extensible : 1;	/* whether or not it can be extended */
    unsigned int	n_fields : 31;	/* number of fields */
    ilu_RecordField_s *	fields;		/* vector of ilu_RecordField_s */
  } Record;

  struct {
    ilu_cardinal	n_elements;	/* number of elements in enumeration */
    ilu_EnumElement_s *	elements;	/* vector of ilu_EnumElement_s */
  } Enumeration;

  struct {
    ilu_Type		discriminant;	/* type used as discriminant */
    ilu_UnionArm_s *	arms;		/* of ilu_UnionArm_s */
    unsigned int n_arms : 15;
    unsigned int others_allowed : 1;	/* 0 for false, 1 for true */
    unsigned int default_arm : 15;	/* 0 indicates no default; 1-n indicates which of "arms" is default */
  } Union;

  struct {
    ilu_Type		type;		/* base type */
  } Optional;

#ifdef ILU_REFERENCE_TYPES

  struct {
    ilu_Type		type;		/* base type */
    ilu_boolean		optionalp;	/* TRUE if optional */
    ilu_boolean		aliasedp;	/* TRUE if aliased */
  } Reference;

#endif /* def ILU_REFERENCE_TYPES */

  struct {
    ilu_Type		type;		/* alias type */
  } Alias;

  struct {
    ilu_string		brand;		/* brand (may be NIL) */
  } Variant;

  struct {

    /* OMG any doesn't require full knowledge about the innards of an
       object type, like what the procedures are, and their arguments.
       All that's required is really the type ID.  So we could punt
       on saving all that other stuff and just save an ilu_Class for
       object types.
       */

    ilu_Class		objdesc;

    /* Or, we could "do it right", and fill in all the information in
       the ISL file, right here.
       */

#if 0
    ilu_ObjectType	objdesc;	/* description of object type; separate because of size */
#endif

  } Object;

  struct {
    ilu_cardinal	limit;		/* max number of bytes */
    ilu_cardinal	char_set;	/* char set */
    ilu_string		language;	/* language of string (often "i-default") */
  } String;

} ilu_TypeDescription_u ;

typedef struct _ilu_TypeName_s {
  ilu_Interface_s	interface_name;	/* name of interface in which it's defined */
  ilu_string		name;		/* the name */
} ilu_TypeName_s;

typedef struct _ilu_Type_s {
  ilu_cardinal		n_names;	/* size of name list (>= 1) */
  ilu_TypeName_s	*names;		/* list of names for type -- extra names are aliases */
  ilu_string		uid;		/* Unique ID or CORBA repository ID */
  ilu_TypeKind		kind;		/* the basic kind of Type */
  ilu_TypeDescription_u	desc;		/* description */
} ilu_Type_s;

/***********************************************************************/
/***  Constant  ********************************************************/
/***********************************************************************/

typedef enum _ilu_ConstantValueKind_e {
  ilu_byte_cvk			= 0,
  ilu_shortinteger_cvk		= 1,
  ilu_integer_cvk		= 2,
  ilu_shortcardinal_cvk		= 3,
  ilu_cardinal_cvk		= 4,
  ilu_shortreal_cvk		= 5,
  ilu_real_cvk			= 6,
  ilu_boolean_cvk		= 7,
  ilu_enumeration_cvk		= 8,
  ilu_string_cvk		= 9
} ilu_ConstantValueKind;

typedef struct _ilu_ConstantValue_s {

  ilu_ConstantValueKind kind;

  union {
    ilu_byte		byte_val;
    ilu_shortinteger	shortinteger_val;
    ilu_integer		integer_val;
    ilu_shortcardinal	shortcardinal_val;
    ilu_cardinal	cardinal_val;
    ilu_shortreal	shortreal_val;
    ilu_real		real_val;
    ilu_boolean		boolean_val;
    ilu_string		enumeration_val;
    ilu_string		string_val;
  } value;

} ilu_ConstantValue_s;

typedef struct _ilu_Constant_s {
  ilu_string		name;		/* name of constant */
  ilu_Type		type;		/* type of constant */
  ilu_string		interface_name;	/* interface constant defined in */
  ilu_ConstantValue_s	value;		/* value of constant */
} ilu_Constant_s;

/***********************************************************************/
/***  Exception  *******************************************************/
/***********************************************************************/

struct ilu_ExceptionType_s {
  ilu_string		name;		/* name of exception */
  ilu_string		interface_name;	/* name of interface in which exception is defined */
  ilu_Type		type;		/* type of exception or NIL */
  ilu_string		corba_rep_id;	/* CORBA RepositoryID, if any */
  ilu_string		doc_string;
};

/***********************************************************************/
/***  Functions exported  **********************************************/
/***********************************************************************/

/* Locking:  L1 >= {otmu}

   When calling a series of these functions around one logical type,
   as in

   ilu_Type t = ilu_RegisterRecordType (...);
   ilu_RegisterRecordTypeField (t, ...);
   ilu_RegisterRecordTypeField (t, ...);
   ilu_RegisterRecordTypeField (t, ...);

   the lock should be held across the set of calls.
*/

/*
   Note that RECORD types have "fields", UNION types have "arms",
   ENUMERATION types have "elements", VARIANT types have "revelations".
*/

#ifdef ILU_FIXED_POINT_SUPPORT

ILU_PUBLIC ilu_Type
  ilu_RegisterFixedpointType (ilu_string,		/* name, RETAIN */
			      ilu_string,		/* interface-name, RETAIN */
			      ilu_string,		/* interface-brand, OPTIONAL, RETAIN */
			      ilu_string,		/* UID, RETAIN */
			      ilu_Bignum,		/* min numerator, OPTIONAL, RETAIN */
			      ilu_Bignum,		/* max numerator, OPTIONAL, RETAIN */
			      ilu_Bignum,		/* denominator, RETAIN */
			      ilu_cardinal,		/* if non-zero, digits as CORBA 'fixed' */
			      ilu_cardinal,		/* decimal places as CORBA 'fixed' */
			      ilu_FixedPointRangeSize,	/* size of range */
			      ilu_boolean *,		/* OUT:  true if new registration */
			      ILU_ERRS((internal, no_memory)) *);

#endif /* def ILU_FIXED_POINT_SUPPORT */

/* register a new string type */
ILU_PUBLIC ilu_Type
  ilu_RegisterStringType (ilu_string,		/* name, RETAIN */
			  ilu_string,		/* interface-name, RETAIN */
			  ilu_string,		/* interface-brand, OPTIONAL, RETAIN */
			  ilu_string,		/* UID, RETAIN */
			  ilu_string,		/* language of string, RETAIN */
			  ilu_cardinal,		/* limit (0 if none) */
			  ilu_cardinal,		/* char_set */
			  ilu_boolean *,	/* OUT:  true if new registration */	
			  ILU_ERRS((internal, no_memory)) *);

/* register a new sequence type */
ILU_PUBLIC ilu_Type
  ilu_RegisterSequenceType (ilu_string,		/* name, RETAIN */
			    ilu_string,		/* interface-name, RETAIN */
			    ilu_string,		/* interface-brand, OPTIONAL, RETAIN */
			    ilu_string,		/* UID, RETAIN */
			    ilu_TypeUID,	/* base type, RETAIN */
			    ilu_cardinal,	/* limit (0 if none) */
			    ilu_boolean *,	/* OUT:  true if new registration */	
			    ILU_ERRS((internal, no_memory)) *);

/* Register a new array type */
ILU_PUBLIC ilu_Type
  ilu_RegisterArrayType (ilu_string,		/* name, RETAIN */
			 ilu_string,		/* interface-name, RETAIN */
			 ilu_string,		/* interface-brand, OPTIONAL, RETAIN */
			 ilu_string,		/* UID, RETAIN */
			 ilu_TypeUID,		/* base type, RETAIN */
			 ilu_cardinal,		/* number of dimensions */
			 ilu_cardinal *,	/* pointer to sequence of dimensions, RETAIN */
			 ilu_boolean *,		/* OUT:  true if new registration */	
			 ILU_ERRS((internal, no_memory)) *);

/* call this function to begin registering a new record type */
ILU_PUBLIC ilu_Type
  ilu_RegisterRecordType (ilu_string,		/* name, RETAIN */
			  ilu_string,		/* interface-name, RETAIN */
			  ilu_string,		/* interface-brand, OPTIONAL, RETAIN */
			  ilu_string,		/* UID, RETAIN */
			  ilu_cardinal,		/* number of fields */
			  ilu_boolean,		/* extensible? */
			  ilu_string,		/* UID of supertype; OPTIONAL, RETAIN */
			  ilu_boolean *,		/* OUT:  true if new registration */	
			  ILU_ERRS((internal, no_memory)) *);

/* call this function repeatedly to fill in the fields of a newly registered record type */
ILU_PUBLIC ilu_boolean
  ilu_RegisterRecordField (ilu_Type,		/* record type */
			   ilu_cardinal,	/* field index */
			   ilu_string,		/* field name, RETAIN */
			   ilu_TypeUID,		/* field type, RETAIN */
			   ILU_ERRS((internal, no_memory, bad_param)) *);

/* call this function to begin registering a new enumeration type */
ILU_PUBLIC ilu_Type
  ilu_RegisterEnumerationType (ilu_string,	/* name, RETAIN */
			       ilu_string,	/* interface-name, RETAIN */
			       ilu_string,	/* interface-brand, OPTIONAL, RETAIN */
			       ilu_string,	/* UID, RETAIN */
			       ilu_cardinal,	/* number of elements */
			       ilu_boolean *,	/* OUT:  true if new registration */	
			       ILU_ERRS((internal, no_memory)) *);

/* call this function repeatedly to fill in the fields of a newly registered enum type */
ILU_PUBLIC ilu_boolean
  ilu_RegisterEnumerationElement (ilu_Type,	/* enumeration type */
				  ilu_cardinal,	/* which element */
				  ilu_string,	/* element name, RETAIN */
				  ilu_cardinal,	/* element value */
				  ILU_ERRS((internal, no_memory, bad_param)) *);

/* call this function to begin registering a new union type */
ILU_PUBLIC ilu_Type
  ilu_RegisterUnionType (ilu_string,		/* name, RETAIN */
			 ilu_string,		/* interface-name, RETAIN */
			 ilu_string,		/* interface-brand, OPTIONAL, RETAIN */
			 ilu_string,		/* UID, RETAIN */
			 ilu_TypeUID,		/* discriminant type, RETAIN */
			 ilu_cardinal,		/* number of arms */
			 ilu_cardinal,		/* default arm (0 for none; 1-n otherwise) */
			 ilu_boolean,		/* others allowed */
			 ilu_boolean *,		/* OUT:  true if new registration */	
			 ILU_ERRS((internal, no_memory)) *);

/* call this function repeatedly to fill in the fields of a newly registered union type */
ILU_PUBLIC ilu_UnionArm
  ilu_RegisterUnionArm (ilu_Type,		/* union type */
			ilu_cardinal,		/* which arm? */
			ilu_string,		/* arm name, RETAIN */
			ilu_TypeUID,		/* arm type, RETAIN */
			ilu_cardinal,		/* number of values */
			ILU_ERRS((internal, no_memory, bad_param)) *);

/* if there was more than one possible value for a union arm, fill them in here */
ILU_PUBLIC ilu_boolean
  ilu_RegisterUnionArmValue (ilu_UnionArm,	/* arm to fill in */
			     ilu_cardinal,	/* value index */
			     ilu_ConstantValue,	/* value, RETAIN */
			     ILU_ERRS((internal, no_memory)) *);

/* register an optional type */
ILU_PUBLIC ilu_Type
  ilu_RegisterOptionalType (ilu_string,		/* name, RETAIN */
			    ilu_string,		/* interface-name, RETAIN */
			    ilu_string,		/* interface-brand, OPTIONAL, RETAIN */
			    ilu_string,		/* UID, RETAIN */
			    ilu_TypeUID,	/* base type for optional, RETAIN */
			    ilu_boolean *,	/* OUT:  true if new registration */	
			    ILU_ERRS((internal, no_memory)) *);

#ifdef ILU_REFERENCE_TYPES

/* register an reference type */
ILU_PUBLIC ilu_Type
  ilu_RegisterReferenceType (ilu_string,	/* name, RETAIN */
			     ilu_string,	/* interface-name, RETAIN */
			     ilu_string,	/* interface-brand, OPTIONAL, RETAIN */
			     ilu_string,	/* UID, RETAIN */
			     ilu_TypeUID,	/* base type for reference, RETAIN */
			     ilu_boolean,	/* TRUE if optional */
			     ilu_boolean,	/* TRUE if aliased */
			     ilu_boolean *,	/* OUT:  true if new registration */	
			     ILU_ERRS((internal, no_memory)) *);

#endif /* def ILU_REFERENCE_TYPES */

/* register an alias for a type */
ILU_PUBLIC ilu_Type
  ilu_RegisterAliasType (ilu_string,		/* name, RETAIN */
			 ilu_string,		/* interface-name, RETAIN */
			 ilu_string,		/* interface-brand, OPTIONAL, RETAIN */
			 ilu_string,		/* UID, RETAIN */
			 ilu_TypeUID,		/* base type, RETAIN */
			 ilu_boolean *,		/* OUT:  true if new registration */	
			 ILU_ERRS((internal, no_memory)) *);

/* register an object type */
ILU_PUBLIC ilu_Type
  ilu_RegisterObjectType (ilu_string,		/* name, RETAIN */
			  ilu_string,		/* interface-name, RETAIN */
			  ilu_string,		/* interface-brand, OPTIONAL, RETAIN */
			  ilu_string,		/* UID, RETAIN */
			  ilu_Class,		/* class ptr from ilu_DefineObjectType */
			  ilu_boolean *,	/* OUT:  true if new registration */	
			  ILU_ERRS((internal, no_memory)) *);

/* add simple type */
ILU_PUBLIC ilu_Type
  ilu_RegisterPrimitiveType (ilu_string,	/* name, RETAIN */
			     ilu_string,	/* interface-name, RETAIN */
			     ilu_string,	/* interface-brand, OPTIONAL, RETAIN */
			     ilu_string,	/* UID, RETAIN */
			     ilu_TypeKind,	/* which primitive type */
			     ilu_boolean *,	/* OUT:  true if new registration */	
			     ILU_ERRS((internal, no_memory, bad_param)) *);

ILU_PUBLIC ilu_boolean ilu_CheckForUnsatisfiedTypeRefs(void);

/* Locking:  L1 < otmu */

/* find type by TypeUID */
ILU_PUBLIC ilu_Type
  ilu_FindTypeByUID (ilu_TypeUID,		/* UID to search for, RETAIN */
		     ILU_ERRS((internal)) *);

/* find type by interface name and type name */
ILU_PUBLIC /* OPTIONAL */ ilu_Type
  ilu_FindTypeByName (ilu_string,		/* "interface.type", RETAIN */
		      ILU_ERRS((internal)) *);

/* Locking:  L1 < otmu */
ILU_PUBLIC ilu_boolean
  ilu_CheckSubtype (ilu_Type /* possible subtype */,
		    ilu_Type /* putative_supertype */);
/* returns ilu_TRUE if first parameter is a subtype of the second
   parameter.  Returns ilu_FALSE otherwise. */

/***********************************************************************/
/***  Macros  **********************************************************/
/***********************************************************************/

#define type_name(t)		((t)->names[0].name)
#define type_interface(t)	((t)->names[0].interface_name)
#define type_interface_name(t)	((t)->names[0].interface_name.name)
#define type_interface_brand(t)	((t)->names[0].interface_name.brand)
#define type_names(t)		((t)->names)
#define type_names_count(t)	((t)->n_names)
#define type_uid(t)		((t)->uid)
#define type_kind(t)		((t)->kind)
#define type_desc(t)		((t)->desc)

#endif /* def ADD_TYPE_REGISTRATION_SUPPORT */

/* ====================================================================== */
/*	support for Pickles						  */
/* ====================================================================== */

typedef struct {
  ilu_cardinal	pi_len;
  ilu_bytes	pi_bytes;
} ilu_Pickle;

/* ==================== pickle marshalling ==================== */

/* The ilu_Type parm in these calls is provided for a future extension
   in which there are multiple Pickle types. */

ILU_PUBLIC void
  ilu_OutputPickle (ilu_Call,		/* the call in progress */
		    ilu_Pickle,		/* pointer to the actual value */
		    ilu_Type,		/* OPTIONAL, RETAIN, pickle type */
		    ILU_ERRS((IoErrs)) *);

ILU_PUBLIC ilu_cardinal
  ilu_SizeOfPickle (ilu_Call,		/* the call in progress */
		    ilu_Pickle,		/* pointer to the actual value */
		    ilu_Type,		/* OPTIONAL, RETAIN, pickle type */
		    ILU_ERRS((IoErrs)) *);

ILU_PUBLIC ilu_boolean			/* the value read, PASS */
  ilu_InputPickle (ilu_Call,		/* the call in progress */
		   ilu_Pickle *,	/* OUT:  the value input, PASS */
		   ilu_Type,		/* OPTIONAL, RETAIN, pickle type */
		   ILU_ERRS((IoErrs)) *);

ILU_PUBLIC ilu_boolean
  ilu_FreePickle (ilu_Pickle *,		/* IN:  pointer to pickle */
		  ilu_Error *err);
/* Frees storage internal to the pickle, but does not free the storage
   directly pointed to by the pickle pointer. */

/* ==================== default pickle marshalling ==================== */

ILU_PUBLIC void
  _ilu_OutputPickle (ilu_Call,		/* the call in progress */
		     ilu_Pickle,	/* pointer to the actual value */
		     ilu_Type,		/* pickle type */
		     ILU_ERRS((IoErrs)) *);

ILU_PUBLIC ilu_cardinal
  _ilu_SizeOfPickle (ilu_Call,		/* the call in progress */
		     ilu_Pickle,	/* pointer to the actual value */
		     ilu_Type,		/* pickle type */
		     ILU_ERRS((IoErrs)) *);

ILU_PUBLIC ilu_boolean			/* the value read, PASS */
  _ilu_InputPickle (ilu_Call,		/* the call in progress */
		    ilu_Pickle *,	/* OUT:  the value input, PASS */
		    ilu_Type,		/* pickle type */
		    ILU_ERRS((IoErrs)) *);

/* ==================== LSR API to pickles ==================== */

/* Locking unconstrained */

ILU_PUBLIC ilu_boolean
  ilu_StartPickle (ilu_Call_s *,	/* Call struct to init */
		   ilu_Type,		/* constraint type, if any */
		   ilu_Error *);	/* error return */
/* initialize a call structure for use in marshalling or
   unmarshalling an ILU pickled value.  If a constraint
   type is specified, the actual type of the value marshalled
   into the pickle will have to conform to a "prefix" relationship
   with the constraint type.  The prefix relationship is not yet
   defined.
   */

ILU_PUBLIC ilu_boolean
  ilu_WritePickle (ilu_Call,
		   ilu_cardinal /* argSize, HINT -- can be zero */,
		   ilu_string	/* type_id, IN, RETAIN */,
		   ilu_Error *	/* err */);
/* write the type tag to the pickle, and allocate space
   for the pickle */

ILU_PUBLIC ilu_TypeKind	/* OUT, belongs to "pickle" arg */
  ilu_PickleTypeKind (ilu_Pickle	/* pickle, IN, RETAIN */,
		      ILU_ERRS((bad_typecode, marshal)) *);
/* Returns the typekind of the pickled value, or raises
   "bad_typecode" if it can't figure out the typecode,
   which might be quite reasonable. */

ILU_PUBLIC ilu_string	/* OUT, belongs to "pickle" arg */
  ilu_PickleType (ilu_Pickle	/* pickle, IN, RETAIN */,
		  ilu_Error *	/* error */);
/* Returns the type ID of the pickle */

ILU_PUBLIC ilu_boolean
  ilu_PickleTypes (ilu_Pickle	/* pickle, IN, RETAIN */,
		  ilu_string **,/* vector of type UIDs, OUT, PASS */
		  ilu_cardinal *,	/* number of type UIDs, OUT */
		  ilu_Error *	/* error */);
/* Returns the type IDs in the pickle.  Since most types have
   only one type ID, it's best to first call ilu_PickleTypeKind(),
   then either ilu_PickleTypes(), for object types or record types
   (which if extensible may have multiple types), or ilu_PickleType()
   for other types.  The vector of type IDs returned, and the
   strings in the vector, are returned to the caller, who must free
   them when finished.  The types are returned in most-derived to
   least-derived order. */

ILU_PUBLIC ilu_boolean
  ilu_ReadPickle (ilu_Call /* call */,
		  ilu_Pickle /* pickle, IN, PASS */,
		  ilu_Error * /* err */);
/* begin parsing an existing pickle, returning its type id
   in the type_ID parameter. */

ILU_PUBLIC ilu_boolean
  ilu_EndPickle (ilu_Call /* call */,
		 ilu_Pickle * /* pickle, OUT, OPTIONAL, PASS */,
		 ilu_Error * /* err */);
/* finish either unmarshalling or marshalling a pickled
   value.  Returns pickle in second arg, if desired.
   Frees it, and its storage, otherwise.  */

#endif /* ndef _ILUTYPES_H_ */
