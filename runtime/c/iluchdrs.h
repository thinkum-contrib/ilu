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
/* $Id: iluchdrs.h,v 1.178 1999/08/03 01:55:25 janssen Exp $ */
/* Last edited by Mike Spreitzer June 1, 1998 10:01 am PDT */

#ifndef __ilu_c_h_
#define __ilu_c_h_

#include <stddef.h>

#include <iluxport.h>

/* define dllexport to support building DLLs on Win32 */
#if defined(WIN32)
#if defined(ILU_BUILDING_RUNTIME)
#define ILU_RUNTIME_PUBLIC __declspec(dllexport) extern
#define ILU_RUNTIME_PUBLIC_CLASS  class __declspec(dllexport)
#else
#define ILU_RUNTIME_PUBLIC __declspec(dllimport) extern
#define ILU_RUNTIME_PUBLIC_CLASS class __declspec(dllimport)
#endif /* defined(ILU_BUILDING_RUNTIME) */
#else
#define ILU_RUNTIME_PUBLIC extern
#define ILU_RUNTIME_PUBLIC_CLASS class
#endif /* defined(WIN32) */

#define ILU_OPTIONAL(x)	x
#define ILU_PASS(x)		x
#define ILU_RETAIN(x)	x
#define ILU_GLOBAL(x)	x
 
/* Note:  Things beginning with "ilu_" are exported from the ILU kernel,
          things beginning with "ILU_C_" are from the ILU ANSI C runtime
          for application use, and things beginning with "_ILU_C_" are
	  exported for use only by automatically generated code. */

/*==================================================*/
/*==================================================*/
/*          The Embedded Object System              */
/*==================================================*/
/*==================================================*/

/*
 * An object is represented by a pointer to an ILU_C_Object.  An
 * object is constructed from an instance data pointer and an
 * ILU_C_Class.  The class and object data structures are
 * deliberately opaque; applications call procedures in this and the
 * generated C header files to create classes and objects.
 * 
 * The ILU_C_Class consists of a dispatch table, describing methods
 * declared in the corresponding ISL, plus a finalization method.
 * There can be more than one ILU_C_Class of objects that implement
 * a given ISL object type.  For each object type declared in ISL, a
 * "default class" is generated, with a defined name-munge to
 * compute the names of the default class's methods from the names
 * of the object type's methods.  Procedures to create instances of
 * this default class are declared in Ifc.h (generated from
 * Ifc.isl), as Ifc_Tyname__CreateTrue; you can create instances of
 * other (ILU_C_Class)es by calling ILU_C_CreateTrueObject.
 */

typedef struct _ILU_C_Object_s ILU_C_Object;
/*
 * A pointer to one of these represents an object in the object
 * system we're embedding in C.
 */

typedef struct _ILU_C_Class_s *ILU_C_Class;
/* Represents a class in the embedded object system. */

/* L1, L2 unconstrained --- COULD BE ANYTHING! */
typedef void (*ILU_C_FinalizationProc)(void * instanceData);
/*
 * The finalization procedure for a class of objects.  It's given
 * the "instance data" pointer provided at the creation of the
 * object being finalized.  Ownership of (instanceData) is passed to
 * this procedure.
 */

typedef struct _ILU_C_Server_s *ILU_C_Server;
/* The C mapping of an ILU "server". */

/* insideServer => Inside(server, class) */
/* otherwise Main invariant holds */
ILU_RUNTIME_PUBLIC ILU_C_Object *
ILU_C_CreateTrueObject(ILU_C_Class c,
		       ILU_OPTIONAL(ilu_string) instance_handle,
		       ILU_OPTIONAL(ILU_C_Server) server,
		       void *instanceData,
		       ilu_boolean inside_server);
/*
 * Create a true object of the given class.  Ownership of
 * (instanceData) is passed to the callee, who will eventually pass
 * it to the class's finalization procedure (if this object is ever
 * finalized).
 */

/*==================================================*/
/*==================================================*/
/*          Other Introductory Dreck                */
/*==================================================*/
/*==================================================*/

typedef struct {
    unsigned long	_maximum;
    unsigned long	_length;
    char		*_buffer;
} ILU_C_Sequence_s, *ILU_C_Sequence;

typedef ilu_Exception	ILU_C_ExceptionCode;

ILU_RUNTIME_PUBLIC void
  _ILU_C_InitializeCRuntime(void);
/* called to do any runtime registration of types and/or other things
   needed before operation.  Should be called *after* fiddling with
   the main loop or thread structure.  Typically called automatically
   from the module initialization code.  May safely be called multiple
   times; only the first call is significant. */

typedef struct _ILU_C_ExceptionDescription_s _ILU_C_ExceptionDescription;

typedef struct _ILU_C_ObjectTable_struct * ILU_C_ObjectTable;

/* provide functions from ilu.isl directly */
typedef char * ilu_CString;

typedef void    (*ILU_C_FreeFn) (void *p);
/* <c_role_type(type, role_Exn, ilu_FALSE)> p*/

/*==================================================*/
/*==================================================*/
/*          CORBA-specified interface               */
/*==================================================*/
/*==================================================*/

/* Main invariant holds; L2 otherwise unconstrained */

typedef ilu_integer		CORBA_long;
typedef ilu_shortinteger	CORBA_short;
typedef ilu_longinteger		CORBA_long_long;
typedef ilu_cardinal		CORBA_unsigned_long;
typedef ilu_shortcardinal	CORBA_unsigned_short;
typedef ilu_longcardinal	CORBA_unsigned_long_long;
typedef ilu_shortreal		CORBA_float;
typedef ilu_real		CORBA_double;
typedef ilu_longreal		CORBA_long_double;
typedef ilu_shortcharacter	CORBA_char;
typedef ilu_byte		CORBA_octet;
#if (SIZEOF_WCHAR_T > 0)
typedef wchar_t			CORBA_wchar;
#else
typedef ilu_character		CORBA_wchar;
#endif
typedef unsigned char		CORBA_boolean;
typedef ilu_cardinal		CORBA_enum;
typedef CORBA_char *		CORBA_string;
typedef ILU_C_Object *		CORBA_Object;
typedef struct _ILU_C_FixedPointType_s *ILU_C_FixedPointType;

#if (SIZEOF_WCHAR_T == 4)
#define ILU_C_CORBA_WCHAR_CHARSET	ILU_StringEncoding_Unicode_UCS_4
#else
#define ILU_C_CORBA_WCHAR_CHARSET	ILU_StringEncoding_Unicode_UCS_2
#endif

#define CORBA_OBJECT_NIL	((CORBA_Object)0)

typedef enum { CORBA_NO_EXCEPTION, CORBA_USER_EXCEPTION, CORBA_SYSTEM_EXCEPTION } CORBA_exception_type;

typedef struct _Environment_s {
  CORBA_exception_type	_major;
  ILU_C_ExceptionCode	returnCode;
  void			*ptr;
  ILU_C_FreeFn		freeRoutine;
  /* freeRoutine is called to free subsidiary storage, if any */
} CORBA_Environment;

typedef enum {
  CORBA_COMPLETED_YES, CORBA_COMPLETED_NO, CORBA_COMPLETED_MAYBE
} CORBA_completion_status;

typedef struct {
  unsigned long minor;
  CORBA_completion_status completed;
} CORBA_ex_body;

ILU_RUNTIME_PUBLIC void		CORBA_exception_free( CORBA_Environment * );
ILU_RUNTIME_PUBLIC char *	CORBA_exception_id( CORBA_Environment * );
ILU_RUNTIME_PUBLIC void *	CORBA_exception_value( CORBA_Environment * );

ILU_RUNTIME_PUBLIC CORBA_boolean ILU_C_exception_source( CORBA_Environment *env,
							 const char **exnsrc_filename,
							 int *exnsrc_linenum );
/* If there is a current exception in the given CORBA_Environment and
   source location information is available for that exception, store
   it in (*exnsrc_filename) (retaining string ownership) and
   (*exnsrc_linenum) and return true; otherwise, return false. */

ILU_RUNTIME_PUBLIC const char*	ILU_C_Exception_SrcFile( CORBA_Environment * );
ILU_RUNTIME_PUBLIC int		ILU_C_Exception_SrcLine( CORBA_Environment * );
/* Shorthands for calling ILU_C_exception_source and returning either
   the exception's source filename and line number or "(no exception
   source location available)" and 0. */

ILU_RUNTIME_PUBLIC const char*	ILU_C_SysExnMinorDescr( CORBA_Environment * );
/* Returns global string describing the current system exception's
   minor condition, or "no minor code/decoding available". */

/* Standard System Exceptions: */
ILU_RUNTIME_PUBLIC ILU_C_ExceptionCode	ex_CORBA_UNKNOWN;
ILU_RUNTIME_PUBLIC ILU_C_ExceptionCode	ex_CORBA_BAD_PARAM;
ILU_RUNTIME_PUBLIC ILU_C_ExceptionCode	ex_CORBA_NO_MEMORY;
ILU_RUNTIME_PUBLIC ILU_C_ExceptionCode	ex_CORBA_IMP_LIMIT;
ILU_RUNTIME_PUBLIC ILU_C_ExceptionCode	ex_CORBA_COMM_FAILURE;
ILU_RUNTIME_PUBLIC ILU_C_ExceptionCode	ex_CORBA_INV_OBJREF;
ILU_RUNTIME_PUBLIC ILU_C_ExceptionCode	ex_CORBA_NO_PERMISSION;
ILU_RUNTIME_PUBLIC ILU_C_ExceptionCode	ex_CORBA_INTERNAL;
ILU_RUNTIME_PUBLIC ILU_C_ExceptionCode	ex_CORBA_MARSHAL;
ILU_RUNTIME_PUBLIC ILU_C_ExceptionCode	ex_CORBA_INITIALIZE;
ILU_RUNTIME_PUBLIC ILU_C_ExceptionCode	ex_CORBA_NO_IMPLEMENT;
ILU_RUNTIME_PUBLIC ILU_C_ExceptionCode	ex_CORBA_BAD_TYPECODE;
ILU_RUNTIME_PUBLIC ILU_C_ExceptionCode	ex_CORBA_BAD_OPERATION;
ILU_RUNTIME_PUBLIC ILU_C_ExceptionCode	ex_CORBA_NO_RESOURCES;
ILU_RUNTIME_PUBLIC ILU_C_ExceptionCode	ex_CORBA_NO_RESPONSE;
ILU_RUNTIME_PUBLIC ILU_C_ExceptionCode	ex_CORBA_PERSIST_STORE;
ILU_RUNTIME_PUBLIC ILU_C_ExceptionCode	ex_CORBA_BAD_INV_ORDER;
ILU_RUNTIME_PUBLIC ILU_C_ExceptionCode	ex_CORBA_TRANSIENT;
ILU_RUNTIME_PUBLIC ILU_C_ExceptionCode	ex_CORBA_FREE_MEM;
ILU_RUNTIME_PUBLIC ILU_C_ExceptionCode	ex_CORBA_INV_IDENT;
ILU_RUNTIME_PUBLIC ILU_C_ExceptionCode	ex_CORBA_INV_FLAG;
ILU_RUNTIME_PUBLIC ILU_C_ExceptionCode	ex_CORBA_INTF_REPOS;
ILU_RUNTIME_PUBLIC ILU_C_ExceptionCode	ex_CORBA_BAD_CONTEXT;
ILU_RUNTIME_PUBLIC ILU_C_ExceptionCode	ex_CORBA_OBJ_ADAPTER;
ILU_RUNTIME_PUBLIC ILU_C_ExceptionCode	ex_CORBA_DATA_CONVERSION;
ILU_RUNTIME_PUBLIC ILU_C_ExceptionCode	ex_CORBA_CODESET_INCOMPATIBLE;

/* ILU-specific System Exceptions: */
ILU_RUNTIME_PUBLIC ILU_C_ExceptionCode	ex_ILU_BARRIER;
ILU_RUNTIME_PUBLIC ILU_C_ExceptionCode	ex_ILU_BAD_LOCKS;
ILU_RUNTIME_PUBLIC ILU_C_ExceptionCode	ex_ILU_BROKEN_LOCKS;
ILU_RUNTIME_PUBLIC ILU_C_ExceptionCode	ex_ILU_INTERRUPTED;
ILU_RUNTIME_PUBLIC ILU_C_ExceptionCode	ex_ILU_GSS_SECURITY;

/* Used by CORBA_ORB_init() */
ILU_RUNTIME_PUBLIC ILU_C_ExceptionCode  ex_CORBA_InvalidName;

/* ORB operations */

typedef void		CORBA_ORBStatus;
typedef void *		CORBA_Context;

ILU_RUNTIME_PUBLIC CORBA_Object ILU_C_ORB;	/* should be first parameter to any ORB operation */

ILU_RUNTIME_PUBLIC char *
  CORBA_ORB_object_to_string (CORBA_Object theORB, CORBA_Object, CORBA_Environment *);
ILU_RUNTIME_PUBLIC CORBA_Object
  CORBA_ORB_string_to_object (CORBA_Object theORB, char *, CORBA_Environment * );
ILU_RUNTIME_PUBLIC CORBA_ORBStatus
  CORBA_ORB_get_default_context (CORBA_Object theORB, CORBA_Context *, CORBA_Environment*);
ILU_RUNTIME_PUBLIC CORBA_Object
  CORBA_ORB_resolve_initial_references (CORBA_Object theORB, CORBA_string, CORBA_Environment * );

typedef CORBA_string CORBA_ORBid;

ILU_RUNTIME_PUBLIC CORBA_Object
  CORBA_ORB_init (int *,		/* argc */
		  char **,		/* argv */
		  CORBA_ORBid,		/* orb_id, OPTIONAL */
		  CORBA_Environment *	/* err return */);

typedef struct _CORBA_ORB_ObjectIdList__sequence CORBA_ORB_ObjectIdList;
typedef ilu_CString CORBA_ORB_ObjectId;

/* sequence "CORBA_ORB_ObjectIdList" */
struct _CORBA_ORB_ObjectIdList__sequence {
 unsigned long _maximum;
 unsigned long _length;
 CORBA_ORB_ObjectId *_buffer;
};
ILU_RUNTIME_PUBLIC void
  CORBA_ORB_ObjectIdList_Every (CORBA_ORB_ObjectIdList *h, void (*f)(CORBA_ORB_ObjectId*, void *), void *);
ILU_RUNTIME_PUBLIC void
  CORBA_ORB_ObjectIdList_Append (CORBA_ORB_ObjectIdList *h, CORBA_ORB_ObjectId item);
ILU_RUNTIME_PUBLIC void
  CORBA_ORB_ObjectIdList_Push (CORBA_ORB_ObjectIdList *h, CORBA_ORB_ObjectId item);
ILU_RUNTIME_PUBLIC void
  CORBA_ORB_ObjectIdList_Pop (CORBA_ORB_ObjectIdList *h, CORBA_ORB_ObjectId *item);
ILU_RUNTIME_PUBLIC CORBA_unsigned_long
  CORBA_ORB_ObjectIdList_Length (CORBA_ORB_ObjectIdList *);
ILU_RUNTIME_PUBLIC CORBA_ORB_ObjectId *
  CORBA_ORB_ObjectIdList_Nth (CORBA_ORB_ObjectIdList *, CORBA_unsigned_long);
ILU_RUNTIME_PUBLIC CORBA_ORB_ObjectIdList *
  CORBA_ORB_ObjectIdList_Create (CORBA_unsigned_long /* size */, CORBA_ORB_ObjectId * /* init val */);
ILU_RUNTIME_PUBLIC void
  CORBA_ORB_ObjectIdList_Init (CORBA_ORB_ObjectIdList * /* seq */, CORBA_unsigned_long /* size */, CORBA_ORB_ObjectId * /* init val */);
ILU_RUNTIME_PUBLIC CORBA_ORB_ObjectIdList *
  CORBA_sequence_CORBA_ORB_ObjectIdList_allocbuf (CORBA_unsigned_long /* count */);
ILU_RUNTIME_PUBLIC void
  CORBA_ORB_ObjectIdList__Free (CORBA_ORB_ObjectIdList* /* _val */);
ILU_RUNTIME_PUBLIC CORBA_ORB_ObjectIdList*
  CORBA_ORB_ObjectIdList__alloc (void);

/* "CORBA_ORB_ObjectId" alias for "ilu_CString" */
#define CORBA_ORB_ObjectId_Every ilu_CString_Every
#define CORBA_ORB_ObjectId_Append ilu_CString_Append
#define CORBA_ORB_ObjectId_Push ilu_CString_Push
#define CORBA_ORB_ObjectId_Pop ilu_CString_Pop
#define CORBA_ORB_ObjectId_Init ilu_CString_Init
#define CORBA_ORB_ObjectId_Create ilu_CString_Create
#define CORBA_ORB_ObjectId__alloc ilu_CString__alloc
#define CORBA_sequence_CORBA_ORB_ObjectId_allocbuf CORBA_sequence_string_allocbuf

ILU_RUNTIME_PUBLIC CORBA_ORB_ObjectIdList
  CORBA_ORB_list_initial_services (CORBA_Object /* theORB */,
				   CORBA_Environment * /* the env */);

ILU_RUNTIME_PUBLIC char *		CORBA_string_alloc (CORBA_unsigned_long len);
ILU_RUNTIME_PUBLIC void			CORBA_free (void *);	/* free returned storage with this */

/* pre-defined basic type operations */
ILU_RUNTIME_PUBLIC CORBA_octet *		CORBA_sequence_octet_allocbuf (CORBA_unsigned_long);
ILU_RUNTIME_PUBLIC CORBA_boolean *		CORBA_sequence_boolean_allocbuf (CORBA_unsigned_long);
ILU_RUNTIME_PUBLIC CORBA_char *		CORBA_sequence_char_allocbuf (CORBA_unsigned_long);
ILU_RUNTIME_PUBLIC CORBA_unsigned_short *	CORBA_sequence_unsigned_short_allocbuf (CORBA_unsigned_long);
ILU_RUNTIME_PUBLIC CORBA_unsigned_long *	CORBA_sequence_unsigned_long_allocbuf (CORBA_unsigned_long);
ILU_RUNTIME_PUBLIC CORBA_short *		CORBA_sequence_short_allocbuf (CORBA_unsigned_long);
ILU_RUNTIME_PUBLIC CORBA_long *		CORBA_sequence_long_allocbuf (CORBA_unsigned_long);
ILU_RUNTIME_PUBLIC CORBA_float *		CORBA_sequence_float_allocbuf (CORBA_unsigned_long);
ILU_RUNTIME_PUBLIC CORBA_double *		CORBA_sequence_double_allocbuf (CORBA_unsigned_long);
ILU_RUNTIME_PUBLIC CORBA_string *		CORBA_sequence_string_allocbuf (CORBA_unsigned_long);

/* Object operations */

/* Main Invariant holds; L2 not further constrained */
ILU_RUNTIME_PUBLIC CORBA_boolean 
CORBA_Object_is_nil(CORBA_Object, CORBA_Environment *);

/* Main Invariant holds; L2 not further constrained */
ILU_RUNTIME_PUBLIC CORBA_boolean 
CORBA_Object_is_a(CORBA_Object, CORBA_string, CORBA_Environment *);

/* Main Invariant holds; L2 not further constrained */
ILU_RUNTIME_PUBLIC CORBA_boolean 
CORBA_Object_non_existent(CORBA_Object, CORBA_Environment *);

/* Main Invariant holds; L2 not further constrained */
ILU_RUNTIME_PUBLIC CORBA_boolean 
CORBA_Object_is_equivalent(CORBA_Object, CORBA_Object, CORBA_Environment *);

/* Main Invariant holds; L2 not further constrained */
ILU_RUNTIME_PUBLIC CORBA_unsigned_long
CORBA_Object_hash(CORBA_Object, CORBA_unsigned_long, CORBA_Environment *);

/* Main Invariant holds; L2 not further constrained */
#define CORBA_Object_duplicate(o,e) CORBA_Object_duplicate_full(o,e,__FILE__,__LINE__)

/* Main Invariant holds; L2 not further constrained */
#define CORBA_Object_release(o,e) CORBA_Object_release_full(o,e,__FILE__,__LINE__)

/* Main Invariant holds; L2 not further constrained */
ILU_RUNTIME_PUBLIC CORBA_Object 
CORBA_Object_duplicate_full(CORBA_Object, CORBA_Environment *,
			    char *filename, int lineno);

/* Main Invariant holds; L2 not further constrained */
ILU_RUNTIME_PUBLIC void 
CORBA_Object_release_full(CORBA_Object, CORBA_Environment *,
			  char *filename, int lineno);

/* CORBA TypeCode operations */

typedef const struct _ILU_C_IoFnsRegistration_s *CORBA_TypeCode;

ILU_RUNTIME_PUBLIC ilu_string
  CORBA_TypeCode_id (CORBA_TypeCode, CORBA_Environment *);
/* return the string type ID for the TypeCode's type */

ILU_RUNTIME_PUBLIC ilu_boolean
  CORBA_TypeCode_equal (CORBA_TypeCode, CORBA_TypeCode, CORBA_Environment *);
/* return ilu_TRUE if the two TypeCodes are for the same ISL type */

ILU_RUNTIME_PUBLIC void
  CORBA_TypeCode_free_value (CORBA_TypeCode, void *, CORBA_Environment *);
/* free the malloc'ed C value pointed to by the void *, which must be of the
   type indicated by the TypeCode, and also free any internal pointers it holds */

typedef ilu_refany	(*ILU_C_CRCreateFn) (CORBA_TypeCode,
					     ilu_cardinal /* putative size */,
					     ilu_Error *);
typedef void	(*ILU_C_CRFreeFn) (CORBA_TypeCode,
				   ilu_refany	/* pointer to struct */);
typedef void	(*ILU_C_CRPreOutputFn) (CORBA_TypeCode,
					ilu_refany, 	/* pointer to struct */
					ilu_Error *);
typedef void	(*ILU_C_CRPostOutputFn) (CORBA_TypeCode,
					 ilu_refany, 	/* pointer to struct */
					 ilu_Error *);
typedef void	(*ILU_C_CRPostInputFn) (CORBA_TypeCode,
					ilu_refany, 	/* pointer to struct */
					ilu_Error *);

/* Locking:  L1 < otmu */
ILU_RUNTIME_PUBLIC void
  ILU_C_RegisterCustomRecord (CORBA_TypeCode,
			      ILU_OPTIONAL(ILU_C_CRCreateFn),
			      ILU_OPTIONAL(ILU_C_CRFreeFn),
			      ILU_OPTIONAL(ILU_C_CRPreOutputFn),
			      ILU_OPTIONAL(ILU_C_CRPostOutputFn),
			      ILU_OPTIONAL(ILU_C_CRPostInputFn),
			      CORBA_Environment *);
/* Registers custom record support for the specified type, which must
   be a record type.  Can raise BAD_PARAM and NO_MEMORY and INTERNAL.
   Note that the PreOutputFn may be called more than once, if sizing
   is performed on the protocol.
*/

#define TC_boolean (&_CORBA_boolean__IoFns)
#define TC_CORBA_boolean (&_CORBA_boolean__IoFns)

#define TC_octet (&_CORBA_octet__IoFns)
#define TC_CORBA_octet (&_CORBA_octet__IoFns)

#define TC_short (&_CORBA_short__IoFns)
#define TC_CORBA_short (&_CORBA_short__IoFns)
#define TC_long (&_CORBA_long__IoFns)
#define TC_CORBA_long (&_CORBA_long__IoFns)
#define TC_CORBA_long_long (&_CORBA_long_long__IoFns)

#define TC_ushort (&_CORBA_unsigned_short__IoFns)
#define TC_CORBA_ushort (&_CORBA_unsigned_short__IoFns)
#define TC_CORBA_unsigned_short (&_CORBA_unsigned_short__IoFns)
#define TC_ulong (&_CORBA_unsigned_long__IoFns)
#define TC_CORBA_ulong (&_CORBA_unsigned_long__IoFns)
#define TC_CORBA_unsigned_long (&_CORBA_unsigned_long__IoFns)
#define TC_CORBA_unsigned_long_long (&_CORBA_unsigned_long_long__IoFns)

#define TC_char (&_CORBA_char__IoFns)
#define TC_CORBA_char (&_CORBA_char__IoFns)
#define TC_CORBA_wchar (&_CORBA_wchar__IoFns)

#define TC_float (&_CORBA_float__IoFns)
#define TC_CORBA_float (&_CORBA_float__IoFns)
#define TC_double (&_CORBA_double__IoFns)
#define TC_CORBA_double (&_CORBA_double__IoFns)
#define TC_CORBA_long_double (&_CORBA_long_double__IoFns)

#define TC_string (&_CORBA_string__IoFns)
#define TC_CORBA_string (&_CORBA_string__IoFns)
#define TC_ilu_CString (&_CORBA_string__IoFns)

#define TC_Object (&_ilu_CORBA_Object__IoFns)
#define TC_CORBA_Object (&_ilu_CORBA_Object__IoFns)

#ifdef ADD_VARIANT_SUPPORT

/* CORBA any operations */

#define TC_any (&_CORBA_any__IoFns)
#define TC_CORBA_any (&_CORBA_any__IoFns)
#define TC_ilu_any (&_CORBA_any__IoFns)

typedef struct CORBA_any_s {
  CORBA_TypeCode _type;
  void *_value;		/* <c_role_type(_type,InOut)> */
  ilu_Pickle _pickle;
} CORBA_any;

#define ilu_any CORBA_any
typedef CORBA_any *ILU_C_Pickle;

ILU_RUNTIME_PUBLIC CORBA_any *			/* OPTIONAL, PASS */
  ILU_C_Any_Create (CORBA_TypeCode,	/* RETAIN */
		    void *,		/* RETAIN */
		    CORBA_Environment *);
/* Create a new CORBA_any value, containing the specified TypeCode
   and C value, return it.  Returns NIL on error. */
#define ILU_C_Pickle_Create ILU_C_Any_Create

ILU_RUNTIME_PUBLIC CORBA_any *			/* OPTIONAL, PASS */
  ILU_C_Any_Init (CORBA_any *,		/* RETAIN */
		  CORBA_TypeCode,	/* RETAIN */
		  void *,		/* RETAIN */
		  CORBA_Environment *);
/* Initialize the new "any".  This any value must not have been used
   before.  See ILU_C_Any_ResetValue for a function which works on
   previously used "any" values.  Returns the Any value, or NIL on error. */
#define ILU_C_Pickle_Init ILU_C_Any_Init

ILU_RUNTIME_PUBLIC CORBA_any *		/* OPTIONAL, PASS */
  ILU_C_Any_ResetValue (CORBA_any *,	/* RETAIN */
			CORBA_TypeCode,	/* RETAIN */
			void *,		/* RETAIN */
			CORBA_Environment *);
/* Frees any previous value in the "any", and sets it to the specified
   value.  This should only be used on Any values which have previously
   been initialized.  Returns the Any value, or NIL on error. */
#define ILU_C_Pickle_ResetValue ILU_C_Any_ResetValue

ILU_RUNTIME_PUBLIC CORBA_TypeCode	/* OPTIONAL, GLOBAL */
  ILU_C_Any_TypeCode (CORBA_any *,	/* RETAIN */
		      CORBA_Environment *);
/* Given a valid CORBA_any, returns the typecode of the any, or
   NIL if the any does not contain a typecode known in the C runtime. */
#define ILU_C_Pickle_TypeCode ILU_C_Any_TypeCode

ILU_RUNTIME_PUBLIC ilu_string		/* OPTIONAL, PASS */
  ILU_C_Any_TypeID (CORBA_any *,	/* RETAIN */
		    CORBA_Environment *);
/* Given a valid CORBA_any, returns the CORBA repository ID of the
   type of the any's value.  May return NIL if an error occurs. */
#define ILU_C_Pickle_TypeID ILU_C_Any_TypeID

ILU_RUNTIME_PUBLIC void *		/* OPTIONAL, PASS */
  ILU_C_Any_Value (CORBA_any *,		/* RETAIN */
		   CORBA_Environment *);
/* Given a valid CORBA_any, returns a copy of the value contained in
   the any, or NIL if the type of the any's value is not known in
   the C runtime. */
#define ILU_C_Pickle_Value ILU_C_Any_Value

ILU_RUNTIME_PUBLIC CORBA_any *		/* OPTIONAL, PASS */
  ILU_C_Any_Duplicate (CORBA_any *,	/* RETAIN */
		       CORBA_Environment *);
/* Returns a newly malloc'ed CORBA_any value containing a copy of the
   type and value contained in the input parameter.  Works even if the
   typecode of the input param is not known in the C runtime.  Returns
   NIL if an error occurs. */
#define ILU_C_Pickle_Duplicate ILU_C_Any_Duplicate

ILU_RUNTIME_PUBLIC void
  ILU_C_Any_FreeStorage (CORBA_any *,	/* RETAIN */
			 CORBA_Environment *);
/* Given a valid CORBA_any, frees any storage held by that Any
   without releasing the storage of the Any itself.  Safe to
   call on stack-allocated Any values.  The storage of the Any
   itself may, for heap-allocated Any values, be freed by a call
   on ilu_free. */
#define ILU_C_Pickle_FreeStorage ILU_C_Any_FreeStorage

#endif


#define ILU_C_OBJECT		CORBA_Object
#define ILU_C_ENVIRONMENT	CORBA_Environment
#define ILU_C_SUCCESSFUL(s)	((s)->_major == CORBA_NO_EXCEPTION)
#define ILU_C_SET_SUCCESSFUL(s)	((s)->returnCode = ILU_NIL, (s)->_major = CORBA_NO_EXCEPTION)
#define ILU_C_NO_EXCEPTION	CORBA_NO_EXCEPTION
#define ILU_C_USER_EXCEPTION	CORBA_USER_EXCEPTION
#define ILU_C_SYSTEM_EXCEPTION	CORBA_SYSTEM_EXCEPTION
#define ILU_C_EXCEPTION_ID(e)	CORBA_exception_id(e)
#define ILU_C_EXCEPTION_FREE(e)	CORBA_exception_free(e)
#define ILU_C_EXCEPTION_VALUE(e) CORBA_exception_value(e)
#define ILU_C_SYSEXN_BODY	CORBA_ex_body
#define ILU_C_COMPLETION(st)	(CORBA_COMPLETED_##st)
#define ILU_C_COMPLETIONSTATUS	CORBA_completion_status

#define ILU_C_STDEX(ename) (ex_CORBA_##ename)
#define ILU_C_ILUEX(ename) (ex_ILU_##ename)

#define ILU_C_RAISE_SYSTEM(p,ename,mcode,ccode) \
ILU_C_RAISE_SYS_EXPR(p,ILU_C_STDEX(ename),mcode,ILU_C_COMPLETION(ccode))

#define ILU_C_RAISE_ILU_SYSTEM(p,ename,mcode,ccode) \
ILU_C_RAISE_SYS_EXPR(p,ILU_C_ILUEX(ename),mcode,ILU_C_COMPLETION(ccode))

#define ILU_C_RAISE_SYS_EXPR(p,exn,mcode,cmpln) \
	ILU_C_RaiseSysExn(p,exn,mcode,cmpln,__FILE__,__LINE__)

ILU_RUNTIME_PUBLIC
ILU_C_ENVIRONMENT *ILU_C_RaiseSysExn(ILU_C_ENVIRONMENT *env,
				     ILU_C_ExceptionCode exn,
				     ilu_cardinal minor,
				     CORBA_completion_status cstat,
				     const char *src_filename,
				     int src_linenum);

/* These definitions are for the object type "ilu.CORBA-Object", which
   provides a base for IDL-defined object types */

typedef ILU_C_OBJECT ilu_CORBA_Object;
#define ilu_CORBA_Object__MSType _ilu_CORBA_Object__ILUType
ILU_RUNTIME_PUBLIC ilu_CORBA_Object
  ilu_CORBA_Object__CreateFromSBH (char * /* sbh */, ILU_C_ENVIRONMENT * /* env */);
ILU_RUNTIME_PUBLIC ilu_CORBA_Object
  ilu_CORBA_Object__CreateTrue (ilu_string /* instance-handle */, ILU_C_Server /* server */, void * /* user data */);
/* only for use within an object table's `ot_object_of_ih' method */
ILU_RUNTIME_PUBLIC ilu_CORBA_Object
  ilu_CORBA_Object__OTCreateTrue (ilu_string /* instance-handle */, ILU_C_Server /* server */, void * /* user data */);
ILU_RUNTIME_PUBLIC void ilu_CORBA_Object__Free (ilu_CORBA_Object*);

ILU_RUNTIME_PUBLIC ilu_Class _ilu_CORBA_Object__ILUType;
struct _ilu_CORBA_Object__MethodBlock_s {
 ilu_Class c;
};
ILU_RUNTIME_PUBLIC struct _ilu_CORBA_Object__MethodBlock_s _ilu_CORBA_Object__SurrogateMethodBlock;

ILU_RUNTIME_PUBLIC struct _ILU_C_IoFnsRegistration_s _ilu_CORBA_Object__IoFns;
#define TC_ilu_CORBA_Object (& _ilu_CORBA_Object__IoFns )

/*==================================================*/
/*==================================================*/
/*                   ILU-specific stuff             */
/*               for application developers         */
/*==================================================*/
/*==================================================*/

/*==== Types =======================================*/

/*L1_sup < otmu*/
/*L2 unconstrained*/
ILU_RUNTIME_PUBLIC ilu_Class	ILU_C_FindILUClassByTypeName ( ilu_string classname );

/*L1_sup < otmu*/
/*L2 unconstrained*/
ILU_RUNTIME_PUBLIC ilu_Class	ILU_C_FindILUClassByTypeID ( ilu_string class_unique_id );

/* Locking unconstrained */
ILU_RUNTIME_PUBLIC ILU_GLOBAL(ILU_OPTIONAL(ilu_string))	ILU_C_ClassName ( ILU_RETAIN(ILU_C_Object *) );

/* Locking unconstrained */
ILU_RUNTIME_PUBLIC ILU_GLOBAL(ILU_OPTIONAL(ilu_string))	ILU_C_ClassID ( ILU_RETAIN(ILU_C_Object *) );

/* locking Unrestricted */
ILU_RUNTIME_PUBLIC ilu_Class	ILU_C_ClassRecordOfInstance (ILU_C_Object *o);	/* returns typecode for object */

/* Unrestricted */
ILU_RUNTIME_PUBLIC          ILU_PASS(char *) ILU_C_Strdup(ILU_RETAIN(char *str));

/* locking Unrestricted */
ILU_RUNTIME_PUBLIC ILU_C_Class
  ILU_C_RegisterCustomSurrogateType (ilu_Class,		/* the kernel type */
				     ILU_C_Class,	/* the C class */
				     ILU_C_ENVIRONMENT *);
/* Allows an application to register a "custom surrogate" type to be
 * used in this address space when creating surrogate instances of the
 * specified kernel type.  The class returned is the previously registered
 * C class for that kernel type.
 */

/*==== Object manipulation =========================*/

/*
 * In all of these, the Main Invariant should hold, and L2 is
 * otherwise unrestricted
 */

ILU_RUNTIME_PUBLIC ilu_string	ILU_C_SBHOfObject ( ILU_C_Object * obj );
/*
 * given an object, returns a string form which is its name and
 * contact information
 */

/* L1, L2 unconstrained */
ILU_RUNTIME_PUBLIC ilu_string
  ILU_C_FormSBH (ilu_string,		/* server_id, plain, RETAIN */
		 ilu_string,		/* instance_handle, plain, RETAIN */
		 ilu_Class,		/* most specific type */
		 ilu_ProtocolInfo,	/* pinfo, RETAIN */
		 ilu_TransportInfo,	/* tinfo, RETAIN */
		 ILU_C_ENVIRONMENT *);	/* indicate errors */
/*
 * given the necessary info about an object, returns a well-formed SBH.
 */

ILU_RUNTIME_PUBLIC ilu_boolean ILU_C_IDOfObject (ILU_C_Object *obj,
						 char ** /* OUT, PASS, server id */,
						 char ** /* OUT, PASS, instance handle */);
/*
 * given an object, returns the two "server ID" and "instance handle" strings
 * which make up its UUID.  The two strings are copies, and should be freed
 * when the user is done with them.
 */

#ifdef IIOP_PROTOCOL

ILU_RUNTIME_PUBLIC ilu_string	ILU_C_IOROfObject ( ILU_C_Object * obj );
/*
 * given an object, returns a string form which is its name and
 * contact information, as specified by the CORBA IIOP spec.
 */

#endif

ILU_RUNTIME_PUBLIC ILU_C_Object   *
  ILU_C_SBHToObject(char *sbh,
		    ilu_Class static_type,
		    ILU_C_ENVIRONMENT * env);
/*
 * Takes an object reference and returns the object.  static_type is
 * a type the caller knows the object to have.
 */

/*Main Invariant holds, L2 not further constrained*/
ILU_RUNTIME_PUBLIC ILU_C_Server 
ILU_C_ServerOfObject(ILU_C_OBJECT obj,
		     ILU_C_ENVIRONMENT * env);
/* Calling this increments ref count of result. */

/* main invariant holds */
ILU_RUNTIME_PUBLIC ilu_integer	/* old timeout */
ILU_C_SetObjectGCTimeout (ILU_C_Object *,
			  ilu_integer /* new timeout */,
			  ILU_C_ENVIRONMENT *);
/* Set the GC timeout field of the object to the value specified in the
 * second parameter.  Returns the old timeout value on success.
 */

/* L1 < gcmu */
ILU_RUNTIME_PUBLIC ilu_FineTime	/* old ping period */
ILU_C_SetDefaultGCPingPeriod (ilu_FineTime /* new period */,
			      ILU_C_ENVIRONMENT *);
/* Set the GC ping period used in the ILU distributed garbage collection
 * algorithm to the period specified in the first parameter.
 * Returns the old ping period on success.
 */

ILU_RUNTIME_PUBLIC void
  ILU_C_PingObject(ILU_C_OBJECT obj, ILU_C_ENVIRONMENT * env);
/* May raise a standard exception. */

/* Main Invariant holds; L2 not further constrained */
ILU_RUNTIME_PUBLIC void 
ILU_C_ShutdownObject(ILU_C_OBJECT obj,
		     ILU_C_ENVIRONMENT * env);
/*
 * Make this object useless for ILU stuff --- you can still set/get
 * instance data (as long as (obj) is not completely released), but
 * attempts to [un]marshall or do other ILU-specific stuff will
 * fail.  In implementation terms, this procedure ensures this C
 * object has no associated kernel object.  This procedure also
 * include the effects of 1 call on CORBA_Object_release.
 */

typedef struct {
  ilu_cardinal    nRefd;	/* num. obj.s not fully released */
  ilu_cardinal    nVI;		/* num. obj.s in which kernel is
				 * very interested */
  ilu_cardinal    nBoth;	/* num. obj.s that meet both
				 * criteria */
}               ILU_C_ObjCounts;
/*
 * This struct contains counts of the number of objects in each
 * category.
 */

/* Main Invariant holds; L2 not further constrained */
ILU_RUNTIME_PUBLIC void
ILU_C_CloseServer(ILU_C_Server s, ilu_boolean andObjects,
		  ilu_cardinal * nCObjs, ILU_C_ObjCounts * counts,
		  ilu_cardinal * nConns, ILU_C_ENVIRONMENT * env);
/*
 * Shuts down the given server, and optionally each object currently
 * reified in the server. Stores some approximate statistics through
 * three optional arguments.  Stores through (nCObjs), if it's not
 * null, the number of ILU_C_OBJECTs that were encountered during
 * this operation. Stores through (counts), if it's not NIL, counts
 * of various categories of objects that remain.  Stores through
 * (nConns), if it's not null, the number of connections (of that
 * server) whose I/O mutex was held at the start.  These sums are
 * not necessarily accurate upon return because the server's mutex
 * may be released and re-acquired for every object, so things could
 * (depending on the application) change during the process.
 */

ILU_RUNTIME_PUBLIC void
ILU_C_ShutdownObjectAndCloseServer(ILU_C_OBJECT obj,
				  ILU_C_ENVIRONMENT * env);
/*
 * CORBA_Object_release then ILU_C_CloseServer(ilu_FALSE, s, NIL, NIL,
 * NIL, env).
 */

ILU_RUNTIME_PUBLIC          ilu_boolean
  ILU_C_ValidateOrCloseObjSvr(ILU_C_OBJECT obj,
				ILU_C_ENVIRONMENT * env);
/*
 * Calls ILU_C_PingObject, and then
 * ILU_C_ShutdownObjectAndCloseServer if the ping suggests obj's
 * server is unreachable.  Standard exceptions suggesting the server
 * is unreachable don't go into *env; internal failures do.  Returns
 * true if ping succeeds, false if ReleaseObjectAndCloseServer
 * called.
 */

/* Main invariant holds */
ILU_RUNTIME_PUBLIC ILU_C_Object *
  ILU_C_CreateSurrogateObject (ilu_Class objtype,
			       ILU_RETAIN(ilu_string) ih,
			       ILU_C_Server server,
			       ILU_C_ENVIRONMENT *env);
/* Create and return an instance of the specified class,
   with the specified ih, on the specified server */

/* Main invariant holds */
ILU_RUNTIME_PUBLIC ILU_C_Object *
  ILU_C_FindObject (char * /* sid */, char * /* ih */);
/* return the C object for the specified object ID, or
 * ILU_NIL if no such object */

/* Main invariant holds */
ILU_RUNTIME_PUBLIC char *
  ILU_C_IDOfServer (ILU_C_Server);
/* Returns the server ID of the specified server */


/*==== Configuring Calls =========================*/
/* Main Invariant holds; L2 not further restricted */

typedef struct _ilu_Serializer_s *ILU_C_Serializer;
/*
 * An ILU_C_Serializer represents an instance of the call order
 * preservation (A.K.A. serialization) guarantee.  An instance is
 * with respect to a particular server and set of calls.  The
 * server's default port (the first one listed in its contact info)
 * must use a non-concurrent protocol; if not, the client's calls
 * will raise the system exception ex_INV_OBJREF with a minor code
 * of ilu_iom_conc_serial (if no other error is noticed first).  The
 * guarantee is that the server application code receives calls in
 * the same order as the client application code makes them, except
 * that calls that return after a barrier call may have started
 * service before calls that return before the same barrier call.  A
 * barrier call is one that raises the ILU-specific system exception
 * ex_ILU_BARRIER.  Remember that ASYNCHRONOUS calls do return, they
 * just do so particularly quickly.  Two calls are considered to
 * have been issued concurrently if each call is initiated before
 * the other returns.  In a multi-threaded runtime, they client may
 * issue concurrent calls with the same ILU_C_Serializer, and the
 * ILU runtime will put them in some serial order.  Note that for
 * two concurrently issued calls, either: (a) the one put first is
 * ASYNCHRONOUS, (b) they both are in the same ILU_C_Pipeline, or
 * (c) the one put second is delayed until the one put first
 * returns.  In a single-threaded runtime, the client may issue two
 * calls "concurrently" (taking advantage of a nested main loop),
 * but both will execute successfully only if the client is lucky;
 * otherwise, one will raise the system exception BAD_PARAM with
 * minor code ilu_bpm_serialConcurrent.  Furthermore, when
 * single-threaded, issuing concurrent calls with the same
 * ILU_C_Serializer and different ILU_C_Pipelines will also cause
 * some to raise BAD_PARAM/serialConcurrent.
 */

ILU_RUNTIME_PUBLIC ILU_C_Serializer
  ILU_C_CreateSerializationContext (ILU_C_Server,
				    ILU_C_ENVIRONMENT *);
/* Create a new instance of the serialization guarantee. */

ILU_RUNTIME_PUBLIC ilu_boolean
  ILU_C_SetSerializationContext (ILU_C_Serializer val);
/*
 * Associate the given ILU_C_Serializer with calls made in the
 * future in the same thread, until overridden by a later call on
 * ILU_C_SetSerializationContext.
 */

ILU_RUNTIME_PUBLIC ILU_C_Serializer
  ILU_C_GetSerializationContext (void);
/*
 * Return the ILU_C_Serializer, if any, currently associated with
 * calls in this thread.  The ILU_C_Serializer is not transmitted
 * over the wire, so a server should not expect to be able to call
 * this procedure and get a useful result.
 */

ILU_RUNTIME_PUBLIC ilu_boolean
ILU_C_ReleaseSerializer(ILU_C_Serializer val,
			ILU_C_ENVIRONMENT * env);
/*
 * Client calls this after it's done using the given
 * ILU_C_Serializer.  Henceforth, (val) is not usable.
 */


typedef struct _ilu_Pipeline_s *ILU_C_Pipeline;
/*
 * A client uses an ILU_C_Pipeline to let the ILU runtime know it
 * can safely pipeline certain calls down a serial (i.e.,
 * non-concurrent) connection.  The ILU_C_Pipeline is associated
 * with each of the calls that can be pipelined.  A given serial
 * connection can have multiple calls outstanding only if they are
 * all associated with the same (non-NIL) ILU_C_Pipeline (remember
 * that absent pipelining, ILU will do concurrent calls over a
 * serial protocol by opening multiple connections).  Multiple
 * connections, even of different servers, can have outstanding
 * calls associated with the same ILU_C_Pipeline.
 */

ILU_RUNTIME_PUBLIC ILU_C_Pipeline
  ILU_C_CreatePipeline (ILU_C_ENVIRONMENT *env);
/* Create a new ILU_C_Pipeline. */

ILU_RUNTIME_PUBLIC ilu_boolean
  ILU_C_SetPipelineContext (ILU_C_Pipeline);
/*
 * Associate the given ILU_C_Pipeline with calls made in the
 * future in the same thread, until overridden by a later call on
 * ILU_C_SetPipelineContext.
 */

ILU_RUNTIME_PUBLIC ILU_C_Pipeline
  ILU_C_GetPipelineContext (void);
/*
 * Return the ILU_C_Pipeline, if any, currently associated with
 * calls in this thread.  The ILU_C_Pipeline is not transmitted
 * over the wire, so a server should not expect to be able to call
 * this procedure and get a useful result.
 */

ILU_RUNTIME_PUBLIC ilu_boolean
ILU_C_ReleasePipeline(ILU_C_Pipeline val,
		      ILU_C_ENVIRONMENT * env);
/*
 * Client calls this after it's done using the given ILU_C_Pipeline.
 * Henceforth, (val) is not usable.
 */


typedef struct ilu_Batcher_struct *ILU_C_Batcher;
/*
 * An ILU_C_Batcher represents a batching scope.  Calls made within
 * a batching scope are batched by the ILU runtime.  That is,
 * delivery of the call messages might not be initiated immediately.
 * The delivery is initiated by the time either (a)
 * ILU_C_PushBatcher is invoked on the batcher or (b) an amount of
 * time equal to the batcher's timeout has passed since the call was
 * initiated.  Any given batcher may use one or the other or both
 * criteria, as specified at its creation.
 */

ILU_RUNTIME_PUBLIC ILU_C_Batcher
ILU_C_CreateBatcher(ilu_FineTime timeout,
		    ilu_boolean pushable,
		    ILU_C_ENVIRONMENT * env);
/*
 * Create a new batcher.  (timeout==0) means the timeout criterion
 * doesn't apply; (!pushable) means ILU_C_PushBatcher may not be
 * invoked on this batcher.  At least one of (timeout > 0) and
 * (pushable) should be true.
 */

ILU_RUNTIME_PUBLIC ilu_boolean
  ILU_C_SetBatcherContext (ILU_C_Batcher);
/*
 * Associate the given ILU_C_Batcher with calls made in the
 * future in the same thread, until overridden by a later call on
 * ILU_C_SetBatcherContext.
 */

ILU_RUNTIME_PUBLIC ILU_C_Batcher
  ILU_C_GetBatcherContext (void);
/*
 * Return the ILU_C_Batcher, if any, currently associated with
 * calls in this thread.  The ILU_C_Batcher is not transmitted
 * over the wire, so a server should not expect to be able to call
 * this procedure and get a useful result.
 */

ILU_RUNTIME_PUBLIC ilu_boolean 
ILU_C_PushBatcher(ILU_C_Batcher b,
		  ILU_C_ENVIRONMENT * env);
/* See comment on definition of ILU_C_Batcher. */

ILU_RUNTIME_PUBLIC ilu_boolean
ILU_C_ReleaseBatcher(ILU_C_Batcher val,
		      ILU_C_ENVIRONMENT * env);
/*
 * Client calls this after it's done using the given ILU_C_Batcher.
 * Henceforth, (val) is not usable.
 */


ILU_RUNTIME_PUBLIC ilu_boolean
  ILU_C_SetPassportContext (ilu_Passport);
/*
 * Associate the given ilu_Passport with future calls made in the
 * same thread, until overridden by a later call on
 * ILU_C_SetPassportContext.
 */

ILU_RUNTIME_PUBLIC ilu_Passport
  ILU_C_GetPassportContext (void);
/*
 * Return the ilu_Passport, if any, currently associated with
 * this thread.  This passport is used in any calls made by this
 * thread.
 */

ILU_RUNTIME_PUBLIC ilu_Passport
  ILU_C_CallerIdentity (void);
/*
 * Return the ilu_Passport, if any, currently associated with calls
 * in this thread.  The ilu_Passport is transmitted over the wire,
 * and this procedure is how a true procedure discovers it.  This
 * procedure will return ILU_NIL only if the call has been made
 * directly (caller and callee in same thread of same process), and
 * the caller has not set the Passport context of the thread.
 */

/*==== Interrupting Calls =========================*/
/* Main Invariant holds; L2 not further restricted */

/* When the runtime is single-threaded, use the following facilities
to interrupt calls.  When the runtime is multi-threaded, interrupt a call by doing the normal thing to interrupt a thread's execution. */

typedef struct ILU_C_InterruptHandle_s *ILU_C_InterruptHandle;

ILU_RUNTIME_PUBLIC ILU_C_InterruptHandle ILU_C_NewInterruptHandle(void);
/* An interruption scope.  Returns NIL if can't allocate. */

ILU_RUNTIME_PUBLIC void 
ILU_C_SetObjectInterruptHandle(ILU_C_Object * obj,
			       ILU_C_InterruptHandle h);
/*
 * Associate h with obj henceforth; overrides the previous
 * association for obj, if any.  Pass h=NIL to remove previous
 * association, if any.  No-op if obj is a true object.
 */

ILU_RUNTIME_PUBLIC void ILU_C_InterruptHandleCalls(ILU_C_InterruptHandle h);
/* Interrupt current RPCs on objects currently associated with h. */


/*==== Simple Binding =========================*/

/*
 * In all of these, the Main Invariant should hold, and L2 is
 * otherwise unrestricted
 */

ILU_RUNTIME_PUBLIC ILU_OPTIONAL(ILU_PASS(char *))
     ILU_C_PublishObject ( ILU_C_Object *obj );
/* Publishes the OID of the object in a domain-wide registry */

ILU_RUNTIME_PUBLIC ilu_boolean	ILU_C_WithdrawObject ( ILU_C_Object *obj,
					      ILU_PASS(char *) proof);
/*
 * Removes the OID of the object from the domain-wide registry.
 * "proof" is the string returned from the publish call.
 */

ILU_RUNTIME_PUBLIC ILU_OPTIONAL(ILU_GLOBAL(ILU_C_Object *))
     ILU_C_LookupObject(ILU_RETAIN(char *) sid, ILU_RETAIN(char *) ih,
			ilu_Class static_type);
/*
 * Using the local registry, find and return the object specified by
 * the given Server ID and server-relative Instance Handle.
 * "static_type" is one you know the actual object must have; it may
 * also have more refined types.  For an already-reified surrogate
 * this procedure will reconsider what contact info to use for
 * reaching the server.
 */

ILU_RUNTIME_PUBLIC ILU_OPTIONAL(ILU_GLOBAL(ILU_C_Object *))
     ILU_C_ReLookupObject(ILU_RETAIN(char *) sid, ILU_RETAIN(char *) ih,
			  ilu_Class static_type, int *change);
/*
 * Like ILU_C_LookupObject, but also sets *change to indicate
 * whether a change was made that isn't evident in the result.
 */


/*==== Threading =====================================*/

/*
 * In all of these, the Main Invariant should hold, and L2 is
 * otherwise unrestricted
 */

#ifdef ILU_OS_THREADED

/*Main invariant holds*/
#define ILU_C_USE_OS_THREADS  \
     ILU_C_EnableThreads(ilu_InitializeOSThreading, ilu_OSForkNewThread, ilu_OSThreads_GetPerThreadDataTech)
/*
 * If you have configured ILU with support for OS-supplied threads,
 * this will tell the C runtime to use that support.  If
 * ILU_C_USE_OS_THREADS is used, you do not need to call
 * ILU_C_SetFork(), described below.  This routine should be called
 * before any other ILU calls are made, or any interface
 * initialization calls are made.  It returns ilu_FALSE (and prints out
 * an error message) if anything goes wrong in setting up the
 * threads support.
 */

#endif

typedef         ilu_boolean
ILU_C_ThreadSetupProc(ILU_ERRS((bad_param, no_memory,
				no_resources,
				internal)) * err);
/*
 * A procedure that calls the ilu_SetWaitTech, ilu_SetMainLoop, and
 * ilu_SetLockTech procedures of ILU's runtime kernel.
 */

typedef void    (*ILU_C_WorkProc) (void *arg);

typedef void    (*ILU_C_ForkProc) (ILU_C_WorkProc work, void *arg);
/* A thread-forking procedure that has no way to report errors. */

typedef         ilu_boolean
(*ILU_C_ErrableForkProc)(ILU_C_WorkProc proc, void *arg,
			 ILU_ERRS((no_memory, no_resources,
				   internal)) * err);
/* A thread-forking procedure that may raise an error. */

typedef		ilu_boolean
(*ILU_C_PerThreadDataSetupProc)(void (*) (void *),/* destructor, IN, GLOBAL, OPTIONAL */
				void *(**)(void),/* OUT, GLOBAL */
				void (**)(const void *, ILU_ERRS((no_memory, internal)) *),/* OUT, GLOBAL */
				ILU_ERRS((no_memory, internal)) *);
/* A routine which returns two functions, one to set a per-thread
   value, the other to retrieve that value. */

ILU_RUNTIME_PUBLIC ilu_boolean ILU_C_EnableThreads
  (ILU_C_ThreadSetupProc,
   ILU_C_ErrableForkProc,
   ILU_C_PerThreadDataSetupProc);
/*
 * A convenient way to call s, and if successful, then
 * ILU_C_SetFork(f).
 */

ILU_RUNTIME_PUBLIC ilu_boolean ILU_C_SetFork(ILU_C_ForkProc fork);
/*
 * A multi-threaded app or runtime extension calls this, before
 * ILU_C_Run or creating any servers.  Returns true if called in
 * time, false otherwise (this error is not reliably detected; you
 * really ought to get it right).
 */

/*==== Servers =====================================*/

/*
 * In all of these, the Main Invariant should hold, and L2 is
 * otherwise unrestricted
 */

ILU_RUNTIME_PUBLIC void ILU_C_Stoppable_Run(int *stop);

/*
 * A single-threaded app calls this to animate a server or other
 * program.  Invokes the event handling loop.  Never returns.  Allows
 * a value to be passed to the loop.
 */

ILU_RUNTIME_PUBLIC void     ILU_C_Run(void);
/*
 * A single-threaded app calls this to animate a server or other
 * program.  Invokes the event handling loop.  Never returns.
 */

ILU_RUNTIME_PUBLIC void     ILU_C_StoppableRun(int *stop);
ILU_RUNTIME_PUBLIC void     ILU_C_StopRun(int *stop);

#define ILU_C_FINISH_MAIN_THREAD(val)  \
    if (1) {ILU_C_Run(); return (val);} else (0)
/*
 * When followed by a semicolon, expands to a statement that will
 * return "val" from the main routine when it is safe to do so,
 * depending on the thread semantics.
 */

/**L1 >= {table's server};
   L1 >= {gcmu} if result is true and collectible;
   L2 unconstrained*/
typedef
CORBA_Object ILU_C_ObjectTable_ApplyProc(ilu_string /* ih */ ,
				     ilu_private /* user_data */ );

/* L1 >= {table's server}; L2 unconstrained */
typedef void    ILU_C_ObjectTable_FreeProc(ilu_private /* user_data */ );

ILU_RUNTIME_PUBLIC ILU_C_ObjectTable
ILU_C_CreateObjectTable(ILU_C_ObjectTable_ApplyProc * object_of_ih,
			ILU_C_ObjectTable_FreeProc * free_ot,
			ilu_private user_data);
/* Creates and returns an ObjectTable structure suitable for use with
 * ILU_C_InitializeServer.
 */

/* Main invariant holds */
ILU_RUNTIME_PUBLIC          ILU_C_Server
ILU_C_InitializeServer(ILU_OPTIONAL(ILU_RETAIN(char *)) serverID,
		       ILU_OPTIONAL(ILU_GLOBAL(ILU_C_ObjectTable)) obj_tab,
		       ILU_OPTIONAL(ILU_RETAIN(char *)) protocol,
		       ILU_OPTIONAL(ILU_RETAIN(ilu_TransportInfo)) transport,
		       ILU_OPTIONAL(ILU_RETAIN(ilu_Passport)) pp,
		       ilu_boolean createPortAnyway);

/* Main invariant holds */
ILU_RUNTIME_PUBLIC          ILU_C_Server
ILU_C_FullInitializeServer(ILU_OPTIONAL(ILU_RETAIN(char *)) serverID,
			   ILU_OPTIONAL(ILU_GLOBAL(ILU_C_ObjectTable)) obj_tab,
			   ILU_OPTIONAL(ILU_RETAIN(char *)) protocol,
			   ILU_OPTIONAL(ILU_RETAIN(ilu_TransportInfo)) transport,
			   ILU_OPTIONAL(ILU_RETAIN(ilu_Passport)) pp,
			   ilu_boolean createPortAnyway,
			   ilu_boolean port_public);

/* Main invariant holds */
ILU_RUNTIME_PUBLIC ilu_boolean
ILU_C_AddPort(ILU_C_Server server,
	      ILU_OPTIONAL(ILU_RETAIN(char *)) protocol,
	      ILU_OPTIONAL(ILU_RETAIN(ilu_TransportInfo)) transport,
	      ILU_OPTIONAL(ILU_RETAIN(ilu_Passport)) pp,
	      ilu_boolean makeDefault,
	      ILU_C_ENVIRONMENT * env);

/* Main invariant holds */
ILU_RUNTIME_PUBLIC ilu_boolean
ILU_C_FullAddPort(ILU_C_Server server,
		  ILU_OPTIONAL(ILU_RETAIN(char *)) protocol,
		  ILU_OPTIONAL(ILU_RETAIN(ilu_TransportInfo)) transport,
		  ILU_OPTIONAL(ILU_RETAIN(ilu_Passport)) pp,
		  ilu_boolean makeDefault,
		  ilu_boolean be_public,
		  ILU_C_ENVIRONMENT * env);

/* Main Invariant holds */
ILU_RUNTIME_PUBLIC ilu_boolean
ILU_C_AddCInfo(ILU_C_Server server,
	       ILU_RETAIN(ilu_CString) pinfo,
	       ILU_RETAIN(ilu_TransportInfo) tinfo,
	       ILU_C_ENVIRONMENT *env);
/* Add contact info for an external replica of the given server. */

/*L1 >= {ilu_Server}; L2 unconstrained*/
typedef ilu_boolean
  (*ILU_C_ServerRelocateProc) (ILU_C_Server,
			       ilu_private,
			       ilu_ProtocolInfo *,
			       ilu_TransportInfo *);
/* return ilu_TRUE if new pinfo and tinfo have been stored into those
   out parameters (with ownership passed to caller); otherwise return
   ilu_FALSE.  No error indication.  */

/* Main Invariant holds */
ILU_RUNTIME_PUBLIC ilu_private
  ILU_C_SetServerRelocationProc (ILU_C_Server server,
				 ILU_C_ServerRelocateProc proc,
				 ilu_private rock,
				 ILU_C_ENVIRONMENT *env);

/* Main Invariant holds */
ILU_RUNTIME_PUBLIC ilu_boolean
ILU_C_Server_CInfo(ILU_C_Server server,
		   ilu_boolean want_public,
		   char **pinfo,
		   ilu_TransportInfo *tinfo,
		   ILU_C_ENVIRONMENT *env);
/* Returns true iff successfully copied server's first public or
   private (as requested) cinfo into (*pinfo) and (*tinfo). */

/* Main Invariant holds */
ILU_RUNTIME_PUBLIC ILU_C_Server
ILU_C_Server_duplicate(ILU_C_Server,
		       ILU_C_ENVIRONMENT *);

/* Main Invariant holds */
ILU_RUNTIME_PUBLIC void
ILU_C_Server_release(ILU_C_Server,
		     ILU_C_ENVIRONMENT *);


/*==== Passports and Identities =====================================*/

#ifdef SECURE_TRANSPORT

ILU_RUNTIME_PUBLIC ilu_IdentityInfo
  ILU_C_AcquireGSSIdentity (gss_cred_id_t, ILU_C_ENVIRONMENT *);

ILU_RUNTIME_PUBLIC ilu_boolean
  ILU_C_DecodeGSSIdentity (ilu_IdentityInfo,	/* input; retain; info to decode */
			   gss_name_t *,		/* output; name in identity */
			   ilu_FineTime *,	/* output; good-till; seconds past Unix epoch */
			   gss_OID,		/* input; actual mechanism desired; optional */
			   ilu_boolean *,		/* if ilu_TRUE, local; otherwise remote */
			   ilu_cardinal *,	/* connection flags, as in gss_inquire_context */
			   ILU_C_ENVIRONMENT *);

ILU_RUNTIME_PUBLIC gss_cred_id_t
  ILU_C_AcquireGSSCredForName (char *,		/* name */
			       ilu_cardinal,	/* lifetime */
			       gss_OID,		/* secmech */
			       ilu_boolean,	/* accept_only */
			       ILU_C_ENVIRONMENT *);
ILU_RUNTIME_PUBLIC ilu_string
  ILU_C_GSSNameToString (gss_name_t, ILU_C_ENVIRONMENT *);

#endif /* def SECURE_TRANSPORT */

#ifdef SUNRPC_PROTOCOL

ILU_RUNTIME_PUBLIC ilu_IdentityInfo
  ILU_C_AcquireSunRPCAuthUnixIdentity (ilu_string,	/* hostname */
				       ilu_shortcardinal, /* uid */
				       ilu_shortcardinal, /* gud */
				       ilu_shortcardinal, /* ngroups */
				       ilu_shortcardinal *, /* groups */
				       ILU_C_ENVIRONMENT *);
/* Create and return the specified Sun RPC "authunix" identity */

#endif

#ifdef W3MUX_TRANSPORT

ILU_RUNTIME_PUBLIC ilu_IdentityInfo
  ILU_C_AcquireW3muxEndpointIdentity (ilu_string /* uuid, OPTIONAL */,
				      ILU_C_ENVIRONMENT * /* env */);
/* Create and return the specified endpoint identity.  If no "uuid" is specified, will
   return the identity for the default endpoint identity. */

#endif /* def W3MUX_TRANSPORT */

/*L1, L2, Main unconstrained*/

ILU_RUNTIME_PUBLIC ilu_Passport /* pass, optional */
  ILU_C_CreatePassport (ILU_C_ENVIRONMENT *);
/* creates and returns a passport */

ILU_RUNTIME_PUBLIC ilu_IdentityInfo
  ILU_C_CopyIdentity (const struct _ilu_IdentityInfo_s *,
		      ILU_C_ENVIRONMENT *);
/* allocates and returns a copy of the ilu_IdentityInfo parameter */

ILU_RUNTIME_PUBLIC ilu_boolean
  ILU_C_AddIdentity (ilu_Passport /* retain */,
		     const struct _ilu_IdentityInfo_s * /* pass */,
		     ILU_C_ENVIRONMENT *);
/* added identity to Passport.  Only one identity of each type is allowed.
   Returns ilu_TRUE if no errors occur. */

ILU_RUNTIME_PUBLIC ilu_boolean
  ILU_C_FreeIdentity (struct _ilu_IdentityInfo_s * /* pass */,
		      ILU_C_ENVIRONMENT *);
/* Deallocates storage associated with identity if possible.
   Raises NO_PERMISSION if "ii_owned_by_passport" bit of identity
   is set.  Returns ilu_TRUE if no errors occur, ilu_FALSE otherwise. */

ILU_RUNTIME_PUBLIC ilu_IdentityInfo /* optional, retain */
  ILU_C_FindIdentity (ilu_Passport /* retain */,
		      ilu_IdentityType);
/* return identity of specified type, if present.  Returns NIL if not present. */

ILU_RUNTIME_PUBLIC ilu_boolean
  ILU_C_DestroyPassport (ilu_Passport /* pass */,
			 ILU_C_ENVIRONMENT *);
/* frees any associated identities, and if free_passport is specified,
   calls ilu_free() on the ilu_Passport arg. */

ILU_RUNTIME_PUBLIC ilu_cardinal
  ILU_C_DisplayIdentity (ilu_IdentityInfo info,
			 char *buffer,		/* RETAIN, output */
			 ilu_cardinal buffersize,
			 ILU_C_ENVIRONMENT *);
/* puts string form of identity into buffer.  May raise error "bad_param"
   if buffer is too small.  Result may not be NUL terminated.  Returns number of
   chars used in buffer. */

/*==================================================*/
/*==================================================*/
/* Built-in methods on ilu.CString sequence type    */
/*==================================================*/
/*==================================================*/

ILU_RUNTIME_PUBLIC CORBA_unsigned_long ilu_CString_Length (ilu_CString *);
ILU_RUNTIME_PUBLIC CORBA_char * ilu_CString_Nth (ilu_CString *, CORBA_unsigned_long);
ILU_RUNTIME_PUBLIC void ilu_CString_Every (ilu_CString *, void (*)(CORBA_char, void*), void *);
ILU_RUNTIME_PUBLIC void ilu_CString_Append (ilu_CString *, CORBA_char);
ILU_RUNTIME_PUBLIC void ilu_CString_Push (ilu_CString *, CORBA_char);
ILU_RUNTIME_PUBLIC void ilu_CString_Pop (ilu_CString *, CORBA_char *);
ILU_RUNTIME_PUBLIC void ilu_CString_Init (ilu_CString *, CORBA_unsigned_long, CORBA_char *);
ILU_RUNTIME_PUBLIC ilu_CString * ilu_CString_Create (CORBA_unsigned_long, CORBA_char *);
ILU_RUNTIME_PUBLIC ilu_CString * ilu_CString__alloc (void);
ILU_RUNTIME_PUBLIC void ilu_CString__Free (ilu_CString *);

/*==================================================*/
/*==================================================*/
/*           Call statistics gathering              */
/*==================================================*/
/*==================================================*/

#ifdef ILU_C_TIMING_STATISTICS

typedef struct {
  double	minX;	/* minimum value, seconds */
  double	maxX;	/* maximum value, seconds */
  double	sumX;	/* sum of all times, seconds */
  double	sumXX;	/* sum of times squared, seconds^2 */
}               ILU_C_CallStatSum;

typedef struct {
  ilu_cardinal		ncalls;
  ILU_C_CallStatSum	total;
  ILU_C_CallStatSum	user;
  ILU_C_CallStatSum	system;
}		ILU_C_CallStats;

/* Locking unconstrained */

ILU_RUNTIME_PUBLIC void
  ILU_C_SetCallStats (ilu_boolean /* on_off */,
		      ilu_boolean /* clear */);
/* Turns gathering of timing statistics on or off.  If "clear" is ilu_TRUE, will
   also zero counters, which will cause the loss of any accumulated timing
   information.
*/

ILU_RUNTIME_PUBLIC void
  ILU_C_GetCallStats (ilu_cardinal *,	/* INOUT, number of synchronous calls made */
		      ilu_cardinal *,	/* INOUT, number of asynchronous calls made */
		      ILU_C_CallStats *,/* INOUT, total_call time */
		      ILU_C_CallStats * /* INOUT, call_latency time */);
/* the last two arguments should be the addresses of ILU_C_CallStats structures; they will
   be filled with the appropriate timing statistics */

#endif /* def ILU_C_TIMING_STATISTICS */

/*==================================================*/
/*==================================================*/
/*           For internal use                       */
/*==================================================*/
/*==================================================*/

/* locking unconstrained */

ILU_RUNTIME_PUBLIC void
  _ILU_C_CheckStubConsistency (char *, char *, char *);

/*======== sequence methods ========================*/

/* locking is unconstrained in these */

ILU_RUNTIME_PUBLIC void     _ILU_C_AppendGeneric(ILU_C_Sequence h, char *p, int sz);
/* append an element pointed at by `p' of size `sz' to `h' */

ILU_RUNTIME_PUBLIC void
_ILU_C_EveryElement(ILU_C_Sequence h,
		    void (*proc) (void *, void *),
		    int sz, void *data);
/* map `proc' over the elements of `h' (each element of size `sz'), calling with (<element>, `data') */

ILU_RUNTIME_PUBLIC void     _ILU_C_PopGeneric(ILU_C_Sequence h, char *p, int sz);
/* remove the first element of `h', and return it in buffer pointed at by `p', of size `sz' */

ILU_RUNTIME_PUBLIC void     _ILU_C_PushGeneric(ILU_C_Sequence h, char *p, int sz);
/* push the new element pointed at by `p', of size `sz', onto the beginning of `p' */

#undef OPTIONAL
#undef RETAIN
#undef PASS
#undef GLOBAL

#endif

