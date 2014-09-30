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
/* $Id: ilucstub.h,v 1.37 1999/08/03 01:55:26 janssen Exp $ */
/* Last edited by Mike Spreitzer December 2, 1997 11:07 pm PST */

#ifndef __ilu_cstub_h_
#define __ilu_cstub_h_

#include "iluchdrs.h"

#if (defined(ILU_FIXED_POINT_SUPPORT) && defined(ILU_BIGNUM_LIBRARY_SUPPORT))
#include <ilubnops.h>	/* bignum arithmetic, for stubs */
#endif

#define ILU_OPTIONAL(x)	x
#define ILU_PASS(x)		x
#define ILU_RETAIN(x)	x
#define ILU_GLOBAL(x)	x

typedef void (*_ILU_C_Method)(void);
/*
 * Actually, the type is a lie, because different methods have
 * different signatures.  But we pick one type to cast them to while
 * stored in a dispatch table.
 */

typedef struct {
  ilu_Class       ilucdts_introType;
  _ILU_C_Method  *ilucdts_methods;
}               _ILU_C_DispatchTableSection;
/*
 * Represents the part of a dispatch table concerning the methods
 * defined directly on (cc_introType).
 */

typedef struct _ILU_C_Class_s _ILU_C_Class_struct;

struct _ILU_C_Class_s {
  _ILU_C_DispatchTableSection *ilucc_sections;
  ILU_C_FinalizationProc ilucc_finalize;
};
/*
 * The dispatch table consists of a sentinel-terminated sequence of
 * sections.  The sentinel is a section whose (ilucdts_introType) is
 * NIL.  The most specific object type implemented appears in the
 * first section; the other sections do not necessarily appear in
 * any particular order.  The finalization method is represented
 * separately, because it's not introduced in ISL.
 */

struct _ILU_C_Object_s {
  /* L1, L2 unconstrained */

  ILU_C_Class     iluco_class;
  ILU_C_Server    server;
  void           *instanceData;
  struct ILU_C_InterruptHandle_s *interruptH;

  /* L1 >= {server->ilucs_ks} */

  ilu_Object      iluco_ko;
  int             iluco_refcnt;
  int             iluco_kvi;	/* iluco_ko && ilu_VeryInterested */
};

struct _ILU_C_Server_s {
  /*L1, L2 unconstrained*/
  ilu_Server      ilucs_ks;

  /*L1 >= {server->ilucs_ks}*/

  int             ilucs_refcnt;
  int             ilucs_objcnt;	/* num C objs of me */
};

typedef ilu_cardinal (*ILU_C_SizeFn) (ilu_Call c, void *p, ilu_Error * e);
/* type_kind(t) scalar:  <c_parameter_type(t, InOut)> * p;
 * type_kind(t) non-scalar:  <c_parameter_type(t, In)> p.
 */

typedef void    (*ILU_C_OutputFn) (ilu_Call c, void *p, ilu_Error * e);
/* type_kind(t) scalar:  <c_parameter_type(t, InOut)> * p;
 * type_kind(t) non-scalar:  <c_parameter_type(t, In)> p.
 */

#ifdef ILU_HTTPNG_OBJECTS

typedef ilu_cardinal (*ILU_C_LocalObjectSizingFn) (ilu_Call c, ILU_C_Object *p, ilu_Class, ilu_Error * e);
typedef void (*ILU_C_LocalObjectOutputFn) (ilu_Call c, ILU_C_Object *p, ilu_Class, ilu_Error * e);

#endif

typedef void   *(*ILU_C_InputFn) (ilu_Call c, void *p, ilu_Error * e);
/* <c_parameter_type(t, InOut)> p;
   <c_role_type(type, role_InpRet, ilu_FALSE)> returned */

typedef struct _ILU_C_IoFnsRegistration_s {
  ilu_TypeKind		type_kind;
  ilu_string		type_id;
  union {
    ilu_cardinal	value_size;
    ilu_Class		object_class;
  } properties;
  ilu_Type		kernelType;
  ILU_C_SizeFn		sizeFn;
  ILU_C_OutputFn	outFn;
  ILU_C_InputFn		inputFn;
  ILU_C_FreeFn		freeFn;
} *		ILU_C_IoFnsRegistration;

typedef struct _ILU_C_CRInfo_s {
  const struct _ILU_C_IoFnsRegistration_s *	cr_typecode;
  ILU_C_CRCreateFn		cr_create_fn;
  ILU_C_CRFreeFn		cr_free_fn;
  ILU_C_CRPreOutputFn		cr_pre_output_fn;
  ILU_C_CRPostOutputFn		cr_post_output_fn;
  ILU_C_CRPostInputFn		cr_post_input_fn;
} *	_ILU_C_CRInfo;

ILU_RUNTIME_PUBLIC ILU_OPTIONAL(_ILU_C_CRInfo)
     _ILU_C_GetCRInfo (CORBA_TypeCode);

ILU_RUNTIME_PUBLIC ilu_refany
  _ILU_C_CRCreate (_ILU_C_CRInfo,
		   ilu_cardinal /* putative size */,
		   ilu_Error *);

ILU_RUNTIME_PUBLIC void
  _ILU_C_CRFree (_ILU_C_CRInfo,
		ilu_refany	/* pointer to struct */);

ILU_RUNTIME_PUBLIC void
  _ILU_C_CRPreOutput (_ILU_C_CRInfo,
		      ilu_refany, 	/* pointer to struct */
		      ilu_Error *);

ILU_RUNTIME_PUBLIC void
  _ILU_C_CRPostOutput (_ILU_C_CRInfo,
		       ilu_refany, 	/* pointer to struct */
		       ilu_Error *);

ILU_RUNTIME_PUBLIC void
  _ILU_C_CRPostInput (_ILU_C_CRInfo,
		      ilu_refany, 	/* pointer to struct */
		      ilu_Error *);

#ifdef ILU_FIXED_POINT_SUPPORT
struct _ILU_C_FixedPointType_s {
  ilu_Bignum min_numerator;
  ilu_Bignum max_numerator;
  ilu_Bignum denominator;
  ilu_cardinal fixed_digits;
  ilu_cardinal fixed_decimal_places;
  ilu_FixedPointRangeSize range_size;
};
#endif

ILU_RUNTIME_PUBLIC struct _ILU_C_IoFnsRegistration_s _CORBA_boolean__IoFns;

ILU_RUNTIME_PUBLIC struct _ILU_C_IoFnsRegistration_s _CORBA_octet__IoFns;

ILU_RUNTIME_PUBLIC struct _ILU_C_IoFnsRegistration_s _CORBA_short__IoFns;
ILU_RUNTIME_PUBLIC struct _ILU_C_IoFnsRegistration_s _CORBA_long__IoFns;
ILU_RUNTIME_PUBLIC struct _ILU_C_IoFnsRegistration_s _CORBA_long_long__IoFns;

ILU_RUNTIME_PUBLIC struct _ILU_C_IoFnsRegistration_s _CORBA_unsigned_short__IoFns;
ILU_RUNTIME_PUBLIC struct _ILU_C_IoFnsRegistration_s _CORBA_unsigned_long__IoFns;
ILU_RUNTIME_PUBLIC struct _ILU_C_IoFnsRegistration_s _CORBA_unsigned_long_long__IoFns;

ILU_RUNTIME_PUBLIC struct _ILU_C_IoFnsRegistration_s _CORBA_char__IoFns;
ILU_RUNTIME_PUBLIC struct _ILU_C_IoFnsRegistration_s _CORBA_wchar__IoFns;

ILU_RUNTIME_PUBLIC struct _ILU_C_IoFnsRegistration_s _CORBA_float__IoFns;
ILU_RUNTIME_PUBLIC struct _ILU_C_IoFnsRegistration_s _CORBA_double__IoFns;
ILU_RUNTIME_PUBLIC struct _ILU_C_IoFnsRegistration_s _CORBA_long_double__IoFns;

ILU_RUNTIME_PUBLIC struct _ILU_C_IoFnsRegistration_s _CORBA_string__IoFns;
#define _ilu_CString__IoFns _CORBA_string__IoFns

ILU_RUNTIME_PUBLIC struct _ILU_C_IoFnsRegistration_s _ilu_CORBA_Object__IoFns;

ILU_RUNTIME_PUBLIC void
  _ILU_C_RegisterIoFns (ILU_C_IoFnsRegistration);

ILU_RUNTIME_PUBLIC ILU_C_IoFnsRegistration
  _ILU_C_LookupIoFns (ilu_string);		/* type UID */

#ifdef ADD_VARIANT_SUPPORT

ILU_RUNTIME_PUBLIC struct _ILU_C_IoFnsRegistration_s _CORBA_any__IoFns;
#define _ilu_any__IoFns _CORBA_any__IoFns

ILU_RUNTIME_PUBLIC void
  _CORBA_any__Output (ilu_Call, CORBA_any*, ilu_Error *);

ILU_RUNTIME_PUBLIC ilu_cardinal
  _CORBA_any__SizeOf (ilu_Call, CORBA_any*, ilu_Error *);

ILU_RUNTIME_PUBLIC CORBA_any *
  _CORBA_any__Input (ilu_Call, CORBA_any*, ilu_Error *);

ILU_RUNTIME_PUBLIC void
  CORBA_any__Free (CORBA_any*);

ILU_RUNTIME_PUBLIC CORBA_any *
  CORBA_sequence_CORBA_any_allocbuf (ilu_cardinal _count);

ILU_RUNTIME_PUBLIC CORBA_any *
  CORBA_any_alloc (void);

ILU_RUNTIME_PUBLIC ilu_boolean
  ILU_C_AutomaticUnpickling;
/* ilu_FALSE to prevent automatic unpickling of 'any' values */

#endif /* ADD_VARIANT_SUPPORT */

#define ILU_C_KERNEL_TYPE(tname)	(_ ## tname ## __IoFns.kernelType)

struct _ILU_C_ExceptionDescription_s {
  unsigned int    size;
  ilu_Class       *ed_class;
  ILU_C_SizeFn    sizeFn;
  ILU_C_OutputFn  outFn;
  ILU_C_InputFn   inFn;
  ILU_C_FreeFn    freeFn;
};

/* Main Invariant holds; L2 not further constrained */
ILU_RUNTIME_PUBLIC void 
_ILU_C_Object_release_full(ILU_C_Object * o,
			   char *filename, int lineno);

#define _ILU_C_Object_release(o) _ILU_C_Object_release_full(o, __FILE__, __LINE__)

ILU_RUNTIME_PUBLIC ilu_cardinal _ilu_CString__SizeOf (ilu_Call, ilu_CString, ilu_Error *);
ILU_RUNTIME_PUBLIC ilu_CString *_ilu_CString__Input (ilu_Call, ilu_CString *, ilu_Error *);
ILU_RUNTIME_PUBLIC void _ilu_CString__Output (ilu_Call, ilu_CString, ilu_Error *);

ILU_RUNTIME_PUBLIC void _ILU_C_OutputWString (ilu_Call, CORBA_wchar *, ilu_cardinal, ilu_cardinal, ilu_Error *);
ILU_RUNTIME_PUBLIC ilu_cardinal _ILU_C_SizeOfWString (ilu_Call, CORBA_wchar *, ilu_cardinal, ilu_cardinal, ilu_Error *);
ILU_RUNTIME_PUBLIC CORBA_wchar * _ILU_C_InputWString (ilu_Call, CORBA_wchar **, ilu_cardinal*, ilu_cardinal, ilu_Error *);

ILU_RUNTIME_PUBLIC ilu_cardinal _ILU_C_Enumeration__SizeOf (ilu_Call, void *, ilu_Error *);
ILU_RUNTIME_PUBLIC void * _ILU_C_Enumeration__Input (ilu_Call, void *, ilu_Error *);
ILU_RUNTIME_PUBLIC void _ILU_C_Enumeration__Output (ilu_Call, void *, ilu_Error *);

ILU_RUNTIME_PUBLIC ilu_cardinal _ILU_C_SizeExtensibleRecord (ilu_Call, void *, ilu_Error *);
ILU_RUNTIME_PUBLIC void _ILU_C_OutputExtensibleRecord (ilu_Call, void *, ilu_Error *);
ILU_RUNTIME_PUBLIC void _ILU_C_InputExtensibleRecord (ilu_Call, void **, ilu_Error *);

/*======== boolean methods ========================*/
/* Note that CORBA boolean is different from ILU boolean */

ILU_RUNTIME_PUBLIC ilu_cardinal ILU_C_SizeOfBoolean (ilu_Call, CORBA_boolean, ilu_Error *);
ILU_RUNTIME_PUBLIC void ILU_C_OutputBoolean (ilu_Call, CORBA_boolean, ilu_Error *);
ILU_RUNTIME_PUBLIC void ILU_C_InputBoolean (ilu_Call, CORBA_boolean *, ilu_Error *);

/*======== wchar methods ========================*/
/* Note that CORBA wchar is different from ILU character */

ILU_RUNTIME_PUBLIC ilu_cardinal ILU_C_SizeOfCharacter (ilu_Call, CORBA_wchar, ilu_Error *);
ILU_RUNTIME_PUBLIC void ILU_C_OutputCharacter (ilu_Call, CORBA_wchar, ilu_Error *);
ILU_RUNTIME_PUBLIC void ILU_C_InputCharacter (ilu_Call, CORBA_wchar *, ilu_Error *);

/* Main invariant holds */
ILU_RUNTIME_PUBLIC void 
_ILU_C_SetProtocolError(CORBA_Environment *,
			ilu_ProtocolException);
/*
 * Set the given CORBA_Environment to indicate the system exception
 * corresponding to the given ilu_ProtocolException, if appropriate.
 * It's not appropriate exactly when the given ilu_ProtocolException
 * is either ilu_ProtocolException_Success or
 * ilu_ProtocolException_Not; in the latter case it's the caller's
 * responsiblity to get the information from the corresponding
 * ilu_Error into the CORBA_Environment.
 */

/* Main invariant holds */
ILU_RUNTIME_PUBLIC ilu_boolean	_ILU_C_CheckSibling (ILU_C_Object *disc,
						     ILU_C_Object *putative_sibling,
						     ILU_C_ENVIRONMENT *status);

/* Main invariant holds */
ILU_RUNTIME_PUBLIC ilu_LanguageIndex	_ILU_C_LanguageIndex;

/* Main invariant holds */
/*
  _ILU_C_GenericCall

  void _ILU_C_GenericCall (ilu_Class class, ilu_Method method,
                           _ILU_C_ExceptionDescription *evec,
                           ILU_C_Object *discriminant,
                           ILU_C_ENVIRONMENT *status,
                           char *argdesc, ...)

Operates in varargs mode, off "argdesc".  "argdesc" provides a
description of the arguments to the method, as follows:

"argdesc" contains 2 fields, separated by ':'.

The first field provides a list of the types of all the arguments
which are either In or InOut arguments, each as a one-character code.
The codes are capital letters if the argument is an InOut argument,
lower-case if an In argument.  Specific types are encoded as follows:

  a & A:  short integer
  b & B:  integer
  c & C:  long integer
  d & D:  short cardinal
  e & E:  cardinal
  f & F:  long cardinal
  g & G:  short real
  h & H:  real
  i & I:  long real
  j & J:  short character
  k & K:  character
  l & L:  long character
  m & m:  byte
  n & N:  boolean
  o & O:  enumeration
  p & P:  object
  q & Q:  string
  r & R:  wide string
  z & Z:  other

  s-y & S-Y:  reserved for future use

The following table tells what's found in the varargs list for each case:

a,d,j,k,m,n:	int
b,c,e,f,g,h,i,l,o: <c_parameter_type(t, In)>
A-O:		<c_parameter_type(t, InOut)>
p:		<c_parameter_type(t, In)>,	ilu_Class pclass
P:		<c_parameter_type(t, InOut)>,	ilu_Class pclass
q, r:		<c_parameter_type(t, In)>,	ilu_cardinal limit
Q, R:		<c_parameter_type(t, InOut)>,	ilu_cardinal limit
z:		<c_parameter_type(t, In)>,	ILU_C_{Size,Output}Fn
Z:		<c_parameter_type(t, InOut)>,	ILU_C_{Size,Output}Fn

The sequence of value specified by the first field of "argdesc"
actually appears twice in the varargs part of the _ILU_C_GenericCall
arglist, once for a sizing pass and once for an output pass (there is
no assurance that "va_start" can be called twice in the same
function).

The second field of "argdesc" contains a list of the types of all the
arguments which are either the return type of the method, or Out or
InOut arguments to the method, again as one-character codes, using the
same coding scheme, but this time with lower-case characters
representing Out arguments.  For the purposes of this routine, the
return value of the method is passed and coded as another Out
argument.  The return value argument appears before the actual Out and
InOut arguments in the arglist.

The following table tells what's found in the varargs list for each case:

a-o, A-O:	<c_parameter_type(t, InOut)>
p, P:		<c_parameter_type(t, InOut)>,	ilu_Class pclass
q, r, Q, R:	<c_parameter_type(t, InOut)>,	ilu_cardinal limit
y:		<c_type_role(t, role_InpRet, ilu_FALSE)>*, ILU_C_InputFn
z, Z:		<c_parameter_type(t, InOut)>,	ILU_C_InputFn


*/
ILU_RUNTIME_PUBLIC void
  _ILU_C_GenericCall (ilu_Class objtype, ilu_Method method,
		      _ILU_C_ExceptionDescription *evec,
		      ILU_C_Object *discriminant,
		      ILU_C_ENVIRONMENT *status,
		      char *argdesc, ...);

typedef struct _ILU_C_ParmDesc_s {
  unsigned				parm_out : 1;
  unsigned				parm_in : 1;
  unsigned				parm_needs_dereference : 1;
  unsigned				parm_needs_assignment : 1;
  unsigned				parm_extensible_record : 1;
  struct _ILU_C_IoFnsRegistration_s *	parm_type;
  void *				parm_val;	/* <c_parameter_type(t, InOut)> */
} _ILU_C_ParmDesc;

ILU_RUNTIME_PUBLIC void
  _ILU_C_VectorCall (ilu_Class pclass, ilu_Method method,
		     _ILU_C_ExceptionDescription *evec,
		     ILU_C_Object *discriminant,
		     ILU_C_ENVIRONMENT *status,
		     ilu_cardinal nparms,
		     _ILU_C_ParmDesc *parms);

/*L1, L2 unconstrained*/
ILU_RUNTIME_PUBLIC void
_ILU_C_ConvertError(ILU_C_ENVIRONMENT * env,
		    ilu_Error * err,
		    ILU_C_COMPLETIONSTATUS cstat);
/*
 * Stubs call this when the kernel raises an error, to convert the
 * error into CORBA terms.  Sets *env (to a CORBA_SYSTEM_EXCEPTION
 * or CORBA_NO_EXCEPTION) according to *err and cstat. Calls
 * ILU_HANDLED(*err);
 */

/*L1, L2 unconstrained*/
#define _ILU_C_ConvertErrorQ(env,err,cc) \
	_ILU_C_ConvertError(env,err,ILU_C_COMPLETION(cc))

/*before: not Inside (cobj->server, cobj->type->c) */
/*after:  return != NULL => Inside(cobj->server, cobj->type->c) */
ILU_RUNTIME_PUBLIC ilu_Object _ILU_C_KernelObjOfObj(ILU_C_Object *);

/* L1, L2, Main unconstrained */
ILU_RUNTIME_PUBLIC void 
_ILU_C_RegisterSurrogateCType(ILU_GLOBAL(ilu_Class) ilutype,
			      ILU_GLOBAL(ILU_C_Class) iluc_class);

/* L1, L2, Main unconstrained */
ILU_RUNTIME_PUBLIC _ILU_C_Method
_ILU_C_FindMethod(ILU_RETAIN(ILU_C_Object *), ILU_GLOBAL(ilu_Class), int);

/**before: Call-Locking(call, IHi);
    after: Call-Invariant(call, err),
	   success => Call-Locking(call, VLo)*/
ILU_RUNTIME_PUBLIC          ilu_boolean
_ILU_C_FinishParameters(ilu_Call,
			ILU_OPTIONAL(ILU_C_Object * disc),
			ILU_ERRS((bad_locks, broken_locks)) *err);
/*
 * Calls ilu_RequestRead, and re-registers request handler if
 * appropriate.
 */

/**before: Main Invariant && Call-VLo(call);
    after: Call-Invariant(call, err),
	   success => call->ca_ms == ilu_cmsHi. */
ILU_RUNTIME_PUBLIC ilu_boolean 
_ILU_C_BeginReply(ilu_Call call, ilu_boolean exceptions,
		  ilu_cardinal argSize,
		  ILU_ERRS((bad_locks, IoErrs)) *err);

/**before: Main Invariant && Call-VLo(call);
    after: Call-Invariant(call, err),
	   success => call->ca_ms == ilu_cmsHi. */
ILU_RUNTIME_PUBLIC ilu_boolean 
_ILU_C_BeginException(ilu_Call call, ilu_cardinal evalue,
		      ilu_cardinal argSize,
		      ILU_ERRS((bad_locks, IoErrs)) *err);

/*Main Invariant; Call-Locking(call, OHi)*/
ILU_RUNTIME_PUBLIC          ilu_boolean
_ILU_C_FinishReply(ilu_Call call, ILU_ERRS((bad_locks, IoErrs)) *err);

/*Main Invariant; Call-Locking(call, OHi)*/
ILU_RUNTIME_PUBLIC ilu_boolean 
_ILU_C_FinishException(ilu_Call call, ILU_ERRS((bad_locks, IoErrs)) *err);

/**before: Main Invariant && Call-Lo(call);
    after: Call-Invariant(call, err),
	   success => call->ca_ms == ilu_cmsHi. */
ILU_RUNTIME_PUBLIC void 
_ILU_C_SendException(ilu_Call call, _ILU_C_ExceptionDescription *evec,
		     ILU_C_ENVIRONMENT * status, ilu_Error * err);

/* Main Invariant */
ILU_RUNTIME_PUBLIC void 
_ILU_C_FreeException(ilu_Method method, _ILU_C_ExceptionDescription *evec,
		     ILU_C_ENVIRONMENT * status, ilu_Error * err);

/**before: Main Invariant, Call-Lo(call);
    after: Call-Invariant(call, err),
	   success => call->ca_ms == ilu_cmsNo*/
ILU_RUNTIME_PUBLIC          ilu_boolean
_ILU_C_NoReply(ilu_Call call,
	     ILU_ERRS((bad_param, bad_locks, broken_locks)) * err);

/**Before: Call-Invariant(call, err);
    After: Main Invariant*/
ILU_RUNTIME_PUBLIC void _ILU_C_FinishServingCall(ilu_Call call, ilu_Error *err);

/**before: Main Invariant, Call-Hi(call);
    after: Call-Invariant(call, err) && call->ca_ms == ilu_cmsHi*/
ILU_RUNTIME_PUBLIC ILU_C_Object *
_ILU_C_GetServerSingleton(ilu_Call, ILU_ERRS((bad_param)) *err);

/* L1, L2, Main unconstrained */
ilu_boolean _ILU_C_IsSingleton (ilu_Class c);

/*Main invariant, Call-Hi(call)*/
ILU_RUNTIME_PUBLIC ILU_C_Object *
_ILU_C_InputObject(ilu_Call call, ilu_Class putative_class,
		   ilu_boolean discriminator_p,
		   ILU_ERRS((IoErrs)) * err);

/*Main invariant, Call-Hi(call)*/
ILU_RUNTIME_PUBLIC          ilu_boolean
_ILU_C_OutputObject(ilu_Call call, ILU_C_Object * obj,
		    ilu_Class putative_class,
		    ilu_boolean discriminator_p,
		    ILU_ERRS((IoErrs)) * err);

/* Main invariant holds */
ILU_RUNTIME_PUBLIC          ilu_cardinal
_ILU_C_SizeOfObject(ilu_Call call, ILU_C_Object * obj,
		    ilu_Class putative_class,
		    ilu_boolean discriminator_p,
		    ILU_ERRS((IoErrs)) *err);

/* Main invariant holds */
ILU_RUNTIME_PUBLIC ilu_boolean
  _ILU_C_SetCallerContext (ilu_Passport);
/*
 * Associate the given ilu_Passport with future calls made in the
 * same thread, until overridden by a later call on
 * ILU_C_SetPassportContext.
 */

typedef ilu_cardinal (*_ILU_C_AlignmentProc) (ilu_Call, ilu_cardinal, ilu_Error *);

/* Unrestricted */
ILU_RUNTIME_PUBLIC ilu_boolean _ILU_C_CanMoveAsBlock (ilu_Call,
						      ilu_Type,		/* type T */
						      ilu_cardinal,	/* max values */
						      ilu_cardinal *,	/* alignment code */
						      _ILU_C_AlignmentProc *,
						      ilu_Error *);
/* Checks to see if a vector of values of type T can be output as
   a block write of bytes.  Returns true if non-error and block move is
   possible.  Returns false if error or block move not possible.
   if alignmentproc return value is non-NULLFN, the alignment proc
   should be called with the alignment code before doing the block
   marshalling.
*/

/* Unrestricted */
ILU_RUNTIME_PUBLIC void _ILU_C_ExtendString (CORBA_char **s, CORBA_char item, CORBA_boolean atend);

/* Unrestricted */
ILU_RUNTIME_PUBLIC void _ILU_C_ExtendWString (CORBA_wchar **s, CORBA_wchar item, CORBA_boolean atend);

/* Unrestricted */
ILU_RUNTIME_PUBLIC void _ILU_C_PopString (CORBA_char **s, CORBA_char *item);

/* Unrestricted */
ILU_RUNTIME_PUBLIC void _ILU_C_PopWString (CORBA_wchar **s, CORBA_wchar *item);

/* Unrestricted */
ILU_RUNTIME_PUBLIC unsigned int _ILU_C_SafeStrlen(ILU_RETAIN(char *str));

/* Unrestricted */
ILU_RUNTIME_PUBLIC unsigned int _ILU_C_SafeWStrlen(ILU_RETAIN(CORBA_wchar *str));

/* Unrestricted */
ILU_RUNTIME_PUBLIC          ILU_PASS(void *) _ILU_C_MallocFailure(CORBA_unsigned_long /* nbytes */);

/*Main invariant holds*/
ILU_RUNTIME_PUBLIC void     _ILU_C_EnsureGcClient(void);
/* The stubs of an interface that defines collectible object types
   calls this at least once, at intialization time. */

#undef OPTIONAL
#undef RETAIN
#undef PASS
#undef GLOBAL

#endif

