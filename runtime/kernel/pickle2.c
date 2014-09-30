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

$Id: pickle2.c,v 1.36 1999/08/03 01:52:50 janssen Exp $
*/
/* Last edited by Mike Spreitzer September 18, 1998 2:23 pm PDT */
/* Chris Jacobi, September 10, 1998 10:29 am PDT */

/*============================================================
 *
 * This file implements ILU's notion of persistent values,
 * also called `pickle's.  A version 1 pickle consists of four
 * elements:  a version number, a `type kind' code for the type
 * of the pickled value, a type ID (the ILU type ID or CORBA
 * repository ID string) for the pickled value, immediately
 * followed by the marshalled value.  The version number and
 * type kind are in the first byte of the pickle, the version in
 * the high order 3 bits, the type kind in the low order 5 bits.
 * For type kinds which represent certain basic types
 * (ilu.{short,long,}cardinal, ilu.{short,long,}integer,
 * ilu.{short,}character,  ilu.{short,long,}real, ilu.byte,
 * ilu.boolean, and ilu.CString), the type ID value is implicit
 * and omitted; for pickles of other types, the type ID is explicit
 * and immediately follows the the version/type-kind byte.
 *
 * The marshalling format is straightforward; primitive values are
 * marshalled in big-endian order, with no padding or alignment.
 * Boolean values are marshalled as single bytes (0 for false, 1 for
 * true), enumerations as 16-bit unsigned values.  Sequences have
 * 4-byte length prefixes giving the number of elements.  The
 * marshalled form of a string does not contain a terminating NUL
 * character.
 *
 * To create a pickle for a value V of type T (error checking and
 * handling omitted):
 *
 *	ilu_Call_s	call;
 *	ilu_Error	err;
 *	ilu_cardinal	size;
 *	ilu_bytes	pickled_value;
 *	ilu_cardinal	pickled_value_len;
 *	ilu_Class	T;
 *	ilu_Pickle	pickle;
 *	T		value;
 *
 *	ilu_StartPickle (&call, &err);
 *	size = T__SizeOf (&call, &V, &err);
 *	ilu_WritePickle (call, size, T->cl_unique_id, &err);
 *	T__Output (&call, &V, &err);
 *	ilu_EndPickle (call, &pickle, &err);
 *
 * To unmarshal a pickled value of type T (again, error checking and
 * handling omitted):
 *
 *	ilu_Call_s	call;
 *	ilu_Error	err;
 *	ilu_string	type_id;
 *	T		value;
 *	ilu_Pickle	pickle;
 *
 *	ilu_StartPickle (&call, &err);
 *	ilu_ReadPickle (&call, pickle, &err);
 *	T__Input (&call, &value, &err);
 *	ilu_EndPickle (&call, &pickle, &err);
 *
 *============================================================ */

#include "iluntrnl.h"
#include "ilutypes.h"
#include "object.h"
#include "call.h"
#include "connect.h"
#include "type.h"
#include "iluprotocol.h"

#define CURRENT_VERSION			2
#define SHIFTED_VERSION			((CURRENT_VERSION << 5) & 0xE0)
#define PICKLE_VERSION(x)		(((x) >> 5) & 0x7)
#define PICKLE_TYPEKIND(x)		((x) & 0x1F)

#define EXTENSIBLE_RECORD_TYPEKIND	((unsigned char) 0x1D)
#define ILU_CSTRING_TYPEKIND		((unsigned char) 0x1E)
#define HAS_TYPEID_TYPEKIND		((unsigned char) 0x1F)
#define OBJECT_TYPEKIND			((unsigned char) ilu_object_tk)

#define PICKLE_BUF(call)		((call)->ca_msg.msg_base)
#define PICKLE_LEN(call)		((call)->ca_msg.msg_len)
#define PICKLE_NEXT(call)		((call)->ca_prdata1)
#define PICKLE_REMAINING(call)		(PICKLE_LEN(call) - PICKLE_NEXT(call))

#define PICKLE_GET_BUFFER(call,len,err)		\
  (((PICKLE_BUF(call) != NIL) && (PICKLE_REMAINING(call) >= len)) \
   ? (ILU_CLER(*(err)), \
      PICKLE_NEXT(call) = PICKLE_NEXT(call) + (len), \
      PICKLE_BUF(call) + (PICKLE_NEXT(call) - (len))) \
   : (((call)->ca_incoming) \
      ? ILU_ERR_CONS1(marshal, (err), minor, ilu_mm_eom, ILU_NIL) \
      : (PICKLE_LEN(call) = PICKLE_LEN(call) + (len), \
	 PICKLE_BUF(call) = ilu_ReallocE(PICKLE_BUF(call), PICKLE_LEN(call), (err)), \
	 (ILU_ERRNOK(*(err)) \
	  ? ILU_NIL \
	  : (PICKLE_NEXT(call) = PICKLE_NEXT(call) + (len), \
	     PICKLE_BUF(call) + (PICKLE_NEXT(call) - (len)))))))

typedef struct pickle2_data_s {
  ilu_Type	prefix_type;
  ilu_string	object_typestring;
} *pickle2_data;

#define PREFIX_TYPE(call) ((pickle2_data)(call->ca_prdata2))->prefix_type
#define OBJECT_TYPESTRING(call) ((pickle2_data)(call->ca_prdata2))->object_typestring

/* ==================== byte ==================== */

static void 
_pickle_OutputByte(ilu_Call call, ilu_byte b, ILU_ERRS((IoErrs)) * err)
{
  ilu_byte *buf = PICKLE_GET_BUFFER(call, 1, err);
  if (ILU_ERROK(*err))
    *buf = b; 
}

static void 
_pickle_InputByte(ilu_Call call, ilu_byte * b,
		  ILU_ERRS((IoErrs)) * err)
{
  ilu_byte *buf = PICKLE_GET_BUFFER(call, 1, err);
  if (ILU_ERROK(*err))
    *b = *buf;
}

/*ARGSUSED*/
static          ilu_cardinal
_pickle_SizeOfByte(ilu_Call call, ilu_byte i, ILU_ERRS((IoErrs)) * err)
{
  ILU_CLER(*err);
  return (1);
}

/* ==================== cardinal ==================== */

#define SWAP_WORD(a) ( ((a) << 24) | \
                      (((a) << 8) & 0x00ff0000) | \
                      (((a) >> 8) & 0x0000ff00) | \
        ((ilu_cardinal)(a) >>24) )

static void
_pickle_OutputCardinal(ilu_Call call, ilu_cardinal l,
		       ILU_ERRS((IoErrs)) * err)
{
  void *buf = PICKLE_GET_BUFFER(call, 4, err);
  ilu_cardinal temp;

  if (ILU_ERROK(*err)) {
#ifdef WORDS_BIGENDIAN
    temp = l;
#else
    temp = SWAP_WORD(l);
#endif
    memcpy(buf, (void *) &temp, 4);
  }
}

static void
_pickle_InputCardinal(ilu_Call call, ilu_cardinal * i,
		      ILU_ERRS((IoErrs)) * err)
{
  void *buf = PICKLE_GET_BUFFER(call, 4, err);

  if (ILU_ERROK(*err)) {
#ifdef WORDS_BIGENDIAN
    memcpy ((void *) i, buf, 4);
#else
    ilu_cardinal temp;
    memcpy ((void *) &temp, buf, 4);
    *i = SWAP_WORD(temp);
#endif
  }
}

/*ARGSUSED*/
static ilu_cardinal _pickle_SizeOfCardinal (ilu_Call call, ilu_cardinal i,
		      ILU_ERRS((IoErrs)) * err)
{
  ILU_CLER(*err);
  return (4);
}

/* ==================== short cardinal ==================== */

static void 
_pickle_OutputShortCardinal(ilu_Call call, ilu_shortcardinal i,
			    ILU_ERRS((IoErrs)) * err)
{
  ilu_byte *buf = PICKLE_GET_BUFFER(call, 2, err);

  if (ILU_ERROK(*err)) {
    buf[0] = ((unsigned) (i & 0xFF00)) >> 8;
    buf[1] = i & 0xFF;
  }
}

static void
_pickle_InputShortCardinal(ilu_Call call, ilu_shortcardinal * i,
			   ILU_ERRS((IoErrs)) * err)
{
  ilu_byte *buf = PICKLE_GET_BUFFER(call, 2, err);

  if (ILU_ERROK(*err)) {
#ifdef WORDS_BIGENDIAN
    ((char *)i)[0] = buf[0];
    ((char *)i)[1] = buf[1];
#else
    ((char *)i)[1] = buf[0];
    ((char *)i)[0] = buf[1];
#endif
  }
}

/*ARGSUSED*/
static ilu_cardinal 
_pickle_SizeOfShortCardinal(ilu_Call call, ilu_shortcardinal i,
			    ILU_ERRS((IoErrs)) * err)
{
  ILU_CLER(*err);
  return (2);
}

/* ==================== long cardinal ==================== */

static void 
_pickle_OutputLongCardinal(ilu_Call call, ilu_longcardinal i,
			   ILU_ERRS((IoErrs)) * err)
{
  _pickle_OutputCardinal(call, ILU_LONGCARD_HIGH_WORD(&i), err);
  if (ILU_ERROK(*err))
    _pickle_OutputCardinal(call, ILU_LONGCARD_LOW_WORD(&i), err);
}

static void
_pickle_InputLongCardinal(ilu_Call call, ilu_longcardinal * i,
			  ILU_ERRS((IoErrs)) * err)
{
  _pickle_InputCardinal (call, &ILU_LONGCARD_HIGH_WORD(i), err);
  if (ILU_ERROK(*err))
    _pickle_InputCardinal (call, &ILU_LONGCARD_LOW_WORD(i), err);
  return;
}

/*ARGSUSED*/
static ilu_cardinal _pickle_SizeOfLongCardinal (ilu_Call call, ilu_longcardinal i,
			    ILU_ERRS((IoErrs)) * err)
{
  ILU_CLER(*err);
  return (8);
}

/* ==================== real ==================== */

static void 
_pickle_OutputReal(ilu_Call call, double d,
		   ILU_ERRS((IoErrs)) * err)
{
  double          l2 = d;

#ifdef WORDS_BIGENDIAN
  _pickle_OutputCardinal(call, ((ilu_cardinal *) (&l2))[0], err);
  if (ILU_ERROK(*err))
    _pickle_OutputCardinal(call, ((ilu_cardinal *) (&l2))[1], err);
#else
  _pickle_OutputCardinal(call, ((ilu_cardinal *) (&l2))[1], err);
  if (ILU_ERROK(*err))
    _pickle_OutputCardinal(call, ((ilu_cardinal *) (&l2))[0], err);
#endif
}

static void 
_pickle_InputReal(ilu_Call call, double *d,
		  ILU_ERRS((IoErrs)) * err)
{
  double l2;

#ifdef WORDS_BIGENDIAN
  _pickle_InputCardinal(call, ((ilu_cardinal *) &l2), err);
  if (ILU_ERROK(*err))
    {
      _pickle_InputCardinal(call, ((ilu_cardinal *) &l2) + 1, err);
      if (ILU_ERROK(*err)) *d = l2;
    }
#else
  _pickle_InputCardinal(call, ((ilu_cardinal *) &l2) + 1, err);
  if (ILU_ERROK(*err))
    {
      _pickle_InputCardinal(call, ((ilu_cardinal *) &l2), err);
      if (ILU_ERROK(*err)) *d = l2;
    }
#endif

}

/*ARGSUSED*/
static ilu_cardinal _pickle_SizeOfReal (ilu_Call call, double d,
		  ILU_ERRS((IoErrs)) * err)
{
  ILU_CLER(*err);
  return (8);
}

/* ==================== long real ==================== */

static void 
_pickle_OutputLongReal(ilu_Call call, ilu_longreal d,
		       ILU_ERRS((IoErrs)) * err)
{
  ilu_longreal l2 = d;

#ifdef WORDS_BIGENDIAN
  _pickle_OutputCardinal (call, ((ilu_cardinal *) &l2)[0], err);
  if (ILU_ERROK(*err)) {
    _pickle_OutputCardinal (call, ((ilu_cardinal *) &l2)[1], err);
    if (ILU_ERROK(*err)) {
      _pickle_OutputCardinal (call, ((ilu_cardinal *) &l2)[2], err);
      if (ILU_ERROK(*err)) {
	_pickle_OutputCardinal (call, ((ilu_cardinal *) &l2)[3], err);
      }
    }
  }
#else
  _pickle_OutputCardinal (call, ((ilu_cardinal *) &l2)[3], err);
  if (ILU_ERROK(*err)) {
    _pickle_OutputCardinal (call, ((ilu_cardinal *) &l2)[2], err);
    if (ILU_ERROK(*err)) {
      _pickle_OutputCardinal (call, ((ilu_cardinal *) &l2)[1], err);
      if (ILU_ERROK(*err)) {
	_pickle_OutputCardinal (call, ((ilu_cardinal *) &l2)[0], err);
      }
    }
  }
#endif
}

static void
_pickle_InputLongReal(ilu_Call call, ilu_longreal * d,
		      ILU_ERRS((IoErrs)) * err)
{
#ifdef WORDS_BIGENDIAN
  _pickle_InputCardinal (call, &(((ilu_cardinal *) d)[0]), err);
  if (ILU_ERROK(*err)) {
    _pickle_InputCardinal (call, &(((ilu_cardinal *) d)[1]), err);
    if (ILU_ERROK(*err)) {
      _pickle_InputCardinal (call, &(((ilu_cardinal *) d)[2]), err);
      if (ILU_ERROK(*err)) {
	_pickle_InputCardinal (call, &(((ilu_cardinal *) d)[3]), err);
      }
    }
  }
#else
  _pickle_InputCardinal (call, &(((ilu_cardinal *) d)[0]), err);
  if (ILU_ERROK(*err)) {
    _pickle_InputCardinal (call, &(((ilu_cardinal *) d)[1]), err);
    if (ILU_ERROK(*err)) {
      _pickle_InputCardinal (call, &(((ilu_cardinal *) d)[2]), err);
      if (ILU_ERROK(*err)) {
	_pickle_InputCardinal (call, &(((ilu_cardinal *) d)[3]), err);
      }
    }
  }
#endif
}

/*ARGSUSED*/
static ilu_cardinal _pickle_SizeOfLongReal (ilu_Call call, ilu_longreal d,
		  ILU_ERRS((IoErrs)) * err)
{
  ILU_CLER(*err);
  return (16);
}

/* ==================== short real ==================== */

static void 
_pickle_OutputShortReal(ilu_Call call, float f,
			ILU_ERRS((IoErrs)) * err)
{
  float           f2;

  f2 = f;
  _pickle_OutputCardinal (call, *((ilu_cardinal *) & f2), err);
}

static void 
_pickle_InputShortReal(ilu_Call call, float *f,
		       ILU_ERRS((IoErrs)) * err)
{
  _pickle_InputCardinal (call, (ilu_cardinal *) f, err);
}

/*ARGSUSED*/
static ilu_cardinal _pickle_SizeOfShortReal (ilu_Call call, float d,
		       ILU_ERRS((IoErrs)) * err)
{
  ILU_CLER(*err);
  return (4);
}

/* ==================== bytes ==================== */

static void
_pickle_OutputBytes(ilu_Call call, ilu_bytes s, ilu_cardinal len,
		    ilu_cardinal limit, ILU_ERRS((IoErrs)) * err)
{
  ilu_bytes buf;

  if (limit > 0 && len > limit) {
    ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_sequenceLimit, 0);
    return;
  } else {
    _pickle_OutputCardinal(call, len, err);
    if (ILU_ERRNOK(*err)) return;
    buf = PICKLE_GET_BUFFER(call, len, err);
    if (ILU_ERRNOK(*err)) return;
    memcpy((void *) buf, (void *) s, len);
  }
}

static void
InputBytes(ilu_Call call, ilu_bytes * s, ilu_cardinal * len,
	   ilu_cardinal limit, ILU_ERRS((IoErrs)) * err,
	   ilu_boolean string_p)
{
  ilu_bytes buf;

  ILU_CLER(*err);
  if (_pickle_InputCardinal(call, len, err), ILU_ERROK(*err)) {
    if (limit > 0 && *len > limit) {
      ILU_ERR_CONS1(marshal, err, minor, ilu_mm_sequenceLimit, 0);
      return;
    }
    if (!string_p && (*len) == 0)
      {
	*s = NIL;
	return;
      }
    buf = PICKLE_GET_BUFFER(call, *len, err);
    if (ILU_ERRNOK(*err)) return;
    *s = ilu_malloc((*len) + (string_p ? 1 : 0));
    if (*s == NIL) {
      ILU_ERR_CONS1(no_memory, err, nbytes, (*len) + (string_p ? 1 : 0), 0);
      return;
    }
    memcpy((void *) *s, (void *) buf, *len);
    if (string_p)
      (*s)[*len] = 0;
    /* ... so this can be used to input a C string */
  }
  return;
}

static void
_pickle_InputBytes(ilu_Call call, ilu_bytes * s, ilu_cardinal * len,
		   ilu_cardinal limit, ILU_ERRS((IoErrs)) * err)
{
  InputBytes (call, s, len, limit, err, ilu_FALSE);
}

  /*ARGSUSED*/
static          ilu_cardinal
_pickle_SizeOfBytes(ilu_Call call, ilu_bytes i, ilu_cardinal l,
		    ilu_cardinal limit, ILU_ERRS((IoErrs)) * err)
{
  if ((limit > 0) && (l > limit))
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_sequenceLimit, 0);
  ILU_CLER(*err);
  return (4 + l);
}

/* ==================== string ==================== */

static void 
_pickle_OutputString(ilu_Call call, void * s, ilu_cardinal len,
		     ilu_cardinal limit,
		     ilu_cardinal expected_encoding,
		     ilu_cardinal current_encoding,
		     ILU_ERRS((IoErrs)) * err)
{
  _pickle_OutputBytes(call, (ilu_bytes) s, len, limit, err);
}

static void
_pickle_InputString(ilu_Call call, void** s, ilu_cardinal * len,
		    ilu_cardinal limit,
		    ilu_cardinal expected_encoding,
		    ilu_cardinal *current_encoding,
		    ILU_ERRS((IoErrs)) * err)
{
  *current_encoding = expected_encoding;
  InputBytes (call, (ilu_bytes *) s, len, limit, err, ilu_TRUE);
}

/*ARGSUSED*/
static ilu_cardinal 
_pickle_SizeOfString(ilu_Call call, void *i, ilu_cardinal l,
		     ilu_cardinal limit,
		     ilu_cardinal expected_encoding,
		     ilu_cardinal current_encoding,
		     ILU_ERRS((IoErrs)) * err)
{
  if ((limit > 0) && (l > limit))
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_sequenceLimit, 0);
  ILU_CLER(*err);
  return (4 + l);
}

/* ==================== fixed length vector of bytes ==================== */

static void
_pickle_OutputByteVec(ilu_Call call, ilu_bytes o, ilu_cardinal len,
		      ILU_ERRS((IoErrs)) * err)
{
  ilu_bytes buf = PICKLE_GET_BUFFER(call, len, err);
  if (ILU_ERROK(*err))
    memcpy((void *) buf, (void *) o, len);
}

static void
_pickle_InputByteVec(ilu_Call call, ilu_bytes * o, ilu_cardinal len,
		     ILU_ERRS((IoErrs)) * err)
{
  ilu_bytes buf = PICKLE_GET_BUFFER(call, len, err);
  *o = NIL;
  if (ILU_ERROK(*err) && (len > 0)) {
    *o = ilu_MallocE(len, err);
    if (ILU_ERROK(*err))
      memcpy((void *) *o, (void *) buf, len);
  }
}

static          ilu_cardinal
_pickle_SizeOfByteVec(ilu_Call call, ilu_bytes o, ilu_cardinal len,
		      ILU_ERRS((IoErrs)) * err)
{
  ILU_CLER(*err);
  return (len);
}

/* ==================== boolean ==================== */

static void 
_pickle_OutputBoolean(ilu_Call call, ilu_boolean b, ILU_ERRS((IoErrs)) * err)
{
  ilu_byte *buf = PICKLE_GET_BUFFER(call, 1, err);
  if (ILU_ERROK(*err))
    *buf = (b ? 1 : 0); 
}

static void 
_pickle_InputBoolean(ilu_Call call, ilu_boolean * b,
		     ILU_ERRS((IoErrs)) * err)
{
  ilu_byte *buf = PICKLE_GET_BUFFER(call, 1, err);
  if (ILU_ERROK(*err))
    *b = ((*buf == 0) ? ilu_FALSE : ilu_TRUE);
}

/*ARGSUSED*/
static          ilu_cardinal
_pickle_SizeOfBoolean(ilu_Call call,
		      ilu_boolean i,
		      ILU_ERRS((IoErrs)) * err)
{
  ILU_CLER(*err);
  return (1);
}

/* ==================== optional ==================== */

static void 
_pickle_OutputOptional(ilu_Call call, ilu_boolean b,
		       ilu_Type the_type, ILU_ERRS((IoErrs)) * err)
{
  ilu_byte *buf = PICKLE_GET_BUFFER(call, 1, err);
  if (ILU_ERROK(*err))
    *buf = (b ? 1 : 0); 
}

static void 
_pickle_InputOptional(ilu_Call call, ilu_boolean * b,
		      ilu_Type the_type, ILU_ERRS((IoErrs)) * err)
{
  ilu_byte *buf = PICKLE_GET_BUFFER(call, 1, err);
  if (ILU_ERROK(*err))
    *b = ((*buf == 0) ? ilu_FALSE : ilu_TRUE);
}

/*ARGSUSED*/
static          ilu_cardinal
_pickle_SizeOfOptional(ilu_Call call, ilu_boolean i,
		       ilu_Type the_type, ILU_ERRS((IoErrs)) * err)
{
  ILU_CLER(*err);
  return (1);
}

/* ==================== enumerations ==================== */

static void 
_pickle_OutputEnumCode(ilu_Call call, ilu_shortcardinal b,
		       ilu_Type the_type, ILU_ERRS((IoErrs)) * err)
{
  _pickle_OutputShortCardinal(call, b, err);
}

static void 
_pickle_InputEnumCode(ilu_Call call, ilu_shortcardinal * b,
		      ilu_Type the_type, ILU_ERRS((IoErrs)) * err)
{
  _pickle_InputShortCardinal(call, b, err);
}

/*ARGSUSED*/
static          ilu_cardinal
_pickle_SizeOfEnumCode(ilu_Call call, ilu_shortcardinal i,
		       ilu_Type the_type, ILU_ERRS((IoErrs)) * err)
{
  ILU_CLER(*err);
  return (2);
}

/* ==================== sequence ==================== */

static void
_pickle_OutputSequence(ilu_Call c, ilu_cardinal sequenceLength,
		       ilu_cardinal limit, ilu_Type the_type,
		       ILU_ERRS((IoErrs)) * err)
{
  if (limit > 0 && sequenceLength > limit)
    {
      ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_sequenceLimit, 0);
    }
  else  
    _pickle_OutputCardinal(c, sequenceLength, err);
}

static void 
_pickle_OutputSequenceMark(ilu_Call c,
			   ilu_cardinal extent,
			   ILU_ERRS((IoErrs)) * err)
{
  ILU_CLER(*err);
}

static void 
_pickle_InputSequenceMark(ilu_Call c, ilu_cardinal extent,
			  ILU_ERRS((IoErrs)) * err)
{
  ILU_CLER(*err);
}

static void
_pickle_InputSequence(ilu_Call c, ilu_cardinal * sequenceLength,
		      ilu_cardinal limit, ilu_Type the_type,
		      ILU_ERRS((IoErrs)) * err)
{
  ilu_cardinal    len = 0;
  _pickle_InputCardinal(c, &len, err);
  if (ILU_ERROK(*err))
    {
      if (limit > 0 && len > limit)
	{
	  ILU_ERR_CONS1(marshal, err, minor, ilu_mm_sequenceLimit, 0);
	  return;
	}
      else
	*sequenceLength = len;
    }
}

static void _pickle_EndSequence (ilu_Call c, ILU_ERRS((IoErrs)) * err)
{
  ILU_CLER(*err);
}

static          ilu_cardinal
_pickle_SizeOfSequence(ilu_Call c, ilu_cardinal length,
		       ilu_cardinal limit, ilu_Type the_type,
		       ILU_ERRS((IoErrs)) * err)
{
  if (limit > 0 && length > limit)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_sequenceLimit, 0);
  else  
    return (_pickle_SizeOfCardinal(c, length, err));
}

/* ==================== union ==================== */

static void 
_pickle_OutputUnion(ilu_Call c, ilu_cardinal typeIndex,
		    ilu_TypeKind disc_kind, ilu_Type the_type,
		    ILU_ERRS((IoErrs)) * err)
{
  switch (disc_kind)
    {
    case ilu_byte_tk:
    case ilu_boolean_tk:
    case ilu_shortcharacter_tk:
      _pickle_OutputByte(c, (ilu_byte)(typeIndex & 0xFF), err);
      break;
    case ilu_shortcardinal_tk:
    case ilu_shortinteger_tk:
    case ilu_character_tk:
    case ilu_enumeration_tk:
      _pickle_OutputShortCardinal (c, (ilu_shortcardinal)(typeIndex & 0xFFFF), err);
      break;
    case ilu_cardinal_tk:
    case ilu_integer_tk:
      _pickle_OutputCardinal (c, typeIndex, err);
      break;
    default:
      ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_unionDiscSize, 0);
    };
}

static void 
_pickle_InputUnion(ilu_Call call, ilu_cardinal * typeIndex,
		   ilu_TypeKind disc_kind, ilu_Type the_type,
		   ILU_ERRS((IoErrs)) * err)
{
  ilu_byte b;
  ilu_shortcardinal sc;

  switch (disc_kind)
    {
    case ilu_byte_tk:
    case ilu_boolean_tk:
    case ilu_shortcharacter_tk:
      _pickle_InputByte (call, &b, err);  *typeIndex = b;
      break;
    case ilu_shortcardinal_tk:
    case ilu_shortinteger_tk:
    case ilu_character_tk:
    case ilu_enumeration_tk:
      _pickle_InputShortCardinal (call, &sc, err);  *typeIndex = sc;
      break;
    case ilu_cardinal_tk:
    case ilu_integer_tk:
      _pickle_InputCardinal(call, typeIndex, err);
      break;
    default:
      ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_unionDiscSize, 0);
    };
}

static void _pickle_EndUnion (ilu_Call c, ILU_ERRS((IoErrs)) * err)
{
  ILU_CLER(*err);
}

static ilu_cardinal 
_pickle_SizeOfUnion(ilu_Call c, ilu_cardinal typeIndex,
		    ilu_TypeKind disc_kind, ilu_Type the_type,
		    ILU_ERRS((IoErrs)) * err)
{
  ILU_CLER(*err);
  switch (disc_kind)
    {
    case ilu_byte_tk:
    case ilu_boolean_tk:
    case ilu_shortcharacter_tk:
      return 1;
    case ilu_shortcardinal_tk:
    case ilu_shortinteger_tk:
    case ilu_character_tk:
    case ilu_enumeration_tk:
      return 2;
    case ilu_cardinal_tk:
    case ilu_integer_tk:
      return 4;
    default:
      return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_unionDiscSize, 0);
    }
}

/* ==================== array ==================== */

static void 
_pickle_OutputArray(ilu_Call c, ilu_cardinal len,
		    ilu_Type the_type, ILU_ERRS((IoErrs)) * err)
{
  ILU_CLER(*err);
}

static void _pickle_InputArray(ilu_Call c, ilu_Type the_type,
			       ILU_ERRS((IoErrs)) * err)
{
  ILU_CLER(*err);
}

static void _pickle_EndArray (ilu_Call c, ILU_ERRS((IoErrs)) * err)
{
  ILU_CLER(*err);
}

static ilu_cardinal _pickle_SizeOfArray (ilu_Call c, ilu_cardinal len,
					 ilu_Type the_type,
					 ILU_ERRS((IoErrs)) * err)
{
  ILU_CLER(*err);
  return(0);
}

/* ==================== record ==================== */

static void 
_pickle_OutputRecord(ilu_Call c, ilu_Type the_type,
		     ILU_ERRS((IoErrs)) * err)
{
  ILU_CLER(*err);
}

static void _pickle_InputRecord(ilu_Call c, ilu_Type the_type,
				ILU_ERRS((IoErrs)) * err)
{
  ILU_CLER(*err);
}

static void _pickle_EndRecord (ilu_Call c, ILU_ERRS((IoErrs)) * err)
{
  ILU_CLER(*err);
}

static          ilu_cardinal
_pickle_SizeOfRecord(ilu_Call c, ilu_Type the_type,
		     ILU_ERRS((IoErrs)) * err)
{
  ILU_CLER(*err);
  return (0);
}

/* ==================== object ==================== */

static ilu_boolean 
  _pickle_OutputObjectID(ilu_Call call, ilu_Object h,
			 ilu_boolean discriminator_p,
			 ilu_Class static_type,
			 ILU_ERRS((IoErrs)) * err)
{
  unsigned typekind = PICKLE_TYPEKIND(PICKLE_BUF(call)[0]);
  ilu_string type_string;

  if (typekind == OBJECT_TYPEKIND) {
    if (h == NIL)
      type_string = OBJECT_TYPESTRING(call);
    else {
      type_string = _ilu_MSTIDToStringifiedDAG((h == NIL) ? class_unique_id(static_type) :
					       ((object_mstid(h) == NIL) ?
						class_unique_id(object_class(h)) : object_mstid(h)));
      ilu_free(OBJECT_TYPESTRING(call));
    }
    _pickle_OutputByteVec (call, (ilu_byte *) type_string, strlen(type_string) + 1, err);
    ilu_free(type_string);
    OBJECT_TYPESTRING(call) = NIL;
    if (ILU_ERRNOK(*err)) return ilu_FALSE;
  }
  return _ilu_OutputObjectID (call, h, discriminator_p, static_type, err);
}

/* ==================== error trap ==================== */

static void catchErr (void) {

  /* if some protocol routine is called that should never be called,
     this routine is invoked, to trap to the error routine. */

  _ilu_Assert (ilu_FALSE, "invalid pickling protocol method invoked");
}

/* ==================== the connection and protocol value ==================== */

static struct _ilu_Protocol_s pickleProtocol = {

  /* A protocol is never changed or freed once created, so these fields
   * are readonly.  The locking comments refer to invoc'ns of the methods.
   */

  ilu_FALSE,	/* ilu_boolean	pr_concurrent_requests; -- N/A */

  ilu_TRUE,	/* ilu_boolean	pr_sizing_required; -- for buffer space allocation */

  ilu_FALSE,	/* needs boundaried transports */

  ilu_TRUE,	/* needs reliable transports */

  /*L2, Main unconstrained*/
  /*L1_sup < prmu (prmu protects class<->prog&ver registry)*/
  (ilu_string (*)(ilu_refany, ilu_Object)) catchErr,		/* pr_form_handle */

  /*L1, L2, Main unconstrained*/

  (ilu_refany (*)(ilu_ProtocolInfo, ILU_ERRS((no_memory)) *)) catchErr,	/* pr_create_data_block */

  (void (*)(ilu_refany)) catchErr,			/* pr_free_data_block */

  (void (*)(ilu_refany,ilu_ConnShutdownReason,ilu_Error *)) catchErr,		/* pr_conn_closing */

  /*L1, L2 unconstrained*/
  (ilu_boolean(*)(ilu_Call,				/* pr_init_call */
		  ILU_ERRS((IoErrs)) *)) catchErr,
  
  /*Main Invariant holds; L2 >= {call's conn's callmu, iomu}*/

  (ilu_boolean(*)(ilu_Call,				/* pr_start_request*/
		  ilu_cardinal,
		  ILU_ERRS((IoErrs))*)) catchErr,

  (ilu_boolean(*) (ilu_Call,				/* pr_discard_output */
		   ILU_ERRS((IoErrs)) *)) catchErr,

  (ilu_boolean(*) (ilu_Call,				/* pr_finish_request */
		   ilu_Message *,
		   ilu_boolean,
		   ILU_ERRS((IoErrs)) *)) catchErr,

  (ilu_ReadHeaderResultCode(*) (ilu_Call,		/* pr_read_header */
				ilu_PacketType *,
				ilu_cardinal *,
				ilu_ConnShutdownReason *,
				ilu_cardinal *,
				ILU_ERRS((IoErrs)) *)) catchErr,

  (ilu_refany(*) (ilu_Call,				/* pr_delay_interp */
		  ILU_ERRS((IoErrs)) *)) catchErr,

  (void(*) (ilu_Call,					/* pr_resume_interp */
	    ilu_refany)) catchErr,
  
  (ilu_boolean(*) (ilu_Connection conn,
		   ilu_refany,				/* pr_abandon_delayed_interp */
		   ILU_ERRS((internal)) *)) catchErr,

  (ilu_ProtocolException(*) (ilu_Call,			/* pr_interpret_reply */
			     ilu_cardinal *,
			     ILU_ERRS((IoErrs)) *)) catchErr,

  (ilu_boolean(*) (ilu_Call,				/* pr_discard_input */
		   ILU_ERRS((internal)) * /* err */)) catchErr,
  
  (void (*) (ilu_Call,					/* pr_reply_read */
	     ILU_ERRS((IoErrs)) *)) catchErr,

  (ilu_boolean(*) (ilu_Call /* call */,			/* pr_interpret_request */
		   ILU_ERRS((IoErrs)) * /* err */)) catchErr,

  (void (*) (ilu_Call,					/* pr_request_read */
	     ILU_ERRS((IoErrs)) *)) catchErr,

  (ilu_cardinal(*) (ilu_Call call,	/* pr_begin_sizing_reply */
		    ilu_boolean exns_possible,
		    ILU_ERRS((IoErrs)) * err)) catchErr,
  (ilu_boolean(*) (ilu_Call call,	/* pr_begin_reply */
		   ilu_boolean exceptions_possible,
		   ilu_cardinal reply_size,
		   ILU_ERRS((IoErrs)) * err)) catchErr,
  (ilu_boolean(*) (ilu_Call call,	/* pr_finish_reply */
		   ilu_boolean push,
		   ILU_ERRS((IoErrs)) * err)) catchErr,
  (ilu_cardinal(*) (ilu_Call call,	/* pr_begin_sizing_exn */
		    ilu_cardinal eindex,
		    ilu_ProtocolException sys_ex_index,
		    ILU_ERRS((IoErrs)) * err)) catchErr,
  (ilu_boolean(*) (ilu_Call call,	/* pr_begin_exception */
		   ilu_cardinal exception_code,
		   ilu_ProtocolException sys_ex_index,
		   ilu_cardinal reply_size,
		   ILU_ERRS((IoErrs)) * err)) catchErr,
  (ilu_boolean(*) (ilu_Call call,	/* pr_finish_exception */
		   ilu_boolean push,
		   ILU_ERRS((IoErrs)) * err)) catchErr,
  (ilu_boolean(*) (ilu_Call call,	/* pr_prefinish_call */
		   ILU_ERRS((IoErrs)) * err)) catchErr,
  (ilu_boolean(*) (ilu_Call call,	/* pr_finish_call */
		   ILU_ERRS((IoErrs)) * err)) catchErr,

  /* [Un]Marshalling routines */
  /*L1, L2 unconstrained for sizing, end*/
  /*Main holds, L2 >= {call's connection's callmu, iomu} for output*/
  /*Main holds, L2 >= {call's connection's callmu, iomu} for input*/

  /* optional */
  _pickle_OutputOptional,		/* pr_output_optional */
  _pickle_InputOptional,			/* pr_input_optional */
  _pickle_SizeOfOptional,		/* pr_size_of_optional */

  /* integer */
  (void            (*) (ilu_Call call,	/* pr_output_integer */
			ilu_integer i,
			ILU_ERRS((IoErrs)) * err)) _pickle_OutputCardinal,
  (void            (*) (ilu_Call call,	/* pr_input_integer */
			ilu_integer * i,
			ILU_ERRS((IoErrs)) * err)) _pickle_InputCardinal,
  (ilu_cardinal(*) (ilu_Call call,	/* pr_size_of_integer */
		    ilu_integer i,
		    ILU_ERRS((IoErrs)) * err)) _pickle_SizeOfCardinal,

  /* cardinal */
  _pickle_OutputCardinal,	/* pr_output_cardinal */
  _pickle_InputCardinal,	/* pr_input_cardinal */
  _pickle_SizeOfCardinal,	/* pr_size_of_cardinal */

  /* short integer */
  (void            (*) (ilu_Call call,	/* pr_output_short_integer */
			ilu_shortinteger i,
			ILU_ERRS((IoErrs)) * err)) _pickle_OutputShortCardinal,
  (void            (*) (ilu_Call call,	/* pr_input_short_integer */
			ilu_shortinteger * i,
			ILU_ERRS((IoErrs)) * err)) _pickle_InputShortCardinal,
  (ilu_cardinal(*) (ilu_Call call,	/* pr_size_of_short_integer */
		    ilu_shortinteger i,
		    ILU_ERRS((IoErrs)) * err)) _pickle_SizeOfShortCardinal,

  /* short cardinal */
  _pickle_OutputShortCardinal,	/* pr_output_shortcardinal */
  _pickle_InputShortCardinal,	/* pr_input_shortcardinal */
  _pickle_SizeOfShortCardinal,	/* pr_size_of_shortcardinal */

  /* real */
  _pickle_OutputReal,		/* pr_output_real */
  _pickle_InputReal,		/* pr_input_real */
  _pickle_SizeOfReal,		/* pr_size_of_real */

  /* short real */
  _pickle_OutputShortReal,	/* pr_output_shortreal */
  _pickle_InputShortReal,	/* pr_input_shortreal */
  _pickle_SizeOfShortReal,	/* pr_size_of_shortreal */

  /* long integer */
  (void            (*) (ilu_Call call,	/* pr_output_longinteger */
			ilu_longinteger i,
			ILU_ERRS((IoErrs)) * err)) _pickle_OutputLongCardinal,
  (void            (*) (ilu_Call call,	/* pr_input_longinteger */
			ilu_longinteger * i,
			ILU_ERRS((IoErrs)) * err)) _pickle_InputLongCardinal,
  (ilu_cardinal(*) (ilu_Call call,	/* pr_size_of_longinteger */
		    ilu_longinteger i,
		    ILU_ERRS((IoErrs)) * err)) _pickle_SizeOfLongCardinal,

  /* long cardinal */
  _pickle_OutputLongCardinal,		/* pr_output_longcardinal */
  _pickle_InputLongCardinal,		/* pr_input_longcardinal */
  _pickle_SizeOfLongCardinal,		/* pr_size_of_longcardinal */


  /* long real */
  _pickle_OutputLongReal,		/* pr_output_longreal */
  _pickle_InputLongReal,		/* pr_input_longreal */
  _pickle_SizeOfLongReal,		/* pr_size_of_longreal */

  /* byte */
  _pickle_OutputByte,			/* pr_output_byte */
  _pickle_InputByte,			/* pr_input_byte */
  _pickle_SizeOfByte,			/* pr_size_of_byte */

  /* boolean */
  _pickle_OutputBoolean,		/* pr_output_boolean */
  _pickle_InputBoolean,			/* pr_input_boolean */
  _pickle_SizeOfBoolean,		/* pr_size_of_boolean */

  /* this is a Unicode character */
  (void            (*) (ilu_Call call,	/* pr_output_character */
			ilu_character i,
			ILU_ERRS((IoErrs)) * err)) _pickle_OutputShortCardinal,
  (void            (*) (ilu_Call call,	/* pr_input_character */
			ilu_character * i,
			ILU_ERRS((IoErrs)) * err)) _pickle_InputShortCardinal,
  (ilu_cardinal(*) (ilu_Call call,	/* pr_size_of_character */
		    ilu_character i,
		    ILU_ERRS((IoErrs)) * err)) _pickle_SizeOfShortCardinal,

  /* this is an ISO Latin-1 character using the ISO 8859-1 encoding */
  (void(*) (ilu_Call call,	/* pr_output_shortchar */
	    ilu_shortcharacter i,
	    ILU_ERRS((IoErrs)) * err)) _pickle_OutputByte,
  (void(*) (ilu_Call call,	/* pr_input_shortchar */
	    ilu_shortcharacter * i,
	    ILU_ERRS((IoErrs)) * err)) _pickle_InputByte,
  (ilu_cardinal(*) (ilu_Call call,	/* pr_size_of_shortchar */
		    ilu_shortcharacter i,
		    ILU_ERRS((IoErrs)) * err)) _pickle_SizeOfByte,

  /* enumcode */
  _pickle_OutputEnumCode,
  _pickle_InputEnumCode,
  _pickle_SizeOfEnumCode,

  /* string */
  _pickle_OutputString,			/* pr_output_string */
  _pickle_InputString,			/* pr_input_string */
  _pickle_SizeOfString,			/* pr_size_of_string */

  /* array of char */
  (void(*) (ilu_Call call,	/* pr_output_stringvec */
	    ilu_string s,
	    ilu_cardinal length,
	    ILU_ERRS((IoErrs)) * err)) _pickle_OutputByteVec,
  (void(*) (ilu_Call call,	/* pr_input_stringvec */
	    ilu_string * s,
	    ilu_cardinal length,
	    ILU_ERRS((IoErrs)) * err)) _pickle_InputByteVec,
  (ilu_cardinal(*) (ilu_Call call,	/* pr_size_of_stringvec */
		    ilu_string s,
		    ilu_cardinal length,
		    ILU_ERRS((IoErrs)) * err)) _pickle_SizeOfByteVec,

  /* wide string */
  _ilu_OutputWString,			/* pr_output_wstring */
  _ilu_InputWString,			/* pr_input_wstring */
  _ilu_SizeOfWString,			/* pr_size_of_wstring */

  /* wide string vector */
  _ilu_OutputWStringVec,		/* pr_output_wstringvec */
  _ilu_InputWStringVec,			/* pr_input_wstringvec */
  _ilu_SizeOfWStringVec,		/* pr_size_of_wstringvec */

  /* fixed-length vector of byte */
  _pickle_OutputByteVec,		/* pr_output_opaque */
  _pickle_InputByteVec,			/* pr_input_opaque */
  _pickle_SizeOfByteVec,		/* pr_size_of_opaque */

  /* sequence of bytes */
  _pickle_OutputBytes,			/* pr_output_bytes */
  _pickle_InputBytes,			/* pr_input_bytes */
  _pickle_SizeOfBytes,			/* pr_size_of_bytes */

  /* objects */
  _pickle_OutputObjectID,		/* pr_output_object_id */
  _ilu_InputObjectID,			/* pr_input_object_id */
  _ilu_SizeOfObjectID,			/* pr_size_of_object_id */

#ifdef ILU_HTTPNG_OBJECTS

  NULLFN,		/* pr_begin_output_object */
  NULLFN,		/* pr_begin_output_state */
  NULLFN,		/* pr_finish_output_state */
  NULLFN,		/* pr_finish_output_object */

  NULLFN,		/* pr_begin_size_of_object */
  NULLFN,		/* pr_begin_size_of_state */
  NULLFN,		/* pr_finish_size_of_state */
  NULLFN,		/* pr_finish_size_of_object */

  NULLFN,		/* pr_begin_input_object */
  NULLFN,		/* pr_begin_input_state */
  NULLFN,		/* pr_skip_input_state */
  NULLFN,		/* pr_finish_input_state */
  NULLFN,		/* pr_finish_input_object */

#endif /* def ILU_HTTPNG_OBJECTS */

  /* sequences */
  _pickle_OutputSequence,		/* pr_output_sequence */
  _pickle_OutputSequenceMark,		/* pr_output_sequence_mark */
  _pickle_InputSequence,		/* pr_input_sequence */
  _pickle_InputSequenceMark,		/* pr_input_sequence_mark */
  _pickle_EndSequence,			/* pr_end_sequence */
  _pickle_SizeOfSequence,		/* pr_size_of_sequence */

  /* array */
  _pickle_OutputArray,			/* pr_output_array */
  _pickle_InputArray,			/* pr_input_array */
  _pickle_EndArray,			/* pr_end_array */
  _pickle_SizeOfArray,			/* pr_size_of_array */

  /* record */
  _pickle_OutputRecord,			/* pr_output_record */
  _pickle_InputRecord,			/* pr_input_record */
  _pickle_EndRecord,			/* pr_end_record */
  _pickle_SizeOfRecord,			/* pr_size_of_record */

  /* unions */
  _pickle_OutputUnion,			/* pr_output_union */
  _pickle_InputUnion,			/* pr_input_union */
  _pickle_EndUnion,			/* pr_end_union */
  _pickle_SizeOfUnion,			/* pr_size_of_union */

  /* pickle */
  _ilu_OutputPickle,			/* pr_output_pickle */
  _ilu_InputPickle,			/* pr_input_pickle */
  _ilu_SizeOfPickle,			/* pr_size_of_pickle */

#ifdef ILU_FIXED_POINT_SUPPORT

  /* fixedpoint */
  _ilu_OutputFixedpoint,		/* pr_output_fixedpoint */
  _ilu_InputFixedpoint,			/* pr_input_fixedpoint */
  _ilu_SizeOfFixedpoint,		/* pr_size_of_fixedpoint */

#endif /* def ILU_FIXED_POINT_SUPPORT */

#ifdef ILU_REFERENCE_TYPES

  /* reference types */
  _ilu_SizeOfReference,			/* pr_size_of_reference */
  _ilu_OutputReference,			/* pr_output_reference */
  _ilu_InputReference,			/* pr_input_reference */
  _ilu_EndInputReference		/* pr_end_input_reference */

#endif /* def ILU_REFERENCE_TYPES */

  };

struct _ilu_Connection_s _ilu_pickle2_Format = {
  NIL,			/* co_mucall */
  NIL,			/* co_waiting */
  ilu_FALSE,		/* co_ioing */
  ilu_FALSE,		/* co_closed */
  ilu_FALSE,		/* co_closing */
  ilu_FALSE,		/* co_pending */
  ilu_FALSE,		/* co_lastSNgood */
  ilu_FALSE,		/* co_lsrCares */
  ilu_FALSE,		/* co_doomed */
  ilu_FALSE,		/* co_pushme */
  ilu_FALSE,		/* co_pushAlarmSet */
  ilu_FALSE,		/* co_callmuBorrowable */
  ilu_FALSE,		/* co_lastWorking */
  &pickleProtocol,	/* co_protocol */
  { NIL },		/* co_tinfo */
  "pickle2",		/* co_pinfo */
  NIL,			/* co_transport */
  NIL,			/* co_port */
  NIL,			/* co_auth_info */
  { 0 },		/* co_conn_identity */
  NIL,			/* co_server */
  NIL,			/* co_protocol_data */
  NIL,			/* co_pipeline */
  0,			/* co_batchCount */
  {0, 0},		/* co_pushTime */
  NIL,			/* co_pushAlarm */
  NIL,			/* co_replies */
  NIL,			/* co_cc */
  NIL,			/* co_serialer */
  1,			/* co_next_sn */
  0,			/* co_last_sn */
  0,			/* co_nOuts */
  0,			/* co_nWaits */
  0,			/* co_nCalls */
  0,			/* co_holds */
  { { NIL, NIL } },	/* co_links */
  {0}			/* co_tih */
};  

/* ==================== routines to begin and end pickle work ==================== */

ilu_boolean
  _ilu_pickle2_StartPickle (ilu_Call_s *call,
			    ilu_Type prefix_type,
			    ilu_Error *err)
{
  static ilu_Message nomsg = {NIL, 0};
  ILU_CLER(*err);
  call->ca_SN = 0;
  call->ca_server = ILU_NIL;
  call->ca_intro_type = NIL;
  call->ca_method = NIL;
  call->ca_connection = &_ilu_pickle2_Format;
  call->ca_private = NIL;
  call->ca_callee = NIL;
  call->ca_caller = NIL;
  call->ca_irq = ilu_FALSE;
  call->ca_prbit1 = ilu_FALSE;
  call->ca_prdata1 = 0;
  call->ca_prdata2 = ilu_MallocE(sizeof(struct pickle2_data_s), err);
  if (ILU_ERRNOK(*err)) return ilu_FALSE;
  PREFIX_TYPE(call) = prefix_type;
  OBJECT_TYPESTRING(call) = NIL;
  call->ca_msg = nomsg;
  call->ca_prTrans = NIL;
  call->ca_incoming = ilu_FALSE;
  call->ca_ios = ilu_ciosNone;
  call->ca_ms = ilu_cmsNo;
  call->ca_pe = ilu_ProtocolException_Success;
  call->ca_reqs_enabled = ilu_FALSE;
  return ilu_TRUE;
}

typedef struct _TypeKind_s {
  const char *	ilu_type_id;
  ilu_byte	ilu_pickle_typekind;
} TypeKind_s;

static TypeKind_s TypeKinds[] = {
  { &ilu_TypeID_ilu_byte[0], (ilu_byte) ilu_byte_tk },
  { &ilu_TypeID_ilu_boolean[0], (ilu_byte) ilu_boolean_tk },
  { &ilu_TypeID_ilu_character[0], (ilu_byte) ilu_character_tk },
  { &ilu_TypeID_ilu_shortcharacter[0], (ilu_byte) ilu_shortcharacter_tk },
  { &ilu_TypeID_ilu_shortinteger[0], (ilu_byte) ilu_shortinteger_tk },
  { &ilu_TypeID_ilu_integer[0], (ilu_byte) ilu_integer_tk },
  { &ilu_TypeID_ilu_longinteger[0], (ilu_byte) ilu_longinteger_tk },
  { &ilu_TypeID_ilu_shortcardinal[0], (ilu_byte) ilu_shortcardinal_tk },
  { &ilu_TypeID_ilu_cardinal[0], (ilu_byte) ilu_cardinal_tk },
  { &ilu_TypeID_ilu_longcardinal[0], (ilu_byte) ilu_longcardinal_tk },
  { &ilu_TypeID_ilu_real[0], (ilu_byte) ilu_real_tk },
  { &ilu_TypeID_ilu_shortreal[0], (ilu_byte) ilu_shortreal_tk },
  { &ilu_TypeID_ilu_longreal[0], (ilu_byte) ilu_longreal_tk },
  { &ilu_TypeID_ilu_CString[0], (ilu_byte) ILU_CSTRING_TYPEKIND }
};
static ilu_boolean TypeKindsSorted = ilu_FALSE;
#define TypeKindsSize (sizeof(TypeKinds)/sizeof(TypeKind_s))

static int CompareTypeKindEntries (const void *p1, const void *p2)
{
  return (strcmp(((TypeKind_s *) p1)->ilu_type_id, ((TypeKind_s *) p2)->ilu_type_id));
}

static int CompareTypeKindTypeIDs (const void *p1, const void *p2)
{
  return (strcmp((ilu_string) p1, ((TypeKind_s *) p2)->ilu_type_id));
}

static ilu_boolean ExtensibleRecordType (ilu_string type_id)
{
#ifdef ADD_TYPE_REGISTRATION_SUPPORT
  ilu_Type t;
  ilu_Error lerr;
  t = ilu_FindTypeByUID(type_id, &lerr);
  if (ILU_ERRNOK(lerr)) {
    ILU_HANDLED(lerr);
    return ilu_FALSE;
  }
  if (t) {
    return ((t->kind == ilu_record_tk) &&
	    (t->desc.Record.extensible ||
	     (t->desc.Record.supertype != NIL)));
  } else {
    return ilu_FALSE;
  }
#else
  return ilu_FALSE;
#endif /* def ADD_TYPE_REGISTRATION_SUPPORT */
}

#ifdef ADD_TYPE_REGISTRATION_SUPPORT
static ilu_string _ilu_ExtensibleRecordTypesString (ilu_string type_id, ilu_Error *err)
{
  ilu_Type t;
  ilu_CharBuf cb;
  char *s;

  t = ilu_FindTypeByUID(type_id, err);
  if (ILU_ERRNOK(*err)) return NIL;
  s = _ilu_EncodeBuffer(type_id, strlen(type_id), err);
  if (ILU_ERRNOK(*err)) return NIL;
  cb = ilu_CharBufFromChars(s, strlen(s), err);
  ilu_free(s);
  if (ILU_ERRNOK(*err)) goto free1;
  for (t = t->desc.Record.supertype;  t != NIL;  t = t->desc.Record.supertype) {
    if (!ilu_CharBufAppend(&cb, ";", 1, err))
      goto free1;
    s = _ilu_EncodeBuffer(type_uid(t), strlen(type_uid(t)), err);
    if (ILU_ERRNOK(*err)) goto free1;
    ilu_CharBufAppend(&cb, s, strlen(s), err);
    ilu_free(s);
    if (ILU_ERRNOK(*err)) goto free1;
  }
  if (!ilu_CharBufAppend(&cb, "", 1, err))
    goto free1;
  return cb.icb_base;

 free1:
  ilu_free(cb.icb_base);
  return NIL;
}
#endif

static ilu_string _ilu_ExtensibleRecordType (ilu_string bytes, ilu_Error *err)
{
#ifdef ADD_TYPE_REGISTRATION_SUPPORT
  ilu_string p, q, uid;
  ilu_cardinal len;
  ilu_Type t;
  for (p = bytes;  p != NIL && *p != 0;  p = strchr(p, ';')) {
    if (*p == ';')
      p++;
    q = strchr(p, ';');
    if (q == NIL)
      q = p + strlen(p);
    uid = _ilu_DecodeBuffer (p, q - p, &len, err);
    if (ILU_ERRNOK(*err)) return NIL;
    t = ilu_FindTypeByUID(uid, err);
    ilu_free(uid);
    if (ILU_ERRNOK(*err)) { return NIL; };
    if (t != NIL) return type_uid(t);
  }
#endif
  return (ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_invalid_variant_type, NIL));
}


static ilu_byte TypeKindCode (ilu_string id, ilu_boolean *needs_explicit_type_id, ilu_Class *objtype, ilu_boolean *extensible_record_type)
{
  TypeKind_s *internal_id;
#ifdef ADD_TYPE_REGISTRATION_SUPPORT
  ilu_Type the_type;
  ilu_Error lerr;
#endif

  if (!TypeKindsSorted) {
    qsort (TypeKinds, TypeKindsSize, sizeof(TypeKind_s), CompareTypeKindEntries);
    TypeKindsSorted = ilu_TRUE;
  }

  *extensible_record_type = ilu_FALSE;
  internal_id = (TypeKind_s *)
    bsearch ((void *) id, TypeKinds, TypeKindsSize,
	     sizeof(TypeKind_s), CompareTypeKindTypeIDs);
  if (internal_id != NIL) {
    *needs_explicit_type_id = ilu_FALSE;
    *objtype = (ilu_Class)ILU_NIL;
    return (SHIFTED_VERSION | (internal_id->ilu_pickle_typekind));
  } else {
    *needs_explicit_type_id = ilu_TRUE;
    if ((*objtype = ilu_FindClassFromID(id)) != NIL) {
      return (SHIFTED_VERSION | OBJECT_TYPEKIND);
    } else if (ExtensibleRecordType(id)) {
      *extensible_record_type = ilu_TRUE;
      return (SHIFTED_VERSION | EXTENSIBLE_RECORD_TYPEKIND);
    } else {
#ifdef ADD_TYPE_REGISTRATION_SUPPORT
      the_type = ilu_FindTypeByUID(id, &lerr);
      ILU_HANDLED(lerr);
      if (the_type != NIL)
	return (SHIFTED_VERSION | (ilu_byte) type_kind(the_type));
      else
#endif
	return (SHIFTED_VERSION | HAS_TYPEID_TYPEKIND);
    }
  }
}

ilu_boolean
  _ilu_pickle2_WritePickle (ilu_Call call,
			    ilu_cardinal argSize,	/* hint; pickle will resize if necessary */
			    ilu_string type_id,
			    ilu_Error *err)
{
  ilu_boolean	needs_explicit_type_id;
  ilu_Class	objtype;
  ilu_byte	version_typekind;
  ilu_Type	t2;
  ilu_string	type_string = NIL;
  ilu_boolean	extensible_record_type;
  ilu_boolean	type_string_malloced = ilu_FALSE;

  version_typekind = TypeKindCode(type_id, &needs_explicit_type_id, &objtype, &extensible_record_type);
  ILU_CLER(*err);
  if ((PREFIX_TYPE(call) != NIL) &&
      ((t2 = ilu_FindTypeByUID(type_id, err)) != ILU_NIL) &&
      ILU_ERROK(*err) &&
      (!ilu_CheckSubtype(PREFIX_TYPE(call), t2))) {
    return (ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_prefix_type_violation, ilu_FALSE));
  } else if (ILU_ERRNOK(*err))
    return ilu_FALSE;

  if (needs_explicit_type_id) {
    if (objtype != NIL) {
      type_string = _ilu_ClassToString(objtype);
      type_string_malloced = ilu_TRUE;
    }
#ifdef ADD_TYPE_REGISTRATION_SUPPORT
    else if (extensible_record_type) {
      type_string = _ilu_ExtensibleRecordTypesString (type_id, err);
      type_string_malloced = ilu_TRUE;
      if (ILU_ERRNOK(*err)) return ilu_FALSE;
    }
#endif
    else {
      type_string = type_id;
      type_string_malloced = ilu_FALSE;
    }
  };

  PICKLE_LEN(call) = argSize + 1 + ((needs_explicit_type_id && (type_string != NIL)) ? strlen(type_string) + 1 : 0);
  PICKLE_BUF(call) = ilu_MallocE(PICKLE_LEN(call), err);
  if (ILU_ERRNOK(*err)) return ilu_FALSE;
  PICKLE_NEXT(call) = 0;
  
  _pickle_OutputByte (call, version_typekind, err);
  if (needs_explicit_type_id && (type_string != NIL) && (objtype == NIL)) {
    if (ILU_ERRNOK(*err)) goto errout;
    _pickle_OutputByteVec (call, (ilu_byte *) type_string, strlen(type_string) + 1, err);
    if (type_string_malloced)
      ilu_free(type_string);
    type_string = NIL;
  } else if (objtype != NIL) {
    OBJECT_TYPESTRING(call) = type_string;
  }
  if (ILU_ERRNOK(*err)) goto errout;
  else return ilu_TRUE;

 errout:
  if (type_string && type_string_malloced) ilu_free(type_string);
  ilu_free(PICKLE_BUF(call));
  return ilu_FALSE;
}

ilu_string
  _ilu_pickle2_PickleType (ilu_Pickle pickle,
			   ilu_Error *err)
{
  ilu_byte b = pickle.pi_bytes[0];
  unsigned typekind = PICKLE_TYPEKIND(pickle.pi_bytes[0]);
  ilu_Class cl;

  if (PICKLE_VERSION(b) != CURRENT_VERSION)
    return (ILU_ERR_CONS1(marshal, err, minor, ilu_mm_versionMismatch, NIL));
  ILU_CLER(*err);
  if (typekind == EXTENSIBLE_RECORD_TYPEKIND) {
    return _ilu_ExtensibleRecordType((ilu_string) &pickle.pi_bytes[1], err);
  } else if (typekind == OBJECT_TYPEKIND) {
    cl = _ilu_StringToClass((ilu_string) &pickle.pi_bytes[1]);
    if (cl != NIL)
      return cl->cl_unique_id;
    else
      return _ilu_rootClass->cl_unique_id;
  } else {
    int i;
    for (i = 0;  i < TypeKindsSize;  i++) {
      if (typekind == TypeKinds[i].ilu_pickle_typekind) {
	return (ilu_string) TypeKinds[i].ilu_type_id;
      }
    }
    /* not an object, not an extensible record, not a built-in type, so
       must be a constructed type not specially handled...  Return the type ID. */
    return (ilu_string) &pickle.pi_bytes[1];
  }
}

static void
  GetExtensibleRecordTypes (ilu_string typestring,
			    ilu_string **types,
			    ilu_cardinal *ntypes,
			    ilu_Error *err)
{
  ilu_string p, q, ts;
  ilu_cardinal count, i, len;
  ilu_string *tps;

  /* first, count the number of types in the typestring known
     in this space... */
  for (p = typestring, count = 1;  p != NIL && *p != 0;  p = strchr(p, ';')) {
    if (*p == ';')
      { p++; count++; };
  }
  tps = (ilu_string *) ilu_MallocE(count * sizeof(ilu_string), err);
  if (ILU_ERRNOK(*err)) return;
  memset((void *) tps, 0, count * sizeof(ilu_string));
  for (p = typestring, i = 0;  p != NIL && *p != 0;  p = strchr(p, ';'), i += 1) {
    if (*p == ';')
      p++;
    q = strchr(p, ';');
    if (q == NIL)
      q = p + strlen(p);
    ts = _ilu_DecodeBuffer (p, q - p, &len, err);
    if (ILU_ERRNOK(*err)) goto free1;
    tps[i] = ts;
  }
  *types = tps;
  *ntypes = count;
  return;

 free1:
  for (i = 0;   i < count;  i++)
    if (tps[i] != NIL) ilu_free(tps[i]);
  ilu_free(tps);
}

static void
  GetObjectTypes (ilu_string typestring,
		  ilu_string ** types,
		  ilu_cardinal *ntypes,
		  ilu_Error *err)
{
  ilu_string p, q, ts;
  ilu_cardinal count, i, len;
  ilu_string *tps;
  static char break_chars[] = ",()";

  /* first, count the number of types in the typestring known
     in this space... */
  for (p = typestring, count = 0;  p != NIL && *p != 0;  count += 1, p = strpbrk(p, break_chars)) {
    while ((*p != 0) && (strchr(break_chars, *p) != NIL))
      p++;
    if (*p == 0)
      break;
  }
  tps = (ilu_string *) ilu_MallocE(count * sizeof(ilu_string), err);
  if (ILU_ERRNOK(*err)) return;
  memset((void *) tps, 0, count * sizeof(ilu_string));
  for (p = typestring, i = 0;  p != NIL && *p != 0;  p = strpbrk(p, break_chars), i += 1) {
    while ((*p != 0) && (strchr(break_chars, *p) != NIL))
      p++;
    if (*p == 0)
      break;
    q = strpbrk(p, break_chars);
    if (q == NIL)
      q = p + strlen(p);
    ts = _ilu_DecodeBuffer (p, q - p, &len, err);
    if (ILU_ERRNOK(*err)) goto free1;
    tps[i] = ts;
  }
  *types = tps;
  *ntypes = count;
  return;

 free1:
  for (i = 0;   i < count;  i++)
    if (tps[i] != NIL) ilu_free(tps[i]);
  ilu_free(tps);
}

ilu_boolean
  _ilu_pickle2_PickleTypes (ilu_Pickle pickle,
			    ilu_string **types_out,
			    ilu_cardinal *types_count_out,
			    ilu_Error *err)
{
  ilu_byte b = pickle.pi_bytes[0];
  unsigned typekind = PICKLE_TYPEKIND(pickle.pi_bytes[0]);
  ilu_string known_type;
  ilu_string *types;

  if (PICKLE_VERSION(b) != CURRENT_VERSION)
    return (ILU_ERR_CONS1(marshal, err, minor, ilu_mm_versionMismatch, ilu_FALSE));
  ILU_CLER(*err);

  if (typekind == EXTENSIBLE_RECORD_TYPEKIND) {
    GetExtensibleRecordTypes ((ilu_string) &pickle.pi_bytes[1], types_out, types_count_out, err);
    return ILU_ERROK(*err);

  } else if (typekind == OBJECT_TYPEKIND) {
    GetObjectTypes ((ilu_string) &pickle.pi_bytes[1], types_out, types_count_out, err);
    return ILU_ERROK(*err);

  } else {
    int i;
    for (i = 0;  i < TypeKindsSize;  i++) {
      if (typekind == TypeKinds[i].ilu_pickle_typekind) {
	types = (ilu_string *) ilu_MallocE(sizeof(ilu_string) * 1, err);
	if (ILU_ERRNOK(*err)) return ilu_FALSE;
	types[0] = ilu_StrdupE((ilu_string) TypeKinds[i].ilu_type_id, err);
	if (ILU_ERRNOK(*err)) { ilu_free(types); return ilu_FALSE; };
	*types_count_out = 1;
	*types_out = types;
	return ILU_CLER(*err);
      }
    }
    if ((known_type = ilu_StrdupE((ilu_string) &pickle.pi_bytes[1], err)), ILU_ERRNOK(*err)) {
      return ilu_FALSE;
    } else {
      types = (ilu_string *) ilu_MallocE(sizeof(ilu_string) * 1, err);
      if (ILU_ERRNOK(*err)) {
	ilu_free(known_type) ; return ilu_FALSE;
      } else {
	types[0] = known_type;
	*types_count_out = 1;
	*types_out = types;
	return ILU_CLER(*err);
      }
    }
  }
}

ilu_TypeKind
  _ilu_pickle2_PickleTypeKind (ilu_Pickle pickle,
			       ilu_Error *err)
{
  ilu_byte typekind = PICKLE_TYPEKIND(pickle.pi_bytes[0]);
  ILU_CLER(*err);
  if (typekind == OBJECT_TYPEKIND)
    return ilu_object_tk;
  else if (typekind == EXTENSIBLE_RECORD_TYPEKIND)
    return ilu_record_tk;
  else if (typekind == HAS_TYPEID_TYPEKIND)
    return ILU_ERR_CONS1(bad_typecode, err, minor, ilu_btm_unknownType, 0);
  else return typekind;
}

ilu_boolean
  _ilu_pickle2_ReadPickle (ilu_Call call,
			   ilu_Pickle pickle,
			   ilu_Error *err)
{
  if (PICKLE_VERSION(pickle.pi_bytes[0]) != CURRENT_VERSION)
    return (ILU_ERR_CONS1(marshal, err, minor, ilu_mm_versionMismatch, ilu_FALSE));

  PICKLE_BUF(call) = pickle.pi_bytes;
  PICKLE_LEN(call) = pickle.pi_len;
  PICKLE_NEXT(call) = 0;
  call->ca_incoming = ilu_TRUE;
  switch (PICKLE_TYPEKIND(pickle.pi_bytes[0]))
    {
    case ilu_byte_tk:
    case ilu_boolean_tk:
    case ilu_character_tk:
    case ilu_shortcharacter_tk:
    case ilu_integer_tk:
    case ilu_shortinteger_tk:
    case ilu_longinteger_tk:
    case ilu_shortcardinal_tk:
    case ilu_cardinal_tk:
    case ilu_longcardinal_tk:
    case ilu_shortreal_tk:
    case ilu_real_tk:
    case ilu_longreal_tk:
    case ILU_CSTRING_TYPEKIND:
      PICKLE_GET_BUFFER(call, 1, err);
      break;
    default:
      {
	ilu_cardinal len = strlen((char *) pickle.pi_bytes + 1);
	PICKLE_GET_BUFFER(call, len + 2, err);
      }
    }
  return ILU_ERROK(*err);
}

ilu_boolean
  _ilu_pickle2_EndPickle (ilu_Call call,
			  ilu_Pickle *pickle,
			  ilu_Error *err)
{
  if ((call->ca_incoming) && ((PICKLE_BUF(call) == NIL) || (PICKLE_REMAINING(call) > 0))) {
    ILU_NOTE(TYPE_DEBUG,
	  ("ilu_EndPickle:  underflow on %s pickle,"
	   " total bytes = %lu, unused bytes = %lu\n",
	   (call->ca_incoming) ? "unpickling" : "forming",
	   PICKLE_LEN(call), PICKLE_REMAINING(call)));
    ILU_ERR_CONS1(marshal, err, minor, ilu_mm_eom, ilu_FALSE);
    /* XXX - this should be an underflow error */
  } else {
    ILU_CLER(*err);
    if (pickle != NIL) {
      pickle->pi_bytes = PICKLE_BUF(call);
      pickle->pi_len = PICKLE_NEXT(call);
    } else {
      ilu_free(PICKLE_BUF(call));
    }
  }
  ilu_free(OBJECT_TYPESTRING(call));
  ilu_free(call->ca_prdata2);
  PICKLE_BUF(call) = NIL;
  PICKLE_NEXT(call) = 0;
  PICKLE_LEN(call) = 0;
  return ILU_ERROK(*err);
}
