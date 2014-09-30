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
/* Portions of this file are also
 *
 * Copyright (C) DSTC Pty Ltd (ACN 052 372 577) 1997.
 */
/* $Id: iiop.c,v 1.236 1999/09/09 17:45:27 janssen Exp $ */
/* Last edited by Mike Spreitzer July 21, 1998 3:12 pm PDT */
/* Chris Jacobi, September 10, 1998 10:56 am PDT */

#include <string.h>

#include "iluntrnl.h"
#include "iiop.h"
#include "iluprotocol.h"
#include "ilutransport.h"

#include "call.h"
#include "connect.h"
#include "port.h"
#include "object.h"
#include "type.h"
#include "method.h"
#include "mooring.h"
#include "server.h"

static const ilu_byte	IIOPDefaultMajorVersion = 1;
static const ilu_byte	IIOPDefaultMinorVersion = 0;

static const char	AnonymousTypeName[] = "-idl-anonymous-";
static const char	ForeignTypeInterface[] = "<foreign>";

#ifdef ADD_VARIANT_SUPPORT
static ilu_Type _IIOP_DefaultPickleType = ILU_NIL;
#endif /* ADD_VARIANT_SUPPORT */

#define IDLAttributePrefix			"ilu--prefix-idlAttribute-"
#define IDLAttributePrefixLen			(sizeof(IDLAttributePrefix)-1)

#define CORBA_NATIVE_OBJECT_IH_PREFIX		"ilu--corba-native-object:"
#define SIZEOF_CORBA_NATIVE_OBJECT_IH_PREFIX	(sizeof(CORBA_NATIVE_OBJECT_IH_PREFIX)-1)

#define CORBA_OBJECT_TYPE_ID			"IDL:omg.org/CORBA/Object:1.0"

static ilu_boolean Initialized = ilu_FALSE;

static ilu_Class  CosNaming_NamingContext = NIL;
static ilu_Method CosNaming_NamingContext_resolve = NIL;

#ifdef ENABLE_DEBUGGING
static ilu_string encode (ilu_bytes key, ilu_cardinal len)
{
  ilu_cardinal i;
  ilu_bytes copy;

  copy = ilu_must_malloc(len + 1);
  memcpy ((char *) copy, (char *) key, len);
  copy[len] = 0;
  for (i = 0;  i < len;  i++)
    if ((copy[i] < ((ilu_byte) 0x20)) || (copy[i] > ((ilu_byte) 0x7E)))
      copy[i] = (ilu_byte) '.';
  return (ilu_string) copy;
}
#endif /* ENABLE_DEBUGGING */

/*************************************************************/
/*		Implement CDR PACKET			     */
/*************************************************************/

/*L1, L2 unconstrained for sizing, end*/
/*Main holds, L2 >= {call's connection's callmu, iomu} for output*/
/*L1 unconstrained, L2 >= {call's connection's callmu, iomu} for input*/

/* ==================== cardinal ==================== */

#ifdef LONG_CARDINAL_TYPE
#define SWAP_LONGWORD(a) ((((a) << 56)) | \
			  (((a) << 40) & 0x00ff000000000000) | \
			  (((a) << 24) & 0x0000ff0000000000) | \
			  (((a) <<  8) & 0x000000ff00000000) | \
			  ((((ilu_longcardinal)(a)) >> 56) & 0x00000000000000ff) | \
			  ((((ilu_longcardinal)(a)) >> 40) & 0x000000000000ff00) | \
			  ((((ilu_longcardinal)(a)) >> 24) & 0x0000000000ff0000) | \
			  ((((ilu_longcardinal)(a)) >>  8) & 0x00000000ff000000))
#endif

#define SWAP_WORD(a) ( ((a) << 24) | \
                      (((a) << 8) & 0x00ff0000) | \
                      (((a) >> 8) & 0x0000ff00) | \
		      ((ilu_cardinal)(a) >>24) )

#define SWAP_HALFWORD(a)  ((((a) << 8) & 0xFF00) | \
			   ((((ilu_shortcardinal)(a)) >> 8) & 0x00FF))

static void
 _cdr_put_u8 (PACKET p, ilu_byte val, ilu_Error *err)
{
  register ilu_byte *buf;

  buf = PACKET_ALLOC(p, 1, 1, err);
  if (ILU_ERROK(*err))
    {
      *buf = val;
      PACKET_INCR(p, 1, 1);
    }
}

static void
 _cdr_put_u16 (PACKET p, ilu_shortcardinal val, ilu_Error *err)
{
  register ilu_byte *buf;
  ilu_shortcardinal val2;

  buf = PACKET_ALLOC(p, 2, 2, err);
  if (ILU_ERROK(*err))
    {
      PTR_ADJUST(buf,p, 2);
      if (ENDIAN_MATCH(p))
	MEMCPY(buf, &val, 2);
      else {
	val2 = SWAP_HALFWORD(val);
	MEMCPY(buf, &val2, 2);
      }
      PACKET_INCR(p, 2, 2);
    }
}

static void
     _cdr_put_u32 (PACKET p, ilu_cardinal val, ilu_Error *err)
{
  register ilu_byte *buf;
  ilu_cardinal val2;

  buf = PACKET_ALLOC(p, 4, 4, err);
  if (ILU_ERROK(*err))
    {
      PTR_ADJUST(buf,p, 4);
      if (ENDIAN_MATCH(p))
	MEMCPY (buf, &val, 4);
      else {
	val2 = SWAP_WORD(val);
	MEMCPY (buf, &val2, 4);
      }
      PACKET_INCR(p, 4, 4);
    }
}

static void
     _cdr_put_u64 (PACKET p, ilu_longcardinal *val, ilu_Error *err)
{
  register ilu_byte *buf;

  buf = PACKET_ALLOC(p, 8, 8, err);
  if (ILU_ERROK(*err))
    {
      PTR_ADJUST(buf,p, 8);
      if (ENDIAN_MATCH(p))
	MEMCPY(buf, val, 8);
      else
	{
	  buf[0] = ((ilu_byte *) val)[7];
	  buf[1] = ((ilu_byte *) val)[6];
	  buf[2] = ((ilu_byte *) val)[5];
	  buf[3] = ((ilu_byte *) val)[4];
	  buf[4] = ((ilu_byte *) val)[3];
	  buf[5] = ((ilu_byte *) val)[2];
	  buf[6] = ((ilu_byte *) val)[1];
	  buf[7] = ((ilu_byte *) val)[0];
	}
      PACKET_INCR(p, 8, 8);
    }
}

static void
 _cdr_put_s8 (PACKET p, signed char val, ilu_Error *err)
{
  _cdr_put_u8(p, (ilu_byte) val, err);
}

static void
 _cdr_put_s16 (PACKET p, ilu_shortinteger val, ilu_Error *err)
{
  _cdr_put_u16(p, (ilu_shortcardinal) val, err);
}

static void
 _cdr_put_s32 (PACKET p, ilu_integer val, ilu_Error *err)
{
  _cdr_put_u32(p, (ilu_cardinal) val, err);
}

static void
  _cdr_put_s64 (PACKET p, ilu_longinteger *val, ilu_Error *err)
{
  _cdr_put_u64(p, (ilu_longcardinal *) val, err);
}

static void
 _cdr_put_r32 (PACKET p, ilu_shortreal val, ilu_Error *err)
{
  _cdr_put_u32(p, *((ilu_cardinal *)(&val)), err);
}

static void
 _cdr_put_r64 (PACKET p, ilu_real val, ilu_Error *err)
{
  _cdr_put_u64(p, ((ilu_longcardinal *) (&val)), err);
}

static void
  _cdr_put_r128 (PACKET p, ilu_longreal val, ilu_Error *err)
{
  register ilu_byte *buf;

  buf = PACKET_ALLOC(p, 16, 8, err);
  if (ILU_ERROK(*err))
    {
      PTR_ADJUST(buf,p, 8);
      if (ENDIAN_MATCH(p))
	MEMCPY(buf, &val, 16);
      else
	{
	  buf[0] = ((ilu_byte *) &val)[15];
	  buf[1] = ((ilu_byte *) &val)[14];
	  buf[2] = ((ilu_byte *) &val)[13];
	  buf[3] = ((ilu_byte *) &val)[12];
	  buf[4] = ((ilu_byte *) &val)[11];
	  buf[5] = ((ilu_byte *) &val)[10];
	  buf[6] = ((ilu_byte *) &val)[9];
	  buf[7] = ((ilu_byte *) &val)[8];
	  buf[8] = ((ilu_byte *) &val)[7];
	  buf[9] = ((ilu_byte *) &val)[6];
	  buf[10] = ((ilu_byte *) &val)[5];
	  buf[11] = ((ilu_byte *) &val)[4];
	  buf[12] = ((ilu_byte *) &val)[3];
	  buf[13] = ((ilu_byte *) &val)[2];
	  buf[14] = ((ilu_byte *) &val)[1];
	  buf[15] = ((ilu_byte *) &val)[0];
	}
      PACKET_INCR(p, 16, 8);
    }
}

static void
 _cdr_put_bytes (PACKET p, ilu_bytes ptr, ilu_cardinal len, ilu_Error *err)
{
  if (_cdr_put_u32 (p, len, err), ILU_ERRNOK(*err))
    return;
  PACKET_WRITE(p, ptr, len, err);
  PACKET_INCR(p, len, 1);
}

static void
 _cdr_put_opaque (PACKET p, ilu_bytes ptr, ilu_cardinal len, ilu_Error *err)
{
  PACKET_WRITE(p, ptr, len, err);
  PACKET_INCR(p, len, 1);
}

static void
     _cdr_get_u8 (PACKET p, ilu_byte *l, ilu_Error *err)
{
  register ilu_byte *buf;

  buf = PACKET_OBTAIN(p, 1, 1, err);
  if (ILU_ERROK(*err))
    {
      *l = buf[0];
      PACKET_INCR(p, 1, 1);
    }
}

static void
  _cdr_get_u16 (PACKET p, ilu_shortcardinal *l, ilu_Error *err)
{
  register ilu_byte *buf;
  ilu_shortcardinal val;

  buf = PACKET_OBTAIN(p, 2, 2, err);
  if (ILU_ERROK(*err))
    {
      PTR_ADJUST(buf,p,2);
      if (ENDIAN_MATCH(p))
	MEMCPY (l, buf, 2);
      else {
	MEMCPY (&val, buf, 2);
	*l = SWAP_HALFWORD(val);
      }
      PACKET_INCR(p, 2, 2);
    }
}

static void
     _cdr_get_u32 (PACKET p, ilu_cardinal *l, ilu_Error *err)
{
  register ilu_byte *buf;
  ilu_cardinal val;

  buf = PACKET_OBTAIN(p, 4, 4, err);
  if (ILU_ERROK(*err))
    {
      PTR_ADJUST(buf,p,4);
      if (ENDIAN_MATCH(p))
	MEMCPY(l, buf, 4);
      else {
	MEMCPY(&val, buf, 4);
	*l = SWAP_WORD(val);
      }
      PACKET_INCR(p, 4, 4);
    }
}

static void
     _cdr_get_u64 (PACKET p, ilu_longcardinal *l, ilu_Error *err)
{
  register ilu_byte *buf;

  buf = PACKET_OBTAIN(p, 8, 8, err);
  if (ILU_ERROK(*err))
    {
      PTR_ADJUST(buf,p, 8);
      if (ENDIAN_MATCH(p))
	MEMCPY (l, buf, 8);
      else
	{
	  ((ilu_byte *) l)[0] = buf[7];
	  ((ilu_byte *) l)[1] = buf[6];
	  ((ilu_byte *) l)[2] = buf[5];
	  ((ilu_byte *) l)[3] = buf[4];
	  ((ilu_byte *) l)[4] = buf[3];
	  ((ilu_byte *) l)[5] = buf[2];
	  ((ilu_byte *) l)[6] = buf[1];
	  ((ilu_byte *) l)[7] = buf[0];
	}
      PACKET_INCR(p, 8, 8);
    }
}

static void
     _cdr_get_s64 (PACKET p, ilu_longinteger *l, ilu_Error *err)
{
  _cdr_get_u64 (p, (ilu_longcardinal *) l, err);
}

static void
     _cdr_get_s32 (PACKET p, ilu_integer *l, ilu_Error *err)
{
  _cdr_get_u32 (p, (ilu_cardinal *) l, err);
}

static void
     _cdr_get_s16 (PACKET p, short *l, ilu_Error *err)
{
  _cdr_get_u16(p, (ilu_shortcardinal *) l, err);
}

static void
     _cdr_get_s8 (PACKET p, signed char * l, ilu_Error *err)
{
  _cdr_get_u8(p, (unsigned char *) l, err);
}

static void
  _cdr_get_r32(PACKET p, float *l, ilu_Error *err)
{
  _cdr_get_u32 (p, (ilu_cardinal *) l, err);
}

static void
  _cdr_get_r64 (PACKET p, ilu_real *l, ilu_Error *err)
{
  _cdr_get_u64 (p, (ilu_longcardinal *) l, err);
}

static void
  _cdr_get_r128 (PACKET p, ilu_longreal *l, ilu_Error *err)
{
  register ilu_byte *buf;

  buf = PACKET_OBTAIN(p, 16, 8, err);
  if (ILU_ERROK(*err))
    {
      PTR_ADJUST(buf,p, 8);
      if (ENDIAN_MATCH(p))
	MEMCPY(l, buf, 16);
      else
	{
	  ((ilu_byte *) l)[0] = buf[15];
	  ((ilu_byte *) l)[1] = buf[14];
	  ((ilu_byte *) l)[2] = buf[13];
	  ((ilu_byte *) l)[3] = buf[12];
	  ((ilu_byte *) l)[4] = buf[11];
	  ((ilu_byte *) l)[5] = buf[10];
	  ((ilu_byte *) l)[6] = buf[9];
	  ((ilu_byte *) l)[7] = buf[8];
	  ((ilu_byte *) l)[8] = buf[7];
	  ((ilu_byte *) l)[9] = buf[6];
	  ((ilu_byte *) l)[10] = buf[5];
	  ((ilu_byte *) l)[11] = buf[4];
	  ((ilu_byte *) l)[12] = buf[3];
	  ((ilu_byte *) l)[13] = buf[2];
	  ((ilu_byte *) l)[14] = buf[1];
	  ((ilu_byte *) l)[15] = buf[0];
	}
      PACKET_INCR(p, 16, 8);
    }
}

static ilu_cardinal IIOPMaxStringSize = 0xFFFFFFFF;

static void
  _cdr_get_bytes(PACKET p, ilu_byte **b, ilu_cardinal *l, ilu_cardinal limit, ILU_ERRS((marshal, bad_param)) *err)
{
  ilu_cardinal l2;
  ilu_boolean was_malloced = ilu_FALSE;

  if (_cdr_get_u32 (p, &l2, err), ILU_ERRNOK(*err))
    return;

  *l = 0;
  if (l2 > IIOPMaxStringSize || (limit > 0 && l2 > limit))
    {
      ILU_NOTE(IIOP_DEBUG, ("%s %lu, which exceeds IIOPMaxStringSize value of %lu or call limit of %lu.\n",
			 "Attempt to read byte sequence of length", l2,
			 IIOPMaxStringSize, limit));
      ILU_ERR_CONS1(marshal, err, minor, ilu_mm_sequenceLimit, 0);
    }
  else if (l2 == 0)
    {
      *l = l2;
      ILU_CLER(*err);
    }
  else
    {
      if (*b == NIL)
	{
	  *b = (ilu_byte *) ilu_malloc(l2 + 1);
	  if (*b == NIL)
	    {
	      ILU_ERR_CONS1(no_memory, err, nbytes, l2 + 1, 0);
	      return;
	    }
	  was_malloced = ilu_TRUE;
	}
      PACKET_READ(p, *b, l2, err);
      if (ILU_ERRNOK(*err) && was_malloced)
	ilu_free(*b);
      else
	{
	  PACKET_INCR(p, l2, 1);
	  if (was_malloced)
	    (*b)[l2] = 0;
	  *l = l2;
	}
    }
}

static void
  _cdr_get_opaque (PACKET p, ilu_byte **b, ilu_cardinal len, ilu_Error *err)
{
  ilu_boolean was_malloced = ilu_FALSE;

  if (*b == NIL)
    {
      *b = (ilu_byte *) ilu_malloc(len);
      if (*b == NIL)
	{
	  ILU_ERR_CONS1(no_memory, err, nbytes, len, 0);
	  return;
	}
      else
	was_malloced = ilu_TRUE;
    }
  PACKET_READ(p, *b, len, err);
  if (ILU_ERRNOK(*err) && was_malloced)
    ilu_free(*b);
  else
    PACKET_INCR(p, len, 1);
}

/* L1.sup < trmu; isMem ? (!p->bs->tr_wc) : (L1.sup < cmu) */
static void
_cdr_fullDestroy(PACKET p, ilu_boolean isMem, ilu_Error * err)
{
  ilu_integer     cdfd;

  if (p->bs != NIL && p->bs_needs_closing_on_finish_call) {
    if (isMem)
      (void) transport_close(p->bs, &cdfd, err);
    else
      (void) ilu_CloseTransport(p->bs, &cdfd, err);
    _ilu_Assert(cdfd == 0, "IIOP _cdr_destroy");
  } else
    ILU_CLER(*err);
  FREETOKEN(p->objKey);
  FREETOKEN(p->principal);
}

/* L1.sup < cmu */
static void _cdr_destroy (PACKET p, ilu_Error *err)
{
  _cdr_fullDestroy(p, ilu_FALSE, err);
  return;
}

/* L1.sup < trmu; !p->bs->tr_wc */
static void _cdr_packet_destroy (PACKET p, ilu_Error *err)
{
  _cdr_fullDestroy(p, ilu_TRUE, err);
  return;
}

static struct ilu_packet_methods_s CDR_methods = {

  _cdr_put_s8, _cdr_put_s16, _cdr_put_s32, _cdr_put_s64,
  _cdr_put_u8, _cdr_put_u16, _cdr_put_u32, _cdr_put_u64,
  _cdr_put_r32, _cdr_put_r64, _cdr_put_r128,
  _cdr_put_bytes, _cdr_put_opaque,

  _cdr_get_s8, _cdr_get_s16, _cdr_get_s32, _cdr_get_s64,
  _cdr_get_u8, _cdr_get_u16, _cdr_get_u32, _cdr_get_u64,
  _cdr_get_r32, _cdr_get_r64, _cdr_get_r128,
  _cdr_get_bytes, _cdr_get_opaque,

  _cdr_packet_destroy
  };

static PACKET _cdr_InitPacket (PACKET p, ilu_Transport bs, enum byte_order bo, ilu_bytes offset)
{
  p->methods = &CDR_methods;
  p->bs = bs;
  p->bs_needs_closing_on_finish_call = ilu_FALSE;
  p->vop = offset;
  p->byteorder = bo;
  p->size = 0;
  p->objKey = NIL;
  p->objKeyLen = 0;
  p->principal = NIL;
  p->principalLen = 0;
  p->short_char_codeset = ilu_IIOP_NATIVE_SHORT_CHAR_CODE_SET_ID;
  p->char_codeset = ilu_IIOP_NATIVE_CHAR_CODE_SET_ID;
  return (p);
}

static PACKET _cdr_CreatePacket (ilu_Transport bs, enum byte_order bo, ilu_bytes offset, ilu_Error *err)
{
  PACKET p = (PACKET) ilu_malloc (sizeof(struct ilu_packet_s));
  if (p == NIL)
    ILU_ERR_CONS1(no_memory, err, nbytes, sizeof(struct ilu_packet_s), 0);
  else
    {
      (void) _cdr_InitPacket (p, bs, bo, offset);
      ILU_CLER(*err);
    }
  return p;
}

static PACKET _cdr_InmemPacket (ilu_cardinal size, ilu_bytes data, enum byte_order bo, ilu_bytes offset, ilu_Error *err)
{
  ilu_Transport t;

  t = _ilu_BufferTransport_Create (size, data, err);
  if (ILU_ERROK(*err))
    return (_cdr_CreatePacket (t, bo, offset, err));
  else
    return NIL;
}

/* L1.sup < trmu */
static void _cdr_InmemFree (PACKET pk, ilu_cardinal *bufferlen, ilu_bytes *buffer)
{
  ilu_Error err;

  ilu_Transport t = pk->bs;
  packet_destroy(pk, &err);
  ILU_HANDLED(err);
  FREETOKEN(pk);
  _ilu_BufferTransport_Destroy (t, bufferlen, buffer, &err);
  ILU_HANDLED(err);
}

/**********************************************************************
  End of packet implementation
***********************************************************************/

/*L1, L2, Main unconstrained*/

static ilu_bytes CharSetsServiceContext = ILU_NIL;
static ilu_cardinal CharSetsServiceContextLength = 0;
static ilu_cardinal CharSetsServiceContextPaddedLength = 0;

static ilu_bytes CodeSetsTaggedComponent = ILU_NIL;
static ilu_cardinal CodeSetsTaggedComponentLength = 0;

static void SetupCharsetsEncapsulations (ilu_Error *err)
{
  PACKET pk;

  /* form service context containing type information first */
  pk = _cdr_InmemPacket (12, NIL, NATIVE_BYTE_ORDER, 0, err);
  if (ILU_ERRNOK(*err))
    return;
  if ((_cdr_put_u8 (pk, (NATIVE_BYTE_ORDER == LittleEndian), err), ILU_ERRNOK(*err)) ||
      (_cdr_put_u8 (pk, IIOPDefaultMajorVersion, err), ILU_ERRNOK(*err)) ||
      (_cdr_put_u8 (pk, IIOPDefaultMinorVersion, err), ILU_ERRNOK(*err)) ||
      (_cdr_put_u32 (pk, ilu_IIOP_NATIVE_SHORT_CHAR_CODE_SET_ID, err), ILU_ERRNOK(*err)) ||
      (_cdr_put_u32 (pk, ilu_IIOP_NATIVE_CHAR_CODE_SET_ID, err), ILU_ERRNOK(*err))) {
    _cdr_InmemFree(pk, NIL, NIL);
    return;
  }
  _cdr_InmemFree(pk, &CharSetsServiceContextLength, &CharSetsServiceContext);
  CharSetsServiceContextPaddedLength =
    CharSetsServiceContextLength + PADDING_NEC(CharSetsServiceContextLength,4);

  /* now the encapsulation for object references */
  pk = _cdr_InmemPacket (20, NIL, NATIVE_BYTE_ORDER, 0, err);
  if (ILU_ERRNOK(*err))
    {
      ilu_free(CharSetsServiceContext);
      CharSetsServiceContext = ILU_NIL;
      return;
    }
  if ((_cdr_put_u8 (pk, (NATIVE_BYTE_ORDER == LittleEndian), err), ILU_ERRNOK(*err)) ||
      (_cdr_put_u8 (pk, IIOPDefaultMajorVersion, err), ILU_ERRNOK(*err)) ||
      (_cdr_put_u8 (pk, IIOPDefaultMinorVersion, err), ILU_ERRNOK(*err)) ||
      (_cdr_put_u32 (pk, ilu_IIOP_NATIVE_SHORT_CHAR_CODE_SET_ID, err), ILU_ERRNOK(*err)) ||
      (_cdr_put_u32 (pk, 0, err), ILU_ERRNOK(*err)) ||	/* no conversion sets */
      (_cdr_put_u32 (pk, ilu_IIOP_NATIVE_CHAR_CODE_SET_ID, err), ILU_ERRNOK(*err)) ||
      (_cdr_put_u32 (pk, 0, err), ILU_ERRNOK(*err))) {	/* no conversion sets */
    _cdr_InmemFree(pk, NIL, NIL);
    ilu_free(CharSetsServiceContext);
    CharSetsServiceContext = ILU_NIL;
    return;
  }
  _cdr_InmemFree(pk, &CodeSetsTaggedComponentLength, &CodeSetsTaggedComponent);
}

static ilu_boolean
  InterpretCodeSetServiceContext (ilu_bytes data,
				  ilu_cardinal datalen,
				  ilu_cardinal *short_character_codeset,
				  ilu_cardinal *character_codeset,
				  ilu_Error *err)
{
  PACKET pk;
  ilu_byte byte_order_flag, major_version, minor_version;
  ilu_cardinal shortchar_codeset, char_codeset, junklen;
  ilu_bytes junk;

  pk = _cdr_InmemPacket (datalen, data, (data[0] == 0) ? BigEndian : LittleEndian, 0, err);
  if (ILU_ERRNOK(*err))
    return ilu_FALSE;
  if (!((_cdr_get_u8 (pk, &byte_order_flag, err), ILU_ERRNOK(*err)) ||
	(_cdr_get_u8 (pk, &major_version, err), ILU_ERRNOK(*err)) ||
	(_cdr_get_u8 (pk, &minor_version, err), ILU_ERRNOK(*err)) ||
	(_cdr_get_u32 (pk, &shortchar_codeset, err), ILU_ERRNOK(*err)) ||
	(_cdr_get_u32 (pk, &char_codeset, err), ILU_ERRNOK(*err)))) {
    *short_character_codeset = shortchar_codeset;
    *character_codeset = char_codeset;
  }
  _cdr_InmemFree (pk, &junklen, &junk);
  return ILU_ERROK(*err);
}

static void Initialize (ilu_Error *err)
{
  if (Initialized) {
    ILU_CLER(*err);
    return;
  };

  SetupCharsetsEncapsulations(err);
  if (ILU_ERRNOK(*err)) return;

#ifdef ADD_TYPE_REGISTRATION_SUPPORT
  {
    ilu_boolean newreg;
    ilu_AcquireMutex(ilu_otmu);
    ilu_RegisterObjectType ("Object", "ilu",
			    class_brand(ilu_rootClass),
			    class_unique_id(ilu_rootClass),
			    ilu_rootClass, &newreg, err);
    ilu_ReleaseMutex(ilu_otmu);
    if (ILU_ERRNOK(*err)) return;
  }
#endif

  Initialized = ILU_ERROK(*err);
}

static ilu_refany _IIOP_CreateDataBlock (ilu_ProtocolInfo pinfo,
					 ilu_Error *err)
{
  struct IIOP_DataBlock *s;
  int             major, minor, mapping, vals;
  char            key[2049];
  char           *localpinfo;

  /* Check for serial version */
  if (pinfo[0] == 's')
    localpinfo = pinfo + 1;
  else
    localpinfo = pinfo;

  if ((strcmp(localpinfo, "iiop") == 0) ||
      (strcmp(localpinfo, "iiop_") == 0)) {
    /* special case, grandfathered in for use with ports. */
    major = 1;
    minor = 0;
    mapping = 1;
    vals = 3;
  } else if ((vals = sscanf(localpinfo, "iiop_%i_%i_%i_%2048s", &major,
			    &minor, &mapping, key)) < 3) {
    ILU_NOTE(IIOP_DEBUG,
	     ("_IIOP_CreateDataBlock:  Badly formed pinfo <%s>.\n",
	      pinfo));
    return (ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_pinfo, NIL));
  };
  if ((major != 1) || (minor != 0)) {
    ILU_NOTE(IIOP_DEBUG,
	     ("Only IIOP 1.0 is currently supported.  Indicated version is %d.%d.\n",
	      major, minor));
    return (ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_pinfo, NIL));
  };
  if ((s = ilu_malloc(sizeof(*s))) == NIL)
    return (ILU_ERR_CONS1(no_memory, err, nbytes, sizeof(*s), NIL));
  s->major = major;
  s->minor = minor;
  s->mapping = mapping;
  s->relocate_checked = ilu_FALSE;
  s->charsets_sent = ilu_FALSE;
  if (vals > 3) {
    s->key = (ilu_byte *) _ilu_DecodeBuffer(key, strlen(key),
					    &s->keylen, err);
    if (ILU_ERRNOK(*err)) {
      ilu_free(s);
      return NIL;
    };
  } else {
    s->key = NIL;
    s->keylen = 0;
  }
  ILU_CLER(*err);
  return (s);
}

static void _IIOP_FreeDataBlock (ilu_refany d /* , ilu_Error *err */)
{
  struct IIOP_DataBlock *s = (struct IIOP_DataBlock *) d;

  if (s->key != ILU_NIL)
    ilu_free(s->key);
  ilu_free(s);
}

static ilu_boolean _IIOP_InitCall (ilu_Call call , ilu_Error *err)
{
  PACKET p = _cdr_CreatePacket (connection_transport(call->ca_connection),
				NATIVE_BYTE_ORDER,
				0,
				err);

  if (ILU_ERRNOK(*err))
    return ilu_FALSE;
  iiop_set_packet(call, p);
  /* figure header size */
  return ilu_TRUE;
}

static ilu_boolean _IIOP_FinishCall (ilu_Call call, ilu_Error *err)
{
  if (!call->ca_incoming)
    iiop_objKey(call) = NIL;
  _cdr_destroy (iiop_packet(call), err);
  FREETOKEN(iiop_packet(call));
  iiop_set_packet(call, NIL);
  return ILU_ERROK(*err);
}

/*======================================================================*/
/*=========== Mapping to and from IIOP classes and methods =============*/
/*======================================================================*/

static ilu_Class FindClassFromObjectKey (ilu_bytes key, ilu_cardinal key_len,
					 ilu_Server s)
{
  /* treat "key" as URL of an object, return its class */
  /* For an ILU object key marshalled over UNO, we have a full URL */
  ilu_Class	cl = NIL;
  ilu_Error	lerr;

  if (strcmp((ilu_string) key, "ilu") != 0)
    {
      char ih[2048];
      char *newkey;
      ilu_Object kobj;

#ifdef ENABLE_DEBUGGING
      ilu_string encoded_key = encode (key, key_len);
      ILU_NOTE(IIOP_DEBUG,
	    ("ILU(iiop.c:FindClassFromObjectKey):  object key <%s> not an ILU object key\n", encoded_key));
      ilu_free(encoded_key);
#endif
      /* we assume that it's a "foreign" object key somehow placed in this server */
      strcpy(ih, CORBA_NATIVE_OBJECT_IH_PREFIX);
      newkey = _ilu_EncodeBuffer((char *) key, key_len, &lerr);
      if (ILU_ERROK(lerr)) {
	strcat (ih, newkey);
	ilu_free(newkey);
	if ((kobj = _ilu_FindObjectInServer(ih, s)) != NIL)
	  cl = object_class(kobj);
	else {
	  ILU_NOTE(IIOP_DEBUG,
		   ("ILU(iiop.c:FindClassFromObjectKey):  no object in server <%s> with ih <%s>\n",
		    server_id(s), ih));
	}
      } else {
	ILU_HANDLED(lerr);
      }
    }
  else
    cl = ilu_FindClassFromID((ilu_string)(key + 4));
  return cl;
}

static ilu_Method FindMethodOnClass2 (ilu_Class c, ilu_string mname, ilu_Class *realclass)
{
  ilu_cardinal i;

  for (i = 0;  i < c->cl_method_count;  i++)
    {
      if (strcmp (mname, c->cl_methods[i].me_name) == 0)
	{
	  *realclass = c;
	  return (c->cl_methods + i);
	}
    }
  for (i = 0;  i < c->cl_scls_count;  i++)
    {
      ilu_Method m;
	  
      if ((m = FindMethodOnClass2 (c->cl_sclses[i], mname, realclass)) != NIL)
	return (m);
    }
  return NIL;
}

static ilu_Method FindMethodOnClass (ilu_Class c, ilu_string mname, ilu_Class *realclass)
{
  ilu_Method m = NIL;

  if ((m = FindMethodOnClass2 (c, mname, realclass)) == NIL)
    {
      ilu_Class root = ilu_rootClass;
      ilu_cardinal i;

      for (i = 0;  i < root->cl_method_count;  i++)
	{
	  if (strcmp (mname, root->cl_methods[i].me_name) == 0)
	    {
	      m = root->cl_methods + i;
	      *realclass = ilu_rootClass;
	      break;
	    }
	}
    }

  return m;
}

static void
  FindClassAndMethodFromIDLMethodName (ilu_Call call, ilu_Class ptype, ilu_string idl_name)
{
  char buf[1024];
  char *b;
  char *p1, *p2;
  ilu_Method m;
  ilu_Class realclass;

  if ((strlen(idl_name) + IDLAttributePrefixLen + 1) < sizeof(buf))
    b = buf;
  else
    b = ilu_must_malloc(strlen(idl_name) + 1);

  for (p1 = idl_name, p2 = b;  *p1 != 0;  p1++, p2++)
    {
      if (*p1 == '_')
	*p2 = '-';
      else
	*p2 = *p1;
    }
  *p2 = 0;
  m = FindMethodOnClass (ilu_rootClass, b, &realclass);
  if (m == NIL) {
    if (idl_name[0] == '_') {
      memmove (b + IDLAttributePrefixLen, b, strlen(b) + 1);
      strncpy (b, IDLAttributePrefix, IDLAttributePrefixLen);
    }
    m = FindMethodOnClass (ptype, b, &realclass);
  }
  if (b != buf)
    ilu_free(b);
  call_intro_type(call) = realclass;
  call_method(call) = m;
  return;
}

static void FormMethodName (char *buf, ilu_Method m)
{
  char *p1, *p2;

  if (strncmp(m->me_name, IDLAttributePrefix, IDLAttributePrefixLen) == 0)
    p2 = m->me_name + IDLAttributePrefixLen;
  else
    p2 = m->me_name;
  for (p1 = buf;  *p2 != 0;  p1++, p2++)
    {
      if (*p2 == '-')
	*p1 = '_';
      else
	*p1 = *p2;
    }
  *p1 = 0;
}

static struct CORBA_exception_s {
  char *name;
  ilu_ProtocolException val;
} CORBA_exceptions[] = {
  { "IDL:omg.org/CORBA/UNKNOWN:1.0", ilu_ProtocolException_Unknown},
  { "IDL:omg.org/CORBA/BAD_PARAM:1.0", ilu_ProtocolException_GarbageArguments},
  { "IDL:omg.org/CORBA/NO_MEMORY:1.0", ilu_ProtocolException_Unknown},
  { "IDL:omg.org/CORBA/IMP_LIMIT:1.0", ilu_ProtocolException_Unknown},
  { "IDL:omg.org/CORBA/COMM_FAILURE:1.0", ilu_ProtocolException_LostConnection},
  { "IDL:omg.org/CORBA/INV_OBJREF:1.0", ilu_ProtocolException_GarbageArguments},
  { "IDL:omg.org/CORBA/NO_PERMISSION:1.0", ilu_ProtocolException_RequestRejected},
  { "IDL:omg.org/CORBA/INTERNAL:1.0", ilu_ProtocolException_Unknown},
  { "IDL:omg.org/CORBA/MARSHALL:1.0", ilu_ProtocolException_GarbageArguments},
  { "IDL:omg.org/CORBA/INITIALIZE:1.0", ilu_ProtocolException_Unknown},
  { "IDL:omg.org/CORBA/NO_IMPLEMENT:1.0", ilu_ProtocolException_NoSuchMethodOnClass},
  { "IDL:omg.org/CORBA/BAD_TYPECODE:1.0", ilu_ProtocolException_NoSuchClassAtServer},
  { "IDL:omg.org/CORBA/BAD_OPERATION:1.0", ilu_ProtocolException_NoSuchMethodOnClass},
  { "IDL:omg.org/CORBA/NO_RESOURCES:1.0", ilu_ProtocolException_Unknown},
  { "IDL:omg.org/CORBA/NO_RESPONSE:1.0", ilu_ProtocolException_RequestTimeout},
  { "IDL:omg.org/CORBA/PERSIST_STORE:1.0", ilu_ProtocolException_Unknown},
  { "IDL:omg.org/CORBA/BAD_INV_ORDER:1.0", ilu_ProtocolException_Unknown},
  { "IDL:omg.org/CORBA/TRANSIENT:1.0", ilu_ProtocolException_Unknown},
  { "IDL:omg.org/CORBA/FREE_MEM:1.0", ilu_ProtocolException_Unknown},
  { "IDL:omg.org/CORBA/INV_IDENT:1.0", ilu_ProtocolException_GarbageArguments},
  { "IDL:omg.org/CORBA/INV_FLAG:1.0", ilu_ProtocolException_GarbageArguments},
  { "IDL:omg.org/CORBA/INTF_REPOS:1.0", ilu_ProtocolException_Unknown},
  { "IDL:omg.org/CORBA/BAD_CONTEXT:1.0", ilu_ProtocolException_GarbageArguments},
  { "IDL:omg.org/CORBA/OBJ_ADAPTER:1.0", ilu_ProtocolException_Unknown},
  { "IDL:omg.org/CORBA/DATA_CONVERSION:1.0", ilu_ProtocolException_GarbageArguments}
};

static ilu_cardinal FigureExceptionIndexFromIDLName (ilu_Class c, ilu_Method m, char *rep_id)
{
  ilu_cardinal i;

  ILU_NOTE(IIOP_DEBUG, ("(FigureExceptionIndexFromIDLName):  exception <%s> received.\n",
		     rep_id));
  if (m != NIL)
    {
      for (i = 0;  i < m->me_exceptionCount;  i++)
	if (strcmp(m->me_exceptionVector[i], rep_id) == 0)
	  return (i + 1);
      ILU_NOTE(IIOP_DEBUG, ("(FigureExceptionIndexFromIDLName):  no exception on method '%s' with ID '%s'.\n",
			 m->me_name, rep_id));
      return 0;
    }
  else
    {
      for (i = 0;  i < (sizeof(CORBA_exceptions)/sizeof(struct CORBA_exception_s));  i++)
	{
	  if (strcmp(CORBA_exceptions[i].name, rep_id) == 0)
	    return (CORBA_exceptions[i].val);
	}
      ILU_NOTE(IIOP_DEBUG, ("(FigureExceptionIndexFromIDLName):  no standard exception '%s' known.\n",
			 rep_id));
      return ilu_ProtocolException_Unknown;
    }
}

static char * system_exceptions[] = {
  /* ilu_ProtocolException_Success = 0, */
  "IDL:omg.org/CORBA/BAD_TYPECODE:1.0",		/* ilu_ProtocolException_NoSuchClassAtServer = 1 */
  "IDL:omg.org/CORBA/UNKNOWN:1.0",		/* ilu_ProtocolException_ClassVersionMismatch = 2 */
  "IDL:omg.org/CORBA/BAD_OPERATION:1.0",	/* ilu_ProtocolException_NoSuchMethodOnClass = 3 */
  "IDL:omg.org/CORBA/BAD_PARAM:1.0",		/* ilu_ProtocolException_GarbageArguments = 4 */
  "IDL:omg.org/CORBA/UNKNOWN:1.0",		/* ilu_ProtocolException_Unknown = 5 */
  "IDL:omg.org/CORBA/COMM_FAILURE:1.0",		/* ilu_ProtocolException_LostConnection = 6 */
  "IDL:omg.org/CORBA/UNKNOWN:1.0",		/* ilu_ProtocolException_RequestRejected = 7 */
  "IDL:omg.org/CORBA/NO_RESPONSE:1.0"		/* ilu_ProtocolException_RequestTimeout = 8 */
  };

static ilu_string FigureNameOfException (ilu_Class c, ilu_Method m, ilu_integer index, ilu_ProtocolException sysExcn)
{
  if (index == 0)
    {
      ilu_cardinal val = ((ilu_cardinal) sysExcn) - 1;

      if (val >= (sizeof(system_exceptions)/sizeof(char *)))
	{
	  ILU_NOTE(IIOP_DEBUG, ("(iiop.c:FigureNameOfException):  Unknown ilu_ProtocolException value passed:  %lu.\n", (long unsigned) val));
	  return (NIL);
	}
      else
	return system_exceptions[val];
    }
  else
    {
      if ((ilu_cardinal) index > m->me_exceptionCount)
	{
	  ILU_NOTE(IIOP_DEBUG, ("Invalid exception index %u specified for method %s.\n",
			     index, m->me_name));
	  return (NIL);
	}
      return (m->me_exceptionVector[index - 1]);
    }
}

ilu_string
  ilu_IIOP_ServerIDFromObjectKey (ilu_bytes key, unsigned int key_len,
				  ilu_string hostname, unsigned int port,
				  unsigned int giop_major_version, unsigned int giop_minor_version,
				  ilu_Error *err)
{
  ilu_cardinal sid2[2];
  sid2[0] = ilu_CRC32(key, key_len);
  sid2[1] = (giop_major_version << 24) + (giop_minor_version << 16) + port;
  sid2[1] = ilu_CRC32WithAccum((ilu_bytes) "iiop_1_0_1", strlen("iiop_1_0_1"), sid2[1]);
  sid2[1] = ilu_CRC32WithAccum((ilu_bytes) hostname, strlen(hostname), sid2[1]);
  return ((ilu_string) _ilu_EncodeBuffer((char *) sid2, 2 * sizeof(ilu_cardinal), err));
}

ilu_string
  ilu_IIOP_IHFromObjectKey (ilu_bytes key, ilu_cardinal key_len,
			    ilu_Error *err)
{
  char *encoded_key;
  char *ih = NIL;
  encoded_key = _ilu_EncodeBuffer ((char *) key, key_len, err);
  if (ILU_ERRNOK(*err)) return NIL;
  ih = ilu_MallocE(strlen(encoded_key) + 1 + SIZEOF_CORBA_NATIVE_OBJECT_IH_PREFIX, err);
  if (ILU_ERRNOK(*err)) { ilu_free(encoded_key); return NIL; };
  sprintf(ih, "%s%s", CORBA_NATIVE_OBJECT_IH_PREFIX, encoded_key);
  ilu_free(encoded_key);
  return ih;
}

static void FreeIORData (struct IIOP_IOR_IOR *ior)
{
  ilu_cardinal i;

  FREETOKEN(ior->type_id);
  for (i = 0;  i < ior->nProfiles;  i++)
    {
      FREETOKEN(ior->Profile[i].profileData);
    }
}

#define isalphanum(x)	(((x)<='z'&&(x)>='a')||((x)<='Z'&&(x)>='A')||((x)<='9'&&(x)>='0'))
static const char hextable[] = "0123456789ABCDEF";
#define hexval(x)	(((x)>='0'&&(x)<='9')?((x)-'0'):(((x)>='A'&&(x)<='F')?((x)-'A'+10):(((x)>='a'&&(x)<='f')?((x)-'a'+10):0)))

static ilu_boolean
FindServerIIOPPort (ilu_Server server,
		   ilu_ProtocolInfo *pinfo,
		   ilu_TransportInfo *tinfo,
		   ilu_boolean *owned,
		   ilu_Error *err)
{
  *pinfo = NIL;
  if (server_is_true(server)) {
    ilu_Port p;
    /* given a true server, grovel through the ports to find one that
       talks IIOP... */
    for (p = server->sr_ports.pl_next;  p != NIL;  p = p->po_links.pl_next) {
      if (strncmp(p->po_pinfo, "iiop", 4) == 0) {
				/* we found an iiop port */
	*pinfo = port_pinfo(p);
	*tinfo = port_tinfo(p);
	*owned = ilu_FALSE;
	return ILU_CLER(*err);
      }
    }
  } else {
    char *p, *q;
    for (p = server_cinfo(server).icb_base;  p != NIL;  p = strchr(p, ILU_CINFO_MARKER)) {
      while (*p == ILU_CINFO_MARKER)
	p++;
      if (strncmp(p, "iiop", 4) == 0) {
	q = strchr(p, ILU_CINFO_MARKER);
	if (q == NIL)
	  q = p + strlen(p);
	*owned = ilu_TRUE;
	return (_ilu_ParseConnectInfo(p, q - p, pinfo, tinfo, err));
      }
    }
  }
  /* couldn't find an iiop port on this server */
  ILU_CLER(*err);
  return ilu_FALSE;
}

static ilu_boolean _iiop_AddIIOPProfile (ilu_Server server,
					 ilu_string ih,
					 ilu_string mstid,
					 ilu_ProtocolInfo pinfo,
					 ilu_TransportInfo tinfo,
					 struct IIOP_IOR_IOR *ior,
					 ilu_Error *err)
{
  ilu_byte	hostname[1000];
  ilu_byte *	object_key;
  ilu_cardinal object_key_len = 0;
  char *sid;
  unsigned long port;
  int i;
  PACKET pk;

  sid = server_id(server);

  if ((strncmp(pinfo, "iiop_1_0_1", 10) != 0) &&
      (strcmp(pinfo, "iiop_") != 0) &&
      (strcmp(pinfo, "iiop") != 0))
    return ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_ps, ilu_FALSE);
  /* Find host and port */
  for (i = 0;  tinfo[i] != NIL;  i++)
    ;
  if ((i < 1) || (sscanf (tinfo[i-1], "tcp_%[^_]_%lu", hostname, &port) != 2))
    return ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_ti, ilu_FALSE);

  if (strncmp(ih, CORBA_NATIVE_OBJECT_IH_PREFIX, SIZEOF_CORBA_NATIVE_OBJECT_IH_PREFIX) == 0)
    /* non-ILU object.  Recover the original object key and send it. */
    {
      object_key = (ilu_byte*) _ilu_DecodeBuffer (ih + SIZEOF_CORBA_NATIVE_OBJECT_IH_PREFIX,
						  strlen(ih + SIZEOF_CORBA_NATIVE_OBJECT_IH_PREFIX),
						  &object_key_len, err);
      if (ILU_ERRNOK(*err)) return ilu_FALSE;
    }
  else
    /* ILU object.  Send mstid, sid, and ih as object key.
       Unfortunately, we need to put the type ID in the object key,
       because the IIOP protocol has left it out of its header. */
    {
      int sidl = strlen(sid), ihl = strlen(ih), mstidl = strlen(mstid);
      object_key_len = sidl + ihl + mstidl + 7;
      object_key = (ilu_bytes) ilu_malloc(object_key_len);
      if (object_key == NIL)
	return ILU_ERR_CONS1(no_memory, err, nbytes, object_key_len, ilu_FALSE);
      memcpy ((void *) object_key, "ilu", 4);
      memcpy ((void *) (object_key + 4), mstid, mstidl + 1);
      memcpy ((void *) (object_key + mstidl + 5), sid, sidl + 1);
      memcpy ((void *) (object_key + mstidl + sidl + 6), ih, ihl + 1);
    }

  /* Build IIOP profile.

     Figure max size.  should be
     1 for byte order,
     2 for IIOP_Version,
     (1 padding),
     max of 4+15=19 for hostname,
     (padding),
     2 for port,
     (padding),
     n + 4 for object_key
     */

  pk = _cdr_InmemPacket (object_key_len + 4 + 20 + 4 + 4, NIL, NATIVE_BYTE_ORDER, 0, err);
  if (ILU_ERRNOK(*err))
    {
      ilu_free(object_key);
      return ilu_FALSE;
    }
  if ((_cdr_put_u8 (pk, (NATIVE_BYTE_ORDER == LittleEndian), err), ILU_ERRNOK(*err)) ||
      (_cdr_put_u8 (pk, IIOPDefaultMajorVersion, err), ILU_ERRNOK(*err)) ||
      (_cdr_put_u8 (pk, IIOPDefaultMinorVersion, err), ILU_ERRNOK(*err)) ||
      (_cdr_put_bytes (pk, hostname, (ilu_cardinal) (strlen((ilu_string) hostname) + 1), err), ILU_ERRNOK(*err)) ||
      (_cdr_put_u16 (pk, (ilu_shortcardinal) port, err), ILU_ERRNOK(*err)) ||
      (_cdr_put_bytes (pk, object_key, object_key_len, err), ILU_ERRNOK(*err)))
    {
      _cdr_InmemFree(pk, NIL, NIL);
      ilu_free(object_key);
      return (ilu_FALSE);
    }
  ior->Profile[ior->nProfiles].tag = IIOP_TAG_INTERNET_IOP;
  _cdr_InmemFree (pk, &ior->Profile[ior->nProfiles].profileDataLen,
		  &ior->Profile[ior->nProfiles].profileData);
  ior->nProfiles += 1;
  ilu_free(object_key);
  return ilu_TRUE;
}

static ilu_boolean _iiop_AddCharSetInfo (struct IIOP_IOR_IOR *ior,
					 ilu_Error *err)
{
  PACKET pk;

  /* Build multicomponent profile.

     Figure max size.  should be
     1 for byte order,
     2 for IIOP_Version,
     (1 padding),
     4 for length (1)
     4 for CodeSetsId,
     4 for CodeSetsTaggedComponentLength,
     CodeSetsTaggedComponentLength for CodeSetsTaggedComponent
     */

  if (CodeSetsTaggedComponentLength == 0) {
    pk = _cdr_InmemPacket (28, NIL, NATIVE_BYTE_ORDER, 0, err);
    if (ILU_ERRNOK(*err)) return ilu_FALSE;
    if ((_cdr_put_u8 (pk, (NATIVE_BYTE_ORDER == LittleEndian), err), ILU_ERRNOK(*err)) ||
	(_cdr_put_u8 (pk, IIOPDefaultMajorVersion, err), ILU_ERRNOK(*err)) ||
	(_cdr_put_u8 (pk, IIOPDefaultMinorVersion, err), ILU_ERRNOK(*err)) ||
	(_cdr_put_u32 (pk, IIOP_CHARSET_ISO_LATIN1, err), ILU_ERRNOK(*err)) ||
	(_cdr_put_u32 (pk, 1, err), ILU_ERRNOK(*err)) ||
	(_cdr_put_u32 (pk, IIOP_CHARSET_US_ASCII, err), ILU_ERRNOK(*err)) ||
	(_cdr_put_u32 (pk, IIOP_CHARSET_UNICODE_UCS_2, err), ILU_ERRNOK(*err)) ||
	(_cdr_put_u32 (pk, 1, err), ILU_ERRNOK(*err)) ||
	(_cdr_put_u32 (pk, IIOP_CHARSET_UNICODE_UTF_8, err), ILU_ERRNOK(*err)))
      {
	_cdr_InmemFree(pk, NIL, NIL);
	return (ilu_FALSE);
      }
    _cdr_InmemFree (pk, &CodeSetsTaggedComponentLength, &CodeSetsTaggedComponent);
  }

  pk = _cdr_InmemPacket (16 + CodeSetsTaggedComponentLength, NIL, NATIVE_BYTE_ORDER, 0, err);
  if (ILU_ERRNOK(*err))
    {
      return ilu_FALSE;
    }
  if ((_cdr_put_u8 (pk, (NATIVE_BYTE_ORDER == LittleEndian), err), ILU_ERRNOK(*err)) ||
      (_cdr_put_u8 (pk, IIOPDefaultMajorVersion, err), ILU_ERRNOK(*err)) ||
      (_cdr_put_u8 (pk, IIOPDefaultMinorVersion, err), ILU_ERRNOK(*err)) ||
      (_cdr_put_u32 (pk, 1, err), ILU_ERRNOK(*err)) ||
      (_cdr_put_u32 (pk, IIOP_TAG_CODE_SETS, err), ILU_ERRNOK(*err)) ||
      (_cdr_put_bytes (pk, CodeSetsTaggedComponent, CodeSetsTaggedComponentLength, err),
       ILU_ERRNOK(*err)))
    {
      _cdr_InmemFree(pk, NIL, NIL);
      return (ilu_FALSE);
    }
  ior->Profile[ior->nProfiles].tag = IIOP_TAG_MULTIPLE_COMPONENT_IOP;
  _cdr_InmemFree (pk, &ior->Profile[ior->nProfiles].profileDataLen,
		  &ior->Profile[ior->nProfiles].profileData);
  ior->nProfiles += 1;
  return ilu_TRUE;
}

static ilu_boolean _iiop_AddILUProfile (char *sbh,
					struct IIOP_IOR_IOR *ior,
					ilu_Error *err)
{
  PACKET pk;

  /* ILU object.  Send SBH as object key. */
  
  /* Build IIOP ILU profile.

     Figure max size.  should be
     1 for byte order,
     2 for IIOP_Version,
     1 for padding,
     4 for length of SBH,
     n + 1 for SBH
     */

  pk = _cdr_InmemPacket (9 + strlen(sbh), NIL, NATIVE_BYTE_ORDER, 0, err);
  if (ILU_ERRNOK(*err))
    return ilu_FALSE;
  if ((_cdr_put_u8 (pk, (NATIVE_BYTE_ORDER == LittleEndian), err), ILU_ERRNOK(*err)) ||
      (_cdr_put_u8 (pk, IIOPDefaultMajorVersion, err), ILU_ERRNOK(*err)) ||
      (_cdr_put_u8 (pk, IIOPDefaultMinorVersion, err), ILU_ERRNOK(*err)) ||
      (_cdr_put_bytes (pk, (ilu_bytes) sbh, strlen(sbh) + 1, err), ILU_ERRNOK(*err)))
    return (ILU_ERROK(*err));
  ior->Profile[ior->nProfiles].tag = IIOP_TAG_ILU_IOP;
  _cdr_InmemFree (pk, &ior->Profile[ior->nProfiles].profileDataLen,
		  &ior->Profile[ior->nProfiles].profileData);
  ior->nProfiles += 1;
  return (ILU_ERROK(*err));
}

/* obj != NIL => Inside(server(obj), class(obj)); */
static ilu_Error IOROfObject (ilu_Object obj, struct IIOP_IOR_IOR **ior)
{
  ilu_Error err = ILU_INIT_NO_ERR;
  ilu_boolean found;
  struct {
    struct IIOP_IOR_IOR ior;
    struct IIOP_IOR_TaggedProfile profiles[10];
  } myior;
	
  Initialize(&err);
  if (ILU_ERRNOK(err)) return err;
	
  myior.ior.type_id = NIL;
  myior.ior.nProfiles = 0;
	
  if (obj == NIL)	/* NIL object reference */
    {
      myior.ior.type_id = "";
    }
  else
    {
      char	*sbh = ilu_SBHOfObject(obj);
      ilu_ProtocolInfo pinfo = NIL;
      ilu_TransportInfo tinfo = NIL;
      ilu_boolean owned;
		
      if (sbh == NIL || object_class(obj) == NIL)
	return ILU_ERR_CONS1(internal, &err, minor, ilu_im_broken, err);
      myior.ior.type_id = _ilu_Strdup(ilu_MstidOfObject(obj));
      if (ILU_ERRNOK(err)) return err;
      if (strncmp(object_ih(obj), CORBA_NATIVE_OBJECT_IH_PREFIX, SIZEOF_CORBA_NATIVE_OBJECT_IH_PREFIX) != 0)
	{
	  if (!_iiop_AddILUProfile (sbh, (struct IIOP_IOR_IOR *) &myior, &err))
	    return err;
	}
      found = FindServerIIOPPort(object_server(obj), &pinfo, &tinfo, &owned, &err);
      if (ILU_ERRNOK(err))
	return err;
#ifdef ILU_REQUIRE_IIOP_ACCESSIBLE_OBJECTS
      if (!found) {
	ILU_ERR_CONS1(inv_objref, &err, minor, ilu_iom_ps, ilu_FALSE);
	return err;
      }
#endif /* def ILU_REQUIRE_IIOP_ACCESSIBLE_OBJECTS */
      if (pinfo != NIL)
	{
	  (void) _iiop_AddIIOPProfile (object_server(obj), object_ih(obj),
				       myior.ior.type_id, pinfo, tinfo,
				       (struct IIOP_IOR_IOR *) &myior, &err);
	  if (owned) {
	    ilu_free(pinfo);
	    ilu_free(tinfo);
	  };
	  if (ILU_ERRNOK(err) ||
	      !_iiop_AddCharSetInfo ((struct IIOP_IOR_IOR *) &myior, &err))
	    return err;
	}
    }
  if (ILU_ERROK(err))
    {
      ilu_cardinal i;
		
      if (*ior == NIL)
	{
	  *ior = ilu_MallocE(sizeof(struct IIOP_IOR_IOR) + ((myior.ior.nProfiles - 1) * sizeof(struct IIOP_IOR_TaggedProfile)), &err);
	  if (ILU_ERRNOK(err)) return err;
	}
      if (*ior != NIL)
	{
	  (*ior)->type_id = myior.ior.type_id;
	  (*ior)->nProfiles = myior.ior.nProfiles;
	  if (myior.ior.nProfiles > 0)
	    {
	      (*ior)->Profile[0].tag = myior.ior.Profile[0].tag;
	      (*ior)->Profile[0].profileDataLen = myior.ior.Profile[0].profileDataLen;
	      (*ior)->Profile[0].profileData = myior.ior.Profile[0].profileData;
	    }
	  for (i = 1;  i < myior.ior.nProfiles;  i++)
	    {
	      (*ior)->Profile[i].tag = myior.profiles[i-1].tag;
	      (*ior)->Profile[i].profileDataLen = myior.profiles[i-1].profileDataLen;
	      (*ior)->Profile[i].profileData = myior.profiles[i-1].profileData;
	    }
	}
    }
	
  return err;
}

/* while holding server mutex of "s"... */
static ilu_Error IOROfInfo (ilu_Server s, ilu_string ih, ilu_string mstid,
			    ilu_ProtocolInfo pinfo, ilu_TransportInfo tinfo,
			    struct IIOP_IOR_IOR **ior)
{
  ilu_Error err = ILU_INIT_NO_ERR;
  struct {
    struct IIOP_IOR_IOR ior;
    struct IIOP_IOR_TaggedProfile profiles[10];
  } myior;

  Initialize(&err);
  if (ILU_ERRNOK(err)) return err;

  myior.ior.type_id = NIL;
  myior.ior.nProfiles = 0;

  if (ih == NIL)	/* NIL object reference */
    {
      myior.ior.type_id = "";
    }
  else
    {
      char *sbh = ilu_FormSBH (server_id(s), ih, mstid, pinfo, tinfo, &err);
      if (ILU_ERRNOK(err)) return err;

      if (sbh == NIL)
	return ILU_ERR_CONS1(internal, &err, minor, ilu_im_broken, err);
      myior.ior.type_id = ilu_StrdupE(mstid, &err);
      if (ILU_ERRNOK(err)) return err;
      if (strncmp(ih, CORBA_NATIVE_OBJECT_IH_PREFIX, SIZEOF_CORBA_NATIVE_OBJECT_IH_PREFIX) != 0)
	{
	  if (!_iiop_AddILUProfile (sbh, (struct IIOP_IOR_IOR *) &myior, &err))
	    return err;
	}
      if (strncmp(pinfo, "iiop_1_0_1", 10) == 0)
	{
	  if (!_iiop_AddIIOPProfile (s, ih, mstid, pinfo, tinfo, (struct IIOP_IOR_IOR *) &myior, &err))
	    return err;
	  if (!_iiop_AddCharSetInfo ((struct IIOP_IOR_IOR *) &myior, &err))
	    return err;
	}
    }
  if (ILU_ERROK(err))
    {
      ilu_cardinal i;

      if (*ior == NIL)
	{
	  *ior = ilu_MallocE(sizeof(struct IIOP_IOR_IOR) + ((myior.ior.nProfiles - 1) * sizeof(struct IIOP_IOR_TaggedProfile)), &err);
	  if (ILU_ERRNOK(err)) return err;
	}
      if (*ior != NIL)
	{
	  (*ior)->type_id = myior.ior.type_id;
	  (*ior)->nProfiles = myior.ior.nProfiles;
	  if (myior.ior.nProfiles > 0)
	    {
	      (*ior)->Profile[0].tag = myior.ior.Profile[0].tag;
	      (*ior)->Profile[0].profileDataLen = myior.ior.Profile[0].profileDataLen;
	      (*ior)->Profile[0].profileData = myior.ior.Profile[0].profileData;
	    }
	  for (i = 1;  i < myior.ior.nProfiles;  i++)
	    {
	      (*ior)->Profile[i].tag = myior.profiles[i-1].tag;
	      (*ior)->Profile[i].profileDataLen = myior.profiles[i-1].profileDataLen;
	      (*ior)->Profile[i].profileData = myior.profiles[i-1].profileData;
	    }
	}
    }
      
  return err;
}

static ilu_boolean parse_IIOP_Profile (struct IIOP_IOR_TaggedProfile *prof,
				       ilu_Class static_type,
				       ilu_string putative_type_id,
				       ilu_string *ih,
				       ilu_string *sid,
				       ilu_string *mstid,
				       ilu_string *cinfo,
				       ilu_cardinal *cinfolen,
				       ilu_Error *err)
{
  PACKET pk;
  ilu_shortcardinal port;
  ilu_byte major_version;
  ilu_byte minor_version;
  ilu_string	hostname = NIL;
  ilu_cardinal	hostname_len;
  ilu_bytes	object_key = NIL;
  ilu_cardinal	object_key_len;
  ilu_byte	byte_order_flag;
  enum byte_order bo;
  ilu_bytes	junk;
  ilu_cardinal	junklen;

  bo = (prof->profileData[0] == 0) ? BigEndian : LittleEndian;
  pk = _cdr_InmemPacket (prof->profileDataLen, prof->profileData, bo, 0, err);
  if (ILU_ERRNOK(*err))
    return ilu_FALSE;
  if ((_cdr_get_u8 (pk, &byte_order_flag, err), ILU_ERRNOK(*err)) ||
      (_cdr_get_u8 (pk, &major_version, err), ILU_ERRNOK(*err)) ||
      (_cdr_get_u8 (pk, &minor_version, err), ILU_ERRNOK(*err)) ||
      (_cdr_get_bytes (pk, (ilu_bytes *) &hostname, &hostname_len, 0xFFFF, err), ILU_ERRNOK(*err)) ||
      (_cdr_get_u16 (pk, &port, err), ILU_ERRNOK(*err)) ||
      (_cdr_get_bytes (pk, &object_key, &object_key_len, 0xFFFF, err), ILU_ERRNOK(*err)))
    {
#ifdef ENABLE_DEBUGGING
      ILU_NOTE(IIOP_DEBUG,
	    ("(iiop.c:parse_IIOP_Profile):  Invalid encapsulated profile detected:\n"));
      _ilu_debug_DumpPacket(prof->profileData, prof->profileDataLen, "encapsulated profile");
#endif
      _cdr_InmemFree(pk, &junklen, &junk);
      return (ilu_FALSE);
    }
  _cdr_InmemFree(pk, &junklen, &junk);

#ifdef ENABLE_DEBUGGING
  if ((ilu_DebugLevel & IIOP_DEBUG) != 0)
    {
      ilu_string ok2 = encode(object_key, object_key_len);
      ilu_DebugPrintf ("(iiop.c:parse_IIOP_Profile):  bo=%s, version=%d.%d, hostname=%s, port=%u, object_key=<%s>\n",
		       (bo == BigEndian) ? "BigEndian" : "LittleEndian", major_version, minor_version, hostname,
		       (unsigned int) port, ok2);
      ilu_free(ok2);
    }
#endif /* ENABLE_DEBUGGING */

  if (strcmp((ilu_string) object_key, "ilu") == 0)
    {
      /* We have an ILU object, go ahead and transform it */

      if (mstid != NIL)
	*mstid = _ilu_Strdup((ilu_string) object_key + 4);
      if (sid != NIL)
	*sid = _ilu_Strdup((ilu_string) object_key + 4 + strlen((ilu_string) object_key + 4) + 1);
      if (ih != NIL)
	*ih = _ilu_Strdup((ilu_string) object_key + 4 + strlen((ilu_string) object_key + 4) + 1 + strlen((ilu_string) object_key + 4 + strlen((ilu_string) object_key + 4) + 1) + 1);
      if (cinfo != NIL || cinfolen != NIL)
	{
	  char cinfobuf[1024];
	  sprintf (cinfobuf, "iiop_1_0_1%ctcp_%s_%lu", ILU_CINFO_DIVIDER, hostname, ((unsigned long) port) & 0xFFFF);
	  if (cinfo != NIL)
	    *cinfo = _ilu_Strdup(cinfobuf);
	  if (cinfolen != NIL)
	    *cinfolen = strlen(cinfobuf);
	}
      ilu_free(object_key);
      ilu_free(hostname);
      return ilu_TRUE;
    }
  else
    {
      /* Non-ILU object from some other ORB:
	 - Use function of cinfo and object-key for serverID
	 - Use object key for IH */

      char *encoded_key = NIL;

      if ((ih != NIL) || (cinfo != NIL)) {
	encoded_key = _ilu_EncodeBuffer((char *) object_key,
					object_key_len, err);
	if (ILU_ERRNOK(*err)) return ilu_FALSE;
	ILU_NOTE(IIOP_DEBUG,
		 ("(iiop.c:parse_IIOP_Profile):  encoded object key is <%s>\n",
		  encoded_key));
      };
      if (ih != NIL) {
	*ih = _ilu_Strcat3(CORBA_NATIVE_OBJECT_IH_PREFIX, encoded_key, NIL);
	if (*ih == NIL) {
	  ILU_ERR_CONS1(no_memory, err, nbytes, strlen(encoded_key) + SIZEOF_CORBA_NATIVE_OBJECT_IH_PREFIX + 1, ilu_FALSE);
	  ilu_free(encoded_key);
	  return ilu_FALSE;
	};
      };
      if (mstid != NIL) {
	/* if the type ID in the IOR (putative_type_id) is a subtype of the static type,
	   and if it has no subtypes itself, we'll trust it -- otherwise not */
	*mstid = NIL;
	if (static_type != NIL && putative_type_id != NIL) {
	  ilu_string ptypeID;
	  ilu_Class ptype;
	  if (strlen(putative_type_id) == 0)
	    ptypeID = "IDL:omg.org/CORBA/Object:1.0";
	  else
	    ptypeID = putative_type_id;
	  ptype = ilu_FindClassFromID(ptypeID);
	  if ((ptype != NIL) && ilu_IsSubObjectType(ptype, static_type) &&
	      (!ilu_HasSubtypes(ptype))) {
	    *mstid = _ilu_Strdup(putative_type_id);
	  }
	}
      }
      if (cinfo != NIL)
	{
	  static char cinfodivider[2] = { ILU_CINFO_DIVIDER, 0 };
	  char pinfobuf[1024];
	  char tinfobuf[1024];
	  char *pinfo;
	  sprintf (pinfobuf, "iiop_%u_%u_1_%s", major_version, minor_version, encoded_key);
	  pinfo = _ilu_EncodeBuffer(pinfobuf, strlen(pinfobuf), err);
	  sprintf (tinfobuf, "tcp_%s_%lu", hostname, ((unsigned long) port) & 0xFFFF);
	  *cinfo = _ilu_Strcat3(pinfo, cinfodivider, tinfobuf);
	  if (ILU_ERRNOK(*err)) {
	    ilu_free(encoded_key);
	    return ilu_FALSE;
	  };
	  if (cinfolen != NIL)
	    *cinfolen = strlen(*cinfo);
	  ILU_NOTE(IIOP_DEBUG,
		   ("(iiop.c:parse_IIOP_Profile):  non-native cinfo is <%s>\n", *cinfo));
	}
      if (encoded_key != NIL)
	ilu_free(encoded_key);
      if (sid != NIL)
	{
	  *sid = ilu_IIOP_ServerIDFromObjectKey(object_key, object_key_len, hostname, port,
						major_version, minor_version, err);
	  if (ILU_ERRNOK(*err))
	    return ilu_FALSE;
	}
      ILU_CLER(*err);
      return ilu_TRUE;
    }
}

static ilu_boolean parse_ILU_Profile (struct IIOP_IOR_TaggedProfile *prof,
				      ilu_string *ih,
				      ilu_string *sid,
				      ilu_string *mstid,
				      ilu_string *cinfo,
				      ilu_cardinal *cinfolen,
				      ilu_Error *err)
{
  PACKET pk;
  ilu_byte major_version;
  ilu_byte minor_version;
  ilu_string	object_key = NIL;
  ilu_cardinal	object_key_len;
  ilu_byte	byte_order_flag;
  enum byte_order bo;
  ilu_bytes	junk;
  ilu_cardinal	junklen;

  bo = (prof->profileData[0] == 0) ? BigEndian : LittleEndian;
  pk = _cdr_InmemPacket (prof->profileDataLen, prof->profileData, bo, 0, err);
  if (ILU_ERRNOK(*err))
    return ilu_FALSE;
  if ((_cdr_get_u8 (pk, &byte_order_flag, err), ILU_ERRNOK(*err)) ||
      (_cdr_get_u8 (pk, &major_version, err), ILU_ERRNOK(*err)) ||
      (_cdr_get_u8 (pk, &minor_version, err), ILU_ERRNOK(*err)) ||
      (_cdr_get_bytes (pk, (ilu_bytes *) &object_key, &object_key_len, 0xFFFF, err), ILU_ERRNOK(*err)))
    {
#ifdef ENABLE_DEBUGGING
      ILU_NOTE(IIOP_DEBUG,
	    ("(iiop.c:parse_ILU_Profile):  Invalid encapsulated profile detected:\n"));
      _ilu_debug_DumpPacket(prof->profileData, prof->profileDataLen, "encapsulated profile");
#endif /* ENABLE_DEBUGGING */
      _cdr_InmemFree(pk, &junklen, &junk);
      return (ilu_FALSE);
    }
  _cdr_InmemFree(pk, &junklen, &junk);

  if ((strncmp(object_key, "ilusbh:", 7) == 0)
#ifndef ILU_DISALLOW_OLD_SBH_FORM
      || (strncmp(object_key, "ilu:", 4) == 0)
#endif
      ) {
      /* We have an ILU object, go ahead and transform it */
      ilu_string lcinfo;
      ilu_cardinal lcinfolen;
      ilu_boolean cinfopass;

      if (!ilu_ParseSBH ((ilu_string) object_key, ih, sid, mstid, &lcinfo, &lcinfolen, &cinfopass, err))
	{
	  ilu_free(object_key);
	  return ilu_FALSE;
	}
      if (cinfo != NIL) {
	if (cinfopass)
	  *cinfo = lcinfo;
	else {
	  *cinfo = ilu_MallocE(lcinfolen+1, err);
	  if (ILU_ERRNOK(*err))
	    {
	      ilu_free(object_key);
	      return ilu_FALSE;
	    }
	  memcpy (*cinfo, lcinfo, lcinfolen);
	  (*cinfo)[lcinfolen] = 0;
	}
      }
      if (cinfolen != NIL)
	*cinfolen = lcinfolen;
      ilu_free(object_key);
      return ilu_TRUE;
    }
  else
    {
      ILU_NOTE(IIOP_DEBUG,
	    ("parse_ILU_Profile:  non-ILU URL <%s>.\n", object_key));
      ilu_free(object_key);
      return ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_sbh, ilu_FALSE);
    }
}

static ilu_boolean parse_MultiComponent_Profile (struct IIOP_IOR_TaggedProfile *prof,
						 ilu_cardinal *short_character_codeset,
						 ilu_cardinal *long_character_codeset,
						 ilu_Error *err)
{
  PACKET pk;
  PACKET codesetInfo;
  ilu_byte major_version;
  ilu_byte minor_version;
  ilu_byte	byte_order_flag;
  enum byte_order bo;
  ilu_cardinal	ncomponents, i, j, component_tag;
  ilu_bytes	junk = NIL;
  ilu_cardinal	nativeCodesetForShortChar = 0, nativeCodesetForChar = 0;
  ilu_cardinal	nConverters;
  ilu_cardinal	junklen;

  bo = (prof->profileData[0] == 0) ? BigEndian : LittleEndian;
  pk = _cdr_InmemPacket (prof->profileDataLen, prof->profileData, bo, 0, err);
  if (ILU_ERRNOK(*err))
    return ilu_FALSE;
  if ((_cdr_get_u8 (pk, &byte_order_flag, err), ILU_ERRNOK(*err)) ||
      (_cdr_get_u8 (pk, &major_version, err), ILU_ERRNOK(*err)) ||
      (_cdr_get_u8 (pk, &minor_version, err), ILU_ERRNOK(*err)) ||
      (_cdr_get_u32 (pk, &ncomponents, err), ILU_ERRNOK(*err)))
    goto errout;
  ILU_NOTE(IIOP_DEBUG,
	   ("(iiop.c:parse_MultiComponent_Profile):  profile contains %u component%s\n",
	    ncomponents, (ncomponents == 1) ? "" : "s"));

  for (i = 0;  i < ncomponents;  i++) {
    junk = NIL;
    if ((_cdr_get_u32 (pk, &component_tag, err), ILU_ERRNOK(*err)) ||
	(_cdr_get_bytes (pk, &junk, &junklen, 0xFFFF, err), ILU_ERRNOK(*err)))
      goto errout;
    ILU_NOTE(IIOP_DEBUG,
	     ("(iiop.c:parse_MultiComponent_Profile):  component %u of type %u, %u bytes\n",
	     i + 1, component_tag, junklen));
    if (component_tag == IIOP_TAG_CODE_SETS) {
      codesetInfo = _cdr_InmemPacket (junklen, junk, (junk[0] == 0) ? BigEndian : LittleEndian, 0, err);
      if (ILU_ERRNOK(*err))
	return ilu_FALSE;
      if ((_cdr_get_u8 (codesetInfo, &byte_order_flag, err), ILU_ERRNOK(*err)) ||
	  (_cdr_get_u8 (codesetInfo, &major_version, err), ILU_ERRNOK(*err)) ||
	  (_cdr_get_u8 (codesetInfo, &minor_version, err), ILU_ERRNOK(*err)) ||
	  (_cdr_get_u32 (codesetInfo, &nativeCodesetForShortChar, err), ILU_ERRNOK(*err)) ||
	  (_cdr_get_u32 (codesetInfo, &nConverters, err), ILU_ERRNOK(*err)))
	goto errout2;
      ILU_NOTE(IIOP_DEBUG,
	       ("(iiop.c:parse_MultiComponent_Profile):  native codeset for SHORT CHARACTER is %08x, with %u converters\n",
		nativeCodesetForShortChar, nConverters));
      if (nativeCodesetForShortChar == ilu_IIOP_NATIVE_SHORT_CHAR_CODE_SET_ID)
	*short_character_codeset = nativeCodesetForShortChar;
      for (j = 0;  j < nConverters;  j++) {
	if (_cdr_get_u32 (codesetInfo, &junklen, err), ILU_ERRNOK(*err))
	  goto errout2;
	if (junklen == ilu_IIOP_NATIVE_SHORT_CHAR_CODE_SET_ID)
	  *short_character_codeset = nativeCodesetForShortChar;
	ILU_NOTE(IIOP_DEBUG,
		 ("(iiop.c:parse_MultiComponent_Profile):    SHORT CHARACTER codeset converter for %08x\n",
		  junklen));
      }
      if ((_cdr_get_u32 (codesetInfo, &nativeCodesetForChar, err), ILU_ERRNOK(*err)) ||
	  (_cdr_get_u32 (codesetInfo, &nConverters, err), ILU_ERRNOK(*err)))
	goto errout2;
      ILU_NOTE(IIOP_DEBUG,
	       ("(iiop.c:parse_MultiComponent_Profile):  native codeset for CHARACTER is %08x, with %u converters\n",
		nativeCodesetForChar, nConverters));
      if (nativeCodesetForChar == ilu_IIOP_NATIVE_CHAR_CODE_SET_ID)
	*long_character_codeset = nativeCodesetForChar;
      for (j = 0;  j < nConverters;  j++) {
	if (_cdr_get_u32 (codesetInfo, &junklen, err), ILU_ERRNOK(*err))
	  goto errout2;
	if (junklen == ilu_IIOP_NATIVE_CHAR_CODE_SET_ID)
	  *long_character_codeset = nativeCodesetForChar;
	ILU_NOTE(IIOP_DEBUG,
		 ("(iiop.c:parse_MultiComponent_Profile):    CHARACTER codeset converter for %08x\n",
		  junklen));
      }
      /* allow free to free "junk" */
      _cdr_InmemFree(codesetInfo, NIL, NIL);
      ILU_NOTE(IIOP_DEBUG,
	       ("(iiop.c:parse_MultiComponent_Profile):  ILU can%s interoperate with these codesets\n",
		((*short_character_codeset != 0) && (*long_character_codeset != 0)) ? "" : "not"));
    } else {
      ilu_free (junk);
    }
  }
  
  /* retain prof->profileData */
  _cdr_InmemFree(pk, &junklen, &junk);
  return ilu_TRUE;

  errout2:
    ILU_NOTE(IIOP_DEBUG,
	     ("(iiop.c:parse_MultiComponent_Profile):  Bad component\n"));
    _cdr_InmemFree(codesetInfo, &junklen, &junk);

  errout:
#ifdef ENABLE_DEBUGGING
    ILU_NOTE(IIOP_DEBUG,
	     ("(iiop.c:parse_MultiComponent_Profile):  Invalid encapsulated profile detected:\n"));
    _ilu_debug_DumpPacket(prof->profileData, prof->profileDataLen, "multi-component profile");
#endif /* ENABLE_DEBUGGING */
    _cdr_InmemFree(pk, &junklen, &junk);
    return (ilu_FALSE);
}

static          ilu_boolean
  IsOfTypeViaRPC(ilu_Object o, ilu_Class type,
		 ILU_ERRS((bad_locks, inv_objref,
			   no_resources, IoErrs)) * err)
{
  ilu_Call_s      call_s;
  ilu_Call        call = &call_s;
  ilu_cardinal    reqSize;
  ilu_cardinal    estatus = 0;
  ilu_ProtocolException pe;
  ilu_string	  id = class_unique_id(type);
  ilu_cardinal idlen = strlen(class_unique_id(type));
  ilu_Server      s = object_server(o);
  ilu_boolean	result = ilu_FALSE;
  ilu_Connection  newconn = NIL;
  extern ilu_Method	_ilu_IsAMethod;
  ILU_NOTE(OBJECT_DEBUG,
	("(iiop.c:IsOfTypeViaRPC):  object %p, type \"%s\"...\n",
	 o, class_name(type)));
  (void) ilu_StartCall(call, s, _ilu_rootClass, _ilu_IsAMethod, 0, NIL,
		       &newconn, err);
 retry:
  if (newconn != NIL)
    (void) _ilu_HandOffNewConnection(newconn, err);
  if (ILU_ERRNOK(*err))
    return ilu_FALSE;
  _ilu_AcquireServerMutex(s);
  reqSize = ilu_SizeOfObjectID(call, o, ilu_TRUE, _ilu_rootClass, err);
  _ilu_ReleaseServerMutex(s);
  if (ILU_ERRNOK(*err))
    goto faild;
  reqSize += ilu_SizeOfString (call, id, idlen, 0xFFFF, err);
  if (ILU_ERRNOK(*err))
    goto faild;
  if (!ilu_StartRequest(call, reqSize, err))
    goto faild;
  ilu_EnterServer(s, object_class(o));
  ilu_OutputObjectID(call, o, ilu_TRUE, _ilu_rootClass, err);
  if (ILU_ERRNOK(*err))
    goto faild;
  ilu_OutputString(call, id, idlen, 0xFFFF, err);
  if (ILU_ERRNOK(*err))
    goto faild;
  if (!ilu_FinishRequest(call, err))
    goto faild;
  pe = ilu_GetReply(call, &estatus, &newconn, err);
  if (ILU_ERRNOK(*err) &&
      (err->ilu_type == ILU_ERRTYP(transient)) &&
      (ILU_ERRSEL(transient,*err).minor == ilu_tm_retry)) {
    ILU_HANDLED(*err);
    ILU_CLER(*err);
    goto retry;
  };
  if (pe != ilu_ProtocolException_Success) {
    (void) ILU_ERR_CONS1(internal, err, minor, ilu_im_callFail, 0);
    goto faild;
  }
  if (estatus != 0) {
    (void) ILU_ERR_CONS1(internal, err, minor, ilu_im_callFail, 0);
    goto faild;
  }
  ilu_InputBoolean(call, &result, err);
  if (ILU_ERRNOK(*err))
    goto faild;
  ilu_ReplyRead(call, err);
  if (ILU_ERRNOK(*err))
    goto faild;
faild:
  ilu_FinishCall(call, err);
  if (ILU_ERRNOK(*err)) {
    ILU_HANDLED(*err);
  }
  return result;
}

typedef struct {
  ilu_Object obj;
  ilu_Class mstid_candidate;
  ilu_boolean abort;
} CheckTypeData;

/* L1 > otmu */
static void CheckTypeViaIsA (ilu_Class type, ilu_refany rock)
{
  ilu_Error err;
  CheckTypeData *d = (CheckTypeData *) rock;
  extern ilu_boolean _ilu_IsSubObjectType (ilu_Class, ilu_Class);

  if (d->abort ||
      (d->mstid_candidate == type) ||
      !_ilu_IsSubObjectType (type, d->mstid_candidate))
    return;
  if (IsOfTypeViaRPC(d->obj, type, &err) && ILU_ERROK(err))
    d->mstid_candidate = type;
  else
    d->abort = ILU_ERRNOK(err);
  ILU_HANDLED(err);
}

ilu_Class 
_ilu_IIOP_FindClassViaRPC(ilu_Object o)
{
  ilu_Class       c = NIL;
  ilu_Class       pclass = object_class(o);
  ILU_ERRS((bad_locks, inv_objref, no_resources, IoErrs)) lerr;
  CheckTypeData	d;

  if ((pclass == ilu_rootClass) &&
      ((c = ilu_FindClassFromID (CORBA_OBJECT_TYPE_ID)) != NIL))
    pclass = c;

  if (class_singleton(pclass)) {
    ILU_NOTE(IIOP_DEBUG | OBJECT_DEBUG,
	  ("%s %s is singleton, not attempting to figure the real types via an RPC call.\n",
	   "_ilu_IIOP_FindClassViaRPC:  pclass", class_name(pclass)));
    return (NIL);
  }
  else
    {
      ILU_NOTE(IIOP_DEBUG | OBJECT_DEBUG,
	    ("_ilu_IIOP_FindClassViaRPC(%p \"%s\" \"%s\")\n",
	     o, server_id(object_server(o)), object_ih(o)));
    }
  _ilu_Assert(!server_is_true(object_server(o)),
	      "_ilu_IIOP_FindClassViaRPC: called on true object");

  /* Make sure object is of at least the putative type */
  if (!IsOfTypeViaRPC(o, pclass, &lerr) || ILU_ERRNOK(lerr))
    {
      ILU_HANDLED(lerr);
      return NIL;
    }

  /* Now check other types to see if the object is some subtype of the putative type */
  d.obj = o;
  d.mstid_candidate = pclass;
  d.abort = ilu_FALSE;
  _ilu_EnumerateClasses (CheckTypeViaIsA, (ilu_refany) &d);
  if (d.abort)
    return NIL;
  else
    return d.mstid_candidate;
}

static ilu_boolean ParseIOR (struct IIOP_IOR_IOR *ior,
			     ilu_Class static_type, /* may be NIL */
			     ilu_string *ih,
			     ilu_string *sid,
			     ilu_string *mstid,
			     ilu_string *cinfo,
			     ilu_cardinal *cinfolen,
			     ilu_cardinal *short_char_codeset,
			     ilu_cardinal *char_codeset,
			     ilu_Error *err)
{
  ilu_cardinal i;
  ilu_boolean stat = ilu_FALSE;

  ILU_CLER(*err);

  if (ior->nProfiles == 0)
    return (ilu_FALSE);
  else
    {
      for (i = 0;  i < ior->nProfiles;  i++)
	{
	  if (ior->Profile[i].tag == IIOP_TAG_ILU_IOP)
	    {
	      _ilu_Assert((ior->type_id != NIL && ((int) strlen(ior->type_id) > 0)),
			  "Bad type_id in IOR with ILU profile");
	      /* Note that, due to lack of rigor by the writers of the IIOP,
		 and lack of agreement among ORB implementors, we can't trust
		 type ID values unless they come from an ILU orb.  Sigh...
	       */
	      if (mstid != NIL)
		*mstid = _ilu_Strdup(ior->type_id);
	      stat = parse_ILU_Profile (&ior->Profile[i],
					ih, sid, NIL, cinfo, cinfolen, err);
	      if (stat) {
		if ((short_char_codeset != NIL) && (*short_char_codeset == 0))
		  *short_char_codeset = ilu_IIOP_NATIVE_SHORT_CHAR_CODE_SET_ID;
		if ((char_codeset != NIL) && (*char_codeset == 0))
		  *char_codeset = ilu_IIOP_NATIVE_CHAR_CODE_SET_ID;
		break;
	      };
	    }
	}
      if (! stat) {
	/* no ILU profile; perhaps there's an IIOP one that we can use */
	for (i = 0;  i < ior->nProfiles;  i++)
	  {
	    if (ior->Profile[i].tag == IIOP_TAG_INTERNET_IOP)
	      {
		stat = parse_IIOP_Profile (&ior->Profile[i],
					   static_type, ior->type_id,
					   ih, sid, mstid, cinfo, cinfolen, err);
		if (!stat)
		  return ilu_FALSE;
	      }
	    else if (ior->Profile[i].tag == IIOP_TAG_MULTIPLE_COMPONENT_IOP)
	      {
		stat = parse_MultiComponent_Profile (&ior->Profile[i],
						     short_char_codeset,
						     char_codeset,
						     err);
		if (!stat)
		  return ilu_FALSE;
	      }
	  }
      };
      /* XXX -- if no character codesets were specified, use defaults */
      if ((short_char_codeset != NIL) && (*short_char_codeset == 0))
	*short_char_codeset = ilu_IIOP_NATIVE_SHORT_CHAR_CODE_SET_ID;
      /* XXX -- this is wrong; it should be 0, but that means that we should
	 check all interfaces of the object being reified to see that they
	 don't use wchar or wstring, and that's too expensive */
      if ((char_codeset != NIL) && (*char_codeset == 0))
	*char_codeset = ilu_IIOP_NATIVE_CHAR_CODE_SET_ID;

      /* if no mstid was discovered, see if the specified mstid can be trusted */
      /* XXX this is for testing only! */
#ifdef XEROX_FIREWALL_HANDLING
      if (getenv("ILU_IIOP_NO_IS_A_TEST") != NIL) {
	if ((mstid != NIL) && (*mstid == NIL)) {
	  *mstid = _ilu_Strdup(ior->type_id);
	}
      }
#endif
    }
  if (!stat)
    return ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_ior, ilu_FALSE);
  else
    return ilu_TRUE;
}

static ilu_Object ObjectOfIOR (struct IIOP_IOR_IOR *ior,
			       ilu_Class static_type,
			       ilu_Error *err)
{
  ilu_string ih = NIL, sid = NIL, mstid = NIL, cinfo = NIL;
  ilu_cardinal cinfolen;
  ilu_Error lerr;
  ilu_Server s = NIL;
  ilu_Object h = NIL;
  ilu_Class foundclass = NIL;
  ilu_cardinal short_char_codeset = 0;
  ilu_cardinal char_codeset = 0;
  int codeset_err;

  Initialize(err);
  if (ILU_ERRNOK(*err)) return NIL;

  if (ior->nProfiles == 0)
    {
      ILU_CLER(*err);
      return NIL;
    }
  else if (ParseIOR(ior, static_type, &ih, &sid, &mstid, &cinfo, &cinfolen, &short_char_codeset, &char_codeset, err))
    {
      if (((short_char_codeset != ilu_IIOP_NATIVE_SHORT_CHAR_CODE_SET_ID) &&
	   (codeset_err = ilu_iom_short_char_codeset)) ||
	  /* XXX - this next clause is wrong; we should really allow a value
	     of 0 as well, but that means we'd have to grovel over the interfaces
	     of the object to ensure they don't use wchar or wstring, and that's
	     too painful. */
	  ((char_codeset != ilu_IIOP_NATIVE_CHAR_CODE_SET_ID) &&
	   (codeset_err = ilu_iom_char_codeset))) {
	ilu_free(sid);
	ilu_free(cinfo);
	ilu_free(ih);
	ilu_free(mstid);
	return ILU_ERR_CONS1(inv_objref, err, minor, codeset_err, NIL);
      };

      s = _ilu_FindAndEnterServer(sid, ilu_TRUE, cinfo, cinfolen,
				  static_type, &lerr);
      ilu_free (sid);
      ilu_free (cinfo);
      if (ILU_ERRNOK(lerr))
	{
	  ILU_HANDLED(lerr);
	  ilu_free(ih);
	  ilu_free(mstid);
	  return ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_sid, NIL);
	}
      else if (s != NIL)
	{
	  h = _ilu_FindObjectInServer (ih, s);
	  if (mstid != NIL)
	    foundclass = ilu_FindClassFromID(mstid);
	  if (h == NIL)
	    h = _ilu_FindOrCreateObject (ih, s, foundclass,
					 static_type, mstid, NIL, err);
	  ilu_free(ih);
	  ilu_free(mstid);
	  if (ILU_ERRNOK(*err))
	    return NIL;
	}
      if (h == NIL)
	{
	  return ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_ih, NIL);
	}
      else
	return (h);
    }
  else
    return ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_ior, NIL);
}

ilu_boolean _ilu_IIOP_ParseIIOP (ilu_string iiop, ilu_string *ih, ilu_string *sid,
				 ilu_string *mstid, ilu_string *cinfo, ilu_cardinal *cinfolen,
				 ilu_boolean *pass_cinfo, ilu_Error *err)
{
  /* Parse Sun IIOP style URL:
   *
   * iiop1.0://<host>:<port>/<key>
   */

  char *p;
  char hostname[1024];
  unsigned long port;
  char object_key[1024];

  if (strncmp(iiop, "iiop:1.0/", 9) == 0)
    p = iiop + 8;
  else if (strncmp(iiop, "iiop1.0:/", 9) == 0)
    p = iiop + 8;
  else
    return ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_ior, ilu_FALSE);

  if (sscanf (p, "//%1023[^:]:%lu/%1023s", hostname, &port, object_key) != 3)
    return ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_ior, ilu_FALSE);
  else if (strcmp(object_key, "ilu") == 0)   /* ILU object */
    {
      /* XXX -- broken, shouldn't object key be encoded? */

      /* We have an ILU object, go ahead and transform it */

      if (mstid != NIL)
	*mstid = _ilu_Strdup(object_key + 4);
      if (sid != NIL)
	*sid = _ilu_Strdup(object_key + 4 + strlen(object_key + 4) + 1);
      if (ih != NIL)
	*ih = _ilu_Strdup(object_key + 4 + strlen(object_key + 4) + 1 + strlen(object_key + 4 + strlen(object_key + 4) + 1) + 1);
      if (cinfo != NIL || cinfolen != NIL)
	{
	  char cinfobuf[1024];
	  sprintf (cinfobuf, "iiop_1_0_1%ctcp_%s_%lu", ILU_CINFO_DIVIDER, hostname,
		   ((unsigned long) port) & 0xFFFF);
	  if (cinfo != NIL)
	    *cinfo = _ilu_Strdup(cinfobuf);
	  if (cinfolen != NIL)
	    *cinfolen = strlen(cinfobuf);
	  *pass_cinfo = ilu_TRUE;
	}
      else if (pass_cinfo != NIL)
	*pass_cinfo = ilu_FALSE;
      ilu_free(hostname);
      return ilu_TRUE;
    }
  else
    {
      char *encoded_key = NIL;
      ilu_cardinal object_key_len = strlen(object_key);
      if ((ih != NIL) || (cinfo != NIL)) {
	encoded_key = _ilu_EncodeBuffer(object_key, object_key_len, err);
	if (ILU_ERRNOK(*err)) return ilu_FALSE;
      };
      if (ih != NIL) {
	*ih = _ilu_Strcat3(CORBA_NATIVE_OBJECT_IH_PREFIX, encoded_key, NIL);
	if (*ih == NIL) {
	  ILU_ERR_CONS1(no_memory, err, nbytes, strlen(encoded_key) + SIZEOF_CORBA_NATIVE_OBJECT_IH_PREFIX + 1, ilu_FALSE);
	  ilu_free(encoded_key);
	  return ilu_FALSE;
	};
      };
      if (sid != NIL) {
	*sid = ilu_IIOP_ServerIDFromObjectKey((ilu_bytes) object_key, object_key_len,
					      hostname, port, 1, 0, err);
	if (ILU_ERRNOK(*err))
	  return ilu_FALSE;
      };
      if (mstid != NIL)
	*mstid = NIL;
      if (cinfo != NIL)
	{
	  static char cinfodivider[2] = { ILU_CINFO_DIVIDER, 0 };
	  char pinfobuf[1024];
	  char tinfobuf[1024];
	  char *pinfo;
	  sprintf (pinfobuf, "iiop_1_0_1_%s", encoded_key);
	  pinfo = _ilu_EncodeBuffer(pinfobuf, strlen(pinfobuf), err);
	  sprintf (tinfobuf, "tcp_%s_%lu", hostname, ((unsigned long) port) & 0xFFFF);
	  *cinfo = _ilu_Strcat3(pinfo, cinfodivider, tinfobuf);
	  if (ILU_ERRNOK(*err)) {
	    ilu_free(encoded_key);
	    return ilu_FALSE;
	  };
	  if (cinfolen != NIL)
	    *cinfolen = strlen(*cinfo);
	  *pass_cinfo = ilu_TRUE;
	}
      else if (pass_cinfo != NIL)
	*pass_cinfo = ilu_FALSE;
      if (encoded_key != NIL)
	ilu_free(encoded_key);
      return ilu_TRUE;
    }
}

static unsigned int
strcharcount (char *str, char chr)
{
  register unsigned int count = 0;
  register char *p;
  for (p = str;  *p;  p++)
    if (*p == chr) count++;
  return count;
}

ilu_boolean _ilu_IIOP_ParseIIOPLoc (ilu_string iiop, ilu_string *ih, ilu_string *sid,
				    ilu_string *mstid, ilu_string *cinfo, ilu_cardinal *cinfolen,
				    ilu_boolean *pass_cinfo, ilu_Error *err)
{
  /* Parse INS-style URL:
   *
   * iioploc://[iiopvers@]<host>[:<port>]/<key>
   */

  char *p;
  char *decoded_key = NIL;
  char addresses[1024];
  char object_key[1024];
  char sid_hostname[1024];
  unsigned int sid_port, key_len;
  unsigned int sid_major, sid_minor;

  if (strncmp(iiop, "iioploc:/", 9) == 0)
    p = iiop + 8;
  else if (strncmp(iiop, "iioploc:/", 9) == 0)
    p = iiop + 8;
  else
    return ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_ior, ilu_FALSE);

  if (sscanf (p, "//%1023[^/]/%1023s", addresses, object_key) != 2)
    return ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_ior, ilu_FALSE);
  else if ((strlen(addresses) < 1) || (strlen(object_key) < 1))
    return ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_ior, ilu_FALSE);
  ILU_NOTE(IIOP_DEBUG,
	   ("_ilu_IIOP_ParseIIOPLoc:  addresses are <%s>, key is <%s>\n",
	    addresses, object_key));
  decoded_key = _ilu_DecodeBuffer(object_key, strlen(object_key), &key_len, err);
  if (ILU_ERRNOK(*err)) return ilu_FALSE;
  if (strcmp(decoded_key, "ilu") == 0)   /* ILU object */
    {
      /* We have an ILU object, go ahead and transform it */

      ilu_cardinal comma_count = strcharcount(addresses, ',') + 1;

      if (mstid != NIL)
	*mstid = _ilu_Strdup(decoded_key + 4);
      if (sid != NIL)
	*sid = _ilu_Strdup(decoded_key + 4 + strlen(decoded_key + 4) + 1);
      if (ih != NIL)
	*ih = _ilu_Strdup(decoded_key + 4 + strlen(decoded_key + 4) + 1 + strlen(decoded_key + 4 + strlen(decoded_key + 4) + 1) + 1);
      if (cinfo != NIL)
	{
	  static char cinfodivider[2] = { ILU_CINFO_DIVIDER, 0 };
	  static char cinfomarker[2] = { ILU_CINFO_MARKER, 0 };
	  char pinfobuf[1024];
	  char tinfobuf[1024];
	  char hostname[1024];
	  int port;
	  int major_version, minor_version;
	  char *pinfo;
	  char *addr_ptr = addresses;
	  char *comma;
	  *cinfo = NULL;
	  if (ILU_ERRNOK(*err)) goto errout;
	  /* we'll just do one address, though we could build a multi-part cinfo
	     which has all the addresses listed in "address" */
	  comma = strchr(addr_ptr, ',');
	  if (comma) *comma = 0;
	  major_version = 1;
	  minor_version = 0;
	  port = 9999;
	  if ((sscanf (addr_ptr, "%u.%u@%1024[^:]:%u", &major_version, &minor_version, hostname, &port) != 4) &&
	      (sscanf (addr_ptr, "%u.%u@%1024[^:]", &major_version, &minor_version, hostname) != 3) &&
	      (sscanf (addr_ptr, "%1024[^:]:%u", hostname, &port) != 2) &&
	      (sscanf (addr_ptr, "%1024[^:]", hostname) != 1)) {
	    ILU_NOTE(IIOP_DEBUG,
		     ("_ilu_IIOP_ParseIIOPLoc:  bad cinfo in URL <%s>\n", iiop));
	    ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_ci, ilu_FALSE);
	    goto errout;
	  };
	  sprintf (pinfobuf, "iiop_%u_%u_1@tcp_%s_%lu", major_version, minor_version,
		     hostname, port);
	  *cinfo = ilu_StrdupE(pinfobuf, err);
	  if (ILU_ERRNOK(*err)) goto errout;
	  if (cinfolen != NIL)
	    *cinfolen = strlen(*cinfo);
	  *pass_cinfo = ilu_TRUE;
	}
      else if (pass_cinfo != NIL)
	*pass_cinfo = ilu_FALSE;
      ilu_free(decoded_key);
      return ilu_TRUE;
    }
  else
    {
      ilu_cardinal comma_count = strcharcount(addresses, ',') + 1;

      if (ih != NIL) {
	*ih = _ilu_Strcat3(CORBA_NATIVE_OBJECT_IH_PREFIX, object_key, NIL);
	if (*ih == NIL) {
	  ILU_ERR_CONS1(no_memory, err, nbytes, strlen(object_key) + SIZEOF_CORBA_NATIVE_OBJECT_IH_PREFIX + 1, ilu_FALSE);
	  goto errout;
	};
      };
      if (mstid != NIL) {
	*mstid = NIL;
      };      
      if (cinfo != NIL)
	{
	  static char cinfodivider[2] = { ILU_CINFO_DIVIDER, 0 };
	  static char cinfomarker[2] = { ILU_CINFO_MARKER, 0 };
	  char pinfobuf[1024];
	  char tinfobuf[1024];
	  char hostname[1024];
	  int port;
	  int major_version, minor_version;
	  char *pinfo;
	  char *addr_ptr;
	  char *comma;
	  *cinfo = NULL;
	  if (ILU_ERRNOK(*err)) goto errout;
	  for (addr_ptr = addresses;  comma_count > 0;  comma_count--) {
	    comma = strchr(addr_ptr, ',');
	    if (comma) *comma = 0;
	    major_version = 1;
	    minor_version = 0;
	    port = 9999;
	    if ((sscanf (addr_ptr, "%u.%u@%1024[^:]:%u", &major_version, &minor_version, hostname, &port) != 4) &&
		(sscanf (addr_ptr, "%u.%u@%1024[^:]", &major_version, &minor_version, hostname) != 3) &&
		((major_version = 1, minor_version = 0, sscanf (addr_ptr, "%1024[^:]:%u", hostname, &port)) != 2) &&
		(sscanf (addr_ptr, "%1024[^:]", hostname) != 1)) {
	      ILU_NOTE(IIOP_DEBUG,
		       ("_ilu_IIOP_ParseIIOPLoc:  bad cinfo in URL <%s>\n", iiop));
	      ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_ci, ilu_FALSE);
	      goto errout;
	    };
	    if (sid != NIL) {
	      strcpy (sid_hostname, hostname);
	      sid_port = port;
	      sid_major = major_version;
	      sid_minor = minor_version;
	    };
	    ILU_NOTE(IIOP_DEBUG,
		     ("_ilu_IIOP_ParseIIOPLoc:  cinfo:  IIOP %u.%u at %s:%u\n",
		      major_version, minor_version, hostname, port));
	    sprintf (pinfobuf, "iiop_%u_%u_1_%s", major_version, minor_version, object_key);
	    pinfo = _ilu_EncodeBuffer(pinfobuf, strlen(pinfobuf), err);
	    sprintf (tinfobuf, "tcp_%s_%lu", hostname, ((unsigned long) port) & 0xFFFF);
	    if (*cinfo)
	      *cinfo = _ilu_Strcat5 (*cinfo, cinfomarker, pinfo, cinfodivider, tinfobuf);
	    else
	      *cinfo = _ilu_Strcat3(pinfo, cinfodivider, tinfobuf);
	    if (ILU_ERRNOK(*err)) goto errout;
	    if (cinfolen != NIL)
	    *cinfolen = strlen(*cinfo);
	    *pass_cinfo = ilu_TRUE;
	    if (comma) addr_ptr = comma + 1;
	  }
	}
      else if (pass_cinfo != NIL)
	*pass_cinfo = ilu_FALSE;
      if (sid != NIL) {
	*sid = ilu_IIOP_ServerIDFromObjectKey((ilu_bytes) decoded_key, key_len,
					      sid_hostname, sid_port,
					      sid_major, sid_minor, err);
	if (ILU_ERRNOK(*err))
	  goto errout;
      };
      ilu_free(decoded_key);
      return ilu_TRUE;
    }
 errout:
  if (decoded_key != NIL)
    ilu_free(decoded_key);
  return ilu_FALSE;
}

typedef struct NameComponent_s {
  char *	id;
  char *	kind;
} NameComponent_s;

typedef struct NameList_s {
  int			nnames;
  NameComponent_s *	names;
} * NameList;

static int
  CountNameListComponents (ilu_string n)
{
  int count = 0;
  char *p = n;
  if (*p == '/') p++;
  while (*p != 0) {
    if (*p == '\\')
      p++;
    else if (*p == '/')
      count++;
    p++;
  }
  return count + 1;
}

static char *FindComponentEnd (ilu_string n)
{
  char *p = n;
  while (*p != 0) {
    if (*p == '\\')
      p++;
    else if (*p == '/')
      return p;
    p++;
  }
  return NIL;
}

static char *FindComponentIDField (ilu_string n)
{
  char *p = n;
  while (*p != 0) {
    if (*p == '\\')
      p++;
    else if (*p == '.')
      return p;
    p++;
  }
  return NIL;
}

static NameList
  ParsePathIntoNameList (ilu_string path, ilu_Error *err)
{
  int components, i, j;
  NameList n;
  char *component, *ending, *possible_dot;
  ilu_cardinal decoded_len;

  components = CountNameListComponents(path);
  n = ilu_MallocE(sizeof(*n) + (components * sizeof(NameComponent_s)), err);
  if (ILU_ERRNOK(*err)) return NIL;
  n->names = (NameComponent_s *) (n + 1);
  n->nnames = components;
  for (component = path, i = 0;  i < components;  i++) {
    ending = FindComponentEnd(component);
    if (ending != NIL) *ending = 0;
    n->names[i].id = _ilu_DecodeBuffer(component, strlen(component), &decoded_len, err);
    if (ILU_ERRNOK(*err)) goto errexit;
    if ((possible_dot = FindComponentIDField(n->names[i].id)) == NIL) {
      n->names[i].kind = n->names[i].id + strlen(n->names[i].id);
    } else {
      *possible_dot = 0;
      n->names[i].kind = possible_dot + 1;
    }
    if (ending != NIL)
      component = ending + 1;
  }
  return n;

 errexit:
  for (j = 0;  j <= i;  j++)
    ilu_free(n->names[j].id);
  ilu_free(n);
  return NIL;
}

static void
  FreeNameList (NameList n)
{
  int i;

  for (i = 0;  i < n->nnames;  i++)
    ilu_free(n->names[i].id);
  ilu_free(n);
}

static ilu_cardinal
  SizeOfNameList (ilu_Call call, NameList nl, ilu_Error *err)
{
  ilu_cardinal size = 0;
  int i;

  size = ilu_SizeOfSequence (call, nl->nnames, 0xFFFFFFFF, NIL, err);
  if (ILU_ERRNOK(*err)) return 0;
  for (i = 0;  i < nl->nnames;  i++) {
    size += ilu_SizeOfString (call, nl->names[i].id, strlen(nl->names[i].id), 0xFFFFFFFF, err);
    if (ILU_ERRNOK(*err)) return 0;
    size += ilu_SizeOfString (call, nl->names[i].kind, strlen(nl->names[i].kind), 0xFFFFFFFF, err);
    if (ILU_ERRNOK(*err)) return 0;
  }
  size += ilu_EndSequence (call, err);
  if (ILU_ERRNOK(*err)) return 0;
  return size;
}

static void
  OutputNameList (ilu_Call call, NameList nl, ilu_Error *err)
{
  int i;

  ilu_OutputSequence (call, nl->nnames, 0xFFFFFFFF, NIL, err);
  if (ILU_ERRNOK(*err)) return;
  for (i = 0;  i < nl->nnames;  i++) {
    ilu_OutputString (call, nl->names[i].id, strlen(nl->names[i].id), 0xFFFFFFFF, err);
    if (ILU_ERRNOK(*err)) return;
    ilu_OutputString (call, nl->names[i].kind, strlen(nl->names[i].kind), 0xFFFFFFFF, err);
    if (ILU_ERRNOK(*err)) return;
  }
  ilu_EndSequence (call, err);
  if (ILU_ERRNOK(*err)) return;
}

static NameList
  InputNameList (ilu_Call call, ilu_Error *err)
{
  NameList n;
  ilu_cardinal count;
  int i, j;

  ilu_InputSequence (call, &count, 0xFFFFFFFF, NIL, err);
  if (ILU_ERRNOK(*err)) return NIL;
  n = ilu_MallocE(sizeof(*n) + (count * sizeof(NameComponent_s)), err);
  if (ILU_ERRNOK(*err)) return NIL;
  n->nnames = count;
  n->names = (NameComponent_s *) (((char *) n) + sizeof(NameList));
  for (i = 0;  i < n->nnames;  i++) {
    n->names[i].id = NIL;
    n->names[i].kind = NIL;
    ilu_InputString (call, &n->names[i].id, &count, 0xFFFF, err);
    if (ILU_ERRNOK(*err)) goto errout;
    ilu_InputString (call, &n->names[i].kind, &count, 0xFFFF, err);
    if (ILU_ERRNOK(*err)) goto errout;
  }
  ilu_EndSequence (call, err);
  if (ILU_ERRNOK(*err)) goto errout;
  return n;

 errout:
  for (j = 0;  j <= i;  j++) {
    if (n->names[j].id != NIL)
      ilu_free(n->names[j].id);
    if (n->names[j].kind != NIL)
      ilu_free(n->names[j].kind);
  };
  ilu_free(n);
  return NIL;    
}

static ilu_string
  StringifyNameList (NameList nl, ilu_Error *err)
{
  int i;
  char *p, *q, *s;
  int size;

  for (size = 0, i = 0;  i < nl->nnames;  i++) {
    size += strlen(nl->names[i].id) + 1;
    size += strcharcount(nl->names[i].id, '/');
    size += strcharcount(nl->names[i].id, '.');
    if (nl->names[i].kind[0] != 0) {
      size += strlen(nl->names[i].kind) + 1;
      size += strcharcount(nl->names[i].kind, '/');
      size += strcharcount(nl->names[i].kind, '.');
    }
  }
  s = ilu_MallocE(size, err);
  if (ILU_ERRNOK(*err)) return NIL;
  for (q = s, i = 0;  i < nl->nnames;  i++) {
    p = nl->names[i].id;
    while (*p != 0) {
      if (*p == '.') {
	*q++ = '\\';
	*q++ = *p++;
      } else {
	*q++ = *p++;
      }
    }
    if (nl->names[i].kind[0] != 0) {
      *q++ = '.';
      p = nl->names[i].kind;
      while (*p != 0) {
	if (*p == '.') {
	  *q++ = '\\';
	  *q++ = *p++;
	} else {
	  *q++ = *p++;
	}
      }
    }
    if ((i + 1) < nl->nnames)
      *q++ = '/';
    else
      *q++ = 0;
  }
  return s;
}

static void
  Initialize_CosNaming_NamingContext (ilu_Error *err)
{
  static ilu_boolean initialized = ilu_FALSE;
  ilu_Exception ex_CosNaming_NamingContext_NotFound;
  ilu_Exception ex_CosNaming_NamingContext_CannotProceed;
  ilu_Exception ex_CosNaming_NamingContext_InvalidName;
  ilu_Exception ex_CosNaming_NamingContext_AlreadyBound;
  ilu_Exception ex_CosNaming_NamingContext_NotEmpty;
  ilu_Error lerr;
  ilu_Class cl;
  ilu_Method m;

  if (initialized)
    return;

  if (!ilu_EnterMutex(ilu_otmu, err))
    goto fail2;

  /* set up exceptions */

  ex_CosNaming_NamingContext_NotFound = ilu_DefineException(ILU_NIL, "IDL:omg.org/CosNaming/NamingContext/NotFound:1.0", "IDL:omg.org/CosNaming/NamingContext/NotFound:1.0", err);
  if (ILU_ERRNOK(*err))
    goto fail1;
  ex_CosNaming_NamingContext_CannotProceed = ilu_DefineException(ILU_NIL, "IDL:omg.org/CosNaming/NamingContext/CannotProceed:1.0", "IDL:omg.org/CosNaming/NamingContext/CannotProceed:1.0", err);
  if (ILU_ERRNOK(*err))
    goto fail1;
  ex_CosNaming_NamingContext_InvalidName = ilu_DefineException(ILU_NIL, "IDL:omg.org/CosNaming/NamingContext/InvalidName:1.0", ILU_NIL, err);
  if (ILU_ERRNOK(*err))
    goto fail1;
  ex_CosNaming_NamingContext_AlreadyBound = ilu_DefineException(ILU_NIL, "IDL:omg.org/CosNaming/NamingContext/AlreadyBound:1.0", ILU_NIL, err);
  if (ILU_ERRNOK(*err))
    goto fail1;
  ex_CosNaming_NamingContext_NotEmpty = ilu_DefineException(ILU_NIL, "IDL:omg.org/CosNaming/NamingContext/NotEmpty:1.0", ILU_NIL, err);
  if (ILU_ERRNOK(*err))
    goto fail1;

  { ilu_string supers[] = {
	"IDL:omg.org/CORBA/Object:1.0",
	NULL};
    cl = ilu_DefineObjectType("CosNaming.NamingContext",	/*name*/
	NULL,	/*no brand*/
	"IDL:omg.org/CosNaming/NamingContext:1.0",	/*uid*/
	NULL,	/*singleton*/
	ilu_TRUE,	/* optional */
	ilu_FALSE,	/* collectible */
	NULL,	/*doc string*/
	10,	/*n methods*/
	1,	/*n supers*/
	supers,	/* supers */
#ifdef ILU_HTTPNG_OBJECTS
	0, ilu_FALSE, ilu_FALSE,
#endif
	err);
    if (ILU_ERRNOK(*err))
      goto fail2;
    CosNaming_NamingContext = cl;
  }
  { ilu_Exception	exns[4];
    exns[0] = ex_CosNaming_NamingContext_NotFound;
    exns[1] = ex_CosNaming_NamingContext_CannotProceed;
    exns[2] = ex_CosNaming_NamingContext_InvalidName;
    exns[3] = ex_CosNaming_NamingContext_AlreadyBound;
    m = ilu_DefineMethod(cl, 0,
	"bind",	/*name*/
	1,	/*id*/
	0,	/*functional*/
	0,	/*asynch*/
	4,	/*n exns*/
	exns,	/*exceptions*/
	2,	/*n args*/
	ILU_NIL,	/*return type ID*/
	err);
    if (ILU_ERRNOK(*err))
      goto fail2;
    ilu_DefineMethodArg (m, 0, "n", ilu_FALSE, ilu_In, "IDL:omg.org/CosNaming/Name:1.0", err);
    if (ILU_ERRNOK(*err))
      goto fail2;
    ilu_DefineMethodArg (m, 1, "obj", ilu_FALSE, ilu_In, "IDL:omg.org/CORBA/Object:1.0", err);
    if (ILU_ERRNOK(*err))
      goto fail2;
  }
  { ilu_Exception	exns[3];
    exns[0] = ex_CosNaming_NamingContext_NotFound;
    exns[1] = ex_CosNaming_NamingContext_CannotProceed;
    exns[2] = ex_CosNaming_NamingContext_InvalidName;
    m = ilu_DefineMethod(cl, 1,
	"rebind",	/*name*/
	2,	/*id*/
	0,	/*functional*/
	0,	/*asynch*/
	3,	/*n exns*/
	exns,	/*exceptions*/
	2,	/*n args*/
	ILU_NIL,	/*return type ID*/
	err);
    if (ILU_ERRNOK(*err))
      goto fail2;
    ilu_DefineMethodArg (m, 0, "n", ilu_FALSE, ilu_In, "IDL:omg.org/CosNaming/Name:1.0", err);
    if (ILU_ERRNOK(*err))
      goto fail2;
    ilu_DefineMethodArg (m, 1, "obj", ilu_FALSE, ilu_In, "IDL:omg.org/CORBA/Object:1.0", err);
    if (ILU_ERRNOK(*err))
      goto fail2;
  }
  { ilu_Exception	exns[4];
    exns[0] = ex_CosNaming_NamingContext_NotFound;
    exns[1] = ex_CosNaming_NamingContext_CannotProceed;
    exns[2] = ex_CosNaming_NamingContext_InvalidName;
    exns[3] = ex_CosNaming_NamingContext_AlreadyBound;
    m = ilu_DefineMethod(cl, 2,
	"bind-context",	/*name*/
	3,	/*id*/
	0,	/*functional*/
	0,	/*asynch*/
	4,	/*n exns*/
	exns,	/*exceptions*/
	2,	/*n args*/
	ILU_NIL,	/*return type ID*/
	err);
    if (ILU_ERRNOK(*err))
      goto fail2;
    ilu_DefineMethodArg (m, 0, "n", ilu_FALSE, ilu_In, "IDL:omg.org/CosNaming/Name:1.0", err);
    if (ILU_ERRNOK(*err))
      goto fail2;
    ilu_DefineMethodArg (m, 1, "nc", ilu_FALSE, ilu_In, "IDL:omg.org/CosNaming/NamingContext:1.0", err);
    if (ILU_ERRNOK(*err))
      goto fail2;
  }
  { ilu_Exception	exns[3];
    exns[0] = ex_CosNaming_NamingContext_NotFound;
    exns[1] = ex_CosNaming_NamingContext_CannotProceed;
    exns[2] = ex_CosNaming_NamingContext_InvalidName;
    m = ilu_DefineMethod(cl, 3,
	"rebind-context",	/*name*/
	4,	/*id*/
	0,	/*functional*/
	0,	/*asynch*/
	3,	/*n exns*/
	exns,	/*exceptions*/
	2,	/*n args*/
	ILU_NIL,	/*return type ID*/
	err);
    if (ILU_ERRNOK(*err))
      goto fail2;
    ilu_DefineMethodArg (m, 0, "n", ilu_FALSE, ilu_In, "IDL:omg.org/CosNaming/Name:1.0", err);
    if (ILU_ERRNOK(*err))
      goto fail2;
    ilu_DefineMethodArg (m, 1, "nc", ilu_FALSE, ilu_In, "IDL:omg.org/CosNaming/NamingContext:1.0", err);
    if (ILU_ERRNOK(*err))
      goto fail2;
  }
  { ilu_Exception	exns[3];
    exns[0] = ex_CosNaming_NamingContext_NotFound;
    exns[1] = ex_CosNaming_NamingContext_CannotProceed;
    exns[2] = ex_CosNaming_NamingContext_InvalidName;
    m = ilu_DefineMethod(cl, 4,
	"resolve",	/*name*/
	5,	/*id*/
	0,	/*functional*/
	0,	/*asynch*/
	3,	/*n exns*/
	exns,	/*exceptions*/
	1,	/*n args*/
	"IDL:omg.org/CORBA/Object:1.0",	/*return type ID*/
	err);
    if (ILU_ERRNOK(*err))
      goto fail2;
    ilu_DefineMethodArg (m, 0, "n", ilu_FALSE, ilu_In, "IDL:omg.org/CosNaming/Name:1.0", err);
    if (ILU_ERRNOK(*err))
      goto fail2;
    CosNaming_NamingContext_resolve = m;
  }
  { ilu_Exception	exns[3];
    exns[0] = ex_CosNaming_NamingContext_NotFound;
    exns[1] = ex_CosNaming_NamingContext_CannotProceed;
    exns[2] = ex_CosNaming_NamingContext_InvalidName;
    m = ilu_DefineMethod(cl, 5,
	"unbind",	/*name*/
	6,	/*id*/
	0,	/*functional*/
	0,	/*asynch*/
	3,	/*n exns*/
	exns,	/*exceptions*/
	1,	/*n args*/
	ILU_NIL,	/*return type ID*/
	err);
    if (ILU_ERRNOK(*err))
      goto fail2;
    ilu_DefineMethodArg (m, 0, "n", ilu_FALSE, ilu_In, "IDL:omg.org/CosNaming/Name:1.0", err);
    if (ILU_ERRNOK(*err))
      goto fail2;
  }
  { ilu_Exception	*exns = NULL;
    m = ilu_DefineMethod(cl, 6,
	"new-context",	/*name*/
	7,	/*id*/
	0,	/*functional*/
	0,	/*asynch*/
	0,	/*n exns*/
	exns,	/*exceptions*/
	0,	/*n args*/
	"IDL:omg.org/CosNaming/NamingContext:1.0",	/*return type ID*/
	err);
    if (ILU_ERRNOK(*err))
      goto fail2;
  }
  { ilu_Exception	exns[4];
    exns[0] = ex_CosNaming_NamingContext_NotFound;
    exns[1] = ex_CosNaming_NamingContext_CannotProceed;
    exns[2] = ex_CosNaming_NamingContext_InvalidName;
    exns[3] = ex_CosNaming_NamingContext_AlreadyBound;
    m = ilu_DefineMethod(cl, 7,
	"bind-new-context",	/*name*/
	8,	/*id*/
	0,	/*functional*/
	0,	/*asynch*/
	4,	/*n exns*/
	exns,	/*exceptions*/
	1,	/*n args*/
	"IDL:omg.org/CosNaming/NamingContext:1.0",	/*return type ID*/
	err);
    if (ILU_ERRNOK(*err))
      goto fail2;
    ilu_DefineMethodArg (m, 0, "n", ilu_FALSE, ilu_In, "IDL:omg.org/CosNaming/Name:1.0", err);
    if (ILU_ERRNOK(*err))
      goto fail2;
  }
  { ilu_Exception	exns[1];
    exns[0] = ex_CosNaming_NamingContext_NotEmpty;
    m = ilu_DefineMethod(cl, 8,
	"destroy",	/*name*/
	9,	/*id*/
	0,	/*functional*/
	0,	/*asynch*/
	1,	/*n exns*/
	exns,	/*exceptions*/
	0,	/*n args*/
	ILU_NIL,	/*return type ID*/
	err);
    if (ILU_ERRNOK(*err))
      goto fail2;
  }
  { ilu_Exception	*exns = NULL;
    m = ilu_DefineMethod(cl, 9,
	"list",	/*name*/
	10,	/*id*/
	0,	/*functional*/
	0,	/*asynch*/
	0,	/*n exns*/
	exns,	/*exceptions*/
	3,	/*n args*/
	ILU_NIL,	/*return type ID*/
	err);
    if (ILU_ERRNOK(*err))
      goto fail2;
    ilu_DefineMethodArg (m, 0, "how-many", ilu_FALSE, ilu_In, "ilut:a9utKC9pScVOm5pXhcjtN2yJ5nO", err);
    if (ILU_ERRNOK(*err))
      goto fail2;
    ilu_DefineMethodArg (m, 1, "bl", ilu_FALSE, ilu_Out, "IDL:omg.org/CosNaming/BindingList:1.0", err);
    if (ILU_ERRNOK(*err))
      goto fail2;
    ilu_DefineMethodArg (m, 2, "bi", ilu_FALSE, ilu_Out, "IDL:omg.org/CosNaming/BindingIterator:1.0", err);
    if (ILU_ERRNOK(*err))
      goto fail2;
  }
 fail1:
 fail2:
  ilu_ExitMutex(ilu_otmu, ilu_TRUE, &lerr);
  return;
}

typedef struct _ilu_ResolveIIOPNameErrs_s {
  enum {
    ilu_rine_Success,
    ilu_rine_SystemError,
    ilu_rine_NotFound,
    ilu_rine_CannotProceed,
    ilu_rine_InvalidName } whicherr;
  union {
    ilu_Error		SystemError;
    struct {
      enum {
	ilu_rine_nfe_missing_node,
	ilu_rine_nfe_not_context,
	ilu_rine_nfe_not_object }	why;
      ilu_string			rest_of_name;
    }			NotFound;
    struct {
      ilu_string			context_sbh;
      ilu_string			rest_of_name;
    }			CannotProceed;
  } error_data;
} ilu_ResolveIIOPNameErrs_s;  

static ilu_Object
  CosNamingResolve (ilu_Object ns, NameList namelist, ilu_ResolveIIOPNameErrs_s *errout, ilu_Error *err)
{
  ilu_Call_s      call_s;
  ilu_Call        call = &call_s;
  ilu_cardinal    reqSize;
  ilu_cardinal    estatus = 0;
  ilu_ProtocolException pe;
  ilu_Server      s = object_server(ns);
  ilu_Object	result = NIL;
  ilu_Connection  newconn = NIL;
  NameList	nl;

  ILU_NOTE(OBJECT_DEBUG,
	("(iiop.c:CosNamingResolve):  name service object %p\n", ns));

  (void) ilu_StartCall(call, s, CosNaming_NamingContext,
		       CosNaming_NamingContext_resolve, 0, NIL, &newconn, err);
 retry:
  if (newconn != NIL)
    (void) _ilu_HandOffNewConnection(newconn, err);
  if (ILU_ERRNOK(*err))
    return result;
  _ilu_AcquireServerMutex(s);
  reqSize = ilu_SizeOfObjectID(call, ns, ilu_TRUE, _ilu_rootClass, err);
  _ilu_ReleaseServerMutex(s);
  if (ILU_ERRNOK(*err))
    goto faild;
  reqSize += SizeOfNameList (call, namelist, err);
  if (ILU_ERRNOK(*err))
    goto faild;
  if (!ilu_StartRequest(call, reqSize, err))
    goto faild;
  ilu_EnterServer(s, object_class(ns));
  ilu_OutputObjectID(call, ns, ilu_TRUE, _ilu_rootClass, err);
  if (ILU_ERRNOK(*err))
    goto faild;
  OutputNameList (call, namelist, err);
  if (ILU_ERRNOK(*err))
    goto faild;
  if (!ilu_FinishRequest(call, err))
    goto faild;
  pe = ilu_GetReply(call, &estatus, &newconn, err);
  if (ILU_ERRNOK(*err) &&
      (err->ilu_type == ILU_ERRTYP(transient)) &&
      (ILU_ERRSEL(transient,*err).minor == ilu_tm_retry)) {
    ILU_HANDLED(*err);
    ILU_CLER(*err);
    goto retry;
  };
  if (ILU_ERRNOK(*err)) goto faild;
  if (pe != ilu_ProtocolException_Success) {
    errout->whicherr = ilu_rine_SystemError;
    switch (pe) {
    case ilu_ProtocolException_NoSuchClassAtServer:
      ILU_ERR_CONS1(bad_typecode, &errout->error_data.SystemError, minor, ilu_btm_unknownType, 0);
      break;
    case ilu_ProtocolException_ClassVersionMismatch:
      ILU_ERR_CONS1(bad_typecode, &errout->error_data.SystemError, minor, ilu_btm_unknownType, 0);
      break;
    case ilu_ProtocolException_NoSuchMethodOnClass:
      ILU_ERR_CONS1(bad_operation, &errout->error_data.SystemError, minor, ilu_bom_noSuchOperationOnType, 0);
      break;
    case ilu_ProtocolException_GarbageArguments:
      ILU_ERR_CONS1(marshal, &errout->error_data.SystemError, minor, ilu_mm_unknown, 0);
      break;
    case ilu_ProtocolException_RequestRejected:
    case ilu_ProtocolException_Unknown:
      ILU_ERR_CONS0(unknown, &errout->error_data.SystemError, 0);
      break;
    case ilu_ProtocolException_LostConnection:
      ILU_ERR_CONS1(comm_failure, &errout->error_data.SystemError, minor, ilu_cfm_conn_lost, 0);
      break;
    case ilu_ProtocolException_RequestTimeout:
      ILU_ERR_CONS0(no_response, &errout->error_data.SystemError, 0);
      break;
    default:
      ILU_ERR_CONS0(unknown, &errout->error_data.SystemError, 0);
      break;
    }
  } else if (estatus == 0) { /* successful completion */
    errout->whicherr = ilu_rine_Success;
    ilu_InputObjectID(call, &result, ilu_FALSE, ilu_rootClass, err);
    if (ILU_ERRNOK(*err))
      goto faild;
    if (result != NIL)
      ilu_ExitServer(object_server(result), object_class(result));
    ilu_ReplyRead(call, err);
    goto faild;
  } else if (estatus == 1) {
    /* NotFound */
    ilu_shortcardinal which;
    NameList nl;
    errout->whicherr = ilu_rine_NotFound;
    ilu_InputEnum (call, &which, NIL, err);
    if (ILU_ERRNOK(*err)) goto faild;
    switch (which) {
    case 0:
      errout->error_data.NotFound.why = ilu_rine_nfe_missing_node;
      break;
    case 1:
      errout->error_data.NotFound.why = ilu_rine_nfe_not_context;
      break;
    case 2:
      errout->error_data.NotFound.why = ilu_rine_nfe_not_object;
      break;
    default:
      ILU_ERR_CONS1(marshal, err, minor, ilu_mm_enum_value, 0);
      goto faild;
    };
    nl = InputNameList(call, err);
    if (ILU_ERRNOK(*err)) goto faild;
    errout->error_data.NotFound.rest_of_name = StringifyNameList(nl, err);
    FreeNameList(nl);
    if (ILU_ERRNOK(*err)) goto faild;
  } else if (estatus == 2) {
    /* CannotProceed */
    ilu_Object nctx;
    errout->whicherr = ilu_rine_CannotProceed;
    ilu_InputObjectID (call, &nctx, ilu_FALSE, CosNaming_NamingContext, err);
    if (ILU_ERRNOK(*err)) goto faild;
    errout->error_data.CannotProceed.context_sbh = ilu_SBHOfObject(nctx);
    ilu_ExitServer(object_server(nctx), CosNaming_NamingContext);
    nl = InputNameList (call, err);
    if (ILU_ERRNOK(*err)) goto faild;
    errout->error_data.CannotProceed.rest_of_name = StringifyNameList(nl, err);
    FreeNameList(nl);
    if (ILU_ERRNOK(*err)) goto faild;
  } else if (estatus == 3) {
    /* InvalidName */
    errout->whicherr = ilu_rine_InvalidName;
  } else {
    ILU_ERR_CONS1(marshal, err, minor, ilu_mm_excn_id, 0);
    goto faild;
  }
faild:
  ilu_FinishCall(call, err);
  if (ILU_ERRNOK(*err)) {
    ILU_HANDLED(*err);
  }
  return result;
}

/* Returns object result of contacting the specified CosNaming service,
   and doing "resolve" on the specified path.  If an error occurs remotely,
   returns NIL, with "err" indicating success, and sets "errout"
   to indicate the error.  If an error occurs
   locally, returns NIL, and sets "err" to indicate the error.
 */
static ilu_Object
  ResolveIIOPName (ilu_string iiopname,
		   ilu_ResolveIIOPNameErrs_s *errout,
		   ILU_ERRS((bad_locks, inv_objref,
			     no_resources, IoErrs)) * err)
{
  char *p;
  char addresses[1024];
  char object_path[1024];
  char ns_url[2048];
  ilu_Object ns_obj;
  ilu_Object user_obj;
  NameList namelist;

  Initialize_CosNaming_NamingContext (err);
  if (ILU_ERRNOK(*err)) return NIL;

  if (strncmp(iiopname, "iiopname:/", 10) == 0)
    p = iiopname + 9;
  else
    return ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_ior, NIL);

  if (sscanf (p, "//%1023[^/]/%1023s", addresses, object_path) != 2)
    return ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_ior, NIL);
  else if ((strlen(addresses) < 1) || (strlen(object_path) < 1))
    return ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_ior, NIL);
  ILU_NOTE(IIOP_DEBUG,
	   ("ilu_IIOP_ResolveIIOPName:  addresses are <%s>, path is <%s>\n",
	    addresses, object_path));

  /* Form an iioploc: url for the NameService */
  sprintf(ns_url, "iioploc://%s/NameService", addresses);

  /* Turn the path into a list of name/extension pairs */
  namelist = ParsePathIntoNameList(object_path, err);
  if (ILU_ERRNOK(*err)) return NIL;

  /* Get an object corresponding to the name service */
  ns_obj = ilu_ObjectOfSBH (ns_url, CosNaming_NamingContext, err);
  if (ILU_ERRNOK(*err)) goto errout1;
  ilu_DeltaHolds(ns_obj, +1);
  ilu_ExitServer (object_server(ns_obj), CosNaming_NamingContext);

  /* call resolve on the name service */
  user_obj = CosNamingResolve (ns_obj, namelist, errout, err);
  if (ILU_ERRNOK(*err)) goto errout2;

  /* free up the ns_obj, and the namelist */
  if (object_server(user_obj) != object_server(ns_obj))
    ilu_EnterServer (object_server(ns_obj), CosNaming_NamingContext);
  _ilu_DeltaHolds(ns_obj, -1);
  if (object_server(user_obj) != object_server(ns_obj))
    ilu_ExitServer (object_server(ns_obj), CosNaming_NamingContext);
  FreeNameList(namelist);

  /* return the user_obj */
  return user_obj;

  /* return with an error */
 errout2:
  ilu_EnterServer (object_server(ns_obj), CosNaming_NamingContext);
  _ilu_DeltaHolds(ns_obj, -1);
  ilu_ExitServer (object_server(ns_obj), CosNaming_NamingContext);
 errout1:
  FreeNameList (namelist);
  return NIL;
}

static void
      FreeResolveIIOPNameErrs(ilu_ResolveIIOPNameErrs_s *err)
{
  switch (err->whicherr) {
  case ilu_rine_Success:
  case ilu_rine_InvalidName:
    break;
  case ilu_rine_SystemError:
    ILU_HANDLED(err->error_data.SystemError);
    break;
  case ilu_rine_NotFound:
    ilu_free(err->error_data.NotFound.rest_of_name);
    break;
  case ilu_rine_CannotProceed:
    ilu_free(err->error_data.CannotProceed.context_sbh);
    ilu_free(err->error_data.CannotProceed.rest_of_name);
    break;
  default:
    break;
  }
}

ilu_boolean _ilu_IIOP_ParseIIOPName (ilu_string iiopname, ilu_string *ih, ilu_string *sid,
				     ilu_string *mstid, ilu_string *cinfo, ilu_cardinal *cinfolen,
				     ilu_boolean *pass_cinfo, ilu_Error *err)
{
  ilu_Object kobj;
  ilu_ResolveIIOPNameErrs_s usererrs;
  ilu_Server s;

  kobj = ResolveIIOPName (iiopname, &usererrs, err);
  if (ILU_ERRNOK(*err)) return ilu_FALSE;
  switch (usererrs.whicherr) {
  case ilu_rine_Success:
    {
      s = object_server(kobj);
      if (ih) {
	*ih = ilu_StrdupE(object_ih(kobj), err);
	if (ILU_ERRNOK(*err)) break;
      };
      if (sid) {
	*sid = ilu_StrdupE(server_id(s), err);
	if (ILU_ERRNOK(*err)) break;
      };
      if (mstid)
	*mstid = NIL;
      if (cinfo || cinfolen) {
	if (cinfo) {
	  *cinfo = ilu_StrdupE(s->sr_cinfo.icb_base, err);
	  if (ILU_ERRNOK(*err)) break;
	};
	if (*cinfolen) {
	  if (cinfo)
	    *cinfolen = strlen(*cinfo);
	  else
	    *cinfolen = s->sr_cinfo.icb_len;
	};
	if (pass_cinfo)
	  *pass_cinfo = ilu_TRUE;
      }
    }
    break;
  case ilu_rine_SystemError:
    *err = usererrs.error_data.SystemError;
    break;
  case ilu_rine_NotFound:
    ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_CosNaming_NotFound, 0);
    FreeResolveIIOPNameErrs(&usererrs);
    break;
  case ilu_rine_CannotProceed:
    ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_CosNaming_CannotProceed, 0);
    FreeResolveIIOPNameErrs(&usererrs);
    break;
  case ilu_rine_InvalidName:
    ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_CosNaming_InvalidName, 0);
    FreeResolveIIOPNameErrs(&usererrs);
    break;
  default:
    ILU_ERR_CONS1(internal, err, minor, ilu_im_badEnumValue, 0);
    FreeResolveIIOPNameErrs(&usererrs);
    break;
  }
  return (ILU_ERROK(*err));
}

static ilu_boolean
  _IIOP_ParseCDR (ilu_bytes iorbytes, ilu_cardinal nbytes, ilu_string *ih, ilu_string *sid,
		  ilu_string *mstid, ilu_string *cinfo, ilu_cardinal *cinfo_len,
		  ilu_Error *err)
{
  PACKET pk;
  struct IIOP_IOR_IOR *p;
  ilu_cardinal short_char_codeset = 0;
  ilu_cardinal char_codeset = 0;
  ilu_string repository_id = NIL;
  ilu_cardinal repository_id_len = 0;
  ilu_cardinal nprofiles = 0;
  enum byte_order bo;
  ilu_inv_objref_Minor codeset_err;
  ilu_byte junk;
  ilu_cardinal i;
  static ilu_Class corba_object = ILU_NIL;

  if (corba_object == ILU_NIL)
    corba_object = ilu_FindClassFromID("IDL:omg.org/CORBA/Object:1.0");

  bo = (iorbytes[0] == 0) ? BigEndian : LittleEndian;
  pk = _cdr_InmemPacket (nbytes, iorbytes, bo, 0, err);
  if (ILU_ERRNOK(*err))
    return ilu_FALSE;
  
  if ((_cdr_get_u8 (pk, &junk, err), ILU_ERRNOK(*err)) ||	/* throw away byte order */
      (_cdr_get_bytes (pk, (ilu_bytes *) &repository_id, &repository_id_len, 0xFFFF, err), ILU_ERRNOK(*err)) ||
      (_cdr_get_u32 (pk, &nprofiles, err), ILU_ERRNOK(*err)))
    {
      _cdr_InmemFree(pk, NIL, NIL);
      return ilu_FALSE;
    }
  ILU_NOTE(IIOP_DEBUG,
	("_IIOP_ParseCDR:  byte order %s, repository id <%s>, %lu profile%s\n",
	 (bo == LittleEndian) ? "LittleEndian" : "BigEndian", repository_id, (unsigned long) nprofiles,
	 (nprofiles == 1) ? "" : "s"));
  if (nprofiles == 0)
    {
      ILU_NOTE(IIOP_DEBUG,
	    ("_IIOP_ParseCDR:  no profiles, so returning the NIL object\n"));
      /* NIL object ref */
      ILU_CLER(*err);
      ilu_free(repository_id);
      _cdr_InmemFree(pk, NIL, NIL);
      return ilu_TRUE;
    }
  nbytes = sizeof(struct IIOP_IOR_IOR) + nprofiles*(sizeof(struct IIOP_IOR_TaggedProfile));
  p = (struct IIOP_IOR_IOR *) ilu_malloc(nbytes);
  if (p == NIL)
    {
      ilu_free(repository_id);
      _cdr_InmemFree(pk, NIL, NIL);
      return ILU_ERR_CONS1(no_memory, err, nbytes, nbytes, ilu_FALSE);
    }
  p->type_id = repository_id;
  p->nProfiles = nprofiles;
  for (i = 0;  i < nprofiles;  i++)
    {
      p->Profile[i].profileData = NIL;
      if ((_cdr_get_u32 (pk, &p->Profile[i].tag, err), ILU_ERRNOK(*err)) ||
	  (_cdr_get_bytes (pk, &p->Profile[i].profileData,
			   &p->Profile[i].profileDataLen, 0xFFFF, err), ILU_ERRNOK(*err)))
	{
	  ILU_NOTE(IIOP_DEBUG,
		("_IIOP_ParseCDR:  Error reading profile %lu\n", (unsigned long) i+1));
	  _cdr_InmemFree(pk, NIL, NIL);
	  FreeIORData(p);
	  ilu_free(p);
	  return ilu_FALSE;
	}
      ILU_NOTE(IIOP_DEBUG,
	    ("_IIOP_ParseCDR:  profile %lu is %lu bytes, tag %lu%s, %s byte order\n",
	     (unsigned long) i+1, (unsigned long) p->Profile[i].profileDataLen,
	     p->Profile[i].tag, ((p->Profile[i].tag == IIOP_TAG_INTERNET_IOP) ? " (INTERNET)" :
				 ((p->Profile[i].tag == IIOP_TAG_ILU_IOP) ? " (ILU)" :
				  ((p->Profile[i].tag == IIOP_TAG_MULTIPLE_COMPONENT_IOP) ? " (MULTIPLE COMPONENT)" : ""))),
	     (p->Profile[i].profileData[0] == 0) ? "BigEndian" : "LittleEndian"));
    }
  _cdr_InmemFree(pk, NIL, NIL);	/* note this also frees iorbytes */
  (void) ParseIOR(p, corba_object, ih, sid, mstid, cinfo, cinfo_len, &short_char_codeset, &char_codeset, err);
  FreeIORData(p);
  ilu_free(p);
  if (((short_char_codeset != ilu_IIOP_NATIVE_SHORT_CHAR_CODE_SET_ID) &&
       (codeset_err = ilu_iom_short_char_codeset)) ||
      /* XXX - this next clause is wrong; we should really allow a value
	 of 0 as well, but that means we'd have to grovel over the interfaces
	 of the object to ensure they don't use wchar or wstring, and that's
	 too painful. */
      ((char_codeset != ilu_IIOP_NATIVE_CHAR_CODE_SET_ID) &&
       (codeset_err = ilu_iom_char_codeset))) {
    ilu_free(sid);
    ilu_free(cinfo);
    ilu_free(ih);
    ilu_free(mstid);
    return ILU_ERR_CONS1(inv_objref, err, minor, codeset_err, ilu_FALSE);
  };
  if (ILU_ERRNOK(*err))
    {
      ILU_NOTE(IIOP_DEBUG,
	    ("_IIOP_ParseCDR:  error:  No object for IOR.\n"));
      return ilu_FALSE;
    }
  else
    return ilu_TRUE;
}

ilu_boolean _ilu_IIOP_ParseIOR (ilu_string ior, ilu_string *ih, ilu_string *sid,
				ilu_string *mstid, ilu_string *cinfo, ilu_cardinal *cinfo_len,
				ilu_boolean *pass_cinfo, ilu_Error *err)
{
  /* Parse OMG IOR: style URL:
   *
   * IOR:<hex-digits>
   */

  ilu_cardinal nbytes;
  ilu_cardinal i, ptr;
  ilu_bytes iorbytes;

  if (ior == NIL)
    {
      ILU_NOTE(IIOP_DEBUG,
	    ("_ilu_IIOP_ParseIOR:  NIL IOR string passed\n"));
      return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_nil, ilu_FALSE);
    }
  else if ((strncmp(ior, "IOR:", 4) != 0) && (strncmp(ior, "ior:", 4) != 0))
    {
      ILU_NOTE(IIOP_DEBUG,
	    ("_ilu_IIOP_ParseIOR:  IOR string doesn't begin with \"IOR:\"\n"));
      return ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_ior, ilu_FALSE);
    }
  else if ((nbytes = strlen(ior+4)) < 2)
    {
      ILU_NOTE(IIOP_DEBUG,
	    ("_ilu_IIOP_ParseIOR:  IOR string is too short to contain obj ref\n"));
      return ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_ior, ilu_FALSE);
    }
  else if ((nbytes % 2) != 0)
    {
      ILU_NOTE(IIOP_DEBUG,
	    ("_ilu_IIOP_ParseIOR:  IOR string contains odd number of hex digits\n"));
      return ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_ior, ilu_FALSE);
    }
  nbytes = (nbytes / 2);
  iorbytes = ilu_MallocE(nbytes, err);
  if (ILU_ERRNOK(*err)) return ilu_FALSE;
  for (i = 0, ptr = 4;  i < nbytes;  i += 1, ptr += 2)
    iorbytes[i] = (hexval(ior[ptr]) << 4) | hexval(ior[ptr+1]);
  /* ParseCDR should free iorbytes */
  (void) _IIOP_ParseCDR (iorbytes, nbytes, ih, sid, mstid, cinfo, cinfo_len, err);
  if (cinfo != NIL && pass_cinfo != NIL)
    *pass_cinfo = ilu_TRUE;
  return ILU_ERROK(*err);
}

static ilu_cardinal _IIOP_cdr_size (ilu_string);
static ilu_cardinal _IIOP_ior2_to_cdr(ilu_string /* ior */, ilu_bytes /* cdr */, ilu_cardinal /* cdrsize */);

ilu_boolean _ilu_IIOP_ParseIOR2 (ilu_string ior, ilu_string *ih, ilu_string *sid,
				 ilu_string *mstid, ilu_string *cinfo, ilu_cardinal *cinfo_len,
				 ilu_boolean *pass_cinfo, ilu_Error *err)
{
  /* Parse OMG IOR: style URL:
   *
   * IOR:<hex-digits>
   */

  ilu_cardinal nbytes, i;
  ilu_bytes iorbytes;

  if (ior == NIL)
    {
      ILU_NOTE(IIOP_DEBUG,
	    ("_ilu_IIOP_ParseIOR2:  NIL IOR string passed\n"));
      return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_nil, ilu_FALSE);
    }
  else if (strncmp(ior, "IOR2:", 4) != 0)
    {
      ILU_NOTE(IIOP_DEBUG,
	    ("_ilu_IIOP_ParseIOR2:  IOR string doesn't begin with \"IOR2:\"\n"));
      return ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_ior, ilu_FALSE);
    };
  nbytes = _IIOP_cdr_size(ior);
  iorbytes = ilu_MallocE(nbytes, err);
  if (ILU_ERRNOK(*err)) return ilu_FALSE;
  if ((i = _IIOP_ior2_to_cdr(ior, iorbytes, nbytes)) == 0) {
    ILU_NOTE(IIOP_DEBUG,
	     ("_ilu_IIOP_ParseIOR2:  Malformed IOR2 string passed.\n"));
    ilu_free(iorbytes);
    return ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_ior, ilu_FALSE);
  };
  /* ParseCDR should free iorbytes */
  (void) _IIOP_ParseCDR (iorbytes, nbytes, ih, sid, mstid, cinfo, cinfo_len, err);
  if (cinfo != NIL && pass_cinfo != NIL)
    *pass_cinfo = ilu_TRUE;
  return ILU_ERROK(*err);
}

static HashTable ObjectIors = ILU_NIL;

static struct IIOP_IOR_IOR NilObjectIOR = { "", 0, { { 0, 0, ILU_NIL } } };

typedef struct iorhash {
  ilu_Server s;
  ilu_Class c;
  struct IIOP_IOR_IOR *ior;
  char ih[1];
} *iorhash;

static struct IIOP_IOR_IOR *FindIORForObject(ilu_Object h)
{
  iorhash ior;

  if (h == NIL)
    return &NilObjectIOR;
  if (ObjectIors == NIL)
    return NIL;
  if ((ior = (iorhash) ilu_hash_FindInTable(ObjectIors, h)) == NIL)
    return NIL;
  if ((h->ob_server != ior->s) ||
      (strcmp(h->ob_ih, ior->ih) != 0) ||
      (h->ob_class != ior->c))
    {
      ilu_hash_RemoveFromTable(ObjectIors, h);
      ilu_free(ior->ior);
      ilu_free(ior);
      return NIL;
    }
  else
    return ior->ior;
}

static void RegisterIORForObject (ilu_Object h, struct IIOP_IOR_IOR *ior)
{
  iorhash n;

  if (h == NIL)
    return;

  if (ObjectIors == NIL)
    ObjectIors = ilu_hash_MakeNewTable(137, ilu_hash_HashPointer,
					ilu_hash_PointerCompare);

  _ilu_Assert(ObjectIors!=NIL,"NIL IOR hash table");
  if (ior == NIL)
    ilu_hash_RemoveFromTable (ObjectIors, h);
  else
    {
      n = (iorhash) ilu_malloc(sizeof(struct iorhash) + strlen(h->ob_ih));
      if (n == NIL)
	return;
      n->c = h->ob_class;
      n->s = h->ob_server;
      strcpy (n->ih, h->ob_ih);
      n->ior = ior;
      ilu_hash_AddToTable(ObjectIors, h, n);
    }
}

static void PossiblyRegisterIORForObject (ilu_Object h, struct IIOP_IOR_IOR *ior)
{
  struct IIOP_IOR_IOR *v = FindIORForObject (h);
  if (v == NIL)
    RegisterIORForObject (h, ior);
  else
    {
      FreeIORData(ior);
      ilu_free(ior);
    }
}

static ilu_cardinal
  _IIOP_CDROfObject (ilu_Object obj, ilu_bytes *cdr, ilu_Error *err)
{
  PACKET pk;
  struct IIOP_IOR_IOR	*iorp = NIL;
  ilu_cardinal size;
  ilu_cardinal i;
  if ((iorp = FindIORForObject(obj)) == NIL)
    {
      if ((*err = IOROfObject(obj, &iorp)), ILU_ERRNOK(*err))
	return (0);
      RegisterIORForObject(obj, iorp);
    }
  size = 4 + 4 + strlen(iorp->type_id) + PADDING_NEC(strlen(iorp->type_id),4);	/* type ID */
  size += 4;								/* nProfiles */
  for (i = 0;  i < iorp->nProfiles;  i++)
    size += (4 + 4 + iorp->Profile[i].profileDataLen + PADDING_NEC(iorp->Profile[i].profileDataLen,4));
  pk = _cdr_InmemPacket (size, NIL, NATIVE_BYTE_ORDER, 0, err);
  if ((_cdr_put_u8 (pk, (NATIVE_BYTE_ORDER == LittleEndian), err), ILU_ERRNOK(*err)) ||
      (_cdr_put_bytes (pk, (ilu_bytes) iorp->type_id, strlen(iorp->type_id) + 1, err), ILU_ERRNOK(*err)) ||
      (_cdr_put_u32 (pk, iorp->nProfiles, err), ILU_ERRNOK(*err)))
    {
      _cdr_InmemFree(pk, NIL, NIL);
      return 0;
    }
  for (i = 0;  i < iorp->nProfiles;  i++)
    {
      if ((_cdr_put_u32 (pk, iorp->Profile[i].tag, err), ILU_ERRNOK(*err)) ||
	  (_cdr_put_bytes (pk, iorp->Profile[i].profileData,
			   iorp->Profile[i].profileDataLen, err), ILU_ERRNOK(*err)))
	{
	  _cdr_InmemFree(pk, NIL, NIL);
	  return 0;
	}
    }
  _cdr_InmemFree (pk, &size, cdr);
  return size;
}

/* (obj!=NIL) => Inside(object_server(obj), object_class(obj) */
ilu_string
  ilu_IOROfObject (ilu_Object obj, ilu_Error *err)
{
  static ilu_string NIL_IOR = "IOR:00000000000000010000000000000000";
  ilu_bytes marshalled_ior;
  ilu_cardinal marshalled_ior_len;
  ilu_string new_ior_string;
  ilu_cardinal i;

  if (obj == NIL)
    {
      ILU_CLER(*err);
      return _ilu_Strdup(NIL_IOR);
    }
  else
    {
      marshalled_ior_len = _IIOP_CDROfObject(obj, &marshalled_ior, err);
      if (ILU_ERRNOK(*err)) return NIL;
      if ((new_ior_string = ilu_malloc(5 + (marshalled_ior_len * 2))) == NIL)
	{
	  ilu_free (marshalled_ior);
	  return (ILU_ERR_CONS1(no_memory, err, nbytes, 5+(marshalled_ior_len*2), NIL));
	}
      strcpy (new_ior_string, "IOR:");
      for (i = 0;  i < marshalled_ior_len;  i++)
	{
	  new_ior_string[4+(2*i)] = hextable[(marshalled_ior[i] >> 4) & 0xF];
	  new_ior_string[5+(2*i)] = hextable[marshalled_ior[i] & 0xF];
	}
      new_ior_string[4+(marshalled_ior_len*2)] = 0;
      ilu_free(marshalled_ior);
      ILU_CLER(*err);
      return new_ior_string;
    }
}

static ilu_cardinal
  _IIOP_cdr_to_ior2 (const ilu_bytes /* cdr */,
		     ilu_cardinal /* cdrsize */,
		     ilu_string /* ior2 */,
		     ilu_cardinal /* ior2size */);

/* (obj!=NIL) => Inside(object_server(obj), object_class(obj) */
ilu_string
  ilu_IOR2OfObject (ilu_Object obj, ilu_Error *err)
{
  static ilu_string NIL_IOR = "IOR2:000g=74=8";
  ilu_bytes marshalled_ior;
  ilu_cardinal marshalled_ior_len, new_ior_string_len;
  ilu_string new_ior_string;

  if (obj == NIL)
    {
      ILU_CLER(*err);
      return _ilu_Strdup(NIL_IOR);
    }
  else
    {
      marshalled_ior_len = _IIOP_CDROfObject(obj, &marshalled_ior, err);
      if (ILU_ERRNOK(*err)) return NIL;
      new_ior_string_len = marshalled_ior_len * 2 + 6;
      new_ior_string = (ilu_string) ilu_MallocE(new_ior_string_len, err);
      if (ILU_ERRNOK(*err)) { ilu_free(marshalled_ior); return NIL; };
      if ((new_ior_string_len = _IIOP_cdr_to_ior2 (marshalled_ior, marshalled_ior_len,
						   new_ior_string, new_ior_string_len)) == 0) {
	ILU_NOTE(IIOP_DEBUG,
		 ("ilu_IOR2OfObject:  encountered unexpected bad size in "
		  "_IIOP_cdr_to_ior2() when developing IOR2 form for (%s/%s)!\n",
		  server_id(object_server(obj)), object_ih(obj)));
	ilu_free(new_ior_string);
	ilu_free(marshalled_ior);
	return ILU_ERR_CONS1(internal, err, minor, ilu_im_callFail, NIL);
      };
      ilu_free(marshalled_ior);
      return new_ior_string;
    }
}

/*======================================================================*/
/*======================== Basic I/O code ==============================*/
/*======================================================================*/

#define INPUT_ERROR		1
#define OUTPUT_ERROR		2

/*L1, L2, Main unconstrained (this is only for calling from debugger)*/
ilu_cardinal _ilu_IIOP_SetMaxStringSize (ilu_cardinal size)
{
  ilu_cardinal old_size = IIOPMaxStringSize;
  if (size > 0)
    IIOPMaxStringSize = size;
  return (old_size);  
}

/*L2 >= {call's connection's iomu}*/
/*L1, Main unconstrained*/

/* ==================== cardinal ==================== */

static void
  _IIOP_OutputCardinal (ilu_Call call, ilu_cardinal l, ILU_ERRS((IoErrs)) *err)
{
  packet_put_u32(iiop_packet(call), l, err);
}

static void
  _IIOP_InputCardinal (ilu_Call call, ilu_cardinal *i, ILU_ERRS((IoErrs)) *err)
{
  packet_get_u32(iiop_packet(call), i, err);
}

/*ARGSUSED*/
static ilu_cardinal
  _IIOP_SizeOfCardinal (ilu_Call call, ilu_cardinal i, ILU_ERRS((IoErrs)) *err)
{
  ilu_cardinal n = PADDING_NEC(iiop_vop(call), 4) + 4;

  iiop_incr_vop(call, n);
  ILU_CLER(*err);
  return (n);
}

/* ==================== short cardinal ==================== */

static void
  _IIOP_OutputShortCardinal (ilu_Call call, ilu_shortcardinal i, ILU_ERRS((IoErrs)) *err)
{
  packet_put_u16(iiop_packet(call), i, err);
}

static void
  _IIOP_InputShortCardinal (ilu_Call call, ilu_shortcardinal *i, ILU_ERRS((IoErrs)) *err)
{
  packet_get_u16(iiop_packet(call), i, err);
}

/*ARGSUSED*/
static ilu_cardinal
  _IIOP_SizeOfShortCardinal (ilu_Call call, ilu_shortcardinal i, ILU_ERRS((IoErrs)) *err)
{
  ilu_cardinal n = PADDING_NEC(iiop_vop(call), 2) + 2;

  iiop_incr_vop(call, n);
  ILU_CLER(*err);
  return (n);
}

/* ==================== integer ==================== */

static void
  _IIOP_OutputInteger (ilu_Call call, ilu_integer i, ILU_ERRS((IoErrs)) *err)
{
  packet_put_s32 (iiop_packet(call), i, err);
}

static void
  _IIOP_InputInteger (ilu_Call call, ilu_integer *i, ILU_ERRS((IoErrs)) *err)
{
  packet_get_s32 (iiop_packet(call), i, err);
}

/*ARGSUSED*/
static ilu_cardinal
  _IIOP_SizeOfInteger (ilu_Call call, ilu_integer i, ILU_ERRS((IoErrs)) *err)
{
  ilu_cardinal n = PADDING_NEC(iiop_vop(call), 4) + 4;

  iiop_incr_vop(call, n);
  ILU_CLER(*err);
  return (n);
}

/* ==================== character ==================== */

static void
  _IIOP_InputCharacter (ilu_Call call, ilu_character *i, ILU_ERRS((IoErrs)) *err)
{
  ilu_shortcardinal v;

  if (iiop_char_codeset(call) != ilu_IIOP_NATIVE_CHAR_CODE_SET_ID)
    {
      ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_char_codeset, 0);
      return;
    }
  packet_get_u16(iiop_packet(call), &v, err);
  if (ILU_ERROK(*err))
    *i = v;
}

/* ==================== short integer ==================== */

static void
  _IIOP_OutputShortInteger (ilu_Call call, ilu_shortinteger i, ILU_ERRS((IoErrs)) *err)
{
  packet_put_s16(iiop_packet(call), i, err);
}

static void
  _IIOP_InputShortInteger (ilu_Call call, ilu_shortinteger *i, ILU_ERRS((IoErrs)) *err)
{
  packet_get_s16 (iiop_packet(call), i, err);
}

/*ARGSUSED*/
static ilu_cardinal
  _IIOP_SizeOfShortInteger (ilu_Call call, ilu_shortinteger i, ILU_ERRS((IoErrs)) *err)
{
  ilu_cardinal n = PADDING_NEC(iiop_vop(call), 2) + 2;

  iiop_incr_vop(call, n);
  ILU_CLER(*err);
  return (n);
}

/* ==================== long integer ==================== */

static void
  _IIOP_OutputLongInteger (ilu_Call call, ilu_longinteger i, ILU_ERRS((IoErrs)) *err)
{
  packet_put_s64 (iiop_packet(call), &i, err);
}

static void
  _IIOP_InputLongInteger (ilu_Call call, ilu_longinteger *i, ILU_ERRS((IoErrs)) *err)
{
  packet_get_s64 (iiop_packet(call), i, err);
}

/*ARGSUSED*/
static ilu_cardinal
  _IIOP_SizeOfLongInteger (ilu_Call call, ilu_longinteger i, ILU_ERRS((IoErrs)) *err)
{
  ilu_cardinal n = PADDING_NEC(iiop_vop(call), 8) + 8;

  iiop_incr_vop(call, n);
  ILU_CLER(*err);
  return (n);
}

/* ==================== long cardinal ==================== */

static void
  _IIOP_OutputLongCardinal (ilu_Call call, ilu_longcardinal i, ILU_ERRS((IoErrs)) *err)
{
  _cdr_put_u64 (iiop_packet(call), &i, err);
}

static void
  _IIOP_InputLongCardinal (ilu_Call call, ilu_longcardinal *i, ILU_ERRS((IoErrs)) *err)
{
  _cdr_get_u64 (iiop_packet(call), i, err);
}

/*ARGSUSED*/
static ilu_cardinal
  _IIOP_SizeOfLongCardinal (ilu_Call call, ilu_longcardinal i, ILU_ERRS((IoErrs)) *err)
{
  ilu_cardinal n = PADDING_NEC(iiop_vop(call), 8) + 8;

  iiop_incr_vop(call, n);
  ILU_CLER(*err);
  return (n);
}

/* ==================== real ==================== */

static void
  _IIOP_OutputReal (ilu_Call call, double d, ILU_ERRS((IoErrs)) *err)
{
  _cdr_put_r64 (iiop_packet(call), d, err);
}

static void
  _IIOP_InputReal (ilu_Call call, double *d, ILU_ERRS((IoErrs)) *err)
{
  _cdr_get_r64 (iiop_packet(call), d, err);
}

/*ARGSUSED*/
static ilu_cardinal
  _IIOP_SizeOfReal (ilu_Call call, double d, ILU_ERRS((IoErrs)) *err)
{
  ilu_cardinal n = PADDING_NEC(iiop_vop(call), 8) + 8;

  iiop_incr_vop(call, n);
  ILU_CLER(*err);
  return n;
}

/* ==================== long real ==================== */

static void
  _IIOP_OutputLongReal (ilu_Call call, ilu_longreal d, ILU_ERRS((IoErrs)) *err)
{
  _cdr_put_r128 (iiop_packet(call), d, err);
}

static void
  _IIOP_InputLongReal (ilu_Call call, ilu_longreal *d, ILU_ERRS((IoErrs)) *err)
{
  _cdr_get_r128 (iiop_packet(call), d, err);
}

/*ARGSUSED*/
static ilu_cardinal
  _IIOP_SizeOfLongReal (ilu_Call call, ilu_longreal d, ILU_ERRS((IoErrs)) *err)
{
  ilu_cardinal n = PADDING_NEC(iiop_vop(call), 8) + 16;

  iiop_incr_vop(call, n);
  ILU_CLER(*err);
  return (n);
}

/* ==================== short real ==================== */

static void
  _IIOP_OutputShortReal (ilu_Call call, float f, ILU_ERRS((IoErrs)) *err)
{
  _cdr_put_r32 (iiop_packet(call), f, err);
}

static void
  _IIOP_InputShortReal (ilu_Call call, float *f, ILU_ERRS((IoErrs)) *err)
{
  _cdr_get_r32 (iiop_packet(call), f, err);
}

/*ARGSUSED*/
static ilu_cardinal
  _IIOP_SizeOfShortReal (ilu_Call call, float d, ILU_ERRS((IoErrs)) *err)
{
  ilu_cardinal n = PADDING_NEC(iiop_vop(call), 4) + 4;

  iiop_incr_vop(call, n);
  ILU_CLER(*err);
  return (n);
}

/* ==================== bytes ==================== */

static void OutputBytes (ilu_Call call, ilu_bytes s, ilu_cardinal len,
			 ilu_cardinal limit, ilu_boolean nulterm, ilu_Error *err)
{
  ilu_cardinal size;

  if (limit > 0)
    size = MIN(len, limit);
  else
    size = len;

  size = size + (nulterm ? 1 : 0);

  _cdr_put_u32 (iiop_packet(call), size, err);
  if (ILU_ERROK(*err))
    {
      _cdr_put_opaque (iiop_packet(call), s, size - (nulterm ? 1 : 0), err);
      if (ILU_ERROK(*err) && nulterm)
	{
	  _cdr_put_u8 (iiop_packet(call), 0, err);
	}
    }
}

static void
  _IIOP_OutputBytes (ilu_Call call, ilu_bytes s, ilu_cardinal len, ilu_cardinal limit, ILU_ERRS((IoErrs)) *err)
{
  OutputBytes (call, s, len, limit, ilu_FALSE, err);
}

static void
  _IIOP_InputBytes (ilu_Call call, ilu_bytes *s, ilu_cardinal *len, ilu_cardinal limit, ILU_ERRS((IoErrs)) *err)
{
  *s = NIL;
  _cdr_get_bytes (iiop_packet(call), s, len, limit, err);
}

  /*ARGSUSED*/
static ilu_cardinal SizeOfBytes (ilu_Call call, ilu_bytes i, ilu_cardinal l, ilu_cardinal limit, ilu_boolean nulterm, ilu_Error *err)
{
  ilu_cardinal n = _IIOP_SizeOfCardinal (call, l, err);
  ilu_cardinal n2;

  if (ILU_ERRNOK(*err))
    return 0;
  if (((limit > 0) && (l > limit)) || l > IIOPMaxStringSize)
    {
      ILU_NOTE(IIOP_DEBUG,
	    ("(iiop.c:SizeOfBytes):  Attempt to pass bytestring of length %lu, which exceeds either IIOPMaxStringSize of %lu, or limit on this sequence type of %lu.\n",
	     l, IIOPMaxStringSize, limit));
      return (0);
    }

  n2 = l + (nulterm ? 1 : 0);
  iiop_incr_vop(call, n2);
  ILU_CLER(*err);
  return (n + n2);
}

  /*ARGSUSED*/
static ilu_cardinal
  _IIOP_SizeOfBytes (ilu_Call call, ilu_bytes i, ilu_cardinal l, ilu_cardinal limit, ILU_ERRS((IoErrs)) *err)
{
  return (SizeOfBytes (call, i, l, limit, ilu_FALSE, err));
}

/* ==================== wide string ==================== */

static void
  _IIOP_InputWString (ilu_Call call, ilu_wstring *s, ilu_cardinal *len, ilu_cardinal limit, ILU_ERRS((IoErrs)) *err)
{
  if (iiop_char_codeset(call) != ilu_IIOP_NATIVE_CHAR_CODE_SET_ID) {
    ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_char_codeset, 0);
    return;
  };
  _ilu_InputWString(call, s, len, (limit == 0) ? 0 : limit + 1, err);
}

/* ==================== string ==================== */

static ilu_cardinal
  figure_iiop_charset (ilu_cardinal iana_charset, enum byte_order byteorder)
{
  switch (iana_charset) {
  case ILU_StringEncoding_Unicode_1_1:
    if (byteorder == BigEndian)
      return IIOP_CHARSET_UNICODE_UCS_2;
    else
      return 0;
  case ILU_StringEncoding_Unicode_UCS_2:
    return IIOP_CHARSET_UNICODE_UCS_2;
  case ILU_StringEncoding_Unicode_UCS_4:
    return IIOP_CHARSET_UNICODE_UCS_4;
  case ILU_StringEncoding_latin1:
    return IIOP_CHARSET_ISO_LATIN1;
  case ILU_StringEncoding_UTF_8:
    return IIOP_CHARSET_UNICODE_UTF_8;
  case ILU_StringEncoding_US_ASCII:
    return IIOP_CHARSET_US_ASCII;
  default:
    return 0;
  }
}

static ilu_cardinal
  figure_iana_charset (ilu_cardinal iiop_charset)
{
  switch (iiop_charset) {
  case IIOP_CHARSET_UNICODE_UCS_2:
    return ILU_StringEncoding_Unicode_UCS_2;
  case IIOP_CHARSET_UNICODE_UCS_4:
    return ILU_StringEncoding_Unicode_UCS_4;
  case IIOP_CHARSET_ISO_LATIN1:
    return ILU_StringEncoding_latin1;
  case IIOP_CHARSET_UNICODE_UTF_8:
    return ILU_StringEncoding_UTF_8;
  case IIOP_CHARSET_US_ASCII:
    return ILU_StringEncoding_US_ASCII;
  default:
    return 0;
  }
}

static ilu_boolean
  wchar_iana_charset (ilu_cardinal iana_charset)
{
  return ((iana_charset == ILU_StringEncoding_Unicode_UCS_4) ||
	  (iana_charset == ILU_StringEncoding_Unicode_UCS_2) ||
	  (iana_charset == ILU_StringEncoding_Unicode_1_1));
}

static ilu_boolean
  wchar_iiop_charset (ilu_cardinal iiop_charset)
{
  return ((iiop_charset == IIOP_CHARSET_UNICODE_UCS_4) ||
	  (iiop_charset == IIOP_CHARSET_UNICODE_UCS_2));
}

static void
  _IIOP_OutputString (ilu_Call call, void * s, ilu_cardinal len,
		      ilu_cardinal limit,
		      ilu_cardinal expected_encoding,
		      ilu_cardinal current_encoding,
		      ILU_ERRS((IoErrs)) *err)
{
  ilu_cardinal iiopCharset = figure_iiop_charset(current_encoding, iiop_byte_order(call));

  if (((expected_encoding != 0) && (current_encoding != expected_encoding)) ||
      ((expected_encoding == 0) && (iiopCharset == 0))) {
    ILU_ERR_CONS1(imp_limit, err, minor, ilu_ilm_unsupported_charset_encoding, 0);
    return;
  }
  if (wchar_iiop_charset(iiopCharset) ? (iiop_char_codeset(call) != iiopCharset) :
      (iiop_short_char_codeset(call) != iiopCharset)) {
    ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_char_codeset, 0);
    return;
  }
  OutputBytes (call, (ilu_bytes) s, len, limit, ilu_TRUE, err);
}

static void
  _IIOP_InputString (ilu_Call call, void **s, ilu_cardinal *len,
		     ilu_cardinal limit, ilu_cardinal expected_encoding,
		     ilu_cardinal *current_encoding,
		     ILU_ERRS((IoErrs)) *err)
{
  *s = NIL;
  _cdr_get_bytes (iiop_packet(call), (ilu_byte **) s, len,
		  (limit == 0) ? 0 : limit + 1, err);
  if (expected_encoding == 0)
    *current_encoding = figure_iana_charset(iiop_short_char_codeset(call));
  else if ((expected_encoding == ILU_StringEncoding_Unicode_1_1) &&
	   (iiop_char_codeset(call) == IIOP_CHARSET_UNICODE_UCS_2) &&
	   (iiop_byte_order(call) == BigEndian))
    *current_encoding = ILU_StringEncoding_Unicode_1_1;
  else if ((expected_encoding == ILU_StringEncoding_Unicode_UCS_2) &&
	   (iiop_char_codeset(call) == IIOP_CHARSET_UNICODE_UCS_2))
    *current_encoding = ILU_StringEncoding_Unicode_UCS_2;
  else if ((expected_encoding == ILU_StringEncoding_Unicode_UCS_4) &&
	   (iiop_char_codeset(call) == IIOP_CHARSET_UNICODE_UCS_4))
    *current_encoding = ILU_StringEncoding_Unicode_UCS_4;
  else if ((expected_encoding == ILU_StringEncoding_latin1) &&
	   (iiop_short_char_codeset(call) == IIOP_CHARSET_ISO_LATIN1))
    *current_encoding = ILU_StringEncoding_latin1;
  else if ((expected_encoding == ILU_StringEncoding_US_ASCII) &&
	   (iiop_short_char_codeset(call) == IIOP_CHARSET_US_ASCII))
    *current_encoding = ILU_StringEncoding_US_ASCII;
  else if ((expected_encoding == ILU_StringEncoding_UTF_8) &&
	   (iiop_char_codeset(call) == IIOP_CHARSET_UNICODE_UTF_8))
    *current_encoding = ILU_StringEncoding_UTF_8;
  else {
    ilu_free(*s);
    ILU_ERR_CONS1(imp_limit, err, minor, ilu_ilm_unsupported_charset_encoding, 0);
    return;
  }
  if (ILU_ERROK(*err))
    *len -= 1;
}

  /*ARGSUSED*/
static ilu_cardinal
  _IIOP_SizeOfString (ilu_Call call, void *i, ilu_cardinal l,
		      ilu_cardinal limit,
		      ilu_cardinal expected_encoding,
		      ilu_cardinal current_encoding,
		      ILU_ERRS((IoErrs)) *err)
{
  ilu_cardinal iiopCharset = figure_iiop_charset(current_encoding, iiop_byte_order(call));

  if (((expected_encoding != 0) && (current_encoding != expected_encoding)) ||
      ((expected_encoding == 0) && (iiopCharset == 0))) {
    ILU_ERR_CONS1(imp_limit, err, minor, ilu_ilm_unsupported_charset_encoding, 0);
    return 0;
  }
  if (wchar_iiop_charset(iiopCharset) ? (iiop_char_codeset(call) != iiopCharset) :
      (iiop_short_char_codeset(call) != iiopCharset)) {
    ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_char_codeset, 0);
    return 0;
  }
  return (SizeOfBytes (call, (ilu_bytes) i, l, limit, ilu_TRUE, err));
}

/* ==================== byte ==================== */

static void
  _IIOP_OutputByte (ilu_Call call, ilu_byte b, ILU_ERRS((IoErrs)) *err)
{
  _cdr_put_u8 (iiop_packet(call), b, err);
}

static void
  _IIOP_InputByte (ilu_Call call, ilu_byte *b, ILU_ERRS((IoErrs)) *err)
{
  _cdr_get_u8 (iiop_packet(call), b, err);
}

/*ARGSUSED*/
static ilu_cardinal
  _IIOP_SizeOfByte (ilu_Call call, ilu_byte i, ILU_ERRS((IoErrs)) *err)
{
  ilu_cardinal n = PADDING_NEC(iiop_vop(call), 1) + 1;

  iiop_incr_vop(call, n);
  ILU_CLER(*err);
  return n;
}

/* ==================== short char ==================== */

static void
_IIOP_OutputShortChar(ilu_Call call, ilu_shortcharacter b,
		      ILU_ERRS((IoErrs)) * err)
{
  _IIOP_OutputByte(call, (ilu_byte) b, err);
}

static void
_IIOP_InputShortChar(ilu_Call call, ilu_shortcharacter * b,
		     ILU_ERRS((IoErrs)) * err)
{
  _IIOP_InputByte(call, (ilu_byte *) b, err);
}

/* ARGSUSED */
static          ilu_cardinal
_IIOP_SizeOfShortChar(ilu_Call call, ilu_shortcharacter i,
		      ILU_ERRS((IoErrs)) * err)
{
  return _IIOP_SizeOfByte(call, (ilu_byte) i, err);
}

/* ==================== boolean ==================== */

static void
  _IIOP_OutputBoolean (ilu_Call call, ilu_boolean b, ILU_ERRS((IoErrs)) *err)
{
  _cdr_put_u8 (iiop_packet(call), (ilu_byte) (b ? 1 : 0), err);
}

static void
  _IIOP_InputBoolean (ilu_Call call, ilu_boolean *b, ILU_ERRS((IoErrs)) *err)
{
  ilu_byte b2;

  _cdr_get_u8 (iiop_packet(call), &b2, err);
  if (ILU_ERROK(*err))
    *b = (b2 == 0) ? ilu_FALSE : ilu_TRUE;
}

/*ARGSUSED*/
static ilu_cardinal
  _IIOP_SizeOfBoolean (ilu_Call call, ilu_boolean i, ILU_ERRS((IoErrs)) *err)
{
  return (_IIOP_SizeOfByte(call, (ilu_byte) (i ? 1 : 0), err));
}

/* ==================== opaque ==================== */

static void
  _IIOP_OutputOpaque (ilu_Call call, ilu_bytes o, ilu_cardinal len, ILU_ERRS((IoErrs)) *err)
{
  _cdr_put_opaque (iiop_packet(call), o, len, err);
}

static void
  _IIOP_InputOpaque (ilu_Call call, ilu_bytes *o, ilu_cardinal len, ILU_ERRS((IoErrs)) *err)
{
  _cdr_get_opaque (iiop_packet(call), o, len, err);
}

static ilu_cardinal
  _IIOP_SizeOfOpaque (ilu_Call call, ilu_bytes o, ilu_cardinal len, ILU_ERRS((IoErrs)) *err)
{
  iiop_incr_vop(call, len);
  ILU_CLER(*err);
  return (len);
}

/* ==================== enumeration ==================== */

static void
  _IIOP_OutputEnumeration (ilu_Call call, ilu_shortcardinal i, ilu_Type the_type, ILU_ERRS((IoErrs)) *err)
{
  _cdr_put_u32 (iiop_packet(call), (ilu_cardinal) i, err);
}

static void
  _IIOP_InputEnumeration (ilu_Call call, ilu_shortcardinal *i, ilu_Type the_type, ILU_ERRS((IoErrs)) *err)
{
  ilu_cardinal i2;

  _cdr_get_u32 (iiop_packet(call), &i2, err);
  if (ILU_ERROK(*err))
    *i = i2;
}

/*ARGSUSED*/
static ilu_cardinal
  _IIOP_SizeOfEnumeration (ilu_Call call, ilu_shortcardinal i, ilu_Type the_type, ILU_ERRS((IoErrs)) *err)
{
  return (_IIOP_SizeOfCardinal(call, (ilu_cardinal) i, err));
}

/* ==================== sequence ==================== */

static void
  _IIOP_OutputSequence (ilu_Call c, ilu_cardinal sequenceLength, ilu_cardinal limit, ilu_Type the_type, ILU_ERRS((IoErrs)) *err)
{
  _IIOP_OutputCardinal (c, sequenceLength, err);
}

static void
  _IIOP_OutputSequenceMark (ilu_Call c, ilu_cardinal extent, ILU_ERRS((IoErrs)) *err)
{
  ILU_CLER(*err);
}

static void
  _IIOP_InputSequenceMark (ilu_Call c, ilu_cardinal extent, ILU_ERRS((IoErrs)) *err)
{
  ILU_CLER(*err);
}

static void
  _IIOP_InputSequence (ilu_Call c, ilu_cardinal *sequenceLength, ilu_cardinal limit, ilu_Type the_type, ILU_ERRS((IoErrs)) *err)
{
  ilu_cardinal len;

  _IIOP_InputCardinal (c, &len, err);
  if (ILU_ERROK(*err))
    *sequenceLength = len;
}

static void
  _IIOP_EndSequence (ilu_Call c, ILU_ERRS((IoErrs)) *err)
{
  ILU_CLER(*err);
}

static ilu_cardinal
  _IIOP_SizeOfSequence (ilu_Call c, ilu_cardinal length, ilu_cardinal limit, ilu_Type the_type, ILU_ERRS((IoErrs)) *err)
{
  return (_IIOP_SizeOfCardinal(c, length, err));
}

/* ==================== optional ==================== */

static void
  _IIOP_OutputOptional (ilu_Call call, ilu_boolean i, ilu_Type the_type, ILU_ERRS((IoErrs)) *err)
{
  _IIOP_OutputCardinal (call, (i ? 1 : 0), err);
}

static void
  _IIOP_InputOptional (ilu_Call call, ilu_boolean *i, ilu_Type the_type, ILU_ERRS((IoErrs)) *err)
{
  ilu_cardinal len;

  _IIOP_InputCardinal (call, &len, err);
  if (ILU_ERROK(*err))
    *i = (len != 0);
}

/*ARGSUSED*/
static ilu_cardinal
  _IIOP_SizeOfOptional (ilu_Call call, ilu_boolean i, ilu_Type the_type, ILU_ERRS((IoErrs)) *err)
{
  return (_IIOP_SizeOfCardinal(call, (i ? 1 : 0), err));
}

/* ==================== union ==================== */

static void
  _IIOP_OutputUnion (ilu_Call c, ilu_cardinal typeIndex,
		     ilu_TypeKind tk, ilu_Type the_type,
		     ILU_ERRS((IoErrs)) *err)
{
  switch (tk)
    {
    case ilu_byte_tk:
    case ilu_boolean_tk:
    case ilu_shortcharacter_tk:
      _IIOP_OutputByte (c, (ilu_byte) typeIndex, err);
      break;
    case ilu_shortcardinal_tk:
    case ilu_shortinteger_tk:
    case ilu_character_tk:
      _IIOP_OutputShortCardinal (c, (ilu_shortcardinal) typeIndex, err);
      break;
    case ilu_cardinal_tk:
    case ilu_integer_tk:
    case ilu_enumeration_tk:
      _IIOP_OutputCardinal (c, (ilu_cardinal) typeIndex, err);
      break;
    default:
      ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_unionDiscSize, 0);
    };
}

static void
  _IIOP_InputUnion (ilu_Call c, ilu_cardinal *typeIndex,
		    ilu_TypeKind tk, ilu_Type the_type,
		    ILU_ERRS((IoErrs)) *err)
{
  ilu_cardinal i;
  ilu_byte b;
  ilu_shortcardinal s;

  switch (tk)
    {
    case ilu_byte_tk:
    case ilu_boolean_tk:
    case ilu_shortcharacter_tk:
      _IIOP_InputByte (c, &b, err);
      break;
    case ilu_shortcardinal_tk:
    case ilu_shortinteger_tk:
    case ilu_character_tk:
      _IIOP_InputShortCardinal (c, &s, err);
      break;
    case ilu_cardinal_tk:
    case ilu_integer_tk:
    case ilu_enumeration_tk:
      _IIOP_InputCardinal (c, &i, err);
      break;
    default:
      ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_unionDiscSize, 0);
    };
  if (ILU_ERROK(*err))
    {
      switch (tk)
	{
	case ilu_byte_tk:
	case ilu_boolean_tk:
	case ilu_shortcharacter_tk:
	  *typeIndex = b;
	  break;
	case ilu_shortcardinal_tk:
	case ilu_shortinteger_tk:
	case ilu_character_tk:
	  *typeIndex = s;
	  break;
	case ilu_cardinal_tk:
	case ilu_integer_tk:
	case ilu_enumeration_tk:
	  *typeIndex = i;
	  break;
	default:
	  ;
	};
    }
}

static void
  _IIOP_EndUnion (ilu_Call c, ILU_ERRS((IoErrs)) *err)
{
  ILU_CLER(*err);
}

static ilu_cardinal
  _IIOP_SizeOfUnion (ilu_Call c, ilu_cardinal typeIndex,
		     ilu_TypeKind tk,
		     ilu_Type the_type, ILU_ERRS((IoErrs)) *err)
{
  switch (tk)
    {
    case ilu_byte_tk:
    case ilu_boolean_tk:
    case ilu_shortcharacter_tk:
      return (_IIOP_SizeOfByte(c, (ilu_byte) typeIndex, err));
    case ilu_shortcardinal_tk:
    case ilu_shortinteger_tk:
    case ilu_character_tk:
      return (_IIOP_SizeOfShortCardinal(c, (ilu_shortcardinal) typeIndex, err));
    case ilu_cardinal_tk:
    case ilu_integer_tk:
    case ilu_enumeration_tk:
      return (_IIOP_SizeOfCardinal(c, typeIndex, err));
    default:
      ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_unionDiscSize, 0);
    }
  return 0;
}

/* ==================== array ==================== */

static void
  _IIOP_OutputArray (ilu_Call c, ilu_cardinal len, ilu_Type the_type, ILU_ERRS((IoErrs)) *err)
{
  ILU_CLER(*err);
}

static void
  _IIOP_InputArray (ilu_Call c, ilu_Type the_type, ILU_ERRS((IoErrs)) *err)
{
  ILU_CLER(*err);
}

static void
  _IIOP_EndArray (ilu_Call c, ILU_ERRS((IoErrs)) *err)
{
  ILU_CLER(*err);
}

static ilu_cardinal
  _IIOP_SizeOfArray (ilu_Call c, ilu_cardinal len, ilu_Type the_type, ILU_ERRS((IoErrs)) *err)
{
  ILU_CLER(*err);
  return(0);
}

/* ==================== record ==================== */

static void
  _IIOP_OutputRecord (ilu_Call c, ilu_Type the_type, ILU_ERRS((IoErrs)) *err)
{
  ILU_CLER(*err);
}

static void
  _IIOP_InputRecord (ilu_Call c, ilu_Type the_type, ILU_ERRS((IoErrs)) *err)
{
  ILU_CLER(*err);
}

static void
  _IIOP_EndRecord (ilu_Call c, ILU_ERRS((IoErrs)) *err)
{
  ILU_CLER(*err);
}

static ilu_cardinal
  _IIOP_SizeOfRecord (ilu_Call c, ilu_Type the_type, ILU_ERRS((IoErrs)) *err)
{
  ILU_CLER(*err);
  return(0);
}

/* ==================== object ==================== */

/*L2 >= {call's connection's callmu, iomu}
  h != NIL => all the following:
  before: Inside(s, cl);
  after:				 L1 disjoint {cmu, s};
  after: cl collectible		      => L1  not >=  {gcmu};
  after: cl collectible & s surrogate => Main Invariant holds;
  where s = h's server and cl = h's type.
  (We don't really need to hold cmu for surrogate or non-collectible
   objects, but this is convenient because ilu_Enter/ExitServer can
   be used.)*/
static ilu_boolean
  _IIOP_OutputObjectID (ilu_Call call, ilu_Object h,
			ilu_boolean discriminator_p,
			ilu_Class static_type, ILU_ERRS((IoErrs)) *err)
{
  ilu_bytes	ostr = NIL;	/* discriminator_p ? OID : SBH */
  ilu_cardinal	ostr_len = 0;	/* length of ostr buffer */
  struct IIOP_IOR_IOR	*iorp;
  ilu_string	tstr = NIL;	/* most specific type string */
  ilu_boolean	is_nil = (h == NIL);
  struct IIOP_DataBlock *s = (struct IIOP_DataBlock *) connection_protocol_data(call_connection(call));

  tstr = (h != NIL) ? ((object_mstid(h) != NIL) ? object_mstid(h) : class_unique_id(object_class(h))) : "";
  ILU_NOTE(OBJECT_DEBUG,
	("(ilu:_IIOP_OutputObjectID:  m-s type id for %s/%s is <%s>\n",
	 (h != NIL) ? server_id(object_server(h)) : "NIL",
	 (h != NIL) ? object_ih(h) : "NIL", tstr));

  if (discriminator_p)
    {
      ostr = iiop_objKey(call);
      ostr_len = iiop_objKeyLen(call);
      iiop_objKey(call) = NIL;
    }
  else {
    if (h == NIL && (discriminator_p || !static_type->cl_optional))
      return ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_nil, 0);
    iorp = FindIORForObject(h);
    /* We just sized this object; there'd better be an IOR for it! */
    _ilu_Assert(iorp!=NIL,"unexpected NIL IOR pointer");
  }

  if (ostr == NIL && iorp->type_id == NIL) {
    ilu_ExitServer(object_server(h), object_class(h));
    return ILU_ERR_CONS1(internal, err, minor, ilu_im_broken, ilu_FALSE);
  }
  if (!is_nil) {
    if (object_is_true(h) && object_collectible(h)) {
      object_lastRemote(h) = ilu_CoarseTime_Now();
      *err = _ilu_TouchedObj(h);
      ILU_MUST_BE_SUCCESS(*err);
    }
    ilu_ExitServer(object_server(h), object_class(h));
  }
  if (discriminator_p)
    {
      char buf[2048];

      protocol_output_bytes(call_proto(call), call, ostr, ostr_len, 0xFFFF, err);
      if (ostr != s->key)
	ilu_free(ostr);
      if (ILU_ERRNOK(*err))
	return ilu_FALSE;
      FormMethodName (buf, call_method(call));
      /* method name */
      if (protocol_output_string(call_proto(call), call, (ilu_bytes) buf,
				 strlen(buf), 0xFFFF,
				 ILU_StringEncoding_latin1,
				 ILU_StringEncoding_latin1,
				 err), ILU_ERRNOK(*err))
	return ilu_FALSE;
      /* principal */
      if (protocol_output_bytes(call_proto(call), call, (ilu_bytes) buf, 0, 0xFFFF, err), ILU_ERRNOK(*err))
	return ilu_FALSE;
    }
  else
    {
      ilu_cardinal i;

      if (protocol_output_string (call_proto(call), call,
				  (ilu_bytes) iorp->type_id, strlen(iorp->type_id),
				  0xFFFF, ILU_StringEncoding_latin1,
				  ILU_StringEncoding_latin1, err),
	  ILU_ERRNOK(*err))
	return ilu_FALSE;
      if (protocol_output_cardinal (call_proto(call), call, iorp->nProfiles, err), ILU_ERRNOK(*err))
	return ilu_FALSE;
      for (i = 0;  i < iorp->nProfiles;  i++)
	{
	  if (protocol_output_cardinal (call_proto(call), call, iorp->Profile[i].tag, err), ILU_ERRNOK(*err))
	    return ilu_FALSE;
	  if (protocol_output_bytes(call_proto(call), call, iorp->Profile[i].profileData,
				    iorp->Profile[i].profileDataLen, 0xFFFF, err), ILU_ERRNOK(*err))
	    return ilu_FALSE;
	}
    }
  return (ILU_CLER(*err));
}

/*before: L1 = {},
  after:  *h!=NIL => Inside(*h's server, static_type);
  after:  *h==NIL => L1 = {};
  forall conn: (L2 >= {conn.iomu}) => (L2 >= {conn.callmu});
  L2 >= {call's connection's callmu, iomu};
  Main otherwise unconstrained*/
static ilu_boolean
  _IIOP_InputObjectID (ilu_Call call, ilu_Object *h,
		       ilu_boolean discriminator_p,
		       ilu_Class static_type,
		       ILU_ERRS((IoErrs)) *err)
{
  ilu_Server	server = connection_server(call_connection(call));

  *h = NIL;
  if (static_type == NIL)
    {
      ILU_NOTE(INCOMING_DEBUG | OBJECT_DEBUG,
	    ("_IIOP_InputObjectID(disc=%ld, static_type=NIL)\n",
	     (long) discriminator_p));
      return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_nil, ilu_FALSE);
    };

  if (discriminator_p)
    {
      ilu_string	ih = NIL, sid = NIL, mstid = NIL;
      ilu_bytes		key;
      ilu_cardinal	keylen;

      /* with the IIOP, the discriminator (or "object key") is input during the
	 InterpretRequest call, and stored in iiop_objKey(call), with the length
	 in iiop_objKeyLen(call). */

      key = iiop_objKey(call);
      keylen = iiop_objKeyLen(call);
      iiop_objKey(call) = NIL;
      iiop_objKeyLen(call) = 0;

      if (key == NIL || strncmp((ilu_string) key, "ilu", 4) != 0)
	{
	  char ihbuf[2048];
	  char *newkey;
	  ILU_NOTE(INCOMING_DEBUG,
		("_IIOP_InputObjectID:  incoming oid not an ILU object-key"));
	  /* we assume that it's a "foreign" object key somehow placed in this server */
	  strcpy(ihbuf, CORBA_NATIVE_OBJECT_IH_PREFIX);
	  if (newkey = _ilu_EncodeBuffer((char *) key, keylen, err), ILU_ERROK(*err)) {
	    strcat (ihbuf, newkey);
	    ilu_free(newkey);
	    ilu_EnterServer(server, static_type);
	    if ((*h = _ilu_FindObjectInServer(ihbuf, server)) == NIL) {
	      ILU_NOTE(IIOP_DEBUG,
		       ("ILU(iiop.c:FindClassFromObjectKey):  no object in server <%s> with ih <%s>\n",
			server_id(server), ihbuf));
	      ilu_ExitServer(server, static_type);
	      ILU_ERR_CONS1(marshal, err, minor, ilu_mm_alien_disc, 6);
	    } else {
	      ILU_CLER(*err);
	    }
	  };
	}
      else
	{
	  mstid = ((char *) key) + 4;
	  sid = mstid + strlen(mstid) + 1;
	  ih = sid + strlen(sid) + 1;

	  ilu_EnterServer(server, static_type);
	  if (strcmp(sid, server_id(server)) != 0) {
	    ILU_NOTE(INCOMING_DEBUG,
		  ("%s %s is for wrong server (not %s).\n",
		   "_IIOP_InputObjectID:  incoming oid sid", sid,
		   server_id(server)));
	    ILU_ERR_CONS1(marshal, err, minor, ilu_mm_alien_disc, 6);
	    ilu_ExitServer(server, static_type);
	  } else if (server_objs(server) == NIL) {
	    ILU_NOTE(INCOMING_DEBUG,
		  ("%s %s is in closed server %s.\n",
		   "_IIOP_InputObjectID:  instance", ih, sid));
	    ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_svr_closed, NIL);
	    ilu_ExitServer(server, static_type);
	  } else if ((*h = _ilu_FindObjectInServer(ih, server)) == NIL) {
	    ILU_NOTE(INCOMING_DEBUG,
		  ("%s %s not found in server %s.\n",
		   "_IIOP_InputObjectID:  instance", ih, sid));
	    ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_inst_nf, NIL);
	    ilu_ExitServer(server, static_type);
	  } else
	    ILU_CLER(*err);
	}
      ilu_free(key);
      if (ILU_ERRNOK(*err)) {
	return ilu_FALSE;
      }
    }
  else
    {
      /* not discriminant */

      ilu_string typeid;
      ilu_cardinal nprofiles;
      struct IIOP_IOR_IOR *p;
      ilu_cardinal len, junk;

      if (protocol_input_string(call_proto(call), call, (void **) &typeid,
				&len, 0xFFFF, ILU_StringEncoding_latin1,
				&junk, err), ILU_ERRNOK(*err))
	return ilu_FALSE;
      if (protocol_input_cardinal(call_proto(call), call, &nprofiles, err), ILU_ERRNOK(*err))
	return ilu_FALSE;
      if (nprofiles == 0)
	{
	  ilu_free(typeid);
	  *h = NIL;
	}	  
      else
	{
	  ilu_cardinal i;

	  if (p = ilu_MallocE(sizeof(struct IIOP_IOR_IOR) + ((nprofiles - 1) * sizeof(struct IIOP_IOR_TaggedProfile)), err),
	      ILU_ERRNOK(*err)) {
	    ilu_free(typeid);
	    return ilu_FALSE;
	  };
	  p->type_id = typeid;
	  p->nProfiles = 0;
	  for (i = 0;  i < nprofiles;  i++)
	    {
	      if (protocol_input_cardinal(call_proto(call), call, &p->Profile[i].tag, err), ILU_ERRNOK(*err))
		{
		  FreeIORData(p);
		  ilu_free(p);
		  return ilu_FALSE;
		}
	      p->Profile[i].profileData = NIL;
	      if (protocol_input_bytes(call_proto(call), call, &p->Profile[i].profileData,
				       &p->Profile[i].profileDataLen, 0xFFFF, err), ILU_ERRNOK(*err))
		{
		  FreeIORData(p);
		  ilu_free(p);
		  return ilu_FALSE;
		}
	      p->nProfiles++;
	    }
	  *h = ObjectOfIOR (p, static_type, err);
	  if (ILU_ERRNOK(*err))
	    {
	      ILU_NOTE(INCOMING_DEBUG,
		    ("_IIOP_InputObjectID:  error:  No object for IOR.\n"));
	      return ilu_FALSE;
	    }
	  else if (*h != NIL)
	    PossiblyRegisterIORForObject (*h, p);
	  else
	    {
	      FreeIORData(p);
	      ilu_free(p);
	      return ilu_FALSE;
	    }	    
	}
      
      if (*h == NIL) {
	if (static_type->cl_optional) {
	  return ilu_TRUE;
	} else {
	  ILU_NOTE(INCOMING_DEBUG,
		("_IIOP_InputObjectID:  bad NIL obj.\n"));
	  return ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_nil, ilu_FALSE);
	}
      }
    }
  return ilu_TRUE;
}

/*h!=NIL => L1 >= {obj's server};
  h==NIL => L1 unconstrained.
  L2, Main unconstrained*/
static ilu_cardinal _IIOP_SizeOfObjectID(ilu_Call call, ilu_Object h,
					 ilu_boolean discriminator_p,
					 ilu_Class static_type,
					 ilu_Error *err)
{
  ilu_bytes	ostr = NIL;	/* discriminator_p ? OID : SBH */
  ilu_cardinal	ostr_len = 0;	/* length of ostr buffer */
  struct IIOP_IOR_IOR	*iorp;
  ilu_string	tstr = NIL;	/* most specific type string */
  ilu_cardinal	size = 0;
  ilu_boolean	is_nil = (h == NIL);
  struct IIOP_DataBlock *s = (struct IIOP_DataBlock *) connection_protocol_data(call_connection(call));

  if (call_connection(call) == NIL) {
    return ILU_ERR_CONS1(internal, err, minor, ilu_im_broken, 0);
  }

  Initialize(err);
  if (ILU_ERRNOK(*err)) return 0;

  tstr = (h != NIL) ? ((object_mstid(h) != NIL) ? object_mstid(h) : class_unique_id(object_class(h))) : "";
  ILU_NOTE(OBJECT_DEBUG,
	("(ilu:_IIOP_SizeOfObjectID:  m-s type id for %s/%s is <%s>\n",
	 (h != NIL) ? server_id(object_server(h)) : "NIL",
	 (h != NIL) ? object_ih(h) : "NIL", tstr));

  if (discriminator_p)
    {
      if (s->key != NIL) {
	ostr = s->key;
	ostr_len = s->keylen;
      } else {
	int sidl = strlen(server_id(object_server(h))), ihl = strlen(object_ih(h)), mstidl = strlen(tstr);
	
	ostr_len = sidl + ihl + mstidl + 7;
	ostr = (ilu_bytes) ilu_malloc (ostr_len);
	if (ostr == NIL)
	  return ILU_ERR_CONS1(no_memory, err, nbytes, ostr_len, 0);
	memcpy ((void *) ostr, "ilu", 4);
	memcpy ((void *) (ostr + 4), tstr, mstidl + 1);
	memcpy ((void *) (ostr + mstidl + 5), server_id(object_server(h)), sidl + 1);
	memcpy ((void *) (ostr + mstidl + sidl + 6), object_ih(h), ihl + 1);
      }
    }
  else {
    if (h == NIL && (discriminator_p || !static_type->cl_optional))
      return ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_nil, 0);
    if ((iorp = FindIORForObject(h)) == NIL)
      {
	if ((*err = IOROfObject(h, &iorp)), ILU_ERRNOK(*err))
	  return (0);
	RegisterIORForObject(h, iorp);
      }
  }

  if (ostr == NIL && iorp == NIL)
    return ILU_ERR_CONS1(internal, err, minor, ilu_im_broken, 0);

  if (!is_nil) {
    if (object_is_true(h) && object_collectible(h)) {
      object_lastRemote(h) = ilu_CoarseTime_Now();
      *err = _ilu_TouchedObj(h);
      ILU_MUST_BE_SUCCESS(*err);
    }
  }
  if (discriminator_p)
    {
      char buf[2048];
      ilu_cardinal size3;

      iiop_vop(call) = 0;
      size3 = 4 + 1 + 1 + 1 + 1 + 4;	/* magic + major version + minor version + msg type + byte order + size */
      if (!s->charsets_sent) {
	size3 += 4 /* one service context */
	  + 4 /* service context identifier */
	    + 4 /* length of service context encapsulation */
	      + CharSetsServiceContextPaddedLength; /* code set service context */
      } else {
	size3 += 4;	/* no service contexts */
      }
      size3 += 4;			/* serial number ("request id") */
      size3 += 1;			/* response expected? */

      /* initialize vop to expected header size */
      iiop_vop(call) = (ilu_bytes) size3;

      /* now calculate the size of the discrimant, method, and principal all in one lump */
      size3 = _IIOP_SizeOfBytes (call, (ilu_bytes) ostr, ostr_len, 0xFFFF, err);	/* object key */
      if (ILU_ERRNOK(*err))
	return 0;
      size += size3;
      FormMethodName (buf, call_method(call));
      size3 = _IIOP_SizeOfString (call, (ilu_bytes) buf, strlen(buf), 0xFFFF,		/* method name */
				  ILU_StringEncoding_latin1, ILU_StringEncoding_latin1,
				  err);
      if (ILU_ERRNOK(*err))
	return 0;
      size += size3;
      size3 = _IIOP_SizeOfBytes (call, (ilu_bytes) buf, 0, 0xFFFF, err);		/* Principal */
      if (ILU_ERRNOK(*err))
	return 0;
      size += size3;
      iiop_objKey(call) = ostr;
      iiop_objKeyLen(call) = ostr_len;
    }
  else
    {
      ilu_cardinal i;
      ilu_cardinal size3;

      size3 = _IIOP_SizeOfString (call, (ilu_bytes) iorp->type_id, strlen(iorp->type_id), 0xFFFF,
				  ILU_StringEncoding_latin1, ILU_StringEncoding_latin1,
				  err);
      if (ILU_ERRNOK(*err)) return 0;
      size += size3;
      size3 = _IIOP_SizeOfCardinal (call, iorp->nProfiles, err);
      if (ILU_ERRNOK(*err)) return 0;
      size += size3;
      for (i = 0;  i < iorp->nProfiles;  i++)
	{
	  size3 = _IIOP_SizeOfCardinal (call, iorp->Profile[i].tag, err);
	  if (ILU_ERRNOK(*err)) return 0;
	  size += size3;
	  size3 = _IIOP_SizeOfBytes (call, iorp->Profile[i].profileData,
				     iorp->Profile[i].profileDataLen, 0xFFFF, err);
	  if (ILU_ERRNOK(*err)) return 0;
	  size += size3;
	}
    }
  ILU_NOTE(IIOP_DEBUG, ("_IIOP_SizeOfObjectID (%p, %p, %s, %s) => %lu\n",
			call, h, discriminator_p ? "is disc" : "not disc",
			class_name(static_type), size));
  ILU_CLER(*err);
  return (size);
}

#ifdef ADD_VARIANT_SUPPORT

/*======================================================================
**======================================================================
**====================  Pickle, Any, and TypeCode ======================
**======================================================================
**====================================================================*/

typedef struct _TypeStack_s {
  ilu_Type		type;
  ilu_integer		value;
  struct _TypeStack_s *	next;
} TypeStack_s, *TypeStack;

static ilu_integer
  has_type(TypeStack s, ilu_Type t)
{
  TypeStack ptr = s;

  while (ptr != NIL)
    {
      if (ptr->type == t)
	return ptr->value;
      ptr = ptr->next;
    }
  return -1;
}

static ilu_Type
  find_type (TypeStack s, ilu_integer index)
{
  TypeStack ptr = s;

  while (ptr != NIL)
    {
      if (ptr->value == index)
	return ptr->type;
      ptr = ptr->next;
    }
  return NIL;
}

static TypeStack
  add_type (TypeStack s, ilu_Type t, ilu_integer where, ilu_Error *err)
{
  TypeStack ptr = s;

  while (ptr != NIL)
    {
      if (ptr->type == t)
	return s;
      else
	ptr = ptr->next;
    }
  /* t is not on list;  add it */
  ptr = ilu_MallocE(sizeof(TypeStack_s), err);
  if (ILU_ERRNOK(*err)) return NIL;
  ptr->type = t;
  ptr->value = where;
  ptr->next = s;
  return ptr;
}

static void
  replace_type (TypeStack s, ilu_Type newtype, ilu_integer where)
{
  TypeStack ptr = s;

  for (ptr = s;  ptr != NIL;  ptr = ptr->next) {
    if (ptr->value == where)
      ptr->type = newtype;
  };
}

static void
  free_list (TypeStack s)
{
  if (s == NIL)
    return;
  else {
    free_list(s->next);
    ilu_free(s);
  }
}

typedef struct _TypeEncapsulation_s {
  ilu_cardinal	ctk;		/* CORBA IIOP type_kind of type */
  ilu_cardinal	size;		/* length in bytes of marshalled form, not including type_kind */
  ilu_bytes	bytes;		/* buffer containing marshalled type */
  ilu_boolean	permanent;	/* true if stored in hash table */
} *TypeEncapsulation;

static void
  FreeTypeEncapsulation (TypeEncapsulation t)
{
  if (t == NIL || t->permanent) return;
  ilu_free(t->bytes);
  ilu_free(t);
}

static ilu_Type
  ur_type (ilu_Type t)
{
  ilu_Type p = t;

  while (type_kind(p) == ilu_alias_tk)
    p = type_desc(t).Alias.type;
  return p;
}

static ilu_shortinteger CORBA_type_kind (ilu_Type t)
{
  if ((type_kind(ur_type(t)) == ilu_sequence_tk) &&
      (type_kind(ur_type(type_desc(t).Sequence.type)) == ilu_shortcharacter_tk))
    return CORBA_tk_string;
  else switch (type_kind(t))
    {
    case ilu_string_tk:
      return CORBA_tk_string;
    case ilu_byte_tk:
      return CORBA_tk_byte;
    case ilu_boolean_tk:
      return CORBA_tk_boolean;
    case ilu_character_tk:
      return CORBA_tk_character;
    case ilu_shortcharacter_tk:
      return CORBA_tk_shortcharacter;
    case ilu_shortinteger_tk:
      return CORBA_tk_shortinteger;
    case ilu_integer_tk:
      return CORBA_tk_integer;
    case ilu_longinteger_tk:
      return CORBA_tk_longinteger;
    case ilu_shortcardinal_tk:
      return CORBA_tk_shortcardinal;
    case ilu_cardinal_tk:
      return CORBA_tk_cardinal;
    case ilu_longcardinal_tk:
      return CORBA_tk_longcardinal;
    case ilu_real_tk:
      return CORBA_tk_real;
    case ilu_shortreal_tk:
      return CORBA_tk_shortreal;
    case ilu_longreal_tk:
      return CORBA_tk_longreal;
    case ilu_object_tk:
      return CORBA_tk_object;
    case ilu_optional_tk:
      return CORBA_tk_optional;
#ifdef ILU_REFERENCE_TYPES
    case ilu_reference_tk:
      return CORBA_tk_reference;
#endif /* def ILU_REFERENCE_TYPES */
    case ilu_alias_tk:
      return CORBA_tk_alias;
    case ilu_union_tk:
      return CORBA_tk_union;
    case ilu_sequence_tk:
      return CORBA_tk_sequence;
    case ilu_record_tk:
      return CORBA_tk_record;
    case ilu_array_tk:
      return CORBA_tk_array;
    case ilu_enumeration_tk:
      return CORBA_tk_enumeration;
    case ilu_pickle_tk:
      return CORBA_tk_variant;
    default:
      return -1;
    }
}

static ilu_boolean is_non_CORBA_ILU_enum (ilu_Type t)
{
  ilu_cardinal i;

  if (type_kind(t) != ilu_enumeration_tk)
    return ilu_FALSE;
  else {
    for (i = 0;  i < type_desc(t).Enumeration.n_elements; i++)
      if (type_desc(t).Enumeration.elements[i].value != i)
	return ilu_TRUE;
  };
  return ilu_FALSE;
}

static ilu_string form_type_name (ilu_Type t, char *buf, ilu_cardinal bufsize, ilu_Error *err)
{
  ilu_cardinal len;
  char *ret;

  if (strcmp(type_interface_name(t), ForeignTypeInterface) == 0) {
    if (strlen(type_interface_name(t)) >= (bufsize - 1)) {
      return ilu_StrdupE(type_interface_name(t), err);
    } else {
      strcpy (buf, type_interface_name(t));
      ILU_CLER(*err);
      return buf;
    }
  } else if (strncmp(type_uid(t), "IDL:", 4) == 0) {
    /* We assume this type came from an IDL specification, so we want to
       reconstruct the IDL name for the type, which we do from the type UID. */
    char *p1, *p2;
    char *ret;

    for (p1 = type_uid(t) + strlen(type_uid(t));  p1 >= type_uid(t);  p1--)
      if (*p1 == ':')
	break;
    for (p2 = p1 - 1;  p2 >= type_uid(t);  p2--)
      if ((*p2 == ':') || (*p2 == '/'))
	break;
    if (p2 < type_uid(t))
      return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_typeID, NIL);
    if ((p1 - p2) >= (bufsize - 1)) {
      ret = ilu_MallocE(p1 - p2, err);
      if (ILU_ERRNOK(*err)) return NIL;
      ret = buf;
    } else {
      ILU_CLER(*err);
      ret = buf;
    }      
    strncpy(ret, p2+1, p1 - p2 - 1);
    ret[p1 - p2 - 1] = 0;
    return ret;
  } else {
    len = 15 +
      strlen(type_name(t)) +
	strlen(type_interface_name(t)) +
	  ((type_interface_brand(t) == NIL) ? 0 : strlen(type_interface_brand(t)));
    if (len >= bufsize) {
      ret = ilu_MallocE(strlen(type_interface_name(t)) + 1, err);
      if (ILU_ERRNOK(*err)) return NIL;
    } else {
      ILU_CLER(*err);
      ret = buf;
    }
    sprintf(ret, "_ilu_%s___%s___%s", type_interface_name(t),
	    (type_interface_brand(t) == NIL) ? "" : type_interface_brand(t),
	    type_name(t));
    return ret;
  }
}

static ilu_cardinal
  enum_code (ilu_string s, ilu_Type t)
{
  ilu_cardinal i;

  for (i = 0;  i < type_desc(t).Enumeration.n_elements;  i++) {
    if (strcmp(s, type_desc(t).Enumeration.elements[i].name) == 0)
      return type_desc(t).Enumeration.elements[i].value;
  }
  return 0;
}

static void
  _IIOP_AddType (PACKET pk, ilu_Type type, TypeStack *stack, ilu_boolean anonymous_pass, ilu_Error *err);

static void 
  _IIOP_FormTypeEncapsulation (PACKET pk,
			       ilu_cardinal *rctk,
			       ilu_Type t,
			       TypeStack *stack,			/* what's been marshalled already */
			       ilu_boolean marshalled_alias,		/* flag for anonymous types */
			       ILU_ERRS((IoErrs)) * err)
{
  ilu_shortinteger ctk = CORBA_type_kind(t);
  char typenamebuf[1024];	/* local buffer to possibly avoid mallocs */
  char *typename = NIL;

  *rctk = ctk;

  switch (ctk)
    {
      /* no parameters, so just marshal type_kind */

    case CORBA_tk_null:
    case CORBA_tk_void:
    case CORBA_tk_shortinteger:
    case CORBA_tk_integer:
    case CORBA_tk_shortcardinal:
    case CORBA_tk_cardinal:
    case CORBA_tk_shortreal:
    case CORBA_tk_real:
    case CORBA_tk_boolean:
    case CORBA_tk_shortcharacter:
    case CORBA_tk_byte:
    case CORBA_tk_longinteger:
    case CORBA_tk_longcardinal:
    case CORBA_tk_longreal:
    case CORBA_tk_character:
    case CORBA_tk_variant:
    case CORBA_tk_type:
    case CORBA_tk_Principal:
      break;

    case CORBA_tk_sequence:
    case CORBA_tk_string:
    case CORBA_tk_array:
      /* for sequence, string, and array types, which are anonymous in OMG IDL, but named in
	 ILU ISL, we marshal them twice:  first as an alias type, with the type ID and
	 name, and then the second pass as an anonymous array, string, or sequence type.
	 We decide whether we should do the first or second pass by the value of
	 marshalled_alias, which is false for the first pass, and true for the second
	 pass. */
      if (!marshalled_alias &&
	  /* For multi-dimensional arrays, we may have to invent multiple levels of
	     anonymous types, one for each dimension.  These types don't have a valid
	     type_uid, so don't process them here... */
	  ((ctk != CORBA_tk_array) || (type_uid(t) != NIL))) {
	ilu_TypeName_s faketypename;
	ilu_Type_s faketype;
	ilu_cardinal underctk;

	*rctk = CORBA_tk_alias;
	type_names(&faketype) = &faketypename;
	type_names_count(&faketype) = 1;
	type_name(&faketype) = type_name(t);
	type_interface_name(&faketype) = type_interface_name(t);
	type_interface_brand(&faketype) = type_interface_brand(t);
	type_uid(&faketype) = type_uid(t);
	type_kind(&faketype) = ilu_alias_tk;
	type_desc(&faketype).Alias.type = t;
	_IIOP_FormTypeEncapsulation (pk, &underctk, &faketype, stack, ilu_TRUE, err);
	break;
      } else if (ctk == CORBA_tk_string) {	/* `simple' parms */
	_cdr_put_u32(pk, type_desc(t).Sequence.limit, err);
	break;
      };	/* otherwise (array or sequence) drop down into the next case */

      /* `complex' parameters */

    case CORBA_tk_object:
    case CORBA_tk_record:
    case CORBA_tk_union:
    case CORBA_tk_enumeration:
    case CORBA_tk_alias:
    case CORBA_tk_except:
    case CORBA_tk_optional:
#ifdef ILU_REFERENCE_TYPES
    case CORBA_tk_reference:
#endif /* def ILU_REFERENCE_TYPES */
      {
	ilu_cardinal size_ptr;
	/* the parms are in an encapsulation, so we need to write the size, but
	   we don't know it yet.  We could either introduce another step here
	   explicitly to do the parms, or we can fake it...  We fake it. */
	_cdr_put_u32 (pk, 0, err);
	if (ILU_ERRNOK(*err)) break;
	size_ptr = (ilu_cardinal) packet_vop(pk) - 4;

	/* all the parms are in an encapsulation, so the first thing we have to do
	   is indicate the byte order of the encapsulation...  And we write something
	   that's easy to see in the ILU packet dump.  */
	if (NATIVE_BYTE_ORDER == LittleEndian)
	  _cdr_put_u32 (pk, 0x444E4501, err);
	else
	  _cdr_put_u32 (pk, 0x00454E44, err);
	/* _cdr_put_u8 (pk, (NATIVE_BYTE_ORDER == LittleEndian), err); */
	if (ILU_ERRNOK(*err)) break;

	/* Note that ILU ISL type names have interface names and interface brands, as well
	   as the simple IDL name.  We encode ILU names into CORBA names by the following:
	   "_ilu_<interface-name>_<interface-brand>_<type-name>".  Names of non-ILU types
	   are just "<type-name>".  form_type_name() does the dirty work.  Sequences and
	   array types don't need names... */
	if ((ctk != CORBA_tk_array) &&
	    (ctk != CORBA_tk_sequence)) {
	  typename = form_type_name(t, typenamebuf, sizeof(typenamebuf), err);
	  if (ILU_ERRNOK(*err)) return;
	};

	switch (ctk)
	  {
	  case CORBA_tk_alias:
	    if ((_cdr_put_bytes (pk, (ilu_bytes) type_uid(t), (strlen(type_uid(t)) + 1), err), ILU_ERRNOK(*err)) ||
		(_cdr_put_bytes (pk, (ilu_bytes) typename, (strlen(typename) + 1), err), ILU_ERRNOK(*err)))
	      return;
	    _IIOP_AddType (pk, type_desc(t).Alias.type, stack, marshalled_alias, err);
	    if (ILU_ERRNOK(*err)) return;
	    break;

#ifdef ILU_REFERENCE_TYPES
	  case CORBA_tk_reference:
	    if ((_cdr_put_bytes (pk, (ilu_bytes) type_uid(t), (strlen(type_uid(t)) + 1), err), ILU_ERRNOK(*err)) ||
		(_cdr_put_bytes (pk, (ilu_bytes) typename, (strlen(typename) + 1), err), ILU_ERRNOK(*err)))
	      return;
	    _cdr_put_u8(pk, type_desc(t).Reference.optionalp, err);
	    if (ILU_ERRNOK(*err)) return;
	    _cdr_put_u8(pk, type_desc(t).Reference.aliasedp, err);
	    if (ILU_ERRNOK(*err)) return;
	    _IIOP_AddType (pk, type_desc(t).Reference.type, stack, ilu_FALSE, err);
	    if (ILU_ERRNOK(*err)) return;
	    break;
#endif /* def ILU_REFERENCE_TYPES */

	  case CORBA_tk_optional:
	    if ((_cdr_put_bytes (pk, (ilu_bytes) type_uid(t), (strlen(type_uid(t)) + 1), err), ILU_ERRNOK(*err)) ||
		(_cdr_put_bytes (pk, (ilu_bytes) typename, (strlen(typename) + 1), err), ILU_ERRNOK(*err)))
	      return;
	    _IIOP_AddType (pk, type_desc(t).Optional.type, stack, ilu_FALSE, err);
	    if (ILU_ERRNOK(*err)) return;
	    break;

	  case CORBA_tk_object:
	    if ((_cdr_put_bytes (pk, (ilu_bytes) type_uid(t), (strlen(type_uid(t)) + 1), err), ILU_ERRNOK(*err)) ||
		(_cdr_put_bytes (pk, (ilu_bytes) typename, (strlen(typename) + 1), err), ILU_ERRNOK(*err)))
	      return;
	    break;

	  case CORBA_tk_record:
	    {
	      ilu_cardinal i;
	      ilu_RecordField_s *field;

	      if ((_cdr_put_bytes(pk, (ilu_bytes) type_uid(t), (strlen(type_uid(t)) + 1), err), ILU_ERRNOK(*err)) ||
		  (_cdr_put_bytes(pk, (ilu_bytes) typename, (strlen(typename) + 1), err), ILU_ERRNOK(*err)) ||
		  (_cdr_put_u32(pk, type_desc(t).Record.n_fields, err), ILU_ERRNOK(*err)))
		return;
	      for (i = 0;  i < type_desc(t).Record.n_fields;  i++) {
		{
		  field = &type_desc(t).Record.fields[i];
		  if ((_cdr_put_bytes(pk, (ilu_bytes) field->base.name, (strlen(field->base.name) + 1), err),
		       ILU_ERRNOK(*err))) return;
		  _IIOP_AddType (pk, field->base.type, stack, ilu_FALSE, err);
		  if (ILU_ERRNOK(*err)) return;
		}
	      }
	    }
	    break;

	  case CORBA_tk_enumeration:
	    {
	      ilu_EnumElement_s *element;
	      ilu_cardinal i;

	      if ((_cdr_put_bytes(pk, (ilu_bytes) type_uid(t), (strlen(type_uid(t)) + 1), err), ILU_ERRNOK(*err)) ||
		  (_cdr_put_bytes(pk, (ilu_bytes) typename, (strlen(typename) + 1), err), ILU_ERRNOK(*err)))
		return;

	      /* ILU enumerated types allow explicit assignment of the integer values of the
		 various enumeration values, so we have two forms, one for ILU-ish enumerations,
		 the other for CORBA-ish enumerations */

	      if (is_non_CORBA_ILU_enum(t))
		{
		  if ((_cdr_put_u32(pk, 0, err), ILU_ERRNOK(*err)) ||
		      (_cdr_put_u32(pk, type_desc(t).Enumeration.n_elements, err), ILU_ERRNOK(*err)))
		    return;
		  for (i = 0;  i < type_desc(t).Enumeration.n_elements;  i++)
		    {
		      element = &type_desc(t).Enumeration.elements[i];
		      if ((_cdr_put_bytes(pk, (ilu_bytes) element->name, strlen(element->name) + 1, err), ILU_ERRNOK(*err)) ||
			  (_cdr_put_u32(pk, element->value, err), ILU_ERRNOK(*err)))
			return;
		    }
		}
	      else
		{
		  if (_cdr_put_u32(pk, type_desc(t).Enumeration.n_elements, err), ILU_ERRNOK(*err))
		    return;
		  for (i = 0;  i < type_desc(t).Enumeration.n_elements;  i++)
		    {
		      element = &type_desc(t).Enumeration.elements[i];
		      if (element->value != i) {
			ILU_NOTE(IIOP_DEBUG,
			      ("_IIOP_FormTypeEncapsulation:  "
			       "attempt to marshal enumeration type %s.%s with non-CORBA semantics!\n",
			       type_interface_name(t), type_name(t)));
			ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, err);
			return;
		      }
		      if (_cdr_put_bytes(pk, (ilu_bytes) element->name,
					 strlen(element->name) + 1, err), ILU_ERRNOK(*err))
			return;
		    }
		}
	    }
	    break;

	  case CORBA_tk_sequence:
	    _IIOP_AddType (pk, type_desc(t).Sequence.type, stack, ilu_FALSE, err);
	    if (ILU_ERRNOK(*err)) return;
	    _cdr_put_u32(pk, type_desc(t).Sequence.limit, err);
	    if (ILU_ERRNOK(*err)) return;
	    break;

	  case CORBA_tk_array:
	    /* multi-dimensional arrays are marshalled as arrays of arrays, so things get
	       complicated here... */
	    if (type_desc(t).Array.n_dims > 1)
	      {
		struct _ilu_Type_s temptype;
		struct _ilu_TypeName_s tempname;
		temptype.names = &tempname;
		temptype.n_names = 1;
		temptype.names[0].name = NIL;
		temptype.names[0].interface_name.name = NIL;
		temptype.names[0].interface_name.brand = NIL;
		temptype.uid = NIL;
		temptype.kind = ilu_array_tk;
		temptype.desc.Array.type = type_desc(t).Array.type;
		temptype.desc.Array.n_dims = type_desc(t).Array.n_dims - 1;
		temptype.desc.Array.dims = &type_desc(t).Array.dims[1];
		_IIOP_AddType (pk, &temptype, stack, ilu_FALSE, err);
		if (ILU_ERRNOK(*err)) return;
	      }
	    else
	      {
		_IIOP_AddType (pk, type_desc(t).Array.type, stack, ilu_FALSE, err);
	      }
	    _cdr_put_u32(pk, type_desc(t).Array.dims[0], err);
	    if (ILU_ERRNOK(*err)) return;
	    break;

	  case CORBA_tk_union:
	    {
	      ilu_cardinal i, j;
	      ilu_integer default_arm;
	      ilu_integer n_arms;
	      ilu_UnionArm_s *arm;

	      default_arm = type_desc(t).Union.default_arm - 1;

	      /* ILU has unions which don't allow invalid discriminant values, so set the
		 high bit of the n_arms field to indicate this.  Note that this means that
		 normal ILU unions, which by default are `clean', don't interoperate with
		 CORBA unions, which are always `dirty' this way. */
	      /* Also, the number of arms sent across the wire isn't really the number of
		 arms, it's the number of discriminant values explicitly specified in the
		 IDL spec for the union.  Idiocy! */

	      for (i = 0, n_arms = 0;  i < type_desc(t).Union.n_arms;  i++) {
		n_arms += type_desc(t).Union.arms[i].n_values;
	      }
	      n_arms |= ((type_desc(t).Union.others_allowed) ? 0 : 0x80000000);

	      if ((_cdr_put_bytes (pk, (ilu_bytes) type_uid(t), (strlen(type_uid(t)) + 1), err), ILU_ERRNOK(*err)) ||
		  (_cdr_put_bytes (pk, (ilu_bytes) typename, (strlen(typename) + 1), err), ILU_ERRNOK(*err)))
		return;
	      _IIOP_AddType (pk, type_desc(t).Union.discriminant, stack, ilu_FALSE, err);
	      if (ILU_ERRNOK(*err)) return;
	      if ((_cdr_put_s32 (pk, default_arm, err), ILU_ERRNOK(*err)) ||
		  (_cdr_put_u32 (pk, n_arms, err), ILU_ERRNOK(*err)))
		return;
	      for (i = 0;  i < type_desc(t).Union.n_arms;  i++) {
		arm = &type_desc(t).Union.arms[i];
		for (j = 0;  j < arm->n_values;  j++) {
		  switch (type_kind(ur_type(type_desc(t).Union.discriminant))) {
		  case ilu_boolean_tk:
		    _cdr_put_u8(pk, (ilu_byte)(arm->values[j].value.boolean_val), err);
		    break;
		  case ilu_byte_tk:
		  case ilu_shortcharacter_tk:
		    _cdr_put_u8(pk, arm->values[j].value.byte_val, err);
		    break;
		  case ilu_integer_tk:
		    _cdr_put_s32(pk, arm->values[j].value.integer_val, err);
		    break;
		  case ilu_shortinteger_tk:
		    _cdr_put_s16(pk, arm->values[j].value.shortinteger_val, err);
		    break;
		  case ilu_cardinal_tk:
		    _cdr_put_u32(pk, arm->values[j].value.cardinal_val, err);
		    break;
		  case ilu_shortcardinal_tk:
		    _cdr_put_u16(pk, arm->values[j].value.shortcardinal_val, err);
		    break;
		  case ilu_enumeration_tk:
		    _cdr_put_u32(pk, enum_code(arm->values[j].value.enumeration_val,
					       type_desc(t).Union.discriminant), err);
		    break;
		  default:
		    ILU_ERR_CONS1(internal, err, minor, ilu_im_badTypeKind, 0);
		    break;
		  };
		  if (ILU_ERRNOK(*err)) return;
		  if (arm->base.name == NIL)
		    _cdr_put_bytes(pk, (ilu_bytes) "", 1, err);
		  else
		    _cdr_put_bytes(pk, (ilu_bytes) arm->base.name, strlen(arm->base.name) + 1, err);
		  if (ILU_ERRNOK(*err)) return;
		  _IIOP_AddType (pk, arm->base.type, stack, ilu_FALSE, err);
		  if (ILU_ERRNOK(*err)) return;
		}
	      }
	    }
	    break;

	  default:
	    _ilu_Assert(0, "bad value for ilu_TypeKind encountered");
	    return;
	  }

	/* free typename if malloc'ed */
	if ((typename != NIL) && (typename != typenamebuf))
	  ilu_free(typename);

	/* now adjust the size of the parms encapsulation...  Ugly! */
	{
	  ilu_bytes where = pk->bs->tr_outBuff + size_ptr;
	  ilu_cardinal size = (ilu_cardinal) packet_vop(pk) - size_ptr - 4;
	  memcpy ((void *) where, (void *) &size, 4);
	}
      }
      break;

    default:
      ILU_ERR_CONS1(marshal, err, minor, ilu_mm_bad_typekind, 0);
      break;
    }
}

static HashTable CachedTypeEncapsulations = NIL;

static TypeEncapsulation
  _IIOP_GetTypeEncapsulation (ilu_Type t,			/* the variant type */
			      ilu_boolean anonymous_pass,	/* for sequence and array types */
			      ILU_ERRS((IoErrs)) * err)
{
  TypeStack s = NIL;
  PACKET pk = NIL;
  ilu_cardinal ctk;
  TypeEncapsulation e = NIL;

  if (CachedTypeEncapsulations != NIL)
    {
      if (type_uid(t) != NIL)
	e = (TypeEncapsulation) ilu_hash_FindInTable (CachedTypeEncapsulations, type_uid(t));
    }
  else
    {
      if ((CachedTypeEncapsulations = ilu_hash_MakeNewTable (CLASS_HASHTABLESIZE,
							      ilu_hash_HashString,
							      ilu_hash_StringCompare)) == NIL) {
	ilu_DebugPrintf("_IIOP_GetTypeEncapsulation:  "
			"Can't create hash table of encapsulated Types!\n");
	return ILU_ERR_CONS1(internal, err, minor, ilu_im_callFail, NIL);
      }
    }
  if (e == NIL) {	/* no match, make one */
    pk = _cdr_InmemPacket (0, NIL, NATIVE_BYTE_ORDER, 0, err);
    if (ILU_ERRNOK(*err)) return NIL;
    s = add_type (s, t, 0, err);
    if (ILU_ERRNOK(*err)) return NIL;
    _IIOP_FormTypeEncapsulation (pk, &ctk, t, &s, anonymous_pass, err);
    free_list(s);
    if (ILU_ERRNOK(*err)) {
      _cdr_InmemFree (pk, NIL, NIL);
    } else {
      e = ilu_MallocE(sizeof(*e), err);
      if (ILU_ERRNOK(*err)) {
	e = NIL;
	_cdr_InmemFree(pk, NIL, NIL);
      } else {
	e->ctk = ctk;
	_cdr_InmemFree(pk, &e->size, &e->bytes);
	if (type_kind(t) != ilu_sequence_tk &&
	    type_kind(t) != ilu_array_tk &&
	    type_uid(t) != NIL) {
	  ilu_hash_AddToTable(CachedTypeEncapsulations,
			      type_uid(t), e);
	  e->permanent = ilu_TRUE;
	} else {
	  e->permanent = ilu_FALSE;
	}
      };
    }
  }
  return e;
}

static void
  _IIOP_AddType (PACKET pk, ilu_Type type, TypeStack *stack, ilu_boolean anonymous_pass, ilu_Error *err)
{
  ilu_integer index;

  index = has_type (*stack, type);
  if ((index >= 0) && ! anonymous_pass) {	/* type already marshalled */
    _cdr_put_u32 (pk, 0xFFFFFFFF, err);
    if (ILU_ERRNOK(*err)) return;
    _cdr_put_u32 (pk, (ilu_cardinal) (index - 4 - (ilu_cardinal) packet_vop(pk)), err);
  } else {		/* new type for this marshalling */
    /* Add an encapsulated type to the current marshalling stream */
    TypeEncapsulation e = _IIOP_GetTypeEncapsulation(type, anonymous_pass, err);
    if (ILU_ERRNOK(*err)) goto free_e;
    _cdr_put_u32 (pk, e->ctk, err);
    if (ILU_ERRNOK(*err)) goto free_e;
    *stack = add_type (*stack, type, (ilu_cardinal) (packet_vop(pk) - 4), err);
    if (ILU_ERRNOK(*err)) goto free_e;
    _cdr_put_opaque (pk, e->bytes, e->size, err);
  free_e:
    FreeTypeEncapsulation(e);
  }
}

static ilu_cardinal
  _IIOP_SizeOfType (ilu_Call call,
		    ilu_Type type,
		    ilu_Error *err)
{
  TypeEncapsulation e;
  ilu_cardinal size = 0;

  e = _IIOP_GetTypeEncapsulation (type, ilu_FALSE, err);
  if (ILU_ERRNOK(*err)) goto free_e;
  size += _IIOP_SizeOfCardinal (call, e->ctk, err);
  if (ILU_ERRNOK(*err)) goto free_e;
  size += _IIOP_SizeOfOpaque (call, e->bytes, e->size, err);
 free_e:
  FreeTypeEncapsulation(e);
  return (ILU_ERROK(*err) ? size : 0);
}

static void
  _IIOP_OutputType (ilu_Call call,
		    ilu_Type type,
		    ilu_Error *err)
{
  TypeEncapsulation e;

  e = _IIOP_GetTypeEncapsulation (type, ilu_FALSE, err);
  if (ILU_ERRNOK(*err)) goto free_e;
  _IIOP_OutputCardinal (call, e->ctk, err);
  if (ILU_ERRNOK(*err)) goto free_e;
  _IIOP_OutputOpaque (call, e->bytes, e->size, err);
 free_e:
  FreeTypeEncapsulation(e);
  return;
}

static char * CORBA_type_kind_name (ilu_integer ctk)
{
  switch (ctk)
    {
    case CORBA_tk_null: return ("null");
    case CORBA_tk_void: return ("void");
    case CORBA_tk_shortinteger: return ("shortinteger");
    case CORBA_tk_integer: return ("integer");
    case CORBA_tk_shortcardinal: return ("shortcardinal");
    case CORBA_tk_cardinal: return ("cardinal");
    case CORBA_tk_shortreal: return ("shortreal");
    case CORBA_tk_real: return ("real");
    case CORBA_tk_boolean: return ("boolean");
    case CORBA_tk_shortcharacter: return ("shortcharacter");
    case CORBA_tk_byte: return ("byte");
    case CORBA_tk_variant: return ("pickle");
    case CORBA_tk_type: return ("type");
    case CORBA_tk_Principal: return ("Principal");
    case CORBA_tk_object: return ("object");
    case CORBA_tk_record: return ("record");
    case CORBA_tk_union: return ("union");
    case CORBA_tk_enumeration: return ("enumeration");
    case CORBA_tk_string: return ("string");
    case CORBA_tk_sequence: return ("sequence");
    case CORBA_tk_array: return ("array");
    case CORBA_tk_alias: return ("alias");
    case CORBA_tk_except: return ("except");
    case CORBA_tk_longinteger: return ("longinteger");
    case CORBA_tk_longcardinal: return ("longcardinal");
    case CORBA_tk_longreal: return ("longreal");
    case CORBA_tk_character: return ("character");
    case CORBA_tk_optional: return ("optional");
#ifdef ILU_REFERENCE_TYPES
    case CORBA_tk_reference: return ("reference");
#endif /* def ILU_REFERENCE_TYPES */
    default:
      return ("<unknown>");
    }
}

static ilu_Type FindSimpleType (ilu_integer corba_typekind, ilu_Error *err)
{
  switch (corba_typekind)
    {
    case CORBA_tk_shortinteger:
      return ilu_FindTypeByName("ilu.shortinteger", err);
    case CORBA_tk_integer:
      return ilu_FindTypeByName("ilu.integer", err);
    case CORBA_tk_shortcardinal:
      return ilu_FindTypeByName("ilu.shortcardinal", err);
    case CORBA_tk_cardinal:
      return ilu_FindTypeByName("ilu.cardinal", err);
    case CORBA_tk_shortreal:
      return ilu_FindTypeByName("ilu.shortreal", err);
    case CORBA_tk_real:
      return ilu_FindTypeByName("ilu.real", err);
    case CORBA_tk_boolean:
      return ilu_FindTypeByName("ilu.boolean", err);
    case CORBA_tk_shortcharacter:
      return ilu_FindTypeByName("ilu.shortcharacter", err);
    case CORBA_tk_byte:
      return ilu_FindTypeByName("ilu.byte", err);
    case CORBA_tk_longinteger:
      return ilu_FindTypeByName("ilu.longinteger", err);
    case CORBA_tk_longcardinal:
      return ilu_FindTypeByName("ilu.longcardinal", err);
    case CORBA_tk_longreal:
      return ilu_FindTypeByName("ilu.longreal", err);
    case CORBA_tk_character:
      return ilu_FindTypeByName("ilu.character", err);
    case CORBA_tk_variant:
      return ilu_FindTypeByName("ilu.pickle", err);
    case CORBA_tk_string:
      return ilu_FindTypeByName("ilu.CString", err);

    default:
      ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_invalid_typekind, 0);
      return ILU_NIL;
    }
}

static ilu_boolean
  decode_type_name (ilu_string encoded_typename,
		    ilu_string *real_typename,
		    ilu_string *interfacename,
		    ilu_string *interfacebrand,
		    ilu_Error *err)
{
  ilu_string ptr1, ptr2;

  if ((strlen(encoded_typename) > 5) &&
      (strncmp(encoded_typename, "_ilu_", 5) == 0))
    {
      /* ILU type */

      /* get interface name */
      ptr1 = encoded_typename + 5;
      ptr2 = strstr(ptr1, "___");
      if (ptr2 == NIL)
	return ILU_ERR_CONS1(marshal, err, minor, ilu_mm_badTypeName, ilu_FALSE);
      else *ptr2 = 0;
      *interfacename = ilu_StrdupE(ptr1, err);
      if (ILU_ERRNOK(*err)) return ilu_FALSE;

      /* get interface brand */
      ptr1 = ptr2 + 3;
      ptr2 = strstr(ptr1, "___");
      if (ptr2 == NIL)
	return ILU_ERR_CONS1(marshal, err, minor, ilu_mm_badTypeName, ilu_FALSE);
      else *ptr2 = 0;
      if (ptr2 == ptr1)
	*interfacebrand = NIL;
      else
	*interfacebrand = ilu_StrdupE(ptr1, err);
      if (ILU_ERRNOK(*err)) return ilu_FALSE;

      /* finally, get the type name */
      ptr1 = ptr2 + 3;
      *real_typename = ilu_StrdupE(ptr1, err);
      return (ILU_ERROK(*err));
    }
  else
    {
      *interfacename = ilu_StrdupE((const ilu_string) ForeignTypeInterface, err);
      if (ILU_ERRNOK(*err)) return ilu_FALSE;
      *interfacebrand = NIL;
      *real_typename = ilu_StrdupE(encoded_typename, err);
      return ILU_ERROK(*err);
    }
}

static ilu_boolean
  GetHeader (PACKET pk, ilu_Type t, ilu_Error *err)
{
  /* Read in the UID and type names */

  ilu_string	typename = NIL;
  ilu_cardinal	len;

  type_uid(t) = NIL;
  _cdr_get_bytes(pk, (ilu_bytes *) &type_uid(t), &len, 0xFFFF, err);
  if (ILU_ERRNOK(*err)) goto endit;
  _cdr_get_bytes(pk, (ilu_bytes *) &typename, &len, 0xFFFF, err);
  if (ILU_ERRNOK(*err)) goto endit;
  decode_type_name (typename,
		    &type_name(t),
		    &type_interface_name(t),
		    &type_interface_brand(t),
		    err);
  ilu_free(typename);
 endit:
  return (ILU_ERROK(*err));
}

static ilu_boolean
  FakeHeader (ilu_Type t, ilu_Error *err)
{
  if (type_names_count(t) < 1) {
    type_names(t) = (ilu_TypeName_s *) ilu_MallocE(sizeof(ilu_TypeName_s), err);
    if (ILU_ERRNOK(*err)) return ilu_FALSE;
    type_names_count(t) = 1;
  };
  type_interface_brand(t) = NIL;
  type_uid(t) = ilu_InventID();
  type_name(t) = ilu_StrdupE(AnonymousTypeName, err);
  if (ILU_ERRNOK(*err)) return ilu_FALSE;
  type_interface_name(t) = ilu_StrdupE((const ilu_string) ForeignTypeInterface, err);
  return (ILU_ERROK(*err));
}

static ilu_Type
  CanonicalizeArrayType (ilu_Type t, ilu_Type basetype, ilu_cardinal size, ilu_Error *err)
{
  /* Multi-dimensional arrays are represented across the wire */
  /* as arrays of arrays of ...  This code packs them into a single */
  /* ILU array type */

  ilu_cardinal i;

  if (type_kind(basetype) == ilu_array_tk && (strcmp(AnonymousTypeName, type_name(basetype)) == 0)) {
    type_desc(t).Array.type = type_desc(basetype).Array.type;
    type_desc(t).Array.n_dims = type_desc(basetype).Array.n_dims + 1;
    type_desc(t).Array.dims = (ilu_cardinal *) ilu_MallocE(sizeof(ilu_cardinal) * type_desc(t).Array.n_dims, err);
    if (ILU_ERRNOK(*err)) return NIL;
    for (i = 0;  i < type_desc(basetype).Array.n_dims;  i++) {
      type_desc(t).Array.dims[i] = type_desc(basetype).Array.dims[i];
    }
    type_desc(t).Array.dims[type_desc(basetype).Array.n_dims] = size;
    ilu_free(type_name(basetype));
    ilu_free(type_interface_name(basetype));
    ilu_free(type_interface_brand(basetype));
    ilu_free(type_uid(basetype));
    ilu_free(type_desc(basetype).Array.dims);
    ilu_free(basetype);
  } else {
    type_desc(t).Array.type = basetype;
    type_desc(t).Array.n_dims = 1;
    type_desc(t).Array.dims = (ilu_cardinal *) ilu_MallocE(sizeof(ilu_cardinal), err);
    if (ILU_ERRNOK(*err)) return NIL;
    type_desc(t).Array.dims[0] = size;
  }
  return t;
}

static void
  CanonicalizeUnionArms (ilu_Type t, ilu_UnionArm_s *arms, ilu_cardinal n_arms, ilu_Error *err)
{
  /* CORBA union arm descriptions are passed in the IIOP redundantly; that is,
     for arms which have multiple selecting descriminant values, there is a full
     arm representation passed for each value.  This procedure collapses this
     inflated set of arms to a non-redundant set, frees the original redundant
     set, and completes the union description. */

  ilu_UnionArm_s *rarms = NIL;
  ilu_cardinal arm_count = 0;
  ilu_cardinal i, j;

  for (i = 0;  i < n_arms;  i++) {
    for (j = 0;  j < arm_count;  j++) {
      if ((strcmp(rarms[j].base.name, arms[i].base.name) == 0) &&
	  (rarms[j].base.type == arms[i].base.type)) {
	rarms[j].values = (ilu_ConstantValue_s *) ilu_ReallocE(rarms[j].values, sizeof(ilu_ConstantValue_s) * (rarms[j].n_values + 1), err);
	if (ILU_ERRNOK(*err)) return;
	rarms[j].values[rarms[j].n_values] = arms[i].values[0];
	rarms[j].n_values += 1;
	ilu_free(arms[i].base.name);
	ilu_free(arms[i].values);
	break;
      }
    }
    if (j == arm_count) {	/* arms[i] not in rarms */
      rarms = (arm_count
	       ? ilu_ReallocE(rarms,
			  sizeof(ilu_UnionArm_s) * (arm_count + 1),
			      err)
	       : ilu_MallocE(sizeof(ilu_UnionArm_s), err));
      if (ILU_ERRNOK(*err)) return;
      rarms[arm_count] = arms[i];
      arm_count += 1;
    }
  }
  ilu_free(arms);
  type_desc(t).Union.arms = rarms;
  type_desc(t).Union.n_arms = arm_count;
  ILU_CLER(*err);
}

static ilu_string
  FigureEnumVal (ilu_shortcardinal val, ilu_Type type, ilu_Error *err)
{
  ilu_cardinal i;

  for (i = 0;  i < type_desc(type).Enumeration.n_elements;  i++) {
    if (val == type_desc(type).Enumeration.elements[i].value) {
      return ilu_StrdupE(type_desc(type).Enumeration.elements[i].name, err);
    }
  };
  ILU_ERR_CONS1(marshal, err, minor, ilu_mm_bad_union_disc, 0);
  return NIL;
}

static ilu_Type _IIOP_InputType2 (PACKET, TypeStack *, ilu_Error *);

static ilu_Type
  DecodeTypeParms2 (ilu_Type t,		/* the actual type structure */
		    ilu_cardinal ctk,	/* the kind of type */
		    PACKET pk,		/* marshalling context */
		    TypeStack *stack,
		    ilu_Error *err)
{
  ilu_cardinal len;

  switch (ctk)
    {
    case CORBA_tk_object:
      {
	ilu_string typeuid = NIL;
	ilu_string typename = NIL;
	ilu_cardinal len;
	ilu_Type t2 = NIL;

	type_kind(t) = ilu_object_tk;
	/* read type info */
	_cdr_get_bytes(pk, (ilu_bytes *) &typeuid, &len, 0xFFFF, err);
	if (ILU_ERRNOK(*err)) break;
	_cdr_get_bytes(pk, (ilu_bytes *) &typename, &len, 0xFFFF, err);
	if (ILU_ERRNOK(*err)) break;
	ilu_free(typename);
	/* if we don't know the actual object type, we truncate it down to
	   ilu.CORBA-Object.  This will break later on if the object is not
	   a descendant of CORBA-Object. */
	if (((t2 = ilu_FindTypeByUID(typeuid, err)) == ILU_NIL) && ILU_ERROK(*err))
	  t2 = ilu_FindTypeByUID((char *) ilu_TypeID_ilu_CORBA_Object, err);
	ilu_free(typeuid);
	if (ILU_ERRNOK(*err))
	  ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_typeID, 0);
	else {
	  type_uid(t) = _ilu_Strdup(type_uid(t2));
	  type_name(t) = _ilu_Strdup(type_name(t2));
	  type_interface_brand(t) = _ilu_Strdup(type_interface_brand(t2));
	  type_interface_name(t) = _ilu_Strdup(type_interface_name(t2));
	}
	break;
      }

    case CORBA_tk_record:
      {
	ilu_cardinal i;
	ilu_cardinal plen;

	type_kind(t) = ilu_record_tk;
	if (!GetHeader(pk, t, err)) break;
	_cdr_get_u32(pk, &plen, err);
	if (ILU_ERRNOK(*err)) break;
	type_desc(t).Record.n_fields = plen;
	type_desc(t).Record.fields =
	  (ilu_RecordField_s *) ilu_MallocE(sizeof(ilu_RecordField_s) * type_desc(t).Record.n_fields, err);
	if (ILU_ERRNOK(*err)) break;
	memset((void *) type_desc(t).Record.fields, 0, sizeof(ilu_RecordField_s) * type_desc(t).Record.n_fields);
	for (i = 0;  i < type_desc(t).Record.n_fields;  i++) {
	  type_desc(t).Record.fields[i].base.name = NIL;
	  _cdr_get_bytes(pk, (ilu_bytes *) &type_desc(t).Record.fields[i].base.name, &len, 0xFFFF, err);
	  if (ILU_ERRNOK(*err)) break;
	  type_desc(t).Record.fields[i].base.type = _IIOP_InputType2(pk, stack, err);
	  if (ILU_ERRNOK(*err)) break;
	}
      }
      break;

    case CORBA_tk_sequence:
      type_kind(t) = ilu_sequence_tk;
      if (!FakeHeader(t, err)) break;
      type_desc(t).Sequence.type = _IIOP_InputType2(pk, stack, err);
      if (ILU_ERRNOK(*err)) break;
      _cdr_get_u32 (pk, &type_desc(t).Sequence.limit, err);
      break;

    case CORBA_tk_alias:
      {
	ilu_Type basetype;

	type_kind(t) = ilu_alias_tk;
	if (!GetHeader(pk, t, err)) break;
	basetype = _IIOP_InputType2(pk, stack, err);
	if ((type_uid(basetype) == NIL) ||
	    (strcmp(type_name(basetype), AnonymousTypeName) == 0)) {
	  ilu_free(type_name(basetype));
	  type_name(basetype) = type_name(t);
	  ilu_free(type_interface_name(basetype));
	  type_interface_name(basetype) = type_interface_name(t);
	  ilu_free(type_interface_brand(basetype));
	  type_interface_brand(basetype) = type_interface_brand(t);
	  ilu_free(type_uid(basetype));
	  type_uid(basetype) = type_uid(t);
	  ilu_free(type_names(t));
	  ilu_free(t);
	  t = basetype;
	} else {
	  type_desc(t).Alias.type = basetype;
	};
      }
      break;

    case CORBA_tk_optional:
      type_kind(t) = ilu_optional_tk;
      if (!GetHeader(pk, t, err)) break;
      type_desc(t).Optional.type = _IIOP_InputType2(pk, stack, err);
      break;

#ifdef ILU_REFERENCE_TYPES
    case CORBA_tk_reference:
      {
	ilu_byte tmp;
	type_kind(t) = ilu_reference_tk;
	if (!GetHeader(pk, t, err)) break;
	_cdr_get_u8 (pk, &tmp, err);
	if (ILU_ERRNOK(*err)) break;
	type_desc(t).Reference.optionalp = ((tmp == 0) ? ilu_FALSE : ilu_TRUE);
	_cdr_get_u8 (pk, &tmp, err);
	if (ILU_ERRNOK(*err)) break;
	type_desc(t).Reference.aliasedp = ((tmp == 0) ? ilu_FALSE : ilu_TRUE);
	type_desc(t).Reference.type = _IIOP_InputType2(pk, stack, err);
	if (ILU_ERRNOK(*err)) break;
      }
      break;
#endif /* def ILU_REFERENCE_TYPES */

    case CORBA_tk_enumeration:
      {
	ilu_cardinal count, i, junk;

	type_kind(t) = ilu_enumeration_tk;
	if (!GetHeader(pk, t, err)) break;
	_cdr_get_u32(pk, &count, err);
	if (ILU_ERRNOK(*err)) break;
	if (count == 0) {	/* ILU enum */
	  _cdr_get_u32(pk, &count, err);
	  if (ILU_ERRNOK(*err)) break;
	  type_desc(t).Enumeration.n_elements = count;
	  type_desc(t).Enumeration.elements = ilu_MallocE(sizeof(ilu_EnumElement_s) * count, err);
	  if (ILU_ERRNOK(*err)) break;
	  for (i = 0;  i < count;  i++) {
	    type_desc(t).Enumeration.elements[i].name = NIL;
	    _cdr_get_bytes(pk, (ilu_bytes *) &type_desc(t).Enumeration.elements[i].name, &junk, 0xFFFF, err);
	    if (ILU_ERRNOK(*err)) break;
	    _cdr_get_u32(pk, &type_desc(t).Enumeration.elements[i].value, err);
	    if (ILU_ERRNOK(*err)) break;
	  }
	} else {		/* CORBA enum */
	  type_desc(t).Enumeration.n_elements = count;
	  type_desc(t).Enumeration.elements = ilu_MallocE(sizeof(ilu_EnumElement_s) * count, err);
	  if (ILU_ERRNOK(*err)) break;
	  for (i = 0;  i < count;  i++) {
	    type_desc(t).Enumeration.elements[i].name = NIL;
	    _cdr_get_bytes(pk, (ilu_bytes *) &type_desc(t).Enumeration.elements[i].name, &junk, 0xFFFF, err);
	    if (ILU_ERRNOK(*err)) break;
	    type_desc(t).Enumeration.elements[i].value = i;
	  };
	}
      }
      break;

    case CORBA_tk_array:
      {
	ilu_cardinal size;
	ilu_Type basetype;

	type_kind(t) = ilu_array_tk;
	if (!FakeHeader(t, err)) break;
	basetype = _IIOP_InputType2(pk, stack, err);
	if (ILU_ERRNOK(*err)) break;
	_cdr_get_u32(pk, &size, err);
	if (ILU_ERRNOK(*err)) break;
	t = CanonicalizeArrayType(t, basetype, size, err);
      }
      break;

    case CORBA_tk_union:
      {
	ilu_integer default_arm;
	ilu_cardinal n_arms, junk, i;
	ilu_UnionArm_s *arms;
	ilu_UnionArm_s *arm;
	ilu_cardinal enumcode;

	type_kind(t) = ilu_union_tk;
	if (!GetHeader(pk, t, err)) break;
	type_desc(t).Union.discriminant = _IIOP_InputType2(pk, stack, err);
	if (ILU_ERRNOK(*err)) break;
	_cdr_get_s32(pk, &default_arm, err);
	if (ILU_ERRNOK(*err)) break;
	_cdr_get_u32(pk, &n_arms, err);
	if (ILU_ERRNOK(*err)) break;
	type_desc(t).Union.others_allowed = ((n_arms & 0x80000000) == 0);
	n_arms = (n_arms & 0x7FFFFFFF);
	type_desc(t).Union.default_arm = default_arm + 1;
	arms = ilu_MallocE(sizeof(ilu_UnionArm_s) * n_arms, err);
	if (ILU_ERRNOK(*err)) break;
	for (i = 0;  i < n_arms;  i++) {
	  arm = &arms[i];
	  arm->n_values = 1;
	  arm->values = (ilu_ConstantValue_s *) ilu_MallocE(sizeof(ilu_ConstantValue_s), err);
	  if (ILU_ERRNOK(*err)) break;
	  switch (type_kind(type_desc(t).Union.discriminant))
	    {
	    case ilu_boolean_tk:
	      {
		ilu_byte foo;
		_cdr_get_u8(pk, &foo, err);
		arm->values[0].value.boolean_val = (foo != 0);
		arm->values[0].kind = ilu_boolean_cvk;
	      }
	      break;
	    case ilu_byte_tk:
	      _cdr_get_u8(pk, &arm->values[0].value.byte_val, err);
	      arm->values[0].kind = ilu_byte_cvk;
	      break;
	    case ilu_shortcharacter_tk:
	      _cdr_get_u8(pk, &arm->values[0].value.byte_val, err);
	      arm->values[0].kind = ilu_byte_cvk;
	      break;
	    case ilu_integer_tk:
	      _cdr_get_s32(pk, &arm->values[0].value.integer_val, err);
	      arm->values[0].kind = ilu_integer_cvk;
	      break;
	    case ilu_shortinteger_tk:
	      _cdr_get_s16(pk, &arm->values[0].value.shortinteger_val, err);
	      arm->values[0].kind = ilu_shortinteger_cvk;
	      break;
	    case ilu_cardinal_tk:
	      _cdr_get_u32(pk, &arm->values[0].value.cardinal_val, err);
	      arm->values[0].kind = ilu_cardinal_cvk;
	      break;
	    case ilu_shortcardinal_tk:
	      _cdr_get_u16(pk, &arm->values[0].value.shortcardinal_val, err);
	      arm->values[0].kind = ilu_shortcardinal_cvk;
	      break;
	    case ilu_enumeration_tk:
	      _cdr_get_u32(pk, &enumcode, err);
	      if (ILU_ERRNOK(*err)) break;
	      arm->values[0].value.enumeration_val = FigureEnumVal((ilu_shortcardinal) enumcode,
								   type_desc(t).Union.discriminant, err);
	      arm->values[0].kind = ilu_enumeration_cvk;
	      break;
	    default:
	      ILU_ERR_CONS1(internal, err, minor, ilu_im_badTypeKind, 0);
	      break;
	    }
	  if (ILU_ERRNOK(*err)) break;
	  arm->base.name = NIL;
	  _cdr_get_bytes(pk, (ilu_bytes *) &arm->base.name, &junk, 0xFFFF, err);
	  if (ILU_ERRNOK(*err)) break;
	  arm->base.type = _IIOP_InputType2(pk, stack, err);
	  if (ILU_ERRNOK(*err)) break;
	}
	/* now we've read in all the arms, which may be redundant (Geesh!).  So we
	   have to go through them and see how many distinct ones we have, and how
	   many discriminant values each distinct arm has.  Many, many times this
	   will only be one, but hey, that's the spec! */
	CanonicalizeUnionArms (t, arms, n_arms, err);
      }
      break;

    default:
      ILU_ERR_CONS1(internal, err, minor, ilu_im_badTypeKind, 0);
      break;
    }
  if (ILU_ERROK(*err)) return t;
  else return NIL;
}

static void
  _ilu_FreeTypeNames (ilu_TypeName_s *tn, int count)
{
  while (count-- > 0) {
    ilu_free(tn[count].interface_name.name);
    ilu_free(tn[count].interface_name.brand);
    ilu_free(tn[count].name);
  };
  ilu_free(tn);
}

static void
  _ilu_FreeType (ilu_Type t)
{
  ilu_cardinal tk = type_kind(t);

  _ilu_FreeTypeNames (type_names(t), type_names_count(t));
  ilu_free(type_uid(t));

  switch (tk)
    {
    case ilu_byte_tk:
    case ilu_boolean_tk:
    case ilu_character_tk:
    case ilu_shortcharacter_tk:
    case ilu_shortinteger_tk:
    case ilu_integer_tk:
    case ilu_longinteger_tk:
    case ilu_shortcardinal_tk:
    case ilu_cardinal_tk:
    case ilu_longcardinal_tk:
    case ilu_real_tk:
    case ilu_shortreal_tk:
    case ilu_longreal_tk:
    case ilu_pickle_tk:
    case ilu_object_tk:
    case ilu_optional_tk:
#ifdef ILU_REFERENCE_TYPES
    case ilu_reference_tk:
#endif /* def ILU_REFERENCE_TYPES */
    case ilu_alias_tk:
    case ilu_sequence_tk:
    case ilu_string_tk:
      break;
    case ilu_array_tk:
      ilu_free(type_desc(t).Array.dims);
      break;
    case ilu_record_tk:
      {
	ilu_cardinal i;
	for (i = 0;  i < type_desc(t).Record.n_fields;  i++) {
	  ilu_free(type_desc(t).Record.fields[i].base.name);
	}
	ilu_free(type_desc(t).Record.fields);
      }
      break;
    case ilu_enumeration_tk:
      {
	ilu_cardinal i;
	for (i = 0;  i < type_desc(t).Enumeration.n_elements;  i++) {
	  ilu_free(type_desc(t).Enumeration.elements[i].name);
	}
	ilu_free(type_desc(t).Enumeration.elements);
      }
      break;
    case ilu_union_tk:
      {
	ilu_cardinal i, j;
	for (i = 0;  i < type_desc(t).Union.n_arms;  i++) {
	  ilu_free(type_desc(t).Union.arms[i].base.name);
	  if (type_kind(type_desc(t).Union.discriminant) == ilu_enumeration_tk) {
	    for (j = 0;  j < type_desc(t).Union.arms[i].n_values;  j++) {
	      ilu_free (type_desc(t).Union.arms[i].values[j].value.enumeration_val);
	    }
	  }
	  ilu_free(type_desc(t).Union.arms[i].values);
	}
	ilu_free(type_desc(t).Union.arms);
      }
      break;
    default:
      return;
    }
  ilu_free(t);
}

static ilu_Type
  _IIOP_RegisterType (ilu_Type t,
		      ilu_Error *err)
{
  /* Check to see if the type "t" is known in this address space.  If so,
     free the storage pointed to by "t", and return the previous registration.
     If not, register it properly. */

  ilu_boolean newreg;
  ilu_cardinal tk = type_kind(t);
  ilu_Type tp;

  switch (tk) {
  case ilu_byte_tk:
  case ilu_boolean_tk:
  case ilu_character_tk:
  case ilu_shortcharacter_tk:
  case ilu_shortinteger_tk:
  case ilu_integer_tk:
  case ilu_longinteger_tk:
  case ilu_shortcardinal_tk:
  case ilu_cardinal_tk:
  case ilu_longcardinal_tk:
  case ilu_real_tk:
  case ilu_shortreal_tk:
  case ilu_longreal_tk:
  case ilu_pickle_tk:
    ILU_CLER(*err);
    return (t);

  case ilu_object_tk:
  case ilu_pipe_tk:
  case ilu_optional_tk:
#ifdef ILU_REFERENCE_TYPES
  case ilu_reference_tk:
#endif /* def ILU_REFERENCE_TYPES */
  case ilu_alias_tk:
  case ilu_union_tk:
  case ilu_sequence_tk:
  case ilu_record_tk:
  case ilu_array_tk:
  case ilu_enumeration_tk:
  case ilu_string_tk:
    tp = ilu_FindTypeByUID (type_uid(t), err);
    if (tp == t)
      return t;
    else if (tp != NIL) {
      _ilu_FreeType (t);
      return (tp);
    } else {	/* type not already known, register it */
      ilu_AcquireMutex(ilu_otmu);
      switch (tk) {
      case ilu_object_tk:
	tp = ilu_RegisterObjectType (type_name(t),
				     type_interface_name(t),
				     type_interface_brand(t),
				     type_uid(t),
				     type_desc(t).Object.objdesc,
				     &newreg, err);
	if (ILU_ERRNOK(*err)) goto exitpoint;
	break;
      case ilu_optional_tk:
	tp = ilu_RegisterOptionalType (type_name(t),
				       type_interface_name(t),
				       type_interface_brand(t),
				       type_uid(t),
				       type_uid(type_desc(t).Optional.type),
				       &newreg, err);
	if (ILU_ERRNOK(*err)) goto exitpoint;
	break;
#ifdef ILU_REFERENCE_TYPES
      case ilu_reference_tk:
	tp = ilu_RegisterReferenceType (type_name(t),
					type_interface_name(t),
					type_interface_brand(t),
					type_uid(t),
					type_uid(type_desc(t).Reference.type),
					type_desc(t).Reference.optionalp,
					type_desc(t).Reference.aliasedp,
					&newreg, err);
	if (ILU_ERRNOK(*err)) goto exitpoint;
	break;
#endif /* def ILU_REFERENCE_TYPES */
      case ilu_string_tk:
	tp = ilu_RegisterStringType (type_name(t),
				     type_interface_name(t),
				     type_interface_brand(t),
				     type_uid(t),
				     type_desc(t).String.language,
				     type_desc(t).String.limit,
				     type_desc(t).String.char_set,
				     &newreg, err);
	if (ILU_ERRNOK(*err)) goto exitpoint;
	break;
      case ilu_alias_tk:
	tp = ilu_RegisterAliasType (type_name(t),
				    type_interface_name(t),
				    type_interface_brand(t),
				    type_uid(t),
				    type_uid(type_desc(t).Alias.type),
				    &newreg, err);
	if (ILU_ERRNOK(*err)) goto exitpoint;
	break;
      case ilu_sequence_tk:
	tp = ilu_RegisterSequenceType (type_name(t),
				       type_interface_name(t),
				       type_interface_brand(t),
				       type_uid(t),
				       type_uid(type_desc(t).Sequence.type),
				       type_desc(t).Sequence.limit,
				       &newreg, err);
	if (ILU_ERRNOK(*err)) goto exitpoint;
	break;
      case ilu_enumeration_tk:
	{
	  ilu_cardinal i;
	  tp = ilu_RegisterEnumerationType (type_name(t),
					    type_interface_name(t),
					    type_interface_brand(t),
					    type_uid(t),
					    type_desc(t).Enumeration.n_elements,
					    &newreg, err);
	  if (ILU_ERRNOK(*err)) goto exitpoint;
	  for (i = 0;  i < type_desc(t).Enumeration.n_elements;  i++) {
	    if (!ilu_RegisterEnumerationElement (tp, i,
						 type_desc(t).Enumeration.elements[i].name,
						 type_desc(t).Enumeration.elements[i].value,
						 err))
	      goto exitpoint;
	  }
	}
	break;
      case ilu_record_tk:
	{
	  ilu_cardinal i;
	  tp = ilu_RegisterRecordType (type_name(t),
				       type_interface_name(t),
				       type_interface_brand(t),
				       type_uid(t),
				       type_desc(t).Record.n_fields,
				       ilu_FALSE, NIL,
				       &newreg, err);
	  if (ILU_ERRNOK(*err)) goto exitpoint;
	  for (i = 0;  i < type_desc(t).Record.n_fields;  i++) {
	    if (!ilu_RegisterRecordField (tp, i,
					  type_desc(t).Record.fields[i].base.name,
					  type_uid(type_desc(t).Record.fields[i].base.type),
					  err))
	      goto exitpoint;
	  }
	}
	break;
      case ilu_array_tk:
	tp = ilu_RegisterArrayType (type_name(t),
				    type_interface_name(t),
				    type_interface_brand(t),
				    type_uid(t),
				    type_uid(type_desc(t).Array.type),
				    type_desc(t).Array.n_dims,
				    type_desc(t).Array.dims,
				    &newreg, err);
	if (ILU_ERRNOK(*err)) goto exitpoint;
	break;
      case ilu_union_tk:
	{
	  ilu_cardinal i, j;
	  ilu_UnionArm arm;
	  tp = ilu_RegisterUnionType (type_name(t),
				      type_interface_name(t),
				      type_interface_brand(t),
				      type_uid(t),
				      type_uid(type_desc(t).Union.discriminant),
				      type_desc(t).Union.n_arms,
				      type_desc(t).Union.default_arm,
				      type_desc(t).Union.others_allowed,
				      &newreg, err);
	  if (ILU_ERRNOK(*err)) goto exitpoint;
	  for (i = 0;  i < type_desc(t).Union.n_arms;  i++) {
	    arm = ilu_RegisterUnionArm (tp, i,
					type_desc(t).Union.arms[i].base.name,
					type_uid(type_desc(t).Union.arms[i].base.type),
					type_desc(t).Union.arms[i].n_values,
					err);
	    if (ILU_ERRNOK(*err)) goto exitpoint;
	    for (j = 0;  j < type_desc(t).Union.arms[i].n_values;  j++) {
	      ilu_RegisterUnionArmValue (arm, j,
					 &type_desc(t).Union.arms[i].values[j],
					 err);
	      if (ILU_ERRNOK(*err)) goto exitpoint;
	    }
	  }
	}
	break;
      default:
	ILU_ERR_CONS1(internal, err, minor, ilu_im_badTypeKind, 0);
	goto exitpoint;
      }
      ILU_CLER(*err);
      _ilu_FreeType (t);
    exitpoint:
      ilu_ReleaseMutex(ilu_otmu);
      return tp;
    }
  default:
    ILU_ERR_CONS1(internal, err, minor, ilu_im_badTypeKind, 0);
    return NIL;
  }
}

static ilu_Type
  _IIOP_InputType2 (PACKET pk,
		    TypeStack *stack,
		    ilu_Error *err)
{
  ilu_cardinal		ctk;
  ilu_Type		t = NIL;

  _cdr_get_u32 (pk, &ctk, err);
  if (ILU_ERRNOK(*err)) return NIL;

  if (ctk == 0xFFFFFFFF) {

    ilu_integer	offset;

    _cdr_get_s32 (pk, &offset, err);
    if (ILU_ERRNOK(*err)) return NIL;
    t = find_type(*stack, (ilu_integer) (pk->vop + offset));

  } else {

    ILU_NOTE(IIOP_DEBUG,
	  ("_IIOP_InputType2:  unmarshalling TypeCode for %s type...\n",
	   CORBA_type_kind_name(ctk)));

    switch (ctk)
      {
      case CORBA_tk_null:
      case CORBA_tk_void:
      case CORBA_tk_shortinteger:
      case CORBA_tk_integer:
      case CORBA_tk_shortcardinal:
      case CORBA_tk_cardinal:
      case CORBA_tk_shortreal:
      case CORBA_tk_real:
      case CORBA_tk_boolean:
      case CORBA_tk_shortcharacter:
      case CORBA_tk_byte:
      case CORBA_tk_longinteger:
      case CORBA_tk_longcardinal:
      case CORBA_tk_longreal:
      case CORBA_tk_character:
      case CORBA_tk_variant:
      case CORBA_tk_type:
      case CORBA_tk_Principal:
	t = FindSimpleType(ctk, err);
	*stack = add_type(*stack, t, (ilu_integer) (pk->vop - 4), err);
	break;

	/* `simple' parameters */

      case CORBA_tk_string:
	{
	  ilu_cardinal limit;
	  
	  _cdr_get_u32(pk, &limit, err);
	  if (ILU_ERRNOK(*err)) break;
	  if (limit == 0) {
	    t = FindSimpleType(ctk, err);
	    if (ILU_ERRNOK(*err)) break;
	  } else {
	    t = ilu_MallocE(sizeof(ilu_Type_s), err);
	    if (ILU_ERRNOK(*err)) return NIL;
	    memset((void *) t, 0, sizeof(ilu_Type_s));
	    if (!FakeHeader(t, err)) { ilu_free(t);  return NIL; };
	    t = _IIOP_RegisterType (t, err);
	    if (ILU_ERRNOK(*err)) { ilu_free(t);  return NIL; };
	  }	  
	  *stack = add_type(*stack, t, (ilu_integer) (pk->vop - 8), err);
	  break;
	}

	/* `complex' parameters */

      case CORBA_tk_object:
      case CORBA_tk_record:
      case CORBA_tk_union:
      case CORBA_tk_enumeration:
      case CORBA_tk_sequence:
      case CORBA_tk_array:
      case CORBA_tk_alias:
      case CORBA_tk_except:
      case CORBA_tk_optional:
#ifdef ILU_REFERENCE_TYPES
      case CORBA_tk_reference:
#endif
	{
	  ilu_cardinal		parmsLen;
	  ilu_byte		byteOrder, oldByteOrder;
	  ilu_integer		where;
	  ilu_Type		t1, t2;

	  t1 = ilu_MallocE(sizeof(ilu_Type_s), err);
	  if (ILU_ERRNOK(*err)) return NIL;
	  memset((void *) t1, 0, sizeof(ilu_Type_s));
	  type_names(t1) = ilu_MallocE(sizeof(ilu_TypeName_s), err);
	  if (ILU_ERRNOK(*err)) return NIL;
	  memset((void *) type_names(t1), 0, sizeof(ilu_TypeName_s));
	  type_names_count(t1) = 1;
	  where = (ilu_integer) (pk->vop - 4);
	  *stack = add_type(*stack, t1, where, err);
	  _cdr_get_u32 (pk, &parmsLen, err);
	  if (ILU_ERRNOK(*err)) break;
	  _cdr_get_u8 (pk, &byteOrder, err);
	  if (ILU_ERRNOK(*err)) break;
	  ILU_NOTE(IIOP_DEBUG,
		("_IIOP_InputType2:  unmarshalling complex %s type parms, byte-order is %s...\n",
		 CORBA_type_kind_name(ctk), byteOrder ? "little-endian" : "big-endian"));
	  oldByteOrder = pk->byteorder;
	  pk->byteorder = (byteOrder ? LittleEndian : BigEndian);
	  t2 = DecodeTypeParms2 (t1, ctk, pk, stack, err);
	  if (t2 != t1)
	    replace_type(*stack, t2, where);
	  t1 = t2;
	  if (ILU_ERRNOK(*err)) break;
	  if (strcmp(type_name(t1), AnonymousTypeName) != 0) {
	    t2 = _IIOP_RegisterType (t1, err);
	    if (t2 != t1)
	      replace_type(*stack, t2, where);
	    t1 = t2;
	  }
	  pk->byteorder = oldByteOrder;
	  t = t1;
	  break;
	}

      default:
	ILU_ERR_CONS1(marshal, err, minor, ilu_mm_bad_typekind, 0);
	break;
      }

  }
  return (ILU_ERROK(*err) ? t : NIL);
}

static ilu_Type
  _IIOP_InputType (ilu_Call call,
		   ilu_Type *return_parm,
		   ilu_Error *err)
{
  TypeStack		s = NIL;
  ilu_Type		t;

  if (call_connection(call) == NIL) {
    ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0);
    return NIL;
  }

  t = _IIOP_InputType2 (iiop_packet(call), &s, err);
  free_list(s);

  if (ILU_ERROK(*err)) {
    if (t != NIL) {
      ILU_NOTE(IIOP_DEBUG,
	       ("_IIOP_InputType:  type is \"%s.%s\".\n",
		type_interface_name(t), type_name(t)));
    } else {
      ILU_NOTE(IIOP_DEBUG,
	       ("_IIOP_InputType:  type is `void'.\n"));
    }
    if (return_parm != NIL)
      *return_parm = t;
    return t;
  } else
    return NIL;
}

static ilu_cardinal
  TotalArrayVals (ilu_Type tp)
{
  /* return the total number of marshalled values for the value of type
     tp, which must be an array type */

  ilu_cardinal i, total;
  if (type_kind(tp) != ilu_array_tk)
    return 0;
  total = type_desc(tp).Array.dims[0];
  for (i = 1;  i < type_desc(tp).Array.n_dims;  i++)
    total = total * type_desc(tp).Array.dims[i];
  return total;
}

static ilu_boolean
  MatchValues (ilu_cardinal ov, ilu_Type disc_type, ilu_ConstantValue cv, ilu_Error *err)
{
  ILU_CLER(*err);

  if (cv->kind == ilu_byte_cvk)
    return ((ilu_byte) ov == cv->value.byte_val);
  else if (cv->kind == ilu_shortinteger_cvk)
    return ((ilu_shortinteger) ov == cv->value.shortinteger_val);
  else if (cv->kind == ilu_integer_cvk)
    return ((ilu_integer) ov == cv->value.integer_val);
  else if (cv->kind == ilu_shortcardinal_cvk)
    return ((ilu_shortcardinal) ov == cv->value.shortcardinal_val);
  else if (cv->kind == ilu_cardinal_cvk)
    return ((ilu_cardinal) ov == cv->value.cardinal_val);
  else if (cv->kind == ilu_boolean_cvk)
    return ((ilu_boolean) ov == cv->value.boolean_val);
  else if ((cv->kind == ilu_enumeration_cvk) &&
	   (type_kind(disc_type) == ilu_enumeration_tk))
    {
      ilu_cardinal i;

      for (i = 0;  i < type_desc(disc_type).Enumeration.n_elements;  i++)
	{
	  if (strcmp(cv->value.enumeration_val, type_desc(disc_type).Enumeration.elements[i].name) == 0)
	    return ((ilu_shortcardinal) ov == type_desc(disc_type).Enumeration.elements[i].value);
	}
    }
  else
    {
      ILU_NOTE(CALL_DEBUG, ("(ilu:MatchValues)  in unmarshalling union as opaque val, bad disc/constantvalue combo\n"));
      (void) ilu_Check(0, err);
    }
  return ilu_FALSE;
}

/* This function recursively descends a type structure and its
   associated pickled value, and figures the size of the value
   if re-marshalled according to IIOP rules */   
static ilu_cardinal
  SizePickledType (ilu_Call call,
		   ilu_Call pickle,
		   ilu_Type tp,
		   ilu_Error *err)
{
  ilu_cardinal	size = 0;

  switch (type_kind(tp)) {

  case ilu_byte_tk:
    {
      ilu_byte b;
      protocol_input_byte (call_proto(pickle), pickle, &b, err);
      if (ILU_ERRNOK(*err)) break;
      size += protocol_size_of_byte(call_proto(call), call, b, err);
    }
    break;

  case ilu_boolean_tk:
    {
      ilu_boolean b;
      protocol_input_boolean (call_proto(pickle), pickle, &b, err);
      if (ILU_ERRNOK(*err)) break;
      size += protocol_size_of_boolean(call_proto(call), call, b, err);
    }
    break;

  case ilu_character_tk:
    {
      ilu_character b;
      protocol_input_character (call_proto(pickle), pickle, &b, err);
      if (ILU_ERRNOK(*err)) break;
      size += protocol_size_of_character(call_proto(call), call, b, err);
    }
    break;

  case ilu_shortcharacter_tk:
    {
      ilu_shortcharacter b;
      protocol_input_shortchar (call_proto(pickle), pickle, &b, err);
      if (ILU_ERRNOK(*err)) break;
      size += protocol_size_of_shortchar(call_proto(call), call, b, err);
    }
    break;

  case ilu_shortinteger_tk:
    {
      ilu_shortinteger b;
      protocol_input_short_integer (call_proto(pickle), pickle, &b, err);
      if (ILU_ERRNOK(*err)) break;
      size += protocol_size_of_short_integer(call_proto(call), call, b, err);
    }
    break;

  case ilu_integer_tk:
    {
      ilu_integer b;
      protocol_input_integer (call_proto(pickle), pickle, &b, err);
      if (ILU_ERRNOK(*err)) break;
      size += protocol_size_of_integer(call_proto(call), call, b, err);
    }
    break;

  case ilu_longinteger_tk:
    {
      ilu_longinteger b;
      protocol_input_long_integer (call_proto(pickle), pickle, &b, err);
      if (ILU_ERRNOK(*err)) break;
      size += protocol_size_of_long_integer(call_proto(call), call, b, err);
    }
    break;

  case ilu_shortcardinal_tk:
    {
      ilu_shortcardinal b;
      protocol_input_short_cardinal (call_proto(pickle), pickle, &b, err);
      if (ILU_ERRNOK(*err)) break;
      size += protocol_size_of_short_cardinal(call_proto(call), call, b, err);
    }
    break;

  case ilu_cardinal_tk:
    {
      ilu_cardinal b;
      protocol_input_cardinal (call_proto(pickle), pickle, &b, err);
      if (ILU_ERRNOK(*err)) break;
      size += protocol_size_of_cardinal(call_proto(call), call, b, err);
    }
    break;

  case ilu_longcardinal_tk:
    {
      ilu_longcardinal b;
      protocol_input_long_cardinal (call_proto(pickle), pickle, &b, err);
      if (ILU_ERRNOK(*err)) break;
      size += protocol_size_of_long_cardinal(call_proto(call), call, b, err);
    }
    break;

  case ilu_real_tk:
    {
      ilu_real b;
      protocol_input_real (call_proto(pickle), pickle, &b, err);
      if (ILU_ERRNOK(*err)) break;
      size += protocol_size_of_real(call_proto(call), call, b, err);
    }
    break;

  case ilu_shortreal_tk:
    {
      ilu_shortreal b;
      protocol_input_short_real (call_proto(pickle), pickle, &b, err);
      if (ILU_ERRNOK(*err)) break;
      size += protocol_size_of_short_real(call_proto(call), call, b, err);
    }
    break;

  case ilu_longreal_tk:
    {
      ilu_longreal b;
      protocol_input_long_real (call_proto(pickle), pickle, &b, err);
      if (ILU_ERRNOK(*err)) break;
      size += protocol_size_of_long_real(call_proto(call), call, b, err);
    }
    break;

  case ilu_string_tk:
    {
      ilu_bytes b = NIL;
      ilu_cardinal encoding, blen;
      protocol_input_string (call_proto(pickle), pickle, (void **) &b, &blen,
			     type_desc(tp).String.limit,
			     type_desc(tp).String.char_set,
			     &encoding, err);
      if (ILU_ERRNOK(*err)) break;
      size += protocol_size_of_string(call_proto(call), call,
				      b, blen,
				      type_desc(tp).String.limit,
				      type_desc(tp).String.char_set,
				      encoding, err);
    }
    break;

  case ilu_object_tk:
    {
      ilu_Object h = NIL;
      protocol_input_object_id(call_proto(pickle), pickle, &h, ilu_FALSE, ilu_rootClass, err);
      /* now Inside(Server(h), ilu_rootClass) */
      if (ILU_ERRNOK(*err)) { if (h != NIL) ilu_ExitServer(object_server(h), ilu_rootClass); break; };
      size += protocol_size_of_object_id(call_proto(call), call, h, ilu_FALSE, ilu_rootClass, err);
      if (h != NIL) ilu_ExitServer(object_server(h), ilu_rootClass);
    }
    break;

  case ilu_optional_tk:
    {
      ilu_boolean b;
      protocol_input_optional (call_proto(pickle), pickle, &b, tp, err);
      if (ILU_ERRNOK(*err)) break;
      size += protocol_size_of_optional(call_proto(call), call, b, tp, err);
      if (ILU_ERRNOK(*err)) break;
      if (b)
	size += SizePickledType(call, pickle, type_desc(tp).Optional.type, err);
    }
    break;

#ifdef ILU_REFERENCE_TYPES

  case ilu_reference_tk:
    {
      ilu_boolean present;
      ilu_ReferenceID ref, ref2;
      ilu_cardinal wire_id;
      ilu_boolean must_marshal_value;

      wire_id = protocol_input_reference (call_proto(call), call, &present, &ref, err);
      if (ILU_ERRNOK(*err)) break;
      if (type_desc(tp).Reference.aliasedp && (ref == NIL))
	ref2 = (ilu_ReferenceID) wire_id;
      else
	ref2 = ref;
      size += protocol_size_of_reference (call_proto(call), call, present, &must_marshal_value, ref2, err);
      if (ILU_ERRNOK(*err)) break;
      if (must_marshal_value) {
	size += SizePickledType(call, pickle, type_desc(tp).Reference.type, err);
	if (ILU_ERRNOK(*err)) break;
      }
      protocol_end_input_reference (call_proto(call), call, wire_id, ref2, err);
      if (ILU_ERRNOK(*err)) break;
    }
    break;

#endif /* def ILU_REFERENCE_TYPES */

  case ilu_alias_tk:
    {
      size += SizePickledType(call, pickle, type_desc(tp).Alias.type, err);
    }
    break;

  case ilu_union_tk:
    {
      ilu_TypeKind disc_typekind;
      ilu_cardinal disc_value;
      ilu_cardinal i, j;
      ilu_boolean output_value = ilu_FALSE;
      ilu_UnionArm ua;

      /* first, read, store, and output the discriminant */

      disc_typekind = type_kind(type_desc(tp).Union.discriminant);
      protocol_input_union(call_proto(pickle), pickle, &disc_value, disc_typekind, tp, err);
      if (ILU_ERRNOK(*err)) break;
      size += protocol_size_of_union(call_proto(call), call, disc_value, disc_typekind, tp, err);
      if (ILU_ERRNOK(*err)) break;

      /* next, figure out which arm of the union is referred to */

      for (i = 0, output_value = ilu_FALSE;
	   i < type_desc(tp).Union.n_arms && !output_value;
	   i++)
	{
	  ua = &type_desc(tp).Union.arms[i];
	  for (j = 0;  j < ua->n_values && !output_value;  j++)
	    {
	      if (MatchValues(disc_value, type_desc(tp).Union.discriminant, &ua->values[j], err))
		{
		  size += SizePickledType (call, pickle, ua->base.type, err);
		  if (ILU_ERRNOK(*err)) break;
		  output_value = ilu_TRUE;
		}
	    }
	}
      if (! output_value) {
	/* no matching arm:  see if there's a default arm */
	if (type_desc(tp).Union.default_arm != 0) {
	  ilu_cardinal default_arm_index = type_desc(tp).Union.default_arm - 1;
	  size += SizePickledType (call, pickle, type_desc(tp).Union.arms[default_arm_index].base.type, err);
	  if (ILU_ERRNOK(*err)) break;
	} else if (type_desc(tp).Union.others_allowed) {
	  /* do nothing -- bogus discriminant values allowed */
	} else {
	  ILU_ERR_CONS1(marshal, err, minor, ilu_mm_bad_union_disc, 0);
	  break;
	}
      }
    }
    break;

  case ilu_sequence_tk:
    {
      if (type_kind(type_desc(tp).Sequence.type) == ilu_shortcharacter_tk) {
	ilu_bytes s = NIL;
	ilu_cardinal len, junk;
	protocol_input_string (call_proto(pickle), pickle, (void **) &s, &len,
			       type_desc(tp).Sequence.limit,
			       ILU_StringEncoding_latin1, &junk, err);
	if (ILU_ERRNOK(*err)) break;
	size += protocol_size_of_string(call_proto(call), call, s, len,
					type_desc(tp).Sequence.limit,
					ILU_StringEncoding_latin1, junk,
					err);
	ilu_free(s);
      } else if (type_kind(type_desc(tp).Sequence.type) == ilu_character_tk) {
	ilu_wstring s = NIL;
	ilu_cardinal len;
	protocol_input_wstring (call_proto(pickle), pickle, &s, &len,
				type_desc(tp).Sequence.limit, err);
	if (ILU_ERRNOK(*err)) break;
	size += protocol_size_of_wstring(call_proto(call), call, s, len,
					 type_desc(tp).Sequence.limit, err);
	ilu_free(s);
      } else if (type_kind(type_desc(tp).Sequence.type) == ilu_byte_tk) {
	ilu_bytes s = NIL;
	ilu_cardinal len;
	protocol_input_bytes (call_proto(pickle), pickle, &s, &len,
			      type_desc(tp).Sequence.limit, err);
	if (ILU_ERRNOK(*err)) break;
	size += protocol_size_of_bytes(call_proto(call), call, s, len,
				       type_desc(tp).Sequence.limit, err);
	ilu_free(s);
      } else {
	ilu_cardinal count, i;
	protocol_input_sequence (call_proto(pickle), pickle, &count, type_desc(tp).Sequence.limit, tp, err);
	if (ILU_ERRNOK(*err)) break;
	size += protocol_size_of_sequence(call_proto(call), call, count, type_desc(tp).Sequence.limit, tp, err);
	if (ILU_ERRNOK(*err)) break;
	for (i = 0;  i < count;  i++) {
	  size += SizePickledType(call, pickle, type_desc(tp).Sequence.type, err);
	  if (ILU_ERRNOK(*err)) break;
	}
      }
    }
    break;

  case ilu_record_tk:
    {
      ilu_cardinal count = type_desc(tp).Record.n_fields;
      ilu_cardinal i;
      ilu_Type supertype = type_desc(tp).Record.supertype;
      if (supertype != NIL)
	size += SizePickledType(call, pickle, supertype, err);
      else {
	protocol_input_record(call_proto(pickle), pickle, tp, err);
	if (ILU_ERRNOK(*err)) break;
	size += protocol_size_of_record(call_proto(call), call, tp, err);
      }
      if (ILU_ERRNOK(*err)) break;
      for (i = 0;  i < count;  i++) {
	size += SizePickledType(call, pickle, type_desc(tp).Record.fields[i].base.type, err);
	if (ILU_ERRNOK(*err)) break;
      }
    }
    break;

  case ilu_array_tk:
    {
      ilu_Type	tp2 = type_desc(tp).Array.type;
      ilu_cardinal count = TotalArrayVals(tp);

      /* three special cases for one-dimensional arrays of character, short character, and byte */

      if ((type_kind(type_desc(tp).Array.type) == ilu_character_tk) && (type_desc(tp).Array.n_dims == 1)) {
	ilu_wstring s;
	protocol_input_wstringvec(call_proto(pickle), pickle, &s, count, err);
	if (ILU_ERRNOK(*err)) break;
	size += protocol_size_of_wstringvec(call_proto(call), call, s, count, err);
	ilu_free(s);
      } else if ((type_kind(type_desc(tp).Array.type) == ilu_character_tk) && (type_desc(tp).Array.n_dims == 1)) {
	ilu_string s;
	protocol_input_stringvec(call_proto(pickle), pickle, &s, count, err);
	if (ILU_ERRNOK(*err)) break;
	size += protocol_size_of_stringvec(call_proto(call), call, s, count, err);
	ilu_free(s);
      } else if ((type_kind(type_desc(tp).Array.type) == ilu_byte_tk) && (type_desc(tp).Array.n_dims == 1)) {
	ilu_bytes s;
	protocol_input_opaque(call_proto(pickle), pickle, &s, count, err);
	if (ILU_ERRNOK(*err)) break;
	size += protocol_size_of_opaque(call_proto(call), call, s, count, err);
	ilu_free(s);
      } else {
	ilu_cardinal i;
	protocol_input_array(call_proto(pickle), pickle, tp, err);
	if (ILU_ERRNOK(*err)) break;
	size += protocol_size_of_array(call_proto(pickle), pickle, count, tp, err);
	if (ILU_ERRNOK(*err)) break;
	for (i = 0;  i < count;  i++) {
	  size += SizePickledType(call, pickle, tp2, err);
	  if (ILU_ERRNOK(*err)) break;
	}
      }
    }
    break;

  case ilu_enumeration_tk:
    {
      ilu_shortcardinal b;
      protocol_input_enum_code(call_proto(pickle), pickle, &b, tp, err);
      if (ILU_ERRNOK(*err)) break;
      size += protocol_size_of_enum_code(call_proto(call), call, b, tp, err);
    }
    break;

  case ilu_pickle_tk:
    {
      ilu_Pickle	p;
      protocol_input_pickle(call_proto(pickle), pickle, &p, tp, err);
      if (ILU_ERRNOK(*err)) break;
      size += protocol_size_of_pickle(call_proto(call), call, p, tp, err);
      ilu_free(p.pi_bytes);
    }
    break;

  default:
    {
      ILU_ERR_CONS1(marshal, err, minor, ilu_mm_bad_typekind, err);
    }
  }

  if (ILU_ERROK(*err)) return size;
  else return 0;
}

static ilu_cardinal
  _IIOP_SizeOfPickle (ilu_Call call,
		      ilu_Pickle pickle,
		      ilu_Type ptype,
		      ilu_Error *err)
{
  /* To figure out the marshalled size of a pickle, we first get the type,
     then build the typecode, then interpret the type to both unpickle the
     pickled value, and figure out its marshalled form on the wire. */

  ilu_cardinal	size;		/* marshalled size of pickle, including typecode */
  ilu_string	type_id;	/* type ID of the pickled value's type */
  ilu_Type	tp;		/* the pickled value's type */
  ilu_Call_s	pickleCall;	/* dummy call to use for unpickling */
  ilu_Pickle	p2;		/* second pickle for catching pickle from EndPickle */
  ilu_Error	lerr;		/* dummy err to use with EndPickle */

  /* first figure out the typecode and its size */
  type_id = ilu_PickleType(pickle, err);
  if (ILU_ERRNOK(*err)) return 0;
  tp = ilu_FindTypeByUID(type_id, err);
  if (ILU_ERRNOK(*err)) return 0;
  if (tp == NIL) {	/* unknown type in this address space */
    tp = _IIOP_DefaultPickleType;
    if (tp == ILU_NIL)
      return (ILU_ERR_CONS1(internal, err, minor, ilu_im_broken, 0));
  };
  size = _IIOP_SizeOfType (call, tp, err);
  if (ILU_ERRNOK(*err)) return 0;

  if (tp == _IIOP_DefaultPickleType) {
    size += _IIOP_SizeOfBytes (call, pickle.pi_bytes, pickle.pi_len, 0, err);
  } else {
    /* now figure out the size of the marshalled value by unpickling it
       on the fly, and then interpreting it */
    ilu_StartPickle(&pickleCall, NIL, err);
    if (ILU_ERRNOK(*err)) return 0;
    ilu_ReadPickle (&pickleCall, pickle, err);
    if (ILU_ERRNOK(*err)) {
      ilu_EndPickle(&pickleCall, &p2, &lerr);
      ILU_HANDLED(lerr);
      return 0;
    }
    size += SizePickledType (call, &pickleCall, tp, err);
    ilu_EndPickle(&pickleCall, &p2, &lerr);
    if (ILU_ERROK(*err))
      *err = lerr;
    else
      ILU_HANDLED(lerr);
  }
  if (ILU_ERRNOK(*err))
    return 0;
  else
    return size;
}

/* This function recursively descends a type structure and its
   associated pickled value, and outputs the value according
   to IIOP marshalling rules */
static void
  RemarshalMarshalledValue (ilu_Call output_call,
			    ilu_Call input_call,
			    ilu_Type tp,
			    ilu_Error *err)
{
  switch (type_kind(tp)) {

  case ilu_byte_tk:
    {
      ilu_byte b;
      protocol_input_byte (call_proto(input_call), input_call, &b, err);
      if (ILU_ERRNOK(*err)) break;
      protocol_output_byte(call_proto(output_call), output_call, b, err);
    }
    break;

  case ilu_boolean_tk:
    {
      ilu_boolean b;
      protocol_input_boolean (call_proto(input_call), input_call, &b, err);
      if (ILU_ERRNOK(*err)) break;
      protocol_output_boolean(call_proto(output_call), output_call, b, err);
    }
    break;

  case ilu_character_tk:
    {
      ilu_character b;
      protocol_input_character (call_proto(input_call), input_call, &b, err);
      if (ILU_ERRNOK(*err)) break;
      protocol_output_character(call_proto(output_call), output_call, b, err);
    }
    break;

  case ilu_shortcharacter_tk:
    {
      ilu_shortcharacter b;
      protocol_input_shortchar (call_proto(input_call), input_call, &b, err);
      if (ILU_ERRNOK(*err)) break;
      protocol_output_shortchar(call_proto(output_call), output_call, b, err);
    }
    break;

  case ilu_shortinteger_tk:
    {
      ilu_shortinteger b;
      protocol_input_short_integer (call_proto(input_call), input_call, &b, err);
      if (ILU_ERRNOK(*err)) break;
      protocol_output_short_integer(call_proto(output_call), output_call, b, err);
    }
    break;

  case ilu_integer_tk:
    {
      ilu_integer b;
      protocol_input_integer (call_proto(input_call), input_call, &b, err);
      if (ILU_ERRNOK(*err)) break;
      protocol_output_integer(call_proto(output_call), output_call, b, err);
    }
    break;

  case ilu_longinteger_tk:
    {
      ilu_longinteger b;
      protocol_input_long_integer (call_proto(input_call), input_call, &b, err);
      if (ILU_ERRNOK(*err)) break;
      protocol_output_long_integer(call_proto(output_call), output_call, b, err);
    }
    break;

  case ilu_shortcardinal_tk:
    {
      ilu_shortcardinal b;
      protocol_input_short_cardinal (call_proto(input_call), input_call, &b, err);
      if (ILU_ERRNOK(*err)) break;
      protocol_output_short_cardinal(call_proto(output_call), output_call, b, err);
    }
    break;

  case ilu_cardinal_tk:
    {
      ilu_cardinal b;
      protocol_input_cardinal (call_proto(input_call), input_call, &b, err);
      if (ILU_ERRNOK(*err)) break;
      protocol_output_cardinal(call_proto(output_call), output_call, b, err);
    }
    break;

  case ilu_longcardinal_tk:
    {
      ilu_longcardinal b;
      protocol_input_long_cardinal (call_proto(input_call), input_call, &b, err);
      if (ILU_ERRNOK(*err)) break;
      protocol_output_long_cardinal(call_proto(output_call), output_call, b, err);
    }
    break;

  case ilu_real_tk:
    {
      ilu_real b;
      protocol_input_real (call_proto(input_call), input_call, &b, err);
      if (ILU_ERRNOK(*err)) break;
      protocol_output_real(call_proto(output_call), output_call, b, err);
    }
    break;

  case ilu_shortreal_tk:
    {
      ilu_shortreal b;
      protocol_input_short_real (call_proto(input_call), input_call, &b, err);
      if (ILU_ERRNOK(*err)) break;
      protocol_output_short_real(call_proto(output_call), output_call, b, err);
    }
    break;

  case ilu_longreal_tk:
    {
      ilu_longreal b;
      protocol_input_long_real (call_proto(input_call), input_call, &b, err);
      if (ILU_ERRNOK(*err)) break;
      protocol_output_long_real(call_proto(output_call), output_call, b, err);
    }
    break;

  case ilu_string_tk:
    {
      ilu_bytes b = NIL;
      ilu_cardinal encoding, blen;
      protocol_input_string (call_proto(input_call), input_call, (void **) &b, &blen,
			     type_desc(tp).String.limit,
			     type_desc(tp).String.char_set,
			     &encoding, err);
      if (ILU_ERRNOK(*err)) break;
      protocol_output_string(call_proto(output_call), output_call,
			     b, blen,
			     type_desc(tp).String.limit,
			     type_desc(tp).String.char_set,
			     encoding, err);
    }
    break;

  case ilu_object_tk:
    {
      ilu_Object h = NIL;
      ilu_Class oc;
      protocol_input_object_id(call_proto(input_call), input_call, &h, ilu_FALSE, ilu_rootClass, err);
      /* now Inside(Server(h), ilu_rootClass) */
      if (ILU_ERRNOK(*err)) { if (h != NIL) ilu_ExitServer(object_server(h), ilu_rootClass); break; };
      if ((h != NIL) && (!class_collectible(object_class(h))) && server_is_true(object_server(h)))
	ilu_ReleaseMutex(ilu_gcmu);
      oc = (h != NIL) ? object_class(h) : ilu_FindClassFromID (CORBA_OBJECT_TYPE_ID);
      protocol_output_object_id(call_proto(output_call), output_call, h, ilu_FALSE, oc, err);
      /* now outside server */      
    }
    break;

  case ilu_optional_tk:
    {
      ilu_boolean b;
      protocol_input_optional (call_proto(input_call), input_call, &b, tp, err);
      if (ILU_ERRNOK(*err)) break;
      protocol_output_optional(call_proto(output_call), output_call, b, tp, err);
      if (ILU_ERRNOK(*err)) break;
      if (b)
	RemarshalMarshalledValue(output_call, input_call, type_desc(tp).Optional.type, err);
    }
    break;

#ifdef ILU_REFERENCE_TYPES

  case ilu_reference_tk:
    {
      ilu_boolean present;
      ilu_ReferenceID ref, ref2;
      ilu_cardinal wire_id;
      ilu_boolean must_marshal_value;

      wire_id = protocol_input_reference (call_proto(input_call), input_call, &present, &ref, err);
      if (ILU_ERRNOK(*err)) break;
      if (type_desc(tp).Reference.aliasedp && (ref == NIL))
	ref2 = (ilu_ReferenceID) wire_id;
      else
	ref2 = ref;
      protocol_output_reference (call_proto(output_call), output_call, present, &must_marshal_value, ref2, err);
      if (ILU_ERRNOK(*err)) break;
      if (must_marshal_value) {
	RemarshalMarshalledValue(output_call, input_call, type_desc(tp).Reference.type, err);
	if (ILU_ERRNOK(*err)) break;
      }
      protocol_end_input_reference (call_proto(input_call), input_call, wire_id, ref2, err);
      if (ILU_ERRNOK(*err)) break;
    }
    break;

#endif /* def ILU_REFERENCE_TYPES */

  case ilu_alias_tk:
    {
      RemarshalMarshalledValue(output_call, input_call, type_desc(tp).Alias.type, err);
    }
    break;

  case ilu_union_tk:
    {
      ilu_TypeKind disc_typekind;
      ilu_cardinal disc_value;
      ilu_cardinal i, j;
      ilu_boolean output_value = ilu_FALSE;
      ilu_UnionArm ua;

      /* first, read, store, and output the discriminant */

      disc_typekind = type_kind(type_desc(tp).Union.discriminant);
      protocol_input_union(call_proto(input_call), input_call, &disc_value, disc_typekind, tp, err);
      if (ILU_ERRNOK(*err)) break;
      protocol_output_union(call_proto(output_call), output_call, disc_value, disc_typekind, tp, err);
      if (ILU_ERRNOK(*err)) break;

      /* next, figure out which arm of the union is referred to */

      for (i = 0, output_value = ilu_FALSE;
	   i < type_desc(tp).Union.n_arms && !output_value;
	   i++)
	{
	  ua = &type_desc(tp).Union.arms[i];
	  for (j = 0;  j < ua->n_values && !output_value;  j++)
	    {
	      if (MatchValues(disc_value, type_desc(tp).Union.discriminant, &ua->values[j], err))
		{
		  RemarshalMarshalledValue(output_call, input_call, ua->base.type, err);
		  if (ILU_ERRNOK(*err)) break;
		  output_value = ilu_TRUE;
		}
	    }
	}
      if (! output_value) {
	/* no matching arm:  see if there's a default arm */
	if (type_desc(tp).Union.default_arm != 0) {
	  ilu_cardinal default_arm_index = type_desc(tp).Union.default_arm - 1;
	  RemarshalMarshalledValue(output_call, input_call, type_desc(tp).Union.arms[default_arm_index].base.type, err);
	  if (ILU_ERRNOK(*err)) break;
	} else if (type_desc(tp).Union.others_allowed) {
	  /* do nothing -- bogus discriminant values allowed */
	} else {
	  ILU_ERR_CONS1(marshal, err, minor, ilu_mm_bad_union_disc, 0);
	  break;
	}
      }

    }
    break;

  case ilu_sequence_tk:
    {
      /* three special cases, for character, short character, and byte */
      if (type_kind(type_desc(tp).Sequence.type) == ilu_shortcharacter_tk) {
	ilu_string s = NIL;
	ilu_cardinal len, junk;
	protocol_input_string (call_proto(input_call), input_call, (void **) &s, &len,
			       type_desc(tp).Sequence.limit,
			       ILU_StringEncoding_latin1, &junk, err);
	if (ILU_ERRNOK(*err)) break;
	protocol_output_string(call_proto(output_call), output_call, (ilu_bytes) s, len,
			       type_desc(tp).Sequence.limit,
			       ILU_StringEncoding_latin1, ILU_StringEncoding_latin1,
			       err);
	ilu_free(s);
      } else if (type_kind(type_desc(tp).Sequence.type) == ilu_character_tk) {
	ilu_wstring s = NIL;
	ilu_cardinal len;
	protocol_input_wstring (call_proto(input_call), input_call, &s, &len,
				type_desc(tp).Sequence.limit, err);
	if (ILU_ERRNOK(*err)) break;
	protocol_output_wstring(call_proto(output_call), output_call, s, len,
					 type_desc(tp).Sequence.limit, err);
	ilu_free(s);
      } else if (type_kind(type_desc(tp).Sequence.type) == ilu_byte_tk) {
	ilu_bytes s = NIL;
	ilu_cardinal len;
	protocol_input_bytes (call_proto(input_call), input_call, &s, &len,
			      type_desc(tp).Sequence.limit, err);
	if (ILU_ERRNOK(*err)) break;
	protocol_output_bytes(call_proto(output_call), output_call, s, len,
				       type_desc(tp).Sequence.limit, err);
	ilu_free(s);
      } else {
	ilu_cardinal count, i;
	ilu_cardinal limit = type_desc(tp).Sequence.limit;
	protocol_input_sequence (call_proto(input_call), input_call, &count, limit, tp, err);
	if (ILU_ERRNOK(*err)) break;
	protocol_output_sequence(call_proto(output_call), output_call, count, limit, tp, err);
	if (ILU_ERRNOK(*err)) break;
	for (i = 0;  i < count;  i++) {
	  RemarshalMarshalledValue(output_call, input_call, type_desc(tp).Sequence.type, err);
	  if (ILU_ERRNOK(*err)) break;
	}
      }
    }
    break;

  case ilu_record_tk:
    {
      ilu_cardinal count = type_desc(tp).Record.n_fields;
      ilu_cardinal i;
      ilu_Type supertype = type_desc(tp).Record.supertype;
      if (supertype != NIL)
	RemarshalMarshalledValue(output_call, input_call, supertype, err);
      else {
	protocol_input_record (call_proto(input_call), input_call, tp, err);
	if (ILU_ERRNOK(*err)) break;
	protocol_output_record(call_proto(output_call), output_call, tp, err);
      }
      if (ILU_ERRNOK(*err)) break;
      for (i = 0;  i < count;  i++) {
	RemarshalMarshalledValue(output_call, input_call, type_desc(tp).Record.fields[i].base.type, err);
	if (ILU_ERRNOK(*err)) break;
      }
    }
    break;

  case ilu_array_tk:
    {
      ilu_Type	tp2 = type_desc(tp).Array.type;
      ilu_cardinal count = TotalArrayVals(tp);

      /* three special cases for one-dimensional arrays of character, short character, and byte */

      if ((type_kind(type_desc(tp).Array.type) == ilu_character_tk) && (type_desc(tp).Array.n_dims == 1)) {
	ilu_wstring s = NIL;
	protocol_input_wstringvec(call_proto(input_call), input_call, &s, count, err);
	if (ILU_ERRNOK(*err)) break;
	protocol_output_wstringvec(call_proto(output_call), output_call, s, count, err);
	ilu_free(s);
      } else if ((type_kind(type_desc(tp).Array.type) == ilu_character_tk) && (type_desc(tp).Array.n_dims == 1)) {
	ilu_string s = NIL;
	protocol_input_stringvec(call_proto(input_call), input_call, &s, count, err);
	if (ILU_ERRNOK(*err)) break;
	protocol_output_stringvec(call_proto(output_call), output_call, s, count, err);
	ilu_free(s);
      } else if ((type_kind(type_desc(tp).Array.type) == ilu_byte_tk) && (type_desc(tp).Array.n_dims == 1)) {
	ilu_bytes s = NIL;
	protocol_input_opaque(call_proto(input_call), input_call, &s, count, err);
	if (ILU_ERRNOK(*err)) break;
	protocol_output_opaque(call_proto(output_call), output_call, s, count, err);
	ilu_free(s);
      } else {
	ilu_cardinal i = 0;
	protocol_input_array (call_proto(input_call), input_call, tp, err);
	if (ILU_ERRNOK(*err)) break;
	protocol_output_array(call_proto(output_call), output_call, i, tp, err);
	if (ILU_ERRNOK(*err)) break;
	for (i = 0;  i < count;  i++) {
	  RemarshalMarshalledValue(output_call, input_call, tp2, err);
	  if (ILU_ERRNOK(*err)) break;
	}
      }
    }
    break;

  case ilu_enumeration_tk:
    {
      ilu_shortcardinal b;
      protocol_input_enum_code(call_proto(input_call), input_call, &b, tp, err);
      if (ILU_ERRNOK(*err)) break;
      protocol_output_enum_code(call_proto(output_call), output_call, b, tp, err);
    }
    break;

  case ilu_pickle_tk:
    {
      ilu_Pickle	p;
      protocol_input_pickle(call_proto(input_call), input_call, &p, tp, err);
      if (ILU_ERRNOK(*err)) break;
      protocol_output_pickle(call_proto(output_call), output_call, p, tp, err);
      ilu_free(p.pi_bytes);
    }
    break;

  default:
    {
      ILU_ERR_CONS1(marshal, err, minor, ilu_mm_bad_typekind, err);
    }
  }
}

static void
  _IIOP_OutputPickle (ilu_Call call,
		      ilu_Pickle pickle,
		      ilu_Type the_type,
		      ilu_Error *err)
{
  /* To figure out the marshalled size of a pickle, we first get the type,
     then build the typecode, then interpret the type to both unpickle the
     pickled value, and resend it as IIOP on the wire */

  ilu_Type	tp;		/* the pickled value's type */
  ilu_string	type_id;	/* the pickled value's type ID */
  ilu_Call_s	pickleCall;	/* dummy call to use for unpickling */
  ilu_Pickle	p2;		/* second pickle for catching pickle from EndPickle */
  ilu_Error	lerr;		/* dummy err to use with EndPickle */

  /* first figure out the typecode and its size */
  type_id = ilu_PickleType(pickle, err);
  if (ILU_ERRNOK(*err)) return;
  tp = ilu_FindTypeByUID(type_id, err);
  if (ILU_ERRNOK(*err)) return;
  if (tp == NIL) {	/* unknown type in this address space */
    tp = _IIOP_DefaultPickleType;
    if (tp == ILU_NIL) {
      ILU_ERR_CONS1(internal, err, minor, ilu_im_broken, 0);
      return;
    }
  };
  _IIOP_OutputType (call, tp, err);
  if (ILU_ERRNOK(*err)) return;

  if (tp == _IIOP_DefaultPickleType) {
    _IIOP_OutputBytes (call, pickle.pi_bytes, pickle.pi_len, 0, err);
    ILU_CLER(lerr);
  } else {
    /* now figure out the size of the marshalled value by unpickling it
       on the fly, and then interpreting it */
    ilu_StartPickle(&pickleCall, NIL, err);
    if (ILU_ERRNOK(*err)) return;
    ilu_ReadPickle (&pickleCall, pickle, err);
    if (ILU_ERRNOK(*err)) {
      ilu_EndPickle(&pickleCall, &p2, &lerr);
      ILU_HANDLED(lerr);
      return;
    }
    RemarshalMarshalledValue (call, &pickleCall, tp, err);
    ilu_EndPickle(&pickleCall, &p2, &lerr);
  }
  if (ILU_ERROK(*err))
    *err = lerr;
  else
    ILU_HANDLED(lerr);
}

static ilu_boolean
  _IIOP_InputPickle (ilu_Call call,
		     ilu_Pickle *pickle,
		     ilu_Type the_type,
		     ilu_Error *err)
{
  /* To read a pickle (CORBA any), we first read the typecode, and register
     it with the ILU kernel, if it's not already known.  Then we make a new
     pickle, and then interpret the type structure, reading each element in
     off the wire, and adding it to the pickle. */

  ilu_Type	tp = NIL;	/* the value's type */
  ilu_Call_s	pickleCall;	/* dummy call to use for pickling */
  ilu_Error	lerr;		/* dummy err to use with EndPickle */

  _IIOP_InputType (call, &tp, err);
  if (ILU_ERRNOK(*err)) return ilu_FALSE;

  if (tp == _IIOP_DefaultPickleType) {
    pickle->pi_bytes = NIL;
    pickle->pi_len = 0;
    /* the sender didn't understand this type */
    _IIOP_InputBytes (call, &pickle->pi_bytes, &pickle->pi_len, 0, err);
  } else {
    /* now start a pickle for the type */
    ilu_StartPickle(&pickleCall, NIL, err);
    if (ILU_ERRNOK(*err)) return ilu_FALSE;
    ilu_WritePickle (&pickleCall, 0, type_uid(tp), err);
    if (ILU_ERRNOK(*err)) {
      ilu_Pickle p2;
      ilu_EndPickle(&pickleCall, &p2, &lerr);
      ILU_HANDLED(lerr);
      return ilu_FALSE;
    }
    RemarshalMarshalledValue (&pickleCall, call, tp, err);
    ilu_EndPickle(&pickleCall, pickle, &lerr);
    if (ILU_ERROK(*err))
      *err = lerr;
    else
      ILU_HANDLED(lerr);
  }
  return (ILU_ERROK(*err));
}

#endif /* ADD_VARIANT_SUPPORT */

/*======================================================================
**======================================================================
**====================  Non-I/O code ===================================
**======================================================================
**====================================================================*/

/* L1, L2 unconstrained */
static char *
  _IIOP_MessageTypeName (int message_type)
{
  char *message_type_names[] = {
    "Request", "Reply", "CancelRequest",
    "LocateRequest", "LocateReply",
    "CloseConnection", "MessageError" };

  if (message_type < 0 || message_type > (sizeof(message_type_names)/sizeof(char *)))
    return NIL;
  else
    return message_type_names[message_type];
}

/*L2, Main unconstrained*/
/*L1_sup < prmu*/
static ilu_string _IIOP_FormConcurrentProtocolHandle (ilu_refany pdata, ilu_Object obj)
{
  char buf[2048];
  char *t1;
  struct IIOP_DataBlock *s = (struct IIOP_DataBlock *) pdata;

  sprintf (buf, "iiop_%u_%u_%u", s->major, s->minor, s->mapping);
  t1 = _ilu_Strdup(buf);
  if (t1 == NIL) return NIL;
  if (strncmp(obj->ob_ih, CORBA_NATIVE_OBJECT_IH_PREFIX, SIZEOF_CORBA_NATIVE_OBJECT_IH_PREFIX) == 0)
    /* non-ILU object.  Recover the original object key and send it. */
    {
      t1 = _ilu_Strcat3(t1, "_", obj->ob_ih + SIZEOF_CORBA_NATIVE_OBJECT_IH_PREFIX);
    }
  return t1;
}
     
/*L2, Main unconstrained*/
/*L1_sup < prmu*/
static ilu_string _IIOP_FormSerialProtocolHandle (ilu_refany pdata, ilu_Object obj)
{
  char buf[2048];
  char *t1;
  struct IIOP_DataBlock *s = (struct IIOP_DataBlock *) pdata;

  sprintf (buf, "siiop_%u_%u_%u", s->major, s->minor, s->mapping);
  t1 = _ilu_Strdup(buf);
  if (t1 == NIL) return NIL;
  if (strncmp(obj->ob_ih, CORBA_NATIVE_OBJECT_IH_PREFIX, SIZEOF_CORBA_NATIVE_OBJECT_IH_PREFIX) == 0)
    /* non-ILU object.  Recover the original object key and send it. */
    {
      t1 = _ilu_Strcat3(t1, "_", obj->ob_ih + SIZEOF_CORBA_NATIVE_OBJECT_IH_PREFIX);
    }
  return t1;
}
     
static void _IIOP_EndMessage (ilu_Transport bs,
			      ilu_boolean input_p,
			      ilu_boolean push,
			      ilu_Error *err)
{
  ILU_CLER(*err);
  if (bs->tr_class->tc_boundaried)
    (void) transport_end_message(bs, push, NIL, err);
  else if (push && !input_p)
    bs->tr_class->tc_push(bs, err);
}

static ilu_boolean
  _IIOP_SendErrorMessage (ilu_Transport bs,
			  ilu_Error *err)
{
  struct {
    unsigned char magic[4];
    unsigned char major_version;
    unsigned char minor_version;
    unsigned char byte_order;
    unsigned char msg_type;
    ilu_cardinal  msg_size;
  } msgheader;
  PACKET p = NIL;

  p = _cdr_CreatePacket (bs, NATIVE_BYTE_ORDER, 0, err);
  if (ILU_ERRNOK(*err))
    return ilu_FALSE;

  if (bs->tr_class->tc_boundaried)
    {
      ilu_ReadHeaderResultCode ans;
      ans = transport_begin_message(bs, ilu_FALSE, err);
      switch (ans) {
      case ilu_rhrc_ok:
      case ilu_rhrc_eof:
      case ilu_rhrc_nothing:
      case ilu_rhrc_handled:
	break;
      case ilu_rhrc_error:
	ILU_NOTE((INCOMING_DEBUG | IIOP_DEBUG),
	      ("_IIOP_SendErrorMessage:  error %s "
	       "on transport_begin_message (output)\n",
	       ILU_ERR_NAME(*err)));
	goto errcase;
      default:
	_ilu_Assert(ilu_FALSE, "_IIOP_SendErrorMessage: bad ilu_ReadHeaderResultCode value");
      }
    }

  msgheader.magic[0] = 'G';
  msgheader.magic[1] = 'I';
  msgheader.magic[2] = 'O';
  msgheader.magic[3] = 'P';
  msgheader.major_version = 1;
  msgheader.minor_version = 0;
  msgheader.byte_order = (NATIVE_BYTE_ORDER == LittleEndian);
  msgheader.msg_type = GIOP_PacketType_MessageError;
  msgheader.msg_size = 0;

  if (transport_write_bytes(bs, (ilu_bytes) &msgheader, sizeof(msgheader), err), ILU_ERRNOK(*err))
    goto errcase;
  _IIOP_EndMessage (bs, ilu_FALSE, ilu_TRUE, err);
  if (ILU_ERRNOK(*err))
    goto errcase;

  if (p != NIL)
    { _cdr_destroy (p, err); ilu_free(p); }

  return ILU_ERROK(*err);

 errcase:
  ILU_NOTE(IIOP_DEBUG,
	("_IIOP_SendErrorMessage:  error %s on attempt to reply.\n",
	 ILU_ERR_NAME(*err)));
  if (p != NIL)
    { ilu_Error lerr; _cdr_destroy (p, &lerr); ILU_HANDLED(lerr); ilu_free(p); }
  return ilu_FALSE;
}

static ilu_boolean
  _IIOP_SendLocateReply (ilu_Transport bs,
			 ilu_cardinal serialNumber,
			 ilu_bytes object_key,
			 ilu_cardinal object_key_len,
			 ilu_Error *err)
{
  ilu_Server server = NIL;
  struct IIOP_IOR_IOR *ior = ILU_NIL;
  enum IIOP_LocateStatus status = IIOP_UNKNOWN_OBJECT;
  PACKET p = NIL;
  struct {
    unsigned char magic[4];
    unsigned char major_version;
    unsigned char minor_version;
    unsigned char byte_order;
    unsigned char msg_type;
    ilu_cardinal  msg_size;
    ilu_cardinal  request_id;
    ilu_cardinal  locate_status;
  } msgheader;

  ILU_NOTE(IIOP_DEBUG,
	("(ilu_IIOP:SendLocateReply): %lu, 0x%p (%ld bytes)\n",
	 serialNumber, object_key, (long) object_key_len));

  if (strncmp((ilu_string) object_key, "ilu", 4) == 0)
    {
      ilu_string mstid;
      ilu_string sid;
      ilu_string ih;
      ilu_Object h;

      /* We have an ILU object, go ahead and transform it */
      
      mstid = ((char *) object_key) + 4;
      sid = mstid + strlen(mstid) + 1;
      ih = sid + strlen(sid) + 1;

      server = _ilu_FindAndEnterServer(sid, ilu_FALSE, NIL, 0,
				       _ilu_rootClass, err);
      if (ILU_ERRNOK(*err))
	{
	  ILU_NOTE(IIOP_DEBUG,
		("_IIOP_SendLocateReply:  "
		 "No local server with sid \"%s\".\n", sid));
	  status = IIOP_UNKNOWN_OBJECT;
	}
      else
	{
	  if (server_objs(server) == NIL)
	    {
	      ILU_NOTE(IIOP_DEBUG,
		    ("%s %s is in closed server %s.\n",
		     "_IIOP_SendLocateReply:  instance", ih, sid));
	      status = IIOP_UNKNOWN_OBJECT;
	    }
	  else if ((h = _ilu_FindObjectInServer(ih, server)) == NIL)
	    {
	      ILU_NOTE(IIOP_DEBUG,
		    ("%s %s not found in server %s.\n",
		     "_IIOP_SendLocateReply:  instance", ih, sid));
	      status = IIOP_UNKNOWN_OBJECT;
	    }
	  else if (ilu_TrueInstanceP(h))
	    status = IIOP_OBJECT_HERE;
	  else
	    {
	      *err = IOROfObject (h, &ior);
	      if (ILU_ERRNOK(*err))
		status = IIOP_UNKNOWN_OBJECT;
	      else
		status = IIOP_OBJECT_FORWARD;
	      ILU_HANDLED(*err);
	    }
	  ilu_ExitServer(server, _ilu_rootClass);
	}
    }

#ifdef ENABLE_DEBUGGING
  if ((ilu_DebugLevel & IIOP_DEBUG) != 0)
    {
      ilu_string encoded_obj_key = encode (object_key, object_key_len);
      ilu_DebugPrintf ("_IIOP_SendLocateReply:  replying with %s to LocateRequest for <%s>\n",
		       (status == IIOP_UNKNOWN_OBJECT) ? "UNKNOWN_OBJECT" :
		       (status == IIOP_OBJECT_FORWARD) ? "OBJECT_FORWARD" :
		       (status == IIOP_OBJECT_HERE) ? "OBJECT_HERE" : "???",
		       encoded_obj_key);
      ilu_free(encoded_obj_key);
    }
#endif

  p = _cdr_CreatePacket (bs, NATIVE_BYTE_ORDER, 0, err);
  if (ILU_ERRNOK(*err))
    return ilu_FALSE;

  if (bs->tr_class->tc_boundaried)
    {
      ilu_ReadHeaderResultCode ans;
      ans = transport_begin_message(bs, ilu_FALSE, err);
      switch (ans) {
      case ilu_rhrc_ok:
      case ilu_rhrc_eof:
      case ilu_rhrc_nothing:
      case ilu_rhrc_handled:
	break;
      case ilu_rhrc_error:
	ILU_NOTE((INCOMING_DEBUG | IIOP_DEBUG),
	      ("_IIOP_SendLocateReply:  error %s "
	       "on transport_begin_message (output)\n",
	       ILU_ERR_NAME(*err)));
	goto errcase;
      default:
	_ilu_Assert(ilu_FALSE, "_IIOP_SendLocateReply: bad ilu_ReadHeaderResultCode value");
      }
    }

  msgheader.magic[0] = 'G';
  msgheader.magic[1] = 'I';
  msgheader.magic[2] = 'O';
  msgheader.magic[3] = 'P';
  msgheader.major_version = 1;
  msgheader.minor_version = 0;
  msgheader.byte_order = (NATIVE_BYTE_ORDER == LittleEndian);
  msgheader.msg_type = GIOP_PacketType_LocateReply;
  msgheader.msg_size = sizeof(msgheader) - 12;
  msgheader.request_id = serialNumber;
  msgheader.locate_status = status;

  if (status == IIOP_OBJECT_FORWARD)
    {
      /* adjust size to allow for marshalled IOR */

      int size;
	  ilu_cardinal i;
      size = strlen(ior->type_id) + 1;
      size = 4 + size + PADDING_NEC(size,4);	/* type id */
      size += 4;			/* nprofiles */
      for (i = 0;  i < ior->nProfiles;  i++)
	{
	  size += 4;	/* tag */
	  size += 4 + ior->Profile[i].profileDataLen;
	  if ((i + 1) < ior->nProfiles)
	    size += PADDING_NEC(ior->Profile[i].profileDataLen,4);
	}
      msgheader.msg_size += size;
    }

  if (transport_write_bytes(bs, (ilu_bytes) &msgheader, 20, err), ILU_ERRNOK(*err))
    goto errcase;
  if (status == IIOP_OBJECT_FORWARD)
    {
      ilu_cardinal i;
      
      if ((_cdr_put_bytes(p, (ilu_bytes)(ior->type_id), strlen(ior->type_id) + 1, err), ILU_ERRNOK(*err)) ||
	  (_cdr_put_u32 (p, ior->nProfiles, err), ILU_ERRNOK(*err)))
	goto errcase;
      for (i = 0;  i < ior->nProfiles;  i++)
	{
	  if ((_cdr_put_u32 (p, ior->Profile[i].tag, err), ILU_ERRNOK(*err)) ||
	      (_cdr_put_bytes (p, ior->Profile[i].profileData, ior->Profile[i].profileDataLen, err), ILU_ERRNOK(*err)))
	    goto errcase;
	}
    }
  _IIOP_EndMessage (bs, ilu_FALSE, ilu_TRUE, err);
  if (ILU_ERRNOK(*err))
    goto errcase;

  if (p != NIL)
    { _cdr_destroy (p, err); ilu_free(p); }

  return ILU_ERROK(*err);

 errcase:
  ILU_NOTE(IIOP_DEBUG,
	("_IIOP_SendLocateReply:  error %s on attempt to reply.\n",
	 ILU_ERR_NAME(*err)));
  if (p != NIL)
    { ilu_Error lerr; _cdr_destroy (p, &lerr); ILU_HANDLED(lerr); ilu_free(p); }
  return ilu_FALSE;
}

static ilu_boolean 
  _IIOP_DiscardInput (ilu_Call call, ILU_ERRS((internal)) * err)
{
  ILU_ERRS((IoErrs)) lerr;
  ilu_Transport t = iiop_transport(call);

  ILU_CLER(*err);
  if (t->tr_class->tc_boundaried) {
    transport_end_message(t, ilu_FALSE, NIL, &lerr);
    ILU_ERR_SWITCH(lerr) {
      ILU_SUCCESS_CASE
	return ILU_CLER(*err);
      ILU_ERR_CASE(internal, e) {
	if (e->minor == ilu_im_tcBytesDropped) {
	  ILU_HANDLED(lerr);
	  return ILU_CLER(*err);
	}
	*err = lerr;
	return ilu_FALSE;
      }
    } ILU_ERR_ENDSWITCH;
  } else {
    ilu_bytes b = NIL;
    ilu_cardinal len;
    if ((iiop_size(call) + 12) < ((ilu_cardinal) iiop_vop(call)))
      ILU_ERR_CONS1(marshal, err, minor, ilu_mm_eom, 0);
    else {
      len = (iiop_size(call) + 12) - ((ilu_cardinal) iiop_vop(call));
      if (len > 0) {
	_IIOP_InputOpaque(call, &b, len, err);
	if (ILU_ERROK(*err)) ilu_free(b);
      } else {
	ILU_CLER(*err);
      }
    }
  }
  return ILU_ERROK(*err);
}

static ilu_boolean 
  _IIOP_DiscardOutput (ilu_Call call, ILU_ERRS((internal)) * err)
{
  ILU_ERRS((IoErrs)) lerr;
  ilu_Transport t = iiop_transport(call);

  ILU_CLER(*err);
  if (t->tr_class->tc_boundaried) {
    transport_end_message(t, ilu_FALSE, NIL, &lerr);
    ILU_ERR_SWITCH(lerr) {
      ILU_SUCCESS_CASE
	return ILU_CLER(*err);
      ILU_ERR_CASE(internal, e) {
	if (e->minor == ilu_im_tcBytesDropped) {
	  ILU_HANDLED(lerr);
	  return ILU_CLER(*err);
	}
	*err = lerr;
	return ilu_FALSE;
      }
    } ILU_ERR_ENDSWITCH;
  } else {
    /* no good way to do this, so signal comm failure... */
    return ILU_ERR_CONS1(comm_failure, err, minor, ilu_cfm_protocol_sync_lost, ilu_FALSE);
  }
  return ILU_ERROK(*err);
}

static ilu_boolean
  _IIOP_SendObjectForward (ilu_Call call,
			   ilu_cardinal serial_number,
			   ilu_boolean locate_request,
			   ilu_Error *relocate,
			   ilu_Error *err)
{
  ilu_Server server = call_server(call);
  ilu_Transport bs = iiop_transport(call);
  struct IIOP_IOR_IOR *ior = ILU_NIL;
  PACKET p = NIL;
  typedef struct {
    ilu_cardinal  service_context;
    ilu_cardinal  request_id;
    ilu_cardinal  reply_status;
  } regular_reply_header;
  typedef struct {
    ilu_cardinal  request_id;
    ilu_cardinal  locate_status;
  } locate_reply_header;
  struct {
    struct {
      unsigned char magic[4];
      unsigned char major_version;
      unsigned char minor_version;
      unsigned char byte_order;
      unsigned char msg_type;
      ilu_cardinal  msg_size;
    } base;
    union {
      regular_reply_header	regular_reply;
      locate_reply_header	locate_reply;
    } reply_header;
  } msgheader;
  ilu_boolean responseExpected;
  ILU_ERRMEMP_T(relocate) reldata = ilu_ErrpQua_relocate(relocate);
  ilu_bytes key;
  ilu_cardinal keylen, i, size;
  char *mstid, *sid, *ih;

  if (reldata == NIL) {
    ILU_NOTE(IIOP_DEBUG,
	     ("_IIOP_SendObjectForwardException:  called with bad relocate err\n"));
    ILU_ERR_CONS1(internal, err, minor, ilu_im_broken, 0);
    return ilu_FALSE;
  };
#ifdef ENABLE_DEBUGGING
  if ((ilu_DebugLevel & IIOP_DEBUG) != 0) {
    ilu_CharBuf cinfo;
    cinfo = ilu_CharBufFromChars("", 0, err);
    if (ILU_ERRNOK(*err)) return ilu_FALSE;
    if (!ilu_Append1Cinfo(&cinfo, reldata->rel_pinfo, reldata->rel_tinfo, err))
      goto err0;
    ILU_NOTE(IIOP_DEBUG,
	     ("_IIOP_SendObjectForwardException:  relocating server <%s> to <%*.*s> (per-%s)\n",
	      server_id(call_server(call)), cinfo.icb_len, cinfo.icb_len,
	      cinfo.icb_base, ilu_RelocateScope_Name(reldata->rel_scope)));
    ilu_free(cinfo.icb_base);
  }
#endif
  /* now read in the discriminant, to re-form it */
  _IIOP_InputBoolean (call, &responseExpected, err);
  if (ILU_ERRNOK(*err))
    goto err0;
  else if (!(_IIOP_InputBytes(call, &key, &keylen, 0, err), ILU_ERROK(*err))) {
    call->ca_pe = ilu_ProtocolException_GarbageArguments;
    goto err0;
  };
  if (key == NIL || strncmp((ilu_string) key, "ilu", 4) != 0) {
    ILU_NOTE(INCOMING_DEBUG,
	     ("_IIOP_SendObjectForwardException:  incoming oid not an ILU object-key"));
    ILU_ERR_CONS1(marshal, err, minor, ilu_mm_alien_disc, 6);
    goto err1;
  };
  mstid = ((char *) key) + 4;
  sid = mstid + strlen(mstid) + 1;
  ih = sid + strlen(sid) + 1;
  ilu_EnterServerMutex(server, ilu_TRUE, err);
  if (ILU_ERRNOK(*err)) goto err1;
  if (strcmp(sid, server_id(server)) != 0) {
    ILU_NOTE(INCOMING_DEBUG,
	     ("%s %s is for wrong server (not %s).\n",
	      "_IIOP_SendObjectForwardException:  incoming oid sid", sid,
	      server_id(server)));
    ilu_ExitServerMutex(server, ilu_TRUE, err);
    ILU_HANDLED(*err);
    ILU_ERR_CONS1(marshal, err, minor, ilu_mm_alien_disc, 6);
    goto err1;
  };
  _IIOP_DiscardInput(call, err);
  if (ILU_ERRNOK(*err)) {
    ilu_Error lerr;
    ilu_ExitServerMutex (server, ilu_TRUE, &lerr);
    ILU_HANDLED(lerr);
    goto err1;
  };

  /* form new IOR for object... */
  *err = IOROfInfo (server, ih, mstid, reldata->rel_pinfo, reldata->rel_tinfo, &ior);
  if (ILU_ERRNOK(*err)) {
    ilu_Error lerr;
    ilu_ExitServerMutex (server, ilu_TRUE, &lerr);
    ILU_HANDLED(lerr);
    goto err1;
  };
  ilu_ExitServerMutex(server, ilu_TRUE, err);
  if (ILU_ERRNOK(*err)) goto err2;

  /* send the OBJECT_FORWARD message... */
  p = _cdr_CreatePacket (bs, NATIVE_BYTE_ORDER, 0, err);
  if (ILU_ERRNOK(*err)) goto err2;

  if (bs->tr_class->tc_boundaried)
    {
      ilu_ReadHeaderResultCode ans;
      ans = transport_begin_message(bs, ilu_FALSE, err);
      switch (ans) {
      case ilu_rhrc_ok:
      case ilu_rhrc_eof:
      case ilu_rhrc_nothing:
      case ilu_rhrc_handled:
	break;
      case ilu_rhrc_error:
	ILU_NOTE((INCOMING_DEBUG | IIOP_DEBUG),
	      ("_IIOP_SendObjectForwardException:  error %s "
	       "on transport_begin_message (output)\n",
	       ILU_ERR_NAME(*err)));
	goto err3;
      default:
	_ilu_Assert(ilu_FALSE, "_IIOP_SendObjectForwardException: bad ilu_ReadHeaderResultCode value");
      }
    }

  msgheader.base.magic[0] = 'G';
  msgheader.base.magic[1] = 'I';
  msgheader.base.magic[2] = 'O';
  msgheader.base.magic[3] = 'P';
  msgheader.base.major_version = 1;
  msgheader.base.minor_version = 0;
  msgheader.base.byte_order = (NATIVE_BYTE_ORDER == LittleEndian);
  msgheader.base.msg_type = GIOP_PacketType_Reply;
  msgheader.base.msg_size = sizeof(msgheader) - 12;
  if (locate_request) {
    msgheader.reply_header.locate_reply.request_id = serial_number;
    msgheader.reply_header.locate_reply.locate_status = IIOP_OBJECT_FORWARD;
  } else {
    msgheader.reply_header.regular_reply.service_context = 0;
    msgheader.reply_header.regular_reply.request_id = serial_number;
    msgheader.reply_header.regular_reply.reply_status = GIOP_ReplyStatusType_LOCATION_FORWARD;
  }

  /* adjust size to allow for marshalled IOR */
  size = strlen(ior->type_id) + 1;
  size = 4 + size + PADDING_NEC(size,4);	/* type id */
  size += 4;			/* nprofiles */
  for (i = 0;  i < ior->nProfiles;  i++)
    {
      size += 4;	/* tag */
      size += 4 + ior->Profile[i].profileDataLen;
      if ((i + 1) < ior->nProfiles)
	size += PADDING_NEC(ior->Profile[i].profileDataLen,4);
    }
  msgheader.base.msg_size += size;

  size = sizeof(msgheader.base) + (locate_request ? sizeof(locate_reply_header) : sizeof(regular_reply_header));
  if (transport_write_bytes(bs, (ilu_bytes) &msgheader, size, err), ILU_ERRNOK(*err))
    goto err3;
  if ((_cdr_put_bytes(p, (ilu_bytes)(ior->type_id), strlen(ior->type_id) + 1, err), ILU_ERRNOK(*err)) ||
      (_cdr_put_u32 (p, ior->nProfiles, err), ILU_ERRNOK(*err)))
    goto err3;
  for (i = 0;  i < ior->nProfiles;  i++)
    {
      if ((_cdr_put_u32 (p, ior->Profile[i].tag, err), ILU_ERRNOK(*err)) ||
	  (_cdr_put_bytes (p, ior->Profile[i].profileData, ior->Profile[i].profileDataLen, err), ILU_ERRNOK(*err)))
	goto err3;
    }
  _IIOP_EndMessage (bs, ilu_FALSE, ilu_TRUE, err);

 err3:
  if (p != NIL)
    { ilu_Error lerr; _cdr_destroy (p, &lerr); ILU_HANDLED(lerr); ilu_free(p); }
 err2:
  FreeIORData (ior);
 err1:
  ilu_free(key);
 err0:
  ILU_NOTE(IIOP_DEBUG,
	("_IIOP_SendObjectForwardException:  return status %s.\n",
	 ILU_ERR_NAME(*err)));
  return ILU_ERROK(*err);
}

/*Main Invariant holds*/
/*L2 >= {conn's iomu}*/

static ilu_boolean
  _IIOP_CheckBoundaries (ilu_Call call, int msgType, ilu_Error *err)
{
  ilu_Transport bs = iiop_transport(call);
  PACKET p = iiop_packet(call);
  int extra = (iiop_size(call) + 12) - (iiop_vop(call) - (ilu_byte *) 0);
  if (extra != 0)
    {
      ILU_NOTE(IIOP_DEBUG,
	    ("_IIOP_CheckBoundaries:  Warning:  "
	     "Received IIOP \"%s\" message on connection \"%s\" "
	     "with %d bytes of trailing garbage.\n",
	     _IIOP_MessageTypeName(msgType),
	     call_connection_id(call), extra));
      if (extra > 0)
	{
	  /* read in the extra bytes and discard them */
	  ilu_bytes foo = NIL;
	  _cdr_get_opaque (p, &foo, extra, err);
	  ilu_free(foo);
	}
      else if (extra < 0)
	{ /* ???? */ };
    }
  if (ILU_ERROK(*err))
    {
      _IIOP_EndMessage (bs, ilu_TRUE, ilu_TRUE, err);

      /* XXX -- should send error message at this point,
	 but will break some current implementations, such
	 as Black Widow 2.0beta. */
#if 0
      if (ILU_ERROK(*err) && extra != 0)
	_IIOP_SendErrorMessage (bs, err);
#endif
    }
  return ILU_ERROK(*err);
}


static ilu_ReadHeaderResultCode
  _IIOP_ReadHeader (ilu_Call call,
		    ilu_PacketType *type,
		    ilu_cardinal *sn,
		    ilu_ConnShutdownReason *reason,
		    ilu_cardinal *lastSN,
		    ILU_ERRS((IoErrs)) *err)
{
  ilu_Transport bs = iiop_transport(call);
  ilu_byte header[8];
  ilu_byte ptype;
  ilu_TransportReport report = { ilu_FALSE, ilu_FALSE };
  ilu_cardinal serialNumber = 0xFFFFFFFF, size;
  ilu_cardinal locateStatus;
  ilu_ReadHeaderResultCode ans;
  PACKET p = iiop_packet(call);
  ilu_cardinal short_char_codeset = ilu_IIOP_NATIVE_SHORT_CHAR_CODE_SET_ID, char_codeset = 0;

  if (bs->tr_class->tc_boundaried)
    {
      ans = transport_begin_message(bs, ilu_TRUE, err);
      switch (ans) {
      case ilu_rhrc_ok:
	break;
      case ilu_rhrc_error:
	ILU_NOTE((INCOMING_DEBUG | IIOP_DEBUG),
	      ("%s:  error %s on transport_begin_message (input)\n",
	       "_IIOP_ReadHeader", ILU_ERR_NAME(*err)));
	ILU_HANDLED(*err);
	return ILU_ERR_CONS1(comm_failure, err, minor, ilu_cfm_protocol_sync_lost, ilu_rhrc_error);
      case ilu_rhrc_eof:
	*reason = ilu_ConnShutdownReason_ReceivedEOF;
	*lastSN = 0;
      case ilu_rhrc_nothing:
      case ilu_rhrc_handled:
	return (ans);
      default:
	_ilu_Assert(ilu_FALSE, "iiop.c:ReadHeader -- bad ilu_ReadHeaderResultCode value");
      }
    }

  iiop_vop(call) = 0;

  /* Read bytes specially here, because we need to distinguish between EOF and
     no-data-available-without-blocking. */

  if ((size = transport_read_upto_bytes (bs, header, 8, &report, err)), ILU_ERRNOK(*err))
    return ilu_rhrc_error;
  else if (report.tr_eof) {
    *reason = ilu_ConnShutdownReason_ReceivedEOF;
    *lastSN = 0;
    return ilu_rhrc_eof;
  } else if ((size == 0) || report.tr_eom)
    return ilu_rhrc_nothing;
  else if (size != 8)
    {
      ILU_NOTE(IIOP_DEBUG,
	    ("_IIOP_ReadHeader:  partial message header encountered.  %ld bytes.\n",
	     (long) size));
      return ILU_ERR_CONS1(comm_failure, err, minor, ilu_cfm_protocol_sync_lost, ilu_rhrc_error);
    };

  iiop_vop(call) += 8;

  if (header[0] != 'G' ||
      header[1] != 'I' ||
      header[2] != 'O' ||
      header[3] != 'P')
    goto marshalError;

  if (header[4] != iiop_major_version(call) ||
      header[5] != iiop_minor_version(call))
    goto marshalError;

  ptype = header[7];
  if (ptype == GIOP_PacketType_Request)
    *type = ilu_PacketType_Request;
  else if (ptype == GIOP_PacketType_Reply)
    *type = ilu_PacketType_Reply;

  iiop_byte_order(call) = (header[6] == 1) ? LittleEndian : BigEndian;
  iiop_packetType(call) = ptype;

  _cdr_get_u32 (p, &iiop_size(call), err);
  if (ILU_ERRNOK(*err))
    return (ilu_rhrc_error);

  /* read the serial number... */
  switch (ptype)
    {
    case GIOP_PacketType_Reply:
    case GIOP_PacketType_Request:
      {
	/* skip any service context */
	ilu_cardinal i, count, id, datalen;
	ilu_bytes data = NIL;

	/* context handling */
	if (!(_cdr_get_u32 (p, &count, err), ILU_ERROK(*err))) goto marshalError;	/* # of service contexts */
	for (i = 0;  i < count;  i++)	/* for each context... */
	  {
	    data = NIL;
	    if (!(_cdr_get_u32 (p, &id, err), ILU_ERROK(*err)))	/* read the ID */
	      goto marshalError;
	    if (!(_cdr_get_bytes (p, &data, &datalen, 0, err), ILU_ERROK(*err)))	/* read the encapsulated value */
	      goto marshalError;
	    ILU_NOTE(IIOP_DEBUG,
		     ("_IIOP_ReadHeader:  service context of type %u, length %u%s\n",
		      id, datalen, (id == IIOP_TAG_CODE_SETS) ? "  (TAG_CODE_SETS)" : ""));
	    if (id == IIOP_TAG_CODE_SETS) {	/* interpret these */
	      InterpretCodeSetServiceContext(data, datalen, &short_char_codeset, &char_codeset, err);
	      if (ILU_ERRNOK(*err))
		goto marshalError;
	      iiop_short_char_codeset(call) = short_char_codeset;
	      iiop_char_codeset(call) = char_codeset;
	    }
	    ilu_free(data);	/* free the encapsulated value */
	  }

	/* now read the serial number */
	if (_cdr_get_u32 (p, &serialNumber, err), ILU_ERRNOK(*err))
	  goto marshalError;

	/* check for relocation... */
	if (server_is_true(call_server(call)) &&
	    server_relocatable_p(call_server(call)) &&
	    (!(iiop_data_block(call)->relocate_checked))) {
	  ilu_Error lerr;
	  iiop_data_block(call)->relocate_checked = ilu_TRUE;
	  ilu_EnterServerMutex(call_server(call), ilu_TRUE, err);
	  if (ILU_ERRNOK(*err)) return ilu_rhrc_error;
	  (*server_relocate_proc(call_server(call)))
	    (call_server(call), server_relocate_rock(call_server(call)), &lerr);
	  ilu_ExitServerMutex(call_server(call), ilu_TRUE, err);
	  if (ILU_ERRNOK(*err)) return ilu_rhrc_error;
	  ILU_ERR_SWITCH(lerr) {
	    ILU_ERR_CASE(relocate,v) {
	      _IIOP_SendObjectForward(call, serialNumber, ilu_FALSE, &lerr, err);
	      ILU_HANDLED(lerr);
	      if (ILU_ERROK(*err))
		return ilu_rhrc_handled;
	    } ILU_ERR_ELSE {
	      *err = lerr;
	    }
	  } ILU_ERR_ENDSWITCH;
	  if (ILU_ERRNOK(*err)) return ilu_rhrc_error;
	};
      }
      break;

    case GIOP_PacketType_CancelRequest:
      {
	ilu_cardinal req_to_cancel;
	if (_cdr_get_u32 (p, &req_to_cancel, err), ILU_ERRNOK(*err))
	  goto marshalError;
	_IIOP_CheckBoundaries(call, GIOP_PacketType_CancelRequest, err);
	ILU_NOTE(IIOP_DEBUG,
	      ("_IIOP_ReadHeader:  Cancel for request %lu received and discarded.\n",
	       (unsigned long) req_to_cancel));
      }
      break;

    case GIOP_PacketType_LocateRequest:
      {
	ilu_bytes object_key = NIL;
	ilu_cardinal object_key_len = 0;

	if (_cdr_get_u32 (p, &serialNumber, err), ILU_ERRNOK(*err))
	  goto marshalError;

	/* check for relocation... */
	if (server_is_true(call_server(call)) &&
	    server_relocatable_p(call_server(call)) &&
	    (!(iiop_data_block(call)->relocate_checked))) {
	  ilu_Error lerr;
	  iiop_data_block(call)->relocate_checked = ilu_TRUE;
	  ilu_EnterServerMutex(call_server(call), ilu_TRUE, err);
	  if (ILU_ERRNOK(*err)) return ilu_rhrc_error;
	  (*server_relocate_proc(call_server(call)))
	    (call_server(call), server_relocate_rock(call_server(call)), &lerr);
	  ilu_ExitServerMutex(call_server(call), ilu_TRUE, err);
	  if (ILU_ERRNOK(*err)) return ilu_rhrc_error;
	  ILU_ERR_SWITCH(lerr) {
	    ILU_ERR_CASE(relocate,v) {
	      _IIOP_SendObjectForward(call, serialNumber, ilu_TRUE, &lerr, err);
	      ILU_HANDLED(lerr);
	      if (ILU_ERROK(*err))
		return ilu_rhrc_handled;
	    } ILU_ERR_ELSE {
	      *err = lerr;
	    }
	  } ILU_ERR_ENDSWITCH;
	  if (ILU_ERRNOK(*err)) return ilu_rhrc_error;
	};

	/* now look at the object key... */
	if (_cdr_get_bytes (p, &object_key, &object_key_len, 0, err), ILU_ERRNOK(*err))
	  goto marshalError;
	_IIOP_CheckBoundaries (call, GIOP_PacketType_LocateRequest, err);
#ifdef ENABLE_DEBUGGING
	if ((ilu_DebugLevel & IIOP_DEBUG) != 0)
	  {
	    ilu_string encoded_object_key;
	    encoded_object_key = encode(object_key, object_key_len);
	    ilu_DebugPrintf ("_IIOP_ReadHeader:  LocateRequest for object <%s> received.\n",
			     encoded_object_key);
	    ilu_free(encoded_object_key);
	  }
#endif /* ENABLE_DEBUGGING */
	_IIOP_SendLocateReply (bs, serialNumber, object_key, object_key_len, err);
	ilu_free(object_key);
      }
      break;

    case GIOP_PacketType_LocateReply:
      {
	if ((_cdr_get_u32 (p, &serialNumber, err), ILU_ERRNOK(*err)) ||
	    (_cdr_get_u32 (p, &locateStatus, err), ILU_ERRNOK(*err)))
	  goto marshalError;
	if (locateStatus == IIOP_OBJECT_FORWARD) {
	  *type = ilu_PacketType_Reply;
	} else {
	  _IIOP_CheckBoundaries (call, GIOP_PacketType_LocateReply, err);
	  ILU_NOTE(IIOP_DEBUG,
		   ("_IIOP_ReadHeader:  LocateReply for request %lu received, status %lu.\n",
		    (unsigned long) serialNumber, (unsigned long) locateStatus));
	}
      }
      break;

    case GIOP_PacketType_MessageError:
      {
	serialNumber = 0;
	_IIOP_CheckBoundaries (call, GIOP_PacketType_MessageError, err);
	ILU_NOTE(IIOP_DEBUG,
	      ("_IIOP_ReadHeader:  MessageError advisory received.\n"));
      }
      break;

    case GIOP_PacketType_CloseConnection:
      {
	serialNumber = 0;
	_IIOP_CheckBoundaries (call, GIOP_PacketType_CloseConnection, err);
	ILU_NOTE(IIOP_DEBUG,
	      ("_IIOP_ReadHeader:  CloseConnection advisory received.\n"));
      }
      break;
    };

 marshalError:

#ifdef ENABLE_DEBUGGING
  if ((ilu_DebugLevel & IIOP_DEBUG) != 0)
    {
      char *mtype = _IIOP_MessageTypeName(ptype);
      ilu_DebugPrintf ("%s%s #%lu, %u bytes, %s, short char codeset %08x, char codeset %08x.\n",
		       "_IIOP_ReadHeader:  ",
		       (mtype == NIL) ? "(unrecognized message)" : mtype,
		       serialNumber, iiop_size(call),
		       (p->byteorder == BigEndian) ? "big-endian" : "little-endian",
		       short_char_codeset, char_codeset);
    }
#endif
	 
  if (ILU_ERRNOK(*err))
    return ilu_rhrc_error;
  else if ((ptype == GIOP_PacketType_Request) ||
	   (ptype == GIOP_PacketType_Reply) ||
	   ((ptype == GIOP_PacketType_LocateReply) &&
	    (locateStatus == IIOP_OBJECT_FORWARD)))
    {
      *sn = serialNumber;
      return ilu_rhrc_ok;
    }
  else
    return ilu_rhrc_handled;
}

static ilu_refany 
  _IIOP_DelayInterp(ilu_Call call,
		    ILU_ERRS((IoErrs)) * err)
{
  PACKET          p;
  ilu_cardinal    nbytes = 0;
  ilu_Transport   temp;
  ilu_boolean     byBytes;

  if ((byBytes = !transport_boundaried(iiop_transport(call))))
    nbytes = (iiop_size(call) + 12) - ((ilu_cardinal) iiop_vop(call));
  temp = _ilu_BufferInputMessage(iiop_transport(call), nbytes,
				 byBytes, err);
  if (ILU_ERRNOK(*err))
    return NIL;
  iiop_vop(call) += nbytes;
  p = _cdr_CreatePacket(temp, iiop_byte_order(call), 0, err);
  if (ILU_ERRNOK(*err)) {
    ilu_integer     cdfd;
    ilu_Error       lerr;
    (void) transport_close(temp, &cdfd, &lerr);
    _ilu_Assert(cdfd == 0, "IIOP DelayInterp");
    ILU_HANDLED(lerr);
    return NIL;
  }
  p->bs_needs_closing_on_finish_call = ilu_TRUE;
  return (ilu_refany) p;
}

static void
  _IIOP_ResumeInterp(ilu_Call call, ilu_refany x)
{
  iiop_set_packet(call,x);
  return;
}

static ilu_boolean
  _IIOP_AbandonDelayedInterp(ilu_Connection conn,
			     ilu_refany x, ILU_ERRS((internal)) * err)
{
  PACKET p = (PACKET) x;
  packet_destroy(p, err);
  ilu_free(p);
  return ILU_ERROK(*err);
}

static void
  _IIOP_RequestRead (ilu_Call call, ILU_ERRS((IoErrs)) *err)
{
  ilu_Transport bs = iiop_transport(call);
  iiop_vop(call) = 0;
  _IIOP_EndMessage (bs, ilu_TRUE, ilu_TRUE, err);
}

static void
  _IIOP_ReplyRead (ilu_Call call, ILU_ERRS((IoErrs)) *err)
{
  ilu_Transport bs = iiop_transport(call);
  iiop_vop(call) = 0;
  _IIOP_EndMessage (bs, ilu_TRUE, ilu_TRUE, err);
}

static ilu_boolean
  _IIOP_InterpretRequest (ilu_Call call, ILU_ERRS((IoErrs)) *err)
{
  ilu_boolean responseExpected;
  ilu_cardinal method_name_len = 0, junk;
  ilu_string method_name;
  ilu_Class putative_class;

  if (iiop_short_char_codeset(call) != ilu_IIOP_NATIVE_SHORT_CHAR_CODE_SET_ID)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_short_char_codeset, ilu_FALSE);

  if (iiop_packetType(call) == GIOP_PacketType_Request)
    {
      _IIOP_InputBoolean (call, &responseExpected, err);
      if (ILU_ERRNOK(*err))
	return ilu_FALSE;
      else
	{
	  if (!(_IIOP_InputBytes(call, &iiop_objKey(call), &iiop_objKeyLen(call), 0, err), ILU_ERROK(*err)))
	    {
	      call->ca_pe = ilu_ProtocolException_GarbageArguments;
	      return ilu_FALSE;
	    }
	  if (!(_IIOP_InputString(call, (void **) &method_name, &method_name_len, 0,
				  0, &junk, err), ILU_ERROK(*err)))
	    {
	      call->ca_pe = ilu_ProtocolException_GarbageArguments;
	      return ilu_FALSE;
	    }
	}
      _ilu_AcquireMutex(ilu_prmu);
      putative_class = FindClassFromObjectKey (iiop_objKey(call), iiop_objKeyLen(call), call_server(call));
      _ilu_ReleaseMutex(ilu_prmu);
      if (putative_class == NIL)
	{
	  ILU_NOTE(IIOP_DEBUG,
		("%s  (call %lu) %s %p (%ul)\n",
		 "_IIOP_InterpretRequest:", call->ca_SN,
		 "Can't find ilu_Class for object_key", iiop_objKey(call), iiop_objKeyLen(call)));
	  call->ca_pe = ilu_ProtocolException_NoSuchClassAtServer;
	  return ilu_FALSE;      
	}

      FindClassAndMethodFromIDLMethodName (call, putative_class, method_name);

      if (call->ca_method == NIL)
	{
	  ILU_NOTE(IIOP_DEBUG,
		("%s  (call %lu) %s \"%s\" with methodID = <%s>.\n",
		 "_IIOP_InterpretRequest:", call->ca_SN,
		 "Can't find method on class", class_name(putative_class),
		 method_name));
	  ilu_free(method_name);
	  call->ca_pe = ilu_ProtocolException_NoSuchMethodOnClass;
	  return ILU_ERR_CONS1(bad_operation, err, minor, ilu_bom_noSuchOperationOnType, ilu_FALSE);
	}
      else
	{
	  ILU_NOTE(IIOP_DEBUG,
		("%sclass %s, method %s is %p (%s).\n",
		 "_IIOP_InterpretRequest:  ", class_name(putative_class),
		 method_name, call->ca_method, call->ca_method->me_name));
	}
      ilu_free(method_name);
      /* Now read and remember the info about the Principal */
      _IIOP_InputBytes (call, &iiop_principal(call), &iiop_principalLen(call), 0xFFFF, err);
      if (ILU_ERRNOK(*err))
	{
	  ILU_NOTE((IIOP_DEBUG | SECURITY_DEBUG),
		("%s:  error <%s> fetching principal of call %lu (%s)\n",
		 ILU_ERR_NAME(*err), (long unsigned) call_serial_number(call),
		 method_name(call_method(call))));
	  call->ca_pe = ilu_ProtocolException_RequestRejected;
	  return ilu_FALSE;
	}
      else
	{
	  ILU_NOTE((IIOP_DEBUG | SECURITY_DEBUG),
		("%s:  Principal of call %lu (%s) is %lu bytes at %p\n",
		 "_IIOP_InterpretRequest",  (long unsigned) call_serial_number(call),
		 method_name(call_method(call)), (long unsigned) iiop_principalLen(call),
		 iiop_principal(call)));
	  FREETOKEN(iiop_principal(call));
	  iiop_principal(call) = NIL;
	  iiop_principalLen(call) = 0;
	}
      ILU_NOTE(IIOP_DEBUG,
	    ("_IIOP_InterpretRequest:  returning ilu_TRUE\n"));
      return (ilu_TRUE);
    }
  else
    return (ILU_ERR_CONS1(marshal, err, minor, ilu_mm_msgTypeUnknown, ilu_FALSE));
}

/*L1, Main unconstrained*/
/*L2 >= {conn's iomu}*/

static void
  HandleRelocateMessage (ilu_Call call,
			 ilu_Error *err)
{
  /* Remaining data of call contains an IOR, to be interpreted and signalled
     back in an ILU "relocate" error. */

  ilu_string typeid = NIL;
  ilu_cardinal nprofiles;
  struct IIOP_IOR_IOR *p = NIL;
  ilu_cardinal len, i;
  ilu_string cinfo = NIL;
  ilu_cardinal cinfolen, junk;
  ilu_cardinal short_char_codeset = 0;
  ilu_cardinal char_codeset = 0;
  ilu_ProtocolInfo	new_pinfo;
  ilu_TransportInfo	new_tinfo;

  if (protocol_input_string(call_proto(call), call,
			    (void **) &typeid, &len, 0xFFFF,
			    ILU_StringEncoding_latin1, &junk,
			    err), ILU_ERRNOK(*err))
    goto marshalError;
  if (protocol_input_cardinal(call_proto(call), call, &nprofiles, err), ILU_ERRNOK(*err))
    goto marshalError;

  if (nprofiles == 0)
    goto marshalError;
  
  if (p = ilu_MallocE(sizeof(struct IIOP_IOR_IOR) + ((nprofiles - 1) * sizeof(struct IIOP_IOR_TaggedProfile)), err),
      ILU_ERRNOK(*err)) goto marshalError;
  p->type_id = typeid;
  typeid = NIL;
  p->nProfiles = 0;
  for (i = 0;  i < nprofiles;  i++)
    {
      if (protocol_input_cardinal(call_proto(call), call, &p->Profile[i].tag, err), ILU_ERRNOK(*err))
	goto marshalError;
      p->Profile[i].profileData = NIL;
      if (protocol_input_bytes(call_proto(call), call, &p->Profile[i].profileData,
			       &p->Profile[i].profileDataLen, 0xFFFF, err), ILU_ERRNOK(*err))
	goto marshalError;
      p->nProfiles++;
    }

  /* Now see if we can parse the IOR */

  if (ParseIOR(p, NIL, NIL, NIL, NIL, &cinfo, &cinfolen, &short_char_codeset, &char_codeset, err) &&
      _ilu_ParseConnectInfo(cinfo, cinfolen, &new_pinfo, &new_tinfo, err)) {
    ILU_ERR_CONS3(relocate,err,rel_scope,ilu_relocate_conn,rel_pinfo,new_pinfo,rel_tinfo,new_tinfo,0);
    ilu_free(cinfo);
  };

 marshalError:
  if (typeid != NIL) ilu_free(typeid);
  if (p != NIL) { FreeIORData(p); ilu_free(p); };
  return;
}

static ilu_ProtocolException _IIOP_InterpretReply (ilu_Call call,
						   ilu_cardinal *exception_code,
						   ILU_ERRS((IoErrs)) *err)
{
  if (iiop_packetType(call) == GIOP_PacketType_LocateReply) {
    HandleRelocateMessage (call, err);
    *exception_code = 0;
    return ilu_ProtocolException_Not;
  } else {
    ilu_cardinal replyStatus, junk;

    _IIOP_InputCardinal (call, &replyStatus, err);

    ILU_NOTE(IIOP_DEBUG,
	     ("_IIOP_InterpretReply:  replyStatus on reply %lu is %lu\n",
	      call_serial_number(call), replyStatus));

    if (ILU_ERRNOK(*err))
      return ilu_ProtocolException_Not;

    if (replyStatus == GIOP_ReplyStatusType_NO_EXCEPTION)
      {
	*exception_code = 0;
	return (ilu_ProtocolException_Success);
      }
    else if (replyStatus == GIOP_ReplyStatusType_USER_EXCEPTION)
      {
	ilu_string exception_name = NIL;
	ilu_cardinal exception_name_len = 0;
	if (_IIOP_InputString (call, (void **) &exception_name, &exception_name_len, 0,
			       0, &junk, err), ILU_ERRNOK(*err))
	  return ilu_ProtocolException_Not;      
	*exception_code = FigureExceptionIndexFromIDLName (call_intro_type(call), call_method(call), exception_name);
	ilu_free(exception_name);
	if (*exception_code == 0)
	  return (ILU_ERR_CONS1(marshal, err, minor, ilu_mm_excn_id, ilu_ProtocolException_Not));
	else
	  return (ilu_ProtocolException_Success);
      }
    else if (replyStatus == GIOP_ReplyStatusType_SYSTEM_EXCEPTION)
      {
	ilu_string exception_name = NIL;
	ilu_cardinal exception_name_len = 0;
	ilu_cardinal i, junk;
	ilu_cardinal minor;
	ilu_cardinal completed;

	if (_IIOP_InputString (call, (void **) &exception_name, &exception_name_len, 0,
			       0, &junk, err), ILU_ERRNOK(*err))
	  return ilu_ProtocolException_Not;
	i = FigureExceptionIndexFromIDLName (NIL, NIL, exception_name);
	ilu_free(exception_name);
	if (_IIOP_InputCardinal (call, &minor, err), ILU_ERRNOK(*err))
	  return ilu_ProtocolException_Not;
	if (_IIOP_InputCardinal (call, &completed, err), ILU_ERRNOK(*err))
	  return ilu_ProtocolException_Not;
	*exception_code = minor;
	ILU_NOTE(IIOP_DEBUG,
		 ("_IIOP_InterpretReply:  system exception <%s> (mapped to protocol error %s) received, minor code %lu,"
		  " completed %s\n", exception_name, ilu_PEName(i), (unsigned long) minor,
		  (completed == 0) ? "YES" : ((completed == 1) ? "NO" : ((completed == 2) ? "MAYBE" : "INVALID"))));	     
	return (i);
      }
    else if (replyStatus == GIOP_ReplyStatusType_LOCATION_FORWARD)
      {
	ILU_NOTE(IIOP_DEBUG,
		 ("_IIOP_InterpretReply:  IIOP LOCATION_FORWARD reply received...\n"));
	HandleRelocateMessage (call, err);
	*exception_code = 0;
	return ilu_ProtocolException_Not;
      }
    else
      {
	ILU_NOTE(IIOP_DEBUG,
		 ("_IIOP_InterpretReply:  unexpected reply status %lu.\n", (unsigned long) replyStatus));
	return ILU_ERR_CONS1(marshal, err, minor, ilu_mm_msgTypeUnknown, ilu_ProtocolException_Not);
      }
  }
}

/*L1_sup < prmu*/

static ilu_boolean
  _IIOP_StartRequest (ilu_Call call,
		      ilu_cardinal argSize,
		      ILU_ERRS((IoErrs)) *err)
{
  static ilu_byte magic[] = "GIOP";
  ilu_cardinal packetSize;
  ilu_Class pclass = call_intro_type(call);
  ilu_Method method = call_method(call);
  ilu_Transport bs = iiop_transport(call);
  struct IIOP_DataBlock *db = (struct IIOP_DataBlock *) connection_protocol_data(call_connection(call));

  ILU_NOTE(IIOP_DEBUG,
	("%s %p (sn %lu), argSize %lu, class %s (%s), meth %s (%lu)\n",
	 "_IIOP_StartRequest:  call", call,
	 call_serial_number(call), argSize, class_name(pclass),
	 class_unique_id(pclass), method_name(method),
	 method_id(method)));

  Initialize(err);
  if (ILU_ERRNOK(*err))
    return ilu_FALSE;

  /* See the sizing code in _IIOP_SizeOfObjectID */
  packetSize = argSize + 21 + (db->charsets_sent ? 0 : (8 + CharSetsServiceContextPaddedLength));

  if (bs->tr_class->tc_boundaried)
    {
      if (transport_begin_message(bs, ilu_FALSE, err)
	  != ilu_rhrc_ok)
	return ilu_FALSE;
    }

  iiop_vop(call) = 0;
  iiop_size(call) = packetSize;

  if ((_IIOP_OutputOpaque (call, magic, 4, err), ILU_ERRNOK(*err)) ||
      (_IIOP_OutputByte (call, (ilu_byte)(iiop_major_version(call) & 0xFF), err), ILU_ERRNOK(*err)) ||
      (_IIOP_OutputByte (call, (ilu_byte)(iiop_minor_version(call) & 0xFF), err), ILU_ERRNOK(*err)) ||
      (_IIOP_OutputByte (call,
			 (NATIVE_BYTE_ORDER == LittleEndian),
			 err), ILU_ERRNOK(*err)) ||
      (_IIOP_OutputByte (call, GIOP_PacketType_Request, err), ILU_ERRNOK(*err)) || 
      (_IIOP_OutputCardinal (call, packetSize - 12, err), ILU_ERRNOK(*err)))
    return (ilu_FALSE);
  if (db->charsets_sent) {
    if (/* no service contexts need be sent with this request */
	(_IIOP_OutputCardinal (call, 0, err), ILU_ERRNOK(*err)))
      return ilu_FALSE;
  } else {
    if (
	/* context carries character set and character code set info */
	(_IIOP_OutputCardinal (call, 1, err), ILU_ERRNOK(*err)) ||
	(_IIOP_OutputCardinal (call, IIOP_TAG_CODE_SETS, err), ILU_ERRNOK(*err)) ||
	(_IIOP_OutputBytes (call, CharSetsServiceContext, CharSetsServiceContextLength, 0xFFFF, err),
	 ILU_ERRNOK(*err)))
      return ilu_FALSE;
    db->charsets_sent = ilu_TRUE;
  }
  if (/* serial number */
      (_IIOP_OutputCardinal (call, call_serial_number(call), err), ILU_ERRNOK(*err)) ||
      /* response expected? */
      (_IIOP_OutputByte (call, (ilu_byte) (! (method_asynchronous(method))), err), ILU_ERRNOK(*err)))
    return (ilu_FALSE);

  /* The object ID and method ID will be output during the call to 
     marshall the discriminant */

  ILU_NOTE(IIOP_DEBUG,
	("_IIOP_StartRequest:  request %lu begun (argsize %lu).\n",
	 call_serial_number(call), argSize));
  return (ilu_TRUE);
}

static          ilu_boolean
_IIOP_FinishRequest(ilu_Call call, ilu_Message * msg,
		    ilu_boolean push, ILU_ERRS((IoErrs)) * err)
{
  ilu_Transport   t = iiop_transport(call);

  if (((ilu_cardinal) iiop_vop(call)) != iiop_size(call)) {
    ILU_NOTE(IIOP_DEBUG,
	     ("IIOP bad message size %lu in \"%s\" request %ld.  %lu bytes actually output.\n",
	      (long unsigned) iiop_size(call),
	      method_name(call_method(call)),
	      (long) call_serial_number(call),
	      (long) iiop_vop(call)));
  }
  _IIOP_EndMessage(t, ilu_FALSE, push, err);
  iiop_vop(call) = 0;
  return ILU_ERROK(*err);
}

static          ilu_boolean
_IIOP_FinishReply(ilu_Call call, ilu_boolean push,
		  ILU_ERRS((IoErrs)) * err)
{
  ilu_Transport   t = iiop_transport(call);

  if (((ilu_cardinal) iiop_vop(call)) != iiop_size(call)) {
    ILU_NOTE(IIOP_DEBUG,
	     ("IIOP bad message size %lu in \"%s\" reply %ld.  %lu bytes actually output.\n",
	      (long unsigned) iiop_size(call),
	      method_name(call_method(call)),
	      (long) call_serial_number(call),
	      (long unsigned) iiop_vop(call)));
  }
  _IIOP_EndMessage(t, ilu_FALSE, push, err);
  return ILU_ERROK(*err);
}

static          ilu_boolean
_IIOP_FinishException(ilu_Call call, ilu_boolean push,
		      ILU_ERRS((IoErrs)) * err)
{
  ilu_Transport   t = iiop_transport(call);

  if (((ilu_cardinal) iiop_vop(call)) != iiop_size(call)) {
    ILU_NOTE(IIOP_DEBUG,
	     ("IIOP bad message size %lu in \"%s\" reply %ld.  %lu bytes actually output.\n",
	      (long unsigned) iiop_size(call),
	      method_name(call_method(call)),
	      (long) call_serial_number(call),
	      (long unsigned) iiop_vop(call)));
  }
  _IIOP_EndMessage(t, ilu_FALSE, push, err);
  return ILU_ERROK(*err);
}

/*L1, Main unconstrained*/
/*L2 >= {call's conn's iomu}*/

static ilu_cardinal
  _IIOP_BeginSizingReply (ilu_Call call,
			  ilu_boolean exceptions_possible,
			  ILU_ERRS((IoErrs)) *err)
{
  struct IIOP_DataBlock *db = (struct IIOP_DataBlock *) connection_protocol_data(call_connection(call));

  Initialize(err);
  if (ILU_ERRNOK(*err)) return 0;

  iiop_byte_order(call) = NATIVE_BYTE_ORDER;
  ILU_CLER(*err);
  iiop_size(call) = 0;
  iiop_vop(call) = (ilu_byte *) (24 + (db->charsets_sent ? 0 : (8 + CharSetsServiceContextPaddedLength)));
  return (ilu_cardinal) iiop_vop(call);
}

static ilu_boolean
  _IIOP_BeginReply (ilu_Call call,
		    ilu_boolean exceptions,
		    ilu_cardinal argSize,
		    ILU_ERRS((IoErrs)) *err)
{
  struct IIOP_DataBlock *db = (struct IIOP_DataBlock *) connection_protocol_data(call_connection(call));
  static ilu_byte magic[] = "GIOP";
  ilu_Transport bs = iiop_transport(call);

  ILU_NOTE(IIOP_DEBUG,
	("%s %lu, argSize %lu, exceptions %s.\n",
	 "_IIOP_BeginReply:  SN", call->ca_SN, argSize,
	 exceptions ? "ilu_TRUE" : "ilu_FALSE"));

  Initialize(err);
  if (ILU_ERRNOK(*err)) return ilu_FALSE;

  iiop_vop(call) = 0;
  iiop_size(call) = argSize;

  if (bs->tr_class->tc_boundaried)
    {
      if (transport_begin_message(bs, ilu_FALSE, err)
	  != ilu_rhrc_ok)
	return ilu_FALSE;
    }

  if ((_IIOP_OutputOpaque (call, magic, 4, err), ILU_ERRNOK(*err)) ||
      (_IIOP_OutputByte (call, (ilu_byte)(iiop_major_version(call) & 0xFF), err), ILU_ERRNOK(*err)) ||
      (_IIOP_OutputByte (call, (ilu_byte)(iiop_minor_version(call) & 0xFF), err), ILU_ERRNOK(*err)) ||
      (_IIOP_OutputByte (call,
			 (NATIVE_BYTE_ORDER == LittleEndian),
			 err), ILU_ERRNOK(*err)) ||	/* always use big-endian */
      (_IIOP_OutputByte (call, GIOP_PacketType_Reply, err), ILU_ERRNOK(*err)) ||
      (_IIOP_OutputCardinal (call, argSize - 12, err), ILU_ERRNOK(*err)))
    return ilu_FALSE;
  if (db->charsets_sent) {
    if (_IIOP_OutputCardinal (call, 0, err), ILU_ERRNOK(*err))
      return ilu_FALSE;
  } else {
    if (/* context carries character set and character code set info */
	(_IIOP_OutputCardinal (call, 1, err), ILU_ERRNOK(*err)) ||
	(_IIOP_OutputCardinal (call, IIOP_TAG_CODE_SETS, err), ILU_ERRNOK(*err)) ||
	(_IIOP_OutputBytes (call, CharSetsServiceContext, CharSetsServiceContextLength, 0xFFFF, err),
	 ILU_ERRNOK(*err)))
      return ilu_FALSE;
    db->charsets_sent = ilu_TRUE;
  }
  if (/* serial number */
      (_IIOP_OutputCardinal (call, call->ca_SN, err), ILU_ERRNOK(*err)) ||
      /* reply status */
      (_IIOP_OutputCardinal (call, GIOP_ReplyStatusType_NO_EXCEPTION, err), ILU_ERRNOK(*err)))
    {
      ILU_NOTE(IIOP_DEBUG,
	    ("_IIOP_BeginReply:  err <%s> starting reply %lu (size %lu).\n",
	     ILU_ERR_NAME(*err), call_serial_number(call), argSize));
      return (ilu_FALSE);
    }
  else
    {
      ILU_NOTE(IIOP_DEBUG,
	    ("_IIOP_BeginReply:  started reply %lu (size %lu).\n",
	     call_serial_number(call), argSize));
      return (ilu_TRUE);
    }
}

static ilu_cardinal
  _IIOP_BeginSizingException (ilu_Call call,
			      ilu_cardinal eindex,
			      ilu_ProtocolException sysExnIdx,
			      ILU_ERRS((IoErrs)) * err)
{
  ilu_cardinal packetsize = 0;
  ilu_string ename;

  Initialize(err);
  if (ILU_ERRNOK(*err)) return 0;

  ILU_CLER(*err);
  
  iiop_byte_order(call) = NATIVE_BYTE_ORDER;
  iiop_size(call) = 0;
  iiop_vop(call) = 0;

  ename = FigureNameOfException (call_intro_type(call), call_method(call), eindex, sysExnIdx);
  ILU_NOTE(IIOP_DEBUG,
	("_IIOP_BeginSizingException:  exception string is <%s>\n", ename));
  iiop_objKey(call) = (ilu_bytes) ename;
  iiop_objKeyLen(call) = strlen(ename);
  
  packetsize += (4	/* magic "GIOP" */
		 + 2	/* version */
		 + 1	/* byte order */
		 + 1	/* message type -- exception */
		 + 4	/* message size */
		 + 12 + CharSetsServiceContextPaddedLength	/* service context */
		 + 4	/* serial number */
		 + 4	/* exception kind -- user or system */
		 );

  iiop_vop(call) = (ilu_bytes) packetsize;
  packetsize += _IIOP_SizeOfBytes(call, iiop_objKey(call), iiop_objKeyLen(call) + 1, 0xFFFF, err);
  if (eindex == 0)	/* system exception */
    {
      packetsize += _IIOP_SizeOfCardinal (call, 1, err);	/* for minor code */
      packetsize += _IIOP_SizeOfCardinal (call, 1, err);	/* for completion status */
    }
  return packetsize;
}

static ilu_boolean
  _IIOP_BeginException (ilu_Call call,
			ilu_cardinal evalue,
			ilu_ProtocolException sysExnIndex,
			ilu_cardinal argSize,
			ILU_ERRS((IoErrs)) *err)
{
  static ilu_byte magic[] = "GIOP";
  ilu_string ename;
  ilu_Transport bs = iiop_transport(call);

  Initialize(err);
  if (ILU_ERRNOK(*err)) return ilu_FALSE;

  if (evalue == 0 && ((int) sysExnIndex) > 0 && iiop_objKey(call) == NIL)
    argSize = _IIOP_BeginSizingException (call, evalue, sysExnIndex, err);

  ename = (char *) iiop_objKey(call);
  
  iiop_vop(call) = 0;
  iiop_size(call) = argSize;
  iiop_objKey(call) = NIL;
  iiop_objKeyLen(call) = 0;

  if (bs->tr_class->tc_boundaried)
    {
      if (transport_begin_message(bs, ilu_FALSE, err)
	  != ilu_rhrc_ok)
	return ilu_FALSE;
    }

  if ((_IIOP_OutputOpaque (call, magic, 4, err), ILU_ERRNOK(*err)) ||
      (_IIOP_OutputByte (call, (ilu_byte)(iiop_major_version(call)), err), ILU_ERRNOK(*err)) ||
      (_IIOP_OutputByte (call, (ilu_byte)(iiop_minor_version(call)), err), ILU_ERRNOK(*err)) ||
      (_IIOP_OutputByte (call,
			 (NATIVE_BYTE_ORDER == LittleEndian),
			 err), ILU_ERRNOK(*err)) ||	/* always use big-endian */
      (_IIOP_OutputByte (call, GIOP_PacketType_Reply, err), ILU_ERRNOK(*err)) ||
      (_IIOP_OutputCardinal (call, iiop_size(call) - 12, err), ILU_ERRNOK(*err)) ||
      /* context carries character set and character code set info */
      (_IIOP_OutputCardinal (call, 1, err), ILU_ERRNOK(*err)) ||
      (_IIOP_OutputCardinal (call, IIOP_TAG_CODE_SETS, err), ILU_ERRNOK(*err)) ||
      (_IIOP_OutputBytes (call, CharSetsServiceContext, CharSetsServiceContextLength, 0xFFFF, err),
       ILU_ERRNOK(*err)) ||
      /* serial number */
      (_IIOP_OutputCardinal (call, call->ca_SN, err), ILU_ERRNOK(*err)) ||
      ((evalue == 0) ?
       ((_IIOP_OutputCardinal (call, GIOP_ReplyStatusType_SYSTEM_EXCEPTION, err), ILU_ERRNOK(*err)) ||
	(_IIOP_OutputString (call, (ilu_bytes) ename, strlen(ename), 0, ILU_StringEncoding_latin1, ILU_StringEncoding_latin1, err), ILU_ERRNOK(*err)) ||
	(_IIOP_OutputCardinal (call, sysExnIndex, err), ILU_ERRNOK(*err)) ||
	(_IIOP_OutputCardinal (call, ((ilu_cardinal)2) /* CORBA::COMPLETED_MAYBE */, err), ILU_ERRNOK(*err))) :
       ((_IIOP_OutputCardinal (call, GIOP_ReplyStatusType_USER_EXCEPTION, err), ILU_ERRNOK(*err)) ||
	(_IIOP_OutputString (call, (ilu_bytes) ename, strlen(ename), 0, ILU_StringEncoding_latin1, ILU_StringEncoding_latin1, err), ILU_ERRNOK(*err)))))
    {
      ILU_NOTE(IIOP_DEBUG,
	    ("_IIOP_BeginException:  err <%s> starting reply %lu (size %lu).\n",
	     ILU_ERR_NAME(*err), call_serial_number(call), iiop_size(call)));
      return (ilu_FALSE);
    }
  else
    {
      ILU_NOTE(IIOP_DEBUG,
	    ("_IIOP_BeginException:  started reply %lu (size %lu).\n",
	     call_serial_number(call), iiop_size(call)));
      return (ilu_TRUE);
    }
}

#if defined(ADD_TYPE_REGISTRATION_SUPPORT)
static ilu_TypeKind
  ur_type_kind (ilu_Type t)
{
  if (t->kind == ilu_alias_tk)
    return ur_type_kind(t->desc.Alias.type);
  else
    return t->kind;
}
#endif /* defined(ADD_TYPE_REGISTRATION_SUPPORT) */

ILU_PUBLIC ilu_boolean
  _ilu_IIOP_CanMoveAsBlock(ilu_Call call,
			   ilu_LanguageIndex li,
			   ilu_Type t,
			   ilu_cardinal m,
			   ilu_cardinal *alignmentCode,
			   ilu_Error *err)
{
#if defined(ADD_TYPE_REGISTRATION_SUPPORT)
  ilu_Protocol p = connection_protocol(call_connection(call));
  static ilu_boolean initialized = ilu_FALSE;
  static ilu_LanguageIndex ansi_c_index;

  if (!initialized) {
    ansi_c_index = ilu_RegisterLanguage("ANSI-C");
    initialized = ilu_TRUE;
  };

  if ((p->pr_output_union == _IIOP_OutputUnion) &&
      (li == ansi_c_index)) {
    /* OK, using Sun RPC with ANSI C language mapping */
    ILU_CLER(*err);
    *alignmentCode = 0;

    /* First, check to see that the marshalling stream byte order matches our byte order */
#ifdef WORDS_BIGENDIAN
    if (iiop_byte_order(call) == LittleEndian)
      return ilu_FALSE;
#else
    if (iiop_byte_order(call) == BigEndian)
      return ilu_FALSE;
#endif

    switch (ur_type_kind(t)) {
    case ilu_string_tk:
    case ilu_object_tk:
    case ilu_pipe_tk:
    case ilu_sequence_tk:
    case ilu_pickle_tk:
    case ilu_optional_tk:
#ifdef ILU_REFERENCE_TYPES
    case ilu_reference_tk:
#endif /* def ILU_REFERENCE_TYPES */
    case ilu_alias_tk:
    case ilu_boolean_tk:
      /* can't do it with these types */
      return ilu_FALSE;

    case ilu_byte_tk:
    case ilu_shortcharacter_tk:
      *alignmentCode = 1;
      return ilu_TRUE;

    case ilu_character_tk:
    case ilu_shortinteger_tk:
    case ilu_shortcardinal_tk:
      *alignmentCode = 2;
      return ilu_TRUE;

    case ilu_integer_tk:
    case ilu_cardinal_tk:
    case ilu_shortreal_tk:
    case ilu_enumeration_tk:
      *alignmentCode = 4;
      return ilu_TRUE;

    case ilu_longinteger_tk:
    case ilu_longcardinal_tk:
    case ilu_real_tk:
    case ilu_longreal_tk:
      *alignmentCode = 8;
      return ilu_TRUE;

    case ilu_union_tk:
      /* too hard to think about, just say no */
      return ilu_FALSE;

    case ilu_array_tk:
      return _ilu_IIOP_CanMoveAsBlock (call, li, t->desc.Array.type, m, alignmentCode, err);

    case ilu_record_tk:
      {
	ilu_cardinal i;
	ilu_Type t2;
	ilu_cardinal lalign;
	t2 = t->desc.Record.fields[0].base.type;
	if (!_ilu_IIOP_CanMoveAsBlock (call, li, t2, 1, alignmentCode, err))
	    return ilu_FALSE;
	for (i = 1;  i < t->desc.Record.n_fields;  i++) {
	  t2 = t->desc.Record.fields[i].base.type;
	  switch (t2->kind) {
	  case ilu_longcardinal_tk:
	  case ilu_longinteger_tk:
	  case ilu_real_tk:
	  case ilu_enumeration_tk:
	  case ilu_integer_tk:
	  case ilu_cardinal_tk:
	  case ilu_shortreal_tk:
	  case ilu_boolean_tk:
	  case ilu_byte_tk:
	  case ilu_shortcharacter_tk:
	  case ilu_character_tk:
	  case ilu_shortcardinal_tk:
	  case ilu_shortinteger_tk:
	    break;
	  case ilu_array_tk:
	  case ilu_record_tk:
	    if (!_ilu_IIOP_CanMoveAsBlock (call, li, t2, 1, &lalign, err))
	      return ilu_FALSE;
	  default:
	    return ilu_FALSE;
	  }
	}
      }
      return ilu_TRUE;
      
    default:
      return ilu_FALSE;      
    }
  };
#endif /* defined(ADD_TYPE_REGISTRATION_SUPPORT) */
  ILU_CLER(*err);
  return ilu_FALSE;
}
			     
ILU_PUBLIC ilu_cardinal
  _ilu_IIOP_AlignStream (ilu_Call call,
			 ilu_cardinal alignmentCode,
			 ilu_Error *err)
{
  ilu_cardinal size;

  size = PADDING_NEC(iiop_vop(call), alignmentCode);
  PACKET_INCR(iiop_packet(call), size, 1);
  ILU_CLER(*err);
  return size;
}

/*L2, Main unconstrained*/
/*L1 >= {prmu}*/

static ilu_Protocol _IIOP_NewIIOP (ilu_boolean concurrent)
{
  ilu_Protocol new = (ilu_Protocol) ilu_must_malloc(sizeof(struct _ilu_Protocol_s));

  new->pr_concurrent_requests = concurrent;
  new->pr_sizing_required = ilu_TRUE;
  new->pr_needs_boundaried_transport = ilu_FALSE;
  new->pr_needs_reliable_transport = ilu_TRUE;

  new->pr_init_call = _IIOP_InitCall;
  new->pr_start_request = _IIOP_StartRequest;
  new->pr_finish_request = _IIOP_FinishRequest;
  new->pr_begin_sizing_reply = _IIOP_BeginSizingReply;
  new->pr_begin_reply = _IIOP_BeginReply;
  new->pr_finish_reply = _IIOP_FinishReply;
  new->pr_begin_sizing_exn = _IIOP_BeginSizingException;
  new->pr_begin_exception = _IIOP_BeginException;
  new->pr_finish_exception = _IIOP_FinishException;
  new->pr_finish_call = _IIOP_FinishCall;
  new->pr_prefinish_call = NULLFN;

  new->pr_read_header = _IIOP_ReadHeader;
  new->pr_delay_interp = _IIOP_DelayInterp;
  new->pr_resume_interp = _IIOP_ResumeInterp;
  new->pr_abandon_delayed_interp = _IIOP_AbandonDelayedInterp;
  new->pr_discard_input = _IIOP_DiscardInput;
  new->pr_discard_output = _IIOP_DiscardOutput;
  
  new->pr_interpret_request = _IIOP_InterpretRequest;
  new->pr_request_read = _IIOP_RequestRead;
  new->pr_interpret_reply = _IIOP_InterpretReply;
  new->pr_reply_read = _IIOP_ReplyRead;

  new->pr_output_integer = _IIOP_OutputInteger;
  new->pr_input_integer = _IIOP_InputInteger;
  new->pr_size_of_integer = _IIOP_SizeOfInteger;

  new->pr_output_shortinteger = _IIOP_OutputShortInteger;
  new->pr_input_shortinteger = _IIOP_InputShortInteger;
  new->pr_size_of_shortinteger = _IIOP_SizeOfShortInteger;

  new->pr_output_longinteger = _IIOP_OutputLongInteger;
  new->pr_input_longinteger = _IIOP_InputLongInteger;
  new->pr_size_of_longinteger = _IIOP_SizeOfLongInteger;

  new->pr_output_cardinal = _IIOP_OutputCardinal;
  new->pr_input_cardinal = _IIOP_InputCardinal;
  new->pr_size_of_cardinal = _IIOP_SizeOfCardinal;

  new->pr_output_shortcardinal = _IIOP_OutputShortCardinal;
  new->pr_input_shortcardinal = _IIOP_InputShortCardinal;
  new->pr_size_of_shortcardinal = _IIOP_SizeOfShortCardinal;

  new->pr_output_longcardinal = _IIOP_OutputLongCardinal;
  new->pr_input_longcardinal = _IIOP_InputLongCardinal;
  new->pr_size_of_longcardinal = _IIOP_SizeOfLongCardinal;

  new->pr_output_real = _IIOP_OutputReal;
  new->pr_input_real = _IIOP_InputReal;
  new->pr_size_of_real = _IIOP_SizeOfReal;

  new->pr_output_shortreal = _IIOP_OutputShortReal;
  new->pr_input_shortreal = _IIOP_InputShortReal;
  new->pr_size_of_shortreal = _IIOP_SizeOfShortReal;

  new->pr_output_longreal = _IIOP_OutputLongReal;
  new->pr_input_longreal = _IIOP_InputLongReal;
  new->pr_size_of_longreal = _IIOP_SizeOfLongReal;

  new->pr_output_optional = _IIOP_OutputOptional;
  new->pr_input_optional = _IIOP_InputOptional;
  new->pr_size_of_optional = _IIOP_SizeOfOptional;

  new->pr_output_enum_code = _IIOP_OutputEnumeration;
  new->pr_input_enum_code = _IIOP_InputEnumeration;
  new->pr_size_of_enum_code = _IIOP_SizeOfEnumeration;

  new->pr_output_byte = _IIOP_OutputByte;
  new->pr_input_byte = _IIOP_InputByte;
  new->pr_size_of_byte = _IIOP_SizeOfByte;

  new->pr_output_character = _IIOP_OutputShortCardinal;
  new->pr_input_character = _IIOP_InputCharacter;
  new->pr_size_of_character = _IIOP_SizeOfShortCardinal;

  new->pr_output_boolean = _IIOP_OutputBoolean;
  new->pr_input_boolean = _IIOP_InputBoolean;
  new->pr_size_of_boolean = _IIOP_SizeOfBoolean;

  new->pr_output_shortchar = _IIOP_OutputShortChar;
  new->pr_input_shortchar = _IIOP_InputShortChar;
  new->pr_size_of_shortchar = _IIOP_SizeOfShortChar;

  new->pr_output_string = _IIOP_OutputString;
  new->pr_input_string = _IIOP_InputString;
  new->pr_size_of_string = _IIOP_SizeOfString;

  new->pr_output_wstring = _ilu_OutputWString;
  new->pr_input_wstring = _IIOP_InputWString;
  new->pr_size_of_wstring = _ilu_SizeOfWString;

  new->pr_output_bytes = _IIOP_OutputBytes;
  new->pr_input_bytes = _IIOP_InputBytes;
  new->pr_size_of_bytes = _IIOP_SizeOfBytes;

  new->pr_output_opaque = _IIOP_OutputOpaque;
  new->pr_input_opaque = _IIOP_InputOpaque;
  new->pr_size_of_opaque = _IIOP_SizeOfOpaque;

  new->pr_output_object_id = _IIOP_OutputObjectID;
  new->pr_input_object_id = _IIOP_InputObjectID;
  new->pr_size_of_object_id = _IIOP_SizeOfObjectID;

  new->pr_output_stringvec = (void (*)(ilu_Call, ilu_string, ilu_cardinal, ilu_Error *)) _IIOP_OutputOpaque;
  new->pr_input_stringvec = (void (*)(ilu_Call, ilu_string *, ilu_cardinal, ilu_Error *)) _IIOP_InputOpaque;
  new->pr_size_of_stringvec = (ilu_cardinal (*)(ilu_Call, ilu_string, ilu_cardinal, ilu_Error *)) _IIOP_SizeOfOpaque;

  new->pr_output_wstringvec = _ilu_OutputWStringVec;
  new->pr_input_wstringvec = _ilu_InputWStringVec;
  new->pr_size_of_wstringvec = _ilu_SizeOfWStringVec;

  new->pr_output_sequence = _IIOP_OutputSequence;
  new->pr_output_sequence_mark = _IIOP_OutputSequenceMark;
  new->pr_input_sequence = _IIOP_InputSequence;
  new->pr_input_sequence_mark = _IIOP_InputSequenceMark;
  new->pr_end_sequence = _IIOP_EndSequence;
  new->pr_size_of_sequence = _IIOP_SizeOfSequence;

  new->pr_output_record = _IIOP_OutputRecord;
  new->pr_input_record = _IIOP_InputRecord;
  new->pr_end_record = _IIOP_EndRecord;
  new->pr_size_of_record = _IIOP_SizeOfRecord;

  new->pr_output_array = _IIOP_OutputArray;
  new->pr_input_array = _IIOP_InputArray;
  new->pr_end_array = _IIOP_EndArray;
  new->pr_size_of_array = _IIOP_SizeOfArray;

  new->pr_output_union = _IIOP_OutputUnion;
  new->pr_input_union = _IIOP_InputUnion;
  new->pr_end_union = _IIOP_EndUnion;
  new->pr_size_of_union = _IIOP_SizeOfUnion;

#ifdef ADD_VARIANT_SUPPORT

  new->pr_output_pickle = _IIOP_OutputPickle;
  new->pr_input_pickle = _IIOP_InputPickle;
  new->pr_size_of_pickle = _IIOP_SizeOfPickle;

#endif /* ADD_VARIANT_SUPPORT */

#ifdef ILU_FIXED_POINT_SUPPORT

  new->pr_output_fixedpoint = _ilu_OutputFixedpoint;
  new->pr_input_fixedpoint = _ilu_InputFixedpoint;
  new->pr_size_of_fixedpoint = _ilu_SizeOfFixedpoint;

#endif /* ILU_FIXED_POINT_SUPPORT */

#ifdef ILU_REFERENCE_TYPES

  new->pr_size_of_reference = _ilu_SizeOfReference;
  new->pr_output_reference = _ilu_OutputReference;
  new->pr_input_reference = _ilu_InputReference;
  new->pr_end_input_reference = _ilu_EndInputReference;

#endif /* def ILU_REFERENCE_TYPES */

  new->pr_form_handle = (concurrent ? _IIOP_FormConcurrentProtocolHandle : _IIOP_FormSerialProtocolHandle);

  new->pr_create_data_block = _IIOP_CreateDataBlock;
  new->pr_free_data_block = (void (*)(void *)) _IIOP_FreeDataBlock;
  new->pr_conn_closing = NULLFN;

  return (new);
}

/*L1_sup < prmu*/

ilu_Protocol _ilu_IIOP_Protocol(ilu_ProtocolInfo pinfo, ilu_Error *err)
{
  /*L1 >= {prmu}*/
  static ilu_Protocol StandardIIOP = NULL;

#ifdef ADD_VARIANT_SUPPORT
  ilu_boolean new;

  if (_IIOP_DefaultPickleType == NIL) {
    ilu_AcquireMutex(ilu_otmu);
    _IIOP_DefaultPickleType =
      ilu_RegisterSequenceType ("__UnknownType",
				"__IIOP",
				NIL,
				"ilu:--standard-pickle-type",
				(char *) &ilu_TypeID_ilu_byte[0], 0,
				&new, err);
    ilu_ReleaseMutex(ilu_otmu);
    if (ILU_ERRNOK(*err)) {
      ilu_DebugPrintf("_ilu_IIOP_Protocol:  Can't register standard pickle type!\n");
      _IIOP_DefaultPickleType = NIL;
      return NIL;
    }
  }
#endif /* ADD_VARIANT_SUPPORT */
			      
  _ilu_AcquireMutex(ilu_prmu);

  Initialize(err);
  if (ILU_ERRNOK(*err)) return NIL;

  if (StandardIIOP == NULL)
    StandardIIOP = _IIOP_NewIIOP(ilu_TRUE);
  _ilu_ReleaseMutex(ilu_prmu);
  ILU_CLER(*err);
  return (StandardIIOP);
}

ilu_Protocol _ilu_IIOP_SerialProtocol(ilu_ProtocolInfo pinfo, ilu_Error *err)
{
  /*L1 >= {prmu}*/
  static ilu_Protocol StandardIIOP = NULL;

#ifdef ADD_VARIANT_SUPPORT
  ilu_boolean new;

  if (_IIOP_DefaultPickleType == NIL) {
    ilu_AcquireMutex(ilu_otmu);
    _IIOP_DefaultPickleType =
      ilu_RegisterSequenceType ("__UnknownType",
				"__IIOP",
				NIL,
				"ilu:--standard-pickle-type",
				(char *) &ilu_TypeID_ilu_byte[0], 0,
				&new, err);
    ilu_ReleaseMutex(ilu_otmu);
    if (ILU_ERRNOK(*err)) {
      ilu_DebugPrintf("_ilu_IIOP_Protocol:  Can't register standard pickle type!\n");
      _IIOP_DefaultPickleType = NIL;
      return NIL;
    }
  }
#endif /* ADD_VARIANT_SUPPORT */
			      
  _ilu_AcquireMutex(ilu_prmu);

  Initialize(err);
  if (ILU_ERRNOK(*err)) return NIL;

  if (StandardIIOP == NULL)
    StandardIIOP = _IIOP_NewIIOP(ilu_FALSE);
  _ilu_ReleaseMutex(ilu_prmu);
  ILU_CLER(*err);
  return (StandardIIOP);
}


/*======================================================================*/
/*======================================================================*/
/*======================================================================*/
/*======================================================================*/

/*
 * The remainder of this file has a different copyright:
 *
 * Copyright (C) DSTC Pty Ltd (ACN 052 372 577) 1997.
 *
 * Any party obtaining a copy of this file from DSTC Pty Ltd, directly
 * or indirectly, is granted, free of charge, a full and unrestricted
 * irrevocable, world-wide, paid up, royalty-free, nonexclusive right and
 * license to deal in this software (the "Software"), including without
 * limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons
 * who receive copies from any such party to do so, with the only
 * requirement being that all copyright notices remain intact.
 *
 * This software is being provided "AS IS" without warranty of any kind,
 * express, implied or otherwise, including without limitation, any
 * warranty of merchantability or fitness for a particular purpose.
 *
 * In no event shall DSTC Pty Ltd be liable for any special, incidental,
 * indirect or consequential damages of any kind, or any damages whatsoever
 * resulting from loss of use, data or profits, whether or not advised of
 * the possibility of damage, and on any theory of liability, arising out
 * of or in connection with the use or performance of this software.
 *
 * DSTC Pty Ltd welcomes comments and bug fixes related to this software.
 * Please address any queries, comments or bug fixes (please include
 * the name and version of the software you are using and details
 * of your operating system version) to the address below:
 *
 *       DSTC Pty Ltd
 *       Level 7, Gehrmann Labs
 *       University of Queensland
 *       St Lucia, 4072
 *       Tel: +61 7 365 4310
 *       Fax: +61 7 365 4311
 *       Email: Enquiries@dstc.edu.au
 */

/*---------------------------------------------------------------------------*/

/*
 * Revision history:
 *
 * 15 Aug 97: Changed init_codevals() to use a loop for initialization
 *            instead of in-line code. Thanks to Rich Salz for suggesting
 *	      this.
 *
 * 20 Oct 97: Changed prefix to add a colon separator. Thanks to Steve
 *	      Vinoski for pointing out that this was necessary for
 *	      use of IORs in web browsers.
 *
 *	      Added example for little-endian nil IOR. Thanks to Martin
 *	      Chilvers for pointing out that there are two representations
 *	      of nil IORs.
 */

/*---------------------------------------------------------------------------*/

/*
 * cdr_to_ior2(),
 * ior2_to_cdr()	- convert from CDR to IOR2 format and vice versa
 *
 * cdr_size()		- return the CDR size of an IOR (both old and new)
 *
 * IOR2 encoded strings are typically 40% - 45% shorter than the old IOR
 * format, depending on the specific IOR (compression improves for
 * longer IORs).
 *
 * Big-endian nil IORs are compressed by 61%, from
 * "IOR:00000000000000010000000000000000" to "IOR2:000g=74=8".
 *
 * Little-endian nil IORs are compressed by 56%, from
 * "IOR:01000000010000000000000000000000" to "IOR2:000g0g=14=c".
 *
 * An IOR in IOR2 format is a well-behaved string - it contains only digits,
 * lower and upper case letters, and the letters ':', '-', '+' and '='.
 * This means it can easily be used on the command line, as a database
 * field value, or for line-oriented file I/O (the stringified IOR does
 * not contain embedded control characters, white space, or newline characters).
 * IOR2 references can also be represented as an ASN.1 printable string.
 *
 * It is possible to compress IORs further, by using a Lempel-Ziv or
 * similar encoding, but this sacrifices the printable nature of the string
 * (binary values need to be used to get the last few percent of compression).
 * The algorithm used here is a compromise - it leaves around 20% redundancy
 * in the compressed IOR, but keeps the resultant string printable.
 *
 * The code below makes no assumptions about codesets, so it works for both
 * ASCII and EBCDIC. There are also no assumptions about character
 * ordering (or groups of characters occupying adjacent code positions),
 * so this source can be used with any single-byte code set.
 * To remain portable, the code table is initialized at run-time.
 *
 * The code compiles cleanly with both ANSI-C and C++ compilers.
 * It works with both signed and unsigned char implementations and
 * makes no assumptions about byte ordering. 2's complement representation
 * is *not* assumed. The code will work on 16 bit machines and is suitable
 * for use in free-standing implementations (it does not require libc or
 * other libraries).
 *
 * The code is thread-safe and reentrant, except for the run-time
 * initialization of the decode array. To get thread safety for that,
 * the 'once' guard variable and the call to the initialization function
 * (init_codevals) need to be protected by a mutex.
 *
 * Performance of the conversions between CDR and IOR2 (and back) should not
 * be a concern. The code below sacrifices a lot of efficiency to remain
 * portable. Even so, on an HP715/100 workstation, the code achieves:
 *
 * CDR length | Direction   | Conversions/sec
 * ------------------------------------------
 *    308     | cdr_to_ior2 |     6500
 *    308     | ior2_to_cdr |     8500
 *    212     | cdr_to_ior2 |     9500
 *    212     | ior2_to_cdr |	 12500
 *
 * So even for a rather long IOR with 308 bytes of CDR, conversion speed
 * won't be an issue.
 *
 * Not surprisingly, the cost of conversion increases linearly with the
 * size of the reference.
 *
 * The algorithm chosen for this implementation minimizes code size
 * (the PA-RISC object code size is 2.5 kB for code and data).
 */

/*---------------------------------------------------------------------------*/
/*
 * decode is an array of code values. It maps a character (the index)
 * to a value. The array is initialized at run-time to make sure
 * the code works with both ASCII and EBCDIC (or other codes).
 *
 * The mapping for the decode array is
 *
 *	'0' -> 0
 *	...
 *	'9' -> 9
 *	'a' -> 10
 *	...
 *	'z' -> 35
 *	'A' -> 36
 *	...
 *	'Z' -> 61
 *	'-' -> 62
 *	'+' -> 63
 *
 * The array is sparse and initialized only for the relevant
 * lookup positions (the unsigned byte value is used directly as an index).
 * Again, this gives codeset independence (and improves speed a little).
 */
static int _IIOP_ior2_decode[256];

/*
 * Guard to do lazy initialization of decode array.
 */
static int _IIOP_ior2_once = 0;

/*
 * Inverse mapping of decode array. Maps a value to a character.
 */
static const char _IIOP_ior2_code[] = "0123456789"
			   "abcdefghijklmnopqrstuvwxyz"
			   "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
			   "-+";

/*
 * ZERO_CODE ('=') is a special marker for run-length encoding of
 * consecutive zeros. It must not occur in the codeval or code arrays.
 *
 * Runs of zeros are encoded as:
 *
 *	Number of zeros		Code
 *	----------------------------
 *		 3		=0
 *		 4		=1
 *		...		...
 *		12		=9
 *		13		=a
 *		14		=b
 *		...		...
 *		38		=z
 *		39		=A
 *		40		=B
 *		...		...
 *		64		=Z
 *		65		=-
 *		66		=+
 *
 * In other words, the number of zeros in a run is encoded by the byte
 * following the '=' character.
 *
 * For encoding, that byte value is
 *
 *	_IIOP_ior2_code[zeros - 3]
 *
 * For decoding, the run length is reconstructed by using the value
 * of the byte following the '=' marker as an index into the decode
 * array and adding 3:
 *
 *	_IIOP_ior2_decode[byte] + 3
 *		
 * Runs of more than 66 zeros are encoded by two consecutive run-length
 * encodings. For example:
 *
 *	(70 zeros) -> =+=1
 *	
 * After each run, at least three zeros must be left for another run,
 * otherwise the normal encoding is used. For example:
 *
 *	(68 zeros) -> =+00
 */
static const char _IIOP_ior2_ZERO_CODE = '=';

/*
 * Some powers of two. The code uses div (/) and mod (%) operations to
 * set and clear bits (to avoid assuming 2's complement representation).
 * Making constants here keeps the code readable. We use the preprocessor
 * (instead of real constants) because real constants can slow the code
 * down by more than a factor of two with some compilers.
 */
#define	P2_18	262144		/* 2^18, 256 kB */
#define	P2_12	4096		/* 2^12, 4 kB */
#define	P2_6	64		/* 2^6,  64 bytes */
#define	P2_4	16		/* 2^4,  16 bytes */
#define	P2_2	4		/* 2^2,  4 bytes */

/*---------------------------------------------------------------------------*/

/*
 * Initialize the decode array with the numeric codes for each character.
 * We do this at run time to make it work for both ASCII and EBCDIC.
 */

static void
  _IIOP_ior2_init_codevals(void)
{
	int i;

	for (i = 0; i < sizeof(_IIOP_ior2_code) - 1; i++)
		_IIOP_ior2_decode[(unsigned)_IIOP_ior2_code[i]] = i;
}

/*---------------------------------------------------------------------------*/

/*
 * Convert 'cdrsize' bytes in the 'cdr' buffer into IOR2 format in 'ior2'.
 * 'ior2size' must be set to the size of the 'ior2' buffer.
 *
 * Return value:
 *
 *	- A non-zero return value indicates the actual number of bytes
 *	  written into 'ior2' (the count includes the terminating NUL byte).
 *
 *	- A zero return value indicates that the length of the 'ior2'
 *	  buffer as specified by 'ior2size' was insufficient to hold
 *	  the encoded IOR.
 */

static ilu_cardinal
  _IIOP_cdr_to_ior2(const ilu_bytes cdr, ilu_cardinal cdrsize, ilu_string ior2, ilu_cardinal ior2size)
{
	ilu_cardinal			val;		/* Temporary */
	unsigned		pos;		/* State variable */
	unsigned		zeros;		/* Num zeros blocks in run */
	ilu_cardinal			ior2len;	/* Length of ior2 */
	const unsigned char	*cdrp;		/* First byte of CDR */
	ilu_cardinal			cdrlen;		/* Length of cdr */
	int			last_iter;	/* True for last byte of CDR */

	/*
	 * Need 9 bytes for prefix and length field.
	 */
	if (ior2size < 9)
		return 0;

	/*
	 * Set prefix.
	 */
	*ior2++ = 'I';
	*ior2++ = 'O';
	*ior2++ = 'R';
	*ior2++ = '2';
	*ior2++ = ':';

	/*
	 * Encode 4-byte CDR length.
	 */
	val = cdrsize;
	*ior2++ = _IIOP_ior2_code[val / P2_18];
	val %= P2_18;
	*ior2++ = _IIOP_ior2_code[val / P2_12];
	val %= P2_12;
	*ior2++ = _IIOP_ior2_code[val / P2_6];
	*ior2++ = _IIOP_ior2_code[val % P2_6];

	ior2len = 9;	/* Five bytes for prefix, plus four bytes for length */

	/*
	 * Repeatedly peel off the next six CDR bits.
	 */
	cdrp = (const unsigned char *)cdr;
	cdrlen = 0;
	zeros = 0;
	pos = 0;
	while (cdrlen < cdrsize) {

		/*
		 * Work out which bits to mask out in this iteration.
		 */
		last_iter = (cdrlen == cdrsize - 1 && pos != 0);
		switch (pos) {
		case 0:
			/*
			 * High-order 6 bits in current byte.
			 */
			val = cdrp[cdrlen] / P2_2;
			break;
		case 1:
			/*
			 * Low-order 2 bits in current byte
			 * and high-order 4 bits in next byte.
			 */
			val = cdrp[cdrlen] % P2_2 * P2_4;
			if (!last_iter)
				val += cdrp[cdrlen + 1] / P2_4;
			cdrlen++;
			break;
		case 2:
			/*
			 * Low-order 4 bits in current byte
			 * and high-order 2 bits in next byte.
			 */
			val = cdrp[cdrlen] % P2_4 * P2_2;
			if (!last_iter)
				val += cdrp[cdrlen + 1] / P2_6;
			cdrlen++;
			break;
		case 3:
			/*
			 * Low-order 6 bits in current byte.
			 */
			val = cdrp[cdrlen] % P2_6;
			cdrlen++;
			break;
		}
		pos = (pos + 1) % 4;

		if (val == 0) {
			/*
			 * val == 0 if a zero block was just peeled off.
			 * If so, increment count of zeros (or output
			 * a run if we have reached the limit of 66).
			 */
			if (zeros == 66) {
				if ((ior2len += 2) > ior2size)
					return 0;
				*ior2++ = _IIOP_ior2_ZERO_CODE;
				*ior2++ = _IIOP_ior2_code[zeros - 3];
				zeros = 1;
			} else {
				zeros++;
			}
		}
		if (val != 0 || last_iter) {
			/*
			 * We just peeled off a non-zero block, or we
			 * had a zero and this is the last iteration.
			 * Output any zeros found up to this point.
			 * If this is not the last iteration, output
			 * the code for the non-zero block.
			 */
			switch (zeros) {
			case 2:
				if (++ior2len > ior2size)
					return 0;
				*ior2++ = _IIOP_ior2_code[0];
				/* FALLTHROUGH */
			case 1:
				if (++ior2len > ior2size)
					return 0;
				*ior2++ = _IIOP_ior2_code[0];
				/* FALLTHROUGH */
			case 0:
				if (val != 0) {
					if (++ior2len > ior2size)
						return 0;
					*ior2++ = _IIOP_ior2_code[val];
				}
				break;
			default:
				if ((ior2len += 2) > ior2size)
					return 0;
				*ior2++ = _IIOP_ior2_ZERO_CODE;
				*ior2++ = _IIOP_ior2_code[zeros - 3];
				if (val != 0) {
					if (++ior2len > ior2size)
						return 0;
					*ior2++ = _IIOP_ior2_code[val];
				}
				break;
			}
			zeros = 0;
		}
	}

	/*
	 * Write terminating NUL.
	 */
	if (++ior2len > ior2size)
		return 0;
	*ior2 = '\0';

	return ior2len;
}

/*---------------------------------------------------------------------------*/

/*
 * Convert the reference in 'ior2' into CDR format. The 'cdr' buffer
 * receives the result. 'cdrsize' must be set to the length of the
 * 'cdr' buffer.
 *
 * Return value:
 *
 *	- A non-zero return value indicates the actual number of bytes
 *	  written into 'cdr'.
 *
 *	- A zero return value indicates failure:
 *
 *		- either the input IOR did not have an "IOR2" prefix, or
 *
 *		- the ior had a zero length field, or
 *
 *		- 'cdr' was too short to receive the decoded IOR.
 *
 * If the input IOR has a correct "IOR2:" prefix and non-zero length field,
 * it is otherwise assumed to be well-formed. Passing a malformed IOR with
 * an invalid length or illegal characters causes undefined behavior.
 */

static ilu_cardinal
  _IIOP_ior2_to_cdr(ilu_string ior2, ilu_bytes cdr, ilu_cardinal cdrsize)
{
	unsigned char	*cdrp;		/* Next byte in CDR */
	unsigned	pos;		/* State variable */
	unsigned	zeros;		/* Num six-bit zeros blocks in run */
	unsigned	val;		/* Decoded value of current IOR2 byte */
	ilu_cardinal		cdrlen;		/* Num CDR bytes written */
	unsigned	ior2_byte;	/* Current byte of ior2 */
	int		last_iter;	/* True for final 6-bit block */
	
	/*
	 * Initialize code value array (if not done yet).
	 */
	if (!_IIOP_ior2_once) {
		_IIOP_ior2_init_codevals();
		_IIOP_ior2_once = 1;
	}

	/*
	 * Test prefix.
	 */
	if (
	       *ior2++ != 'I'
	    || *ior2++ != 'O'
	    || *ior2++ != 'R'
	    || *ior2++ != '2'
	    || *ior2++ != ':') {
		return 0;
	}

	/*
	 * Test CDR length for sanity.
	 */
	if (cdrsize == 0)
		return 0;

	/*
	 * Decode 4-byte CDR length.
	 */
	cdrlen = _IIOP_ior2_decode[(unsigned)*ior2++] * P2_18;
	cdrlen += _IIOP_ior2_decode[(unsigned)*ior2++] * P2_12;
	cdrlen += _IIOP_ior2_decode[(unsigned)*ior2++] * P2_6;
	cdrlen += _IIOP_ior2_decode[(unsigned)*ior2++];
	if (cdrlen == 0)
		return 0;

	/*
	 * Make sure the output buffer is long enough to hold the result.
	 */
	if (cdrsize < cdrlen)
		return 0;

	/*
	 * Iterate over the IOR2 bytes, decoding each byte into
	 * six bits of CDR.
	 */
	cdrp = (unsigned char *)cdr;
	pos = 0;
	zeros = 0;
	while (zeros || (ior2_byte = *ior2) != '\0') {
		if (zeros) {
			/*
			 * val == 0 from previous iteration, do
			 * another zero.
			 */
			zeros--;
		} else if (ior2_byte == (unsigned)_IIOP_ior2_ZERO_CODE) {
			/*
			 * Deal with zero run.
			 */
			ior2++;				/* Skip '=' marker */
			ior2_byte = *ior2++;		/* Get run length */
			zeros = _IIOP_ior2_decode[ior2_byte] + 2;	/* Count of zeros */
			val = _IIOP_ior2_decode[0];		/* val = 0 */
		} else {
			/*
			 * Normal decoding.
			 */
			val = _IIOP_ior2_decode[ior2_byte];
			ior2++;
		}

		/*
		 * Use value of current byte, and copy
		 * bottom six bits of the value
		 * into CDR at the current output position.
		 */
		last_iter = (zeros == 0 && *ior2 == '\0');
		switch (pos) {
		case 0:
			*cdrp = val * P2_2;
			break;
		case 1:
			*cdrp++ += val / P2_4;
			if (!last_iter)
				*cdrp = val % P2_4 * P2_4;
			break;
		case 2:
			*cdrp++ += val / P2_2; 
			if (!last_iter)
				*cdrp = val % P2_2 * P2_6;
			break;
		case 3:
			*cdrp++ += val;
			break;
		}
		pos = (pos + 1) % 4;
	}

	return cdrlen;
}

/*---------------------------------------------------------------------------*/

/*
 * Return the number of CDR bytes required to hold a decoded IOR.
 */

static ilu_cardinal
  _IIOP_cdr_size(ilu_string ior)
{
	ilu_cardinal	len;

	/*
	 * Check prefix.
	 */
	if (*ior++ != 'I' || *ior++ != 'O' || *ior++ != 'R')
		return 0;
	
	/*
	 * Initialize code value array (if not done yet).
	 */
	if (!_IIOP_ior2_once) {
		_IIOP_ior2_init_codevals();
		_IIOP_ior2_once = 1;
	}

	/*
	 * Determine IOR version.
	 */
	if (*ior == '2' && *(ior + 1) == ':') {
		/*
		 * New IOR, decode length.
		 */
		ior += 2;
		len = _IIOP_ior2_decode[(unsigned)*ior++] * P2_18;
		len += _IIOP_ior2_decode[(unsigned)*ior++] * P2_12;
		len += _IIOP_ior2_decode[(unsigned)*ior++] * P2_6;
		len += _IIOP_ior2_decode[(unsigned)*ior];
	} else if (*ior == ':') {
		/*
		 * Old IOR, count number of digits. If a free-standing
		 * implementation is not required, calling strlen() here
		 * will probably be faster.
		 */
		++ior;
		len = 0;
		while (*ior++ != '\0')
			len++;
		len /= 2;
	} else {
		/*
		 * Not an "IOR:" or "IOR2:" prefix.
		 */
		len = 0;
	}

	return len;
}
