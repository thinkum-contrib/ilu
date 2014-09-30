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
/* $Id: courier.c,v 1.148 1999/09/13 23:30:10 janssen Exp $ */
/* Last edited by Mike Spreitzer July 21, 1998 3:10 pm PDT */

#ifdef MACOS
#pragma segment ilu
#endif

#include "iluntrnl.h"
#include "iluprotocol.h"
#include "ilutransport.h"

#include "call.h"
#include "connect.h"
#include "port.h"
#include "object.h"
#include "type.h"
#include "method.h"
#include "mooring.h"

#include <sys/types.h>

#include <fcntl.h>

#include <stdio.h>

#if (defined(WIN32) && defined(_WINIO))
#include <winiodef.h>
#endif /* (defined(WIN32) && defined(_WINIO)) */


#define ILU_COURIER_PROGRAM_NUMBER	0x00110000


#define MEMCPY(to,from,len)	memcpy((void *)(to),(void *)(from),(len))


/*L1, L2, Main unconstrained*/

#define ODD(x)		(((x)&0x1)!=0)
#define EVEN(x)		(((x)&0x1)==0)  
#define PADDED_SIZE(x)	((((unsigned) (x))+1) & (~0x1))
  
#ifdef WORDS_BIGENDIAN
#define THIS_ARCH_IS_BIGENDIAN	ilu_TRUE
#else
#define THIS_ARCH_IS_BIGENDIAN	ilu_FALSE
#endif

#define callport(call)	((call)->ca_prTrans)
/*
 * callport(call)'s xmu == call's conn's iomu;
 * callport(call)'s ymu == (call->ca_prbit1 ? conn's iomu :
 * conn's waitmu).
 */

typedef struct {
  ilu_Class cri_class;
  ilu_string cri_type_id;
  ilu_cardinal cri_pnumber, cri_version;
} courierinfo;

static ilu_cardinal CourierMaxStringSize = 0xFFFFFFFF;

static          ilu_boolean
_courier_InitCall(ilu_Call call, ILU_ERRS((IoErrs)) * err)
{
  if (!transport_boundaried(connection_transport(call->ca_connection)))
    return ILU_ERR_CONS1(comm_failure, err, minor, ilu_cfm_tr_non_boundaried, ilu_FALSE);
  call->ca_prTrans = connection_transport(call->ca_connection);
  call->ca_prbit1 = ilu_FALSE;
  return ILU_CLER(*err);
}

static          ilu_boolean
_courier_FinishCall(ilu_Call call, ILU_ERRS((IoErrs)) * err)
{
  ILU_CLER(*err);
  if (call->ca_prbit1) {
    ilu_integer     cdfd;
    ilu_CloseTransport(call->ca_prTrans, &cdfd, err);
    ILU_ERR_SWITCH(*err) {
      ILU_ERR_CASE(bad_locks, x) {
	ILU_HANDLED(*err);
	(void) ilu_Check(ilu_FALSE, err);
      }
      ILU_ERR_ELSE /* do nothing */;
    } ILU_ERR_ENDSWITCH;
    _ilu_Assert(cdfd == 0, "courier FinishCall");
  }
  return ILU_ERROK(*err);
}

static ilu_refany _courier_CreateDataBlock (ilu_ProtocolInfo pinfo,
					    ILU_ERRS((no_memory)) *err)
{
  return (ILU_CLER(*err), NIL);
}

static void _courier_FreeDataBlock (void *d)
{
  return;
}

/**********************************************************************
  Code to figure Courier program # given ILU type code
***********************************************************************/

/*L1 >= {prmu}*/

static HashTable ProgramNumberHashTable = NIL;
/* program number & version -> courierinfo* */

static HashTable RegistryHashTable = NIL;
/* type unique_id -> courierinfo* */

static courierinfo *_courier_AddClassInformation(ilu_Class);

static ilu_cardinal _courier_HashCourier (courierinfo *key, ilu_cardinal size)
{
  return ((key->cri_pnumber + key->cri_version) % size);
}

static ilu_boolean _courier_CompareCourier (courierinfo *key1, courierinfo *key2)
{
  return ((key1->cri_pnumber == key2->cri_pnumber) && (key1->cri_version == key2->cri_version));
}

typedef ilu_cardinal (*ILU_hash_proc)(ilu_refany, ilu_cardinal);
typedef ilu_boolean (*ILU_find_proc)(ilu_refany, ilu_refany);

static void _courier_EnsureRegistries(void)
{
  if (RegistryHashTable == NIL)
    {
      RegistryHashTable = ilu_hash_MakeNewTable (137, NULLFN, NULLFN);
      ProgramNumberHashTable =
	ilu_hash_MakeNewTable (137,
				(ILU_hash_proc)_courier_HashCourier,
				(ILU_find_proc)_courier_CompareCourier);
      _courier_AddClassInformation (_ilu_rootClass);
      _courier_AddClassInformation (_ilu_GcCallbackClass);
    }
}

static courierinfo * _courier_AddClassInformation (ilu_Class class)
{
  char *next = NIL;
  courierinfo *s = (courierinfo *) ilu_must_malloc(sizeof(courierinfo));

  _courier_EnsureRegistries();

  s->cri_class = class;
  s->cri_type_id = class->cl_unique_id;
  if (class_singleton(class))
    {
      if (((unsigned long) (class->cl_singleton)) == 1)
	{
	  ILU_ERRPRINTF("\
Fatal ILU error:  Stubs for type \"%s\" were generated\n\
by a pre-1.6.4-p8 stubber.  Please re-stub, re-compile, and re-link.\n",
		   class->cl_name);
	  exit(1);
	}
      if (strncmp(class->cl_singleton, "courier_", 9) == 0)
	{
	  if (! ((s->cri_pnumber = _ilu_atoi(class->cl_singleton + 9, &next),
		    (s->cri_pnumber > 0 && (next != NIL) && (*next == '_'))) &&
		   (s->cri_version = _ilu_atoi(next + 1, NIL),
		    (s->cri_version > 0))))
	    {
	      ILU_ERRPRINTF("(ILU:courier)  Couldn't determine Courier program number for ILU class \"%s\", given native Courier info \"%s\".\n",
			    class->cl_name, class->cl_singleton);
	      return (NIL);
	    }
	}
      else
	return NIL;
    }
  else
    {
      ilu_cardinal hash = ilu_CRC32 ((ilu_bytes) class->cl_unique_id, strlen(class->cl_unique_id));

      s->cri_pnumber = ILU_COURIER_PROGRAM_NUMBER | ((hash >> 16) & 0xFFFF);
      s->cri_version = hash & 0xFFFF;
    }
  _ilu_Assert((int) ilu_hash_AddToTable(RegistryHashTable,
				   s->cri_type_id, s),
	      "Courier AddToTable RegistryHashTable");
  _ilu_Assert((int) ilu_hash_AddToTable(ProgramNumberHashTable, s, s),
	      "Courier AddToTable ProgramNumberHashTable");
  return (s);
}

static courierinfo *_courier_CourierInformationForClass (ilu_Class class)
{
  courierinfo *s = NIL;

  _courier_EnsureRegistries();

  s = (courierinfo *) ilu_hash_FindInTable (RegistryHashTable,
					    class->cl_unique_id);
  if (s == NIL)
    s = _courier_AddClassInformation (class);

  ILU_NOTE(COURIER_DEBUG,
	("%s \"%s:%s\", pnumber is 0x%lx, version is %lu.\n",
	 "_courier_CourierInformationForClass:  Class",
	 class->cl_name, class->cl_unique_id,
	 (s == NIL) ? 0L : s->cri_pnumber,
	 (s == NIL) ? 0L : s->cri_version));

  return (s);
}

static void AddClass (ilu_Class c, ilu_refany junk)
{
  _courier_CourierInformationForClass (c);
}

static courierinfo *_courier_ClassFromProgramNumber (ilu_cardinal pnumber,
						   ilu_cardinal version,
						   ilu_boolean try_adds)
{
  courierinfo dummy;
  courierinfo *s = NIL;

  _courier_EnsureRegistries();

  dummy.cri_pnumber = pnumber;
  dummy.cri_version = version;
  s = (courierinfo *) ilu_hash_FindInTable (ProgramNumberHashTable,
					      (ilu_refany) &dummy);
  if (s == NIL)
    {
      if (try_adds)
	{
	  _ilu_EnumerateClasses (AddClass, NIL);
	  return _courier_ClassFromProgramNumber (pnumber, version, ilu_FALSE);
	}
      else
	{
	  ILU_ERRPRINTF("(ILU:_courier_ClassFromProgramNumber):  Couldn't find class for program number 0x%lx, version %lu\n",
		   pnumber, version);
	}
    }
  else
    {
      ILU_NOTE(COURIER_DEBUG,
	    ("%s \"%s:%s\", pnumber is 0x%lx, version is %lu.\n",
	     "_courier_ClassFromProgramNumber:  Class",
	     s->cri_class->cl_name, s->cri_class->cl_unique_id,
	     s->cri_pnumber, s->cri_version));
    }

  return (s);
}

/*======================================================================*/
/*======================== Basic I/O code ==============================*/
/*======================================================================*/

#define INPUT_ERROR		1
#define OUTPUT_ERROR		2

#define SWAP_WORD(a) ( ((a) << 24) | \
                      (((a) << 8) & 0x00ff0000) | \
                      (((a) >> 8) & 0x0000ff00) | \
		      ((ilu_cardinal)(a) >>24) )

/*L1, L2, Main unconstrained (this is only for calling from debugger)*/
static ilu_cardinal ilu_courier_SetMaxStringSize (ilu_cardinal size)
{
  ilu_cardinal old_size = CourierMaxStringSize;
  if (size > 0)
    CourierMaxStringSize = size;
  return (old_size);  
}

/*L1, L2 unconstrained for sizing, end*/
/*Main Invariant, Call-OHi(call) for output*/
/*Main Invariant, Call-IHi(call) for input*/

/* ==================== short cardinal ==================== */

static void
_courier_OutputShortCardinal(ilu_Call call, ilu_shortcardinal i,
			     ILU_ERRS((IoErrs)) * err)
{
  register unsigned char *buf;

  buf = transport_get_output_buffer(callport(call), 2, err);
  if (ILU_ERROK(*err)) {
#ifdef WORDS_BIGENDIAN
    *((ilu_shortcardinal *) &buf[0]) = i;
#else
    *((ilu_shortcardinal *) &buf[0]) = ((i >> 8) | ((i & 0xFF) << 8));
#endif
  }
  return;
}

static void
_courier_OutputCardShouldBeShort(ilu_Call call, ilu_cardinal i,
				 ILU_ERRS((IoErrs)) * err)
{
  _ilu_Assert(i < 0x10000, "courier_OutputCardShouldBeShort");
  _courier_OutputShortCardinal(call, (ilu_shortcardinal) i, err);
}

static void 
_courier_InputShortCardinal(ilu_Call call, ilu_shortcardinal * i,
			    ILU_ERRS((IoErrs)) * err)
{
  unsigned char  *buf;
  buf = transport_get_input_buffer(callport(call), 2, err);
  if (ILU_ERROK(*err)) {
#ifdef WORDS_BIGENDIAN
    *i = *((ilu_shortcardinal *) &buf[0]);
#else
    *i = (buf[0] << 8) | buf[1];
#endif
  }
  return;
}

/*ARGSUSED*/
static          ilu_cardinal
_courier_SizeOfShortCardinal(ilu_Call call, ilu_shortcardinal i,
			     ILU_ERRS((IoErrs)) * err)
{
  (void) _ilu_Assert(i < 0x10000, "courier_OutputCardShouldBeShort");
  ILU_CLER(*err);
  return (2);
}

/* ==================== cardinal ==================== */

static void 
_courier_OutputCardinal(ilu_Call call, ilu_cardinal l,
			ILU_ERRS((IoErrs)) * err)
{
  register unsigned char *buf;

  buf = transport_get_output_buffer(callport(call), 4, err);
  if (ILU_ERROK(*err)) {
#ifdef WORDS_BIGENDIAN
    MEMCPY(buf, &l, 4);
#else
    ilu_cardinal tmp = SWAP_WORD(l);
    MEMCPY(buf, &tmp, 4);
#endif
  }
  return;
}

static void 
_courier_InputCardinal(ilu_Call call, ilu_cardinal * i,
		       ILU_ERRS((IoErrs)) * err)
{
  unsigned char  *buf;
  buf = transport_get_input_buffer(callport(call), 4, err);
  if (ILU_ERROK(*err)) {
#ifdef WORDS_BIGENDIAN
    MEMCPY(i, buf, 4);
#else
    ilu_cardinal tmp;
    MEMCPY (&tmp, buf, 4);
    *i = SWAP_WORD(tmp);
#endif
  }
  return;
}

/*ARGSUSED*/
static          ilu_cardinal
_courier_SizeOfCardinal(ilu_Call call, ilu_cardinal i,
			ILU_ERRS((IoErrs)) * err)
{
  ILU_CLER(*err);
  return (4);
}

/* ==================== integer ==================== */

static void 
_courier_OutputInteger(ilu_Call call, ilu_integer i,
		       ILU_ERRS((IoErrs)) * err)
{
  _courier_OutputCardinal(call, (ilu_cardinal) i, err);
}

static void 
_courier_InputInteger(ilu_Call call, ilu_integer * i,
		      ILU_ERRS((IoErrs)) * err)
{
  _courier_InputCardinal(call, (ilu_cardinal *) i, err);
}

/*ARGSUSED*/
static          ilu_cardinal
_courier_SizeOfInteger(ilu_Call call, ilu_integer i,
		       ILU_ERRS((IoErrs)) * err)
{
  ILU_CLER(*err);
  return (4);
}

/* ==================== short integer ==================== */

static void 
_courier_OutputShortInteger(ilu_Call call, ilu_shortinteger i,
			    ILU_ERRS((IoErrs)) * err)
{
  _courier_OutputShortCardinal(call, (ilu_shortcardinal) i, err);
}

static void 
_courier_InputShortInteger(ilu_Call call, ilu_shortinteger * i,
			   ILU_ERRS((IoErrs)) * err)
{
  _courier_InputShortCardinal(call, (ilu_shortcardinal *) i, err);
}

/*ARGSUSED*/
static          ilu_cardinal
_courier_SizeOfShortInteger(ilu_Call call, ilu_shortinteger i,
			    ILU_ERRS((IoErrs)) * err)
{
  ILU_CLER(*err);
  return (2);
}

/* ==================== long integer ==================== */

static void 
_courier_OutputLongInteger(ilu_Call call, ilu_longinteger i,
			   ILU_ERRS((IoErrs)) * err)
{
  _courier_OutputInteger(call,
			 (ilu_integer) ILU_LONGINT_HIGH_WORD(&i),
			 err);
  if (ILU_ERROK(*err))
    _courier_OutputCardinal(call, ILU_LONGINT_LOW_WORD(&i), err);
}

static void 
_courier_InputLongInteger(ilu_Call call, ilu_longinteger * i,
			  ILU_ERRS((IoErrs)) * err)
{
  _courier_InputInteger(call,
			(ilu_integer *) & ILU_LONGINT_HIGH_WORD(i),
			err);
  if (ILU_ERROK(*err))
    _courier_InputCardinal(call, &ILU_LONGINT_LOW_WORD(i), err);
}

/*ARGSUSED*/
static          ilu_cardinal
_courier_SizeOfLongInteger(ilu_Call call, ilu_longinteger i,
			   ILU_ERRS((IoErrs)) * err)
{
  ILU_CLER(*err);
  return (8);
}

/* ==================== long cardinal ==================== */

static void 
_courier_OutputLongCardinal(ilu_Call call, ilu_longcardinal i,
			    ILU_ERRS((IoErrs)) * err)
{
  _courier_OutputCardinal(call, ILU_LONGCARD_HIGH_WORD(&i), err);
  if (ILU_ERROK(*err))
    _courier_OutputCardinal(call, ILU_LONGCARD_LOW_WORD(&i), err);
}

static void 
_courier_InputLongCardinal(ilu_Call call, ilu_longcardinal * i,
			   ILU_ERRS((IoErrs)) * err)
{
  _courier_InputCardinal(call, &ILU_LONGCARD_HIGH_WORD(i), err);
  if (ILU_ERROK(*err))
    _courier_InputCardinal(call, &ILU_LONGCARD_LOW_WORD(i), err);
}

/*ARGSUSED*/
static          ilu_cardinal
_courier_SizeOfLongCardinal(ilu_Call call, ilu_longcardinal i,
			    ILU_ERRS((IoErrs)) * err)
{
  ILU_CLER(*err);
  return (8);
}

/* ==================== real ==================== */

static void 
_courier_OutputReal(ilu_Call call, double d,
		    ILU_ERRS((IoErrs)) * err)
{
  double          l2 = d;
  ilu_cardinal   *h = (ilu_cardinal *) & l2;

  _courier_OutputCardinal(call, h[THIS_ARCH_IS_BIGENDIAN ? 0 : 1], err);
  if (ILU_ERROK(*err))
    _courier_OutputCardinal(call, h[THIS_ARCH_IS_BIGENDIAN ? 1 : 0], err);
}

static void 
_courier_InputReal(ilu_Call call, double *d,
		   ILU_ERRS((IoErrs)) * err)
{
  ilu_cardinal   *h = (ilu_cardinal *) d;

  _courier_InputCardinal(call, h + (THIS_ARCH_IS_BIGENDIAN ? 0 : 1), err);
  if (ILU_ERROK(*err))
    _courier_InputCardinal(call, h + (THIS_ARCH_IS_BIGENDIAN ? 1 : 0), err);
}

/*ARGSUSED*/
static          ilu_cardinal
_courier_SizeOfReal(ilu_Call call, double d,
		    ILU_ERRS((IoErrs)) * err)
{
  ILU_CLER(*err);
  return (8);
}

/* ==================== long real ==================== */

static void 
_courier_OutputLongReal(ilu_Call call, ilu_longreal d,
			ILU_ERRS((IoErrs)) * err)
{
  transport_write_bytes(callport(call), (ilu_bytes) &d, 16, err);
}

static void 
_courier_InputLongReal(ilu_Call call, ilu_longreal * d,
		       ILU_ERRS((IoErrs)) * err)
{
  (void) transport_read_bytes(callport(call), (ilu_bytes) d, 16, err);
}

/*ARGSUSED*/
static          ilu_cardinal
_courier_SizeOfLongReal(ilu_Call call, ilu_longreal d,
			ILU_ERRS((IoErrs)) * err)
{
  ILU_CLER(*err);
  return (16);
}

/* ==================== short real ==================== */

static void 
_courier_OutputShortReal(ilu_Call call, float f,
			 ILU_ERRS((IoErrs)) * err)
{
  float           f2 = f;
  _courier_OutputCardinal(call, *((ilu_cardinal *) & f2), err);
}

static void 
_courier_InputShortReal(ilu_Call call, float *f,
			ILU_ERRS((IoErrs)) * err)
{
  _courier_InputCardinal(call, (ilu_cardinal *) f, err);
}

/*ARGSUSED*/
static ilu_cardinal 
_courier_SizeOfShortReal(ilu_Call call, float d,
			 ILU_ERRS((IoErrs)) * err)
{
  ILU_CLER(*err);
  return (4);
}

/* ==================== bytes ==================== */

static void
_courier_OutputBytes(ilu_Call call, ilu_bytes s, ilu_cardinal len,
		     ilu_cardinal limit, ILU_ERRS((IoErrs)) * err)
{
  if (limit > 0 && len > limit) {
    ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_sequenceLimit, 0);
    return;
  }
  if (limit > 0 && limit <= 0xFFFF)
    _courier_OutputCardShouldBeShort(call, len, err);
  else
    _courier_OutputCardinal(call, len, err);
  if (ILU_ERRNOK(*err))
    return;
  (void) transport_write_bytes(callport(call), s,
			       PADDED_SIZE(len), err);
}

static void 
_courier_InputBytes(ilu_Call call, ilu_bytes * s, ilu_cardinal * len,
		    ilu_cardinal limit, ILU_ERRS((IoErrs)) * err)
{
  ilu_Transport   t = callport(call);
  ilu_shortcardinal l3;
  ilu_cardinal    l2;
  if (limit != 0 && limit <= 0xFFFF) {
    /* short (native Courier) string */
    _courier_InputShortCardinal(call, &l3, err);
    l2 = l3;
  } else
    _courier_InputCardinal(call, &l2, err);
  if (ILU_ERRNOK(*err))
    return;
  if (limit > 0 && l2 > limit) {
    (void) ILU_ERR_CONS1(marshal, err, minor, ilu_mm_sequenceLimit, 6);
    return;
  }
  *s = ilu_malloc(l2 + 1);
  if (*s == NIL) {
    (void) ILU_ERR_CONS1(no_memory, err, nbytes, l2 + 1, 6);
    return;
  }
  (void) transport_read_bytes(t, *s, PADDED_SIZE(l2), err);
  (*s)[l2] = 0;
  /* ... so this can be used to input a C string */
  *len = l2;
  return;
}

/*ARGSUSED*/
static          ilu_cardinal
_courier_SizeOfBytes(ilu_Call call, ilu_bytes i, ilu_cardinal l,
		     ilu_cardinal limit, ILU_ERRS((IoErrs)) * err)
{
  if ((limit > 0) && (l > limit))
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0);
  if (l > CourierMaxStringSize)
    return ILU_ERR_CONS1(imp_limit, err, minor, ilu_ilm_strlen, 0);
  ILU_CLER(*err);
  return (((limit != 0 && limit <= 0xFFFF) ? 2 : 4) + PADDED_SIZE(l));
}

/* ==================== string ==================== */

static void 
_courier_OutputString(ilu_Call call, void * s, ilu_cardinal len,
		      ilu_cardinal limit,
		      ilu_cardinal expected_encoding,
		      ilu_cardinal current_encoding,
		      ILU_ERRS((IoErrs)) * err)
{
  if ((expected_encoding != ILU_StringEncoding_latin1) ||
      (current_encoding != ILU_StringEncoding_latin1)) {
    ILU_ERR_CONS1(imp_limit, err, minor, ilu_ilm_unsupported_charset_encoding, 0);
    return;
  }
  _courier_OutputBytes(call, (ilu_bytes) s, len, limit, err);
}

static void 
_courier_InputString(ilu_Call call, void ** s, ilu_cardinal * len,
		     ilu_cardinal limit, ilu_cardinal expected_encoding,
		     ilu_cardinal *current_encoding,
		     ILU_ERRS((IoErrs)) * err)
{
  if (expected_encoding != ILU_StringEncoding_latin1) {
    ILU_ERR_CONS1(imp_limit, err, minor, ilu_ilm_unsupported_charset_encoding, 0);
    return;
  };
  *current_encoding = ILU_StringEncoding_latin1;
  _courier_InputBytes(call, (ilu_bytes *) s, len, limit, err);
}

  /*ARGSUSED*/
static          ilu_cardinal
_courier_SizeOfString(ilu_Call call, void * i, ilu_cardinal l,
		      ilu_cardinal limit,
		      ilu_cardinal expected_encoding,
		      ilu_cardinal current_encoding,
		      ILU_ERRS((IoErrs)) * err)
{
  if ((expected_encoding != ILU_StringEncoding_latin1) ||
      (current_encoding != ILU_StringEncoding_latin1)) {
    return ILU_ERR_CONS1(imp_limit, err, minor, ilu_ilm_unsupported_charset_encoding, 0);
  }
  return _courier_SizeOfBytes(call, (ilu_bytes) i, l, limit, err);
}

/* ==================== byte ==================== */

static void _courier_OutputByte (ilu_Call call, ilu_byte b,
			                 ILU_ERRS((IoErrs)) * err)
{
  _courier_OutputShortCardinal(call, (ilu_shortcardinal) b, err);
}

static void
_courier_InputByte(ilu_Call call, ilu_byte * b,
		   ILU_ERRS((IoErrs)) * err)
{
  ilu_shortcardinal l = 0;
  _courier_InputShortCardinal(call, &l, err);
  *b = (ilu_byte)(l & 0xFF);
  return;
}

/*ARGSUSED*/
static          ilu_cardinal
_courier_SizeOfByte(ilu_Call call, ilu_byte i,
		    ILU_ERRS((IoErrs)) * err)
{
  ILU_CLER(*err);
  return (2);
}

/* ==================== short char ==================== */

static void
_courier_OutputShortChar(ilu_Call call, ilu_shortcharacter b,
			 ILU_ERRS((IoErrs)) * err)
{
  _courier_OutputByte(call, (ilu_byte) b, err);
}

static void
_courier_InputShortChar(ilu_Call call, ilu_shortcharacter * b,
			ILU_ERRS((IoErrs)) * err)
{
  _courier_InputByte(call, (ilu_byte*) b, err);
}

/*ARGSUSED*/
static          ilu_cardinal
_courier_SizeOfShortChar(ilu_Call call, ilu_shortcharacter i,
			 ILU_ERRS((IoErrs)) * err)
{
  return _courier_SizeOfByte(call, (ilu_byte) i, err);
}


/* ==================== optional ==================== */

static void
  _courier_OutputOptional (ilu_Call call, ilu_boolean i,
			   ilu_Type junk, ILU_ERRS((IoErrs)) *err)
{
  _courier_OutputShortCardinal(call, i ? 1 : 0, err);
}

static void
  _courier_InputOptional (ilu_Call call, ilu_boolean *i,
			  ilu_Type junk, ILU_ERRS((IoErrs)) *err)
{
  ilu_shortcardinal i2;

  _courier_InputShortCardinal(call, &i2, err);
  if (ILU_ERROK(*err))
    *i = ((i2 == 0) ? ilu_FALSE : ilu_TRUE);
}

/*ARGSUSED*/
static ilu_cardinal
  _courier_SizeOfOptional (ilu_Call call, ilu_boolean i,
			   ilu_Type junk, ILU_ERRS((IoErrs)) *err)
{
  ILU_CLER(*err);
  return (2);
}

/* ==================== opaque ==================== */

static void
_courier_OutputOpaque(ilu_Call call, ilu_bytes o, ilu_cardinal len,
		      ILU_ERRS((IoErrs)) * err)
{
  (void) transport_write_bytes(callport(call), o,
			       PADDED_SIZE(len), err);
}

static void 
_courier_InputOpaque(ilu_Call call, ilu_bytes * o, ilu_cardinal len,
		     ILU_ERRS((IoErrs)) * err)
{
  if (*o == NIL) {
    if ((*o = ilu_malloc(PADDED_SIZE(len))) == NIL) {
      (void) ILU_ERR_CONS1(no_memory, err, nbytes, PADDED_SIZE(len), 6);
      return;
    }
  }
  (void) transport_read_bytes(callport(call), *o,
			      PADDED_SIZE(len), err);
}

static          ilu_cardinal
_courier_SizeOfOpaque(ilu_Call call, ilu_bytes o, ilu_cardinal len,
		      ILU_ERRS((IoErrs)) * err)
{
  ILU_CLER(*err);
  return (PADDED_SIZE(len));
}

/* ==================== sequence ==================== */

static void
_courier_OutputSequence(ilu_Call c, ilu_cardinal seqLen,
		      ilu_cardinal limit, ilu_Type prefix, ILU_ERRS((IoErrs)) * err)
{
  if (limit != 0 && limit <= 0xFFFF)
    _courier_OutputShortCardinal(c, (ilu_shortcardinal) seqLen, err);
  else
    _courier_OutputCardinal(c, seqLen, err);
}

static void 
_courier_OutputSequenceMark(ilu_Call c, ilu_cardinal extent,
			    ILU_ERRS((IoErrs)) * err)
{
  ILU_CLER(*err);
}

static void 
_courier_InputSequenceMark(ilu_Call c, ilu_cardinal extent,
			   ILU_ERRS((IoErrs)) * err)
{
  ILU_CLER(*err);
}

static void 
_courier_InputSequence(ilu_Call c, ilu_cardinal * sequenceLength,
		       ilu_cardinal limit, ilu_Type prefix, ILU_ERRS((IoErrs)) * err)
{
  ilu_shortcardinal len = 0;
  if (limit != 0 && limit <= 0xFFFF) {
    _courier_InputShortCardinal(c, &len, err);
    *sequenceLength = len;
  } else
    _courier_InputCardinal(c, sequenceLength, err);
  return;
}

static void _courier_EndSequence (ilu_Call c, ILU_ERRS((IoErrs)) * err)
{
  ILU_CLER(*err);
}

static          ilu_cardinal
_courier_SizeOfSequence(ilu_Call c, ilu_cardinal length,
		      ilu_cardinal limit, ilu_Type prefix, ILU_ERRS((IoErrs)) * err)
{
  if (limit != 0 && limit <= 0xFFFF)
    return _courier_SizeOfShortCardinal(c, (ilu_shortcardinal) length,
					err);
  else
    return _courier_SizeOfCardinal(c, length, err);
}

/* ==================== enumeration ==================== */

static void 
_courier_OutputEnumeration(ilu_Call call, ilu_shortcardinal i,
			   ilu_Type prefix, ILU_ERRS((IoErrs)) * err)
{
  _courier_OutputShortCardinal(call, i, err);
}

static void 
_courier_InputEnumeration(ilu_Call call, ilu_shortcardinal * i,
			  ilu_Type prefix, ILU_ERRS((IoErrs)) * err)
{
  _courier_InputShortCardinal(call, i, err);
}

/*ARGSUSED*/
static          ilu_cardinal
_courier_SizeOfEnumeration(ilu_Call call, ilu_shortcardinal i,
			   ilu_Type prefix, ILU_ERRS((IoErrs)) * err)
{
  ILU_CLER(*err);
  return (2);
}

/* ==================== union ==================== */

static void 
_courier_OutputUnion(ilu_Call c, ilu_cardinal typeIndex,
		     ilu_TypeKind tk,
		     ilu_Type prefix, ILU_ERRS((IoErrs)) * err)
{
  if ((tk == ilu_cardinal_tk) || (tk == ilu_integer_tk)) {
    ILU_ERR_CONS1(imp_limit,err,minor,ilu_ilm_bad_type_for_protocol,0);
    return;
  };
  _courier_OutputShortCardinal(c, (ilu_shortcardinal) typeIndex, err);
}

static void 
_courier_InputUnion(ilu_Call c, ilu_cardinal * typeIndex,
		    ilu_TypeKind tk,
		    ilu_Type prefix, ILU_ERRS((IoErrs)) * err)
{
  ilu_shortcardinal temp;

  if ((tk == ilu_cardinal_tk) || (tk == ilu_integer_tk)) {
    ILU_ERR_CONS1(imp_limit,err,minor,ilu_ilm_bad_type_for_protocol,0);
    return;
  };
  _courier_InputShortCardinal(c, &temp, err);
  *typeIndex = temp;
  return;
}

static void _courier_EndUnion (ilu_Call c, ILU_ERRS((IoErrs)) * err)
{
  ILU_CLER(*err);
}

static          ilu_cardinal
_courier_SizeOfUnion(ilu_Call c, ilu_cardinal typeIndex,
		     ilu_TypeKind tk,
		     ilu_Type prefix, ILU_ERRS((IoErrs)) * err)
{
  if ((tk == ilu_cardinal_tk) || (tk == ilu_integer_tk)) {
    ILU_ERR_CONS1(imp_limit,err,minor, ilu_ilm_bad_type_for_protocol,0);
    return 0;
  };
  return (_courier_SizeOfShortCardinal(c, (ilu_shortcardinal) typeIndex,
				       err));
}

/* ==================== array ==================== */

static void
_courier_OutputArray(ilu_Call c, ilu_cardinal len,
		     ilu_Type prefix, ILU_ERRS((IoErrs)) * err)
{
  ILU_CLER(*err);
}

static void 
_courier_InputArray(ilu_Call c, ilu_Type prefix, ILU_ERRS((IoErrs)) * err)
{
  ILU_CLER(*err);
}

static void _courier_EndArray (ilu_Call c, ILU_ERRS((IoErrs)) * err)
{
  ILU_CLER(*err);
}

static          ilu_cardinal
_courier_SizeOfArray(ilu_Call c, ilu_cardinal len,
		     ilu_Type prefix, ILU_ERRS((IoErrs)) * err)
{
  ILU_CLER(*err);
  return (0);
}

/* ==================== record ==================== */

static void _courier_OutputRecord (ilu_Call c, ilu_Type prefix, ILU_ERRS((IoErrs)) * err)
{
  ILU_CLER(*err);
}

static void _courier_InputRecord (ilu_Call c, ilu_Type prefix, ILU_ERRS((IoErrs)) * err)
{
  ILU_CLER(*err);
}

static void _courier_EndRecord (ilu_Call c, ILU_ERRS((IoErrs)) * err)
{
  ILU_CLER(*err);
}

static          ilu_cardinal
_courier_SizeOfRecord(ilu_Call c, ilu_Type prefix, ILU_ERRS((IoErrs)) * err)
{
  ILU_CLER(*err);
  return (0);
}

/*======================================================================
**======================================================================
**====================  Non-I/O code ===================================
**======================================================================
**====================================================================*/

/*L2, Main unconstrained*/
/*L1_sup < prmu*/
static ilu_string _courier_FormProtocolHandle (ilu_refany pdata, ilu_Object obj)
{
  char            buf[100];
  courierinfo    *s = NULL;
  ilu_Class       class = object_class(obj);

  _ilu_AcquireMutex(ilu_prmu);
  s = _courier_CourierInformationForClass(class);
  if (s == NULL) {
    _ilu_ReleaseMutex(ilu_prmu);
    ILU_ERRPRINTF("%s program#/version for class %s.\n",
     "_courier_FormProtocolHandle:  Can't figure", class->cl_name);
    return (NULL);
  } else {
    sprintf(buf, "courier_0x%lx_%ld", ((unsigned long) s->cri_pnumber) & 0xFFFFFFFF,
	    ((unsigned long) s->cri_version) & 0xFFFFFFFF);
    _ilu_ReleaseMutex(ilu_prmu);
    return (_ilu_Strdup(buf));
  }
}
     
/*Main Invariant holds*/
/*L2 >= {conn's iomu}*/

static          ilu_ReadHeaderResultCode
_courier_ReadHeader(ilu_Call call, ilu_PacketType * type,
		    ilu_cardinal * sn,
		    ilu_ConnShutdownReason *reason, ilu_cardinal *lastSN,
		    ILU_ERRS((IoErrs)) * err)
{
  ilu_Transport   bs = callport(call);
  ilu_bytes       packet;
  ilu_cardinal    serialNumber, packetType;
  ilu_ReadHeaderResultCode ans;

  ans = transport_begin_message(bs, ilu_TRUE, err);
  switch (ans) {
  case ilu_rhrc_ok:
    break;
  case ilu_rhrc_error:
    ILU_NOTE((INCOMING_DEBUG | COURIER_DEBUG),
	  ("courier:  error %s on transport_begin_message (input)\n",
	   ILU_ERR_NAME(*err)));
  case ilu_rhrc_eof:
    *reason = ilu_ConnShutdownReason_ReceivedEOF;
    *lastSN = 0;
  case ilu_rhrc_nothing:
    return ans;
  default:
    _ilu_Assert(ilu_FALSE, "courier_ReadHeader: transport_begin_message");
  }
  packet = transport_get_input_buffer(bs, 4, err);
  if (packet == NIL) {
    ILU_NOTE((INCOMING_DEBUG | COURIER_DEBUG),
	  ("courier:  error %s reading header.\n",
	   ILU_ERR_NAME(*err)));
    return (ilu_rhrc_error);
  }

  packetType = (packet[0] << 8) | packet[1];
  serialNumber = (packet[2] << 8) | packet[3];

  ILU_NOTE((INCOMING_DEBUG | COURIER_DEBUG),
	("courier:  read header with SN %lu, type %lu (%s).\n",
	 serialNumber, packetType,
	 ((packetType == 0) ? "request"
	  : ((packetType == 2) ? "reply"
	     : ((packetType == 3 || packetType == 1) ? "exception"
		: "unknown type")))));

  * type = ((packetType == 0) ? ilu_PacketType_Request
	    : ilu_PacketType_Reply);
  call->ca_prdata1 = packetType;
  *sn = serialNumber;
  return (ans);
}

typedef struct {
  ilu_Transport   trans;
  ilu_cardinal    replyType;
}               *Queued;

static ilu_refany 
_courier_DelayInterp(ilu_Call call,
		    ILU_ERRS((IoErrs)) * err)
{
  ilu_Transport   t = callport(call);
  Queued          q = ilu_malloc(sizeof(*q));
  if (q == NIL)
    return ILU_ERR_CONS1(no_memory, err, nbytes, sizeof(*q), NIL);
  q->replyType = call->ca_prdata1;
  q->trans = _ilu_BufferInputMessage(t, 0, ilu_FALSE, err);
  return (ILU_ERROK(*err) ? q : NIL);
}

static void _courier_ResumeInterp(ilu_Call call, ilu_refany x)
{
  Queued          q = (Queued) x;
  call->ca_prTrans = q->trans;
  call->ca_prdata1 = q->replyType;
  call->ca_prbit1 = ilu_TRUE;
  ilu_free(q);
  return;
}

static ilu_boolean 
_courier_DiscardMessage(ilu_Call call, ILU_ERRS((internal)) * err)
{
  ilu_Transport   t = callport(call);
  ILU_ERRS((IoErrs)) lerr;
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
  return ILU_CLER(*err);
}

/*L1 >= {cmu}; L2 >= {conn's iomu, waitmu}*/
static          ilu_boolean
_courier_AbandonDelayedInterp(ilu_Connection conn, ilu_refany x,
			      ILU_ERRS((internal)) * err)
{
  Queued          q = (Queued) x;
  ilu_boolean     ans;
  ilu_Transport   trans = q->trans;
  ilu_integer     cdfd;
  ilu_free(q);
  ans = transport_close(trans, &cdfd, err);
  ILU_ERR_SWITCH(*err) {
    ILU_ERR_CASE2(bad_locks, broken_locks) {
      ILU_HANDLED(*err);
      (void) ilu_Check(ilu_FALSE, err);
    }
    ILU_ERR_ELSE /* no effect */;
  } ILU_ERR_ENDSWITCH;
  _ilu_Assert(cdfd == 0, "courier AbandonDelayedInterp");
  return ans;
}

static          ilu_boolean
_courier_InterpretRequest(ilu_Call call,
			   ILU_ERRS((IoErrs)) *err)
{
  ilu_cardinal    programNumber;
  ilu_shortcardinal programVersion, methodID;
  ilu_Class       class;
  courierinfo    *s;

  _courier_InputCardinal(call, &programNumber, err);
  if (ILU_ERRNOK(*err)) return ilu_FALSE;
  _courier_InputShortCardinal(call, &programVersion, err);
  if (ILU_ERRNOK(*err)) return ilu_FALSE;
  _courier_InputShortCardinal(call, &methodID, err);
  if (ILU_ERRNOK(*err)) return ilu_FALSE;
  ILU_NOTE((INCOMING_DEBUG | COURIER_DEBUG),
	("courier:InterpretRequest:  (call SN %lu) class ID = 0x%lx (version %u), method ID = %u\n",
	 call->ca_SN, programNumber, programVersion, methodID));

  _ilu_AcquireMutex(ilu_prmu);
  s = _courier_ClassFromProgramNumber(programNumber, programVersion,
				      ilu_TRUE);
  class = (s == NULL) ? NULL
    : (s->cri_class == NULL) ? ilu_FindClassFromID(s->cri_type_id)
    : s->cri_class;
  call_intro_type(call) = class;
  _ilu_ReleaseMutex(ilu_prmu);
  if (NULL == s || NULL == class) {
    ILU_NOTE((INCOMING_DEBUG | COURIER_DEBUG),
	  ("_courier_InterpretRequest:  (call %lu) Can't find ilu_Class with pn 0x%lx, version %u\n",
	   call->ca_SN, programNumber, programVersion));
    call->ca_pe = ilu_ProtocolException_NoSuchClassAtServer;
    return (ilu_FALSE);
  }
  ILU_NOTE((INCOMING_DEBUG | COURIER_DEBUG),
	("_courier_InterpretRequest:  (call SN %lu) intro_type is %s\n",
	 call->ca_SN, class->cl_name));

  call->ca_method = ilu_FindMethodByID(class, methodID);

  if (call->ca_method == NULL) {
    ILU_NOTE((INCOMING_DEBUG | COURIER_DEBUG),
	  ("_courier_InterpretRequest:  (call %lu) Can't find method on class 0x%lx with methodID = %u.\n",
	   call->ca_SN, programNumber, methodID));
    call->ca_pe = ilu_ProtocolException_NoSuchMethodOnClass;
    return (ilu_FALSE);
  } else {
    ILU_NOTE((INCOMING_DEBUG | COURIER_DEBUG),
	  ("_courier_InterpretRequest:  record for method %d is %p (%s).\n",
	   methodID, call->ca_method, call->ca_method->me_name));
  }
  ILU_NOTE((INCOMING_DEBUG | COURIER_DEBUG),
	("_courier_InterpretRequest:  returning ilu_TRUE\n"));
  return (ilu_TRUE);
}

/*L1, Main unconstrained*/
/*L2 >= {conn's iomu}*/

static          ilu_ProtocolException
_courier_InterpretReply(ilu_Call call, ilu_cardinal * estatus,
			ILU_ERRS((IoErrs)) * err)
{
  ilu_shortcardinal replyStatus = call->ca_prdata1, junk;
  static ilu_ProtocolException replyStatusExceptions[] = {
    ilu_ProtocolException_NoSuchClassAtServer,
    ilu_ProtocolException_ClassVersionMismatch,
    ilu_ProtocolException_NoSuchMethodOnClass,
    ilu_ProtocolException_GarbageArguments
  };

  ILU_NOTE((INCOMING_DEBUG | COURIER_DEBUG),
	("courier:InterpretReply:  SN %lu, replyStatus is %u\n",
	 call->ca_SN, replyStatus));

  if (replyStatus == 2) {	/* MSG_ACCEPTED */
    *estatus = 0;
    return (ilu_ProtocolException_Success);
  } else if (replyStatus == 3) {
    ilu_shortcardinal e;
    _courier_InputShortCardinal(call, &e, err);
    if (ILU_ERRNOK(*err))
      return ilu_ProtocolException_Not;
    *estatus = e;
    return (ilu_ProtocolException_Success);
  } else if (replyStatus == 1) {
    _courier_InputShortCardinal(call, &replyStatus, err);
    if (ILU_ERRNOK(*err))
      return ilu_ProtocolException_Not;
    switch (replyStatus) {
    case 1:
      _courier_InputShortCardinal(call, &junk, err);
      if (ILU_ERRNOK(*err))
	return ilu_ProtocolException_Not;
      /* lowest version num */
      _courier_InputShortCardinal(call, &junk, err);
      if (ILU_ERRNOK(*err))
	return ilu_ProtocolException_Not;
      /* highest version num */

    case 0:
    case 2:
    case 3:
      return (replyStatusExceptions[replyStatus]);

    default:
      return (ilu_ProtocolException_Unknown);
    }
  }
  return (ilu_ProtocolException_Unknown);
}

/*L1_sup < prmu*/

static ilu_boolean GetCourierProgramNumberAndVersion (ilu_Class pclass, ilu_cardinal *pnumber, ilu_cardinal *version)
{
  courierinfo *s = NULL;

  _ilu_AcquireMutex(ilu_prmu);
  if ((s = _courier_CourierInformationForClass (pclass)) != NULL)	/* probably consult external DB */
    {
      *pnumber = s->cri_pnumber;
      *version = s->cri_version;
    }
  _ilu_ReleaseMutex(ilu_prmu);
  return (s != NULL);
}

/*Main Invariant holds; L2 >= {call's conn's callmu, iomu}*/
static          ilu_boolean
_courier_StartRequest(ilu_Call call, ilu_cardinal argSize,
		      ILU_ERRS((IoErrs)) * err)
{
  ilu_cardinal    packetSize;
  ilu_cardinal    pnumber, version;
  ilu_Class       pclass = call->ca_intro_type;
  ilu_Method      method = call->ca_method;

  ILU_NOTE(COURIER_DEBUG,
	("_courier_StartRequest:  call %p (sn %lu), argSize %lu, class %s (%s), method %s (%ld)\n",
	 call, call_serial_number(call), argSize,
	 class_name(pclass), class_unique_id(pclass),
	 method_name(method), method_id(method)));

  if (!GetCourierProgramNumberAndVersion(pclass, &pnumber, &version)) {
    ILU_ERRPRINTF(
	    "_courier_StartRequest:  Can't determine program-number/version of class \"%s\" on call %p.\n",
	    pclass->cl_name, call);
    return (ilu_FALSE);
  }
  ILU_NOTE(COURIER_DEBUG,
	("_courier_StartRequest:  pnum/vers 0x%lx/%ld\n",
	 pnumber, version));

  packetSize = argSize
    + 12;			/* for fields of header */

  if (transport_begin_message(callport(call), ilu_FALSE, err)
      == ilu_rhrc_error)
    goto dun;

  _courier_OutputCardShouldBeShort(call, 0, err);	/* CallMessageBody */
  if (ILU_ERRNOK(*err))
    goto dun;
  _courier_OutputCardShouldBeShort(call, call->ca_SN, err);	/* trans'n ID */
  if (ILU_ERRNOK(*err))
    goto dun;
  _courier_OutputCardinal(call, pnumber, err);
  if (ILU_ERRNOK(*err))
    goto dun;
  _courier_OutputCardShouldBeShort(call, version, err);
  if (ILU_ERRNOK(*err))
    goto dun;
  _courier_OutputCardShouldBeShort(call, method->me_id, err);
  if (ILU_ERRNOK(*err))
    goto dun;

  ILU_NOTE(COURIER_DEBUG,
	("_courier_StartRequest:  request %lu begun (size %lu).\n",
	 call->ca_SN, packetSize));
  return (ilu_TRUE);
dun:
  return ilu_FALSE;
}

/*L1, Main unconstrained*/
/*L2 >= {conn's iomu}*/

static void 
_courier_MessageRead(ilu_Call call, ILU_ERRS((IoErrs)) * err)
{
  transport_end_message(callport(call), ilu_FALSE, NIL, err);
  return;
}

/*Main Invariant holds; L2 >= {call's conn's callmu, iomu}*/

static          ilu_boolean
_courier_SendPacket(ilu_Call call, ilu_boolean isReply,
		    ilu_Message * msg, ILU_ERRS((IoErrs)) * err)
{
  ilu_Transport   bs = callport(call);
  if (!transport_end_message(bs, ilu_TRUE, msg, err)) 
    return (ilu_FALSE);
  if (isReply && !bs->tr_class->tc_reliable)
    return _ilu_CacheCall(call, msg, err);
  return ilu_TRUE;
}

static          ilu_boolean
_courier_FinishRequest(ilu_Call call, ilu_Message * msg,
		       ilu_boolean push, ILU_ERRS((IoErrs)) * err)
{
  return (_courier_SendPacket(call, push, msg, err));
}

static          ilu_boolean
_courier_FinishReply(ilu_Call call, ilu_boolean push,
		     ILU_ERRS((IoErrs)) * err)
{
  ilu_Message     msg = {NIL, 0};
  return (_courier_SendPacket(call, push, &msg, err));
}

static          ilu_boolean
_courier_FinishException(ilu_Call call, ilu_boolean push,
			 ILU_ERRS((IoErrs)) * err)
{
  ilu_Message     msg = {NIL, 0};
  return (_courier_SendPacket(call, push, &msg, err));
}

static          ilu_cardinal
_courier_BeginSizingReply(ilu_Call call,
			  ilu_boolean exceptions,
			  ILU_ERRS((IoErrs)) * err)
{
  return (ILU_CLER(*err), 0);
}

static          ilu_boolean
_courier_BeginReply(ilu_Call call, ilu_boolean exceptions,
		    ilu_cardinal argSize, ILU_ERRS((IoErrs)) * err)
{
  ilu_cardinal    packetSize;
  ilu_Transport   trans = callport(call);

  ILU_NOTE(PACKET_DEBUG,
	("courier:BeginReply:  SN %lu, argSize %lu, exceptions %s, trans %p.\n",
	 call->ca_SN, argSize, exceptions ? "ilu_TRUE" : "ilu_FALSE",
	 trans));

  packetSize = argSize
    + 4;			/* for the basic header fields */

  if (transport_begin_message(trans, ilu_FALSE, err) == ilu_rhrc_error)
    goto faild;

  _courier_OutputCardShouldBeShort(call, 2, err);	/* msg type == REPLY */
  if (ILU_ERRNOK(*err)) goto faild;
  _courier_OutputCardShouldBeShort(call, call->ca_SN, err);
  if (ILU_ERRNOK(*err)) goto faild;

  ILU_NOTE(PACKET_DEBUG,
	("_courier_BeginReply:  started reply %lu (size %lu).\n",
	 call->ca_SN, packetSize));
  return (ilu_TRUE);
faild:
  return ilu_FALSE;
}

static          ilu_cardinal
_courier_BeginSizingExn(ilu_Call call,
			ilu_cardinal eindex,
			ilu_ProtocolException sysExnIdx,
			ILU_ERRS((IoErrs)) * err)
{
  return (ILU_CLER(*err), 0);
}

static          ilu_boolean
_courier_BeginException(ilu_Call call, ilu_cardinal evalue,
			ilu_ProtocolException sysExnIdx,
			ilu_cardinal argSize, ILU_ERRS((IoErrs)) * err)
{
  ilu_cardinal    packetSize;
  ilu_Transport   trans = callport(call);
  ilu_Connection conn = call_connection(call);

  if (evalue == 0)		/* send protocol error */
    packetSize = 6;		/* no values to send */
  else
    packetSize = argSize + 6;	/* for the fields of the header */

  if (transport_begin_message (trans, ilu_FALSE, err) == ilu_rhrc_error)
    goto faild;

  _courier_OutputCardShouldBeShort(call, (evalue == 0) ? 1 : 3, err);
  /* msg type == REJECT or ABORT */
  if (ILU_ERRNOK(*err)) goto faild;
  _courier_OutputCardShouldBeShort(call, call->ca_SN, err);
  if (ILU_ERRNOK(*err)) goto faild;
  _courier_OutputCardShouldBeShort(call, (evalue == 0) ? sysExnIdx : evalue,
				   err);
  /* exception value */
  if (ILU_ERRNOK(*err)) goto faild;

  ILU_NOTE(PACKET_DEBUG,
	("courier:BeginException:  exception started to peer %s:  SN %lu, size %lu, evalue %lu.\n",
	 conn_peerinfo(conn),
	 call_serial_number(call), packetSize,
	 (evalue == 0) ? sysExnIdx : evalue));
  return (ilu_TRUE);
faild:
  return ilu_FALSE;
}

/*L2, Main unconstrained*/
/*L1 >= {prmu}*/

static ilu_Protocol _courier_NewCourier (void)
{
  ilu_Protocol new = (ilu_Protocol)
		     ilu_malloc(sizeof(struct _ilu_Protocol_s));

  new->pr_concurrent_requests = ilu_FALSE;
  new->pr_sizing_required = ilu_FALSE;
  new->pr_needs_boundaried_transport = ilu_TRUE;
  new->pr_needs_reliable_transport = ilu_TRUE;

  new->pr_init_call = _courier_InitCall;
  new->pr_start_request = _courier_StartRequest;
  new->pr_finish_request = _courier_FinishRequest;
  new->pr_begin_sizing_reply = _courier_BeginSizingReply;
  new->pr_begin_reply = _courier_BeginReply;
  new->pr_finish_reply = _courier_FinishReply;
  new->pr_begin_sizing_exn = _courier_BeginSizingExn;
  new->pr_begin_exception = _courier_BeginException;
  new->pr_finish_exception = _courier_FinishException;
  new->pr_finish_call = _courier_FinishCall;
  new->pr_prefinish_call = NULLFN;

  new->pr_output_integer = _courier_OutputInteger;
  new->pr_input_integer = _courier_InputInteger;
  new->pr_size_of_integer = _courier_SizeOfInteger;

  new->pr_output_shortinteger = _courier_OutputShortInteger;
  new->pr_input_shortinteger = _courier_InputShortInteger;
  new->pr_size_of_shortinteger = _courier_SizeOfShortInteger;

  new->pr_output_longinteger = _courier_OutputLongInteger;
  new->pr_input_longinteger = _courier_InputLongInteger;
  new->pr_size_of_longinteger = _courier_SizeOfLongInteger;

  new->pr_output_cardinal = _courier_OutputCardinal;
  new->pr_input_cardinal = _courier_InputCardinal;
  new->pr_size_of_cardinal = _courier_SizeOfCardinal;

  new->pr_output_shortcardinal = _courier_OutputShortCardinal;
  new->pr_input_shortcardinal = _courier_InputShortCardinal;
  new->pr_size_of_shortcardinal = _courier_SizeOfShortCardinal;

  new->pr_output_longcardinal = _courier_OutputLongCardinal;
  new->pr_input_longcardinal = _courier_InputLongCardinal;
  new->pr_size_of_longcardinal = _courier_SizeOfLongCardinal;

  new->pr_output_real = _courier_OutputReal;
  new->pr_input_real = _courier_InputReal;
  new->pr_size_of_real = _courier_SizeOfReal;

  new->pr_output_shortreal = _courier_OutputShortReal;
  new->pr_input_shortreal = _courier_InputShortReal;
  new->pr_size_of_shortreal = _courier_SizeOfShortReal;

  new->pr_output_longreal = _courier_OutputLongReal;
  new->pr_input_longreal = _courier_InputLongReal;
  new->pr_size_of_longreal = _courier_SizeOfLongReal;

  new->pr_output_optional = _courier_OutputOptional;
  new->pr_input_optional = _courier_InputOptional;
  new->pr_size_of_optional = _courier_SizeOfOptional;

  new->pr_output_enum_code = _courier_OutputEnumeration;
  new->pr_input_enum_code = _courier_InputEnumeration;
  new->pr_size_of_enum_code = _courier_SizeOfEnumeration;

  new->pr_output_byte = _courier_OutputByte;
  new->pr_input_byte = _courier_InputByte;
  new->pr_size_of_byte = _courier_SizeOfByte;

  new->pr_output_character = _courier_OutputShortCardinal;
  new->pr_input_character = _courier_InputShortCardinal;
  new->pr_size_of_character = _courier_SizeOfShortCardinal;

  new->pr_output_boolean =
	(void (*)(ilu_Call,ilu_boolean,ilu_Error*))
	_courier_OutputCardinal;
  new->pr_input_boolean =
	(void (*)(ilu_Call,ilu_boolean *,ilu_Error*))
	_courier_InputCardinal;
  new->pr_size_of_boolean =
	(ilu_cardinal (*)(ilu_Call,ilu_boolean,ilu_Error*))
	_courier_SizeOfCardinal;

  new->pr_output_shortchar = _courier_OutputShortChar;
  new->pr_input_shortchar = _courier_InputShortChar;
  new->pr_size_of_shortchar = _courier_SizeOfShortChar;

  new->pr_output_string = _courier_OutputString;
  new->pr_input_string = _courier_InputString;
  new->pr_size_of_string = _courier_SizeOfString;

  new->pr_output_wstring = _ilu_OutputWString;
  new->pr_input_wstring = _ilu_InputWString;
  new->pr_size_of_wstring = _ilu_SizeOfWString;

  new->pr_output_bytes = _courier_OutputBytes;
  new->pr_input_bytes = _courier_InputBytes;
  new->pr_size_of_bytes = _courier_SizeOfBytes;

  new->pr_output_opaque = _courier_OutputOpaque;
  new->pr_input_opaque = _courier_InputOpaque;
  new->pr_size_of_opaque = _courier_SizeOfOpaque;

  new->pr_output_wstringvec = _ilu_OutputWStringVec;
  new->pr_input_wstringvec = _ilu_InputWStringVec;
  new->pr_size_of_wstringvec = _ilu_SizeOfWStringVec;

  new->pr_output_object_id = _ilu_OutputObjectID;
  new->pr_input_object_id = _ilu_InputObjectID;
  new->pr_size_of_object_id = _ilu_SizeOfObjectID;

  new->pr_output_stringvec =
	(void (*)(ilu_Call,ilu_string,ilu_cardinal,ilu_Error*))
	_courier_OutputOpaque;
  new->pr_input_stringvec =
	(void (*)(ilu_Call,ilu_string *,ilu_cardinal,ilu_Error*))
	_courier_InputOpaque;
  new->pr_size_of_stringvec =
	(ilu_cardinal (*)(ilu_Call,ilu_string,ilu_cardinal,ilu_Error*))
	_courier_SizeOfOpaque;

  new->pr_output_sequence = _courier_OutputSequence;
  new->pr_output_sequence_mark = _courier_OutputSequenceMark;
  new->pr_input_sequence = _courier_InputSequence;
  new->pr_input_sequence_mark = _courier_InputSequenceMark;
  new->pr_end_sequence = _courier_EndSequence;
  new->pr_size_of_sequence = _courier_SizeOfSequence;

  new->pr_output_record = _courier_OutputRecord;
  new->pr_input_record = _courier_InputRecord;
  new->pr_end_record = _courier_EndRecord;
  new->pr_size_of_record = _courier_SizeOfRecord;

  new->pr_output_array = _courier_OutputArray;
  new->pr_input_array = _courier_InputArray;
  new->pr_end_array = _courier_EndArray;
  new->pr_size_of_array = _courier_SizeOfArray;

  new->pr_output_union = _courier_OutputUnion;
  new->pr_input_union = _courier_InputUnion;
  new->pr_end_union = _courier_EndUnion;
  new->pr_size_of_union = _courier_SizeOfUnion;

#ifdef ADD_VARIANT_SUPPORT

  new->pr_output_pickle = _ilu_OutputPickle;
  new->pr_input_pickle = _ilu_InputPickle;
  new->pr_size_of_pickle = _ilu_SizeOfPickle;

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

  new->pr_form_handle = _courier_FormProtocolHandle;

  new->pr_read_header = _courier_ReadHeader;
  new->pr_delay_interp = _courier_DelayInterp;
  new->pr_resume_interp = _courier_ResumeInterp;
  new->pr_abandon_delayed_interp = _courier_AbandonDelayedInterp;
  new->pr_discard_input = _courier_DiscardMessage;
  new->pr_discard_output = _courier_DiscardMessage;
  
  new->pr_interpret_request = _courier_InterpretRequest;
  new->pr_request_read = _courier_MessageRead;
  new->pr_interpret_reply = _courier_InterpretReply;
  new->pr_reply_read = _courier_MessageRead;

  new->pr_create_data_block = _courier_CreateDataBlock;
  new->pr_free_data_block = (void (*)(void *)) _courier_FreeDataBlock;
  new->pr_conn_closing = NULLFN;

  return (new);
}

/*L1_sup < prmu*/

ilu_Protocol _ilu_courier_Protocol(ilu_ProtocolInfo pinfo,
				   ilu_Error *err)
{
  /*L1 >= {prmu}*/
  static ilu_Protocol StandardCourier = NULL;
  _ilu_AcquireMutex(ilu_prmu);
  if (StandardCourier == NULL)
    StandardCourier = _courier_NewCourier();
  _ilu_ReleaseMutex(ilu_prmu);
  ILU_CLER(*err);
  return (StandardCourier);
}

