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
/* $Id: sunrpc.c,v 1.228 1999/08/12 17:54:47 janssen Exp $ */
/* Last edited by Mike Spreitzer October 16, 1998 11:07 pm PDT */

#include "iluntrnl.h"
#include "sunrpc.h"

#include "call.h"
#include "iluprotocol.h"
#include "connect.h"
#include "ilutransport.h"
#include "port.h"
#include "object.h"
#include "type.h"
#include "method.h"
#include "mooring.h"

#ifdef ILU_TYPEUID_V2
#define ILU_SUNRPC_PROGRAM_NUMBER	0x00061a7a
#else
#define ILU_SUNRPC_PROGRAM_NUMBER	0x00061a79
#endif /* def ILU_TYPEUID_V2 */
#define OLD_ILU_SUNRPC_PROGRAM_NUMBER	0x00061a78
#define OLDER_ILU_SUNRPC_PROGRAM_NUMBER	0x31000400

#define ILU_UNSECURED_GENERIC_IDENTITY	0x000493E2	/* AUTH_UGEN, in Sun Registry */

#define ODD(x)		(((x)&0x1)!=0)
#define EVEN(x)		(((x)&0x1)==0)  
#define PADDED_SIZE(x)	((((unsigned) (x))+3) & (~0x3))
  
#ifdef WORDS_BIGENDIAN
#define THIS_ARCH_IS_BIGENDIAN	ilu_TRUE
#else
#define THIS_ARCH_IS_BIGENDIAN	ilu_FALSE
#endif

/*L1, L2 unconstrained*/

#define sunrpc_transport(call)	((call)->ca_prTrans)
/*
 * sunrpc_transport(call)'s xmu == call's conn's iomu;
 * sunrpc_transport(call)'s ymu == (call->ca_prbit1 ? conn's iomu :
 * conn's waitmu).
 */

static          ilu_boolean
_sunrpc_InitCall(ilu_Call call,
		 ILU_ERRS((IoErrs)) * err)
{
  if (!transport_boundaried(connection_transport(call->ca_connection)))
    return ILU_ERR_CONS1(comm_failure, err, minor, ilu_cfm_tr_non_boundaried, ilu_FALSE);
  call->ca_prTrans = connection_transport(call->ca_connection);
  call->ca_prbit1 = ilu_FALSE;
  return ILU_CLER(*err);
}

/*Main Invariant holds; L2 disjoint {call's conn}*/
static          ilu_boolean
_sunrpc_FinishCall(ilu_Call call, ILU_ERRS((IoErrs)) * err)
{
  ILU_CLER(*err);
  if (!ilu_Check(call->ca_prTrans, err))
    return ilu_FALSE;
  if (call->ca_prbit1) {
    ilu_integer     cdfd;
    if (ilu_CloseTransport(call->ca_prTrans, &cdfd, err)) {
      /*
       * XXX We'd also like to check upon failure, but have no place
       * to report failure of the check.
       */
      if (!ilu_Check(cdfd == 0, err))
	return ilu_FALSE;
    } else {
      ILU_ERR_SWITCH(*err) {
	ILU_ERR_CASE(bad_locks, x) {
	  ILU_HANDLED(*err);
	  (void) ilu_Check(ilu_FALSE, err);
	}
      } ILU_ERR_ENDSWITCH;
    }
  }
  return ILU_ERROK(*err);
}

/*L1, L2 unconstrained*/

static ilu_refany 
_sunrpc_CreateNonBatchingNonConcurrentDataBlock(ilu_ProtocolInfo pinfo,
						ILU_ERRS((no_memory)) * err)
{
  SunRPC  *new = (SunRPC *) ilu_MallocE(sizeof(*new), err);
  if (ILU_ERRNOK(*err))
    return NIL;

  new->batching = ilu_FALSE;
  new->concurrent = ilu_FALSE;
  return ((ilu_refany) new);
}

static ilu_refany 
_sunrpc_CreateBatchingNonConcurrentDataBlock(ilu_ProtocolInfo pinfo,
					     ILU_ERRS((no_memory)) * err)
{
  SunRPC  *new = (SunRPC *) ilu_MallocE(sizeof(*new), err);
  if (ILU_ERRNOK(*err))
    return NIL;

  new->batching = ilu_TRUE;
  new->concurrent = ilu_FALSE;
  return ((ilu_refany) new);
}

static ilu_refany 
_sunrpc_CreateNonBatchingConcurrentDataBlock(ilu_ProtocolInfo pinfo,
					     ILU_ERRS((no_memory)) * err)
{
  SunRPC  *new = (SunRPC *) ilu_MallocE(sizeof(*new), err);
  if (ILU_ERRNOK(*err))
    return NIL;

  new->batching = ilu_FALSE;
  new->concurrent = ilu_TRUE;
  return ((ilu_refany) new);
}

static ilu_refany 
_sunrpc_CreateBatchingConcurrentDataBlock(ilu_ProtocolInfo pinfo,
					  ILU_ERRS((no_memory)) * err)
{
  SunRPC  *new = (SunRPC *) ilu_MallocE(sizeof(*new), err);
  if (ILU_ERRNOK(*err))
    return NIL;

  new->batching = ilu_TRUE;
  new->concurrent = ilu_TRUE;
  return ((ilu_refany) new);
}

static void _sunrpc_FreeDataBlock(ilu_refany d)
{
  ilu_free(d);
}

/**********************************************************************
  Code to figure Sun RPC program # given ILU type code
***********************************************************************/

/*L1 >= {prmu}*/

static HashTable ProgramNumberHashTable = NIL;
/* program number & version -> sunrpcinfo* */

static HashTable RegistryHashTable = NIL;
/* type unique_id -> sunrpcinfo* */

static sunrpcinfo *_sunrpc_AddClassInformation(ilu_Class);

static ilu_cardinal _sunrpc_HashSunRPC (sunrpcinfo *key, ilu_cardinal size)
{
  return ((key->sui_pnumber + key->sui_version) % size);
}

static ilu_boolean _sunrpc_CompareSunRPC (sunrpcinfo *key1, sunrpcinfo *key2)
{
  return ((key1->sui_pnumber == key2->sui_pnumber) && (key1->sui_version == key2->sui_version));
}

typedef ilu_cardinal (*ILU_hash_proc)(ilu_refany, ilu_cardinal);
typedef ilu_boolean (*ILU_find_proc)(ilu_refany, ilu_refany);

static void _sunrpc_EnsureRegistries(void)
{
  if (RegistryHashTable == NIL)
    {
      RegistryHashTable = ilu_hash_MakeNewTable (137, NULLFN, NULLFN);
      ProgramNumberHashTable =
	ilu_hash_MakeNewTable (137,
				(ILU_hash_proc)_sunrpc_HashSunRPC,
				(ILU_find_proc)_sunrpc_CompareSunRPC);
      _sunrpc_AddClassInformation (_ilu_rootClass);
      _sunrpc_AddClassInformation (_ilu_GcCallbackClass);
    }
}

static sunrpcinfo * _sunrpc_AddClassInformation (ilu_Class class)
{
  char *next = NIL;
  sunrpcinfo *s = (sunrpcinfo *) ilu_must_malloc(sizeof(sunrpcinfo));

  _sunrpc_EnsureRegistries();

  s->sui_class = class;
  s->sui_type_id = class->cl_unique_id;
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
      if (! (strncmp(class->cl_singleton, "sunrpc_2_", 9) == 0 &&
	       (s->sui_pnumber = _ilu_atoi(class->cl_singleton + 9, &next),
		(s->sui_pnumber > 0 && (next != NIL) && (*next == '_'))) &&
	       (s->sui_version = _ilu_atoi(next + 1, NIL),
		(s->sui_version > 0))))
	{
	  ILU_ERRPRINTF("(ILU:sunrpc)  Couldn't determine Sun RPC program number for ILU class \"%s\", given native Sun RPC info \"%s\".\n",
		   class->cl_name, class->cl_singleton);
	  return (NIL);
	}
    }
  else
    {
      s->sui_pnumber = ILU_SUNRPC_PROGRAM_NUMBER;
      s->sui_version = ilu_CRC32 ((ilu_bytes) (class->cl_unique_id), strlen(class->cl_unique_id));
    }
  _ilu_Assert((int) ilu_hash_AddToTable(RegistryHashTable,
				   s->sui_type_id, s),
	      "SunRPC AddToTable RegistryHashTable");
  _ilu_Assert((int) ilu_hash_AddToTable(ProgramNumberHashTable, s, s),
	      "SunRPC AddToTable ProgramNumberHashTable");
  return (s);
}

static sunrpcinfo *_sunrpc_SunRPCInformationForClass (ilu_Class class)
{
  sunrpcinfo *s = NIL;

  _sunrpc_EnsureRegistries();

  s = (sunrpcinfo *) ilu_hash_FindInTable (RegistryHashTable,
					    class->cl_unique_id);
  if (s == NIL)
    s = _sunrpc_AddClassInformation (class);

  ILU_NOTE(SUNRPC_DEBUG,
	("%s \"%s:%s\", pnumber is 0x%lx, version is %lu.\n",
	 "ILU: _sunrpc_SunRPCInformationForClass:  Class",
	 class->cl_name, class->cl_unique_id,
	 (s == NIL) ? 0L : s->sui_pnumber,
	 (s == NIL) ? 0L : s->sui_version));

  return (s);
}

static void AddClass (ilu_Class c, ilu_refany junk)
{
  _sunrpc_SunRPCInformationForClass (c);
}

static sunrpcinfo *_sunrpc_ClassFromProgramNumber (ilu_cardinal pnumber,
						   ilu_cardinal version,
						   ilu_boolean try_adds)
{
  sunrpcinfo dummy;
  sunrpcinfo *s = NIL;

  _sunrpc_EnsureRegistries();

  if (pnumber == OLD_ILU_SUNRPC_PROGRAM_NUMBER || pnumber == OLDER_ILU_SUNRPC_PROGRAM_NUMBER)
    {
      ILU_ERRPRINTF("The specified Sun RPC program number of 0x%x indicates that you are communicating\n"
		    "with an old ILU client, pre-%s.  You should update that service to ILU %s.\n",
		    pnumber, ilu_GetILUVersion(), ilu_GetILUVersion());
      dummy.sui_pnumber = ILU_SUNRPC_PROGRAM_NUMBER;
    }
  else
    dummy.sui_pnumber = pnumber;
  dummy.sui_version = version;
  s = (sunrpcinfo *) ilu_hash_FindInTable (ProgramNumberHashTable,
					      (ilu_refany) &dummy);
  if (s == NIL)
    {
      if (try_adds)
	{
	  _ilu_EnumerateClasses (AddClass, NIL);
	  return _sunrpc_ClassFromProgramNumber (pnumber, version, ilu_FALSE);
	}
      else
	{
	  ILU_ERRPRINTF("(ILU:_sunrpc_ClassFromProgramNumber):  Couldn't find class for program number 0x%lx, version %lu\n",
		   pnumber, version);
	}
    }
  else
    {
      ILU_NOTE(SUNRPC_DEBUG,
	    ("%s \"%s:%s\", pnumber is 0x%lx, version is %lu.\n",
	     "ILU: _sunrpc_ClassFromProgramNumber:  Class",
	     s->sui_class->cl_name, s->sui_class->cl_unique_id,
	     s->sui_pnumber, s->sui_version));
    }

  return (s);
}

/*======================================================================*/
/*======================== Basic I/O code ==============================*/
/*======================================================================*/

#define INPUT_ERROR		1
#define OUTPUT_ERROR		2

static ilu_cardinal SunRPCMaxStringSize = 0;

/*L1, L2, Main unconstrained (this is only for calling from debugger)*/
static ilu_cardinal ilu_sunrpc_SetMaxStringSize (ilu_cardinal size)
{
  ilu_cardinal old_size = SunRPCMaxStringSize;
  if (size > 0)
    SunRPCMaxStringSize = size;
  return (old_size);  
}

/*L1, L2 unconstrained for sizing, end*/
/*Call-OHi(call) for output*/
/*Call-IHi(call) for input*/

/* ==================== cardinal ==================== */

#define SWAP_WORD(a) ( ((a) << 24) | \
                      (((a) << 8) & 0x00ff0000) | \
                      (((a) >> 8) & 0x0000ff00) | \
        ((ilu_cardinal)(a) >>24) )

#ifdef WORDS_BIGENDIAN
#define cardout(buf,l) *((ilu_cardinal *) buf) = l;
#else
#define cardout(buf,l)                  \
        *((ilu_cardinal *)buf) = SWAP_WORD((l));
#endif

#define Output_Cardinal_Work(call,l,err) \
{					\
  register unsigned char *buf;		\
  ILU_CLER(*(err));			\
  buf = transport_get_output_buffer(sunrpc_transport(call), 4, err);	\
  if (ILU_ERROK(*(err))) {		\
    cardout(buf,l)			\
  }					\
}					\

static void
_sunrpc_OutputCardinal(ilu_Call call, ilu_cardinal l,
		       ILU_ERRS((IoErrs)) * err)
{
  Output_Cardinal_Work(call, l, err);
}

#define Output_Cardinal(c,l,e)	Output_Cardinal_Work(c,l,e)

static void
_sunrpc_InputCardinal(ilu_Call call, ilu_cardinal * i,
		      ILU_ERRS((IoErrs)) * err)
{
  register unsigned char *buf;
  buf = transport_get_input_buffer(sunrpc_transport(call), 4, err);
  if (buf != NIL) {
#ifdef WORDS_BIGENDIAN
    *i = *((ilu_cardinal *) buf);
#else                           /* not bigendian */
    register ilu_cardinal tmp = *((ilu_cardinal *) buf);
    *i = SWAP_WORD(tmp);
#endif
  }
}

/*ARGSUSED*/
static ilu_cardinal _sunrpc_SizeOfCardinal (ilu_Call call, ilu_cardinal i,
		      ILU_ERRS((IoErrs)) * err)
{
  ILU_CLER(*err);
  return (4);
}

/* ==================== integer ==================== */

static void 
_sunrpc_OutputInteger(ilu_Call call, ilu_integer i,
		      ILU_ERRS((IoErrs)) * err)
{
  Output_Cardinal(call, (ilu_cardinal) i, err);
}

static void
_sunrpc_InputInteger(ilu_Call call, ilu_integer * i,
		     ILU_ERRS((IoErrs)) * err)
{
  _sunrpc_InputCardinal (call, (ilu_cardinal *) i, err);
}

/*ARGSUSED*/
static ilu_cardinal _sunrpc_SizeOfInteger (ilu_Call call, ilu_integer i,
		     ILU_ERRS((IoErrs)) * err)
{
  ILU_CLER(*err);
  return (4);
}

/* ==================== short integer ==================== */

static void 
_sunrpc_OutputShortInteger(ilu_Call call, ilu_shortinteger i,
			   ILU_ERRS((IoErrs)) * err)
{
  _sunrpc_OutputInteger(call, (ilu_integer) i, err);
}

static void
_sunrpc_InputShortInteger(ilu_Call call, ilu_shortinteger * i,
			  ILU_ERRS((IoErrs)) * err)
{
  ilu_integer     l = 0;

  _sunrpc_InputInteger (call, &l, err);
  if (ILU_ERROK(*err))
    *i = (ilu_shortinteger) l;
  return;
}

/*ARGSUSED*/
static ilu_cardinal _sunrpc_SizeOfShortInteger (ilu_Call call, ilu_shortinteger i,
			  ILU_ERRS((IoErrs)) * err)
{
  ILU_CLER(*err);
  return (4);
}

/* ==================== long integer ==================== */

static void
_sunrpc_OutputLongInteger(ilu_Call call, ilu_longinteger i,
			  ILU_ERRS((IoErrs)) * err)
{
  _sunrpc_OutputInteger(call, ILU_LONGINT_HIGH_WORD(&i), err);
  if (ILU_ERROK(*err))
    Output_Cardinal(call, ILU_LONGINT_LOW_WORD(&i), err);
}

static void 
_sunrpc_InputLongInteger(ilu_Call call, ilu_longinteger * i,
			 ILU_ERRS((IoErrs)) * err)
{
  _sunrpc_InputInteger (call, &ILU_LONGINT_HIGH_WORD(i), err);
  if (ILU_ERROK(*err))
    _sunrpc_InputCardinal (call, &ILU_LONGINT_LOW_WORD(i), err);
  return;
}

/*ARGSUSED*/
static ilu_cardinal _sunrpc_SizeOfLongInteger (ilu_Call call, ilu_longinteger i,
			 ILU_ERRS((IoErrs)) * err)
{
  ILU_CLER(*err);
  return (8);
}

/* ==================== short cardinal ==================== */

static void 
_sunrpc_OutputShortCardinal(ilu_Call call, ilu_shortcardinal i,
			    ILU_ERRS((IoErrs)) * err)
{
  Output_Cardinal(call, (ilu_cardinal) i, err);
}

static void
_sunrpc_InputShortCardinal(ilu_Call call, ilu_shortcardinal * i,
			   ILU_ERRS((IoErrs)) * err)
{
  ilu_cardinal    l = 0;

  _sunrpc_InputCardinal (call, &l, err);
  if (ILU_ERROK(*err))
    *i = (ilu_shortcardinal) (l & 0xFFFF);
}

/*ARGSUSED*/
static ilu_cardinal 
_sunrpc_SizeOfShortCardinal(ilu_Call call, ilu_shortcardinal i,
			    ILU_ERRS((IoErrs)) * err)
{
  ILU_CLER(*err);
  return (4);
}

/* ==================== long cardinal ==================== */

static void 
_sunrpc_OutputLongCardinal(ilu_Call call, ilu_longcardinal i,
			   ILU_ERRS((IoErrs)) * err)
{
  Output_Cardinal(call, ILU_LONGCARD_HIGH_WORD(&i), err);
  if (ILU_ERROK(*err))
    Output_Cardinal(call, ILU_LONGCARD_LOW_WORD(&i), err);
}

static void
_sunrpc_InputLongCardinal(ilu_Call call, ilu_longcardinal * i,
			  ILU_ERRS((IoErrs)) * err)
{
  _sunrpc_InputCardinal (call, &ILU_LONGCARD_HIGH_WORD(i), err);
  if (ILU_ERROK(*err))
    _sunrpc_InputCardinal (call, &ILU_LONGCARD_LOW_WORD(i), err);
  return;
}

/*ARGSUSED*/
static ilu_cardinal _sunrpc_SizeOfLongCardinal (ilu_Call call, ilu_longcardinal i,
			    ILU_ERRS((IoErrs)) * err)
{
  ILU_CLER(*err);
  return (8);
}

/* ==================== real ==================== */

static void 
_sunrpc_OutputReal(ilu_Call call, double d,
		   ILU_ERRS((IoErrs)) * err)
{
  double          l2 = d;

#ifdef WORDS_BIGENDIAN
  Output_Cardinal(call, ((ilu_cardinal *) (&l2))[0], err);
  if (ILU_ERROK(*err))
    Output_Cardinal(call, ((ilu_cardinal *) (&l2))[1], err);
#else
  Output_Cardinal(call, ((ilu_cardinal *) (&l2))[1], err);
  if (ILU_ERROK(*err))
    Output_Cardinal(call, ((ilu_cardinal *) (&l2))[0], err);
#endif

}

static void 
_sunrpc_InputReal(ilu_Call call, double *d,
		  ILU_ERRS((IoErrs)) * err)
{
  double l2;

#ifdef WORDS_BIGENDIAN
  _sunrpc_InputCardinal(call, ((ilu_cardinal *) &l2), err);
  if (ILU_ERROK(*err))
    {
      _sunrpc_InputCardinal(call, ((ilu_cardinal *) &l2) + 1, err);
      if (ILU_ERROK(*err)) *d = l2;
    }
#else
  _sunrpc_InputCardinal(call, ((ilu_cardinal *) &l2) + 1, err);
  if (ILU_ERROK(*err))
    {
      _sunrpc_InputCardinal(call, ((ilu_cardinal *) &l2), err);
      if (ILU_ERROK(*err)) *d = l2;
    }
#endif

}

/*ARGSUSED*/
static ilu_cardinal _sunrpc_SizeOfReal (ilu_Call call, double d,
		  ILU_ERRS((IoErrs)) * err)
{
  ILU_CLER(*err);
  return (8);
}

/* ==================== long real ==================== */

static void 
_sunrpc_OutputLongReal(ilu_Call call, ilu_longreal d,
		       ILU_ERRS((IoErrs)) * err)
{
  transport_write_bytes(sunrpc_transport(call), (ilu_bytes) &d, 16, err);
}

static void
_sunrpc_InputLongReal(ilu_Call call, ilu_longreal * d,
		      ILU_ERRS((IoErrs)) * err)
{
  (void) transport_read_bytes(sunrpc_transport(call), (ilu_bytes) d, 16,
			      err);
  return;
}

/*ARGSUSED*/
static ilu_cardinal _sunrpc_SizeOfLongReal (ilu_Call call, ilu_longreal d,
		  ILU_ERRS((IoErrs)) * err)
{
  ILU_CLER(*err);
  return (16);
}

/* ==================== short real ==================== */

static void 
_sunrpc_OutputShortReal(ilu_Call call, float f,
			ILU_ERRS((IoErrs)) * err)
{
  float           f2;

  f2 = f;
  Output_Cardinal(call, *((ilu_cardinal *) & f2), err);
}

static void 
_sunrpc_InputShortReal(ilu_Call call, float *f,
		       ILU_ERRS((IoErrs)) * err)
{
  _sunrpc_InputCardinal (call, (ilu_cardinal *) f, err);
}

/*ARGSUSED*/
static ilu_cardinal _sunrpc_SizeOfShortReal (ilu_Call call, float d,
		       ILU_ERRS((IoErrs)) * err)
{
  ILU_CLER(*err);
  return (4);
}

/* ==================== bytes ==================== */

static void
_sunrpc_OutputBytes(ilu_Call call, ilu_bytes s, ilu_cardinal len,
		    ilu_cardinal limit, ILU_ERRS((IoErrs)) * err)
{
  if (limit > 0 && len > limit) {
    ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_sequenceLimit, 0);
    return;
  } else {
    ilu_cardinal    paddedlen = PADDED_SIZE(len);
    Output_Cardinal(call, len, err);
    if (paddedlen == 0)
      return;
    if (ILU_ERROK(*err))
      (void) transport_write_bytes(sunrpc_transport(call), s,
				   paddedlen, err);
  }
}

static void
InputBytes(ilu_Call call, ilu_bytes * s, ilu_cardinal * len,
	   ilu_cardinal limit, ILU_ERRS((IoErrs)) * err,
	   ilu_boolean string_p)
{
  ilu_cardinal    size;

  ILU_CLER(*err);
  if (_sunrpc_InputCardinal(call, len, err), ILU_ERROK(*err)) {
    if (limit > 0 && *len > limit) {
      ILU_ERR_CONS1(marshal, err, minor, ilu_mm_sequenceLimit, 0);
      return;
    }
    if (!string_p && *len == 0)
      {
	*s = NIL;
	return;
      }
    size = PADDED_SIZE(*len);
    *s = ilu_malloc(size + 4);
    if (*s == NIL) {
      ILU_ERR_CONS1(no_memory, err, nbytes, size, 0);
      return;
    }
    (void) transport_read_bytes(sunrpc_transport(call), *s, size, err);
    (*s)[*len] = 0;
    /* ... so this can be used to input a C string */
  }
  return;
}

static void
_sunrpc_InputBytes(ilu_Call call, ilu_bytes * s, ilu_cardinal * len,
		   ilu_cardinal limit, ILU_ERRS((IoErrs)) * err)
{
  InputBytes (call, s, len, limit, err, ilu_FALSE);
}

  /*ARGSUSED*/
static          ilu_cardinal
_sunrpc_SizeOfBytes(ilu_Call call, ilu_bytes i, ilu_cardinal l,
		    ilu_cardinal limit, ILU_ERRS((IoErrs)) * err)
{
  if ((limit > 0) && (l > limit))
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_sequenceLimit, 0);
  if (SunRPCMaxStringSize > 0 && l > SunRPCMaxStringSize)
    return ILU_ERR_CONS1(imp_limit, err, minor, ilu_ilm_strlen, 0);
  ILU_CLER(*err);
  return (4 + PADDED_SIZE(l));
}

/* ==================== string ==================== */

static void 
_sunrpc_OutputString(ilu_Call call, void * s, ilu_cardinal len,
		     ilu_cardinal limit, ilu_cardinal expected_encoding,
		     ilu_cardinal current_encoding,
		     ILU_ERRS((IoErrs)) * err)
{
  if ((expected_encoding != 0) && (expected_encoding != current_encoding)) {
    ILU_ERR_CONS1(imp_limit, err, minor, ilu_ilm_unsupported_charset_encoding, 0);
    return;
  } else {
    if (expected_encoding == 0) {
      _sunrpc_OutputCardinal(call, expected_encoding, err);
      if (ILU_ERRNOK(*err)) return;
    };
    _sunrpc_OutputBytes(call, (ilu_bytes) s, len, limit, err);
  }
}

static void
_sunrpc_InputString(ilu_Call call, void ** s, ilu_cardinal * len,
		    ilu_cardinal limit, ilu_cardinal expected_encoding,
		    ilu_cardinal *actual_encoding,
		    ILU_ERRS((IoErrs)) * err)
{
  if (expected_encoding == 0) {
    _sunrpc_InputCardinal(call, actual_encoding, err);
    if (ILU_ERRNOK(*err))
      return;
  } else {
    *actual_encoding = expected_encoding;
  }
  InputBytes (call, (ilu_bytes *) s, len, limit, err, ilu_TRUE);
}

/*ARGSUSED*/
static ilu_cardinal 
_sunrpc_SizeOfString(ilu_Call call, void *i, ilu_cardinal l,
		     ilu_cardinal limit, 
		     ilu_cardinal current_encoding,
		     ilu_cardinal expected_encoding,
		     ILU_ERRS((IoErrs)) * err)
{
  if ((expected_encoding != 0) && (expected_encoding != current_encoding))
    return ILU_ERR_CONS1(imp_limit, err, minor, ilu_ilm_unsupported_charset_encoding, 0);
  if ((limit > 0) && (l > limit))
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_sequenceLimit, 0);
  if (SunRPCMaxStringSize > 0 && l > SunRPCMaxStringSize)
    return ILU_ERR_CONS1(imp_limit, err, minor, ilu_ilm_strlen, 0);
  ILU_CLER(*err);
  return (((expected_encoding == 0) ? 4 : 0) + 4 + PADDED_SIZE(l));
}

/* ==================== byte ==================== */

static void 
_sunrpc_OutputByte(ilu_Call call, ilu_byte b, ILU_ERRS((IoErrs)) * err)
{
  Output_Cardinal(call, (ilu_cardinal) b, err);
}

static void 
_sunrpc_InputByte(ilu_Call call, ilu_byte * b,
		  ILU_ERRS((IoErrs)) * err)
{
  ilu_cardinal    l = 0;

  _sunrpc_InputCardinal (call, &l, err);
  if (ILU_ERROK(*err))
    *b = (ilu_byte) (l & 0xFF);
}

/*ARGSUSED*/
static          ilu_cardinal
_sunrpc_SizeOfByte(ilu_Call call, ilu_byte i, ILU_ERRS((IoErrs)) * err)
{
  ILU_CLER(*err);
  return (4);
}

/* ==================== boolean ==================== */

static void 
_sunrpc_OutputBoolean(ilu_Call call, ilu_boolean b, ILU_ERRS((IoErrs)) * err)
{
  Output_Cardinal(call, (b != ilu_FALSE) ? 1 : 0, err);
}

static void 
_sunrpc_InputBoolean(ilu_Call call, ilu_boolean * b,
		     ILU_ERRS((IoErrs)) * err)
{
  ilu_cardinal    l = 0;

  _sunrpc_InputCardinal (call, &l, err);
  if (ILU_ERROK(*err))
    *b = ((l != 0) ? ilu_TRUE : ilu_FALSE);
}

/*ARGSUSED*/
static          ilu_cardinal
_sunrpc_SizeOfBoolean(ilu_Call call, ilu_boolean i, ILU_ERRS((IoErrs)) * err)
{
  ILU_CLER(*err);
  return (4);
}

/* ==================== short char ==================== */

static void
_sunrpc_OutputShortChar(ilu_Call call, ilu_shortcharacter b,
			ILU_ERRS((IoErrs)) * err)
{
  _sunrpc_OutputByte(call, (ilu_byte) b, err);
}

static void
_sunrpc_InputShortChar(ilu_Call call, ilu_shortcharacter * b,
		       ILU_ERRS((IoErrs)) * err)
{
  _sunrpc_InputByte(call, (ilu_byte *) b, err);
}

/* ARGSUSED */
static          ilu_cardinal
_sunrpc_SizeOfShortChar(ilu_Call call, ilu_shortcharacter i,
			ILU_ERRS((IoErrs)) * err)
{
  return _sunrpc_SizeOfByte(call, (ilu_byte) i, err);
}

/* ==================== opaque ==================== */

static void
_sunrpc_OutputOpaque(ilu_Call call, ilu_bytes o, ilu_cardinal len,
		     ILU_ERRS((IoErrs)) * err)
{
  ilu_cardinal    paddedlen = PADDED_SIZE(len);
  (void) transport_write_bytes(sunrpc_transport(call), o, paddedlen, err);
}

static void
_sunrpc_InputOpaque(ilu_Call call, ilu_bytes * o, ilu_cardinal len,
		    ILU_ERRS((IoErrs)) * err)
{
  if (*o == NIL) {
    if ((*o = ilu_malloc(PADDED_SIZE(len))) == NIL) {
      (void) ILU_ERR_CONS1(no_memory, err, nbytes, PADDED_SIZE(len), 0);
      return;
    }
  }
  (void) transport_read_bytes(sunrpc_transport(call), *o,
			      PADDED_SIZE(len), err);
}

static          ilu_cardinal
_sunrpc_SizeOfOpaque(ilu_Call call, ilu_bytes o, ilu_cardinal len,
		     ILU_ERRS((IoErrs)) * err)
{
  ILU_CLER(*err);
  return (PADDED_SIZE(len));
}

/* ==================== optional ==================== */

static void 
_sunrpc_OutputOptional(ilu_Call call, ilu_boolean i,
		       ilu_Type prefix, ILU_ERRS((IoErrs)) * err)
{
  Output_Cardinal(call, (ilu_cardinal) i, err);
}

static void 
_sunrpc_InputOptional(ilu_Call call, ilu_boolean *i,
		      ilu_Type prefix, ILU_ERRS((IoErrs)) * err)
{
  ilu_cardinal    i2 = 0;

  _sunrpc_InputCardinal (call, &i2, err);
  if (ILU_ERROK(*err)) *i = ((i2 == 0) ? ilu_FALSE : ilu_TRUE);
}

/*ARGSUSED*/
static ilu_cardinal
  _sunrpc_SizeOfOptional (ilu_Call call, ilu_boolean i,
			  ilu_Type prefix, ILU_ERRS((IoErrs)) * err)
{
  ILU_CLER(*err);
  return (4);
}

/* ==================== enumeration ==================== */

static void 
_sunrpc_OutputEnumeration(ilu_Call call, ilu_shortcardinal i,
			  ilu_Type prefix,
			  ILU_ERRS((IoErrs)) * err)
{
  Output_Cardinal(call, (ilu_cardinal) i, err);
}

static void 
_sunrpc_InputEnumeration(ilu_Call call, ilu_shortcardinal * i,
			 ilu_Type prefix,
			 ILU_ERRS((IoErrs)) * err)
{
  _sunrpc_InputShortCardinal (call, i, err);
}

/*ARGSUSED*/
static ilu_cardinal _sunrpc_SizeOfEnumeration (ilu_Call call, ilu_shortcardinal i,
					       ilu_Type prefix, ILU_ERRS((IoErrs)) * err)
{
  ILU_CLER(*err);
  return (4);
}

/* ==================== sequence ==================== */

static void
_sunrpc_OutputSequence(ilu_Call c, ilu_cardinal sequenceLength,
		       ilu_cardinal limit, ilu_Type prefix, ILU_ERRS((IoErrs)) * err)
{
  if (limit > 0 && sequenceLength > limit)
    {
      ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_sequenceLimit, 0);
    }
  else  
    Output_Cardinal(c, sequenceLength, err);
}

static void 
_sunrpc_OutputSequenceMark(ilu_Call c,
			   ilu_cardinal extent,
			   ILU_ERRS((IoErrs)) * err)
{
  ILU_CLER(*err);
}

static void 
_sunrpc_InputSequenceMark(ilu_Call c, ilu_cardinal extent,
			  ILU_ERRS((IoErrs)) * err)
{
  ILU_CLER(*err);
}

static void
_sunrpc_InputSequence(ilu_Call c, ilu_cardinal * sequenceLength,
		      ilu_cardinal limit, ilu_Type prefix, ILU_ERRS((IoErrs)) * err)
{
  ilu_cardinal    len = 0;
  _sunrpc_InputCardinal(c, &len, err);
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

static void _sunrpc_EndSequence (ilu_Call c, ILU_ERRS((IoErrs)) * err)
{
  ILU_CLER(*err);
}

static          ilu_cardinal
_sunrpc_SizeOfSequence(ilu_Call c, ilu_cardinal length,
		       ilu_cardinal limit, ilu_Type prefix, ILU_ERRS((IoErrs)) * err)
{
  if (limit > 0 && length > limit)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_sequenceLimit, 0);
  else  
    return (_sunrpc_SizeOfCardinal(c, length, err));
}

/* ==================== union ==================== */

static void 
_sunrpc_OutputUnion(ilu_Call c, ilu_cardinal typeIndex,
		    ilu_TypeKind tk, ilu_Type prefix,
		    ILU_ERRS((IoErrs)) * err)
{
  Output_Cardinal(c, typeIndex, err);
}

static void 
_sunrpc_InputUnion(ilu_Call c, ilu_cardinal * typeIndex,
		   ilu_TypeKind tk, ilu_Type prefix,
		   ILU_ERRS((IoErrs)) * err)
{
  _sunrpc_InputCardinal(c, typeIndex, err);
}

static void _sunrpc_EndUnion (ilu_Call c, ILU_ERRS((IoErrs)) * err)
{
  ILU_CLER(*err);
}

static ilu_cardinal 
_sunrpc_SizeOfUnion(ilu_Call c, ilu_cardinal typeIndex,
		    ilu_TypeKind tk, ilu_Type prefix,
		    ILU_ERRS((IoErrs)) * err)
{
  return (_sunrpc_SizeOfCardinal(c, typeIndex, err));
}

/* ==================== array ==================== */

static void 
_sunrpc_OutputArray(ilu_Call c, ilu_cardinal len,
		    ilu_Type prefix, ILU_ERRS((IoErrs)) * err)
{
  ILU_CLER(*err);
}

static void _sunrpc_InputArray(ilu_Call c, ilu_Type prefix, ILU_ERRS((IoErrs)) * err)
{
  ILU_CLER(*err);
}

static void _sunrpc_EndArray (ilu_Call c, ILU_ERRS((IoErrs)) * err)
{
  ILU_CLER(*err);
}

static ilu_cardinal _sunrpc_SizeOfArray (ilu_Call c, ilu_cardinal len,
		    ilu_Type prefix, ILU_ERRS((IoErrs)) * err)
{
  ILU_CLER(*err);
  return(0);
}

/* ==================== record ==================== */

static void 
_sunrpc_OutputRecord(ilu_Call c, ilu_Type prefix, ILU_ERRS((IoErrs)) * err)
{
  ILU_CLER(*err);
}

static void _sunrpc_InputRecord(ilu_Call c, ilu_Type prefix, ILU_ERRS((IoErrs)) * err)
{
  ILU_CLER(*err);
}

static void _sunrpc_EndRecord (ilu_Call c, ILU_ERRS((IoErrs)) * err)
{
  ILU_CLER(*err);
}

static          ilu_cardinal
_sunrpc_SizeOfRecord(ilu_Call c, ilu_Type prefix, ILU_ERRS((IoErrs)) * err)
{
  ILU_CLER(*err);
  return (0);
}

/*======================================================================
**======================================================================
**====================  Non-I/O code ===================================
**======================================================================
**====================================================================*/

/*L2 unconstrained*/
/*L1_sup < prmu*/
static ilu_string FormProtocolHandle (ilu_Object obj, char *header)
{
  char buf[100];
  sunrpcinfo *s = NIL;
  ilu_Class class = object_class(obj);

  _ilu_AcquireMutex(ilu_prmu);
  s = _sunrpc_SunRPCInformationForClass (class);
  if (s == NIL)
    {
      _ilu_ReleaseMutex(ilu_prmu);
      ILU_ERRPRINTF("%s program#/version for class %s.\n",
	       "ILU: _sunrpc_FormProtocolHandle:  Can't figure", class->cl_name);
      return (NIL);
    }
  else
    {
      sprintf (buf, "%ssunrpc_2_0x%lx_%lu", header,
	       ((unsigned long) s->sui_pnumber) & 0xFFFFFFFF,
	       ((unsigned long) s->sui_version) & 0xFFFFFFFF);
      _ilu_ReleaseMutex(ilu_prmu);
      return (_ilu_Strdup(buf));
    }
}
     
/*L2 unconstrained*/
/*L1_sup < prmu*/
static ilu_string _sunrpc_FormProtocolHandle (ilu_refany pdata, ilu_Object obj)
{
  return (FormProtocolHandle(obj, ""));
}

/*L2 unconstrained*/
/*L1_sup < prmu*/
static ilu_string _csunrpc_FormProtocolHandle (ilu_refany pdata, ilu_Object obj)
{
  return (FormProtocolHandle(obj, "c"));
}

/*L2 unconstrained*/
/*L1_sup < prmu*/
static ilu_string _bsunrpc_FormProtocolHandle (ilu_refany pdata, ilu_Object obj)
{
  return (FormProtocolHandle(obj, "b"));
}

/*L2 unconstrained*/
/*L1_sup < prmu*/
static ilu_string _bcsunrpc_FormProtocolHandle (ilu_refany pdata, ilu_Object obj)
{
  return (FormProtocolHandle(obj, "bc"));
}

/*Call-IHi(call)*/

static          ilu_ReadHeaderResultCode
_sunrpc_ReadHeader(ilu_Call call, ilu_PacketType * type,
		   ilu_cardinal * sn,
		   ilu_ConnShutdownReason *reason, ilu_cardinal *lastSN,
		   ILU_ERRS((IoErrs)) * err)
{
  ilu_Transport   bs = sunrpc_transport(call);
  ilu_bytes       packet;
  ilu_cardinal    serialNumber, packetType;
  ilu_ReadHeaderResultCode ans;

  ans = transport_begin_message(bs, ilu_TRUE, err);
  switch (ans) {
  case ilu_rhrc_ok:
    break;
  case ilu_rhrc_error:
    ILU_NOTE((INCOMING_DEBUG | SUNRPC_DEBUG),
     ("%s:  error %s on transport_begin_message (input)\n",
      "ILU: _sunrpc_ReadHeader", ILU_ERR_NAME(*err)));
  case ilu_rhrc_eof:
    *reason = ilu_ConnShutdownReason_ReceivedEOF;
    *lastSN = 0;
  case ilu_rhrc_nothing:
    return (ans);
  default:
    _ilu_Assert(ilu_FALSE, "sunrpc.c:ReadHeader");
  }

  if ((packet = transport_get_input_buffer(bs, 8, err)) == NIL) {
    ILU_NOTE((INCOMING_DEBUG | SUNRPC_DEBUG),
	  ("%s:  error %s reading serialnumber or packetType\n",
	   "ILU: _sunrpc_ReadHeader", ILU_ERR_NAME(*err)));
    return (ilu_rhrc_error);
  }

  serialNumber = (packet[0] << 24) + (packet[1] << 16)
    + (packet[2] << 8) + packet[3];
  packetType = (packet[4] << 24) + (packet[5] << 16)
    + (packet[6] << 8) + packet[7];

  ILU_NOTE((INCOMING_DEBUG | SUNRPC_DEBUG),
	("%s, SN %lu, type %lu (%s).\n",
	 "ILU: _sunrpc_ReadHeader:  reading packet",
	 serialNumber, packetType,
	 ((packetType == ilu_PacketType_Request) ? "request"
	  : (packetType == ilu_PacketType_Reply) ? "reply"
	  : "unknown type")));

  *type = (ilu_PacketType) packetType;
  *sn = serialNumber;
  return (ans);
}

static ilu_refany 
_sunrpc_DelayInterp(ilu_Call call,
		    ILU_ERRS((IoErrs)) * err)
{
  ilu_Transport   t = sunrpc_transport(call);
  return _ilu_BufferInputMessage(t, 0, ilu_FALSE, err);
}

static void _sunrpc_ResumeInterp(ilu_Call call, ilu_refany x)
{
  ilu_Transport   d = (ilu_Transport) x;
  call->ca_prTrans = d;
  call->ca_prbit1 = ilu_TRUE;
  return;
}

static ilu_boolean 
_sunrpc_DiscardMessage(ilu_Call call, ILU_ERRS((internal)) * err)
{
  ilu_Transport   t = sunrpc_transport(call);
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
_sunrpc_AbandonDelayedInterp(ilu_Connection conn,
			     ilu_refany x,
			     ILU_ERRS((internal)) * err)
{
  ilu_Transport   d = (ilu_Transport) x;
  ilu_boolean     ans;
  ilu_integer     cdfd;
  ans = transport_close(d, &cdfd, err);
  ILU_ERR_SWITCH(*err) {
    ILU_SUCCESS_CASE /* no effect */;
    ILU_ERR_CASE2(bad_locks, broken_locks) {
      ILU_HANDLED(*err);
      (void) ilu_Check(ilu_FALSE, err);
    }
  } ILU_ERR_ENDSWITCH;
  _ilu_Assert(cdfd == 0, "sunrpc AbandonDelayedInterp");
  return ans;
}

static void
  _sunrpc_RequestRead (ilu_Call call, ILU_ERRS((IoErrs)) *err)
{
  transport_end_message(sunrpc_transport(call), ilu_FALSE, NIL, err);
}

static void
  _sunrpc_ReplyRead (ilu_Call call, ILU_ERRS((IoErrs)) *err)
{
  transport_end_message(sunrpc_transport(call), ilu_FALSE, NIL, err);
}

/*======================================================================*/
/*     Sun RPC Auth UNIX Identity type                                  */
/*======================================================================*/

/*L1, L2 ???*/

static ilu_cardinal
  _ilu_SunRPCAuthUnixIdentity_StringForm (ilu_refany info,
					  char *buffer,
					  ilu_cardinal bufferlen,
					  ilu_Error *err)
{
  ilu_SunRPCAuthUnixIdentityInfo i = (ilu_SunRPCAuthUnixIdentityInfo) info;
  ilu_cardinal needed;

  needed = 15 + strlen(i->ii_hostname);

  if (bufferlen < needed)
    return (ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_small_buffer, needed));
  else
    {
      ILU_CLER(*err);
      return sprintf(buffer, "[%.5u,%.5u]@%s", i->ii_UID, i->ii_GID, i->ii_hostname);
    }      
}

static ilu_refany
  _ilu_SunRPCAuthUnixIdentity_DuplicateData (ilu_refany info,
					     ilu_Error *err)
{
  ilu_SunRPCAuthUnixIdentityInfo oldi = (ilu_SunRPCAuthUnixIdentityInfo) info;
  ilu_SunRPCAuthUnixIdentityInfo i;
  ilu_cardinal    j, needed;
  needed = (sizeof(*i) + strlen(oldi->ii_hostname) + 1
	    + oldi->ii_ngids * sizeof(ilu_shortcardinal));
  i = ilu_MallocE(needed, err);
  if (ILU_ERRNOK(*err))
    return NIL;
  i->ii_UID = oldi->ii_UID;
  i->ii_GID = oldi->ii_UID;
  i->ii_ngids = oldi->ii_UID;
  i->ii_gids = (ilu_shortcardinal *) (((char *) i) + sizeof(*i));
  i->ii_hostname = (((char *) i) + sizeof(*i)
		    + sizeof(ilu_shortcardinal) * i->ii_ngids);
  strcpy(i->ii_hostname, oldi->ii_hostname);
  for (j = 0; j < i->ii_ngids; j++)
    i->ii_gids[j] = oldi->ii_gids[j];
  return i;
}

static void
  _ilu_SunRPCAuthUnixIdentity_FreeData (ilu_refany info,
					ilu_Error *err)
{
  ilu_SunRPCAuthUnixIdentityInfo i = (ilu_SunRPCAuthUnixIdentityInfo) info;
  ILU_CLER(*err);
  if ((i->ii_gids == ((ilu_shortcardinal *) (((char *)i) + sizeof(*i)))) &&
      (i->ii_hostname == ((char *)i) + sizeof(*i)) + (sizeof(ilu_shortcardinal)*(i->ii_ngids)))
    {
    }
  else
    {
      ilu_free(i->ii_hostname);
      ilu_free(i->ii_gids);
    }
  ilu_free(i);
}

struct _ilu_IdentityType_s ilu_SunRPCAuthUnixIdentity_s = {
  "SunRPCAuthUnixIdentity",
  _ilu_SunRPCAuthUnixIdentity_StringForm,
  _ilu_SunRPCAuthUnixIdentity_DuplicateData,
  _ilu_SunRPCAuthUnixIdentity_FreeData,
  NULLFN, NULLFN };


/*Call-IHi(call)*/

static ilu_Passport 
ReadCredentialsAndVerifier(ilu_Call call,
			   ILU_ERRS((IoErrs)) * err)
{
  ilu_bytes       credBuf = NIL, verifierBuf = NIL;
  ilu_cardinal    credSize, credType, verifierSize, verifierType;
  ilu_Passport    passport = call->ca_caller;

  _sunrpc_InputCardinal(call, &credType, err);
  if (ILU_ERRNOK(*err)) return passport;
  if (credType == 0) {		/* no authentication info */
    _sunrpc_InputBytes(call, &credBuf, &credSize, 0, err);
    if (ILU_ERRNOK(*err)) return passport;
    _sunrpc_InputCardinal(call, &verifierType, err);
    if (ILU_ERRNOK(*err)) return passport;
    _sunrpc_InputBytes(call, &verifierBuf, &verifierSize, 0, err);
    if (ILU_ERRNOK(*err)) return passport;
  }
  else if (credType == 1)	/* AUTH_UNIX */
    {
      ilu_cardinal stamp;
      ilu_IdentityInfo i;
      ilu_SunRPCAuthUnixIdentityInfo info;

      i = (ilu_IdentityInfo) ilu_MallocE(sizeof(*i), err);
      if (ILU_ERRNOK(*err)) return NIL;
      info = (ilu_SunRPCAuthUnixIdentityInfo) ilu_MallocE(sizeof(*info), err);
      if (ILU_ERRNOK(*err)) return NIL;
      i->ii_type = ilu_SunRPCAuthUnixIdentity;
      if (_sunrpc_InputCardinal (call, &credSize, err), ILU_ERRNOK(*err)) return NIL;
      if (_sunrpc_InputCardinal (call, &stamp, err), ILU_ERRNOK(*err)) return NIL;
      info->ii_hostname = NIL;
      _sunrpc_InputString(call, (void **) &info->ii_hostname, &stamp, 255, ILU_StringEncoding_latin1, &stamp, err);
      if (ILU_ERRNOK(*err)) return NIL;
      _sunrpc_InputShortCardinal(call, &info->ii_UID, err);
      if (ILU_ERRNOK(*err)) return NIL;
      _sunrpc_InputShortCardinal(call, &info->ii_GID, err);
      if (ILU_ERRNOK(*err)) return NIL;
      _sunrpc_InputShortCardinal(call, &info->ii_ngids, err);
      if (ILU_ERRNOK(*err)) return NIL;
      info->ii_gids = (ilu_shortcardinal *) ilu_MallocE(info->ii_ngids * sizeof(ilu_shortcardinal), err);
      if (ILU_ERRNOK(*err)) return NIL;
      for (stamp = 0;  stamp < info->ii_ngids;  stamp++)
	{
	  _sunrpc_InputShortCardinal(call, &info->ii_gids[stamp], err);
	  if (ILU_ERRNOK(*err)) return NIL;
	}
      i->ii_info = (ilu_refany) info;
      i->ii_owned_by_passport = ilu_TRUE;
      if (passport == NIL)
	{
	  passport = call->ca_caller = ilu_CreatePassport(i, err);
	  if (ILU_ERRNOK(*err)) return NIL;
	}
      else if (!ilu_AddIdentity (passport, i, err))
	return NIL;
      _sunrpc_InputCardinal(call, &verifierType, err);
      if (ILU_ERRNOK(*err)) return passport;
      _sunrpc_InputBytes(call, &verifierBuf, &verifierSize, 0, err);
      if (ILU_ERRNOK(*err)) return passport;
      ILU_NOTE(AUTHENTICATION_DEBUG,
	       ("ILU: (sunrpc.c/ReadCredentialsAndVerifier): Sun RPC UNIX"
		" auth:  [%ld, %ld] on \"%s\", %ld groups\n",
		(long) info->ii_UID,
		(long) info->ii_GID,
		info->ii_hostname,
		(long) info->ii_ngids));
    }
  else if (credType == ILU_UNSECURED_GENERIC_IDENTITY)
    {
      ilu_string idtypename = NIL;
      ilu_bytes id_pickled = NIL;
      ilu_cardinal id_pickled_len;
      ilu_IdentityInfo info;
      ilu_IdentityType id_type;

      _sunrpc_InputCardinal (call, &credSize, err);
      if (ILU_ERRNOK(*err)) return NIL;
      _sunrpc_InputString (call, (void **) &idtypename, &credSize, 0xFFFF, ILU_StringEncoding_latin1, &id_pickled_len, err);
      if (ILU_ERRNOK(*err)) return NIL;
      _sunrpc_InputBytes (call, &id_pickled, &id_pickled_len, 0xFFFF, err);
      if (ILU_ERRNOK(*err)) { ilu_free(idtypename); return NIL; }
      _sunrpc_InputCardinal(call, &verifierType, err);
      if (ILU_ERRNOK(*err)) { ilu_free(idtypename); ilu_free(id_pickled); return NIL; }
      _sunrpc_InputBytes(call, &verifierBuf, &verifierSize, 0, err);
      if (ILU_ERRNOK(*err)) { ilu_free(idtypename); ilu_free(id_pickled); return NIL; }
      ilu_free(verifierBuf);

      ILU_NOTE(AUTHENTICATION_DEBUG,
	    ("ILU: (sunrpc.c/ReadCredentialsAndVerifier): "
	     "identity of type \"%s\" encountered.\n",
	     idtypename));

      if ((id_type = ilu_FindIdentityTypeByName (idtypename, err)) == NIL)
	{ ILU_NOTE(AUTHENTICATION_DEBUG,
		("ILU: (sunrpc.c/ReadCredentialsAndVerifier): "
		 "identity type \"%s\" unknown in this address space.\n",
		 idtypename));
	  ilu_free(idtypename); ilu_free(id_pickled); return NIL; }
      else if ((info = (*id_type->it_unpickle)(id_pickled, id_pickled_len, err)) == NIL)
	{ ilu_free(id_pickled); return NIL; }
      ilu_free(id_pickled);
      info->ii_owned_by_passport = ilu_TRUE;
      if (passport == NIL)
	{
	  passport = call->ca_caller = ilu_CreatePassport(info, err);
	  if (ILU_ERRNOK(*err))
	    { (*id_type->it_free_data)(info->ii_info, err); ilu_free(info); return NIL; }
	}
      else if (!ilu_AddIdentity (passport, info, err))
	{ (*id_type->it_free_data)(info->ii_info, err); ilu_free(info); return NIL; }
    }
  else {
    _sunrpc_InputBytes(call, &credBuf, &credSize, 0, err);
    if (ILU_ERRNOK(*err)) return passport;
    _sunrpc_InputCardinal(call, &verifierType, err);
    if (ILU_ERRNOK(*err)) return passport;
    _sunrpc_InputBytes(call, &verifierBuf, &verifierSize, 0, err);
    if (ILU_ERRNOK(*err)) return passport;
    ILU_NOTE(AUTHENTICATION_DEBUG,
	  ("ILU: (sunrpc.c/ReadCredentialsAndVerifier): Call SN=%lu, "
	   " credBuf=%p, credSize is %lu, verfBuf is %p, verfSize is %lu\n",
	   call->ca_SN, credBuf, credSize, verifierBuf, verifierSize));
  }

  FREETOKEN(credBuf);
  FREETOKEN(verifierBuf);

  return passport;
}

static ilu_boolean 
  _sunrpc_InterpretRequest(ilu_Call call,
			   ILU_ERRS((IoErrs)) *err)
{
  sunrpcinfo     *s;
  ilu_cardinal    rpcVersion, programNumber, programVersion, methodID;

  _sunrpc_InputCardinal(call, &rpcVersion, err);
  if (ILU_ERRNOK(*err)) return ilu_FALSE;
  _sunrpc_InputCardinal(call, &programNumber, err);
  if (ILU_ERRNOK(*err)) return ilu_FALSE;
  _sunrpc_InputCardinal(call, &programVersion, err);
  if (ILU_ERRNOK(*err)) return ilu_FALSE;
  _sunrpc_InputCardinal(call, &methodID, err);
  if (ILU_ERRNOK(*err)) return ilu_FALSE;
  ILU_NOTE((INCOMING_DEBUG | SUNRPC_DEBUG),
	("%s  (call SN %lu) prognum 0x%lx (version %lu), method ID = %lu\n",
	 "ILU: _sunrpc_InterpretRequest:", call->ca_SN, programNumber,
	 programVersion, methodID));

  ReadCredentialsAndVerifier(call, err);
  if (ILU_ERRNOK(*err)) return ilu_FALSE;

  _ilu_AcquireMutex(ilu_prmu);
  s = _sunrpc_ClassFromProgramNumber(programNumber, programVersion,
				     ilu_TRUE);
  call_intro_type(call)
    = (s == NIL) ? NIL
    : (s->sui_class == NIL) ? ilu_FindClassFromID(s->sui_type_id)
    : s->sui_class;
  _ilu_ReleaseMutex(ilu_prmu);
  if (s == NIL || call_intro_type(call) == NIL) {
    ILU_NOTE((INCOMING_DEBUG | SUNRPC_DEBUG),
	  ("%s  (call %lu) %s 0x%lx, version %lu\n",
	   "ILU: _sunrpc_InterpretRequest:", call->ca_SN,
	   "Can't find ilu_Class with pn",
	   programNumber, programVersion));
    call->ca_pe = ilu_ProtocolException_NoSuchClassAtServer;
    return (ilu_FALSE);
  }
  ILU_NOTE((INCOMING_DEBUG | SUNRPC_DEBUG),
	("ILU: _sunrpc_InterpretRequest:  (call SN %lu) intro_type is %s:%s\n",
	 call->ca_SN, call->ca_intro_type->cl_name,
	 call->ca_intro_type->cl_unique_id));

  call->ca_method = ilu_FindMethodByID(call_intro_type(call), methodID);

  if (call->ca_method == NIL) {
    ILU_NOTE((INCOMING_DEBUG | SUNRPC_DEBUG),
	  ("%s  (call %lu) %s \"%s\" (pn 0x%lx) with methodID = %lu.\n",
	   "ILU: _sunrpc_InterpretRequest:", call->ca_SN,
      "Can't find method on class", call_intro_type(call)->cl_name,
	   programNumber, methodID));
    call->ca_pe = ilu_ProtocolException_NoSuchMethodOnClass;
    return (ilu_FALSE);
  } else {
    ILU_NOTE((INCOMING_DEBUG | SUNRPC_DEBUG),
	  ("%s %lu is %p (%s).\n",
	   "ILU: _sunrpc_InterpretRequest:  record for method",
	   methodID, call->ca_method, call->ca_method->me_name));
  }

  if (ILU_ERRNOK(*err))
    return ilu_FALSE;

  ILU_NOTE((INCOMING_DEBUG | SUNRPC_DEBUG),
	("ILU: _sunrpc_InterpretRequest:  returning ilu_TRUE\n"));
  return (ilu_TRUE);
}

static          ilu_ProtocolException
_sunrpc_InterpretReply(ilu_Call call, ilu_cardinal * estatus,
		       ILU_ERRS((IoErrs)) * err)
{
  ilu_cardinal    verifierSize, replyStatus;
  ilu_bytes       verifierBuf;
  ilu_cardinal    authenticationType;
  static ilu_ProtocolException replyStatusExceptions[] = {
    ilu_ProtocolException_Success,
    ilu_ProtocolException_NoSuchClassAtServer,
    ilu_ProtocolException_ClassVersionMismatch,
    ilu_ProtocolException_NoSuchMethodOnClass,
    ilu_ProtocolException_GarbageArguments
  };
  static ilu_string acceptErrors[] = {
    "Success",
    "program unavailable",
    "program version mismatch",
    "procedure unavailable",
    "garbage arguments",
  };

  ILU_NOTE((INCOMING_DEBUG | SUNRPC_DEBUG),
	("ILU: _sunrpc_InterpretReply:  SN %lu\n", call->ca_SN));

  _sunrpc_InputCardinal(call, &replyStatus, err);
  if (ILU_ERRNOK(*err))
    return ilu_ProtocolException_Not;

  ILU_NOTE((INCOMING_DEBUG | SUNRPC_DEBUG),
	("ILU: _sunrpc_InterpretReply:  replyStatus is %lu\n",
	 replyStatus));

  if (replyStatus == 0) {	/* MSG_ACCEPTED */
    _sunrpc_InputCardinal(call, &authenticationType, err);
    if (ILU_ERRNOK(*err))
      return ilu_ProtocolException_Not;
    _sunrpc_InputBytes(call, &verifierBuf, &verifierSize, 0, err);
    if (ILU_ERRNOK(*err))
      return ilu_ProtocolException_Not;
    FREETOKEN(verifierBuf);
    _sunrpc_InputCardinal(call, &replyStatus, err);
    if (ILU_ERRNOK(*err))
      return ilu_ProtocolException_Not;

    ILU_NOTE((INCOMING_DEBUG | SUNRPC_DEBUG),
	  ("ILU: _sunrpc_InterpretReply:  *real* replyStatus is %lu\n",
	   replyStatus));

    if (replyStatus > 0) {	/* *really* accepted */
      ILU_NOTE((INCOMING_DEBUG | SUNRPC_DEBUG),
	    ("ILU: _sunrpc_InterpretReply: call %lu signals protocol error %lu (%s).\n",
	     call->ca_SN, replyStatus,
	     (replyStatus < 5) ? acceptErrors[replyStatus]
	     : "unknown error"));
    } else if (call->ca_method->me_exceptionCount > 0) {
      _sunrpc_InputCardinal(call, estatus, err);
      if (ILU_ERRNOK(*err))
	return ilu_ProtocolException_Not;
    } else
      *estatus = 0;

    if (replyStatus < 5)
      return (replyStatusExceptions[replyStatus]);
    else
      return (ilu_ProtocolException_Unknown);
  } else if (replyStatus == 1) {/* MSG_REJECTED */
    ILU_NOTE((INCOMING_DEBUG | SUNRPC_DEBUG),
       ("ILU: _sunrpc_InterpretReply:  call %lu signals \"Message Rejected\".\n",
	call->ca_SN));
    return (ilu_ProtocolException_RequestRejected);
  }
  return (ilu_ProtocolException_Unknown);
}

/*L1_sup < prmu*/
/*L2 ???*/

static ilu_boolean GetSunRPCProgramNumberAndVersion (ilu_Class pclass,
			ilu_cardinal *pnumber, ilu_cardinal *version)
{
  sunrpcinfo *s = NIL;
  _ilu_AcquireMutex(ilu_prmu);
  s = _sunrpc_SunRPCInformationForClass (pclass);
	/* probably consult external DB */
  if (s != NIL) {
      *pnumber = s->sui_pnumber;
      *version = s->sui_version;
    }
  else
    {
      ILU_ERRPRINTF("ILU: (GetSunRPCProgramNumberAndVersion):  Can't get program number information for class \"%s\".\n", pclass->cl_name);
    }
  _ilu_ReleaseMutex(ilu_prmu);
  return (s != NIL);
}

/*L1 >= {prmu}*/

#ifdef _IS_POSIX
#include "os/posix.h"
#else	/* win32 */
typedef ilu_shortcardinal gid_t;
#endif

#define MAX_GIDS 100
static int nameset = 0;
static char hostname[255];
static int hostnamelen = 0, hostnamelenr, credlen;
static int gid, uid, ngids;
#ifdef HAS_SOLARIS1_GID_T_SIZE_BUG
static unsigned int gids[MAX_GIDS];
#else
static gid_t gids[MAX_GIDS];
#endif

static int credlenr = 4 * 2;

/*L1_sup < prmu*/

static void FigureCredentialSize (ilu_Call call)
{
  int i;
  ilu_SunRPCAuthUnixIdentityInfo info;
  ilu_IdentityInfo id;

  if (getenv("ILU_NO_SUNRPC_UNIX_AUTH") != NIL)
    return;

  if ((!nameset) && (hostnamelen > 0))
    hostnamelen = 0;
  if ((call->ca_caller != NIL) &&
      ((id = ilu_FindIdentity(call->ca_caller, ilu_SunRPCAuthUnixIdentity)) != NIL)) {
    info = (ilu_SunRPCAuthUnixIdentityInfo) (id->ii_info);
    _ilu_AcquireMutex(ilu_prmu);
    strcpy(hostname, info->ii_hostname);
    hostnamelen = strlen(hostname);
    hostnamelenr = 4 * ((hostnamelen + 3) / 4);
    gid = info->ii_GID;
    uid = info->ii_UID;
    ngids = info->ii_ngids;
    for (i = 0;  i < ngids;  i++)
      gids[i] = info->ii_gids[i];
    nameset = ilu_FALSE;
    _ilu_ReleaseMutex(ilu_prmu);
  }
#ifdef _IS_POSIX
  else if (!nameset) {
    ilu_boolean cant_get_ip_addr_string = ilu_FALSE;
    _ilu_AcquireMutex(ilu_prmu);
    if (!nameset) {
      nameset = 1;

#ifdef _IS_BSD
      {
	ilu_Error       lerr;
	ilu_string      ans;
	ans = _ilu_CurrentHostIPAddrStringOnly(&lerr);
	if (ILU_ERROK(lerr))
	  strcpy(hostname, ans);
	else {
	  cant_get_ip_addr_string = ilu_TRUE;
	  ILU_HANDLED(lerr);
	};
      }
      if (cant_get_ip_addr_string)
#endif				/* def _IS_BSD */
	strcpy(hostname, _ilu_Hostname());

      hostnamelen = strlen(hostname);
      hostnamelenr = 4 * ((hostnamelen + 3) / 4);
      gid = getgid();
      uid = geteuid();
      ngids = getgroups(MAX_GIDS, gids);
      _ilu_Assert(ngids >= 0, "getgroups() failed");
      _ilu_ReleaseMutex(ilu_prmu);
    };
  }
#endif				/* def _IS_POSIX */
  if (hostnamelen > 0) {
    credlen = 4 /* stamp */
      + 4 + hostnamelenr	/* machinename */
	+ 4	/* uid */
	  + 4 /* gid */
	    + 4 + 4 * ngids /* gids */ ;
    credlenr = 4 * ((credlen + 3) / 4);
  } else {
    credlenr = 0;
  };
}

/*Call-OHi(call)*/

static void
WriteCredentials(ilu_Call call, ILU_ERRS((IoErrs)) * err)
{
  ilu_string      identity_type;
  ilu_IdentityType id_type;
  ilu_IdentityInfo id_info;
  ilu_cardinal    len, size;
  ilu_bytes       buffer = NIL;

  if ((identity_type = getenv("ILU_SUNRPC_PREFERRED_IDENTITY")) &&
      (id_type = ilu_FindIdentityTypeByName(identity_type, err)) &&
      (id_type->it_pickle != NULLFN) &&
      (call->ca_caller != NIL) &&
      ((id_info = ilu_FindIdentity(call->ca_caller, id_type))
       != NIL) &&
      (len = ilu_PickleIdentity (id_info, &buffer, 0, err),
       ILU_ERROK(*err))) {
    Output_Cardinal(call, ILU_UNSECURED_GENERIC_IDENTITY, err);
    if (ILU_ERRNOK(*err))
      return;
    size = strlen(id_type->it_name);
    size = 4 + 4 + PADDED_SIZE(size) + PADDED_SIZE(len);
    Output_Cardinal(call, size, err);
    if (ILU_ERRNOK(*err))
      return;
    _sunrpc_OutputString(call, (ilu_bytes) id_type->it_name,
			 strlen(id_type->it_name), 0xFFFF,
			 ILU_StringEncoding_latin1,
			 ILU_StringEncoding_latin1, err);
    if (ILU_ERRNOK(*err))
      return;
    _sunrpc_OutputBytes(call, buffer, len, 0xFFFF, err);
    ilu_free(buffer);
    if (ILU_ERRNOK(*err))
      return;
    Output_Cardinal(call, 0, err);	/* verifier:  AUTH_NULL */
    if (ILU_ERRNOK(*err))
      return;
    Output_Cardinal(call, 0, err);	/* 0 bytes */
    if (ILU_ERRNOK(*err))
      return;
  } else if ((getenv("ILU_NO_SUNRPC_UNIX_AUTH") == NIL) && (hostnamelen > 0)) {

    /* Do Sun RPC AUTH_UNIX */

    Output_Cardinal(call, 1, err);	/* UNIX credentials follow */
    if (ILU_ERRNOK(*err))
      return;
    Output_Cardinal(call, credlen, err);	/* credentials length */
    if (ILU_ERRNOK(*err))
      return;
    Output_Cardinal(call, 1, err);	/* stamp */
    if (ILU_ERRNOK(*err))
      return;
    _sunrpc_OutputString(call, (ilu_bytes) hostname, hostnamelen, 255,
			 ILU_StringEncoding_latin1, ILU_StringEncoding_latin1,
			 err);
    if (ILU_ERRNOK(*err))
      return;
    Output_Cardinal(call, (ilu_cardinal) uid, err);
    if (ILU_ERRNOK(*err))
      return;
    Output_Cardinal(call, (ilu_cardinal) gid, err);
    if (ILU_ERRNOK(*err))
      return;
    Output_Cardinal(call, (ilu_cardinal) ngids, err);
    if (ILU_ERRNOK(*err))
      return;
    {
      int             i;
      for (i = 0; i < ngids; i++) {
	Output_Cardinal(call, (ilu_cardinal) (gids[i]), err);
	if (ILU_ERRNOK(*err))
	  return;
      }
    }

    Output_Cardinal(call, 0, err);	/* verifier:  AUTH_NULL */
    if (ILU_ERRNOK(*err))
      return;
    Output_Cardinal(call, 0, err);	/* 0 bytes */
    if (ILU_ERRNOK(*err))
      return;
  } else {
    static unsigned char credAndVerf[16] = {0,};
    (void) transport_write_bytes(sunrpc_transport(call), credAndVerf,
				 16, err);
  }
}

static          ilu_boolean
_sunrpc_StartRequest(ilu_Call call, ilu_cardinal argSize,
		     ILU_ERRS((IoErrs)) * err)
{
  ilu_cardinal    packetSize;
  ilu_cardinal    pnumber = 0, version = 0;
  ilu_Class       pclass = call->ca_intro_type;
  ilu_Method      method = call->ca_method;

  ILU_NOTE(SUNRPC_DEBUG,
	("%s %p (sn %lu), aSize %lu, class %s (%s), meth %s (%lu)\n",
	 "ILU: _sunrpc_StartRequest:  call", call,
	 call_serial_number(call), argSize, class_name(pclass),
	 class_unique_id(pclass), method_name(method),
	 method_id(method)));

  if (!GetSunRPCProgramNumberAndVersion(pclass, &pnumber, &version)) {
    ILU_ERRPRINTF("%s %s of class \"%s\" on call %p.\n",
		  "ILU: _sunrpc_StartRequest:  Can't determine",
		  "program-number/version",
		  pclass->cl_name, call);
    return (ilu_FALSE);
  }
  ILU_NOTE(SUNRPC_DEBUG,
	("ILU: _sunrpc_StartRequest:  call %p (sn %lu),"
	 " aSize %lu, prog/ver 0x%lx/%lu, method id %lu\n",
	 call, call_serial_number(call), argSize,
	 pnumber, version, method_id(method)));

  FigureCredentialSize(call);

  packetSize = argSize
    + (4 * 10)			/* for fields of header */
    +credlenr /* for UNIX credentials */ ;

  if (transport_begin_message(sunrpc_transport(call), ilu_FALSE, err)
      != ilu_rhrc_ok)
    goto faild;

  Output_Cardinal(call, call->ca_SN, err);
  if (ILU_ERRNOK(*err))
    goto faild;
  Output_Cardinal(call, 0, err);/* message type == CALL */
  if (ILU_ERRNOK(*err))
    goto faild;
  Output_Cardinal(call, 2, err);/* Sun RPC version 2 */
  if (ILU_ERRNOK(*err))
    goto faild;
  Output_Cardinal(call, pnumber, err);
  if (ILU_ERRNOK(*err))
    goto faild;
  Output_Cardinal(call, version, err);
  if (ILU_ERRNOK(*err))
    goto faild;
  Output_Cardinal(call, method->me_id, err);
  if (ILU_ERRNOK(*err))
    goto faild;

  WriteCredentials(call, err);
  if (ILU_ERRNOK(*err))
    goto faild;

  ILU_NOTE(SUNRPC_DEBUG,
	("ILU: _sunrpc_StartRequest:  request %lu begun (size %lu).\n",
	 call->ca_SN, packetSize));
  return (ilu_TRUE);
faild:
  return ilu_FALSE;
}

static          ilu_boolean
_sunrpc_FinishRequest(ilu_Call call,
		      ilu_Message * msg,
		      ilu_boolean push,
		      ILU_ERRS((IoErrs)) * err)
{
  ilu_boolean     ans;
  ans = transport_end_message(sunrpc_transport(call), push, msg, err);
  return ans;
}

static          ilu_boolean
_sunrpc_FinishReply(ilu_Call call,
		    ilu_boolean push,
		    ILU_ERRS((IoErrs)) * err)
{
  ilu_Transport   t = sunrpc_transport(call);
  ilu_Message     msg = {NIL, 0};
  ilu_boolean     ans;

  ans = transport_end_message(t, push, &msg, err);
  if (ans && !transport_reliable(t))
    ans = _ilu_CacheCall(call, &msg, err);
  return ans;
}

static          ilu_boolean
_sunrpc_FinishException(ilu_Call call,
			ilu_boolean push,
			ILU_ERRS((IoErrs)) * err)
{
  ilu_Transport   t = sunrpc_transport(call);
  ilu_Message     msg = {NIL, 0};
  ilu_boolean     ans;
  ans = transport_end_message(t, push, &msg, err);
  if (ans && !transport_reliable(t))
    ans = _ilu_CacheCall(call, &msg, err);
  return ans;
}

/*L1, L2 unconstrained*/
static          ilu_cardinal
_sunrpc_BeginSizingReply(ilu_Call call,
			 ilu_boolean exceptions,
			 ILU_ERRS((IoErrs)) * err)
{
  return (ILU_CLER(*err), 0);
}

static          ilu_boolean
_sunrpc_BeginReply(ilu_Call call,
		   ilu_boolean exceptions,
		   ilu_cardinal argSize,
		   ILU_ERRS((IoErrs)) * err)
{
  ilu_cardinal    packetSize;

  ILU_NOTE(PACKET_DEBUG,
	("%s %lu, argSize %lu, exceptions %s, trans %p.\n",
	 "ILU: _sunrpc_BeginReply:  SN", call->ca_SN, argSize,
	 exceptions ? "ilu_TRUE" : "ilu_FALSE",
	 sunrpc_transport(call)));

  packetSize = argSize
    + (4 * 6)			/* for the basic header fields */
    +(exceptions ? 4 : 0);	/* possible extra word for excn code */

  if (transport_begin_message(sunrpc_transport(call), ilu_FALSE, err)
      != ilu_rhrc_ok)
    goto faild;

  Output_Cardinal(call, call->ca_SN, err);
  if (ILU_ERRNOK(*err)) goto faild;
  Output_Cardinal(call, 1, err);	/* message type ==REPLY */
  if (ILU_ERRNOK(*err)) goto faild;
  Output_Cardinal(call, 0, err);	/* message accepted */
  if (ILU_ERRNOK(*err)) goto faild;
  Output_Cardinal(call, 0, err);	/* verifier:  AUTH_NULL */
  if (ILU_ERRNOK(*err)) goto faild;
  Output_Cardinal(call, 0, err);	/* 0 bytes */
  if (ILU_ERRNOK(*err)) goto faild;
  Output_Cardinal(call, 0, err);	/* successful execution */
  if (ILU_ERRNOK(*err)) goto faild;
  if (exceptions) {
    Output_Cardinal(call, 0, err);	/* ret code is Success */
    if (ILU_ERRNOK(*err)) goto faild;
  }

  ILU_NOTE(PACKET_DEBUG,
	("ILU: _sunrpc_BeginReply:  started reply %lu (size %lu).\n",
	 call->ca_SN, packetSize));
  return (ilu_TRUE);
faild:
  return ilu_FALSE;
}

/*L1, L2 unconstrained*/
static          ilu_cardinal
_sunrpc_BeginSizingExn(ilu_Call call,
		       ilu_cardinal eindex,
		       ilu_ProtocolException sysExnIdx,
		       ILU_ERRS((IoErrs)) * err)
{
  return (ILU_CLER(*err), 0);
}

static          ilu_boolean
_sunrpc_BeginException(ilu_Call call,
		       ilu_cardinal evalue,
		       ilu_ProtocolException sysExnIdx,
		       ilu_cardinal argSize,
		       ILU_ERRS((IoErrs)) * err)
{
  ilu_cardinal    packetSize;

  /*
   * if "evalue" == 0, then sysExnIdx contains a protocol exception
   * detail code.
   */

  if (evalue == 0)		/* signal protocol error */
    packetSize = (4 * 6);
  else
    packetSize = (4 * 7) + argSize;

  if (transport_begin_message(sunrpc_transport(call), ilu_FALSE, err)
      != ilu_rhrc_ok)
    goto faild;

  Output_Cardinal(call, call->ca_SN, err);
  if (ILU_ERRNOK(*err)) goto faild;
  Output_Cardinal(call, 1, err);	/* message type ==REPLY */
  if (ILU_ERRNOK(*err)) goto faild;
  Output_Cardinal(call, 0, err);	/* message accepted */
  if (ILU_ERRNOK(*err)) goto faild;
  Output_Cardinal(call, 0, err);	/* verifier:  AUTH_NULL */
  if (ILU_ERRNOK(*err)) goto faild;
  Output_Cardinal(call, 0, err);	/* 0 bytes */
  if (ILU_ERRNOK(*err)) goto faild;
  Output_Cardinal(call, ((evalue == 0) ? sysExnIdx : 0), err);
  if (ILU_ERRNOK(*err)) goto faild;
  /* successful execution */
  if (evalue > 0) {
    Output_Cardinal(call, evalue, err);	/* exception value */
    if (ILU_ERRNOK(*err)) goto faild;
  }
  ILU_NOTE(PACKET_DEBUG,
	("%s: trans %p, SN %lu, size %lu, evalue %lu.\n",
	 "ILU: _sunrpc_BeginException:  exception started to peer",
	 sunrpc_transport(call),
	 call_serial_number(call), packetSize, (evalue == 0) ? sysExnIdx : evalue));
  return (ilu_TRUE);
faild:
  return ilu_FALSE;
}

/*L1, L2 unconstrained*/

static ilu_Protocol _sunrpc_NewSunRPC (void)
{
  ilu_Protocol    new = (ilu_Protocol) ilu_must_malloc(sizeof(*new));

  new->pr_concurrent_requests = ilu_FALSE;
  new->pr_sizing_required = ilu_FALSE;
  new->pr_needs_boundaried_transport = ilu_TRUE;
  new->pr_needs_reliable_transport = ilu_TRUE;

  new->pr_init_call = _sunrpc_InitCall;
  new->pr_start_request = _sunrpc_StartRequest;
  new->pr_finish_request = _sunrpc_FinishRequest;
  new->pr_begin_sizing_reply = _sunrpc_BeginSizingReply;
  new->pr_begin_reply = _sunrpc_BeginReply;
  new->pr_finish_reply = _sunrpc_FinishReply;
  new->pr_begin_sizing_exn = _sunrpc_BeginSizingExn;
  new->pr_begin_exception = _sunrpc_BeginException;
  new->pr_finish_exception = _sunrpc_FinishException;
  new->pr_finish_call = _sunrpc_FinishCall;
  new->pr_prefinish_call = NULLFN;

  new->pr_read_header = _sunrpc_ReadHeader;
  new->pr_delay_interp = _sunrpc_DelayInterp;
  new->pr_resume_interp = _sunrpc_ResumeInterp;
  new->pr_abandon_delayed_interp = _sunrpc_AbandonDelayedInterp;
  new->pr_discard_input = _sunrpc_DiscardMessage;
  new->pr_discard_output = _sunrpc_DiscardMessage;
  
  new->pr_interpret_request = _sunrpc_InterpretRequest;
  new->pr_request_read = _sunrpc_RequestRead;
  new->pr_interpret_reply = _sunrpc_InterpretReply;
  new->pr_reply_read = _sunrpc_ReplyRead;

  new->pr_output_integer = _sunrpc_OutputInteger;
  new->pr_input_integer = _sunrpc_InputInteger;
  new->pr_size_of_integer = _sunrpc_SizeOfInteger;

  new->pr_output_shortinteger = _sunrpc_OutputShortInteger;
  new->pr_input_shortinteger = _sunrpc_InputShortInteger;
  new->pr_size_of_shortinteger = _sunrpc_SizeOfShortInteger;

  new->pr_output_longinteger = _sunrpc_OutputLongInteger;
  new->pr_input_longinteger = _sunrpc_InputLongInteger;
  new->pr_size_of_longinteger = _sunrpc_SizeOfLongInteger;

  new->pr_output_cardinal = _sunrpc_OutputCardinal;
  new->pr_input_cardinal = _sunrpc_InputCardinal;
  new->pr_size_of_cardinal = _sunrpc_SizeOfCardinal;

  new->pr_output_shortcardinal = _sunrpc_OutputShortCardinal;
  new->pr_input_shortcardinal = _sunrpc_InputShortCardinal;
  new->pr_size_of_shortcardinal = _sunrpc_SizeOfShortCardinal;

  new->pr_output_longcardinal = _sunrpc_OutputLongCardinal;
  new->pr_input_longcardinal = _sunrpc_InputLongCardinal;
  new->pr_size_of_longcardinal = _sunrpc_SizeOfLongCardinal;

  new->pr_output_real = _sunrpc_OutputReal;
  new->pr_input_real = _sunrpc_InputReal;
  new->pr_size_of_real = _sunrpc_SizeOfReal;

  new->pr_output_shortreal = _sunrpc_OutputShortReal;
  new->pr_input_shortreal = _sunrpc_InputShortReal;
  new->pr_size_of_shortreal = _sunrpc_SizeOfShortReal;

  new->pr_output_longreal = _sunrpc_OutputLongReal;
  new->pr_input_longreal = _sunrpc_InputLongReal;
  new->pr_size_of_longreal = _sunrpc_SizeOfLongReal;

  new->pr_output_optional = _sunrpc_OutputOptional;
  new->pr_input_optional = _sunrpc_InputOptional;
  new->pr_size_of_optional = _sunrpc_SizeOfOptional;

  new->pr_output_enum_code = _sunrpc_OutputEnumeration;
  new->pr_input_enum_code = _sunrpc_InputEnumeration;
  new->pr_size_of_enum_code = _sunrpc_SizeOfEnumeration;

  new->pr_output_byte = _sunrpc_OutputByte;
  new->pr_input_byte = _sunrpc_InputByte;
  new->pr_size_of_byte = _sunrpc_SizeOfByte;

  new->pr_output_character = _sunrpc_OutputShortCardinal;
  new->pr_input_character = _sunrpc_InputShortCardinal;
  new->pr_size_of_character = _sunrpc_SizeOfShortCardinal;

  new->pr_output_boolean = _sunrpc_OutputBoolean;
  new->pr_input_boolean = _sunrpc_InputBoolean;
  new->pr_size_of_boolean = _sunrpc_SizeOfBoolean;

  new->pr_output_shortchar = _sunrpc_OutputShortChar;
  new->pr_input_shortchar = _sunrpc_InputShortChar;
  new->pr_size_of_shortchar = _sunrpc_SizeOfShortChar;

  new->pr_output_string = _sunrpc_OutputString;
  new->pr_input_string = _sunrpc_InputString;
  new->pr_size_of_string = _sunrpc_SizeOfString;

  new->pr_output_wstring = _ilu_OutputWString;
  new->pr_input_wstring = _ilu_InputWString;
  new->pr_size_of_wstring = _ilu_SizeOfWString;

  new->pr_output_bytes = _sunrpc_OutputBytes;
  new->pr_input_bytes = _sunrpc_InputBytes;
  new->pr_size_of_bytes = _sunrpc_SizeOfBytes;

  new->pr_output_opaque = _sunrpc_OutputOpaque;
  new->pr_input_opaque = _sunrpc_InputOpaque;
  new->pr_size_of_opaque = _sunrpc_SizeOfOpaque;

  new->pr_output_wstringvec = _ilu_OutputWStringVec;
  new->pr_input_wstringvec = _ilu_InputWStringVec;
  new->pr_size_of_wstringvec = _ilu_SizeOfWStringVec;

  new->pr_output_object_id = _ilu_OutputObjectID;
  new->pr_input_object_id = _ilu_InputObjectID;
  new->pr_size_of_object_id = _ilu_SizeOfObjectID;

  new->pr_output_stringvec =
	(void (*)(ilu_Call,ilu_string,ilu_cardinal,ilu_Error*))
	_sunrpc_OutputOpaque;
  new->pr_input_stringvec =
	(void (*)(ilu_Call,ilu_string *,ilu_cardinal,ilu_Error*))
	_sunrpc_InputOpaque;
  new->pr_size_of_stringvec =
	(ilu_cardinal (*)(ilu_Call,ilu_string,ilu_cardinal,ilu_Error*))
	_sunrpc_SizeOfOpaque;

  new->pr_output_sequence = _sunrpc_OutputSequence;
  new->pr_output_sequence_mark = _sunrpc_OutputSequenceMark;
  new->pr_input_sequence = _sunrpc_InputSequence;
  new->pr_input_sequence_mark = _sunrpc_InputSequenceMark;
  new->pr_end_sequence = _sunrpc_EndSequence;
  new->pr_size_of_sequence = _sunrpc_SizeOfSequence;

  new->pr_output_record = _sunrpc_OutputRecord;
  new->pr_input_record = _sunrpc_InputRecord;
  new->pr_end_record = _sunrpc_EndRecord;
  new->pr_size_of_record = _sunrpc_SizeOfRecord;

  new->pr_output_array = _sunrpc_OutputArray;
  new->pr_input_array = _sunrpc_InputArray;
  new->pr_end_array = _sunrpc_EndArray;
  new->pr_size_of_array = _sunrpc_SizeOfArray;

  new->pr_output_union = _sunrpc_OutputUnion;
  new->pr_input_union = _sunrpc_InputUnion;
  new->pr_end_union = _sunrpc_EndUnion;
  new->pr_size_of_union = _sunrpc_SizeOfUnion;

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

  new->pr_form_handle = NULLFN;

  new->pr_create_data_block = NULLFN;
  new->pr_free_data_block = _sunrpc_FreeDataBlock;
  new->pr_conn_closing = NULLFN;

  return (new);
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
  _ilu_sunrpc_CanMoveAsBlock(ilu_Call call,
			     ilu_LanguageIndex li,
			     ilu_Type t,
			     ilu_cardinal m,
			     ilu_cardinal *alignmentCode,
			     ilu_Error *err)
{
#if (defined(WORDS_BIGENDIAN) && defined(ADD_TYPE_REGISTRATION_SUPPORT))
  ilu_Protocol p = connection_protocol(call_connection(call));
  static ilu_boolean initialized = ilu_FALSE;
  static ilu_LanguageIndex ansi_c_index;

  if (!initialized) {
    ansi_c_index = ilu_RegisterLanguage("ANSI-C");
    initialized = ilu_TRUE;
  };

  if ((p->pr_output_union == _sunrpc_OutputUnion) &&
      (li == ansi_c_index)) {
    /* OK, using Sun RPC with ANSI C language mapping */
    ILU_CLER(*err);
    *alignmentCode = 0;

    switch (ur_type_kind(t)) {
    case ilu_byte_tk:
    case ilu_string_tk:
    case ilu_character_tk:
    case ilu_shortcharacter_tk:
    case ilu_shortinteger_tk:
    case ilu_shortcardinal_tk:
    case ilu_object_tk:
    case ilu_pipe_tk:
    case ilu_pickle_tk:
    case ilu_optional_tk:
#ifdef ILU_REFERENCE_TYPES
    case ilu_reference_tk:
#endif /* def ILU_REFERENCE_TYPES */
    case ilu_sequence_tk:
    case ilu_alias_tk:
    case ilu_boolean_tk:
#ifdef ILU_FIXED_POINT_SUPPORT
    case ilu_fixedpoint_tk:
#endif /* def ILU_FIXED_POINT_SUPPORT */
      /* can't do it with these types */
      return ilu_FALSE;

    case ilu_longinteger_tk:
    case ilu_integer_tk:
    case ilu_cardinal_tk:
    case ilu_longcardinal_tk:
    case ilu_real_tk:
    case ilu_shortreal_tk:
    case ilu_enumeration_tk:
    case ilu_longreal_tk:
      /* can always do it with these types */
      return ilu_TRUE;

    case ilu_union_tk:
      /* too hard to think about, just say no */
      return ilu_FALSE;

    case ilu_array_tk:
      return _ilu_sunrpc_CanMoveAsBlock (call, li, t->desc.Array.type, m, alignmentCode, err);

    case ilu_record_tk:
      {
	/* just handle three special cases, all 4-byte values or
	   all 8-byte values, or all 8-byte values followed by
	   all 4 byte values */
	unsigned int i;
	ilu_Type t2;
	enum states { nothingyet, sz8, sz4odd, sz4even } st;
	ilu_boolean leading_sz8 = ilu_FALSE;

	for (i = 0, st = nothingyet;  i < t->desc.Record.n_fields;  i++) {
	  t2 = t->desc.Record.fields[i].base.type;
	  switch (t2->kind) {
	  case ilu_longcardinal_tk:
	  case ilu_longinteger_tk:
	  case ilu_real_tk:
	    if (st == nothingyet)
	      leading_sz8 = ilu_TRUE;
	    else if (st == sz4odd)
	      return ilu_FALSE;
	    st = sz8;
	    break;
#if (SIZEOF_ENUM == 4)
	  case ilu_enumeration_tk:
#endif	    
	  case ilu_integer_tk:
	  case ilu_cardinal_tk:
	  case ilu_shortreal_tk:
	    if (st == sz4odd)
	      st = sz4even;
	    else
	      st = sz4odd;
	    break;
	  default:
	    return ilu_FALSE;
	  }
	}
	return (!((st == sz4odd) && leading_sz8));
      }
    default:
      return ilu_FALSE;      
    }
  };
#endif /* (defined(WORDS_BIGENDIAN) && defined(ADD_TYPE_REGISTRATION_SUPPORT)) */
  ILU_CLER(*err);
  return ilu_FALSE;
}
			     

/*L1_sup < prmu*/

ilu_Protocol _ilu_sunrpc_Protocol(ilu_ProtocolInfo pinfo,
				  ilu_Error *err)
{
  /* L1 >= {prmu} */
  static ilu_Protocol protocol = NIL;
  _ilu_AcquireMutex(ilu_prmu);
  if (protocol == NIL)
    protocol = _sunrpc_NewSunRPC();
  protocol->pr_form_handle = _sunrpc_FormProtocolHandle;
  protocol->pr_create_data_block = _sunrpc_CreateNonBatchingNonConcurrentDataBlock;
  _ilu_ReleaseMutex(ilu_prmu);
  ILU_CLER(*err);
  return (protocol);
}

ilu_Protocol _ilu_csunrpc_Protocol(ilu_ProtocolInfo pinfo,
				   ilu_Error *err)
{
  /*L1 >= {prmu}*/
  static ilu_Protocol protocol = NIL;
  _ilu_AcquireMutex(ilu_prmu);
  if (protocol == NIL)
    protocol = _sunrpc_NewSunRPC();
  protocol->pr_form_handle = _csunrpc_FormProtocolHandle;
  protocol->pr_concurrent_requests = ilu_TRUE;
  protocol->pr_create_data_block = _sunrpc_CreateNonBatchingConcurrentDataBlock;
  _ilu_ReleaseMutex(ilu_prmu);
  ILU_CLER(*err);
  return (protocol);
}

ilu_Protocol _ilu_bsunrpc_Protocol(ilu_ProtocolInfo pinfo,
				   ilu_Error *err)
{
  /*L1 >= {prmu}*/
  static ilu_Protocol protocol = NIL;
  _ilu_AcquireMutex(ilu_prmu);
  if (protocol == NIL)
    protocol = _sunrpc_NewSunRPC();
  protocol->pr_form_handle = _bsunrpc_FormProtocolHandle;
  protocol->pr_create_data_block = _sunrpc_CreateBatchingNonConcurrentDataBlock;
  _ilu_ReleaseMutex(ilu_prmu);
  ILU_CLER(*err);
  return (protocol);
}

ilu_Protocol _ilu_bcsunrpc_Protocol(ilu_ProtocolInfo pinfo,
				    ilu_Error *err)
{
  /* ignore pinfo */

  /*L1 >= {prmu}*/
  static ilu_Protocol protocol = NIL;
  _ilu_AcquireMutex(ilu_prmu);
  if (protocol == NIL)
    protocol = _sunrpc_NewSunRPC();
  protocol->pr_form_handle = _bcsunrpc_FormProtocolHandle;
  protocol->pr_concurrent_requests = ilu_TRUE;
  protocol->pr_create_data_block = _sunrpc_CreateBatchingConcurrentDataBlock;
  _ilu_ReleaseMutex(ilu_prmu);
  ILU_CLER(*err);
  return (protocol);
}
