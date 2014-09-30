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
 * Zlib Transport Filter for ILU
 *
 * Paul Bennett <paul.bennett@bt.com>
 * Thursday, 18 June 1998
 *
 * Some bizarre BT copyright statement ought to appear here.
 *
 * $Id: zlib.c,v 1.13 1999/08/03 01:53:05 janssen Exp $
 */
/* Last edited by Mike Spreitzer September 13, 1998 9:27 pm PDT */

/*
 * This is a compression transport filter for Xerox PARC's ILU system.
 * The code has been developed as part of the World Wide Web Consortium's
 * Hypertext Transfer Protocol - Next Generation (HTTP-NG) project.
 *
 * 	ILU:		ftp://ftp.parc.xerox.com/pub/ilu/ilu.html
 * 	HTTP-NG:	http://www.w3.org/Protocols/HTTP-NG/
 * 	zlib:		http://www.cdrom.com/pub/infozip/zlib/
 *
 * Points where I've been particularly hacky and taken a short-cut
 * or some such are marked !!!pab!!!.
 *
 * The canonical form is the name "zlib" followed by underscore-separeted
 * parameters.  The parameters are:
 *
 *	boundaried	- zlib is boundaried
 *	nonboundaried	- zlib is not boundaried
 *	0-9		- compression level; 0 worst -> 9 best
 *	10+		- buffer size
 *
 * The zlib layer can be boundaried if and only if the lower layer is.
 *
 * If no parameters are specified, the zlib's compression level
 * is Z_DEFAULT_COMPRESSION (defined in <zlib.h>),
 * buffer size is 1024 (Z_BUFSZ, defined below),
 * and the zlib is boundaried iff the lower layer is.
 *
 * Some examples:
 *
 *	zlib_0		- default buffer size, no compression
 *	zlib_9_2048	- best compression, buffer size 2048
 *
 * Note that some magic bytes are sent down the wire so that the decoding
 * end can use the correct decompression level; thus it *is* possible that
 * this transport will increase the on-the-wire byte size, but very unlikely
 * unless you're using a compression level of 0.
 *
 * Also, there may be some scope in having asymmetric compression levels.
 * Currently both streams (input and output) use the same compression level.
 */

/*
 * The code here is based on $ILUSRC/runtime/kernel/simptran.c by
 * Bill Janssen of Xerox.  The original copyright message follows.
 */

/*======================================================================*/
/*
*/
/*======================================================================*/

#include "iluntrnl.h"

#include "ilutransport.h"

#include <zlib.h>

#define MSGORNIL(m) ((m)?(m):"(null)")

/*
 * *SIGH* Zlib requires memory management routines with plain weird
 * prototypes.  We can't get away with just passing the ILU ones in,
 * so we have to patch the calls here.
 */
static voidpf _zlibtran_alloc(voidpf opaque, uInt items, uInt size)
  {return ilu_malloc(items*size);}
static void _zlibtran_free(voidpf opaque, voidpf address)
  {ilu_free(address);}

#define BUFFERSIZE	1024
#define Z_BUFSZ		1024

typedef struct {
  ilu_TransportCreator	lower;
  ilu_boolean		lower_boundaried;
  ilu_boolean		boundaried;
  ilu_cardinal		compression_level;
  ilu_cardinal		buffer_size;
}              *ZlibtranCreatorParms;

typedef struct {
  ilu_Mooring		lower;
  ilu_boolean		lower_boundaried;
  ilu_boolean		boundaried;
  ilu_cardinal		compression_level;
  ilu_cardinal		buffer_size;
}              *ZlibtranMooringParms;

typedef struct {

  /* L2 >= {ymu} */

  ilu_boolean	busyIn;	/* currently processing input msg? */
  ilu_TransportReport trLower;

  /* L2 >= {xmu} */

  ilu_boolean	busyOut;

  /* L2 unconstrained */

  /*
   * The unboundaried transport from which (self) is constructed. We
   * don't want an exposed buffer in (lower).  lower's xmu = self's
   * xmu; same for ymu.
   */
  ilu_Transport	lower;
  ilu_boolean	lower_boundaried;

  /*
   * These structures hold the state of the compression library
   */
  z_stream     *zstr_in;
  z_stream     *zstr_out;

  ilu_cardinal	compression_level;
  ilu_cardinal	buffer_size;

  /*
   * Buffers for use on the lower-level of the transport stack
   * (these hold compressed bytes).
   */
  ilu_bytes	zbuf_in;	/* holds uncompressed data */
  
  ilu_bytes	zbuf_out;
  /*
   * Holds compressed data.  [zbuf_out, zstr_out->next_out) need to
   * be written before any other output operations are performed.
   */
  
}              *ZlibtranParms;
/* What goes in the data field of a ilu_Transport.  If boundaried,
   while closed, exposed input buffer is empty. */

/*L1, L2 unconstrained*/

#define ACCESS_ZLIBTRAN_PARMS(a) ((ZlibtranParms)(a))

/*L1.sup < trmu; L2 unconstrained*/
static          ZlibtranCreatorParms
_zlibtran_InterpretInfo(ilu_TransportInfo info,
			ILU_ERRS((no_memory, inv_objref)) * err)
{
  ZlibtranCreatorParms cp;
  ilu_TransportCreator lower;

  ilu_boolean     boundaried = ilu_FALSE;
  ilu_integer     compression_level = Z_DEFAULT_COMPRESSION;
  ilu_cardinal    buffer_size = Z_BUFSZ;

  if (info[0] == NIL || info[1] == NIL || strncmp(info[0], "zlib", 4) != 0
      || !(info[0][4]=='_' || !info[0][4]) )
    /* We check info[1] because "zlib" is not a transport endpoint */
    return ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_ts, NIL);

  {
    /* Break apart the tinfo. */
    char           *next, *prev;
    next = info[0]+4;
    while (*next) {
      int n;
      prev = next+1;
      next = strchr(prev, '_');
      if (next)
	n = next - prev;
      else
	next = prev + (n = strlen(prev));
      if (strncmp(prev, "boundaried", n) == 0)
	boundaried = ilu_TRUE;
      else if (strncmp(prev, "nonboundaried", n) == 0)
	boundaried = ilu_FALSE;
      else {
	long int             i;
	if (sscanf(prev, "%ld", &i) == 1) {
	  if (i <= Z_BEST_COMPRESSION)
	    compression_level = i;
	  else
	    buffer_size = i;
	} else
	  return ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_ts, NIL);
      }
    }
  }

  lower = _ilu_GetTransportCreator(info + 1, err);
  if (ILU_ERRNOK(*err))
    return NIL;
  if ((!lower->tcr_reliable) || (boundaried  != lower->tcr_boundaried)) {
    (*lower->tcr_close) (lower);
    return ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_ts, NIL);
  }
  cp = (ZlibtranCreatorParms) ilu_MallocE(sizeof(*cp), err);
  if (cp == NIL) {
    (*lower->tcr_close) (lower);
    return NIL;
  }
  cp->lower = lower;
  cp->lower_boundaried = lower->tcr_boundaried;
  cp->boundaried = boundaried;
  cp->compression_level = compression_level;
  cp->buffer_size = buffer_size;
  return cp;
}

/* L1.sup < trmu; L2 >= {xmu, ymu} */
static          ilu_boolean
_zlibtran_SetInputHandler(ilu_Transport self, ilu_TIH tih,
			  ILU_ERRS((no_memory, internal, no_resources)) * err)
{
  ZlibtranParms   p = ACCESS_ZLIBTRAN_PARMS(transport_data(self));
  if (p->zstr_in->avail_in)
    return !ILU_CLER(*err);
  return _ilu_SetTransportInputHandler(p->lower, tih, err);
}

/* Main Invariant holds; L2 >= {ymu} */
static          ilu_boolean
_zlibtran_WaitForInput(ilu_Transport self, int *disabled,
		       ilu_FineTime * limit,
		       ILU_ERRS((broken_locks, interrupted)) * err)
{
  ZlibtranParms   p = ACCESS_ZLIBTRAN_PARMS(transport_data(self));
  if (p->zstr_in->avail_in) {
    if (disabled)
      *disabled = ilu_TRUE;
    return ILU_CLER(*err);
  }
  return (transport_wait_for_input(p->lower, disabled, limit, err));
}

/* L1.sup < trmu; L2 >= {xmu} */
static          ilu_boolean
_zlibtran_InterruptST(ilu_Transport self, ILU_ERRS((bad_param)) * err)
{
  ZlibtranParms   p = ACCESS_ZLIBTRAN_PARMS(transport_data(self));
  return (transport_interruptST(p->lower, err));
}

/* L1 >= {cmu}; L1.sup < trmu; L2 >= {xmu} */

static          ilu_boolean
_zlibtran_DisableWait(ilu_Transport self,
		      ILU_ERRS((broken_locks, bad_param,
				internal)) * err)
{
  ZlibtranParms   p = ACCESS_ZLIBTRAN_PARMS(transport_data(self));
  return (transport_disableWait(p->lower, err));
}

static          ilu_boolean
_zlibtran_EnableWait(ilu_Transport self,
		     ILU_ERRS((broken_locks, bad_param,
			       internal)) * err)
{
  ZlibtranParms   p = ACCESS_ZLIBTRAN_PARMS(transport_data(self));
  return (transport_enableWait(p->lower, err));
}

/* L1.sup < trmu; L2 unconstrained */

static          ilu_integer
_zlibtran_FdUsage(ilu_TransportCreator self, ilu_boolean mooring)
{
  ZlibtranCreatorParms    cp = (ZlibtranCreatorParms) self->tcr_data;
  return (*cp->lower->tcr_dfd) (cp->lower, mooring);
}

/* mayBlock ? Main Invariant : L1.sup < trmu */
/* L2 >= {xmu}; input => L2 >= {ymu} */

static          ilu_boolean
ClearOut(ilu_Transport self, ZlibtranParms p,
	 ilu_boolean flush, ilu_boolean * flushed,
	 ilu_cardinal mayBlock, ILU_ERRS((IoErrs)) * err)
{
  if (p->zstr_out->next_out > p->zbuf_out) {
    ilu_cardinal    nOut, wrote;
    nOut = p->zstr_out->next_out - p->zbuf_out;
    wrote = (transport_write_bytes_full
	     (p->lower, p->zbuf_out, nOut, flush, flushed,
	      mayBlock, err));
    ILU_NOTE(ZLIB_DEBUG,
	     ("zlib(%p) wrote %lu of %lu queued bytes%s.\n",
	      self, (long unsigned) wrote, (long unsigned) nOut,
	 flush ? (*flushed ? ", flushed" : ", not flushed") : ""));
    if (wrote < nOut && ILU_ERROK(*err)) {
      memmove((void *) p->zbuf_out,
	      (void *) (p->zbuf_out + wrote),
	      nOut - wrote);
      p->zstr_out->avail_out += wrote;
    } else {
      p->zstr_out->next_out = p->zbuf_out;
      p->zstr_out->avail_out = p->buffer_size;
    }
    return ILU_CLER(*err);
  } else {
    if (!flush)
      ILU_CLER(*err);
    else if (mayBlock)
      *flushed = (*p->lower->tr_class->tc_push) (p->lower, err);
    else
      (void) ((*p->lower->tr_class->tc_write_bytes_nonblock)
	      (p->lower, NIL, 0, ilu_TRUE, flushed, err));
    return ILU_ERROK(*err);
  }
}

static          ilu_ReadHeaderResultCode
_zlibtran_BeginMessage_Full(ilu_Transport self,
			    ilu_boolean input_p,
			    ilu_boolean mayBlock,
			    ILU_ERRS((IoErrs)) * err)
{
  ZlibtranParms   p = ACCESS_ZLIBTRAN_PARMS(transport_data(self));
  if (!self->tr_class->tc_boundaried) {
    return ILU_ERR_CONS1(internal, err, minor, ilu_im_tcNotBoundaried,
			 ilu_rhrc_error);
  }
  if (p->busyIn || p->busyOut)
    return ILU_ERR_CONS1(internal, err, minor, ilu_im_beginMessage,
			 ilu_rhrc_error);
  if (!ilu_Check((p->zstr_out->next_out == p->zbuf_out
		  && p->lower_boundaried),
		 err))
    return ilu_rhrc_error;
  if (!(input_p || mayBlock)) {
    if (!transport_begin_output_nonblock(p->lower, err))
      return ilu_rhrc_error;
  } else {
    ilu_ReadHeaderResultCode result;
    result = transport_begin_message(p->lower, input_p, err);
    if (result != ilu_rhrc_ok)
      return result;
  }
  if (input_p) {
    p->busyIn = ilu_TRUE;
    p->trLower.tr_eom = p->trLower.tr_eof = ilu_FALSE;
  } else {
    p->busyOut = ilu_TRUE;
    self->tr_outLimit = BUFFERSIZE;
    self->tr_outNext = 0;
  }
  ILU_CLER(*err);
  ILU_NOTE(ZLIB_DEBUG, ("zlib(%p) %s message begun.\n",
			self, input_p ? "input" : "output"));
  return ilu_rhrc_ok;
}

static          ilu_ReadHeaderResultCode
_zlibtran_BeginMessage(ilu_Transport self,
		       ilu_boolean input_p,
		       ILU_ERRS((IoErrs)) * err)
{
  return _zlibtran_BeginMessage_Full(self, input_p, ilu_TRUE, err);
}

static          ilu_boolean
_zlibtran_BeginOutputMessageNonblock(ilu_Transport self,
				     ILU_ERRS((IoErrs)) * err)
{
  return (_zlibtran_BeginMessage_Full(self, ilu_FALSE, ilu_FALSE, err)
	  == ilu_rhrc_ok);
}

static          ilu_cardinal
WriteBuf(ilu_Transport self, ZlibtranParms p, ilu_bytes buf,
	 ilu_cardinal len, ilu_boolean flush, ilu_boolean * flushed,
	 ilu_cardinal mayBlock, ILU_ERRS((IoErrs)) * err)
{
  z_stream       *zout = p->zstr_out;
  ilu_cardinal    taken = 0, toWrite, dWrote, offset = zout->total_in;
  ilu_boolean     dun = ilu_FALSE;
  if (!ilu_Check(zout->next_out == p->zbuf_out &&
		 zout->avail_out == p->buffer_size,
		 err))
    return 0;
  zout->next_in = buf;
  zout->avail_in = len;
  ILU_NOTE(ZLIB_DEBUG,
	   ("zlib(%p) compressing %lu bytes, %sflushing, may%s block.\n",
	    self, (long unsigned) len,
	    flush ? "" : "not ",
	    mayBlock ? "":" not"));
  while (ILU_ERROK(*err) && !dun) {
    int             zerr;
    long unsigned    totOut0 = zout->total_out, nOut, wrote0;
    long unsigned    totIn0 = zout->total_in;
    long unsigned    avIn0 = zout->avail_in;
    long unsigned    avOut0 = zout->avail_out;
    ilu_bytes        nextOut0 = zout->next_out;
    zerr = deflate(zout, flush ? Z_SYNC_FLUSH : Z_NO_FLUSH);
    nOut = zout->next_out - nextOut0;
    ILU_NOTE(ZLIB_DEBUG,
	     ("zlib(%p) compressor consumed plain bytes [%lu,same+%lu) out of same+%lu,"
	      " produced compressed bytes [%lu,same+%lu) limited by same+%lu,"
	      " returned code %d, msg=%s.\n",
	      self, totIn0, avIn0 - zout->avail_in, avIn0,
	      totOut0, nOut, avOut0, zerr, MSGORNIL(zout->msg)));
    taken = len - zout->avail_in;
    if (zerr != Z_OK) {
      /* return ILU_ERR_CONS1(zlib, err, code, zerr, ret); */
      return ILU_ERR_CONS0(unknown, err, taken);
    }
    if (!ilu_Check(zout->avail_in == 0 ||
		   zout->avail_out == 0, err))
      return taken;
    toWrite = zout->next_out - p->zbuf_out;
    wrote0 = zout->total_out - toWrite;
    dWrote = (transport_write_bytes_full
	     (p->lower, p->zbuf_out, toWrite, flush, flushed,
	      mayBlock, err));
    ILU_NOTE(ZLIB_DEBUG,
	     ("zlib(%p) wrote compressed bytes [%lu,same+%lu) out of same+%lu.\n",
	      self, wrote0, (long unsigned) dWrote, toWrite));
    if (dWrote < toWrite && ILU_ERROK(*err)) {
      memmove((void *) p->zbuf_out,
	      (void *) (p->zbuf_out + dWrote),
	      toWrite - dWrote);
      zout->next_out -= dWrote;
      zout->avail_out += dWrote;
      break;
    }
    dun = (zout->avail_out && !zout->avail_in);
    zout->next_out = p->zbuf_out;
    zout->avail_out = p->buffer_size;
  }
#ifdef ENABLE_DEBUGGING
  if (((ilu_DebugLevel & PACKET_DEBUG) != 0) &&
      (taken > 0))
    _ilu_debug_DumpPacket_Offset(buf, taken,
				 offset, "uncompressed outgoing");
#endif				/* ENABLE_DEBUGGING */
  return taken;
}

static          ilu_boolean
_zlibtran_EndMessage_Full(ilu_Transport self,
			  ilu_boolean flush,
			  ilu_boolean * flushed,
			  ilu_Message * msgh,
			  ilu_boolean mayBlock,
			  ILU_ERRS((IoErrs)) * err)
{
  ZlibtranParms   p;
  ilu_boolean     ans, bytesDropped = ilu_FALSE;
  if (!self->tr_class->tc_boundaried) {
    return ILU_ERR_CONS1(internal, err, minor, ilu_im_tcNotBoundaried,
			 ilu_rhrc_error);
  }
  p = ACCESS_ZLIBTRAN_PARMS(transport_data(self));

  if ((!p->busyOut) && (!p->busyIn))
    return ILU_ERR_CONS1(internal, err, minor, ilu_im_endMessage,
			 ilu_FALSE);

  if (p->busyOut) {
    if (!ClearOut(self, p, flush && !self->tr_outNext, flushed,
		  mayBlock, err))
      return ilu_FALSE;
    if (self->tr_outNext > 0) {
      ilu_cardinal    wrote;
      wrote = WriteBuf(self, p, self->tr_outBuff, self->tr_outNext,
		       flush, flushed, mayBlock, err);
      if (ILU_ERRNOK(*err))
	return ilu_FALSE;
    }
    self->tr_outNext = 0;
    p->busyOut = ilu_FALSE;
    ILU_NOTE(ZLIB_DEBUG,
	     ("zlib(%p) ended output message (flush=%d).\n",
	      self, flush));
  } else if (p->busyIn) {
    ilu_TransportReport rpt = {ilu_FALSE};
    bytesDropped = self->tr_inNext < self->tr_inLimit;
    self->tr_inNext = self->tr_inLimit = 0;
    while (!rpt.tr_eom) {
      ilu_cardinal nRead;
      nRead = ((*self->tr_class->tc_read_bytes)
	       (self, NIL, 0, &rpt, err));
      if (nRead)
	bytesDropped = ilu_TRUE;
      if (ILU_ERRNOK(*err))
	return ilu_FALSE;
      if (rpt.tr_eom)
	break;
      if (self->tr_inLimit)
	self->tr_inLimit = 0;
      else {
	if (!_ilu_TransportWaitForInputNoClose(self, NIL, err))
	  return ilu_FALSE;
      }
    };
    self->tr_inLimit = 0;
    p->busyIn = ilu_FALSE;
    ILU_NOTE(ZLIB_DEBUG,
	     ("zlib(%p) ended input message.\n", self));
  };
  ans = transport_end_message(p->lower, flush, msgh, err);
  if (bytesDropped & ILU_ERROK(*err))
    ILU_ERR_CONS1(internal, err, minor, ilu_im_tcBytesDropped, (void) 6);
  return ans;
}

static          ilu_boolean
_zlibtran_EndMessage(ilu_Transport self,
		     ilu_boolean flush,
		     ilu_Message * msgh,
		     ILU_ERRS((IoErrs)) * err)
{
  ilu_boolean     flushed;
  return _zlibtran_EndMessage_Full(self, flush, &flushed, msgh,
				   ilu_TRUE, err);
}

static          ilu_TransportEndReport
_zlibtran_EndOutputMessageNonblock(ilu_Transport self,
				   ilu_boolean flush,
				   ilu_Message * msgh,
				   ILU_ERRS((IoErrs)) * err)
{
  ilu_TransportEndReport ans;
  ans.iluter_ended = (_zlibtran_EndMessage_Full
		      (self, flush, &ans.iluter_flushed,
		       msgh, ilu_TRUE, err));
  return ans;
}

/*Main Invariant holds; L2 >= {xmu}*/

static ilu_boolean
  _zlibtran_Push (ilu_Transport self,
		  ILU_ERRS((IoErrs)) * err)
{
  return ((*self->tr_class->tc_write_bytes)
	  (self, NIL, 0, ilu_TRUE, err));
}

static          ilu_boolean
_zlibtran_SendWholeMessage(ilu_Transport self, ilu_Message * msgh,
			   ILU_ERRS((IoErrs)) * err)
{
  return ILU_ERR_CONS1(internal, err, minor, ilu_im_tcReliable, ilu_FALSE);
}

/* mayBlock ? Main Invariant : L1.sup < trmu; L2 >= {xmu} */
static          ilu_cardinal
_zlibtran_WriteBytes_Full(ilu_Transport self, ilu_bytes b,
			  ilu_cardinal bufferSize,
			  ilu_boolean flush,
			  ilu_boolean * flushed,
			  ilu_boolean mayBlock,
			  ILU_ERRS((IoErrs)) * err)
{
  ZlibtranParms   p = ACCESS_ZLIBTRAN_PARMS(transport_data(self));
  ilu_cardinal    wrote, taken = 0;

  if (self->tr_class->tc_boundaried && (!p->busyOut))
    return ILU_ERR_CONS1(internal, err, minor, ilu_im_bytesWithoutMsg,
			 0);
  if (!ClearOut(self, p, (flush
			  && !(self->tr_outBuff && self->tr_outNext)
			  && !bufferSize),
		flushed, mayBlock, err))
    return 0;
  if (self->tr_outBuff && self->tr_outNext) {
    wrote = WriteBuf(self, p, self->tr_outBuff, self->tr_outNext,
		     flush && bufferSize == 0, flushed,
		     mayBlock, err);
    if (wrote < self->tr_outNext) {
      self->tr_outNext -= wrote;
      memmove((void *) self->tr_outBuff,
	      (void *) (self->tr_outBuff + wrote),
	      self->tr_outNext);
      return 0;
    }
    self->tr_outNext = 0;
  }
  if (bufferSize && p->zstr_out->next_out == p->zbuf_out)
    taken = WriteBuf(self, p, b, bufferSize, flush, flushed,
		     mayBlock, err);
  return taken;
}

static          ilu_boolean
_zlibtran_WriteBytes(ilu_Transport self, ilu_bytes b,
		     ilu_cardinal bufferSize,
		     ilu_boolean flush,
		     ILU_ERRS((IoErrs)) * err)
{
  ilu_boolean     flushed;
  (void) _zlibtran_WriteBytes_Full(self, b, bufferSize, flush, &flushed,
				   ilu_TRUE, err);
  return ILU_ERROK(*err);
}

/*L1.sup < trmu; L2 >= {xmu}*/
static          ilu_cardinal
_zlibtran_WriteBytesNonblock(ilu_Transport self, ilu_bytes b,
			     ilu_cardinal bufferSize,
			     ilu_boolean flush,
			     ilu_boolean * flushed,
			     ILU_ERRS((IoErrs)) * err)
{
  return _zlibtran_WriteBytes_Full(self, b, bufferSize, flush, flushed,
				   ilu_FALSE, err);
}

/*Main Invariant holds; L2 >= {xmu, ymu}*/

static          ilu_cardinal
_zlibtran_ReadBytes(ilu_Transport self,
		    ilu_bytes buffer,
		    ilu_cardinal len,
		    ilu_TransportReport * rpt,
		    ILU_ERRS((IoErrs,zlib)) * err)
{
  ZlibtranParms   p = ACCESS_ZLIBTRAN_PARMS(transport_data(self));
  z_stream       *zin = p->zstr_in;
  ilu_cardinal    ans, effLen, offset = zin->total_out;
  ilu_bytes       dest;
  if (self->tr_class->tc_boundaried && (!p->busyIn))
    return ILU_ERR_CONS1(internal, err, minor, ilu_im_bytesWithoutMsg,
			 0);
  if (self->tr_inNext < self->tr_inLimit)
    return ILU_ERR_CONS1(internal, err, minor, ilu_im_tcInputSkipsBuff,
			 0);
  if (buffer) {
    zin->next_out = dest = buffer;
    zin->avail_out = effLen = len;
  } else {
    zin->next_out = dest = self->tr_inBuff;
    zin->avail_out = effLen = p->buffer_size;
    self->tr_inNext = self->tr_inLimit = 0;
  }
  rpt->tr_eom = rpt->tr_eof = ilu_FALSE;
  if (zin->avail_out == 0) {
    ILU_CLER(*err);
    return 0;
  }
  /* Loop inspired by the one in gzread in gzio.c. */
  while (1) {
    int             zerr;
    ilu_cardinal    totIn0 = zin->total_in;
    ilu_cardinal    totOut0 = zin->total_out;
    ilu_cardinal    availIn0, dIn, dOut;
    ilu_cardinal    availOut0 = zin->avail_out;
    /* Get more bytes from the lower layer if needed. */
    if (zin->avail_in == 0 && !p->trLower.tr_eom) {
      zin->next_in = p->zbuf_in;
      zin->avail_in = (transport_read_upto_bytes
		       (p->lower, zin->next_in,
			p->buffer_size, &p->trLower, err));
      if (p->trLower.tr_eof)
	p->trLower.tr_eom = ilu_TRUE;
      ILU_NOTE(ZLIB_DEBUG,
	       ("zlib(%p) got %lu bytes from lower"
		" (whose eom=%d, eof=%d).\n",
		self, zin->avail_in,
		p->trLower.tr_eom, p->trLower.tr_eof));
      if (ILU_ERRNOK(*err))
	goto dun;
    }
    availIn0 = zin->avail_in;
    zerr = inflate(zin, Z_SYNC_FLUSH);
    dIn = availIn0 - zin->avail_in;
    dOut = availOut0 - zin->avail_out;
    ILU_NOTE(ZLIB_DEBUG,
	     ("zlib(%p) expander consumed compressed bytes [%lu,same+%lu)"
	      " out of same+%lu,"
	      " produced plain bytes [%lu,same+%lu) limited to same+%lu,"
	      " returned code %d, msg=%s.\n",
	      self, totIn0, dIn, availIn0,
	      totOut0, dOut, availOut0, zerr, MSGORNIL(zin->msg)));
    if (!(zerr == Z_OK || zerr==Z_BUF_ERROR && !availIn0 && !dOut)) {
      ILU_ERR_CONS0(unknown, err, (void) 6);
      goto dun;
    }
    if (!ilu_Check(zin->avail_in == 0 || zin->avail_out == 0, err))
      goto dun;
    if (!zin->avail_out || !availIn0
	|| ((!zin->avail_in) && p->trLower.tr_eom))
      break;
  }
dun:
  rpt->tr_eof = zin->avail_out && p->trLower.tr_eof;
  if (self->tr_class->tc_boundaried)
    rpt->tr_eom = zin->avail_out && p->trLower.tr_eom;
  else
    rpt->tr_eom = ilu_FALSE;
  if (buffer) {
    ans = zin->next_out - buffer;
  } else {
    ans = (self->tr_inLimit = zin->next_out - self->tr_inBuff);
  }
  ILU_NOTE(ZLIB_DEBUG,
	   ("zlib(%p) delivered plaintext bytes [%lu,same+%lu)"
	    " into buffer of size %lu.\n",
	    self, (long unsigned) offset, (long unsigned) ans,
	    (long unsigned) effLen));
#ifdef ENABLE_DEBUGGING
  if (((ilu_DebugLevel & PACKET_DEBUG) != 0) &&
      (ans > 0))
    _ilu_debug_DumpPacket_Offset(dest, ans, offset,
				 "uncompressed incoming");
#endif				/* ENABLE_DEBUGGING */
  return ans;
}

/*L1.sup < trmu; L2 >= {xmu, ymu}*/

static          ilu_boolean
_zlibtran_Close(ilu_Transport self, ilu_integer * dfd,
		ILU_ERRS((bad_locks, broken_locks, internal)) * err)
{
  ZlibtranParms   p = ACCESS_ZLIBTRAN_PARMS(transport_data(self));
  ilu_Transport   lt = p->lower;
  int             zerr;

  ILU_NOTE(ZLIB_DEBUG, ("zlib(%p) closing.\n", self));

  /*
   * Close down the zlib streams
   */

  if ((zerr = deflateEnd(p->zstr_out)) != Z_OK)
    ILU_NOTE(ZLIB_DEBUG,
	     ("zlib(%p): deflateEnd(out) returned %d, msg=%s.\n",
	      self, zerr, MSGORNIL(p->zstr_out->msg)));

  if ((zerr = inflateEnd(p->zstr_in)) != Z_OK)
    ILU_NOTE(ZLIB_DEBUG,
	     ("zlib(%p): inflateEnd(in) returned %d, msg=%s.\n",
	      self, zerr, MSGORNIL(p->zstr_in->msg)));

  ilu_free(p->zstr_out);
  ilu_free(p->zstr_in);
  ilu_free(p->zbuf_out);
  ilu_free(p->zbuf_in);

  ilu_free(p);
  ilu_free(self);
  return transport_close(lt, dfd, err);
}

/*L1, L2 unconstrained*/

static struct _ilu_TransportClass_s myclass = {
  ilu_TRUE,			/* boundaried */
  ilu_TRUE,			/* reliable */
  _zlibtran_SetInputHandler,
  _zlibtran_WaitForInput,
  _zlibtran_InterruptST,
  _zlibtran_DisableWait,
  _zlibtran_EnableWait,
  _zlibtran_BeginMessage,
  _zlibtran_EndMessage,
  _zlibtran_BeginOutputMessageNonblock,
  _zlibtran_EndOutputMessageNonblock,
  _zlibtran_Push,
  _zlibtran_SendWholeMessage,
  _zlibtran_WriteBytes,
  _zlibtran_WriteBytesNonblock,
  _zlibtran_ReadBytes,
  _zlibtran_Close
};

static ilu_TransportClass
  NewClass (ilu_boolean boundaried, ilu_Error *err)
{
  ilu_TransportClass n;

  if ((n = (ilu_TransportClass) ilu_MallocE(sizeof(*n), err)) == NIL)
    return NIL;
  *n = myclass;
  n->tc_boundaried = boundaried;
  return n;
}

/*L1.sup < trmu; L2 >= result's {xmu, ymu}*/
static          ilu_Transport
NewTrans(ilu_Transport lower,
	 ilu_boolean boundaried, ilu_boolean lower_boundaried,
	 ilu_cardinal compression_level, ilu_cardinal buffer_size,
	 ilu_integer * dfd,
	 ILU_ERRS((no_memory, bad_param)) * err)
{
  ilu_Transport   ans = NIL;
  ZlibtranParms   parms = NIL;

  if (boundaried && !lower_boundaried)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, NIL);
  parms = (ZlibtranParms) ilu_MallocE(sizeof(*parms), err);
  if (parms == NIL)
    return NIL;
  parms->lower = lower;
  parms->lower_boundaried = lower_boundaried;
  parms->busyIn = ilu_FALSE;
  parms->busyOut = ilu_FALSE;
  parms->trLower.tr_eof = parms->trLower.tr_eom = ilu_FALSE;

  /*
   * Zlib initialisation.  First the algorithm parameters
   * - these should really be taken from the tinfo.
   */
  parms->compression_level = compression_level;
  parms->buffer_size = buffer_size;

  /* Next the malloc'ing */
  
  parms->zstr_in = parms->zstr_out = NIL;
  parms->zbuf_in = parms->zbuf_out = NIL;
  
  if (   NIL == (parms->zstr_in =
		 (z_stream *) ilu_MallocE(sizeof(*parms->zstr_in), err))
      || NIL == (parms->zstr_out =
		 (z_stream *) ilu_MallocE(sizeof(*parms->zstr_out), err))
      || NIL == (parms->zbuf_in =
		 (ilu_bytes) ilu_MallocE(parms->buffer_size, err))
      || NIL == (parms->zbuf_out =
		 (ilu_bytes) ilu_MallocE(parms->buffer_size, err)))
    goto fale_2;
  
  /* Now initialise the zstreams */
  
  parms->zstr_in->zalloc = parms->zstr_out->zalloc = _zlibtran_alloc;
  parms->zstr_in->zfree  = parms->zstr_out->zfree  = _zlibtran_free;
  parms->zstr_in->opaque = parms->zstr_out->opaque = 0;
  
  parms->zstr_in->next_in  = parms->zbuf_in;
  parms->zstr_in->avail_in  = 0;
  parms->zstr_out->next_in = NIL;
  parms->zstr_out->avail_in = 0;

  if (Z_OK != deflateInit(parms->zstr_out, parms->compression_level))
    goto fale_1;
  if (Z_OK != inflateInit(parms->zstr_in))
    goto fale0;
  
  parms->zstr_out->next_out = parms->zbuf_out;
  parms->zstr_out->avail_out = parms->buffer_size;
  
  /*
   * That's it for the zlib stuff... back to the main plot.
   */
  
  ans = (ilu_Transport) ilu_MallocE(sizeof(*ans), err);
  if (ans == NIL)
    goto fale1;
  ans->tr_outBuff = (unsigned char *) ilu_MallocE(BUFFERSIZE, err);
  if (ans->tr_outBuff == NIL) { goto fale2; };
  ans->tr_inBuff = NIL;
  ans->tr_inNext = ans->tr_inLimit = 0;
  ans->tr_outNext = 0;
  ans->tr_outLimit = BUFFERSIZE;
  if ((ans->tr_class = NewClass(boundaried, err)) == NIL)
    goto fale3;  
  ans->tr_data = parms;
  ans->tr_wc = lower->tr_wc;
  ans->tr_estFDs = lower->tr_estFDs;
  ILU_CLER(*err);
  ILU_NOTE(ZLIB_DEBUG,
	   ("zlib(%p) created, lower=%p, level=%d.\n",
	    ans, lower, parms->compression_level));
  return ans;
 fale3:
  ilu_free(ans->tr_outBuff);
 fale2:
  ilu_free(ans);

  /* zlib failure */
 fale1:
  inflateEnd(parms->zstr_in);
 fale0:
  deflateEnd(parms->zstr_out);
 fale_1:
  if (parms->zbuf_out != NIL) ilu_free(parms->zbuf_out);
  if (parms->zbuf_in  != NIL) ilu_free(parms->zbuf_in);
  if (parms->zstr_out != NIL) ilu_free(parms->zstr_out);
  if (parms->zstr_in  != NIL) ilu_free(parms->zstr_in);
  /* back to the main story */
  
 fale_2:{
   ILU_ERRS((bad_locks, broken_locks, internal)) cerr;
   ilu_integer     cdfd = 0;
   ilu_free(parms);
   transport_close(lower, &cdfd, &cerr);
   ILU_HANDLED(cerr);
   *dfd += cdfd;
   return NIL;
 }
}

/* Main Invariant holds */
static          ilu_Transport
_zlibtran_CreateTransport(ilu_TransportCreator self, ilu_boolean buffer,
			  ilu_integer *dfd, ilu_Passport pp,
			  ILU_ERRS((IoErrs)) * err)
{
  ZlibtranCreatorParms    cp = (ZlibtranCreatorParms) self->tcr_data;
  ilu_Transport   lower;
  lower = (*cp->lower->tcr_createTransport) (cp->lower, buffer, dfd, pp, err);
  if (ILU_ERRNOK(*err))
    return NIL;
  if (!transport_reliable(lower))
    /* He promised! (We checked earlier) */
    return ILU_ERR_CONS1(internal, err, minor, ilu_im_tcBug, NIL);

  /*
   * Initialise the zlib stream objects
   */
  
  /* pretend to acquire result's xmu, ymu */
  return NewTrans(lower, cp->boundaried, cp->lower_boundaried,
		  cp->compression_level, cp->buffer_size, dfd, err);
  /* pretend to release result's xmu, ymu */
}

/*L1.sup < trmu; L2 >= {xmu, ymu}*/
static ilu_integer
  _zlibtran_MooringDFd(ilu_Mooring self, ilu_boolean add)
{
  ZlibtranMooringParms mp = (ZlibtranMooringParms) self->mo_data;
  return ((*mp->lower->mo_dfd) (mp->lower, add));
}

/*L1.sup < trmu; L2 >= {xmu, ymu}*/
static          ilu_boolean
_zlibtran_SetReqHandler(ilu_Mooring self,
			ilu_TIH tih,
			ILU_ERRS((no_memory, imp_limit, no_resources,
				  broken_locks, internal)) * err)
{
  ZlibtranMooringParms mp = (ZlibtranMooringParms) self->mo_data;
  return ((*mp->lower->mo_set_req_handler) (mp->lower, tih, err));
}

/* Main Invariant holds; L2 >= {ymu} */
static          ilu_boolean
_zlibtran_WaitForReq(ilu_Mooring self, int *disabled,
	   ILU_ERRS((interrupted, broken_locks)) * err)
{
  ZlibtranMooringParms mp = (ZlibtranMooringParms) self->mo_data;
  return ((*mp->lower->mo_wait_for_req) (mp->lower, disabled, err));
}

/* L1 >= {cmu}; L1.sup < trmu; L2 >= {xmu} */

static          ilu_boolean
_zlibtran_DisableReqWait(ilu_Mooring self,
	       ILU_ERRS((broken_locks, bad_param, internal)) * err)
{
  ZlibtranMooringParms mp = (ZlibtranMooringParms) self->mo_data;
  return ((*mp->lower->mo_disableWait) (mp->lower, err));
}

static          ilu_boolean
_zlibtran_EnableReqWait(ilu_Mooring self,
	       ILU_ERRS((broken_locks, bad_param, internal)) * err)
{
  ZlibtranMooringParms mp = (ZlibtranMooringParms) self->mo_data;
  return ((*mp->lower->mo_enableWait) (mp->lower, err));
}

/* Main Invariant holds; L2 >= self's {xmu, ymu} */

static          ilu_Transport
_zlibtran_AcceptClient(ilu_Mooring self, ilu_string * tinfo_out,
		       ilu_integer *dfd, ilu_Passport pp,
		       ILU_ERRS((IoErrs)) * err)
{
  ZlibtranMooringParms mp = (ZlibtranMooringParms) self->mo_data;
  ilu_Transport   lower;
  ilu_string      subtinfo = NIL;
  ilu_Transport   ans = NIL;
  lower = ((*mp->lower->mo_accept_connection)
	   (mp->lower, tinfo_out ? &subtinfo : NIL, dfd, pp, err));
  if (ILU_ERRNOK(*err) || lower == NIL)
    goto dun;
  if (tinfo_out) {
    *tinfo_out = ilu_Strcat3E("zlib", " over ", subtinfo, err);
    if (ILU_ERRNOK(*err))
      goto dun;
  }
  ans = NewTrans(lower, mp->boundaried, lower->tr_class->tc_boundaried,
		 mp->compression_level, mp->buffer_size, dfd, err);
dun:
  if (subtinfo)
    ilu_free(subtinfo);
  return ans;
}

/*L1.sup < trmu; L2 >= {xmu, ymu}*/
static          ilu_boolean
_zlibtran_CloseMooring(ilu_Mooring self, ilu_integer * dfd,
		       ILU_ERRS((bad_locks, broken_locks,
				 internal)) * err)
{
  ZlibtranMooringParms mp = (ZlibtranMooringParms) self->mo_data;
  if (!(*mp->lower->mo_close) (mp->lower, dfd, err))
    return ilu_FALSE;
  ilu_free(mp);
  ilu_free(self);
  return ilu_TRUE;
}

/*L1, L2 unconstrained*/

static struct _ilu_Mooring_s mooringProto = {
  _zlibtran_MooringDFd,
  _zlibtran_SetReqHandler,
  _zlibtran_WaitForReq,
  _zlibtran_DisableReqWait,
  _zlibtran_EnableReqWait,
  _zlibtran_AcceptClient,
  _zlibtran_CloseMooring,
  NIL				/* data */
};

/*L1.sup < trmu*/
static          ilu_Mooring
_zlibtran_CreateMooring(ilu_TransportCreator self,
			ilu_TransportInfo * tinfo_out,
			ilu_boolean buffer,
			ilu_integer *dfd,
			ilu_Passport pp,	/* unused here */
			ILU_ERRS((no_memory)) * err)
{
  ZlibtranCreatorParms	cp = (ZlibtranCreatorParms) self->tcr_data;
  ZlibtranMooringParms	mp;
  ilu_Mooring     lower, ans;
  ilu_TransportInfo      subtinfo = NIL;
  lower = ((*cp->lower->tcr_createMooring)
	   (cp->lower, tinfo_out ? &subtinfo : NIL, buffer, dfd, pp, err));
  if (ILU_ERRNOK(*err))
    return NIL;
  ans = (ilu_Mooring) ilu_MallocE(sizeof(*ans), err);
  if (ans == NIL)
    return NIL;
  mp = (ZlibtranMooringParms) ilu_MallocE(sizeof(*mp), err);
  if (mp == NIL) { ilu_free(ans); return NIL; };
  if (tinfo_out) {
    char buf[1024];	/* !!!pab!!!  Don't hard-code it */
    sprintf (buf, "zlib_%s", cp->boundaried ? "boundaried" : "nonboundaried");
    if (cp->compression_level != Z_DEFAULT_COMPRESSION)
      sprintf(strchr(buf,'\0'), "_%d", cp->compression_level);
    if (cp->buffer_size != Z_BUFSZ)
      sprintf(strchr(buf,'\0'), "_%d", cp->buffer_size);
    *tinfo_out = _ilu_ConcatTinfo (buf, subtinfo, err);
    if (ILU_ERRNOK(*err))
      return NIL;
    else
      ilu_free(subtinfo);
  }
  *ans = mooringProto;
  mp->lower = lower;
  mp->lower_boundaried = cp->lower_boundaried;
  mp->boundaried = cp->boundaried;
  mp->compression_level = cp->compression_level;
  mp->buffer_size = cp->buffer_size;
  ans->mo_data = mp;
  return (ans);
}

static void _zlibtran_CloseCreator(ilu_TransportCreator self)
{
  ZlibtranCreatorParms    cp = (ZlibtranCreatorParms) self->tcr_data;
  (*cp->lower->tcr_close) (cp->lower);
  ilu_free(cp);
  ilu_free(self);
  return;
}

static struct _ilu_TransportCreator_s myCreatorProto = {
  ilu_TRUE,				/* boundaried */
  ilu_TRUE,				/* reliable */
  0,				/* tcr_holds */
  ilu_FALSE,			/* tcr_wantClose */
  _zlibtran_FdUsage,
  _zlibtran_CreateTransport,
  _zlibtran_CreateMooring,
  _zlibtran_CloseCreator,
  NIL				/* data */
};

/*L1.sup < trmu*/
ilu_TransportCreator
_ilu_zlib_TransportCreator(ilu_TransportInfo tinfo,
			   ILU_ERRS((no_memory,
				     inv_objref)) * err)
{
  ilu_TransportCreator ans;
  ZlibtranCreatorParms    cp;
  cp = _zlibtran_InterpretInfo(tinfo, err);
  if (ILU_ERRNOK(*err))
    return NIL;
  ans = (ilu_TransportCreator) ilu_MallocE(sizeof(*ans), err);
  if (ans == NIL)
    return NIL;
  *ans = myCreatorProto;
  ans->tcr_data = cp;
  ans->tcr_boundaried = cp->boundaried;
  ILU_CLER(*err);
  return ans;
}
