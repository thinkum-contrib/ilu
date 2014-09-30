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
/* $Id: sunrpcrm.c,v 1.49 1999/08/03 01:52:59 janssen Exp $ */
/* Last edited by Mike Spreitzer September 21, 1998 10:52 pm PDT */

#include "iluntrnl.h"

#include "ilutransport.h"
#include "mooring.h"

#include "oscalls.h"

typedef unsigned short ilusunrpcrmport_t;
/* WIN port needs arg of htons() to be an unsigned short */

typedef struct {
  ilu_TransportCreator lower;
}              *CreatorParms;

typedef struct {
  /* L1 unconstrained */

  /* L2 >= {ymu} */
  /* For input: */

  ilu_boolean     inWedged;	/* further input msgs forbidden? */
  ilu_boolean     lastIn;	/* current chunk is last? */
  ilu_boolean     firstIn;	/* next chunk is first? */
  ilu_cardinal    inLength;	/* of lower-level data in buffer */
  ilu_cardinal    inChunkRem;	/* how much more after inLength? */
  ilu_cardinal    inSize;	/* size of input buffer */
  /**for read:  L2 includes xmu or ymu;
     for write: L2    >=   {xmu,   ymu}*/
  ilu_boolean     busyIn;	/* currently processing input msg? */
  ilu_boolean     skipping;	/* prev msg not yet finished? */
  /*
   * ymu's invariant includes all the following.  Unconsumed
   * lower-level input bytes are at indices [tr_inNext, inLength) in
   * tr_inBuff.  "next" in def'n of (firstIn) means first chunk
   * whose header hasn't been entirely processed by this layer.
   * (skipping) means we're between messages as far as our caller is
   * concerned, but we haven't finished skipping over all the input
   * from the previous message.  When busyIn||skipping: firstIn and
   * lastIn are meaningful, and the unconsumed bytes of the current
   * chunk are: (a) at indices [tr_inNext, tr_inLimit), and then (b)
   * inChunkRem bytes yet to be read from lower level. inChunkRem>0
   * => tr_inLimit == inLength.  skipping => tr_inNext ==
   * tr_inLimit.  inChunkRem==0 and tr_inNext==tr_inLimit when
   * !busyIn && !skipping && !inWedged.  inSize is the allocated
   * size of tr_inBuff.  busyIn, skipping, and inWedged are mutually
   * exclusive.  When inWedged, no more messages may be input (we've
   * lost synch with lower layer).
   */

  /* L2 >= {xmu} */
  /* For output: */

  ilu_boolean     busyOut;	/* currently processing output msg? */
  ilu_cardinal    outFirst;	/* loc o 1st unwritten byte */
  ilu_cardinal    outStart;	/* loc o 1st payload byte of cur msg */
  ilu_cardinal    outSize;	/* size of output buffer */
  /*
   * Unwritten lower-level data are at indices [outFirst, tr_outNext) of
   * tr_outBuff.  When !busyOut, tr_outLimit==tr_outNext.  When
   * busyOut, payload so far of current message is at indices
   * [outStart, tr_outNext); header needs to be written at
   * [outStart-4, outStart).  The allocated size of tr_outBuff is
   * outSize, which is always >= tr_outLimit + (busyOut?4:8).
   */

  /* For general use: */
  
  ilu_boolean     dirIsIn;
  /*
   * xmu's invariant includes this: at most one of (busyIn),
   * (busyOut) is true; when one is, (dirIsIn) indicates which one.
   */

  /* L2 unconstrained */

  ilu_Transport   lower;
  /*
   * The unboundaried transport from which (self) is constructed. We
   * don't want an exposed buffer in (lower).  lower's xmu = self's
   * xmu; same for ymu.
   */
}              *SUNRPCRMParms;
/* What goes in the data field of a SUNRPCRM ilu_Transport. */

/*L1, L2 unconstrained*/

#define BUFFERSIZE		(8192 + 8)
#define DIRECT_THRESHOLD	1024

#define SUNRPCRMPARMS(a) ((SUNRPCRMParms)(a))

/*L1.sup < trmu; L2 unconstrained*/
static          CreatorParms
_sunrpcrm_InterpretInfo(ilu_TransportInfo info,
			ILU_ERRS((no_memory, inv_objref)) * err)
{
  CreatorParms    cp;
  ilu_TransportCreator lower;

  if (strncmp(info[0], "sunrpcrm", 8) != 0 ||
      info[1] == NIL)
    return ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_ts, NIL);
  lower = _ilu_GetTransportCreator(info + 1, err);
  if (ILU_ERRNOK(*err))
    return NIL;
  if (lower->tcr_boundaried || !lower->tcr_reliable)
    return ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_ts, NIL);
  cp = (CreatorParms) ilu_MallocE(sizeof(*cp), err);
  if (cp == NIL)
    return NIL;
  cp->lower = lower;
  return cp;
}

/* L1.sup < trmu; L2 >= {xmu, ymu} */
static          ilu_boolean
SetInputHandler(ilu_Transport self, ilu_TIH tih,
		ILU_ERRS((no_memory, internal, no_resources)) * err)
{
  SUNRPCRMParms   p = SUNRPCRMPARMS(transport_data(self));
  if (tih &&
      (p->inWedged ||
       ((self->tr_inNext + 4) <= p->inLength) ||
       (p->busyIn && p->lastIn && p->inChunkRem == 0)))
    return (ILU_CLER(*err), ilu_FALSE);
  return _ilu_SetTransportInputHandler(p->lower, tih, err);
}

/* Main Invariant holds; L2 >= {ymu} */
static          ilu_boolean
_sunrpcrm_WaitForInput(ilu_Transport self, int *disabled,
		       ilu_FineTime * limit,
		       ILU_ERRS((broken_locks, interrupted)) * err)
{
  SUNRPCRMParms   p = SUNRPCRMPARMS(transport_data(self));
  if (p->inWedged ||
      ((self->tr_inNext + 4) <= p->inLength) ||
      (p->busyIn && p->lastIn && (p->inChunkRem == 0)))
    return (*disabled = 0, ILU_CLER(*err));
  return (transport_wait_for_input(p->lower, disabled, limit, err));
}

/* L1.sup < trmu; L2 >= {xmu} */
static          ilu_boolean
_sunrpcrm_InterruptST(ilu_Transport self, ILU_ERRS((bad_param)) * err)
{
  SUNRPCRMParms   p = SUNRPCRMPARMS(transport_data(self));
  return (transport_interruptST(p->lower, err));
}

/* L1 >= {cmu}; L1.sup < trmu; L2 >= {xmu} */

static          ilu_boolean
_sunrpcrm_DisableWait(ilu_Transport self,
		      ILU_ERRS((broken_locks, bad_param,
				internal)) * err)
{
  SUNRPCRMParms   p = SUNRPCRMPARMS(transport_data(self));
  return (transport_disableWait(p->lower, err));
}

static          ilu_boolean
_sunrpcrm_EnableWait(ilu_Transport self,
		     ILU_ERRS((broken_locks, bad_param,
			       internal)) * err)
{
  SUNRPCRMParms   p = SUNRPCRMPARMS(transport_data(self));
  return (transport_enableWait(p->lower, err));
}

/* L1.sup < trmu; L2 unconstrained */

static          ilu_integer
_sunrpcrm_FdUsage(ilu_TransportCreator self, ilu_boolean mooring)
{
  CreatorParms    cp = (CreatorParms) self->tcr_data;
  return (*cp->lower->tcr_dfd) (cp->lower, mooring);
}

/*Main Invariant holds; L2 >= {xmu, ymu}*/
static          ilu_cardinal
ReadWork(ilu_Transport self,
	 ilu_bytes buffer,
	 ilu_cardinal len,
	 ilu_TransportReport * rpt,
	 int *progress,
	 ILU_ERRS((IoErrs)) * err);

/* Main Invariant holds; L2 >= {xmu}; input => L2 >= {ymu} */

static          ilu_ReadHeaderResultCode
_sunrpcrm_BeginMessage(ilu_Transport self,
		       ilu_boolean input_p,
		       ILU_ERRS((IoErrs)) * err)
{
  SUNRPCRMParms   p = SUNRPCRMPARMS(transport_data(self));
  int             progress = 1;
  ILU_NOTE(SUNRPCRM_DEBUG,
	("ILU: _sunrpcrm_BeginMessage(self=%p): %s\n",
	 self, input_p ? "input" : "output"));
  if (p->busyIn || p->busyOut)
    return ILU_ERR_CONS1(internal, err, minor, ilu_im_beginMessage,
			 ilu_rhrc_error);
  p->dirIsIn = input_p;
  if (input_p) {
    ilu_TransportReport rpt = {ilu_FALSE, ilu_FALSE};
    if (p->inWedged)
      return ILU_ERR_CONS1(comm_failure, err, minor, ilu_cfm_conn_lost,
			   ilu_rhrc_error);
    if (p->skipping) {
      while (progress && !rpt.tr_eom) {
	(void) ReadWork(self, NIL, 0, &rpt, &progress, err);
	self->tr_inNext = self->tr_inLimit;
	if (ILU_ERRNOK(*err))
	  return p->inWedged = ilu_TRUE, p->skipping = ilu_FALSE, ilu_rhrc_error;
      }
      if (!progress)
	return ilu_rhrc_nothing;
      p->skipping = ilu_FALSE;
    }
    p->busyIn = p->firstIn = ilu_TRUE;
    p->lastIn = ilu_FALSE;
    if (self->tr_inNext == p->inLength) {
      (void) ReadWork(self, NIL, 0, &rpt, &progress, err);
      if (ILU_ERRNOK(*err))
	return p->inWedged = ilu_TRUE, p->busyIn = ilu_FALSE, ilu_rhrc_error;
      if (p->inLength == 0) {
	p->busyIn = ilu_FALSE;
	return (rpt.tr_eof ? ilu_rhrc_eof : ilu_rhrc_nothing);
      }
    }
  } else {
    p->busyOut = ilu_TRUE;
    p->outStart = self->tr_outNext += 4;
    self->tr_outLimit = p->outSize - 4;
  }
  ILU_CLER(*err);
  return ilu_rhrc_ok;
}

/*L1, L2 unconstrained*/
#define FORMAT_HEADER(where,size,eom)		\
(						\
 (where)[0] = (((size) >> 24) & 0xFF) | ((eom) ? 0x80 : 0),	\
 (where)[1] = (((size) >> 16) & 0xFF),		\
 (where)[2] = (((size) >> 8) & 0xFF),		\
 (where)[3] = ((size) & 0xFF)			\
)

static          ilu_boolean
_sunrpcrm_EndMessage(ilu_Transport self,
		     ilu_boolean flush,
		     ilu_Message * msgh,
		     ILU_ERRS((IoErrs)) * err)
{
  register SUNRPCRMParms p = SUNRPCRMPARMS(transport_data(self));
  if (p->busyOut) {
    ilu_cardinal    n1 = self->tr_outNext - p->outStart;
    ilu_boolean     ans = ilu_TRUE;
    ILU_NOTE(SUNRPCRM_DEBUG,
	  ("ILU: _sunrpcrm_EndMessage(self=%p): output, flush=%s, last chunkSize=%lu\n",
	   self, (flush != 0) ? "True" : "False", (long unsigned) (n1)));
    FORMAT_HEADER(self->tr_outBuff + p->outStart - 4, n1, ilu_TRUE);
    p->busyOut = ilu_FALSE;
    if (flush || self->tr_outNext + 8 > p->outSize) {
      ans = ((*p->lower->tr_class->tc_write_bytes)
	     (p->lower, self->tr_outBuff + p->outFirst,
	      self->tr_outNext - p->outFirst, flush, err));
      p->outFirst = self->tr_outNext = 0;
    } else
      ILU_CLER(*err);
    self->tr_outLimit = self->tr_outNext;
    return ans;
  } else if (p->busyIn) {
    ilu_boolean     drops = ilu_FALSE;
    ilu_TransportReport rpt = {ilu_FALSE, ilu_FALSE};
    ILU_NOTE(SUNRPCRM_DEBUG,
	  ("ILU: _sunrpcrm_EndMessage(self=%p): input\n", self));
    if (!(p->lastIn && p->inChunkRem == 0)) {
      ilu_cardinal    lread;
      int             progress;
      drops = (p->inChunkRem || self->tr_inLimit != self->tr_inNext);
      self->tr_inNext = self->tr_inLimit;
      lread = ReadWork(self, NIL, 0, &rpt, &progress, err);
      if (ILU_ERRNOK(*err))
	goto wedge;
    }
    drops = (drops || p->inChunkRem ||
	     self->tr_inLimit != self->tr_inNext);
    self->tr_inNext = self->tr_inLimit;
    p->busyIn = ilu_FALSE;
    if (p->lastIn && p->inChunkRem == 0)
      p->skipping = ilu_FALSE;
    else
      p->skipping = ilu_TRUE;
    if (drops)
      return ILU_ERR_CONS1(internal, err, minor, ilu_im_tcBytesDropped,
			   ilu_FALSE);
    return ILU_CLER(*err);
wedge:
    p->inWedged = ilu_TRUE;
    return (p->busyIn = ilu_FALSE);
  } else
    return ILU_ERR_CONS1(internal, err, minor, ilu_im_endMessage,
			 ilu_FALSE);
}

/*L1.sup < trmu; L2 >= {xmu}*/

static          ilu_boolean
_sunrpcrm_BeginOutputMessageNonblock(ilu_Transport self,
				     ILU_ERRS((IoErrs)) * err)
{
  SUNRPCRMParms   p = SUNRPCRMPARMS(transport_data(self));
  int             progress = 1;
  ILU_NOTE(SUNRPCRM_DEBUG,
	   ("ILU: _sunrpcrm_BeginOutputMessageNonblock(self=%p)\n",
	    self));
  if (p->busyIn || p->busyOut)
    return ILU_ERR_CONS1(internal, err, minor, ilu_im_beginMessage,
			 ilu_FALSE);
  p->dirIsIn = ilu_FALSE;
  p->busyOut = ilu_TRUE;
  p->outStart = self->tr_outNext += 4;
  self->tr_outLimit = p->outSize - 4;
  return ILU_CLER(*err);
}

static          ilu_TransportEndReport
_sunrpcrm_EndOutputMessageNonblock(ilu_Transport self,
				   ilu_boolean flush,
				   ilu_Message * msgh,
				   ILU_ERRS((IoErrs)) * err)
{
  register SUNRPCRMParms p = SUNRPCRMPARMS(transport_data(self));
  ilu_TransportEndReport ans = {ilu_TRUE, ilu_FALSE};
  if (p->busyOut) {
    ilu_cardinal    n1 = self->tr_outNext - p->outStart;
    ILU_NOTE(SUNRPCRM_DEBUG,
	     ("ILU: _sunrpcrm_EndOutputMessageNonblock(self=%p): flush=%s, last chunkSize=%lu\n",
	      self, (flush != 0) ? "True" : "False",
	      (long unsigned) (n1)));
    FORMAT_HEADER(self->tr_outBuff + p->outStart - 4, n1, ilu_TRUE);
    p->busyOut = ilu_FALSE;
    if (flush || self->tr_outNext + 8 > p->outSize) {
      ilu_cardinal    wrote = 0;
      wrote = ((*p->lower->tr_class->tc_write_bytes_nonblock)
	       (p->lower, self->tr_outBuff + p->outFirst,
		self->tr_outNext - p->outFirst,
		flush, &ans.iluter_flushed, err));
      p->outFirst += wrote;
      if (p->outFirst == self->tr_outNext)
	p->outFirst = self->tr_outNext = 0;
    } else
      ILU_CLER(*err);
    self->tr_outLimit = self->tr_outNext;
    return ans;
  } else if (p->busyIn) {
    return ILU_ERR_CONS1(internal, err, minor, ilu_im_endMessageDir,
			 ans);
  } else
    return ILU_ERR_CONS1(internal, err, minor, ilu_im_endMessage,
			 ans);
}

/*Main Invariant holds; L2 >= {xmu}*/

static          ilu_boolean
_sunrpcrm_Push(ilu_Transport self,
	       ILU_ERRS((IoErrs)) * err)
{
  SUNRPCRMParms   p = SUNRPCRMPARMS(transport_data(self));
  ilu_boolean     ans;
  if (!ilu_Check(!(p->busyOut || p->busyIn), err))
    return ilu_FALSE;
  ans = ((*p->lower->tr_class->tc_write_bytes)
	 (p->lower, self->tr_outBuff, self->tr_outNext, ilu_TRUE, err));
  self->tr_outNext = 0;
  return ans;
}

static          ilu_boolean
_sunrpcrm_SendWholeMessage(ilu_Transport self, ilu_Message * msgh,
			   ILU_ERRS((IoErrs)) * err)
{
  return ILU_ERR_CONS1(internal, err, minor, ilu_im_tcReliable, ilu_FALSE);
}

/* block ? Main Invariant : L1.sup < trmu; L2 >= {xmu} */
static          ilu_cardinal
_sunrpcrm_WriteBytesMaybeblock(ilu_Transport self, ilu_bytes b,
			       ilu_cardinal bufferSize,
			       ilu_boolean block,
			       ILU_ERRS((IoErrs)) * err)
{
  SUNRPCRMParms   p = SUNRPCRMPARMS(transport_data(self));
  ilu_Transport   lt = p->lower;
  ilu_cardinal    rem = self->tr_outLimit - self->tr_outNext;
  ilu_cardinal    ans = 0;
  ilu_boolean     direct;
  if (!p->busyOut)
    return ILU_ERR_CONS1(internal, err, minor, ilu_im_bytesWithoutMsg,
			 0);
  direct = (bufferSize >= DIRECT_THRESHOLD);
  if (direct) {
    ilu_cardinal    l1 = self->tr_outNext - p->outStart;
    if (l1 > 0) {
      FORMAT_HEADER(self->tr_outBuff + p->outStart - 4, l1, ilu_FALSE);
      FORMAT_HEADER(self->tr_outBuff + self->tr_outNext, bufferSize,
		    ilu_FALSE);
      self->tr_outNext += 4;
      ILU_NOTE(SUNRPCRM_DEBUG,
	       ("ILU: _sunrpcrm_WriteBytes(self=%p):"
	" Writing length %lu chunk, plus header for length %lu.\n",
		self, l1, bufferSize));
    } else {
      /* current chunk empty --- hijack its header */
      FORMAT_HEADER(self->tr_outBuff + p->outStart - 4, bufferSize,
		    ilu_FALSE);
      ILU_NOTE(SUNRPCRM_DEBUG,
	       ("ILU: _sunrpcrm_WriteBytes(self=%p):"
		" Writing header for length %lu chunk.\n",
		self, bufferSize));
    }
    p->outFirst += (transport_write_bytes_maybeblock
		    (lt, self->tr_outBuff + p->outFirst,
		     self->tr_outNext - p->outFirst, block, err));
    if (ILU_ERRNOK(*err))
      return 0;
    ILU_NOTE(SUNRPCRM_DEBUG,
	     ("ILU: _sunrpcrm_WriteBytes(self=%p):"
	      " Writing length %lu chunk body.\n",
	      self, bufferSize));
    ans = transport_write_bytes_maybeblock(lt, b, bufferSize, block, err);
    if (ILU_ERRNOK(*err))
      return ans;
    /* (!block) => bufferSize < DIRECT_THRESHOLD, so (block) must be true, so it must have written the whole thing. */
    if (!ilu_Check(ans == bufferSize, err))
      return ans;
    self->tr_outNext = p->outStart = 4;
    p->outFirst = 0;
  } else {
    ilu_cardinal    l1 = MIN(rem, bufferSize);
    ilu_cardinal    hedlen = self->tr_outNext - p->outFirst;
    if (l1 > 0)
      memcpy((void *) (self->tr_outBuff + self->tr_outNext),
	     (void *) b, l1);
    self->tr_outNext += l1;
    b += l1;
    bufferSize -= l1;
    if (bufferSize > 0 || (self->tr_outLimit - self->tr_outNext) < 16) {
      /*
       * Gotta write buffer to make room for more, or to make sure
       * we always have room for 16 more bytes in the buffer.
       */
      /* Possible improvement: avoid writing 0-length chunk here. */
      ilu_cardinal    dFirst;
      ilu_cardinal    req = self->tr_outNext - p->outStart;
      FORMAT_HEADER(self->tr_outBuff + p->outStart - 4, req, ilu_FALSE);
      ILU_NOTE(SUNRPCRM_DEBUG,
	       ("ILU: _sunrpcrm_WriteBytes(self=%p):"
		" Writing length %lu chunk.\n",
		self, (long unsigned) req));
      dFirst = (transport_write_bytes_maybeblock
		(lt, self->tr_outBuff + p->outFirst,
		 self->tr_outNext - p->outFirst, block, err));
      p->outFirst += dFirst;
      if (dFirst > hedlen)
	ans = dFirst - hedlen;
      if (p->outFirst == self->tr_outNext)
	p->outFirst = self->tr_outNext = 0;
      else if (bufferSize + self->tr_outNext + 20 > self->tr_outLimit) {
	memmove((void *) (self->tr_outBuff + 0),
		(void *) (self->tr_outBuff + p->outFirst),
		self->tr_outNext - p->outFirst);
	self->tr_outNext -= p->outFirst;
	p->outFirst = 0;
      }
      p->outStart = self->tr_outNext = self->tr_outNext + 4;
      if (bufferSize > 0) {
	memcpy((void *) (self->tr_outBuff + self->tr_outNext), (void *) b,
	       bufferSize);
	self->tr_outNext += bufferSize;
	ans += bufferSize;
      }
      if (ILU_ERRNOK(*err))
	return ans;
    }
  }
  if (!ilu_Check((self->tr_outLimit - self->tr_outNext) >= 16,
		 err))
    return ans;
  return ans;
}

static          ilu_boolean
_sunrpcrm_WriteBytes(ilu_Transport self, ilu_bytes b,
		     ilu_cardinal bufferSize,
		     ilu_boolean flush,
		     ILU_ERRS((IoErrs)) * err)
{
  (void) _sunrpcrm_WriteBytesMaybeblock(self, b, bufferSize, ilu_TRUE, err);
  return ILU_ERROK(*err);
}

static          ilu_cardinal
_sunrpcrm_WriteBytesNonblock(ilu_Transport self, ilu_bytes b,
			     ilu_cardinal bufferSize,
			     ilu_boolean flush,
			     ilu_boolean * flushed,
			     ILU_ERRS((IoErrs)) * err)
{
  return _sunrpcrm_WriteBytesMaybeblock(self, b, bufferSize, ilu_FALSE, err);
}

/*L1, L2 unconstrained*/
#define PARSE_HEADER(ptr,last,size)				\
	(last = ((ptr)[0] & 0x80) != 0,				\
	 size = ((((ptr)[0] & 0x7F) << 24) + ((ptr)[1] << 16)	\
		 + ((ptr)[2] << 8) + (ptr)[3]))

/*Main Invariant holds; L2 >= {xmu, ymu}*/

static          ilu_cardinal
_sunrpcrm_ReadBytes(ilu_Transport self,
		    ilu_bytes buffer,
		    ilu_cardinal len,
		    ilu_TransportReport * rpt,
		    ILU_ERRS((IoErrs)) * err)
{
  SUNRPCRMParms   p = SUNRPCRMPARMS(transport_data(self));
  int             progress;
  if (!p->busyIn)
    return ILU_ERR_CONS1(internal, err, minor, ilu_im_bytesWithoutMsg,
			 0);
  return ReadWork(self, buffer, len, rpt, &progress, err);
}

static          ilu_cardinal
ReadWork(ilu_Transport self,
	 ilu_bytes buffer,
	 ilu_cardinal len,
	 ilu_TransportReport * rpt,
	 int *progress,
	 ILU_ERRS((IoErrs)) * err)
{
  SUNRPCRMParms   p = SUNRPCRMPARMS(transport_data(self));
  ilu_cardinal    rem = self->tr_inLimit - self->tr_inNext;
  if (rem > 0)
    return ILU_ERR_CONS1(internal, err, minor, ilu_im_tcInputSkipsBuff,
			 0);
  rpt->tr_eom = rpt->tr_eof = *progress = 0;
  if (p->inChunkRem > 0) {
    /* Chunk not done; read more of it. */
    ilu_cardinal    lread, limit, oldrem = p->inChunkRem;
    if (buffer) {
      limit = MIN(len, p->inChunkRem);
      lread = transport_read_upto_bytes(p->lower, buffer,
					limit, rpt, err);
      buffer += lread;
      len -= lread;
      self->tr_inNext = self->tr_inLimit = p->inLength = 0;
      p->inChunkRem -= lread;
    } else {
      lread = transport_read_upto_bytes(p->lower, self->tr_inBuff,
					p->inSize, rpt, err);
      self->tr_inNext = 0;
      p->inLength = lread;
      p->inChunkRem -= (self->tr_inLimit = MIN(lread, p->inChunkRem));
    }
    ILU_NOTE(SUNRPCRM_DEBUG,
	  ("ILU: (sunrpcrm.c/ReadWork)(self=%p): Read %lu more bytes, out of %lu remaining.\n",
	   self, lread, oldrem));
    *progress = (lread > 0);
    if (ILU_ERRNOK(*err))
      return (buffer ? 0 : lread);
    if (rpt->tr_eof && self->tr_inLimit == p->inLength) {
      rpt->tr_eom = ilu_TRUE;
      if (p->inChunkRem == 0 && p->lastIn) {
	ILU_NOTE(SUNRPCRM_DEBUG,
	      ("ILU: (sunrpcrm.c/ReadWork)(self=%p): stream ends cleanly after msg remainder.\n",
	       self));
      } else {
	ILU_NOTE(SUNRPCRM_DEBUG,
	      ("ILU: (sunrpcrm.c/ReadWork)(self=%p): stream ends mid-message!\n",
	       self));
	return ILU_ERR_CONS1(comm_failure, err, minor, ilu_cfm_eof,
			     0);
      }
    } else
      rpt->tr_eof = ilu_FALSE;
    if (buffer)
      return lread;
  } else if (!p->lastIn) {
    /* process another chunk. */
    ilu_cardinal    csize, cbsize;
    if (self->tr_inNext + 4 <= p->inLength) {
      /* Chunk's header already in buffer. */
      *progress = 1;
    } else {
      /* Shift header fragment to buffer start, then read more */
      ilu_cardinal    l1 = p->inLength - self->tr_inNext;
      ilu_cardinal    lread, l2 = p->inSize - l1;
      if (self->tr_inNext)
	memcpy((void *) self->tr_inBuff,
	       (void *) (self->tr_inBuff + self->tr_inNext), l1);
      self->tr_inNext = self->tr_inLimit = 0;
      p->inLength = l1;
      lread = transport_read_upto_bytes(p->lower,
				   (void *) (self->tr_inBuff + l1),
					l2, rpt, err);
      ILU_NOTE(SUNRPCRM_DEBUG,
	    ("ILU: (sunrpcrm.c/ReadWork)(self=%p): Read %lu more bytes, in addition to %lu previous, of next chunk (incl header).\n",
	     self, lread, l1));
      *progress = (lread > 0);
      p->inLength = l1 + lread;
      if (ILU_ERRNOK(*err))
	return 0;
      if (l1 + lread == 0 && rpt->tr_eof && p->firstIn) {
	/* clean EOF */
	ILU_NOTE(SUNRPCRM_DEBUG,
	      ("ILU: (sunrpcrm.c/ReadWork)(self=%p): No more messages incoming.\n", self));
	rpt->tr_eom = ilu_TRUE;
	return 0;
      }
      if (l1 + lread < 4 && rpt->tr_eof) {
	/* EOF before end of next header */
	rpt->tr_eom = ilu_TRUE;
	ILU_NOTE(SUNRPCRM_DEBUG,
	      ("ILU: (sunrpcrm.c/ReadWork)(self=%p): EOF while reading header!\n", self));
	return ILU_ERR_CONS1(comm_failure, err, minor, ilu_cfm_eof,
			     0);
      }
      rpt->tr_eof = ilu_FALSE;
      /* If the EOF is real, it'll happen again. */
    }
    if (self->tr_inNext + 4 <= p->inLength) {
      PARSE_HEADER(self->tr_inBuff + self->tr_inNext, p->lastIn, csize);
      p->firstIn = ilu_FALSE;
      self->tr_inNext += 4;
      cbsize = MIN(p->inLength - self->tr_inNext, csize);
      self->tr_inLimit = self->tr_inNext + cbsize;
      p->inChunkRem = csize - cbsize;
      ILU_NOTE(SUNRPCRM_DEBUG,
	    ("ILU: (sunrpcrm.c/ReadWork)(self=%p): Parse header of length %lu chunk (last=%d), %lu in buffer.\n",
	     self, csize, p->lastIn != 0, cbsize));
    }
  }
  /* Now use what's in tr_inBuff */
  rem = self->tr_inLimit - self->tr_inNext;
  if (rem == 0 && p->lastIn && p->inChunkRem == 0) {
    rpt->tr_eom = ilu_TRUE;
    return 0;
  }
  if (buffer == NIL)
    return rem;
  if (rem > 0) {
    ilu_cardinal    l1 = MIN(rem, len);
    memcpy((void *) buffer,
	   (void *) (self->tr_inBuff + self->tr_inNext),
	   l1);
    self->tr_inNext += l1;
    return l1;
  }
  return 0;
}

/*L1.sup < trmu; self->tr_wc => L1 >= {cmu}; L2 >= {xmu, ymu}*/

static          ilu_boolean
_sunrpcrm_Close(ilu_Transport self, ilu_integer * dfd,
		ILU_ERRS((bad_locks, broken_locks, internal)) * err)
{
  SUNRPCRMParms   p = SUNRPCRMPARMS(transport_data(self));
  ilu_Transport   lt = p->lower;
  ILU_NOTE(SUNRPCRM_DEBUG,
	("ILU: _sunrpcrm_Close(self=%p): lower=%p\n", self, lt));
  ilu_free(self->tr_inBuff);
  ilu_free(self->tr_outBuff);
  ilu_free(p);
  ilu_free(self);
  return transport_close(lt, dfd, err);
}

/*L1, L2 unconstrained*/

static struct _ilu_TransportClass_s myclass = {
  ilu_TRUE,			/* boundaried */
  ilu_TRUE,			/* reliable */
  SetInputHandler,
  _sunrpcrm_WaitForInput,
  _sunrpcrm_InterruptST,
  _sunrpcrm_DisableWait,
  _sunrpcrm_EnableWait,
  _sunrpcrm_BeginMessage,
  _sunrpcrm_EndMessage,
  _sunrpcrm_BeginOutputMessageNonblock,
  _sunrpcrm_EndOutputMessageNonblock,
  _sunrpcrm_Push,
  _sunrpcrm_SendWholeMessage,
  _sunrpcrm_WriteBytes,
  _sunrpcrm_WriteBytesNonblock,
  _sunrpcrm_ReadBytes,
  _sunrpcrm_Close
};

/*L1.sup < cmu; L2 >= result's {xmu, ymu}*/
static          ilu_Transport
NewTrans(ilu_Transport lower, ilu_integer * dfd,
	 ILU_ERRS((no_memory)) * err)
{
  ilu_Transport   ans = NIL;
  SUNRPCRMParms   parms = NIL;
  parms = (SUNRPCRMParms) ilu_MallocE(sizeof(*parms), err);
  if (parms == NIL)
    return NIL;
  parms->busyIn = parms->busyOut = ilu_FALSE;
  parms->inWedged = parms->skipping = ilu_FALSE;
  parms->inLength = parms->inChunkRem = parms->outFirst = 0;
  parms->inSize = parms->outSize = BUFFERSIZE;
  parms->lower = lower;
  ans = (ilu_Transport) ilu_MallocE(sizeof(*ans), err);
  if (ans == NIL)
    goto fale;
  ans->tr_outBuff = NIL;
  ans->tr_inBuff = ilu_MallocE(parms->inSize, err);
  if (ans->tr_inBuff)
    ans->tr_outBuff = ilu_MallocE(parms->outSize, err);
  if (ans->tr_inBuff == NIL || ans->tr_outBuff == NIL)
    goto fale;
  ans->tr_inNext = ans->tr_inLimit = 0;
  ans->tr_outNext = ans->tr_outLimit = 0;
  ans->tr_class = &myclass;
  ans->tr_data = parms;
  ans->tr_wc = lower->tr_wc;
  ans->tr_estFDs = lower->tr_estFDs;
  ILU_NOTE(SUNRPCRM_DEBUG,
	("ILU: (sunrpcrm.c/NewTrans): lower=%p, new transport is %p\n",
	 lower, ans));
  ILU_CLER(*err);
  return ans;
fale:{
    ILU_ERRS((bad_locks, broken_locks, internal)) cerr;
    ilu_integer     cdfd = 0;
    if (ans) {
      ilu_free(ans->tr_inBuff);
      ilu_free(ans->tr_outBuff);
    }
    ilu_free(parms);
    transport_close(lower, &cdfd, &cerr);
    ILU_HANDLED(cerr);
    *dfd += cdfd;
    return NIL;
  }
}

/* Main Invariant holds */
static          ilu_Transport
_sunrpcrm_CreateTransport(ilu_TransportCreator self, ilu_boolean buffer,
			  ilu_integer *dfd, ilu_Passport pp,
			  ILU_ERRS((IoErrs)) * err)
{
  CreatorParms    cp = (CreatorParms) self->tcr_data;
  ilu_Transport   lower;
  lower = (*cp->lower->tcr_createTransport) (cp->lower, ilu_FALSE, dfd, pp,
					     err);
  if (ILU_ERRNOK(*err))
    return NIL;
  if (lower->tr_class->tc_boundaried || !transport_reliable(lower))
    /* He promised! (We checked earlier) */
    return ILU_ERR_CONS1(internal, err, minor, ilu_im_tcBug, NIL);
  /* pretend to acquire result's xmu, ymu */
  return NewTrans(lower, dfd, err);
  /* pretend to release result's xmu, ymu */
}

/*L1.sup < trmu; L2 >= {xmu, ymu}*/
static ilu_integer MooringDFd(ilu_Mooring self, ilu_boolean add)
{
  ilu_Mooring     lm = (ilu_Mooring) self->mo_data;
  return ((*lm->mo_dfd) (lm, add));
}

/*L1.sup < trmu; L2 >= {xmu, ymu}*/
static          ilu_boolean
SetReqHandler(ilu_Mooring self,
	      ilu_TIH tih,
	      ILU_ERRS((no_memory, imp_limit, no_resources,
			broken_locks, internal)) * err)
{
  ilu_Mooring     lm = (ilu_Mooring) self->mo_data;
  return ((*lm->mo_set_req_handler) (lm, tih, err));
}

/* Main Invariant holds; L2 >= {ymu} */
static          ilu_boolean
WaitForReq(ilu_Mooring self, int *disabled,
	   ILU_ERRS((interrupted, broken_locks)) * err)
{
  ilu_Mooring     lm = (ilu_Mooring) self->mo_data;
  return ((*lm->mo_wait_for_req) (lm, disabled, err));
}

/* L1 >= {cmu}; L1.sup < trmu; L2 >= {xmu} */

static          ilu_boolean
DisableReqWait(ilu_Mooring self,
	       ILU_ERRS((broken_locks, bad_param, internal)) * err)
{
  ilu_Mooring     lm = (ilu_Mooring) self->mo_data;
  return ((*lm->mo_disableWait) (lm, err));
}

static          ilu_boolean
EnableReqWait(ilu_Mooring self,
	       ILU_ERRS((broken_locks, bad_param, internal)) * err)
{
  ilu_Mooring     lm = (ilu_Mooring) self->mo_data;
  return ((*lm->mo_enableWait) (lm, err));
}

/* Main Invariant holds; L2 >= self's {xmu, ymu} */

static          ilu_Transport
_sunrpcrm_AcceptClient(ilu_Mooring self, ilu_string * tinfo_out,
		       ilu_integer *dfd, ilu_Passport pp,
		       ILU_ERRS((IoErrs)) * err)
{
  ilu_Mooring     lm = (ilu_Mooring) self->mo_data;
  ilu_Transport   lower;
  ilu_string      subtinfo = NIL;
  ilu_Transport   ans = NIL;
  lower = ((*lm->mo_accept_connection)
	   (lm, tinfo_out ? &subtinfo : NIL, dfd, pp, err));
  if (ILU_ERRNOK(*err) || lower == NIL)
    goto dun;
  if (tinfo_out) {
    *tinfo_out = ilu_Strcat3E("sunrpcrm", " over ", subtinfo, err);
    if (ILU_ERRNOK(*err))
      goto dun;
  }
  ans = NewTrans(lower, dfd, err);
dun:
  if (subtinfo)
    ilu_free(subtinfo);
  return ans;
}

/*L1.sup < trmu; L1 >= {cmu}; L2 >= {xmu, ymu}*/
static          ilu_boolean
_sunrpcrm_CloseMooring(ilu_Mooring self, ilu_integer * dfd,
		       ILU_ERRS((bad_locks, broken_locks,
				 internal)) * err)
{
  ilu_Mooring     lower = (ilu_Mooring) self->mo_data;
  ILU_NOTE(SUNRPCRM_DEBUG,
	   ("ILU: _sunrpcrm_CloseMooring(self=%p): lower=%p\n",
	    self, lower));
  if (!(*lower->mo_close) (lower, dfd, err))
    return ilu_FALSE;
  ilu_free(self);
  return ilu_TRUE;
}

/*L1, L2 unconstrained*/

static struct _ilu_Mooring_s mooringProto = {
  MooringDFd,
  SetReqHandler,
  WaitForReq,
  DisableReqWait,
  EnableReqWait,
  _sunrpcrm_AcceptClient,
  _sunrpcrm_CloseMooring,
  NIL				/* data */
};

/*L1.sup < trmu*/
static          ilu_Mooring
_sunrpcrm_CreateMooring(ilu_TransportCreator self,
			ilu_TransportInfo * tinfo_out,
			ilu_boolean buffer,
			ilu_integer *dfd,
			ilu_Passport pp,	/* unused here */
			ILU_ERRS((no_memory)) * err)
{
  CreatorParms    cp = (CreatorParms) self->tcr_data;
  ilu_Mooring     lower, ans;
  ilu_TransportInfo      subtinfo = NIL;
  lower = ((*cp->lower->tcr_createMooring)
	   (cp->lower, tinfo_out ? &subtinfo : NIL, ilu_FALSE, dfd, pp, err));
  if (ILU_ERRNOK(*err))
    return NIL;
  ans = (ilu_Mooring) ilu_MallocE(sizeof(*ans), err);
  if (ans == NIL)
    return NIL;
  if (tinfo_out) {
    *tinfo_out = _ilu_ConcatTinfo ("sunrpcrm", subtinfo, err);
    if (ILU_ERRNOK(*err))
      return NIL;
    else
      ilu_free(subtinfo);
  }
  ILU_NOTE(SUNRPCRM_DEBUG,
	("ILU: _sunrpcrm_CreateMooring: lower=%p, new mooring is %p\n",
	 lower, ans));
  *ans = mooringProto;
  ans->mo_data = lower;
  ans->mo_wc = lower->mo_wc;
  return (ans);
}

static void _sunrpcrm_CloseCreator(ilu_TransportCreator self)
{
  CreatorParms    cp = (CreatorParms) self->tcr_data;
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
  _sunrpcrm_FdUsage,
  _sunrpcrm_CreateTransport,
  _sunrpcrm_CreateMooring,
  _sunrpcrm_CloseCreator,
  NIL				/* data */
};

/*L1.sup < trmu*/
ilu_TransportCreator
_ilu_sunrpcrm_TransportCreator(ilu_TransportInfo tinfo,
			       ILU_ERRS((no_memory,
					 inv_objref)) * err)
{
  ilu_TransportCreator ans;
  CreatorParms    cp;
  cp = _sunrpcrm_InterpretInfo(tinfo, err);
  if (ILU_ERRNOK(*err))
    return NIL;
  ans = (ilu_TransportCreator) ilu_MallocE(sizeof(*ans), err);
  if (ans == NIL)
    return NIL;
  *ans = myCreatorProto;
  ans->tcr_data = cp;
  ILU_CLER(*err);
  return ans;
}

