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
/* $Id: ilubufxp.c,v 1.27 1999/08/03 01:53:00 janssen Exp $ */
/* Last edited by Mike Spreitzer September 10, 1998 9:00 pm PDT */

#include "iluntrnl.h"
#include "ilutransport.h"

/* L1, L2 unconstrained */

typedef struct buffCons BuffCons, *BuffList;

#define CHUNKSIZE 4096

struct buffCons {
  BuffList        next;
  ilu_cardinal    start, len;
  ilu_byte        bytes[CHUNKSIZE];
};

typedef struct {
  BuffList        buffs;
}              *TransportParms;
/*
 * First buffer is in exposed buffer while processing the message.
 * tr_inBuff != NIL <=> processing the message.  Elements are freed
 * as they are passed.  Between close and read of last byte,
 * buffs==NIL and tr_inBuff points to freed buffer.
 */

static          ilu_boolean
SetInputHandler(ilu_Transport t,
		ilu_TIH tih,
		ILU_ERRS((no_memory, internal,
			  no_resources)) * err)
{
  /*
   * tc_set_input_handler is used only for incoming transports. This
   * is only an outgoing transport.
   */
  return ILU_ERR_CONS1(internal, err, minor, ilu_im_tportRole, ilu_FALSE);
}

static          ilu_boolean
WaitForInput(ilu_Transport t, int *disabled,
	     ilu_FineTime * limit, ILU_ERRS(()) * err)
{
  return (*disabled = 0, ILU_CLER(*err));
}

static ilu_boolean 
InterruptInpWait(ilu_Transport t, ILU_ERRS(()) * err)
{
  return ILU_CLER(*err);
}

static ilu_boolean 
AbleInpWait(ilu_Transport t, ILU_ERRS(()) * err)
{
  return ILU_CLER(*err);
}

static          ilu_ReadHeaderResultCode
BeginMessage(ilu_Transport t,
	     ilu_boolean input,
	     ILU_ERRS((IoErrs)) * err)
{
  /* Creator promised this won't happen. */
  return ILU_ERR_CONS1(internal, err, minor, ilu_im_bufxpMisuse,
		       ilu_rhrc_error);
}

static          ilu_boolean
EndMessage(ilu_Transport t,
	   ilu_boolean flush,
	   ilu_Message * msg,
	   ILU_ERRS((IoErrs)) * err)
{
  if (t->tr_inBuff == NIL)
    return ILU_ERR_CONS1(internal, err, minor, ilu_im_endMessage, ilu_FALSE);
  t->tr_inBuff = NIL;
  return ILU_CLER(*err);
}

static          ilu_boolean
BeginOutputMessageNonblock(ilu_Transport t,
			   ILU_ERRS((IoErrs)) * err)
{
  /* Creator promised this won't happen. */
  return ILU_ERR_CONS1(internal, err, minor, ilu_im_bufxpMisuse,
		       ilu_FALSE);
}

static          ilu_TransportEndReport
EndOutputMessageNonblock(ilu_Transport t,
			 ilu_boolean flush,
			 ilu_Message * msg,
			 ILU_ERRS((IoErrs)) * err)
{
  ilu_TransportEndReport ans = {ilu_FALSE, ilu_FALSE};
  if (t->tr_inBuff == NIL)
    return ILU_ERR_CONS1(internal, err, minor, ilu_im_endMessage, ans);
  return ILU_ERR_CONS1(internal, err, minor, ilu_im_endMessageDir, ans);
}

static ilu_boolean 
Push(ilu_Transport t, ILU_ERRS((IoErrs)) * err)
{
  return ILU_CLER(*err);
}

static          ilu_boolean
SendWholeMessage(ilu_Transport t,
		 ilu_Message * msg,
		 ILU_ERRS((IoErrs)) * err)
{
  /* Creator promised I won't be used for output. */
  return ILU_ERR_CONS1(internal, err, minor, ilu_im_bufxpMisuse, ilu_FALSE);
}

static          ilu_boolean
WriteBytes(ilu_Transport t, ilu_bytes buf, ilu_cardinal bufLen,
	   ilu_boolean flush, ILU_ERRS((IoErrs)) * err)
{
  /* Creator promised I won't be used for output. */
  return ILU_ERR_CONS1(internal, err, minor, ilu_im_bufxpMisuse, ilu_FALSE);
}

static          ilu_cardinal
WriteBytesNonblock(ilu_Transport t, ilu_bytes buf, ilu_cardinal bufLen,
		   ilu_boolean flush, ilu_boolean * flushed,
		   ILU_ERRS((IoErrs)) * err)
{
  /* Creator promised I won't be used for output. */
  *flushed = 0;
  return ILU_ERR_CONS1(internal, err, minor, ilu_im_bufxpMisuse, 0);
}

static          ilu_cardinal
ReadBytes(ilu_Transport t, ilu_bytes buf, ilu_cardinal bufLen,
	  ilu_TransportReport * rpt, ILU_ERRS((IoErrs)) * err)
{
  TransportParms  tp = (TransportParms) t->tr_data;
  BuffList        head = tp->buffs, next;
  if (t->tr_inBuff == NIL)
    return ILU_ERR_CONS1(internal, err, minor, ilu_im_bytesWithoutMsg,
			 0);
  if (t->tr_inNext < t->tr_inLimit)
    return ILU_ERR_CONS1(internal, err, minor, ilu_im_tcInputSkipsBuff,
			 0);
  tp->buffs = next = head->next;
  ilu_free(head);
  if (next != NIL) {
    ILU_NOTE(CONNECTION_DEBUG,
	  ("ilubufxp.ReadBytes(%p): next chunk has %lu bytes\n",
	   t, (long unsigned) next->len));
    t->tr_inBuff = next->bytes;
    t->tr_inNext = next->start;
    t->tr_inLimit = next->start + next->len;
    ILU_CLER(*err);
    rpt->tr_eom = rpt->tr_eof = ilu_FALSE;
    if (buf != NIL) {
      ilu_cardinal    l1 = MIN(next->len, bufLen);
      memcpy((void *) buf, (void *) (t->tr_inBuff + t->tr_inNext),
	     (SIZE_T) l1);
      t->tr_inNext += l1;
      return l1;
    }
    return (next->len);
  }
  ILU_NOTE(CONNECTION_DEBUG,
	("ilubufxp.ReadBytes(%p): at EOM&F.\n", t));
  rpt->tr_eom = rpt->tr_eof = ilu_TRUE;
  ILU_CLER(*err);
  return 0;
}

static ilu_boolean 
Close(ilu_Transport t, ilu_integer * dfd,
      ILU_ERRS((internal)) * err)
{
  TransportParms  tp = (TransportParms) t->tr_data;
  BuffList        bl = tp->buffs;
  ILU_NOTE(CONNECTION_DEBUG, ("ilubufxp.Close(%p)\n", t));
  *dfd = 0;
  while (bl != NIL) {
    BuffList        tmp = bl;
    bl = bl->next;
    ilu_free(tmp);
  }
  t->tr_inBuff = NIL;
  tp->buffs = NIL;
  ilu_free(tp);
  ilu_free(t);
  return ILU_CLER(*err);
}

static struct _ilu_TransportClass_s myClass = {
  ilu_TRUE,				/* boundaried */
  ilu_TRUE,				/* reliable */
  SetInputHandler,
  WaitForInput,
  InterruptInpWait,
  AbleInpWait,
  AbleInpWait,
  BeginMessage,
  EndMessage,
  BeginOutputMessageNonblock,
  EndOutputMessageNonblock,
  Push,
  SendWholeMessage,
  WriteBytes,
  WriteBytesNonblock,
  ReadBytes,
  Close
};

/* Main Invariant holds; L2 >= {t's xmu, ymu} */
ilu_Transport
_ilu_BufferInputMessage(ilu_Transport t,
			ilu_cardinal msgsize,
			ilu_boolean byBytes,
			ILU_ERRS((IoErrs)) * err)
{
  ilu_Transport   ans = (ilu_Transport) ilu_MallocE(sizeof(*ans), err);
  TransportParms  tp = NIL;
  BuffList       *tailp;
  ilu_cardinal	  bytes_to_read;
  ilu_cardinal    nchunks = 0, nbytes = 0;
  ilu_TransportReport rpt;

  rpt.tr_eom = ilu_FALSE;
  rpt.tr_eof = ilu_FALSE;

  if (ans)
    tp = (TransportParms) ilu_MallocE(sizeof(*tp), err);
  if (tp == NIL)
    goto faild;
  tp->buffs = NIL;
  if (transport_boundaried(t) && byBytes) {
    ILU_ERR_CONS1(internal, err, minor, ilu_im_tcBug, 0);
    goto faild;
  } else if (!transport_boundaried(t) && !byBytes) {
    ILU_ERR_CONS1(internal, err, minor, ilu_im_tcNotBoundaried, 0);
    goto faild;
  }
  tailp = &tp->buffs;
  while ((!rpt.tr_eom) && (!byBytes || (nbytes < msgsize))) {
    BuffList        c = (BuffList) ilu_MallocE(sizeof(*c), err);
    if (c == NIL)
      goto faild;
    if (t->tr_inBuff == NIL || t->tr_inNext >= t->tr_inLimit)
      if (!_ilu_TransportWaitForInputNoClose(t, NIL, err))
	goto faild;
    c->start = 0;
    c->next = NIL;
    *tailp = c;
    tailp = &c->next;
    if (byBytes)
      bytes_to_read = MIN((msgsize - nbytes), CHUNKSIZE);
    else
      bytes_to_read = CHUNKSIZE;
    c->len = transport_read_upto_bytes(t, c->bytes, bytes_to_read, &rpt,
				       err);
    if (ILU_ERRNOK(*err))
      goto faild;
    nchunks += 1;
    nbytes += c->len;
  }
  if (!tp->buffs) {
    BuffList        c = (BuffList) ilu_MallocE(sizeof(*c), err);
    if (c == NIL)
      goto faild;
    c->start = 0;
    c->next = NIL;
    *tailp = c;
    tailp = &c->next;
    c->len = 0;
  }
  ans->tr_inBuff = tp->buffs->bytes;
  ans->tr_inNext = tp->buffs->start;
  ans->tr_inLimit = tp->buffs->start + tp->buffs->len;
  ans->tr_outBuff = NIL;
  ans->tr_outNext = ans->tr_outLimit = 0;
  ans->tr_class = &myClass;
  ans->tr_data = tp;
  ans->tr_wc = NIL;
  ans->tr_estFDs = 0;
  if (!byBytes) {
    if (!transport_end_message(t, ilu_FALSE, NIL, err))
      goto faild;
  }
  ILU_NOTE(CONNECTION_DEBUG,
	   ("_ilu_BufferInputMessage(%p) makes %p from "
	    "%lu bytes in %lu chunks.\n",
	    t, ans, (long unsigned) nbytes,
	    (long unsigned) nchunks));
  return ans;
faild:
  if (tp != NIL) {
    while (tp->buffs != NIL) {
      BuffList        tmp = tp->buffs;
      tp->buffs = tp->buffs->next;
      ilu_free(tmp);
    }
    ilu_free(tp);
  }
  if (ans != NIL)
    ilu_free(ans);
  return NIL;
}
