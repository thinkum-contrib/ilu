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
/* $Id: inmem.c,v 1.44 1999/08/03 01:53:00 janssen Exp $ */
/* Last edited by Mike Spreitzer September 10, 1998 8:55 pm PDT */

/*
 * In-memory Transport.  The in-memory transport is a boundaried,
 * reliable transport which buffers a single request and a reply
 * sequence.  Its transport info begins "inmem_".  An LSR must
 * animate an inmem_ port in the single-threaded style, even in
 * multi-threaded runtimes.
 */

/* place includes here */
#include "iluxport.h"
#include "iluntrnl.h"
#include "mooring.h"
#include "ilutransport.h"

/* place #defines here */
#define BUFSIZE 4096 /* Stab in the dark.  Double if too small.  */

typedef struct BufferList_s {
  /* L1, L2 unconstrained */
  
  ilu_bytes buffer;
  ilu_cardinal offset; /* valid data at [0, offset),
			  unused space at [offset, size). */
  ilu_cardinal size; /* change if buffer redimmed. */
  struct BufferList_s * next;
} * BufferList;

/* Shared between the two transports at the "ends" of the connection */
typedef struct SharedData_s {
  /* L1, L2 unconstrained; synch provided by single-threadedness */
  
  BufferList sd_request;	/* next is NIL */

  /* There may be multiple outstanding replies. */
  /* -Tail is NIL exactly when -Head is NIL. */
  BufferList sd_repliesHead;	/* Take off from */
  BufferList sd_repliesTail;	/* Put on to */

  /* Messages that have been read. */
  BufferList sd_reuseHead; 
  BufferList sd_reuseTail;

  /* Input handler of incoming side Transport */
  ilu_TIH         sd_tih;	/* owned by client, not this struct */

  ilu_cardinal sd_bufferSize;	/* Used to allocate buffers.
			Possibly read from creator's tinfo.  */
  
} *SharedData;

typedef struct {
  /* L1, L2 unconstrained */
  
  /* Connection request handler. */
  ilu_TIH         md_tih;		/* !=NIL when req handler reg'd */
  SharedData      md_sharedData;	/* NIL outside mutex. */
  ilu_boolean     md_locked;		/* stand-in for a mutex */
}              *MooringParms;
/* (md_tih) storage owned by client. */


typedef struct {
  /* L1, L2 unconstrained */
  
  MooringParms cd_mooringData;	/* NIL until mooring is created */
  ilu_TransportInfo cd_tinfo;
  /* tinfo passed at creator creation time */
}              *CreatorParms;



typedef struct {
  /* L1, L2 unconstrained */

  ilu_boolean     td_working;	/* between Begin- and EndMessage */
  ilu_boolean     td_input;	/* vs. output.  Meaningful if working. */

  ilu_boolean     td_outgoing;	/* vs. incoming. */

  SharedData      td_sharedData; /* shared with "other end" transport */
  BufferList      td_current;	/* NIL if !working */

}              *TransportParms;

/*********************************************
  Methods of the in-memory transport class

  tc_closeDFd
  tc_set_input_handler
  tc_wait_for_input
  tc_interrupt
  tc_begin_message
  tc_end_message
  tc_send_whole_message
  tc_write_bytes
  tc_read_bytes
  tc_close

  *********************************************/

/*L1, L2 unconstrained*/

static          ilu_boolean
_inmem_SetInputHandler(ilu_Transport self,
		       ilu_TIH ptih,
		       ILU_ERRS((no_memory, no_resources)) * err)
{
  TransportParms  tp = (TransportParms) self->tr_data;
  SharedData      sd = tp->td_sharedData;

  if (tp->td_outgoing)
    /* Don't bother, it's just ReadExtraMsg */
    return ILU_CLER(*err);
  sd->sd_tih = ptih;
  return ILU_CLER(*err);
}

/*
 * Should only be called on outgoing end, when message is finished
 * and waiting in shared data space, and control has returned from
 * message writer to message reader.
 */
static          ilu_boolean
_inmem_WaitForInput(ilu_Transport t, int *disabled,
		    ilu_FineTime * limit, ILU_ERRS(()) * err)
{
  return (*disabled = 0, ILU_CLER(*err));
}

static          ilu_boolean
_inmem_Interrupt(ilu_Transport self, ILU_ERRS(()) * err)
{
  return ILU_CLER(*err);
}

static          ilu_boolean
_inmem_AbleInpWait(ilu_Transport self, ILU_ERRS((bad_param)) * err)
{
  return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_threading, ilu_FALSE);
}

static          BufferList
NewBufferList(ilu_cardinal size, ilu_bytes buffer,
	      ILU_ERRS((IoErrs)) * err)
{
  BufferList      b = (BufferList) ilu_MallocE(sizeof(*b), err);
  if (b == NIL)
    return NIL;
  if (buffer == NIL) {
    b->buffer = (ilu_bytes) ilu_MallocE(size, err);
    if (b->buffer == NIL) {
      ilu_free(b);
      return NIL;
    }
  } else
    b->buffer = buffer;
  b->offset = 0;
  b->size = size;
  b->next = NIL;
  ILU_CLER(*err);
  return b;
}

/*
 * The inmemory transport is boundaried.
 * Begin- and EndMessage should be used.
 */

static          ilu_ReadHeaderResultCode
_inmem_BeginMessage(ilu_Transport self,
		    ilu_boolean input_p,
		    ILU_ERRS((IoErrs)) * err)
{
  TransportParms tp = (TransportParms) self->tr_data;
  SharedData sd = tp->td_sharedData;

  ILU_NOTE(INMEM_DEBUG,
	("ILU: _inmem_BeginMessage(%p,%p,%s)\n",
	 self, sd, input_p ? "input" : "output"));
  
  /* Already began a message? */
  if (tp->td_working)
    return ILU_ERR_CONS1(internal, err, minor, ilu_im_beginMessage,
			 ilu_rhrc_error);
  tp->td_working = ilu_TRUE;
  
  if (input_p)  {
    tp->td_input = ilu_TRUE;
    if (tp->td_outgoing) {
      if (sd->sd_repliesHead == NIL) {
	ILU_NOTE(INMEM_DEBUG,
	      ("ILU: _inmem_BeginMessage(%p,%p,outgoing): found nothing.\n",
	       self, sd));
	tp->td_working = ilu_FALSE;
	return ilu_rhrc_nothing;
      }
      tp->td_current = sd->sd_repliesHead; /* read a reply */
      sd->sd_repliesHead = tp->td_current->next;
      if (sd->sd_repliesHead == NIL)
	sd->sd_repliesTail = NIL;
    } else {
      if (sd->sd_request == NIL) {
	ILU_NOTE(INMEM_DEBUG,
	      ("ILU: _inmem_BeginMessage(%p,%p,incoming): found nothing.\n",
	       self, sd));
	tp->td_working = ilu_FALSE;
	return ilu_rhrc_nothing;
      }
      tp->td_current = sd->sd_request;   /* read the request */
      sd->sd_request = NIL;
    }
    tp->td_current->next = NIL;
    /* Make read buffer visible */
    self->tr_inBuff = tp->td_current->buffer;
    self->tr_inNext = 0;
    self->tr_inLimit = tp->td_current->offset;
  } else {
    tp->td_input = ilu_FALSE;
    /* Recycle a buffer, if possible.
       Else allocate a new one. */
    if (sd->sd_reuseHead != NIL) {
      tp->td_current = sd->sd_reuseHead;
      sd->sd_reuseHead = tp->td_current->next;
      if (sd->sd_reuseHead == NIL)
	sd->sd_reuseTail = NIL;
    } else {
      tp->td_current = NewBufferList(sd->sd_bufferSize, NIL, err);
      if (ILU_ERRNOK(*err))
	return ilu_rhrc_error;
    }
    tp->td_current->next = NIL;
    /* Make write buffer visible */
    self->tr_outBuff = tp->td_current->buffer;
    self->tr_outNext = 0;
    self->tr_outLimit = tp->td_current->size;
  }
  ILU_CLER(*err);
  return ilu_rhrc_ok;
}

/*L1.sup < trmu; L2 >= {xmu}*/
static ilu_boolean 
_inmem_BeginOutputMessageNonblock(ilu_Transport self,
				  ILU_ERRS((IoErrs)) * err)
{
  return (_inmem_BeginMessage(self, ilu_FALSE, err) == ilu_rhrc_ok);
}

/**Main Invariant holds;
   Before: (does not hold mooring mutex)
   After: if flush, holds mooring mutex */
static          ilu_boolean
_inmem_EndMessage(ilu_Transport self,
		ilu_boolean flush, /* ignore if inputting? */
		ilu_Message *msg, /* ignore: reliable. */
		ILU_ERRS((IoErrs)) * err)
{
  TransportParms tp = (TransportParms) self->tr_data;
  SharedData sd = tp->td_sharedData;

  /* Already ended message? */
  if (!tp->td_working)
    return ILU_ERR_CONS1(internal, err, minor, ilu_im_endMessage, ilu_FALSE);
  tp->td_working = ilu_FALSE;
  
  if (tp->td_input) {
    ILU_NOTE(INMEM_DEBUG,
	  ("ILU: _inmem_EndMessage(self=%p,sd=%p,input)\n",
	   self, sd));
    /* Finished reading a message, "throw it away" */
    if (sd->sd_reuseHead == NIL)
      sd->sd_reuseHead = sd->sd_reuseTail = tp->td_current;
    else {
      sd->sd_reuseTail->next = tp->td_current;
      sd->sd_reuseTail = sd->sd_reuseTail->next;
    }
    sd->sd_reuseTail->next = NIL;
    sd->sd_reuseTail->offset = 0;	/* don't mess with ->size */
    self->tr_inBuff = NIL;
    tp->td_current = NIL;
    self->tr_inNext = self->tr_inLimit = 0;
  } else {
    /* Finished writing a message. */
    /* Remember how much was written */
    ILU_NOTE(INMEM_DEBUG,
	  ("ILU: _inmem_EndMessage(self=%p,sd=%p,output,flush=%s,len=%lu)\n",
	   self, sd, (flush ? "True" : "False"),
	   (long unsigned) self->tr_outNext));
    tp->td_current->offset = self->tr_outNext;
    if (tp->td_outgoing) {
      /* Store mesage as request */
      _ilu_Assert(sd->sd_request == NIL,
		  "ILU: _inmem_EndMessage: Outstanding request");
      /* signal ilu_im_broken? */
      sd->sd_request = tp->td_current;
      /* Call the input handler! */
      if (sd->sd_tih)
	(*sd->sd_tih->proc) (sd->sd_tih->rock);
      /* No return value. */
    } else {
      /* Store message as reply */
      if (sd->sd_repliesHead == NIL)
	sd->sd_repliesHead = sd->sd_repliesTail = tp->td_current;
      else {
	sd->sd_repliesTail->next = tp->td_current;
	sd->sd_repliesTail = sd->sd_repliesTail->next;
      }
    }
    tp->td_current = NIL;
  }
  return ILU_CLER(*err);
}

static          ilu_TransportEndReport
_inmem_EndOutputMessageNonblock(ilu_Transport self,
				ilu_boolean flush,
				ilu_Message * msg,
				ILU_ERRS((IoErrs)) * err)
{
  ilu_TransportEndReport ans = {ilu_TRUE, ilu_TRUE};
  ans.iluter_ended = _inmem_EndMessage(self, flush, msg, err);
  return ans;
}

static          ilu_boolean
_inmem_Push(ilu_Transport self,
	    ILU_ERRS((IoErrs)) * err)
{
  return ILU_CLER(*err);
}

/*
 * The inmemory transport is reliable.  Should not resend.
 */
static          ilu_boolean
_inmem_SendWholeMessage(ilu_Transport self, ilu_Message * msgh,
		      ILU_ERRS((IoErrs)) * err)
{
  return ILU_ERR_CONS1(internal, err, minor, ilu_im_tcReliable,
		       ilu_FALSE);
}


/*
 * will need some work 
 */
static          ilu_boolean
_inmem_WriteBytes(ilu_Transport self, ilu_bytes buf,
		ilu_cardinal bufferSize,
		ilu_boolean flush, /* ignore */
		ILU_ERRS((IoErrs)) * err)
     /* If reallocating is not desirable, add a continue bit to
	BufferList, so that multiple BL's may represent one message.
	Affects Begin/EndMessage, Read/WriteBytes, others.  */
{

  TransportParms  tp = (TransportParms) self->tr_data;
  ilu_bytes       b;
  ilu_cardinal    more = bufferSize + 16;

  /* Error if not working on a message */
  if (!tp->td_working)
    return ILU_ERR_CONS1(internal, err, minor,
			 ilu_im_bytesWithoutMsg, ilu_FALSE);

  if ((self->tr_outNext + bufferSize + 16) >= self->tr_outLimit) {
    ilu_cardinal    nusize;
    nusize = tp->td_current->size + MAX(tp->td_current->size, more);
    b = (ilu_bytes) ilu_realloc(self->tr_outBuff, nusize);
    if (b == NIL)
      return ILU_ERR_CONS1(no_memory, err, nbytes,
			   nusize, ilu_FALSE);
    self->tr_outLimit = tp->td_current->size = nusize;
    self->tr_outBuff = tp->td_current->buffer = b;
  }
  if (buf != NIL) {
    /* Copy it into the exposed buffer */
    memcpy((void *) (self->tr_outBuff + self->tr_outNext),
	   (void *) buf, (SIZE_T) bufferSize);
    self->tr_outNext += bufferSize;
  }
  return ILU_CLER(*err);
}

static          ilu_cardinal
_inmem_WriteBytesNonblock(ilu_Transport self, ilu_bytes buf,
			  ilu_cardinal bufferSize,
			  ilu_boolean flush,	/* ignore */
			  ilu_boolean * flushed,
			  ILU_ERRS((IoErrs)) * err)
{
  ilu_boolean     ok = _inmem_WriteBytes(self, buf, bufferSize, flush,
					 err);
  return (ok ? bufferSize : 0);
}


static          ilu_cardinal
_inmem_ReadBytes(ilu_Transport self,
	       ilu_bytes buffer,
	       ilu_cardinal len,
	       ilu_TransportReport * rpt,
	       ILU_ERRS((IoErrs)) * err)
{
  
  TransportParms tp = (TransportParms) self->tr_data;

  rpt->tr_eom = rpt->tr_eof = ilu_FALSE;
  
  /* Error if not working on a message */
  if (!tp->td_working)
    return ILU_ERR_CONS1(internal, err, minor, ilu_im_bytesWithoutMsg, 0);

  /* Error if exposed buffer holds more input. */
  if (self->tr_inNext != self->tr_inLimit)
    return ILU_ERR_CONS1(internal, err, minor, ilu_im_tcInputSkipsBuff,
			 0);

  /* The only place to read from is the exposed buffer itself,
     and since it does not hold more input, this must be EOM. */
  rpt->tr_eom = ilu_TRUE;
  ILU_CLER(*err);
  return 0;
}


/*L1, L2 unconstrained*/

static void CloseSharedData(SharedData sd)
{
  BufferList bl, next;

  if (sd->sd_request != NIL) {
    ilu_free(sd->sd_request->buffer);
    ilu_free(sd->sd_request);
  }
  if (sd->sd_repliesHead != NIL) {
    for (bl = sd->sd_repliesHead ; bl != NIL ; bl = next)  {
      next = bl->next;
      ilu_free(bl->buffer);
      ilu_free(bl);
    }
  }
  if (sd->sd_reuseHead != NIL)  {
    for (bl = sd->sd_reuseHead ; bl != NIL ; bl = next)  {
      next = bl->next;
      ilu_free(bl->buffer);
      ilu_free(bl);
    }
  }
  ilu_free(sd);
}

static          ilu_boolean
_inmem_CloseTransport(ilu_Transport self, ilu_integer * dfd,
		      ILU_ERRS((bad_locks, broken_locks,
				internal)) * err)
{
  /*
   * Free anything that belongs to self. Outgoing transport
   * responsible for shared data.
   */
  TransportParms  tp = (TransportParms) self->tr_data;
  SharedData      sd = tp->td_sharedData;
  ILU_NOTE(INMEM_DEBUG,
	   ("ILU: _inmem_CloseTransport(self=%p,sd=%p)\n", tp, sd));
  *dfd = 0;
  if (tp->td_outgoing)
    CloseSharedData(tp->td_sharedData);
  else if (sd->sd_tih) {
    ILU_ERRS((internal, bad_param, bad_locks, broken_locks)) lerr;
    ilu_boolean     ans;
    ilu_Closure     c = _ilu_ClosureFromTIH(sd->sd_tih);
    ILU_NOTE(INMEM_DEBUG,
	     ("_inmem_CloseTransport(self=%p,sd=%p) => DoSoon(c=%p{proc=%p,rock=%p})\n",
	      tp, sd, c, c->proc, c->rock));
    ans = ilu_DoSoon(c, &lerr);
    ILU_ERR_SWITCH(lerr) {
      ILU_SUCCESS_CASE;
      ILU_ERR_CASE3(internal, bad_locks, broken_locks) * err = lerr;
      ILU_ERR_ELSE    (void)ilu_Check(ilu_FALSE, err);
    } ILU_ERR_ENDSWITCH;
    if (!ans)
      return ilu_FALSE;
  }
  if (tp->td_current != NIL) {
    ilu_free(tp->td_current->buffer);
    ilu_free(tp->td_current);
  }
  ilu_free(tp);
  self->tr_data = NIL;
  ilu_free(self);
  return ILU_CLER(*err);
}

/*
 * Set the transport class methods...
 * there is only one memClass.
 */

static struct _ilu_TransportClass_s memClass = {
  ilu_TRUE,			/* boundaried */
  ilu_TRUE,			/* reliable */
  _inmem_SetInputHandler,
  _inmem_WaitForInput,
  _inmem_Interrupt,
  _inmem_AbleInpWait,
  _inmem_AbleInpWait,
  _inmem_BeginMessage,
  _inmem_EndMessage,
  _inmem_BeginOutputMessageNonblock,
  _inmem_EndOutputMessageNonblock,
  _inmem_Push,
  _inmem_SendWholeMessage,
  _inmem_WriteBytes,
  _inmem_WriteBytesNonblock,
  _inmem_ReadBytes,
  _inmem_CloseTransport
};

/***********************************
  Methods of the in-memory mooring

  mo_set_req_handler
  mo_wait_for_req
  mo_accept_connection
  mo_close
  
  ***********************************/

/*L1, L2 unconstrained*/

static ilu_integer _inmem_MooringDFd(ilu_Mooring self, ilu_boolean add)
{
  return 0;
}

static          ilu_boolean
_inmem_SetReqHandler(ilu_Mooring self,
		     ilu_TIH tih,
		     ILU_ERRS((no_memory, internal)) * err)
{
  MooringParms    mp = (MooringParms) self->mo_data;
  if (!ilu_Check(mp->md_tih == NIL, err))
    return ilu_FALSE;
  mp->md_tih = tih;
  return ILU_CLER(*err);

}

static          ilu_boolean
_inmem_WaitForReq(ilu_Mooring self, int *disabled,
		  ILU_ERRS(()) * err)
{
  *disabled = 0;
  /* Should not be used for inmem. */
  _ilu_Assert(0, "_inmem_WaitForReq");
  return ilu_FALSE;
}

static          ilu_boolean
_inmem_AbleReqWait(ilu_Mooring self,
		  ILU_ERRS((bad_param)) * err)
{
  /* Should not be used for inmem. */
  return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_threading, ilu_FALSE);
}

/* L1.sup < cmu */
static          ilu_Transport
_inmem_NewT(ILU_ERRS((IoErrs)) * err)
     /* Used for outgoing and incoming transports. */
{
  ilu_Transport   ans;
  TransportParms parms;

  /* Allocate. */
  ans = (ilu_Transport) ilu_MallocE(sizeof(*ans), err);
  if (ans == NIL)
    return NIL;
  parms = (TransportParms) ilu_MallocE(sizeof(*parms), err);
  if (parms == NIL) {
    ilu_free(ans);
    return NIL;
  }

  /* Initialize. */
  parms->td_working = ilu_FALSE; /* set in Begin/EndMsg */
  parms->td_input = ilu_FALSE; /* set in BeginMsg */
  parms->td_outgoing = ilu_FALSE; /* set by caller */
  parms->td_sharedData = NIL; /* set by caller */
  parms->td_current = NIL;	/* set in Begin/EndMsg */

  ans->tr_class = &memClass;
  ans->tr_data = parms;
  ans->tr_wc = NIL;
  ans->tr_estFDs = 0;

  /* set by _inmem_BeginMessage */
  ans->tr_inBuff = ans->tr_outBuff = NIL;
  ans->tr_inLimit = ans->tr_outLimit = 0;
  ans->tr_inNext = ans->tr_outNext = 0;

  ILU_CLER(*err);
  return ans;
}

/* Main Invariant holds */
static          ilu_Transport
_inmem_AcceptClient(ilu_Mooring self, ilu_string * tinfo_out,
		    ilu_integer * dfd, ilu_Passport pp,
		    ILU_ERRS((IoErrs)) * err)
{
  /*
   * Create incoming transport. Called by connection request
   * handler.
   */

  MooringParms    mp = (MooringParms) self->mo_data;
  TransportParms  tp;
  ilu_Transport   ans;
  char            buf[32];

  *dfd = 0;
  _ilu_Assert((int) mp->md_locked, "_inmem_AcceptClient");
  /* signal ilu_im_brokenLocks? */

  if (tinfo_out != NIL) {
    sprintf(buf, "inmem(sd=%p)", mp->md_sharedData);
    *tinfo_out = ilu_StrdupE(buf, err);
    if (*tinfo_out == NIL)
      return NIL;
  }
  ans = _inmem_NewT(err);
  if (ILU_ERRNOK(*err))
    return NIL;

  tp = (TransportParms) ans->tr_data;
  tp->td_outgoing = ilu_FALSE;

  /* Connect to outgoing transport */
  tp->td_sharedData = mp->md_sharedData;
  mp->md_sharedData = NIL;
  mp->md_locked = ilu_FALSE;

  ILU_NOTE(INMEM_DEBUG,
	   ("ILU: _inmem_AcceptClient(%p): transport %p shared data %p\n",
	 self, ans, tp->td_sharedData));

  return ans;
}

static          ilu_boolean
_inmem_CloseMooring(ilu_Mooring self, ilu_integer * dfd,
		    ILU_ERRS((bad_locks, broken_locks,
			      internal)) * err)
{
  MooringParms    mp = (MooringParms) self->mo_data;
  ilu_Closure     c = _ilu_ClosureFromTIH(mp->md_tih);

  ILU_NOTE(INMEM_DEBUG,
	   ("ILU: _inmem_CloseMooring(%p): req handler=%p{%p(%p)}\n",
	    self, c, (c ? c->proc : 0), (c ? c->rock : 0)));
  if (c) {
    ILU_ERRS((internal, bad_param, bad_locks, broken_locks)) lerr;
    ilu_boolean     ans;
    ans = ilu_DoSoon(c, &lerr);
    ILU_ERR_SWITCH(lerr) {
      ILU_SUCCESS_CASE;
      ILU_ERR_CASE3(internal, bad_locks, broken_locks) * err = lerr;
      ILU_ERR_ELSE    (void) ilu_Check(ilu_FALSE, err);
    } ILU_ERR_ENDSWITCH;
    if (!ans)
      return ilu_FALSE;
  }
  *dfd = 0;
  ilu_free(mp);
  self->mo_data = NIL;
  ilu_free(self);
  return ILU_CLER(*err);
}



/****************************************************
  Methods of the in-memory transport/mooring creator

  tcr_dfd
  tcr_createTransport
  tcr_createMooring
  tcr_close

  ***************************************************/


/* L1, L2 unconstrained */

static          ilu_integer
_inmem_FdUsage(ilu_TransportCreator self, ilu_boolean mooring)
{
  return (0);
}

/* Main Invariant holds */
static          ilu_Transport
_inmem_CreateTransport(ilu_TransportCreator self, ilu_boolean buffer,
		       ilu_integer * dfd, ilu_Passport pp,
		       ILU_ERRS((IoErrs)) * err)
{
  /* Creates outgoing transport, makes call to create incoming transport
     and link the two.  (Will allocate buffers even if !buffer.) */
  
  CreatorParms    cp = (CreatorParms) self->tcr_data;
  MooringParms    mp;
  TransportParms  tp;
  ilu_Transport   ans;
  SharedData      sd;
  ilu_Error       lerr;

  *dfd = 0;
  ans = _inmem_NewT(err);
  if (ILU_ERRNOK(*err))
    return NIL;
  /* Exposed buffers are NIL until message begun. */
  
  /* Create the shared data space. */
    
  sd = (SharedData) ilu_MallocE(sizeof(struct SharedData_s), err);
  if (sd == NIL)  {
    _inmem_CloseTransport(ans, dfd, &lerr); /* clean up */
    ILU_HANDLED(lerr);
    return NIL;
  }
  sd->sd_bufferSize = BUFSIZE; /* could also be contained in tinfo? */
  sd->sd_tih = NIL;
  sd->sd_reuseHead = sd->sd_reuseTail = NIL;
  sd->sd_repliesTail = sd->sd_repliesHead = sd->sd_request = NIL;
  
  ILU_NOTE(INMEM_DEBUG,
	("ILU: _inmem_CreateTransport: Create outgoing transport %p sharedData %p\n",
	 ans, sd));
  
  tp = (TransportParms) ans->tr_data;
  tp->td_sharedData = sd;
  tp->td_outgoing = ilu_TRUE;
  
  if (cp != NIL)
    {
      mp = cp->cd_mooringData;
      if (mp != NIL)
	{
	  /* Get the mooring mutex.
	     Put pointer to the shared data space in mooring.
	     Call the mooring's registered conn-request handler. */
	  mp->md_locked = ilu_TRUE;
	  mp->md_sharedData = sd;
	  (*mp->md_tih->proc) (mp->md_tih->rock);
	  /* No return value.  Must call mo_accept_connection. */
	}
    }
  /* I think that's all. */
  return ans;
}

static struct _ilu_Mooring_s mooringProto = {
  _inmem_MooringDFd,
  _inmem_SetReqHandler,
  _inmem_WaitForReq,
  _inmem_AbleReqWait,
  _inmem_AbleReqWait,
  _inmem_AcceptClient,
  _inmem_CloseMooring,
  NIL,			/* data */
  NIL			/* mo_wc */
};

static ilu_Mooring 
_inmem_CreateMooring(ilu_TransportCreator self,
		     ilu_TransportInfo * tinfo_out,
		     ilu_boolean buffer, /* ignored */
		     ilu_integer *dfd,
		     ilu_Passport pp,	/* unused */
		     ILU_ERRS((IoErrs)) * err)
{
  CreatorParms    cp = (CreatorParms) self->tcr_data;
  MooringParms    mp;
  ilu_Mooring     ans;

  ILU_CLER(*err);
  *dfd = 0;

  _ilu_Assert(cp->cd_mooringData == NIL, "_inmem_CreateMooring");
  /* signal something? */
  
  ans = (ilu_Mooring) ilu_MallocE(sizeof(*ans), err);
  if (ans == NIL) {
    return NIL;
  }
  mp = (MooringParms) ilu_MallocE(sizeof(*mp), err);
  if (mp == NIL) {
    ilu_free(ans);
    return NIL;
  }
  mp->md_tih = NIL;
  mp->md_sharedData = NIL;
  mp->md_locked = ilu_FALSE;

  *ans = mooringProto;
  ans->mo_data = mp;

  /* create transport info */
  if (tinfo_out != NIL)
    {
      ilu_string t[1] = { NIL };
      *tinfo_out = _ilu_ConcatTinfo("inmem_", t, err);
      if (ILU_ERRNOK(*err))
	{
	  ilu_free(mp);
	  ilu_free(ans);
	  return NIL;
	}
    }

  /* Convey to outgoing transports. */
  cp->cd_mooringData = mp;
  
  ILU_NOTE(INMEM_DEBUG, ("ILU: _inmem_CreateMooring: ans=%p\n", ans));

  return ans;
  
}

/* L1, L2 unconstrained */

static void 
_inmem_CloseCreator(ilu_TransportCreator self)
{
  CreatorParms    cp = (CreatorParms) self->tcr_data;
  ilu_free(cp->cd_tinfo);
  ilu_free(cp);
  ilu_free(self);
  return;
}


/*
 * Set the transport creator methods...
 * There should be one TCr for every inmem port.
 */

static struct _ilu_TransportCreator_s creatorProto = {
  ilu_TRUE,				/* boundaried */
  ilu_TRUE,				/* reliable */
  0,				/* tcr_holds */
  ilu_FALSE,			/* tcr_wantClose */
  _inmem_FdUsage,
  _inmem_CreateTransport,
  _inmem_CreateMooring,
  _inmem_CloseCreator,
  NIL				/* data */
};

ilu_TransportCreator
_ilu_inmem_TransportCreator(ilu_TransportInfo tinfo,
			  ILU_ERRS((no_memory, inv_objref)) * err)
{
  ilu_TransportCreator ans;
  CreatorParms    cp;
  cp = (CreatorParms) ilu_MallocE(sizeof(*cp), err);
  if (cp == NIL)
    return NIL;
  ans = (ilu_TransportCreator) ilu_MallocE(sizeof(*ans), err);
  if (ans == NIL)
    return NIL;
  *ans = creatorProto;
  ans->tcr_data = cp;
  /* This will be set when the tCr creates a mooring and will not
     be changed after that:  */
  cp->cd_mooringData = NIL;
  /* might as well save it */
  cp->cd_tinfo = _ilu_CopyTinfo(tinfo, err);
  if (ILU_ERRNOK(*err))
    {
      ilu_free(ans);
      ilu_free(cp);
      return NIL;
    }
  ILU_CLER(*err);
  return (ans);
}

/*L1, L2 unconstrained*/

static          ilu_ReadHeaderResultCode
_buffer_BeginMessage(ilu_Transport self,
		     ilu_boolean input_p,
		     ILU_ERRS((IoErrs)) * err)
{
  return ILU_ERR_CONS1(internal, err, minor, ilu_im_tcNotBoundaried,
		       ilu_rhrc_error);
}

static          ilu_boolean
_buffer_EndMessage(ilu_Transport self,
		   ilu_boolean flush,
		   ilu_Message *msgh,
		   ILU_ERRS((IoErrs)) * err)
{
  return ILU_ERR_CONS1(internal, err, minor, ilu_im_tcNotBoundaried,
		       ilu_FALSE);
}

static          ilu_boolean
_buffer_BeginOutputMessageNonblock(ilu_Transport self,
				   ILU_ERRS((IoErrs)) * err)
{
  return ILU_ERR_CONS1(internal, err, minor, ilu_im_tcNotBoundaried,
		       ilu_FALSE);
}

static          ilu_TransportEndReport
_buffer_EndOutputMessageNonblock(ilu_Transport self,
				 ilu_boolean flush,
				 ilu_Message * msgh,
				 ILU_ERRS((IoErrs)) * err)
{
  ilu_TransportEndReport ans = {ilu_FALSE, ilu_FALSE};
  return ILU_ERR_CONS1(internal, err, minor, ilu_im_tcNotBoundaried,
		       ans);
}

/*
 * Set the transport class methods...
 * there is only one memClass.
 */

static struct _ilu_TransportClass_s bufMemClass = {
  ilu_FALSE,			/* boundaried */
  ilu_TRUE,			/* reliable */
  _inmem_SetInputHandler,
  _inmem_WaitForInput,
  _inmem_Interrupt,
  _inmem_AbleInpWait,
  _inmem_AbleInpWait,
  _buffer_BeginMessage,
  _buffer_EndMessage,
  _buffer_BeginOutputMessageNonblock,
  _buffer_EndOutputMessageNonblock,
  _inmem_Push,
  _inmem_SendWholeMessage,
  _inmem_WriteBytes,
  _inmem_WriteBytesNonblock,
  _inmem_ReadBytes,
  _inmem_CloseTransport
};

ilu_Transport
_ilu_BufferTransport_Create (ilu_cardinal size, ilu_bytes buffer,
			     ILU_ERRS((IoErrs)) * err)
{
  TransportParms  tp;
  ilu_Transport   t;
  SharedData      sd;
  ilu_Error       lerr;
  ilu_integer	  dfd;
  ilu_ReadHeaderResultCode rhrc;

  t = _inmem_NewT(err);
  t->tr_class = &bufMemClass;
  if (ILU_ERRNOK(*err))
    return NIL;
  /* Exposed buffers are NIL until message begun. */
  
  /* Create the shared data space. */
    
  sd = (SharedData) ilu_MallocE(sizeof(*sd), err);
  if (sd == NIL)  {
    _inmem_CloseTransport(t, &dfd, &lerr); /* clean up */
    ILU_HANDLED(lerr);
    return NIL;
  }
  sd->sd_bufferSize = size; /* could also be contained in tinfo? */
  sd->sd_tih = NIL;
  sd->sd_reuseHead = sd->sd_reuseTail = NIL;
  sd->sd_repliesTail = sd->sd_repliesHead = sd->sd_request = NIL;
  
  ILU_NOTE(INMEM_DEBUG,
	("_ilu_BufferTransport: Create outgoing inmem transport %p sharedData %p\n",
	 t, sd));
  
  tp = (TransportParms) t->tr_data;
  tp->td_sharedData = sd;
  tp->td_outgoing = ilu_FALSE;
  tp->td_working = ilu_FALSE;
  tp->td_current = NIL;

  if (buffer != NIL) {
    sd->sd_request = NewBufferList(size, buffer, err);
    if (ILU_ERRNOK(*err)) {
      _inmem_CloseTransport(t, &dfd, &lerr);
      return (NIL);
    }
    sd->sd_request->offset = size;
  }

  rhrc = _inmem_BeginMessage (t, (buffer != NIL), err);
  
  switch (rhrc) {
  case ilu_rhrc_ok:
    break;
  case ilu_rhrc_error:
    _inmem_CloseTransport(t, &dfd, &lerr);	/* clean up */
    ILU_HANDLED(lerr);
    return NIL;
  default:
    _ilu_Assert(ilu_FALSE, "inmem.c:BufferTransport");
  }

  return t;
}

/*L1, L2 unconstrained*/
void
_ilu_BufferTransport_Destroy(ilu_Transport self, ilu_cardinal * size,
			     ilu_bytes * buffer,
			     ILU_ERRS((IoErrs)) * err)
{
  TransportParms  tp = (TransportParms) self->tr_data;
  SharedData	sd = NIL;
  BufferList      current;
  ilu_integer     dfd;

  if (!tp->td_outgoing)
    sd = ((TransportParms) self->tr_data)->td_sharedData;
  current = tp->td_current;

  _inmem_EndMessage(self, ilu_TRUE, NIL, err);	/* XXX locking bug! */
  if (ILU_ERRNOK(*err))
    return;

  if (current != NIL) {
    if (size != NIL)
      *size = current->offset;
    if (buffer != NIL) {
      *buffer = current->buffer;
      current->buffer = NIL;
    }
  }

  _inmem_CloseTransport(self, &dfd, err);

  /*
   * We need to close this, as this transport is always (td_outgoing
   * == ilu_FALSE).
   */
  if (sd != NIL)
    CloseSharedData(sd);
}
