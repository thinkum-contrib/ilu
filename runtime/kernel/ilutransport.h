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
/* $Id: ilutransport.h,v 1.58 1999/08/03 01:53:21 janssen Exp $ */
/* Last edited by Mike Spreitzer September 10, 1998 3:44 pm PDT */

#ifndef _TRANSPORT_H_
#define _TRANSPORT_H_ 1

/* There are three `classes' defined in this header file.

   The `ilu_Transport' class serves as an abstraction of a
   source/sink for data.

   The `ilu_Mooring' class serves as an abstraction of a `place'
   for incoming connections to `tie up' at.  Moorings are related
   to Transports in that an incoming transport must tie up at
   a Mooring of the kind associated with that Transport class.

   The `ilu_TransportCreator' class serves as a factory for
   generating ilu_Transport instances, and their associated
   ilu_Mooring instances.
*/

typedef enum {
  ilu_rhrc_ok,			/* Header successfully started */
  ilu_rhrc_eof,			/* Found EOF instead of header */
  ilu_rhrc_nothing,		/* No input available w/o blocking */
  ilu_rhrc_error,		/* An error is being raised */
  ilu_rhrc_handled		/* A protocol-specific request was handled */
}               ilu_ReadHeaderResultCode;

typedef struct {
  /* L1, L2 unconstrained */

  ilu_boolean     tr_eom;
  ilu_boolean     tr_eof;
}               ilu_TransportReport;
/*
 * Used by tc_read_bytes to report whether end-of-message and/or
 * end-of-file follows bytes just read.  ("file" is a bit of a
 * misnomer; we just mean the end of whatever the source is.) tr_eom
 * is meaningful only for boundaried transports, which also set it
 * ilu_TRUE whenever they set tr_eof ilu_TRUE.
 */

/* Definition of transport metaobjects. */

struct _ilu_TransportCreator_s {
  /*
   * A TransportCreator is never changed once created, so these
   * fields are readonly.  Caller is responsible for using
   * sequentially (e.g., not making two concurrent calls); this
   * is in addition to the constraints in the locking comments.  The
   * locking comments refer to invocationns of the methods.
   */

  ilu_boolean     tcr_boundaried;
  ilu_boolean     tcr_reliable;
  
  /*L1 >= {cmu}; L2 unconstrained*/
  int             tcr_holds;
  ilu_boolean     tcr_wantClose;
  /*
   * tcr_holds and tcr_wantClose are for the use of the client of
   * this TransportCreator; the ilu_TransportInstantiator should
   * initialize them to 0 and ilu_FALSE (respectively).
   */
  
  /* L1.sup < trmu; L2 unconstrained */
  ilu_integer(*tcr_dfd) (ilu_TransportCreator /* self */ ,
			 ilu_boolean /* mooring */ );
  /*
   * Estimates how many FDs will be consumed when creating a mooring
   * (if mooring) or outgoing transport (if !mooring).  Estimate may
   * err high, but not low.
   */

  /* Main Invariant holds */
  ilu_Transport(*tcr_createTransport) (ilu_TransportCreator /* self */ ,
				       ilu_boolean /* buffer */ ,
				       ilu_integer * /* dfd */ ,
				       ilu_Passport	/* for getting
				           identities from */ ,
				  ILU_ERRS((IoErrs)) * /* err */ );
  /*
   * The Transport instance creation procedure.  Caller promises
   * exposed buffers won't be used if !buffer (in which case callee
   * is free to not allocate any).  The result is owned by the
   * caller (who will eventually close it).  Always stores at *dfd
   * the number of FDs actually consumed, even when erring.  Only
   * called when enough FDs (as opined by tcr_dfd) are available to
   * actually create the transport, so no need to check in the
   * implementation.
   */

  /* L1 >= {cmu}, L1.sup < trmu; L2 unconstrained */
  ilu_Mooring(*tcr_createMooring) (ilu_TransportCreator /* self */ ,
				   ilu_TransportInfo * /* tinfo_out */ ,
				   ilu_boolean /* buffer */ ,
				   ilu_integer * /* dfd */ ,
				   ilu_Passport /* passport */,
				   ILU_ERRS((IoErrs)) * /* err */ );
  /*
   * The Mooring instance creation procedure.  Caller promises
   * exposed buffers of results of mo_accept_connection won't be
   * used if !buffer (in which case mo_accept_connection is free to
   * not allocate any).  If tinfo_out != NIL, store through it a
   * fully-detailed transport info string, which will be passed to
   * ilu_GetTransportCreator in a peer.  Always store at *dfd the
   * number of FDs actually consumed.  Only called when enough FDs
   * (as opined by tcr_dfd) are available to actually create the
   * transport, so no need to check in the implementation.  The
   * results are owned by the caller (who will eventually close the
   * mooring and ilu_free the string).
   */

  /* L1, L2 unconstrained */
  void            (*tcr_close) (ilu_TransportCreator /* self */ );
  /* Even frees self. */

  ilu_refany      tcr_data;
  /* For the private use of the above methods. */
};

typedef struct {
  /* L1 >= {cmu}; L2 unconstrained */

  ilu_integer     iluwc_waitsDisabled;
  /*
   * (calls on mo/tc_disableWait(*)) - (calls on
   * mo/tc_enableWait(*)), summed over the transports and/or
   * moorings of this cohort; always >= 0.  mo/tc_disableWait and
   * mo/tc_enableWait update this.
   */

  ilu_Condition   iluwc_change;
  /*
   * Notified (a) by transport/mooring methods when
   * (!iluwc_waitsDisabled) changes, and (b) by client (i.e., "user"
   * code) whenever it wants to.
   */

  ilu_integer     iluwc_refcount;
  /*
   * Counts transports, moorings, and code that holds independent
   * references.
   */

  ilu_boolean     iluwc_global;

}              *ilu_WaitCohort;

/* L1 >= {cmu}; L2 unconstrained */
#define	ilu_DeltaWaitCohortRefCount(wc,drc,err) \
	ilu_FullDeltaWaitCohortRefCount(wc,drc,(err),__FILE__,__LINE__)
/*
 * Code that holds an ilu_WaitCohort past the times when its
 * transports and/or moorings might all be closed needs to use this
 * procedure to ensure the ilu_WaitCohort survives.
 */


/* L1 >= {cmu}; L2 unconstrained */
extern          ilu_boolean
ilu_FullDeltaWaitCohortRefCount(ilu_WaitCohort /* wc */ ,
				int /* dRefCount */ ,
				ILU_ERRS((internal)) * /* err */ ,
				const char * /* filename */ ,
				int /* linenum */ );
/* Used in the expansion of the above macro. */

/* L1 >= {cmu}; L2 unconstrained */
extern          ilu_boolean
_ilu_DeltaCohortWaits(ilu_WaitCohort /* wc */ ,
		      int /* dWaits */ ,
		      ILU_ERRS((broken_locks,
				internal)) * /* err */ );
/*
 * mo/tc_disableWait and mo/tc_enableWait can call this to update
 * iluwc_waitsDisabled and, if needed, notify iluwc_change.
 */

/*L1, L2 unconstrained*/
extern          ilu_WaitCohort
ilu_CreateWaitCohort(ilu_string d1, ilu_string d2,
		     ilu_boolean global,
		     ILU_ERRS((no_memory, no_resources,
			       internal)) * /* err */ );
/*
 * If multi-threaded, returns a new ilu_WaitCohort with
 * (iluwc_refcount==1); if single-threaded, returns NIL. (d1) and
 * (d2) describe this cohort.  Set (global) to cause checking that
 * the result's refcount never goes to 0.
 */

/*L1.sup < cmu; L2 unconstrained*/
extern          ilu_WaitCohort
ilu_GetNeverWaitCohort(ILU_ERRS((no_memory, no_resources,
				 internal)) * e);
/* Returns NIL iff single-threaded or error. */

/* L1 >= {cmu}; L2 unconstrained */
extern          ilu_WaitCohort
_ilu_GetNeverWaitCohort(ILU_ERRS((no_memory, no_resources,
				  internal)) * e);
/* Returns NIL iff single-threaded or error. */

/* Replaced by iluwc_waitsDisabled, _ilu_DeltaCohortWaits */
#if 0
extern ilu_integer _ilu_waitsDisabled;

/* L1 >= {cmu}; L2 unconstrained */
extern ilu_boolean _ilu_NoteWaitsAble(ILU_ERRS((broken_locks)) * err);
/*
 * tc_disableWait and tc_enableWait call this when
 * (_ilu_waitsDisabled==0) changes.
 */
#endif

/*
 * An ILU transport is used to transport either bytes or messages
 * bidirectionally between peers.
 * 
 * Each transport is either message-oriented (we say "boundaried") or
 * byte-oriented. For a boundaried transport, the caller makes
 * explicit calls to begin and finish processing each message; for a
 * non-boundaried transport these calls are not made.  The content
 * of each message is a byte sequence, processed with the same calls
 * that process the whole content of a non-boundaried trasnport.
 * 
 * Some transports are reliable, others aren't.  Only boundaried
 * transports may be unreliable.  An unreliable transport may drop
 * or re-order messages; higher layers cope with this through the
 * use of timeouts, retransmissions, and duplicate-handling
 * techniques.
 */

struct _ilu_Transport_s {
  /* L1 unconstrained */

  /*
   * Following are a pair of buffers used by both generic procs and
   * tr_class.  When tr_inBuff != NIL, bytes at indices [tr_inNext,
   * tr_inLimit) are the next input bytes.  When tr_outBuff != NIL,
   * it contains some amount (known to the transport) of past
   * output, and space for more output at indices [tr_outNext,
   * tr_outLimit).  A boundaried transport presents no input bytes
   * when not inputting a message and no output space when not
   * outputting a message.  We place the buffers first in this
   * struct on the suspicion that some machines can access data at
   * small offsets more quickly than at large offsets.  The input
   * buffer has to be accessible under the same locking conditions
   * as tc_wait_for_input is callable.
   */
  /* L2 >= {ymu} */

  ilu_bytes       tr_inBuff;
  ilu_cardinal    tr_inNext, tr_inLimit;

  /* L2 >= {xmu} */

  ilu_bytes       tr_outBuff;
  ilu_cardinal    tr_outNext, tr_outLimit;

  /* L2 unconstrained */

  ilu_TransportClass tr_class;
  ilu_refany      tr_data;	/* For use by tr_class. */

  /* Timeouts used by callers of unreliable transports. */
  ilu_FineTime    tr_to1;	/* Initial timeout */
  ilu_FineTime    tr_toN;	/* Max timeout */
  ilu_FineTime    tr_tto;	/* Total timeout */

  ilu_byte        tr_tinBuff[16];
  /* Used by _ilu_transportGetInputBuffer. */

  ilu_WaitCohort  tr_wc;
  /*
   * This transport's wait-cohort.  NIL if single-threaded.  May
   * also be NIL if waiting is never disabled for this transport's
   * wait-cohort, or if waiting never happens (i.e.,
   * tc_wait_for_input always returns immediately).
   */
   
  /* L2 unconstrained */
  ilu_cardinal    tr_estFDs;
  /*
   * An estimate (may be high, may not be low) of the number of FDs
   * that would be freed by closing self.
   */
};

typedef struct {
  ilu_boolean     iluter_ended;
  ilu_boolean     iluter_flushed;
}               ilu_TransportEndReport;

/*
 * Each individual ilu_Transport and each ilu_Mooring has two
 * associated L2 mutexes that we call (xmu) and (ymu); exactly what
 * these are will vary from transport to transport.  The caller is
 * responsible for picking and respecting (xmu) and (ymu).
 * 
 * In a multi-threaded runtime, the constraints wrt (xmu) and (ymu)
 * clearly express the concurrency allowed.
 * 
 * In a single-threaded runtime, the intent is almost the same as for
 * multi-threaded use.  The only difference concerns the
 * tc_wait_for_input method, which may call the main loop with the
 * expectation that during that run of the main loop there can be
 * output, input-waiting, and/or input-wait-interrupting calls on
 * the same transport.  For a method that calls the main loop, no
 * locking comments can technically forbid any calls during that
 * main loop execution, because (a) an event handler could note that
 * it (running in the only thread in the whole program) already
 * holds the L2 mutexes that need to be held, and (b) an event
 * handler could temporarily release any L2 mutexes that it must not
 * hold. However, we generally expect this level of finesse is not
 * applied.  A method M1 that calls the main loop can generally
 * expect that the main loop will not call any event handler that in
 * turn calls any method M2 that requires any of the same L2 mutexes
 * as M1; clients of transports are required to uphold this
 * restriction. The only exceptions are when M2 is tc_wait_for_input
 * or tc_interruptST.
 * 
 * Note that, among other things, this means that tc_close(x) is not
 * called concurrently with any other method of (x), nor is it
 * called from a run of the main loop called from any method of (x).
 *
 * Shorthand ref card for x and y locks, where "=>" means "requires":
 *
 *		input	=> x & y
 *		output	=> x
 *		waiting	=> y
 *	 interrupt wait => x
 */

struct _ilu_TransportClass_s {
  /* A TransportClass is never changed or freed once created, so
   * these fields are readonly.  The locking comments refer to
   * invocationns of the methods. */

  ilu_boolean     tc_boundaried;	/* am I boundaried? */
  ilu_boolean     tc_reliable;		/* am I reliable? */

  /* L1.sup < trmu; L2 >= {xmu, ymu} */
  ilu_boolean(*tc_set_input_handler) (ilu_Transport /* self */ ,
				      ilu_TIH /* tih */ ,
				      ILU_ERRS((no_memory, internal,
					     no_resources)) * err);
  /*
   * Used for incoming and outgoing Transports.  Used only (a) in
   * single-threaded runtimes or (b) for the in-memory transport;
   * may raise internal/threading in the other situations.  When
   * (tih != NIL): normally returns ilu_TRUE after guaranteeing that
   * from now on, until changed by another call to this proc,
   * presence of input or EOF will cause a call on this procedure,
   * which should read a message and deal with it.  If input
   * progress can be made now, returns ilu_FALSE without setting the
   * handler.  Exposed buffer is empty, else may raise
   * internal/tcInputSkipsBuff.  Caller is responsible for making
   * sure the main loop calls (tih) only at times that respect
   * (tih)'s L2 exclusions.  When (tih == NIL): cancels previous
   * setting.  In either case: also returns ilu_FALSE when raising an
   * error.  Caller retains ownership of (tih), but promises to not
   * modify or free it until after its last call (which will follow
   * tc_close).
   */
  
  /* Main Invariant holds; L2 >= {ymu} */
  ilu_boolean(*tc_wait_for_input) (ilu_Transport /* self */ ,
				   int * /* disabled */ ,
				   ilu_FineTime * /* limit */ ,
				   ILU_ERRS((broken_locks,
				       interrupted)) * /* err */ );
  /*
   * Returns ILU_ERROK(*err) after either: (1) there is a decent
   * reason to suspect that, after a tc_begin_message if boundaried
   * and not already reading a message, "input progress" can be
   * made, or an error or EOM or EOF can be detected, without
   * blocking, (2) *limit passed (limit==NIL means *limit ==
   * +infinity), or (3) interrupt requested in a multi-threaded
   * runtime (in which case interrupted is raised), (4)
   * tc_interruptST(self, ..) was called in a single-threaded
   * runtime (in which case interrupted is *not* raised), (5) the
   * runtime is multi-threaded and waiting is disabled for self's
   * wait-cohort (self->tr_wc).  Sets (*disabled), to a non-zero
   * value when only (5) applies; this is only a hint, and can be
   * wrong (with the only adverse consequence being a small
   * performance hit) --- as long as false positives are not
   * frequent.  The exposed input buffer holds no more input (else
   * internal/tcInputSkipsBuff is raised).  See
   * transport_wait_for_input for a convenient wrapper.  "Input
   * progress" means any advancement in the state of any stage in
   * the input pipeline --- regardless of whether any bytes dribble
   * out this end.  Used in single-threaded and multi-threaded
   * runtimes.  In S-T R/T, blocks by running main loop; in M-T,
   * blocks only the calling thread.
   */

  /* L1.sup < trmu; L2 >= {xmu} */

  ilu_boolean(*tc_interruptST) (ilu_Transport /* self */ ,
				ILU_ERRS((bad_param)) * /* err */ );
  /*
   * Applicable only in single-threaded runtime.  Causes current
   * calls on tc_wait_for_input, tc_begin_message, tc_end_message,
   * and tc_write_bytes to return in the appropriate way.
   */

  /* L1 >= {cmu}; L1.sup < trmu; L2 >= {xmu} */

  ilu_boolean(*tc_disableWait) (ilu_Transport /* self */ ,
				ILU_ERRS((broken_locks, bad_param,
					  internal)) * /* err */ );
  ilu_boolean(*tc_enableWait) (ilu_Transport /* self */ ,
			       ILU_ERRS((broken_locks, bad_param,
					 internal)) * /* err */ );
  /*
   * Applicable only in multi-threaded runtime.  Caller ensures that
   * for each T, at every moment, the number of tc_disableWait(T,..)
   * calls that have been made so far is always at least as great as
   * the number of tc_enableWait(T,..) calls, regardless of thread
   * scheduling.  Moorings/Transports are partitioned into disjoint
   * sets called `wait-cohorts'.  Each mooring and each transport is
   * in charge of identifying which set it belongs to (tr_wc), but
   * it is not possible to enumerate the members of a given set.
   * While there have been more {mo,tc}_disableWait calls on members
   * of (self)'s wait-cohort than {mo,tc}_enableWait calls, we say
   * waiting is disabled for (self)'s wait-cohort.  This condition
   * affect the behavior of tc_wait_for_input, above.
   */

  /* Main Invariant holds; L2 >= {xmu}; input => L2 >= {ymu} */

  ilu_ReadHeaderResultCode(*tc_begin_message) (ilu_Transport /* self */ ,
					  ilu_boolean /* input */ ,
				  ILU_ERRS((IoErrs)) * /* err */ );
  /*
   * Begin a new message in the given direction (with caveats, in
   * the input case).  Raises internal/tcNotBoundaried if transport
   * is not boundaried.  Raises internal/beginMessage if not done
   * with previous message.  For output, returns either ilu_rhrc_ok
   * or _error.  May return any of the following four codes for
   * input.  Returns _ok if input message successfully started.
   * Returns _eof if EOF instead of a message was waiting; caller
   * will close the ilu_Transport.  May return _nothing to indicate
   * EOF has not been detected and message has not been started (due
   * to lack of input).  Must not block before start of EOF or next
   * message is detected; otherwise, might block.  When
   * single-threaded, will raise interrupted (with meaningless
   * ilu_interruptSet) if tc_interruptST invoked while blocked; when
   * MT, will raise interrupted (with meaningful ilu_interruptSet)
   * when the alert-this-thread signal is detected.  Returns _error
   * iff raising an error, in which case the message has not been
   * started.  Never returns _handled.
   */

  ilu_boolean(*tc_end_message) (ilu_Transport /* self */ ,
				ilu_boolean /* flush */ ,
				ilu_Message * /* msg */ ,
				ILU_ERRS((IoErrs)) * /* err */ );
  /*
   * Finish the current message in the current direction.  If output
   * and flush, be sure to start it on its way; otherwise, this call
   * might do blocking I/O, and/or it might schedule I/O to be done
   * later.  If single-threaded, blocking is done via the main loop;
   * if multi-threaded, blocking affects only the calling thread. If
   * unreliable and output, return a copy (including ownership) of
   * the whole message (i.e., all the bytes given to tc_write_bytes)
   * to the caller through OUT parameter (msg), so that it may later
   * be retransmitted with tc_send_whole_message; otherwise ignore
   * (msg).  Raises interrupted as for tc_begin_message.  Raises
   * internal/endMessage if not currently I/Oing a message.  Will
   * raise internal/tcBytesDropped if direction is input and the
   * whole message has not yet been read, unless the inability to
   * block prevents accurate determination of whether that condition
   * holds.  Raises internal/tcNotBoundaried if transport is not
   * boundaried.  Raises internal/tcNoMsgHandle if unreliable,
   * output, and msg == NIL.
   */

  /*L1.sup < trmu; L2 >= {xmu}*/

  ilu_boolean(*tc_begin_output_message_nonblock)
    (ilu_Transport /* self */ ,
     ILU_ERRS((IoErrs)) * /* err */ );
  /*
   * Begin a new output message, subject to the constraint that the
   * call may not block.  Raises internal/tcNotBoundaried if
   * transport is not boundaried.  Raises internal/beginMessage if
   * not done with previous message.  Returns ilu_FALSE if raising error
   * or message not begun.
   */

  ilu_TransportEndReport(*tc_end_output_message_nonblock)
    (ilu_Transport /* self */ ,
     ilu_boolean /* flush */ ,
     ilu_Message * /* msg */ ,
     ILU_ERRS((IoErrs)) * /* err */ );
  /*
   * Finish the current output message, to the degree possible
   * without blocking.  If flush, try to start it on its way;
   * otherwise, relevant bytes might languish somewhere in the
   * pipeline until another call provokes progress.  Returns a pair
   * of bits that indicate (1) whether the output message mode has
   * been succesfully exited and (2) whether the requested flush has
   * been done.  If unreliable and output, returns a copy (including
   * ownership) of the whole message (i.e., all the bytes given to
   * tc_write_bytes) to the caller through OUT parameter (msg), so
   * that it may later be retransmitted with tc_send_whole_message;
   * otherwise ignore (msg).  Raises internal/endMessage if not
   * currently I/Oing a message.  Raises internal/endMessageDir if
   * currently inputting a message.  Raises internal/tcNotBoundaried
   * if transport is not boundaried.  Raises internal/tcNoMsgHandle
   * if unreliable, output, and msg == NIL.
   */

  /*Main Invariant holds; L2 >= {xmu}*/

  ilu_boolean(*tc_push) (ilu_Transport /* self */ ,
			 ILU_ERRS((IoErrs)) * /* err */ );
  /*
   * Ensure buffered (both exposed and internal) output is on its
   * way.  If transport is boundaried, only applicable between
   * messages; raises internal on violations.  May return before
   * that output reaches any particular destination.  Returns
   * ILU_ERROK(*err).
   */


  ilu_boolean(*tc_send_whole_message) (ilu_Transport /* self */ ,
				       ilu_Message * /* msg */ ,
				  ILU_ERRS((IoErrs)) * /* err */ );
  /*
   * Applicable only to unreliable transports; others may raise
   * internal/tcReliable.  Like {begin_message; write_bytes;
   * end_message}, except that the message is not returned again at
   * the end.  Caller retains ownership of *msg and *msg->msg_base.
   */

  ilu_boolean(*tc_write_bytes) (ilu_Transport /* self */ ,
				ilu_bytes /* buf */ ,
				ilu_cardinal /* bufLen */ ,
				ilu_boolean /* flush */ ,
				ILU_ERRS((IoErrs)) * /* err */ );
  /*
   * Write the contents of the exposed buffer (if any), followed by
   * the given buffer (if buf != NIL).  If flush and not boundaried,
   * be sure to start these bytes on their way; flush is not
   * significant when boundaried.  Prepare the exposed buffer to
   * receive at least 16 more bytes of output.  Raises interrupted
   * as for tc_begin_message.  Raises internal/bytesWithoutMsg if
   * boundaried and not currently outputting a message.  Caller
   * retains ownership of buf.
   */

  /*L1.sup < trmu; L2 >= {xmu}*/
  ilu_cardinal(*tc_write_bytes_nonblock)
    (ilu_Transport /* self */ ,
     ilu_bytes /* buf */ ,
     ilu_cardinal /* bufLen */ ,
     ilu_boolean /* flush */ ,
     ilu_boolean * /* flushed */ ,
     ILU_ERRS((IoErrs)) * /* err */ );
  /*
   * Do as much of the following as possible without blocking: write
   * the contents of the exposed buffer (if any), followed by the
   * given buffer (if buf != NIL), followed, if (flush) and not
   * boundaried, by ensuring that all the written bytes are on their
   * way.  (flush) is not significant when boundaried.  Returns the
   * number of bytes written from the given buffer. If all the
   * output was done, prepare the exposed buffer to receive at least
   * 16 more bytes of output. If all the output was done, (self) is
   * not boundaried, (flush), and (flushed), returns through
   * (*flushed) an indication of whether the flush was done.  Raises
   * internal/bytesWithoutMsg if boundaried and not currently
   * outputting a message.  Caller retains ownership of buf.
   */


  /*Main Invariant holds; L2 >= {xmu, ymu}*/

  ilu_cardinal(*tc_read_bytes) (ilu_Transport /* self */ ,
				ilu_bytes /* buf */ ,
				ilu_cardinal /* bufLen */ ,
				ilu_TransportReport * /* rpt */ ,
				ILU_ERRS((IoErrs)) * /* err */ );
  /*
   * Read some bytes into the given buffer (buf==NIL means the
   * exposed buffer is being given and bufLen is not meaningful).
   * The exposed input buffer holds no more input (else
   * internal/tcInputSkipsBuff is raised).  When buf==NIL, result is
   * final inLimit - inNext.  Note that buf==NIL grants freedom to
   * transfer headers and trailers along with payload.  Does not
   * read past message boundary (if boundaried), EOF, or end of
   * given buffer.  Does not block (so any loop involving this
   * procedure must also involve tc_wait_for_input).  Makes some
   * input progress, if permitted by the above restrictions.  Sets
   * EOM and EOF bits in *rpt; may falsely set them to ilu_FALSE if at
   * least 1 byte was delivered.  An unboundaried transport always
   * sets EOM to ilu_FALSE.  EOF (between messages for boundaried
   * transports, anywhere at all for others) is not a reason to
   * raise an error.  Returns number of bytes transferred into given
   * buffer, even when an error is raised.  Raises
   * internal/bytesWithoutMsg if boundaried and not currently
   * inputting a message.  Caller retains ownership of *buf.
   */

  /*L1.sup < trmu; self->tr_wc => L1 >= {cmu}; L2 >= {xmu, ymu}*/
  ilu_boolean(*tc_close) (ilu_Transport /* self */ ,
			  ilu_integer * /* dfd */ ,
			  ILU_ERRS((bad_locks, broken_locks,
				    internal)) * /* err */ );
  /*
   * Even frees self.  Stores at *dfd the number of FDs freed,
   * regardless of whether an error is raised.  If there was an
   * input handler registered, arrange that it's eventually called.
   */
};

struct _ilu_Mooring_s {		/* transport-level port */
  /* L1, L2 unconstrained for access; calling comments folllow. */

  /*L1.sup < trmu; L2 >= {xmu, ymu}*/
  ilu_integer(*mo_dfd) (ilu_Mooring /* self */ , ilu_boolean /* add */ );
  /*
   * Estimates how many FDs will be freed by closing this mooring
   * (if !add) or consumed by accepting a connection (if add).  In
   * either case, may err high but not low.
   */
  
  /*L1.sup < trmu; L2 >= {xmu, ymu}*/
  ilu_boolean(*mo_set_req_handler) (ilu_Mooring /* self */ ,
				    ilu_TIH /* tih */ ,
				    ILU_ERRS((no_memory, imp_limit,
					no_resources, broken_locks,
					      internal)) * err);
  /*
   * In a single-threaded runtime, call this to register a
   * connection request handler with the main loop.  If (tih != NIL)
   * and a connection request is probably present already, returns
   * ilu_FALSE without setting the handler.  Also returns ilu_FALSE if
   * ILU_ERRNOK(*err).  Caller retains ownership of (tih), but
   * promises to not modify or free it until after its last call
   * (which will follow mo_close).
   */

  /*Main Invariant holds; L2 >= {ymu}*/
  ilu_boolean(*mo_wait_for_req) (ilu_Mooring /* self */ ,
				 int * /* disabled */ ,
				 ILU_ERRS((interrupted,
					   broken_locks)) * err);
  /*
   * In a multi-threaded runtime, a thread calls this to wait for
   * the next connection request.  Returns ILU_ERROK(*err) after
   * either: (1) there is a decent reason to suspect that a
   * connection request is present, or an error or EOM or EOF can be
   * detected without blocking, (2) the thread system's generic
   * interrupt-that-thread function has been invoked on this thread
   * (in which case interrupted is raised), or (3) waiting is
   * disabled for (self)'s wait-cohort (see mo_disableWait).  Sets
   * (*disabled), to a non-zero value when only (3) applies; this is
   * only a hint, and can be wrong (with the only adverse
   * consequence being a small performance hit) --- as long as false
   * positives are not frequent.  Caller guarantees that (self) is
   * not closed at start of call (of course); callee guarantees that
   * (self) is not closed at time of return.
   */

  /* L1 >= {cmu}; L1.sup < trmu; L2 >= {xmu} */

  ilu_boolean(*mo_disableWait) (ilu_Mooring /* self */ ,
				ILU_ERRS((broken_locks,
					  bad_param,
					  internal)) * /* err */ );
  ilu_boolean(*mo_enableWait) (ilu_Mooring /* self */ ,
			       ILU_ERRS((broken_locks,
					 bad_param,
					 internal)) * /* err */ );
  /*
   * Applicable only in multi-threaded runtime.  Caller ensures that
   * for each M, at every moment, the number of mo_disableWait(M,..)
   * calls that have been made so far is always at least as great as
   * the number of mo_enableWait(M,..) calls, regardless of thread
   * scheduling.  Moorings/Transports are partitioned into disjoint
   * sets called `wait-cohorts'.  Each mooring and each transport is
   * in charge of identifying which set it belongs to (see mo_wc),
   * but it is not possible to enumerate the members of a given set.
   * While there have been more {mo,tc}_disableWait calls on members
   * of (self)'s wait-cohort than {mo,tc}_enableWait calls, we say
   * waiting is disabled for (self)'s wait-cohort.  This condition
   * affecte the behavior of mo_wait_for_req, above.
   */

  /* Main Invariant holds; L2 >= {xmu, ymu} */
  
  ilu_Transport(*mo_accept_connection) (ilu_Mooring /* self */ ,
				      ilu_string * /* peerinfo */ ,
					ilu_integer * /* dfd */ ,
					ilu_Passport	/* for stuffing
					    identities into */ ,
				  ILU_ERRS((IoErrs)) * /* err */ );
  /*
   * Create a new transport in response to the waiting connection
   * request; will return NIL if there wasn't really a connection
   * request waiting.  Also returns (with ownership) through
   * peerinfo (if not NIL) a string (not necessarily a real "tinfo")
   * identifying the peer.  Stores at *dfd the number of FDs
   * consumed (regardless of whether error is raised).
   */

  /*L1.sup < trmu; L1 >= {cmu}; L2 >= {xmu, ymu}*/
  ilu_boolean(*mo_close) (ilu_Mooring /* self */ ,
			  ilu_integer * /* dfd */ ,
			  ILU_ERRS((bad_locks, broken_locks,
				    internal)) * /* err */ );
  /*
   * Even frees self.  Stores at *dfd the number of FDs freed (even
   * if error raised).  If there was a request handler registered,
   * arrange that it's eventually called.
   */

  /*L1, L2 unconstrained*/
  ilu_private     mo_data;
  /* For private use by above methods. */
  
  /*L1 >= {cmu}; L2 unconstrained*/
  ilu_WaitCohort  mo_wc;
  /*
   * This mooring's wait-cohort.  NIL if single-threaded.  May also
   * be NIL if waiting is never disabled for this mooring's
   * wait-cohort, or if waiting never happens (i.e., mo_wait_for_req
   * always returns immediately).
   */
};


/* L1.sup < trmu; L2 unconstrained */
typedef         ilu_TransportCreator
ilu_TransportInstantiator(ilu_TransportInfo /* tinfo */ ,
			  ILU_ERRS((no_memory,
				    inv_objref)) * /* err */ );
/*
 * Something that parses "transport info" and returns corresponding
 * a ilu_TransportCreator.
 */

/* L1, L2 unconstrained */
ILU_PUBLIC
ILU_ERRS((TransportAlreadyRegistered, MaxCountExceeded))
ilu_RegisterTransport(char *tname,
		      ilu_TransportInstantiator * instantiator,
		      ilu_boolean override_existing_registration);
/*
 * There probably should be some non-trivial locking comments on
 * this proc.  You don't want to make concurrent calls on it, and
 * don't want to call it concurrently with anything that might want
 * to search the registry.
 */

/* L1.sup < cmu; L2 >= {xmu, ymu} */

extern          ilu_boolean
ilu_CloseTransport(ilu_Transport /* self */ ,
		   ilu_integer * /* dfd */ ,
		   ILU_ERRS((bad_locks, broken_locks,
			     internal)) * /* err */ );

extern          ilu_boolean
ilu_CloseMooring(ilu_Mooring /* self */ ,
		 ilu_integer * /* dfd */ ,
		 ILU_ERRS((bad_locks, broken_locks,
			   internal)) * /* err */ );

/* L1, L2 constrained appropriately for each macro's expansion */


/* some field accessors */

#define transport_data(bs)		((bs)->tr_data)
#define transport_class(bs)		((bs)->tr_class)

#define transport_reliable(bs)	((bs)->tr_class->tc_reliable)
#define transport_boundaried(bs)  ((bs)->tr_class->tc_boundaried)

/* some macros for method dereferencing */

#define transport_close(bs,dfd,err)			((*((bs)->tr_class->tc_close))(bs,dfd,err))
#define transport_interruptST(bs,err)		((*((bs)->tr_class->tc_interruptST))((bs),(err)))
#define transport_disableWait(bs,err)		((*((bs)->tr_class->tc_disableWait))((bs),(err)))
#define transport_enableWait(bs,err)		((*((bs)->tr_class->tc_enableWait))((bs),(err)))

#define transport_begin_message(bs,inputp,err)	((*((bs)->tr_class->tc_begin_message))((bs),(inputp),(err)))
#define transport_end_message(bs,flushp,msgh,err)	((*((bs)->tr_class->tc_end_message))((bs),(flushp),(msgh),(err)))
#define transport_begin_output_nonblock(bs,err)	((*((bs)->tr_class->tc_begin_output_message_nonblock))((bs),(err)))
#define transport_end_output_nonblock(bs,flushp,msgh,err)	((*((bs)->tr_class->tc_end_output_message_nonblock))((bs),(flushp),(msgh),(err)))
#define transport_send_whole_message(bs,msgh,err)	((*((bs)->tr_class->tc_send_whole_message))((bs),(msgh),(err)))
#define transport_set_input_handler(bs,tih,err)	((*((bs)->tr_class->tc_set_input_handler))((bs),(tih),(err)))

/* Main Invariant holds; L2 >= {ymu} */
#define transport_wait_for_input(bs,disabled,limit,err)	\
(((bs)->tr_inBuff && (bs)->tr_inNext < (bs)->tr_inLimit)\
 ? (*(disabled) = 0, ILU_CLER(*(err)))			\
 : ((*((bs)->tr_class->tc_wait_for_input))		\
    (bs, disabled, limit, err)))
/*
 * Like tc_wait_for_input, but first checks the exposed input
 * buffer.  See _ilu_TransportWaitForInputNoClose for a
 * sometimes-convenient wrapper.
 */

/* Main Invariant holds; L2 >= {xmu, ymu} */
extern          ilu_boolean
_ilu_TransportWaitForInputNoClose(ilu_Transport /* self */ ,
				  ilu_FineTime * /* limit */ ,
				  ILU_ERRS((bad_locks, broken_locks,
				       interrupted)) * /* err */ );
/*
 * Like transport_wait_for_input, but doesn't return for reason (5)
 * (instead it loops waiting for a different reason to return).
 * Caller guarantees tc_disableWait won't be called on (self) in the
 * interim.  Caller guarantees (self) won't be closed in the
 * interim.  These guarantees are easy to make, because calling
 * tc_disableWait or tc_close requires holding xmu, but xmu is held
 * for this call.
 */

/*Main Invariant holds; L2 >= {xmu}*/

extern          ilu_boolean
_ilu_transportWriteBytes(ilu_Transport /* self */ ,
			 ilu_bytes /* buf */ ,
			 ilu_cardinal /* len */ ,
			 ILU_ERRS((IoErrs)) * /* err */ );
/* Write the given buffer contents to the given transport. */

#define transport_write_bytes(bs,buf,len,err)	transport_write_bytes_maybeblock(bs,buf,len,ilu_TRUE,err)

#define transport_write_bytes_nonblock(bs,buf,len,err)	transport_write_bytes_maybeblock(bs,buf,len,ilu_FALSE,err)

#define transport_write_bytes_maybeblock(bs,buf,len,block,err)	transport_write_bytes_full(bs,buf,len,ilu_FALSE,((ilu_boolean*)0),block,err)

#define transport_write_bytes_full(bs,buf,len,flush,flushed,block,err)	\
(((!(flush)) && (bs)->tr_outBuff != NIL				\
  && (bs)->tr_outNext < (bs)->tr_outLimit			\
  && (len) <= (bs)->tr_outLimit - (bs)->tr_outNext)		\
 ? (memcpy((void *) ((bs)->tr_outBuff + (bs)->tr_outNext),	\
	   (void *) (buf), (len)),				\
    (bs)->tr_outNext += (len), ILU_CLER(*(err)), len)		\
 : (block							\
    ?((*(bs)->tr_class->tc_write_bytes)(bs, buf, len, flush, err),	\
      ((flush)?(*(flushed))=ILU_ERROK(*err):0),			\
      (ILU_ERROK(*err)?len:0))					\
    :(*(bs)->tr_class->tc_write_bytes_nonblock)(bs, buf, len,	\
					        flush, flushed, err)))

/* Generalized wrappers. */

extern ilu_bytes 
_ilu_transportGetOutputBuffer(ilu_Transport /* self */ ,
			      ilu_cardinal /* len */ ,
			      ILU_ERRS((IoErrs)) * /* err */ );
/*
 * Return the location where the next /len/ bytes of output are to
 * be put.
 */

#define transport_get_output_buffer(bs,len,err)			\
(((bs)->tr_outBuff != NIL && (bs)->tr_outNext < (bs)->tr_outLimit \
  && (len) <= (bs)->tr_outLimit - (bs)->tr_outNext)		\
 ? (ILU_CLER(*(err)),						\
    (bs)->tr_outBuff + ((bs)->tr_outNext += (len)) - (len))	\
 : _ilu_transportGetOutputBuffer(bs, len, err))
/* Efficient wrapper around above procedure. */

/*L1.sup < trmu; L2 >= {xmu}*/

extern          ilu_cardinal
_ilu_transportWriteBytesNonblock(ilu_Transport /* self */ ,
				 ilu_bytes /* buf */ ,
				 ilu_cardinal /* len */ ,
				 ILU_ERRS((IoErrs)) * /* err */ );
/*
 * Write as much of the given buffer contents to the given transport
 * as possible without blocking; returns number of bytes consumed.
 */

/*Main Invariant holds; L2 >= {xmu, ymu}*/

extern ilu_cardinal 
_ilu_transportReadBytes(ilu_Transport /* bs */ ,
			ilu_bytes /* buf */ ,
			ilu_cardinal /* len */ ,
			ILU_ERRS((IoErrs)) * /* err */ );
/* Read the next (len) bytes into given buffer. Returns num read */

#define transport_read_bytes(bs,buf,len,err)                     \
(((bs)->tr_inBuff != NIL && (bs)->tr_inNext < (bs)->tr_inLimit   \
  && (len) <= (bs)->tr_inLimit - (bs)->tr_inNext)                \
 ? (memcpy((void *) (buf),                                       \
	   (void *) ((bs)->tr_inBuff + (bs)->tr_inNext), (len)), \
    (bs)->tr_inNext += (len), ILU_CLER(*(err)), len)             \
 : _ilu_transportReadBytes(bs, buf, len, err))
/* Efficient wrapper around above procedure. */


extern ilu_bytes 
_ilu_transportGetInputBuffer(ilu_Transport /* self */ ,
			     ilu_cardinal /* len */ ,
			     ILU_ERRS((IoErrs)) * /* err */ );
/*
 * Return a location from which the next (len) bytes of input can be
 * read; len <= 16.
 */

#define transport_get_input_buffer(bs,len,err)			\
(((bs)->tr_inBuff != NIL && (bs)->tr_inNext < (bs)->tr_inLimit	\
  && (len) <= (bs)->tr_inLimit - (bs)->tr_inNext)		\
 ? (ILU_CLER(*(err)),						\
    (bs)->tr_inBuff + ((bs)->tr_inNext += (len)) - (len))	\
 : _ilu_transportGetInputBuffer(bs, len, err))
/* Efficient wrapper around above procedure. */

extern          ilu_cardinal
_ilu_transportReadUpToBytes(ilu_Transport /* self */ ,
			    ilu_bytes /* buf */ ,
			    ilu_cardinal /* len */ ,
			    ilu_TransportReport * /* rpt */ ,
			    ILU_ERRS((IoErrs)) * /* err */ );
/*
 * Read some bytes into the given buffer. Does not read past message
 * boundary (if boundaried), EOF, or end of given buffer.  Does not
 * block.  Makes as much input progress as is permitted by the above
 * restrictions.  Sets EOM and EOF bits in *rpt; may falsely set
 * them to ilu_FALSE if at least 1 byte was delivered.  Returns number
 * of bytes delivered.  Raises internal/bytesWithoutMsg if
 * boundaried and not currently inputting a message.  Caller retains
 * ownership of *buf.
 */

#define transport_read_upto_bytes(bs,buf,len,rpt,err)			\
(((bs)->tr_inBuff != NIL && (bs)->tr_inNext < (bs)->tr_inLimit		\
  && (len) <= (bs)->tr_inLimit - (bs)->tr_inNext)			\
 ? (memcpy((void *) (buf),						\
	   (void *) ((bs)->tr_inBuff + (bs)->tr_inNext), (len)),	\
    (rpt)->tr_eom = (rpt)->tr_eof = ilu_FALSE,				\
    (bs)->tr_inNext += (len), ILU_CLER(*(err)), (len))			\
 : _ilu_transportReadUpToBytes(bs, buf, len, rpt, err))
/* Efficient wrapper around above procedure. */

extern          ilu_boolean
_ilu_transportReadMessage(ilu_Transport /* self */ ,
			  ilu_bytes * /* msg */ ,
			  ilu_cardinal * /* len */ ,
			  ilu_TransportReport * /* rpt */ ,
			  ILU_ERRS((IoErrs)) * /* err */ );
/*
 * Applicable to boundaried transports currently inputting a
 * message.  Does not call begin_message.  Reads the rest of the
 * current message and returns it (the part read) in a contiguous
 * buffer.  Does not call end_message.  rpt->tr_eom is never
 * significant.
 */

/*L1.sup < trmu; L2 >= {xmu, ymu}*/
extern          ilu_boolean
_ilu_SetTransportInputHandler(ilu_Transport /* trans */ ,
			      ilu_TIH /* tih */ ,
			      ILU_ERRS((no_memory, no_resources,
					internal)) * /* err */ );
/*
 * If (tih != NIL) and (trans) has a non-empty exposed input
 * buffer, return ilu_FALSE; otherwise, call (trans)'s
 * tc_set_input_handler method.  Caller owns (tih).
 */

/*L1 >= {cmu}; L2 unconstrained*/
extern void _ilu_CloseTCR(ilu_TransportCreator tcr);
/* ... unless tcr_holds, in which case tcr_wantClose is set. */

/*L1 >= {cmu}; L2 unconstrained*/
extern void _ilu_DHoldsTCR(ilu_TransportCreator tcr, int dholds);
/* ... and close if tcr_wantClose && !tcr_holds. */

/* Main Invariant holds; L2 >= {t's xmu, ymu} */
extern          ilu_Transport
_ilu_BufferInputMessage(ilu_Transport,	/* t */
			ilu_cardinal,	/* nbytes */
			ilu_boolean, 	/* byBytes */
			ILU_ERRS((IoErrs)) * /* err */);
/*
 * Reads the rest of the message currently being input --- unless
 * and until error is raised.  Returns a reliable boundaried
 * transport that can be used to re-read those bytes, then reaches
 * EOM and EOF.  Close the result to free the last of its resources.
 * If (byBytes), transport must not be boundaried, and "the rest of
 * the message currently being input" consists of the next (nBytes)
 * bytes; otherwise, transport must be boundaried, and
 * transport_end_message(t, ..) is called at end.
 */

/*L1, L2 unconstrained*/
extern          ilu_Transport
_ilu_BufferTransport_Create(ilu_cardinal size, ilu_bytes buffer,
			    ILU_ERRS((IoErrs)) * err);
/*
 * Create a Buffer Transport, usable for one message in one
 * direction.  When (buffer) == NIL, create an output buffer,
 * initially (size); when (buffer != NIL), create an input buffer,
 * of (size).  If (buffer) is non-NIL, ownership of (buffer) is
 * transferred to the Transport.
 * 
 * No concurrent operations allowed on result.
 */

/*L1 unconstrained; L2 >= {xmu, ymu}*/
extern void
_ilu_BufferTransport_Destroy(ilu_Transport self, ilu_cardinal * size,
			     ilu_bytes * buffer,
			     ILU_ERRS((IoErrs)) * err);
/*
 * Ends message and tears down transport.  Call with non-NIL
 * "buffer" to transfer buffer back to caller; otherwise it's freed
 * when the transport is freed.
 */

/* L1.sup < trmu; L2 unconstrained */

#define MAX_TRANSPORTS	10

#ifdef TCPIP_TRANSPORT
extern ilu_TransportCreator 
_ilu_tcp_TransportCreator(ilu_TransportInfo tinfo,
			  ILU_ERRS((no_memory, inv_objref)) * err);
#endif /* TCPIP_TRANSPORT */

#ifdef UDPSOCKET_TRANSPORT
extern ilu_TransportCreator 
_ilu_udp_TransportCreator(ilu_TransportInfo tinfo,
			  ILU_ERRS((no_memory, inv_objref)) * err);
#endif /* UDPSOCKET_TRANSPORT */

#ifdef SUNRPCRM_TRANSPORT
extern ilu_TransportCreator 
_ilu_sunrpcrm_TransportCreator(ilu_TransportInfo tinfo,
			  ILU_ERRS((no_memory, inv_objref)) * err);
#endif /* def SUNRPCRM_TRANSPORT */

#ifdef SECURE_TRANSPORT
extern ilu_TransportCreator 
_ilu_gss_TransportCreator(ilu_TransportInfo tinfo,
			  ILU_ERRS((no_memory, inv_objref)) * err);
#endif /* def SECURE_TRANSPORT */

#ifdef W3MUX_TRANSPORT
extern ilu_TransportCreator 
_ilu_w3mux_TransportCreator(ilu_TransportInfo tinfo,
			    ILU_ERRS((no_memory, inv_objref)) * err);
#endif /* W3MUX_TRANSPORT */

#ifdef ILU_ZLIB_TRANSPORT
extern ilu_TransportCreator
_ilu_zlib_TransportCreator(ilu_TransportInfo tinfo,
			   ILU_ERRS((no_memory, inv_objref)) * err);
#endif

extern ilu_TransportCreator 
_ilu_inmem_TransportCreator(ilu_TransportInfo tinfo,
			  ILU_ERRS((no_memory, inv_objref)) * err);


/* L1, L2 unconstrained */

#ifndef MIN
#define MIN(a,b)	(((a)<(b))?(a):(b))
#endif

#ifndef MAX
#define MAX(a,b)	(((a)>(b))?(a):(b))
#endif

#endif
