/** $Id: iluprotocol.h,v 1.84 1999/08/03 01:53:22 janssen Exp $
 Beginilucopyright
 
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
/* Last edited by Mike Spreitzer January 19, 1999 1:36 pm PST */

#ifndef _ILU_PROTOCOL_H
#define _ILU_PROTOCOL_H

typedef enum ilu_PacketTypes {
  ilu_PacketType_Request = 0,
  ilu_PacketType_Reply = 1
} ilu_PacketType;

/* Definition of the ilu_Protocol metaobject type */

struct _ilu_Protocol_s {
  /*
   * A protocol is never changed or freed once created, so these
   * fields are readonly.  The locking comments refer to invocations
   * of the methods.
   */

  ilu_boolean	pr_concurrent_requests;
  /*
   * Does this protocol support concurrent requests on one
   * connection?
   */

  ilu_boolean	pr_sizing_required;
  /*
   * ilu_TRUE if this protocol requires accurate argument sizes to be
   * passed to pr_start_request, pr_begin_reply, and
   * pr_begin_exception.  When ilu_FALSE, implies that the pr_size_of_*
   * calls may be invalid or NIL.
   */

  ilu_boolean	pr_needs_boundaried_transport;
  /* ilu_TRUE if this protocol requires record marking at the top of
   * the transport stack.
   */

  ilu_boolean	pr_needs_reliable_transport;
  /* ilu_TRUE if this protocol requires a reliable transport layer.
   */

  /*L2 unconstrained*/
  /*L1.sup < prmu (prmu protects class<->prog&ver registry)*/
  ilu_string (*pr_form_handle)(ilu_refany, /* RETAIN, protocol instance data block */
			       ilu_Object  /* obj */);
  /*
   * Return the "pinfo" part of (obj)'s SBH.  String ownership
   * passed to caller.
   */

  /*L1, L2 unconstrained*/

  ilu_refany(*pr_create_data_block) (ilu_ProtocolInfo /* pinfo */,
				     ILU_ERRS((no_memory)) * /* err */);
  /*
   * Called during connection creation, so protocol can create
   * co_protocol_data.
   */

  void (*pr_free_data_block)(ilu_refany protocol_data);
  /*
   * Called during connection destruction, so protocol can free
   * co_protocol_data and whatever is owned by it.
   */

  /*L1 >= {cmu, conn's server}; L1.sup < trmu*/
  /*L2 >= {conn's iomu}*/
  void (*pr_conn_closing) (ilu_refany,	/* the data block */
			   ilu_ConnShutdownReason,	/* why */
			   ILU_ERRS((IoErrs)) * err);
  /*
   * Called when closing a connection.  May write to underlying
   * transport.  If pr_conn_closing is NULLFN, not called.
   */

  /*L1 >= {cmu, conn's server}; Main Remnant holds*/
  ilu_boolean(*pr_init_call) (ilu_Call call,
			      ILU_ERRS((IoErrs)) * err);
  /*
   * Called early in the life of incoming and outgoing calls, so the
   * protocol's various private data can be initialized; ca_server
   * ca_connection, and ca_tryIndex are the only fields that are
   * certainly initialized before entry; the connection is not
   * closed.  Also called when redirected to new contact info.
   * Next: sizing of arguments, or pr_read_header.
   */
  
  /*Main Invariant; Call-OHi(call)*/

  ilu_boolean(*pr_start_request) (ilu_Call call,
				  ilu_cardinal arg_size,
				  ILU_ERRS((IoErrs)) * err);
  /*
   * intro_type should be the class that originally defined the
   * method.  Can rely on call->ca_method and call->ca_intro_type
   * being already filled in.  Marshalling of arguments is next.
   */

  ilu_boolean(*pr_discard_output) (ilu_Call /* call */ ,
				   ILU_ERRS((IoErrs)) * /* err */ );
  /*
   * Call this any time between pr_start_request and
   * pr_finish_request, between pr_begin_reply and pr_finish_reply,
   * or between pr_begin_exception and pr_finish_exception, to abort
   * composing, and not send, the message being composed.  Caller
   * next calls pr_prefinish_call, pr_begin_sizing_exn, or
   * pr_begin_exception.
   */
  
  ilu_boolean(*pr_finish_request) (ilu_Call /* call */ ,
				   ilu_Message * /* msg */ ,
				   ilu_boolean /* push */ ,
				   ILU_ERRS((IoErrs)) * /* err */ );
  /*
   * End bracket of sending of the call parameters, including
   * discriminator.  If transport is reliable, (msg) is not
   * meaningful.  Otherwise, callee returns (including ownership) a
   * copy of the whole call message in (*msg).  Iff (!push), the
   * call message may linger in transport buffers until explicit
   * steps are later taken.  Next: if the method being called is
   * ASYNCHRONOUS, then the caller proceeds to pr_prefinish_call,
   * otherwise the caller waits for input on the call's connection
   * then calls pr_read_header.
   */

  /*Main Invariant; Call-IHi(call)*/

  ilu_ReadHeaderResultCode(*pr_read_header) (ilu_Call call,
					     ilu_PacketType * packetType,
					     ilu_cardinal * packetSN,
					     ilu_ConnShutdownReason *reason,
					     ilu_cardinal * lastSN,
					     ILU_ERRS((IoErrs)) * err);
  /*
   * Caller has reason to suspect that either EOF or a new message
   * is waiting on call's connection.  Check it out.  If a message
   * is found, begin interpreting it, setting *packetType and
   * *packetSN (if appropriate), and return ilu_rhrc_ok; caller will
   * then call pr_delay_interp, pr_interpret_reply,
   * pr_interpret_request, or pr_discard_input.  If EOF is found,
   * return ilu_rhrc_eof; caller will close the ilu_Connection and
   * proceed to pr_prefinish_call.  Note that finding EOF should not
   * be considered an error.  The "reason" and "lastSN" parameters
   * are valid only if EOF is returned.  "reason" signals the reason
   * for the EOF, if the protocol carries such information, or
   * ilu_ConnShutdownReason_ReceivedEOF otherwise.  If the protocol
   * carries information that makes it possible to determine what the
   * last serial number of the last message properly handled was, that
   * value is returned in "lastSN"; otherwise "lastSN" contains a value
   * of zero.  If it is impossible to tell whether
   * a message or EOF is next without blocking (i.e., caller was
   * wrong about something waiting), return ilu_rhrc_nothing; client
   * stub will go back to waiting for input, server stub will
   * proceed to pr_prefinish_call.  If a control message internal to
   * the protocol was received and handled, return ilu_rhrc_handled;
   * client stub will go back to waiting for an input, server stub
   * will proceed to pr_prefinish_call.  Return ilu_rhrc_error iff
   * raising an error; caller will proceed to pr_prefinish_call.
   */

  ilu_refany(*pr_delay_interp) (ilu_Call call,
				ILU_ERRS((IoErrs)) * err);
  /*
   * Formerly meaningful only in concurrent protocol; now also used
   * when pipelining calls on a non-concurrent protocol.  May be
   * called after pr_read_header, to save a copy of the current
   * reply message and interpreter state for use in a different
   * call.  On success, returns a thing that will later be used
   * exactly once, in a call of pr_resume_interp (with a different
   * call) or pr_abandon_delayed_interp, and advances call's
   * transport to just after the end of this message.  Upon return,
   * the call should be in such a state as to allow pr_read_header
   * to be called next, and the transport of the call should be in
   * such a state that the message being delayed has been advanced
   * over.  On failure, returns NIL, in which case the caller next
   * calls pr_discard_input then proceeds to pr_prefinish_call.
   */

  void            (*pr_resume_interp) (ilu_Call call, ilu_refany x);
  /*
   * Continue processing a reply started in the wrong call.  Same
   * connection used in both calls.  pr_interpret_reply will be
   * called next.
   */
  
  /*L1 >= {cmu}; L2 >= {conn's iomu, waitmu}*/
  ilu_boolean(*pr_abandon_delayed_interp) (ilu_Connection conn,
					   ilu_refany x,
				       ILU_ERRS((internal)) * err);
  /*
   * Sorry, pr_resume_interp will never be called on x; free
   * associated resources (including x itself).
   */

  ilu_ProtocolException(*pr_interpret_reply) (ilu_Call call,
					   ilu_cardinal * exn_code,
					      ILU_ERRS((IoErrs,
						 relocate)) * err);
  /*
   * After a reply packet with the right serial number and
   * type==ilu_PacketType_Reply has been found, call this to
   * continue decoding the reply msg.  The result is either success,
   * a protocol-level error, a relocate "error", or other kernel
   * error.  In the relocate case, the caller calls
   * pr_prefinish_call (note no call on pr_discard_input), and then
   * loops back to making a call on pr_init_call.  If a protocol
   * error or other kernel error is being reported, the caller next
   * calls pr_discard_input then proceeds to pr_prefinish_call, and
   * eventually passes the error to client code.  Otherwise, we're
   * in the success case.  In this case, this procedure also decodes
   * whether the marshalled results are normal results or an
   * exception parameter; `*exn_code` gets 0 for normal results,
   * otherwise 1 + (index into method's exception vector).
   * Unmarshalling of results/exception parameter is next, followed
   * by pr_reply_read.
   */

  ilu_boolean(*pr_discard_input) (ilu_Call /* call */ ,
				ILU_ERRS((internal)) * /* err */ );
  /*
   * Abandon processing current input message.  Call any time
   * between pr_read_header/pr_resume_interp and
   * pr_reply_read/pr_delay_interp on client side, between
   * pr_read_header and pr_request_read on server side.  Caller next
   * calls pr_prefinish_call or pr_begin_sizing_exn.
   */
  
  void            (*pr_reply_read) (ilu_Call, ILU_ERRS((IoErrs)) *);
  /*
   * Called after arguments have been unmarshalled.  Cleans up after
   * use of call's connection's co_protocol_data slot.  Note that
   * call's connection may already be closed.  Caller next calls
   * pr_prefinish_call.
   */

  ilu_boolean(*pr_interpret_request) (ilu_Call /* call */,
				      ILU_ERRS((IoErrs)) * /* err */);
  /*
   * Server stub calls this after pr_read_header.  Fills in the
   * intro_type and method fields of call; serialNumber, server,
   * connection fields are already set.  Stores in the call's
   * connection's co_protocol_data slot some private data used for
   * unmarshalling arguments.  Fails, returning ilu_FALSE, when setting
   * either *err or call->ca_pe to a failure; caller next calls
   * pr_discard_input then proceeds to pr_prefinish_call. If
   * success, caller next unmarshalls arguments.
   */

  void            (*pr_request_read) (ilu_Call, ILU_ERRS((IoErrs)) *);
  /*
   * Called after arguments have been unmarshalled.  Cleans up after
   * use of call's connection's co_protocol_data slot.  Note that
   * call's connection may already be closed.  Caller next executes
   * true method, then pr_begin_sizing_reply, pr_begin_sizing_exn,
   * pr_begin_exception, or pr_prefinish_call.
   */

  /*Main Invariant holds; L2 not further constrained*/
  ilu_cardinal(*pr_begin_sizing_reply) (ilu_Call call,
					ilu_boolean exns_possible,
					ILU_ERRS((IoErrs)) * err);
  /*
   * Starts computation of reply_size parm of pr_begin_reply; add to
   * result of this proc the sizes of all the results.
   */

  /*Main Invariant; Call-OHi(call)*/

  ilu_boolean(*pr_begin_reply) (ilu_Call call,
				ilu_boolean exceptions_possible,
				ilu_cardinal reply_size,
				ILU_ERRS((IoErrs)) * err);

  ilu_boolean(*pr_finish_reply) (ilu_Call call,
				 ilu_boolean /* push */ ,
				 ILU_ERRS((IoErrs)) * err);
  /*
   * pr_begin_reply and pr_finish_reply bracket the sending of the
   * results.  This proc calls _ilu_CacheCall if transport timesout.
   * Caller next calls pr_prefinish_call.
   */

  /*Main Invariant holds; L2 not further constrained*/
  ilu_cardinal(*pr_begin_sizing_exn) (ilu_Call call,
				      ilu_cardinal eindex,
				      ilu_ProtocolException sys_ex_index,
				      ILU_ERRS((IoErrs)) * err);
  /*
   * Starts computation of reply_size parm of pr_begin_exception,
   * when either a user or system exception is being returned.  Add
   * to result of this proc the size the exn's parm, if any.  For a
   * user exception, eindex is 1 + (the subscript into the method's
   * exceptionVector), and sys_ex_index is
   * ilu_ProtocolException_Success.  If a system exception is being
   * signalled, eindex should be 0, and the sys_ex_index value
   * should indicate the system exception.
   */

  ilu_boolean(*pr_begin_exception) (ilu_Call call,
				    ilu_cardinal exception_code,
				    ilu_ProtocolException sys_ex_index,
				    ilu_cardinal reply_size,
				    ILU_ERRS((IoErrs)) * err);
  /*
   * Introduce exceptional results, either a protocol error or a
   * programmer-defined one.  In the former case, exception_code=0
   * and sys_ex_index is the ilu_ProtocolException; in the latter
   * case, exception_code is 1 + (the subscript into the method's
   * exceptionVector) and sys_ex_index is
   * ilu_ProtocolException_Success.  reply_size is the marshalled
   * size of the exeption parameter.
   */

  ilu_boolean(*pr_finish_exception) (ilu_Call call,
				     ilu_boolean /* push */ ,
				     ILU_ERRS((IoErrs)) * err);
  /*
   * pr_begin_exception and pr_finish_exception bracket the sending
   * of exceptional results.  pr_finish_exception calls
   * _ilu_CacheCall if transport timesout.  Caller next calls
   * pr_prefinish_call.
   */

  /**L1 >= {cmu, calls' server}, L1.sup < trmu;
     Main Remnant holds; Call-OHi(call)*/
  ilu_boolean(*pr_prefinish_call) (ilu_Call call,
				   ILU_ERRS((IoErrs)) * err);
  /*
   * Called when a call is done with a connection (unless kernel is
   * too broken).  May be null, meaning NO-OP.  Called just before
   * pr_finish_call, and in case of relocation.  call's connection
   * may already be closed, but not freed.  (call->ca_method) and
   * (call->ca_intro_type) may be NIL (if we jumped here from
   * pr_read_header).  Next call is either pr_init_call or
   * pr_finish_call.
   */

  /*Main Invariant holds; L2 disjoint {call's conn}*/
  ilu_boolean(*pr_finish_call) (ilu_Call call,
				ILU_ERRS((IoErrs)) * err);
  /*
   * Called at the end of processing of an incoming or outgoing
   * call.  Shall not do anything that might free the call's last
   * connection, which might already be closed, or even freed.
   * (call->ca_method) and (call->ca_intro_type) may be NIL.
   */

  /* [Un]Marshalling routines */
  /*L1, L2 unconstrained for sizing, end*/
  /*Main Invariant, Call-OHi(call) for output*/
  /*Main Invariant, Call-IHi(call) for input*/
  
  void            (*pr_output_optional) (ilu_Call call, ilu_boolean i,
					 ILU_OPTIONAL(ilu_Type),
			                 ILU_ERRS((IoErrs)) * err);
  void            (*pr_input_optional) (ilu_Call call, ilu_boolean * i,
					ILU_OPTIONAL(ilu_Type),
			                 ILU_ERRS((IoErrs)) * err);
  ilu_cardinal(*pr_size_of_optional) (ilu_Call call, ilu_boolean i,
				      ILU_OPTIONAL(ilu_Type),
				      ILU_ERRS((IoErrs)) * err);

  void            (*pr_output_integer) (ilu_Call call, ilu_integer i,
			                 ILU_ERRS((IoErrs)) * err);
  void            (*pr_input_integer) (ilu_Call call, ilu_integer * i,
			                 ILU_ERRS((IoErrs)) * err);
  ilu_cardinal(*pr_size_of_integer) (ilu_Call call, ilu_integer i,
				     ILU_ERRS((IoErrs)) * err);

  void            (*pr_output_cardinal) (ilu_Call call, ilu_cardinal i,
			                 ILU_ERRS((IoErrs)) * err);
  void            (*pr_input_cardinal) (ilu_Call call, ilu_cardinal * i,
			                 ILU_ERRS((IoErrs)) * err);
  ilu_cardinal(*pr_size_of_cardinal) (ilu_Call call, ilu_cardinal i,
				      ILU_ERRS((IoErrs)) * err);

  void            (*pr_output_shortinteger) (ilu_Call call,
				                ilu_shortinteger i,
			                 ILU_ERRS((IoErrs)) * err);
  void            (*pr_input_shortinteger) (ilu_Call call,
			                      ilu_shortinteger * i,
			                 ILU_ERRS((IoErrs)) * err);
  ilu_cardinal(*pr_size_of_shortinteger) (ilu_Call call,
					  ilu_shortinteger i,
					  ILU_ERRS((IoErrs)) * err);

  void            (*pr_output_shortcardinal) (ilu_Call call,
			                       ilu_shortcardinal i,
			                 ILU_ERRS((IoErrs)) * err);
  void            (*pr_input_shortcardinal) (ilu_Call call,
			                     ilu_shortcardinal * i,
			                 ILU_ERRS((IoErrs)) * err);
  ilu_cardinal(*pr_size_of_shortcardinal) (ilu_Call call,
					   ilu_shortcardinal i,
					 ILU_ERRS((IoErrs)) * err);

  void            (*pr_output_real) (ilu_Call call, ilu_real i,
			                 ILU_ERRS((IoErrs)) * err);
  void            (*pr_input_real) (ilu_Call call, ilu_real * i,
			                 ILU_ERRS((IoErrs)) * err);
  ilu_cardinal(*pr_size_of_real) (ilu_Call call, ilu_real i,
				  ILU_ERRS((IoErrs)) * err);

  void            (*pr_output_shortreal) (ilu_Call call, ilu_shortreal i,
			                 ILU_ERRS((IoErrs)) * err);
  void            (*pr_input_shortreal) (ilu_Call call, ilu_shortreal * i,
			                 ILU_ERRS((IoErrs)) * err);
  ilu_cardinal(*pr_size_of_shortreal) (ilu_Call call, ilu_shortreal i,
				       ILU_ERRS((IoErrs)) * err);

  void            (*pr_output_longinteger) (ilu_Call call,
				                 ilu_longinteger i,
			                 ILU_ERRS((IoErrs)) * err);
  void            (*pr_input_longinteger) (ilu_Call call,
			                       ilu_longinteger * i,
			                 ILU_ERRS((IoErrs)) * err);
  ilu_cardinal(*pr_size_of_longinteger) (ilu_Call call,
					 ilu_longinteger i,
					 ILU_ERRS((IoErrs)) * err);

  void            (*pr_output_longcardinal) (ilu_Call call,
				                ilu_longcardinal i,
			                 ILU_ERRS((IoErrs)) * err);
  void            (*pr_input_longcardinal) (ilu_Call call,
			                      ilu_longcardinal * i,
			                 ILU_ERRS((IoErrs)) * err);
  ilu_cardinal(*pr_size_of_longcardinal) (ilu_Call call,
					  ilu_longcardinal i,
					  ILU_ERRS((IoErrs)) * err);

  void            (*pr_output_longreal) (ilu_Call call, ilu_longreal i,
			                 ILU_ERRS((IoErrs)) * err);
  void            (*pr_input_longreal) (ilu_Call call, ilu_longreal * i,
			                 ILU_ERRS((IoErrs)) * err);
  ilu_cardinal(*pr_size_of_longreal) (ilu_Call call, ilu_longreal i,
				      ILU_ERRS((IoErrs)) * err);

  void            (*pr_output_byte) (ilu_Call call, ilu_byte i,
			                 ILU_ERRS((IoErrs)) * err);
  void            (*pr_input_byte) (ilu_Call call, ilu_byte * i,
			                 ILU_ERRS((IoErrs)) * err);
  ilu_cardinal(*pr_size_of_byte) (ilu_Call call, ilu_byte i,
				  ILU_ERRS((IoErrs)) * err);

  void            (*pr_output_boolean) (ilu_Call call, ilu_boolean i,
			                 ILU_ERRS((IoErrs)) * err);
  void            (*pr_input_boolean) (ilu_Call call, ilu_boolean * i,
			                 ILU_ERRS((IoErrs)) * err);
  ilu_cardinal(*pr_size_of_boolean) (ilu_Call call, ilu_boolean i,
				     ILU_ERRS((IoErrs)) * err);

  /* this is a Unicode character */
  void            (*pr_output_character) (ilu_Call call, ilu_character i,
			                 ILU_ERRS((IoErrs)) * err);
  void            (*pr_input_character) (ilu_Call call, ilu_character * i,
			                 ILU_ERRS((IoErrs)) * err);
  ilu_cardinal(*pr_size_of_character) (ilu_Call call, ilu_character i,
				       ILU_ERRS((IoErrs)) * err);

  /* this is an ISO Latin-1 character using the ISO 8859-1 encoding */
  void(*pr_output_shortchar) (ilu_Call call, ilu_shortcharacter i,
				     ILU_ERRS((IoErrs)) * err);
  void(*pr_input_shortchar) (ilu_Call call,
				    ilu_shortcharacter * i,
				    ILU_ERRS((IoErrs)) * err);
  ilu_cardinal(*pr_size_of_shortchar) (ilu_Call call,
				       ilu_shortcharacter i,
				       ILU_ERRS((IoErrs)) * err);

  void(*pr_output_enum_code) (ilu_Call call, ilu_shortcardinal i,
			      ilu_Type prefix, ILU_ERRS((IoErrs)) * err);
  void(*pr_input_enum_code) (ilu_Call call, ilu_shortcardinal * i,
			     ilu_Type prefix, ILU_ERRS((IoErrs)) * err);
  ilu_cardinal(*pr_size_of_enum_code) (ilu_Call call,
				       ilu_shortcardinal i,
				       ilu_Type prefix,
				       ILU_ERRS((IoErrs)) * err);

  /* a string is a sequence of shortcharacter */
  void(*pr_output_string) (ilu_Call call,
			   void * s,				/* pointer to data */
			   ilu_cardinal length,			/* bytes of data */
			   ilu_cardinal limit,			/* specified max */
			   ilu_cardinal current_encoding,	/* IANA charset MIB */
			   ilu_cardinal expected_encoding,	/* IANA charset MIB */
			   ILU_ERRS((IoErrs)) * err);
  /* If expected_encoding is 0, marshal actual encoding
     onto wire along with encoding indicator; otherwise,
     convert to expected_encoding, and marshal that onto
     wire.  Or do something elese that may be protocol-dependent... */
  void(*pr_input_string) (ilu_Call call,
			  void ** s,				/* string, OUT */
			  ilu_cardinal * length,		/* in bytes */
			  ilu_cardinal limit,			/* in bytes */
			  ilu_cardinal expected_encoding,	/* IANA charset MIB */
			  ilu_cardinal *actual_encoding,	/* IANA charset MIB */
			  ILU_ERRS((IoErrs)) * err);
  /* If "expected_encoding" is 0, return actual encoding in
     "actual_encoding".  Otherwise, convert incoming string
     to "expected_encoding", and return that encoding in
     "actual_encoding".  A zero-octet should always be appended.
     The value assigned to "length" should be the number of bytes,
     not counting the zero-octet. */
  ilu_cardinal(*pr_size_of_string) (ilu_Call call,
				    void * s,				/* pointer to data */
				    ilu_cardinal length,		/* bytes of data */
				    ilu_cardinal limit,			/* specified max */
				    ilu_cardinal current_encoding,	/* IANA charset MIB */
				    ilu_cardinal expected_encoding,	/* IANA charset MIB */
				    ILU_ERRS((IoErrs)) * err);
  /* If expected_encoding is 0, marshal actual encoding
     onto wire along with encoding indicator; otherwise,
     convert to expected_encoding, and marshal that onto
     wire.  Or do something elese that may be protocol-dependent... */

  /*
   * a stringvec is a vector of shortcharacter -- not
   * null-terminated
   */
  void(*pr_output_stringvec) (ilu_Call call, ilu_string s,
				     ilu_cardinal length,
				     ILU_ERRS((IoErrs)) * err);
  void(*pr_input_stringvec) (ilu_Call call, ilu_string * s,
				    ilu_cardinal length,
				    ILU_ERRS((IoErrs)) * err);
  ilu_cardinal(*pr_size_of_stringvec) (ilu_Call call, ilu_string s,
				       ilu_cardinal length,
				       ILU_ERRS((IoErrs)) * err);

  /* a wstring is a sequence of character */
  void(*pr_output_wstring) (ilu_Call call, ilu_wstring s,
			    ilu_cardinal length,
			    ilu_cardinal limit,
			    ILU_ERRS((IoErrs)) * err);
  void(*pr_input_wstring) (ilu_Call call, ilu_wstring * s,
			   ilu_cardinal * length,
			   ilu_cardinal limit,
			   ILU_ERRS((IoErrs)) * err);
  ilu_cardinal(*pr_size_of_wstring) (ilu_Call call, ilu_wstring s,
				     ilu_cardinal length,
				     ilu_cardinal limit,
				     ILU_ERRS((IoErrs)) * err);

  /*
   * a wstringvec is a vector of character -- not null-terminated
   */
  void(*pr_output_wstringvec) (ilu_Call call, ilu_wstring s,
			       ilu_cardinal length,
			       ILU_ERRS((IoErrs)) * err);
  void(*pr_input_wstringvec) (ilu_Call call, ilu_wstring * s,
			      ilu_cardinal length,
			      ILU_ERRS((IoErrs)) * err);
  ilu_cardinal(*pr_size_of_wstringvec) (ilu_Call call, ilu_wstring s,
					ilu_cardinal length,
					ILU_ERRS((IoErrs)) * err);

  /* opaque is a vector of bytes */
  void(*pr_output_opaque) (ilu_Call call, ilu_opaque s,
				  ilu_cardinal length,
				  ILU_ERRS((IoErrs)) * err);
  void(*pr_input_opaque) (ilu_Call call, ilu_opaque * s,
				 ilu_cardinal length,
				 ILU_ERRS((IoErrs)) * err);
  ilu_cardinal(*pr_size_of_opaque) (ilu_Call call, ilu_opaque s,
				    ilu_cardinal length,
				    ILU_ERRS((IoErrs)) * err);

  /* bytes is a sequence of bytes */
  void(*pr_output_bytes) (ilu_Call call, ilu_bytes s,
				 ilu_cardinal length,
				 ilu_cardinal limit,
				 ILU_ERRS((IoErrs)) * err);
  void(*pr_input_bytes) (ilu_Call call, ilu_bytes * s,
				ilu_cardinal * length,
				ilu_cardinal limit,
				ILU_ERRS((IoErrs)) * err);
  ilu_cardinal(*pr_size_of_bytes) (ilu_Call call, ilu_bytes s,
				   ilu_cardinal length,
				   ilu_cardinal limit,
				   ILU_ERRS((IoErrs)) * err);

/*************************************************************************/
/************************  Simple ILU Objects  ***************************/
/*************************************************************************/

  /* object references vary from protocol to protocol */
  
/**Call-OHi(call).
   kobj == NIL => Main Invariant holds.
   kobj != NIL => all the following:
   before: Inside(s, cl);
   after:				  L1 disjoint {cmu, s};
   after: cl collectible	       => L1  not >=  {gcmu};
   after: cl collectible & s surrogate => Main Invariant holds;
   where s = kobj's server and cl = kobj's type.
   (We don't really need to hold cmu for surrogate or non-collectible
    objects, but this is convenient because ilu_Enter/ExitServer can
    be used.)*/
  ilu_boolean(*pr_output_object_id) (ilu_Call call, ilu_Object kobj,
				     ilu_boolean discriminator_p,
				     ilu_Class static_type,
				     ILU_ERRS((IoErrs)) * err);

/**before: L1 = {};
   after:  *kobj!=NIL => Inside(*kobj's server, static_type);
   after:  *kobj==NIL => L1 = {};
   after:  ILU_ERRNOK(*err) => *kobj==NIL;
   Main Remnant holds, Call-IHi(call)*/
  ilu_boolean(*pr_input_object_id) (ilu_Call call, ilu_Object * kobj,
				    ilu_boolean discriminator_p,
				    ilu_Class static_type,
				    ILU_ERRS((IoErrs)) * err);

/**kobj!=NIL => L1 >= {kobj's server};
   kobj==NIL => L1 unconstrained;
   L2 unconstrained*/
  ilu_cardinal(*pr_size_of_object_id) (ilu_Call call, ilu_Object kobj,
				       ilu_boolean discriminator_p,
				       ilu_Class static_type,
				       ILU_ERRS((IoErrs)) * err);

/*************************************************************************/
/**************************  Objects by Value  ***************************/
/*************************************************************************/

#ifdef ILU_HTTPNG_OBJECTS

/*L2 >= {call's conn's callmu, iomu}.
  obj == NIL => Main Invariant holds.
  obj != NIL => Inside(s, cl);
*/

  ilu_boolean(*pr_begin_output_object) (ilu_Call,	/* call */
					ilu_Object,	/* kobj */
					ilu_boolean,	/* discriminator_p */
					ilu_Class,	/* static_type */
					ilu_cardinal,	/* nstates */
					ILU_ERRS((IoErrs)) *);

/*L2 >= {call's conn's callmu, iomu}.
  Inside(s, cl)
  where s = obj's server and cl = obj's type. */
  ilu_boolean(*pr_begin_output_state) (ilu_Call,	/* call */
				       ilu_Object,	/* obj */
				       ilu_Class,	/* class of the state */
				       ILU_ERRS((IoErrs)) *);

/*L2 >= {call's conn's callmu, iomu}.
  Inside(s, cl)
  where s = obj's server and cl = obj's type. */
  ilu_boolean (*pr_finish_output_state) (ilu_Call,	/* call */
					 ilu_Object,	/* obj */
					 ilu_Class,	/* class of the state */
					 ILU_ERRS((IoErrs)) *);

/**L2 >= {call's conn's callmu, iomu}.
  obj == NIL => Main Invariant holds.
  obj != NIL => all the following:
  before: Inside(s, cl);
  after:				 L1 disjoint {cmu, s};
  after: cl collectible		      => L1  not >=  {gcmu};
  after: cl collectible & s surrogate => Main Invariant holds;
  where s = obj's server and cl = obj's type.
  (We don't really need to hold cmu for surrogate or non-collectible
   objects, but this is convenient because ilu_Enter/ExitServer can
   be used.)*/
  ilu_boolean (*pr_finish_output_object) (ilu_Call,		/* call */
					  ilu_Object,		/* kobj */
					  ilu_boolean,		/* discriminator_p */
					  ilu_Class,		/* static_type */
					  ILU_ERRS((IoErrs)) *);


/**Main Remnant holds.
   obj!=NIL => L1 = {obj's server};
   obj==NIL => L1 = {}*/

  ilu_cardinal (*pr_begin_size_of_object) (ilu_Call,	/* call */
					  ilu_Object,	/* kobj */
					  ilu_boolean,	/* discriminant_p */
					  ilu_Class,	/* static_type */
					  ilu_cardinal,	/* nstates */
					  ILU_ERRS((IoErrs)) *);
  
  ilu_cardinal (*pr_begin_size_of_state) (ilu_Call,	/* call */
					 ilu_Object,	/* kobj */
					 ilu_Class,	/* state class */
					 ILU_ERRS((IoErrs)) *);

  ilu_cardinal (*pr_finish_size_of_state) (ilu_Call,	/* call */
					  ilu_Object,	/* kobj */
					  ilu_Class,	/* state class */
					  ILU_ERRS((IoErrs)) *);

  ilu_cardinal (*pr_finish_size_of_object) (ilu_Call,	/* call */
					   ilu_Object,	/* kobj */
					   ilu_boolean,	/* discriminant_p */
					   ilu_Class,	/* static_type */
					   ILU_ERRS((IoErrs)) *);

/**Main Remnant holds, Call-IHi(call);
   before: L1 = {}
*/
  ilu_Class (*pr_begin_input_object) (ilu_Call,		/* call */
				      ilu_boolean,	/* discriminator_p */
				      ilu_Class,	/* static_type */
				      ilu_cardinal *,	/* nstates */
				      ILU_ERRS((IoErrs)) *);
  /* Begins process of unmarshalling object ref.  Returns actual type of object. */

  ilu_string	/* type UID of state */
    (*pr_begin_input_state) (ilu_Call,		/* call */
			     ILU_ERRS((IoErrs)) *);
  /*Begins process of unmarshalling state of some object type. Called
    "nstate" times, where "nstate" is returned from ilu_BeginInputObject.
    Followed by either calls to input state attributes and final call
    to ilu_FinishInputState, or by a call to ilu_SkipInputState.
    */

  ilu_boolean
    (*pr_skip_input_state) (ilu_Call,
			    ILU_ERRS((IoErrs)) *);
  /* Used to skip over the rest of the current state packet.  The contents
     of the packet are lost. */

  ilu_boolean
    (*pr_finish_input_state) (ilu_Call,		/* call */
			      ILU_ERRS((IoErrs)) *);
/* Finishes process of unmarshalling state for some object type "state_class".
   Always followed by call to "ilu_BeginInputState". */

/* Main Remnant holds, Call-IHi(call);
   after:  result!=NIL => Inside(result's server, static_type);
   after:  result==NIL => L1 = {};
   after:  ILU_ERRNOK(*err) => result==NIL*/
  ilu_Object	/* OPTIONAL */
    (*pr_finish_input_object) (ilu_Call,	/* call */
			       ilu_boolean,	/* discriminator_p */
			       ilu_Class,	/* static_type */
			       ILU_ERRS((IoErrs)) *);
/* Afterward, if *o!=NIL && ilu_GetLanguageSpecificObject(*o)==NIL,
   the caller will invoke ilu_RegisterLanguageSpecificObject
   on *o before unlocking the server. */

#endif /* def ILU_HTTPNG_OBJECTS */

/*************************************************************************/
/*****************************  Sequence *********************************/
/*************************************************************************/

  void            (*pr_output_sequence) (ilu_Call call,
					 ilu_cardinal length,
					 ilu_cardinal limit,
					 ILU_OPTIONAL(ilu_Type),
			                 ILU_ERRS((IoErrs)) * err);
  void            (*pr_output_sequence_mark) (ilu_Call call,
					      ilu_cardinal extent,
					      ILU_ERRS((IoErrs)) * err);
  /* ...called every 2^16-1 items */
  void            (*pr_input_sequence) (ilu_Call call,
					ilu_cardinal * length,
					ilu_cardinal limit,
					ILU_OPTIONAL(ilu_Type),
					ILU_ERRS((IoErrs)) * err);
  void            (*pr_input_sequence_mark) (ilu_Call call,
					     ilu_cardinal extent,
					     ILU_ERRS((IoErrs)) * err);
  /* ...called every 2^16-1 items */
  void            (*pr_end_sequence) (ilu_Call call,
				      ILU_ERRS((IoErrs)) * err);
  ilu_cardinal(*pr_size_of_sequence) (ilu_Call call, ilu_cardinal length,
				      ilu_cardinal limit,
				      ILU_OPTIONAL(ilu_Type),
				      ILU_ERRS((IoErrs)) * err);

/*************************************************************************/
/*****************************  Array  ***********************************/
/*************************************************************************/

  void            (*pr_output_array) (ilu_Call call, ilu_cardinal length,
				      ILU_OPTIONAL(ilu_Type),
				      ILU_ERRS((IoErrs)) * err);
  void            (*pr_input_array) (ilu_Call call,
				     ILU_OPTIONAL(ilu_Type),
				     ILU_ERRS((IoErrs)) * err);
  void            (*pr_end_array) (ilu_Call call,
				   ILU_ERRS((IoErrs)) * err);
  ilu_cardinal(*pr_size_of_array) (ilu_Call call, ilu_cardinal length,
				   ILU_OPTIONAL(ilu_Type),
				   ILU_ERRS((IoErrs)) * err);

/*************************************************************************/
/*****************************  Record   *********************************/
/*************************************************************************/

  void            (*pr_output_record) (ilu_Call call,
				       ILU_OPTIONAL(ilu_Type),
				       ILU_ERRS((IoErrs)) * err);
  void            (*pr_input_record) (ilu_Call call,
				      ILU_OPTIONAL(ilu_Type),
				      ILU_ERRS((IoErrs)) * err);
  void            (*pr_end_record) (ilu_Call call,
				    ILU_ERRS((IoErrs)) * err);
  ilu_cardinal(*pr_size_of_record) (ilu_Call call,
				    ILU_OPTIONAL(ilu_Type),
				    ILU_ERRS((IoErrs)) * err);

/*************************************************************************/
/*****************************   Union   *********************************/
/*************************************************************************/

  void            (*pr_output_union) (ilu_Call call,
				      ilu_cardinal discrim,
				      ilu_TypeKind dtype,
				      ILU_OPTIONAL(ilu_Type),
				      ILU_ERRS((IoErrs)) * err);
  void            (*pr_input_union) (ilu_Call call,
		                     ilu_cardinal * typeIndex,
				     ilu_TypeKind dtype,
				     ILU_OPTIONAL(ilu_Type),
				     ILU_ERRS((IoErrs)) * err);
  void            (*pr_end_union) (ilu_Call call,
				   ILU_ERRS((IoErrs)) * err);
  ilu_cardinal(*pr_size_of_union) (ilu_Call call,
				   ilu_cardinal typeIndex,
				   ilu_TypeKind dtype,
				   ILU_OPTIONAL(ilu_Type),
				   ILU_ERRS((IoErrs)) * err);

/*************************************************************************/
/*****************************  Pickle   *********************************/
/*************************************************************************/

#ifdef ADD_VARIANT_SUPPORT

  void            (*pr_output_pickle)(ilu_Call,
				      ilu_Pickle,
				      ILU_OPTIONAL(ilu_Type),
				      ILU_ERRS((IoErrs)) *);
  ilu_boolean        (*pr_input_pickle) (ilu_Call,
					 ilu_Pickle *,
					 ILU_OPTIONAL(ilu_Type),
					 ILU_ERRS((IoErrs)) *);
  ilu_cardinal   (*pr_size_of_pickle)(ilu_Call,
				      ilu_Pickle,
				      ILU_OPTIONAL(ilu_Type),
				      ILU_ERRS((IoErrs)) *);

#endif /* ADD_VARIANT_SUPPORT */

/*************************************************************************/
/*****************************  Fixed-point   ****************************/
/*************************************************************************/

#ifdef ILU_FIXED_POINT_SUPPORT

  void            (*pr_output_fixedpoint)(ilu_Call,
					  ILU_OPTIONAL(ilu_Bignum), /* numerator -- NIL indicates NaN */
					  ILU_OPTIONAL(ilu_Bignum), /* min numerator val */
					  ILU_OPTIONAL(ilu_Bignum), /* max numerator val */
					  ILU_OPTIONAL(ilu_Bignum), /* denominator -- NIL indicates Infinity */
					  ilu_cardinal,	/* digits if a CORBA 'fixed' type */
					  ilu_cardinal, /* decimal places if a CORBA 'fixed' type */
					  ilu_FixedPointRangeSize,
					  ILU_ERRS((IoErrs)) *);
  ilu_boolean        (*pr_input_fixedpoint) (ilu_Call,
					     ilu_Bignum *,	/* numerator -- NIL indicates NaN */
					     ILU_OPTIONAL(ilu_Bignum), /* min numerator val */
					     ILU_OPTIONAL(ilu_Bignum), /* max numerator val */
					     ilu_Bignum,	/* denominator -- NIL indicates Infinity */
					     ilu_cardinal,	/* digits if a CORBA 'fixed' type */
					     ilu_cardinal,	/* decimal places if a CORBA 'fixed' type */
					     ilu_FixedPointRangeSize,
					     ILU_ERRS((IoErrs)) *);
  ilu_cardinal   (*pr_size_of_fixedpoint)(ilu_Call,
					  ILU_OPTIONAL(ilu_Bignum), /* numerator -- NIL indicates NaN */
					  ILU_OPTIONAL(ilu_Bignum), /* min numerator val */
					  ILU_OPTIONAL(ilu_Bignum), /* max numerator val */
					  ILU_OPTIONAL(ilu_Bignum), /* denominator -- NIL indicates Infinity */
					  ilu_cardinal,	/* digits if a CORBA 'fixed' type */
					  ilu_cardinal, /* decimal places if a CORBA 'fixed' type */
					  ilu_FixedPointRangeSize,
					  ILU_ERRS((IoErrs)) *);

#endif /* def ILU_FIXED_POINT_SUPPORT */

#ifdef ILU_REFERENCE_TYPES
/*************************************************************************/
/*****************************  Reference types   ************************/
/*************************************************************************/

  ilu_cardinal
    (*pr_size_of_reference) (ilu_Call,
			     ilu_boolean,	/* value provided? */
			     ilu_boolean *first,/* NIL for non-aliased types */
			     ilu_ReferenceID,	/* LSR value identifier */
			     ILU_ERRS((IoErrs)) *);

  void
    (*pr_output_reference) (ilu_Call,
			    ilu_boolean,	/* value provided? */
			    ilu_boolean *first,/* NIL for non-aliased types */
			    ilu_ReferenceID,	/* LSR value identifier */
			    ILU_ERRS((IoErrs)) *);

  ilu_cardinal					/* temporary ref ID, zero if real RefID is returned */
    (*pr_input_reference) (ilu_Call,
			   ilu_boolean *present,/* False if no value */
			   ilu_ReferenceID *,	/* 0 to indicate new value,
						   non-zero to indicate previously passed value */
			   ILU_ERRS((IoErrs)) *);

  void
    (*pr_end_input_reference) (ilu_Call,
			       ilu_cardinal, /* temp ID returned from pr_input_reference */
			       ilu_ReferenceID,	/* actual RefID */
			       ilu_Error *);

#endif /* def ILU_REFERENCE_TYPES */

};


/* L1, L2 unconstrained */
ILU_PUBLIC
ILU_ERRS((ProtocolAlreadyRegistered, MaxCountExceeded))
ilu_RegisterProtocol(char *pname,
		     ilu_Protocol(*instantiator) (ilu_ProtocolInfo, ilu_Error *),
		     ilu_boolean override_existing_registration);



/* Macros for accessing slots in a ilu_Protocol metaobject */

#define protocol_type(proto)		((proto)->pr_type)
#define protocol_concurrent(proto)	((proto)->pr_concurrent_requests)
#define protocol_needs_sizing(proto)	((proto)->pr_sizing_required)

#define protocol_free_data_block(proto,block)	((*((proto)->pr_free_data_block))(block))
#define protocol_create_data_block(proto,pinfo,err)		((*((proto)->pr_create_data_block))(pinfo,err))
#define protocol_form_handle(proto,pidata,object)	((*((proto)->pr_form_handle))((pidata),(object)))
#define protocol_close_connection(proto,pidata,r,err)	((*((proto)->pr_conn_closing))((pidata),(r),(err)))

#define protocol_read_header(proto,call,ptype,psn,eofreason,eofsn,se)	((*((proto)->pr_read_header))((call),(ptype),(psn),(eofreason),(eofsn),(se)))
#define protocol_delay_interp(proto,call,err) ((*((proto)->pr_delay_interp))((call),(err)))
#define protocol_resume_interp(proto,call,x) ((*((proto)->pr_resume_interp))((call),(x)))
#define protocol_abandon_delayed_interp(proto,conn,x,err) ((*((proto)->pr_abandon_delayed_interp))(conn,x,err))
#define protocol_discard_input(proto,call,err) ((*((proto)->pr_discard_input))((call),(err)))
#define protocol_discard_output(proto,call,err) ((*((proto)->pr_discard_output))((call),(err)))

#define protocol_interpret_request(proto,call,err) ((*((proto)->pr_interpret_request))((call),(err)))
#define protocol_interpret_reply(proto,call,estatus,err) ((*((proto)->pr_interpret_reply))((call),(estatus),(err)))

#define protocol_start_request(proto,call,argSize,err) ((*((proto)->pr_start_request))((call),(argSize),(err)))
#define protocol_begin_sizing_reply(proto,call,exns,err) ((*((proto)->pr_begin_sizing_reply))((call),(exns),(err)))
#define protocol_begin_reply(proto,call,exns,argSize,err) ((*((proto)->pr_begin_reply))((call),(exns),(argSize),(err)))
#define protocol_begin_sizing_exn(proto,call,exceptionCode,sysExnIdx,err) ((*((proto)->pr_begin_sizing_exn))((call),(exceptionCode),(sysExnIdx),(err)))
#define protocol_begin_exception(proto,call,exceptionCode,sysExnIdx,argSize,err) ((*((proto)->pr_begin_exception))((call),(exceptionCode),(sysExnIdx),(argSize),(err)))
#define protocol_finish_exception(proto,call,push,err) ((*((proto)->pr_finish_exception))((call),(push),(err)))
#define protocol_finish_request(proto,call,msgh,push,err) ((*((proto)->pr_finish_request))((call),(msgh),(push),(err)))
#define protocol_finish_reply(proto,call,push,err) ((*((proto)->pr_finish_reply))((call),(push),(err)))
#define protocol_request_read(proto,call,err) ((*((proto)->pr_request_read))((call),(err)))
#define protocol_reply_read(proto,call,err) ((*((proto)->pr_reply_read))((call),(err)))

#define protocol_skip_bytes(proto,call,numBytes) ((*((proto)->pr_skip_bytes))((call),(numBytes)))

#define protocol_output_integer(proto,call,i,e) ((*((proto)->pr_output_integer))((call),(i),(e)))
#define protocol_input_integer(proto,call,i,e) ((*((proto)->pr_input_integer))((call),(i),(e)))
#define protocol_size_of_integer(proto,call,i,err) ((*((proto)->pr_size_of_integer))((call),(i),(err)))

#define protocol_output_cardinal(proto,call,i,e) ((*((proto)->pr_output_cardinal))((call),(i),(e)))
#define protocol_input_cardinal(proto,call,i,e) ((*((proto)->pr_input_cardinal))((call),(i),(e)))
#define protocol_size_of_cardinal(proto,call,i,err) ((*((proto)->pr_size_of_cardinal))((call),(i),(err)))

#define protocol_output_short_integer(proto,call,i,e) ((*((proto)->pr_output_shortinteger))((call),(i),(e)))
#define protocol_input_short_integer(proto,call,i,e) ((*((proto)->pr_input_shortinteger))((call),(i),(e)))
#define protocol_size_of_short_integer(proto,call,i,err) ((*((proto)->pr_size_of_shortinteger))((call),(i),(err)))

#define protocol_output_short_cardinal(proto,call,i,e) ((*((proto)->pr_output_shortcardinal))((call),(i),(e)))
#define protocol_input_short_cardinal(proto,call,i,e) ((*((proto)->pr_input_shortcardinal))((call),(i),(e)))
#define protocol_size_of_short_cardinal(proto,call,i,err) ((*((proto)->pr_size_of_shortcardinal))((call),(i),(err)))

#define protocol_output_long_integer(proto,call,i,e) ((*((proto)->pr_output_longinteger))((call),(i),(e)))
#define protocol_input_long_integer(proto,call,i,e) ((*((proto)->pr_input_longinteger))((call),(i),(e)))
#define protocol_size_of_long_integer(proto,call,i,err) ((*((proto)->pr_size_of_longinteger))((call),(i),(err)))

#define protocol_output_long_cardinal(proto,call,i,e) ((*((proto)->pr_output_longcardinal))((call),(i),(e)))
#define protocol_input_long_cardinal(proto,call,i,e) ((*((proto)->pr_input_longcardinal))((call),(i),(e)))
#define protocol_size_of_long_cardinal(proto,call,i,err) ((*((proto)->pr_size_of_longcardinal))((call),(i),(err)))

#define protocol_output_long_real(proto,call,i,e) ((*((proto)->pr_output_longreal))((call),(i),(e)))
#define protocol_input_long_real(proto,call,i,e) ((*((proto)->pr_input_longreal))((call),(i),(e)))
#define protocol_size_of_long_real(proto,call,i,err) ((*((proto)->pr_size_of_longreal))((call),(i),(err)))

#define protocol_output_real(proto,call,i,e) ((*((proto)->pr_output_real))((call),(i),(e)))
#define protocol_input_real(proto,call,i,e) ((*((proto)->pr_input_real))((call),(i),(e)))
#define protocol_size_of_real(proto,call,i,err) ((*((proto)->pr_size_of_real))((call),(i),(err)))

#define protocol_output_short_real(proto,call,i,e) ((*((proto)->pr_output_shortreal))((call),(i),(e)))
#define protocol_input_short_real(proto,call,i,e) ((*((proto)->pr_input_shortreal))((call),(i),(e)))
#define protocol_size_of_short_real(proto,call,i,err) ((*((proto)->pr_size_of_shortreal))((call),(i),(err)))

#define protocol_output_character(proto,call,i,e) ((*((proto)->pr_output_character))((call),(i),(e)))
#define protocol_input_character(proto,call,i,e) ((*((proto)->pr_input_character))((call),(i),(e)))
#define protocol_size_of_character(proto,call,i,err) ((*((proto)->pr_size_of_character))((call),(i),(err)))

#define protocol_output_shortchar(proto,call,i,e) ((*((proto)->pr_output_shortchar))((call),(i),(e)))
#define protocol_input_shortchar(proto,call,i,e) ((*((proto)->pr_input_shortchar))((call),(i),(e)))
#define protocol_size_of_shortchar(proto,call,i,err) ((*((proto)->pr_size_of_shortchar))((call),(i),(err)))

#define protocol_output_byte(proto,call,i,e) ((*((proto)->pr_output_byte))((call),(i),(e)))
#define protocol_input_byte(proto,call,i,e) ((*((proto)->pr_input_byte))((call),(i),(e)))
#define protocol_size_of_byte(proto,call,i,err) ((*((proto)->pr_size_of_byte))((call),(i),(err)))

#define protocol_output_boolean(proto,call,i,e) ((*((proto)->pr_output_boolean))((call),(i),(e)))
#define protocol_input_boolean(proto,call,i,e) ((*((proto)->pr_input_boolean))((call),(i),(e)))
#define protocol_size_of_boolean(proto,call,i,err) ((*((proto)->pr_size_of_boolean))((call),(i),(err)))

#define protocol_output_string(proto,call,s,len,limit,exp,act,e) ((*((proto)->pr_output_string))((call),(s),(len),(limit),(exp),(act),(e)))
#define protocol_input_string(proto,call,s,len,limit,exp,act,e) ((*((proto)->pr_input_string))((call),(s),(len),(limit),(exp),(act),(e)))
#define protocol_size_of_string(proto,call,i,len,limit,exp,act,err) ((*((proto)->pr_size_of_string))((call),(i),(len),(limit),(exp),(act),(err)))

#define protocol_output_stringvec(proto,call,i,len,e) ((*((proto)->pr_output_stringvec))((call),(i),(len),(e)))
#define protocol_input_stringvec(proto,call,i,len,e) ((*((proto)->pr_input_stringvec))((call),(i),(len),(e)))
#define protocol_size_of_stringvec(proto,call,i,len,err) ((*((proto)->pr_size_of_stringvec))((call),(i),(len),(err)))

#define protocol_output_wstring(proto,call,s,len,limit,e) ((*((proto)->pr_output_wstring))((call),(s),(len),(limit),(e)))
#define protocol_input_wstring(proto,call,s,len,limit,e) ((*((proto)->pr_input_wstring))((call),(s),(len),(limit),(e)))
#define protocol_size_of_wstring(proto,call,i,len,limit,err) ((*((proto)->pr_size_of_wstring))((call),(i),(len),(limit),(err)))

#define protocol_output_wstringvec(proto,call,i,len,e) ((*((proto)->pr_output_wstringvec))((call),(i),(len),(e)))
#define protocol_input_wstringvec(proto,call,i,len,e) ((*((proto)->pr_input_wstringvec))((call),(i),(len),(e)))
#define protocol_size_of_wstringvec(proto,call,i,len,err) ((*((proto)->pr_size_of_wstringvec))((call),(i),(len),(err)))

#define protocol_output_bytes(proto,call,i,len,limit,e) ((*((proto)->pr_output_bytes))((call),(i),(len),(limit),(e)))
#define protocol_input_bytes(proto,call,i,len,limit,e) ((*((proto)->pr_input_bytes))((call),(i),(len),(limit),(e)))
#define protocol_size_of_bytes(proto,call,i,len,limit,err) ((*((proto)->pr_size_of_bytes))((call),(i),(len),(limit),(err)))

#define protocol_output_opaque(proto,call,i,len,e) ((*((proto)->pr_output_opaque))((call),(i),(len),(e)))
#define protocol_input_opaque(proto,call,i,len,e) ((*((proto)->pr_input_opaque))((call),(i),(len),(e)))
#define protocol_size_of_opaque(proto,call,i,len,err) ((*((proto)->pr_size_of_opaque))((call),(i),(len),(err)))

#define protocol_output_object_id(proto,call,kernel_obj,disc_p,static_type,err) ((*((proto)->pr_output_object_id))((call),(kernel_obj),(disc_p),(static_type),(err)))
#define protocol_input_object_id(proto,call,kernel_obj,disc_p,static_type,err) ((*((proto)->pr_input_object_id))((call),(kernel_obj),(disc_p),(static_type),(err)))
#define protocol_size_of_object_id(proto,call,kernel_obj,disc_p,static_type,err) ((*((proto)->pr_size_of_object_id))((call),(kernel_obj),(disc_p),(static_type),(err)))

#define protocol_output_pipe(proto,call,i) ((*((proto)->pr_output_pipe))((call),(i)))
#define protocol_input_pipe(proto,call,i) ((*((proto)->pr_input_pipe))((call),(i)))
#define protocol_size_of_pipe(proto,call,i) ((*((proto)->pr_size_of_pipe))((call),(i)))

#define protocol_output_sequence(proto,call,i,limit,p,e) ((*((proto)->pr_output_sequence))((call),(i),(limit),(p),(e)))
#define protocol_output_sequence_mark(proto,call,extent,err)	((*((proto)->pr_output_sequence_mark))((call),(extent),(err)))
#define protocol_input_sequence(proto,call,i,limit,p,e) ((*((proto)->pr_input_sequence))((call),(i),(limit),(p),(e)))
#define protocol_input_sequence_mark(proto,call,extent,err)	((*((proto)->pr_input_sequence_mark))((call),(extent),(err)))
#define protocol_end_sequence(proto,call,err) ((*((proto)->pr_end_sequence))((call),(err)))
#define protocol_size_of_sequence(proto,call,i,limit,p,err) ((*((proto)->pr_size_of_sequence))((call),(i),(limit),(p),(err)))

#define protocol_output_enum_code(proto,call,i,p,e) ((*((proto)->pr_output_enum_code))((call),(i),(p),(e)))
#define protocol_input_enum_code(proto,call,i,p,e) ((*((proto)->pr_input_enum_code))((call),(i),(p),(e)))
#define protocol_size_of_enum_code(proto,call,i,p,err) ((*((proto)->pr_size_of_enum_code))((call),(i),(p),(err)))

#define protocol_output_optional(proto,call,i,p,e) ((*((proto)->pr_output_optional))((call),(i),(p),(e)))
#define protocol_input_optional(proto,call,i,p,e) ((*((proto)->pr_input_optional))((call),(i),(p),(e)))
#define protocol_size_of_optional(proto,call,i,p,err) ((*((proto)->pr_size_of_optional))((call),(i),(p),(err)))

#ifdef ILU_REFERENCE_TYPES
#define protocol_output_reference(proto,call,o,f,id,e) ((*((proto)->pr_output_reference))((call),(o),(f),(id),(e)))
#define protocol_size_of_reference(proto,call,o,f,id,e) ((*((proto)->pr_size_of_reference))((call),(o),(f),(id),(e)))
#define protocol_input_reference(proto,call,o,id,e) ((*((proto)->pr_input_reference))((call),(o),(id),(e)))
#define protocol_end_input_reference(proto,call,wire_id,id,err) ((*((proto)->pr_end_input_reference))((call),(wire_id),(id),(err)))
#endif

#define protocol_output_union(proto,call,i,dsize,p,e) ((*((proto)->pr_output_union))((call),(i),(dsize),(p),(e)))
#define protocol_input_union(proto,call,i,dsize,p,e) ((*((proto)->pr_input_union))((call),(i),(dsize),(p),(e)))
#define protocol_end_union(proto,call,err) ((*((proto)->pr_end_union))((call),(err)))
#define protocol_size_of_union(proto,call,i,dsize,p,err) ((*((proto)->pr_size_of_union))((call),(i),(dsize),(p),(err)))

#define protocol_output_array(proto,call,len,p,e) ((*((proto)->pr_output_array))((call),(len),(p),(e)))
#define protocol_input_array(proto,call,p,e) ((*((proto)->pr_input_array))((call),(p),(e)))
#define protocol_end_array(proto,call,err) ((*((proto)->pr_end_array))((call),(err)))
#define protocol_size_of_array(proto,call,len,p,err) ((*((proto)->pr_size_of_array))((call),(len),(p),(err)))

#define protocol_output_record(proto,call,p,e) ((*((proto)->pr_output_record))((call),(p),(e)))
#define protocol_input_record(proto,call,p,e) ((*((proto)->pr_input_record))((call),(p),(e)))
#define protocol_end_record(proto,call,err) ((*((proto)->pr_end_record))((call),(err)))
#define protocol_size_of_record(proto,call,p,err) ((*((proto)->pr_size_of_record))((call),(p),(err)))

#ifdef ILU_FIXED_POINT_SUPPORT

#define protocol_output_fixedpoint(proto,call,n,mn,mx,d,fd,fp,r,e) ((*((proto)->pr_output_fixedpoint))((call),(n),(mn),(mx),(d),(fd),(fp),(r),(e)))
#define protocol_input_fixedpoint(proto,call,n,mn,mx,d,fd,fp,r,e) ((*((proto)->pr_input_fixedpoint))((call),(n),(mn),(mx),(d),(fd),(fp),(r),(e)))
#define protocol_size_of_fixedpoint(proto,call,n,mn,mx,d,fd,fp,r,e) ((*((proto)->pr_size_of_fixedpoint))((call),(n),(mn),(mx),(d),(fd),(fp),(r),(e)))

#endif /* def ILU_FIXED_POINT_SUPPORT */

#ifdef ILU_HTTPNG_OBJECTS

#define protocol_begin_output_object(proto,call,kernel_obj,disc_p,static_type,nstates,err) ((*((proto)->pr_begin_output_object))((call),(kernel_obj),(disc_p),(static_type),(nstates),(err)))
#define protocol_begin_output_state(proto,call,kernel_obj,state_type,err) ((*((proto)->pr_begin_output_state))((call),(kernel_obj),(state_type),(err)))
#define protocol_finish_output_state(proto,call,kernel_obj,state_type,err) ((*((proto)->pr_finish_output_state))((call),(kernel_obj),(state_type),(err)))
#define protocol_finish_output_object(proto,call,kernel_obj,disc_p,static_type,err) ((*((proto)->pr_finish_output_object))((call),(kernel_obj),(disc_p),(static_type),(err)))

#define protocol_begin_size_of_object(proto,call,kernel_obj,disc_p,static_type,nstates,err) ((*((proto)->pr_begin_size_of_object))((call),(kernel_obj),(disc_p),(static_type),(nstates),(err)))
#define protocol_begin_size_of_state(proto,call,kernel_obj,state_type,err) ((*((proto)->pr_begin_size_of_state))((call),(kernel_obj),(state_type),(err)))
#define protocol_finish_size_of_state(proto,call,kernel_obj,state_type,err) ((*((proto)->pr_finish_size_of_state))((call),(kernel_obj),(state_type),(err)))
#define protocol_finish_size_of_object(proto,call,kernel_obj,disc_p,static_type,err) ((*((proto)->pr_finish_size_of_object))((call),(kernel_obj),(disc_p),(static_type),(err)))

#define protocol_begin_input_object(proto,call,disc_p,static_type,nstates,err) ((*((proto)->pr_begin_input_object))((call),(disc_p),(static_type),(nstates),(err)))
#define protocol_begin_input_state(proto,call,err) ((*((proto)->pr_begin_input_state))((call),(err)))
#define protocol_skip_input_state(proto,call,err) ((*((proto)->pr_skip_input_state))((call),(err)))
#define protocol_finish_input_state(proto,call,err) ((*((proto)->pr_finish_input_state))((call),(err)))
#define protocol_finish_input_object(proto,call,disc_p,static_type,err) ((*((proto)->pr_finish_input_object))((call),(disc_p),(static_type),(err)))

#endif /* def ILU_HTTPNG_OBJECTS */

#ifdef ADD_VARIANT_SUPPORT

#define protocol_output_pickle(proto,call,t,p,e) ((*((proto)->pr_output_pickle))((call),(t),(p),(e)))
#define protocol_input_pickle(proto,call,t,p,e) ((*((proto)->pr_input_pickle))((call),(t),(p),(e)))
#define protocol_size_of_pickle(proto,call,t,p,e) ((*((proto)->pr_size_of_pickle))((call),(t),(p),(e)))

#endif /* ADD_VARIANT_SUPPORT */



/* protocol "instantiators" (see protocol.c) */

#ifdef SUNRPC_PROTOCOL
extern ilu_Protocol _ilu_sunrpc_Protocol(ilu_ProtocolInfo /* pinfo */, ilu_Error * /* err return */);
extern ilu_Protocol _ilu_bsunrpc_Protocol(ilu_ProtocolInfo /* pinfo */, ilu_Error * /* err return */);
extern ilu_Protocol _ilu_csunrpc_Protocol(ilu_ProtocolInfo /* pinfo */, ilu_Error * /* err return */);
extern ilu_Protocol _ilu_bcsunrpc_Protocol(ilu_ProtocolInfo /* pinfo */, ilu_Error * /* err return */);
#endif /* def SUNRPC_PROTOCOL */

#ifdef COURIER_PROTOCOL
extern ilu_Protocol _ilu_courier_Protocol(ilu_ProtocolInfo /* pinfo */, ilu_Error * /* err return */);
#endif /* def COURIER_PROTOCOL */

#ifdef IIOP_PROTOCOL
extern ilu_Protocol _ilu_IIOP_Protocol(ilu_ProtocolInfo /* pinfo */, ilu_Error * /* err return */);
extern ilu_Protocol _ilu_IIOP_SerialProtocol(ilu_ProtocolInfo /* pinfo */, ilu_Error * /* err return */);
#endif /* IIOP_PROTOCOL */

#ifdef W3NG_PROTOCOL
extern ilu_Protocol _ilu_w3ng_Protocol(ilu_ProtocolInfo /* pinfo */, ilu_Error * /* err return */);
#endif /* def W3NG_PROTOCOL */

#ifdef JAVARMI_PROTOCOL
extern ilu_Protocol _ilu_javarmi_Protocol(ilu_ProtocolInfo /* pinfo */, ilu_Error * /* err return */);
#endif /* def JAVARMI_PROTOCOL */

#ifdef HTTP_PROTOCOL
extern ilu_Protocol _ilu_http_Protocol(ilu_ProtocolInfo /* pinfo */, ilu_Error * /* err return */);
#endif


#endif /* _ILU_PROTOCOL_H */
