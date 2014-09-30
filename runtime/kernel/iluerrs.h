/** $Id: iluerrs.h,v 1.138 1999/09/20 19:15:18 janssen Exp $
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
/* Last edited by Mike Spreitzer October 23, 1998 2:05 pm PDT */

#ifndef _ILUERRS_H_
#define _ILUERRS_H_

#ifdef __cplusplus
extern "C" {
#endif

ILU_DECL_PARMLESS_ERR(success);
/* A non-error that reserves a code for successful completion. */


/*
 * First, the errors that can appear (suitably mapped, of course) in
 * the language mappings.  Each of these has a defined mapping into
 * a CORBA 1.2 standard system exception; that mapping is documented
 * in these declarations, except where the mapping is to the
 * exception of the same name. Some, which are noted as such, aren't
 * actually used --- but appear in this list so that a prefix of
 * this list corresponds in number and position to the published
 * list for CORBA.
 */

ILU_DECL_PARMLESS_ERR(unknown);	/* not used */

#define ILU_VMCID_BASE		0x494C0000
#define ILU_VMCID_EXTRA		0x00000800

#define ILU_M1NOR(n,c)		ilu_##n = ILU_VMCID_BASE,
#define ILU_MINOR(n,c)		ilu_##n,
#define ILU_MINOR_LAST(n)	ilu_##n##_nextMinor
#define ILU_MINOR_COMMENT(c)

#define ILU_BAD_PARAM_MINORS \
  ILU_M1NOR(bpm_duh,	"It should be pretty obvious")\
  ILU_MINOR(bpm_true,	"Attempting RPC on true server")\
  ILU_MINOR(bpm_asynch_unreliable,	"asynch method call on unreliable transport")\
  ILU_MINOR(bpm_late,	"called ilu_SetFoo too late")\
  ILU_MINOR(bpm_not_exported,	"asked to output object of server with no ports")\
  ILU_MINOR(bpm_tinfo,	"invalid transport info string")\
  ILU_MINOR(bpm_pinfo,	"invalid protocol info string")\
  ILU_MINOR(bpm_typeID,	"invalid type ID")\
  ILU_MINOR(bpm_OID,	"invalid object ID")\
  ILU_MINOR(bpm_SBH,	"bad SBH")\
  ILU_MINOR(bpm_URL,	"invalid URL")\
  ILU_MINOR(bpm_serverId,	"bad server ID string")\
  ILU_MINOR(bpm_networkAddr,	"bad network address or host name")\
  ILU_MINOR(bpm_connToDefault,	"default host appears where a definite one should")\
  ILU_MINOR(bpm_badPointer,	"attempt to address memory not in the process address space")\
  ILU_MINOR(bpm_fd,	"bad file descriptor")\
  ILU_MINOR(bpm_sequenceLimit,	"sequence too long for its limit")\
  ILU_MINOR(bpm_unionDiscSize,	"invalid discriminant size")\
  ILU_MINOR(bpm_unionDiscValue,	"discriminant value invalid for this union type")\
  ILU_MINOR(bpm_nil,	"NIL passed where not allowed")\
  ILU_MINOR(bpm_broken,	"not NIL, but contents look bad")\
  ILU_MINOR(bpm_closed,	"op invoked on closed something")\
  ILU_MINOR(bpm_small_buffer,	"callee needs larger buffer")\
  ILU_MINOR(bpm_string_null_char,	"octet 0 in `string' parm")\
  ILU_MINOR(bpm_threading,	"A proc appropriate only for single-threaded runtimes was called in a mult-threaded runtime --- or the other way around.")\
  ILU_MINOR(bpm_prefix_type_violation,	"type in pickle doesn't satisfy prefix type condition on pickle being marshalled or unmarshalled.")\
  ILU_MINOR_COMMENT("\
  \
  The following errors may be raised by the runtime type system\
  \
                    ")\
  ILU_MINOR(bpm_not_byte_type,	"expected byte type")\
  ILU_MINOR(bpm_not_boolean_type,	"expected boolean type")\
  ILU_MINOR(bpm_not_character_type,	"expected character type")\
  ILU_MINOR(bpm_not_shortcharacter_type,	"expected shortcharacter type")\
  ILU_MINOR(bpm_not_shortinteger_type,	"expected shortinteger type")\
  ILU_MINOR(bpm_not_integer_type,	"expected integer type")\
  ILU_MINOR(bpm_not_longinteger_type,	"expected longinteger type")\
  ILU_MINOR(bpm_not_shortcardinal_type,	"expected shortcardinal type")\
  ILU_MINOR(bpm_not_cardinal_type,	"expected cardinal type")\
  ILU_MINOR(bpm_not_longcardinal_type,	"expected longcardinal type")\
  ILU_MINOR(bpm_not_shortreal_type,	"expected shortreal type")\
  ILU_MINOR(bpm_not_real_type,	"expected real type")\
  ILU_MINOR(bpm_not_longreal_type,	"expected longreal type")\
  ILU_MINOR(bpm_not_object_type,	"expected object type")\
  ILU_MINOR(bpm_not_optional_type,	"expected optional type")\
  ILU_MINOR(bpm_not_alias_type,	"expected alias type")\
  ILU_MINOR(bpm_not_union_type,	"expected union type")\
  ILU_MINOR(bpm_invalid_union_arm_index,	"out of bounds")\
  ILU_MINOR(bpm_not_sequence_type,	"expected sequence type")\
  ILU_MINOR(bpm_not_array_type,	"expected array type")\
  ILU_MINOR(bpm_not_record_type,	"expected record type")\
  ILU_MINOR(bpm_invalid_record_field_index,	"invalid record field index")\
  ILU_MINOR(bpm_not_enumeration_type,	"expected enumeration type")\
  ILU_MINOR(bpm_invalid_enum_element_index,	"invalid enum element index")\
  ILU_MINOR(bpm_not_variant_type,	"expected variant type")\
  ILU_MINOR(bpm_invalid_variant_type,	"variant type is badly formed")\
  ILU_MINOR(bpm_not_string_type,	"expected string type")\
  ILU_MINOR(bpm_invalid_typekind,	"ilu_Type with bad \"kind\"")\
  ILU_MINOR(bpm_invalid_method_index,	"out of bounds")\
  ILU_MINOR(bpm_invalid_argument_index,	"out of bounds")\
  ILU_MINOR(bpm_invalid_exception_index,	"out of bounds")\
  ILU_MINOR(bpm_not_sibling,	"non-sibling passed where sibling required")\
  ILU_MINOR_COMMENT("\
                        ... and now back to our regularly scheduled enumeration ...\
                    ")\
  ILU_MINOR(bpm_protocol_registered,	"attempt to register already-registered protocol")\
  ILU_MINOR(bpm_transport_registered,	"attempt to register already-registered transport")\
  ILU_MINOR(bpm_identity_type_registered,	"attempting to register already-registered identity type")\
  ILU_MINOR(bpm_bogus_raise,	"Method tried to raise an exception not in its RAISES list.")\
  ILU_MINOR(bpm_some_raise,	"Method with empty exn list tried to raise an exn.")\
  ILU_MINOR(bpm_short_char_codeset,	"unsupported short character code set")\
  ILU_MINOR(bpm_char_codeset,	"unsupported character code set")\
  ILU_MINOR(bpm_non_batching,	"flush called on non-batching conn")\
  ILU_MINOR(bpm_serialVsServer,	"ilu_Serializer used on wrong server")\
  ILU_MINOR(bpm_idTypePresent,	"passport already contains this identity type")\
  ILU_MINOR(bpm_serialConcurrent,	"bad case of ilu_Serializer used in concurrent calls")\
  ILU_MINOR(bpm_bad_character,	"this char not supported in this codeset")\
  ILU_MINOR(bpm_invalid_base,	"bad radix specified for string scan")\
  ILU_MINOR(bpm_divide_by_zero,	"zero passed as divisor")\
  ILU_MINOR(bpm_serialVsTransport,	"ilu_Serializer used with unreliable transport")\
  ILU_MINOR(bpm_muxBadEndpoint,	"bad endpoint specified in mux trans")\
  ILU_MINOR(bpm_mux_channel,	"bad channel specified in mux trans")\
  ILU_MINOR(bpm_not_collectible,	"non-collectible class used")\
  ILU_MINOR(bpm_surrogate,	"surrogate obj or server used where true required")\
  ILU_MINOR(bpm_badMallocPtr,	"attempt to free non-malloced storage")\
  ILU_MINOR(bpm_badTinfoIndex,	"invalid index into tinfo vector (too large)")\
  ILU_MINOR(bpm_convProtocolExcn,	"converted ilu_ProtocolException_GarbageArguments")\
  ILU_MINOR(bpm_gssNameString,	"malformed gss namestring; missing namespace id?")\
  ILU_MINOR_LAST(bad_param)

typedef enum { ILU_BAD_PARAM_MINORS } ilu_bad_param_Minor;

ILU_DECL_ERR(bad_param)
{
  ilu_bad_param_Minor minor;
} ILU_END_DECL_ERR;

ILU_DECL_ERR(no_memory)
{
  ilu_cardinal    nbytes;	/* size of allocation that failed; 0
				 * if not known */
} ILU_END_DECL_ERR;

#define ILU_IMP_LIMIT_MINORS\
  ILU_M1NOR(ilm_strlen,	"ILU will marshal only strings & byte-sequences less than a certain length.")\
  ILU_MINOR(ilm_nomst,	"When importing a surrogate, ILU requires that the importing program know some of the object's types, and that one of those known types is a subtype of all the other known types.")\
  ILU_MINOR(ilm_max_protocols,	"too many protocols registered")\
  ILU_MINOR(ilm_max_transports,	"too many transports registered")\
  ILU_MINOR(ilm_max_identity_types,	"too many identity types registered")\
  ILU_MINOR(ilm_refcnt,	"refcount would overflow")\
  ILU_MINOR(ilm_max_union_arms,	"too many arms to a union (ILU only permits 32766)")\
  ILU_MINOR(ilm_bad_type_for_protocol,	"this protocol does not support this type (some reasonable operation cannot be performed because of a bug in the CORBA spec which ILU faithfully implements)")\
  ILU_MINOR(ilm_sub_protocol,	"a sub-protocol has been specified which is not supported")\
  ILU_MINOR(ilm_unsupportedPickleFormat,	"specified pickle format not supported")\
  ILU_MINOR(ilm_max_buffer_size,	"attempt to use a buffer for a sequence too long for it")\
  ILU_MINOR(ilm_unsupported_charset_encoding,	"specified character set encoding not supported")\
  ILU_MINOR(ilm_unsupported_language,	"specified string language not supported")\
  ILU_MINOR(ilm_unsupported_id_type,	"specified identity type not supported")\
  ILU_MINOR(ilm_bignum_size,	"bignum too large for use")\
  ILU_MINOR(ilm_corba_iiop_unions,	"some valid OMG IDL union typecodes cannot be marshalled with IIOP 1.0 or 1.1")\
  ILU_MINOR(ilm_redirect_cross_protocol,	"attempt to redirect call to different RPC protocol")\
  ILU_MINOR_LAST(imp_limit)

typedef enum { ILU_IMP_LIMIT_MINORS } ilu_imp_limit_Minor;

ILU_DECL_ERR(imp_limit)
{
  ilu_imp_limit_Minor minor;
} ILU_END_DECL_ERR;

#define ILU_COMM_FAILURE_MINORS\
  /* First, failures to establish connection: */\
\
  ILU_M1NOR(cfm_socket_type,	"OS doesn't support sock type or protocol")\
  ILU_MINOR(cfm_bad_address,	"local or remote addr not available")\
  ILU_MINOR(cfm_connect_refused,	"remote end refused connection")\
  ILU_MINOR(cfm_timeout,	"timeout")\
  ILU_MINOR(cfm_nonblock,	"can't achieve non-blocking I/O")\
  ILU_MINOR(cfm_connect_failed,	"some other, or unknown, reason")\
\
  /* Second, failures of established connections: */\
\
  ILU_MINOR(cfm_eof,	"unexpected eof on connection")\
  ILU_MINOR(cfm_protocol_sync_lost,	"unexpected bytes with no way to recover")\
  ILU_MINOR(cfm_tr_non_boundaried,	"can't use this transport stack")\
  ILU_MINOR(cfm_conn_lost,	"other or unknown reason")\
  ILU_MINOR(cfm_resource_mgmt,	"closed due to need for FD")\
  ILU_MINOR(cfm_convProtocolExcn,	"converted ilu_ProtocolException_LostConnection")\
\
  /* Third, other failures: */\
\
  ILU_MINOR(cfm_gcRegFailed,	"a GC callback failed")\
  ILU_MINOR(cfm_pingFailed,	"a ping failed")\
  ILU_MINOR_LAST(comm_failure)

typedef enum { ILU_COMM_FAILURE_MINORS } ilu_comm_failure_Minor;

ILU_DECL_ERR(comm_failure)
{
  ilu_comm_failure_Minor minor;
} ILU_END_DECL_ERR;

#define ILU_INV_OBJREF_MINORS\
  ILU_M1NOR(iom_unknown,	"reason unknown or has no specific minor code")\
  ILU_MINOR(iom_sbh,	"malformed SBH")\
  ILU_MINOR(iom_ci,	"malformed contact info")\
  ILU_MINOR(iom_pi,	"malformed protocol info")\
  ILU_MINOR(iom_pc,	"unknown protocol class")\
  ILU_MINOR(iom_ps,	"protocol-specific part invalid")\
  ILU_MINOR(iom_ti,	"malformed transport info")\
  ILU_MINOR(iom_tc,	"unknown transport class")\
  ILU_MINOR(iom_ts,	"transport-specific part invalid")\
  ILU_MINOR(iom_sid,	"malformed server ID")\
  ILU_MINOR(iom_ih,	"malformed instance handle")\
  ILU_MINOR(iom_bad_mstid,	"malformed MSTID")\
  ILU_MINOR(iom_mstid_fail,	"can't resolve MSTID")\
  ILU_MINOR(iom_nil,	"NIL object found in invalid context")\
  ILU_MINOR(iom_bad_url_scheme,	"invalid scheme tag")\
  ILU_MINOR(iom_tf,	"Transport class used in inappropriately filterly position.")\
  ILU_MINOR(iom_ior,	"invalid IIOP IOR")\
  ILU_MINOR(iom_cant_connect,	"can't connect to server")\
  ILU_MINOR(iom_svr_closed,	"server closed")\
  ILU_MINOR(iom_type_nf,	"exist no inst.s of req'd type")\
  ILU_MINOR(iom_meth_nf,	"receiver doesn't recognize method")\
  ILU_MINOR(iom_inst_nf,	"instance doesn't exist")\
  ILU_MINOR(iom_bad_single,	"can't exist due to singleton restr.")\
  ILU_MINOR(iom_wrong_type,	"instance doesn't have right type")\
  ILU_MINOR(iom_short_char_codeset,	"unsupported short character code set")\
  ILU_MINOR(iom_char_codeset,	"unsupported character code set")\
  ILU_MINOR(iom_relocate_loop,	"relocate loop"/*detection might be conservative*/)\
  ILU_MINOR(iom_conc_serial,	"serializer used when contact info is concurrent")\
  ILU_MINOR(iom_CosNaming_NotFound,	"CosNaming returned NotFound")\
  ILU_MINOR(iom_CosNaming_CannotProceed,	"CosNaming returned CannotProceed")\
  ILU_MINOR(iom_CosNaming_InvalidName,	"CosNaming returned InvalidName")\
  ILU_MINOR_LAST(inv_objref)

typedef enum { ILU_INV_OBJREF_MINORS } ilu_inv_objref_Minor;

ILU_DECL_ERR(inv_objref)
{
  ilu_inv_objref_Minor minor;
} ILU_END_DECL_ERR;

ILU_DECL_PARMLESS_ERR(no_permission);

#define ILU_INTERNAL_MINORS\
  ILU_M1NOR(im_inv_mutex,	"some mutex was deemed \"invalid\"")\
  ILU_MINOR(im_broken,		"kernel data str broken --- NIL where shouldn't be")\
  ILU_MINOR(im_unhandled,	"Unexpected ilu_Error type raised in MOP")\
  ILU_MINOR(im_errno,		"syscall raised unexpected errno")\
  ILU_MINOR(im_badKernelErr,	"kernel call returned undexpected major err code")\
  ILU_MINOR(im_trBufSize,	"tc_get_output_buffer couldn't")\
  ILU_MINOR(im_tInfoLen,	"tinfo too long")\
  ILU_MINOR(im_badTypeKind,	"invalid ilu_TypeKind code for context")\
  ILU_MINOR(im_callFail,	"an internal call failed")\
  ILU_MINOR(im_badLocks,	"bad_locks maps to this case")\
  ILU_MINOR(im_brokenLocks,	"broken_locks maps to this case")\
  ILU_MINOR(im_inputBuffer,	"Input buffer expected but not found")\
  ILU_MINOR(im_outputBuffer,	"Output buffer expected but not found")\
  ILU_MINOR(im_endMessage,	"tc_end_message when no msg active")\
  ILU_MINOR(im_endMessageDir,	"end_output_message_nonblock called during message input")\
  ILU_MINOR(im_beginMessage,	"tc_begin_message when msg active")\
  ILU_MINOR(im_bytesWithoutMsg,	"byte I/O outside message boundaries")\
  ILU_MINOR(im_tcBytesDropped,	"for tc_end_message")\
  ILU_MINOR(im_tcBug,		"TransportClass didn't meet contract")\
  ILU_MINOR(im_tcInputSkipsBuff,	"a particular Trans'Class caller bug")\
  ILU_MINOR(im_tcNotBoundaried,	"for b'd method of unb'd Trans'Class")\
  ILU_MINOR(im_tcReliable,	"for unreliable call on reliable TC")\
  ILU_MINOR(im_tcBadBuff,	"bad buffer given to Trans'Class proc")\
  ILU_MINOR(im_protNonConcurrent,	"DelayInterp called on non-concurrent prot")\
  ILU_MINOR(im_tcNoMsgHandle,	"for tc_end_message")\
  ILU_MINOR(im_noHostName,	"unable to get a name for this host")\
  ILU_MINOR(im_noHostIpAddr,	"unable to get IP addr for this host")\
  ILU_MINOR(im_bufxpMisuse,	"bufxp caller violated contract")\
  ILU_MINOR(im_typeMismatch,	"two stubs with different ideas")\
  ILU_MINOR(im_typeIncomplete,	"type not completely constructed")\
  ILU_MINOR(im_typeDuplicated,	"type multiply registered (same ID)")\
  ILU_MINOR(im_typeNameCollision,	"same name on different types")\
  ILU_MINOR(im_dupForkProc,	"attempt to register ForkProc twice")\
  ILU_MINOR(im_noForkProc,	"no way to fork an internal thread")\
  ILU_MINOR(im_threading,	"confusion on whether threaded")\
  ILU_MINOR(im_threadFork,	"fork failed")\
  ILU_MINOR(im_threadIDSize,	"thread ID size too large for context")\
  ILU_MINOR(im_threadAttribute,	"problem setting/getting thread attr")\
  ILU_MINOR(im_tportRole,	"incoming vs. outgoing transport err")\
  ILU_MINOR(im_check,		"internal consistency check failed")\
  ILU_MINOR(im_badEnumValue,	"unexpected enumeration value")\
  ILU_MINOR(im_pickleFormat,	"bad pointer found in pickle format pos")\
  ILU_MINOR(im_ptrAlignment,	"mis-aligned pointer encountered")\
  ILU_MINOR(im_tcCreate,	"unknown error in transport creation")\
  ILU_MINOR(im_multiple_channels,	"in W3MUX, same chnl regd twice")\
  ILU_MINOR(im_mux_max_credit,	"in W3MUX, local credit buildup")\
  ILU_MINOR(im_badRelocVals,	"relocate proc returned odd results")\
  ILU_MINOR(im_badRelocPinfo,	"relocate proc returned invalid pinfo value")\
  ILU_MINOR(im_badRelocTinfo,	"relocate proc returned invalid tinfo value")\
  ILU_MINOR(im_convPESuccess,	"attempt to convert ilu_ProtocolException_Success to an error")\
  ILU_MINOR(im_invalidPE,	"attempt to convert invalid ilu_ProtocolException value to an error")\
  ILU_MINOR_LAST(internal)

typedef enum { ILU_INTERNAL_MINORS } ilu_internal_Minor;

ILU_DECL_ERR(internal)
{
  ilu_internal_Minor minor;
} ILU_END_DECL_ERR;

#define ILU_MARSHAL_MINORS\
  ILU_M1NOR(mm_eom,		"attempted read past end of msg")\
  ILU_MINOR(mm_alien_disc,	"unmarshalling discriminator of different server")\
  ILU_MINOR(mm_bad_union_disc,	"invalid union discriminant value")\
  ILU_MINOR(mm_bad_typekind,	"TypeCode received with invalid type kind value")\
  ILU_MINOR(mm_wronglen,	"fixed length array came in with different length")\
  ILU_MINOR(mm_sequenceLimit,	"attempt to read or write a sequence longer than its limit")\
  ILU_MINOR(mm_badMagicNumber,	"bad message header magic number")\
  ILU_MINOR(mm_versionMismatch,	"wrong version of message protocol")\
  ILU_MINOR(mm_badInteger,	"signed or unsigned integer that doesn't fit position")\
  ILU_MINOR(mm_badFloat,	"floating point value that doesn't fit position")\
  ILU_MINOR(mm_dgramLimit,	"datagram (eg, UDP) size limit exceeded")\
  ILU_MINOR(mm_badPickle,	"malformed pickle bytes")\
  ILU_MINOR(mm_badTypeName,	"badly formed type name")\
  ILU_MINOR(mm_protNoTypekind,	"this typekind not supported by this protocol")\
  ILU_MINOR(mm_msgTypeUnknown,	"invalid message type received")\
  ILU_MINOR(mm_utf2Len,		"UTF2 encoded string's length doesn't correspond to plain string length")\
  ILU_MINOR(mm_noCharset,	"can't determine charset of string")\
  ILU_MINOR(mm_cantConvertCharset,	"can't convert string to specified charset")\
  ILU_MINOR(mm_mst_unreg,	"LSR can't make surrogate")\
  ILU_MINOR(mm_fixedpoint_range,	"bounded fixedpoint value out-of-range")\
  ILU_MINOR(mm_excn_id,		"Exception reply has bad excn indicator")\
  ILU_MINOR(mm_enum_value,	"Value for enum out of range")\
  ILU_MINOR(mm_unknown,		"Unknown marshalling error")\
  ILU_MINOR(mm_url_quoted_char,	"Bad quoted hex char in URL form")\
  ILU_MINOR(mm_no_val_for_nonopt_ref,	"no value for non-optional reference")\
  ILU_MINOR_LAST(marshal)

typedef enum { ILU_MARSHAL_MINORS } ilu_marshal_Minor;

ILU_DECL_ERR(marshal)
{
  ilu_marshal_Minor minor;
} ILU_END_DECL_ERR;

ILU_DECL_PARMLESS_ERR(initialize);	/* not used */
ILU_DECL_PARMLESS_ERR(no_implement);

#define ILU_BAD_TYPECODE_MINORS\
  ILU_M1NOR(btm_unknownType,	"reference to unknown (by this addr space) type")\
  ILU_M1NOR(btm_convNoSuchClass,"converted ilu_ProtocolException_NoSuchClassAtServer")\
  ILU_M1NOR(btm_convVersionMismatch,	"converted ilu_ProtocolException_ClassVersionMismatch")\
  ILU_MINOR_LAST(bad_typecode)

typedef enum { ILU_BAD_TYPECODE_MINORS } ilu_bad_typecode_Minor;

ILU_DECL_ERR(bad_typecode)
{
  ilu_bad_typecode_Minor minor;
} ILU_END_DECL_ERR;

#define ILU_BAD_OPERATION_MINORS\
  ILU_M1NOR(bom_noSuchOperationOnType,	"specified operation not defined on specified type")\
  ILU_MINOR(bom_convProtocolExcn,	"converted ilu_ProtocolException_NoSuchMethodOnClass")\
  ILU_MINOR_LAST(bad_operation)

typedef enum { ILU_BAD_OPERATION_MINORS } ilu_bad_operation_Minor;

ILU_DECL_ERR(bad_operation)
{
  ilu_bad_operation_Minor minor;
} ILU_END_DECL_ERR;

#define ILU_NO_RESOURCES_MINORS\
  ILU_M1NOR(nrm_EMFILE,	"per-process descriptor table full")\
  ILU_MINOR(nrm_ENFILE,	"system file table full")\
  ILU_MINOR(nrm_ENOBUFS,	"insufficient buffer space avail")\
  ILU_MINOR(nrm_fds,	"FD budget")\
  ILU_MINOR(nrm_mux_sessions,	"Sessions on a single mux channel")\
  ILU_MINOR(nrm_mux_channels,	"Channels available via mux in a single endpoint")\
  ILU_MINOR(nrm_mlreg,	"main loop registrations")\
  ILU_MINOR(nrm_mux_atom_id,	"no more atom values available")\
  ILU_MINOR_LAST(no_resources)

typedef enum { ILU_NO_RESOURCES_MINORS } ilu_no_resources_Minor;

ILU_DECL_ERR(no_resources)
{
  ilu_no_resources_Minor minor;
} ILU_END_DECL_ERR;

ILU_DECL_PARMLESS_ERR(no_response);	/* not used */
ILU_DECL_PARMLESS_ERR(persist_store);	/* not used */
ILU_DECL_PARMLESS_ERR(bad_inv_order);	/* not used */

#define ILU_TRANSIENT_MINORS\
  ILU_M1NOR(tm_retry,	"stub should retry call (but didn't, if app sees this)")\
  ILU_MINOR_LAST(transient)

typedef enum { ILU_TRANSIENT_MINORS } ilu_transient_Minor;

ILU_DECL_ERR(transient)
{
  ilu_transient_Minor minor;
} ILU_END_DECL_ERR;


ILU_DECL_PARMLESS_ERR(free_mem);	/* not used */
ILU_DECL_PARMLESS_ERR(inv_ident);	/* not used */
ILU_DECL_PARMLESS_ERR(inv_flag);	/* not used */
ILU_DECL_PARMLESS_ERR(intf_repos);	/* not used */
ILU_DECL_PARMLESS_ERR(bad_context);	/* not used */
ILU_DECL_PARMLESS_ERR(obj_adapter);	/* not used */
ILU_DECL_PARMLESS_ERR(data_conversion);	/* not used */

ILU_DECL_PARMLESS_ERR(codeset_incompatible);	/* added in CORBA 2.1 revision */

/* This is the end of the prefix that corresponds to CORBA's list. */

ILU_DECL_PARMLESS_ERR(barrier);
/*
 * Raised to indicate an exception to a serialization guarantee.
 */

ILU_DECL_PARMLESS_ERR(bad_locks);
/*
 * Raised when the calling thread is detected to have violated the
 * locking precondition.  => CORBA::INTERNAL (cf discussion w David
 * Brownell)
 */

ILU_DECL_PARMLESS_ERR(broken_locks);
/*
 * This is raised when the implementation of mutexes & condition
 * variables either (a) refuses to operate on the mutexes and
 * condition variables of the kernel or (b) raises an error that the
 * kernel knows it shouldn't.  When broken_locks is raised, we can't
 * even establish the locking postcondition of the raising
 * procedure.  => CORBA::INTERNAL
 */

ILU_DECL_ERR(interrupted)
{
  ilu_byte	ilu_interruptSet;
  /*
   * A threaded LS runtime can set this to indicate which of several
   * possible reasons for an interrupt are active.
   */
} ILU_END_DECL_ERR;

typedef enum {
  ilu_gsm_GSS_S_BAD_BINDINGS,		/* Channel Binding Mismatch */
  ilu_gsm_GSS_S_BAD_MECH,		/* Unsupported Mechanism Requested */
  ilu_gsm_GSS_S_BAD_NAME,		/* Invalid Name Provided */
  ilu_gsm_GSS_S_BAD_NAMETYPE,		/* Name Of Unsupported Type Provided */
  ilu_gsm_GSS_S_BAD_STATUS,		/* Invalid Input Status Selector */
  ilu_gsm_GSS_S_BAD_SIG,		/* Token Had Invalid Signature */
  ilu_gsm_GSS_S_CONTEXT_EXPIRED,	/* Specified Security Context Expired */
  ilu_gsm_GSS_S_CREDENTIALS_EXPIRED,	/* Expired Credentials Detected */
  ilu_gsm_GSS_S_DEFECTIVE_CREDENTIAL,	/* Defective Credential Detected */
  ilu_gsm_GSS_S_DEFECTIVE_TOKEN,	/* Defective Token Detected */
  ilu_gsm_GSS_S_FAILURE,		/* Failure, Unspecified At GSS-API Level */
  ilu_gsm_GSS_S_NO_CONTEXT,		/* No Valid Security Context Specified */
  ilu_gsm_GSS_S_NO_CRED,		/* No Valid Credentials Provided */
  ilu_gsm_GSS_S_BAD_QOP,		/* Unsupported QOP Value */
  ilu_gsm_GSS_S_UNAUTHORIZED,		/* Operation Unauthorized */
  ilu_gsm_GSS_S_UNAVAILABLE,		/* Operation Unavailable */
  ilu_gsm_GSS_S_CONTINUE_NEEDED,	/* Continuation Call To Routine Required */
  ilu_gsm_GSS_S_DUPLICATE_TOKEN,	/* Duplicate Per-Message Token Detected */
  ilu_gsm_GSS_S_OLD_TOKEN,		/* Timed-Out Per-Message Token Detected */
  ilu_gsm_GSS_S_UNSEQ_TOKEN,		/* Reordered (Early) Per-Message Token Detected */
  ilu_gsm_GSS_S_GAP_TOKEN		/* Skipped Predecessor Token(S) Detected */
  } ilu_gss_security_Major;

ILU_DECL_ERR(gss_security)
{
  ilu_gss_security_Major	major;
  /* The GSS major code for the error.  See the GSS spec for mappings. */

  ilu_cardinal			minor;
  /* The GSS minor code for the error. */

} ILU_END_DECL_ERR;


/* Next, the additional errors that can appear in the kernel MOPs. */

typedef enum {
  ilu_relocate_call,	/* just this call */
  ilu_relocate_conn	/* use new conn. while it lasts */
}               ilu_RelocateScope;

extern ilu_string ilu_RelocateScope_Names[2];

#define ilu_RelocateScope_Name(rs) ((rs) <= ilu_relocate_conn ? ilu_RelocateScope_Names[rs] : "(invalid ilu_RelocateScope)")

ILU_DECL_ERR(relocate)
{
  ilu_RelocateScope	rel_scope;
  ilu_ProtocolInfo	rel_pinfo;	/* owned by this err */
  ilu_TransportInfo	rel_tinfo;	/* owned by this err */
} ILU_END_DECL_ERR;


/* Following are the internal errors of the new draft. */




/*
 * Following are the internal errors of the old draft; they're being
 * eliminated or promoted, as appropriate.
 */


/* Some standard errors for use throughout the kernel */


/* Raised when a fixed-length array in the impl overflows. */

ILU_DECL_ERR(MaxCountExceeded) {
  int max_count;
}
ILU_END_DECL_ERR;

/* Signalled by ilu_RegisterProtocol */

ILU_DECL_ERR(ProtocolAlreadyRegistered) {
  char * name;
  ilu_Protocol (*old_protocol) (ilu_ProtocolInfo, ilu_Error *);
  ilu_Protocol (*new_protocol) (ilu_ProtocolInfo, ilu_Error *);
}
ILU_END_DECL_ERR;

/* Signalled by ilu_RegisterTransport */

ILU_DECL_ERR(TransportAlreadyRegistered)
{
  char				*name;
  ilu_TransportCreator (*old_transport) (ilu_TransportInfo, ilu_Error *);
  ilu_TransportCreator (*new_transport) (ilu_TransportInfo, ilu_Error *);
}
ILU_END_DECL_ERR;

/*
 * Raised when the caller violates a locking precondition of a
 * procedure.  /which/ identifies a mutex relevent to the violated
 * constraint.  Signalled by lots of stuff.  When raised, nothing
 * else has been done, unless otherwise noted at the procedure in
 * question.
 */

typedef enum {
  ilu_lock_smu, ilu_lock_otmu, ilu_lock_cmu, ilu_lock_prmu,
  ilu_lock_trmu, ilu_lock_gcmu, ilu_lock_timu, ilu_lock_server,
  ilu_lock_io, ilu_lock_call
}               ilu_lock;

ILU_DECL_ERR(BadProtocolInfo)
{
  ilu_string      x;		/* owned by error */
} ILU_END_DECL_ERR;

ILU_DECL_ERR(GcRegFailed)
{
  ilu_string      why;		/* global -- not to be freed */

  ILU_ERRS((IoErrs, bad_locks, inv_objref, no_resources)) *sub;
  /* from subsidiary call */
} ILU_END_DECL_ERR;



ILU_DECL_ERR(NoObjectForSBH) {
  char           *sbh;		/* owned by error */
} ILU_END_DECL_ERR;

ILU_DECL_PARMLESS_ERR(CantCondition);

#define IoErrs bad_param, imp_limit, marshal, comm_failure, no_memory, internal, broken_locks, interrupted
/* Errors that might be rasied when doing I/O. */

/* And now we drop the shoe you've all been waiting for! */

#define ILU_ERRLIST 			\
					\
    ILU_ERRLISTELT(success)		\
    					\
    /* public types */			\
    ILU_ERRLISTELT(unknown)		\
    ILU_ERRLISTELT(bad_param)		\
    ILU_ERRLISTELT(no_memory)		\
    ILU_ERRLISTELT(imp_limit)		\
    ILU_ERRLISTELT(comm_failure)	\
    ILU_ERRLISTELT(inv_objref)		\
    ILU_ERRLISTELT(no_permission)	\
    ILU_ERRLISTELT(internal)		\
    ILU_ERRLISTELT(marshal)		\
    ILU_ERRLISTELT(initialize)		\
    ILU_ERRLISTELT(no_implement)	\
    ILU_ERRLISTELT(bad_typecode)	\
    ILU_ERRLISTELT(bad_operation)	\
    ILU_ERRLISTELT(no_resources)	\
    ILU_ERRLISTELT(no_response)		\
    ILU_ERRLISTELT(persist_store)	\
    ILU_ERRLISTELT(bad_inv_order)	\
    ILU_ERRLISTELT(transient)		\
    ILU_ERRLISTELT(free_mem)		\
    ILU_ERRLISTELT(inv_ident)		\
    ILU_ERRLISTELT(inv_flag)		\
    ILU_ERRLISTELT(intf_repos)		\
    ILU_ERRLISTELT(bad_context)		\
    ILU_ERRLISTELT(obj_adapter)		\
    ILU_ERRLISTELT(data_conversion)	\
    ILU_ERRLISTELT(codeset_incompatible)	\
    ILU_ERRLISTELT(barrier)		\
    ILU_ERRLISTELT(bad_locks)		\
    ILU_ERRLISTELT(broken_locks)	\
    ILU_ERRLISTELT(interrupted)		\
    ILU_ERRLISTELT(gss_security)	\
    					\
    /* approved private types (not to cross kernel interface) */	\
    ILU_ERRLISTELT(relocate)		\
    					\
    /* unresolved private types (not to cross kernel interface) */	\
    ILU_ERRLISTELT(MaxCountExceeded)	\
    ILU_ERRLISTELT(ProtocolAlreadyRegistered)	\
    ILU_ERRLISTELT(TransportAlreadyRegistered)	\
    ILU_ERRLISTELT(BadProtocolInfo)	\
    ILU_ERRLISTELT(GcRegFailed)		\
    ILU_ERRLISTELT(NoObjectForSBH)	\
    ILU_ERRLISTELT(CantCondition)

/* Define the enumeration of error types */

#define ILU_ERRLISTELT(id) ILU_ERRTYP(id),
typedef enum {ILU_ERRLIST ILU_ERRTYP(ErrListLen)} ilu_ErrorType;
#undef ILU_ERRLISTELT

/* Define the error struct */

#define ILU_ERRLISTELT(id) ILU_ERRMEM_T(id) ILU_ERRLBL(id);

struct ilu_Error_s {
  const char     *ilu_file;
  int             ilu_line;
  ilu_ErrorType   ilu_type;
  union {
  ILU_ERRLIST
  }               u;
};

#undef ILU_ERRLISTELT

ILU_PUBLIC ilu_Error ilu_success_err;

ILU_PUBLIC unsigned long
  ilu_CORBAizeSystemErr(ilu_Error * err,
		      ilu_integer * major);
/*
 * Call this to translate an ilu_Error from the kernel interface
 * into the terms of a CORBA system exception.  On success, sets
 * *major to the 0-based index into CORBA's list of standard
 * exceptions, and returns the minor code.  On failure, sets *major
 * to -1 and returns 0.  Calls ILU_HANDLED(*err).
 */

ILU_PUBLIC unsigned long
ilu_FullCORBAizeSystemErr(ilu_Error     *err,
			  ilu_integer   *major,
			  const char   **ilusl_filename,
			  int		*ilusl_linenum);
/*
 * Call this to translate an ilu_Error from the kernel interface into
 * the terms of a CORBA system exception.  On success: sets (*major)
 * to the 0-based index into CORBA's list of standard exceptions; sets
 * (*ilusl_filename) and (*ilusl_linenum), if they're not null, to the
 * source location of the error if available otherwise to NIL and 0;
 * and returns the minor code.  On failure, sets *major to -1 and
 * returns 0.  Error source filename is statically allocated ---
 * nobody modifies or frees it.  Calls ILU_HANDLED(*err).  */

ILU_PUBLIC const char *ilu_DescribeCorbaMinor(ilu_integer major,
					      unsigned long minor);
/* Returns global string, if any. */

#ifdef __cplusplus
}
#endif

#endif /* ndef _ILUERRS_H_ */
