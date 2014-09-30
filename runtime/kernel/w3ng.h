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
/* $Id: w3ng.h,v 1.16 1999/08/03 01:53:02 janssen Exp $ */

#include <stdio.h>

#if ((defined(WIN32) || defined(WIN16)) && defined(_WINIO))
#include <winiodef.h>
#endif /* ((defined(WIN32) || defined(WIN16)) && defined(_WINIO)) */

#include <sys/types.h>

typedef enum {
  Success = 0,
  UserException = 1,
  SystemExceptionBefore = 2,
  SystemExceptionAfter = 3
  } w3ng_ReplyStatus;

typedef enum {
  MangledMessage = 0,
  ProcessTermination = 1,
  ResourceManagement = 2,
  InvalidServerID = 3,
  MaxSerialNumber = 4,
  InvalidProtocolID = 5
  } w3ng_TermCause;

typedef struct {
  ilu_cardinal	cache_index;
  ilu_Class	cached_class;
  ilu_Method	cached_method;
}		CachedOperation;

typedef struct {
  ilu_cardinal	cache_index;
  ilu_string	serverID;
  ilu_string	instance_handle;
}		CachedObject;

typedef struct {
  unsigned	version : 8;
  unsigned	type_kind : 8;
  unsigned	type_id_len : 16;
}		PickleTypeIDHeader;

typedef struct {
  /* L1 >= {prmu}; L2 unconstrained */

  ilu_byte		major_version;
  ilu_cardinal		serial_number;
  ilu_cardinal		current_op_cache_val;
  ilu_HashTable		op_cache;	/* on incoming connections, maps index
					   value to operation; on outgoing connections,
					   maps operation to index value */
  ilu_cardinal		current_object_cache_val;
  ilu_HashTable		object_cache;	/* on incoming connections, maps index
					   value to object; on outgoing connections,
					   maps object to index value */
  ilu_boolean		incoming;
  ilu_boolean		connection_initialized;
  ilu_boolean		connection_closed;
  ilu_Transport		transport_stack;
  ilu_boolean		server_relocate_tried;
  ilu_cardinal		outgoing_default_charset;
  ilu_cardinal		incoming_default_charset;

  /* for caching sid... */
  ilu_bytes		outgoing_last_sid;
  ilu_cardinal		outgoing_last_sid_len;
  ilu_bytes		incoming_last_sid;
  ilu_cardinal		incoming_last_sid_len;

  /* for caching cinfo... */
  ilu_CharBuf *		outgoing_last_cinfo;
  ilu_cardinal		outgoing_last_cinfo_count;
  ilu_CharBuf		incoming_last_cinfo_string;

#ifdef ILU_W3NG_RELATIVE_IH_PASSING
  /* for caching instance handle prefixes... */
  ilu_byte		outgoing_ih_prefix[256];
  ilu_byte		incoming_ih_prefix[256];
#endif  
}               W3ng;

typedef struct w3ng_CharBufStack_s {
  ilu_CharBuf			cb;
  struct w3ng_CharBufStack_s *	next;
} *w3ng_CharBufStack;

typedef struct w3ng_TransportStack_s {
  ilu_Transport			transport;
  struct w3ng_TransportStack_s *next;
} *w3ng_TransportStack;

typedef struct {
  /* L1 >= {prmu}; L2 unconstrained */

  unsigned		reply_status : 2;
  unsigned		minor_version : 4;
  unsigned		context_present : 1;

  unsigned		disc_cached : 1;
  unsigned		cache_disc : 1;
  unsigned		disc_cache_index : 14;
  unsigned		disc_len : 13;

  unsigned		op_cached : 1;
  unsigned		cache_op : 1;
  unsigned		op_cache_index : 14;
  unsigned		op_index : 13;

  unsigned		message_begun : 1;
  unsigned		close_transport_on_finish_call : 1;

  /* used on input to retain data over the span of calls to
     _w3ng_{Begin,End}InputObject() */
  ilu_Object		discriminant;	/* discriminant of call */
  w3ng_CharBufStack	mstids;		/* stack of mstids */

  w3ng_TransportStack	transports;	/* stack of other transports */
}               W3ngCallData;

#define w3ng_instance_data(call)	((W3ng*)(connection_protocol_data(call_connection(call))))
#define w3ng_call_data(call)		((W3ngCallData *)((call)->ca_prdata2))

/* Message types */

#define W3NG_CONTROL_MSG_P(x)		(((x)&0x80000000)!=0)
#define W3NG_CONTROL_MSG_ID(x)		(((x)&0x70000000)>>28)
#define W3NG_EXTENSION_HEADERS_P(x)	(((x)&0x40000000)!=0)

#define W3NG_REQUEST_DISC_CACHED_P(x)	(((x)&0x00004000)!=0)
#define W3NG_REQUEST_DISC_CACHE_IDX(x)	((x) &0x00003FFF)
#define W3NG_REQUEST_CACHE_DISC_P(x)	(((x)&0x00002000)!=0)
#define W3NG_REQUEST_DISC_LEN(x)	((x) &0x00001FFF)
#define W3NG_REQUEST_OP_CACHED_P(x)	(((x)&0x20000000)!=0)
#define W3NG_REQUEST_OP_CACHE_IDX(x)	(((x)&0x1FFF8000)>>15)
#define W3NG_REQUEST_CACHE_OP_P(x)	(((x)&0x10000000)!=0)
#define W3NG_REQUEST_OP_INDEX(x)	(((x)&0x0FFF8000)>>15)

#define W3NG_REPLY_STATUS(x)		(((x)&0x30000000)>>28)
#define W3NG_REPLY_SERIAL_NUMBER(x)	((x)&0x00FFFFFF)

#define W3NG_INITIALIZE_CONNECTION_MSG	0
#define W3NG_TERMINATE_CONNECTION_MSG	1
#define W3NG_DEFAULT_CHARSET_MSG	2

#define W3NG_INITIALIZE_CONNECTION_SIDLEN(x)	((x)&0x0000FFFF)
#define W3NG_TERMINATE_CONNECTION_CAUSE(x)	(((x)&0x0F000000)>>24)
#define W3NG_TERMINATE_CONNECTION_LAST_SN(x)	((x)&0x00FFFFFF)
#define W3NG_DEFAULT_CHARSET_CHARSET(x)		((x)&0x0000FFFF)

#define W3NG_REPLY_STATUS_SUCCESS		0
#define W3NG_REPLY_STATUS_USEREXN		1
#define W3NG_REPLY_STATUS_SYSEXNB		2
#define W3NG_REPLY_STATUS_SYSEXNA		3

/* System Exceptions */

#define W3NG_SYSEXN_UnknownProblem		0
#define W3NG_SYSEXN_ImplementationLimit		1
#define W3NG_SYSEXN_SwitchSessionCinfo		2
#define W3NG_SYSEXN_Marshal			3
#define W3NG_SYSEXN_NoSuchObjectType		4
#define W3NG_SYSEXN_NoSuchMethod		5
#define W3NG_SYSEXN_Rejected			6
#define W3NG_SYSEXN_DiscOrOpCacheOverflow	7
