/** $Id: iluntrnl.h,v 1.248 1999/08/11 02:11:21 janssen Exp $
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
/* Last edited by Mike Spreitzer January 15, 1999 11:53 am PST */
/* Chris Jacobi, June 10, 1998 11:25 am PDT */

#ifndef _ILU_INTERNALS_
#define _ILU_INTERNALS_

#include <iluxport.h>
#include <ilutransport.h>
#include <iluvector.h>

#ifdef ADD_VARIANT_SUPPORT
#include "ilutypes.h"
#endif /* ADD_VARIANT_SUPPORT */

#include <stdio.h>

#if ((defined(WIN32) || defined(WIN16)) && defined(_WINIO))
#include <winiodef.h>
#endif /* ((defined(WIN32) || defined(WIN16)) && defined(_WINIO)) */

#include <string.h>

#define NIL ILU_NIL
#define NULLFN 0
#define NULLCH 0

#define ILU_OPTIONAL(x)	x
#define ILU_PASS(x)		x
#define ILU_RETAIN(x)	x
#define ILU_GLOBAL(x)	x


/* ==================== internal type info ==================== */

#define MAX_CCPEER_LEN 64

extern ilu_cardinal _ilu_NLanguages;	/* # lang.s registered. */
extern ilu_string _ilu_LangNames[ILU_MAX_ADDR_SPACE_LANGUAGES];	/* their names */
extern ilu_LanguageIndex _ilu_InternalLanguageIndex;
/* language index used for built-in methods */

extern ilu_integer	_ilu_GCTimeOut;	/* number of seconds to wait for new client
					   before collecting collectible true inst */

/*Main Invariant holds; L2 >= {call's conn's callmu}*/
extern void
  _ilu_AddConnIdentities (ilu_Call, ILU_ERRS((no_memory, bad_param)) *);
/* Adds the identities in conn->co_auth_info
   to the call->ca_caller passport.  Creates the passport if that field is NIL */

/* ----- utility functions for dealing with WString IO ----- */

/* L1, L2 unconstrained */
ilu_cardinal
  _ilu_SizeOfWString (ilu_Call call,
		      ilu_wstring s,
		      ilu_cardinal l1	/* size of wstring */,
		      ilu_cardinal limit,
		      ILU_ERRS((IoErrs)) *err);

/* Main holds, Call-OHi(call) */
void
  _ilu_OutputWString (ilu_Call call,
		      ilu_wstring s,
		      ilu_cardinal l1,
		      ilu_cardinal limit,
		      ILU_ERRS((IoErrs)) * err);

/* Main holds, call-IHi(call) */
void
  _ilu_InputWString (ilu_Call call,
		     ilu_wstring * s,
		     ilu_cardinal * l,
		     ilu_cardinal limit,
		     ILU_ERRS((IoErrs)) * err);

/* L1, L2 unconstrained */
ilu_cardinal
  _ilu_SizeOfWStringVec (ilu_Call call,
			 ilu_wstring s,
			 ilu_cardinal l1	/* size of wstringvec */,
			 ILU_ERRS((IoErrs)) *err);

/* Main holds, Call-OHi(call) */
void
  _ilu_OutputWStringVec (ilu_Call call,
			 ilu_wstring s,
			 ilu_cardinal l1,
			 ILU_ERRS((IoErrs)) * err);

/* Main holds, Call-IHi(call) */
void
  _ilu_InputWStringVec (ilu_Call call,
			ilu_wstring * s,
			ilu_cardinal l,
			ILU_ERRS((IoErrs)) * err);

/* ----- utility functions for dealing with fixed-point IO ----- */

#ifdef ILU_FIXED_POINT_SUPPORT

ILU_PUBLIC ilu_boolean
  _ilu_InputFixedpoint (ilu_Call,
			ilu_Bignum *,	/* numerator, PASS, ((ilu_Bignum)0) => NaN, ((ilu_Bignum)1) => Infinity */
			ilu_Bignum,	/* min_numerator value, OPTIONAL, RETAIN */
			ilu_Bignum,	/* max_numerator value, OPTIONAL, RETAIN */
			ilu_Bignum,	/* denominator value, RETAIN */
			ilu_cardinal,		/* if non-zero, digits as CORBA 'fixed' */
			ilu_cardinal,		/* decimal places as CORBA 'fixed' */
			ilu_FixedPointRangeSize,	
			ILU_ERRS((IoErrs)) *);

ILU_PUBLIC ilu_cardinal
  _ilu_SizeOfFixedpoint (ilu_Call,
			 ilu_Bignum,	/* numerator, RETAIN, ((ilu_Bignum)0) => NaN, ((ilu_Bignum)1) => Infinity */
			 ilu_Bignum,	/* min_numerator value, OPTIONAL, RETAIN */
			 ilu_Bignum,	/* max_numerator value, OPTIONAL, RETAIN */
			 ilu_Bignum,	/* denominator value, RETAIN */
			 ilu_cardinal,		/* if non-zero, digits as CORBA 'fixed' */
			 ilu_cardinal,		/* decimal places as CORBA 'fixed' */
			 ilu_FixedPointRangeSize,	
			 ILU_ERRS((IoErrs)) *);
ILU_PUBLIC void
  _ilu_OutputFixedpoint (ilu_Call,
			 ilu_Bignum,	/* numerator, RETAIN, ((ilu_Bignum)0) => NaN, ((ilu_Bignum)1) => Infinity */
			 ilu_Bignum,	/* min_numerator value, OPTIONAL, RETAIN */
			 ilu_Bignum,	/* max_numerator value, OPTIONAL, RETAIN */
			 ilu_Bignum,	/* denominator value, RETAIN */
			 ilu_cardinal,		/* if non-zero, digits as CORBA 'fixed' */
			 ilu_cardinal,		/* decimal places as CORBA 'fixed' */
			 ilu_FixedPointRangeSize,	
			 ILU_ERRS((IoErrs)) *);

#endif /* def ILU_FIXED_POINT_SUPPORT */

#ifdef ILU_REFERENCE_TYPES

/* default procedures for protocol-independent marshalling of the
   reference type */

ILU_PUBLIC ilu_cardinal
  _ilu_SizeOfReference (ilu_Call call, ilu_boolean providedp,
		       ilu_boolean *first, ilu_ReferenceID id,
		       ILU_ERRS((IoErrs)) *err);

ILU_PUBLIC void
  _ilu_OutputReference (ilu_Call call, ilu_boolean providedp,
			ilu_boolean *first, ilu_ReferenceID id,
			ILU_ERRS((IoErrs)) *err);

ILU_PUBLIC ilu_cardinal
  _ilu_InputReference (ilu_Call call, ilu_boolean *present,
		       ilu_ReferenceID *id,
		       ILU_ERRS((IoErrs)) *err);

ILU_PUBLIC void
  _ilu_EndInputReference (ilu_Call call, ilu_cardinal wire_id,
			  ilu_ReferenceID id,
			  ilu_Error *err);

#endif /* def ILU_REFERENCE_TYPES */

/* ----- utility functions for dealing with object IO ----- */

/*Call-OHi(call).
  kobj == NIL => Main Invariant holds.
  kobj != NIL => all the following:
  before: Inside(s, cl);
  after:				 L1 disjoint {cmu, s};
  after: cl collectible		      => L1  not >=  {gcmu};
  after: cl collectible & s surrogate => Main Invariant holds;
  where s = kobj's server and cl = kobj's type.
  (We don't really need to hold cmu for surrogate or non-collectible
   objects, but this is convenient because ilu_Enter/ExitServer can
   be used.)*/
ilu_boolean 
_ilu_OutputObjectID(ilu_Call call, ilu_Object kobj,
		    ilu_boolean discriminator_p,
		    ilu_Class static_type,
		    ILU_ERRS((IoErrs)) * err);

/**L2 unconstrained;
   kobj!=NIL => L1 >= {kobj's server}, L1.sup < prmu;
   kobj==NIL => L1 unconstrained*/
ilu_cardinal _ilu_SizeOfObjectID (ilu_Call call, ilu_Object kobj,
				  ilu_boolean discriminator_p,
				  ilu_Class static_type,
				  ILU_ERRS((IoErrs)) *err);

/**before: L1 = {};
   after:  *kobj!=NIL => Inside(*kobj's server, static_type);
   after:  *kobj==NIL => L1 = {};
   Main Remnant holds;
   Call-IHi(call) */
ilu_boolean 
_ilu_InputObjectID(ilu_Call call, ilu_Object * kobj,
		   ilu_boolean discriminator_p,
		   ilu_Class static_type,
		   ILU_ERRS((IoErrs)) * err);

struct _ilu_Serializer_s {
  ilu_Server      si_server;

  /* L1 >= {si_server}; L2 unconstrained */

  ilu_Connection  si_conn;	/* NIL if not bound right now */
  ilu_cardinal    si_nCalls;	/* num. calls between ilu_StartCall
				 * and ilu_FinishCall */
  ilu_boolean     si_lsrCares;
  ilu_boolean     si_connecting;
  ilu_Condition   si_connChg;
};

/* L1 >= {si->si_server}; L2 unconstrained */
extern void     _ilu_MaybeFreeSerializer(ilu_Serializer si);
/*
 * NIL argument OK.  If (si && si->si_conn), call
 * _ilu_MaybeFreeConnection(si->si_conn) afterward.
 */

struct _ilu_Pipeline_s {
  /* L1 >= {cmu}; L2 unconstrained */

  ilu_cardinal    pl_nCalls;	/* using me */
  ilu_boolean     pl_lsrCares;
};
/*
 * The essence of the implementation of pipelining is this: we allow
 * multiple outstanding calls on a serial connection iff they are
 * all associated with the same non-NIL ilu_Pipeline.  Note that
 * this allows the same ilu_Pipeline to be associated with multiple
 * connections (of different servers).  To enable the multiplicity
 * of calls on a single connection, we release the call mutex while
 * waiting for a reply over a serial connection when an ilu_Pipeline
 * is involved.  (pl_nCalls) counts the number of associated calls
 * between ilu_StartCall and ilu_FinishCall; (pl_lsrCares) is true
 * until ilu_ReleasePipeline is called; _ilu_MaybeFreePipeline will
 * free the ilu_Pipeline when (!pl_nCalls && !pl_lsrCares).
 */

/* L1 >= {cmu}; L2 unconstrained */
extern void     _ilu_MaybeFreePipeline(ilu_Pipeline);
/* NIL argument OK */

struct ilu_Batcher_struct {
  /* L1, L2 unconstrained */

  ilu_Mutex       bchr_lock;	/* > cmu, < timu */
  ilu_FineTime    bchr_TO;	/* timeout */
  ilu_boolean     bchr_timed;	/* timeout > 0 */
  ilu_boolean     bchr_pushable; /* ilu_PushBatcher callable */

  /* L1 >= {self}; L2 unconstrained */

  ilu_boolean     bchr_lsrCares;
  ilu_integer     bchr_pushCount;	/* current Push activations */
  ilu_HashTable   bchr_conns;	/* (pushable ? conn -> same conn :
				 * NIL) */
  ilu_Vector      bchr_tmpVec;	/* if not NIL, available for any use */
};

typedef struct {
  /* See container for L1, L2 constraints. */
  ilu_Connection next, prev;	/* links in doubly-linked list */
} ilu_ConnLinks;

typedef struct ilu_replyCons *ilu_ReplyList;

struct ilu_replyCons {
  /* L1, L2 unconstrained */

  ilu_cardinal    rp_SN;
  ilu_refany      rp_queued;	/* from pr_delay_interp */

  /* L1 >= {connection's server} */
  ilu_ReplyList   rp_next;
};

struct _ilu_Connection_s {
  /*L1 >= {server}; L2 unconstrained*/

  ilu_Call        co_mucall;	/* the one holding my call mutex */
  ilu_Call        co_waiting;	/* call holding mutex, if held */
  unsigned        co_ioing:1;	/* implements iomu */
  unsigned        co_closed:1;
  unsigned        co_closing:1;	/* ASAP close and, if MT, enable
				 * waits */
  unsigned        co_pending:1;	/* in pending-close list */
  unsigned	  co_lastSNgood:1;
				/* value of co_last_sn valid --
				   should be relocated by first
				   caller to ilu_GetReply with SN
				   greater than co_last_sn */
  unsigned        co_lsrCares:1;/* kernel client using directly */
  unsigned        co_doomed:1;	/* don't use for any new calls */
  unsigned        co_pushme:1;	/* please push t'port ASAP */
  unsigned        co_pushAlarmSet:1;
  unsigned        co_callmuBorrowable:1;

  /*L1 unconstrained*/
  
  ilu_boolean     co_lastWorking;
  /*
   * Meaningful for serial protocols; true when I don't know that
   * the last call sent is done executing.
   */

  ilu_Protocol    co_protocol;
  union {
    ilu_string      co_peerinfo;/* always use "conn_peerinfo()" */
    ilu_TransportInfo co_tinfo;	/* always use "conn_tinfo()" */
  }               co_tinfo;	/* never access this field directly */
  /*
   * Never NIL.  Owned by conn.  For outoing connections, the
   * "tinfo" from which co_transport was created; for incoming
   * connections, a string (not necessarily a real "tinfo"), <
   * MAX_CCPEER_LEN long, identifying the peer for purposes of
   * replay detection.
   */
  ilu_string      co_pinfo;	/* never NIL; conn owns;
				 * outgoing => complete */
  ilu_Transport   co_transport;
  /*
   * The actual byte stream.  xmu = (self)'s I/O mutex; ymu =
   * (self)'s wait mutex.  When conn closed, the now-meaningless
   * pointer is retained, so protocols can detect when a call has
   * been switched to a different transport.  When co_closed or
   * co_closing change, (co_transport->tr_wc->iluwc_change) is
   * notified.
   */
  ilu_Port        co_port;	/* NIL on client side, points to
				 * port on server side */
  ilu_Passport    co_auth_info;	/* information about the identity of
				 * the caller */
  struct _ilu_IdentityInfo_s co_conn_identity;
  /* holds pointer to peerinfo info in identityinfo form */

  ilu_Server co_server;		/* never NIL */

  /*L2 >= {iomu}*/

  ilu_refany co_protocol_data;	/* NIL when closed */

  
  /*L1 >= {cmu}; L2 unconstrained*/

  ilu_Pipeline    co_pipeline;	/* non-NIL iff serial outgoing
				 * connection with co_nCalls>0  */

  /*L1 >= {server}; L2 unconstrained*/

  int             co_batchCount;/* number of batchers using me, roughly */
  ilu_FineTime    co_pushTime;	/* push then, iff co_pushAlarmSet */
  ilu_refany      co_pushAlarm; /* used to do timed pushes */

  /* The following two fields are meaningful if the protocol is
     concurrent and the connection is outgoing. */
  ilu_ReplyList   co_replies;	/* queue of unprocessed replies;
				 * empty when closed */
  ilu_Condition   co_cc;	/* MT: cond var for changes in
				 * co_closed, co_ioing, co_pushme, or
				 * co_mucall. */
  ilu_Serializer  co_serialer;	/* meaningful if outgoing */
  ilu_cardinal	  co_next_sn;	/* meaningful for outgoing connections;
				   the next request msg serial number */
  ilu_cardinal    co_last_sn;	/* SN of last request processed by
				 * other side -- meaningful iff
				 * co_lastSNgood=ilu_TRUE */
  ilu_integer	  co_nOuts;	/* number of calls between holding callmu
				   for request and for reply */
  ilu_integer     co_nWaits;	/* ST * active input-waits */
  ilu_integer     co_nCalls;	/* calls currently using self */
  ilu_integer     co_holds;	/* other reasons to not free */

  /*L1 >= {server} for [ilu_psl], which are for the doubly-linked list of
				the port's or server's connections;
   *L1 >= {cmu} for [ilu_reapable],	which are for the global
				doubly-linked LRU list of idle connections
				that _ilu_ReduceFdsTo can close;
   *L1 >= {cmu} for [ilu_otherIdle],	which are for the global
				doubly-linked LRU list of idle connections
				that _ilu_ReduceFdsTo can't close;
   *L1 >= {cmu} for [ilu_pcl],	which are for the global doubly-linked
				LRU list of pending-close connections. */
  ilu_ConnLinks co_links[4];	/* NIL-terminated, not circular */
  
  /*L1 unconstrained*/
  ilu_TIH_s       co_tih;	/* used for call.c:ReadExtraMsg &
				 * ilu_SetConnectionInputHandler */
};
/*
 * A protocol instance.  One end of a connection, at the protocol
 * level of abstraction.
 * 
 * Server mutex's invariant includes this statement: either L2 >=
 * {callmu} or co_nOuts>0 or co_nCalls>0 or co_lsrCares or
 * !co_closed or co_serialer or this ilu_Connection has been
 * ilu_free()d (or a locking problem prevented establishing this
 * condition).
 * 
 * Server mutex's invariant also includes: co_nOuts <= co_nCalls &&
 * co_nOuts >= 0 && co_nCalls >= 0.
 * 
 * A server's outgoing connections, in co_tih[ilu_psl], are kept in
 * reverse order of creation (youngest first).  This is so that
 * connections created as a consequence of a connection-scoped
 * relocation are used in preference to other connections.
 * 
 * A connection is idle iff it's open, its callmu isn't held, and its
 * nOuts=0.
 * 
 * Server mutex's invariant also includes: if conn is idle then conn is
 * in exactly one of ilu_reapable, ilu_otherIdle lists otherwise
 * conn is in neither list.
 */

typedef enum {
	ilu_psl,
	ilu_reapable,
	ilu_otherIdle,
	ilu_pcl
} ilu_ConnLinkKind;

typedef struct {
  char		cc_peerinfo[MAX_CCPEER_LEN]; /* Address of requestor */
  ilu_cardinal	cc_sn;
  ilu_Class	cc_intro_type;
  ilu_Method	cc_meth;
  ilu_Message	cc_replyMsg;
} ilu_CachedCall;

typedef struct {
  /* L1 >= {the server} */
  ilu_Port        pl_next, pl_prev;
}               ilu_PortLinks;

struct _ilu_Port_s {		/* where a server listens for
				 * connection requests */
  /* L1, L2 unconstrained */

  ilu_Server      po_server;	/* The server exported via this
				 * port. */
  ilu_string      po_pinfo;	/* given at create time; owned by
				 * port */
  ilu_Protocol    po_protocol;	/* protocol being used */
  ilu_refany	  po_prot_data;	/* inst. data assoc'd w this protocol */
  ilu_TransportInfo po_tinfo;	/* transport info for this port;
				 * owned by the port */
  ilu_Mooring     po_mooring;
  /*
   * Where this port listens.  xmu = (self)'s iomu, and ymu =
   * (self)'s waitmu.  Upon closure,
   * (po_mooring->mo_wc->iluwc_change) is notified (if
   * po_mooring->mo_wc exists).
   */

  ilu_TIH_s       po_tih;	/* for ilu_SetConnectionRequestHandler */

  /* L1 >= {server} */

  unsigned        po_closed:1;
  unsigned        po_closing:1;	/* close when my L2 mutexes not held */
  unsigned        po_disabled:1;/* mooring wait disabled */
  unsigned        po_ioing:1;	/* I/O mutex held? */
  unsigned        po_waiting:1;	/* wait mutex held? */
  unsigned        po_lsrCares:1;
  unsigned	  po_public:1;
  ilu_PortLinks   po_links;	/* ...in chain of server's ports */
  ilu_ConnLinks   po_connHead;	/* head of chain of conns from me */
  ilu_ConnLinks   po_closedConns;	/* ...but not freed */
  ilu_TransportCreator po_tcr;
  ilu_CachedCall *po_call_cache;/* non-NIL when transport unreliable */
  int             po_call_cache_size;	/* non-0 when transport
					 * unreliable */
  int             po_call_cache_finger;	/* cache is a FIFO; next
					 * goes here */
};
/*
 * po_disabled => po_closing.  ILU will get more responsive when we
 * make all mo_input and tc_input methods completely non-blocking,
 * which will enable us to eliminate the need for po_disabled.
 * Server mutex invariant includes this, for extant ports:
 * (!po_closed || po_lsrCares || po_ioing || po_waiting) &&
 * (po_ioing => po_waiting).
 */

struct _ilu_Server_s {
  /* L1, L2 unconstrained */

  ilu_Mutex       sr_lock;
  ilu_boolean     sr_true;	/* is this a true server? */

  /*
   * Ids of servers may have two different forms, their "plain"
   * form, in which any character except ((octet)0) may occur, and
   * their "encoded" form, in which any character aside from
   * alphanumeric and $-.+ is expressed as "%xx" where "xx" is the
   * hex form of code for the offending character.  The encoded form
   * is usually only seen in ILU URLs.
   */

  ilu_string      sr_id;	/* part of an oid; plain form */
  ilu_cardinal    sr_crc32;	/* CRC32 of the plain server ID */

  ilu_LanguageIndex sr_true_language;	/* index in language table */


  /* L1 >= {self} */

  /*
   * The next four fields are meaningful for outgoing connections.
   * They are derived from the first contact info known by
   * this surrogate server, and are used to contact the true server.
   * A true server's contact infos are kept in its ports.
   */

  ilu_TransportInfo sr_tinfo;	/* transport info; owned by server */
  ilu_TransportCreator sr_tcr;	/* from tinfo */
  ilu_string      sr_pinfo;	/* protocol info; owned by server */
  ilu_Protocol    sr_protocol;	/* from pinfo */

  ilu_CharBuf     sr_cinfo;	/* multiple, complete, encoded; public only */
  ilu_CharBuf	  sr_extCinfo;	/* for ports not animated here */
  ilu_private     sr_lsss[ILU_MAX_ADDR_SPACE_LANGUAGES];
  /*
   * Vector of slots in which to store Language-Specific Server
   * pointers, indexed by language-index
   */
  unsigned int    sr_holds;	/* Num. reasons kernel cares about
				 * me */
  unsigned        sr_closing:1;	/* ilu_BankServer sets this ilu_TRUE */
  unsigned        sr_cfails:1;	/* connect info suspect */
  ilu_ConnLinks   sr_connHead;	/* meaningful for surrogates: list
				 * of open connections */
  ilu_ConnLinks   sr_closedConns;	/* for surrogates: conns
					 * closed but not yet freed */
  ilu_PortLinks   sr_ports;	/* chain of ports usable for
				 * contacting this true server. */
  ilu_Port        sr_local_port;/* port for connection from this
				 * true server to itself */
  ilu_PortLinks   sr_closedPorts;	/* .. but not freed */
  ilu_HashTable   sr_objs;	/* maps ih -> ilu_Object, including
				 * singletons; goes NIL when closing
				 * && all objects deleted */
  ilu_HashTable   sr_singles;	/* maps singleton ilu_Class ->
				 * ilu_Object; goes NIL when closing
				 * && all objects deleted */
  ilu_ObjectTable sr_objtab;	/* significant in a true server; if
				 * not NIL, the app's chance to
				 * instantiate an object not found
				 * in objs */
  ilu_Port        sr_default_port;	/* default port; could be
					 * NIL */
  /* The following closure fields are provided to allow for
   * inetd-style relocation of servers. */
  ilu_ServerRelocateProc	sr_relocate_proc;
  ilu_private			sr_relocate_rock;
  /* XXX -- the rock is leaked when the server is destroyed... */
};
/*
 * A server's mutex invariant includes this: the server exists
 * while: (1) it has some (closed or otherwise) ports; (2) it has
 * some connections (closed or otherwise); (3) it has some objects;
 * or (4) it has some Language-Specific Server associated with it.
 * The local port goes into sr_closedPorts when it's closed.
 */

/*
 * Instance handles of objects may have two different forms, their
 * "plain" form, in which any character except ((octet)0) may occur,
 * and their "encoded" form, in which any character aside from
 * alphanumeric and $-.+ is expressed as "%xx" where "xx" is the hex
 * form of code for the offending character.
 */

struct _ilu_Object_s {
  /* L1, L2 unconstrained */

  ilu_string      ob_ih;	/* id wrt server; never NIL; plain */
  ilu_Server      ob_server;	/* never NIL */
  ilu_integer     ob_timeout;	/* num. sec.s after which to GC */
  ilu_Class       ob_class;
  /*
   * Can only be NIL when doing _ilu_FindClassViaRPC. No locking
   * requirements because ilu_ObjectOfSBH doesn't insert an object
   * in its server's hash table 'till after class is set.
   */
  ilu_string      ob_mstid;	/* unique_id of true object's most
				 * specific type (may be NIL); plain */
  /* L1 >= {server} */

  ilu_string      ob_sbh;	/* really a cache, lazily eval'ed;
				 * may be NIL at any time; encoded */
  ilu_private     ob_lspos[ILU_MAX_ADDR_SPACE_LANGUAGES];
  /*
   * Vector of slots in which to store LS pointers, indexed by
   * language-index
   */
  unsigned int    ob_intNoted:1;/* LSR thinks this about whether
				 * kernel is very interested in me */
  unsigned int    ob_holds:31;	/* Count of uses in root method
				 * stubs and gc.c. */
  union {

    struct {
      /* The following fields are meaningful only for coll. surrogate
       * objs. */

      ilu_boolean	ob_notifying;	/* we're calling true server now */
      ilu_boolean	ob_known;	/* when last !notifying, did true
					 * server know about this surrogate? */
    } ob_surrogate;

    struct {
      /* The following fields are meaningful only for collectible true
       * objs. */

      /* L1 >= {gcmu} */

      ilu_Alarmette_s ob_gco;	/* mxamu==gcmu */
      ilu_integer     ob_lastRemote;/* when we last knew the object to
				     * be loose in the network; not
				     * meaningful while gclist non-empty */
      ilu_Vector      ob_gclist;/* set of gc.c:counted_client*; NIL
				 * may represent the empty set */
    } ob_true;

  } ob_collectibleInfo;
};
/*
The string fields are owned by the object.

The kernel is very interested in this object if either:
* gclist not empty (for collectible true objects);
* gclist empty and now < lastRemote+timeout (for coll. true objects);
* notifying && !known (for collectible surrogates);
* holds != 0.

As long as either:
* gclist not empty (for collectible true objects);
* gclist empty and now < lastRemote+timeout (for coll. true objects);
* notifying (for collectible surrogates);
* holds != 0;
* lspo != NIL;
the kernel is "interested" in this object, the object is in the server's hash table, and the object is not freed.

The kernel needs to make a GC callback when
	known != (lspo != NIL || holds != 0).
*/


/* ================ Old ilu.h above ================ */
/* ================ Old iluntrnl.h below ================ */

#include <stdlib.h>
#include <errno.h>

#include "iludebug.h"

#define FREETOKEN(s) {if((s)!=NIL)ilu_free(s);}

#define PROTOCOL_TYPE(call)	(call->ca_connection->co_protocol->pr_type)

#define OBJECT_HASHTABLESIZE	113		/* number of different object slots */
#define SERVER_HASHTABLESIZE	53		/* number of different server slots */
#define CLASS_HASHTABLESIZE	53		/* number of different classes */

#define INITIAL_GCLIST_SIZE		5
#define INITIAL_GCDOBJECTSET_SIZE	200
#define INITIAL_GCCLIENTS_SIZE		20

#define GC_CALLBACK_CLASS_ID		"ilu.GCCallback"

/* ================ Internal Consistency Checking ================ */

/* L1, L2 unconstrained */

#undef ASSERT
#define ASSERT(flg,buf,spargs) {if (!(flg)) {char buf[1000]; sprintf spargs; _ilu_Assert(0, buf);}}

typedef struct {
  ilu_FailureConsumer fc;
  ilu_boolean     printMsg;
}               _ilu_FailureHandler;

extern _ilu_FailureHandler 
_ilu_FailureActionToConsumer(int fa, int which);
/*
 * Translates an integer code to the procedure it calls for.
 * which==0 for ilu_must_malloc, 1 for _ilu_Assert, and 2 for
 * ilu_Check.
 */

extern void     _ilu_ConsumeByLoop(const char *file, int line);
/* The result of _ilu_FaultActionToConsumer(-1, x) */


/* ================ Locking routines ================ */

/* We use simple mutual exclusion (ie, semaphores with only two
states).  When we get an error system, we may have a way of expressing
the interruption of an Acquire operation.  */

/*L1, L2 unconstrained*/

ilu_Mutex _ilu_CreateMutex( ilu_string d1, ilu_string d2 );
/*
 * The pair (d1, d2) describes the mutex; storage for strings owned
 * by caller.  Returns NIL on mem. alloc. failure.
 */

/*L1.sup < m before, L1.sup = m after*/
void _ilu_AcquireMutex( ilu_Mutex m );
/* Blocks until acquisition succeeds. */

/*L1 >= {m}*/
void _ilu_HoldMutex(ilu_Mutex m);
/* Checks that the caller holds the given mutex. */

/*L1 >= {m} before, L1 not >= {m} after*/
void _ilu_ReleaseMutex( ilu_Mutex m );
/* Releases held lock. */

#define _ilu_EnterServerMutex(s,h,e) \
	ilu_EnterServerMutexFull(s,h,e,__FILE__,__LINE__)

#define _ilu_ExitServerMutex(s,h,e) \
	ilu_ExitServerMutexFull(s,h,e,__FILE__,__LINE__)

#define _ilu_AcquireServerMutex(s)		\
	do {ilu_Error e;			\
	    _ilu_EnterServerMutex((s),ilu_FALSE,&e); \
	    ILU_MUST_BE_SUCCESS(e);}		\
	while (0)

#define _ilu_ReleaseServerMutex(s)		\
	do {ilu_Error e = ILU_INIT_NO_ERR;	\
	    _ilu_ExitServerMutex((s),ilu_FALSE,&e); \
	    ILU_MUST_BE_SUCCESS(e);}		\
	while (0)

extern ilu_boolean _ilu_CanCondition(void);

extern          ilu_Condition
_ilu_CreateCondition(ilu_string d1, ilu_string d2,
		     ILU_ERRS((no_memory, no_resources,
			       internal/check)) * err);
/*
 * Strings owned by caller.  Raises internal/check if not
 * multi-threaded.
 */

ILU_ERRS((CantCondition)) _ilu_NotifyCondition(ilu_Condition c);

extern          ilu_boolean
_ilu_CondDestroy(ilu_Condition c,
		 ILU_ERRS((internal)) * e);

extern void     _ilu_CommitThreadedness(void);
/*
 * Calling this indicates that threadedness can't change in the
 * future.
 */

extern ilu_boolean _ilu_lockTechDefined;
/*
 * Set when ilu_SetLockTech is finished.  
 */


/* ================ Synthesized Locking ================ */

/*L1 >= {conn's server}*/
/*L2 as implied by name, except where otherwise noted*/
/*Main unconstrained*/

/*L1 >= {port's server}*/
extern          ilu_boolean
_ilu_TakePortIO(ilu_Port port, ilu_boolean hard,
		ILU_ERRS((bad_locks, broken_locks)) * err);
/*
 * No thread holds port's I/O mutex; take it.  Returns ilu_TRUE iff
 * success; sets *err in all cases.
 */

/*L1 >= {cmu, port's server}; L1.sup < trmu*/
extern          ilu_boolean
_ilu_ReleasePortIO(ilu_Port port, ilu_boolean hard,
		   ILU_ERRS((bad_locks, broken_locks)) * err);
/*
 * Release port's I/O mutex.  On success, returns ilu_TRUE without
 * modifying *err.  On failure raises (hard ? broken_locks :
 * bad_locks) .
 */

/*L1 >= {port's server}*/
extern          ilu_boolean
_ilu_TakePortWait(ilu_Port port, ilu_boolean hard,
		  ILU_ERRS((bad_locks, broken_locks)) * err);
/*
 * At entry, waitmu not held by ANY thread.
 */

/*L1 >= {cmu, port's server}; L1.sup < trmu*/
extern          ilu_boolean
_ilu_ReleasePortWait(ilu_Port port, ilu_boolean hard,
		     ILU_ERRS((bad_locks, broken_locks)) * err);
/*
 * Release (port)'s waitmu; also close port if po_closing.
 * On success, returns ilu_TRUE without setting *err.  Returns ilu_FALSE on
 * failure, after setting *err if it's not already set for an error.
 */

/*L1 >= {conn's server}*/
extern          ilu_boolean
_ilu_TakeConnIO(ilu_Connection conn, ilu_boolean hard,
		ILU_ERRS((bad_locks, broken_locks)) * err);
/*
 * No thread holds conn's I/O mutex; take it.  Returns ilu_TRUE iff
 * success; sets *err in all cases.
 */

/*L1 = {conn's server, cmu}*/
extern          ilu_boolean
_ilu_EnterConnIO(ilu_Connection conn, ilu_boolean hard,
		 ILU_ERRS((bad_locks, broken_locks)) * err);
/*
 * Returns ilu_TRUE iff success.  On failure (not reliably detected),
 * rasies broken_locks if hard, bad_locks otherwise. Set hard when
 * kernel code ensures calling thread doesn't already hold the I/O
 * mutex.  Sets *err in all cases.
 */

/*L1 >= {cmu, conn's server}; L1.sup < trmu*/
extern          ilu_boolean
_ilu_ReleaseConnIO(ilu_Connection conn, ilu_boolean hard,
		   ILU_ERRS((bad_locks, broken_locks)) * err);
/*
 * Release I/O mutex, after closing the connection if its server is
 * closing.  On success, returns ilu_TRUE without modifying *err.  On
 * failure returns ilu_FALSE and, if (ILU_ERROK(*err)), sets (*err) to
 * (hard ? broken_locks : bad_locks).
 */

/**L1 = {cmu, conn's server}; Main Remnant holds; L2 >= {conn's iomu}*/
ilu_boolean
_ilu_PushAsNeeded(ilu_Connection conn,
		  ILU_ERRS((IoErrs, bad_locks)) * err);

/**L1 = {cmu, conn's server}; Main Remnant holds;
   before: L2 >= {conn's callmu, iomu};
   after:  L2 >= {conn's callmu}, L2 disjoint {conn's iomu}*/
extern          ilu_boolean
_ilu_PushAndReleaseConnIO(ilu_Connection conn, ilu_boolean hard,
			  ILU_ERRS((IoErrs, bad_locks)) * err);
/*
 * Release I/O mutex, after closing the connection if its server is
 * closing and pushing the connection while (co_pushme &&
 * ILU_ERROK(*err) && !conn->co_closed).  On success, returns ilu_TRUE
 * without modifying (*err).  If called with wrong mutexes held,
 * "raises" (hard ? broken_locks : bad_locks); may also "raise" I/O
 * errors.  "Raise" means to return ilu_FALSE and, if ILU_ERROK(*err),
 * set (*err).
 */

/* The following also shift conn into and out of the idle list. */

/* L1 = {conn's server, cmu} */
#define _ilu_EnterConnCall(conn,call,hard,err) \
 _ilu_FullEnterConnCallAndWait(conn,call,hard,ilu_FALSE,ilu_TRUE,ilu_FALSE,NIL,\
	err,__FILE__,__LINE__)
/*
 * Either enter (conn)'s call mutex.  Returns ilu_TRUE iff success.
 * (co_pushme) should be false on entry, and is left unchanged.  If
 * called inside wrong mutexes (not reliably detected), rasies
 * broken_locks if hard, bad_locks otherwise; raises internal if
 * called with (co_pushme) true.  Set hard when kernel code ensures
 * calling thread doesn't already hold the call mutex.  Sets (*err)
 * in all cases.
 * 
 * In a single-threaded runtime, an outgoing connection has a
 * monitoring input handler registered when its callmu isn't held
 * and nOuts == 0; callers of the callmu enter/exit procedures are
 * responsible for maintaining this invariant.
 */

/* L1 = {conn's server}; conn->co_nOuts > 0 */
#define _ilu_AltEnterConnCall(conn,call,hard,err) \
 _ilu_FullEnterConnCallAndWait(conn,call,hard,ilu_TRUE,ilu_TRUE,ilu_FALSE,NIL,\
	err,__FILE__,__LINE__)
/*
 * Returns ilu_TRUE iff success.  (co_pushme) should be false on entry,
 * and is left unchanged.  If called inside wrong mutexes (not
 * reliably detected), rasies broken_locks if hard, bad_locks
 * otherwise; raises internal if called with (co_pushme) true.  Set
 * hard when kernel code ensures calling thread doesn't already hold
 * the call mutex.  Sets *err in all cases.
 */

/* L1 = alt?{conn's server}:{conn's server, cmu} */
/**Before: L2 disjoint {conn's callmu, waitmu};
    After: L2 >= {conn's callmu};
	   L2 >= {conn's waitmu} iff !(qrl && *qrl)*/
extern          ilu_boolean
_ilu_FullEnterConnCallAndWait(ilu_Connection conn, ilu_Call call,
			      ilu_boolean hard, ilu_boolean alt,
			      ilu_boolean doCall,
			      ilu_boolean doWait,
			      ilu_ReplyList * qrl,
			      ILU_ERRS((bad_locks, broken_locks,
					internal)) * err,
			      const char *file, int line);
/*
 * When (doWait && qrl), (*qrl) is an INOUT parameter, and in the IN
 * direction may carry a reply or none; when !(doWait && qrl),
 * (*qrl) is unimportant.  If (doWait && qrl && !*qrl), as soon as
 * possible (if ever) fills in (*qrl) with a queued reply to
 * (call). As soon as possible simultaneously (1) if (doCall) then
 * enters (conn)'s callmu on behalf of (call) else noop, and (2) if
 * (doWait) then either (2a) (qrl && *qrl) or (2b) enters (conn)'s
 * waitmu on behalf of (call) else noop.
 */

#define _ilu_EnterConnCallAndWait(co,ca,h,a,dc,dw,q,e) \
	_ilu_FullEnterConnCallAndWait(co,ca,h,a,dc,dw,q,e,\
	__FILE__,__LINE__)

/**L1 = {cmu, conn's server}; Main Remnant holds;
   before: L2 >= {conn's callmu}, L2 disjoint {conn's iomu};
   after:  L2 disjoint {conn's callmu, iomu}*/
extern          ilu_boolean
_ilu_ReleaseConnCall(ilu_Connection conn, ilu_Call call,
		     ilu_boolean hard,
		     ILU_ERRS((IoErrs, bad_locks)) * err);
/*
 * On success, returns ilu_TRUE without setting (*err).  Pushes
 * connection while (conn->co_pushme && ILU_ERROK(*err) &&
 * !connection_closed(conn)).  On failure or bad call returns ilu_FALSE
 * after setting (*err) if it's not already set for an error.
 */

/*L1 >= {conn's server};
  conn->co_nOuts==0 => L1 >= {cmu}*/
extern          ilu_boolean
_ilu_QuickReleaseConnCall(ilu_Connection conn, ilu_Call call,
			  ilu_boolean hard,
			  ILU_ERRS((bad_locks, broken_locks,
				    internal)) * err);
/*
 * On success, returns ilu_TRUE without setting (*err).  Applicable only
 * if (connection_closed(conn) || ILU_ERRNOK(*err) ||
 * !conn->co_pushme).  If called otherwise or inside wrong mutexes,
 * returns ilu_FALSE after setting (*err) if it's not already set for an
 * error.
 */

/*L1 >= {cmu, conn's server}*/

extern void 
_ilu_FullTakeConnWait(ilu_Connection conn, ilu_Call call,
		      const char *file, int line);
/*
 * At entry, waitmu not held by ANY thread.  (call) is NIL when no
 * particular call involved (yet).
 */

#define _ilu_TakeConnWait(co,ca) \
	_ilu_FullTakeConnWait(co,ca,__FILE__,__LINE__)

/*L1 = {conn's server, cmu}*/

#define _ilu_EnterConnWait(co,ca,h,e) \
	_ilu_FullEnterConnCallAndWait(co,ca,h,ilu_FALSE,ilu_FALSE,ilu_TRUE,NIL,e,\
	__FILE__,__LINE__)

/*L1 >= {cmu, conn's server}; L1.sup < trmu*/

extern          ilu_boolean
_ilu_FullReleaseConnWait(ilu_Connection conn, ilu_Call call,
			 ilu_boolean hard,
			 ILU_ERRS((bad_locks, broken_locks)) * err,
			 const char *file, int line);
/*
 * Release (conn)'s waitmu; also close connection if co_closing.
 * On success, returns ilu_TRUE without setting *err.  Returns ilu_FALSE on
 * failure, after setting *err if it's not already set for an error.
 */

#define _ilu_ReleaseConnWait(co,ca,h,e) \
	_ilu_FullReleaseConnWait(co,ca,h,e,__FILE__,__LINE__)



/* ================ FD & Connection Management ================ */

/*L1 >= {cmu}*/
/*L2 unconstrained*/

extern ilu_integer ilu_fdbudget;	/* # FDs allowed */
extern ilu_integer ilu_fdstaken;	/* # FDs used (incl idle) */

#define ilu_DeltaFD(n)	ilu_FullDeltaFD((n),__FILE__,__LINE__)

extern void     ilu_FullDeltaFD(ilu_integer n, char *file, int line);
/* Call this to change ilu_fdstaken, by n. */

/*L1.sup = cmu (but not really "inside"); Main Remnant holds*/
extern          ilu_boolean
_ilu_ReduceFdsTo(ilu_integer goal,
		 const ilu_FineTime * timeout,	/* absolute, optional */
		 ILU_ERRS((bad_locks, broken_locks,
			   internal)) * err);
/*
 * Close idle connections until ilu_fdstaken <= goal or we run out
 * of idle connections and time.  (timeout==NIL) means the timeout
 * is infinite.  Exits and re-enters cmu.  Some idle connections may
 * have been closed even if an error is raised; some idle
 * connections may finish closing after return.  Returns
 * ILU_ERR(*err).
 */

/*L1, L2 unconstrained*/
ilu_Closure _ilu_ClosureFromTIH(ilu_TIH tih);

/*Main Invariant holds; L2 otherwise unconstrained*/
extern void     _ilu_InvokeTIH(int, ilu_refany);
/*
 * First arg is ignored; second arg should really be an ilu_TIH;
 * this proc invokes it.  Weakening the locking precondition from
 * that of ilu_TransportInputHandler to that of ilu_ClosureProc
 * seems to work out in this case (why?).
 */

/*L1.sup < timu; L2 unconstrained*/

ILU_PUBLIC ilu_boolean 
_ilu_FinishInputSource(int fd);
/*
 * Calls ilu_UnregisterAndReturnInputSource, and then ilu_DoSoon on
 * the formerly-registered handler, if any.
 */

ILU_PUBLIC ilu_boolean 
_ilu_FinishOutputSource(int fd);


/* ================ iluxport.h counterparts ================ */
/* These procedures are like their similarly-named iluxport.h
 * counterparts, except that they require different mutexes to be held.
 */

/**L1     =    {cmu, conn's server}; Main Remnant;
   L2    >=    {conn's waitmu};
   L2 disjoint {conn's iomu}*/
extern          ilu_boolean
_ilu_BlockingWaitForInputOnConnection(ilu_Connection conn,
				      ilu_FineTime * limit,
				      ilu_boolean retToPush,
				      ILU_ERRS((broken_locks,
						interrupted,
						internal)) * err);
/*
 * Returns ILU_ERROK(*err) after either: (1) there is a decent
 * reason to suspect that "input progress" can be made, or an error
 * or EOM or EOF can be detected, without blocking, or closure of
 * this connection has already been requested (i.e., co_closing is
 * set), (2) *limit passed (limit==NIL means *limit == +infinity),
 * (3) interrupt requested in a multi-threaded runtime (in which
 * case interrupted is raised), (4)
 * tc_interruptST(conn->co_transport, ..) was called in a
 * single-threaded runtime (in which case interrupted is *not*
 * raised), or (5) the runtime is multi-threaded, waiting is
 * disabled for (conn)'s transport's wait-cohort (see
 * tc_disableWait), (retToPush), and (conn->co_pushme). "Input
 * progress" means any advancement in the state of any stage in the
 * input pipeline --- regardless of whether any bytes dribble out
 * this end.  Used in single-threaded and multi-threaded runtimes.
 * In S-T R/T, blocks by running main loop; in M-T, blocks only the
 * calling thread. Caller ensures that (conn) is not closed at entry
 * time; callee ensures (conn) is not closed at return time.
 */

/*L1 >= {conn's server}; (L1-{conn's server}).sup = cmu*/
/*L2 disjoint {conn's iomu, callmu, waitmu}*/

extern          ilu_boolean
_ilu_InnerSetConnectionInputHandler(ilu_Connection conn,
				ilu_TransportInputHandler tih_proc,
				    ilu_refany tih_rock,
				    ILU_ERRS((no_memory, internal,
					   no_resources, bad_param,
					      bad_locks)) * err);

/*L1 >= {port's server}; L2 unconstrained*/

void _ilu_ClearPortFromServer(ilu_Port port, ilu_Server s);
/* Unlink this port from its server, s.
   Called when port is closing and last connection is closed. */

ilu_boolean _ilu_ServerEmptyP (ilu_Server s);
/* True if no objects in server, and no object table defined. */

/*L1 >= {cmu, port's server}, L1.sup < trmu; L2 unconstrained*/
void _ilu_ClosePort(ilu_Port port);

/*L1 >= {cmu, conn's server}; L1.sup < trmu*/
/*L2 >= {conn's iomu}*/
extern void _ilu_CloseIoingConnection(ilu_Connection conn,
				      ilu_boolean set_cfails,
				      ilu_ConnShutdownReason);
/*
 * Usable on either incoming or outgoing connections.  For a
 * connection of a surrogate server, set_cfails if you suspect the
 * server's connect info is bad.  If (conn)'s waitmu is held (by any
 * thread), this proc sets co_closing but doesn't actually close.
 */


/*L1 >= {conn's server, cmu}; L2 disjoint {conn's callmu, iomu}*/
extern ILU_ERRS((bad_locks, broken_locks))
_ilu_CloseConnection(ilu_Connection connection, ilu_ConnShutdownReason);
/* Usable on either incoming or outgoing connections. */


/*Main Invariant, L2 >= {conn's iomu}*/
extern ilu_boolean 
_ilu_CloseConnWithIo(ilu_Connection conn, ilu_boolean set_cfails,
		     ilu_ConnShutdownReason, ILU_ERRS((IoErrs)) *err);
/*
 * ... where kernel code guarantees that cmu, conn's server mutexes
 * not held.  (*err) is left untouched on success, set on failure if
 * the failure is more pressing than (*err) indicated upon entry.
 */

/* L1 >= {conn's server}; L2 disjoint {conn's callmu, iomu} */
extern void _ilu_MaybeFreeConnection(ilu_Connection conn);
/*
 * Call this before exiting server mutex after establishing
 * !co_nOuts && !co_nCalls && !co_lsrCares && co_closed &&
 * !co_serialer && !co_waiting && !co_mucall && !co_ioing, to
 * ilu_free(conn); may also call when this condition doesn't hold,
 * in which case nothing is done.
 */

/* L1 >= {port's server}; L2 unconstrained */
extern          ilu_boolean
_ilu_MaybeFreePort(ilu_Port port,
		   ILU_ERRS((internal)) * err);
/*
 * Call this before exiting server mutex after establishing
 * !po_lsrCares && po_closed && !po_waiting && !po_ioing &&
 * !port_connections(port), to ilu_free(port); may also call when
 * this condition doesn't hold, in which case nothing is done.
 */


/* ================ Disorganized ================ */


/*L1 >= {server}; L2 unconstrained */
extern ilu_ReplyList _ilu_GetQueuedReply(ilu_Call call);

/* L1_sup < otmu */
extern ilu_Class _ilu_StringToClass(ilu_string);
/* Inspect a string constructed by "_ilu_ClassToString"
 * and return the most specific object type in that string known
 * in this address space.  Returns NIL if no object type named
 * in "s" is registered in this address space.
 */

/* L1_sup < otmu */
extern ilu_string _ilu_ClassToString(ilu_Class);
/* Form a string which lists the type ID for the specified
 * object type, and the type IDs for all of its supertypes,
 * and all of their supertypes, etc., in a regular format
 * so that information about the type inheritance hierarchy
 * is preserved.  Returns a newly malloc'ed string if
 * successful; returns NIL to signal an error.
 */

/* L1_sup < otmu */
extern ilu_string _ilu_MSTIDToStringifiedDAG(ilu_string /* MSTID */);
/* Form a string which lists the type ID for the specified
 * object type, and the type IDs for all of its supertypes,
 * and all of their supertypes, etc., in a regular format
 * so that information about the type inheritance hierarchy
 * is preserved.  Returns a newly malloc'ed string if
 * successful; returns NIL if the type both not known
 * in this address space, and not in the list of type
 * graphs for `unknown' types maintained by the kernel.
 */

/* Locking:  L1 >= {otmu} */
extern void _ilu_RegisterBuiltInTypes(void);
/* provided in generated file ilutpcod.c, this causes all the
   primitive types to be registered. */

/*L1 >= {daimu}; L2 unconstrained*/

extern struct _ilu_DefaultAlarm_struct _ilu_gcoDefaultAlarm_s,
                _ilu_gccDefaultAlarm_s, _ilu_iotDefaultAlarm_s,
                _ilu_grDefaultAlarm_s, _ilu_udpDefaultAlarm_s,
                _ilu_soonDefaultAlarm_s, _ilu_cvtoDefaultAlarm_s;
/*
 * The default implementations of the six alarms used by the
 * kernel.  Note that this header file doesn't define struct
 * _ilu_DefaultAlarm_struct; those details can be private to the
 * default alarm implementation (apparently ANSI C doesn't mind
 * declarations of partially typed variables, at least as long as
 * only the address of such a variable is used in code to which the
 * type is only partial).
 */

/* L1??? */

extern ilu_SignalCallbackHandler _ilu_SignalCallbackHandler;
/* This function pointer is checked when the main loop is
   interrupted by a signal.  If non-NULL, the function
   is invoked. */

extern ilu_refany		 _ilu_SignalCallbackHandlerArg;
/* Passed as arg to _ilu_SignalCallbackHandler when invoked */

/*L1.sup < trmu*/
void _ilu_HandleSigPIPE(void);
/* Called to set up signal halder for SIGPIPE before attempting
   read or write or connect.  Checked with ilu_SIGPIPE_Handled. */

/*L1, L2 unconstrained*/

extern ilu_MainLoop _ilu_DefaultMainLoop;
/* The one that's used unless otherwise specified. */

void _ilu_AutoSetDebugLevel (void);

void _ilu_debug_DumpPacket (ilu_byte * /* packet */,
			    ilu_cardinal /* length */,
			    ilu_string /* packet type - optional, retained */);

void _ilu_debug_DumpPacket_Offset (ilu_byte * /* packet */,
				ilu_cardinal /* length */,
				ilu_cardinal /* offset */,
				ilu_string /* packet type - optional, retained */);
/* Reported indices are incremented by offset. */

#define _ilu_debug_DumpPacket(p,l,t) _ilu_debug_DumpPacket_Offset(p,l,0,t)

ilu_cardinal _ilu_SafeStrlen (ilu_string s);

void _ilu_FreeToken (ilu_refany token);

extern ilu_string _ilu_Strdup(const char * /* str */ );

#define _ilu_Strdup(s)		_ilu_full_Strdup((s),__FILE__,__LINE__)

extern          ilu_string
_ilu_full_Strdup(const char * /* str */ ,
		 const char * /* filename */ ,
		 int /* line number */ );

extern          ilu_string
_ilu_StringifyTinfo(ilu_TransportInfo,
		    ILU_ERRS((no_memory, internal/check)) *);
/* returned string malloced by callee, owned by caller */

extern ilu_cardinal _ilu_TinfoStringLength(ilu_TransportInfo);
/* Returns length of string form (excluding trailing 0). */

extern          ilu_string
_ilu_StringifyTinfoToBuffer(ilu_TransportInfo t,
			    ilu_string b, ilu_cardinal blen,
			    ILU_ERRS((internal/check)) * err);
/*
 * Write stringified TInfo into given buffer.  Trailing 0 not
 * written.  Return next char position, or NIL if err (not enough
 * space).
 */

extern ilu_boolean 
_ilu_CompareTinfo(ilu_TransportInfo,
		  ilu_TransportInfo);
/* returns ilu_TRUE if tinfos are equal */

extern ilu_TransportInfo 
_ilu_ConcatTinfo(ilu_string,	/* retained, required */
		 ilu_TransportInfo,	/* retained, required */
		 ILU_ERRS((no_memory)) *);
/*
 * Creates a new tinfo with the string at the beginning, and returns
 * it.
 */

extern ilu_TransportInfo 
_ilu_CopyTinfo(ilu_TransportInfo,
	       ILU_ERRS((no_memory)) *);
/* creates and returns a copy of the arg */

#ifdef ENABLE_DEBUGGING
extern void _ilu_PrintTinfo (ilu_TransportInfo);
     /* prints the tinfo using ilu_DebugPrintf */
#endif /* ENABLE_DEBUGGING */

ilu_string _ilu_Hostname (void);
     /* returns pointer to static string giving name of host machine */

ilu_integer _ilu_atoi (ilu_string buf, /*OPTIONAL*/ ilu_string * nextp);
     /* returns int read from buf, and if nextp is non-NIL,
	returns pointer to next non-number char in buf.
	Handles explicit bases like 0xff3, or 0303, or 0d129,
	or 0b11001101010, and uses strtol(). */

extern int      _ilu_casefree_cmp(const ilu_string, const ilu_string);
/* returns 0 if s1 == s2, -1 if s1 < s2, 1 if s1 > s2 */

extern int 
_ilu_casefree_ncmp(const ilu_string, const ilu_string,
		   ilu_cardinal n);
/* Compares at most n chars. */

ilu_string _ilu_Strcat3(const ilu_string s1, const ilu_string s2,
			const ilu_string s3);
/* Returns s1+s2+s3, in fresh storage; NIL may be used to input
 * an empty string.  Result will be NIL only if malloc fails. */

ilu_string _ilu_Strcat5(const ilu_string s1, const ilu_string s2,
			const ilu_string s3, const ilu_string s4,
			const ilu_string s5);
/* Like _ilu_Strcat3, only more parts. */

/*L1 >= {trmu}; L2 unconstrained*/
extern          ilu_string
_ilu_CurrentHostIPAddrStringOnly(ILU_ERRS((IoErrs)) * err);

extern          ilu_boolean
_ilu_ParseConnectInfo(ilu_string encodedContactInfo,
		      ilu_cardinal encodedContactInfoLen,
		      ilu_string * plainProtocolInfo,
		      ilu_TransportInfo * /* tinfo */ ,
		      ILU_ERRS((no_memory, inv_objref,
				internal)) * err);
/*
 * Parse first contact info in given sequence.  Returns ilu_TRUE iff
 * parse successful.  Stores through non-NIL pointer arguments;
 * storage ownership of strings and ilu_TransportInfo is returned to
 * caller iff successful.
 */

/*L1.sup < trmu, L2 unconstrained */
ilu_boolean _ilu_CheckTransportInfo (ilu_TransportInfo /* tinfo */,
				     ILU_ERRS((bad_param)) *err);
						   

/**Main Remnant Holds;
   before:           L1 = {};
   after:  result => Inside(result, st);
   after: !result => L1 = {}*/
ilu_Server
_ilu_FindAndEnterServer(ilu_string serverid, ilu_boolean add,
			ilu_string cinfo, ilu_cardinal cinfolen,
			ilu_Class st,
			ILU_ERRS((BadProtocolInfo, internal,
				  no_memory, inv_objref)) * err);
/*
 * Looks for the server with the given id.  If (add) and not found,
 * adds a new surrogate server of the given name and contact info.
 * (cinfo)'s length is (cinfolen); (cinfo) is encoded, not plain,
 * and is not necessarily NULL-terminated. Caller owns (serverid)
 * and (cinfo).  For non-null results, also does ilu_EnterServer.
 */

/* L1 > {s} */
int
_ilu_ServerLSSCount (ilu_Server /* s */);
/*
 * Returns the number of LSS's associated with this ilu_Server.
 */

/*L1, L2 unconstrained*/

/**before: Inside(s, static_type)
   after:  result!=NIL => Inside(s, static_type);
   after:  result==NIL => L1 = {};
   Main Remnant holds*/
ilu_Object
_ilu_FindOrCreateObject(ilu_string /* ih (retain) */,
			ilu_Server /* s */,
			ilu_Class /* found_class (optional) */,
			ilu_Class /* static_type */,
			char * /* mstid (optional,retain) */,
			char * /* sbh (optional,retain) */,
			ILU_ERRS((inv_objref)) * /* err */);
/*
 * Looks for object in s with the given ih.  If not there, and the
 * server is not true, creates new surrogate object.  If
 * (found_class) is not specified, does RPC to object to determine
 * appropriate type.  If (sbh) is specified, copies it to object's
 * SBH cache.  If (mstid) is specified, will check object's type
 * against it to detect type clashes.  If result!=NIL &&
 * ilu_GetLanguageSpecificObject(result)==NIL, the caller must
 * invoke ilu_RegisterLanguageSpecificObject or ilu_DeltaHolds on
 * the result before unlocking the server.
 */

/**Call-Locking(call, IHi) before,
   Call-Locking(call,  No) after*/
void _ilu_HandlePing (ilu_Call call);
/* Handles the built-in Ping method. */

/*L1 >= {cmu, server}; L1.sup < trmu*/
extern 
ILU_ERRS((bad_locks, broken_locks, internal))
_ilu_ServerRemoveObject(ilu_Server s, ilu_Object obj);

/*Inside(server, result's type)*/
ilu_Object _ilu_FindObjectInServer(ilu_string ih, ilu_Server s);

/*L1 >= {s}*/

ilu_boolean _ilu_Addable(ilu_Server s, ilu_Class t, ilu_Object *h);

void _ilu_AddSingleton(ilu_Server s, ilu_Class t, ilu_Object o);

extern          ilu_boolean
_ilu_CacheCall(ilu_Call call, ilu_Message * reply,
	       ILU_ERRS((internal)) * err);
/*
 * Called from a Protocol's finish_reply and _exception methods to
 * cache the reply.  Caller relinquishes ownership of
 * *reply->msg_base, but retains ownsership of *reply.
 */

/*L1.sup = s; L1 >= {cmu}*/
/*L2 unconstrained*/

extern          ilu_Connection
_ilu_CreateConnection(ilu_Transport bs,	/* pass */
		      ilu_TransportInfo tinfo,	/* retain */
		      ilu_string peerinfo,	/* retain */
		      ilu_Protocol pr,
		      ilu_string pinfo,	/* retain */
		      ilu_Port port,
		      ilu_Server s,
		      ilu_Passport,	/* optional, pass */
		      ILU_ERRS((no_memory, internal/check)) * err);
/*
 * Used internally to create both incoming and outgoing connections.
 * bs, pr, pinfo, and s aren't NIL; port is NIL for outgoing
 * connections, non-NIL for incoming ones.  tinfo is real tinfo of
 * peer when outgoing, but NIL when incoming.  peerinfo is NIL when
 * outgoing, but some arbitrary identifying string when incoming.
 * String args owned by caller.  Result's mutexes are not held by
 * any thread.  Caller is responsible for staying within FD budget.
 * Sets *err for return from kernel interface.
 */

/*L1 >= {cmu}; L2 unconstrained*/

extern ilu_Condition _ilu_connHandoffChange;
extern ilu_integer _ilu_connCount;	/* num connections open */
extern ilu_Condition _ilu_connCountChg;

/*L1.sup < cmu; L2 unconstrained*/

extern          ilu_boolean
_ilu_HandOffNewConnection(ilu_Connection conn,
			  ILU_ERRS((bad_locks, broken_locks,
				    internal)) * err);
/*
 * On success, returns ilu_TRUE without modifying (*err).  On failure,
 * returns ilu_FALSE, after setting (*err) if it had contained success.
 * May raise one of bad_locks, broken_locks, internal.
 */

/*L1, L2 unconstrained*/

extern ilu_refany _ilu_ioTimeoutAlarm;

extern ilu_refany _ilu_grAlarm;		/* for use in ilu_GetReply */
extern ilu_refany _ilu_udpAlarm;	/* for use in udp.c */
extern ilu_refany _ilu_soonAlarm;	/* for use in ilu_DoSoon */
extern ilu_refany _ilu_cvtoAlarm;	/* for use in ilu_CMWait2Full */


#ifdef UDPSOCKET_TRANSPORT
void _ilu_udp_SetTimeouts (double to1, double toN, double tto);
#endif

/*L1.sup < trmu*/

ilu_TransportCreator 
  _ilu_GetTransportCreator(ilu_TransportInfo tinfo,
			   ILU_ERRS((no_memory, inv_objref)) * err);
/* tinfo is owned by caller, and is syntactically complete. */

/*L1.sup < prmu*/

ilu_Protocol _ilu_GetProtocolFromInfo (ilu_string pinfo);
/* Caller owns the string arg. */

/*Main Invariant holds*/

void
_ilu_WaitForInputOnFD(int fd, ilu_boolean * sure,
		      ilu_FineTime * limit,
		      ILU_ERRS((interrupted)) * err);
void 
_ilu_WaitForOutputOnFD(int fd, ilu_boolean * sure,
		       ilu_FineTime * limit,
		       ILU_ERRS((interrupted)) * err);
/*
 * These two procedures return some time after one of the following
 * conditions has become true: (1) the appropriate kind
 * of I/O can be done on the given file descriptor without blocking,
 * (2) an exceptional condition exists on the FD, (3) *limit has
 * arrived, (4) (ST only:) _ilu_InterruptFD(fd,..) has been called, 
 * (5) (MT input waiting only:) _ilu_DisableFDWaits has been called
 * more times than _ilu_EnableFDWaits, or (6) the
 * implementation feels like it.  NIL may be passed for limit, in
 * which case *limit is effectively +infinity.
 * 
 * In a multi-threaded program, these procedures block only the calling
 * thread.  *sure is set, but not read.  When *sure is set true, (1),
 * (2), or (5) was the cause.  When *sure is set false,
 * any of (1), (2), (3), (5), or (6) may hold.  In case 6, the
 * implementation may raise interrupted, indicating that something
 * has asked the thread to interrupt its current RPC.  There's no
 * guarantee about what happens if there's a concurrent call that
 * closes the FD (i.e., don't do that).
 * 
 * In a single-threaded program, these procedures process input on
 * other FDs and the alarm while waiting.  This processing may lead
 * to a nested call on one of these procedures, with the same or a
 * different FD.  When I/O is finally enabled on an FD, all nested
 * calls waiting on the same FD are unblocked; *sure is set ilu_TRUE for
 * the innermost call, ilu_FALSE for the others.  After calling one of
 * these procedures, the caller proceeds to do I/O.  When a socket
 * reaches EOF, reads will return 0 bytes (but no error will be
 * indicated).  The calling code can conclude it is seeing EOF only
 * if the first read after WaitForInputOnFD returns 0 bytes and
 * *sure was set to true.
 * Can FD be closed during a ST wait?
 * 
 * Use of _ilu_InterruptFD does not cause interrupted to be raised.
 */

/*L1, L2 unconstrained*/
ilu_boolean
_ilu_InterruptFD(int fd, ILU_ERRS((bad_param)) * err);
/*
 * Interrupt all current waits on I/O on the given FD.  Applicable
 * only when single-threaded; raises bad_param/threading otherwise.
 * For use by transports in implementing their interrupt methods,
 * which are just one low-level part of the scheme for interrupting
 * calls.
 */

/* L1 >= {cmu}; L1.sup < trmu; L2 unconstrained */

extern ilu_boolean 
_ilu_DisableFDWaits(ILU_ERRS((broken_locks,
			      internal)) * err);
extern ilu_boolean 
_ilu_EnableFDWaits(ILU_ERRS((broken_locks,
			     internal)) * err);
/*
 * These two procedures affect the behavior of the I/O wait procs
 * above, and call _ilu_DeltaCohortWaits() on the FD wait cohort
 * exposed below.
 */

/*L1.sup < cmu; L2 unconstrained*/
extern          ilu_WaitCohort
ilu_GetFDWaitCohort(ILU_ERRS((no_memory, no_resources, broken_locks,
			      bad_locks, internal)) * e);
/*
 * Returns NIL iff single-threaded or error.  Otherwise returns the
 * ilu_WaitCohort for transports/moorings that wait on FDs.
 */

/* L1 >= {cmu}; L2 unconstrained */
extern          ilu_WaitCohort
_ilu_GetFDWaitCohort(ILU_ERRS((no_memory, no_resources,
			       internal)) * e);
/* Like ilu_GetFDWaitCohort, but with different locking. */

/*L1, L2 unconstrained*/
extern          ilu_cardinal
_ilu_NbSockRead(int fd, ilu_byte * buf, ilu_cardinal bufLen,
		ilu_TransportReport * rpt,
		ILU_ERRS((internal / errno)) * err);
/*
 * A thin veneer on one of the underlying OS's "read from socket"
 * operations; this veneer iterates until the system call is not
 * interrupted. In particular, this procedure does not block trying
 * to fill the buffer; instead it just returns the number of bytes
 * read. Will return 0 if no bytes can be read without blocking.
 * Sets rpt->tr_eof iff EOF detected.  Raises only internal/errno,
 * for impossible errnos.
 */

/*L1, L2 unconstrained*/
extern          ilu_cardinal
_ilu_NbSockWrite(int fd, ilu_byte * buf, ilu_cardinal nbytes,
	       ILU_ERRS((comm_failure / conn_lost,
			 internal / errno)) * err);
/*
 * A thin veneer on one of the underlying OS's "write to socket"
 * operations; this veneer iterates until the system call is not
 * interrupted.  In particular, this procedure does not block;
 * instead it just returns the number of bytes successfully written.
 * May raise comm_failure/conn_lost or internal/errno.
 */

/*Main Invariant holds; L2 not further constrained*/
extern          ilu_boolean
_ilu_SockWrite(int fd, ilu_byte * buf, ilu_cardinal nbytes,
	       ILU_ERRS((comm_failure / conn_lost,
			 internal / errno)) * err);
/*
 * This is like UNIX send(), except that it iterates over multiple
 * system calls, blocking only the calling thread, until the entire
 * transfer has been completed or has failed.  In single-threaded
 * runtimes, caller will want to ensure there'll be no nested calls
 * to the same procedure with the same FD.  May raise
 * comm_failure/conn_lost or internal/errno.
 */

/*Inside(obj's server, obj's type)*/
/*Main Remnant holds*/

extern
ILU_ERRS((GcRegFailed, bad_locks, broken_locks, internal))
_ilu_DeltaHolds(ilu_Object obj, ilu_integer dholds);

extern
ILU_ERRS((internal, GcRegFailed,
	  bad_locks, broken_locks))
_ilu_VIUpdate(ilu_Object obj);
/*
 * Call this after holds or gclist's emptiness changes, or
 * lastRemote+timeout passes, and you're ready to have the server
 * invariant restored. L1 mutexes are exited and re-entered inside
 * this procedure!
 */


/****************************** from type.c ********************/

/*L1, L2 unconstrained */

extern const ilu_Class _ilu_rootClass;
/* The one with all the methods every object supports */

extern ilu_Method _ilu_GetTypesMethod;
extern ilu_Method _ilu_RegisterGCInterestMethod;
extern ilu_Method _ilu_UnregisterGCInterestMethod;
extern ilu_Method _ilu_PingMethod;
/* The methods of _ilu_rootClass */

/*L1.sup < otmu*/
ilu_Class _ilu_FindMSKA(ilu_string tid);
/* Returns the one most specific known ancestor of the type identified
   by the given string; returns NIL if that's not well-defined, or we
   don't yet know about the ancestry of the given type. */

/*L1.sup < otmu*/
void _ilu_EnumerateClasses (void (*proc) (ilu_Class, ilu_refany rock), ilu_refany rock);
/* Calls "proc" on every registered class, passing the class and "rock".
   The order in which the classes are supplied is not specified. */

/*Main Invariant holds; L2 otherwise unconstrained*/

ilu_Class _ilu_FindClassViaRPC (ilu_Object o);
/* o->ob_class is temporarily set to some known type of o */

/*L1, L2 unconstrained */

ilu_string	/* result, NUL-terminated malloced string, PASS */
  _ilu_EncodeBuffer (ilu_string,	/* buffer, RETAIN */
		     ilu_cardinal,	/* length of buffer */
		     ILU_ERRS((no_memory)) *);

ilu_string	/* result, NUL-terminated malloced buffer, PASS */
  _ilu_DecodeBuffer (ilu_string,		/* buffer, RETAIN */
		     ilu_cardinal,	/* size of buffer */
		     ilu_cardinal *,	/* number of chars in output */
					/*  (not counting terminal NUL) */
		     ILU_ERRS((inv_objref, no_memory, internal)) *);

#ifdef IIOP_PROTOCOL

/*Main Invariant holds; L2 otherwise unconstrained*/

ilu_Class _ilu_IIOP_FindClassViaRPC(ilu_Object o);
/* o->ob_class is temporarily set to some known type of o */

/*L1, L2 unconstrained*/

extern ilu_boolean _ilu_IIOP_ParseIIOP (ilu_string, ilu_string *, ilu_string *, ilu_string *, ilu_string *, ilu_cardinal *, ilu_boolean *, ilu_Error *);
extern ilu_boolean _ilu_IIOP_ParseIIOPLoc (ilu_string, ilu_string *, ilu_string *, ilu_string *, ilu_string *, ilu_cardinal *, ilu_boolean *, ilu_Error *);
extern ilu_boolean _ilu_IIOP_ParseIIOPName (ilu_string, ilu_string *, ilu_string *, ilu_string *, ilu_string *, ilu_cardinal *, ilu_boolean *, ilu_Error *);
extern ilu_boolean _ilu_IIOP_ParseIOR (ilu_string, ilu_string *, ilu_string *, ilu_string *, ilu_string *, ilu_cardinal *, ilu_boolean *, ilu_Error *);
extern ilu_boolean _ilu_IIOP_ParseIOR2 (ilu_string, ilu_string *, ilu_string *, ilu_string *, ilu_string *, ilu_cardinal *, ilu_boolean *, ilu_Error *);

/*L1, L2, Main unconstrained (this is only for calling from debugger)*/
ilu_cardinal _ilu_IIOP_SetMaxStringSize (ilu_cardinal size);

#endif


#ifdef HTTP_PROTOCOL
extern ilu_boolean _ilu_Parse_HTTP_URL(ilu_string, ilu_string *, ilu_string *, ilu_string *, ilu_string *, ilu_cardinal *, ilu_boolean *, ilu_Error *);
#endif

#ifdef W3NG_PROTOCOL
extern ilu_boolean _ilu_w3ng_ParseURL(ilu_string, ilu_string *, ilu_string *, ilu_string *, ilu_string *, ilu_cardinal *, ilu_boolean *, ilu_Error *);
#endif

/**Call-Locking(call, IHi) before,
   Call-Locking(call,  No) after*/
void _ilu_HandleGetTypes (ilu_Call call);

/* L1 > otmu */
/* To be used inside an _ilu_EnumerateClasses enumeration */
ilu_boolean 
_ilu_IsSubObjectType(ilu_Class a, ilu_Class b);
/* Returns ilu_TRUE iff a is a subtype of b
   (including the degenerate case of a=b). */

/********************* from pickle2.c and pickle3.c */

/* Locking unconstrained */

#if (defined(ADD_PICKLE2_SUPPORT))

ILU_PUBLIC struct _ilu_Connection_s _ilu_pickle2_Format;

ILU_PUBLIC ilu_boolean
  _ilu_pickle2_StartPickle (ilu_Call_s *,	/* Call struct to init */
			    ilu_Type,		/* constraint type, if any */
			    ilu_Error *);	/* error return */
/* see ilu_StartPickle() */

ILU_PUBLIC ilu_boolean
  _ilu_pickle2_WritePickle (ilu_Call,
			    ilu_cardinal /* argSize, HINT -- can be zero */,
			    ilu_string	/* type_id, IN, RETAIN */,
			    ilu_Error *	/* err */);
/* see ilu_WritePickle() */

ILU_PUBLIC ilu_string	/* OUT, belongs to "pickle" arg */
  _ilu_pickle2_PickleType (ilu_Pickle	/* pickle, IN, RETAIN */,
			   ilu_Error *	/* error */);
/* See ilu_PickleType() */

ILU_PUBLIC ilu_TypeKind
  _ilu_pickle2_PickleTypeKind (ilu_Pickle	/* pickle, IN, RETAIN */,
			       ilu_Error *	/* error */);
/* See ilu_PickleTypeKind() */

ILU_PUBLIC ilu_boolean
  _ilu_pickle2_PickleTypes (ilu_Pickle,
			    ilu_string **,
			    ilu_cardinal *,
			    ilu_Error *	/* error */);
/* See ilu_PickleTypes() */

ILU_PUBLIC ilu_boolean
  _ilu_pickle2_ReadPickle (ilu_Call /* call */,
			   ilu_Pickle /* pickle, IN, PASS */,
			   ilu_Error * /* err */);
/* see ilu_ReadPickle() */

ILU_PUBLIC ilu_boolean
  _ilu_pickle2_EndPickle (ilu_Call /* call */,
			  ilu_Pickle * /* pickle, OUT, OPTIONAL, PASS */,
			  ilu_Error * /* err */);
/* see ilu_EndPickle() */

#endif /* ADD_PICKLE2_SUPPORT */

#if (defined(ADD_PICKLE3_SUPPORT))

ILU_PUBLIC struct _ilu_Connection_s _ilu_pickle3_Format;

ILU_PUBLIC ilu_boolean
  _ilu_pickle3_StartPickle (ilu_Call_s *,	/* Call struct to init */
			    ilu_Type,		/* constraint type, if any */
			    ilu_Error *);	/* error return */
/* see ilu_StartPickle() */

ILU_PUBLIC ilu_boolean
  _ilu_pickle3_WritePickle (ilu_Call,
			    ilu_cardinal /* argSize, HINT -- can be zero */,
			    ilu_string	/* type_id, IN, RETAIN */,
			    ilu_Error *	/* err */);
/* see ilu_WritePickle() */

ILU_PUBLIC ilu_string	/* OUT, belongs to "pickle" arg */
  _ilu_pickle3_PickleType (ilu_Pickle	/* pickle, IN, RETAIN */,
			   ilu_Error *	/* error */);
/* See ilu_PickleType() */

ILU_PUBLIC ilu_TypeKind
  _ilu_pickle3_PickleTypeKind (ilu_Pickle	/* pickle, IN, RETAIN */,
			       ilu_Error *	/* error */);
/* See ilu_PickleTypeKind() */

ILU_PUBLIC ilu_boolean
  _ilu_pickle3_PickleTypes (ilu_Pickle,
			    ilu_string **,
			    ilu_cardinal *,
			    ilu_Error *	/* error */);
/* See ilu_PickleTypes() */

ILU_PUBLIC ilu_boolean
  _ilu_pickle3_ReadPickle (ilu_Call /* call */,
			   ilu_Pickle /* pickle, IN, PASS */,
			   ilu_Error * /* err */);
/* see ilu_ReadPickle() */

ILU_PUBLIC ilu_boolean
  _ilu_pickle3_EndPickle (ilu_Call /* call */,
			  ilu_Pickle * /* pickle, OUT, OPTIONAL, PASS */,
			  ilu_Error * /* err */);
/* see ilu_EndPickle() */

#endif /* ADD_PICKLE3_SUPPORT */

/********************* from gc.c ********************/

/*L1, L2, Main unconstrained*/

extern const ilu_Class _ilu_GcCallbackClass;

/*L1 >= {gcmu}*/

void _ilu_StartGCingTrueObj(ilu_Object obj);

void _ilu_StopGCingTrueObj(ilu_Object obj);

/*L1 >= {gcmu, cmu, obj's server}*/
ILU_ERRS((bad_locks, broken_locks, internal))
_ilu_TouchedObj(ilu_Object obj);
/* Applicable to collectible true objects;
   call this when lastRemote or gclist's emptiness changes. */

extern ilu_refany _ilu_gcoAlarm;
extern ilu_refany _ilu_gccAlarm;

/**Call-Locking(call, IHi) before,
   Call-Locking(call,  No) after*/

void _ilu_HandleGCInterestDeregistration(ilu_Call call);
void _ilu_HandleGCInterestRegistration(ilu_Call call);
/* Server stubs for two built-in methods. */

/*Main Invariant holds; L2 otherwise unconstrained*/

extern          ilu_boolean
_ilu_RegisterGCInterest(ilu_Object obj,
			ILU_ERRS((GcRegFailed, bad_locks,
				  broken_locks, internal)) * err);

extern          ilu_boolean
_ilu_UnregisterGCInterest(ilu_Object obj,
			  ILU_ERRS((GcRegFailed, bad_locks,
				    broken_locks, internal)) * err);

/*
 * These two procedures attempt to notify the true server of the
 * surrogate obj's (non-)existance.  They return ilu_TRUE iff mission
 * accomplished, and independently may raise an error.
 */

#endif /* ndef _ILU_INTERNALS_ */
