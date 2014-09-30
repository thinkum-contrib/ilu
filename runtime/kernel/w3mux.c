/** $Id: w3mux.c,v 1.95 1999/08/03 01:53:08 janssen Exp $
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
/* Last edited by Mike Spreitzer October 16, 1998 10:43 pm PDT */

/***********************************************************************/
/***********************************************************************

This file contains an implementation of the W3C MUX protocol, adapted
for ILU.  Basically it is the protocol described in

  http://www.w3.org/Protocols/MUX/WD-mux-971203.html

modified as follows:

  Endpoint semantics:  Each address space has a single endpoint,
  identified by a generated UUID.  This endpoint appears in the
  return tinfo for MUX when you create a port.

  DefineEndpoint is completely re-interpreted.  Endpoints are just
  identifiers, not meant to be parsed.  Each LTt has an optional
  endpoint associated with it, signifying that that endpoint can
  be reached by connecting back across that LTt.

  MUX channels are identified by integer channel numbers, analogous
  to IP ports.  This implementation will accept and even store atoms
  if received, but has no idea of what to do with them.  In particular,
  the channel id (ProtocolId) in a SYN msg will not be associated in
  any way with any interned atom.  This implementation never sends
  InternAtom.

************************************************************************/
/***********************************************************************/

#include "iluntrnl.h"

#include "ilutransport.h"
#include "mooring.h"

#include "oscalls.h"

/*
#define ILU_W3MUX_EXCESS_DEBUGGING	1
*/

typedef unsigned int flagbit;
typedef struct Header_s {
  ilu_cardinal short_header;
  ilu_cardinal long_length;
} Header_s;

typedef struct LTm_s *LTm;	/* lower-tinfo mooring state */
typedef struct LTt_s *LTt;	/* lower-tinfo transport state */
typedef struct Mm_s  *Mm;	/* mux mooring state */
typedef struct Mt_s  *Mt;	/* mux transport state */
typedef struct InBuf_s *InBuf;	/* input msg read from underlying connection */
typedef struct Mc_s  *Mc;	/* mux creator state */
typedef struct PendingSession_s *PendingSession;
typedef struct Endpoint_s *Endpoint;	/* Endpoint structure */
typedef ilu_CharBuf *Atom;	/* holds text of Atom */
typedef struct AtomDef_s *AtomDef;	/* local atom info */

enum MsgDirection { InputDir, OutputDir, None };

struct Endpoint_s {
  char *	id;		/* UUID identifier for endpoint */
  ilu_boolean	local;		/* represents a local endpoint or a remote one? */
  union {
    struct {
      ilu_HashTable	channels;	/* maps from channel # -> Mm */
      ilu_HashTable	channels_by_name;	/* maps from string -> Mm */
      ilu_cardinal	channel_counter;/* starts at 0x30000, allocates channels locally */
    } local;			/* state if local */
    struct {
      ilu_Vector	connections;	/* vector of LTt, connections to remote endpoint */
    } remote;			/* state if remote */
  } state;
};

struct AtomDef_s {
  ilu_cardinal	atom_id;
  ilu_string	atom_name;
};

#define PREBUFLEN 128

struct LTt_s {
  ilu_Transport   source;	/* actual transport */
  ilu_Mutex       lock;		/* protects data */
  ilu_Condition   x_CV;		/* if (source != NIL), wait for x
				 * mutex; otherwise, wait for
				 * session deaths */
  ilu_boolean     x_held;	/* implements x mutex */
  ilu_boolean     m_open;	/* (!initiator) && mooring opened on
				 * still open */
  ilu_Vector      sessions;	/* vector of Mt */
  Endpoint        endpoint;	/* endpoint ID of other end,
				 * OPTIONAL */
  ilu_boolean     lower_disabled;	/* is waiting on "source"
					 * disabled? */
  ilu_cardinal    max_frag_size;/* maximum fragment size allowed, 0
				 * for none */
  ilu_cardinal    default_credit;	/* default initial Mt credit
					 * size */

  /*
   * "interned_atoms_remote" holds intern defs received from peer.
   * "interned_atoms_local" holds interns sent to peer.
   * "interned_atoms_local_id" contains next atom id to use when
   * interning an atom on peer.
   */
  ilu_HashTable   interned_atoms_remote;
  /* Protocol ID (atom range) => (ilu_CharBuf *) */
  
  ilu_HashTable   interned_atoms_local;
  /* ilu_string => Protocol ID (atom range) */
  
  ilu_cardinal    interned_atoms_local_id;	/* next atom ID to use */

  /*
   * "pending_conn_reqs" contains a number of incoming connection
   * requests that have not yet been serviced by a mooring.  Each
   * entry may have an arbitrary number of InBufs chained to it, but
   * the first InBuf in each entry must have a header with the SYN
   * bit set, and a valid channel ID.  The mooring thread examines
   * this array when looking for a new conn req to fulfill, and will
   * remove entries from this array.  The LTt worker thread will add
   * entries to this array, and add chunks to each entry as they
   * come it.
   */
  InBuf          *pending_conn_reqs;	/* array of rcvd conn
					 * requests not yet serviced */
  ilu_cardinal    pending_size;	/* size of pending_conn_reqs array */
  ilu_cardinal    pending_used;	/* no. of elements used in
				 * pending_conn_reqs */

  /* the following are readonly */
  ilu_boolean     initiator;	/* this side initiated connection */
  ilu_string      description;	/* string form of the tinfo for this
				 * Transport */
  Endpoint        my_endpoint;

  /* the following are private to the worker thread */
  ilu_boolean     reading_header;	/* ilu_TRUE when reading chunk
					 * header */
  ilu_boolean     reading_long_len;	/* reading the long_length
					 * field of header */
  ilu_boolean     reading_padding;	/* reading discardable
					 * padding bytes */
  Header_s        current_header;	/* buffer to hold current
					 * header while reading
					 * header */
  InBuf           current_buffer;	/* buffer to hold current
					 * chunk and chunk metadata */
  ilu_bytes       where;	/* pointer to buffer to fill when
				 * reading bytes */
  ilu_cardinal    to_read;	/* # of bytes to read when reading
				 * bytes */
  ilu_cardinal    padding;	/* # of extra bytes to discard (0-3) */
  
  /*
   * Bytes [preFirst, preNext) of preBytes should be written to
   * lower transport before anything else.  Protected by lower x mutex.
   */
  ilu_byte        preBytes[PREBUFLEN];
  ilu_cardinal    preFirst, preNext;
};

struct InBuf_s {
  InBuf		next;		/* when used in linked list, points to next */
  Header_s	header;		/* header of message, byte-swapped to native endianness */
  ilu_boolean	user_supplied;	/* ilu_TRUE if "bytes" are supplied by higher-level code */
  LTt		ltt;		/* the underlying transport it was received on */
  ilu_cardinal	size;		/* total size of buffer */
  ilu_cardinal	used;		/* number of bytes currently used in buffer */
  ilu_bytes	bytes;		/* pointer to actual buffer space */
};

struct LTm_s {
  ilu_Mutex lock;		/* protects LTm data */
  ilu_Condition x_CV;		/* to wait for X mutex */
  ilu_boolean x_held;		/* implements X mutex */
  ilu_boolean delay_close;	/* ilu_TRUE when closing should be delayed */
  ilu_Condition delay_close_wait;	/* to wait for change in delay_close */
  ilu_Vector channels;		/* vector of Mm */
  ilu_Vector transports;	/* vector of LTt */
  ilu_Mooring source;		/* underlying Mooring */
  Endpoint endpoint;		/* the endpoint for this mooring */
  ilu_string description;	/* for debugging messages -- tinfo string */
  ilu_TransportInfo tinfo_in;	/* specified tinfo when mooring created */
  ilu_TransportInfo tinfo_out;	/* returned tinfo when mooring created */
};

struct Mm_s {
  ilu_cardinal channel;		/* MUX port # */
  ilu_string   name;		/* name, OPTIONAL */
  LTm ltm;			/* underlying LT mooring */
  struct {
    /* "conn_reqs" is an array of InBuf ptrs which have pending connection requests
       for this mooring.  The same LTt may appear more than once.  The mooring
       processes this array in first-to-last order, and is responsible for removing
       the InBuf from the pending_conn_reqs list of the associated LTt. */
    InBuf *pending_conn_reqs;	/* unprocessed connection requests */
    ilu_cardinal pending_size;/* size of array */
    ilu_cardinal pending_used;/* number of elements currently used */
    ilu_integer wait_disables;	/* count of calls to disable_wait */
    ilu_Condition wait_CV;	/* used to wait for a connection request */
  } ltm_locked;
};

struct Mt_s {
  ilu_cardinal session;		/* MUX session # */
  ilu_cardinal channel;		/* MUX channel connected to */
  LTt ltt;			/* lower level transport */
  ilu_Transport transport;	/* transport data for self */
  enum MsgDirection msgdir;	/* state of current msg boundaries */
  ilu_boolean eom_received;	/* has received PUSH from other end */
  ilu_boolean eof_received;	/* has received FIN or RESET from other end */
  ilu_boolean closed;		/* has processed FIN or RESET from either end */
  ilu_boolean unused;		/* ilu_FALSE if a msg of any type has already been
				   sent on this session, ilu_TRUE otherwise */
  struct {
    struct {
      /* if reading into user-supplied buffer, this is the buffer */
      ilu_bytes user_buffer;
      ilu_cardinal user_buffer_size;
      InBuf current;
    } input_state;
    struct {
      ilu_boolean credit_limited;/* ilu_TRUE if has credit limit */
      ilu_cardinal credit;	/* max # bytes can send to peer */
    } output_state;
    ilu_integer wait_disables;	/* count of calls to disable_wait */
    ilu_Condition wait_CV;	/* CV to wait on when waiting */
    InBuf bufferlist;		/* incoming buffers to process */
    ilu_cardinal local_credit;	/* max # bytes peer can send */
  } ltt_locked;
};

struct Mc_s {
  ilu_TransportCreator		lower;
  ilu_TransportInfo		lower_tinfo;
  ilu_cardinal			channel_number;
  ilu_string			channel_name;
  char *			endpoint_id;	/* OPTIONAL */
};

/**********************************************************************/
/**********************************************************************/
/*								      */
/*			Macro definitions			      */
/*								      */
/**********************************************************************/
/**********************************************************************/

#define MUX_CONTROL             0x00800000
#define MUX_CONTROL_SHIFT	23
#define MUX_CONTROL_CODE	0x00780000
#define MUX_CONTROL_CODE_SHIFT	19
#define MUX_SYN                 0x00400000
#define MUX_SYN_SHIFT		22
#define MUX_FIN                 0x00200000
#define MUX_FIN_SHIFT		21
#define MUX_RST                 0x00100000
#define MUX_RST_SHIFT		20
#define MUX_PUSH                0x00080000
#define MUX_PUSH_SHIFT		19
#define MUX_LONG_LENGTH		0x00040000
#define MUX_LONG_LENGTH_SHIFT	18
#define MUX_SESSION             0xFF000000
#define MUX_SESSION_SHIFT	24
#define MUX_FRAGMENT_SIZE       0x0003FFFF
#define MUX_FRAGMENT_SIZE_SHIFT 0

#define MUX_TCP_CHANNEL_MIN	0x00000000
#define MUX_TCP_CHANNEL_MAX	0x0000FFFF
#define MUX_UDP_CHANNEL_MIN	0x00010000
#define MUX_UDP_CHANNEL_MAX	0x0001FFFF
#define MUX_ATOM_CHANNEL_MIN	0x00020000
#define MUX_ATOM_CHANNEL_MAX	0x000200FF
#define MUX_LOCAL_CHANNEL_MIN	0x00030000
#define MUX_LOCAL_CHANNEL_MAX	0x0003FFFF

#define HEADER_EXTENDED(ptr)		(((ptr)->short_header & MUX_LONG_LENGTH) != 0)
#define HEADER_FRAGMENT_SIZE(ptr)	((ptr)->short_header & MUX_FRAGMENT_SIZE)
#define HEADER_CHUNK_SIZE(ptr)		(HEADER_EXTENDED(ptr)?((ptr)->long_length):HEADER_FRAGMENT_SIZE(ptr))
#define HEADER_SESSION_ID(ptr)		(((ptr)->short_header & MUX_SESSION) >> MUX_SESSION_SHIFT)
#define HEADER_CHANNEL_ID(ptr)		((ptr)->short_header & MUX_FRAGMENT_SIZE)
#define HEADER_MAX_FRAGMENT_SIZE(ptr)	((ptr)->short_header & MUX_FRAGMENT_SIZE)
#define HEADER_CREDIT_SIZE(ptr)		((ptr)->short_header & MUX_FRAGMENT_SIZE)
#define HEADER_DEFAULT_CREDIT_SIZE(ptr)	((ptr)->short_header & MUX_FRAGMENT_SIZE)
#define HEADER_ATOM_STR_LENGTH(ptr)	((ptr)->short_header & MUX_FRAGMENT_SIZE)
#define HEADER_ENDPOINT_LENGTH(ptr)	((ptr)->short_header & MUX_FRAGMENT_SIZE)
#define HEADER_CONTROL_MSG(ptr)		(((ptr)->short_header & MUX_CONTROL) != 0)
#define HEADER_CONTROL_CODE(ptr)	(((ptr)->short_header & MUX_CONTROL_CODE) >> MUX_CONTROL_CODE_SHIFT)
#define HEADER_LAST_CHUNK(ptr)		(((ptr)->short_header & MUX_PUSH) != 0)
#define HEADER_NEW_SESSION(ptr)		(((ptr)->short_header & MUX_SYN) != 0)
#define HEADER_LAST_MSG(ptr)		(((ptr)->short_header & MUX_FIN) != 0)
#define HEADER_RESET(ptr)		(((ptr)->short_header & MUX_RST) != 0)

#define SHORT_HEADER_SIZE	0x0003FFFF

/* control messages */
#define MUX_InternAtom		0
#define MUX_DefineEndpoint	1
#define MUX_SetMSS		2
#define MUX_AddCredit		3
#define MUX_SetDefaultCredit	4
#define MUX_NoOp		5

#define MUX_INITIAL_CREDIT	0x10000
#define MUX_MAX_CREDIT		MUX_FRAGMENT_SIZE

#define MT_OUTBUFF_SIZE		0x1008

#define ENDPOINT_CHANNEL_HASHTABLE_BUCKETS	13

#define BYTE_SWAP_WORD(a) ( ((a) << 24) | \
			   (((a) << 8) & 0x00ff0000) | \
			   (((a) >> 8) & 0x0000ff00) | \
			   ((ilu_cardinal)(a) >>24) )

#ifdef WORDS_BIGENDIAN
#define FORMAT_HEADER(where,size,syn,fin,reset,push,channel,session) \
(((syn) || ((size) > SHORT_HEADER_SIZE)) ?	\
  ((where)[0] = (((session) << 24) |		\
		 ((syn) ? MUX_SYN : 0) |	\
		 ((fin) ? MUX_FIN : 0) |	\
		 ((reset) ? MUX_RST : 0) |	\
		 ((push) ? MUX_PUSH : 0) |	\
		 MUX_LONG_LENGTH | \
		 ((syn) ? ((channel) & MUX_FRAGMENT_SIZE) : 0)), \
   (where)[1] = (size)				\
   ) :						\
  ((where)[0] = (((session) << 24) |		\
		 ((syn) ? MUX_SYN : 0) |	\
		 ((fin) ? MUX_FIN : 0) |	\
		 ((reset) ? MUX_RST : 0) |	\
		 ((push) ? MUX_PUSH : 0) |	\
		 ((size) & MUX_FRAGMENT_SIZE))	\
   )						\
  ),						\
 (syn) = ilu_FALSE
#else
#define FORMAT_HEADER(where,size,syn,fin,reset,push,channel,session) \
(((syn) || ((size) > SHORT_HEADER_SIZE)) ?			\
  ((where)[0] = BYTE_SWAP_WORD((((session) << 24) |		\
				((syn) ? MUX_SYN : 0) |		\
				((fin) ? MUX_FIN : 0) |		\
				((reset) ? MUX_RST : 0) |	\
				((push) ? MUX_PUSH : 0) |	\
				MUX_LONG_LENGTH |		\
				((syn) ? ((channel) & MUX_FRAGMENT_SIZE) : 0))), \
   (where)[1] = BYTE_SWAP_WORD(size)				\
   ) :								\
  ((where)[0] = BYTE_SWAP_WORD((((session) << 24) |		\
				((syn) ? MUX_SYN : 0) |		\
				((fin) ? MUX_FIN : 0) |		\
				((reset) ? MUX_RST : 0) |	\
				((push) ? MUX_PUSH : 0) |	\
				((size) & MUX_FRAGMENT_SIZE)))	\
   )								\
  ),								\
 (syn) = ilu_FALSE
#endif

#define HEADERSIZE(h)		(sizeof(Header_s) + ((((Header_s *)(h))->contents.data_hdr.long_length) ? 4 : 0))
#define HEADERLENGTH(size,fo)	(((fo) || ((size) > SHORT_HEADER_SIZE)) ? 8 : 4)

#define DECREMENT_HEADER_SIZE(ptr,size)		(HEADER_EXTENDED(ptr)?((ptr)->long_length -= (size)):((ptr)->short_header = (((ptr)->short_header & (~MUX_FRAGMENT_SIZE)) | ((HEADER_FRAGMENT_SIZE(ptr) - (size)) & MUX_FRAGMENT_SIZE))))

#define CONNECTION_TABLE_BUCKETS	23
#define ATOMS_HT_BUCKETS		23

#ifndef WORDS_BIGENDIAN
#define SWAP_IF_NEEDED(n)	((n) = BYTE_SWAP_WORD(n))
#else
#define SWAP_IF_NEEDED(n)	
#endif

#ifdef WORDS_BIGENDIAN
#define LITTLE_ENDIAN_TO_NATIVE(a)	BYTE_SWAP_WORD(a)
#define NATIVE_TO_LITTLE_ENDIAN(a)	BYTE_SWAP_WORD(a)
#define BIG_ENDIAN_TO_NATIVE(a)		(a)
#define NATIVE_TO_BIG_ENDIAN(a)		(a)
#else
#define LITTLE_ENDIAN_TO_NATIVE(a)	(a)
#define NATIVE_TO_LITTLE_ENDIAN(a)	(a)
#define BIG_ENDIAN_TO_NATIVE(a)		BYTE_SWAP_WORD(a)
#define NATIVE_TO_BIG_ENDIAN(a)		BYTE_SWAP_WORD(a)
#endif

#define ROUND_UP(val,pt)		((((val)%(pt))==0)?(val):((val)+((pt)-((val)%(pt)))))
#define FIGURE_PADDING_2(amt,aln)	((((amt)%(aln))==0)?0:((aln)-((amt)%(aln))))
#define FIGURE_PADDING_1(amt,hdr)	FIGURE_PADDING_2((amt),HEADER_EXTENDED(hdr)?8:4)

#define transport_has_exposed_input(t)  ((t)->tr_inBuff && ((t)->tr_inNext < (t)->tr_inLimit))

#define SESSION_NAME(x) (x)->ltt->description, ((unsigned)((x)->session)), ((unsigned)((x)->channel))
#define CHANNEL_NAME(p)	(p)->ltm->description, (unsigned long) ((p)->channel)

#ifdef ENABLE_DEBUGGING
static char *control_code[16] = {
  "InternAtom", "DefineEndpoint", "SetMSS", "AddCredit", "SetDefaultCredit", "NoOp",
  "6", "7", "8", "9", "10", "11", "12", "13", "14", "15" };

static void
  display_header (Header_s *p)
{
  ilu_DebugPrintf ("ILU w3mux Header:\n");
  if (HEADER_EXTENDED(p))
    ilu_DebugPrintf ("  session=%lu, control=%s, extended=%s, payload=%lu, channel=%lu\n",
		     HEADER_SESSION_ID(p), HEADER_CONTROL_MSG(p) ? "ilu_TRUE" : "ilu_FALSE",
		     HEADER_EXTENDED(p) ? "ilu_TRUE" : "ilu_FALSE", HEADER_CHUNK_SIZE(p),
		     HEADER_CHANNEL_ID(p));
  else
    ilu_DebugPrintf ("  session=%lu, control=%s, extended=%s, payload=%lu\n",
		     HEADER_SESSION_ID(p), HEADER_CONTROL_MSG(p) ? "ilu_TRUE" : "ilu_FALSE",
		     HEADER_EXTENDED(p) ? "ilu_TRUE" : "ilu_FALSE", HEADER_CHUNK_SIZE(p));
  if (HEADER_CONTROL_MSG(p)) {
    ilu_DebugPrintf("  control_code=%s\n", control_code[HEADER_CONTROL_CODE(p)]);
  } else {
    ilu_DebugPrintf("  syn=%s, fin=%s, rst=%s, push=%s\n",
		    HEADER_NEW_SESSION(p) ? "ilu_TRUE" : "ilu_FALSE",
		    HEADER_LAST_MSG(p) ? "ilu_TRUE" : "ilu_FALSE",
		    HEADER_RESET(p) ? "ilu_TRUE" : "ilu_FALSE",
		    HEADER_LAST_CHUNK(p) ? "ilu_TRUE" : "ilu_FALSE");
  }
  return;		   
}
#define DISPLAY_HEADER(p)	if (ilu_DebugLevel & W3MUX_DEBUG) display_header(p)
#else
#define DISPLAY_HEADER(p)	
#endif

#if 0
#define W3MUX_RELEASE_MUTEX(m)	w3mux_ReleaseMutex(m, __FILE__, __LINE__)

static void
  w3mux_ReleaseMutex (ilu_Mutex m, char *filename, unsigned int lineno)
{
  if ((ilu_DebugLevel & W3MUX_DEBUG) && (ilu_DebugLevel & LOCK_DEBUG)) {
    ilu_DebugPrintf ("w3mux_ReleaseMutex:  %p in %s at line %lu\n",  m, filename, lineno);
  }
  ilu_ReleaseMutex(m);
}
#else
#define W3MUX_RELEASE_MUTEX(m)	ilu_ReleaseMutex(m)
#endif

#if 1
#define LTT_UNLOCK(ltt)		_ltt_unlock((ltt), __FILE__, __LINE__)

static void _ltt_unlock (LTt ltt, char *filename, unsigned int lineno)
{
  if ((ilu_DebugLevel & W3MUX_DEBUG) && (ilu_DebugLevel & LOCK_DEBUG)) {
    ilu_DebugPrintf ("_ltt_unlock:  %p in %s at line %lu\n",  ltt, filename, lineno);
  }
  W3MUX_RELEASE_MUTEX(ltt->lock);
}
#else
#define LTT_UNLOCK(ltt)		ltt_unlock(ltt)
#endif

#if 0
#define ENTER_GLOBLOCK()	_enter_globlock(__FILE__, __LINE__)

static void enter_globlock(void);

static void _enter_globlock (char *filename, unsigned int lineno)
{
  ILU_NOTE(W3MUX_DEBUG,
	   ("_enter_globlock (%s, %lu)\n", filename, lineno));
  enter_globlock();
}
#else
#define ENTER_GLOBLOCK()	enter_globlock()
#endif

/***********************************************************************/
/***********************************************************************/
/***********************  Forward declarations  ************************/
/***********************************************************************/
/***********************************************************************/

static void ltt_lock(LTt);
static void ltt_unlock(LTt);
static Mt ltt_allocate_session (LTt, ilu_Error *);
static void ltm_lock(LTm);
static void ltm_unlock(LTm);
static Mt mt_create (LTt, ilu_cardinal, ilu_Error *);
static void mt_free (Mt);

/* holding ltm->lock */
static void mm_add_pending (Mm, InBuf, ilu_Error *);


/***********************************************************************/
/***********************************************************************/
/***********  functions to hash and compare tinfo sequences    *********/
/***********************************************************************/
/***********************************************************************/

/*L1, L2 unconstrained*/
static ilu_cardinal
  hash_tinfo (void *key, ilu_cardinal modulus)
{
  ilu_TransportInfo tinfo = (ilu_TransportInfo) key;
  int i;
  ilu_cardinal hashval = 0xFFFFFFFF;

  for (i = 0;  tinfo[i] != NIL;  i++) {
    hashval = ilu_CRC32WithAccum((ilu_bytes) tinfo[i], strlen(tinfo[i]), hashval);
  };
  return (hashval % modulus);
}

/*L1, L2 unconstrained*/
static ilu_boolean
  compare_tinfo (void *key1, void *key2)
{
  ilu_TransportInfo tinfo1 = (ilu_TransportInfo) key1;
  ilu_TransportInfo tinfo2 = (ilu_TransportInfo) key2;
  int i;

  for (i = 0;  tinfo1[i] != NIL;  i++) {
    if (tinfo2[i] == NIL ||
	strcmp(tinfo1[i], tinfo2[i]) != 0)
      return ilu_FALSE;
  }
  return (tinfo2[i] == NIL);
}

/***********************************************************************/
/***********************************************************************/
/********  Acquire (and create if necessary) the global mutex  *********/
/***********************************************************************/
/***********************************************************************/

static ilu_Mutex w3mux_globlock = NIL;

static void create_globlock(void)
{
  if (w3mux_globlock == NIL)
    w3mux_globlock = ilu_CreateMutex("w3mux", "globlock");
  _ilu_Assert(w3mux_globlock != NIL, "can't create w3mux globlock");
}

static void enter_globlock(void)
{
  if (w3mux_globlock == NIL)
    create_globlock();
  ilu_AcquireMutex(w3mux_globlock);
}

static void exit_globlock(void)
{
  W3MUX_RELEASE_MUTEX(w3mux_globlock);
}

/***********************************************************************/
/***********************************************************************/
/***********************  Input-buffer object  *************************/
/***********************************************************************/
/***********************************************************************/

/* XXX a sorted resource list of unused InBuf blocks, linked to the ILU
   malloc code via a call to ilu_AddFreer, might make a lot of sense here */

static InBuf
  inbuf_get_linked_buffer (Header_s *header, LTt ltt, ilu_Error *err)
{
  InBuf ptr = ilu_MallocE(sizeof(*ptr), err);
  if (ILU_ERRNOK(*err)) return NIL;
  ptr->next = NIL;
  ptr->header = *header;
  ptr->user_supplied = ilu_TRUE;
  ptr->ltt = ltt;
  ptr->size = 0;
  ptr->used = 0;
  ptr->bytes = NIL;
  return ptr;
}

static InBuf
  inbuf_get_full_buffer (Header_s *header, LTt ltt, ilu_Error *err)
{
  InBuf ptr = (InBuf) ilu_MallocE(sizeof(*ptr) + HEADER_CHUNK_SIZE(header), err);
  if (ILU_ERRNOK(*err)) return NIL;
  ptr->next = NIL;
  ptr->header = *header;
  ptr->user_supplied = ilu_FALSE;
  ptr->ltt = ltt;
  ptr->size = HEADER_CHUNK_SIZE(header);
  ptr->used = 0;
  ptr->bytes = ((ilu_bytes) ptr) + sizeof(*ptr);
  return ptr;
}

static void
  inbuf_free (InBuf buf)
{
  if (buf == NIL) return;
  if (buf->next != NIL)
    inbuf_free(buf->next);
  ilu_free(buf);
}

/***********************************************************************/
/***********************************************************************/
/**************************  Endpoint table  ***************************/
/***********************************************************************/
/***********************************************************************/

static ilu_Vector w3mux_Endpoints;	/* protected by globlock */

/* no locking */
static Endpoint
  endpoint_create (char *id, ilu_boolean local, ilu_Error *err)
{
  Endpoint ep;
  unsigned padded_len;

  ep = ilu_MallocE(sizeof(*ep), err);
  if (ILU_ERRNOK(*err)) return NIL;
  ep->local = local;
  padded_len = strlen(id);
  padded_len = ROUND_UP(padded_len, 4);
  ep->id = ilu_MallocE(padded_len + 1, err);
  if (ILU_ERRNOK(*err)) { ilu_free(ep);  return NIL; };
  memset (ep->id, 0, padded_len + 1);
  strcpy (ep->id, id);
  if (local) {
    ep->state.local.channel_counter = MUX_LOCAL_CHANNEL_MIN;
    ep->state.local.channels = ilu_hash_MakeNewTable(ENDPOINT_CHANNEL_HASHTABLE_BUCKETS,
						     ilu_hash_HashPointer, ilu_hash_PointerCompare);
    if (ep->state.local.channels == NIL) {
      ilu_free(ep->id);
      ilu_free(ep);
      ILU_ERR_CONS1(no_memory, err, nbytes, 0, 0);
      return NIL;
    };
    ep->state.local.channels_by_name = ilu_hash_MakeNewTable(ENDPOINT_CHANNEL_HASHTABLE_BUCKETS,
							     ilu_hash_HashString, ilu_hash_StringCompare);
    if (ep->state.local.channels_by_name == NIL) {
      ilu_hash_FreeHashTable (ep->state.local.channels, NULLFN, NULLFN);
      ilu_free(ep->id);
      ilu_free(ep);
      ILU_ERR_CONS1(no_memory, err, nbytes, 0, 0);
      return NIL;
    };
  } else {
    ep->state.remote.connections = _ilu_vector_new(2, err);
    if (ILU_ERRNOK(*err)) {
      ilu_free(ep->id);
      ilu_free(ep);
      return NIL;
    };
  }
  return ep;
}

static void
  endpoint_free (Endpoint endpoint)
{
  ilu_free(endpoint->id);
  if (endpoint->local) {
    ilu_hash_FreeHashTable (endpoint->state.local.channels, NULLFN, NULLFN);
    ilu_hash_FreeHashTable (endpoint->state.local.channels_by_name, NULLFN, NULLFN);
  } else {
    _ilu_vector_destroy (endpoint->state.remote.connections, NULLFN);
  }
  ilu_free(endpoint);
}

/* inside globlock */
static Endpoint
  endpoint_get (char *id /* OPTIONAL */, ilu_boolean local, ilu_Error *err)
{
  Endpoint ep = NIL;
  Endpoint *elts;
  char buf[1000];
  int i, size;
  static char *default_id = NIL;

  ilu_HoldMutex (w3mux_globlock);

  if (id == NIL) {
    if (!local)
      return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_muxBadEndpoint, NIL);
    if (default_id == NIL)
      default_id = ilu_InventID();
    sprintf (buf, "ilu-uuid:%s", default_id);
    id = buf;
  }
  for (i = 0, size = _ilu_vector_size(w3mux_Endpoints),
       elts = (Endpoint *) _ilu_vector_elements(w3mux_Endpoints);
       i < size;  i+=1) {
    if (strcmp(elts[i]->id, id) == 0) {
      ep = elts[i];
      break;
    }
  }
  if (ep == NIL) {
    ep = endpoint_create(id, local, err);
    if (ILU_ERRNOK(*err)) return NIL;
    _ilu_vector_add (w3mux_Endpoints, (ilu_refany) ep, err);
    if (ILU_ERRNOK(*err)) {
      endpoint_free(ep);
      return NIL;
    };
  } else if (local != ep->local) {
    ILU_NOTE(W3MUX_DEBUG,
	     ("ILU(w3mux:endpoint_get):  attempt to get %s endpoint <%s> as %s\n",
	      ep->local ? "local" : "non-local", ep->id, local ? "local" : "non-local"));
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_muxBadEndpoint, NIL);
  } else
    ILU_CLER(*err);
  return ep;
}

/* holding globlock... */
static void
  endpoint_register_channel (Endpoint endpoint, Mm mm, ilu_Error *err)
{
  Mm mm2;

  if (((mm2 = ilu_hash_FindInTable(endpoint->state.local.channels, (void *) (mm->channel))) != NIL) ||
      ((mm->name != NIL) &&
       ((mm2 = ilu_hash_FindInTable(endpoint->state.local.channels_by_name, (void *) (mm->name))) != NIL))) {
    ILU_ERR_CONS1(internal, err, minor, ilu_im_multiple_channels, 0);
  } else {
    ilu_hash_AddToTable(endpoint->state.local.channels, (void *) (mm->channel), mm);
    if (mm->name != NIL)
      ilu_hash_AddToTable(endpoint->state.local.channels_by_name, (void *) (mm->name), mm);
  }
}

/* holding globlock... */
static Mm
  endpoint_find_channel_by_number (Endpoint endpoint, ilu_cardinal channel)
{
  return (Mm) ilu_hash_FindInTable(endpoint->state.local.channels, (void *) channel);
}

/* holding globlock... */
static Mm
  endpoint_find_channel_by_name (Endpoint endpoint, Atom channel_name)
{
  return (Mm) ilu_hash_FindInTable(endpoint->state.local.channels_by_name, (void *) (channel_name->icb_base));
}

/* holding globlock... */
static Mm
  endpoint_unregister_channel (Endpoint endpoint, ilu_cardinal channel, ilu_string name)
{
  Mm mm;

  if ((mm = ilu_hash_FindInTable (endpoint->state.local.channels, (void *) channel)) == NIL)
    return NIL;
  ilu_hash_RemoveFromTable(endpoint->state.local.channels, (void *) channel);
  if (ilu_hash_FindInTable (endpoint->state.local.channels_by_name, (void *) name) != NIL)
    ilu_hash_RemoveFromTable(endpoint->state.local.channels, (void *) name);    
  return mm;
}

/* holding globlock... */
/* returns 0 to indicate no free channel */
static ilu_cardinal
  endpoint_find_unused_channel_number (Endpoint endpoint)
{
  /* skip over any user-specified channels that are in use */
  while ((endpoint->state.local.channel_counter <= MUX_LOCAL_CHANNEL_MAX) &&
	 (ilu_hash_FindInTable(endpoint->state.local.channels,
			       (void *) endpoint->state.local.channel_counter) != NIL))
    endpoint->state.local.channel_counter += 1;
  /* return next free channel */
  if (endpoint->state.local.channel_counter <= MUX_LOCAL_CHANNEL_MAX)
    return endpoint->state.local.channel_counter;
  else
    return 0;
}

/* holding globlock... */
static void
  endpoint_remove_connection (Endpoint endpoint, LTt connection)
{
  _ilu_vector_remove (endpoint->state.remote.connections, connection);
}

/* holding globlock... */
static void
  endpoint_add_connection (Endpoint endpoint, LTt connection, ilu_Error *err)
{
  _ilu_vector_add (endpoint->state.remote.connections, connection, err);
}

/* when returning non-NIL, holds lock of return value's LTt */
static Mt
  endpoint_allocate_session (Endpoint ep, ilu_Error *err)
{
  unsigned int i;
  LTt ltt;
  Mt mt;

  for (i = 0, mt = NIL;  (mt == NIL) && (i < _ilu_vector_size(ep->state.remote.connections));  i++) {
    ltt = (LTt) _ilu_vector_elements(ep->state.remote.connections)[i];
    ltt_lock(ltt);
    if (ltt->source == NIL) {	/* closing */
      LTT_UNLOCK(ltt);
      continue;
    }
    mt = ltt_allocate_session(ltt, err);
    if (ILU_ERRNOK(*err))
      return NIL;
    LTT_UNLOCK(ltt);
  }
  return mt;
}

/***********************************************************************/
/***********************************************************************/
/******************  Data structure Initialization  ********************/
/***********************************************************************/
/***********************************************************************/
static void
  w3mux_InitializeDataStructures (ilu_Error *err)
{
  static ilu_boolean initialized = ilu_FALSE;

  if (! initialized ) {
    create_globlock();
    w3mux_Endpoints = _ilu_vector_new(2, err);
    initialized = ilu_TRUE;
  }
}

/***********************************************************************/
/***********************************************************************/
/**************************  Endpoint Identity  ************************/
/***********************************************************************/
/***********************************************************************/

struct endpoint_identity_info_s {
  char *id;
};

static ilu_cardinal
  endpoint_identity_string_form (ilu_refany info, char *buf, ilu_cardinal bufsize,
				 ILU_ERRS((internal, bad_param)) *err)
{
  struct endpoint_identity_info_s *eii = (struct endpoint_identity_info_s *) info;
  unsigned int needed;

  needed = 16 + strlen(eii->id);
  if (bufsize < needed)
    return (ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_small_buffer, needed));
  else
    {
      ILU_CLER(*err);
      return sprintf(buf, "w3mux_endpoint=%s", eii->id);
    }      
}			 
				 
static ilu_refany
  endpoint_identity_duplicate_data (ilu_refany info, ilu_Error *err)
{
  struct endpoint_identity_info_s *oldi = (struct endpoint_identity_info_s *) info;
  struct endpoint_identity_info_s *newi;
  newi = ilu_MallocE(sizeof(*newi) + strlen(oldi->id) + 1, err);
  if (ILU_ERRNOK(*err)) return NIL;
  newi->id = ((char *) newi) + sizeof(*newi);
  strcpy (newi->id, oldi->id);
  return (ilu_refany) newi;
}

static void
  endpoint_identity_free_data (ilu_refany info, ILU_ERRS((internal)) *err)
{
  ilu_free(info);
}

struct _ilu_IdentityType_s ilu_w3muxEndpointIdentity_s = {
  "w3mux_Endpoint",
  endpoint_identity_string_form,
  endpoint_identity_duplicate_data,
  endpoint_identity_free_data,
  NULLFN,		/* pickle */
  NULLFN		/* unpickle */
  };

ilu_IdentityInfo
  ilu_AcquireW3muxEndpointIndentity (char *id, ilu_Error *err)
{
  Endpoint ep;
  ilu_IdentityInfo info;
  struct endpoint_identity_info_s temp;
  struct endpoint_identity_info_s *epi;
	
  w3mux_InitializeDataStructures(err);
  if (ILU_ERRNOK(*err)) return NIL;

  info = (ilu_IdentityInfo) ilu_MallocE(sizeof(*info), err);
  if (ILU_ERRNOK(*err)) return NIL;
  if (id == NIL) {
    /* use default endpoint */
    ENTER_GLOBLOCK();
    ep = endpoint_get (NIL, ilu_TRUE, err);
    exit_globlock();
    if (ILU_ERRNOK(*err)) return NIL;
    temp.id = ep->id;
  } else 
    temp.id = id;
  epi = endpoint_identity_duplicate_data (&temp, err);
  if (ILU_ERRNOK(*err)) { ilu_free(info); return NIL; }
  info->ii_type = &ilu_w3muxEndpointIdentity_s;
  info->ii_owned_by_passport = ilu_FALSE;
  info->ii_info = (ilu_refany) epi;
  return info;
}

/***********************************************************************/
/***********************************************************************/
/**************************  Methods on LTt  ***************************/
/***********************************************************************/
/***********************************************************************/

static void _free_atom (void *p)
{
  Atom a = (Atom) p;
  ilu_free(p);
}

static void ltt_free (LTt ltt)
{
  ilu_Error lerr;
  int i;
  ILU_NOTE(W3MUX_DEBUG,
	   ("ilu_w3mux(ltt_free):  \"%s\", pending_conn_reqs = %lu\n",
	    ltt->description, ltt->pending_used));
#if 0
  for (i = 0;  i < ltt->pending_used;  i++)
    inbuf_free(ltt->pending_conn_reqs[i]);
  ilu_free(ltt->pending_conn_reqs);
#endif
  ilu_hash_FreeHashTable(ltt->interned_atoms_remote, NULLFN, _free_atom);
  ilu_hash_FreeHashTable(ltt->interned_atoms_local, NULLFN, _free_atom);
  _ilu_vector_destroy(ltt->sessions, NULLFN);
  lerr = ilu_DestroyCondition(ltt->x_CV);
  ILU_HANDLED(lerr);
  ilu_DestroyMutex(ltt->lock, &lerr);
  ILU_HANDLED(lerr);
  ilu_free(ltt->description);
  ilu_free(ltt);
  return;
}

/* holding ltt->lock */
static void ltt_add_session (LTt ltt, Mt mt, ilu_Error *err)
{
  _ilu_vector_add(ltt->sessions, mt, err);
}

/* holding ltt->lock */
static Mt ltt_find_session (LTt ltt, ilu_cardinal session)
{
  int i, len;
  Mt * sessions;

  sessions = (Mt *) _ilu_vector_elements(ltt->sessions);
  len = _ilu_vector_size(ltt->sessions);

  for (i = 0;  i < len;  i++) {
    if (sessions[i]->session == session)
      return sessions[i];
  }
  return NIL;
}

/* holding ltt->lock */
static Mt
  ltt_allocate_session (LTt ltt, ilu_Error *err)
{
  Mt mt;
  ilu_cardinal session = ltt->initiator ? 2 : 3;
  /* first, pick a number */
  for (; session <= 255;  session += 2) {
    if (ltt_find_session(ltt, session) == NIL)
      break;
  }
  /* now, create the corresponding Mux transport */
  mt = mt_create(ltt, session, err);
  if (ILU_ERRNOK(*err)) return NIL;
  _ilu_vector_add(ltt->sessions, (void *) mt, err);
  if (ILU_ERRNOK(*err)) { mt_free(mt); return NIL; };
  return mt;
}

/* holding ltt->lock, holding globlock */
static void
  ltt_deallocate_session (LTt ltt, Mt mt)
{
  _ilu_vector_remove(ltt->sessions, (void *) mt);
  mt_free(mt);
}

static void ltt_lock (LTt ltt)
{
  ilu_AcquireMutex(ltt->lock);
}
     
static void ltt_unlock (LTt ltt)
{
  W3MUX_RELEASE_MUTEX(ltt->lock);
}

/* inside lock(ltt) */
static ilu_boolean ltt_acquire_x (LTt ltt)
{
  ilu_Error lerr;
  if (ltt->source == NIL) return ilu_FALSE;
  for (; ltt->x_held; ) {
    ilu_CMWait1(ltt->x_CV, ltt->lock, &lerr);
    ILU_MUST_BE_SUCCESS(lerr);
    if (ltt->source == NIL) return ilu_FALSE;
  }
  ltt->x_held = ilu_TRUE;
  return ilu_TRUE;
}

/* inside lock(ltt) */
static void ltt_acquire_x_2 (LTt ltt)
{
  ilu_Error lerr;
  for (; ltt->x_held; ) {
    ilu_CMWait2(ltt->x_CV, ltt->lock, ilu_cmu, &lerr);
    ILU_MUST_BE_SUCCESS(lerr);
  }
  ltt->x_held = ilu_TRUE;
}

/* inside lock(ltt) */
static void ltt_release_x (LTt ltt)
{
  ilu_Error lerr;
  ltt->x_held = ilu_FALSE;
  ilu_CondNotify(ltt->x_CV, &lerr);
  ILU_MUST_BE_SUCCESS(lerr);
}

/* ...holding ltt->lock... */
static void ltt_setup_data_chunk_read (LTt ltt)
{
  Mt mt;
  ilu_Error lerr;
  ilu_cardinal size, padding;

  /* The basic action here is to allocate a buffer to read the data chunk into.
     This buffer may be either user-supplied (when reading large single values,
     such as big strings) or allocated.  In either case, we wrap the buffer in
     an InBuf struct which contains information about the buffer. */

  mt = ltt_find_session(ltt, HEADER_SESSION_ID(&ltt->current_header));

  /* Is there a pending user-supplied input buffer?  If so, this must be the
     second or third chunk to be read in that buffer. */

  if (ltt->current_buffer != NIL) { /* buffer already there, must be user-supplied
				       single value split across multiple MUX fragments */

    /* continue to use pre-allocated InBuf in ltt->current_buffer,
       instead of allocating a new one */
    /* figure out how much of chunk to put in buffer */
    size = MIN((HEADER_CHUNK_SIZE(&ltt->current_header)),
	       (ltt->current_buffer->size - ltt->current_buffer->used));
    ILU_NOTE(W3MUX_DEBUG,
	     ("ilu_w3mux(ltt_setup_data_chunk_read \"%s\"):  "
	      "reading %lu bytes into caller buffer %p, size=%lu, used=%lu\n",
	      ltt->description, (unsigned long) size, ltt->current_buffer->bytes,
	      ltt->current_buffer->size, ltt->current_buffer->used));
    /* ...and remember that by modifying header size... */
    DECREMENT_HEADER_SIZE(&ltt->current_header, size);
    /* ...figure out flags properly for this chunk... */
    if (HEADER_CHUNK_SIZE(&ltt->current_header) > 0) {
      /* splitting chunk across two buffers */
      ltt->current_buffer->header.short_header &= ~MUX_SYN;
      ltt->current_buffer->header.short_header &= ~MUX_FIN;
      ltt->current_buffer->header.short_header &= ~MUX_RST;
      ltt->current_buffer->header.short_header &= ~MUX_PUSH;
      padding = 0;
    } else {
      /* last buffer of this chunk */
      ltt->current_buffer->header.short_header &= ~MUX_SYN;
      ltt->current_buffer->header.short_header &= ~MUX_RST;
      padding = FIGURE_PADDING_1(size, &ltt->current_header);
    }
    /* ...and set up read instructions... */
    ltt->where = ltt->current_buffer->bytes + ltt->current_buffer->used;
    ltt->to_read = size;
    ltt->reading_padding = ilu_FALSE;
    ltt->padding = padding;
    
  } else if ((mt != NIL) &&
	     (mt->ltt_locked.input_state.user_buffer != NIL)) {

    /* buffer there, but so far unused */
    /* input buffer may be smaller or larger than chunk --
       figure out how much of chunk to put in buffer */
    size = MIN((HEADER_CHUNK_SIZE(&ltt->current_header)),
	       mt->ltt_locked.input_state.user_buffer_size);
    ILU_NOTE(W3MUX_DEBUG,
	     ("ilu_w3mux(ltt_setup_data_chunk_read \"%s\"):  "
	      "reading %lu bytes into caller buffer %p, size=%lu, used=0\n",
	      ltt->description, (unsigned long) size, ltt->current_buffer->bytes, ltt->current_buffer->size));
    /* ...and remember that by modifying header size... */
    DECREMENT_HEADER_SIZE(&ltt->current_header, size);
    /* ...and get InBuf header for buffer... */
    ltt->current_buffer = inbuf_get_linked_buffer(&ltt->current_header, ltt, &lerr);
    ILU_MUST_BE_SUCCESS(lerr);
    /* ...figure out flags properly for this chunk... */
    if (HEADER_CHUNK_SIZE(&ltt->current_header) > 0) {
      /* splitting chunk across two buffers */
      ltt->current_buffer->header.short_header &= ~MUX_PUSH;
      ltt->current_buffer->header.short_header &= ~MUX_FIN;
      padding = 0;
    } else {
      padding = FIGURE_PADDING_1(size, &ltt->current_header);
    }
    /* ...and transfer buffer from "mt" to "ltt"... */
    ltt->current_buffer->bytes = mt->ltt_locked.input_state.user_buffer;
    ltt->current_buffer->size = mt->ltt_locked.input_state.user_buffer_size;
    ltt->current_buffer->used = 0;
    mt->ltt_locked.input_state.user_buffer = NIL;
    /* ...and set up read instructions... */
    ltt->where = ltt->current_buffer->bytes;
    ltt->to_read = size;
    ltt->reading_padding = ilu_FALSE;
    ltt->padding = padding;

  } else {				/* no user buffer */

    /* allocate header with attached buffer space for whole chunk... */
    ltt->current_buffer = inbuf_get_full_buffer(&ltt->current_header, ltt, &lerr);
    ILU_MUST_BE_SUCCESS(lerr);
    /* ...remember that we promised to read the whole thing... */
    DECREMENT_HEADER_SIZE(&ltt->current_header, ltt->current_buffer->size);
    _ilu_Assert(HEADER_CHUNK_SIZE(&ltt->current_header) == 0, "bad header chunk size in ltt_setup_data_chunk_read");
    /* ...and set up read instructions... */
    ltt->where = ltt->current_buffer->bytes;
    ltt->to_read = ltt->current_buffer->size;
    ltt->reading_padding = ilu_FALSE;
    ltt->padding = FIGURE_PADDING_1(ltt->current_buffer->size, &ltt->current_header);
  }
}

static ilu_boolean ltt_zero_data_control_msg (Header_s *header)
{
  return (HEADER_CONTROL_MSG(header) &&
	  ((HEADER_CONTROL_CODE(header) == MUX_SetMSS) ||
	   (HEADER_CONTROL_CODE(header) == MUX_AddCredit) ||
	   (HEADER_CONTROL_CODE(header) == MUX_SetDefaultCredit)));
}

static void ltt_handle_control_message (LTt ltt)
{
  ilu_Error lerr;

  switch (HEADER_CONTROL_CODE(&ltt->current_header)) {
  case MUX_InternAtom:
    {
      /* We set up the buffer specially for this message so that it includes space
	 for the ilu_CharBuf, and for the string itself.  */
      ilu_cardinal atomvalue = MUX_ATOM_CHANNEL_MIN + HEADER_SESSION_ID(&ltt->current_buffer->header);
      ilu_cardinal namelen = HEADER_CHANNEL_ID(&ltt->current_buffer->header);
      ilu_CharBuf *cb;

      cb = (ilu_CharBuf *) ilu_hash_FindInTable(ltt->interned_atoms_remote, (void *) atomvalue);
      if (cb != NIL) {
	ILU_NOTE(W3MUX_DEBUG,
		 ("ilu_w3mux(ltt_handle_control_message \"%s\"):  "
		  "redefining atom %lu, formerly \"%*.*s\", to \"%*.*s\"\n",
		  ltt->description, (unsigned long) atomvalue,
		  cb->icb_len, cb->icb_len, cb->icb_base,
		  namelen, namelen, ltt->current_buffer->bytes));
	ilu_hash_RemoveFromTable(ltt->interned_atoms_remote, (void *) atomvalue);
	ilu_free(cb);
      } else {
	ILU_NOTE(W3MUX_DEBUG,
		 ("ilu_w3mux(ltt_handle_control_message \"%s\"):  "
		  "atom %lu as \"%*.*s\"\n",
		  ltt->description, (unsigned long) atomvalue,
		  namelen, namelen, ltt->current_buffer->bytes));
      }
      cb = (ilu_CharBuf *) (ltt->current_buffer->bytes - sizeof(ilu_CharBuf));
      cb->icb_base = (char *) ltt->current_buffer->bytes;
      cb->icb_size = ltt->current_buffer->used;
      cb->icb_len = namelen;
      cb->icb_base[namelen] = 0;
      ilu_hash_AddToTable (ltt->interned_atoms_remote, (void *) atomvalue, (void *) cb);
      inbuf_free(ltt->current_buffer);
      ltt->current_buffer = NIL;
    }    
    break;
  case MUX_DefineEndpoint:
    {
      ltt->current_buffer->bytes[HEADER_CHANNEL_ID(&ltt->current_buffer->header)] = 0;
      ILU_NOTE(W3MUX_DEBUG,
	       ("ilu_w3mux(ltt_handle_control_message \"%s\"):  DefineEndpoint <%s>\n",
		ltt->description, ltt->current_buffer->bytes));
      ENTER_GLOBLOCK();
      ltt->endpoint = endpoint_get ((ilu_string) (ltt->current_buffer->bytes), ilu_FALSE, &lerr);
      if (ILU_ERRNOK(lerr)) {
	ILU_NOTE(W3MUX_DEBUG,
		 ("ILU(w3mux:ltt_handle_control_message \"%s\"):  "
		  "DefineEndpoint -- can't get endpoint, error %s\n",
		  ltt->description, ILU_ERR_NAME(lerr)));
	ILU_HANDLED(lerr);
      } else {
	endpoint_add_connection (ltt->endpoint, ltt, &lerr);
	if (ILU_ERRNOK(lerr)) {
	  ILU_NOTE(W3MUX_DEBUG,
		   ("ILU(w3mux:ltt_handle_control_message \"%s\"):  "
		    "DefineEndpoint -- can't add ltt to endpoint, error %s\n",
		  ltt->description, ILU_ERR_NAME(lerr)));
	  ILU_HANDLED(lerr);
	}
      }
      exit_globlock();
      ilu_free(ltt->current_buffer->bytes);
      inbuf_free(ltt->current_buffer);
      ltt->current_buffer = NIL;
    }
    break;
  case MUX_SetMSS:
    {
      /* note that this may have no associated current_buffer value */
      if (HEADER_SESSION_ID(&ltt->current_header) != 0) {
	ILU_NOTE(W3MUX_DEBUG,
		 ("ilu_w3mux(ltt_handle_control_message \"%s\"):  SetMSS "
		  "control message received with non-zero session ID %lu\n",
		  ltt->description, (unsigned long) HEADER_SESSION_ID(&ltt->current_header)));
      } else {
	ILU_NOTE(W3MUX_DEBUG,
		 ("ilu_w3mux(ltt_handle_control_message \"%s\"):  SetMSS to %lu\n",
		  ltt->description,
		  (unsigned long) HEADER_MAX_FRAGMENT_SIZE(&ltt->current_header)));
      }
      ltt->max_frag_size = HEADER_MAX_FRAGMENT_SIZE(&ltt->current_header);
    }
    break;
  case MUX_AddCredit:
    {
      /* note that this may have no associated current_buffer value */
      /* XXX this needs to be reworked so that addcredit for not-yet-accepted connections
	 can be handled properly */
      Mt mt;
      mt = ltt_find_session(ltt, HEADER_SESSION_ID(&ltt->current_header));
      if (mt == NIL) {
	ILU_NOTE(W3MUX_DEBUG,
		 ("ilu_w3mux(ltt_handle_control_message \"%s\"):  AddCredit "
		  "received for non-existent session %lu (credit of %lu bytes)\n",
		  ltt->description,
		  (unsigned long) HEADER_SESSION_ID(&ltt->current_header),
		  (unsigned long) HEADER_CREDIT_SIZE(&ltt->current_header)));
      } else {
	ILU_NOTE(W3MUX_DEBUG,
		 ("ilu_w3mux(ltt_handle_control_message \"%s\"):  AddCredit "
		  "for session %lu of %lu bytes\n",
		  ltt->description,
		  (unsigned long) HEADER_SESSION_ID(&ltt->current_header),
		  (unsigned long) HEADER_CREDIT_SIZE(&ltt->current_header)));
	if (HEADER_CREDIT_SIZE(&ltt->current_header) == 0) {
	  mt->ltt_locked.output_state.credit_limited = ilu_FALSE;
	  mt->ltt_locked.output_state.credit = 0;
	} else {
	  mt->ltt_locked.output_state.credit_limited = ilu_TRUE;
	  mt->ltt_locked.output_state.credit += HEADER_CREDIT_SIZE(&ltt->current_header);
	}
	ilu_CondNotify(mt->ltt_locked.wait_CV, &lerr);
	if (ILU_ERRNOK(lerr)) {
	  ILU_NOTE(W3MUX_DEBUG,
		   ("ilu_w3mux(ltt_handle_control_message \"%s\":  AddCredit:  ilu_CondNotify raised <%s>\n",
		    ltt->description, ILU_ERR_NAME(lerr)));
	  ILU_HANDLED(lerr);
	}
      }
    }
    break;
  case MUX_SetDefaultCredit:
    {
      /* note that this may have no associated current_buffer value */
      if (HEADER_SESSION_ID(&ltt->current_header) != 0) {
	ILU_NOTE(W3MUX_DEBUG,
		 ("ilu_w3mux(ltt_handle_control_message \"%s\"):  SetDefaultCredit "
		  "received with non-zero session ID %lu\n",
		  ltt->description, (unsigned long) HEADER_SESSION_ID(&ltt->current_header)));
      } else {
	ILU_NOTE(W3MUX_DEBUG,
		 ("ilu_w3mux(ltt_handle_control_message \"%s\"):  SetDefaultCredit to %lu\n",
		  ltt->description,
		  (unsigned long) HEADER_DEFAULT_CREDIT_SIZE(&ltt->current_header)));
      }
      ltt->default_credit = HEADER_DEFAULT_CREDIT_SIZE(&ltt->current_header);
    }
    break;
  case MUX_NoOp:
    {
      /* May or may not have associated data */
      ILU_NOTE(W3MUX_DEBUG,
	       ("ilu_w3mux(ltt_handle_control_message \"%s\":  NoOp received\n",
		ltt->description));
      if (ltt->current_buffer != NIL) {
	inbuf_free(ltt->current_buffer);
	ltt->current_buffer = NIL;
      }
    }
    break;
  default:
    {
      /* May or may not have associated data */
      ILU_NOTE(W3MUX_DEBUG,
	       ("ilu_w3mux(ltt_handle_control_message \"%s\":  Unknown control "
		"msg %lu received and ignored\n",
		ltt->description, HEADER_CONTROL_CODE(&ltt->current_header)));
      if (ltt->current_buffer != NIL) {
	inbuf_free(ltt->current_buffer);
	ltt->current_buffer = NIL;
      }
    }
    break;
  }
}

/* Writes bytes, if any, waiting in ltt->preBytes. */
/* mayBlock ? Main Invariant : L1.sup < trmu; L2 >= {ltt's x mutex} */
static          ilu_boolean
ClearPre(LTt ltt, ilu_boolean mayBlock,
	 ILU_ERRS((IoErrs)) * err)
{
  ilu_cardinal    took;
  if (ltt->preNext == 0)
    return ILU_CLER(*err);
  took = transport_write_bytes_maybeblock(ltt->source,
				     ltt->preBytes + ltt->preFirst,
				      ltt->preNext - ltt->preFirst,
					  mayBlock, err);
  ltt->preFirst += took;
  if (ltt->preFirst == ltt->preNext) {
    ltt->preFirst = ltt->preNext = 0;
    return ILU_ERROK(*err);
  }
  if (ILU_ERROK(*err))
    ilu_Check(!mayBlock, err);
  return ilu_FALSE;
}

/* holding ltt's x mutex and lock */
static void
  ltt_send_endpoint (LTt ltt)
{
  Header_s        msg;
  ilu_Error       lerr;
  unsigned        padded_len, len;

  if (ltt->my_endpoint == NIL)
    return;
  ILU_NOTE(W3MUX_DEBUG,
	   ("ILU(w3mux.c:ltt_send_endpoint \"%s\"):  <%s>\n",
	    ltt->description, ltt->my_endpoint->id));
  _ilu_Assert(ClearPre(ltt, ilu_TRUE, &lerr),
	      "can't clear pre-queue in ltt_send_endpoint");
  len = strlen(ltt->my_endpoint->id);
  padded_len = ROUND_UP(len, 4);
  msg.short_header = ((1 << MUX_CONTROL_SHIFT) |
		   (MUX_DefineEndpoint << MUX_CONTROL_CODE_SHIFT) |
		      len);
  /* flip if necessary */
  SWAP_IF_NEEDED(msg.short_header);
  transport_write_bytes(ltt->source, (ilu_bytes) & msg.short_header,
			sizeof(msg.short_header), &lerr);
  ILU_MUST_BE_SUCCESS(lerr);
  transport_write_bytes(ltt->source, (ilu_bytes) ltt->my_endpoint->id,
			padded_len, &lerr);
  ILU_MUST_BE_SUCCESS(lerr);
}

/* holding ltt->lock */
static void
  ltt_add_pending (LTt ltt, InBuf pending, ilu_Error *err)
{
  if (ltt->pending_used == ltt->pending_size) {
    /* need more entries */
    InBuf *newarray;
    ilu_cardinal newsize;
    if (ltt->pending_size == 0) {
      newarray = (InBuf *) ilu_MallocE(sizeof(InBuf) * (newsize = 2), err);
    } else {
      newarray = (InBuf *) ilu_ReallocE(ltt->pending_conn_reqs, sizeof(InBuf) * (newsize = ltt->pending_size * 2), err);
    }
    if (ILU_ERRNOK(*err)) return;
    ltt->pending_conn_reqs = newarray;
    ltt->pending_size = newsize;
  };
  ltt->pending_conn_reqs[ltt->pending_used] = pending;
  ltt->pending_used += 1;
  ILU_CLER(*err);
  return;
}

/* holding ltt->lock */
static void
  ltt_remove_pending (LTt ltt, InBuf pending, ilu_Error *err)
{
  ilu_cardinal i, j;

  for (i = 0;  i < ltt->pending_used;  i++) {
    if (pending == ltt->pending_conn_reqs[i]) {
      ltt->pending_used -= 1;
      for (j = i;  j < ltt->pending_used;  j++) {
	ltt->pending_conn_reqs[j] = ltt->pending_conn_reqs[j + 1];
      }
      ILU_CLER(*err);
      return;
    }
  }
  ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0);
  return;
}

/* holding ltt->lock */
static InBuf
  ltt_most_recent_pending (LTt ltt, ilu_cardinal session)
{
  /* this routine searches for the "most recent" session that's pending,
     so it searches backward in the array.  It is used by the LTt worker
     thread to find pending sessions to add data chunks to.
  */

  int i;

  if (ltt->pending_used < 1)
    return NIL;
  for (i = ltt->pending_used - 1;  i >= 0;  i--) {
    if (HEADER_SESSION_ID(&(ltt->pending_conn_reqs[i])->header) == session)
      return ltt->pending_conn_reqs[i];
  }
  return NIL;
}

/* holding ltt->lock */
static InBuf
  ltt_find_pending_by_channel (LTt ltt, ilu_cardinal channel)
{
  /* this routine searches for the first pending conn req that is for
     the channel specified by channel.
  */

  ilu_cardinal i;

  for (i = 0;  i < ltt->pending_used;  i += 1) {
    if (HEADER_CHANNEL_ID(&(ltt->pending_conn_reqs[i])->header) == channel)
      return ltt->pending_conn_reqs[i];
  }
  return NIL;
}

/* holding ltt->lock */
static void
  ltt_add_current_to_inbuf (LTt ltt, InBuf inbuf)
{
  InBuf ptr, last;
  for (ptr = inbuf, last = NIL;  ptr != ILU_NIL;  last = ptr, ptr = ptr->next)
    ;
  last->next = ltt->current_buffer;
  ltt->current_buffer->next = NIL;
  ltt->current_buffer = NIL;
}

/* holding ltt->lock */
static Atom
  ltt_find_atom (LTt ltt, ilu_cardinal atom_id)
{
  return ((Atom) ilu_hash_FindInTable (ltt->interned_atoms_remote, (void *) atom_id));
}

/* holding ltt->lock */
static ilu_cardinal
  ltt_intern_atom (LTt ltt, ilu_string atom_name, ILU_ERRS((no_memory)) *err)
{
  AtomDef a;
  Header_s msg;
  unsigned padded_len, len;

  if ((a = ilu_hash_FindInTable(ltt->interned_atoms_local, (void *) atom_name)) != NIL)
    return a->atom_id;
  else {
    len = strlen(atom_name);
    padded_len = ROUND_UP(len, 4);
    a = ilu_MallocE(sizeof(*a) + padded_len + 1, err);
    if (ILU_ERRNOK(*err)) return 0;
    a->atom_name = (char *) (a + 1);
    memset(a->atom_name, 0, padded_len);
    strcpy (a->atom_name, atom_name);
    a->atom_id = ltt->interned_atoms_local_id;
    if (a->atom_id <= MUX_ATOM_CHANNEL_MAX) {
      if (ltt_acquire_x(ltt)) {
	if (!ClearPre(ltt, ilu_TRUE, err)) goto free1;
	ILU_NOTE(W3MUX_DEBUG,
		 ("ilu_w3mux(ltt_intern_atom):  LTt \"%s\" interning \"%s\" as %lu\n",
		  ltt->description, atom_name, a->atom_id));
	msg.short_header = ((1 << MUX_CONTROL_SHIFT) |
			    (MUX_InternAtom << MUX_CONTROL_CODE_SHIFT) |
			    ((a->atom_id - MUX_ATOM_CHANNEL_MIN) << MUX_SESSION_SHIFT) |
			    (len << MUX_FRAGMENT_SIZE_SHIFT));
	/* flip if necessary */
	SWAP_IF_NEEDED(msg.short_header);
	transport_write_bytes(ltt->source, (ilu_bytes) & msg.short_header,
			      sizeof(msg.short_header), err);
	if (ILU_ERRNOK(*err)) goto free1;
	transport_write_bytes(ltt->source, (ilu_bytes) atom_name,
			      padded_len, err);
      free1:
	ltt_release_x(ltt);
      } else {
	ILU_ERR_CONS1(comm_failure, err, minor, ilu_cfm_conn_lost, 0);
      }
    } else {
      ILU_ERR_CONS1(no_resources, err, minor, ilu_nrm_mux_atom_id, 0);
    }
    if (ILU_ERRNOK(*err))
      { ilu_free(a); return 0; }
    else {
      ilu_hash_AddToTable (ltt->interned_atoms_local, a->atom_name, a);
      ltt->interned_atoms_local_id++;
      return a->atom_id;
    }
  }
}

static void ltt_workproc (void *rock)
{
  LTt ltt = (LTt) rock;
  ilu_Error lerr;
  int dfd;
  ilu_cardinal size, i, n;
  Mt mt;
  Mt * sessions;
  ilu_TransportReport report = { ilu_FALSE, ilu_FALSE };
  ilu_byte padding_buf[8];

  ilu_Transport lower;

  ILU_NOTE(W3MUX_DEBUG,
	   ("ILU(w3mux.c:ltt_workproc):  starting worker thread for LTt <%s>; my_endpoint = <%s>\n",
	    ltt->description, (ltt->my_endpoint == NIL) ? "" : ltt->my_endpoint->id));

  ltt_lock (ltt);
  (void) ltt_acquire_x(ltt);

  if (ltt->initiator && (ltt->my_endpoint != NIL)) {
    ltt_send_endpoint (ltt);
  };

  /* process input while the underlying mooring is open OR any
     sessions exist over the LTt */

  /* set up to read first message */
  ltt->where = (ilu_bytes) &ltt->current_header;
  ltt->to_read = sizeof(ltt->current_header.short_header);
  ltt->padding = 0;
  ltt->reading_header = ilu_TRUE;
  ltt->reading_long_len = ilu_FALSE;
  ltt->reading_padding = ilu_FALSE;
  ltt->current_buffer = NIL;
  ltt->current_header.short_header = 0;

  while (ltt->m_open || (_ilu_vector_size(ltt->sessions) > 0)) {
    /* L1 = {ltt}; L2 = {ltt.mx, ltt.my}; Main Remnant */
    LTT_UNLOCK(ltt);

    /* read available bytes from lower, up to at most
     * (a) end of current chunk header, if working on chunk header, else
     * (b) end of current chunk body.
     * The number of bytes to read is kept in ltt->to_read.
     * The buffer to read into is kept in ltt->where.
     */
    _ilu_Assert((ltt->where != NIL) && (ltt->source != NIL), "bad read in ltt_workproc");

    if (ltt->to_read > 0) {
      size = transport_read_upto_bytes(ltt->source, ltt->where, ltt->to_read, &report, &lerr);
    } else {
      size = 0;
      ILU_CLER(lerr);
      report.tr_eof = ilu_FALSE;
      report.tr_eom = ilu_FALSE;
    }

    if (ILU_ERRNOK(lerr)) {			/* Error on read */
      ILU_NOTE(W3MUX_DEBUG,
	       ("ilu_w3mux(ltt_workproc \"%s\"):  transport_read_upto_bytes signals err '%s'\n",
		ltt->description, ILU_ERR_NAME(lerr)));
      ILU_HANDLED(lerr);
      /* XXX handle error somehow */
      ltt_lock(ltt);
      break;
    } else if (report.tr_eof) {			/* EOF on read */
      ILU_NOTE(W3MUX_DEBUG,
	       ("ilu_w3mux(ltt_workproc \"%s\"):  other side closed connection\n",
		ltt->description, ILU_ERR_NAME(lerr)));
      ILU_HANDLED(lerr);
      ltt_lock(ltt);
      break;
    } else if (((ltt->to_read > 0) && (size == 0)) || report.tr_eom) {	/* nothing there */
      int disabled = 0;
      ltt_lock(ltt);
      ltt_release_x(ltt);
      LTT_UNLOCK(ltt);
      (*ltt->source->tr_class->tc_wait_for_input)(ltt->source, &disabled, NIL, &lerr);
      ltt_lock(ltt);
      ltt_acquire_x(ltt);
      ILU_ERR_SWITCH (lerr) {
	ILU_SUCCESS_CASE {
	  if (disabled) {
	    /* XXX what to do here? */
	  }
	  ILU_HANDLED(lerr);
	}
	ILU_ERR_CASE(interrupted, e) {
	  /* go around again */
	  ILU_HANDLED(lerr);
	}
	ILU_ERR_ELSE {
	  ILU_NOTE(W3MUX_DEBUG,
		   ("ilu_w3mux(ltt_workproc \"%s\"):  Error <%s> while waiting for input\n",
		    ltt->description, ILU_ERR_NAME(lerr)));
	  ILU_HANDLED(lerr);
	}
      } ILU_ERR_ENDSWITCH;	
    } else {					/* bytes read -- process them */
#ifdef ILU_W3MUX_EXCESS_DEBUGGING
	    ILU_NOTE(W3MUX_DEBUG,
		     ("ilu_w3mux(ltt_workproc \"%s\"):  got %lu bytes for session %lu into %p\n",
		      ltt->description, size, HEADER_SESSION_ID(&ltt->current_header), ltt->where));
#endif

      ltt->to_read -= size;
      ltt->where += size;
      if (ltt->to_read > 0) {			/* still more bytes to read -- reloop */
	ltt_lock(ltt);
      } else {					/* filled some buffer -- process it */

	if (ltt->reading_padding) {		/* got some padding; discard it and re-read header */
	  _ilu_Assert((ltt->padding == 0) &&
		      (!ltt->reading_header) &&
		      (!ltt->reading_long_len), "w3mux: bad state in handling padding");
	  ltt->reading_padding = ilu_FALSE;

	  ltt->reading_header = ilu_TRUE;
	  ltt->reading_long_len = ilu_FALSE;
	  ltt->where = (ilu_bytes) &ltt->current_header.short_header;
	  ltt->to_read = sizeof(ltt->current_header.short_header);

	  ltt_lock(ltt);
	  continue;	/* go back to top of loop and read */

	} else if (ltt->reading_header) {		/* reading a chunk header */
	  if (ltt->reading_long_len) {		/* finished long header */
	    SWAP_IF_NEEDED(ltt->current_header.long_length);
	    ltt->reading_long_len = ilu_FALSE;
	  } else {				/* finished regular header */
	    SWAP_IF_NEEDED(ltt->current_header.short_header);
#ifdef ILU_W3MUX_EXCESS_DEBUGGING
	    /* check data chunks for validity of session */
	    {
	      ilu_cardinal session = HEADER_SESSION_ID(&ltt->current_header);
	      /* could be as-yet-unaccepted connection */
	      _ilu_Assert (HEADER_CONTROL_MSG(&ltt->current_header) ||
			   HEADER_NEW_SESSION(&ltt->current_header) ||
			   (ltt_most_recent_pending (ltt, session) != NIL) ||
			   (ltt_find_session(ltt, session) != NIL),
			   "w3mux:  header for bad session");
	    }
#endif /* def ILU_W3MUX_EXCESS_DEBUGGING */
						/* if extended, read long length */
	    if (HEADER_EXTENDED(&ltt->current_header)) {
	      ltt->reading_long_len = ilu_TRUE;
	      ltt->where = (ilu_bytes) &ltt->current_header.long_length;
	      ltt->to_read = sizeof(ltt->current_header.long_length);
	      ltt_lock(ltt);
	      continue;	/* go back to top of loop and read */
	    }
	  }
	  ltt->reading_header = ilu_FALSE;
	  DISPLAY_HEADER(&ltt->current_header);

	  /* at this point, we've read the full header, including the long_length,
	     if necessary.  Now we look at the header and process it. */

	  ltt_lock(ltt);
	  if (ltt_zero_data_control_msg(&ltt->current_header)) {/* handle control messages */
	    ltt_handle_control_message (ltt);
	    /* Then set up read instructions for next read. */
	    ltt->reading_header = ilu_TRUE;
	    ltt->where = (ilu_bytes) (&ltt->current_header.short_header);
	    ltt->to_read = sizeof(ltt->current_header.short_header);

	  } else if (HEADER_CONTROL_MSG(&ltt->current_header) &&
		     (HEADER_CONTROL_CODE(&ltt->current_header) == MUX_InternAtom)) {
	    /* set up input buffer to be reused later */
	    unsigned atom_size;
	    ilu_bytes buf;

	    atom_size = HEADER_ATOM_STR_LENGTH(&ltt->current_header);
	    buf = (ilu_bytes) ilu_MallocE(atom_size + 1 + sizeof(ilu_CharBuf), &lerr);
	    ILU_MUST_BE_SUCCESS(lerr);
	    ltt->current_buffer = inbuf_get_linked_buffer(&ltt->current_header, ltt, &lerr);
	    ILU_MUST_BE_SUCCESS(lerr);
	    ltt->current_buffer->bytes = buf + sizeof(ilu_CharBuf);
	    ltt->current_buffer->size = atom_size;
	    /* ...and set up read instructions... */
	    ltt->where = ltt->current_buffer->bytes;
	    ltt->to_read = ltt->current_buffer->size;
	    ltt->current_buffer->used = ltt->current_buffer->size;
	    ltt->padding = FIGURE_PADDING_1(atom_size, &ltt->current_header);
	    DECREMENT_HEADER_SIZE(&ltt->current_header, atom_size);

	  } else if (HEADER_CONTROL_MSG(&ltt->current_header) &&
		     (HEADER_CONTROL_CODE(&ltt->current_header) == MUX_DefineEndpoint)) {
	    /* set up input buffer to be reused later */
	    unsigned endpoint_size;
	    ilu_bytes buf;
	    endpoint_size = HEADER_ENDPOINT_LENGTH(&ltt->current_header);
	    buf = ilu_MallocE(endpoint_size + 1, &lerr);
	    ILU_MUST_BE_SUCCESS(lerr);
	    ltt->current_buffer = inbuf_get_linked_buffer(&ltt->current_header, ltt, &lerr);
	    ILU_MUST_BE_SUCCESS(lerr);
	    ltt->current_buffer->used = 0;
	    ltt->current_buffer->bytes = buf;
	    ltt->current_buffer->size = endpoint_size;
	    /* ...and set up read instructions... */
	    ltt->where = ltt->current_buffer->bytes;
	    ltt->to_read = ltt->current_buffer->size;
	    ltt->current_buffer->used = ltt->current_buffer->size;
	    ltt->padding = FIGURE_PADDING_1(endpoint_size, &ltt->current_header);
	    DECREMENT_HEADER_SIZE(&ltt->current_header, endpoint_size);

	  } else {						/* handle data chunks */
	    ltt_setup_data_chunk_read(ltt);
	  }

	/* So much for reading headers.  In this code, we have read a data chunk,
	   and we process it. */

	} else {
	  ltt_lock(ltt);
	  if (HEADER_CONTROL_MSG(&ltt->current_header)) {
	    /* we've read a control message with associated data,
	       which we always read completely, so process it */
	    ltt_handle_control_message(ltt);
	  } else {
	    /* data chunk for some session */
	    if (HEADER_NEW_SESSION(&ltt->current_header)) {	/* syn bit set? */
	      Mm mm = NIL;
	      Atom channel_name;
	      ilu_cardinal channel = HEADER_CHANNEL_ID(&ltt->current_header);
	      LTT_UNLOCK(ltt);
	      ENTER_GLOBLOCK();
	      if ((channel >= MUX_ATOM_CHANNEL_MIN) && (channel <= MUX_ATOM_CHANNEL_MAX)) {
		channel_name = ltt_find_atom (ltt, channel);
		if (channel_name != NIL)
		  mm = endpoint_find_channel_by_name (ltt->my_endpoint, channel_name);
	      } else
		mm = endpoint_find_channel_by_number (ltt->my_endpoint, channel);
	      if (mm == NIL) {
		ltt_lock(ltt);
		DISPLAY_HEADER(&ltt->current_header);
		ILU_NOTE(W3MUX_DEBUG,
			 ("ilu_w3mux(ltt_workproc):  chunk for unknown channel %lu received over \"%s\" and discarded.\n",
			  HEADER_CHANNEL_ID(&ltt->current_header), ltt->description));
		inbuf_free(ltt->current_buffer);
	      } else {
		ilu_cardinal session = HEADER_SESSION_ID(&ltt->current_header);
		/* transfer buffer to mooring */
		ltm_lock(mm->ltm);
		ltt_lock(ltt);
		ltt_add_pending(ltt, ltt->current_buffer, &lerr);
		ILU_MUST_BE_SUCCESS(lerr);
		ltt_unlock(ltt);
		mm_add_pending(mm, ltt->current_buffer, &lerr);
		ILU_MUST_BE_SUCCESS(lerr);
		ltm_unlock(mm->ltm);
		/* notify mooring of new connection request */
		ilu_CondNotify (mm->ltm_locked.wait_CV, &lerr);
	      }
	      exit_globlock();
	      if (mm != NIL)
		ltt_lock(ltt);
	    } else {						/* syn bit not set */
	      /* first we check pending connections, then if there is no session
		 by this number on the pending list, we check the current sessions
		 that are already open. */
	      InBuf ib;
	      ilu_cardinal session = HEADER_SESSION_ID(&ltt->current_header);
	      /* could be as-yet-unaccepted connection */
	      if ((ib = ltt_most_recent_pending (ltt, session)) != NIL) {
		ltt_add_current_to_inbuf (ltt, ib);
	      } else if ((mt = ltt_find_session(ltt, HEADER_SESSION_ID(&ltt->current_header))) != NIL) {
		/* transfer buffer to MUX transport */
		/* Note we are still holding ltt lock */
		if (mt->ltt_locked.bufferlist == NIL)
		  mt->ltt_locked.bufferlist = ltt->current_buffer;
		else
		  ltt_add_current_to_inbuf(ltt, mt->ltt_locked.bufferlist);
		/* notify transport of new input buffer */
		ilu_CondNotify (mt->ltt_locked.wait_CV, &lerr);
		ILU_MUST_BE_SUCCESS(lerr);
	      } else {
		/* hmmm...  Chunk is ostensibly for non-existent session! */
		ILU_NOTE(W3MUX_DEBUG,
			 ("ilu_w3mux(ltt_workproc):  chunk for unknown session %lu received over \"%s\" and discarded.\n",
			  HEADER_SESSION_ID(&ltt->current_header), ltt->description));
		inbuf_free(ltt->current_buffer);
	      }
	    }
	  }
	  ltt->current_buffer = NIL;
	  if (HEADER_CHUNK_SIZE(&ltt->current_header) > 0) {
	  /* this happens if we filled a user-specified buffer, but only read part
	     of the chunk.  We need to read the rest of the chunk into an allocated
	     buffer.  We set up another buffer to read the rest of the chunk into,
	     copying the header from the ltt->current_header.  We are still holding
	     ltt lock... */
	    ILU_NOTE(W3MUX_DEBUG,
		     ("ilu_w3mux(ltt_workproc \"%s\"):  split chunk across two buffers (%lu)\n",
		      ltt->description, (unsigned long) HEADER_CHUNK_SIZE(&ltt->current_header)));
	    ltt_setup_data_chunk_read(ltt);
	  } else {
	    if (ltt->padding > 0) {
	      /* read the padding */
	      _ilu_Assert(ltt->padding < 8, "w3mux:  invalid padding length");
	      ltt->reading_padding = ilu_TRUE;
	      ltt->where = padding_buf;
	      ltt->to_read = ltt->padding;
	      ltt->padding = 0;
	    } else {
	      ltt->reading_header = ilu_TRUE;
	      ltt->reading_long_len = ilu_FALSE;
	      ltt->where = (ilu_bytes) &ltt->current_header.short_header;
	      ltt->to_read = sizeof(ltt->current_header.short_header);
	    }
	  } /* end of partial-chunk-read if */
	} /* end of reading-header if */
      } /* end of read-full-buffer if */

#ifdef ILU_W3MUX_EXCESS_DEBUGGING
	    ILU_NOTE(W3MUX_DEBUG,
		     ("ilu_w3mux(ltt_workproc \"%s\"):  reading %lu bytes (%s:%lu) for session %lu into %p\n",
		      ltt->description, (unsigned long) ltt->to_read,
		      ltt->reading_header ? (ltt->reading_long_len ? "LL" : "H") : "D",
		      (unsigned long) HEADER_CHUNK_SIZE(&ltt->current_header),
		      (unsigned long) HEADER_SESSION_ID(&ltt->current_header), ltt->where));
#endif

    } /* end of analysis of results from transport_read_upto_bytes if chain */
  } /* end of while loop */

  /* At this point, we're finished with this LTt;
     EITHER the mooring is closed, and all sessions are gone,
     OR the other end closed the connection. */

  ILU_NOTE(W3MUX_DEBUG,
	   ("ilu_w3mux(ltt_workproc \"%s\"):  closing\n", ltt->description));

  /* Now wait for all the sessions to die.  We re-use the X CV here, to wait
   for the connections to die.  We set "source" to NIL to signal that. */
  lower = ltt->source;
  ltt->source = NIL;
  ltt_release_x(ltt);
  /* take ourself off the list of connections */
  if (ltt->endpoint != NIL) {
    ENTER_GLOBLOCK();
    endpoint_remove_connection(ltt->endpoint, ltt);
    exit_globlock();
  }
  /* Now no one else can acquire the x mutex for the lower transport stack */
  LTT_UNLOCK(ltt);
  if (ltt->lower_disabled) {
    ilu_AcquireMutex(ilu_cmu);
    ltt_lock(ltt);
    ltt_acquire_x_2(ltt);
    (*lower->tr_class->tc_enableWait)(lower, &lerr);
    if (ILU_ERRNOK(lerr)) {
      ILU_NOTE(W3MUX_DEBUG,
	       ("ilu_w3mux(handleLTt):  Error %s enabling wait on lower transport \"%s\"\n",
		ILU_ERR_NAME(lerr), ltt->description));
      ILU_HANDLED(lerr);
    }
  } else {
    ltt_lock(ltt);
  }
  (void) ilu_CloseTransport(lower, &dfd, &lerr);
  if (ILU_ERRNOK(lerr)) {
    ILU_NOTE(W3MUX_DEBUG,
	     ("ilu_w3mux(handleLTt):  Error %s closing lower transport \"%s\"\n",
	      ILU_ERR_NAME(lerr), ltt->description));
    ILU_HANDLED(lerr);
  }
  if (!ltt->lower_disabled)
    ilu_AcquireMutex(ilu_cmu);
  ilu_DeltaFD(-dfd);
  W3MUX_RELEASE_MUTEX(ilu_cmu);

  ILU_NOTE(W3MUX_DEBUG,
	   ("ilu_w3mux(ltt_workproc \"%s\"):  lower transport now closed\n",
	    ltt->description));

  for (i = 0, n = _ilu_vector_size(ltt->sessions),
       sessions = (Mt *) _ilu_vector_elements(ltt->sessions);  i < n;  i++) {
    ILU_NOTE(W3MUX_DEBUG,
	     ("ilu_w3mux(ltt_workproc \"%s\"):  notifying session %lu\n",
	      ltt->description, sessions[i]->session));
    ilu_CondNotify (sessions[i]->ltt_locked.wait_CV, &lerr);
    ILU_MUST_BE_SUCCESS(lerr);
  }
  while(_ilu_vector_size(ltt->sessions) > 0) {
    ILU_NOTE(W3MUX_DEBUG,
	     ("ilu_w3mux(ltt_workproc \"%s\"):  still %lu session%s open\n",
	      ltt->description, _ilu_vector_size(ltt->sessions),
	      (_ilu_vector_size(ltt->sessions) == 1) ? "" : "s"));
    ilu_CMWait1(ltt->x_CV, ltt->lock, &lerr);
    ILU_MUST_BE_SUCCESS(lerr);
  }

  ILU_NOTE(W3MUX_DEBUG,
	   ("ilu_w3mux(ltt_workproc \"%s\"):  finishing\n", ltt->description));

  ltt_free(ltt);
}

static LTt
  ltt_create (ilu_Transport t,
	      LTm ltm,			/* optional -- NIL if initiator */
	      Endpoint ep,		/* optional -- NIL if receiver */
	      ilu_string peerinfo,	/* optional -- NIL if initiator */
	      ilu_TransportInfo lower_tinfo,
	      ilu_Passport passport,
	      ilu_Error *err)
{
  ilu_Error lerr;
  LTt ltt;
  ilu_string description;

  if (peerinfo == NIL) {
    description = _ilu_StringifyTinfo(lower_tinfo, err);
    if (ILU_ERRNOK(*err)) return NIL;
  } else {
    description = peerinfo;
  }
  ILU_NOTE(W3MUX_DEBUG,
	   ("ilu_w3mux(ltt_create):  tinfo=\"%s\"\n", description));
  ltt = ilu_MallocE(sizeof(*ltt), err);
  if (ILU_ERRNOK(*err)) goto free1;
  ltt->lock = ilu_CreateMutex("w3mux_LTt", description);
  if (ltt->lock == NIL) { ILU_ERR_CONS1(internal, err, minor, ilu_im_brokenLocks, 0); goto free2; };
  ltt->x_CV = ilu_CreateCondition("w3mux_ltt_x_CV", description, err);
  if (ILU_ERRNOK(*err)) goto free3;
  _ilu_Assert(ltt->x_CV != NIL, "NIL ltt->x_CV in ltt_create");
  if (ltt->sessions = _ilu_vector_new(2, err), ILU_ERRNOK(*err)) goto free4;
  ltt->source = t;
  ltt->pending_conn_reqs = NIL;
  ltt->pending_size = 0;
  ltt->pending_used = 0;
  ltt->endpoint = ep;
  ltt->x_held = ilu_FALSE;
  ltt->m_open = ilu_TRUE;
  ltt->reading_header = ilu_TRUE;
  ltt->description = description;
  ltt->max_frag_size = 0;
  ltt->default_credit = MUX_INITIAL_CREDIT;
  ltt->lower_disabled = ilu_FALSE;
  ltt->preFirst = ltt->preNext = 0;
  if (ltm != NIL)
    ltt->my_endpoint = ltm->endpoint;
  else if (passport != NIL) {
    ilu_IdentityInfo i = ilu_FindIdentity (passport, ilu_w3muxEndpointIdentity);
    ILU_NOTE(W3MUX_DEBUG,
	     ("ILU(w3mux.c:ltt_create \"%s\"):  checking passport for endpoint to send to other side...\n",
	      description));
    if (i != NIL) {
      ltt->my_endpoint = endpoint_get (((struct endpoint_identity_info_s *) (i->ii_info))->id,
				       ilu_TRUE, err);
      if (ILU_ERRNOK(*err)) goto free6;
      ILU_NOTE(W3MUX_DEBUG,
	       ("ILU(w3mux.c:ltt_create \"%s\"):  endpoint in passport is <%s>\n",
		description, ltt->my_endpoint->id));
    } else {
      ltt->my_endpoint = NIL;
      ILU_NOTE(W3MUX_DEBUG,
	       ("ILU(w3mux.c:ltt_create \"%s\"):  no endpoint in passport.\n", description));
    }
  } else {
    ltt->my_endpoint = NIL;
    ILU_NOTE(W3MUX_DEBUG,
	     ("ILU(w3mux.c:ltt_create \"%s\"):  no my_endpoint value.\n", description));
  }
  if ((ltt->interned_atoms_remote = ilu_hash_MakeNewTable(ATOMS_HT_BUCKETS,
							  ilu_hash_HashPointer,
							  ilu_hash_PointerCompare)) == NIL) {
    ILU_ERR_CONS1(no_memory, err, nbytes, 0, 0);
    goto free6;
  };
  if ((ltt->interned_atoms_local = ilu_hash_MakeNewTable(ATOMS_HT_BUCKETS,
							 ilu_hash_HashString,
							 ilu_hash_StringCompare)) == NIL) {
    ILU_ERR_CONS1(no_memory, err, nbytes, 0, 0);
    goto free7;
  };
  ltt->interned_atoms_local_id = MUX_ATOM_CHANNEL_MIN;
  ltt->initiator = (ltm == NIL);
  if (ltm == NIL) {
    endpoint_add_connection(ep, ltt, err);
    if (ILU_ERRNOK(*err)) goto free8;
  }
  if (ilu_KernelThreaded()) {
    (void) ilu_Fork (ltt_workproc, (void *) ltt, err);
  } else {
    _ilu_Assert(0, "w3mux only available in threaded spaces");
  }
  if (ILU_ERRNOK(*err)) goto free9;
  return ltt;

 free9:
  if (ltm == NIL)
    endpoint_remove_connection(ep, ltt);
 free8:
  ilu_hash_FreeHashTable(ltt->interned_atoms_local, NULLFN, _free_atom);
 free7:
  ilu_hash_FreeHashTable(ltt->interned_atoms_remote, NULLFN, _free_atom);
 free6:
  _ilu_vector_destroy(ltt->sessions, NULLFN);
 free4:
  lerr = ilu_DestroyCondition(ltt->x_CV);
  ILU_HANDLED(lerr);
 free3:
  ilu_DestroyMutex(ltt->lock, &lerr);
  ILU_HANDLED(lerr);
 free2:
  ilu_free(ltt);
 free1:
  ILU_NOTE(W3MUX_DEBUG,
	   ("ilu_w3mux(ltt_create):  Error \"%s\" creating ltt for \"%s\", at line %d of \"%s\".\n",
	    ILU_ERR_NAME(*err), description, ilu_ErrorLine(err), ilu_ErrorFile(err)));
  ilu_free(description);
  return NIL;
}

/***********************************************************************/
/***********************************************************************/
/**************************  Methods on LTm  ***************************/
/***********************************************************************/
/***********************************************************************/

static ilu_HashTable w3mux_LTms = NIL;
/* tinfo_in -> LTm for that tinfo_in */

static void ltm_lock (LTm ltm)
{
  ilu_AcquireMutex(ltm->lock);
}
     
static void ltm_unlock (LTm ltm)
{
  W3MUX_RELEASE_MUTEX(ltm->lock);
}

static LTm ltm_find (ilu_TransportInfo tinfo)
{
  if (w3mux_LTms == NIL) {
    w3mux_LTms = ilu_hash_MakeNewTable(29, hash_tinfo, compare_tinfo);
    _ilu_Assert(w3mux_LTms != NIL, "Can't create w3mux_LTms");
  };
  return (LTm) ilu_hash_FindInTable(w3mux_LTms, tinfo);
}
  
static void ltm_register (ilu_TransportInfo tinfo, LTm result)
{
  ilu_hash_AddToTable(w3mux_LTms, (void *) tinfo, (void *) result);
}

static void ltm_free (LTm ltm)
{
  ilu_Error lerr;

  ILU_NOTE(W3MUX_DEBUG,
	   ("ilu_w3mux(ltm_free):  \"%s\"\n", ltm->description));
  ilu_DestroyMutex(ltm->lock, &lerr);
  ILU_HANDLED(lerr);
  lerr = ilu_DestroyCondition(ltm->x_CV);
  ILU_HANDLED(lerr);
  lerr = ilu_DestroyCondition(ltm->delay_close_wait);
  ILU_HANDLED(lerr);
  _ilu_Assert(_ilu_vector_size(ltm->channels) == 0, "ltm with existing channels exiting!");
  _ilu_vector_destroy(ltm->channels, NULLFN);	/* already empty */
  _ilu_Assert(_ilu_vector_size(ltm->transports) == 0, "ltm with existing transports exiting!");
  _ilu_vector_destroy(ltm->transports, NULLFN);	/* already empty */
  ilu_free(ltm->description);
  ilu_free(ltm->tinfo_in);
  ilu_free(ltm->tinfo_out);
}

/* inside lock(ltm) */
static void
  ltm_acquire_x (LTm ltm)
{
  ilu_Error lerr;
  for (; ltm->x_held; ) {
    ilu_CMWait1(ltm->x_CV, ltm->lock, &lerr);
    ILU_MUST_BE_SUCCESS(lerr);
  }
  ltm->x_held = ilu_TRUE;
}

/* inside lock(ltm) */
static void
  ltm_release_x (LTm ltm)
{
  ilu_Error lerr;
  ltm->x_held = ilu_FALSE;
  ilu_CondNotify(ltm->x_CV, &lerr);
  ILU_MUST_BE_SUCCESS(lerr);
}

/* inside lock(ltm) */
static void
  ltm_acquire_x_2 (LTm ltm)
{
  ilu_Error lerr;
  for (; ltm->x_held; ) {
    ilu_CMWait2(ltm->x_CV, ltm->lock, ilu_cmu, &lerr);
    ILU_MUST_BE_SUCCESS(lerr);
  }
  ltm->x_held = ilu_TRUE;
}

/* L2 >= {ltm_xmu, ltm_ymu}, L1_sup >= {ltm_lock} */
static ilu_boolean
  ltm_check_dfds (LTm ltm, int *dfd_change)
{
  ilu_Error lerr;
  int dfd;
  ilu_boolean result;

    /* Check to see whether we have the fd budget to accept a new connection */
  ilu_AcquireMutex(ilu_cmu);
  dfd = mooring_dfd(ltm->source, ilu_TRUE);
  if (ilu_fdbudget < ilu_fdstaken + dfd) {
    if (!_ilu_ReduceFdsTo(ilu_fdbudget - dfd, NIL, &lerr)) {
      ILU_NOTE(W3MUX_DEBUG,
	    ("ilu_w3mux(ltm_check_dfds):  err <%s> while attempting to reduce fds from %s to %s\n",
	     ILU_ERR_NAME(lerr), ilu_fdstaken, ilu_fdstaken - dfd));
      ILU_HANDLED(lerr);
      result = ilu_FALSE;
    } else if ((ilu_fdbudget < ilu_fdstaken + dfd) && (dfd > 0)) {
      ILU_NOTE(W3MUX_DEBUG,
	    ("ilu_w3mux(ltm_check_dfds):  FD budget exhausted.\n"));
      /* XXX - Shouldn't we do something here, like accepting the connection,
	 then closing it immediately, to clear the tcp mooring? */
      result = ilu_FALSE;
    } else {
      *dfd_change = dfd;
      result = ilu_TRUE;
    }
  } else {
    *dfd_change = dfd;
    result = ilu_TRUE;
  }
  W3MUX_RELEASE_MUTEX(ilu_cmu);
  return result;
}

static void ltm_close_if_empty (void *rock)
{
  /* XXX -- implement */
}

static void
  ltm_workproc (void *rock)
{
  LTm ltm = (LTm) rock;
  LTt newltt;
  ilu_Transport t;
  int disabled, dfd;
  ilu_string peerinfo;
  ilu_Error lerr;
  ilu_Passport pp;

  /* wait for setup to complete by waiting for release of globlock */
  ENTER_GLOBLOCK(); exit_globlock();

  ltm_lock(ltm);
  while (_ilu_vector_size(ltm->channels) > 0) {
    ltm_unlock(ltm);
    if (!(*ltm->source->mo_wait_for_req)(ltm->source, &disabled, &lerr)) {
      ILU_NOTE(W3MUX_DEBUG,
	       ("ilu_w3mux(handleLTm):  Error \"%s\" "
		"waiting for requests on lower mooring \"%s\"\n",
		ILU_ERR_NAME(lerr), ltm->description));
      ILU_HANDLED(lerr);
    } else {
      ltm_lock(ltm);
      ltm_acquire_x(ltm);
      ltm_unlock(ltm);
      peerinfo = NIL;
      if (!ltm_check_dfds(ltm, &dfd)) {
	ILU_NOTE(W3MUX_DEBUG,
		 ("ilu_w3mux(handleLTm):  file descriptor budget exhausted; "
		  "can't accept conn on lower mooring \"%s\"\n", ltm->description));
	/* XXX -- shouldn't we still accept it and discard it, to get it off the mooring? */
	(void) _ilu_Assert(1, "file descriptor budget exhausted in mux mooring LTm worker thread");
      };
      if ((pp = ilu_CreatePassport(NIL, &lerr)) == NIL) {
	ILU_NOTE(W3MUX_DEBUG,
		 ("ilu_w3mux(handleLTm):  err <%s> while attempting to create passport.\n",
		  ILU_ERR_NAME(lerr)));
	ILU_HANDLED(lerr);
      } else {
	t = (*ltm->source->mo_accept_connection)(ltm->source, &peerinfo, &dfd, pp, &lerr);
	ltm_lock(ltm);
	ltm_release_x(ltm);
	ltm_unlock(ltm);
	ilu_AcquireMutex(ilu_cmu);
	ilu_DeltaFD(dfd);
	W3MUX_RELEASE_MUTEX(ilu_cmu);
	if (ILU_ERRNOK(lerr)) {
	  ILU_NOTE(W3MUX_DEBUG,
		   ("ilu_w3mux(handleLTm):  Error \"%s\" "
		    "accepting a connection on lower mooring \"%s\"\n",
		    ILU_ERR_NAME(lerr), ltm->description));
	  ILU_HANDLED(lerr);
	} else if (t == NIL) {	/* spurious indicator */
	  ilu_DestroyPassport(pp, &lerr);
	  ILU_MUST_BE_SUCCESS(lerr);
	} else {
	  /* set up new connection */
	  newltt = ltt_create(t, ltm, NIL, peerinfo, ltm->tinfo_out, pp, &lerr);
	  if (ILU_ERRNOK(lerr)) {
	    ILU_NOTE(W3MUX_DEBUG,
		     ("ilu_w3mux(handleLTm):  Error \"%s\" "
		   "creating new LTt for connection from \"%s\"\n",
		      ILU_ERR_NAME(lerr), peerinfo));
	    ILU_HANDLED(lerr);
	    ilu_AcquireMutex(ilu_cmu);
	    transport_close (t, &dfd, &lerr);
	    ILU_HANDLED(lerr);
	    if (dfd != 0)
	      ilu_DeltaFD(-dfd);
	    W3MUX_RELEASE_MUTEX(ilu_cmu);
	    ilu_DestroyPassport(pp, &lerr);
	    ILU_MUST_BE_SUCCESS(lerr);
	  };
	}
      }
    }
    ltm_lock(ltm);
  }
  /* wait for ltm->delay_close to be false */
  while (ltm->delay_close) {
    ilu_CMWait1(ltm->delay_close_wait, ltm->lock, &lerr);
    ILU_MUST_BE_SUCCESS(lerr);
  };
  ltm_unlock(ltm);
  ilu_AcquireMutex(ilu_cmu);
  ltm_lock(ltm);
  ltm_acquire_x_2(ltm);
  ltm_unlock(ltm);
  (*ltm->source->mo_enableWait)(ltm->source, &lerr);
  ILU_HANDLED(lerr);
  (*ltm->source->mo_close)(ltm->source, &dfd, &lerr);
  ILU_HANDLED(lerr);
  if (dfd != 0)
    ilu_DeltaFD(-dfd);
  W3MUX_RELEASE_MUTEX(ilu_cmu);
  ltm_free(ltm);
}

/* L1 >= {cmu, globlock} */
static LTm
  ltm_create (ilu_TransportCreator lower,
	      ilu_TransportInfo tinfo_lower,
	      Endpoint endpoint,
	      ilu_boolean buffer, int *dfd,
	      ilu_Passport pp, ilu_Error *err)
{
  ilu_Mooring source;
  ilu_TransportInfo tinfo_out = NIL;
  ilu_TransportInfo tinfo_in;
  ilu_Error lerr;
  ilu_string tinfo_string;
  LTm result;

  tinfo_string = _ilu_StringifyTinfo(tinfo_lower, err);
  if (ILU_ERRNOK(*err)) return NIL;
  ILU_NOTE(W3MUX_DEBUG,
	   ("ilu_w3mux(ltm_create):  tinfo=\"%s\", buffer=%s\n",
	    tinfo_string, buffer ? "True" : "False"));
  if ((result = ltm_find(tinfo_lower)) != NIL) {
    ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_tinfo, NIL);
    goto free0;
  };
  source = ((*lower->tcr_createMooring)
	    (lower, &tinfo_out, buffer, dfd, pp, err));
  if (ILU_ERRNOK(*err)) goto free0;
  /* make a copy of the tinfo to use in the hash table */
  tinfo_in = _ilu_CopyTinfo (tinfo_lower, err);
  if (ILU_ERRNOK(*err)) goto free1;
  result = ilu_MallocE(sizeof(*result), err);
  if (ILU_ERRNOK(*err)) goto free2;
  result->description = tinfo_string;
  result->lock = ilu_CreateMutex("w3mux_ltm", result->description);
  if (result->channels = _ilu_vector_new(2, err), ILU_ERRNOK(*err)) goto free3;
  if (result->transports = _ilu_vector_new(2, err), ILU_ERRNOK(*err)) goto free4;
  result->source = source;
  result->tinfo_in = tinfo_in;
  result->tinfo_out = tinfo_out;
  result->x_held = ilu_FALSE;
  result->endpoint = endpoint;
  result->x_CV = ilu_CreateCondition("w3mux_ltm_x_CV", result->description, err);
  if (ILU_ERRNOK(*err)) goto free5;
  result->delay_close_wait = ilu_CreateCondition("w3mux_ltm_delay_close_wait", result->description, err);
  if (ILU_ERRNOK(*err)) goto free6;
  result->delay_close = ilu_FALSE;
  if (ilu_KernelThreaded()) {
    (void) ilu_Fork (ltm_workproc, (void *) result, err);
  } else {
    _ilu_Assert(0, "w3mux only available in threaded spaces");
  }
  if (ILU_ERRNOK(*err)) goto free7;
  ltm_register(tinfo_in, result);
  return result;
  
 free7:
  lerr = ilu_DestroyCondition(result->delay_close_wait);
  ILU_HANDLED(lerr);
 free6:
  lerr = ilu_DestroyCondition(result->x_CV);
  ILU_HANDLED(lerr);
 free5:
  _ilu_vector_destroy(result->transports, NULLFN);
 free4:
  _ilu_vector_destroy(result->channels, NULLFN);
 free3:
  ilu_DestroyMutex(result->lock, &lerr);
  ILU_HANDLED(lerr);
  ilu_free(result);
 free2:
  ilu_free(tinfo_in);
 free1:
  ilu_free(tinfo_out);
  (*source->mo_close)(source, dfd, &lerr);
  ILU_HANDLED(lerr);
 free0:
  ILU_NOTE(W3MUX_DEBUG,
	   ("ilu_w3mux(ltm_create):  Error \"%s\" creating ltm for \"%s\", at line %d\n",
	    ILU_ERR_NAME(*err), tinfo_string, ilu_ErrorLine(err)));
  ilu_free(tinfo_string);
  return NIL;
}
  
/***********************************************************************/
/***********************************************************************/
/***************  Methods on MUX transport object  *********************/
/***********************************************************************/
/***********************************************************************/

/* L1.sup < cmu */
/* while holding mt->ltt->lock... */
static void mt_free (Mt mt)
{
  ilu_free(mt->transport->tr_outBuff - 8);
  ilu_DestroyCondition(mt->ltt_locked.wait_CV);
  if (mt->ltt_locked.bufferlist != NIL)
    inbuf_free(mt->ltt_locked.bufferlist);
  mt->ltt_locked.bufferlist = NIL;
  ilu_free(mt->transport);
  ilu_free(mt);
}

/* while holding mt's x mutex... */
static void
  mt_send_open (Mt mt, ilu_Error *err)
{
  Header_s        header;

  if (!mt->unused) {
    ILU_CLER(*err);
    return;
  };

  ltt_lock(mt->ltt);
  if (ltt_acquire_x(mt->ltt)) {
    if (ClearPre(mt->ltt, ilu_TRUE, err)) {
      ILU_NOTE(W3MUX_DEBUG,
	       ("ilu_w3mux(mt_send_open):  sending open message for %s(%u=>%u)...\n",
		SESSION_NAME(mt)));
      header.short_header = ((1 << MUX_SYN_SHIFT) |
			     (1 << MUX_PUSH_SHIFT) |
			     (mt->session << MUX_SESSION_SHIFT));
      SWAP_IF_NEEDED(header.short_header);
      transport_write_bytes(mt->ltt->source, (ilu_bytes) & header,
			    sizeof(header), err);
      mt->unused = ilu_FALSE;
    }
    ltt_release_x(mt->ltt);
  };
  LTT_UNLOCK(mt->ltt);
}

/* holding mt->ltt->lock... */
static void
  mt_send_credit (Mt mt, ilu_boolean flush, ilu_Error *err)
{
  Header_s        msg;
  ilu_cardinal    credit_amount;

  if (mt->unused) {
    mt_send_open(mt, err);
    if (ILU_ERRNOK(*err))
      return;
  };

  if (ltt_acquire_x(mt->ltt)) {
    if (ClearPre(mt->ltt, ilu_TRUE, err)) {
      ilu_boolean     flushed;
      credit_amount = mt->ltt_locked.local_credit;
      if (credit_amount > MUX_MAX_CREDIT) {
	ILU_ERR_CONS1(internal, err, minor, ilu_im_mux_max_credit, 0);
	goto dun;
      };
      _ilu_Assert(credit_amount <= MUX_MAX_CREDIT,
		  "mux max credit exceeded (mt_send_credit)");
      mt->ltt_locked.local_credit = 0;

      ILU_NOTE(W3MUX_DEBUG,
	       ("ilu_w3mux(mt_send_credit):  %s(%u=>%u) sending credit of %u bytes...\n",
		SESSION_NAME(mt), credit_amount));

      msg.short_header = ((1 << MUX_CONTROL_SHIFT) |
			(MUX_AddCredit << MUX_CONTROL_CODE_SHIFT) |
			  (mt->session << MUX_SESSION_SHIFT) |
		       (credit_amount << MUX_FRAGMENT_SIZE_SHIFT));
      /* flip if necessary */
      SWAP_IF_NEEDED(msg.short_header);
      transport_write_bytes_full(mt->ltt->source,
				 (ilu_bytes) & msg.short_header,
				 sizeof(msg.short_header),
				 flush, &flushed, ilu_TRUE, err);
    }
dun:
    ltt_release_x(mt->ltt);
  }
}

/* holding ltt->lock... */
static ilu_boolean
  mt_send_close (Mt mt, ilu_Error *err)
{
  Header_s        header;
  ilu_boolean     stat = ilu_TRUE;

  ILU_CLER(*err);

  if (mt->unused)
    return ilu_FALSE;

  stat = ltt_acquire_x(mt->ltt);
  if (stat) {
    LTT_UNLOCK(mt->ltt);

    if (ClearPre(mt->ltt, ilu_TRUE, err)) {
      ILU_NOTE(W3MUX_DEBUG,
	       ("ilu_w3mux(mt_send_close):  sending close message for %s(%u=>%u)...\n",
		SESSION_NAME(mt)));

      header.short_header = ((1 << MUX_FIN_SHIFT) |
			     (1 << MUX_PUSH_SHIFT) |
			     (mt->session << MUX_SESSION_SHIFT));
      SWAP_IF_NEEDED(header.short_header);
      transport_write_bytes(mt->ltt->source, (ilu_bytes) & header,
			    sizeof(header.short_header), err);
    } else
      stat = ilu_FALSE;
    ltt_lock(mt->ltt);
    ltt_release_x(mt->ltt);
  };
  return stat;
}

#define BUFFERSIZE	1024

/* L1.sup < trmu; L2 >= {xmu, ymu} */
static          ilu_boolean
w3mux_Mt_SetInputHandler(ilu_Transport self,
			 ilu_TIH tih,
			 ILU_ERRS((no_memory, internal, no_resources)) * err)
{
  _ilu_Assert(0, "Attempt to set w3mux transport input handler; w3mux only works in threaded runtimes\n");
  return ilu_FALSE;
}

/* Main Invariant holds; L2 >= {ymu} */
static          ilu_boolean
  w3mux_Mt_WaitForInput(ilu_Transport self, int *disabled,
			ilu_FineTime * limit,
			ILU_ERRS((broken_locks, interrupted)) * err)
{
  Mt mt = (Mt) self->tr_data;
  LTt ltt = ((Mt) self->tr_data)->ltt;

  if (mt->closed)
    return ILU_CLER(*err);

  ltt_lock(ltt);
  while (mt->ltt_locked.wait_disables == 0) {
    if (mt->ltt_locked.bufferlist != NIL) {
      ILU_NOTE(W3MUX_DEBUG,
	       ("ILU(w3mux_Mt_WaitForInput):  %s(%lu=>%lu) %p, input in bufferlist\n",
		SESSION_NAME(mt), mt));
#ifdef ILU_W3MUX_EXCESS_DEBUGGING
      if (ilu_DebugLevel & W3MUX_DEBUG) {
	InBuf p;
	for (p = mt->ltt_locked.bufferlist;  p != NIL;  p = p->next) {
	  ilu_DebugPrintf("  %8.8x %8.8x, %lu bytes\n",
			  p->header.short_header, p->header.long_length, HEADER_CHUNK_SIZE(&p->header));
	}
      }
#endif
      *disabled = ilu_FALSE;
      LTT_UNLOCK(ltt);
      return ILU_CLER(*err);
    } else if (ltt->source == NIL) {	/* EOF */
      ILU_NOTE(W3MUX_DEBUG,
	       ("ILU(w3mux_Mt_WaitForInput):  %s(%lu=>%lu) %p, EOF on ltt\n",
		SESSION_NAME(mt), mt));
      *disabled = ilu_FALSE;
      LTT_UNLOCK(ltt);
      return ILU_CLER(*err);
    }
    ILU_NOTE(W3MUX_DEBUG,
	     ("ILU(w3mux_Mt_WaitForInput):  %s(%lu=>%lu) %p -- waiting on wait_CV\n",
	      SESSION_NAME(mt), mt));
    ilu_CMWait1(mt->ltt_locked.wait_CV, ltt->lock, err);
    if (ILU_ERRNOK(*err)) return ilu_FALSE;
  }
  LTT_UNLOCK(ltt);
  *disabled = ilu_TRUE;
  return ILU_CLER(*err);
}

/* L1.sup < trmu; L2 >= {xmu} */
static          ilu_boolean
w3mux_Mt_InterruptST(ilu_Transport self, ILU_ERRS((bad_param)) * err)
{
  _ilu_Assert(0, "Attempt to interruptST a w3mux transport; w3mux only works in threaded runtimes\n");
  return ilu_FALSE;
}

/* L1 >= {cmu}; L1.sup < trmu; L2 >= {xmu} */

static          ilu_boolean
w3mux_Mt_DisableWait(ilu_Transport self,
		     ILU_ERRS((broken_locks, bad_param,
			       internal)) * err)
{
  Mt              mt = (Mt) self->tr_data;
  ltt_lock(mt->ltt);
  mt->ltt_locked.wait_disables += 1;
  ilu_CondNotify(mt->ltt_locked.wait_CV, err);
  LTT_UNLOCK(mt->ltt);
  if (ILU_ERRNOK(*err))
    return ilu_FALSE;
  if (!_ilu_DeltaCohortWaits(self->tr_wc, 1, err))
    return ilu_FALSE;
  return ilu_TRUE;
}

static          ilu_boolean
w3mux_Mt_EnableWait(ilu_Transport self,
		    ILU_ERRS((broken_locks, bad_param,
			      internal)) * err)
{
  Mt              mt = (Mt) self->tr_data;
  ltt_lock(mt->ltt);
  mt->ltt_locked.wait_disables -= 1;
  ilu_CondNotify(mt->ltt_locked.wait_CV, err);
  LTT_UNLOCK(mt->ltt);
  if (ILU_ERRNOK(*err))
    return ilu_FALSE;
  if (!_ilu_DeltaCohortWaits(self->tr_wc, -1, err))
    return ilu_FALSE;
  return ilu_TRUE;
}

/*Main Invariant holds; L2 >= {xmu}*/

static          ilu_boolean
w3mux_Mt_SendWholeMessage(ilu_Transport self, ilu_Message * msgh,
			   ILU_ERRS((IoErrs)) * err)
{
  return ILU_ERR_CONS1(internal, err, minor, ilu_im_tcReliable, ilu_FALSE);
}

static void
  mt_notice_input_flags (Mt mt, InBuf buf)
{
  if (HEADER_LAST_CHUNK(&buf->header))
    mt->eom_received = ilu_TRUE;
  if (HEADER_RESET(&buf->header) || HEADER_LAST_MSG(&buf->header)) {
    ILU_NOTE(W3MUX_DEBUG,
	     ("ILU(mt_notice_input_flags) session \"%s\"(%lu=>%lu):  close header %8.8x seen\n",
	      SESSION_NAME(mt), buf->header.short_header));
    mt->eom_received = ilu_TRUE;
    mt->closed = ilu_TRUE;
  };
}

/* holding mt->ltt->lock */
static ilu_boolean
  mt_push (Mt mt, ilu_Error *err)
{
  ilu_Transport   lower;

  lower = mt->ltt->source;
  if (mt->closed || (lower == NIL))
    ILU_CLER(*err);
#if 0
    ILU_ERR_CONS1(comm_failure, err, minor, ilu_cfm_conn_lost, ilu_FALSE);
#endif
  else if (ltt_acquire_x(mt->ltt)) {
    LTT_UNLOCK(mt->ltt);
    (void) (*lower->tr_class->tc_push) (lower, err);
    ltt_lock(mt->ltt);
    ltt_release_x(mt->ltt);
  } else
    ILU_CLER(*err);
  return ILU_ERROK(*err);
}

/*Main Invariant holds; L2 >= {xmu}*/

static          ilu_boolean
w3mux_Mt_Push(ilu_Transport self, ILU_ERRS((IoErrs)) * err)
{
  Mt              mt = (Mt) self->tr_data;

  ltt_lock(mt->ltt);
  mt_push(mt, err);
  LTT_UNLOCK(mt->ltt);
  return ILU_ERROK(*err);
}

/* L1_sup >= {mt->ltt->lock}, L2 >= {mt.xmu, mt.ymu} */
static ilu_cardinal
  mt_do_read (Mt mt, ilu_bytes buf, ilu_cardinal len, ilu_TransportReport *rpt, ilu_Error *err)
{
  ilu_cardinal size = len, to_read, bytes_read = 0;
  ilu_boolean flush_credit = ilu_TRUE;
  InBuf bufptr;

  if (mt->ltt_locked.input_state.current != NIL) {
    /* finished previous input buffer, so free it... */
    mt_notice_input_flags(mt, mt->ltt_locked.input_state.current);
    inbuf_free(mt->ltt_locked.input_state.current);
    mt->ltt_locked.input_state.current = NIL;
  };
  mt->transport->tr_inBuff = NIL;
  if (mt->eom_received || mt->closed) {
    rpt->tr_eom = mt->eom_received;
    rpt->tr_eof = mt->closed;
    ILU_CLER(*err);
    return 0;
  }
  if (buf != NIL) {
    to_read = len;
    /* read into specified buffer */
    while ((!mt->eom_received) &&
	   (!mt->closed) &&
	   (to_read > 0) &&
	   ((bufptr = mt->ltt_locked.bufferlist) != NIL)) {
      /* we have data; use it */
      mt->ltt_locked.bufferlist = bufptr->next;
      bufptr->next = NIL;
      size = MIN(bufptr->size, to_read);
      memcpy((void *) buf, (void *) bufptr->bytes, size);
      to_read -= size;
      bytes_read += size;
      bufptr->used += size;
      mt->ltt_locked.local_credit += bufptr->size;
      if (size == bufptr->size) {
	mt_notice_input_flags(mt, bufptr);
	inbuf_free(bufptr);
      } else {	/* only used part of the buffer */
	mt->ltt_locked.input_state.current = bufptr;
	mt->transport->tr_inBuff = bufptr->bytes;
	mt->transport->tr_inNext = bufptr->used;
	mt->transport->tr_inLimit = bufptr->size;
      }
    }
    flush_credit = (to_read > 0);
  } else {	/* buf is NIL, put something in external buffer */
    if ((bufptr = mt->ltt_locked.bufferlist) != NIL) {
      mt->ltt_locked.bufferlist = bufptr->next;
      bufptr->next = NIL;
      mt->ltt_locked.input_state.current = bufptr;
      mt->transport->tr_inBuff = bufptr->bytes;
      mt->transport->tr_inNext = 0;
      mt->transport->tr_inLimit = bufptr->size;
      mt->ltt_locked.local_credit += bufptr->size;
      bytes_read += bufptr->size;
    } else {
      mt->transport->tr_inBuff = NIL;
    }
  }
  if (mt->ltt_locked.local_credit > (MUX_INITIAL_CREDIT/2)) {
    /* send AddCredit to peer */
    mt_send_credit(mt, ilu_TRUE, err);
  } else if (flush_credit) {
    mt_push (mt, err);
  }
  return bytes_read;
}

/* mayBlock ? Main Invariant : L1.sup < trmu */
/* L2 >= {xmu}; input => L2 >= {ymu} */

static          ilu_ReadHeaderResultCode
w3mux_Mt_BeginMessage (ilu_Transport self,
		       ilu_boolean input_p,
		       ILU_ERRS((IoErrs)) * err)
{
  ilu_ReadHeaderResultCode status;
  Mt mt = (Mt) self->tr_data;
  ilu_boolean eos;
  ilu_cardinal nbytes;
  InBuf buf;

  ILU_CLER(*err);
  ILU_NOTE(W3MUX_DEBUG,
	   ("ILU(w3mux_Mt_BeginMessage) dir=%s (curdir=%s) \"%s\"(%lu=>%lu)\n",
	    input_p ? "Input" : "Output",
	    ((mt->msgdir == InputDir) ? "Input" :
	     ((mt->msgdir == OutputDir) ? "Output" :
	      ((mt->msgdir == None) ? "None" : "Unknown"))),
	    SESSION_NAME(mt)));
  if (mt->msgdir != None) {
    return (ILU_ERR_CONS1(internal, err, minor, ilu_im_beginMessage, ilu_rhrc_error));
  } else if (input_p && mt->closed) {
    return ilu_rhrc_eof;
  } else if (input_p) {
#if 0
    if (ilu_DebugLevel & W3MUX_DEBUG) {
      InBuf p;
      for (p = mt->ltt_locked.bufferlist;  p != NIL;  p = p->next) {
	ilu_DebugPrintf("  %8.8x %8.8x, %lu bytes\n",
			p->header.short_header, p->header.long_length, HEADER_CHUNK_SIZE(&p->header));
      }
    }
#endif
    mt->eom_received = ilu_FALSE;
    mt->transport->tr_inBuff = NIL;
    ltt_lock(mt->ltt);
    mt->ltt_locked.input_state.current = NIL;
    for (nbytes = 0, eos = ilu_FALSE, buf = mt->ltt_locked.bufferlist;
	 buf != NIL;  buf = buf->next) {
      if (HEADER_CHUNK_SIZE(&buf->header) > 0)
	nbytes += HEADER_CHUNK_SIZE(&buf->header);
      if (HEADER_LAST_MSG(&buf->header) || HEADER_RESET(&buf->header))
	eos = ilu_TRUE;
    }
    if (nbytes == 0) {
      if (eos || (mt->ltt->source == NIL)) {
	mt->eof_received = ilu_TRUE;
	status = ilu_rhrc_eof;
      } else
	status = ilu_rhrc_nothing;
    } else {
      status = ilu_rhrc_ok;
    }
    LTT_UNLOCK(mt->ltt);
    if (status == ilu_rhrc_ok)
      mt->msgdir = InputDir;
  } else {
    if (mt->eof_received)
      return ILU_ERR_CONS1(comm_failure, err, minor, ilu_cfm_conn_lost, ilu_rhrc_error);
    mt->msgdir = OutputDir;
    status = ilu_rhrc_ok;
  }
  return status;
}

static          ilu_boolean
w3mux_Mt_BeginOutputMessageNonblock(ilu_Transport self,
				    ILU_ERRS((IoErrs)) * err)
{
  return (w3mux_Mt_BeginMessage(self, ilu_FALSE, err) == ilu_rhrc_ok);
}

#define Queue(ltt,buf,len)				\
(memcpy((void *) (ltt->preBytes + ltt->preFirst),	\
	(void *) (buf),					\
	(len)),						\
 ltt->preNext += (len))

#define writeAndQueue(ltt,buf,len,flush,flushed,block,err)	\
do {								\
  ilu_cardinal    wrote;					\
  wrote = (transport_write_bytes_full				\
	   (ltt->source, buf, len, flush, flushed, block, err)); \
  if (wrote < (len))						\
    Queue(ltt, ((buf) + wrote), len - wrote);			\
} while (0)

#define writeOrQueue(ltt,buf,len,flush,flushed,block,err)	\
do {								\
  if (ltt->preNext) {						\
    Queue(ltt, buf, len);					\
    if (flush)							\
      *(flushed) = ilu_FALSE;					\
  } else							\
    writeAndQueue(ltt, buf, len, flush, flushed, block, err);	\
} while (0)

/* L2 >= {mt's xmu}, L1 >= {mt->ltt} */
static ilu_cardinal
  mt_do_write (Mt mt, ilu_bytes buf, ilu_cardinal buflen,
	       ilu_cardinal headerloc[2], ilu_boolean last,
	       ilu_boolean flush, ilu_boolean * flushed,
	       ilu_boolean mayBlock, ilu_Error *err)
{
  ilu_Transport   lower = mt->ltt->source;
  ilu_bytes       bufptr;
  ilu_cardinal    size, size2, bytes_taken = 0, header_offset;
  ilu_cardinal    alignment, padding_len;
  ilu_boolean     push, dullFlushed;
  static ilu_byte padding[8] = {0, 0, 0, 0, 0, 0, 0, 0};

  if (flush)
    *flushed = ilu_FALSE;
  else
    flushed = &dullFlushed;
  if (!ltt_acquire_x(mt->ltt))
    return 0;
  if (!ClearPre(mt->ltt, mayBlock, err))
    goto dun;
  bufptr = buf;
  size = buflen;
  if ((size == 0) && last) {
    ilu_cardinal    scratch[1];
    ILU_NOTE(W3MUX_DEBUG,
	 ("ILU_w3mux(mt_do_write): writing empty final chunk.\n"));
    LTT_UNLOCK(mt->ltt);
    FORMAT_HEADER(scratch, 0, mt->unused, ilu_FALSE, ilu_FALSE,
		  ilu_TRUE, mt->channel, mt->session);
    writeAndQueue(mt->ltt, (ilu_bytes) scratch, 4, flush, flushed,
		  mayBlock, err);
    ltt_lock(mt->ltt);
    ltt_release_x(mt->ltt);
    return 0;
  }
  while (size > 0) {
    /* mt->ltt's x mutex held, and mt->ltt->preNext==0 */
    if ((!mt->ltt_locked.output_state.credit_limited) ||
	(mt->ltt_locked.output_state.credit > 0)) {
      ilu_boolean     flushhere;
      if (mt->ltt_locked.output_state.credit_limited) {
	size2 = MIN(size, mt->ltt_locked.output_state.credit);
	_ilu_Assert(size2 <= MUX_MAX_CREDIT,
		    "mux max credit exceeded (mt_do_write)");
      } else
	size2 = size;
      if ((mt->ltt->max_frag_size > 0) &&
	  (size2 > mt->ltt->max_frag_size))
	size2 = mt->ltt->max_frag_size;
      if ((!mayBlock) && size2 > PREBUFLEN - 8)
	size2 = ((PREBUFLEN - 8) / 8) * 8;
      flushhere = ((flush && size2 == size)
		   || (mt->ltt_locked.output_state.credit_limited
		     && mt->ltt_locked.output_state.credit == size2
		       && size2 < size));
      push = last && (size2 == size);
      LTT_UNLOCK(mt->ltt);
      alignment = (mt->unused || (size2 > SHORT_HEADER_SIZE)) ? 8 : 4;
      header_offset = alignment;
      padding_len = FIGURE_PADDING_2(size2, alignment);
      if (!ilu_Check(mayBlock ||
		     (header_offset + size2 + padding_len
		      <= PREBUFLEN),
		     err))
	goto dun;
      ILU_NOTE(W3MUX_DEBUG,
	       ("ILU_w3mux(mt_do_write): writing chunk, size=%lu,"
		" push=%d, ch=%d, sess=%d.\n",
		(long unsigned) size2, push,
		mt->channel, mt->session));
      if (headerloc != NIL) {
	FORMAT_HEADER(headerloc, size2, mt->unused, ilu_FALSE,
		      ilu_FALSE, push, mt->channel, mt->session);
	writeAndQueue(mt->ltt, (ilu_bytes) headerloc,
		      header_offset, ilu_FALSE, (ilu_boolean *) 0,
		      mayBlock, err);
	ILU_MUST_BE_SUCCESS(*err);
	writeOrQueue(mt->ltt, bufptr, size2, flushhere && !padding_len,
		     flushed, mayBlock, err);
	ILU_MUST_BE_SUCCESS(*err);
      } else {
	FORMAT_HEADER(((ilu_cardinal *) (bufptr - header_offset)),
		      size2, mt->unused, ilu_FALSE, ilu_FALSE,
		      push, mt->channel, mt->session);
	writeAndQueue(mt->ltt, bufptr - header_offset,
		      size2 + header_offset,
		    flushhere && !padding_len, flushed, mayBlock, err);
	ILU_MUST_BE_SUCCESS(*err);
      }
      bytes_taken += size2;
      if (padding_len) {
	ILU_NOTE(W3MUX_DEBUG,
	("ILU_w3mux(mt_do_write):  writing padding of %lu bytes\n",
	 padding_len));
	writeOrQueue(mt->ltt, padding, padding_len, flushhere,
		     flushed, mayBlock, err);
	ILU_MUST_BE_SUCCESS(*err);
      }
      ltt_lock(mt->ltt);
      size -= size2;
      bufptr += size2;
      if (mt->ltt->preNext || ILU_ERRNOK(*err))
	goto dun;
      if (mt->ltt_locked.output_state.credit_limited)
	mt->ltt_locked.output_state.credit -= size2;
    } else {
      if (!mayBlock)
	goto dun;
      ltt_release_x(mt->ltt);
      ilu_CMWait1(mt->ltt_locked.wait_CV, mt->ltt->lock, err);
      if (ILU_ERRNOK(*err))
	return bytes_taken;
      if (!ltt_acquire_x(mt->ltt))
	return bytes_taken;
    }
  }
dun:
  ltt_release_x(mt->ltt);
  return bytes_taken;
}

static          ilu_TransportEndReport
w3mux_Mt_EndMessage_Full(ilu_Transport self,
			 ilu_boolean flush,
			 ilu_Message * msgh,
			 ilu_boolean mayBlock,
			 ILU_ERRS((IoErrs)) * err)
{
  Mt              mt = (Mt) self->tr_data;
  InBuf           ptr;
  ilu_boolean     bytes_dropped = ilu_FALSE;
  ilu_TransportEndReport ans = {ilu_FALSE, ilu_FALSE};

  ILU_CLER(*err);
  if (mt->closed) {
    mt->msgdir = None;
    return ILU_ERR_CONS1(comm_failure, err, minor, ilu_cfm_conn_lost,
			 ans);
  };
  if (mt->msgdir == None)
    return ILU_ERR_CONS1(internal, err, minor, ilu_im_endMessage, ans);
  ILU_NOTE(W3MUX_DEBUG,
	   ("ILU(w3mux_Mt_EndMessage) dir=%s flush=%s mayBlock=%s \"%s\"(%lu=>%lu)\n",
	    ((mt->msgdir == InputDir) ? "Input" :
	     ((mt->msgdir == OutputDir) ? "Output" :
	      ((mt->msgdir == None) ? "None" : "Unknown"))),
	    flush ? "ilu_TRUE" : "ilu_FALSE", mayBlock ? "ilu_TRUE" : "ilu_FALSE",
	    SESSION_NAME(mt)));
  ltt_lock(mt->ltt);
  if (mt->msgdir == InputDir) {
    if (mt->ltt_locked.input_state.current != NIL) {
      /* finished previous input buffer, so free it... */
      mt_notice_input_flags(mt, mt->ltt_locked.input_state.current);
      inbuf_free(mt->ltt_locked.input_state.current);
      mt->ltt_locked.input_state.current = NIL;
      if ((self->tr_inLimit - self->tr_inNext) > 0)
	bytes_dropped = ilu_TRUE;
    };
    mt->transport->tr_inBuff = NIL;
    if (!mt->eom_received) {
      /* read in rest of msg */
      ilu_cardinal    bytes = 0;
      while ((mt->ltt->source != NIL) && !mt->eom_received
	     && !mt->eof_received) {
	if (mt->ltt_locked.bufferlist == NIL) {
	  ilu_CMWait1(mt->ltt_locked.wait_CV, mt->ltt->lock, err);
	  if (ILU_ERRNOK(*err))
	    return ans;
	};
	while ((!mt->eom_received) &&
	       ((ptr = mt->ltt_locked.bufferlist) != NIL)) {
	  mt->ltt_locked.bufferlist = ptr->next;
	  bytes += HEADER_CHUNK_SIZE(&ptr->header);
	  if (HEADER_LAST_MSG(&ptr->header) ||
	      HEADER_RESET(&ptr->header)) {
	    ILU_NOTE(W3MUX_DEBUG,
		     ("ILU(w3mux_Mt_EndMessage \"%s\"(%lu=>%lu)):  close header (%8.8x) seen\n",
		      SESSION_NAME(mt), ptr->header.short_header));
	    mt->eom_received = ilu_TRUE;
	    mt->closed = ilu_TRUE;
	    mt->eof_received = ilu_TRUE;
	  } else if (HEADER_LAST_CHUNK(&ptr->header)) {
	    mt->eom_received = ilu_TRUE;
	  }
	  inbuf_free(ptr);
	}
      }
      bytes_dropped = (bytes > 0);
    };
    mt->msgdir = None;
    if (bytes_dropped)
      ILU_ERR_CONS1(internal, err, minor, ilu_im_tcBytesDropped, 0);
    else
      ILU_CLER(*err);
  } else {
    ilu_cardinal    took;
    _ilu_Assert(mt->ltt_locked.input_state.current == NIL,
	   "Bad mt->ltt_locked.input_state.current in EndMessage");
    took = mt_do_write(mt, self->tr_outBuff, self->tr_outNext, NIL,
		       ilu_TRUE, flush, &ans.iluter_flushed,
		       mayBlock, err);
    if (took < self->tr_outNext) {
      self->tr_outNext -= took;
      memmove((void *) self->tr_outBuff,
	      (void *) (self->tr_outBuff + took),
	      self->tr_outNext);
      ILU_NOTE(W3MUX_DEBUG,
	       ("ILU(w3mux_Mt_EndMessage) output, nonblocking \"%s\"(%lu=>%lu) fails due to full pipe.\n",
		SESSION_NAME(mt)));
    } else {
      self->tr_outNext = 0;
      mt->msgdir = None;
    }
  }
  LTT_UNLOCK(mt->ltt);
  ans.iluter_ended = ILU_ERROK(*err);
  return ans;
}

static          ilu_boolean
w3mux_Mt_EndMessage(ilu_Transport self,
		    ilu_boolean flush,
		    ilu_Message * msgh,
		    ILU_ERRS((IoErrs)) * err)
{
  return w3mux_Mt_EndMessage_Full(self, flush, msgh, ilu_TRUE,
				  err).iluter_ended;
}

/*L1.sup < trmu; L2 >= {xmu}*/
static          ilu_TransportEndReport
w3mux_Mt_EndOutputMessageNonblock(ilu_Transport self,
				  ilu_boolean flush,
				  ilu_Message * msgh,
				  ILU_ERRS((IoErrs)) * err)
{
  return w3mux_Mt_EndMessage_Full(self, flush, msgh, ilu_FALSE,
				  err);
}

static          ilu_cardinal
w3mux_Mt_WriteBytes_Full(ilu_Transport self,
			 ilu_bytes buf,
			 ilu_cardinal buflen,
			 ilu_boolean flush,
			 ilu_boolean * flushed,
			 ilu_boolean mayBlock,
			 ILU_ERRS((IoErrs)) * err)
{
  Mt              mt = (Mt) self->tr_data;
  ilu_Transport   t = mt->transport;
  ilu_cardinal    bytes_written = 0;

  ILU_CLER(*err);
  if (flush)
    *flushed = ilu_FALSE;

  if (mt->closed)
    return ILU_ERR_CONS1(comm_failure, err, minor, ilu_cfm_conn_lost,
			 0);
  if (mt->msgdir != OutputDir)
    return ILU_ERR_CONS1(internal, err, minor, ilu_im_bytesWithoutMsg,
			 0);

  ltt_lock(mt->ltt);

  _ilu_Assert(buf != t->tr_outBuff,
	      "w3mux: writebytes:  internal buffer passed as parm");

  /* anything in internal buffer to write? */
  if (t->tr_outNext > 0) {
    ilu_cardinal    took;
    took = mt_do_write(mt, t->tr_outBuff, t->tr_outNext, NIL,
		       ilu_FALSE, ilu_FALSE, flushed,
		       mayBlock, err);
    if (took < t->tr_outNext) {
      self->tr_outNext -= took;
      memmove((void *) self->tr_outBuff,
	      (void *) (self->tr_outBuff + took),
	      self->tr_outNext);
    } else {
      t->tr_outNext = 0;
    }
    if (ILU_ERRNOK(*err))
      goto retpoint;
  }
  /* internal buffer now empty */
  if (buf != NIL) {		/* also need to write this */
    ilu_cardinal    scratch[2], took;
    took = mt_do_write(mt, buf, buflen, scratch, ilu_FALSE,
		       ilu_FALSE, flushed,
		       mayBlock, err);
    bytes_written = took;
    if (ILU_ERRNOK(*err))
      goto retpoint;
  };

  (void) ilu_Check((t->tr_outLimit - t->tr_outNext) > 16, err);

retpoint:
  LTT_UNLOCK(mt->ltt);
  return bytes_written;
}

static          ilu_boolean
w3mux_Mt_WriteBytes(ilu_Transport self,
		    ilu_bytes buf,
		    ilu_cardinal buflen,
		    ilu_boolean flush,
		    ILU_ERRS((IoErrs)) * err)
{
  (void) w3mux_Mt_WriteBytes_Full(self, buf, buflen, flush, NIL,
				  ilu_TRUE, err);
  return ILU_ERROK(*err);
}

/*L1.sup < trmu; L2 >= {xmu}*/
static          ilu_cardinal
w3mux_Mt_WriteBytesNonblock(ilu_Transport self,
			    ilu_bytes buf,
			    ilu_cardinal buflen,
			    ilu_boolean flush,
			    ilu_boolean * flushed,
			    ILU_ERRS((IoErrs)) * err)
{
  return w3mux_Mt_WriteBytes_Full(self, buf, buflen, flush, flushed,
				  ilu_FALSE, err);
}

/*Main Invariant holds; L2 >= {xmu, ymu}*/

static          ilu_cardinal
w3mux_Mt_ReadBytes(ilu_Transport self,
		    ilu_bytes buf,
		    ilu_cardinal len,
		    ilu_TransportReport * rpt,
		    ILU_ERRS((IoErrs)) * err)
{
  Mt mt = (Mt) self->tr_data;
  ilu_cardinal bytes_read;

  ILU_CLER(*err);
  if (mt->closed) {
    rpt->tr_eof = ilu_TRUE;
    rpt->tr_eom = mt->eom_received;
    return 0;
  }
  if (mt->msgdir != InputDir)
    return ILU_ERR_CONS1(internal, err, minor, ilu_im_bytesWithoutMsg, 0);
  ltt_lock(mt->ltt);
  if ((mt->ltt_locked.bufferlist == NIL) && (mt->ltt->source == NIL)) {
    mt->closed = ilu_TRUE;
    bytes_read = 0;
  } else {
    bytes_read = mt_do_read (mt, buf, len, rpt, err);
  }
  LTT_UNLOCK(mt->ltt);
  rpt->tr_eom = mt->eom_received;
  rpt->tr_eof = mt->closed;
  return bytes_read;
}

/*L1.sup < trmu; L2 >= {xmu, ymu}*/
static          ilu_boolean
w3mux_Mt_Close (ilu_Transport self,
		ilu_integer * dfd,
		ILU_ERRS((bad_locks, broken_locks, internal)) * err)
{
  Mt mt = (Mt) self->tr_data;
  ilu_Error lerr;
  LTt ltt;
  ilu_boolean stat = ilu_FALSE;

  ENTER_GLOBLOCK();
  ltt = mt->ltt;
  ltt_lock(ltt);
  ILU_NOTE(W3MUX_DEBUG,
	   ("ILU(w3mux_Mt_Close): \"%s\"(%lu=>%lu)\n",
	    SESSION_NAME(mt)));
  if (!mt->eof_received) {
    stat = mt_send_close(mt, &lerr);
    ILU_MUST_BE_SUCCESS(lerr);
  };
  (void) ilu_DeltaWaitCohortRefCount(self->tr_wc, -1, &lerr);
  ILU_MUST_BE_SUCCESS(lerr);
  ltt_deallocate_session(ltt, mt);
  if ((!stat) && (ltt->source == NIL)) {
    /* ltt must be closing; make sure ltt_workproc notices us */
    ILU_NOTE(W3MUX_DEBUG,
	     ("ILU(w3mux_Mt_Close): \"%s\"(%lu=>%lu):  notifying ltt->x_CV\n",
	      SESSION_NAME(mt)));
    ilu_CondNotify(ltt->x_CV, &lerr);
    ILU_MUST_BE_SUCCESS(lerr);
  };
  LTT_UNLOCK(ltt);
  exit_globlock();
  *dfd = 0;		/* XXX bug */
  return ILU_CLER(*err);
}

/*L1, L2 unconstrained*/
static struct _ilu_TransportClass_s w3mux_TransportClass = {
  ilu_TRUE,			/* boundaried */
  ilu_TRUE,			/* reliable */
  w3mux_Mt_SetInputHandler,
  w3mux_Mt_WaitForInput,
  w3mux_Mt_InterruptST,
  w3mux_Mt_DisableWait,
  w3mux_Mt_EnableWait,
  w3mux_Mt_BeginMessage,
  w3mux_Mt_EndMessage,
  w3mux_Mt_BeginOutputMessageNonblock,
  w3mux_Mt_EndOutputMessageNonblock,
  w3mux_Mt_Push,
  w3mux_Mt_SendWholeMessage,
  w3mux_Mt_WriteBytes,
  w3mux_Mt_WriteBytesNonblock,
  w3mux_Mt_ReadBytes,
  w3mux_Mt_Close
};

static Mt mt_create (LTt ltt, ilu_cardinal session, ilu_Error *err)
{
  Mt mt;
  char buf[100];
  ilu_Error lerr;

  mt = ilu_MallocE(sizeof(*mt), err);
  if (ILU_ERRNOK(*err)) return NIL;
  mt->channel = 0;
  mt->session = session;
  mt->ltt = ltt;
  mt->transport = (ilu_Transport) ilu_MallocE(sizeof(*mt->transport), err);
  if (ILU_ERRNOK(*err)) goto free1;
  mt->transport->tr_outBuff = ilu_MallocE(MT_OUTBUFF_SIZE, err);
  mt->transport->tr_outBuff += 8;	/* space for header */
  if (ILU_ERRNOK(*err)) goto free2;
  mt->transport->tr_outNext = 0;
  mt->transport->tr_outLimit = MT_OUTBUFF_SIZE - 8;
  mt->transport->tr_inNext = mt->transport->tr_inLimit = 0;
  mt->transport->tr_inBuff = NIL;
  mt->transport->tr_class = &w3mux_TransportClass;
  mt->transport->tr_data = (void *) mt;
  mt->msgdir = None;
  mt->eof_received = ilu_FALSE;
  mt->eom_received = ilu_FALSE;
  mt->closed = ilu_FALSE;
  mt->unused = ilu_TRUE;
  mt->ltt_locked.input_state.user_buffer = NIL;
  mt->ltt_locked.input_state.user_buffer_size = 0;
  mt->ltt_locked.input_state.current = NIL;
  mt->ltt_locked.output_state.credit_limited = (ltt->default_credit > 0);
  mt->ltt_locked.output_state.credit = ltt->default_credit;
  mt->ltt_locked.wait_disables = 0;
  sprintf(buf, "w3mux_Mt_%lu", (unsigned long) session);
  mt->ltt_locked.wait_CV = ilu_CreateCondition(buf, ltt->description, err);
  if (ILU_ERRNOK(*err)) goto free3;
  mt->ltt_locked.bufferlist = NIL;
  mt->ltt_locked.local_credit = 0;
  sprintf(buf, "w3mux_Mt_%lu_WC", (unsigned long) session);
  mt->transport->tr_wc = ilu_CreateWaitCohort(buf, ltt->description, ilu_FALSE, err);
  mt->transport->tr_estFDs = ltt->source->tr_estFDs;
  if (ILU_ERRNOK(*err)) goto free4;
  return mt;

 free5:
  ilu_AcquireMutex(ilu_cmu);
  ilu_DeltaWaitCohortRefCount(mt->transport->tr_wc, -1, &lerr);
  W3MUX_RELEASE_MUTEX(ilu_cmu);
  ILU_HANDLED(lerr);
 free4:
  ilu_DestroyCondition(mt->ltt_locked.wait_CV);
 free3:
  ilu_free(mt->transport);
 free2:
  ilu_free(mt->transport->tr_outBuff);
 free1:
  ilu_free(mt);
  return NIL;
}

/***********************************************************************/
/***********************************************************************/
/*******************  Methods of w3mux_Mooring object  *****************/
/***********************************************************************/
/***********************************************************************/

/* Inside globlock */
static Mm
  mm_create (LTm ltm, ilu_cardinal channel, ilu_string name, ilu_Error *err)
{
  char buf[1000];
  Mm mm;

  mm = (Mm) ilu_MallocE(sizeof(*mm), err);
  if (ILU_ERRNOK(*err)) return NIL;
  mm->channel = channel;
  if (name == NIL)
    mm->name = NIL;
  else {
    mm->name = ilu_StrdupE(name, err);
    if (ILU_ERRNOK(*err)) goto free1;
  };
  endpoint_register_channel(ltm->endpoint, mm, err);
  if (ILU_ERRNOK(*err)) goto free2;
  mm->ltm = ltm;
  _ilu_vector_add (ltm->channels, mm, err);
  if (ILU_ERRNOK(*err)) goto free3;
  mm->ltm_locked.wait_disables = 0;
  sprintf (buf, "w3mux_Mm_%lu", (unsigned long) channel);
  mm->ltm_locked.wait_CV = ilu_CreateCondition (buf, ltm->description, err);
  if (ILU_ERRNOK(*err)) goto free4;
  mm->ltm_locked.pending_conn_reqs = NIL;
  mm->ltm_locked.pending_size = 0;
  mm->ltm_locked.pending_used = 0;
  return mm;

 free4:
  _ilu_vector_remove(mm->ltm->channels, mm);
 free3:
  endpoint_unregister_channel(ltm->endpoint, mm->channel, mm->name);
 free2:
  if (name != NIL)
    ilu_free(mm->name);
 free1:
  ilu_free(mm);
  return NIL;
}

/* Inside globlock */
static void
  mm_free (Mm mm)
{
  ilu_Error lerr;
  Endpoint ep = mm->ltm->endpoint;
  if (mm->ltm_locked.pending_conn_reqs != NIL)
    ilu_free(mm->ltm_locked.pending_conn_reqs);
  lerr = ilu_DestroyCondition (mm->ltm_locked.wait_CV);
  ILU_HANDLED(lerr);
  _ilu_vector_remove(mm->ltm->channels, mm);
  endpoint_unregister_channel(ep, mm->channel, mm->name);
  if (mm->name != NIL)
    ilu_free(mm->name);
  ilu_free(mm);  
}

/* holding ltm->lock */
static void
  mm_add_pending (Mm mm, InBuf pending, ilu_Error *err)
{
  if (mm->ltm_locked.pending_used == mm->ltm_locked.pending_size) {
    /* need more entries */
    InBuf *newarray;
    ilu_cardinal newsize;
    if (mm->ltm_locked.pending_size == 0) {
      newarray = (InBuf *) ilu_MallocE(sizeof(InBuf) * (newsize = 2), err);
    } else {
      newarray = (InBuf *) ilu_ReallocE(mm->ltm_locked.pending_conn_reqs, sizeof(InBuf) * (newsize = mm->ltm_locked.pending_size * 2), err);
    }
    if (ILU_ERRNOK(*err)) return;
    mm->ltm_locked.pending_conn_reqs = newarray;
    mm->ltm_locked.pending_size = newsize;
  };
  mm->ltm_locked.pending_conn_reqs[mm->ltm_locked.pending_used] = pending;
  mm->ltm_locked.pending_used += 1;
  ILU_CLER(*err);
  return;
}

/* holding ltm->lock */
static void
  mm_remove_pending (Mm mm, InBuf pending)
{
  ilu_cardinal i, j;

  for (i = 0;  i < mm->ltm_locked.pending_used;  i++) {
    if (pending == mm->ltm_locked.pending_conn_reqs[i]) {
      for (j = i;  j < mm->ltm_locked.pending_used - 1;  j++) {
	mm->ltm_locked.pending_conn_reqs[j] = mm->ltm_locked.pending_conn_reqs[j + 1];
      }
      mm->ltm_locked.pending_used -= 1;
      return;
    }
  }
  return;
}

/*L1.sup < trmu; L2 >= {xmu, ymu}*/
static ilu_integer
  w3mux_Mm_FdUsage(ilu_Mooring self, ilu_boolean add)
{
  Mm mm = (Mm) self->mo_data;
  ilu_integer ldfd = 0;

  /* in general, if !add, the answer is 0 unless this is the only mooring
     for the underlying tcp mooring, in which case the answer is whatever
     it yields.
     If add, the answer is 0, if the underlying tcp mooring already exists;
     otherwise it is whatever that layer returns.
   */

  ldfd = 0;
  ltm_lock(mm->ltm);
  if ((!add) && (_ilu_vector_size(mm->ltm->channels) == 1)) {
    /* should close underlying mooring if we close this mux mooring, so
       see how many dfds that would use */
    ldfd = (*mm->ltm->source->mo_dfd)(mm->ltm->source, add);
  } else {
    ldfd = 0;
  }
  ltm_unlock(mm->ltm);
  return ldfd;
}

/*L1.sup < trmu; L2 >= {xmu, ymu}*/
static          ilu_boolean
w3mux_Mm_SetConnReqHandler(ilu_Mooring self,
			   ilu_TIH tih,
			   ILU_ERRS((no_memory, imp_limit, no_resources,
				     broken_locks, internal)) * err)
{
  _ilu_Assert(0, "Attempt to set w3mux mooring connection request handler; w3mux only works in threaded runtimes\n");
  return ilu_FALSE;
}

/* Main Invariant holds; L2 >= {ymu} */
static          ilu_boolean
w3mux_Mm_WaitForConnReq(ilu_Mooring self,
			int *disabled,
			ILU_ERRS((interrupted, broken_locks)) * err)
{
  Mm mm = (Mm) self->mo_data;
  LTm ltm = ((Mm) self->mo_data)->ltm;

  ltm_lock(ltm);
  while (mm->ltm_locked.wait_disables == 0) {
    if (mm->ltm_locked.pending_used > 0) {
      *disabled = ilu_FALSE;
      ltm_unlock(ltm);
      return ILU_CLER(*err);
    }
    ILU_NOTE(W3MUX_DEBUG,
	     ("ilu_w3mux(Mm_WaitForConnReq):  mux mooring \"%s\":%lu waiting for connection requests\n",
	      CHANNEL_NAME(mm)));
    ilu_CMWait1(mm->ltm_locked.wait_CV, ltm->lock, err);
    if (ILU_ERRNOK(*err)) return ilu_FALSE;
  }
  ltm_unlock(ltm);
  *disabled = ilu_TRUE;
  return ILU_CLER(*err);
}

/* L1 >= {cmu}; L1.sup < trmu; L2 >= {xmu} */

static          ilu_boolean
w3mux_Mm_DisableConnReqWait(ilu_Mooring self,
			    ILU_ERRS((broken_locks, bad_param,
				      internal)) * err)
{
  Mm              mm = (Mm) self->mo_data;
  ltm_lock(mm->ltm);
  mm->ltm_locked.wait_disables += 1;
  ilu_CondNotify(mm->ltm_locked.wait_CV, err);
  ltm_unlock(mm->ltm);
  if (ILU_ERRNOK(*err))
    return ilu_FALSE;
  if (!_ilu_DeltaCohortWaits(self->mo_wc, 1, err))
    return ilu_FALSE;
  return ilu_TRUE;
}

static          ilu_boolean
w3mux_Mm_EnableConnReqWait(ilu_Mooring self,
			   ILU_ERRS((broken_locks, bad_param,
				     internal)) * err)
{
  Mm              mm = (Mm) self->mo_data;
  ltm_lock(mm->ltm);
  mm->ltm_locked.wait_disables -= 1;
  ilu_CondNotify(mm->ltm_locked.wait_CV, err);
  ltm_unlock(mm->ltm);
  if (ILU_ERRNOK(*err))
    return ilu_FALSE;
  if (!_ilu_DeltaCohortWaits(self->mo_wc, -1, err))
    return ilu_FALSE;
  return ilu_TRUE;
}

/* Main Invariant holds; L2 >= self's {xmu, ymu} */

static          ilu_Transport
w3mux_Mm_AcceptConn(ilu_Mooring self,
		    ilu_string * tinfo_out,
		    ilu_integer *dfd,
		    ilu_Passport pp,
		    ILU_ERRS((IoErrs)) * err)
{
  Mm mm = (Mm) self->mo_data;
  LTt ltt;
  ilu_cardinal session;
  InBuf req;
  Mt mt = NIL;
  ilu_Error lerr;
  unsigned int i;

  *dfd = 0;
  ILU_CLER(*err);
  ltm_lock(mm->ltm);
  if (mm->ltm_locked.pending_used == 0) {
    /* false alarm */
    ltm_unlock(mm->ltm);
    return NIL;
  } else {
    for (i = 0;  i < mm->ltm_locked.pending_used;  i++) {
      req = mm->ltm_locked.pending_conn_reqs[i];
      ltt = req->ltt;
      session = HEADER_SESSION_ID(&req->header);
      ltt_lock(ltt);
      /* Can't open a new session till the old one is finished... */
      if (ltt_find_session(ltt, session) == NIL)
	break;
      else
	ltt_unlock(ltt);
    }
    if (i >= mm->ltm_locked.pending_used) {
      /* no sessions can be opened yet */
      ltm_unlock(mm->ltm);
      return NIL;
    }
    mm_remove_pending (mm, req);
    ILU_NOTE(W3MUX_DEBUG,
	     ("ilu_w3mux(Mm_AcceptConn \"%s\":%lu):  processing conn req from \"%s\"...\n",
	      CHANNEL_NAME(mm), req->ltt->description));
    mt = mt_create (ltt, session, err);
    if (ILU_ERRNOK(*err)) {
      ILU_NOTE(W3MUX_DEBUG,
	       ("ilu_w3mux(Mm_AcceptConn \"%s\":%lu):  err <%s> in call "
		"to mt_create causes loss of connection request from \"%s\"\n",
		CHANNEL_NAME(mm), ILU_ERR_NAME(*err), req->ltt->description));
      LTT_UNLOCK(ltt);
      ltm_unlock(mm->ltm);
      return NIL;
    };
    mt->ltt_locked.bufferlist = req;
    mt->channel = mm->channel;
    mt->unused = ilu_FALSE;	/* implicitly */
    if (tinfo_out != NIL) {
      static char *format_string = "w3mux ch%u s%u over <%s>";
      char *buf = ilu_MallocE(strlen(format_string) + strlen(ltt->description) + 10, err);
      if (ILU_ERRNOK(*err)) {
	mt_free(mt);
	ILU_NOTE(W3MUX_DEBUG,
		 ("ilu_w3mux(Mm_AcceptConn \"%s\":%lu):  can't form peerinfo, err <%s>,"
		  " causes loss of connection request from \"%s\"\n",
		  CHANNEL_NAME(mm), ILU_ERR_NAME(*err), req->ltt->description));
      } else {
	sprintf(buf, format_string, mm->channel, session, ltt->description);
	*tinfo_out = buf;
      }
    }
    if (ILU_ERROK(*err)) {
      ltt_add_session (ltt, mt, err);
      if (ILU_ERRNOK(*err)) {
	mt_free(mt);
	ILU_NOTE(W3MUX_DEBUG,
		 ("ilu_w3mux(Mm_AcceptConn \"%s\":%lu):  can't add to list of ltt sessions, "
		  "err <%s>, causes loss of connection request from \"%s\"\n",
		  CHANNEL_NAME(mm), ILU_ERR_NAME(*err), req->ltt->description));
      }
    };
    ltt_remove_pending(ltt, req, &lerr);
    ILU_HANDLED(lerr);
#ifdef ENABLE_DEBUGGING
    if (ilu_DebugLevel & W3MUX_DEBUG) {
      unsigned long nchunks = 0, size = 0;
      InBuf b;
      for (b = mt->ltt_locked.bufferlist;  b != NIL;  b = b->next) {
	nchunks += 1;
	size += HEADER_CHUNK_SIZE(&b->header);
      }
      ilu_DebugPrintf
	("ilu_w3mux(Mm_AcceptConn \"%s\":%lu):  accepted conn req "
	 "for session %lu=>%lu from \"%s\", %lu bytes in %lu chunks received already\n",
	 CHANNEL_NAME(mm), session, mm->channel, req->ltt->description, size, nchunks);
    }
#endif
    LTT_UNLOCK(ltt);
    ltm_unlock(mm->ltm);
    if (ILU_ERROK(*err))
      return mt->transport;
    else
      return NIL;
  }
}

/*L1 >= {cmu}; L1.sup < trmu; L2 >= {xmu, ymu}*/
static          ilu_boolean
w3mux_Mm_Close (ilu_Mooring self,
		ilu_integer * dfd,
		ILU_ERRS((bad_locks, broken_locks,
			  internal)) * err)
{
  Mm mm = (Mm) self->mo_data;
  LTm ltm;

  ENTER_GLOBLOCK();
  ltm = mm->ltm;
  ltm_lock(ltm);
  mm_free(mm);
  ilu_DeltaWaitCohortRefCount(self->mo_wc, -1, err);
  ILU_MUST_BE_SUCCESS(*err);
  if (_ilu_vector_size(ltm->channels) == 0) {
    ilu_Closure cl = ilu_MallocE(sizeof(*cl), err);
    ILU_MUST_BE_SUCCESS(*err);
    cl->next = NIL;
    cl->proc = ltm_close_if_empty;
    cl->rock = (void *) ltm;
    /*
     * Rats, the locking constraints won't let us enter the cmu
     * mutex here, so we have to make a bit of a mess.
     */
    ilu_DoSoon(cl, err);
    ILU_MUST_BE_SUCCESS(*err);
  }
  ltm_unlock(ltm);
  exit_globlock();
  ilu_free(self);
  return ILU_CLER(*err);
}

/*L1, L2 unconstrained*/

static struct _ilu_Mooring_s mooringProto = {
  w3mux_Mm_FdUsage,
  w3mux_Mm_SetConnReqHandler,
  w3mux_Mm_WaitForConnReq,
  w3mux_Mm_DisableConnReqWait,
  w3mux_Mm_EnableConnReqWait,
  w3mux_Mm_AcceptConn,
  w3mux_Mm_Close,
  NIL				/* data */
};

/***********************************************************************/
/***********************************************************************/
/*************  Methods of w3mux_TransportCreator object  **************/
/***********************************************************************/
/***********************************************************************/

static ilu_boolean
  mc_check_dfds (Mc mc, int *dfd_change)
{
  ilu_Error lerr;
  int dfd;
  ilu_boolean result;

    /* Check to see whether we have the fd budget to accept a new connection */
  ilu_AcquireMutex(ilu_cmu);
  dfd = (*mc->lower->tcr_dfd) (mc->lower, ilu_FALSE);
  if (ilu_fdbudget < ilu_fdstaken + dfd) {
    if (!_ilu_ReduceFdsTo(ilu_fdbudget - dfd, NIL, &lerr)) {
      ILU_NOTE(W3MUX_DEBUG,
	    ("ilu_w3mux(mc_check_dfds):  err <%s> while attempting to reduce fds from %s to %s\n",
	     ILU_ERR_NAME(lerr), ilu_fdstaken, ilu_fdstaken - dfd));
      ILU_HANDLED(lerr);
      result = ilu_FALSE;
    } else if ((ilu_fdbudget < ilu_fdstaken + dfd) && (dfd > 0)) {
      ILU_NOTE(W3MUX_DEBUG,
	    ("ilu_w3mux(mc_check_dfds):  FD budget exhausted.\n"));
      result = ilu_FALSE;
    } else {
      *dfd_change = dfd;
      result = ilu_TRUE;
    }
  } else {
    *dfd_change = dfd;
    result = ilu_TRUE;
  }
  W3MUX_RELEASE_MUTEX(ilu_cmu);
  return result;
}

/* Main Invariant holds */
static ilu_Transport
  w3mux_Mc_CreateTransport (ilu_TransportCreator self,
			    ilu_boolean buffer,
			    ilu_integer *dfd,
			    ilu_Passport pp,
			    ILU_ERRS((IoErrs)) * err)
/*
 * The outgoing Transport instance creation procedure.  Caller
 * promises exposed buffers won't be used if !buffer (in which case
 * callee is free to not allocate any).  The result is owned by the
 * caller (who will eventually close it).  Always stores at *dfd
 * the number of FDs actually consumed, even when erring.
 */
{
  Mc mc = (Mc) self->tcr_data;
  Mt mt = NIL;
  LTt ltt = NIL;
  ilu_Transport lower = NIL;
  Endpoint ep;
  ilu_Error lerr;
  ilu_integer ldfd;
  char *description;

  ENTER_GLOBLOCK();
  ep = endpoint_get (mc->endpoint_id, ilu_FALSE, err);
  if (ILU_ERRNOK(*err)) goto free0;
  *dfd = 0;
  mt = endpoint_allocate_session (ep, err);
  if (ILU_ERRNOK(*err)) goto free0;
  if (mt == NIL) {
    /* need to create new ltt */
    description = _ilu_StringifyTinfo(mc->lower_tinfo, err);
    if (ILU_ERRNOK(*err)) return NIL;
    if (!mc_check_dfds(mc, &ldfd)) {
      ILU_NOTE(W3MUX_DEBUG,
	       ("ilu_w3mux(mc_CreateTransport):  file descriptor budget exhausted; "
		"can't create conn to \"%s\"\n", description));
      ILU_ERR_CONS1(no_resources, err, minor, ilu_nrm_fds, 0);
      goto free1;
    };
    lower = (*mc->lower->tcr_createTransport)(mc->lower, ilu_TRUE, &ldfd, pp, err);
    *dfd += ldfd;
    if (ILU_ERRNOK(*err)) goto free1;
    ltt = ltt_create(lower, NIL, ep, description, mc->lower_tinfo, pp, err);
    if (ILU_ERRNOK(*err)) goto free2;
    ltt_lock(ltt);
    mt = ltt_allocate_session(ltt, err);
    if (ILU_ERRNOK(*err) || (mt == NIL)) goto free3;
    ltt_unlock(ltt);
  };
  mt->channel = mc->channel_number;
  if ((mt->channel == 0) && (mc->channel_name != NIL)) {
    /* intern an atom to get a valid protocol ID */
    mt->channel = ltt_intern_atom (mt->ltt, mc->channel_name, err);
    if (ILU_ERRNOK(*err)) goto free4;
  }
  ILU_NOTE(W3MUX_DEBUG,
	   ("ilu_w3mux(Mt_CreateTransport):  created outgoing mux session %s(%u=>%u)\n",
	    SESSION_NAME(mt)));
  exit_globlock();
  return mt->transport;

 free4:
  if (mt != NIL) {
    ltt_deallocate_session (ltt, mt);
  };
 free3:
  if (lower != NIL) {
    ltt_free (ltt);
    ILU_ERR_CONS1(no_resources, err, minor, ilu_nrm_mux_sessions, 0);
  };
 free2:
  if (lower != NIL) {
    (void) ilu_CloseTransport(lower, &ldfd, &lerr);
    *dfd += ldfd;
    ILU_HANDLED(lerr);
  };
 free1:
  ilu_free(description);
 free0:
  exit_globlock();
  return NIL;
}

/* L1 >= {cmu}, L1.sup < trmu; L2 unconstrained */
static ilu_Mooring
  w3mux_Mc_CreateMooring(ilu_TransportCreator self,
			 ilu_TransportInfo * tinfo_out,
			 ilu_boolean buffer,
			 ilu_integer *dfd,
			 ilu_Passport pp,
			 ILU_ERRS((no_memory)) * err)
{
  Mc mc = (Mc) self->tcr_data;
  char *encoded_endpoint = NIL;
  char *encoded_name = NIL;
  char *endpoint_id = NIL;
  Endpoint ep;
  LTm ltm, ltm_new;
  Mm mm;
  ilu_cardinal channel = ((Mc) self->tcr_data)->channel_number;
  ilu_Mooring ans = NIL;
  
  ENTER_GLOBLOCK();
  if (((endpoint_id = mc->endpoint_id) == NIL) && (pp != NIL)) {
    ilu_IdentityInfo i = ilu_FindIdentity (pp, ilu_w3muxEndpointIdentity);
    ILU_NOTE(W3MUX_DEBUG,
	     ("ILU(w3mux_Mc_CreateMooring):  no endpoint specified, checking passport...\n"));
    if (i != NIL) {
      endpoint_id = ((struct endpoint_identity_info_s *) (i->ii_info))->id;
      ILU_NOTE(W3MUX_DEBUG,
	       ("ILU(w3mux_Mc_CreateMooring):  using endpoint <%s> from passport.\n",
		endpoint_id));
    } else {
      ILU_NOTE(W3MUX_DEBUG,
	       ("ILU(w3mux_Mc_CreateMooring):  no endpoint identity in passport.\n",
		endpoint_id));
    }
  }
  ep = endpoint_get (endpoint_id, ilu_TRUE, err);
  if (endpoint_id == NIL)
    ILU_NOTE(W3MUX_DEBUG,
	     ("ILU(w3mux_Mc_CreateMooring):  using default endpoint id <%s>\n", ep->id));
  if (ILU_ERRNOK(*err)) return NIL;
  if (channel == 0) {	/* use unassigned channel */
    channel = endpoint_find_unused_channel_number(ep);
    if (channel == 0) {
      ILU_NOTE(W3MUX_DEBUG,
	       ("ILU(w3mux_Mc_CreateMooring):  can't assign new channel on endpoint <%s>\n", ep->id));
      ILU_ERR_CONS1(no_resources, err, minor, ilu_nrm_mux_channels, 0);
      goto free1;
    }
  } else if (channel > MUX_LOCAL_CHANNEL_MAX) {
    ILU_NOTE(W3MUX_DEBUG,
	     ("ILU(w3mux_Mc_CreateMooring):  invalid channel # 0x%lx (too large) specified for endpoint <%s>\n",
	      (unsigned long) channel, ep->id));
    ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_mux_channel, 0);
    goto free1;
  }
  *dfd = 0;
  if ((ltm = ltm_find(mc->lower_tinfo)) == NIL) {
    ltm = ltm_new = ltm_create(mc->lower, mc->lower_tinfo, ep,
			       buffer, dfd, pp, err);
    if (ILU_ERRNOK(*err)) goto free1;
  };
  ltm_lock(ltm);
  if ((mm = mm_create(ltm, channel, mc->channel_name, err)) == NIL) goto free3;
  if ((ans = ilu_MallocE(sizeof(*ans), err)) == NIL) goto free4;
  *ans = mooringProto;
  ans->mo_data = (ilu_private) mm;
  {
    char buf[1000];
    sprintf (buf, "w3muxport_%lu", (long unsigned int) channel);
    ans->mo_wc = ilu_CreateWaitCohort(buf, "wait_cohort", ilu_FALSE, err);
  }
  if (ILU_ERRNOK(*err)) goto free5;
  if (tinfo_out) {
    char buf[100];
    encoded_endpoint = _ilu_EncodeBuffer (ep->id, strlen(ep->id), err);
    if (ILU_ERRNOK(*err)) goto free6;
    if (mc->channel_name != NIL) {
      encoded_name = _ilu_EncodeBuffer (mc->channel_name, strlen(mc->channel_name), err);
      if (ILU_ERRNOK(*err)) goto free7;
      sprintf(buf, "w3mux_%lu_%s_%s", (long unsigned int) channel, encoded_endpoint, encoded_name);
      ilu_free(encoded_name);
    } else {
      sprintf(buf, "w3mux_%lu_%s", (long unsigned int) channel, encoded_endpoint);
    }
    ilu_free(encoded_endpoint);
    *tinfo_out = _ilu_ConcatTinfo(buf, ltm->tinfo_out, err);
    if (ILU_ERRNOK(*err)) goto free6;
  }
  ltm_unlock(ltm);
  exit_globlock();
  return ans;

 free7:
  ilu_free(encoded_endpoint);
 free6:
  /* Leak wait cohort here */
 free5:
  ilu_free(ans);
 free4:
  mm_free(mm);
  ans = ILU_NIL;
 free3:
  ltm_unlock(ltm);
  if (ltm_new)
    ltm_free (ltm_new);
 free1:
  exit_globlock();
  return ans;
}

static void
  w3mux_Mc_Close(ilu_TransportCreator self)
{
  Mc cp = (Mc) self->tcr_data;
  (*cp->lower->tcr_close) (cp->lower);
  if (cp->lower_tinfo)
    ilu_free(cp->lower_tinfo);
  if (cp->channel_name != NIL)
    ilu_free(cp->channel_name);
  ilu_free(cp);
  ilu_free(self);
  return;
}

/* L1.sup < trmu; L2 unconstrained */
static ilu_integer
  w3mux_Mc_FdUsage (ilu_TransportCreator self, ilu_boolean mooring)
/*
 * Estimates how many FDs will be consumed when creating a mooring
 * (if mooring) or outgoing transport (if !mooring).  Estimate may
 * err high, but not low.
 */
{
  /* We will use 0 file descriptions in this layer, so just
   * pass along whatever's necessary in any underlying layer.
   * It may be high, but won't be low.
   */
  Mc cp = (Mc) self->tcr_data;
  return (*cp->lower->tcr_dfd) (cp->lower, mooring);
}

static struct _ilu_TransportCreator_s myCreatorProto = {
  ilu_TRUE,				/* boundaried */
  ilu_TRUE,				/* reliable */
  0,				/* tcr_holds */
  ilu_FALSE,			/* tcr_wantClose */
  w3mux_Mc_FdUsage,
  w3mux_Mc_CreateTransport,
  w3mux_Mc_CreateMooring,
  w3mux_Mc_Close,
  NIL				/* data */
};

/*L1_sup < trmu; L2 unconstrained*/
static Mc
  _w3mux_Mc_InterpretInfo(ilu_TransportInfo info,
			  ILU_ERRS((no_memory, inv_objref)) * err)
{
  Mc cp;
  ilu_TransportCreator lower;
  unsigned long id;
  ilu_TransportInfo lower_tinfo;
  char buf[1000];
  char namebuf[1000];
  char *name = NIL;
  char *decoded_endpoint = NIL;

  if (info[1] == NIL) {
    return ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_ts, NIL);
  }  else
    /* simple case for server ports "...|w3mux|..." */
    if (strcmp(info[0], "w3mux") == 0) {
    id = 0;
  } else
    /* full case "...|w3mux_<port>_<endpoint>_<name>|..." */
    if (sscanf(info[0], "w3mux_%lu_%999[^_]_%999s", &id, buf, namebuf) == 3) {
    ilu_cardinal junk;
    if (strcmp(buf, "0") != 0) {
      decoded_endpoint = _ilu_DecodeBuffer(buf, strlen(buf), &junk, err);
      if (ILU_ERRNOK(*err)) return NIL;
    }
    name = _ilu_DecodeBuffer(namebuf, strlen(namebuf), &junk, err);
    if (ILU_ERRNOK(*err)) return NIL;
  } else
    /* partial case "...|w3mux_<port>_<endpoint>|..." */
    if (sscanf(info[0], "w3mux_%lu_%999[^_]", &id, buf) == 2) {
    ilu_cardinal junk;
    if (strcmp(buf, "0") != 0) {
      decoded_endpoint = _ilu_DecodeBuffer(buf, strlen(buf), &junk, err);
      if (ILU_ERRNOK(*err)) return NIL;
    };
  } else
    /* another simple case for server ports "...|w3mux_<port>|..." */
    if (sscanf(info[0], "w3mux_%lu", &id) == 1) {
  } else
    return ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_ts, NIL);
  lower = _ilu_GetTransportCreator(info + 1, err);
  if (ILU_ERRNOK(*err))
    return NIL;
  if (lower->tcr_boundaried || !lower->tcr_reliable) {
    ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_ts, NIL);
    goto free0;
  };
  lower_tinfo = _ilu_CopyTinfo(info + 1, err);
  if (ILU_ERRNOK(*err)) goto free0;
  cp = (Mc) ilu_MallocE(sizeof(*cp), err);
  if (ILU_ERRNOK(*err)) goto free1;
  cp->lower = lower;
  cp->channel_number = id;
  cp->channel_name = name;
  cp->lower_tinfo = lower_tinfo;
  cp->endpoint_id = decoded_endpoint;
  if (ILU_ERRNOK(*err)) goto free2;
  return cp;

 free2:
  ilu_free(cp);
 free1:
  ilu_free(lower_tinfo);
 free0:
  (*lower->tcr_close)(lower);
  if (name != NIL)
    ilu_free(name);
  if (decoded_endpoint != NIL)
    ilu_free(decoded_endpoint);
  return NIL;
}

/*L1.sup < trmu*/
ilu_TransportCreator
_ilu_w3mux_TransportCreator(ilu_TransportInfo tinfo,
			    ILU_ERRS((no_memory,
				      inv_objref)) * err)
{
  ilu_TransportCreator ans;
  Mc cp;
  static ilu_boolean initialized = ilu_FALSE;

  if (!ilu_CanCondition()) {
    ilu_DebugPrintf("ILU's MUX implementation only works with threaded programs.  You must have registered your threads system with ILU before using MUX.\n");
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_threading, NIL);
  };

  if (!initialized) {
    w3mux_InitializeDataStructures(err);
    if (ILU_ERRNOK(*err))
      return NIL;
    initialized = ilu_TRUE;
  };

  cp = _w3mux_Mc_InterpretInfo(tinfo, err);
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
