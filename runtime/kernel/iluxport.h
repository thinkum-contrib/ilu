/** $Id: iluxport.h,v 1.375 1999/09/20 22:33:32 janssen Exp $
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
/* Chris Jacobi, May 2, 1997 3:48 pm PDT */

#ifndef _ILU_EXPORTS_
#define _ILU_EXPORTS_

#ifdef __cplusplus
extern "C" {
#endif

/* define dllexport to support building DLLs on Win32 */
#if (defined(WIN32)||defined(_WIN32))
/* ensure WIN32 defined */
#ifndef WIN32
#define WIN32
#endif /* end ensure WIN32 defined */
#if defined(ILU_BUILDING_KERNEL)
#define ILU_PUBLIC __declspec(dllexport) extern
#define ILU_PUBLIC_CLASS  class __declspec(dllexport)
#else
#define ILU_PUBLIC __declspec(dllimport) extern
#define ILU_PUBLIC_CLASS class __declspec(dllimport)
#endif /* defined(ILU_BUILDING_KERNEL) */
#define ILU_STDCALL __stdcall
#else
#define ILU_PUBLIC extern
#define ILU_PUBLIC_CLASS class
#define ILU_STDCALL
#endif /*(defined(WIN32)||defined(_WIN32)) */

#if (defined(WIN32)||defined(WIN16))
#include <iluwin.h>
#elif defined( macintosh )
#include <ilumac.h>
extern void ILUStartup( void );
extern void ILUShutdown( void );
#else
#include <iluconf.h>
#endif


#include <iluerror.h>

/* ilutpcod.h contains #defines for the type IDs of all of the basic
 * ILU types found in stubbers/parser/ilu.isl.  They have the form
 *	#define ILU_TYPEID_CONST_<name> "<typeid>"
 * This file is generated during the "make" step by running the
 * program "stubbers/parser/genregs".
 *
 * We conditionalize its inclusion on __ILU_PROCESSING_GENMINORS__,
 * which is only set when building the "genminors" program in this
 * directory during the "make Dist" step of ILU.  This is to prevent
 * the (at that time) non-existent ilutpcod.h file from breaking the
 * build of genminors, which doesn't need it.
 */
#ifndef __ILU_PROCESSING_GENMINORS__
#include <ilutpcod.h>
#endif /* __ILU_PROCESSING_GENMINORS__ */

#if (defined(ILU_SOLARIS2_THREADS) || defined(ILU_POSIX_THREADS) || defined(ILU_WIN32_THREADS) || defined(ILU_DCE_THREADS))
#define ILU_OS_THREADED
#endif

#ifdef ILU_FIXED_POINT_SUPPORT

#include <ilubignm.h>

typedef ilubignum_Value ilu_Bignum;

typedef enum _ilu_FixedPointRangeSize_enum {
  ilu_fprs_large = 0,
  ilu_fprs_byte = 1,
  ilu_fprs_shortcardinal = 2,
  ilu_fprs_cardinal = 3,
  ilu_fprs_longcardinal = 4,
  ilu_fprs_shortinteger = 5,
  ilu_fprs_integer = 6,
  ilu_fprs_longinteger = 7
  } ilu_FixedPointRangeSize;

#endif /* def ILU_FIXED_POINT_SUPPORT */

#ifdef ILU_REFERENCE_TYPES

typedef void * ilu_ReferenceID;

#endif /* def ILU_REFERENCE_TYPES */

/*
 * Unless specified otherwise, a result type of ilu_boolean
 * indicates ilu_TRUE is returned on normal completion, and ilu_FALSE
 * indicates something went wrong.
 * 
 * An argument of type "ilu_Error *" is never NIL, is written through
 * but not read, and, unless specified otherwise, is always given a
 * meaningful value; a proc whose result type has a
 * failure-indicating value returns this value exactly when raising
 * an error; when broken_locks is raised, the locking post-condition
 * doesn't necessarily hold.
 */


/* ================ Basic typedefs ================ */

#define ILU_ERRPRINTF	ilu_DebugPrintf

#include <ilubasic.h>
/*
 * Which #defines ILU_NIL, which we render as simply "NIL" in
 * comments in this file.
 */

#define ILU_MAX_ADDR_SPACE_LANGUAGES		5
/* An upper bound on the # of languages found in the same address space. */

typedef ilu_cardinal ilu_LanguageIndex;
        /* represents a registered language */

typedef char * ilu_Exception;
	/* address of exception description */

typedef struct _ilu_Server_s * ilu_Server;
	/* handle held by client on server */

typedef struct _ilu_Client_s * ilu_Client;
	/* handle held by server on client */

typedef struct _ilu_Connection_s * ilu_Connection;
	/* wrapper for bytestream connection */

typedef struct _ilu_Object_s * ilu_Object;
	/* wrapper around server for instances */

typedef struct _ilu_Pipe_s * ilu_Pipe;
	/* pipe for connection to other module */

typedef struct _ilu_TransportCreator_s *ilu_TransportCreator;
	/* creates transports and moorings */

typedef struct _ilu_Transport_s *ilu_Transport;
	/* abstraction of a TCP/IP or XNS Transport */

typedef struct _ilu_TransportClass_s *ilu_TransportClass;
	/* methods for Transport */

typedef struct _ilu_Mooring_s * ilu_Mooring;
	/* abstraction of a listen socket */

typedef struct _ilu_Protocol_s * ilu_Protocol;
	/* protocol binding */

typedef struct _ilu_Port_s * ilu_Port;
	/* handle held by server on bytestream */

typedef struct _ilu_Class_s * ilu_Class;
	/* class description for server */

typedef struct _ilu_MethodArg_s * ilu_MethodArg;
	/* description of an argument of a method */

typedef struct _ilu_Method_s * ilu_Method;
	/* method description for server */

typedef enum _ilu_ArgDirection_enum
	{ ilu_In=1, ilu_Out=2, ilu_InOut=3 } ilu_ArgDirection;
	/* direction of method arg */

typedef struct _ilu_Call_s ilu_Call_s, *ilu_Call;
	/* call description */

typedef ilu_refany ilu_Lock;
	/* slot for lock implementation's use */

typedef int (ILU_STDCALL *ilu_RecvProc)(int,char*,int,int);
typedef int (ILU_STDCALL *ilu_SendProc)(int,const char*,int,int);

typedef struct _ilu_IdentityInfo_s * ilu_IdentityInfo;
typedef struct _ilu_IdentityType_s * ilu_IdentityType;

typedef struct _ilu_Passport_s * ilu_Passport;

typedef ilu_string *ilu_TransportInfo;
/*
 * NIL-terminated vector of plain tinfo strings.  Both the vector
 * and the strings are in the same dynamic memory "object", and the
 * vector starts at the start of that object.  In other words, to
 * free one of these things, just free the vector.  And construct
 * them so that doesn't leak memory.
 */

ILU_PUBLIC ilu_cardinal ilu_TransportInfo_Len(ilu_TransportInfo ti);

ILU_PUBLIC ilu_TransportInfo ilu_CopyTinfo(ilu_TransportInfo,
					   ILU_ERRS((no_memory)) *);
/* creates and returns a copy of the arg */

ILU_PUBLIC ilu_string
ilu_TransportInfo_Stringify(ilu_TransportInfo,
			    ILU_ERRS((no_memory, internal/check)) *);
/* returned string malloced by callee, owned by caller */

ILU_PUBLIC void ilu_TransportInfo_Free(ilu_TransportInfo ti);

typedef ilu_string ilu_ProtocolInfo;
/*
 * NUL-terminated string, containing protocol type and parms,
 * separated by underscores.  Typical pinfo string.
 */

/*Main Invariant holds;
  Call-Locking(call, IHi) before,
  Call-Locking(call,  No) after*/
typedef void    (*ilu_StubProc) (ilu_Call);

/* perhaps these three structs should be in iluntrnl.h, but the C, C++, and Lisp
   runtimes (and possibly others) use their definition. */

typedef struct _ilu_MethodArg_s {
  /* read-only; no locks needed */

  ilu_string      ma_name;	/* ISL name of argument */
  ilu_string      ma_type;	/* type ID of argument's type */
  unsigned        ma_dir:2;	/* actually an ilu_ArgDirection */
  unsigned        ma_sibling:1;	/* true if sibling */
}               ilu_MethodArg_s;

struct _ilu_Method_s {
  /* read-only; no locks needed */
  
  ilu_string		me_name;
  ilu_cardinal		me_id;			/* on-the-wire code */
  unsigned		me_cacheable : 1;	/* functional? */
  unsigned		me_asynchronous : 1;	/* wait after calling? */
  unsigned		me_return_vals : 1;	/* true if has return type or inout or out parms */
  unsigned		me_exceptionCount : 13;	/* num exns in list */
  unsigned		me_argCount : 16;	/* number of args */
  ilu_Exception *	me_exceptionVector;	/* list o possible exns */
  ilu_MethodArg_s *	me_argVector;		/* vector of ilu_MethodArg_s */
  ilu_string		me_returnType;		/* NIL (for void) or type ID of return type */
  ilu_StubProc		me_stubprocs[ILU_MAX_ADDR_SPACE_LANGUAGES];
						/* vector of pointers to true stubs */
};

#ifdef ILU_HTTPNG_OBJECTS

typedef struct _ilu_ObjectState_s {
  /* read-only, no locks needed */
  ilu_string	os_name;
  ilu_string	os_typeuid;
}		ilu_ObjectState_s;

typedef struct _ilu_ObjectState_s * ilu_ObjectState;

#endif /* def ILU_HTTPNG_OBJECTS */

struct _ilu_Class_s {
  /* L1, L2 unconstrained */

  ilu_string      cl_name;	/* ILU name of class */
  ilu_string      cl_brand;	/* brand on class */
  ilu_string      cl_unique_id;	/* unique id for type graph of class */
  ilu_string      cl_singleton;	/* pinfo if class is a singleton */
  ilu_boolean     cl_collectible;	/* class is collectible? */

  ilu_string      cl_doc_string;/* if any */

  ilu_Method      cl_methods;
  ilu_cardinal    cl_method_count;
  ilu_cardinal    cl_scls_count;/* number of superclasses */
  ilu_string     *cl_scls_ids;	/* address of vector of UID strings */

  /* L1 >= {otmu} */

  ilu_Class      *cl_sclses;	/* address o vector o ptrs to
				 * supercls */

#ifdef ILU_HTTPNG_OBJECTS
  ilu_cardinal	  cl_nstate_fields;
  ilu_ObjectState_s *cl_state;	/* vector of object state */

  unsigned	  cl_local:1;	/* local object? */
  unsigned	  cl_sealed:1;	/* sealed? */
#endif

  unsigned        cl_shown:1;	/* temp bit for use in type.c */
  unsigned        cl_optional:1;/* may be NIL? -- IDL support */
  unsigned	  cl_phony:1;	/* cons'ed up by kernel to handle
				   multiple supertypes prob */
};

typedef struct ilu_FineTime_s ilu_FineTime;


/* ================ Errors ================ */
/* Some commonly used errors */

#include <iluerrs.h>


/* ================ Run-time Types ================ */
/* Support for runtime types, and VARIANT */

enum _ilu_TypeKind_e {
  ilu_byte_tk		= 0,
  ilu_boolean_tk	= 1,
  ilu_character_tk	= 2,
  ilu_shortcharacter_tk	= 3,
  ilu_shortinteger_tk	= 4,
  ilu_integer_tk	= 5,
  ilu_longinteger_tk	= 6,
  ilu_shortcardinal_tk	= 7,
  ilu_cardinal_tk	= 8,
  ilu_longcardinal_tk	= 9,
  ilu_real_tk		= 10,
  ilu_shortreal_tk	= 11,
  ilu_longreal_tk	= 12,
  ilu_object_tk		= 13,
  ilu_pipe_tk		= 14,
  ilu_optional_tk	= 15,
  ilu_alias_tk		= 16,
  ilu_union_tk		= 17,
  ilu_sequence_tk	= 18,
  ilu_record_tk		= 19,
  ilu_array_tk		= 20,
  ilu_enumeration_tk	= 21,
  ilu_pickle_tk		= 22,
  ilu_string_tk		= 23,
  ilu_fixedpoint_tk	= 24,
  ilu_reference_tk	= 25
};
typedef enum _ilu_TypeKind_e			ilu_TypeKind;

#if (defined(ADD_VARIANT_SUPPORT) || defined(ADD_TYPE_REGISTRATION_SUPPORT))
#include <ilutypes.h>
#else
typedef void *ilu_Type;
#endif /* ADD_VARIANT_SUPPORT || ADD_TYPE_REGISTRATION_SUPPORT */

/* The following type ids are exported from the kernel for the
   use of language runtimes */

ILU_PUBLIC const char ilu_TypeID_ilu_ProtocolErrorDetail[];
ILU_PUBLIC const char ilu_TypeID_ilu_CString[];
ILU_PUBLIC const char ilu_TypeID_ilu_pickle[];
ILU_PUBLIC const char ilu_TypeID_ilu_shortcharacter[];
ILU_PUBLIC const char ilu_TypeID_ilu_character[];
ILU_PUBLIC const char ilu_TypeID_ilu_boolean[];
ILU_PUBLIC const char ilu_TypeID_ilu_byte[];
ILU_PUBLIC const char ilu_TypeID_ilu_longreal[];
ILU_PUBLIC const char ilu_TypeID_ilu_longcardinal[];
ILU_PUBLIC const char ilu_TypeID_ilu_longinteger[];
ILU_PUBLIC const char ilu_TypeID_ilu_shortreal[];
ILU_PUBLIC const char ilu_TypeID_ilu_shortcardinal[];
ILU_PUBLIC const char ilu_TypeID_ilu_shortinteger[];
ILU_PUBLIC const char ilu_TypeID_ilu_real[];
ILU_PUBLIC const char ilu_TypeID_ilu_cardinal[];
ILU_PUBLIC const char ilu_TypeID_ilu_integer[];
ILU_PUBLIC const char ilu_TypeID_ilu_CORBA_Object[];

/* ================ Locking ================ */
/****************************************************************
We use mutual exclusion.  A "mutex" is an object that can be held by
one thread at a time.  We say a thread "enters" and "exits" a mutex
(we used to say "acquires" and "releases").  A thread is either "inside"
or "outside" a mutex.  The operation of "entering" a mutex blocks the
calling thread until no thread is inside the mutex, then enters.

Associated with each mutex is a set of variables, and an invariant
that involves these variables.  A thread may inspect and modify those
variables only while inside the mutex.  The thread assumes the
invariant to hold when entering the mutex; may violate the invariant
while inside the mutex; and must restore the invariant before exiting
the mutex.  Among other things, this means it would be an error for a
thread to try to enter a mutex while the thread is already inside the
mutex.

For each variable, we have "locking comments" that indicate which
mutex is associated with the variable.  We haven't been rigorous about
writing down the mutex invariants.  This may be symptommatic of the
fact that we sometimes think of a mutex as simply an object lock on
its associated variables.

We also have locking comments on procedures, giving pre- and post-
conditions wrt mutexes held/not held.

ILU has two classes of mutexes: connection mutexes and non-connection
mutexes.

There are three mutexes per connection: "call" (callmu), "I/O" (iomu),
and "wait" (waitmu).  The call and I/O mutexes are held during I/O of
a request or reply message on the connection.  The wait mutex is held
while waiting for a reply.  The I/O and wait mutexes are held while
closing a connection.  For a call over a connection that uses a
concurrent protocol, and for a call that's part of a pipeline and uses
a non-concurrent protocol, the call mutex is released while waiting
for a reply; for a non-pipelined call over a connection that uses a
non-concurrent protocol, the call mutex is held from the start of the
request to the end of the reply.  When starting a call, one of the
server's available connections is used, or a new one is created and
used if all the existing ones have their call mutexes held.

Here are the non-connection mutexes:
otmu:	  global mutex for object type data structures;
cmu:	  global LRU list of connections
prmu:	  global mutex for protocol registry
trmu:	  global mutex for transport registry
(lamu:    global mutex for language registry)
gcmu:	  global mutex for GC data structures
daimu:	  default global mutex for alarm implementation.
debugmu:  global mutex for debugging output
cvtomu:	  global mutex for use in generic impl. of CV wait timeouts
server:	one mutex per server.

Here are some non-connection mutex variables:
mxamu:	mutex for alarm multiplexor
timu:	mutex for an alarm's implementation

Our main technique for avoiding deadlocks is to put a partial order on
mutexes, and acquire mutexes in an order consistent with the partial
order.  That is, a thread may enter mutex B while holding mutex A only
if A < B (we have a few carefully managed exceptions to this rule;
more on this later).  The partial order is the transitive closure of
the following relationships.

cmu < server
server < prmu
server < trmu
gcmu < server
gcmu < timu
gcmu < cmu		(for gc.c:gcaInvoke)
cmu < timu		(for call.c:GR{Set,Cancel})
prmu < otmu		(for Protocol->interpret_request)
cmu < trmu		(for impl vs. decl of ilu_InventID)
trmu < timu		(for _ilu_CloseIoingConnection call tc_close call ilu_DoSoon)
cvtomu < timu		(for generic impl of CV timeouts)
X < debugmu, for all other non-connection X
conn.iomu < X, for all non-connection X
conn.callmu < conn.iomu
conn.waitmu < conn.iomu
conn.callmu < conn.waitmu
[[
OLD: we used to say:
 relcal => conn.waitmu < conn.callmu
!relcal => conn.callmu < conn.waitmu

where
relcal == (connection_concurrent(conn) || conn->co_pipeline).
]]

The exceptions to the entering order rule are this: a thread may enter
a connection mutex while it holds any collection of non-connection
mutexes and connection mutexes of other connections --- this rule
declines to allow only mutexes of the same connection (and one of the
two cases of that is allowed by the partial order rule).

We use the symbols L2 and L1 to stand for the sets of connection and
non-connection mutexes held by a thread, respectively.  We write ">="
for the set inclusion relation.  We write "L1.sup < X" to mean that
either (a) L1 is empty, or (b) the maximum elment of L1 (the partial
order rule says there must be exactly one maximal element whenever L1
isn't empty) precedes X in the partial order.  We write "L1.sup = X"
to mean that L1 is not empty and its maximum member is X.  We don't
speak of "L2.sup" because a thread is allowed to violate the partial
order rule with respect to L2 mutexes.

Lemma 1: There can be no deadlocks that
involve only non-connection mutexes.
This is so because threads acquire non-connection mutexes in
an order consistent with a fixed partial order.

Deadlocks involving connection mutexes are avoided by more complicated
reasoning.

There is a locking invariant called the "Main Remnant":

  forall conn: (L2 >= {conn.iomu}) => (L2 >= {conn.callmu});

it holds at all times for all threads, except inside certain
procedures involved with closing a connection (and these procedures
don't enter the call mutex for the connection).  This means that a
thread that will hold both the call and I/O mutexes of a connection
acquires the call mutex first.  There is a common locking invariant,
called the "Main Invariant":

  L1 = {} and Main Remnant.

It holds in many places.  The Main Invariant is exactly what's guaranteed
to hold while an application's service routines are called.  The Main
Invariant is among the things guaranteed to hold while a stub is
marshalling or unmarshalling.  The Main Invariant is exactly what's
guaranteed to hold while waiting for I/O on a File Descriptor to be
enabled.  The Main Invariant is among the things guaranteed to hold while
doing I/O on a File Descriptor.

While waiting for a reply message to arrive, a thread may hold only:
(a) the call and/or wait mutexes of the connection, and (b) call and
I/O mutexes of other connections.

Lemma 2: In a multi-threaded runtime, there can be no deadlocks
involving both L1 and L2 mutexes.  This is so because a thread holds
no L1 mutexes while blocked waiting to acquire an L2 mutex (see the
procedures for entering L2 mutexes).

Lemma 3: In a multi-threaded runtime, there can be no deadlocks
involving only L2 mutexes.  The L2 mutexes of a given connection are
always entered in a total order (given above); pretend for now that
the order is fixed.  Thus, we need consider only deadlocks involving
multiple connections.  When might a thread block trying to enter a
mutex of some connection C2 while holding a mutex of some other
connection C1?  There are two cases:

1. while reading an object reference from C1 and making a type or GC
callback over C2; or

2. when making a cross-language in-memory call, while in
ilu_FinishRequest for C1, executing the application-level service
method, and making a call over C2.

(Note that:

* when making a cross-language in-memory call, while in
ilu_FinishRequest for C1, there will be manipulations of mutexes of
the complementary connection, but they only happen while C1.call is
held, and so never block;

* when unmarshalling an object reference in the call message of an
in-memory call, no type or GC callback will ever need to be made.)

Note that in both case 1 and case 2, C2 is chosen by ilu_StartCall at
some time after C1 was last entered, and the blockage on C2 occurs
some time after that.  ilu_StartCall will not choose a connection
whose call mutex is held.  A deadlock would involve a cycle of
instances I1, I2, ... IN of cases 1 and/or 2 where I1.C2 = I2.C1,
I2.C2 = I3.C1, ... I(N-1).C2 = IN.C1, and IN.C2 = I1.C1.  When IX.C2 =
IY.C1, C1 is held and was last entered some time after it was chosen
as C2 for instance IX; therefore IY's choice of C2 is made some time
after IX's.  Chaining this temporal precendence tells us that IN's
choice of C2 was made some time after I1's, which in turn was made
after I1.C1 was last entered --- which means ilu_StartCall would not
choose I1.C1 as IN's C2.

Unfortunately, the order among the L2 mutexes of a given connection
can change over time, and this breaks the proof of Lemma 3.

Theorem: There can be no deadlocks.

Proof: In a single-threaded runtime, no thread ever blocks trying to
enter an L2 mutex; Lemma 1 covers the remaining cases.  In a
multi-threaded runtime, the above three lemmas cover the three
possible cases; too bad Lemma 3 isn't true!

There is another common requirement, which is with respect to some
server s and object type cl, is called Inside(s, cl), and is this:

  ~GC(cl)	      => L1 >= {      cmu, s};
   GC(cl) &&  true(s) => L1 >= {gcmu, cmu, s};
   GC(cl) && ~true(s) => L1  = {      cmu, s} && Main Remnant;
   L2 not otherwise constrained.

Note that this invariant has the property that if C2 is a subtype of
C1, then Inside(s, C1) => Inside(s, C2).  This property is used in
situations where we know some static type of an object, but not
necessarily the most specific type (yet).

There are two common refinements of the Main Invariant, known as
Call-Invariant and Call-Locking.  One or the other holds during most
of the processing of a call, on both the client and server sides.
They involve some members of the call data structure, and
Call-Invariant involves an ilu_Error*.  Here is the definition:

Call-Invariant(call, err) == Call-Remnant(call, err, L1=={});

Call-Locking(call) == ( L1=={} && Call-Locking-Remnant(call) )

where

Call-Remnant(call, err, L1stmt)
==
ILU_ERR_SWITCH(*err) {
  ILU_ERR_CASE2(bad_locks, broken_locks)
    ilu_TRUE;
  ILU_ERR_ELSE
    L1stmt && Call-Locking-Remnant(call)
} ILU_ERR_ENDSWITCH;

and

Call-Locking-Remnant(call)
==
Main Remnant &&
switch (call->ca_ms) {
    case ilu_cmsNo: Call-No(call);
    case ilu_cmsLo: Call-Lo(call);
    case ilu_cmsHi: Call-OHi(call);
} &&
((call->ca_disownWait
  || (call->ca_ms == ilu_cmsHi && call->ca_msInput && !call->ca_dontWait))
 <=> call.conn && L2 >= {call's conn's waitmu})

This uses three other locking abstractions:

Call-No(call) == !call.conn || L2 disjoint {call's conn's callmu, iomu};

Call-Lo(call) == call.conn &&
		 L2 disjoint {call's conn's         iomu} &&
		(L2 disjoint {call's conn's callmu}
		 iff (proto concurrent or conn doing a pipelined call));

Call-OHi(call) == call.conn && L2 >= {call's conn's callmu, iomu};

Call-IHi(call) == call.conn && L2 >= {call's conn's callmu, iomu} &&
		  (call->ca_dontWait || L2 >= {call's conn's waitmu}).

We furthermore define some convenient bundles:

Call-VNo(call) == Call-No(call) && L2 disjoint {call's conn's waitmu}

Call-VLo(call) == Call-Lo(call) && L2 disjoint {call's conn's waitmu}

Call-Locking(call, IHi) == Call-Locking(call) &&
			   call->ca_ms==ilu_cmsHi && call->ca_msInput

Call-Locking(call, OHi) == Call-Locking(call) &&
			   call->ca_ms==ilu_cmsHi && !call->ca_msInput

Call-Locking(call, Lo)  == Call-Locking(call) &&
                           call->ca_ms==ilu_cmsLo

Call-Locking(call, No)  == Call-Locking(call) &&
                           call->ca_ms==ilu_cmsNo

Call-Locking(call, VLo) == Call-Locking(call, Lo) &&
			   L2 disjoint {call's conn's waitmu}

Call-Locking(call, VNo) == Call-Locking(call, No) &&
			   L2 disjoint {call's conn's waitmu}

Throughout most of the kernel interface, an ilu_Call is an opaque data
structure.  But two members, ca_ms and ca_msInput, are revealed in the
above definitions.  This is mainly so we can write an accurate locking
comment for ilu_FinishCall, which is called in a great variety of
circumstances that are distinguishable (with respect to locking) by no
state directly accessible through this interface.

For variables, the locking comments say what mutexes must be held to
access the variable.  For procedure values, the locking comments say
what mutexes must be held to call the procedure, and, if the procedure
changes the set of held mutexes, how.  Both sorts of comment are
applicable to procedure-valued variables; we prefer to document the
locking pre- and post-conditions in a typedef of the procedure type,
and describe the variable/mutex association in the usual way.

We have two sorts of locking comments: those about L1, those about L2.
Locking comments come in blocks.  There are two kinds of blocks of
locking comments: a "sticky" block is followed by a blank line; a
"one-shot" is not.  A locking comment is also called "sticky" or
"one-shot", depending on the kind of the comment block in which the
comment is contained.  A one-shot comment applies only to the
immediately following item.  A sticky comment of a certain sort
applies to all items between it and the next sticky comment of the
same sort, except those items to which a one-shot comment of the same
sort applies.

When a procedure raises INTERNAL/broken_locks, its locking
post-condition does not necessarily hold.


Sadly, we need condition variables to get high-performance
multithreaded operation.  A thread can wait on a condition variable.
Another thread can "notify" that condition variable.  This causes all
threads currently waiting on the condition variable to return from the
wait operation.  To prevent timing splinters, decisions about waiting
and notifying should be made inside a mutex.  This means the mutex
must be released while waiting on a condition variable, and there must
be no possibilty of a thread switch between the release of the mutex
and the start of the wait; the wait operation thus takes the mutex as an
argument, because in a pre-emptive threads environment (eg, PCR) the
release and the wait must be an atomic thread operation.

Some runtimes (eg, a single-threaded one) cannot support condition
variables; these runtimes supply NIL for all the condition variable
operations.  */

/*L1, L2 unconstrained*/

typedef ilu_private ilu_Mutex, ilu_Condition;

ILU_PUBLIC ilu_Mutex ilu_otmu;	/* ..for object type data struct.s */
ILU_PUBLIC ilu_Mutex ilu_cmu;	/* Global mutex for conn mgmt */
ILU_PUBLIC ilu_Mutex ilu_prmu;	/* Global mutex for transp. reg'y */
ILU_PUBLIC ilu_Mutex ilu_trmu;	/* Global mutex for proto. reg'y */
ILU_PUBLIC ilu_Mutex ilu_gcmu;
ILU_PUBLIC ilu_Mutex ilu_daimu;	/* For default alarm impl */
ILU_PUBLIC ilu_Mutex ilu_debugmu;
  /* for synchronizing debug messages */
ILU_PUBLIC ilu_Mutex ilu_cvtomu;
  /* For generic CV wait timeout impl */

/* We don't declare here what timu is; each implementation of alarms
   has the freedom and responsibility to choose an implementation of
   timu.  ilu_daimu is available for use by one implementation per
   program. */

/* L1.sup < m before, L1.sup = m after */
#define ilu_EnterMutex(m,err) \
	ilu_EnterMutexWork(m,ilu_FALSE,err,__FILE__,__LINE__)
/*
 * Blocks until acquisition succeeds or an error is raised.  Returns
 * ilu_TRUE on success, ilu_FALSE on failure.  Sets *err appropriately for a
 * kernel interface call (bad_param => internal/inv_mutex, bad_locks
 * stet, others => internal/unhandled).
 */

/* L1.sup < m before, L1.sup = m after */
#define ilu_ReEnterMutex(m,err) \
	ilu_EnterMutexWork(m,ilu_TRUE,err,__FILE__,__LINE__)
/*
 * Blocks until acquisition succeeds or an error is raised.  On
 * success, returns ilu_TRUE without touching (*err).  On failure returns
 * ilu_FALSE after calling ILU_HANDLED on the incoming (*err) and then
 * setting (*err) to broken_locks.
 */

/* L1.sup < m before, L1.sup = m after */
ILU_PUBLIC          ilu_boolean
ilu_EnterMutexWork(ilu_Mutex m, ilu_boolean hard,
	       ILU_ERRS((bad_locks, internal, broken_locks)) * err,
		   const char *file, int line);

/* L1 >= {m} before, L1 not >= {m} after */
#define ilu_ExitMutex(m,hard,err) \
	ilu_ExitMutexWork(m,hard,err,__FILE__,__LINE__)
/*
 * Releases held lock.  On success, returns ilu_TRUE without modifying
 * *err.  On failure examines hard.  If true, raises broken_locks;
 * otherwise, maps error like ilu_EnterMutex.
 */

/* L1 >= {m} before, L1 not >= {m} after */
ILU_PUBLIC          ilu_boolean
ilu_ExitMutexWork(ilu_Mutex m, ilu_boolean hard,
	       ILU_ERRS((bad_locks, internal, broken_locks)) * err,
		  const char *file, int line);

#define ilu_EnterServerMutex(s,h,e) \
	ilu_EnterServerMutexFull(s,h,e,__FILE__,__LINE__)
/* See comment below on err setting behavior. */

ILU_PUBLIC      ilu_boolean
ilu_EnterServerMutexFull(ilu_Server server, ilu_boolean hard,
			 ILU_ERRS((bad_locks, broken_locks,
				   internal)) * err,
			 char *filename, int lineno);
/*
 * Blocks until acquisition succeeds or an error is raised.  Returns
 * ilu_TRUE upon success, ilu_FALSE upon failure.  (*err) is an INOUT
 * parameter if (hard), an OUT parameter if (!hard).  If (!hard),
 * (*err) is always set apropriately for a kernel call.  If (hard),
 * (*err) is left untouched in the success case, and in the failure
 * case either: (a) (*err) indicated success upon entry and this
 * proc sets it to indicate failure upon return, (b) this proc calls
 * ILU_HANDLED(*err) and then sets (*err) to broken_locks, or (c)
 * (*err) indicated a problem upon entry and this proc leaves it
 * unchanged.
 */

#define ilu_ExitServerMutex(s,h,e) \
	ilu_ExitServerMutexFull(s,h,e,__FILE__,__LINE__)

ILU_PUBLIC      ilu_boolean
ilu_ExitServerMutexFull(ilu_Server server, ilu_boolean hard,
			ILU_ERRS((bad_locks, broken_locks,
				  internal)) * err,
			char *filename, int lineno);
/*
 * Frees the server if appropriate, and exits the server's mutex.
 * On success, returns ilu_TRUE without modifying *err.  On failure
 * examines hard.  If true, raises broken_locks; otherwise, maps
 * error like ilu_EnterMutex.
 */

/*L1 disjoint {m}*/
ILU_PUBLIC      ilu_boolean
ilu_DestroyMutex(ilu_Mutex m,
		 ILU_ERRS((bad_locks, bad_param, internal)) * err);
/*
 * Frees (m) and everything owned by it.  (m) not held by any
 * thread, otherwise may raise bad_locks. May raise bad_param if (m)
 * not valid.
 */

ILU_PUBLIC ilu_boolean 
ilu_CondNotify(ilu_Condition c,
	       ILU_ERRS((broken_locks)) * err);
/*
 * Returns true iff success.  Sets *err appropriately for a kernel
 * interface call: broken_locks if c invalid or the LockTech doesn't
 * do conditions.
 */

/* L1.sup = m */
#define ilu_CMWait1(c,m,err)	ilu_CMWait2(c,m,m,err)
/*
 * Atomically exit m and commence waiting on c.  After c is
 * notified, enter m. Returns true iff success.  Sets *err
 * appropriately for a kernel interface call: broken_locks if c or m
 * invalid, if m not held, or if the LockTech doesn't do conditions
 * (as if there were a hard==ilu_TRUE argument).
 */

/* L1.sup = m */
#define ilu_CMWait1TO(c,m,tout,err)	ilu_CMWait2TO(c,m,m,tout,err)

/* L1.sup = m1 */
#define ilu_CMWait2(c,m1,m2,err) \
	ilu_CMWait2TO(c,m1,m2,ILU_NIL,err)

/* L1.sup = m; L1 >= {m2} */
#define ilu_CMWait2TO(c,m,m2,tout,err) \
	ilu_CMWait2Full(c,m,m2,tout,err,__FILE__,__LINE__)

/* L1.sup = m; L1 >= {m2} */
ILU_PUBLIC      ilu_boolean
ilu_CMWait2Full(ilu_Condition c, ilu_Mutex m,
		ilu_Mutex m2, const ilu_FineTime * timeout,
		ILU_ERRS((broken_locks)) * err,
		const char *filename, int lineno);
/*
 * If m2 != m, exit m2.  Then atomically exit m and commence waiting
 * on c or timeout.  When (!timeout), the timeout is infinite;
 * otherwise, the wait times out ASAP after non-relative time
 * (*timeout) arrives. Timeout does not cause an error to be raised;
 * the caller has no way to distinguish timeout from c being
 * notified.  After c is notified or the timeout occurs, enter m2 if
 * m2 != m, then enter m. Returns true iff success.  Sets *err
 * appropriately for a kernel interface call: broken_locks if c, m,
 * or m2 invalid, if m or m2 not held, or if the LockTech doesn't do
 * conditions (as if there were a hard==ilu_TRUE argument).
 */

ILU_PUBLIC ilu_Mutex ilu_CreateMutex( ilu_string d1, ilu_string d2 );
/* The concatenation of d1 & d2 describes the mutex;
 * storage for them owned by caller. */

/*L1.sup < m before, L1.sup = m after*/
ILU_PUBLIC void ilu_AcquireMutex( ilu_Mutex m );
/* Blocks until acquisition succeeds. */

/* L1 >= {m} */
ILU_PUBLIC void     ilu_HoldMutex(ilu_Mutex m);
/* Checks that the caller holds the given mutex. */

/*L1 >= {m} before, L1 not >= {m} after*/
ILU_PUBLIC void ilu_ReleaseMutex( ilu_Mutex m );
/* Releases held lock. */

/* Locking unconstrained */
ILU_PUBLIC ilu_Mutex ilu_GetOTMutex(void);

ILU_PUBLIC ilu_boolean ilu_CanCondition(void);

ILU_PUBLIC      ilu_Condition
ilu_CreateCondition(ilu_string d1, ilu_string d2,
		    ILU_ERRS((bad_param/threading,
			      no_memory, no_resources)) * err);
/* Strings owned by caller. */

ILU_PUBLIC ILU_ERRS((CantCondition)) ilu_NotifyCondition(ilu_Condition c);

ILU_PUBLIC ILU_ERRS((CantCondition)) ilu_DestroyCondition(ilu_Condition c);

/*before: 				       L1.sup < cmu;
  before: cl collectible		    => L1.sup < gcmu;
  before: cl collectible & server surrogate => Main Invariant holds;
  after:  Inside(server, cl)*/
ILU_PUBLIC void ilu_EnterServer(ilu_Server server, ilu_Class cl);
/* Needed by LS runtime to call GetLanguageSpecificObject or
 * RegisterLSO */

/*before: Inside(server, cl);
  after:				      L1 disjoint {cmu, server};
  after: cl collectible			   => L1 disjoint {gcmu};
  after: cl collectible & server surrogate => Main Invariant holds*/
ILU_PUBLIC void ilu_ExitServer(ilu_Server server, ilu_Class cl);

/******** Supplying the impl of mutexes ********/

typedef void Void;

typedef struct {
  /* All fields are readonly. L1, L2 unconstrained for access. */
  
  ilu_boolean lt_canTimeoutWait;
  /* When ilu_FALSE, lt_wait can only be called with (timeout==NIL). */
  
  /* L2, Main unconstrained for calling */

  /* L1 unconstrained */
  ilu_Mutex(*lt_mcreate) (ilu_string d1, ilu_string d2);
  /*
   * The pair (d1, d2) describes the mutex; storage for strings
   * owned by caller.  Returns NIL on mem. alloc. failure.
   */

  /* L1 unconstrained */
  Void(*lt_muncons) (ilu_Mutex m, ilu_string * d1,
		     ilu_string * d2, ILU_ERRS((bad_param)) * err);
  /*
   * Reveals strings given to lt_mcreate.  Returned strings owned by
   * mutex.  May raise bad_param when m is not valid; raises
   * bad_param when d1 or d2 is null.  m is valid when it's non-null
   * and the result of a previous call on lt_mcreate.
   */

  /* L1.sup < m before, L1.sup = m after */
  Void(*lt_acquire) (ilu_Mutex m,
		     ILU_ERRS((bad_param, bad_locks)) * err);
  /*
   * May raise bad_param if m not valid; raises bad_locks if m held
   * at entry (should we allow deadlock?).
   */

  /* L1 >= {m} */
  Void(*lt_hold) (ilu_Mutex m,
		  ILU_ERRS((bad_param, bad_locks)) * err);
  /*
   * May raise bad_param if m not valid; raises bad_locks if not
   * held.
   */

  /* L1 >= {m} before, L1 disjoint {m} after */
  Void(*lt_release) (ilu_Mutex m,
		     ILU_ERRS((bad_param, bad_locks)) * err);
  /*
   * May raise bad_param if m not valid; raises bad_locks if not
   * held at entry.
   */

  /* L1 disjoint {m} */
  Void(*lt_mdestroy) (ilu_Mutex m,
		     ILU_ERRS((bad_param, bad_locks)) * err);
  /*
   * Even frees (m).  May raise bad_param if m not valid, bad_locks
   * if m held by any thread.
   */

  /* L1 unconstrained */

  ilu_Condition(*lt_ccreate) (ilu_string d1, ilu_string d2);
  /*
   * Returns ILU_NIL on memory alloc failure.  Strings owned by
   * caller.
   */

  Void(*lt_cuncons) (ilu_Condition c, ilu_string * d1,
		     ilu_string * d2, ILU_ERRS((bad_param)) * err);
  /* Reveals strings given to lt_ccreate. */
  /* Returned strings owned by c. */

  Void(*lt_notify) (ilu_Condition c,
		    ILU_ERRS((bad_param)) * err);
  Void(*lt_cdestroy) (ilu_Condition c,
		      ILU_ERRS((bad_param)) * err);

  /* L1.sup = m */
  Void(*lt_wait) (ilu_Condition c, ilu_Mutex m, ilu_Mutex m2,
		  const ilu_FineTime * timeout,
		  ILU_ERRS((bad_param, bad_locks)) * err);
  /*
   * If m2 != m, exit m2.  Then atomically exit m and commence
   * waiting on c or timeout.  When (!timeout), the timeout is
   * infinite; otherwise, the wait times out ASAP after non-relative
   * time (*timeout) arrives.  Timeout does not cause an error to be
   * raised; the caller has no way to distinguish timeout from c
   * being notified.  After c is notified or the timeout occurs,
   * enter m2 if m2 != m, then enter m.  Of course, the caller can't
   * know or care whether any of m and m2 are entered & exited
   * multiple times after c is notified; this freedom is needed to
   * implement this procedure in terms of a simpler primitive that
   * has no m2 parameter and re-acquires m after c is notified.
   * Returns true iff success. Raises bad_param if c, m, or m2 not
   * valid, bad_locks if m not held, or if m2 different from m and
   * not held.
   */

}               ilu_LockTech;

ILU_PUBLIC void
ilu_SetLockTech(ilu_LockTech * lt,
		ILU_ERRS((bad_param, no_memory)) * err);
/*
 * Call this procedure once at startup time to tell the kernel how
 * to manage mutexes.  You want to call this as soon as possible,
 * because mutexes are used to coordinate access to everything else.
 * The default implementation will be used, if necessary, before
 * this proc is called; some languages, like C++, provide very poor
 * control over the order in which modules' start codes are
 * executed.  This proc must be called at a time when no mutexes are
 * held, and ilu_CreateMutex and ilu_CreateCondition have not yet
 * been called; violations are sometimes detected.  The procs are
 * like their iluntrnl.h counterparts.  A single-threaded runtime
 * can give NIL as the argument.  A single-threaded runtime must not
 * supply non-NIL condition var procs; a multi-threaded runtime must
 * provide non-NIL ones.
 */


/* ================ Thread Forking ================ */

/*Main Invariant holds; L2 no further constrained*/
typedef void    (*ilu_ClosureProc) (void *rock);

/* L1, L2 unconstrained */
/*
 * This is the signature for a procedure which will fork a new
 * thread, running PROC(ROCK), or signal an error.
 */
typedef ilu_boolean (*ilu_ForkProc) (ilu_ClosureProc /* PROC */,
				     void * /* ROCK */,
				     ILU_ERRS((no_memory,
					       no_resources,
					       internal)) *);

/* L1, L2 unconstrained */
ILU_PUBLIC      ilu_boolean
ilu_Fork(ilu_ClosureProc /* PROC */ ,
	 void * /* ROCK */ ,
	 ILU_ERRS((no_memory,
		   no_resources,
		   internal)) *);
/*
 * Applicable only when threaded; raises internal otherwise.  Fork a
 * thread running PROC(ROCK).
 */

/*Main Invariant holds; called only from start code */
ILU_PUBLIC      ilu_boolean
ilu_SetForkTech(ilu_ForkProc,	/* proc, IN */
		ILU_ERRS((internal)) *);
/*
 * Every multi-threaded runtime calls this, to supply the
 * implementation of thread forking; no single-threaded runtime
 * calls this.  Call this procedure once at startup time to tell the
 * kernel how to fork a new thread.
 */

/* L1, L2 unconstrained */
ILU_PUBLIC	ilu_boolean
  ilu_KernelThreaded(void);
/* return ilu_TRUE if ilu_Fork can be used to fork a new thread.
 * This can be used by an LSR to see if another LSR has already
 * supplied threading to the kernel.
 */

/* ================ Memory Management ================ */

/*L1, L2 unconstrained*/

ILU_PUBLIC void *ilu_malloc(ilu_cardinal size);
ILU_PUBLIC void *ilu_realloc(void *p, ilu_cardinal size);
ILU_PUBLIC void ilu_free(void *p);
#define ilu_malloc(s)		ilu_full_malloc(s,__FILE__,__LINE__)
#define ilu_realloc(p,s)	ilu_full_realloc(p,s,__FILE__,__LINE__)
#define ilu_free(p)		ilu_full_free(p,__FILE__,__LINE__)
/* The ILU runtime (kernel and LS (if appropriate)) use these procedures
   to manage dynamic memory.  ilu_malloc and ilu_realloc call malloc
   and realloc, respectively.  If the basic procedure (malloc or
   realloc) fails, the ILU version then tries to free up some memory,
   perhaps even calling application-specified procs to free memory,
   and then tries again.  These procedures return NIL if the
   requested amount of memory still can't be allocated. */

#if 0
ILU_PUBLIC void    *
ilu_MallocE(ilu_cardinal size,
	    ILU_ERRS((no_memory)) * err);
ILU_PUBLIC void    *
ilu_ReallocE(void *p, ilu_cardinal size,
	     ILU_ERRS((no_memory)) * err);
#else
#define ilu_MallocE(s,e)	ilu_full_MallocE(s,e,__FILE__,__LINE__)
#define ilu_ReallocE(p,s,e)	ilu_full_ReallocE(p,s,e,__FILE__,__LINE__)
#endif
/*
 * Convenient wrappers around ilu_malloc and ilu_realloc that set
 * *err appropriately for a kernel interface call.
 */

ILU_PUBLIC void    *
ilu_full_malloc(ilu_cardinal size,
		const char *file, int line);
ILU_PUBLIC void    *
ilu_full_realloc(void *p, ilu_cardinal size,
		 const char *file, int line);
ILU_PUBLIC void     ilu_full_free(void *p, const char *file, int line);
ILU_PUBLIC void    *
ilu_full_MallocE(ilu_cardinal size, ILU_ERRS((no_memory)) * err,
		 const char *file, int line);
ILU_PUBLIC void    *
ilu_full_ReallocE(void *p, ilu_cardinal size, ILU_ERRS((no_memory)) * err,
		  const char *file, int line);
/* The actual procedures used by the above macros */

ILU_PUBLIC ilu_string 
  ilu_StrdupE(const char *,
	      ILU_ERRS((no_memory)) * err);
#define ilu_StrdupE(s,e)	ilu_full_StrdupE(s,e,__FILE__,__LINE__)
ILU_PUBLIC ilu_string
  ilu_full_StrdupE(const char * /* str */,
		   ILU_ERRS((no_memory)) * /* err */,
		   const char * /* filename */,
		   int /* lineno */);

ILU_PUBLIC ilu_string 
ilu_Strcat3E(const char * s1, const char * s2,
	     const char * s3, ILU_ERRS((no_memory)) * err);

ILU_PUBLIC ilu_string 
ilu_Strcat5E(const char * s1, const char * s2,
	     const char * s3, const char * s4,
	     const char * s5, ILU_ERRS((no_memory)) * err);

typedef struct {
  /* L1, L2 unconstrained */
  
  char           *icb_base;
  ilu_cardinal    icb_len, icb_size;
}               ilu_CharBuf;
/*
 * A growable string.  icb_base[0]...icb_base[icb_len-1] are
 * significant; icb_base[icb_len] is the terminating null.  (icb_base)
 * points to an array of (icb_size) characters, or may be null if
 * (icb_size) is 0.  */

/* L1, L2 unconstrained */

extern          ilu_boolean
ilu_CharBufAppend(ilu_CharBuf * s1, char *s2, ilu_cardinal s2len,
		  ILU_ERRS((no_memory)) * err);

extern          ilu_boolean
ilu_CharBufReserve(ilu_CharBuf * s1, ilu_cardinal s2len,
		   ILU_ERRS((no_memory)) * err);
/*
 * Ensure (*s1) has enough space to append a string whose length
 * (not counting the terminal NUL) is (s2len).
 */

extern          ilu_CharBuf
ilu_CharBufFromChars(const char *s, ilu_cardinal len,
		     ILU_ERRS((no_memory)) * err);

#define ilu_CharBufFromString(s,err) \
	ilu_CharBufFromChars((s), strlen(s), (err))

#define ilu_CharBufDup(cb,err) \
	ilu_CharBufFromChars((cb).icb_base, (cb).icb_len, (err))

#define ilu_CharBufFree(cb) ilu_free((cb).icb_base)

extern          ilu_boolean
ilu_Append1Cinfo(ilu_CharBuf * cinfo, ilu_string pinfo,
		 ilu_TransportInfo tinfo,
		 ILU_ERRS((no_memory)) * err);

/* More conveniences */

ILU_PUBLIC ilu_boolean ilu_AddFreer(void (*free)(ilu_cardinal size));
/* Apps (and LS runtimes) can call this proc to register ways
   to free up memory.  Returns ilu_FALSE iff unable to register.
   Concurrent calls not allowed, not detected.
   The /size/ argument to /free/ indicates how big a
   request is prompting this freeing. */

#define ilu_must_malloc(s) ilu_full_must_malloc((s),__FILE__,__LINE__)
/*
 * Like ilu_malloc, but returns only if the memory can be allocated;
 * otherwise, something drastic is done to indicate an unrecoverable
 * fault.  This is used only in the error-reporting system; other
 * allocation failures should be reported in the more controlled
 * way.
 */

ILU_PUBLIC void    *
ilu_full_must_malloc(ilu_cardinal size,
		     const char *file, int line);
/* Used to implement the macro above. */

typedef void    (*ilu_FailureConsumer) (const char *file, int line);
/* A procedure that never returns. */

ILU_PUBLIC void     ilu_SetMemFailureAction(int mfa);
/*
 * Calling this tells the runtime which drastic action is to be
 * performed when ilu_must_malloc fails.  -2 means to print an
 * explanatory message on stderr and coredump; -1 means to print and
 * then loop forever; positive numbers mean to print and then
 * exit(mfa); others number reserved. The default is -1.
 */

ILU_PUBLIC void     ilu_SetMemFailureConsumer(ilu_FailureConsumer mfc);
/*
 * An alternative to ilu_SetMemFailureAction: this causes mfc to be
 * called when ilu_must_malloc fails.
 */

/* ================ Internal Consistency Checking ================ */

/*L1, L2 unconstrained*/

/*
 * The first two macros and procedures here are for use only inside the
 * kernel, and elsewhere only in the expansions of the macros in
 * iluerror.h for the kernel exception system.
 */

#define _ilu_Assert(pred,clue) ((pred)?0:(_ilu_FullAssert(pred,clue,__FILE__,__LINE__),0))
/*
 * Code in the kernel calls this at internal consistency checks from
 * which it is *not* prepared to return an error (there will be none
 * of these once the error system is fully deployed).  The first
 * argument should be a C boolean that's true (i.e., an int that's
 * not 0).  The second argument is some string that distinguishes
 * the call point from every other point that calls _ilu_Assert;
 * storage is owned by the caller.  This procedure returns iff t.
 */

ILU_PUBLIC void
_ilu_FullAssert(int t, const char *id,
		const char *file, int line);
/* Used in implementing the above macro. */

#define ilu_Check(pred,err) (((pred) && ILU_CLER(*err)) || ilu_FullCheckFailed((err),__FILE__,__LINE__))
/*
 * Code in the kernel calls this at internal consistency checks from
 * which it *is* prepared to return an error.  An invocation of this
 * macro expands to an expression.  When the check succeeds (pred is
 * a true value), the expression evaluates to a true value and sets
 * *err to indicate success.  When the check fails, the check
 * failure action/consumer is consulted; the macro expansion will
 * either (a) not return, or (b) evaluate to a false value after
 * setting *err to internal/check.
 */

ILU_PUBLIC          ilu_boolean
ilu_FullCheckFailed(ILU_ERRS((internal)) * err,
		    const char *file, int line);
/* Used in implementing the above macro. */


/* The following two procedures are generally available. */

ILU_PUBLIC void     ilu_SetAssertionFailureAction(int afa);
/*
 * Calling this tells the runtime which drastic action is to be
 * performed when a run-time assertion fails.  -2 means to print an
 * explanatory message to stderr and then coredump; -1 means to
 * print and then loop forever; non-negative numbers mean to print
 * and then exit(afa); others number reserved. The default is -1.
 */

ILU_PUBLIC void     ilu_SetAssertionFailConsumer(ilu_FailureConsumer afc);
/*
 * An alternative to ilu_SetAssertionFailureAction: this causes afc
 * to be called (and no printing) when a run-time assertion fails.
 */

typedef void    (*ilu_CheckFailureConsumer) (const char *file, int line);
/*
 * A procedure for handling an internal consistency check failure.
 * If this procedure returns, the consistency check failure will be
 * raised as an error from the kernel.
 */

ILU_PUBLIC void     ilu_SetCheckFailureAction(int cfa);
/*
 * Calling this tells the runtime which action is to be performed
 * when an internal consistency check fails.  -3 means to raise an
 * error from the kernel (without necessarily printing anything); -2
 * means to print an explanatory message to stderr and then
 * coredump; -1 means to print and then loop forever; non-negative
 * numbers mean to print and then exit(afa); others number reserved.
 * The default is -1.
 */

ILU_PUBLIC void     ilu_SetCheckFailureConsumer(ilu_CheckFailureConsumer cfc);
/*
 * An alternative to ilu_SetCheckFailureAction: this causes cfc to
 * be called (and no printing); if cfc returns, an error will be
 * raised from the kernel.
 */

typedef void    (*ilu_RaiseDebugHook) (ilu_ErrorType et,
		                       const char *file, int line);
/*
 * A procedure that's called when an error is being raised in the
 * kernel.  Should return without doing anything (visible to the
 * kernel).  Intended for debugging use by ILU maintainers only.
 */
	       
ILU_PUBLIC void     ilu_SetRaiseDebugHook(ilu_RaiseDebugHook rdh);
/*
 * Call this to make rdh the one called as part of every error raise
 * in the kernel.  A language-specific runtime might call this,
 * passing a procedure that calls into the language in question, so
 * that breakpoints can be set in that language.  Intended for
 * debugging use by ILU maintainers only.
 */

ILU_PUBLIC ilu_cardinal	ilu_DebugLevel;
/*
 * This variable is exported for read-only access.  It should be set
 * via calls on ilu_SetDebugLevel() and ilu_SetDebugLevelViaString().
 * It contains a bitmask describing the various forms of debugging
 * output currently enabled.
 */

/* ================ Adding Protocols and Transports ================ */
/* See iluprotocol.h and ilutransport.h. */


/* ================ Deleting Objects ================ */
/*
 * A kernel object (ilu_Object) is accessible only inside its
 * server's mutex.  That mutex's invariant includes this: either (1)
 * the kernel object and its language-specific object (LSO) point to
 * each other, or (2) neither the kernel object nor the LSO points
 * to the other.  Sadly, this means application-specific code for
 * introducing and finalizing an object must run inside the server's
 * mutex.
 * 
 * The kernel may be "interested" in a kernel object for one of a few
 * reasons:
 * 
 * (1) It is a collectible true object with remote surrogates extant or
 * still possible (the timeout hasn't expired).
 * 
 * (2) It is a true object on which one of the built-in methods is
 * working.
 * 
 * (3) It is a collectible surrogate and the kernel is notifying the true
 * server of the positive existance of the surrogate.
 * 
 * (4) It is a collectible surrogate and the kernel is notifying the true
 * server that the surrogate is being deleted.
 * 
 * (5) It has an associated LSO.
 * 
 * We say the kernel is "very interested" in a kernel object if it is
 * interested for any but the last two of the above reasons.  The
 * kernel keeps the language runtime appraised of whether the kernel
 * is currently very interested in each object.
 * 
 * The language runtime may, at any time, choose to disassociate the
 * kernel object and the LSO.  A language with a garbage collector
 * might choose to keep the object in a collector-visible global
 * data structure during those times when the kernel is very
 * interested in the object, and choose to disassociate the KO and
 * LSO when the LSO is finalized.  A language with or without a
 * garbage collector might choose to give the application the
 * opportunity to explicitly disassociate the KO and LSO.
 * 
 * When the language runtime finds itself holding an LSO with no
 * associated KO, it can choose either to declare the LSO "broken",
 * or try to find or create a KO for it.  In the latter case, a
 * disassociated true LSO will need to be holding the object's OID
 * (ie, server plus server-relative ID); a disassociated surrogate
 * LSO will need to be holding the full SBH of the object.  When the
 * language runtime finds itself holding a KO with no associated LSO
 * it may try to find or create an LSO, or --- if the object is true
 * --- complain that the object is "closed".
 * 
 * As long as the kernel is interested, the object stays in the kernel
 * server's hash table of objects.  When the kernel is not
 * interested in the object, the kernel will un-table, destroy, and
 * free the object.
 * 
 * The LS runtime, stubs, and application --- as well as the kernel ---
 * promise to not hold onto a kernel object (directly --- indirect
 * through an LSO or server is OK, because of the server's
 * invariant) while outside its server's mutex.  Pay close attention
 * to the locking dance done during marshalling and unmarshalling.
 * If an LS runtime promises to never disassociate a certain KO and
 * LSO, that runtime may hold onto that KO outside its server's
 * mutex.  This could be done for the true GC callback object
 * exported by a client of collectible objects.  Similarly, where
 * the kernel has indicated its "interest" in an object, the kernel
 * may hold onto that object outside its server's mutex.
 */


/* ==================================== from debug.c */

/*L1, L2 unconstrained*/

ILU_PUBLIC ilu_cardinal ilu_SetDebugLevel(ilu_cardinal /* bits */);
/* ILU allows debugging messages for a number of internal features to
   be enabled or disabled by setting or clearing bits in the argument
   to ilu_SetDebugLevel().  See iludebug.h for a listing of the
   specific features which can be selected.   Returns the previous
   setting.  */

ILU_PUBLIC ilu_cardinal ilu_SetDebugLevelViaString(char *spec);
/* The features for which debugging messages are to be displayed
   may be specified as a string consisting of colon-separated names,
   as well as via bits, by using ilu_SetDebugLevelViaString() instead
   of ilu_SetDebugLevel().  See iludebug.h for a listing of the
   allowable features.  This can also be set with the environment
   variable ILU_DEBUG.  Returns the previous setting. */

#ifndef va_start
#include <stdarg.h>
#endif

/* L1.sup < debugmu */
ILU_PUBLIC void ilu_DebugPrintf (const char *formatSpec, ...);
/* All debugging messages are displayed via calls on this printf-alike
   routine, or the following triple.  Use this procedure if one call outputs an entire message. */  

/* before: L1.sup < debugmu; after: L1 >= {debugmu} */
ILU_PUBLIC void ilu_DebugPrintfIntro(const char *formatSpec, ...);

/* L1 unconstrained */
ILU_PUBLIC void ilu_DebugPrintfCont(const char *formatSpec, ...);

/* before: L1 >= {debugmu}; after: L1 disjoint {debugmu} */
ILU_PUBLIC void ilu_DebugPrintfFin(const char *formatSpec, ...);
/*
 * ilu_DebugPrintf may print a standard prefix.  If a single message
 * is output in multiple calls, use this triple.  But first, swear
 * upon all you hold sacred to not mess this up.  The first call for
 * a message is upon ilu_DebugPrintfIntro; then call
 * ilu_DebugPrintfCont any number of times; then call
 * ilu_DebugPrintfFin once (NO MATTER WHAT!); then you're done.
 */

/* L1 >= debugmu */
typedef void (*ilu_DebugMessageHandler)
     (const char * /* formatspec */,
      va_list /* parms to output */);

/* L1.sup < debugmu */
ILU_PUBLIC void ilu_SetDebugMessageHandler (ilu_DebugMessageHandler);
/* Sets the output function which ilu_DebugPrintf() uses to display
   the messages printed through it.  By default, ilu_DebugPrintf()
   uses vfprintf(stderr, ...).  Two special values for the argument to
   this function are accepted:  ILU_DEFAULT_DEBUG_MESSAGE_HANDLER, which
   causes the default vfprintf(stderr, ...) message handler to be
   re-instated, and ILU_NIL_DEBUG_MESSAGE_HANDLER, which causes debug
   messages to be discarded. */

#define ILU_DEFAULT_DEBUG_MESSAGE_HANDLER ((void(*)(char *,va_list))1)
#define ILU_NIL_DEBUG_MESSAGE_HANDLER ((void(*)(char *,va_list))0)

/* L1, L2 unconstrained */
typedef void    (*ilu_MessagePrinter) (const char *formatSpec,...);

/* L1, L2 unconstrained */
typedef void    (*ilu_ThreadPrinter) (ilu_MessagePrinter);
/*
 * A procedure that prints the current thread followed by a space,
 * using the given printing procedure.
 */

/* L1.sup < debugmu */
ILU_PUBLIC void ilu_SetThreadPrinter(ilu_ThreadPrinter);
/*
 * Sets the procedure used to preface debugging output lines with
 * the ID of the relevant thread.  ilu_SetThreadPrinter is called by
 * the code that supplies the threading implementation (i.e., the
 * same code that calls ilu_SetForkTech).
 */

/* L1.sup < debugmu */
ILU_PUBLIC void ilu_SendDebugOutputToFile (ilu_string	/* filename */);
/* Sets the debug message handler to write the debugging messages
   to the file named by the argument.  This can also be invoked by
   setting the environment variable ILU_DEBUG_FILE to the filename
   before running the program. */

#ifdef TCPIP_TRANSPORT

/* Main Invariant */
ILU_PUBLIC void
  ilu_tcp_GetStats (ilu_cardinal * /* bytes_sent */,
		    ilu_cardinal * /* bytes_received */,
		    ilu_cardinal * /* moorings_created */,
		    ilu_cardinal * /* connections_received */,
		    ilu_cardinal * /* connections_created */,
		    ilu_cardinal * /* current_connections */,
		    ilu_cardinal * /* max_connections */);
/* Returns statistics about bytes sent, bytes received,
 * moorings created, TCP/IP connections received, TCP/IP connections
 * created, and the maximum number of connections simultaneously
 * open, since the last initialization of the statistics.
 * The number of connections currently open is also returned.
 */

ILU_PUBLIC void
  ilu_tcp_InitializeStats(void);
/* Initializes the statistics counters returned by
 * "ilu_tcp_GetStats" to zero, except for current_connections,
 * which is left unchanged, and max_connections, which is set
 * to current_connections.
 */

#endif /* def TCPIP_TRANSPORT */

/* ================ Time ================ */
/*L1, L2 unconstrained*/

struct ilu_FineTime_s {
  ilu_integer ft_s;	/* seconds since some origin */
  ilu_cardinal ft_t;	/* fraction of a second */
};
/* Represents s + t/N seconds since some origin.  0 <= t < N.
   If ilu_FineTimeRate is 0, N is one greater than the largest
   ilu_cardinal; otherwise, N is ilu_FineTimeRate. */

ILU_PUBLIC const ilu_cardinal ilu_FineTimeRate;

ILU_PUBLIC ilu_FineTime ilu_FineTime_Now(void);

ILU_PUBLIC ilu_integer ilu_CoarseTime_Now(void);
/* Like ilu_FineTime_Now, but returns just the seconds component. */
/* Do we also need a way to get a "very old" or "invalid" time? */

ILU_PUBLIC ilu_FineTime ilu_FineTime_Add(ilu_FineTime a, ilu_FineTime b);

ILU_PUBLIC ilu_FineTime ilu_FineTime_Sub(ilu_FineTime a, ilu_FineTime b);

ILU_PUBLIC ilu_FineTime ilu_FineTime_Mul(ilu_FineTime a, float b);

ILU_PUBLIC ilu_integer ilu_FineTime_Cmp(ilu_FineTime a, ilu_FineTime b);
/* sgn(result) == sgn(a-b) */

#define ilu_FineTime_Eq(a, b) (((a).ft_s==(b).ft_s) && ((a).ft_t==(b).ft_t))

ILU_PUBLIC ilu_cardinal ilu_rescale(ilu_cardinal n, ilu_cardinal dfrom,
					 ilu_cardinal dto);
/* Returns floor(X(dto)*n/X(dfrom)), where
   X(c) = (double) (one more than the biggest ilu_cardinal) if c==0,
   X(c) = (double) c					    if c!=0.
   Caller guarantees 0 <= n < X(dfrom).*/

ILU_PUBLIC ilu_FineTime ilu_FineTime_FromDouble(double seconds);


/* ================ FD & Connection Management ================ */
/*
 * Because we may open multiple connections to a server, we need
 * some policy for when to close them.  That policy is this: the
 * application gives the ILU kernel a "File Descriptor Budget".  The
 * ILU kernel promises to use no more than this many File
 * Descriptors at once.  [How much sense does this make for the
 * Macintosh?  Other non-UNIX-like OSes?]  Off the top of this
 * budget we take FDs needed for serving (one per listening socket
 * and one per accept).  The remainder is allocated to outgoing
 * connections (over transports that use FDs --- ie, not inmemory).
 * When we want to consume a new FD, and there's no room left in the
 * budget, we go looking for an idle outgoing connection (one with
 * no outstanding calls) to close.  All idle outgoing connections
 * are kept in a doubly-linked list, ordered by when the connection
 * went idle (most recently at the front).
 */

/*L1.sup < cmu; L2 unconstrained*/
ILU_PUBLIC ilu_cardinal ilu_GetFDBudget(void);

/*Main Invariant holds; L2 otherwise unconstrained*/
ILU_PUBLIC ilu_cardinal ilu_SetFDBudget(ilu_cardinal n);
/* Sets the FD budget to n, if possible.  This is impossible when n is
smaller than the previous budget, and the kernel can't close enough
idle connections to reach n; in this case the kernel sets the budget
as low as it can.  It is also impossible if n is higher than an
operating system resource limit, in which case the resource limit is
substituted for n.  In all cases the new budget is returned. */


/* ================ Character Sets ====================== */

#define ILU_StringEncoding_US_ASCII		3	/* IANA "ANSI_X3.4-1968" */
/* US-ASCII, as defined in RFC 1345 */

#define ILU_StringEncoding_latin1		4	/* IANA "ISO_8859-1:1987" */
/* ISO Latin 1, as defined in RFC 1345 */

#define ILU_StringEncoding_Unicode_UCS_2	1000	/* IANA "ISO-10646-UCS-2" */
/* 2-byte wide subset of Unicode.  Note that this is defined in terms of
   16-bit integers; no byte-ordering is supposed or prescribed. */

#define ILU_StringEncoding_Unicode_UCS_4	1001	/* IANA "ISO-10646-UCS-4" */
/* 4-byte wide Unicode.  Note that this is defined in terms of
   31-bit integers; no byte-ordering is supposed or prescribed. */

#define ILU_StringEncoding_Unicode_1_1		1010	/* IANA "UNICODE-1-1" */
/* 2-byte wide encoding of Unicode UCS-2, as defined in RFC 1641.
 * Note that this encoding is defined to be big-endian.
 */

#define ILU_StringEncoding_UTF_8		106	/* IANA "UTF-8" */
/* UTF-8 encoding of Unicode UCS-2, as defined in RFC 2044 */

/* ================ Client side routines ================ */

/*L1, L2 unconstrained*/
ILU_PUBLIC void ilu_SetGcClient(ilu_Object interest);
/*
 * A client of GCed objects calls this --- once, before
 * RegisterLSO on any GCed object.
 */

/*L1, L2 unconstrained*/
ILU_PUBLIC ilu_boolean ilu_IsGcClientSet(void);
/*
 * returns true if a gc client has already been set - 
 * useful to know if there are multiple runtimes involved
 * with more than one using collectible objects
 */

/*Main Invariant holds; L2 otherwise unconstrained*/

/*before: L1 = {};
  after:  result!=NIL => Inside(result's server, static_type);
  after:  result==NIL => L1 = {};
  Main Remnant holds*/
ILU_PUBLIC          ilu_Object
ilu_ObjectOfSBH(ilu_string sbh,
		ilu_Class static_type,
		ILU_ERRS((bad_locks, broken_locks, inv_objref,
			  no_memory, internal)) * err);
/*
 * Importing an object begins with calling this procedure, which
 * returns the kernel representation of an object (which may be true
 * or a surrogate).  mstid is the unique_id of the true object; in
 * general, it's a subtype of static_type; mstid may be NIL, which
 * means we don't know (and will never find out!) any type more
 * specific than the type statically associated with "this
 * position".  Storage of sbh and mstid is owned by the caller.
 * Neither sbh nor static_type may be NIL.  If result!=NIL &&
 * ilu_GetLanguageSpecificObject(result)==NIL, the caller must
 * invoke ilu_RegisterLSO or ilu_DeltaHolds on the result before
 * unlocking the server.
 */

/* before: L1 = {};
   after:  result == NIL => L1 = {}
           result != NIL => Inside(object_server(obj), ilu_rootClass)
 */
ILU_PUBLIC     ilu_Object 
ilu_FindObject (ilu_string /* sid, RETAIN */, ilu_string /* ih, RETAIN */);
/* Find and return specified object, or NIL
   if object doesn't exist or server doesn't exist */

/*L1 >= {obj's server}; L2 unconstrained*/
ILU_PUBLIC      ilu_refany
ilu_GetLanguageSpecificObject(ilu_Object obj,
			      ilu_LanguageIndex language);
/* Returns the language-specific object, if any, associated with
 * the given kernel object and the given language. */

/*Inside(obj's server, static_type)*/
ILU_PUBLIC ilu_boolean
  ilu_RegisterLSO(ilu_Object,		/* obj */
		  ilu_Class,		/* static_type */
		  ilu_refany,		/* lso */
		  ilu_LanguageIndex,	/* language */
		  ILU_ERRS((IoErrs, broken_locks, GcRegFailed)) *);
/*
 * Makes a link from the given kernel object to the given
 * language-specific object; removes such a link if given lso==NIL.
 * This clues the kernel into whether the application is using the
 * object.  If lso==NIL, this may provoke the kernel to destroy and
 * free the kernel object; see "Deleting Objects" above.  (obj) has
 * (static_type), among possibly others; the (static_type) parameter
 * is used only to characterize the locking condition.  L1 mutexes
 * are exited and re-entered inside this procedure!
 */

/*Inside(obj's server, t)*/
ILU_PUBLIC void ilu_SetLSO(ilu_Object obj, ilu_Class t, ilu_refany lso,
		       ilu_LanguageIndex language);
/*
 * Like ilu_RegisterLSO, but can't raise errors (internal
 * consistency checks fail instead).  Deprecated, for that reason.
 */

/*Inside(obj's server, obj's type)*/
ILU_PUBLIC void 
ilu_RegisterLanguageSpecificObject(ilu_Object obj,
				   ilu_refany lso,
				   ilu_LanguageIndex language);
/*
 * Like ilu_SetLSO, but with (t) fixed at (obj)'s most specific
 * type.  Deprecated, like ilu_SetLSO.
 */

/*L1 >= {s}; L2 unconstrained*/
ILU_PUBLIC      ilu_refany
ilu_GetLSS(ilu_Server s, ilu_LanguageIndex language);

/*L1 >= {s}; L2 unconstrained*/
ILU_PUBLIC      ilu_boolean
ilu_SetLSS(ilu_Server s, ilu_refany lss,
	   ilu_LanguageIndex language,
	   ILU_ERRS((bad_param, internal)) * err);
/*
 * Makes a link from the given kernel server to the given
 * Language-Specific Server; removes such a link if (lss==NIL). If
 * (lss==NIL), this may provoke the kernel to destroy and free the
 * kernel server.
 */

/*L1 >= {s}; L2 unconstrained*/
ILU_PUBLIC      ilu_boolean
ilu_DeltaServerHolds(ilu_Server s, int dholds,
		     ILU_ERRS((bad_locks, internal/broken)) * err);

/*L1 >= {ilu_Server}; L2 unconstrained*/
typedef void (*ilu_ServerRelocateProc) (ilu_Server,	/* the server */
					ilu_private,	/* "rock" for call */
					ILU_ERRS((relocate, internal)) *);
/* This proc should raise a "relocate" error if it wishes the client
 * to rebind to a different cinfo for this server.  Otherwise, it should
 * signal success via the ilu_Error parameter.
 */

/*L1 >= {ilu_Server}; L2 unconstrained*/
ILU_PUBLIC	ilu_private
ilu_SetServerRelocateProc (ilu_Server,			/* server */
			   ilu_ServerRelocateProc,	/* proc */
			   ilu_private,			/* rock */
			   ILU_ERRS((no_memory, internal)) *);

/* This proc can be called from the kernel during pr_read_header or
 * pr_interpret_request to see if the actual server should be
 * somewhere else.  If so, an appropriate "transient" error should be
 * signalled.  This is provided to allow for load-balancing and
 * inetd-like servers.  The server has only one relocate proc at
 * a time (or none).  If successful, the previous rock argument
 * is returned; this mechanism can be used when explicitly closing
 * servers, to deallocate the storage used by the rock.
 */

/*Inside(obj's server, obj's type)*/
ILU_PUBLIC 
ILU_ERRS((GcRegFailed, bad_locks, broken_locks, internal))
ilu_DeltaHolds(ilu_Object obj, ilu_integer dholds);
/*
 * The holds on an ilu_Object count the number of places the object
 * is being used outside its server's mutex.  A LS runtime will
 * need to have a hold while calling ilu_PingObject, for example.
 * Pass +1 to add a hold, then -1 to remove it.  When the count goes
 * to 0, the kernel may destroy and free the kernel object; see
 * "Deleting Objects" above. L1 mutexes are exited and re-entered
 * inside this procedure!
 */

/*Inside(obj's server, obj's type)*/
ILU_PUBLIC void
ilu_DHolds(ilu_Object obj, ilu_integer dholds);
/*
 * Like ilu_DeltaHolds, but doesn't chase down consequences (and
 * doesn't exit L1 mutexes).  ilu_DeltaHolds or ilu_RegisterLSO must
 * later be called before exiting server mutex.
 */

typedef struct _ilu_Serializer_s *ilu_Serializer;
/*
 * An ilu_Serializer represents an instance of the call order
 * preservation (A.K.A. serialization) guarantee.  An instance is
 * with respect to a particular server and set of calls.  The
 * server's default port must use a non-concurrent protocol; if not,
 * the client's call on ilu_FullStartCall will raise
 * inv_objref/conc_serial (if no other error is noticed first).  The
 * guarantee is that the server application code receives calls in
 * the same order as the client application code makes them, except
 * that calls that return after a barrier call may have started
 * service before calls that return before the same barrier call.  A
 * barrier call is one that raises the barrier error.  Remember that
 * ASYNCHRONOUS calls do return, they just do so particularly
 * quickly.  Two calls are considered to have been issued
 * concurrently if each one's ilu_FullStartCall is initiated before
 * the other's ilu_FinishCall returns.  In a multi-threaded runtime,
 * they client may issue concurrent calls with the same
 * ilu_Serializer, and the ILU runtime kernel will put them in some
 * serial order.  Note that for two concurrently issued calls,
 * either: (a) the one put first is ASYNCHRONOUS, (b) they both are
 * in the same ilu_Pipeline, or (c) the one put second is delayed
 * until the one put first returns.  In a single-threaded runtime,
 * the client may issue two calls "concurrently", but both will
 * execute successfully only if the client is lucky; otherwise,
 * ilu_FullStartCall will raise bad_param/serialConcurrent for one
 * of the calls.
 */

/* Main Invariant holds */
ILU_PUBLIC      ilu_Serializer
ilu_GetSerializer(ilu_Server server,
		  ILU_ERRS((no_memory, no_resources, bad_locks,
			    broken_locks, bad_param)) * err);
/*
 * Return a new instance of the serialization guarantee, with
 * respect to the given server.
 */

/* L1 = {server}; Main Remnant holds */
ILU_PUBLIC      ilu_Serializer
ilu_InnerGetSerializer(ilu_Server server,
		       ILU_ERRS((no_memory, no_resources, bad_locks,
				 broken_locks, bad_param)) * err);
/*
 * Like ilu_GetSerializer, but with different locking.
 */

/* Main Invariant holds */
ILU_PUBLIC      ilu_boolean
ilu_ReleaseSerializer(ilu_Serializer,
		      ILU_ERRS((bad_locks, broken_locks,
				bad_param)) *);
/*
 * Client calls this after last pass of given ilu_Serializer to
 * ilu_StartCall.  Not necessarily before ilu_FinishCall of all
 * corresponding calls.  Henceforth, client cannot use the given
 * ilu_Serializer for anything (i.e., it may be freed at any time
 * from here on).
 */

typedef struct _ilu_Pipeline_s *ilu_Pipeline;
/*
 * A client uses an ilu_Pipeline to let the kernel know it can
 * safely pipeline certain calls down a serial (i.e.,
 * non-concurrent) ilu_Connection.  The ilu_Pipeline is passed to
 * ilu_StartCall in each of the calls that can be pipelined.  A
 * given serial connection can have multiple calls outstanding only
 * if they are all associated with the same (non-NIL) ilu_Pipeline
 * (remember that absent pipelining, ILU will do concurrent calls
 * over a serial protocol by opening multiple connections).  Multiple
 * connections, even of different servers, can have outstanding
 * calls associated with the same ilu_Pipeline.
 */

/* Main Invariant holds */
ILU_PUBLIC ilu_Pipeline 
ilu_GetPipeline(ILU_ERRS((no_memory, no_resources, bad_locks,
			  broken_locks, bad_param)) * err);
/*
 * Return a new ilu_Pipeline.
 */

/* Main Invariant holds */
ILU_PUBLIC      ilu_boolean
ilu_ReleasePipeline(ilu_Pipeline,
		    ILU_ERRS((bad_locks, broken_locks,
			      bad_param)) *);
/*
 * Client calls this after last pass of the given ilu_Pipeline to
 * ilu_StartCall.  Not necessarily before ilu_FinishCall of all
 * corresponding calls.  Henceforth, client cannot use the given
 * ilu_Pipeline for anything (i.e., it may be freed at any time from
 * here on).
 */

#ifdef BATCHING_TRANSPORT
/* 3/24/98: DEPRECATED */

/* L2 < {serializer_server(si)}; Main Invariant holds otherwise */
ILU_PUBLIC	ilu_boolean
  ilu_batching_Flush(ilu_Serializer,	/* serializer associated with server */
		     ILU_ERRS((bad_locks, broken_locks,
			       bad_param)) *);
/* If the specified serializer is associated with a connection
 * that has a batching transport filter as its top transport
 * layer, this call will flush any pending output on that
 * layer.  The flush only affects output in the concealed buffer
 * of the transport; output in the exposed buffer may not be
 * flushed.  Raises "bad_param" if called on a serializer not
 * associated with connection where batching is the top layer.
 */
#endif /* BATCHING_TRANSPORT */

typedef struct ilu_Batcher_struct *ilu_Batcher;
/*
 * An ilu_Batcher represents a batching scope.  Calls made within a
 * batching scope are batched by the ILU runtime (for the exact
 * definition of batching, see the behavioral prescriptions in the
 * comments on ilu_FullFinishRequest and ilu_PushBatcher).  The
 * `push' operation affects all the calls in a given batching scope
 * (hence its name).
 */

/* Main Invariant holds; L2 otherwise unconstrained */
ILU_PUBLIC      ilu_Batcher
ilu_CreateBatcher(ilu_FineTime timeout,
		  ilu_boolean pushable,
		  ILU_ERRS((internal, no_memory, no_resources,
			    imp_limit, bad_locks, broken_locks)) * err);
/*
 * Create a new batching scope.  (timeout > 0) or (pushable) should
 * be true.
 */


/* Main Invariant holds; L2 otherwise unconstrained. */
ILU_PUBLIC      ilu_boolean
ilu_PushBatcher(ilu_Batcher b,
		ILU_ERRS((IoErrs, bad_locks)) * err);
/*
 * Ensure that the requests of all calls associated with (b) since
 * the last call on ilu_PushBatcher(b) will eventually be completely
 * transmitted to their servers (barring various failures).  (b)
 * must have been created with (pushable != ilu_FALSE).  There is no
 * guarantee of how much progress the transmissions have made by the
 * time this procedure returns.  While (b) is un-pushable or has not
 * been associated with any calls since the last call of
 * ilu_PushBatcher(b), (b) consumes few resources (only a little
 * memory).
 */

/* Main Invariant holds */
ILU_PUBLIC      ilu_boolean
ilu_ReleaseBatcher(ilu_Batcher,
		   ILU_ERRS((bad_locks, broken_locks,
			     bad_param)) *);
/*
 * Client calls this after last pass of the given ilu_Batcher to
 * ilu_PushBatcher and ilu_FullFinishRequest.  Not necessarily
 * before ilu_FinishCall of all corresponding calls.  Henceforth,
 * client cannot use the given ilu_Batcher for anything (i.e., it
 * may be freed at any time from here on).
 */

/**Main Invariant holds;
   after: success => Call-Locking(call, OHi)*/

ILU_PUBLIC      ilu_boolean
ilu_FullStartCall(ilu_Call_s * call, ilu_Server server,
		  ilu_Class intro_type, ilu_Method method,
		  ilu_LanguageIndex caller_language,
		  ilu_Passport pp,
		  ilu_Serializer si,	/* OPTIONAL */
		  ilu_Pipeline pl,	/* OPTIONAL */
		  ilu_Connection * new_conn,
		  ILU_ERRS((IoErrs, barrier, bad_locks,
			    inv_objref, no_resources)) * err);
/*
 * Client stub calls this to initiate a call.  (call) is a pointer
 * to uninitialized memory owned by the stub.  (intro_type) is the
 * object type that introduced the method, and (method) is the
 * representation in that object type.  (si) may be NIL, indicating
 * that this call need not be serialized with respect to any other
 * call; otherwise, the server must have contact info that uses a
 * non-concurrent ilu_Protocol.  (pl) may be NIL, indicating that
 * this call is not part of any pipeline.  (new_conn) is an OUT
 * parameter, through which ilu_StartCall will pass either NIL or a
 * new outgoing connection that must be monitored with
 * ilu_OutgoingConnectionThreadProc. New connections will only be
 * returned in a multi-threaded runtime; in a single-threaded one,
 * the kernel can monitor the outgoing connections without any help
 * from the LSR.  On success, initializes *call and returns ilu_TRUE;
 * ilu_FinishCall must eventually be called.  Otherwise, returns
 * ilu_FALSE and ilu_FinishCall should not be called.  A new connection
 * may be or not be returned through (new_conn) independently of
 * result and (*err).
 */

ILU_PUBLIC      ilu_boolean
ilu_StartCall(ilu_Call_s * call, ilu_Server server,
	      ilu_Class intro_type, ilu_Method method,
	      ilu_LanguageIndex caller_language,
	      ilu_Passport pp,
	      ilu_Connection * new_conn,
	      ILU_ERRS((IoErrs, barrier, bad_locks,
			inv_objref, no_resources)) * err);
/*
 * ilu_FullStartCall with (si==NIL && pl==NIL).
 */

/*Main Invariant holds; L2 disjoint connection's mutexes*/

ILU_PUBLIC      ilu_Connection
                ilu_OtherNewConnection(ILU_ERRS((internal)) * err);
/*
 * The kernel also produces new outgoing connections in
 * circumstances from which it is difficult to return the new
 * connection to the LSR.  To get these, a multi-threaded LSR forks,
 * at startup time, a special thread that repeatedly calls
 * ilu_OtherNewConnection, and forks a thread to run
 * ilu_OutgoingConnectionThreadProc for each returned connection.
 * The special thread must be forked AFTER the call on
 * ilu_SetLockTech.
 */

ILU_PUBLIC      ilu_boolean
ilu_OutgoingConnectionThreadProc(ilu_Connection conn,
				 ILU_ERRS((IoErrs)) * err);
/*
 * In a multi-threaded LSR, for each ilu_Connection produced by
 * ilu_StartCall or ilu_OtherNewConnection, a thread is forked to
 * call this procedure, which normally returns when the connection
 * is closed.  Ownership of the ilu_Connection is passed from caller
 * to this proc.  That means caller can't do ANYTHING with the
 * ilu_Connection after (or concurrently with) calling this proc.
 */

/*Main Invariant holds; L2 not further constrained*/

ILU_PUBLIC      ilu_boolean
                ilu_NewConnectionGetterForked(ILU_ERRS((internal)) * err);
/*
 * A multi-threaded runtime calls this at startup time, after
 * forking the thread that will call ilu_OtherNewConnection.
 */

typedef enum {
  ilucsr_err,			/* see *err */
  ilucsr_notReified,		/* sbh identifies a non-reified
				 * server */
  ilucsr_noProblem,		/* no problem has been detected
				 * with the server's current
				 * contact info */
  ilucsr_isTrue,		/* the server is true for some language */
  ilucsr_noNews,		/* sbh doesn't contain new contact
				 * info */
  ilucsr_changed		/* the identified surrogate server
				 * has been switched to the contact
				 * info in sbh */
}               ilu_ConsiderSbhResult;

/* L1.sup < cmu; L2 unconstrained */
ILU_PUBLIC          ilu_ConsiderSbhResult
ilu_ConsiderSBH(ilu_string sbh,
		ILU_ERRS((BadProtocolInfo,
			  no_memory, inv_objref, internal)) * err);
/*
 * If ilu_StartCall raised inv_objref, it might be because the
 * contact info held for the server is no longer valid.  If you
 * think sbh might hold newer, valid contact info, call this
 * procedure.
 */

/* Call-Locking(call, OHi) */

ILU_PUBLIC          ilu_boolean
ilu_StartRequest(ilu_Call call, ilu_cardinal argSize,
		 ILU_ERRS((IoErrs)) * err);
/*
 * Client calls this to introduce the arguments.  The size includes
 * that of the discriminator, if any.  Returns ilu_FALSE iff raising an
 * error, in which case the call is over: the LSR next calls
 * ilu_FinishCall, then raises a language-specific exception.
 * Otherwise, the arguments are mashalled next, using the
 * marshalling routines introduced later.
 */

ILU_PUBLIC      ilu_boolean
ilu_FullFinishRequest(ilu_Call call,
		      ilu_Batcher b,	/* OPTIONAL */
		      ILU_ERRS((IoErrs)) * err);
/*
 * End bracket for arguments.  If (b) is NIL, complete transmission
 * of the request to the server is immediately initiated (although
 * no necessarily completed) by the time ilu_FullFinishRequest
 * returns; if (b) is non-NIL and has a non-zero timeout, complete
 * transmission of the request to the server will by initiated
 * within (b)'s timeout; otherwise, ilu_PushBatcher(b) must be called
 * to guarantee complete transmission.  If the call is of an
 * ASYNCHRONOUS method, the client proceeds to ilu_FinishCall.
 * Otherwise, the client continues with ilu_GetReply.  Returns ilu_FALSE
 * iff raising an error, in which case the call is over.
 */

ILU_PUBLIC          ilu_boolean
ilu_FinishRequest(ilu_Call call,
		  ILU_ERRS((IoErrs)) * err);
/*
 * ilu_FullFinishRequest with (b == NIL).
 */

typedef enum ilu_ProtocolExceptions {
  ilu_ProtocolException_Success = 0,
  ilu_ProtocolException_NoSuchClassAtServer = 1,
  ilu_ProtocolException_ClassVersionMismatch = 2,
  ilu_ProtocolException_NoSuchMethodOnClass = 3,
  ilu_ProtocolException_GarbageArguments = 4,
  ilu_ProtocolException_Unknown = 5,
  ilu_ProtocolException_LostConnection = 6,
  ilu_ProtocolException_RequestRejected = 7,
  ilu_ProtocolException_RequestTimeout = 8,
  ilu_ProtocolException_Not = 9	/* non-protocol failure; see *err */
} ilu_ProtocolException;

/*L1, L2 unconstrained*/
#define ilu_PEName(pe) \
(((pe) <= ilu_ProtocolException_Not) \
 ? ilu_PENames[pe] \
 : "(invalid ProtoExn!)")

/*L1, L2 unconstrained*/
ILU_PUBLIC const char *const ilu_PENames[ilu_ProtocolException_Not + 1];

typedef enum _ilu_ConnShutdownReasons {
  ilu_ConnShutdownReason_LostProtocolSync,
  /* don't know how to interpret bytes coming across, or unrecoverable error encountered */

  ilu_ConnShutdownReason_ProcessTermination,
  /* process or agent no longer active */

  ilu_ConnShutdownReason_ResourceManagement,
  /* need to re-use the FD or buffer space for something else */

  ilu_ConnShutdownReason_BadEndpointID,
  /* have reason to think the other end is misinformed about our identity */

  ilu_ConnShutdownReason_MaxSerialNumber,
  /* the max request serial number has been used */

  ilu_ConnShutdownReason_Relocating,
  /* doing relocate; connecting to somewhere else */

  ilu_ConnShutdownReason_ReceivedEOF
  /* received EOF during read on the connection */
} ilu_ConnShutdownReason;

typedef enum {
  ILU_COMPLETED_YES, ILU_COMPLETED_NO, ILU_COMPLETED_MAYBE
} ilu_Completion;

/* Locking unconstrained */
ILU_PUBLIC void ilu_MapProtocolExceptionToError (ilu_ProtocolException,
						 ilu_Error *,
						 ilu_Completion * /* OPTIONAL */);
/* Maps protocol error in first parameter to an ilu_Error, using the pointer to
   an ilu_Error structure that is passed as the second parameter.  For use by
   surrogate sides of LSR, to map ProtocolExceptions received across the wire to
   appropriate ilu_Error values.  If ilu_Completion is given, will return in it
   an appropriate completion value.  */

/* L1, L2 unconstrained */
ILU_PUBLIC const char *ilu_CompletionNames[3];

/**Before: Call-Locking(call, OHi);
    After: Call-Invariant(call, err) &&
	   (success => Call-Locking(call, IHi)) &&
	   (transient/retry => Call-Locking(call, OHi))*/

ILU_PUBLIC      ilu_ProtocolException
ilu_FullGetReply(ilu_Call call, ilu_cardinal * errorStatus,
		 ilu_Completion * completion,
		 ilu_Connection * new_conn,
		 ILU_ERRS((bad_locks, IoErrs,
			   transient/retry)) * err);
/*
 * The client calls this to wait for the reply message and begin
 * processing it.  The result is either success, a protocol-level
 * error, a signal to retry, or other kernel error.  The retry
 * signal is the kernel error `transient', with the minor code
 * ilu_tm_retry; when this is raised, the caller loop back to the
 * step of computing the request size.  If a protocol error or other
 * kernel error is being reported, caller next calls ilu_FinishCall.
 * In the success case, ilu_GetReply also decodes whether the
 * marshalled results are normal results or an exception parameter;
 * `errorStatus` gets 0 if normal results are being returned,
 * otherwise 1 + (index into method's exception vector). The client
 * next calls the appropriate unmarshalling routine, then
 * ilu_ReplyRead, then ilu_FinishCall, and finally returns to the
 * client code.
 */

ILU_PUBLIC      ilu_ProtocolException
ilu_GetReply(ilu_Call call, ilu_cardinal * errorStatus,
	     ilu_Connection * new_conn,
	     ILU_ERRS((bad_locks, IoErrs,
		       transient / retry)) * err);
/*
 * Obsolete.  Like ilu_FullGetReply, but doesn't return the
 * completion information.
 */

/*Call-Locking(call, IHi)*/

ILU_PUBLIC          ilu_boolean
ilu_ReplyRead(ilu_Call call,
	      ILU_ERRS((IoErrs)) * err);
/*
 * A client stub that handles a reply in any way calls this after
 * succesfully unmarshalling any results or exception parameter,
 * before calling ilu_FinishCall().  Next call is ilu_FinishCall,
 * regardless of success or failure.
 */

/**Before: Call-Invariant(call, err).
   After:  Main Invariant holds,
	   (L2 disjoint {call's conn's callmu, iomu}), and
	   (L2 disjoint {call's conn's waitmu} iff disjoint when
	       call initialized (by ilu_StartCall or ilu_ReceiveRequest)),
	   if possible (*err indicates bad_locks or broken_locks
	   when not possible).*/
ILU_PUBLIC void
ilu_FinishCall(ilu_Call call,
	       ILU_ERRS((bad_locks, IoErrs)) * err);
/*
 * This is the last procedure called for an ilu_Call, on both the
 * client and server sides.  After this procedure returns, (call)
 * cannot be re-used until after it's re-initialized.  This
 * procedure does not free the ilu_Call, because the caller owns it.
 * The ilu_Error is an INOUT parameter; it's a comm_failure upon
 * entry when the connection needs to be closed. On the client side,
 * upon exit it is the error to raise from the stub.  On the server
 * side, ILU_ERRNOK(*err) upon return indicates an error not
 * returned across the wire.
 */

/*L1, L2 unconstrained*/

ILU_PUBLIC ilu_Exception 
ilu_ExceptionOfMethod(ilu_Method method,
		      ilu_cardinal index);
/* This maps the exception index into a method-independent
 * (indeed, even object-type-independent) representation for
 * the exception.  This is useful for writing one exception
 * unmarshalling routine to share among all the methods of
 * an object type.  The index is 1+ the subscript into the
 * method's exceptionVector. */

/* Main holds; L2 not further constrained */
ILU_PUBLIC      ilu_boolean
ilu_InterruptCall(ilu_Call call,
		  ILU_ERRS((bad_locks, broken_locks,
			    bad_param)) * err);
/*
 * In a single-threaded runtime, the LSR makes this available to the
 * application, to call if and when it is tired of waiting for a
 * call to return.  Do not use in a multi-threaded runtime; instead
 * use runtime-specific methods to interrupt a wait on I/O or CV.
 */

/*
 * The following procedures are applicable to both true and
 * surrogate servers.
 */

/* L1 < cmu */
ILU_PUBLIC void ilu_BankServer(ilu_Server s);
/*
 * Begin shutting down the given server.  Henceforth no more
 * objects may be added to the server (for a surrogate server, this
 * means unmarshalling currently unknown surrogates will fail).
 * Closes all the server's ports (if it's a true server), and each
 * open connection as soon as its I/O mutex is not held. An
 * application or LS runtime can free damn near all a server's
 * resources by calling this procedure and then unlinking every
 * kernel and LS object in the server.
 */

typedef int     (*ilu_objectCallback) (ilu_Object /* obj */ ,
			                   ilu_refany /* rock */ );

/* L1 >= {s} */
ILU_PUBLIC int
ilu_ScanServerObjs(ilu_Server /* s */ ,
		   ilu_objectCallback /* cb */ ,
		   ilu_refany /* rock */ );
/*
 * Calls cb(obj, rock) for each obj currently extant in server s,
 * unless and until cb returns a non-zero value, at which point
 * that value is returned.  0 is returned iff cb never returns a
 * non-zero value.  cb is called under the same mutexes as
 * ilu_ScanServerObjs.  cb can release and re-acquire mutexes; cb
 * should return under the same mutexes as called.  Objects added
 * or removed during the enumeration might or might not be
 * enumerated. If an object is removed, others might be enumerated
 * twice.  An enumerated object is in the server when enumerated.
 */

/* L1 < gcmu */
ILU_PUBLIC int
ilu_BankAndScanServer(ilu_Server s,
		      ilu_objectCallback cb,
		      ilu_refany rock,
		      ilu_cardinal * nconns);
/*
 * Bank the server, then (if nconns != NIL) *nconns =
 * ilu_NumIoingConnsOfServer(s), and then enumerate its objects (as
 * in ilu_ScanServerObjs) while Inside(s, ilu_rootClass).  E.g., cb
 * could disassociate the given object and its LSO (if any).
 */

/* L1 >= {cmu, s} */
ILU_PUBLIC void ilu_InnerBankServer(ilu_Server s);
/*
 * Like BankServer, but for calling within the specified mutexes.
 */

/* L1 >= {cmu, s} */
ILU_PUBLIC void ilu_PreBankServer(ilu_Server s);
/*
 * Just like InnerBankServer.  Used to be similar, but require fewer
 * mutexes and free fewer resources.  Used to require BankServer or
 * InnerBankServer to be called later.
 */

/* L1 >= {s} */
ILU_PUBLIC ilu_cardinal ilu_NumObjsInServer(ilu_Server s);
/*
 * Returns the number of objects currently reified in the given
 * server.  Objects are counted regardless of whether they
 * currently have an associated LSO.
 */

/* L1 >= {s} */
ILU_PUBLIC ilu_cardinal ilu_NumIoingConnsOfServer(ilu_Server s);
/*
 * Returns the number of connections of the given server whose I/O
 * mutex is held.
 */


/* ================ Language Registry ================ */

/* L2 unconstrained;
   L1 unconstrained at present
   (Threaded runtimes may require language table mutex?). */
ILU_PUBLIC ilu_LanguageIndex ilu_RegisterLanguage(ilu_string name);
/*
 * Pass in name of language and get an index.  Wherever the kernel
 * wants to store/compare a language, it will store/compare the index
 * of the language.  Every language runtime must register its presence
 * at least once, ideally before doing anything else.  Reregistering a
 * name will not change the table (behaves as a lookup.)
 * name will never be freed.
 *
 * The indices returned by ilu_RegisterLanguage are contiguous and
 * begin at a low number.  It is reasonable to use them as indices
 * into an array of language-specific things.
 */
     
/* ================ Object Type Registry ================ */

/*L1, L2 unconstrained*/

ILU_PUBLIC const ilu_Class ilu_rootClass;
/*
 * Every object type is implicitly a subtype of this one; this
 * relation is not explicitly mentioned in the supertype list.
 */

/*L1 >= {otmu}*/

ILU_PUBLIC          ilu_Class
ilu_DefineObjectType(ilu_string cl_name,
		     ilu_string cl_brand,
		     ilu_string cl_unique_id,
		     ilu_string cl_singleton,
		     ilu_boolean cl_optional,
		     ilu_boolean cl_collectible,
		     ilu_string cl_doc_string,
		     ilu_cardinal cl_method_count,
		     ilu_cardinal cl_scls_count,
		     ilu_string cl_scls_ids[],
#ifdef ILU_HTTPNG_OBJECTS
		     ilu_cardinal cl_nstate_fields,
		     ilu_boolean cl_local,
		     ilu_boolean cl_sealed,
#endif		     
		     ILU_ERRS((internal, no_memory)) *err);
/*
 * The following sequence brings the kernel and a stub to a mutual
 * understanding of an object type.  Note that this must work when
 * there are multiple stubs (in different languages) that know about
 * the same object type.  First, the stub enters the Object Type
 * Mutex (otmu).  Then it calls this procedure, which either adds a
 * (partially constructed) new object type to the kernel or checks
 * the arguments against an object type already known to the kernel.
 * Then the stub makes a similar call for each method, and each
 * exception of each method.  This completes the construction of the
 * new object type, if it wasn't already known.  Finally, the stub
 * calls ilu_ObjectTypeDefined, and then exits the Object Type Mutex
 * (otmu).   Caller owns string arguments, and the array thereof;
 * result will never be freed.  Note that (cl_scls_ids[]) holds
 * object type UIDs, not names.
 */

ILU_PUBLIC          ilu_Exception
ilu_DefineException(char *,	/* i, RETAIN */
		    char *,	/* e, RETAIN */
		    char *,	/* OPTIONAL, value type ID, RETAIN */
		    ILU_ERRS((internal, no_memory)) * err);
/*
 * Returns the representation of an exception.  When i != NIL, args
 * are interface and exception names; when i==NIL, e is the CORBA
 * GIOP repository ID representation.  Caller owns args; result is
 * never freed.  The value type ID is either NIL, if the exception
 * has no associated value, or the type ID of the associated value's
 * type.
 */

ILU_PUBLIC ilu_Method 
ilu_DefineMethod(ilu_Class c,
		 ilu_cardinal i,
		 ilu_string me_name,
		 ilu_cardinal me_id,
		 ilu_boolean me_cacheable,
		 ilu_boolean me_asynchronous,
		 ilu_cardinal me_exceptionCount,
		 ilu_Exception *me_exceptionVector,
		 ilu_cardinal,		/* number of args */
		 ilu_string,		/* return type, OPTIONAL, RETAIN */
		 ILU_ERRS((internal, no_memory)) *err);
/*
 * Defines the i'th method of class c.  Caller owns me_name,
 * me_exceptionVector.  Each (me_exceptionVector[i]) is a result of
 * calling ilu_DefineException.  The "return type" parm is either
 * type ID of the method's return type, or NIL if the method has
 * no return type.
 */

ILU_PUBLIC ilu_boolean
ilu_DefineMethodArg(ilu_Method,		/* m */
		    ilu_cardinal,	/* index of arg */
		    ilu_string,		/* arg name, RETAIN */
		    ilu_boolean,	/* sibling */
		    ilu_ArgDirection,	/* in, out, or inout */
		    ilu_string,		/* type ID, RETAIN */
		    ILU_ERRS((internal, no_memory)) *err);
/*
 * Defines the index'th arg of method m.
 */

#ifdef ILU_HTTPNG_OBJECTS

ILU_PUBLIC ilu_boolean
  ilu_DefineObjectState (ilu_Class,	/* class */
			 ilu_cardinal,	/* index of state field */
			 ilu_string,	/* field name, RETAIN */
			 ilu_string,	/* type ID, RETAIN */
			 ILU_ERRS((internal, no_memory)) *err);
/* defines the n'th state field of the class */

#endif

ILU_PUBLIC ilu_boolean 
ilu_ObjectTypeDefined(ilu_Class t,
		      ILU_ERRS((internal/typeIncomplete)) * err);
/*
 * Called by stub when it thinks it has completely described the
 * given object type.
 */

/*L1, L2 unconstrained*/

ILU_PUBLIC ilu_boolean ilu_CollectibleP (ilu_Class);

ILU_PUBLIC ilu_string ilu_TypeOfException (ilu_Exception);

ILU_PUBLIC ilu_Method 
ilu_MethodNOfClass(ilu_Class,
		   ilu_cardinal /* method index */ );

ILU_PUBLIC      ilu_boolean
ilu_DataOfClass(ilu_Class /* c */ ,
/*
 * All the rest are out parameters, which may be NIL if that info is
 * not needed.
 */
		char ** /* name */ ,
		char ** /* brand */ ,
		char ** /* id */ ,
		char ** /* singleton */ ,
		ilu_boolean * /* collectible */ ,
		ilu_cardinal * /* method_count */ ,
		ilu_cardinal * /* superclass_count */ ,
		ilu_Class ** /* superclasses */ ,
		ilu_boolean * /* optional */ ,
#ifdef ILU_HTTPNG_OBJECTS
		ilu_cardinal * /* n_state_fields */,
		ilu_ObjectState * /* state_fields */,
		ilu_boolean * /* local */,
		ilu_boolean * /* sealed */,
#endif
		ilu_Method * /* methods */ );
/*
 * Returns data of the class.  Any out parameter may be NIL to not
 * receive that data.  Data is still owned by the callee.
 */
     
ILU_PUBLIC      ilu_string	/* RETAIN */
ilu_IDOfClass(ilu_Class);
/* returns cl_unique_id field of ilu_Class */

ILU_PUBLIC      ilu_boolean
ilu_PhonyOfClass(ilu_Class);
/* returns cl_phony field of ilu_Class */

ILU_PUBLIC      ilu_string	/* RETAIN */
ilu_DocStringOfClass(ilu_Class);
/* returns cl_doc_string field of ilu_Class */

ILU_PUBLIC      ilu_boolean
ilu_DataOfMethod(ilu_Method /* m */ ,
/*
 * All the rest are out parameters, which may be NIL if that info is
 * not needed.
 */
		 ilu_string * /* name */ ,
		 ilu_cardinal * /* id */ ,
		 ilu_boolean * /* cacheable */ ,
		 ilu_boolean * /* asynchronous */ ,
		 ilu_cardinal * /* ecount */ ,
		 ilu_Exception ** /* evec */ ,
		 ilu_StubProc ** /* stubproc */ );
/*
 * Returns data of the method.  Any out parm may be NIL to not
 * receive that data.  Ownership of out data is retained by callee.
 */


/* L1, L2 unconstrained */
ILU_PUBLIC ilu_Class ilu_GetGcCallbackClass(void);
/* returns GC callback class */

/*L1.sup < otmu; L2 unconstrained*/

ILU_PUBLIC ilu_Class ilu_FindClassFromID( char *unique_id );
ILU_PUBLIC ilu_Class ilu_FindClassFromName( char *classname );
    /* Ways to look up registered object types. */

ILU_PUBLIC ilu_boolean ilu_IsSubObjectType( ilu_Class a, ilu_Class b );
/* Returns ilu_TRUE iff a is a subtype of b
   (including the degenerate case of a=b). */

ILU_PUBLIC ilu_boolean ilu_HasSubtypes (ilu_Class a);
/* returns ilu_TRUE iff a has registered subtypes */

/* ================ Server side ================ */

typedef struct ilu_ObjectTable_struct ilu_ObjectTable_s, *ilu_ObjectTable;

/**L2 unconstrained;
   before: L1.sup < gcmu;
   after:  result => Inside(result, ilu_rootClass);
   after: !result => L1.sup < gcmu*/
ILU_PUBLIC      ilu_Server
ilu_CreateTrueServer(ilu_string id,
		     ilu_ObjectTable objtab,
		     ilu_LanguageIndex language,
		     ILU_ERRS((bad_locks, broken_locks, bad_param,
			       no_memory, internal)) * err);
/*
 * A server module starts by declaring its existence, with a call on
 * ilu_CreateTrueServer.  (id) must be non-NIL, and different from
 * the ID of every pre-existing server.  If a non-NIL (objtab) is
 * given, the kernel will call its ot_object_of_ih when
 * unmarshalling a reference to an object not currently in the
 * server's hash table of objects; otherwise, only tabled objects
 * may be unmarshalled.  Ownership of the arguments is associated
 * with the result.  Returns NIL iff raising an error.  The server
 * is true for the specified language.
 */

/*L1.sup < cmu*/
/*L2 unconstrained*/

ILU_PUBLIC ilu_string ilu_InventID(void);
/* Generates a string that's unique over space and time.  A server
   with nothing better to use might call this to get an ID.  The
   malloc'ed return value is owned by the caller.  */

/* L1 >= {server}; L1 >= {gcmu} if result is true and collectible */
typedef         ilu_Object
ilu_ObjectTable_apply_proc(ilu_ObjectTable self,
			   ilu_string ih);
/*
 * Returns the object associated with the given instance handle, or
 * NIL if no such object.  Caller owns (ih).  The object returned
 * was obtained by calling ilu_FindOrCreateTrueObject.
 */

/* L1 >= {server}; L2 unconstrained */
typedef void    ilu_ObjectTable_free_proc(ilu_ObjectTable self);
/*
 * The server using this object table is being closed,
 * ot_object_of_ih will not be called again. Release appropriate
 * resources and ilu_free(self).
 */

struct ilu_ObjectTable_struct {
  /* Fields are readonly. */

  ilu_ObjectTable_apply_proc *ot_object_of_ih;
  ilu_ObjectTable_free_proc *ot_free_self;
  ilu_private     ot_rock;
};
/*
 * An object table gives the application the ability to create true
 * objects upon presentation of an instance handle.  The object
 * table is (ultimately) implemented by the application, and passed
 * to the kernel through ilu_CreateTrueServer.  For those
 * applications that don't need this, NIL can be passed.
 */


/*Main Invariant holds*/
ILU_PUBLIC      ilu_Port
ilu_CreatePort(ilu_Server,
	       ilu_string,	/* protocol, retain */
	       ilu_TransportInfo,	/* tinfo, retain */
	       ilu_Passport,	/* optional, retain */
	       ILU_ERRS((IoErrs, inv_objref, no_resources,
			 bad_locks)) *);
/*
 * A server will use this to create a Port on which to listen for
 * connection requests.  The protocolInfo may be a prefix of a real
 * protocolInfo string; it must at least identify the protocol.  The
 * transportInfo should be suitable for mooring creation (see the
 * "Protocols and Transports" chapter of the ILU reference manual).
 * Ports that have some notion of server identity will use identity
 * info in the passport to some end; it may be an error not to pass a
 * passport, depending on the transport info.  Caller owns the string
 * arguments.  */

/*Main Invariant holds*/
ILU_PUBLIC      ilu_Port
ilu_FullCreatePort(ilu_Server,
		   ilu_string,	        /* protocol, retain */
		   ilu_TransportInfo,	/* tinfo, retain */
		   ilu_Passport,	/* optional, retain */
		   ilu_boolean,         /* public */
		   ILU_ERRS((IoErrs, inv_objref, no_resources,
			     bad_locks)) *);
/*
 * A server will use this to create a Port on which to listen for
 * connection requests.  The protocolInfo may be a prefix of a real
 * protocolInfo string; it must at least identify the protocol.  The
 * transportInfo should be suitable for mooring creation (see the
 * "Protocols and Transports" chapter of the ILU reference manual).
 * Ports that have some notion of server identity will use identity
 * info in the passport to some end; it may be an error not to pass a
 * passport, depending on the transport info.  This port's connection
 * info appears in SBHs for the server's objects iff (public).  Caller
 * owns the string arguments.  */

/*Main Invariant holds*/
ILU_PUBLIC ilu_boolean
ilu_AddCInfo(ilu_Server,
	     ilu_string,  	  	/* pinfo, retain */
	     ilu_TransportInfo,		/* tinfo, retain */
	     ILU_ERRS((inv_objref, bad_locks, internal,
		       no_memory)) *);
/* Add the given contact info to the given server. */

/* L1, L2 unconstrained */
ILU_PUBLIC ilu_boolean
ilu_PortCInfo(ilu_Port,
	      ilu_string *,           /* &pinfo */
	      ilu_TransportInfo *,    /* &tinfo */
	      ILU_ERRS((bad_param, internal, no_memory)) *);
/* Return the contact info of the given port.  Callee retains
   ownership of the returned string and ilu_TransportInfo, which might
   not outlive ilu_DoneWithPort. */

/* L1 >= {server} */
ILU_PUBLIC ilu_boolean
ilu_ServerCInfo(ilu_Server,		/* server, retain */
		ilu_boolean,		/* public */
		ilu_string *,           /* &pinfo */
		ilu_TransportInfo *,    /* &tinfo */
		ILU_ERRS((bad_param, internal, no_memory)) *);
/* Return the contact info of the first public or private (as
   indicated) non-inmem port of the given server.  Callee retains
   ownership of the returned string and ilu_TransportInfo, which might
   not outlive server shutdown.  Returns False if no cinfo is present,
   True otherwise. */

/*L1.sup < s*/
ILU_PUBLIC void ilu_SetServerDefaultPort(ilu_Server s, ilu_Port p);
/*
 * If more than one port is created for a server, this operation
 * specifies which of those ports is used to create contact info for
 * objects in that server.  A no-op if the port is closed.
 */

/**L1 >= {the object's server};
   L1 >= {gcmu} if cl collectible*/
ILU_PUBLIC      ilu_Object
ilu_FindOrCreateTrueObject(ilu_string /* ih, REQUIRED, RETAINED */ ,
			   ilu_Server server, ilu_Class cl,
			   ilu_refany languageSpecificObject);
/*
 * This procedure is used for creating true objects.  The object is
 * true for the language for which the server is true.  This
 * procedure is called with non-NIL ih, server, cl, and
 * languageSpecificObject.  The LS runtime lets the application
 * choose ih, and/or provides a default way of choosing ih.
 * Ownership of ih is retained by caller.  If the kernel object
 * already exists, its object type must be exactly cl, and
 * languageSpecificObject is ignored.  Otherwise,
 * languageSpecificObject is the one for the language for which the
 * server is true.
 */

/*Inside(obj's server, obj's type)*/
ILU_PUBLIC	ilu_integer
ilu_SetObjectGCTimeout (ilu_Object, ilu_integer, ILU_ERRS((bad_param)) *);
/*
 * Set the GC timeout field of the object to the value specified in the
 * second parameter.  Returns the old timeout value on success.
 */

/*L2 unconstrained*/

/*Inside(obj's server, obj's type)*/
typedef ilu_boolean(*ilu_ObjectNoter) (ilu_Object obj, int vi);
/*
 * The LS runtime provides this procedure.  The kernel calls this
 * procedure when the kernel becomes, or ceases being, very
 * interested in an object.  The LS runtime uses this opportunity to
 * keep a LS object around as long as there are surrogates.  This
 * procedure should not call anything [eg, ilu_RegisterLSO] that
 * might free the object. This procedure normally returns ilu_TRUE; it
 * can return ilu_FALSE to cause the effect of ilu_RegisterLSO(obj, NIL,
 * noter's lang).
 */

/*L1.sup < cmu*/
ILU_PUBLIC void 
ilu_SetNoter(ilu_ObjectNoter n,
	     ilu_LanguageIndex language);
/*
 * Each LS runtime in the address space calls this at most once,
 * before any objects are created.
 */

/**L1 >= {obj's server};
   obj true && collectible => L1 >= {gcmu}*/
ILU_PUBLIC ilu_boolean ilu_VeryInterested(ilu_Object obj);
/* Tests whether the kernel is very interested in this object. */

/**L1 < gcmu*/
ILU_PUBLIC ilu_FineTime
ilu_SetDefaultGCPingPeriod(ilu_FineTime);
/* When called, sets the default callback ping period to its
 * parameter, and returns the previously set default ping period.
 * GC Callback objects received after the setting will have the new
 * ping period.
 */

/**before: L1 = {};
   after:  result!=NIL => Inside(result's server, cl);
   after:  result==NIL => L1 = {};
   Main Remnant holds; L2 otherwise unconstrained*/
ILU_PUBLIC      ilu_Object
ilu_FindOrCreateSurrogate(ilu_Server server,
			  ilu_string ih,
			  ilu_Class type,
			  ILU_ERRS((bad_locks, broken_locks,
				    inv_objref, internal)) * err);
/*
 * Create and return an instance of the specified type, with the
 * specified ih, on the specified server.
 */

/** Main Invariant holds;
    L2 disjoint {X's iomu, callmu}, for relevant port or connection,
				    if any */
typedef void    (*ilu_TransportInputHandler) (ilu_refany rock);
/*
 * For use with ilu_SetConnectionRequestHandler and
 * ilu_SetConnectionInputHandler.
 */

typedef struct ilu_TIH_str ilu_TIH_s, *ilu_TIH;

struct ilu_TIH_str {
  /* L1, L2 unconstrained */

  ilu_TIH         next;		/* for impl's use, not client's */
  ilu_TransportInputHandler proc;
  ilu_refany      rock;
};
/*
 * When supplying a ilu_TIH, (next) is not significant --- but you
 * can't modify it later.
 */

/*Main Invariant holds; L2 disjoint {port's iomu, waitmu}*/

ILU_PUBLIC          ilu_boolean
ilu_SetConnectionRequestHandler(ilu_Port /*port*/,
				ilu_TransportInputHandler /*proc*/,
				ilu_refany /*rock*/,
				ILU_ERRS((no_memory, imp_limit,
					  no_resources, bad_param,
					  bad_locks, internal,
					  broken_locks)) * err);
/*
 * The server then waits for connection requests to show up on the
 * port.  A multi-threaded runtime does this by forking a thread per
 * port; a single-threaded one registers a connection request
 * handler with the main loop; handler registration is also used for
 * the local (in-memory) port regardless of threading.  (!proc) is
 * equivalent to passing a no-op.  Either: (a) a connection request
 * is probably present right now and this procedure calls
 * ((*proc)(rock)), or (b) this call arranges that, until a later
 * call on this proc for the same port, whenever connection requests
 * are probably present, eventually either (1) ((*proc)(rock)) is
 * invoked, (2) the port is closed (which in turn causes an eventual
 * call on the given handler), or (3) this procedure is called again
 * on the same port.  Raises bad_param/closed if the port is closed.
 * The port's iomu and waitmu are not held by any thread.  Returns
 * ilu_TRUE if no errors.
 */

ILU_PUBLIC      ilu_boolean
ilu_WaitForPortConnectionRequest(ilu_Port port,
				 ILU_ERRS((bad_locks, broken_locks,
					   interrupted)) * err);
/*
 * A multi-threaded runtime uses this procedure to wait for a
 * connection request to arrive at the given port.  Returns false
 * when erring or port is closed, true when a connection request is
 * (probably --- ilu_HandleNewConnection should cope with the
 * uncertainty) waiting.
 */

ILU_PUBLIC      ilu_Connection
ilu_HandleNewConnection(ilu_Port port, ilu_boolean * closed,
			ILU_ERRS((IoErrs, bad_locks,
				  no_resources)) * err);
/*
 * When input shows up on the FD for a port, the server calls this
 * procedure to create a connection.  This proc returns a new
 * "incoming" connection to the port's server.  This proc sets
 * *closed; result is meaningful only if *closed is false.  Raises
 * no_resources/fds if opening the connection now would exceed the
 * kernel's FD budget (LSR is now stuck --- it has no way to "wait"
 * for this to change).  Returns NIL if raising an error, or if
 * there wasn't really a connection request waiting.  If the port is
 * closed, the server next calls ilu_DoneWithPort.  If result is
 * non-NIL and (*closed) false, server eventually calls
 * ilu_DoneServingConnection(result).
 */

ILU_PUBLIC      ilu_boolean
ilu_DoneWithPort(ilu_Port port,
		 ILU_ERRS((bad_param, bad_locks, internal)) * err);
/*
 * The server calls this after it's done passing (port) as a
 * parameter to procedures in this interface.  After this call,
 * (port) is unusable (i.e., maybe freed).
 */

/*Main Invariant holds; L2 disjoint {conn's iomu, callmu, waitmu}*/

ILU_PUBLIC          ilu_boolean
  ilu_BlockingWaitForInputOnConnection(ilu_Connection conn,
				       ilu_FineTime * limit);
/*
 * The server then waits for input to show up on that connection.
 * Again, a multi-threaded runtime forks a thread that calls this
 * procedure (passing NIL for limit means +infinity), a
 * single-threaded runtime sets an input handler.  Catches the
 * threading system's interrupt-a-thread signal, returning ilu_TRUE in
 * that case (but giving no other clue that this happened,
 * unfortunately).  If ilu_BlockingWaitForInputOnConnection returns
 * ilu_FALSE, the connection should be abandoned --- the server proceeds
 * to call ilu_DoneServingConnection.  After input allegedly shows
 * up, the server begins processing the request by calling
 * ilu_ReceiveRequest.
 */

ILU_PUBLIC          ilu_boolean
ilu_SetConnectionInputHandler(ilu_Connection conn,
			      ilu_TransportInputHandler tih_proc,
			      ilu_refany tih_rock,
			      ILU_ERRS((no_memory, internal,
					no_resources, bad_param,
					bad_locks)) * err);
/*
 * A single-threaded runtime calls this to set the input handler for
 * the connection; also used inside the kernel for outgoing
 * connections when single-threaded; also used (by LSR) on
 * connections of the local (in-memory) port of a true server,
 * regardless of threading.  (!tih_proc) means don't handle input
 * for a while.  Returns ilu_TRUE if no error has been raised, ilu_FALSE if
 * an error has been raised.
 * 
 * In a single-threaded runtime, each connection has a conceptual
 * variable that holds an input-handling closure.  Whenever input
 * progress can be made on this connection, there will eventually be
 * an invocation of the current value of the variable (unless and
 * until the variable holds a null closure); closing the connection
 * causes one last eventual call on this closure (if not null).
 * Thus, DoneServingConnection cannot be called until that final
 * call.  When a connection is first created, its input-handling
 * closure variable holds a null closure.
 * 
 * Either (a) this procedure stores the given closure into the
 * input-handling closure variable of this connection, or (b) input
 * progress can be made right now, the given closure is not null,
 * and this procedure invokes the given closure immediately without
 * changing the variable.  A non-null closure thus cannot rely on
 * the input-handling closure variable being already set by this
 * call, and must arrange that upon its return the closure variable
 * is set correctly (e.g., by calling this procedure again as one of
 * the last things it does).
 * 
 * Note that this procedure doesn't restrict what the input-handling
 * closure can do (beyond all the other restrictions in this
 * interface).  In particular, the closure can call
 * ilu_DoneServingConnection, which renders (conn) unusable.
 */

/*L1 >= {cmu, conn's server}, L1.sup < trmu; L2 disjoint conn*/
ILU_PUBLIC      ilu_boolean
ilu_ClearConnectionInputHandler(ilu_Connection conn,
				ILU_ERRS((no_memory, internal,
					  no_resources)) * err);
/*
 * Like ilu_SetConnectionInputHandler(conn, NIL, NIL, err), but with
 * different locking requirements.
 */

/* Locking unconstrained */
ILU_PUBLIC	ilu_boolean
  ilu_InmemConnection(ilu_Connection);
/* returns ilu_TRUE if connection is via an inmem transport */

typedef enum {
  ilu_RcvReqStat_noop,		/* nothing to do for this request */
  ilu_RcvReqStat_quit,		/* stop working on connection */
  ilu_RcvReqStat_request	/* decent message received */
}               ilu_RcvReqStat;

/*Main Invariant holds*/
/**before: L2 disjoint {conn's callmu, iomu, waitmu},
 *  after: Call-Locking(call, IHi)                   if  *initted,
 *  after: L2 disjoint {conn's callmu, iomu, waitmu} if !*initted */
 
ILU_PUBLIC      ilu_RcvReqStat
ilu_ReceiveRequest(ilu_Call_s * call, ilu_boolean * initted,
		   ilu_Connection conn, ilu_Class * intro_type,
		   ilu_Method * meth, ilu_cardinal * sn,
		   ILU_ERRS((bad_locks, IoErrs)) * err);
/*
 * A server runtime calls this to start processing a request. (call)
 * points to uninitialized memory owned by the stub; (initted) is an
 * OUT ilu_boolean parameter.
 * 
 * There should be no concurrent calls on ilu_ReceiveRequest for the
 * same connection.  No thread should hold (conn)'s waitmu.
 * 
 * If the result is ilu_RcvReqStat_request: *initted is ilu_TRUE; *call has
 * been initialized; meaningful values have been stored through
 * intro_type, meth, and sn (provided so that the LSR can include
 * some interesting details in debugging printouts); *err indicates
 * success; call->ca_ms == ilu_cmsHi.  Next arguments are
 * unmarshalled, then ilu_RequestRead and then one of
 * ilu_BeginReply, ilu_BeginException, or ilu_NoReply must later be
 * called (unless an error causes a jump to ilu_FinishCall); these
 * things are done by the stub, which the LSR next invokes like
 * this: (*meth->me_stubproc)(call).  The language-specific runtime
 * can use the "private" field of the ilu_Call to pass other
 * information to the stub.
 * 
 * For other results, *initted may or may not be ilu_TRUE, *call is
 * uninitialized iff *initted is ilu_FALSE, *err may or may not indicate
 * an error, and intro_type, methd, and sn may or may not have been
 * stored through.  Whenever this procedure sets *initted to a true
 * value, ilu_FinishCall must eventually be called; pass err to
 * ilu_FinishCall, then pass *err to server module if ilu_FinishCall
 * doesn't consume it.  When *initted is set to ilu_FALSE, notify the
 * server module of *err.
 * 
 * When this procedure returns ilu_RcvReqStat_quit, the LSR should stop
 * processing requests on this connection, and proceed to call
 * ilu_DoneServingConnection.  The connection has been closed, or
 * will be closed by the kernel ASAP, so the LSR can rely on input
 * and wait-for-input procedures behaving accordingly.
 * 
 * When this procedure returns ilu_RcvReqStat_noop, the stub should not
 * be called, but the connection should continue to be served; the
 * LSR goes back to waiting for the next request on this connection.
 */
/****
Here's an outline of how a multi-threaded LSR uses ilu_ReceiveRequest,
assuming the stub calls ilu_FinishCall:

ilu_boolean     going = ilu_TRUE;
while (going) {
  // Main Invariant holds, L2 disjoint conn's mutexes
  ilu_RcvReqStat  rrs;
  ilu_Call_s      call;
  ilu_boolean     initted;
  ilu_Class       intro;
  ilu_Method      meth;
  ilu_cardinal    sn;
  ilu_Error       err;
  if (!ilu_BlockingWaitForInputOnConnection(conn, NULL))
    break;
  rrs = ilu_ReceiveRequest(&call, &initted, conn, &intro, &meth, &sn,
			   &err);
  going = rrs != ilu_RcvReqStat_quit;
  if (rrs == ilu_RcvReqStat_request) {
    if (ilu_ThreadPerRequest(conn))
      fork({(*meth->me_stubproc) (&call);});
    else
      (*meth->me_stubproc) (&call);
  } else if (initted)
    ilu_FinishCall(&call, &err);
}
ilu_DoneServingConnection(conn);
****/

/*L1, L2 unconstrained*/
ILU_PUBLIC ilu_boolean ilu_ThreadPerRequest(ilu_Connection conn);
/* After calling ilu_ReceiveRequest, and before invoking the stub,
 * a multithreaded runtime consults this procedure to decide whether
 * to fork a thread to process this request. */

/**before: Call-Locking(call, IHi);
    after: Call-Remnant(call, err, ilu_TRUE);
    after: call->ca_ms==ilu_cmsHi && call->ca_msInput;
    after: result!=NIL => Inside(call->ca_server, call->ca_intro_type);
    after: result==NIL => L1 = {}*/
ILU_PUBLIC ilu_Object 
ilu_GetCallSingleton(ilu_Call call,
		     ILU_ERRS((bad_param)) * err);
/*
 * The stub unmarshalls the arguments, beginning with the
 * discriminator.  If call->ca_intro_type is a singleton,
 * ilu_GetCallSingleton is called to get the discriminator;
 * otherwise, the discriminator is unmarshalled by a call on
 * ilu_InputObjectID.  This procedure returns NIL iff raising an
 * error, in which case the caller should jump to the call on
 * ilu_FinishCall.
 */

/**before: Call-Locking(call, IHi);
    after: Call-Invariant(call, err),
	   success => Call-Locking(call, VLo)*/
ILU_PUBLIC          ilu_boolean
ilu_RequestRead(ilu_Call call,
		ILU_ERRS((IoErrs)) * err);
/*
 * A server stub calls this after unmarshalling the arguments,
 * before executing the procedure.  If err raised, caller then jumps
 * to the call on ilu_FinishCall.
 */

/*Main Invariant holds, L2 otherwise unconstrained*/

ILU_PUBLIC          ilu_cardinal
ilu_BeginSizingReply(ilu_Call call,
		     ilu_boolean exns_possible,
		     ILU_ERRS((IoErrs)) * err);
/*
 * After successful procedure execution, the server stub calls this
 * procedure to set up the reply size computation.  The sum of this
 * procedure's result and results' sizes is passed as argSize to
 * ilu_BeginReply.
 */

ILU_PUBLIC          ilu_cardinal
ilu_BeginSizingException(ilu_Call call,
			 ilu_integer eindex,
			 ILU_ERRS((IoErrs)) * err);
/*
 * If the call raises a programmer-defined exception, the server
 * stub calls this procedure to start computation of the reply size.
 * The sum of this procedure's result and size of exn's parm (if
 * any) is passed as argSize to ilu_BeginException.  This routine
 * is used to signal both system exceptions and user exceptions.
 * In the first case, the eindex value should be the inverse of the
 * integer value of the ilu_ProtocolException being signalled; in
 * the second case, it should be 1 + the zero-based index of the
 * exception in the list of exceptions for the method.
 */

/**before: Call-Locking(call, Lo);
    after: Call-Invariant(call, err),
	   success => Call-Locking(call, OHi). */

ILU_PUBLIC          ilu_boolean
ilu_BeginReply(ilu_Call call,
	       ilu_boolean exns_possible, ilu_cardinal argSize,
	       ILU_ERRS((bad_locks, IoErrs)) * err);
/*
 * Server stub calls this to introduce successful results.
 * `exns_possible` indicates whether this call might raise exceptions.
 * If result is ilu_TRUE, the results are marshalled; if result is
 * ilu_FALSE, proceed to ilu_FinishCall.
 */

ILU_PUBLIC          ilu_boolean
ilu_BeginException(ilu_Call call,
		   ilu_integer evalue, ilu_cardinal argSize,
		   ILU_ERRS((bad_locks, IoErrs)) * err);
/* If the call should raise an exception instead of return some
 * results, the server stub calls this (instead of ilu_BeginReply)
 * to introduce the exception&parameter.  For protocol exceptions, evalue
 * is the inverse of the integer value of the ilu_ProtocolException value
 * of the exception; for programmer-defined
 * exceptions, evalue is 1 + the subscript into the method's
 * exceptionVector.  argSize is the marshalled size of the
 * exeption parameter.  If result is ilu_TRUE, then the exception parameter
 * is marshalled next; otherwise, proceed to ilu_FinishCall. */

/* Call-Locking(call, OHi) */

ILU_PUBLIC      ilu_boolean
ilu_FullFinishReply(ilu_Call call,
		    ilu_Batcher b,	/* OPTIONAL */
		    ILU_ERRS((bad_locks, IoErrs)) * err);
/*
 * End bracket for success results; call ilu_FinishCall next,
 * regardless of err.
 */

ILU_PUBLIC      ilu_boolean
ilu_FinishReply(ilu_Call call,
		ILU_ERRS((bad_locks, IoErrs)) * err);
/*
 * ilu_FullFinishReply where (b==NIL).
 */

ILU_PUBLIC      ilu_boolean
ilu_FullFinishException(ilu_Call call,
			ilu_Batcher b,	/* OPTIONAL */
			ILU_ERRS((bad_locks, IoErrs)) * err);
/*
 * End bracket for exn result; call ilu_FinishCall next, regardless
 * of err.
 */

ILU_PUBLIC      ilu_boolean
ilu_FinishException(ilu_Call call,
		    ILU_ERRS((bad_locks, IoErrs)) * err);
/*
 * ilu_FullFinishException with (b==NIL).
 */

/* Call-Locking(call, Lo) */

ILU_PUBLIC ilu_boolean 
ilu_NoReply(ilu_Call call,
	    ILU_ERRS((bad_param, bad_locks, broken_locks)) * err);
/*
 * The server stub calls this for asynchronous methods.  Next call
 * ilu_FinishCall.
 */

/*L1, L2 unconstrained; call only when single-threaded */
ILU_PUBLIC ilu_boolean ilu_ConnectionServingP(ilu_Connection conn);
/* Is this there a call outstanding on this incoming connection? */

/*L1.sup < cmu*/

/*L2 disjoint {conn's callmu, iomu, waitmu}*/
ILU_PUBLIC      ilu_boolean
ilu_DoneServingConnection(ilu_Connection conn,
			  ILU_ERRS((bad_param, broken_locks,
				    bad_locks, internal)) * err);
/*
 * A true server was serving on (conn), and calls this to cease
 * serving.  This procedure can *ONLY* be called on "incoming"
 * connections.  After this call, (conn) cannot be used for
 * anything.  So a single-threaded runtime should call this only for
 * the outermost handler working on a given connection;
 * ilu_ConnectionServingP is useful for this restriction.
 */

/*L2 unconstrained*/
ILU_PUBLIC void ilu_ClosePort(ilu_Port port);
/*
 * A true server can call this to cease exporting itself through the
 * given port.  If the port was the server's default port, some
 * other port (if there are any) is chosen to be the default.
 * Depending on the degree of interference from other
 * threads/event-handlers, either the port will have been closed by
 * return time, or a bit will have been set indicating that the port
 * should be closed as soon as is convenient; in the latter case,
 * this procedure may or may not have done something to hasten the
 * arrival of that convenient time.  This procedure may free (port).
 * Since only the first contact info in an SBH is used by a client,
 * woe unto a client that uses an SBH for a closed port.
 */

/*L1, L2 unconstrained*/
ILU_PUBLIC ilu_boolean ilu_InmemPort (ilu_Port);
/* returns ilu_TRUE if the port uses the inmem transport */

/* ================ Concurrent I/O routines ================ */

/*Main Invariant holds; L2 otherwise unconstrained*/
ILU_PUBLIC void ilu_RunMainLoop(int *stop);
/*
 * A single-threaded runtime calls this to animate all true servers,
 * handle alarms, and do any other registered input processing.  A
 * multi-threaded runtime never calls this procedure, instead
 * forking threads to animate servers and pass the time.  This
 * procedure may be invoked recursively, so long as no two
 * cuncurrent invocations are given the same address in (stop). This
 * procedure processes input and time until ilu_ExitMainLoop is
 * invoked on (stop).
 */

/*L1, L2 unconstrained; synch provided by single-threadedness*/

ILU_PUBLIC void ilu_ExitMainLoop(int *stop); 
/*
 * This causes the current invocation of (ilu_RunMainLoop(stop)) to
 * return once it's done with the input handler or alarm it's
 * currently executing.
 */

/*Main Invariant holds; L2 otherwise unconstrained*/
typedef void (*ilu_SignalCallbackHandler) (ilu_refany);

/*Main Invariant holds; L2 otherwise unconstrained*/
ILU_PUBLIC void
  ilu_SetSignalCallbackHandler
  (ilu_SignalCallbackHandler,	/* function to invoke when select is
				   broken by an interrupt */
   ilu_refany,			/* argument to pass to handler */
   ilu_SignalCallbackHandler *,	/* OUT:  previously registered handler,
				   or NULLFN */
   ilu_refany *,		/* OUT:  arg for previously registered
				   handler, or NIL */
   ILU_ERRS((internal)) *);	/* possible error return */
/* This function pointer is checked when the main loop is
   interrupted by a signal.  If non-NULL, the function
   is invoked before the main loop resumes. */

typedef struct ilu_Closure_str ilu_Closure_s, *ilu_Closure;

struct ilu_Closure_str {
  /* L1 >= {daimu}; L2 unconstrained */

  ilu_Closure     next;

  /* L1, L2 unconstrained */

  ilu_ClosureProc proc;
  ilu_private     rock;
};

/*Main Invariant holds; L2 otherwise unconstrained*/
typedef void (*ilu_IOHandler)(int fd, ilu_private rock);
/*
 * (fd) not significant when called after unregistering.
 */

ILU_PUBLIC      ilu_boolean
ilu_RegisterInputSource(int fd,
			ilu_IOHandler proc, ilu_private rock);
/*
 * A single-threaded runtime calls this procedure to declare how to
 * handle input on a given FD.  It returns ilu_FALSE if it can't do its
 * job due to some resource limitation.  ilu_UnregisterInputSource
 * must be called on this FD before ilu_RegisterInputSource can be
 * called on it again.
 */

ILU_PUBLIC ilu_boolean ilu_UnregisterInputSource(int fd);
/*
 * A single-threaded runtime calls this procedure to suspend
 * handling input on an FD.  It returns ilu_FALSE if input on the FD
 * wasn't being handled.
 */

ILU_PUBLIC      ilu_boolean
ilu_UnregisterAndReturnInputSource(int fd, ilu_IOHandler * proc,
				   ilu_private * rock);
/*
 * Like ilu_UnregisterInputSource, but also returns the registered
 * handler if there was one.
 */

ILU_PUBLIC ilu_boolean 
ilu_RegisterOutputSource(int fd,
			 ilu_IOHandler proc, ilu_private rock);
/*
 * A single-threaded runtime calls this procedure to queue output
 * for an FD.  It returns ilu_FALSE if it can't do its job due to some
 * resource limitation.  ilu_UnregisterOutputSource must be called
 * on this FD before ilu_RegisterOutputSource can be called on it
 * again.
 */

ILU_PUBLIC ilu_boolean ilu_UnregisterOutputSource(int fd);
/*
 * A single-threaded runtime calls this procedure to suspend
 * queueing output on an FD.  It returns ilu_FALSE if output wasn't
 * being queued on the FD.
 */

ILU_PUBLIC ilu_boolean 
ilu_UnregisterAndReturnOutputSource(int fd, ilu_IOHandler * proc,
				    ilu_private * rock);
/*
 * Like ilu_UnregisterOutputSource, but also returns the registered
 * handler if there was one.
 */

/*L1.sup < timu; L2 unconstrained*/

ILU_PUBLIC ilu_refany ilu_CreateAlarm(void);
/* Available in both single-threaded and multi-threaded environments.
   Creates a (re)settable alarm. */

ILU_PUBLIC void ilu_SetAlarm(ilu_refany alarm, ilu_FineTime t,
			     /*for invoking: Main Invariant holds*/
			     ilu_ClosureProc proc,
			     ilu_private rock);
/*
 * An alarm has a trigger time and a closure.  The closure is
 * invoked once, as soon after the trigger time as the runtime is
 * able.  ilu_SetAlarm overwrites the previous setting of the alarm.
 */

ILU_PUBLIC void ilu_UnsetAlarm(ilu_refany alarm);
/* Effectively sets the trigger time to infinity. */

ILU_PUBLIC void ilu_DestroyAlarm(ilu_refany alarm);
/*
 * Frees the resources of the given alarm.  If the alarm is
 * currently "set", does not invoke.
 */

ILU_PUBLIC      ilu_boolean
ilu_DoSoon(ilu_Closure c,
	   ILU_ERRS((bad_param, bad_locks, broken_locks,
		     internal)) * err);
/*
 * Causes the given closure to be invoked (from the main loop or a
 * separate thread) as soon as is convenient.  Caller retains
 * ownership of the ilu_Closure, but promises to not modify or free
 * it until after its execution begins.
 * The kernel uses this internally, but applications can also use
 * it by creating an ilu_Closure struct value with a NULL value in
 * the "next" field, and calling ilu_DoSoon() with that struct.
 */

typedef struct {
  /* These fields are readonly */

  /* Main Invariant holds; L2 otherwise unconstrained */
  void            (*ml_run) (int *stop);

  /* L1, L2 unconstrained */

  void            (*ml_exit) (int *stop);
  ilu_boolean(*ml_register_input) (int fd,
				   ilu_IOHandler handler,
				   ilu_private rock);
  ilu_boolean(*ml_unregister_input) (int fd,
				     ilu_IOHandler * handler,
				     ilu_private * rock);
  ilu_boolean(*ml_register_output) (int fd,
				    ilu_IOHandler handler,
				    ilu_private rock);
  ilu_boolean(*ml_unregister_output) (int fd,
				      ilu_IOHandler * handler,
				      ilu_private * rock);

  /* L1.sup < timu */

  ilu_refany      (*ml_create_alarm) (void);
  void            (*ml_set_alarm) (ilu_refany alarm,
				   ilu_FineTime t,
  /* Main Invariant holds; L2 otherwise unconstrained. */
		                   void (*proc) (ilu_private rock),
				   ilu_private rock);
  void            (*ml_unset_alarm) (ilu_refany alarm);
  void            (*ml_destroy_alarm) (ilu_refany alarm);
  /*
   * Even free()s (alarm).  If (alarm) is currently "set", does not
   * invoke the set proc.
   */
}               ilu_MainLoop;
/*
 * ml_unregister_input and ml_unregister_output return the
 * registered handlers, if any.
 */

/*L1, L2 unconstrained; synch provided by single-threadedness*/

ILU_PUBLIC void ilu_SetMainLoop(ilu_MainLoop *ml);
/*
 * A single-threaded runtime, or an application running thereon, can
 * call this procedure to supply a non-standard implementation of
 * the main loop (eg, the main loop of another toolkit).  A
 * multi-threaded runtime calls this to supply the implementation of
 * alarms; the other procedure slots are NIL.  This procedure should
 * be called before any calls on ilu_RunMainLoop, ilu_ExitMainLoop,
 * ilu_RegisterInputSource, ilu_UnregisterInputSource,
 * ilu_RegisterOutputSource, ilu_UnregisterOutputSource,
 * ilu_CreateAlarm, ilu_SetAlarm, ilu_UnsetAlarm, or
 * ilu_DestroyAlarm; when we get our error system we can report
 * violations.  The storage for the argument is never freed.  The
 * kernel makes few enough calls on create_alarm that each could
 * fork a new thread (in a multi-threaded runtime); the
 * application's demands are not constrained.
 */

ILU_PUBLIC ilu_boolean     ilu_AddRegisterersToDefault
                (
		 ilu_boolean(*reg_inp) (int fd,
					ilu_IOHandler handler,
					ilu_private rock),
		 ilu_boolean(*can_inp) (int fd),
		 ilu_boolean(*reg_out) (int fd,
					ilu_IOHandler handler,
					ilu_private rock),
		 ilu_boolean(*can_out) (int fd),
		 void (*set_alarm) (ilu_FineTime t,
/* Main Invariant holds; L2 otherwise unconstrained */
				    void (*proc) (ilu_FineTime t)),
		 void (*can_alarm) (void)
);
/*
 * ILU's default main loop for UNIX is willing to notify another
 * main loop of input and output handler registrations and
 * de-registrations.  This is useful for integrating ILU's main
 * loop with some other, relatively uncooperative, main loop.  The
 * integrator calls ilu_AddRegisterersToDefault with procs that
 * notify the other main loop.  ilu_TRUE is returned on successful
 * extension; ilu_FALSE otherwise.  The default main loop
 * multiplexes its alarms into one fundamental alarm; set_alarm and
 * can_alarm are given the scheduling of this fundamental alarm.
 * When ILU calls (*set_alarm)(t, proc), the non-ILU main loop
 * should arrange to call proc(u) soon after time t arrives, where
 * t <= u <= the time of the call on proc(u); larger (valid) values
 * of u are better, but not a lot better.  If set_alarm is called
 * again before proc is called, this changes the time at which proc
 * should be called.  When set_alarm is called after proc, this
 * schedules a new call on proc.  can_alarm cancels the scheduled
 * call on proc, if any.
 */

/* Main Invariant holds, L2 otherwise unconstrained */
typedef void
ilu_FDWaitProc(int fd, int auxfd,
	       ilu_boolean * sure,
	       ilu_FineTime * limit,
	       ILU_ERRS((interrupt)) * err);

typedef struct {
  /* These fields are readonly */

  ilu_FDWaitProc *wt_read_wait;
  ilu_FDWaitProc *wt_write_wait;

}               ilu_WaitTech;
/*
 * These two procedures return ASAP after any of the following five
 * conditions becomes true:
 * 
 * (1) the appropriate kind of I/O can be done on the given file
 * descriptor without blocking (this includes detecting EOF or any
 * error),
 * 
 * (2) (auxfd != -1) and (input can be done on auxfd or an error can be
 * detected on auxfd) without blocking.
 * 
 * (3) when an exceptional condition exists on the FD,
 * 
 * (4) when *limit (+infinity if limit==NIL) is exceeded, or
 * 
 * (5) the thread has been asked to interrupt its current call.
 * 
 * These procedures set *sure.  When *sure is set true, one of the
 * first three conditions held; when *sure is set false, one of the
 * last two held.  interrupt is raised in case (5), and the
 * ilu_interruptSet error member is significant.  This data
 * structure is only for use in multi-threaded programs, and these
 * procedures block only the calling thread.
 * 
 * Caller will naturally plan on not closing the given FD (nor the
 * transport and connection or mooring and port that uses it)
 * concurrently with one of these calls.
 */

/*Main Invariant holds; called only from start code*/

ILU_PUBLIC void ilu_SetWaitTech(ilu_WaitTech *wt);
/*
 * A multi-threaded runtime calls this procedure to supply the means
 * to block a thread until it can do I/O on a given file descriptor.
 */

/*L1.sup < trmu*/
ILU_PUBLIC ilu_boolean ilu_SIGPIPE_Handled(void);
/*
 * SIGPIPE is occasionally raised as ILU TCP connections are torn
 * down.  A runtime or app that cares about SIGPIPE signals does the
 * following two things before creating the first TCP connection or
 * port: (1) installs its SIGPIPE handler, and (2) calls
 * ilu_SIGPIPE_Handled to prevent ILU from installing its own
 * handler (which simply ignores the signal).  Such an app or
 * runtime must be prepared to cope with SIGPIPEs arising due to
 * ILU.  ilu_SIGPIPE_Handled returns ilu_TRUE if called early enough;
 * otherwise returns ilu_FALSE.
 */

/* Main Invariant */
ILU_PUBLIC void ilu_SetRecvSendProcs(ilu_RecvProc, ilu_SendProc);
/* 
 * Binds the implementations of "recv()" and "send()" in the
 * bsdutils code to the specified functions.  This is necessary
 * to allow the Java runtime to override the broken implementations
 * of these two functions provided in the Solaris2 JDK 1.0.2.
 */

#ifdef TCPIP_TRANSPORT

ILU_PUBLIC ilu_cardinal
  ilu_tcp_SetDefaultBuffersize (ilu_cardinal /* buffersize in bytes */);
/* Sets the default buffersize used in the TCP transport to the
 * specified value.  Returns the old value.
 */

#endif /* def TCPIP_TRANSPORT */

/* ================ OS Thread support ================ */
/*
 * These routines are only provided if support for OS threads
 * has been configured into the ILU build.
 */

#ifdef ILU_OS_THREADED

/*L1, L2 unconstrained; synch provided by single-threadedness*/
ILU_PUBLIC ilu_boolean
  ilu_InitializeOSThreading(ILU_ERRS((bad_param, no_memory,
				      no_resources, internal)) * err);
/*
 * This routine will initialize the ILU runtime kernel to use either
 * Solaris2 or Win32 or Posix threads (only one kind is allowed at a
 * time).  It amounts to calling ilu_SetWaitTech, ilu_SetMainLoop,
 * and ilu_SetLockTech with metaobjects constructed from the
 * OS-supplied facilities.  It is intended to be used by the C and
 * C++ runtimes, immediately previous to calling ILU_C_SetFork() or
 * its C++ equivalent.  May raise bad_param if the kernel has
 * already been set threaded.
 */

/* the Main invariant holds; L2 otherwise unconstrained */
ILU_PUBLIC ilu_boolean
  ilu_OSForkNewThread (ilu_ClosureProc proc, void *rock,
		       ILU_ERRS((no_memory, no_resources,
				 internal)) *err);


/* ----------------------------------------------- */
/* ilu_OSForkNewThreadEx is the same as ilu_OSForkNewThread
   only it returns thread id in p_thread if p_thread is 
   non null and we're successfull -- assumes that *pv_thread        
   is large enough to hold a thread id on the system.
*/

ILU_PUBLIC ilu_boolean
  ilu_OSForkNewThreadEx (void (*proc)(void *arg), void *arg,
		       void* pv_thread, 
			   ILU_ERRS((no_memory, no_resources, internal)) *err);


ILU_PUBLIC void
  ilu_OSThreads_GetTech (ilu_WaitTech **,	/* OUT, GLOBAL */
			 ilu_LockTech **,	/* OUT, GLOBAL */
			 ilu_MainLoop **);	/* OUT, GLOBAL */

/* L1, L2 unconstrained */

typedef void    (*ilu_PerThreadDataDestructor) (void *perThreadData);

typedef void   *(*ilu_PerThreadDataGetter) (void);

typedef void    (*ilu_PerThreadDataSetter) (const void *perThreadData,
	                    ILU_ERRS((no_memory, internal)) * err);

ILU_PUBLIC ilu_boolean
  ilu_OSThreads_GetPerThreadDataTech
  (ilu_PerThreadDataDestructor destructor,	/* IN, GLOBAL, OPTIONAL */
   ilu_PerThreadDataGetter *getter,		/* OUT, GLOBAL */
   ilu_PerThreadDataSetter *setter,	/* OUT, GLOBAL */
   ILU_ERRS((no_memory, internal)) *);
/*
 * This exports the ability to set/get a single per-thread value.
 * If "destructor" is specified, it is called automatically when a
 * thread disappears on the current per-thread value for that
 * thread, if that value is non-NIL.
 * ilu_OSThreads_GetPerThreadDataTech may be called multiple times,
 * but the first must not be concurrent with any others, and
 * (destructor) is significant in only the first call.
 */

#endif /* (defined(ILU_OS_THREADED)) */


/* ================ Alarm Multiplexing ================ */
/*
 * These data structures and procedures are useful for multiplexing
 * multiple alarms on top of one alarm.  The client constructs
 * procedures with the proper alarming signatures by calling the
 * procedures below to do most of the work.  The data structures and
 * procedures below operate within some mutex provided by the
 * client; call that mutex mxamu.
 */

typedef struct _ilu_Alarmette_s ilu_Alarmette_s, *ilu_Alarmette;

/*L2 unconstrained*/

struct _ilu_Alarmette_s {
  /*L1 >= {mxamu}*/
  
  ilu_Alarmette al_next, al_prev;	/* list links */
  ilu_boolean  al_set;			/* in queue? */
  ilu_FineTime al_trigger;		/* when to execute */
};
/* A data structure common to all alarms multiplexed into other alarms.
   When set==ilu_TRUE, next and prev are non-NIL; when set==ilu_FALSE,
   they are NIL.  Initialize set to ilu_FALSE and next and prev to NIL. */

typedef struct {
  /* L1 >= {mxamu} for access and invocations */

  ilu_Alarmette   ar_head;	/* of queue of alarmettes yet to
				 * trigger */
  /*
   * for calling: L1.sup = mxamu, & other things true of
   * ilu_MXAProc(..)
   */
  void            (*ar_invoke) (ilu_Alarmette a); /* invoke one now */

  void            (*ar_set) (ilu_FineTime t);	/* schedule a call on
						 * ilu_MXAProc */
  void            (*ar_cancel) (void);	/* cancel that */
}               ilu_AlarmRep;
/*
 * Data and procedures provided by the client for multiplexing a
 * set of Alarmettes onto one alarm.  ar_head->al_next and
 * ar_head->al_prev are initialized to ar_head.  ar_invoke invokes
 * the given Alarmette.  ar_set and ar_cancel manipulate the one
 * alarm into which Alarmettes are multiplexed by the procedures
 * below.  (*ar->ar_set)(t) means the client should call
 * ilu_MXAProc(u, ar), once, soon after time t arrives; t <= u <=
 * the time of the call on ilu_MXAProc.  Larger (valid) values of u
 * are better than smaller ones, but not a lot better.  If ar_set
 * is called again before ilu_MXAProc, this changes the time at
 * which ilu_MXAProc should be called.  A call on ar_set after
 * ilu_MXAProc schedules a new call on ilu_MXAProc.  ar_cancel
 * cancels the pending call to ilu_MXAProc, if any.
 */

/*L1 >= {mxamu}*/

ILU_PUBLIC void ilu_MXASet(ilu_AlarmRep *ar, ilu_Alarmette a, ilu_FineTime t);
/* Schedule (*ar->invoke)(a) to happen ASAP after t;
   this replaces any other scheduled invocation of a. */

ILU_PUBLIC void ilu_MXAClear(ilu_AlarmRep *ar, ilu_Alarmette a);
/* Cancel the scheduled invocation of a, if any. */

/*L1.sup = mxamu*/
ILU_PUBLIC void ilu_MXAProc(ilu_FineTime u, ilu_AlarmRep *ar);
/*
 * The client arranges to call ilu_MXAProc(t, ar), once, soon after
 * time t, in response to a call (*ar->ar_set)(t), as described
 * above.  ilu_MXAProc(t, ar) calls (*ar->ar_invoke)(a) for the
 * appropriate Alarmettes a.
 */


/* ================ (Un)Marshalling routines ================ */

/* End brackets for [un]marshalling and size routines;
 * call these after the contents introduced by a call on
 * (Output|Input|SizeOf)(Sequence|Union|Array|Record).
 */
/*L1, L2 unconstrained*/

ILU_PUBLIC ilu_boolean ilu_EndSequence(ilu_Call call, ILU_ERRS((IoErrs)) *err);
ILU_PUBLIC ilu_boolean ilu_EndUnion(ilu_Call call, ILU_ERRS((IoErrs)) *err);
ILU_PUBLIC ilu_boolean ilu_EndArray(ilu_Call call, ILU_ERRS((IoErrs)) *err);
ILU_PUBLIC ilu_boolean ilu_EndRecord(ilu_Call call, ILU_ERRS((IoErrs)) *err);

/* Marshalling routines */
/* Call-Locking(call, OHi)*/

ILU_PUBLIC void
ilu_OutputShortInteger(ilu_Call call, ilu_shortinteger i,
		       ILU_ERRS((IoErrs)) * err);

ILU_PUBLIC void 
ilu_OutputInteger(ilu_Call call, ilu_integer i,
		  ILU_ERRS((IoErrs)) * err);

ILU_PUBLIC void 
ilu_OutputLongInteger(ilu_Call call, ilu_longinteger i,
		      ILU_ERRS((IoErrs)) * err);

ILU_PUBLIC void 
ilu_OutputShortCardinal(ilu_Call call, ilu_shortcardinal i,
			ILU_ERRS((IoErrs)) * err);

ILU_PUBLIC void 
ilu_OutputCardinal(ilu_Call call, ilu_cardinal i,
		   ILU_ERRS((IoErrs)) * err);

ILU_PUBLIC void 
ilu_OutputLongCardinal(ilu_Call call, ilu_longcardinal i,
		       ILU_ERRS((IoErrs)) * err);

ILU_PUBLIC void 
ilu_OutputShortReal(ilu_Call call, float f,
		    ILU_ERRS((IoErrs)) * err);

ILU_PUBLIC void 
ilu_OutputReal(ilu_Call call, double d,
	       ILU_ERRS((IoErrs)) * err);

ILU_PUBLIC void 
ilu_OutputLongReal(ilu_Call call, ilu_longreal f,
		   ILU_ERRS((IoErrs)) * err);

ILU_PUBLIC void 
ilu_OutputEnum(ilu_Call call, ilu_shortcardinal i,
	       ilu_Type the_type,
	       ILU_ERRS((IoErrs)) * err);

ILU_PUBLIC void 
ilu_OutputCharacter(ilu_Call call, ilu_character i,
		    ILU_ERRS((IoErrs)) * err);

ILU_PUBLIC void 
ilu_OutputByte(ilu_Call call, ilu_byte b,
	       ILU_ERRS((IoErrs)) * err);

ILU_PUBLIC void 
ilu_OutputShortCharacter (ilu_Call call, ilu_shortcharacter b,
			  ILU_ERRS((IoErrs)) * err);

ILU_PUBLIC void 
ilu_OutputBoolean(ilu_Call call, ilu_boolean b,
		  ILU_ERRS((IoErrs)) * err);

ILU_PUBLIC void 
ilu_OutputOptional(ilu_Call call, ilu_boolean optionalStatus,
		   ilu_Type the_type, ILU_ERRS((IoErrs)) * err);

ILU_PUBLIC void
ilu_OutputSequence(ilu_Call call, ilu_cardinal len,
		   ilu_cardinal limit, ilu_Type the_type,
		   ILU_ERRS((IoErrs)) * err);

ILU_PUBLIC void 
ilu_OutputSequenceMark(ilu_Call call,
		       ilu_cardinal extent,
		       ILU_ERRS((IoErrs)) * err);
/* Call this every 2^16-1 elements.  ??? What's extent ??? */

ILU_PUBLIC void 
ilu_OutputUnion(ilu_Call call, ilu_cardinal discriminator,
		ilu_TypeKind discriminator_kind,
		ilu_Type the_type, ILU_ERRS((IoErrs)) * err);

ILU_PUBLIC void 
ilu_OutputArray(ilu_Call call, ilu_cardinal length,
		ilu_Type the_type, ILU_ERRS((IoErrs)) * err);

ILU_PUBLIC void 
ilu_OutputRecord(ilu_Call call, ilu_Type the_type,
		 ILU_ERRS((IoErrs)) * err);

ILU_PUBLIC void 
ilu_OutputString(ilu_Call call, ilu_string s,
		 ilu_cardinal len, ilu_cardinal limit,
		 ILU_ERRS((IoErrs)) * err);
    /* Variable-length array of short character. */

ILU_PUBLIC void 
ilu_OutputEString(ilu_Call call, ilu_bytes s,
		  ilu_cardinal len, ilu_cardinal limit,
		  ilu_cardinal expected_encoding,
		  ilu_cardinal current_encoding,
		  ILU_ERRS((IoErrs)) * err);
    /* "s" contains a variable-length sequence of characters,
       using the character set encoding specified in "current_encoding".
       The "len" is the number of bytes, not the number of characters.
       The "expected_encoding" is the character set specified in the
       ISL for this value -- the kernel might be able to translate,
       but it's best to have "expected_encoding" and "current_encoding"
       be the same. */

ILU_PUBLIC void 
ilu_OutputStringVec(ilu_Call call, ilu_string s,
		    ilu_cardinal len,
		    ILU_ERRS((IoErrs)) * err);
    /* Fixed-length array of short character. */

ILU_PUBLIC void
ilu_OutputWString(ilu_Call,
		  ilu_wstring,		/* the string */
		  ilu_cardinal len,	/* len of the string */
		  ilu_cardinal limit,	/* limit for the parm */
		  ILU_ERRS((IoErrs)) * err);
    /* Variable-length array of character. */

ILU_PUBLIC void
ilu_OutputWStringVec(ilu_Call,
		     ilu_wstring,	/* the vector of wchars */
		     ilu_cardinal,	/* len */
		     ILU_ERRS((IoErrs)) * err);
    /* Fixed-length array of character. */

ILU_PUBLIC void 
ilu_OutputBytes(ilu_Call call, ilu_bytes o,
		ilu_cardinal len, ilu_cardinal limit,
		ILU_ERRS((IoErrs)) * err);
    /* Variable-length array of byte. */

#ifdef ILU_FIXED_POINT_SUPPORT

ILU_PUBLIC void
  ilu_OutputFixedpoint (ilu_Call,
			ilu_Bignum,	/* numerator, RETAIN, ((ilu_Bignum)0) => NaN, ((ilu_Bignum)1) => Infinity */
			ilu_Bignum,	/* min_numerator value, OPTIONAL, RETAIN */
			ilu_Bignum,	/* max_numerator value, OPTIONAL, RETAIN */
			ilu_Bignum,	/* denominator value, RETAIN */
			ilu_cardinal,		/* if non-zero, digits as CORBA 'fixed' */
			ilu_cardinal,		/* decimal places as CORBA 'fixed' */
			ilu_FixedPointRangeSize,	
			ILU_ERRS((IoErrs)) *);

#endif /* def ILU_FIXED_POINT_SUPPORT */

#ifdef ILU_HTTPNG_OBJECTS

/*L2 >= {call's conn's callmu, iomu}.
  obj == NIL => Main Invariant holds.
  obj != NIL => Inside(s, cl);
*/
ILU_PUBLIC void
  ilu_BeginOutputObject(ilu_Call call,
  ilu_Object obj,
  ilu_boolean discriminator_p,
  ilu_Class static_type,
  ilu_cardinal nstates,
  ILU_ERRS((IoErrs)) * err);
/* Begin output of object "obj".  "nstates" is the count of the types
   which "obj" has which have state to be output. */

/*L2 >= {call's conn's callmu, iomu}.
  Inside(s, cl)
  where s = obj's server and cl = obj's type. */
ILU_PUBLIC void
  ilu_BeginOutputState(ilu_Call call,
  ilu_Object obj,
  ilu_Class state_class,	/* output state attributes of this type */
  ILU_ERRS((IoErrs)) *err);
/* Begin output for state of type "state_class" */

/*L2 >= {call's conn's callmu, iomu}.
  Inside(s, cl)
  where s = obj's server and cl = obj's type. */
ILU_PUBLIC void
  ilu_FinishOutputState(ilu_Call call,
  ilu_Object obj,
  ilu_Class state_class,	/* output state attributes of this type */
  ILU_ERRS((IoErrs)) *err);
/* Finish output for state of type "state_class" */

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
ILU_PUBLIC void
  ilu_FinishOutputObject(ilu_Call call,
  ilu_Object obj,
  ilu_boolean discriminator_p,
  ilu_Class static_type,
  ILU_ERRS((IoErrs)) *err);
/* Finish output of object "obj" */

#endif /* def ILU_HTTPNG_OBJECTS */

#ifdef ILU_REFERENCE_TYPES

ILU_PUBLIC void
  ilu_OutputReference (ilu_Call call, ilu_boolean providedp,
		       ilu_boolean *first, ilu_ReferenceID id,
		       ILU_ERRS((IoErrs)) *err);

#endif /* def ILU_REFERENCE_TYPES */

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
ILU_PUBLIC void
ilu_OutputObjectID(ilu_Call call, ilu_Object obj,
		   ilu_boolean discriminator_p,
		   ilu_Class static_type,
		   ILU_ERRS((IoErrs)) * err);
/* Output a object; `discriminator_p` iff in discriminator position. */

ILU_PUBLIC void 
ilu_OutputOpaque(ilu_Call call, ilu_opaque o,
		 ilu_cardinal len,
		 ILU_ERRS((IoErrs)) * err);
    /* Fixed-length array of byte. */

/* Un-marshalling routines */
/* Call-Locking(call, IHi) */

ILU_PUBLIC void 
ilu_InputShortInteger(ilu_Call call,
		      ilu_shortinteger * i,
		      ILU_ERRS((IoErrs)) * err);

ILU_PUBLIC void 
ilu_InputInteger(ilu_Call call, ilu_integer * i,
		 ILU_ERRS((IoErrs)) * err);

ILU_PUBLIC void 
ilu_InputLongInteger(ilu_Call call,
		     ilu_longinteger * i,
		     ILU_ERRS((IoErrs)) * err);

ILU_PUBLIC void 
ilu_InputShortCardinal(ilu_Call call,
		       ilu_shortcardinal * i,
		       ILU_ERRS((IoErrs)) * err);

ILU_PUBLIC void 
ilu_InputCardinal(ilu_Call call, ilu_cardinal * i,
		  ILU_ERRS((IoErrs)) * err);

ILU_PUBLIC void 
ilu_InputLongCardinal(ilu_Call call,
		      ilu_longcardinal * i,
		      ILU_ERRS((IoErrs)) * err);

ILU_PUBLIC void 
ilu_InputShortReal(ilu_Call call, float *f,
		   ILU_ERRS((IoErrs)) * err);

ILU_PUBLIC void 
ilu_InputReal(ilu_Call call, double *d,
	      ILU_ERRS((IoErrs)) * err);

ILU_PUBLIC void 
ilu_InputLongReal(ilu_Call call, ilu_longreal * f,
		  ILU_ERRS((IoErrs)) * err);

ILU_PUBLIC void 
ilu_InputEnum(ilu_Call call,
	      ilu_shortcardinal * i,
	      ilu_Type the_type,
	      ILU_ERRS((IoErrs)) * err);

ILU_PUBLIC void 
ilu_InputCharacter(ilu_Call call,
		   ilu_character * i,
		   ILU_ERRS((IoErrs)) * err);

ILU_PUBLIC void 
ilu_InputByte(ilu_Call call, ilu_byte * b,
	      ILU_ERRS((IoErrs)) * err);

ILU_PUBLIC void 
ilu_InputShortCharacter (ilu_Call call, ilu_shortcharacter * b,
			 ILU_ERRS((IoErrs)) * err);

ILU_PUBLIC void 
ilu_InputBoolean(ilu_Call call, ilu_boolean * b,
		 ILU_ERRS((IoErrs)) * err);

ILU_PUBLIC void 
ilu_InputOptional(ilu_Call call,
		  ilu_boolean * optionalStatus,
		  ilu_Type the_type,
		  ILU_ERRS((IoErrs)) * err);

ILU_PUBLIC void 
ilu_InputSequence(ilu_Call call,
		  ilu_cardinal * len, ilu_cardinal limit,
		  ilu_Type the_type, ILU_ERRS((IoErrs)) * err);

ILU_PUBLIC void 
ilu_InputSequenceMark(ilu_Call call,
		      ilu_cardinal extent,
		      ILU_ERRS((IoErrs)) * err);

ILU_PUBLIC void
ilu_InputUnion(ilu_Call call, ilu_cardinal * discriminator,
	       ilu_TypeKind discriminator_kind,
	       ilu_Type the_type, ILU_ERRS((IoErrs)) * err);

ILU_PUBLIC void 
ilu_InputArray(ilu_Call call, ilu_Type the_type,
	       ILU_ERRS((IoErrs)) * err);

ILU_PUBLIC void 
ilu_InputRecord(ilu_Call call, ilu_Type the_type,
		ILU_ERRS((IoErrs)) * err);

ILU_PUBLIC void 
ilu_InputString(ilu_Call call, ilu_string * s,
		ilu_cardinal * len, ilu_cardinal limit,
		ILU_ERRS((IoErrs)) * err);
/*
 * Input a (variable-length, by definition) sequence of short
 * characters; a terminating NUL is appended.  (s) and (len) are OUT
 * parameters, through which the length and base pointer will be
 * returned.  When length is 0, a non-null base pointer is returned.
 */

ILU_PUBLIC void 
  ilu_InputEString(ilu_Call call, ilu_bytes * s,
		   ilu_cardinal *nbytes, ilu_cardinal limit,
		   ilu_cardinal expected_encoding,
		   ilu_cardinal *actual_encoding,
		   ILU_ERRS((IoErrs)) * err);
/*
 * Input a (variable-length, by definition) sequence of characters and
 * store its character set in (*actual_encoding).  If
 * (expected_encoding) isn't (0), that should be the encoding
 * returned; otherwise, any encoding is acceptable.  A terminating NUL
 * is appended, even when it doesn't make sense for the encoding, but
 * is not included in the value assigned to "nbytes", which is the
 * number of bytes in the string, not characters.  (s) and (nbytes)
 * are OUT parameters, through which the length and base pointer will
 * be returned.  When length is 0, a non-null base pointer is
 * returned.  The caller owns the returned value (*s).  */

ILU_PUBLIC void 
ilu_InputStringVec(ilu_Call call, ilu_string * s, ilu_cardinal len,
		   ILU_ERRS((IoErrs)) * err);
/*
 * Input a (fixed-length, by definition) array of short characters.
 * Caller may pass non-null (*s), in which case the characters will
 * be stored there; otherwise, callee will allocate the necessary
 * memory, and store a pointer to it in (*s).
 */

ILU_PUBLIC void 
ilu_InputWString(ilu_Call call, ilu_wstring * s,
		 ilu_cardinal * len, ilu_cardinal limit,
		 ILU_ERRS((IoErrs)) * err);
/*
 * Input a (variable-length, by definition) sequence of characters;
 * a terminating NUL is appended.  (s) and (len) are OUT parameters,
 * through which the length and base pointer will be returned.  When
 * length is 0, a non-null base pointer is returned.
 */

ILU_PUBLIC void 
ilu_InputWStringVec(ilu_Call call, ilu_wstring * s,
		    ilu_cardinal len, ILU_ERRS((IoErrs)) * err);
/*
 * Input a (fixed-length, by definition) array of characters.
 * Caller may pass non-null (*s), in which case the characters will
 * be stored there; otherwise, callee will allocate the necessary
 * memory, and store a pointer to it in (*s).
 */

ILU_PUBLIC void 
ilu_InputOpaque(ilu_Call call, ilu_opaque * o,
		ilu_cardinal len, ILU_ERRS((IoErrs)) * err);
/*
 * Input a (fixed-length, by definition) array of bytes.  Caller may
 * pass non-null (*s), in which case the bytes will be stored there;
 * otherwise, callee will allocate the necessary memory, and store a
 * pointer to it in (*s).
 */

ILU_PUBLIC void 
ilu_InputBytes(ilu_Call call, ilu_bytes * o,
	       ilu_cardinal * len, ilu_cardinal limit,
	       ILU_ERRS((IoErrs)) * err);
/*
 * Input a (variable-length, by definition) sequence of bytes; a
 * terminating NUL is appended.  (o) and (len) are OUT parameters,
 * through which the length and base pointer will be returned.  When
 * the length is 0, NULL might be returned as the base poiner.
 */

#ifdef ILU_FIXED_POINT_SUPPORT

ILU_PUBLIC void 
  ilu_InputFixedpoint (ilu_Call,
		       ilu_Bignum *,	/* numerator, PASS, ((ilu_Bignum)0) => NaN, ((ilu_Bignum)1) => Infinity */
		       ilu_Bignum,	/* min_numerator value, OPTIONAL, RETAIN */
		       ilu_Bignum,	/* max_numerator value, OPTIONAL, RETAIN */
		       ilu_Bignum,	/* denominator value, RETAIN */
		       ilu_cardinal,		/* if non-zero, digits as CORBA 'fixed' */
		       ilu_cardinal,		/* decimal places as CORBA 'fixed' */
		       ilu_FixedPointRangeSize,	
		       ILU_ERRS((IoErrs)) *);

#endif /* def ILU_FIXED_POINT_SUPPORT */

#ifdef ILU_HTTPNG_OBJECTS

/**Main Remnant holds, Call-IHi(call);
   before: L1 = {}
*/
ILU_PUBLIC ilu_Class	/* non-OPTIONAL */
  ilu_BeginInputObject(ilu_Call call,
		       ilu_boolean discriminator_p,
		       ilu_Class static_type,
		       ilu_cardinal *nstates,
		       ILU_ERRS((IoErrs)) * err);
/* Begins process of unmarshalling object ref.  Returns most-specific-known type of object. */

ILU_PUBLIC ilu_string
  ilu_BeginInputState (ilu_Call call, ILU_ERRS((IoErrs)) * err);
/*
  Begins process of unmarshalling state of some object type. Called
  "nstate" times, where "nstate" is returned from ilu_BeginInputObject.
  Followed by either calls to input state attributes and final call
  to ilu_FinishInputState, or by call to ilu_SkipInputState.
  */

ILU_PUBLIC void
  ilu_SkipInputState (ilu_Call call,
		      ILU_ERRS((IoErrs)) * err);
/* Finishes process of unmarshalling state for some object type. */

ILU_PUBLIC void
  ilu_FinishInputState (ilu_Call call,
			ILU_ERRS((IoErrs)) * err);
/* Finishes process of unmarshalling state for some object type. */

/* Main Remnant holds, Call-IHi(call);
   after:  result!=NIL => Inside(result's server, static_type);
   after:  result==NIL => L1 = {};
   after:  ILU_ERRNOK(*err) => result==NIL*/
ILU_PUBLIC ilu_Object	/* OPTIONAL */
  ilu_FinishInputObject(ilu_Call call,
  ilu_boolean discriminator_p,
  ilu_Class static_type,
  ILU_ERRS((IoErrs)) * err);
/* Afterward, if *o!=NIL && ilu_GetLanguageSpecificObject(*o)==NIL,
   the caller will invoke ilu_RegisterLSO
   on *o before unlocking the server. */

#endif /* def ILU_HTTPNG_OBJECTS */

#ifdef ILU_REFERENCE_TYPES

ILU_PUBLIC ilu_cardinal
  ilu_InputReference (ilu_Call call, ilu_boolean *present,
		      ilu_ReferenceID *id,
		      ILU_ERRS((IoErrs)) *err);
/* This is only called for optional or aliased reference types.
   Non-optional, non-aliased reference types simply input the value.
   For optional ref types, "present" returns ilu_TRUE if a value
   follows, ilu_FALSE otherwise.  For non-optional ref types,
   "present" always returns TRUE.  For aliased types, the first time
   the value is present in the scope, "id"
   will be 0, and the return value will be non-zero.  The caller
   should unmarshal the value, then call "ilu_EndInputReference"
   with the returned value from ilu_InputReference and the new
   reference ID of the unmarshalled value.  The second time an
   aliased value appears on the wire, the return value will be 0,
   and "id" will contain the reference ID passed in the previous
   call to ilu_EndInputReference.
*/

ILU_PUBLIC void
  ilu_EndInputReference (ilu_Call, ilu_cardinal wire_id,
			 ilu_ReferenceID id,
			 ilu_Error *);

#endif /* def ILU_REFERENCE_TYPES */

/**Main Remnant holds, Call-IHi(call);
   before: L1 = {},
   after:  *o!=NIL => Inside(*o's server, static_type);
   after:  *o==NIL => L1 = {};
   after:  ILU_ERRNOK(*err) => *o==NIL*/
ILU_PUBLIC void
ilu_InputObjectID(ilu_Call call, ilu_Object * o,
		  ilu_boolean discriminator_p, ilu_Class static_type,
		  ILU_ERRS((IoErrs)) * err);
/* static_type is not NIL.
   Afterward, if *o!=NIL && ilu_GetLanguageSpecificObject(*o)==NIL,
   the caller will invoke ilu_RegisterLSO
   on *o before unlocking the server. */

/* Size-computing routines */
/*L1, L2 unconstrained*/

ILU_PUBLIC ilu_cardinal ilu_SizeOfShortInteger(ilu_Call call,
					   ilu_shortinteger i,
					   ILU_ERRS((IoErrs)) *err);
ILU_PUBLIC ilu_cardinal ilu_SizeOfInteger(ilu_Call call,
				      ilu_integer i,
				      ILU_ERRS((IoErrs)) *err);
ILU_PUBLIC ilu_cardinal ilu_SizeOfLongInteger(ilu_Call call,
					  ilu_longinteger i,
					  ILU_ERRS((IoErrs)) *err);
ILU_PUBLIC ilu_cardinal ilu_SizeOfShortCardinal(ilu_Call call,
					    ilu_shortcardinal i,
					    ILU_ERRS((IoErrs)) *err);
ILU_PUBLIC ilu_cardinal ilu_SizeOfCardinal(ilu_Call call,
				       ilu_cardinal i,
				       ILU_ERRS((IoErrs)) *err);
ILU_PUBLIC ilu_cardinal ilu_SizeOfLongCardinal(ilu_Call call,
					   ilu_longcardinal i,
					   ILU_ERRS((IoErrs)) *err);
ILU_PUBLIC ilu_cardinal ilu_SizeOfShortReal(ilu_Call call,
					float d, ILU_ERRS((IoErrs)) *err);
ILU_PUBLIC ilu_cardinal ilu_SizeOfReal(ilu_Call call, double d,
				   ILU_ERRS((IoErrs)) *err);
ILU_PUBLIC ilu_cardinal ilu_SizeOfLongReal(ilu_Call call,
				       ilu_longreal d,
				       ILU_ERRS((IoErrs)) *err);
ILU_PUBLIC ilu_cardinal ilu_SizeOfEnum(ilu_Call call,
				       ilu_shortcardinal i,
				       ilu_Type the_type,
				       ILU_ERRS((IoErrs)) *err);
ILU_PUBLIC ilu_cardinal ilu_SizeOfCharacter(ilu_Call call,
					ilu_character i,
					ILU_ERRS((IoErrs)) *err);
ILU_PUBLIC ilu_cardinal ilu_SizeOfByte(ilu_Call call, ilu_byte i,
				   ILU_ERRS((IoErrs)) *err);
ILU_PUBLIC ilu_cardinal ilu_SizeOfShortCharacter(ilu_Call call, ilu_shortcharacter i,
					     ILU_ERRS((IoErrs)) *err);
ILU_PUBLIC ilu_cardinal ilu_SizeOfBoolean(ilu_Call call, ilu_boolean i,
				      ILU_ERRS((IoErrs)) *err);
ILU_PUBLIC ilu_cardinal ilu_SizeOfOptional(ilu_Call call,
					   ilu_boolean optionalStatus,
					   ilu_Type the_type,
					   ILU_ERRS((IoErrs)) *err);
ILU_PUBLIC ilu_cardinal ilu_SizeOfSequence(ilu_Call call, ilu_cardinal len,
					   ilu_cardinal limit,
					   ilu_Type the_type,
					   ILU_ERRS((IoErrs)) *err);
ILU_PUBLIC ilu_cardinal ilu_SizeOfUnion(ilu_Call call,
					ilu_cardinal discriminator,
					ilu_TypeKind discriminator_kind,
					ilu_Type the_type,
					ILU_ERRS((IoErrs)) *err);
ILU_PUBLIC ilu_cardinal ilu_SizeOfArray(ilu_Call call,ilu_cardinal length,
					ilu_Type the_type,
					ILU_ERRS((IoErrs)) *err);
ILU_PUBLIC ilu_cardinal ilu_SizeOfRecord(ilu_Call call,
					 ilu_Type the_type,
					 ILU_ERRS((IoErrs)) *err);

ILU_PUBLIC ilu_cardinal ilu_SizeOfString(ilu_Call call, ilu_string i,
					 ilu_cardinal l, ilu_cardinal limit,
					 ILU_ERRS((IoErrs)) *err);
ILU_PUBLIC ilu_cardinal ilu_SizeOfEString(ilu_Call call, ilu_bytes i,
					  ilu_cardinal l, ilu_cardinal limit,
					  ilu_cardinal expected_encoding,
					  ilu_cardinal current_encoding,
					  ILU_ERRS((IoErrs)) *err);
ILU_PUBLIC ilu_cardinal ilu_SizeOfStringVec(ilu_Call call, ilu_string i,
				        ilu_cardinal l,
				        ILU_ERRS((IoErrs)) *err);
ILU_PUBLIC ilu_cardinal ilu_SizeOfWString(ilu_Call call, ilu_wstring i,
					  ilu_cardinal l, ilu_cardinal limit,
					  ILU_ERRS((IoErrs)) *err);
ILU_PUBLIC ilu_cardinal ilu_SizeOfWStringVec(ilu_Call call, ilu_wstring i,
					 ilu_cardinal l,
					 ILU_ERRS((IoErrs)) *err);

ILU_PUBLIC ilu_cardinal ilu_SizeOfOpaque(ilu_Call call, ilu_opaque o,
					 ilu_cardinal l,
					 ILU_ERRS((IoErrs)) *err);

ILU_PUBLIC ilu_cardinal ilu_SizeOfBytes(ilu_Call call, ilu_bytes o,
					ilu_cardinal l, ilu_cardinal limit,
					ILU_ERRS((IoErrs)) *err);

#ifdef ILU_FIXED_POINT_SUPPORT

ILU_PUBLIC ilu_cardinal
  ilu_SizeOfFixedpoint (ilu_Call,
			ilu_Bignum,	/* numerator, RETAIN, ((ilu_Bignum)0) => NaN, ((ilu_Bignum)1) => Infinity */
			ilu_Bignum,	/* min_numerator value, OPTIONAL, RETAIN */
			ilu_Bignum,	/* max_numerator value, OPTIONAL, RETAIN */
			ilu_Bignum,	/* denominator value, RETAIN */
			ilu_cardinal,	/* if non-zero, digits as CORBA 'fixed' */
			ilu_cardinal,	/* decimal places as CORBA 'fixed' */
			ilu_FixedPointRangeSize,	
			ILU_ERRS((IoErrs)) *);

#endif /* def ILU_FIXED_POINT_SUPPORT */

#ifdef ILU_HTTPNG_OBJECTS

/**Main Remnant holds.
   obj!=NIL => L1 = {obj's server};
   obj==NIL => L1 = {}*/

ILU_PUBLIC ilu_cardinal
  ilu_BeginSizeOfObject(ilu_Call call,
  ilu_Object obj,
  ilu_boolean discriminator_p,
  ilu_Class static_type,
  ilu_cardinal nstates,
  ILU_ERRS((IoErrs)) *err);

ILU_PUBLIC ilu_cardinal
  ilu_BeginSizeOfState(ilu_Call call,
  ilu_Object obj,
  ilu_Class state_class,
  ILU_ERRS((IoErrs)) *err);

ILU_PUBLIC ilu_cardinal
  ilu_FinishSizeOfState(ilu_Call call,
  ilu_Object obj,
  ilu_Class state_class,
  ILU_ERRS((IoErrs)) *err);

ILU_PUBLIC ilu_cardinal
  ilu_FinishSizeOfObject(ilu_Call call,
  ilu_Object obj,
  ilu_boolean discriminator_p,
  ilu_Class static_type,
  ILU_ERRS((IoErrs)) *err);

#endif /* def ILU_HTTPNG_OBJECTS */

#ifdef ILU_REFERENCE_TYPES

ILU_PUBLIC ilu_cardinal
  ilu_SizeOfReference (ilu_Call call, ilu_boolean providedp,
		       ilu_boolean *first, ilu_ReferenceID id,
		       ILU_ERRS((IoErrs)) *err);

#endif /* def ILU_REFERENCE_TYPES */

/**Main Remnant holds.
   obj!=NIL => L1 = {obj's server};
   obj==NIL => L1 = {}*/
ILU_PUBLIC ilu_cardinal ilu_SizeOfObjectID(ilu_Call call, ilu_Object obj,
				       ilu_boolean discriminator_p,
				       ilu_Class static_type,
				       ILU_ERRS((IoErrs)) *err);

/* ================ Simple Binding ================ */

/*before: Inside(s, cl)
  after:				 L1 disjoint {cmu, s};
  after: cl collectible		      => L1  not >=  {gcmu};
  after: cl collectible & s surrogate => Main Invariant holds;
  where s = obj's server and cl = obj's type.
  (We don't really need to hold cmu for surrogate or non-collectible
   objects, but this is convenient because ilu_Enter/ExitServer can
   be used.)*/
ILU_PUBLIC /* OPTIONAL */ char *ilu_PublishObject(ilu_Object obj);
/*
 * Publishes the SBH of the object in the local object domain.
 * Returns an "ownership proof", a string which must be supplied to
 * withdraw the object.  Caller owns result.
 */

/*before: Inside(s, cl)
  after:				 L1 disjoint {cmu, s};
  after: cl collectible		      => L1  not >=  {gcmu};
  after: cl collectible & s surrogate => Main Invariant holds;
  where s = obj's server and cl = obj's type.
  (We don't really need to hold cmu for surrogate or non-collectible
   objects, but this is convenient because ilu_Enter/ExitServer can
   be used.)*/
ILU_PUBLIC          ilu_boolean
ilu_WithdrawObject(ilu_Object obj,
		    /* PASS */ char *ownership_proof);
/*
 * Withdraws the object "obj", if "ownership_proof" is that
 * returned when the object was registered.
 */

/*before: L1 = {};
  after:  result!=NIL => Inside(result's server, pclass);
  after:  result==NIL => L1 = {};
  forall conn: (L2 >= {conn.iomu}) => (L2 >= {conn.callmu});
  Main otherwise unconstrained */
ILU_PUBLIC /* OPTIONAL */ ilu_Object
ilu_LookupObject(char *sid, char *ih,
		 ilu_Class pclass);
/*
 * Attempts to find in the local domain the object identified by the
 * given server ID and server-relative Instance Handle.  "pclass" is
 * a type the caller knows the object to have.  Returns NIL on
 * failure.  Causes the kernel to reconsider which
 * contact info it wants to use for the identified server.
 */

/*before: L1 = {};
  after:  result!=NIL => Inside(result's server, pclass);
  after:  result==NIL => L1 = {};
  forall conn: (L2 >= {conn.iomu}) => (L2 >= {conn.callmu});
  Main otherwise unconstrained */
ILU_PUBLIC          ilu_boolean
ilu_ReLookupObject(char *sid, char *ih,
		   ilu_Class pclass,
		    /* OPTIONAL */ ilu_Object * po);
/*
 * Like ilu_LookupObject, but result indicates whether the kernel
 * changed its choice of contact info.  The object (or NIL) is
 * returned at *po; po may not be NIL, but *po need not be
 * initialized in any way.
 */

/* ================ Identities and Passports ================ */

/* L1, L2 ?? */

struct _ilu_IdentityInfo_s {
  ilu_IdentityType ii_type;
  ilu_boolean ii_owned_by_passport;
  ilu_refany ii_info;
};

struct _ilu_IdentityType_s {

  char *it_name;	/* constant string */

  ilu_cardinal				/* size of return string */
    (*it_string_form) (ilu_refany,	/* instance info, retain */
		       char *,		/* address of caller buffer */
		       ilu_cardinal,	/* size of caller buffer */
		       ILU_ERRS((internal, bad_param)) *);
  /* formats a string form of the IdentityInfo into the caller-supplied
     buffer.  If the buffer is too small for the identity, it may either
     truncate the identity and return success, or signal an error */

  ilu_refany
    (*it_duplicate_data) (ilu_refany,	/* instance data, retain */
			  ILU_ERRS((no_memory, internal)) *);
  /* returns a deep copy of the "ii_info" field of the argument. */

  void
    (*it_free_data) (ilu_refany,
		     ILU_ERRS((internal)) *);
  /* frees any associated data structure pointed to by the "ii_info" field */
      

  /*-------------------*/
  /* the following two methods may be either NULLFN, or defined.
     If not defined, this identity type can not be transported
     arbitrarily across the wire.  Otherwise, it can be, by protocols
     which do that sort of thing. */

  ilu_cardinal				/* size of pickled info */
    (*it_pickle) (ilu_refany,		/* instance, retain */
		  ilu_bytes *,		/* caller output buffer, retain */
		  ilu_cardinal,		/* size of caller output buffer */
		  ilu_Error *);
  /* writes identity info into buffer in way that can be recovered
     by a call on "it_unpickle", but is otherwise unconstrained.
     if the buffer is insufficient for pickling, should raise no_memory
     and return the suggested number of bytes to call this with,
     though that value need not be authoritative.  If the specified
     buffer is ILU_NIL, a buffer of sufficient length will be
     allocated and returned.
     The pickled form should always take fewer than 0x10000 bytes. */

  ilu_refany
    (*it_unpickle) (ilu_bytes,		/* buffer to unpickle, retain */
		    ilu_cardinal,	/* len of pickled form */
		    ilu_Error *);
  /* creates an identity info from the data in the buffer, and returns
     it. */
};

ILU_PUBLIC ilu_boolean
  ilu_RegisterIdentityType (ilu_IdentityType,
			    ilu_Error *);
/* make the identity type known to the ILU runtime, so that
   the generic identity operators (below) will work properly on it. */

/* 'standard' identity types */

ILU_PUBLIC struct _ilu_IdentityType_s	ilu_NoIdentity_s;
#define ilu_NoIdentity (&ilu_NoIdentity_s)
typedef void * ilu_NoIdentityInfo;

ILU_PUBLIC struct _ilu_IdentityType_s	ilu_ConnectionIdentity_s;
#define ilu_ConnectionIdentity (&ilu_ConnectionIdentity_s)
typedef ilu_string ilu_ConnectionIdentityInfo;

ILU_PUBLIC struct _ilu_IdentityType_s	ilu_OpaqueIdentity_s;
#define ilu_OpaqueIdentity (&ilu_OpaqueIdentity_s)
typedef struct _ilu_OpaqueIdentity_Data * ilu_OpaqueIdentityInfo;

ILU_PUBLIC ilu_IdentityInfo
  ilu_CreateOpaqueIdentity (ilu_string /* name */,
			    ilu_bytes  /* databytes */,	/* RETAIN */
			    ilu_cardinal /* datalen */,
			    ilu_Error * /* err */);
/* takes a sequence of bytes, and a name, and returns an identity */

ILU_PUBLIC ilu_string
  ilu_OpaqueIdentityName (ilu_IdentityInfo, ilu_Error *);
/* returns name in IdentityInfo as string */

ILU_PUBLIC ilu_cardinal
  ilu_OpaqueIdentityBytes (ilu_IdentityInfo,
			   ilu_bytes *,		/* OUT, PASS */
			   ilu_Error *);
/* returns the bytes of the Identity, and their length */

#ifdef SECURE_TRANSPORT

#include <gssapi.h>

ILU_PUBLIC struct _ilu_IdentityType_s	ilu_GSSIdentity_s;
#define ilu_GSSIdentity (&ilu_GSSIdentity_s)

typedef struct _ilu_GSSIdentityInfo_s * ilu_GSSIdentityInfo;

ILU_PUBLIC ilu_IdentityInfo
  ilu_AcquireGSSIdentity (gss_cred_id_t, ilu_Error *);

ILU_PUBLIC ilu_boolean
  ilu_DecodeGSSIdentity (ilu_IdentityInfo,	/* input; retain; info to decode */
			 gss_name_t *,		/* output; name in identity */
			 ilu_FineTime *,	/* output; good-till; seconds past Unix epoch */
			 gss_OID,		/* input; actual mechanism desired; optional */
			 ilu_boolean *,		/* if ilu_TRUE, local; otherwise remote */
			 ilu_cardinal *,	/* connection flags, as in gss_inquire_context */
			 ilu_Error *);

ILU_PUBLIC gss_cred_id_t
  ilu_AcquireGSSCredForName (char *,		/* name */
			     ilu_cardinal,	/* lifetime */
			     gss_OID,		/* secmech */
			     ilu_boolean,	/* accept_only */
			     ilu_Error *	/* err */);
ILU_PUBLIC ilu_string
  ilu_GSSNameToString (gss_name_t,
		       ilu_Error *err);

#endif /* def SECURE_TRANSPORT */

#ifdef SUNRPC_PROTOCOL

ILU_PUBLIC struct _ilu_IdentityType_s	ilu_SunRPCAuthUnixIdentity_s;
#define ilu_SunRPCAuthUnixIdentity (&ilu_SunRPCAuthUnixIdentity_s)

typedef struct {
  ilu_shortcardinal ii_UID;
  ilu_shortcardinal ii_GID;
  ilu_string      ii_hostname;
  ilu_shortcardinal ii_ngids;
  ilu_shortcardinal *ii_gids;
} *ilu_SunRPCAuthUnixIdentityInfo;

ILU_PUBLIC ilu_IdentityInfo
  ilu_GetSunRPCAuthUnixIdentityInfo (ilu_Error *);
/* returns identity info for the current user */

#endif				/* SUNRPC_PROTOCOL */

#ifdef W3MUX_TRANSPORT

ILU_PUBLIC struct _ilu_IdentityType_s	ilu_w3muxEndpointIdentity_s;
#define ilu_w3muxEndpointIdentity (&ilu_w3muxEndpointIdentity_s)

ILU_PUBLIC ilu_IdentityInfo
  ilu_AcquireW3muxEndpointIndentity (char *,	/* endpoint UUID, OPTIONAL, RETAIN */
				     ILU_ERRS((no_memory, bad_param)) *);
/* Returns Identity instance containing the specified W3MUX Endpoint ID.
   If no ID is specified, returns an identity containing the default endpoint ID
   for the address space.
*/

#endif

/*L1, L2 unconstrained*/

ILU_PUBLIC ilu_boolean
  ilu_RegisterIdentityType (struct _ilu_IdentityType_s *,	/* pass */
			    ilu_Error *);

ILU_PUBLIC ilu_IdentityType
  ilu_FindIdentityTypeByName (char *,
			      ilu_Error *);
/* returns the identity type specified by the first parm,
   or NIL if there is no identity type by that name */

ILU_PUBLIC ilu_Passport /* pass, optional */
  ilu_CreatePassport (const struct _ilu_IdentityInfo_s *,	/* optional, pass */
		      ILU_ERRS((no_memory)) *);
/* creates and returns a passport, optionally containing the specified identity */

ILU_PUBLIC ilu_IdentityInfo
  ilu_CopyIdentity (const struct _ilu_IdentityInfo_s *,
		    ILU_ERRS((no_memory)) *);
/* allocates and returns a copy of the ilu_IdentityInfo parameter */

ILU_PUBLIC ilu_boolean
  ilu_AddIdentity (ilu_Passport /* retain */,
		   const struct _ilu_IdentityInfo_s *,
		   ilu_Error *);
/* added identity to Passport.  Only one identity of each type is allowed.
   Returns ILU_ERROK() of the error parameter. */

ILU_PUBLIC ilu_IdentityInfo
  ilu_RemoveIdentity (ilu_Passport,
		      ilu_IdentityType,
		      ILU_ERRS((no_memory, bad_param)) *);
/* Removes identity of type "idtype" from Passport if present,
   and returns it.  Returns NIL if not present. */

ILU_PUBLIC ilu_boolean
  ilu_ReplaceIdentity (ilu_Passport,
		       const struct _ilu_IdentityInfo_s *,
		       ILU_ERRS((no_memory, bad_param)) *);
/* adds identity to Passport, replacing previous identity of the same type
   if necessary.  Only one identity of each type is allowed.
   Returns ILU_ERROK() of the error parameter. */

ILU_PUBLIC ilu_cardinal
  ilu_DisplayIdentity (struct _ilu_IdentityInfo_s *,	/* RETAIN */
		       char *,		/* buffer in which to put string */
		       ilu_cardinal,	/* length of buffer */
		       ilu_Error *);
/* return in buffer a string form for the identity */

ILU_PUBLIC ilu_IdentityInfo /* optional, retain */
  ilu_FindIdentity (ilu_Passport /* retain */,
		    ilu_IdentityType);
/* return identity of specified type, if present.  Returns NIL if not present. */

ILU_PUBLIC ilu_cardinal
  ilu_PickleIdentity (ilu_IdentityInfo,	/* in; retain */
		      ilu_bytes *,	/* inout buffer; retain or pass */
		      ilu_cardinal,	/* in; buffer len */
		      ilu_Error *);
  /* writes identity info into buffer in way that can be recovered
     by a call on "ilu_UnpickleIdentity", but is otherwise unconstrained.
     if the buffer is insufficient for pickling, should raise no_memory
     and return the suggested number of bytes to call this with,
     though that value need not be authoritative.  If the specified
     buffer is ILU_NIL, a buffer of sufficient length will be
     allocated and returned.
     The pickled form should always take fewer than 0x10000 bytes. */

ILU_PUBLIC ilu_IdentityInfo
  ilu_UnpickleIdentity (ilu_IdentityType,
			ilu_bytes,	/* in; retain; pickled data */
			ilu_cardinal,	/* in; pickled data len */
			ilu_Error *);
/* returns an ilu_IdentityInfo if the pickling succeeds,
   returns NIL if ilu_IdentityType doesn't defined an unpickling
   method, or if an error occurs. */

ILU_PUBLIC ilu_boolean
  ilu_FreeIdentity (struct _ilu_IdentityInfo_s *,
		    ilu_Error *);
/* Frees the identity and associated data owned by the identity,
   if the "ii_owned_by_passport" bit of the identity is clear;
   raises "no_permission" error otherwise.
   Returns ILU_ERROK() of the error parameter. */

ILU_PUBLIC ilu_boolean
  ilu_DestroyPassport (ilu_Passport /* pass */,
		       ilu_Error * /* retain */);
/* frees any associated identities which have the "ii_owned_by_passport"
   flag set, by calling the "it_free_data" function of the identity type on
   the "ii_info" field of the identity, then calling ilu_free() on the
   identity value struct itself; calls ilu_free() on the ilu_Passport arg. */

/* ================ Other routines ================ */

/* ================ URL Syntax ============ */

/*
   ILU regards string binding handles generically as a way of encoding
four pieces of information: the instance handle for an object, the
server ID for an object, the most-specific type ID (MSTID) for an
object, and communication information about how to communicate with that
object, which we call contact-info.  It further restricts them to
conform to the URL syntax specified in the World Wide Web Consortium and
IETF standard RFC 1738
(`http://www.w3.org/pub/WWW/Addressing/rfc1738.txt').  But this still
allows ILU to support any number of URL schemes, which we define as
some way of encoding these four pieces of information which conforms to
the URL syntax rules.

   The default URL scheme is called `ilu:', and encodes the information
as

   `ilu:<SERVER-ID>/<INSTANCE-HANDLE>;<MSTID>;<CONTACT-INFO>'

Most of these elements consist of US-ASCII strings with various
additional constraints.  The strings are encoded in what is called the
SBH element encoded form: the set of alphanumeric characters plus the 4
characters DOLLAR (`$'), HYPHEN (`-'), PERIOD (`.'), and PLUS (`+') are
represented by the character itself; other characters are escaped via
the mechanism specified in RFC 1738:  each is represented with 3
characters, a PERCENT (`%') character followed by two hexadecimal
digits giving the US-ASCII character code for the escaped character.

   The non-encoded form of the <SERVER-ID> and <INSTANCE-HANDLE>
strings may contain any character except for US-ASCII NUL.

   The non-encoded form of the <MSTID> consists of the following

   `<TYPE-ID-SCHEME>:<TYPE-ID>'

where the <TYPE-ID-SCHEME> consists of US-ASCII alphanumeric
characters, and any constraints on <TYPE-ID> are specified by the
<TYPE-ID-SCHEME>.

   The <CONTACT-INFO> is not encoded in the same way as the other
fields.  Rather, it consists of a series of communication info fields,
separated by SEMICOLON (`;') characters.  Each communications info
field has the form

   `<PROTOCOL-INFO>@<TRANSPORT-LAYER>[=<TRANSPORT-LAYER>...]'

where each of the <PROTOCOL-INFO> and <TRANSPORT-LAYER> elements
contain SBH element-encoded strings.  The non-encoded form of these
strings has an additional constraint:  each must begin with the name or
identifier for the protocol or transport layer it specifies, optionally
followed by an UNDERSCORE (`_') character and any parameters for the
protocol or transport.  The name of the protocol or transport may not
contain any UNDERSCORE (`_') characters.  There are no additional ILU
constraints on the formats used to represent parameters for the
protocol or transport.
*/


#define ILU_TYPE_MARKER		';'
#define ILU_CINFO_MARKER	';'
#define ILU_CINFO_DIVIDER	'@'
#define ILU_TINFO_DIVIDER	'='

/* ======================================== */

/*L1, L2 unconstrained*/

typedef 
ilu_boolean(*ilu_SBHParser) (ilu_string,	/* encoded SBH */
			     ilu_string *,	/* plain instance handle
						 * (opt) */
			     ilu_string *,	/* plain server ID (opt) */
			     ilu_string *,	/* plain MSTID (opt) */
			     ilu_string *,	/* encoded contact info
						 * (opt) */
			     ilu_cardinal *,	/* encoded contact info
						 * len (opt) */
			     ilu_boolean *,	/* ILU_PASS(cinfo)? */
		    ILU_ERRS((no_memory, internal, inv_objref)) *);

ILU_PUBLIC void
  ilu_RegisterSBHParser (ilu_string,		/* scheme name */
			 ilu_SBHParser);	/* parser for that scheme */

ILU_PUBLIC          ilu_boolean
  ilu_ParseSBH(ilu_string /* URL (encoded, of course) */ ,
	       ilu_string * /* plainInstH (opt) */ ,
	       ilu_string * /* plainServerID (opt) */ ,
	       ilu_string * /* plainMstid (opt) */ ,
	       ilu_string * /* encodedContactInfo (opt) */ ,
	       ilu_cardinal * /* encodedContactInfoLen (opt) */ ,
	       ilu_boolean * /* ILU_PASS(encodedContactInfoLen) */,
	       ILU_ERRS((no_memory, internal, inv_objref)) *);
/*
 * Parse an SBH (==ILU URL), returning whichever elements are
 * specified by passing in non-NIL pointers.  The whole sequence of
 * contact info.s is returned in *encodedContactInfo.  Caller
 * retains ownership of URL argument.  If ih != NIL, ownership of
 * *ih is passed to caller iff successful.  Similarly for
 * plainServerID and plainMstid.  If the boolean output parm
 * is ilu_FALSE, *encodedContactInfo is set to point
 * into the given URL, and *encodedContactInfoLen is set to the
 * length of the contact info.s substring; the next character is
 * left unmolested.  If the boolean output parm is ilu_TRUE, the
 * encodedContactInfo is malloc'ed, and the caller must arrange
 * for it to be freed.
 */

/*Main Invariant holds; L2 otherwise unconstrained*/
ILU_PUBLIC ilu_boolean 
ilu_PingObject(ilu_Object o,
	       ilu_Connection * new_conn);
/*
 * Returns ilu_TRUE if the true object exists, and the process
 * serving it can be contacted; ilu_FALSE otherwise.  May return a
 * new outgoing connection to monitor (a la ilu_StartCall).
 */

/*L1 >= {obj's server}; L1.sup < prmu*/
ILU_PUBLIC ilu_string ilu_SBHOfObject( ilu_Object obj );
/*
 * Ownership of result is retained by callee, which guarantees the
 * string to be valid only until the server mutex is exited.  May
 * return NIL if the object's server isn't exported through any
 * port; may return an invalid SBH if the cached one references a
 * closed port.
 */

#ifdef IIOP_PROTOCOL

/* (obj!=NIL) => Inside(object_server(obj), object_class(obj) */
ILU_PUBLIC ilu_string ilu_IOROfObject (ilu_Object /* obj */,
				       ilu_Error * /* errp */);
/* String result owned by caller.
 * Returns OMG IIOP-specified IOR string for object.  May return
 * NIL if object is not exported through an IIOP ilu_Port.
 */

/* (obj!=NIL) => Inside(object_server(obj), object_class(obj) */
ILU_PUBLIC ilu_string ilu_IOR2OfObject (ilu_Object /* obj */,
					ilu_Error * /* errp */);
/* String result owned by caller.
 * Returns DSTC IOR2 string for object.  May return
 * NIL if object is not exported through an IIOP ilu_Port.
 */

/* Locking unconstrained */
ILU_PUBLIC ilu_string
    ilu_IIOP_ServerIDFromObjectKey (ilu_bytes,		/* object key */
				    unsigned int,	/* len of object key */
				    ilu_string,		/* hostname */
				    unsigned int,	/* port */
				    unsigned int,	/* giop_major_version */
				    unsigned int,	/* giop_minor_version */
				    ilu_Error *);
/* Converts the CORBA object key, port, hostname, and GIOP versions
   to the ILU server ID which would be used with a "foreign" object
   key.  This can be used to create true servers which ILU doesn't
   recognize as ILU-ish, and thus treats in a strictly CORBA fashion.
   It should be used in conjunction with ilu_IIOP_IHFromObjectKey.
   The caller owns the newly-malloced returned string.
*/

/* Locking unconstrained */
ILU_PUBLIC ilu_string				/* PASS */
  ilu_IIOP_IHFromObjectKey (ilu_bytes,		/* object key */
			    ilu_cardinal,	/* length of object key */
			    ilu_Error *);

/* Converts the CORBA object key specified to the ILU instance handle
   which would be used with a "foreign" object key.  This can be used
   to create true instances which ILU doesn't recognize as ILU-ish,
   and thus treats in a strictly CORBA fashion.  It can also be used
   to create the instance handles for true instances which must be
   exported with a given object key, such as the "NameService" key for
   the root context of CosNaming.  The string return value is malloc'ed
   and owned by the caller.  Returns NIL on error.
 */
#endif /* IIOP_PROTOCOL */

#ifdef HTTP_PROTOCOL

/* no locking constraints */
ILU_PUBLIC ilu_string ilu_URLOfObject (ilu_Object /* obj */,
				       ilu_Error * /* errp */);
/* String result owned by caller. Returns HTTP URL for an object.   
 * May return NIL if object is not exported through an HTTP ilu_Port.
 * or if URL is not obtainable for some other reason.
 */

#endif /* HTTP_PROTOCOL */

#ifdef W3NG_PROTOCOL

/* Inside(object_server(obj), object_class(obj)) */
ILU_PUBLIC ilu_string
  ilu_w3ng_URLOfObject (ilu_Object, ilu_Error *);
/* String result owned by caller. Returns w3ng URL for an object.   
 * May return NIL if object is not exported through a w3ng ilu_Port.
 * or if URL is not obtainable for some other reason.
 */

#endif /* def W3NG_PROTOCOL */

ILU_PUBLIC ilu_string ilu_MstidOfObject( ilu_Object obj );
/*
 * Returns the ID of the most specific type of the given object.
 * Storage for result owned by the object
 * (i.e., freed when the object is freed).  Caller should thus hold
 * obj's server in order to know the object won't be freed upon
 * return.
 */

/* L1, L2 unconstrained */
ILU_PUBLIC ilu_string /* PASS */
  ilu_FormSBH (ilu_string,		/* server id, RETAIN */
	       ilu_string,		/* instance handle, RETAIN */
	       ilu_string,		/* mstid, RETAIN */
	       ilu_ProtocolInfo,	/* pinfo, RETAIN */
	       ilu_TransportInfo,	/* tinfo, RETAIN */
	       ILU_ERRS((no_memory)) *err);
/*
 * Forms a valid string binding handle from the information supplied,
 * and returns a copy of it.
 */

ILU_PUBLIC ilu_string ilu_GetILUVersion(void);

ILU_PUBLIC ilu_cardinal ilu_GetILUMajorVersion(void);

ILU_PUBLIC ilu_cardinal ilu_GetILUMinorVersion(void);

ILU_PUBLIC ilu_string ilu_GetILUTypeUIDVersion(void);

ILU_PUBLIC ilu_cardinal ilu_IDOfMethod(ilu_Method method);

ILU_PUBLIC ilu_cardinal ilu_ExceptionCountOfMethod(ilu_Method method);

ILU_PUBLIC ilu_string ilu_NameOfMethod(ilu_Method method);

ILU_PUBLIC void ilu_SetMethodStubProc(ilu_Method , ilu_StubProc, ilu_LanguageIndex);

ILU_PUBLIC ilu_StubProc ilu_GetMethodStubProc(ilu_Method, ilu_LanguageIndex);

ILU_PUBLIC ilu_Method ilu_FindMethodByID( ilu_Class /* intro_type */,
					  ilu_cardinal /* ID */);

ILU_PUBLIC ilu_Method ilu_MethodOfCall(ilu_Call);

ILU_PUBLIC ilu_Connection ilu_ConnectionOfCall(ilu_Call);

ILU_PUBLIC ilu_boolean ilu_CallNeedsSizing(ilu_Call);
/* returns ilu_TRUE if this call requires accurate argument sizes
   to be furnished for ilu_StartCall, ilu_BeginReply, or
   ilu_BeginException.  If ilu_FALSE, 0 may be passed as an
   argument size. */

ILU_PUBLIC ilu_Class ilu_IntroTypeOfCall(ilu_Call);

ILU_PUBLIC /* OPTIONAL */ ilu_Passport ilu_CallerPassportOfCall(ilu_Call call);
/* in true method stub, returns the caller's passport, if any.
   The reference is valid throughout the call */

ILU_PUBLIC void ilu_SetCallerPassportOfCall(ilu_Call, ilu_Passport);
/* Sets the passport of the call to the specified Passport.
   For use by client stubs. */

ILU_PUBLIC ilu_Server ilu_ServerOfConnection(ilu_Connection);

ILU_PUBLIC ilu_string ilu_IDOfServer( ilu_Server );

ILU_PUBLIC ilu_cardinal ilu_CRC32OfIDOfServer ( ilu_Server );

ILU_PUBLIC ilu_boolean ilu_TrueServerP( ilu_Server );
/* Is this server true for some language? */

ILU_PUBLIC ilu_boolean ilu_TrueServerForLanguageP( ilu_Server, ilu_LanguageIndex );
/* Is this server true for the specified language? */

/*L1, L2 unconstrained*/
/*(But be careful about holding directly onto an ilu_Object)*/

ILU_PUBLIC ilu_Class ilu_ClassOfObject( ilu_Object obj );

ILU_PUBLIC ilu_boolean ilu_TrueInstanceP( ilu_Object obj );
/* Is this object true for some language? */

ILU_PUBLIC ilu_boolean ilu_InstanceTrueForLangP( ilu_Object obj,
						ilu_LanguageIndex li );
/* Is this object true for the given language? */

ILU_PUBLIC ilu_Server ilu_ServerOfObject( ilu_Object obj );

ILU_PUBLIC ilu_string ilu_IhOfObject( ilu_Object o );
/* Return the ih of the given object; result is owned by the object.
   This could be useful when mapping a disassociated true KO
   to a LSO (ih is what's needed to consult the objtab). */

/*L1, L2 unconstrained*/

ILU_PUBLIC ilu_cardinal ilu_CRC32
  (ilu_bytes,		/* buffer to take the CRC-32 of, retain */
   ilu_cardinal);	/* length of buffer */

ILU_PUBLIC ilu_cardinal ilu_CRC32WithAccum
  (ilu_bytes,		/* buffer to take the CRC-32 of, retain */
   ilu_cardinal,	/* length of buffer */
   ilu_cardinal);	/* accumulated CRC returned from previous call to
			   ilu_CRC32 or ilu_CRC32WithAccum */

#define ilu_CRC32InitialAccum 0xFFFFFFFF

#define ilu_crc32(buf,len) \
ilu_CRC32WithAccum(buf, (len), ilu_CRC32InitialAccum)


/****************************** from sbilu.c ********************/

#ifdef ILU_BINDING_HOST

/* These are used by the sbilu server code, so we export them,
   but they should still be considered private to the ILU system! */

/*L1, L2 unconstrained */

ILU_PUBLIC ilu_boolean ilu_GetSimpleBindingSBH
  (char *,		/* buffer to receive SBH */
   ilu_cardinal		/* size of the buffer */
   );

ILU_PUBLIC ilu_boolean ilu_GetSBServiceParms
  (char *,		/* buffer to receive realm name (min 1100 bytes) */
   char *,		/* buffer to receive host name (min 1100 bytes) */
   ilu_shortcardinal *	/* 16-bit int pointer to receive port */
   );

#endif /* def ILU_BINDING_HOST */


/* ================ For transports & protocols ================ */

typedef struct {
  /* L1, L2 unconstrained */

  ilu_bytes       msg_base;
  ilu_cardinal    msg_len;
}               ilu_Message;
/* A contiguous copy of a whole message. */

/* Locking unconstrained */

ILU_PUBLIC /* READONLY */ ilu_ProtocolInfo
  ilu_DefaultProtocolInfo(void);
/* default protocol to use for intra-address-space calls, and if no protocol specified */

ILU_PUBLIC /* READONLY */ ilu_TransportInfo
  ilu_DefaultTransportInfo(void);
/* default transport info to use if no tinfo specified for a port */

ILU_PUBLIC /* READONLY */ ilu_TransportInfo
  ilu_LocalTransportInfo(void);
/* default transport info to use for intra-address-space calls */

/* ================ ilu_Call ================ */
/*
 * The structure of an ilu_Call is revealed here so that a stub or
 * LSR can stack-allocate one.  All of the following details are
 * private to the kernel, except: ca_private, ca_reqs_enabled,
 * and (sizeof (struct _ilu_Call_s)).
 */

typedef enum {
  ilu_ciosNone,			/* neither inputting nor outputting */
  ilu_ciosIn,			/* inputting */
  ilu_ciosOut			/* outputting */
}               ilu_CallIOState;
/*
 * Indicates where a call is in relation to the message framing
 * methods of its ilu_Protocol: tells whether pr_discard_output or
 * pr_discard_input should be called upon abort.
 */

typedef enum {
  ilu_cmsNo, ilu_cmsLo, ilu_cmsHi
}               ilu_CallMutexState;
/*
 * Which of its connection's L2 mutexes are held by a call?  See
 * "Locking" section for details.
 */

struct _ilu_Call_s {
  /* L1, L2 unconstrained */

  ilu_cardinal    ca_SN;	/* serial number of request */
  ilu_Server      ca_server;	/* of call */
  ilu_Class       ca_intro_type;/* type having ca_method */
  ilu_Method      ca_method;	/* ID number of method */
  ilu_Connection  ca_connection;/* connection which points to
				 * (server or client) */
  ilu_Passport    ca_caller;	/* slot for caller's identity
				 * passport */
  ilu_Passport    ca_callee;	/* slot for callee's identity (on
				 * reply) */
  ilu_Pipeline    ca_pl;	/* may be NIL */
  ilu_Serializer  ca_si;	/* may be NIL */
  ilu_LanguageIndex ca_lang;
  ilu_Call        ca_callmuDonor;
  ilu_Call        ca_callmuBorrower;
  ilu_Message     ca_msg;
  /*
   * When the transport is unreliable, a copy of the call message is
   * kept here for retransmissions.
   */
  ilu_ProtocolException ca_pe;	/* to return from server side */
  unsigned        ca_irq:1;	/* interrupt requested? */
  unsigned        ca_incoming:1;/* true on server side */
  unsigned        ca_tryIndex:4;/* 0 on first time */
  unsigned        ca_ios:2;	/* actually ilu_CallIOState */
  unsigned        ca_ms:2;	/* actually ilu_CallMutexState */
  unsigned        ca_msInput:1;	/* when cmsHi, waitmu held too? */
  unsigned        ca_disownWait:1;	/* ST && waitmu held when
					 * initted? */
  unsigned        ca_dontWait:1;/* IHi doesn't want waitmu (i.e.,
				 * decided to read queued reply) ? */
  unsigned        ca_prbit1:1;	/* for protocol's use */
  unsigned        ca_reqs_enabled:1;	/* for LSR use */
  ilu_cardinal    ca_prdata1;	/* for protocol's use */
  ilu_refany      ca_prdata2;	/* for protocol's use */
  ilu_Transport   ca_prTrans;	/* for protocol's use */
  ilu_refany      ca_private;	/* for LSR use */
};

/****************************** Hash Table ops ********************/

#include <iluhash.h>

#ifdef __cplusplus
}
#endif

#endif /* _ILU_EXPORTS_ */


