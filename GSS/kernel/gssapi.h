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

$Id: gssapi.h,v 1.13 1999/08/03 01:59:33 janssen Exp $
*/
#ifndef GSSAPI_H_
#define GSSAPI_H_

#ifdef __cplusplus
extern "C" {
#endif


/* define dllexport to support building DLLs on Win32 */
#if (defined(WIN32)||defined(_WIN32))
/* ensure WIN32 defined */
#ifndef WIN32
#define WIN32
#endif /* end ensure WIN32 defined */
#if defined(GSS_BUILDING_KERNEL)
#define GSS_PUBLIC __declspec(dllexport) extern
#else
#define GSS_PUBLIC __declspec(dllimport) extern
#endif /* defined(ILU_BUILDING_KERNEL) */
#include "ilugsswin_conf.h"
#else
#define GSS_PUBLIC extern
#include "ilugss_conf.h"
#endif /*(defined(WIN32)||defined(_WIN32)) */

/*
 * First, include stddef.h to get size_t defined.
 */
#include <stddef.h>

#ifdef HAVE_XOM_HEADER
/*
 * If the platform supports the xom.h header file, it should be
 * included here.
 */
#include <xom.h>
#endif


/* Figure out the size of 32 and 16 bit unsigned ints */
/* SIZEOF_SHORT, SIZEOF_INT, SIZEOF_LONG, and SIZEOF_CHAR are defined in gss_conf.h */

#if (SIZEOF_SHORT != 2)
#if (SIZEOF_INT != 2)
#if (SIZEOF_CHAR != 2)
#error "Can't figure a good type for gss_uint16!"
#else
typedef unsigned char gss_uint16;
#endif /* CHAR */
#else
typedef unsigned int gss_uint16;
#endif /* INT */
#else
typedef unsigned short gss_uint16;
#endif /* SHORT */

#if (SIZEOF_INT != 4)
#if (SIZEOF_SHORT != 4)
#if (SIZEOF_LONG != 4)
#error "Can't figure a good type for INT32_TYPE!"
#else
typedef unsigned long int gss_uint32;
#endif /* LONG */
#else
typedef unsigned short int gss_uint32;
#endif /* SHORT */
#else
typedef unsigned int gss_uint32;
#endif /* INT */

#ifdef OM_STRING
/*
 * We have included the xom.h header file.  Verify that OM_uint32
 * is defined correctly.
 */

#if sizeof(gss_uint32) != sizeof(OM_uint32)
#error Incompatible definition of OM_uint32 from xom.h
#endif

typedef OM_object_identifier gss_OID_desc, *gss_OID;

#else

/*
 * We can't use X/Open definitions, so roll our own.
 */

typedef gss_uint16 OM_uint16;
typedef gss_uint32 OM_uint32;

typedef struct gss_OID_desc_struct {
     OM_uint32 length;
     void      *elements;
} gss_OID_desc, *gss_OID;

#endif

/*
 * First, define the three platform-dependent pointer types.
 */
typedef struct gss_impl_ctx_id_s *gss_ctx_id_t;
typedef struct gss_impl_cred_id_s *gss_cred_id_t;
typedef struct gss_impl_name_s *gss_name_t;

typedef struct gss_OID_set_desc_struct  {
     size_t     count;
     gss_OID    elements;
} gss_OID_set_desc, *gss_OID_set;

typedef struct gss_buffer_desc_struct {
     size_t length;
     void *value;
} gss_buffer_desc, *gss_buffer_t;

typedef struct gss_channel_bindings_struct {
     OM_uint32 initiator_addrtype;
     gss_buffer_desc initiator_address;
     OM_uint32 acceptor_addrtype;
     gss_buffer_desc acceptor_address;
     gss_buffer_desc application_data;
} *gss_channel_bindings_t;


/*
 * For now, define a QOP-type as an OM_uint32 (pending resolution
 * of ongoing discussions).
 */
typedef OM_uint32 gss_qop_t;

typedef int gss_cred_usage_t;

/*
 * Flag bits for context-level services.
 */
#define GSS_C_DELEG_FLAG 1
#define GSS_C_MUTUAL_FLAG 2
#define GSS_C_REPLAY_FLAG 4
#define GSS_C_SEQUENCE_FLAG 8
#define GSS_C_CONF_FLAG 16
#define GSS_C_INTEG_FLAG 32
#define GSS_C_ANON_FLAG 64

/*
 * Credential usage options
 */
#define GSS_C_BOTH 0
#define GSS_C_INITIATE 1
#define GSS_C_ACCEPT 2

/*
 * Status code types for gss_display_status
 */
#define GSS_C_GSS_CODE 1
#define GSS_C_MECH_CODE 2

/*
 * The constant definitions for channel-bindings address families
 */
#define GSS_C_AF_UNSPEC     0
#define GSS_C_AF_LOCAL      1
#define GSS_C_AF_INET       2
#define GSS_C_AF_IMPLINK    3
#define GSS_C_AF_PUP        4
#define GSS_C_AF_CHAOS      5
#define GSS_C_AF_NS         6
#define GSS_C_AF_NBS        7
#define GSS_C_AF_ECMA       8
#define GSS_C_AF_DATAKIT    9
#define GSS_C_AF_CCITT      10
#define GSS_C_AF_SNA        11
#define GSS_C_AF_DECnet     12
#define GSS_C_AF_DLI        13
#define GSS_C_AF_LAT        14
#define GSS_C_AF_HYLINK     15
#define GSS_C_AF_APPLETALK  16
#define GSS_C_AF_BSC        17
#define GSS_C_AF_DSS        18
#define GSS_C_AF_OSI        19
#define GSS_C_AF_X25        21

#define GSS_C_AF_NULLADDR   255

/*
 * Various Null values
 */
#define GSS_C_NO_BUFFER ((gss_buffer_t) 0)
#define GSS_C_NO_OID ((gss_OID) 0)
#define GSS_C_NO_OID_SET ((gss_OID_set) 0)
#define GSS_C_NO_CONTEXT ((gss_ctx_id_t) 0)
#define GSS_C_NO_CREDENTIAL ((gss_cred_id_t) 0)
#define GSS_C_NO_CHANNEL_BINDINGS ((gss_channel_bindings_t) 0)
#define GSS_C_EMPTY_BUFFER {0, NULL}

/*
 * Some alternate names for a couple of the above
 * values.  These are defined for V1 compatibility.
 */
#define GSS_C_NULL_OID GSS_C_NO_OID
#define GSS_C_NULL_OID_SET GSS_C_NO_OID_SET

/*
 * Define the default Quality of Protection for per-message
 * services.  Note that an implementation that offers multiple
 * levels of QOP may either reserve a value (for example zero,
 * as assumed here) to mean "default protection", or
 * alternatively may simply equate GSS_C_QOP_DEFAULT to a specific
 * explicit QOP value.  However, a value of 0 should always be
 * interpreted by a GSSAPI implementation as a request for the
 * default protection level.
 */
#define GSS_C_QOP_DEFAULT 0

/*
 * Expiration time of 2^32-1 seconds means infinite lifetime for a
 * credential or security context
 */
#define GSS_C_INDEFINITE 0xfffffffful

/*
 * The implementation must reserve static storage for a
 * gss_OID_desc object containing the value
 * {10, (void *)"\x60\x86\x48\x01\x86\xf8\x17\x01\x02\x01"},
 * and GSS_C_NT_ANONYMOUS should be initialized to point
 * to that gss_OID_desc.
 */
extern gss_OID GSS_C_NT_ANONYMOUS;


/* Major status codes */

#define GSS_S_COMPLETE 0

/*
 * Some "helper" definitions to make the status code macros obvious.
 */
#define GSS_C_CALLING_ERROR_OFFSET 24
#define GSS_C_ROUTINE_ERROR_OFFSET 16
#define GSS_C_SUPPLEMENTARY_OFFSET 0
#define GSS_C_CALLING_ERROR_MASK 0377ul
#define GSS_C_ROUTINE_ERROR_MASK 0377ul
#define GSS_C_SUPPLEMENTARY_MASK 0177777ul

/*
 * The macros that test status codes for error conditions.
 * Note that the GSS_ERROR() macro has changed slightly from
 * the V1 GSSAPI so that it now evaluates its argument
 * only once.
 */
#define GSS_CALLING_ERROR(x) \
(x & (GSS_C_CALLING_ERROR_MASK << GSS_C_CALLING_ERROR_OFFSET))
#define GSS_ROUTINE_ERROR(x) \
(x & (GSS_C_ROUTINE_ERROR_MASK << GSS_C_ROUTINE_ERROR_OFFSET))
#define GSS_SUPPLEMENTARY_INFO(x) \
(x & (GSS_C_SUPPLEMENTARY_MASK << GSS_C_SUPPLEMENTARY_OFFSET))
#define GSS_ERROR(x) \
(x & ((GSS_C_CALLING_ERROR_MASK << GSS_C_CALLING_ERROR_OFFSET) | \
      (GSS_C_ROUTINE_ERROR_MASK << GSS_C_ROUTINE_ERROR_OFFSET)))


/*
 * Now the actual status code definitions
 */

/*
 * Calling errors:
 */
#define GSS_S_CALL_INACCESSIBLE_READ \
(1ul << GSS_C_CALLING_ERROR_OFFSET)
#define GSS_S_CALL_INACCESSIBLE_WRITE \
(2ul << GSS_C_CALLING_ERROR_OFFSET)
#define GSS_S_CALL_BAD_STRUCTURE \
(3ul << GSS_C_CALLING_ERROR_OFFSET)

/*
 * Routine errors:
 */
#define GSS_S_BAD_MECH (1ul << GSS_C_ROUTINE_ERROR_OFFSET)
#define GSS_S_BAD_NAME (2ul << GSS_C_ROUTINE_ERROR_OFFSET)
#define GSS_S_BAD_NAMETYPE (3ul << GSS_C_ROUTINE_ERROR_OFFSET)
#define GSS_S_BAD_BINDINGS (4ul << GSS_C_ROUTINE_ERROR_OFFSET)
#define GSS_S_BAD_STATUS (5ul << GSS_C_ROUTINE_ERROR_OFFSET)
#define GSS_S_BAD_SIG (6ul << GSS_C_ROUTINE_ERROR_OFFSET)
#define GSS_S_NO_CRED (7ul << GSS_C_ROUTINE_ERROR_OFFSET)
#define GSS_S_NO_CONTEXT (8ul << GSS_C_ROUTINE_ERROR_OFFSET)
#define GSS_S_DEFECTIVE_TOKEN (9ul << GSS_C_ROUTINE_ERROR_OFFSET)
#define GSS_S_DEFECTIVE_CREDENTIAL (10ul << GSS_C_ROUTINE_ERROR_OFFSET)
#define GSS_S_CREDENTIALS_EXPIRED (11ul << GSS_C_ROUTINE_ERROR_OFFSET)
#define GSS_S_CONTEXT_EXPIRED (12ul << GSS_C_ROUTINE_ERROR_OFFSET)
#define GSS_S_FAILURE (13ul << GSS_C_ROUTINE_ERROR_OFFSET)
#define GSS_S_BAD_QOP (14ul << GSS_C_ROUTINE_ERROR_OFFSET)
#define GSS_S_UNAUTHORIZED (15ul << GSS_C_ROUTINE_ERROR_OFFSET)
#define GSS_S_UNAVAILABLE (16ul << GSS_C_ROUTINE_ERROR_OFFSET)

/*
 * Supplementary info bits:
 */
#define GSS_S_CONTINUE_NEEDED (1ul << (GSS_C_SUPPLEMENTARY_OFFSET + 0))
#define GSS_S_DUPLICATE_TOKEN (1ul << (GSS_C_SUPPLEMENTARY_OFFSET + 1))
#define GSS_S_OLD_TOKEN (1ul << (GSS_C_SUPPLEMENTARY_OFFSET + 2))
#define GSS_S_UNSEQ_TOKEN (1ul << (GSS_C_SUPPLEMENTARY_OFFSET + 3))
#define GSS_S_GAP_TOKEN (1ul << (GSS_C_SUPPLEMENTARY_OFFSET + 4))


/*
 * Finally, function prototypes for the GSS-API routines.
 */

GSS_PUBLIC OM_uint32 gss_acquire_cred
(OM_uint32 *,        /* minor_status */
 gss_name_t,         /* desired_name */
 OM_uint32,          /* time_req */
 gss_OID_set,        /* desired_mechs */
 gss_cred_usage_t,   /* cred_usage */
 gss_cred_id_t *,    /* output_cred_handle */
 gss_OID_set *,      /* actual_mechs */
 OM_uint32 *         /* time_rec */
     );

GSS_PUBLIC OM_uint32 gss_release_cred
(OM_uint32 *,        /* minor_status */
 gss_cred_id_t *     /* cred_handle */
     );

GSS_PUBLIC OM_uint32 gss_init_sec_context
(OM_uint32 *,        /* minor_status */
 gss_cred_id_t,      /* initiator_cred_handle */
 gss_ctx_id_t *,     /* context_handle */
 gss_name_t,         /* target_name */
 gss_OID,            /* mech_type */
 OM_uint32,          /* req_flags */
 OM_uint32,          /* time_req */
 gss_channel_bindings_t,
 /* input_chan_bindings */
 gss_buffer_t,       /* input_token */
 gss_OID *,          /* actual_mech_type */
 gss_buffer_t,       /* output_token */
 OM_uint32 *,        /* ret_flags */
 OM_uint32 *         /* time_rec */
     );

GSS_PUBLIC OM_uint32 gss_accept_sec_context
(OM_uint32 *,        /* minor_status */
 gss_ctx_id_t *,     /* context_handle */
 gss_cred_id_t,      /* acceptor_cred_handle */
 gss_buffer_t,       /* input_token_buffer */
 gss_channel_bindings_t,
 /* input_chan_bindings */
 gss_name_t *,       /* src_name */
 gss_OID *,          /* mech_type */
 gss_buffer_t,       /* output_token */
 OM_uint32 *,        /* ret_flags */
 OM_uint32 *,        /* time_rec */
 gss_cred_id_t *     /* delegated_cred_handle */
     );

GSS_PUBLIC OM_uint32 gss_process_context_token
(OM_uint32 *,        /* minor_status */
 gss_ctx_id_t,       /* context_handle */
 gss_buffer_t        /* token_buffer */
     );

GSS_PUBLIC OM_uint32 gss_delete_sec_context
(OM_uint32 *,        /* minor_status */
 gss_ctx_id_t *,     /* context_handle */
 gss_buffer_t        /* output_token */
     );

GSS_PUBLIC OM_uint32 gss_context_time
(OM_uint32 *,        /* minor_status */
 gss_ctx_id_t,       /* context_handle */
 OM_uint32 *         /* time_rec */
     );

GSS_PUBLIC OM_uint32 gss_get_mic
(OM_uint32 *,        /* minor_status */
 gss_ctx_id_t,       /* context_handle */
 gss_qop_t,          /* qop_req */
 gss_buffer_t,       /* message_buffer */
 gss_buffer_t        /* message_token */
     );


GSS_PUBLIC OM_uint32 gss_verify_mic
(OM_uint32 *,        /* minor_status */
 gss_ctx_id_t,       /* context_handle */
 gss_buffer_t,       /* message_buffer */
 gss_buffer_t,       /* token_buffer */
 gss_qop_t *         /* qop_state */
     );

GSS_PUBLIC OM_uint32 gss_wrap
(OM_uint32 *,        /* minor_status */
 gss_ctx_id_t,       /* context_handle */
 int,                /* conf_req_flag */
 gss_qop_t,          /* qop_req */
 gss_buffer_t,       /* input_message_buffer */
 int *,              /* conf_state */
 gss_buffer_t        /* output_message_buffer */
     );

GSS_PUBLIC OM_uint32 gss_unwrap
(OM_uint32 *,        /* minor_status */
 gss_ctx_id_t,       /* context_handle */
 gss_buffer_t,       /* input_message_buffer */
 gss_buffer_t,       /* output_message_buffer */
 int *,              /* conf_state */
 gss_qop_t *         /* qop_state */
     );



GSS_PUBLIC OM_uint32 gss_display_status
(OM_uint32 *,        /* minor_status */
 OM_uint32,          /* status_value */
 int,                /* status_type */
 gss_OID,            /* mech_type */
 OM_uint32 *,        /* message_context */
 gss_buffer_t        /* status_string */
     );

GSS_PUBLIC OM_uint32 gss_indicate_mechs
(OM_uint32 *,        /* minor_status */
 gss_OID_set *       /* mech_set */
     );

GSS_PUBLIC OM_uint32 gss_compare_name
(OM_uint32 *,        /* minor_status */
 gss_name_t,         /* name1 */
 gss_name_t,         /* name2 */
 int *               /* name_equal */
     );

GSS_PUBLIC OM_uint32 gss_display_name
(OM_uint32 *,        /* minor_status */
 gss_name_t,         /* input_name */
 gss_buffer_t,       /* output_name_buffer */
 gss_OID *           /* output_name_type */
     );

GSS_PUBLIC OM_uint32 gss_import_name
(OM_uint32 *,        /* minor_status */
 gss_buffer_t,       /* input_name_buffer */
 gss_OID,            /* input_name_type */
 gss_name_t *        /* output_name */
     );

GSS_PUBLIC OM_uint32 gss_canonicalize_name
  (OM_uint32 *,		/* minor_status */
   gss_name_t,		/* input_name */
   gss_OID,		/* mech_type */
   gss_name_t *		/* output_name */
   );

GSS_PUBLIC OM_uint32 gss_inquire_mechs_for_name
  (OM_uint32 *,		/* minor_status */
   gss_name_t,		/* input_name */
   gss_OID_set *	/* mech_types */
   );

GSS_PUBLIC OM_uint32 gss_release_name
(OM_uint32 *,        /* minor_status */
 gss_name_t *        /* input_name */
     );

GSS_PUBLIC OM_uint32 gss_release_buffer
(OM_uint32 *,        /* minor_status */
 gss_buffer_t        /* buffer */
     );

GSS_PUBLIC OM_uint32 gss_release_oid_set
(OM_uint32 *,        /* minor_status */
 gss_OID_set *       /* set */
     );

GSS_PUBLIC OM_uint32 gss_inquire_cred
(OM_uint32 *,        /* minor_status */
 gss_cred_id_t,      /* cred_handle */
 gss_name_t *,       /* name */
 OM_uint32 *,        /* lifetime */
 gss_cred_usage_t *, /* cred_usage */
 gss_OID_set *       /* mechanisms */
     );

GSS_PUBLIC OM_uint32 gss_inquire_context (
     OM_uint32 *,        /* minor_status */
     gss_ctx_id_t,       /* context_handle */
     gss_name_t *,       /* src_name */
     gss_name_t *,       /* targ_name */
     OM_uint32 *,        /* lifetime_rec */
     gss_OID *,          /* mech_type */
     OM_uint32 *,        /* ctx_flags */
     int *,              /* locally_initiated */
     int *               /* open */
     );

GSS_PUBLIC OM_uint32 gss_wrap_size_limit (
     OM_uint32 *,        /* minor_status */
     gss_ctx_id_t,       /* context_handle */
     int,                /* conf_req_flag */
     gss_qop_t,          /* qop_req */
     OM_uint32,          /* req_output_size */
     OM_uint32 *         /* max_input_size */
     );

GSS_PUBLIC OM_uint32 gss_import_name_object (
     OM_uint32 *,        /* minor_status */
     void *,             /* input_name */
     gss_OID,            /* input_name_type */
     gss_name_t *       /* output_name */
     );

GSS_PUBLIC OM_uint32 gss_export_name_object (
     OM_uint32 *,        /* minor_status */
     gss_name_t,         /* input_name */
     gss_OID,            /* desired_name_type */
     void **            /* output_name */
     );

GSS_PUBLIC OM_uint32 gss_add_cred (
     OM_uint32 *,        /* minor_status */
     gss_cred_id_t,      /* input_cred_handle */
     gss_name_t,         /* desired_name */
     gss_OID,            /* desired_mech */
     gss_cred_usage_t,   /* cred_usage */
     OM_uint32,          /* initiator_time_req */
     OM_uint32,          /* acceptor_time_req */
     gss_cred_id_t *,    /* output_cred_handle */
     gss_OID_set *,      /* actual_mechs */
     OM_uint32 *,        /* initiator_time_rec */
     OM_uint32 *         /* acceptor_time_rec */
     );


GSS_PUBLIC OM_uint32 gss_inquire_cred_by_mech (
     OM_uint32  *,       /* minor_status */
     gss_cred_id_t,      /* cred_handle */
     gss_OID,            /* mech_type */
     gss_name_t *,       /* name */
     OM_uint32 *,        /* initiator_lifetime */
     OM_uint32 *,        /* acceptor_lifetime */
     gss_cred_usage_t *  /* cred_usage */
     );

GSS_PUBLIC OM_uint32 gss_export_sec_context (
     OM_uint32 *,        /* minor_status */
     gss_ctx_id_t *,     /* context_handle */
     gss_buffer_t        /* interprocess_token */
     );

GSS_PUBLIC OM_uint32 gss_import_sec_context (
     OM_uint32 *,        /* minor_status */
     gss_buffer_t,       /* interprocess_token */
     gss_ctx_id_t *      /* context_handle */
     );


GSS_PUBLIC OM_uint32 gss_release_oid (
     OM_uint32 *,        /* minor_status */
     gss_OID *           /* oid */
     );

GSS_PUBLIC OM_uint32 gss_create_empty_oid_set (
     OM_uint32 *,        /* minor_status */
     gss_OID_set *       /* oid_set */
     );

GSS_PUBLIC OM_uint32 gss_add_oid_set_member (
     OM_uint32 *,        /* minor_status */
     gss_OID,            /* member_oid */
     gss_OID_set *       /* oid_set */
     );

GSS_PUBLIC OM_uint32 gss_test_oid_set_member (
     OM_uint32 *,        /* minor_status */
     gss_OID,            /* member */
     gss_OID_set,        /* set */
     int *               /* present */
     );

GSS_PUBLIC OM_uint32 gss_str_to_oid (
     OM_uint32 *,        /* minor_status */
     gss_buffer_t,       /* oid_str */
     gss_OID *           /* oid */
     );

GSS_PUBLIC OM_uint32 gss_oid_to_str (
     OM_uint32 *,        /* minor_status */
     gss_OID,            /* oid */
     gss_buffer_t        /* oid_str */
     );

GSS_PUBLIC OM_uint32 gss_inquire_names_for_mech (
     OM_uint32 *,        /* minor_status */
     gss_OID,            /* mechanism */
     gss_OID_set *       /* name_types */
     );

/*
 * The following routines are obsolete variants of gss_get_mic,
 * gss_wrap, gss_verify_mic and gss_unwrap.  They should be
 * provided by GSSAPI V2 implementations for backwards
 * compatibility with V1 applications.  Distinct entrypoints
 * (as opposed to #defines) should be provided, to allow GSSAPI
 * V1 applications to link against GSSAPI V2 implementations.
 */

GSS_PUBLIC OM_uint32 gss_sign
(OM_uint32 *,        /* minor_status */
 gss_ctx_id_t,       /* context_handle */
 int,                /* qop_req */
 gss_buffer_t,       /* message_buffer */
 gss_buffer_t        /* message_token */
     );


GSS_PUBLIC OM_uint32 gss_verify
(OM_uint32 *,        /* minor_status */
 gss_ctx_id_t,       /* context_handle */
 gss_buffer_t,       /* message_buffer */
 gss_buffer_t,       /* token_buffer */
 int *               /* qop_state */
     );

GSS_PUBLIC OM_uint32 gss_seal
(OM_uint32 *,        /* minor_status */
 gss_ctx_id_t,       /* context_handle */
 int,                /* conf_req_flag */
 int,                /* qop_req */
 gss_buffer_t,       /* input_message_buffer */
 int *,              /* conf_state */
 gss_buffer_t        /* output_message_buffer */
     );


GSS_PUBLIC OM_uint32 gss_unseal
(OM_uint32 *,        /* minor_status */
 gss_ctx_id_t,       /* context_handle */
 gss_buffer_t,       /* input_message_buffer */
 gss_buffer_t,       /* output_message_buffer */
 int *,              /* conf_state */
 int *               /* qop_state */
     );


#ifdef __cplusplus
}
#endif


#endif /* GSSAPI_H_ */
