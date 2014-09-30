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

$Id: ilugss_ext.h,v 1.8 1999/08/03 01:59:30 janssen Exp $
*/
#ifndef GSS_EXT_H
#define GSS_EXT_H 1
/*
 * gss_ext.h -- general-purpose extensions to GSS API for use at PARC
 *
 * Antony Courtney,	8/4/95
 */

/* gss_pkcs_decrypt_key() -- decrypt a private key which is stored in
 * PKCS #8 encrypted form
 */
OM_uint32 gss_pkcs_decrypt_key
(OM_uint32 *minor_status,
 gss_buffer_t input_buffer,		/* encrypted */
 gss_buffer_t pass_buffer,		/* pass phrase */
 gss_buffer_t output_buffer		/* decrypted */
);

/* ilu_gssext_strip_initial_token() -- strip GSS-mandated object-identifier from
 * 				initial token, for wire-compatibility with
 *				non-GSS based protocols
 *
 * N.B. The original token_buffer is assumed to have been allocated by the
 * GSS implementation; the original storage is released and new storage is
 * allocated
 */
OM_uint32 ilu_gssext_strip_initial_token
(OM_uint32 *minor_status,
 gss_buffer_t token_buffer
);
 
/* ilu_gssext_insert_mech_oid() -- insert GSS-mandated mechanism object-identifier
 *			    into initial context token received from remote
 *			    peer, so that it may be passed to
 *			    gss_accept_sec_context()
 */
OM_uint32 ilu_gssext_insert_mech_oid
(OM_uint32 *minor_status,
 gss_buffer_t input_buffer,
 gss_OID mech_oid,
 gss_buffer_t output_buffer
);


/* gss_find_token() -- find a token boundary in an input buffer
 * if *token_read is TRUE, at least a full token was read, and *nbytes is
 * the number of bytes in input_buffer beyond the end of a full token;
 * if *token_read is FALSE, a full token has not been read and *nbytes is
 * a hint about how many more bytes will be required before a full token
 * is read
 */
OM_uint32 gss_find_token
(OM_uint32 *minor_status,
 gss_OID mech,
 gss_buffer_t input_buffer,
 int *token_read,	/* boolean */
 OM_uint32 *nbytes
);

/* Given the name of a security scheme, such as "nil" or "ssl",
   returns the OID of that security scheme */
extern OM_uint32
  gss__find_scheme_by_name (OM_uint32 * /* OUT:  minor_status */,
			    char * /* IN:  name of security scheme */,
			    gss_OID * /* OUT: OID of specified scheme */);

/* Given the name of a namespace, such as "x509" or "rfc822",
   returns the OID of that namespace */
extern OM_uint32
  gss__find_namespace_by_name (OM_uint32 * /* OUT: minor_status */,
			       char *	   /* IN: name of namespace */,
			       gss_OID *   /* OUT: OID of namespace */);

/* Adds a scheme to the registry of scheme names. */
extern OM_uint32
  ilu_gssext_add_scheme_by_name (OM_uint32 *,	/* OUT: minor_status */
				 char *,	/* IN, RETAIN:  name */
				 gss_OID	/* IN: oid	*/
				 );
/* Adds a namespace to the registry of scheme names. */
extern OM_uint32
  ilu_gssext_add_namespace_by_name (OM_uint32 *,/* OUT: minor_status */
				    char *,	/* IN, RETAIN:  name */
				    gss_OID	/* IN: oid	*/
				    );
#endif /* GSS_EXT_H */
