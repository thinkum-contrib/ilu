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
/* $Id: gss.c,v 1.54 1999/09/20 22:49:33 janssen Exp $ */
/* Last edited by Mike Spreitzer September 19, 1998 0:06 am PDT */

#include <ctype.h>		/* for isdigit() */

#include "iluntrnl.h"

#ifdef SECURE_TRANSPORT

#include "ilutransport.h"
#include "mooring.h"

#include "oscalls.h"

typedef OM_uint32	gss_status;

/* Notes :

  The type gss_OID, or Object identifier, is a type containing ISO-defined tree-structured
  values, and is used by the GSSAPI caller to select an underlying
  security mechanism and to specify namespaces.  Here they are actually pointers to 
  gss_OID_desc structures.
  
	
*/


typedef struct {
	
	ilu_TransportCreator lower;         /* must be reliable and boundaried */
	ilu_string           secmech_name;  /* owned by TransportCreator */
	gss_OID              secmech;       /* const */
	ilu_string           server_name;   /* owned by TransportCreator */
	
}   *CreatorParms;


#define SECBUFMAX 1100

/*  struct securityparms goes in the data field of a SECURITY ilu_Transport. */
typedef struct securityparms {	
	
	gss_OID         secmech;
	gss_ctx_id_t    context;
	OM_uint32       context_flags;
	ilu_FineTime    context_max_time;
	ilu_Transport   lower;
	
	ilu_byte        preBytes[SECBUFMAX];
	ilu_cardinal    preFirst, preNext;
	ilu_boolean     startNeeded, finishNeeded;
	/*
	 * Before any other output ops are done on lower, we have to: (1) start-message if startNeeded, (2)
	 * write preBytes[preFirst,preNext) and then, (3) if
	 * finishNeeded, end-message.
	 */
	
}   *SECURITYParms;



typedef struct securitymooringparms {
	
	ilu_string          server_name;
	gss_cred_id_t       server_cred;
	ilu_FineTime        server_cred_max_time;
	gss_OID             secmech;
	ilu_Mooring         lower;
	
}   *SECURITYMooringParms;


struct _ilu_GSSIdentityInfo_s {

  gss_ctx_id_t  context;
  /* caller-opaque atomic value identifying one end of a GSSAPI security context;
     belongs to transport instance */

  gss_cred_id_t cred;
  /* caller-opaque atomic datum identifying a GSSAPI credential data structure;
     belongs to identity */

  ilu_cardinal refcount;
  /* when 0, should delete the credential, and free self */

};

static ilu_IdentityInfo
AcquireGSSIdentity (gss_cred_id_t cred, gss_ctx_id_t ctx, ilu_Error *err)
{
  ilu_IdentityInfo info;
  ilu_GSSIdentityInfo gssinfo;
	
  info = (ilu_IdentityInfo) ilu_MallocE(sizeof(*info), err);
  if (ILU_ERRNOK(*err)) return NIL;
  gssinfo = (ilu_GSSIdentityInfo) ilu_MallocE(sizeof(*gssinfo), err);
  if (ILU_ERRNOK(*err)) { ilu_free(info); return NIL; }
  info->ii_type = ilu_GSSIdentity;
  info->ii_owned_by_passport = ilu_FALSE;
  info->ii_info = (ilu_refany) gssinfo;
  gssinfo->context = ctx;
  gssinfo->cred = cred;
  gssinfo->refcount = 1;
  return info;
}

ilu_IdentityInfo
  ilu_AcquireGSSIdentity (gss_cred_id_t cred,	/* input, pass */
			  ilu_Error *err)
{
  return AcquireGSSIdentity(cred, NIL, err);
}

/* forward declare our transport class */
static struct _ilu_TransportClass_s _gss_transport_class;


#define BUFFERSIZE		8192

#define SECURITYPARMS(a) ((SECURITYParms)(a))
#define SECURITYMOORINGPARMS(a)	((SECURITYMooringParms)(a))


/* returns true if the OIDs are the same */
static ilu_boolean compare_oids (gss_OID o1, gss_OID o2)
{
	if ((o1->length != o2->length) ||
		((o1->elements == NIL) && (o2->elements != NIL)) ||
		((o1->elements != NIL) && (o2->elements == NIL)))
		return ilu_FALSE;
	return (ilu_boolean) (memcmp(o1->elements, o2->elements, o1->length) == 0);
}

static ilu_cardinal strchrcount (ilu_string s, ilu_string chars)
{
	ilu_cardinal count = 0;
	ilu_cardinal index = 0;
	
	while (s[index] != 0)
    {
		index += strcspn (s + index, chars);
		if (s[index] != 0)
		{
			count += 1;
			index += 1;
		}
    }
	return count;
}

#ifdef ENABLE_DEBUGGING
static char *gssStrErr(OM_uint32 status_code)
{
	char *ret;
	gss_buffer_desc errbuf;
	OM_uint32 minor_code,msg_context=0;
	
	if (gss_display_status(&minor_code,status_code,
		GSS_C_GSS_CODE,NULL,
		&msg_context,
		&errbuf) != GSS_S_COMPLETE)
    {
		return _ilu_Strdup("unknown GSS error");
    };
	ret = ilu_must_malloc(errbuf.length + 1);
	strncpy (ret, errbuf.value, errbuf.length);
	ret[errbuf.length] = 0;
	_ilu_Assert(gss_release_buffer(&minor_code, &errbuf) == GSS_S_COMPLETE,
		"release of GSS buffer failed");
	return ret;
}
#endif /* ENABLE_DEBUGGING */


/* structure to associate a name with a gss_OID_desc */
typedef struct { 
	char *name;  
	gss_OID_desc oid[1]; 
} oid_names;



/* Returns the gss_OID associated with the given id.  
If id is in string form, i.e. {... or dotted decimal form, gss_str_to_oid is used, 
else the array oid_names is searched for a matching id. */
static gss_OID
  lookupOID (const char *id, oid_names *known, int sizeof_known, ilu_boolean *known_p)
{
  int i;
  if (id[0] == '{' ||		/* string form of OID */
      isdigit(id[0]))		/* dotted decimal form */
    {
      gss_buffer_desc b;
      gss_OID out;
      OM_uint32 major, minor = 0;
      
      b.value = (char *) id;
      b.length = strlen(id);
      major = gss_str_to_oid (&minor, &b, &out);
      if (major != GSS_S_COMPLETE)
	{
	  ILU_NOTE(SECURITY_DEBUG,
		   ("gss.c:  gss_str_to_oid failed with %s (%d)\n",
		    gssStrErr(major), minor));
	  return NIL;				 
	}
      else {
	*known_p = ilu_FALSE;
	return out;
      }
    }
  else
    {
      /* lookup in table of names */
      for (i = 0;  i < sizeof_known;  i++)
	if (strcmp(known[i].name, id) == 0) {
	  *known_p = ilu_TRUE;
	  return known[i].oid;
	}
    }
  return NIL;
}


/* Returns the gss_OID associated with the id, which may be 
one of the Xerox.ILU.GSS.SSL or Xerox.ILU.GSS.NIL */
static gss_OID figureScheme (const char *id, ilu_boolean *old)
{
  /*
   * OIDs used in PARC implementation of GSS-API:
   *
   * Xerox ::= {iso (1) member-body (2) US (840) Xerox (113550) }
   *
   * ILU ::= {Xerox 9}
   *
   * GSS ::= {ILU 1}
   *
   * SSL ::= {GSS 1}   -- ILU GSS SSL implementation (gss_mech_ssl)
   * X500 ::= { GSS 2} -- ILU GSS X.500 namespace  (gss_nt_x509)
   * nil-mech ::= { GSS 3} -- ILU GSS nil security mechanism (gss_mech_nil)
   *
   * unofficial:
   * rfc822 ::= { GSS 4} -- ILU GSS RFC-822 namespace (gss_nt_rfc822)
   *
   */
  static oid_names known[] = {
    { "Xerox.ILU.GSS.SSL", { { 9, "\x2a\x86\x48\x86\xf7\x0e\x09\x01\x01" } } },
    { "Xerox.ILU.GSS.NIL", { { 9, "\x2a\x86\x48\x86\xf7\x0e\x09\x01\x03" } } },
  };
  gss_OID oid = lookupOID(id, known, sizeof(known)/sizeof(oid_names), old);
#ifdef ENABLE_DEBUGGING
  if ((ilu_DebugLevel & SECURITY_DEBUG) != 0)
    {
      ilu_DebugPrintf ("(gss.c:figureScheme):  oid for %s is", id);
      if (oid != NIL)
	{
	  unsigned int i;
	  for (i = 0;  i < oid->length;  i++)
	    {
	      ilu_DebugPrintf (" %2.2x", ((unsigned char *)(oid->elements))[i]);
	    }
	  ilu_DebugPrintf ("\n");
	}
      else
	ilu_DebugPrintf (" NIL\n");
    }
#endif
  return (oid);
}


/* Returns the gss_OID associated with the id, which may be 
one of the Xerox.ILU.GSS.X509 or Xerox.ILU.GSS.RFC822 */
static gss_OID figureNamespace (const char *id, ilu_boolean *old)
{
  static oid_names known[] = {
    { "Xerox.ILU.GSS.X509", { { 9, "\x2a\x86\x48\x86\xf7\x0e\x09\x01\x02" } } },
    { "Xerox.ILU.GSS.RFC822", { { 9, "\x2a\x86\x48\x86\xf7\x0e\x09\x01\x04" } } },
  };
  gss_OID oid = lookupOID(id, known, sizeof(known)/sizeof(oid_names), old);
#ifdef ENABLE_DEBUGGING
  if ((ilu_DebugLevel & SECURITY_DEBUG) != 0)
    {
      ilu_DebugPrintf ("(gss.c:figureNamespace):  oid for %s is", id);
      if (oid != NIL)
	{
	  unsigned int i;
	  for (i = 0;  i < oid->length;  i++)
	    {
	      ilu_DebugPrintf (" %2.2x", ((unsigned char *)(oid->elements))[i]);
	    }
	  ilu_DebugPrintf ("\n");
	}
      else
	ilu_DebugPrintf (" NIL\n");
    }
#endif
  return oid;
}

static          ilu_boolean
ClearPre(SECURITYParms p,
	 ilu_Transport lower,
	 ilu_boolean mayBlock,
	 ilu_Error * err)
{
  if (p->startNeeded) {
    if (mayBlock)
      p->startNeeded = !transport_begin_message(lower, ilu_FALSE, err);
    else
      p->startNeeded = !transport_begin_output_nonblock(lower, err);
    if (ILU_ERRNOK(*err) || p->startNeeded)
      return ilu_FALSE;
  }
  if (p->preNext) {
    ilu_cardinal    wrote;
    wrote = (transport_write_bytes_maybeblock
	     (lower, p->preBytes + p->preFirst,
	      p->preNext - p->preFirst, mayBlock, err));
    p->preFirst += wrote;
    if (p->preFirst == p->preNext)
      p->preFirst = p->preNext = 0;
    else
      return ilu_FALSE;
    if (ILU_ERRNOK(*err))
      return ilu_FALSE;
  }
  if (p->finishNeeded) {
    if (mayBlock)
      p->finishNeeded = !transport_end_message(lower, ilu_FALSE, NIL, err);
    else
      p->finishNeeded = !transport_end_output_nonblock(lower, ilu_FALSE,
						       NIL, err).iluter_ended;
    if (ILU_ERRNOK(*err) || p->finishNeeded)
      return ilu_FALSE;
  }
  return ILU_CLER(*err);
}

static          ilu_boolean
SendWholeMessage(SECURITYParms p,	/* optional */
		 ilu_Transport lower,
		 ilu_bytes buf,
		 ilu_cardinal len,
		 ilu_boolean mayBlock,
		 ilu_Error * err)
{
  if (!ilu_Check(!(p && (p->startNeeded || p->preNext
			 || p->finishNeeded)),
		 err))
    return ilu_FALSE;
  if (!mayBlock) {
    ilu_cardinal    taken = 0;
    if (!ilu_Check(p && len < SECBUFMAX, err))
      return ilu_FALSE;
    if (!transport_begin_output_nonblock(lower, err))
      p->startNeeded = ilu_TRUE;
    else
      taken = transport_write_bytes_nonblock(lower, buf, len, err);
    if (taken < len || p->startNeeded || ILU_ERRNOK(*err)) {
      if (taken < len)
	memcpy((void *) p->preBytes,
	       (void *) (buf + taken),
	       p->preNext = len - taken);
      p->finishNeeded = ilu_TRUE;
    } else {
      if (!transport_end_message(lower, ilu_TRUE, NIL, err))
	p->finishNeeded = ilu_TRUE;
    }
    return ILU_ERROK(*err);
  }
  if (transport_begin_message(lower, ilu_FALSE, err), ILU_ERRNOK(*err))
    return ilu_FALSE;
  if (transport_write_bytes(lower, buf, len, err), ILU_ERRNOK(*err))
    return ilu_FALSE;
  if (!transport_end_message(lower, ilu_TRUE, NIL, err))
    return ilu_FALSE;
  return ilu_TRUE;
}

/*
* Returns ilu_TRUE iff a message successfully read.  Always sets both
* members of *rpt.  If !forSure, will return (without error) early
* if EOF or if no bytes are available without blocking.  Will also
* return early without error if tc_interrupt was called in a
* single-threaded runtime.
*/
static          ilu_boolean
ReadWholeMessage(ilu_Transport lower,
		 ilu_bytes * out,
		 ilu_cardinal * outlen,
		 ilu_TransportReport * rpt,
		 ilu_boolean forSure,
		 ilu_Error * err)
{
	ilu_ReadHeaderResultCode rhrc;
	rpt->tr_eom = rpt->tr_eof = ilu_FALSE;
	while (1) {
		int disabled;
		rhrc = transport_begin_message(lower, ilu_TRUE, err);
		switch (rhrc) {
		case ilu_rhrc_ok:
			goto gotit;
		case ilu_rhrc_eof:
			if (forSure)
				return ILU_ERR_CONS1(comm_failure, err,
				minor, ilu_cfm_connect_failed, ilu_FALSE);
			rpt->tr_eom = rpt->tr_eof = ilu_TRUE;
			return ilu_FALSE;
		case ilu_rhrc_nothing:
			if (forSure)
				goto waitforit;
			return ilu_FALSE;
		case ilu_rhrc_error:
			return ilu_FALSE;
		default:
			(void) ilu_Check(ilu_FALSE, err);
			return ilu_FALSE;
		}
		
waitforit:
		if (!transport_wait_for_input(lower, &disabled, NIL, err))
			return ilu_FALSE;
		/* XXX is this the correct action (i.e. loop) regardless of the value of disabled? */
	}
	
gotit:
	_ilu_transportReadMessage(lower, out, outlen, rpt, err);
	rpt->tr_eom = rpt->tr_eof;
	if (ILU_ERRNOK(*err))
		return ilu_FALSE;
	return (transport_end_message(lower, ilu_FALSE, NIL, err), ILU_ERROK(*err));
}


static ilu_string encode (ilu_string s)
{
	static const ilu_string badchars = "_@=/;?%$\1\2\3\4\5\6\7\10\11\12\13\14\15\16\17\20\21\22\23\24\25\26\27\30\31\32\33\34\35\36\37\40";
	static const ilu_string hexdigits = "0123456789ABCDEF";
	ilu_cardinal len = (strchrcount (s, badchars) * 2) + strlen(s) + 1;
	register char *from, *to;
	ilu_string n;
	
	n = (ilu_string) ilu_malloc(len);
	if (n == NIL)
		return NIL;
	for (from = s, to = n;  *from != 0;  from++)
    {
		if (strchr(badchars, *from) != NIL)
		{
			*to++ = '%';
			*to++ = hexdigits[((unsigned)(((unsigned char)(*from)) & 0xF0)) >> 4];
			*to++ = hexdigits[(((unsigned char)(*from)) & 0x0F)];
		}
		else
			*to++ = *from;
    }
	*to = 0;
	return n;
}

#define FROMHEX(a)	((((a)>='0')&&((a)<='9'))?((a)-'0'):((((a)>='A')&&((a)<='F'))?((a)-'A'):0))

static ilu_string decode (ilu_string s)
{
	ilu_cardinal len = strlen(s) - strchrcount (s, "%") + 1;
	register char *from, *to;
	ilu_string n;
	
	n = (ilu_string) ilu_malloc(len);
	if (n == NIL)
		return NIL;
	for (from = s, to = n;  *from != 0;  from++)
    {
		if (*from == '%')
			*to++ = FROMHEX(from[1]) + FROMHEX(from[2]);
		else
			*to++ = *from;
    }
	*to = 0;
	return n;
}


static void MapGSSErrors (gss_status major, gss_status minor, ilu_Error *err, ilu_string filename, ilu_cardinal lineno)
{
	/* remap errors to make GDB debugging easier */
	
	ILU_FULLBIND_ERR(gss_security, err, ev, filename, lineno)
    {
		switch (major)
		{
		case GSS_S_BAD_BINDINGS:  ev->major = ilu_gsm_GSS_S_BAD_BINDINGS;
			break;		/* Channel Binding Mismatch */
		case GSS_S_BAD_MECH:  ev->major = ilu_gsm_GSS_S_BAD_MECH;
			break;		/* Unsupported Mechanism Requested */
		case GSS_S_BAD_NAME:  ev->major = ilu_gsm_GSS_S_BAD_NAME;
			break;		/* Invalid Name Provided */
		case GSS_S_BAD_NAMETYPE:  ev->major = ilu_gsm_GSS_S_BAD_NAMETYPE;
			break;		/* Name Of Unsupported Type Provided */
		case GSS_S_BAD_STATUS:  ev->major = ilu_gsm_GSS_S_BAD_STATUS;
			break;		/* Invalid Input Status Selector */
		case GSS_S_BAD_SIG:  ev->major = ilu_gsm_GSS_S_BAD_SIG;
			break;		/* Token Had Invalid Signature */
		case GSS_S_CONTEXT_EXPIRED:  ev->major = ilu_gsm_GSS_S_CONTEXT_EXPIRED;
			break;		/* Specified Security Context Expired */
		case GSS_S_CREDENTIALS_EXPIRED:  ev->major = ilu_gsm_GSS_S_CREDENTIALS_EXPIRED;
			break;		/* Expired Credentials Detected */
		case GSS_S_DEFECTIVE_CREDENTIAL:  ev->major = ilu_gsm_GSS_S_DEFECTIVE_CREDENTIAL;
			break;		/* Defective Credential Detected */
		case GSS_S_DEFECTIVE_TOKEN:  ev->major = ilu_gsm_GSS_S_DEFECTIVE_TOKEN;
			break;		/* Defective Token Detected */
		case GSS_S_FAILURE:  ev->major = ilu_gsm_GSS_S_FAILURE;
			break;		/* Failure, Unspecified At GSS-API Level */
		case GSS_S_NO_CONTEXT:  ev->major = ilu_gsm_GSS_S_NO_CONTEXT;
			break;		/* No Valid Security Context Specified */
		case GSS_S_NO_CRED:  ev->major = ilu_gsm_GSS_S_NO_CRED;
			break;		/* No Valid Credentials Provided */
		case GSS_S_BAD_QOP:  ev->major = ilu_gsm_GSS_S_BAD_QOP;
			break;		/* Unsupported QOP Value */
		case GSS_S_UNAUTHORIZED:  ev->major = ilu_gsm_GSS_S_UNAUTHORIZED;
			break;		/* Operation Unauthorized */
		case GSS_S_UNAVAILABLE:  ev->major = ilu_gsm_GSS_S_UNAVAILABLE;
			break;		/* Operation Unavailable */
		case GSS_S_CONTINUE_NEEDED:  ev->major = ilu_gsm_GSS_S_CONTINUE_NEEDED;
			break;		/* Continuation Call To Routine Required */
		case GSS_S_DUPLICATE_TOKEN:  ev->major = ilu_gsm_GSS_S_DUPLICATE_TOKEN;
			break;		/* Duplicate Per-Message Token Detected */
		case GSS_S_OLD_TOKEN:  ev->major = ilu_gsm_GSS_S_OLD_TOKEN;
			break;		/* Timed-Out Per-Message Token Detected */
		case GSS_S_UNSEQ_TOKEN:  ev->major = ilu_gsm_GSS_S_UNSEQ_TOKEN;
			break;		/* Reordered (Early) Per-Message Token Detected */
		case GSS_S_GAP_TOKEN:  ev->major = ilu_gsm_GSS_S_GAP_TOKEN;
			break;		/* Skipped Predecessor Token(S) Detected */
		}
		ev->minor = minor;
    } ILU_END_FULLBIND_ERR;
}


static ilu_string NameTToString (gss_name_t name, ilu_Error *err)
{
	OM_uint32 major, minor;
	gss_buffer_desc namebuffer, nametypebuffer;
	gss_OID nametype;
	char buffer[2048];
	
	major = gss_display_name (&minor, name, &namebuffer, &nametype);
	if (GSS_ERROR(major))
    {
		ILU_NOTE(SECURITY_DEBUG,
			("(gss.c:NameTToString):  gss_display_name() signals error \"%s\" (%u).\n",
			gssStrErr(major), minor));
		MapGSSErrors (major, minor, err, __FILE__, __LINE__);
		return NIL;
    }
	else
    {
		major = gss_oid_to_str (&minor, nametype, &nametypebuffer);
		if (GSS_ERROR(major))
		{
			ILU_NOTE(SECURITY_DEBUG,
				("(gss.c:NameTToString):  gss_oid_to_str() signals error \"%s\" (%u).\n",
				gssStrErr(major), minor));
			MapGSSErrors (major, minor, err, __FILE__, __LINE__);
			return NIL;
		}
		else
		{
			sprintf (buffer, "%*.*s:%*.*s",
				 (int) nametypebuffer.length,
				 (int) nametypebuffer.length, (char *) nametypebuffer.value,
				 (int) namebuffer.length,
				 (int) namebuffer.length, (char *) namebuffer.value);
			major = gss_release_buffer (&minor, &nametypebuffer);
		}
		major = gss_release_buffer(&minor, &namebuffer);
    }
	major = gss_release_name (&minor, &name);
	ILU_CLER(*err);
	return ilu_StrdupE(buffer, err);
}


static gss_name_t StringToNameT (ilu_string name, ilu_Error *err)
{
  gss_status major, minor = 0;
  gss_name_t gssname;
  gss_OID namespace;
  gss_buffer_desc nameb;
  char buf[100];
  char *p;
  ilu_boolean old;
	
  if (name == NIL)
    return (ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_nil, NIL));
  
  p = strchr(name, ':');
  if (p == NIL)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_gssNameString, NIL);
  strncpy (buf, name, p - name);
  buf[p - name] = 0;
  if ((namespace = figureNamespace (buf, &old)) == NIL)
    {
      ILU_NOTE(SECURITY_DEBUG,
	       ("(gss.c:StringToNameT):  Can't find namespace OID for <%s>\n", buf));
      return (ILU_ERR_CONS2(gss_security, err, major, ilu_gsm_GSS_S_BAD_NAME, minor, 0, NIL));
    }
  nameb.value = p + 1;
  nameb.length = strlen(p + 1);
  major = gss_import_name (&minor, &nameb, namespace, &gssname);
  if (!old) {
    gss_status nmajor, nminor;
    nmajor = gss_release_oid(&nminor, &namespace);
  };
  if (GSS_ERROR(major))
    {
      ILU_NOTE(SECURITY_DEBUG,
	       ("(gss.c:StringToNameT):  gss_import_name() signals error \"%s\" (%u).\n",
		gssStrErr(major), minor));
      MapGSSErrors (major, minor, err, __FILE__, __LINE__);
      return NIL;
    }
  return (gssname);  
}



static          ilu_Transport
NewTrans(ilu_Transport lower, gss_OID secmech,
		 gss_ctx_id_t context, OM_uint32 flags, OM_uint32 lifetime,
		 ILU_ERRS((no_memory)) * err)
{
	ilu_Transport   ans;
	SECURITYParms   parms;
	double	  dlifetime = lifetime;
	
	parms = (SECURITYParms) ilu_malloc(sizeof(*parms));
	if (parms == NIL)
		return ILU_ERR_CONS1(no_memory, err, nbytes, sizeof(*parms), NIL);
	parms->secmech = secmech;
	parms->context = context;
	parms->context_flags = flags;
	parms->context_max_time = ilu_FineTime_Add(ilu_FineTime_Now(), ilu_FineTime_FromDouble(dlifetime));
	parms->lower = lower;
	parms->startNeeded = parms->finishNeeded = ilu_FALSE;
	parms->preFirst = parms->preNext = 0;
	
	ans = (ilu_Transport) ilu_malloc(sizeof(*ans));
	if (ans == NIL)
		return ILU_ERR_CONS1(no_memory, err, nbytes, sizeof(*ans), NIL);
	ans->tr_inBuff = NIL;
	ans->tr_outBuff = ilu_malloc(BUFFERSIZE);
	if (ans->tr_outBuff == NIL)
		return ILU_ERR_CONS1(no_memory, err, nbytes, sizeof(BUFFERSIZE), NIL);
	ans->tr_outLimit = BUFFERSIZE;
	ans->tr_inNext = ans->tr_inLimit = 0;
	ans->tr_outNext = ans->tr_outLimit = 0;
	ans->tr_class = &_gss_transport_class;
	ans->tr_data = parms;
	ans->tr_wc = lower->tr_wc;
	ans->tr_estFDs = lower->tr_estFDs;
	
	ILU_NOTE(SECURITY_DEBUG,
		("gss.c:NewTrans(lower=%p) = %p\n",
		lower, ans));
	ILU_CLER(*err);
	
	return ans;
}

static gss_OID
  oiddupE (gss_OID inoid, ilu_Error *err)
{
  gss_status major, minor;
  gss_buffer_desc b;
  gss_OID newoid;

  major = gss_oid_to_str(&minor, inoid, &b);
  if (GSS_ERROR(major)) goto errout0;
  major = gss_str_to_oid(&minor, &b, &newoid);
  if (GSS_ERROR(major)) goto errout1;
  major = gss_release_buffer(&minor, &b);

  ILU_CLER(*err);
  return newoid;  

 errout1:
  major = gss_release_buffer(&minor, &b);

 errout0:

  MapGSSErrors(major, minor, err, __FILE__, __LINE__);
  return NIL;
}

/*L1.sup < trmu; L2 unconstrained*/
static          CreatorParms
_gss_InterpretInfo(ilu_TransportInfo info, ILU_ERRS((no_memory, inv_objref)) * err)
{
  CreatorParms    cp;
  ilu_TransportCreator lower;
  char secmech[100];
  char servername[2000];
  char *decoded_secmech;
  gss_OID	secmechoid;
  int parmcount;
  ilu_boolean old;
  gss_status major, minor;
	
  if (info == NIL || info[0] == NIL || info[1] == NIL ||
      ((parmcount = sscanf(info[0], "gss_1_%99[^_]_%1999s", secmech, servername)) < 1))
    return ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_ts, NIL);
  if ((decoded_secmech = decode(secmech)) == NIL)
    return ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_ts, NIL);
  if ((secmechoid = figureScheme (decoded_secmech, &old)) == NIL)
    {
      ilu_free(decoded_secmech);
      return ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_ts, NIL);
    }
  else
    ilu_free(decoded_secmech);
  lower = _ilu_GetTransportCreator(info + 1, err);
  if (ILU_ERRNOK(*err))
    return NIL;
  if (!lower->tcr_reliable || !lower->tcr_boundaried)
    return ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_ts, NIL);
  cp = (CreatorParms) ilu_malloc(sizeof(*cp));
  if (cp == NIL)
    return ILU_ERR_CONS1(no_memory, err, nbytes, sizeof(*cp), NIL);
  cp->secmech_name = _ilu_Strdup(secmech);
  cp->secmech = oiddupE(secmechoid, err);
  if (ILU_ERRNOK(*err)) {ilu_free(cp->secmech_name); ilu_free(cp); return NIL; };
  if (!old) {
    major = gss_release_oid(&minor, &secmechoid);
  }
  cp->lower = lower;
  if (parmcount == 2)
    cp->server_name = decode(servername);
  else
    cp->server_name = NIL;
  ILU_NOTE(SECURITY_DEBUG, ("_gss_InterpretInfo:  scheme %s\n", secmech));
  return cp;
}



/* L1.sup < trmu; L2 >= {xmu, ymu} */
static          ilu_boolean
_gss_SetInputHandler(ilu_Transport self,
					 ilu_TIH tih,
					 ILU_ERRS((no_memory, internal,
					 no_resources)) * err)
{
	SECURITYParms   p = SECURITYPARMS(transport_data(self));
	return _ilu_SetTransportInputHandler(p->lower, tih, err);
}


/* Main Invariant holds */

static          ilu_Transport
_gss_CreateTransport(ilu_TransportCreator self, ilu_boolean buffer,
					 ilu_integer *dfds, ilu_Passport pp,
					 ILU_ERRS((IoErrs)) * err)
{
  /* create an outgoing link -- client side */
	
  CreatorParms	cp = (CreatorParms) self->tcr_data;
  ilu_Transport	lower;
  gss_buffer_desc output_token, input_token;
  ilu_Message m;
  gss_status major, minor;
  OM_uint32 lifetime = 0, flags = 0;
  gss_ctx_id_t	context = GSS_C_NO_CONTEXT;
  gss_cred_id_t	client_cred = GSS_C_NO_CREDENTIAL;
  gss_name_t server_name;
  ilu_IdentityInfo ident;
  ilu_TransportReport rpt;
	
  *dfds = 0;
	
  if ((pp != NIL) &&
      (ident = ilu_FindIdentity(pp, ilu_GSSIdentity)) != NIL &&
      ((struct _ilu_GSSIdentityInfo_s*)(ident->ii_info))->cred != NIL)
    client_cred = ((struct _ilu_GSSIdentityInfo_s*)(ident->ii_info))->cred;
	
  _ilu_Assert(cp->server_name != NIL,
	      "NIL server name in GSS CreateTransport");
  if ((server_name = StringToNameT(cp->server_name, err)) == NIL)
    return NIL;
	
  lower = (*cp->lower->tcr_createTransport) (cp->lower, ilu_FALSE, dfds,
					     pp, err);
  if (ILU_ERRNOK(*err)) goto errout1;
  if (!transport_reliable(lower) || !transport_boundaried(lower))
    goto errout2;
	
  input_token.value = NIL;
  input_token.length = 0;
  do {
    output_token.length = 0;
    output_token.value = NIL;
    major = (gss_init_sec_context
	     (&minor,
	      client_cred,	/* IN:  client credential */
	      &context,		/* OUT:  context being established */
	      server_name,	/* IN:  server name */
	      cp->secmech,	/* IN:  the security mechanism */
	      GSS_C_MUTUAL_FLAG | GSS_C_REPLAY_FLAG | GSS_C_SEQUENCE_FLAG,
	      0,		/* IN:  use default lifetime */
	      GSS_C_NO_CHANNEL_BINDINGS, /* channel binding info */
	      &input_token,	/* IN:  token from peer, if any */
	      NIL,		/* OUT:  actual mech type */
	      &output_token,	/* OUT:  token to send to other peer */
	      &flags,		/* OUT:  flag bits */
	      &lifetime));	/* OUT:  actual lifetime */
    if (input_token.value != NIL) {
      OM_uint32       nmajor, nminor;
      nmajor = gss_release_buffer(&nminor, &input_token);
    }
    if (GSS_ERROR(major)) {
      ILU_NOTE(SECURITY_DEBUG,
	       ("%s  gss_init_sec_context() signals error \"%s\" (%u).\n",
		"_gss_CreateTransport:", gssStrErr(major), minor));
      MapGSSErrors(major, minor, err, __FILE__, __LINE__);
      if (output_token.value != NIL)
	major = gss_release_buffer(&minor, &output_token);
      goto errout1;
    }
    if (output_token.length > 0) {
      if (!SendWholeMessage(NIL, lower,
			    output_token.value, output_token.length,
			    ilu_TRUE, err))
	goto errout1;
      if (output_token.value != NIL) {
	OM_uint32       nmajor, nminor;
	nmajor = gss_release_buffer(&nminor, &output_token);
      }
    }
    if (major == GSS_S_CONTINUE_NEEDED) {
      m.msg_base = NIL;
      m.msg_len = 0;
      if (!ReadWholeMessage(lower, &m.msg_base, &m.msg_len, &rpt,
			    ilu_TRUE, err))
	goto errout1;
      input_token.value = m.msg_base;
      input_token.length = m.msg_len;
    }
  } while ((GSS_SUPPLEMENTARY_INFO(major) & GSS_S_CONTINUE_NEEDED) != 0);

  major = gss_release_name(&minor, &server_name);
  return (NewTrans(lower, cp->secmech, context, flags, lifetime, err));

 errout2:
  ILU_ERR_CONS1(internal, err, minor, ilu_im_tcBug, NIL);
 errout1:
  major = gss_release_name(&minor, &server_name);
  return NIL;
}

/* Main Invariant holds; L2 >= {ymu} */
static          ilu_boolean
_gss_WaitForInput(ilu_Transport self, int*  disabled,  ilu_FineTime * limit,
				  ILU_ERRS((interrupted)) * err)
{
	SECURITYParms   p = SECURITYPARMS(transport_data(self));
	return (transport_wait_for_input(p->lower, disabled, limit, err));
}


/* L1.sup < trmu; L2 >= {xmu} */
static          ilu_boolean
_gss_Interrupt(ilu_Transport self, ILU_ERRS((bad_param)) * err)
{
	SECURITYParms   p = SECURITYPARMS(transport_data(self));
	return ( transport_interruptST(p->lower, err));
}

/* L1 >= {cmu}; L1.sup < trmu; L2 >= {xmu} */
static ilu_boolean
_gss_DisableWait (ilu_Transport self,
				  ILU_ERRS((broken_locks,
				  bad_param,
				  internal)) *err) 
{
	SECURITYParms   p = SECURITYPARMS(transport_data(self));
	return ( transport_disableWait(p->lower, err));
}

/* L1 >= {cmu}; L1.sup < trmu; L2 >= {xmu} */
static ilu_boolean
_gss_EnableWait (ilu_Transport self, 
				 ILU_ERRS((broken_locks,
				 bad_param,
				 internal)) *err)
{
	SECURITYParms   p = SECURITYPARMS(transport_data(self));
	return ( transport_enableWait(p->lower, err));
}



/* L1.sup < trmu; L2 unconstrained */

static          ilu_integer
_gss_FdUsage(ilu_TransportCreator self,
			 ilu_boolean mooring)
{
	CreatorParms    cp = (CreatorParms) self->tcr_data;
	return (*cp->lower->tcr_dfd) (cp->lower, mooring);
}



/* L1.sup < trmu; L2 >= {xmu} */

static          ilu_boolean
_gss_BeginOutputMessageNonblock(ilu_Transport self,
				ILU_ERRS((IoErrs)) * err)
{
  ILU_NOTE(SECURITY_DEBUG,
	   ("_gss_BeginOutputMessageNonblock(%p)\n",
	    self));
  return ILU_ERR_CONS1(internal, err, minor, ilu_im_tcNotBoundaried,
		       ilu_FALSE);
}


static          ilu_TransportEndReport
_gss_EndOutputMessageNonblock(ilu_Transport self,
			      ilu_boolean flush,
			      ilu_Message * msgh,
			      ILU_ERRS((IoErrs)) * err)
{
  ilu_TransportEndReport ans = {ilu_FALSE, ilu_FALSE};
  ILU_NOTE(SECURITY_DEBUG,
	   ("_gss_EndOutputMessageNonblock (%p, %s, (%p, %u))\n",
	    self, flush ? "flush" : "noflush", msgh->msg_base,
	    msgh->msg_len));
  return ILU_ERR_CONS1(internal, err, minor, ilu_im_tcNotBoundaried,
		       ans);
}

/* Main Invariant holds; L2 >= {xmu}; input => L2 >= {ymu} */
static          ilu_ReadHeaderResultCode
_gss_BeginMessage(ilu_Transport self,
				  ilu_boolean input_p,
				  ILU_ERRS((IoErrs)) * err)
{
	ILU_NOTE(SECURITY_DEBUG,
		("_gss_BeginMessage(%p, %s)\n",
		self, input_p ? "input" : "output"));
	return ILU_ERR_CONS1(internal, err, minor, ilu_im_tcNotBoundaried,
		ilu_rhrc_error);
}


/* Main Invariant holds; L2 >= {xmu}; input => L2 >= {ymu} */
static          ilu_boolean
_gss_EndMessage(ilu_Transport self,
				ilu_boolean flush,
				ilu_Message * msgh,
				ILU_ERRS((IoErrs)) * err)
{
	ILU_NOTE(SECURITY_DEBUG,
		("_gss_EndMessage (%p, %s, (%p, %u))\n",
		self, flush ? "flush" : "noflush", msgh->msg_base,
		msgh->msg_len));
	return ILU_ERR_CONS1(internal, err, minor, ilu_im_tcNotBoundaried,
		ilu_FALSE);
}



/*Main Invariant holds; L2 >= {xmu}*/
static ilu_boolean 
SecureWrite(ilu_Transport self,
	    ilu_bytes b, ilu_cardinal bufferSize,
	    ilu_boolean flush, ilu_boolean *flushed,
	    ilu_boolean mayBlock, ilu_Error * err)
{
  SECURITYParms   p = SECURITYPARMS(transport_data(self));
  ilu_Transport   lt = p->lower;
  gss_status      major, minor;
  gss_buffer_desc plainbuf, cryptobuf;
  
  if (!ClearPre(p, lt, mayBlock, err))
    return ilu_FALSE;

#ifdef ENABLE_DEBUGGING

  ILU_NOTE(SECURITY_DEBUG | PACKET_DEBUG,
  ("_gss_SecureWrite: wrapping buffer of %u bytes:\n", bufferSize));


  if ((ilu_DebugLevel & (PACKET_DEBUG | SECURITY_DEBUG)) == (PACKET_DEBUG | SECURITY_DEBUG))
    _ilu_debug_DumpPacket(b, bufferSize, "outgoing plain");

#endif				/* ENABLE_DEBUGGING */

  plainbuf.length = bufferSize;
  plainbuf.value = b;
  cryptobuf.length = 0;
  cryptobuf.value = NIL;
  major = gss_wrap(&minor,	/* OUT:  return buffer for minor
				 * error code */
		   p->context,	/* IN:   current security context */
		   ilu_TRUE,	/* IN:   requests both
				 * confidentiality and integrity */
		   GSS_C_QOP_DEFAULT,	/* IN:   quality of
					 * protection desired */
		   &plainbuf,	/* IN:   data to wrap */
		   NIL,		/* OUT:  whether confidentiality has
				 * been applied */
		   &cryptobuf	/* OUT:  wrapped data */
    );
  if (GSS_ERROR(major)) {
    ILU_NOTE(SECURITY_DEBUG,
    ("(gss.c:SecureWrite):  gss_wrap signals error \"%s\" (%u).\n",
     gssStrErr(major), minor));
    MapGSSErrors(major, minor, err, __FILE__, __LINE__);
    if (cryptobuf.value != NIL)
      major = gss_release_buffer(&minor, &cryptobuf);
    return ilu_FALSE;
  }
  (void) SendWholeMessage(p, lt, cryptobuf.value, cryptobuf.length,
			  mayBlock, err);
  major = gss_release_buffer(&minor, &cryptobuf);
  return (ILU_ERROK(*err));
}


/*mayBlock ? Main Invariant : L1.sup < trmu; L2 >= {xmu}*/

static          ilu_cardinal
_gss_WriteBytes_Full(ilu_Transport self, ilu_bytes b,
		     ilu_cardinal bufferSize,
		     ilu_boolean flush,
		     ilu_boolean * flushed,
		     ilu_boolean mayBlock,
		     ILU_ERRS((IoErrs)) * err)
{
  ilu_cardinal    taken = 0;
  *flushed = ilu_FALSE;
  if (self->tr_outNext > 0) {
    (void) SecureWrite(self, self->tr_outBuff, self->tr_outNext,
		       (b == NIL) && flush, flushed, mayBlock, err);
    if (ILU_ERRNOK(*err))
      return 0;
  } else if (flush && (b == NIL))
    *flushed = ilu_TRUE;
  self->tr_outNext = 0;
  if (b != NIL) {
    if (SecureWrite(self, b, bufferSize, flush, flushed,
		    mayBlock, err))
      taken = bufferSize;
  }
  return taken;
}

static          ilu_cardinal
_gss_WriteBytesNonblock(ilu_Transport self, ilu_bytes b,
			ilu_cardinal bufferSize,
			ilu_boolean flush,
			ilu_boolean * flushed,
			ILU_ERRS((IoErrs)) * err)
{
  return _gss_WriteBytes_Full(self, b, bufferSize, flush, flushed,
			      ilu_FALSE, err);
}

/*Main Invariant holds; L2 >= {xmu}*/

static          ilu_boolean
_gss_WriteBytes(ilu_Transport self, ilu_bytes b,
		ilu_cardinal bufferSize,
		ilu_boolean flush,
		ILU_ERRS((IoErrs)) * err)
{
  ilu_boolean     flushed;
  (void) _gss_WriteBytes_Full(self, b, bufferSize, flush, &flushed,
			      ilu_TRUE, err);
  return ILU_ERROK(*err);
}

static          ilu_boolean
_gss_Push(ilu_Transport self,
	  ILU_ERRS((IoErrs)) * err)
{
  return _gss_WriteBytes(self, NIL, 0, ilu_TRUE, err);
}

/*Main Invariant holds; L2 >= {xmu}*/
static          ilu_boolean
_gss_SendWholeMessage(ilu_Transport self, ilu_Message * msgh,
					  ILU_ERRS((IoErrs)) * err)
{
	return ILU_ERR_CONS1(internal, err, minor, ilu_im_tcReliable, ilu_FALSE);
}

/*Main Invariant holds; L2 >= {xmu, ymu}*/
static          ilu_cardinal
_gss_ReadBytes(ilu_Transport self,
			   ilu_bytes buffer,
			   ilu_cardinal len,
			   ilu_TransportReport * rpt,
			   ILU_ERRS((IoErrs)) * err)
{
	SECURITYParms   p = SECURITYPARMS(transport_data(self));
	ilu_bytes	  buf = NIL;
	ilu_cardinal	  buflen = 0;
	gss_status	major, minor;
	gss_buffer_desc	plainbuf, cryptobuf;
	
	rpt->tr_eom = rpt->tr_eof = ilu_FALSE;
	
	if (self->tr_inBuff != NIL &&
		self->tr_inNext != self->tr_inLimit)
		return ILU_ERR_CONS1(internal, err, minor,
		ilu_im_tcInputSkipsBuff, 0);
	
	if (self->tr_inBuff != NIL)
    {
		plainbuf.value = self->tr_inBuff;
		plainbuf.length = self->tr_inLimit;
		(void) gss_release_buffer (&minor, &plainbuf);
		self->tr_inBuff = NIL;
		self->tr_inLimit = 0;
    }
	if (!ReadWholeMessage(p->lower, &buf, &buflen, rpt, ilu_FALSE, err))
		return 0;
	if (rpt->tr_eof && (buflen == 0))
    {
		ILU_CLER(*err);
		return 0;
    }
	
	cryptobuf.value = buf;
	cryptobuf.length = buflen;
	plainbuf.length = 0;
	plainbuf.value = NIL;
	major = gss_unwrap(&minor,	/* OUT:  return buffer for minor
		* error code */
		p->context,/* IN:   current security context */
		&cryptobuf,/* IN:   buffer to be unwrapped */
		&plainbuf,	/* OUT:  unwrapped buffer */
					NIL,	/* OUT:  boolean indicating whether
					* confidentiality was used */
					NIL	/* OUT:  gss_qop_t indicating
					* quality of protection used */
					);
	if (GSS_ERROR(major)) {
		ILU_NOTE(SECURITY_DEBUG,
			("_gss_ReadBytes:  gss_unwrap signals error \"%s\" (%u).\n",
			gssStrErr(major), minor));
		MapGSSErrors(major, minor, err, __FILE__, __LINE__);
		if (cryptobuf.value != NIL)
			ilu_free(cryptobuf.value);
		return ilu_FALSE;
	}
	ilu_free (buf);
	
#ifdef ENABLE_DEBUGGING
	
	ILU_NOTE(SECURITY_DEBUG | PACKET_DEBUG,
		("_gss_ReadBytes: unwrapped buffer of %u bytes:\n",
		plainbuf.length));
	if ((ilu_DebugLevel & (PACKET_DEBUG | SECURITY_DEBUG))
		== (PACKET_DEBUG | SECURITY_DEBUG))
		_ilu_debug_DumpPacket(plainbuf.value, plainbuf.length,
		"incoming plain");
	
#endif /* ENABLE_DEBUGGING */
	
	ILU_CLER(*err);
	if (buflen >= plainbuf.length) {
		ilu_cardinal    plainbuflen = plainbuf.length;
		memcpy((void *) buffer, plainbuf.value, plainbuf.length);
		(void) gss_release_buffer(&minor, &plainbuf);
		return (plainbuflen);
	} else {
		self->tr_inBuff = plainbuf.value;
		self->tr_inLimit = plainbuf.length;
		self->tr_inNext = buflen;
		memcpy((void *) buffer, plainbuf.value, buflen);
		return (buflen);
	}
}


/*L1.sup < trmu; self->tr_wc => L1 >= {cmu}; L2 >= {xmu, ymu}*/
static          ilu_boolean
_gss_Close(ilu_Transport self, ilu_integer * fds_released,
		   ILU_ERRS((bad_locks, broken_locks, internal)) * err)
{
	SECURITYParms   p = SECURITYPARMS(transport_data(self));
	ilu_Transport   lt = p->lower;
	gss_buffer_desc	out_token;
	gss_status	major, minor;
	
	*fds_released = 0;
	ILU_NOTE(SECURITY_DEBUG,
		("_gss_Close(%p, lower=%p)\n", self, lt));
	
	out_token.length = 0;
	out_token.value  = NIL;
	if (p->context != NIL)
    {
		major = gss_delete_sec_context (&minor, &p->context, &out_token);
		if (major == GSS_S_NO_CONTEXT)
			;
		else if (GSS_ERROR(major))
		{
			ILU_NOTE(SECURITY_DEBUG,
				("_gss_Close:  gss_del_sec_context(0x%p) signals error \"%s\" (%u).\n",
				p->context, gssStrErr(major), minor));
			MapGSSErrors (major, minor, err, __FILE__, __LINE__);
			if (out_token.value != NIL)
				(void) gss_release_buffer (&minor, &out_token);
			return ilu_FALSE;
		}
		else if (out_token.length > 0)
		{
			if (!(ClearPre(p, lt, ilu_TRUE, err) &&
			   SendWholeMessage(p, lt, out_token.value,
					    out_token.length, ilu_TRUE, err)))
			  ILU_HANDLED(*err);
			if (out_token.value != NIL)
				(void) gss_release_buffer (&minor, &out_token);
		}
    }
	if (self->tr_inBuff != NIL)
    {
		out_token.value = self->tr_inBuff;
		out_token.length = self->tr_inLimit;
		(void) gss_release_buffer (&minor, &out_token);
		self->tr_inBuff = NIL;
		self->tr_inLimit = 0;
    }
	ilu_free (self->tr_outBuff);
	ilu_free (p);
	ilu_free (self);
	return transport_close(lt, fds_released, err);
}



/*L1.sup < trmu; L2 >= {xmu, ymu}*/
static ilu_integer
_gss_MooringEstFDs (ilu_Mooring self, ilu_boolean add)
{
	ilu_Mooring lm =  SECURITYMOORINGPARMS(mooring_data(self))->lower;
	return ((*lm->mo_dfd) (lm, add));
}

/*L1.sup < trmu; L2 >= {xmu, ymu}*/
static          ilu_boolean
_gss_MooringSetReqHandler(ilu_Mooring self, 
						  ilu_TIH tih, 
						  ILU_ERRS((no_memory, imp_limit, no_resources,
						  broken_locks, internal)) * err)
{
	ilu_Mooring	lm = SECURITYMOORINGPARMS(mooring_data(self))->lower;
	return ((*lm->mo_set_req_handler) (lm, tih, err));
}

/*Main Invariant holds; L2 >= {ymu}*/
static          ilu_boolean
_gss_MooringWaitForReq(ilu_Mooring self, int * disabled_p,
					   ILU_ERRS((broken_locks)) * err)
{
	ilu_Mooring	lm = SECURITYMOORINGPARMS(mooring_data(self))->lower;
	return ((*lm->mo_wait_for_req) (lm, disabled_p, err));
}


/* L1 >= {cmu}; L1.sup < trmu; L2 >= {xmu} */
static ilu_boolean
_gss_MooringDisableWait (ilu_Mooring self,
						 ILU_ERRS((broken_locks,
						 bad_param,
						 internal)) *err)
{
	ilu_Mooring	lm = SECURITYMOORINGPARMS(mooring_data(self))->lower;
	return ((*lm->mo_disableWait) (lm, err));
}


/* L1 >= {cmu}; L1.sup < trmu; L2 >= {xmu} */
static ilu_boolean
_gss_MooringEnableWait (ilu_Mooring self,
						ILU_ERRS((broken_locks,
						bad_param,
						internal)) *err)
{
	ilu_Mooring	lm = SECURITYMOORINGPARMS(mooring_data(self))->lower;
	return ((*lm->mo_enableWait) (lm, err));
}



/* Main Invariant holds; L2 >= {xmu, ymu} */
static          ilu_Transport
  _gss_MooringAcceptClient(ilu_Mooring self, ilu_string * tinfo_out,
			   ilu_integer *fds_consumed,
			   ilu_Passport pp,
			   ILU_ERRS((IoErrs)) * err)
{
  SECURITYMooringParms	mp = SECURITYMOORINGPARMS(mooring_data(self));
  ilu_Mooring		lm = SECURITYMOORINGPARMS(mooring_data(self))->lower;
  ilu_Transport		lower;
  ilu_TransportReport	rpt;
  ilu_string		subtinfo;
  gss_status		major, minor, nminor;
  gss_buffer_desc	intoken, outtoken;
  gss_ctx_id_t		context = GSS_C_NO_CONTEXT;
  gss_OID		mech_type;
  gss_name_t		client_name;
  OM_uint32		flags, lifetime;
  ilu_Message		m;
	
  lower = ((*lm->mo_accept_connection)
	   (lm, tinfo_out ? &subtinfo : NIL, fds_consumed, pp, err));
  if (ILU_ERRNOK(*err) || lower == NIL)
    return NIL;
  do {
    rpt.tr_eom = rpt.tr_eof = ilu_FALSE;
    outtoken.value = NIL;
    outtoken.length = 0;
    m.msg_base = NIL;
    m.msg_len = 0;
    if (!ReadWholeMessage(lower, &m.msg_base, &m.msg_len, &rpt,
			  ilu_TRUE, err))
      return NIL;
    intoken.value = m.msg_base;
    intoken.length = m.msg_len;
    major = (gss_accept_sec_context
	     (&minor,
	      &context,		/* OUT: context being established */
	      mp->server_cred,	/* IN: our server credentials */
	      &intoken,		/* IN: client message */
	      GSS_C_NO_CHANNEL_BINDINGS, /* IN:  channel bindings */
	      &client_name,	/* OUT: name of client */
	      &mech_type,	/* OUT: sec mech being used */
	      &outtoken,	/* OUT: possible data for client */
	      &flags,		/* OUT: flags describing context */
	      &lifetime,	/* OUT: lifetime of context */
	      NIL));
    if (intoken.value != NIL)
      (void) gss_release_buffer (&nminor, &intoken);
    if (GSS_ERROR(major)) {
      ILU_NOTE(SECURITY_DEBUG,
	       ("%s  gss_accept_sec_context() signals error \"%s\" (%u).\n",
		"_gss_MooringAcceptClient:", gssStrErr(major), minor));
      MapGSSErrors(major, minor, err, __FILE__, __LINE__);
      if (outtoken.value != NIL)
	(void) gss_release_buffer(&nminor, &outtoken);
      return NIL;
    }
    if (outtoken.length > 0) {
      if (!SendWholeMessage(NIL, lower, outtoken.value, outtoken.length,
			    ilu_TRUE, err)) {
	(void) gss_release_buffer(&nminor, &outtoken);
	return NIL;
      }
    }
    if (outtoken.value != NIL)
      (void) gss_release_buffer (&nminor, &outtoken);
  } while ((GSS_SUPPLEMENTARY_INFO(major) & GSS_S_CONTINUE_NEEDED) != 0);
	
  if (tinfo_out) {
    *tinfo_out = _ilu_Strcat3("gss", " over ", subtinfo);
    if (*tinfo_out == NIL)
      return ILU_ERR_CONS1(no_memory, err, nbytes,
			   10 + strlen(subtinfo), NIL);
  }
	
  if (pp) {
    ilu_IdentityInfo info = AcquireGSSIdentity(NIL, context, err);
    info->ii_owned_by_passport = ilu_TRUE;
    if (ILU_ERRNOK(*err)) return NIL;
    if (!ilu_AddIdentity(pp, (ilu_IdentityInfo) info, err))
      return NIL;
  }
	
  return NewTrans(lower, mech_type, context, flags, lifetime, err);
}


/* L1.sup < trmu; L1 >= {cmu}; L2 >= {xmu, ymu}*/
static          ilu_boolean
_gss_MooringClose(ilu_Mooring self,
				  ilu_integer * fds_released,
				  ILU_ERRS((bad_locks, broken_locks,
				  internal)) * err)
{
	SECURITYMooringParms p = SECURITYMOORINGPARMS(mooring_data(self));
	gss_status major, minor;
	
	ILU_NOTE(SECURITY_DEBUG,
		("_gss_MooringClose(%p, lower=%p)\n", self, p->lower));
	if (!(*(p->lower)->mo_close) (p->lower, fds_released, err))
		return ilu_FALSE;
	
	/* XXX -- should check for errors here */
	ilu_free (p->server_name); 
	major = gss_release_cred (&minor, &p->server_cred);
	
	ilu_free(p);
	ilu_free(self);
	return ilu_TRUE;
}



/*L1, L2 unconstrained*/
static struct _ilu_Mooring_s mooringProto = {
	_gss_MooringEstFDs,
		_gss_MooringSetReqHandler,
		_gss_MooringWaitForReq,
		_gss_MooringDisableWait,  
		_gss_MooringEnableWait,
		_gss_MooringAcceptClient,
		_gss_MooringClose,
		NIL				/* data */
};


static SECURITYMooringParms NewMooringParms (ilu_Mooring lower,
											 ilu_string server_name,
											 gss_cred_id_t cred,
											 gss_OID secmech,
											 OM_uint32 lifetime,
											 ilu_Error *err)
{
	SECURITYMooringParms p = (SECURITYMooringParms) ilu_malloc(sizeof(struct securitymooringparms));
	static const ilu_FineTime never = { 0, 0 };
	
	if (p == NIL)
		return (ILU_ERR_CONS1(no_memory, err, nbytes, sizeof(struct securitymooringparms), NIL));
	p->server_name = _ilu_Strdup(server_name);
	p->lower = lower;
	p->secmech = secmech;
	p->server_cred = cred;
	if (lifetime == GSS_C_INDEFINITE)
		p->server_cred_max_time = never;
	else
    {
		double l = lifetime;
		p->server_cred_max_time = ilu_FineTime_Add(ilu_FineTime_Now(), ilu_FineTime_FromDouble(l));
    }
	return p;
}

/* Main Invariant holds */
static          ilu_Mooring
  _gss_CreateMooring(ilu_TransportCreator self,
		     ilu_TransportInfo * tinfo_out,
		     ilu_boolean buffer,
		     ilu_integer * fds_consumed,
		     ilu_Passport pp,
		     ILU_ERRS((no_memory)) * err)
{
  CreatorParms    cp = (CreatorParms) self->tcr_data;
  SECURITYMooringParms mp;
  ilu_Mooring     lower, ans;
  ilu_string	  coded_server_name;
  ilu_TransportInfo subtinfo = NIL;
  ilu_cardinal len;
  gss_cred_id_t cred = GSS_C_NO_CREDENTIAL;
  ilu_IdentityInfo ident;
  OM_uint32 major, minor;
  gss_name_t name;
  OM_uint32 lifetime;
  gss_cred_usage_t usage;
	
  /* create a mooring for whatever is below us, getting its tinfo into subtinfo
     if we need it, and since we don't use any fd's ourselves, just pass in where
     the lower mooring should store the fds it consumes */
  lower = ((*cp->lower->tcr_createMooring)
	   (cp->lower, tinfo_out ? &subtinfo : NIL, ilu_FALSE, fds_consumed, pp, err));
  if (ILU_ERRNOK(*err))
    return NIL;
	
  /* now make up a mooring struct for us */
  ans = (ilu_Mooring) ilu_malloc(sizeof(*ans));
  if (ans == NIL)
    return (ILU_ERR_CONS1(no_memory, err, nbytes, sizeof(*ans), NIL));
	
  /* establish an identity.  Check the passport to see if there's a GSS identity of
     the correct type already in there.  */
  if ((pp != NIL) &&
      ((ident = (ilu_IdentityInfo) ilu_FindIdentity(pp, ilu_GSSIdentity)) != NIL) &&
      (((struct _ilu_GSSIdentityInfo_s*)(ident->ii_info))->cred != NIL))
    {
      cred = ((struct _ilu_GSSIdentityInfo_s*)(ident->ii_info))->cred;
      major = gss_inquire_cred_by_mech (&minor, cred, cp->secmech, &name, NIL, &lifetime, &usage);
      if (GSS_ERROR(major))
	{
	  ILU_NOTE(SECURITY_DEBUG,
		   ("_gss_CreateMooring:  gss_inquire_cred_by_mech() signals error \"%s\" (%u).\n",
		    gssStrErr(major), minor));
	  return ILU_ERR_CONS0(no_permission, err, NIL);
	}
      if (usage == GSS_C_INITIATE)
	{
	  ILU_NOTE(SECURITY_DEBUG,
		   ("_gss_CreateMooring:  GSS credential only allows initiation of contexts\n"));
	  return ILU_ERR_CONS0(no_permission, err, NIL);
	}
    }
  else
    {
      return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_nil, NIL);
    }
	
  if ((cp->server_name = NameTToString(name, err)), ILU_ERRNOK(*err))
    return NIL;
	
  /* cons up the tinfo string */
  if (tinfo_out) {
    char *myinfo;
		
    if ((coded_server_name = encode(cp->server_name)) == NIL)
      return ILU_ERR_CONS1(no_memory, err, nbytes, strlen(cp->server_name), NIL);
    len = 6			/* "gss_1_" */
      + strlen(cp->secmech_name)
	+ 1			/* "_" */
	  + strlen(coded_server_name)
	    + 1			/* trailing NUL */;
    myinfo = (ilu_string) ilu_malloc(len);
    if (myinfo == NIL)
      {
	ilu_integer fds_released = 0;
			
	ilu_free(ans);
	(void) ilu_CloseMooring(lower, &fds_released, err);
	ILU_HANDLED(*err);
	*fds_consumed -= fds_released;
	return ILU_ERR_CONS1(no_memory, err, nbytes, len, NIL);
      }
    sprintf (myinfo, "gss_1_%s_%s", cp->secmech_name, coded_server_name);
		
    /* add on the tinfo of the morring below us */
    *tinfo_out = _ilu_ConcatTinfo (myinfo, subtinfo, err);
    ilu_free(myinfo);
    if (ILU_ERRNOK(*err))
      {
	ilu_integer fds_released = 0;
			
	ilu_free(ans);
	(void) ilu_CloseMooring(lower, &fds_released, err);
	ILU_HANDLED(*err);
	*fds_consumed -= fds_released;
	return ILU_ERR_CONS1(no_memory, err, nbytes, len, NIL);
      }
    ilu_free(coded_server_name);
    ilu_free(subtinfo);
  }
  ILU_NOTE(SECURITY_DEBUG, ("_gss_CreateMooring(%p, lower=%p)\n", ans, lower));
	
  if ((mp = NewMooringParms(lower, cp->server_name, cred, cp->secmech,
			    GSS_C_INDEFINITE, err)) == NIL) {
    ILU_ERRS((bad_locks, broken_locks, internal)) lerr;
    ilu_integer     fds_released = 0;

    ilu_free(ans);
    (void) ilu_CloseMooring(lower, &fds_released, &lerr);
    ILU_HANDLED(lerr);
    *fds_consumed -= fds_released;
    if (tinfo_out)
      ilu_free(*tinfo_out);
    return NIL;
  }
  *ans = mooringProto;
  ans->mo_data = (ilu_refany) mp;
  ans->mo_wc = lower->mo_wc;
  return (ans);
}


/* L1, L2 unconstrained */
static void _gss_CloseCreator(ilu_TransportCreator self)
{
	CreatorParms    cp = (CreatorParms) self->tcr_data;
	
	/* close anyone beneath us */
	(*cp->lower->tcr_close) (cp->lower);
	
	/* release our storage */
	ilu_free(cp);
	ilu_free(self);
	return;
}


static struct _ilu_TransportClass_s _gss_transport_class = {
	ilu_FALSE,            /* boundaried */
		ilu_TRUE,             /* reliable */
		_gss_SetInputHandler,
		_gss_WaitForInput,
		_gss_Interrupt,
		_gss_DisableWait,
		_gss_EnableWait,
		_gss_BeginMessage,
		_gss_EndMessage,
		_gss_BeginOutputMessageNonblock,
		_gss_EndOutputMessageNonblock,
		_gss_Push,
		_gss_SendWholeMessage,
		_gss_WriteBytes,
		_gss_WriteBytesNonblock,
		_gss_ReadBytes,
		_gss_Close
};


static struct _ilu_TransportCreator_s _gss_CreatorProto = {
	ilu_FALSE,             /* boundaried */
		ilu_TRUE,              /* reliable */
		0,                 /* tcr_holds */
		ilu_FALSE,             /* tcr_wantClose */
		_gss_FdUsage,
		_gss_CreateTransport,
		_gss_CreateMooring,
		_gss_CloseCreator,
		NIL                /* data */
};


/* L1.sup < trmu; L2 unconstrained */
ilu_TransportCreator
_ilu_gss_TransportCreator(ilu_TransportInfo tinfo,
						  ILU_ERRS((no_memory, inv_objref)) * err)
{
	ilu_TransportCreator new_creator;
	CreatorParms    create_params;
	
	/* get the parameters the transport should be created with based on the tinfo */
	create_params = _gss_InterpretInfo(tinfo, err);
	if (ILU_ERRNOK(*err))
		return NIL;
	
	/* make up a new creator, initialized from our prototype */
	new_creator = (ilu_TransportCreator) ilu_malloc(sizeof(*new_creator));
	if (new_creator == NIL)
		return ILU_ERR_CONS1(no_memory, err, nbytes, sizeof(*new_creator), NIL);
	*new_creator = _gss_CreatorProto;
	
	/* put the tinfo specific create params  in */
	new_creator->tcr_data = create_params;
	ILU_CLER(*err);
	return new_creator;
}



/* formats a string form of the IdentityInfo into the caller-supplied
   buffer.  If the buffer is too small for the identity, it may either
   truncate the identity and return success, or signal an error */
static ilu_cardinal				/* size of return string */
  _ilu_GSSIdentity_StringForm (ilu_refany info,	/* instance, retain */
			       char * pc_caller_buffer,		/* RETAIN: address of caller buffer */
			       ilu_cardinal card_buffer_size,	/* size of caller buffer */
			       ILU_ERRS((internal, bad_param)) *err)
{
  struct _ilu_GSSIdentityInfo_s* ident = (struct _ilu_GSSIdentityInfo_s *) info;
  OM_uint32 major, minor, lifetime32, flags = 0, needed;
  gss_OID mechtype;
  ilu_string src_name_str;
  ilu_string targ_name_str;
  gss_buffer_desc mech_name = GSS_C_EMPTY_BUFFER;
  gss_name_t src_name, targ_name;
  const ilu_string format_context = "<GSS context:scheme=%*.*s;src=%s;targ=%s;lifetime=%s>";
  const ilu_string format_no_context = "<GSS credential:name=%s;lifetime=%s>";
  char lifetime[32];

  if (ident==NIL)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_nil, ilu_FALSE);
  if (((ident->context==NIL)&&(ident->cred==NIL))||
      ((ident->context!=NIL)&&(ident->cred!=NIL)))
    return ILU_ERR_CONS1(internal, err, minor, ilu_im_broken, ilu_FALSE);
  
  if (ident->context != NIL)
    major = gss_inquire_context (&minor, ident->context, &src_name, &targ_name,
				 &lifetime32, &mechtype, &flags, NIL, NIL);
  else {
    major = gss_inquire_cred (&minor, ident->cred, &src_name,
			      &lifetime32, NIL, NIL);
  }
  if (GSS_ERROR(major))
    {
      ILU_NOTE(SECURITY_DEBUG,
	       ("(ilu_GSSIdentity_StringForm):  gss_inquire_%s() signals error \"%s\" (%u).\n",
		(ident->context == NIL) ? "cred_by_mech" : "context", gssStrErr(major), minor));
      MapGSSErrors (major, minor, err, __FILE__, __LINE__);
      return ilu_FALSE;
    }
  if (lifetime32 == GSS_C_INDEFINITE)
    strcpy (lifetime, "indefinite");
  else
    sprintf (lifetime, "%lu", (long unsigned int) lifetime32);
  src_name_str = ilu_GSSNameToString(src_name, err);
  if (ILU_ERRNOK(*err))
    return 0;
  if (ident->context != NIL) {
    targ_name_str = ilu_GSSNameToString(targ_name, err);
    if (ILU_ERRNOK(*err)) {
      ilu_free(src_name_str);
      return 0;
    };
    major = gss_oid_to_str(&minor, mechtype, &mech_name);
    if (GSS_ERROR(major)) {
      ilu_free(src_name_str);
      ilu_free(targ_name_str);
      MapGSSErrors (major, minor, err, __FILE__, __LINE__);
      return 0;
    }
    needed = strlen(format_context) +
      strlen(src_name_str) +
	strlen(targ_name_str) +
	  mech_name.length +
	    strlen(lifetime);
    if (needed > card_buffer_size)
      return (ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_small_buffer, needed));
    else
      sprintf (pc_caller_buffer, format_context,
	       mech_name.length, mech_name.length, mech_name.value,
	       src_name_str, targ_name_str, lifetime);
  } else {
    needed = strlen(format_no_context) +
      strlen(src_name_str) +
	strlen(lifetime);
    if (needed > card_buffer_size)
      return (ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_small_buffer, needed));
    else
      sprintf (pc_caller_buffer, format_no_context, src_name_str, lifetime);
  }
  ILU_CLER(*err);
  return strlen(pc_caller_buffer);
}


/* returns a deep copy of the "ii_info" field of the argument. */
static ilu_refany
  _ilu_GSSIdentity_DuplicateData (ilu_refany info,	/* instance, retain */
				  ILU_ERRS((no_memory, internal)) *err)
{
  struct _ilu_GSSIdentityInfo_s* ident = (struct _ilu_GSSIdentityInfo_s *) info;
  
  if (ident==NIL)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_nil, NIL);

  ident->refcount++;
  ILU_CLER(*err);
  return ident;
}


/* frees any associated data structure pointed to by the "ii_info" field */
static void
  _ilu_GSSIdentity_FreeData (ilu_refany info,
			     ILU_ERRS((internal)) *err)
{
  struct _ilu_GSSIdentityInfo_s* ident = (struct _ilu_GSSIdentityInfo_s *) info;
  OM_uint32 major, minor;
  
  ILU_CLER(*err);
  
  if (ident==NIL)
    return;
  if (ident->refcount < 2) {
    if (ident->cred != NIL) {
      major = gss_release_cred(&minor, &ident->cred);
      if (GSS_ERROR(major)) {
	MapGSSErrors (major, minor, err, __FILE__, __LINE__);
	return;
      }
    };
    /* We don't do anything with the context, because that is owned by
       the transport, not by the identity */
    ilu_free(ident);
  } else {
    ident->refcount--;
  }
}


struct _ilu_IdentityType_s ilu_GSSIdentity_s   = {
  "GSSIdentity",
  _ilu_GSSIdentity_StringForm,   
  _ilu_GSSIdentity_DuplicateData,
  _ilu_GSSIdentity_FreeData,
  NULLFN,  
  NULLFN 
  };


/***********************************************************************
The two public routines to manipulate GSS Identities
***********************************************************************/

ilu_boolean
  ilu_DecodeGSSIdentity (ilu_IdentityInfo idinfo,
			 gss_name_t *who,
			 ilu_FineTime *good_til,
			 gss_OID desired_mech,
			 ilu_boolean *localp,
			 ilu_cardinal *conn_flags,
			 ilu_Error *err)
{
  struct _ilu_GSSIdentityInfo_s* ident;
  OM_uint32 major, minor, lifetime32, flags = 0;
  gss_OID mechtype;
  
  if (idinfo == NIL)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_nil, ilu_FALSE);
  if (idinfo->ii_type != ilu_GSSIdentity)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, ilu_FALSE);
  else
    ident = idinfo->ii_info;
  
  if (ident==NIL)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_nil, ilu_FALSE);
  if (((ident->context==NIL)&&(ident->cred==NIL))||
      ((ident->context!=NIL)&&(ident->cred!=NIL)))
    return ILU_ERR_CONS1(internal, err, minor, ilu_im_broken, ilu_FALSE);
  
  if (ident->context != NIL)
    {
      major = gss_inquire_context (&minor, ident->context, who,
				   NIL, &lifetime32, &mechtype, &flags, NIL, NIL);
      if (desired_mech != NIL && !compare_oids(desired_mech, mechtype))
	{
	  ILU_CLER(*err);
	  return ilu_FALSE;
	}
    }
  else
    {
      if (desired_mech == NIL)
	major = gss_inquire_cred (&minor, ident->cred, who,
				  &lifetime32, NIL, NIL);
      else
	major = gss_inquire_cred_by_mech (&minor, ident->cred, desired_mech, who,
					  &lifetime32, NIL, NIL);
    }
  if (GSS_ERROR(major))
    {
      ILU_NOTE(SECURITY_DEBUG,
	       ("ilu_DecodeGSSIdentity:  gss_inquire_%s() signals error \"%s\" (%u).\n",
		(ident->context == NIL) ? "cred_by_mech" : "context", gssStrErr(major), minor));
      MapGSSErrors (major, minor, err, __FILE__, __LINE__);
      return ilu_FALSE;
    }
  if (conn_flags != NIL)
    *conn_flags = flags;
  if (good_til != NIL)
    {
      if (lifetime32 == GSS_C_INDEFINITE)
	{
	  double t1 = 0.0;
	  *good_til = ilu_FineTime_FromDouble(t1);
	}
      else
	{
	  double t1 = lifetime32;
	  *good_til = ilu_FineTime_Add(ilu_FineTime_Now(), ilu_FineTime_FromDouble(t1));
	}
    }
  if (localp != NIL)
    *localp = (ident->context == NIL);
  ILU_CLER(*err);
  return ilu_TRUE;
}

gss_cred_id_t
  ilu_AcquireGSSCredForName (char *name,
			     ilu_cardinal lifetime,
			     gss_OID secmech,
			     ilu_boolean accept_only,
			     ilu_Error *err)
{
  gss_status major, minor;
  gss_OID_set oidset = NIL;
  gss_name_t gssname;
  gss_cred_id_t cred;
	
  major = gss_create_empty_oid_set (&minor, &oidset);
  major = gss_add_oid_set_member (&minor, secmech, &oidset);
  cred = GSS_C_NO_CREDENTIAL;
  if (name == NIL)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_nil, cred);
  else
    {
      if ((gssname = StringToNameT (name, err)) == NIL)
	return cred;
    }
  major = gss_acquire_cred (&minor,
			    gssname,		/* IN:  name of principal to acquire cred of */
			    lifetime,		/* IN:  # of seconds cred should be good for */
			    oidset,		/* IN:  set of sec mechs to use */
			    (accept_only ? GSS_C_ACCEPT : GSS_C_INITIATE),
			    &cred,		/* OUT:  the actual cred */
			    NIL,		/* OUT:  mechs for which the credential is valid */
			    NIL);		/* OUT:  actual lifetime of cred */
  if (GSS_ERROR(major))
    {
      ILU_NOTE(SECURITY_DEBUG,
	       ("(gss.c:GetCredForName):  gss_acquire_cred(\"%s\") signals error \"%s\" (%u).\n",
		name, gssStrErr(major), minor));
      MapGSSErrors (major, minor, err, __FILE__, __LINE__);
      return cred;
    }
  major = gss_release_oid_set (&minor, &oidset);
  ILU_CLER(*err);
  return cred;
}

ilu_string
  ilu_GSSNameToString (gss_name_t name, ilu_Error *err)
{
  return (NameTToString (name, err));
}

#else

/* L1.sup < trmu; L2 unconstrained */
ilu_TransportCreator
_ilu_gss_TransportCreator(ilu_TransportInfo tinfo,
						  ILU_ERRS((no_memory, inv_objref)) * err)
{
	return 0;
}

#endif
