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
/* $Id: object.c,v 1.205 1999/08/30 22:19:30 janssen Exp $ */
/* Last edited by Mike Spreitzer September 22, 1998 10:55 pm PDT */

#define _POSIX_SOURCE

#include "iluntrnl.h"

#include <ctype.h>

#include "object.h"
#include "server.h"
#include "type.h"
#include "iluprotocol.h"
#include "port.h"

#include "version.h"	/* defines ILU_VERSION */

#define CORBA_NATIVE_OBJECT_IH_PREFIX		"ilu--corba-native-object:"
#define SIZEOF_CORBA_NATIVE_OBJECT_IH_PREFIX	(sizeof(CORBA_NATIVE_OBJECT_IH_PREFIX)-1)

/*L1, L2, Main unconstrained*/

static const ilu_string _ilu_id = "$" "Id: ILU version " ILU_VERSION_STRING ".  Copyright 1990-1998 Xerox Corporation.  All Rights Reserved. $" ;
static const ilu_string _ilu_version = ILU_VERSION_STRING ;
static const ilu_cardinal _ilu_major_version = ILU_MAJOR_VERSION ;
static const ilu_cardinal _ilu_minor_version = ILU_MINOR_VERSION ;

/*L1 >= {cmu}*/
static ilu_ObjectNoter theNoters[ILU_MAX_ADDR_SPACE_LANGUAGES] = {NULLFN};
/* The proc to call when the "very interested" predicate changes. */

/*L1_sup < cmu*/
void ilu_SetNoter(ilu_ObjectNoter n, ilu_cardinal lang)
{
  _ilu_Assert(n!=NULLFN, "SetNoter: given NIL noter");
  _ilu_Assert(lang<=_ilu_NLanguages, "SetNoter: unknown language specified");
  _ilu_Assert(theNoters[lang]==NULLFN, "SetNoter: already set");
  _ilu_AcquireMutex(ilu_cmu);
  theNoters[lang] = n;
  _ilu_ReleaseMutex(ilu_cmu);
  return;
}

/*L1, L2, Main unconstrained*/

ilu_string ilu_GetILUVersion (void)
{
  return (_ilu_version);
}

ilu_cardinal ilu_GetILUMajorVersion (void)
{
  return (_ilu_major_version);
}

ilu_cardinal ilu_GetILUMinorVersion (void)
{
  return (_ilu_minor_version);
}

ilu_string ilu_GetILUTypeUIDVersion (void)
{
  return (ILU_TYPEUID_VERSION_STRING);
}

#define ISSAFE(c)	(isalnum(c)||((c)=='$')||((c)=='.')||((c)=='+')||((c)=='-')||((c)=='_'))

static const char hextable[] = "0123456789ABCDEF";
#define HEXDIGIT(x)	(hextable[x])
#define HEXVALUE(x)	\
	 (((x)>='0'&&(x)<='9')?((x)-'0')\
	:(((x)>='A'&&(x)<='F')?((x)-'A'+10)\
	:(((x)>='a'&&(x)<='f')?((x)-'a'+10):16)))

/*
 * Copy s into b, quoting unsafe chars.  Writes the terminating 0.
 * Caller has ensured b has enough space.  Returns NIL on error (not
 * enough space), pointer to terminating 0 if success.
 */
static char    *
QuoteBuffer(char *s, ilu_cardinal slen,
	    char *b, ilu_cardinal blen,
	    ILU_ERRS((internal)) * err)
{
  unsigned char  *sp = (unsigned char *) s, *slim = ((unsigned char *) s) + slen;
  char           *bp = b, *blim = b + blen;

  for (; sp < slim; sp++)
    if (ISSAFE(*sp)) {
      if (!ilu_Check(bp < blim, err))
	return NIL;
      *bp++ = (char) *sp;
    } else {
      if (!ilu_Check(bp + 3 <= blim, err))
	return NIL;
      *bp++ = '%';
      *bp++ = HEXDIGIT((*sp >> 4) & 0xF);
      *bp++ = HEXDIGIT(*sp & 0xF);
    }
  if (!ilu_Check(bp < blim, err))
    return NIL;
  *bp = 0;
  return bp;
}

static ilu_cardinal QuotedLength(char *s, ilu_cardinal slen)
{
  ilu_cardinal    ans = 0;
  unsigned char  *sp = (unsigned char *) s, *slim = ((unsigned char *) s) + slen;
  for (; sp < slim; sp++) {
      if (ISSAFE(*sp)) {
	  ans += 1;
      } else {
	  ans += 3;
      }
  }
  return ans;
}

ilu_string
_ilu_EncodeBuffer(char *b, ilu_cardinal len,
		  ILU_ERRS((internal, no_memory)) * err)
{
  ilu_string      ans;
  ilu_cardinal    anslen;
  anslen = QuotedLength(b, len) + 1;
  ans = (ilu_string) ilu_MallocE(anslen, err);
  if (ans == NIL)
    return NIL;
  if (!QuoteBuffer(b, len, ans, anslen, err))
    return NIL;
  return ans;
}

/*
 * Copy s into b, turning quoted chars into plain chars; return next
 * position in b after terminating 0.  Raise inv_objref/minor on
 * quoting botch.  Raise internal/check if b isn't long enough.
 */
static          ilu_string
DeQuoteBuffer(char *s, ilu_cardinal slen,
	      char *b, ilu_cardinal blen, ilu_cardinal * out_len,
	      ILU_ERRS((marshal, internal)) * err)
{
  char           *sp = s, *slim = s + slen;
  char           *bp = b, *blim = b + blen;
  for (; (sp < slim) && (bp < blim); bp++)
    if (*sp == '%') {
      int             hv1, hv2;
      if ((slim - sp <= 2)
	  || ((hv1 = HEXVALUE(sp[1])) == 16)
	  || ((hv2 = HEXVALUE(sp[2])) == 16))
	return ILU_ERR_CONS1(marshal, err, minor, ilu_mm_url_quoted_char,
			     NIL);
      *bp = (char) ((hv1 << 4) + hv2);
      sp += 3;
    } else
      *bp = *sp++;
  if (!ilu_Check(sp == slim && bp < blim, err))
    return NIL;
  *out_len = bp - b;
  *bp++ = 0;
  return (bp);
}

ilu_string
  _ilu_DecodeBuffer (char *b, ilu_cardinal len, ilu_cardinal *out_len,
		     ILU_ERRS((internal, no_memory, marshal)) * err)
{
  char		*p, *plim = b + len, *ans;
  ilu_cardinal	count = 0, i;
  for (p = b; p < plim;)
    if (*p == '%') {
      count++;
      p += 3;
    } else
      p++;

  ans = (char *) ilu_MallocE(i = len - (count * 2) + 1, err);
  if (ans == NIL)
    return NIL;
  if (DeQuoteBuffer(b, len, ans, i, out_len, err) == NIL)
    return NIL;
  return ans;
}

ilu_boolean
ilu_Append1Cinfo(ilu_CharBuf * cinfo, ilu_string pinfo,
		 ilu_TransportInfo tinfo,
		 ILU_ERRS((no_memory)) * err)
{
  ilu_cardinal    plen, tlen, dlen;
  char           *s, *t;
  plen = QuotedLength(pinfo, strlen(pinfo));
  tlen = _ilu_TinfoStringLength(tinfo);
  dlen = (cinfo->icb_len > 0) + plen + 1 + tlen;
  if (!ilu_CharBufReserve(cinfo, dlen, err))
    return ilu_FALSE;
  s = cinfo->icb_base + cinfo->icb_len;
  t = s + dlen;
  if (cinfo->icb_len > 0)
    *s++ = ILU_CINFO_MARKER;
  if (!(s = QuoteBuffer(pinfo, strlen(pinfo), s, t - s, err)))
    return ilu_FALSE;
  if (!ilu_Check(s < t, err))
    return ilu_FALSE;
  *s++ = ILU_CINFO_DIVIDER;
  if (!(s = _ilu_StringifyTinfoToBuffer(tinfo, s, t - s, err)))
    return ilu_FALSE;
  if (!ilu_Check(s == t, err))
    return ilu_FALSE;
  cinfo->icb_len = s - cinfo->icb_base;
  *s = 0;
  return ilu_TRUE;
}

static ilu_boolean
  Parse_ILU_SBH(ilu_string encodedSBH,
		ilu_string * plainInstanceHandle,
		ilu_string * plainServerID,
		ilu_string * plainMstid,
		ilu_string * encodedContactInfo,
		ilu_cardinal * encodedContactInfoLen,
		ilu_boolean * passEncodedContactInfo,
		ILU_ERRS((no_memory, internal, inv_objref)) * err)
{
  char           *p, *sid, *ih, *mstid, *cinfo;
  ilu_cardinal	outlen, prefix_len;

  ILU_CLER(*err);

  ILU_NOTE(OBJECT_DEBUG,
	("ILU: (object.c/Parse_ILU_SBH):  sbh=<%s>\n", encodedSBH));

  if (strncmp(encodedSBH, "ilusbh:", 7) == 0)
    prefix_len = 7;
#ifndef ILU_DISALLOW_OLD_SBH_FORM
  else if (strncmp(encodedSBH, "ilu:", 4) == 0)
    prefix_len = 4;
#endif
  else
    return ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_bad_url_scheme,
			 ilu_FALSE);

  /* server ID */
  sid = encodedSBH + prefix_len;
  if ((p = strchr(sid, '/')) == NIL)
    return ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_sid, ilu_FALSE);
  ILU_NOTE(OBJECT_DEBUG,
	("ILU: (object.c/Parse_ILU_SBH):  encoded sid=<%*.*s>\n",
	 p - sid, p - sid, sid));
  if (plainServerID != NIL) {
    *plainServerID = (char *) _ilu_DecodeBuffer(sid, p - sid, &outlen, err);
    if (ILU_ERRNOK(*err) && (err->ilu_type == ILU_ERRTYP(marshal))) {
      ILU_HANDLED(*err);
      return ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_sid, ilu_FALSE);
    };
    if (*plainServerID == NIL)
      return ilu_FALSE;
  };

  /* instance handle */
  ih = p + 1;
  if ((p = strchr(ih, ILU_TYPE_MARKER)) == NIL) {
    if (plainServerID != NIL)
      ilu_free(*plainServerID);
    return ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_ih, ilu_FALSE);
  }
  ILU_NOTE(OBJECT_DEBUG,
	("ILU: (object.c/Parse_ILU_SBH):  encoded ih=<%*.*s>\n",
	 p - ih, p - ih, ih));
  if (plainInstanceHandle != NIL) {
    *plainInstanceHandle = (char *) _ilu_DecodeBuffer(ih, p - ih, &outlen, err);
    if (ILU_ERRNOK(*err) && (err->ilu_type == ILU_ERRTYP(marshal))) {
      ILU_HANDLED(*err);
      return ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_ih, ilu_FALSE);
    };
    if (*plainInstanceHandle == NIL) {
      if (plainServerID != NIL)
	ilu_free(*plainServerID);
      return ilu_FALSE;
    }
  };

  /* Most Specific Type ID */
  mstid = p + 1;
  if ((p = strchr(mstid, ILU_CINFO_MARKER)) == NIL) {
    if (plainServerID != NIL)
      ilu_free(*plainServerID);
    if (plainInstanceHandle != NIL)
      ilu_free(*plainInstanceHandle);
    return ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_bad_mstid,
			 ilu_FALSE);
  }
  ILU_NOTE(OBJECT_DEBUG,
	("ILU: (object.c/Parse_ILU_SBH):  encoded mstid=<%*.*s>\n",
	 p - mstid, p - mstid, mstid));
  if (plainMstid != NIL) {
    *plainMstid = (char *) _ilu_DecodeBuffer(mstid, p - mstid, &outlen, err);
    if (ILU_ERRNOK(*err) && (err->ilu_type == ILU_ERRTYP(marshal))) {
      ILU_HANDLED(*err);
      return ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_bad_mstid, ilu_FALSE);
    };
    if (*plainMstid == NIL) {
      if (plainServerID != NIL)
	ilu_free(*plainServerID);
      if (plainInstanceHandle != NIL)
	ilu_free(*plainInstanceHandle);
      return ilu_FALSE;
    }
  };

  /* Contact Info.s */
  cinfo = p + 1;
  ILU_NOTE(OBJECT_DEBUG,
	("ILU: (object.c/Parse_ILU_SBH):  encoded cinfo=<%s>\n", cinfo));
  if (encodedContactInfo != NIL)
    *encodedContactInfo = cinfo;
  if (passEncodedContactInfo != NIL)
    *passEncodedContactInfo = ilu_FALSE;
  if (encodedContactInfoLen != NIL)
    *encodedContactInfoLen = strlen(cinfo);

  return (ilu_TRUE);
}

static HashTable RegisteredParsers = NIL;

void ilu_RegisterSBHParser (ilu_string sbh_scheme, ilu_SBHParser parser)
{
  if (RegisteredParsers == NIL)
    RegisteredParsers = ilu_hash_MakeNewTable(13, ilu_hash_HashString,
					       ilu_hash_StringCompare);
  ilu_hash_AddToTable (RegisteredParsers, _ilu_Strdup(sbh_scheme), (ilu_private) parser);
}

ilu_boolean
  ilu_ParseSBH(ilu_string encodedSBH,
	       ilu_string * plainInstanceHandle,
	       ilu_string * plainServerID,
	       ilu_string * plainMstid,
	       ilu_string * encodedContactInfo,
	       ilu_cardinal * encodedContactInfoLen,
	       ilu_boolean * passEncodedContactInfo,
	       ILU_ERRS((no_memory, internal, inv_objref)) * err)
{
  char *ptr;
  char scheme_buffer[128];
  ilu_SBHParser parser;
  static ilu_boolean initted = ilu_FALSE;

  if (! initted)
    {
#ifndef ILU_DISALLOW_OLD_SBH_FORM
      ilu_RegisterSBHParser ("ilu", Parse_ILU_SBH);
#endif
      ilu_RegisterSBHParser ("ilusbh", Parse_ILU_SBH);
#ifdef IIOP_PROTOCOL
      ilu_RegisterSBHParser ("IOR", _ilu_IIOP_ParseIOR);
      ilu_RegisterSBHParser ("ior", _ilu_IIOP_ParseIOR);
      ilu_RegisterSBHParser ("IOR2", _ilu_IIOP_ParseIOR2);
      ilu_RegisterSBHParser ("iiop", _ilu_IIOP_ParseIIOP);
      ilu_RegisterSBHParser ("iioploc", _ilu_IIOP_ParseIIOPLoc);
      ilu_RegisterSBHParser ("iiopname", _ilu_IIOP_ParseIIOPName);
#endif
#ifdef W3NG_PROTOCOL
      ilu_RegisterSBHParser ("w3ng", _ilu_w3ng_ParseURL);
#endif
#ifdef HTTP_PROTOCOL
      ilu_RegisterSBHParser ("http", _ilu_Parse_HTTP_URL);
#endif
      initted = ilu_TRUE;
    }

  ptr = strchr (encodedSBH, ':');
  if (ptr == NIL || (ptr - encodedSBH) > 127)
    return ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_bad_url_scheme, ilu_FALSE);
  strncpy (scheme_buffer, encodedSBH, (ptr - encodedSBH));
  scheme_buffer[ptr - encodedSBH] = 0;
  parser = (ilu_SBHParser) ilu_hash_FindInTable (RegisteredParsers, scheme_buffer);
  if (parser == NULLFN)
    return ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_bad_url_scheme, ilu_FALSE);
  else
    return (*parser)(encodedSBH, plainInstanceHandle, plainServerID, plainMstid,
		     encodedContactInfo, encodedContactInfoLen, passEncodedContactInfo, err);
}

ilu_boolean 
_ilu_ParseConnectInfo(ilu_string cinfo,
		      ilu_cardinal cinfolen,
		      ilu_string * plainProtocolInfo,
		      ilu_TransportInfo * transportInfo,
		      ILU_ERRS((no_memory, inv_objref,
				internal)) * err)
{
  char           *plim, *pstart, *pnext;
  ilu_cardinal	outlen;
  if ((plim = strchr(cinfo, ILU_CINFO_MARKER)) == NIL
      || plim > cinfo + cinfolen)
    plim = cinfo + cinfolen;
  if ((pnext = strchr(cinfo, ILU_CINFO_DIVIDER)) == NIL
      || pnext > plim)
    return ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_ci, ilu_FALSE);
  if (plainProtocolInfo != NIL) {
    *plainProtocolInfo = (char *) _ilu_DecodeBuffer(cinfo, pnext - cinfo, &outlen, err);
    if (ILU_ERRNOK(*err) && (err->ilu_type == ILU_ERRTYP(marshal))) {
      ILU_HANDLED(*err);
      return ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_ci, ilu_FALSE);
    };
    if (*plainProtocolInfo == NIL)
      return ilu_FALSE;
  }
  pstart = pnext + 1;
  if (transportInfo != NIL) {
    char           *buf;
    ilu_cardinal    count = 1, len;

    for (buf = strchr(pstart, ILU_TINFO_DIVIDER);
	 buf != NIL && buf < plim;
	 buf = strchr(buf + 1, ILU_TINFO_DIVIDER))
      count++;			/* count filters in tinfo string */
    /*
     * Allocate one piece of memory for both the vector and the
     * strings.
     */
    len = ((count + 1) * sizeof(ilu_string)) + (plim - pstart + 1);
    *transportInfo = (ilu_TransportInfo) ilu_MallocE(len, err);
    if (*transportInfo == NIL) {
      if (plainProtocolInfo != NIL)
	ilu_free(*plainProtocolInfo);
      return ilu_FALSE;
    }
    buf = (char *) ((*transportInfo) + count + 1);
    count = 0;
    while (pstart < plim) {
      pnext = strchr(pstart, ILU_TINFO_DIVIDER);
      if (pnext == NIL || pnext > plim)
	pnext = plim;
      (*transportInfo)[count] = buf;
      buf = DeQuoteBuffer(pstart, pnext - pstart, buf, 1 + pnext - pstart,
			  &outlen, err);
      if (ILU_ERRNOK(*err) && (err->ilu_type == ILU_ERRTYP(marshal))) {
	ILU_HANDLED(*err);
	return ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_ti, ilu_FALSE);
      };
      if (buf == NIL)
	return ilu_FALSE;
      count++;
      pstart = pnext + 1;
    }
    (*transportInfo)[count] = NIL;
  }
  return ilu_TRUE;
}

/*L1, L2, Main unconstrained*/

/*
 * This procedure is used internally for creating both true and
 * surrogate objects.  For true objects, this procedure is called
 * with non-NIL ih, server, cl, and lspo; for surrogate objects,
 * this procedure is called with non-NIL ih, server, and sbh.
 * Storage for ih, sbh, and mstid is owned by the caller.
 */
static ilu_Object CreateObject (ilu_string ih, ilu_Server server,
				ilu_Class class, ilu_refany lspo,
				ilu_string sbh, ilu_string mstid)
{
  ilu_Object new = (ilu_Object) ilu_must_malloc(sizeof(struct _ilu_Object_s));
  ilu_Alarmette_s gco = {NIL, NIL, ilu_FALSE, {0, 0}};
  unsigned        i;
  
  object_ih(new)	= _ilu_Strdup(ih);
  object_server(new)	= server;
  object_timeout(new)	= _ilu_GCTimeOut;
  object_class(new)	= class;
  new->ob_mstid		= _ilu_Strdup(mstid);
  object_sbh(new)	= _ilu_Strdup(sbh);
  for (i = 0; i < ILU_MAX_ADDR_SPACE_LANGUAGES; i++)
    object_lspos(new)[i] = NIL;
  if (server_is_true(server))
    object_lspo(new, server_true_language(server)) = lspo;
  object_holds(new)	= 0;
  object_intNoted(new)	= (int) ilu_FALSE;
  if (object_is_true(new)) {
    object_gco(new) = gco;
    object_lastRemote(new) = 0;
    object_gclist(new) = NIL;
  } else {
    object_notifying(new) = ilu_FALSE;
    object_known(new) = ilu_FALSE;
  }
  return (new);
}

/*Destroy an object created but not yet put into the server's object table. */
static void UncreateObject(ilu_Object o)
{
  ilu_free(o->ob_ih);
  if (object_mstid(o) != NIL)
    ilu_free(object_mstid(o));
  if (o->ob_sbh != NIL)
    ilu_free(o->ob_sbh);
  ilu_free(o);
}

/*L1 >= {s}*/

ilu_boolean _ilu_Addable(ilu_Server s, ilu_Class t, ilu_Object *h)
{
  ilu_cardinal i, l;
  if (!class_singleton(t))
      return ilu_TRUE;
  if (  (*h = (ilu_Object) ilu_hash_FindInTable(server_singles(s), t))
	!= NIL)
      return ilu_FALSE;
  l = class_superclass_count(t);
  for (i = 0; i < l; i++)
      if (!_ilu_Addable(s, class_superclass(t, i), h))
          return ilu_FALSE;
  return ilu_TRUE;
}

/*
 * Bug: if the ancestors don't form a tree, some ancestors will be
 * visited more than once, and the later visits will fail in
 * ilu_hash_AddToTable.
 */
void 
_ilu_AddSingleton(ilu_Server s, ilu_Class t, ilu_Object o)
{
  ilu_cardinal i, l;
  if (!class_singleton(t))
    return;
  _ilu_Assert((int) ilu_hash_AddToTable(server_singles(s), t, o),
	      "AddSingleton");
  l = class_superclass_count(t);
  for (i = 0; i < l; i++)
    _ilu_AddSingleton(s, class_superclass(t, i), o);
  ILU_NOTE(OBJECT_DEBUG,
	("_ilu_AddSingleton (server \"%s\", class \"%s\", object %p \"%s\")\n",
	 s->sr_id, t->cl_name, o, (o->ob_ih == NIL) ? "(unset)" : o->ob_ih));
  return;
}

/*L1 >= {the object's server};
  L1 >= {gcmu} if cl collectible*/
ilu_Object ilu_FindOrCreateTrueObject (ilu_string ih, ilu_Server server,
					ilu_Class cl, ilu_refany lspo)
{
  ilu_Object      new;
  if (server == NIL)
    return (NIL);
  _ilu_Assert(ih != NIL, "CreateTrueObject: ih==NIL");
  _ilu_Assert(lspo != NIL, "CreateTrueObject: lspo==NIL");
  _ilu_Assert(cl != NIL, "CreateTrueObject: class==NIL");
  _ilu_HoldMutex(server_lock(server));
  new = (ilu_Object) ilu_hash_FindInTable(server_objs(server), ih);
  if (new != NIL) {
    if (new->ob_class != cl) {
      ILU_NOTE(OBJECT_DEBUG,
	    ("ilu_FindOrCreateTrueObject: type of %s/%s is %s, not the specified %s\n",
	     server->sr_id, ih, new->ob_class->cl_unique_id,
	     cl->cl_unique_id));
      return (NIL);
    }
    return (new);
  }
  if (server->sr_closing == ilu_TRUE) {
    ILU_NOTE(OBJECT_DEBUG,
	  ("ilu_FindOrCreateTrueObject: invoked on closing server %s.\n",
	   server->sr_id));
    return (NIL);
  }
  if (!_ilu_Addable(server, cl, &new)) {
    ILU_NOTE(OBJECT_DEBUG,
	  ("%s %s %s of type %s in server %s because %s %s.\n",
	   "ilu_FindOrCreateTrueObject: can't create another",
	   "singleton object", ih, cl->cl_unique_id, server->sr_id,
	   new->ob_ih, "already exists"));
    return (NIL);
  }
  new = CreateObject(ih, server, cl, lspo, NIL, cl->cl_unique_id);
  ILU_NOTE(OBJECT_DEBUG,
	("ilu_FindOrCreateTrueObject (ih=\"%s\", server=\"%s\", class=\"%s\", lspo=%p) => %p\n",
	 ih, server->sr_id, cl->cl_name, lspo, new));
  _ilu_Assert((int) ilu_hash_AddToTable(server_objs(server),
					 object_ih(new), new),
	      "ilu_FindOrCreateTrueObject: AddToTable failed");
  _ilu_AddSingleton(server, cl, new);
  if (class_collectible(cl))
    _ilu_StartGCingTrueObj(new);
  return new;
}

/*before: Inside(s, static_type)
  after:  result!=NIL => Inside(s, static_type);
  after:  result==NIL => L1 = {};
  forall conn: (L2 >= {conn.iomu}) => (L2 >= {conn.callmu});
  Main otherwise unconstrained*/
ilu_Object
_ilu_FindOrCreateObject(ilu_string ih, ilu_Server s,
			ilu_Class found_class,
			ilu_Class static_type, char *mstid, char *sbh,
			ILU_ERRS((inv_objref)) * err)
{
  ilu_Object      o = NIL;
  ilu_Class       cl = (found_class == NIL) ? static_type : found_class;
  ilu_boolean     tidfound = (found_class != NIL);
#ifdef W3NG_PROTOCOL
  extern ilu_Class _ilu_HTTPng_RemoteObjectBase;
  extern ilu_Class _ilu_w3ng_FindClassViaRPC(ilu_Object);
#endif

  ILU_CLER(*err);

  o = _ilu_FindObjectInServer(ih, s);

  if (o == NIL) {
    ilu_Object      o2;
    if (server_is_true(s)) {
      ilu_ExitServer(s, static_type);
      (void) ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_inst_nf, 6);
    } else if (s->sr_closing == ilu_TRUE) {
      ilu_ExitServer(s, static_type);
      ILU_NOTE(OBJECT_DEBUG,
	    ("%s (ih %s) for closing server %s.\n",
      "_ilu_FindOrCreateObject:  refusing to create new surrogate",
	     ih, s->sr_id));
      (void) ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_svr_closed, 6);
    } else if (!_ilu_Addable(s, cl, &o2)) {
      ilu_ExitServer(s, static_type);
      ILU_NOTE(OBJECT_DEBUG,
	    ("%s %s of type %s in server %s because %s %s.\n",
	"_ilu_FindOrCreateObject:  won't create new singleton", ih,
	 cl->cl_unique_id, s->sr_id, o2->ob_ih, "already exists"));
      (void) ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_bad_single, 6);
    } else {
      o = CreateObject(ih, s, cl, NIL, sbh, mstid);
      ILU_NOTE(OBJECT_DEBUG,
	    ("_ilu_FindOrCreateObject:  "
	     "Created new surrogate object %p, ih <%s>, "
	     "on server <%s> (%p).\n",
	     o, ih, s->sr_id, s));
      if (mstid == NIL || !tidfound) {
	ilu_DeltaServerHolds(s, +1, err);
	ilu_ExitServer(s, static_type);
	if (ILU_ERROK(*err)) {
#ifdef IIOP_PROTOCOL
	  if (strncmp(ih, CORBA_NATIVE_OBJECT_IH_PREFIX, SIZEOF_CORBA_NATIVE_OBJECT_IH_PREFIX) == 0)
	    /*
	     * non-ILU CORBA object; use "-is-a" instead of "ILUGetTypes";
	     * less efficient but works with non-ILU orbs
	     */
	    cl = _ilu_IIOP_FindClassViaRPC(o);
	  else
#endif
#ifdef W3NG_PROTOCOL
	  if (_ilu_IsSubObjectType (cl, _ilu_HTTPng_RemoteObjectBase))
	    /*
	     * use HTTP-ng GetTypeHierarchy()
	     */
	    cl = _ilu_w3ng_FindClassViaRPC(o);
	  else
#endif
	    cl = _ilu_FindClassViaRPC(o);
	  ilu_EnterServer(s, static_type);
	  ilu_DeltaServerHolds(s, -1, err);
	  if (cl != NIL) {
	    object_class(o) = cl;
	  } else if (static_type == ilu_rootClass) {
	    object_class(o) = ilu_rootClass;
	  } else {
#ifdef ENABLE_DEBUGGING
	    ilu_DebugPrintf("ILU Error:  Unable to determine the object type of object <%s/%s>,\n",
			    server_id(object_server(o)), object_ih(o));
	    ilu_DebugPrintf("            which means that this object cannot be instantiated in this address space!\n");
	    ilu_DebugPrintf("            No object types for the type UID \"%s\" are registered.\n",
			    (mstid ? mstid : "NULL"));
	    ilu_DebugPrintf("            This *might* indicate either an ILU version mismatch, or an ISL interface version mismatch.\n");
#endif
	    ILU_NOTE(OBJECT_DEBUG,
		     ("_ilu_FindOrCreateObject:  Couldn't determine"
		      " type for object %p, given mstid \"%s\".\n",
		      o, (mstid ? mstid : "NULL")  ));
	    UncreateObject(o);
	    o = NIL;
	    ilu_ExitServer(s, static_type);
	    (void) ILU_ERR_CONS1(inv_objref, err, minor,
				 ilu_iom_mstid_fail, 6);
	  }
	}
      }
      if (ILU_ERROK(*err)) {
	if (s->sr_closing == ilu_TRUE) {
	  ILU_NOTE(OBJECT_DEBUG,
		("_ilu_FindOrCreateObject:  %s (ih %s) %s %s.\n",
		 "refusing to create new surrogate",
		 ih, "for closing server", s->sr_id));
	  UncreateObject(o);
	  ilu_ExitServer(s, static_type);
	  o = NIL;
	  (void) ILU_ERR_CONS1(inv_objref, err, minor,
			       ilu_iom_svr_closed, 6);
	} else if (NIL != (o2 = (ilu_Object) ilu_hash_FindInTable
			   (server_objs(s), object_ih(o)))) {
	  UncreateObject(o);
	  o = o2;
	} else if (!_ilu_Addable(s, object_class(o), &o2)) {
	  ILU_NOTE(OBJECT_DEBUG,
		("%s %s of type %s in server %s because %s %s.\n",
	    "_ilu_FindOrCreateObject:  won't create new singleton",
	    ih, object_class(o)->cl_unique_id, s->sr_id, o2->ob_ih,
		 "already exists"));
	  UncreateObject(o);
	  ilu_ExitServer(s, static_type);
	  o = NIL;
	  (void) ILU_ERR_CONS1(inv_objref, err, minor,
			       ilu_iom_bad_single, 6);
	} else {
	  _ilu_Assert((int) ilu_hash_AddToTable(server_objs(s),
						 object_ih(o), o),
	       "_ilu_FindOrCreateObject: add to cache (2) failed");
	  _ilu_AddSingleton(s, object_class(o), o);
	}
      }
    }
  }
  if (o != NIL && !ilu_IsSubObjectType(o->ob_class, static_type)) {
    ILU_NOTE(OBJECT_DEBUG,
	  ("%s %s/%s has type %s:%s, not a subtype of %s:%s.\n",
	   "_ilu_FindOrCreateObject: Existing object",
	   s->sr_id, ih,
	   o->ob_class->cl_unique_id, o->ob_class->cl_name,
	   static_type->cl_unique_id, static_type->cl_name));
    ilu_ExitServer(s, static_type);
    if (ILU_ERROK(*err))
      ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_wrong_type,
		    (void) 6);
    o = NIL;
  }
  ILU_NOTE(OBJECT_DEBUG,
	("_ilu_FindOrCreateObject:  Object of <%s/%s> is %p.\n",
	 s->sr_id, ih, o));
  if (o == NIL) {
  } else if (object_mstid(o) == NIL && mstid != NIL)
    o->ob_mstid = _ilu_Strdup(mstid);
  else if (object_mstid(o) == NIL || mstid == NIL) {
  } else if (strcmp(object_mstid(o), mstid) != 0) {
    ILU_NOTE(OBJECT_DEBUG,
	  ("_ilu_FindOrCreateObject: Existing object %s/%s"
	   " has mstid %s, not %s.\n",
	   s->sr_id, ih, object_mstid(o), mstid));
  }
  return (o);
}

/*before: L1 = {};
  after:  result!=NIL => Inside(result's server, cl);
  after:  result==NIL => L1 = {};
  Main Remnant holds; L2 otherwise unconstrained*/
ilu_Object 
ilu_ObjectOfSBH(ilu_string sbh, ilu_Class static_type,
		ILU_ERRS((bad_locks, broken_locks, inv_objref,
			  internal)) * err)
{
  ilu_string      ih = NIL, serverID = NIL, connectInfo = NIL;
  ilu_string      mstid = NIL;
  ilu_cardinal    cinfolen;
  ilu_Object      o = NIL;
  ilu_Class       c2;
  ilu_Server      s;
  ilu_boolean	  pass_cinfo = ilu_FALSE;

  ILU_AUTOSETDEBUGLEVEL;

  if (!ilu_Check(sbh != NIL, err))
    return NIL;
  if (!ilu_Check(static_type != NIL, err))
    return NIL;

  if (!ilu_ParseSBH(sbh, &ih, &serverID, &mstid, &connectInfo,
		    &cinfolen, &pass_cinfo, err)
      || ih == NIL || serverID == NIL
      || connectInfo == NIL) {
    ILU_NOTE(OBJECT_DEBUG,
	  ("ilu_ObjectOfSBH: SBH parse of <%s> unsatisfactory.\n",
	   sbh));
    return NIL;
  }
  if (mstid == NIL)
    {
#ifdef IIOP_PROTOCOL
      if (strncmp(sbh, "IOR:", 4) == 0 ||
	  strncmp(sbh, "iiop:", 5) == 0 ||
	  strncmp(sbh, "IOR2:", 5) == 0 ||
	  strncmp(sbh, "iioploc:", 8) == 0 ||
	  strncmp(sbh, "iiopname:", 9) == 0 ||
	  strncmp(sbh, "ior:", 4) == 0)
	{
	  ILU_NOTE(OBJECT_DEBUG,
		("ilu_ObjectOfSBH:  no type info found in <%s>.  Using static type \"%s\".\n",
		 sbh, class_name(static_type)));
	  c2 = NIL;
	}
      else
#endif /* def IIOP_PROTOCOL */
	{
	  ILU_NOTE(OBJECT_DEBUG,
		("ilu_ObjectOfSBH:  Error, no object type found in SBH <%s>.\n", sbh));
	  return NIL;
	}
    }
  else
    {
      c2 = ilu_FindClassFromID(mstid);
      if (c2 == NIL)
	c2 = _ilu_FindMSKA(mstid);
    }

  if (!ilu_Check(ih != NIL, err))
    return NIL;
  s = _ilu_FindAndEnterServer(serverID, ilu_TRUE, connectInfo, cinfolen,
			      static_type, err);
  if (ILU_ERRNOK(*err))
    return NIL;
  if (!ilu_Check(s != NIL, err))
    return NIL;

  if (s->sr_objs == NIL) {
    ILU_NOTE((OBJECT_DEBUG | SERVER_DEBUG),
	  ("ilu_ObjectOfSBH:  %s (id=%s, true=%d).\n",
	   "asked for object in closed server", serverID,
	   server_is_true(s)));
    ilu_ExitServer(s, static_type);
    ilu_free(ih);
    ilu_free(serverID);
    ilu_free(mstid);
    if (pass_cinfo) ilu_free(connectInfo);
    return ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_svr_closed, NIL);
  }
  ILU_NOTE((OBJECT_DEBUG | SERVER_DEBUG),
	("ilu_ObjectOfSBH:  Server for id %s, ci %*.*s is %p.\n",
	 serverID, cinfolen, cinfolen, connectInfo, s));
  if (pass_cinfo) ilu_free(connectInfo);
  ilu_free(serverID);
  o = _ilu_FindOrCreateObject(ih, s, c2, static_type, mstid, sbh, err);
  if (ILU_ERRNOK(*err)) {
    ILU_NOTE(OBJECT_DEBUG,
	  ("ilu_ObjectOfSBH:  Can't create object <%s> on server <%s>.\n",
	   ih, server_id(s)));
  }
  ilu_free(mstid);
  ilu_free(ih);
  return o;
}

/*before: L1 = {};
  after:  result!=NIL => Inside(result's server, cl);
  after:  result==NIL => L1 = {};
  Main Remnant holds; L2 otherwise unconstrained*/
ilu_Object 
  ilu_FindOrCreateSurrogate (ilu_Server server,
			     ilu_string ih,
			     ilu_Class type,
			     ILU_ERRS((bad_locks, broken_locks, inv_objref,
				       internal)) * err)
{
  ilu_Object o = NIL;

  ILU_AUTOSETDEBUGLEVEL;

  if (!ilu_Check(ih != NIL, err))
    return NIL;
  if (!ilu_Check(type != NIL, err))
    return NIL;
  if (!ilu_Check(server != NIL, err))
    return NIL;

  ilu_EnterServer(server, type);

  if (server->sr_objs == NIL) {
    ILU_NOTE(OBJECT_DEBUG,
	  ("ilu_FindOrCreateSurrogate:  %s (id=%s, true=%d).\n",
	   "asked for object in closed server", server->sr_id,
	   server_is_true(server)));
    ilu_ExitServer(server, type);
    return ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_svr_closed, NIL);
  }

  o = _ilu_FindOrCreateObject(ih, server, NIL, type, NIL, NIL, err);

  ILU_NOTE(OBJECT_DEBUG,
	("ilu_FindOrCreateSurrogate (server=\"%s\", ih=\"%s\", type=\"%s\") => %p\n",
	 server->sr_id, ih, type->cl_name, o));
  if (ILU_ERRNOK(*err)) {
    ILU_NOTE(OBJECT_DEBUG,
	  ("ilu_FindOrCreateSurrogate:  Error <%s>\n", ILU_ERR_NAME(*err)));
  }
  return o;
}

#define ADDTO(s,c)	(s)[strlen(s)+1]=0;(s)[strlen(s)]=(c)

/* L1, L2 unconstrained */
static ilu_string
GFormSBH (ilu_string server_id,	/* RETAIN */
	  ilu_string instance_handle,/* RETAIN */
	  ilu_string mstid,		/* RETAIN */
	  ilu_string cinfo,		/* RETAIN */
	  ilu_cardinal cinfolen,
	  ilu_Error * err)
{
  ilu_string      quoted_server_id;
  ilu_string      quoted_instance_handle;
  ilu_string      quoted_mstid;
  ilu_cardinal    i, fulli;
  ilu_string      ans;

  quoted_mstid = _ilu_EncodeBuffer(mstid, strlen(mstid), err);
  if (ILU_ERRNOK(*err))
    return NIL;
  quoted_server_id = _ilu_EncodeBuffer(server_id, strlen(server_id), err);
  if (ILU_ERRNOK(*err)) {
    ilu_free(quoted_mstid);
    return NIL;
  };
  quoted_instance_handle = _ilu_EncodeBuffer(instance_handle,
				     strlen(instance_handle), err);
  if (ILU_ERRNOK(*err)) {
    ilu_free(quoted_mstid);
    ilu_free(quoted_server_id);
    return NIL;
  };
  i = (strlen(quoted_server_id)
       + strlen(quoted_instance_handle)
       + strlen(quoted_mstid)
       + cinfolen
       + 13);
  fulli = i + 4 - (i % 4);
  ans = ilu_MallocE(fulli, err);
  if (ILU_ERRNOK(*err)) {
    ilu_free(quoted_mstid);
    ilu_free(quoted_server_id);
    ilu_free(quoted_instance_handle);
    return NIL;
  };

#ifdef ILU_TYPEUID_V2
  strcpy(ans, "ilusbh:");
#else
  strcpy(ans, "ilu:");
#endif
  strcat(ans, quoted_server_id);
  ADDTO(ans, '/');
  strcat(ans, quoted_instance_handle);
  ADDTO(ans, ILU_TYPE_MARKER);
  strcat(ans, quoted_mstid);
  ADDTO(ans, ILU_CINFO_MARKER);
  ans[strlen(ans) + cinfolen] = 0;
  strncpy(ans + strlen(ans), cinfo, cinfolen);
  ilu_free(quoted_mstid);
  ilu_free(quoted_server_id);
  ilu_free(quoted_instance_handle);
  return ans;
}

/* L1, L2 unconstrained */
ilu_string
ilu_FormSBH(ilu_string server_id,	/* RETAIN */
	    ilu_string instance_handle,	/* RETAIN */
	    ilu_string mstid,		/* RETAIN */
	    ilu_ProtocolInfo pinfo,	/* RETAIN */
	    ilu_TransportInfo tinfo,	/* RETAIN */
	    ilu_Error * err)
{
  static char     cinfo_divider_s[2] = {ILU_CINFO_DIVIDER, 0};
  ilu_string      quoted_pinfo, quoted_tinfo, cinfo, ans = NIL;
  if (!(quoted_tinfo = _ilu_StringifyTinfo(tinfo, err)))
    return NIL;
  if (!(quoted_pinfo = _ilu_EncodeBuffer(pinfo, strlen(pinfo), err)))
    goto fale1;
  if (!(cinfo = ilu_Strcat3E(quoted_pinfo, cinfo_divider_s, quoted_tinfo,
			     err)))
    goto fale2;
  ans = GFormSBH(server_id, instance_handle, mstid, cinfo, strlen(cinfo), err);
  ilu_free(cinfo);
fale2:
  ilu_free(quoted_pinfo);
fale1:
  ilu_free(quoted_tinfo);
  return ans;
}

/*L1 >= {obj's server}; L1_sup < prmu*/
static ilu_boolean
  FormSingletonCinfo (ilu_Server s,
		      ilu_string sinfo,
		      ilu_string *cinfo,
		      ilu_cardinal *cinfolen,
		      ILU_ERRS((no_memory)) * err)
{
  ilu_Port p;
  ILU_CLER(*err);
  for (p = s->sr_ports.pl_next;  p != NIL;  p = p->po_links.pl_next) {
#ifdef ILU_RELAXED_SINGLETON_CHECKING
    char *i = strchr(p->po_pinfo, '_');
    if (strncmp(sinfo, p->po_pinfo, (i == NIL) ? strlen(p->po_pinfo) : (i - p->po_pinfo)) == 0) {
#else
    if (strcmp(sinfo, p->po_pinfo) == 0) {
#endif
      ilu_CharBuf buf;
      buf.icb_base = NIL;
      buf.icb_len = 0;
      buf.icb_size = 0;
      if (ilu_Append1Cinfo(&buf, p->po_pinfo, p->po_tinfo, err))
	{
	  *cinfo = buf.icb_base;
	  *cinfolen = buf.icb_len;
	  return ilu_TRUE;
	}
      else
	return ilu_FALSE;
    }
  }
  return ilu_FALSE;
}

/*L1 >= {obj's server}; L1_sup < prmu*/
ilu_string ilu_SBHOfObject (ilu_Object obj)
{
  ilu_Server      s;

  if (obj == NIL)
    return (NIL);

  s = object_server(obj);
  _ilu_HoldMutex(server_lock(s));
  if (object_sbh(obj) != NIL)
    /* do nothing */;
  else {
    ilu_string      ans;
    ilu_Error       lerr;
    ilu_string	    cinfo = NIL;
    ilu_cardinal    cinfolen;
    ILU_CLER(lerr);
    if (object_is_singleton(obj) && server_is_true(s)) {
      if (!FormSingletonCinfo(s, object_singleton_info(obj), &cinfo, &cinfolen, &lerr)) {
	if (ILU_ERROK(lerr)) {
	  ILU_NOTE((OBJECT_DEBUG | EXPORT_DEBUG),
		   ("ilu_SBHOfObject:  Can't form SBH for %s/%s; requires ilu_Port with protocol of \"%s\".\n",
		    s->sr_id, obj->ob_ih, object_singleton_info(obj)));
	  return NIL;
	} else {
	  ILU_NOTE((OBJECT_DEBUG | EXPORT_DEBUG),
		   ("ilu_SBHOfObject:  Error <%s> forming cinfo for singleton %s/%s.\n",
		    ILU_ERR_NAME(lerr), s->sr_id, obj->ob_ih));
	  ILU_HANDLED(lerr);
	  return NIL;
	}
      }
    } else {
      cinfo = s->sr_cinfo.icb_base;
      cinfolen = s->sr_cinfo.icb_len;
    }
    ans = GFormSBH(s->sr_id, obj->ob_ih, ilu_MstidOfObject(obj), cinfo, cinfolen, &lerr);
    if (ILU_ERRNOK(lerr)) {
      ILU_NOTE((OBJECT_DEBUG | EXPORT_DEBUG),
	       ("ilu_SBHOfObject:  can't form SBH for %s/%s, err \"%s\".\n",
		s->sr_id, obj->ob_ih, ILU_ERR_DESCRIPTION(lerr)));
      ILU_HANDLED(lerr);
      ans = NIL;
    } else {
      object_sbh(obj) = ans;
    }
    if (cinfo != s->sr_cinfo.icb_base)
      ilu_free(cinfo);
  }
  return (object_sbh(obj));
}

/*L1, L2, Main unconstrained*/

ilu_string ilu_MstidOfObject (ilu_Object obj)
{
  if (obj == NIL)
    return NIL;
  if (object_mstid(obj) == NIL)
    obj->ob_mstid = _ilu_Strdup(class_unique_id(object_class(obj)));
  return (object_mstid(obj));
}

ilu_Class ilu_ClassOfObject (ilu_Object obj)
{
  if (obj == NIL)
    return (NIL);
  else
    return (object_class(obj));
}

ilu_boolean ilu_TrueInstanceP (ilu_Object obj)
{
  ilu_boolean s = ilu_FALSE;

  if (obj != NIL)
    {
      s = object_is_true(obj);
    }
  return (s);
}

ilu_boolean 
ilu_InstanceTrueForLangP(ilu_Object obj,
			 ilu_LanguageIndex li)
{
  ilu_boolean     ans = ilu_FALSE;
  ilu_Server      s;

  if (obj != NIL && (s = obj->ob_server)) {
    ans = server_is_true(s) && server_true_language(s) == li;
  }
  return (ans);
}

ilu_Server ilu_ServerOfObject (ilu_Object obj)
{
  if (obj == NIL)
    return (NIL);
  else
    return (object_server(obj));
}

ilu_string ilu_IhOfObject (ilu_Object obj)
{
  if (obj == NIL)
    return (NIL);
  else
    return (object_ih(obj));
}

/*L1 >= {cmu, s}; L1_sup < trmu*/
static
ILU_ERRS((internal, bad_locks, broken_locks))
DestroyObject(ilu_Server s, ilu_Object obj)
{
  ilu_Class       c = object_class(obj);
  ILU_ERRS((internal, bad_locks, broken_locks)) err;
  ASSERT(c != NIL, buf,
	 (buf, "object.c:DestroyObject: class(%s/%s) == NIL",
	  obj->ob_server->sr_id, obj->ob_ih));
  ASSERT(obj->ob_ih != NIL, buf,
	 (buf, "object.c:DestroyObject: ih(%s/%s) == NIL",
	  obj->ob_server->sr_id, obj->ob_ih));
  _ilu_HoldMutex(ilu_cmu);
  _ilu_HoldMutex(server_lock(s));
  ILU_NOTE(OBJECT_DEBUG,
	   ("DestroyObject (server=%p \"%s\", obj=%p \"%s\");\n"
	    "\tserver's ports=%s %s %s, conns=%s %s,"
	    " objs:=%d-1, LSSes=%d.\n",
	    s, s->sr_id, obj, obj->ob_ih,
	    (server_ports(s) ? "X" : "0"),
	    (s->sr_local_port ? "X" : "0"),
	    (s->sr_closedPorts.pl_next ? "X" : "0"),
	    (s->sr_connHead.next ? "X" : "0"),
	    (s->sr_closedConns.next ? "X" : "0"),
	    (s->sr_objs ? ilu_hash_PairsInTable(s->sr_objs) : 0),
	    _ilu_ServerLSSCount(s)
	    ));
  if (object_collectible(obj) && server_is_true(s))
    _ilu_StopGCingTrueObj(obj);
  err = _ilu_ServerRemoveObject(s, obj);
  ILU_ERR_SWITCH(err) {
    ILU_SUCCESS_CASE;
    ILU_ERR_CASE3(internal, bad_locks, broken_locks)
      return err;
  } ILU_ERR_ENDSWITCH;
  FREETOKEN(object_ih(obj));
  if (object_sbh(obj) != NIL)
    FREETOKEN(object_sbh(obj));
  ilu_free(object_mstid(obj));
  ilu_free(obj);
  return ILU_NO_ERR;
}

/*L1 >= {s}*/
ilu_refany ilu_GetLanguageSpecificObject (ilu_Object obj, ilu_cardinal lang)
{
  ilu_refany s = NIL;
  _ilu_Assert(lang<=_ilu_NLanguages, "ilu_GetLanguageSpecificObject: unknown language specified");
  if (obj != NIL)
    {
      _ilu_HoldMutex(server_lock(object_server(obj)));
      s = object_lspo(obj, lang);
    }
  return (s);
}

/* Main Remnant */

/*Inside(obj's server, st)*/
static
ILU_ERRS((GcRegFailed, bad_locks, broken_locks, internal))
IUpdate(ilu_Object obj, ilu_Class st)
{
  ilu_Server      s = object_server(obj);
  ilu_Error       ans = ILU_INIT_NO_ERR;
  unsigned int    i;
  ilu_boolean     should = ilu_FALSE, did;

  if (!object_is_true(obj) && object_notifying(obj)) {
    ILU_NOTE(GC_DEBUG | OBJECT_DEBUG,
	  ("IUpdate:  called while notifying object %s/%s (%p).\n",
	   s->sr_id, obj->ob_ih, obj));
    goto dun;
  }
  if (object_collectible(obj) && !object_is_true(obj)) {
    should = (object_holds(obj) != 0);
    for (i = 0; (!should) && (i < _ilu_NLanguages); i++)
      should = (obj->ob_lspos[i] != NIL);
    object_notifying(obj) = ilu_TRUE;
    while (should != object_known(obj) && ILU_ERROK(ans)) {
      ILU_NOTE(GC_DEBUG | OBJECT_DEBUG,
	    ("IUpdate:  notifying object %s/%s (%p), should=%d.\n",
	     s->sr_id, obj->ob_ih, obj, should));
      ilu_ExitServer(s, st);
      if (should)
	did = _ilu_RegisterGCInterest(obj, &ans);
      else
	did = _ilu_UnregisterGCInterest(obj, &ans);
      ilu_EnterServer(s, st);
      ILU_NOTE(GC_DEBUG | OBJECT_DEBUG,
	       ("IUpdate:  done notifying object %s/%s (%p),"
		" result=%s.\n",
		s->sr_id, obj->ob_ih, obj, ILU_ERR_NAME(ans)));
      if (did)
	object_known(obj) = should;
      should = (object_holds(obj) != 0);
      for (i = 0; (!should) && (i < _ilu_NLanguages); i++)
	should = (obj->ob_lspos[i] != NIL);
    }
    object_notifying(obj) = ilu_FALSE;
  }
  if (ILU_ERRNOK(ans))
    goto dun;
  if (object_holds(obj) != 0)
    goto dun;
  for (i = 0; i < _ilu_NLanguages; i++) {
    if (obj->ob_lspos[i] != NIL)
      goto dun;
  }
  if (object_is_true(obj) && object_collectible(obj)) {
    ilu_integer     now, tot;
    if (object_gclist(obj) != NIL &&
	_ilu_vector_size(object_gclist(obj)) != 0) {
      ILU_NOTE(GC_DEBUG | OBJECT_DEBUG,
	       ("IUpdate:  Collectibe object %s/%s (%p)"
		" preserved by remote references.\n",
		s->sr_id, obj->ob_ih, obj));
      goto dun;
    }
    now = ilu_CoarseTime_Now();
    tot = object_lastRemote(obj) + obj->ob_timeout;
    ILU_NOTE(GC_DEBUG | OBJECT_DEBUG,
	     ("IUpdate:  For collectible object %s/%s (%p),"
	      " now=%ld, timeout=%ld.\n",
	      s->sr_id, obj->ob_ih, obj, now, tot));
    if (now < tot)
      goto dun;
  }
  ILU_NOTE(OBJECT_DEBUG,
     ("IUpdate:  deleting uninteresting object %s/%s (%p).\n",
      s->sr_id, obj->ob_ih, obj));
  ans = DestroyObject(object_server(obj), obj);
dun:
  return ans;
}


/*Inside(obj's server, obj's type)*/
ILU_ERRS((GcRegFailed, bad_locks, broken_locks, internal))
_ilu_VIUpdate(ilu_Object obj)
{
  unsigned int             vi;
  unsigned int    i;

  vi = ((object_holds(obj) != 0)
	|| (object_collectible(obj) &&
	    (object_is_true(obj)
	     ? ((object_gclist(obj) != NIL &&
		 _ilu_vector_size(object_gclist(obj)) != 0)
		|| (ilu_CoarseTime_Now() <
		    object_lastRemote(obj) + obj->ob_timeout))
	     : (object_notifying(obj) && !object_known(obj)))));
  if (vi != object_intNoted(obj)) {
    object_intNoted(obj) = vi;
    for (i = 0; i < _ilu_NLanguages; i++) {
      if (theNoters[i] != NULLFN) {
	if (!(*theNoters[i]) (obj, vi)) {
	  obj->ob_lspos[i] = NIL;
	  ILU_NOTE(OBJECT_DEBUG,
		("ilu_ObjectNoter(obj=%p \"%s\" in \"%s\", lang=%s) clears LSO.\n",
		 obj, obj->ob_ih, obj->ob_server->sr_id,
		 _ilu_LangNames[i]));
	}
      }
    }
  }
  return IUpdate(obj, obj->ob_class);
}

/*Inside(obj's server, t)*/
static          ilu_boolean
SetLSO(ilu_Object obj, ilu_Class t, ilu_refany lso,
       ilu_cardinal lang,
       ILU_ERRS((GcRegFailed, bad_locks, broken_locks,
		 internal)) * err)
{
  ilu_Server      s = obj->ob_server;

  _ilu_Assert(lang <= _ilu_NLanguages,
	      "ilu_SetLSO: unknown language passed");
  obj->ob_lspos[lang] = lso;
  ILU_NOTE(OBJECT_DEBUG,
   ("ILU(SetLSO(obj=%p \"%s\" in %p \"%s\", lang=%s, lso:=%p)).\n",
    obj, obj->ob_ih, s, s->sr_id, _ilu_LangNames[lang], lso));
  *err = IUpdate(obj, t);
  return ILU_ERROK(*err);
}

ilu_boolean
ilu_RegisterLSO(ilu_Object obj,
		ilu_Class t,
		ilu_refany lso,
		ilu_cardinal lang,
		ILU_ERRS((GcRegFailed, bad_locks, broken_locks,
			  internal)) * err)
{
  return SetLSO(obj, t, lso, lang, err);
}

void
ilu_SetLSO(ilu_Object obj,
	   ilu_Class t,
	   ilu_refany lso,
	   ilu_cardinal lang)
{
  ILU_ERRS((GcRegFailed, bad_locks, broken_locks, internal)) lerr;
  if (!SetLSO(obj, t, lso, lang, &lerr)) {
    ILU_ERR_SWITCH(lerr) {
      ILU_SUCCESS_CASE;
      ILU_ERR_CASE(GcRegFailed, e)
	_ilu_Assert(lso == NIL,
		    "Couldn't register GC interest in collectible instance (in ilu_SetLSO)");
    } ILU_ERR_ENDSWITCH;
    ILU_HANDLED(lerr);
  }
}

void ilu_RegisterLanguageSpecificObject (ilu_Object obj, ilu_refany lso, ilu_cardinal lang)
{
  ilu_SetLSO(obj, obj->ob_class, lso, lang);
  return;
}

/*L2 unconstrained*/
void 
ilu_DHolds(ilu_Object obj, ilu_integer dholds)
{
  ilu_integer new_holds = object_holds(obj) + dholds;
  _ilu_Assert(new_holds>=0,"holds on object goes negative");
  object_holds(obj) = new_holds;
  return;
}

ILU_ERRS((GcRegFailed, bad_locks, broken_locks, internal))
ilu_DeltaHolds(ilu_Object obj, ilu_integer dholds)
{
  return _ilu_DeltaHolds(obj, dholds);
}

ILU_ERRS((GcRegFailed, bad_locks, broken_locks, internal))
_ilu_DeltaHolds(ilu_Object obj, ilu_integer dholds)
{
  ilu_integer new_holds = object_holds(obj) + dholds;
  _ilu_Assert(new_holds>=0,"holds on object goes negative");
  object_holds(obj) = new_holds;
  return _ilu_VIUpdate(obj);
}

/* a bit of scaffolding code for debugging the class record */
/*L1, L2, Main unconstrained; for calling from debugger*/
#ifdef NOPE
static void dump_class_record(ilu_Class class)
{
	ilu_DebugPrintf ("dumping class record:\n");
	ilu_DebugPrintf ("class name: %s\n",class->cl_name);
	ilu_DebugPrintf ("class brand: %s\n",class->cl_brand ? class->cl_brand : "");
	ilu_DebugPrintf ("class id: %s\n",class->cl_unique_id ? class->cl_unique_id : "");
	ilu_DebugPrintf ("class singleton == \"%s\"",
	       class->cl_singleton ? class->cl_singleton : "(not a singleton)");
	ilu_DebugPrintf ("class collectible == 0x%x\n",class->cl_collectible);
	ilu_DebugPrintf ("class authentication: %s\n",
	       class->cl_authentication ? class->cl_authentication : "");
	ilu_DebugPrintf ("class methods table: 0x%x\n",class->cl_methods);
	ilu_DebugPrintf ("class method count: %d\n",class->cl_method_count);
	ilu_DebugPrintf ("class superclass record: 0x%x\n",class->superclass);
	ilu_DebugPrintf ("class superclass name: 0x%x (%s)\n",class->superclassname,
	       class->superclassname ? class->superclassname : "");
}
#endif

/*Inside(object_server(obj), object_class(obj))*/
ilu_integer
  ilu_SetObjectGCTimeout (ilu_Object obj, ilu_integer seconds, ilu_Error *err)
{
  ilu_integer old_seconds = 0;

  if (!server_is_true(object_server(obj)))
    ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_surrogate, 0);
  else if (!class_collectible(object_class(obj)))
    ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_not_collectible, 0);
  else {
    old_seconds = obj->ob_timeout;
    obj->ob_timeout = seconds;
    ILU_CLER(*err);
  }
  return old_seconds;
}

/*Main Invariant holds; L2 otherwise unconstrained*/
ilu_boolean ilu_PingObject(ilu_Object o, ilu_Connection * new_conn)
{
  ilu_Call_s      call_s;
  ilu_Call        call = &call_s;
  ilu_cardinal    estatus = 0;
  ilu_ProtocolException internal;
  ilu_cardinal    reqSize;
  ilu_Class       pclass = object_class(o);
  ilu_Server      s = object_server(o);
  ilu_boolean     status = ilu_FALSE;
  ILU_ERRS((IoErrs, bad_locks, inv_objref, no_resources)) lerr;
  ilu_Connection  newconn = NIL;
  
  *new_conn = NIL;

  if (class_singleton(pclass))
    return (ilu_FALSE);

  ILU_NOTE(OBJECT_DEBUG,
	   ("ilu_PingObject:  object <%s>/<%s>...\n",
	    s->sr_id, o->ob_ih));

  if (server_is_true(s)) {
    ilu_refany      lspo;

    _ilu_AcquireServerMutex(s);
    lspo = object_lspo(o, server_true_language(s));
    _ilu_ReleaseServerMutex(s);
    return (lspo != NIL);
  } else {
    if (!ilu_StartCall(call, s, _ilu_rootClass, _ilu_PingMethod, 0,
		       NIL, &newconn, &lerr)) {
      ILU_NOTE(OBJECT_DEBUG,
	       ("ilu_PingObject:  ilu_StartCall failed.\n"));
      ILU_HANDLED(lerr);
      return (ilu_FALSE);
    }
  retry:
    if (newconn != NIL) {
      if (*new_conn != NIL) {
	(void) _ilu_HandOffNewConnection(*new_conn, &lerr);
	if (ILU_ERRNOK(lerr))
	  return ilu_FALSE;
      };
      *new_conn = newconn;
      newconn = NIL;
    };
    _ilu_AcquireServerMutex(s);
    reqSize = ilu_SizeOfObjectID(call, o, ilu_TRUE, _ilu_rootClass, &lerr);
    _ilu_ReleaseServerMutex(s);
    if (ILU_ERRNOK(lerr))
      goto dun;
    if (!ilu_StartRequest(call, reqSize, &lerr))
      goto dun;
    ilu_EnterServer(s, pclass);
    ilu_OutputObjectID(call, o, ilu_TRUE, _ilu_rootClass, &lerr);
    if (ILU_ERRNOK(lerr))
      goto dun;
    if (!ilu_FinishRequest(call, &lerr))
      goto dun;
    internal = ilu_GetReply(call, &estatus, &newconn, &lerr);
    if (ILU_ERRNOK(lerr) &&
	(lerr.ilu_type == ILU_ERRTYP(transient)) &&
	(ILU_ERRSEL(transient,lerr).minor == ilu_tm_retry)) {
      ILU_HANDLED(lerr);
      ILU_CLER(lerr);
      goto retry;
    };
    _ilu_Assert((!ILU_ERROK(lerr) ==
		 (internal == ilu_ProtocolException_Not)),
		"GetReply botched error raise");
    if (internal != ilu_ProtocolException_Success)
      goto dun;
    ilu_ReplyRead(call, &lerr);
    if (ILU_ERRNOK(lerr))
      goto dun;
    status = (estatus == 0);
    ILU_NOTE(OBJECT_DEBUG,
	     ("ilu_PingObject:  returns %u and %lu => %s.\n",
	internal, estatus, status ? "Good object" : "Bad object"));
dun:
    ilu_FinishCall(call, &lerr);
    if (ILU_ERRNOK(lerr)) {
      ILU_HANDLED(lerr);
      return ilu_FALSE;
    }
    return status;
  }
}

/**Call-Locking(call, IHi) before,
   Call-Locking(call,  No) after*/
void
_ilu_HandlePing(ilu_Call call)
{
  ilu_Object      disc;
  ilu_Error       lerr = ILU_INIT_NO_ERR;
  ilu_cardinal asize;

  /* XXX This is all bogus: upon err we're supposed to jump to FinishCall */
  
  ilu_InputObjectID(call, &disc, ilu_TRUE, _ilu_rootClass, &lerr);
  if (ILU_ERRNOK(lerr)) {
    ILU_HANDLED(lerr);
    if (!ilu_RequestRead(call, &lerr))
      ILU_HANDLED(lerr);
    goto garbin;
  }
  if (disc != NIL) {
    lerr = ilu_DeltaHolds(disc, 1);
    ILU_MUST_BE_SUCCESS(lerr);
    ilu_ExitServer(object_server(disc), _ilu_rootClass);
  }
  if (!ilu_RequestRead(call, &lerr))
    goto dun;

  if ((disc != NIL) && ilu_TrueInstanceP(disc)) {
    ilu_cardinal rsize = ilu_BeginSizingReply (call, ilu_FALSE, &lerr);
    if (ILU_ERRNOK(lerr))
      goto dun;
    if (!ilu_BeginReply(call, ilu_FALSE, rsize, &lerr))
      goto dun;
    if (!ilu_FinishReply(call, &lerr))
      goto dun;
    goto dun;
  }
garbin:
  asize = ilu_BeginSizingException (call,
				    - (ilu_integer) ilu_ProtocolException_GarbageArguments,
				    &lerr);
  if (ILU_ERRNOK(lerr))
    goto dun;
  if (!ilu_BeginException(call,
			  - (ilu_integer) ilu_ProtocolException_GarbageArguments,
			  asize, &lerr))
    goto dun;
  if (!ilu_FinishException(call, &lerr))
    goto dun;
dun:
  ILU_HANDLED(lerr);
  if (disc != NIL) {
    ilu_Server      s = object_server(disc);
    ilu_Class       cl = object_class(disc);
    ilu_EnterServer(s, cl);
    lerr = ilu_DeltaHolds(disc, -1);
    ILU_MUST_BE_SUCCESS(lerr);
    ilu_ExitServer(s, cl);
  }
  return;
}
