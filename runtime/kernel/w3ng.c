/** $Id: w3ng.c,v 1.89 1999/08/12 17:57:46 janssen Exp $
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
/* Last edited by Mike Spreitzer October 16, 1998 10:50 pm PDT */

#include "iluntrnl.h"
#include "w3ng.h"

#include "call.h"
#include "iluprotocol.h"
#include "connect.h"
#include "ilutransport.h"
#include "port.h"
#include "object.h"
#include "type.h"
#include "method.h"
#include "mooring.h"
#include "server.h"

/**********************************************************************/
/**********************************************************************/
/*								      */
/*			Macro definitions			      */
/*								      */
/**********************************************************************/
/**********************************************************************/

#define ODD(x)		(((x)&0x1)!=0)
#define EVEN(x)		(((x)&0x1)==0)  
#define PADDED_SIZE(x)	((((unsigned) (x))+3) & (~0x3))
  
#define OBJECT_CACHE_BUCKETS	123
#define OPERATION_CACHE_BUCKETS	123
#define MAX_OP_CACHE_INDEX	0x3FFF

#define DEFAULT_CINFO		"w3ng@sunrpcrm"

#ifdef WORDS_BIGENDIAN
#define THIS_ARCH_IS_BIGENDIAN	ilu_TRUE
#else
#define THIS_ARCH_IS_BIGENDIAN	ilu_FALSE
#endif

#define ILU_CORBA_NIL_OBJ_URI	"ilucorbanilobject:"

/**********************************************************************/
/**********************************************************************/
/*								      */
/*			Macro definitions			      */
/*								      */
/**********************************************************************/
/**********************************************************************/

/*L1, L2 unconstrained*/

#define w3ng_transport(call)		(call)->ca_prTrans

/*
 * w3ng_transport(call)'s xmu == call's conn's iomu;
 * w3ng_transport(call)'s ymu == (call->ca_prbit1 ? conn's iomu : conn's waitmu).
 */
#define w3ng_transport_ymu(call)	(call)->ca_prbit1
#define CONN_WAITMU			ilu_FALSE
#define CONN_IOMU			ilu_TRUE

static ilu_cardinal
  _w3ng_HashObject (CachedObject *ref, ilu_cardinal chop)
{
  ilu_cardinal crc = ilu_CRC32 ((ilu_bytes) ref->serverID, (ilu_cardinal) strlen(ref->serverID));
  crc = ilu_CRC32WithAccum ((ilu_bytes) ref->instance_handle, (ilu_cardinal) strlen(ref->instance_handle), crc);
  return (crc % chop);
}

static ilu_cardinal
  _w3ng_HashOperation (CachedOperation *ref, ilu_cardinal chop)
{
  ilu_cardinal crc = ilu_CRC32 ((ilu_bytes) class_unique_id(ref->cached_class),
				(ilu_cardinal) strlen(class_unique_id(ref->cached_class)));
  crc = ilu_CRC32WithAccum ((ilu_bytes) method_name(ref->cached_method),
			    (ilu_cardinal) strlen(method_name(ref->cached_method)),
			    crc);
  return (crc % chop);
}

static ilu_cardinal
  _w3ng_HashIndex (ilu_cardinal ref, ilu_cardinal chop)
{
  return (ref % chop);
}

static ilu_boolean
  _w3ng_CompareObject (CachedObject *ref1, CachedObject *ref2)
{
  return (((ref1->serverID == ref2->serverID) &&
	   (ref1->instance_handle == ref2->instance_handle)) ||
	  ((strcmp(ref1->serverID, ref2->serverID) == 0) &&
	   (strcmp(ref1->instance_handle, ref2->instance_handle) == 0)));
}

static ilu_boolean
  _w3ng_CompareOperation (CachedOperation *ref1, CachedOperation *ref2)
{
  return ((strcmp(class_unique_id(ref1->cached_class), class_unique_id(ref2->cached_class)) == 0) &&
	  (method_id(ref1->cached_method) == method_id(ref2->cached_method)));
}

static ilu_boolean
  _w3ng_CompareIndex (ilu_cardinal index1, ilu_cardinal index2)
{
  return (index1 == index2);
}

static void
  _w3ng_FreeObjectData (ilu_refany data)
{
  CachedObject *d = (CachedObject *) data;
  ilu_free(d->serverID);
  ilu_free(d->instance_handle);
  ilu_free(d);
}

static void
  _w3ng_FreeOperationData (ilu_refany data)
{
  ilu_free(data);
}

/*======================================================================
**======================================================================
**====================  URL handling ===================================
**======================================================================
**====================================================================*/

static ilu_string
  StrndupE (ilu_string s, ilu_cardinal length,
	    ilu_Error *err)
{
  ilu_string dup = ilu_MallocE(length + 1, err);
  if (ILU_ERRNOK(*err)) return NIL;
  memcpy (dup, s, length);
  dup[length] = 0;
  return dup;
}

ilu_boolean
  _ilu_w3ng_ParseURL (ilu_string in_url, ilu_string *ih, ilu_string *sid,
		      ilu_string *mstid, ilu_string *cinfo, ilu_cardinal *cinfolen,
		      ilu_boolean *pass_cinfo, ilu_Error *err)
{
  /* Parse W3NG style URL
   *
   * w3ng:<server-id>/<object-ih>[;type=<type>][;cinfo=<cinfo>]
   */
  
  unsigned long vals;
  char serverID[1024], instance_handle[1024], extra_stuff[2048];
  ilu_cardinal junk;

  vals = sscanf (in_url, "w3ng:%1023[^/]/%1023[^;]%2047s",
		 serverID, instance_handle, extra_stuff);
  if ((vals < 2) || ((vals == 3) && (extra_stuff[0] != ';')))
    return ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_ior, ilu_FALSE);
  if ((sid != NIL) &&
      (*sid = _ilu_DecodeBuffer (serverID, strlen(serverID),
				 &junk, err)) == NIL)
    return ilu_FALSE;
  if ((ih != NIL) &&
      (*ih = _ilu_DecodeBuffer (instance_handle, strlen(instance_handle),
				&junk, err)) == NIL)
    return ilu_FALSE;
  if (vals > 2) {
    ilu_string p, q;
    if ((cinfo != NIL) && ((p = strstr(extra_stuff, ";cinfo=")) != NIL)) {
      q = strchr(p + 7, ';');
      if (q == NIL)
	*cinfo = StrndupE(p + 7, strlen(p + 7), err);
      else
	*cinfo = StrndupE(p + 7, q - (p + 7), err);
      if (ILU_ERRNOK(*err)) return ilu_FALSE;
      if (cinfolen != NIL)
	*cinfolen = strlen(*cinfo);
    };
    if ((mstid != NIL) && ((p = strstr(extra_stuff, ";type=")) != NIL)) {
      q = strchr(p + 6, ';');
      if (q == NIL)
	*mstid = _ilu_DecodeBuffer(p + 6, strlen(p + 6), &junk, err);
      else
	*mstid = _ilu_DecodeBuffer(p + 6, q - (p + 6), &junk, err);
      if (ILU_ERRNOK(*err)) return ilu_FALSE;
    };
  };
  if ((mstid != NIL) && (*mstid == NIL)) {
    *mstid = ilu_StrdupE(class_unique_id(ilu_rootClass), err);
    if (ILU_ERRNOK(*err)) return ilu_FALSE;
  };
  if (cinfo != NIL) {
    if (*cinfo == NIL)
      return ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_ci, ilu_FALSE);
    else if (pass_cinfo != NIL)
      *pass_cinfo = ilu_TRUE;
  };
  return ILU_CLER(*err);
}

/* Inside(object_server(obj), object_class(obj)) */
static ilu_Port
  FindNGPort (ilu_Server s)
{
  ilu_Port p;
  /* given a true server, grovel through the ports to find one that
     talks w3ng... */
  for (p = s->sr_ports.pl_next;  p != NIL;  p = p->po_links.pl_next) {
    if (strncmp(p->po_pinfo, "w3ng", 4) == 0)
      return p;
  };
  return NIL;  
}

/* Inside(object_server(obj), object_class(obj)) */
static ilu_CharBuf
  FigureNGCinfo (ilu_Object obj, ilu_Error *err)
{
  ilu_Port p;
  ilu_ProtocolInfo pinfo;
  ilu_TransportInfo tinfo;
  ilu_Server s = object_server(obj);
  ilu_string temp;
  ilu_CharBuf cinfo = { 0 };
  int i;
  char separator1[2] = { ILU_CINFO_DIVIDER, 0 };
  char separator2[2] = { ILU_TINFO_DIVIDER, 0 };

  /* figure out hostname and port... */
  if (server_is_true(s)) {
    if ((p = FindNGPort(s)) == NIL)
      return ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_ps, cinfo);
    pinfo = port_pinfo(p);
    tinfo = port_tinfo(p);
  } else {
    pinfo = s->sr_pinfo;
    tinfo = s->sr_tinfo;
  }
  if (strncmp((ilu_string) pinfo, "w3ng", 4) != 0)
    return ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_ps, cinfo);
  temp = _ilu_EncodeBuffer(pinfo, strlen(pinfo), err);
  if (ILU_ERRNOK(*err)) return cinfo;
  cinfo = ilu_CharBufFromString(temp, err);
  ilu_free(temp);
  if (ILU_ERRNOK(*err)) return cinfo;
  for (i = 0;  tinfo[i] != NIL;  i++) {
    ilu_CharBufAppend(&cinfo, (i == 0) ? separator1 : separator2, 1, err);
    if (ILU_ERRNOK(*err)) goto freeup1;
    temp = _ilu_EncodeBuffer(tinfo[i], strlen(tinfo[i]), err);
    if (ILU_ERRNOK(*err)) goto freeup1;
    ilu_CharBufAppend(&cinfo, temp, strlen(temp), err);
    ilu_free(temp);
    if (ILU_ERRNOK(*err)) goto freeup1;
  }
  return cinfo;

 freeup1:
  ilu_CharBufFree(cinfo);
  cinfo.icb_size = 0;
  cinfo.icb_len = 0;
  cinfo.icb_base = 0;
  return cinfo;
}

/* Inside(object_server(obj), object_class(obj)) */
static ilu_cardinal
  FormCinfoVector (ilu_Object obj, ilu_CharBuf **result, ilu_Error *err)
{
  ilu_Port p;
  ilu_ProtocolInfo pinfo;
  ilu_TransportInfo tinfo;
  ilu_Server s = object_server(obj);
  ilu_string temp;
  ilu_CharBuf *cinfo;
  int nports, i, j;
  char separator1[2] = { ILU_CINFO_DIVIDER, 0 };
  char separator2[2] = { ILU_TINFO_DIVIDER, 0 };

  if (server_is_true(s)) {
    for (p = server_ports(s), nports = 0;  p != NIL;  p = p->po_links.pl_next, nports += 1)
      ;
  } else {
    nports = 1;
  }
  *result = (ilu_CharBuf *) ilu_MallocE(sizeof(ilu_CharBuf) * nports, err);
  if (server_is_true(s))
    p = server_ports(s);
  for (i = 0;  i < nports;  i++) {
    cinfo = &(*result)[i];
    if (server_is_true(s)) {
      pinfo = port_pinfo(p);
      tinfo = port_tinfo(p);
    } else {
      pinfo = s->sr_pinfo;
      tinfo = s->sr_tinfo;
    }
    temp = _ilu_EncodeBuffer(pinfo, strlen(pinfo), err);
    if (ILU_ERRNOK(*err)) return 0;
    *cinfo = ilu_CharBufFromString(temp, err);
    ilu_free(temp);
    if (ILU_ERRNOK(*err)) return 0;
    for (j = 0;  tinfo[j] != NIL;  j++) {
      ilu_CharBufAppend(cinfo, (j == 0) ? separator1 : separator2, 1, err);
      if (ILU_ERRNOK(*err)) goto freeup1;
      temp = _ilu_EncodeBuffer(tinfo[j], strlen(tinfo[j]), err);
      if (ILU_ERRNOK(*err)) goto freeup1;
      ilu_CharBufAppend(cinfo, temp, strlen(temp), err);
      ilu_free(temp);
      if (ILU_ERRNOK(*err)) goto freeup1;
    }
    if (server_is_true(s))
      p = p->po_links.pl_next;
  }
  return nports;

 freeup1:
  for (j = 0;  j < i;  j += 1) {
    ilu_CharBufFree((*result)[j]);
  }
  ilu_free(*result);
  return 0;
}

/* Inside(object_server(obj), object_class(obj)) */
ilu_string
  ilu_w3ng_URLOfObject (ilu_Object obj, ilu_Error *err)
{
  ilu_string sid = server_id(object_server(obj));
  ilu_string mstid;
  ilu_string ih = object_ih(obj);
  ilu_string encoded_sid, encoded_ih, encoded_mstid;
  ilu_CharBuf cinfo;
  ilu_cardinal templen;
  ilu_string buf = NIL;

  if (object_is_singleton(obj) &&
      (strncmp(object_singleton_info(obj), "w3ng", 4) != 0)) {
    ILU_NOTE(W3NG_DEBUG,
	     ("ilu_w3ng_URLOfObject:  can't form W3NG URL for singleton (%s) object %s/%s.\n",
	      object_singleton_info(obj), sid, ih));
    return ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_ps, NIL);
  };

  cinfo = FigureNGCinfo(obj, err);
  if (ILU_ERRNOK(*err))
    return NIL;

  encoded_sid = _ilu_EncodeBuffer(sid, strlen(sid), err);
  if (ILU_ERRNOK(*err)) goto freeup1;
  encoded_ih = _ilu_EncodeBuffer(ih, strlen(ih), err);
  if (ILU_ERRNOK(*err)) goto freeup2;
  mstid = ilu_MstidOfObject(obj);
  encoded_mstid = _ilu_EncodeBuffer(mstid, strlen(mstid), err);
  if (ILU_ERRNOK(*err)) goto freeup3;
  templen = strlen(encoded_mstid) + strlen(encoded_sid) + strlen(encoded_ih)
    + cinfo.icb_len + 30;
  buf = ilu_MallocE(templen, err);
  if (ILU_ERRNOK(*err)) goto freeup4;
  sprintf (buf, "w3ng:%s/%s;type=%s;cinfo=%*.*s",
	   encoded_sid, encoded_ih, encoded_mstid,
	   (int) cinfo.icb_len, (int) cinfo.icb_len, cinfo.icb_base);
  ILU_CLER(*err);
 freeup4:
  ilu_free(encoded_mstid);
 freeup3:
  ilu_free(encoded_ih);
 freeup2:
  ilu_free(encoded_sid);
 freeup1:
  ilu_CharBufFree(cinfo);
  return buf;
}

/*======================================================================
**======================================================================
**====================  data marshalling  ==============================
**======================================================================
**====================================================================*/

/* ==================== cardinal ==================== */

#define SWAP_WORD(a) ( ((a) << 24) | \
                      (((a) << 8) & 0x00ff0000) | \
                      (((a) >> 8) & 0x0000ff00) | \
        ((ilu_cardinal)(a) >>24) )

#ifdef WORDS_BIGENDIAN
#define cardout(buf,l) *((ilu_cardinal *) buf) = l;
#else
#define cardout(buf,l)                  \
        *((ilu_cardinal *)buf) = SWAP_WORD((l));
#endif

#define Output_Cardinal_Work(trans,l,err) \
{					\
  register unsigned char *buf;		\
  ILU_CLER(*(err));			\
  buf = transport_get_output_buffer((trans), 4, err);	\
  if (ILU_ERROK(*(err))) {		\
    cardout(buf,l)			\
  }					\
}					\

static void
_w3ng_OutputCardinal(ilu_Call call, ilu_cardinal l,
		       ILU_ERRS((IoErrs)) * err)
{
  Output_Cardinal_Work(w3ng_transport(call), l, err);
}

#define Output_Cardinal(t,l,e)	Output_Cardinal_Work(t,l,e)

static void
_w3ng_InputCardinal(ilu_Call call, ilu_cardinal * i,
		      ILU_ERRS((IoErrs)) * err)
{
  register unsigned char *buf;
  buf = transport_get_input_buffer(w3ng_transport(call), 4, err);
  if (buf != NIL) {
#ifdef WORDS_BIGENDIAN
    *i = *((ilu_cardinal *) buf);
#else                           /* not bigendian */
    register ilu_cardinal tmp = *((ilu_cardinal *) buf);
    *i = SWAP_WORD(tmp);
#endif
  }
}

/* ==================== integer ==================== */

static void 
_w3ng_OutputInteger(ilu_Call call, ilu_integer i,
		      ILU_ERRS((IoErrs)) * err)
{
  Output_Cardinal(w3ng_transport(call), (ilu_cardinal) i, err);
}

static void
_w3ng_InputInteger(ilu_Call call, ilu_integer * i,
		     ILU_ERRS((IoErrs)) * err)
{
  _w3ng_InputCardinal (call, (ilu_cardinal *) i, err);
}

/* ==================== short integer ==================== */

static void 
_w3ng_OutputShortInteger(ilu_Call call, ilu_shortinteger i,
			   ILU_ERRS((IoErrs)) * err)
{
  _w3ng_OutputInteger(call, (ilu_integer) i, err);
}

static void
_w3ng_InputShortInteger(ilu_Call call, ilu_shortinteger * i,
			  ILU_ERRS((IoErrs)) * err)
{
  ilu_integer     l = 0;

  _w3ng_InputInteger (call, &l, err);
  if (ILU_ERROK(*err))
    *i = (ilu_shortinteger) l;
  return;
}

/* ==================== long integer ==================== */

static void
_w3ng_OutputLongInteger(ilu_Call call, ilu_longinteger i,
			  ILU_ERRS((IoErrs)) * err)
{
  _w3ng_OutputInteger(call, ILU_LONGINT_HIGH_WORD(&i), err);
  if (ILU_ERROK(*err))
    Output_Cardinal(w3ng_transport(call), ILU_LONGINT_LOW_WORD(&i), err);
}

static void 
_w3ng_InputLongInteger(ilu_Call call, ilu_longinteger * i,
			 ILU_ERRS((IoErrs)) * err)
{
  _w3ng_InputInteger (call, &ILU_LONGINT_HIGH_WORD(i), err);
  if (ILU_ERROK(*err))
    _w3ng_InputCardinal (call, &ILU_LONGINT_LOW_WORD(i), err);
  return;
}

/* ==================== short cardinal ==================== */

static void 
_w3ng_OutputShortCardinal(ilu_Call call, ilu_shortcardinal i,
			    ILU_ERRS((IoErrs)) * err)
{
  Output_Cardinal(w3ng_transport(call), (ilu_cardinal) i, err);
}

static void
_w3ng_InputShortCardinal(ilu_Call call, ilu_shortcardinal * i,
			   ILU_ERRS((IoErrs)) * err)
{
  ilu_cardinal    l = 0;

  _w3ng_InputCardinal (call, &l, err);
  if (ILU_ERROK(*err))
    *i = (ilu_shortcardinal) (l & 0xFFFF);
}

/* ==================== long cardinal ==================== */

static void 
_w3ng_OutputLongCardinal(ilu_Call call, ilu_longcardinal i,
			   ILU_ERRS((IoErrs)) * err)
{
  Output_Cardinal(w3ng_transport(call), ILU_LONGCARD_HIGH_WORD(&i), err);
  if (ILU_ERROK(*err))
    Output_Cardinal(w3ng_transport(call), ILU_LONGCARD_LOW_WORD(&i), err);
}

static void
_w3ng_InputLongCardinal(ilu_Call call, ilu_longcardinal * i,
			  ILU_ERRS((IoErrs)) * err)
{
  _w3ng_InputCardinal (call, &ILU_LONGCARD_HIGH_WORD(i), err);
  if (ILU_ERROK(*err))
    _w3ng_InputCardinal (call, &ILU_LONGCARD_LOW_WORD(i), err);
  return;
}

/* ==================== real ==================== */

static void 
_w3ng_OutputReal(ilu_Call call, double d,
		   ILU_ERRS((IoErrs)) * err)
{
  double          l2 = d;

#ifdef WORDS_BIGENDIAN
  Output_Cardinal(w3ng_transport(call), ((ilu_cardinal *) (&l2))[0], err);
  if (ILU_ERROK(*err))
    Output_Cardinal(w3ng_transport(call), ((ilu_cardinal *) (&l2))[1], err);
#else
  Output_Cardinal(w3ng_transport(call), ((ilu_cardinal *) (&l2))[1], err);
  if (ILU_ERROK(*err))
    Output_Cardinal(w3ng_transport(call), ((ilu_cardinal *) (&l2))[0], err);
#endif

}

static void 
_w3ng_InputReal(ilu_Call call, double *d,
		  ILU_ERRS((IoErrs)) * err)
{
  double l2;

#ifdef WORDS_BIGENDIAN
  _w3ng_InputCardinal(call, ((ilu_cardinal *) &l2), err);
  if (ILU_ERROK(*err))
    {
      _w3ng_InputCardinal(call, ((ilu_cardinal *) &l2) + 1, err);
      if (ILU_ERROK(*err)) *d = l2;
    }
#else
  _w3ng_InputCardinal(call, ((ilu_cardinal *) &l2) + 1, err);
  if (ILU_ERROK(*err))
    {
      _w3ng_InputCardinal(call, ((ilu_cardinal *) &l2), err);
      if (ILU_ERROK(*err)) *d = l2;
    }
#endif

}

/* ==================== long real ==================== */

static void 
_w3ng_OutputLongReal(ilu_Call call, ilu_longreal d,
		       ILU_ERRS((IoErrs)) * err)
{
  transport_write_bytes(w3ng_transport(call), (ilu_bytes) &d, 16, err);
}

static void
_w3ng_InputLongReal(ilu_Call call, ilu_longreal * d,
		      ILU_ERRS((IoErrs)) * err)
{
  (void) transport_read_bytes(w3ng_transport(call), (ilu_bytes) d, 16,
			      err);
  return;
}

/* ==================== short real ==================== */

static void 
_w3ng_OutputShortReal(ilu_Call call, float f,
			ILU_ERRS((IoErrs)) * err)
{
  float           f2;

  f2 = f;
  Output_Cardinal(w3ng_transport(call), *((ilu_cardinal *) & f2), err);
}

static void 
_w3ng_InputShortReal(ilu_Call call, float *f,
		       ILU_ERRS((IoErrs)) * err)
{
  _w3ng_InputCardinal (call, (ilu_cardinal *) f, err);
}

/* ==================== bytes ==================== */

static void
_w3ng_OutputBytes(ilu_Call call, ilu_bytes s, ilu_cardinal len,
		    ilu_cardinal limit, ILU_ERRS((IoErrs)) * err)
{
  if (limit > 0 && len > limit) {
    ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_sequenceLimit, 0);
    return;
  } else {
    ilu_cardinal    paddedlen = PADDED_SIZE(len);
    Output_Cardinal(w3ng_transport(call), len, err);
    if (paddedlen == 0)
      return;
    if (ILU_ERROK(*err))
      (void) transport_write_bytes(w3ng_transport(call), s,
				   paddedlen, err);
  }
}

static void
InputBytes(ilu_Call call, ilu_bytes * s, ilu_cardinal * len,
	   ilu_cardinal limit, ILU_ERRS((IoErrs)) * err,
	   ilu_boolean string_p)
{
  ilu_cardinal    size;

  ILU_CLER(*err);
  if (_w3ng_InputCardinal(call, len, err), ILU_ERROK(*err)) {
    if (limit > 0 && *len > limit) {
      ILU_ERR_CONS1(marshal, err, minor, ilu_mm_sequenceLimit, 0);
      return;
    }
    if (!string_p && *len == 0)
      {
	*s = NIL;
	return;
      }
    size = PADDED_SIZE(*len);
    *s = ilu_malloc(size + 4);
    if (*s == NIL) {
      ILU_ERR_CONS1(no_memory, err, nbytes, size, 0);
      return;
    }
    (void) transport_read_bytes(w3ng_transport(call), *s, size, err);
    (*s)[*len] = 0;
    /* ... so this can be used to input a C string */
  }
  return;
}

static void
_w3ng_InputBytes(ilu_Call call, ilu_bytes * s, ilu_cardinal * len,
		   ilu_cardinal limit, ILU_ERRS((IoErrs)) * err)
{
  InputBytes (call, s, len, limit, err, ilu_FALSE);
}

/* ==================== string ==================== */

static void 
_w3ng_OutputString(ilu_Call call, void * s, ilu_cardinal len,
		   ilu_cardinal limit,
		   ilu_cardinal expected_encoding,
		   ilu_cardinal current_encoding,
		   ILU_ERRS((IoErrs)) * err)
{
  W3ng  *d = w3ng_instance_data(call);

  if ((d->outgoing_default_charset == 0) ||
      (d->outgoing_default_charset != current_encoding)) {
    ilu_cardinal	paddedlen = PADDED_SIZE(len + 2);
    ilu_byte		buf[2];
    _w3ng_OutputCardinal(call, 0x80000000 | (len + 2), err);
    if (ILU_ERRNOK(*err)) return;
    buf[0] = (current_encoding >> 8) & 0xFF;
    buf[1] = current_encoding & 0xFF;
    (void) transport_write_bytes(w3ng_transport(call), buf, 2, err);
    if (ILU_ERRNOK(*err)) return;
    (void) transport_write_bytes(w3ng_transport(call), s, paddedlen - 2, err);
    if (ILU_ERRNOK(*err)) return;
  } else {
    _w3ng_OutputBytes(call, (ilu_bytes) s, len, limit, err);
  }
}

static void
_w3ng_InputString(ilu_Call call, void ** s, ilu_cardinal * len,
		  ilu_cardinal limit,
		  ilu_cardinal expected_encoding,
		  ilu_cardinal *actual_encoding,
		  ILU_ERRS((IoErrs)) * err)
{
  ilu_cardinal len_and_flag, true_len;
  W3ng  *d = w3ng_instance_data(call);
  ilu_bytes inbuf;
  ilu_cardinal paddedlen, to_read;

  _w3ng_InputCardinal(call, &len_and_flag, err);
  paddedlen = PADDED_SIZE(len_and_flag & 0x7FFFFFFF);
  if (ILU_ERRNOK(*err)) return;
  if ((len_and_flag & 0x80000000) != 0) {
    ilu_byte buf[2];
    true_len = (len_and_flag & 0x7FFFFFFF) - 2;
    (void) transport_read_bytes(w3ng_transport(call), buf, 2, err);
    if (ILU_ERRNOK(*err)) return;
    to_read = paddedlen - 2;
    *actual_encoding = (buf[0] << 8) + buf[1];
  } else {
    true_len = len_and_flag;
    to_read = paddedlen;
    *actual_encoding = d->incoming_default_charset;
  }
  inbuf = (ilu_bytes) ilu_MallocE(to_read + 1, err);
  if (ILU_ERRNOK(*err)) return;
  (void) transport_read_bytes(w3ng_transport(call), inbuf, to_read, err);
  if (ILU_ERRNOK(*err)) return;
  inbuf[true_len] = 0;	/* for C... */
  *len = true_len;
  *s = inbuf;
}

/* ==================== byte ==================== */

static void 
_w3ng_OutputByte(ilu_Call call, ilu_byte b, ILU_ERRS((IoErrs)) * err)
{
  Output_Cardinal(w3ng_transport(call), (ilu_cardinal) b, err);
}

static void 
_w3ng_InputByte(ilu_Call call, ilu_byte * b,
		  ILU_ERRS((IoErrs)) * err)
{
  ilu_cardinal    l = 0;

  _w3ng_InputCardinal (call, &l, err);
  if (ILU_ERROK(*err))
    *b = (ilu_byte) (l & 0xFF);
}

/* ==================== boolean ==================== */

static void 
_w3ng_OutputBoolean(ilu_Call call, ilu_boolean b, ILU_ERRS((IoErrs)) * err)
{
  Output_Cardinal(w3ng_transport(call), (b != ilu_FALSE) ? 1 : 0, err);
}

static void 
_w3ng_InputBoolean(ilu_Call call, ilu_boolean * b,
		   ILU_ERRS((IoErrs)) * err)
{
  ilu_cardinal    l = 0;

  _w3ng_InputCardinal (call, &l, err);
  if (ILU_ERROK(*err))
    *b = ((l != 0) ? ilu_TRUE : ilu_FALSE);
}

/* ==================== short char ==================== */

static void
_w3ng_OutputShortChar(ilu_Call call, ilu_shortcharacter b,
			ILU_ERRS((IoErrs)) * err)
{
  _w3ng_OutputByte(call, (ilu_byte) b, err);
}

static void
_w3ng_InputShortChar(ilu_Call call, ilu_shortcharacter * b,
		       ILU_ERRS((IoErrs)) * err)
{
  _w3ng_InputByte(call, (ilu_byte *) b, err);
}

/* ==================== opaque ==================== */

static void
  _w3ng_OutputOpaque(ilu_Call call, ilu_bytes o, ilu_cardinal len,
		     ILU_ERRS((IoErrs)) * err)
{
  ilu_cardinal    paddedlen = PADDED_SIZE(len);
  (void) transport_write_bytes(w3ng_transport(call), o, paddedlen, err);
}

static void
  _w3ng_OutputOpaque2(ilu_Call call,
		      ilu_bytes bytes1, ilu_cardinal len1,
		      ilu_bytes bytes2, ilu_cardinal len2,
		      ILU_ERRS((IoErrs)) * err)
{
  ilu_cardinal    paddedlen = PADDED_SIZE(len1 + len2) - len1;
  (void) transport_write_bytes(w3ng_transport(call), bytes1, len1, err);
  (void) transport_write_bytes(w3ng_transport(call), bytes2, paddedlen, err);
}

static void
_w3ng_InputOpaque(ilu_Call call, ilu_bytes * o, ilu_cardinal len,
		  ILU_ERRS((IoErrs)) * err)
{
  if (*o == NIL) {
    if ((*o = ilu_malloc(PADDED_SIZE(len))) == NIL) {
      (void) ILU_ERR_CONS1(no_memory, err, nbytes, PADDED_SIZE(len), 0);
      return;
    }
  }
  (void) transport_read_bytes(w3ng_transport(call), *o,
			      PADDED_SIZE(len), err);
}

/* ==================== optional ==================== */

static void 
_w3ng_OutputOptional(ilu_Call call, ilu_boolean i,
		       ilu_Type prefix, ILU_ERRS((IoErrs)) * err)
{
  Output_Cardinal(w3ng_transport(call), (ilu_cardinal) i, err);
}

static void 
_w3ng_InputOptional(ilu_Call call, ilu_boolean *i,
		      ilu_Type prefix, ILU_ERRS((IoErrs)) * err)
{
  ilu_cardinal    i2 = 0;

  _w3ng_InputCardinal (call, &i2, err);
  if (ILU_ERROK(*err)) *i = ((i2 == 0) ? ilu_FALSE : ilu_TRUE);
}

/* ==================== enumeration ==================== */

static void 
_w3ng_OutputEnumeration(ilu_Call call, ilu_shortcardinal i,
			  ilu_Type prefix,
			  ILU_ERRS((IoErrs)) * err)
{
  Output_Cardinal(w3ng_transport(call), (ilu_cardinal) i, err);
}

static void 
_w3ng_InputEnumeration(ilu_Call call, ilu_shortcardinal * i,
			 ilu_Type prefix,
			 ILU_ERRS((IoErrs)) * err)
{
  _w3ng_InputShortCardinal (call, i, err);
}

/* ==================== sequence ==================== */

static void
_w3ng_OutputSequence(ilu_Call c, ilu_cardinal sequenceLength,
		       ilu_cardinal limit, ilu_Type prefix, ILU_ERRS((IoErrs)) * err)
{
  if (limit > 0 && sequenceLength > limit)
    {
      ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_sequenceLimit, 0);
    }
  else  
    Output_Cardinal(w3ng_transport(c), sequenceLength, err);
}

static void 
_w3ng_OutputSequenceMark(ilu_Call c,
			   ilu_cardinal extent,
			   ILU_ERRS((IoErrs)) * err)
{
  ILU_CLER(*err);
}

static void 
_w3ng_InputSequenceMark(ilu_Call c, ilu_cardinal extent,
			  ILU_ERRS((IoErrs)) * err)
{
  ILU_CLER(*err);
}

static void
_w3ng_InputSequence(ilu_Call c, ilu_cardinal * sequenceLength,
		      ilu_cardinal limit, ilu_Type prefix, ILU_ERRS((IoErrs)) * err)
{
  ilu_cardinal    len = 0;
  _w3ng_InputCardinal(c, &len, err);
  if (ILU_ERROK(*err))
    {
      if (limit > 0 && len > limit)
	{
	  ILU_ERR_CONS1(marshal, err, minor, ilu_mm_sequenceLimit, 0);
	  return;
	}
      else
	*sequenceLength = len;
    }
}

static void _w3ng_EndSequence (ilu_Call c, ILU_ERRS((IoErrs)) * err)
{
  ILU_CLER(*err);
}

/* ==================== union ==================== */

static void 
_w3ng_OutputUnion(ilu_Call c, ilu_cardinal typeIndex,
		    ilu_TypeKind tk, ilu_Type prefix,
		    ILU_ERRS((IoErrs)) * err)
{
  Output_Cardinal(w3ng_transport(c), typeIndex, err);
}

static void 
_w3ng_InputUnion(ilu_Call c, ilu_cardinal * typeIndex,
		   ilu_TypeKind tk, ilu_Type prefix,
		   ILU_ERRS((IoErrs)) * err)
{
  _w3ng_InputCardinal(c, typeIndex, err);
}

static void _w3ng_EndUnion (ilu_Call c, ILU_ERRS((IoErrs)) * err)
{
  ILU_CLER(*err);
}

/* ==================== array ==================== */

static void 
_w3ng_OutputArray(ilu_Call c, ilu_cardinal len,
		    ilu_Type prefix, ILU_ERRS((IoErrs)) * err)
{
  ILU_CLER(*err);
}

static void _w3ng_InputArray(ilu_Call c, ilu_Type prefix, ILU_ERRS((IoErrs)) * err)
{
  ILU_CLER(*err);
}

static void _w3ng_EndArray (ilu_Call c, ILU_ERRS((IoErrs)) * err)
{
  ILU_CLER(*err);
}

/* ==================== record ==================== */

static void 
_w3ng_OutputRecord(ilu_Call c, ilu_Type prefix, ILU_ERRS((IoErrs)) * err)
{
  ILU_CLER(*err);
}

static void _w3ng_InputRecord(ilu_Call c, ilu_Type prefix, ILU_ERRS((IoErrs)) * err)
{
  ILU_CLER(*err);
}

static void _w3ng_EndRecord (ilu_Call c, ILU_ERRS((IoErrs)) * err)
{
  ILU_CLER(*err);
}

/* ==================== object ==================== */

static ilu_boolean
  EnsureObjectCache (W3ng *instance_data,
		     ilu_Error *err)
{
  if (instance_data->object_cache == NIL) {
    if (instance_data->incoming)
      instance_data->object_cache
	= ilu_hash_MakeNewTable(OBJECT_CACHE_BUCKETS,
				(ilu_cardinal (*)(ilu_refany, ilu_cardinal)) _w3ng_HashIndex,
				(ilu_boolean (*)(ilu_refany, ilu_refany)) _w3ng_CompareIndex);
    else
      instance_data->object_cache
	= ilu_hash_MakeNewTable(OBJECT_CACHE_BUCKETS,
				(ilu_cardinal (*)(ilu_refany, ilu_cardinal)) _w3ng_HashObject,
				(ilu_boolean (*)(ilu_refany, ilu_refany)) _w3ng_CompareObject);
    if (instance_data->object_cache == NIL)
      return ILU_ERR_CONS1(internal, err, minor, ilu_im_broken, ilu_FALSE);
  };
  ILU_CLER(*err);
  return ilu_TRUE;
}

static ilu_boolean
  CacheObject (W3ng *instance_data,
	       ilu_string sid,	/* RETAIN */
	       ilu_string ih,	/* RETAIN */
	       ilu_cardinal *index,
	       ilu_Error *err)
{
  CachedObject *obj;

  if (!EnsureObjectCache(instance_data, err))
    return ilu_FALSE;
  if ((instance_data->current_object_cache_val + 1) >= MAX_OP_CACHE_INDEX)
    return ILU_ERR_CONS1(imp_limit, err, minor, ilu_ilm_strlen, ilu_FALSE);
  obj = ilu_MallocE(sizeof(CachedObject), err);
  if (ILU_ERRNOK(*err)) return ilu_FALSE;
  obj->cache_index = instance_data->current_object_cache_val++;
  obj->serverID = ilu_StrdupE(sid, err);
  if (ILU_ERRNOK(*err)) goto faild1;
  obj->instance_handle = ilu_StrdupE(ih, err);
  if (ILU_ERRNOK(*err)) goto faild2;
  if ((instance_data->incoming &&
       (!ilu_hash_AddToTable(instance_data->object_cache, (ilu_refany) obj->cache_index, obj))) ||
      (!instance_data->incoming &&
       (!ilu_hash_AddToTable(instance_data->object_cache, obj, obj)))) {
    ILU_ERR_CONS1(internal, err, minor,ilu_im_callFail, ilu_FALSE);
    goto faild3;
  };
  *index = obj->cache_index;
  return ilu_TRUE;
 faild3:
  ilu_free(obj->instance_handle);
 faild2:
  ilu_free(obj->serverID);
 faild1:
  ilu_free(obj);
  return ilu_FALSE;
}

static ilu_boolean
  FindCachedObjectFromIndex (W3ng *instance_data,
			     ilu_Server s,
			     ilu_cardinal index,
			     ilu_Object *object_out,
			     ilu_Error *err)
{
  CachedObject *obj;
  ilu_Object kobj;

  if (!EnsureObjectCache(instance_data, err))
    return ilu_FALSE;
  if (instance_data->incoming) {
    if ((obj = ilu_hash_FindInTable(instance_data->object_cache, (ilu_refany) index)) == NIL)
      return ILU_ERR_CONS1(internal, err, minor, ilu_im_callFail, ilu_FALSE);
    kobj = _ilu_FindObjectInServer(obj->instance_handle, s);
    if (kobj == NIL) {
      ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_sbh, 0);
    } else {
      *object_out = kobj;
      ILU_CLER(*err);
    }
  } else {
    ILU_ERR_CONS1(internal, err, minor, ilu_im_broken, 0);
  }
  return ILU_ERROK(*err);
}

static ilu_boolean
  FindCachedObjectFromObjData (W3ng *instance_data,
			       ilu_Object kobj,
			       ilu_cardinal *index,
			       ilu_Error *err)
{
  CachedObject *obj;
  ilu_boolean ans;

  if (!EnsureObjectCache(instance_data, err))
    return ilu_FALSE;
  if (!instance_data->incoming) {
    CachedObject temp;
    temp.serverID = server_id(object_server(kobj));
    temp.instance_handle = object_ih(kobj);
    temp.cache_index = 0;
    if ((obj = ilu_hash_FindInTable(instance_data->object_cache, &temp)) == NIL)
      ans = ilu_FALSE;
    else {
      *index = obj->cache_index;
      ans = ILU_CLER(*err);
    }
  } else {
    ILU_ERR_CONS1(internal, err, minor, ilu_im_broken, 0);
  }
  return ans;
}

static ilu_boolean
  cinfo_compare (ilu_CharBuf *cinfo1, ilu_cardinal cinfolen1,
		 ilu_CharBuf *cinfo2, ilu_cardinal cinfolen2)
{
  unsigned int i;
  if (cinfolen1 != cinfolen2)
    return ilu_FALSE;
  for (i = 0;  i < cinfolen1;  i+= 1)
    {
      if ((cinfo1[i].icb_len != cinfo2[i].icb_len) ||
	  (memcmp(cinfo1[i].icb_base, cinfo2[i].icb_base, cinfo1[i].icb_len) != 0))
	return ilu_FALSE;
    }
  return ilu_TRUE;
}

static ilu_boolean
  _w3ng_BeginOutputObject (ilu_Call call,
			   ilu_Object kobj,
			   ilu_boolean discriminant_p,
			   ilu_Class formal_type,
			   ilu_cardinal nstates,
			   ILU_ERRS((IoErrs)) * err)
{
  if (!discriminant_p) {
    ilu_bytes typeid;

#ifdef ILU_W3NG_ALLOW_ILU_URLS
    if (kobj == NIL) {
      typeid = (ilu_bytes) "";
    } else
#endif
      {
	if (object_class(kobj) == formal_type)
	  typeid = (ilu_bytes) "";
	else
	  typeid = (ilu_bytes) object_mstid(kobj);
      }
    if (kobj != NIL)
      ilu_ExitServer(object_server(kobj), object_class(kobj));
    _w3ng_OutputBytes (call, typeid, strlen((const char *) typeid), 0xFFFF, err);
    if (kobj != NIL)
      ilu_EnterServer(object_server(kobj), object_class(kobj));
    if (ILU_ERRNOK(*err)) return ilu_FALSE;
    ilu_OutputCardinal (call, nstates, err);
    return ILU_ERROK(*err);
  }
  return ILU_CLER(*err);
}
			   
static ilu_boolean
  _w3ng_BeginOutputState (ilu_Call call,
			  ilu_Object kobj,
			  ilu_Class state_class,
			  ilu_Error *err)
{
  W3ngCallData *cd = w3ng_call_data(call);
  ilu_Transport trans;
  w3ng_TransportStack ts;
  ilu_Error lerr;

  ILU_NOTE(W3NG_DEBUG,
	   ("ILU(_w3ng_BeginOutputState):  kobj=%s/%s, state_class=%s\n",
	    server_id(object_server(kobj)), object_ih(kobj), class_name(state_class)));

  /* First, output the type UID of the object type the state is from */
  _w3ng_OutputBytes (call, (ilu_bytes) class_unique_id(state_class),
		     strlen(class_unique_id(state_class)), 0xFFFF, err);
  if (ILU_ERRNOK(*err)) return ilu_FALSE;
  /* we need to form an encapsulation here.  We do so by diverting the
     output to a pseudo-transport... */
  trans = _ilu_BufferTransport_Create (100, NIL, err);
  if (ILU_ERRNOK(*err)) return ilu_FALSE;
  /* replace the real transport with the pseudo-transport */
  ts = ilu_MallocE(sizeof(*ts), err);
  if (ILU_ERRNOK(*err)) goto err1;
  ts->transport = w3ng_transport(call);
  ts->next = cd->transports;
  cd->transports = ts;
  w3ng_transport(call) = trans;
  return ilu_TRUE;

 err1:
  _ilu_BufferTransport_Destroy (trans, NIL, NIL, &lerr);
  ILU_HANDLED(lerr);
  return ilu_FALSE;
}

static ilu_boolean
  _w3ng_EndOutputState (ilu_Call call,
			ilu_Object kobj,
			ilu_Class state_class,
			ilu_Error *err)
{
  W3ngCallData *cd = w3ng_call_data(call);
  ilu_Transport trans;
  w3ng_TransportStack ts;
  ilu_bytes state_data;
  ilu_cardinal state_data_len;

  ILU_NOTE(W3NG_DEBUG,
	   ("ILU(_w3ng_EndOutputState):  kobj=%s/%s, state_class=%s\n",
	    server_id(object_server(kobj)), object_ih(kobj), class_name(state_class)));

  /* We've marshalled all the state into w3ng_transport, a pseudo-transport
     that's buffered the output.  We now replace the real transport, and write
     the buffered pseudo-transport to it. */
  /* 1.  Pop the real transport back into position. */
  trans = w3ng_transport(call);
  ts = cd->transports;
  w3ng_transport(call) = ts->transport;
  cd->transports = ts->next;
  ilu_free(ts);
  /* 2.  Get the bytes from the pseudo-transport, and close it. */
  _ilu_BufferTransport_Destroy (trans, &state_data_len, &state_data, err);
  if (ILU_ERRNOK(*err)) return ilu_FALSE;
  /* 3.  Write the data to the underlying transport */
  _w3ng_OutputBytes (call, state_data, state_data_len, 0xFFFFFFFF, err);
  ilu_free(state_data);
  return ILU_ERROK(*err);
}

static ilu_boolean
  OutputRequestHeader (ilu_Call call, unsigned use_cache, unsigned index, unsigned cache_this, ilu_Error *err)
{
  ilu_cardinal msgheader;
  ilu_string id;
  W3ngCallData *call_data = w3ng_call_data(call);

  msgheader = (((call_data->op_cached << 14) |
		(call_data->op_cached ?
		 (call_data->op_cache_index & 0x3FFF) :
		 ((call_data->cache_op << 13) | (call_data->op_index & 0x1FFF)))) << 15) |
		   ((use_cache << 14) |
		    (use_cache ?
		     (index & 0x3FFF) :
		     ((cache_this << 13) | (index & 0x1FFF))));
  _w3ng_OutputCardinal(call, msgheader, err);
  if (ILU_ERRNOK(*err)) return ilu_FALSE;
  if (!call_data->op_cached) {
    id = class_unique_id(call_intro_type(call));
    _w3ng_OutputString(call, (ilu_bytes) id, strlen(id), 0x3FFF,
		       ILU_StringEncoding_latin1, ILU_StringEncoding_latin1, err);
    id = NIL;
    if (ILU_ERRNOK(*err)) return ilu_FALSE;
  }
  return ilu_TRUE;
}
  
static ilu_boolean
  _w3ng_EndOutputObject (ilu_Call call,
			 ilu_Object kobj,
			 ilu_boolean discriminant_p,
			 ilu_Class formal_type,
			 ILU_ERRS((IoErrs)) * err)
{
  W3ngCallData *call_data = w3ng_call_data(call);
  W3ng *	instance_data = w3ng_instance_data(call);
  ilu_cardinal	index, prefix_len;
  ilu_boolean	use_cache = ilu_FALSE;
  ilu_boolean	cache_this = ilu_FALSE;
  ilu_cardinal	msgheader;
  ilu_bytes	obj_name;
  ilu_byte	prefix[4];

  if (discriminant_p) {
    /* already cached? */
    cache_this = ilu_FALSE;
    use_cache = FindCachedObjectFromObjData(instance_data, kobj, &index, err);
    if (ILU_ERRNOK(*err)) {
      ILU_HANDLED(*err);
      use_cache = ilu_FALSE;
    };
    if (!use_cache) {		/* try caching */
      ilu_Server      s = object_server(kobj);
      cache_this = CacheObject(instance_data, server_id(s), object_ih(kobj), &index, err);
      if (ILU_ERRNOK(*err))
	goto errout;
    };
    if (!use_cache) {
      obj_name = (ilu_bytes) object_ih(kobj);
      index = strlen((ilu_string) obj_name);
#ifdef ILU_W3NG_RELATIVE_IH_PASSING
      {
	ilu_bytes p, q;

	for (p = obj_name, q = instance_data->outgoing_ih_prefix, prefix_len = 0;
	     (*p == *q) && (*p != 0) && (prefix_len < 256);  p++, q++, prefix_len++)
	  ;
	index = index - prefix_len + 1;
	memcpy (q, p, sizeof(instance_data->outgoing_ih_prefix) - prefix_len);
	obj_name = p;
	prefix[0] = prefix_len;
	ILU_NOTE(W3NG_DEBUG,
		 ("ILU(w3ng_EndOutputObject):  prefix of %lu bytes for discriminant ih \"%s\"\n",
		  prefix_len, object_ih(kobj)));
      }
#endif /* def ILU_W3NG_RELATIVE_IH_PASSING */
    }
    ilu_ExitServer(object_server(kobj), object_class(kobj));
    OutputRequestHeader (call, use_cache, index, cache_this, err);
    if (ILU_ERRNOK(*err)) goto errout;
    if (!use_cache) {
#ifdef ILU_W3NG_RELATIVE_IH_PASSING
      _w3ng_OutputOpaque2(call, prefix, 1, obj_name, index - 1, err);
#else
      _w3ng_OutputOpaque(call, obj_name, index, err);
#endif /* def ILU_W3NG_RELATIVE_IH_PASSING */
      if (ILU_ERRNOK(*err)) goto errout;
    }
    ILU_CLER(*err);
    return ilu_TRUE;
  } else {
    ilu_bytes instancehandle;
    ilu_bytes sid;
    ilu_CharBuf *cinfo;
    ilu_cardinal cinfo_count, i, sidlen;
    ilu_boolean use_cached_sid, cinfo_precious = ilu_FALSE;

#ifdef ILU_W3NG_ALLOW_ILU_URLS
    if (kobj == NIL) {
      sid = (ilu_bytes) "";
      instancehandle = (ilu_bytes) "";
      cinfo_count = 0;
      cinfo = NIL;
    } else {
      sid = (ilu_bytes) server_id(object_server(kobj));
      instancehandle = (ilu_bytes) object_ih(kobj);
      cinfo_count = FormCinfoVector (kobj, &cinfo, err);
      if (ILU_ERRNOK(*err)) goto errout;
    }
#else
    if (kobj == NIL) {
      ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_nil, 0);
      goto errout;
    }
    sid = (ilu_bytes) server_id(object_server(kobj));
    instancehandle = (ilu_bytes) object_ih(kobj);
    cinfo = (ilu_CharBuf *) ilu_MallocE(sizeof(ilu_CharBuf), err);
    cinfo_count = 1;
    if (ILU_ERRNOK(*err)) return ilu_FALSE;
    cinfo[0] = FigureNGCinfo(kobj, err);
    if (ILU_ERRNOK(*err)) goto errout;
    if (cinfo[0].icb_len == 0) {
      ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_ps, 0);
      goto errout;
    }
#endif /* ILU_W3NG_ALLOW_ILU_URLS */
    if (kobj != NIL)
      ilu_ExitServer(object_server(kobj), object_class(kobj));
    sidlen = strlen((const char *) sid);
    use_cached_sid = ilu_FALSE;
    if (instance_data->outgoing_last_sid != NIL) {
      use_cached_sid = (sidlen == instance_data->outgoing_last_sid_len) &&
	(memcmp((void *) instance_data->outgoing_last_sid, (void *) sid, sidlen) == 0);
      if (!use_cached_sid) {
	/* new sid, so free old one */
	ilu_free(instance_data->outgoing_last_sid);
	instance_data->outgoing_last_sid = NIL;
	instance_data->outgoing_last_sid_len = 0;
	for (i = 0;  i < instance_data->outgoing_last_cinfo_count;  i+= 1)
	  ilu_CharBufFree(instance_data->outgoing_last_cinfo[i]);
	ilu_free(instance_data->outgoing_last_cinfo);
	instance_data->outgoing_last_cinfo = NIL;
	instance_data->outgoing_last_cinfo_count = 0;
      };
    };
    if (!use_cached_sid) {
      instance_data->outgoing_last_sid = ilu_MallocE(sidlen + 1, err);
      if (ILU_ERRNOK(*err)) return ilu_FALSE;
      memcpy((void *) instance_data->outgoing_last_sid, (void *) sid, sidlen);
      instance_data->outgoing_last_sid_len = sidlen;
      instance_data->outgoing_last_sid[instance_data->outgoing_last_sid_len] = 0;
      instance_data->outgoing_last_cinfo = cinfo;
      instance_data->outgoing_last_cinfo_count = cinfo_count;
      cinfo_precious = ilu_TRUE;
    } else {
      sidlen = 0;
      if (!cinfo_compare(cinfo, cinfo_count, instance_data->outgoing_last_cinfo, instance_data->outgoing_last_cinfo_count)) {
	instance_data->outgoing_last_cinfo = cinfo;
	instance_data->outgoing_last_cinfo_count = cinfo_count;
      } else {
	for (i = 0;  i < cinfo_count;  i += 1)
	  ilu_CharBufFree(cinfo[i]);
	ilu_free(cinfo);
	cinfo_count = 0;
	cinfo = NIL;
      }
      cinfo_precious = ilu_TRUE;
    }
    /* Now output the object ID of the remote object */
    _w3ng_OutputBytes (call, sid, sidlen, 0xFFFF, err);
    if (ILU_ERRNOK(*err)) return ilu_FALSE;
#ifdef ILU_W3NG_RELATIVE_IH_PASSING
    {
      ilu_bytes p, q;

      for (p = instancehandle, q = instance_data->outgoing_ih_prefix, prefix_len = 0;
	   (*p == *q) && (*p != 0) && (prefix_len < 256);  p++, q++, prefix_len++)
	;
      index = strlen((const char *) instancehandle) - prefix_len;
      memcpy (q, p, sizeof(instance_data->outgoing_ih_prefix) - prefix_len);
      ILU_NOTE(W3NG_DEBUG,
	       ("ILU(w3ng_EndOutputObject):  prefix of %lu bytes for parameter ih \"%s\"\n",
		prefix_len, instancehandle));
      instancehandle = p;
      prefix[0] = prefix_len;
    }
    _w3ng_OutputCardinal (call, index + 1, err);
    if (ILU_ERRNOK(*err)) return ilu_FALSE;
    _w3ng_OutputOpaque2 (call, prefix, 1, instancehandle, index, err);
#else
    _w3ng_OutputBytes (call, instancehandle, strlen((const char *) instancehandle), 0xFFFF, err);
#endif /* def ILU_W3NG_RELATIVE_IH_PASSING */
    if (ILU_ERRNOK(*err)) return ilu_FALSE;
    _w3ng_OutputSequence (call, cinfo_count, 0, NIL, err);
    if (ILU_ERRNOK(*err)) return ilu_FALSE;
    for (i = 0;  i < cinfo_count;  i += 1) {
      _w3ng_OutputBytes (call, (ilu_bytes) cinfo[i].icb_base, cinfo[i].icb_len, 0xFFFF, err);
      if (ILU_ERRNOK(*err)) return ilu_FALSE;
    }
    if (!cinfo_precious) {
      for (i = 0;  i < cinfo_count;  i += 1)
	ilu_CharBufFree(cinfo[i]);
      ilu_free(cinfo);
    }
    _w3ng_EndSequence (call, err);
    return ILU_ERROK(*err);
  }
 errout:
  ilu_ExitServer(object_server(kobj), object_class(kobj));
  return ilu_FALSE;
}
			   
static ilu_Class
  _w3ng_BeginInputObject (ilu_Call call,
			  ilu_boolean discriminator_p,
			  ilu_Class static_type,
			  ilu_cardinal *nstates,
			  ILU_ERRS((IoErrs)) * err)
{
  W3ng *	instance_data = w3ng_instance_data(call);
  W3ngCallData *call_data = w3ng_call_data(call);
  ilu_Server	server = call_server(call);
  ilu_cardinal	index;
  ilu_Object	kobj;
  ilu_bytes tempb = NIL;

  if (discriminator_p) {
    if (call_data->disc_cached) {
      ilu_EnterServer(server, static_type);
      if (!FindCachedObjectFromIndex(instance_data, call_server(call),
				     call_data->disc_cache_index, &kobj, err)) {
	ilu_ExitServer(server, static_type);
	return ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_inst_nf, NIL);
      } else if (ILU_ERRNOK(*err)) {
	ilu_ExitServer(server, static_type);
	return NIL;
      } else {
	call_data->discriminant = kobj;
	return object_class(kobj);
      }
    } else {
      ilu_string      buf;
#ifdef ILU_W3NG_RELATIVE_IH_PASSING
      buf = ilu_MallocE(PADDED_SIZE(call_data->disc_len) + 1 + sizeof(instance_data->incoming_ih_prefix), err);
#else
      buf = ilu_MallocE(PADDED_SIZE(call_data->disc_len) + 1, err);
#endif /* def ILU_W3NG_RELATIVE_IH_PASSING */
      if (ILU_ERRNOK(*err))
	return NIL;
      _w3ng_InputOpaque(call, (ilu_bytes *) & buf, call_data->disc_len, err);
      if (ILU_ERRNOK(*err)) return (ilu_Class)ILU_NIL;
      buf[call_data->disc_len] = 0;
#ifdef ILU_W3NG_RELATIVE_IH_PASSING
      {
	ilu_cardinal prefix_len = buf[0];
	memmove (buf + prefix_len, buf + 1, call_data->disc_len);
	if (prefix_len > 0) {
	  memcpy (buf, instance_data->incoming_ih_prefix, prefix_len);
	};
	memcpy (instance_data->incoming_ih_prefix, buf, sizeof(instance_data->incoming_ih_prefix));
      }
#endif
      ilu_EnterServer(server, static_type);
      if ((kobj = _ilu_FindObjectInServer(buf, server)) == NIL) {
	ILU_NOTE(INCOMING_DEBUG,
		 ("%s %s not found in server <%s>.\n",
		  "_w3ng_InputObjectID:  discriminant instance", buf,
		  server_id(server)));
	(void) ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_inst_nf, 6);
	ilu_free(buf);
	ilu_ExitServer(server, static_type);
	return NIL;
      }
      if ((!call_data->disc_cached) && (call_data->cache_disc) &&
	  (!CacheObject(instance_data, server_id(server), buf, &index, err))) {
	ilu_free(buf);
	ilu_ExitServer(server, static_type);
	return NIL;
      };
      ilu_free(buf);
      call_data->discriminant = kobj;
      return object_class(kobj);
    }
  } else {
    ilu_cardinal mstidlen;
    ilu_Class found_class;
    w3ng_CharBufStack cbs;

    _w3ng_InputCardinal (call, &mstidlen, err);
    cbs = (w3ng_CharBufStack) ilu_MallocE (sizeof(*cbs) + mstidlen + 4, err);
    if (ILU_ERRNOK(*err)) return NIL;
    cbs->cb.icb_size = mstidlen + 4;
    cbs->cb.icb_base = (ilu_string) (cbs + 1);
    cbs->cb.icb_len = mstidlen;
    _w3ng_InputOpaque (call, (ilu_byte **) &cbs->cb.icb_base, mstidlen, err);
    if (ILU_ERRNOK(*err)) {
      ilu_free(cbs);
      return NIL;
    }
    /* input the state count */
    _w3ng_InputCardinal (call, nstates, err);
    if (ILU_ERRNOK(*err)) {
      ilu_free(cbs);
      return NIL;
    }

    cbs->cb.icb_base[mstidlen] = 0;
    cbs->next = call_data->mstids;
    call_data->mstids = cbs;
    found_class = ilu_FindClassFromID ((ilu_string) cbs->cb.icb_base);
    return found_class;
  }
}

static ilu_string
  _w3ng_BeginInputState (ilu_Call call,
			 ilu_Error *err)
{
  W3ngCallData *cd = w3ng_call_data(call);
  ilu_bytes typeid = NIL, state_data = NIL;
  ilu_cardinal typeidlen, state_data_len;
  ilu_Transport trans;
  w3ng_TransportStack ts;
  ilu_Error lerr;

  _w3ng_InputBytes (call, &typeid, &typeidlen, 0xFFFF, err);
  if (ILU_ERRNOK(*err)) return NIL;
  /* read in state data */
  _w3ng_InputBytes (call, &state_data, &state_data_len, 0xFFFF, err);
  if (ILU_ERRNOK(*err)) goto err1;

  /* set up pseudo-transport to read from */
  trans = _ilu_BufferTransport_Create (state_data_len, state_data, err);
  if (ILU_ERRNOK(*err)) goto err2;
  ts = (w3ng_TransportStack) ilu_MallocE(sizeof(*ts), err);
  if (ILU_ERRNOK(*err)) goto err3;
  ts->transport = w3ng_transport(call);
  ts->next = cd->transports;
  cd->transports = ts;
  w3ng_transport(call) = trans;

  ILU_NOTE(W3NG_DEBUG,
	   ("ILU(_w3ng_BeginInputState):  state_class=<%s>, %lu bytes, stateid=%p\n",
	    typeid, state_data_len, trans));

  return (ilu_string) typeid;

 err3:
  _ilu_BufferTransport_Destroy(trans, NIL, NIL, &lerr);
  ILU_HANDLED(lerr);
 err2:
  ilu_free(state_data);
 err1:
  ilu_free(typeid);
  return (ilu_string)NIL;
}

static ilu_boolean
  _w3ng_SkipInputState (ilu_Call call,
		       ILU_ERRS((IoErrs)) *err)
{
  W3ngCallData *cd = w3ng_call_data(call);
  ilu_Transport trans;
  w3ng_TransportStack ts;

  ILU_NOTE(W3NG_DEBUG,
	   ("ILU(_w3ng_SkipInputState):  stateid=%p\n", w3ng_transport(call)));

  /* replace old transport */
  ts = cd->transports;
  cd->transports = ts->next;
  trans = w3ng_transport(call);
  w3ng_transport(call) = ts->transport;
  ilu_free(ts);
  /* free old transport */
  _ilu_BufferTransport_Destroy (trans, NIL, NIL, err);
  return ILU_ERROK(*err);
}

static ilu_boolean
  _w3ng_EndInputState (ilu_Call call,
		       ILU_ERRS((IoErrs)) *err)
{
  W3ngCallData *cd = w3ng_call_data(call);
  ilu_Transport trans;
  w3ng_TransportStack ts;

  ILU_NOTE(W3NG_DEBUG,
	   ("ILU(_w3ng_FinishInputState):  stateid=%p\n", w3ng_transport(call)));

  /* replace old transport */
  ts = cd->transports;
  cd->transports = ts->next;
  trans = w3ng_transport(call);
  w3ng_transport(call) = ts->transport;
  ilu_free(ts);
  /* free old transport */
  _ilu_BufferTransport_Destroy (trans, NIL, NIL, err);
  return ILU_ERROK(*err);
}

static ilu_Object
  _w3ng_EndInputObject (ilu_Call call,
			ilu_boolean discriminator_p,
			ilu_Class static_type,
			ILU_ERRS((IoErrs)) * err)
{
  W3ng *	instance_data = w3ng_instance_data(call);
  W3ngCallData *call_data = w3ng_call_data(call);
  ilu_Server	server = call_server(call);
  ilu_Object	kobj = NIL;
  ilu_bytes sid, instancehandle;
  ilu_bytes tempb = NIL;
  ilu_CharBuf cinfo;
  w3ng_CharBufStack cbs;

  if (discriminator_p) {
    ILU_CLER(*err);
    kobj = call_data->discriminant;
  } else {
    ilu_bytes mstid;
    ilu_cardinal mstidlen;
    ilu_cardinal instancehandlelen, cinfo_count, sidlen, temp, i;
    ilu_boolean cinfo_precious = ilu_FALSE;

    cbs = call_data->mstids;
    call_data->mstids = cbs->next;
    if (ILU_ERRNOK(*err)) return NIL;
    if (cbs->cb.icb_len == 0) {
      mstid = (ilu_bytes) class_unique_id(static_type);
      mstidlen = strlen((ilu_string) mstid);
    } else {
      mstid = (ilu_bytes) cbs->cb.icb_base;
      mstidlen = cbs->cb.icb_len;
    }
    _w3ng_InputBytes (call, &sid, &sidlen, 0xFFFF, err);
    if (ILU_ERRNOK(*err)) goto errout1;
#ifdef ILU_W3NG_RELATIVE_IH_PASSING
    _w3ng_InputCardinal (call, &instancehandlelen, err);
    if (ILU_ERRNOK(*err)) goto errout2;
    instancehandle = ilu_MallocE(instancehandlelen + 1 + sizeof(instance_data->incoming_ih_prefix), err);
    if (ILU_ERRNOK(*err)) goto errout2;
    _w3ng_InputOpaque (call, &instancehandle, instancehandlelen, err);
    instancehandle[instancehandlelen] = 0;
    if (ILU_ERRNOK(*err)) goto errout2;
    {
      ilu_cardinal prefix_len = instancehandle[0];
      memmove (instancehandle + prefix_len, instancehandle + 1, instancehandlelen);
      if (prefix_len > 0) {
	memcpy (instancehandle, instance_data->incoming_ih_prefix, prefix_len);
      }
      memcpy (instance_data->incoming_ih_prefix, instancehandle, sizeof(instance_data->incoming_ih_prefix));
    }
#else
    _w3ng_InputBytes (call, &instancehandle, &instancehandlelen, 0xFFFF, err);
    if (ILU_ERRNOK(*err)) goto errout2;
#endif
    _w3ng_InputSequence (call, &cinfo_count, 0, NIL, err);
    cinfo = ilu_CharBufFromString("", err);
    if (ILU_ERRNOK(*err)) goto errout3;
    for (i = 0;  i < cinfo_count;  i += 1) {
      _w3ng_InputBytes (call, &tempb, &temp, 0xFFFF, err);
      if (ILU_ERRNOK(*err)) goto errout4;
      if (i > 0) {
	ilu_CharBufAppend(&cinfo, ";", 1, err);
	if (ILU_ERRNOK(*err)) goto errout4;
      }
      ilu_CharBufAppend (&cinfo, (ilu_string) tempb, temp, err);
      if (ILU_ERRNOK(*err)) goto errout4;
      ilu_free(tempb);
      tempb = NIL;
    }
    ilu_EndSequence (call, err);
    if (ILU_ERRNOK(*err)) goto errout4;
#ifdef ILU_W3NG_ALLOW_ILU_URLS
    if ((sidlen == 0) && (instancehandlelen == 0) && (static_type->cl_optional))
      {
	kobj = NIL;
      } else
#endif /* def ILU_W3NG_ALLOW_ILU_URLS */
	{
	  ilu_Class found_class;
	  if ((sidlen > 0) && (instance_data->incoming_last_sid_len > 0)) {
	    /* new one to cache */
	    ilu_free(instance_data->incoming_last_sid);
	    instance_data->incoming_last_sid_len = 0;
	    instance_data->incoming_last_sid = NIL;
	  };
	  if ((sidlen == 0) && (instance_data->incoming_last_sid_len > 0)) {
	    /* cached SID */
	    ilu_free(sid);
	    sid = ilu_MallocE(instance_data->incoming_last_sid_len + 1, err);
	    if (ILU_ERRNOK(*err)) goto errout4;
	    memcpy((void *) sid, (void *) instance_data->incoming_last_sid,
		   instance_data->incoming_last_sid_len);
	    sidlen = instance_data->incoming_last_sid_len;
	    sid[sidlen] = 0;
	    if (cinfo.icb_len == 0) {
	      /* use cached cinfo, too */
	      ilu_CharBufFree(cinfo);
	      cinfo = instance_data->incoming_last_cinfo_string;
	    } else {
	      /* or save new cached value for this sid */
	      if (instance_data->incoming_last_cinfo_string.icb_len > 0)
		ilu_CharBufFree(instance_data->incoming_last_cinfo_string);
	      instance_data->incoming_last_cinfo_string = cinfo;
	    }
	    cinfo_precious = ilu_TRUE;
	  } else if ((sidlen > 0) && (instance_data->incoming_last_sid_len == 0)) {
	    instance_data->incoming_last_sid = ilu_MallocE(sidlen + 1, err);
	    if (ILU_ERRNOK(*err)) goto errout4;
	    memcpy((void *) instance_data->incoming_last_sid, (void *) sid, sidlen);
	    instance_data->incoming_last_sid_len = sidlen;
	    instance_data->incoming_last_sid[instance_data->incoming_last_sid_len] = 0;
	    if (instance_data->incoming_last_cinfo_string.icb_len > 0)
	      ilu_CharBufFree(instance_data->incoming_last_cinfo_string);
	    instance_data->incoming_last_cinfo_string = cinfo;
	    cinfo_precious = ilu_TRUE;
	  }
	  found_class = ilu_FindClassFromID ((ilu_string) mstid);
	  server = _ilu_FindAndEnterServer ((ilu_string) sid, ilu_TRUE, cinfo.icb_base, cinfo.icb_len,
					    static_type, err);
	  if (ILU_ERRNOK(*err)) goto errout4;
	  /* Now Inside(server,static_type) */
	  kobj = _ilu_FindOrCreateObject((ilu_string) instancehandle, server, found_class,
					 static_type, (ilu_string) mstid, NIL, err);
	  if (ILU_ERRNOK(*err)) { if (kobj == NIL) goto errout4; else goto errout5; };
	}
    ilu_free(cbs);
    ilu_free(sid);
    ilu_free(instancehandle);
    if (!cinfo_precious)
      ilu_CharBufFree(cinfo);
  }
  return kobj;

 errout5:
  ilu_ExitServer(server, static_type);
 errout4:
  ilu_free(tempb);
  ilu_CharBufFree(cinfo);
 errout3:
  ilu_free(instancehandle);
 errout2:
  ilu_free(sid);
 errout1:
  ilu_free(cbs);
  return NIL;
}

static ilu_boolean
  _w3ng_OutputObjectID (ilu_Call call, ilu_Object h,
			ilu_boolean discriminator_p,
			ilu_Class static_type,
			ILU_ERRS((IoErrs)) * err)
{
  if (!_w3ng_BeginOutputObject (call, h, discriminator_p, static_type, 0, err)) {
    ilu_ExitServer(object_server(h), static_type);
    return ilu_FALSE;
  } else
    _w3ng_EndOutputObject (call, h, discriminator_p, static_type, err);
  return ILU_ERROK(*err);
}

static ilu_boolean 
  _w3ng_InputObjectID(ilu_Call call, ilu_Object * h,
		      ilu_boolean discriminator_p,
		      ilu_Class static_type,
		      ILU_ERRS((IoErrs)) * err)
{
  ilu_Object kobj;
  ilu_Class cl;
  ilu_cardinal nstates;

  cl = _w3ng_BeginInputObject (call, discriminator_p, static_type, &nstates, err);
  if (ILU_ERRNOK(*err)) {
    return ilu_FALSE;
  }
  kobj = _w3ng_EndInputObject(call, discriminator_p, static_type, err);
  if (ILU_ERROK(*err)) {
    *h = kobj;
    return ilu_TRUE;
  } else {
    return ilu_FALSE;
  }
}

/*======================================================================
**======================================================================
**====================  Non-I/O code ===================================
**======================================================================
**====================================================================*/

/*L1, L2 unconstrained*/

static ilu_refany 
  _w3ng_CreateDataBlock(ilu_ProtocolInfo pinfo,
			ILU_ERRS((no_memory)) * err)
{
  W3ng  *new;

  if ((strncmp(pinfo, "w3ng", 4) != 0) ||
      ((strlen(pinfo) > 4) && (strncmp(pinfo, "w3ng_1", 6) != 0))) {
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_pinfo, NIL);
  };
  new = (W3ng *) ilu_MallocE(sizeof(*new), err);
  if (ILU_ERRNOK(*err))
    return NIL;
  new->major_version = 1;
  new->serial_number = 1;
  new->current_op_cache_val = 0;
  new->current_object_cache_val = 0;
  new->op_cache = NIL;
  new->object_cache = NIL;
  new->connection_initialized = ilu_FALSE;
  new->connection_closed = ilu_FALSE;
  new->transport_stack = NIL;
  new->server_relocate_tried = ilu_FALSE;
  new->outgoing_default_charset = 0;
  new->incoming_default_charset = 0;
  new->outgoing_last_sid = NIL;
  new->outgoing_last_sid_len = 0;
  new->incoming_last_sid = NIL;
  new->incoming_last_sid_len = 0;
  new->outgoing_last_cinfo = NIL;
  new->outgoing_last_cinfo_count = 0;
  new->incoming_last_cinfo_string.icb_len = 0;
  new->incoming_last_cinfo_string.icb_base = NIL;
#ifdef ILU_W3NG_RELATIVE_IH_PASSING
  memset(new->incoming_ih_prefix, 0, sizeof(new->incoming_ih_prefix));
  memset(new->outgoing_ih_prefix, 0, sizeof(new->outgoing_ih_prefix));
#endif /* def ILU_W3NG_RELATIVE_IH_PASSING */
  ILU_NOTE(W3NG_DEBUG,
	   ("_w3ng_CreateDataBlock:  created %p\n", new));
  return ((ilu_refany) new);
}

static void
  _w3ng_FreeDataBlock(ilu_refany d)
{
  W3ng	*	instance_data = (W3ng *) d;
  ILU_NOTE(W3NG_DEBUG,
	   ("_w3ng_FreeDataBlock:  freeing %p\n", d));
  if (instance_data->op_cache != NIL) {
    ilu_hash_FreeHashTable(instance_data->op_cache, NULLFN, _w3ng_FreeOperationData);
  };
  if (instance_data->object_cache != NIL) {
    ilu_hash_FreeHashTable(instance_data->object_cache, NULLFN, _w3ng_FreeObjectData);
  };
  ilu_free(d);
}

/*L2 unconstrained*/
/*L1_sup < prmu*/
static ilu_string _w3ng_FormProtocolHandle (ilu_refany idata, ilu_Object obj)
{
  W3ng *instance_data = (W3ng *) idata;
  char buf[100];

  sprintf (buf, "w3ng_%lu", (long unsigned int) (instance_data->major_version));
  return (_ilu_Strdup(buf));
}
     
static void
  InitializeCallState (W3ngCallData *data)
{
  data->minor_version = 0;
  data->context_present = ilu_FALSE;
  data->reply_status = 0;

  data->disc_cached = ilu_FALSE;
  data->cache_disc = ilu_FALSE;
  data->disc_cache_index = 0;
  data->disc_len = 0;

  data->op_cached = ilu_FALSE;
  data->cache_op = ilu_FALSE;
  data->op_cache_index = 0;
  data->op_index = 0;

  data->message_begun = ilu_FALSE;
  data->close_transport_on_finish_call = ilu_FALSE;

  data->discriminant = NIL;
  data->mstids = NIL;

  data->transports = NIL;
}

static          ilu_boolean
_w3ng_InitCall(ilu_Call call,
	       ILU_ERRS((IoErrs)) * err)
{
  W3ng *instance_data = w3ng_instance_data(call);
  if (!transport_boundaried(connection_transport(call->ca_connection)))
    return ILU_ERR_CONS1(comm_failure, err, minor, ilu_cfm_tr_non_boundaried, ilu_FALSE);
  w3ng_transport(call) = connection_transport(call->ca_connection);
  instance_data->transport_stack = w3ng_transport(call);
  w3ng_transport_ymu(call) = CONN_WAITMU;
  w3ng_instance_data(call)->incoming = connection_incoming(call_connection(call));
  call->ca_prdata2 = ilu_MallocE(sizeof(W3ngCallData), err);
  if (ILU_ERRNOK(*err)) return ilu_FALSE;
  InitializeCallState(w3ng_call_data(call));
  return ILU_CLER(*err);
}

/*Main Invariant holds; L2 disjoint {call's conn}*/
static          ilu_boolean
_w3ng_FinishCall(ilu_Call call, ILU_ERRS((IoErrs)) * err)
{
  W3ngCallData   *call_data = w3ng_call_data(call);

  ILU_CLER(*err);
  if (!ilu_Check(call_transport(call), err))
    return ilu_FALSE;
  if (call_data->close_transport_on_finish_call) {
    ilu_integer     cdfd;
    if (ilu_CloseTransport(call_transport(call), &cdfd, err)) {
      /*
       * XXX We'd also like to check upon failure, but have no place
       * to report failure of the check.
       */
      if (!ilu_Check(cdfd == 0, err))
	return ilu_FALSE;
    } else {
      ILU_ERR_SWITCH(*err) {
	ILU_ERR_CASE(bad_locks, x) {
	  ILU_HANDLED(*err);
	  (void) ilu_Check(ilu_FALSE, err);
	}
      } ILU_ERR_ENDSWITCH;
    }
  }
  ilu_free(call->ca_prdata2);
  return ILU_ERROK(*err);
}

/*Call-IHi(call)*/

static	void
  _w3ng_SendTerminateSession (ilu_Call call,
			      w3ng_TermCause termcause,
			      ilu_Error *err)
{
  W3ng *	instance_data = w3ng_instance_data(call);
  ilu_cardinal	msgheader;

  ILU_NOTE(W3NG_DEBUG,
	   ("_w3ng_SendTerminateSession:  due to %d\n", termcause));
  if (transport_begin_message(w3ng_transport(call), ilu_FALSE, err)
      != ilu_rhrc_ok)
    return;
  msgheader = (0x80000000) |
    (W3NG_TERMINATE_CONNECTION_MSG << 28) |
      (((unsigned)termcause) << 24) |
	(instance_data->serial_number & 0x00FFFFFF);
  _w3ng_OutputCardinal(call, msgheader, err);
  if (ILU_ERRNOK(*err)) return;
  if (transport_end_message(w3ng_transport(call), ilu_TRUE, NIL, err) != ilu_rhrc_ok)
    return;
  instance_data->connection_closed = ilu_TRUE;
  return;
}

/*L1 >= {cmu, conn's server}; L1.sup < trmu*/
/*L2 >= {conn's iomu}*/
static	void
  _w3ng_CloseConnection (ilu_refany rock,
			 ilu_ConnShutdownReason reason,
			 ilu_Error *err)
{
  W3ng *	instance_data = (W3ng *) rock;
  ilu_cardinal	msgheader;
  w3ng_TermCause termcause;
  ilu_Error lerr;

  ILU_CLER(*err);
  if ((instance_data->connection_closed) ||
      (instance_data->transport_stack == NIL))
    return;
  ILU_NOTE(W3NG_DEBUG,
	   ("_w3ng_CloseConnection:  due to %d\n", reason));
  switch (reason) {
  case ilu_ConnShutdownReason_ProcessTermination:
    termcause = ProcessTermination;
    break;
  case ilu_ConnShutdownReason_ResourceManagement:
    termcause = ResourceManagement;
    break;
  case ilu_ConnShutdownReason_LostProtocolSync:
    termcause = MangledMessage;
    break;
  case ilu_ConnShutdownReason_BadEndpointID:
    termcause = InvalidServerID;
    break;
  case ilu_ConnShutdownReason_MaxSerialNumber:
    termcause = MaxSerialNumber;
    break;
  case ilu_ConnShutdownReason_Relocating:
    termcause = ResourceManagement;
    break;
  case ilu_ConnShutdownReason_ReceivedEOF:
    termcause = MangledMessage;
    break;
  default:
    ILU_ERR_CONS1(internal, err, minor, ilu_im_check, err);
    return;
  };
  /* May not be able to send, if reacting to a close from the other end */
  /* Main Invariant holds; L2 >= {xmu}; input => L2 >= {ymu} */
  if (transport_begin_output_nonblock(instance_data->transport_stack,
				      &lerr)) {
    unsigned char   msgbuf[4];
    msgheader = (0x80000000) |
      (W3NG_TERMINATE_CONNECTION_MSG << 28) |
      (((unsigned) termcause) << 24) |
      ((instance_data->serial_number - 1) & 0x00FFFFFF);
    cardout(msgbuf, msgheader);
    (void) transport_write_bytes_nonblock(instance_data->transport_stack,
					  msgbuf, 4, &lerr);
    if (ILU_ERROK(lerr)) {
      (void) transport_end_output_nonblock(instance_data->transport_stack,
				    ilu_TRUE, NIL, &lerr);
    }
  }
  ILU_HANDLED(lerr);
  instance_data->connection_closed = ilu_TRUE;
  return;
}

static ilu_boolean 
_w3ng_DiscardMessage(ilu_Call call, ILU_ERRS((internal)) * err)
{
  ilu_Transport   t = w3ng_transport(call);
  ILU_ERRS((IoErrs)) lerr;

  ILU_NOTE(W3NG_DEBUG,
	   ("_w3ng_DiscardMessage called\n"));

  transport_end_message(t, ilu_FALSE, NIL, &lerr);
  ILU_ERR_SWITCH(lerr) {
    ILU_SUCCESS_CASE
      return ILU_CLER(*err);
    ILU_ERR_CASE(internal, e) {
      if (e->minor == ilu_im_tcBytesDropped) {
	ILU_HANDLED(lerr);
	return ILU_CLER(*err);
      }
      *err = lerr;
      return ilu_FALSE;
    }
  } ILU_ERR_ENDSWITCH;
  return ILU_CLER(*err);
}

static void _w3ng_SendRelocate (ilu_Call call,
				ilu_cardinal serial_number,
				ilu_Error *relocate,
				ilu_Error *err)
{
  ILU_ERRMEMP_T(relocate) reldata = ilu_ErrpQua_relocate(relocate);
  ilu_CharBuf cinfo;
  ilu_cardinal msgheader;

  if (reldata == NIL) {
    ILU_NOTE(W3NG_DEBUG,
	     ("_w3ng_SendRelocate:  called with bad relocate err\n"));
    ILU_ERR_CONS1(internal, err, minor, ilu_im_broken, 0);
    return;
  };
  cinfo = ilu_CharBufFromChars("", 0, err);
  if (ILU_ERRNOK(*err)) return;
  if (!ilu_Append1Cinfo(&cinfo, reldata->rel_pinfo, reldata->rel_tinfo, err))
    return;
  ILU_NOTE(W3NG_DEBUG,
	   ("_w3ng_SendRelocate:  relocating server <%s> to <%*.*s> (per-%s)\n",
	    server_id(call_server(call)), cinfo.icb_len, cinfo.icb_len,
	    cinfo.icb_base, ilu_RelocateScope_Name(reldata->rel_scope)));
  _w3ng_DiscardMessage(call, err);
  if (ILU_ERRNOK(*err)) return;
  if (transport_begin_message(w3ng_transport(call), ilu_FALSE, err)
      != ilu_rhrc_ok)
    return;
  msgheader = 
    (SystemExceptionBefore << 28) |
      (serial_number & 0x00FFFFFF);
  _w3ng_OutputCardinal(call, msgheader, err);
  if (ILU_ERRNOK(*err)) return;
  _w3ng_OutputCardinal(call, W3NG_SYSEXN_SwitchSessionCinfo, err);
  if (ILU_ERRNOK(*err)) return;
  _w3ng_OutputString(call, (ilu_bytes) cinfo.icb_base, cinfo.icb_len, 0xFFFF,
		     ILU_StringEncoding_latin1, ILU_StringEncoding_latin1, err);
  if (ILU_ERRNOK(*err)) return;
  transport_end_message(w3ng_transport(call), ilu_TRUE, NIL, err);
}

static          ilu_ReadHeaderResultCode
_w3ng_ReadHeader(ilu_Call call, ilu_PacketType * type,
		 ilu_cardinal * sn,
		 ilu_ConnShutdownReason *reason, ilu_cardinal *lastSN,
		 ILU_ERRS((IoErrs)) * err)
{
  ilu_Transport   bs = w3ng_transport(call);
  W3ng *	  id = w3ng_instance_data(call);
  W3ngCallData *  cd = w3ng_call_data(call);
  ilu_cardinal    packet;
  ilu_ReadHeaderResultCode ans;

  ans = transport_begin_message(bs, ilu_TRUE, err);
  switch (ans) {
  case ilu_rhrc_ok:
    break;
  case ilu_rhrc_error:
    ILU_NOTE((INCOMING_DEBUG | W3NG_DEBUG),
     ("%s:  error %s on transport_begin_message (input)\n",
      "_w3ng_ReadHeader", ILU_ERR_NAME(*err)));
  case ilu_rhrc_eof:
    *reason = ilu_ConnShutdownReason_ProcessTermination;
    *lastSN = 0;
  case ilu_rhrc_nothing:
    return (ans);
  default:
    _ilu_Assert(ilu_FALSE, "w3ng.c:ReadHeader");
  }

  if (_w3ng_InputCardinal(call, &packet, err), ILU_ERRNOK(*err)) {
    ILU_NOTE((INCOMING_DEBUG | W3NG_DEBUG),
	     ("%s:  error %s reading message header\n",
	      "_w3ng_ReadHeader", ILU_ERR_NAME(*err)));
    return (ilu_rhrc_error);
  }

  if (W3NG_CONTROL_MSG_P(packet)) {
    /* Control message */
    switch (W3NG_CONTROL_MSG_ID(packet))
      {
      case W3NG_INITIALIZE_CONNECTION_MSG:
	{
	  ilu_cardinal sid_len;
	  ilu_bytes sid = NIL;
	  sid_len = packet & 0x0000FFFF;
	  _w3ng_InputOpaque(call, &sid, sid_len, err);
#ifdef ENABLE_DEBUGGING
	  if (ILU_ERRNOK(*err))
	    ILU_NOTE(W3NG_DEBUG,
		     ("_w3ng_ReadHeader:  InitializeConnection:  error <%s> reading SID of %d bytes on conn <%s>\n",
		      ILU_ERR_NAME(*err), sid_len, conn_peerinfo(call_connection(call))));
#endif
	  if (ILU_ERRNOK(*err)) return ilu_rhrc_error;
	  if (!transport_end_message(bs, ilu_FALSE, NIL, err))
	    return ilu_rhrc_error;
	  cd->message_begun = ilu_FALSE;
	  if (memcmp((void *) sid, (void *) call_server_id(call), sid_len) != 0) {
	    ilu_Error       lerr;
	    _w3ng_SendTerminateSession(call, InvalidServerID, &lerr);
	    ILU_HANDLED(lerr);
	    ILU_NOTE(W3NG_DEBUG,
		     ("_w3ng_ReadHeader:  InitializeConnection:  read server prefix of <%*.*s> for server <%s>!\n",
		      sid_len, sid_len, (ilu_string) sid, call_server_id(call)));
	    ilu_free(sid);
	    ILU_ERR_CONS1(comm_failure, err, minor, ilu_cfm_conn_lost, err);
	    return ilu_rhrc_error;
	  };
	  if (((packet & 0x00F00000) >> 20) != id->major_version) {
	    ilu_Error lerr;
	    _w3ng_SendTerminateSession(call, InvalidProtocolID, &lerr);
	    ILU_HANDLED(lerr);
	    ILU_NOTE(W3NG_DEBUG,
		     ("_w3ng_ReadHeader:  InitializeConnection:  read major protocol version of %d!\n",
		      (packet & 0x00FF0000) >> 20));
	    ilu_free(sid);
	    ILU_ERR_CONS1(comm_failure, err, minor, ilu_cfm_conn_lost, err);
	    return ilu_rhrc_error;
	  }
	  ILU_NOTE(W3NG_DEBUG,
		   ("_w3ng_ReadHeader:  read InitializeConnection of <%*.*s>, version=%d\n",
		    sid_len, sid_len, (ilu_string) sid, id->major_version));
	  ilu_free(sid);
	  return (ilu_rhrc_handled);
	}
      case W3NG_TERMINATE_CONNECTION_MSG:
	{
	  w3ng_TermCause cause = (packet & 0x0F000000) >> 24;
	  ilu_cardinal serial_no = (packet & 0x00FFFFFF);
	  ILU_NOTE(W3NG_DEBUG,
		   ("_w3ng_ReadHeader:  read TerminateConnection, reason=%d, serial_no=%lu\n",
		    cause, serial_no));		   
	  if (!transport_end_message(bs, ilu_FALSE, NIL, err))
	    return ilu_rhrc_error;
	  ILU_CLER(*err);
	  *lastSN = serial_no;
	  switch (cause) {
	  case ResourceManagement:
	    *reason = ilu_ConnShutdownReason_ResourceManagement;
	    break;
	  case InvalidServerID:
	    *reason = ilu_ConnShutdownReason_BadEndpointID;
	    break;
	  case ProcessTermination:
	    *reason = ilu_ConnShutdownReason_ProcessTermination;
	    break;
	  case InvalidProtocolID:
	  case MangledMessage:
	    *reason = ilu_ConnShutdownReason_LostProtocolSync;
	    break;
	  case MaxSerialNumber:
	    *reason = ilu_ConnShutdownReason_MaxSerialNumber;
	    break;
	  default:
	    ILU_ERR_CONS1(comm_failure, err, minor, ilu_cfm_protocol_sync_lost, 0);
	    break;
	  }
	  id->connection_closed = ilu_TRUE;
	  return (ILU_ERROK(*err) ? ilu_rhrc_eof : ilu_rhrc_error);
	}
      case W3NG_DEFAULT_CHARSET_MSG:
	{
	  ilu_cardinal charset = (packet & 0x0000FFFF);
	  ILU_NOTE(W3NG_DEBUG,
		   ("_w3ng_ReadHeader:  read DefaultCharset, mibenum=%lu\n", charset));
	  if (!transport_end_message(bs, ilu_FALSE, NIL, err))
	    return ilu_rhrc_error;
	  ILU_CLER(*err);
	  id->incoming_default_charset = charset;
	  return ilu_rhrc_handled;
	}
      default:
	/* bad message type */
	ILU_NOTE(W3NG_DEBUG,
		 ("_w3ng_ReadHeader:  bad message header %08x\n", packet));
	return ILU_ERR_CONS1(comm_failure, err, minor, ilu_cfm_protocol_sync_lost, ilu_rhrc_error);
      }
  } else {
    if (connection_incoming(call_connection(call))) {
      /* request */
      *type = ilu_PacketType_Request;
      *sn = id->serial_number++;
      cd->context_present = W3NG_EXTENSION_HEADERS_P(packet);
      if (server_relocatable_p(call_server(call)) && (!id->server_relocate_tried)) {
	ilu_Error lerr;
	id->server_relocate_tried = ilu_TRUE;
	ilu_EnterServerMutex(call_server(call), ilu_TRUE, err);
	if (ILU_ERRNOK(*err)) return ilu_rhrc_error;
	(*server_relocate_proc(call_server(call)))
	  (call_server(call), server_relocate_rock(call_server(call)), &lerr);
	ilu_ExitServerMutex(call_server(call), ilu_TRUE, err);
	if (ILU_ERRNOK(*err)) return ilu_rhrc_error;
	ILU_ERR_SWITCH(lerr) {
	  ILU_ERR_CASE(relocate,v) {
	    _w3ng_SendRelocate(call, *sn, &lerr, err);
	    ILU_HANDLED(lerr);
	    if (ILU_ERROK(*err))
	      return ilu_rhrc_handled;
	  } ILU_ERR_ELSE {
	    *err = lerr;
	  }
	} ILU_ERR_ENDSWITCH;
	if (ILU_ERRNOK(*err)) return ilu_rhrc_error;
      };
      cd->disc_cached = W3NG_REQUEST_DISC_CACHED_P(packet);
      if (cd->disc_cached)
	cd->disc_cache_index = W3NG_REQUEST_DISC_CACHE_IDX(packet);
      else {
	cd->cache_disc = W3NG_REQUEST_CACHE_DISC_P(packet);
	cd->disc_len = W3NG_REQUEST_DISC_LEN(packet);
      }
      cd->op_cached = W3NG_REQUEST_OP_CACHED_P(packet);
      if (cd->op_cached)
	cd->op_cache_index = W3NG_REQUEST_OP_CACHE_IDX(packet);
      else {
	cd->cache_op = W3NG_REQUEST_CACHE_OP_P(packet);
	cd->op_index = W3NG_REQUEST_OP_INDEX(packet);
      }
      ans = ilu_rhrc_ok;
    } else {
      /* reply */
      *type = ilu_PacketType_Reply;
      *sn = W3NG_REPLY_SERIAL_NUMBER(packet);
      cd->context_present = W3NG_EXTENSION_HEADERS_P(packet);
      cd->reply_status = W3NG_REPLY_STATUS(packet);
      ans = ilu_rhrc_ok;
    }
  }

  ILU_NOTE((INCOMING_DEBUG | W3NG_DEBUG),
	   ("%s %s message, SN %lu.\n",
	    "_w3ng_ReadHeader:  reading",
	    ((*type == ilu_PacketType_Request) ? "request"
	     : (*type == ilu_PacketType_Reply) ? "reply"
	     : "unknown type"), *sn));

  return (ans);
}

typedef struct {
  ilu_Transport the_message;
  W3ngCallData  the_state;
} DelayedW3ngReply;  

static ilu_refany 
_w3ng_DelayInterp(ilu_Call call,
		  ILU_ERRS((IoErrs)) * err)
{
  DelayedW3ngReply *temp;

  temp = ilu_MallocE(sizeof(*temp), err);
  if (ILU_ERRNOK(*err)) return NIL;
  temp->the_message = _ilu_BufferInputMessage(w3ng_transport(call), 0, ilu_FALSE, err);
  temp->the_state = *w3ng_call_data(call);
  InitializeCallState(w3ng_call_data(call));
  return (ilu_refany) temp;
}

static void _w3ng_ResumeInterp(ilu_Call call, ilu_refany x)
{
  DelayedW3ngReply *tmp = (DelayedW3ngReply *) x;

  call_transport(call) = tmp->the_message;
  *w3ng_call_data(call) = tmp->the_state;
  w3ng_call_data(call)->close_transport_on_finish_call = ilu_TRUE;
  ilu_free(tmp);
}

/*L1 >= {cmu}; L2 >= {conn's iomu, waitmu}*/
static          ilu_boolean
_w3ng_AbandonDelayedInterp(ilu_Connection conn,
			     ilu_refany x,
			     ILU_ERRS((internal)) * err)
{
  ilu_boolean     ans;
  ilu_integer     cdfd;
  DelayedW3ngReply *tmp = (DelayedW3ngReply *) x;

  ans = transport_close(tmp->the_message, &cdfd, err);
  ILU_ERR_SWITCH(*err) {
    ILU_SUCCESS_CASE
      /* do nothing */;
    ILU_ERR_CASE2(bad_locks, broken_locks) {
      ILU_HANDLED(*err);
      (void) ilu_Check(ilu_FALSE, err);
    }
    ILU_ERR_CASE(internal, x)
      /* do nothing */;
  } ILU_ERR_ENDSWITCH;
  _ilu_Assert(cdfd == 0, "w3ng AbandonDelayedInterp");
  ilu_free(tmp);
  return ans;
}

static void
  _w3ng_RequestRead (ilu_Call call, ILU_ERRS((IoErrs)) *err)
{
  transport_end_message(w3ng_transport(call), ilu_FALSE, NIL, err);
  w3ng_call_data(call)->message_begun = ilu_FALSE;
}

static void
  _w3ng_ReplyRead (ilu_Call call, ILU_ERRS((IoErrs)) *err)
{
  transport_end_message(w3ng_transport(call), ilu_FALSE, NIL, err);
  w3ng_call_data(call)->message_begun = ilu_FALSE;
}

static ilu_boolean
  EnsureOperationCache (W3ng *instance_data,
			ilu_Error *err)
{
  if (instance_data->op_cache == NIL) {
    if (instance_data->incoming)
      instance_data->op_cache = ilu_hash_MakeNewTable(OPERATION_CACHE_BUCKETS,
						      (ilu_cardinal (*)(ilu_refany, ilu_cardinal)) _w3ng_HashIndex,
						      (ilu_boolean (*)(ilu_refany, ilu_refany)) _w3ng_CompareIndex);
    else
      instance_data->op_cache = ilu_hash_MakeNewTable(OPERATION_CACHE_BUCKETS,
						      (ilu_cardinal (*)(ilu_refany, ilu_cardinal)) _w3ng_HashOperation,
						      (ilu_boolean (*)(ilu_refany, ilu_refany)) _w3ng_CompareOperation);
    if (instance_data->op_cache == NIL)
      return ILU_ERR_CONS1(internal, err, minor, ilu_im_broken, ilu_FALSE);
  };
  ILU_CLER(*err);
  return ilu_TRUE;
}

static ilu_boolean
  CacheOperation (W3ng *instance_data,
		  ilu_Class c,
		  ilu_Method m,
		  ilu_cardinal *index,
		  ilu_Error *err)
{
  CachedOperation *op;

  if (!EnsureOperationCache(instance_data, err))
    return ilu_FALSE;
  if ((instance_data->current_op_cache_val + 1) >= MAX_OP_CACHE_INDEX)
    return ILU_ERR_CONS1(imp_limit, err, minor, ilu_ilm_strlen, ilu_FALSE);
  op = ilu_MallocE(sizeof(CachedOperation), err);
  if (ILU_ERRNOK(*err)) return ilu_FALSE;
  op->cache_index = instance_data->current_op_cache_val++;
  op->cached_class = c;
  op->cached_method = m;
  if ((instance_data->incoming &&
       (!ilu_hash_AddToTable(instance_data->op_cache, (ilu_refany) op->cache_index, op))) ||
      (!instance_data->incoming &&
       (!ilu_hash_AddToTable(instance_data->op_cache, (ilu_refany) op, op))))
    return ILU_ERR_CONS1(internal, err, minor,ilu_im_callFail, ilu_FALSE);
  *index = op->cache_index;
  return ilu_TRUE;
}

static ilu_boolean
  FindCachedOperationFromIndex (W3ng *instance_data,
				ilu_cardinal index,
				ilu_Class *class_out,
				ilu_Method *method_out,
				ilu_Error *err)
{
  CachedOperation *op;

  if (!EnsureOperationCache(instance_data, err))
    return ilu_FALSE;
  if (instance_data->incoming) {
    if ((op = ilu_hash_FindInTable(instance_data->op_cache, (ilu_refany) index)) == NIL)
      return ILU_ERR_CONS1(internal, err, minor, ilu_im_callFail, ilu_FALSE);
    *class_out = op->cached_class;
    *method_out = op->cached_method;
    ILU_CLER(*err);
  } else {
    ILU_ERR_CONS1(internal, err, minor, ilu_im_broken, 0);
  }
  return ILU_ERROK(*err);
}

static ilu_boolean
  FindCachedOperationFromOpData (W3ng *instance_data,
				 ilu_Class pclass,
				 ilu_Method pmethod,
				 ilu_cardinal *index,
				 ilu_Error *err)
{
  CachedOperation *op;
  ilu_boolean ans;

  if (!EnsureOperationCache(instance_data, err))
    return ilu_FALSE;
  if (!instance_data->incoming) {
    CachedOperation temp;
    temp.cached_class = pclass;
    temp.cached_method = pmethod;
    temp.cache_index = 0;
    if ((op = ilu_hash_FindInTable(instance_data->op_cache, &temp)) == NIL)
      ans = ilu_FALSE;
    else {
      *index = op->cache_index;
      ans = ILU_CLER(*err);
    };
  } else {
    ILU_ERR_CONS1(internal, err, minor, ilu_im_broken, 0);
    ans = ilu_FALSE;
  }
  return ans;
}

static ilu_boolean
  FigureMethod (ilu_Call call,
		ilu_boolean cached_operation_id,
		ilu_boolean cache_operation_id,
		ilu_shortcardinal methodID,
		ilu_Transport bs,
		ilu_Error *err)
{
  W3ng *	instance_data = w3ng_instance_data(call);
  ilu_string classID = NIL;
  ilu_cardinal oplen, val, junk;
  ilu_Method m;
  ilu_Class c;

  if (!cached_operation_id) {
    _w3ng_InputString(call, (void **) &classID, &oplen, 0xFFFF, ILU_StringEncoding_latin1, &junk, err);
    if (ILU_ERRNOK(*err)) goto err1;
    /* OK, now parse it */
    if ((c = ilu_FindClassFromID (classID)) == NIL) {
      ILU_ERR_CONS1(marshal, err, minor, ilu_mm_badTypeName, ilu_FALSE);
      goto err1;
    };
    if (methodID >= class_method_count(c)) {
      ILU_ERR_CONS1(bad_operation, err, minor, ilu_bom_noSuchOperationOnType, ilu_FALSE);
      goto err1;
    } else {
      m = class_methods(c) + methodID;
    }
    ilu_free(classID);
    if (cache_operation_id) {
      if (!CacheOperation (instance_data, c, m, &val, err))
	return ilu_FALSE;
    }
  } else {
    if (!FindCachedOperationFromIndex(instance_data, methodID, &c, &m, err))
      return ilu_FALSE;
  }
  call_intro_type(call) = c;
  call_method(call) = m;
  return ilu_TRUE;

 err1:
  if (!cached_operation_id) ilu_free(classID);
  return ilu_FALSE;
}

static ilu_boolean 
  _w3ng_InterpretRequest(ilu_Call call,
			 ILU_ERRS((IoErrs)) *err)
{
  ilu_Transport	bs = w3ng_transport(call);
  W3ngCallData	*d = w3ng_call_data(call);

  if (!FigureMethod(call, d->op_cached, d->cache_op,
		    (ilu_shortcardinal)(d->op_cached ? d->op_cache_index : d->op_index),
		    bs, err))
    return ilu_FALSE;
  ILU_NOTE((INCOMING_DEBUG | W3NG_DEBUG),
	("_w3ng_InterpretRequest:  (call SN %lu, disc_cached=%s, cache_disc=%s, disc_cache_index=%lu) "
	 "intro_type is %s:%s, method is %s\n",
	 call_serial_number(call), d->disc_cached ? "ilu_TRUE" : "ilu_FALSE",
	 d->cache_disc ? "ilu_TRUE" : "ilu_FALSE",
	 (unsigned long) (d->disc_cached ? d->disc_cache_index : d->disc_len),
	 class_name(call_intro_type(call)),
	 class_unique_id(call_intro_type(call)),
	 method_name(call_method(call))));
  if (d->context_present) {
    ilu_cardinal ncontexts, i, context_name_len, junk;
    ilu_string context_name;
    ilu_Pickle context_value;
    _w3ng_InputCardinal (call, &ncontexts, err);
    if (ILU_ERRNOK(*err)) return ilu_FALSE;
    for (i = 0;  i < ncontexts;  i++) {
      context_name = NIL;
      _w3ng_InputString (call, (void **) &context_name, &context_name_len,
			 0xFFFF, ILU_StringEncoding_latin1, &junk, err);
      if (ILU_ERRNOK(*err)) return ilu_FALSE;
      _ilu_InputPickle (call, &context_value, NIL, err);
      if (ILU_ERRNOK(*err)) return ilu_FALSE;
      ILU_NOTE(W3NG_DEBUG,
	       ("_w3ng_InterpretRequest:  discarding <*.*%s> context of %lu bytes\n",
		context_name_len, context_name_len, context_name, context_value.pi_len));
      ilu_free(context_name);
      ilu_FreePickle(&context_value, err);
      if (ILU_ERRNOK(*err)) return ilu_FALSE;
      d->context_present = ilu_FALSE;
    };
  };
  return (ilu_TRUE);
}

static ilu_ProtocolException
  MapWireToProtocolExtension (ilu_cardinal exception)
{
  switch (exception) {
  case W3NG_SYSEXN_UnknownProblem:
    return ilu_ProtocolException_Unknown;
  case W3NG_SYSEXN_ImplementationLimit:
    return ilu_ProtocolException_Unknown;
  case W3NG_SYSEXN_SwitchSessionCinfo:
    return ilu_ProtocolException_LostConnection;
  case W3NG_SYSEXN_Marshal:
    return ilu_ProtocolException_GarbageArguments;
  case W3NG_SYSEXN_NoSuchObjectType:
    return ilu_ProtocolException_NoSuchClassAtServer;
  case W3NG_SYSEXN_NoSuchMethod:
    return ilu_ProtocolException_NoSuchMethodOnClass;
  case W3NG_SYSEXN_Rejected:
    return ilu_ProtocolException_RequestRejected;
  default:
    return ilu_ProtocolException_Unknown;
  }
}

static void
  HandleRelocateMessage (ilu_Call call,
			 ilu_Error *err)
{
  ilu_string new_cinfo = NIL;
  ilu_cardinal new_cinfo_len = 0, junk;
  ilu_ProtocolInfo new_pinfo = NIL;
  ilu_TransportInfo new_tinfo = NIL;

  _w3ng_InputString (call, (void **) &new_cinfo, &new_cinfo_len, 0xFFFF, ILU_StringEncoding_latin1, &junk, err);
  if (ILU_ERRNOK(*err)) return;
  if (_ilu_ParseConnectInfo(new_cinfo, new_cinfo_len, &new_pinfo, &new_tinfo, err)) {
    ILU_ERR_CONS3(relocate,err,rel_scope,ilu_relocate_conn,rel_pinfo,new_pinfo,rel_tinfo,new_tinfo,0);
  } else {
    ILU_ERR_CONS1(comm_failure, err, minor, ilu_cfm_conn_lost, 0);
  }
  ilu_free(new_cinfo);
}

static          ilu_ProtocolException
  HandleSystemException (ilu_Call call,
			 w3ng_ReplyStatus status,
			 ilu_cardinal val,
			 ilu_Error *err)
{
  if (val == W3NG_SYSEXN_SwitchSessionCinfo) {
    HandleRelocateMessage(call, err);
    return ilu_ProtocolException_Not;
  } else if (val == W3NG_SYSEXN_Rejected) {
    ilu_boolean reason_present;
    ilu_string reason;
    ilu_cardinal reason_len, junk;
    _w3ng_InputOptional (call, &reason_present, NIL, err);
    if (ILU_ERRNOK(*err)) return ilu_ProtocolException_Not;
    if (reason_present) {
      _w3ng_InputString (call, (void **) &reason, &reason_len, 0xFFFF, ILU_StringEncoding_latin1, &junk, err);
      if (ILU_ERRNOK(*err)) return ilu_ProtocolException_Not;
      ILU_NOTE(W3NG_DEBUG,
	       ("_w3ng_InterpretReply:  Request %lu rejected (%s.%s), reason \"%s\"\n",
		call_serial_number(call), class_name(call_intro_type(call)),
		method_name(call_method(call)), reason));
      ilu_free(reason);
    }
    return ilu_ProtocolException_RequestRejected;
  } else {
    ILU_CLER(*err);
    return MapWireToProtocolExtension(val);
  }
}

static          ilu_ProtocolException
  _w3ng_InterpretReply(ilu_Call call, ilu_cardinal * estatus,
		       ILU_ERRS((IoErrs)) * err)
{
  W3ngCallData	*d = w3ng_call_data(call);
  ilu_cardinal temp;

#ifdef ENABLE_DEBUGGING
  if (d->reply_status == Success) {
    ILU_NOTE((INCOMING_DEBUG | W3NG_DEBUG),
	     ("_w3ng_InterpretReply:  replyStatus is Success\n",
	      d->reply_status));
  } else {
    ILU_NOTE((INCOMING_DEBUG | W3NG_DEBUG),
	     ("_w3ng_InterpretReply:  replyStatus is %u\n",
	      d->reply_status));
  };
#endif	     

  if (d->context_present) {
    ilu_cardinal ncontexts, i, context_name_len, junk;
    ilu_string context_name;
    ilu_Pickle context_value;
    _w3ng_InputCardinal (call, &ncontexts, err);
    if (ILU_ERRNOK(*err)) return ilu_ProtocolException_Not;
    for (i = 0;  i < ncontexts;  i++) {
      context_name = NIL;
      _w3ng_InputString (call, (void **) &context_name, &context_name_len,
			 0xFFFF, ILU_StringEncoding_latin1, &junk, err);
      if (ILU_ERRNOK(*err)) return ilu_ProtocolException_Not;
      _ilu_InputPickle (call, &context_value, NIL, err);
      if (ILU_ERRNOK(*err)) return ilu_ProtocolException_Not;
      ILU_NOTE(W3NG_DEBUG,
	       ("_w3ng_InterpretReply:  discarding <*.*%s> context of %lu bytes\n",
		context_name_len, context_name_len, context_name, context_value.pi_len));
      ilu_free(context_name);
      ilu_FreePickle(&context_value, err);
      if (ILU_ERRNOK(*err)) return ilu_ProtocolException_Not;
    };
    d->context_present = ilu_FALSE;
  };

  switch (d->reply_status) {

  case Success:
    *estatus = 0;
    return ilu_ProtocolException_Success;

  case UserException:
    _w3ng_InputCardinal (call, estatus, err);
    return (ILU_ERROK(*err) ? ilu_ProtocolException_Success : ilu_ProtocolException_Not);

  case SystemExceptionBefore:
  case SystemExceptionAfter:
    _w3ng_InputCardinal (call, &temp, err);
    return HandleSystemException (call, d->reply_status, temp, err);

  default:
    ILU_NOTE(W3NG_DEBUG,
	     ("_w3ng_InterpretReply:  bad reply status in call data!\n"));
    return ILU_ERR_CONS1(marshal, err, minor, ilu_mm_msgTypeUnknown, ilu_ProtocolException_Not);
  }
}

static		ilu_boolean
  _w3ng_SendInitializeConnection (ilu_Call call,
				  ilu_Error *err)
{
  W3ng *	instance_data = w3ng_instance_data(call);
  W3ngCallData *call_data = w3ng_call_data(call);
  ilu_string	sid = call_server_id(call);
  ilu_cardinal	len = strlen(call_server_id(call));
  ilu_cardinal	msgheader;

  if (instance_data->connection_initialized)
    return ILU_CLER(*err);
  ILU_NOTE(W3NG_DEBUG,
	   ("_w3ng_SendInitializeConnection:  server <%s>, protocol version %d\n",
	    sid, instance_data->major_version));

  /* First, do InitializeConnection */

  if (transport_begin_message(w3ng_transport(call), ilu_FALSE, err)
      != ilu_rhrc_ok)
    return ilu_FALSE;
  msgheader = (0x80000000 | (W3NG_INITIALIZE_CONNECTION_MSG << 28) |
	       (((instance_data->major_version << 4) | (call_data->minor_version & 0x0F)) << 16) |
	       (len & 0xFFFF));
  _w3ng_OutputCardinal(call, msgheader, err);
  if (ILU_ERRNOK(*err)) return ilu_FALSE;
  _w3ng_OutputOpaque(call, (ilu_bytes) sid, len, err);
  if (ILU_ERRNOK(*err)) return ilu_FALSE;
  if (!transport_end_message(w3ng_transport(call), ilu_FALSE, NIL, err))
    return ilu_FALSE;

  /* Next, send Latin-1 as our default charset */

  if (transport_begin_message(w3ng_transport(call), ilu_FALSE, err)
      != ilu_rhrc_ok)
    return ilu_FALSE;
  msgheader = (0x80000000 | (W3NG_DEFAULT_CHARSET_MSG << 28) | ILU_StringEncoding_latin1);
  _w3ng_OutputCardinal(call, msgheader, err);
  if (ILU_ERRNOK(*err)) return ilu_FALSE;
  if (!transport_end_message(w3ng_transport(call), ilu_FALSE, NIL, err))
    return ilu_FALSE;
  instance_data->outgoing_default_charset = ILU_StringEncoding_latin1;

  instance_data->connection_initialized = ilu_TRUE;
  return ilu_TRUE;
}

static          ilu_boolean
_w3ng_StartRequest(ilu_Call call, ilu_cardinal argSize,
		   ILU_ERRS((IoErrs)) * err)
{
  W3ng *	instance_data = w3ng_instance_data(call);
  W3ngCallData *call_data = w3ng_call_data(call);
  ilu_Class	pclass = call_intro_type(call);
  ilu_Method	method = call_method(call);
  ilu_cardinal	op_index;
  ilu_boolean	pre_cached, cache_this;

  if (!instance_data->connection_initialized) {
    _w3ng_SendInitializeConnection(call, err);
    if (ILU_ERRNOK(*err))
      return ilu_FALSE;
  };

  ILU_NOTE(W3NG_DEBUG,
	("%s %p (sn %lu), aSize %lu, class %s (%s), meth %s (%lu)\n",
	 "_w3ng_StartRequest:  call", call,
	 call_serial_number(call), argSize, class_name(pclass),
	 class_unique_id(pclass), method_name(method),
	 method_id(method)));

  if (!ilu_Check(call_serial_number(call) == instance_data->serial_number, err)) {
    ILU_NOTE(W3NG_DEBUG,
	     ("_w3ng_StartRequest:  call %p (sn %lu):  internal serial number is %lu!\n",
	     call, call_serial_number(call), instance_data->serial_number));
    return ilu_FALSE;
  };
  instance_data->serial_number++;

  /* figure out whether or not to send cached operation */
  if (!FindCachedOperationFromOpData(instance_data, pclass, method, &op_index, err)) {
    if (ILU_ERRNOK(*err)) return ilu_FALSE;
    pre_cached = ilu_FALSE;
    /* not cached; see if we can cache it */
    if (!CacheOperation(instance_data, pclass, method, &op_index, err)) {
      ILU_ERR_SWITCH(*err) {
	ILU_ERR_CASE(imp_limit,m) {
	  if (m->minor == ilu_ilm_strlen)
	    ILU_HANDLED(*err);
	  else
	    return ilu_FALSE;
	}
	ILU_ERR_ELSE {
	  return ilu_FALSE;
	}
      } ILU_ERR_ENDSWITCH;
      /* couldn't cache it, so send it full */
      cache_this = ilu_FALSE;
    } else {
      /* cached, use cache */
      cache_this = ilu_TRUE;
    }
    /* we overwrite op_index here with ordinal value of method in vector of methods */
    op_index = (method - class_methods(pclass));
  } else {
    /* cached, use cache */
    pre_cached = ilu_TRUE;
    cache_this = ilu_FALSE;
  }

  if (transport_begin_message(w3ng_transport(call), ilu_FALSE, err)
      != ilu_rhrc_ok)
    return ilu_FALSE;

  call_data->message_begun = ilu_TRUE;
  call_data->op_cached = pre_cached;
  if (pre_cached)
    call_data->op_cache_index = op_index;
  else {
    call_data->cache_op = cache_this;
    call_data->op_index = op_index;
  }

  ILU_NOTE(W3NG_DEBUG,
	("_w3ng_StartRequest:  call %p (sn %lu),"
	 " method \"%s\" (%lu), %s, %s\n",
	 call, call_serial_number(call), method_name(method),
	 method_id(method), pre_cached ? "cached" : "not cached",
	 cache_this ? "to be cached" : "not to be cached"));

  if (class_singleton(pclass)) {
    /* we'll never try to output the discriminant, so output the request header
       here instead of waiting till OutputObjectID. */
    OutputRequestHeader (call, 0, 0, 0, err);
    if (ILU_ERRNOK(*err)) return ilu_FALSE;
  }

  return (ilu_TRUE);
}

static          ilu_boolean
_w3ng_FinishRequest(ilu_Call call,
		      ilu_Message * msg,
		      ilu_boolean push,
		      ILU_ERRS((IoErrs)) * err)
{
  ilu_boolean	ans;
  ans = transport_end_message(w3ng_transport(call), push, msg, err);
  w3ng_call_data(call)->message_begun = ilu_FALSE;
  return ans;
}

static          ilu_boolean
_w3ng_FinishReply(ilu_Call call, ilu_boolean push,
		    ILU_ERRS((IoErrs)) * err)
{
  ilu_Message     msg = {NIL, 0};
  ilu_boolean     ans;

  ans = transport_end_message(w3ng_transport(call), push, &msg, err);
  w3ng_call_data(call)->message_begun = ilu_FALSE;
  return ans;
}

static          ilu_boolean
_w3ng_FinishException(ilu_Call call, ilu_boolean push,
			ILU_ERRS((IoErrs)) * err)
{
  ilu_Message     msg = {NIL, 0};
  ilu_boolean     ans;
  ans = transport_end_message(w3ng_transport(call), push, &msg, err);
  w3ng_call_data(call)->message_begun = ilu_FALSE;
  return ans;
}

static          ilu_boolean
_w3ng_BeginReply(ilu_Call call,
		 ilu_boolean exceptions,
		 ilu_cardinal argSize,
		 ILU_ERRS((IoErrs)) * err)
{
  W3ngCallData *call_data = w3ng_call_data(call);
  ilu_cardinal	msgheader;

  ILU_NOTE(PACKET_DEBUG,
	("%s %lu, argSize %lu, exceptions %s, trans %p.\n",
	 "_w3ng_BeginReply:  SN", call->ca_SN, argSize,
	 exceptions ? "ilu_TRUE" : "ilu_FALSE",
	 w3ng_transport(call)));

  if (transport_begin_message(w3ng_transport(call), ilu_FALSE, err)
      != ilu_rhrc_ok)
    return ilu_FALSE;
  call_data->message_begun = ilu_TRUE;
  msgheader = call_serial_number(call) & 0x00FFFFFF;
  _w3ng_OutputCardinal(call, msgheader, err);
  if (ILU_ERRNOK(*err)) return ilu_FALSE;
  ILU_NOTE(PACKET_DEBUG,
	   ("_w3ng_BeginReply:  started reply %lu.\n",
	    call_serial_number(call)));
  return (ilu_TRUE);
}

static ilu_cardinal
  MapProtocolExceptionToWire (ilu_ProtocolException sysExnIdx)
{
  switch (sysExnIdx) {
  case ilu_ProtocolException_Success:
    return W3NG_SYSEXN_UnknownProblem;
  case ilu_ProtocolException_NoSuchClassAtServer:
    return W3NG_SYSEXN_NoSuchObjectType;
  case ilu_ProtocolException_ClassVersionMismatch:
    return W3NG_SYSEXN_NoSuchObjectType;
  case ilu_ProtocolException_NoSuchMethodOnClass:
    return W3NG_SYSEXN_NoSuchMethod;
  case ilu_ProtocolException_GarbageArguments:
    return W3NG_SYSEXN_Marshal;
  case ilu_ProtocolException_Unknown:
    return W3NG_SYSEXN_UnknownProblem;
  case ilu_ProtocolException_LostConnection:
    return W3NG_SYSEXN_UnknownProblem;
  case ilu_ProtocolException_RequestRejected:
    return W3NG_SYSEXN_Rejected;
  case ilu_ProtocolException_RequestTimeout:
    return W3NG_SYSEXN_ImplementationLimit;
  case ilu_ProtocolException_Not:
    return W3NG_SYSEXN_UnknownProblem;
  default:
    return W3NG_SYSEXN_UnknownProblem;
  }
}

static          ilu_boolean
_w3ng_BeginException(ilu_Call call,
		       ilu_cardinal evalue,
		       ilu_ProtocolException sysExnIdx,
		       ilu_cardinal argSize,
		       ILU_ERRS((IoErrs)) * err)
{
  W3ngCallData *call_data = w3ng_call_data(call);
  ilu_cardinal	msgheader;
  w3ng_ReplyStatus	status;
  ilu_cardinal		err_indicator;

  /*
   * if "evalue" == 0, then sysExnIdx contains a protocol exception
   * detail code.
   */
  status = (evalue == 0) ? SystemExceptionAfter : UserException;
  err_indicator = (evalue == 0) ? MapProtocolExceptionToWire(sysExnIdx) : evalue;

  if (transport_begin_message(w3ng_transport(call), ilu_FALSE, err)
      != ilu_rhrc_ok)
    return ilu_FALSE;
  call_data->message_begun = ilu_TRUE;
  msgheader = (status << 28) | (call_serial_number(call) & 0x00FFFFFF);
  _w3ng_OutputCardinal(call, msgheader, err);
  if (ILU_ERRNOK(*err)) return ilu_FALSE;
  _w3ng_OutputCardinal(call, err_indicator, err);
  if (ILU_ERRNOK(*err)) return ilu_FALSE;
  ILU_NOTE(PACKET_DEBUG,
	("%s: trans %p, SN %lu, evalue %lu, sysExnIdx %lu.\n",
	 "_w3ng_BeginException:  exception started to peer",
	 w3ng_transport(call), call_serial_number(call), evalue, sysExnIdx));
  return ilu_TRUE;
}

/*======================================================================
**======================================================================
**====================  Pickle transformation  =========================
**======================================================================
**====================================================================*/

#ifdef ADD_VARIANT_SUPPORT
#if (!defined(ADD_PICKLE3_SUPPORT)) || (!defined(ADD_TYPE_REGISTRATION_SUPPORT))
#error "W3ng requires both pickle3 support and type registration support"
#endif

static ilu_cardinal
  TotalArrayVals (ilu_Type tp)
{
  /* return the total number of marshalled values for the value of type
     tp, which must be an array type */

  ilu_cardinal i, total;
  if (type_kind(tp) != ilu_array_tk)
    return 0;
  total = type_desc(tp).Array.dims[0];
  for (i = 1;  i < type_desc(tp).Array.n_dims;  i++)
    total = total * type_desc(tp).Array.dims[i];
  return total;
}

static ilu_boolean
  MatchValues (ilu_cardinal ov, ilu_Type disc_type, ilu_ConstantValue cv, ilu_Error *err)
{
  ILU_CLER(*err);

  if (cv->kind == ilu_byte_cvk)
    return ((ilu_byte) ov == cv->value.byte_val);
  else if (cv->kind == ilu_shortinteger_cvk)
    return ((ilu_shortinteger) ov == cv->value.shortinteger_val);
  else if (cv->kind == ilu_integer_cvk)
    return ((ilu_integer) ov == cv->value.integer_val);
  else if (cv->kind == ilu_shortcardinal_cvk)
    return ((ilu_shortcardinal) ov == cv->value.shortcardinal_val);
  else if (cv->kind == ilu_cardinal_cvk)
    return ((ilu_cardinal) ov == cv->value.cardinal_val);
  else if (cv->kind == ilu_boolean_cvk)
    return ((ilu_boolean) ov == cv->value.boolean_val);
  else if ((cv->kind == ilu_enumeration_cvk) &&
	   (type_kind(disc_type) == ilu_enumeration_tk))
    {
      unsigned int i;

      for (i = 0;  i < type_desc(disc_type).Enumeration.n_elements;  i++)
	{
	  if (strcmp(cv->value.enumeration_val, type_desc(disc_type).Enumeration.elements[i].name) == 0)
	    return ((ilu_shortcardinal) ov == type_desc(disc_type).Enumeration.elements[i].value);
	}
    }
  else
    {
      ILU_NOTE(W3NG_DEBUG, ("(ilu -- w3ng.c:MatchValues)  in unmarshalling union as opaque val, bad disc/constantvalue combo\n"));
      (void) ilu_Check(0, err);
    }
  return ILU_ERROK(*err);
}

/* This function recursively descends a type structure and its
   associated pickled value, and outputs the value according
   to w3ng marshalling rules */

static void
  RemarshalMarshalledValue (ilu_Call output_call,
			    ilu_Call input_call,
			    ilu_Type tp,
			    ilu_Error *err)
{
  switch (type_kind(tp)) {

  case ilu_byte_tk:
    {
      ilu_byte b;
      protocol_input_byte (call_proto(input_call), input_call, &b, err);
      if (ILU_ERRNOK(*err)) break;
      protocol_output_byte(call_proto(output_call), output_call, b, err);
    }
    break;

  case ilu_boolean_tk:
    {
      ilu_boolean b;
      protocol_input_boolean (call_proto(input_call), input_call, &b, err);
      if (ILU_ERRNOK(*err)) break;
      protocol_output_boolean(call_proto(output_call), output_call, b, err);
    }
    break;

  case ilu_character_tk:
    {
      ilu_character b;
      protocol_input_character (call_proto(input_call), input_call, &b, err);
      if (ILU_ERRNOK(*err)) break;
      protocol_output_character(call_proto(output_call), output_call, b, err);
    }
    break;

  case ilu_shortcharacter_tk:
    {
      ilu_shortcharacter b;
      protocol_input_shortchar (call_proto(input_call), input_call, &b, err);
      if (ILU_ERRNOK(*err)) break;
      protocol_output_shortchar(call_proto(output_call), output_call, b, err);
    }
    break;

  case ilu_shortinteger_tk:
    {
      ilu_shortinteger b;
      protocol_input_short_integer (call_proto(input_call), input_call, &b, err);
      if (ILU_ERRNOK(*err)) break;
      protocol_output_short_integer(call_proto(output_call), output_call, b, err);
    }
    break;

  case ilu_integer_tk:
    {
      ilu_integer b;
      protocol_input_integer (call_proto(input_call), input_call, &b, err);
      if (ILU_ERRNOK(*err)) break;
      protocol_output_integer(call_proto(output_call), output_call, b, err);
    }
    break;

  case ilu_longinteger_tk:
    {
      ilu_longinteger b;
      protocol_input_long_integer (call_proto(input_call), input_call, &b, err);
      if (ILU_ERRNOK(*err)) break;
      protocol_output_long_integer(call_proto(output_call), output_call, b, err);
    }
    break;

  case ilu_shortcardinal_tk:
    {
      ilu_shortcardinal b;
      protocol_input_short_cardinal (call_proto(input_call), input_call, &b, err);
      if (ILU_ERRNOK(*err)) break;
      protocol_output_short_cardinal(call_proto(output_call), output_call, b, err);
    }
    break;

  case ilu_cardinal_tk:
    {
      ilu_cardinal b;
      protocol_input_cardinal (call_proto(input_call), input_call, &b, err);
      if (ILU_ERRNOK(*err)) break;
      protocol_output_cardinal(call_proto(output_call), output_call, b, err);
    }
    break;

  case ilu_longcardinal_tk:
    {
      ilu_longcardinal b;
      protocol_input_long_cardinal (call_proto(input_call), input_call, &b, err);
      if (ILU_ERRNOK(*err)) break;
      protocol_output_long_cardinal(call_proto(output_call), output_call, b, err);
    }
    break;

  case ilu_real_tk:
    {
      ilu_real b;
      protocol_input_real (call_proto(input_call), input_call, &b, err);
      if (ILU_ERRNOK(*err)) break;
      protocol_output_real(call_proto(output_call), output_call, b, err);
    }
    break;

  case ilu_shortreal_tk:
    {
      ilu_shortreal b;
      protocol_input_short_real (call_proto(input_call), input_call, &b, err);
      if (ILU_ERRNOK(*err)) break;
      protocol_output_short_real(call_proto(output_call), output_call, b, err);
    }
    break;

  case ilu_longreal_tk:
    {
      ilu_longreal b;
      protocol_input_long_real (call_proto(input_call), input_call, &b, err);
      if (ILU_ERRNOK(*err)) break;
      protocol_output_long_real(call_proto(output_call), output_call, b, err);
    }
    break;

  case ilu_string_tk:
    {
      ilu_bytes b = NIL;
      ilu_cardinal encoding, blen;
      protocol_input_string (call_proto(input_call), input_call, (void **) &b, &blen,
			     type_desc(tp).String.limit,
			     type_desc(tp).String.char_set,
			     &encoding, err);
      if (ILU_ERRNOK(*err)) break;
      protocol_output_string(call_proto(output_call), output_call,
			     b, blen,
			     type_desc(tp).String.limit,
			     type_desc(tp).String.char_set,
			     encoding, err);
    }
    break;

  case ilu_object_tk:
    {
      ilu_Object h = NIL;
      ilu_Class oc;
#ifdef ILU_HTTPNG_OBJECTS
      _ilu_Assert (0, "XXX - unfinished work for HTTP-NG local object types");
#else
      protocol_input_object_id(call_proto(input_call), input_call, &h, ilu_FALSE, ilu_rootClass, err);
      /* now Inside(Server(h), ilu_rootClass) */
      if (ILU_ERRNOK(*err)) { if (h != NIL) ilu_ExitServer(object_server(h), ilu_rootClass); break; };
      if ((h != NIL) && (!class_collectible(object_class(h))) && server_is_true(object_server(h)))
	ilu_ReleaseMutex(ilu_gcmu);
      oc = (h != NIL) ? object_class(h) : ilu_rootClass;
      protocol_output_object_id(call_proto(output_call), output_call, h, ilu_FALSE, oc, err);
      /* now outside server */      
#endif
    }
    break;

  case ilu_optional_tk:
    {
      ilu_boolean b;
      protocol_input_optional (call_proto(input_call), input_call, &b, tp, err);
      if (ILU_ERRNOK(*err)) break;
      protocol_output_optional(call_proto(output_call), output_call, b, tp, err);
      if (ILU_ERRNOK(*err)) break;
      if (b)
	RemarshalMarshalledValue(output_call, input_call, type_desc(tp).Optional.type, err);
    }
    break;

#ifdef ILU_REFERENCE_TYPES

  case ilu_reference_tk:
    {
      ilu_boolean present;
      ilu_ReferenceID ref, ref2;
      ilu_cardinal wire_id;
      ilu_boolean must_marshal_value;

      wire_id = protocol_input_reference (call_proto(input_call), input_call, &present, &ref, err);
      if (ILU_ERRNOK(*err)) break;
      if (type_desc(tp).Reference.aliasedp && (ref == NIL))
	ref2 = (ilu_ReferenceID) wire_id;
      else
	ref2 = ref;
      protocol_output_reference (call_proto(output_call), output_call, present, &must_marshal_value, ref2, err);
      if (ILU_ERRNOK(*err)) break;
      if (must_marshal_value) {
	RemarshalMarshalledValue(output_call, input_call, type_desc(tp).Reference.type, err);
	if (ILU_ERRNOK(*err)) break;
      }
      protocol_end_input_reference (call_proto(input_call), input_call, wire_id, ref2, err);
      if (ILU_ERRNOK(*err)) break;
    }
    break;

#endif /* def ILU_REFERENCE_TYPES */

  case ilu_alias_tk:
    {
      RemarshalMarshalledValue(output_call, input_call, type_desc(tp).Alias.type, err);
    }
    break;

  case ilu_union_tk:
    {
      ilu_TypeKind disc_typekind;
      ilu_cardinal disc_value;
      ilu_cardinal i, j;
      ilu_boolean output_value = ilu_FALSE;
      ilu_UnionArm ua;

      /* first, read, store, and output the discriminant */

      disc_typekind = type_kind(type_desc(tp).Union.discriminant);
      protocol_input_union(call_proto(input_call), input_call, &disc_value, disc_typekind, tp, err);
      if (ILU_ERRNOK(*err)) break;
      protocol_output_union(call_proto(output_call), output_call, disc_value, disc_typekind, tp, err);
      if (ILU_ERRNOK(*err)) break;

      /* next, figure out which arm of the union is referred to */

      for (i = 0, output_value = ilu_FALSE;
	   i < type_desc(tp).Union.n_arms && !output_value;
	   i++)
	{
	  ua = &type_desc(tp).Union.arms[i];
	  for (j = 0;  j < ua->n_values && !output_value;  j++)
	    {
	      if (MatchValues(disc_value, type_desc(tp).Union.discriminant, &ua->values[j], err))
		{
		  RemarshalMarshalledValue(output_call, input_call, ua->base.type, err);
		  if (ILU_ERRNOK(*err)) break;
		  output_value = ilu_TRUE;
		}
	    }
	}
      if (! output_value) {
	/* no matching arm:  see if there's a default arm */
	if (type_desc(tp).Union.default_arm != 0) {
	  ilu_cardinal default_arm_index = type_desc(tp).Union.default_arm - 1;
	  RemarshalMarshalledValue(output_call, input_call, type_desc(tp).Union.arms[default_arm_index].base.type, err);
	  if (ILU_ERRNOK(*err)) break;
	} else if (type_desc(tp).Union.others_allowed) {
	  /* do nothing -- bogus discriminant values allowed */
	} else {
	  ILU_ERR_CONS1(marshal, err, minor, ilu_mm_bad_union_disc, 0);
	  break;
	}
      }

    }
    break;

  case ilu_sequence_tk:
    {
      /* three special cases, for character, short character, and byte */
      if (type_kind(type_desc(tp).Sequence.type) == ilu_shortcharacter_tk) {
	ilu_bytes s = NIL;
	ilu_cardinal len, junk;
	protocol_input_string (call_proto(input_call), input_call, (void **) &s, &len,
			       type_desc(tp).Sequence.limit, ILU_StringEncoding_latin1,
			       &junk, err);
	if (ILU_ERRNOK(*err)) break;
	protocol_output_string(call_proto(output_call), output_call, s, len,
			       type_desc(tp).Sequence.limit,
			       ILU_StringEncoding_latin1, ILU_StringEncoding_latin1,
			       err);
	ilu_free(s);
      } else if (type_kind(type_desc(tp).Sequence.type) == ilu_character_tk) {
	ilu_wstring s = NIL;
	ilu_cardinal len;
	protocol_input_wstring (call_proto(input_call), input_call, &s, &len,
				type_desc(tp).Sequence.limit, err);
	if (ILU_ERRNOK(*err)) break;
	protocol_output_wstring(call_proto(output_call), output_call, s, len,
				type_desc(tp).Sequence.limit, err);
	ilu_free(s);
      } else if (type_kind(type_desc(tp).Sequence.type) == ilu_byte_tk) {
	ilu_bytes s = NIL;
	ilu_cardinal len;
	protocol_input_bytes (call_proto(input_call), input_call, &s, &len,
			      type_desc(tp).Sequence.limit, err);
	if (ILU_ERRNOK(*err)) break;
	protocol_output_bytes(call_proto(output_call), output_call, s, len,
			      type_desc(tp).Sequence.limit, err);
	ilu_free(s);
      } else {
	ilu_cardinal count, i;
	ilu_cardinal limit = type_desc(tp).Sequence.limit;
	protocol_input_sequence (call_proto(input_call), input_call, &count, limit, tp, err);
	if (ILU_ERRNOK(*err)) break;
	protocol_output_sequence(call_proto(output_call), output_call, count, limit, tp, err);
	if (ILU_ERRNOK(*err)) break;
	for (i = 0;  i < count;  i++) {
	  RemarshalMarshalledValue(output_call, input_call, type_desc(tp).Sequence.type, err);
	  if (ILU_ERRNOK(*err)) break;
	}
      }
    }
    break;

  case ilu_record_tk:
    {
      ilu_cardinal count = type_desc(tp).Record.n_fields;
      ilu_Type supertype = type_desc(tp).Record.supertype;
      ilu_cardinal i;
      if (ILU_ERRNOK(*err)) break;
      if (supertype != NIL)
	RemarshalMarshalledValue(output_call, input_call, supertype, err);
      else {
	protocol_input_record (call_proto(input_call), input_call, tp, err);
	if (ILU_ERRNOK(*err)) break;
	protocol_output_record(call_proto(output_call), output_call, tp, err);
      }
      if (ILU_ERRNOK(*err)) break;
      for (i = 0;  i < count;  i++) {
	RemarshalMarshalledValue(output_call, input_call, type_desc(tp).Record.fields[i].base.type, err);
	if (ILU_ERRNOK(*err)) break;
      }
    }
    break;

  case ilu_array_tk:
    {
      ilu_Type	tp2 = type_desc(tp).Array.type;
      ilu_cardinal count = TotalArrayVals(tp);

      /* three special cases for one-dimensional arrays of character, short character, and byte */

      if ((type_kind(type_desc(tp).Array.type) == ilu_character_tk) && (type_desc(tp).Array.n_dims == 1)) {
	ilu_wstring s;
	protocol_input_wstringvec(call_proto(input_call), input_call, &s, count, err);
	if (ILU_ERRNOK(*err)) break;
	protocol_output_wstringvec(call_proto(output_call), output_call, s, count, err);
	ilu_free(s);
      } else if ((type_kind(type_desc(tp).Array.type) == ilu_character_tk) && (type_desc(tp).Array.n_dims == 1)) {
	ilu_string s;
	protocol_input_stringvec(call_proto(input_call), input_call, &s, count, err);
	if (ILU_ERRNOK(*err)) break;
	protocol_output_stringvec(call_proto(output_call), output_call, s, count, err);
	ilu_free(s);
      } else if ((type_kind(type_desc(tp).Array.type) == ilu_byte_tk) && (type_desc(tp).Array.n_dims == 1)) {
	ilu_bytes s;
	protocol_input_opaque(call_proto(input_call), input_call, &s, count, err);
	if (ILU_ERRNOK(*err)) break;
	protocol_output_opaque(call_proto(output_call), output_call, s, count, err);
	ilu_free(s);
      } else {
	ilu_cardinal i = 0;
	protocol_input_array (call_proto(input_call), input_call, tp, err);
	if (ILU_ERRNOK(*err)) break;
	protocol_output_array(call_proto(output_call), output_call, i, tp, err);
	if (ILU_ERRNOK(*err)) break;
	for (i = 0;  i < count;  i++) {
	  RemarshalMarshalledValue(output_call, input_call, tp2, err);
	  if (ILU_ERRNOK(*err)) break;
	}
      }
    }
    break;

  case ilu_enumeration_tk:
    {
      ilu_shortcardinal b;
      protocol_input_enum_code(call_proto(input_call), input_call, &b, tp, err);
      if (ILU_ERRNOK(*err)) break;
      protocol_output_enum_code(call_proto(output_call), output_call, b, tp, err);
    }
    break;

  case ilu_pickle_tk:
    {
      ilu_Pickle	p;
      protocol_input_pickle(call_proto(input_call), input_call, &p, tp, err);
      if (ILU_ERRNOK(*err)) break;
      protocol_output_pickle(call_proto(output_call), output_call, p, tp, err);
      ilu_free(p.pi_bytes);
    }
    break;

  default:
    {
      ILU_ERR_CONS1(marshal, err, minor, ilu_mm_bad_typekind, err);
    }
  }
}

static void
  _w3ng_OutputPickle (ilu_Call call,
		      ilu_Pickle pickle,
		      ilu_Type the_type,
		      ilu_Error *err)
{
  /* To figure out the marshalled size of a pickle, we first get the type,
     then build the typecode, then interpret the type to both unpickle the
     pickled value, and resend it as w3ng on the wire */

  ilu_Type	tp;		/* the pickled value's type */
  ilu_string	type_id;	/* the pickled value's type ID */
  ilu_Pickle	p2;		/* second pickle for catching pickle from EndPickle */
  ilu_Error	lerr;		/* dummy err to use with EndPickle */
  ilu_cardinal	version;
  ilu_cardinal	type_kind;
  ilu_cardinal	header;
  ilu_cardinal	id_len;

  version = (pickle.pi_bytes[0] >> 5) & 0x7;

  if (version != 3) {
    struct _ilu_Call_s version2;
    struct _ilu_Call_s version3;

    /* need to convert from an ILU internal pickle format to w3ng pickle
       format... */
    /* first figure out the typecode and its size */
    type_id = ilu_PickleType(pickle, err);
    if (ILU_ERRNOK(*err)) return;
    /* this only works if the type is known in this address space... */
    tp = ilu_FindTypeByUID(type_id, err);
    if (ILU_ERRNOK(*err)) return;
    if (tp == NIL) {	/* unknown type in this address space */
      ILU_ERR_CONS1(internal, err, minor, ilu_im_broken, 0);
      return;
    };

    /* now figure out the size of the marshalled value by unpickling it
       on the fly, and then interpreting it */
    ilu_StartPickle(&version2, NIL, err);
    if (ILU_ERRNOK(*err)) return;
    _ilu_pickle3_StartPickle(&version3, NIL, err);
    if (ILU_ERRNOK(*err)) return;
    ilu_ReadPickle (&version2, pickle, err);
    if (ILU_ERRNOK(*err)) {
      ilu_EndPickle(&version2, &p2, &lerr);
      ILU_HANDLED(lerr);
      ilu_EndPickle(&version3, &p2, &lerr);
      ILU_HANDLED(lerr);
      return;
    }
    ilu_WritePickle (&version3, 0, type_id, err);
    if (ILU_ERRNOK(*err)) {
      ilu_EndPickle(&version2, &p2, &lerr);
      ILU_HANDLED(lerr);
      ilu_EndPickle(&version3, &p2, &lerr);
      ILU_HANDLED(lerr);
      return;
    }
    RemarshalMarshalledValue (&version3, &version2, tp, err);
    ilu_EndPickle(&version2, &p2, &lerr);
    if (ILU_ERROK(*err))
      *err = lerr;
    else
      ILU_HANDLED(lerr);
    if (ILU_ERRNOK(*err)) {
      ilu_EndPickle(&version3, &p2, &lerr);
      ILU_HANDLED(lerr);
      return;
    }
    ilu_EndPickle(&version3, &p2, &lerr);
    if (ILU_ERROK(*err))
      *err = lerr;
    else
      ILU_HANDLED(lerr);

    /* OK, now we have a version 3 pickle in p2 */
  } else {
    p2.pi_bytes = pickle.pi_bytes;
    p2.pi_len = pickle.pi_len;
  }

  version = 0;
  type_kind = p2.pi_bytes[1];
  if (ILU_ERRNOK(*err)) goto errout;
  id_len = (p2.pi_bytes[2] << 8) | p2.pi_bytes[3];

  header = ((version & 0xFF) << 24) | ((type_kind & 0xFF) << 16) | (id_len & 0xFFFF);
  _w3ng_OutputCardinal(call, p2.pi_len, err);
  if (ILU_ERRNOK(*err)) goto errout;
  _w3ng_OutputCardinal(call, header, err);
  if (ILU_ERRNOK(*err)) goto errout;
  _w3ng_OutputOpaque (call, p2.pi_bytes + 4, p2.pi_len - 4, err);

 errout:
  if (p2.pi_bytes != pickle.pi_bytes)
    ilu_free(p2.pi_bytes);
  return;
}

static ilu_boolean
  _w3ng_InputPickle (ilu_Call call,
		     ilu_Pickle *pickle,
		     ilu_Type the_type,
		     ilu_Error *err)
{
  /* We read in the NG pickle, then convert it to an ILU pickle by changing
   * the header slightly */

  pickle->pi_bytes = NIL;
  pickle->pi_len = 0;
  _w3ng_InputBytes (call, &pickle->pi_bytes, &pickle->pi_len, 0xFFFFFFFF, err);
  if (ILU_ERRNOK(*err)) return ilu_FALSE;

  if (pickle->pi_bytes[0] != 0) {
    return ILU_ERR_CONS1(marshal, err, minor, ilu_mm_versionMismatch, ilu_FALSE);
  } else {
    pickle->pi_bytes[0] = (0x3 << 5);
  }
  return ilu_TRUE;
}

#endif /* ADD_VARIANT_SUPPORT */

#if defined(ADD_TYPE_REGISTRATION_SUPPORT)
static ilu_TypeKind
  ur_type_kind (ilu_Type t)
{
  if (t->kind == ilu_alias_tk)
    return ur_type_kind(t->desc.Alias.type);
  else
    return t->kind;
}
#endif /* defined(ADD_TYPE_REGISTRATION_SUPPORT) */

ILU_PUBLIC ilu_boolean
  _ilu_w3ng_CanMoveAsBlock(ilu_Call call,
			   ilu_LanguageIndex li,
			   ilu_Type t,
			   ilu_cardinal m,
			   ilu_cardinal *alignmentCode,
			   ilu_Error *err)
{
#if (defined(WORDS_BIGENDIAN) && defined(ADD_TYPE_REGISTRATION_SUPPORT))
  ilu_Protocol p = connection_protocol(call_connection(call));
  static ilu_boolean initialized = ilu_FALSE;
  static ilu_LanguageIndex ansi_c_index;

  if (!initialized) {
    ansi_c_index = ilu_RegisterLanguage("ANSI-C");
    initialized = ilu_TRUE;
  };

  if ((p->pr_output_union == _w3ng_OutputUnion) &&
      (li == ansi_c_index)) {
    /* OK, using Sun RPC with ANSI C language mapping */
    ILU_CLER(*err);
    *alignmentCode = 0;

    switch (ur_type_kind(t)) {
    case ilu_byte_tk:
    case ilu_string_tk:
    case ilu_character_tk:
    case ilu_shortcharacter_tk:
    case ilu_shortinteger_tk:
    case ilu_shortcardinal_tk:
    case ilu_object_tk:
    case ilu_pipe_tk:
    case ilu_pickle_tk:
    case ilu_optional_tk:
#ifdef ILU_REFERENCE_TYPES
    case ilu_reference_tk:
#endif /* def ILU_REFERENCE_TYPES */
    case ilu_sequence_tk:
    case ilu_alias_tk:
    case ilu_boolean_tk:
      /* can't do it with these types */
      return ilu_FALSE;

    case ilu_longinteger_tk:
    case ilu_integer_tk:
    case ilu_cardinal_tk:
    case ilu_longcardinal_tk:
    case ilu_real_tk:
    case ilu_shortreal_tk:
    case ilu_enumeration_tk:
    case ilu_longreal_tk:
      /* can always do it with these types */
      return ilu_TRUE;

    case ilu_union_tk:
      /* too hard to think about, just say no */
      return ilu_FALSE;

    case ilu_array_tk:
      return _ilu_w3ng_CanMoveAsBlock (call, li, t->desc.Array.type, m, alignmentCode, err);

    case ilu_record_tk:
      {
	/* just handle three special cases, all 4-byte values or
	   all 8-byte values, or all 8-byte values followed by
	   all 4 byte values */
	unsigned int i;
	ilu_Type t2;
	enum states { nothingyet, sz8, sz4odd, sz4even } st;
	ilu_boolean leading_sz8 = ilu_FALSE;

	for (i = 0, st = nothingyet;  i < t->desc.Record.n_fields;  i++) {
	  t2 = t->desc.Record.fields[i].base.type;
	  switch (t2->kind) {
	  case ilu_longcardinal_tk:
	  case ilu_longinteger_tk:
	  case ilu_real_tk:
	    if (st == nothingyet)
	      leading_sz8 = ilu_TRUE;
	    else if (st == sz4odd)
	      return ilu_FALSE;
	    st = sz8;
	    break;
	  case ilu_enumeration_tk:
	  case ilu_integer_tk:
	  case ilu_cardinal_tk:
	  case ilu_shortreal_tk:
	    if (st == sz4odd)
	      st = sz4even;
	    else
	      st = sz4odd;
	    break;
	  default:
	    return ilu_FALSE;
	  }
	}
	return (!((st == sz4odd) && leading_sz8));
      }
    default:
      return ilu_FALSE;      
    }
  };
#endif /* (defined(WORDS_BIGENDIAN) && defined(ADD_TYPE_REGISTRATION_SUPPORT)) */
  ILU_CLER(*err);
  return ilu_FALSE;
}
			     
/**********************************************************************/
/**********************************************************************/
/*								      */
/*			HTTP-ng.RemoteObjectBase		      */
/*								      */
/**********************************************************************/
/**********************************************************************/

#define ILU_TYPEID_CONST_HTTPng_RemoteObjectBase  "w3ngid:www.w3.org/HTTP-ng/RemoteObjectBase"
#define ILU_TYPEID_CONST_HTTPng_UUIDString  "w3ngid:www.w3.org/HTTP-ng/UUIDString"
#define ILU_TYPEID_CONST_HTTPng_InheritanceHierarchy "w3ngid:www.w3.org/HTTP-ng/InheritanceHierarchy"

/* L1, L2, Main unconstrained */

ilu_Class _ilu_HTTPng_RemoteObjectBase = NIL;
static ilu_Method HTTPng_RemoteObjectBase_GetTypeHierarchy = NIL;

struct entries_list {
  ilu_cardinal index;
  ilu_Vector elements;
};

struct type_id_entry {
  ilu_Class cl;
  ilu_cardinal index;
};

static struct type_id_entry *
  find_entry_for_class (struct entries_list *v, ilu_Class c)
{
  struct type_id_entry **entries = (struct type_id_entry **) _ilu_vector_elements(v->elements);
  int i, size;
  for (i = 0, size = _ilu_vector_size(v->elements);  i < size;  i++) {
    if (c == entries[i]->cl)
      return entries[i];
  }
  return NIL;
}

static struct type_id_entry *
  add_entry_for_class (struct entries_list *v, ilu_Class c)
{
  ilu_Error lerr;
  struct type_id_entry *e = ilu_malloc(sizeof(struct type_id_entry));
  if (e == NIL)
    return NIL;
  e->cl = c;
  e->index = v->index++;
  _ilu_vector_add(v->elements, e, &lerr);
  if (ILU_ERRNOK(lerr)) {
    ILU_HANDLED(lerr);
    return NIL;
  };
  return e;
}

static ilu_boolean
  PutTypeIDHierarchy (ilu_Call call, ilu_Class cl, struct entries_list *entries, ilu_Error *err)
{
  struct type_id_entry *e;
  unsigned int i;

  if ((e = find_entry_for_class(entries, cl)) == NIL) {
    e = add_entry_for_class(entries, cl);
    _w3ng_OutputCardinal (call, e->index, err);
    if (ILU_ERRNOK(*err)) return ilu_FALSE;
    _w3ng_OutputBytes (call, (ilu_bytes) class_unique_id(cl), strlen(class_unique_id(cl)),
		       0xFFFF, err);
    if (ILU_ERRNOK(*err)) return ilu_FALSE;
    _w3ng_OutputCardinal (call, class_superclass_count(cl), err);
    if (ILU_ERRNOK(*err)) return ilu_FALSE;
    for (i = 0;  i < class_superclass_count(cl);  i++) {
      PutTypeIDHierarchy (call, class_superclasses(cl)[i], entries, err);
      if (ILU_ERRNOK(*err)) return ilu_FALSE;
    }
  } else {
    _w3ng_OutputCardinal (call, e->index, err);
    if (ILU_ERRNOK(*err)) return ilu_FALSE;
  }
  return (ILU_ERROK(*err));
}

static void FreeEntry(void *p)
{
  ilu_free(p);
}

/* L2    >=    {conn's callmu, iomu} before;
 * L2 disjoint {conn's callmu, iomu} after */
static void 
  w3ng_HandleGetTypeHierarchy (ilu_Call call)
{
  ilu_Object      disc;
  ilu_Error       lerr = ILU_INIT_NO_ERR;
  struct entries_list entries;
  ilu_cardinal    alen;
  ilu_InputObjectID(call, &disc, ilu_TRUE, _ilu_HTTPng_RemoteObjectBase, &lerr);
  if (ILU_ERRNOK(lerr))
    goto errout;
  if (disc != NIL) {
    lerr = _ilu_DeltaHolds(disc, 1);
    ILU_MUST_BE_SUCCESS(lerr);
    ilu_ExitServer(object_server(disc), _ilu_HTTPng_RemoteObjectBase);
  }
  if (!ilu_RequestRead(call, &lerr))
    goto errout;

  alen = 0;	/* we know we don't have to size this,
		   since w3ng doesn't require sizing */
  if (!ilu_BeginReply(call, ilu_FALSE, alen, &lerr))
    goto dun;
  entries.index = 1;
  entries.elements = _ilu_vector_new(4, &lerr);
  if (ILU_ERRNOK(lerr)) goto errout;
  PutTypeIDHierarchy (call, object_class(disc), &entries, &lerr);
  _ilu_vector_destroy(entries.elements, FreeEntry);
  if (ILU_ERRNOK(lerr)) goto errout;
  if (!ilu_FinishReply(call, &lerr))
    goto errout;

dun:
  ILU_HANDLED(lerr);
  if (disc != NIL) {
    ilu_Server      s = object_server(disc);
    ilu_Class       cl = object_class(disc);
    ilu_EnterServer(s, cl);
    lerr = _ilu_DeltaHolds(disc, -1);
    ILU_MUST_BE_SUCCESS(lerr);
    ilu_ExitServer(s, cl);
  }
  return;

 errout:
  ILU_ERR_SWITCH(lerr) {
    ILU_ERR_CASE2(inv_objref, marshal) {
      call->ca_pe = ilu_ProtocolException_GarbageArguments;
    }
    ILU_ERR_ELSE {
      call->ca_pe = ilu_ProtocolException_Unknown;
    }
  } ILU_ERR_ENDSWITCH;
  goto dun;
}

typedef struct TypeHierarchy_s {
  ilu_bytes typeID;
  ilu_cardinal typeIDlen;
  ilu_cardinal index;
  ilu_cardinal nsupertypes;
  struct TypeHierarchy_s **supertypes;
} *TypeHierarchy;

static void
  FreeTypeHierarchy (TypeHierarchy h)
{
  unsigned int i;
  for (i = 0;  i < h->nsupertypes;  i++)
    FreeTypeHierarchy (h->supertypes[i]);
  ilu_free(h->typeID);
  ilu_free(h->supertypes);
}

static TypeHierarchy
  find_entry_by_index (ilu_Vector entries, ilu_cardinal index)
{
  TypeHierarchy *p;
  int i, count;

  for (p = (TypeHierarchy *) _ilu_vector_elements(entries), count = _ilu_vector_size(entries), i = 0;
       i < count;  i++) {
    if (p[i]->index == index)
      return p[i];
  }
  return NIL;
}

static TypeHierarchy
  InputTypeIDHierarchy (ilu_Call call, ilu_Vector entries, ilu_Error *err)
{
  ilu_cardinal index;
  TypeHierarchy n;
  unsigned int i;

  _w3ng_InputCardinal (call, &index, err);
  if (ILU_ERRNOK(*err)) return NIL;
  if ((n = find_entry_by_index(entries, index)) == NIL) {
    n = ilu_MallocE(sizeof(*n), err);
    if (ILU_ERRNOK(*err)) return NIL;
    n->index = index;
    _w3ng_InputBytes (call, &n->typeID, &n->typeIDlen, 0xFFFF, err);
    if (ILU_ERRNOK(*err)) { ilu_free(n); return NIL; };
    _w3ng_InputCardinal (call, &n->nsupertypes, err);
    if (ILU_ERRNOK(*err)) { ilu_free(n->typeID); ilu_free(n); return NIL; };
    n->supertypes = (TypeHierarchy *) ilu_MallocE(sizeof(n) * n->nsupertypes, err);
    if (ILU_ERRNOK(*err)) { ilu_free(n->typeID); ilu_free(n); return NIL; };
    _ilu_vector_add (entries, n, err);
    if (ILU_ERRNOK(*err)) { ilu_free(n->supertypes); ilu_free(n->typeID); ilu_free(n); return NIL; };
    for (i = 0;  i < n->nsupertypes;  i++) {
      n->supertypes[i] = InputTypeIDHierarchy(call, entries, err);
      if (ILU_ERRNOK(*err)) { ilu_free(n->supertypes); ilu_free(n->typeID); ilu_free(n); return NIL; };
    }
  }
  return n;
}

/* Main Invariant holds; L2 otherwise unconstrained */

static ilu_boolean
  CallGetTypeHierarchy (ilu_Object o, TypeHierarchy *hier, ilu_Error *err)
{
  ilu_Call_s      call_s;
  ilu_Call        call = &call_s;
  ilu_cardinal    reqSize;
  ilu_cardinal    estatus = 0;
  ilu_ProtocolException internal;
  ilu_Server      s = object_server(o);
  ilu_Connection  newconn;
  ilu_boolean     ans;
  ilu_Vector	  v;
  ILU_NOTE(OBJECT_DEBUG, ("_ilu_w3ng_FindClassViaRPC:  object %p...\n",
		       o));
  ans = ilu_StartCall(call, s, _ilu_HTTPng_RemoteObjectBase,
		      HTTPng_RemoteObjectBase_GetTypeHierarchy,
		      0, NIL, &newconn, err);
  if (newconn != NIL)
    _ilu_HandOffNewConnection(newconn, err);
  if (ILU_ERRNOK(*err))
    return ilu_FALSE;
  _ilu_AcquireServerMutex(s);
  reqSize = ilu_SizeOfObjectID(call, o, ilu_TRUE,
			       _ilu_HTTPng_RemoteObjectBase, err);
  _ilu_ReleaseServerMutex(s);
  if (ILU_ERRNOK(*err))
    goto faild;
  if (!ilu_StartRequest(call, reqSize, err))
    goto faild;
  ilu_EnterServer(s, object_class(o));
  ilu_OutputObjectID(call, o, ilu_TRUE, _ilu_rootClass, err);
  if (ILU_ERRNOK(*err))
    goto faild;
  if (!ilu_FinishRequest(call, err))
    goto faild;
  internal = ilu_GetReply(call, &estatus, &newconn, err);
  if (newconn != NIL)
    _ilu_HandOffNewConnection(newconn, err);
  if (ILU_ERRNOK(*err))
    goto faild;
  if (internal != ilu_ProtocolException_Success) {
    (void) ILU_ERR_CONS1(internal, err, minor, ilu_im_callFail, 0);
    goto faild;
  }
  if (estatus != 0) {
    (void) ILU_ERR_CONS1(internal, err, minor, ilu_im_callFail, 0);
    goto faild;
  }
  v = _ilu_vector_new(4, err);
  if (ILU_ERRNOK(*err)) goto faild;
  *hier = InputTypeIDHierarchy (call, v, err);
  _ilu_vector_destroy(v, NULLFN);
  if (ILU_ERRNOK(*err))
    goto faild;
  ilu_ReplyRead(call, err);
  if (ILU_ERRNOK(*err))
    goto faild;
faild:
  ilu_FinishCall(call, err);
  if (ILU_ERRNOK(*err)) {
    ILU_HANDLED(*err);
    return ilu_FALSE;
  }
  return ilu_TRUE;
}

#ifdef ILU_GENERATE_SUBTYPES
static ilu_Class
  CreateNewClass (ilu_Vector supertypes, char *typeid)
{
  ilu_string *supertype_ids;
  int i;
  ilu_Error lerr;
  ilu_Class c;

  supertype_ids = (ilu_string *) ilu_malloc(sizeof(ilu_string) * _ilu_vector_size(supertypes));
  if (supertype_ids == NIL)
    return NIL;
  for (i = 0;  i < _ilu_vector_size(supertypes);  i++) {
    supertype_ids[i] = class_unique_id((ilu_Class) _ilu_vector_elements(supertypes)[i]);
  }
  ilu_AcquireMutex (ilu_otmu);
  c = ilu_DefineObjectType (ilu_InventID(),	/* make up name */
			    NIL,		/* no brand */
			    typeid,		/* use passed typeid */
			    NIL,		/* not singleton */
			    ilu_FALSE,		/* not optional */
			    ilu_FALSE,		/* not collectible */
			    NIL,		/* no doc string */
			    0,			/* no methods */
			    _ilu_vector_size(supertypes),
			    supertype_ids,		/* type IDs of supertypes */
#ifdef ILU_HTTPNG_OBJECTS
			    0, ilu_FALSE, ilu_FALSE,
#endif
			    &lerr);
  if (ILU_ERRNOK(lerr)) {
    ILU_HANDLED(lerr);
    ilu_free(supertype_ids);
    ilu_ReleaseMutex(ilu_otmu);
    return NIL;
  };
  c->cl_phony = ilu_TRUE;
  ilu_ObjectTypeDefined (c, &lerr);
  if (ILU_ERRNOK(lerr)) {
    ILU_HANDLED(lerr);
    ilu_ReleaseMutex(ilu_otmu);
    return NIL;
  };
  ilu_ReleaseMutex(ilu_otmu);
  return c;
}
#endif

static ilu_boolean
  class_in_vector (ilu_Vector v, ilu_Class c)
{
  unsigned int i;
  for (i = 0;  i < _ilu_vector_size(v);  i++) {
    if (c == (ilu_Class) _ilu_vector_elements(v)[i])
      return ilu_TRUE;
  }
  return ilu_FALSE;
}

static void
  FindKnownClass2 (TypeHierarchy h, ilu_Vector known_types)
{
  ilu_Class c;
  unsigned int i;
  ilu_Error lerr;

  if ((c = ilu_FindClassFromID ((char *) (h->typeID))) != NIL) {
    if (!class_in_vector(known_types, c)) {
      _ilu_vector_add(known_types, c, &lerr);
      ILU_MUST_BE_SUCCESS(lerr);
    };
  } else {
    for (i = 0;  i < h->nsupertypes;  i++) {
      (void) FindKnownClass2 (h->supertypes[i], known_types);
    }
  }
}

static ilu_Class
  FindKnownClass (TypeHierarchy h)
{
  ilu_Vector known_types = NIL;
  ilu_Error lerr;
  ilu_Class c;
  unsigned int i;

  known_types = _ilu_vector_new (1, &lerr);
  ILU_MUST_BE_SUCCESS(lerr);

  FindKnownClass2 (h, known_types);

  if (_ilu_vector_size(known_types) == 1) {
    c = (ilu_Class) _ilu_vector_elements(known_types)[0];
    _ilu_vector_destroy (known_types, NULLFN);
    return c;
  } else if (_ilu_vector_size(known_types) > 1) {
#ifdef ILU_GENERATE_SUBTYPES
    c = CreateNewClass (known_types, (char *) h->typeID);
    _ilu_vector_destroy(known_types, NULLFN);
    return c;
#else
#ifdef ENABLE_DEBUGGING
    ILU_NOTE(W3NG_DEBUG | OBJECT_DEBUG | TYPE_DEBUG,
		 ("ILU(w3ng.c:FindKnownClass):  multiple known supertypes for unknown type %s are:\n",
		  (char *) h->typeID));
    for (i = 0; i < _ilu_vector_size(known_types);  i++) {
      c = _ilu_vector_elements(known_types)[i];
      ILU_NOTE(W3NG_DEBUG | OBJECT_DEBUG | TYPE_DEBUG,
	     ("    %s (%s)\n", class_name(c), class_unique_id(c)));
    }
#endif /* def ENABLE_DEBUGGING */
    _ilu_vector_destroy (known_types, NULLFN);
    return NIL;
#endif /* def ILU_GENERATE_SUBTYPES */
  } else {
    return NIL;
  }
}

ilu_Class
  _ilu_w3ng_FindClassViaRPC (ilu_Object o)
{
  ilu_Class       c = NIL;
  ilu_Class       pclass = object_class(o);
  TypeHierarchy	  types;
  ILU_ERRS((bad_locks, inv_objref,
		      no_resources, IoErrs)) lerr;

  if (class_singleton(pclass)) {
    ILU_NOTE(OBJECT_DEBUG,
	  ("%s %s is singleton, not attempting GetTypes RPC call.\n",
	   "_ilu_w3ng_FindClassViaRPC:  pclass", class_name(pclass)));
    return (NIL);
  }

  _ilu_Assert(!server_is_true(object_server(o)), "_ilu_w3ng_FindClassViaRPC: called on true object");
  
  if (!CallGetTypeHierarchy(o, &types, &lerr)) {
    ILU_NOTE(OBJECT_DEBUG | TYPE_DEBUG,
	  ("_ilu_w3ng_FindClassViaRPC:  CallGetTypeHierarchy raised %s from %s:%d\n",
	   ILU_ERR_NAME(lerr), ilu_ErrorFile(&lerr),
	   ilu_ErrorLine(&lerr)));
  } else {
    if ((c = FindKnownClass (types)) == NIL)
      ILU_NOTE(OBJECT_DEBUG | TYPE_DEBUG,
	       ("_ilu_w3ng_FindClassViaRPC:  Can't figure any known type from the type hierarchy\n"));
  }
  return c;
}

/*======================================================================
**======================================================================
**====================  Protocol Object Init ===========================
**======================================================================
**====================================================================*/

/*L1, L2 unconstrained*/

static ilu_Protocol _w3ng_NewW3ng (void)
{
  ilu_Protocol    new = (ilu_Protocol) ilu_must_malloc(sizeof(*new));

  new->pr_concurrent_requests = ilu_FALSE;
  new->pr_sizing_required = ilu_FALSE;
  new->pr_needs_boundaried_transport = ilu_TRUE;
  new->pr_needs_reliable_transport = ilu_TRUE;

  new->pr_init_call = _w3ng_InitCall;
  new->pr_start_request = _w3ng_StartRequest;
  new->pr_finish_request = _w3ng_FinishRequest;
  new->pr_begin_sizing_reply = NULLFN;
  new->pr_begin_reply = _w3ng_BeginReply;
  new->pr_finish_reply = _w3ng_FinishReply;
  new->pr_begin_sizing_exn = NULLFN;
  new->pr_begin_exception = _w3ng_BeginException;
  new->pr_finish_exception = _w3ng_FinishException;
  new->pr_finish_call = _w3ng_FinishCall;
  new->pr_prefinish_call = NULLFN;

  new->pr_read_header = _w3ng_ReadHeader;
  new->pr_delay_interp = _w3ng_DelayInterp;
  new->pr_resume_interp = _w3ng_ResumeInterp;
  new->pr_abandon_delayed_interp = _w3ng_AbandonDelayedInterp;
  new->pr_discard_input = _w3ng_DiscardMessage;
  new->pr_discard_output = _w3ng_DiscardMessage;
  
  new->pr_interpret_request = _w3ng_InterpretRequest;
  new->pr_request_read = _w3ng_RequestRead;
  new->pr_interpret_reply = _w3ng_InterpretReply;
  new->pr_reply_read = _w3ng_ReplyRead;

  new->pr_output_integer = _w3ng_OutputInteger;
  new->pr_input_integer = _w3ng_InputInteger;
  new->pr_size_of_integer = NULLFN;

  new->pr_output_shortinteger = _w3ng_OutputShortInteger;
  new->pr_input_shortinteger = _w3ng_InputShortInteger;
  new->pr_size_of_shortinteger = NULLFN;

  new->pr_output_longinteger = _w3ng_OutputLongInteger;
  new->pr_input_longinteger = _w3ng_InputLongInteger;
  new->pr_size_of_longinteger = NULLFN;

  new->pr_output_cardinal = _w3ng_OutputCardinal;
  new->pr_input_cardinal = _w3ng_InputCardinal;
  new->pr_size_of_cardinal = NULLFN;

  new->pr_output_shortcardinal = _w3ng_OutputShortCardinal;
  new->pr_input_shortcardinal = _w3ng_InputShortCardinal;
  new->pr_size_of_shortcardinal = NULLFN;

  new->pr_output_longcardinal = _w3ng_OutputLongCardinal;
  new->pr_input_longcardinal = _w3ng_InputLongCardinal;
  new->pr_size_of_longcardinal = NULLFN;

  new->pr_output_real = _w3ng_OutputReal;
  new->pr_input_real = _w3ng_InputReal;
  new->pr_size_of_real = NULLFN;

  new->pr_output_shortreal = _w3ng_OutputShortReal;
  new->pr_input_shortreal = _w3ng_InputShortReal;
  new->pr_size_of_shortreal = NULLFN;

  new->pr_output_longreal = _w3ng_OutputLongReal;
  new->pr_input_longreal = _w3ng_InputLongReal;
  new->pr_size_of_longreal = NULLFN;

  new->pr_output_optional = _w3ng_OutputOptional;
  new->pr_input_optional = _w3ng_InputOptional;
  new->pr_size_of_optional = NULLFN;

  new->pr_output_enum_code = _w3ng_OutputEnumeration;
  new->pr_input_enum_code = _w3ng_InputEnumeration;
  new->pr_size_of_enum_code = NULLFN;

  new->pr_output_byte = _w3ng_OutputByte;
  new->pr_input_byte = _w3ng_InputByte;
  new->pr_size_of_byte = NULLFN;

  new->pr_output_character = _w3ng_OutputShortCardinal;
  new->pr_input_character = _w3ng_InputShortCardinal;
  new->pr_size_of_character = NULLFN;

  new->pr_output_boolean = _w3ng_OutputBoolean;
  new->pr_input_boolean = _w3ng_InputBoolean;
  new->pr_size_of_boolean = NULLFN;

  new->pr_output_shortchar = _w3ng_OutputShortChar;
  new->pr_input_shortchar = _w3ng_InputShortChar;
  new->pr_size_of_shortchar = NULLFN;

  new->pr_output_string = _w3ng_OutputString;
  new->pr_input_string = _w3ng_InputString;
  new->pr_size_of_string = NULLFN;

  new->pr_output_wstring = _ilu_OutputWString;
  new->pr_input_wstring = _ilu_InputWString;
  new->pr_size_of_wstring = NULLFN;

  new->pr_output_bytes = _w3ng_OutputBytes;
  new->pr_input_bytes = _w3ng_InputBytes;
  new->pr_size_of_bytes = NULLFN;

  new->pr_output_opaque = _w3ng_OutputOpaque;
  new->pr_input_opaque = _w3ng_InputOpaque;
  new->pr_size_of_opaque = NULLFN;

  new->pr_output_wstringvec = _ilu_OutputWStringVec;
  new->pr_input_wstringvec = _ilu_InputWStringVec;
  new->pr_size_of_wstringvec = NULLFN;

#ifdef ILU_HTTPNG_OBJECTS

  new->pr_begin_output_object = _w3ng_BeginOutputObject;
  new->pr_begin_output_state = _w3ng_BeginOutputState;
  new->pr_finish_output_state = _w3ng_EndOutputState;
  new->pr_finish_output_object = _w3ng_EndOutputObject;

  new->pr_begin_size_of_object = NULLFN;
  new->pr_begin_size_of_state = NULLFN;
  new->pr_finish_size_of_state = NULLFN;
  new->pr_finish_size_of_object = NULLFN;

  new->pr_begin_input_object = _w3ng_BeginInputObject;
  new->pr_begin_input_state = _w3ng_BeginInputState;
  new->pr_skip_input_state = _w3ng_SkipInputState;
  new->pr_finish_input_state = _w3ng_EndInputState;
  new->pr_finish_input_object = _w3ng_EndInputObject;

#endif

  new->pr_output_object_id = _w3ng_OutputObjectID;
  new->pr_input_object_id = _w3ng_InputObjectID;
  new->pr_size_of_object_id = NULLFN;

  new->pr_output_stringvec =
	(void (*)(ilu_Call,ilu_string,ilu_cardinal,ilu_Error*))
	_w3ng_OutputOpaque;
  new->pr_input_stringvec =
	(void (*)(ilu_Call,ilu_string *,ilu_cardinal,ilu_Error*))
	_w3ng_InputOpaque;
  new->pr_size_of_stringvec =
	(ilu_cardinal (*)(ilu_Call,ilu_string,ilu_cardinal,ilu_Error*))
	NIL;

  new->pr_output_sequence = _w3ng_OutputSequence;
  new->pr_output_sequence_mark = _w3ng_OutputSequenceMark;
  new->pr_input_sequence = _w3ng_InputSequence;
  new->pr_input_sequence_mark = _w3ng_InputSequenceMark;
  new->pr_end_sequence = _w3ng_EndSequence;
  new->pr_size_of_sequence = NULLFN;

  new->pr_output_record = _w3ng_OutputRecord;
  new->pr_input_record = _w3ng_InputRecord;
  new->pr_end_record = _w3ng_EndRecord;
  new->pr_size_of_record = NULLFN;

  new->pr_output_array = _w3ng_OutputArray;
  new->pr_input_array = _w3ng_InputArray;
  new->pr_end_array = _w3ng_EndArray;
  new->pr_size_of_array = NULLFN;

  new->pr_output_union = _w3ng_OutputUnion;
  new->pr_input_union = _w3ng_InputUnion;
  new->pr_end_union = _w3ng_EndUnion;
  new->pr_size_of_union = NULLFN;

#ifdef ADD_VARIANT_SUPPORT

  new->pr_output_pickle = _w3ng_OutputPickle;
  new->pr_input_pickle = _w3ng_InputPickle;
  new->pr_size_of_pickle = NULLFN;

#endif /* ADD_VARIANT_SUPPORT */

#ifdef ILU_FIXED_POINT_SUPPORT

  new->pr_output_fixedpoint = _ilu_OutputFixedpoint;
  new->pr_input_fixedpoint = _ilu_InputFixedpoint;
  new->pr_size_of_fixedpoint = _ilu_SizeOfFixedpoint;

#endif /* ILU_FIXED_POINT_SUPPORT */

#ifdef ILU_REFERENCE_TYPES

  new->pr_size_of_reference = _ilu_SizeOfReference;
  new->pr_output_reference = _ilu_OutputReference;
  new->pr_input_reference = _ilu_InputReference;
  new->pr_end_input_reference = _ilu_EndInputReference;

#endif /* def ILU_REFERENCE_TYPES */

  new->pr_form_handle = _w3ng_FormProtocolHandle;

  new->pr_create_data_block = _w3ng_CreateDataBlock;
  new->pr_free_data_block = _w3ng_FreeDataBlock;
  new->pr_conn_closing = _w3ng_CloseConnection;

  return (new);
}

/*L1_sup < prmu*/

ilu_Protocol _ilu_w3ng_Protocol(ilu_ProtocolInfo pinfo,
				  ilu_Error *err)
{
  /* L1 >= {prmu} */
  static ilu_Protocol protocol = NIL;

  if (_ilu_HTTPng_RemoteObjectBase == NIL) {
    _ilu_HTTPng_RemoteObjectBase = ilu_FindClassFromID(ILU_TYPEID_CONST_HTTPng_RemoteObjectBase);
    if (_ilu_HTTPng_RemoteObjectBase == NIL) {
      ilu_AcquireMutex(ilu_otmu);
      _ilu_HTTPng_RemoteObjectBase =
	ilu_DefineObjectType ("HTTP-ng.RemoteObjectBase",
			      NIL,
			      ILU_TYPEID_CONST_HTTPng_RemoteObjectBase,
			      NIL,
			      ilu_FALSE, /* optional */
			      ilu_FALSE, /* collectible */
			      "internal root type for all HTTP-ng objects",
			      1,
			      0,
			      NIL,
#ifdef ILU_HTTPNG_OBJECTS
			      0, ilu_FALSE, ilu_FALSE,
#endif
			      err);
      if (ILU_ERRNOK(*err)) {
	ILU_NOTE(W3NG_DEBUG,
		 ("_ilu_w3ng_Protocol:  Can't register HTTP-ng.RemoteObjectBase!\n"));
      } else {
	HTTPng_RemoteObjectBase_GetTypeHierarchy =
	  ilu_DefineMethod (_ilu_HTTPng_RemoteObjectBase,
			    0, "GetTypeHierarchy", 0xFF86,
			    ilu_TRUE, /* cacheable */
			    ilu_FALSE, /* async */
			    0,	/* no exceptions */
			    NIL, /* empty excn vector */
			    0,	/* no args */
			    ILU_TYPEID_CONST_HTTPng_InheritanceHierarchy,
			    err);
	if (ILU_ERRNOK(*err)) {
	  ILU_NOTE(W3NG_DEBUG,
		   ("_ilu_w3ng_Protocol:  Can't define method HTTP-ng.RemoteObjectBase.GetTypeHierachy!\n"));
	} else {
	  ilu_ObjectTypeDefined (_ilu_HTTPng_RemoteObjectBase, err);
	  if (ILU_ERRNOK(*err)) {
	    ILU_NOTE(W3NG_DEBUG,
		     ("_ilu_w3ng_Protocol:  Can't register HTTP-ng.RemoteObjectBase!\n"));
	  }
	}
      }
      ilu_ReleaseMutex(ilu_otmu);
      if (ILU_ERRNOK(*err))
	return NIL;
    } else {
      HTTPng_RemoteObjectBase_GetTypeHierarchy = &class_methods(_ilu_HTTPng_RemoteObjectBase)[0];
    }
    if (ilu_GetMethodStubProc(HTTPng_RemoteObjectBase_GetTypeHierarchy, 0) == NULLFN)
      ilu_SetMethodStubProc (HTTPng_RemoteObjectBase_GetTypeHierarchy,
			     w3ng_HandleGetTypeHierarchy, 0);
  };

  _ilu_AcquireMutex(ilu_prmu);
  if (protocol == NIL) {
    protocol = _w3ng_NewW3ng();
  }
  _ilu_ReleaseMutex(ilu_prmu);
  ILU_CLER(*err);
  return (protocol);
}
