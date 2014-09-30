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
/* $Id: ilutransport.c,v 1.80 1999/09/09 17:46:24 janssen Exp $ */
/* Last edited by Mike Spreitzer May 22, 1998 8:28 am PDT */

#define _POSIX_SOURCE


#include "iluntrnl.h"
#include "ilutransport.h"

struct transports_s {
  /* L1, L2, Main unconstrained */

  ilu_string      name;
  ilu_TransportInstantiator *instantiator;
};

#if 0
/* L1 >= {cmu}; L2 unconstrained */

ilu_integer     _ilu_waitsDisabled = 0;
ilu_Condition   _ilu_connAbleCC = NIL;

ilu_boolean _ilu_NoteWaitsAble(ILU_ERRS((broken_locks)) * err)
{
  return ilu_CondNotify(_ilu_connAbleCC, err);
}
#endif

/*L1, L2 unconstrained*/
ilu_cardinal ilu_TransportInfo_Len(ilu_TransportInfo ti) {
  unsigned i;
  if (!ti)
    return 0;
  for (i=0; ti[i]; i++)
    0;
  return i;
}

/*L1, L2 unconstrained*/
ilu_WaitCohort
ilu_CreateWaitCohort(ilu_string d1, ilu_string d2,
		     ilu_boolean global,
		     ILU_ERRS((no_memory, no_resources,
			       internal)) * err)
{
  ilu_WaitCohort  wc;
  _ilu_CommitThreadedness();
  if (!ilu_CanCondition())
    return ILU_CLER(*err), NIL;
  wc = (ilu_WaitCohort) ilu_MallocE(sizeof(*wc), err);
  if (!wc)
    return NIL;
  wc->iluwc_change = _ilu_CreateCondition(d1, d2, err);
  if (!wc->iluwc_change) {
    ilu_free(wc);
    return NIL;
  }
  wc->iluwc_waitsDisabled = 0;
  wc->iluwc_refcount = 1;
  wc->iluwc_global = global;
  ILU_NOTE(CONNECTION_DEBUG,
	   ("ilu_CreateWaitCohort(%s %s) => %p\n", d1, d2, wc));
  return wc;
}

/*L1.sup < cmu; L2 unconstrained*/
ilu_WaitCohort
ilu_GetNeverWaitCohort(ILU_ERRS((no_memory, no_resources, broken_locks,
				 bad_locks, internal)) * err)
{
  ilu_WaitCohort  ans;
  if (!ilu_EnterMutex(ilu_cmu, err))
    return NIL;
  ans = _ilu_GetNeverWaitCohort(err);
  if (!ilu_ExitMutex(ilu_cmu, ilu_TRUE, err))
    return NIL;
  return ans;
}

/* L1 >= {cmu}; L2 unconstrained */

static ilu_boolean theNeverWaitCohortSet = ilu_FALSE;
static ilu_WaitCohort theNeverWaitCohort = NIL;

ilu_WaitCohort
_ilu_GetNeverWaitCohort(ILU_ERRS((no_memory, no_resources,
				  internal)) * err)
{
  if (!theNeverWaitCohortSet) {
    theNeverWaitCohort = ilu_CreateWaitCohort("never", "waits", ilu_TRUE,
					      err);
    if (ILU_ERRNOK(*err))
      return NIL;
    theNeverWaitCohortSet = ilu_TRUE;
  }
  if (theNeverWaitCohort)
    if (!ilu_DeltaWaitCohortRefCount(theNeverWaitCohort, 1, err))
      return NIL;
  return theNeverWaitCohort;
}

extern          ilu_boolean
ilu_FullDeltaWaitCohortRefCount(ilu_WaitCohort wc,
				int dRefCount,
				ILU_ERRS((internal)) * err,
				const char *filename,
				int linenum)
{
  ilu_integer     newRefCount = wc->iluwc_refcount + dRefCount;
  _ilu_HoldMutex(ilu_cmu);
  ILU_NOTE(CONNECTION_DEBUG,
    ("ilu_DeltaWaitCohortRefCount(%p), count = %ld + %d @ %s:%d\n",
     wc, (long int) wc->iluwc_refcount, dRefCount,
     filename, linenum));
  if (!ilu_Check(wc->iluwc_refcount >= 0 && newRefCount >= 0, err))
    return ilu_FALSE;
  wc->iluwc_refcount = newRefCount;
  if (wc->iluwc_refcount)
    return ilu_TRUE;
  if (!ilu_Check(!wc->iluwc_global, err))
    return ilu_FALSE;
  (void) _ilu_CondDestroy(wc->iluwc_change, err);
  ilu_free(wc);
  return ILU_ERROK(*err);
}

extern          ilu_boolean
_ilu_DeltaCohortWaits(ilu_WaitCohort wc,
		      int dWaits,
		      ILU_ERRS((broken_locks,
				internal)) * err)
{
  ilu_boolean     wasEnabled = !wc->iluwc_waitsDisabled;
  ilu_integer     newWaitsDisabled = wc->iluwc_waitsDisabled + dWaits;
  _ilu_HoldMutex(ilu_cmu);
  ILU_NOTE(CONNECTION_DEBUG,
	   ("ilu_DeltaCohortWaits(%p), waits = %ld + %d\n",
	    wc, (long int) wc->iluwc_waitsDisabled, dWaits));
  if (!ilu_Check(wc != theNeverWaitCohort &&
		 wc->iluwc_waitsDisabled >= 0 &&
		 newWaitsDisabled >= 0,
		 err))
    return ilu_FALSE;
  wc->iluwc_waitsDisabled = newWaitsDisabled;
  if (wasEnabled != (!wc->iluwc_waitsDisabled)) {
    if (!ilu_CondNotify(wc->iluwc_change, err))
      return ilu_FALSE;
  }
  return ILU_CLER(*err);
}

/*L1, L2 unconstrained*/

static struct transports_s transports[MAX_TRANSPORTS] = {
#ifdef UDPSOCKET_TRANSPORT
  { "udp", _ilu_udp_TransportCreator },
#endif /* UDPSOCKET_TRANSPORT */
#ifdef TCPIP_TRANSPORT
  { "tcp", _ilu_tcp_TransportCreator },
#endif /* TCPIP_TRANSPORT */
#ifdef SUNRPCRM_TRANSPORT
  { "sunrpcrm", _ilu_sunrpcrm_TransportCreator },
#endif /* SUNRPCRM_TRANSPORT */
#ifdef W3MUX_TRANSPORT
  { "w3mux", _ilu_w3mux_TransportCreator },
#endif /* W3MUX_TRANSPORT */
#ifdef BATCHING_TRANSPORT
  { "batching", _ilu_batching_TransportCreator },
#endif /* BATCHING_TRANSPORT */
#ifdef SECURE_TRANSPORT
  { "gss", _ilu_gss_TransportCreator },
#endif /* SECURE_TRANSPORT */
#ifdef ILU_ZLIB_TRANSPORT
  { "zlib", _ilu_zlib_TransportCreator },
#endif /* def ILU_ZLIB_TRANSPORT */
  { "inmem", _ilu_inmem_TransportCreator },
  { NIL, NULLFN } };

ILU_ERRS((TransportAlreadyRegistered, MaxCountExceeded))
ilu_RegisterTransport(char *name,
		      ilu_TransportInstantiator * new_transport,
		      ilu_boolean override)
{
  int             i;
  ilu_Error       e;

  ILU_NOTE(EXPORT_DEBUG, ("ilu_RegisterTransport (%s)\n", name));
  for (i = 0; i < MAX_TRANSPORTS && transports[i].name != NIL; i++) {
    if (strcmp(transports[i].name, name) == 0) {
      if (override)
	transports[i].instantiator = new_transport;
      else
	{
	  ILU_NOTE(EXPORT_DEBUG,
		    ("ilu_RegisterTransport:  \"%s\" already registered.\n",
		     name));
	  return ILU_ERR_CONS3(TransportAlreadyRegistered, &e,
			       name, name,
			       old_transport, transports[i].instantiator,
			       new_transport, new_transport, e);
	}
    }
  }
  if (i < MAX_TRANSPORTS && transports[i].name == NIL) {
    transports[i].name = name;
    transports[i].instantiator = new_transport;
    if ((i + 1) < MAX_TRANSPORTS)
      transports[i + 1].name = NIL;
    return ILU_NO_ERR;
  } else
    {
      ILU_NOTE(EXPORT_DEBUG,
		("ilu_RegisterTransport:  too many transports"
		 "(%d) to register \"%s\"\n", MAX_TRANSPORTS, name));
      return ILU_ERR_CONS1(MaxCountExceeded, &e,
			   max_count, MAX_TRANSPORTS, e);
    }
}

static struct transports_s *
  FindTransport(ilu_string tinfo)
{
  ilu_integer     i, l1;
  char           *p;
  if (tinfo == NIL)
    return NIL;
  p = strchr(tinfo, '_');
  if (p == NIL)
    l1 = strlen(tinfo);
  else
    l1 = p - tinfo;
  for (i = 0; transports[i].name != NIL; i += 1)
    if (_ilu_casefree_ncmp(tinfo, transports[i].name, l1) == 0
	&& transports[i].name[l1] == 0)
      {
	ILU_NOTE(CONNECTION_DEBUG,
		  ("(ilutransport.c/FindTransport)(\"%s\") => %p\n",
		   tinfo, &transports[i]));
	return (&transports[i]);
      }
  ILU_NOTE(CONNECTION_DEBUG,
	    ("(ilutransport.c/FindTransport)(\"%s\") => NIL\n", tinfo));
  return (NIL);
}

/*L1.sup < trmu*/
ilu_TransportCreator
_ilu_GetTransportCreator(ilu_TransportInfo tinfo,
			 ILU_ERRS((no_memory, inv_objref)) * err)
{
  struct transports_s *p;
  ilu_TransportCreator tcr;

  if ((p = FindTransport(tinfo[0])) == NIL) {
    ILU_NOTE(CONNECTION_DEBUG,
	  ("_ilu_GetTransportCreator: Unable to find registered "
	   "transport creator for info \"%s\".\n",
	   tinfo[0]));
    return ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_tc, NIL);
  }
  tcr = (*(p->instantiator)) (tinfo, err);
  ILU_NOTE(CONNECTION_DEBUG, 
	   ("_ilu_GetTransportCreator (\"%s\", ...) => %p\n",
	    tinfo[0], tcr));
  return tcr;
}

/*Main Invariant holds; L2 >= {xmu}*/
ilu_boolean
_ilu_transportWriteBytes(ilu_Transport self,
			 ilu_bytes buf,
			 ilu_cardinal bufLen,
			 ILU_ERRS((IoErrs)) * err)
{
  ilu_TransportClass tc = self->tr_class;
  return ((*tc->tc_write_bytes) (self, buf, bufLen, ilu_FALSE, err));
}

/*Main Invariant holds; L2 >= {xmu}*/
ilu_bytes
_ilu_transportGetOutputBuffer(ilu_Transport self,
			      ilu_cardinal len,
			      ILU_ERRS((IoErrs)) * err)
{
  if (len > 16)
    return ILU_ERR_CONS1(internal, err, minor, ilu_im_trBufSize, NIL);
  if (!(*self->tr_class->tc_write_bytes) (self, NIL, 0, ilu_FALSE, err))
    return NIL;
  if (self->tr_outBuff == NIL || self->tr_outNext >= self->tr_outLimit
      || 16 > self->tr_outLimit - self->tr_outNext)
    return ILU_ERR_CONS1(internal, err, minor, ilu_im_tcBug, NIL);
  return (self->tr_outBuff + (self->tr_outNext += len) - len);
}


/**------------------------------------------------------------**/

/* Main Invariant holds; L2 >= {xmu, ymu} */

ilu_boolean
ilu_CloseTransport(ilu_Transport self,
		   ilu_integer * dfd,
		   ILU_ERRS((bad_locks, broken_locks,
			     internal)) * err)
{
  ilu_boolean     ans;
  if (!self->tr_wc)
    return transport_close(self, dfd, err);
  if (!ilu_EnterMutex(ilu_cmu, err))
    return ilu_FALSE;
  ans = transport_close(self, dfd, err);
  if (!ilu_ExitMutex(ilu_cmu, ilu_TRUE, err))
    return ilu_FALSE;
  return ans;
}

ilu_boolean
ilu_CloseMooring(ilu_Mooring self,
		 ilu_integer * dfd,
		 ILU_ERRS((bad_locks, broken_locks,
			   internal)) * err)
{
  ilu_boolean     ans;
  if (!self->mo_wc)
    return (*self->mo_close) (self, dfd, err);
  if (!ilu_EnterMutex(ilu_cmu, err))
    return ilu_FALSE;
  ans = (*self->mo_close) (self, dfd, err);
  if (!ilu_ExitMutex(ilu_cmu, ilu_TRUE, err))
    return ilu_FALSE;
  return ans;
}

/*
 * transport_read_bytes attempts to read the number of bytes desired
 * from the specified transport into the specified buffer.  It
 * returns the number of bytes actually read.  The number actually
 * read may be less than requested if some error occurred during the
 * read, e.g. end of file, in which case p_error will be set
 * appropriately and the number of bytes actually read into the
 * buffer will be returned immediately.
 */

ilu_cardinal
_ilu_transportReadBytes(ilu_Transport bs,
			ilu_bytes pc_buffer,
			ilu_cardinal card_num_desired,
			ILU_ERRS((IoErrs)) * p_error)
{
  ilu_cardinal    card_total_read = 0;
  ilu_cardinal    card_num_read;
  ilu_TransportReport transport_report;
  ilu_boolean     ok;

  ILU_CLER(*p_error);

  /* get what we can out of our buffer */
  if (bs->tr_inBuff != NIL) {
    card_total_read = MIN(card_num_desired,
			  bs->tr_inLimit - bs->tr_inNext);
    memcpy((void *) pc_buffer,
	   (void *) (bs->tr_inBuff + bs->tr_inNext),
	   card_total_read);
    bs->tr_inNext += card_total_read;
    pc_buffer += card_total_read;
  }
  /* if we got all we needed from the buffer, just give em back */
  if (card_total_read == card_num_desired)
    return card_total_read;

  while (1) {

    /* read some */
    card_num_read = ((*bs->tr_class->tc_read_bytes)
		     (bs, pc_buffer, card_num_desired - card_total_read,
		      &transport_report, p_error));

    /* adjust to account for what we read */
    pc_buffer += card_num_read;
    card_total_read += card_num_read;

    /* had a problem, just return count of what we got */
    if (ILU_ERRNOK(*p_error))
      return card_total_read;

    /* got all that was desired, just return the count */
    if (card_total_read == card_num_desired)
      return card_total_read;

    /*
     * if we hit end of file or end of message, set the error
     * accordingly, and and return how many we got before the error
     * happened
     */
    if (transport_report.tr_eof)
      return ILU_ERR_CONS1(comm_failure, p_error, minor, ilu_cfm_eof,
			   card_total_read);
    else if (transport_report.tr_eom)
      return ILU_ERR_CONS1(marshal, p_error, minor, ilu_mm_eom,
			   card_total_read);

    /*
     * else wait until there's some more available to read, then
     * loop
     */
    ok = _ilu_TransportWaitForInputNoClose(bs, NIL, p_error);
    if (!ok)
      return card_total_read;
  }
}


ilu_bytes
_ilu_transportGetInputBuffer(ilu_Transport self,
			     ilu_cardinal len,
			     ILU_ERRS((IoErrs)) * err)
{
  if (len > 16)
    return ILU_ERR_CONS1(internal, err, minor, ilu_im_trBufSize, NIL);
  if ((void) transport_read_bytes(self, self->tr_tinBuff, len, err),
      ILU_ERRNOK(*err))
    return NIL;
  ILU_CLER(*err);
  return self->tr_tinBuff;
}

ilu_cardinal
_ilu_transportReadUpToBytes(ilu_Transport self,
			    ilu_bytes buf,
			    ilu_cardinal len,
			    ilu_TransportReport * rpt,
			    ILU_ERRS((IoErrs)) * err)
{
  ilu_cardinal    l1 = 0, l2;
  ILU_CLER(*err);
  rpt->tr_eom = rpt->tr_eof = ilu_FALSE;
  if (self->tr_inBuff != NIL) {
    l1 = MIN(len, self->tr_inLimit - self->tr_inNext);
    memcpy((void *) buf, (void *) (self->tr_inBuff + self->tr_inNext),
	   l1);
    self->tr_inNext += l1;
    buf += l1;
  }
  if (l1 < len) {
    ilu_cardinal    lreq = len - l1;
    l2 = (*self->tr_class->tc_read_bytes) (self, buf, lreq, rpt, err);
    l1 += l2;
    buf += l2;
    if (ILU_ERRNOK(*err))
      return l1;
  }
  return l1;
}

extern          ilu_boolean
_ilu_transportReadMessage(ilu_Transport self,
			  ilu_bytes * msg,
			  ilu_cardinal * msgLen,
			  ilu_TransportReport * rpt,
			  ILU_ERRS((IoErrs)) * err)
{
  ilu_cardinal    nread, pos = 0, len = 4096;
  ilu_bytes       buf = NIL;
  rpt->tr_eom = rpt->tr_eof = ilu_FALSE;
  ILU_NOTE(CONNECTION_DEBUG,
	("_ilu_transportReadMessage:"
	 "  reading message from transport <%p>\n",
	 self));
  while (!(rpt->tr_eom || rpt->tr_eof)) {
    if (buf) {
      len *= 2;
      buf = ilu_realloc(buf, len);
    } else
      buf = ilu_malloc(len);
    if (buf == NIL)
      return ILU_ERR_CONS1(no_memory, err, nbytes, len, ilu_FALSE);
    if (_ilu_TransportWaitForInputNoClose(self, NIL, err),
	ILU_ERRNOK(*err))
      return ilu_FALSE;
    nread = transport_read_upto_bytes(self, buf + pos, len - pos,
				      rpt, err);
    ILU_NOTE(CONNECTION_DEBUG,
	  ("_ilu_transportReadMessage:  read %lu bytes, eof=%s, eom=%s\n",
	   (unsigned long) nread, rpt->tr_eof ? "true" : "false",
	   rpt->tr_eom ? "true" : "false"));
    if (ILU_ERRNOK(*err)) {
      ilu_free(buf);
      return ilu_FALSE;
    }
    pos += nread;
  }

  ILU_NOTE(CONNECTION_DEBUG,
	("_ilu_transportReadMessage:  returning buf %p of %lu bytes\n",
	 buf, (unsigned long) pos));
  *msg = buf;
  *msgLen = pos;
  return ilu_TRUE;
}

ilu_boolean 
_ilu_TransportWaitForInputNoClose(ilu_Transport self ,
				  ilu_FineTime * limit  ,
				  ILU_ERRS((bad_locks, broken_locks,
				       interrupted)) * err )
{
  int             disabled = 1;
  ilu_boolean     domore = ilu_TRUE;
  while (disabled && domore) {
    if (!ilu_EnterMutex(ilu_cmu, err))
      return ilu_FALSE;
    if (self->tr_wc) {
      ilu_WaitCohort  wc = self->tr_wc;
      while (wc->iluwc_waitsDisabled) {
	if (!ilu_CMWait1(wc->iluwc_change, ilu_cmu, err))
	  return ilu_FALSE;
      }
    }
    if (!ilu_ExitMutex(ilu_cmu, ilu_TRUE, err))
      return ilu_FALSE;
    domore = transport_wait_for_input(self, &disabled, limit, err);
  }
  return ILU_ERROK(*err);
}

/*L1, L2 unconstrained*/

ilu_boolean
_ilu_CompareTinfo(ilu_TransportInfo t1, ilu_TransportInfo t2)
{
  int             i;
  for (i = 0; t1[i] != NIL && t2[i] != NIL; i++)
    if (strcmp(t1[i], t2[i]) != 0)
      return ilu_FALSE;
  return (t1[i] == NIL && t2[i] == NIL);
}

ilu_TransportInfo
ilu_CopyTinfo(ilu_TransportInfo tinfo, ILU_ERRS((no_memory)) * err)
{
  int             count;
  int             i, sz = 0;
  ilu_string      s;
  ilu_TransportInfo ans;

  for (s = tinfo[count = 0]; s != NIL; s = tinfo[++count])
    sz += strlen(s) + 1;
  sz += (sizeof(ilu_string) * (count + 1));
  if ((ans = (ilu_TransportInfo) ilu_MallocE(sz, err)) == NIL)
    return NIL;
  s = (char *) (ans + count + 1);
  for (i = 0; i < count; i++) {
    ans[i] = s;
    strcpy(ans[i], tinfo[i]);
    s += strlen(tinfo[i]) + 1;
  }
  ans[count] = NIL;
  ILU_CLER(*err);
  return (ans);
}

ilu_TransportInfo _ilu_CopyTinfo(ilu_TransportInfo ti,
				 ILU_ERRS((no_memory)) * err)
{ return ilu_CopyTinfo(ti, err); }

void ilu_TransportInfo_Free(ilu_TransportInfo ti)
{ ilu_free(ti); }

ilu_TransportInfo
_ilu_ConcatTinfo(ilu_string info, ilu_TransportInfo tinfo,
		 ILU_ERRS((no_memory)) * err)
{
  ilu_cardinal    len, count;
  ilu_TransportInfo ans;
  ilu_string      p;

  for (len = strlen(info) + 1, count = 0; tinfo[count] != NIL; count++)
    len += (strlen(tinfo[count]) + 1);
  len += (sizeof(ilu_string) * (count + 2));
  if ((ans = (ilu_TransportInfo) ilu_MallocE(len, err)) == NIL)
    return NIL;
  ans[0] = p = (char *) (ans + count + 2);
  strcpy(ans[0], info);
  p += strlen(info) + 1;
  for (len = 0; tinfo[len] != NIL; len++) {
    ans[len + 1] = p;
    strcpy(p, tinfo[len]);
    p += strlen(tinfo[len]) + 1;
  }
  ans[len + 1] = NIL;
  ILU_CLER(*err);
  return ans;
}

ilu_cardinal _ilu_TinfoStringLength(ilu_TransportInfo tinfo)
{
  int             i, len = 0;
  for (i = 0; tinfo[i] != NIL; i++)
    len += strlen(tinfo[i]) + (i > 0);
  return len;
}

ilu_string
_ilu_StringifyTinfoToBuffer(ilu_TransportInfo tinfo,
			    ilu_string b, ilu_cardinal blen,
			    ILU_ERRS((internal / check)) * err)
{
  int             i;
  ilu_string      p = b, q = b + blen, r;
  for (i = 0; tinfo[i] != NIL; i++) {
    if (i) {
      if (!ilu_Check(p < q, err))
	return NIL;
      *p++ = ILU_TINFO_DIVIDER;
    }
    r = p + strlen(tinfo[i]);
    if (!ilu_Check(r <= q, err))
      return NIL;
    strcpy(p, tinfo[i]);
    p = r;
  }
  return (p);
}

ilu_string
_ilu_StringifyTinfo(ilu_TransportInfo tinfo,
		    ILU_ERRS((no_memory, internal / check)) * err)
{
  int             len;
  ilu_string      ans, p;

  len = 1 + _ilu_TinfoStringLength(tinfo);
  if (!(ans = (char *) ilu_MallocE(len, err)))
    return NIL;
  if ((p = _ilu_StringifyTinfoToBuffer(tinfo, ans, len - 1, err))) {
    *p = 0;
    return ans;
  } else {
    ilu_free(ans);
    return NIL;
  }
}

ilu_string
ilu_TransportInfo_Stringify(ilu_TransportInfo tinfo,
			    ILU_ERRS((no_memory, internal/check)) *err)
{
  return _ilu_StringifyTinfo(tinfo, err);
}

#ifndef ILU_DEFAULT_TRANSPORT_INFO
#define ILU_DEFAULT_TRANSPORT_INFO "sunrpcrm", "tcp_0_0"
#endif

ilu_TransportInfo
  ilu_DefaultTransportInfo (void)
{
  static ilu_string tinfo[] = { ILU_DEFAULT_TRANSPORT_INFO , NIL };

  return (ilu_TransportInfo) tinfo;
}

ilu_TransportInfo
  ilu_LocalTransportInfo (void)
{
  static ilu_string tinfo[] = { "inmem" , NIL };

  return (ilu_TransportInfo) tinfo;
}

#ifdef ENABLE_DEBUGGING
void _ilu_PrintTinfo(ilu_TransportInfo tinfo)
{
  int             i;

  for (i = 0; tinfo[i] != NIL; i++) {
    ilu_DebugPrintf("<%s>", tinfo[i]);
  }
}
#endif

/*L1 >= {cmu}; L2 unconstrained*/
void _ilu_CloseTCR(ilu_TransportCreator tcr)
{
  tcr->tcr_wantClose = ilu_TRUE;
  _ilu_DHoldsTCR(tcr, 0);
}

/*L1 >= {cmu}; L2 unconstrained*/
void _ilu_DHoldsTCR(ilu_TransportCreator tcr, int dholds)
{
  int             nuholds = tcr->tcr_holds + dholds;
  _ilu_Assert(tcr->tcr_holds >= 0 && nuholds >= 0, "_ilu_DHoldsTCR");
  tcr->tcr_holds = nuholds;
  if (tcr->tcr_wantClose && !tcr->tcr_holds)
    (*tcr->tcr_close) (tcr);
  return;
}

