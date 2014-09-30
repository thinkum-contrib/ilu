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
/* $Id: simptran.c,v 1.13 1999/08/03 01:53:12 janssen Exp $ */
/* Last edited by Mike Spreitzer May 22, 1998 8:31 am PDT */

#include "iluntrnl.h"

#include "ilutransport.h"
#include "mooring.h"

#include "oscalls.h"

#define BUFFERSIZE	1024

typedef struct {
  ilu_TransportCreator	lower;
  ilu_boolean		lower_boundaried;
  ilu_boolean		boundaried;
}              *SimptranCreatorParms;

typedef struct {
  ilu_Mooring		lower;
  ilu_boolean		lower_boundaried;
  ilu_boolean		boundaried;
}              *SimptranMooringParms;

typedef struct {

  /* L2 >= {ymu} */

  ilu_boolean	busyIn;	/* currently processing input msg? */

  /* L2 >= {xmu} */

  ilu_boolean	busyOut;

  /* L2 unconstrained */

  ilu_Transport	lower;
  ilu_boolean	lower_boundaried;

  /*
   * The unboundaried transport from which (self) is constructed. We
   * don't want an exposed buffer in (lower).  lower's xmu = self's
   * xmu; same for ymu.
   */
}              *SimptranParms;
/* What goes in the data field of a ilu_Transport. */

/*L1, L2 unconstrained*/

#define ACCESS_SIMPTRAN_PARMS(a) ((SimptranParms)(a))

/*L1.sup < trmu; L2 unconstrained*/
static          SimptranCreatorParms
_simptran_InterpretInfo(ilu_TransportInfo info,
			ILU_ERRS((no_memory, inv_objref)) * err)
{
  SimptranCreatorParms    cp;
  ilu_TransportCreator lower;
  ilu_boolean boundaried = ilu_FALSE;

  if (info[0] == NIL || info[1] == NIL)
    /* We check info[1] because "simptran" is not a transport
       endpoint */
    return ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_ts, NIL);
  if (strncmp(info[0], "simptran_boundaried", 19) == 0)
    boundaried = ilu_TRUE;
  else if (strncmp(info[0], "simptran_nonboundaried", 22) == 0)
    boundaried = ilu_FALSE;
  else if (strncmp(info[0], "simptran", 8) == 0)
    boundaried = ilu_FALSE;
  else
    return ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_ts, NIL);
  lower = _ilu_GetTransportCreator(info + 1, err);
  if (ILU_ERRNOK(*err))
    return NIL;
  if (!lower->tcr_reliable)
    return ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_ts, NIL);
  cp = (SimptranCreatorParms) ilu_MallocE(sizeof(*cp), err);
  if (cp == NIL)
    return NIL;
  cp->lower = lower;
  cp->lower_boundaried = lower->tcr_boundaried;
  cp->boundaried = boundaried;
  return cp;
}

/* L1.sup < trmu; L2 >= {xmu, ymu} */
static          ilu_boolean
_simptran_SetInputHandler(ilu_Transport self, ilu_TIH tih,
			  ILU_ERRS((no_memory, internal, no_resources)) * err)
{
  SimptranParms   p = ACCESS_SIMPTRAN_PARMS(transport_data(self));
  return _ilu_SetTransportInputHandler(p->lower, tih, err);
}

/* Main Invariant holds; L2 >= {ymu} */
static          ilu_boolean
_simptran_WaitForInput(ilu_Transport self, int *disabled,
		       ilu_FineTime * limit,
		       ILU_ERRS((broken_locks, interrupted)) * err)
{
  SimptranParms   p = ACCESS_SIMPTRAN_PARMS(transport_data(self));
  return (transport_wait_for_input(p->lower, disabled, limit, err));
}

/* L1.sup < trmu; L2 >= {xmu} */
static          ilu_boolean
_simptran_InterruptST(ilu_Transport self, ILU_ERRS((bad_param)) * err)
{
  SimptranParms   p = ACCESS_SIMPTRAN_PARMS(transport_data(self));
  return (transport_interruptST(p->lower, err));
}

/* L1 >= {cmu}; L1.sup < trmu; L2 >= {xmu} */

static          ilu_boolean
_simptran_DisableWait(ilu_Transport self,
		      ILU_ERRS((broken_locks, bad_param,
				internal)) * err)
{
  SimptranParms   p = ACCESS_SIMPTRAN_PARMS(transport_data(self));
  return (transport_disableWait(p->lower, err));
}

static          ilu_boolean
_simptran_EnableWait(ilu_Transport self,
		     ILU_ERRS((broken_locks, bad_param,
			       internal)) * err)
{
  SimptranParms   p = ACCESS_SIMPTRAN_PARMS(transport_data(self));
  return (transport_enableWait(p->lower, err));
}

/* L1.sup < trmu; L2 unconstrained */

static          ilu_integer
_simptran_FdUsage(ilu_TransportCreator self, ilu_boolean mooring)
{
  SimptranCreatorParms    cp = (SimptranCreatorParms) self->tcr_data;
  return (*cp->lower->tcr_dfd) (cp->lower, mooring);
}

/* L1.sup < trmu; L2 >= {xmu, ymu} */
static          ilu_integer
_simptran_CloseDFd(ilu_Transport self)
{
  SimptranParms   p = ACCESS_SIMPTRAN_PARMS(transport_data(self));
  return ((*p->lower->tr_class->tc_closeDFd) (p->lower));
}

/* Main Invariant holds; L2 >= {xmu}; input => L2 >= {ymu} */

static          ilu_ReadHeaderResultCode
_simptran_BeginMessage(ilu_Transport self,
		       ilu_boolean input_p,
		       ILU_ERRS((IoErrs)) * err)
{
  if (self->tr_class->tc_boundaried) {

    SimptranParms   p = ACCESS_SIMPTRAN_PARMS(transport_data(self));
    ilu_ReadHeaderResultCode result;

    if (p->busyIn || p->busyOut)
      return ILU_ERR_CONS1(internal, err, minor, ilu_im_beginMessage,
			   ilu_rhrc_error);
    if (p->lower_boundaried) {
      result = transport_begin_message(p->lower, input_p, err);
      if (result != ilu_rhrc_ok) return result;
    }
    if (input_p) {
      p->busyIn = ilu_TRUE;
    } else {
      p->busyOut = ilu_TRUE;
      self->tr_outLimit = BUFFERSIZE;
      self->tr_outNext = 0;
    }
    ILU_CLER(*err);
    return ilu_rhrc_ok;
  } else {
    return ILU_ERR_CONS1(internal, err, minor, ilu_im_tcNotBoundaried,
			 ilu_rhrc_error);
  }
}

static          ilu_boolean
_simptran_EndMessage(ilu_Transport self,
		     ilu_boolean flush,
		     ilu_Message * msgh,
		     ILU_ERRS((IoErrs)) * err)
{
  if (self->tr_class->tc_boundaried) {

    register SimptranParms p = ACCESS_SIMPTRAN_PARMS(transport_data(self));
    ilu_boolean result;

    if ((!p->busyOut) && (!p->busyIn))
      return ILU_ERR_CONS1(internal, err, minor, ilu_im_endMessage,
			   ilu_FALSE);

    if (p->busyOut) {
      if (self->tr_outNext > 0) {
	if (!(p->lower->tr_class->tc_write_bytes(p->lower, self->tr_outBuff, self->tr_outNext, flush, err)))
	  return ilu_FALSE;
      }
      self->tr_outNext = 0;
      p->busyOut = ilu_FALSE;
    } else if (p->busyIn) {
      p->busyIn = ilu_FALSE;
    };
    if (p->lower_boundaried) {
      result = transport_end_message(p->lower, flush, msgh, err);
      if (ILU_ERRNOK(*err)) return result;
    }
    ILU_CLER(*err);
    return result;
  } else {
    return ILU_ERR_CONS1(internal, err, minor, ilu_im_tcNotBoundaried,
			 ilu_rhrc_error);
  }
}

/*Main Invariant holds; L2 >= {xmu}*/

static          ilu_boolean
_simptran_Push(ilu_Transport self,
	       ILU_ERRS((IoErrs)) * err)
{
  SimptranParms   p = ACCESS_SIMPTRAN_PARMS(transport_data(self));
  ilu_Transport   lower = p->lower;
  return (*lower->tr_class->tc_push) (lower, err);
}

static          ilu_boolean
_simptran_SendWholeMessage(ilu_Transport self, ilu_Message * msgh,
			   ILU_ERRS((IoErrs)) * err)
{
  return ILU_ERR_CONS1(internal, err, minor, ilu_im_tcReliable, ilu_FALSE);
}

static          ilu_boolean
_simptran_WriteBytes(ilu_Transport self, ilu_bytes b,
		     ilu_cardinal bufferSize,
		     ilu_boolean flush,
		     ILU_ERRS((IoErrs)) * err)
{
  SimptranParms   p = ACCESS_SIMPTRAN_PARMS(transport_data(self));

  if (self->tr_class->tc_boundaried && (!p->busyOut))
    return ILU_ERR_CONS1(internal, err, minor, ilu_im_bytesWithoutMsg,
			 ilu_FALSE);

  if (self->tr_outNext > 0) {
    if (!(p->lower->tr_class->tc_write_bytes(p->lower, self->tr_outBuff, self->tr_outNext, ilu_FALSE, err)))
      return ilu_FALSE;
    self->tr_outNext = 0;
  };

  /* just send the bytes to the next layer... */
  return (p->lower->tr_class->tc_write_bytes(p->lower, b, bufferSize, flush, err));
}

/*Main Invariant holds; L2 >= {xmu, ymu}*/

static          ilu_cardinal
_simptran_ReadBytes(ilu_Transport self,
		    ilu_bytes buffer,
		    ilu_cardinal len,
		    ilu_TransportReport * rpt,
		    ILU_ERRS((IoErrs)) * err)
{
  SimptranParms   p = ACCESS_SIMPTRAN_PARMS(transport_data(self));
  int             progress;
  if (self->tr_class->tc_boundaried && (!p->busyIn))
    return ILU_ERR_CONS1(internal, err, minor, ilu_im_bytesWithoutMsg,
			 0);

  /* just get the bytes from the next layer... */
  return transport_read_upto_bytes(p->lower, buffer, len, rpt, err);
}

/*L1.sup < trmu; self->tr_wc => L1 >= {cmu}; L2 >= {xmu, ymu}*/

static          ilu_boolean
_simptran_Close(ilu_Transport self, ilu_integer * dfd,
		ILU_ERRS((bad_locks, broken_locks, internal)) * err)
{
  SimptranParms   p = ACCESS_SIMPTRAN_PARMS(transport_data(self));
  ilu_Transport   lt = p->lower;
  ilu_free(self->tr_inBuff);
  ilu_free(self->tr_outBuff);
  ilu_free(p);
  ilu_free(self);
  return transport_close(lt, dfd, err);
}

/*L1, L2 unconstrained*/

static struct _ilu_TransportClass_s myclass = {
  ilu_TRUE,			/* boundaried */
  ilu_TRUE,			/* reliable */
  _simptran_CloseDFd,
  _simptran_SetInputHandler,
  _simptran_WaitForInput,
  _simptran_InterruptST,
  _simptran_DisableWait,
  _simptran_EnableWait,
  _simptran_BeginMessage,
  _simptran_EndMessage,
  _simptran_Push,
  _simptran_SendWholeMessage,
  _simptran_WriteBytes,
  _simptran_ReadBytes,
  _simptran_Close
};

static ilu_TransportClass
  NewClass (ilu_boolean boundaried, ilu_Error *err)
{
  ilu_TransportClass n;

  if ((n = (ilu_TransportClass) ilu_MallocE(sizeof(*n), err)) == NIL)
    return NIL;
  *n = myclass;
  n->tc_boundaried = boundaried;
  return n;
}

/*L1.sup < cmu; L2 >= result's {xmu, ymu}*/
static          ilu_Transport
NewTrans(ilu_Transport lower, ilu_boolean boundaried,
	 ilu_boolean lower_boundaried, ilu_integer * dfd,
	 ILU_ERRS((no_memory, bad_param)) * err)
{
  ilu_Transport   ans = NIL;
  SimptranParms   parms = NIL;

  if (boundaried && !lower_boundaried)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, NIL);
  parms = (SimptranParms) ilu_MallocE(sizeof(*parms), err);
  if (parms == NIL)
    return NIL;
  parms->lower = lower;
  parms->lower_boundaried = lower_boundaried;
  parms->busyIn = ilu_FALSE;
  parms->busyOut = ilu_FALSE;
  ans = (ilu_Transport) ilu_MallocE(sizeof(*ans), err);
  if (ans == NIL)
    goto fale1;
  ans->tr_outBuff = (unsigned char *) ilu_MallocE(BUFFERSIZE, err);
  if (ans->tr_outBuff == NIL) { goto fale2; };
  ans->tr_inBuff = NIL;
  ans->tr_inNext = ans->tr_inLimit = 0;
  ans->tr_outNext = 0;
  ans->tr_outLimit = BUFFERSIZE;
  if ((ans->tr_class = NewClass(boundaried, err)) == NIL)
    goto fale3;  
  ans->tr_data = parms;
  ans->tr_wc = lower->tr_wc;
  ILU_CLER(*err);
  return ans;
 fale3:
  ilu_free(ans->tr_outBuff);
 fale2:
  ilu_free(ans);
 fale1:{
   ILU_ERRS((bad_locks, broken_locks, internal)) cerr;
   ilu_integer     cdfd = 0;
   ilu_free(parms);
   transport_close(lower, &cdfd, &cerr);
   ILU_HANDLED(cerr);
   *dfd += cdfd;
   return NIL;
 }
}

/* Main Invariant holds */
static          ilu_Transport
_simptran_CreateTransport(ilu_TransportCreator self, ilu_boolean buffer,
			  ilu_integer *dfd, ilu_Passport pp,
			  ILU_ERRS((IoErrs)) * err)
{
  SimptranCreatorParms    cp = (SimptranCreatorParms) self->tcr_data;
  ilu_Transport   lower;
  lower = (*cp->lower->tcr_createTransport) (cp->lower, buffer, dfd, pp, err);
  if (ILU_ERRNOK(*err))
    return NIL;
  if (!transport_reliable(lower))
    /* He promised! (We checked earlier) */
    return ILU_ERR_CONS1(internal, err, minor, ilu_im_tcBug, NIL);
  /* pretend to acquire result's xmu, ymu */
  return NewTrans(lower, cp->boundaried, cp->lower_boundaried, dfd, err);
  /* pretend to release result's xmu, ymu */
}

/*L1.sup < trmu; L2 >= {xmu, ymu}*/
static ilu_integer
  _simptran_MooringDFd(ilu_Mooring self, ilu_boolean add)
{
  SimptranMooringParms mp = (SimptranMooringParms) self->mo_data;
  return ((*mp->lower->mo_dfd) (mp->lower, add));
}

/*L1.sup < trmu; L2 >= {xmu, ymu}*/
static          ilu_boolean
_simptran_SetReqHandler(ilu_Mooring self,
			ilu_TIH tih,
			ILU_ERRS((no_memory, imp_limit, no_resources,
				  broken_locks, internal)) * err)
{
  SimptranMooringParms mp = (SimptranMooringParms) self->mo_data;
  return ((*mp->lower->mo_set_req_handler) (mp->lower, tih, err));
}

/* Main Invariant holds; L2 >= {ymu} */
static          ilu_boolean
_simptran_WaitForReq(ilu_Mooring self, int *disabled,
	   ILU_ERRS((interrupted, broken_locks)) * err)
{
  SimptranMooringParms mp = (SimptranMooringParms) self->mo_data;
  return ((*mp->lower->mo_wait_for_req) (mp->lower, disabled, err));
}

/* L1 >= {cmu}; L1.sup < trmu; L2 >= {xmu} */

static          ilu_boolean
_simptran_DisableReqWait(ilu_Mooring self,
	       ILU_ERRS((broken_locks, bad_param, internal)) * err)
{
  SimptranMooringParms mp = (SimptranMooringParms) self->mo_data;
  return ((*mp->lower->mo_disableWait) (mp->lower, err));
}

static          ilu_boolean
_simptran_EnableReqWait(ilu_Mooring self,
	       ILU_ERRS((broken_locks, bad_param, internal)) * err)
{
  SimptranMooringParms mp = (SimptranMooringParms) self->mo_data;
  return ((*mp->lower->mo_enableWait) (mp->lower, err));
}

/* Main Invariant holds; L2 >= self's {xmu, ymu} */

static          ilu_Transport
_simptran_AcceptClient(ilu_Mooring self, ilu_string * tinfo_out,
		       ilu_integer *dfd, ilu_Passport pp,
		       ILU_ERRS((IoErrs)) * err)
{
  SimptranMooringParms mp = (SimptranMooringParms) self->mo_data;
  ilu_Transport   lower;
  ilu_string      subtinfo = NIL;
  ilu_Transport   ans = NIL;
  lower = ((*mp->lower->mo_accept_connection)
	   (mp->lower, tinfo_out ? &subtinfo : NIL, dfd, pp, err));
  if (ILU_ERRNOK(*err) || lower == NIL)
    goto dun;
  if (tinfo_out) {
    *tinfo_out = ilu_Strcat3E("simptran", " over ", subtinfo, err);
    if (ILU_ERRNOK(*err))
      goto dun;
  }
  ans = NewTrans(lower, mp->boundaried, lower->tr_class->tc_boundaried, dfd, err);
dun:
  if (subtinfo)
    ilu_free(subtinfo);
  return ans;
}

/*L1.sup < trmu; L1 >= {cmu}; L2 >= {xmu, ymu}*/
static          ilu_boolean
_simptran_CloseMooring(ilu_Mooring self, ilu_integer * dfd,
		       ILU_ERRS((bad_locks, broken_locks,
				 internal)) * err)
{
  SimptranMooringParms mp = (SimptranMooringParms) self->mo_data;
  if (!(*mp->lower->mo_close) (mp->lower, dfd, err))
    return ilu_FALSE;
  ilu_free(mp);
  ilu_free(self);
  return ilu_TRUE;
}

/*L1, L2 unconstrained*/

static struct _ilu_Mooring_s mooringProto = {
  _simptran_MooringDFd,
  _simptran_SetReqHandler,
  _simptran_WaitForReq,
  _simptran_DisableReqWait,
  _simptran_EnableReqWait,
  _simptran_AcceptClient,
  _simptran_CloseMooring,
  NIL				/* data */
};

/*L1.sup < trmu*/
static          ilu_Mooring
_simptran_CreateMooring(ilu_TransportCreator self,
			ilu_TransportInfo * tinfo_out,
			ilu_boolean buffer,
			ilu_integer *dfd,
			ilu_Passport pp,	/* unused here */
			ILU_ERRS((no_memory)) * err)
{
  SimptranCreatorParms	cp = (SimptranCreatorParms) self->tcr_data;
  SimptranMooringParms	mp;
  ilu_Mooring     lower, ans;
  ilu_TransportInfo      subtinfo = NIL;
  lower = ((*cp->lower->tcr_createMooring)
	   (cp->lower, tinfo_out ? &subtinfo : NIL, buffer, dfd, pp, err));
  if (ILU_ERRNOK(*err))
    return NIL;
  ans = (ilu_Mooring) ilu_MallocE(sizeof(*ans), err);
  if (ans == NIL)
    return NIL;
  mp = (SimptranMooringParms) ilu_MallocE(sizeof(*mp), err);
  if (mp == NIL) { ilu_free(ans); return NIL; };
  if (tinfo_out) {
    *tinfo_out = _ilu_ConcatTinfo ("simptran", subtinfo, err);
    if (ILU_ERRNOK(*err))
      return NIL;
    else
      ilu_free(subtinfo);
  }
  *ans = mooringProto;
  mp->lower = lower;
  mp->lower_boundaried = cp->lower_boundaried;
  mp->boundaried = cp->boundaried;
  ans->mo_data = mp;
  ans->mo_wc = lower->mo_wc;
  return (ans);
}

static void _simptran_CloseCreator(ilu_TransportCreator self)
{
  SimptranCreatorParms    cp = (SimptranCreatorParms) self->tcr_data;
  (*cp->lower->tcr_close) (cp->lower);
  ilu_free(cp);
  ilu_free(self);
  return;
}

static struct _ilu_TransportCreator_s myCreatorProto = {
  ilu_TRUE,				/* boundaried */
  ilu_TRUE,				/* reliable */
  0,				/* tcr_holds */
  ilu_FALSE,			/* tcr_wantClose */
  _simptran_FdUsage,
  _simptran_CreateTransport,
  _simptran_CreateMooring,
  _simptran_CloseCreator,
  NIL				/* data */
};

/*L1.sup < trmu*/
ilu_TransportCreator
_ilu_simptran_TransportCreator(ilu_TransportInfo tinfo,
			       ILU_ERRS((no_memory,
					 inv_objref)) * err)
{
  ilu_TransportCreator ans;
  SimptranCreatorParms    cp;
  cp = _simptran_InterpretInfo(tinfo, err);
  if (ILU_ERRNOK(*err))
    return NIL;
  ans = (ilu_TransportCreator) ilu_MallocE(sizeof(*ans), err);
  if (ans == NIL)
    return NIL;
  *ans = myCreatorProto;
  ans->tcr_data = cp;
  ans->tcr_boundaried = cp->boundaried;
  ILU_CLER(*err);
  return ans;
}
