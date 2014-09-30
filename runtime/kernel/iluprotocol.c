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
/* $Id: iluprotocol.c,v 1.44 1999/08/03 01:53:21 janssen Exp $ */
/* Last edited by Mike Spreitzer April 30, 1998 11:45 am PDT */

#define _POSIX_SOURCE


#include "iluntrnl.h"
#include "ilutransport.h"
#include "iluprotocol.h"
#include "call.h"
#include "type.h"

#define MAX_PROTOCOLS	10	/* max number of protocols in registration table */

struct protocols_s {
  /*L1, L2, Main unconstrained*/
  
  ilu_string name;
  /*For calling: L1_sup < prmu, L2, Main unconstrained*/
  ilu_Protocol (*instantiator)(ilu_ProtocolInfo /* pinfo */,
			       ilu_Error * /* err */);
};

/*L1, L2, Main unconstrained*/

static struct protocols_s protocols[MAX_PROTOCOLS] = {
#ifdef SUNRPC_PROTOCOL
  { "sunrpc", _ilu_sunrpc_Protocol },
  { "bsunrpc", _ilu_bsunrpc_Protocol },
  { "csunrpc", _ilu_csunrpc_Protocol },
  { "bcsunrpc", _ilu_bcsunrpc_Protocol },
#endif /* SUNRPC_PROTOCOL */
#ifdef COURIER_PROTOCOL
  { "courier", _ilu_courier_Protocol },
#endif /* COURIER_PROTOCOL */
#ifdef IIOP_PROTOCOL
  { "iiop", _ilu_IIOP_Protocol },
  { "siiop", _ilu_IIOP_SerialProtocol },
#endif /* IIOP_PROTOCOL */
#ifdef W3NG_PROTOCOL
  { "w3ng", _ilu_w3ng_Protocol },
#endif /* W3NG_PROTOCOL */
#ifdef JAVARMI_PROTOCOL
  { "javarmi", _ilu_javarmi_Protocol },
#endif /* JAVARMI_PROTOCOL */
#ifdef HTTP_PROTOCOL
   { "http", _ilu_http_Protocol },
#endif
  { NIL, NULLFN } };

ILU_ERRS((ProtocolAlreadyRegistered, MaxCountExceeded))
ilu_RegisterProtocol(char *name, ilu_Protocol(*new_protocol) (ilu_ProtocolInfo, ilu_Error *),
		     ilu_boolean override)
{
  int             i;
  ilu_Error       e;

  for (i = 0; i < MAX_PROTOCOLS && protocols[i].name != NIL; i++) {
    if (strcmp(protocols[i].name, name) == 0) {
      if (override)
	protocols[i].instantiator = new_protocol;
      else
	return ILU_ERR_CONS3(ProtocolAlreadyRegistered, &e,
			     name, name,
			     old_protocol, protocols[i].instantiator,
			     new_protocol, new_protocol, e);
    }
  }
  if (i < MAX_PROTOCOLS && protocols[i].name == NIL) {
    protocols[i].name = name;
    protocols[i].instantiator = new_protocol;
    if ((i + 1) < MAX_PROTOCOLS)
      protocols[i + 1].name = NIL;
    return ILU_NO_ERR;
  } else
    return ILU_ERR_CONS1(MaxCountExceeded, &e,
			 max_count, MAX_PROTOCOLS, e);
}

static struct protocols_s *FindProtocol (ilu_string pinfo)
{
  char buf[1000];
  ilu_integer i;

  if (pinfo != NIL)
    {
      ilu_string p;

      p = strchr(pinfo, '_');
      if (p == NIL)
	strcpy (buf, pinfo);
      else
	{
	  strncpy (buf, pinfo, p - pinfo);
	  buf[p - pinfo] = '\0';
	}
      for (i = 0;  protocols[i].name != NIL;  i += 1)
	if (_ilu_casefree_cmp(buf, protocols[i].name) == 0)
	  return(&protocols[i]);
    }
  return (NIL);
}

/*L1_sup < prmu*/

ilu_Protocol _ilu_GetProtocolFromInfo (ilu_string pinfo)
{
  struct protocols_s *p;
  ilu_Error lerr;

  if ((p = FindProtocol(pinfo)) == NIL)
    return (NIL);
  else {
    ilu_Protocol pr = (*(p->instantiator))(pinfo, &lerr);
    if (ILU_ERRNOK(lerr)) {
      ilu_DebugPrintf("_ilu_GetProtocolFromInfo:  Can't find protocol with pinfo string <%p>!\n",
		      pinfo);
      ILU_HANDLED(lerr);
      return NIL;
    };
    ILU_HANDLED(lerr);
    return pr;
  }
}

#ifndef ILU_DEFAULT_PROTOCOL_INFO
#define ILU_DEFAULT_PROTOCOL_INFO "sunrpc"
#endif

ilu_ProtocolInfo
  ilu_DefaultProtocolInfo (void)
{
  return (ilu_ProtocolInfo) ILU_DEFAULT_PROTOCOL_INFO ;
}

