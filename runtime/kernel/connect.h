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
/* $Id: connect.h,v 1.17 1999/08/03 01:52:44 janssen Exp $ */
/* Last edited by Mike Spreitzer October 22, 1996 12:34 pm PDT */

#define connection_transport(c)		((c)->co_transport)
#define connection_protocol(c)		((c)->co_protocol)
#define connection_protocol_data(c)	((c)->co_protocol_data)
#define connection_port(c)		((c)->co_port)
#define connection_server(c)		((c)->co_server)
#define connection_closed(c)		((c)->co_closed != ilu_FALSE)
#define connection_closish(c)		((c)->co_closed || (c)->co_closing)
#define connection_cc(c)		((c)->co_cc)
#define connection_next(c)		((c)->co_links[ilu_psl].next)
#define connection_concurrent(c)	protocol_concurrent((c)->co_protocol)
#define connection_incoming(c)		((c)->co_port != NIL)
#define connection_pinfo(c)		((c)->co_pinfo)

#define conn_tinfo(c)			((c)->co_tinfo.co_tinfo)
#define conn_peerinfo(c)		((c)->co_tinfo.co_peerinfo)
