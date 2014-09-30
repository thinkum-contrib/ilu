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
/* $Id: port.h,v 1.14 1999/08/03 01:52:49 janssen Exp $ */
/* Last tweaked by Mike Spreitzer March 28, 1997 2:48 pm PST */

#define port_mooring(port)		((port)->po_mooring)
#define port_protocol(port)		((port)->po_protocol)
#define port_protocol_data(port)	((port)->po_prot_data)
#define port_server(port)		((port)->po_server)
#define port_tinfo(port)		((port)->po_tinfo)
#define port_pinfo(port)		((port)->po_pinfo)
#define port_transport_creator(port)	((port)->po_tcr)
#define port_closed(port)		((port)->po_closed != ilu_FALSE)
#define port_next(port)			((port)->po_links.pl_next)
#define port_connections(port)		((port)->po_connHead.next)
