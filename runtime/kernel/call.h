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
/* $Id: call.h,v 1.14 1999/08/03 01:52:43 janssen Exp $ */
/* Last tweaked by Mike Spreitzer February 6, 1996 11:13 am PST */

#define call_serial_number(call) ((call)->ca_SN)
#define call_server(call)	((call)->ca_server)
#define call_server_id(call)	(((call)->ca_server)->sr_id)
#define call_intro_type(call)	((call)->ca_intro_type)
#define call_method(call)	((call)->ca_method)
#define call_connection(call)	((call)->ca_connection)
#define call_proto(call)	connection_protocol((call)->ca_connection)
#define call_transport(call)	((call)->ca_prTrans)
#define call_singleton(call)	class_singleton((call)->ca_intro_type)
#define call_incoming(call)	((call)->ca_incoming)
#define call_connection_id(call)	((((call)->ca_connection != NIL)&&((call)->ca_connection->co_port != NIL))?((call)->ca_connection->co_tinfo.co_peerinfo):(((call)->ca_server != NIL)?((call)->ca_server->sr_id):"*"))
