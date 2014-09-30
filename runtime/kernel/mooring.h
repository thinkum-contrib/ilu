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
/* $Id: mooring.h,v 1.17 1999/08/03 01:53:10 janssen Exp $ */
/* Last edited by Mike Spreitzer November 20, 1996 9:16 am PST */


#define mooring_accept(m,peerinfo,dfd,err)\
	((*((m)->mo_accept_connection))(m,peerinfo,dfd,err)))

#define mooring_data(m)			((m)->mo_data)
#define mooring_close(m,dfd,err)	((*((m)->mo_close))(m,dfd,err))
#define mooring_disableWait(m,err)	((*((m)->mo_disableWait))(m,err))
#define mooring_enableWait(m,err)	((*((m)->mo_enableWait))(m,err))
#define mooring_set_req_handler(m,tih,err)	((*((m)->mo_set_req_handler))((m),(tih),(err)))
#define mooring_accept_connection(m,peerinfo,dfd,passport,err) ((*((m)->mo_accept_connection))((m),(peerinfo),(dfd),(passport),(err)))
#define mooring_dfd(m,add)		((*((m)->mo_dfd))((m),(add)))



