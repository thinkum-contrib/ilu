/*  -*- Mode: C; -*-
 *
 * This code contributed by Siemens Corporate Research, Inc.
 */
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

#ifndef _ILUSRVR_SCM_H_
#define _ILUSRVR_SCM_H_

#include "ilu-type.h"

/* for internal use */
void iluguile_server_monitor_conn(ilu_Connection);
ilu_boolean iluguile_server_enable_requests(iluguile_SCMCall, ilu_Connection);
ilu_boolean iluguile_server_disable_requests(iluguile_SCMCall, ilu_Connection);

/* scheme interface */
SCM iluguile_server__add_port(SCM _ks, SCM _protocolType, SCM, SCM);
SCM iluguile_server__create(SCM _serviceID, SCM _objtab);
SCM iluguile_server__id(SCM _ks);

#endif
