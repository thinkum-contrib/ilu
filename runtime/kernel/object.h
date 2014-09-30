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
/* $Id: object.h,v 1.19 1999/08/03 01:52:47 janssen Exp $ */
/* Last tweaked by Mike Spreitzer July 8, 1997 11:04 pm PDT */

#define object_ih(o)			((o)->ob_ih)
#define object_server(o)		((o)->ob_server)
#define object_timeout(o)		((o)->ob_timeout)
#define object_class(o)			((o)->ob_class)
#define object_mstid(o)			((o)->ob_mstid)
#define object_sbh(o)			((o)->ob_sbh)
#define object_lspo(o,lang)		((o)->ob_lspos[(lang)])
#define object_lspos(o)			((o)->ob_lspos)
#define object_holds(o)			((o)->ob_holds)
#define object_intNoted(o)		((o)->ob_intNoted)
#define object_is_true(o)		server_is_true(o->ob_server)
#define object_collectible(o)		class_collectible((o)->ob_class)
#define object_is_singleton(o)		((o)->ob_class->cl_singleton != NIL)
#define object_singleton_info(o)	((o)->ob_class->cl_singleton)
#define object_local(o)			((o)->ob_class->cl_local)


/* Use the following only if object_is_true(o) is ilu_FALSE!!! */

#define object_notifying(o) \
	((o)->ob_collectibleInfo.ob_surrogate.ob_notifying)

#define object_known(o) \
	((o)->ob_collectibleInfo.ob_surrogate.ob_known)


/* Use the following only if object_is_true(o) is ilu_TRUE!!! */

#define object_gco(o) \
	((o)->ob_collectibleInfo.ob_true.ob_gco)

#define object_lastRemote(o) \
	((o)->ob_collectibleInfo.ob_true.ob_lastRemote)

#define object_gclist(o) \
	((o)->ob_collectibleInfo.ob_true.ob_gclist)
