(** $Id: relocate.isl,v 1.4 1999/08/03 01:59:00 janssen Exp $
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
*)
(* Last edited by Mike Spreitzer October 9, 1998 10:58 am PDT *)

INTERFACE relocate;

TYPE ProtocolInfo = ilu.CString;
TYPE TransportInfo = SEQUENCE OF ilu.CString;

TYPE RelocationManager = OBJECT
  DOCUMENTATION "This object manages one or more 'dummy' servers, each with its \
		own port.  When a request arrives at that port, a real server \
		process for the server ID is started, and passed the cinfo it's \
		supposed to advertise, and the SBH of the RelocationManager, \
		as parameters.  The new server then calls back to the RelocationManager \
		via RegisterRealCinfo with its real cinfo.  The RelocationManager \
		then passes that back to the client."
  METHODS
    RegisterRealCinfo (server-id : ilu.CString, pinfo : ProtocolInfo, tinfo : TransportInfo)
  END;

TYPE Foo = OBJECT METHODS dummy () : Foo END;
