(** $Id: Test2.isl,v 1.5 1999/08/03 01:52:09 janssen Exp $
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
(* Last edited by Mike Spreitzer October 9, 1998 11:23 am PDT *)

INTERFACE Test2 IMPORTS Test1 END;

TYPE F = Test1.E;

TYPE U2 = F UNION
	ilu.CString = ev1 END,
	Test1.O1 = ev3 END,
	P = ev7 END
	END;

TYPE T1U = Test1.U;
TYPE T1U2 = Test1.U2;
TYPE T1U3 = Test1.U3;

TYPE T1O3 = Test1.O3;

TYPE P = OBJECT METHODS 
	SR-to-I (i: SHORT REAL): INTEGER
	END;

EXCEPTION E1: T1U;
