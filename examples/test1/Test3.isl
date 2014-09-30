(** $Id: Test3.isl,v 1.5 1999/08/03 01:52:10 janssen Exp $
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

INTERFACE Test3 IMPORTS Test1, Test2 END;

EXCEPTION E1: ilu.CString;

TYPE O = OBJECT SUPERTYPES Test2.T1O3, Test2.P END
  METHODS
    I-to-Test1U (i: INTEGER): Test2.T1U RAISES E1, Test1.E1 END
  END;

TYPE T2T1U3 = Test2.T1U3;
TYPE T2T1O3 = Test2.T1O3;

TYPE FU = Test2.F UNION O = ev1 END, INTEGER = ev3 END END;
