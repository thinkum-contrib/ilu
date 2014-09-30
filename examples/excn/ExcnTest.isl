(** $Id: ExcnTest.isl,v 1.6 1999/08/03 01:58:53 janssen Exp $
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
(* Last edited by Mike Spreitzer October 8, 1998 10:48 pm PDT *)

INTERFACE ExcnTest BRAND "excntest.examples.ilu.parc.xerox.com";

TYPE I = INTEGER;
TYPE IS = SEQUENCE OF INTEGER;
TYPE SC = SHORT CHARACTER;
TYPE C = CHARACTER;
TYPE ScS = SEQUENCE OF SC;
TYPE CSS = SEQUENCE OF ScS;
TYPE A0 = ARRAY OF 8 BYTE;
TYPE A1 = ARRAY OF 3, 4 SHORT CARDINAL;
TYPE BS = SEQUENCE OF BYTE;
TYPE R = RECORD a: A1, css: CSS, i: I END;
TYPE RS = SEQUENCE OF R;
TYPE RO = OPTIONAL R;
TYPE U1 = UNION R, A1 END;
TYPE TheE = ENUMERATION ev1, ev3 = 3, ev5, ev7 END;
TYPE E = TheE;
TYPE U2 = UNION
	x1 : BOOLEAN = 3, 7 END,
	x2 : O = 1, 22 END,
	x3 : CSS = 2 END
	END OTHERS;
TYPE ilu--prefix-idlExceptionType-E11 = RECORD reason : ilu.CString END;

EXCEPTION E1: U1;
EXCEPTION E2: INTEGER;
EXCEPTION CantCreate;
EXCEPTION E3: RO;
EXCEPTION E4: O;
EXCEPTION E5: A0;
EXCEPTION E6: RS;
EXCEPTION E7: ilu.CString;
EXCEPTION E8: A1;
EXCEPTION E9: R;
EXCEPTION E10: E;
EXCEPTION E11: ilu--prefix-idlExceptionType-E11;	(* fake CORBA-style exception *)
EXCEPTION NotUsed;

TYPE O = OBJECT SUPERTYPES ilu.CORBA-Object END	(* inherit from CORBA-Object so can use CosNaming *)
  METHODS
    throw-excn (which : SHORT CARDINAL) RAISES E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11 END
  END;
