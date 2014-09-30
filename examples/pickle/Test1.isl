(** $Id: Test1.isl,v 1.9 1999/08/03 01:58:44 janssen Exp $
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
(* Last edited by Mike Spreitzer October 9, 1998 10:43 am PDT *)

INTERFACE Test1;

TYPE I = INTEGER;
TYPE IS = SEQUENCE OF INTEGER;
TYPE SC = SHORT CHARACTER;
TYPE C = CHARACTER;
TYPE ScS = SEQUENCE OF SC;
TYPE CSS = SEQUENCE OF ScS;
TYPE A0 = ARRAY OF 8 BYTE;
TYPE PS = SEQUENCE OF PICKLE;	(* suggested by Peter Phillips *)
TYPE TheA1 = ARRAY OF 3 ScS;
TYPE A1 = TheA1;
TYPE A2 = ARRAY OF 2, 2 INTEGER;
TYPE BS = SEQUENCE OF BYTE TYPEID "IDL:Test1/BS:1.0";
TYPE TheR = RECORD a: A1, css: CSS, i: I END;
TYPE R = TheR;
TYPE R2 = RECORD a : TheR, b : PICKLE END;
TYPE R3 = RECORD a : INTEGER, b : REAL END TYPEID "IDL:Test1/R3:1.0";
TYPE TheRS = SEQUENCE OF R;
TYPE RS = TheRS;
TYPE RO = OPTIONAL R;
TYPE TheOO = OPTIONAL O5;
TYPE OO = TheOO;
TYPE OO2 = BOOLEAN UNION O5 = TRUE END END OTHERS;
TYPE TheU = UNION R, RO, CSS, O5, OO, BOOLEAN END;
TYPE U = TheU;
TYPE U1 = UNION R, A1 END;
TYPE TheE = ENUMERATION ev1, ev3 = 3, ev5, ev7 END;
TYPE E = TheE;
TYPE U2 = UNION
	x1 : BOOLEAN = 3, 7 END,
	x2 : O5 = 1, 22 END,
	x3 : CSS = 2 END
	END OTHERS;
TYPE U3 = E UNION
	ilu.CString = ev1 END,
	U2 = ev3 END,
	v3: O5 = ev7 END,
	v4: RO = DEFAULT
	END;
TYPE U4 = SHORT INTEGER UNION
	x1 : BOOLEAN = 3, 7 END,
	x2 : O5 = DEFAULT,
	x3 : CSS = 1, 0 END
	END;
TYPE U-byte = BYTE UNION R, O2 END;
TYPE U-int = INTEGER UNION R, O5 END;
TYPE U-card = CARDINAL UNION R, O5 END;
TYPE U-scard = SHORT CARDINAL UNION R, O5 END;

EXCEPTION E1: U;
EXCEPTION E2: INTEGER;
EXCEPTION CantCreate;
EXCEPTION E3: RO;
EXCEPTION E4: O5;
EXCEPTION E5: A0;
EXCEPTION E6: RS;
EXCEPTION E7: ilu.CString;
EXCEPTION E8: A1;
EXCEPTION E9: R;

TYPE O2 = OBJECT;
TYPE O3 = OBJECT COLLECTIBLE;
TYPE O4 = OBJECT OPTIONAL;
TYPE O5 = OBJECT OPTIONAL SUPERTYPES ilu.CORBA-Object END TYPEID "IDL:Test1/O5:1.0";
TYPE O6 = OBJECT SUPERTYPES O2 END;
