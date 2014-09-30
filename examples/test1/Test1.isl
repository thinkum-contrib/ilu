(** $Id: Test1.isl,v 1.20 1999/08/03 01:52:10 janssen Exp $
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

INTERFACE Test1;

TYPE I = INTEGER;
TYPE IS = SEQUENCE OF INTEGER;
TYPE SC = SHORT CHARACTER;
TYPE C = CHARACTER;
TYPE ScS = SEQUENCE OF SC;
TYPE CSS = SEQUENCE OF ScS;
TYPE A0 = ARRAY OF 8 BYTE;
TYPE TheA1 = ARRAY OF 3 ScS;
TYPE A1 = TheA1;
TYPE A2 = ARRAY OF 3, 4 CARDINAL;
TYPE BS = SEQUENCE OF BYTE;
TYPE TheR = RECORD a: A1, css: CSS, i: I END;
TYPE R = TheR;
TYPE TheRS = SEQUENCE OF R;
TYPE RS = TheRS;
TYPE RO = OPTIONAL R;
TYPE TheOO = OPTIONAL O1;
TYPE OO = TheOO;
TYPE OO2 = BOOLEAN UNION O2 = TRUE END END OTHERS;
TYPE TheU = UNION R, RO, CSS, O1, OO, BOOLEAN END;
TYPE U = TheU;
TYPE U1 = UNION R, A1 END;
TYPE TheE = ENUMERATION ev1, ev3 = 3, ev5, ev7 END;
TYPE E = TheE;
TYPE U2 = UNION
	x1 : BOOLEAN = 3, 7 END,
	x2 : O2 = 1, 22 END,
	x3 : CSS = 2 END
	END OTHERS;
TYPE U3 = E UNION
	ilu.CString = ev1 END,
	U2 = ev3 END,
	v3: O2 = ev7 END,
	v4: RO = DEFAULT
	END;
TYPE U4 = SHORT INTEGER UNION
	x1 : BOOLEAN = 3, 7 END,
	x2 : O2 = DEFAULT,
	x3 : CSS = 1, 0 END
	END;
TYPE U-byte = BYTE UNION R, O2 END;
TYPE U-int = INTEGER UNION R, O2 END;
TYPE U-card = CARDINAL UNION R, O2 END;
TYPE U-scard = SHORT CARDINAL UNION R, One END;

EXCEPTION E1: U;
EXCEPTION E2: INTEGER;
EXCEPTION CantCreate;
EXCEPTION E3: RO;
EXCEPTION E4: O1;
EXCEPTION E5: A0;
EXCEPTION E6: RS;
EXCEPTION E7: ilu.CString;
EXCEPTION E8: A1;
EXCEPTION E9: R;

TYPE O1 = TheO1;

TYPE TheO1 = OBJECT
  DOCUMENTATION "sample
multi-line doc string"
  METHODS
    U-CSS-to-U (u: U, css: CSS) : U RAISES E1, E2 END,
    FUNCTIONAL f-CSS-to-RO(css: CSS): RO RAISES E1 END,
    R-ScS-to-F (r: R, s: ScS): SHORT REAL,
    ASYNCHRONOUS a-RO (ro: RO),
    get-O2 () : O2 RAISES CantCreate END,
    get-O3 (subclass : BOOLEAN) : O3 RAISES CantCreate END
  END;

TYPE One = TheO1;

TYPE O2 = OBJECT
  DOCUMENTATION "sample multi-line
    doc string with leading whitespace on the second line"
  SINGLETON "sunrpc_2_0x3458_3"
  METHODS
    OO-A0-to-CSS (o : OO, a : A0) : CSS RAISES E2 END,
    R-I-A1-to-I-A0 (r : R, INOUT i : I, a : A1) : A0
  END;

TYPE O3 = OBJECT
  BRAND "v1"
  COLLECTIBLE
  METHODS
    RS-R-to-R-IS (r : RS, OUT r2 : R) : IS,
    O1-U-to-U (o : SIBLING O1, INOUT u: U) RAISES E2 END,
    BS-to-I (b : BS) : I
  END;
    
TYPE P = OBJECT SUPERTYPES O3 END
  METHODS
    m2 (j: INTEGER): IS
  END;

TYPE O4 = OBJECT SUPERTYPES O3 END
  METHODS
    R-to-R (r : REAL): REAL
  END;
(*
TYPE O5 = OBJECT SUPERTYPES O1, O3 END
  METHODS
    m5-1 (j: INTEGER): IS
  END;

TYPE O6 = OBJECT SUPERTYPES O3, O1 END
  METHODS
    m6-1 (j: INTEGER): IS
  END;
*)
