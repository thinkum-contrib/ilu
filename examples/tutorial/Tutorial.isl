(* $Id: Tutorial.isl,v 1.6 1999/08/03 01:57:24 janssen Exp $
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

INTERFACE Tutorial;

EXCEPTION DivideByZero
  "this error is signalled if the client of the Calculator calls
the Divide method with a value of 0";

TYPE Calculator = OBJECT COLLECTIBLE
  DOCUMENTATION "4-function calculator"
  METHODS
    SetValue (v : REAL) "Set the value of the calculator to `v'",
    GetValue () : REAL  "Return the value of the calculator",
    Add (v : REAL)      "Adds `v' to the calculator's value",
    Subtract (v : REAL) "Subtracts `v' from the calculator's value",
    Multiply (v : REAL) "Multiplies the calculator's value by `v'",
    Divide (v : REAL) RAISES DivideByZero END
      "Divides the calculator's value by `v'"
  END;

TYPE Factory = OBJECT
  METHODS
    CreateCalculator () : Calculator
  END;
