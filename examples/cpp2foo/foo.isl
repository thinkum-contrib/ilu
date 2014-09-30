(**
$Id: foo.isl,v 1.7 1999/08/03 01:58:42 janssen Exp $
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
(* Last edited by Mike Spreitzer October 8, 1998 10:39 pm PDT *)

(* Sample interface for CORBA 2.0 C++ example *)

INTERFACE foo;

EXCEPTION zapexception : integer;

TYPE enumtype = ENUMERATION red, orange, yellow, green, blue, indigo, violet END;
TYPE LatinString = SEQUENCE OF SHORT CHARACTER;
TYPE UnicodeString = SEQUENCE OF CHARACTER;

TYPE fixedrecordtype = RECORD
  fixedrecordinteger : integer,
  fixedrecordenum : enumtype
END;

TYPE variablerecordtype = RECORD
  variablerecordstring : LatinString,
  variablerecordinteger : integer,
  variablerecordobject : bar
END;

TYPE integerarray = array of 5, 10 integer;
TYPE bararray = array of 2, 3 bar;

TYPE unboundedlongseq = SEQUENCE OF integer;
TYPE boundedbarseq = SEQUENCE OF bar LIMIT 3 ;

TYPE variableuniontype = short cardinal UNION
	bararm : bar = 0, 1 END,
    integerarm : integer = DEFAULT
	END;

TYPE optionalbarobject = OPTIONAL bar;
TYPE optionalinteger = OPTIONAL integer;

TYPE bar = OBJECT
  COLLECTIBLE
  METHODS

	(* out initial strawman function *)
    zap (IN inarg : integer, INOUT inoutarg : byte, OUT outarg : real ) : boolean
		RAISES zapexception END,

	(* pass objects *)
	passobj (IN inarg : bar, INOUT inoutarg : bar, OUT outarg : bar ) : bar,

	(* pass enums *)
	passenum (IN inarg : enumtype, INOUT inoutarg : enumtype, OUT outarg : enumtype ) : enumtype,

	(* pass latin and unicode strings *)
	passlatinstring (IN inarg : LatinString, INOUT inoutarg : LatinString, OUT outarg : LatinString ) : LatinString,
	passunicodestring (IN inarg : UnicodeString, INOUT inoutarg : UnicodeString, OUT outarg : UnicodeString ) : UnicodeString,
	
	(* pass fixed and variable records *)
	passfixedrecord(IN inarg : fixedrecordtype, INOUT inoutarg : fixedrecordtype, OUT outarg : fixedrecordtype ) : fixedrecordtype,
	passvariablerecord(IN inarg : variablerecordtype, INOUT inoutarg : variablerecordtype, OUT outarg : variablerecordtype ) : variablerecordtype,

	(* pass arrays *)
	passfixedarray (IN inarg : integerarray, INOUT inoutarg : integerarray, OUT outarg : integerarray ) : integerarray,
	passvariablearray (IN inarg : bararray, INOUT inoutarg : bararray, OUT outarg : bararray ) : bararray,

	(* pass sequences *)
	passunboundedsequence (IN inarg : unboundedlongseq, INOUT inoutarg : unboundedlongseq, OUT outarg : unboundedlongseq ) : unboundedlongseq,
	passboundedsequence (IN inarg : boundedbarseq, INOUT inoutarg : boundedbarseq, OUT outarg : boundedbarseq ) : boundedbarseq,

 	(* pass unions - variable type used, fixed type should be fairly obvious *)
	passvariableunion (IN inarg : variableuniontype, INOUT inoutarg : variableuniontype, OUT outarg : variableuniontype ) : variableuniontype,

 	(* pass optionals  *)
	passoptionals (IN inarg : optionalinteger, INOUT inoutarg : optionalbarobject, OUT outarg : optionalbarobject ) : optionalinteger,

 	(* pass simple types anys  *)
	passsimpleanys(IN inarg : PICKLE, INOUT inoutarg : PICKLE, OUT outarg : PICKLE ) : PICKLE,

 	(* pass arrays in anys  *)
	passarrayanys(IN inarg : PICKLE, INOUT inoutarg : PICKLE, OUT outarg : PICKLE ) : PICKLE,

 	(* pass assorted types in anys  *)
	passassortedanys(IN inarg : PICKLE, INOUT inoutarg : PICKLE, OUT outarg : PICKLE ) : PICKLE


 END;

