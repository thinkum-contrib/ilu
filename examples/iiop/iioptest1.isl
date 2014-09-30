INTERFACE iioptest1;

(*
// @(#)test1.idl	2.1 95/03/16
// Copyright 1994-1995 by Sun Microsystems, Inc.
//
// TEST		basic marshaling tests for all IDL primitive types, modes
//
// This test omits constructed types (struct, union, enum, sequence, and
// array types), and only tests very simple user defined exceptions.
//
// Values returned are well defined functions of the input values:
//
//	* For numeric types (octet, short, long, longlong, float, double,
//	  longdouble, and unsigned variants) the value is cubed.
//	* For Boolean, it's the negation.
//	* For Any, TypeCode, Principal, Object, char and wchar,
//	  it's the input value.
//	* For strings (and wstrings), it's the input value catenated
//	  to itself ("foo" --> "foofoo")
//
// The "return" and "out" parameter is the function of the "in" parameter;
// the "inout" parameter is the function of its original value.
//
*)

TYPE UnicodeString = SEQUENCE OF CHARACTER;	(* wstring *)
TYPE "string" = SEQUENCE OF SHORT CHARACTER;

TYPE x2-rec = RECORD
  obj : obj,
  case-num : INTEGER
END;

TYPE x1-rec = RECORD
  case-num : INTEGER
END;

EXCEPTION x1 : x1-rec;
EXCEPTION x2 : x2-rec;



TYPE obj = OBJECT OPTIONAL TYPEID "IDL:Eng.SUN.COM/test1:1.0"
  METHODS

    test-void (),

    test-short (IN a1 : SHORT INTEGER, OUT a2 : SHORT INTEGER, INOUT a3 : SHORT INTEGER ) : SHORT INTEGER,
    test-long (IN a1 : INTEGER, OUT a2 : INTEGER, INOUT a3 : INTEGER ) : INTEGER,
    test-ushort (IN a1 : SHORT CARDINAL, OUT a2 : SHORT CARDINAL, INOUT a3 : SHORT CARDINAL ) : SHORT CARDINAL,
    test-ulong (IN a1 : CARDINAL, OUT a2 : CARDINAL, INOUT a3 : CARDINAL ) : CARDINAL,
    test-float (IN a1 : SHORT REAL, OUT a2 : SHORT REAL, INOUT a3 : SHORT REAL ) : SHORT REAL,
    test-double (IN a1 : REAL, OUT a2 : REAL, INOUT a3 : REAL ) : REAL,
    test-boolean (IN a1 : BOOLEAN, OUT a2 : BOOLEAN, INOUT a3 : BOOLEAN ) : BOOLEAN,
    test-char (IN a1 : SHORT CHARACTER, OUT a2 : SHORT CHARACTER, INOUT a3 : SHORT CHARACTER ) : SHORT CHARACTER,
    test-octet (IN a1 : BYTE, OUT a2 : BYTE, INOUT a3 : BYTE ) : BYTE,
    test-string (IN a1 : "string", OUT a2 : "string", INOUT a3 : "string" ) : "string",

    test-longlong (IN a1 : LONG INTEGER, OUT a2 : LONG INTEGER, INOUT a3 : LONG INTEGER ) : LONG INTEGER,
    test-ulonglong (IN a1 : LONG CARDINAL, OUT a2 : LONG CARDINAL, INOUT a3 : LONG CARDINAL ) : LONG CARDINAL,
    test-wchar (IN a1 : CHARACTER, OUT a2 : CHARACTER, INOUT a3 : CHARACTER ) : CHARACTER,
    test-wstring (IN a1 : UnicodeString, OUT a2 : UnicodeString, INOUT a3 : UnicodeString ) : UnicodeString,
    test-longdouble (IN a1 : LONG REAL, OUT a2 : LONG REAL, INOUT a3 : LONG REAL ) : LONG REAL,

    (*
	Tests of "any", "Principal", "Object", and "TypeCode" omitted.
    *)

    (*
    //
    // All cases, "case_num" in the exception is the same as the 'in' param
    //	* negative or zero, throws x1
    //	* positive even cases, throws x2 with obj = null objref
    //	* positive odd cases, throws x2 with obj = target objref
    //
    *)

    test-throw (IN case-num : INTEGER) RAISES x1, x2 END,

    (*
    //
    // Aid for test cleanup in case server's not told to quit after
    // being idle for some time period
    //
    *)

    ASYNCHRONOUS please-exit ()
END;

TYPE rec1 = RECORD	(* CORBA C mapping's "variable-length" record *)
 f1 : SHORT INTEGER,
 f2 : INTEGER,
 f3 : SHORT CARDINAL,
 f4 : CARDINAL,
 f5 : BYTE,
 f6 : BOOLEAN,
 f7 : SHORT REAL,
 f8 : REAL,
 f9 : "string"
END;

TYPE rec2 = RECORD	(* CORBA C mapping's "fixed-length" record *)
 f1 : SHORT INTEGER,
 f2 : BYTE
END;

TYPE arr1 = ARRAY OF 2,3 rec1;
TYPE arr2 = ARRAY OF 2,3 "string";
TYPE arr3 = ARRAY OF 2,3 short character;
TYPE arr4 = ARRAY OF 2,3 rec2;

TYPE seq1 = SEQUENCE OF rec2;

TYPE enum1 = ENUMERATION a, b, c, d, e END;

TYPE union1 = enum1 UNION a : rec1 = a END, b : arr1 = b END, c : seq1 = c END, d : enum1 = default END;

TYPE union2 = UNION integer, real END;

TYPE opt1 = OPTIONAL rec2;

TYPE ext-obj = OBJECT SUPERTYPES obj END OPTIONAL
  METHODS

    test-obj (IN a1 : ext-obj, OUT a2 : ext-obj, INOUT a3 : ext-obj) : ext-obj
	"Object returned via a2, a3, and return value is same as a1",

    test-fixed-record (IN a1 : rec2, OUT a2 : rec2, INOUT a3 : rec2) : rec2
	"All fields of record are returned as per their individual types",

    test-enumeration (IN a1 : enum1, OUT a2 : enum1, INOUT a3 : enum1) : enum1
	"value returned in a2 and as return value is next value in sequence after a1;
	value returned in a3 is previous value to input value of a3",

    test-sequence (IN a1 : seq1, OUT a2 : seq1, INOUT a3 : seq1) : seq1
	"All values of sequence are returned as per their individual type",

    test-var-record (IN a1 : rec1, OUT a2 : rec1, INOUT a3 : rec1) : rec1
	"All fields of record are returned as per their individual types",

    test-var-array (IN a1 : arr1, OUT a2 : arr1, INOUT a3 : arr1) : arr1
	"Values returned modified as per their primitive type",

    test-fixed-array (IN a1 : arr4, OUT a2 : arr4, INOUT a3 : arr4) : arr4
	"Values returned modified as per their primitive type",

    test-var-union (IN a1 : union1, OUT a2 : union1, INOUT a3 : union1) : union1
	"value is treated as per its regular type",

    test-fixed-union (IN a1 : union2, OUT a2 : union2, INOUT a3 : union2) : union2
	"a3 returns transform of a1; return and a2 return transform of a3.  Transform
	is as follows:  real input leads to integer output, integer input lead to
	negative real output"

  END;

TYPE xilu-obj = OBJECT SUPERTYPES ext-obj END

  METHODS

    test-optional (IN a1 : opt1, OUT a2 : opt1, INOUT a3 : opt1) : opt1
	"if a1 is NIL, a2 and return value are not, and vice versa;
	if a3 is NIL, a3 is not, and vice versa",

    test-asynchronous (IN a1 : integer)

  END;
