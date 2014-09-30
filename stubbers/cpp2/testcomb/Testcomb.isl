(** $Id: Testcomb.isl,v 1.7 1999/08/11 06:16:08 pnewman Exp $
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

INTERFACE Testcomb;


CONSTANT IntConst        : INTEGER = 25000; 
CONSTANT SignIntConst    : INTEGER = +25000; 
CONSTANT HexIntConst     : INTEGER = +0xF00; 
CONSTANT ShortIntConst   : SHORT INTEGER = -165;
CONSTANT LongIntConst    : LONG  INTEGER = +15;
CONSTANT BooleanConst    : BOOLEAN = FALSE;  
CONSTANT CardConst       : CARDINAL = 2500;
CONSTANT OctCardConst    : CARDINAL = 0o577;
CONSTANT ShortCardConst  : SHORT CARDINAL = 25;
CONSTANT LongCardConst   : LONG CARDINAL = 250000;
CONSTANT ByteConst       : BYTE = 15;
CONSTANT BinByteConst    : BYTE = 0b111;
CONSTANT FloatConst      : REAL =  -3.44000000e7;
CONSTANT ShortFloatConst : SHORT REAL =  -3.4e7;
CONSTANT ShortFloatConst1 : SHORT REAL =  -3.4;
CONSTANT LongFloatConst  : SHORT REAL =  -3.44000000000e7;
CONSTANT StringConst     : ilu.CString  =  "stringconstant";
CONSTANT EscapedString   : ilu.CString = "#"###3B#n"; 

TYPE MyByte         = BYTE;
TYPE MyInteger      = INTEGER;
TYPE MyBoolean      = BOOLEAN;
TYPE ShortCardinal  = SHORT CARDINAL;
TYPE ShortInteger   = SHORT INTEGER;
TYPE ShortReal      = SHORT REAL;
TYPE ShortChar      = SHORT CHARACTER;
TYPE MyChar         = CHARACTER;
TYPE LongReal       = LONG REAL; 
TYPE LongCardinal   = LONG CARDINAL; 
TYPE LongInteger    = LONG INTEGER; 

TYPE MyObject       = OBJECT
  METHODS
    R-to-R(r: REAL) : REAL
END;

TYPE OptMyBoolean     = OPTIONAL BOOLEAN;
TYPE OptShortCardinal = OPTIONAL SHORT CARDINAL;
TYPE OptShortInteger  = OPTIONAL SHORT INTEGER;
TYPE OptShortReal     = OPTIONAL SHORT REAL;
TYPE OptShortChar     = OPTIONAL SHORT CHARACTER;
TYPE OptChar          = OPTIONAL CHARACTER;
TYPE OptLongCardinal  = OPTIONAL LONG CARDINAL;  
TYPE OptLongInteger   = OPTIONAL LONG INTEGER; 
TYPE OptLongReal      = OPTIONAL LONG REAL; 
TYPE OptMyInteger     = OPTIONAL MyInteger; 
TYPE OptMyChar        = OPTIONAL MyChar;
TYPE OptPickle        = OPTIONAL Pickle;
TYPE OptMyObject      = OPTIONAL MyObject; 


TYPE RecOfBuiltIns  = RECORD
  MbrBoolean:       MyBoolean,
  MbrShortCardinal: ShortCardinal,
  MbrShortInteger:  ShortInteger,
  MbrShortReal:     ShortReal,
  MbrShortChar:     ShortChar,
  MbrChar:          MyChar,
  MbrLongCardinal:  LongCardinal, 
  MbrLongInteger:   LongInteger,  
  MbrLongReal:      LongReal, 
  MbrPickle:        Pickle,
  MbrObject:        MyObject,
  MbrRecOfBuiltIns:  OptRecOfBuiltIns
 END;


TYPE RecOfOptBuiltIns  = RECORD
  MbrOptBoolean:       OptMyBoolean,
  MbrOptShortCardinal: OptShortCardinal,
  MbrOptShortInteger:  OptShortInteger,
  MbrOptShortReal:     OptShortReal,
  MbrOptShortChar:     OptShortChar,
  MbrOptChar:          OptChar,
  MbrOptLongInteger:   OptLongInteger,
  MbrOptLongReal:      OptLongReal,
  MbrOptPickle:        OptPickle,
  MbrOptObj:           OptMyObject
END;


TYPE ArrayOfMyByte        = ARRAY OF 6   MyByte;
TYPE ArrayOfInteger       = ARRAY OF 8, 9  MyInteger;
TYPE ArrayOfBoolean       = ARRAY OF 10, 11, 12 MyBoolean;
TYPE ArrayOfShortCardinal = ARRAY OF 6 ShortCardinal;
TYPE ArrayOfShortInteger  = ARRAY OF 10, 11, 12 ShortInteger;
TYPE ArrayOfShortChar     = ARRAY OF 10, 11, 12 ShortChar;
TYPE ArrayOfChar          = ARRAY OF 20, 13, 10, 5 MyChar;
TYPE VectorOfShortChar    = ARRAY OF 10 ShortChar;
TYPE VectorOfChar         = ARRAY OF 10 MyChar;
TYPE ArrayOfOptInteger    = ARRAY OF 20, 13, 10, 5 OptMyInteger;
TYPE ArrayOfOptShortReal  = ARRAY OF 6 OptShortReal;
TYPE ArrayOfOptShortChar  = ARRAY OF 10 OptShortChar;

TYPE ArrayOfOptChar       = ARRAY OF 20, 13, 10, 5 OptChar; 
TYPE ArrayOfPickle     = ARRAY OF 20, 13, 10, 5 Pickle; 
TYPE ArrayOfOptPickle     = ARRAY OF 20, 13, 10, 5 OptPickle; 

TYPE ArrayOfLongCardinal  = ARRAY OF 8, 9 LongCardinal;  
TYPE ArrayOfLongInteger   = ARRAY OF 20, 13, 10, 5 LongInteger;
TYPE ArrayOfOptLongReal   = ARRAY OF 8, 9 OptLongReal; 

TYPE SeqOfMyByte          = SEQUENCE OF MyByte;
TYPE SeqOfMyByteLim       = SEQUENCE OF MyByte LIMIT 5;
TYPE SeqOfIntegerLim      = SEQUENCE OF MyInteger LIMIT 5;
TYPE SeqOfBoolean         = SEQUENCE OF MyBoolean;
TYPE SeqOfShortCardinal   = SEQUENCE OF ShortCardinal;
TYPE SeqOfShortInteger    = SEQUENCE OF ShortInteger;
TYPE SeqOfShortCharLim    = SEQUENCE OF ShortChar LIMIT 1000;
TYPE SeqOfChar            = SEQUENCE OF MyChar;
TYPE SeqOfCharLim         = SEQUENCE OF MyChar  LIMIT 27;
TYPE SeqOfOptShortReal    = SEQUENCE OF OptShortReal;
TYPE SeqOfOptShortChar    = SEQUENCE OF OptShortChar;
TYPE SeqOfOptShortCharLim = SEQUENCE OF OptShortChar LIMIT 1000;
TYPE SeqOfOptChar         = SEQUENCE OF OptChar;
TYPE SeqOfOptCharLim      = SEQUENCE OF OptChar  LIMIT 27;


TYPE SeqOfPickleLim    = SEQUENCE OF Pickle  LIMIT 10; 
TYPE SeqOfLongCardinalLim = SEQUENCE OF LongCardinal LIMIT 100; 
TYPE SeqOfLongIntegerLim  = SEQUENCE OF LongInteger LIMIT 1000; 
TYPE SeqOfOptLongRealLim  = SEQUENCE OF OptLongReal LIMIT 10000;

TYPE UnionOfBuiltIns = UNION
  ArmMbrBoolean:    MyBoolean     = 1 END,
  ArmShortCardinal: ShortCardinal = 2 END,
  ArmShortInteger:  ShortInteger  = 3 END,
  ArmShortReal:     ShortReal     = 4 END,
  ArmShortChar:     ShortChar     = 5 END,
  ArmChar:          MyChar        = 6 END,
  ArmLongCardinal:  LongCardinal  = 7 END, 
  ArmLongInteger:   LongInteger   = 8 END, 
  ArmLongReal:      LongReal      = 9 END
END;

TYPE UnionOfOptBuiltIns = UNION
  ArmMbrOptBoolean:    OptMyBoolean     = 1 END,
  ArmOptShortCardinal: OptShortCardinal = 2 END,
  ArmOptShortInteger:  OptShortInteger  = 3 END,
  ArmOptShortReal:     OptShortReal     = 4 END,
  ArmOptShortChar:     OptShortChar     = 5 END,
  ArmOptChar:          OptMyChar        = 6 END,
  ArmOptLongCardinal:  OptLongCardinal  = 7 END, 
  ArmOptLongInteger:   OptLongInteger   = 8 END, 
  ArmOptLongReal:      OptLongReal      = 9 END,
  ArmOptPickle:        OptPickle      =  10 END
END;
    

TYPE OptRecOfBuiltIns   = OPTIONAL RecOfBuiltIns;
TYPE OptArrayOfInteger  = OPTIONAL ArrayOfInteger;
TYPE OptSeqOfChar       = OPTIONAL SeqOfChar;
TYPE OptSeqOfOptChar    = OPTIONAL SeqOfOptChar; 
TYPE OptUnionOfBuiltIns = OPTIONAL UnionOfBuiltIns;

TYPE RecOfStructs = RECORD
   MbrRecOfBuiltins:   RecOfBuiltIns, 
   MbrArrayOfInteger:  ArrayOfInteger,
   MbrSeqOfChar:       SeqOfChar,
   MbrUnionOfBuiltIns: UnionOfBuiltIns 
END;

TYPE UnionOfStructs = UNION
   ArmRecOfBuiltins:   RecOfBuiltIns   = 1 END, 
   ArmArrayOfInteger:  ArrayOfInteger  = 2 END,
   ArmSeqOfChar:       SeqOfChar       = 3 END,
   ArmUnionOfBuiltIns: UnionOfBuiltIns = 4 END, 
   ArmMyObject:        MyObject        = 5 END 
END;

TYPE RecOfOptStructs = RECORD
   MbrOptRecOfBuiltins:   OptRecOfBuiltIns, 
   MbrOptArrayOfInteger:  OptArrayOfInteger,
   MbrOptSeqOfChar:       OptSeqOfChar,
   MbrOptUnionOfBuiltIns: OptUnionOfBuiltIns 
END;

TYPE SeqOfArrayOfInteger = SEQUENCE OF ArrayOfInteger;
TYPE SeqOfSeqOfChar       = SEQUENCE OF SeqOfChar;
TYPE SeqOfRecOfBuiltIns   = SEQUENCE OF RecOfBuiltIns;
TYPE SeqOfUnionOfBuiltIns = SEQUENCE OF UnionOfBuiltIns;

TYPE SeqOfOptArrayOfInteger = SEQUENCE OF OptArrayOfInteger;
TYPE SeqOfOptSeqOfChar       = SEQUENCE OF OptSeqOfChar;
TYPE SeqOfOptRecOfBuiltIns   = SEQUENCE OF OptRecOfBuiltIns;
TYPE SeqOfOptUnionOfBuiltIns = SEQUENCE OF OptUnionOfBuiltIns;
TYPE SeqOfOptPickle         = SEQUENCE OF OptPickle;
TYPE SeqOfMyObject         = SEQUENCE OF MyObject;

TYPE ArrayOfOptArray = ARRAY OF 5   OptArrayOfInteger;
TYPE ArrayOfOptSeq   = ARRAY OF 5,6 OptSeqOfChar;
TYPE ArrayOptRec     = ARRAY OF 5   OptRecOfBuiltIns;
TYPE ArrayOfOptUnion = ARRAY OF 5,6 OptUnionOfBuiltIns;

TYPE UnionOfOptStructs = UNION
   ArmOptRecOfBuiltins:   OptRecOfBuiltIns   = 1 END, 
   ArmOptArrayOfInteger:  OptArrayOfInteger  = 2 END,
   ArmOptSeqOfChar:       OptSeqOfChar       = 3 END,
   ArmOptUnionOfBuiltIns: OptUnionOfBuiltIns = 4 END, 
   ArmOptMyObject:           OptMyObject        = 5 END 
END;

