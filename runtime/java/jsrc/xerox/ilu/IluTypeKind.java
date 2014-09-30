// IluTypeKind.java
//
// This file was automatically generated with ILU (version 2.0alpha9) tools
// at Mon Apr 14 14:53:17 1997 by `jacobi'
// running "/tilde/jacobi/ilus/stubbers/java/java-stubber" of Mon Apr 14 12:08:10 1997
// on "/tilde/jacobi/ilus/runtime/java/IluTypeKind.isl" of Mon Apr 14 14:52:29 1997,
// and "/tilde/jacobi/iluh/interfaces/ilu.isl" of Thu Jan 16 13:31:08 1997.
//
// Chris Jacobi, January 18, 1999 7:29 pm PST

/*
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
*/

// ILU information:  ftp://ftp.parc.xerox.com/pub/ilu/ilu.html.
// Chris Jacobi, June 30, 1998 10:44 am PDT



package xerox.ilu;

/** 
 * Public literals classifying ilu types.
 * @see IluTypeCode
 * @see org.omg.CORBA.TCKind
 */
public final class IluTypeKind 
        implements java.io.Serializable, xerox.ilu.IluResolving { 

    private int val;
    private static IluTypeKind[] a = new IluTypeKind[23];
    private static final int __hashOffset = 567;

    protected IluTypeKind(){}
    private static final IluTypeKind define(int i) {
        IluTypeKind x = new IluTypeKind();
        x.val = i;
        a[i] = x;
        return x;
    } //define

    public static final IluTypeKind from_int(int i)
            throws org.omg.CORBA.SystemException {
        if (i < 0 || i > 22) {
            throw new org.omg.CORBA.BAD_PARAM();
        }
        return a[i];
    } //from_int


    public final int value() {
        return val;
    } //value

    public static final int _byte_tk = 0;
    public static final int _boolean_tk = 1;
    public static final int _character_tk = 2;
    public static final int _shortcharacter_tk = 3;
    public static final int _shortinteger_tk = 4;
    public static final int _integer_tk = 5;
    public static final int _longinteger_tk = 6;
    public static final int _shortcardinal_tk = 7;
    public static final int _cardinal_tk = 8;
    public static final int _longcardinal_tk = 9;
    public static final int _real_tk = 10;
    public static final int _shortreal_tk = 11;
    public static final int _longreal_tk = 12;
    public static final int _object_tk = 13;
    public static final int _pipe_tk = 14;
    public static final int _optional_tk = 15;
    public static final int _alias_tk = 16;
    public static final int _union_tk = 17;
    public static final int _sequence_tk = 18;
    public static final int _record_tk = 19;
    public static final int _array_tk = 20;
    public static final int _enumeration_tk = 21;
    public static final int _pickle_tk = 22;

    public static final IluTypeKind byte_tk = define(_byte_tk);
    public static final IluTypeKind boolean_tk = define(_boolean_tk);
    public static final IluTypeKind character_tk = define(_character_tk);
    public static final IluTypeKind shortcharacter_tk = define(_shortcharacter_tk);
    public static final IluTypeKind shortinteger_tk = define(_shortinteger_tk);
    public static final IluTypeKind integer_tk = define(_integer_tk);
    public static final IluTypeKind longinteger_tk = define(_longinteger_tk);
    public static final IluTypeKind shortcardinal_tk = define(_shortcardinal_tk);
    public static final IluTypeKind cardinal_tk = define(_cardinal_tk);
    public static final IluTypeKind longcardinal_tk = define(_longcardinal_tk);
    public static final IluTypeKind real_tk = define(_real_tk);
    public static final IluTypeKind shortreal_tk = define(_shortreal_tk);
    public static final IluTypeKind longreal_tk = define(_longreal_tk);
    public static final IluTypeKind object_tk = define(_object_tk);
    public static final IluTypeKind pipe_tk = define(_pipe_tk);
    public static final IluTypeKind optional_tk = define(_optional_tk);
    public static final IluTypeKind alias_tk = define(_alias_tk);
    public static final IluTypeKind union_tk = define(_union_tk);
    public static final IluTypeKind sequence_tk = define(_sequence_tk);
    public static final IluTypeKind record_tk = define(_record_tk);
    public static final IluTypeKind array_tk = define(_array_tk);
    public static final IluTypeKind enumeration_tk = define(_enumeration_tk);
    public static final IluTypeKind pickle_tk = define(_pickle_tk);
    

    /** Object procedure */
    public int hashCode() {
        return (val + __hashOffset);
    } //hashCode

    /** Object procedure */
    public boolean equals(java.lang.Object __o) {
        if (this == __o) return true;
        if (__o instanceof IluTypeKind) {
            return (val == ((IluTypeKind)__o).val);
        }
        return false;
    } //equals

    /** IluResolving. Used by (de) serialization */
    public java.lang.Object readResolve() throws java.io.ObjectStreamException {
        return from_int(val);
    } //readResolve

    static final long serialVersionUID = 3366305872378260902L;
    
} //IluTypeKind
