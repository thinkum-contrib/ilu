// /tilde/jacobi/ilus/runtime/java/IluConstantValueKind.java
//
// This file was automatically generated with ILU (version 2.0alpha9) tools
// at Mon Apr 14 14:33:10 1997 by `jacobi'
// running "/tilde/jacobi/ilus/stubbers/java/java-stubber" of Mon Apr 14 12:08:10 1997
// on "/tilde/jacobi/ilus/runtime/java/IluConstantValueKind.isl" of Mon Apr 14 14:33:07 1997,
// and "/tilde/jacobi/iluh/interfaces/ilu.isl" of Thu Jan 16 13:31:08 1997.
//
// Chris Jacobi, January 18, 1999 7:31 pm PST

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
// Chris Jacobi, April 14, 1997 2:37 pm PDT

package xerox.ilu;

public final class IluConstantValueKind 
        implements java.io.Serializable, xerox.ilu.IluResolving { 

    private int val;
    private static IluConstantValueKind[] a = new IluConstantValueKind[9];
    private static final int __hashOffset = 999;

    private IluConstantValueKind(){}
    private static final IluConstantValueKind define(int i) {
        IluConstantValueKind x = new IluConstantValueKind();
        x.val = i;
        a[i] = x;
        return x;
    } //define

    public static final IluConstantValueKind from_int(int i)
            throws org.omg.CORBA.SystemException {
        if (i < 0 || i > 8) {
            throw new org.omg.CORBA.BAD_PARAM();
        }
        return a[i];
    } //from_int

    public final int value() {
        return val;
    } //value

    public static final int _byte_cvk = 0;
    public static final int _shortinteger_cvk = 1;
    public static final int _integer_cvk = 2;
    public static final int _shortcardinal_cvk = 3;
    public static final int _cardinal_cvk = 4;
    public static final int _real_cvk = 5;
    public static final int _boolean_cvk = 6;
    public static final int _enumeration_cvk = 7;
    public static final int _string_cvk = 8;

    public static final IluConstantValueKind byte_cvk = define(_byte_cvk);
    public static final IluConstantValueKind shortinteger_cvk = define(_shortinteger_cvk);
    public static final IluConstantValueKind integer_cvk = define(_integer_cvk);
    public static final IluConstantValueKind shortcardinal_cvk = define(_shortcardinal_cvk);
    public static final IluConstantValueKind cardinal_cvk = define(_cardinal_cvk);
    public static final IluConstantValueKind real_cvk = define(_real_cvk);
    public static final IluConstantValueKind boolean_cvk = define(_boolean_cvk);
    public static final IluConstantValueKind enumeration_cvk = define(_enumeration_cvk);
    public static final IluConstantValueKind string_cvk = define(_string_cvk);
    
    /** Object procedure */
    public int hashCode() {
        return (val + __hashOffset);
    } //hashCode

    /** Object procedure */
    public boolean equals(java.lang.Object __o) {
        if (this == __o) return true;
        if (__o instanceof IluConstantValueKind) {
            return (val == ((IluConstantValueKind)__o).val);
        }
        return false;
    } //equals

    /** IluResolving. Used by (de) serialization */
    public java.lang.Object readResolve() throws java.io.ObjectStreamException {
        return from_int(val);
    } //readResolve

    static final long serialVersionUID = -5131988597173621537L;
    
} //IluConstantValueKind
