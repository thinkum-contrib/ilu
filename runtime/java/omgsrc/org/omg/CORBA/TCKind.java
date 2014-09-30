// javastubs/CORBA/TCKind.java
// Stubs for "CORBA"
//
// This file was automatically generated with ILU (version 0.0jacobi) tools
// at Sun Apr 20 15:31:47 1997 by `jacobi'
// running "/tilde/jacobi/ilus/stubbers/java/java-stubber" of Sun Apr 20 14:16:41 1997
// on "/tilde-am/jacobi/ilus/runtime/java/TCKind.isl" of Sun Apr 20 15:30:24 1997,
// and "/tilde/jacobi/iluh/interfaces/ilu.isl" of Sat Apr 19 16:26:16 1997.
//
// Chris Jacobi, November 13, 1998 3:06 pm PST

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



package org.omg.CORBA;

/** 
 * Literals classifying corba types.
 * @see xerox.ilu.IluTypeCode
 * @see org.omg.CORBA.TCKind
 */
public final class TCKind { /*an enumeration*/

    private int __val;
    private static TCKind[] __a = new TCKind[29];

    private TCKind(){}
    private static final TCKind __define(int __i) {
        TCKind __x = new TCKind();
        __x.__val = __i;
        __a[__i] = __x;
        return __x;
    } //__define

    public static final TCKind from_int(int __i)
            throws org.omg.CORBA.SystemException {
        if (__i < 0 || __i > 28) {
            throw new org.omg.CORBA.BAD_PARAM();
        }
        return __a[__i];
    } //from_int

    public final int value() {
        return __val;
    } //value

    public static final int _tk_null = 0;
    public static final int _tk_void = 1;
    public static final int _tk_short = 2;
    public static final int _tk_long = 3;
    public static final int _tk_ushort = 4;
    public static final int _tk_ulong = 5;
    public static final int _tk_float = 6;
    public static final int _tk_double = 7;
    public static final int _tk_boolean = 8;
    public static final int _tk_char = 9;
    public static final int _tk_octet = 10;
    public static final int _tk_any = 11;
    public static final int _tk_TypeCode = 12;
    public static final int _tk_Principal = 13;
    public static final int _tk_objref = 14;
    public static final int _tk_struct = 15;
    public static final int _tk_union = 16;
    public static final int _tk_enum = 17;
    public static final int _tk_string = 18;
    public static final int _tk_sequence = 19;
    public static final int _tk_array = 20;
    public static final int _tk_alias = 21;
    public static final int _tk_except = 22;
    public static final int _tk_longlong = 23;
    public static final int _tk_ulonglong = 24;
    public static final int _tk_longdouble = 25;
    public static final int _tk_wchar = 26;
    public static final int _tk_wstring = 27;
    public static final int _tk_fixed = 28;

    public static final TCKind tk_null = __define(_tk_null);
    public static final TCKind tk_void = __define(_tk_void);
    public static final TCKind tk_short = __define(_tk_short);
    public static final TCKind tk_long = __define(_tk_long);
    public static final TCKind tk_ushort = __define(_tk_ushort);
    public static final TCKind tk_ulong = __define(_tk_ulong);
    public static final TCKind tk_float = __define(_tk_float);
    public static final TCKind tk_double = __define(_tk_double);
    public static final TCKind tk_boolean = __define(_tk_boolean);
    public static final TCKind tk_char = __define(_tk_char);
    public static final TCKind tk_octet = __define(_tk_octet);
    public static final TCKind tk_any = __define(_tk_any);
    public static final TCKind tk_TypeCode = __define(_tk_TypeCode);
    public static final TCKind tk_Principal = __define(_tk_Principal);
    public static final TCKind tk_objref = __define(_tk_objref);
    public static final TCKind tk_struct = __define(_tk_struct);
    public static final TCKind tk_union = __define(_tk_union);
    public static final TCKind tk_enum = __define(_tk_enum);
    public static final TCKind tk_string = __define(_tk_string);
    public static final TCKind tk_sequence = __define(_tk_sequence);
    public static final TCKind tk_array = __define(_tk_array);
    public static final TCKind tk_alias = __define(_tk_alias);
    public static final TCKind tk_except = __define(_tk_except);
    public static final TCKind tk_longlong = __define(_tk_longlong);
    public static final TCKind tk_ulonglong = __define(_tk_ulonglong);
    public static final TCKind tk_longdouble = __define(_tk_longdouble);
    public static final TCKind tk_wchar = __define(_tk_wchar);
    public static final TCKind tk_wstring = __define(_tk_wstring);
    public static final TCKind tk_fixed = __define(_tk_fixed);

} //TCKind

