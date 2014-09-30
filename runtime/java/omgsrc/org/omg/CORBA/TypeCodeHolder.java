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
/* TypeCodeHolder.java */
/* Chris Jacobi, August 21, 1997 5:26 pm PDT */

/*
 */
 
/* $Id: TypeCodeHolder.java,v 1.5 1999/08/03 01:54:58 janssen Exp $ */
 

package org.omg.CORBA;

/**
 * A means to pass a CORBA TypeCode as a reference parameter. <p>
 * Holder classes are mutable.<p>
 *
 * Note that Ilu does not allow passing typecodes as parameter of
 * methods or as value field of any-s; This class is therefore 
 * of limited use and mainly for corba compatibility...
 */
public final class TypeCodeHolder {
    public org.omg.CORBA.TypeCode value = null;
    public TypeCodeHolder() { };
    public TypeCodeHolder(org.omg.CORBA.TypeCode tc) {
        value = tc;
    }
} //TypeCodeHolder
 
