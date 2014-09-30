/* Any.java */
/* Chris Jacobi, November 16, 1998 1:57 pm PST */

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

/* $Id: Any.java,v 1.15 1999/08/03 01:55:12 janssen Exp $ */

package org.omg.CORBA;

/**
 * A skeleton class to achieve corba source compatibility;
 * not complete.
 *
 * @see org.omg.CORBA.TypeCode
 * @see xerox.ilu.IluAny 
 */
 
abstract public class Any {

    abstract public org.omg.CORBA.TypeCode type();
    abstract public void type(org.omg.CORBA.TypeCode t);
    abstract public boolean equal(org.omg.CORBA.Any a);
    
} //Any 
