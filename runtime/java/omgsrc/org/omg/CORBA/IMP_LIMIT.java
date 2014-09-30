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
/* org.omg.CORBA.IMP_LIMIT.java */
/* Chris Jacobi, November 16, 1998 10:41 am PST */

/*
 */ 
  
/*
 * $Id: IMP_LIMIT.java,v 1.4 1999/08/03 01:54:47 janssen Exp $
 */

package org.omg.CORBA;

/**
 * org.omg.CORBA.IMP_LIMIT.java
 * The corba standard SystemException "IMP_LIMIT"   
 */
public  
class IMP_LIMIT extends org.omg.CORBA.SystemException {
    public IMP_LIMIT() {
	super();
    }
    public IMP_LIMIT(java.lang.String s) {
	super(s);
    }
    public IMP_LIMIT(int minor, CompletionStatus completed) {
	super(minor, completed);
    }
    public IMP_LIMIT(java.lang.String s, int minor, CompletionStatus completed) {
	super(s, minor, completed);
    }
    public static IMP_LIMIT alloc() {
        return new IMP_LIMIT();
    }
} //IMP_LIMIT
