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
/* org.omg.CORBA.INTF_REPOS.java */
/* Chris Jacobi, November 16, 1998 10:43 am PST */

/*
 */ 
  
/*
 * $Id: INTF_REPOS.java,v 1.4 1999/08/03 01:54:53 janssen Exp $
 */

package org.omg.CORBA;

/**
 * org.omg.CORBA.INTF_REPOS.java
 * The corba standard SystemException "INTF_REPOS"   
 */
public  
class INTF_REPOS extends org.omg.CORBA.SystemException {
    public INTF_REPOS() {
	super();
    }
    public INTF_REPOS(java.lang.String s) {
	super(s);
    }
    public INTF_REPOS(int minor, CompletionStatus completed) {
	super(minor, completed);
    }
    public INTF_REPOS(java.lang.String s, int minor, CompletionStatus completed) {
	super(s, minor, completed);
    }
    public static INTF_REPOS alloc() {
        return new INTF_REPOS();
    }
} //INTF_REPOS
