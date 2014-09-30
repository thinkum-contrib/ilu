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
/* IluMethodArgRep.java */
/* Chris Jacobi, December 23, 1998 12:10 pm PST */
/* $Id: IluMethodArgRep.java,v 1.6 1999/08/03 01:53:56 janssen Exp $ */
 
package xerox.ilu;

/*friendly*/ 
/**
 * Definition of method arguments.<p>
 * 
 * Conceptionally local to IluMethodRep.java
 * and accessed by IluJava_IluClassRep.c <br>
 * This is in an extra file to ensure a good class name 
 * to enable access by native methods.<br>
 *
 * @see IluMethodRep
 */
final class IluMethodArgRep {
    
    /*friendly*/ java.lang.String argName = null;
    /*friendly*/ boolean sibling = false;
    /*friendly*/ int argDirection = 0;
    /*friendly*/ java.lang.String typeUid = null;
    
    public IluMethodArgRep() {};
    
    /** argDirection constants */
    public static final int in = 0;
    public static final int out = 1;
    public static final int inout = 2;
    
} //IluMethodArgRep
