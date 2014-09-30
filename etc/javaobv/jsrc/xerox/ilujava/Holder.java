/* $Id: Holder.java,v 1.2 1999/08/03 01:56:21 janssen Exp $
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
/* Chris Jacobi, December 14, 1998 11:32 pm PST */
/* Last edited by Mike Spreitzer October 9, 1998 1:31 pm PDT */


package xerox.ilujava;

/**
 * A means to pass a java.lang.Object as a reference parameter. <p>
 * This is used as the holder class for the full custom mapping
 * of xerox.ilujava.JavaObject.<p>
 * Holder classes are mutable.
 */
public class Holder {
    
    public java.lang.Object value = null;
    
    public Holder() { };
    
    public Holder(java.lang.Object obj) {
        value = obj;
    }
    
} //Holder
 
