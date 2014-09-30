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
/* CompletionStatus.java */
/* Chris Jacobi, September 3, 1997 10:51 pm PDT */

/*
 */ 
 

/*
 * $Id: CompletionStatus.java,v 1.3 1999/08/03 01:54:59 janssen Exp $
 */

package org.omg.CORBA;
public final class CompletionStatus {
    
    private int val = 0;
    
    // Completion Status constants
    public static final int _COMPLETED_YES = 0; 
    public static final int _COMPLETED_NO = 1; 
    public static final int _COMPLETED_MAYBE = 2;
        
    public static final CompletionStatus 
        COMPLETED_YES = new CompletionStatus(_COMPLETED_YES);
        
    public static final CompletionStatus 
        COMPLETED_NO = new CompletionStatus(_COMPLETED_NO);
        
    public static final CompletionStatus 
        COMPLETED_MAYBE = new CompletionStatus(_COMPLETED_MAYBE);
        
    public int value() {return val;}
    public static final CompletionStatus from_int(int i) {
        switch (i) {
            case _COMPLETED_YES: return COMPLETED_YES;
            case _COMPLETED_NO: return COMPLETED_NO;
            default: return COMPLETED_MAYBE;
        }
    } //from_int
    
    private CompletionStatus(int val) {
        this.val = val;
    } //constructor

} //CompletionStatus
