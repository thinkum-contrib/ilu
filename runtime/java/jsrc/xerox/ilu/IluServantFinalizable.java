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
/* xerox.ilu.IluServantFinalizable.java */
/* Chris Jacobi, August 4, 1998 8:55 pm PDT */

/*
 */
 
/* $Id: IluServantFinalizable.java,v 1.5 1999/08/03 01:53:43 janssen Exp $ */
 
package xerox.ilu;

/**
 * Interface to request a finalization method to be called. <p>
 *
 *
 * This serves as a general replacement for finalize because
 * Ilu forbids clients to use finalize directly.  <p>
 *
 * A server object can implement this interface to
 * request a call of iluServantFinalize on its real
 * finalization time.  Please no resurection or
 * other fancy usage. <p>
 *
 * Clients also must use corba_ServantFinalizer for this 
 * to make it conceptionally work on non ilu orbs; but
 * this is not likely to be adobted by the omg anyway.  <p>
 *
 * @see corba_ServantFinalizer 
 */
public interface IluServantFinalizable {

    /** Use this instead of "finalize" */
    public void iluServantFinalize() throws java.lang.Throwable;
    
} //IluServantFinalizable
