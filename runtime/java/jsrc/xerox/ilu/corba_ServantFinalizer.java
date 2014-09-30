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
/* xerox.ilu.corba_ServantFinalizer.java */
/* Chris Jacobi, June 30, 1998 9:31 am PDT */

 
/* $Id: corba_ServantFinalizer.java,v 1.6 1999/08/03 01:54:07 janssen Exp $ */
 
package xerox.ilu;

/**
 * A servant object which wants to take part in the ServantFinalizable
 * "dance" has to call doFinalize in it finalization. <p>
 *
 * Given Ilu's current implementation this is a no-op however; it is
 * provided as it is assumed to be real in other ORBs.
 *
 * @see IluServantFinalizable
 */
public class corba_ServantFinalizer {
    public static void doFinalize(IluServantFinalizable obj) {
        //If Ilu were a trivial ORB, thats how this procedure would look
        //try {
        //    obj.iluServantFinalize(); 
        //} catch (java.lang.Throwable t) {
        //}
    } //doFinalize
}

