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
/* xerox.ilu.IluServerRelocation */
/* Chris Jacobi, December 23, 1998 12:19 pm PST */
/* $Id: IluServerRelocation.java,v 1.8 1999/08/03 01:54:00 janssen Exp $ */
 
package xerox.ilu;

/**
 * Implementing this interface allows a client to
 * request an IluServer to raise relocate "exceptions"
 * for protocols which support this feature. <p>
 * This is provided to allow for load-balancing and
 * inetd-like servers. <p>
 *
 * The checkIluServerRelocation method is called by ilu and 
 * implemented by clients.
 *
 * @see IluServer
 */
public interface IluServerRelocation {

    /** 
     * To request relocation pInfoContainer[0] and tInfoContainer[0] 
     * must be assigned proper values. 
     * If no assignments are made (or null is assigned) no
     * relocation will be signalled and the server will be
     * asked to serve "this" request. <p>
     *
     * This method is called while ilu holds a server lock; the client 
     * must not call back into ilu with operations using the same ilu 
     * server.
     */
    public void checkIluServerRelocation(
        xerox.ilu.IluServer server, 
        java.lang.String[] pInfoContainer, 
        xerox.ilu.IluTransportInfo[] tInfoContainer
        );
        
} //IluServerRelocation


