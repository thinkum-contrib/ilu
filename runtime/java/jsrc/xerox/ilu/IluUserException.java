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
/* IluUserException.java */
/* Chris Jacobi, December 23, 1998 12:28 pm PST */
/*
 * $Id: IluUserException.java,v 1.12 1999/08/03 01:53:46 janssen Exp $
 */

package xerox.ilu;

/**
 * Base class for all ilu user exceptions <p>
 * - Applications may catch this exception <br>
 * - This is used for subclassing by the stubber as base 
 *   class for client specified exceptions.  <p>
 *
 * This class does not initialize ilu and can be used "alone".
 */
 
abstract public 
class IluUserException extends org.omg.CORBA.UserException {

    protected IluUserException() {
    } //constructor
    
    /** NOT really public; must be accessable by stubs and runtime */
    protected void readException(java.lang.Object call) {
        //Stubs overload this method, if the exception has arguments...
        //"call" is xerox.ilu.IluCall, but we want to
        //allow usage of this class to clients without need
        //to load xerox.ilu.IluCall
    } //readException
     
} // IluUserException 
