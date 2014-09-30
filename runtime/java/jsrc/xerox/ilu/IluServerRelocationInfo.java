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
/* xerox.ilu.IluServerRelocationInfo */
/* Chris Jacobi, December 23, 1998 12:20 pm PST */
/* $Id: IluServerRelocationInfo.java,v 1.6 1999/08/03 01:54:07 janssen Exp $ */
 
package xerox.ilu;

/*friendly*/ 

/**
 * ILU private class used for implementing
 * server side relocation. <p>
 *
 * @see IluServer
 */
 
final class IluServerRelocationInfo {
    
    /*friendly*/ IluServerRelocationInfo (IluServerRelocation checker) {
        this.pInfoContainer = new java.lang.String[1];
        this.tInfoContainer = new xerox.ilu.IluTransportInfo[1];
        this.jjRelocatonChecking = checker;
    } //constructor
    
       /**
        * Passed back to native caller of mustCheckRelocate.  
        * Java side must prevent garbage collection for as long as 
        * native side uses values.   Done by preventing overwriting
        * the jjSaveProtocolInfo and jjSaveTransportInfo fields
        * again (by holding the server lock when calling
        * mustCheckRelocate)  
        * Native side must extract values of its interest before
        * java garbage collector does any compactions.
        */
    /*friendly*/ java.lang.String jjSaveProtocolInfo = null;
    /*friendly*/ xerox.ilu.IluTransportInfo jjSaveTransportInfo = null;
    
    
       /**
        * Callback description.
        */
    /*friendly*/ xerox.ilu.IluServerRelocation jjRelocatonChecking = null;

       /**
        * Out arguments container passed into call-back method.
        */
    /*friendly*/ java.lang.String[] pInfoContainer = null;
    /*friendly*/ xerox.ilu.IluTransportInfo[] tInfoContainer = null;

    /*friendly*/ static final void init() {
    }
        
} //IluServerRelocationInfo



