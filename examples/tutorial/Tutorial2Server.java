/* $Id: Tutorial2Server.java,v 1.18 1999/08/03 01:57:24 janssen Exp $
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
/* Rick Yardumian December 16, 1996 */
/* Chris Jacobi, November 13, 1998 2:42 pm PST */
/* Last edited by Mike Spreitzer October 9, 1998 1:47 pm PDT */

/*
 * Run this like
 * java Tutorial2.Tutorial2Server servername
 */ 
 
package Tutorial2;

import xerox.ilu.Ilu;
import org.omg.CORBA.SystemException;
import xerox.ilu.IluServer;


class Factory2Impl 
    extends xerox.ilu.IluObjectBase 
    implements Tutorial2.Factory {
    
    static java.util.Hashtable gtable = new java.util.Hashtable();
    
    public Factory2Impl() {
        //in case of corba objects only
        gtable.put(this, this);
    }
    
    public Tutorial.Calculator CreateCalculator() 
                        throws org.omg.CORBA.SystemException {
        System.out.println("Factory2Impl: request for a simple calculator");
        Tutorial.Calculator tc = new Tutorial2.TapeCalculatorImpl();
        gtable.put(tc, tc);
        return tc;
    } //CreateCalculator
    
    public Tutorial2.TapeCalculator CreateTapeCalculator() 
                throws org.omg.CORBA.SystemException {
        System.out.println("Factory2Impl: request for a tape calculator");
        Tutorial2.TapeCalculator tc = new Tutorial2.TapeCalculatorImpl();
        gtable.put(tc, tc);
        return tc;
    } //CreateTapeCalculator
    
} //Factory2Impl


public class Tutorial2Server {
    static Factory2Impl factory;
    static xerox.ilu.IluServer trueServer;
    
    public static void main(String argv[]) {
        String serverId;
        if (argv.length != 1) {
            System.err.println("usage: java Tutorial2.Tutorial2Server servername");
            System.exit(1);
        }
        System.out.println("Create the server");
        try {
            //Create a server with appropriate server id (which is
            //taken from the first argument) 
            serverId = argv[0];
            trueServer = xerox.ilu.IluServer.createServer(serverId);
        } catch (org.omg.CORBA.SystemException e) {
            System.err.println("Failed creating server: " + e);
            System.exit(1);
        }
        System.out.println("Create the factory");
        try {
            //Now create an instance of a Factory object on the server
            //with an instance handle "theFactory"
            factory = new Factory2Impl();
            Tutorial2.FactoryStub.registerTrueObject( 
                "theFactory", 
                factory, 
                trueServer
                );
            //Make the factory well known by publishing it
            xerox.ilu.IluSimpleBinding.publish(factory);
        } catch (org.omg.CORBA.SystemException e) {
            System.err.println("Failed creating Factory: " + e);
            System.exit(1);
        }
        //Now we print the string binding handle (the object's name
        //plus its location) of the new Factory instance
        System.out.println("Factory instance published");
        System.out.println("Its SBH is '" + Ilu.sbhOfObject(factory) + "'");
        //the program doesn't terminate because the server is still alive...
    } //main 
    
} //Tutorial2Server

