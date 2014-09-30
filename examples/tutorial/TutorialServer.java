/* $Id: TutorialServer.java,v 1.20 1999/08/03 01:57:22 janssen Exp $
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
/* Chris Jacobi, November 23, 1998 5:39 pm PST */
/* Last edited by Mike Spreitzer October 9, 1998 1:48 pm PDT */

/*
 * Run this like
 * java Tutorial.TutorialServer servername
 */ 
 
/* $Id: TutorialServer.java,v 1.20 1999/08/03 01:57:22 janssen Exp $ */

package Tutorial;

class FactoryImpl 
        extends xerox.ilu.IluObjectBase 
        implements Tutorial.Factory  {
    xerox.ilu.IluServer server;
    public FactoryImpl(xerox.ilu.IluServer server) {
        this.server = server;
    }
    public Tutorial.Calculator CreateCalculator() 
                throws org.omg.CORBA.SystemException
    {
        Tutorial.Calculator calc = new Tutorial.CalculatorImpl();
        xerox.ilu.Ilu.registerTrueObject(
            xerox.ilu.Ilu.inventID(), 
            calc, 
            this.server, 
            Tutorial.CalculatorStub.iluClass(),
            xerox.ilu.Ilu.unspec
            );
        return calc;
    }
} //FactoryImpl


public class TutorialServer {
    static FactoryImpl factory;
    static xerox.ilu.IluServer trueServer;
    public static void main(String argv[]) {
        try {
            String serverId;
            if (argv.length < 1) {
                 System.out.println("Must specify a server id");
                 return;
            }
            //Create a server with appropriate server id (which is
            //taken from the first argument) 
            serverId = argv[0];
            trueServer = xerox.ilu.IluServer.createServer(serverId);
            //Now create an instance of a Factory object on the server
            //with an instance handle "theFactory"
            factory = new FactoryImpl(trueServer);
            xerox.ilu.Ilu.registerTrueObject(
                "theFactory", 
                factory, 
                trueServer, 
                Tutorial.FactoryStub.iluClass(),
                xerox.ilu.Ilu.remember
                );
            //Make the factory well known by publishing it
            xerox.ilu.IluSimpleBinding.publish(factory);
            //Now we print the string binding handle (the object's name
            //plus its location) of the new Factory instance
            System.out.println("Factory instance published");
            System.out.println("Its SBH is '" + xerox.ilu.Ilu.sbhOfObject(factory) + "'");
            //the program doesn't terminate because the server is still alive...
        } catch (org.omg.CORBA.SystemException e) {
            System.out.println("raised CORBA.SystemException: " + e);
        }
    }
} //TutorialServer
