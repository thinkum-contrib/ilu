/* $Id: JaServ.java,v 1.11 1999/08/03 01:58:45 janssen Exp $
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
/* Chris Jacobi, December 2, 1998 4:22 pm PST */
/* Last edited by Mike Spreitzer October 9, 1998 1:35 pm PDT */
 
/*
 * Run this like "java Test.JaServ" or "java Test.JaServ hard"
 */ 
 
package Test;

public class JaServ {
    
    static public boolean hard = false;
    static Bouncer bouncer;
    static xerox.ilu.IluServer trueServer;
    
    public static void main(String argv[]) {
        if (argv.length >= 1) {
            hard = true;
            System.out.println("HARD");
        }
        System.out.println("Load the junk");
        Test2._allJavaStubs.load();
        System.out.println("Create the server");
        String[] info = {"sunrpcrm", "tcp_0_0"};
        xerox.ilu.IluTransportInfo tInfo = new xerox.ilu.IluTransportInfo(info);
        trueServer = xerox.ilu.IluServer.createServer("pickleServer");
        trueServer.createPort("sunrpc", tInfo, null);
        System.out.println("Create the bouncer object");
        bouncer = new Bouncer();
        Test2.O1Stub.registerTrueObject( 
                "pickleObj", 
                bouncer, 
                trueServer
                );
        System.out.println("Publish the bouncer object");
        xerox.ilu.IluSimpleBinding.publish(bouncer);
        System.out.println("Its SBH is '" 
            + xerox.ilu.Ilu.sbhOfObject(bouncer) 
            + "'");
    }  //main
    
} //JaServ


class Bouncer extends xerox.ilu.IluObjectBase implements Test2.O1 {
    
    public Bouncer() {}
    
    public xerox.ilu.IluAny bounce(xerox.ilu.IluAny v)
            throws org.omg.CORBA.SystemException {
        System.out.println("bounce called " + v);
        if (JaServ.hard) {
            xerox.ilu.IluTypeCode tc = (xerox.ilu.IluTypeCode) v.type();
            System.out.println("  TypeCode: " + tc);
            Object ob = v.value();
            System.out.println("  Object: " + ob);
            if (ob instanceof Test1.OO2) {
                Test1.OO2 oo2 = (Test1.OO2) ob;
                System.out.println("disc: "  + oo2.discriminator());
            }
            v = xerox.ilu.IluAny.alloc(tc, ob);
            System.out.println("  --");
        }
        return v;
    } //bounce
    
} //Bouncer


