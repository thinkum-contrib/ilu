/* $Id: Client.java,v 1.5 1999/08/03 01:59:06 janssen Exp $
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
/* Chris Jacobi, November 30, 1998 4:38 pm PST */
/* Last edited by Mike Spreitzer October 9, 1998 1:30 pm PDT */

package hello_world;

/**
 * The hello_world client.
 * Usage: java hello_world.Client SBH-OF-SERVER
 */ 
public class Client {
    
    public static void main(String argv[]) {
        if (argv.length < 1) {
            System.err.println(
                "Usage: java hello_world.Client SBH-OF-SERVER"
                );
            System.exit(1);
        }
        String sbh = argv[0];
        String response = null;
        hello_world.service serv = null;
        try {
            serv = (hello_world.service) 
                xerox.ilu.Ilu.objectFromSBH(
                    sbh, hello_world.serviceStub.iluClass()
                    );
        } catch (org.omg.CORBA.SystemException e) {
            System.err.println("Can't get object for sbh " + sbh);
            System.err.println("Exception is: " + e);
            System.exit(1);
        }
        try {
            response = serv.hello_world();
        } catch (org.omg.CORBA.SystemException e) {
            System.err.println("Service fails");
            System.err.println("Exception is: " + e);
            System.exit(1);
        }
        System.out.println("The response is: " + response);
    } //main
    
} //client



