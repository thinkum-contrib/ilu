/* $Id: Server.java,v 1.5 1999/08/03 01:59:06 janssen Exp $
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
/* Chris Jacobi, November 30, 1998 4:43 pm PST */
/* Last edited by Mike Spreitzer October 9, 1998 1:30 pm PDT */

package hello_world;

/**
 * The hello_world server.
 * Usage: java -Djava.compiler=NONE hello_world.Server 
 */ 
public class Server {
        
    public static void main(String argv[]) {
        xerox.ilu.Ilu.init();
        ServiceImpl service = new ServiceImpl();
        xerox.ilu.Ilu.registerTrueObject(
            xerox.ilu.Ilu.inventID(), 
            service, 
            xerox.ilu.IluServer.createServer(null), 
            hello_world.serviceStub.iluClass(),
            xerox.ilu.IluLifetimeArgs.iluLifetimeRemember
            );
        System.out.println(
            "hello world server is: \"" +  
            xerox.ilu.Ilu.sbhOfObject(service) +
            "\""
            );
    } //main
    
} //Server


/**
 * Implementing the actual service.
 */ 
class ServiceImpl  
    extends xerox.ilu.IluObjectBase 
    implements hello_world.service
{
    
    public String hello_world() {
        return "\"Hello, World!\" from Java";
    } //hello_world
    
} //ServiceImpl


