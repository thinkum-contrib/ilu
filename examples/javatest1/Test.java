/* $Id: Test.java,v 1.18 1999/08/03 01:58:35 janssen Exp $
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
/* Chris Jacobi, December 2, 1998 4:24 pm PST */
/* Last edited by Mike Spreitzer October 9, 1998 1:33 pm PDT */

/* See the files Simple.isl and README */

package xerox.ilu.test1;

class TrueOB extends xerox.ilu.IluObjectBase implements Simple.TestObject {
    private int ac = 0;
    
    TrueOB(){
    }
    
    public int method1(int i) 
    		throws Simple.Exc1, Simple.Exc2 {
        System.out.println("< method1 called: arg = " + i);
        if (i == -1) {
            System.out.println("< will throw Exc1");
            throw new Simple.Exc1();
        }
        if (i == -2) {
            System.out.println("< will throw Exc2");
            throw new Simple.Exc2();
        }
        if (i == -3) {
            System.out.println("< will throw an undeclared exception");
            i = 17 / (3+i);
        }
        ac = ac + i;
        System.out.println("< will return: ac = " + ac);
        return ac;
    } //method1
    
} //TrueOB


public class Test {
    
    Test(){
    }
    
    static xerox.ilu.IluClassRep iluClass;
    static TrueOB trueOb = null;
    static xerox.ilu.IluServer server = null;
    static String serverId = "server-for-test-1";
    static String ih = "test-object-1";
    
    public static void clientSide() throws org.omg.CORBA.SystemException {
       Simple.TestObject remoteObject = null;
       System.out.println("< client side");
       try {
           remoteObject = (Simple.TestObject) 
               xerox.ilu.IluSimpleBinding.lookup(serverId, ih, iluClass);
           System.out.println("< got remote object " + remoteObject);
       } catch (Exception e) {
           System.out.println("< exception for lookupObject " + e);
           xerox.ilu.IluDebug.halt("exception...");
       }
       try {
           boolean caught = false;
           int ret = 0;
           
           System.out.println("< before calling method1");
           ret = remoteObject.method1(7);
           System.out.println("< method returned: " + ret);
           
           System.out.println("< before calling method1");
           ret = remoteObject.method1(7); 
           System.out.println("< method returned: " + ret);
           
           System.out.println("< before calling method1 exception case");
           try {
               caught = false;
               ret = remoteObject.method1(-1);
           } catch (Simple.Exc1 e) {
               caught = true;
               System.out.println("< caught excpected exception " + e);
           }
           if (! caught) {
               xerox.ilu.IluDebug.halt("< should have caught an exception");
           }
     
           System.out.println("< returned; did it raise the exception? ");
                      
       } catch (Exception e) {
           System.out.println("< unexpected exception " + e);
           xerox.ilu.IluDebug.halt("unexpected exception...");
       }
       
       System.out.println("< done");
    } //clientSide
    
    public static void serverSide() throws org.omg.CORBA.SystemException {
        System.out.println("< server side init");
        server = xerox.ilu.IluServer.createServer(serverId);
        System.out.println("< server created");
        
        System.out.println("< port created");
        trueOb = new TrueOB();
        System.out.println("< trueOb created");
        //
        Simple.TestObjectStub.registerTrueObject(
        	ih, trueOb, server
        	);
        System.out.println("< trueOb registered; sbh: " +
        	xerox.ilu.Ilu.sbhOfObject(trueOb));
        xerox.ilu.IluSimpleBinding.publish(trueOb);
        System.out.println("< sleep and serve requests");
    } //serverSide
    
    public static void main(String argv[]) {
       try {
           System.out.println("< Test start...");
           iluClass = Simple.TestObjectStub.iluClass();
           System.out.println("< iluClass built");
           if (argv.length == 0) {
               System.err.println("** server or client ? ");
           } else {
               String arg = argv[0];
               if (arg.equalsIgnoreCase("server")) {
                   serverSide();
               } else if (arg.equalsIgnoreCase("client")) {
                   clientSide();
               } else {
                   System.err.println("** server or client ?");
               }
           }
       } catch (org.omg.CORBA.SystemException e) {
           System.out.println("< CORBA SystemException caught at top level: " + e );
       }
    } //main
    
} //Test
