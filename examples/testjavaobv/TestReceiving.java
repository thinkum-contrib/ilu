/* $Id: TestReceiving.java,v 1.2 1999/08/03 01:59:10 janssen Exp $
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
/* Chris Jacobi, December 26, 1998 8:49 pm PST */
/* Last edited by Mike Spreitzer October 9, 1998 1:31 pm PDT */

package test_client; 


/**
 * This class is the client side of a sample application of 
 * full custom mapping.
 */
class TestReceiving {
      
    static testJavaSerialObjects.ObjectGetter getter;
        
    static void handleIn() {
        try {
            java.lang.Object obj;
            System.out.println("Looking up a getter");
            getter = (testJavaSerialObjects.ObjectGetter) 
                xerox.ilu.IluSimpleBinding.lookup(
                    "testJavaSerialServer", 
                    "myFactory", 
                    testJavaSerialObjects.ObjectGetterStub.iluClass()
                    );
            System.out.println("Got a getter " + getter);
            //
            //Get the first object ...
            obj = getter.GetIt();
            System.out.println("First returned object is: " + obj);
            //
            //Get a second object ...
            obj = getter.GetIt();
            System.out.println("Second returned object is: " + obj);
            //
            //Get a third object ...
            obj = getter.GetIt();
            System.out.println("Third returned object is: " + obj);
            //
            //Get a fouth object ...
            obj = getter.GetIt();
            System.out.println("Fourth returned object is: " + obj);
            //
        } catch (org.omg.CORBA.SystemException e) {
            System.err.println("Caught an ilu exception: " + e );
        }
    } //handleIn

    public static void main(String argv[]) {
        System.out.println("Start TestReceiving...");
        //
        //Making sure test program has its stubs loaded
        testJavaSerialObjects._allJavaStubs.load();
        //
        //Telling ilu that it may load classes through the wire
        xerox.ilujava.IluOBVWireClassLoader.load();
        System.out.println("wire-class-loader loaded");
        //
        //Telling ilu that it may load classes through URL's
        if (argv.length>0) {
            try {
                xerox.ilujava.IluOBVURLClassLoader.load();
                xerox.ilujava.IluOBVURLClassLoader.addURLToPath(
                    new java.net.URL(argv[0])
                    );
                System.out.println("URL-class-loader for " 
                    + argv[0] + " loaded"
                    );
            } catch (Exception e) {
                System.out.println(
                    "**Couldn't register IluOBVURLClassLoader" + e
                    );
            }
        }
        //
        System.out.println("All the junk is loaded");
        handleIn();
    } //main

} //TestReceiving

