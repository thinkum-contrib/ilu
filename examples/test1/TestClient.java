/* $Id: TestClient.java,v 1.21 1999/08/03 01:52:23 janssen Exp $
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
/* Chris Jacobi, November 13, 1998 4:15 pm PST */
/* Last edited by Mike Spreitzer October 9, 1998 1:35 pm PDT */

/*
 * Run this like
 *     java test01.TestClient
 */ 

package test01;

public class TestClient {
    
    static {
        Test1._allJavaStubs.load();
        Test3._allJavaStubs.load();
        Test3._allJavaStubs.load();
    }
    
    static void say(String s) {
         System.out.println(s);
    }
    
    static void fail(String s, Exception e) {
         System.err.println(s + e);
         e.printStackTrace(System.err);
         //xerox.ilu.IluDebug.halt("xxx");
         System.exit(1);
    }
    
    static String FmtU(Test1.TheU u) {
        //try {
            int d = u.discriminator();
            switch (d) {
                case 0: return "R[...]";
                case 1: return "R0[...]";
                case 2: return "CSS[...]";
                case 3: return "O1[" 
                    + xerox.ilu.Ilu.sbhOfObject(u.get_O1()) 
                    + "]";
                case 4: return "OO[...]";
                case 5: return "BOOLEAN[" + u.get__boolean() + "]";
                default: return "[bad U variant; d = "+ d +"]";
            }
        //} catch (xerox.ilu.IluException e) {
        //    fail("FmtU failed: ", e);
        //}
    } //FmtU
    
    static void loadIluClasses() {
        //ilu classes must be registered before objects
        //of these classes are received...
        Object loadThis = null;
        loadThis = Test2.PStub.iluClass();
        loadThis = Test3.OStub.iluClass();
        loadThis = Test1.O4Stub.iluClass();
    }
    
    public static void main(String argv[]) {
        Test1.TheO1 handle = null;
        Test1.O2 o2;
        Test1.O3 o3;
        double f;
        Test1.TheU u, u2;
        Test1.TheUHolder uholder = new Test1.TheUHolder();
        String [] css, css2;
        Test1.TheR ro; //optional
        Test1.TheR r;
        Test1.TheRHolder rholder = new Test1.TheRHolder();
        byte[] a = null;
        String [] a1;
        int i = 0;
        org.omg.CORBA.IntHolder iholder = new org.omg.CORBA.IntHolder();
        Test1.TheR[] rs;
        int[] i2;
        xerox.ilu.IluClassRep o3t, tT1O3;  
        double r1, r2;
        
        loadIluClasses();
        try {
            handle = (Test1.TheO1) xerox.ilu.IluSimpleBinding.lookup(
                "Test1-Server", 
                "Test1_Initial_Object", 
                Test1.TheO1Stub.iluClass()
                );
        } catch (org.omg.CORBA.SystemException e) {
            fail("Lookup failed: ", e);
        }
        
        try {
            
            css = new String [2];
              css[0] = "hello world";
              css[1] = "hello mars";
            u = Test1.TheU.alloc__boolean(true);
            u = handle.U_CSS_to_U(u, css);
            say("u = " + FmtU(u));

            ro = handle.f_CSS_to_RO(css);
            say("ro->i = " + ro.i);

            f = handle.R_ScS_to_F(ro, css[0]);
            say("f = " + f);

            handle.a_RO(ro);
            o2 = handle.get_O2();
            say("got O2, sbh = " 
                + xerox.ilu.Ilu.sbhOfObject(o2));
            
            a = new byte[8];
            //contents need not be initialized
            say("DDD  a = new byte");
            css2 = o2.OO_A0_to_CSS(handle, a);
            say("got css2");
            
            r = new Test1.TheR(
                new String[3],	//a
                new String[0],	//css
                12		//i
                );
              r.a[0] = "this is";
              r.a[1] = "data";
              r.a[2] = "initialization";
            i = 0;
            a1 = new String[3];
              a1[0] = "but this";
              a1[1] = "is";
              a1[2] = "fun";
            iholder.value = i;
            a = o2.R_I_A1_to_I_A0(r, iholder, a1);
            i = iholder.value;
            say("got a; i = " + i);
            
            o3 = handle.get_O3(false);
            say("got o3, sbh = " + xerox.ilu.Ilu.sbhOfObject(o3));
            
            o3t = xerox.ilu.IluClassRep.fromIluObject(o3);
            tT1O3 = Test1.O3Stub.iluClass(); 
            if (o3t != tT1O3) {
                say("Instance of type " + o3t.iluClassName() + " received");
            } else {
                rs = new Test1.TheR[0];
                rholder.value = r;
                i2 = o3.RS_R_to_R_IS(rs, rholder);
                r = rholder.value;
                say("got i2");
                
                // sibling not implemented yet
                uholder.value = u; 
                o3.O1_U_to_U(handle, uholder);
                u = uholder.value;
                say("u = " + FmtU(u));
            }
            
            o3 = handle.get_O3(true);
            say("got o3, sbh = " + xerox.ilu.Ilu.sbhOfObject(o3));
            
            rs = new Test1.TheR[0];
            rholder.value = r;
            i2 = o3.RS_R_to_R_IS(rs, rholder);
            r = rholder.value;
            say("got i2");
            
            uholder.value = u;
            o3.O1_U_to_U(handle, uholder);
            u = uholder.value;
            say("u = " + FmtU(u));
            
            if (o3 instanceof Test3.O) {
                Test3.O x = (Test3.O) o3;
                u2 = x.I_to_Test1U(397);
                say("u2 = " + FmtU(u2));
            }
            
            o3 = handle.get_O3(false);
            say("got O3, sbh = " 
                + xerox.ilu.Ilu.sbhOfObject(o3));
            
            if (o3 instanceof Test1.O4) {
                Test1.O4 x = (Test1.O4) o3;
                r1 = 12345.6789D;
                r2 = x.R_to_R(r1);
                say("doubles:  r1 is  " 
                    + r1 
                    + ", r2 is " 
                    + r2);
            }
        } catch (org.omg.CORBA.SystemException e) {
            fail("TestClient raised IluSystemException: ", e);
        } catch (xerox.ilu.IluUserException e) {
            fail("TestClient raised IluUserException: ", e);
        }
    } //main
    
} //TestClient





