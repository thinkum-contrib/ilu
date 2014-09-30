/* $Id: TestServer.java,v 1.22 1999/08/03 01:52:19 janssen Exp $
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
/* Chris Jacobi, January 6, 1999 11:06 pm PST */
/* Last edited by Mike Spreitzer October 9, 1998 1:35 pm PDT */

/*
 * Run this like
 *     java test01.TestServer
 */ 

package test01;
import Test1.*;

class T1O1Svr extends xerox.ilu.IluObjectBase implements TheO1 {
    static T1O2Svr theO2 = null; 
    static boolean flop = false;

    public TheU U_CSS_to_U(TheU u, String[] css)
        throws E1, E2, org.omg.CORBA.SystemException
    {
        TestServer.say("Test1.O1.U-CSS-to-U");
        return u;
    } //U_CSS_to_U
    
    public TheR f_CSS_to_RO(String[] css)
        throws E1, org.omg.CORBA.SystemException
    {
        TheR ans = new TheR();
        ans.i = 9;
        ans.css = new String[0];
        ans.a = new String[3];
        ans.a[0] = "hi";
        ans.a[1] = "hi";
        ans.a[2] = "hi";
        TestServer.say("Test1.O1.f-CSS-to-R0"); 
        return ans;
    } //f_CSS_to_RO
    
    public float R_ScS_to_F(TheR r, String s)
        throws org.omg.CORBA.SystemException
    {
        TestServer.say("Test1.O1.R-ScS-to-F");
        return 39.7F;
    } //R_ScS_to_F
    
    public void a_RO(TheR ro)
        throws org.omg.CORBA.SystemException
    {
        TestServer.say("Test1.O1.a_RO");
    } //a_RO
    
    public O2 get_O2()
        throws CantCreate, org.omg.CORBA.SystemException
    {
        if (theO2==null) {
            theO2 = new T1O2Svr();
            O2Stub.registerTrueObject(
                null,
                theO2, 
                TestServer.sunrpcServer);
        }
        
        return theO2;
    } //get_O2
    
        
    public O3 get_O3(boolean subclass)
        throws CantCreate, org.omg.CORBA.SystemException
    {
        TestServer.say("Test1.O1.get-O3");
        if (subclass) {
            TestServer.say("     get_O3 subclass");
            T3OSvrLead l = new T3OSvrLead();
            T3OSvrFolw f = new T3OSvrFolw();
            Test3.OStub.registerTrueObject(
                null, 
                l, 
                TestServer.theServer);
            Test2.PStub.registerTrueObject(
                null, 
                f, 
                TestServer.theServer);
            l.follower = f;
            f.leader = l;
            return l;
        } else if (flop) {
            TestServer.say("     get_O3 flop");
            flop = ! flop;
            T1O4Svr o = new T1O4Svr();
            O4Stub.registerTrueObject(
                null,
                o, 
                TestServer.theServer);
            return o;
        } else {
            TestServer.say("     get_O3 else");
            flop = ! flop;
            T1O3Svr o = new T1O3Svr();
            O3Stub.registerTrueObject(
                null,
                o, 
                TestServer.theServer);
            return o;
        }
    } //get_O3
    
} //T1O1Svr


class T1O2Svr extends xerox.ilu.IluObjectBase implements O2 {
        
    public String[] OO_A0_to_CSS(TheO1 o, byte[] a)
        throws E2, org.omg.CORBA.SystemException
    {
        TestServer.say("Test1.o2.OO-A0-to-CSS");
        if (o == null) {
            throw new E2(7);
        } else {
            return new String[0];
        }
    } //OO_A0_to_CSS
    
    public byte[] R_I_A1_to_I_A0(TheR r, org.omg.CORBA.IntHolder i, String[] a)
        throws org.omg.CORBA.SystemException
    {
        TestServer.say("Test1.o2.R-I-A1-to-I-A0");
        byte[] ans = new byte[8];
        for (int j = 0; j<8; j++) {
            ans[j] = (byte)j;
        }
        return ans;
    } //R_I_A1_to_I_A0 

} //T1O2Svr



class T1O3Svr extends xerox.ilu.IluObjectBase implements O3 {
    
    public int[] RS_R_to_R_IS(TheR[] r, TheRHolder r2)
        throws org.omg.CORBA.SystemException
    {
        TestServer.say("Test1.O3.RS-R-to-R-IS in T1O3Svr");
        TestServer.say("   r2 = " + r2 + " ..0= " + r2.value);
        r2.value = new TheR();
        r2.value.css = new String[0];
        r2.value.a = new String[3];
          r2.value.a[0] = "just";
        r2.value.a[1] = "a";
        r2.value.a[2] = "string";
        return new int[0];
    }
    
    public void O1_U_to_U(TheO1 o, TheUHolder u)
        throws E2, org.omg.CORBA.SystemException
    {
        TestServer.say("Test1.O3.O1-U-to-U");
        u.value = TheU.alloc_O1((short)3, o);
    }

    public int BS_to_I(byte[] b)
        throws org.omg.CORBA.SystemException
    {
        TestServer.say("Test1.O3.BS-to-I");
        return b.length * b.length;
    }

} //T1O3Svr

class T1O4Svr extends T1O3Svr implements O4 {
    
    public double R_to_R(double r)
        throws org.omg.CORBA.SystemException
    {
        double r2 = 1020304.05060708D;
        TestServer.say("Test1.O4.R-to-R(" 
            + r 
            + ") => "
            + r2
            + ")"
            );
        return r2;
    }
    
    public int[] RS_R_to_R_IS(TheR[] r, TheRHolder r2)
        throws org.omg.CORBA.SystemException
    {
        TestServer.say("Test1.O4.RS-R-to-R-IS in T1O4Svr");
        r2.value = new TheR();
        r2.value.i = 25719;
        r2.value.css = new String[0];
        r2.value.a = new String[3];
        r2.value.a[0] = "from";
        r2.value.a[1] = "p";
        r2.value.a[2] = "string";
        return new int[0];
    }


} //T1O4Svr


class T3OSvrLead extends T1O3Svr implements Test3.O {
    
    T3OSvrFolw follower = null;
    
    public TheU I_to_Test1U(int i)
        throws E1, E1, org.omg.CORBA.SystemException
    {
        TestServer.say("Test3.O.I-to-Test1U(" + i + ")");
        return TheU.alloc__boolean((short)5, true); 
    }
    
    public int[] RS_R_to_R_IS(TheR[] r, TheRHolder r2)
        throws org.omg.CORBA.SystemException
    {
        TestServer.say("Test1.O3.RS-R-to-R-IS  in T3OSvrLead");
        r2.value = new TheR();
        r2.value.i = 3;
        r2.value.css = new String[0];
        r2.value.a = new String[3];
        r2.value.a[0] = "just";
        r2.value.a[1] = "a";
        r2.value.a[2] = "string";
        return new int[0];
    }
    
    public int SR_to_I(float i) throws org.omg.CORBA.SystemException {
        TestServer.say("T3OSvrLead.SR_to_I(" + i + "); SHOULD NOT BE CALLED");
        return 9999;
    }

 
} //T3OSvrLead



class T3OSvrFolw extends xerox.ilu.IluObjectBase implements Test2.P {
    
    T3OSvrLead leader = null;
    
    public int SR_to_I(float i) throws org.omg.CORBA.SystemException {
        TestServer.say("Test3.O.SR-to-I(" + i + ")");
        return Math.round(i);
    }

  
} //T3OSvrFolw


public class TestServer { 
    
    static xerox.ilu.IluServer theServer;
    static xerox.ilu.IluServer sunrpcServer;
    static TheO1 uc;
    
    public static void say(String s) {
         System.out.println(s);
    }
    
    static void fail(String s, Exception e) {
         System.err.println(s + e);
         e.printStackTrace(System.err);
         //xerox.ilu.IluDebug.halt("xxx");
         System.exit(1);
    }
       
    static void Start() {
        String sbh;
        String mstid;
        org.omg.CORBA.Object o2;
	String sunrpc_pinfo = "sunrpc";
	String[] sunrpc_tinfo_text = { "sunrpcrm", "tcp_0_0" };
	xerox.ilu.IluTransportInfo sunrpc_tinfo = new xerox.ilu.IluTransportInfo(sunrpc_tinfo_text);
        try {
            MyObjectTable ot = new MyObjectTable();
	      
            theServer = xerox.ilu.IluServer.createServer("Test1-Server");
            theServer.setObjectTable(ot);
            // ?? need to set ports...
            theServer.finishInitialization();

            sunrpcServer = xerox.ilu.IluServer.createServer("Test1-Server-SunRPC");
	    sunrpcServer.createPort(sunrpc_pinfo, sunrpc_tinfo, null);
            theServer.finishInitialization();
            
            uc = new T1O1Svr();
            TheO1Stub.registerTrueObject("Test1_Initial_Object", uc, theServer);
            
            sbh = xerox.ilu.Ilu.sbhOfObject(uc);
            mstid = xerox.ilu.IluClassRep.fromIluObject(uc).iluClassName();
            say("Created and exported '" + sbh + "' '" + mstid + "'");
            xerox.ilu.IluSimpleBinding.publish(uc);
            say("Published it too.");
            try {
                o2 = (org.omg.CORBA.Object) xerox.ilu.IluSimpleBinding.lookup(
                    "Test1-Server", 
                    "Test1_Initial_Object", 
                    TheO1Stub.iluClass()
                    );
                if (uc==o2) {
                    say("Lookup returned same object.");
                } else {
                    say("Lookup returned different object!");
                }
            } catch (org.omg.CORBA.SystemException e) {
                fail("Lookup failed ", e);
            }
        } catch (org.omg.CORBA.SystemException e) {
            fail("caught ", e);
        }
    } //Start
    
    public static void main(String argv[]) {
        Start();
    } //main
    
} //TestServer

class MyObjectTable extends xerox.ilu.IluObjectTable {
    protected void createTrueObject(String ih) {
        org.omg.CORBA.Object tobj = null;
        xerox.ilu.IluClassRep iluClass = null;
        // Don't do it...
        //this.returnTrueObject(
        //    tobj, iluClass, 
        //    xerox.ilu.IluObjectTable.remember, null
        //    );
    } //createTrueObject
} //MyObjectTable
