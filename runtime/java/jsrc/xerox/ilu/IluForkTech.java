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
/* IluForkTech.java */
/* Chris Jacobi, December 4, 1998 12:51 pm PST */

 
/* 
 * $Id: IluForkTech.java,v 1.7 1999/08/03 01:53:51 janssen Exp $
 * See IluJava_IluAlarmTech.c for the native implementation
 */
 
package xerox.ilu;

/**
 * Class to implement the Java side of ILU for tech... <br>
 * No Java clients anticipated.
 */
 
/*friendly*/ final
class IluForkTech extends IluServiceThread {
    
    private static int priority = IluEnvironment.threadPriority();
    
    private int x1; private int x2; //x1 .. x4 represent the proc and the arg 
    private int x3; private int x4;
   
    static {
        IluInit.init();
    }
    
    /** private because real creation is done in forkRequest */
    private IluForkTech() {
    } //constructor
      
      
    /** (called from C side) when requesting fork */
    /*friendly*/ void forkRequest(int x1, int x2, int x3, int x4) {
        IluForkTech ft = new IluForkTech();
        ft.x1 = x1; ft.x2 = x2;
        ft.x3 = x3; ft.x4 = x4;
        ft.setDaemon(true);
        ft.setPriority(priority);
        ft.start();
    } //forkRequest


    /** (called from C side) when it needs a permanent object */
    /*friendly*/ java.lang.Object givePermanentObject() {
        java.lang.Object ob = new java.lang.Object();
        xerox.basics.VMExtras.makeGCRoot(ob);
        return ob;
    } //givePermanentObject


    /** From java call back into C to execute the C procedure variable */
    private native void callForkee(int x1, int x2, int x3, int x4);
   
    /** Not public; publicness required by super class */
    public void run () {
        callForkee(this.x1, this.x2, this.x3, this.x4);
    } //run
    
    /** 
     * Native code needs an instance of an IluForkTech
     * which will not be garbage collected.
     */
    private native void registerForkTechProto();
    private static IluForkTech prototype = null; //must not be gc'ed

    /*friendly*/ static void initPhase2() {
        if (prototype == null) {
            prototype = new IluForkTech();
            prototype.registerForkTechProto();
            xerox.basics.VMExtras.makeGCRoot(prototype);
        }
    } //initPhase2
   
    /** Loads the code and executes static initializations if any */
    /*friendly*/ static void init() {
    } //init
    

} // IluForkTech

