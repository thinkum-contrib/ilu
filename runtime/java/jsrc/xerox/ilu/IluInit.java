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
/* IluInit.java */
/* Chris Jacobi, December 23, 1998 12:09 pm PST */
/* $Id: IluInit.java,v 1.14 1999/08/03 01:54:01 janssen Exp $ */
 
/*
 * See IluInit2.java
 */


package xerox.ilu;

/** 
 * Helper class which takes care of initialization of ILU. <p>
 *
 * Initialization is deferred throw dynamic invokation of the loader
 * to break dependency circles.  There are no deep reason except
 * the speed up and frequency of recompilations.
 */
 
public class IluInit {
    
    /* Flag to prevent recursive initializations */
    private static int initState = 0;
    private static int abortInit = 0;
    private static java.lang.Thread initThread = null;
    
    static {
        init();
    }

    /* 
     * Monitored to initialize just once.
     */
    private static final synchronized int swapState() {
       int wasinit = initState;
       if (initState == 0) {
           //Initialization not yet started; start it
           initState = -1;
           initThread = java.lang.Thread.currentThread();
           return 0; //We need to initialize
       }
       if (initState == 1) {
           //Initialization already finished; ignore call
           return 1; 
       }
       //Initialization in progress; check why
       if (initThread == java.lang.Thread.currentThread()) {
           //Same thread; recursive call; ignore call
           return 2;
       } else {
           //Different thread; need to wait
           return 3;
       }
    } //swapState
    
    private static final synchronized void declareVictory() {
        initThread = null;
        initState = 1;
    } //declareVictory
    
    /** Initialization procedures.  Idempotent */
    public static final void init() {
        if (initState != 1) {
            int oldState = swapState();
            switch (oldState) {
                case 1: {
                    //Already initialized
                    return;
                }
                case 2: {
                    //This thread is initializing
                    return;
                }
                case 3: {
                    //Other thread is initializing
                    //Rare case if multiple threads already in initialization.
                    while (oldState == 3) {
                        try {
                            java.lang.Thread.sleep(200);
                        } catch (java.lang.InterruptedException e) {
                        }
                        oldState = swapState();
                    }
                    return;
                } 
                case 0: {
                    //we need to initialize
                    if (abortInitializations()){
                        declareVictory();
                        return;
                    }
                    try {
                        doInitialize();
                    } finally {
                        declareVictory();
                    }
                }
            }
        }
    } //init

    private static final void doInitialize() {
        java.lang.Class clazz = null;
        java.lang.Object notused = null;
        try {
            clazz = java.lang.Class.forName("xerox.ilu.IluInit2");
        } catch (java.lang.ClassNotFoundException e) {
            java.lang.System.err.println("** failed initializing Ilu" + e);
        }
        try {
            notused = clazz.newInstance();
        } catch (java.lang.IllegalAccessException e) {
            java.lang.System.err.println("** failed initializing Ilu" + e);
        } catch (java.lang.InstantiationException e) {
            java.lang.System.err.println("** failed initializing Ilu" + e);
        }
    } //doInitialize
    
    
    /** 
     * Checkout whether initialization is required.  Idempotent.<p>
     * Returns "true" to abort initializations.  If it ever
     * returns "true" the loaded ilu doesn't need to be functional. <p>
     * This procedure is nexcessary to abort initialization
     * when stub compiling for RNI. 
     */
    public static final boolean abortInitializations() {
        if (abortInit != 0) {
            //abortInit has already been initialized
            return abortInit>0;
        }
        if (xerox.basics.Environment.getBooleanProp(
                "xerox.ilu.IluInit.abort", false)) {
             //abort initializatins!
             abortInit = 1; 
             java.lang.System.err.println("ABORT ILU INITIALIZATIONS");
             return true;
        }
        //normal case: DO initializations
        abortInit = -1;
        return false;
    }
    
    /** not public
     * Temporary hack used in RNI based ilu implementation only
     */ 
    public static final int threadId () {
        return ((java.lang.Thread.currentThread()).getName()).hashCode();
    }
    
} // IluInit

