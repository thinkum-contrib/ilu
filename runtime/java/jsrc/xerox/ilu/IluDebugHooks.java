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
/* IluDebugHooks.java */
/* Chris Jacobi, December 23, 1998 12:02 pm PST */
/* $Id: IluDebugHooks.java,v 1.17 1999/08/03 01:53:47 janssen Exp $ */
 
/*
 * APPLET NOTE: 
 * I expect the classloader to deny access to this package from applets
 * Or, the procedure setRaiseDebugHook must not be public.
 *
 * Native stuff in IluJava_IluDebug.c
 *
 */

package xerox.ilu;
import java.lang.System;

/** 
 * For debugging Ilu only <p>
 * @see IluDebug
 */
public class
IluDebugHooks {

    /**
     * Constructor...
     */
    /*friendly*/ IluDebugHooks() {
    } //constructor
    
    
    /**
     * We need an instance on the native side
     * to be able to invoke methods.
     */
    private native void registerTheInst();
    private static IluDebugHooks theInst = new IluDebugHooks();


   /*friendly*/ static void initPhase2() {
        theInst.registerTheInst();
        if (IluDebug.setRaiseDebugHookFlag() > IluDebug.dont) {
            setRaiseDebugHook(theInst);
        }
        defineAssertionFailure(theInst);
        defineCheckFailure(theInst);
    } //initPhase2
    
    
    /**
     * Reports an instance of this class to enable
     * the native side to call the right reportAssertionFailure<p> 
     * Used for debugging IluJava only. 
     */
    /*friendly*/ static native void 
    defineAssertionFailure(IluDebugHooks x);
    
    /**
     * This is called from the native side
     * to report errors back into the java side.
     */
    protected void
    reportAssertionFailure(java.lang.String file, int line) {
        System.err.println("! **** assertion failure");
        System.err.println("!  file: " + file + " line: " + line);
        IluDebug.panic("AssertionFailure reported from kernel"); //no return...
    } //reportAssertionFailure
    
    
    /**
     * Reports an instance of this class to enable 
     * the native side to call the right reportCheckFailure<p> 
     * Used for debugging IluJava only. > 
     * (Like ilu_SetCheckFailureConsumer) 
     */
    /*friendly*/ static native void  
    defineCheckFailure(IluDebugHooks x);
    
    /**
     * This is called from the runtime support side to report 
     * when a (kernel) internal consistency check fails.<p>
     */
    protected void
    reportCheckFailure(java.lang.String file, int line) {
        System.err.println("! **** internal consistency check");
        System.err.println("!  file: " + file + " line: " + line);
        try {
            throw new org.omg.CORBA.TRANSIENT("print the stack trace");
        } catch (org.omg.CORBA.TRANSIENT e) {
            e.printStackTrace();
            // We are not waiting but hogg the cpu so this is where the debugger
            // will interrupt us...
            IluDebug.panic("kernel consistency"); //no return...
        }
    } //reportCheckFailure

       
    /**
     * Reports an instance of this class to enable 
     * the native side to call the right reportDebugHook<p> 
     * Intended for debugging by ILU maintainers only. 
     */
    /*friendly*/ static native void setRaiseDebugHook(IluDebugHooks x);
    
    
    /**
     * Called from native code when an error is being raised in the ilu 
     * kernel.  Should return without doing anything (visible to the
     * kernel).  Intended for debugging by ILU maintainers only. 
     */
    protected void
    reportDebugHook(java.lang.String file, int line, int errorType) {
        if (IluDebug.traceGeneric() > IluDebug.dont) {
             System.err.println("! **** ILU error " 
                 + " errorType [" + file + "] line " + line
                 );
        }
    } //reportDebugHook
    
    
    static {
        IluInit.init();
        xerox.basics.VMExtras.makeGCRoot(theInst);
    } //static
    
    /*friendly*/ static final void init() {
    } //init
    
} // IluDebugHooks
