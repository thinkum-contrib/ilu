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
/* VMExtras.java */
/* Chris Jacobi, November 4, 1997 11:18 am PST */

/*
 */
 
/* 
 * $Id: VMExtras.java,v 1.3 1999/08/03 01:53:37 janssen Exp $ 
 */
 
package xerox.basics;

/**
 * Basic vm level primitives <br>
 * (Dealing with garbage collection and preemting threads)
 */
public class VMExtras {

    /** instance variables; all private */
    private java.lang.Object ob = null;
    private VMExtras next = null;

    /** 
     * static variables; all private; 
     * also uses variables from VMExtrasThread
     */
    private static java.util.Hashtable classTable = new java.util.Hashtable();
    
    /** no clients direct use */
    private VMExtras() {
    }
    
    /** Remembers its argument so that it will never be garbage collected */
    public static synchronized void makeGCRoot(java.lang.Object ob) {
        if (ob instanceof java.lang.Class) {
            //don't allocate memory for duplicates of 
            //classes because we plan on remembering
            //classes multiple times... 
            java.lang.Object old = classTable.put(ob, ob);
            if (old == null) return;
            if (old == ob) return;
            ob = old;
        }
        VMExtras cont = new VMExtras();
        cont.ob = ob;
        cont.next = VMExtrasThread.anchor;
        VMExtrasThread.anchor = cont;
    } // makeGCRoot
    
    static {
        VMExtrasThread.init();
        makeGCRoot(classTable);
    } //static
    
} // VMExtras


/**
 * Noop-thread which wakes up periodically to force the stupid
 * java scheduler to do time slicing
 */
/*friendly*/ final class VMExtrasThread extends java.lang.Thread {
    private static int interval = 
        xerox.basics.Environment.getIntProp("timeslice", 150);
    /** Accessed synchronization set in VMExtras only */ 
    /*friendly*/ static VMExtras anchor = null;
            
    /** 
     * This is NOT really public, but the signature of java.lang.Thread
     * requires the public specifier.
     */
    public void run () {
        if (interval <= 0) {
            interval = 1000000; 
        }
        for (;;) {
            try {
                java.lang.Thread.sleep(interval);
            } catch (java.lang.InterruptedException e) {
            }
        }
    } //run
    
    /* Called once only */
    /*friendly*/ static void init() {
    } //init
    
    static {
        VMExtrasThread theOneInst = new VMExtrasThread();
        theOneInst.setPriority(java.lang.Thread.MAX_PRIORITY);
        theOneInst.setDaemon(true);
        theOneInst.start();
    }
} // VMExtrasThread


