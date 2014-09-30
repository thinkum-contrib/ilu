/* IluPreLoad.java */
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
/* Chris Jacobi, October 26, 1998 5:02 pm PST */
/* $Id: IluPreLoad.java,v 1.5 1999/08/03 01:54:10 janssen Exp $ */

package xerox.ilu;

/** 
 * Helper class which takes care of delayed initializations for ILU. <p>
 *
 * The purpose of this class is to enable stubs to be used locally before
 * Ilu is initialized and enqueue initializations for remote use later
 * in case and after Ilu has been initialized.
 *
 */
 
public class IluPreLoad {
    
    public IluPreLoad() {
    } //constructor
    
    
    /**
     * Closure for possible action to be performed.
     */
    public void postLoadAction() {
    } //constructor
    
    
    private static boolean delayStopped = false;
    private static java.util.Hashtable table = new java.util.Hashtable();
    private static xerox.basics.Queue queue = new xerox.basics.Queue();
    

    private static final void perform1(Object ob) {
        if (ob == null) { 
           return;
        } else if (ob instanceof IluPreLoad) {
           ((IluPreLoad) ob).postLoadAction();
        } else if (ob instanceof java.lang.String) {
           xerox.basics.Environment.loadClasses((java.lang.String) ob);
        }
    } //perform1
    
    
    /**
     * Called by Ilu so that IluPreLoad performs its stuff.
     */
    /*friendly*/ static final void stopDelaying() {
        while (true) {
            java.lang.Object ob = null;
            synchronized (queue) {
                delayStopped = true;
                if (queue.empty()) {
                    table = null;
                    return;
                }
                ob = queue.removeRight();
            }
            if (ob != null) {perform1(ob);}
        }
    } //stopDelaying


    /**
     * Load the argument class after ilu is initialized.
     */
    public static final void enqueueLoad(String className) {
        synchronized (queue) {
            if (!delayStopped) {
                //avoid growing data structure reloading the same class
                if (table.get(className) == null) {
                    table.put(className, className);
                    queue.insertLeft(className);
                }
                className = null; 
            }
        }
        if (className != null) {perform1(className);}
    } //enqueueLoad
    

    /**
     * Perform the argument action after ilu is initialized.
     */
    public static final void enqueueAction(IluPreLoad action) {
        synchronized (queue) {
            if (!delayStopped) {
                queue.insertLeft(action);
                action = null;
            }
        }
        if ( action!= null) {perform1(action);}
    } //enqueueAction
    
    
    private static java.lang.String stubVersionString = null;
    

    /**
     * Prevent combining stubs which don't fit together.
     */
    public static final void checkStubConsistency13(java.lang.String s) {
        if (stubVersionString == null) {stubVersionString = s;}
        if (! stubVersionString.equals(s)) {
            throw new java.lang.LinkageError(
                "Combining stubs from " + s + " and " + stubVersionString 
                );
        }
    } //checkStubConsistency13
    
    
} // IluPreLoad



