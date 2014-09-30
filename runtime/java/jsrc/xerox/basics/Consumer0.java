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
/* Consumer0.java  */
/* Chris Jacobi, April 21, 1997 10:01 am PDT */

/*
 */
 
/* $Id: Consumer0.java,v 1.5 1999/08/03 01:53:37 janssen Exp $ */

package xerox.basics;

/**
 * A simple queue with a thread which consumes all elements provided. <p>
 *
 * Clients are expected to subclass the consume method.<br>
 * Clients should not exploit the fact that Consumer0 is a Thread; they
 * should assume that Consumer0 has a Thread. <br> 
 * Consume is called by a single deamon thread in the order by which
 * objects have been provided. <p>
 *
 * As the null in the name of the class suggests, this is a simple
 * version:<br>
 * The implementation is heavy weight as it requires a permanent thread,
 * and, prevents garbage collection of the Consumer0.  It doesn't even
 * try any batching yet...
 * 
 * @author Chris Jacobi
 */

public abstract class Consumer0 extends Thread {
    private xerox.basics.Queue queue;
    private boolean running = false; //To check correct usage
    
    /** Constructor.<p>
     * Arguments are priority and name for thread.
     * 0 priority means don't assign priority. 
     */
    protected Consumer0(int priority, String name) {
        super(name);
        queue = new xerox.basics.Queue();
        this.setDaemon(true);
        if (priority>=Thread.MIN_PRIORITY) this.setPriority(priority); 
        this.start();
    } //Consumer0

    /** Subclass consume to implement the actual consuming action */
    protected abstract void consume(java.lang.Object obj);
    
    /** enqueue obj to be consumed */
    public final void provide(java.lang.Object obj) {
        synchronized (queue) {
            if (obj != null) queue.insertLeft(obj);
            queue.notify(); //there is only one thread
        }
    } //provide
    
    private final java.lang.Object get() {
        synchronized (queue) {
            for (;;) {
                java.lang.Object ob = queue.removeRight();
                if (ob != null) return ob;
                try {
                    queue.wait();
                } catch (java.lang.InterruptedException e) {
                }
            }
        }
    } //get
    
    /** Not public at all */
    public final void run() {
        if (running || queue==null) {
            //running => started twice.
            //queue==null => wrong (superclass) constructor used.
            // Not synchronized because it only serves for error
            // detection and does not influence outcome of usage 
            // by correct clients.
            throw new RuntimeException("Consumer0 started badly");
        }
        running = true;
        for (;;) {
            java.lang.Object ob = get();
            consume(ob);
        }
    } //run
   
} //Consumer0 

