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
/* IluSurrogateConnection.java */
/* Chris Jacobi, December 4, 1998 12:50 pm PST */
/* $Id: IluSurrogateConnection.java,v 1.18 1999/08/03 01:54:05 janssen Exp $ */
 
 
/*
 * Native implementations in IluJava_IluConnOps.c
 */


package xerox.ilu;

/**
 * IluSurrogateConnection represents connections for surrogates.
 * NOT client visible
 */
final class IluSurrogateConnection {
    
    private static boolean classInit = false;
    private static int tracePCT = IluDebug.tracePCT();
    /*friendly*/ long ySurrConn = 0; //encodes ilu_Connection
    
    /** 
     * The kernel handles one outgoing connection.
     * Uses "this.ySurrConn" 
     */
    /*friendly*/ native void nativeOutgoingConnectionThreadProc() 
        throws org.omg.CORBA.SystemException;

    /** Call at start up time */
    /*friendly*/ native void nativeNewConnGetterForked();

    /** Get those connections which need to be handled */
    /*friendly*/ native boolean nativeOtherNewConnection();
        
    /* 
     * Clients don't create IluSurrogateConnection's; they 
     * rather use forkSC. 
     */
    /*friendly*/ IluSurrogateConnection() {
    } //constructor
    
    /** 
     * Provided for calls by native code.
     * Called "dynamic" because I didn't succeeded with calling static methods;
     */
    /*friendly*/ void dynamicForkSC(long surrConn) {
        forkSC(surrConn);
    } 
    
    
    /** 
     * Forks a new surrogate connection worker thread.
     */
    /*friendly*/ static void 
    forkSC(long surrConn) {
        /*$ idb */  if (tracePCT > 0) {
        /*$ idb */      IluDebug.log.println("! IluSurrogateConnection forkSC");
        /*$ idb */  }
        IluSurrogateConnection conn = new IluSurrogateConnection();
        conn.ySurrConn = surrConn;
        IluRegularSurrConnectionThread.fork(conn);
    } //forkSC
    
    
    /*friendly*/ static void init() {
        IluInit.init();
    }
    
    
    /** Initialization. Must be called after LockTech initialization */
    /*friendly*/ static void initPhase3() {
        if (!classInit) {
            //No locking.
            //Initialization done long before first clints 
            //can introduce threads
            classInit = true;  
            IluSurrogateConnection proto = new IluSurrogateConnection();
            IluOtherSurrogateConnForkerThread.fork(proto);
        }
    } //initPhase3
    
    
} //IluSurrogateConnection



/**
 * IluRegularSurrConnectionThread implements the thread handling a
 * surrogate connection.
 */
/*friendly*/ final
class IluRegularSurrConnectionThread extends java.lang.Thread {
    
    /* friendly */ IluSurrogateConnection worker;
    private static int priority = IluEnvironment.threadPriority();
    private static int count = 0; // used to provide unique thread name
    private static int tracePCT = IluDebug.tracePCT();

	/* returns and increments the classes object count */
    private static final synchronized long incrementCount() {
        return count++;
    }

    /* private: use fork to create IluRegularSurrConnectionThread's */
    private IluRegularSurrConnectionThread() {
        super("IluRegularSurrConnectionThread" + incrementCount());
        this.setDaemon(true);
        this.setPriority(priority);  
    } //IluRegularSurrConnectionThread
    
    static void fork(IluSurrogateConnection worker) {
        IluRegularSurrConnectionThread t = new IluRegularSurrConnectionThread();
        t.worker = worker;
        t.start();
    } //fork
    
    public void run() {
        /*$ idb */  if (tracePCT > 0) {
        /*$ idb */      IluDebug.log.println("! IluRegularSurrConnThread run " 
        /*$ idb */          + worker);
        /*$ idb */  }
        try {
            worker.nativeOutgoingConnectionThreadProc();
            /*$ idb */ if (tracePCT > 0) {
            /*$ idb */     IluDebug.log.println(
            /*$ idb */         "! nativeOutgoingConnectionThreadProc: return");
            /*$ idb */ }
        } catch (java.lang.Exception e ) {
            IluDebug.log.println(e.toString());
            IluDebug.iluPreError("** caught in connection forker..");
            //this is the bottom of the thread...
        }
    } //run
    
} // IluRegularSurrConnectionThread



/**
 * IluOtherSurrogateConnForkerThread implements a thread witch watches for
 * surrogate connection requests initiated by the Ilu kernel and then forks
 * handlers.
 */
/*friendly*/ final
class IluOtherSurrogateConnForkerThread extends java.lang.Thread {
    private IluSurrogateConnection proto; //there is exactly one instance
    private static int count = 0; // used to provide unique thread name

	/* returns and increments the classes object count */
    private static final synchronized long incrementCount() {
        return count++;
    }

    IluOtherSurrogateConnForkerThread() {
        super("IluOtherSurrogateConnForkerThread" + incrementCount());
        this.setDaemon(true);
        this.setPriority(IluEnvironment.threadPriority()); 
    } //IluOtherSurrogateConnForkerThread
    
    static void fork(IluSurrogateConnection proto) {
        IluOtherSurrogateConnForkerThread cft 
            = new IluOtherSurrogateConnForkerThread();
        cft.proto = proto;
        cft.start();
        xerox.basics.VMExtras.makeGCRoot(cft);
    } //fork
    
    public void run() {
        boolean mustFork;
        proto.nativeNewConnGetterForked();
        for (;;) {
            try {
                mustFork = proto.nativeOtherNewConnection(); //side-eff: i1, i2
                if (mustFork) {
                    proto.forkSC(proto.ySurrConn);
                }
            } catch (java.lang.Exception e ) {
                IluDebug.log.println("** Exception " + e);
                IluDebug.iluPreError("** caught in IluSurrConnWatcher...");
                //this is the bottom of a thread; keep going...
            }
        }
    } //run
    
} // IluOtherSurrogateConnForkerThread
