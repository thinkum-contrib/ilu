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
/* IluServerConnection.java */
/* Chris Jacobi, December 23, 1998 12:19 pm PST */
/* $Id: IluServerConnection.java,v 1.24 1999/08/03 01:53:53 janssen Exp $ */
 
/*
 * Native implementations in IluJava_IluConnOps.c
 */

package xerox.ilu;

/**
 * IluServerConnection represents connections for true objects.<p>
 * NOT client visible
 *
 */
final
class IluServerConnection {
    
    private long yConn = 0; //native use
    private static int tracePCT = IluDebug.tracePCT();
    
    private static final void println(java.lang.String s) {
        IluDebug.log.println(s);
    } //println
  
    /* 
     * Private; use the static forkConnectionReader method to create 
     * new IluServerConnection
     */
    private IluServerConnection() {
    } //constructor
    
    
    /* Used to create connection for surrogate objects */
    /*friendly*/ static void forkConnectionReader(long yConn) {
        /*$ idb */  if (tracePCT > 0) {
        /*$ idb */      println("! IluServerConnection: fork");
        /*$ idb */  }
        java.lang.Thread.yield(); //give another thread a chance to finish 
        IluServerConnection cr = new IluServerConnection();
        cr.yConn = yConn;
        IluServerConnectionThread.fork(cr);
    } //forkConnectionReader
    
    
    /* Loop serving requests */
    /*friendly*/ void connectionReaderLoop(IluServiceThread serviceThread) {
        boolean conn_ok = true;
        try {
            IluCall callShell = xerox.ilu.IluCall.makeCallShell();
            while (conn_ok) {
                /*$ idb */  if (tracePCT > IluDebug.basic) {
                /*$ idb */     println("! IluServerConnection...: loop");
                /*$ idb */  }
                callShell.jjMethodRep = null;
                conn_ok = receiveRequest(callShell);
                /*$ idb */  if (tracePCT > IluDebug.basic) {
                /*$ idb */     println("! IluServerConnection...: received " 
                /*$ idb */         + conn_ok);
                /*$ idb */  }
                if (callShell.jMustFork) {
                    callShell.jMustFork = false;
                    IluServerConnectionExtraThread thread 
                        = new IluServerConnectionExtraThread(callShell);
                    /*$ idb */  if (tracePCT > 0) {
                    /*$ idb */      println("! IluServerConnection... fork Service");
                    /*$ idb */  }
                    thread.start();
                    callShell = xerox.ilu.IluCall.makeCallShell();
                } else {
                    serve1(callShell, serviceThread);
                }
            }
            /*$ idb */  if (tracePCT > IluDebug.basic) {
            /*$ idb */      println("! IluServerConnection...: loop done");
            /*$ idb */  }
        } catch (java.lang.Exception e ) {
            IluDebug.log.println("** Exception " + e);
            IluDebug.iluPreError("** caught in connection reader..");
            //this is the bottom of a thread
        }
    } //connectionReaderLoop
       
       
    /* serving one request */
    /*friendly*/ static void  
    serve1(IluCall call, IluServiceThread serviceThread) {
        IluMethodRep mRep = call.jjMethodRep;
        xerox.ilu.IluSkeleton skeleton = null;
        /*$ idb */  if (tracePCT > 1) {
        /*$ idb */     println("! IluServerConnection.serve1 mRep: " + mRep);
        /*$ idb */  }
        if (mRep != null) {skeleton = mRep.skeleton;}          
        if (skeleton == null) {
            /*$ idb */  if (tracePCT > 0) {
            /*$ idb */      IluDebug.log.println("! IluServerConnection no skeleton");
            /*$ idb */  }
            call.nativeMarkCallFailure(
                IluSystemExceptionBase.protocolException_NoSuchMethodOnClass
                );
            call.finishCall();
        } else {
            try {
                serviceThread.setSkeleton(call);
                skeleton.serveRequest(call, mRep);
            } catch (xerox.ilu.IluUnexpectedException e) {
                //A skeleton is expected to call a procedure which
                //raises this if it detects an unexpected exception 
                //raised by the servant.
                //
                //Note that we are printing the stack trace of the original
                //exception; not the stack trace of e.
                IluDebug.log.println(
                    "** Ilu stub caught exception thrown by servant object " 
                    + e.was);
                e.was.printStackTrace(IluDebug.log);
                IluDebug.clientPreError(
                    "** Ilu stub caught exception thrown by servant object"
                    );
                IluDebug.log.println("--");
            } catch (java.lang.Exception e) {
                //We don't trust the stubs as a means of debugging
                //and in case of applets there can be fake stubs; that
                //is why we can't rely on IluUnexpectedException being
                //the only exception encountered here.
                call.nativeMarkCallFailure(
                    IluSystemExceptionBase.protocolException_Unknown);
                IluDebug.log.println(
                    "** Ilu runtime caught exception thrown by servant object " 
                    + e);
                e.printStackTrace(IluDebug.log);
                IluDebug.clientPreError(
                    "** Ilu runtime caught exception thrown by servant object"
                    );
                IluDebug.log.println("---");
            } catch (java.lang.Error e) {
                //really bad, but give client a chance if we can
                call.nativeMarkCallFailure(
                    IluSystemExceptionBase.protocolException_Unknown);
                java.lang.System.err.println(
                    "** Ilu runtime caught error thrown by servant object"
                    );
                e.printStackTrace(java.lang.System.err);
                java.lang.System.err.println("---");
                if (e instanceof java.lang.ThreadDeath) {
                    throw e;
                }
            } finally {
                call.finishCall();
            }
        }
        if (call.needFinish != 0) {
            call.finishCall();
        }
    } //serve1
    
    
    /* True "connection" waiting for a request */
    private native boolean receiveRequest(IluCall callShell);
    
} //IluServerConnection



/**
 * IluServerConnectionThread is a helper class implementing fork 
 * for IluServerConnection.
 *
 * If you wonder why this is a separate class, congratulations.
 * The reason is that IluPort has member variables accessed from
 * native code.  If IluPort would extend Thread, those variables
 * would be aligned erronously by the JDK1.0.2 native stubber.  
 */
/*friendly*/ final
class IluServerConnectionThread extends IluServiceThread {
    
    private IluServerConnection cr;
    private static int priority = IluEnvironment.threadPriority();
	private static int count = 0; // used to provide unique thread name
      
	/* returns and increments the classes object count */
    private static final synchronized long incrementCount() {
        return count++;
    }
	
    /*friendly*/ IluServerConnectionThread(IluServerConnection cr) {
        super("IluServerConnectionThread" + incrementCount());
        this.setDaemon(true);
        this.setPriority(priority);  
        this.cr = cr;
    } //constructor
    
    static void fork(IluServerConnection cr) {
        IluServerConnectionThread crf = new IluServerConnectionThread(cr);
        crf.start();
    } //fork
    
    public void run() {
        this.cr.connectionReaderLoop(this);
    } //run
    
} // IluServerConnectionThread


/**
 * IluServerConnectionExtraThread is a helper class to fork a thread 
 * servicing one request
 */
/*friendly*/ final
class IluServerConnectionExtraThread extends IluServiceThread {
    private IluCall call;
    private static int priority = IluEnvironment.threadPriority();
    
    /*friendly*/ IluServerConnectionExtraThread(IluCall call) {
        this.call = call;
        this.setPriority(priority);
    } //constructor
    
    public void run() {
        try {
            IluServerConnection.serve1(call, this);
        } catch (java.lang.Exception e ) {
            IluDebug.log.println("** Exception " + e);
            e.printStackTrace(IluDebug.log);
            IluDebug.iluPreError("** caught in IluServerConnection...");
            //this is the bottom of a thread...
        }
    } //run
    
} // IluServerConnectionExtraThread

