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
/* IluPort.java */
/* Chris Jacobi, January 14, 1999 4:16 pm PST */
/* $Id: IluPort.java,v 1.34 1999/08/03 01:53:58 janssen Exp $ */
 
/*
 * See IluJava_IluPort.c
 */

package xerox.ilu;

/**
 * This is the Java representation of the concept ilu_Port.<p>
 * Server applications should create ports with the IluServer class instead.  
 * However this class provides additional methods useful to destroy ports.<p>
 * @see IluServer
 * @see IluPassport
 * @see IluTransportInfo
 */
public final class
IluPort extends IluWPBase {
    
    /*private; friendly for debugging*/ long yPort = 0;
    /*private; friendly for debugging*/ long yConn = 0;
    /*friendly*/ xerox.ilu.IluServer jjServer = null;
    private java.lang.String jjProtocolInfo = null;
    private xerox.ilu.IluTransportInfo jjTransportInfo = null;
    private xerox.ilu.IluPassport jjPassport = null;
    private int pstate = portVirgin; // use the server lock to protect

    //State codes
    private static final int portVirgin = 0;
    private static final int portActive = 1;
    private static final int portClosed = 2;
    private static final int portDone = 3;

    //Codes to communicate with the native side
    private static final int watcherLoop = 0;
    private static final int watcherFork = 1;
    private static final int watcherClose = 2;
    private static final int watcherDone = 3;
    
    private static int tracePCT = IluDebug.tracePCT();

    private IluPort() {
        super();
    }
    
    /** 
     * Construcing a port.
     * We recommend however to use the IluServer class instead.
     * @see IluServer#createPort
     */
    public static IluPort createPort(
        	xerox.ilu.IluServer server,      		/* required */
        	java.lang.String protocolInfo,         		/* null is ok */
        	xerox.ilu.IluTransportInfo transportInfo,	/* null is ok */
        	xerox.ilu.IluPassport passport  		/* null is ok */
        	) throws org.omg.CORBA.SystemException
    {
        /*$ idb */  if (tracePCT > 0) {
        /*$ idb */      IluDebug.log.println("! IluPort: createPort");
        /*$ idb */  }
        if (server.surrogateServer) {
            throw new xerox.ilu.IluSystemExceptionBase(
                "don't use surrogate servers"
                );
        }
        server.promisedPort = true; 
        server.finishInitialization();
        IluPort port = new IluPort();
        port.jjServer = server;
        //
        //protocolInfo
        if (protocolInfo == null) {
            protocolInfo = defaultProtocolInfo();
        }
        port.jjProtocolInfo = protocolInfo;
        //
        //transportInfo
        if (transportInfo == null) {
            transportInfo = new IluTransportInfo(null);
        }
        port.jjTransportInfo = transportInfo;
        //
        port.jjPassport = passport;
        port.nativeInitPort();
        /*$ idb */  if (tracePCT > 1) {
        /*$ idb */      IluDebug.log.println("! IluPort: half way " 
        /*$ idb */          + port.yPort);
        /*$ idb */  }
        synchronized (server.lock) {
            if (server.state>1) {
               throw new org.omg.CORBA.INTERNAL("Bad server");
            }
            server.portTable.put(port, port);
            port.pstate = portActive;
        }
        /*$ idb */  if (tracePCT > 0) {
        /*$ idb */      IluDebug.log.println("! IluPort: init " + port.yPort);
        /*$ idb */  }
        if (port.yPort == 0) {
           IluDebug.clientPreError("! IluPort failure: yPort==0");
           throw new org.omg.CORBA.INTERNAL("Bad port creation");
        }
        IluPortWatcherThread.forkPortWatcher(port);
        /*$ idb */  if (tracePCT>0) {
        /*$ idb */      IluDebug.log.println("! IluPort: done " + port.yPort);
        /*$ idb */  }
        return port;
    } //createPort
    
    private static java.lang.String defaultPInfo = null;
    /*friendly*/ static native java.lang.String nDefaultProtocol();

    /** 
     * Returns the name of the default protocol used if none is specified.
     */
    public static java.lang.String defaultProtocolInfo() {
        if (defaultPInfo == null) {
            defaultPInfo = nDefaultProtocol();
            if (defaultPInfo == null) {
                throw new org.omg.CORBA.INITIALIZE(
                    "Failed finding a default ProtocolInfo"
                    );
            }
        }
        return defaultPInfo;
    } //defaultProtocolInfo
    
    private native void nativeInitPort();
    private native int nativeWatchPort(); //side effect: sets yConn
    private native void nativeClosePort();
    private native void nativeDonePort();
        
    /*friendly*/ void portWatcherLoop() {
        int code;
        for (;;) {
            /*$ idb */  if (tracePCT > 0) {
            /*$ idb */      IluDebug.log.println("! IluPort: loop " 
            /*$ idb */          + this.yPort);
            /*$ idb */  }
            code = nativeWatchPort();//sets yConn if necessary
            switch (code) {
               case watcherLoop: 
                   break; //spurious wakeup
               case watcherFork:
                   IluServerConnection.forkConnectionReader(yConn); 
                   yConn = 0;
                   break;
               case watcherClose:
                   //rare error situation
                   closePort(); 
                   break;
               case watcherDone: 
                   doneWithPort(); 
                   return;
            }
            java.lang.Thread.yield(); //serve existing connections first
        }
    } //portWatcherLoop
    
    /** 
     * Closing a port prevents new connections.
     */
    public void closePort() {
        xerox.ilu.IluServer server = this.jjServer;
        IluPort that = null;
        if (server != null) {
            java.lang.Object ob = server.portTable.remove(this);
            if (ob == this) {
                synchronized (server.lock) {
                    if (this.pstate == portActive) {
                        this.pstate = portClosed; 
                        that = this;
                    }
                }
            }
            if (that!=null) {
                //if called from portwatcher loop 
                //    this is same thread where doneWithPort will be called
                //if called from random other thread 
                //    excludes other random threads
                //    but doesn't exclude spurios io errors
                that.nativeClosePort();
            }
        }
    } //closePort
    
    private void doneWithPort() {
        closePort();
        xerox.ilu.IluServer server = this.jjServer;
        IluPort that = null;
        if (server != null) {
            synchronized (server.lock) {
                if (this.pstate == portClosed) {
                    this.pstate = portDone; 
                    this.jjServer = null;
                    that = this;
                }
            }
            if (that!=null) that.nativeDonePort();
        }
    } //doneWithPort
    
}//IluPort


/**
 * IluPortWatcherThread is a helper class implementing fork for IluPort.
 *
 * If you wonder why this is a separate class, congratulations.
 * The reason is that IluPort has member variables accessed from
 * native code.  If IluPort would extend Thread, those variables
 * would be aligned erronously by the JDK1.0.2 native stubber.  
 */
final class IluPortWatcherThread extends java.lang.Thread {
    
    private IluPort jjPort; 
    private static int priority =
        IluEnvironment.threadPriority();
	private static int count = 0; // used to provide unique thread name

	/* returns and increments the classes object count */
    private static final synchronized long incrementCount() {
        return count++;
    }

    private IluPortWatcherThread() {
        super("IluPortWatcherThread" + incrementCount());
    } //IluPortWatcherThread

    static void forkPortWatcher(xerox.ilu.IluPort port) {
        IluPortWatcherThread pw = new IluPortWatcherThread();
        pw.jjPort = port; 
        pw.setDaemon(port.jjServer.deamon);
        pw.setPriority(priority);  
        pw.start();
    } //forkPortWatcher

    /** shouldn't be public */
    public void run() {
        try {
            this.jjPort.portWatcherLoop();
        } catch (java.lang.Exception e ) {
            IluDebug.log.println(e.toString());
            IluDebug.iluPreError("** caught in connection forker..");
            //make message, but let Ilu continue...
        }
    } //run

} //IluPortWatcherThread



