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
/* IluServer.java */
/* Chris Jacobi, December 23, 1998 12:18 pm PST */
/* $Id: IluServer.java,v 1.46 1999/08/03 01:53:52 janssen Exp $ */
 
/* 
 * See IluJava_IluServer.c for native implementation.
 */

package xerox.ilu;
    import java.util.Hashtable;

/**
 * This is the Java representation of the an ilu_Server. <p>
 *
 * Most features have reasonable default values.  In particular
 * if no ports is added to a server a default port will be made up.
 * The default port is set up for protocol and transport 
 * according to the ILU_DEFAULT_CINFO environment variable or 
 * property. <p>
 * 
 * Since true servers have associated threads running, don't expect
 * true servers to be garbage collected.<p>
 *
 * @see IluObjectTable
 * @see IluPort
 * @see IluServerRelocation
 *
 */
public final class IluServer extends IluWPBase {
    
    private long yIluServer = 0;
    private long yPtr = 0; //just in case the c side needs a hook
    private java.lang.String id; 
    private xerox.ilu.IluObjectTable jjObjTab = null;
    private xerox.ilu.IluServerRelocationInfo jjRelocatonInfo = null;
    /*friendly*/ java.util.Hashtable portTable; 
    /*friendly*/ java.util.Hashtable retainTable = null; 
        //prevent gc of IluOInt's
    /*friendly*/ int state = 0;
    /*friendly*/ java.lang.Object lock;
    /*friendly*/ boolean deamon = false;
    private boolean hasBeenAsserted = false; /*synchronized*/
    /*friendly*/ boolean promisedPort = false;  
    /*friendly*/ boolean surrogateServer = false;  
    /*friendly*/ boolean dontUseForDefault = false;  
    
    private static int tracePCT = IluDebug.tracePCT();
    private static IluServer defaultServer0 = null;
    
    static {
        IluInit.init();
    }
    
    public static void init() {
    }
    
    /* We report a proto type server because when calling
     * back from native code to java we use only dynamic methods
     */
    /*friendly*/ native void reportIluServerInst();
    /*friendly*/ static IluServer serverProto = null; 
    public static void initPhase3() {
        if (serverProto != null) {IluDebug.panic("init order");}
        serverProto = new IluServer();
        xerox.basics.VMExtras.makeGCRoot(serverProto);
        serverProto.reportIluServerInst();
    } //initPhase3
             
    /**
     * Returns whether this is a surrogate server.
     * If it is not a surrogate server, it is a true server.
     */
    public boolean isSurrogateServer() {
        return surrogateServer;
    } //isSurrogateServer
    
    /**
     * Application visible "constructor" for true server. <p>
     *
     * Excpect the type of objFactory to change; null objFactory is legal 
     * and results in a server not automatically generating true objects.
     */
    public static IluServer createServer(
                java.lang.String id //null -> make one up
                ) throws org.omg.CORBA.SystemException {
        IluServer serv = new IluServer();
        serv.surrogateServer = false;
        if (id == null) id = xerox.ilu.IluRT0.inventID();
        serv.id = id;
        /*$ idb */  if (tracePCT > 0) {
        /*$ idb */      IluDebug.log.println("! IluServer: create server " 
        /*$ idb */          + id);
        /*$ idb */  }
        return serv;
    } //createServer
    
    
    /**
     * "accessor" for a true server. <p>
     * Not recommended for clients using secure transports.
     */
    /*friendly*/ static IluServer defaultServer() 
            throws org.omg.CORBA.SystemException {
        IluServer serv = defaultServer0;
        if (serv == null) {
            serv = createServer(null);
            serv.finishInitialization();
            defaultServer0 = serv;
        }
        return serv;
    } //defaultServer
    
    
    /**
     * Creates a port and adds it to the server.
     */
    public IluPort createPort(
            java.lang.String protocolInfo,         	//null -> default
            xerox.ilu.IluTransportInfo transportInfo,	//null -> default
            IluPassport passport                 	//null -> default
            ) throws org.omg.CORBA.SystemException {
        IluPort port;
        this.checkTrue();
        this.promisedPort = true; 
        this.finishInitialization();
        port = IluPort.createPort(this, protocolInfo, transportInfo, passport);
        return port;
    } //createPort
    
    
    /**
     * Internal procedure to return a new array of strings with one
     * more string appended.
     */
    public static java.lang.String [] appendStringToArray(
        java.lang.String [] a,
        java.lang.String s
        )
    {
        java.lang.String [] x;
        int len = 0;
        if (a != null ) {len = a.length;}
        x = new java.lang.String[len+1];
        for (int i = 0; i<len; i++) {
            x[i] = a[i];
        }
        x[len] = s;
        return x;
    } //appendStringToArray
    
    
    /**
     * Internal procedure to split a sring up into an array of
     * strings with separators removed.
     */
    public static java.lang.String [] splitString(String xx, char sep) {
        java.lang.String [] a = null;
        while (xx != null) {
            String piece = xerox.basics.Environment.leftOfSeparator(xx, sep);
            a = appendStringToArray(a, piece);
            xx = xerox.basics.Environment.rightOfSeparator(xx, sep);
        }
        return a;
    } //splitString
    
    /**
     * Adds a port to the server; parses description for protocol
     * and transport.
     */
    public IluPort createParsedPort(String description, IluPassport passport) {
        IluPort p = null;
        String protocol = 
            xerox.basics.Environment.leftOfSeparator(description, '@');
        String stinfo = 
            xerox.basics.Environment.rightOfSeparator(description, '@');
        if (protocol==null) {
            throw new xerox.ilu.IluSystemExceptionBase(
                    "invalid protocol in port description: " + description
                );
        }
        if (stinfo==null) {
            throw new xerox.ilu.IluSystemExceptionBase(
                    "invalid tinfo in port description: " + description
                );
        }
        /*$ idb */  if (tracePCT > 0) {
        /*$ idb */      IluDebug.log.println(
        /*$ idb */          "! IluServer.createParsedPort " +   description);
        /*$ idb */  }
        java.lang.String [] atinfo = splitString(stinfo, '=');
        IluTransportInfo tinfo = new IluTransportInfo(atinfo);
        try {
            p = createPort(protocol, tinfo, passport);
        } catch (java.lang.RuntimeException e) {
            //Extra error message since this is a vexing error which occurs  
            //frequently at the beginning of small vanilla programs and
            //in addition jdk1.2 didn't print the right stack trace...
            System.err.println(
                "ILU failed creating port: <" + description  + "> " 
                + e.getMessage());
            //re-throw the same exception with original stack trace
            throw (java.lang.RuntimeException) e;
        }
        return p;
    } //createParsedPort

    /**
     * Parses string for descriptions for protocol and transport
     * and add appropriate ports.  Returns number of ports added.
     */
    public int createParsedPorts(String descriptions, IluPassport passport) {
        /*$ idb */  if (tracePCT > 0) {
        /*$ idb */      IluDebug.log.println(
        /*$ idb */          "! IluServer.createParsedPorts " +   descriptions);
        /*$ idb */  }
        int count = 0;
        while (descriptions != null) {
            String piece = 
                xerox.basics.Environment.leftOfSeparator(descriptions, ';');
            IluPort somePort = createParsedPort(piece, passport);
            if (somePort != null) {
                count = count+1;
            }
            descriptions = 
                xerox.basics.Environment.rightOfSeparator(descriptions, ';');
        }
        return count;
    } //createParsedPorts
    
    /**
     * Adds port to the server; checks properties for protocol
     * and transport but no fancy processing is promised.
     */
    public void createDefaultPorts(IluPassport passport) {
        int count = 0;
        java.lang.String xx = 
            IluEnvironment.getStringPropX("ILU_DEFAULT_CINFO");
        if (xx != null) {
            count = createParsedPorts(xx, passport);
        }
        if (count==0) {
            this.createPort(null, null, passport);
        }
    } //createDefaultPorts


    /**
     * Sets the "deamon" flag for ports.<p>
     *
     * The "deamon" flag marks threads to be deamons; deamon threads
     * do not prevent the application from termination. <p>
     *
     * This is a simple flag: Influences ports created in the future.
     * Not synchronized; this is for the server creator only.
     * Returns "this" for conveniance.
     */
    public IluServer setDeamonFlag(boolean deamon)
            throws org.omg.CORBA.SystemException {
        this.deamon = deamon;
        return this;
    } //setDeamonFlag
    

    /* Check that this server is not a surrogate server... */
    private final void checkTrue() throws org.omg.CORBA.SystemException {
        if (this.surrogateServer) {
            throw new xerox.ilu.IluSystemExceptionBase(
                "don't use surrogate servers"
                );
        }
    } //checkTrue


    /* Check that this server is still in its initialization phase */
    private final void checkInInit() throws org.omg.CORBA.SystemException {
        if (this.hasBeenAsserted) {
            throw new xerox.ilu.IluSystemExceptionBase(
                "must be called before initialization is asserted"
                );
        }
    } //checkInInit
    
    
    /**
     * Sets the object table for the server. <p>
     *
     * Can be called only before any ports are created.  Will not
     * be advertized unless ports a created or finishInitialization
     * is called directly or indirectly.<p>
     *
     * Not synchronized; this is for the server creator only.
     * An object table can have exactly one server.
     * A server can have at most one object table.
     * Returns "this" for conveniance.<p>
     *
     * Excpect the type of objFactory to change; null objFactory is legal 
     * and results in a server not automatically generating true objects.
     */
    public IluServer setObjectTable(xerox.ilu.IluObjectTable ot)
            throws org.omg.CORBA.SystemException {
        this.checkTrue();
        this.checkInInit();
        if (this.jjObjTab != null) {
            throw new xerox.ilu.IluSystemExceptionBase(
                "server already has an object table"
                );
        }
        this.jjObjTab = ot;
        ot.setServer(this);
        return this;
    } //setObjectTable
    
    /**
     * Enables IluServerRelocation for this server. <p>
     *
     * Can be called only once at server initialization time.<p>
     * Returns "this" for conveniance.<p>
     */
    public IluServer setServerRelocation(xerox.ilu.IluServerRelocation sr)
            throws org.omg.CORBA.SystemException {
        this.checkTrue();
        this.checkInInit();
        if (sr == null) {
            this.jjRelocatonInfo = null;
        } else {
            this.jjRelocatonInfo = new IluServerRelocationInfo(sr);
        }
        return this;
    } //setServerRelocation
    
    /**
     * Call back to decide whether relocation is necessary.
     * Called by native side only. 
     * Called within server lock. 
     */ 
    IluServerRelocationInfo mustCheckRelocate() {
        IluServerRelocationInfo info = this.jjRelocatonInfo;
        if (info != null) {
            IluServerRelocation checker = info.jjRelocatonChecking;
            if (checker != null) {
                info.pInfoContainer[0] = null;
                info.tInfoContainer[0] = null;
                checker.checkIluServerRelocation(
                    this, info.pInfoContainer, info.tInfoContainer
                    );
                //The containers have been touched by clients; make
                //untouchable safety copies to prevent gc while still
                //continuing with native caller.
                info.jjSaveProtocolInfo = info.pInfoContainer[0];
                info.jjSaveTransportInfo = info.tInfoContainer[0];
                if (info.jjSaveProtocolInfo != null && info.jjSaveTransportInfo != null) {
                   return info;
                }
            }
        }
        return null;
    } //mustCheckRelocate
    

    /**
     * Makes sure enough of the server has been initialized
     * so it can really be used.
     * Returns "this" for conveniance.
     */
    public IluServer finishInitialization(
            ) throws org.omg.CORBA.SystemException {
        this.checkTrue();
        if (! this.hasBeenAsserted) {
            boolean promisedPort = this.promisedPort;
            if (this.state == 29) {
                throw new xerox.ilu.IluSystemExceptionBase(
                    "use of destroyed server"
                );
            }
            synchronized (this) {
                if (! this.hasBeenAsserted) {
                    /*$ idb */  if (tracePCT > 0) {
                    /*$ idb */      IluDebug.log.println("! IluServer fin init " 
                    /*$ idb */          + this.id);
                    /*$ idb */  }
                    this.nativeOfCreateServer(this.id);
                    this.hasBeenAsserted = true;
                }
            }
            if (!this.promisedPort) { 
                //No port has been specified; we will make one up.
                this.createDefaultPorts(null);
            }
        }
        if (defaultServer0 == null && (! this.dontUseForDefault)) {
            defaultServer0 = this;
        }
        return this;
    } //finishInitialization
    
    
    /**
     * Accessor function 
     */
    public final java.lang.String serverId() {
        return this.id;
    } //serverId
    
    
    /*
     * Private<br>
     * Implementation for createServer.
     */
    private native void nativeOfCreateServer(java.lang.String id);
    
    
    /*
     * Constructor private: use createServer.<br>
     * Prevents creation of random servers.
     */
    private IluServer() {
        this.retainTable = new java.util.Hashtable();
        this.portTable = new java.util.Hashtable();
        this.lock = retainTable;
    }  //constructor
    
    /**
     * Destroys true server.<p>
     *
     * (But does not necessarily get rid of existing objects)
     * Once we support applets, this should be security checked.
     */
    public void destroyServer() {
        this.checkTrue();
        if (this == defaultServer0) {
            defaultServer0 = null;
        }
        synchronized (this.lock) {
           if (this.state>=9) return;
           this.state = 19;
        };
        java.util.Hashtable safeCopy = 
            (java.util.Hashtable) this.portTable.clone();
        java.util.Enumeration enum = safeCopy.keys();
        while (enum.hasMoreElements()) {
            IluPort p = (IluPort) enum.nextElement();
            p.closePort();
        }
        this.nativeOfDestroyServer();
        this.state = 29;
        this.hasBeenAsserted = false;
    } //destroyServer
    
    
    private native void nativeOfDestroyServer();
    private native void nativeOfFreeServer();
    
    /**
     * Not available to the general public.
     * (protected final to make sure)
     */
    protected final void finalize() throws java.lang.Throwable {
        if (! this.surrogateServer) {
            this.destroyServer();
            this.state = 99;
            this.nativeOfFreeServer();
            super.finalize();
        }
    } //finalize
    
} //IluServer

