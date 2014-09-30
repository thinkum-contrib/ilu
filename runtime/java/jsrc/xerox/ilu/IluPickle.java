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
/* IluPickle.java */
/* Chris Jacobi, December 23, 1998 12:14 pm PST */
/* $Id: IluPickle.java,v 1.11 1999/08/03 01:53:49 janssen Exp $ */
/* See native code in IluJava_IluPickle.c */ 


package xerox.ilu;

/**
 * Representation of typed values...<p>
 * 
 * Pickles are assign-once then immutable. <p>
 *
 * @see xerox.ilu.IluAny
 * @see org.omg.CORBA.Any
 */
 
public final class IluPickle extends IluWPBase {
    
    private long ypickleptr = 0;  
    private long yuuid = 0; //Avoid conversion all the time
    private int pState = 0;
        private static final int validState = 1;
        private static final int initialState = 0;
        private static final int justStarted = -3;
        private static final int justMiddled = -2;
        private static final int intermediate = -1;
        private static final int oneDeadState = 9999;
        /* We are quite paranoid with the state of a pickle.
         * While the actual memory ownership rules are
         * easy, we have to prove so... 
         */ 
    private IluTypeCode type = null;
    private java.lang.String jjuid = null;
    private IluCall workingCall = null; //for insertion and extraction
    private static IluCall specialNonWorkingCall = null;
     
    private int lockingstate = 0;
        private static final int lockFree = 0;
        private static final int lockOccupied = 1;
        private static final int lockBroken = 2; //controls finalization state
     
    /**
     * Not for applications; called by stubs.
     * Allocates unusable IluPickle.
     */
    public IluPickle() {
        super();
    }
     
     
    private synchronized void enterLock() 
             throws org.omg.CORBA.SystemException {
         while  (lockingstate == lockOccupied){
              try {
                  wait(1000);
              } catch (InterruptedException ie) {
                  throw new xerox.ilu.IluSystemExceptionBase("lock broken");
              }
         } 
         if (lockingstate >= lockBroken) {
             throw new xerox.ilu.IluSystemExceptionBase("no more available");
         }
         lockingstate = lockOccupied;
    } //enterLock
     
    private synchronized void leaveLock()  
             throws org.omg.CORBA.SystemException {
        if (lockingstate == lockOccupied) {
            lockingstate = lockFree;
        }
        notify(); //We don't care which thread gets woken up
    } //leaveLock
     
    private synchronized int breakLock() {
        int old = lockingstate;
        lockingstate = lockBroken;
        notifyAll();
        return old;
    } //breakLock
     
     
    private synchronized boolean useupCall_throw(IluCall call) {
        if (call == this.workingCall) {
            //initialize this dynamicly to simplify ilu initialization order
            if (specialNonWorkingCall==null) {
                specialNonWorkingCall = new IluCall();
            }
            this.workingCall = specialNonWorkingCall;
            return false; //good
        }
        if (lockingstate != lockOccupied) {
            return true; //bad
        }
        return true; //bad
    } //useupCall_throw
    

    /** 
     * Guarantees that pickle is defined, immutable, and not 
     * yet destroyed. 
     */
    public final void ensureValidPickle() 
            throws org.omg.CORBA.SystemException {
        if (pState != validState) {
            IluDebug.clientPreError("Bad pickle state: " + pState);
            if (pState<validState) {
                throw new xerox.ilu.IluSystemExceptionBase(
                    "Pickle not initialized " + pState
                    );
            } else {
                throw new xerox.ilu.IluSystemExceptionBase(
                    "Pickle already finalized " + pState
                    );
            }
        }
    } //ensureValidPickle
     
    public final void assertState(int state) 
            throws org.omg.CORBA.SystemException {
        if (this.pState != state) {
            IluDebug.clientPreError("Bad pickle-state: " + this.pState);
            throw new xerox.ilu.IluSystemExceptionBase(
                "Pickle used out of context; " 
                + this.pState + " instead of " + state
                );
        }
    } //assertState
    
    private native java.lang.String nativeUuidPickle() 
         throws org.omg.CORBA.SystemException;
         
     /** 
      * Returns a uuid for the value contained in the Pickle. 
      */
     public java.lang.String getUUID() throws org.omg.CORBA.SystemException {
         java.lang.String uuid;
         ensureValidPickle();
         uuid = this.jjuid;
         if (uuid == null) {
             uuid = nativeUuidPickle();
             this.jjuid = uuid;
         };
         return uuid;
    } //getUUID   
 
 
    /** 
     * Returns a TypeCode for the value contained in the Pickle. 
     */
    public IluTypeCode typeOrNull() throws org.omg.CORBA.SystemException {
        IluTypeCode t = this.type;
        if (t != null) return t;
        java.lang.String uuid = getUUID();
        t = IluTypeCode.fromUID_or_null(uuid);
        this.type = t;
        return t;
    } //typeOrNull   
 
 
    /** 
     * Returns a TypeCode for the value contained in the Pickle. 
     */
    public IluTypeCode type() throws org.omg.CORBA.SystemException {
        IluTypeCode t = typeOrNull();
        if (t == null) {
            java.lang.String uuid = getUUID();
            IluDebug.clientPreError("TypeCode not available: " + uuid);
            throw new xerox.ilu.IluSystemExceptionBase("no TypeCode");
        }
        return t;
    } //type   
 
 
    /* Internal test done in Java side to avoid testing in native method */
    private static final void ensureValidCall(xerox.ilu.IluCall call) 
            throws org.omg.CORBA.SystemException {
        if (call==null) {
            throw new xerox.ilu.IluSystemExceptionBase("null call");
        }
    } //ensureValidCall
     
     
    private native void nativeOutPickle(xerox.ilu.IluCall call) 
         throws org.omg.CORBA.SystemException;
         
    /** Off-limits to applications; accessed by "Any" */
    public void _out(xerox.ilu.IluCall call) 
             throws org.omg.CORBA.SystemException {
        ensureValidCall(call);
        ensureValidPickle();
        nativeOutPickle(call);
    } //_out
     
     
    private native int nativeSzPickle(xerox.ilu.IluCall call) 
         throws org.omg.CORBA.SystemException;
         
    /** Off-limits to applications; accessed by "Any" */
    public int _sz(xerox.ilu.IluCall call) 
             throws org.omg.CORBA.SystemException {
        ensureValidCall(call);
        ensureValidPickle();
        return nativeSzPickle(call);
    } //_sz
     
     
    private native void nativeInPickle(xerox.ilu.IluCall call) 
         throws org.omg.CORBA.SystemException;
     
    /** Off-limits to applications; accessed by "Any" */
    public static IluPickle _in(xerox.ilu.IluCall call) 
             throws org.omg.CORBA.SystemException {
        ensureValidCall(call);
        IluPickle pickle = new IluPickle();
        pickle.nativeInPickle(call);
        pickle.pState = validState;
        return pickle;
    } //_in
     
     
    private native void nativeFinalizePickle(long savedYpickleptr);
        //passes ypickleptr as an extra argument so that the lock
        //needs to be entered between taking the value and assigning 
        //0 (to avoid memory smash if finalize is called illegally
        //by a java application and another thread uses ypickleptr)
        //as long as all aplications enter the monitor.  However,
        //not all entry points enter the lock!!  We must protect
        //finalize from being called illegally. 
         
    /** 
     * Need finalize because we must return pickle memory to the kernel. 
     * Note that this is protected and Pickle is final.  Nobody can
     * make illegal calls. 
     */
    protected final void finalize() throws java.lang.Throwable {
        int oldLockState;
        long savedYpickleptr = ypickleptr;
        ypickleptr = 0;
        this.pState = oneDeadState;
        oldLockState = breakLock();
        if (oldLockState == lockBroken) return; //finalize only once
        nativeFinalizePickle(ypickleptr);
        this.type = null;
        //
        super.finalize();
        //
    } //finalize
      
     
       
    /*
     * EXTRACTION
     *
     *
     * anticipated stubber generated code
     *
     * public static XXXX from_IluPickle(IluPickle pickle) throws ... {
     *     XXXX xxxx = XXXX-initval;
     *     IluCall _call = null;
     *     try {
     *         _call = pickle.startFromPickle("XXXX-UID");
     *         xxxx = in_XXXX(_call);
     *     } finally {
     *         pickle.endFromPickle(_call); 
     *     }
     *     return xxxx;
     * } //from_IluPickle
     */
    
    
    /** 
     * Returns a Java value for the value contained in the IluPickle
     * Returns instances of Java wrapper classes instead of Java 
     * basic types.
     *
     * WARNING:
     * In case of object types and the object class is not known to 
     * the java runtime but a superclass is, fail anyway. ??fix this??
     */
    public java.lang.Object value(IluTypeCode t) throws org.omg.CORBA.SystemException {
        java.lang.Object xxxx = null;
        IluCall call = null;
        ensureValidPickle();
        java.lang.String uuid = getUUID();
        IluTypeCode tc = typeOrNull();
        if (tc==null) {
            tc = t;
        }
        IluIOFunctions iofunctions = tc.getIOFunctions();
        if (iofunctions==null) {
            throw new xerox.ilu.IluSystemExceptionBase("type not available");
        }
        try {
            call = this.startFromPickle(uuid);
            xxxx = iofunctions.inFunc(call, tc);
        } finally {
            this.endFromPickle(call); 
        }
        return xxxx;
    } //value


    private native 
    void nativeStartPickleExtract(xerox.ilu.IluCall call, java.lang.String uuid) 
         throws org.omg.CORBA.SystemException;
     
     
    /** 
     * No applications. 
     * Called by stubs (and ilu only).
     * Opens a security hole for denial of service attack: might not free
     * the lock on the pickle. (on purpose or by forgetting try finally)
     */
    public IluCall startFromPickle(java.lang.String uuid)
            throws org.omg.CORBA.SystemException {
        IluCall call;
        enterLock();
        if (this.workingCall != null) {
            throw new xerox.ilu.IluSystemExceptionBase("illegal usage");
        }
        call = new IluCall();
        nativeStartPickleExtract(call, uuid); //Tests subtype relationship!
        //the following statement is executed only if no exception!
        this.workingCall = call; 
        return call;
    } //startFromPickle
    
    
    private native 
    void nativeEndPickleWork(xerox.ilu.IluCall call);
         // Never use with null value for call.
         // NO EXCEPTIONS; caller needs to free lock afterwards
     
     
    /** 
     * No applications. 
     * Called by stubs (and ilu) only. 
     */
    public void endFromPickle(IluCall call)
            throws org.omg.CORBA.SystemException {
        if (call == null) {
            // either: procedure was used out of context.
            // or:  procedure used by try-finally before call 
            //      has been initialized.  For this case return quietly.
            return; 
        }
        if (useupCall_throw(call)) {
            throw new xerox.ilu.IluSystemExceptionBase("used out of context");
        }
        nativeEndPickleWork(call);
        this.workingCall = null;
        leaveLock();
    } //endFromPickle
    


    /*
     * INSERTION
     *
     *
     * anticipated stubber generated code
     *
     * public static IluPickle pickle to_pickle(XXX _x) throws ... {
     *     int _sz = 0;
     *     IluCall _call = null;
     *     IluPickle pickle = new IluPickle();
     *     try {
     *         _call = pickle.startToPickle();
     *         if (_call.needsSizing()) {
     *             _sz = _call.XXX_szFunc(_x);
     *         }
     *         pickle.midToPickle(_call, _sz, "XXXX-UID")
     *         _call.XXX_outFunc(_x);
     *     } finally {
     *         pickle.endToPickle(_call); 
     *     }
     *     return pickle;
     * } //to_pickle
     *
     * Security goal:  A malicious stub may fool itself
     * but not a correct stub.  This is achieved easily by
     * not returning the pickle until it is initialized
     * correctly.  However we still need to think about
     * resource ownership conflicts. 
     */
    
    
    /** 
     * The formal value input argument needs to have the Java class
     * or subclass for the mapping of the IDL type.  If an input
     * argument is mapped to a Java basic type, this procedure will
     * accept the corresponding Java wrapper class.
     *
     * WARNING:
     * In case of object types the type "type" is used
     * even if "value" is a subtype. ??fix this??
     */
    public static IluPickle alloc(IluTypeCode type, java.lang.Object value) 
            throws org.omg.CORBA.SystemException {
        IluPickle pickle = null;
        int sz = 0;
        IluCall call = null;
        IluIOFunctions iofunctions = type.getIOFunctions();
        if (iofunctions==null) {
            throw new xerox.ilu.IluSystemExceptionBase("type not available"); 
        }
        if (! iofunctions.isAFunc(value, type)) { 
            //System.err.println("**wrong type in IluPickle");
            //System.err.println("iofunctions= " + iofunctions);
            //System.err.println("type= " + type);
            //System.err.println("value= " + value);
            throw new xerox.ilu.IluSystemExceptionBase("wrong type");
        }
        /*
        if (type.isObjectType()) {
             
        }
        */
        pickle = new IluPickle();
        try {
            call = pickle.startToPickle();
            if (call.needsSizing()) {
                sz = iofunctions.szFunc(call, value, type);
            }
            pickle.midToPickle(call, sz, type.getUID()); //subtype ???
            iofunctions.outFunc(call, value, type);
        } finally {
            pickle.endToPickle(call);
        }
        return pickle;
    } //alloc
         
         
    private native 
    void nativeStartPickleInsert(xerox.ilu.IluCall call) 
         throws org.omg.CORBA.SystemException;
     

    /** called by stubs */
    public IluCall startToPickle()
            throws org.omg.CORBA.SystemException {
        IluCall call;
        assertState(initialState);
        this.pState = intermediate;
        call = new IluCall();
        nativeStartPickleInsert(call);
        this.workingCall = call;
        this.pState = justStarted;
        return this.workingCall;
    } //startToPickle
    
    
    private native void nativeWritePickle(xerox.ilu.IluCall call, 
        int sz, java.lang.String uuid)  throws org.omg.CORBA.SystemException;
        // no changes about memory ownership
         
    /** called by stubs */
    public void midToPickle(IluCall call, int sz, java.lang.String uuid)
            throws org.omg.CORBA.SystemException {
        if (call != this.workingCall) {
            throw new xerox.ilu.IluSystemExceptionBase("used out of context");
        }
        assertState(justStarted);
        this.pState = intermediate;
        if (uuid == null) {
            throw new xerox.ilu.IluSystemExceptionBase("bad uuid");
        }
        nativeWritePickle(call, sz, uuid);
        this.pState = justMiddled;
    } //midToPickle
    
    
    /** called by stubs */
    public void endToPickle(IluCall call)
            throws org.omg.CORBA.SystemException {
        if (call == null) {
            // either: procedure used out of context.
            // or:  procedure used by try-finally before call 
            //      has been initialized.  For this case return quietly.
            return; 
        }
        if (call != this.workingCall) {
            throw new xerox.ilu.IluSystemExceptionBase("used out of context");
        }
        if (this.pState == justMiddled) { 
            //good case
            this.pState = intermediate;
            nativeEndPickleWork(call);
            this.workingCall = null;
            this.pState = validState;
        } else if (this.pState == validState) {
            throw new xerox.ilu.IluSystemExceptionBase("used out of context");
        } else  {
            //if it is a bad stub: we don't care about throwing or not
            //if it is a good stub we are here due to an exception thrown
            //    and caught in the finally; that is re-thrown. 
        }
    } //endToPickle
    
    static {xerox.ilu.IluInit.init();}

    /*friendly*/ static final void init() {
    }
    
} //IluPickle
