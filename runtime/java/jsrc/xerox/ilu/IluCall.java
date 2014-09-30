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
/* Chris Jacobi, January 6, 1999 4:47 pm PST */
/* IluCall.java */
/* $Id: IluCall.java,v 1.85 1999/08/03 01:53:46 janssen Exp $ */
/* See native code in IluJava_IluCall.c */ 

package xerox.ilu;

/**
 * Data structures and operations to perform ilu calls.<p>
 *
 * Class is used from stubs only; clients have no need
 * to know this class.  Even public procedures are of
 * no use to applications.<p>
 *
 * The types strings used for naming procedures refer to ILU-types; not 
 * Java types.<br>
 * 	(But the actual types are the Java representation  
 * 	for the named ILU-types.)
 */
 
public final
class IluCall extends IluWPBase {
    
    private long yCall; //native R+W; 
        //Holds the native information; used by the native side only

    private long yCallSurrConn;
        //Container, allows passing native pointer to connection
        //into a forked thread.

    /*friendly*/ xerox.ilu.IluMethodRep jjMethodRep;	//native R+W!!
    /*friendly*/ xerox.ilu.IluClassRep jjClassRep;	//native R+W!!
    /*friendly*/ boolean jMustFork = false;	//native write
    private boolean jNeedsSizing = false;      	//native write
    
    /*friendly*/ int needFinish = 0;	//native R+W;  
        //Controls necessity of ilu_FinishCall  
        //Reset natively monitored 
    
    private int selfCheck = 0;	//native R+W
        //Checks on program flow. 
        //Not monitored as correct usage is sequential.
        
    private IluOInt discrOI = null; //currently only used for sibling check     	
    
    /*friendly*/ IluPassport skeletonAcceptingPassport = null;
        //passport data is valid through call only..

    /*friendly*/ IluBatcher batcher = null;
           
    private static IluCall prototype = null;
    private static IluCall callCache = null;
    private static int traceCalls = 999; //better value set in init phase
    private static int traceObjects = 999; //better value set in init phase
    
    
    /*friendly*/ static final void init() {
    } //init
    
    
    /* 
     * Used by the native side to find the class.
     * This is necessary because it limits the navel show 
     * required otherwise. 
     */
    native void givePrototype();
    
    
    /*friendly*/ static void initPhase2() {
        prototype = new IluCall();
        prototype.givePrototype();
        xerox.basics.VMExtras.makeGCRoot(prototype);
        traceCalls = IluDebug.traceCalls();
        traceObjects = IluDebug.traceObjects();
    } //initPhase2
   
   
    static void badChar() throws org.omg.CORBA.SystemException {
        IluDebug.clientPreError("bad character");
        throw new org.omg.CORBA.DATA_CONVERSION("bad character");
    } //badChar
    
    
    static void badLength() throws org.omg.CORBA.SystemException {
        IluDebug.clientPreError("bad length");
        throw new org.omg.CORBA.BAD_PARAM("bad length");
    } //badLength
    
    
    /* 
     * Limit access to constructor.  We might also be constructing  
     * IluCall's on the native side (Check the particular case)
     */
    /*friendly*/ IluCall () {
    } //constructor
    
    
    /* Real finalizer */
    private final native 
    void nativeFinalize() throws java.lang.Throwable;
    
    
    /** 
     * Finalizer to give the native implementation a chance for cleanup.
     * Native code frees memory: protected final to avoid illegal calls
     * producing a memory smash.
     */
    protected final void finalize() throws java.lang.Throwable {
        nativeFinalize(); 
        super.finalize();
    } //finalize
    
    
    /**
     * Stub visible "constructor" for SURROGATE calls
     * Either allocates or reuses a call.
     */
    public static IluCall startCall(
    	xerox.ilu.IluMethodRep methodRep,
    	xerox.ilu.IluSurrogateObject discriminantObj 
    	) throws org.omg.CORBA.SystemException
    {
        xerox.ilu.IluPassport passport = null; //need to prevent .. 
        xerox.ilu.IluPipeline pipeline = null; //.. garbage GC until .. 
        xerox.ilu.IluSerializationContext serializationCtx = null; 
            //.. nativeStartCall returns
        /*$ idb */  if (traceCalls > 0) {
        /*$ idb */      IluDebug.log.println("! startCall " + discriminantObj 
        /*$ idb */          + " " + methodRep);
        /*$ idb */  }
        IluOInt discriminantOI = IluOInt.peek(discriminantObj);
        if (discriminantOI == null) {
            throw new org.omg.CORBA.INV_OBJREF("discriminantObj not known");
        }
        IluCall call = getCallShell();
        java.lang.Thread thread = java.lang.Thread.currentThread();
        if (thread instanceof IluServiceThread) {
            IluServiceThread st = (IluServiceThread) thread;
            passport = st.getInitiatingPassport();
            pipeline = st.stubPipeline;
            serializationCtx = st.stubSerializationCtx;
            call.batcher = st.stubBatcher;
        } else {
            call.batcher = null;
        }
        try {
            call.discrOI = discriminantOI;
            call.nativeStartCall(
                methodRep, discriminantOI, passport, pipeline, serializationCtx
                );
        } finally {
            if (call.jMustFork) {
                IluSurrogateConnection.forkSC(call.yCallSurrConn);
            }
        }
        return call;
    } //startCall
    
    	
    private native void nativeStartCall(
    	xerox.ilu.IluMethodRep methodRep,
    	xerox.ilu.IluOInt discriminantOI, 
	xerox.ilu.IluPassport passport,
	xerox.ilu.IluPipeline pipeline,
	xerox.ilu.IluSerializationContext serializationCtx
    	) throws org.omg.CORBA.SystemException;
	
	
    /**
     * Native memory allocation to be used after "new IluCall".
     * Trusts "this" to be freshly allocated and therefore may 
     * avoid monitor locking.  
     * Allocates native data structures which are freed on
     * finalization.
     */
    private native void initCallShell() throws org.omg.CORBA.SystemException;
    
    	
    /**
     * Allocates a new reusable IluCall.
     */
    public static IluCall makeCallShell() throws org.omg.CORBA.SystemException {
       IluCall cl = new IluCall();
       cl.initCallShell();
       return cl;
    } //makeCallShell
    
    
    /**
     * Returns a new or cached IluCall.
     */
    public static IluCall getCallShell() throws org.omg.CORBA.SystemException {
        if (callCache!=null) {
            synchronized (prototype) {
                if (callCache!=null) {
                   IluCall c = callCache;
                   callCache = null;
                   return c;
                }
            }
        }
        return makeCallShell();
    } //getCallShell
    
    
    public void finishCall() {
        //
        //Prevent accessing native passport data which disappeares when
        //the call is done.
        IluPassport sp = skeletonAcceptingPassport;
        skeletonAcceptingPassport = null;
        if (sp != null) {
            sp.disable();
        }
        //
        int safeToCache = this.nFinishCall();
        //Temporarily disabled for debugging reasons.
        //BTW: We are assigning the function result to a variable
        //explicitely because the microsoft sdk compiler used to 
        //remove the function call in an erroneous optimization.      
        //if (safeToCache == 1) {
        //    if (IluEnvironment.isPriviledged(2)) {callCache = this;}
        //}
    } //finishCall


    /* returns 1 if everything looks totally clean */
    private native int nFinishCall();
    
    /**
     * Marshalling routines.
     * All name of types are as defined by ilu.
     * Arguments are as used in Java.
     *
     * The IluClassRep parameters are as specified in the class which did 
     * define the method; the actual object passed can be from a 
     * further subclass.
     */
    
    
    /*friendly*/ final
    IluOInt getOi(java.lang.Object obj, IluClassRep staticClass) 
            throws org.omg.CORBA.SystemException {
        IluOInt oi = null;
        /*$ idb */  if (traceCalls > IluDebug.basic) {
        /*$ idb */      IluDebug.log.println("! Enter IluCall.getOi, obj = " 
        /*$ idb */          + obj);
        /*$ idb */  }
        if (obj != null) {
            oi = IluOInt.peek(obj);
            /*$ idb */  if (traceCalls > IluDebug.basic) {
            /*$ idb */      IluDebug.log.println("! IluCall.getOi, oi = " + oi);
            /*$ idb */  }
            if (oi == null) {
                //object unknown; try to register it right here as 
                //a true object...
                /*$ idb */  if (traceCalls > IluDebug.basic) {
                /*$ idb */      IluDebug.log.println("! IluCall.getOi tries auto-reg");
                /*$ idb */  }
                IluClassRep iluClass = 
                    IluClassAccess.iluClassFromJavaClass(obj.getClass());
                /*$ idb */  if (traceObjects > 0) {
                /*$ idb */      IluDebug.log.println("! IluCall.getOi auto-register obj: " 
                /*$ idb */          + obj + " ilu-class: " + iluClass);
                /*$ idb */  } 
                if (iluClass != null & iluClass.isA(staticClass)) {
                    IluRT0.registerTrueObject(
                        null,	//ih
                        obj, 
                        null,	//server
                        iluClass,
                        0	//lifetime
                        );
                } else {
                    /*$ idb */  if (traceObjects > 0) {
                    /*$ idb */      IluDebug.log.println("! IluCall.getOi wrong iluClass"); 
                    /*$ idb */  }
                }
                oi = IluOInt.peek(obj);
                if (oi==null) {
                    IluDebug.clientPreError("not registerd with Ilu " + obj);
                    throw new org.omg.CORBA.INV_OBJREF("not registerd with Ilu " + obj);
                }
            }
        }
        /*$ idb */  if (traceCalls > IluDebug.basic) {
        /*$ idb */      IluDebug.log.println("! IluCall.getOi returns " + oi);
        /*$ idb */  } 
        return oi;
    } //getOi
    
    
    public void checkSibling(java.lang.Object obj, IluClassRep staticClass) 
            throws org.omg.CORBA.SystemException
    {
        if (obj != null) {
            IluOInt oi = getOi(obj, staticClass);
            if (oi != null) {
                if (oi.yServer != this.discrOI.yServer) {
                    throw new org.omg.CORBA.BAD_PARAM("failed sibling test");
                }
            }
        }
    } //checkSibling
      
      
    private native int szObjectx(IluOInt oi, 
        boolean discriminant, IluClassRep staticClass
        ) throws org.omg.CORBA.SystemException;
    
    
    public int szObject(java.lang.Object obj, 
        boolean discriminant, IluClassRep staticClass
        ) throws org.omg.CORBA.SystemException {
        return szObjectx(getOi(obj, staticClass), discriminant, staticClass);
    } //szObject
    
    
    private native void outObjectx(IluOInt oi, 
        boolean discriminant, IluClassRep staticClass
        ) throws org.omg.CORBA.SystemException;
        
        
    public void outObject(java.lang.Object obj, 
        boolean discriminant, IluClassRep staticClass
        ) throws org.omg.CORBA.SystemException 
    {
        /*$ idb */  if (traceCalls > IluDebug.basic) {
        /*$ idb */      IluDebug.log.println("! outObject " + obj);
        /*$ idb */  }
        outObjectx(getOi(obj, staticClass), discriminant, staticClass);
    } //outObject
   
    
    /* may be "un-finished" */
    private native IluOInt 
    inOI(boolean dis, IluClassRep staticClass) throws org.omg.CORBA.SystemException;
    
    
    public java.lang.Object inObject(
        	boolean discriminant, IluClassRep staticClass
        	) throws org.omg.CORBA.SystemException
    {
        java.lang.Object realObject = null;
        IluOInt oi;
        //I'd love to have this outside any locks !?
        //Compare to IluRT0
        /*$ idb */  if (traceCalls > IluDebug.basic) {
        /*$ idb */      IluDebug.log.println("! IluCall_inObject: enter");
        /*$ idb */  }
        oi = inOI(discriminant, staticClass);
        if (oi == null) {
            if (!discriminant && staticClass.isOptional()) {
                /*$ idb */  if (traceCalls > IluDebug.dont) {
                /*$ idb */      IluDebug.log.println("! IluCall_inObject: null");
                /*$ idb */  }
                return null;
            }
            IluDebug.clientPreError("inObject failed (null)");
            throw new org.omg.CORBA.INV_OBJREF("inObject failed (null)");
        }
        if (discriminant) {
            this.discrOI = oi;
        }
        realObject = oi.getTheObject();
        if (realObject == null) {
            IluDebug.clientPreError("inObject failed (local)");
            throw new org.omg.CORBA.INV_OBJREF("inObject failed (local)");
        }
        /*$ idb */  if (traceCalls > IluDebug.dont) {
        /*$ idb */      IluDebug.log.println("! IluCall_inObject got: " 
        /*$ idb */        + realObject);
        /*$ idb */  }
        return realObject;
    } //inObject
    
    
    public java.lang.Object getCallSingleton() 
            throws org.omg.CORBA.SystemException {
        java.lang.Object realObject = null;
        if (traceCalls > IluDebug.basic) {
            IluDebug.log.println("! IluCall_getCallSingleton: enter");
        }
        IluOInt oi = getCallSingletonOIx();
        if (oi != null) {
            realObject = oi.getTheObject();
        }
        if (realObject == null) {
            IluDebug.clientPreError("getCallSingleton failed");
            new org.omg.CORBA.INV_OBJREF("getCallSingleton failed");
        }
        /*$ idb */  if (traceCalls > IluDebug.dont) {
        /*$ idb */      IluDebug.log.println("! IluCall_getCallSingleton got: " 
        /*$ idb */          + realObject);
        /*$ idb */  }
        return realObject;
    } //getCallSingleton
    
    
    private native IluOInt 
    getCallSingletonOIx() throws org.omg.CORBA.SystemException;
    
    public native int  szInt64(long val) throws org.omg.CORBA.SystemException;
    public native void outInt64(long val) throws org.omg.CORBA.SystemException;
    public native long inInt64() throws org.omg.CORBA.SystemException;
    
    public native int  szInt32(int val) throws org.omg.CORBA.SystemException;
    public native void outInt32(int val) throws org.omg.CORBA.SystemException;
    public native int  inInt32() throws org.omg.CORBA.SystemException;
    
    public native int  szInt16(short val) throws org.omg.CORBA.SystemException;
    public native void outInt16(short val) throws org.omg.CORBA.SystemException;
    public native short inInt16() throws org.omg.CORBA.SystemException;
    
    //bytes preserve the bit pattern, not the numeric value 
    public native int  szByte(byte val) throws org.omg.CORBA.SystemException;
    public native void outByte(byte val) throws org.omg.CORBA.SystemException; 
    public native byte inByte() throws org.omg.CORBA.SystemException; 
    
    public native int  szBool(boolean val) throws org.omg.CORBA.SystemException;
    public native void outBool(boolean val) 
             throws org.omg.CORBA.SystemException;
    public native boolean inBool() throws org.omg.CORBA.SystemException;
    
    public native int  szChar16(char val) throws org.omg.CORBA.SystemException;
    public native void outChar16x(char val) 
             throws org.omg.CORBA.SystemException;
    public void outChar16(char val) throws org.omg.CORBA.SystemException {
        if (val=='\u0000') badChar();
        outChar16x(val);
    } //outChar16
    public native char inChar16() throws org.omg.CORBA.SystemException;
    
    public native int  szChar8(char val) throws org.omg.CORBA.SystemException;
    public native void outChar8x(char val) throws org.omg.CORBA.SystemException;
    public void outChar8(char val) throws org.omg.CORBA.SystemException {
        if (val == '\u0000' || val > '\u00ff' ) badChar();
        outChar8x(val);
    } //outChar8
    public native char inChar8() throws org.omg.CORBA.SystemException;

    public native int  szOptional(boolean stat) 
             throws org.omg.CORBA.SystemException;
    public native void outOptional(boolean stat) 
             throws org.omg.CORBA.SystemException;
    public native boolean inOptional() throws org.omg.CORBA.SystemException;
    
    public native int  szArray(int elementCount) 
             throws org.omg.CORBA.SystemException;
    public native void outArray(int elementCount) 
             throws org.omg.CORBA.SystemException;
    public native void inArray() throws org.omg.CORBA.SystemException;
    public native void endArray() throws org.omg.CORBA.SystemException;
    
    public native int  szRecord() throws org.omg.CORBA.SystemException;
    public native void outRecord() throws org.omg.CORBA.SystemException;
    public native void inRecord() throws org.omg.CORBA.SystemException;
    public native void endRecord() throws org.omg.CORBA.SystemException;
    
    public native int  
    	szSequence(int leng, int limit) throws org.omg.CORBA.SystemException;
    public native void 
    	outSequence(int leng, int limit) throws org.omg.CORBA.SystemException;
    public native void 
    	outSequenceMark(int extent) throws org.omg.CORBA.SystemException;
    public native int  inSequence(int limit) 
             throws org.omg.CORBA.SystemException;
    public native void inSequenceMark(int extent) 
             throws org.omg.CORBA.SystemException;
    public native void endSequence() throws org.omg.CORBA.SystemException;
    
    native int  
    	nSzUnion(int discrim, int discrimKind) 
    	         throws org.omg.CORBA.SystemException;
    public int  
    	szUnion(int discrim, IluTypeKind k) 
    	         throws org.omg.CORBA.SystemException {
    	return nSzUnion(discrim, k.value());
    } //nSzUnion
    
    native void 
    	nOutUnion(int discrim, int discrimKind) 
    	         throws org.omg.CORBA.SystemException;
    public void 
    	outUnion(int discrim, IluTypeKind k) 
    	         throws org.omg.CORBA.SystemException{
    	nOutUnion(discrim, k.value());
    } //outUnion
    
    native int  
    	nInUnion(int discrimKind) throws org.omg.CORBA.SystemException; 
    
    public int  
    	inUnion(IluTypeKind k) throws org.omg.CORBA.SystemException { 
    	//returns discriminator...
    	return nInUnion(k.value());
    } //inUnion
    
    public native void 
    	endUnion() throws org.omg.CORBA.SystemException;
    
    private native int  szReal128Buff(byte[] val) 
             throws org.omg.CORBA.SystemException;
    private native void outReal128Buff(byte[] val) 
             throws org.omg.CORBA.SystemException;
    private native void inReal128Buff(byte[] buf) 
             throws org.omg.CORBA.SystemException;
    
    public int  szReal128(xerox.ilu.float128 val) 
            throws org.omg.CORBA.SystemException {
        return szReal128Buff(val.toByte16());
    }
    
    public void outReal128(xerox.ilu.float128 val) 
            throws org.omg.CORBA.SystemException {
        outReal128Buff(val.toByte16());
    }
    
    public xerox.ilu.float128 inReal128() throws org.omg.CORBA.SystemException {
        byte[] b16 = new byte[16];
        inReal128Buff(b16);
        return xerox.ilu.float128.fromByte16(b16);
    }
    
    public native int  szReal64(double val) 
             throws org.omg.CORBA.SystemException;
    public native void outReal64(double val) 
             throws org.omg.CORBA.SystemException;
    public native double inReal64() throws org.omg.CORBA.SystemException;
    
    public native int   szReal32(float val) 
             throws org.omg.CORBA.SystemException;
    public native void  outReal32(float val) 
             throws org.omg.CORBA.SystemException;
    public native float inReal32() throws org.omg.CORBA.SystemException;
    
    public native int  szEnum(int val) throws org.omg.CORBA.SystemException;
    public native void outEnum(int val) throws org.omg.CORBA.SystemException;
    public native int  inEnum() throws org.omg.CORBA.SystemException;
    
    public native int  szCard16(short val) throws org.omg.CORBA.SystemException;
    public native void outCard16(short val) 
             throws org.omg.CORBA.SystemException;
    public native short  inCard16() throws org.omg.CORBA.SystemException;
    
    /* preserve bit pattern not value */
    public native int  szCard32(int val) throws org.omg.CORBA.SystemException;
    public native void outCard32(int val) throws org.omg.CORBA.SystemException;
    public native int  inCard32() throws org.omg.CORBA.SystemException;
    
    /* preserve bit pattern not value */
    public native int  szCard64(long val) throws org.omg.CORBA.SystemException;
    public native void outCard64(long val) throws org.omg.CORBA.SystemException;
    public native long inCard64() throws org.omg.CORBA.SystemException;
    
    /* Variable-length array of short character (String) */
    public native int  
    	szString8(java.lang.String s, int limit) 
    	         throws org.omg.CORBA.SystemException;
    public native void 
    	outString8x(java.lang.String s, int limit) 
    	         throws org.omg.CORBA.SystemException;
    
    public void outString8(java.lang.String s, int limit) 
             throws org.omg.CORBA.SystemException
    {
        if (s == null) {s = "";}
        int n = s.length();
        if ((limit < n) && (limit != 0)) badLength();
        for (int i = 0; i<n; i++) {
            char c = s.charAt(i);
            if ( c == '\u0000' || c > '\u00ff' ) badChar();
        }
        outString8x(s, limit);
    } //outString8
    
    public native java.lang.String 
    	inString8(int limit) throws org.omg.CORBA.SystemException;
    
    /* Fix-length array of short character (StringVec) */
    public native int  
    	szChar8Array(char[] a, int limit) throws org.omg.CORBA.SystemException;
    public native void 
    	outChar8Arrayx(char[] a, int limit) 
    	         throws org.omg.CORBA.SystemException;
    
    public void 
    	outChar8Array(char[] a, int limit) throws org.omg.CORBA.SystemException {
        if (limit != a.length) badLength();
        for (int i = 0; i<limit; i++) {
            if ( a[i] == '\u0000' || a[i] > '\u00ff' ) badChar();
        }
        outChar8Arrayx(a, limit);
    } //outChar8Array
    
    public native char[] 
    	inChar8Array(int limit) throws org.omg.CORBA.SystemException;

    /* Variable-length array of character (WString) */
    public native int  
    	szString16(java.lang.String s, int limit) 
    	         throws org.omg.CORBA.SystemException;
    
    public native void 
    	outString16x(java.lang.String s, int limit) 
    	         throws org.omg.CORBA.SystemException;
    
    public void outString16(java.lang.String s, int limit) 
            throws org.omg.CORBA.SystemException
    {
        if (s == null) {s = "";}
        int n = s.length();
        if ((limit < n) && (limit != 0)) badLength();
        for (int i = 0; i<n; i++) {
            if ( s.charAt(i) == '\u0000' ) badChar();
        }
        outString16x(s, limit);
    } //outString16
    
    private native char[] 
        inString16AsArray(int limit) throws org.omg.CORBA.SystemException;
    
    public java.lang.String 
    	inString16(int limit) throws org.omg.CORBA.SystemException {
    	char[] a = inString16AsArray(limit);
        return new java.lang.String(a);
    } //inString16

    /* Fix-length array of character (WStringVec) */
    public native int  
    szChar16Array(char[] a, int limit) throws org.omg.CORBA.SystemException;
    
    public native void 
    outChar16Arrayx(char[] a, int limit) throws org.omg.CORBA.SystemException;
    
    public void 
    outChar16Array(char[] a, int limit) throws org.omg.CORBA.SystemException {
        if (limit != a.length) badLength();
        for (int i = 0; i<limit; i++) {
            if ( a[i] == '\u0000' ) badChar();
        }
        outChar16Arrayx(a, limit);
    } //outChar16Array
    
    public native char[] 
    	inChar16Array(int limit) throws org.omg.CORBA.SystemException;


    /* 
     * Variable-length array of byte (Bytes) 
     */
    public /*not used by stubber*/ native int  
    	szBytesSx(byte[] bytes, int start, int len, int limit
    	) throws org.omg.CORBA.SystemException; 
    
    public final int 
        szBytesS(byte[] bytes, int limit
    	) throws org.omg.CORBA.SystemException {
    	return szBytesSx(bytes, 0, bytes.length, limit);
    } //szBytesS
    
    public /*not used by stubber*/ native void 
    	outBytesSx(byte[] bytes, int start, int len, int limit
    	) throws org.omg.CORBA.SystemException; 
    
    public final void 
    	outBytesS(byte[] bytes, int limit
    	) throws org.omg.CORBA.SystemException {
    	outBytesSx(bytes, 0, bytes.length, limit);
    } //outBytesS
    
    public native byte[] 
    	inBytesS(int limit) throws org.omg.CORBA.SystemException;
    
    /* 
     * Fixed-length array of byte (Opaque) 
     * bytes.length()#limit because 
     * limit from isl and bytes.length from application which
     * might be buggy.
     */
    public /*not used by stubber*/ native int  
    	szBytesAx(byte[] bytes, int start, int limit
    	) throws org.omg.CORBA.SystemException;
    
    public final int  
    	szBytesA(byte[] bytes, int limit
    	) throws org.omg.CORBA.SystemException
    {
    	return szBytesAx(bytes, 0, limit);
    } //szBytesA
    
    public/*not used by stubber*/ native void 
    	outBytesAx(byte[] bytes, int start, int limit
    	) throws org.omg.CORBA.SystemException;
    	
    public final void 
    	outBytesA(byte[] bytes, int limit
    	) throws org.omg.CORBA.SystemException
    {
    	outBytesAx(bytes, 0, limit);
    } //outBytesA
    
    public native byte[] 
    	inBytesA(int limit) throws org.omg.CORBA.SystemException;
    
    /** true object utilities */
    public native int  startReadRequest() throws org.omg.CORBA.SystemException;
    public native boolean doneReadRequest() 
             throws org.omg.CORBA.SystemException;
    public native boolean noReply() throws org.omg.CORBA.SystemException;
    public native int beginSizingReply() throws org.omg.CORBA.SystemException;
    public native boolean startWriteReply(int sz) 
             throws org.omg.CORBA.SystemException;
    public native boolean doneWriteReply() throws org.omg.CORBA.SystemException;
    
    public native int 
    	beginSizingException(int eindex) throws org.omg.CORBA.SystemException;
    
    public native boolean 
    	startWriteException(int evalue, int sz) 
    	         throws org.omg.CORBA.SystemException;
    

    public native boolean doneWriteException() 
             throws org.omg.CORBA.SystemException;
   
    
    /** surrogate object utilities */
    public native void startWriteRequest(int sz) 
    	throws org.omg.CORBA.SystemException;
    
    
    /*friendly*/ native void nDoneWriteRequest(IluBatcher ib) 
        throws org.omg.CORBA.SystemException;
    

    public void doneWriteRequest() throws org.omg.CORBA.SystemException {
        this.nDoneWriteRequest(this.batcher);
    } //doneWriteRequest
    

    public native int  
    	startReadReply() throws org.omg.CORBA.SystemException; 
    	//returns errcode
        public static final int retryCode = -9999; //stubs only
    
    
    public native void doneReadReply() throws org.omg.CORBA.SystemException;
    
    
    public IluUserException readException(IluMethodRep method, int errcode)
            throws org.omg.CORBA.SystemException
    {
         if ((errcode<=0) || (errcode>method.jjExceptions.length)) {
             return null;
         } else {
             IluExceptionRep exRep = method.jjExceptions[errcode-1];
             IluUserException ex = exRep.allocInstance();
             ex.readException(this);
             return ex;
         }
    } //readException
    
    /*friendly*/ native void nativeMarkCallFailure(int protocolException);
    
    
    /** for server side stubs only */
    public void unexpectedException(java.lang.Throwable e) 
            throws xerox.ilu.IluUnexpectedException
    { 
        /*$ idb */  if (traceCalls > IluDebug.dont) {
        /*$ idb */      IluDebug.log.println("! IluCall.unexpectedException reports exception: " + e );
        /*$ idb */      if (traceCalls >= IluDebug.detailed) {
        /*$ idb */          e.printStackTrace(IluDebug.log);
        /*$ idb */          IluDebug.log.println("--");
        /*$ idb */      }
        /*$ idb */  }
        nativeMarkCallFailure(
            IluSystemExceptionBase.protocolException_Unknown);
        throw new xerox.ilu.IluUnexpectedException(e);
    } //unexpectedException

        
    /** accessor */
    public final boolean needsSizing() {
        return jNeedsSizing;
    } //needsSizing
    
    
    /** accessor */
    public final boolean mustFork() {
        return jMustFork;
    } //jMustFork
    

    /** 
     * Debugging aid.
     * Do not use in real applications.
     */
    public final int getSelfCheck() {
        return selfCheck;
    } //getSelfCheck
    
    
    /** 
     * NOT really public; must be accessable by stubs
     * Signature like in generic writeException stubs, for simplicity
     * of stub generation  
     */
    public static 
    void simpleWriteException(IluUserException ex, int index, IluCall call) 
            throws org.omg.CORBA.SystemException
    {
        //Stubs redefine this, for exceptions with arguments...
        int sz = 0;
        if (call.needsSizing()) {
            sz = call.beginSizingException(index);
        }
        call.startWriteException(index, sz);
        call.doneWriteException();
    } //simpleWriteException

        
} //IluCall




