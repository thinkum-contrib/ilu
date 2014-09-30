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
/* IluDebug.java */
/* Chris Jacobi, January 14, 1999 2:38 pm PST */
/* $Id: IluDebug.java,v 1.37 1999/08/12 05:58:53 janssen Exp $ */
/* See native code in IluJava_IluDebug.c */

package xerox.ilu;

/**
 * This class helps in debugging IluJava. <p>
 * Main use is user setting flags; programmmatic
 * use is discouraged as this api changes on need. <p>
 *
 * Most of the debug flags are set from the command line only.
 * This module is used by Ilu to query the flag and not by
 * an application to set the flags.
 *
 * @see IluDebugHooks
 * @see IluEnvironment
 */
public class IluDebug {
    
    /** Debug flag denoting no debug output */
    public static final int dont = 0;

    /** Debug flag denoting limited debug output */
    public static final int basic = 1;
    
    /** Debug flag denoting detailed debug output (gets lengthy) */
    public static final int detailed = 2; 
    
    
    /**
     * Accessing an argument
     */
    private static final int gip(java.lang.String key, int deflt) {
        return IluEnvironment.getIntProp(key, deflt);
    } //gip
    
    
    /**
     * Value for a default debug level when no specific value is specified.
     * Guides only those flags expected to be set by users.
     */
    private static int def = gip("ilu.debug", dont);
    

    /**
     * Generic debugging:
     * (missnamed; should be "process", "tracing" or "enabled") <pre>
     *     0 "dont" 
     *         to run quiet 
     *         (explicitely disables most debug output for speed)
     *     1 "basic"
     *         to provide debug logs
     *         (normal ilu application developmment)
     *     2 "detailed" 
     *         to block ilu on client errors as well as ilu errors
     *         (for chasing hard to find bugs) </pre>
     */
    public static final int traceGeneric() {return genericFlag;}
    private static int genericFlag = gip("ilu.debug.enabled", basic);
    
    
    /** 
     * Queries whether ilu_SetRaiseDebugHook should be used.<p>
     * The "debug-hook" feature is for debugging ilu, not production use.
     * It is invoked every time a non-success ilu_Error is created,
     * WHETHER OR NOT it's a "real error" .
     */
    public static final int setRaiseDebugHookFlag() {
        return gip("ilu.debug.setRaiseDebugHook", dont);
    } //setRaiseDebugHookFlag
   
    
    /** Spare to allow access without recompiling */
    public static final int traceSpare1() {return spare1Flag;}
    private static int spare1Flag = gip("ilu.debug.spare1", dont);
    
    
    /** Spare to allow access without recompiling */
    public static final int traceSpare2() {return spare2Flag;}
    private static int spare2Flag = gip("ilu.debug.spare2", dont);
    
    
    /** The ilu initialization process */
    public static final int traceInits() {return initsFlag;}
    //Friendly because of initialization order problems.
    /*friendly*/ static int initsFlag = gip("ilu.debug.init", dont);


    /** Creation and destruction of objects */
    public static final int traceObjects() {return objectsFlag;}
    private static int objectsFlag = gip("ilu.debug.object", def);


    /** 
     * Garbage collection etc; rarely used by users.
     * Recommend to also trace "ilu.debug.object" when using this.
     */
    public static final int traceGC() {return gcFlag;}
    /*friendly*/ static int gcFlag = gip("ilu.debug.gc", def);


    /** Pickles and Any */
    public static final int tracePickle() {return pickleFlag;}
    private static int pickleFlag = gip("ilu.debug.pickle", def);
    
    
    /** Calls, marshalling */
    public static final int traceCalls() {return callsFlag;}
    /*friendly*/ static int callsFlag = gip("ilu.debug.calls", def);
    
    
    /** Definitions of classes, methods, types etc. */
    public static final int traceDefs() {return definitionsFlag;}
    private static int definitionsFlag = gip("ilu.debug.defs", def);


    /** Ports, connections, transports, servers etc. */
    public static final int tracePCT() {return pctFlag;}
    private static int pctFlag = gip("ilu.debug.trans", def);


    /** Monitor locking. Ilu internal only; expensive */
    public static final int traceLocks() {return lockFlag;}
    private static int lockFlag = gip("ilu.debug.locks", dont); 

    
    /** Alarm tech.  Ilu internal only; expensive */
    public static final int traceAlarms() {return alarmFlag;}
    private static int alarmFlag = gip("ilu.debug.alarms", dont); 
    
    
    /** 
     * Set this to a nonzero value to disable garbage collection.
     * Disable garbage collection as divide and conquer to prove
     * that a bug is not related to this topic. 
     */
    public static final int dontGC() {return dontGCFlag;}
    /*friendly*/ static int dontGCFlag = gip("ilu.debug.dontgc", 0);


    
    /**
     * Debugging aid to capture the java stack. 
     */
    public static java.lang.String captureStackTrace() {
        java.io.CharArrayWriter cawr = new java.io.CharArrayWriter();
        java.io.PrintWriter pwr = new java.io.PrintWriter(cawr);
        java.lang.Exception e = new java.lang.Exception();
        e.printStackTrace(pwr);
        return cawr.toString();
    } //captureStackTrace
    
    
    /*
     * "Constructor" for a debug/log stream.
     */
    private static final java.io.PrintWriter makeLog() {
        if (traceGeneric() > dont) {
            return IluDebugWriter.getIluDebugWriter();
        }
        return new java.io.PrintWriter(new xerox.basics.NowhereWriter());
    } //makeLog

    
    /**
     * A stream whose output can be redirected and is used for debugging
     * purposes. While this may be redirected, it is still visible somewhere.
     */
    public static java.io.PrintWriter log = makeLog();
    
    
    //order important !
    static { //after the flags and inits used by IluInit...
       IluInit.init();
       if (initsFlag > 0) {
           java.lang.System.err.println("IluDebug static 1");
       }
    } //static
    
     
    /**
     * Notification that ilu is about to raise some client
     * error soon.
     * 
     * - In Ilu debugging mode this will stop the thread and
     *   might even try to stop other threads to ease debugging.  
     * - In Ilu production mode this is either a no-op. It relies 
     *    on the caller to raise necessary errors.  
     *
     * clientPreError's are normal production features to Ilu. 
     */
    public static final void clientPreError(java.lang.String s) {
        if (traceGeneric() > basic) {
            block("clientPreError: " + s);
        }
    } //clientPreError
    
    
    /**
     * Notification that ilu is about to raise some internal
     * error soon.   
     * 
     * - In Ilu debugging mode this will stop the thread and
     *   might even try to stop other threads to ease debugging.  
     * - In Ilu production mode this might cause a log entry
     *   but will proceed and rely on the caller to raise 
     *   necessary errors.  
     */
    public static final void iluPreError(java.lang.String s) {
        System.err.println("** xerox.ilu.IluDebug iluPreError: " + s);
        if (traceGeneric() > dont) {
            block("iluError: " + s);
        }
    } //iluPreError
    
    
    /** Use when desperate */
    public static final void loopForEver() {
        for (;;) {
            try {
                java.lang.Thread.sleep(10000);
            } catch (java.lang.InterruptedException e) {
            }
        }
    } //loopForEver
    
    
    /**
     * Ilu internal panic.
     * 
     * Does not return; No recovery possible.  
     * Severe ilu problem is encountered which makes
     * any further progress meaningless. Could as well
     * exit the program, but this way more debugging
     * is possible.
     * Trusted callers only.  
     */
    public static final void panic(java.lang.String s) {
        System.err.println("**** xerox.ilu.IluDebug panic");
        System.err.println("     severe internal ilu problem");
        System.err.println(s);
        System.err.println(captureStackTrace());
        loopForEver();
    } //panic
    
    
    /**
     * block's the thread and blocks as much as conveniant
     * from Ilu..
     * 
     * Does not return; No recovery possible.  
     * This is used to stop ilu in situations which warrant
     * serious debugging in debugging mode; this shouldn't
     * be called in production mode.
     * Trusted callers only.  
     */
    static final void block(java.lang.String s) {
        System.err.println("**** xerox.ilu.IluDebug block");
        System.err.println("     blocking thread in ilu debugging mode");
        System.err.println(s);
        System.err.println(captureStackTrace());
        loopForEver();
    } //block
    
    
    /**
     * halt's the thread and blocks as much as conveniant
     * from Ilu.
     * Does not return; no recovery possible.  <p>
     *
     * This is used by test programs only; the difference
     * compared to "panic" is the users perception that  
     * the test program and not Ilu is initiating the action.
     * Trusted callers only.
     */
    public static final void halt(java.lang.String s) {
        System.err.println("**** xerox.ilu.IluDebug halt");
        System.err.println("     halt invoked by test program");
        System.err.println(s);
        System.err.println(captureStackTrace());
        loopForEver();
    } //halt
    
    
    /**
     * Accessing environment variables.  Generic Ilu clients should
     * rather use IluEnvironment.getenv 
     */
    /*friendly*/ static native java.lang.String ngetenv(java.lang.String key);


    /**
     * Setting environment variables.  Not going to work if HAVE_PUTENV
     * is not defined.  Returns 1 on success, 0 otherwise.
     */
    /*friendly*/ static native int nputenv(java.lang.String key, java.lang.String val);


    /*
     * Transfers value of actual named debug flags into the native world...
     */
    private static native void reportFlags(
    	int genericFlag, int spare1Flag, int spare2Flag,
    	int objectsFlag, int pickleFlag, int callsFlag, 
    	int definitionsFlag, int pctFlag, int gcFlag, 
    	int lockFlag, int alarmFlag
    	);
    	
    	
    /**
     * Like ilu_SetDebugLevelViaString
     */
    /*friendly*/ static native void nSetDebugLevel(java.lang.String s);


    /**
     * Transfers the current state of most debug flags downwards
     * into the native world...
     */
    public static void reportSomeFlags() {
        reportFlags(
            genericFlag, spare1Flag, spare2Flag, 
            objectsFlag, pickleFlag, callsFlag, 
            definitionsFlag, pctFlag, gcFlag, 
            lockFlag, alarmFlag
            );
    } //reportSomeFlags


    /**
     * Internal initialization.
     */
    /*friendly*/ static void initPhase2() {
        if (initsFlag>0) {
            System.err.println("IluDebug initPhase2 start");
        }
        if (iluDebugInstance != null) {
            panic("IluDebug init problem");
        }
        iluDebugInstance = new IluDebug();
        iluDebugInstance.registerTheInst();
        reportSomeFlags();
        if (initsFlag>0) {
            System.err.println("IluDebug initPhase2 done");
        }
    } //initPhase2
    
    
    /**
     * We need an instance on the native side
     * to be able to invoke methods.
     */
    private native void registerTheInst();
    private static IluDebug iluDebugInstance = null;
    
    
    /**
     * Dynamic version of captureStackTrace for impoverished
     * calls from the native side 
     */
    public java.lang.String dynamicCaptureStackTrace() {
        return IluDebug.captureStackTrace(); 
    } //dynamicCaptureStackTrace

    
    /**
     * Dynamic version of getIntProp for impoverished
     * calls from the native side 
     */
    public int dynamicGetIntProp(java.lang.String key, int deflt) {
        return IluEnvironment.getIntProp(key, deflt); 
    } //dynamicGetIntProp
    
    
    /**
     * Wait for interactive go-ahead typed into System.in.
     * Useful to stop ilu for setting break points after the
     * native code is loaded.
     */
    public static void interactiveWait() {
        java.io.InputStreamReader ir = new java.io.InputStreamReader(System.in);
        java.io.BufferedReader br = new java.io.BufferedReader(ir);
        boolean goAhead = false;
        java.lang.String line = null;
        while (!goAhead) {
            try {
                System.out.print("interactive wait g<return>:");
                System.out.flush();
                line = br.readLine(); 
            } catch (java.io.IOException e) { 
                line = null;
            }
            if (line != null & line.length() > 0)  {
                if (line.charAt(0)=='g') goAhead = true;
            }
        }
    } //interactiveWait


    /**
     * The features for which ilu-kernel debugging messages are to be 
     * displayed may be specified as a string consisting of
     * colon-separated names.  See iludebug.h for a listing of the 
     * allowable features.
     * Like the ILU_DEBUG environment variable.
     */
    public static final void setDebugLevel(java.lang.String s) {
        if (IluEnvironment.isPriviledged(2)) {
            nSetDebugLevel(s);
        }
    } //setDebugLevel


    /**
     * Can be called to start initialize this and all of ilu.
     * Internally called; ilu clients do not have to.
     */
    public static final void init() {
    } //init
    
    
    static { 
       if (initsFlag > 0) {
           java.lang.System.err.println("IluDebug static done");
       }
    } //static

} // IluDebug

