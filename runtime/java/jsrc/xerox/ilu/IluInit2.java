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
/* IluInit2.java */ 
/* Chris Jacobi, December 10, 1998 3:36 pm PST */
/* $Id: IluInit2.java,v 1.47 1999/08/03 01:53:59 janssen Exp $ */
 
/*
 * Native code in IluJava_IluInit.c
 */


package xerox.ilu;

/** 
 * Helper class which takes care of initialization of ILU classes and
 * libraries in the right order. <br>
 * Not for applications. <p>
 *
 * @see IluInit
 * @see IluDebug
 */
 
/*friendly*/ final class IluInit2 {
    
    private static void println(java.lang.String s) {
        java.lang.System.err.println(s);
    } //println
    
    /* Invokes initialization procedures for various native packages. 
     * Split into small pieces to allow debugging ilu with divide
     * and conquer.  There is no other reason for splitting int
     * small procedures.
     */
    private native static void nInitLibrary1();
    private native static void nInitLibrary2();
    private native static void nInitLibrary3();

    /* Prevent loading inmcompatible library */
    private static final int expectedLibraryVersion = 22;
    
    /* Native code in IluJava_IluInit.c */
    private native static int checkLibrary12x9x98(int key);
    private static void checkLibrary() {
        //
        // We got this far; that means the library has been loaded
        //
        // Check against the manully edited control value which
        // will be incremented on incompatible semantic changes
        if (checkLibrary12x9x98(0) != expectedLibraryVersion) {
            IluDebug.panic("bad ilu library");
        } 
        //
        // Check the java major version
        if (checkLibrary12x9x98(1) != 1) {
            IluDebug.panic("incompatible java major version");
        }
        //
        // Check the java minor version
        IluMozillaBase mb = IluMozillaBase.getInstance();
        String versActual = mb.getSystemProperty("java-vm.version");
        
        if (versActual != null) {
            String versNativeLib = "1." + checkLibrary12x9x98(2);
            if (versActual.indexOf(versNativeLib) == -1) {
                //this test does not recognize all possible problems...
                println(
                    "***WARNING***: "
                    + "\n   the ilu native library is configured with java <" 
                    + versNativeLib + ">"
                    + "\n   however the running java system is version <" 
                    + versActual + ">"
                    + "\n   This may or may not cause severe problems."
                    );
            }
        }
        // 
        // Now do some native initializations
        if (checkLibrary12x9x98(-1) != 0) {
            IluDebug.panic("ilu library initialization problem");
        } 
    } //checkLibrary
    
    private static void log(java.lang.String s) {
        println("IluInit2 did " + s);
    } //log
    
    /* Loads all the java code
     * Done in first step that later native stuff can find the class
     * May spread out into arbitrary order but doesn't do anything
     * order dependent.
     */
    private static void myInitPhase0() {
        //we need IluDebug first
        xerox.ilu.IluDebug.init();
        boolean trace = (IluDebug.initsFlag > 0);
        if (trace) {log("start phase 0");};
        if (trace) {log("0 IluDebug");}
        xerox.ilu.IluDebugHooks.init();
        if (trace) {log("0 IluDebugHooks");}
        xerox.ilu.IluForkTech.init();
        if (trace) {log("0 IluForkTech");}
        xerox.ilu.IluAlarmTech.init();
        if (trace) {log("0 IluAlarmTech");}
        xerox.ilu.IluWPBase.init0();
        if (trace) {log("0 IluWPBase");}
        xerox.ilu.IluRT0.init();
        if (trace) {log("0 IluRT0");}
        xerox.ilu.IluSurrogateConnection.init();
        if (trace) {log("0 IluSurrogateConnection");}
        xerox.ilu.IluIdentityType.init();
        if (trace) {log("0 IluIdentityType");}
        xerox.ilu.IluServer.init();
        if (trace) {log("0 IluServer");}
        xerox.ilu.IluGCClient.init();
        if (trace) {log("0 IluGCClient");}
        xerox.ilu.IluOInt.init();
        if (trace) {log("0 IluOInt");}
        xerox.ilu.IluClassRep.init();
        if (trace) {log("0 IluClassRep");}
        xerox.ilu.IluCall.init();
        if (trace) {log("0 IluCall");}
        xerox.ilu.IluServerRelocationInfo.init();
        if (trace) {log("0 IluServerRelocationInfo");}
        xerox.ilu.IluTypeCode.init();
        if (trace) {log("0 IluTypeCode");}
        xerox.ilu.IluPickle.init();
        if (trace) {log("0 IluPickle");}
    } //myInitPhase0
    
    /* Simple non-spreading initializations 
     * (including native but only if not calling fancy stuff) 
     */
    private static void myInitPhase2() {
        boolean trace = (IluDebug.initsFlag > 0);
        if (trace) {log("start phase 2");}
        xerox.ilu.IluDebug.initPhase2();
        if (trace) {log("2 IluDebug");}
        xerox.ilu.IluDebugHooks.initPhase2();
        if (trace) {log("2 IluDebugHooks");}
        xerox.ilu.IluForkTech.initPhase2();
        if (trace) {log("2 IluForkTech");}
        xerox.ilu.IluAlarmTech.initPhase2();
        if (trace) {log("2 IluAlarmTech");}
        xerox.ilu.IluOInt.initPhase2();
        if (trace) {log("2 IluOInt");}
        xerox.ilu.IluCall.initPhase2();
        if (trace) {log("2 IluCall");}
    } //myInitPhase2

    
    /* Final initializations.
     * Order most likely important
     */
    private static final void myInitPhase3() {
        boolean trace = (IluDebug.initsFlag > 0);
        if (trace) {log("start phase 3");}
        xerox.ilu.IluTypeCode.initPhase3();
        if (trace) {log("3 IluTypeCode");}
        xerox.ilu.IluRootObjectHelper.initPhase3();
        if (trace) {log("3 IluRootObjectHelper");}  
        xerox.ilu.IluSurrogateConnection.initPhase3();
        if (trace) {log("3 IluSurrogateConnection");}  
        xerox.ilu.IluIdentityType.initPhase3();
        if (trace) {log("3 IluIdentityType");}
        xerox.ilu.IluServer.initPhase3();
        if (trace) {log("3 IluServer");}
    } //myInitPhase3
    
    /**
     * Loads native library.
     * Order important.
     */
    private static void myLoadLibrary() {
        /* Allow different native libraries set up by
         * appropriate use of properties.  The standard
         * ilu build simply sets up one library. It is 
         * conceivable however that a site installs multiple
         * ilu versions.
         */
        java.lang.String libname = IluEnvironment.getStringProp0(
            "xerox.ilu.nativeLibrary"
            );
        if (libname == null) {
            libname = "IluJava";
        }
        try {
            IluMozillaBase mb = IluMozillaBase.getInstance();
            if (mb==null) {
                println("----using System.loadLibrary"); 
                java.lang.System.loadLibrary(libname);
            } else {
                mb.loadNativeLibrary(libname);
            }
            //
            //for debugging only
            //java.lang.System.load(
            //    "/rubberduckyChris/ilus/runtime/java/libIluJava.so"
            //    );
        } catch (UnsatisfiedLinkError e) {
            /* This error caused enough traffic on our
             * distribution list to warrant carefull message
             */
            println("**** Couldn't load native ilu library \"" 
                +  libname + "\": " + e);
            println(e.getMessage());
            println("  The Java runtime for ILU is very machine");
            println("  dependent. Are you using the right library? ");
            if ( ! libname.equals("IluJava")) {
                println("  You are using a non-standard library name!");
            }
            println("  On unix check LD_LIBRARY_PATH.");
            println("  On PC check PATH.");
            println("  (Should everything look correct, the file"); 
            println("  $ILUSRC/runtime/java/jsrc/xerox/ilu/IluInit2.java");
            println("  contains additional info how to debug this)"); 
            //
            //In some versions of java there is a boot class path and
            //a class path. In jdk1.2beta4 the boot class loader fails
            //to load native methods from non-standard places.    
            //
            //If all else fails: on unix use the LD_DEBUG;
            //environment variable to produce more output.
            //On Solaris 2.5 use:
            //setenv LD_DEBUG_OUTPUT some_file_name
            //setenv LD_DEBUG files
            //
            //Do man ld
            //setenv LD_DEBUG help
            //ld
            //
            IluDebug.panic("loading native ilu library");
        }
    } //myLoadLibrary
    
    
    private static final void myCheckPhase4() {
        //deprecated:  ILU works with standard orbs
        //try {
        //    org.omg.CORBA.ORB.check_ILU_ness();
        //} catch (java.lang.NoSuchMethodError nsme) {
        //    println("**** " + nsme.getMessage());
        //    println("Change classpath/bootclasspath to find ILU before finding other orbs.");
        //    IluDebug.panic("Bad version of class");
        //}
    } //myCheckPhase4
    
    
    private static final void doInitialize () {
        try { 
            myInitPhase0();
            myLoadLibrary();
            checkLibrary();
            myInitPhase2();
            //
            // If you get a segmentation violation with a stack
            // trace including *this* location, that could be caused by
            // an incompatible native library version.  What version of
            // java were you using?
            if (IluDebug.initsFlag > 0) {log(" before nInitLibrary1");}
            nInitLibrary1();
            if (IluDebug.initsFlag > 0) {log(" before nInitLibrary2");}
            nInitLibrary2();
            if (IluDebug.initsFlag > 0) {log(" before nInitLibrary3");}
            nInitLibrary3();
            //
            myInitPhase3();
            myCheckPhase4();

            /* THIS IS FOR SETTING BREAKPOINTS...*/
            if (xerox.basics.Environment.getBooleanProp(
                    "ilu.interactivewait", false)) {
                IluDebug.interactiveWait();
            }
            
        } catch (java.lang.Exception e) {
            println("** Failed initializing Ilu " +  e);
            e.printStackTrace();
            IluDebug.panic("initializing ilu");
        }
        
        //load dynamicly stub specified classes
        IluPreLoad.stopDelaying();
        
        //load client specified classes
        //do not use xerox.basics.Environment.load
        //because ilu.load provides better initialization order
        java.lang.String loadAlso = 
                xerox.basics.Environment.getStringProp("ilu.load");
        xerox.basics.Environment.loadClasses(loadAlso);
        
    } //doInitialize
    
    private boolean isInit = false;
    
    /*friendly*/ IluInit2() {
        if (!isInit) {
            isInit = true;
            doInitialize();
        }
    } //constructor

} //IluInit2

