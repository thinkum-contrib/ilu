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
/* xerox.ilu.IluMozillaBase.java */
/* Chris Jacobi, December 23, 1998 12:11 pm PST */
/* 
 * $Id: IluMozillaBase.java,v 1.10 1999/08/03 01:53:54 janssen Exp $ 
 */
 

 
/*
 * xerox.ilu.IluMozillaBase
 * This class encapsulates the SecurityManager differences between
 * Netscape and JDK for Ilu. <p>
 *
 * This class itself has no syntactical Ilu dependencies for the
 * purpose of making the build easy:  Ilu as application is build 
 * with no netscape dependencies.  The Ilu netscape subclass of this
 * can be built without having Ilu prebuild.   
 */
 
 
package xerox.ilu;

/*friendly*/ class IluMozillaBase {

    private static IluMozillaBase mb = null;
    
    private static void log(java.lang.String s) {
        java.lang.System.err.println(s);
    } //println
    
    
    static {
        java.lang.Class clazz = null;
        try {
            Object ob;
            clazz = java.lang.Class.forName("xerox.ilu.IluMozillaExtra");
        } catch (java.lang.ClassNotFoundException e) {
        }
        if (clazz != null) {
            try {
                mb = (IluMozillaBase) clazz.newInstance();
            } catch (java.lang.InstantiationException e) {
            } catch (java.lang.IllegalAccessException e) {
            }
        }
        if (mb == null) {
            mb = new IluMozillaBase();
        }
    } //static
    
    
    public static IluMozillaBase getInstance() {
        return mb;
    } //getInstance


    /**
     * Overload this with a mozilla based loadLibrary...
     * In spite of its general look, this is only used
     * for ilu based libraries and subclasses might use
     * their own algorithm to override libname
     */
    public void loadNativeLibrary(java.lang.String libname) 
            throws java.lang.UnsatisfiedLinkError {
        /*
         * I found no way to predict whether green threads or native
         * threads shall be used, so we simply try both libraries. :-)
         *
         * We write a generous message because the exception case
         * writes an alarming error message which needs to be
         * explained to the user.
         *
         * Little tidbit:  Trying the native library first would not
         * work because no load time exception is thrown but an error
         * is raised much later.
         */
        try {
            java.lang.String vendor = System.getProperty("java.vendor");
            if (vendor != null) {
                if (vendor.lastIndexOf("Microsoft") >= 0) {
                    libname  = libname + "_ms";
                }
            }
            java.lang.String version = 
                System.getProperty("java.specification.version");
            if (version != null) {
                if (version.startsWith("1.2")) {
                    libname  = libname + "_12";
                }
            }

        } catch (SecurityException se) {
        }
        try {
            //debug output removed to relax compile time dependency
            //if (IluDebug.traceInits() > 0) {log("loadLibrary " + libname);}
            java.lang.System.loadLibrary(libname);
            //if (IluDebug.traceInits() > 0) {log("loadLibrary ok");}
            return;
        } catch (UnsatisfiedLinkError e) {
        }
        log("ILU failed loading the native code library.");
        log("ILU now tries loading an alternate library (maybe supporting native java threads).");
        java.lang.System.loadLibrary(libname + "_alt");
        log("Got it...");
    } //loadNativeLibrary

    
    public java.lang.String getSystemProperty(java.lang.String key) {
        return System.getProperty(key);
    } //getSystemProperty


} //IluMozillaBase
 

