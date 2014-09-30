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
/* xerox.ilu.IluMozillaExtra.java */
/* Chris Jacobi, December 23, 1998 12:11 pm PST */
/* 
 * $Id: IluMozillaExtra.java,v 1.11 1999/08/03 01:54:11 janssen Exp $ 
 */
 

 
/**
 * xerox.ilu.IluMozillaExtra
 * This class extends IluMozillaBase to make it active. <p>
 * The native library needs to be build with netscape options also
 * otherwise it can't be linked into netscape. 
 *
 * This is supporting the original vm of netscape version 4.05
 * and has been only lightly tested on a sparc station.
 * (This class is not necessary with java-plugin (activator).
 *
 * Ilu must loaded from the class path.
 * Ilu stubs must loaded from the class path also because there
 * is no dynamic unloading.
 *
 * Putting Ilu on the class path prevents other applets from
 * using visigenics.  I don't understand why I'm sorry:
 * visigenics doesn't really try very hard to be compatible; and
 * all the corba stuff for compatibility is nice and usefull for
 * orbs which do not use qualitative different implementations. 
 */
 
 
 
/* ******* Building instructions ********

See the source/java/runtime/Imakefile for options
how to rebuild the native java runtime so that it can be
loaded into the vm. 

See also 
/import/netscape-4.05/sparc-sun-solaris2.5.1/bin/README

Fix your path names. (Get rid of /tilde/jacobi...)

Copy the native executable to the spot where netscape can load it
cp /rubberduckyChris/ilus/runtime/java/libIluJava.so /tilde/jacobi/ns4/plugins/libIluJava.so

Copy the regular ilu jar file to the spot where netscape can find it
cp /rubberduckyChris/ilus/runtime/java/ilu.jar   /tilde/jacobi/ns4/java/classes/ilu.jar

Compiling this needs access to the netscape.security.PrivilegeManager
cd /rubberduckyChris/ilus/runtime/java
javac -d classes -classpath ./classes:/tilde/jacobi/ns4/java/classes/java40.jar IluMozillaExtra.java

Make an extra jar file for this class
rm /tilde/jacobi/ns4/java/classes/ilumozilla.jar
cd classes; jar -0cf ../ilumozilla.jar xerox/ilu/IluMozillaExtra.class; cd ..

Copy the extra jar file to the spot where netscape can find it
cp /rubberduckyChris/ilus/runtime/java/ilumozilla.jar /tilde/jacobi/ns4/java/classes/ilumozilla.jar

*************************************** */




package xerox.ilu;
import netscape.security.PrivilegeManager;
 

/*friendly*/ class IluMozillaExtra extends IluMozillaBase {

    /** overloads corresponding IluMozillaBase method */
    public void loadNativeLibrary(java.lang.String libname) 
            throws java.lang.UnsatisfiedLinkError {
        java.lang.System.out.println("===enter IluMozillaExtra.lnl");
        PrivilegeManager.enablePrivilege("UniversalLinkAccess");
        java.lang.System.out.println("---done enablePrivilege");
        try {
            java.lang.System.out.println(
                "---try loading /tilde/jacobi/ns4/plugins/libIluJava.so"
                );
            java.lang.System.load("/tilde/jacobi/ns4/plugins/libIluJava.so");
            java.lang.System.out.println(
                "---done loading /tilde/jacobi/ns4/plugins/libIluJava.so"
                );
        } catch (UnsatisfiedLinkError e) {
            java.lang.System.out.println(
                "---failed loading /tilde/jacobi/ns4/plugins/libIluJava.so"
                );
            java.lang.System.out.println("***UnsatisfiedLinkError " + e);
            throw e;
        }
        PrivilegeManager.revertPrivilege("UniversalLinkAccess");
        java.lang.System.out.println("===exit IluMozillaExtra.lnl");
    } //loadNativeLibrary


    /** overloads corresponding IluMozillaBase method */
    public java.lang.String getSystemProperty(java.lang.String key) {
        java.lang.String s;
        PrivilegeManager.enablePrivilege("UniversalPropertyRead");
        s = System.getProperty(key);
        PrivilegeManager.revertPrivilege("UniversalPropertyRead");
        return s;
    } //getSystemProperty


} //IluMozillaExtra


