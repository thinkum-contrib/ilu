/* $Id: ClassAccessorImplFromJar.java,v 1.2 1999/08/03 01:56:20 janssen Exp $
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
/* Chris Jacobi, December 14, 1998 11:33 pm PST */
/* Last edited by Mike Spreitzer October 9, 1998 1:31 pm PDT */


/*
create jar file like
cd classes
jar cv0f ../MyJarFile.jar *
cd ..

compile
javac -d classes ClassAccessorImplFromJar.java

run
java xerox.ilujava.ClassAccessorImplFromJar

*/


package xerox.ilujava;

/** 
 * Utility class; not really part of ilu.
 * Implementation of a ClassAccessor object. 
 * This is mainly for the example program, but it might be usefull
 * for other applications as well.
 */
public class ClassAccessorImplFromJar extends xerox.ilu.IluObjectBase
    implements xerox.ilujava.ClassAccessor {

    static final char separatorChar = '/';
    static boolean debug = false;
    
    java.lang.String path = null;
    
    public ClassAccessorImplFromJar(java.lang.String path) {
        this.path = path;
    } //constructor
    
    public byte[] GetBytes(java.lang.String classname)
        throws xerox.ilujava.AccessError, org.omg.CORBA.SystemException
    {
        java.lang.String entryName = 
            classname.replace('.', separatorChar) + ".class";
        java.lang.String pathRest = this.path;
        while (pathRest != null) {
            //loop through the filenames found on the path variable
            java.lang.String thisName
                = xerox.basics.Environment.leftOfSeparator(pathRest, ':').trim();
            try {
                if (debug) {
                    System.out.println("Check [" + thisName + "]");
                }
                java.util.zip.ZipFile zf = new java.util.zip.ZipFile(thisName);
                if  (zf != null) {
                    java.util.zip.ZipEntry ze = zf.getEntry(entryName);
                    if  (ze != null) {
                        java.io.InputStream is = zf.getInputStream(ze);
                        long sizeLong = ze.getSize();
                        int sizeInt = (int) sizeLong;
                        if  ((sizeLong == (long) sizeInt) && (sizeInt > 0)) {
                            byte[] b = new byte[sizeInt];
                            int didRead = is.read(b, 0, sizeInt);
                            if (didRead == sizeInt) {
                                is.close();
                                zf.close();
                                if (debug) {
                                    System.out.println(
                                    "Successfully accessed " + classname
                                    );
                                }
                                return b;
                            }
                        }
                        is.close();
                    }
                    zf.close();
                }
            } catch (java.io.IOException e) {
                //try next entry on path
                if (debug) {
                    System.out.println(
                        "exception while accessing [" + thisName + "]: " + e
                        );
                    e.printStackTrace();
                }
            }
        pathRest = xerox.basics.Environment.rightOfSeparator(pathRest, ':');
        }
        throw new xerox.ilujava.AccessError("not available");
    } // GetBytes
     
    /** main is only used to debug ClassAccessorImplFromJar */
    public static void main(String argv[]) {
        System.out.println("Start ClassAccessorImplFromJar.main");
        byte[] b = null;
        ClassAccessorImplFromJar ai = new ClassAccessorImplFromJar(
            "YourJarFile.jar:MyJarFile.jar"
            );
        try {
            b = ai.GetBytes("xerox.ilujava.ClassAccessorImplFromJar");
        } catch (xerox.ilujava.AccessError e) {
            System.out.println("Exception " + e);
            e.printStackTrace();
        }
        System.out.println("Done ClassAccessorImplFromJar.main; got " + b);
    } //main
        
} //ClassAccessorImplFromJar


