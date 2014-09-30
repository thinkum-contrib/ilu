/* $Id $
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
/* Chris Jacobi, December 26, 1998 10:20 pm PST */

package xerox.ilujava;

/**
 * A class loader for IluOBV using URLClassLoader for its impl.  <p>
 * (JDK1.2 only)<p>
 *
 * We do not register a IluOBVClassLoaderSetter, as there is only one
 * static class loader for this accessMethod. <br>
 * Given there is only one static class loader we have the restriction of
 * no support for class unloading.
 */
public class IluOBVURLClassLoader 
        extends java.net.URLClassLoader
        implements IluOBVClassLoaderGetter, IluOBVSetter
{
    
    public static final java.lang.String accessMethod = "URL"; 
    private static java.net.URL[] noUrl = new java.net.URL[0];
    
    /**
     * Constructor not usefull to general clients because we only need 
     * one prototype.<p>
     */
    /*friendly*/ IluOBVURLClassLoader() {
        super(noUrl);
    } //constructor
    

    /**
     * The proto type instance.<p>
     */
    private static IluOBVURLClassLoader proto = null;
    
    /**
     * Appends URL to search path of class loader.<p>
     */
    public static void addURLToPath(java.net.URL url) {
        proto.addURL(url);
    } //addURLToPath
    

    /**
     * Implementing IluOBVClassLoaderGetter.<p>
     * (A class loader factory for an accessMethod)
     */
    public java.lang.ClassLoader IluOBVGetClassLoader(
        MarshalledObjectBase m) throws java.lang.ClassNotFoundException
    {
        return proto;
    } //IluOBVGetClassLoader
    
    
    /**
     * Implementing IluOBVSetter.<p>
     */
    public void iluOBVSet (
        MarshalledObjectBase m, java.lang.Object obj
        ) throws java.io.IOException
    {
        IluOBV.setMO(m, obj, accessMethod, null);
    } //iluOBVSet
    
    
    static {
        proto = new IluOBVURLClassLoader();
        IluOBV.registerClassLoaderGetter(accessMethod, proto);
        IluOBV.registerObjectSetter(
            IluOBVURLClassLoader.class.getName(), proto
            );
    } //static
    

    /**
     * Load the class (Noop).  This needs only be called if no 
     * other method is called which would load the class.
     */
    public static void load() {
    } //load


} //IluOBVURLClassLoader
