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
/* Chris Jacobi, December 26, 1998 10:21 pm PST */

package xerox.ilujava;

/**
 * A class loader which loads classes over the ilu wire.
 * Inherently unsafe.  Needs to be loaded by application if
 * the application wants to retrieve classes from the wire.<p>
 * (Does not load itself automaticly)
 */
public class IluOBVWireClassLoader 
        extends java.lang.ClassLoader 
        implements IluOBVSetter
{
    public static final java.lang.String cachedAccess =
        "wire_cached"; 
    
    public static final java.lang.String unCachedAccess =
        "wire_uncached"; 
    
    /** "shared" access is implemented by using one single accessor */
    
    
    static boolean debug = IluOBV.debug;
    
    ClassAccessor accessor = null;
    java.lang.String accessMethod;
    java.util.Hashtable cache = new java.util.Hashtable();
    
    
    /** 
     * Semi public.
     * Caching applications use the IluOBVGetClassLoader on
     * the prototype instance instead.
     */
    public IluOBVWireClassLoader(
            java.lang.String accessMethod, 
            ClassAccessor accessor)
    {
        this.accessMethod = accessMethod;
        this.accessor = accessor;
        if (debug) {
            System.out.println(
                "IluOBVWireClassLoader created for " + accessor
                );
        }
    } //constructor 
    

    /**
     * Implementing IluOBVSetter.<p>
     * Objects loaded with this classloader will be outputed using
     * this very classloader on eventual re-output.
     */
    public void iluOBVSet(
        MarshalledObjectBase m, 
        java.lang.Object obj) throws java.io.IOException
    {   
        IluOBV.setMO(m, obj, this.accessMethod, this.accessor);
    } //iluOBVSet
    
    
    private byte[] getClassData(java.lang.String name) {
        byte[] b = null;
        if (debug) {
            System.out.println("class loader accessing bytes for " + name);
        }
        try {
            b = accessor.GetBytes(name);
        } catch (xerox.ilujava.AccessError e) {
            System.out.println("Failed accessing class [" + name + "]: " + e);
        }
        if (debug) {
            System.out.println("class loader accessing bytes done");
        }
        return b;
    } //getClassData
    
    
    /** For subclassing ClassLoader */
    public synchronized java.lang.Class loadClass(
        java.lang.String name,
        boolean resolve)
    {
        java.lang.Class c = (java.lang.Class) cache.get(name);
        if (c == null) {
            try {
                c = findSystemClass(name);
                cache.put(name, c);
                if (resolve) {
                    resolveClass(c);
                }
                return c;
            } catch (ClassNotFoundException e) {
            }
            byte[] data = getClassData(name);
            c = defineClass(name, data, 0, data.length);
            cache.put(name, c);
        }
        if (resolve) {
            resolveClass(c);
        }
        return c;
    } //loadClass
    

    static {
        System.out.println("**** this class loader is inherently unsafe. "); 
        System.out.println("It allows accessing bytecodes over the wire");
        
        xerox.ilujava._allJavaStubs.load();
        IluOBVWireClassLoaderGetter getter = 
            new IluOBVWireClassLoaderGetter(true);
        IluOBV.registerClassLoaderGetter(
            cachedAccess, new IluOBVWireClassLoaderGetter(true)
            );
        IluOBV.registerClassLoaderGetter(
            unCachedAccess, new IluOBVWireClassLoaderGetter(false)
            );
    }
    
    /** 
     * A simple way for clients to load this class. When loaded this
     * class will register itself with IluOBV.
     */
    public static void load() {
    }
    
} //IluOBVWireClassLoader


/*friendly*/ class IluOBVWireClassLoaderGetter 
    implements IluOBVClassLoaderGetter {
    
    private boolean cached;
       
    public IluOBVWireClassLoaderGetter(boolean cached) {
        this.cached = cached;
    } //constructor
    
    /**
     * Implementing IluOBVClassLoaderGetter.<p>
     * (A class loader factory for an accessMethod)
     */
    public java.lang.ClassLoader IluOBVGetClassLoader(
        MarshalledObjectBase m) throws java.lang.ClassNotFoundException
    {   
        if (! cached) {
            return new IluOBVWireClassLoader(
                IluOBVWireClassLoader.unCachedAccess, m.accessor
                );
        }
        java.lang.ClassLoader loader = null;
        xerox.ilujava.ClassAccessor acsr = m.accessor;
        //
        //Check accessor first to see whether it has a cached loader
        if (acsr instanceof IluOBVClassLoaderGetter) {
            IluOBVClassLoaderGetter getter =
                (IluOBVClassLoaderGetter) acsr;
            loader = getter.IluOBVGetClassLoader(m);
            if (loader != null) return loader;
        }
        //
        //Wasn't cached; create a loader
        loader = new IluOBVWireClassLoader(
            IluOBVWireClassLoader.cachedAccess, m.accessor
            );
        //
        //Check whether loader can be cached
        if (acsr instanceof IluOBVClassLoaderSetter) {
            IluOBVClassLoaderSetter setter =
                (IluOBVClassLoaderSetter) acsr;
            setter.IluOBVSetClassLoader(loader);
        }
        return loader;
    } //IluOBVGetClassLoader
    
} //IluOBVWireClassLoaderGetter
