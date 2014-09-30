/* $Id: IluOBV.java,v 1.2 1999/08/03 01:56:20 janssen Exp $
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
/* Chris Jacobi, January 6, 1999 10:24 pm PST */
/* Last edited by Mike Spreitzer October 9, 1998 1:31 pm PDT */


package xerox.ilujava;

/**
 * Class implementing 
 * xerox.ilujava.JavaObject 
 * and 
 * xerox.ilujava.MarshalledObjectBase's
 * methods. <p>
 *
 * Internally also used as custom record for MarshalledObjectBase.
 */
public class IluOBV 
        extends xerox.ilujava.MarshalledObjectBase
        implements java.lang.Cloneable, java.io.Serializable
        { 
    
    /** 
     * A cache for the internalized java object. 
     * allways matches super.sb <br>
     * Monitored with globalLock
     */ 
    /*friendly*/ transient java.lang.Object cachedObject = null;
    
    /** For internal use */
    public static final boolean debug = false;
    
    /** Re-use instead re-allocate since its immutable */
    static final byte[] empty = new byte[0]; 
    
    /** Lock protecting the "match" of cachedObject */
    private static final java.lang.Object globalLock = new Object(); 
    
    /** Current static default values */
    /*friendly*/ static ClassAccessor defaultAccessor = null;
    /*friendly*/ static java.lang.String defaultAccessMethod = null;
    
    /*friendly*/ static java.util.Hashtable classLoaderName2ObjectSetter = 
        new java.util.Hashtable();
        
    public static void registerObjectSetter(
        java.lang.String s, IluOBVSetter setter)
    {
        classLoaderName2ObjectSetter.put(s, setter);
    } //registerObjectSetter
    
    
    /** 
     * Given an object suggests a setter.  Returns null
     * if none found. <p>
     *
     * Design rationale: Defaults are not filled in at this level
     * because applications might choose their own idea
     * of defaults  
     */
    public static IluOBVSetter getObjectSetter (
        java.lang.Object obj
        ) 
    {
        java.lang.ClassLoader cl = obj.getClass().getClassLoader();
        if (cl instanceof IluOBVSetter) {
            return (IluOBVSetter) cl;
        }
        java.lang.Object os = classLoaderName2ObjectSetter.get(
            cl.getClass().getName()
            );
        if (os instanceof IluOBVSetter) {
            return (IluOBVSetter) os;
        }
        return null;
    } //getObjectSetter
    

    /** 
     * Sets the default ClassAccessor which will be used for SENDING java 
     * objects which do not have an associated accessor or method
     */
    public static void setDefaultAccessMethod(
        java.lang.String s, ClassAccessor acc
        )
    {
        defaultAccessMethod = s;
        defaultAccessor = acc;
    } //setDefaultAccessMethod
  

    // This makes xerox.ilujava.IluOBV a custom record type
    // for xerox.ilujava.MarshalledObjectBase
    // We want it to be a custom record so it can cache the internalized
    // Java Object.
    static { 
        try {
             xerox.ilujava._allJavaStubs.load();
             xerox.ilujava.MarshalledObjectBase._theClass = 
                 java.lang.Class.forName("xerox.ilujava.IluOBV");
        } catch (java.lang.Exception e) {
            System.err.println("Exception while loading IluOBV: " 
                + e );
            e.printStackTrace(System.err);
        }
        //
        JavaObjectHelper._registerCustomMapping(
            new IluOBV_JavaObjectHandler()
            );
        //
        //making the custom surrogates for ClassAccessor
        xerox.ilu.IluClassRep caRep =  
            xerox.ilujava.ClassAccessorStub.iluClass();
        caRep.setSurrFactory(
            new IluOBV_ClassAccessorCustomSurrogateFactory()
            );
    } //static
    
    
    /** A simple means to assure this class is loaded */
    public static void load() {
    }
    
    
    /** Create a marshaller containing the null object */
    public IluOBV() {
        this.cachedObject = null;
        this.accessMethod = null;
        this.accessor = null;
        this.sb = empty;
    } //constructor
    
    
    /** 
     * Create a xerox.ilujava.MarshalledObjectBase 
     * (respectively the right subclass)
     */
    IluOBV(
        java.lang.Object sob, 
        java.lang.String accessMethod, 
        ClassAccessor accessor) throws java.io.IOException
    {
        set(sob, accessMethod, accessor);
    } //constructor
    
    
    private static java.util.Hashtable registeredLoaders = 
        new java.util.Hashtable(); 
    
    /** 
     * A classloader tells its existence to IluOBV.<p>
     * The string "s" is the accessMethod under which the
     * registered IluOBVClassLoaderGetter
     * will be called.  The "extra" level of indirection is used
     * so that mechanisms can register different classloaders
     * depending on context, if so desired.
     */
    public static void registerClassLoaderGetter(
        java.lang.String s, 
        IluOBVClassLoaderGetter getter)
    {
        registeredLoaders.put(s, getter);
    } //registerClassLoaderGetter
    
    /* Sorry the getSystemClassLoader method is new since JDK1.1 */ 
    /*$ 1.2 *///static {
    /*$ 1.2 *///    registeredLoaders.put(
    /*$ 1.2 *///        "system", java.lang.ClassLoader.getSystemClassLoader()
    /*$ 1.2 *///        );
    /*$ 1.2 *///}

    /** 
     * Find a class loader for interning a MarshalledObjectBase.<p>
     * I'm not sure this is usefull for clients, but it is internally
     * used. 
     */
    public static java.lang.ClassLoader getLoader(MarshalledObjectBase m) 
            throws java.lang.ClassNotFoundException
    {
        java.lang.ClassLoader loader = null;
        //
        //check whether the accessor denotes a ClassLoader
        ClassAccessor acsr = m.accessor;
        if (acsr instanceof IluOBVClassLoaderGetter) {
            IluOBVClassLoaderGetter getter = 
                (IluOBVClassLoaderGetter) acsr;
            loader = getter.IluOBVGetClassLoader(m);
            if (loader != null) return loader;
        }
        //
        //check whether the accessMethod denotes a ClassLoader
        java.lang.Object obj = registeredLoaders.get(m.accessMethod);
        if (obj instanceof IluOBVClassLoaderGetter) {
            IluOBVClassLoaderGetter getter = 
                (IluOBVClassLoaderGetter) obj;
            loader =  getter.IluOBVGetClassLoader(m);
            if (loader != null) return loader;
        } 
        //
        //check whether the table did contain the loader directly
        if (obj instanceof java.lang.ClassLoader) {
            return (java.lang.ClassLoader) obj;
        }
        //
        //No loader has been found
        throw new java.lang.ClassNotFoundException(
            "IluOBV: No class loader registered (" 
            + m.accessMethod + ")"
            );
    } //getLoader
    
    
    /** 
     * Accesses the java object represented by the serialized bytes
     * Shortcut which avoids caching. I don't think this is usefull
     * for external clients.
     */
    public static java.lang.Object getMOUncachecd(MarshalledObjectBase m) 
            throws java.io.IOException, 
            java.lang.ClassNotFoundException 
    {
        byte[] ba = m.sb;
        if (ba == null || ba.length == 0) return null;
        if (debug) {
            System.out.println("IluOBV.getMOUncachecd enter");
        }
        ClassLoader loader = getLoader(m);
        java.io.ByteArrayInputStream in = new java.io.ByteArrayInputStream(ba);
        IluOBV_InStream s = new IluOBV_InStream(loader, in);
        Object obj = s.readObject();
        if (debug) {
            System.out.println("IluOBV.getMOUncachecd got: " + obj);
        }
        return obj;
    } //getMOUncachecd 


    /** 
     * Accesses the java object represented by the serialized bytes.
     * Defined as static procedure to also operate on MarshalledObjectBase
     * super class.
     */
    public static java.lang.Object getMO(MarshalledObjectBase m) 
            throws java.io.IOException, 
            java.lang.ClassNotFoundException 
    {
        if (m == null) return null;
        if (m instanceof IluOBV) {
            IluOBV ma = (IluOBV) m;
            return ma.get();
        }
        return getMOUncachecd(m);
    } //getMO
    
    
    /** 
     * Accesses the java object represented by the serialized bytes.
     * Instance method for known types.  
     */
    public java.lang.Object get() 
            throws java.io.IOException, 
            java.lang.ClassNotFoundException 
    {
        if (this.cachedObject != null) return this.cachedObject;
        byte[] ba = this.sb;
        Object obj = getMOUncachecd(this);
        synchronized (globalLock) {
            if (this.sb == ba) {this.cachedObject = obj;}
        }
        return obj;
    } //get
    
    
    /** 
     * Defines java object refered to by the serialized bytes.
     * I don't think clients find direct use of this to be usefull. 
     */
    public static byte[] externalizeToArray(
            java.lang.Object sob) 
            throws java.io.IOException
    {
        byte[] ba = null;
        if (debug) { 
            System.out.println("IluOBV.externalizeToArray: " + sob);
        }
        if (sob == null) return empty;
        java.io.ByteArrayOutputStream bs = new java.io.ByteArrayOutputStream();
        IluOBV_OutStream os = new IluOBV_OutStream(bs);
        os.writeObject(sob);
        os.flush();
        ba = bs.toByteArray();
        os.close();
        if (debug) {
            System.out.println("IluOBV.externalizeToArray done");
        }
        return ba;
    } //externalizeToArray
    
    
    /** 
     * Defines java object refered to by the serialized bytes.
     * Immediately externalizes a copy.
     */
    void set(java.lang.Object sob) throws java.io.IOException
    {
        set(sob, defaultAccessMethod, defaultAccessor);
    } //set
    
    
    /** 
     * Defines java object refered to by the serialized bytes.
     * Immediately externalizes a copy.
     */
    void set(
        java.lang.Object sob, 
        java.lang.String accessMethod, 
        ClassAccessor accessor) throws java.io.IOException
    {
        byte[] ba = externalizeToArray(sob);
        synchronized (globalLock) {
            this.cachedObject = sob;
            this.sb = ba;
            this.accessMethod = accessMethod;
            this.accessor = accessor;
        }
    } //set
    
    
    /** 
     * Sets java object refered to by MarshalledObjectBase and immediately
     * externalizes a copy.
     */
    public static void setMO(
        MarshalledObjectBase m, 
        java.lang.Object sob) 
            throws java.io.IOException
    {
        setMO(m, sob, defaultAccessMethod, defaultAccessor);
    } //setMO
 
 
    /** 
     * Sets java object refered to by MarshalledObjectBase and immediately
     * externalizes a copy.
     */
    public static void setMO(
        MarshalledObjectBase m, 
        java.lang.Object sob, 
        java.lang.String accessMethod, 
        ClassAccessor accessor) 
            throws java.io.IOException
    {
        if (m instanceof IluOBV) {
            IluOBV ma = (IluOBV) m;
            ma.set(sob, accessMethod, accessor);
        } else {
            byte[] ba = externalizeToArray(sob);
            synchronized (globalLock) {
                m.sb = ba;
                m.accessMethod = accessMethod;
                m.accessor = accessor;
            }
        }
    } //setMO
 
    
} //IluOBV



/** 
 * Helper class for serializing
 */
class IluOBV_OutStream 
        extends java.io.ObjectOutputStream 
{    
    static boolean debug = IluOBV.debug;

    public IluOBV_OutStream(java.io.ByteArrayOutputStream bs) 
            throws java.io.IOException
    {
        super(bs);
        try {
            this.enableReplaceObject(true); 
        } catch (java.lang.SecurityException s) {
            //dont raise exception here; simply fail to serialize
            //those object which shouldn't be serialized when attempted
            if (debug) {
                System.out.println(
                    "security exception caught but reporting delayed " + s
                    );
                s.printStackTrace(System.err);
            }
        }
    } //constructor
    
    
    /**
     * Replace ilu objects by something which works as either
     * true or surrogate object.
     * Called on serialization (output) <p>
     */
    protected java.lang.Object replaceObject(java.lang.Object obj)
	throws java.io.IOException
    {
	//We don't care whether replacement happens here explicitely (jdk1.1)
	//or replacement happened automaticly because of IluObjectBase 
	//method (jdk1.2).  In both cases we serialize an IluSubstitute object
	if (obj instanceof org.omg.CORBA.Object) {
            org.omg.CORBA.Object iobj = (org.omg.CORBA.Object) obj;
            xerox.ilu.IluSubstitute placeholder = 
                new xerox.ilu.IluSubstitute();
            placeholder.sbh = xerox.ilu.IluRT0.sbhOfObject(iobj);
            return placeholder;
        }
	if (obj instanceof java.io.Serializable) {
            return obj;
        }
	return obj; //will later throw an NotSerializableException 
    } //replaceObject

} //IluOBV_OutStream


/** 
 * Helper class for serializing
 */
class IluOBV_InStream 
        extends java.io.ObjectInputStream 
{    
    static boolean debug = IluOBV.debug;
    private ClassLoader myLoader = null;
    
    public IluOBV_InStream(
            ClassLoader loader, 
            java.io.InputStream s 
            ) throws java.io.IOException, java.io.StreamCorruptedException
    {
        super(s);
        try {
           this.enableResolveObject(true); 
        } catch (java.lang.SecurityException se) {
            //dont raise exception here; simply fail to deserialize 
            //those object which shouldn't later
        }
        this.myLoader = loader;
    } //constructor
    
    
    /**
     * Called automaticly by InStream.    
     * Resolving a class means loading it with the right loader.    
     */
    protected java.lang.Class resolveClass(java.io.ObjectStreamClass v)
            throws java.io.IOException, java.lang.ClassNotFoundException
    {
        java.lang.Class clazz = null;
        String name = v.getName();
        if (debug) {
            System.out.println("input stream resolveClass: " + name);
        }
        clazz = this.myLoader.loadClass(name);
        return clazz;
    } //resolveClass
    
    
    /**
     * Undo replacements of ilu objects.
     * (ilu objects are replaced with SBH so that we get true or
     * surrogate object depending on need)  <p>
     * Called automaticly by InStream. <p>
     * Don't confuse this method with the replaceObject in IluSubstitute.  
     * resolveObject in IluSubstitute is an object method; this
     * resolveObject is a stream method.  They have different signatures
     * (exceptions don't match).  Nevertheless, it doesn't matter which of 
     * the two methods is called as long as at least one is.    
     */
    protected java.lang.Object resolveObject(java.lang.Object obj)
	throws java.io.IOException
    {
        //DEFFICIENCY: objects can be resolved twice
        //They can be resolved once here and
        //once automaticly by the deserialization
        //This code here serves JDK1.1 which doesn't yet do
        //the automatic resolving on deserialization 
        /*$ 1.1 */  if (obj instanceof xerox.ilu.IluResolving) {
        /*$ 1.1 */      xerox.ilu.IluResolving resolver = 
        /*$ 1.1 */          (xerox.ilu.IluResolving) obj;
        /*$ 1.1 */      obj = resolver.readResolve();
        /*$ 1.1 */  }
	return obj;  
    } //resolveObject
    
} //IluOBV_InStream


/**
 * This implements the transformation necessary for the full custom mapping
 * of JavaObject. <p>
 *
 * This class is loaded by stubs which use JavaObject through the
 * custom mapping specification.
 */
class IluOBV_JavaObjectHandler 
        extends xerox.ilujava.JavaObjectBase 
        implements xerox.ilu.IluCustomMapping {

    /** The constructor is only used to register the mapping */
    public IluOBV_JavaObjectHandler() {
    } //constructor
    
    /** Small cache, but very usefull in sized protocols */
    static JavaObjectBase remembered = null;
    
    
    /** Transformation for IluCustomMapping */
    public java.lang.Object 
        iluCustomMapping_customFromIlu(java.lang.Object iluObject)
        // no instance data is used; this procedure is registered
        // independent on what JavaObject is used
    {
        if (iluObject == null) return null;
        JavaObjectBase x = (JavaObjectBase) iluObject;
        xerox.ilujava.IluOBV m = (xerox.ilujava.IluOBV) x.mo;
        if (m == null) return null;
        if (x.code==(byte)3) return m; //special case MarshalledObject
        try {
            return m.get();
        } catch (java.lang.ClassNotFoundException e1) {
            //
            System.err.println("**IluCustomMapping class not found: " + e1);
            e1.printStackTrace(System.err);
            //
            throw new xerox.ilu.IluCustomMappingException(
                "custom mapping class not found [" + e1 + "]"
                );
        } catch (java.io.IOException e2) {
            //
            System.err.println("**IluCustomMapping IOException: " + e2);
            e2.printStackTrace(System.err);
            //
            throw new xerox.ilu.IluCustomMappingException(
                "custom mapping io exception because [" + e2 + "]"
                );
        }
    } //iluCustomMapping_customFromIlu
    
        
    
    /** Transformation for IluCustomMapping */
    public java.lang.Object 
        iluCustomMapping_iluFromCustom(java.lang.Object customObject)
        // no instance data is used; this procedure is registered
        // independent on what JavaObject is used
    {
        if (customObject == null) {
            return null;
        }
        xerox.ilujava.JavaObjectBase x = null;
        if (customObject instanceof xerox.ilujava.JavaObjectBase) {
           // the client must have created it directly for some 
           // (unknown to us) purpose
           return customObject;
        }
        if (customObject instanceof xerox.ilujava.MarshalledObjectBase) {
           // the client must explicitely pass an MarshalledObjectBase
           // typed java object.  We try to preserve that.
           MarshalledObjectBase mob1 = (MarshalledObjectBase) customObject;
           return new xerox.ilujava.JavaObjectBase((byte)3, mob1);
        }
        x = remembered;
        if (x != null) {
           xerox.ilujava.MarshalledObjectBase mob2 = x.mo;
           if (mob2 instanceof IluOBV) {
               IluOBV m = (IluOBV) mob2;
               if (m.cachedObject == customObject) {return x;}
           }
        }
        try {
            x = new xerox.ilujava.JavaObjectBase();
            xerox.ilujava.IluOBV m = new xerox.ilujava.IluOBV();
            x.mo = m;
            IluOBVSetter setter = 
                IluOBV.getObjectSetter(customObject); 
            if (setter != null) {
                setter.iluOBVSet(m, customObject);
            }  else {
                 m.set(customObject, 
                     IluOBV.defaultAccessMethod, 
                     IluOBV.defaultAccessor
                     );
            }      
        } catch (java.io.IOException e2) {
           //
           System.err.println("**IluCustomMapping failure: " + e2);
           e2.printStackTrace(System.err);
           //
           throw new xerox.ilu.IluCustomMappingException(
               "custom mapping io exception [" + e2 + "]"
               );
        }
        remembered = x;
        return x;
    } //iluCustomMapping_iluFromCustom

    
} //IluOBV_JavaObjectHandler



/**
 * Helper class: a custom surrogate for ClassAccessor,
 * providing a cached class loader.
 */
class IluOBV_ClassAccessorCustomSurrogate
    extends xerox.ilujava.ClassAccessorStub
    implements 
        xerox.ilujava.IluOBVClassLoaderSetter, 
        xerox.ilujava.IluOBVClassLoaderGetter
{
    transient java.lang.ClassLoader loader = null;
    
    public IluOBV_ClassAccessorCustomSurrogate() {
        super();
    } //constructor
    
    /**
     * The xerox.ilujava.IluOBVClassLoaderSetter method
     */
    public void IluOBVSetClassLoader(java.lang.ClassLoader loader)
    {
        this.loader = loader;
    } //IluOBVSetClassLoader 

    /**
     * The xerox.ilujava.IluOBVClassLoaderGetter method
     */
    public java.lang.ClassLoader IluOBVGetClassLoader(
        xerox.ilujava.MarshalledObjectBase m) 
            throws java.lang.ClassNotFoundException
    {
        return this.loader;
    } //IluOBVGetClassLoader 
    
} //IluOBV_ClassAccessorCustomSurrogate



/**
 * Helper class to make IluOBV_ClassAccessorCustomSurrogate a custom
 * surrogate.
 */
class IluOBV_ClassAccessorCustomSurrogateFactory
    extends xerox.ilu.IluFactory
{
    
    public IluOBV_ClassAccessorCustomSurrogateFactory() {
        super();
    } //constructor
    
    /**
     * This method makes this class an IluFactory
     */
    public java.lang.Object createSurrogateObject(java.lang.Object arg) {
        return new IluOBV_ClassAccessorCustomSurrogate();
    } //createSurrogateObject
  
} //IluOBV_ClassAccessorCustomSurrogate

