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
/* IluClassRep.java */
/* Chris Jacobi, January 6, 1999 5:02 pm PST */
/* $Id: IluClassRep.java,v 1.50 1999/08/03 01:53:49 janssen Exp $ */



/* 
 * See also IluJava_IluClassRep.c
 */

package xerox.ilu;

/**
 * An IluClassRep represents the ilu-class of an ILU object.<p>
 *
 * To applications an IluClassRep is immutable.  
 * IluClassRep's are assembled by stubs.<p>
 *
 * To access the class (IluClassRep), use one of these methods:<br>
 *   - The generated stub has a static iluClass() method.<br>
 *   - Use the IluClassRep.fromIluObject method.
 *   - Use the IluClassRep.fromIluClassName method. 
 *
 * @see IluMethodRep
 * @see IluExceptionRep
 */
public final class IluClassRep extends IluWPBase {
    
    private long yIluClass = 0;
        //No garbage collection necessary as ILU will never free classes anyway
        
    private java.lang.String refIfJName; //java-ifc name for object references
    private java.lang.String jjIluClassName;
    private java.lang.String jjbrand = null;
    private java.lang.String jjuid = null;
    private java.lang.String jjsingleton = null;
    private boolean optional = false;
    private boolean collectible = false;
    private java.lang.String docString = null;
    private int method_count = 0;
    private IluClassRep[] jjIluSuperClasses = null;
    private java.lang.String jjIfBrand = null;
    private java.lang.String jjIfName = null;
    /*friends*/ IluMethodRep[] jjMethods = null;
    /*friends*/ java.lang.Class surrogateJClass = null; 
    /*friends*/ IluFactory factory = null;
    /*friends*/ xerox.ilu.IluSkeleton defaultSkeleton = null;
    /*friends*/ static java.util.Hashtable gClassTable = 
        new java.util.Hashtable();
        
    static {
        IluInit.init();
    }
    
    /** 
     * Prevent creation of bogus IluClassRep's
     */
    private IluClassRep() {
        super();
    }
    
    /** 
     * "Constructor" to be used by stubs only.<p>
     *
     * This sets IluClassRep up but does not yet talk to the ilu kernel
     * Java stubs will collect the data but the kernel call will be made
     * when everything is ready only
     * (so stubs don't need to acquire the kernel monitor otmu)<br>
     *
     * Stubs then use IluMethodRep's to continue defining this class 
     * and finally use finishClass to finalize the construction.<br>
     *
     * The iluClassName must include the interface (unlike other type names)
     */ 
    //going to prepare iluxport.h: ilu_DefineObjectType
    public static IluClassRep setupClass (
            java.lang.String refIfJName, //java-ifc name for object references
            java.lang.String iluClassName, //class name (in ilu's name space)
            java.lang.String uuid,
            int method_count) throws org.omg.CORBA.SystemException {
        IluClassRep cls = new IluClassRep();
        cls.refIfJName = refIfJName;
        cls.jjIluClassName = iluClassName;
        cls.jjuid = uuid;
        cls.method_count = method_count;
        cls.jjMethods = new IluMethodRep[method_count];
        return cls;
    } // setupClass

    /** 
     * Talks to the ilu kernel and sets up the real data
     */ 
    private native void nativeFinishClass(int i) 
            throws org.omg.CORBA.SystemException;
    
    /** 
     * Finishes the registration of the class;
     * at this time the registration is forwarded all the way
     * to the Ilu kernel.
     * Stub use only. 
     */ 
    public void finishClass() throws org.omg.CORBA.SystemException {
        if (yIluClass != 0) {
            throw new org.omg.CORBA.INTERNAL();
        }
        for (int i = 0 ; i<method_count ; i++) {
            if (this.jjMethods[i]==null) {
                IluDebug.clientPreError("methods of (stub) class inconsistent");
                throw new org.omg.CORBA.INTERNAL();
            }
            if (this.jjMethods[i].skeleton==null) {
                this.jjMethods[i].skeleton = defaultSkeleton;
                if (defaultSkeleton==null) {
                    throw new org.omg.CORBA.INTERNAL();
                }
            }
        }
       if (this.jjIluSuperClasses == null) {
           this.jjIluSuperClasses = new IluClassRep[0];
       }
       nativeFinishClass(0); 
       //Prevent garbage collection as we need to hang on to the
       //methods (used for dispatching in true objects) at least
       //as long as the kernel does.
       gClassTable.put(jjIluClassName, this);
       //
       //Pure paranoya, but we don't want a memory smash when some  
       //stupid (malicious) stub reuses the same jjIluClassName for 
       //a different class
       xerox.basics.VMExtras.makeGCRoot(this);
    } //finishClass
    
    
    /*friendly*/ void finishRootClass() throws org.omg.CORBA.SystemException {
        if (yIluClass != 0) {
            throw new org.omg.CORBA.INTERNAL();
        }
        if (this != IluRootClassObjects_stub.rootClass) {
            throw new org.omg.CORBA.INTERNAL();
        }
        nativeFinishClass(1);
    } //finishRootClass
    
    
    /**
     * Defines the surrogate class. 
     * Returns self 
     */
    public IluClassRep setSurrClass(java.lang.String name) 
            throws org.omg.CORBA.SystemException {
        if (yIluClass != 0 & !IluEnvironment.isPriviledged(2)) {
            throw new org.omg.CORBA.INTERNAL();
        }
        try {
            this.surrogateJClass = java.lang.Class.forName(name);
        } catch (java.lang.ClassNotFoundException e) {
            IluDebug.clientPreError("bad java stub class " + name);
            throw new org.omg.CORBA.INTERNAL("bad java stub class " + name);
        }
        return this;
    } //setSurrClass

    
    /**
     * Defines the surrogate factory. 
     * Returns self 
     */
    public IluClassRep setSurrFactory(xerox.ilu.IluFactory factory) 
            throws org.omg.CORBA.SystemException {
        if (yIluClass != 0 & !IluEnvironment.isPriviledged(2)) {
            throw new org.omg.CORBA.INTERNAL();
        }
        this.factory = factory;
        return this;
    } //setSurrFactory
    
    
    /**
     * Defines the doc-string. 
     * Returns self 
     * Stub only
     */
    public IluClassRep setDocString(java.lang.String s) 
            throws org.omg.CORBA.SystemException {
        if (yIluClass != 0) {
            throw new org.omg.CORBA.INTERNAL();
        }
        this.docString = s;
        return this;
    } //setDocString
    

    /**
     * Defines the class to be collectible. <p>
     * Returns self. <br>
     * Stub only. <br>
     * Types inheriting from collectible are not automatically collectible
     * unless explicitly marked as such. Collectible supertypes are 
     * necessary but not sufficient for a class to be collectible. 
     */
    public IluClassRep setCollectable() 
            throws org.omg.CORBA.SystemException {
        if (yIluClass != 0) {
            throw new org.omg.CORBA.INTERNAL();
        }
        xerox.ilu.IluGCClient.initPhaseX();
        this.collectible = true;
        return this;
    } //setCollectable
    
    
    /**
     * Makes this optional. 
     * Returns self 
     * Stub only
     */
    public IluClassRep setOptional() 
            throws org.omg.CORBA.SystemException {
        if (yIluClass != 0) {
            throw new org.omg.CORBA.INTERNAL();
        }
        this.optional = true;
        return this;
    } //setOptional
    
    
    /**
     * Makes this a singleton. 
     * Returns self 
     * Stub only
     */
    public IluClassRep setSingleton(java.lang.String singleton) 
            throws org.omg.CORBA.SystemException {
        if (yIluClass != 0) {
            throw new org.omg.CORBA.INTERNAL();
        }
        this.jjsingleton = singleton;
        return this;
    } //setSingleton
    
    
    /**
     * Sets a brand name. 
     * Returns self 
     * Stub only
     */
    public IluClassRep setBrand(java.lang.String brand) 
            throws org.omg.CORBA.SystemException {
        if (yIluClass != 0) {
            throw new org.omg.CORBA.INTERNAL();
        }
        this.jjbrand = brand;
        return this;
    } //setBrand
    

    /**
     * Defines brand name of isl-interface. 
     * Returns self 
     * Stub only
     */
    public IluClassRep setIfBrand(java.lang.String brand) 
            throws org.omg.CORBA.SystemException {
        if (yIluClass != 0) {
            throw new org.omg.CORBA.INTERNAL();
        }
        this.jjIfBrand = brand;
        return this;
    } //setIfBrand
    

    /**
     * Defines name of isl-interface. 
     * Returns self 
     * Stub only
     */
    public IluClassRep setIfName(java.lang.String name) 
            throws org.omg.CORBA.SystemException {
        if (yIluClass != 0) {
            throw new org.omg.CORBA.INTERNAL();
        }
        this.jjIfName = name;
        return this;
    } //setIfName
    

    /**
     * Defines the super classes. 
     * Returns self 
     */
    public IluClassRep setSuperClasses(
            xerox.ilu.IluClassRep[] iluSuperClasses) 
            throws org.omg.CORBA.SystemException {
        if (yIluClass != 0) {
            throw new org.omg.CORBA.INTERNAL();
        }
        for (int i = 0; i<iluSuperClasses.length; i++) {
            iluSuperClasses[i].checkClass();
        }
        this.jjIluSuperClasses = iluSuperClasses;
        return this;
    } //setSuperClasses


    /**
     * Defines skeleton used by methods without skeleton declaration. 
     * Returns self 
     */
    public IluClassRep setDefaultSkeleton(
            xerox.ilu.IluSkeleton defaultSkeleton) 
            throws org.omg.CORBA.SystemException {
        if (yIluClass != 0) {
            throw new org.omg.CORBA.INTERNAL();
        }        this.defaultSkeleton = defaultSkeleton;
        return this;
    } //setDefaultSkeleton
    
    
    /**
     * Raises an exception if the classRep is not well defined 
     * Returns self 
     */
    public IluClassRep checkClass() throws org.omg.CORBA.SystemException {
        if (yIluClass == 0) {
            throw new org.omg.CORBA.INTERNAL("use bad IluClassRep " + this);
        }
        return this;
    } //checkClass


    /**
     * Returns class name (in ISL/IDL name space) 
     */
    public final java.lang.String iluClassName() {
        return jjIluClassName;
    }

    /**
     * Returns class id (in ISL/IDL name space) 
     */
    public final java.lang.String iluClassId() {
        return jjuid;
    }


    /**
     * Returns name of the java reference interface (in java's name space) 
     */
    public final java.lang.String referenceIfName() {
        return this.refIfJName;
    }
    
    
    /**
     * Returns IluClassRep given the name of the type in 
     * the ISL/IDL world.  (like Interface.ObjectType).
     * Or null if not registered. 
     */
    public static IluClassRep fromIluClassName(java.lang.String iluClassName) {
        return (IluClassRep) gClassTable.get(iluClassName);
    }
    
    
    /**
     * Returns IluClassRep given an object.
     */
    public static IluClassRep fromIluObject(java.lang.Object obj)
        throws org.omg.CORBA.SystemException 
    {
        IluOInt oi = IluOInt.peek(obj);
        if (oi == null) {
            throw new org.omg.CORBA.BAD_PARAM("not an ilu object");
        }
        return oi.jjClassRep;
    }
    
    
    /**
     * Returns whether "this" is an a, or, a subclass of a.
     */
    public boolean isA(IluClassRep a)
        throws org.omg.CORBA.SystemException 
    {
        if (this == a) return true;
        if (a == IluRootClassObjects_stub.rootClass) return true;
        for (int i = 0; i<this.jjIluSuperClasses.length; i++) {
            if ((this.jjIluSuperClasses[i]).isA(a)) return true;
        } 
        return false;
    } //isA
    
    
    /** 
     * Returns the direct Ilu superclasses of an Ilu class.
     * But does not return the root class. 
     */
    public IluClassRep[] superClasses() {
        IluClassRep[] safeCopy = 
            new IluClassRep[this.jjIluSuperClasses.length];
        java.lang.System.arraycopy(
            this.jjIluSuperClasses, 0, safeCopy, 0, safeCopy.length);
        return safeCopy;
    } //superClasses
    
    
    /** 
     * Returns the number of super classes.
     */
    public int superClassCnt() {
        int cnt = 0;
        for (int i = 0; i<this.jjIluSuperClasses.length; i++) {
            cnt = cnt + 1 + this.jjIluSuperClasses[i].superClassCnt();
        }
        return cnt;
    } //superClassCnt
    
    
    /** Accessor; returns whether null is a legal value */
    public boolean isOptional() {
        return optional;
    }
    
    
    /** 
     * Accessor; returns whether whether this class is subject to
     * distributed GC.
     */
    public boolean isCollectible() {
        return collectible;
    }
    
    
    /** returns readable representation for debugging */
    public java.lang.String toString() {
       return super.toString() + ":" + this.jjIluClassName;
    }
    
    
    public static final IluClassRep rootClass() {
       return IluRootClassObjects_stub.rootClass;
    }
    
    
    /*friendly*/ static void init () {
    }

    static {
        xerox.basics.VMExtras.makeGCRoot(gClassTable);
    } //static
    
} //IluClassRep



/** 
 * This is the stub used for objects whose only known
 * class is the root class.
 */
/*friendly*/
class IluRootClassObjects_stub 
        extends xerox.ilu.IluSurrogateObject 
        implements org.omg.CORBA.Object {
    
    /*friendly*/ IluRootClassObjects_stub() {
    } //constructor
    
    /*friendly*/ static IluClassRep rootClass = null;
    
    static {
        if ((!IluInit.abortInitializations()) && rootClass==null) {
            if (IluDebug.initsFlag > 0) {
                System.err.println(" before init ilu:root-object-type");
            }
            rootClass = IluClassRep.setupClass(
                "org.omg.CORBA.Object", //java reference interface
                "ilu.Object", //ilu object type
                "ilu:root-object-type", //uuid
                0); //method count
            rootClass.setBrand("version 2");
            rootClass.setSurrClass("xerox.ilu.IluRootClassObjects_stub");
            rootClass.finishRootClass();
            IluRootObjectHelper.id();
            //There are only surrogate objects of type "rootclass",
            //true objects must have a known class.
            if (IluDebug.initsFlag > 0) {
                System.err.println(" after init ilu:root-object-type");
            }
        }
    } //static

} //IluRootClassObjects_stub



