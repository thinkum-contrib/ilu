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
/* IluRT0.java */
/* Chris Jacobi, January 6, 1999 4:57 pm PST */
/* $Id: IluRT0.java,v 1.58 1999/08/03 01:54:04 janssen Exp $ */ 
 
/* 
 * See also IluJava_IluRT0.c
 */

package xerox.ilu;


/**
 * Top level implementation of ilu features.  <p>
 *
 * Applications rather use Ilu for most of the functionality.
 * @see Ilu
 */
public class IluRT0 {

    private static int traceObjects = IluDebug.traceObjects();
    
    /** 
     * Initializes IluRT0 and the rest of ilu; idempotent 
     */
    public static void init() {
        IluInit.init();
    } //init

    
    /** 
     * Shortcut, internally used 
     */
    public static void throwErr(java.lang.String s) {
        s = "**xerox.ilu.IluRT0." + s;
        IluDebug.clientPreError(s);
        throw new org.omg.CORBA.BAD_OPERATION(s);
    } //throwErr
    
    
    /** 
     * Shortcut internally used 
     */
    public static void panic(java.lang.String s) {
        s = "**xerox.ilu.IluRT0." + s;
        IluDebug.panic(s);
    } //panic
    
    
    /** 
     * Importing an object.<p>
     * Object might be created as consequence of this call. <br>
     * Returned object might be a subclass of iluClass; in fact a null
     * iluClass is interpreted as the rooClass.  This is merely a
     * type check. <br>
     * This call does not automaticly load unknown ilu types
     * @see Ilu#objectFromSBH
     */
    public static java.lang.Object  
    objectFromSBH (
                java.lang.String sbh, 
                xerox.ilu.IluClassRep iluClass
                ) throws org.omg.CORBA.SystemException
    {
        java.lang.Object obj = null;
        if (sbh == null) throwErr("null ih");
        xerox.ilu.IluOInt oi = oiFromSBH(sbh, iluClass);
        if (oi == null) {
            IluDebug.clientPreError("objectFromSBH finds no object");
            throw new org.omg.CORBA.OBJECT_NOT_EXIST(
                "no object for [sbh = '" + sbh + "']"
                );
        }
        obj = oi.getTheObject();
        /*$ idb */  if (traceObjects>0) {
        /*$ idb */      IluDebug.log.println("! ILU: objectFromSBH found " 
        /*$ idb */          + obj + " oi:" + oi);
        /*$ idb */  }
        return obj;
    } //objectFromSBH
    
    
    /**
     * Native sub-part for objectFromSBH. <p> 
     * New "unfinished" IluOInt might be created.
     */
    /*friends*/ static native xerox.ilu.IluOInt  
    oiFromSBH (
        java.lang.String sbh, 
        xerox.ilu.IluClassRep iluClass
          ) throws org.omg.CORBA.SystemException;
    

    /** 
     * Importing an object.<p>
     * Object might be created as consequence of this call. 
     */
    public static  
    java.lang.Object lookupObject(
                java.lang.String sid, 
                java.lang.String ih, 
                xerox.ilu.IluClassRep iluClass
             ) throws org.omg.CORBA.SystemException
    {
        java.lang.Object obj = null;
        if (sid == null) {throwErr("null sid");}
        if (ih == null) {throwErr("null ih");}
        xerox.ilu.IluOInt oi = oiFromLookup(sid, ih, iluClass);
        if (oi == null) {
            IluDebug.clientPreError("lookup finds no object");
            throw new org.omg.CORBA.OBJECT_NOT_EXIST(
                "no object for [sid = '" + sid + "' ih = '" + ih + "']"
                );
        }
        obj = oi.getTheObject();
        /*$ idb */  if (traceObjects > 0) {
        /*$ idb */      IluDebug.log.println("! ILU: lookupObject found " 
        /*$ idb */          + obj + " oi:" + oi);
        /*$ idb */  }
        return obj;
    } //lookupObject
    
    
    /**
     * Native sub-part for lookupObject <p>
     * New "unfinished" IluOInt might be created.
     */
    private static native xerox.ilu.IluOInt  
    oiFromLookup (
        java.lang.String sid, 
        java.lang.String ih, 
        xerox.ilu.IluClassRep iluClass
        ) throws org.omg.CORBA.SystemException;
    
    
    /**
     * Generates a string that's unique over space and time. <p>
     * A server with nothing better to use might call this to 
     * get an ID.
     * @See Ilu.inventID
     */
    public native static java.lang.String  
    inventID();
    
    
    /** 
     * deprecated fields: use interface instead.
     * Literals for the lifetime argument of registerTrueObject.
     * Remember obj to prevent java garbage collection.
     * Forget obj to subject it to java garbage collection. 
     * Unspec requires ilu set a value depending on other arguments. 
     * @see IluLifetimeArgs
     */
    public static final int unspec = IluLifetimeArgs.iluLifetimeUnspec;
    public static final int remember = IluLifetimeArgs.iluLifetimeRemember;
    public static final int forget = IluLifetimeArgs.iluLifetimeForget;
    
    
    /**
     * Create true objects.<p>
     *
     * Life time is important and must be determined.  Normally the ilu
     * object stays alive it is garbage collected.  There are other
     * schemes to prevent garbage collection. <p>
     
     * 
     * Return value is intermediate object.  <p>
     *
     * @see IluLifetimeArgs
     * @see IluIHProposer
     * @see Ilu#registerTrueObject
     */
    public static java.lang.Object 
    registerTrueObject(
                java.lang.String ih,
                java.lang.Object obj, 
                xerox.ilu.IluServer server, 
                IluClassRep iluClass,
                int lifetime //0 gives default behaviour,
                ) throws org.omg.CORBA.SystemException
    {
        xerox.ilu.IluOInt oi;
        if (obj == null) {
            throwErr("null object");
        }
        if (iluClass == null) {
            java.lang.Class jclz = obj.getClass();
            iluClass = xerox.ilu.IluClassAccess.iluClassFromJavaClass(jclz);
            if (iluClass == null) {
                throwErr("IluClass of object unknown");
            }
        }
        iluClass.checkClass();
        if (ih == null) {
            if (obj instanceof IluIHProposer) {
                ih = ((IluIHProposer)obj).proposeIluIH();
            }
            if (ih == null) {
                ih = inventID();
            }
        }
        if (server == null) {
            server = IluServer.defaultServer();
        }
        server.finishInitialization(); 
        oi = xerox.ilu.IluOInt.newOI(obj, iluClass, server); 
              //throws exception if object already known to Ilu
        if (oi.jjClassRep == null) {
            throwErr("registerTrueObject jjClassRep missing ");
        }
        registerTrue(oi, ih, server);
        if (oi.yIluKernelObject == 0) {
            oi.forgetOI();
            throwErr("registerTrueObject: object already exists");
        }
        if (oi.yServer == 0) {
            oi.forgetOI();
            throwErr("server has been destroyed");
        }
        oi.setLifetime(lifetime);
        return oi;
    } //registerTrueObject
    
    
    /**
     * Native part of registerTrueObject
     * check out jjoi.yIluKernelObject to look for success
     */
    private static native void
    registerTrue(
        xerox.ilu.IluOInt jjoi, 
        java.lang.String ih, 
        xerox.ilu.IluServer server);
    
    
    /** 
     * Publishes the SBH of the object in the local object domain
     */
    public static void
    publishTrueObject(java.lang.Object obj0) 
            throws org.omg.CORBA.SystemException
    {
        java.lang.Object obj = obj0;
        if (obj instanceof IluSurrogateObject) {throwErr("is surrogate");}
        IluOInt oi = IluOInt.peek(obj);
        if (oi == null) {
            //object unknown; try to register it right here and now...
            java.lang.String ih = null;
            if (obj instanceof IluIHProposer) {
                ih = ((IluIHProposer)obj).proposeIluIH();
            }
            if (ih == null) {
                throwErr("publishTrueObject does not invent instance handles");
            }
            java.lang.Class jclz = obj.getClass();
            IluClassRep iluClass = IluClassAccess.iluClassFromJavaClass(jclz);
            if (iluClass != null) {
                IluRT0.registerTrueObject(
                    ih,       //ih
                    obj, 
                    null,     //server
                    iluClass,
                    0         //lifetime
                    );
                oi = IluOInt.peek(obj);
            }
            if (oi == null) {
                throwErr("object not registerd with Ilu " + obj);
            }
        }
        /*$ idb */  if (traceObjects > 0) {
        /*$ idb */      IluDebug.log.println("! ILU:publishTrueObject " 
        /*$ idb */          + obj + " oi:" + oi); 
        /*$ idb */  }
        oi.publishOI();
        if (oi.yOwnerKey == 0) {
            throwErr("Couldn't publish");
        }
    } //publishTrueObject
    
    
    /** 
     * withdrawObject is undo of publishTrueObject
     */
    public static void
    withdrawObject(java.lang.Object obj)
    {
        if (obj instanceof IluSurrogateObject) {throwErr("is surrogate");}
        IluOInt oi = IluOInt.peek(obj);
        /*$ idb */  if (traceObjects>0) {
        /*$ idb */      IluDebug.log.println("! ILU withdrawObject " 
        /*$ idb */          + obj + " oi:" + oi); 
        /*$ idb */  }
        if (oi != null) {
            oi.withdrawOI();
        }
    } //withdrawObject
    
    
    /** 
     * May return NIL if the object's server isn't exported through any
     * port; may return an invalid SBH if the cached one references a
     * closed port.
     * @See Ilu.sbhOfObject
     */
    public static java.lang.String 
    sbhOfObject(java.lang.Object obj)
    {
        IluOInt oi = IluOInt.peek(obj);
        if (oi == null) return null;
        return oi.sbhOfOI();
    } //sbhOfObject
    
        
    /** 
     * Returns OMG IIOP-specified IOR string for object.
     * May return null if object is not exported through IIOP.
     * @See Ilu.iorOfObject
     */
    public static java.lang.String 
    iorOfObject(java.lang.Object obj) {
        IluOInt oi = IluOInt.peek(obj);
        if (oi == null) return null;
        return oi.iorOfOI();
    } //iorOfObject
    

    /** 
     * Destroy ILU-ness of object.  Only for impatient applications,
     * normal applications leave this to the garbage collector.
     * Do NOT use from applets; error behavior is not yet defined.
     *
     * True objects only.
     * @See Ilu.destroyObject
     */
    public static void destroyObject(java.lang.Object obj)
    {
        if (obj instanceof IluSurrogateObject) {
            throwErr("is surrogate");
        }
        IluOInt oi = IluOInt.peek(obj);
        /*$ idb */  if (traceObjects>0) {
        /*$ idb */      IluDebug.log.println("! ILU destroyObject ob: " 
        /*$ idb */          + obj + " oi:" + oi); 
        /*$ idb */  }
        if (oi != null) {
            oi.destroyRudeOI();
        }
    } //destroyObject
    
    
    /** 
     * Test whether object is alive
     * @See Ilu.ping
     */
    public static void
    ping(java.lang.Object obj)
    {
        IluOInt oi = IluOInt.peek(obj);
        if (oi == null) {
            throw new org.omg.CORBA.INV_OBJREF();
        }
        oi.nativePingOI();
    } //ping
    
    /** 
     * Test whether object is known to ILU
     */
    public static boolean
    known(java.lang.Object obj)
    {
        IluOInt oi = IluOInt.peek(obj);
        if (oi == null) return false;
        return true;
    } //known
    
    
    /** 
     * Returns HTTP URL for an object.
     * May return null if object is not exported through an HTTP ilu_Port
     */
    public static java.lang.String 
    URLOfObject(java.lang.Object obj)
    {
        IluOInt oi = IluOInt.peek(obj);
        if (oi == null) return null;
        return oi.nativeURLOfObject();
    } //URLOfObject

    
    /**
     * Prevent garbage collection of true object. 
     * Object will not be garbage collected as long as its server is alive. 
     * Using retainTrueObject directly will override any setting made
     * with the lifetime argument of registerTrueObject or
     * IluObjectTable.returnTrueObject, as well as any marker interface 
     * settings.
     */
    public static void 
    retainTrueObject(java.lang.Object obj) {
        IluOInt.retain(obj);
    } //retainTrueObject
    
    
    /**
     * Allows garbage collection of true object.  
     * (undo of all previous calls of retainTrueObject).  
     * Using exposeTrueObject directly will override any setting made
     * with the lifetime argument of registerTrueObject or
     * IluObjectTable.returnTrueObject, as well as any marker interface 
     * settings.
     */
    public static void 
    exposeTrueObject(java.lang.Object obj) {
        IluOInt.expose(obj);
    } //exposeTrueObject
    
    
    /**
     * Returns whether obj is a surrogate object.
     * i.e. not a true object.  
     */
    public static boolean 
    isSurrogate(java.lang.Object obj)
    {
        IluOInt oi = IluOInt.peek(obj);
        if (oi == null) return false;
        IluServer s = oi.jjServer;
        if (s == null) return true; 
        return s.isSurrogateServer();
    } //isSurrogate
    
    
    /**
     * Returns whether two objects are siblings. 
     * (Whether they belong to the same ilu server)<p>
     *
     * This is much more lightweight then figuring out their IluServer
     */
    public static boolean 
    objectsAreSiblings(java.lang.Object obj1, java.lang.Object obj2)
    {
        IluOInt oi1 = IluOInt.peek(obj1);
        IluOInt oi2 = IluOInt.peek(obj2);
        if (oi1 == null) return false;
        if (oi2 == null) return false;
        if (oi1.yServer == 0) return false;
        return (oi1.yServer == oi2.yServer);
    } //objectsAreSiblings
    
    
    /**
     * Returns the ilu version.
     */
    public native static java.lang.String iluVersion();
    
    
    /**
     * Returns internal ilu information.
     * See the implementation for the encoding.
     */ 
    /*friendly*/ native static int iluConfigurationData(int x);
    
    
    /**
     * Returns whether ilu has been configured with secure transports or not.
     */ 
    public static boolean hasSecurity() {
        return (iluConfigurationData(10)>0);
    }
    
    /**
     * Returns current budget for file descriptors.
     * See the ilu kernel documentation.
     */ 
    public static native int getFDBudget();    
    
    /**
     * Sets the FD budget to n, if possible.
     * Stay within bounds of what the operating system allows
     * and return new number. 
     */ 
    /*friendly*/ static native int nSetFDBudget(int n);    
    
    /**
     * Sets the FD budget to n, if possible.
     */ 
    public static int setFDBudget(int n) {
        //disable use from applets
        return nSetFDBudget(n);
    } //setFDBudget  
    
    
    static {
        init();
    }

} // IluRT0
