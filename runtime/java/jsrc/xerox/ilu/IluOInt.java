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
/* IluOInt.java */
/* Chris Jacobi, January 6, 1999 5:08 pm PST */
/* $Id: IluOInt.java,v 1.63 1999/08/03 01:53:41 janssen Exp $ */

/*
 * This class helps in the mapping from real Java objects to kernel object 
 * See also IluJava_IluOInt.c 
 */

package xerox.ilu;

/**
 * This class helps in the mapping from real Java objects to kernel object;
 * It resides in the middle.  It implements what the kernel considers a 
 * LSO. While applications might hold an IluOInt; none of the operations
 * here should be used by applications.
 */
public final class IluOInt 
        extends org.omg.CORBA.portable.Delegate {
    
    /* friends */ long ywpxIluOInt = 0;
    /* friends */ long yIluKernelObject = 0;
    /* friends */ long yServer = 0; //true AND surrogate
    /* friends */ long yOwnerKey = 0;
    /* friends */ xerox.ilu.IluServer jjServer = null; //currently only if true
    /* friends */ boolean veryInterest = false; //by kernel
    /* friends */ boolean retained = false; //by application
    /* friends */ xerox.ilu.IluReEnableFinalization reFinalizer = null;
    /* friends */ boolean destroyed = false;
    /* friends */ boolean ghost = false; //Used unprotected. Delays garbage
        //collection a turn.  Used conceptionally to un-enqueue finalizations
        //when a disguised ref is undisguised but finalization is already
        //enqueued from before undisguisation.  Since used unprotected, must
        //not participate in proof of memory safety.
    /* friends */ xerox.ilu.IluClassRep jjClassRep = null;
    /* friends */ java.lang.Object jjJavaObject = null; 
        //in synch with peektab
    
    private static java.util.Hashtable globalTable = new java.util.Hashtable();
    private static boolean neverGC = (IluDebug.dontGC() != 0);
    private static int traceObjects = IluDebug.traceObjects();
    private static int traceGC = IluDebug.traceGC();
    private static int traceCalls = IluDebug.traceCalls();
    
    
    /** 
     * Special prototype instance for native side
     * calling pseudo instance methods.
     */
    /*friendly*/ native void reportIluOIntInst();
    /*friendly*/ static IluOInt iluOIntProto = null;


    static {
        IluInit.init();
        if (IluDebug.initsFlag > 0) {
            System.err.println("IluOInt static");
        }
    } //static
    
    
    public static void init() {
        if (IluDebug.initsFlag > 0) {
            System.err.println("IluOInt init");
        }
    } //init
    
    
    /*friendly*/ static void initPhase2() {
        if (IluDebug.initsFlag > 0) {
            System.err.println("IluOInt initPhase2 start");
        }
        if (iluOIntProto != null) {
            IluDebug.panic("init problem");
        }
        iluOIntProto = new IluOInt();
        if (IluDebug.initsFlag > 0) {
            System.err.println("IluOInt initPhase2 middle");
        }
        iluOIntProto.reportIluOIntInst();
        xerox.basics.VMExtras.makeGCRoot(iluOIntProto); 
        if (IluDebug.initsFlag > 0) {
            System.err.println("IluOInt initPhase2 done");
        }
    } //initPhase2
    
    
    private native void setupIluWPBaseIluOInt();
    private native void finalizeIluOInt() throws java.lang.Throwable;
    
    
    /** No random creations */
    private IluOInt() {
        super();
        if (neverGC) {
            //Prevent any garbage collection.
            //Temporary courtesy to early users until
            //finalization is trusted to work correctly.
            xerox.basics.VMExtras.makeGCRoot(this);
        }
        /*$ idb */  if (traceObjects > 0) {
        /*$ idb */      IluDebug.log.println("! IluOInt: new " + this);
        /*$ idb */  }
        setupIluWPBaseIluOInt();
    } //constructor
    
    
    /** 
     * Reserved for LSR to generate new instances.
     * Instance is not yet created all the way.
     */
    /*friendly*/ final IluOInt allocateOI() {
        //RT will fill in fields !
        return new IluOInt();
    } //allocateOI
    
    
    /**
     * Returns the object of this IluOInt.
     * Finishes allocation if necessary     
     */
    public final java.lang.Object getTheObject() {
        java.lang.Object obj = this.jjJavaObject;
        if (obj == null) {
            //if the object exists it can be true or surrogate
            //but we only create surrogates here
            this.createSurrogateForOI();
            obj = this.jjJavaObject;
        }
        return obj;
    } //getTheObject
    
    
    //tables to temporarily prevent gc
    private static java.util.Hashtable interestTable = 
        new java.util.Hashtable(); 
    
    //Finalize real work on harmless thread
    private static IluOIntFinalizer finThread = new IluOIntFinalizer();


    /** 
     * To prevent garbage collection of true object 
     */
    public static
    void retain(java.lang.Object realObject) {
        IluOInt oi = peek(realObject);
        if (oi == null ) {throw new org.omg.CORBA.UNKNOWN("can't retain");}
        oi.retainOI();
    } //retain
    
    
    /*
     * To prevent garbage collection of true object.
     * Object will not be garbage collected as long as server is alive.  
     */
    /*friendly*/ void retainOI() {
        if (this.destroyed) {
            throw new org.omg.CORBA.INV_OBJREF("destroyed; too late to retain");
        }
        xerox.ilu.IluServer server = this.jjServer;
        if (server != null) {
            if (server.surrogateServer) {return;}
            /*$ idb */  if (traceGC > 2) {
            /*$ idb */      IluDebug.log.println("! IluOInt.retain " + this);
            /*$ idb */  }
            this.retained = true;
            server.retainTable.put(this, this);
        }
    } //retainOI
    
    
    /** 
     * Allow garbage collection of true object 
     */
    public static
    void expose(java.lang.Object realObject) {
        IluOInt oi = peek(realObject);
        if (oi != null) oi.exposeOI();
    } //expose
    
    
    /*
     * Allow garbage collection of true object 
     */
    /*friendly*/ void exposeOI() {
        xerox.ilu.IluServer server = this.jjServer;
        if (server != null) {
            if (server.surrogateServer) {return;}
            /*$ idb */  if (traceGC > 2) {
            /*$ idb */      IluDebug.log.println("! IluOInt.expose " + this);
            /*$ idb */  }
            this.retained = false;
            server.retainTable.remove(this);
        }
    } //exposeOI
    
    
    /* 
     * Called from from kernel when it starts being "very interested". 
     * Server lock is/must-be held
     */
    /*friendly*/ void showInterest() {
        /*$ idb */  if (traceGC > 0) {
        /*$ idb */      IluDebug.log.println("! IluOInt.showInterest " + this);
        /*$ idb */  }
        this.veryInterest = true;
        interestTable.put(this, this);
        if (! this.retained) this.ghost = true;
    } //showInterest
    
    
    /* 
     * Called from from kernel when it is no more "very interested". 
     * Server lock is/must-be held
     */
    /*friendly*/ void removeInterest() {
        /*$ idb */  if (traceGC > 0) {
        /*$ idb */      IluDebug.log.println("! IluOInt.remInterest " + this);
        /*$ idb */  }
        this.veryInterest = false;
        interestTable.remove(this);
    } //removeInterest
    
    
    /** 
     * Given a prospective real TRUE object, create the internal 
     * object. A real object may have at most one internal.
     *
     * Throws exception if object already known to ilu.
     */
    public static 
    IluOInt newOI(
                java.lang.Object realObject, 
                xerox.ilu.IluClassRep iluClassRep,
                xerox.ilu.IluServer server
                ) throws org.omg.CORBA.SystemException
    {
        if (realObject==null) {
            throw new org.omg.CORBA.BAD_PARAM("null object");
        }
        IluOInt oi = iluOIntProto.allocateOI();
        oi.jjClassRep = iluClassRep;
        oi.jjServer = server;
        oi.jjJavaObject = realObject;
        if (realObject instanceof org.omg.CORBA.portable.ObjectImpl) {
            org.omg.CORBA.portable.ObjectImpl impl = 
                (org.omg.CORBA.portable.ObjectImpl) realObject;
            org.omg.CORBA.portable.Delegate d;
            synchronized (realObject) {
                try {
                    d =impl._get_delegate();
                } catch (org.omg.CORBA.BAD_OPERATION o) {
                    //the delegate has not been set! GOOD!!!
                    d = iluOIntProto;
                }
                if ((d != iluOIntProto) && (d != null)) {
                    IluDebug.clientPreError("object already exists");
                    throw new org.omg.CORBA.BAD_PARAM(
                        "object already exists"
                        );
                }
                impl._set_delegate(oi);
            }
        } else if (realObject instanceof IluDataSupport) {
            IluDataSupport ids = (IluDataSupport) realObject;
            synchronized (realObject) {
                if (ids.getIluData() != null) {
                    IluDebug.clientPreError("object already exists");
                    throw new org.omg.CORBA.BAD_PARAM("object already exists");
                }
                ids.setIluData(oi);
            }
        } else {
            synchronized (globalTable) {
                if (globalTable.get(realObject) != null) {
                    IluDebug.clientPreError("object already exists");
                    throw new org.omg.CORBA.INTERNAL("object already exists");
                }
                globalTable.put(realObject, oi);
            }
        }
        /*$ idb */  if (traceObjects > 0) {
        /*$ idb */      IluDebug.log.println("! IluOInt oi: " + oi 
        /*$ idb */          + " obj: " + realObject);
        /*$ idb */  }
        return oi;
    } //newOI
    
    
    /* undo the effect of newOI in case native registration failed ... */
    /*friendly*/ void forgetOI() {
        java.lang.Object realObject = this.jjJavaObject;
        if (realObject != null) {
            globalTable.remove(realObject);
        }
    } //forgetOI
       
        
    private int proposeLifeTime(int proposal) {
        if (proposal == IluLifetimeArgs.iluLifetimeRemember) return proposal;
        if (proposal == IluLifetimeArgs.iluLifetimeForget) return proposal;
        java.lang.Object obj = this.jjJavaObject;
        if (obj != null) {
            if (obj instanceof IluLifetimeRemember) {
                return IluLifetimeArgs.iluLifetimeForget;
            }
            if (obj instanceof IluLifetimeForget) {
                return IluLifetimeArgs.iluLifetimeRemember;
            }
            if (! (obj instanceof IluDataSupport)) {
                if (! (obj instanceof org.omg.CORBA.portable.ObjectImpl)) {
                    return IluLifetimeArgs.iluLifetimeRemember;
                }
            }
        }
        if (this.jjClassRep != null) {
            if (this.jjClassRep.isCollectible()) {
                return IluLifetimeArgs.iluLifetimeForget;
            }
        }
        return IluLifetimeArgs.iluLifetimeRemember;
    } //proposeLifeTime
    
    
    /* true objects only */
    /*friendly*/ void setLifetime(int lifetime) {
        lifetime = proposeLifeTime(lifetime);
        switch (lifetime) {
            case IluLifetimeArgs.iluLifetimeRemember:
                this.retainOI();
                /*$ idb */  if (traceObjects > 0) {
                /*$ idb */      java.lang.Object obj = this.jjJavaObject;
                /*$ idb */      IluDebug.log.println("! ILU: reg/rem t-ob: " 
                /*$ idb */          + obj + " oi:" + this);
                /*$ idb */  }
                break;
            case IluLifetimeArgs.iluLifetimeForget:
                //...oi.exposeOI(); ..it never was retained
                /*$ idb */  if (traceObjects > 0) {
                /*$ idb */      java.lang.Object obj = this.jjJavaObject;
                /*$ idb */      IluDebug.log.println("! ILU: reg/expose t-ob:" 
                /*$ idb */          + obj + " oi:" + this);
                /*$ idb */  }
                break;
        }
    } //setLifetime
    
    
    /** 
     * Given a prospective real TRUE object, find an internal if it exists.
     * Returns null if none exists or existence can not be proved.
     *
     * (If caller is outside the server lock, the Ilu'ness of the returned
     * IluOInt can get lost, but the memory is safe).
     */
    public static IluOInt
    peek(java.lang.Object realObject) {
        /*$ idb */  if (traceCalls > IluDebug.basic) {
        /*$ idb */      IluDebug.log.println("! Enter IluOInt.peek, from " 
        /*$ idb */          + realObject);
        /*$ idb */  }
        if (realObject instanceof org.omg.CORBA.portable.ObjectImpl) {
            org.omg.CORBA.portable.ObjectImpl imp = 
                (org.omg.CORBA.portable.ObjectImpl) realObject;
            org.omg.CORBA.portable.Delegate d;
            try {
                d = imp._get_delegate();
            } catch (org.omg.CORBA.BAD_OPERATION o) {
                //the delegate has not yet been set!
                //this could be ok...
                /*$ idb */  if (traceCalls > IluDebug.detailed) {
                /*$ idb */      IluDebug.log.println("! IluOInt.peek no del."); 
                /*$ idb */  }
                return null;
            }
            if (d instanceof IluOInt) {
                if (d == iluOIntProto) {
                    /*$ idb */  if (traceCalls > IluDebug.detailed) {
                    /*$ idb */      IluDebug.log.println("! IluOInt.peek 00");
                    /*$ idb */  }
                    return null;
                }
                /*$ idb */  if (traceCalls > IluDebug.detailed) {
                /*$ idb */      IluDebug.log.println("! IluOInt.peek delegate >" 
                /*$ idb */          + d );
                /*$ idb */  }
                return (IluOInt) d;
            }
        }
        if (realObject instanceof IluDataSupport) {
            IluDataSupport ids = (IluDataSupport) realObject;
            java.lang.Object raw = ids.getIluData();
            /*$ idb */  if (traceCalls > IluDebug.detailed) {
            /*$ idb */      IluDebug.log.println("! Ex. IluOInt.peek, supp > " 
            /*$ idb */          + raw );
            /*$ idb */  }
            return (IluOInt) raw; 
        }
        IluOInt oi = (IluOInt) globalTable.get(realObject);
        /*$ idb */  if (traceCalls > IluDebug.basic) {
        /*$ idb */      IluDebug.log.println("! Ex IluOInt.peek, tab > " + oi );
        /*$ idb */  }
        return oi;
    } //peek


    /** 
     * Called by LSR to finish generating new SURROGATE instances.
     * This is usefull for SURROGATE objects.
     * It can be called for true objects, but since jjJavaObject is 
     * not null there will be no object creation.
     */
    private void createSurrogateForOI() 
               throws org.omg.CORBA.SystemException {
        /*$ idb */  if (traceObjects > 1) {
        /*$ idb */      IluDebug.log.println("! IluOInt: ent createSurrFOI oi: " 
        /*$ idb */          + this + " obj: " + this.jjJavaObject);
        /*$ idb */  }
        synchronized (this) {
            if (this.jjJavaObject != null) {
                //corresponding real object has already been created 
                return;
            }
            java.lang.Object surr = null;
            if (this.jjClassRep.factory != null) {
               java.lang.Object stupidCast = this;
               surr = 
                   this.jjClassRep.factory.createSurrogateObject(stupidCast);
            } else if (this.jjClassRep.surrogateJClass != null) {
                try {
                    surr = 
                        this.jjClassRep.surrogateJClass.newInstance();
                } catch (IllegalAccessException e) {
                } catch (InstantiationException e) {
                }
            }
            if (surr == null) {
                IluDebug.clientPreError("failed creating surrogate");
                throw new org.omg.CORBA.UNKNOWN("failed creating surrogate");
            }
            this.jjJavaObject = surr;
            if (surr instanceof org.omg.CORBA.portable.ObjectImpl) {
                 ((org.omg.CORBA.portable.ObjectImpl)surr)._set_delegate(this);
            } else if (surr instanceof IluDataSupport) {
                 ((IluDataSupport)surr).setIluData(this);
            } else {
                synchronized (globalTable) {
                    if (globalTable.get(surr) != null) {
                        IluDebug.clientPreError("object already exists");
                        throw new org.omg.CORBA.UNKNOWN("object already exists");
                    }
                    globalTable.put(surr, this);
                }
            }
        }
        /*$ idb */  if (traceObjects > 1) {
        /*$ idb */      IluDebug.log.println("! IluOInt: done createSurFOI oi: " 
        /*$ idb */          + this + " obj: " + this.jjJavaObject);
        /*$ idb */  }
    } //createSurrogateForOI
    
    
    /** 
     * Start eventually forgetting object...
     * Not available to the general public by means of "protected final".
     */
    protected final void  
    finalize() throws java.lang.Throwable {
        /*$ idb */  if (traceObjects > 1) {
        /*$ idb */      IluDebug.log.println("! IluOInt: finalize " + this
        /*$ idb */          + " destr." + this.destroyed);
        /*$ idb */  }
        if (this.destroyed) {
            //we are not interested in this in anymore
            return;
        }
        //use separate thread so we won't lock up the real finalizer
        finThread.provide(this);
    } //finalize
    

    /** 
     * Report finalization to IluWPBase class...
     */
    /*friendly*/ final void
    superFinalize() throws java.lang.Throwable {
        finalizeIluOInt();
    }
    
    
    /** 
     * "Destroys object" if it is neither used or appears to be used.
     * Might or might not succeed.
     * Returns whether there is a need to re-finalize
     */ 
    /*friendly*/ native boolean destroyGentleOI();
    
    
    /** 
     * "Destroys object" and ignores whether the application keeps using it.
     * Further use of the object might raise appropriate errors in the 
     * application. The kernel however will keep its piece of the object 
     * until it is safe. You get what you deserve.
     */ 
    /*friendly*/ void destroyRudeOI() {
         java.lang.Object ob0 = this.jjJavaObject;
         if (ob0 != null) {
             if (!(ob0 instanceof org.omg.CORBA.portable.ObjectImpl)) {
                 if (!(ob0 instanceof IluDataSupport)) {
                     synchronized(globalTable) {globalTable.remove(ob0);}
                 }
             }
         }
         this.exposeOI();
         this.nDestroyRudeOI();
    } //destroyRudeOI
    
    private native void nDestroyRudeOI();

    
    /** 
     * Publishes the SBH of the object in the local object domain
     */
    /*friendly*/ native void publishOI();
    

    /** 
     * withdrawOI is undo of publishOI
     */
    /*friendly*/ native void withdrawOI();
    
    
    /** 
     * See comments in IluRT0.sbhOfObject
     */
    /*friendly*/ native java.lang.String sbhOfOI();
    

    /** 
     * Returns OMG IIOP-specified IOR string for object.
     * May return null if object is not exported through IIOP.
     */
    /*friendly*/ native java.lang.String iorOfOI();
    
    
    /** 
     * See comments in IluRT0.ping
     */
    /*friendly*/ native void nativePingOI();
    
    
    /** 
     * See comments in IluRT0.URLOfObject
     */
    /*friendly*/ native java.lang.String nativeURLOfObject();
    
    
    public static int longNames = 1;
    
    /** 
     * Standard procedure overridden to include ilu kernel obj.
     */
    public java.lang.String toString() {
        java.lang.Object ob = this.jjJavaObject;
        java.lang.String s =
            "ilu-oi@" + java.lang.Integer.toHexString(this.hashCode()) + "<" 
            + java.lang.Long.toHexString(this.yIluKernelObject) + ">";
        if ((ob != null) && (longNames > 0)) {
            //don't use ob.toString() since some object 
            //might recurse and print IluOInt...
            s = s + "(" + java.lang.Integer.toHexString(ob.hashCode()) +")";
        }
        return s;
    } //toString
    
    
    /** 
     * Used by ilu native implementation in architectures where I had 
     * troubles implementing this functionality on the native side.
     * This is conceptionally private; the api may change whenever
     * native code feels a need  
     */
    public void throwThisException(
        int code,
        java.lang.String x, 
        java.lang.String arg) throws java.lang.RuntimeException
    {
        switch(code) {
            case 20: {
                //depricated
                //
                //this case is reserved for corba exceptions
                //fall through to case 0 as long as it is not
                //further specified
            }
            case 0: {
                //depricated
                //
                java.lang.Class ec;
                java.lang.RuntimeException thex;
                if (x != null) {
                   x = x.replace('/', '.');
                   x = x.replace('\\', '.');
                }
                try {
                    ec = java.lang.Class.forName(x);
                    thex = (java.lang.RuntimeException) ec.newInstance();
                } catch (java.lang.ClassNotFoundException ex) {
                    throw new org.omg.CORBA.INTERNAL("bad exception 1 [" + x + "][" + ex + "]");
                } catch (java.lang.InstantiationException ex) {
                    throw new org.omg.CORBA.INTERNAL("bad exception 2 " + x);
                } catch (java.lang.IllegalAccessException ex) {
                    throw new org.omg.CORBA.INTERNAL("bad exception 3 " + x);
                }
                //no more possible
                //if (arg != null) {
                //    if (thex instanceof IluSystemExceptionBase) {
                //        ((IluSystemExceptionBase) thex).setString(arg);
                //    }
                //}
                if (arg != null) {
                    if (arg.length()>0) {
                        System.err.println("** about to throw exception "
                            + x + arg
                            );
                    }
                }
                throw thex;
            }
            
            //specific primordially loaded exceptions
            case 1: throw new java.lang.NullPointerException(arg);
            case 2: throw new java.lang.OutOfMemoryError(arg);
            
            //specific ilu exceptions
            case 10: throw new org.omg.CORBA.INTERNAL(arg);
            case 12: throw new xerox.ilu.IluInconsistentCallException(arg); //ok
            case 13: throw new xerox.ilu.IluNotConfiguredException(arg); //ok
            case 14: throw new org.omg.CORBA.UNKNOWN(arg);//surrogate class not registered
            case 15: throw new org.omg.CORBA.UNKNOWN(arg);//ilu-class missmatch
            case 16: throw new org.omg.CORBA.UNKNOWN(arg);//ilu call exception
            case 17: throw new org.omg.CORBA.NO_IMPLEMENT(arg);//exception not implemented
            case 18: throw new org.omg.CORBA.UNKNOWN(arg);//not specified
            case 19: throw new org.omg.CORBA.UNKNOWN(arg);//other case
            
            //case 20 special case
            
            //generic corba exceptions as reported by the ilu kernel 
            case 21: throw new org.omg.CORBA.UNKNOWN(arg);
            case 22: throw new org.omg.CORBA.BAD_PARAM(arg);
            case 23: throw new org.omg.CORBA.NO_MEMORY(arg);
            case 24: throw new org.omg.CORBA.IMP_LIMIT(arg);
            case 25: throw new org.omg.CORBA.COMM_FAILURE(arg);
            case 26: throw new org.omg.CORBA.INV_OBJREF(arg);
            case 27: throw new org.omg.CORBA.NO_PERMISSION(arg);
            case 28: throw new org.omg.CORBA.INTERNAL(arg);
            case 29: throw new org.omg.CORBA.MARSHAL(arg);
            case 30: throw new org.omg.CORBA.INITIALIZE(arg);
            case 31: throw new org.omg.CORBA.NO_IMPLEMENT(arg);
            case 32: throw new org.omg.CORBA.BAD_TYPECODE(arg);
            case 33: throw new org.omg.CORBA.BAD_OPERATION(arg);
            case 34: throw new org.omg.CORBA.NO_RESOURCES(arg);
            case 35: throw new org.omg.CORBA.NO_RESPONSE(arg);
            case 36: throw new org.omg.CORBA.PERSIST_STORE(arg);
            case 37: throw new org.omg.CORBA.BAD_INV_ORDER(arg);
            case 38: throw new org.omg.CORBA.TRANSIENT(arg);
            case 39: throw new org.omg.CORBA.FREE_MEM(arg);
            case 40: throw new org.omg.CORBA.INV_IDENT(arg);
            case 41: throw new org.omg.CORBA.INV_FLAG(arg);
            case 42: throw new org.omg.CORBA.INTF_REPOS(arg);
            case 43: throw new org.omg.CORBA.BAD_CONTEXT(arg);
            case 44: throw new org.omg.CORBA.OBJ_ADAPTER(arg);
            case 45: throw new org.omg.CORBA.DATA_CONVERSION(arg);
            
            //specific ilu exceptions which don't easily map to corba...
            case 50: throw new org.omg.CORBA.NO_RESOURCES(arg + " MaxCountExceeded");
            case 51: throw new org.omg.CORBA.INTF_REPOS(arg + " ProtocolAlreadyRegistered");
            case 52: throw new org.omg.CORBA.INTF_REPOS(arg + " TransportAlreadyRegistered");
            case 53: throw new org.omg.CORBA.INTF_REPOS(arg + " BadProtocolInfo");
            case 54: throw new xerox.ilu.IluGcRegFailedException(arg);
            case 55: throw new org.omg.CORBA.OBJECT_NOT_EXIST(arg);
            case 56: throw new org.omg.CORBA.INTERNAL(arg + " CantCondition");
            default: break;
        }
        throw new org.omg.CORBA.INTERNAL("bad exception code");
    } //throwThisException
    
    
    /**
     * This construct prevents any attempts of cloning.<p>
     * @see     java.lang.Cloneable
     */
    protected final java.lang.Object clone() 
        throws java.lang.CloneNotSupportedException
    {
        throw new java.lang.CloneNotSupportedException();
    } //clone


    /**
     * Required because CORBA.portable.Delegate has
     * an abstract method
     * REMOVED SINCE JDK1.2-RC2
     *
     * public org.omg.CORBA.ImplementationDef 
     *         get_implementation(org.omg.CORBA.Object obj)
     * {
     *     throw new org.omg.CORBA.NO_IMPLEMENT();
     * } //get_implementation
     */


    /**
     * Required because CORBA.portable.Delegate has
     * an abstract method
     * REMOVED SINCE JDK1.2-RC2
     * 
     * public org.omg.CORBA.InterfaceDef 
     *         get_interface(org.omg.CORBA.Object obj) {
     *     throw new org.omg.CORBA.NO_IMPLEMENT();
     * } //get_interface
     */
    
    
    /**
     * Required because CORBA.portable.Delegate has
     * an abstract method
     */
    public org.omg.CORBA.Object duplicate(org.omg.CORBA.Object obj) {
        return obj;
    } //duplicate


    /**
     * Required because CORBA.portable.Delegate has
     * an abstract method
     */
    public void release(org.omg.CORBA.Object obj) {
    } //release
    
    
    /**
     * Required because CORBA.portable.Delegate has
     * an abstract method
     */
    public boolean is_a(
            org.omg.CORBA.Object obj, 
            java.lang.String repository_id) {
        throw new xerox.ilu.IluSystemExceptionBase("not impl");;
    } //is_a
    
    
    /**
     * Required because CORBA.portable.Delegate has
     * an abstract method
     */
    public boolean non_existent(org.omg.CORBA.Object obj) {
        if (obj != jjJavaObject) {return true;}
        try {
            IluRT0.ping(obj);
            return false;
        } catch (org.omg.CORBA.SystemException se) {
            return true;
        }
    } //non_existent
    

    /**
     * Required because CORBA.portable.Delegate has
     * an abstract method
     */
    public boolean is_equivalent(
            org.omg.CORBA.Object obj, 
            org.omg.CORBA.Object other) {
        return (obj == other);
    } //is_equivalent
    
    
    /**
     * Required because CORBA.portable.Delegate has
     * an abstract method
     */
    public int hash(org.omg.CORBA.Object obj, int max) {
        return 0;
    } //hash
    

    /**
     * Required because CORBA.portable.Delegate has
     * an abstract method
     */
    public org.omg.CORBA.Request request(
            org.omg.CORBA.Object obj, java.lang.String operation) {
        throw new org.omg.CORBA.NO_IMPLEMENT();
    } //request
    
    
    /**
     * Required because CORBA.portable.Delegate has
     * an abstract method
     */
    public org.omg.CORBA.Request create_request(
            org.omg.CORBA.Object obj,
            org.omg.CORBA.Context ctx,
            java.lang.String operation,
            org.omg.CORBA.NVList arg_list,
            org.omg.CORBA.NamedValue result) {
        throw new org.omg.CORBA.NO_IMPLEMENT();
    } //create_request


    /**
     * Required because CORBA.portable.Delegate has
     * an abstract method
     */
    public org.omg.CORBA.Request create_request(
            org.omg.CORBA.Object obj,
            org.omg.CORBA.Context ctx,
            java.lang.String operation,
            org.omg.CORBA.NVList arg_list,
            org.omg.CORBA.NamedValue result,
            org.omg.CORBA.ExceptionList exclist,
            org.omg.CORBA.ContextList ctxlist) {
        throw new org.omg.CORBA.NO_IMPLEMENT();
    } //create_request
    
    /**
     * Required because CORBA.portable.Delegate provides
     * this method 
     */
    public org.omg.CORBA.ORB orb(org.omg.CORBA.Object obj) {
        return IluORB.iluOrb(obj);
    } //orb
    

} //IluOInt



/*friendly*/ final class IluOIntFinalizer 
    extends xerox.basics.Consumer0 {

    private static int count = 0; // used to provide unique thread name
    private static int traceObjects = IluDebug.traceObjects();
    private static IluFinalizeReporter reporter = 
        new IluFinalizeReporter();

    /* returns and increments the classes object count */
    private static final synchronized long incrementCount() {
        return count++;
    }

    /*friendly*/ IluOIntFinalizer() {
        super(IluEnvironment.threadPriority(), "IluOIntFinalizer" 
            + incrementCount());
    } //IluOIntFinalizer
    
    protected void consume(java.lang.Object obj) {
        //take object from finalization queue 
        //and decide whether to finalize it or not (yet) 
        IluOInt oi = (IluOInt) obj;
        /*$ idb */  if (traceObjects > 0) {
        /*$ idb */      IluDebug.log.println("! IluOIntFin: consume " 
        /*$ idb */          + oi + " (" + oi.jjJavaObject + ")"
        /*$ idb */          );
        /*$ idb */  }
        if (oi.destroyed) {
            //do nothing; the ilu object has already been destroyed
            /*$ idb */  if (traceObjects > 0) {
            /*$ idb */      IluDebug.log.println("! IluOIntFin: already dead"  
            /*$ idb */          + oi);
            /*$ idb */  }
        } else if (oi.ghost) {
            //de-ghost object
            /*$ idb */  if (traceObjects > 0) {
            /*$ idb */      IluDebug.log.println("! IluOIntFin: de-ghost " 
            /*$ idb */          + oi);
            /*$ idb */  }
            oi.ghost = false;
            IluReEnableFinalization.reEnable(oi);
        } else {
            //try to destroy
            boolean need_to_re_finalize = oi.destroyGentleOI();
            if (need_to_re_finalize) {
                //destroy failed; try again later
                /*$ idb */  if (traceObjects > 0) {
                /*$ idb */      IluDebug.log.println("! IluOIntFin: try later " 
                /*$ idb */          + oi);
                /*$ idb */  }
                oi.ghost = false;
                IluReEnableFinalization.reEnable(oi);
            } else {
                //destroy succeeded; optionally report it to client
                /*$ idb */  if (traceObjects > 0) {
                /*$ idb */      IluDebug.log.println("! IluOIntFin: done " 
                /*$ idb */          + oi);
                /*$ idb */  }
                java.lang.Object robj = oi.jjJavaObject;
                if (robj instanceof xerox.ilu.IluServantFinalizable) {
                    reporter.provide(robj);
                }
                //handle deferred IluWPBase finalization
                try {
                    oi.superFinalize();
                } catch (java.lang.Throwable t) {
                }
            }
        } 
    } //consume 

} //IluOIntFinalizer



/*friendly*/ final class IluFinalizeReporter 
    extends xerox.basics.Consumer0 {

    private static int count = 0; // used to provide unique thread name
    private static int traceObjects = IluDebug.traceObjects();

    /* returns and increments the classes object count */
    private static final synchronized long incrementCount() {
        return count++;
    }

    /*friendly*/ IluFinalizeReporter() {
        super(IluEnvironment.threadPriority(), "IluFinalizeReporter" 
            + incrementCount());
    } //IluFinalizeReporter
    
    protected void consume(java.lang.Object obj) {
        /*$ idb */  if (traceObjects > 1) {
        /*$ idb */      IluDebug.log.println("! IluFinRep: consume " + obj);
        /*$ idb */  }
        xerox.ilu.IluServantFinalizable fobj =
            (xerox.ilu.IluServantFinalizable) obj;
        try {
            fobj.iluServantFinalize(); 
        } catch (java.lang.Throwable t) {
        }
    } //consume

} //IluFinalizeReporter



/*friendly*/ final 
class IluReEnableFinalization {
    
    private IluOInt theOI = null;
    private static int traceObjects = IluDebug.traceObjects();

    /*friends*/ static void reEnable(IluOInt oi) {
        new IluReEnableFinalization(oi);
    } //reEnable
    
    /*friends*/ IluReEnableFinalization(IluOInt oi) {
        theOI = oi;
        IluReEnableFinalization old = oi.reFinalizer;
        if (old != null) old.theOI = null;
        oi.reFinalizer = this;
        /*$ idb */  if (traceObjects > 0) {
        /*$ idb */      IluDebug.log.println("! IluReEnableFin reenable " + oi);
        /*$ idb */  }
    } //IluReEnableFinalization
    
    protected void finalize() throws Throwable {
        IluOInt oi = theOI;
        /*$ idb */  if (traceObjects > 0) {
        /*$ idb */      IluDebug.log.println("! IluReEnableFin finalize " + oi);
        /*$ idb */  }
        if (oi != null) oi.finalize();
    } //finalize
    
} //IluReEnableFinalization
