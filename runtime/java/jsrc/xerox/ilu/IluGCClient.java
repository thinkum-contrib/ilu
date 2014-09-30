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
/* IluGCClient.java */
/* Chris Jacobi, December 23, 1998 12:05 pm PST */
/* $Id: IluGCClient.java,v 1.29 1999/08/03 01:54:12 janssen Exp $ */
 
/* 
 * Private ilu internal package.
 * Native code in IluJava_GCClient.c
 */

package xerox.ilu;

/** 
 * Special class used by ilu java runtime itself to implement 
 * collectable objects.  Nothing is client accessible, but
 * clients should have access to the source code for debugging
 * reasons.
 */
 
/* friendly */ final 
class IluGCClient extends IluObjectBase 
        implements IluDataSupport {

    /** No random instances... */
    private IluGCClient() {
    } //constructor
    
    
    private static boolean isInit = false;
    private static IluServer server;
    private static IluClassRep classRep;
    private static IluGCClient gcObj;
    
    
    private final static synchronized boolean initialized(){
        if (isInit) return true;
        isInit = true;
        return false;
    } //initialized
    
    
    /** Used for implementing IluDataSupport */
    private java.lang.Object anchor = null;

    
    /** Implements IluDataSupport interface */
    public void setIluData(java.lang.Object internal) {
        if (anchor == null) anchor = internal;
    } //setIluData
    
    
    /** Implements IluDataSupport interface */
    public java.lang.Object getIluData() {
        return anchor;
    } //getIluData
    
    
    /*friendly*/  final static void init() {
    } //init
    
    
    /** Initialized on need only */
    /*friendly*/ static void initPhaseX() {
        if (isInit) return;
        if (initialized()) return;
        if (IluInit.abortInitializations()) return;
        if (nativeIsGcClientSet()) {
            //some other LSR must have already set up the callback
            return;
        }
        try {
            java.lang.String specialCInfo = 
                IluEnvironment.getStringPropX("ILU_COLLECTABLE_CINFO");
            IluPassport passport = null;
            java.lang.Thread thread = java.lang.Thread.currentThread();
            if (thread instanceof IluServiceThread) {
                IluServiceThread st = (IluServiceThread) thread;
                passport = st.getInitiatingPassport();
            }
            server = IluServer.createServer(null);
            server.dontUseForDefault = true;
            server.setDeamonFlag(true);
            if (specialCInfo != null) {
                server.createParsedPorts(specialCInfo, passport);
            } else {
                server.createDefaultPorts(passport);
            }
            server.finishInitialization();
            classRep = IluClassRep.setupClass(
                "xerox.ilu.IluGCClient" , //bogus
            	"special_gc_class" , //bogus, 
            	null, //uuid
        	0 //method_count
        	);
            finishSpecialGCClass(classRep);
            gcObj = new IluGCClient();
            IluRT0.registerTrueObject(
                "special_gc_inst", 
                gcObj, 
                server, 
                classRep,
                IluLifetimeArgs.iluLifetimeRemember
                );
            gcObj.setupSpecialGCObj(IluOInt.peek(gcObj));
            xerox.basics.VMExtras.makeGCRoot(gcObj);
        } catch (java.lang.Exception e) {
            //This is sufficiently hard to debug that an extra error 
            //message reduces trafic on the ilu mailing list
            System.err.println(
                "****ILU failed initializing the special server object "
                + "used for handling distributed garbage collection.\n"
                + e.getMessage()
                + "\nSee xerox.ilu.IluGCClient.java for more information."
                );
                //
                //A possible source of such problems is the use of
                //secure transports which require passport information
                //whe the special server object is set up.
                //
                //This special server object object is allocated
                //whenever a collectable class is used, independent
                //whether the class is used for true or surrogate
                //objects.
                //
                //There is a property or environment variable which
                //allows you to specify the contact info for this
                //object separately.
                //
            if (e instanceof org.omg.CORBA.SystemException) {
                //re-throw the same exception
                throw (org.omg.CORBA.SystemException) e;
            } else {
                throw new org.omg.CORBA.INTERNAL(
                    "exception initializing xerox.ilu.IluGCClient" + e
                    );
            }
        }
    } //initPhaseX
    
    
    private static native boolean 
    nativeIsGcClientSet();
    
    private static native void 
    finishSpecialGCClass(IluClassRep classRep);
    
    private static native void 
    setupSpecialGCObj(IluOInt jjoi);
    
} // IluGCClient
