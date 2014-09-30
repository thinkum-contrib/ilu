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
/* IluGssCredId.java */
/* Chris Jacobi, January 14, 1999 2:39 pm PST */
/* 
 * $Id: IluGssCredId.java,v 1.14 1999/08/03 01:54:05 janssen Exp $ 
 */


/* 
 * Representation for gssapi::gss_cred_id_t <p>
 * See IluJava_IluPort.c
 */


package xerox.ilu;

/**
 * Credentials for secure transports.
 *
 * WARNING: This class is only implemented if ilu is configured
 * enabling secure transports. Check using xerox.ilu.IluRT0.hasSecurity().
 *
 * @see IluIdentity
 * @see IluIdentityType
 * @see IluPassport
 */
public final class IluGssCredId extends IluWPBase {
    
    /*friendly*/ long yIluGssCredId = 0; //gssapi::gss_cred_id_t
    private int state = 0;
    
    static {
        IluInit.init();
    } //static
    
    /**
     * Disables unauthorized creation
     */
    private IluGssCredId() {
    } //constructor
    
    /*friendly*/ native void nIluGssCredIdAcquire(
        java.lang.String principalName, 
        int lifetimeSecs,
        IluGssOid mechanism,
        boolean acceptOnly) throws org.omg.CORBA.SystemException;
    
    /*friendly*/ native void nIluGssCredIdFree();
    
    /**
     * Constant value which can be used to specify
     * an indefinite lifetime in acquireForName
     */
    public static final int GSS_INDEFINITE = -1;
    
    /**
     * Acquires a IluGssCredId.
     * [in ilu_kerneleese: AcquireGSSIdentity
     */
    public static IluGssCredId acquireForName(
            java.lang.String principalName, 
            int lifetimeSecs,
            IluGssOid mechanism,
            boolean acceptOnly) throws org.omg.CORBA.SystemException
    {
        if (principalName == null) {
            throw new org.omg.CORBA.NO_PERMISSION("no principal");
        }
        if (mechanism == null) {
            throw new org.omg.CORBA.NO_PERMISSION("no mechanism");
        }
        IluGssCredId ci = new IluGssCredId();
        ci.nIluGssCredIdAcquire(
            principalName, lifetimeSecs, mechanism, acceptOnly
            );
        ci.state = 1; 
        return ci;
    } //acquireForName
    
    
    /*friendly*/ native void nIluGssCredIdToIdentity(IluIdentity id);
    
    /**
     * Creates an IluIdentity.
     * [in ilu_kerneleese: ilu_AcquireGSSIdentity
     */
    public IluIdentity toIdentity() {
        IluIdentity id = null;
        synchronized (this) {
            if (state != 1) {
                throw new org.omg.CORBA.FREE_MEM("use of freed credentials");
            }
            id = new IluIdentity(
                IluIdentityType.getGSSIdentityType()
                );
            nIluGssCredIdToIdentity(id);
            id.dontGC = this; //prevent garbage collection
        }
        return id;
    } //toIdentity


    /**
     * Returns the principal name of this (GSS) IluIdentity.<br>
     * idt: must be of type gssIdentity <br>
     * mechanism: optional
     */
    public static native java.lang.String 
    decodeName(IluIdentity idt, IluGssOid mechanism);
    
    
    /**
     * Returns whether this (GSS) IluIdentity is local. 
     * idt: must be of type gssIdentity <br>
     * mechanism: optional 
     */
    public static native boolean 
    decodeIsLocal(IluIdentity idt, IluGssOid mechanism);
    
    
    /**
     * Returns connection flags if (GSS) IluIdentity. 
     * idt: must be of type gssIdentity <br>
     * mechanism: optional 
     */
    public static native int 
    decodeFlags(IluIdentity idt, IluGssOid mechanism);
    
    
    /**
     * Returns good-till value of (GSS) IluIdentity. 
     * Measured in seconds since "ilu's origin" <br>
     * idt: must be of type gssIdentity <br>
     * mechanism: optional <br>
     */
    public static native int 
    decodeGoodTill(IluIdentity idt, IluGssOid mechanism);
    
    
    /**
     * Returns "current time" using same units and base as decodeGoodTill.
     * (This is ilu's time in seconds).
     */
    public static native int now();
    
    
    /**
     * Explicitely null-out any accumulated credentials
     * acquired
     */
    public void free() {
        int oldstate = 0;
        synchronized(this) {
            oldstate = this.state;
            this.state = 0;
        }
        if (oldstate==1) {
            this.nIluGssCredIdFree();
        }
    } //free
    
    protected final void finalize () throws java.lang.Throwable {
        if (this.state == 1) {
            this.state = 0;
            this.nIluGssCredIdFree();	//frees native data
        }	
        super.finalize();		//IluWPBase requirement 
    } //finalize
    
    
} //IluGssCredId


