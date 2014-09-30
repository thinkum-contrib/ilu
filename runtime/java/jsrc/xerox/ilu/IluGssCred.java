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
/* IluGssCred.java */
/* Chris Jacobi, December 23, 1998 12:06 pm PST */
/* 
 * $Id: IluGssCred.java,v 1.12 1999/08/03 01:53:51 janssen Exp $ 
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
public final class IluGssCred extends IluWPBase {
    
    /*friendly*/ long yIluGssCred = 0; //gssapi::gss_cred_id_t
    private int state = 0;
    private IluIdentity idt = null;
    
    static {
        IluInit.init();
    } //static
    
    /**
     * Disables unauthorized creation
     */
    private IluGssCred() {
    } //constructor
    
    /*friendly*/ native void nIluGssCredAcquire(
        java.lang.String principalName, 
        int lifetimeSecs,
        IluGssOid mechanism,
        boolean acceptOnly) throws org.omg.CORBA.SystemException;
    
    /*friendly*/ native void nIluGssCredFree();
    
    /**
     * Constant value which can be used to specify
     * an indefinite lifetime in acquireForName
     */
    public static final int GSS_INDEFINITE = -1;
    
    /**
     * Acquires a IluGssCred.
     * [in ilu_kerneleese: AcquireGSSIdentity
     */
    public static IluGssCred acquireForName(
            java.lang.String principalName, 
            int lifetimeSecs,
            IluGssOid mechanism,
            boolean acceptOnly) throws org.omg.CORBA.SystemException
    {
        if (principalName == null) {
            throw new org.omg.CORBA.BAD_OPERATION("no principal");
        }
        if (mechanism == null) {
            throw new org.omg.CORBA.BAD_OPERATION("no mechanism");
        }
        IluGssCred ci = new IluGssCred();
        ci.nIluGssCredAcquire(
            principalName, lifetimeSecs, mechanism, acceptOnly
            );
        ci.state = 1; 
        //
        //for reasons of native memory management of gss_cred_id we 
        //always create an identity.
        ci.idt = ci.real_toIdentity();
        //
        return ci;
    } //acquireForName
    
    
    /*friendly*/ native void nIluGssCredIdToIdentity(IluIdentity id);
    
    /**
     * Creates an IluIdentity.
     * [in ilu_kerneleese: ilu_AcquireGSSIdentity]
     */
    private IluIdentity real_toIdentity() {
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
    } //real_toIdentity


    /**
     * Creates an IluIdentity.
     * [in ilu_kerneleese: ilu_AcquireGSSIdentity]
     */
    public IluIdentity toIdentity() {
        return this.idt;
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
            this.nIluGssCredFree();
        }
    } //free
    
    /**
     * Not available to the general public by means of "protected final".
     */
    protected final void finalize() throws java.lang.Throwable {
        if (this.state == 1) {
            this.state = 0;
            this.nIluGssCredFree();	//frees native data
        }	
        super.finalize();		//IluWPBase requirement 
    } //finalize
    
    
} //IluGssCred


