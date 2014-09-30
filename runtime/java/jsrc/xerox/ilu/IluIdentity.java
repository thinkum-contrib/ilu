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
/* IluIdentity.java */
/* Chris Jacobi, December 23, 1998 12:07 pm PST */
/* 
 * $Id: IluIdentity.java,v 1.9 1999/08/03 01:53:48 janssen Exp $ 
 */


/* 
 * Representation for ilu_IdentityInfo <p>
 * See IluJava_IluPort.c
 */


package xerox.ilu;

/**
 * Client visible.<p>
 *
 * Warning: Different IluIdentity objects might refer to the same
 * identity. <p>
 *
 * In theory we would like an immutable IluIdentity class which is 
 * subclassed for different IluIdentityType's. However since we need 
 * to allocate the real data on the native side after we present a 
 * container, we can not use the type mechanism for IluIdentity but 
 * must delegate message dispatch to its "it" field.<p>
 *
 * Creation of IluIdentity happens in two cases, both limited to 
 * "friends".  
 * 
 * - IluIdentity-s can be explicitely created explicitely using 
 * the IluIdentityType specific method for creating IluIdentity-s 
 * of this "type".
 * 
 * - IluIdentity-s are "read" from the wire using the IluPassport.
 *
 * @see IluIdentityType
 * @see IluPassport
 */
public final class IluIdentity extends IluWPBase {
    /*friendly*/ long yIluIdentity = 0;
    /*friendly*/ IluIdentityType it = null;
    private java.lang.String asString = null;
    /*friendly*/ IluPassport owner = null; //to prevent garbage collection
    /*friendly*/ java.lang.Object dontGC = null; //to prevent garbage collection
    
    /**
     * Disable untrusted creations
     */
    /*friendly*/ IluIdentity() {
    } //constructor

    
    /**
     * Creation of IluIdentity is for friends only; Applications can
     * not create IluIdentity-s directly.
     * (This creates a shell only, the caller must fill-in the real info
     * according to the requirements of the IluIdentityType)
     *
     * @see IluIdentityType
     * @see IluIdentityType.createIluIdentity
     * @see IluPassport.findIdentity
     */
    /*friendly*/ IluIdentity(IluIdentityType it) {
        this.it = it;
    } //constructor


    /**
     * Primitive used by IluIdentityType
     */
    /*friendly*/ native java.lang.String nId2String();
    
    private native void nFinalizeIdentity();
    
    /**
     * Not available to the general public by means of "protected final".
     */
    protected final void finalize() throws java.lang.Throwable {
        nFinalizeIdentity();	//return native memory
        super.finalize();	//IluWPBase requirement 
    } //finalize

    /**
     * Returns string form representation of IluIdentity.
     * This is much more elaborate then simple using the toString method.
     */
    public java.lang.String stringForm() {
       if (this.asString == null) {
           //use "it" to dispatch since identities are allocated
           //before type is known and can't be subclassed.
           java.lang.String s = this.it.identityToStringPart(this);
           if (this.asString == null) {this.asString = s;}
       }
       return this.asString;
    } //stringForm
    
} //IluIdentity
