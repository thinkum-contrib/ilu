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
/* IluWPBase.java */
/* Chris Jacobi, January 14, 1999 3:45 pm PST */
/* $Id: IluWPBase.java,v 1.14 1999/08/03 01:53:39 janssen Exp $ */
 

package xerox.ilu;

/**
 * Ilu internal superclass; Not public at all...<p>
 *
 * Base super class for ilu classes which need to carry
 * a weak pointer on the native side.  <p>
 *
 * Security note: all constructors are guarded:
 * either by making them friendly, or by requiring a key.
 * It is assumed that Ilu will only pass the key to other
 * priviledged packages, like org.omg.CORBA.<p>
 * (reason: errors with finalization take the risk of
 * a native memory smash [and security hole]) <p>
 *
 * Implementor note:  This class must not initialize ilu:
 * On Borlands JBuilder 2 ilu crashes dismally because of initialization
 * order problems, if this class recurses in ilu's initializations.
 * Seems not be a problem with all other Java VM's I have tried.
 */
public class IluWPBase {

    /**
     * Key could be passed to priviledged applications. 
     */
    /*friendly*/ static java.lang.Object staticKey = new java.lang.Object();
    
    /**
     * Key Storage space for weak pointer on native side.  This
     * field is not accessed from the java side.
     * The kind of weak pointer used may depend on the subclass. 
     */
    private transient long ywpx = 0; 
    
    
    /**
     * Native setup. 
     */
    private native void setupIluWPBase();
     

    /** unusual name to avoid conflict with subclasses */
    /*friendly*/ final static void init0() {
    } //init0
    
    
    /**
     * Constructor not available to the general public
     */
    /*friendly*/ IluWPBase() {
        this.setupIluWPBase();
    } //constructor
    
    
    /**
     * Constructor may be made available to the general public, but 
     * needs a matching key, so only priviledged applications having
     * key can construct such objects. 
     */
    protected IluWPBase(java.lang.Object key) {
        if (key != staticKey) {
            throw new java.lang.SecurityException("don't subclass IluWPBase"); 
        }
        this.setupIluWPBase();
    } //constructor
    
        
    /**
     * MUST NOT be made available to the general public. <p>
     * Subclass overriding this MUST call this on finalization
     * and nowhere else...
     */
    protected native void finalize() throws java.lang.Throwable;


    /**
     * This construct prevents subclasses from implementing cloning.<p>
     * @see     java.lang.Cloneable
     */
    protected final java.lang.Object clone() 
        throws java.lang.CloneNotSupportedException
    {
        throw new java.lang.CloneNotSupportedException();
    } //clone
    

} //IluWPBase
