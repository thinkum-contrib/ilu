/* Ilu.java */
/* Chris Jacobi, January 6, 1999 4:54 pm PST */
/* $Id: Ilu.java,v 1.31 1999/08/03 01:53:59 janssen Exp $ */

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
 
package xerox.ilu;

/**
 * Main (static) application interface for ILU.<p>
 *
 * The javadoc documentation is only supplemental
 * to the regular ILU documentation.   The way javadoc
 * formats its output, reading the java sources might
 * be preferable to reading javadoc documentation.
 */
public final class Ilu {
        
    /**
     * Don't create instances 
     */
    private Ilu(){};
    
    static {
       init();
    }

    /** 
     * Importing an object <p>
     * The iluClass argument could be a superclass if you
     * are sure that the proper class has been loaded. If a
     * superclass is used, it should have matching collectability. <p>
     * 
     * @see xerox.ilu.IluSBH  
     */
    public static java.lang.Object 
    objectFromSBH (
            java.lang.String sbh, 
            IluClassRep iluClass
            ) throws org.omg.CORBA.SystemException {
        return IluRT0.objectFromSBH(sbh, iluClass);
    } //objectFromSBH

    /** 
     * Literals for the lifetime argument of registerTrueObject.
     */
    
    /** Requires ilu set a value depending on other arguments */
    public static final int unspec = IluLifetimeArgs.iluLifetimeUnspec;
    
    /** Remember obj to prevent java garbage collection. */
    public static final int remember = IluLifetimeArgs.iluLifetimeRemember;
    
    /** Forget obj to subject it to java garbage collection. */
    public static final int forget = IluLifetimeArgs.iluLifetimeForget;

    /**
     * Create true objects. <p>
     * Life time is important and must be determined.  Normally the ilu
     * object stays alive until the intermediate object is garbage
     * collected.  Use any of a large number of schemes to prevent
     * garbage collection. Setting a non-zero lifetime will
     * override marker interfaces like IluLifetimeForget or
     * IluLifetimeRemember.  <p>
     * 
     * Return value is intermediate object.  Caller may prevent this
     * from being garbage collected. <p>
     *
     * @see IluLifetimeArgs
     * @see IluIHProposer
     * @see IluLifetimeForget
     * @see IluLifetimeRemember
     *
     */
    public static java.lang.Object 
    registerTrueObject(
            java.lang.String ih,
            java.lang.Object tobj, 
            IluServer server, 
            IluClassRep iluClass,
            int lifetime //0 gives default behaviour
            ) throws org.omg.CORBA.SystemException {
        return IluRT0.registerTrueObject(ih, tobj, server, iluClass, lifetime);
    } //registerTrueObject
    
    
    /** 
     * Returns the string binding handle.<br>
     * May return NIL if the object's server isn't exported through any
     * port; may return an invalid SBH if the cached one references a
     * closed port.
     */
    public static java.lang.String 
    sbhOfObject(java.lang.Object obj) {
        return IluRT0.sbhOfObject(obj);
    } //sbhOfObject
    
    
    /** 
     * Returns OMG IIOP-specified IOR string for object. <br>
     * May return null if object is not exported through IIOP.
     */
    public static java.lang.String 
    iorOfObject(java.lang.Object obj) {
        return IluRT0.iorOfObject(obj);
    } //iorOfObject
  
  
    /** 
     * Destroy ILU-ness of true object.<p>
     *
     * Only for impatient applications,
     * Normal applications leave this to the garbage collector. <br>
     * Do NOT use from applets; error behavior is not yet defined. <br>
     * Do not use on surrogates.
     */
    public static void 
    destroyObject(java.lang.Object obj) {
        IluRT0.destroyObject(obj);
    } //destroyObject
    
    
    /**
     * Generates a string that's unique over space and time.
     * A server with nothing better to use might call this to 
     * get an ID.
     */
    public static java.lang.String 
    inventID() {
        return IluRT0.inventID();
    } //inventID
    
    
    /**
     * Test whether object is alive.
     */
    public static void ping(java.lang.Object obj) {
        IluRT0.ping(obj);
    } //ping    

    /**
     * Makes sure Ilu is loaded.<p>
     * Idempotent.
     */
    public static void init() {
        IluInit.init();
    } //init

} //Ilu
