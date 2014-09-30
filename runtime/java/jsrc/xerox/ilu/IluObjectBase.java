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
/* IluObjectBase.java */
/* Chris Jacobi, January 18, 1999 7:33 pm PST */
/* $Id: IluObjectBase.java,v 1.23 1999/08/03 01:54:12 janssen Exp $ */
 

package xerox.ilu;

/**
 * Base super class used for surrogate object implementation.<p>
 * 
 * This might as well be used as base class for true objects, at the
 * discretion of the programmer (of the true object's class).<br>
 *
 * Actually, this is the PREFERRED method of implementing true
 * ilu objects, as the programmer gets the goodies for free.  
 * There are two alternative methods of implementing true
 * ilu object:  <br>
 *    a) Implement the IluDataSupport interface <br>
 *    b) Let the ilu server hang on permanently to the java object <br> 
 *       (preventing java garbage collection while the server is alive).<p>
 *
 * Class is only fake java.io.Serializable; depends on substitution! <p>
 *
 * @see IluDataSupport
 * @see org.omg.CORBA.portable.ObjectImpl
 *
 *
 */
public abstract class IluObjectBase 
        extends org.omg.CORBA.portable.ObjectImpl 
        implements org.omg.CORBA.Object, java.io.Serializable {

    /** 
     * Not public but for surrogate stubs.
     * Ilu private... 
     */
    protected IluObjectBase() {
        super();
        //
        //special convention
        //avoids throwing an exception when testing whether
        //the delegate is initialized
        this._set_delegate(IluOInt.iluOIntProto);
    } //constructor
    

    /**
     * Called automaticly on serialization.<p>
     * (Not corba compatible)
     * Works on JKD1.2 and newer only
     */
     public java.lang.Object writeReplace() 
         throws java.io.ObjectStreamException
     {
         IluSubstitute subs = new IluSubstitute();
         subs.sbh = xerox.ilu.IluRT0.sbhOfObject(this);
         return subs;
     } //writeReplace
    

    /**
     * This construct prevents subclasses from implementing cloning.<p>
     * (Not corba required)
     * @see     java.lang.Cloneable
     */
     protected final java.lang.Object clone() 
         throws java.lang.CloneNotSupportedException
     {
         throw new java.lang.CloneNotSupportedException();
     } //clone
    

    /** 
     * Used only because org.omg.CORBA.portable.ObjectImpl
     * has an abstract method. 
     */
    public java.lang.String[] _ids() {
        throw new org.omg.CORBA.NO_IMPLEMENT();
    } //_ids

    
/* start of EDITED template */
    // 
    // This is a template:  You should be able to put this */
    // into your code without looking at it.  You need this
    // when defining Ilu true types but you can't inherit 
    // from xerox.ilu.IluObjectBase. 
    // This implements xerox.ilu.IluDataSupport; your class 
    // definition should specify so if you whish to be able
    // to use garbage collection.
    // EXPECT THIS TEMPLATE TO BE REPLACED ON BOTH
    // ILU RELEASES AS WELL AS JDK RELEASES.
    // This version is tested with JDK-1.2-RC2
    // Chris Jacobi, November 23, 1998 11:43 am PST 
    //

//    /** re-implements Delegate; if necessary */
//    private transient org.omg.CORBA.portable.Delegate __delegate;

//    /** re-implements Delegate; if necessary */
//    public void _set_delegate(org.omg.CORBA.portable.Delegate arg) {
//        __delegate = arg;
//    } //_set_delegate

//    /** re-implements Delegate; if necessary */
//    public org.omg.CORBA.portable.Delegate _get_delegate() {
//        if (__delegate == null) {
//            throw new org.omg.CORBA.BAD_OPERATION(
//                "The delegate has not been set!"
//                );
//        }
//        return __delegate;
//    } //_get_delegate

//    /** required in case xerox.ilu.IluDataSupport is desired */
//    public void setIluData(java.lang.Object internal) {
//        __delegate = (org.omg.CORBA.portable.Delegate) internal
//    } //setIluData
    
//    /** required in case xerox.ilu.IluDataSupport is desired */
//    public java.lang.Object getIluData() {
//        return __delegate;
//    } //getIluData
    
    /** required by Ilu (actually stubs only). Ilu private. */
    public xerox.ilu.IluClassRep getIluClass() {
        xerox.ilu.IluOInt oint = (xerox.ilu.IluOInt) 
            this._get_delegate();
        return oint.jjClassRep;
    } //getIluClass
    
    /** required by org.omg.CORBA.Object */
    public boolean _is_a(java.lang.String repid) {
        return _get_delegate().is_a(this, repid);
    } //_is_a
    
    /** required by org.omg.CORBA.Object; impl is Ilu specific */
    public boolean _is_equivalent(org.omg.CORBA.Object other) {
        return  (other == this);
    } //_is_equivalent

    /** required by org.omg.CORBA.Object */
    public boolean _non_existent() {
        return _get_delegate().non_existent(this);
    } //_non_existent

    /** required by org.omg.CORBA.Object */
    public int _hash(int maximum) {
        return _get_delegate().hash(this, maximum);
    } //_hash

    /** required by org.omg.CORBA.Object */
    public org.omg.CORBA.Object _duplicate() {
        return _get_delegate().duplicate(this);
    } //_duplicate

    /** required by org.omg.CORBA.Object */
    public void _release() {
        _get_delegate().release(this);
    } //_release

    /** required by org.omg.CORBA.Object */
    public org.omg.CORBA.Object _get_interface_def() {
        throw new org.omg.CORBA.NO_IMPLEMENT();
    } //_get_interface_def

    /** required by org.omg.CORBA.Object */
    public org.omg.CORBA.Request _request(java.lang.String operation) {
        throw new org.omg.CORBA.NO_IMPLEMENT();
    } //_request

    /** required by org.omg.CORBA.Object */
    public org.omg.CORBA.Request _create_request(org.omg.CORBA.Context ctx,
            java.lang.String operation,
            org.omg.CORBA.NVList arg_list,
            org.omg.CORBA.NamedValue result) {
        throw new org.omg.CORBA.NO_IMPLEMENT();
    } //_create_request

    /** required by org.omg.CORBA.Object */
    public org.omg.CORBA.Request _create_request(org.omg.CORBA.Context ctx,
            java.lang.String operation,
            org.omg.CORBA.NVList arg_list,
            org.omg.CORBA.NamedValue result,
            org.omg.CORBA.ExceptionList exclist,
            org.omg.CORBA.ContextList ctxlist) {
        throw new org.omg.CORBA.NO_IMPLEMENT();
    } //_create_request

    /** required by org.omg.CORBA.Object */
    public org.omg.CORBA.Policy _get_policy(int policy_type) {
        throw new org.omg.CORBA.NO_IMPLEMENT();
    } //_get_policy

    /** required by org.omg.CORBA.Object */
    public org.omg.CORBA.Object _set_policy_override(
            org.omg.CORBA.Policy[] policies,
            org.omg.CORBA.SetOverrideType set_add) {
        throw new org.omg.CORBA.NO_IMPLEMENT();
    } //_set_policy_override

    /** required by org.omg.CORBA.Object */
    public org.omg.CORBA.DomainManager[] _get_domain_managers() {
        throw new org.omg.CORBA.NO_IMPLEMENT();
    } //_get_domain_managers

/* END OF TEMPLATE */
    
    static final long serialVersionUID = -4916923811099125783L;

} //IluObjectBase
