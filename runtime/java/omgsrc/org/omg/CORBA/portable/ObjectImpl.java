/* ObjectImpl.java */
/* Chris Jacobi, November 20, 1998 2:47 pm PST */

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

 
/* $Id: ObjectImpl.java,v 1.10 1999/08/03 01:54:34 janssen Exp $ */

 
package org.omg.CORBA.portable;

/**
 * CORBA (but not ilu) requires all true classes to inherit from this class<p>
 *
 * This class is not an Ilu requirement; in fact since it implements
 * the org.omg.CORBA.Object interface it is not recommended to be used
 * by pure ilu objects unless they are also CORBA objects.<p>
 *
 * This class is not a corba complete implementation.
 * @see org.omg.CORBA.Object 
 * @see xerox.ilu.IluObjectBase 
 * @see xerox.ilu.IluDataSupport 
 */
abstract public class ObjectImpl  
        implements org.omg.CORBA.Object {
        
    /** constructor */
    protected ObjectImpl() {
    } //constructor
    
    private transient org.omg.CORBA.portable.Delegate __delegate;
    
    
    /** return the Delegate contained in this ObjectImpl instance. */
    public org.omg.CORBA.portable.Delegate _get_delegate() {
        if (__delegate == null) {
	    throw new org.omg.CORBA.BAD_OPERATION(
	        "The delegate has not been set!"
	        );
        }
        return __delegate;
    } //_get_delegate


    /** set the Delegate contained in this ObjectImpl instance. */
    public void _set_delegate(org.omg.CORBA.portable.Delegate arg) {
        __delegate = arg;
    } //_set_delegate

    private static org.omg.CORBA.ORB iluOrb = null;
    private static org.omg.CORBA.BOA iluBoa = null;
    
    public org.omg.CORBA.ORB _orb() {
        if (iluOrb == null) {
            iluOrb = org.omg.CORBA.ORB.init();
        }
        return iluOrb;
    } //_orb
    
    public org.omg.CORBA.BOA _boa() {
        if (iluBoa == null) {
            iluBoa = new org.omg.CORBA.BOA(_orb());
        }
        return iluBoa;
    } //_boa
    
    public org.omg.CORBA.Object _duplicate() {
        return _get_delegate().duplicate(this);
    }
    
    /** default implementation of the org.omg.CORBA.Object method. */
    public void _release() {
        _get_delegate().release(this);
    }
    
    /* not used by ilu
    public abstract String[] _ids();
    */
    
    /** default implementation of the org.omg.CORBA.Object method. */
    public boolean _is_a(String repository_id) {
        return _get_delegate().is_a(this, repository_id);
    }
    
    /** default implementation of the org.omg.CORBA.Object method. */
    public boolean _is_equivalent(org.omg.CORBA.Object that) {
        return _get_delegate().is_equivalent(this, that);
    }
    
    /** default implementation of the org.omg.CORBA.Object method. */
    public boolean _non_existent() {
        return _get_delegate().non_existent(this);
    }

    /** default implementation of the org.omg.CORBA.Object method. */
    public int _hash(int maximum) {
        return _get_delegate().hash(this, maximum);
    }
    

} // ObjectImpl

