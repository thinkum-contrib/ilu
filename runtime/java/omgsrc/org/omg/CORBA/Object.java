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
/* org.omg.CORBA.Object.java */
/* Chris Jacobi, November 23, 1998 3:30 pm PST */

/*
 */
 
/* $Id: Object.java,v 1.8 1999/08/03 01:54:55 janssen Exp $ */
 
package org.omg.CORBA;

/**
 * Generic, client visible interface for corba objects.<p>
 * This interface is only for corba compatible ilu objects
 * and should not be used by pure ilu objects.
 * The interface is not corba complete.
 * 
 * 
 * @see xerox.ilu.IluObject
 * @see org.omg.CORBA.portable.ObjectImpl
 */
public interface Object {
    boolean _is_a(java.lang.String identifier);
    boolean _is_equivalent(org.omg.CORBA.Object that);
    boolean _non_existent();
    int _hash(int maximum);
    org.omg.CORBA.Object _duplicate();
    void _release();
    
    /* not used by ilu; 
     * With standard corba they will be here, but impl in ObjectImpl
     * With ilu-only, not here, and, not required
    org.omg.CORBA.ImplementationDef _get_implementation();
    org.omg.CORBA.InterfaceDef _get_interface();
    org.omg.CORBA.Request _request(java.lang.String operation);
    org.omg.CORBA.Request _create_request(
        org.omg.CORBA.Context ctx,
        java.lang.String operation,
        java.lang.NVList arg_list,
        java.lang.NamedValue result);
    org.omg.CORBA.Request _create_request(
        org.omg.CORBA.Context ctx,
        java.lang.String operation,
        org.omg.CORBA.NVList arg_list,
        org.omg.CORBA.NamedValue result,
        org.omg.CORBA.ExceptionList exclist,
        org.omg.CORBA.ContextList ctxlist);
    org.omg.CORBA.Policy _get_policy(int policy_type);
    org.omg.CORBA.DomainManager[] _get_domain_managers();
    org.omg.CORBA.Object _set_policy_override(
        org.omg.CORBA.Policy[] policies,
        org.omg.CORBA.SetOverrideType set_add);
    */
        
} //Object
