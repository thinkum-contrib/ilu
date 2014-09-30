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
/* IluRootObjectHelper.java */
/* Chris Jacobi, January 6, 1999 5:24 pm PST */
/* $Id: IluRootObjectHelper.java,v 1.10 1999/08/03 01:54:11 janssen Exp $ */

package xerox.ilu;

/** 
 * ILU internal class used in the implementation of ilu objects
 */
public class IluRootObjectHelper implements xerox.ilu.IluIOFunctions  {
    private final static java.lang.String uid = "ilu:root-object-type";
    private static IluTypeCode tc = null;

    /*friendly*/ static final void initPhase3() {
        if (tc==null) {
            tc = IluTypeCode.newTypeCode(uid, 
                new IluRootObjectHelper(), 
                org.omg.CORBA.TCKind.tk_objref);
        }
    } //initPhase3


    public final static java.lang.String id() {
        return uid;
    } //id 

    public final static IluTypeCode type() {
        return tc;
    } //type 

    public static void insert(org.omg.CORBA.Any _any, java.lang.Object _x) 
            throws org.omg.CORBA.SystemException {
        xerox.ilu.IluAny _ia = (xerox.ilu.IluAny) _any;
        _ia.assign(tc, _x);
    } //insert 

    public static java.lang.Object extract(org.omg.CORBA.Any _any) 
            throws org.omg.CORBA.SystemException {
        IluAny iua = (IluAny) _any;
        java.lang.Object _ob = iua.cachedValue();
        if (_ob == null) {_ob = iua.value(tc);}
        return _ob;
    } //extract 

    public static xerox.ilu.IluPickle to_pickle(java.lang.Object _x) 
            throws org.omg.CORBA.SystemException {
        int _sz = 0;
        xerox.ilu.IluCall _call = null;
        xerox.ilu.IluPickle _pickle = new xerox.ilu.IluPickle();
        try {
            _call = _pickle.startToPickle();
            _sz = _call.szObject(_x, false, IluClassRep.rootClass());
            _pickle.midToPickle(_call, _sz, id());
            _call.outObject(_x, false, IluClassRep.rootClass());
        } finally {
            _pickle.endToPickle(_call);
        }
        return _pickle;
    } //to_pickle 

    public static java.lang.Object from_pickle(xerox.ilu.IluPickle _pickle) 
            throws org.omg.CORBA.SystemException {
        java.lang.Object _x = null;
        xerox.ilu.IluCall _call = null;
        try {
            _call = _pickle.startFromPickle(id());
            _x = _call.inObject(
                false, IluClassRep.rootClass());
        } finally {
            _pickle.endFromPickle(_call);
        }
        return _x;
    } //from_pickle 

    /** Not public; implements xerox.ilu.IluIOFunctions */
    public int szFunc(
            xerox.ilu.IluCall _call, 
            java.lang.Object _x, 
            IluTypeCode tc) 
            throws org.omg.CORBA.SystemException {
        return _call.szObject(_x, false, IluClassRep.rootClass());
    } //szFunc 

    /** Not public; implements xerox.ilu.IluIOFunctions */
    public void outFunc(
            xerox.ilu.IluCall _call, 
            java.lang.Object _x, 
            IluTypeCode tc) 
            throws org.omg.CORBA.SystemException {
        _call.outObject(_x, false, IluClassRep.rootClass());
    } //outFunc 

    /** Not public; implements xerox.ilu.IluIOFunctions */
    public java.lang.Object inFunc(
            xerox.ilu.IluCall _call, 
            IluTypeCode tc) 
            throws org.omg.CORBA.SystemException {
        return _call.inObject(false, IluClassRep.rootClass());
    } //inFunc 

    public boolean isAFunc(java.lang.Object _x, IluTypeCode tc) {
        if (_x == null) return false;
        return true;
    } //isAFunc 

}//IluRootObjectHelper

