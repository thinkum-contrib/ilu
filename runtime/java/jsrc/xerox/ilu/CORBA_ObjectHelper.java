/* CORBA_ObjectHelper.java */
/* Chris Jacobi, December 23, 1998 11:41 am PST */
/* $Id: CORBA_ObjectHelper.java,v 1.9 1999/08/03 01:53:54 janssen Exp $ */

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


public class CORBA_ObjectHelper implements xerox.ilu.IluIOFunctions  {
    private final static java.lang.String _id = 
        "IDL:omg.org/CORBA/Object:1.0";
    
    private static xerox.ilu.IluTypeCode _tc =
        xerox.ilu.IluTypeCode.corba_objref();

    public final static java.lang.String id() {
        return _id;
    } //id 

    public final static xerox.ilu.IluTypeCode type() {
        return _tc;
    } //type 

    public static org.omg.CORBA.Object narrow(java.lang.Object _x) {
        return (org.omg.CORBA.Object) _x;
    } //narrow 

    public static void insert(org.omg.CORBA.Any _any, org.omg.CORBA.Object _x) 
            throws org.omg.CORBA.SystemException {
        xerox.ilu.IluAny _ia = (xerox.ilu.IluAny) _any;
        _ia.assign(_tc, _x);
    } //insert 

    public static org.omg.CORBA.Object extract(org.omg.CORBA.Any _any) 
            throws org.omg.CORBA.SystemException {
        xerox.ilu.IluAny _ia = (xerox.ilu.IluAny) _any;
        java.lang.Object _ob = _ia.cachedValue();
        if (_ob == null) {_ob = _ia.value(_tc);}
        return (org.omg.CORBA.Object) _ob;
    } //extract 

    public static xerox.ilu.IluPickle to_pickle(org.omg.CORBA.Object _x) 
            throws org.omg.CORBA.SystemException {
        int _sz = 0;
        xerox.ilu.IluCall _call = null;
        xerox.ilu.IluPickle _pickle = new xerox.ilu.IluPickle();
        try {
            _call = _pickle.startToPickle();
            _sz = _call.szObject(_x, false, CORBA_ObjectStub.iluClass());
            _pickle.midToPickle(_call, _sz, id());
            _call.outObject(_x, false, CORBA_ObjectStub.iluClass());
        } finally {
            _pickle.endToPickle(_call);
        }
        return _pickle;
    } //to_pickle 

    public static org.omg.CORBA.Object from_pickle(xerox.ilu.IluPickle _pickle) 
            throws org.omg.CORBA.SystemException {
        org.omg.CORBA.Object _x = null;
        xerox.ilu.IluCall _call = null;
        try {
            _call = _pickle.startFromPickle(id());
            _x = (org.omg.CORBA.Object) _call.inObject(
                false, CORBA_ObjectStub.iluClass());
        } finally {
            _pickle.endFromPickle(_call);
        }
        return _x;
    } //from_pickle 

    /** Not public; implements xerox.ilu.IluIOFunctions */
    public int szFunc(
            xerox.ilu.IluCall _call, 
            java.lang.Object _x, 
            xerox.ilu.IluTypeCode _tc) 
            throws org.omg.CORBA.SystemException {
        org.omg.CORBA.Object _xx = (org.omg.CORBA.Object)_x;
        return _call.szObject(_xx, false, CORBA_ObjectStub.iluClass());
    } //szFunc 

    /** Not public; implements xerox.ilu.IluIOFunctions */
    public void outFunc(
            xerox.ilu.IluCall _call, 
            java.lang.Object _x, 
            xerox.ilu.IluTypeCode _tc) 
            throws org.omg.CORBA.SystemException {
        org.omg.CORBA.Object _xx = (org.omg.CORBA.Object)_x;
        _call.outObject(_xx, false, CORBA_ObjectStub.iluClass());
    } //outFunc 

    /** Not public; implements xerox.ilu.IluIOFunctions */
    public java.lang.Object inFunc(
            xerox.ilu.IluCall _call, 
            xerox.ilu.IluTypeCode _tc) 
            throws org.omg.CORBA.SystemException {
        return (org.omg.CORBA.Object) _call.inObject(
            false, CORBA_ObjectStub.iluClass());
    } //inFunc 

    public boolean isAFunc(Object _x, xerox.ilu.IluTypeCode _tc) {
        if (_x == null) return false;
        return (_x instanceof org.omg.CORBA.Object);
    } //isAFunc 

}//CORBA_ObjectHelper

