// javastubs/xerox/ilu/CStringHelper.java
// Stubs for "xerox.ilu"
//
// This file was automatically generated with ILU (version 0.0jacobi) tools
// at Thu Nov  6 14:21:58 1997 by `jacobi'
// running "/tilde/jacobi/ilus/stubbers/java/java-stubber" of Thu Nov  6 10:25:30 1997
// on "/tilde-am/jacobi/temp/ilu.isl" of Fri Sep 26 10:56:14 1997.
//
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
// ILU information:  ftp://ftp.parc.xerox.com/pub/ilu/ilu.html.
// Chris Jacobi, November 25, 1998 5:24 pm PST
// Manually edited February 17, 1998 6:01:04 pm PS

package xerox.ilu;

public class CStringHelper implements xerox.ilu.IluIOFunctions  {
    
    private final static java.lang.String _id = 
        IluTypeCode.getTypeId("string");
    private static xerox.ilu.IluTypeCode _tc = null;
    
    static {
        _tc = xerox.ilu.IluTypeCode.newTypeCode(id(), new CStringHelper(), org.omg.CORBA.TCKind.tk_sequence);
    }

    public final static java.lang.String id() {
        return _id;
    } //id 

    public final static xerox.ilu.IluTypeCode type() {
        return _tc;
    } //type 

    public static void insert(org.omg.CORBA.Any _any, java.lang.String _x)
            throws org.omg.CORBA.SystemException {
        xerox.ilu.IluAny _ia = (xerox.ilu.IluAny) _any;
        _ia.assign(_tc, _x);
    } //insert 

    public static java.lang.String extract(org.omg.CORBA.Any _any) 
            throws org.omg.CORBA.SystemException {
        xerox.ilu.IluAny _ia = (xerox.ilu.IluAny) _any;
        java.lang.Object _ob = _ia.cachedValue();
        if (_ob == null) {_ob = _ia.value(_tc);}
        return (java.lang.String) _ob;
    } //extract 

    public static xerox.ilu.IluPickle to_pickle(
            java.lang.String _x) 
            throws org.omg.CORBA.SystemException {
        int _sz = 0;
        xerox.ilu.IluCall _call = null;
        xerox.ilu.IluPickle _pickle = new xerox.ilu.IluPickle();
        try {
            _call = _pickle.startToPickle();
            _sz = _call.szString8(_x, 0);
            _pickle.midToPickle(_call, _sz, id());
            _call.outString8(_x, 0);
        } finally {
            _pickle.endToPickle(_call);
        }
        return _pickle;
    } //to_pickle 

    public static java.lang.String from_pickle(
            xerox.ilu.IluPickle _pickle) 
            throws org.omg.CORBA.SystemException {
        java.lang.String _x = null;
        xerox.ilu.IluCall _call = null;
        try {
            _call = _pickle.startFromPickle(id());
            _x = _call.inString8(0);
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
        java.lang.String _xx = (java.lang.String)_x;
        return _call.szString8(_xx, 0);
    } //szFunc 

    /** Not public; implements xerox.ilu.IluIOFunctions */
    public void outFunc(
            xerox.ilu.IluCall _call, 
            java.lang.Object _x, 
            xerox.ilu.IluTypeCode _tc) 
            throws org.omg.CORBA.SystemException {
        java.lang.String _xx = (java.lang.String)_x;
        _call.outString8(_xx, 0);
    } //outFunc 

    /** Not public; implements xerox.ilu.IluIOFunctions */
    public java.lang.Object inFunc(xerox.ilu.IluCall _call, xerox.ilu.IluTypeCode _tc) throws org.omg.CORBA.SystemException {
        return _call.inString8(0);
    } //inFunc 

    public boolean isAFunc(java.lang.Object _x, xerox.ilu.IluTypeCode _tc) {
        if (_x == null) return false;
        return (_x instanceof java.lang.String);
    } //isAFunc 

}//CStringHelper



