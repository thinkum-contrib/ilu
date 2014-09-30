// javastubs/xerox/ilu/CORBA_WStringHelper.java
// Stubs for "dummy"
//
// This file was automatically generated with ILU (version 0.0jacobi) tools
// at Mon Jun 15 20:33:10 1998 by `jacobi'
// running "/rubberduckyChris/iluh/bin/java-stubber" of Thu Jun 11 14:31:22 1998
// on "/tilde-am/jacobi/temp/ilu/dummy.isl" of Mon Jun 15 20:33:06 1998,
// and "/tilde-am/jacobi/temp/ilu/ilu.isl" of Mon Jun 15 19:46:47 1998.
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
/* Chris Jacobi, November 25, 1998 4:58 pm PST */

// ILU information:  ftp://ftp.parc.xerox.com/pub/ilu/ilu.html.
// ****** MANUALLY EDITED ******
// Chris Jacobi, June 30, 1998 10:57 am PDT


package xerox.ilu;

/** Helper class implementing CORBA wstring */
public class CORBA_WStringHelper implements xerox.ilu.IluIOFunctions  {
    
    private final static java.lang.String _id = 
        IluTypeCode.getTypeId("wstring");
    private static xerox.ilu.IluTypeCode _tc = null;
    
    static {
        _tc = xerox.ilu.IluTypeCode.newTypeCode(
            id(), 
            new CORBA_WStringHelper(), 
            org.omg.CORBA.TCKind.tk_wstring /*MANUAL CHANGE*/ );
        /* MANUAL ADDITION */
        xerox.ilu.IluTypeRep tr =
            xerox.ilu.IluTypeRep.registerSequenceType( 
                    //this is ilu_RegisterSequenceType
                "ilu.CORBA-wstring", //name
                "ilu", //islIfName
                "version 2", //islIfBrand
                _id, //uid
                IluTypeCode.getTypeId("character"), //baseUID
                0); //limit
    } //static

    
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
        return ((java.lang.String)_ob);
    } //extract 


    public static xerox.ilu.IluPickle to_pickle(java.lang.String _x)  
            throws org.omg.CORBA.SystemException {
        int _sz = 0;
        xerox.ilu.IluCall _call = null;
        xerox.ilu.IluPickle _pickle = new xerox.ilu.IluPickle();
        try {
            _call = _pickle.startToPickle();
            _sz = _call.szString16(_x, 0);
            _pickle.midToPickle(_call, _sz, id());
            _call.outString16(_x, 0);
        } finally {
            _pickle.endToPickle(_call);
        }
        return _pickle;
    } //to_pickle 

    
    public static java.lang.String from_pickle(xerox.ilu.IluPickle _pickle) 
            throws org.omg.CORBA.SystemException {
        java.lang.String _x = null;
        xerox.ilu.IluCall _call = null;
        try {
            _call = _pickle.startFromPickle(id());
            _x = _call.inString16(0);
        } finally {
            _pickle.endFromPickle(_call);
        }
        return _x;
    } //from_pickle 

    
    /** Not public; implements xerox.ilu.IluIOFunctions */
    public int szFunc(
            xerox.ilu.IluCall _call, 
            java.lang.Object _x, 
            xerox.ilu.IluTypeCode _tc) throws org.omg.CORBA.SystemException {
        java.lang.String _xx = ((java.lang.String)_x);
        return _call.szString16(_xx, 0);
    } //szFunc 


    /** Not public; implements xerox.ilu.IluIOFunctions */
    public void outFunc(
            xerox.ilu.IluCall _call, 
            java.lang.Object _x, 
            xerox.ilu.IluTypeCode _tc) throws org.omg.CORBA.SystemException {
        java.lang.String _xx = ((java.lang.String)_x);
        _call.outString16(_xx, 0);
    } //outFunc 


    /** Not public; implements xerox.ilu.IluIOFunctions */
    public java.lang.Object inFunc(
            xerox.ilu.IluCall _call, 
            xerox.ilu.IluTypeCode _tc) throws org.omg.CORBA.SystemException {
        return _call.inString16(0);
    } //inFunc 

    public boolean isAFunc(java.lang.Object _x, xerox.ilu.IluTypeCode _tc) {
        if (_x == null) return false;
        return (_x instanceof java.lang.String);
    } //isAFunc 


}//CORBA_WStringHelper

