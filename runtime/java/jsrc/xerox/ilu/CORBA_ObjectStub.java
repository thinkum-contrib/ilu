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
/* CORBA_ObjectStub.java */
/* Chris Jacobi, January 6, 1999 4:18 pm PST */
/* $Id: CORBA_ObjectStub.java,v 1.13 1999/08/03 01:53:44 janssen Exp $ */
 

package xerox.ilu;

/** 
 * Implementation for CORBA objects. No client use expected.
 * @see org.omg.CORBA.Object
 */
public class CORBA_ObjectStub 
        extends xerox.ilu.IluSurrogateObject 
        implements org.omg.CORBA.Object {
    
    static xerox.ilu.IluClassRep CORBA_Object_classRep;
    
    /** returns ilu class implemented by this stub class */
    static public xerox.ilu.IluClassRep iluClass() {
        return CORBA_Object_classRep;
    } //iluClass
    
    static private _CORBA_Object_skeletonClass _CORBA_Object_skeleton = 
        new _CORBA_Object_skeletonClass();
        
    static {
        CORBA_Object_classRep = xerox.ilu.IluClassRep.setupClass(
            "org.omg.CORBA.Object", //java reference interface
            "ilu.CORBA-Object", //ilu object type
            "IDL:omg.org/CORBA/Object:1.0", //uuid
            0); //method count
        CORBA_Object_classRep.setOptional();
        CORBA_Object_classRep.setSurrClass(
            "xerox.ilu.CORBA_ObjectStub"
            );
        CORBA_Object_classRep.setSurrFactory(
            new _CORBA_Object_Factory()
            );
        CORBA_Object_classRep.setIfName("ilu"); 
        CORBA_Object_classRep.setIfBrand("version 2"); 
        CORBA_ObjectHelper.id(); //makes sure helper class is loaded
        CORBA_Object_classRep.finishClass();
    } //static

    public static void registerTrueObject(
            java.lang.String _ih,
            org.omg.CORBA.Object _tob,
            xerox.ilu.IluServer _s) throws org.omg.CORBA.SystemException {
        xerox.ilu.Ilu.registerTrueObject(
            _ih, _tob, _s, CORBA_Object_classRep, 0);
    } //registerTrueObject

    CORBA_ObjectStub() {
    } //constructor
    
    CORBA_ObjectStub(Object arg) {
    } //constructor
    
} //CORBA_ObjectStub


/* friendly */
class _CORBA_Object_Factory extends xerox.ilu.IluFactory {

    _CORBA_Object_Factory() {
    } //constructor

    public java.lang.Object createSurrogateObject(java.lang.Object _arg) {
        return new CORBA_ObjectStub();
    }
    
} // _CORBA_Object_Factory


/* friendly */
class _CORBA_Object_skeletonClass implements xerox.ilu.IluSkeleton {

    _CORBA_Object_skeletonClass() {
    } //constructor

    public void serveRequest(
            xerox.ilu.IluCall _call, xerox.ilu.IluMethodRep _m) 
            throws org.omg.CORBA.SystemException {
    } //serveRequest
    
} //_CORBA_Object_skeletonClass
