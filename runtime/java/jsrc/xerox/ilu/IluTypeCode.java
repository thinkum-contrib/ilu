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
/* IluTypeCode.java */
/* Chris Jacobi, January 18, 1999 7:26 pm PST */
/* $Id: IluTypeCode.java,v 1.18 1999/08/03 01:54:02 janssen Exp $ */
 
package xerox.ilu;
import org.omg.CORBA.TCKind;
import org.omg.CORBA.TypeCode;


/**
 * Representation of types... (CORBA TypeCode's)<p>
 * 
 * Once initialized IluTypeCode's are immutable and can freely be 
 * passed around. <p>
 *
 * @see org.omg.CORBA.TCKind
 * @see org.omg.CORBA.Any
 * @see IluTypeKind
 */
public final class IluTypeCode 
        extends org.omg.CORBA.TypeCode 
        implements IluResolving, java.io.Serializable {
    
    private transient long yuuid = 0; 
    private java.lang.String jjuid;	
    private transient xerox.ilu.IluIOFunctions iofunctions = null;
    private transient TCKind corbaKind = null; //corba encoding
    
    private static java.util.Hashtable gtable = new java.util.Hashtable();
    
    
    /** package initialization; idempotent */
    public static final void init() {
        IluInit.init();
    } //init
    
    static { //order matters (gtable needs to be initialized)
        init();
    }
    
    /** private: arbitrary applications must not create random IluTypeCode's */
    protected IluTypeCode() {
    } //constructor
    
    /** accessing the uuid */
    public final java.lang.String getUID() {
        return jjuid;
    } //getUID
    
    /** corba encoding */
    public final TCKind kind() {
        return corbaKind;
    } //kind
    
    /** Stub and ILU use; not used by applications */
    public final xerox.ilu.IluIOFunctions getIOFunctions() {
        return iofunctions;
    } //getIOFunctions
    
    /** 
     * public constructor for an IluTypeCode.
     * Returns null if type not known (e.g. because stub not initialized)
     */
    public static IluTypeCode fromUID_or_null(java.lang.String uuid) {
        return (IluTypeCode) gtable.get(uuid);
    } //fromUID_or_null
    
    private native void nativeInitTypeCode(java.lang.String uuid);
    
    /** 
     * Stub use; not used by applications.
     * Can't over-write global IluTypeCode table for existing types. 
     */
    public static IluTypeCode newTypeCode(java.lang.String uuid, 
        xerox.ilu.IluIOFunctions f, 
        TCKind corbaKind)  
    {
        IluTypeCode tc = null;
        if (corbaKind==null) {
           throw new xerox.ilu.IluSystemExceptionBase("null TCKind");
        }
        synchronized (gtable) {
            tc = fromUID_or_null(uuid);
            if (tc != null) {
                return tc;
                //don't overwrite existing registrations
            } else {
                tc = new IluTypeCode();
                tc.jjuid = uuid;
                tc.corbaKind = corbaKind;
                tc.iofunctions = f;
                if (xerox.ilu.IluInit.abortInitializations()) {return tc;}
                tc.nativeInitTypeCode(uuid);
                gtable.put(uuid, tc);
            }
        }
        return tc;
    } //newTypeCode
    

    /*
     * A few standard types...
     */
    
    private native void nativeSpecialInitTypeCode(int corbaTypeKind);
    private static SpecialIOFuncs sf = new SpecialIOFuncs();
    
    /* for pre-defined IluTypeCode's */
    private static IluTypeCode specialNewTypeCode(TCKind corbaTypeKind) {
        IluTypeCode tc = new IluTypeCode();
        tc.corbaKind = corbaTypeKind;
        tc.iofunctions = sf;
        if (! xerox.ilu.IluInit.abortInitializations()) {
            tc.nativeSpecialInitTypeCode(corbaTypeKind.value());
            gtable.put(tc.jjuid, tc);
        }
        return tc;
    }
    
    private static IluTypeCode tc_corba_short = null;
    public static IluTypeCode corba_short() {return tc_corba_short;}
    
    private static IluTypeCode tc_corba_long = null;
    public static IluTypeCode corba_long() {return tc_corba_long;}
    
    private static IluTypeCode tc_corba_ushort = null;
    public static IluTypeCode corba_ushort() {return tc_corba_ushort;}
    
    private static IluTypeCode tc_corba_ulong = null;
    public static IluTypeCode corba_ulong() {return tc_corba_ulong;}

    private static IluTypeCode tc_corba_string = null;
    public static IluTypeCode corba_string() {return tc_corba_string;}
    
    private static IluTypeCode tc_corba_wstring = null;
    public static IluTypeCode corba_wstring() {return tc_corba_wstring;}
    
    private static IluTypeCode tc_corba_boolean = null;
    public static IluTypeCode corba_boolean() {return tc_corba_boolean;}
    
    private static IluTypeCode tc_corba_char = null;
    public static IluTypeCode corba_char() {return tc_corba_char;}
    
    private static IluTypeCode tc_corba_wchar = null;
    public static IluTypeCode corba_wchar() {return tc_corba_wchar;}
    
    private static IluTypeCode tc_corba_octet = null;
    public static IluTypeCode corba_octet() {return tc_corba_octet;}
    
    private static IluTypeCode tc_corba_float = null;
    public static IluTypeCode corba_float() {return tc_corba_float;}
    
    private static IluTypeCode tc_corba_double = null;
    public static IluTypeCode corba_double() {return tc_corba_double;}
    
    private static IluTypeCode tc_corba_longlong = null;
    public static IluTypeCode corba_longlong() {return tc_corba_longlong;}
    
    private static IluTypeCode tc_corba_ulonglong = null;
    public static IluTypeCode corba_ulonglong() {return tc_corba_ulonglong;}
    
    private static IluTypeCode tc_corba_objref = null;
    public static IluTypeCode corba_objref() {return tc_corba_objref;}
   
    private static IluTypeCode tc_corba_any = null;
    public static IluTypeCode corba_any() {return tc_corba_any;}
   

    /*friendly*/ static final void initPhase3() {
        if (tc_corba_short == null) { //otherwise already initialized
            xerox.basics.VMExtras.makeGCRoot(gtable); 
            /* initialize our static typecodes */
            tc_corba_short = specialNewTypeCode(TCKind.tk_short);
            tc_corba_long = specialNewTypeCode(TCKind.tk_long);
            tc_corba_ushort = specialNewTypeCode(TCKind.tk_ushort);
            tc_corba_ulong = specialNewTypeCode(TCKind.tk_ulong);
            tc_corba_string = specialNewTypeCode(TCKind.tk_string);
            tc_corba_boolean = specialNewTypeCode(TCKind.tk_boolean);
            tc_corba_char = specialNewTypeCode(TCKind.tk_char);
            tc_corba_wchar = specialNewTypeCode(TCKind.tk_wchar);
            tc_corba_octet = specialNewTypeCode(TCKind.tk_octet);
            tc_corba_float = specialNewTypeCode(TCKind.tk_float);
            tc_corba_double = specialNewTypeCode(TCKind.tk_double);
            tc_corba_longlong = specialNewTypeCode(TCKind.tk_longlong);
            tc_corba_ulonglong = specialNewTypeCode(TCKind.tk_ulonglong);
            tc_corba_objref = specialNewTypeCode(TCKind.tk_objref);
            tc_corba_any = specialNewTypeCode(TCKind.tk_any);
            /* special*/
            tc_corba_wstring = CORBA_WStringHelper.type();
        }
    } //initPhase3


    /** overrides toString of java.lang.Object; usefull for debugging */
    public java.lang.String toString() {
        if (jjuid != null) {
            return "ilu-IluTypeCode<" + jjuid + ">";
        } else {
            return super.toString();
        }
    } //toString
    
    public boolean equal(org.omg.CORBA.TypeCode tc) {
        return (this==tc);
    }
    
    public java.lang.String id() throws org.omg.CORBA.TypeCodePackage.BadKind {
        throw new org.omg.CORBA.NO_IMPLEMENT();
    }
    
    public java.lang.String name() throws org.omg.CORBA.TypeCodePackage.BadKind {
        throw new org.omg.CORBA.NO_IMPLEMENT();
    }
    
    public int member_count() throws org.omg.CORBA.TypeCodePackage.BadKind {
        throw new org.omg.CORBA.NO_IMPLEMENT();
    }
    
    public java.lang.String member_name(int index) throws
            org.omg.CORBA.TypeCodePackage.BadKind, 
            org.omg.CORBA.TypeCodePackage.Bounds {
        throw new org.omg.CORBA.NO_IMPLEMENT();
    }
    
    public org.omg.CORBA.TypeCode member_type(int index) throws
            org.omg.CORBA.TypeCodePackage.BadKind, 
            org.omg.CORBA.TypeCodePackage.Bounds {
        throw new org.omg.CORBA.NO_IMPLEMENT();
    }
    
    public org.omg.CORBA.Any member_label(int index) throws
            org.omg.CORBA.TypeCodePackage.BadKind, 
            org.omg.CORBA.TypeCodePackage.Bounds {
        throw new org.omg.CORBA.NO_IMPLEMENT();
    }
    
    public org.omg.CORBA.TypeCode discriminator_type() throws org.omg.CORBA.TypeCodePackage.BadKind {
        throw new org.omg.CORBA.NO_IMPLEMENT();
    }

    public int default_index() throws org.omg.CORBA.TypeCodePackage.BadKind {
        throw new org.omg.CORBA.NO_IMPLEMENT();
    }
    
    public int length() throws org.omg.CORBA.TypeCodePackage.BadKind {
        throw new org.omg.CORBA.NO_IMPLEMENT();
    }

    public org.omg.CORBA.TypeCode content_type() throws org.omg.CORBA.TypeCodePackage.BadKind {
        throw new org.omg.CORBA.NO_IMPLEMENT();
    }
    
    
    private static java.util.Hashtable typeIdProps = null;
    
    /*friendly*/ static java.lang.String getTypeId(java.lang.String key) {
        if (typeIdProps == null) {
            typeIdProps = new xerox.ilu.IluTypeIdProps();
        }
        java.lang.String s = (java.lang.String) typeIdProps.get(key);
        if (s==null) throw new org.omg.CORBA.UNKNOWN();
        return s;
    } //getId
    
    
    /**implementing IluResolving*/
    public java.lang.Object readResolve() 
            throws java.io.ObjectStreamException
    {
        IluTypeCode r = fromUID_or_null(this.jjuid);
        if (r==null) {
            throw new IluRuntimeException(
                "Resolving IluTypeCode: probably stub not loaded for " + this.jjuid
                );
        }
        return r;
    } //readResolve
    
    static final long serialVersionUID = -3025970082430343405L;

} //IluTypeCode



/** 
 * IluIOFunctions: marshalling implementations for pre-defined types.
 * Nothing to be called by applications directly!
 */
/* friendly */
class SpecialIOFuncs implements xerox.ilu.IluIOFunctions {
    
    static private xerox.ilu.IluClassRep _class_CORBA_Object = null;
    
    public static final xerox.ilu.IluClassRep class_CORBA_Object() {
        if (_class_CORBA_Object == null) {
            xerox.basics.Environment.loadClasses("ilu.CORBA_Object_stub");
            _class_CORBA_Object = CORBA_ObjectStub.iluClass();
        }
        return _class_CORBA_Object;
    } // class_CORBA_Object
    
    
    /** Not called directly by applications */
    public int szFunc(
            xerox.ilu.IluCall _call, 
            java.lang.Object _x, IluTypeCode _tc) 
            throws org.omg.CORBA.SystemException {
        int corba_typekind = _tc.kind().value();
        switch (corba_typekind) {
            case TCKind._tk_short: 
            case TCKind._tk_ushort:
                java.lang.Integer i1 = (java.lang.Integer) _x;
                return _call.szInt16((short) i1.intValue());
            case TCKind._tk_long: 
            case TCKind._tk_ulong:
                java.lang.Integer i2 = (java.lang.Integer) _x;
                return _call.szInt32(i2.intValue());
            case TCKind._tk_string:
                /* assuming a limit of zero.*/
                java.lang.String s0 = (java.lang.String) _x;
                return _call.szString8(s0, 0);
            case TCKind._tk_wstring:
                /* assuming a limit of zero.*/
                java.lang.String s1 = (java.lang.String) _x;
                return _call.szString16(s1, 0);
            case TCKind._tk_boolean:
                java.lang.Boolean b0 = (java.lang.Boolean) _x;
                return _call.szBool(b0.booleanValue());
            case TCKind._tk_char: 
                java.lang.Character c0 = (java.lang.Character) _x;
                return _call.szChar8(c0.charValue());
            case TCKind._tk_wchar:
                java.lang.Character c1 = (java.lang.Character) _x;
                return _call.szChar16(c1.charValue());
            case TCKind._tk_octet:
                java.lang.Integer ib = (java.lang.Integer) _x;
                return _call.szByte((byte) ib.intValue());
            case TCKind._tk_float:
                java.lang.Float f0 = (java.lang.Float) _x;
                return _call.szReal32(f0.floatValue());
            case TCKind._tk_double:
                java.lang.Double d0 = (java.lang.Double) _x;
                return _call.szReal64(d0.doubleValue());
            case TCKind._tk_longlong:
                java.lang.Long l0 = (java.lang.Long) _x;
                return _call.szInt64(l0.longValue());
            case TCKind._tk_ulonglong:
                java.lang.Long l1 = (java.lang.Long) _x;
                return _call.szCard64(l1.longValue());
            case TCKind._tk_objref:
                java.lang.Object ob = _x;
                return _call.szObject(ob, false, IluClassRep.rootClass());
            case TCKind._tk_any:
                xerox.ilu.IluAny a = (xerox.ilu.IluAny) _x;
                return a._szAny(_call); 
            default: 
                break;
        }
        xerox.ilu.IluDebug.iluPreError("SpecialIOFuncs.szFunc not impl.");
        throw new xerox.ilu.IluSystemExceptionBase("NOT IMPL");
    } //szFunc 

    /** Not called directly by applications */
    public void outFunc(
            xerox.ilu.IluCall _call, java.lang.Object _x, IluTypeCode _tc) 
            throws org.omg.CORBA.SystemException {
        int corba_typekind = _tc.kind().value();
        switch (corba_typekind) {
            case TCKind._tk_short: 
            case TCKind._tk_ushort:
                java.lang.Integer i0 = (java.lang.Integer) _x;
                _call.outInt16((short) i0.intValue());
                break;
            case TCKind._tk_long: 
            case TCKind._tk_ulong:
                java.lang.Integer i1 = 
                    (java.lang.Integer) _x;
                _call.outInt32(i1.intValue());
                break;
            case TCKind._tk_string:
                java.lang.String s0 = (java.lang.String) _x;
                _call.outString8(s0, 0);
                break;
            case TCKind._tk_wstring:
                java.lang.String s1 = (java.lang.String) _x;
                _call.outString16(s1, 0);
                break;
            case TCKind._tk_boolean:
                java.lang.Boolean b0 = (java.lang.Boolean) _x;
                _call.outBool(b0.booleanValue());
                break;
            case TCKind._tk_char: 
                java.lang.Character c0 = (java.lang.Character) _x;
                _call.outChar8(c0.charValue());
                break;
            case TCKind._tk_wchar:
                java.lang.Character c1 = (java.lang.Character) _x;
                _call.outChar16(c1.charValue());
                break;
            case TCKind._tk_octet:
                java.lang.Integer ib = (java.lang.Integer) _x;
                _call.outByte((byte) ib.intValue());
                break;
            case TCKind._tk_float:
                java.lang.Float f0 = (java.lang.Float) _x;
                _call.outReal32(f0.floatValue());
                break;
            case TCKind._tk_double:
                java.lang.Double d0 = (java.lang.Double) _x;
                _call.outReal64(d0.doubleValue());
                break;
            case TCKind._tk_longlong:
                java.lang.Long l0 = (java.lang.Long) _x;
                _call.outInt64(l0.longValue());
                break;
            case TCKind._tk_ulonglong:
                java.lang.Long l1 = (java.lang.Long) _x;
                _call.outCard64(l1.longValue());
                break;
            case TCKind._tk_objref:
                _call.outObject(_x, false, IluClassRep.rootClass()); 
            case TCKind._tk_any:
                xerox.ilu.IluAny a = (xerox.ilu.IluAny) _x;
                a._outAny(_call); 
                break;
            default: 
                xerox.ilu.IluDebug.iluPreError("SpecialIOFuncs.outFunc not impl.");
                throw new xerox.ilu.IluSystemExceptionBase("NOT IMPL TC");
        }
    } //outFunc 

    /** Not called directly by applications */
    public java.lang.Object inFunc(xerox.ilu.IluCall _call, IluTypeCode _tc) 
            throws org.omg.CORBA.SystemException {
        int corba_typekind = _tc.kind().value();
        switch (corba_typekind) {
            case TCKind._tk_short: 
            case TCKind._tk_ushort:
                int i1 =_call.inInt16();
                return new java.lang.Integer(i1);
            case TCKind._tk_long: 
            case TCKind._tk_ulong:
                int i2 =_call.inInt32();
                return new java.lang.Integer(i2);
            case TCKind._tk_string:
                return _call.inString8(0);
            case TCKind._tk_wstring:
                return _call.inString16(0);
            case TCKind._tk_boolean:
                boolean b0 = _call.inBool();
                return new java.lang.Boolean(b0);
            case TCKind._tk_char: 
                char c0 = _call.inChar8();
                return new java.lang.Character(c0);
            case TCKind._tk_wchar:
                char c1 =  _call.inChar16();
                return new java.lang.Character(c1);
            case TCKind._tk_octet:
                byte b1 =  _call.inByte();
                return new java.lang.Integer(b1);
            case TCKind._tk_float:
                float f1 = _call.inReal32();
                return new java.lang.Float(f1);
            case TCKind._tk_double:
                double d1 = _call.inReal64();
                return new java.lang.Double(d1);
            case TCKind._tk_longlong:
                double d2 = _call.inInt64();
                return new java.lang.Double(d2);
            case TCKind._tk_objref:
                java.lang.Object ob = 
                    _call.inObject(false, IluClassRep.rootClass()); 
                return ob;
            case TCKind._tk_any:
                xerox.ilu.IluAny a = xerox.ilu.IluAny._inAny(_call);
                return a;
            default: 
                break;
        }
        xerox.ilu.IluDebug.iluPreError("SpecialIOFuncs.inFunc not impl.");
        throw new xerox.ilu.IluSystemExceptionBase("NOT IMPL");
    } //inFunc 

    /** Not intended for direct calls by applications */
    public boolean isAFunc(java.lang.Object _x, IluTypeCode _tc) 
            throws org.omg.CORBA.SystemException {
        int corba_typekind = _tc.kind().value();
        switch (corba_typekind) {
            case TCKind._tk_short:
                if (_x instanceof java.lang.Integer) {
                    java.lang.Integer i0 = (java.lang.Integer) _x;
                    int i2 = i0.intValue();
                    return (i2 >= -32768 && i2 <= 32767);
                }
                return false;
            case TCKind._tk_long: 
            case TCKind._tk_ulong: 
            case TCKind._tk_ushort:
                return (_x instanceof java.lang.Integer);
            case TCKind._tk_string: 
            case TCKind._tk_wstring:
                return (_x instanceof java.lang.String);
            case TCKind._tk_boolean:
                return (_x instanceof java.lang.Boolean);
            case TCKind._tk_char: 
            case TCKind._tk_wchar:
                return (_x instanceof java.lang.Character);
            case TCKind._tk_octet:
                if (_x instanceof java.lang.Integer) {
                    java.lang.Integer i3 = (java.lang.Integer) _x;
                    int i4 = i3.intValue();
                    return (i4 >= -128 && i4 <= 127);
                }
                return false;
            case TCKind._tk_float:
                return (_x instanceof java.lang.Float);
            case TCKind._tk_double:
                return (_x instanceof java.lang.Double);
            case TCKind._tk_longlong: 
            case TCKind._tk_ulonglong:
                return (_x instanceof java.lang.Long);
            case TCKind._tk_objref:
                return (_x instanceof org.omg.CORBA.Object);
            case TCKind._tk_any:
                return (_x instanceof xerox.ilu.IluAny);
            default: 
                break;
        }
        xerox.ilu.IluDebug.iluPreError("SpecialIOFuncs.isAFunc not impl.");
        throw new xerox.ilu.IluSystemExceptionBase("NOT IMPL");
    } //isAFunc 

} //SpecialIOFuncs
