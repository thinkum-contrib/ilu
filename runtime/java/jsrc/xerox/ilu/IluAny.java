/* IluAny.java */
/* Chris Jacobi, January 18, 1999 7:25 pm PST */
/* $Id: IluAny.java,v 1.14 1999/08/03 01:53:38 janssen Exp $ */

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
 * Representation of values of unspecified types. <p>
 *  
 * The fact that Any's are mutable is a disaster of poor design.
 * It not only would forces us to do locking but forces 
 * restrictions to get reasonable locking constraints... <p>
 *
 * We simplify the design and can ignore locking issues by
 * making the "IluAny" a simple shell holding a "xerox.ilu.IluPickle" <br>
 * The "xerox.ilu.IluPickle" is immutable... <p>
 *
 * We used to do pickling immediately when an IluAny was assigned and
 * unpickling only when a value is extracted; The new corba standard
 * (orbos/98-01016) forces us however to delay pickling. <p>
 *
 * @see xerox.ilu.IluPickle
 * @see xerox.ilu.IluTypeCode
 * @see org.omg.CORBA.TypeCode
 * @see org.omg.CORBA.Any
 */
 
public final class IluAny extends org.omg.CORBA.Any
    implements java.io.Externalizable {
    
    //these 3 variables are always in synch
    private xerox.ilu.IluPickle thePickle = null;
    private java.lang.Object theVal = null;
    private IluTypeCode theType = null;
    
    /** readonly accessor for stored or cached value
     * Used by stubs
     */
    public java.lang.Object cachedValue() {
        return this.theVal;
    }
    
    /*friendly*/ final synchronized void assign3(
            org.omg.CORBA.TypeCode t, 
            java.lang.Object ob, 
            IluPickle p)
    {
        this.theType = (IluTypeCode) t;
        this.theVal = ob;
        this.thePickle = p;
    } //assign3
    
    
    /*friendly*/ final synchronized void condAssignPickle(
            IluTypeCode t, 
            java.lang.Object ob, 
            IluPickle p)
    {
        if (this.theType==t && this.theVal==ob) {
            this.thePickle = p;
        }
    } //condAssignPickle
    

    /*friendly*/ final synchronized void condAssignType(
            IluTypeCode t, IluPickle p)
    {
        if (this.thePickle==p) {
            this.theType = t;
        }
    } //condAssignType
    
    
    /*friendly*/ final synchronized void condAssignObject(
            java.lang.Object v, IluPickle p)
    {
        if (this.thePickle==p) {
            this.theVal = v;
        }
    } //condAssignObject
    
    
    /** Not recommended */
    public IluAny() {
    } //constructor
    
    
    /** 
     * Returns the current pickle, computes it if necessary
     * or raises an exception if it can't.
     * Not CORBA. 
     */
    public final xerox.ilu.IluPickle getPickle() 
            throws org.omg.CORBA.SystemException
    {
        xerox.ilu.IluPickle pickle = this.thePickle;
        if (pickle == null) { 
            IluTypeCode t = this.theType;
            java.lang.Object v = this.theVal;
            pickle = xerox.ilu.IluPickle.alloc(t, v);
            this.thePickle = pickle;
            condAssignPickle(t, v, pickle);
        }
        return pickle;
    } //getPickle
       

    /** 
     * Assigns a new pickle.
     * Not CORBA. 
     */
    public final void setPickle(xerox.ilu.IluPickle pickle) 
            throws org.omg.CORBA.SystemException {
        pickle.ensureValidPickle();
        assign3(null, null, pickle);
    } //setPickle
    
    
    /** 
     * Returns a TypeCode for the value contained in the IluAny. 
     */
    public org.omg.CORBA.TypeCode type() {
        IluTypeCode tc = this.theType;
        if (tc == null) {
            xerox.ilu.IluPickle pickle = this.thePickle;
            if (pickle != null) {
               tc = pickle.type();
               condAssignType(tc, pickle);
            }
        }
        return tc;
    } //type   


    public boolean equal(org.omg.CORBA.Any a) {
        if (this == a) {
            return true;
        }
        org.omg.CORBA.TypeCode otc = a.type();
        org.omg.CORBA.TypeCode ttc = this.type();
        if (otc != ttc) {
            return false;
        }
        //java.lang.Object t = this.value();
        //java.lang.Object o = a.value();
        //if (t==null) {
        //    return (o==null);
        //}
        //return t.equals(o);
        throw new org.omg.CORBA.NO_IMPLEMENT();
    } //equal


    /** 
     * CORBA FUNCTION; Not recommended for ILU style programmers.
     */
    public void type(org.omg.CORBA.TypeCode t) 
            throws org.omg.CORBA.SystemException {
        assign3(t, null, null);
    } //type   
 

    
    /** 
     * Returns a Java value for the value contained in the IluAny.
     * Returns instances of Java wrapper classes instead of Java 
     * basic types.
     * Not CORBA.
     */
    public java.lang.Object value() 
            throws org.omg.CORBA.SystemException
    {
        java.lang.Object v = this.theVal;
        if (v == null) { 
            xerox.ilu.IluPickle pickle = this.thePickle;
            if (pickle != null) {
                v = pickle.value(null);
                condAssignObject(v, pickle);
            }
        }
        return v;
    } //value


    /** 
     * Returns a Java value for the value contained in the IluAny.
     * Returns instances of Java wrapper classes instead of Java 
     * basic types. May use type to convert to value of right type. 
     * Not CORBA.
     */
    public java.lang.Object value(IluTypeCode type) 
            throws org.omg.CORBA.SystemException
    {
        java.lang.Object v = this.theVal;
        if (v == null) { 
            xerox.ilu.IluPickle pickle = this.thePickle;
            if (pickle != null) {
                v = pickle.value(type);
                condAssignObject(v, pickle);
            }
        }
        return v;
    } //value


    /** 
     * The formal value input argument needs to have the Java class
     * or subclass for the mapping of the IDL type.  If an input
     * argument is mapped to a Java basic type, this procedure will
     * accept the corresponding Java wrapper class.
     * Not CORBA.
     */ 
    public static IluAny alloc(
            IluTypeCode type, 
            java.lang.Object value) 
            throws org.omg.CORBA.SystemException
    {
        IluAny a = new IluAny();
        a.theType = type;
        a.theVal = value;
        a.thePickle = null;
        return a;
    } //alloc
    
    /** 
     * Not recommended to clients.
     * Not CORBA.
     */
    public void assign(IluTypeCode type, java.lang.Object value) 
            throws org.omg.CORBA.SystemException
    {
        assign3(type, value, null);
    } //assign
         
    
    /** Off-limits to applications; accessed by stubs */
    public void _outAny(xerox.ilu.IluCall call) 
             throws org.omg.CORBA.SystemException
    {
        xerox.ilu.IluPickle pickle = getPickle();
        pickle._out(call);
    } //_outAny
    
           
    /** Off-limits to applications; accessed by stubs */
    public int _szAny(xerox.ilu.IluCall call) 
             throws org.omg.CORBA.SystemException
    {
        xerox.ilu.IluPickle pickle = getPickle();
        return pickle._sz(call);
    } //_szAny
    
    
    /** Off-limits to applications; accessed by stubs */
    public static IluAny _inAny(xerox.ilu.IluCall call) 
             throws org.omg.CORBA.SystemException
    {
        xerox.ilu.IluPickle pickle = xerox.ilu.IluPickle._in(call);
        IluAny a = new IluAny();
        a.thePickle = pickle;
        return a;
    } //_inAny
        
      
    /* 
     * anticipated stubber generated code in helper classes
     *
     *
     * public static XXX extract(IluAny _any) throws ... {
     *     java.lang.Object _ob = _any.cachedValue();
     *     if (_ob == null) {_ob = _any.value(typecode);} 
     *     return (XXX) _ob; 
     * } //extract
     *
     *
     * public static void insert(org.omg.CORBA.Any _any, XXX _x) throws ... {
     *     xerox.ilu.IluAny.assign(typecode, _x);
     * } //insert
     *
     */

    
    /*friendly*/ static org.omg.CORBA.BAD_OPERATION BadOp() {
        org.omg.CORBA.BAD_OPERATION e = 
            new org.omg.CORBA.BAD_OPERATION();
        return e; 
    } //BadOp


    /* 
     * Required standard types ... 
     */
    
    /** 
     * Extract an idl-short value, if it is stored in the IluAny. 
     */
    public short extract_short() throws org.omg.CORBA.BAD_OPERATION {
        java.lang.Object v = this.value();
        if (v instanceof java.lang.Short) {
            java.lang.Short s = (java.lang.Short) v;
            return s.shortValue();
        }
        throw BadOp(); 
    } //extract_short   

    /** 
     * Stores an idl-short value into the IluAny.
     */
    public void insert_short(short s) 
            throws org.omg.CORBA.SystemException {
        assign(IluTypeCode.corba_short(), new java.lang.Short(s));
    } //insert_short   
 
 
    /** 
     * Extract an idl-long (java-int) value, if it is stored in the IluAny. 
     */
    public int extract_long() throws org.omg.CORBA.BAD_OPERATION {
        java.lang.Object v = this.value();
        if (v instanceof java.lang.Integer) {
            java.lang.Integer i = (java.lang.Integer) v;
            return i.intValue();
        }
        throw BadOp();
    } //extract_long   

    /** 
     * Stores an idl-long (java-int) value into the IluAny.
     */
    public void insert_long(int i) 
            throws org.omg.CORBA.SystemException {
        assign(IluTypeCode.corba_long(), new java.lang.Integer(i));
    } //insert_long   
 
 
    /** 
     * Extract an idl-longlong (java-long) value, if it is 
     * stored in the IluAny. 
     */
    public long extract_longlong() throws org.omg.CORBA.SystemException {
        java.lang.Object v = this.value();
        if (v instanceof java.lang.Long) {
            java.lang.Long i = (java.lang.Long) v;
            return i.longValue();
        }
        throw BadOp();
    } //extract_longlong   

    /** 
     * Stores an idl-longlong (java-long) value into the IluAny.
     */
    public void insert_longlong(long i) 
            throws org.omg.CORBA.SystemException {
        assign(IluTypeCode.corba_longlong(), new java.lang.Long(i));
    } //insert_longlong   
 
 
    /** 
     * Extract an idl-ushort value, if it is stored in the IluAny. 
     */
    public short extract_ushort() throws org.omg.CORBA.SystemException {
        java.lang.Object v = this.value();
        if (v instanceof java.lang.Short) {
            java.lang.Short s = (java.lang.Short) v;
            return s.shortValue();
        }
        throw BadOp(); 
    } //extract_ushort   

    /** 
     * Stores an idl-ushort value into the IluAny.
     */
    public void insert_ushort(short s) 
            throws org.omg.CORBA.SystemException {
        assign(IluTypeCode.corba_ushort(), new java.lang.Short(s));
    } //insert_ushort   
 
 
    /** 
     * Extract an idl-ulong (java-int) value, if it is stored in 
     * the IluAny. 
     */
    public int extract_ulong() throws org.omg.CORBA.SystemException {
        java.lang.Object v = this.value();
        if (v instanceof java.lang.Integer) {
            java.lang.Integer i = (java.lang.Integer) v;
            return i.intValue();
        }
        throw BadOp();
    } //extract_ulong   

    /** 
     * Stores an idl-long (java-int) value into the IluAny.
     */
    public void insert_ulong(int i) 
            throws org.omg.CORBA.SystemException {
        assign(IluTypeCode.corba_ulong(), new java.lang.Integer(i));
    } //insert_long   
 
    /** 
     * Extract an idl-ulonglong (java-long) value, if it is 
     * stored in the IluAny. 
     */
    public long extract_ulonglong() throws org.omg.CORBA.SystemException {
        java.lang.Object v = this.value();
        if (v instanceof java.lang.Long) {
            java.lang.Long i = (java.lang.Long) v;
            return i.longValue();
        }
        throw BadOp();
    } //extract_ulonglong   

    /** 
     * Stores an idl-ulonglong (java-long) value into the IluAny.
     */
    public void insert_ulonglong(long i) 
            throws org.omg.CORBA.SystemException {
        assign(IluTypeCode.corba_ulonglong(), new java.lang.Long(i));
    } //insert_ulonglong   
 
 
    /** 
     * Extract an float value, if it is stored in the IluAny. 
     */
    public float extract_float() throws org.omg.CORBA.SystemException {
        java.lang.Object v = this.value();
        if (v instanceof java.lang.Float) {
            java.lang.Float i = (java.lang.Float) v;
            return i.floatValue();
        }
        throw BadOp();
    } //extract_float   

    /** 
     * Stores an float value into the IluAny.
     */
    public void insert_float(float i) 
            throws org.omg.CORBA.SystemException {
        assign(IluTypeCode.corba_float(), new java.lang.Float(i));
    } //insert_float   
 
 
    /** 
     * Extract an double value, if it is stored in the IluAny. 
     */
    public double extract_double() throws org.omg.CORBA.SystemException {
        java.lang.Object v = this.value();
        if (v instanceof java.lang.Double) {
            java.lang.Double i = (java.lang.Double) v;
            return i.doubleValue();
        }
        throw BadOp();
    } //extract_double   

    /** 
     * Stores an double value into the IluAny.
     */
    public void insert_double(double i) 
            throws org.omg.CORBA.SystemException {
        assign(IluTypeCode.corba_double(), new java.lang.Double(i));
    } //insert_double   
 
 
    /** 
     * Extract an boolean value, if it is stored in the IluAny. 
     */
    public boolean extract_boolean() throws org.omg.CORBA.SystemException {
        java.lang.Object v = this.value();
        if (v instanceof java.lang.Boolean) {
            java.lang.Boolean i = (java.lang.Boolean) v;
            return i.booleanValue();
        }
        throw BadOp();
    } //extract_boolean   

    /** 
     * Stores an boolean value into the IluAny.
     */
    public void insert_boolean(boolean i) 
            throws org.omg.CORBA.SystemException {
        assign(
            IluTypeCode.corba_boolean(), 
            (i ? java.lang.Boolean.TRUE : java.lang.Boolean.FALSE)
            );
    } //insert_boolean   
 
    /** 
     * Extract an char value, if it is stored in the IluAny. 
     */
    public char extract_char() throws org.omg.CORBA.SystemException {
        java.lang.Object v = this.value();
        if (v instanceof java.lang.Character) {
            java.lang.Character i = (java.lang.Character) v;
            return i.charValue();
        }
        throw BadOp();
    } //extract_char   

    /** 
     * Stores an char value into the IluAny.
     */
    public void insert_char(char i) 
            throws org.omg.CORBA.SystemException {
        assign(IluTypeCode.corba_char(), new java.lang.Character(i));
    } //insert_char   
 

    /** 
     * Extract an wchar value, if it is stored in the IluAny. 
     */
    public char extract_wchar() throws org.omg.CORBA.SystemException {
        java.lang.Object v = this.value();
        if (v instanceof java.lang.Character) {
            java.lang.Character i = (java.lang.Character) v;
            return i.charValue();
        }
        throw BadOp();
    } //extract_wchar   

    /** 
     * Stores an wchar value into the IluAny.
     */
    public void insert_wchar(char i) 
            throws org.omg.CORBA.SystemException {
        assign(IluTypeCode.corba_char(), new java.lang.Character(i));
    } //insert_wchar   
 

    /** 
     * Extract an octet value, if it is stored in the IluAny. 
     */
    public byte extract_octet() throws org.omg.CORBA.SystemException {
        java.lang.Object v = this.value();
        if (v instanceof java.lang.Byte) {
            java.lang.Byte b = (java.lang.Byte) v;
            return b.byteValue();
        }
        throw BadOp();
    } //extract_octet   

    /** 
     * Stores an octet value into the IluAny.
     */
    public void insert_octet(byte i) 
            throws org.omg.CORBA.SystemException {
        assign(IluTypeCode.corba_octet(), new java.lang.Byte(i));
    } //insert_octet   
 

    /** 
     * Extract an IluAny value from an IluAny, if it is stored in the IluAny. 
     */
    public IluAny extract_IluAny() throws org.omg.CORBA.SystemException {
        java.lang.Object v = this.value();
        if (v instanceof IluAny) return (IluAny) v;
        throw BadOp();
    } //extract_IluAny   

    /** 
     * Stores an IluAny value into the IluAny.
     */
    public void insert_IluAny(IluAny a) throws org.omg.CORBA.SystemException {
        assign(IluTypeCode.corba_any(), a);
    } //insert_IluAny   
 
 
    /** 
     * Extract an org.omg.CORBA.Object value from an IluAny, 
     * if it is stored in the IluAny. 
     */
    public org.omg.CORBA.Object extract_Object() 
            throws org.omg.CORBA.BAD_OPERATION {
        java.lang.Object v = this.value();
        if (v instanceof org.omg.CORBA.Object) {
            return (org.omg.CORBA.Object) v;
        }
        throw BadOp();
    } //extract_Object   

    /** 
     * Stores an CORBA.Object value into the IluAny.
     */
    public void insert_Object(org.omg.CORBA.Object obj) {
        assign(IluTypeCode.corba_objref(), obj);
    } //insert_Object   

    public void insert_Object(
        org.omg.CORBA.Object obj, 
        org.omg.CORBA.TypeCode t) throws org.omg.CORBA.BAD_OPERATION {
        assign((IluTypeCode) t, obj);
    } //insert_Object   

    public org.omg.CORBA.TypeCode extract_TypeCode() 
             throws org.omg.CORBA.BAD_OPERATION {
        throw new org.omg.CORBA.NO_IMPLEMENT();
    }
    
    public void insert_TypeCode(org.omg.CORBA.TypeCode t) {
        throw new org.omg.CORBA.NO_IMPLEMENT();
    }

    /** 
     * Extract an string value, if it is stored in the IluAny. 
     */
    public java.lang.String extract_string() 
            throws org.omg.CORBA.SystemException {
        java.lang.Object v = this.value();
        if (v instanceof java.lang.String) {
            return (java.lang.String) v;
        }
        throw BadOp();
    } //extract_string   

    /** 
     * Stores an string value into the IluAny.
     */
    public void insert_string(java.lang.String s) 
            throws org.omg.CORBA.SystemException {
        assign(IluTypeCode.corba_string(), s);
    } //insert_string   
 

    /** 
     * Extract an wstring value, if it is stored in the IluAny. 
     */
    public java.lang.String extract_wstring() 
            throws org.omg.CORBA.SystemException {
        java.lang.Object v = this.value();
        if (v instanceof java.lang.String) {
            return (java.lang.String) v;
        }
        throw BadOp();
    } //extract_wstring   


    /** 
     * Stores an wstring value into the IluAny.
     */
    public void insert_wstring(java.lang.String s) 
            throws org.omg.CORBA.SystemException {
        assign(IluTypeCode.corba_wstring(), s);
    } //insert_wstring   
    

    /* TypeCode is not a first class data type in ILU  */
    /* Principal is depreciated by omg; use Passports in ILU */
    public org.omg.CORBA.Principal extract_Principal() throws org.omg.CORBA.BAD_OPERATION {
        throw new org.omg.CORBA.NO_IMPLEMENT();
    }
    
    public void insert_Principal(org.omg.CORBA.Principal p) {
        throw new org.omg.CORBA.NO_IMPLEMENT();
    }
    
    public void read_value(
        org.omg.CORBA.portable.InputStream is, 
        org.omg.CORBA.TypeCode t) throws org.omg.CORBA.MARSHAL
    {
        throw new org.omg.CORBA.NO_IMPLEMENT();
    }

    public void write_value(org.omg.CORBA.portable.OutputStream os) {
        throw new org.omg.CORBA.NO_IMPLEMENT();
    }
    
    public org.omg.CORBA.portable.OutputStream create_output_stream() {
        throw new org.omg.CORBA.NO_IMPLEMENT();
    }
    
    public org.omg.CORBA.portable.InputStream create_input_stream() {
        throw new org.omg.CORBA.NO_IMPLEMENT();
    }
    
    public void insert_Streamable(org.omg.CORBA.portable.Streamable s) {
        throw new org.omg.CORBA.NO_IMPLEMENT();
    }


    public org.omg.CORBA.Any extract_any() throws org.omg.CORBA.BAD_OPERATION {
        throw new org.omg.CORBA.NO_IMPLEMENT();
    }
    
    public void insert_any(org.omg.CORBA.Any a) {
        throw new org.omg.CORBA.NO_IMPLEMENT();
    }
    
    /** to make this Externalizable */ 
    public void readExternal(java.io.ObjectInput in) 
            throws java.io.IOException, java.lang.ClassNotFoundException
    {
        java.lang.Object ob1;
        IluTypeCode tc1;
        ob1 = in.readObject();
        tc1 = (IluTypeCode) in.readObject();
        this.assign3(tc1, ob1, null);
    } //readExternal
    
    /** to make this Externalizable */ 
    public synchronized void writeExternal(java.io.ObjectOutput out) 
            throws java.io.IOException
    {
        java.lang.Object ob1 = this.value();
        IluTypeCode tc1 = (IluTypeCode) this.type();
        out.writeObject(ob1);
        out.writeObject(tc1);
    } //writeExternal
    
    static final long serialVersionUID = 2597255393105692919L;
     
} //IluAny




