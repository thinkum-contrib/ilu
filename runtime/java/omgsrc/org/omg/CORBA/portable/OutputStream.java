/*
 * class required for corba compatibility.
 */
/* OuputStream.java */
/* Chris Jacobi, November 16, 1998 12:40 pm PST */
/* $Id: OutputStream.java,v 1.1 1998/11/17 06:47:42 jacobi Exp $ */

package org.omg.CORBA.portable;

import org.omg.CORBA.TypeCode;
import org.omg.CORBA.Principal;
import org.omg.CORBA.Any;

public abstract class OutputStream extends java.io.OutputStream
{
    public abstract InputStream create_input_stream();

    public abstract void write_boolean(boolean value);
    public abstract void write_char(char value);
    public abstract void write_wchar(char value);
    public abstract void write_octet(byte value);
    public abstract void write_short(short value);
    public abstract void write_ushort(short value);
    public abstract void write_long(int value);
    public abstract void write_ulong(int value);
    public abstract void write_longlong(long value);
    public abstract void write_ulonglong(long value);
    public abstract void write_float(float value);
    public abstract void write_double(double value);
    public abstract void write_string(String value);
    public abstract void write_wstring(String value);

    public abstract void write_boolean_array(boolean[] value, int offset,
								int length);
    public abstract void write_char_array(char[] value, int offset,
								int length);
    public abstract void write_wchar_array(char[] value, int offset,
								int length);
    public abstract void write_octet_array(byte[] value, int offset,
								int length);
    public abstract void write_short_array(short[] value, int offset,
								int length);
    public abstract void write_ushort_array(short[] value, int offset,
								int length);
    public abstract void write_long_array(int[] value, int offset,
								int length);
    public abstract void write_ulong_array(int[] value, int offset,
								int length);
    public abstract void write_longlong_array(long[] value, int offset,
								int length);
    public abstract void write_ulonglong_array(long[] value, int offset,
								int length);
    public abstract void write_float_array(float[] value, int offset,
								int length);
    public abstract void write_double_array(double[] value, int offset,
								int length);

    public abstract void write_Object(org.omg.CORBA.Object value);
    public abstract void write_TypeCode(TypeCode value);
    public abstract void write_any(Any value);
    public abstract void write_Principal(Principal value);


    public void write(int b) throws java.io.IOException {
        throw new org.omg.CORBA.NO_IMPLEMENT();
    }

    public void write_fixednum(java.lang.Number value) {
        throw new org.omg.CORBA.NO_IMPLEMENT();
    }

    public void write_fixednum_array(java.lang.Number[] value,
				     int offset, int length) {
        throw new org.omg.CORBA.NO_IMPLEMENT();
    }


    /*
     * The following methods were added by orbos/98-03-16: Java to IDL
     * Mapping. These are used by RMI over IIOP.
     */

    public  void write_Value(java.io.Serializable value) {
	throw new org.omg.CORBA.NO_IMPLEMENT();
    }

    /**
     * get_offset returns the current offset in the internal buffer.
     * This value can later be used to mark references to the same
     * object.
     *
     * @return the offset in the internal buffer
     */
    public  int get_offset() {
	throw new org.omg.CORBA.NO_IMPLEMENT();
    }

    /**
     * start_block and end_block methods are used to delimit a block
     * of data to be sent as GIOP chunked data. The actual chunking
     * should be handled within the OutputStream and it can
     * arbitrarily divide the higher-level block into chunks as needed.
     * start_block/end_block pairs may be arbitrarily nested.
     */
    public  void start_block() {
	throw new org.omg.CORBA.NO_IMPLEMENT();
    }

    /**
     * start_block and end_block methods are used to delimit a block
     * of data to be sent as GIOP chunked data. The actual chunking
     * should be handled within the OutputStream and it can
     * arbitrarily divide the higher-level block into chunks as needed.
     * start_block/end_block pairs may be arbitrarily nested.
     */
    public  void end_block() {
	throw new org.omg.CORBA.NO_IMPLEMENT();
    }

    /**
     * Return the ORB that created this OutputStream
     */
    public org.omg.CORBA.ORB orb() {
	throw new org.omg.CORBA.NO_IMPLEMENT();
    }
    
}
