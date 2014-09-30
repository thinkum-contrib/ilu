/*
 * class required for corba compatibility.
 */
/* InputStream.java */
/* Chris Jacobi, November 16, 1998 12:40 pm PST */
/* $Id: InputStream.java,v 1.1 1998/11/17 06:47:42 jacobi Exp $ */
package org.omg.CORBA.portable;

import org.omg.CORBA.TypeCode;
import org.omg.CORBA.Principal;
import org.omg.CORBA.Any;

public abstract class InputStream extends java.io.InputStream
{
    public abstract boolean	read_boolean();
    public abstract char	read_char();
    public abstract char	read_wchar();
    public abstract byte	read_octet();
    public abstract short	read_short();
    public abstract short	read_ushort();
    public abstract int		read_long();
    public abstract int		read_ulong();
    public abstract long	read_longlong();
    public abstract long	read_ulonglong();
    public abstract float	read_float();
    public abstract double	read_double();
    public abstract String	read_string();
    public abstract String	read_wstring();

    public abstract void	read_boolean_array(boolean[] value, int offset, int length);
    public abstract void	read_char_array(char[] value, int offset, int length);
    public abstract void	read_wchar_array(char[] value, int offset, int length);
    public abstract void	read_octet_array(byte[] value, int offset, int length);
    public abstract void	read_short_array(short[] value, int offset, int length);
    public abstract void	read_ushort_array(short[] value, int offset, int length);
    public abstract void	read_long_array(int[] value, int offset, int length);
    public abstract void	read_ulong_array(int[] value, int offset, int length);
    public abstract void	read_longlong_array(long[] value, int offset, int length);
    public abstract void	read_ulonglong_array(long[] value, int offset, int length);
    public abstract void	read_float_array(float[] value, int offset, int length);
    public abstract void	read_double_array(double[] value, int offset, int length);

    public abstract org.omg.CORBA.Object read_Object();
    public abstract TypeCode	read_TypeCode();
    public abstract Any		read_any();
    public abstract Principal	read_Principal();


    public int read() throws java.io.IOException {
        throw new org.omg.CORBA.NO_IMPLEMENT();
    }

    public java.lang.Number read_fixednum() {
        throw new org.omg.CORBA.NO_IMPLEMENT();
    }

    public void read_fixednum_array(java.lang.Number[] value,
                                    int offset, int length) {
        throw new org.omg.CORBA.NO_IMPLEMENT();
    }

    /*
     * The following methods were added by orbos/98-03-16: Java to IDL
     * Mapping. These are used by RMI over IIOP.
     */

    /**
     * read_Object unmarshals an object and returns a CORBA Object
     * which is an instance of the class passed as its argument.
     * This class is the stub class of the expected type.
     */
    public  org.omg.CORBA.Object read_Object(java.lang.Class
						     clz) {
	throw new org.omg.CORBA.NO_IMPLEMENT();
    }

    /**
     * read_Value unmarshals and returns a value object.
     */
    public  java.io.Serializable read_Value() {
	throw new org.omg.CORBA.NO_IMPLEMENT();
    }

    /**
     * read_AbstractObject unmarshals and returns either a value
     * object or a remote object reference.
     */
    public
	java.lang.Object read_AbstractObject(java.lang.Class clz) {
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

}
