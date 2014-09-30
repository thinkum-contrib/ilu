/*
 * class required for corba compatibility.
 */
/* Streamable.java */
/* Chris Jacobi, November 16, 1998 12:39 pm PST */
/* $Id: Streamable.java,v 1.1 1998/11/17 06:46:43 jacobi Exp $ */

package org.omg.CORBA.portable;

public interface Streamable {

    void _read(org.omg.CORBA.portable.InputStream istream);
    void _write(org.omg.CORBA.portable.OutputStream ostream);
    org.omg.CORBA.TypeCode _type();
    
} //Streamable
