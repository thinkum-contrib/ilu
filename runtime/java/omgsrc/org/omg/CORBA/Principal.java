/*
 * @class required for corba compatibility.
 */
/* Principal.java */
/* Chris Jacobi, November 16, 1998 12:42 pm PST */
/* $Id: Principal.java,v 1.1 1998/11/17 07:06:25 jacobi Exp $ */

package org.omg.CORBA;

public abstract class Principal {

    public abstract void name(byte[] value);
    public abstract byte[] name();
    
} //Principal
