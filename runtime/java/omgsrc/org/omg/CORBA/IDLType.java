/*
 * Class for corba compatibility
 * contents is not corba compatible.
 */
/* StructMember.java */
/* Chris Jacobi, November 16, 1998 12:59 pm PST */
/* $Id: IDLType.java,v 1.1 1998/11/17 07:06:25 jacobi Exp $ */

package org.omg.CORBA;

interface IDLType extends org.omg.CORBA.Object {
    
    public org.omg.CORBA.TypeCode type();
    
} //IDLType
