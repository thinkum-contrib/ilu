/*
 * Class for corba compatibility
 * contents is not corba compatible.
 */
/* StructMember.java */
/* Chris Jacobi, November 16, 1998 12:57 pm PST */
/* $Id: StructMember.java,v 1.1 1998/11/17 07:06:25 jacobi Exp $ */

package org.omg.CORBA;

public final class StructMember {
    //	instance variables
    public String name;
    public org.omg.CORBA.TypeCode type;
    public org.omg.CORBA.IDLType type_def;

    public StructMember() {
    } //constructor
    
    public StructMember(String __name, org.omg.CORBA.TypeCode __type, org.omg.CORBA.IDLType __type_def) {
	name = __name;
	type = __type;
	type_def = __type_def;
    } //constructor
    
} //StructMember
