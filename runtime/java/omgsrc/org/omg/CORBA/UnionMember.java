/*
 * Class for corba compatibility
 */
/* UnionMember.java */
/* Chris Jacobi, November 16, 1998 1:03 pm PST */
/* $Id: UnionMember.java,v 1.1 1998/11/17 07:06:25 jacobi Exp $ */

package org.omg.CORBA;
public final class UnionMember {
    
    public String name;
    public org.omg.CORBA.Any label;
    public org.omg.CORBA.TypeCode type;
    public org.omg.CORBA.IDLType type_def;
    
    public UnionMember() {
    } //constructor
    
    public UnionMember(
            String __name, 
            org.omg.CORBA.Any __label, 
            org.omg.CORBA.TypeCode __type, 
            org.omg.CORBA.IDLType __type_def) {
	name = __name;
	label = __label;
	type = __type;
	type_def = __type_def;
    } //constructor
    
} //UnionMember
