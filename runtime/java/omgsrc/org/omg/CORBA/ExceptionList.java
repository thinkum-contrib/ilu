/*
 * Class for corba compatibility.
 */
/* ExceptionList.java */
/* Chris Jacobi, November 16, 1998 1:11 pm PST */
/* $Id: ExceptionList.java,v 1.1 1998/11/17 07:06:25 jacobi Exp $ */

package org.omg.CORBA;
public abstract class ExceptionList {

  public abstract int count();
  public abstract void add(TypeCode exc);
  public abstract TypeCode item(int index) throws org.omg.CORBA.Bounds;
  public abstract void remove(int index) throws org.omg.CORBA.Bounds;
  
} //ExceptionList
