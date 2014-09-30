/*
 * Class for corba compatibility.
 */
/* ContextList.java */
/* Chris Jacobi, November 16, 1998 1:10 pm PST */
/* $Id: ContextList.java,v 1.1 1998/11/17 07:06:25 jacobi Exp $ */


package org.omg.CORBA;

public abstract class ContextList {

  public abstract int count();
  public abstract void add(String ctx);
  public abstract String item(int index) throws org.omg.CORBA.Bounds;
  public abstract void remove(int index) throws org.omg.CORBA.Bounds;

} //ContextList

