/*
 * Class for corba compatibility.
 */
/* Environment.java */
/* Chris Jacobi, November 16, 1998 1:09 pm PST */
/* $Id: Environment.java,v 1.1 1998/11/17 07:06:25 jacobi Exp $ */

package org.omg.CORBA;

public abstract class Environment {

  public abstract java.lang.Exception exception();
  public abstract void exception(java.lang.Exception except);
  public abstract void clear();

} //Environment
