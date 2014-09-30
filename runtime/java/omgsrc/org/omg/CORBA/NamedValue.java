/*
 * class required for corba compatibility.
 * CONTENTS IS NOT CORBA COMPLIANT.
 */
/* NamedValue.java */
/* Chris Jacobi, November 16, 1998 11:27 am PST */
/* $Id: NamedValue.java,v 1.1 1998/11/17 07:04:54 jacobi Exp $ */

package org.omg.CORBA;


public class NamedValue {

  public String name() {
        throw new org.omg.CORBA.NO_IMPLEMENT();
  }
  
  public Any value() {
        throw new org.omg.CORBA.NO_IMPLEMENT();
  }
  
  public int flags() {
        throw new org.omg.CORBA.NO_IMPLEMENT();
  }

} //NamedValue
