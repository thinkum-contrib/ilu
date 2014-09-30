/*
 * class required for corba compatibility.
 * CONTENTS IS NOT CORBA COMPLIANT.
 */
/* NVList.java */
/* Chris Jacobi, November 16, 1998 11:32 am PST */
/* $Id: NVList.java,v 1.1 1998/11/17 07:04:54 jacobi Exp $ */

package org.omg.CORBA;

public class NVList {

  public int count() {
      throw new org.omg.CORBA.NO_IMPLEMENT();
  }

  public NamedValue add(int flags) {
      throw new org.omg.CORBA.NO_IMPLEMENT();
  }

  public NamedValue add_item(String item_name, int flags) {
      throw new org.omg.CORBA.NO_IMPLEMENT();
  }

  public NamedValue add_value(String item_name, Any val, int flags) {
      throw new org.omg.CORBA.NO_IMPLEMENT();
  }

  /*
  public NamedValue item(int index) throws org.omg.CORBA.Bounds {
  }
  */

  /*  
  public abstract void remove(int index) throws org.omg.CORBA.Bounds;
  */

} //NVList
