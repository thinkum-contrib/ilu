/*
 * Interface for corba compatibility
 */
/* Policy.java */
/* Chris Jacobi, November 23, 1998 3:20 pm PST */
/* $Id: Policy.java,v 1.1 1998/11/23 23:21:54 jacobi Exp $ */


package org.omg.CORBA;

public interface Policy extends org.omg.CORBA.Object
{
    public int policy_type();

    public org.omg.CORBA.Policy copy();

    public void destroy();
    
} //Policy
