/*
 * Interface for corba compatibility
 */
/* DomainManager.java */
/* Chris Jacobi, November 23, 1998 3:21 pm PST */
/* $Id: DomainManager.java,v 1.1 1998/11/23 23:21:41 jacobi Exp $ */


package org.omg.CORBA;


public interface DomainManager extends org.omg.CORBA.Object
{
    public org.omg.CORBA.Policy get_domain_policy(int policy_type);
} //DomainManager


