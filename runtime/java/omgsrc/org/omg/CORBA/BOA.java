/*
 BeginILUCopyright
 
 Copyright (c) 1991-1999 Xerox Corporation.  All Rights Reserved.
 
 Unlimited use, reproduction, modification, and distribution of this
 software and modified versions thereof is permitted.  Permission is
 granted to make derivative works from this software or a modified
 version thereof.  Any copy of this software, a modified version
 thereof, or a derivative work must include both the above copyright
 notice of Xerox Corporation and this paragraph.  Any distribution of
 this software, a modified version thereof, or a derivative work must
 comply with all applicable United States export control laws.  This
 software is made available AS IS, and XEROX CORPORATION DISCLAIMS ALL
 WARRANTIES, EXPRESS OR IMPLIED, INCLUDING WITHOUT LIMITATION THE
 IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 PURPOSE, AND NOTWITHSTANDING ANY OTHER PROVISION CONTAINED HEREIN, ANY
 LIABILITY FOR DAMAGES RESULTING FROM THE SOFTWARE OR ITS USE IS
 EXPRESSLY DISCLAIMED, WHETHER ARISING IN CONTRACT, TORT (INCLUDING
 NEGLIGENCE) OR STRICT LIABILITY, EVEN IF XEROX CORPORATION IS ADVISED
 OF THE POSSIBILITY OF SUCH DAMAGES.
 
 EndILUCopyright
*/
/* BOA.java */
/* Chris Jacobi, November 16, 1998 6:41 pm PST */
/* $Id: BOA.java,v 1.7 1999/08/03 01:55:05 janssen Exp $ */

 
package org.omg.CORBA;

/**
 * I apologize, but I have no clue what a BOA is, or what it is for,
 * or, whom it constricts.<p>
 * I'm implementing this to make Ilu look more compatible to CORBA...
 */
public class BOA {
        
  private org.omg.CORBA.ORB orb = null;
  
  /** constructor */
  public BOA() {
  } //constructor
  
  /** constructor */
  public BOA(org.omg.CORBA.ORB orb) {
      this.orb = orb;
  } //constructor
  
  public void obj_is_ready(org.omg.CORBA.Object ob) 
          throws org.omg.CORBA.SystemException
  {
      if (this.orb == null) {
          this.orb = org.omg.CORBA.ORB.init();
      }
      this.orb.connect(ob);
  } //obj_is_ready
  
  public void impl_is_ready() 
  {
      while (true) {
          try {
              java.lang.Thread.sleep(20000);
          } catch (java.lang.InterruptedException ie) {
              return;
          }
      }
  } //impl_is_ready
  
} //BOA
