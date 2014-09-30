/* $Id: ClientOMG.java,v 1.2 1999/08/03 01:58:34 janssen Exp $
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
// Chris Jacobi, February 15, 1999 3:19 pm PST
/* Last edited by Mike Spreitzer October 9, 1998 1:28 pm PDT */


public class ClientOMG {

  public static void main(String args[]) {
    try {
      // Initialize the ORB.
      org.omg.CORBA.ORB orb = org.omg.CORBA.ORB.init(args, null);
      // Locate an account manager.
      org.omg.CORBA.Object obj = orb.string_to_object(args[0]);
      Bank.AccountManager manager = Bank.AccountManagerHelper.narrow(obj);
      // use args[0] as the account name, or a default.
      String name = args.length > 1 ? args[1] : "Jack B. Quick";
      // Request the account manager to open a named account.
      Bank.Account account = manager.open(name);
      // Get the balance of the account.
      float balance = account.balance();
      // Print out the balance.
      System.out.println
	("The balance in " + name + "'s account is $" + balance);
    }
    catch(org.omg.CORBA.SystemException e) {
      System.err.println(e);
      e.printStackTrace(System.err);
    }
  }
}
