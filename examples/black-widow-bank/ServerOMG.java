/* $Id: ServerOMG.java,v 1.2 1999/08/03 01:58:34 janssen Exp $
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
// Chris Jacobi, February 15, 1999 3:38 pm PST

import java.util.*;

class AccountOMG extends Bank._AccountImplBase {
  AccountOMG(float balance) {
    _balance = balance;
  }
  public float balance() throws org.omg.CORBA.SystemException {
    return _balance;
  }
  private float _balance;
} //AccountOMG


class AccountManagerOMG extends Bank._AccountManagerImplBase {
  AccountManagerOMG(String name) {
  }
  public Bank.Account open(String name) 
          throws org.omg.CORBA.SystemException {
    // Lookup the account in the account dictionary.
    Bank.Account account = (Bank.Account) _accounts.get(name);
    if (account == null) {
      // Create a new account with between 0 and 1000 dollars.
      float balance = Math.abs(_random.nextInt()) % 100000 / 100f;
      account = new AccountOMG(balance);
      // Export the new object reference.
      ServerOMG.orb.connect(account);
      System.out.println("Created " + name + "'s account: " + account);
      // Save the account in the account dictionary.
      _accounts.put(name, account);
    }
    // Return the account.
    return account;
  }
  private Dictionary _accounts = new Hashtable();
  private Random _random = new Random();
} //AccountManagerOMG


public class ServerOMG {
  
  public static org.omg.CORBA.ORB orb = null;
  
  public static void main(String[] args) {
    try {
      // Initialize the ORB.
      orb = org.omg.CORBA.ORB.init(args, null);
      // Create the account manager object.
      AccountManagerOMG manager = new AccountManagerOMG("Post-Modern Bank");
      // Export the newly create object.
      orb.connect(manager);      
      //print its ior
      System.out.println(orb.object_to_string(manager));
      // wait for invocations from clients
      java.lang.Object sync = new java.lang.Object();
      synchronized (sync) {
          sync.wait();
      }    
    } catch (Exception e) {
      System.err.println(e);
      e.printStackTrace(System.err);
    }
  }
  
} //ServerOMG


