/* $Id: ServerVisi.java,v 1.4 1999/08/03 01:58:33 janssen Exp $
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
/* Last edited by Mike Spreitzer October 9, 1998 1:29 pm PDT */

import java.util.*;

class Account extends Bank._sk_Account {
  Account(float balance) {
    _balance = balance;
  }
  public float balance() throws CORBA.SystemException {
    return _balance;
  }
  private float _balance;
}

class AccountManager extends Bank._sk_AccountManager {
  AccountManager(String name) {
    super(name);
  }
  public Bank.Account open(String name) throws CORBA.SystemException {
    // Lookup the account in the account dictionary.
    Bank.Account account = (Bank.Account) _accounts.get(name);
    if(account == null) {
      // Create a new account with between 0 and 1000 dollars.
      float balance = Math.abs(_random.nextInt()) % 100000 / 100f;
      account = new Account(balance);
      System.out.println("Created " + name + "'s account: " + account);
      // Export the new object reference.
      CORBA.ORB.init().BOA_init().obj_is_ready(account);
      // Save the account in the account dictionary.
      _accounts.put(name, account);
    }
    // Return the account.
    return account;
  }
  private Dictionary _accounts = new Hashtable();
  private Random _random = new Random();
}

public class ServerVisi {
  public static void main(String[] args) {
    try {
      // Initialize the ORB.
      CORBA.ORB orb = CORBA.ORB.init();
      // Initialize the BOA.
      CORBA.BOA boa = orb.BOA_init();
      // Create the account manager object.
      AccountManager manager = 
	new AccountManager("Post-Modern Bank");
      // Export the newly create object.
      boa.obj_is_ready(manager);
      System.out.println(orb.object_to_string(manager));
      // Wait for incoming requests
      boa.impl_is_ready();
    }
    catch(CORBA.SystemException e) {
      System.err.println(e);
    }
  }
}

