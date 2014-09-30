/** $Id: server3.cc,v 1.3 1999/08/03 01:58:58 janssen Exp $
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
/* Last edited by Mike Spreitzer October 9, 1998 10:40 am PDT */

///////////////////////////////////////////////////////////////////////
//
// File Name: server3.cc
//
// Contents: Source file for the factory and bank implementation of 
//           step3 of the Progressive Sample Applications
//
//  ----------------------------------------------------------------------
// !  Copyright Hewlett-Packard Company 1992-1996.  All Rights Reserved.  !
// !  An unpublished and CONFIDENTIAL work.  Reproduction, adaptation, or !
// !  translation without prior written permission is prohibited except   !
// !  as allowed under the copyright laws.                                !
//  ----------------------------------------------------------------------
//
//////////////////////////////////////////////////////////////////////

#include <string.h>
#include <iostream.h>
#include <fstream.h>
#include <NamingTypes.hh>
#include <bank3Server.hh>
#include <msd/sighandler.hh>

#ifdef WIN32
#include <string.h>

// Work around for bug in MS VC++ 2.2 compiler, See Note ?? in readme
static CORBA::HPSOA_var  my_soa     = CORBA_HPSOA::_nil();
#else
static CORBA::HPSOA_var  my_soa     = CORBA::HPSOA::_nil();
#endif

///////////////////////////////////////////////////////////////////
//
//  sighandler
//     It is used by the user as a signal to shutdown the HPSOA and 
//     exit.
//
static void
sighandler(int)
{
    cout << "Shutting down the HPSOA..." << endl;

    try {
	my_soa->shutdown();
    } catch (const CORBA::Exception&) {
	cerr << "HPSOA::shutdown failed." << endl;
    }
}

///////////////////////////////////////////////////////////////////
//
// AccountImpl : The implementation of BankApp's Account interface
//

// Work around for bug in MS VC++ 2.2 compiler, See Note ?? in readme
#ifdef WIN32
class AccountImpl : public virtual HPSOA_BankApp_Account
#else
class AccountImpl : public virtual HPSOA_BankApp::Account
#endif
{

  public:
                  AccountImpl(
                      const char *account_id,
                      float account_balance
                  );
    
    virtual       ~AccountImpl();

    virtual char* ID();

    virtual CORBA::Float balance();

  private:

    char *account_id;
    float account_balance;

};    

///////////////////////////////////////////////////////////////////
//
// AccountImpl constructor
//    This constructor is to set the Account ID and balance. 
//
// 
AccountImpl::AccountImpl(
    const char *acct_id,
    float acct_balance
)
{
    account_id = new char[strlen(acct_id) + 1];

    strcpy(account_id, acct_id);

    account_balance = acct_balance;
}

///////////////////////////////////////////////////////////////////
//
// AccountImpl's destructor
//
AccountImpl::~AccountImpl()
{
    delete [] account_id;
}

///////////////////////////////////////////////////////////////////
//
//  AccountImpl::ID
//     This method returns the Account ID the caller.
//
char* 
AccountImpl::ID()
{
    //
    // No checking for a NULL account_id here because create_account 
    // has already checked for a NULL account_id
    //

    return CORBA::string_dup(account_id);
}

///////////////////////////////////////////////////////////////////
//
//  AccountImpl::balance
//     This method returns the Account balance to the caller.
//
CORBA::Float
AccountImpl::balance()
{
    //
    // No checking for a zero balance here because create_account 
    // has already checked for a zero balance
    //

    CORBA::Float the_balance = account_balance;

    return the_balance;
}

///////////////////////////////////////////////////////////////////
//
// BankImpl : The implementation of BankApp's Bank interface
//

// Work around for bug in MS VC++ 2.2 compiler, See Note ?? in readme
#ifdef WIN32
class BankImpl : public HPSOA_BankApp_Bank
#else
class BankImpl : public HPSOA_BankApp::Bank
#endif
{

  public:
                  BankImpl(
                      const char *b_name
                  );
    
    virtual       ~BankImpl();

    virtual char* name();
    
    virtual BankApp::Account_ptr create_account(
				     const char* ID,
				     CORBA::Float deposit
                  );

  private:

    char* my_name;
    
};

///////////////////////////////////////////////////////////////////
//
// BankImpl constructor
//    This constructor is to set the Bank's name. 
//
// 
BankImpl::BankImpl(
    const char *b_name
)
{
    my_name = new char[strlen(b_name) + 1];

    strcpy(my_name, b_name);
}

///////////////////////////////////////////////////////////////////
//
// BankImpl's destructor
//
BankImpl::~BankImpl()
{
    delete [] my_name;
}

///////////////////////////////////////////////////////////////////
//
//  BankImpl::name
//     This method returns the Bank's name to the caller.
//
char* 
BankImpl::name()
{
    //
    // No checking for a NULL bank name here becuase create_bank has 
    // already checked for a NULL bank name after  
    //

    return CORBA::string_dup(my_name);
}

///////////////////////////////////////////////////////////////////
//
//  BankImpl::create_account
//     This method creates a new instance of an Account object 
//     and returns it to the caller.
//
BankApp::Account_ptr 
BankImpl::create_account(
    const char* ID,
    CORBA::Float deposit
)
{
    //////////////////////////////////////////////////////////////
    // 
    //  Check whether the account ID received is NULL or 
    //  the balance received is zero
    //

    cout << "Checking whether the account ID is NULL..." << endl;

    if (ID == NULL) {
        cerr << "ERROR: The account ID is NULL." << endl;

// Work around for bug in MS VC++ 2.2 compiler, See Note ?? in readme
#ifdef WIN32
        return BankApp_Account::_nil();    
#else
        return BankApp::Account::_nil();    
#endif

    }
 
    cout << "Checking whether the initial deposit is zero..." << endl;

    if (deposit == 0.00) {
        cerr << "ERROR: The initial deposit is zero." << endl;

// Work around for bug in MS VC++ 2.2 compiler, See Note ?? in readme
#ifdef WIN32
        return BankApp_Account::_nil();    
#else
        return BankApp::Account::_nil();    
#endif

    }

    //////////////////////////////////////////////////////////////
    // 
    //  Declare an object implementation for the account
    //

    cout << "Declaring an object implementation for the account..." << endl;

    AccountImpl *my_account_impl = new AccountImpl(ID, deposit);
    if (my_account_impl == NULL) {
        cerr << "ERROR: new failed for AccountImpl(ID, deposit)." << endl;

// Work around for bug in MS VC++ 2.2 compiler, See Note ?? in readme
#ifdef WIN32
        return BankApp_Account::_nil();    
#else
        return BankApp::Account::_nil();    
#endif

    }

    //////////////////////////////////////////////////////////////
    // 
    // Return the account object reference to the client
    //

    cout << "Returning the account object reference to the client..." << endl;
    
    return my_account_impl->_this();
}

///////////////////////////////////////////////////////////////////
//
//  FactoryImpl class
//

// Work around for bug in MS VC++ 2.2 compiler, See Note ?? in readme
#ifdef WIN32
class FactoryImpl : public virtual HPSOA_BankApp_Factory
#else
class FactoryImpl : public virtual HPSOA_BankApp::Factory
#endif
{

  public:

    // Constructor, destructor, and data members are included by default ...

    BankApp::Bank_ptr create_bank(
                          const char* name
		      );

};

///////////////////////////////////////////////////////////////////
//
//  FactoryImpl::create_bank
//     This method creates a new instance of a Bank object and
//     returns it to the caller.  
//
BankApp::Bank_ptr
FactoryImpl::create_bank(
    const char* name
)
{
    //////////////////////////////////////////////////////////////
    // 
    //  Check whether the bank name received is NULL
    //

    cout << "Checking whether the bank name received is NULL..." << endl;

    if (name == NULL) {
        cerr << "ERROR: The bank name received is NULL." << endl;

// Work around for bug in MS VC++ 2.2 compiler, See Note ?? in readme
#ifdef WIN32
        return BankApp_Bank::_nil();    
#else
        return BankApp::Bank::_nil();    
#endif

    }

    //////////////////////////////////////////////////////////////
    // 
    //  Declare an object implementation for the bank 
    //

    cout << "Declaring an object implementation for the bank..." << endl;

    BankImpl *my_bank_impl = new BankImpl(name);
    if (my_bank_impl == NULL) {
        cerr << "ERROR: Factory failed to declare an object implementation for the bank..." << endl;

// Work around for bug in MS VC++ 2.2 compiler, See Note ?? in readme
#ifdef WIN32
        return BankApp_Bank::_nil();    
#else
        return BankApp::Bank::_nil();    
#endif

    }

    //////////////////////////////////////////////////////////////
    // 
    // Return the bank object reference to the client
    //

    cout << "Returning the bank object reference to the client..." << endl;
    
    return my_bank_impl->_this();
}

///////////////////////////////////////////////////////////////////
//
//  MAIN   
//          for server3 
//
//
int
main(
    int argc, 
    char *argv[]
) 
{
    ///////////////////////////////////////////////////////////////
    // 
    //  Initialize the ORB
    //

    cout << "Initializing the ORB..." << endl;

    CORBA::ORB_var my_orb;
    try {
	my_orb = CORBA::ORB_init(argc, argv, CORBA::HPORBid);
    } catch (const CORBA::Exception&) {
	cerr << "ERROR: Server failed to initialize the ORB." << endl;
	return 1;
    } 

    ///////////////////////////////////////////////////////////////    
    // 
    //  Initialize the HPSOA
    //

    cout << "Initializing the HPSOA..." << endl;
    
    try {
	my_soa = my_orb->HPSOA_init(argc, argv, CORBA::HPSOAid);
    } catch (const CORBA::Exception&) {
	cerr << "ERROR: Server failed to initialize the HPSOA." << endl;
	return 1;
    } 
	    
    ///////////////////////////////////////////////////////////////
    // 
    //  Declare an object implementation for the factory  
    //

    cout << "Declaring an object implementation for the factory..." << endl;

    FactoryImpl my_factoryImpl;

    ///////////////////////////////////////////////////////////////
    // 
    //  Obtain an object reference for the object implementation  
    //

    cout << "Obtaining an object reference for the object implementation..." << endl;

    BankApp::Factory_var factory_objref = my_factoryImpl._this();

    ///////////////////////////////////////////////////////////////
    // 
    // Get initial naming context reference 
    //

    cout << "Getting initial naming context reference..." << endl; 

    CORBA::Object_var obj;
    try {
	obj = my_orb->resolve_initial_references("NameService");
    } catch (const CORBA::Exception&) {
	cerr << "Error retrieving the initial NamingContext from ORB." << endl;
	return 1;
    }    
    if (CORBA::is_nil(obj)) {
	cerr << "Unable to retrieve the initial NamingContext from ORB." << endl;
	return 1;
    }    

// Work around for bug in MS VC++ 2.2 compiler, See Note ?? in readme
    CosNaming::NamingContext_var initial_nc_ref;
    try {
#ifdef WIN32
	initial_nc_ref = CosNaming_NamingContext::_narrow(obj);
#else
	initial_nc_ref = CosNaming::NamingContext::_narrow(obj);
#endif
    } catch (const CORBA::Exception&) {
	cerr << "Unable to narrow object ptr to initial NamingContext" << endl;
	return 1;
    }
    if (CORBA::is_nil(initial_nc_ref)) {
	cerr << "Unable to narrow object ptr to initial NamingContext" << endl;
	return 1;
    }

#if 0

    ///////////////////////////////////////////////////////////////
    // 
    //  Bind the factory reference to a name
    //

    cout << "Binding factory reference..." << endl;

    CosNaming::Name factory_name;
    factory_name.length(1);
    factory_name[0].id   = CORBA::string_dup("BankFactory");
    factory_name[0].kind = CORBA::string_dup(""); 

    CORBA::Environment ev;
    initial_nc_ref->bind(factory_name, factory_objref, ev);
    if (ev.exception() != 0) {
	cerr << "Binding the factory reference to a name failed." << endl;
	return 1;
    }

#endif

    cout << "IOR is " << my_orb->object_to_string(factory_objref) << endl;

    ///////////////////////////////////////////////////////////////
    // 
    //  Signal to be used to shutdown the HPSOA and exit   
    // 

    MSD_SigHandler::activate(MSD_SigSet::set_shutdown(), sighandler);
        
    ///////////////////////////////////////////////////////////////////
    //     
    //  Run the HPSOA. Factory now is ready to accept create_bank 
    //  request from a client.
    //

    cout << "Server accepting requests from a client for creating a bank..." << endl;

    try {
	my_soa->run();
    } catch (const CORBA::Exception&) {
	cerr << "HPSOA run failed." << endl;
	return 1;
    }

#if 0

    //////////////////////////////////////////////////////////////////
    //
    //  Unbind the factory reference
    //

    cout << "Unbinding the factory reference..." << endl;
    
    initial_nc_ref->unbind(factory_name, ev);
    if (ev.exception() != 0) {
	cerr << "Unbind failed." << endl;
	return 1;
    }
    
#endif

    //////////////////////////////////////////////////////////////////
    //
    // Terminate
    //
    
    cout << "Server completed successfully!" << endl;
    
    return 0;
}
