# $Id: client.py,v 1.4 1999/08/03 01:58:57 janssen Exp $
# BeginILUCopyright

# Copyright (c) 1991-1999 Xerox Corporation.  All Rights Reserved.

# Unlimited use, reproduction, modification, and distribution of this
# software and modified versions thereof is permitted.  Permission is
# granted to make derivative works from this software or a modified
# version thereof.  Any copy of this software, a modified version
# thereof, or a derivative work must include both the above copyright
# notice of Xerox Corporation and this paragraph.  Any distribution of
# this software, a modified version thereof, or a derivative work must
# comply with all applicable United States export control laws.  This
# software is made available AS IS, and XEROX CORPORATION DISCLAIMS ALL
# WARRANTIES, EXPRESS OR IMPLIED, INCLUDING WITHOUT LIMITATION THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
# PURPOSE, AND NOTWITHSTANDING ANY OTHER PROVISION CONTAINED HEREIN, ANY
# LIABILITY FOR DAMAGES RESULTING FROM THE SOFTWARE OR ITS USE IS
# EXPRESSLY DISCLAIMED, WHETHER ARISING IN CONTRACT, TORT (INCLUDING
# NEGLIGENCE) OR STRICT LIABILITY, EVEN IF XEROX CORPORATION IS ADVISED
# OF THE POSSIBILITY OF SUCH DAMAGES.

# EndILUCopyright

import BankApp, CORBA, sys

def do_test (factory, b_name, account_id, initial_deposit):

	print "Requesting factory to create a bank with name = " + b_name + "..."
	bank = factory.create_bank(b_name)

	print "Checking whether the bank (" + str(bank) + ") is NIL..."
	if not bank:
		print "ERROR:  Client received NIL bank object"
		sys.exit(1)

	print "Retrieving the bank's name..."
	a_name = bank._get_name()

	print "Comparing the bank's name (%s) with the specified name (%s)..." % (a_name, b_name)
	if a_name <> b_name:
		print "ERROR: The bank's name does not match the bank name requested by the client."
		sys.exit(1)

	print "Requesting the bank to create an account..." 
	account = bank.create_account (account_id, initial_deposit)

	print "Checking whether the account (" + str(account) + ") is NIL..."
	if not account:
		print "ERROR:  Client received NIL account object"
		sys.exit(1)

	print "Retrieving the account ID..."
	an_account_id = account._get_ID()

	print "Comparing the actual account ID (%s) with the specified account ID (%s)..." % (an_account_id, account_id)
	if an_account_id <> account_id:
		print "ERROR: The actual account ID oes not match the account ID requested by the client."
		sys.exit(1)

	print "Retrieving the balance..." 
	balance = account._get_balance()

	print "Comparing the balance (%f) with the initial deposit (%f)..." % (initial_deposit, balance)
	if initial_deposit <> balance:
		print "ERROR: The balance is not equal to the initial deposit."
		sys.exit(1)
	print "Client completed successfully!"
	sys.exit(0)

def main (argv):
	if len(argv) < 2:
		# Well, we assume that we're saving time by using the ILU simple binding
		# mechanism.  So, load ILU, and look up the object by name...
		import ilu
		factory = ilu.LookupObject("bank3.examples.ORBPlus.hp.com", "Factory", BankApp.Factory)
		if not factory:
			print 'Usage:  %s FACTORY-IOR'
			sys.exit(1)
		do_test(factory, 'ABank', '275-45-7483', 100.00)
	else:
		# get the CORBA ORB...
		orb = CORBA.ORB_init(argv, '')
		# using the ORB, convert the IOR string to an object reference...
		obj = orb.string_to_object(argv[1])
		# call the test routine with the object ref...
		do_test(obj, 'ABank', '275-45-7483', 100.00)

if __name__ == '__main__':
	main(sys.argv)
		
