# $Id: server.py,v 1.3 1999/08/03 01:58:58 janssen Exp $
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

import BankApp, ilu, BankApp__skel, rand

mainloopvar = ilu.CreateLoopHandle()

class realAccount (BankApp__skel.Account):

	def __init__(self, bank, id, balance):
		self.IluServer = bank.IluServer
		self.IluInstHandle = bank._get_name() + '.' + id
		self.id = id
		self.bank = bank
		self.bank_balance = balance

	def _get_ID (self):
		return self.id

	def _get_balance (self):
		return self.bank_balance

class realBank (BankApp__skel.Bank):

	def __init__(self, name, srvr=None):
		self.IluInstHandle = name
		self.IluServer = srvr
		self.accounts = {}

	def _get_name (self):
		return self.IluInstHandle

	def create_account (self, id, initial_balance):
		if not self.accounts.has_key(id):
			initial_balance = initial_balance
			account = realAccount (self, id, initial_balance)
			self.accounts[id] = account
		else:
			account = self.accounts[id]
		return account

class realFactory (BankApp__skel.Factory):

	def __init__(self, ih, server):
		self.IluInstHandle = ih
		self.IluServer = server
		self.banks = []

	def create_bank (self, name):
		newbank = realBank(name, self.IluServer)
		self.banks.append(newbank)	# hold onto it so it doesn't get GC'd
		return newbank


def main():

  instHandle = "Factory"
  serverID = "bank3.examples.ORBPlus.hp.com"

  s = ilu.CreateServer(serverID, ("tcp_0_0",), "iiop_1_0_1")
  uc = realFactory(instHandle, s)

  uc.IluPublish()

  print ilu.IOROfObject(uc)

  ilu.RunMainLoop(mainloopvar)

if __name__ == '__main__':
	main()
