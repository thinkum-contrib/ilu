# $Id: bankServer.py,v 1.3 1999/08/03 01:58:31 janssen Exp $
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

import Bank, ilu, Bank__skel, rand

mainloopvar = ilu.CreateLoopHandle()

class realAccount (Bank__skel.Account):

	def __init__(self, manager, name, balance):
		self.manager = manager
		self.IluServer = manager.IluServer
		self.IluInstHandle = manager.IluInstHandle + ' - ' + name
		self.name = name
		self.bank_balance = balance

	def balance (self):
		return self.bank_balance

class realAccountManager (Bank__skel.AccountManager):

	def __init__(self, name, srvr=None):
		self.IluInstHandle = name
		self.IluServer = srvr
		self.accounts = {}

	def open (self, name):
		if not self.accounts.has_key(name):
			initial_balance = rand.rand() / 32.768
			account = realAccount (self, name, initial_balance)
			self.accounts[name] = account
		else:
			account = self.accounts[name]
		return account

def main():

  instHandle = "Post-Modern Bank"
  serverID = "bankserver.somedept.somecompany.com"

  s = ilu.CreateServer(serverID, ("tcp_0_0",), "iiop_1_0_1")
  uc = realAccountManager(instHandle, s)

  uc.IluPublish()

  print ilu.IOROfObject(uc)

  ilu.RunMainLoop(mainloopvar)

if __name__ == '__main__':
	main()
