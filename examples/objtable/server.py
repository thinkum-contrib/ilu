# $Id: server.py,v 1.5 1999/08/03 01:58:04 janssen Exp $
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

import sys

if '-mt' in sys.argv:
	import thread
	import ilu
	ilu.ThreadedOperation()
else:
	import ilu

import objtable, objtable__skel

class realFile(objtable__skel.file):
	def __init__(self, filename, ih=None, server=None):
		self.IluServer = server
		self.IluInstHandle = ih or filename
		self.filename = filename

	def name (self):
		return self.filename

class realServer(objtable__skel.server):
	def __init__(self, ih=None, server=None):
		self.IluServer = server
		self.IluInstHandle = ih or filename

	def find_file(self, name):
		return realFile(name, name, self.IluServer)

loopvar = ilu.CreateLoopHandle();

theServer = None

def createInstance (ih):
	return realFile(ih, ih, theServer)

def main():
  global theServer

  instHandle = "----"
  serverId = sys.argv[1]

  theServer = ilu.CreateServer(serverId, None, None, createInstance)
  uc = realServer(instHandle, theServer)

  uc.IluPublish()

  sys.stdout.write(uc.IluSBH() + "\n")
  sys.stdout.flush()

  ilu.RunMainLoop(loopvar)

main()
