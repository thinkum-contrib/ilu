# $Id: manager.py,v 1.2 1999/08/03 01:59:02 janssen Exp $
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

import relocate__skel, ilu, getopt, socket, os, sys, time

program = "relocate server"
mainloopvar = None

RegisteredServers = {}
ServiceProgram = "./sprogram.py"
loopvar = None

class Manager (relocate__skel.RelocationManager):
	def __init__(self, server, ih):
		self.IluInstHandle = ih
		self.IluServer = server

	def RegisterRealCinfo (self, sid, pinfo, tinfo):
		global RegisteredServers
		RegisteredServers[sid] = (pinfo, tuple(tinfo),)
		if (loopvar):
			ilu.ExitMainLoop(loopvar)		

	def start_service (self, sid, ih, pinfo, tinfo):
		cmd = "%s '%s' '%s' '%s' '%s'" % (ServiceProgram, sid, ih,
						  self.IluSBH(), pinfo)
		for element in tinfo:
			cmd = cmd + " '" + element + "'"
		cmd = "( " + cmd + " & )"
		sys.stderr.write("manager:  cmd is <" + cmd + ">\n");
		os.system(cmd)

class Foo (relocate__skel.Foo):
	def __init__(self, server, ih):
		self.IluInstHandle = ih
		self.IluServer = server

	def dummy (self):
		pass

def reloc (sid, ih, pinfo, tinfo, mgr):
	global loopvar
	try:
		sys.stderr.write("manager:  mgr is " + str(mgr) + ", sid is '" + sid + "', ih is '" + ih + "'\n")
		sys.stderr.flush()
		if not RegisteredServers.has_key(sid):
			mgr.start_service (sid, ih, pinfo, tinfo)
		while not RegisteredServers.has_key(sid):
			sys.stderr.write("manager:  waiting for registration of <" + sid + ">...\n");
			if not loopvar:
				loopvar = ilu.CreateLoopHandle()
			ilu.RunMainLoop(loopvar)
		sys.stderr.write("manager:  pinfo is %s, tinfo is %s\n" % RegisteredServers[sid])
		return RegisteredServers[sid]
	except:
		import traceback
		traceback.print_exc()

def main(argv):
	global program, mainloopvar, ServiceProgram
	pinfo = "iiop"
	second = "23456"
	tinfo = None
	sid = "relocate-server"

	program = argv[0]
	hostname = socket.gethostname()
	opts, args = getopt.getopt(argv[1:], "e:s:p:t:")
	for opt in opts:
		if (opt[0] == '-e'):
			ServiceProgram = opt[1]
		if (opt[0] == '-s'):
			sid = opt[1]
		if (opt[0] == '-p'):
			pinfo = opt[1]
		if (opt[0] == '-t'):
			if not tinfo:
				tinfo = [opt[1]]
			else:
				tinfo.append(opt[1])

	# create the Manager object
	mgr_server = ilu.CreateServer(None)
	mgr = Manager(mgr_server, None)

	# now a dummy server for the Foo object
	dummy_server = ilu.CreateServer(sid, tinfo or ["tcp_0_0",], pinfo, None)
	dummy_pinfo, dummy_tinfo = dummy_server.nativeCInfo()
	relocate_proc = lambda a=sid, b="dummy", c=dummy_pinfo, d=dummy_tinfo, e=mgr: reloc(a, b, c, d, e)
	sys.stderr.write("manager:  relocproc is " + str(relocate_proc) + "\n")
	dummy_server.setRelocator(relocate_proc)
	dummy = Foo(dummy_server, "dummy")
	print ilu.IOROfObject(dummy)
	dummy.IluPublish()
	print dummy.IluSBH()

	# and run the main loop
	mainloopvar = ilu.CreateLoopHandle()
	ilu.RunMainLoop(mainloopvar)

if __name__ == "__main__":
	import sys
	main(sys.argv)
