# $Id: server.py,v 1.6 1999/08/03 01:59:00 janssen Exp $
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

import relocate__skel, ilu, getopt, socket

program = "relocate server"
mainloopvar = None

class Foo (relocate__skel.Foo):
	def __init__(self, server, ih):
		self.IluInstHandle = ih
		self.IluServer = server

	def dummy (self):
		pass

def reloc (pinfo, tinfo):
	print "relocating to", pinfo, tinfo
	return (pinfo, tinfo,)

def main(argv):
	global program, mainloopvar
	pinfo = "iiop"
	second = "23456"
	tinfo1 = None
	tinfo2 = None
	tinfo_rm = None

	program = argv[0]
	hostname = socket.gethostname()
	opts, args = getopt.getopt(argv[1:], "p:1:2:t:")
	for opt in opts:
		if (opt[0] == '-p'):
			pinfo = opt[1]
		if (opt[0] == '-1'):
			tinfo1 = ("tcp_%s_%s" % (hostname, opt[1]),)
		if (opt[0] == '-2'):
			tinfo2 = ("tcp_%s_%s" % (hostname, opt[1]),)
		if (opt[0] == '-t'):
			tinfo_rm = opt[1]

	if not tinfo1 or not tinfo2:
		print "Usage:  %s [-p PROTOCOL] [-t RECORD-MARKING-TINFO-LAYER] -1 PORT1 -2 PORT2" % (argv[0])
		return 1
	if tinfo_rm:
		tinfo1 = (tinfo_rm, tinfo1[0])
		tinfo2 = (tinfo_rm, tinfo2[0])
	server = ilu.CreateServer("relocate-server", tinfo1, pinfo, None)
	server.addPort (tinfo2, pinfo)
	relocate = lambda a=pinfo, b=tinfo2: reloc(a, b)
	server.setRelocator(relocate)
	dummy = Foo(server, "dummy")
	print ilu.IOROfObject(dummy)
	dummy.IluPublish()
	print dummy.IluSBH()
	mainloopvar = ilu.CreateLoopHandle()
	ilu.RunMainLoop(mainloopvar)

if __name__ == "__main__":
	import sys
	main(sys.argv)
