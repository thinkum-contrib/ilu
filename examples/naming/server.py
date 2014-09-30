# $Id: server.py,v 1.5 1999/08/03 01:59:12 janssen Exp $
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

import sys, ilu, NamingTest, CosNaming, string, CORBA

if ilu.CORBAMapping:
	import NamingTest__POA
	trueModule = NamingTest__POA
	notFoundExcn = CosNaming.NamingContext.NotFound
	alreadyBoundExcn = CosNaming.NamingContext.AlreadyBound
else:
	import NamingTest__skel
	trueModule = NamingTest__skel
	notFoundExcn = CosNaming.NamingContext_NotFound
	alreadyBoundExcn = CosNaming.NamingContext_AlreadyBound

class O(trueModule.O):
  def __init__(self, ih, srvr):
    self.IluInstHandle = ih
    self.IluServer = srvr
    self.count = 0

  def print_hello (self):
	  print 'hello'
	  self.count = self.count + 1
	  return self.count;

loopvar = ilu.CreateLoopHandle();

def usage(pname):
	sys.stderr.write("Usage:  %s [-p PINFO] [-t TINFO] [-n NAME] [-ns IOR]\n" % pname)
	sys.exit(1)

def NameListFromName (namestring):
	namelist = []
	names = string.split(namestring, '/')
	for name in names:
		parts = string.split(name, '.')
		id = parts[0]
		if (len(parts) > 1):
			kind = parts[1]
		else:
			kind = ""
		namelist.append(CosNaming.NameComponent(id, kind))
	return namelist

def main(argv):
  instHandle = "T"
  serverId = "NamingTest.server.parc.xerox.com"
  pinfo = None
  tinfo = None
  name = "NamingTest/T"
  nameserver = None
  lookup = 0
  ns = None
  ior_file = None

  orb = CORBA.ORB_init(argv)
  i = 1;
  while (i < len(argv)):
	  if (argv[i] == '-p'):
		  i = i + 1
		  pinfo = argv[i]
		  i = i + 1
	  elif (argv[i] == '-n'):
		  i = i + 1
		  name = argv[i]
		  i = i + 1
	  elif (argv[i] == '-ior_file'):
		  i = i + 1
		  ior_file = argv[i]
		  i = i + 1
	  elif (argv[i] == '-ns'):
		  i = i + 1
		  nameserver = argv[i]
		  i = i + 1
	  elif (argv[i] == '-t'):
		  tinfo = []
		  i = i + 1
		  while (i < len(argv) and (argv[i][0] != '-')):
			  tinfo.append(argv[i])
			  i = i + 1
	  else:
		  sys.stderr.write("Bad switch '" + argv[i] + "'\n")
		  usage(argv[0])

  if nameserver:
	  ns = orb.string_to_object(nameserver, CosNaming.NamingContext);
  elif ior_file:
	  fp = open(ior_file, 'r')
	  ior = fp.read()
	  fp.close()
	  if (ior[-1] == '\n'):
		  ior = ior[:-1]
	  ns = orb.string_to_object(ior, CosNaming.NamingContext);
  else:
	  ns = orb.resolve_initial_references("NameService")
  if not ns:
	  print "Can't access CosNaming service"

  s = ilu.CreateServer(serverId, tinfo, pinfo)
  uc = O(instHandle, s)

  namelist = NameListFromName (name)
  print 'name is', name, ' and namelist is', namelist

  for i in range(len(namelist)-1):
	  nl2 = namelist[0:i+1]
	  print 'i is', i, 'nl2 is', nl2
	  try:
		  ctx = ns.resolve(nl2)
	  except notFoundExcn:
		  # ok, make it
		  ctx = ns.bind_new_context(nl2)

  try:
	  ns.bind(namelist, uc)
  except alreadyBoundExcn:
	  ns.rebind(namelist, uc)
	  
  print "exported", uc.IluSBH()

  ilu.RunMainLoop(loopvar)

if __name__ == "__main__":
	main(sys.argv)
