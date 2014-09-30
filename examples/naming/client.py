# $Id: client.py,v 1.6 1999/08/03 01:59:12 janssen Exp $
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

import sys, ilu, string, CORBA, CosNaming, NamingTest

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

def usage (pname):
	sys.stderr.write("usage:  %s [-mt] [-p PINFO] [-t TINFO [TINFO...]] [-name NAME] [-url URL] [-ns NAMESERVICE-URL]\n" % pname)

def main(argv):
  threaded = 0
  nosys = 0
  name = None
  url = None
  handle = None
  nameserver = None
  ior_file = None

  orb = CORBA.ORB_init(argv)

  i = 1;
  while (i < len(argv)):
	  if (argv[i] == '-name'):
		  i = i + 1
		  name = argv[i]
		  i = i + 1
	  elif (argv[i] == '-url'):
		  i = i + 1
		  url = argv[i]
		  i = i + 1
	  elif (argv[i] == '-ns'):
		  i = i + 1
		  nameserver = argv[i]
		  i = i + 1
	  elif (argv[i] == '-ior_file'):
		  i = i + 1
		  ior_file = argv[i]
		  i = i + 1
	  elif (argv[i] == '-mt'):
		  threaded = 1;
		  i = i + 1
	  else:
		  usage(argv[0])

  if (threaded):
	  import thread
	  ilu.ThreadedOperation()

  if name:
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

	  namelist = NameListFromName (name)
	  handle = ns.resolve(namelist)

  elif url:
	  handle = orb.string_to_object(url)

  if not handle:
	  sys.stderr.write ("Can't bind to server object %s\n" % (name or url))
	  sys.exit(1)

  print 'handle is', handle
  print 'print_hello() returns', handle.print_hello()

  sys.exit(0)

if __name__ == "__main__":
	main(sys.argv)
