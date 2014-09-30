# $Id: client.py,v 1.2 1999/04/30 18:13:20 janssen Exp $
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

import sys, ilu, reference_test, string, CORBA

def usage(pname):
	sys.stderr.write("Usage:  %s [-mt] [-n REPEAT-COUNT]\n" % pname)
	sys.exit(1)

def main(argv):
  instHandle = "O"
  serverId = "reference-test.parc.xerox.com"
  pinfo = None
  tinfo = None
  threaded = 0

  i = 1;
  while (i < len(argv)):
	  if (argv[i] == '-n'):
		  i = i + 1
		  count = string.atoi(argv[i])
		  i = i + 1
	  elif (argv[i] == '-mt'):
		  threaded = 1;
		  i = i + 1
	  else:
		  usage(argv[0])

  if (threaded):
	  import thread
	  ilu.ThreadedOperation()

  handle = ilu.LookupObject(serverId, instHandle, reference_test.O)
  if not handle:
	  sys.stderr.write ("Can't bind to server object %s/%s\n" % (serverId, instHandle))
	  sys.exit(1)

  n4 = reference_test.Node1("Node 4", None, None)
  n3 = reference_test.Node1("Node 3", n4, None)
  n2 = reference_test.Node1("Node 2", None, n4)
  n1 = reference_test.Node1("Node 1", n2, n3)

  n5 = handle.m1(n1)

  if not ((n5.name == "Node 1") and
	  n5.left and
	  (n5.left.name == "Node 2") and
	  (not n5.left.left) and
	  n5.left.right and
	  (n5.left.right.name == "Node 4") and
	  (not n5.left.right.left) and
	  (not n5.left.right.right) and
	  n5.right and
	  (n5.right.name == 'Node 3') and
	  n5.right.left and
	  (n5.right.left.name == 'Node 4') and
	  (not n5.right.left.left) and
	  (not n5.right.left.right) and
	  (not n5.right.right) and
	  (n5.right.left is n5.left.right)):
	print 'Bad data structure returned.'
	print "returned node is ", n5
	return 1

  return 0

if __name__ == "__main__":
	main(sys.argv)
