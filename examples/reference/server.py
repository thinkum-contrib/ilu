# $Id: server.py,v 1.1 1999/02/26 01:12:21 janssen Exp $
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

import sys, ilu, reference_test, reference_test__skel

class O(reference_test__skel.O):
  def __init__(self, ih, srvr):
    self.IluInstHandle = ih
    self.IluServer = srvr

  def m1 (self, p):
	  print "Received node is:"
	  print p
	  return p;

  def m2 (self, p):
	  return p;

loopvar = ilu.CreateLoopHandle();

def usage(pname):
	sys.stderr.write("Usage:  %s [-p PINFO] [-t TINFO] [-mt]\n" % pname)
	sys.exit(1)

def main(argv):
  instHandle = "O"
  serverId = "reference-test.parc.xerox.com"
  pinfo = None
  tinfo = None
  threaded = 0;

  i = 1;
  while (i < len(argv)):
	  if (argv[i] == '-p'):
		  i = i + 1
		  pinfo = argv[i]
		  i = i + 1
	  elif (argv[i] == '-mt'):
		  threaded = 1;
		  i = i + 1
	  elif (argv[i] == '-t'):
		  tinfo = []
		  i = i + 1
		  while (i < len(argv) and (argv[i][0] != '-')):
			  tinfo.append(argv[i])
			  i = i + 1
	  else:
		  usage(argv[0])

  if (threaded):
	  import thread
	  ilu.ThreadedOperation()

  s = ilu.CreateServer(serverId, tinfo, pinfo)
  uc = O(instHandle, s)

  uc.IluPublish()

  print "exported", uc.IluSBH()

  ilu.RunMainLoop(loopvar)

if __name__ == "__main__":
	main(sys.argv)
