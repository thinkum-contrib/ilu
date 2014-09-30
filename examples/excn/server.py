# $Id: server.py,v 1.2 1999/08/03 01:58:53 janssen Exp $
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

import sys, ilu, ExcnTest, ExcnTest__skel

class obj(ExcnTest__skel.O):
  def __init__(self, ih, srvr):
    self.IluInstHandle = ih
    self.IluServer = srvr

  def throw_excn (self, v):

	  if (v == 1):
		  raise ExcnTest.E1, (0, ExcnTest.R(((13, 13, 13, 13), (13, 13, 13, 13), (13, 13, 13, 13)),
						    ['first', 'second', 'third'], 789),)

	  elif (v == 2):
		  raise ExcnTest.E2, 13

	  elif (v == 3):
		  raise ExcnTest.E3, None

	  elif (v == 4):
		  raise ExcnTest.E4, self

	  elif (v == 5):
		  raise ExcnTest.E5, (0, 1, 2, 3, 4, 5, 6, 7)

	  elif (v == 6):
		  raise ExcnTest.E6, [ExcnTest.R(((9, 9, 9, 9),(9, 9, 9, 9),(9, 9, 9, 9)), ['red', 'blue', 'green'], 123), ExcnTest.R(((9, 9, 9, 9),(9, 9, 9, 9),(9, 9, 9, 9)), ['red', 'blue', 'green'], 123)]

	  elif (v == 7):
		  raise ExcnTest.E7, "test-string"

	  elif (v == 8):
		  raise ExcnTest.E8, ((0, 1, 2, 3),(4, 5, 6, 7),(8, 9, 10, 11))

	  elif (v == 9):
		  raise ExcnTest.E9, ExcnTest.R(((9, 9, 9, 9),(9, 9, 9, 9),(9, 9, 9, 9)), ['red', 'blue', 'green'], 123)

	  elif (v == 10):
		  raise ExcnTest.E10, ExcnTest.TheE.ev5

	  elif (v == 11):
		  raise ExcnTest.E11, ExcnTest.E11__omgidl_exctype("some nutty reason")

	  elif (v == 12):
		  pass

	  elif (v == 13):
		  pass

	  elif (v == 14):
		  raise ExcnTest.NotUsed

	  elif (v == 15):
		  pass

	  else:
		  raise ExcnTest.NotUsed

loopvar = ilu.CreateLoopHandle();

def usage(pname):
	sys.stderr.write("Usage:  %s [-p PINFO] [-t TINFO]\n" % pname)
	sys.exit(1)

def main(argv):
  instHandle = "T"
  serverId = "ExcnTest.server.parc.xerox.com"
  pinfo = None
  tinfo = None

  i = 1;
  while (i < len(argv)):
	  if (argv[i] == '-p'):
		  i = i + 1
		  pinfo = argv[i]
		  i = i + 1
	  elif (argv[i] == '-t'):
		  tinfo = []
		  i = i + 1
		  while (i < len(argv) and (argv[i][0] != '-')):
			  tinfo.append(argv[i])
			  i = i + 1
	  else:
		  usage(argv[0])

  s = ilu.CreateServer(serverId, tinfo, pinfo)
  uc = obj(instHandle, s)

  uc.IluPublish()

  print "exported", uc.IluSBH()

  ilu.RunMainLoop(loopvar)

if __name__ == "__main__":
	main(sys.argv)
