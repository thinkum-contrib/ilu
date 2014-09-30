# $Id: pyServer.py,v 1.25 1999/08/03 01:52:19 janssen Exp $
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

TRUE = 1
FALSE = 0

if '-mt' in sys.argv and not ('-st' in sys.argv):
	import thread
	import ilu
	ilu.ThreadedOperation()
else:
	import ilu

import Test1
import Test1__skel
import Test3__skel

singleO2 = None
sunrpcServer = None

the_Test1_O3 = None
the_Test1_O4 = None
the_Test3_O  = None

quiet = None

class O1(Test1__skel.O1):
  def __init__(self, ih, srvr):
    self.IluInstHandle = ih
    self.IluServer = srvr
    self.one = 0

  def U_CSS_to_U(self, u, css):
    if not quiet: print "Test1.O1.U-CSS-to-U"
    return u

  def f_CSS_to_RO(self, css):
    if not quiet: print "Test1.O1.f-CSS-to-R0"
    return Test1.TheR(("one", "two", "three"), ["hi", "there"], 9)

  def R_ScS_to_F(self, r, s):
    if not quiet: print "Test1.O1.R-ScS-to-F"
    return 39.7

  def a_RO(self, ro):
    if not quiet: print "Test1.O1.a-RO"

  def get_O2(self):
    global singleO2, sunrpcServer
    if not quiet: print "Test1.O1.get-O2"
    if not sunrpcServer:
	    try:
		    sunrpcServer = ilu.CreateServer("Test1-SunRPC-Server",
						    ("sunrpcrm", "tcp_0_0"),
						    "sunrpc_2_0x3458_3");
	    except:
		    pass
    if not sunrpcServer:
	    raise Test1.CantCreate
    if singleO2:
      uc = singleO2
    else:
      try:
        uc = O2(sunrpcServer)
      except:
        raise Test1.CantCreate
      singleO2 = uc
    return uc

  def get_O3(self, subclass):
    global the_Test1_O3, the_Test1_O4, the_Test3_O
    if not quiet: print "Test1.O1.get-O3"
    try:
      if subclass:
	if not the_Test3_O:
	  the_Test3_O = O(self.IluServer)
	return the_Test3_O
      else:
	if self.one == 0:
	  self.one = 1
	  if not the_Test1_O3:
	    if not quiet: print "making O3..."
	    the_Test1_O3 = O3(self.IluServer)
	  return the_Test1_O3
	else:
	  self.one = 0
	  if not the_Test1_O4:
	    if not quiet: print "making O4..."
	    the_Test1_O4 = O4(self.IluServer)
	  return the_Test1_O4
    except:
      raise Test1.CantCreate

class O2(Test1__skel.O2):
  def __init__(self, srvr):
    self.IluServer = srvr

  def OO_A0_to_CSS(self, o, a):
    if not quiet: print "Test1.o2.OO-A0-to-CSS"
    if o == None:
      raise Test1.E2, 7
    return []

  def R_I_A1_to_I_A0(self, r, i, a):
    if not quiet: print "Test1.O2.R-I-A1-to-I-A0"
    ret = [1, 2, 3, 4, 5, 6, 7, 8]
    return ret, i

class O3(Test1__skel.O3):
  def __init__(self, srvr):
    self.IluServer = srvr

  def RS_R_to_R_IS(self, r):
    passport = ilu.CallerIdentity()
    if not quiet: print "passport is", passport
    identity = {}
    val = passport.lookupIdentity("ConnectionIdentity")
    if val:
	    identity["ConnectionIdentity"] = val
    val = passport.lookupIdentity("SunRPCAuthUnixIdentity")
    if val:
	    identity["SunRPCAuthUnixIdentity"] = val
    if not quiet: print "Test1.O3.RS-R-to-R-IS"
    if not quiet: print "(Caller is " + str(identity) + ")"
    r2 = Test1.TheR(("a", "b", "c"), ["just", "a", "string"], -133567)
    ret = []
    return ret, r2

  def O1_U_to_U(self, o, u):
    if not quiet: print "Test1.O3.O1-U-to-U"
    u = (3, o)
    return u

  def BS_to_I(self, b):
    return len(b) * len(b)

class P(Test1__skel.P):
  def RS_R_to_R_IS(self, r):
    if not quiet: print "Test1.P.RS-R-to-R-IS"
    r2 = Test1.TheR(["from", "P", "string"], ["one", "two"], 25719)
    ret = []
    return ret, r2

  def O1_U_to_U(self, o, u):
    if not quiet: print "Test1.P.O1-U-to-U"
    u = (3, o)
    return u

  def BS_to_I(self, b):
    return len(b)

  def m2(self, j):
    return [j, j * j]

class O4(Test1__skel.O4):
  def __init__(self, srvr):
    self.IluServer = srvr

  def RS_R_to_R_IS(self, r):
    if not quiet: print "Test1.O4.RS-R-to-R-IS"
    r2 = Test1.TheR(["from", "P", "string"], ["three", "four"], 0x7FFFFFF3)
    ret = []
    return ret, r2

  def O1_U_to_U(self, o, u):
    if not quiet: print "Test1.O4.O1-U-to-U"
    u = (3, o)
    return u

  def BS_to_I(self, b):
    bLen = len(b)
    if not quiet: print "Test1.O4.BS-to-I (%d:" % bLen,
    for i in range(0, 11):
      if i < bLen:
	val = b[i]
      else:
	val = 0
      if not quiet: print "%02x" % val
    if not quiet: print "...) => %d" % bLen
    return bLen

  def R_to_R(self, r):
    r2 = 1020304.05060708
    if not quiet: print "Test1.O4.R_to_R (%.10f) => %.10f" % (r, r2)
    return r2

class O(Test3__skel.O):
  def __init__(self, srvr):
    self.IluServer = srvr

  def RS_R_to_R_IS(self, r):
    if not quiet: print "Test3.O.RS-R-to-R-IS"
    r2 = Test1.TheR(["from", "P", "string"], ["three", "four"], -1)
    ret = []
    return ret, r2

  def O1_U_to_U(self, o, u):
    if not quiet: print "Test3.O.O1-U-to-U (", o, ", {%d})" % u[0]
    u = (3, o)
    return u

  def BS_to_I(self, b):
    return len(b) * len(b)

  def SR_to_I(self, i):
    if not quiet: print "Test3.O.SR-to-I(%f)" % i
    return int(i)

  def I_to_Test1U(self, i):
    if not quiet: print "Test3.O.I-to-Test1U(%d)" % i
    return (5, ilu.TRUE)

loopvar = ilu.CreateLoopHandle();

def Usage (pname):
	import sys
	sys.stderr.write ("Usage:  python " + pname + " [-mt | -st] [-quiet] [-sid SID] [-ih IH] [-p PINFO] [-t TINFO...]\n");
	sys.exit(1)

def main(argv):
  global quiet
  instHandle = "Test1_Initial_Object"
  serverId = "Test1-Server"
  tinfo = None	# use default tinfo
  pinfo = None	# use default pinfo
  threaded = FALSE

  i = 1;
  while i < len(argv):
	  arg = argv[i]
	  if (arg[0] != '-'):
		  break;
	  elif (arg == '-quiet'):
		  quiet = TRUE;
	  elif (arg == '-mt'):
		  threaded = TRUE;
	  elif (arg == '-st'):
		  threaded = FALSE;
	  elif (arg == '-sid'):
		  serverId = argv[i+1]
		  i = i + 1
	  elif (arg == '-ih'):
		  instHandle = argv[i+1]
		  i = i + 1
	  elif (arg == '-p'):
		  pinfo = argv[i+1]
		  i = i + 1
	  elif (arg == '-t'):
		  i = i + 1
		  tinfo = []
		  while (i < len(argv)):
			  tinfo.append(argv[i])
			  i = i + 1
		  break
	  else:
		  Usage(argv[0])
	  i = i + 1

  s = ilu.CreateServer(serverId, tinfo, pinfo)
  uc = O1(instHandle, s)

  uc.IluPublish()
  uc2 = ilu.LookupObject(serverId, instHandle, Test1.O1)
  if uc2 != uc:
    print "*** Error, lookup returns wrong object"
  uc2.IluPublish()

  ilu.SetFDBudget(1000)

  if not quiet: print "exported", uc.IluSBH()

  ilu.RunMainLoop(loopvar)

if __name__ == "__main__":
	main (sys.argv)
