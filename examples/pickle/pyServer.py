# $Id: pyServer.py,v 1.6 1999/09/02 06:11:13 janssen Exp $
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

import sys, ilu
if ilu.CORBAMapping:
	import Test2__POA
	trueModule = Test2__POA
else:
	import Test2__skel
	trueModule = Test2__skel

class O1(trueModule.O1):
  def __init__(self, ih, srvr):
    self.IluInstHandle = ih
    self.IluServer = srvr

  def bounce (self, v):
	  return v;

loopvar = ilu.CreateLoopHandle();

def main():
  instHandle = "pickleObj"
  serverId = "pickleServer"

  s = ilu.CreateServer(serverId, ('tcp_0_0',), 'iiop_1_0_1')
  uc = O1(instHandle, s)

  uc.IluPublish()

  print "exported", uc.IluSBH()

  ilu.RunMainLoop(loopvar)

main()
