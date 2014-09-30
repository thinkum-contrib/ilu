# $Id: gridServer.py,v 1.5 1999/08/03 01:58:27 janssen Exp $
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

import ilu, sys, grid, grid__skel

mainloopvar = ilu.CreateLoopHandle()

class real_grid (grid__skel.grid):

	def __init__(self, width, height, name=None, srvr=None):
		self.IluInstHandle = name
		self.IluServer = srvr
		self.width = width;
		self.height = height;
		self.values = {}

	def _get_height (self):
		return self.height

	def _get_width (self):
		return self.height

	def set (self, row, column, val):
		if (row < 0 or row >= self.height or column < 0 or column >= self.width):
			return
		key = "%d,%d" % (row, column)
		self.values[key] = val

	def get (self, row, column):
		if (row < 0 or row >= self.height or column < 0 or column >= self.width):
			return 0
		key = "%d,%d" % (row, column)
		if (self.values.has_key(key)):
			return self.values[key]
		else:
			return 0

def main():

  instHandle = "i1"
  serverID = "gridexample." + ilu.BuildDomain

  s = ilu.CreateServer(serverID, ("tcp_0_0",), "iiop_1_0_1")
  uc = real_grid (10, 10, instHandle, s)

  uc.IluPublish()

  sys.stdout.write(ilu.IOROfObject(uc) + "\n");
  # sys.stdout.write(uc.IluSBH() + "\n");
  sys.stdout.flush()
  
  ilu.RunMainLoop(mainloopvar)

if __name__ == '__main__':
	main()
