# $Id: multlangimpl.py,v 1.6 1999/08/03 01:57:44 janssen Exp $
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

import multlang, multlang__skel, ilu, sys

# here's the implementation of Squarer

multiplier = None

class Squarer (multlang__skel.Squarer):

	def __init__(self, ih, srvr):
		self.IluInstHandle = ih
		self.IluServer = srvr

	def ObtainSquare (self, val):
		global multiplier

		if ((long(val) * long(val)) > 0xFFFFFFFFL):
			raise multlang.TooBig

		if (not multiplier):
			multiplier = ilu.LookupObject("Server1", "theMultiplierObject", multlang.Multiplier)
		if (not multiplier):
			print "Can't find multiplier object!"
			sys.exit(1)
		try:
			result = multiplier.Multiply(val, val)
		except multlang.TooBig:
			raise multlang.TooBig
		return result



# on loading of this module, create an instance
# of Squarer, and Publish it

s = ilu.CreateServer("Server2");
o = Squarer("theSquarerObject", s);
o.IluPublish()
print "created Squarer object <%s>" % o.IluSBH()
