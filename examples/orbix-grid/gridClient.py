# $Id: gridClient.py,v 1.6 1999/08/03 01:58:29 janssen Exp $
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

import CORBA, sys, grid

def do_test (orb, grid_ior):
	g = orb.string_to_object(grid_ior)
	print g
	width = g._get_width()
	height = g._get_height()
	print 'width = %d, height = %d' % (width, height)
	g.set(1, 1, 23)
	val = g.get (1, 1)
	if (val != 23):
		print "wrong val", val, "returned by get(1,1)"
		return 1
	else:
		print "set of (1,1) succeeded"
		return 0

def main (argv):
	if not (len(argv) == 2):
		print 'Usage:  %s GRID-IOR'
		sys.exit(1)
	orb = CORBA.ORB_init(argv)
	sys.exit(do_test(orb, argv[1]))

if __name__ == '__main__':
	main(sys.argv)
		
