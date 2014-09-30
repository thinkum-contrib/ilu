# $Id: simple3.py,v 1.6 1999/08/03 01:57:17 janssen Exp $
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

# simple3.py -- a simple client program that finds the Calculator Factory,
#   creates a calculator, and adds up its arguments as before
#
# to run:  python simple4.py ARG [ARG...]

import Tutorial, ilu, sys, string

# We define a new routine, "Get_Tutorial_Calculator", which 
# finds the tutorial factory, then creates a new Calculator
# object for us.

def Get_Tutorial_Calculator (sid, ih):

	# We have to call ilu.LookupObject() with the object ID of
	# the factory object, and the ``type'' of the object we're looking
	# for, which is always available as MODULE.TYPENAME

	f = ilu.LookupObject (sid, ih, Tutorial.Factory)
	if not f:
		print "Can't find Tutorial.Factory instance %s/%s" % (sid, ih)
		sys.exit(1)
	c = f.CreateCalculator()
	return (c)

def main (argv):

	# A simple program:
	#  1)  make an instance of Tutorial.Calculator
	#  2)  add all the arguments by invoking the Add method
	#  3)  print the resultant value.

	if (len(argv) < 3):
		print "Usage:  python simple3.py FACTORY-OBJECT-SID FACTORY-OBJECT-IH NUMBER [NUMBER...]\n",
		sys.exit(1)

	c = Get_Tutorial_Calculator(argv[1], argv[2])
	if not c:
		print "Couldn't create calculator"
		sys.exit(1)

	# clear the calculator before using it

	c.SetValue (0.0)

	# now loop over the arguments, adding each in turn

	for arg in argv[3:]:
		v = string.atof (arg)
		c.Add (v)

	# and print the result

	print "the sum is " + str(c.GetValue())

	sys.exit (0);  


main(sys.argv)
