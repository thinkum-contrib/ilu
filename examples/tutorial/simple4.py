# $Id: simple4.py,v 1.5 1999/08/03 01:57:18 janssen Exp $
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

# simple4.py -- a simple client program that finds the TapeCalculator Factory,
#   creates a calculator, and provides a simple interactive calculator
#
# to run:  python simple4.py ARG [ARG...]

import Tutorial2, ilu, sys, string

# We define a new routine, "Get_Tutorial_Calculator", which 
# finds the tutorial factory, then creates a new Calculator
# object for us.

def Get_Tutorial_Calculator (sid, ih):

	# We have to call ilu.LookupObject() with the object ID of
	# the factory object, and the ``type'' of the object we're looking
	# for, which is always available as MODULE.TYPENAME

	f = ilu.LookupObject (sid, ih, Tutorial2.Factory)
	if not f:
		print "Can't find Tutorial.Factory instance " + factoryObjectID
		sys.exit(1)
	c = f.CreateTapeCalculator()
	return (c)


opname = ['SetValue', 'Add', 'Subtract', 'Divide', 'Multiply']

def Print_Tape (tape):

	# print out the Calculator tape nicely

	for op in tape:
		print "  %s(%f) => %f" % (opname[op['op']], op['value'], op['accumulator'])

def main (argv):

	if (len(argv) < 3):
		print "Usage:  python simple4.py FACTORY-OBJECT-SID FACTORY-OBJECT-IH NUMBER [NUMBER...]\n",
		sys.exit(1)

	c = Get_Tutorial_Calculator(argv[1], argv[2])
	if not c:
		print "Couldn't create calculator"
		sys.exit(1)

	# clear the calculator before using it

	newval = 0.0
	c.SetValue (newval)

	quitflag = 0

	while not quitflag:

		sys.stdout.write("%.5f\n> " % newval)
		sys.stdout.flush()

		line = sys.stdin.readline()

		if (not line):
			sys.exit(0)

		try:
			if (line[0] == '\n'):
				pass
			elif (line[0] == '+'):
				val = string.atof(line[1:-1])
				c.Add(val)
			elif (line[0] == '-'):
				val = string.atof(line[1:-1])
				c.Subtract(val)
			elif (line[0] == '*'):
				val = string.atof(line[1:-1])
				c.Multiply(val)
			elif (line[0] == '/'):
				val = string.atof(line[1:-1])
				c.Divide(val)
			elif (line[0] == 'q'):
				quitflag = 1
			elif (line[0] == 'c'):
				c.SetValue(0.0)
			elif (line[0] == 't'):
				Print_Tape ( c.GetTape() )
			else:
				print "Invalid operation <" + line[:-1] + ">"
				print "Valid ops are +, -, *, /, tape, clear, quit"

			newval = c.GetValue()

		except:
			print "Operation <%s> signals error <%s>." % (line[:-1], sys.exc_type)

	sys.exit(0)


main(sys.argv)
