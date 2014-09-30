# server2.py -- a program that runs a Tutorial.Calculator server,
#  which actually returns instances of Tutorial2.TapeCalculator.
#
# $Id: server2.py,v 1.6 1999/08/03 01:57:16 janssen Exp $
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

import ilu, FactoryImpl2, sys

def main(argv):

	if (len(argv) < 2):
		print "Usage:  python server2.py SERVER-ID"
		sys.exit(1)

	theServer = ilu.CreateServer (argv[1])
	theFactory = FactoryImpl2.Factory ("theFactory", theServer)

	# Now make the Factory object "well-known" by publishing it.

	theFactory.IluPublish()

	# Now we print the string binding handle (the object's name plus
	# its location) of the new instance.

	print "Factory2 instance published."
	print "Its SBH is '" + theFactory.IluSBH() + "'"

	loophandle = ilu.CreateLoopHandle()
	ilu.RunMainLoop (loophandle)


main(sys.argv)
