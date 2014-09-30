# server3.py -- a program that runs a Tutorial.Calculator server
# $Id: server3.py,v 1.7 1999/08/03 01:57:20 janssen Exp $
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

#  Puts up a Tk button to kill it with.

# load ilu_tk first, so that the Tk main loop gets set up with ILU
import ilu_tk
# then load the other ilu-dependent modules
import FactoryImpl2, sys, Tkinter, ilu

def main(argv):

	def quit():
		sys.exit(0)

	if (len(argv) < 2):
		print "Usage:  python server3.py SERVER-ID"
		sys.exit(1)

	theServer = ilu.CreateServer (argv[1])
	theFactory = FactoryImpl2.Factory ("theFactory", theServer)
	theFactory.IluPublish()

	# Now we put up a Tk button so that the user can kill the
	#  server by pressing the button

	f = Tkinter.Frame() ; Tkinter.Pack.config(f)
	b = Tkinter.Button (f, {'text' : theFactory.IluObjectID(),\
				'command': quit})
	b.pack ({'side': 'left', 'fill': 'both'})

	# Then we wait in the ilu_tk mainloop, instead of either
	#  the ILU mainloop or the Tkinter mainloop

	ilu_tk.RunMainLoop()


main(sys.argv)
