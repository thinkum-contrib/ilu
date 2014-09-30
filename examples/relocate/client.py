# $Id: client.py,v 1.4 1999/08/03 01:59:01 janssen Exp $
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

import sys, string
import ilu
import relocate

def main(argv):
  
	handle = ilu.LookupObject("relocate-server", "dummy", relocate.Foo)
	handle_sbh = handle.IluSBH()
	print 'sbh of handle is', handle_sbh
	cinfo = string.rfind(handle_sbh, ';')
	if cinfo < 0:
		print "can't find cinfo of dummy object in sbh <" + handle_sbh + "!"
		sys.exit(1)
	handle_cinfo = handle_sbh[cinfo+1:]
	if not handle:
		print "Can't find dummy"
		sys.exit(2)
	newobj = handle.dummy()
	newobj_sbh = newobj.IluSBH()
	print 'sbh of new obj is', newobj_sbh
	cinfo = string.rfind(newobj_sbh, ';')
	if cinfo < 0:
		print "can't find cinfo of dummy object in sbh <" + newobj_sbh + "!"
		sys.exit(1)
	newobj_cinfo = newobj_sbh[cinfo+1:]
	if (newobj_cinfo != handle_cinfo):
		print 'different cinfos!'
		sys.exit(1)
	sys.exit(0)

if __name__ == '__main__':
	main (sys.argv)

