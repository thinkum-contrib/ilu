# BeginILUCopyright
# 
# Copyright (c) 1991-1999 Xerox Corporation.  All Rights Reserved.
# 
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
# 
# EndILUCopyright
#
# $Id: ilu_tk.py,v 1.15 1999/08/03 01:55:40 janssen Exp $
#

import ilu

# The Python module for TK 4.1 is "_tkinter", the module for TK 4.0 is
# "tkinter".  We try "_tkinter" first, then "tkinter" if that's
# unsuccessful, and assign whichever one works to "ilutkinter".
# Thereafter in this module, we use the generic module "ilutkinter"
# instead of either of the specific ones.

try:
	import _tkinter
	ilutkinter = _tkinter
except:
	import tkinter
	ilutkinter = tkinter

#
# Exceptions
#
FilenoMismatch = 'fileno mismatch'
BadMask = 'bad mask'

#
# Tk's model for registering file descriptors for events is different than
# ILU's.
#
# Tk wants a single callback function per fd, with a mask that tells which
# conditions (READABLE, WRITABLE, or both) are of interest.  One or the other
# of the bits for READABLE and WRITABLE are then passed to the callback
# function to tell it what to do.
#
# ILU, on the other hand, wants to register a possibly different callback
# function on the same fd for each of READABLE and WRITABLE.
#
# The class _iohcPair and the functions _registerHandler and _deleteHandler
# compensate for the mismatch between the two models.
#

class _iohcPair:
    def __init__(self, fileno):
	self.curFileno = fileno
	self.curMask = 0
	self.reader = None
	self.writer = None
	self.READABLE = ilutkinter.READABLE
	self.WRITABLE = ilutkinter.WRITABLE

    def fileno(self):
	return self.curFileno

    def mask(self):
	return self.curMask

    def setMember(self, iohc, mask):
	if self.curFileno != iohc.fileno():
	    raise FilenoMismatch
	if mask == self.READABLE:
	    self.reader = iohc
	elif mask == self.WRITABLE:
	    self.writer = iohc
	else:
	    raise BadMask
	self.curMask = self.curMask | mask

    def unsetMember(self, mask):
	if mask == self.READABLE:
	    self.reader = None
	elif mask == self.WRITABLE:
	    self.writer = None
	else:
	    raise BadMask
	self.curMask = self.curMask & ~mask

    def which(self, mask):
	if mask == self.READABLE:
	    return self.reader
	elif mask == self.WRITABLE:
	    return self.writer
	else:
	    raise BadMask

def _callhandler(pair, mask):
    pair.which(mask).call()


_fdMap = {}

def _registerHandler(iohc, mask):
    global _fdMap
    fd = iohc.fileno()
    if _fdMap.has_key(fd):
	pair = _fdMap[fd]
    else:
	pair = _iohcPair(fd)
	_fdMap[fd] = pair
    pair.setMember(iohc, mask)
    ilutkinter.createfilehandler(pair, pair.mask(), _callhandler)
    return 1

# In the functions that follow, and also the _iohcPair class above, we
# eliminate any references to globals in the bodies of the
# functions/methods. This is because _can_inp() and _can_out() will get called
# late in the Python shutdown phase, when the globals for ilu_tk.py module
# have already been cleared.

def _deleteHandler(fd, mask,
		   _fdMap=_fdMap,
		   _callhandler=_callhandler,
		   deletefilehandler=ilutkinter.deletefilehandler,
		   createfilehandler=ilutkinter.createfilehandler):
    if _fdMap.has_key(fd):
	    pair = _fdMap[fd]
	    iohc = pair.which(mask)
	    pair.unsetMember(mask)
	    if pair.mask() == 0:
		    del _fdMap[fd]
		    deletefilehandler(fd)
	    else:
		    createfilehandler(pair, pair.mask(), _callhandler)
	    return iohc
    else:
	    return None

def _reg_inp(iohc):
    return _registerHandler(iohc, ilutkinter.READABLE)

def _can_inp(fd, _deleteHandler=_deleteHandler, READABLE=ilutkinter.READABLE):
    return _deleteHandler(fd, READABLE)

def _reg_out(iohc):
    return _registerHandler(iohc, ilutkinter.WRITABLE)

def _can_out(fd, _deleteHandler=_deleteHandler, WRITABLE=ilutkinter.WRITABLE):
    return _deleteHandler(fd, WRITABLE)

class _tk_alarm:

	def __init__(self):
		self.token = None

	def cancel_alarm (self):
		if self.token:
			self.token.deletetimerhandler()
			self.token = None

	def set_alarm (self, thc):
		self.cancel_alarm()
		milliseconds = int(1000 * float(thc.time() - ilu.FineTime_Now()))
		self.token = ilutkinter.createtimerhandler(milliseconds, thc.call)

def _create_alarm ():
	return (_tk_alarm())

def _do_event (*args):
	print '_do_event(%s)' % args
	ilutkinter.dooneevent()

def _set_alarm (alarm, thc):
	alarm.set_alarm(thc)

def _cancel_alarm (alarm):
	alarm.cancel_alarm()

ilu.SetMainLoop (ilutkinter.dooneevent, _reg_inp, _can_inp, _reg_out, _can_out,  _create_alarm, _set_alarm, _cancel_alarm)

def RunMainLoop():
	ilutkinter.mainloop(-1)

def ExitMainLoop():
	ilutkinter.quit()
