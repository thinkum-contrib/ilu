# $Id: TapeCalculatorImpl.py,v 1.4 1999/08/03 01:57:16 janssen Exp $
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

import Tutorial, Tutorial2, Tutorial2__skel

class TapeCalculator (Tutorial2__skel.TapeCalculator):

	def __init__ (self):
		self.value = 0.0
		self.tape = []

	def SetValue (self, v):
		global value
		self.value = v
		self.tape.append({'op' : Tutorial2.OpType.SetValue, 'value' : v, 'accumulator' : self.value})

	def GetValue (self):
		return self.value

	def Add (self, v):
		self.value = self.value + v
		self.tape.append({'op' : Tutorial2.OpType.Add, 'value' : v, 'accumulator' : self.value})


	def Subtract (self, v):
		self.value = self.value - v
		self.tape.append({'op' : Tutorial2.OpType.Subtract, 'value' : v, 'accumulator' : self.value})


	def Multiply (self, v):
		self.value = self.value * v
		self.tape.append({'op' : Tutorial2.OpType.Multiply, 'value' : v, 'accumulator' : self.value})


	def Divide (self, v):
		try:
			self.value = self.value / v
		except ZeroDivisionError:
			raise Tutorial.DivideByZero
		self.tape.append({'op' : Tutorial2.OpType.Divide, 'value' : v, 'accumulator' : self.value})

	def GetTape (self):
		return (self.tape)
