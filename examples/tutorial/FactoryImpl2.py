import Tutorial2, Tutorial2__skel, TapeCalculatorImpl

class Factory (Tutorial2__skel.Factory):

	# have the __init__ method take handle and server args
	# so that we can control which ILU kernel server is used,
	# and what the instance handle of the Factory object on
	# that server is.  This allows us to control the object ID
	# of the new Factory instance.

	def __init__(self, handle=None, server=None):
		self.IluInstHandle = handle
		self.IluServer = server
		
	def CreateCalculator (self):
		return (TapeCalculatorImpl.TapeCalculator())

	CreateTapeCalculator = CreateCalculator
