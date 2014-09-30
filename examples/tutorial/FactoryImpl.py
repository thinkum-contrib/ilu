import Tutorial, Tutorial__skel, CalculatorImpl, ilu

def IsSubType (type1, type2):
	# returns TRUE if type2 is a base type of type1,
	# possibly at several removes
	for baseclass in type1.__bases__:
		if (baseclass == type2):
			return 1
		elif IsSubType (baseclass, type2):
			return 1
	return 0

NeedsHoldingOnTo = IsSubType(Tutorial.Calculator, ilu.CORBA_Object)
if (NeedsHoldingOnTo):
	print 'Using OMG IDL derived interfaces...'
	Holder = []

class Factory (Tutorial__skel.Factory):

	# have the __init__ method take handle and server args
	# so that we can control which ILU kernel server is used,
	# and what the instance handle of the Factory object on
	# that server is.  This allows us to control the object ID
	# of the new Factory instance.

	def __init__(self, handle=None, server=None):
		self.IluInstHandle = handle
		self.IluServer = server
		
	def CreateCalculator (self):
		newcalc = CalculatorImpl.Calculator()
		if (NeedsHoldingOnTo):
			Holder.append(newcalc)
		return newcalc
