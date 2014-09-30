import ilu
import test__skel

inst = None

class Strlen(test__skel.Strlen):

	def __init__(self, ih, server):
		self.IluInstHandle = ih
		self.IluServer = server

	def len (self, str):
		return len(str)

def init(sid):

	global inst

	# we create the server carefully here.  We don't create
	# a normal port, and it doesn't become the default server
	# (though it could be)
	server = ilu.FullCreateServer(sid, None, None, None)

	# now make an instance of Strlen with the server
	inst = Strlen(None, server)

	# and return its SBH
	return inst.IluSBH()
