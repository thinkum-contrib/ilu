from ILUTesting import *

if iluconf_dict.has_key("ILU_CORBA_PYTHON_MAPPING"):
	test = SimpleClientServerTest ("excn: Python (with CORBA mapping)",
				       (python_command, "clientCORBA.py",),
				       (python_command, "serverCORBA.py"))
else:
	# the ILU mapping has no provision for raising system exceptions, so
	#  pass the -nosys flag to the client to tell it not to test them
	test = SimpleClientServerTest ("excn: Python (with ILU mapping) (no system exceptions)",
				       (python_command, "client.py", "-nosys",),
				       (python_command, "server.py"))

def run_test():

	try:
		print "**************************************************"
		test.run()
		print "**************************************************"
	except:
		sys.exit(1)

if __name__ == "__main__":
	run_test()
