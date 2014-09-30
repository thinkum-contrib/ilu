from ILUTesting import *
import sys

tests = []

if iludefs_dict.has_key("ADD_PYTHON_LANGUAGE") and not iluconf_dict.has_key("ILU_CORBA_PYTHON_MAPPING"):
	tests.append(SimpleClientServerTest("Python test of reference passing with default protocol",
					    (python_command, "client.py",),
					    (python_command, "server.py",)))

def run_test():

	exceptions = 0
	for test in tests:
		try:
			print "**************************************************"
			test.run()
		except:
			exceptions = exceptions + 1
	if tests:
		print "**************************************************"
	else:
		print "***************   No Tests   *********************"
	if (exceptions > 0):
		sys.exit(1)
	else:
		sys.exit(0)

if __name__ == "__main__":
	run_test()
