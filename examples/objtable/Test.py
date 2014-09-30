from ILUTesting import *
import sys

tests = []

# test explicit IOR passing...

if iludefs_dict.has_key("ADD_PYTHON_LANGUAGE") and not iludefs_dict.has_key("ILU_CORBA_PYTHON_MAPPING"):
	tests.append(ClientSBHServerTest("Python test of object tables (with ILU mapping)",
					 "client $SBH$",
					 python_command + " server.py objtable-test." + iluconf_dict["FULLY_QUALIFIED_DOMAIN_NAME"]))

tests.append(ClientSBHServerTest("C test of object tables",
				 "client $SBH$", "server"))

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
