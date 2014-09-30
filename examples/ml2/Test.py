#
# From ILUTesting, we import "ilu_threaded", "java_command", "lisp_command", "guile_command", "python_command",
#   "iluconf_dict", "makefile_dict", "iludefs_dict", and, of course, "SimpleClientServerTest"
#

from ILUTesting import *
import sys, os, string

tests = []

############################################################
#
#  First we set up a number of specific tests
#
############################################################

if iludefs_dict.has_key("ADD_PYTHON_LANGUAGE"):

	tests.append(SimpleSingleProgramTest("multlang2 Python/C++ program",
					     python_command + " testprog.py foo bar bletch"));

	if iludefs_dict.has_key("ILU_PYTHON_HAS_THREADS"):
		tests.append(SimpleSingleProgramTest("multlang2 Python/C++ program, threaded",
						     python_command + " testprog.py -mt foo bar bletch"));

if iludefs_dict.has_key("ADD_JAVA_LANGUAGE") and iludefs_dict.has_key("ADD_KERNEL_SHARED_LIBRARY"):

	import os
	if (os.environ.has_key("LD_LIBRARY_PATH")):
		LD_LIBRARY_PATH = "." + os.pathsep + os.environ["LD_LIBRARY_PATH"]
	else:
		LD_LIBRARY_PATH = "."
	tests.append(SimpleSingleProgramTest("multlang2 Java/C++ program",
					     java_command + " testprog foo bar bletch",
					     {"LD_LIBRARY_PATH" : LD_LIBRARY_PATH}))

def run_test():

	exceptions = 0
	if len(tests) < 1:
		print 'No tests.'
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
