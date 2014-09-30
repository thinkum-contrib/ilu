#
# From ILUTesting, we import "ilu_threaded", "java_command", "lisp_command", "guile_command", "python_command",
#   "iluconf_dict", "makefile_dict", "iludefs_dict", "run_tests", and, of course, "SimpleClientServerTest"
#

from ILUTesting import *
import sys, os, string

############################################################
#
#  First we set up a number of specific tests
#
############################################################

tests = []

if iludefs_dict.has_key("ADD_JAVA_LANGUAGE") and iludefs_dict.has_key("ADD_KERNEL_SHARED_LIBRARY") and iludefs_dict.has_key("ADD_PYTHON_LANGUAGE"):

	import os
	if (os.environ.has_key("LD_LIBRARY_PATH")):
		LD_LIBRARY_PATH = "." + os.pathsep + os.environ["LD_LIBRARY_PATH"]
	else:
		LD_LIBRARY_PATH = "."
	tests.append(SimpleSingleProgramTest("multlang3 Java/Python program",
					     java_command + " testprog foo bar bletch",
					     {"LD_LIBRARY_PATH" : LD_LIBRARY_PATH}))

if __name__ == "__main__":
	print 'CLASSPATH is', os.environ["CLASSPATH"]
	print 'LD_LIBRARY_PATH is', LD_LIBRARY_PATH
	print 'java_command is', java_command
	run_tests(tests)
