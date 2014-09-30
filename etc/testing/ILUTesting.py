# $Id: ILUTesting.py,v 1.29 1999/08/11 01:32:25 janssen Exp $
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

import ILUTestingMachinery
from ILUTestingMachinery import \
     iluconf_dict, \
     makefile_dict, \
     iludefs_dict, \
     prefix_environment_variable, \
     Process, \
     SimpleClientServerTest, \
     SimpleSingleProgramTest, \
     ClientSBHServerTest, \
     MultiClientMultiServerTest, \
     Client, \
     Server, \
     SBH_Server, \
     Program, \
     Logfile

import sys, os, string, tempfile, traceback

# delete temporary directory when process exits
def _delete_binding_directory(dir):
	import os
	os.system("rm -rf " + dir);

# get rid of some common ILU defs in the environment
if os.environ.has_key("ILUHOME"):
	del os.environ["ILUHOME"]
if os.environ.has_key("ILUPATH"):
	del os.environ["ILUPATH"]
if os.environ.has_key("ILUPATH_NO_ILUHOME"):
	del os.environ["ILUPATH_NO_ILUHOME"]
if os.environ.has_key("ILU_DEBUG"):
	del os.environ["ILU_DEBUG"]
if os.environ.has_key("ISLDEBUG"):
	del os.environ["ISLDEBUG"]

if iluconf_dict.has_key("ILU_BINDING_DIRECTORY"):
	# using shared files for simple binding
	filename = tempfile.mktemp()
	os.system("mkdir " + filename)
	if not (os.path.exists(filename) and os.path.isdir(filename)):
		raise "Couldn't make a temporary binding directory", filename
	ILUTestingMachinery.add_exit_function(lambda x=filename: _delete_binding_directory(x))
	os.environ["ILU_BINDING_DIRECTORY"] = filename
elif makefile_dict.has_key("ILUSB"):
	# using simple binding service for simple binding
	logfile = Logfile()
	bindingfile = Logfile()
	theSBProcess = Process(makefile_dict["ILUSB"], ("-p", "0", "-advertise", "-f", bindingfile.filename), {}, logfile)
	import time
	time.sleep(5)
	if not theSBProcess.active():
		raise "Can't start ilusb process"
	f = logfile.open()
	line = f.readline()	# logfile header with command line and env dict
	line = f.readline()[:-1]
	f.close()
	os.environ["ILU_BINDING_SERVICE"] = line
elif not makefile_dict.has_key("ILUHOME"):
	# not in a source directory, so don't bother trying to start simple binding
	pass
else:
	raise "Don't know how to set up simple binding."

if iludefs_dict.has_key("ADD_KERNEL_SHARED_LIBRARY"):
	prefix_environment_variable("LD_LIBRARY_PATH", makefile_dict["ILU_KERNEL_SHAREDLIB_DIRECTORY"])

if iludefs_dict.has_key("ADD_PYTHON_LANGUAGE"):
	python_command = os.path.join(makefile_dict["PYTHON_EXEC_PREFIX"], "bin", "python")
	if makefile_dict.has_key("ILUPYTHONLIBS"):
		prefix_environment_variable("PYTHONPATH", makefile_dict["ILUPYTHONLIBS"])
	else:
		prefix_environment_variable("PYTHONPATH", os.path.join(makefile_dict["ILUHOME"], "/lib"))
	os.environ["ILU_ASSERTION_FAILURE_ACTION"] = "-2"
	os.environ["ILU_MEMORY_FAILURE_ACTION"] = "-2"
	os.environ["ILU_CHECK_FAILURE_ACTION"] = "-2"

if iludefs_dict.has_key("ADD_JAVA_LANGUAGE") and makefile_dict.has_key("ILUHOME"):
	java_command = os.path.join(makefile_dict["ILUJAVA_M_JAVAHOME"], "bin", "java")
	prefix_environment_variable("CLASSPATH", makefile_dict["ILUJAVA_M_ILUCLASSES"])
	prefix_environment_variable("CLASSPATH", makefile_dict["ILUJAVA_M_CLASSESDIR"])
	prefix_environment_variable("LD_LIBRARY_PATH", makefile_dict["ILUJAVA_M_ILUMODULE_HOME"])

if iludefs_dict.has_key("ADD_COMMONLISP_LANGUAGE") and makefile_dict.has_key("ILUHOME"):
	# we cheat here -- we know LISP_BATCH_COMMAND is a set of tokens, rather than just one
	lisp_command = tuple(string.split(makefile_dict["LISP_BATCH_COMMAND"]))

if iludefs_dict.has_key("ADD_GUILE_LANGUAGE") and makefile_dict.has_key("ILUHOME"):
	guile_command = makefile_dict["ILUGUILE_COMMAND"]
	prefix_environment_variable("SCHEME_LOAD_PATH", makefile_dict["ILUGUILE_DIRECTORY"])
	prefix_environment_variable("LD_LIBRARY_PATH", makefile_dict["GUILE_LIB_DIR"])


ilu_threaded = (iluconf_dict.has_key("ILU_SOLARIS2_THREADS") or
		iluconf_dict.has_key("ILU_POSIX_THREADS") or
		iluconf_dict.has_key("ILU_DCE_THREADS") or
		iluconf_dict.has_key("ILU_WIN32_THREADS"))

def run_tests(tests, cleanup=None):

	exceptions = 0
	for test in tests:
		try:
			print "**************************************************"
			test.run()
			sys.stdout.flush()
		except:
			traceback.print_exc()
			exceptions = exceptions + 1
	if tests:
		print "**************************************************"
	else:
		print "***************   No Tests   *********************"
	if cleanup:
		cleanup()
	if (exceptions > 0):
		sys.exit(1)
	else:
		sys.exit(0)

