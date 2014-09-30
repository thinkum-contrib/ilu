#
# From ILUTesting, we import "ilu_threaded", "java_command", "lisp_command", "guile_command", "python_command",
#   "iluconf_dict", "makefile_dict", "iludefs_dict", "run_tests", and, of course, "SimpleClientServerTest"
#

from ILUTesting import *
import sys, os, string

tests = []

############################################################
#
#  First we set up a number of specific tests
#
############################################################

default_protocol = iluconf_dict["ILU_DEFAULT_PROTOCOL_INFO"]
if len(default_protocol) >= 6 and default_protocol[0:6] == '"iiop"':
	nil_objs_switch = ' -allow-different-types-on-nil-objects'
else:
	nil_objs_switch = ''

if iludefs_dict.has_key("ADD_PYTHON_LANGUAGE"):

	tests.append(SimpleClientServerTest("pickle: Python client / Python server, IIOP protocol",
					    python_command + " pyClient.py -allow-different-types-on-nil-objects", python_command + " pyServer.py"))

	if iludefs_dict.has_key("ADD_C_LANGUAGE"):

		tests.append(SimpleClientServerTest("pickle: Python client / C server, default protocol",
						    python_command + " pyClient.py" + nil_objs_switch,
						    "server"))

		if iluconf_dict.has_key("HTTP_PROTOCOL"):
			tests.append(SimpleClientServerTest("pickle: Python client / C server, HTTP 1.1 protocol",
							    python_command + " pyClient.py",
							    "server -p http_1_1 -t tcp_0_0"))

		if iluconf_dict.has_key("IIOP_PROTOCOL"):
			tests.append(SimpleClientServerTest("pickle: Python client / C server, IIOP protocol",
							    python_command + " pyClient.py -allow-different-types-on-nil-objects",
							    "server -p iiop -t tcp_0_0"))

		if iluconf_dict.has_key("SUNRPC_PROTOCOL") and iluconf_dict.has_key("SUNRPCRM_TRANSPORT"):
			tests.append(SimpleClientServerTest("pickle: Python client / C server, Sun RPC protocol with sunrpcrm/tcp transport",
							    python_command + " pyClient.py",
							    "server -p sunrpc -t sunrpcrm tcp_0_0"))

		if iluconf_dict.has_key("COURIER_PROTOCOL") and iluconf_dict.has_key("SUNRPCRM_TRANSPORT"):
			tests.append(SimpleClientServerTest("pickle: Python client / C server, Courier protocol with sunrpcrm/tcp transport",
							    python_command + " pyClient.py",
							    "server -p courier -t sunrpcrm tcp_0_0"))

		if iluconf_dict.has_key("W3NG_PROTOCOL") and iluconf_dict.has_key("SUNRPCRM_TRANSPORT"):
			tests.append(SimpleClientServerTest("pickle: Python client / C server, w3ng protocol with sunrpcrm/tcp transports",
							    python_command + " pyClient.py",
							    "server -p w3ng -t sunrpcrm tcp_0_0"))

	if iludefs_dict.has_key("ADD_CORBA_CPLUSPLUS_LANGUAGE"):

		tests.append(SimpleClientServerTest("pickle: Python client / CORBA C++ server, default protocol",
						    python_command + " pyClient.py" + nil_objs_switch,
						    "server-cpp"))

	if iludefs_dict.has_key("ADD_JAVA_LANGUAGE"):

		test = SimpleClientServerTest("pickle: Python client / Java server, default protocol",
					      python_command + " pyClient.py" + nil_objs_switch,
					      java_command + " Test.JaServ")
		test.server_startup_delay = 20
		tests.append(test)

	if iludefs_dict.has_key("ADD_COMMONLISP_LANGUAGE"):

		ldir = makefile_dict["ILU_LISP_DIR"]
		test = SimpleClientServerTest("pickle: Python client / Lisp server, default protocol",
					      python_command + " pyClient.py" + nil_objs_switch,
					      lisp_command + ('-e', '(load "' + ldir + '/pdefsys")',
							      '-e', '(load "' + ldir + '/ilu-sysdcl")',
							      '-e', '(load "test2-sysdcl")',
							      '-e', '(pdefsys:compile-system :test2)',
							      '-e', '(pdefsys:compile-system :test2-server)',
							      '-e', '(pdefsys:load-system :test2-server)',
							      '-e', '(load "server.lisp")',
							      '-e', '(pickle-test:start-server)',
							      '-e', '(sleep 10000)'));
		test.server_startup_delay = 30
		tests.append(test)

############################################################
#
#  The main() routine
#
############################################################

if __name__ == "__main__":
	run_tests(tests)
