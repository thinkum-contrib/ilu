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

if iludefs_dict.has_key("ADD_C_LANGUAGE"):

	tests.append(SimpleClientServerTest("test1:  C client / C server, default protocol",
					    "client",
					    "server -nossl"))

	if iluconf_dict.has_key("SUNRPC_PROTOCOL") and iluconf_dict.has_key("SUNRPCRM_TRANSPORT"):
		tests.append(SimpleClientServerTest("test1:  C/C with Sun RPC protocol and sunrpcrm/tcp transports",
						    "client",
						    "server -p sunrpc -t sunrpcrm tcp_0_0"))
		tests.append(SimpleClientServerTest("test1:  C/C with concurrent Sun RPC protocol and sunrpcrm/tcp transports",
						    "client",
						    "server -p csunrpc -t sunrpcrm tcp_0_0"))
		if ilu_threaded:
			tests.append(SimpleClientServerTest("test1:  C/C with Sun RPC protocol and sunrpcrm/tcp transports, threaded",
							    "client -mt",
							    "server -mt -p sunrpc -t sunrpcrm tcp_0_0"))
			tests.append(SimpleClientServerTest("test1:  C/C with concurrent Sun RPC protocol and sunrpcrm/tcp transports, threaded",
							    "client -mt",
							    "server -mt -p csunrpc -t sunrpcrm tcp_0_0"))

	if iluconf_dict.has_key("HTTP_PROTOCOL"):
		tests.append(SimpleClientServerTest("test1:  C/C with HTTP 1.0 protocol and tcp transport",
						    "client",
						    "server -p http_1_0 -t tcp_0_0"))
		tests.append(SimpleClientServerTest("test1:  C/C with HTTP 1.0p protocol and tcp transport",
						    "client",
						    "server -p http_1_0p -t tcp_0_0"))
		tests.append(SimpleClientServerTest("test1:  C/C with HTTP 1.1 protocol and tcp transport",
						    "client",
						    "server -p http_1_1 -t tcp_0_0"))
		if ilu_threaded:
			tests.append(SimpleClientServerTest("test1:  C/C with HTTP protocol and tcp transport, threaded",
							    "client -mt",
							    "server -mt -p http -t tcp_0_0"))

	if iluconf_dict.has_key("IIOP_PROTOCOL"):
		tests.append(SimpleClientServerTest("test1:  C/C with IIOP protocol over tcp",
						    "client",
						    "server -p iiop -t tcp_0_0"))
		if ilu_threaded:
			tests.append(SimpleClientServerTest("test1:  C/C with IIOP protocol over tcp, threaded",
							    "client -mt",
							    "server -mt -p iiop -t tcp_0_0"))

	if ilu_threaded and iluconf_dict.has_key("W3NG_PROTOCOL") and iluconf_dict.has_key("W3MUX_TRANSPORT"):
		tests.append(SimpleClientServerTest("test1:  C/C with w3ng protocol and w3mux/tcp transports, threaded",
						    "client -mt",
						    "server -mt -p w3ng -t w3mux tcp_0_0"))

	if iluconf_dict.has_key("COURIER_PROTOCOL") and iluconf_dict.has_key("SUNRPCRM_TRANSPORT"):
		tests.append(SimpleClientServerTest("test1:  C/C with Courier protocol and sunrpcrm/tcp transports",
						    "client",
						    "server -p courier -t sunrpcrm tcp_0_0"))

if iludefs_dict.has_key("ADD_ILU_CPLUSPLUS_LANGUAGE"):

	tests.append(SimpleClientServerTest("test1:  ILU-C++ client/ILU-C++ server, default protocol and transport",
					    "cppclient",
					    "cppserver"))
	if ilu_threaded:
		tests.append(SimpleClientServerTest("test1:  ILU-C++ client/ILU-C++ server, default protocol and transport, threaded",
						    "cppclient",
						    "cppserver"))

if iludefs_dict.has_key("ADD_CORBA_CPLUSPLUS_LANGUAGE"):

	tests.append(SimpleClientServerTest("test1:  CORBA-C++ client / CORBA-C++ server, default protocol and transport",
					    "cpp2client",
					    "cpp2server"))
	if ilu_threaded:
		tests.append(SimpleClientServerTest("test1:  CORBA-C++ client / CORBA-C++ server, default protocol and transport, threaded",
						    "cpp2client",
						    "cpp2server"))

if iludefs_dict.has_key("ADD_PYTHON_LANGUAGE") and not iluconf_dict.has_key("ILU_CORBA_PYTHON_MAPPING"):
	tests.append(SimpleClientServerTest("test1:  Python client / Python server, default protocol and transport",
					    (python_command, "pyClient.py",),
					    (python_command, "pyServer.py",)))
	if ilu_threaded and iludefs_dict.has_key("ILU_PYTHON_HAS_THREADS"):
		tests.append(SimpleClientServerTest("test1:  Python client / Python server, default protocol and transport, threaded",
						    (python_command, "pyClient.py", "-mt",),
						    (python_command, "pyServer.py", "-mt",)))
		if iluconf_dict.has_key("IIOP_PROTOCOL"):
			tests.append(SimpleClientServerTest("test1:  Python/Python, threaded, with IIOP protocol over tcp",
						    (python_command, "pyClient.py", "-mt",),
						    (python_command, "pyServer.py", "-mt", "-p", "iiop", "-t", "tcp_0_0",)))
		elif iluconf_dict.has_key("SUNRPC_PROTOCOL"):
			tests.append(SimpleClientServerTest("test1:  Python/Python, threaded, with concurrent Sun RPC protocol over sunrpcrm/tcp",
						    (python_command, "pyClient.py", "-mt",),
						    (python_command, "pyServer.py", "-mt", "-p", "csunrpc", "-t", "sunrpcrm", "tcp_0_0",)))

if iludefs_dict.has_key("ADD_COMMONLISP_LANGUAGE"):

	test = SimpleClientServerTest("test1:  Lisp client / Lisp server, default protocol",
				      lisp_command + ('-e', '(load "load-lisp-example")',
						      '-e', '(test-server)',
						      '-e', '(exit)'),
				      lisp_command + ('-e', '(load "load-lisp-example")',
						      '-e', '(test1-server:start-server)',
						      '-e', '(sleep 10000)'))
	# lisp can be a bit sluggish, so give it 15 seconds before starting the client
	test.server_startup_delay = 15
	tests.append(test)

if iludefs_dict.has_key("ADD_JAVA_LANGUAGE"):

	test = SimpleClientServerTest("test1:  Java client / Java server, default protocol",
				      (java_command, 'test01.TestClient',),
				      (java_command, 'test01.TestServer',))
	# Java too can be a bit sluggish, so...
	test.server_startup_delay = 20
	tests.append(test)

if iludefs_dict.has_key("ADD_GUILE_LANGUAGE"):

	test = SimpleClientServerTest("test1:  Guile client / Guile server, default protocol",
				      (guile_command, 'client.scm',),
				      (guile_command, 'server.scm',))
	# Guile is *really* slow, so wait a full minute before trying to run the client
	test.server_startup_delay = 60
	tests.append(test)

############################################################
#
#  OK, now try some combos
#
############################################################

languages = [ "C", "C++ (ILU)", "C++ (CORBA)", "Python (ILU)", "Java", "Guile", "Common Lisp" ]

makefile_keys = [ "ADD_C_LANGUAGE",
		  "ADD_ILU_CPLUSPLUS_LANGUAGE",
		  "ADD_CORBA_CPLUSPLUS_LANGUAGE",
		  "ADD_PYTHON_LANGUAGE",
		  "ADD_JAVA_LANGUAGE",
		  "ADD_GUILE_LANGUAGE",
		  "ADD_COMMONLISP_LANGUAGE" ]		  

servers = [	"server -nossl",					# C
		"cppserver",						# C++ (ILU)
		"cpp2server",						# C++ (CORBA)
		((iludefs_dict.has_key("ADD_PYTHON_LANGUAGE") and
		  python_command) or "foo") + " pyServer.py",		# Python
		((iludefs_dict.has_key("ADD_JAVA_LANGUAGE") and
		  java_command) or "foo") + " test01.TestServer",	# Java
		((iludefs_dict.has_key("ADD_GUILE_LANGUAGE") and
		  guile_command) or "foo") + " server.scm",		# Guile
		((iludefs_dict.has_key("ADD_COMMONLISP_LANGUAGE") and
		  lisp_command) or ("foo",)) +
		('-e', '(load "load-lisp-example")',
		 '-e', '(test1-server:start-server)',
		 '-e', '(sleep 10000)')					# Common Lisp
		];

clients = [	"client",						# C
		"cppclient",						# C++ (ILU)
		"cpp2client",						# C++ (CORBA)
		((iludefs_dict.has_key("ADD_PYTHON_LANGUAGE") and
		  python_command) or "foo") + " pyClient.py",		# Python
		((iludefs_dict.has_key("ADD_JAVA_LANGUAGE") and
		  java_command) or "foo") + " test01.TestClient",	# Java
		((iludefs_dict.has_key("ADD_GUILE_LANGUAGE") and
		  guile_command) or "foo") + " client.scm",		# Guile
		((iludefs_dict.has_key("ADD_COMMONLISP_LANGUAGE") and
		  lisp_command) or ("foo",)) +
		('-e', '(load "load-lisp-example")',
		 '-e', '(test-server)',
		 '-e', '(exit)')					# Common Lisp
		];
		
def make_test(i1, i2):
	name = "test1:  %s client / %s server, default protocol" % (languages[i1], languages[i2])
	client = clients[i1];
	server = servers[i2];
	test = SimpleClientServerTest(name, client, server)
	if languages[i2] == 'Guile':
		test.server_startup_delay = 60
	elif languages[i2] == 'Java':
		test.server_startup_delay = 20
	elif languages[i2] == 'Common Lisp':
		test.server_startup_delay = 15
	return test

for i1 in range(len(languages)):
	for i2 in range(i1 + 1,len(languages)):
		if (iludefs_dict.has_key(makefile_keys[i1]) and iludefs_dict.has_key(makefile_keys[i2]) and
		    ((languages[i1] != 'Python (ILU)' and languages[i2] != 'Python (ILU)') or
		     not iluconf_dict.has_key("ILU_CORBA_PYTHON_MAPPING"))):
			tests.append(make_test(i1, i2))
			tests.append(make_test(i2, i1))

############################################################
#
#  The main() routine
#
############################################################

if __name__ == "__main__":
	run_tests(tests)
