from ILUTesting import *

tests = []

if iluconf_dict.has_key("IIOP_PROTOCOL"):

	if not iludefs_dict.has_key("ILU_CORBA_PYTHON_MAPPING"):
		tests.append(SimpleClientServerTest("relocate:  Python with IIOP protocol",
						    (python_command, "client.py",),
						    (python_command, "manager.py")))

		if iludefs_dict.has_key("ADD_CORBA_CPLUSPLUS_LANGUAGE"):
			tests.append(SimpleClientServerTest("relocate:  Python with C++ sprogram, IIOP protocol",
							    (python_command, "client.py",),
							    (python_command, "manager.py", "-e", "./sprogram-cpp")))

run_tests(tests)
