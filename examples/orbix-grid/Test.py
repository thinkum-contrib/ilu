from ILUTesting import *

tests = []

if not iluconf_dict.has_key("ILU_CORBA_PYTHON_MAPPING"):
	tests.append(ClientSBHServerTest ("orbix-grid: Python (with ILU mapping)",
					  (python_command, "gridClient.py", "$SBH$",),
					  (python_command, "gridServer.py")))

if __name__ == "__main__":
	run_tests(tests)
