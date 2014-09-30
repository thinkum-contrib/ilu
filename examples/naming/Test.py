from ILUTesting import *
import tempfile, os

CosNamingExecutable = makefile_dict["ILU_COS_NAMING_IMPL"]
tmpfile = tempfile.mktemp()

tests = []

if (iludefs_dict.has_key("ADD_PYTHON_LANGUAGE") and not iludefs_dict.has_key("ILU_CORBA_PYTHON_MAPPING")):
	tests.append(MultiClientMultiServerTest("CosNaming with Python client and server",
						((python_command, "client.py", "-name", "foo/bar/bletch.o", "-ior_file", tmpfile),),
						((CosNamingExecutable, "-p", "0", "-ior_to_file", tmpfile),
						 (python_command, "server.py", "-n", "foo/bar/bletch.o", "-ior_file", tmpfile))))

if (iludefs_dict.has_key("ADD_PYTHON_LANGUAGE") and (not iludefs_dict.has_key("ILU_CORBA_PYTHON_MAPPING")) and iludefs_dict.has_key("ADD_C_LANGUAGE")):
	tests.append(MultiClientMultiServerTest("CosNaming with Python client and C server",
						((python_command, "client.py", "-name", "foo/bar/bletch.o", "-ior_file", tmpfile),),
						((CosNamingExecutable, "-p", "0", "-ior_to_file", tmpfile),
						 ("server", "-n", "foo/bar/bletch.o", "-ior_file", tmpfile))))

if iludefs_dict.has_key("ADD_C_LANGUAGE"):
	tests.append(MultiClientMultiServerTest("CosNaming with C client server",
						(("client", "-name", "foo/bar/bletch.o", "-ior_file", tmpfile),),
						((CosNamingExecutable, "-p", "0", "-ior_to_file", tmpfile),
						 ("server", "-n", "foo/bar/bletch.o", "-ior_file", tmpfile))))


if __name__ == "__main__":
	run_tests(tests, lambda x=tmpfile: os.unlink(x))
