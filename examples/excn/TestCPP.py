from ILUTesting import *
import sys

tests = []

tests.append(SimpleClientServerTest("excn: CORBA C++ with default protocol and transport",
				    "cppclient",
				    "cppserver"))

tests.append(SimpleClientServerTest("excn: CORBA C++ client, C server, with default protocol and transport",
				    "cppclient",
				    "server"))

if iluconf_dict.has_key("W3NG_PROTOCOL"):
	if ilu_threaded and iluconf_dict.has_key("W3MUX_TRANSPORT"):
		tests.append(SimpleClientServerTest("excn: CORBA C++, threaded, with w3ng/w3mux",
						    "cppclient -mt",
						    "cppserver -mt -p w3ng -t w3mux tcp_0_0"))
	elif iluconf_dict.has_key("SUNRPCRM_TRANSPORT"):
		tests.append(SimpleClientServerTest("excn: CORBA C++ with w3ng/sunrpcrm",
						    "cppclient",
						    "cppserver -p w3ng -t sunrpcrm tcp_0_0"))

if iluconf_dict.has_key("SUNRPC_PROTOCOL") and iluconf_dict.has_key("SUNRPCRM_TRANSPORT"):
	tests.append(SimpleClientServerTest("excn: CORBA C++ with Sun RPC protocol",
					    "cppclient",
					    "cppserver -p sunrpc -t sunrpcrm tcp_0_0"))

if iluconf_dict.has_key("HTTP_PROTOCOL"):
	tests.append(SimpleClientServerTest("excn: CORBA C++ with HTTP protocol (no system exceptions, currently broken)",
					    "cppclient -nosys",
					    "cppserver -p http -t tcp_0_0"))

if iluconf_dict.has_key("IIOP_PROTOCOL"):
	tests.append(SimpleClientServerTest("excn: CORBA C++ with IIOP protocol",
					    "cppclient",
					    "cppserver -p iiop -t tcp_0_0"))

if iluconf_dict.has_key("COURIER_PROTOCOL") and iluconf_dict.has_key("SUNRPCRM_TRANSPORT"):
	tests.append(SimpleClientServerTest("excn: CORBA C++ with Courier protocol",
					    "cppclient",
					    "cppserver -p courier -t sunrpcrm tcp_0_0"))

if __name__ == "__main__":
	run_tests(tests)
