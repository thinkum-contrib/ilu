from ILUTesting import *
import sys, socket

tests = []

tests.append(SimpleClientServerTest("cpp2foo: CORBA C++ with default protocol and transport",
				    "fooclient",
				    "fooserver"))

if ilu_threaded:
    tests.append(SimpleClientServerTest("cpp2foo: CORBA C++, threaded, with default protocol and transport",
					"fooclient",
					"fooserver -mt"))

if iluconf_dict.has_key("COURIER_PROTOCOL") and iluconf_dict.has_key("SUNRPCRM_TRANSPORT"):
    tests.append(SimpleClientServerTest("cpp2foo: CORBA C++ with Courier protocol and sunrpcrm transport",
					"fooclient",
					"fooserver -p courier -t sunrpcrm tcp_0_0"))

if iluconf_dict.has_key("HTTP_PROTOCOL") and iluconf_dict.has_key("SUNRPCRM_TRANSPORT"):
    tests.append(SimpleClientServerTest("cpp2foo: CORBA C++ with HTTP protocol and sunrpcrm transport",
					"fooclient",
					"fooserver -p http -t sunrpcrm tcp_0_0"))

if iluconf_dict.has_key("SUNRPC_PROTOCOL") and iluconf_dict.has_key("SUNRPCRM_TRANSPORT"):
    tests.append(SimpleClientServerTest("cpp2foo: CORBA C++ with Sun RPC protocol and sunrpcrm transport",
					"fooclient",
					"fooserver -p sunrpc -t sunrpcrm tcp_0_0"))
    tests.append(SimpleClientServerTest("cpp2foo: CORBA C++ with Concurrent Sun RPC protocol and sunrpcrm transport",
					"fooclient",
					"fooserver -p csunrpc -t sunrpcrm tcp_0_0"))

if iluconf_dict.has_key("IIOP_PROTOCOL") and iluconf_dict.has_key("SUNRPCRM_TRANSPORT"):
    tests.append(SimpleClientServerTest("cpp2foo: CORBA C++ with IIOP protocol and sunrpcrm transport",
					"fooclient",
					"fooserver -p iiop -t sunrpcrm tcp_0_0"))

if iluconf_dict.has_key("W3NG_PROTOCOL") and iluconf_dict.has_key("SUNRPCRM_TRANSPORT"):
    tests.append(SimpleClientServerTest("cpp2foo: CORBA C++ with w3ng protocol and sunrpcrm transport",
					"fooclient",
					"fooserver -p w3ng -t sunrpcrm tcp_0_0"))

if ilu_threaded and iluconf_dict.has_key("W3NG_PROTOCOL") and iluconf_dict.has_key("W3MUX_TRANSPORT"):
    tests.append(SimpleClientServerTest("cpp2foo: CORBA C++ with w3ng protocol and w3mux transport",
					"fooclient " + socket.gethostname() + " mt",
					"fooserver -mt -p w3ng -t w3mux tcp_0_0"))

if __name__ == "__main__":
	run_tests(tests)
