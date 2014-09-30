from ILUTesting import *
import sys

tests = []

tests.append(SimpleClientServerTest("excn: C with default protocol and transport",
				    "client",
				    "server"))

if iluconf_dict.has_key("W3NG_PROTOCOL"):
	if ilu_threaded and iluconf_dict.has_key("W3MUX_TRANSPORT"):
		tests.append(SimpleClientServerTest("excn: C, threaded, with w3ng/w3mux",
						    "client -mt",
						    "server -mt -p w3ng -t w3mux tcp_0_0"))
	elif iluconf_dict.has_key("SUNRPCRM_TRANSPORT"):
		tests.append(SimpleClientServerTest("excn: C with w3ng/sunrpcrm",
						    "client",
						    "server -p w3ng -t sunrpcrm tcp_0_0"))

if iluconf_dict.has_key("SUNRPC_PROTOCOL") and iluconf_dict.has_key("SUNRPCRM_TRANSPORT"):
	tests.append(SimpleClientServerTest("excn: C with Sun RPC protocol",
					    "client",
					    "server -p sunrpc -t sunrpcrm tcp_0_0"))

if iluconf_dict.has_key("HTTP_PROTOCOL"):
	tests.append(SimpleClientServerTest("excn: C with HTTP protocol (no system exceptions, currently broken)",
					    "client -nosys",
					    "server -p http -t tcp_0_0"))

if iluconf_dict.has_key("IIOP_PROTOCOL"):
	tests.append(SimpleClientServerTest("excn: C with IIOP protocol",
					    "client",
					    "server -p iiop -t tcp_0_0"))

if iluconf_dict.has_key("COURIER_PROTOCOL") and iluconf_dict.has_key("SUNRPCRM_TRANSPORT"):
	tests.append(SimpleClientServerTest("excn: C with Courier protocol",
					    "client",
					    "server -p courier -t sunrpcrm tcp_0_0"))

def run_test():

	exceptions = 0
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
