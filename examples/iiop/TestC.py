from ILUTesting import *
import sys

tests = []

#
# Can't test w3ng/webmux because server doesn't run threaded.
#

if iluconf_dict.has_key("SUNRPC_PROTOCOL") and iluconf_dict.has_key("SUNRPCRM_TRANSPORT"):
	tests.append(SimpleClientServerTest("iioptest: C with Sun RPC protocol and sunrpcrm/tcp transports",
					    "client",
					    "server -p sunrpc -t sunrpcrm tcp_0_0"))

if iluconf_dict.has_key("HTTP_PROTOCOL"):
	tests.append(SimpleClientServerTest("iioptest: C with HTTP protocol",
					    "client",
					    "server -p http -t tcp_0_0"))

if iluconf_dict.has_key("IIOP_PROTOCOL"):
	tests.append(SimpleClientServerTest("iioptest: C with IIOP protocol",
					    "client -n 10",
					    "server -p iiop -t tcp_0_0"))

	# test explicit IOR passing...
	tests.append(ClientSBHServerTest("iioptest: C with IIOP protocol, using explicit IOR passing",
					 "client -n 10 -O $SBH$",
					 "server -p iiop -t tcp_0_0"))

if iluconf_dict.has_key("COURIER_PROTOCOL"):
	tests.append(SimpleClientServerTest("iioptest: C with Courier protocol and sunrpcrm/tcp transports",
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
