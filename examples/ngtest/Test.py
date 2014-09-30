#
# From ILUTesting, we import "ilu_threaded", "java_command", "lisp_command", "guile_command", "python_command",
#   "iluconf_dict", "makefile_dict", "iludefs_dict", "run_tests", and, of course, "SimpleClientServerTest", 
#

from ILUTesting import *
import sys, os, string, posix, re, socket

thishost = socket.gethostname()
filebase = os.path.join(os.getcwd(), "url-test-material", "serverdocs")
tempfiles = []

tests = []

def transform_urls():

	# create a copy of each file "mscape_modelhost_*", calling it
	# "mscape_<thishost>_*", and change each line in the file similarly

	import socket
	thishost = socket.gethostname()
	urls_dir = os.path.join("url-test-material", "ngtesturls")
	urls_files = posix.listdir(urls_dir)
	for file in urls_files:
		m = re.match(r"mscape_modelhost_(.*)", file)
		if m:
			newfile = "mscape_" + thishost + "_" + m.group(1);
			fp_in = open(os.path.join(urls_dir, file), "r")
			newfilepath = os.path.join(urls_dir, newfile)
			fp_out = open(newfilepath, "w")
			tempfiles.append(newfilepath)
			line = fp_in.readline()
			while line:
				newline = re.sub("modelhost", thishost, line)
				fp_out.write(newline)
				line = fp_in.readline()
			fp_in.close()
			fp_out.close()


def remove_tempfiles():
	for pathname in tempfiles:
		os.unlink(pathname)

def urls_file (site, protocol, transport, port):
	urlfile = "%s/url-test-material/ngtesturls/%s_%s_w3ng_%s" % (os.getcwd(),
								     site, thishost, protocol)
	if transport:
		urlfile = urlfile + "_" + transport
	urlfile = urlfile + "_" + str(port) + ".urls"
	return urlfile

############################################################
#
#  The Main routine
#
############################################################

if __name__ == "__main__":

	print 'transforming the url files...'
	transform_urls()

	############################################################
	#
	#  First we set up a number of specific tests
	#
	############################################################

	if iluconf_dict.has_key("W3NG_PROTOCOL") and iluconf_dict.has_key("W3MUX_TRANSPORT"):
	    test = SimpleClientServerTest("NG Base:  w3ng/w3mux on mscape with batching",
					  "./nggetbot -urls " +
					  urls_file("mscape", "w3ng", "w3mux", 2718) +
					  " -batch 5000 -logfile stdout -v 2 -mt 10",
					  "./ngwebserver -httptinfo tcp_0_8080 -filebase " +
					  filebase + " -batch 50000 -verbose 2")
	    test.server_shutdown_delay = 20
	    tests.append(test);

	    test = SimpleClientServerTest("Pipelined NG:  w3ng/w3mux on mscape with batching, pipelined",
						    "./nggetbot -urls " +
						    urls_file("mscape", "w3ng", "w3mux", 2718) +
						    " -batch 5000 -logfile stdout -v 2 -mt 10 -pipeline -serial",
						    "./ngwebserver -httptinfo tcp_0_8080 -filebase " +
						    filebase + " -batch 50000 -verbose 2")
	    test.server_shutdown_delay = 20
	    tests.append(test);


	    if iluconf_dict.has_key("SUNRPCRM_TRANSPORT"):
		test = SimpleClientServerTest("Pipelined Sunrpcrm NG:  w3ng/sunrpcrm on mscape with batching, pipelined",
					      "./nggetbot -urls " +
					      urls_file("mscape", "w3ng", "sunrpcrm", 2718) +
					      " -batch 5000 -logfile stdout -v 2 -mt 10 -pipeline -serial",
					      "./ngwebserver -httptinfo tcp_0_8080 -filebase " +
					      filebase + " -batch 50000 -verbose 2 -ngtinfo sunrpcrm")
		test.server_shutdown_delay = 20
		tests.append(test);


	    test = SimpleClientServerTest("Batchless NG:  w3ng/w3mux on mscape without batching",
					  "./nggetbot -urls " +
					  urls_file("mscape", "w3ng", "w3mux", 2718) +
					  " -logfile stdout -v 2 -mt 10",
					  "./ngwebserver -httptinfo tcp_0_8080 -filebase " +
					  filebase + " -verbose 2")
	    test.server_shutdown_delay = 20
	    tests.append(test);


	    test = SimpleClientServerTest("Async NG:  w3ng/w3mux on mscape with async API",
					  "./nggetbot -urls " +
					  urls_file("mscape", "w3ng", "w3mux", 2718) +
					  " -batch 50000 -logfile stdout -v 2 -mt 10 -sink",
					  "./ngwebserver -httptinfo tcp_0_8080 -filebase " +
					  filebase + " -batch 50000 -verbose 2")
	    test.server_shutdown_delay = 20
	    tests.append(test);


	if iluconf_dict.has_key("IIOP_PROTOCOL"):
	    test = SimpleClientServerTest("IIOP NG:  IIOP on mscape with batching",
					  "./nggetbot -urls " +
					  urls_file("mscape", "iiop", None, 2718) +
					  " -batch 5000 -logfile stdout -v 2 -mt 10",
					  "./ngwebserver -httptinfo tcp_0_8080 " +
					  " -ngpinfo iiop -ngtinfo -fdbudget 128 -filebase " +
					  filebase + " -batch 50000 -verbose 2")
	    test.server_shutdown_delay = 20
	    tests.append(test);


	test = SimpleClientServerTest("Multi-connection HTTP:  HTTP 1.0 on mscape",
				      "./nggetbot -urls " +
				      urls_file("mscape", "http10", None, 2718) +
				      " -httppinfo http_1_0 -httptinfo tcp_0_8080" +
				      " -ngpinfo http_1_0 -ngtinfo -logfile stdout -v 2 -mt 10",
				      "./ngwebserver -httppinfo http_1_0 -httptinfo tcp_0_8080" +
				      " -ngpinfo http_1_0 -ngtinfo -filebase " +
				      filebase + " -verbose 2 -fdbudget 128")
	test.server_shutdown_delay = 20
	tests.append(test);

	test = SimpleClientServerTest("Pipelined HTTP:  HTTP 1.1 on mscape",
				      "./nggetbot -urls " +
				      urls_file("mscape", "http11", None, 2718) +
				      " -logfile stdout -v 2 -mt 10 -pipeline -serial -batch 50000",
				      "./ngwebserver -httptinfo tcp_0_8080" +
				      " -ngpinfo http_1_1 -ngtinfo tcp_0_2718 -filebase " +
				      filebase + " -batch 50000 -verbose 2")
	test.server_shutdown_delay = 20
	tests.append(test);


	############################################################
	#
	#  then we run them (and remove the munged URL files)
	#
	############################################################

	run_tests(tests, remove_tempfiles)
