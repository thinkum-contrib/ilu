# this file contains a tuple of triples of test name, client invocation, and server invocation.
#
non_nt_test_descriptions = (
			("Base-NG",
			 "/opt/local/ilu/examples/ngtest/nggetbot -urls /opt/local/http-ng/ngtesturls/mscape_ngsun2_w3ng_w3ng_w3mux_2718.urls -batch 50000 -logfile stdout -v 1 -mt 10",
			 "/opt/local/ilu/examples/ngtest/ngwebserver -httptinfo tcp_0_8080 -filebase /opt/local/http-ng/serverdocs -batch 50000 -verbose 1"),

			("Multi-connection_HTTP",
			 "/opt/local/ilu/examples/ngtest/nggetbot -urls /opt/local/http-ng/ngtesturls/mscape_ngsun2_w3ng_http10_2718.urls -httppinfo http_1_0 -httptinfo tcp_0_8080 -ngpinfo http_1_0 -ngtinfo -logfile stdout -v 1 -mt 10",
			 "/opt/local/ilu/examples/ngtest/ngwebserver -httppinfo http_1_0 -httptinfo tcp_0_8080 -ngpinfo http_1_0 -ngtinfo -filebase /opt/local/http-ng/serverdocs -verbose 1 -fdbudget 128"),

			("Pipelined_HTTP",
			 "/opt/local/ilu/examples/ngtest/nggetbot -urls /opt/local/http-ng/ngtesturls/mscape_ngsun2_w3ng_http11_2718.urls -batch 50000 -logfile stdout -v 1 -mt 10 -pipeline -serial",
			 "/opt/local/ilu/examples/ngtest/ngwebserver -httptinfo tcp_0_8080 -ngpinfo http_1_1 -ngtinfo tcp_0_2718 -filebase /opt/local/http-ng/serverdocs -batch 50000 -verbose 1"),

			("Pipelined_NG",
			 "/opt/local/ilu/examples/ngtest/nggetbot -urls /opt/local/http-ng/ngtesturls/mscape_ngsun2_w3ng_w3ng_w3mux_2718.urls -batch 50000 -logfile stdout -v 1 -mt 10 -pipeline -serial",
			 "/opt/local/ilu/examples/ngtest/ngwebserver -httptinfo tcp_0_8080 -filebase /opt/local/http-ng/serverdocs -batch 50000 -verbose 1"),

			("Sunrpcrm",
			 "/opt/local/ilu/examples/ngtest/nggetbot -urls /opt/local/http-ng/ngtesturls/mscape_ngsun2_w3ng_w3ng_sunrpcrm_2718.urls -batch 50000 -logfile stdout -v 1 -mt 10 -pipeline -serial",
			 "/opt/local/ilu/examples/ngtest/ngwebserver -httptinfo tcp_0_8080 -filebase /opt/local/http-ng/serverdocs -batch 50000 -verbose 1 -ngtinfo sunrpcrm"),

			("Batchless_NG",
			 "/opt/local/ilu/examples/ngtest/nggetbot -urls /opt/local/http-ng/ngtesturls/mscape_ngsun2_w3ng_w3ng_w3mux_2718.urls -logfile stdout -v 1 -mt 10",
			 "/opt/local/ilu/examples/ngtest/ngwebserver -httptinfo tcp_0_8080 -filebase /opt/local/http-ng/serverdocs -verbose 1"),

			("Native_Apache",
			 "/opt/local/ilu/examples/ngtest/nggetbot -urls /opt/local/http-ng/ngtesturls/mscape_ngsun2_http_http_8080.urls -logfile stdout -v 1 -mt 10 -pipeline -serial -batch 50000",
			 "/opt/local/ilu/examples/ngtest/apache_wrapper.py /opt/local/http-ng/apache/logs/httpd.pid /opt/local/http-ng/apache/src/httpd -f /opt/local/http-ng/apache/conf/httpd.conf"),

			("libwww_Webbot",
			 "/opt/local/http-ng/w3c-libwww-5.1m/bin/webbot -saveimg -n http://ngsun2.parc.xerox.com:8080/microscape/microscape.html",
			 "/opt/local/ilu/examples/ngtest/apache_wrapper.py /opt/local/http-ng/apache/logs/httpd.pid /opt/local/http-ng/apache/src/httpd -f /opt/local/http-ng/apache/conf/httpd.conf"),

			("Slowed_libwww_Webbot",
			 "/opt/local/http-ng/w3c-libwww-5.1m/bin/webbot -saveimg -n http://ngsun2.parc.xerox.com:8080/microscape/microscape.html",
			 "/opt/local/ilu/examples/ngtest/apache_wrapper.py /opt/local/http-ng/apache/logs/httpd.pid /opt/local/http-ng/apache/src/httpd -f /opt/local/http-ng/apache/conf/httpd.conf"),

			("ILU_Apache",
			 "/opt/local/ilu/examples/ngtest/nggetbot -urls /opt/local/http-ng/ngtesturls/mscape_ngsun2_http_http_2719.urls -logfile stdout -v 1 -mt 10 -pipeline -serial -batch 50000",
			 "/opt/local/ilu/examples/ngtest/apache_wrapper.py /opt/local/http-ng/apache/logs/httpd.pid /opt/local/http-ng/apache/src/httpd -f /opt/local/http-ng/apache/conf/httpd.conf"),

			("IIOP",
			 "/opt/local/ilu/examples/ngtest/nggetbot -urls /opt/local/http-ng/ngtesturls/mscape_ngsun2_w3ng_iiop_2718.urls -batch 50000 -logfile stdout -v 1 -mt 10",
			 "/opt/local/ilu/examples/ngtest/ngwebserver -httptinfo tcp_0_8080 -ngpinfo iiop -ngtinfo -filebase /opt/local/http-ng/serverdocs -batch 50000 -fdbudget 128 -verbose 1"),

			("Slowed_NG",
			 "/opt/local/ilu/examples/ngtest/nggetbot -urls /opt/local/http-ng/ngtesturls/mscape_ngsun2_w3ng_w3ng_w3mux_2718.urls -batch 50000 -logfile stdout -v 1 -mt 10",
			 "/opt/local/ilu/examples/ngtest/ngwebserver -httptinfo tcp_0_8080 -filebase /opt/local/http-ng/serverdocs -batch 50000 -verbose 1"),

			("Async_NG",
			 "/opt/local/ilu/examples/ngtest/nggetbot -urls /opt/local/http-ng/ngtesturls/mscape_ngsun2_w3ng_w3ng_w3mux_2718.urls -batch 50000 -logfile stdout -v 1 -mt 10 -sink",
			 "/opt/local/ilu/examples/ngtest/ngwebserver -httptinfo tcp_0_8080 -filebase /opt/local/http-ng/serverdocs -batch 50000 -verbose 1"),

			("AOL_NG",
			 "/opt/local/ilu/examples/ngtest/nggetbot -urls /opt/local/http-ng/ngtesturls/aol_ngsun2_w3ng_w3ng_w3mux_2718.urls -batch 50000 -logfile stdout -v 1 -mt 10",
			 "/opt/local/ilu/examples/ngtest/ngwebserver -httptinfo tcp_0_8080 -filebase /opt/local/http-ng/serverdocs/aol -batch 50000 -verbose 1"),

			("MS_NG",
			 "/opt/local/ilu/examples/ngtest/nggetbot -urls /opt/local/http-ng/ngtesturls/ms_ngsun2_w3ng_w3ng_w3mux_2718.urls -batch 50000 -logfile stdout -v 1 -mt 10",
			 "/opt/local/ilu/examples/ngtest/ngwebserver -httptinfo tcp_0_8080 -filebase /opt/local/http-ng/serverdocs/ms -batch 50000 -verbose 1")
			)

nt_test_descriptions = (
			("Solaris_NT",
			 "\iluwin\examples\ngtest\winrel\nggetbot -urls \http-ng\ngtesturls\mscape_ngsun2_w3ng_w3ng_w3mux_2718.urls -batch 50000 -logfile stdout -v 1 -mt 10",
			 "/opt/local/ilu/examples/ngtest/ngwebserver -httptinfo tcp_0_8080 -filebase /opt/local/http-ng/serverdocs -batch 50000 -verbose 1"),

			("NT_Solaris",
			 "/opt/local/ilu/examples/ngtest/nggetbot -urls /opt/local/http-ng/ngtesturls/mscape_ngpc2_w3ng_w3ng_w3mux_2718.urls -batch 50000 -logfile stdout -v 1 -mt 10",
			 "\iluwin\examples\ngtest\winrel\ngwebserver -filebase \http-ng\serverdocs -batch 50000 -verbose 1"),

			("NT_NT",
			 "\iluwin\examples\ngtest\winrel\nggetbot -urls \http-ng\ngtesturls\mscape_ngpc2_w3ng_w3ng_w3mux_2718.urls -batch 50000 -logfile stdout -v 1 -mt 10",
			 "\iluwin\examples\ngtest\winrel\ngwebserver -filebase \http-ng\serverdocs -batch 50000 -verbose 1"),
			)



		
