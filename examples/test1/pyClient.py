# $Id: pyClient.py,v 1.17 1999/08/03 01:52:11 janssen Exp $
# BeginILUCopyright

# Copyright (c) 1991-1999 Xerox Corporation.  All Rights Reserved.

# Unlimited use, reproduction, modification, and distribution of this
# software and modified versions thereof is permitted.  Permission is
# granted to make derivative works from this software or a modified
# version thereof.  Any copy of this software, a modified version
# thereof, or a derivative work must include both the above copyright
# notice of Xerox Corporation and this paragraph.  Any distribution of
# this software, a modified version thereof, or a derivative work must
# comply with all applicable United States export control laws.  This
# software is made available AS IS, and XEROX CORPORATION DISCLAIMS ALL
# WARRANTIES, EXPRESS OR IMPLIED, INCLUDING WITHOUT LIMITATION THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
# PURPOSE, AND NOTWITHSTANDING ANY OTHER PROVISION CONTAINED HEREIN, ANY
# LIABILITY FOR DAMAGES RESULTING FROM THE SOFTWARE OR ITS USE IS
# EXPRESSLY DISCLAIMED, WHETHER ARISING IN CONTRACT, TORT (INCLUDING
# NEGLIGENCE) OR STRICT LIABILITY, EVEN IF XEROX CORPORATION IS ADVISED
# OF THE POSSIBILITY OF SUCH DAMAGES.

# EndILUCopyright

import sys, string, traceback

okay = 1

if '-mt' in sys.argv:
	import thread
	import ilu
	ilu.ThreadedOperation()
	threaded = 1
	sys.argv.remove('-mt')
else:
	import ilu
	threaded = 0

if '-q' in sys.argv:
	quiet = 1
	sys.argv.remove('-q')
else:
	quiet = 0

def find_arg(args, switch, default):
	import string
	try:
		arg_loc = args.index(switch)
	except:
		return default

	sys.argv.remove(switch)
	
	ret = string.atoi(sys.argv[arg_loc])

	if ret <= 0:
		raise ValueError, 'argument must be > 0'
	sys.argv.remove(sys.argv[arg_loc])

	return ret

try:
	iterations = find_arg(sys.argv, '-i', 1)
except:
	okay = 0

if threaded:
	try:
		threads = find_arg(sys.argv, '-t', 5)
	except:
		okay = 0

if len(sys.argv) > 1:
	okay = 0

if not okay:
	print 'Usage is "%s [-q<uiet>] [-i <iterations>] [-mt<multithreaded> [-t <threads>]]".' % sys.argv[0]
	sys.exit(1)

import Test1
import Test2
import Test3

handle = ilu.LookupObject("Test1-Server", "Test1_Initial_Object", Test1.O1)

if not handle:
  print "couldn't get initial object"
  sys.exit(1)

ilu.SetFDBudget(1000)

def run_thread(thread_num):

  if (iterations > 1):

	  print '******** ',
	  if thread_num:
		  print 'Thread %d ' % thread_num,
		  global threads
	  print 'Beginning Execution ********' 
	
  try:
    for x in xrange(iterations):
	u = (5, ilu.TRUE)
	css = ["hello world", "hello mars"]
	if not quiet:
		print "U_CSS_to_U (%s, %s, %s) => " % (handle, u, css),
	u = handle.U_CSS_to_U(u, css)
	if not quiet:
		print u

	if not quiet:
		print "f_CSS_to_RO (%s, %s) => " % (handle, css),
	ro = handle.f_CSS_to_RO(css)
	if not quiet:
		print "ro->i=%d" % ro["i"]

	if not quiet:
		print "R_ScS_to_F (%s, %s, %s) => " % (handle, ro, css[0]),
	f = handle.R_ScS_to_F(ro, css[0])
	if not quiet:
		print "%f" % f

	if not quiet:
		print "a_RO (%s, %s) => " % (handle, ro),
	handle.a_RO(ro)
	if not quiet:
		print "completion"

	if not quiet:
		print "get_O2 (%s) => " % handle,
	o2 = handle.get_O2()
	if not quiet:
		print o2

	a = [1, 2, 3, 4, 5, 6, 7, 8]
	if not quiet:
		print "OO_A0_to_CSS (%s, %s, %s) => " % (o2, handle, a),
	css2 = o2.OO_A0_to_CSS(handle, a)
	if not quiet:
		print css2

	r = {"i": 12, "css": [], "a": ["this is", "data", "initialization"]}
	i = 0
	a1 = ["but this", "is", "fun"]
	if not quiet:
		print "R_I_A1_to_I_A0 (%s, %s, %s, %s) => " % (o2, r, i, a1),
	ap, i = o2.R_I_A1_to_I_A0(r, i, a1)
	if not quiet:
		print "%s, %s" % (ap, i)

	if not quiet:
		print "get_O3 (FALSE) => ",
	o3 = handle.get_O3(ilu.FALSE)
	if not quiet:
		print o3

	rs = []
	if not quiet:
		print "RS_R_to_R_IS (%s, %s) => " % (o3, rs),
	i2, r2 = o3.RS_R_to_R_IS(rs)
	if not quiet:
		print "%s, %s" % (i2, r2)

	if not quiet:
		print "O1_U_to_U (%s, %s, %s) => " % (o3, handle, u),
	u = o3.O1_U_to_U(handle, u)
	if not quiet:
		print u

	# This next call should return an instance of Test3.O
	if not quiet:
		print "get_O3 (TRUE) => ",
	o3 = handle.get_O3(ilu.TRUE)
	if not quiet:
		print o3

	if not quiet:
		print "RS_R_to_R_IS (%s, %s) => " % (o3, rs),
	i2, r = o3.RS_R_to_R_IS(rs)
	if not quiet:
		print "%s, %s" % (i2, r) 

	if not quiet:
		print "O1_U_to_U (%s, %s, %s) => " % (o3, handle, u),
	u = o3.O1_U_to_U(handle, u)
	if not quiet:
		print u

	f = 397
	if not quiet:
		print "I_to_Test1U (%s, %s) => " % (o3, f),
	u2 = o3.I_to_Test1U(f)
	if not quiet:
		print u2

	if not thread_num:
	  #This next call should return an instance of Test1.O4
	  if not quiet:
		print "get_O3 (FALSE) => ",
		o3a = handle.get_O3(ilu.FALSE)
		if not quiet:
			print o3a

		r1 = 12345.6789
		if not quiet:
			print "R_to_R (%s, %.10f) => " % (o3a, r1),
		r2 = o3a.R_to_R(r1)
		if not quiet:
			print "%.10f" % r2

	if not quiet:
		try:
			print "TCP statistics: ", ilu.TCPStatistics(1)
		except:
			pass

	if (iterations > 1) and not quiet:
		print '**** ',
		if thread_num:
			print 'Thread %d ' % thread_num,
		print 'Completed Iteration %d ****' % (x + 1,)
  except:
	traceback.print_exc()
	print '**** ',
	if thread_num:
		print 'Thread %d ' % thread_num,
	        threads = threads - 1
	print 'Caught Exception on Iteration %d (Exiting) ****' % (x + 1,)
    
  print '*********',
  if thread_num:
	  print ' Thread %d' % thread_num,
	  global threads
	  threads = threads - 1
  print ' Completed Execution (Exiting) ********' 
	
def main():
	if threaded:
		import thread, time
		for x in range(threads):
			thread.start_new_thread(run_thread, (x + 1,))

		while threads > 0:
			print 'Main Thread Sleeping (%d threads)...' % threads
			time.sleep(4)
	else:
		run_thread(0)

main()
