# $Id: pyClient.py,v 1.15 1999/09/03 16:19:26 janssen Exp $
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

x = 0

okay = 1

errors = 0

if '-mt' in sys.argv:
	import thread
	import ilu
	ilu.ThreadedOperation()
	threaded = 1
	sys.argv.remove('-mt')
else:
	import ilu
	threaded = 0

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

if threaded:
	try:
		threads = find_arg(sys.argv, '-t', 5)
	except:
		okay = 0

skip_nil_object_tests = 0
if '-skip-nil-object-tests' in sys.argv:
	skip_nil_object_tests = 1
	sys.argv.remove('-skip-nil-object-tests')

allow_different_types_on_nil_objects = 0
if '-allow-different-types-on-nil-objects' in sys.argv:
	allow_different_types_on_nil_objects = 1
	sys.argv.remove('-allow-different-types-on-nil-objects')

if len(sys.argv) > 1:
	okay = 0

if not okay:
	print 'Usage is "%s [-mt<multithreaded> [-t <threads>]] [-skip-nil-object-tests] [-allow-different-types-on-nil-objects]".' % sys.argv[0]
	sys.exit(1)

import Test1, Test2
if ilu.CORBAMapping:
	import Test1__POA
	trueModule = Test1__POA
else:
	import Test1__skel
	trueModule = Test1__skel

class realO2 (trueModule.O2):		# simple ILU object

	def __init__(self, ih, srvr):
		self.IluInstHandle = ih
		self.IluServer = srvr

class realO3 (trueModule.O3):		# collectible ILU object

	def __init__(self, ih, srvr):
		self.IluInstHandle = ih
		self.IluServer = srvr

class realO4 (trueModule.O4):		# optional ILU object

	def __init__(self, ih, srvr):
		self.IluInstHandle = ih
		self.IluServer = srvr

class realO5 (trueModule.O5):		# CORBA object

	def __init__(self, ih, srvr):
		self.IluInstHandle = ih
		self.IluServer = srvr

handle = ilu.LookupObject("pickleServer", "pickleObj", Test2.O1)

if not handle:
  print "couldn't get initial object"
  sys.exit(1)

anO2 = realO2("anO2", None)
anO3 = realO3("anO3", None)
anO4 = realO4("anO4", None)
anO5 = realO5("anO5", None)

def bounce_val (handle, typename, val, isObjectType=0):
	global errors
	try:
		tc = ilu.Typecode(typename)
		v = ilu.Pickle(tc, val)
		v2 = handle.bounce(v)
		print '%s.bounce(%s) => %s' % (handle, v, v2)
		sys.stdout.flush()
		if (tc.id() != v2.typecode().id()) or (v2.value() != v.value()):
			print '*** returned value has typecode id ' + v2.typecode().id() + ' and value ' + str(v2.value()) + '; original typecode id was ' + tc.id() + ' and value was ' + str(v.value())
			if (val == None) and (v2.value() == None) and (v2.typecode() == ilu.Typecode("ilu.CORBA-Object")) and isObjectType and allow_different_types_on_nil_objects:
				pass
			else:
				raise 'different pickle value received from server', (v, v2)
	except:
		print '%s.bounce(%s) => exception' % (handle, typename)
		traceback.print_exc();
		errors = errors + 1

def run_thread(thread_num):

  print '******** ',
  if thread_num:
	  print 'Thread %d ' % thread_num,
	  global threads
  print 'Beginning Execution ********' 
	
  try:
	  bounce_val(handle, 'Test1.I', 344)
	  bounce_val(handle, 'Test1.IS', (345, 3, 14))
	  bounce_val(handle, 'Test1.SC', 45)
	  bounce_val(handle, 'Test1.C', 454)
	  bounce_val(handle, 'Test1.ScS', 'foobar')
	  bounce_val(handle, 'Test1.CSS', ('foobar', 'bletch',))
	  bounce_val(handle, 'Test1.A0', 'abcdefgh')
	  bounce_val(handle, 'Test1.PS', [])
	  bounce_val(handle, 'Test1.PS', [ilu.Pickle(ilu.Typecode('Test1.I'), 344), ilu.Pickle(ilu.Typecode('Test1.ScS'), 'a string')])
	  bounce_val(handle, 'Test1.TheA1', ['foo', 'bar', 'bletch'])
	  bounce_val(handle, 'Test1.A1', ['foo', 'bar', 'bletch'])
	  bounce_val(handle, 'Test1.BS', '\1\2\3\4\5\6\7')
	  bounce_val(handle, 'Test1.TheR', Test1.TheR(['foo', 'bar', 'bletch'], ('foo', 'bar'), 2))
	  bounce_val(handle, 'Test1.R', Test1.TheR(['foo', 'bar', 'bletch'], ('foo', 'bar'), 2))
	  bounce_val(handle, 'Test1.R2', Test1.R2(Test1.TheR(['foo', 'bar', 'bletch'], ('foo', 'bar'), 3),
						  ilu.Pickle(ilu.Typecode("Test1.CSS"), ('foobar', 'bletch',))))
	  bounce_val(handle, 'Test1.R3', Test1.R3(3, 1.7))
	  bounce_val(handle, 'Test1.TheRS', (Test1.TheR(['foo', 'bar', 'bletch'], ('foo', 'bar'), 2),
					     Test1.TheR(['foo', 'bar', 'bletch'], ('foo', 'bar'), 1),
					     Test1.TheR(['foo', 'bar', 'bletch'], ('foo', 'bar'), 0)))
	  bounce_val(handle, 'Test1.RS', (Test1.TheR(['foo', 'bar', 'bletch'], ('foo', 'bar'), 2),
					 Test1.TheR(['foo', 'bar', 'bletch'], ('foo', 'bar'), 1),
					 Test1.TheR(['foo', 'bar', 'bletch'], ('foo', 'bar'), 0)))
	  
	  bounce_val(handle, 'Test1.RO', None)
	  bounce_val(handle, 'Test1.RO', Test1.TheR(['foo', 'bar', 'bletch'], ('foo', 'bar'), 2))
	  bounce_val(handle, 'Test1.O5', anO5)
	  if not skip_nil_object_tests:
		  bounce_val(handle, 'Test1.O5', None, 1)
	  bounce_val(handle, 'Test1.O2', anO2)
	  bounce_val(handle, 'Test1.O3', anO3)
	  bounce_val(handle, 'Test1.O4', anO4)
	  if not skip_nil_object_tests:
		  bounce_val(handle, 'Test1.O4', None, 1)
	  bounce_val(handle, 'Test1.TheOO', None)
	  bounce_val(handle, 'Test1.OO', anO5)
	  bounce_val(handle, 'Test1.OO2', (1, anO5))
	  bounce_val(handle, 'Test1.OO2', (0, None))
	  bounce_val(handle, 'Test1.TheU', (0, Test1.TheR(['foo', 'bar', 'bletch'], ('foo', 'bar'), 2)))
	  bounce_val(handle, 'Test1.TheU', (1, Test1.TheR(['foo', 'bar', 'bletch'], ('foo', 'bar'), 2)))
	  bounce_val(handle, 'Test1.TheU', (2, ('foo', 'bar')))
	  bounce_val(handle, 'Test1.TheU', (3, anO5))
	  bounce_val(handle, 'Test1.TheU', (4, None))
	  bounce_val(handle, 'Test1.TheU', (4, anO5))
	  bounce_val(handle, 'Test1.TheU', (5, 1))
	  bounce_val(handle, 'Test1.U', (5, 1))
	  bounce_val(handle, 'Test1.U1', (0, Test1.TheR(['foo', 'bar', 'bletch'], ('foo', 'bar'), 2)))
	  bounce_val(handle, 'Test1.U1', (1, ['foo', 'bar', 'bletch']))
	  bounce_val(handle, 'Test1.TheE', Test1.TheE.ev3)
	  bounce_val(handle, 'Test1.E', Test1.TheE.ev3)
	  bounce_val(handle, 'Test1.U2', (22, anO5))
	  bounce_val(handle, 'Test1.U3', (Test1.TheE.ev1, 'foobar'))
	  bounce_val(handle, 'Test1.U4', (7, 1))
	  bounce_val(handle, 'Test1.U-byte', (1, anO2))
	  bounce_val(handle, 'Test1.U-int', (1, anO5))
	  bounce_val(handle, 'Test1.U-card', (1, anO5))
	  bounce_val(handle, 'Test1.U-scard', (1, anO5))

  except:
        global errors
	errors = errors + 1
	traceback.print_exc()
	print '**** ',
	if thread_num:
		print 'Thread %d ' % thread_num,
	        threads = threads - 1
	print 'Caught Exception (Exiting) ****'
    
  print '******** ',
  if thread_num:
	  print 'Thread %d ' % thread_num,
	  global threads
	  threads = threads - 1
  print 'Completed Execution ********' 
	
def main():
	global x, errors
	if threaded:
		import thread, time
		for x in range(threads):
			thread.start_new_thread(run_thread, (x + 1,))

		while threads:
			print 'Main Thread Sleeping...'
			time.sleep(4)
	else:
		run_thread(0)
	if errors > 0:
		sys.exit(1)
	else:
		sys.exit(0)

main()
