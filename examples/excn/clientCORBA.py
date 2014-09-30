# $Id: clientCORBA.py,v 1.4 1999/09/01 17:19:18 janssen Exp $
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

import sys, ilu, ExcnTest, string, CORBA, types

def do_call (handle, count, expected_excn):
	status = 0
	try:
		handle.throw_excn (count)
		status = 1
	except:
		etype, eval, etraceback = sys.exc_info()

	if status == 1 and expected_excn:
		sys.stderr.write("ExcnTest.O.throw-excn(%d) completed successfully!\n" % count)
		sys.exit(1)

	else:
		if (status == 1):
			print "ExcnTest.O.throw-excn(%d) completed successfully." % count
		else:
			print "ExcnTest.O.throw-excn(%d) signalled exception %s." % (count, etype)
			if expected_excn and (expected_excn != 1) and (etype != expected_excn):
				sys.stderr.write("Wrong exception!  Expected %s\n" % str(expected_excn))
				sys.exit(1)
			elif (etype == ExcnTest.E11):				# OMG IDL style exception
				print '  val = ["%s"]' % eval.reason
			elif (type(etype) == types.ClassType and issubclass(etype, CORBA.UserException)):	# normal ILU-style exception
				print "  val =", eval.value
			elif (type(etype) == types.ClassType and issubclass(etype, CORBA.SystemException)):
				print "  val = { minor =", eval.minor, ", completion =", CORBA.completion_status.__image__[eval.completed], "}"
			else:
				print "  val =", eval


def main(argv):
  instHandle = "T"
  serverId = "ExcnTest.server.parc.xerox.com"
  pinfo = None
  tinfo = None
  threaded = 0

  i = 1;
  while (i < len(argv)):
	  if (argv[i] == '-p'):
		  i = i + 1
		  pinfo = argv[i]
		  i = i + 1
	  elif (argv[i] == '-n'):
		  i = i + 1
		  count = string.atoi(argv[i])
		  i = i + 1
	  elif (argv[i] == '-mt'):
		  threaded = 1;
		  i = i + 1
	  elif (argv[i] == '-t'):
		  tinfo = []
		  i = i + 1
		  while (i < len(argv) and (argv[i][0] != '-')):
			  tinfo.append(argv[i])
			  i = i + 1
	  else:
		  usage(argv[0])

  if (threaded):
	  import thread
	  ilu.ThreadedOperation()

  # try binding via CORBA name service
  ns = None
  try:
	  ns = CORBA.ORB_init().resolve_initial_references("NameService");
  except CORBA.InvalidName:
	  pass
  if ns:
	  print "binding via CosNaming..."
	  import CosNaming
	  name = [ CosNaming.NameComponent(serverId, "serverID"), CosNaming.NameComponent(instHandle, "instanceHandle") ]
	  handle = ns.resolve(name)
  else:
	  print "binding via ILU simple binding..."
	  handle = ilu.LookupObject(serverId, instHandle, ExcnTest.O)
  if not handle:
	  sys.stderr.write ("Can't bind to server object %s/%s\n" % (serverId, instHandle))
	  sys.exit(1)

  do_call (handle, 1, ExcnTest.E1)
  do_call (handle, 2, ExcnTest.E2)
  do_call (handle, 3, ExcnTest.E3)
  do_call (handle, 4, ExcnTest.E4)
  do_call (handle, 5, ExcnTest.E5)
  do_call (handle, 6, ExcnTest.E6)
  do_call (handle, 7, ExcnTest.E7)
  do_call (handle, 8, ExcnTest.E8)
  do_call (handle, 9, ExcnTest.E9)
  do_call (handle, 10, ExcnTest.E10)
  do_call (handle, 11, ExcnTest.E11)
  do_call (handle, 12, CORBA.BAD_PARAM)
  do_call (handle, 13, CORBA.BAD_PARAM)
  do_call (handle, 14, CORBA.UNKNOWN)
  do_call (handle, 15, None)
  return 0

if __name__ == "__main__":
	main(sys.argv)
