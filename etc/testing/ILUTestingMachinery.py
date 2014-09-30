# $Id: ILUTestingMachinery.py,v 1.24 1999/09/14 03:16:41 janssen Exp $
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

import sys, re, os, time, traceback, string

SBHPattern = re.compile('(ilusbh:|IOR:|ilu:)([a-zA-Z0-9@/:=+-._;%]*)')

ServerStart = 'problem starting a server'

from ILUTestingConfig import iluconf_dict, makefile_dict, iludefs_dict

#
# Some exit wizardry to kill servers, delete log files, etc., automagically
#

def do_exit_function (fn, next_fn):
	fn()
	if next_fn:
		next_fn()

def add_exit_function (fn):
	if sys.__dict__.has_key('exitfunc'):
		oldexit = sys.exitfunc
	else:
		oldexit = None
	sys.exitfunc = lambda x=oldexit, y=fn: do_exit_function(y,x)

def killproc(pid):
	import os
	try:
		os.kill(pid, 9)
	except:
		pass

def delfile(fname):
	import os
	try:
		os.unlink(fname)
	except:
		pass

# A msg function which only writes when ILUTEST_VERBOSE is set

Verbose = os.environ.has_key('ILUTEST_VERBOSE')

def user_msg (msg):
	global Verbose
	if Verbose:
		sys.stderr.write(msg)

# A function which adds to an environment variable

def prefix_environment_variable (var_name, value):
	existing_value = (os.environ.has_key(var_name) and os.environ[var_name])
	if existing_value:
		new_value = value + ":" + existing_value
	else:
		new_value = value
	os.environ[var_name] = new_value

# Exceptions to raise when process oddities are encountered

class Already_Started:
	def __init__ (self, proc):
		self.proc = proc

class Server_Failed:
	def __init__ (self, server_commands):
		self.commands = server_commands

	def __str__(self):
		import string
		return '<Server_Failed:  ' + string.join(server_commands) + '>'

	__repr__ = __str__

class Error_Exit_Status:
	def __init__ (self, status, signal, program, logfile):
		self.status = status
		self.program = program
		self.logfile = logfile
		self.signal = signal

	def __str__(self):
		return '<Error_Exit_Status:  status=%s, signal=%d, program="%s", logfile="%s">' % (self.status, self.signal, self.program, self.logfile)

	__repr__ = __str__

class TestError:
	pass

# Logfile class to keep output in

class Logfile:
	def __init__ (self, name=None):
		import tempfile
		if not name:
			self.filename = tempfile.mktemp()
		else:
			self.filename = name
		self.verbose = Verbose
		self.os = os
		add_exit_function(lambda x=self.filename: delfile(x))

	def display (self, banner=None):
		if banner:
			print banner
		f = open(self.filename,'r')
		line = f.readline()
		while line:
			sys.stdout.write(line)
			line = f.readline()
		f.close()

	def exists(self):
		return os.path.exists(self.filename)

	def open(self, mode='r'):
		return open(self.filename, mode)

	def delete (self):
		import os
		if os.path.exists(self.filename):
			os.unlink(self.filename)
		
	zero = delete

	def __str__(self):
		return '<Logfile %s>' % self.filename

	__repr__ = __str__

# A Process class that wraps a running server or client process (or any other kind of process)

def find_executable (program_name, opt_path=None):

	def is_executable (path):
		import os
		mode = os.stat(path)[0]
		return ((mode & 0111) != 0)

	import string, os
	pname = os.path.expanduser(os.path.expandvars(program_name))
	if pname[0] == '.' or pname[0] == '/':
		return program_name
	path = opt_path or os.environ["PATH"]
	dirs = string.split(path, os.pathsep)
	for dir in dirs:
		test_path = os.path.join(dir, pname)
		if os.path.exists(test_path) and os.path.isfile(test_path) and is_executable(test_path):
			return test_path
	return None

class Process:
	def __init__ (self, executable, args, env, logfile=None):
		import tempfile
		import os
		self.logfile = logfile
		self.executable = find_executable(executable, env.has_key("PATH") and env["PATH"])
		self.args = args
		self.exitstatus = None
		self.running = 0
		self.pid = os.fork()
		if self.pid == 0:	# child process
			tempfile.template = None
			if logfile:
				fd = os.open(logfile.filename, os.O_WRONLY | os.O_CREAT | os.O_APPEND, 0664)
				os.close(1)
				os.dup2(fd, 1)
				os.close(2)
				os.dup2(fd, 2)
			for key in env.keys():
				os.environ[key] = env[key]
			sys.stderr.write("executable is " + str(self.executable) + ", args are " + str(args) + ", env = " + str(os.environ) + "\n");
			sys.exitfunc = None
			os.execve(self.executable, (self.executable,) + args, os.environ)
		else:
			self.running = 1
			add_exit_function(lambda x=self.pid: killproc(x))

	def active(self):
		import os
		if not self.running:
			return 0
		status = os.waitpid (self.pid, os.WNOHANG)
		if (status[0] == 0):
			# process still running
			return 1
		else:
			self.exitstatus = status[1]/256
			self.running = 0
			return 0

	def finish (self):
		import os
		if self.running:
			status = os.waitpid (self.pid, 0)
			self.exitstatus = status[1]/256
			self.running = 0
			return status[1]%256
		else:
			return 0

	def kill (self, signal=9):
		import posix
		if self.running:
			posix.kill(self.pid, signal)
			return self.finish()
		else:
			return 0
		
class Program:

	program_type = "Program"

	def __init__ (self, executable, default_args=(), env={}):
		import types
		if type(default_args) == types.StringType:
			default_args = ( default_args, )
		if type(default_args) != types.TupleType:
			default_args = tuple(default_args)
		self.executable = executable
		self.default_args = default_args
		self.proc = None
		self.env_bindings = env
		self.logfile = None

	def start (self, logfile, args=()):
		import types
		if self.proc:
			raise Already_Started(self.proc)
		if not args:
			args = self.default_args
		if (type(args) != types.TupleType):
			args = tuple(args)
		user_msg ("starting " + self.program_type +
			  " " + str(self.executable) + " with args '" +
			  str(args) + "' and output to " + str(logfile) + "\n")
		self.logfile = logfile
		self.proc = Process (self.executable, args, self.env_bindings, logfile)

	def finish (self):
		status = self.proc.finish()
		exitstatus = self.proc.exitstatus
		self.proc = None
		return status, exitstatus

	def run (self, logfile):
		self.start(logfile)
		killsignal, exitstatus = self.finish()
		if (exitstatus != 0) or (killsignal != 0):
			raise Error_Exit_Status (exitstatus, killsignal, self, logfile)

	def active (self):
		# returns TRUE if program is still running
		return self.proc and self.proc.active()

	def kill (self, signal=9):
		status = self.proc.kill(signal)
		exitstatus = self.proc.exitstatus
		self.proc = None
		return status, exitstatus

	def logfile (self):
		if not self.proc:
			raise Not_Started (self)
		return self.proc.logfile.open('r')

	def __str__(self):
		import string
		return '<%s.%s "%s" proc=%s>' % (self.__class__.__module__, self.__class__.__name__,
						  string.join((self.executable,) + self.default_args),
						  self.proc and self.proc.pid)

	__repr__ = __str__

class Server (Program):
	program_type = "Server"

class SBH_Server (Server):
	"""subclass of Server which writes its SBH to stdout.  The SBH can be retrieved as server.sbh"""
	def __init__(self, executable, default_args=(), env={}):
		Server.__init__(self, executable, default_args, env)
		self.sbh = None

	def start (self, logfile, args=()):
		import time
		global SBHPattern
		Server.start (self, logfile, args)
		while not logfile.exists():
			time.sleep(1)	# wait for file to be created
		time.sleep(10)		# wait for service to be registered
		f = logfile.open('r')
		junkline = f.readline()
		sbhline = f.readline()
		f.close()
		sbhmatch = SBHPattern.search(sbhline)
		if sbhmatch:
			self.sbh = sbhline[sbhmatch.start(1):sbhmatch.end(2)]

class Client (Program):
	program_type = "Client"

class Test:

	def normalize_command (self, cmd, cmd_name):
		import types, string
		if type(cmd) == types.StringType:
			c = string.split(cmd)
		elif (type(cmd) == types.TupleType or
			 type(cmd) == types.SequenceType):
			c = cmd
		else:
			raise "bad " + cmd_name +" '" + str(cmd) + "' supplied."
		return c

class SimpleClientServerTest(Test):

	def __init__(self, test_name, client_commands, server_commands):
		self.name = test_name
		self.client_commands = self.normalize_command(client_commands, "client commands")
		self.server_commands = self.normalize_command(server_commands, "server commands")
		self.server_startup_delay = 5
		self.server_shutdown_delay = 0

	def run(self):
		oldpath = os.environ["PATH"]
		os.environ["PATH"] = "." + os.pathsep + os.environ["PATH"]

		print 'Running test "' + self.name + '"...'

		clientlog = None
		serverlog = None

		try:
			try:

				serverlog = Logfile()

				# start server -- sys.executable holds pathname of Python interpreter
				s = Server (self.server_commands[0], self.server_commands[1:])
				s.start(serverlog)
				time.sleep(self.server_startup_delay)	# let it get started
				if not s.active():
					raise 'server startup failed : ' + string.join(self.server_commands)
			
				# run client against server
				clientlog = Logfile()
				Client (self.client_commands[0], self.client_commands[1:]).run(clientlog)
			
				# OK, things are fine.  Print a message and exit
				print 'Test "' + self.name + '" completed successfully.'

				# delete the client log
				clientlog.delete()
				# kill the server
				s.kill()
				if (self.server_shutdown_delay > 0):
				    time.sleep(self.server_shutdown_delay)
				# delete the server log
				serverlog.delete()

			except:
				print "Error in test " + self.name + ":"
				traceback.print_exc()
				if serverlog:
					serverlog.display("******* Server Log ***********")
					serverlog.delete()
				if clientlog:
					clientlog.display("******* Client Log ***********")
					clientlog.delete()
				raise TestError()

		finally:
			os.environ["PATH"] = oldpath


class MultiClientMultiServerTest(Test):

	# run several servers and clients.  Each client must exit successfully
	# for the test to succeed.

	def __init__(self, test_name, client_commands, server_commands):
		self.name = test_name
		self.clients = []
		for command in client_commands:
			self.clients.append(self.normalize_command(command, "client command"))
		self.servers = []
		for command in server_commands:
			self.servers.append(self.normalize_command(command, "server command"))
		self.server_startup_delay = 5
		self.server_shutdown_delay = 0
		self.verbose = 0

	def start_server(self, command, delay, count):

		import time, traceback, string
		oldpath = os.environ["PATH"]
		os.environ["PATH"] = "." + os.pathsep + os.environ["PATH"]
		try:
			serverlog = Logfile()
			# start server -- sys.executable holds pathname of Python interpreter
			s = Server (command[0], command[1:])
			if self.verbose:
				print 'starting', str(count), command
			s.start(serverlog)
			time.sleep(delay)	# let it get started
			if not s.active():
				raise ServerStart, (string.join(command), serverlog)
			else:
				if self.verbose:
					print 'started server ' + str(count) + ': "' + string.join(command) + '"'
					sys.stdout.flush()
			return s
		finally:
			os.environ["PATH"] = oldpath

	def start_servers (self):
		started_servers = []
		count = 1
		for server in self.servers:
			started_servers.append(self.start_server(server, self.server_startup_delay, count))
			count = count + 1
		return started_servers

	def kill_servers(self, servers):
		for server in servers:
			if self.verbose:
				sys.stdout.write("killing server " + str(server) + "\n")
				sys.stdout.flush()
			server.kill()
			if (self.server_shutdown_delay > 0):
			    time.sleep(self.server_shutdown_delay)

	def run(self):
		import time, traceback, string
		started_servers = []

		oldpath = os.environ["PATH"]
		os.environ["PATH"] = "." + os.pathsep + os.environ["PATH"]

		try:
			print 'Running test "' + self.name + '"...'

			try:
				started_servers = self.start_servers()
			except ServerStart, data:
				print 'failed to start server "' + data[0] + '"'
				data[1].display("*********** Log of failed server ***********")
				print '**************************************'
				raise ServerStart, data
			except:
				traceback.print_exc()
				raise 'problem starting one of the servers'

			count = 1
			for client in self.clients:

				try:

					# run client against server
					clientlog = Logfile()
					if self.verbose:
						print 'running client ' + str(count) + ': "' + string.join(client) + '"'
					Client (client[0], client[1:]).run(clientlog)
					count = count + 1

				except:
					print "Error in client " + str(count) + ":"
					traceback.print_exc()
					for log in started_servers:
						s.logfile.display("******* Server Log ***********")
					if clientlog:
						clientlog.display("******* Client Log ***********")
					raise TestError()
			# OK, things are fine.  Print a message and exit
			print 'Test "' + self.name + '" completed successfully.'
			# kill the servers
			self.kill_servers(started_servers)
		finally:
			os.environ["PATH"] = oldpath


class ClientSBHServerTest(Test):
	"""Subclass of Test in which the server writes its SBH or IOR to stdout, and the client uses that SBH
	in one of its command-line arguments.  The string '$SBH$' in any client command-line argument will be
	replaced with the actual SBH output by the server."""

	def __init__(self, test_name, client_commands, server_commands):
		self.name = test_name
		self.client_commands = self.normalize_command(client_commands, "client commands")
		self.server_commands = self.normalize_command(server_commands, "server commands")
		self.server_startup_delay = 5
		self.server_shutdown_delay = 0

	def run(self):
		import time, traceback, string

		oldpath = os.environ["PATH"]
		os.environ["PATH"] = "." + os.pathsep + os.environ["PATH"]

		print 'Running test "' + self.name + '"...'

		clientlog = None
		serverlog = None

		try:
			try:

				serverlog = Logfile()

				# start server -- sys.executable holds pathname of Python interpreter
				s = SBH_Server (self.server_commands[0], self.server_commands[1:])
				s.start(serverlog)
				time.sleep(self.server_startup_delay)	# let it get started
				if not s.active():
					raise 'server startup failed : ' + string.join(self.server_commands)
				if not s.sbh:
					raise "can't find server SBH"
			
				# run client against server
				clientlog = Logfile()
				commands = []
				for command in self.client_commands:
					real_command = string.replace(command, '$SBH$', s.sbh);
					commands.append(real_command)
				Client (commands[0], commands[1:]).run(clientlog)
			
				# OK, things are fine.  Print a message and exit
				print 'Test "' + self.name + '" completed successfully.'

				# delete the client log
				clientlog.delete()
				# kill the server
				s.kill()
				if (self.server_shutdown_delay > 0):
				    time.sleep(self.server_shutdown_delay)
				# delete the server log
				serverlog.delete()

			except:
				print "Error in test " + self.name + ":"
				traceback.print_exc()
				if serverlog:
					serverlog.display("******* Server Log ***********")
					serverlog.delete()
				if clientlog:
					clientlog.display("******* Client Log ***********")
					serverlog.delete()
				raise TestError()

		finally:
			os.environ["PATH"] = oldpath


class SimpleSingleProgramTest (Test):

	def __init__(self, test_name, commands, env={}):
		import types, string
		self.name = test_name
		self.commands = self.normalize_command(commands, "commands")
		self.environment = env

	def run(self):
		import time, traceback

		oldpath = os.environ["PATH"]
		os.environ["PATH"] = "." + os.pathsep + os.environ["PATH"]

		print 'Running test "' + self.name + '"...'

		try:
			try:

				logfile = Logfile()

				s = Program(self.commands[0], self.commands[1:], self.environment)
				s.run(logfile)
			
				# OK, things are fine.  Print a message and exit
				print 'Test "' + self.name + '" completed successfully.'

				logfile.delete()

			except:
				print "Error in test " + self.name + ":"
				traceback.print_exc()
				logfile.display("******* Program Log ***********")
				logfile.delete()
				raise TestError()

		finally:
			os.environ["PATH"] = oldpath

######################################################################
######################################################################
###
###  Class for defining configurations of the ILU system
###
######################################################################
######################################################################

ST_none = 0
ST_tarfile = 1
ST_copy_ilu_tree = 2

import types

class TestingConfiguration:

	def __init__(self, name, copy_ilu_tree=None, tarfile=None, enables=None, config_switches = None, directory=None, environment=None, patchfile=None, temproot=None):

		self.name = name

		# check source of ILU code
		if copy_ilu_tree and tarfile:
			raise "Both tarfile and copy-ilu-tree specified.  Exactly one is allowed."
		if tarfile:
			if not os.path.isfile(tarfile):
				raise "Cannot find specified ILU source tar file", tarfile
			self.source_type = ST_tarfile
			self.source_arg = tarfile
			self.source_gzipped = (tarfile[-3:] == '.gz' or tarfile[-4:] == '.tgz')
			self.patchfile = None
			if patchfile:
				if not os.path.isfile(patchfile):
					raise "Cannot find specified patch file", patchfile
				self.patchfile = patchfile
		else:	# must be copy_ilu_tree
			if not os.path.isfile("/project/rpc/tools/copy-ilu-tree") or not os.path.isdir("/project/rpc/RCS-tree") or not os.path.isfile("/project/rpc/RCS-tree/Imakefile,v"):
				raise "Cannot use copy-ilu-tree testing in this domain; use a tarfile instead"
			self.source_type = ST_copy_ilu_tree
			self.source_arg = copy_ilu_tree
			self.source_gzipped = 0
			if patchfile:
				raise "Patchfile only meaningful with tarfile source"

		# check packages to enable
		if enables:
			if not os.path.isfile("/import/import-support/bin/determine-os"):
				raise "Don't know how to do `enable' in this environment."
			elif ((type(enables) != types.ListType) and
			      (type(enables) != types.TupleType)):
				raise "Non-list/tuple value passed for enables param", enables
			self.enables = enables
		else:
			self.enables = []

		# check directory to use
		if directory:
			if not os.path.isdir(directory):
				raise "Can't find directory", directory
			self.directory = directory
			self.directory_temp = 0
		else:
			self.directory = None
			self.directory_temp = 0

		# check temproot to use
		if temproot:
			if not os.path.isdir(temproot):
				raise "Can't find directory", temproot
			self.temproot = temproot
			self.directory_temp = 0
		else:
			self.temproot = None
			self.directory_temp = 0

		# check environment
		if environment:
			if (not (type(environment) == types.DictionaryType) and
			    not (type(environment) == types.InstanceType)):
				raise "Non-dictionary passed for environment param", environment
			self.environment = environment
		else:
			self.environment = {}

		# finally, stash away config switches
		if config_switches:
			if ((type(config_switches) != types.ListType) and
			    (type(config_switches) != types.TupleType)):
				raise "Non-list/tuple passed for config_switches param", config_switches
			self.config_switches = config_switches
		else:
			self.config_switches = []

	def set_tarfile (self, tarfile, patchfile):
		if not os.path.isfile(tarfile):
			raise "Cannot find specified ILU source tar file", tarfile
		self.source_type = ST_tarfile
		self.source_arg = tarfile
		self.source_gzipped = (tarfile[-3:] == '.gz' or tarfile[-4:] == '.tgz')
		self.patchfile = None
		if patchfile:
			if not os.path.isfile(patchfile):
				raise "Cannot find specified patch file", patchfile
			self.patchfile = patchfile

	def set_copy_ilu_tree (self, copy_ilu_tree):
		if not os.path.isfile("/project/rpc/tools/copy-ilu-tree") or not os.path.isdir("/project/rpc/RCS-tree") or not os.path.isfile("/project/rpc/RCS-tree/Imakefile,v"):
			raise "Cannot use copy-ilu-tree testing in this domain; use a tarfile instead"
		self.source_type = ST_copy_ilu_tree
		self.source_arg = copy_ilu_tree
		self.source_gzipped = 0
		self.patchfile = None
