import tempfile
import os
import msvcrt
import win32api
import win32process

class Process:
    	def __init__ (self, executable, args, env, logfile=None):
		self.logfile = logfile
		self.executable = find_executable(executable)
		self.args = args
		self.exitstatus = None
		self.running = 0
		tempfile.template = None
		if logfile:
			fd = os.open(logfile.filename, 
				     O_WRONLY | O_CREAT | O_APPEND, 0664)
			# Get the NT handle for fd
			hStdout = msvcrt.get_osfhandle(fd)
			# Equivalent of os.dup() except for Win32 HANDLE's
			hStderr = win32api.DuplicateHandle(hStdout)
		else:
			# Grab the HANDLE's for current stdout & stderr
			hStdout = msvcrt.get_osfhandle(sys.stdout)
			hStderr = msvcrt.get_osfhandle(sys.stderr)
			# Grab the HANDLE for stdin
			hStdin = msvcrt.get_osfhandle(sys.stdin)
		for key in env.keys():
			os.environ[key] = env[key]
		if logfile:
			output = fd
		else:
			output = sys.stderr
		output.write("executable is " + 
			     str(self.executable) + 
			     ", args are " + 
			     str(args) + ", env = " +
			     str(os.environ) + "\n")
		# Create the process
		# All of this footwork, should allow this sucker to run from a console, or GUI app.
		sCmdLine = self.executable
		for arg in args:
			sCmdLine = sCmdLine + " " + arg
			StartupInfo = win32process.STARTUPINFO()
			StartupInfo.dwFlags = win32con.STARTF_USESTDHANDLES | win32con.STARTF_USESHOWWINDOW
			StartupInfo.hStdInput = hStdin
			StartupInfo.hStdOutput = hStdout
			StartupInfo.hStdError  = hStderr
			StartupInfo.wShowWindow = win32con.SW_HIDE
			hProcess, hThread, dwPid, dwTid = win32api.CreateProcess(self.executable,
										 sCmdLine,
										 None,
										 None,
										 0,
										 win32process.CREATE_NEW_CONSOLE,
										 None,
										 None,
										 StartupInfo)
			win32api.CloseHandle(hThread)
			self.pid = dwPid
			self.hProcess = hProcess
			self.running = 1
			add_exit_function(lambda x=self: x.kill())
        
	def finish (self):
		import win32event
		win32event.WaitForSingleObject(self.hProcess, -1)
		status = win32process.GetExitCodeProcess(self.hProcess)
		self.exitstatus = status/256
		self.running = 0
		return status
        
 	def kill (self, signal=9):
		if self.running:
			win32process.TerminateProcess(self.hProcess, -9)
			return self.finish()
		else:
			return 0
            
 	def __del__(self):
		user_msg('deleting pid ' + str(self.pid) + '\n')
		if self.running:
			self.kill()
		win32api.CloseHandle(self.hProcess)
