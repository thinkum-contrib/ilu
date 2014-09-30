# Top level makefile to build Win32 version of ILU using
# Microsoft Visual C++
#
# Assumes your PATH has the appropriate bin directory in it (e.g. msvc\bin)
# Assumes your INCLUDE environment variable includes the appropriate directories (e.g. msvc\include) 
# Assumes your LIB environment variable includes the appropriate directories (e.g. msvc\lib) 

!IF "$(CFG)" == ""
CFG=Win32 Release
!MESSAGE No configuration specified.  Defaulting to Win32 Release.
!ENDIF 

!IF "$(CFG)" != "Win32 Release" && "$(CFG)" != "Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE on this makefile
!MESSAGE by defining the macro CFG on the command line.  For example:
!MESSAGE 
!MESSAGE NMAKE /f "iluwin32.mak" CFG="Win32 Release"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "Win32 Release"
!MESSAGE "Win32 Debug"
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 



################################################################################

!IF "$(JAVA_HOME)" == ""
!MESSAGE JAVA_HOME environment variable not set, not building Java example
ALL : TEST1MAKE
!ELSE
ALL : TEST1MAKE JAVAMAKE
!ENDIF 

ALL : TEST1MAKE


TEST1MAKE :
	c-stubber Test1.isl
	c-stubber Test2.isl
	c-stubber Test3.isl
	nmake -f clnt.mak CFG="$(CFG)"
	nmake -f clntw.mak CFG="$(CFG)"
	nmake -f srvr.mak CFG="$(CFG)"
	nmake -f srvrw.mak CFG="$(CFG)"
	c++-stubber Test1.isl
	c++-stubber Test2.isl
	c++-stubber Test3.isl
	nmake -f cppclnt.mak CFG="$(CFG)"
	nmake -f cppclntw.mak CFG="$(CFG)"
	nmake -f cppsrvr.mak CFG="$(CFG)"
	nmake -f cppsrvrw.mak CFG="$(CFG)"
	python-stubber Test1.isl
	python-stubber Test2.isl
	python-stubber Test3.isl
	cpp2-stubber Test1.isl
	cpp2-stubber Test2.isl
	cpp2-stubber Test3.isl
	nmake -f cpp2clnt.mak CFG="$(CFG)"
	nmake -f cpp2srvr.mak CFG="$(CFG)"


JAVAMAKE:
	nmake -f javawin32.mak CFG="$(CFG)"


clean :
    if exist WinDebug\nul del /q WinDebug\*
    if exist WinDebug\nul rmdir WinDebug
    if exist WinRel\nul del /q Winrel\*
    if exist WinRel\nul rmdir Winrel
    if exist WinDebugW\nul del /q WinDebugW\*
    if exist WinDebugW\nul rmdir WinDebugW
    if exist WinRelW\nul del /q WinrelW\*
    if exist WinRelW\nul rmdir WinrelW
    if exist classes\nul rmdir classes /s
    if exist javastubs\nul rmdir javastubs /s



# End 
################################################################################
