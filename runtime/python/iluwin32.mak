# Top level makefile to build Win 32 version of ILU using
# Microsoft Visual C++
#
# Assumes your PATH has the appropriate bin directory in it (e.g. msvc20\bin)
# Assumes your INCLUDE environment variable includes the appropriate directories (e.g. msvc20\include) 
# Assumes your LIB environment variable includes the appropriate directories (e.g. msvc20\lib) 

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

!IF "$(PYTHONSRC)" == ""
ALL : NOPYTHONMAKE
!ELSE

# To build for python 1.4, Change the following line to ALL : PMAKE14
ALL : PMAKE15

!ENDIF

clean :
    if exist WinDebug\nul del /q WinDebug\*
    if exist WinDebug\nul rmdir WinDebug
    if exist WinRel\nul del /q Winrel\*
    if exist WinRel\nul rmdir Winrel

NOPYTHONMAKE :
    echo PYTHONSRC environment variable not set, not building Python runtime

# WARNING: By default, the Python Language Specific Runtime is only built if the
# environment variable PYTHONSRC is set to point to your Python Source directory 
# which contains the Python Include directory, the Python PC directory, and (for 
# Python 1.4, the Python vc40 directory containing python14.lib) (for Python 1.5.1, 
# the PCBuild\Release directory containing python15.lib).  
# 
# If you need these files, please retrieve the python source from http://www.python.org,
# as python1.4.tar.gz for Python 1.4, and pyth151.tgz for Python 1.5.1.  They are NOT 
# distributed with ILU.  
#
# Python 1.5.1 build is this makefile's default
#
# For Python 1.5.1, Ensure that you build the python 'Release' Configuration as this
# is set up to use the Multithreaded C Runtime DLL.  
# If you really want to build the 'Debug' version of python, you must adjust the project 
# to use the Multithreaded C Runtime DLL, NOT the Debug Multithreaded C Runtime DLL.
# Failure to do this will cause strange problems as ILU always uses Multithreaded C Runtime DLL.
# Also, if building the 'Debug' version of python, you should adjust all references to
# python15.lib in the iluPr15.mak file to reflect the fact that it's in 
# $(PYTHONSRC)\PCbuild\Debug
#
#
# For Python 1.4, Edit the file in the PC directory called python_nt.def, so as to also 
# export the following symbols: start_new_thread init_thread get_thread_ident exit_thread
# (and for compatibility with pythonwin, PyArg_ParseTupleAndKeywords).
# Then follow the instructions in the PC/readme.txt file to build python.
# Next, change the line for 'ALL' in this makefile to ALL : PMAKE14
#
# You will need to put ILUHOME/lib on your PYTHONPATH before
# using ILU with Python.

PMAKE14 :
    echo Building for Python 1.4 - read $(ILUSRC)\runtime\python\iluwin32.mak and 
    echo $(ILUSRC)\stubbers\python\iluwin32.mak to change python version
    copy pythonversion14.win pythonversion.h
    if exist iluimport.py.dist copy iluimport.py.dist iluimport.py
	if exist $(PYTHONSRC)\include\pythread.h echo #include "pythread.h" > pythonthreadheaderfile.h
	if not exist $(PYTHONSRC)\include\pythread.h echo #include "thread.h" > pythonthreadheaderfile.h
    nmake -f iluPr.mak CFG="$(CFG)"

PMAKE15 :
    echo Building for Python 1.5.1 - read $(ILUSRC)\runtime\python\iluwin32.mak and 
    echo $(ILUSRC)\stubbers\python\iluwin32.mak to change python version
    copy pythonversion15.win pythonversion.h
    if exist iluimport.py.dist copy iluimport.py.dist iluimport.py
	if exist $(PYTHONSRC)\include\pythread.h echo #include "pythread.h" > pythonthreadheaderfile.h
	if not exist $(PYTHONSRC)\include\pythread.h echo #include "thread.h" > pythonthreadheaderfile.h
    nmake -f iluPr15.mak CFG="$(CFG)"


# End 
################################################################################
