# Top level makefile to build Win 32version of ILU using
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

!IF "$(PYTHONSRC)" == ""
ALL : NOPYTHONMAKE
!ELSE

# To build for python 1.4, Change the following line to ALL : PMAKE14
ALL : $(ILUHOME) $(ILUHOME)\bin $(ILUHOME)\lib PMAKE15

!ENDIF

NOPYTHONMAKE :
    echo PYTHONSRC environment variable not set, not building Python Stubber

$(ILUHOME) : 
    if not exist $(ILUHOME)\nul mkdir $(ILUHOME)

$(ILUHOME)\lib : 
    if not exist $(ILUHOME)\lib\nul mkdir $(ILUHOME)\lib

$(ILUHOME)\bin : 
    if not exist $(ILUHOME)\bin\nul mkdir $(ILUHOME)\bin

PMAKE14 :
    echo Building for Python 1.4 - read $(ILUSRC)\runtime\python\iluwin32.mak and 
    echo $(ILUSRC)\stubbers\python\iluwin32.mak to change python version
    copy $(ILUSRC)\runtime\python\pythonversion14.win $(ILUSRC)\runtime\python\pythonversion.h
    nmake -f pstubber.mak CFG="$(CFG)"

PMAKE15 :
    echo Building for Python 1.5.1 - read $(ILUSRC)\runtime\python\iluwin32.mak and 
    echo $(ILUSRC)\stubbers\python\iluwin32.mak to change python version
    copy $(ILUSRC)\runtime\python\pythonversion15.win $(ILUSRC)\runtime\python\pythonversion.h
    nmake -f pstubber.mak CFG="$(CFG)"


clean :
    if exist WinDebug\nul del /q WinDebug\*
    if exist WinDebug\nul rmdir WinDebug
    if exist WinRel\nul del /q Winrel\*
    if exist WinRel\nul rmdir Winrel



# End 
################################################################################