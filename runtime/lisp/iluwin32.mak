# Top level makefile to build Win32 version of ILU using
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

ALL : $(ILUHOME) $(ILUHOME)\bin $(ILUHOME)\lib LISPMAKE

$(ILUHOME) : 
    if not exist $(ILUHOME)\nul mkdir $(ILUHOME)

$(ILUHOME)\lib : 
    if not exist $(ILUHOME)\lib\nul mkdir $(ILUHOME)\lib

$(ILUHOME)\bin : 
    if not exist $(ILUHOME)\bin\nul mkdir $(ILUHOME)\bin

LISPMAKE : 
!MESSAGE WARNING: By default, the LISP Language Specific Runtime is NOT built!
!MESSAGE To build it, uncomment the follwing lines in this makefile.
!MESSAGE Note that ilulisp32.mak builds only the C portions.
#	if not exist pdefsys.lisp copy pdefsys.lisp.dist pdefsys.lisp
#	if not exist ilu-sysdcl.lisp copy ilu-win-sysdcl.lisp ilu-sysdcl.lisp
#	nmake -f ilulisp32.mak CFG="$(CFG)"

clean :
    if exist WinDebug\nul del /q WinDebug\*
    if exist WinDebug\nul rmdir WinDebug
    if exist WinRel\nul del /q Winrel\*
    if exist WinRel\nul rmdir Winrel


# End 
################################################################################
