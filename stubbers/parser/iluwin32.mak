# Top level makefile to build Windows NT 3.5 version of ILU using
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

ALL : $(ILUHOME) $(ILUHOME)\bin $(ILUHOME)\lib PARSERMAKE GENREGSMAKE ISLSCANMAKE

$(ILUHOME) : 
    if not exist $(ILUHOME)\nul mkdir $(ILUHOME)

$(ILUHOME)\lib : 
    if not exist $(ILUHOME)\lib\nul mkdir $(ILUHOME)\lib

$(ILUHOME)\bin : 
    if not exist $(ILUHOME)\bin\nul mkdir $(ILUHOME)\bin

PARSERMAKE :
	copy ..\..\runtime\kernel\iluwin.h .
	if not exist iluparse.c copy iluparse.c.dist iluparse.c
	if not exist iluparse.h copy iluparse.h.dist iluparse.h
	if not exist ..\idl\idlparser-output.c copy ..\idl\idlparser-output.c.dist ..\idl\idlparser-output.c
	if not exist ..\idl\idlparser.c copy ..\idl\idlparser.c.dist ..\idl\idlparser.c
	if not exist ..\idl\idlparser.h copy ..\idl\idlparser.h.dist ..\idl\idlparser.h
	if not exist ..\idl\idlscan.c copy ..\idl\idlscan.c.dist ..\idl\idlscan.c
	nmake -f parser32.mak CFG="$(CFG)"

ISLSCANMAKE :
	nmake -f islscan.mak CFG="$(CFG)"

GENREGSMAKE :
	nmake -f genregs.mak CFG="$(CFG)"
	genregs "../../runtime/kernel/ilutpcod"

clean :
    if exist WinDebug\nul del /q WinDebug\*
    if exist WinDebug\nul rmdir WinDebug
    if exist WinRel\nul del /q Winrel\*
    if exist WinRel\nul rmdir Winrel


# End 
################################################################################
