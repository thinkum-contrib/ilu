# Top level makefile to build Win32  version of ILU using
# Microsoft Visual C++
#
# Assumes your PATH has the appropriate bin directory in it (e.g. msvc20\bin)
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

!IF  "$(CFG)" == "Win32 Release"
PARSERULES=$(ILUSRC)\stubbers\genstub\WinRel\parserules.exe
!ELSE
PARSERULES=$(ILUSRC)\stubbers\genstub\WinDebug\parserules.exe
!ENDIF 

RULEFILES=cpp.hpp.rules cpp.cpp.rules cppsurrogate.hpp.rules cppsurrogate.cpp.rules cpptrue.hpp.rules cpptrue.cpp.rules

################################################################################

ALL : $(ILUHOME) $(ILUHOME)\bin $(ILUHOME)\lib CPPSTUBBERMAKE

$(ILUHOME) : 
    if not exist $(ILUHOME)\nul mkdir $(ILUHOME)

$(ILUHOME)\lib : 
    if not exist $(ILUHOME)\lib\nul mkdir $(ILUHOME)\lib

$(ILUHOME)\bin : 
    if not exist $(ILUHOME)\bin\nul mkdir $(ILUHOME)\bin

encoded-rules.c : $(RULEFILES)
	$(PARSERULES) encoded-rules.c $(RULEFILES)
	

CPPSTUBBERMAKE : encoded-rules.c
	nmake -f cpp2stubber.mak CFG="$(CFG)"

clean :
    if exist encoded-rules.c del /q encoded-rules.c
    if exist WinDebug\nul del /q WinDebug\*
    if exist WinDebug\nul rmdir WinDebug
    if exist WinRel\nul del /q Winrel\*
    if exist WinRel\nul rmdir Winrel




# End 
################################################################################
