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

!IF "$(CFG)" != "Win32 Release" && "$(CFG)" != "Win32 Debug" && "$(CFG)" != "clean"
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
!MESSAGE "clean"
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 


################################################################################

SUBDIRECTORIES= kernel.dir c.dir python.dir java.dir cpp.dir cpp2.dir lisp.dir 

CLEANSUBS=  kernel.clean c.clean python.clean java.clean cpp.clean cpp2.clean lisp.clean

################################################################################

ALL : $(ILUHOME) $(ILUHOME)\bin $(ILUHOME)\lib $(SUBDIRECTORIES)

$(ILUHOME) : 
    if not exist $(ILUHOME)\nul mkdir $(ILUHOME)

$(ILUHOME)\lib : 
    if not exist $(ILUHOME)\lib\nul mkdir $(ILUHOME)\lib

$(ILUHOME)\bin : 
    if not exist $(ILUHOME)\bin\nul mkdir $(ILUHOME)\bin

$(SUBDIRECTORIES) :
	cd $*
	nmake -f iluwin32.mak CFG="$(CFG)"
	cd ..


clean : $(CLEANSUBS)

$(CLEANSUBS) :
	cd $*
	nmake -f iluwin32.mak CFG="$(CFG)" clean
	cd ..
	


# End 
################################################################################
