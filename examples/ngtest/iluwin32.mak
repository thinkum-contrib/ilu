# Top level makefile to build Win32 version of ILU examples using
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
.SUFFIXES : .isl

# to make an h file from an isl file
.isl.h:
    c-stubber $*.isl
    
# to make an c file from an isl file
.isl.c:
    c-stubber $*.isl
    

ALL : HTTESTMAKE

STUBSHEADERS=IANA_Charsets_Registry.h iluhttp.h NgBasic.h NgCache.h NgDocument.h NgFormProcessor.h NgProperty.h NgRendering.h NgStream.h

STUBSCOMMON=IANA_Charsets_Registry-common.c iluhttp-common.c NgBasic-common.c NgCache-common.c NgDocument-common.c NgFormProcessor-common.c NgProperty-common.c NgRendering-common.c NgStream-common.c

STUBSTRUES=IANA_Charsets_Registry-true.c iluhttp-true.c NgBasic-true.c NgCache-true.c NgDocument-true.c NgFormProcessor-true.c NgProperty-true.c NgRendering-true.c NgStream-true.c

STUBSURROGATES=IANA_Charsets_Registry-surrogate.c iluhttp-surrogate.c NgBasic-surrogate.c NgCache-surrogate.c NgDocument-surrogate.c NgFormProcessor-surrogate.c NgProperty-surrogate.c NgRendering-surrogate.c NgStream-surrogate.c

IANA_Charsets_Registry.h IANA_Charsets_Registry-common.c IANA_Charsets_Registry-true.c IANA_Charsets_Registry-surrogate.c : ianacharsets.isl
    c-stubber ianacharsets.isl
    
NgFormProcessor.h NgFormProcessor-common.c NgFormProcessor-true.c NgFormProcessor-surrogate.c : ngform.isl
    c-stubber ngform.isl



HTTESTMAKE : $(STUBSHEADERS) $(STUBSCOMMON) $(STUBSTRUES) $(STUBSURROGATES)
    nmake -f ngwebserver.mak CFG="$(CFG)"
    nmake -f nggetbot.mak CFG="$(CFG)"


clean :
    if exist WinDebug\nul del /q WinDebug\*
    if exist WinDebug\nul rmdir WinDebug
    if exist WinRel\nul del /q Winrel\*
    if exist WinRel\nul rmdir Winrel
    del $(STUBSHEADERS)
    del $(STUBSCOMMON)
    del $(STUBSTRUES)
    del $(STUBSURROGATES)



# End 
################################################################################
