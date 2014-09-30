# Modified so that $(ILUHOME) can be used to specifiy location
# Note that if you use this as a VC++ make file, $(ILUHOME) will be \ilu
# Microsoft Visual C++ Generated NMAKE File, Format Version 2.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

!IF "$(CFG)" == ""
CFG=Win32 Debug
!MESSAGE No configuration specified.  Defaulting to Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "Win32 Release" && "$(CFG)" != "Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE on this makefile
!MESSAGE by defining the macro CFG on the command line.  For example:
!MESSAGE 
!MESSAGE NMAKE /f "cppstubber.mak" CFG="Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

################################################################################
# Begin Project
# PROP Target_Last_Scanned "Win32 Debug"
CPP=cl.exe
RSC=rc.exe

!IF  "$(CFG)" == "Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "WinRel"
# PROP BASE Intermediate_Dir "WinRel"
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "WinRel"
# PROP Intermediate_Dir "WinRel"
OUTDIR=.\WinRel
INTDIR=.\WinRel

ALL : $(ILUHOME)\bin\c++-stubber.exe $(OUTDIR)/cppstubber.bsc

$(OUTDIR) : 
    if not exist $(OUTDIR)/nul mkdir $(OUTDIR)

# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /FR /c
# ADD CPP /nologo /MD /W3 /GX /Zi /Ox /Oy- /I "\ilu\src\stubbers\parser" /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /c
# SUBTRACT CPP /Fr
CPP_PROJ=/nologo /MD /W3 /GX /Zi /Ox /Oy- /I "$(ILUSRC)\stubbers\parser"\
 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /Fp$(OUTDIR)/"cppstubber.pch"\
 /Fo$(INTDIR)/ /c /I "$(ILUSRC)\runtime\kernel" 
CPP_OBJS=.\WinRel/
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
BSC32_SBRS= \
	
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o$(OUTDIR)/"cppstubber.bsc" 

$(OUTDIR)/cppstubber.bsc : $(OUTDIR)  $(BSC32_SBRS)
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /NOLOGO /SUBSYSTEM:console /MACHINE:I386
# ADD LINK32 \ilu\lib\parser32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /NOLOGO /SUBSYSTEM:console /MACHINE:I386 /OUT:"\ilu\bin\c++-stubber.exe"
# SUBTRACT LINK32 /DEBUG
LINK32_FLAGS=$(ILUHOME)\lib\parser32.lib kernel32.lib user32.lib gdi32.lib\
 winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib\
 uuid.lib odbc32.lib odbccp32.lib /NOLOGO /SUBSYSTEM:console /INCREMENTAL:no\
 /PDB:$(OUTDIR)/"cppstubber.pdb" /MACHINE:I386 /OUT:"$(ILUHOME)\bin\c++-stubber.exe" 
DEF_FILE=
LINK32_OBJS= \
	$(INTDIR)/cplusplus.obj \
	$(INTDIR)/headers.obj \
	$(INTDIR)/declare-object.obj \
	$(INTDIR)/common.obj \
	$(INTDIR)/code.obj \
	$(INTDIR)/client.obj \
	$(INTDIR)/names.obj \
	$(INTDIR)/server.obj

"$(ILUHOME)\bin\c++-stubber.exe" : $(OUTDIR)  $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "WinDebug"
# PROP BASE Intermediate_Dir "WinDebug"
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "WinDebug"
# PROP Intermediate_Dir "WinDebug"
OUTDIR=.\WinDebug
INTDIR=.\WinDebug

ALL : $(ILUHOME)\bin\c++-stubber.exe $(OUTDIR)/cppstubber.bsc

$(OUTDIR) : 
    if not exist $(OUTDIR)/nul mkdir $(OUTDIR)

# ADD BASE CPP /nologo /W3 /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /FR /c
# ADD CPP /nologo /MD /W3 /GX /Zi /Od /I "\ilu\src\stubbers\parser" /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /c
# SUBTRACT CPP /Fr
CPP_PROJ=/nologo /MD /W3 /GX /Zi /Od /I "$(ILUSRC)\stubbers\parser" /D\
 "WIN32" /D "_DEBUG" /D "_CONSOLE" /Fp$(OUTDIR)/"cppstubber.pch" /Fo$(INTDIR)/\
 /c /I "$(ILUSRC)\runtime\kernel" 
CPP_OBJS=.\WinDebug/
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
BSC32_SBRS= \
	
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o$(OUTDIR)/"cppstubber.bsc" 

$(OUTDIR)/cppstubber.bsc : $(OUTDIR)  $(BSC32_SBRS)
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /NOLOGO /SUBSYSTEM:console /DEBUG /MACHINE:I386
# ADD LINK32 \ilu\lib\parser32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /NOLOGO /SUBSYSTEM:console /DEBUG /MACHINE:I386 /OUT:"\ilu\bin\c++-stubber.exe"
LINK32_FLAGS=$(ILUHOME)\lib\parser32.lib kernel32.lib user32.lib gdi32.lib\
 winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib\
 uuid.lib odbc32.lib odbccp32.lib /NOLOGO /SUBSYSTEM:console /INCREMENTAL:yes\
 /PDB:$(OUTDIR)/"cppstubber.pdb" /DEBUG /MACHINE:I386\
 /OUT:"$(ILUHOME)\bin\c++-stubber.exe" 
DEF_FILE=
LINK32_OBJS= \
	$(INTDIR)/cplusplus.obj \
	$(INTDIR)/headers.obj \
	$(INTDIR)/declare-object.obj \
	$(INTDIR)/common.obj \
	$(INTDIR)/code.obj \
	$(INTDIR)/client.obj \
	$(INTDIR)/names.obj \
	$(INTDIR)/server.obj

"$(ILUHOME)\bin\c++-stubber.exe" : $(OUTDIR)  $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ENDIF 

.c{$(CPP_OBJS)}.obj:
   $(CPP) $(CPP_PROJ) $<  

.cpp{$(CPP_OBJS)}.obj:
   $(CPP) $(CPP_PROJ) $<  

.cxx{$(CPP_OBJS)}.obj:
   $(CPP) $(CPP_PROJ) $<  

################################################################################
# Begin Group "Source Files"

################################################################################
# Begin Source File

SOURCE=.\cplusplus.c
DEP_CPLUS=\
	".\cplusplus.h"\
	"$(ILUSRC)\stubbers\parser\iluptype.h"

$(INTDIR)/cplusplus.obj :  $(SOURCE)  $(DEP_CPLUS) $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\headers.c
DEP_HEADE=\
	".\cplusplus.h"\
	"$(ILUSRC)\stubbers\parser\iluptype.h"

$(INTDIR)/headers.obj :  $(SOURCE)  $(DEP_HEADE) $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=".\declare-object.c"

$(INTDIR)/declare-object.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\common.c
DEP_COMMO=\
	".\cplusplus.h"\
	"$(ILUSRC)\stubbers\parser\iluptype.h"

$(INTDIR)/common.obj :  $(SOURCE)  $(DEP_COMMO) $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\code.c
DEP_CODE_=\
	".\cplusplus.h"\
	"$(ILUSRC)\stubbers\parser\iluptype.h"

$(INTDIR)/code.obj :  $(SOURCE)  $(DEP_CODE_) $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\client.c

$(INTDIR)/client.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\names.c
DEP_NAMES=\
	".\cplusplus.h"\
	"$(ILUSRC)\stubbers\parser\iluptype.h"

$(INTDIR)/names.obj :  $(SOURCE)  $(DEP_NAMES) $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\server.c
DEP_SERVE=\
	".\cplusplus.h"\
	"$(ILUSRC)\stubbers\parser\iluptype.h"

$(INTDIR)/server.obj :  $(SOURCE)  $(DEP_SERVE) $(INTDIR)

# End Source File
# End Group
# End Project
################################################################################
