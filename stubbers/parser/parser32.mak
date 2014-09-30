# Modified so that $(ILUHOME) can be used to specifiy location
# Note that if you use this as a VC++ make file, $(ILUHOME) will be \ilu
# Microsoft Visual C++ Generated NMAKE File, Format Version 2.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Dynamic-Link Library" 0x0102

!IF "$(CFG)" == ""
CFG=Win32 Debug
!MESSAGE No configuration specified.  Defaulting to Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "Win32 Release" && "$(CFG)" != "Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE on this makefile
!MESSAGE by defining the macro CFG on the command line.  For example:
!MESSAGE 
!MESSAGE NMAKE /f "parser32.mak" CFG="Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "Win32 Release" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE "Win32 Debug" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

################################################################################
# Begin Project
# PROP Target_Last_Scanned "Win32 Debug"
MTL=MkTypLib.exe
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

ALL : $(ILUHOME)\bin\parser32.dll $(OUTDIR)/parser32.bsc

$(OUTDIR) : 
    if not exist $(OUTDIR)/nul mkdir $(OUTDIR)

# ADD BASE MTL /nologo /D "NDEBUG" /win32
# ADD MTL /nologo /D "NDEBUG" /win32
MTL_PROJ=/nologo /D "NDEBUG" /win32 
# ADD BASE CPP /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "ILU_BUILDING_PARSER" /FR /c
# ADD CPP /nologo /MD /W3 /GX /Ox /Oy- /I "\ilu\src\runtime\kernel" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "ILU_BUILDING_PARSER" /c
# SUBTRACT CPP /Fr
CPP_PROJ=/nologo /MD /W3 /GX /Ox /Oy- /I "$(ILUSRC)\runtime\kernel" /I "$(ILUSRC)\stubbers\idl" /I "$(ILUSRC)\stubbers\parser" /D\
 "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "ILU_BUILDING_PARSER" /Fp$(OUTDIR)/"parser32.pch" /Fo$(INTDIR)/ /c 
CPP_OBJS=.\WinRel/
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
BSC32_SBRS= \
	
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o$(OUTDIR)/"parser32.bsc" 

$(OUTDIR)/parser32.bsc : $(OUTDIR)  $(BSC32_SBRS)
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /NOLOGO /SUBSYSTEM:windows /DLL /MACHINE:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /NOLOGO /SUBSYSTEM:windows /DLL /MACHINE:I386 /OUT:"\ilu\bin\parser32.dll" /IMPLIB:"\ilu\lib\parser32.lib"
# SUBTRACT LINK32 /PDB:none
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib\
 advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib\
 odbccp32.lib /NOLOGO /SUBSYSTEM:windows /DLL /INCREMENTAL:no\
 /PDB:$(OUTDIR)/"parser32.pdb" /MACHINE:I386 /DEF:".\parser32.def"\
 /OUT:"$(ILUHOME)\bin\parser32.dll" /IMPLIB:"$(ILUHOME)\lib\parser32.lib" 
DEF_FILE=.\parser32.def
LINK32_OBJS= \
	$(INTDIR)/iluparse.obj \
	$(INTDIR)/malloc.obj \
	$(INTDIR)/pathname.obj \
	$(INTDIR)/shs.obj \
	$(INTDIR)/typeuid.obj \
	$(INTDIR)/typeuid2.obj \
	$(INTDIR)/util.obj \
	$(INTDIR)/aprintf.obj \
	$(INTDIR)/idlparser.obj \
	$(INTDIR)/idlscan.obj \
	$(INTDIR)/idl2isl.obj

$(ILUHOME)\bin\parser32.dll : $(OUTDIR)  $(DEF_FILE) $(LINK32_OBJS)
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

ALL : $(ILUHOME)\bin\parser32.dll $(OUTDIR)/parser32.bsc

$(OUTDIR) : 
    if not exist $(OUTDIR)/nul mkdir $(OUTDIR)

# ADD BASE MTL /nologo /D "_DEBUG" /win32
# ADD MTL /nologo /D "_DEBUG" /win32
MTL_PROJ=/nologo /D "_DEBUG" /win32 
# ADD BASE CPP /nologo /MT /W3 /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "ILU_BUILDING_PARSER" /FR /c
# ADD CPP /nologo /MD /W3 /GX /Zi /Od /I "\ilu\src\runtime\kernel" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "ILU_BUILDING_PARSER" /c
CPP_PROJ=/nologo /MD /W3 /GX /Zi /Od /I "$(ILUSRC)\runtime\kernel" /I "$(ILUSRC)\stubbers\idl" /I "$(ILUSRC)\stubbers\parser" /D\
 "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "ILU_BUILDING_PARSER" /Fp$(OUTDIR)/"parser32.pch" /Fo$(INTDIR)/\
 /Fd$(OUTDIR)/"parser32.pdb" /c 
CPP_OBJS=.\WinDebug/
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
BSC32_SBRS= \
	
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o$(OUTDIR)/"parser32.bsc" 

$(OUTDIR)/parser32.bsc : $(OUTDIR)  $(BSC32_SBRS)
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /NOLOGO /SUBSYSTEM:windows /DLL /DEBUG /MACHINE:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /NOLOGO /SUBSYSTEM:windows /DLL /DEBUG /MACHINE:I386 /OUT:"\ilu\bin\parser32.dll" /IMPLIB:"\ilu\lib\parser32.lib"
# SUBTRACT LINK32 /PDB:none
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib\
 advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib\
 odbccp32.lib /NOLOGO /SUBSYSTEM:windows /DLL /INCREMENTAL:yes\
 /PDB:$(OUTDIR)/"parser32.pdb" /DEBUG /MACHINE:I386 /DEF:".\parser32.def"\
 /OUT:"$(ILUHOME)\bin\parser32.dll" /IMPLIB:"$(ILUHOME)\lib\parser32.lib" 
DEF_FILE=.\parser32.def
LINK32_OBJS= \
	$(INTDIR)/iluparse.obj \
	$(INTDIR)/malloc.obj \
	$(INTDIR)/pathname.obj \
	$(INTDIR)/shs.obj \
	$(INTDIR)/typeuid.obj \
	$(INTDIR)/typeuid2.obj \
	$(INTDIR)/util.obj \
	$(INTDIR)/aprintf.obj \
	$(INTDIR)/idlparser.obj \
	$(INTDIR)/idlscan.obj \
	$(INTDIR)/idl2isl.obj

$(ILUHOME)\bin\parser32.dll : $(OUTDIR)  $(DEF_FILE) $(LINK32_OBJS)
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

SOURCE=.\iluparse.c
DEP_ILUPA=\
	.\iluptype.h\
	$(ILUSRC)\runtime\kernel\version.h

$(INTDIR)/iluparse.obj :  $(SOURCE)  $(DEP_ILUPA) $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\malloc.c

$(INTDIR)/malloc.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\pathname.c

$(INTDIR)/pathname.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\shs.c
DEP_SHS_C=\
	.\shs.h

$(INTDIR)/shs.obj :  $(SOURCE)  $(DEP_SHS_C) $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\typeuid.c
DEP_TYPEU=\
	.\iluptype.h\
	.\shs.h

$(INTDIR)/typeuid.obj :  $(SOURCE)  $(DEP_TYPEU) $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\typeuid2.c
DEP_TYPEU=\
	.\iluptype.h\
	.\shs.h

$(INTDIR)/typeuid2.obj :  $(SOURCE)  $(DEP_TYPEU) $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\util.c
DEP_UTIL_=\
	$(ILUSRC)\runtime\kernel\version.h\
	.\iluptype.h

$(INTDIR)/util.obj :  $(SOURCE)  $(DEP_UTIL_) $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\aprintf.c
DEP_APRIN=

$(INTDIR)/aprintf.obj :  $(SOURCE)  $(DEP_APRIN) $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=$(ILUSRC)\stubbers\idl\idlparser.c
DEP_IDLPA=\
	.\iluptype.h

$(INTDIR)/idlparser.obj :  $(SOURCE)  $(DEP_IDLPA) $(INTDIR)
	$(CPP) $(CPP_PROJ) $(SOURCE)
	
# End Source File
################################################################################
# Begin Source File

SOURCE=$(ILUSRC)\stubbers\idl\idlscan.c
DEP_IDLSC=\
	.\iluptype.h

$(INTDIR)/idlscan.obj :  $(SOURCE)  $(DEP_IDLSC) $(INTDIR)
	$(CPP) $(CPP_PROJ) $(SOURCE)

# End Source File
################################################################################
# Begin Source File

SOURCE=$(ILUSRC)\stubbers\idl\idl2isl.c
DEP_IDLSC=\
	.\iluptype.h \
	..\idl\iluidl.h

$(INTDIR)/idl2isl.obj :  $(SOURCE)  $(DEP_IDLSC) $(INTDIR)
	$(CPP) $(CPP_PROJ) $(SOURCE)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\parser32.def
# End Source File
# End Group
# End Project
################################################################################
