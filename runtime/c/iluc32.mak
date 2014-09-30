# Modified so that $(ILUHOME) can be used to specifiy location
# Note that if you use this as a VC++ make file, $(ILUHOME) wiil be \ilu
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
!MESSAGE NMAKE /f "iluc32.mak" CFG="Win32 Debug"
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

ALL : $(ILUHOME)\bin\iluc32.dll $(OUTDIR)/iluc32.bsc

$(OUTDIR) : 
    if not exist $(OUTDIR)/nul mkdir $(OUTDIR)

# ADD BASE MTL /nologo /D "NDEBUG" /win32
# ADD MTL /nologo /D "NDEBUG" /win32
MTL_PROJ=/nologo /D "NDEBUG" /win32 
# ADD BASE CPP /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /FR /c
# ADD CPP /nologo /MD /W3 /GX /Ox /Oy- /I "\ilu\src\runtime\kernel" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /c
# SUBTRACT CPP /Fr
CPP_PROJ=/nologo /MD /W3 /GX /Ox /Oy- /I "$(ILUSRC)\runtime\c" /I "$(ILUSRC)\runtime\kernel" /I "$(ILUSRC)\GSS\kernel" /D\
 "WIN32" /D "NDEBUG" /D "_WINDOWS" /Fp$(OUTDIR)/"iluc32.pch" /Fo$(INTDIR)/ /c /D "ILU_BUILDING_RUNTIME"
CPP_OBJS=.\WinRel/
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
BSC32_SBRS= \
    
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o$(OUTDIR)/"iluc32.bsc" 

$(OUTDIR)/iluc32.bsc : $(OUTDIR)  $(BSC32_SBRS)
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /NOLOGO /SUBSYSTEM:windows /DLL /MACHINE:I386
# ADD LINK32 \ilu\lib\ilu32.lib wsock32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /NOLOGO /SUBSYSTEM:windows /DLL /MACHINE:I386 /OUT:"\ilu\bin\iluc32.dll" /IMPLIB:"\ilu\lib\iluc32.lib"
# SUBTRACT LINK32 /PDB:none
LINK32_FLAGS=$(ILUHOME)\lib\ilu32.lib wsock32.lib kernel32.lib user32.lib gdi32.lib\
 winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib\
 uuid.lib odbc32.lib odbccp32.lib /NOLOGO /SUBSYSTEM:windows /DLL\
 /INCREMENTAL:no /PDB:$(OUTDIR)/"iluc32.pdb" /MACHINE:I386 /DEF:".\iluc32.def"\
 /OUT:"$(ILUHOME)\bin\iluc32.dll" /IMPLIB:"$(ILUHOME)\lib\iluc32.lib" 
DEF_FILE=.\iluc32.def
LINK32_OBJS= \
    $(INTDIR)/ilu.obj \
    $(INTDIR)/orb.obj

$(ILUHOME)\bin\iluc32.dll : $(OUTDIR)  $(DEF_FILE) $(LINK32_OBJS)
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

ALL : $(ILUHOME)\bin\iluc32.dll $(OUTDIR)/iluc32.bsc

$(OUTDIR) : 
    if not exist $(OUTDIR)/nul mkdir $(OUTDIR)

# ADD BASE MTL /nologo /D "_DEBUG" /win32
# ADD MTL /nologo /D "_DEBUG" /win32
MTL_PROJ=/nologo /D "_DEBUG" /win32 
# ADD BASE CPP /nologo /MT /W3 /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /FR /c
# ADD CPP /nologo /MD /W3 /GX /Zi /Od /I "\ilu\src\runtime\kernel" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /c
# SUBTRACT CPP /Fr
CPP_PROJ=/nologo /MD /W3 /GX /Zi /Od /I "$(ILUSRC)\runtime\c" /I "$(ILUSRC)\runtime\kernel" /I "$(ILUSRC)\GSS\kernel" /D\
 "WIN32" /D "_DEBUG" /D "_WINDOWS" /Fp$(OUTDIR)/"iluc32.pch" /Fo$(INTDIR)/\
 /Fd$(OUTDIR)/"iluc32.pdb" /c /D "ILU_BUILDING_RUNTIME" $(ILU_DEBUG_CFLAGS)
CPP_OBJS=.\WinDebug/
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
BSC32_SBRS= \
    
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o$(OUTDIR)/"iluc32.bsc" 

$(OUTDIR)/iluc32.bsc : $(OUTDIR)  $(BSC32_SBRS)
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /NOLOGO /SUBSYSTEM:windows /DLL /DEBUG /MACHINE:I386
# ADD LINK32 \ilu\lib\ilu32.lib wsock32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /NOLOGO /SUBSYSTEM:windows /DLL /DEBUG /MACHINE:I386 /OUT:"\ilu\bin\iluc32.dll" /IMPLIB:"\ilu\lib\iluc32.lib"
# SUBTRACT LINK32 /PDB:none
LINK32_FLAGS= $(ILUHOME)\lib\ilu32.lib wsock32.lib kernel32.lib user32.lib gdi32.lib\
 winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib\
 uuid.lib odbc32.lib odbccp32.lib /NOLOGO /SUBSYSTEM:windows /DLL\
 /INCREMENTAL:yes /PDB:$(OUTDIR)/"iluc32.pdb" /DEBUG /MACHINE:I386\
 /DEF:".\iluc32.def" /OUT:"$(ILUHOME)\bin\iluc32.dll" /IMPLIB:"$(ILUHOME)\lib\iluc32.lib" 
DEF_FILE=.\iluc32.def
LINK32_OBJS= \
    $(INTDIR)/ilu.obj \
    $(INTDIR)/orb.obj

$(ILUHOME)\bin\iluc32.dll : $(OUTDIR)  $(DEF_FILE) $(LINK32_OBJS)
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

SOURCE=.\ilu.c
DEP_ILU_C=\
    .\iluchdrs.h\
    .\ilucstub.h\
    .\ilucpvt.h\
    $(ILUSRC)\runtime\kernel\iluxport.h\
    $(ILUSRC)\runtime\kernel\iluerror.h\
    $(ILUSRC)\runtime\kernel\ilubasic.h

$(INTDIR)/ilu.obj :  $(SOURCE)  $(DEP_ILU_C) $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\orb.c
DEP_ORB_C=\
    .\iluchdrs.h\
    .\ilucstub.h\
    .\ilucpvt.h\
    $(ILUSRC)\runtime\kernel\iluxport.h\
    $(ILUSRC)\runtime\kernel\iluerror.h\
    $(ILUSRC)\runtime\kernel\ilubasic.h

$(INTDIR)/orb.obj :  $(SOURCE) $(DEP_ORB_C) $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\iluc32.def
# End Source File
# End Group
# End Project
################################################################################
