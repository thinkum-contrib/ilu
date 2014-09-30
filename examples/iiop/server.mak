# Microsoft Developer Studio Generated NMAKE File, Format Version 4.10
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
!MESSAGE NMAKE /f "server.mak" CFG="Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 
################################################################################
# Begin Project
CPP=cl.exe
RSC=rc.exe

!IF  "$(CFG)" == "Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "WinRel"
# PROP Intermediate_Dir "WinRel"
# PROP Target_Dir ""
OUTDIR=.\WinRel
INTDIR=.\WinRel

ALL : "$(OUTDIR)\server.exe"

CLEAN : 
	-@erase "$(INTDIR)\iioptest1-common.obj"
	-@erase "$(INTDIR)\iioptest1-true.obj"
	-@erase "$(INTDIR)\server.obj"
	-@erase "$(OUTDIR)\server.exe"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /YX /c
# ADD CPP /nologo /MD /W3 /GX /O2 /I "$(ILUHOME)\include" /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /YX /c
CPP_PROJ=/nologo /MD /W3 /GX /O2 /I "$(ILUHOME)\include" /D "WIN32" /D "NDEBUG"\
 /D "_CONSOLE" /Fp"$(INTDIR)/server.pch" /YX /Fo"$(INTDIR)/" /c 
CPP_OBJS=.\WinRel/
CPP_SBRS=.\.
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/server.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 $(ILUHOME)\lib\iluc32.lib $(ILUHOME)\lib\ilu32.lib wsock32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /machine:I386
LINK32_FLAGS=$(ILUHOME)\lib\iluc32.lib $(ILUHOME)\lib\ilu32.lib wsock32.lib\
 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib\
 shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo\
 /subsystem:console /incremental:no /pdb:"$(OUTDIR)/server.pdb" /machine:I386\
 /out:"$(OUTDIR)/server.exe" 
LINK32_OBJS= \
	"$(INTDIR)\iioptest1-common.obj" \
	"$(INTDIR)\iioptest1-true.obj" \
	"$(INTDIR)\server.obj"

"$(OUTDIR)\server.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "WinDebug"
# PROP Intermediate_Dir "WinDebug"
# PROP Target_Dir ""
OUTDIR=.\WinDebug
INTDIR=.\WinDebug

ALL : "$(OUTDIR)\server.exe"

CLEAN : 
	-@erase "$(INTDIR)\iioptest1-common.obj"
	-@erase "$(INTDIR)\iioptest1-true.obj"
	-@erase "$(INTDIR)\server.obj"
	-@erase "$(INTDIR)\vc40.idb"
	-@erase "$(INTDIR)\vc40.pdb"
	-@erase "$(OUTDIR)\server.exe"
	-@erase "$(OUTDIR)\server.ilk"
	-@erase "$(OUTDIR)\server.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /YX /c
# ADD CPP /nologo /MD /W3 /Gm /GX /Zi /Od /I "$(ILUHOME)\include" /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /YX /c
CPP_PROJ=/nologo /MD /W3 /Gm /GX /Zi /Od /I "$(ILUHOME)\include" /D "WIN32" /D\
 "_DEBUG" /D "_CONSOLE" /Fp"$(INTDIR)/server.pch" /YX /Fo"$(INTDIR)/"\
 /Fd"$(INTDIR)/" /c 
CPP_OBJS=.\WinDebug/
CPP_SBRS=.\.
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/server.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /debug /machine:I386
# ADD LINK32 $(ILUHOME)\lib\iluc32.lib $(ILUHOME)\lib\ilu32.lib wsock32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /debug /machine:I386
LINK32_FLAGS=$(ILUHOME)\lib\iluc32.lib $(ILUHOME)\lib\ilu32.lib wsock32.lib\
 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib\
 shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo\
 /subsystem:console /incremental:yes /pdb:"$(OUTDIR)/server.pdb" /debug\
 /machine:I386 /out:"$(OUTDIR)/server.exe" 
LINK32_OBJS= \
	"$(INTDIR)\iioptest1-common.obj" \
	"$(INTDIR)\iioptest1-true.obj" \
	"$(INTDIR)\server.obj"

"$(OUTDIR)\server.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
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

.c{$(CPP_SBRS)}.sbr:
   $(CPP) $(CPP_PROJ) $<  

.cpp{$(CPP_SBRS)}.sbr:
   $(CPP) $(CPP_PROJ) $<  

.cxx{$(CPP_SBRS)}.sbr:
   $(CPP) $(CPP_PROJ) $<  

################################################################################
# Begin Target

# Name "Win32 Release"
# Name "Win32 Debug"

!IF  "$(CFG)" == "Win32 Release"

!ELSEIF  "$(CFG)" == "Win32 Debug"

!ENDIF 

################################################################################
# Begin Source File

SOURCE=.\server.c
DEP_CPP_SERVE=\
	".\iioptest1.h"\
	
NODEP_CPP_SERVE=\
	".\iluchdrs.h"\
	

"$(INTDIR)\server.obj" : $(SOURCE) $(DEP_CPP_SERVE) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=".\iioptest1-true.c"
DEP_CPP_IIOPT=\
	".\iioptest1.h"\
	
NODEP_CPP_IIOPT=\
	".\iluchdrs.h"\
	

"$(INTDIR)\iioptest1-true.obj" : $(SOURCE) $(DEP_CPP_IIOPT) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=".\iioptest1-common.c"
DEP_CPP_IIOPTE=\
	".\iioptest1.h"\
	
NODEP_CPP_IIOPTE=\
	".\iluchdrs.h"\
	

"$(INTDIR)\iioptest1-common.obj" : $(SOURCE) $(DEP_CPP_IIOPTE) "$(INTDIR)"


# End Source File
# End Target
# End Project
################################################################################
