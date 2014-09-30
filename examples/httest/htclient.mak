################################################################################
# Microsoft Developer Studio Generated NMAKE File, Format Version 4.10
# ** NOTE: Manually modified for command line builds with $(ILUHOME) **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

!IF "$(CFG)" == ""
CFG=Win32 Debug
!MESSAGE No configuration specified.  Defaulting to Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "Win32 Release" && "$(CFG)" !=\
 "Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE on this makefile
!MESSAGE by defining the macro CFG on the command line.  For example:
!MESSAGE 
!MESSAGE NMAKE /f "htclient.mak" CFG="Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "Win32 Release" (based on\
 "Win32 (x86) Console Application")
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
RSC=rc.exe
CPP=cl.exe

!IF  "$(CFG)" == "Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "WinRel"
# PROP BASE Intermediate_Dir "WinRel"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "WinRel"
# PROP Intermediate_Dir "WinRel"
# PROP Target_Dir ""
OUTDIR=.\WinRel
INTDIR=.\WinRel

ALL : "$(OUTDIR)\htclient.exe"

CLEAN : 
	-@erase "$(INTDIR)\htclient.obj"
	-@erase "$(INTDIR)\httest-common.obj"
	-@erase "$(INTDIR)\httest-surrogate.obj"
	-@erase "$(INTDIR)\iluhttp-common.obj"
	-@erase "$(INTDIR)\iluhttp-surrogate.obj"
	-@erase "$(OUTDIR)\htclient.exe"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /YX /c
# ADD CPP /nologo /MD /W3 /GX /O2 /I $(ILUHOME)\include /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /YX /c
CPP_PROJ=/nologo /MD /W3 /GX /O2 /I $(ILUHOME)\include /D "WIN32" /D "NDEBUG"\
 /D "_CONSOLE" /Fp"$(INTDIR)/htclient.pch" /YX /Fo"$(INTDIR)/" /c 
CPP_OBJS=.\WinRel/
CPP_SBRS=.\.
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/htclient.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 $(ILUHOME)\lib\iluc32.lib $(ILUHOME)\lib\ilu32.lib wsock32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /machine:I386
LINK32_FLAGS=$(ILUHOME)\lib\iluc32.lib $(ILUHOME)\lib\ilu32.lib wsock32.lib\
 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib\
 shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo\
 /subsystem:console /incremental:no /pdb:"$(OUTDIR)/htclient.pdb" /machine:I386\
 /out:"$(OUTDIR)/htclient.exe" 
LINK32_OBJS= \
	"$(INTDIR)\htclient.obj" \
	"$(INTDIR)\httest-common.obj" \
	"$(INTDIR)\httest-surrogate.obj" \
	"$(INTDIR)\iluhttp-common.obj" \
	"$(INTDIR)\iluhttp-surrogate.obj"

"$(OUTDIR)\htclient.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "WinDebug"
# PROP BASE Intermediate_Dir "WinDebug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "WinDebug"
# PROP Intermediate_Dir "WinDebug"
# PROP Target_Dir ""
OUTDIR=.\WinDebug
INTDIR=.\WinDebug

ALL : "$(OUTDIR)\htclient.exe"

CLEAN : 
	-@erase "$(INTDIR)\htclient.obj"
	-@erase "$(INTDIR)\httest-common.obj"
	-@erase "$(INTDIR)\httest-surrogate.obj"
	-@erase "$(INTDIR)\iluhttp-common.obj"
	-@erase "$(INTDIR)\iluhttp-surrogate.obj"
	-@erase "$(INTDIR)\vc40.idb"
	-@erase "$(INTDIR)\vc40.pdb"
	-@erase "$(OUTDIR)\htclient.exe"
	-@erase "$(OUTDIR)\htclient.ilk"
	-@erase "$(OUTDIR)\htclient.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /YX /c
# ADD CPP /nologo /MD /W3 /Gm /GX /Zi /Od /I $(ILUHOME)\include /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /YX /c
CPP_PROJ=/nologo /MD /W3 /Gm /GX /Zi /Od /I $(ILUHOME)\include /D "WIN32" /D\
 "_DEBUG" /D "_CONSOLE" /Fp"$(INTDIR)/htclient.pch" /YX /Fo"$(INTDIR)/"\
 /Fd"$(INTDIR)/" /c 
CPP_OBJS=.\WinDebug/
CPP_SBRS=.\.
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/htclient.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /debug /machine:I386
# ADD LINK32 $(ILUHOME)\lib\iluc32.lib $(ILUHOME)\lib\ilu32.lib wsock32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /debug /machine:I386
LINK32_FLAGS=$(ILUHOME)\lib\iluc32.lib $(ILUHOME)\lib\ilu32.lib wsock32.lib\
 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib\
 shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo\
 /subsystem:console /incremental:yes /pdb:"$(OUTDIR)/htclient.pdb" /debug\
 /machine:I386 /out:"$(OUTDIR)/htclient.exe" 
LINK32_OBJS= \
	"$(INTDIR)\htclient.obj" \
	"$(INTDIR)\httest-common.obj" \
	"$(INTDIR)\httest-surrogate.obj" \
	"$(INTDIR)\iluhttp-common.obj" \
	"$(INTDIR)\iluhttp-surrogate.obj"

"$(OUTDIR)\htclient.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
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

SOURCE=".\iluhttp-surrogate.c"

!IF  "$(CFG)" == "Win32 Release"


"$(INTDIR)\iluhttp-surrogate.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_HTTP_=\
	".\iluhttp.h"\
	
NODEP_CPP_HTTP_=\
	".\iluchdrs.h"\
	

"$(INTDIR)\iluhttp-surrogate.obj" : $(SOURCE) $(DEP_CPP_HTTP_) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=".\iluhttp-common.c"
DEP_CPP_HTTP_C=\
	".\iluhttp.h"\
	
NODEP_CPP_HTTP_C=\
	".\iluchdrs.h"\
	

"$(INTDIR)\iluhttp-common.obj" : $(SOURCE) $(DEP_CPP_HTTP_C) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=".\httest-surrogate.c"
DEP_CPP_HTTES=\
	".\httest.h"\
	".\iluhttp.h"\
	
NODEP_CPP_HTTES=\
	".\iluchdrs.h"\
	

"$(INTDIR)\httest-surrogate.obj" : $(SOURCE) $(DEP_CPP_HTTES) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=".\httest-common.c"
DEP_CPP_HTTEST=\
	".\httest.h"\
	".\iluhttp.h"\
	
NODEP_CPP_HTTEST=\
	".\iluchdrs.h"\
	

"$(INTDIR)\httest-common.obj" : $(SOURCE) $(DEP_CPP_HTTEST) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\htclient.c
DEP_CPP_HTCLI=\
	$(ILUHOME)\include\iludebug.h\
	$(ILUHOME)\include\iluhash.h\
	".\httest.h"\
	".\iluhttp.h"\
	$(ILUHOME)\include\ilubasic.h\
	$(ILUHOME)\include\iluerror.h\
	$(ILUHOME)\include\iluerrs.h\
	$(ILUHOME)\include\iluntrnl.h\
	$(ILUHOME)\include\iluwin.h\
	$(ILUHOME)\include\iluxport.h
	
NODEP_CPP_HTCLI=\
	".\iluchdrs.h"\
	

"$(INTDIR)\htclient.obj" : $(SOURCE) $(DEP_CPP_HTCLI) "$(INTDIR)"


# End Source File
# End Target
# End Project
################################################################################
