# Microsoft Developer Studio Generated NMAKE File, Format Version 4.10
# ** DO NOT EDIT **

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
!MESSAGE NMAKE /f "bserver.mak" CFG="Win32 Debug"
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
RSC=rc.exe
CPP=cl.exe

!IF  "$(CFG)" == "Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Target_Dir ""
OUTDIR=.\WinRel
INTDIR=.\WinRel

ALL : ".\WinRel\bserver.exe"

CLEAN : 
	-@erase ".\WinRel\Batcher-common.obj"
	-@erase ".\WinRel\Batcher-true.obj"
	-@erase ".\WinRel\bserver.exe"
	-@erase ".\WinRel\bsvr.obj"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /I $(ILUHOME)\include /YX /c
# ADD CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /I $(ILUHOME)\include /YX /c
CPP_PROJ=/nologo /ML /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /I $(ILUHOME)\include\
 /Fp"$(INTDIR)/bserver.pch" /YX /Fo"$(INTDIR)/" /c 
CPP_OBJS=.\WinRel/
CPP_SBRS=.\.
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/bserver.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /machine:I386
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib\
 advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib\
 odbccp32.lib /nologo /subsystem:console /incremental:no\
 /pdb:"$(OUTDIR)/bserver.pdb" /machine:I386 /out:"$(OUTDIR)/bserver.exe" 
LINK32_OBJS= \
	"$(ILUHOME)\lib\ilu32.lib" \
	"$(ILUHOME)\lib\iluc32.lib" \
	".\WinRel\Batcher-common.obj" \
	".\WinRel\Batcher-true.obj" \
	".\WinRel\bsvr.obj"

".\WinRel\bserver.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
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
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Target_Dir ""
OUTDIR=.\WinDebug
INTDIR=.\WinDebug

ALL : ".\WinDebug\bserver.exe"

CLEAN : 
	-@erase ".\WinDebug\Batcher-common.obj"
	-@erase ".\WinDebug\Batcher-true.obj"
	-@erase ".\WinDebug\bserver.exe"
	-@erase ".\WinDebug\bserver.ilk"
	-@erase ".\WinDebug\bserver.pdb"
	-@erase ".\WinDebug\bsvr.obj"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /I $(ILUHOME)\include /YX /c
# ADD CPP /nologo /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /I $(ILUHOME)\include /YX /c
CPP_PROJ=/nologo /MLd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /I $(ILUHOME)\include\
 /Fp"$(INTDIR)/bserver.pch" /YX /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c 
CPP_OBJS=.\WinDebug/
CPP_SBRS=.\.
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/bserver.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /debug /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /debug /machine:I386
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib\
 advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib\
 odbccp32.lib /nologo /subsystem:console /incremental:yes\
 /pdb:"$(OUTDIR)/bserver.pdb" /debug /machine:I386 /out:"$(OUTDIR)/bserver.exe" 
LINK32_OBJS= \
	"$(ILUHOME)\lib\ilu32.lib" \
	"$(ILUHOME)\lib\iluc32.lib" \
	".\WinDebug\Batcher-common.obj" \
	".\WinDebug\Batcher-true.obj" \
	".\WinDebug\bsvr.obj"

".\WinDebug\bserver.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
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

SOURCE=.\bsvr.c
DEP_CPP_BSVR_=\
	".\Batcher.h"\
	{$(ILUHOME)\include}"\ilubasic.h"\
	{$(ILUHOME)\include}"\iluchdrs.h"\
	{$(ILUHOME)\include}"\iluerror.h"\
	{$(ILUHOME)\include}"\iluerrs.h"\
	{$(ILUHOME)\include}"\iluwin.h"\
	{$(ILUHOME)\include}"\iluxport.h"\
	

!IF  "$(CFG)" == "Win32 Release"


".\WinRel\bsvr.obj" : $(SOURCE) $(DEP_CPP_BSVR_) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "Win32 Debug"


".\WinDebug\bsvr.obj" : $(SOURCE) $(DEP_CPP_BSVR_) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=".\Batcher-common.c"
DEP_CPP_BATCH=\
	".\Batcher.h"\
	{$(ILUHOME)\include}"\ilubasic.h"\
	{$(ILUHOME)\include}"\iluchdrs.h"\
	{$(ILUHOME)\include}"\iluerror.h"\
	{$(ILUHOME)\include}"\iluerrs.h"\
	{$(ILUHOME)\include}"\iluwin.h"\
	{$(ILUHOME)\include}"\iluxport.h"\
	

!IF  "$(CFG)" == "Win32 Release"


".\WinRel\Batcher-common.obj" : $(SOURCE) $(DEP_CPP_BATCH) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "Win32 Debug"


".\WinDebug\Batcher-common.obj" : $(SOURCE) $(DEP_CPP_BATCH) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=".\Batcher-true.c"
DEP_CPP_BATCHE=\
	".\Batcher.h"\
	{$(ILUHOME)\include}"\ilubasic.h"\
	{$(ILUHOME)\include}"\iluchdrs.h"\
	{$(ILUHOME)\include}"\iluerror.h"\
	{$(ILUHOME)\include}"\iluerrs.h"\
	{$(ILUHOME)\include}"\iluwin.h"\
	{$(ILUHOME)\include}"\iluxport.h"\
	

!IF  "$(CFG)" == "Win32 Release"


".\WinRel\Batcher-true.obj" : $(SOURCE) $(DEP_CPP_BATCHE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "Win32 Debug"


".\WinDebug\Batcher-true.obj" : $(SOURCE) $(DEP_CPP_BATCHE) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\Batcher.h

!IF  "$(CFG)" == "Win32 Release"

!ELSEIF  "$(CFG)" == "Win32 Debug"

!ENDIF 

# End Source File
# End Target
# End Project
################################################################################
