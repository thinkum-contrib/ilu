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
!MESSAGE NMAKE /f "fibber.mak" CFG="Win32 Debug"
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

ALL : ".\WinRel\fibber.exe"

CLEAN : 
	-@erase ".\WinRel\Fibber-common.obj"
	-@erase ".\WinRel\Fibber-surrogate.obj"
	-@erase ".\WinRel\Fibber-true.obj"
	-@erase ".\WinRel\fibber.exe"
	-@erase ".\WinRel\fprog.obj"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /I $(ILUHOME)\include /YX /c
# ADD CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /I $(ILUHOME)\include /YX /c
CPP_PROJ=/nologo /ML /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /I $(ILUHOME)\include\
 /Fp"$(INTDIR)/fibber.pch" /YX /Fo"$(INTDIR)/" /c 
CPP_OBJS=.\WinRel/
CPP_SBRS=.\.
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/fibber.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /machine:I386
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib\
 advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib\
 odbccp32.lib /nologo /subsystem:console /incremental:no\
 /pdb:"$(OUTDIR)/fibber.pdb" /machine:I386 /out:"$(OUTDIR)/fibber.exe" 
LINK32_OBJS= \
	"$(ILUHOME)\lib\ilu32.lib" \
	"$(ILUHOME)\lib\iluc32.lib" \
	".\WinRel\Fibber-common.obj" \
	".\WinRel\Fibber-surrogate.obj" \
	".\WinRel\Fibber-true.obj" \
	".\WinRel\fprog.obj"

".\WinRel\fibber.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
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

ALL : ".\WinDebug\fibber.exe"

CLEAN : 
	-@erase ".\WinDebug\Fibber-common.obj"
	-@erase ".\WinDebug\Fibber-surrogate.obj"
	-@erase ".\WinDebug\Fibber-true.obj"
	-@erase ".\WinDebug\fibber.exe"
	-@erase ".\WinDebug\fibber.ilk"
	-@erase ".\WinDebug\fibber.pdb"
	-@erase ".\WinDebug\fprog.obj"
	-@erase ".\WinDebug\vc40.idb"
	-@erase ".\WinDebug\vc40.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /I $(ILUHOME)\include /YX /c
# ADD CPP /nologo /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /I $(ILUHOME)\include /YX /c
CPP_PROJ=/nologo /MLd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /I $(ILUHOME)\include\
 /Fp"$(INTDIR)/fibber.pch" /YX /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c 
CPP_OBJS=.\WinDebug/
CPP_SBRS=.\.
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/fibber.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /debug /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /debug /machine:I386
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib\
 advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib\
 odbccp32.lib /nologo /subsystem:console /incremental:yes\
 /pdb:"$(OUTDIR)/fibber.pdb" /debug /machine:I386 /out:"$(OUTDIR)/fibber.exe" 
LINK32_OBJS= \
	"$(ILUHOME)\lib\ilu32.lib" \
	"$(ILUHOME)\lib\iluc32.lib" \
	".\WinDebug\Fibber-common.obj" \
	".\WinDebug\Fibber-surrogate.obj" \
	".\WinDebug\Fibber-true.obj" \
	".\WinDebug\fprog.obj"

".\WinDebug\fibber.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
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

# Name "fibber - Win32 Release"
# Name "fibber - Win32 Debug"

!IF  "$(CFG)" == "Win32 Release"

!ELSEIF  "$(CFG)" == "Win32 Debug"

!ENDIF 

################################################################################
# Begin Source File

SOURCE=.\fprog.c
DEP_CPP_FPROG=\
	".\Fibber.h"\
	{$(ILUHOME)\include}"\ilubasic.h"\
	{$(ILUHOME)\include}"\iluchdrs.h"\
	{$(ILUHOME)\include}"\iluerror.h"\
	{$(ILUHOME)\include}"\iluerrs.h"\
	{$(ILUHOME)\include}"\iluwin.h"\
	{$(ILUHOME)\include}"\iluxport.h"\
	

!IF  "$(CFG)" == "Win32 Release"


".\WinRel\fprog.obj" : $(SOURCE) $(DEP_CPP_FPROG) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "Win32 Debug"


".\WinDebug\fprog.obj" : $(SOURCE) $(DEP_CPP_FPROG) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=".\Fibber-common.c"
DEP_CPP_FIBBE=\
	".\Fibber.h"\
	{$(ILUHOME)\include}"\ilubasic.h"\
	{$(ILUHOME)\include}"\iluchdrs.h"\
	{$(ILUHOME)\include}"\iluerror.h"\
	{$(ILUHOME)\include}"\iluerrs.h"\
	{$(ILUHOME)\include}"\iluwin.h"\
	{$(ILUHOME)\include}"\iluxport.h"\
	

!IF  "$(CFG)" == "Win32 Release"


".\WinRel\Fibber-common.obj" : $(SOURCE) $(DEP_CPP_FIBBE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "Win32 Debug"


".\WinDebug\Fibber-common.obj" : $(SOURCE) $(DEP_CPP_FIBBE) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=".\Fibber-surrogate.c"
DEP_CPP_FIBBER=\
	".\Fibber.h"\
	{$(ILUHOME)\include}"\ilubasic.h"\
	{$(ILUHOME)\include}"\iluchdrs.h"\
	{$(ILUHOME)\include}"\iluerror.h"\
	{$(ILUHOME)\include}"\iluerrs.h"\
	{$(ILUHOME)\include}"\iluwin.h"\
	{$(ILUHOME)\include}"\iluxport.h"\
	

!IF  "$(CFG)" == "Win32 Release"


".\WinRel\Fibber-surrogate.obj" : $(SOURCE) $(DEP_CPP_FIBBER) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "Win32 Debug"


".\WinDebug\Fibber-surrogate.obj" : $(SOURCE) $(DEP_CPP_FIBBER) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=".\Fibber-true.c"
DEP_CPP_FIBBER_=\
	".\Fibber.h"\
	{$(ILUHOME)\include}"\ilubasic.h"\
	{$(ILUHOME)\include}"\iluchdrs.h"\
	{$(ILUHOME)\include}"\iluerror.h"\
	{$(ILUHOME)\include}"\iluerrs.h"\
	{$(ILUHOME)\include}"\iluwin.h"\
	{$(ILUHOME)\include}"\iluxport.h"\
	

!IF  "$(CFG)" == "Win32 Release"


".\WinRel\Fibber-true.obj" : $(SOURCE) $(DEP_CPP_FIBBER_) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "Win32 Debug"


".\WinDebug\Fibber-true.obj" : $(SOURCE) $(DEP_CPP_FIBBER_) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\Fibber.h

!IF  "$(CFG)" == "Win32 Release"

!ELSEIF  "$(CFG)" == "Win32 Debug"

!ENDIF 

# End Source File
# End Target
# End Project
################################################################################
