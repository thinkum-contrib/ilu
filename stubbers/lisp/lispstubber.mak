# Microsoft Developer Studio Generated NMAKE File, Format Version 4.00
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
!MESSAGE NMAKE /f "lispstubber.mak" CFG="Win32 Debug"
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
# PROP Target_Last_Scanned "Win32 Debug"
CPP=cl.exe
RSC=rc.exe

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

ALL : "$(ILUHOME)\bin\lisp-stubber.exe"

CLEAN : 
	-@erase ".\WinRel\exports.obj"
	-@erase ".\WinRel\names.obj"
	-@erase ".\WinRel\lisp.obj"
	-@erase ".\WinRel\class-basics.obj"
	-@erase ".\WinRel\class-server.obj"
	-@erase ".\WinRel\type-basics.obj"
	-@erase ".\WinRel\sysdcl.obj"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /FR /c
# ADD CPP /nologo /MD /W3 /GX /Zi /Ox /Oy- /I "\ilu\src\stubbers\parser" /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /c
CPP_PROJ=/nologo /MD /W3 /GX /Zi /Ox /Oy- /I "$(ILUSRC)\stubbers\parser" /D "WIN32" /D\
 "NDEBUG" /D "_CONSOLE" /Fp"$(INTDIR)/lisp-stubber.pch" /Fo"$(INTDIR)/" /c 
CPP_OBJS=.\WinRel/
CPP_SBRS=
# ADD BASE RSC /l 0x1009 /d "NDEBUG"
# ADD RSC /l 0x1009 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/lisp-stubber.bsc" 
BSC32_SBRS=
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 \ilu\lib\parser32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /machine:I386
LINK32_FLAGS=$(ILUHOME)\lib\parser32.lib kernel32.lib user32.lib gdi32.lib\
 winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib\
 uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /incremental:no\
 /pdb:"$(OUTDIR)/lisp-stubber.pdb" /machine:I386\
 /out:"$(ILUHOME)/bin/lisp-stubber.exe" 
LINK32_OBJS= \
	"$(INTDIR)/exports.obj" \
	"$(INTDIR)/names.obj" \
	"$(INTDIR)/lisp.obj" \
	"$(INTDIR)/class-basics.obj" \
	"$(INTDIR)/class-server.obj" \
	"$(INTDIR)/type-basics.obj" \
	"$(INTDIR)/sysdcl.obj"

"$(ILUHOME)\bin\lisp-stubber.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
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

ALL : "$(ILUHOME)\bin\lisp-stubber.exe"

CLEAN : 
	-@erase ".\WinDebug\vc40.pdb"
	-@erase ".\WinDebug\vc40.idb"
	-@erase ".\WinDebug\lisp.obj"
	-@erase ".\WinDebug\type-basics.obj"
	-@erase ".\WinDebug\names.obj"
	-@erase ".\WinDebug\class-basics.obj"
	-@erase ".\WinDebug\exports.obj"
	-@erase ".\WinDebug\sysdcl.obj"
	-@erase ".\WinDebug\class-server.obj"
	-@erase ".\WinDebug\lisp-stubber.ilk"
	-@erase ".\WinDebug\lisp-stubber.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /W3 /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /FR /c
# ADD CPP /nologo /MD /W3 /GX /Zi /Od /I "\ilu\src\stubbers\parser" /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /c
CPP_PROJ=/nologo /MD /W3 /GX /Zi /Od /I "$(ILUSRC)\stubbers\parser" /D\
 "WIN32" /D "_DEBUG" /D "_CONSOLE" /Fp"$(INTDIR)/lisp-stubber.pch" \
 /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c 
CPP_OBJS=.\WinDebug/
CPP_SBRS=
# ADD BASE RSC /l 0x1009 /d "_DEBUG"
# ADD RSC /l 0x1009 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/lisp-stubber.bsc" 
BSC32_SBRS=
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /debug /machine:I386
# ADD LINK32 \ilu\lib\parser32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /debug /machine:I386
LINK32_FLAGS=$(ILUHOME)\lib\parser32.lib kernel32.lib user32.lib gdi32.lib\
 winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib\
 uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /incremental:yes\
 /pdb:"$(OUTDIR)/lisp-stubber.pdb" /debug /machine:I386\
 /out:"$(ILUHOME)/bin/lisp-stubber.exe" 
LINK32_OBJS= \
	"$(INTDIR)/lisp.obj" \
	"$(INTDIR)/type-basics.obj" \
	"$(INTDIR)/names.obj" \
	"$(INTDIR)/class-basics.obj" \
	"$(INTDIR)/exports.obj" \
	"$(INTDIR)/sysdcl.obj" \
	"$(INTDIR)/class-server.obj"

"$(ILUHOME)\bin\lisp-stubber.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
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

SOURCE=".\type-basics.c"
DEP_CPP_TYPE_=\
	".\lisp.h"
	

"$(INTDIR)\type-basics.obj" : $(SOURCE) $(DEP_CPP_TYPE_) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=".\class-server.c"
DEP_CPP_CLASS=\
	".\lisp.h"
	

"$(INTDIR)\class-server.obj" : $(SOURCE) $(DEP_CPP_CLASS) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\exports.c
DEP_CPP_EXPOR=\
	".\lisp.h"
	

"$(INTDIR)\exports.obj" : $(SOURCE) $(DEP_CPP_EXPOR) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\lisp.c
DEP_CPP_LISP_=\
	".\lisp.h"\
	{$(INCLUDE)}"\sys\Stat.h"\
	{$(INCLUDE)}"\sys\Types.h"
	

"$(INTDIR)\lisp.obj" : $(SOURCE) $(DEP_CPP_LISP_) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\names.c
DEP_CPP_NAMES=\
	".\lisp.h"
	

"$(INTDIR)\names.obj" : $(SOURCE) $(DEP_CPP_NAMES) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\sysdcl.c
DEP_CPP_SYSDC=\
	".\lisp.h"
	

"$(INTDIR)\sysdcl.obj" : $(SOURCE) $(DEP_CPP_SYSDC) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=".\class-basics.c"
DEP_CPP_CLASS_=\
	".\lisp.h"
	

"$(INTDIR)\class-basics.obj" : $(SOURCE) $(DEP_CPP_CLASS_) "$(INTDIR)"


# End Source File
# End Target
# End Project
################################################################################
