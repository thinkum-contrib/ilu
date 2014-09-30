# Microsoft Developer Studio Generated NMAKE File, Format Version 4.20
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
!MESSAGE NMAKE /f "fooserver.mak" CFG="Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "Win32 Release" (based on\
 "Win32 (x86) Console Application")
!MESSAGE "Win32 Debug" (based on\
 "Win32 (x86) Console Application")
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
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "WinRel"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "WinRel"
# PROP Target_Dir ""
OUTDIR=.\WinRel
INTDIR=.\WinRel

ALL : "$(OUTDIR)\fooserver.exe" 

CLEAN : 
	-@erase "$(INTDIR)\foo-cpp.obj"
	-@erase "$(INTDIR)\foo-cpp.sbr"
	-@erase "$(INTDIR)\foo-cpptrue.obj"
	-@erase "$(INTDIR)\foo-cpptrue.sbr"
	-@erase "$(INTDIR)\servermain.obj"
	-@erase "$(INTDIR)\servermain.sbr"
	-@erase "$(OUTDIR)\fooserver.bsc"
	-@erase "$(OUTDIR)\fooserver.exe"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /YX /c
# ADD CPP /nologo /MD /W3 /GX /O2 /I "g:\dlilu\foo" /I "g:\dlilu\cpp2" /I "$(ILUHOME)\include" /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /YX /c
CPP_PROJ=/nologo /MD /W3 /GX /O2 /I "g:\dlilu\foo" /I "g:\dlilu\cpp2" /I\
 "$(ILUHOME)\include" /D "WIN32" /D "NDEBUG" /D "_CONSOLE" \
 /Fp"$(INTDIR)/fooserver.pch" /YX /Fo"$(INTDIR)/" /c 
CPP_OBJS=.\Release/
CPP_SBRS=.\Release/
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/fooserver.bsc" 
BSC32_SBRS= \
	"$(INTDIR)\foo-cpp.sbr" \
	"$(INTDIR)\foo-cpptrue.sbr" \
	"$(INTDIR)\servermain.sbr"

"$(OUTDIR)\fooserver.bsc" : "$(OUTDIR)" $(BSC32_SBRS)
    $(BSC32) @<<
  $(BSC32_FLAGS) $(BSC32_SBRS)
<<

LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 $(ILUHOME)\lib\ilucpp2.lib $(ILUHOME)\lib\ilu32.lib wsock32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /machine:I386
LINK32_FLAGS=$(ILUHOME)\lib\ilucpp2.lib $(ILUHOME)\lib\ilu32.lib wsock32.lib\
 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib\
 shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo\
 /subsystem:console /incremental:no /pdb:"$(OUTDIR)/fooserver.pdb"\
 /machine:I386 /out:"$(OUTDIR)/fooserver.exe" 
LINK32_OBJS= \
	"$(INTDIR)\foo-cpp.obj" \
	"$(INTDIR)\foo-cpptrue.obj" \
	"$(INTDIR)\servermain.obj"

"$(OUTDIR)\fooserver.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "WinDebug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "WinDebug"
# PROP Target_Dir ""
OUTDIR=.\WinDebug
INTDIR=.\WinDebug

ALL : "$(OUTDIR)\fooserver.exe"

CLEAN : 
	-@erase "$(INTDIR)\foo-cpp.obj"
	-@erase "$(INTDIR)\foo-cpp.sbr"
	-@erase "$(INTDIR)\foo-cpptrue.obj"
	-@erase "$(INTDIR)\foo-cpptrue.sbr"
	-@erase "$(INTDIR)\servermain.obj"
	-@erase "$(INTDIR)\servermain.sbr"
	-@erase "$(INTDIR)\vc40.idb"
	-@erase "$(INTDIR)\vc40.pdb"
	-@erase "$(OUTDIR)\fooserver.bsc"
	-@erase "$(OUTDIR)\fooserver.exe"
	-@erase "$(OUTDIR)\fooserver.ilk"
	-@erase "$(OUTDIR)\fooserver.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /YX /c
# ADD CPP /nologo /MD /W3 /Gm /GX /Zi /Od /I "$(ILUHOME)\include" /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /YX /c
CPP_PROJ=/nologo /MD /W3 /Gm /GX /Zi /Od \
 /I "$(ILUHOME)\include" /D "WIN32" /D "_DEBUG" /D "_CONSOLE" \
 /Fp"$(INTDIR)/fooserver.pch" /YX /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c 
CPP_OBJS=.\WinDebug/
CPP_SBRS=.\WinDebug/
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/fooserver.bsc" 
BSC32_SBRS= \
	"$(INTDIR)\foo-cpp.sbr" \
	"$(INTDIR)\foo-cpptrue.sbr" \
	"$(INTDIR)\servermain.sbr"

"$(OUTDIR)\fooserver.bsc" : "$(OUTDIR)" $(BSC32_SBRS)
    $(BSC32) @<<
  $(BSC32_FLAGS) $(BSC32_SBRS)
<<

LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /debug /machine:I386
# ADD LINK32 $(ILUHOME)\lib\ilucpp2.lib $(ILUHOME)\lib\ilu32.lib wsock32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /debug /machine:I386
LINK32_FLAGS=$(ILUHOME)\lib\ilucpp2.lib $(ILUHOME)\lib\ilu32.lib msvcirt.lib wsock32.lib\
 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib\
 shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo\
 /subsystem:console /incremental:yes /pdb:"$(OUTDIR)/fooserver.pdb" /debug\
 /machine:I386 /out:"$(OUTDIR)/fooserver.exe" 
LINK32_OBJS= \
	"$(INTDIR)\foo-cpp.obj" \
	"$(INTDIR)\foo-cpptrue.obj" \
	"$(INTDIR)\servermain.obj"

"$(OUTDIR)\fooserver.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
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

SOURCE=.\servermain.cpp

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_SERVE=\
	"$(ILUHOME)\include\corba.hpp"\
	".\foo-cpp.hpp"\
	".\foo-cpptrue.hpp"\
	

BuildCmds= \
	$(CPP) $(CPP_PROJ) $(SOURCE) \
	

"$(INTDIR)\servermain.obj" : $(SOURCE) $(DEP_CPP_SERVE) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\servermain.sbr" : $(SOURCE) $(DEP_CPP_SERVE) "$(INTDIR)"
   $(BuildCmds)

!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_SERVE=\
	"$(ILUHOME)\include\iluntrnl.h"\
	"$(ILUHOME)\include\corba.hpp"\
	"$(ILUHOME)\include\ilu.hpp"\
	".\foo-cpp.hpp"\
	".\foo-cpptrue.hpp"\
	

BuildCmds= \
	$(CPP) $(CPP_PROJ) $(SOURCE) \
	

"$(INTDIR)\servermain.obj" : $(SOURCE) $(DEP_CPP_SERVE) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\servermain.sbr" : $(SOURCE) $(DEP_CPP_SERVE) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=".\foo-cpptrue.cpp"

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_foo_=\
	"$(ILUHOME)\include\corba.hpp"\
	".\foo-cpp.hpp"\
	".\foo-cpptrue.hpp"\
	

BuildCmds= \
	$(CPP) $(CPP_PROJ) $(SOURCE) \
	

"$(INTDIR)\foo-cpptrue.obj" : $(SOURCE) $(DEP_CPP_foo_) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\foo-cpptrue.sbr" : $(SOURCE) $(DEP_CPP_foo_) "$(INTDIR)"
   $(BuildCmds)

!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_foo_=\
	"$(ILUHOME)\include\iluntrnl.h"\
	"$(ILUHOME)\include\corba.hpp"\
	"$(ILUHOME)\include\ilu.hpp"\
	".\foo-cpp.hpp"\
	".\foo-cpptrue.hpp"\
	

BuildCmds= \
	$(CPP) $(CPP_PROJ) $(SOURCE) \
	

"$(INTDIR)\foo-cpptrue.obj" : $(SOURCE) $(DEP_CPP_foo_) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\foo-cpptrue.sbr" : $(SOURCE) $(DEP_CPP_foo_) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=".\foo-cpp.cpp"

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_foo_C=\
	"$(ILUHOME)\include\corba.hpp"\
	".\foo-cpp.hpp"\
	

BuildCmds= \
	$(CPP) $(CPP_PROJ) $(SOURCE) \
	

"$(INTDIR)\foo-cpp.obj" : $(SOURCE) $(DEP_CPP_foo_C) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\foo-cpp.sbr" : $(SOURCE) $(DEP_CPP_foo_C) "$(INTDIR)"
   $(BuildCmds)

!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_foo_C=\
	"$(ILUHOME)\include\iluntrnl.h"\
	"$(ILUHOME)\include\corba.hpp"\
	"$(ILUHOME)\include\ilu.hpp"\
	".\foo-cpp.hpp"\
	

BuildCmds= \
	$(CPP) $(CPP_PROJ) $(SOURCE) \
	

"$(INTDIR)\foo-cpp.obj" : $(SOURCE) $(DEP_CPP_foo_C) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\foo-cpp.sbr" : $(SOURCE) $(DEP_CPP_foo_C) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
# End Target
# End Project
################################################################################
