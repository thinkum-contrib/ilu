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
!MESSAGE NMAKE /f "fooclient.mak" CFG="Win32 Debug"
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

ALL : "$(OUTDIR)\fooclient.exe"

CLEAN : 
	-@erase "$(INTDIR)\clientmain.obj"
	-@erase "$(INTDIR)\foo-cpp.obj"
	-@erase "$(INTDIR)\foo-cppsurrogate.obj"
	-@erase "$(OUTDIR)\fooclient.exe"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /YX /c
# ADD CPP /nologo /MD /W3 /GX /O2 /I "$(ILUHOME)\include" /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /YX /c
# SUBTRACT CPP /Fr
CPP_PROJ=/nologo /MD /W3 /GX /O2 /I "$(ILUHOME)\include" /D "WIN32" /D "NDEBUG" /D "_CONSOLE"\
 /Fp"$(INTDIR)/fooclient.pch" /YX /Fo"$(INTDIR)/" /c 
CPP_OBJS=.\WinRel/
CPP_SBRS=.\.
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/fooclient.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 $(ILUHOME)\lib\ilucpp2.lib $(ILUHOME)\lib\ilu32.lib wsock32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /machine:I386
LINK32_FLAGS=$(ILUHOME)\lib\ilucpp2.lib $(ILUHOME)\lib\ilu32.lib\
 wsock32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib\
 advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib\
 odbccp32.lib /nologo /subsystem:console /incremental:no\
 /pdb:"$(OUTDIR)/fooclient.pdb" /machine:I386 /out:"$(OUTDIR)/fooclient.exe" 
LINK32_OBJS= \
	"$(INTDIR)\clientmain.obj" \
	"$(INTDIR)\foo-cpp.obj" \
	"$(INTDIR)\foo-cppsurrogate.obj"

"$(OUTDIR)\fooclient.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
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

ALL : "$(OUTDIR)\fooclient.exe"

CLEAN : 
	-@erase "$(INTDIR)\clientmain.obj"
	-@erase "$(INTDIR)\clientmain.sbr"
	-@erase "$(INTDIR)\foo-cpp.obj"
	-@erase "$(INTDIR)\foo-cpp.sbr"
	-@erase "$(INTDIR)\foo-cppsurrogate.obj"
	-@erase "$(INTDIR)\foo-cppsurrogate.sbr"
	-@erase "$(INTDIR)\vc40.idb"
	-@erase "$(INTDIR)\vc40.pdb"
	-@erase "$(OUTDIR)\fooclient.bsc"
	-@erase "$(OUTDIR)\fooclient.exe"
	-@erase "$(OUTDIR)\fooclient.ilk"
	-@erase "$(OUTDIR)\fooclient.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /YX /c
# ADD CPP /nologo /MD /W3 /Gm /GX /Zi /Od /I "$(ILUHOME)\include" /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /YX /c
CPP_PROJ=/nologo /MD /W3 /Gm /GX /Zi /Od \
 /I "$(ILUHOME)\include" /D "WIN32" /D "_DEBUG" /D "_CONSOLE" \
 /Fp"$(INTDIR)/fooclient.pch" /YX /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c 
CPP_OBJS=.\WinDebug/

# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/fooclient.bsc" 
BSC32_SBRS= \
	"$(INTDIR)\clientmain.sbr" \
	"$(INTDIR)\foo-cpp.sbr" \
	"$(INTDIR)\foo-cppsurrogate.sbr"

"$(OUTDIR)\fooclient.bsc" : "$(OUTDIR)" $(BSC32_SBRS)
    $(BSC32) @<<
  $(BSC32_FLAGS) $(BSC32_SBRS)
<<

LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /debug /machine:I386
# ADD LINK32 $(ILUHOME)\lib\ilucpp2.lib $(ILUHOME)\lib\ilu32.lib wsock32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /debug /machine:I386
LINK32_FLAGS=$(ILUHOME)\lib\ilucpp2.lib $(ILUHOME)\lib\ilu32.lib\
 msvcirt.lib wsock32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib\
 advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib\
 odbccp32.lib /nologo /subsystem:console /incremental:yes\
 /pdb:"$(OUTDIR)/fooclient.pdb" /debug /machine:I386\
 /out:"$(OUTDIR)/fooclient.exe" 
LINK32_OBJS= \
	"$(INTDIR)\clientmain.obj" \
	"$(INTDIR)\foo-cpp.obj" \
	"$(INTDIR)\foo-cppsurrogate.obj"

"$(OUTDIR)\fooclient.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
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

SOURCE=.\clientmain.cpp

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_CLIEN=\
	"$(ILUHOME)\include\iluhash.h"\
	"$(ILUHOME)\include\ilubasic.h"\
	"$(ILUHOME)\include\iludebug.h"\
	"$(ILUHOME)\include\iluerror.h"\
	"$(ILUHOME)\include\iluerrs.h"\
	"$(ILUHOME)\include\iluntrnl.h"\
	"$(ILUHOME)\include\iluwin.h"\
	"$(ILUHOME)\include\iluxport.h"\
	"$(ILUHOME)\include\corba-templates.hpp"\
	"$(ILUHOME)\include\corba.hpp"\
	"$(ILUHOME)\include\cppportability.hpp"\
	"$(ILUHOME)\include\ilu.hpp"\
	".\foo-cpp.hpp"\
	".\foo-cppsurrogate.hpp"\
	
NODEP_CPP_CLIEN=\
	"$(ILUHOME)\include\ilutypes.h"\
	

"$(INTDIR)\clientmain.obj" : $(SOURCE) $(DEP_CPP_CLIEN) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_CLIEN=\
	"$(ILUHOME)\include\corba.hpp"\
	".\foo-cpp.hpp"\
	".\foo-cppsurrogate.hpp"\
	

BuildCmds= \
	$(CPP) $(CPP_PROJ) $(SOURCE) \
	

"$(INTDIR)\clientmain.obj" : $(SOURCE) $(DEP_CPP_CLIEN) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\clientmain.sbr" : $(SOURCE) $(DEP_CPP_CLIEN) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=".\foo-cppsurrogate.cpp"

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_foo_=\
	"$(ILUHOME)\include\iluhash.h"\
	"$(ILUHOME)\include\ilubasic.h"\
	"$(ILUHOME)\include\iludebug.h"\
	"$(ILUHOME)\include\iluerror.h"\
	"$(ILUHOME)\include\iluerrs.h"\
	"$(ILUHOME)\include\iluntrnl.h"\
	"$(ILUHOME)\include\iluwin.h"\
	"$(ILUHOME)\include\iluxport.h"\
	"$(ILUHOME)\include\corba-templates.hpp"\
	"$(ILUHOME)\include\corba.hpp"\
	"$(ILUHOME)\include\cppportability.hpp"\
	"$(ILUHOME)\include\ilu.hpp"\
	".\foo-cpp.hpp"\
	".\foo-cppsurrogate.hpp"\
	
NODEP_CPP_foo_=\
	"$(ILUHOME)\include\ilutypes.h"\
	

"$(INTDIR)\foo-cppsurrogate.obj" : $(SOURCE) $(DEP_CPP_foo_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_foo_=\
	"$(ILUHOME)\include\corba.hpp"\
	".\foo-cpp.hpp"\
	".\foo-cppsurrogate.hpp"\
	

BuildCmds= \
	$(CPP) $(CPP_PROJ) $(SOURCE) \
	

"$(INTDIR)\foo-cppsurrogate.obj" : $(SOURCE) $(DEP_CPP_foo_) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\foo-cppsurrogate.sbr" : $(SOURCE) $(DEP_CPP_foo_) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=".\foo-cpp.cpp"
DEP_CPP_foo_C=\
	"$(ILUHOME)\include\iluhash.h"\
	"$(ILUHOME)\include\ilubasic.h"\
	"$(ILUHOME)\include\iludebug.h"\
	"$(ILUHOME)\include\iluerror.h"\
	"$(ILUHOME)\include\iluerrs.h"\
	"$(ILUHOME)\include\iluntrnl.h"\
	"$(ILUHOME)\include\iluwin.h"\
	"$(ILUHOME)\include\iluxport.h"\
	"$(ILUHOME)\include\corba-templates.hpp"\
	"$(ILUHOME)\include\corba.hpp"\
	"$(ILUHOME)\include\cppportability.hpp"\
	"$(ILUHOME)\include\ilu.hpp"\
	".\foo-cpp.hpp"\
	
NODEP_CPP_foo_C=\
	"$(ILUHOME)\include\ilutypes.h"\
	

!IF  "$(CFG)" == "Win32 Release"


"$(INTDIR)\foo-cpp.obj" : $(SOURCE) $(DEP_CPP_foo_C) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"


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
