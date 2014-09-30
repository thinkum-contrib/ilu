# Microsoft Developer Studio Generated NMAKE File, Format Version 4.10
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

!IF "$(CFG)" == ""
CFG=Win32 Debug
!MESSAGE No configuration specified.  Defaulting to Win32\
 Debug.
!ENDIF 

!IF "$(CFG)" != "Win32 Release" && "$(CFG)" !=\
 "Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE on this makefile
!MESSAGE by defining the macro CFG on the command line.  For example:
!MESSAGE 
!MESSAGE NMAKE /f "pstubber.mak" CFG="Win32 Debug"
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

ALL : "$(ILUHOME)\bin\python-stubber.exe"

CLEAN : 
	-@erase ".\WinRel\genskel.obj"
	-@erase ".\WinRel\genstub.obj"
	-@erase ".\WinRel\io.obj"
	-@erase ".\WinRel\isl2python.obj"
	-@erase ".\WinRel\manifest.obj"
	-@erase ".\WinRel\name.obj"
	-@erase ".\WinRel\prttree.obj"
	-@erase ".\WinRel\util.obj"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /YX /c
# ADD CPP /nologo /W3 /GX /O2 /I "$(ILUSRC)\stubbers\parser" /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /YX /c
CPP_PROJ=/nologo /MD /W3 /GX /O2 /I "$(ILUSRC)\stubbers\parser" /I "$(ILUSRC)\runtime\python" /D "WIN32" /D\
 "NDEBUG" /D "_CONSOLE" /Fp"$(INTDIR)/python-stubber.pch" /YX /Fo"$(INTDIR)/" /c\
 
CPP_OBJS=.\WinRel/
CPP_SBRS=.\.
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/python-stubber.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /machine:I386
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib\
 advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib\
 odbccp32.lib /nologo /subsystem:console /incremental:no\
 /pdb:"$(OUTDIR)/python-stubber.pdb" /machine:I386\
 /out:"$(ILUHOME)/bin/python-stubber.exe" 
LINK32_OBJS= \
	$(ILUHOME)\lib\parser32.lib \
	".\WinRel\genskel.obj" \
	".\WinRel\genstub.obj" \
	".\WinRel\io.obj" \
	".\WinRel\isl2python.obj" \
	".\WinRel\manifest.obj" \
	".\WinRel\name.obj" \
	".\WinRel\prttree.obj" \
	".\WinRel\util.obj"

"$(ILUHOME)\bin\python-stubber.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
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

ALL : "$(ILUHOME)\bin\python-stubber.exe"

CLEAN : 
	-@erase ".\WinDebug\genskel.obj"
	-@erase ".\WinDebug\genstub.obj"
	-@erase ".\WinDebug\io.obj"
	-@erase ".\WinDebug\isl2python.obj"
	-@erase ".\WinDebug\manifest.obj"
	-@erase ".\WinDebug\name.obj"
	-@erase ".\WinDebug\prttree.obj"
	-@erase ".\WinDebug\python-stubber.pdb"
	-@erase ".\WinDebug\util.obj"
	-@erase ".\WinDebug\vc40.idb"
	-@erase ".\WinDebug\vc40.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /YX /c
# ADD CPP /nologo /MD /W3 /Gm /GX /Zi /Od /I "$(ILUSRC)\stubbers\parser" /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /YX /c
CPP_PROJ=/nologo /MD /W3 /Gm /GX /Zi /Od /I "$(ILUSRC)\stubbers\parser"  /I "$(ILUSRC)\runtime\python" /D "WIN32"\
 /D "_DEBUG" /D "_CONSOLE" /Fp"$(INTDIR)/python-stubber.pch" /YX /Fo"$(INTDIR)/"\
 /Fd"$(INTDIR)/" /c 
CPP_OBJS=.\WinDebug/
CPP_SBRS=.\.
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/python-stubber.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /debug /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /debug /machine:I386
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib\
 advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib\
 odbccp32.lib /nologo /subsystem:console /incremental:yes\
 /pdb:"$(OUTDIR)/python-stubber.pdb" /debug /machine:I386\
 /out:"$(ILUHOME)/bin/python-stubber.exe" 
LINK32_OBJS= \
	$(ILUHOME)\lib\parser32.lib \
	".\WinDebug\genskel.obj" \
	".\WinDebug\genstub.obj" \
	".\WinDebug\io.obj" \
	".\WinDebug\isl2python.obj" \
	".\WinDebug\manifest.obj" \
	".\WinDebug\name.obj" \
	".\WinDebug\prttree.obj" \
	".\WinDebug\util.obj"

"$(ILUHOME)\bin\python-stubber.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
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

SOURCE=.\util.c

!IF  "$(CFG)" == "Win32 Release"


".\WinRel\util.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_UTIL_=\
	".\manifest.h"\
	".\name.h"\
	".\util.h"\
	
NODEP_CPP_UTIL_=\
	".\iluptype.h"\
	

".\WinDebug\util.obj" : $(SOURCE) $(DEP_CPP_UTIL_) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\prttree.c
DEP_CPP_PRTTR=\
	".\manifest.h"\
	".\prttree.h"\
	".\util.h"\
	
NODEP_CPP_PRTTR=\
	".\iluptype.h"\
	

!IF  "$(CFG)" == "Win32 Release"


".\WinRel\prttree.obj" : $(SOURCE) $(DEP_CPP_PRTTR) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "Win32 Debug"


".\WinDebug\prttree.obj" : $(SOURCE) $(DEP_CPP_PRTTR) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\name.c
DEP_CPP_NAME_=\
	".\manifest.h"\
	".\name.h"\
	".\util.h"\
	
NODEP_CPP_NAME_=\
	".\iluptype.h"\
	

!IF  "$(CFG)" == "Win32 Release"


".\WinRel\name.obj" : $(SOURCE) $(DEP_CPP_NAME_) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "Win32 Debug"


".\WinDebug\name.obj" : $(SOURCE) $(DEP_CPP_NAME_) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\manifest.c
DEP_CPP_MANIF=\
	".\manifest.h"\
	

!IF  "$(CFG)" == "Win32 Release"


".\WinRel\manifest.obj" : $(SOURCE) $(DEP_CPP_MANIF) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "Win32 Debug"


".\WinDebug\manifest.obj" : $(SOURCE) $(DEP_CPP_MANIF) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\isl2python.c
DEP_CPP_ISL2P=\
	".\genskel.h"\
	".\genstub.h"\
	".\manifest.h"\
	".\name.h"\
	".\prttree.h"\
	".\util.h"\
	
NODEP_CPP_ISL2P=\
	".\iluptype.h"\
	

!IF  "$(CFG)" == "Win32 Release"


".\WinRel\isl2python.obj" : $(SOURCE) $(DEP_CPP_ISL2P) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "Win32 Debug"


".\WinDebug\isl2python.obj" : $(SOURCE) $(DEP_CPP_ISL2P) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\io.c
DEP_CPP_IO_Ca=\
	".\io.h"\
	".\manifest.h"\
	".\util.h"\
	
NODEP_CPP_IO_Ca=\
	".\iluptype.h"\
	

!IF  "$(CFG)" == "Win32 Release"


".\WinRel\io.obj" : $(SOURCE) $(DEP_CPP_IO_Ca) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "Win32 Debug"


".\WinDebug\io.obj" : $(SOURCE) $(DEP_CPP_IO_Ca) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\genstub.c
DEP_CPP_GENST=\
	".\genstub.h"\
	".\io.h"\
	".\manifest.h"\
	".\name.h"\
	".\util.h"\
	
NODEP_CPP_GENST=\
	".\iluptype.h"\
	$(ILUSRC)\runtime\python\pythonversion.h\
	

!IF  "$(CFG)" == "Win32 Release"


".\WinRel\genstub.obj" : $(SOURCE) $(DEP_CPP_GENST) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "Win32 Debug"


".\WinDebug\genstub.obj" : $(SOURCE) $(DEP_CPP_GENST) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\genskel.c
DEP_CPP_GENSK=\
	".\genskel.h"\
	".\io.h"\
	".\manifest.h"\
	".\name.h"\
	".\util.h"\
	
NODEP_CPP_GENSK=\
	".\iluptype.h"\
	

!IF  "$(CFG)" == "Win32 Release"


".\WinRel\genskel.obj" : $(SOURCE) $(DEP_CPP_GENSK) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "Win32 Debug"


".\WinDebug\genskel.obj" : $(SOURCE) $(DEP_CPP_GENSK) "$(INTDIR)"


!ENDIF 

# End Source File
# End Target
# End Project
################################################################################
