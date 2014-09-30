# Microsoft Developer Studio Generated NMAKE File, Format Version 4.20
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
!MESSAGE NMAKE /f "iluPr.mak" CFG="Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "Win32 Release" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE "Win32 Debug" (based on "Win32 (x86) Dynamic-Link Library")
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
RSC=rc.exe
MTL=mktyplib.exe
CPP=cl.exe

!IF  "$(CFG)" == "Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "$(ILUHOME)\lib"
# PROP Intermediate_Dir "WinRel"
# PROP Target_Dir ""
OUTDIR=$(ILUHOME)\lib
INTDIR=.\WinRel

ALL : "$(OUTDIR)\iluPr.pyd"

CLEAN : 
	-@erase "$(INTDIR)\ilualobject.obj"
	-@erase "$(INTDIR)\ilucaobject.obj"
	-@erase "$(INTDIR)\iluclobject.obj"
	-@erase "$(INTDIR)\iluftobject.obj"
	-@erase "$(INTDIR)\ilulpobject.obj"
	-@erase "$(INTDIR)\iluszobject.obj"
	-@erase "$(INTDIR)\iluplobject.obj"
	-@erase "$(INTDIR)\iluppobject.obj"
	-@erase "$(INTDIR)\ilulrobject.obj"
	-@erase "$(INTDIR)\iluPrmodule.obj"
	-@erase "$(INTDIR)\ilupygss.obj"
	-@erase "$(INTDIR)\ilugiobject.obj"
	-@erase "$(INTDIR)\ilusvobject.obj"
	-@erase "$(INTDIR)\ilutpobject.obj"
	-@erase "$(INTDIR)\iohcobject.obj"
	-@erase "$(INTDIR)\ivobject.obj"
	-@erase "$(INTDIR)\thcobject.obj"
	-@erase "$(OUTDIR)\iluPr.exp"
	-@erase "$(OUTDIR)\iluPr.lib"
	-@erase "$(OUTDIR)\iluPr.pyd"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

"$(INTDIR)" :
    if not exist "$(INTDIR)/$(NULL)" mkdir "$(INTDIR)"

# ADD BASE CPP /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /c
# ADD CPP /nologo /MD /W3 /GX /O2 /I "$(PYTHONSRC)\PC" /I "$(PYTHONSRC)\include" /I "$(ILUSRC)\runtime\kernel" /I "$(ILUSRC)\GSS\kernel" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /c /Tp
# SUBTRACT CPP /X
CPP_PROJ=/nologo /MD /W3 /GX /O2 /I "$(PYTHONSRC)\PC" /I\
 "$(PYTHONSRC)\include" /I "$(ILUSRC)\runtime\kernel" /I "$(ILUSRC)\GSS\kernel" /D\
 "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "ILU_BUILDING_RUNTIME" /Fp"$(INTDIR)/iluPr.pch" /YX /Fo"$(INTDIR)/"\
 /c /Tp 
CPP_OBJS=.\WinRel/
CPP_SBRS=.\.
# ADD BASE MTL /nologo /D "NDEBUG" /win32
# ADD MTL /nologo /D "NDEBUG" /win32
MTL_PROJ=/nologo /D "NDEBUG" /win32 
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/iluPr.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /machine:I386
# ADD LINK32 $(PYTHONSRC)\vc40\python14.lib $(ILUHOME)\lib\ilu32.lib $(ILUHOME)\lib\gss.lib wsock32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /machine:I386 /out:"$(ILUHOME)\lib/iluPr.pyd" /export:initiluPr
# SUBTRACT LINK32 /pdb:none
LINK32_FLAGS=$(PYTHONSRC)\vc40\python14.lib\
 $(ILUHOME)\lib\ilu32.lib $(ILUHOME)\lib\gss.lib wsock32.lib kernel32.lib user32.lib gdi32.lib\
 winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib\
 uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll\
 /incremental:no /pdb:"$(OUTDIR)/iluPr.pdb" /machine:I386\
 /out:"$(OUTDIR)/iluPr.pyd" /implib:"$(OUTDIR)/iluPr.lib" /export:initiluPr \
 /export:ilupython_GetPerThreadDataTech /export:ilupython_ForkNewThread 
LINK32_OBJS= \
	"$(INTDIR)\ilualobject.obj" \
	"$(INTDIR)\ilucaobject.obj" \
	"$(INTDIR)\iluclobject.obj" \
	"$(INTDIR)\iluftobject.obj" \
	"$(INTDIR)\ilulpobject.obj" \
	"$(INTDIR)\iluszobject.obj" \
	"$(INTDIR)\ilulrobject.obj" \
	"$(INTDIR)\iluPrmodule.obj" \
	"$(INTDIR)\ilupygss.obj" \
	"$(INTDIR)\ilugiobject.obj"\
	"$(INTDIR)\iluplobject.obj"\
	"$(INTDIR)\iluppobject.obj"\
	"$(INTDIR)\ilusvobject.obj" \
	"$(INTDIR)\ilutpobject.obj" \
	"$(INTDIR)\iohcobject.obj" \
	"$(INTDIR)\ivobject.obj" \
	"$(INTDIR)\thcobject.obj"

"$(OUTDIR)\iluPr.pyd" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
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
# PROP Output_Dir "$(ILUHOME)\lib"
# PROP Intermediate_Dir "WinDebug"
# PROP Target_Dir ""
OUTDIR=$(ILUHOME)\lib
INTDIR=.\WinDebug

ALL : "$(OUTDIR)\iluPr.pyd"

CLEAN : 
	-@erase "$(INTDIR)\ilualobject.obj"
	-@erase "$(INTDIR)\ilucaobject.obj"
	-@erase "$(INTDIR)\iluclobject.obj"
	-@erase "$(INTDIR)\iluftobject.obj"
	-@erase "$(INTDIR)\ilulpobject.obj"
	-@erase "$(INTDIR)\iluszobject.obj"
	-@erase "$(INTDIR)\iluplobject.obj"
	-@erase "$(INTDIR)\iluppobject.obj"
	-@erase "$(INTDIR)\ilulrobject.obj"
	-@erase "$(INTDIR)\ilufpobject.obj"
	-@erase "$(INTDIR)\iluPrmodule.obj"
	-@erase "$(INTDIR)\ilupygss.obj"
	-@erase "$(INTDIR)\ilugiobject.obj"
	-@erase "$(INTDIR)\ilusvobject.obj"
	-@erase "$(INTDIR)\ilutpobject.obj"
	-@erase "$(INTDIR)\iohcobject.obj"
	-@erase "$(INTDIR)\ivobject.obj"
	-@erase "$(INTDIR)\thcobject.obj"
	-@erase "$(INTDIR)\vc40.idb"
	-@erase "$(INTDIR)\vc40.pdb"
	-@erase "$(OUTDIR)\iluPr.exp"
	-@erase "$(OUTDIR)\iluPr.ilk"
	-@erase "$(OUTDIR)\iluPr.lib"
	-@erase "$(OUTDIR)\iluPr.pdb"
	-@erase "$(OUTDIR)\iluPr.pyd"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

"$(INTDIR)" :
    if not exist "$(INTDIR)/$(NULL)" mkdir "$(INTDIR)"

# ADD BASE CPP /nologo /MTd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /YX /c
# ADD CPP /nologo /MD /W3 /Gm /GX /Zi /Od /I "$(PYTHONSRC)\PC" /I "$(PYTHONSRC)\include" /I "$(ILUSRC)\runtime\kernel" /I "$(ILUSRC)\GSS\kernel" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /YX /c /Tp
# SUBTRACT CPP /X
CPP_PROJ=/nologo /MD /W3 /Gm /GX /Zi /Od /I "$(PYTHONSRC)\PC" /I\
 "$(PYTHONSRC)\include" /I "$(ILUSRC)\runtime\kernel" /I "$(ILUSRC)\GSS\kernel" /D\
 "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "ILU_BUILDING_RUNTIME" /Fp"$(INTDIR)/iluPr.pch" /YX /Fo"$(INTDIR)/"\
 /Fd"$(INTDIR)/" /c /Tp 
CPP_OBJS=.\WinDebug/
CPP_SBRS=.\.
# ADD BASE MTL /nologo /D "_DEBUG" /win32
# ADD MTL /nologo /D "_DEBUG" /win32
MTL_PROJ=/nologo /D "_DEBUG" /win32 
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/iluPr.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /debug /machine:I386
# ADD LINK32 $(PYTHONSRC)\vc40\python14.lib $(ILUHOME)\lib\ilu32.lib $(ILUHOME)\lib\gss.lib wsock32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /debug /machine:I386 /out:"$(ILUHOME)\lib/iluPr.pyd" /export:initiluPr
# SUBTRACT LINK32 /pdb:none
LINK32_FLAGS=$(PYTHONSRC)\vc40\python14.lib\
 $(ILUHOME)\lib\ilu32.lib $(ILUHOME)\lib\gss.lib wsock32.lib kernel32.lib user32.lib gdi32.lib\
 winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib\
 uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll\
 /incremental:yes /pdb:"$(OUTDIR)/iluPr.pdb" /debug /machine:I386\
 /out:"$(OUTDIR)/iluPr.pyd" /implib:"$(OUTDIR)/iluPr.lib" /export:initiluPr \
 /export:ilupython_GetPerThreadDataTech /export:ilupython_ForkNewThread 
LINK32_OBJS= \
	"$(INTDIR)\ilualobject.obj" \
	"$(INTDIR)\ilucaobject.obj" \
	"$(INTDIR)\iluclobject.obj" \
	"$(INTDIR)\iluftobject.obj" \
	"$(INTDIR)\ilulpobject.obj" \
	"$(INTDIR)\iluszobject.obj" \
	"$(INTDIR)\ilulrobject.obj" \
	"$(INTDIR)\iluPrmodule.obj" \
	"$(INTDIR)\ilupygss.obj" \
	"$(INTDIR)\ilugiobject.obj"\
	"$(INTDIR)\iluplobject.obj"\
	"$(INTDIR)\iluppobject.obj"\
	"$(INTDIR)\ilusvobject.obj" \
	"$(INTDIR)\ilutpobject.obj" \
	"$(INTDIR)\iohcobject.obj" \
	"$(INTDIR)\ivobject.obj" \
	"$(INTDIR)\thcobject.obj"

"$(OUTDIR)\iluPr.pyd" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
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

SOURCE=.\ilulpobject.c
DEP_CPP_ILULP=\
	"$(PYTHONSRC)\include\abstract.h"\
	"$(PYTHONSRC)\include\accessobject.h"\
	"$(PYTHONSRC)\include\bltinmodule.h"\
	"$(PYTHONSRC)\include\ceval.h"\
	"$(PYTHONSRC)\include\classobject.h"\
	"$(PYTHONSRC)\include\cobject.h"\
	"$(PYTHONSRC)\include\complexobject.h"\
	"$(PYTHONSRC)\include\fileobject.h"\
	"$(PYTHONSRC)\include\floatobject.h"\
	"$(PYTHONSRC)\include\funcobject.h"\
	"$(PYTHONSRC)\include\import.h"\
	"$(PYTHONSRC)\include\intobject.h"\
	"$(PYTHONSRC)\include\listobject.h"\
	"$(PYTHONSRC)\include\longobject.h"\
	"$(PYTHONSRC)\include\mappingobject.h"\
	"$(PYTHONSRC)\include\methodobject.h"\
	"$(PYTHONSRC)\include\modsupport.h"\
	"$(PYTHONSRC)\include\moduleobject.h"\
	"$(PYTHONSRC)\include\mymalloc.h"\
	"$(PYTHONSRC)\include\myproto.h"\
	"$(PYTHONSRC)\include\object.h"\
	"$(PYTHONSRC)\include\objimpl.h"\
	"$(PYTHONSRC)\include\pydebug.h"\
	"$(PYTHONSRC)\include\pyerrors.h"\
	"$(PYTHONSRC)\include\rangeobject.h"\
	"$(PYTHONSRC)\include\rename2.h"\
	"$(PYTHONSRC)\include\sliceobject.h"\
	"$(PYTHONSRC)\include\stringobject.h"\
	"$(PYTHONSRC)\include\tupleobject.h"\
	".\ilulpobject.h"\
	".\python.h"\
	".\pythonversion.h"\
	"$(ILUSRC)\runtime\kernel\ilubasic.h"\
	"$(ILUSRC)\runtime\kernel\iluerror.h"\
	"$(ILUSRC)\runtime\kernel\iluerrs.h"\
	"$(ILUSRC)\runtime\kernel\ilutypes.h"\
	"$(ILUSRC)\runtime\kernel\iluwin.h"\
	"$(ILUSRC)\runtime\kernel\iluxport.h"\
	"$(PYTHONSRC)\include\allobjects.h"\
	"$(PYTHONSRC)\include\intrcheck.h"\
	"$(PYTHONSRC)\include\Python.h"\
	"$(PYTHONSRC)\include\pythonrun.h"\
	"$(PYTHONSRC)\include\sysmodule.h"\
	"$(PYTHONSRC)\include\thread.h"\
	"$(PYTHONSRC)\include\traceback.h"\
	"$(PYTHONSRC)\PC\config.h"\
	
NODEP_CPP_ILULP=\
	".\rename1.h"\
	

"$(INTDIR)\ilulpobject.obj" : $(SOURCE) $(DEP_CPP_ILULP) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\iluszobject.c
DEP_CPP_ILUSZ=\
	"$(PYTHONSRC)\include\abstract.h"\
	"$(PYTHONSRC)\include\accessobject.h"\
	"$(PYTHONSRC)\include\bltinmodule.h"\
	"$(PYTHONSRC)\include\ceval.h"\
	"$(PYTHONSRC)\include\classobject.h"\
	"$(PYTHONSRC)\include\cobject.h"\
	"$(PYTHONSRC)\include\complexobject.h"\
	"$(PYTHONSRC)\include\fileobject.h"\
	"$(PYTHONSRC)\include\floatobject.h"\
	"$(PYTHONSRC)\include\funcobject.h"\
	"$(PYTHONSRC)\include\import.h"\
	"$(PYTHONSRC)\include\intobject.h"\
	"$(PYTHONSRC)\include\listobject.h"\
	"$(PYTHONSRC)\include\longobject.h"\
	"$(PYTHONSRC)\include\mappingobject.h"\
	"$(PYTHONSRC)\include\methodobject.h"\
	"$(PYTHONSRC)\include\modsupport.h"\
	"$(PYTHONSRC)\include\moduleobject.h"\
	"$(PYTHONSRC)\include\mymalloc.h"\
	"$(PYTHONSRC)\include\myproto.h"\
	"$(PYTHONSRC)\include\object.h"\
	"$(PYTHONSRC)\include\objimpl.h"\
	"$(PYTHONSRC)\include\pydebug.h"\
	"$(PYTHONSRC)\include\pyerrors.h"\
	"$(PYTHONSRC)\include\rangeobject.h"\
	"$(PYTHONSRC)\include\rename2.h"\
	"$(PYTHONSRC)\include\sliceobject.h"\
	"$(PYTHONSRC)\include\stringobject.h"\
	"$(PYTHONSRC)\include\tupleobject.h"\
	".\iluszobject.h"\
	".\python.h"\
	".\pythonversion.h"\
	"$(ILUSRC)\runtime\kernel\ilubasic.h"\
	"$(ILUSRC)\runtime\kernel\iluerror.h"\
	"$(ILUSRC)\runtime\kernel\iluerrs.h"\
	"$(ILUSRC)\runtime\kernel\ilutypes.h"\
	"$(ILUSRC)\runtime\kernel\iluwin.h"\
	"$(ILUSRC)\runtime\kernel\iluxport.h"\
	"$(PYTHONSRC)\include\allobjects.h"\
	"$(PYTHONSRC)\include\intrcheck.h"\
	"$(PYTHONSRC)\include\Python.h"\
	"$(PYTHONSRC)\include\pythonrun.h"\
	"$(PYTHONSRC)\include\sysmodule.h"\
	"$(PYTHONSRC)\include\thread.h"\
	"$(PYTHONSRC)\include\traceback.h"\
	"$(PYTHONSRC)\PC\config.h"\
	
NODEP_CPP_ILUSZ=\
	".\rename1.h"\
	

"$(INTDIR)\iluszobject.obj" : $(SOURCE) $(DEP_CPP_ILUSZ) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\ilucaobject.c
DEP_CPP_ILUCA=\
	"$(PYTHONSRC)\include\abstract.h"\
	"$(PYTHONSRC)\include\accessobject.h"\
	"$(PYTHONSRC)\include\bltinmodule.h"\
	"$(PYTHONSRC)\include\ceval.h"\
	"$(PYTHONSRC)\include\classobject.h"\
	"$(PYTHONSRC)\include\cobject.h"\
	"$(PYTHONSRC)\include\complexobject.h"\
	"$(PYTHONSRC)\include\fileobject.h"\
	"$(PYTHONSRC)\include\floatobject.h"\
	"$(PYTHONSRC)\include\funcobject.h"\
	"$(PYTHONSRC)\include\import.h"\
	"$(PYTHONSRC)\include\intobject.h"\
	"$(PYTHONSRC)\include\listobject.h"\
	"$(PYTHONSRC)\include\longobject.h"\
	"$(PYTHONSRC)\include\mappingobject.h"\
	"$(PYTHONSRC)\include\methodobject.h"\
	"$(PYTHONSRC)\include\modsupport.h"\
	"$(PYTHONSRC)\include\moduleobject.h"\
	"$(PYTHONSRC)\include\mymalloc.h"\
	"$(PYTHONSRC)\include\myproto.h"\
	"$(PYTHONSRC)\include\object.h"\
	"$(PYTHONSRC)\include\objimpl.h"\
	"$(PYTHONSRC)\include\pydebug.h"\
	"$(PYTHONSRC)\include\pyerrors.h"\
	"$(PYTHONSRC)\include\rangeobject.h"\
	"$(PYTHONSRC)\include\rename2.h"\
	"$(PYTHONSRC)\include\sliceobject.h"\
	"$(PYTHONSRC)\include\stringobject.h"\
	"$(PYTHONSRC)\include\tupleobject.h"\
	".\ilucaobject.h"\
	".\python.h"\
	".\pythonversion.h"\
	"$(ILUSRC)\runtime\kernel\ilubasic.h"\
	"$(ILUSRC)\runtime\kernel\iluerror.h"\
	"$(ILUSRC)\runtime\kernel\iluerrs.h"\
	"$(ILUSRC)\runtime\kernel\ilutypes.h"\
	"$(ILUSRC)\runtime\kernel\iluwin.h"\
	"$(ILUSRC)\runtime\kernel\iluxport.h"\
	"$(PYTHONSRC)\include\allobjects.h"\
	"$(PYTHONSRC)\include\intrcheck.h"\
	"$(PYTHONSRC)\include\Python.h"\
	"$(PYTHONSRC)\include\pythonrun.h"\
	"$(PYTHONSRC)\include\sysmodule.h"\
	"$(PYTHONSRC)\include\thread.h"\
	"$(PYTHONSRC)\include\traceback.h"\
	"$(PYTHONSRC)\PC\config.h"\
	
NODEP_CPP_ILUCA=\
	".\rename1.h"\
	

"$(INTDIR)\ilucaobject.obj" : $(SOURCE) $(DEP_CPP_ILUCA) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\iluclobject.c
DEP_CPP_ILUCL=\
	"$(PYTHONSRC)\include\abstract.h"\
	"$(PYTHONSRC)\include\accessobject.h"\
	"$(PYTHONSRC)\include\bltinmodule.h"\
	"$(PYTHONSRC)\include\ceval.h"\
	"$(PYTHONSRC)\include\classobject.h"\
	"$(PYTHONSRC)\include\cobject.h"\
	"$(PYTHONSRC)\include\complexobject.h"\
	"$(PYTHONSRC)\include\fileobject.h"\
	"$(PYTHONSRC)\include\floatobject.h"\
	"$(PYTHONSRC)\include\funcobject.h"\
	"$(PYTHONSRC)\include\import.h"\
	"$(PYTHONSRC)\include\intobject.h"\
	"$(PYTHONSRC)\include\listobject.h"\
	"$(PYTHONSRC)\include\longobject.h"\
	"$(PYTHONSRC)\include\mappingobject.h"\
	"$(PYTHONSRC)\include\methodobject.h"\
	"$(PYTHONSRC)\include\modsupport.h"\
	"$(PYTHONSRC)\include\moduleobject.h"\
	"$(PYTHONSRC)\include\mymalloc.h"\
	"$(PYTHONSRC)\include\myproto.h"\
	"$(PYTHONSRC)\include\object.h"\
	"$(PYTHONSRC)\include\objimpl.h"\
	"$(PYTHONSRC)\include\pydebug.h"\
	"$(PYTHONSRC)\include\pyerrors.h"\
	"$(PYTHONSRC)\include\rangeobject.h"\
	"$(PYTHONSRC)\include\rename2.h"\
	"$(PYTHONSRC)\include\sliceobject.h"\
	"$(PYTHONSRC)\include\stringobject.h"\
	"$(PYTHONSRC)\include\tupleobject.h"\
	".\iluclobject.h"\
	".\python.h"\
	".\pythonversion.h"\
	"$(ILUSRC)\runtime\kernel\ilubasic.h"\
	"$(ILUSRC)\runtime\kernel\iluerror.h"\
	"$(ILUSRC)\runtime\kernel\iluerrs.h"\
	"$(ILUSRC)\runtime\kernel\ilutypes.h"\
	"$(ILUSRC)\runtime\kernel\iluwin.h"\
	"$(ILUSRC)\runtime\kernel\iluxport.h"\
	"$(PYTHONSRC)\include\allobjects.h"\
	"$(PYTHONSRC)\include\intrcheck.h"\
	"$(PYTHONSRC)\include\Python.h"\
	"$(PYTHONSRC)\include\pythonrun.h"\
	"$(PYTHONSRC)\include\sysmodule.h"\
	"$(PYTHONSRC)\include\thread.h"\
	"$(PYTHONSRC)\include\traceback.h"\
	"$(PYTHONSRC)\PC\config.h"\
	
NODEP_CPP_ILUCL=\
	".\rename1.h"\
	

"$(INTDIR)\iluclobject.obj" : $(SOURCE) $(DEP_CPP_ILUCL) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\iluftobject.c
DEP_CPP_ILUFT=\
	"$(PYTHONSRC)\include\abstract.h"\
	"$(PYTHONSRC)\include\accessobject.h"\
	"$(PYTHONSRC)\include\bltinmodule.h"\
	"$(PYTHONSRC)\include\ceval.h"\
	"$(PYTHONSRC)\include\classobject.h"\
	"$(PYTHONSRC)\include\cobject.h"\
	"$(PYTHONSRC)\include\complexobject.h"\
	"$(PYTHONSRC)\include\fileobject.h"\
	"$(PYTHONSRC)\include\floatobject.h"\
	"$(PYTHONSRC)\include\funcobject.h"\
	"$(PYTHONSRC)\include\import.h"\
	"$(PYTHONSRC)\include\intobject.h"\
	"$(PYTHONSRC)\include\listobject.h"\
	"$(PYTHONSRC)\include\longobject.h"\
	"$(PYTHONSRC)\include\mappingobject.h"\
	"$(PYTHONSRC)\include\methodobject.h"\
	"$(PYTHONSRC)\include\modsupport.h"\
	"$(PYTHONSRC)\include\moduleobject.h"\
	"$(PYTHONSRC)\include\mymalloc.h"\
	"$(PYTHONSRC)\include\myproto.h"\
	"$(PYTHONSRC)\include\object.h"\
	"$(PYTHONSRC)\include\objimpl.h"\
	"$(PYTHONSRC)\include\pydebug.h"\
	"$(PYTHONSRC)\include\pyerrors.h"\
	"$(PYTHONSRC)\include\rangeobject.h"\
	"$(PYTHONSRC)\include\rename2.h"\
	"$(PYTHONSRC)\include\sliceobject.h"\
	"$(PYTHONSRC)\include\stringobject.h"\
	"$(PYTHONSRC)\include\tupleobject.h"\
	".\iluftobject.h"\
	".\python.h"\
	".\pythonversion.h"\
	"$(ILUSRC)\runtime\kernel\ilubasic.h"\
	"$(ILUSRC)\runtime\kernel\iluerror.h"\
	"$(ILUSRC)\runtime\kernel\iluerrs.h"\
	"$(ILUSRC)\runtime\kernel\ilutypes.h"\
	"$(ILUSRC)\runtime\kernel\iluwin.h"\
	"$(ILUSRC)\runtime\kernel\iluxport.h"\
	"$(PYTHONSRC)\include\allobjects.h"\
	"$(PYTHONSRC)\include\intrcheck.h"\
	"$(PYTHONSRC)\include\Python.h"\
	"$(PYTHONSRC)\include\pythonrun.h"\
	"$(PYTHONSRC)\include\sysmodule.h"\
	"$(PYTHONSRC)\include\thread.h"\
	"$(PYTHONSRC)\include\traceback.h"\
	"$(PYTHONSRC)\PC\config.h"\
	
NODEP_CPP_ILUFT=\
	".\rename1.h"\
	

"$(INTDIR)\iluftobject.obj" : $(SOURCE) $(DEP_CPP_ILUFT) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\ilualobject.c
DEP_CPP_ILUAL=\
	"$(PYTHONSRC)\include\abstract.h"\
	"$(PYTHONSRC)\include\accessobject.h"\
	"$(PYTHONSRC)\include\bltinmodule.h"\
	"$(PYTHONSRC)\include\ceval.h"\
	"$(PYTHONSRC)\include\classobject.h"\
	"$(PYTHONSRC)\include\cobject.h"\
	"$(PYTHONSRC)\include\complexobject.h"\
	"$(PYTHONSRC)\include\fileobject.h"\
	"$(PYTHONSRC)\include\floatobject.h"\
	"$(PYTHONSRC)\include\funcobject.h"\
	"$(PYTHONSRC)\include\import.h"\
	"$(PYTHONSRC)\include\intobject.h"\
	"$(PYTHONSRC)\include\listobject.h"\
	"$(PYTHONSRC)\include\longobject.h"\
	"$(PYTHONSRC)\include\mappingobject.h"\
	"$(PYTHONSRC)\include\methodobject.h"\
	"$(PYTHONSRC)\include\modsupport.h"\
	"$(PYTHONSRC)\include\moduleobject.h"\
	"$(PYTHONSRC)\include\mymalloc.h"\
	"$(PYTHONSRC)\include\myproto.h"\
	"$(PYTHONSRC)\include\object.h"\
	"$(PYTHONSRC)\include\objimpl.h"\
	"$(PYTHONSRC)\include\pydebug.h"\
	"$(PYTHONSRC)\include\pyerrors.h"\
	"$(PYTHONSRC)\include\rangeobject.h"\
	"$(PYTHONSRC)\include\rename2.h"\
	"$(PYTHONSRC)\include\sliceobject.h"\
	"$(PYTHONSRC)\include\stringobject.h"\
	"$(PYTHONSRC)\include\tupleobject.h"\
	".\ilualobject.h"\
	".\iluftobject.h"\
	".\python.h"\
	".\pythonthreads.h"\
	".\pythonversion.h"\
	"$(ILUSRC)\runtime\kernel\ilubasic.h"\
	"$(ILUSRC)\runtime\kernel\iluerror.h"\
	"$(ILUSRC)\runtime\kernel\iluerrs.h"\
	"$(ILUSRC)\runtime\kernel\ilutypes.h"\
	"$(ILUSRC)\runtime\kernel\iluwin.h"\
	"$(ILUSRC)\runtime\kernel\iluxport.h"\
	"$(PYTHONSRC)\include\allobjects.h"\
	"$(PYTHONSRC)\include\intrcheck.h"\
	"$(PYTHONSRC)\include\Python.h"\
	"$(PYTHONSRC)\include\pythonrun.h"\
	"$(PYTHONSRC)\include\sysmodule.h"\
	"$(PYTHONSRC)\include\thread.h"\
	"$(PYTHONSRC)\include\traceback.h"\
	"$(PYTHONSRC)\PC\config.h"\
	
NODEP_CPP_ILUAL=\
	".\rename1.h"\
	

"$(INTDIR)\ilualobject.obj" : $(SOURCE) $(DEP_CPP_ILUAL) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\ilulrobject.c
DEP_CPP_ILULR=\
	"$(PYTHONSRC)\include\abstract.h"\
	"$(PYTHONSRC)\include\accessobject.h"\
	"$(PYTHONSRC)\include\bltinmodule.h"\
	"$(PYTHONSRC)\include\ceval.h"\
	"$(PYTHONSRC)\include\classobject.h"\
	"$(PYTHONSRC)\include\cobject.h"\
	"$(PYTHONSRC)\include\complexobject.h"\
	"$(PYTHONSRC)\include\fileobject.h"\
	"$(PYTHONSRC)\include\floatobject.h"\
	"$(PYTHONSRC)\include\funcobject.h"\
	"$(PYTHONSRC)\include\import.h"\
	"$(PYTHONSRC)\include\intobject.h"\
	"$(PYTHONSRC)\include\listobject.h"\
	"$(PYTHONSRC)\include\longobject.h"\
	"$(PYTHONSRC)\include\mappingobject.h"\
	"$(PYTHONSRC)\include\methodobject.h"\
	"$(PYTHONSRC)\include\modsupport.h"\
	"$(PYTHONSRC)\include\moduleobject.h"\
	"$(PYTHONSRC)\include\mymalloc.h"\
	"$(PYTHONSRC)\include\myproto.h"\
	"$(PYTHONSRC)\include\object.h"\
	"$(PYTHONSRC)\include\objimpl.h"\
	"$(PYTHONSRC)\include\pydebug.h"\
	"$(PYTHONSRC)\include\pyerrors.h"\
	"$(PYTHONSRC)\include\rangeobject.h"\
	"$(PYTHONSRC)\include\rename2.h"\
	"$(PYTHONSRC)\include\sliceobject.h"\
	"$(PYTHONSRC)\include\stringobject.h"\
	"$(PYTHONSRC)\include\tupleobject.h"\
	".\ilulrobject.h"\
	".\python.h"\
	".\pythonversion.h"\
	"$(ILUSRC)\runtime\kernel\ilubasic.h"\
	"$(ILUSRC)\runtime\kernel\iluerror.h"\
	"$(ILUSRC)\runtime\kernel\iluerrs.h"\
	"$(ILUSRC)\runtime\kernel\ilutypes.h"\
	"$(ILUSRC)\runtime\kernel\iluwin.h"\
	"$(ILUSRC)\runtime\kernel\iluxport.h"\
	"$(PYTHONSRC)\include\allobjects.h"\
	"$(PYTHONSRC)\include\intrcheck.h"\
	"$(PYTHONSRC)\include\Python.h"\
	"$(PYTHONSRC)\include\pythonrun.h"\
	"$(PYTHONSRC)\include\sysmodule.h"\
	"$(PYTHONSRC)\include\thread.h"\
	"$(PYTHONSRC)\include\traceback.h"\
	"$(PYTHONSRC)\PC\config.h"\
	
NODEP_CPP_ILULR=\
	".\rename1.h"\
	

"$(INTDIR)\ilulrobject.obj" : $(SOURCE) $(DEP_CPP_ILULR) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\iluPrmodule.c
DEP_CPP_ILUPR=\
	"$(PYTHONSRC)\include\abstract.h"\
	"$(PYTHONSRC)\include\accessobject.h"\
	"$(PYTHONSRC)\include\bltinmodule.h"\
	"$(PYTHONSRC)\include\ceval.h"\
	"$(PYTHONSRC)\include\classobject.h"\
	"$(PYTHONSRC)\include\cobject.h"\
	"$(PYTHONSRC)\include\complexobject.h"\
	"$(PYTHONSRC)\include\fileobject.h"\
	"$(PYTHONSRC)\include\floatobject.h"\
	"$(PYTHONSRC)\include\funcobject.h"\
	"$(PYTHONSRC)\include\import.h"\
	"$(PYTHONSRC)\include\intobject.h"\
	"$(PYTHONSRC)\include\listobject.h"\
	"$(PYTHONSRC)\include\longobject.h"\
	"$(PYTHONSRC)\include\mappingobject.h"\
	"$(PYTHONSRC)\include\methodobject.h"\
	"$(PYTHONSRC)\include\modsupport.h"\
	"$(PYTHONSRC)\include\moduleobject.h"\
	"$(PYTHONSRC)\include\mymalloc.h"\
	"$(PYTHONSRC)\include\myproto.h"\
	"$(PYTHONSRC)\include\object.h"\
	"$(PYTHONSRC)\include\objimpl.h"\
	"$(PYTHONSRC)\include\pydebug.h"\
	"$(PYTHONSRC)\include\pyerrors.h"\
	"$(PYTHONSRC)\include\rangeobject.h"\
	"$(PYTHONSRC)\include\rename2.h"\
	"$(PYTHONSRC)\include\sliceobject.h"\
	"$(PYTHONSRC)\include\stringobject.h"\
	"$(PYTHONSRC)\include\tupleobject.h"\
	"$(ILUSRC)\runtime\kernel\os\posix.h"\
	"$(ILUSRC)\runtime\kernel\os\win.h"\
	".\ilualobject.h"\
	".\ilucaobject.h"\
	".\iluclobject.h"\
	".\iluftobject.h"\
	".\ilugiobject.h"\
	".\ilulpobject.h"\
	".\iluszobject.h"\
	".\iluplobject.h"\
	".\iluppobject.h"\
	".\ilulrobject.h"\
	".\ilusvobject.h"\
	".\ilutpobject.h"\
	".\iohcobject.h"\
	".\ivobject.h"\
	".\python.h"\
	".\pythonthreads.h"\
	".\pythonversion.h"\
	".\thcobject.h"\
	"$(ILUSRC)\runtime\kernel\ilubasic.h"\
	"$(ILUSRC)\runtime\kernel\iluerror.h"\
	"$(ILUSRC)\runtime\kernel\iluerrs.h"\
	"$(ILUSRC)\runtime\kernel\ilutypes.h"\
	"$(ILUSRC)\runtime\kernel\iluwin.h"\
	"$(ILUSRC)\runtime\kernel\iluxport.h"\
	"$(ILUSRC)\runtime\kernel\oscalls.h"\
	"$(PYTHONSRC)\include\allobjects.h"\
	"$(PYTHONSRC)\include\intrcheck.h"\
	"$(PYTHONSRC)\include\Python.h"\
	"$(PYTHONSRC)\include\pythonrun.h"\
	"$(PYTHONSRC)\include\sysmodule.h"\
	"$(PYTHONSRC)\include\thread.h"\
	"$(PYTHONSRC)\include\traceback.h"\
	"$(PYTHONSRC)\PC\config.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	
NODEP_CPP_ILUPR=\
	".\rename1.h"\
	

"$(INTDIR)\iluPrmodule.obj" : $(SOURCE) $(DEP_CPP_ILUPR) "$(INTDIR)"


# End Source File
################################################################################



################################################################################
# Begin Source File

SOURCE=.\ilusvobject.c
DEP_CPP_ILUSV=\
	"$(PYTHONSRC)\include\abstract.h"\
	"$(PYTHONSRC)\include\accessobject.h"\
	"$(PYTHONSRC)\include\bltinmodule.h"\
	"$(PYTHONSRC)\include\ceval.h"\
	"$(PYTHONSRC)\include\classobject.h"\
	"$(PYTHONSRC)\include\cobject.h"\
	"$(PYTHONSRC)\include\complexobject.h"\
	"$(PYTHONSRC)\include\fileobject.h"\
	"$(PYTHONSRC)\include\floatobject.h"\
	"$(PYTHONSRC)\include\funcobject.h"\
	"$(PYTHONSRC)\include\import.h"\
	"$(PYTHONSRC)\include\intobject.h"\
	"$(PYTHONSRC)\include\listobject.h"\
	"$(PYTHONSRC)\include\longobject.h"\
	"$(PYTHONSRC)\include\mappingobject.h"\
	"$(PYTHONSRC)\include\methodobject.h"\
	"$(PYTHONSRC)\include\modsupport.h"\
	"$(PYTHONSRC)\include\moduleobject.h"\
	"$(PYTHONSRC)\include\mymalloc.h"\
	"$(PYTHONSRC)\include\myproto.h"\
	"$(PYTHONSRC)\include\object.h"\
	"$(PYTHONSRC)\include\objimpl.h"\
	"$(PYTHONSRC)\include\pydebug.h"\
	"$(PYTHONSRC)\include\pyerrors.h"\
	"$(PYTHONSRC)\include\rangeobject.h"\
	"$(PYTHONSRC)\include\rename2.h"\
	"$(PYTHONSRC)\include\sliceobject.h"\
	"$(PYTHONSRC)\include\stringobject.h"\
	"$(PYTHONSRC)\include\tupleobject.h"\
	".\ilusvobject.h"\
	".\iluszobject.h"\
	".\python.h"\
	".\pythonversion.h"\
	"$(ILUSRC)\runtime\kernel\ilubasic.h"\
	"$(ILUSRC)\runtime\kernel\iluerror.h"\
	"$(ILUSRC)\runtime\kernel\iluerrs.h"\
	"$(ILUSRC)\runtime\kernel\ilutypes.h"\
	"$(ILUSRC)\runtime\kernel\iluwin.h"\
	"$(ILUSRC)\runtime\kernel\iluxport.h"\
	"$(PYTHONSRC)\include\allobjects.h"\
	"$(PYTHONSRC)\include\intrcheck.h"\
	"$(PYTHONSRC)\include\Python.h"\
	"$(PYTHONSRC)\include\pythonrun.h"\
	"$(PYTHONSRC)\include\sysmodule.h"\
	"$(PYTHONSRC)\include\thread.h"\
	"$(PYTHONSRC)\include\traceback.h"\
	"$(PYTHONSRC)\PC\config.h"\
	
NODEP_CPP_ILUSV=\
	".\rename1.h"\
	

"$(INTDIR)\ilusvobject.obj" : $(SOURCE) $(DEP_CPP_ILUSV) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\ilupygss.c
DEP_CPP_ILUPY=\
	"$(ILUSRC)\gss\kernel\gssapi.h"\
	"$(ILUSRC)\gss\kernel\ilugss_conf.h"\
	"$(ILUSRC)\runtime\kernel\ilubasic.h"\
	"$(ILUSRC)\runtime\kernel\iluconf.h"\
	"$(ILUSRC)\runtime\kernel\iluerror.h"\
	"$(ILUSRC)\runtime\kernel\iluerrs.h"\
	"$(ILUSRC)\runtime\kernel\iluhash.h"\
	"$(ILUSRC)\runtime\kernel\ilumac.h"\
	"$(ILUSRC)\runtime\kernel\ilutpcod.h"\
	"$(ILUSRC)\runtime\kernel\ilutypes.h"\
	"$(ILUSRC)\runtime\kernel\iluwin.h"\
	"$(ILUSRC)\runtime\kernel\iluxport.h"\
	"$(ILUSRC)\runtime\python\ilupygss.h"\
	"$(ILUSRC)\runtime\python\python.h"\
	"$(ILUSRC)\runtime\python\pythonversion.h"\
	"$(PYTHONSRC)\include\abstract.h"\
	"$(PYTHONSRC)\include\accessobject.h"\
	"$(PYTHONSRC)\include\allobjects.h"\
	"$(PYTHONSRC)\include\bltinmodule.h"\
	"$(PYTHONSRC)\include\ceval.h"\
	"$(PYTHONSRC)\include\classobject.h"\
	"$(PYTHONSRC)\include\cobject.h"\
	"$(PYTHONSRC)\include\complexobject.h"\
	"$(PYTHONSRC)\include\fileobject.h"\
	"$(PYTHONSRC)\include\floatobject.h"\
	"$(PYTHONSRC)\include\funcobject.h"\
	"$(PYTHONSRC)\include\import.h"\
	"$(PYTHONSRC)\include\intobject.h"\
	"$(PYTHONSRC)\include\intrcheck.h"\
	"$(PYTHONSRC)\include\listobject.h"\
	"$(PYTHONSRC)\include\longobject.h"\
	"$(PYTHONSRC)\include\mappingobject.h"\
	"$(PYTHONSRC)\include\methodobject.h"\
	"$(PYTHONSRC)\include\modsupport.h"\
	"$(PYTHONSRC)\include\moduleobject.h"\
	"$(PYTHONSRC)\include\mymalloc.h"\
	"$(PYTHONSRC)\include\myproto.h"\
	"$(PYTHONSRC)\include\object.h"\
	"$(PYTHONSRC)\include\objimpl.h"\
	"$(PYTHONSRC)\include\pydebug.h"\
	"$(PYTHONSRC)\include\pyerrors.h"\
	"$(PYTHONSRC)\include\python.h"\
	"$(PYTHONSRC)\include\pythonrun.h"\
	"$(PYTHONSRC)\include\rangeobject.h"\
	"$(PYTHONSRC)\include\rename2.h"\
	"$(PYTHONSRC)\include\sliceobject.h"\
	"$(PYTHONSRC)\include\stringobject.h"\
	"$(PYTHONSRC)\include\sysmodule.h"\
	"$(PYTHONSRC)\include\thread.h"\
	"$(PYTHONSRC)\include\traceback.h"\
	"$(PYTHONSRC)\include\tupleobject.h"\
	"$(PYTHONSRC)\pc\config.h"\
	
NODEP_CPP_ILUPY=\
	"$(ILUSRC)\runtime\python\rename1.h"\
	

"$(INTDIR)\ilupygss.obj" : $(SOURCE) $(DEP_CPP_ILUPY) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\ilutpobject.c
DEP_CPP_ILUTP=\
	"$(PYTHONSRC)\include\abstract.h"\
	"$(PYTHONSRC)\include\accessobject.h"\
	"$(PYTHONSRC)\include\bltinmodule.h"\
	"$(PYTHONSRC)\include\ceval.h"\
	"$(PYTHONSRC)\include\classobject.h"\
	"$(PYTHONSRC)\include\cobject.h"\
	"$(PYTHONSRC)\include\complexobject.h"\
	"$(PYTHONSRC)\include\fileobject.h"\
	"$(PYTHONSRC)\include\floatobject.h"\
	"$(PYTHONSRC)\include\funcobject.h"\
	"$(PYTHONSRC)\include\import.h"\
	"$(PYTHONSRC)\include\intobject.h"\
	"$(PYTHONSRC)\include\listobject.h"\
	"$(PYTHONSRC)\include\longobject.h"\
	"$(PYTHONSRC)\include\mappingobject.h"\
	"$(PYTHONSRC)\include\methodobject.h"\
	"$(PYTHONSRC)\include\modsupport.h"\
	"$(PYTHONSRC)\include\moduleobject.h"\
	"$(PYTHONSRC)\include\mymalloc.h"\
	"$(PYTHONSRC)\include\myproto.h"\
	"$(PYTHONSRC)\include\object.h"\
	"$(PYTHONSRC)\include\objimpl.h"\
	"$(PYTHONSRC)\include\pydebug.h"\
	"$(PYTHONSRC)\include\pyerrors.h"\
	"$(PYTHONSRC)\include\rangeobject.h"\
	"$(PYTHONSRC)\include\rename2.h"\
	"$(PYTHONSRC)\include\sliceobject.h"\
	"$(PYTHONSRC)\include\stringobject.h"\
	"$(PYTHONSRC)\include\tupleobject.h"\
	".\ilutpobject.h"\
	".\python.h"\
	".\pythonversion.h"\
	"$(ILUSRC)\runtime\kernel\ilubasic.h"\
	"$(ILUSRC)\runtime\kernel\iluerror.h"\
	"$(ILUSRC)\runtime\kernel\iluerrs.h"\
	"$(ILUSRC)\runtime\kernel\ilutypes.h"\
	"$(ILUSRC)\runtime\kernel\iluwin.h"\
	"$(ILUSRC)\runtime\kernel\iluxport.h"\
	"$(PYTHONSRC)\include\allobjects.h"\
	"$(PYTHONSRC)\include\intrcheck.h"\
	"$(PYTHONSRC)\include\Python.h"\
	"$(PYTHONSRC)\include\pythonrun.h"\
	"$(PYTHONSRC)\include\sysmodule.h"\
	"$(PYTHONSRC)\include\thread.h"\
	"$(PYTHONSRC)\include\traceback.h"\
	"$(PYTHONSRC)\PC\config.h"\
	
NODEP_CPP_ILUTP=\
	".\rename1.h"\
	

"$(INTDIR)\ilutpobject.obj" : $(SOURCE) $(DEP_CPP_ILUTP) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\iohcobject.c
DEP_CPP_IOHCO=\
	"$(PYTHONSRC)\include\abstract.h"\
	"$(PYTHONSRC)\include\accessobject.h"\
	"$(PYTHONSRC)\include\bltinmodule.h"\
	"$(PYTHONSRC)\include\ceval.h"\
	"$(PYTHONSRC)\include\classobject.h"\
	"$(PYTHONSRC)\include\cobject.h"\
	"$(PYTHONSRC)\include\complexobject.h"\
	"$(PYTHONSRC)\include\fileobject.h"\
	"$(PYTHONSRC)\include\floatobject.h"\
	"$(PYTHONSRC)\include\funcobject.h"\
	"$(PYTHONSRC)\include\import.h"\
	"$(PYTHONSRC)\include\intobject.h"\
	"$(PYTHONSRC)\include\listobject.h"\
	"$(PYTHONSRC)\include\longobject.h"\
	"$(PYTHONSRC)\include\mappingobject.h"\
	"$(PYTHONSRC)\include\methodobject.h"\
	"$(PYTHONSRC)\include\modsupport.h"\
	"$(PYTHONSRC)\include\moduleobject.h"\
	"$(PYTHONSRC)\include\mymalloc.h"\
	"$(PYTHONSRC)\include\myproto.h"\
	"$(PYTHONSRC)\include\object.h"\
	"$(PYTHONSRC)\include\objimpl.h"\
	"$(PYTHONSRC)\include\pydebug.h"\
	"$(PYTHONSRC)\include\pyerrors.h"\
	"$(PYTHONSRC)\include\rangeobject.h"\
	"$(PYTHONSRC)\include\rename2.h"\
	"$(PYTHONSRC)\include\sliceobject.h"\
	"$(PYTHONSRC)\include\stringobject.h"\
	"$(PYTHONSRC)\include\tupleobject.h"\
	".\iohcobject.h"\
	".\python.h"\
	".\pythonversion.h"\
	"$(ILUSRC)\runtime\kernel\ilubasic.h"\
	"$(ILUSRC)\runtime\kernel\iluerror.h"\
	"$(ILUSRC)\runtime\kernel\iluerrs.h"\
	"$(ILUSRC)\runtime\kernel\ilutypes.h"\
	"$(ILUSRC)\runtime\kernel\iluwin.h"\
	"$(ILUSRC)\runtime\kernel\iluxport.h"\
	"$(PYTHONSRC)\include\allobjects.h"\
	"$(PYTHONSRC)\include\intrcheck.h"\
	"$(PYTHONSRC)\include\Python.h"\
	"$(PYTHONSRC)\include\pythonrun.h"\
	"$(PYTHONSRC)\include\sysmodule.h"\
	"$(PYTHONSRC)\include\thread.h"\
	"$(PYTHONSRC)\include\traceback.h"\
	"$(PYTHONSRC)\PC\config.h"\
	
NODEP_CPP_IOHCO=\
	".\rename1.h"\
	

"$(INTDIR)\iohcobject.obj" : $(SOURCE) $(DEP_CPP_IOHCO) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\ivobject.c
DEP_CPP_IVOBJ=\
	"$(PYTHONSRC)\include\abstract.h"\
	"$(PYTHONSRC)\include\accessobject.h"\
	"$(PYTHONSRC)\include\bltinmodule.h"\
	"$(PYTHONSRC)\include\ceval.h"\
	"$(PYTHONSRC)\include\classobject.h"\
	"$(PYTHONSRC)\include\cobject.h"\
	"$(PYTHONSRC)\include\complexobject.h"\
	"$(PYTHONSRC)\include\fileobject.h"\
	"$(PYTHONSRC)\include\floatobject.h"\
	"$(PYTHONSRC)\include\funcobject.h"\
	"$(PYTHONSRC)\include\import.h"\
	"$(PYTHONSRC)\include\intobject.h"\
	"$(PYTHONSRC)\include\listobject.h"\
	"$(PYTHONSRC)\include\longobject.h"\
	"$(PYTHONSRC)\include\mappingobject.h"\
	"$(PYTHONSRC)\include\methodobject.h"\
	"$(PYTHONSRC)\include\modsupport.h"\
	"$(PYTHONSRC)\include\moduleobject.h"\
	"$(PYTHONSRC)\include\mymalloc.h"\
	"$(PYTHONSRC)\include\myproto.h"\
	"$(PYTHONSRC)\include\object.h"\
	"$(PYTHONSRC)\include\objimpl.h"\
	"$(PYTHONSRC)\include\pydebug.h"\
	"$(PYTHONSRC)\include\pyerrors.h"\
	"$(PYTHONSRC)\include\rangeobject.h"\
	"$(PYTHONSRC)\include\rename2.h"\
	"$(PYTHONSRC)\include\sliceobject.h"\
	"$(PYTHONSRC)\include\stringobject.h"\
	"$(PYTHONSRC)\include\tupleobject.h"\
	".\ivobject.h"\
	".\python.h"\
	".\pythonthreads.h"\
	".\pythonversion.h"\
	"$(ILUSRC)\runtime\kernel\ilubasic.h"\
	"$(ILUSRC)\runtime\kernel\iluerror.h"\
	"$(ILUSRC)\runtime\kernel\iluerrs.h"\
	"$(ILUSRC)\runtime\kernel\ilutypes.h"\
	"$(ILUSRC)\runtime\kernel\iluwin.h"\
	"$(ILUSRC)\runtime\kernel\iluxport.h"\
	"$(PYTHONSRC)\include\allobjects.h"\
	"$(PYTHONSRC)\include\intrcheck.h"\
	"$(PYTHONSRC)\include\Python.h"\
	"$(PYTHONSRC)\include\pythonrun.h"\
	"$(PYTHONSRC)\include\sysmodule.h"\
	"$(PYTHONSRC)\include\thread.h"\
	"$(PYTHONSRC)\include\traceback.h"\
	"$(PYTHONSRC)\PC\config.h"\
	
NODEP_CPP_IVOBJ=\
	".\rename1.h"\
	

"$(INTDIR)\ivobject.obj" : $(SOURCE) $(DEP_CPP_IVOBJ) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\thcobject.c
DEP_CPP_THCOB=\
	"$(PYTHONSRC)\include\abstract.h"\
	"$(PYTHONSRC)\include\accessobject.h"\
	"$(PYTHONSRC)\include\bltinmodule.h"\
	"$(PYTHONSRC)\include\ceval.h"\
	"$(PYTHONSRC)\include\classobject.h"\
	"$(PYTHONSRC)\include\cobject.h"\
	"$(PYTHONSRC)\include\complexobject.h"\
	"$(PYTHONSRC)\include\fileobject.h"\
	"$(PYTHONSRC)\include\floatobject.h"\
	"$(PYTHONSRC)\include\funcobject.h"\
	"$(PYTHONSRC)\include\import.h"\
	"$(PYTHONSRC)\include\intobject.h"\
	"$(PYTHONSRC)\include\listobject.h"\
	"$(PYTHONSRC)\include\longobject.h"\
	"$(PYTHONSRC)\include\mappingobject.h"\
	"$(PYTHONSRC)\include\methodobject.h"\
	"$(PYTHONSRC)\include\modsupport.h"\
	"$(PYTHONSRC)\include\moduleobject.h"\
	"$(PYTHONSRC)\include\mymalloc.h"\
	"$(PYTHONSRC)\include\myproto.h"\
	"$(PYTHONSRC)\include\object.h"\
	"$(PYTHONSRC)\include\objimpl.h"\
	"$(PYTHONSRC)\include\pydebug.h"\
	"$(PYTHONSRC)\include\pyerrors.h"\
	"$(PYTHONSRC)\include\rangeobject.h"\
	"$(PYTHONSRC)\include\rename2.h"\
	"$(PYTHONSRC)\include\sliceobject.h"\
	"$(PYTHONSRC)\include\stringobject.h"\
	"$(PYTHONSRC)\include\tupleobject.h"\
	".\iluftobject.h"\
	".\python.h"\
	".\pythonversion.h"\
	".\thcobject.h"\
	"$(ILUSRC)\runtime\kernel\ilubasic.h"\
	"$(ILUSRC)\runtime\kernel\iluerror.h"\
	"$(ILUSRC)\runtime\kernel\iluerrs.h"\
	"$(ILUSRC)\runtime\kernel\ilutypes.h"\
	"$(ILUSRC)\runtime\kernel\iluwin.h"\
	"$(ILUSRC)\runtime\kernel\iluxport.h"\
	"$(PYTHONSRC)\include\allobjects.h"\
	"$(PYTHONSRC)\include\intrcheck.h"\
	"$(PYTHONSRC)\include\Python.h"\
	"$(PYTHONSRC)\include\pythonrun.h"\
	"$(PYTHONSRC)\include\sysmodule.h"\
	"$(PYTHONSRC)\include\thread.h"\
	"$(PYTHONSRC)\include\traceback.h"\
	"$(PYTHONSRC)\PC\config.h"\
	
NODEP_CPP_THCOB=\
	".\rename1.h"\
	

"$(INTDIR)\thcobject.obj" : $(SOURCE) $(DEP_CPP_THCOB) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=$(ILUSRC)\runtime\python\ilugiobject.c

DEP_CPP_ILUGI=\
	"$(ILUSRC)\gss\kernel\gssapi.h"\
	"$(ILUSRC)\gss\kernel\ilugss_conf.h"\
	"$(ILUSRC)\runtime\kernel\ilubasic.h"\
	"$(ILUSRC)\runtime\kernel\iluconf.h"\
	"$(ILUSRC)\runtime\kernel\iluerror.h"\
	"$(ILUSRC)\runtime\kernel\iluerrs.h"\
	"$(ILUSRC)\runtime\kernel\iluhash.h"\
	"$(ILUSRC)\runtime\kernel\ilumac.h"\
	"$(ILUSRC)\runtime\kernel\ilutpcod.h"\
	"$(ILUSRC)\runtime\kernel\ilutypes.h"\
	"$(ILUSRC)\runtime\kernel\iluwin.h"\
	"$(ILUSRC)\runtime\kernel\iluxport.h"\
	"$(ILUSRC)\runtime\python\ilugiobject.h"\
	"$(ILUSRC)\runtime\python\python.h"\
	"$(ILUSRC)\runtime\python\pythonversion.h"\
	"$(PYTHONSRC)\include\abstract.h"\
	"$(PYTHONSRC)\include\accessobject.h"\
	"$(PYTHONSRC)\include\allobjects.h"\
	"$(PYTHONSRC)\include\bltinmodule.h"\
	"$(PYTHONSRC)\include\ceval.h"\
	"$(PYTHONSRC)\include\classobject.h"\
	"$(PYTHONSRC)\include\cobject.h"\
	"$(PYTHONSRC)\include\complexobject.h"\
	"$(PYTHONSRC)\include\fileobject.h"\
	"$(PYTHONSRC)\include\floatobject.h"\
	"$(PYTHONSRC)\include\funcobject.h"\
	"$(PYTHONSRC)\include\import.h"\
	"$(PYTHONSRC)\include\intobject.h"\
	"$(PYTHONSRC)\include\intrcheck.h"\
	"$(PYTHONSRC)\include\listobject.h"\
	"$(PYTHONSRC)\include\longobject.h"\
	"$(PYTHONSRC)\include\mappingobject.h"\
	"$(PYTHONSRC)\include\methodobject.h"\
	"$(PYTHONSRC)\include\modsupport.h"\
	"$(PYTHONSRC)\include\moduleobject.h"\
	"$(PYTHONSRC)\include\mymalloc.h"\
	"$(PYTHONSRC)\include\myproto.h"\
	"$(PYTHONSRC)\include\object.h"\
	"$(PYTHONSRC)\include\objimpl.h"\
	"$(PYTHONSRC)\include\pydebug.h"\
	"$(PYTHONSRC)\include\pyerrors.h"\
	"$(PYTHONSRC)\include\python.h"\
	"$(PYTHONSRC)\include\pythonrun.h"\
	"$(PYTHONSRC)\include\rangeobject.h"\
	"$(PYTHONSRC)\include\rename2.h"\
	"$(PYTHONSRC)\include\sliceobject.h"\
	"$(PYTHONSRC)\include\stringobject.h"\
	"$(PYTHONSRC)\include\sysmodule.h"\
	"$(PYTHONSRC)\include\thread.h"\
	"$(PYTHONSRC)\include\traceback.h"\
	"$(PYTHONSRC)\include\tupleobject.h"\
	"$(PYTHONSRC)\pc\config.h"\
	
NODEP_CPP_ILUGI=\
	"$(ILUSRC)\runtime\python\rename1.h"\
	

"$(INTDIR)\ilugiobject.obj" : $(SOURCE) $(DEP_CPP_ILUGI) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)

# End Source File
################################################################################
# Begin Source File

SOURCE=$(ILUSRC)\runtime\python\iluplobject.c

DEP_CPP_ILUPL=\
	"$(ILUSRC)\gss\kernel\gssapi.h"\
	"$(ILUSRC)\gss\kernel\ilugss_conf.h"\
	"$(ILUSRC)\runtime\kernel\ilubasic.h"\
	"$(ILUSRC)\runtime\kernel\iluconf.h"\
	"$(ILUSRC)\runtime\kernel\iluerror.h"\
	"$(ILUSRC)\runtime\kernel\iluerrs.h"\
	"$(ILUSRC)\runtime\kernel\iluhash.h"\
	"$(ILUSRC)\runtime\kernel\ilumac.h"\
	"$(ILUSRC)\runtime\kernel\ilutpcod.h"\
	"$(ILUSRC)\runtime\kernel\ilutypes.h"\
	"$(ILUSRC)\runtime\kernel\iluwin.h"\
	"$(ILUSRC)\runtime\kernel\iluxport.h"\
	"$(ILUSRC)\runtime\python\iluplobject.h"\
	"$(ILUSRC)\runtime\python\python.h"\
	"$(ILUSRC)\runtime\python\pythonversion.h"\
	"$(PYTHONSRC)\include\abstract.h"\
	"$(PYTHONSRC)\include\accessobject.h"\
	"$(PYTHONSRC)\include\allobjects.h"\
	"$(PYTHONSRC)\include\bltinmodule.h"\
	"$(PYTHONSRC)\include\ceval.h"\
	"$(PYTHONSRC)\include\classobject.h"\
	"$(PYTHONSRC)\include\cobject.h"\
	"$(PYTHONSRC)\include\complexobject.h"\
	"$(PYTHONSRC)\include\fileobject.h"\
	"$(PYTHONSRC)\include\floatobject.h"\
	"$(PYTHONSRC)\include\funcobject.h"\
	"$(PYTHONSRC)\include\import.h"\
	"$(PYTHONSRC)\include\intobject.h"\
	"$(PYTHONSRC)\include\intrcheck.h"\
	"$(PYTHONSRC)\include\listobject.h"\
	"$(PYTHONSRC)\include\longobject.h"\
	"$(PYTHONSRC)\include\mappingobject.h"\
	"$(PYTHONSRC)\include\methodobject.h"\
	"$(PYTHONSRC)\include\modsupport.h"\
	"$(PYTHONSRC)\include\moduleobject.h"\
	"$(PYTHONSRC)\include\mymalloc.h"\
	"$(PYTHONSRC)\include\myproto.h"\
	"$(PYTHONSRC)\include\object.h"\
	"$(PYTHONSRC)\include\objimpl.h"\
	"$(PYTHONSRC)\include\pydebug.h"\
	"$(PYTHONSRC)\include\pyerrors.h"\
	"$(PYTHONSRC)\include\python.h"\
	"$(PYTHONSRC)\include\pythonrun.h"\
	"$(PYTHONSRC)\include\rangeobject.h"\
	"$(PYTHONSRC)\include\rename2.h"\
	"$(PYTHONSRC)\include\sliceobject.h"\
	"$(PYTHONSRC)\include\stringobject.h"\
	"$(PYTHONSRC)\include\sysmodule.h"\
	"$(PYTHONSRC)\include\thread.h"\
	"$(PYTHONSRC)\include\traceback.h"\
	"$(PYTHONSRC)\include\tupleobject.h"\
	"$(PYTHONSRC)\pc\config.h"\
	
NODEP_CPP_ILUPL=\
	"$(ILUSRC)\runtime\python\rename1.h"\
	

"$(INTDIR)\iluplobject.obj" : $(SOURCE) $(DEP_CPP_ILUPL) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=$(ILUSRC)\runtime\python\iluppobject.c

DEP_CPP_ILUPP=\
	"$(ILUSRC)\gss\kernel\gssapi.h"\
	"$(ILUSRC)\gss\kernel\ilugss_conf.h"\
	"$(ILUSRC)\runtime\kernel\ilubasic.h"\
	"$(ILUSRC)\runtime\kernel\iluconf.h"\
	"$(ILUSRC)\runtime\kernel\iluerror.h"\
	"$(ILUSRC)\runtime\kernel\iluerrs.h"\
	"$(ILUSRC)\runtime\kernel\iluhash.h"\
	"$(ILUSRC)\runtime\kernel\ilumac.h"\
	"$(ILUSRC)\runtime\kernel\ilutpcod.h"\
	"$(ILUSRC)\runtime\kernel\ilutypes.h"\
	"$(ILUSRC)\runtime\kernel\iluwin.h"\
	"$(ILUSRC)\runtime\kernel\iluxport.h"\
	"$(ILUSRC)\runtime\python\ilugiobject.h"\
	"$(ILUSRC)\runtime\python\iluppobject.h"\
	"$(ILUSRC)\runtime\python\ilupygss.h"\
	"$(ILUSRC)\runtime\python\python.h"\
	"$(ILUSRC)\runtime\python\pythonversion.h"\
	"$(PYTHONSRC)\include\abstract.h"\
	"$(PYTHONSRC)\include\accessobject.h"\
	"$(PYTHONSRC)\include\allobjects.h"\
	"$(PYTHONSRC)\include\bltinmodule.h"\
	"$(PYTHONSRC)\include\ceval.h"\
	"$(PYTHONSRC)\include\classobject.h"\
	"$(PYTHONSRC)\include\cobject.h"\
	"$(PYTHONSRC)\include\complexobject.h"\
	"$(PYTHONSRC)\include\fileobject.h"\
	"$(PYTHONSRC)\include\floatobject.h"\
	"$(PYTHONSRC)\include\funcobject.h"\
	"$(PYTHONSRC)\include\import.h"\
	"$(PYTHONSRC)\include\intobject.h"\
	"$(PYTHONSRC)\include\intrcheck.h"\
	"$(PYTHONSRC)\include\listobject.h"\
	"$(PYTHONSRC)\include\longobject.h"\
	"$(PYTHONSRC)\include\mappingobject.h"\
	"$(PYTHONSRC)\include\methodobject.h"\
	"$(PYTHONSRC)\include\modsupport.h"\
	"$(PYTHONSRC)\include\moduleobject.h"\
	"$(PYTHONSRC)\include\mymalloc.h"\
	"$(PYTHONSRC)\include\myproto.h"\
	"$(PYTHONSRC)\include\object.h"\
	"$(PYTHONSRC)\include\objimpl.h"\
	"$(PYTHONSRC)\include\pydebug.h"\
	"$(PYTHONSRC)\include\pyerrors.h"\
	"$(PYTHONSRC)\include\python.h"\
	"$(PYTHONSRC)\include\pythonrun.h"\
	"$(PYTHONSRC)\include\rangeobject.h"\
	"$(PYTHONSRC)\include\rename2.h"\
	"$(PYTHONSRC)\include\sliceobject.h"\
	"$(PYTHONSRC)\include\stringobject.h"\
	"$(PYTHONSRC)\include\sysmodule.h"\
	"$(PYTHONSRC)\include\thread.h"\
	"$(PYTHONSRC)\include\traceback.h"\
	"$(PYTHONSRC)\include\tupleobject.h"\
	"$(PYTHONSRC)\pc\config.h"\
	
NODEP_CPP_ILUPP=\
	"$(ILUSRC)\runtime\python\rename1.h"\
	

"$(INTDIR)\iluppobject.obj" : $(SOURCE) $(DEP_CPP_ILUPP) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# End Target
# End Project
################################################################################