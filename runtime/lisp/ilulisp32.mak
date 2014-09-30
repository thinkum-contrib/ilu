# Microsoft Developer Studio Generated NMAKE File, Format Version 4.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Dynamic-Link Library" 0x0102

!IF "$(CFG)" == ""
CFG=Win32 Debug
!MESSAGE No configuration specified.  Defaulting to Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "Win32 Release" && "$(CFG)" != "Win32 Debug" && "$(CFG)" !=\
 "BoundsChecker"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE on this makefile
!MESSAGE by defining the macro CFG on the command line.  For example:
!MESSAGE 
!MESSAGE NMAKE /f "ilulisp32.mak" CFG="Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "Win32 Release" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE "Win32 Debug" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE "BoundsChecker" (based on "Win32 (x86) Dynamic-Link Library")
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

!IF  "$(CFG)" == "Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "WinRel"
# PROP BASE Intermediate_Dir "WinRel"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "$(ILUHOME)\lib"
# PROP Intermediate_Dir "WinRel"
# PROP Target_Dir ""
OUTDIR=$(ILUHOME)\lib
INTDIR=.\WinRel

ALL : "$(ILUHOME)\bin\ilulisp.dll"

CLEAN : 
	-@erase ".\WinRel\ilu-franz-win-skin.obj"
	-@erase ".\WinRel\ilu-lisp-skin.obj"
	-@erase ".\WinRel\ilulisp.exp"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"
	
"$(INTDIR)" :
    if not exist "$(INTDIR)/$(NULL)" mkdir "$(INTDIR)"
	
CPP=cl.exe
# ADD BASE CPP /nologo /MD /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /c
# ADD CPP /nologo /MD /W3 /GX /O2 /I "$(ILUSRC)\runtime\kernel" /I "$(ILUSRC)\GSS\kernel" /D "NDEBUG" /D "WIN32" /D "_WINDOWS" /D "ILU_BUILDING_KERNEL" /YX /c
# Note we used to use the /Ox /Oy- optimization, but starting with MCVS5 and ilu2.0alpha9
# the global optimization started producing bogus code
CPP_PROJ=/nologo /MD /W3 /GX /Ob1 /Oi /Ot /Gs /YX /I "$(ILUSRC)\runtime\lisp" /I "$(ILUSRC)\runtime\kernel" /I "$(ILUSRC)\GSS\kernel" /D "NDEBUG" /D\
 "WIN32" /D "_WINDOWS" /D "ILU_BUILDING_RUNTIME" /Fp"$(INTDIR)/ilulisp.pch"\
 /Fo"$(INTDIR)/" /c 
CPP_OBJS=.\WinRel/
CPP_SBRS=

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

MTL=mktyplib.exe
# ADD BASE MTL /nologo /D "NDEBUG" /win32
# ADD MTL /nologo /D "NDEBUG" /win32
MTL_PROJ=/nologo /D "NDEBUG" /win32 
RSC=rc.exe
# ADD BASE RSC /l 0x1009 /d "NDEBUG"
# ADD RSC /l 0x1009 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/ilulisp.bsc" 
BSC32_SBRS=
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /machine:I386
# ADD LINK32 j:\ilu\lib\winio32w.lib wsock32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /machine:I386 /nodefaultlib:"l"
LINK32_FLAGS=$(ILUHOME)\lib\ilu32.lib wsock32.lib\
 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib\
 shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo\
 /subsystem:windows /dll\
 /incremental:no /pdb:"$(OUTDIR)/ilulisp.pdb" /machine:I386\
 /out:"$(ILUHOME)/bin/ilulisp.dll" /implib:"$(ILUHOME)/lib/ilulisp.lib" 
LINK32_OBJS= \
	"$(INTDIR)/ilu-franz-win-skin.obj" \
	"$(INTDIR)/ilu-lisp-skin.obj"

"$(ILUHOME)\bin\ilulisp.dll" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
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
# PROP Output_Dir "$(ILUHOME)\lib"
# PROP Intermediate_Dir "WinDebug"
# PROP Target_Dir ""
OUTDIR=$(ILUHOME)\lib
INTDIR=.\WinDebug

# ALL : "$(ILUHOME)\bin\ilulisp.dll" "$(OUTDIR)\ilulisp.bsc"

ALL : "$(ILUHOME)\bin\ilulisp.dll"

CLEAN : 
	-@erase ".\WinDebug\vc40.pdb"
	-@erase ".\WinDebug\vc40.idb"
	-@erase ".\WinDebug\ilulisp.bsc"
	-@erase ".\WinDebug\ilu-lisp-skin.sbr"
	-@erase ".\WinDebug\ilu-franz-win-skin.sbr"
	-@erase ".\WinDebug\ilu-franz-win-skin.obj"
	-@erase ".\WinDebug\ilu-lisp-skin.obj"
	-@erase ".\WinDebug\ilulisp.exp"
	-@erase ".\WinDebug\ilulisp.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"
	
"$(INTDIR)" :
    if not exist "$(INTDIR)/$(NULL)" mkdir "$(INTDIR)"
	
CPP=cl.exe
# ADD BASE CPP /nologo /MD /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /YX /c
# ADD CPP /nologo /MD /W3 /Gm /GX /Zi /Od /I "$(ILUSRC)\runtime\kernel" /I "$(ILUSRC)\GSS\kernel" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "ILU_BUILDING_KERNEL" /c
CPP_PROJ=/nologo /MD /W3 /Gm /GX /Zi /Od /YX /I "$(ILUSRC)\runtime\lisp" /I "$(ILUSRC)\runtime\kernel" /I "$(ILUSRC)\GSS\kernel" /D\
 "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "ILU_BUILDING_RUNTIME" /FR"$(INTDIR)/"\
 /Fp"$(INTDIR)/ilulisp.pch" /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c 
CPP_OBJS=.\WinDebug/
CPP_SBRS=.\WinDebug/

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

MTL=mktyplib.exe
# ADD BASE MTL /nologo /D "_DEBUG" /win32
# ADD MTL /nologo /D "_DEBUG" /win32
MTL_PROJ=/nologo /D "_DEBUG" /win32 
RSC=rc.exe
# ADD BASE RSC /l 0x1009 /d "_DEBUG"
# ADD RSC /l 0x1009 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/ilulisp.bsc" 
BSC32_SBRS= \
	"$(INTDIR)/ilu-lisp-skin.sbr" \
	"$(INTDIR)/ilu-franz-win-skin.sbr"

"$(OUTDIR)\ilulisp.bsc" : "$(OUTDIR)" $(BSC32_SBRS)
    $(BSC32) @<<
  $(BSC32_FLAGS) $(BSC32_SBRS)
<<

LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /debug /machine:I386
# ADD LINK32 j:\ilu\lib\winio32w.lib wsock32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /incremental:no /debug /machine:I386 /nodefaultlib:"libcmtd.lib"
LINK32_FLAGS=$(ILUHOME)\lib\ilu32.lib wsock32.lib kernel32.lib user32.lib\
 gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib\
 oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll\
 /incremental:no /pdb:"$(OUTDIR)/ilulisp.pdb" /debug /machine:I386\
 /out:"$(ILUHOME)/bin/ilulisp.dll"\
 /implib:"$(ILUHOME)/lib/ilulisp.lib" 
LINK32_OBJS= \
	"$(INTDIR)/ilu-franz-win-skin.obj" \
	"$(INTDIR)/ilu-lisp-skin.obj"

"$(ILUHOME)\bin\ilulisp.dll" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "BoundsChecker"

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
OUTDIR=.\BCPro
INTDIR=.\BCPro

# ALL : "$(ILUHOME)\bin\ilulisp.dll" "$(OUTDIR)\ilulisp.bsc"

ALL : "$(ILUHOME)\bin\ilulisp.dll"

CLEAN : 
	-@erase ".\BCPro\vc40.pdb"
	-@erase ".\BCPro\vc40.idb"
	-@erase ".\BCPro\ilulisp.bsc"
	-@erase ".\BCPro\ilu-lisp-skin.sbr"
	-@erase ".\BCPro\ilu-franz-win-skin.sbr"
	-@erase ".\BCPro\ilulisp.dll"
	-@erase ".\BCPro\ilu-franz-win-skin.obj"
	-@erase ".\BCPro\ilu-lisp-skin.obj"
	-@erase ".\BCPro\ilulisp.lib"
	-@erase ".\BCPro\ilulisp.exp"
	-@erase ".\BCPro\ilulisp.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=C:\BCProNT\BCOMPILE.EXE
# ADD BASE CPP /nologo /MD /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /YX /c
# ADD CPP /nologo /MD /W3 /Gm /GX /Zi /Od /I "$(ILUSRC)\runtime\kernel" /I "$(ILUSRC)\GSS\kernel" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "ILU_BUILDING_KERNEL" /FR /YX /c
CPP_PROJ=/nologo /MD /W3 /Gm /GX /Zi /Od /YX /I "$(ILUSRC)\runtime\kernel" /I "$(ILUSRC)\GSS\kernel" /D\
 "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "ILU_BUILDING_RUNTIME" /FR"$(INTDIR)/"\
 /Fp"$(INTDIR)/ilulisp.pch" /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c 
CPP_OBJS=.\BCPro/
CPP_SBRS=.\BCPro/

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

MTL=mktyplib.exe
# ADD BASE MTL /nologo /D "_DEBUG" /win32
# ADD MTL /nologo /D "_DEBUG" /win32
MTL_PROJ=/nologo /D "_DEBUG" /win32 
RSC=rc.exe
# ADD BASE RSC /l 0x1009 /d "_DEBUG"
# ADD RSC /l 0x1009 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/ilulisp.bsc" 
BSC32_SBRS= \
	"$(INTDIR)/ilu-lisp-skin.sbr" \
	"$(INTDIR)/ilu-franz-win-skin.sbr"

"$(OUTDIR)\ilulisp.bsc" : "$(OUTDIR)" $(BSC32_SBRS)
    $(BSC32) @<<
  $(BSC32_FLAGS) $(BSC32_SBRS)
<<

LINK32=C:\BCProNT\BCLINK.EXE
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /debug /machine:I386
# ADD LINK32 j:\ilu\lib\winio32w.lib wsock32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /incremental:no /debug /machine:I386 /nodefaultlib:"libcmtd.lib"
LINK32_FLAGS= $(ILUHOME)\lib\ilu32.lib wsock32.lib kernel32.lib user32.lib\
 gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib\
 oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll\
 /incremental:no /pdb:"$(OUTDIR)/ilulisp.pdb" /debug /machine:I386\
 /out:"$(ILUHOME)/bin/ilulisp.dll"\
 /implib:"$(ILUHOME)/lib/ilulisp.lib" 
LINK32_OBJS= \
	"$(INTDIR)/ilu-franz-win-skin.obj" \
	"$(INTDIR)/ilu-lisp-skin.obj"

"$(ILUHOME)\bin\ilulisp.dll" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ENDIF 

################################################################################
# Begin Target

# Name "Win32 Release"
# Name "Win32 Debug"

!IF  "$(CFG)" == "Win32 Release"

!ELSEIF  "$(CFG)" == "Win32 Debug"

!ENDIF 

################################################################################
# Begin Source File

SOURCE=".\ilu-lisp-skin.c"

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_ILU_L=\
	{$(INCLUDE)}"\sys\TYPES.H"\
	"$(ILUSRC)\runtime\kernel\iluxport.h"\
	"$(ILUSRC)\runtime\kernel\version.h"\
	"$(ILUSRC)\runtime\kernel\iluwin.h"\
	"$(ILUSRC)\runtime\kernel\iluerror.h"\
	"$(ILUSRC)\runtime\kernel\ilubasic.h"\
	"$(ILUSRC)\runtime\kernel\iluerrs.h"\
	

"$(INTDIR)\ilu-lisp-skin.obj" : $(SOURCE) $(DEP_CPP_ILU_L) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_ILU_L=\
	{$(INCLUDE)}"\sys\TYPES.H"\
	"$(ILUSRC)\runtime\kernel\iluxport.h"\
	"$(ILUSRC)\runtime\kernel\version.h"\
	

"$(INTDIR)\ilu-lisp-skin.obj" : $(SOURCE) $(DEP_CPP_ILU_L) "$(INTDIR)"

"$(INTDIR)\ilu-lisp-skin.sbr" : $(SOURCE) $(DEP_CPP_ILU_L) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=".\ilu-franz-win-skin.c"

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_ILU_F=\
	{$(INCLUDE)}"\sys\TYPES.H"\
	"$(ILUSRC)\runtime\kernel\iluxport.h"\
	"$(ILUSRC)\runtime\kernel\iluwin.h"\
	"$(ILUSRC)\runtime\kernel\iluerror.h"\
	"$(ILUSRC)\runtime\kernel\ilubasic.h"\
	"$(ILUSRC)\runtime\kernel\iluerrs.h"\
	

"$(INTDIR)\ilu-franz-win-skin.obj" : $(SOURCE) $(DEP_CPP_ILU_F) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_ILU_F=\
	{$(INCLUDE)}"\sys\TYPES.H"\
	"$(ILUSRC)\runtime\kernel\iluxport.h"\
	

"$(INTDIR)\ilu-franz-win-skin.obj" : $(SOURCE) $(DEP_CPP_ILU_F) "$(INTDIR)"

"$(INTDIR)\ilu-franz-win-skin.sbr" : $(SOURCE) $(DEP_CPP_ILU_F) "$(INTDIR)"


!ENDIF 

# End Source File
# End Target
# End Project
################################################################################
