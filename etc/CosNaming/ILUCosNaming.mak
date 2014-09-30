# Microsoft Developer Studio Generated NMAKE File, Based on ILUCOSNaming.dsp
!IF "$(CFG)" == ""
CFG=Win32 Debug
!MESSAGE No configuration specified. Defaulting to Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "Win32 Release" && "$(CFG)" !=\
 "Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "ILUCOSNaming.mak" CFG="Win32 Debug"
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

CPP=cl.exe
RSC=rc.exe

!IF  "$(CFG)" == "Win32 Release"

OUTDIR=.\WinRel
INTDIR=.\WinRel

!IF "$(RECURSE)" == "0" 

ALL : "$(ILUHOME)\bin\ILUCosNaming.exe"

!ELSE 

ALL : "$(ILUHOME)\bin\ILUCosNaming.exe"

!ENDIF 

CLEAN :
	-@erase "$(INTDIR)\CosNaming-common.obj"
	-@erase "$(INTDIR)\CosNaming-true.obj"
	-@erase "$(INTDIR)\NamingImpl.obj"
	-@erase "$(INTDIR)\vc50.idb"
	-@erase "$(ILUHOME)\bin\ILUCosNaming.exe"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /MD /W3 /GX /O2 /I "$(ILUSRC)\runtime\c" /I\
 "$(ILUSRC)\runtime\kernel" /I "$(ILUSRC)\GSS\kernel" /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS"\
 /Fp"$(INTDIR)\ILUCOSNaming.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
CPP_OBJS=.\WinRel/
CPP_SBRS=.
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\ILUCOSNaming.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=$(ILUHOME)\lib\iluc32.lib $(ILUHOME)\lib\ilu32.lib wsock32.lib\
 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib\
 shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo\
 /subsystem:console /incremental:no /pdb:"$(OUTDIR)\ILUCosNaming.pdb"\
 /machine:I386 /out:"$(ILUHOME)\bin\ILUCosNaming.exe" 
LINK32_OBJS= \
	"$(INTDIR)\CosNaming-common.obj" \
	"$(INTDIR)\CosNaming-true.obj" \
	"$(INTDIR)\NamingImpl.obj"

"$(ILUHOME)\bin\ILUCosNaming.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "Win32 Debug"

OUTDIR=.\WinDebug
INTDIR=.\WinDebug

!IF "$(RECURSE)" == "0" 

ALL : "$(ILUHOME)\bin\ILUCosNaming.exe"

!ELSE 

ALL : "$(ILUHOME)\bin\ILUCosNaming.exe"

!ENDIF 

CLEAN :
	-@erase "$(INTDIR)\CosNaming-common.obj"
	-@erase "$(INTDIR)\CosNaming-true.obj"
	-@erase "$(INTDIR)\NamingImpl.obj"
	-@erase "$(INTDIR)\vc50.idb"
	-@erase "$(INTDIR)\vc50.pdb"
	-@erase "$(OUTDIR)\ILUCosNaming.pdb"
	-@erase "$(ILUHOME)\bin\ILUCosNaming.exe"
	-@erase "$(ILUHOME)\bin\ILUCosNaming.ilk"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /MD /W3 /Gm /GX /Zi /Od /I "$(ILUSRC)\runtime\c" /I\
 "$(ILUSRC)\runtime\kernel" /I "$(ILUSRC)\GSS\kernel" /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS"\
 /Fp"$(INTDIR)\ILUCOSNaming.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
CPP_OBJS=.\WinDebug/
CPP_SBRS=.
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\ILUCOSNaming.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=$(ILUHOME)\lib\iluc32.lib $(ILUHOME)\lib\ilu32.lib  wsock32.lib\
 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib\
 shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo\
 /subsystem:console /incremental:yes /pdb:"$(OUTDIR)\ILUCosNaming.pdb" /debug\
 /machine:I386 /out:"$(ILUHOME)\bin\ILUCosNaming.exe" /pdbtype:sept 
LINK32_OBJS= \
	"$(INTDIR)\CosNaming-common.obj" \
	"$(INTDIR)\CosNaming-true.obj" \
	"$(INTDIR)\NamingImpl.obj"

"$(ILUHOME)\bin\ILUCosNaming.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ENDIF 

.c{$(CPP_OBJS)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(CPP_OBJS)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(CPP_OBJS)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.c{$(CPP_SBRS)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(CPP_SBRS)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(CPP_SBRS)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<


!IF "$(CFG)" == "Win32 Release" || "$(CFG)" ==\
 "Win32 Debug"
SOURCE=".\CosNaming-common.c"

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_COSNA=\
	"$(ILUSRC)\runtime\c\iluchdrs.h"\
	"$(ILUSRC)\runtime\c\ilucstub.h"\
	"$(ILUSRC)\runtime\kernel\ilubasic.h"\
	"$(ILUSRC)\runtime\kernel\iluerror.h"\
	"$(ILUSRC)\runtime\kernel\iluerrs.h"\
	"$(ILUSRC)\runtime\kernel\iluhash.h"\
	"$(ILUSRC)\runtime\kernel\ilutpcod.h"\
	"$(ILUSRC)\runtime\kernel\ilutypes.h"\
	"$(ILUSRC)\runtime\kernel\iluwin.h"\
	"$(ILUSRC)\runtime\kernel\iluxport.h"\
	".\cosnaming.h"\
	

"$(INTDIR)\CosNaming-common.obj" : $(SOURCE) $(DEP_CPP_COSNA) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_COSNA=\
	"$(ILUSRC)\runtime\c\iluchdrs.h"\
	"$(ILUSRC)\runtime\c\ilucstub.h"\
	"$(ILUSRC)\runtime\kernel\ilubasic.h"\
	"$(ILUSRC)\runtime\kernel\iluerror.h"\
	"$(ILUSRC)\runtime\kernel\iluerrs.h"\
	"$(ILUSRC)\runtime\kernel\iluhash.h"\
	"$(ILUSRC)\runtime\kernel\ilutpcod.h"\
	"$(ILUSRC)\runtime\kernel\ilutypes.h"\
	"$(ILUSRC)\runtime\kernel\iluwin.h"\
	"$(ILUSRC)\runtime\kernel\iluxport.h"\
	".\cosnaming.h"\
	

"$(INTDIR)\CosNaming-common.obj" : $(SOURCE) $(DEP_CPP_COSNA) "$(INTDIR)"


!ENDIF 

SOURCE=".\CosNaming-true.c"

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_COSNAM=\
	"$(ILUSRC)\runtime\c\iluchdrs.h"\
	"$(ILUSRC)\runtime\c\ilucstub.h"\
	"$(ILUSRC)\runtime\kernel\ilubasic.h"\
	"$(ILUSRC)\runtime\kernel\iluerror.h"\
	"$(ILUSRC)\runtime\kernel\iluerrs.h"\
	"$(ILUSRC)\runtime\kernel\iluhash.h"\
	"$(ILUSRC)\runtime\kernel\ilutpcod.h"\
	"$(ILUSRC)\runtime\kernel\ilutypes.h"\
	"$(ILUSRC)\runtime\kernel\iluwin.h"\
	"$(ILUSRC)\runtime\kernel\iluxport.h"\
	".\cosnaming.h"\
	

"$(INTDIR)\CosNaming-true.obj" : $(SOURCE) $(DEP_CPP_COSNAM) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_COSNAM=\
	"$(ILUSRC)\runtime\c\iluchdrs.h"\
	"$(ILUSRC)\runtime\c\ilucstub.h"\
	"$(ILUSRC)\runtime\kernel\ilubasic.h"\
	"$(ILUSRC)\runtime\kernel\iluerror.h"\
	"$(ILUSRC)\runtime\kernel\iluerrs.h"\
	"$(ILUSRC)\runtime\kernel\iluhash.h"\
	"$(ILUSRC)\runtime\kernel\ilutpcod.h"\
	"$(ILUSRC)\runtime\kernel\ilutypes.h"\
	"$(ILUSRC)\runtime\kernel\iluwin.h"\
	"$(ILUSRC)\runtime\kernel\iluxport.h"\
	".\cosnaming.h"\
	

"$(INTDIR)\CosNaming-true.obj" : $(SOURCE) $(DEP_CPP_COSNAM) "$(INTDIR)"


!ENDIF 

SOURCE=.\NamingImpl.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_NAMIN=\
	"$(ILUSRC)\runtime\c\iluchdrs.h"\
	"$(ILUSRC)\runtime\c\ilucstub.h"\
	"$(ILUSRC)\runtime\kernel\ilubasic.h"\
	"$(ILUSRC)\runtime\kernel\iluerror.h"\
	"$(ILUSRC)\runtime\kernel\iluerrs.h"\
	"$(ILUSRC)\runtime\kernel\iluhash.h"\
	"$(ILUSRC)\runtime\kernel\ilutpcod.h"\
	"$(ILUSRC)\runtime\kernel\ilutypes.h"\
	"$(ILUSRC)\runtime\kernel\iluwin.h"\
	"$(ILUSRC)\runtime\kernel\iluxport.h"\
	".\cosnaming.h"\
	

"$(INTDIR)\NamingImpl.obj" : $(SOURCE) $(DEP_CPP_NAMIN) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_NAMIN=\
	"$(ILUSRC)\runtime\c\iluchdrs.h"\
	"$(ILUSRC)\runtime\c\ilucstub.h"\
	"$(ILUSRC)\runtime\kernel\ilubasic.h"\
	"$(ILUSRC)\runtime\kernel\iluerror.h"\
	"$(ILUSRC)\runtime\kernel\iluerrs.h"\
	"$(ILUSRC)\runtime\kernel\iluhash.h"\
	"$(ILUSRC)\runtime\kernel\ilutpcod.h"\
	"$(ILUSRC)\runtime\kernel\ilutypes.h"\
	"$(ILUSRC)\runtime\kernel\iluwin.h"\
	"$(ILUSRC)\runtime\kernel\iluxport.h"\
	".\cosnaming.h"\
	

"$(INTDIR)\NamingImpl.obj" : $(SOURCE) $(DEP_CPP_NAMIN) "$(INTDIR)"


!ENDIF 


!ENDIF 

