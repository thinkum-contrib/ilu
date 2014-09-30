# Microsoft Developer Studio Generated NMAKE File, Based on ilugetbot.dsp
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
!MESSAGE NMAKE /f "ilugetbot.mak" CFG="Win32 Debug"
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

CPP=cl.exe
RSC=rc.exe

!IF  "$(CFG)" == "Win32 Release"

OUTDIR=.\WinRel
INTDIR=.\WinRel
# Begin Custom Macros
OutDir=.\WinRel
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\ilugetbot.exe"

!ELSE 

ALL : "$(OUTDIR)\ilugetbot.exe"

!ENDIF 

CLEAN :
	-@erase "$(INTDIR)\ilugetbot.obj"
	-@erase "$(INTDIR)\iluhttp-common.obj"
	-@erase "$(INTDIR)\iluhttp-surrogate.obj"
	-@erase "$(INTDIR)\vc50.idb"
	-@erase "$(OUTDIR)\ilugetbot.exe"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /MD /W3 /GX /O2 /I "$(ILUHOME)\include" /D "WIN32" /D "NDEBUG"\
 /D "_CONSOLE" /D "_MBCS" /Fp"$(INTDIR)\ilugetbot.pch" /YX /Fo"$(INTDIR)\\"\
 /Fd"$(INTDIR)\\" /FD /c 
CPP_OBJS=.\WinRel/
CPP_SBRS=.
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\ilugetbot.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=$(ILUHOME)\lib\iluc32.lib $(ILUHOME)\lib\ilu32.lib wsock32.lib\
 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib\
 shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo\
 /subsystem:console /incremental:no /pdb:"$(OUTDIR)\ilugetbot.pdb" /machine:I386\
 /out:"$(OUTDIR)\ilugetbot.exe" 
LINK32_OBJS= \
	"$(INTDIR)\ilugetbot.obj" \
	"$(INTDIR)\iluhttp-common.obj" \
	"$(INTDIR)\iluhttp-surrogate.obj"

"$(OUTDIR)\ilugetbot.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "Win32 Debug"

OUTDIR=.\WinDebug
INTDIR=.\WinDebug
# Begin Custom Macros
OutDir=.\WinDebug
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\ilugetbot.exe"

!ELSE 

ALL : "$(OUTDIR)\ilugetbot.exe"

!ENDIF 

CLEAN :
	-@erase "$(INTDIR)\ilugetbot.obj"
	-@erase "$(INTDIR)\iluhttp-common.obj"
	-@erase "$(INTDIR)\iluhttp-surrogate.obj"
	-@erase "$(INTDIR)\vc50.idb"
	-@erase "$(INTDIR)\vc50.pdb"
	-@erase "$(OUTDIR)\ilugetbot.exe"
	-@erase "$(OUTDIR)\ilugetbot.ilk"
	-@erase "$(OUTDIR)\ilugetbot.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /MD /W3 /Gm /GX /Zi /Od /I "$(ILUHOME)\include" /D "WIN32" /D\
 "_DEBUG" /D "_CONSOLE" /D "_MBCS" /Fp"$(INTDIR)\ilugetbot.pch" /YX\
 /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
CPP_OBJS=.\WinDebug/
CPP_SBRS=.
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\ilugetbot.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=$(ILUHOME)\lib\iluc32.lib $(ILUHOME)\lib\ilu32.lib wsock32.lib\
 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib\
 shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo\
 /subsystem:console /incremental:yes /pdb:"$(OUTDIR)\ilugetbot.pdb" /debug\
 /machine:I386 /out:"$(OUTDIR)\ilugetbot.exe" /pdbtype:sept 
LINK32_OBJS= \
	"$(INTDIR)\ilugetbot.obj" \
	"$(INTDIR)\iluhttp-common.obj" \
	"$(INTDIR)\iluhttp-surrogate.obj"

"$(OUTDIR)\ilugetbot.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
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
SOURCE=.\ilugetbot.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_ILUGE=\
	"$(ILUHOME)\include\ilubasic.h"\
	"$(ILUHOME)\include\iluchdrs.h"\
	"$(ILUHOME)\include\ilucstub.h"\
	"$(ILUHOME)\include\iluerror.h"\
	"$(ILUHOME)\include\iluerrs.h"\
	"$(ILUHOME)\include\iluhash.h"\
	"$(ILUHOME)\include\ilutpcod.h"\
	"$(ILUHOME)\include\ilutypes.h"\
	"$(ILUHOME)\include\iluwin.h"\
	"$(ILUHOME)\include\iluxport.h"\
	".\iluhttp.h"\
	

"$(INTDIR)\ilugetbot.obj" : $(SOURCE) $(DEP_CPP_ILUGE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_ILUGE=\
	"$(ILUHOME)\include\ilubasic.h"\
	"$(ILUHOME)\include\iluchdrs.h"\
	"$(ILUHOME)\include\ilucstub.h"\
	"$(ILUHOME)\include\iluerror.h"\
	"$(ILUHOME)\include\iluerrs.h"\
	"$(ILUHOME)\include\iluhash.h"\
	"$(ILUHOME)\include\ilutpcod.h"\
	"$(ILUHOME)\include\ilutypes.h"\
	"$(ILUHOME)\include\iluwin.h"\
	"$(ILUHOME)\include\iluxport.h"\
	".\iluhttp.h"\
	

"$(INTDIR)\ilugetbot.obj" : $(SOURCE) $(DEP_CPP_ILUGE) "$(INTDIR)"


!ENDIF 

SOURCE=".\iluhttp-common.c"

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_ILUHT=\
	"$(ILUHOME)\include\ilubasic.h"\
	"$(ILUHOME)\include\iluchdrs.h"\
	"$(ILUHOME)\include\ilucstub.h"\
	"$(ILUHOME)\include\iluerror.h"\
	"$(ILUHOME)\include\iluerrs.h"\
	"$(ILUHOME)\include\iluhash.h"\
	"$(ILUHOME)\include\ilutpcod.h"\
	"$(ILUHOME)\include\ilutypes.h"\
	"$(ILUHOME)\include\iluwin.h"\
	"$(ILUHOME)\include\iluxport.h"\
	".\iluhttp.h"\
	

"$(INTDIR)\iluhttp-common.obj" : $(SOURCE) $(DEP_CPP_ILUHT) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "Win32 Debug"


"$(INTDIR)\iluhttp-common.obj" : $(SOURCE) "$(INTDIR)"


!ENDIF 

SOURCE=".\iluhttp-surrogate.c"

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_ILUHTT=\
	"$(ILUHOME)\include\ilubasic.h"\
	"$(ILUHOME)\include\iluchdrs.h"\
	"$(ILUHOME)\include\ilucstub.h"\
	"$(ILUHOME)\include\iluerror.h"\
	"$(ILUHOME)\include\iluerrs.h"\
	"$(ILUHOME)\include\iluhash.h"\
	"$(ILUHOME)\include\ilutpcod.h"\
	"$(ILUHOME)\include\ilutypes.h"\
	"$(ILUHOME)\include\iluwin.h"\
	"$(ILUHOME)\include\iluxport.h"\
	".\iluhttp.h"\
	

"$(INTDIR)\iluhttp-surrogate.obj" : $(SOURCE) $(DEP_CPP_ILUHTT) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "Win32 Debug"


"$(INTDIR)\iluhttp-surrogate.obj" : $(SOURCE) "$(INTDIR)"


!ENDIF 


!ENDIF 

