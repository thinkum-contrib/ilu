# Microsoft Developer Studio Generated NMAKE File, Based on parserules.dsp
!IF "$(CFG)" == ""
CFG=Win32 Debug
!MESSAGE No configuration specified. Defaulting to Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "Win32 Release" && "$(CFG)" != "Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "parserules.mak" CFG="Win32 Debug"
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

!IF  "$(CFG)" == "Win32 Release"

OUTDIR=$(ILUSRC)\stubbers\genstub\WinRel
INTDIR=$(ILUSRC)\stubbers\genstub\WinRel
# Begin Custom Macros
OutDir=$(ILUSRC)\stubbers\genstub\WinRel
# End Custom Macros

ALL : "$(OUTDIR)\parserules.exe"


CLEAN :
	-@erase "$(INTDIR)\parse-util.obj"
	-@erase "$(INTDIR)\parse.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(OUTDIR)\parserules.exe"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MD /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /Fp"$(INTDIR)\parserules.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

.c{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.c{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

RSC=rc.exe
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\parserules.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /incremental:no /pdb:"$(OUTDIR)\parserules.pdb" /machine:I386 /out:"$(OUTDIR)\parserules.exe" 
LINK32_OBJS= \
	"$(INTDIR)\parse-util.obj" \
	"$(INTDIR)\parse.obj"

"$(OUTDIR)\parserules.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "Win32 Debug"

OUTDIR=$(ILUSRC)\stubbers\genstub\WinDebug
INTDIR=$(ILUSRC)\stubbers\genstub\WinDebug
# Begin Custom Macros
OutDir=$(ILUSRC)\stubbers\genstub\WinDebug
# End Custom Macros

ALL : "$(OUTDIR)\parserules.exe"


CLEAN :
	-@erase "$(INTDIR)\parse-util.obj"
	-@erase "$(INTDIR)\parse.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(OUTDIR)\parserules.exe"
	-@erase "$(OUTDIR)\parserules.ilk"
	-@erase "$(OUTDIR)\parserules.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MD /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /Fp"$(INTDIR)\parserules.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /GZ /c 

.c{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.c{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

RSC=rc.exe
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\parserules.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /incremental:yes /pdb:"$(OUTDIR)\parserules.pdb" /debug /machine:I386 /out:"$(OUTDIR)\parserules.exe" /pdbtype:sept 
LINK32_OBJS= \
	"$(INTDIR)\parse-util.obj" \
	"$(INTDIR)\parse.obj"

"$(OUTDIR)\parserules.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ENDIF 


"$(ILUSRC)\stubbers\genstub\parse-util.c" : \
	"$(ILUSRC)\stubbers\genstub\parse.h"\
	"$(ILUSRC)\stubbers\genstub\rules.h"
	

"$(ILUSRC)\stubbers\genstub\parse.c" : \
	"$(ILUSRC)\stubbers\genstub\parse.h"\
	"$(ILUSRC)\stubbers\genstub\rules.h"
	

!IF "$(CFG)" == "Win32 Release" || "$(CFG)" == "Win32 Debug"
SOURCE="$(ILUSRC)\stubbers\genstub\parse-util.c"

"$(INTDIR)\parse-util.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE="$(ILUSRC)\stubbers\genstub\parse.c"

"$(INTDIR)\parse.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)



!ENDIF 

