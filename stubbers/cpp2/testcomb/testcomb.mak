# Microsoft Developer Studio Generated NMAKE File, Based on testcomb.dsp
!IF "$(CFG)" == ""
CFG=Win32 Debug
!MESSAGE No configuration specified. Defaulting to Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "Win32 Release" && "$(CFG)" != "Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "testcomb.mak" CFG="Win32 Debug"
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

CPP=cl.exe
RSC=rc.exe

!IF  "$(CFG)" == "Win32 Release"

OUTDIR=.\WinRel
INTDIR=.\WinRel
# Begin Custom Macros
OutDir=.\WinRel
# End Custom Macros

ALL : "$(OUTDIR)\testcomb.exe"


CLEAN :
	-@erase "$(INTDIR)\main.obj"
	-@erase "$(INTDIR)\Testcomb-cpp.obj"
	-@erase "$(INTDIR)\Testcomb-cppsurrogate.obj"
	-@erase "$(INTDIR)\Testcomb-cpptrue.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(OUTDIR)\testcomb.exe"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /MD /W3 /GX /O2 /I "$(ILUSRC)\stubbers\cpp2" /I "$(ILUSRC)\runtime\cpp2" /I "$(ILUSRC)\runtime\kernel" /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /Fp"$(INTDIR)\testcomb.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\testcomb.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=$(ILUHOME)\lib\ilucpp2.lib $(ILUHOME)\lib\ilu32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /incremental:no /pdb:"$(OUTDIR)\testcomb.pdb" /machine:I386 /out:"$(OUTDIR)\testcomb.exe" 
LINK32_OBJS= \
	"$(INTDIR)\Testcomb-cpptrue.obj" \
	"$(INTDIR)\Testcomb-cppsurrogate.obj" \
	"$(INTDIR)\Testcomb-cpp.obj" \
	"$(INTDIR)\main.obj"

"$(OUTDIR)\testcomb.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "Win32 Debug"

OUTDIR=.\WinDebug
INTDIR=.\WinDebug
# Begin Custom Macros
OutDir=.\WinDebug
# End Custom Macros

ALL : "$(OUTDIR)\testcomb.exe"


CLEAN :
	-@erase "$(INTDIR)\main.obj"
	-@erase "$(INTDIR)\Testcomb-cpp.obj"
	-@erase "$(INTDIR)\Testcomb-cppsurrogate.obj"
	-@erase "$(INTDIR)\Testcomb-cpptrue.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(OUTDIR)\testcomb.exe"
	-@erase "$(OUTDIR)\testcomb.ilk"
	-@erase "$(OUTDIR)\testcomb.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /MD /W3 /Gm /GX /ZI /Od /I "$(ILUSRC)\stubbers\cpp2" /I "$(ILUSRC)\runtime\cpp2" /I "$(ILUSRC)\runtime\kernel" /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /Fp"$(INTDIR)\testcomb.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /GZ  /c 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\testcomb.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=$(ILUHOME)\lib\ilucpp2.lib $(ILUHOME)\lib\ilu32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /incremental:yes /pdb:"$(OUTDIR)\testcomb.pdb" /debug /machine:I386 /out:"$(OUTDIR)\testcomb.exe" /pdbtype:sept 
LINK32_OBJS= \
	"$(INTDIR)\Testcomb-cpptrue.obj" \
	"$(INTDIR)\Testcomb-cppsurrogate.obj" \
	"$(INTDIR)\Testcomb-cpp.obj" \
	"$(INTDIR)\main.obj"

"$(OUTDIR)\testcomb.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ENDIF 

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


!IF "$(NO_EXTERNAL_DEPS)" != "1"
!IF EXISTS("testcomb.dep")
!INCLUDE "testcomb.dep"
!ELSE 
!MESSAGE Warning: cannot find "testcomb.dep"
!ENDIF 
!ENDIF 


!IF "$(CFG)" == "Win32 Release" || "$(CFG)" == "Win32 Debug"
SOURCE=$(ILUSRC)\stubbers\cpp2\testcomb\main.cpp

"$(INTDIR)\main.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE="$(ILUSRC)\stubbers\cpp2\testcomb\Testcomb-cpp.cpp"

"$(INTDIR)\Testcomb-cpp.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE="$(ILUSRC)\stubbers\cpp2\testcomb\Testcomb-cppsurrogate.cpp"

"$(INTDIR)\Testcomb-cppsurrogate.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE="$(ILUSRC)\stubbers\cpp2\testcomb\Testcomb-cpptrue.cpp"

"$(INTDIR)\Testcomb-cpptrue.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)



!ENDIF 

