# Microsoft Developer Studio Generated NMAKE File, Based on cpp2stubber.dsp
!IF "$(CFG)" == ""
CFG=Win32 Debug
!MESSAGE No configuration specified. Defaulting to Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "Win32 Release" && "$(CFG)" != "Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "cpp2stubber.mak" CFG="Win32 Debug"
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

OUTDIR=$(ILUSRC)\stubbers\cpp2\WinRel
INTDIR=$(ILUSRC)\stubbers\cpp2\WinRel

ALL : "$(ILUHOME)\bin\cpp2-stubber.exe"


CLEAN :
	-@erase "$(INTDIR)\convert.obj"
	-@erase "$(INTDIR)\cpp2-gen.obj"
	-@erase "$(INTDIR)\encoded-rules.obj"
	-@erase "$(INTDIR)\generate.obj"
	-@erase "$(INTDIR)\typesort.obj"
	-@erase "$(INTDIR)\ilup.data.obj"
	-@erase "$(INTDIR)\modules.obj"
	-@erase "$(INTDIR)\vc60.idb"


"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MD /W3 /GX /O2 /I "$(ILUSRC)\stubbers\genstub" /I "$(ILUSRC)\stubbers\cpp2" /I "$(ILUSRC)\stubbers\parser" /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /Fp"$(INTDIR)\cpp2stubber.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

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
BSC32_FLAGS=/nologo /o"$(OUTDIR)\cpp2stubber.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=$(ILUHOME)\lib\parser32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /incremental:no /pdb:"$(OUTDIR)\cpp2-stubber.pdb" /machine:I386 /out:"$(ILUHOME)\bin\cpp2-stubber.exe" 
LINK32_OBJS= \
	"$(INTDIR)\convert.obj" \
	"$(INTDIR)\cpp2-gen.obj" \
	"$(INTDIR)\encoded-rules.obj" \
	"$(INTDIR)\generate.obj" \
	"$(INTDIR)\typesort.obj" \
	"$(INTDIR)\ilup.data.obj" \
	"$(INTDIR)\modules.obj"

"$(ILUHOME)\bin\cpp2-stubber.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "Win32 Debug"

OUTDIR=$(ILUSRC)\stubbers\cpp2\WinDebug
INTDIR=$(ILUSRC)\stubbers\cpp2\WinDebug

ALL : "$(ILUHOME)\bin\cpp2-stubber.exe"


CLEAN :
	-@erase "$(INTDIR)\convert.obj"
	-@erase "$(INTDIR)\cpp2-gen.obj"
	-@erase "$(INTDIR)\encoded-rules.obj"
	-@erase "$(INTDIR)\generate.obj"
	-@erase "$(INTDIR)\typesort.obj"
	-@erase "$(INTDIR)\ilup.data.obj"
	-@erase "$(INTDIR)\modules.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(OUTDIR)\cpp2-stubber.pdb"
	-@erase "$(ILUHOME)\bin\cpp2-stubber.ilk"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MD /W3 /Gm /GX /ZI /Od /I "$(ILUSRC)\stubbers\genstub" /I "$(ILUSRC)\stubbers\cpp2" /I "$(ILUSRC)\stubbers\parser" /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /Fp"$(INTDIR)\cpp2stubber.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /GZ /c 

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
BSC32_FLAGS=/nologo /o"$(OUTDIR)\cpp2stubber.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=$(ILUHOME)\lib\parser32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /incremental:yes /pdb:"$(OUTDIR)\cpp2-stubber.pdb" /debug /machine:I386 /out:"$(ILUHOME)\bin\cpp2-stubber.exe" /pdbtype:sept 
LINK32_OBJS= \
	"$(INTDIR)\convert.obj" \
	"$(INTDIR)\cpp2-gen.obj" \
	"$(INTDIR)\encoded-rules.obj" \
	"$(INTDIR)\generate.obj" \
	"$(INTDIR)\typesort.obj" \
	"$(INTDIR)\ilup.data.obj" \
	"$(INTDIR)\modules.obj"

"$(ILUHOME)\bin\cpp2-stubber.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ENDIF 


"$(ILUSRC)\stubbers\genstub\convert.c" : \
	"$(ILUSRC)\stubbers\genstub\convert.h"\
	"$(ILUSRC)\stubbers\parser\iluptype.h"\
	"$(ILUSRC)\stubbers\parser\iluwin.h"\
	

"$(ILUSRC)\stubbers\cpp2\cpp2-gen.c" : \
	"$(ILUSRC)\stubbers\genstub\rules.h"\
	"$(ILUSRC)\stubbers\parser\iluptype.h"\
	"$(ILUSRC)\stubbers\parser\iluwin.h"\
	

"$(ILUSRC)\stubbers\cpp2\encoded-rules.c" : \
	"$(ILUSRC)\stubbers\genstub\rules.h"\
	

"$(ILUSRC)\stubbers\genstub\generate.c" : \
	"$(ILUSRC)\stubbers\genstub\rules.h"\
	"$(ILUSRC)\stubbers\parser\iluptype.h"\
	"$(ILUSRC)\stubbers\parser\iluwin.h"\


"$(ILUSRC)\stubbers\genstub\typesort.c" : \
	"$(ILUSRC)\stubbers\genstub\convert.h"\
	"$(ILUSRC)\stubbers\genstub\rules.h"\
	"$(ILUSRC)\stubbers\parser\iluptype.h"\
	"$(ILUSRC)\stubbers\parser\iluwin.h"\
	

"$(ILUSRC)\stubbers\genstub\modules.c" : \
	"$(ILUSRC)\stubbers\genstub\convert.h"\
	"$(ILUSRC)\stubbers\parser\iluptype.h"\
	"$(ILUSRC)\stubbers\parser\iluwin.h"\
	



!IF "$(CFG)" == "Win32 Release" || "$(CFG)" == "Win32 Debug"
SOURCE=$(ILUSRC)\stubbers\genstub\convert.c

"$(INTDIR)\convert.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE="$(ILUSRC)\stubbers\cpp2\cpp2-gen.c"

"$(INTDIR)\cpp2-gen.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE="$(ILUSRC)\stubbers\cpp2\encoded-rules.c"

"$(INTDIR)\encoded-rules.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=$(ILUSRC)\stubbers\genstub\generate.c

"$(INTDIR)\generate.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)
	
	
SOURCE=$(ILUSRC)\stubbers\genstub\typesort.c

"$(INTDIR)\typesort.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=$(ILUSRC)\stubbers\genstub\ilup.data.c

"$(INTDIR)\ilup.data.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=$(ILUSRC)\stubbers\genstub\modules.c

"$(INTDIR)\modules.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)



!ENDIF 

