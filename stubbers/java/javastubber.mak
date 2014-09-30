# Microsoft Developer Studio Generated NMAKE File, Based on javastubber.dsp
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
!MESSAGE NMAKE /f "javastubber.mak" CFG="Win32 Debug"
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

!IF  "$(CFG)" == "Win32 Release"

OUTDIR=.\WinRel
INTDIR=.\WinRel

!IF "$(RECURSE)" == "0" 

ALL : "$(ILUHOME)\bin\java-stubber.exe"

!ELSE 

ALL : "$(ILUHOME)\bin\java-stubber.exe"

!ENDIF 

CLEAN :
    -@erase "$(INTDIR)\context.obj"
    -@erase "$(INTDIR)\genarr.obj"
    -@erase "$(INTDIR)\genenum.obj"
    -@erase "$(INTDIR)\genex.obj"
    -@erase "$(INTDIR)\genobj.obj"
    -@erase "$(INTDIR)\genopt.obj"
    -@erase "$(INTDIR)\genpick.obj"
    -@erase "$(INTDIR)\genrecord.obj"
    -@erase "$(INTDIR)\genseq.obj"
    -@erase "$(INTDIR)\genstub.obj"
    -@erase "$(INTDIR)\genunion.obj"
    -@erase "$(INTDIR)\hlpcls.obj"
    -@erase "$(INTDIR)\io.obj"
    -@erase "$(INTDIR)\isl2java.obj"
    -@erase "$(INTDIR)\name.obj"
    -@erase "$(INTDIR)\gencust.obj"
    -@erase "$(INTDIR)\stubops.obj"
    -@erase "$(INTDIR)\util.obj"
    -@erase "$(INTDIR)\vc50.idb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MD /W3 /GX /O2 /I "$(ILUSRC)\stubbers\java" /I\
 "$(ILUSRC)\stubbers\parser" /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS"\
 /Fp"$(INTDIR)\javastubber.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
CPP_OBJS=.\WinRel/
CPP_SBRS=.

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

RSC=rc.exe
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\javastubber.bsc" 
BSC32_SBRS= \
    
LINK32=link.exe
LINK32_FLAGS=$(ILUHOME)\lib\parser32.lib kernel32.lib user32.lib gdi32.lib\
 winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib\
 uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /incremental:no\
 /pdb:"$(OUTDIR)\java-stubber.pdb" /machine:I386\
 /out:"$(ILUHOME)\bin\java-stubber.exe" 
LINK32_OBJS= \
    "$(INTDIR)\context.obj" \
    "$(INTDIR)\genarr.obj" \
    "$(INTDIR)\genenum.obj" \
    "$(INTDIR)\genex.obj" \
    "$(INTDIR)\genobj.obj" \
    "$(INTDIR)\genopt.obj" \
    "$(INTDIR)\genpick.obj" \
    "$(INTDIR)\genrecord.obj" \
    "$(INTDIR)\genseq.obj" \
    "$(INTDIR)\genstub.obj" \
    "$(INTDIR)\gencust.obj" \
    "$(INTDIR)\genunion.obj" \
    "$(INTDIR)\hlpcls.obj" \
    "$(INTDIR)\io.obj" \
    "$(INTDIR)\isl2java.obj" \
    "$(INTDIR)\name.obj" \
    "$(INTDIR)\stubops.obj" \
    "$(INTDIR)\util.obj"

"$(ILUHOME)\bin\java-stubber.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "Win32 Debug"

OUTDIR=.\WinDebug
INTDIR=.\WinDebug

!IF "$(RECURSE)" == "0" 

ALL : "$(ILUHOME)\bin\java-stubber.exe"

!ELSE 

ALL : "$(ILUHOME)\bin\java-stubber.exe"

!ENDIF 

CLEAN :
    -@erase "$(INTDIR)\context.obj"
    -@erase "$(INTDIR)\genarr.obj"
    -@erase "$(INTDIR)\genenum.obj"
    -@erase "$(INTDIR)\genex.obj"
    -@erase "$(INTDIR)\genobj.obj"
    -@erase "$(INTDIR)\genopt.obj"
    -@erase "$(INTDIR)\genpick.obj"
    -@erase "$(INTDIR)\genrecord.obj"
    -@erase "$(INTDIR)\genseq.obj"
    -@erase "$(INTDIR)\genstub.obj"
    -@erase "$(INTDIR)\gencust.obj"
    -@erase "$(INTDIR)\genunion.obj"
    -@erase "$(INTDIR)\hlpcls.obj"
    -@erase "$(INTDIR)\io.obj"
    -@erase "$(INTDIR)\isl2java.obj"
    -@erase "$(INTDIR)\name.obj"
    -@erase "$(INTDIR)\stubops.obj"
    -@erase "$(INTDIR)\util.obj"
    -@erase "$(INTDIR)\vc50.idb"
    -@erase "$(INTDIR)\vc50.pdb"
    -@erase "$(OUTDIR)\java-stubber.pdb"
    -@erase "$(ILUHOME)\bin\java-stubber.ilk"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MD /W3 /Gm /GX /Zi /Od /I "$(ILUSRC)\stubbers\java" /I\
 "$(ILUSRC)\stubbers\parser" /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS"\
 /Fp"$(INTDIR)\javastubber.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
CPP_OBJS=.\WinDebug/
CPP_SBRS=.

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

RSC=rc.exe
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\javastubber.bsc" 
BSC32_SBRS= \
    
LINK32=link.exe
LINK32_FLAGS=$(ILUHOME)\lib\parser32.lib kernel32.lib user32.lib gdi32.lib\
 winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib\
 uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /incremental:yes\
 /pdb:"$(OUTDIR)\java-stubber.pdb" /debug /machine:I386\
 /out:"$(ILUHOME)\bin\java-stubber.exe" /pdbtype:sept 
LINK32_OBJS= \
    "$(INTDIR)\context.obj" \
    "$(INTDIR)\genarr.obj" \
    "$(INTDIR)\genenum.obj" \
    "$(INTDIR)\genex.obj" \
    "$(INTDIR)\genobj.obj" \
    "$(INTDIR)\genopt.obj" \
    "$(INTDIR)\genpick.obj" \
    "$(INTDIR)\genrecord.obj" \
    "$(INTDIR)\genseq.obj" \
    "$(INTDIR)\genstub.obj" \
    "$(INTDIR)\gencust.obj" \
    "$(INTDIR)\genunion.obj" \
    "$(INTDIR)\hlpcls.obj" \
    "$(INTDIR)\io.obj" \
    "$(INTDIR)\isl2java.obj" \
    "$(INTDIR)\name.obj" \
    "$(INTDIR)\stubops.obj" \
    "$(INTDIR)\util.obj"

"$(ILUHOME)\bin\java-stubber.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ENDIF 


!IF "$(CFG)" == "Win32 Release" || "$(CFG)" ==\
 "Win32 Debug"
SOURCE=$(ILUSRC)\stubbers\java\context.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_CONTE=\
    "$(ILUSRC)\stubbers\java\context.h"\
    "$(ILUSRC)\stubbers\java\shared.h"\
    "$(ILUSRC)\stubbers\java\stubops.h"\
    "$(ILUSRC)\stubbers\parser\iluptype.h"\
    "$(ILUSRC)\stubbers\parser\iluwin.h"\
    

"$(INTDIR)\context.obj" : $(SOURCE) $(DEP_CPP_CONTE) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_CONTE=\
    "$(ILUSRC)\stubbers\java\context.h"\
    "$(ILUSRC)\stubbers\java\shared.h"\
    "$(ILUSRC)\stubbers\java\stubops.h"\
    "$(ILUSRC)\stubbers\parser\iluptype.h"\
    "$(ILUSRC)\stubbers\parser\iluwin.h"\
    

"$(INTDIR)\context.obj" : $(SOURCE) $(DEP_CPP_CONTE) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=$(ILUSRC)\stubbers\java\genarr.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_GENAR=\
    "$(ILUSRC)\stubbers\java\context.h"\
    "$(ILUSRC)\stubbers\java\genarr.h"\
    "$(ILUSRC)\stubbers\java\genseq.h"\
    "$(ILUSRC)\stubbers\java\name.h"\
    "$(ILUSRC)\stubbers\java\shared.h"\
    "$(ILUSRC)\stubbers\java\stubops.h"\
    "$(ILUSRC)\stubbers\java\util.h"\
    "$(ILUSRC)\stubbers\parser\iluptype.h"\
    "$(ILUSRC)\stubbers\parser\iluwin.h"\
    

"$(INTDIR)\genarr.obj" : $(SOURCE) $(DEP_CPP_GENAR) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_GENAR=\
    "$(ILUSRC)\stubbers\java\context.h"\
    "$(ILUSRC)\stubbers\java\genarr.h"\
    "$(ILUSRC)\stubbers\java\genseq.h"\
    "$(ILUSRC)\stubbers\java\name.h"\
    "$(ILUSRC)\stubbers\java\shared.h"\
    "$(ILUSRC)\stubbers\java\stubops.h"\
    "$(ILUSRC)\stubbers\java\util.h"\
    "$(ILUSRC)\stubbers\parser\iluptype.h"\
    "$(ILUSRC)\stubbers\parser\iluwin.h"\
    

"$(INTDIR)\genarr.obj" : $(SOURCE) $(DEP_CPP_GENAR) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=$(ILUSRC)\stubbers\java\genenum.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_GENEN=\
    "$(ILUSRC)\stubbers\java\context.h"\
    "$(ILUSRC)\stubbers\java\genenum.h"\
    "$(ILUSRC)\stubbers\java\genstub.h"\
    "$(ILUSRC)\stubbers\java\hlpcls.h"\
    "$(ILUSRC)\stubbers\java\name.h"\
    "$(ILUSRC)\stubbers\java\shared.h"\
    "$(ILUSRC)\stubbers\java\stubops.h"\
    "$(ILUSRC)\stubbers\java\util.h"\
    "$(ILUSRC)\stubbers\parser\iluptype.h"\
    "$(ILUSRC)\stubbers\parser\iluwin.h"\
    

"$(INTDIR)\genenum.obj" : $(SOURCE) $(DEP_CPP_GENEN) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_GENEN=\
    "$(ILUSRC)\stubbers\java\context.h"\
    "$(ILUSRC)\stubbers\java\genenum.h"\
    "$(ILUSRC)\stubbers\java\genstub.h"\
    "$(ILUSRC)\stubbers\java\hlpcls.h"\
    "$(ILUSRC)\stubbers\java\name.h"\
    "$(ILUSRC)\stubbers\java\shared.h"\
    "$(ILUSRC)\stubbers\java\stubops.h"\
    "$(ILUSRC)\stubbers\java\util.h"\
    "$(ILUSRC)\stubbers\parser\iluptype.h"\
    "$(ILUSRC)\stubbers\parser\iluwin.h"\
    

"$(INTDIR)\genenum.obj" : $(SOURCE) $(DEP_CPP_GENEN) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=$(ILUSRC)\stubbers\java\genex.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_GENEX=\
    "$(ILUSRC)\stubbers\java\context.h"\
    "$(ILUSRC)\stubbers\java\genex.h"\
    "$(ILUSRC)\stubbers\java\genrecord.h"\
    "$(ILUSRC)\stubbers\java\name.h"\
    "$(ILUSRC)\stubbers\java\shared.h"\
    "$(ILUSRC)\stubbers\java\stubops.h"\
    "$(ILUSRC)\stubbers\java\util.h"\
    "$(ILUSRC)\stubbers\parser\iluptype.h"\
    "$(ILUSRC)\stubbers\parser\iluwin.h"\
    

"$(INTDIR)\genex.obj" : $(SOURCE) $(DEP_CPP_GENEX) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_GENEX=\
    "$(ILUSRC)\stubbers\java\context.h"\
    "$(ILUSRC)\stubbers\java\genex.h"\
    "$(ILUSRC)\stubbers\java\genrecord.h"\
    "$(ILUSRC)\stubbers\java\name.h"\
    "$(ILUSRC)\stubbers\java\shared.h"\
    "$(ILUSRC)\stubbers\java\stubops.h"\
    "$(ILUSRC)\stubbers\java\util.h"\
    "$(ILUSRC)\stubbers\parser\iluptype.h"\
    "$(ILUSRC)\stubbers\parser\iluwin.h"\
    

"$(INTDIR)\genex.obj" : $(SOURCE) $(DEP_CPP_GENEX) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=$(ILUSRC)\stubbers\java\genobj.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_GENOB=\
    "$(ILUSRC)\stubbers\java\context.h"\
    "$(ILUSRC)\stubbers\java\genex.h"\
    "$(ILUSRC)\stubbers\java\genobj.h"\
    "$(ILUSRC)\stubbers\java\genrecord.h"\
    "$(ILUSRC)\stubbers\java\genstub.h"\
    "$(ILUSRC)\stubbers\java\genunion.h"\
    "$(ILUSRC)\stubbers\java\hlpcls.h"\
    "$(ILUSRC)\stubbers\java\name.h"\
    "$(ILUSRC)\stubbers\java\shared.h"\
    "$(ILUSRC)\stubbers\java\stubops.h"\
    "$(ILUSRC)\stubbers\java\util.h"\
    "$(ILUSRC)\stubbers\parser\iluptype.h"\
    "$(ILUSRC)\stubbers\parser\iluwin.h"\
    

"$(INTDIR)\genobj.obj" : $(SOURCE) $(DEP_CPP_GENOB) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_GENOB=\
    "$(ILUSRC)\stubbers\java\context.h"\
    "$(ILUSRC)\stubbers\java\genex.h"\
    "$(ILUSRC)\stubbers\java\genobj.h"\
    "$(ILUSRC)\stubbers\java\genrecord.h"\
    "$(ILUSRC)\stubbers\java\genstub.h"\
    "$(ILUSRC)\stubbers\java\genunion.h"\
    "$(ILUSRC)\stubbers\java\hlpcls.h"\
    "$(ILUSRC)\stubbers\java\name.h"\
    "$(ILUSRC)\stubbers\java\shared.h"\
    "$(ILUSRC)\stubbers\java\stubops.h"\
    "$(ILUSRC)\stubbers\java\util.h"\
    "$(ILUSRC)\stubbers\parser\iluptype.h"\
    "$(ILUSRC)\stubbers\parser\iluwin.h"\
    

"$(INTDIR)\genobj.obj" : $(SOURCE) $(DEP_CPP_GENOB) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=$(ILUSRC)\stubbers\java\genopt.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_GENOP=\
    "$(ILUSRC)\stubbers\java\context.h"\
    "$(ILUSRC)\stubbers\java\genopt.h"\
    "$(ILUSRC)\stubbers\java\name.h"\
    "$(ILUSRC)\stubbers\java\shared.h"\
    "$(ILUSRC)\stubbers\java\stubops.h"\
    "$(ILUSRC)\stubbers\java\util.h"\
    "$(ILUSRC)\stubbers\parser\iluptype.h"\
    "$(ILUSRC)\stubbers\parser\iluwin.h"\
    

"$(INTDIR)\genopt.obj" : $(SOURCE) $(DEP_CPP_GENOP) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_GENOP=\
    "$(ILUSRC)\stubbers\java\context.h"\
    "$(ILUSRC)\stubbers\java\genopt.h"\
    "$(ILUSRC)\stubbers\java\name.h"\
    "$(ILUSRC)\stubbers\java\shared.h"\
    "$(ILUSRC)\stubbers\java\stubops.h"\
    "$(ILUSRC)\stubbers\java\util.h"\
    "$(ILUSRC)\stubbers\parser\iluptype.h"\
    "$(ILUSRC)\stubbers\parser\iluwin.h"\
    

"$(INTDIR)\genopt.obj" : $(SOURCE) $(DEP_CPP_GENOP) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=$(ILUSRC)\stubbers\java\genpick.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_GENPI=\
    "$(ILUSRC)\stubbers\java\context.h"\
    "$(ILUSRC)\stubbers\java\genstub.h"\
    "$(ILUSRC)\stubbers\java\name.h"\
    "$(ILUSRC)\stubbers\java\shared.h"\
    "$(ILUSRC)\stubbers\java\stubops.h"\
    "$(ILUSRC)\stubbers\java\util.h"\
    "$(ILUSRC)\stubbers\parser\iluptype.h"\
    "$(ILUSRC)\stubbers\parser\iluwin.h"\
    

"$(INTDIR)\genpick.obj" : $(SOURCE) $(DEP_CPP_GENPI) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_GENPI=\
    "$(ILUSRC)\stubbers\java\context.h"\
    "$(ILUSRC)\stubbers\java\genstub.h"\
    "$(ILUSRC)\stubbers\java\name.h"\
    "$(ILUSRC)\stubbers\java\shared.h"\
    "$(ILUSRC)\stubbers\java\stubops.h"\
    "$(ILUSRC)\stubbers\java\util.h"\
    "$(ILUSRC)\stubbers\parser\iluptype.h"\
    "$(ILUSRC)\stubbers\parser\iluwin.h"\
    

"$(INTDIR)\genpick.obj" : $(SOURCE) $(DEP_CPP_GENPI) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=$(ILUSRC)\stubbers\java\genrecord.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_GENRE=\
    "$(ILUSRC)\stubbers\java\context.h"\
    "$(ILUSRC)\stubbers\java\genrecord.h"\
    "$(ILUSRC)\stubbers\java\genstub.h"\
    "$(ILUSRC)\stubbers\java\hlpcls.h"\
    "$(ILUSRC)\stubbers\java\name.h"\
    "$(ILUSRC)\stubbers\java\shared.h"\
    "$(ILUSRC)\stubbers\java\stubops.h"\
    "$(ILUSRC)\stubbers\java\util.h"\
    "$(ILUSRC)\stubbers\parser\iluptype.h"\
    "$(ILUSRC)\stubbers\parser\iluwin.h"\
    

"$(INTDIR)\genrecord.obj" : $(SOURCE) $(DEP_CPP_GENRE) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_GENRE=\
    "$(ILUSRC)\stubbers\java\context.h"\
    "$(ILUSRC)\stubbers\java\genrecord.h"\
    "$(ILUSRC)\stubbers\java\genstub.h"\
    "$(ILUSRC)\stubbers\java\hlpcls.h"\
    "$(ILUSRC)\stubbers\java\name.h"\
    "$(ILUSRC)\stubbers\java\shared.h"\
    "$(ILUSRC)\stubbers\java\stubops.h"\
    "$(ILUSRC)\stubbers\java\util.h"\
    "$(ILUSRC)\stubbers\parser\iluptype.h"\
    "$(ILUSRC)\stubbers\parser\iluwin.h"\
    

"$(INTDIR)\genrecord.obj" : $(SOURCE) $(DEP_CPP_GENRE) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=$(ILUSRC)\stubbers\java\genseq.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_GENSE=\
    "$(ILUSRC)\stubbers\java\context.h"\
    "$(ILUSRC)\stubbers\java\genseq.h"\
    "$(ILUSRC)\stubbers\java\name.h"\
    "$(ILUSRC)\stubbers\java\shared.h"\
    "$(ILUSRC)\stubbers\java\stubops.h"\
    "$(ILUSRC)\stubbers\java\util.h"\
    "$(ILUSRC)\stubbers\parser\iluptype.h"\
    "$(ILUSRC)\stubbers\parser\iluwin.h"\
    

"$(INTDIR)\genseq.obj" : $(SOURCE) $(DEP_CPP_GENSE) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_GENSE=\
    "$(ILUSRC)\stubbers\java\context.h"\
    "$(ILUSRC)\stubbers\java\genseq.h"\
    "$(ILUSRC)\stubbers\java\name.h"\
    "$(ILUSRC)\stubbers\java\shared.h"\
    "$(ILUSRC)\stubbers\java\stubops.h"\
    "$(ILUSRC)\stubbers\java\util.h"\
    "$(ILUSRC)\stubbers\parser\iluptype.h"\
    "$(ILUSRC)\stubbers\parser\iluwin.h"\
    

"$(INTDIR)\genseq.obj" : $(SOURCE) $(DEP_CPP_GENSE) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=$(ILUSRC)\stubbers\java\genstub.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_GENST=\
    "$(ILUSRC)\stubbers\java\context.h"\
    "$(ILUSRC)\stubbers\java\genarr.h"\
    "$(ILUSRC)\stubbers\java\genenum.h"\
    "$(ILUSRC)\stubbers\java\genex.h"\
    "$(ILUSRC)\stubbers\java\genobj.h"\
    "$(ILUSRC)\stubbers\java\genopt.h"\
    "$(ILUSRC)\stubbers\java\genrecord.h"\
    "$(ILUSRC)\stubbers\java\genseq.h"\
    "$(ILUSRC)\stubbers\java\genstub.h"\
    "$(ILUSRC)\stubbers\java\genunion.h"\
    "$(ILUSRC)\stubbers\java\hlpcls.h"\
    "$(ILUSRC)\stubbers\java\name.h"\
    "$(ILUSRC)\stubbers\java\shared.h"\
    "$(ILUSRC)\stubbers\java\stubops.h"\
    "$(ILUSRC)\stubbers\java\util.h"\
    "$(ILUSRC)\stubbers\parser\iluptype.h"\
    "$(ILUSRC)\stubbers\parser\iluwin.h"\
    

"$(INTDIR)\genstub.obj" : $(SOURCE) $(DEP_CPP_GENST) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_GENST=\
    "$(ILUSRC)\stubbers\java\context.h"\
    "$(ILUSRC)\stubbers\java\genarr.h"\
    "$(ILUSRC)\stubbers\java\genenum.h"\
    "$(ILUSRC)\stubbers\java\genex.h"\
    "$(ILUSRC)\stubbers\java\genobj.h"\
    "$(ILUSRC)\stubbers\java\genopt.h"\
    "$(ILUSRC)\stubbers\java\genrecord.h"\
    "$(ILUSRC)\stubbers\java\genseq.h"\
    "$(ILUSRC)\stubbers\java\genstub.h"\
    "$(ILUSRC)\stubbers\java\genunion.h"\
    "$(ILUSRC)\stubbers\java\hlpcls.h"\
    "$(ILUSRC)\stubbers\java\name.h"\
    "$(ILUSRC)\stubbers\java\shared.h"\
    "$(ILUSRC)\stubbers\java\stubops.h"\
    "$(ILUSRC)\stubbers\java\util.h"\
    "$(ILUSRC)\stubbers\parser\iluptype.h"\
    "$(ILUSRC)\stubbers\parser\iluwin.h"\
    

"$(INTDIR)\genstub.obj" : $(SOURCE) $(DEP_CPP_GENST) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=$(ILUSRC)\stubbers\java\genunion.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_GENUN=\
    "$(ILUSRC)\stubbers\java\context.h"\
    "$(ILUSRC)\stubbers\java\genenum.h"\
    "$(ILUSRC)\stubbers\java\genopt.h"\
    "$(ILUSRC)\stubbers\java\genunion.h"\
    "$(ILUSRC)\stubbers\java\hlpcls.h"\
    "$(ILUSRC)\stubbers\java\name.h"\
    "$(ILUSRC)\stubbers\java\shared.h"\
    "$(ILUSRC)\stubbers\java\stubops.h"\
    "$(ILUSRC)\stubbers\java\util.h"\
    "$(ILUSRC)\stubbers\parser\iluptype.h"\
    "$(ILUSRC)\stubbers\parser\iluwin.h"\
    

"$(INTDIR)\genunion.obj" : $(SOURCE) $(DEP_CPP_GENUN) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_GENUN=\
    "$(ILUSRC)\stubbers\java\context.h"\
    "$(ILUSRC)\stubbers\java\genenum.h"\
    "$(ILUSRC)\stubbers\java\genopt.h"\
    "$(ILUSRC)\stubbers\java\genunion.h"\
    "$(ILUSRC)\stubbers\java\hlpcls.h"\
    "$(ILUSRC)\stubbers\java\name.h"\
    "$(ILUSRC)\stubbers\java\shared.h"\
    "$(ILUSRC)\stubbers\java\stubops.h"\
    "$(ILUSRC)\stubbers\java\util.h"\
    "$(ILUSRC)\stubbers\parser\iluptype.h"\
    "$(ILUSRC)\stubbers\parser\iluwin.h"\
    

"$(INTDIR)\genunion.obj" : $(SOURCE) $(DEP_CPP_GENUN) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=$(ILUSRC)\stubbers\java\hlpcls.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_HLPCL=\
    "$(ILUSRC)\stubbers\java\context.h"\
    "$(ILUSRC)\stubbers\java\genenum.h"\
    "$(ILUSRC)\stubbers\java\genobj.h"\
    "$(ILUSRC)\stubbers\java\genopt.h"\
    "$(ILUSRC)\stubbers\java\name.h"\
    "$(ILUSRC)\stubbers\java\shared.h"\
    "$(ILUSRC)\stubbers\java\stubops.h"\
    "$(ILUSRC)\stubbers\java\util.h"\
    "$(ILUSRC)\stubbers\java\gencust.h"\
    "$(ILUSRC)\stubbers\parser\iluptype.h"\
    "$(ILUSRC)\stubbers\parser\iluwin.h"\
    

"$(INTDIR)\hlpcls.obj" : $(SOURCE) $(DEP_CPP_HLPCL) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_HLPCL=\
    "$(ILUSRC)\stubbers\java\context.h"\
    "$(ILUSRC)\stubbers\java\genenum.h"\
    "$(ILUSRC)\stubbers\java\genobj.h"\
    "$(ILUSRC)\stubbers\java\genopt.h"\
    "$(ILUSRC)\stubbers\java\name.h"\
    "$(ILUSRC)\stubbers\java\shared.h"\
    "$(ILUSRC)\stubbers\java\stubops.h"\
    "$(ILUSRC)\stubbers\java\gencust.h"\
    "$(ILUSRC)\stubbers\java\util.h"\
    "$(ILUSRC)\stubbers\parser\iluptype.h"\
    "$(ILUSRC)\stubbers\parser\iluwin.h"\
    

"$(INTDIR)\hlpcls.obj" : $(SOURCE) $(DEP_CPP_HLPCL) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=$(ILUSRC)\stubbers\java\io.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_IO_C18=\
    "$(ILUSRC)\stubbers\java\context.h"\
    "$(ILUSRC)\stubbers\java\genarr.h"\
    "$(ILUSRC)\stubbers\java\genenum.h"\
    "$(ILUSRC)\stubbers\java\genobj.h"\
    "$(ILUSRC)\stubbers\java\genopt.h"\
    "$(ILUSRC)\stubbers\java\genpick.h"\
    "$(ILUSRC)\stubbers\java\genrecord.h"\
    "$(ILUSRC)\stubbers\java\genseq.h"\
    "$(ILUSRC)\stubbers\java\genunion.h"\
    "$(ILUSRC)\stubbers\java\name.h"\
    "$(ILUSRC)\stubbers\java\shared.h"\
    "$(ILUSRC)\stubbers\java\stubops.h"\
    "$(ILUSRC)\stubbers\java\util.h"\
    "$(ILUSRC)\stubbers\java\gencust.h"\
    "$(ILUSRC)\stubbers\parser\iluptype.h"\
    "$(ILUSRC)\stubbers\parser\iluwin.h"\
    

"$(INTDIR)\io.obj" : $(SOURCE) $(DEP_CPP_IO_C18) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_IO_C18=\
    "$(ILUSRC)\stubbers\java\context.h"\
    "$(ILUSRC)\stubbers\java\genarr.h"\
    "$(ILUSRC)\stubbers\java\genenum.h"\
    "$(ILUSRC)\stubbers\java\genobj.h"\
    "$(ILUSRC)\stubbers\java\genopt.h"\
    "$(ILUSRC)\stubbers\java\genpick.h"\
    "$(ILUSRC)\stubbers\java\genrecord.h"\
    "$(ILUSRC)\stubbers\java\genseq.h"\
    "$(ILUSRC)\stubbers\java\genunion.h"\
    "$(ILUSRC)\stubbers\java\name.h"\
    "$(ILUSRC)\stubbers\java\shared.h"\
    "$(ILUSRC)\stubbers\java\stubops.h"\
    "$(ILUSRC)\stubbers\java\gencust.h"\
    "$(ILUSRC)\stubbers\java\util.h"\
    "$(ILUSRC)\stubbers\parser\iluptype.h"\
    "$(ILUSRC)\stubbers\parser\iluwin.h"\
    

"$(INTDIR)\io.obj" : $(SOURCE) $(DEP_CPP_IO_C18) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=$(ILUSRC)\stubbers\java\isl2java.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_ISL2J=\
    "$(ILUSRC)\stubbers\java\context.h"\
    "$(ILUSRC)\stubbers\java\genstub.h"\
    "$(ILUSRC)\stubbers\java\name.h"\
    "$(ILUSRC)\stubbers\java\shared.h"\
    "$(ILUSRC)\stubbers\java\stubops.h"\
    "$(ILUSRC)\stubbers\java\util.h"\
    "$(ILUSRC)\stubbers\java\gencust.h"\
    "$(ILUSRC)\stubbers\parser\iluptype.h"\
    "$(ILUSRC)\stubbers\parser\iluwin.h"\
    

"$(INTDIR)\isl2java.obj" : $(SOURCE) $(DEP_CPP_ISL2J) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_ISL2J=\
    "$(ILUSRC)\stubbers\java\context.h"\
    "$(ILUSRC)\stubbers\java\genstub.h"\
    "$(ILUSRC)\stubbers\java\name.h"\
    "$(ILUSRC)\stubbers\java\shared.h"\
    "$(ILUSRC)\stubbers\java\gencust.h"\
    "$(ILUSRC)\stubbers\java\stubops.h"\
    "$(ILUSRC)\stubbers\java\util.h"\
    "$(ILUSRC)\stubbers\parser\iluptype.h"\
    "$(ILUSRC)\stubbers\parser\iluwin.h"\
    

"$(INTDIR)\isl2java.obj" : $(SOURCE) $(DEP_CPP_ISL2J) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=$(ILUSRC)\stubbers\java\name.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_NAME_=\
    "$(ILUSRC)\stubbers\java\context.h"\
    "$(ILUSRC)\stubbers\java\genarr.h"\
    "$(ILUSRC)\stubbers\java\genenum.h"\
    "$(ILUSRC)\stubbers\java\genobj.h"\
    "$(ILUSRC)\stubbers\java\genopt.h"\
    "$(ILUSRC)\stubbers\java\genpick.h"\
    "$(ILUSRC)\stubbers\java\genrecord.h"\
    "$(ILUSRC)\stubbers\java\genseq.h"\
    "$(ILUSRC)\stubbers\java\genunion.h"\
    "$(ILUSRC)\stubbers\java\name.h"\
    "$(ILUSRC)\stubbers\java\shared.h"\
    "$(ILUSRC)\stubbers\java\stubops.h"\
    "$(ILUSRC)\stubbers\java\util.h"\
    "$(ILUSRC)\stubbers\java\gencust.h"\
    "$(ILUSRC)\stubbers\parser\iluptype.h"\
    "$(ILUSRC)\stubbers\parser\iluwin.h"\
    

"$(INTDIR)\name.obj" : $(SOURCE) $(DEP_CPP_NAME_) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_NAME_=\
    "$(ILUSRC)\stubbers\java\context.h"\
    "$(ILUSRC)\stubbers\java\genarr.h"\
    "$(ILUSRC)\stubbers\java\genenum.h"\
    "$(ILUSRC)\stubbers\java\genobj.h"\
    "$(ILUSRC)\stubbers\java\genopt.h"\
    "$(ILUSRC)\stubbers\java\genpick.h"\
    "$(ILUSRC)\stubbers\java\gencust.h"\
    "$(ILUSRC)\stubbers\java\genrecord.h"\
    "$(ILUSRC)\stubbers\java\genseq.h"\
    "$(ILUSRC)\stubbers\java\genunion.h"\
    "$(ILUSRC)\stubbers\java\name.h"\
    "$(ILUSRC)\stubbers\java\shared.h"\
    "$(ILUSRC)\stubbers\java\stubops.h"\
    "$(ILUSRC)\stubbers\java\util.h"\
    "$(ILUSRC)\stubbers\parser\iluptype.h"\
    "$(ILUSRC)\stubbers\parser\iluwin.h"\
    

"$(INTDIR)\name.obj" : $(SOURCE) $(DEP_CPP_NAME_) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=$(ILUSRC)\stubbers\java\stubops.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_STUBO=\
    "$(ILUSRC)\stubbers\java\shared.h"\
    "$(ILUSRC)\stubbers\parser\iluptype.h"\
    "$(ILUSRC)\stubbers\parser\iluwin.h"\
    

"$(INTDIR)\stubops.obj" : $(SOURCE) $(DEP_CPP_STUBO) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_STUBO=\
    "$(ILUSRC)\stubbers\java\shared.h"\
    "$(ILUSRC)\stubbers\parser\iluptype.h"\
    "$(ILUSRC)\stubbers\parser\iluwin.h"\
    

"$(INTDIR)\stubops.obj" : $(SOURCE) $(DEP_CPP_STUBO) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=$(ILUSRC)\stubbers\java\util.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_UTIL_=\
    "$(ILUSRC)\stubbers\java\context.h"\
    "$(ILUSRC)\stubbers\java\name.h"\
    "$(ILUSRC)\stubbers\java\shared.h"\
    "$(ILUSRC)\stubbers\java\stubops.h"\
    "$(ILUSRC)\stubbers\java\util.h"\
    "$(ILUSRC)\stubbers\parser\iluptype.h"\
    "$(ILUSRC)\stubbers\parser\iluwin.h"\
    {$(INCLUDE)}"sys\stat.h"\
    {$(INCLUDE)}"sys\types.h"\
    

"$(INTDIR)\util.obj" : $(SOURCE) $(DEP_CPP_UTIL_) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_UTIL_=\
    "$(ILUSRC)\stubbers\java\context.h"\
    "$(ILUSRC)\stubbers\java\name.h"\
    "$(ILUSRC)\stubbers\java\shared.h"\
    "$(ILUSRC)\stubbers\java\stubops.h"\
    "$(ILUSRC)\stubbers\java\util.h"\
    "$(ILUSRC)\stubbers\parser\iluptype.h"\
    "$(ILUSRC)\stubbers\parser\iluwin.h"\
    

"$(INTDIR)\util.obj" : $(SOURCE) $(DEP_CPP_UTIL_) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 


SOURCE=$(ILUSRC)\stubbers\java\gencust.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_GENCU=\
    "$(ILUSRC)\stubbers\java\context.h"\
    "$(ILUSRC)\stubbers\java\name.h"\
    "$(ILUSRC)\stubbers\java\shared.h"\
    "$(ILUSRC)\stubbers\java\stubops.h"\
    "$(ILUSRC)\stubbers\java\util.h"\
    "$(ILUSRC)\stubbers\parser\iluptype.h"\
    "$(ILUSRC)\stubbers\parser\iluwin.h"\
    "$(ILUSRC)\stubbers\java\io.h"\
    "$(ILUSRC)\stubbers\java\gencust.h"\
    "$(ILUSRC)\stubbers\java\hlpcls.h"\
    {$(INCLUDE)}"sys\stat.h"\
    {$(INCLUDE)}"sys\types.h"\
    

"$(INTDIR)\gencust.obj" : $(SOURCE) $(DEP_CPP_GENCU) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_GENCU=\
    "$(ILUSRC)\stubbers\java\context.h"\
    "$(ILUSRC)\stubbers\java\name.h"\
    "$(ILUSRC)\stubbers\java\shared.h"\
    "$(ILUSRC)\stubbers\java\stubops.h"\
    "$(ILUSRC)\stubbers\java\util.h"\
    "$(ILUSRC)\stubbers\parser\iluptype.h"\
    "$(ILUSRC)\stubbers\parser\iluwin.h"\
    "$(ILUSRC)\stubbers\java\io.h"\
    "$(ILUSRC)\stubbers\java\gencust.h"\
    "$(ILUSRC)\stubbers\java\hlpcls.h"\
    {$(INCLUDE)}"sys\stat.h"\
    {$(INCLUDE)}"sys\types.h"\
        

"$(INTDIR)\gencust.obj" : $(SOURCE) $(DEP_CPP_GENCU) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 


!ENDIF 

