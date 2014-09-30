# Microsoft Developer Studio Generated NMAKE File, Based on genregs.dsp
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
!MESSAGE NMAKE /f "genregs.mak" CFG="Win32 Debug"
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

!IF "$(RECURSE)" == "0" 

ALL : ".\$(ILUSRC)\stubbers\parser\genregs.exe"

!ELSE 

ALL : ".\$(ILUSRC)\stubbers\parser\genregs.exe"

!ENDIF 

CLEAN :
    -@erase "$(INTDIR)\genregs.obj"
    -@erase "$(INTDIR)\vc50.idb"
    -@erase ".\$(ILUSRC)\stubbers\parser\genregs.exe"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /MD /W3 /GX /O2 /I . /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D\
 "_MBCS" /Fp"$(INTDIR)\genregs.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c\
 
CPP_OBJS=.\WinRel/
CPP_SBRS=.
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\genregs.bsc" 
BSC32_SBRS= \
    
LINK32=link.exe
LINK32_FLAGS=$(ILUHOME)\lib\parser32.lib kernel32.lib user32.lib gdi32.lib\
 winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib\
 uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /incremental:no\
 /pdb:"$(OUTDIR)\genregs.pdb" /machine:I386\
 /out:"$(ILUSRC)\stubbers\parser\genregs.exe" 
LINK32_OBJS= \
    "$(INTDIR)\genregs.obj"

".\$(ILUSRC)\stubbers\parser\genregs.exe" : "$(OUTDIR)" $(DEF_FILE)\
 $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "Win32 Debug"

OUTDIR=.\WinDebug
INTDIR=.\WinDebug

!IF "$(RECURSE)" == "0" 

ALL : ".\genregs.exe"

!ELSE 

ALL : ".\genregs.exe"

!ENDIF 

CLEAN :
    -@erase "$(INTDIR)\genregs.obj"
    -@erase "$(INTDIR)\vc50.idb"
    -@erase "$(INTDIR)\vc50.pdb"
    -@erase "$(OUTDIR)\genregs.pdb"
    -@erase ".\genregs.exe"
    -@erase ".\genregs.ilk"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /MD /W3 /Gm /GX /Zi /Od /I . /D "WIN32" /D "_DEBUG" /D "_CONSOLE"\
 /D "_MBCS" /Fp"$(INTDIR)\genregs.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD\
 /c 
CPP_OBJS=.\WinDebug/
CPP_SBRS=.
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\genregs.bsc" 
BSC32_SBRS= \
    
LINK32=link.exe
LINK32_FLAGS=$(ILUHOME)\lib\parser32.lib kernel32.lib user32.lib gdi32.lib\
 winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib\
 uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /incremental:yes\
 /pdb:"$(OUTDIR)\genregs.pdb" /debug /machine:I386\
 /out:"$(ILUSRC)\stubbers\parser\genregs.exe" /pdbtype:sept 
LINK32_OBJS= \
    "$(INTDIR)\genregs.obj"

".\genregs.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
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
SOURCE=.\genregs.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_GENRE=\
    ".\iluptype.h"\
    ".\iluwin.h"\    

"$(INTDIR)\genregs.obj" : $(SOURCE) $(DEP_CPP_GENRE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_GENRE=\
    ".\iluptype.h"\
    ".\iluwin.h"\
    

"$(INTDIR)\genregs.obj" : $(SOURCE) $(DEP_CPP_GENRE) "$(INTDIR)"


!ENDIF 


!ENDIF 

