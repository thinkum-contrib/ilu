# Modified so that $(ILUHOME) can be used to specifiy location
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
!MESSAGE NMAKE /f "ilucpp2.mak" CFG="Win32 Debug"
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
CPP=cl.exe
MTL=mktyplib.exe
RSC=rc.exe

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

ALL : "$(ILUHOME)\bin\ilucpp2.dll"

CLEAN : 
    -@erase "$(INTDIR)\corba.obj"
    -@erase "$(INTDIR)\iluactivation.obj"
    -@erase "$(INTDIR)\ilucall.obj"
    -@erase "$(INTDIR)\ilucppinternal.obj"
    -@erase "$(INTDIR)\ilucppruntime.obj"
    -@erase "$(INTDIR)\ilugarbagecollect.obj"
    -@erase "$(INTDIR)\ilumainloop.obj"
    -@erase "$(INTDIR)\iluobject.obj"
    -@erase "$(INTDIR)\iluobjecttable.obj"
    -@erase "$(INTDIR)\ilusecurity.obj"
    -@erase "$(INTDIR)\iluserver.obj"


"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

"$(INTDIR)" :
    if not exist "$(INTDIR)/$(NULL)" mkdir "$(INTDIR)"

# ADD BASE CPP /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /c
# ADD CPP /nologo /MD /W3 /GX /O2 /I "$(ILUSRC)\runtime\cpp2" /I "$(ILUSRC)\stubbers\cpp2" /I "$(ILUSRC)\runtime\kernel" /I "$(ILUSRC)\GSS\kernel" /D "NDEBUG" /D "WIN32" /D "_WINDOWS" /D "ILU_BUILDING_RUNTIME" /YX /c
# SUBTRACT CPP /Fr
CPP_PROJ=/nologo /MD /W3 /GX /O2 /I "$(ILUSRC)\runtime\cpp2"  /I "$(ILUSRC)\stubbers\cpp2" /I\
 "$(ILUSRC)\runtime\kernel" /I "$(ILUSRC)\GSS\kernel" /D "NDEBUG" /D "WIN32" /D "_WINDOWS" /D\
 "ILU_BUILDING_RUNTIME" /Fp"$(INTDIR)/cpp2.pch" /YX /Fo"$(INTDIR)/" /c 
CPP_OBJS=.\Release/
CPP_SBRS=.\.
# ADD BASE MTL /nologo /D "NDEBUG" /win32
# ADD MTL /nologo /D "NDEBUG" /win32
MTL_PROJ=/nologo /D "NDEBUG" /win32 
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/cpp2.bsc" 
BSC32_SBRS= \
    
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /machine:I386
# ADD LINK32 $(ILUHOME)\lib\ilu32.lib wsock32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /machine:I386 /out:"$(ILUHOME)\bin\ilucpp2.dll"
LINK32_FLAGS=$(ILUHOME)\lib\ilu32.lib wsock32.lib kernel32.lib user32.lib\
 gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib\
 oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll\
 /incremental:no /pdb:"$(OUTDIR)/ilucpp2.pdb" /machine:I386\
 /out:"$(ILUHOME)\bin\ilucpp2.dll" /implib:"$(OUTDIR)/ilucpp2.lib" 
LINK32_OBJS= \
    "$(INTDIR)\corba.obj" \
    "$(INTDIR)\iluactivation.obj" \
    "$(INTDIR)\ilucall.obj" \
    "$(INTDIR)\ilucppinternal.obj" \
    "$(INTDIR)\ilucppruntime.obj" \
    "$(INTDIR)\ilugarbagecollect.obj" \
    "$(INTDIR)\ilumainloop.obj" \
    "$(INTDIR)\iluobject.obj" \
    "$(INTDIR)\ilusecurity.obj" \
    "$(INTDIR)\iluobjecttable.obj" \
    "$(INTDIR)\iluserver.obj"

"$(ILUHOME)\bin\ilucpp2.dll" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
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

ALL : "$(ILUHOME)\bin\ilucpp2.dll" 

CLEAN : 
    -@erase "$(INTDIR)\corba.obj"
    -@erase "$(INTDIR)\corba.sbr"
    -@erase "$(INTDIR)\iluactivation.obj"
    -@erase "$(INTDIR)\iluactivation.sbr"
    -@erase "$(INTDIR)\ilucall.obj"
    -@erase "$(INTDIR)\ilucall.sbr"
    -@erase "$(INTDIR)\ilucppinternal.obj"
    -@erase "$(INTDIR)\ilucppinternal.sbr"
    -@erase "$(INTDIR)\ilucppruntime.obj"
    -@erase "$(INTDIR)\ilucppruntime.sbr"
    -@erase "$(INTDIR)\ilugarbagecollect.obj"
    -@erase "$(INTDIR)\ilugarbagecollect.sbr"
    -@erase "$(INTDIR)\ilumainloop.obj"
    -@erase "$(INTDIR)\ilumainloop.sbr"
    -@erase "$(INTDIR)\iluobject.obj"
    -@erase "$(INTDIR)\iluobject.sbr"
    -@erase "$(INTDIR)\ilusecurity.obj"
    -@erase "$(INTDIR)\ilusecurity.sbr"
    -@erase "$(INTDIR)\iluobjecttable.obj"
    -@erase "$(INTDIR)\iluobjecttable.sbr"
    -@erase "$(INTDIR)\iluserver.obj"
    -@erase "$(INTDIR)\iluserver.sbr"
    -@erase "$(INTDIR)\vc40.idb"
    -@erase "$(INTDIR)\vc40.pdb"
    -@erase "$(OUTDIR)\ilucpp2.exp"
    -@erase "$(ILUHOME)\bin\ilucpp2.ilk"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

"$(INTDIR)" :
    if not exist "$(INTDIR)/$(NULL)" mkdir "$(INTDIR)"

# ADD BASE CPP /nologo /MTd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /YX /c
# ADD CPP /nologo /MD /W3 /Gm /GX /Zi /Od /I "$(ILUSRC)\runtime\cpp2"  /I "$(ILUSRC)\stubbers\cpp2" /I "$(ILUSRC)\runtime\kernel" /I "$(ILUSRC)\GSS\kernel" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "ILU_BUILDING_RUNTIME" /Fr /YX /c
CPP_PROJ=/nologo /MD /W3 /Gm /GX /Zi /Od /I "$(ILUSRC)\runtime\cpp2"  /I "$(ILUSRC)\stubbers\cpp2" /I\
 "$(ILUSRC)\runtime\kernel" /I "$(ILUSRC)\GSS\kernel" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D\
 "ILU_BUILDING_RUNTIME" $(ILU_DEBUG_CPPFLAGS) /Fp"$(INTDIR)/cpp2.pch" /YX\
 /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c 
CPP_OBJS=.\Debug/
CPP_SBRS=.\Debug/
# ADD BASE MTL /nologo /D "_DEBUG" /win32
# ADD MTL /nologo /D "_DEBUG" /win32
MTL_PROJ=/nologo /D "_DEBUG" /win32 
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo /o"$(ILUHOME)\browsefiles\ilucpp2.bsc"
BSC32_FLAGS=/nologo /o"$(ILUHOME)\browsefiles\ilucpp2.bsc" 
BSC32_SBRS= \
    "$(INTDIR)\corba.sbr" \
    "$(INTDIR)\iluactivation.sbr" \
    "$(INTDIR)\ilucall.sbr" \
    "$(INTDIR)\ilucppinternal.sbr" \
    "$(INTDIR)\ilucppruntime.sbr" \
    "$(INTDIR)\ilugarbagecollect.sbr" \
    "$(INTDIR)\ilumainloop.sbr" \
    "$(INTDIR)\iluobject.sbr" \
    "$(INTDIR)\ilusecurity.sbr" \
    "$(INTDIR)\iluobjecttable.sbr" \
    "$(INTDIR)\iluserver.sbr"

"$(ILUHOME)\browsefiles\ilucpp2.bsc" : "$(OUTDIR)" $(BSC32_SBRS)
    $(BSC32) @<<
  $(BSC32_FLAGS) $(BSC32_SBRS)
<<

LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /debug /machine:I386
# ADD LINK32 $(ILUHOME)\lib\ilu32.lib wsock32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /debug /machine:I386 /out:"$(ILUHOME)\bin\ilucpp2.dll"
LINK32_FLAGS=$(ILUHOME)\lib\ilu32.lib wsock32.lib kernel32.lib user32.lib\
 gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib\
 oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll\
 /incremental:yes /pdb:"$(OUTDIR)/ilucpp2.pdb" /debug /machine:I386\
 /out:"$(ILUHOME)\bin\ilucpp2.dll" /implib:"$(OUTDIR)/ilucpp2.lib" 
LINK32_OBJS= \
    "$(INTDIR)\corba.obj" \
    "$(INTDIR)\iluactivation.obj" \
    "$(INTDIR)\ilucall.obj" \
    "$(INTDIR)\ilucppinternal.obj" \
    "$(INTDIR)\ilucppruntime.obj" \
    "$(INTDIR)\ilugarbagecollect.obj" \
    "$(INTDIR)\ilumainloop.obj" \
    "$(INTDIR)\iluobject.obj" \
    "$(INTDIR)\ilusecurity.obj" \
    "$(INTDIR)\iluobjecttable.obj" \
    "$(INTDIR)\iluserver.obj"

"$(ILUHOME)\bin\ilucpp2.dll" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
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

SOURCE=$(ILUSRC)\runtime\cpp2\iluserver.cpp
DEP_CPP_ILUSE=\
    "$(ILUSRC)\runtime\cpp2\ilu.hpp"\
    "$(ILUSRC)\runtime\kernel\ilubasic.h"\
    "$(ILUSRC)\runtime\kernel\iludebug.h"\
    "$(ILUSRC)\runtime\kernel\iluerror.h"\
    "$(ILUSRC)\runtime\kernel\iluerrs.h"\
    "$(ILUSRC)\runtime\kernel\iluhash.h"\
    "$(ILUSRC)\runtime\kernel\iluntrnl.h"\
    "$(ILUSRC)\runtime\kernel\ilutypes.h"\
    "$(ILUSRC)\runtime\kernel\iluwin.h"\
    "$(ILUSRC)\runtime\kernel\iluxport.h"
    

!IF  "$(CFG)" == "Win32 Release"


"$(INTDIR)\iluserver.obj" : $(SOURCE) $(DEP_CPP_ILUSE) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"


BuildCmds= \
    $(CPP) $(CPP_PROJ) $(SOURCE) \
    

"$(INTDIR)\iluserver.obj" : $(SOURCE) $(DEP_CPP_ILUSE) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\iluserver.sbr" : $(SOURCE) $(DEP_CPP_ILUSE) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=$(ILUSRC)\runtime\cpp2\ilucppinternal.cpp

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_ILUCP=\
    "$(ILUSRC)\runtime\cpp2\corba-templates.hpp"\
    "$(ILUSRC)\runtime\cpp2\corba.hpp"\
    "$(ILUSRC)\stubbers\cpp2\cppportability.hpp"\
    "$(ILUSRC)\runtime\cpp2\ilu.hpp"\
    "$(ILUSRC)\runtime\kernel\ilubasic.h"\
    "$(ILUSRC)\runtime\kernel\iludebug.h"\
    "$(ILUSRC)\runtime\kernel\iluerror.h"\
    "$(ILUSRC)\runtime\kernel\iluerrs.h"\
    "$(ILUSRC)\runtime\kernel\iluhash.h"\
    "$(ILUSRC)\runtime\kernel\iluntrnl.h"\
    "$(ILUSRC)\runtime\kernel\ilutypes.h"\
    "$(ILUSRC)\runtime\kernel\iluwin.h"\
    "$(ILUSRC)\runtime\kernel\iluxport.h"\
    

"$(INTDIR)\ilucppinternal.obj" : $(SOURCE) $(DEP_CPP_ILUCP) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_ILUCP=\
    "$(ILUSRC)\runtime\cpp2\corba-templates.hpp"\
    "$(ILUSRC)\runtime\cpp2\corba.hpp"\
    "$(ILUSRC)\stubbers\cpp2\cppportability.hpp"\
    "$(ILUSRC)\runtime\cpp2\ilu.hpp"\
    "$(ILUSRC)\runtime\kernel\ilubasic.h"\
    "$(ILUSRC)\runtime\kernel\iludebug.h"\
    "$(ILUSRC)\runtime\kernel\iluerror.h"\
    "$(ILUSRC)\runtime\kernel\iluerrs.h"\
    "$(ILUSRC)\runtime\kernel\iluhash.h"\
    "$(ILUSRC)\runtime\kernel\iluntrnl.h"\
    "$(ILUSRC)\runtime\kernel\ilutypes.h"\
    "$(ILUSRC)\runtime\kernel\iluwin.h"\
    "$(ILUSRC)\runtime\kernel\iluxport.h"\
    
NODEP_CPP_ILUCP=\
    "$(ILUSRC)\runtime\cpp2\CORBA_ERRTYP"\
    

BuildCmds= \
    $(CPP) $(CPP_PROJ) $(SOURCE) \
    

"$(INTDIR)\ilucppinternal.obj" : $(SOURCE) $(DEP_CPP_ILUCP) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\ilucppinternal.sbr" : $(SOURCE) $(DEP_CPP_ILUCP) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=$(ILUSRC)\runtime\cpp2\ilucppruntime.cpp
DEP_CPP_ILUCPP=\
    "$(ILUSRC)\runtime\cpp2\ilu.hpp"\
    "$(ILUSRC)\runtime\kernel\ilubasic.h"\
    "$(ILUSRC)\runtime\kernel\iludebug.h"\
    "$(ILUSRC)\runtime\kernel\iluerror.h"\
    "$(ILUSRC)\runtime\kernel\iluerrs.h"\
    "$(ILUSRC)\runtime\kernel\iluhash.h"\
    "$(ILUSRC)\runtime\kernel\iluntrnl.h"\
    "$(ILUSRC)\runtime\kernel\ilutypes.h"\
    "$(ILUSRC)\runtime\kernel\iluwin.h"\
    "$(ILUSRC)\runtime\kernel\iluxport.h"\
    

!IF  "$(CFG)" == "Win32 Release"


"$(INTDIR)\ilucppruntime.obj" : $(SOURCE) $(DEP_CPP_ILUCPP) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"


BuildCmds= \
    $(CPP) $(CPP_PROJ) $(SOURCE) \
    

"$(INTDIR)\ilucppruntime.obj" : $(SOURCE) $(DEP_CPP_ILUCPP) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\ilucppruntime.sbr" : $(SOURCE) $(DEP_CPP_ILUCPP) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=$(ILUSRC)\runtime\cpp2\ilumainloop.cpp
DEP_CPP_ILUMA=\
    "$(ILUSRC)\runtime\cpp2\ilu.hpp"\
    "$(ILUSRC)\runtime\kernel\ilubasic.h"\
    "$(ILUSRC)\runtime\kernel\iludebug.h"\
    "$(ILUSRC)\runtime\kernel\iluerror.h"\
    "$(ILUSRC)\runtime\kernel\iluerrs.h"\
    "$(ILUSRC)\runtime\kernel\iluhash.h"\
    "$(ILUSRC)\runtime\kernel\iluntrnl.h"\
    "$(ILUSRC)\runtime\kernel\ilutypes.h"\
    "$(ILUSRC)\runtime\kernel\iluwin.h"\
    "$(ILUSRC)\runtime\kernel\iluxport.h"\
    

!IF  "$(CFG)" == "Win32 Release"


"$(INTDIR)\ilumainloop.obj" : $(SOURCE) $(DEP_CPP_ILUMA) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"


BuildCmds= \
    $(CPP) $(CPP_PROJ) $(SOURCE) \
    

"$(INTDIR)\ilumainloop.obj" : $(SOURCE) $(DEP_CPP_ILUMA) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\ilumainloop.sbr" : $(SOURCE) $(DEP_CPP_ILUMA) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=$(ILUSRC)\runtime\cpp2\iluobject.cpp
DEP_CPP_ILUOB=\
    "$(ILUSRC)\runtime\cpp2\ilu.hpp"\
    "$(ILUSRC)\runtime\kernel\ilubasic.h"\
    "$(ILUSRC)\runtime\kernel\iludebug.h"\
    "$(ILUSRC)\runtime\kernel\iluerror.h"\
    "$(ILUSRC)\runtime\kernel\iluerrs.h"\
    "$(ILUSRC)\runtime\kernel\iluhash.h"\
    "$(ILUSRC)\runtime\kernel\iluntrnl.h"\
    "$(ILUSRC)\runtime\kernel\ilutypes.h"\
    "$(ILUSRC)\runtime\kernel\iluwin.h"\
    "$(ILUSRC)\runtime\kernel\iluxport.h"\
    

!IF  "$(CFG)" == "Win32 Release"


"$(INTDIR)\iluobject.obj" : $(SOURCE) $(DEP_CPP_ILUOB) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"


BuildCmds= \
    $(CPP) $(CPP_PROJ) $(SOURCE) \
    

"$(INTDIR)\iluobject.obj" : $(SOURCE) $(DEP_CPP_ILUOB) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\iluobject.sbr" : $(SOURCE) $(DEP_CPP_ILUOB) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=$(ILUSRC)\runtime\cpp2\iluobjecttable.cpp
DEP_CPP_ILUOBJ=\
    "$(ILUSRC)\runtime\cpp2\ilu.hpp"\
    "$(ILUSRC)\runtime\kernel\ilubasic.h"\
    "$(ILUSRC)\runtime\kernel\iludebug.h"\
    "$(ILUSRC)\runtime\kernel\iluerror.h"\
    "$(ILUSRC)\runtime\kernel\iluerrs.h"\
    "$(ILUSRC)\runtime\kernel\iluhash.h"\
    "$(ILUSRC)\runtime\kernel\iluntrnl.h"\
    "$(ILUSRC)\runtime\kernel\ilutypes.h"\
    "$(ILUSRC)\runtime\kernel\iluwin.h"\
    "$(ILUSRC)\runtime\kernel\iluxport.h"\
    

!IF  "$(CFG)" == "Win32 Release"


"$(INTDIR)\iluobjecttable.obj" : $(SOURCE) $(DEP_CPP_ILUOBJ) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"


BuildCmds= \
    $(CPP) $(CPP_PROJ) $(SOURCE) \
    

"$(INTDIR)\iluobjecttable.obj" : $(SOURCE) $(DEP_CPP_ILUOBJ) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\iluobjecttable.sbr" : $(SOURCE) $(DEP_CPP_ILUOBJ) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=$(ILUSRC)\runtime\cpp2\ilusecurity.cpp
DEP_CPP_ILUOBJ=\
    "$(ILUSRC)\runtime\cpp2\ilu.hpp"\
    "$(ILUSRC)\runtime\kernel\ilubasic.h"\
    "$(ILUSRC)\runtime\kernel\iludebug.h"\
    "$(ILUSRC)\runtime\kernel\iluerror.h"\
    "$(ILUSRC)\runtime\kernel\iluerrs.h"\
    "$(ILUSRC)\runtime\kernel\iluhash.h"\
    "$(ILUSRC)\runtime\kernel\iluntrnl.h"\
    "$(ILUSRC)\runtime\kernel\ilutypes.h"\
    "$(ILUSRC)\runtime\kernel\iluwin.h"\
    "$(ILUSRC)\runtime\kernel\iluxport.h"\
    

!IF  "$(CFG)" == "Win32 Release"


"$(INTDIR)\ilusecurity.obj" : $(SOURCE) $(DEP_CPP_ILUOBJ) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"


BuildCmds= \
    $(CPP) $(CPP_PROJ) $(SOURCE) \
    

"$(INTDIR)\ilusecurity.obj" : $(SOURCE) $(DEP_CPP_ILUOBJ) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\ilusecurity.sbr" : $(SOURCE) $(DEP_CPP_ILUOBJ) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=$(ILUSRC)\runtime\cpp2\ilucall.cpp
DEP_CPP_ILUCA=\
    "$(ILUSRC)\runtime\cpp2\ilu.hpp"\
    "$(ILUSRC)\runtime\kernel\ilubasic.h"\
    "$(ILUSRC)\runtime\kernel\iludebug.h"\
    "$(ILUSRC)\runtime\kernel\iluerror.h"\
    "$(ILUSRC)\runtime\kernel\iluerrs.h"\
    "$(ILUSRC)\runtime\kernel\iluhash.h"\
    "$(ILUSRC)\runtime\kernel\iluntrnl.h"\
    "$(ILUSRC)\runtime\kernel\ilutypes.h"\
    "$(ILUSRC)\runtime\kernel\iluwin.h"\
    "$(ILUSRC)\runtime\kernel\iluxport.h"\
    

!IF  "$(CFG)" == "Win32 Release"


"$(INTDIR)\ilucall.obj" : $(SOURCE) $(DEP_CPP_ILUCA) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"


BuildCmds= \
    $(CPP) $(CPP_PROJ) $(SOURCE) \
    

"$(INTDIR)\ilucall.obj" : $(SOURCE) $(DEP_CPP_ILUCA) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\ilucall.sbr" : $(SOURCE) $(DEP_CPP_ILUCA) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=$(ILUSRC)\runtime\cpp2\corba.cpp
DEP_CPP_CORBA=\
    "$(ILUSRC)\runtime\cpp2\corba-templates.hpp"\
    "$(ILUSRC)\runtime\cpp2\corba.hpp"\
    "$(ILUSRC)\stubbers\cpp2\cppportability.hpp"\
    "$(ILUSRC)\runtime\cpp2\ilu.hpp"\
    "$(ILUSRC)\runtime\kernel\ilubasic.h"\
    "$(ILUSRC)\runtime\kernel\iludebug.h"\
    "$(ILUSRC)\runtime\kernel\iluerror.h"\
    "$(ILUSRC)\runtime\kernel\iluerrs.h"\
    "$(ILUSRC)\runtime\kernel\iluhash.h"\
    "$(ILUSRC)\runtime\kernel\iluntrnl.h"\
    "$(ILUSRC)\runtime\kernel\ilutypes.h"\
    "$(ILUSRC)\runtime\kernel\iluwin.h"\
    "$(ILUSRC)\runtime\kernel\iluxport.h"\
    

!IF  "$(CFG)" == "Win32 Release"


"$(INTDIR)\corba.obj" : $(SOURCE) $(DEP_CPP_CORBA) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"


BuildCmds= \
    $(CPP) $(CPP_PROJ) $(SOURCE) \
    

"$(INTDIR)\corba.obj" : $(SOURCE) $(DEP_CPP_CORBA) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\corba.sbr" : $(SOURCE) $(DEP_CPP_CORBA) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=$(ILUSRC)\runtime\cpp2\ilugarbagecollect.cpp
DEP_CPP_ILUGA=\
    "$(ILUSRC)\runtime\cpp2\ilu.hpp"\
    "$(ILUSRC)\runtime\kernel\ilubasic.h"\
    "$(ILUSRC)\runtime\kernel\iludebug.h"\
    "$(ILUSRC)\runtime\kernel\iluerror.h"\
    "$(ILUSRC)\runtime\kernel\iluerrs.h"\
    "$(ILUSRC)\runtime\kernel\iluhash.h"\
    "$(ILUSRC)\runtime\kernel\iluntrnl.h"\
    "$(ILUSRC)\runtime\kernel\ilutypes.h"\
    "$(ILUSRC)\runtime\kernel\iluwin.h"\
    "$(ILUSRC)\runtime\kernel\iluxport.h"\
    

!IF  "$(CFG)" == "Win32 Release"


"$(INTDIR)\ilugarbagecollect.obj" : $(SOURCE) $(DEP_CPP_ILUGA) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"


BuildCmds= \
    $(CPP) $(CPP_PROJ) $(SOURCE) \
    

"$(INTDIR)\ilugarbagecollect.obj" : $(SOURCE) $(DEP_CPP_ILUGA) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\ilugarbagecollect.sbr" : $(SOURCE) $(DEP_CPP_ILUGA) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=$(ILUSRC)\runtime\cpp2\iluactivation.cpp
DEP_CPP_ILUAC=\
    "$(ILUSRC)\runtime\cpp2\ilu.hpp"\
    "$(ILUSRC)\runtime\kernel\ilubasic.h"\
    "$(ILUSRC)\runtime\kernel\iludebug.h"\
    "$(ILUSRC)\runtime\kernel\iluerror.h"\
    "$(ILUSRC)\runtime\kernel\iluerrs.h"\
    "$(ILUSRC)\runtime\kernel\iluhash.h"\
    "$(ILUSRC)\runtime\kernel\iluntrnl.h"\
    "$(ILUSRC)\runtime\kernel\ilutypes.h"\
    "$(ILUSRC)\runtime\kernel\iluwin.h"\
    "$(ILUSRC)\runtime\kernel\iluxport.h"\
    

!IF  "$(CFG)" == "Win32 Release"


"$(INTDIR)\iluactivation.obj" : $(SOURCE) $(DEP_CPP_ILUAC) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"


BuildCmds= \
    $(CPP) $(CPP_PROJ) $(SOURCE) \
    

"$(INTDIR)\iluactivation.obj" : $(SOURCE) $(DEP_CPP_ILUAC) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\iluactivation.sbr" : $(SOURCE) $(DEP_CPP_ILUAC) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
# End Target
# End Project
################################################################################
