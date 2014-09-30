# Microsoft Visual C++ Generated NMAKE File, Format Version 2.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Application" 0x0101

!IF "$(CFG)" == ""
CFG=Win32 Debug
!MESSAGE No configuration specified.  Defaulting to Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "Win32 Release" && "$(CFG)" != "Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE on this makefile
!MESSAGE by defining the macro CFG on the command line.  For example:
!MESSAGE 
!MESSAGE NMAKE /f "srvrw.mak" CFG="Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "Win32 Release" (based on "Win32 (x86) Application")
!MESSAGE "Win32 Debug" (based on "Win32 (x86) Application")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

################################################################################
# Begin Project
# PROP Target_Last_Scanned "Win32 Release"
MTL=MkTypLib.exe
CPP=cl.exe
RSC=rc.exe

!IF  "$(CFG)" == "Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "WinRelW"
# PROP BASE Intermediate_Dir "WinRelW"
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "WinRelW"
# PROP Intermediate_Dir "WinRelW"
OUTDIR=.\WinRelW
INTDIR=.\WinRelW

ALL : $(OUTDIR)/serverw.exe $(OUTDIR)/srvrw.bsc

$(OUTDIR) : 
    if not exist $(OUTDIR)/nul mkdir $(OUTDIR)

# ADD BASE MTL /nologo /D "NDEBUG" /win32
# ADD MTL /nologo /D "NDEBUG" /win32
MTL_PROJ=/nologo /D "NDEBUG" /win32 
# ADD BASE CPP /nologo /W3 /GX /YX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /FR /c
# ADD CPP /nologo /MD /W3 /GX /YX /Ox /Oy- /I "\ilu\src\runtime\c" /I "\ilu\src\runtime\kernel" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /c
# SUBTRACT CPP /Fr
CPP_PROJ=/nologo /MD /W3 /GX /YX /Ox /Oy- /I $(ILUHOME)\include \
  /D "WIN32" /D "NDEBUG" /D\
 "_WINDOWS" /Fp$(OUTDIR)/"srvrw.pch" /Fo$(INTDIR)/ /c 
CPP_OBJS=.\WinRelW/
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
RSC_PROJ=/l 0x409 /fo$(INTDIR)/"srvrw.res" /d "NDEBUG" 
BSC32=bscmake.exe
BSC32_SBRS= \
    
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o$(OUTDIR)/"srvrw.bsc" 

$(OUTDIR)/srvrw.bsc : $(OUTDIR)  $(BSC32_SBRS)
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /NOLOGO /SUBSYSTEM:windows /MACHINE:I386
# ADD LINK32 \ilu\lib\iluc32.lib \ilu\lib\ilu32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /NOLOGO /SUBSYSTEM:windows /MACHINE:I386
LINK32_FLAGS=$(ILUHOME)\lib\iluc32.lib $(ILUHOME)\lib\ilu32.lib $(ILUHOME)\lib\gss.lib\
 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib\
 shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /NOLOGO\
 /SUBSYSTEM:windows /INCREMENTAL:no /PDB:$(OUTDIR)/"srvrw.pdb" /MACHINE:I386\
 /OUT:$(OUTDIR)/"serverw.exe" 
DEF_FILE=
LINK32_OBJS= \
    $(INTDIR)/Test3-true.obj \
    $(INTDIR)/Test1-true.obj \
    $(INTDIR)/Test1-common.obj \
    $(INTDIR)/Test2-common.obj \
    $(INTDIR)/Test3-common.obj \
    $(INTDIR)/srvrw.obj \
    $(INTDIR)/srvr.obj \
    $(INTDIR)/srvrw.res \
    $(INTDIR)/msgalarm.obj

$(OUTDIR)/serverw.exe : $(OUTDIR)  $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "WinDebugW"
# PROP BASE Intermediate_Dir "WinDebugW"
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "WinDebugW"
# PROP Intermediate_Dir "WinDebugW"
OUTDIR=.\WinDebugW
INTDIR=.\WinDebugW

ALL : $(OUTDIR)/serverw.exe $(OUTDIR)/srvrw.bsc

$(OUTDIR) : 
    if not exist $(OUTDIR)/nul mkdir $(OUTDIR)

# ADD BASE MTL /nologo /D "_DEBUG" /win32
# ADD MTL /nologo /D "_DEBUG" /win32
MTL_PROJ=/nologo /D "_DEBUG" /win32 
# ADD BASE CPP /nologo /W3 /GX /Zi /YX /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /FR /c
# ADD CPP /nologo /MD /W3 /GX /Zi /YX /Od /I "\ilu\src\runtime\c" /I "\ilu\src\runtime\kernel" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /c
# SUBTRACT CPP /Fr
CPP_PROJ=/nologo /MD /W3 /GX /Zi /YX /Od /I $(ILUHOME)\include \
  /D "WIN32" /D "_DEBUG" /D\
 "_WINDOWS" /Fp$(OUTDIR)/"srvrw.pch" /Fo$(INTDIR)/\
 /Fd$(OUTDIR)/"srvrw.pdb" /c 
CPP_OBJS=.\WinDebugW/
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
RSC_PROJ=/l 0x409 /fo$(INTDIR)/"srvrw.res" /d "_DEBUG" 
BSC32=bscmake.exe
BSC32_SBRS= \
    
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o$(OUTDIR)/"srvrw.bsc" 

$(OUTDIR)/srvrw.bsc : $(OUTDIR)  $(BSC32_SBRS)
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /NOLOGO /SUBSYSTEM:windows /DEBUG /MACHINE:I386
# ADD LINK32 \ilu\lib\iluc32.lib \ilu\lib\ilu32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /NOLOGO /SUBSYSTEM:windows /DEBUG /MACHINE:I386 /OUT:"WinDebugW/serverw.exe"
LINK32_FLAGS=$(ILUHOME)\lib\iluc32.lib $(ILUHOME)\lib\ilu32.lib $(ILUHOME)\lib\gss.lib \
 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib\
 shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /NOLOGO\
 /SUBSYSTEM:windows /INCREMENTAL:yes /PDB:$(OUTDIR)/"srvrw.pdb" /DEBUG\
 /MACHINE:I386 /OUT:"WinDebugW/serverw.exe" 
DEF_FILE=
LINK32_OBJS= \
    $(INTDIR)/Test3-true.obj \
    $(INTDIR)/Test1-true.obj \
    $(INTDIR)/Test1-common.obj \
    $(INTDIR)/Test2-common.obj \
    $(INTDIR)/Test3-common.obj \
    $(INTDIR)/srvrw.obj \
    $(INTDIR)/srvr.obj \
    $(INTDIR)/srvrw.res \
    $(INTDIR)/msgalarm.obj

$(OUTDIR)/serverw.exe : $(OUTDIR)  $(DEF_FILE) $(LINK32_OBJS)
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

################################################################################
# Begin Group "Source Files"

################################################################################
# Begin Source File

SOURCE=".\Test3-true.c"
DEP_TEST3=\
    .\Test3.h\
    .\Test1.h\
    .\Test2.h\
    $(ILUHOME)\include\iluchdrs.h\
    $(ILUHOME)\include\iluxport.h\
    $(ILUHOME)\include\iluerror.h\
    $(ILUHOME)\include\ilubasic.h

$(INTDIR)/Test3-true.obj :  $(SOURCE)  $(DEP_TEST3) $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=".\Test1-true.c"
DEP_TEST1=\
    .\Test1.h\
    $(ILUHOME)\include\iluchdrs.h\
    $(ILUHOME)\include\iluxport.h\
    $(ILUHOME)\include\iluerror.h\
    $(ILUHOME)\include\ilubasic.h

$(INTDIR)/Test1-true.obj :  $(SOURCE)  $(DEP_TEST1) $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=".\Test1-common.c"
DEP_TEST1_=\
    .\Test1.h\
    $(ILUHOME)\include\iluchdrs.h\
    $(ILUHOME)\include\iluxport.h\
    $(ILUHOME)\include\iluerror.h\
    $(ILUHOME)\include\ilubasic.h

$(INTDIR)/Test1-common.obj :  $(SOURCE)  $(DEP_TEST1_) $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=".\Test2-common.c"
DEP_TEST2=\
    .\Test2.h\
    .\Test1.h\
    $(ILUHOME)\include\iluchdrs.h\
    $(ILUHOME)\include\iluxport.h\
    $(ILUHOME)\include\iluerror.h\
    $(ILUHOME)\include\ilubasic.h

$(INTDIR)/Test2-common.obj :  $(SOURCE)  $(DEP_TEST2) $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=".\Test3-common.c"
DEP_TEST3_=\
    .\Test3.h\
    .\Test1.h\
    .\Test2.h\
    $(ILUHOME)\include\iluchdrs.h\
    $(ILUHOME)\include\iluxport.h\
    $(ILUHOME)\include\iluerror.h\
    $(ILUHOME)\include\ilubasic.h

$(INTDIR)/Test3-common.obj :  $(SOURCE)  $(DEP_TEST3_) $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\srvr.c
DEP_SRVR=\
    .\Test1.h\
    .\Test3.h\
    $(ILUHOME)\include\iluchdrs.h\
    .\Test2.h\
    $(ILUHOME)\include\iluxport.h\
    $(ILUHOME)\include\iluerror.h\
    $(ILUHOME)\include\ilubasic.h

$(INTDIR)/srvr.obj :  $(SOURCE)  $(DEP_SRVR) $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\srvrw.c
DEP_SRVRW=\
    .\srvr.h

$(INTDIR)/srvrw.obj : $(SOURCE)  $(DEP_SRVRW) $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\srvrw.rc
DEP_SRVRW_=\
    .\srvrw.ico\
    .\svrconsl.ico

$(INTDIR)/srvrw.res :  $(SOURCE)  $(DEP_SRVRW_) $(INTDIR)
   $(RSC) $(RSC_PROJ)  $(SOURCE) 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\msgalarm.c
DEP_MSGAL=\
    $(ILUHOME)\include\iluxport.h\
    $(ILUHOME)\include\iluerror.h\
    $(ILUHOME)\include\ilubasic.h

$(INTDIR)/msgalarm.obj :  $(SOURCE)  $(DEP_MSGAL) $(INTDIR)

# End Source File
# End Group
# End Project
################################################################################
