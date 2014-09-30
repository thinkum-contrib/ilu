# Microsoft Visual C++ Generated NMAKE File, Format Version 2.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

!IF "$(CFG)" == ""
CFG=Win32 Debug
!MESSAGE No configuration specified.  Defaulting to Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "Win32 Release" && "$(CFG)" != "Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE on this makefile
!MESSAGE by defining the macro CFG on the command line.  For example:
!MESSAGE 
!MESSAGE NMAKE /f "srvr.mak" CFG="Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

################################################################################
# Begin Project
# PROP Target_Last_Scanned "Win32 Release"
CPP=cl.exe
RSC=rc.exe

!IF  "$(CFG)" == "Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "WinRel"
# PROP BASE Intermediate_Dir "WinRel"
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "WinRel"
# PROP Intermediate_Dir "WinRel"
OUTDIR=.\WinRel
INTDIR=.\WinRel

ALL : $(OUTDIR)/server.exe $(OUTDIR)/srvr.bsc

$(OUTDIR) : 
    if not exist $(OUTDIR)/nul mkdir $(OUTDIR)

# ADD BASE CPP /nologo /W3 /GX /YX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /FR /c
# ADD CPP /nologo /MD /W3 /GX /YX /Ox /Oy- /I "\ilu\src\runtime\c" /I "\ilu\src\runtime\kernel" /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /c
# SUBTRACT CPP /Fr
CPP_PROJ=/nologo /MD /W3 /GX /YX /Ox /Oy- /I $(ILUHOME)\include \
  /D "WIN32" /D "NDEBUG" /D "_CONSOLE"\
 /Fp$(OUTDIR)/"srvr.pch" /Fo$(INTDIR)/ /c 
CPP_OBJS=.\WinRel/
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
BSC32_SBRS= \
    
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o$(OUTDIR)/"srvr.bsc" 

$(OUTDIR)/srvr.bsc : $(OUTDIR)  $(BSC32_SBRS)
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /NOLOGO /SUBSYSTEM:console /MACHINE:I386
# ADD LINK32 \ilu\lib\iluc32.lib \ilu\lib\ilu32.lib wsock32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /NOLOGO /SUBSYSTEM:console /MACHINE:I386
LINK32_FLAGS=$(ILUHOME)\lib\iluc32.lib $(ILUHOME)\lib\ilu32.lib $(ILUHOME)\lib\gss.lib wsock32.lib kernel32.lib\
 user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib\
 ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /NOLOGO\
 /SUBSYSTEM:console /INCREMENTAL:no /PDB:$(OUTDIR)/"srvr.pdb" /MACHINE:I386\
 /OUT:$(OUTDIR)/"server.exe" 
DEF_FILE=
LINK32_OBJS= \
    $(INTDIR)/srvr.obj \
    $(INTDIR)/srvru.obj \
    $(INTDIR)/Test3-common.obj \
    $(INTDIR)/Test3-true.obj \
    $(INTDIR)/Test1-true.obj \
    $(INTDIR)/Test1-common.obj \
    $(INTDIR)/Test2-common.obj

$(OUTDIR)/server.exe : $(OUTDIR)  $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "WinDebug"
# PROP BASE Intermediate_Dir "WinDebug"
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "WinDebug"
# PROP Intermediate_Dir "WinDebug"
OUTDIR=.\WinDebug
INTDIR=.\WinDebug

ALL : $(OUTDIR)/server.exe $(OUTDIR)/srvr.bsc

$(OUTDIR) : 
    if not exist $(OUTDIR)/nul mkdir $(OUTDIR)

# ADD BASE CPP /nologo /W3 /GX /Zi /YX /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /FR /c
# ADD CPP /nologo /MD /W3 /GX /Zi /YX /Od /I "\ilu\src\runtime\c" /I "\ilu\src\runtime\kernel" /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /c
# SUBTRACT CPP /Fr
CPP_PROJ=/nologo /MD /W3 /GX /Zi /YX /Od /I $(ILUHOME)\include \
  /D "WIN32" /D "_DEBUG" /D "_CONSOLE"\
 /Fp$(OUTDIR)/"srvr.pch" /Fo$(INTDIR)/ /Fd$(OUTDIR)/"srvr.pdb" /c 
CPP_OBJS=.\WinDebug/
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
BSC32_SBRS= \
    
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o$(OUTDIR)/"srvr.bsc" 

$(OUTDIR)/srvr.bsc : $(OUTDIR)  $(BSC32_SBRS)
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /NOLOGO /SUBSYSTEM:console /DEBUG /MACHINE:I386
# ADD LINK32 \ilu\lib\iluc32.lib \ilu\lib\ilu32.lib wsock32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /NOLOGO /SUBSYSTEM:console /DEBUG /MACHINE:I386 /OUT:"WinDebug/server.exe"
LINK32_FLAGS=$(ILUHOME)\lib\iluc32.lib $(ILUHOME)\lib\ilu32.lib $(ILUHOME)\lib\gss.lib wsock32.lib kernel32.lib\
 user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib\
 ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /NOLOGO\
 /SUBSYSTEM:console /INCREMENTAL:yes /PDB:$(OUTDIR)/"srvr.pdb" /DEBUG\
 /MACHINE:I386 /OUT:"WinDebug/server.exe" 
DEF_FILE=
LINK32_OBJS= \
    $(INTDIR)/srvr.obj \
    $(INTDIR)/srvru.obj \
    $(INTDIR)/Test3-common.obj \
    $(INTDIR)/Test3-true.obj \
    $(INTDIR)/Test1-true.obj \
    $(INTDIR)/Test1-common.obj \
    $(INTDIR)/Test2-common.obj

$(OUTDIR)/server.exe : $(OUTDIR)  $(DEF_FILE) $(LINK32_OBJS)
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

SOURCE=.\srvr.c
DEP_SRVR_=\
    .\Test1.h\
    .\Test3.h\
    $(ILUHOME)\include\iluchdrs.h\
    .\Test2.h\
    $(ILUHOME)\include\iluxport.h\
    $(ILUHOME)\include\iluerror.h\
    $(ILUHOME)\include\ilubasic.h

$(INTDIR)/srvr.obj :  $(SOURCE)  $(DEP_SRVR_) $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\srvru.c
DEP_SRVRU_=\
    .\srvr.h

$(INTDIR)/srvru.obj :  $(SOURCE)  $(DEP_SRVRU_) $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=".\Test3-common.c"
DEP_TEST3=\
    .\Test3.h\
    .\Test1.h\
    .\Test2.h\
    $(ILUHOME)\include\iluchdrs.h\
    $(ILUHOME)\include\iluxport.h\
    $(ILUHOME)\include\iluerror.h\
    $(ILUHOME)\include\ilubasic.h

$(INTDIR)/Test3-common.obj :  $(SOURCE)  $(DEP_TEST3) $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=".\Test3-true.c"
DEP_TEST3_=\
    .\Test3.h\
    .\Test1.h\
    .\Test2.h\
    $(ILUHOME)\include\iluchdrs.h\
    $(ILUHOME)\include\iluxport.h\
    $(ILUHOME)\include\iluerror.h\
    $(ILUHOME)\include\ilubasic.h

$(INTDIR)/Test3-true.obj :  $(SOURCE)  $(DEP_TEST3_) $(INTDIR)

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
# End Group
# End Project
################################################################################
