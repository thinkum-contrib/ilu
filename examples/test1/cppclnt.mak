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
!MESSAGE NMAKE /f "cppclnt.mak" CFG="Win32 Debug"
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

ALL : $(OUTDIR)/cppclnt.exe $(OUTDIR)/cppclnt.bsc

$(OUTDIR) : 
    if not exist $(OUTDIR)/nul mkdir $(OUTDIR)

# ADD BASE CPP /nologo /W3 /GX /YX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /FR /c
# ADD CPP /nologo /MD /W3 /GX /YX /Ox /Oy- /I "f:\ilu\src\runtime\c++" /I "f:\ilu\src\runtime\c" /I "f:\ilu\src\runtime\kernel" /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /c
# SUBTRACT CPP /Fr
CPP_PROJ=/nologo /MD /W3 /GX /YX /Ox /Oy- /I $(ILUHOME)\include \
  /D "WIN32" /D "NDEBUG" /D\
 "_CONSOLE" /Fp$(OUTDIR)/"cppclnt.pch" /Fo$(INTDIR)/ /c 
CPP_OBJS=.\WinRel/
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
BSC32_SBRS= \
	
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o$(OUTDIR)/"cppclnt.bsc" 

$(OUTDIR)/cppclnt.bsc : $(OUTDIR)  $(BSC32_SBRS)
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /NOLOGO /SUBSYSTEM:console /MACHINE:I386
# ADD LINK32 f:\ilu\lib\ilucpp32.lib f:\ilu\lib\ilu32.lib wsock32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /NOLOGO /SUBSYSTEM:console /MACHINE:I386
LINK32_FLAGS=$(ILUHOME)\lib\ilucpp32.lib $(ILUHOME)\lib\ilu32.lib wsock32.lib\
 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib\
 shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /NOLOGO\
 /SUBSYSTEM:console /INCREMENTAL:no /PDB:$(OUTDIR)/"cppclnt.pdb" /MACHINE:I386\
 /OUT:$(OUTDIR)/"cppclnt.exe" 
DEF_FILE=
LINK32_OBJS= \
	$(INTDIR)/cppclnt.obj \
	$(INTDIR)/Test3.obj \
	$(INTDIR)/Test2.obj \
	$(INTDIR)/Test1.obj

$(OUTDIR)/cppclnt.exe : $(OUTDIR)  $(DEF_FILE) $(LINK32_OBJS)
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

ALL : $(OUTDIR)/cppclnt.exe $(OUTDIR)/cppclnt.bsc

$(OUTDIR) : 
    if not exist $(OUTDIR)/nul mkdir $(OUTDIR)

# ADD BASE CPP /nologo /W3 /GX /Zi /YX /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /FR /c
# ADD CPP /nologo /MD /W3 /GX /Zi /YX /Od /I "f:\ilu\src\runtime\c++" /I "f:\ilu\src\runtime\c" /I "f:\ilu\src\runtime\kernel" /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /c
# SUBTRACT CPP /Fr
CPP_PROJ=/nologo /MD /W3 /GX /Zi /YX /Od /I $(ILUHOME)\include \
  /D "WIN32" /D "_DEBUG" /D\
 "_CONSOLE" /Fp$(OUTDIR)/"cppclnt.pch" /Fo$(INTDIR)/ /Fd$(OUTDIR)/"cppclnt.pdb"\
 /c 
CPP_OBJS=.\WinDebug/
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
BSC32_SBRS= \
	
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o$(OUTDIR)/"cppclnt.bsc" 

$(OUTDIR)/cppclnt.bsc : $(OUTDIR)  $(BSC32_SBRS)
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /NOLOGO /SUBSYSTEM:console /DEBUG /MACHINE:I386
# ADD LINK32 f:\ilu\lib\ilucpp32.lib f:\ilu\lib\ilu32.lib wsock32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /NOLOGO /SUBSYSTEM:console /DEBUG /MACHINE:I386
LINK32_FLAGS=$(ILUHOME)\lib\ilucpp32.lib $(ILUHOME)\lib\ilu32.lib wsock32.lib\
 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib\
 shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /NOLOGO\
 /SUBSYSTEM:console /INCREMENTAL:yes /PDB:$(OUTDIR)/"cppclnt.pdb" /DEBUG\
 /MACHINE:I386 /OUT:$(OUTDIR)/"cppclnt.exe" 
DEF_FILE=
LINK32_OBJS= \
	$(INTDIR)/cppclnt.obj \
	$(INTDIR)/Test3.obj \
	$(INTDIR)/Test2.obj \
	$(INTDIR)/Test1.obj

$(OUTDIR)/cppclnt.exe : $(OUTDIR)  $(DEF_FILE) $(LINK32_OBJS)
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

SOURCE=.\cppclnt.cpp
DEP_CPPCL=\
	.\Test1.hh\
	.\Test2.hh\
	.\Test3.hh\
	$(ILUHOME)\include\ilu.hh\
	$(ILUHOME)\include\iluxport.h\
	$(ILUHOME)\include\iluerror.h\
	$(ILUHOME)\include\ilubasic.h

$(INTDIR)/cppclnt.obj :  $(SOURCE)  $(DEP_CPPCL) $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\Test3.cpp
DEP_TEST3=\
	$(ILUHOME)\include\ilu.hh\
	.\Test3.hh\
	$(ILUHOME)\include\iluxport.h\
	.\Test1.hh\
	.\Test2.hh\
	$(ILUHOME)\include\iluerror.h\
	$(ILUHOME)\include\ilubasic.h

$(INTDIR)/Test3.obj :  $(SOURCE)  $(DEP_TEST3) $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\Test2.cpp
DEP_TEST2=\
	$(ILUHOME)\include\ilu.hh\
	.\Test2.hh\
	$(ILUHOME)\include\iluxport.h\
	.\Test1.hh\
	$(ILUHOME)\include\iluerror.h\
	$(ILUHOME)\include\ilubasic.h

$(INTDIR)/Test2.obj :  $(SOURCE)  $(DEP_TEST2) $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\Test1.cpp
DEP_TEST1=\
	$(ILUHOME)\include\ilu.hh\
	.\Test1.hh\
	$(ILUHOME)\include\iluxport.h\
	$(ILUHOME)\include\iluerror.h\
	$(ILUHOME)\include\ilubasic.h

$(INTDIR)/Test1.obj :  $(SOURCE)  $(DEP_TEST1) $(INTDIR)

# End Source File
# End Group
# End Project
################################################################################
