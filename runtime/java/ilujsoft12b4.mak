# Microsoft Developer Studio Generated NMAKE File, Based on ilujava.dsp
# This used to be called ilujsoft12.mak and used to work with jdk1.2beta4
# but since has been replaced.  Keep this in case somebody needs to go back.
JDK_OINCL =  $(JDK_HOME)\include-old
JDK_MDOINCL =  $(JDK_HOME)\include-old\win32
JDK_NINCL =  $(JDK_HOME)\include
JDK_MDNINCL = $(JDK_HOME)\include\win32

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
!MESSAGE NMAKE /f "ilujava.mak" CFG="Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "Win32 Release" (based on\
 "Win32 (x86) Dynamic-Link Library")
!MESSAGE "Win32 Debug" (based on "Win32 (x86) Dynamic-Link Library")
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

ALL : "$(ILUHOME)\bin\ilujava_12$(ILUJG).dll"

!ELSE 

ALL : "$(ILUHOME)\bin\ilujava_12$(ILUJG).dll"

!ENDIF 

CLEAN :
    -@erase "$(INTDIR)\IluJava_GCClient.obj"
    -@erase "$(INTDIR)\IluJava_IluAlarmTech.obj"
    -@erase "$(INTDIR)\IluJava_IluCall.obj"
    -@erase "$(INTDIR)\IluJava_IluClassRep.obj"
    -@erase "$(INTDIR)\IluJava_IluConnOps.obj"
    -@erase "$(INTDIR)\IluJava_IluDebug.obj"
    -@erase "$(INTDIR)\IluJava_IluInit.obj"
    -@erase "$(INTDIR)\IluJava_IluOInt.obj"
    -@erase "$(INTDIR)\IluJava_IluPort.obj"
    -@erase "$(INTDIR)\IluJava_IluRT0.obj"
    -@erase "$(INTDIR)\IluJava_IluServer.obj"
    -@erase "$(INTDIR)\IluJava_JOps.obj"
    -@erase "$(INTDIR)\IluJava_JMon.obj"
    -@erase "$(INTDIR)\IluJava_LockTech.obj"
    -@erase "$(INTDIR)\IluJava_IluPickle.obj"
    -@erase "$(INTDIR)\IluJava_selectwt.obj"
    -@erase "$(INTDIR)\ilunative.obj"
    -@erase "$(INTDIR)\vc50.idb"
    -@erase "$(INTDIR)\xerox_ilu_IluAlarmTech.obj"
    -@erase "$(INTDIR)\xerox_ilu_IluForkTech.obj"
    -@erase "$(INTDIR)\xerox_ilu_IluCall.obj"
    -@erase "$(INTDIR)\xerox_ilu_IluClassRep.obj"
    -@erase "$(INTDIR)\xerox_ilu_IluDebug.obj"
    -@erase "$(INTDIR)\xerox_ilu_IluDebugHooks.obj"
    -@erase "$(INTDIR)\xerox_ilu_IluDebugWriter.obj"
    -@erase "$(INTDIR)\xerox_ilu_IluExceptionRep.obj"
    -@erase "$(INTDIR)\xerox_ilu_IluGCClient.obj"
    -@erase "$(INTDIR)\xerox_ilu_IluInit2.obj"
    -@erase "$(INTDIR)\xerox_ilu_IluOInt.obj"
    -@erase "$(INTDIR)\xerox_ilu_IluPassport.obj"
    -@erase "$(INTDIR)\xerox_ilu_IluSerializationContext.obj"
    -@erase "$(INTDIR)\xerox_ilu_IluPipeline.obj"
    -@erase "$(INTDIR)\xerox_ilu_IluIdentity.obj"
    -@erase "$(INTDIR)\xerox_ilu_IluIdentityType.obj"
    -@erase "$(INTDIR)\xerox_ilu_IluServiceThread.obj"
    -@erase "$(INTDIR)\xerox_ilu_IluPort.obj"
    -@erase "$(INTDIR)\xerox_ilu_IluGssOid.obj"
    -@erase "$(INTDIR)\xerox_ilu_IluGssCred.obj"
    -@erase "$(INTDIR)\xerox_ilu_IluBatcher.obj"
    -@erase "$(INTDIR)\xerox_ilu_IluRT0.obj"
    -@erase "$(INTDIR)\xerox_ilu_IluServer.obj"
    -@erase "$(INTDIR)\xerox_ilu_IluSBH.obj"
    -@erase "$(INTDIR)\xerox_ilu_IluServerConnection.obj"
    -@erase "$(INTDIR)\xerox_ilu_IluSurrogateConnection.obj"
    -@erase "$(INTDIR)\xerox_ilu_IluTransportInfo.obj"
    -@erase "$(INTDIR)\xerox_ilu_IluTypeRep.obj"
    -@erase "$(INTDIR)\xerox_ilu_IluWPBase.obj"
    -@erase "$(INTDIR)\xerox_ilu_IluPickle.obj"
    -@erase "$(INTDIR)\xerox_ilu_ilutypecode.obj"
    -@erase "$(OUTDIR)\ilujava_12.exp"
    -@erase "$(OUTDIR)\ilujava_12$(ILUJG).lib"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MD /W3 /GX /O2 \
/I "$(JDK_MDOINCL)" /I "$(JDK_OINCL)" /I  "$(JDK_MDNINCL)" /I "$(JDK_NINCL)" \
/I "$(ILUSRC)\runtime\java" \
/I "$(ILUSRC)\runtime\java\build" /I "$(ILUSRC)\runtime\kernel" /I "$(ILUSRC)\GSS\kernel" \
/D "NDEBUG" /D "ONI" /D "WIN32" /D "_WINDOWS" \
/D "ILU_BUILDING_RUNTIME" \
/Fp"$(INTDIR)\ilujava_12.pch" /YX /Fo"$(INTDIR)\\"\
 /Fd"$(INTDIR)\\" /FD /c 
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

MTL=midl.exe
MTL_PROJ=/nologo /D "NDEBUG" /mktyplib203 /o NUL /win32 
RSC=rc.exe
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\ilujava_12.bsc" 

    
LINK32=link.exe
LINK32_FLAGS=$(ILUHOME)\lib\ilu32.lib $(ILUHOME)\lib\gss.lib $(JDK_HOME)\lib\jvm.lib wsock32.lib\
 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib\
 shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo\
 /subsystem:windows /dll /incremental:no /pdb:"$(OUTDIR)\javai$(ILUJG).pdb"\
 /machine:I386 /out:"$(ILUHOME)\bin\ilujava_12$(ILUJG).dll" /implib:"$(OUTDIR)\ilujava_12$(ILUJG).lib" 
LINK32_OBJS= \
    "$(INTDIR)\IluJava_GCClient.obj" \
    "$(INTDIR)\IluJava_IluAlarmTech.obj" \
    "$(INTDIR)\IluJava_IluCall.obj" \
    "$(INTDIR)\IluJava_IluClassRep.obj" \
    "$(INTDIR)\IluJava_IluConnOps.obj" \
    "$(INTDIR)\IluJava_IluDebug.obj" \
    "$(INTDIR)\IluJava_IluInit.obj" \
    "$(INTDIR)\IluJava_IluOInt.obj" \
    "$(INTDIR)\IluJava_IluPort.obj" \
    "$(INTDIR)\IluJava_IluRT0.obj" \
    "$(INTDIR)\IluJava_IluServer.obj" \
    "$(INTDIR)\IluJava_JOps.obj" \
    "$(INTDIR)\IluJava_JMon.obj" \
    "$(INTDIR)\IluJava_LockTech.obj" \
    "$(INTDIR)\IluJava_IluPickle.obj" \
    "$(INTDIR)\IluJava_selectwt.obj" \
    "$(INTDIR)\ilunative.obj" \
    "$(INTDIR)\xerox_ilu_IluAlarmTech.obj" \
    "$(INTDIR)\xerox_ilu_IluForkTech.obj" \
    "$(INTDIR)\xerox_ilu_IluCall.obj" \
    "$(INTDIR)\xerox_ilu_IluClassRep.obj" \
    "$(INTDIR)\xerox_ilu_IluDebug.obj" \
    "$(INTDIR)\xerox_ilu_IluDebugHooks.obj" \
    "$(INTDIR)\xerox_ilu_IluDebugWriter.obj" \
    "$(INTDIR)\xerox_ilu_IluExceptionRep.obj" \
    "$(INTDIR)\xerox_ilu_IluGCClient.obj" \
    "$(INTDIR)\xerox_ilu_IluInit2.obj" \
    "$(INTDIR)\xerox_ilu_IluOInt.obj" \
    "$(INTDIR)\xerox_ilu_IluPassport.obj" \
    "$(INTDIR)\xerox_ilu_IluSerializationContext.obj" \
    "$(INTDIR)\xerox_ilu_IluPipeline.obj" \
    "$(INTDIR)\xerox_ilu_IluIdentity.obj" \
    "$(INTDIR)\xerox_ilu_IluIdentityType.obj" \
    "$(INTDIR)\xerox_ilu_IluServiceThread.obj" \
    "$(INTDIR)\xerox_ilu_IluPort.obj" \
    "$(INTDIR)\xerox_ilu_IluGssOid.obj" \
    "$(INTDIR)\xerox_ilu_IluGssCred.obj" \
    "$(INTDIR)\xerox_ilu_IluBatcher.obj" \
    "$(INTDIR)\xerox_ilu_IluRT0.obj" \
    "$(INTDIR)\xerox_ilu_IluServer.obj" \
    "$(INTDIR)\xerox_ilu_IluSBH.obj" \
    "$(INTDIR)\xerox_ilu_IluServerConnection.obj" \
    "$(INTDIR)\xerox_ilu_IluSurrogateConnection.obj" \
    "$(INTDIR)\xerox_ilu_IluTransportInfo.obj" \
    "$(INTDIR)\xerox_ilu_IluTypeRep.obj" \
    "$(INTDIR)\xerox_ilu_IluWPBase.obj" \
    "$(INTDIR)\xerox_ilu_IluPickle.obj" \
    "$(INTDIR)\xerox_ilu_ilutypecode.obj"

"$(ILUHOME)\bin\ilujava_12$(ILUJG).dll" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
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

ALL : "$(ILUHOME)\bin\ilujava_12$(ILUJG).dll" 

!ELSE 

ALL : "$(ILUHOME)\bin\ilujava_12$(ILUJG).dll" 

!ENDIF 

CLEAN :
    -@erase "$(INTDIR)\IluJava_GCClient.obj"
    -@erase "$(INTDIR)\IluJava_GCClient.sbr"
    -@erase "$(INTDIR)\IluJava_IluAlarmTech.obj"
    -@erase "$(INTDIR)\IluJava_IluAlarmTech.sbr"
    -@erase "$(INTDIR)\IluJava_IluCall.obj"
    -@erase "$(INTDIR)\IluJava_IluCall.sbr"
    -@erase "$(INTDIR)\IluJava_IluClassRep.obj"
    -@erase "$(INTDIR)\IluJava_IluClassRep.sbr"
    -@erase "$(INTDIR)\IluJava_IluConnOps.obj"
    -@erase "$(INTDIR)\IluJava_IluConnOps.sbr"
    -@erase "$(INTDIR)\IluJava_IluDebug.obj"
    -@erase "$(INTDIR)\IluJava_IluDebug.sbr"
    -@erase "$(INTDIR)\IluJava_IluInit.obj"
    -@erase "$(INTDIR)\IluJava_IluInit.sbr"
    -@erase "$(INTDIR)\IluJava_IluOInt.obj"
    -@erase "$(INTDIR)\IluJava_IluOInt.sbr"
    -@erase "$(INTDIR)\IluJava_IluPort.obj"
    -@erase "$(INTDIR)\IluJava_IluPort.sbr"
    -@erase "$(INTDIR)\IluJava_IluRT0.obj"
    -@erase "$(INTDIR)\IluJava_IluRT0.sbr"
    -@erase "$(INTDIR)\IluJava_IluServer.obj"
    -@erase "$(INTDIR)\IluJava_IluServer.sbr"
    -@erase "$(INTDIR)\IluJava_JOps.obj"
    -@erase "$(INTDIR)\IluJava_JOps.sbr"
    -@erase "$(INTDIR)\IluJava_JMon.obj"
    -@erase "$(INTDIR)\IluJava_JMon.sbr"
    -@erase "$(INTDIR)\IluJava_LockTech.obj"
    -@erase "$(INTDIR)\IluJava_LockTech.sbr"
    -@erase "$(INTDIR)\IluJava_IluPickle.obj"
    -@erase "$(INTDIR)\IluJava_IluPickle.sbr"
    -@erase "$(INTDIR)\IluJava_selectwt.obj"
    -@erase "$(INTDIR)\IluJava_selectwt.sbr"
    -@erase "$(INTDIR)\ilunative.obj"
    -@erase "$(INTDIR)\ilunative.sbr"
    -@erase "$(INTDIR)\vc50.idb"
    -@erase "$(INTDIR)\vc50.pdb"
    -@erase "$(INTDIR)\xerox_ilu_IluAlarmTech.obj"
    -@erase "$(INTDIR)\xerox_ilu_IluAlarmTech.sbr"
    -@erase "$(INTDIR)\xerox_ilu_IluForkTech.obj"
    -@erase "$(INTDIR)\xerox_ilu_IluForkTech.sbr"
    -@erase "$(INTDIR)\xerox_ilu_IluCall.obj"
    -@erase "$(INTDIR)\xerox_ilu_IluCall.sbr"
    -@erase "$(INTDIR)\xerox_ilu_IluClassRep.obj"
    -@erase "$(INTDIR)\xerox_ilu_IluClassRep.sbr"
    -@erase "$(INTDIR)\xerox_ilu_IluDebug.obj"
    -@erase "$(INTDIR)\xerox_ilu_IluDebug.sbr"
    -@erase "$(INTDIR)\xerox_ilu_IluDebugHooks.obj"
    -@erase "$(INTDIR)\xerox_ilu_IluDebugHooks.sbr"
    -@erase "$(INTDIR)\xerox_ilu_IluDebugWriter.obj"
    -@erase "$(INTDIR)\xerox_ilu_IluDebugWriter.sbr"
    -@erase "$(INTDIR)\xerox_ilu_IluExceptionRep.obj"
    -@erase "$(INTDIR)\xerox_ilu_IluExceptionRep.sbr"
    -@erase "$(INTDIR)\xerox_ilu_IluGCClient.obj"
    -@erase "$(INTDIR)\xerox_ilu_IluGCClient.sbr"
    -@erase "$(INTDIR)\xerox_ilu_IluInit2.obj"
    -@erase "$(INTDIR)\xerox_ilu_IluInit2.sbr"
    -@erase "$(INTDIR)\xerox_ilu_IluOInt.obj"
    -@erase "$(INTDIR)\xerox_ilu_IluOInt.sbr"
    -@erase "$(INTDIR)\xerox_ilu_IluPassport.obj"
    -@erase "$(INTDIR)\xerox_ilu_IluSerializationContext.obj"
    -@erase "$(INTDIR)\xerox_ilu_IluPipeline.obj"
    -@erase "$(INTDIR)\xerox_ilu_IluSerializationContext.obj"
    -@erase "$(INTDIR)\xerox_ilu_IluPipeline.obj"
    -@erase "$(INTDIR)\xerox_ilu_IluIdentity.obj"
    -@erase "$(INTDIR)\xerox_ilu_IluIdentityType.obj"
    -@erase "$(INTDIR)\xerox_ilu_IluServiceThread.obj"
    -@erase "$(INTDIR)\xerox_ilu_IluPassport.sbr"
    -@erase "$(INTDIR)\xerox_ilu_IluSerializationContext.sbr"
    -@erase "$(INTDIR)\xerox_ilu_IluPipeline.sbr"
    -@erase "$(INTDIR)\xerox_ilu_IluIdentity.sbr"
    -@erase "$(INTDIR)\xerox_ilu_IluIdentityType.sbr"
    -@erase "$(INTDIR)\xerox_ilu_IluServiceThread.sbr"
    -@erase "$(INTDIR)\xerox_ilu_IluPort.obj"
    -@erase "$(INTDIR)\xerox_ilu_IluPort.sbr"
    -@erase "$(INTDIR)\xerox_ilu_IluGssOid.obj"
	-@erase "$(INTDIR)\xerox_ilu_IluGssOid.sbr"
    -@erase "$(INTDIR)\xerox_ilu_IluGssCred.obj"
	-@erase "$(INTDIR)\xerox_ilu_IluGssCred.sbr"
    -@erase "$(INTDIR)\xerox_ilu_IluBatcher.obj"
    -@erase "$(INTDIR)\xerox_ilu_IluBatcher.sbr"
    -@erase "$(INTDIR)\xerox_ilu_IluRT0.obj"
    -@erase "$(INTDIR)\xerox_ilu_IluRT0.sbr"
    -@erase "$(INTDIR)\xerox_ilu_IluServer.obj"
    -@erase "$(INTDIR)\xerox_ilu_IluServer.sbr"
    -@erase "$(INTDIR)\xerox_ilu_IluSBH.obj"
    -@erase "$(INTDIR)\xerox_ilu_IluSBH.sbr"
    -@erase "$(INTDIR)\xerox_ilu_IluServerConnection.obj"
    -@erase "$(INTDIR)\xerox_ilu_IluServerConnection.sbr"
    -@erase "$(INTDIR)\xerox_ilu_IluSurrogateConnection.obj"
    -@erase "$(INTDIR)\xerox_ilu_IluSurrogateConnection.sbr"
    -@erase "$(INTDIR)\xerox_ilu_IluTransportInfo.obj"
    -@erase "$(INTDIR)\xerox_ilu_IluTransportInfo.sbr"
    -@erase "$(INTDIR)\xerox_ilu_IluTypeRep.obj"
    -@erase "$(INTDIR)\xerox_ilu_IluWPBase.obj"
    -@erase "$(INTDIR)\xerox_ilu_IluTypeRep.sbr"
    -@erase "$(INTDIR)\xerox_ilu_IluPickle.obj"
    -@erase "$(INTDIR)\xerox_ilu_IluPickle.sbr"
    -@erase "$(INTDIR)\xerox_ilu_ilutypecode.obj"
    -@erase "$(INTDIR)\xerox_ilu_ilutypecode.sbr"
    -@erase "$(OUTDIR)\ilujava_12.bsc"
    -@erase "$(OUTDIR)\ilujava_12.exp"
    -@erase "$(OUTDIR)\ilujava_12$(ILUJG).lib"
    -@erase "$(OUTDIR)\ilujava_12$(ILUJG).pdb"
    -@erase "$(ILUHOME)\bin\ilujava_12.ilk"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MD /W3 /Gm /GX /Zi /Od \
/I "$(JDK_MDOINCL)" /I "$(JDK_OINCL)" /I  "$(JDK_MDNINCL)" /I "$(JDK_NINCL)" \
/I "$(ILUSRC)\runtime\java" \
/I "$(ILUSRC)\runtime\java\build" /I "$(ILUSRC)\runtime\kernel" /I "$(ILUSRC)\GSS\kernel" \
/D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "ONI" /D "ILU_BUILDING_RUNTIME" /D\
  $(ILU_DEBUG_JFLAGS) /Fp"$(INTDIR)\ilujava_12.pch" /YX\
 /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
CPP_OBJS=.\WinDebug/

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

MTL=midl.exe
MTL_PROJ=/nologo /D "_DEBUG" /mktyplib203 /o NUL /win32 
RSC=rc.exe
BSC32=bscmake.exe


LINK32=link.exe
LINK32_FLAGS=$(ILUHOME)\lib\ilu32.lib $(ILUHOME)\lib\gss.lib $(JDK_HOME)\lib\jvm.lib wsock32.lib\
 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib\
 shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo\
 /subsystem:windows /dll /incremental:yes /pdb:"$(OUTDIR)\javai$(ILUJG).pdb" /debug\
 /machine:I386 /out:"$(ILUHOME)\bin\ilujava_12$(ILUJG).dll" /implib:"$(OUTDIR)\ilujava_12$(ILUJG).lib"\
 /pdbtype:sept 
LINK32_OBJS= \
    "$(INTDIR)\IluJava_GCClient.obj" \
    "$(INTDIR)\IluJava_IluAlarmTech.obj" \
    "$(INTDIR)\IluJava_IluCall.obj" \
    "$(INTDIR)\IluJava_IluClassRep.obj" \
    "$(INTDIR)\IluJava_IluConnOps.obj" \
    "$(INTDIR)\IluJava_IluDebug.obj" \
    "$(INTDIR)\IluJava_IluInit.obj" \
    "$(INTDIR)\IluJava_IluOInt.obj" \
    "$(INTDIR)\IluJava_IluPort.obj" \
    "$(INTDIR)\IluJava_IluRT0.obj" \
    "$(INTDIR)\IluJava_IluServer.obj" \
    "$(INTDIR)\IluJava_JOps.obj" \
    "$(INTDIR)\IluJava_JMon.obj" \
    "$(INTDIR)\IluJava_LockTech.obj" \
    "$(INTDIR)\IluJava_IluPickle.obj" \
    "$(INTDIR)\IluJava_selectwt.obj" \
    "$(INTDIR)\ilunative.obj" \
    "$(INTDIR)\xerox_ilu_IluAlarmTech.obj" \
    "$(INTDIR)\xerox_ilu_IluForkTech.obj" \
    "$(INTDIR)\xerox_ilu_IluCall.obj" \
    "$(INTDIR)\xerox_ilu_IluClassRep.obj" \
    "$(INTDIR)\xerox_ilu_IluDebug.obj" \
    "$(INTDIR)\xerox_ilu_IluDebugHooks.obj" \
    "$(INTDIR)\xerox_ilu_IluDebugWriter.obj" \
    "$(INTDIR)\xerox_ilu_IluExceptionRep.obj" \
    "$(INTDIR)\xerox_ilu_IluGCClient.obj" \
    "$(INTDIR)\xerox_ilu_IluInit2.obj" \
    "$(INTDIR)\xerox_ilu_IluOInt.obj" \
    "$(INTDIR)\xerox_ilu_IluPassport.obj" \
    "$(INTDIR)\xerox_ilu_IluSerializationContext.obj" \
    "$(INTDIR)\xerox_ilu_IluPipeline.obj" \
    "$(INTDIR)\xerox_ilu_IluIdentity.obj" \
    "$(INTDIR)\xerox_ilu_IluIdentityType.obj" \
    "$(INTDIR)\xerox_ilu_IluServiceThread.obj" \
    "$(INTDIR)\xerox_ilu_IluPort.obj" \
	"$(INTDIR)\xerox_ilu_IluGssOid.obj" \
    "$(INTDIR)\xerox_ilu_IluGssCred.obj" \
    "$(INTDIR)\xerox_ilu_IluBatcher.obj" \
    "$(INTDIR)\xerox_ilu_IluRT0.obj" \
    "$(INTDIR)\xerox_ilu_IluServer.obj" \
    "$(INTDIR)\xerox_ilu_IluSBH.obj" \
    "$(INTDIR)\xerox_ilu_IluServerConnection.obj" \
    "$(INTDIR)\xerox_ilu_IluSurrogateConnection.obj" \
    "$(INTDIR)\xerox_ilu_IluTransportInfo.obj" \
    "$(INTDIR)\xerox_ilu_IluTypeRep.obj" \
    "$(INTDIR)\xerox_ilu_IluWPBase.obj" \
    "$(INTDIR)\xerox_ilu_IluPickle.obj" \
    "$(INTDIR)\xerox_ilu_ilutypecode.obj"

"$(ILUHOME)\bin\ilujava_12$(ILUJG).dll" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ENDIF 


!IF "$(CFG)" == "Win32 Release" || "$(CFG)" ==\
 "Win32 Debug"
SOURCE=$(ILUSRC)\runtime\java\IluJava_GCClient.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_ILUJA=\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilucall.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluclassrep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilugcclient.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilumethodrep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluobject.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluobjecttable.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluoint.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilupassport.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluidentity.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluidentitytype.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluservicethread.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluport.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilubatcher.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilurt0.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluserver.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluserverrelocationinfo.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluserializationcontext.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilusurrogateobject.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilutransportinfo.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilutyperep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluwpbase.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilupickle.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilutypecode.h"\
    "$(ILUSRC)\runtime\java\ilujava_common.h"\
    "$(ILUSRC)\runtime\java\ilujava_includes.h"\
    "$(ILUSRC)\runtime\java\ilujava_jmem.h"\
    "$(ILUSRC)\runtime\java\ilujava_jstubs.h"\
    "$(ILUSRC)\runtime\java\ilujava_jtypes.h"\
    "$(ILUSRC)\runtime\java\ilunative.h"\
    "$(ILUSRC)\runtime\kernel\ilubasic.h"\
    "$(ILUSRC)\runtime\kernel\iluerror.h"\
    "$(ILUSRC)\runtime\kernel\iluerrs.h"\
    "$(ILUSRC)\runtime\kernel\iluhash.h"\
    "$(ILUSRC)\runtime\kernel\ilumac.h"\
    "$(ILUSRC)\runtime\kernel\ilutpcod.h"\
    "$(ILUSRC)\runtime\kernel\ilutypes.h"\
    "$(ILUSRC)\runtime\kernel\iluwin.h"\
    "$(ILUSRC)\runtime\kernel\iluxport.h"\
    "$(JDK_OINCL)\bool.h"\
    "$(JDK_OINCL)\debug.h"\
    "$(JDK_OINCL)\interpreter.h"\
    "$(JDK_OINCL)\java_lang_string.h"\
    "$(JDK_OINCL)\javastring.h"\
    "$(JDK_OINCL)\jcov.h"\
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_OINCL)\native.h"\
    "$(JDK_OINCL)\oobj.h"\
    "$(JDK_OINCL)\signature.h"\
    "$(JDK_OINCL)\stubpreamble.h"\
    "$(JDK_OINCL)\sys_api.h"\
    "$(JDK_OINCL)\tree.h"\
    "$(JDK_OINCL)\typecodes.h"\
    "$(JDK_OINCL)\typedefs.h"\
    "$(JDK_MDOINCL)\io_md.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
    "$(JDK_MDOINCL)\oobj_md.h"\
    "$(JDK_MDOINCL)\sysmacros_md.h"\
    "$(JDK_MDOINCL)\timeval_md.h"\
    "$(JDK_MDOINCL)\typedefs_md.h"\
    {$(INCLUDE)}"sys\types.h"\
    

"$(INTDIR)\IluJava_GCClient.obj" : $(SOURCE) $(DEP_CPP_ILUJA) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_ILUJA=\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilucall.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluclassrep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilugcclient.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilumethodrep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluobject.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluobjecttable.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluoint.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilupassport.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluidentity.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluidentitytype.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluservicethread.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluport.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilubatcher.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilurt0.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluserver.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluserverrelocationinfo.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluserializationcontext.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilusurrogateobject.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilutransportinfo.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilutyperep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluwpbase.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilupickle.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilutypecode.h"\
    "$(ILUSRC)\runtime\java\ilujava_common.h"\
    "$(ILUSRC)\runtime\java\ilujava_includes.h"\
    "$(ILUSRC)\runtime\java\ilujava_jmem.h"\
    "$(ILUSRC)\runtime\java\ilujava_jstubs.h"\
    "$(ILUSRC)\runtime\java\ilujava_jtypes.h"\
    "$(ILUSRC)\runtime\java\ilunative.h"\
    "$(ILUSRC)\runtime\kernel\ilubasic.h"\
    "$(ILUSRC)\runtime\kernel\iluerror.h"\
    "$(ILUSRC)\runtime\kernel\iluerrs.h"\
    "$(ILUSRC)\runtime\kernel\iluhash.h"\
    "$(ILUSRC)\runtime\kernel\ilutpcod.h"\
    "$(ILUSRC)\runtime\kernel\ilutypes.h"\
    "$(ILUSRC)\runtime\kernel\iluwin.h"\
    "$(ILUSRC)\runtime\kernel\iluxport.h"\
    "$(JDK_OINCL)\bool.h"\
    "$(JDK_OINCL)\debug.h"\
    "$(JDK_OINCL)\interpreter.h"\
    "$(JDK_OINCL)\java_lang_string.h"\
    "$(JDK_OINCL)\javastring.h"\
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_OINCL)\native.h"\
    "$(JDK_OINCL)\oobj.h"\
    "$(JDK_OINCL)\signature.h"\
    "$(JDK_OINCL)\stubpreamble.h"\
    "$(JDK_OINCL)\sys_api.h"\
    "$(JDK_OINCL)\tree.h"\
    "$(JDK_OINCL)\typecodes.h"\
    "$(JDK_OINCL)\typedefs.h"\
    "$(JDK_MDOINCL)\io_md.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
    "$(JDK_MDOINCL)\oobj_md.h"\
    "$(JDK_MDOINCL)\sysmacros_md.h"\
    "$(JDK_MDOINCL)\timeval_md.h"\
    "$(JDK_MDOINCL)\typedefs_md.h"\
    

"$(INTDIR)\IluJava_GCClient.obj"    "$(INTDIR)\IluJava_GCClient.sbr" : $(SOURCE)\
 $(DEP_CPP_ILUJA) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=$(ILUSRC)\runtime\java\IluJava_IluAlarmTech.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_ILUJAV=\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilualarmtech.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluforktech.h"\
    "$(ILUSRC)\runtime\java\ilujava_common.h"\
    "$(ILUSRC)\runtime\java\ilujava_includes.h"\
    "$(ILUSRC)\runtime\java\ilujava_jgc.h"\
    "$(ILUSRC)\runtime\java\ilujava_jmem.h"\
    "$(ILUSRC)\runtime\java\ilujava_jtypes.h"\
    "$(ILUSRC)\runtime\java\ilunative.h"\
    "$(ILUSRC)\runtime\kernel\ilubasic.h"\
    "$(ILUSRC)\runtime\kernel\iluerror.h"\
    "$(ILUSRC)\runtime\kernel\iluerrs.h"\
    "$(ILUSRC)\runtime\kernel\iluhash.h"\
    "$(ILUSRC)\runtime\kernel\ilumac.h"\
    "$(ILUSRC)\runtime\kernel\ilutpcod.h"\
    "$(ILUSRC)\runtime\kernel\ilutypes.h"\
    "$(ILUSRC)\runtime\kernel\iluwin.h"\
    "$(ILUSRC)\runtime\kernel\iluxport.h"\
    "$(JDK_OINCL)\bool.h"\
    "$(JDK_OINCL)\debug.h"\
    "$(JDK_OINCL)\interpreter.h"\
    "$(JDK_OINCL)\java_lang_string.h"\
    "$(JDK_OINCL)\javastring.h"\
    "$(JDK_OINCL)\jcov.h"\
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_OINCL)\native.h"\
    "$(JDK_OINCL)\oobj.h"\
    "$(JDK_OINCL)\signature.h"\
    "$(JDK_OINCL)\stubpreamble.h"\
    "$(JDK_OINCL)\sys_api.h"\
    "$(JDK_OINCL)\tree.h"\
    "$(JDK_OINCL)\typecodes.h"\
    "$(JDK_OINCL)\typedefs.h"\
    "$(JDK_MDOINCL)\io_md.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
    "$(JDK_MDOINCL)\oobj_md.h"\
    "$(JDK_MDOINCL)\sysmacros_md.h"\
    "$(JDK_MDOINCL)\timeval_md.h"\
    "$(JDK_MDOINCL)\typedefs_md.h"\
    {$(INCLUDE)}"sys\types.h"\
    

"$(INTDIR)\IluJava_IluAlarmTech.obj" : $(SOURCE) $(DEP_CPP_ILUJAV) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_ILUJAV=\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilualarmtech.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluforktech.h"\
    "$(ILUSRC)\runtime\java\ilujava_common.h"\
    "$(ILUSRC)\runtime\java\ilujava_includes.h"\
    "$(ILUSRC)\runtime\java\ilujava_jgc.h"\
    "$(ILUSRC)\runtime\java\ilujava_jmem.h"\
    "$(ILUSRC)\runtime\java\ilujava_jtypes.h"\
    "$(ILUSRC)\runtime\java\ilunative.h"\
    "$(ILUSRC)\runtime\kernel\ilubasic.h"\
    "$(ILUSRC)\runtime\kernel\iluerror.h"\
    "$(ILUSRC)\runtime\kernel\iluerrs.h"\
    "$(ILUSRC)\runtime\kernel\iluhash.h"\
    "$(ILUSRC)\runtime\kernel\ilutpcod.h"\
    "$(ILUSRC)\runtime\kernel\ilutypes.h"\
    "$(ILUSRC)\runtime\kernel\iluwin.h"\
    "$(ILUSRC)\runtime\kernel\iluxport.h"\
    "$(JDK_OINCL)\bool.h"\
    "$(JDK_OINCL)\debug.h"\
    "$(JDK_OINCL)\interpreter.h"\
    "$(JDK_OINCL)\java_lang_string.h"\
    "$(JDK_OINCL)\javastring.h"\
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_OINCL)\native.h"\
    "$(JDK_OINCL)\oobj.h"\
    "$(JDK_OINCL)\signature.h"\
    "$(JDK_OINCL)\stubpreamble.h"\
    "$(JDK_OINCL)\sys_api.h"\
    "$(JDK_OINCL)\tree.h"\
    "$(JDK_OINCL)\typecodes.h"\
    "$(JDK_OINCL)\typedefs.h"\
    "$(JDK_MDOINCL)\io_md.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
    "$(JDK_MDOINCL)\oobj_md.h"\
    "$(JDK_MDOINCL)\sysmacros_md.h"\
    "$(JDK_MDOINCL)\timeval_md.h"\
    "$(JDK_MDOINCL)\typedefs_md.h"\
    

"$(INTDIR)\IluJava_IluAlarmTech.obj"    "$(INTDIR)\IluJava_IluAlarmTech.sbr" : \
$(SOURCE) $(DEP_CPP_ILUJAV) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=$(ILUSRC)\runtime\java\IluJava_IluCall.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_ILUJAVA=\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilucall.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluclassrep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilumethodrep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluobject.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluobjecttable.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluoint.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilupassport.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluidentity.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluidentitytype.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluservicethread.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluport.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilubatcher.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilurt0.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluserver.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluserverrelocationinfo.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluserializationcontext.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilusurrogateobject.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilutransportinfo.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilutyperep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluwpbase.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilupickle.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilutypecode.h"\
    "$(ILUSRC)\runtime\java\ilujava_callmacros.h"\
    "$(ILUSRC)\runtime\java\ilujava_common.h"\
    "$(ILUSRC)\runtime\java\ilujava_includes.h"\
    "$(ILUSRC)\runtime\java\ilujava_jarrays.h"\
    "$(ILUSRC)\runtime\java\ilujava_jgc.h"\
    "$(ILUSRC)\runtime\java\ilujava_jmem.h"\
    "$(ILUSRC)\runtime\java\ilujava_jmon.h"\
    "$(ILUSRC)\runtime\java\ilujava_jops.h"\
    "$(ILUSRC)\runtime\java\ilujava_jstubs.h"\
    "$(ILUSRC)\runtime\java\ilujava_jtypes.h"\
    "$(ILUSRC)\runtime\java\ilujava_ops.h"\
    "$(ILUSRC)\runtime\java\ilunative.h"\
    "$(ILUSRC)\runtime\kernel\ilubasic.h"\
    "$(ILUSRC)\runtime\kernel\iluerror.h"\
    "$(ILUSRC)\runtime\kernel\iluerrs.h"\
    "$(ILUSRC)\runtime\kernel\iluhash.h"\
    "$(ILUSRC)\runtime\kernel\ilumac.h"\
    "$(ILUSRC)\runtime\kernel\ilutpcod.h"\
    "$(ILUSRC)\runtime\kernel\ilutypes.h"\
    "$(ILUSRC)\runtime\kernel\iluwin.h"\
    "$(ILUSRC)\runtime\kernel\iluxport.h"\
    "$(ILUSRC)\runtime\kernel\threads.h"\
    "$(JDK_OINCL)\bool.h"\
    "$(JDK_OINCL)\debug.h"\
    "$(JDK_OINCL)\interpreter.h"\
    "$(JDK_OINCL)\java_lang_string.h"\
    "$(JDK_OINCL)\java_lang_thread.h"\
    "$(JDK_OINCL)\javastring.h"\
    "$(JDK_OINCL)\jcov.h"\
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_OINCL)\monitor.h"\
    "$(JDK_OINCL)\native.h"\
    "$(JDK_OINCL)\oobj.h"\
    "$(JDK_OINCL)\signature.h"\
    "$(JDK_OINCL)\stubpreamble.h"\
    "$(JDK_OINCL)\sys_api.h"\
    "$(JDK_OINCL)\tree.h"\
    "$(JDK_OINCL)\typecodes.h"\
    "$(JDK_OINCL)\typedefs.h"\
    "$(JDK_MDOINCL)\io_md.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
    "$(JDK_MDOINCL)\oobj_md.h"\
    "$(JDK_MDOINCL)\sysmacros_md.h"\
    "$(JDK_MDOINCL)\timeval_md.h"\
    "$(JDK_MDOINCL)\typedefs_md.h"\
    {$(INCLUDE)}"sys\types.h"\
    

"$(INTDIR)\IluJava_IluCall.obj" : $(SOURCE) $(DEP_CPP_ILUJAVA) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_ILUJAVA=\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilucall.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluclassrep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilumethodrep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluobject.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluobjecttable.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluoint.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilupassport.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluidentity.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluidentitytype.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluservicethread.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluport.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilubatcher.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilurt0.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluserver.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluserverrelocationinfo.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluserializationcontext.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilusurrogateobject.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilutransportinfo.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilutyperep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluwpbase.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilupickle.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilutypecode.h"\
    "$(ILUSRC)\runtime\java\ilujava_callmacros.h"\
    "$(ILUSRC)\runtime\java\ilujava_common.h"\
    "$(ILUSRC)\runtime\java\ilujava_includes.h"\
    "$(ILUSRC)\runtime\java\ilujava_jarrays.h"\
    "$(ILUSRC)\runtime\java\ilujava_jgc.h"\
    "$(ILUSRC)\runtime\java\ilujava_jmem.h"\
    "$(ILUSRC)\runtime\java\ilujava_jmon.h"\
    "$(ILUSRC)\runtime\java\ilujava_jops.h"\
    "$(ILUSRC)\runtime\java\ilujava_jstubs.h"\
    "$(ILUSRC)\runtime\java\ilujava_jtypes.h"\
    "$(ILUSRC)\runtime\java\ilujava_ops.h"\
    "$(ILUSRC)\runtime\java\ilunative.h"\
    "$(ILUSRC)\runtime\kernel\ilubasic.h"\
    "$(ILUSRC)\runtime\kernel\iluerror.h"\
    "$(ILUSRC)\runtime\kernel\iluerrs.h"\
    "$(ILUSRC)\runtime\kernel\iluhash.h"\
    "$(ILUSRC)\runtime\kernel\ilutpcod.h"\
    "$(ILUSRC)\runtime\kernel\ilutypes.h"\
    "$(ILUSRC)\runtime\kernel\iluwin.h"\
    "$(ILUSRC)\runtime\kernel\iluxport.h"\
    "$(JDK_OINCL)\bool.h"\
    "$(JDK_OINCL)\debug.h"\
    "$(JDK_OINCL)\interpreter.h"\
    "$(JDK_OINCL)\java_lang_string.h"\
    "$(JDK_OINCL)\javastring.h"\
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_OINCL)\monitor.h"\
    "$(JDK_OINCL)\native.h"\
    "$(JDK_OINCL)\oobj.h"\
    "$(JDK_OINCL)\signature.h"\
    "$(JDK_OINCL)\stubpreamble.h"\
    "$(JDK_OINCL)\sys_api.h"\
    "$(JDK_OINCL)\tree.h"\
    "$(JDK_OINCL)\typecodes.h"\
    "$(JDK_OINCL)\typedefs.h"\
    "$(JDK_MDOINCL)\io_md.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
    "$(JDK_MDOINCL)\oobj_md.h"\
    "$(JDK_MDOINCL)\sysmacros_md.h"\
    "$(JDK_MDOINCL)\timeval_md.h"\
    "$(JDK_MDOINCL)\typedefs_md.h"\
    

"$(INTDIR)\IluJava_IluCall.obj" "$(INTDIR)\IluJava_IluCall.sbr" : $(SOURCE)\
 $(DEP_CPP_ILUJAVA) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=$(ILUSRC)\runtime\java\IluJava_IluClassRep.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_ILUJAVA_=\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilucall.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluclassrep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluexceptionrep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilumethodargrep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilumethodrep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluobject.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluobjecttable.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluoint.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilupassport.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluidentity.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluidentitytype.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluservicethread.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluport.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilubatcher.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilurt0.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluserver.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluserverrelocationinfo.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluserializationcontext.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilusurrogateobject.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilutransportinfo.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilutyperep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluwpbase.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilupickle.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilutypecode.h"\
    "$(ILUSRC)\runtime\java\ilujava_common.h"\
    "$(ILUSRC)\runtime\java\ilujava_includes.h"\
    "$(ILUSRC)\runtime\java\ilujava_jarrays.h"\
    "$(ILUSRC)\runtime\java\ilujava_jmem.h"\
    "$(ILUSRC)\runtime\java\ilujava_jstubs.h"\
    "$(ILUSRC)\runtime\java\ilujava_jtypes.h"\
    "$(ILUSRC)\runtime\java\ilunative.h"\
    "$(ILUSRC)\runtime\kernel\ilubasic.h"\
    "$(ILUSRC)\runtime\kernel\iluerror.h"\
    "$(ILUSRC)\runtime\kernel\iluerrs.h"\
    "$(ILUSRC)\runtime\kernel\iluhash.h"\
    "$(ILUSRC)\runtime\kernel\ilumac.h"\
    "$(ILUSRC)\runtime\kernel\ilutpcod.h"\
    "$(ILUSRC)\runtime\kernel\ilutypes.h"\
    "$(ILUSRC)\runtime\kernel\iluwin.h"\
    "$(ILUSRC)\runtime\kernel\iluxport.h"\
    "$(JDK_OINCL)\bool.h"\
    "$(JDK_OINCL)\debug.h"\
    "$(JDK_OINCL)\interpreter.h"\
    "$(JDK_OINCL)\java_lang_string.h"\
    "$(JDK_OINCL)\javastring.h"\
    "$(JDK_OINCL)\jcov.h"\
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_OINCL)\native.h"\
    "$(JDK_OINCL)\oobj.h"\
    "$(JDK_OINCL)\signature.h"\
    "$(JDK_OINCL)\stubpreamble.h"\
    "$(JDK_OINCL)\sys_api.h"\
    "$(JDK_OINCL)\tree.h"\
    "$(JDK_OINCL)\typecodes.h"\
    "$(JDK_OINCL)\typedefs.h"\
    "$(JDK_MDOINCL)\io_md.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
    "$(JDK_MDOINCL)\oobj_md.h"\
    "$(JDK_MDOINCL)\sysmacros_md.h"\
    "$(JDK_MDOINCL)\timeval_md.h"\
    "$(JDK_MDOINCL)\typedefs_md.h"\
    {$(INCLUDE)}"sys\types.h"\
    

"$(INTDIR)\IluJava_IluClassRep.obj" : $(SOURCE) $(DEP_CPP_ILUJAVA_) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_ILUJAVA_=\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilucall.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluclassrep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluexceptionrep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilumethodargrep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilumethodrep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluobject.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluobjecttable.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluoint.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilupassport.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluidentity.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluidentitytype.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluservicethread.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluport.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilubatcher.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilurt0.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluserver.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluserverrelocationinfo.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluserializationcontext.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilusurrogateobject.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilutransportinfo.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilutyperep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluwpbase.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilupickle.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilutypecode.h"\
    "$(ILUSRC)\runtime\java\ilujava_common.h"\
    "$(ILUSRC)\runtime\java\ilujava_includes.h"\
    "$(ILUSRC)\runtime\java\ilujava_jarrays.h"\
    "$(ILUSRC)\runtime\java\ilujava_jmem.h"\
    "$(ILUSRC)\runtime\java\ilujava_jstubs.h"\
    "$(ILUSRC)\runtime\java\ilujava_jtypes.h"\
    "$(ILUSRC)\runtime\java\ilunative.h"\
    "$(ILUSRC)\runtime\kernel\ilubasic.h"\
    "$(ILUSRC)\runtime\kernel\iluerror.h"\
    "$(ILUSRC)\runtime\kernel\iluerrs.h"\
    "$(ILUSRC)\runtime\kernel\iluhash.h"\
    "$(ILUSRC)\runtime\kernel\ilutpcod.h"\
    "$(ILUSRC)\runtime\kernel\ilutypes.h"\
    "$(ILUSRC)\runtime\kernel\iluwin.h"\
    "$(ILUSRC)\runtime\kernel\iluxport.h"\
    "$(JDK_OINCL)\bool.h"\
    "$(JDK_OINCL)\debug.h"\
    "$(JDK_OINCL)\interpreter.h"\
    "$(JDK_OINCL)\java_lang_string.h"\
    "$(JDK_OINCL)\javastring.h"\
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_OINCL)\native.h"\
    "$(JDK_OINCL)\oobj.h"\
    "$(JDK_OINCL)\signature.h"\
    "$(JDK_OINCL)\stubpreamble.h"\
    "$(JDK_OINCL)\sys_api.h"\
    "$(JDK_OINCL)\tree.h"\
    "$(JDK_OINCL)\typecodes.h"\
    "$(JDK_OINCL)\typedefs.h"\
    "$(JDK_MDOINCL)\io_md.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
    "$(JDK_MDOINCL)\oobj_md.h"\
    "$(JDK_MDOINCL)\sysmacros_md.h"\
    "$(JDK_MDOINCL)\timeval_md.h"\
    "$(JDK_MDOINCL)\typedefs_md.h"\
    

"$(INTDIR)\IluJava_IluClassRep.obj" "$(INTDIR)\IluJava_IluClassRep.sbr" : \
$(SOURCE) $(DEP_CPP_ILUJAVA_) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=$(ILUSRC)\runtime\java\IluJava_IluConnOps.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_ILUJAVA_I=\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilucall.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluclassrep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilumethodrep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluport.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilubatcher.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluserverconnection.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluserializationcontext.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilusurrogateconnection.h"\
    "$(ILUSRC)\runtime\java\ilujava_common.h"\
    "$(ILUSRC)\runtime\java\ilujava_includes.h"\
    "$(ILUSRC)\runtime\java\ilujava_jgc.h"\
    "$(ILUSRC)\runtime\java\ilujava_jmem.h"\
    "$(ILUSRC)\runtime\java\ilujava_jtypes.h"\
    "$(ILUSRC)\runtime\java\ilunative.h"\
    "$(ILUSRC)\runtime\kernel\ilubasic.h"\
    "$(ILUSRC)\runtime\kernel\iluerror.h"\
    "$(ILUSRC)\runtime\kernel\iluerrs.h"\
    "$(ILUSRC)\runtime\kernel\iluhash.h"\
    "$(ILUSRC)\runtime\kernel\ilumac.h"\
    "$(ILUSRC)\runtime\kernel\ilutpcod.h"\
    "$(ILUSRC)\runtime\kernel\ilutypes.h"\
    "$(ILUSRC)\runtime\kernel\iluwin.h"\
    "$(ILUSRC)\runtime\kernel\iluxport.h"\
    "$(JDK_OINCL)\bool.h"\
    "$(JDK_OINCL)\debug.h"\
    "$(JDK_OINCL)\interpreter.h"\
    "$(JDK_OINCL)\java_lang_string.h"\
    "$(JDK_OINCL)\javastring.h"\
    "$(JDK_OINCL)\jcov.h"\
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_OINCL)\native.h"\
    "$(JDK_OINCL)\oobj.h"\
    "$(JDK_OINCL)\signature.h"\
    "$(JDK_OINCL)\stubpreamble.h"\
    "$(JDK_OINCL)\sys_api.h"\
    "$(JDK_OINCL)\tree.h"\
    "$(JDK_OINCL)\typecodes.h"\
    "$(JDK_OINCL)\typedefs.h"\
    "$(JDK_MDOINCL)\io_md.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
    "$(JDK_MDOINCL)\oobj_md.h"\
    "$(JDK_MDOINCL)\sysmacros_md.h"\
    "$(JDK_MDOINCL)\timeval_md.h"\
    "$(JDK_MDOINCL)\typedefs_md.h"\
    {$(INCLUDE)}"sys\types.h"\
    

"$(INTDIR)\IluJava_IluConnOps.obj" : $(SOURCE) $(DEP_CPP_ILUJAVA_I) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_ILUJAVA_I=\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilucall.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluclassrep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilumethodrep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluport.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilubatcher.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluserverconnection.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluserializationcontext.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilusurrogateconnection.h"\
    "$(ILUSRC)\runtime\java\ilujava_common.h"\
    "$(ILUSRC)\runtime\java\ilujava_includes.h"\
    "$(ILUSRC)\runtime\java\ilujava_jgc.h"\
    "$(ILUSRC)\runtime\java\ilujava_jmem.h"\
    "$(ILUSRC)\runtime\java\ilujava_jtypes.h"\
    "$(ILUSRC)\runtime\java\ilunative.h"\
    "$(ILUSRC)\runtime\kernel\ilubasic.h"\
    "$(ILUSRC)\runtime\kernel\iluerror.h"\
    "$(ILUSRC)\runtime\kernel\iluerrs.h"\
    "$(ILUSRC)\runtime\kernel\iluhash.h"\
    "$(ILUSRC)\runtime\kernel\ilutpcod.h"\
    "$(ILUSRC)\runtime\kernel\ilutypes.h"\
    "$(ILUSRC)\runtime\kernel\iluwin.h"\
    "$(ILUSRC)\runtime\kernel\iluxport.h"\
    "$(JDK_OINCL)\bool.h"\
    "$(JDK_OINCL)\debug.h"\
    "$(JDK_OINCL)\interpreter.h"\
    "$(JDK_OINCL)\java_lang_string.h"\
    "$(JDK_OINCL)\javastring.h"\
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_OINCL)\native.h"\
    "$(JDK_OINCL)\oobj.h"\
    "$(JDK_OINCL)\signature.h"\
    "$(JDK_OINCL)\stubpreamble.h"\
    "$(JDK_OINCL)\sys_api.h"\
    "$(JDK_OINCL)\tree.h"\
    "$(JDK_OINCL)\typecodes.h"\
    "$(JDK_OINCL)\typedefs.h"\
    "$(JDK_MDOINCL)\io_md.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
    "$(JDK_MDOINCL)\oobj_md.h"\
    "$(JDK_MDOINCL)\sysmacros_md.h"\
    "$(JDK_MDOINCL)\timeval_md.h"\
    "$(JDK_MDOINCL)\typedefs_md.h"\
    

"$(INTDIR)\IluJava_IluConnOps.obj"  "$(INTDIR)\IluJava_IluConnOps.sbr" : \
$(SOURCE) $(DEP_CPP_ILUJAVA_I) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=$(ILUSRC)\runtime\java\IluJava_IluDebug.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_ILUJAVA_IL=\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluclassrep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iludebug.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iludebughooks.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iludebugwriter.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluoint.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilurt0.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluserver.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluserverrelocationinfo.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluserializationcontext.h"\
    "$(ILUSRC)\runtime\java\ilujava_common.h"\
    "$(ILUSRC)\runtime\java\ilujava_includes.h"\
    "$(ILUSRC)\runtime\java\ilujava_jarrays.h"\
    "$(ILUSRC)\runtime\java\ilujava_jmem.h"\
    "$(ILUSRC)\runtime\java\ilujava_jtypes.h"\
    "$(ILUSRC)\runtime\java\ilunative.h"\
    "$(ILUSRC)\runtime\kernel\ilubasic.h"\
    "$(ILUSRC)\runtime\kernel\iluerror.h"\
    "$(ILUSRC)\runtime\kernel\iluerrs.h"\
    "$(ILUSRC)\runtime\kernel\iluhash.h"\
    "$(ILUSRC)\runtime\kernel\ilumac.h"\
    "$(ILUSRC)\runtime\kernel\ilutpcod.h"\
    "$(ILUSRC)\runtime\kernel\ilutypes.h"\
    "$(ILUSRC)\runtime\kernel\iluwin.h"\
    "$(ILUSRC)\runtime\kernel\iluxport.h"\
    "$(JDK_OINCL)\bool.h"\
    "$(JDK_OINCL)\debug.h"\
    "$(JDK_OINCL)\interpreter.h"\
    "$(JDK_OINCL)\java_lang_string.h"\
    "$(JDK_OINCL)\javastring.h"\
    "$(JDK_OINCL)\jcov.h"\
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_OINCL)\native.h"\
    "$(JDK_OINCL)\oobj.h"\
    "$(JDK_OINCL)\signature.h"\
    "$(JDK_OINCL)\stubpreamble.h"\
    "$(JDK_OINCL)\sys_api.h"\
    "$(JDK_OINCL)\tree.h"\
    "$(JDK_OINCL)\typecodes.h"\
    "$(JDK_OINCL)\typedefs.h"\
    "$(JDK_MDOINCL)\io_md.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
    "$(JDK_MDOINCL)\oobj_md.h"\
    "$(JDK_MDOINCL)\sysmacros_md.h"\
    "$(JDK_MDOINCL)\timeval_md.h"\
    "$(JDK_MDOINCL)\typedefs_md.h"\
    {$(INCLUDE)}"sys\types.h"\
    

"$(INTDIR)\IluJava_IluDebug.obj" : $(SOURCE) $(DEP_CPP_ILUJAVA_IL) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_ILUJAVA_IL=\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluclassrep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iludebug.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iludebughooks.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iludebugwriter.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluoint.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilurt0.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluserver.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluserverrelocationinfo.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluserializationcontext.h"\
    "$(ILUSRC)\runtime\java\ilujava_common.h"\
    "$(ILUSRC)\runtime\java\ilujava_includes.h"\
    "$(ILUSRC)\runtime\java\ilujava_jarrays.h"\
    "$(ILUSRC)\runtime\java\ilujava_jmem.h"\
    "$(ILUSRC)\runtime\java\ilujava_jtypes.h"\
    "$(ILUSRC)\runtime\java\ilunative.h"\
    "$(ILUSRC)\runtime\kernel\ilubasic.h"\
    "$(ILUSRC)\runtime\kernel\iluerror.h"\
    "$(ILUSRC)\runtime\kernel\iluerrs.h"\
    "$(ILUSRC)\runtime\kernel\iluhash.h"\
    "$(ILUSRC)\runtime\kernel\ilutpcod.h"\
    "$(ILUSRC)\runtime\kernel\ilutypes.h"\
    "$(ILUSRC)\runtime\kernel\iluwin.h"\
    "$(ILUSRC)\runtime\kernel\iluxport.h"\
    "$(JDK_OINCL)\bool.h"\
    "$(JDK_OINCL)\debug.h"\
    "$(JDK_OINCL)\interpreter.h"\
    "$(JDK_OINCL)\java_lang_string.h"\
    "$(JDK_OINCL)\javastring.h"\
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_OINCL)\native.h"\
    "$(JDK_OINCL)\oobj.h"\
    "$(JDK_OINCL)\signature.h"\
    "$(JDK_OINCL)\stubpreamble.h"\
    "$(JDK_OINCL)\sys_api.h"\
    "$(JDK_OINCL)\tree.h"\
    "$(JDK_OINCL)\typecodes.h"\
    "$(JDK_OINCL)\typedefs.h"\
    "$(JDK_MDOINCL)\io_md.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
    "$(JDK_MDOINCL)\oobj_md.h"\
    "$(JDK_MDOINCL)\sysmacros_md.h"\
    "$(JDK_MDOINCL)\timeval_md.h"\
    "$(JDK_MDOINCL)\typedefs_md.h"\
    

"$(INTDIR)\IluJava_IluDebug.obj"    "$(INTDIR)\IluJava_IluDebug.sbr" : $(SOURCE)\
 $(DEP_CPP_ILUJAVA_IL) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=$(ILUSRC)\runtime\java\IluJava_IluInit.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_ILUJAVA_ILU=\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluinit2.h"\
    "$(ILUSRC)\runtime\java\ilujava_common.h"\
    "$(ILUSRC)\runtime\java\ilujava_includes.h"\
    "$(ILUSRC)\runtime\java\ilujava_jmem.h"\
    "$(ILUSRC)\runtime\java\ilujava_jtypes.h"\
    "$(ILUSRC)\runtime\java\ilunative.h"\
    "$(ILUSRC)\runtime\kernel\ilubasic.h"\
    "$(ILUSRC)\runtime\kernel\iluerror.h"\
    "$(ILUSRC)\runtime\kernel\iluerrs.h"\
    "$(ILUSRC)\runtime\kernel\iluhash.h"\
    "$(ILUSRC)\runtime\kernel\ilumac.h"\
    "$(ILUSRC)\runtime\kernel\ilutpcod.h"\
    "$(ILUSRC)\runtime\kernel\ilutypes.h"\
    "$(ILUSRC)\runtime\kernel\iluwin.h"\
    "$(ILUSRC)\runtime\kernel\iluxport.h"\
    "$(JDK_OINCL)\bool.h"\
    "$(JDK_OINCL)\debug.h"\
    "$(JDK_OINCL)\interpreter.h"\
    "$(JDK_OINCL)\java_lang_string.h"\
    "$(JDK_OINCL)\javastring.h"\
    "$(JDK_OINCL)\jcov.h"\
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_OINCL)\native.h"\
    "$(JDK_OINCL)\oobj.h"\
    "$(JDK_OINCL)\signature.h"\
    "$(JDK_OINCL)\stubpreamble.h"\
    "$(JDK_OINCL)\sys_api.h"\
    "$(JDK_OINCL)\tree.h"\
    "$(JDK_OINCL)\typecodes.h"\
    "$(JDK_OINCL)\typedefs.h"\
    "$(JDK_MDOINCL)\io_md.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
    "$(JDK_MDOINCL)\oobj_md.h"\
    "$(JDK_MDOINCL)\sysmacros_md.h"\
    "$(JDK_MDOINCL)\timeval_md.h"\
    "$(JDK_MDOINCL)\typedefs_md.h"\
    {$(INCLUDE)}"sys\types.h"\
    

"$(INTDIR)\IluJava_IluInit.obj" : $(SOURCE) $(DEP_CPP_ILUJAVA_ILU) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_ILUJAVA_ILU=\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluinit2.h"\
    "$(ILUSRC)\runtime\java\ilujava_common.h"\
    "$(ILUSRC)\runtime\java\ilujava_includes.h"\
    "$(ILUSRC)\runtime\java\ilujava_jmem.h"\
    "$(ILUSRC)\runtime\java\ilujava_jtypes.h"\
    "$(ILUSRC)\runtime\java\ilunative.h"\
    "$(ILUSRC)\runtime\kernel\ilubasic.h"\
    "$(ILUSRC)\runtime\kernel\iluerror.h"\
    "$(ILUSRC)\runtime\kernel\iluerrs.h"\
    "$(ILUSRC)\runtime\kernel\iluhash.h"\
    "$(ILUSRC)\runtime\kernel\ilutpcod.h"\
    "$(ILUSRC)\runtime\kernel\ilutypes.h"\
    "$(ILUSRC)\runtime\kernel\iluwin.h"\
    "$(ILUSRC)\runtime\kernel\iluxport.h"\
    "$(JDK_OINCL)\bool.h"\
    "$(JDK_OINCL)\debug.h"\
    "$(JDK_OINCL)\interpreter.h"\
    "$(JDK_OINCL)\java_lang_string.h"\
    "$(JDK_OINCL)\javastring.h"\
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_OINCL)\native.h"\
    "$(JDK_OINCL)\oobj.h"\
    "$(JDK_OINCL)\signature.h"\
    "$(JDK_OINCL)\stubpreamble.h"\
    "$(JDK_OINCL)\sys_api.h"\
    "$(JDK_OINCL)\tree.h"\
    "$(JDK_OINCL)\typecodes.h"\
    "$(JDK_OINCL)\typedefs.h"\
    "$(JDK_MDOINCL)\io_md.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
    "$(JDK_MDOINCL)\oobj_md.h"\
    "$(JDK_MDOINCL)\sysmacros_md.h"\
    "$(JDK_MDOINCL)\timeval_md.h"\
    "$(JDK_MDOINCL)\typedefs_md.h"\
    

"$(INTDIR)\IluJava_IluInit.obj" "$(INTDIR)\IluJava_IluInit.sbr" : $(SOURCE)\
 $(DEP_CPP_ILUJAVA_ILU) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=$(ILUSRC)\runtime\java\IluJava_IluOInt.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_ILUJAVA_ILUO=\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilucall.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluclassrep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilumethodrep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluobject.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluobjecttable.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluoint.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilupassport.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluidentity.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluidentitytype.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluservicethread.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluport.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilubatcher.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilurt0.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluserver.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluserverrelocationinfo.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluserializationcontext.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilusurrogateobject.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilutransportinfo.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilutyperep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluwpbase.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilupickle.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilutypecode.h"\
    "$(ILUSRC)\runtime\java\ilujava_common.h"\
    "$(ILUSRC)\runtime\java\ilujava_includes.h"\
    "$(ILUSRC)\runtime\java\ilujava_jgc.h"\
    "$(ILUSRC)\runtime\java\ilujava_jmem.h"\
    "$(ILUSRC)\runtime\java\ilujava_jmon.h"\
    "$(ILUSRC)\runtime\java\ilujava_jstubs.h"\
    "$(ILUSRC)\runtime\java\ilujava_jtypes.h"\
    "$(ILUSRC)\runtime\java\ilujava_ops.h"\
    "$(ILUSRC)\runtime\java\ilunative.h"\
    "$(ILUSRC)\runtime\kernel\ilubasic.h"\
    "$(ILUSRC)\runtime\kernel\iluerror.h"\
    "$(ILUSRC)\runtime\kernel\iluerrs.h"\
    "$(ILUSRC)\runtime\kernel\iluhash.h"\
    "$(ILUSRC)\runtime\kernel\ilumac.h"\
    "$(ILUSRC)\runtime\kernel\ilutpcod.h"\
    "$(ILUSRC)\runtime\kernel\ilutypes.h"\
    "$(ILUSRC)\runtime\kernel\iluwin.h"\
    "$(ILUSRC)\runtime\kernel\iluxport.h"\
    "$(ILUSRC)\runtime\kernel\threads.h"\
    "$(JDK_OINCL)\bool.h"\
    "$(JDK_OINCL)\debug.h"\
    "$(JDK_OINCL)\interpreter.h"\
    "$(JDK_OINCL)\java_lang_string.h"\
    "$(JDK_OINCL)\java_lang_thread.h"\
    "$(JDK_OINCL)\javastring.h"\
    "$(JDK_OINCL)\jcov.h"\
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_OINCL)\monitor.h"\
    "$(JDK_OINCL)\native.h"\
    "$(JDK_OINCL)\oobj.h"\
    "$(JDK_OINCL)\signature.h"\
    "$(JDK_OINCL)\stubpreamble.h"\
    "$(JDK_OINCL)\sys_api.h"\
    "$(JDK_OINCL)\tree.h"\
    "$(JDK_OINCL)\typecodes.h"\
    "$(JDK_OINCL)\typedefs.h"\
    "$(JDK_MDOINCL)\io_md.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
    "$(JDK_MDOINCL)\oobj_md.h"\
    "$(JDK_MDOINCL)\sysmacros_md.h"\
    "$(JDK_MDOINCL)\timeval_md.h"\
    "$(JDK_MDOINCL)\typedefs_md.h"\
    {$(INCLUDE)}"sys\types.h"\
    

"$(INTDIR)\IluJava_IluOInt.obj" : $(SOURCE) $(DEP_CPP_ILUJAVA_ILUO) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_ILUJAVA_ILUO=\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilucall.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluclassrep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilumethodrep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluobject.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluobjecttable.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluoint.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilupassport.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluidentity.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluidentitytype.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluservicethread.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluport.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilubatcher.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilurt0.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluserver.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluserverrelocationinfo.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluserializationcontext.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilusurrogateobject.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilutransportinfo.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilutyperep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluwpbase.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilupickle.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilutypecode.h"\
    "$(ILUSRC)\runtime\java\ilujava_common.h"\
    "$(ILUSRC)\runtime\java\ilujava_includes.h"\
    "$(ILUSRC)\runtime\java\ilujava_jgc.h"\
    "$(ILUSRC)\runtime\java\ilujava_jmem.h"\
    "$(ILUSRC)\runtime\java\ilujava_jmon.h"\
    "$(ILUSRC)\runtime\java\ilujava_jstubs.h"\
    "$(ILUSRC)\runtime\java\ilujava_jtypes.h"\
    "$(ILUSRC)\runtime\java\ilujava_ops.h"\
    "$(ILUSRC)\runtime\java\ilunative.h"\
    "$(ILUSRC)\runtime\kernel\ilubasic.h"\
    "$(ILUSRC)\runtime\kernel\iluerror.h"\
    "$(ILUSRC)\runtime\kernel\iluerrs.h"\
    "$(ILUSRC)\runtime\kernel\iluhash.h"\
    "$(ILUSRC)\runtime\kernel\ilutpcod.h"\
    "$(ILUSRC)\runtime\kernel\ilutypes.h"\
    "$(ILUSRC)\runtime\kernel\iluwin.h"\
    "$(ILUSRC)\runtime\kernel\iluxport.h"\
    "$(JDK_OINCL)\bool.h"\
    "$(JDK_OINCL)\debug.h"\
    "$(JDK_OINCL)\interpreter.h"\
    "$(JDK_OINCL)\java_lang_string.h"\
    "$(JDK_OINCL)\javastring.h"\
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_OINCL)\monitor.h"\
    "$(JDK_OINCL)\native.h"\
    "$(JDK_OINCL)\oobj.h"\
    "$(JDK_OINCL)\signature.h"\
    "$(JDK_OINCL)\stubpreamble.h"\
    "$(JDK_OINCL)\sys_api.h"\
    "$(JDK_OINCL)\tree.h"\
    "$(JDK_OINCL)\typecodes.h"\
    "$(JDK_OINCL)\typedefs.h"\
    "$(JDK_MDOINCL)\io_md.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
    "$(JDK_MDOINCL)\oobj_md.h"\
    "$(JDK_MDOINCL)\sysmacros_md.h"\
    "$(JDK_MDOINCL)\timeval_md.h"\
    "$(JDK_MDOINCL)\typedefs_md.h"\
    

"$(INTDIR)\IluJava_IluOInt.obj" "$(INTDIR)\IluJava_IluOInt.sbr" : $(SOURCE)\
 $(DEP_CPP_ILUJAVA_ILUO) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=$(ILUSRC)\runtime\java\IluJava_IluPort.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_ILUJAVA_ILUP=\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilucall.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluclassrep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilumethodrep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluobject.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluobjecttable.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluoint.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilupassport.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluidentity.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluidentitytype.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluservicethread.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluport.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilubatcher.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilurt0.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilusbh.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluserver.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluserverrelocationinfo.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluserializationcontext.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilusurrogateobject.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilutransportinfo.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilutyperep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluwpbase.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilupickle.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilutypecode.h"\
    "$(ILUSRC)\runtime\java\ilujava_common.h"\
    "$(ILUSRC)\runtime\java\ilujava_includes.h"\
    "$(ILUSRC)\runtime\java\ilujava_jarrays.h"\
    "$(ILUSRC)\runtime\java\ilujava_jmem.h"\
    "$(ILUSRC)\runtime\java\ilujava_jstubs.h"\
    "$(ILUSRC)\runtime\java\ilujava_jtypes.h"\
    "$(ILUSRC)\runtime\java\ilunative.h"\
    "$(ILUSRC)\runtime\kernel\ilubasic.h"\
    "$(ILUSRC)\runtime\kernel\iluerror.h"\
    "$(ILUSRC)\runtime\kernel\iluerrs.h"\
    "$(ILUSRC)\runtime\kernel\iluhash.h"\
    "$(ILUSRC)\runtime\kernel\ilumac.h"\
    "$(ILUSRC)\runtime\kernel\ilutpcod.h"\
    "$(ILUSRC)\runtime\kernel\ilutypes.h"\
    "$(ILUSRC)\runtime\kernel\iluwin.h"\
    "$(ILUSRC)\runtime\kernel\iluxport.h"\
    "$(JDK_OINCL)\bool.h"\
    "$(JDK_OINCL)\debug.h"\
    "$(JDK_OINCL)\interpreter.h"\
    "$(JDK_OINCL)\java_lang_string.h"\
    "$(JDK_OINCL)\javastring.h"\
    "$(JDK_OINCL)\jcov.h"\
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_OINCL)\native.h"\
    "$(JDK_OINCL)\oobj.h"\
    "$(JDK_OINCL)\signature.h"\
    "$(JDK_OINCL)\stubpreamble.h"\
    "$(JDK_OINCL)\sys_api.h"\
    "$(JDK_OINCL)\tree.h"\
    "$(JDK_OINCL)\typecodes.h"\
    "$(JDK_OINCL)\typedefs.h"\
    "$(JDK_MDOINCL)\io_md.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
    "$(JDK_MDOINCL)\oobj_md.h"\
    "$(JDK_MDOINCL)\sysmacros_md.h"\
    "$(JDK_MDOINCL)\timeval_md.h"\
    "$(JDK_MDOINCL)\typedefs_md.h"\
    {$(INCLUDE)}"sys\types.h"\
    

"$(INTDIR)\IluJava_IluPort.obj" : $(SOURCE) $(DEP_CPP_ILUJAVA_ILUP) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_ILUJAVA_ILUP=\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilucall.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluclassrep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilumethodrep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluobject.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluobjecttable.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluoint.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilupassport.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluidentity.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluidentitytype.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluservicethread.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluport.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilubatcher.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilurt0.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilusbh.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluserver.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluserverrelocationinfo.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluserializationcontext.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilusurrogateobject.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilutransportinfo.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilutyperep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluwpbase.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilupickle.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilutypecode.h"\
    "$(ILUSRC)\runtime\java\ilujava_common.h"\
    "$(ILUSRC)\runtime\java\ilujava_includes.h"\
    "$(ILUSRC)\runtime\java\ilujava_jarrays.h"\
    "$(ILUSRC)\runtime\java\ilujava_jmem.h"\
    "$(ILUSRC)\runtime\java\ilujava_jstubs.h"\
    "$(ILUSRC)\runtime\java\ilujava_jtypes.h"\
    "$(ILUSRC)\runtime\java\ilunative.h"\
    "$(ILUSRC)\runtime\kernel\ilubasic.h"\
    "$(ILUSRC)\runtime\kernel\iluerror.h"\
    "$(ILUSRC)\runtime\kernel\iluerrs.h"\
    "$(ILUSRC)\runtime\kernel\iluhash.h"\
    "$(ILUSRC)\runtime\kernel\ilutpcod.h"\
    "$(ILUSRC)\runtime\kernel\ilutypes.h"\
    "$(ILUSRC)\runtime\kernel\iluwin.h"\
    "$(ILUSRC)\runtime\kernel\iluxport.h"\
    "$(JDK_OINCL)\bool.h"\
    "$(JDK_OINCL)\debug.h"\
    "$(JDK_OINCL)\interpreter.h"\
    "$(JDK_OINCL)\java_lang_string.h"\
    "$(JDK_OINCL)\javastring.h"\
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_OINCL)\native.h"\
    "$(JDK_OINCL)\oobj.h"\
    "$(JDK_OINCL)\signature.h"\
    "$(JDK_OINCL)\stubpreamble.h"\
    "$(JDK_OINCL)\sys_api.h"\
    "$(JDK_OINCL)\tree.h"\
    "$(JDK_OINCL)\typecodes.h"\
    "$(JDK_OINCL)\typedefs.h"\
    "$(JDK_MDOINCL)\io_md.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
    "$(JDK_MDOINCL)\oobj_md.h"\
    "$(JDK_MDOINCL)\sysmacros_md.h"\
    "$(JDK_MDOINCL)\timeval_md.h"\
    "$(JDK_MDOINCL)\typedefs_md.h"\
    

"$(INTDIR)\IluJava_IluPort.obj" "$(INTDIR)\IluJava_IluPort.sbr" : $(SOURCE)\
 $(DEP_CPP_ILUJAVA_ILUP) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=$(ILUSRC)\runtime\java\IluJava_IluRT0.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_ILUJAVA_ILUR=\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilucall.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluclassrep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilumethodrep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluobject.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluobjecttable.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluoint.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilupassport.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluidentity.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluidentitytype.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluservicethread.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluport.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilubatcher.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilurt0.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluserver.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluserverrelocationinfo.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluserializationcontext.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilusurrogateobject.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilutransportinfo.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilutyperep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluwpbase.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilupickle.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilutypecode.h"\
    "$(ILUSRC)\runtime\java\ilujava_common.h"\
    "$(ILUSRC)\runtime\java\ilujava_includes.h"\
    "$(ILUSRC)\runtime\java\ilujava_jgc.h"\
    "$(ILUSRC)\runtime\java\ilujava_jmem.h"\
    "$(ILUSRC)\runtime\java\ilujava_jstubs.h"\
    "$(ILUSRC)\runtime\java\ilujava_jtypes.h"\
    "$(ILUSRC)\runtime\java\ilujava_ops.h"\
    "$(ILUSRC)\runtime\java\ilunative.h"\
    "$(ILUSRC)\runtime\kernel\ilubasic.h"\
    "$(ILUSRC)\runtime\kernel\iludebug.h"\
    "$(ILUSRC)\runtime\kernel\iluerror.h"\
    "$(ILUSRC)\runtime\kernel\iluerrs.h"\
    "$(ILUSRC)\runtime\kernel\iluhash.h"\
    "$(ILUSRC)\runtime\kernel\ilumac.h"\
    "$(ILUSRC)\runtime\kernel\iluntrnl.h"\
    "$(ILUSRC)\runtime\kernel\ilutpcod.h"\
    "$(ILUSRC)\runtime\kernel\ilutypes.h"\
    "$(ILUSRC)\runtime\kernel\iluwin.h"\
    "$(ILUSRC)\runtime\kernel\iluxport.h"\
    "$(ILUSRC)\runtime\kernel\ilutransport.h"\
    "$(JDK_OINCL)\bool.h"\
    "$(JDK_OINCL)\debug.h"\
    "$(JDK_OINCL)\interpreter.h"\
    "$(JDK_OINCL)\java_lang_string.h"\
    "$(JDK_OINCL)\javastring.h"\
    "$(JDK_OINCL)\jcov.h"\
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_OINCL)\native.h"\
    "$(JDK_OINCL)\oobj.h"\
    "$(JDK_OINCL)\signature.h"\
    "$(JDK_OINCL)\stubpreamble.h"\
    "$(JDK_OINCL)\sys_api.h"\
    "$(JDK_OINCL)\tree.h"\
    "$(JDK_OINCL)\typecodes.h"\
    "$(JDK_OINCL)\typedefs.h"\
    "$(JDK_MDOINCL)\io_md.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
    "$(JDK_MDOINCL)\oobj_md.h"\
    "$(JDK_MDOINCL)\sysmacros_md.h"\
    "$(JDK_MDOINCL)\timeval_md.h"\
    "$(JDK_MDOINCL)\typedefs_md.h"\
    {$(INCLUDE)}"sys\types.h"\
    

"$(INTDIR)\IluJava_IluRT0.obj" : $(SOURCE) $(DEP_CPP_ILUJAVA_ILUR) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_ILUJAVA_ILUR=\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilucall.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluclassrep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilumethodrep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluobject.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluobjecttable.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluoint.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilupassport.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluidentity.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluidentitytype.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluservicethread.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluport.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilubatcher.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilurt0.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluserver.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluserverrelocationinfo.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluserializationcontext.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilusurrogateobject.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilutransportinfo.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilutyperep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluwpbase.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilupickle.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilutypecode.h"\
    "$(ILUSRC)\runtime\java\ilujava_common.h"\
    "$(ILUSRC)\runtime\java\ilujava_includes.h"\
    "$(ILUSRC)\runtime\java\ilujava_jgc.h"\
    "$(ILUSRC)\runtime\java\ilujava_jmem.h"\
    "$(ILUSRC)\runtime\java\ilujava_jstubs.h"\
    "$(ILUSRC)\runtime\java\ilujava_jtypes.h"\
    "$(ILUSRC)\runtime\java\ilujava_ops.h"\
    "$(ILUSRC)\runtime\java\ilunative.h"\
    "$(ILUSRC)\runtime\kernel\ilubasic.h"\
    "$(ILUSRC)\runtime\kernel\iludebug.h"\
    "$(ILUSRC)\runtime\kernel\iluerror.h"\
    "$(ILUSRC)\runtime\kernel\iluerrs.h"\
    "$(ILUSRC)\runtime\kernel\iluhash.h"\
    "$(ILUSRC)\runtime\kernel\iluntrnl.h"\
    "$(ILUSRC)\runtime\kernel\ilutpcod.h"\
    "$(ILUSRC)\runtime\kernel\ilutypes.h"\
    "$(ILUSRC)\runtime\kernel\iluwin.h"\
    "$(ILUSRC)\runtime\kernel\iluxport.h"\
    "$(ILUSRC)\runtime\kernel\ilutransport.h"\
    "$(JDK_OINCL)\bool.h"\
    "$(JDK_OINCL)\debug.h"\
    "$(JDK_OINCL)\interpreter.h"\
    "$(JDK_OINCL)\java_lang_string.h"\
    "$(JDK_OINCL)\javastring.h"\
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_OINCL)\native.h"\
    "$(JDK_OINCL)\oobj.h"\
    "$(JDK_OINCL)\signature.h"\
    "$(JDK_OINCL)\stubpreamble.h"\
    "$(JDK_OINCL)\sys_api.h"\
    "$(JDK_OINCL)\tree.h"\
    "$(JDK_OINCL)\typecodes.h"\
    "$(JDK_OINCL)\typedefs.h"\
    "$(JDK_MDOINCL)\io_md.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
    "$(JDK_MDOINCL)\oobj_md.h"\
    "$(JDK_MDOINCL)\sysmacros_md.h"\
    "$(JDK_MDOINCL)\timeval_md.h"\
    "$(JDK_MDOINCL)\typedefs_md.h"\
    

"$(INTDIR)\IluJava_IluRT0.obj"  "$(INTDIR)\IluJava_IluRT0.sbr" : $(SOURCE)\
 $(DEP_CPP_ILUJAVA_ILUR) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=$(ILUSRC)\runtime\java\IluJava_IluServer.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_ILUJAVA_ILUS=\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilucall.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluclassrep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilumethodrep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluobject.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluobjecttable.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluoint.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilupassport.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluidentity.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluidentitytype.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluservicethread.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluport.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilubatcher.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilurt0.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluserver.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluserverrelocationinfo.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluserializationcontext.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilusurrogateobject.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilutransportinfo.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilutyperep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluwpbase.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilupickle.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilutypecode.h"\
    "$(ILUSRC)\runtime\java\ilujava_common.h"\
    "$(ILUSRC)\runtime\java\ilujava_includes.h"\
    "$(ILUSRC)\runtime\java\ilujava_jgc.h"\
    "$(ILUSRC)\runtime\java\ilujava_jmem.h"\
    "$(ILUSRC)\runtime\java\ilujava_jmon.h"\
    "$(ILUSRC)\runtime\java\ilujava_jstubs.h"\
    "$(ILUSRC)\runtime\java\ilujava_jtypes.h"\
    "$(ILUSRC)\runtime\java\ilunative.h"\
    "$(ILUSRC)\runtime\kernel\ilubasic.h"\
    "$(ILUSRC)\runtime\kernel\iludebug.h"\
    "$(ILUSRC)\runtime\kernel\iluerror.h"\
    "$(ILUSRC)\runtime\kernel\iluerrs.h"\
    "$(ILUSRC)\runtime\kernel\iluhash.h"\
    "$(ILUSRC)\runtime\kernel\ilumac.h"\
    "$(ILUSRC)\runtime\kernel\iluntrnl.h"\
    "$(ILUSRC)\runtime\kernel\ilutpcod.h"\
    "$(ILUSRC)\runtime\kernel\ilutypes.h"\
    "$(ILUSRC)\runtime\kernel\iluwin.h"\
    "$(ILUSRC)\runtime\kernel\iluxport.h"\
    "$(ILUSRC)\runtime\kernel\threads.h"\
    "$(ILUSRC)\runtime\kernel\ilutransport.h"\
    "$(JDK_OINCL)\bool.h"\
    "$(JDK_OINCL)\debug.h"\
    "$(JDK_OINCL)\interpreter.h"\
    "$(JDK_OINCL)\java_lang_string.h"\
    "$(JDK_OINCL)\java_lang_thread.h"\
    "$(JDK_OINCL)\javastring.h"\
    "$(JDK_OINCL)\jcov.h"\
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_OINCL)\monitor.h"\
    "$(JDK_OINCL)\native.h"\
    "$(JDK_OINCL)\oobj.h"\
    "$(JDK_OINCL)\signature.h"\
    "$(JDK_OINCL)\stubpreamble.h"\
    "$(JDK_OINCL)\sys_api.h"\
    "$(JDK_OINCL)\tree.h"\
    "$(JDK_OINCL)\typecodes.h"\
    "$(JDK_OINCL)\typedefs.h"\
    "$(JDK_MDOINCL)\io_md.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
    "$(JDK_MDOINCL)\oobj_md.h"\
    "$(JDK_MDOINCL)\sysmacros_md.h"\
    "$(JDK_MDOINCL)\timeval_md.h"\
    "$(JDK_MDOINCL)\typedefs_md.h"\
    {$(INCLUDE)}"sys\types.h"\
    

"$(INTDIR)\IluJava_IluServer.obj" : $(SOURCE) $(DEP_CPP_ILUJAVA_ILUS)\
 "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_ILUJAVA_ILUS=\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilucall.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluclassrep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilumethodrep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluobject.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluobjecttable.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluoint.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilupassport.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluidentity.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluidentitytype.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluservicethread.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluport.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilubatcher.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilurt0.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluserver.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluserverrelocationinfo.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluserializationcontext.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilusurrogateobject.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilutransportinfo.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilutyperep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluwpbase.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilupickle.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilutypecode.h"\
    "$(ILUSRC)\runtime\java\ilujava_common.h"\
    "$(ILUSRC)\runtime\java\ilujava_includes.h"\
    "$(ILUSRC)\runtime\java\ilujava_jgc.h"\
    "$(ILUSRC)\runtime\java\ilujava_jmem.h"\
    "$(ILUSRC)\runtime\java\ilujava_jmon.h"\
    "$(ILUSRC)\runtime\java\ilujava_jstubs.h"\
    "$(ILUSRC)\runtime\java\ilujava_jtypes.h"\
    "$(ILUSRC)\runtime\java\ilunative.h"\
    "$(ILUSRC)\runtime\kernel\ilubasic.h"\
    "$(ILUSRC)\runtime\kernel\iludebug.h"\
    "$(ILUSRC)\runtime\kernel\iluerror.h"\
    "$(ILUSRC)\runtime\kernel\iluerrs.h"\
    "$(ILUSRC)\runtime\kernel\iluhash.h"\
    "$(ILUSRC)\runtime\kernel\iluntrnl.h"\
    "$(ILUSRC)\runtime\kernel\ilutpcod.h"\
    "$(ILUSRC)\runtime\kernel\ilutypes.h"\
    "$(ILUSRC)\runtime\kernel\iluwin.h"\
    "$(ILUSRC)\runtime\kernel\iluxport.h"\
    "$(ILUSRC)\runtime\kernel\ilutransport.h"\
    "$(JDK_OINCL)\bool.h"\
    "$(JDK_OINCL)\debug.h"\
    "$(JDK_OINCL)\interpreter.h"\
    "$(JDK_OINCL)\java_lang_string.h"\
    "$(JDK_OINCL)\javastring.h"\
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_OINCL)\monitor.h"\
    "$(JDK_OINCL)\native.h"\
    "$(JDK_OINCL)\oobj.h"\
    "$(JDK_OINCL)\signature.h"\
    "$(JDK_OINCL)\stubpreamble.h"\
    "$(JDK_OINCL)\sys_api.h"\
    "$(JDK_OINCL)\tree.h"\
    "$(JDK_OINCL)\typecodes.h"\
    "$(JDK_OINCL)\typedefs.h"\
    "$(JDK_MDOINCL)\io_md.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
    "$(JDK_MDOINCL)\oobj_md.h"\
    "$(JDK_MDOINCL)\sysmacros_md.h"\
    "$(JDK_MDOINCL)\timeval_md.h"\
    "$(JDK_MDOINCL)\typedefs_md.h"\
    

"$(INTDIR)\IluJava_IluServer.obj"   "$(INTDIR)\IluJava_IluServer.sbr" : $(SOURCE)\
 $(DEP_CPP_ILUJAVA_ILUS) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=$(ILUSRC)\runtime\java\IluJava_JOps.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_ILUJAVA_J=\
    "$(ILUSRC)\runtime\java\ilujava_includes.h"\
    "$(ILUSRC)\runtime\java\ilujava_jarrays.h"\
    "$(ILUSRC)\runtime\java\ilujava_jops.h"\
    "$(ILUSRC)\runtime\java\ilujava_jtypes.h"\
    "$(ILUSRC)\runtime\kernel\ilubasic.h"\
    "$(ILUSRC)\runtime\kernel\iluerror.h"\
    "$(ILUSRC)\runtime\kernel\iluerrs.h"\
    "$(ILUSRC)\runtime\kernel\iluhash.h"\
    "$(ILUSRC)\runtime\kernel\ilumac.h"\
    "$(ILUSRC)\runtime\kernel\ilutpcod.h"\
    "$(ILUSRC)\runtime\kernel\ilutypes.h"\
    "$(ILUSRC)\runtime\kernel\iluwin.h"\
    "$(ILUSRC)\runtime\kernel\iluxport.h"\
    "$(JDK_OINCL)\bool.h"\
    "$(JDK_OINCL)\debug.h"\
    "$(JDK_OINCL)\interpreter.h"\
    "$(JDK_OINCL)\java_lang_string.h"\
    "$(JDK_OINCL)\javastring.h"\
    "$(JDK_OINCL)\jcov.h"\
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_OINCL)\native.h"\
    "$(JDK_OINCL)\oobj.h"\
    "$(JDK_OINCL)\signature.h"\
    "$(JDK_OINCL)\stubpreamble.h"\
    "$(JDK_OINCL)\tree.h"\
    "$(JDK_OINCL)\typecodes.h"\
    "$(JDK_OINCL)\typedefs.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
    "$(JDK_MDOINCL)\oobj_md.h"\
    "$(JDK_MDOINCL)\typedefs_md.h"\
    

"$(INTDIR)\IluJava_JOps.obj" : $(SOURCE) $(DEP_CPP_ILUJAVA_J) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_ILUJAVA_J=\
    "$(ILUSRC)\runtime\java\ilujava_includes.h"\
    "$(ILUSRC)\runtime\java\ilujava_jarrays.h"\
    "$(ILUSRC)\runtime\java\ilujava_jops.h"\
    "$(ILUSRC)\runtime\java\ilujava_jtypes.h"\
    "$(ILUSRC)\runtime\kernel\ilubasic.h"\
    "$(ILUSRC)\runtime\kernel\iluerror.h"\
    "$(ILUSRC)\runtime\kernel\iluerrs.h"\
    "$(ILUSRC)\runtime\kernel\iluhash.h"\
    "$(ILUSRC)\runtime\kernel\ilutpcod.h"\
    "$(ILUSRC)\runtime\kernel\ilutypes.h"\
    "$(ILUSRC)\runtime\kernel\iluwin.h"\
    "$(ILUSRC)\runtime\kernel\iluxport.h"\
    "$(JDK_OINCL)\bool.h"\
    "$(JDK_OINCL)\debug.h"\
    "$(JDK_OINCL)\interpreter.h"\
    "$(JDK_OINCL)\java_lang_string.h"\
    "$(JDK_OINCL)\javastring.h"\
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_OINCL)\native.h"\
    "$(JDK_OINCL)\oobj.h"\
    "$(JDK_OINCL)\signature.h"\
    "$(JDK_OINCL)\stubpreamble.h"\
    "$(JDK_OINCL)\tree.h"\
    "$(JDK_OINCL)\typecodes.h"\
    "$(JDK_OINCL)\typedefs.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
    "$(JDK_MDOINCL)\oobj_md.h"\
    "$(JDK_MDOINCL)\typedefs_md.h"\
    

"$(INTDIR)\IluJava_JOps.obj"    "$(INTDIR)\IluJava_JOps.sbr" : $(SOURCE)\
 $(DEP_CPP_ILUJAVA_J) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=$(ILUSRC)\runtime\java\IluJava_JMon.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_ILUJAVA_J=\
    "$(ILUSRC)\runtime\java\ilujava_includes.h"\
    "$(ILUSRC)\runtime\java\ilujava_jarrays.h"\
    "$(ILUSRC)\runtime\java\ilujava_jops.h"\
    "$(ILUSRC)\runtime\java\ilujava_jtypes.h"\
    "$(ILUSRC)\runtime\kernel\ilubasic.h"\
    "$(ILUSRC)\runtime\kernel\iluerror.h"\
    "$(ILUSRC)\runtime\kernel\iluerrs.h"\
    "$(ILUSRC)\runtime\kernel\iluhash.h"\
    "$(ILUSRC)\runtime\kernel\ilumac.h"\
    "$(ILUSRC)\runtime\kernel\ilutpcod.h"\
    "$(ILUSRC)\runtime\kernel\ilutypes.h"\
    "$(ILUSRC)\runtime\kernel\iluwin.h"\
    "$(ILUSRC)\runtime\kernel\iluxport.h"\
    "$(JDK_OINCL)\bool.h"\
    "$(JDK_OINCL)\debug.h"\
    "$(JDK_OINCL)\interpreter.h"\
    "$(JDK_OINCL)\java_lang_string.h"\
    "$(JDK_OINCL)\javastring.h"\
    "$(JDK_OINCL)\jcov.h"\
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_OINCL)\native.h"\
    "$(JDK_OINCL)\oobj.h"\
    "$(JDK_OINCL)\signature.h"\
    "$(JDK_OINCL)\stubpreamble.h"\
    "$(JDK_OINCL)\tree.h"\
    "$(JDK_OINCL)\typecodes.h"\
    "$(JDK_OINCL)\typedefs.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
    "$(JDK_MDOINCL)\oobj_md.h"\
    "$(JDK_MDOINCL)\typedefs_md.h"\
    

"$(INTDIR)\IluJava_JMon.obj" : $(SOURCE) $(DEP_CPP_ILUJAVA_J) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_ILUJAVA_J=\
    "$(ILUSRC)\runtime\java\ilujava_includes.h"\
    "$(ILUSRC)\runtime\java\ilujava_jarrays.h"\
    "$(ILUSRC)\runtime\java\ilujava_jops.h"\
    "$(ILUSRC)\runtime\java\ilujava_jtypes.h"\
    "$(ILUSRC)\runtime\kernel\ilubasic.h"\
    "$(ILUSRC)\runtime\kernel\iluerror.h"\
    "$(ILUSRC)\runtime\kernel\iluerrs.h"\
    "$(ILUSRC)\runtime\kernel\iluhash.h"\
    "$(ILUSRC)\runtime\kernel\ilutpcod.h"\
    "$(ILUSRC)\runtime\kernel\ilutypes.h"\
    "$(ILUSRC)\runtime\kernel\iluwin.h"\
    "$(ILUSRC)\runtime\kernel\iluxport.h"\
    "$(JDK_OINCL)\bool.h"\
    "$(JDK_OINCL)\debug.h"\
    "$(JDK_OINCL)\interpreter.h"\
    "$(JDK_OINCL)\java_lang_string.h"\
    "$(JDK_OINCL)\javastring.h"\
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_OINCL)\native.h"\
    "$(JDK_OINCL)\oobj.h"\
    "$(JDK_OINCL)\signature.h"\
    "$(JDK_OINCL)\stubpreamble.h"\
    "$(JDK_OINCL)\tree.h"\
    "$(JDK_OINCL)\typecodes.h"\
    "$(JDK_OINCL)\typedefs.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
    "$(JDK_MDOINCL)\oobj_md.h"\
    "$(JDK_MDOINCL)\typedefs_md.h"\
    

"$(INTDIR)\IluJava_JMon.obj"    "$(INTDIR)\IluJava_JMon.sbr" : $(SOURCE)\
 $(DEP_CPP_ILUJAVA_J) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=$(ILUSRC)\runtime\java\IluJava_LockTech.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_ILUJAVA_L=\
    "$(ILUSRC)\runtime\java\ilujava_common.h"\
    "$(ILUSRC)\runtime\java\ilujava_includes.h"\
    "$(ILUSRC)\runtime\java\ilujava_jmem.h"\
    "$(ILUSRC)\runtime\java\ilujava_jmon.h"\
    "$(ILUSRC)\runtime\java\ilujava_jtypes.h"\
    "$(ILUSRC)\runtime\java\ilunative.h"\
    "$(ILUSRC)\runtime\kernel\ilubasic.h"\
    "$(ILUSRC)\runtime\kernel\iluerror.h"\
    "$(ILUSRC)\runtime\kernel\iluerrs.h"\
    "$(ILUSRC)\runtime\kernel\iluhash.h"\
    "$(ILUSRC)\runtime\kernel\ilumac.h"\
    "$(ILUSRC)\runtime\kernel\ilutpcod.h"\
    "$(ILUSRC)\runtime\kernel\ilutypes.h"\
    "$(ILUSRC)\runtime\kernel\iluwin.h"\
    "$(ILUSRC)\runtime\kernel\iluxport.h"\
    "$(ILUSRC)\runtime\kernel\threads.h"\
    "$(JDK_OINCL)\bool.h"\
    "$(JDK_OINCL)\debug.h"\
    "$(JDK_OINCL)\interpreter.h"\
    "$(JDK_OINCL)\java_lang_string.h"\
    "$(JDK_OINCL)\java_lang_thread.h"\
    "$(JDK_OINCL)\javastring.h"\
    "$(JDK_OINCL)\jcov.h"\
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_OINCL)\monitor.h"\
    "$(JDK_OINCL)\native.h"\
    "$(JDK_OINCL)\oobj.h"\
    "$(JDK_OINCL)\signature.h"\
    "$(JDK_OINCL)\stubpreamble.h"\
    "$(JDK_OINCL)\sys_api.h"\
    "$(JDK_OINCL)\tree.h"\
    "$(JDK_OINCL)\typecodes.h"\
    "$(JDK_OINCL)\typedefs.h"\
    "$(JDK_MDOINCL)\io_md.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
    "$(JDK_MDOINCL)\oobj_md.h"\
    "$(JDK_MDOINCL)\sysmacros_md.h"\
    "$(JDK_MDOINCL)\timeval_md.h"\
    "$(JDK_MDOINCL)\typedefs_md.h"\
    {$(INCLUDE)}"sys\types.h"\
    

"$(INTDIR)\IluJava_LockTech.obj" : $(SOURCE) $(DEP_CPP_ILUJAVA_L) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_ILUJAVA_L=\
    "$(ILUSRC)\runtime\java\ilujava_common.h"\
    "$(ILUSRC)\runtime\java\ilujava_includes.h"\
    "$(ILUSRC)\runtime\java\ilujava_jmem.h"\
    "$(ILUSRC)\runtime\java\ilujava_jmon.h"\
    "$(ILUSRC)\runtime\java\ilujava_jtypes.h"\
    "$(ILUSRC)\runtime\java\ilunative.h"\
    "$(ILUSRC)\runtime\kernel\ilubasic.h"\
    "$(ILUSRC)\runtime\kernel\iluerror.h"\
    "$(ILUSRC)\runtime\kernel\iluerrs.h"\
    "$(ILUSRC)\runtime\kernel\iluhash.h"\
    "$(ILUSRC)\runtime\kernel\ilutpcod.h"\
    "$(ILUSRC)\runtime\kernel\ilutypes.h"\
    "$(ILUSRC)\runtime\kernel\iluwin.h"\
    "$(ILUSRC)\runtime\kernel\iluxport.h"\
    "$(JDK_OINCL)\bool.h"\
    "$(JDK_OINCL)\debug.h"\
    "$(JDK_OINCL)\interpreter.h"\
    "$(JDK_OINCL)\java_lang_string.h"\
    "$(JDK_OINCL)\java_lang_thread.h"\
    "$(JDK_OINCL)\java_lang_threadgroup.h"\
    "$(JDK_OINCL)\javastring.h"\
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_OINCL)\monitor.h"\
    "$(JDK_OINCL)\native.h"\
    "$(JDK_OINCL)\oobj.h"\
    "$(JDK_OINCL)\signature.h"\
    "$(JDK_OINCL)\stubpreamble.h"\
    "$(JDK_OINCL)\sys_api.h"\
    "$(JDK_OINCL)\threads.h"\
    "$(JDK_OINCL)\tree.h"\
    "$(JDK_OINCL)\typecodes.h"\
    "$(JDK_OINCL)\typedefs.h"\
    "$(JDK_MDOINCL)\io_md.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
    "$(JDK_MDOINCL)\oobj_md.h"\
    "$(JDK_MDOINCL)\sysmacros_md.h"\
    "$(JDK_MDOINCL)\timeval_md.h"\
    "$(JDK_MDOINCL)\typedefs_md.h"\
    

"$(INTDIR)\IluJava_LockTech.obj"    "$(INTDIR)\IluJava_LockTech.sbr" : $(SOURCE)\
 $(DEP_CPP_ILUJAVA_L) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=$(ILUSRC)\runtime\java\IluJava_IluPickle.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_ILUJAVA_P=\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilucall.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluclassrep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilumethodrep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluobject.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluobjecttable.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluoint.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilupassport.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluidentity.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluidentitytype.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluservicethread.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluport.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilubatcher.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilurt0.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluserver.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluserverrelocationinfo.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluserializationcontext.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilusurrogateobject.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilutransportinfo.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilutyperep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluwpbase.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilupickle.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilutypecode.h"\
    "$(ILUSRC)\runtime\java\ilujava_callmacros.h"\
    "$(ILUSRC)\runtime\java\ilujava_common.h"\
    "$(ILUSRC)\runtime\java\ilujava_includes.h"\
    "$(ILUSRC)\runtime\java\ilujava_jarrays.h"\
    "$(ILUSRC)\runtime\java\ilujava_jmem.h"\
    "$(ILUSRC)\runtime\java\ilujava_jops.h"\
    "$(ILUSRC)\runtime\java\ilujava_jstubs.h"\
    "$(ILUSRC)\runtime\java\ilujava_jtypes.h"\
    "$(ILUSRC)\runtime\java\ilujava_ops.h"\
    "$(ILUSRC)\runtime\java\ilunative.h"\
    "$(ILUSRC)\runtime\kernel\ilubasic.h"\
    "$(ILUSRC)\runtime\kernel\iluerror.h"\
    "$(ILUSRC)\runtime\kernel\iluerrs.h"\
    "$(ILUSRC)\runtime\kernel\iluhash.h"\
    "$(ILUSRC)\runtime\kernel\ilumac.h"\
    "$(ILUSRC)\runtime\kernel\ilutpcod.h"\
    "$(ILUSRC)\runtime\kernel\ilutypes.h"\
    "$(ILUSRC)\runtime\kernel\iluwin.h"\
    "$(ILUSRC)\runtime\kernel\iluxport.h"\
    "$(JDK_OINCL)\bool.h"\
    "$(JDK_OINCL)\debug.h"\
    "$(JDK_OINCL)\interpreter.h"\
    "$(JDK_OINCL)\java_lang_string.h"\
    "$(JDK_OINCL)\javastring.h"\
    "$(JDK_OINCL)\jcov.h"\
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_OINCL)\native.h"\
    "$(JDK_OINCL)\oobj.h"\
    "$(JDK_OINCL)\signature.h"\
    "$(JDK_OINCL)\stubpreamble.h"\
    "$(JDK_OINCL)\sys_api.h"\
    "$(JDK_OINCL)\tree.h"\
    "$(JDK_OINCL)\typecodes.h"\
    "$(JDK_OINCL)\typedefs.h"\
    "$(JDK_MDOINCL)\io_md.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
    "$(JDK_MDOINCL)\oobj_md.h"\
    "$(JDK_MDOINCL)\sysmacros_md.h"\
    "$(JDK_MDOINCL)\timeval_md.h"\
    "$(JDK_MDOINCL)\typedefs_md.h"\
    {$(INCLUDE)}"sys\types.h"\
    

"$(INTDIR)\IluJava_IluPickle.obj" : $(SOURCE) $(DEP_CPP_ILUJAVA_P) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_ILUJAVA_P=\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilucall.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluclassrep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilumethodrep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluobject.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluobjecttable.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluoint.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilupassport.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluidentity.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluidentitytype.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluservicethread.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluport.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilubatcher.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilurt0.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluserver.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluserverrelocationinfo.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluserializationcontext.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilusurrogateobject.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilutransportinfo.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilutyperep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluwpbase.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilupickle.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilutypecode.h"\
    "$(ILUSRC)\runtime\java\ilujava_callmacros.h"\
    "$(ILUSRC)\runtime\java\ilujava_common.h"\
    "$(ILUSRC)\runtime\java\ilujava_includes.h"\
    "$(ILUSRC)\runtime\java\ilujava_jarrays.h"\
    "$(ILUSRC)\runtime\java\ilujava_jmem.h"\
    "$(ILUSRC)\runtime\java\ilujava_jops.h"\
    "$(ILUSRC)\runtime\java\ilujava_jstubs.h"\
    "$(ILUSRC)\runtime\java\ilujava_jtypes.h"\
    "$(ILUSRC)\runtime\java\ilujava_ops.h"\
    "$(ILUSRC)\runtime\java\ilunative.h"\
    "$(ILUSRC)\runtime\kernel\ilubasic.h"\
    "$(ILUSRC)\runtime\kernel\iluerror.h"\
    "$(ILUSRC)\runtime\kernel\iluerrs.h"\
    "$(ILUSRC)\runtime\kernel\iluhash.h"\
    "$(ILUSRC)\runtime\kernel\ilutpcod.h"\
    "$(ILUSRC)\runtime\kernel\ilutypes.h"\
    "$(ILUSRC)\runtime\kernel\iluwin.h"\
    "$(ILUSRC)\runtime\kernel\iluxport.h"\
    "$(JDK_OINCL)\bool.h"\
    "$(JDK_OINCL)\debug.h"\
    "$(JDK_OINCL)\interpreter.h"\
    "$(JDK_OINCL)\java_lang_string.h"\
    "$(JDK_OINCL)\javastring.h"\
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_OINCL)\native.h"\
    "$(JDK_OINCL)\oobj.h"\
    "$(JDK_OINCL)\signature.h"\
    "$(JDK_OINCL)\stubpreamble.h"\
    "$(JDK_OINCL)\sys_api.h"\
    "$(JDK_OINCL)\tree.h"\
    "$(JDK_OINCL)\typecodes.h"\
    "$(JDK_OINCL)\typedefs.h"\
    "$(JDK_MDOINCL)\io_md.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
    "$(JDK_MDOINCL)\oobj_md.h"\
    "$(JDK_MDOINCL)\sysmacros_md.h"\
    "$(JDK_MDOINCL)\timeval_md.h"\
    "$(JDK_MDOINCL)\typedefs_md.h"\
    

"$(INTDIR)\IluJava_IluPickle.obj"   "$(INTDIR)\IluJava_IluPickle.sbr" : $(SOURCE)\
 $(DEP_CPP_ILUJAVA_P) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=$(ILUSRC)\runtime\java\IluJava_selectwt.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_ILUJAVA_S=\
    "$(ILUSRC)\runtime\java\ilujava_common.h"\
    "$(ILUSRC)\runtime\java\ilujava_includes.h"\
    "$(ILUSRC)\runtime\java\ilujava_jmem.h"\
    "$(ILUSRC)\runtime\java\ilujava_jtypes.h"\
    "$(ILUSRC)\runtime\java\ilunative.h"\
    "$(ILUSRC)\runtime\kernel\ilubasic.h"\
    "$(ILUSRC)\runtime\kernel\iluerror.h"\
    "$(ILUSRC)\runtime\kernel\iluerrs.h"\
    "$(ILUSRC)\runtime\kernel\iluhash.h"\
    "$(ILUSRC)\runtime\kernel\ilumac.h"\
    "$(ILUSRC)\runtime\kernel\ilutpcod.h"\
    "$(ILUSRC)\runtime\kernel\ilutypes.h"\
    "$(ILUSRC)\runtime\kernel\iluwin.h"\
    "$(ILUSRC)\runtime\kernel\iluxport.h"\
    "$(JDK_OINCL)\bool.h"\
    "$(JDK_OINCL)\debug.h"\
    "$(JDK_OINCL)\interpreter.h"\
    "$(JDK_OINCL)\java_lang_string.h"\
    "$(JDK_OINCL)\javastring.h"\
    "$(JDK_OINCL)\jcov.h"\
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_OINCL)\native.h"\
    "$(JDK_OINCL)\oobj.h"\
    "$(JDK_OINCL)\signature.h"\
    "$(JDK_OINCL)\stubpreamble.h"\
    "$(JDK_OINCL)\sys_api.h"\
    "$(JDK_OINCL)\tree.h"\
    "$(JDK_OINCL)\typecodes.h"\
    "$(JDK_OINCL)\typedefs.h"\
    "$(JDK_MDOINCL)\io_md.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
    "$(JDK_MDOINCL)\oobj_md.h"\
    "$(JDK_MDOINCL)\sysmacros_md.h"\
    "$(JDK_MDOINCL)\timeval_md.h"\
    "$(JDK_MDOINCL)\typedefs_md.h"\
    {$(INCLUDE)}"sys\types.h"\
    

"$(INTDIR)\IluJava_selectwt.obj" : $(SOURCE) $(DEP_CPP_ILUJAVA_S) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_ILUJAVA_S=\
    "$(ILUSRC)\runtime\java\ilujava_common.h"\
    "$(ILUSRC)\runtime\java\ilujava_includes.h"\
    "$(ILUSRC)\runtime\java\ilujava_jmem.h"\
    "$(ILUSRC)\runtime\java\ilujava_jtypes.h"\
    "$(ILUSRC)\runtime\java\ilunative.h"\
    "$(ILUSRC)\runtime\kernel\ilubasic.h"\
    "$(ILUSRC)\runtime\kernel\iluerror.h"\
    "$(ILUSRC)\runtime\kernel\iluerrs.h"\
    "$(ILUSRC)\runtime\kernel\iluhash.h"\
    "$(ILUSRC)\runtime\kernel\ilutpcod.h"\
    "$(ILUSRC)\runtime\kernel\ilutypes.h"\
    "$(ILUSRC)\runtime\kernel\iluwin.h"\
    "$(ILUSRC)\runtime\kernel\iluxport.h"\
    "$(JDK_OINCL)\bool.h"\
    "$(JDK_OINCL)\debug.h"\
    "$(JDK_OINCL)\interpreter.h"\
    "$(JDK_OINCL)\java_lang_string.h"\
    "$(JDK_OINCL)\javastring.h"\
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_OINCL)\native.h"\
    "$(JDK_OINCL)\oobj.h"\
    "$(JDK_OINCL)\signature.h"\
    "$(JDK_OINCL)\stubpreamble.h"\
    "$(JDK_OINCL)\sys_api.h"\
    "$(JDK_OINCL)\tree.h"\
    "$(JDK_OINCL)\typecodes.h"\
    "$(JDK_OINCL)\typedefs.h"\
    "$(JDK_MDOINCL)\io_md.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
    "$(JDK_MDOINCL)\oobj_md.h"\
    "$(JDK_MDOINCL)\sysmacros_md.h"\
    "$(JDK_MDOINCL)\timeval_md.h"\
    "$(JDK_MDOINCL)\typedefs_md.h"\
    

"$(INTDIR)\IluJava_selectwt.obj"    "$(INTDIR)\IluJava_selectwt.sbr" : $(SOURCE)\
 $(DEP_CPP_ILUJAVA_S) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=$(ILUSRC)\runtime\java\ilunative.c
DEP_CPP_ILUNA=\
    "$(ILUSRC)\runtime\java\ilunative.h"\
    

!IF  "$(CFG)" == "Win32 Release"


"$(INTDIR)\ilunative.obj" : $(SOURCE) $(DEP_CPP_ILUNA) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"


"$(INTDIR)\ilunative.obj"   "$(INTDIR)\ilunative.sbr" : $(SOURCE)\
 $(DEP_CPP_ILUNA) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=$(ILUSRC)\runtime\java\build\xerox_ilu_IluAlarmTech.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_XEROX=\
    "$(JDK_OINCL)\bool.h"\
    "$(JDK_OINCL)\debug.h"\
    "$(JDK_OINCL)\interpreter.h"\
    "$(JDK_OINCL)\jcov.h"\
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_OINCL)\oobj.h"\
    "$(JDK_OINCL)\signature.h"\
    "$(JDK_OINCL)\stubpreamble.h"\
    "$(JDK_OINCL)\typedefs.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
    "$(JDK_MDOINCL)\oobj_md.h"\
    "$(JDK_MDOINCL)\typedefs_md.h"\
    

"$(INTDIR)\xerox_ilu_IluAlarmTech.obj" : $(SOURCE) $(DEP_CPP_XEROX) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_XEROX=\
    "$(JDK_OINCL)\bool.h"\
    "$(JDK_OINCL)\debug.h"\
    "$(JDK_OINCL)\interpreter.h"\
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_OINCL)\oobj.h"\
    "$(JDK_OINCL)\signature.h"\
    "$(JDK_OINCL)\stubpreamble.h"\
    "$(JDK_OINCL)\typedefs.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
    "$(JDK_MDOINCL)\oobj_md.h"\
    "$(JDK_MDOINCL)\typedefs_md.h"\
    

"$(INTDIR)\xerox_ilu_IluAlarmTech.obj"  "$(INTDIR)\xerox_ilu_IluAlarmTech.sbr" :\
 $(SOURCE) $(DEP_CPP_XEROX) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=$(ILUSRC)\runtime\java\build\xerox_ilu_IluForkTech.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_XEROX=\
    "$(JDK_OINCL)\bool.h"\
    "$(JDK_OINCL)\debug.h"\
    "$(JDK_OINCL)\interpreter.h"\
    "$(JDK_OINCL)\jcov.h"\
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_OINCL)\oobj.h"\
    "$(JDK_OINCL)\signature.h"\
    "$(JDK_OINCL)\stubpreamble.h"\
    "$(JDK_OINCL)\typedefs.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
    "$(JDK_MDOINCL)\oobj_md.h"\
    "$(JDK_MDOINCL)\typedefs_md.h"\
    

"$(INTDIR)\xerox_ilu_IluForkTech.obj" : $(SOURCE) $(DEP_CPP_XEROX) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_XEROX=\
    "$(JDK_OINCL)\bool.h"\
    "$(JDK_OINCL)\debug.h"\
    "$(JDK_OINCL)\interpreter.h"\
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_OINCL)\oobj.h"\
    "$(JDK_OINCL)\signature.h"\
    "$(JDK_OINCL)\stubpreamble.h"\
    "$(JDK_OINCL)\typedefs.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
    "$(JDK_MDOINCL)\oobj_md.h"\
    "$(JDK_MDOINCL)\typedefs_md.h"\
    

"$(INTDIR)\xerox_ilu_IluForkTech.obj"   "$(INTDIR)\xerox_ilu_IluForkTech.sbr" :\
 $(SOURCE) $(DEP_CPP_XEROX) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=$(ILUSRC)\runtime\java\build\xerox_ilu_IluCall.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_XEROX_=\
    "$(JDK_OINCL)\bool.h"\
    "$(JDK_OINCL)\debug.h"\
    "$(JDK_OINCL)\interpreter.h"\
    "$(JDK_OINCL)\jcov.h"\
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_OINCL)\oobj.h"\
    "$(JDK_OINCL)\signature.h"\
    "$(JDK_OINCL)\stubpreamble.h"\
    "$(JDK_OINCL)\typedefs.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
    "$(JDK_MDOINCL)\oobj_md.h"\
    "$(JDK_MDOINCL)\typedefs_md.h"\
    

"$(INTDIR)\xerox_ilu_IluCall.obj" : $(SOURCE) $(DEP_CPP_XEROX_) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_XEROX_=\
    "$(JDK_OINCL)\bool.h"\
    "$(JDK_OINCL)\debug.h"\
    "$(JDK_OINCL)\interpreter.h"\
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_OINCL)\oobj.h"\
    "$(JDK_OINCL)\signature.h"\
    "$(JDK_OINCL)\stubpreamble.h"\
    "$(JDK_OINCL)\typedefs.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
    "$(JDK_MDOINCL)\oobj_md.h"\
    "$(JDK_MDOINCL)\typedefs_md.h"\
    

"$(INTDIR)\xerox_ilu_IluCall.obj"   "$(INTDIR)\xerox_ilu_IluCall.sbr" : $(SOURCE)\
 $(DEP_CPP_XEROX_) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=$(ILUSRC)\runtime\java\build\xerox_ilu_IluClassRep.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_XEROX_I=\
    "$(JDK_OINCL)\bool.h"\
    "$(JDK_OINCL)\debug.h"\
    "$(JDK_OINCL)\interpreter.h"\
    "$(JDK_OINCL)\jcov.h"\
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_OINCL)\oobj.h"\
    "$(JDK_OINCL)\signature.h"\
    "$(JDK_OINCL)\stubpreamble.h"\
    "$(JDK_OINCL)\typedefs.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
    "$(JDK_MDOINCL)\oobj_md.h"\
    "$(JDK_MDOINCL)\typedefs_md.h"\
    

"$(INTDIR)\xerox_ilu_IluClassRep.obj" : $(SOURCE) $(DEP_CPP_XEROX_I)\
 "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_XEROX_I=\
    "$(JDK_OINCL)\bool.h"\
    "$(JDK_OINCL)\debug.h"\
    "$(JDK_OINCL)\interpreter.h"\
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_OINCL)\oobj.h"\
    "$(JDK_OINCL)\signature.h"\
    "$(JDK_OINCL)\stubpreamble.h"\
    "$(JDK_OINCL)\typedefs.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
    "$(JDK_MDOINCL)\oobj_md.h"\
    "$(JDK_MDOINCL)\typedefs_md.h"\
    

"$(INTDIR)\xerox_ilu_IluClassRep.obj"   "$(INTDIR)\xerox_ilu_IluClassRep.sbr" : \
$(SOURCE) $(DEP_CPP_XEROX_I) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=$(ILUSRC)\runtime\java\build\xerox_ilu_IluDebug.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_XEROX_IL=\
    "$(JDK_OINCL)\bool.h"\
    "$(JDK_OINCL)\debug.h"\
    "$(JDK_OINCL)\interpreter.h"\
    "$(JDK_OINCL)\jcov.h"\
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_OINCL)\oobj.h"\
    "$(JDK_OINCL)\signature.h"\
    "$(JDK_OINCL)\stubpreamble.h"\
    "$(JDK_OINCL)\typedefs.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
    "$(JDK_MDOINCL)\oobj_md.h"\
    "$(JDK_MDOINCL)\typedefs_md.h"\
    

"$(INTDIR)\xerox_ilu_IluDebug.obj" : $(SOURCE) $(DEP_CPP_XEROX_IL) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_XEROX_IL=\
    "$(JDK_OINCL)\bool.h"\
    "$(JDK_OINCL)\debug.h"\
    "$(JDK_OINCL)\interpreter.h"\
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_OINCL)\oobj.h"\
    "$(JDK_OINCL)\signature.h"\
    "$(JDK_OINCL)\stubpreamble.h"\
    "$(JDK_OINCL)\typedefs.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
    "$(JDK_MDOINCL)\oobj_md.h"\
    "$(JDK_MDOINCL)\typedefs_md.h"\
    

"$(INTDIR)\xerox_ilu_IluDebug.obj"  "$(INTDIR)\xerox_ilu_IluDebug.sbr" : \
$(SOURCE) $(DEP_CPP_XEROX_IL) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=$(ILUSRC)\runtime\java\build\xerox_ilu_IluDebugHooks.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_XEROX_ILU=\
    "$(JDK_OINCL)\bool.h"\
    "$(JDK_OINCL)\debug.h"\
    "$(JDK_OINCL)\interpreter.h"\
    "$(JDK_OINCL)\jcov.h"\
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_OINCL)\oobj.h"\
    "$(JDK_OINCL)\signature.h"\
    "$(JDK_OINCL)\stubpreamble.h"\
    "$(JDK_OINCL)\typedefs.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
    "$(JDK_MDOINCL)\oobj_md.h"\
    "$(JDK_MDOINCL)\typedefs_md.h"\
    

"$(INTDIR)\xerox_ilu_IluDebugHooks.obj" : $(SOURCE) $(DEP_CPP_XEROX_ILU)\
 "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_XEROX_ILU=\
    "$(JDK_OINCL)\bool.h"\
    "$(JDK_OINCL)\debug.h"\
    "$(JDK_OINCL)\interpreter.h"\
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_OINCL)\oobj.h"\
    "$(JDK_OINCL)\signature.h"\
    "$(JDK_OINCL)\stubpreamble.h"\
    "$(JDK_OINCL)\typedefs.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
    "$(JDK_MDOINCL)\oobj_md.h"\
    "$(JDK_MDOINCL)\typedefs_md.h"\
    

"$(INTDIR)\xerox_ilu_IluDebugHooks.obj" "$(INTDIR)\xerox_ilu_IluDebugHooks.sbr"\
 : $(SOURCE) $(DEP_CPP_XEROX_ILU) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=$(ILUSRC)\runtime\java\build\xerox_ilu_IluDebugWriter.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_XEROX_ILU_=\
    "$(JDK_OINCL)\bool.h"\
    "$(JDK_OINCL)\debug.h"\
    "$(JDK_OINCL)\interpreter.h"\
    "$(JDK_OINCL)\jcov.h"\
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_OINCL)\oobj.h"\
    "$(JDK_OINCL)\signature.h"\
    "$(JDK_OINCL)\stubpreamble.h"\
    "$(JDK_OINCL)\typedefs.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
    "$(JDK_MDOINCL)\oobj_md.h"\
    "$(JDK_MDOINCL)\typedefs_md.h"\
    

"$(INTDIR)\xerox_ilu_IluDebugWriter.obj" : $(SOURCE) $(DEP_CPP_XEROX_ILU_)\
 "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_XEROX_ILU_=\
    "$(JDK_OINCL)\bool.h"\
    "$(JDK_OINCL)\debug.h"\
    "$(JDK_OINCL)\interpreter.h"\
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_OINCL)\oobj.h"\
    "$(JDK_OINCL)\signature.h"\
    "$(JDK_OINCL)\stubpreamble.h"\
    "$(JDK_OINCL)\typedefs.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
    "$(JDK_MDOINCL)\oobj_md.h"\
    "$(JDK_MDOINCL)\typedefs_md.h"\
    

"$(INTDIR)\xerox_ilu_IluDebugWriter.obj"\
    "$(INTDIR)\xerox_ilu_IluDebugWriter.sbr" : $(SOURCE) $(DEP_CPP_XEROX_ILU_)\
 "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=$(ILUSRC)\runtime\java\build\xerox_ilu_IluExceptionRep.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_XEROX_ILU_I=\
    "$(JDK_OINCL)\bool.h"\
    "$(JDK_OINCL)\debug.h"\
    "$(JDK_OINCL)\interpreter.h"\
    "$(JDK_OINCL)\jcov.h"\
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_OINCL)\oobj.h"\
    "$(JDK_OINCL)\signature.h"\
    "$(JDK_OINCL)\stubpreamble.h"\
    "$(JDK_OINCL)\typedefs.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
    "$(JDK_MDOINCL)\oobj_md.h"\
    "$(JDK_MDOINCL)\typedefs_md.h"\
    

"$(INTDIR)\xerox_ilu_IluExceptionRep.obj" : $(SOURCE) $(DEP_CPP_XEROX_ILU_I)\
 "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_XEROX_ILU_I=\
    "$(JDK_OINCL)\bool.h"\
    "$(JDK_OINCL)\debug.h"\
    "$(JDK_OINCL)\interpreter.h"\
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_OINCL)\oobj.h"\
    "$(JDK_OINCL)\signature.h"\
    "$(JDK_OINCL)\stubpreamble.h"\
    "$(JDK_OINCL)\typedefs.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
    "$(JDK_MDOINCL)\oobj_md.h"\
    "$(JDK_MDOINCL)\typedefs_md.h"\
    

"$(INTDIR)\xerox_ilu_IluExceptionRep.obj"\
    "$(INTDIR)\xerox_ilu_IluExceptionRep.sbr" : $(SOURCE) $(DEP_CPP_XEROX_ILU_I)\
 "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=$(ILUSRC)\runtime\java\build\xerox_ilu_IluGCClient.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_XEROX_ILU_IL=\
    "$(JDK_OINCL)\bool.h"\
    "$(JDK_OINCL)\debug.h"\
    "$(JDK_OINCL)\interpreter.h"\
    "$(JDK_OINCL)\jcov.h"\
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_OINCL)\oobj.h"\
    "$(JDK_OINCL)\signature.h"\
    "$(JDK_OINCL)\stubpreamble.h"\
    "$(JDK_OINCL)\typedefs.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
    "$(JDK_MDOINCL)\oobj_md.h"\
    "$(JDK_MDOINCL)\typedefs_md.h"\
    

"$(INTDIR)\xerox_ilu_IluGCClient.obj" : $(SOURCE) $(DEP_CPP_XEROX_ILU_IL)\
 "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_XEROX_ILU_IL=\
    "$(JDK_OINCL)\bool.h"\
    "$(JDK_OINCL)\debug.h"\
    "$(JDK_OINCL)\interpreter.h"\
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_OINCL)\oobj.h"\
    "$(JDK_OINCL)\signature.h"\
    "$(JDK_OINCL)\stubpreamble.h"\
    "$(JDK_OINCL)\typedefs.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
    "$(JDK_MDOINCL)\oobj_md.h"\
    "$(JDK_MDOINCL)\typedefs_md.h"\
    

"$(INTDIR)\xerox_ilu_IluGCClient.obj"   "$(INTDIR)\xerox_ilu_IluGCClient.sbr" : \
$(SOURCE) $(DEP_CPP_XEROX_ILU_IL) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=$(ILUSRC)\runtime\java\build\xerox_ilu_IluInit2.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_XEROX_ILU_ILU=\
    "$(JDK_OINCL)\bool.h"\
    "$(JDK_OINCL)\debug.h"\
    "$(JDK_OINCL)\interpreter.h"\
    "$(JDK_OINCL)\jcov.h"\
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_OINCL)\oobj.h"\
    "$(JDK_OINCL)\signature.h"\
    "$(JDK_OINCL)\stubpreamble.h"\
    "$(JDK_OINCL)\typedefs.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
    "$(JDK_MDOINCL)\oobj_md.h"\
    "$(JDK_MDOINCL)\typedefs_md.h"\
    

"$(INTDIR)\xerox_ilu_IluInit2.obj" : $(SOURCE) $(DEP_CPP_XEROX_ILU_ILU)\
 "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_XEROX_ILU_ILU=\
    "$(JDK_OINCL)\bool.h"\
    "$(JDK_OINCL)\debug.h"\
    "$(JDK_OINCL)\interpreter.h"\
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_OINCL)\oobj.h"\
    "$(JDK_OINCL)\signature.h"\
    "$(JDK_OINCL)\stubpreamble.h"\
    "$(JDK_OINCL)\typedefs.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
    "$(JDK_MDOINCL)\oobj_md.h"\
    "$(JDK_MDOINCL)\typedefs_md.h"\
    

"$(INTDIR)\xerox_ilu_IluInit2.obj"  "$(INTDIR)\xerox_ilu_IluInit2.sbr" : \
$(SOURCE) $(DEP_CPP_XEROX_ILU_ILU) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=$(ILUSRC)\runtime\java\build\xerox_ilu_IluOInt.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_XEROX_ILU_ILUO=\
    "$(JDK_OINCL)\bool.h"\
    "$(JDK_OINCL)\debug.h"\
    "$(JDK_OINCL)\interpreter.h"\
    "$(JDK_OINCL)\jcov.h"\
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_OINCL)\oobj.h"\
    "$(JDK_OINCL)\signature.h"\
    "$(JDK_OINCL)\stubpreamble.h"\
    "$(JDK_OINCL)\typedefs.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
    "$(JDK_MDOINCL)\oobj_md.h"\
    "$(JDK_MDOINCL)\typedefs_md.h"\
    

"$(INTDIR)\xerox_ilu_IluOInt.obj" : $(SOURCE) $(DEP_CPP_XEROX_ILU_ILUO)\
 "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_XEROX_ILU_ILUO=\
    "$(JDK_OINCL)\bool.h"\
    "$(JDK_OINCL)\debug.h"\
    "$(JDK_OINCL)\interpreter.h"\
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_OINCL)\oobj.h"\
    "$(JDK_OINCL)\signature.h"\
    "$(JDK_OINCL)\stubpreamble.h"\
    "$(JDK_OINCL)\typedefs.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
    "$(JDK_MDOINCL)\oobj_md.h"\
    "$(JDK_MDOINCL)\typedefs_md.h"\
    

"$(INTDIR)\xerox_ilu_IluOInt.obj"   "$(INTDIR)\xerox_ilu_IluOInt.sbr" : $(SOURCE)\
 $(DEP_CPP_XEROX_ILU_ILUO) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=$(ILUSRC)\runtime\java\build\xerox_ilu_IluPassport.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_XEROX_ILU_ILUP=\
    "$(JDK_OINCL)\bool.h"\
    "$(JDK_OINCL)\debug.h"\
    "$(JDK_OINCL)\interpreter.h"\
    "$(JDK_OINCL)\jcov.h"\
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_OINCL)\oobj.h"\
    "$(JDK_OINCL)\signature.h"\
    "$(JDK_OINCL)\stubpreamble.h"\
    "$(JDK_OINCL)\typedefs.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
    "$(JDK_MDOINCL)\oobj_md.h"\
    "$(JDK_MDOINCL)\typedefs_md.h"\
    

"$(INTDIR)\xerox_ilu_IluPassport.obj" : $(SOURCE) $(DEP_CPP_XEROX_ILU_ILUP)\
 "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_XEROX_ILU_ILUP=\
    "$(JDK_OINCL)\bool.h"\
    "$(JDK_OINCL)\debug.h"\
    "$(JDK_OINCL)\interpreter.h"\
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_OINCL)\oobj.h"\
    "$(JDK_OINCL)\signature.h"\
    "$(JDK_OINCL)\stubpreamble.h"\
    "$(JDK_OINCL)\typedefs.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
    "$(JDK_MDOINCL)\oobj_md.h"\
    "$(JDK_MDOINCL)\typedefs_md.h"\
    

"$(INTDIR)\xerox_ilu_IluPassport.obj"   "$(INTDIR)\xerox_ilu_IluPassport.sbr" : \
$(SOURCE) $(DEP_CPP_XEROX_ILU_ILUP) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=$(ILUSRC)\runtime\java\build\xerox_ilu_IluPort.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_XEROX_ILU_ILUPO=\
    "$(JDK_OINCL)\bool.h"\
    "$(JDK_OINCL)\debug.h"\
    "$(JDK_OINCL)\interpreter.h"\
    "$(JDK_OINCL)\jcov.h"\
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_OINCL)\oobj.h"\
    "$(JDK_OINCL)\signature.h"\
    "$(JDK_OINCL)\stubpreamble.h"\
    "$(JDK_OINCL)\typedefs.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
    "$(JDK_MDOINCL)\oobj_md.h"\
    "$(JDK_MDOINCL)\typedefs_md.h"\
    

"$(INTDIR)\xerox_ilu_IluPort.obj" : $(SOURCE) $(DEP_CPP_XEROX_ILU_ILUPO)\
 "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_XEROX_ILU_ILUPO=\
    "$(JDK_OINCL)\bool.h"\
    "$(JDK_OINCL)\debug.h"\
    "$(JDK_OINCL)\interpreter.h"\
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_OINCL)\oobj.h"\
    "$(JDK_OINCL)\signature.h"\
    "$(JDK_OINCL)\stubpreamble.h"\
    "$(JDK_OINCL)\typedefs.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
    "$(JDK_MDOINCL)\oobj_md.h"\
    "$(JDK_MDOINCL)\typedefs_md.h"\
    

"$(INTDIR)\xerox_ilu_IluPort.obj"   "$(INTDIR)\xerox_ilu_IluPort.sbr" : $(SOURCE)\
 $(DEP_CPP_XEROX_ILU_ILUPO) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=$(ILUSRC)\runtime\java\build\xerox_ilu_IluGssOid.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_XEROX_ILU_ILUGS=\
    "$(JDK_OINCL)\bool.h"\
    "$(JDK_OINCL)\debug.h"\
    "$(JDK_OINCL)\interpreter.h"\
    "$(JDK_OINCL)\jcov.h"\
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_OINCL)\oobj.h"\
    "$(JDK_OINCL)\signature.h"\
    "$(JDK_OINCL)\stubpreamble.h"\
    "$(JDK_OINCL)\typedefs.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
    "$(JDK_MDOINCL)\oobj_md.h"\
    "$(JDK_MDOINCL)\typedefs_md.h"\
    

"$(INTDIR)\xerox_ilu_IluGssOid.obj" : $(SOURCE) $(DEP_CPP_XEROX_ILU_ILUGS)\
 "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_XEROX_ILU_ILUGS=\
    "$(JDK_OINCL)\bool.h"\
    "$(JDK_OINCL)\debug.h"\
    "$(JDK_OINCL)\interpreter.h"\
    "$(JDK_OINCL)\jcov.h"\
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_OINCL)\oobj.h"\
    "$(JDK_OINCL)\signature.h"\
    "$(JDK_OINCL)\stubpreamble.h"\
    "$(JDK_OINCL)\typedefs.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
    "$(JDK_MDOINCL)\oobj_md.h"\
    "$(JDK_MDOINCL)\typedefs_md.h"\
    

"$(INTDIR)\xerox_ilu_IluGssOid.obj"   "$(INTDIR)\xerox_ilu_IluGssOid.sbr" : $(SOURCE)\
 $(DEP_CPP_XEROX_ILU_ILUGS) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=$(ILUSRC)\runtime\java\build\xerox_ilu_IluGssCred.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_XEROX_ILU_ILUGS=\
    "$(JDK_OINCL)\bool.h"\
    "$(JDK_OINCL)\debug.h"\
    "$(JDK_OINCL)\interpreter.h"\
    "$(JDK_OINCL)\jcov.h"\
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_OINCL)\oobj.h"\
    "$(JDK_OINCL)\signature.h"\
    "$(JDK_OINCL)\stubpreamble.h"\
    "$(JDK_OINCL)\typedefs.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
    "$(JDK_MDOINCL)\oobj_md.h"\
    "$(JDK_MDOINCL)\typedefs_md.h"\
    

"$(INTDIR)\xerox_ilu_IluGssCred.obj" : $(SOURCE) $(DEP_CPP_XEROX_ILU_ILUGS)\
 "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_XEROX_ILU_ILUGS=\
    "$(JDK_OINCL)\bool.h"\
    "$(JDK_OINCL)\debug.h"\
    "$(JDK_OINCL)\interpreter.h"\
    "$(JDK_OINCL)\jcov.h"\
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_OINCL)\oobj.h"\
    "$(JDK_OINCL)\signature.h"\
    "$(JDK_OINCL)\stubpreamble.h"\
    "$(JDK_OINCL)\typedefs.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
    "$(JDK_MDOINCL)\oobj_md.h"\
    "$(JDK_MDOINCL)\typedefs_md.h"\
    

"$(INTDIR)\xerox_ilu_IluGssCred.obj"   "$(INTDIR)\xerox_ilu_IluGssCred.sbr" : $(SOURCE)\
 $(DEP_CPP_XEROX_ILU_ILUGS) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 


SOURCE=$(ILUSRC)\runtime\java\build\xerox_ilu_IluBatcher.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_XEROX_ILU_ILUBA=\
    "$(JDK_OINCL)\bool.h"\
    "$(JDK_OINCL)\debug.h"\
    "$(JDK_OINCL)\interpreter.h"\
    "$(JDK_OINCL)\jcov.h"\
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_OINCL)\oobj.h"\
    "$(JDK_OINCL)\signature.h"\
    "$(JDK_OINCL)\stubpreamble.h"\
    "$(JDK_OINCL)\typedefs.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
    "$(JDK_MDOINCL)\oobj_md.h"\
    "$(JDK_MDOINCL)\typedefs_md.h"\
    

"$(INTDIR)\xerox_ilu_IluBatcher.obj" : $(SOURCE) $(DEP_CPP_XEROX_ILU_ILUBA)\
 "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_XEROX_ILU_ILUBA=\
    "$(JDK_OINCL)\bool.h"\
    "$(JDK_OINCL)\debug.h"\
    "$(JDK_OINCL)\interpreter.h"\
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_OINCL)\oobj.h"\
    "$(JDK_OINCL)\signature.h"\
    "$(JDK_OINCL)\stubpreamble.h"\
    "$(JDK_OINCL)\typedefs.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
    "$(JDK_MDOINCL)\oobj_md.h"\
    "$(JDK_MDOINCL)\typedefs_md.h"\
    

"$(INTDIR)\xerox_ilu_IluBatcher.obj"   "$(INTDIR)\xerox_ilu_IluBatcher.sbr" : $(SOURCE)\
 $(DEP_CPP_XEROX_ILU_ILUBA) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=$(ILUSRC)\runtime\java\build\xerox_ilu_IluRT0.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_XEROX_ILU_ILUR=\
    "$(JDK_OINCL)\bool.h"\
    "$(JDK_OINCL)\debug.h"\
    "$(JDK_OINCL)\interpreter.h"\
    "$(JDK_OINCL)\jcov.h"\
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_OINCL)\oobj.h"\
    "$(JDK_OINCL)\signature.h"\
    "$(JDK_OINCL)\stubpreamble.h"\
    "$(JDK_OINCL)\typedefs.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
    "$(JDK_MDOINCL)\oobj_md.h"\
    "$(JDK_MDOINCL)\typedefs_md.h"\
    

"$(INTDIR)\xerox_ilu_IluRT0.obj" : $(SOURCE) $(DEP_CPP_XEROX_ILU_ILUR)\
 "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_XEROX_ILU_ILUR=\
    "$(JDK_OINCL)\bool.h"\
    "$(JDK_OINCL)\debug.h"\
    "$(JDK_OINCL)\interpreter.h"\
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_OINCL)\oobj.h"\
    "$(JDK_OINCL)\signature.h"\
    "$(JDK_OINCL)\stubpreamble.h"\
    "$(JDK_OINCL)\typedefs.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
    "$(JDK_MDOINCL)\oobj_md.h"\
    "$(JDK_MDOINCL)\typedefs_md.h"\
    

"$(INTDIR)\xerox_ilu_IluRT0.obj"    "$(INTDIR)\xerox_ilu_IluRT0.sbr" : $(SOURCE)\
 $(DEP_CPP_XEROX_ILU_ILUR) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=$(ILUSRC)\runtime\java\build\xerox_ilu_IluServer.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_XEROX_ILU_ILUS=\
    "$(JDK_OINCL)\bool.h"\
    "$(JDK_OINCL)\debug.h"\
    "$(JDK_OINCL)\interpreter.h"\
    "$(JDK_OINCL)\jcov.h"\
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_OINCL)\oobj.h"\
    "$(JDK_OINCL)\signature.h"\
    "$(JDK_OINCL)\stubpreamble.h"\
    "$(JDK_OINCL)\typedefs.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
    "$(JDK_MDOINCL)\oobj_md.h"\
    "$(JDK_MDOINCL)\typedefs_md.h"\
    

"$(INTDIR)\xerox_ilu_IluServer.obj" : $(SOURCE) $(DEP_CPP_XEROX_ILU_ILUS)\
 "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_XEROX_ILU_ILUS=\
    "$(JDK_OINCL)\bool.h"\
    "$(JDK_OINCL)\debug.h"\
    "$(JDK_OINCL)\interpreter.h"\
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_OINCL)\oobj.h"\
    "$(JDK_OINCL)\signature.h"\
    "$(JDK_OINCL)\stubpreamble.h"\
    "$(JDK_OINCL)\typedefs.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
    "$(JDK_MDOINCL)\oobj_md.h"\
    "$(JDK_MDOINCL)\typedefs_md.h"\
    

"$(INTDIR)\xerox_ilu_IluServer.obj" "$(INTDIR)\xerox_ilu_IluServer.sbr" : \
$(SOURCE) $(DEP_CPP_XEROX_ILU_ILUS) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=$(ILUSRC)\runtime\java\build\xerox_ilu_IluSBH.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_XEROX_ILU_ILUS=\
    "$(JDK_OINCL)\bool.h"\
    "$(JDK_OINCL)\debug.h"\
    "$(JDK_OINCL)\interpreter.h"\
    "$(JDK_OINCL)\jcov.h"\
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_OINCL)\oobj.h"\
    "$(JDK_OINCL)\signature.h"\
    "$(JDK_OINCL)\stubpreamble.h"\
    "$(JDK_OINCL)\typedefs.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
    "$(JDK_MDOINCL)\oobj_md.h"\
    "$(JDK_MDOINCL)\typedefs_md.h"\
    

"$(INTDIR)\xerox_ilu_IluSBH.obj" : $(SOURCE) $(DEP_CPP_XEROX_ILU_ILUS)\
 "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_XEROX_ILU_ILUS=\
    "$(JDK_OINCL)\bool.h"\
    "$(JDK_OINCL)\debug.h"\
    "$(JDK_OINCL)\interpreter.h"\
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_OINCL)\oobj.h"\
    "$(JDK_OINCL)\signature.h"\
    "$(JDK_OINCL)\stubpreamble.h"\
    "$(JDK_OINCL)\typedefs.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
    "$(JDK_MDOINCL)\oobj_md.h"\
    "$(JDK_MDOINCL)\typedefs_md.h"\
    

"$(INTDIR)\xerox_ilu_IluSBH.obj"    "$(INTDIR)\xerox_ilu_IluSBH.sbr" : \
$(SOURCE) $(DEP_CPP_XEROX_ILU_ILUS) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=$(ILUSRC)\runtime\java\build\xerox_ilu_IluSerializationContext.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_XEROX_ILU_ILUS=\
    "$(JDK_OINCL)\bool.h"\
    "$(JDK_OINCL)\debug.h"\
    "$(JDK_OINCL)\interpreter.h"\
    "$(JDK_OINCL)\jcov.h"\
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_OINCL)\oobj.h"\
    "$(JDK_OINCL)\signature.h"\
    "$(JDK_OINCL)\stubpreamble.h"\
    "$(JDK_OINCL)\typedefs.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
    "$(JDK_MDOINCL)\oobj_md.h"\
    "$(JDK_MDOINCL)\typedefs_md.h"\
    

"$(INTDIR)\xerox_ilu_IluSerializationContext.obj" : $(SOURCE) $(DEP_CPP_XEROX_ILU_ILUS)\
 "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_XEROX_ILU_ILUS=\
    "$(JDK_OINCL)\bool.h"\
    "$(JDK_OINCL)\debug.h"\
    "$(JDK_OINCL)\interpreter.h"\
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_OINCL)\oobj.h"\
    "$(JDK_OINCL)\signature.h"\
    "$(JDK_OINCL)\stubpreamble.h"\
    "$(JDK_OINCL)\typedefs.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
    "$(JDK_MDOINCL)\oobj_md.h"\
    "$(JDK_MDOINCL)\typedefs_md.h"\
    

"$(INTDIR)\xerox_ilu_IluSerializationContext.obj"   "$(INTDIR)\xerox_ilu_IluSerializationContext.sbr" : \
$(SOURCE) $(DEP_CPP_XEROX_ILU_ILUS) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 



SOURCE=$(ILUSRC)\runtime\java\build\xerox_ilu_IluPipeline.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_XEROX_ILU_ILUS=\
    "$(JDK_OINCL)\bool.h"\
    "$(JDK_OINCL)\debug.h"\
    "$(JDK_OINCL)\interpreter.h"\
    "$(JDK_OINCL)\jcov.h"\
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_OINCL)\oobj.h"\
    "$(JDK_OINCL)\signature.h"\
    "$(JDK_OINCL)\stubpreamble.h"\
    "$(JDK_OINCL)\typedefs.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
    "$(JDK_MDOINCL)\oobj_md.h"\
    "$(JDK_MDOINCL)\typedefs_md.h"\
    

"$(INTDIR)\xerox_ilu_IluPipeline.obj" : $(SOURCE) $(DEP_CPP_XEROX_ILU_ILUS)\
 "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_XEROX_ILU_ILUS=\
    "$(JDK_OINCL)\bool.h"\
    "$(JDK_OINCL)\debug.h"\
    "$(JDK_OINCL)\interpreter.h"\
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_OINCL)\oobj.h"\
    "$(JDK_OINCL)\signature.h"\
    "$(JDK_OINCL)\stubpreamble.h"\
    "$(JDK_OINCL)\typedefs.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
    "$(JDK_MDOINCL)\oobj_md.h"\
    "$(JDK_MDOINCL)\typedefs_md.h"\
    

"$(INTDIR)\xerox_ilu_IluPipeline.obj"   "$(INTDIR)\xerox_ilu_IluPipeline.sbr" : \
$(SOURCE) $(DEP_CPP_XEROX_ILU_ILUS) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 



SOURCE=$(ILUSRC)\runtime\java\build\xerox_ilu_IluIdentity.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_XEROX_ILU_ILUS=\
    "$(JDK_OINCL)\bool.h"\
    "$(JDK_OINCL)\debug.h"\
    "$(JDK_OINCL)\interpreter.h"\
    "$(JDK_OINCL)\jcov.h"\
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_OINCL)\oobj.h"\
    "$(JDK_OINCL)\signature.h"\
    "$(JDK_OINCL)\stubpreamble.h"\
    "$(JDK_OINCL)\typedefs.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
    "$(JDK_MDOINCL)\oobj_md.h"\
    "$(JDK_MDOINCL)\typedefs_md.h"\
    

"$(INTDIR)\xerox_ilu_IluIdentity.obj" : $(SOURCE) $(DEP_CPP_XEROX_ILU_ILUS)\
 "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_XEROX_ILU_ILUS=\
    "$(JDK_OINCL)\bool.h"\
    "$(JDK_OINCL)\debug.h"\
    "$(JDK_OINCL)\interpreter.h"\
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_OINCL)\oobj.h"\
    "$(JDK_OINCL)\signature.h"\
    "$(JDK_OINCL)\stubpreamble.h"\
    "$(JDK_OINCL)\typedefs.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
    "$(JDK_MDOINCL)\oobj_md.h"\
    "$(JDK_MDOINCL)\typedefs_md.h"\
    

"$(INTDIR)\xerox_ilu_IluIdentity.obj"   "$(INTDIR)\xerox_ilu_IluIdentity.sbr" : \
$(SOURCE) $(DEP_CPP_XEROX_ILU_ILUS) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 



SOURCE=$(ILUSRC)\runtime\java\build\xerox_ilu_IluIdentityType.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_XEROX_ILU_ILUS=\
    "$(JDK_OINCL)\bool.h"\
    "$(JDK_OINCL)\debug.h"\
    "$(JDK_OINCL)\interpreter.h"\
    "$(JDK_OINCL)\jcov.h"\
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_OINCL)\oobj.h"\
    "$(JDK_OINCL)\signature.h"\
    "$(JDK_OINCL)\stubpreamble.h"\
    "$(JDK_OINCL)\typedefs.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
    "$(JDK_MDOINCL)\oobj_md.h"\
    "$(JDK_MDOINCL)\typedefs_md.h"\
    

"$(INTDIR)\xerox_ilu_IluIdentityType.obj" : $(SOURCE) $(DEP_CPP_XEROX_ILU_ILUS)\
 "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_XEROX_ILU_ILUS=\
    "$(JDK_OINCL)\bool.h"\
    "$(JDK_OINCL)\debug.h"\
    "$(JDK_OINCL)\interpreter.h"\
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_OINCL)\oobj.h"\
    "$(JDK_OINCL)\signature.h"\
    "$(JDK_OINCL)\stubpreamble.h"\
    "$(JDK_OINCL)\typedefs.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
    "$(JDK_MDOINCL)\oobj_md.h"\
    "$(JDK_MDOINCL)\typedefs_md.h"\
    

"$(INTDIR)\xerox_ilu_IluIdentityType.obj"   "$(INTDIR)\xerox_ilu_IluIdentityType.sbr" : \
$(SOURCE) $(DEP_CPP_XEROX_ILU_ILUS) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=$(ILUSRC)\runtime\java\build\xerox_ilu_IluServiceThread.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_XEROX_ILU_ILUS=\
    "$(JDK_OINCL)\bool.h"\
    "$(JDK_OINCL)\debug.h"\
    "$(JDK_OINCL)\interpreter.h"\
    "$(JDK_OINCL)\jcov.h"\
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_OINCL)\oobj.h"\
    "$(JDK_OINCL)\signature.h"\
    "$(JDK_OINCL)\stubpreamble.h"\
    "$(JDK_OINCL)\typedefs.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
    "$(JDK_MDOINCL)\oobj_md.h"\
    "$(JDK_MDOINCL)\typedefs_md.h"\
    

"$(INTDIR)\xerox_ilu_IluServiceThread.obj" : $(SOURCE) $(DEP_CPP_XEROX_ILU_ILUS)\
 "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_XEROX_ILU_ILUS=\
    "$(JDK_OINCL)\bool.h"\
    "$(JDK_OINCL)\debug.h"\
    "$(JDK_OINCL)\interpreter.h"\
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_OINCL)\oobj.h"\
    "$(JDK_OINCL)\signature.h"\
    "$(JDK_OINCL)\stubpreamble.h"\
    "$(JDK_OINCL)\typedefs.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
    "$(JDK_MDOINCL)\oobj_md.h"\
    "$(JDK_MDOINCL)\typedefs_md.h"\
    

"$(INTDIR)\xerox_ilu_IluServiceThread.obj"  "$(INTDIR)\xerox_ilu_IluServiceThread.sbr" : \
$(SOURCE) $(DEP_CPP_XEROX_ILU_ILUS) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=$(ILUSRC)\runtime\java\build\xerox_ilu_IluServerConnection.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_XEROX_ILU_ILUSE=\
    "$(JDK_OINCL)\bool.h"\
    "$(JDK_OINCL)\debug.h"\
    "$(JDK_OINCL)\interpreter.h"\
    "$(JDK_OINCL)\jcov.h"\
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_OINCL)\oobj.h"\
    "$(JDK_OINCL)\signature.h"\
    "$(JDK_OINCL)\stubpreamble.h"\
    "$(JDK_OINCL)\typedefs.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
    "$(JDK_MDOINCL)\oobj_md.h"\
    "$(JDK_MDOINCL)\typedefs_md.h"\
    

"$(INTDIR)\xerox_ilu_IluServerConnection.obj" : $(SOURCE)\
 $(DEP_CPP_XEROX_ILU_ILUSE) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_XEROX_ILU_ILUSE=\
    "$(JDK_OINCL)\bool.h"\
    "$(JDK_OINCL)\debug.h"\
    "$(JDK_OINCL)\interpreter.h"\
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_OINCL)\oobj.h"\
    "$(JDK_OINCL)\signature.h"\
    "$(JDK_OINCL)\stubpreamble.h"\
    "$(JDK_OINCL)\typedefs.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
    "$(JDK_MDOINCL)\oobj_md.h"\
    "$(JDK_MDOINCL)\typedefs_md.h"\
    

"$(INTDIR)\xerox_ilu_IluServerConnection.obj"\
    "$(INTDIR)\xerox_ilu_IluServerConnection.sbr" : $(SOURCE)\
 $(DEP_CPP_XEROX_ILU_ILUSE) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=$(ILUSRC)\runtime\java\build\xerox_ilu_IluSurrogateConnection.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_XEROX_ILU_ILUSU=\
    "$(JDK_OINCL)\bool.h"\
    "$(JDK_OINCL)\debug.h"\
    "$(JDK_OINCL)\interpreter.h"\
    "$(JDK_OINCL)\jcov.h"\
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_OINCL)\oobj.h"\
    "$(JDK_OINCL)\signature.h"\
    "$(JDK_OINCL)\stubpreamble.h"\
    "$(JDK_OINCL)\typedefs.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
    "$(JDK_MDOINCL)\oobj_md.h"\
    "$(JDK_MDOINCL)\typedefs_md.h"\
    

"$(INTDIR)\xerox_ilu_IluSurrogateConnection.obj" : $(SOURCE)\
 $(DEP_CPP_XEROX_ILU_ILUSU) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_XEROX_ILU_ILUSU=\
    "$(JDK_OINCL)\bool.h"\
    "$(JDK_OINCL)\debug.h"\
    "$(JDK_OINCL)\interpreter.h"\
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_OINCL)\oobj.h"\
    "$(JDK_OINCL)\signature.h"\
    "$(JDK_OINCL)\stubpreamble.h"\
    "$(JDK_OINCL)\typedefs.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
    "$(JDK_MDOINCL)\oobj_md.h"\
    "$(JDK_MDOINCL)\typedefs_md.h"\
    

"$(INTDIR)\xerox_ilu_IluSurrogateConnection.obj"\
    "$(INTDIR)\xerox_ilu_IluSurrogateConnection.sbr" : $(SOURCE)\
 $(DEP_CPP_XEROX_ILU_ILUSU) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=$(ILUSRC)\runtime\java\build\xerox_ilu_IluTransportInfo.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_XEROX_ILU_ILUT=\
    "$(JDK_OINCL)\bool.h"\
    "$(JDK_OINCL)\debug.h"\
    "$(JDK_OINCL)\interpreter.h"\
    "$(JDK_OINCL)\jcov.h"\
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_OINCL)\oobj.h"\
    "$(JDK_OINCL)\signature.h"\
    "$(JDK_OINCL)\stubpreamble.h"\
    "$(JDK_OINCL)\typedefs.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
    "$(JDK_MDOINCL)\oobj_md.h"\
    "$(JDK_MDOINCL)\typedefs_md.h"\
    

"$(INTDIR)\xerox_ilu_IluTransportInfo.obj" : $(SOURCE)\
 $(DEP_CPP_XEROX_ILU_ILUT) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_XEROX_ILU_ILUT=\
    "$(JDK_OINCL)\bool.h"\
    "$(JDK_OINCL)\debug.h"\
    "$(JDK_OINCL)\interpreter.h"\
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_OINCL)\oobj.h"\
    "$(JDK_OINCL)\signature.h"\
    "$(JDK_OINCL)\stubpreamble.h"\
    "$(JDK_OINCL)\typedefs.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
    "$(JDK_MDOINCL)\oobj_md.h"\
    "$(JDK_MDOINCL)\typedefs_md.h"\
    

"$(INTDIR)\xerox_ilu_IluTransportInfo.obj"\
    "$(INTDIR)\xerox_ilu_IluTransportInfo.sbr" : $(SOURCE)\
 $(DEP_CPP_XEROX_ILU_ILUT) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=$(ILUSRC)\runtime\java\build\xerox_ilu_IluTypeRep.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_XEROX_ILU_ILUTY=\
    "$(JDK_OINCL)\bool.h"\
    "$(JDK_OINCL)\debug.h"\
    "$(JDK_OINCL)\interpreter.h"\
    "$(JDK_OINCL)\jcov.h"\
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_OINCL)\oobj.h"\
    "$(JDK_OINCL)\signature.h"\
    "$(JDK_OINCL)\stubpreamble.h"\
    "$(JDK_OINCL)\typedefs.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
    "$(JDK_MDOINCL)\oobj_md.h"\
    "$(JDK_MDOINCL)\typedefs_md.h"\
    

"$(INTDIR)\xerox_ilu_IluTypeRep.obj" : $(SOURCE) $(DEP_CPP_XEROX_ILU_ILUTY)\
 "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_XEROX_ILU_ILUTY=\
    "$(JDK_OINCL)\bool.h"\
    "$(JDK_OINCL)\debug.h"\
    "$(JDK_OINCL)\interpreter.h"\
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_OINCL)\oobj.h"\
    "$(JDK_OINCL)\signature.h"\
    "$(JDK_OINCL)\stubpreamble.h"\
    "$(JDK_OINCL)\typedefs.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
    "$(JDK_MDOINCL)\oobj_md.h"\
    "$(JDK_MDOINCL)\typedefs_md.h"\
    

"$(INTDIR)\xerox_ilu_IluTypeRep.obj"    "$(INTDIR)\xerox_ilu_IluTypeRep.sbr" : \
$(SOURCE) $(DEP_CPP_XEROX_ILU_ILUTY) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=$(ILUSRC)\runtime\java\build\xerox_ilu_IluPickle.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_XEROX_ILU_P=\
    "$(JDK_OINCL)\bool.h"\
    "$(JDK_OINCL)\debug.h"\
    "$(JDK_OINCL)\interpreter.h"\
    "$(JDK_OINCL)\jcov.h"\
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_OINCL)\oobj.h"\
    "$(JDK_OINCL)\signature.h"\
    "$(JDK_OINCL)\stubpreamble.h"\
    "$(JDK_OINCL)\typedefs.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
    "$(JDK_MDOINCL)\oobj_md.h"\
    "$(JDK_MDOINCL)\typedefs_md.h"\
    

"$(INTDIR)\xerox_ilu_IluPickle.obj" : $(SOURCE) $(DEP_CPP_XEROX_ILU_P) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_XEROX_ILU_P=\
    "$(JDK_OINCL)\bool.h"\
    "$(JDK_OINCL)\debug.h"\
    "$(JDK_OINCL)\interpreter.h"\
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_OINCL)\oobj.h"\
    "$(JDK_OINCL)\signature.h"\
    "$(JDK_OINCL)\stubpreamble.h"\
    "$(JDK_OINCL)\typedefs.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
    "$(JDK_MDOINCL)\oobj_md.h"\
    "$(JDK_MDOINCL)\typedefs_md.h"\
    

"$(INTDIR)\xerox_ilu_IluPickle.obj" "$(INTDIR)\xerox_ilu_IluPickle.sbr" : $(SOURCE)\
 $(DEP_CPP_XEROX_ILU_P) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=$(ILUSRC)\runtime\java\build\xerox_ilu_ilutypecode.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_XEROX_ILU_T=\
    "$(JDK_OINCL)\bool.h"\
    "$(JDK_OINCL)\debug.h"\
    "$(JDK_OINCL)\interpreter.h"\
    "$(JDK_OINCL)\jcov.h"\
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_OINCL)\oobj.h"\
    "$(JDK_OINCL)\signature.h"\
    "$(JDK_OINCL)\stubpreamble.h"\
    "$(JDK_OINCL)\typedefs.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
    "$(JDK_MDOINCL)\oobj_md.h"\
    "$(JDK_MDOINCL)\typedefs_md.h"\
    

"$(INTDIR)\xerox_ilu_ilutypecode.obj" : $(SOURCE) $(DEP_CPP_XEROX_ILU_T)\
 "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_XEROX_ILU_T=\
    "$(JDK_OINCL)\bool.h"\
    "$(JDK_OINCL)\debug.h"\
    "$(JDK_OINCL)\interpreter.h"\
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_OINCL)\oobj.h"\
    "$(JDK_OINCL)\signature.h"\
    "$(JDK_OINCL)\stubpreamble.h"\
    "$(JDK_OINCL)\typedefs.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
    "$(JDK_MDOINCL)\oobj_md.h"\
    "$(JDK_MDOINCL)\typedefs_md.h"\
    

"$(INTDIR)\xerox_ilu_ilutypecode.obj"   "$(INTDIR)\xerox_ilu_ilutypecode.sbr" : \
$(SOURCE) $(DEP_CPP_XEROX_ILU_T) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 


SOURCE=$(ILUSRC)\runtime\java\build\xerox_ilu_iluwpbase.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_XEROX_ILU_W=\
    "$(JDK_OINCL)\bool.h"\
    "$(JDK_OINCL)\debug.h"\
    "$(JDK_OINCL)\interpreter.h"\
    "$(JDK_OINCL)\jcov.h"\
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_OINCL)\oobj.h"\
    "$(JDK_OINCL)\signature.h"\
    "$(JDK_OINCL)\stubpreamble.h"\
    "$(JDK_OINCL)\typedefs.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
    "$(JDK_MDOINCL)\oobj_md.h"\
    "$(JDK_MDOINCL)\typedefs_md.h"\
    

"$(INTDIR)\xerox_ilu_iluwpbase.obj" : $(SOURCE) $(DEP_CPP_XEROX_ILU_W)\
 "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_XEROX_ILU_W=\
    "$(JDK_OINCL)\bool.h"\
    "$(JDK_OINCL)\debug.h"\
    "$(JDK_OINCL)\interpreter.h"\
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_OINCL)\oobj.h"\
    "$(JDK_OINCL)\signature.h"\
    "$(JDK_OINCL)\stubpreamble.h"\
    "$(JDK_OINCL)\typedefs.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
    "$(JDK_MDOINCL)\oobj_md.h"\
    "$(JDK_MDOINCL)\typedefs_md.h"\
    

"$(INTDIR)\xerox_ilu_iluwpbase.obj" "$(INTDIR)\xerox_ilu_iluwpbase.sbr" : \
$(SOURCE) $(DEP_CPP_XEROX_ILU_W) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 


!ENDIF 

