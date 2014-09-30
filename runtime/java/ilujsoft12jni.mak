# Microsoft Developer Studio Generated NMAKE File, Based on ilujava.dsp
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
/I  "$(JDK_MDNINCL)" /I "$(JDK_NINCL)" \
/I "$(ILUSRC)\runtime\java" \
/I "$(ILUSRC)\runtime\java\build" /I "$(ILUSRC)\runtime\kernel" /I "$(ILUSRC)\GSS\kernel" \
/D "NDEBUG" /D "JNI" /D "WIN32" /D "_WINDOWS" \
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
    "$(INTDIR)\ilunative.obj"

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
/I  "$(JDK_MDNINCL)" /I "$(JDK_NINCL)" \
/I "$(ILUSRC)\runtime\java" \
/I "$(ILUSRC)\runtime\java\build" /I "$(ILUSRC)\runtime\kernel" /I "$(ILUSRC)\GSS\kernel" \
/D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "JNI" /D "ILU_BUILDING_RUNTIME" /D\
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
    "$(INTDIR)\ilunative.obj" 


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
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
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
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_MDNINCL)\jni_md.h"
    

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
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
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
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_MDNINCL)\jni_md.h"
    

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
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
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
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_MDNINCL)\jni_md.h"
    

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
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
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
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_MDNINCL)\jni_md.h"
    

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
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
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
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_MDNINCL)\jni_md.h"
    

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
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
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
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_MDNINCL)\jni_md.h"
    

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
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
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
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_MDNINCL)\jni_md.h"
    

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
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
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
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_MDNINCL)\jni_md.h"
    

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
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
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
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_MDNINCL)\jni_md.h"
    

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
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
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
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_MDNINCL)\jni_md.h"
    

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
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
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
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_MDNINCL)\jni_md.h"
    

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
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_MDNINCL)\jni_md.h"
    

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
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_MDNINCL)\jni_md.h"
    

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
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_MDNINCL)\jni_md.h"
    

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
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_MDNINCL)\jni_md.h"
    

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
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
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
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_MDNINCL)\jni_md.h"
    

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
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
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
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_MDNINCL)\jni_md.h"
    

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
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_MDNINCL)\jni_md.h"\
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
    "$(JDK_NINCL)\jni.h"\
    "$(JDK_MDNINCL)\jni_md.h"
    

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



!ENDIF 

