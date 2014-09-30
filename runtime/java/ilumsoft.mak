# Microsoft Developer Studio Generated NMAKE File, Based on ilujava.dsp
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

ALL : "$(ILUHOME)\bin\ilujava_ms.dll"

!ELSE 

ALL : "$(ILUHOME)\bin\ilujava_ms.dll"

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
    -@erase "$(INTDIR)\IluJava_IluPickle.obj"
    -@erase "$(INTDIR)\IluJava_IluPort.obj"
    -@erase "$(INTDIR)\IluJava_IluRT0.obj"
    -@erase "$(INTDIR)\IluJava_IluServer.obj"
    -@erase "$(INTDIR)\IluJava_JMon.obj"
    -@erase "$(INTDIR)\IluJava_JOps.obj"
    -@erase "$(INTDIR)\IluJava_LockTech.obj"
    -@erase "$(INTDIR)\IluJava_RNI.obj"
    -@erase "$(INTDIR)\IluJava_selectwt.obj"
    -@erase "$(INTDIR)\ilunative.obj"
    -@erase "$(INTDIR)\vc50.idb"
    -@erase "$(OUTDIR)\ilujava_ms.exp"


"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MD /W3 /GX /O2  /I\
 "$(JAVA_HOME)\include" /I "$(ILUSRC)\runtime\java" /I\
 "$(ILUSRC)\runtime\java\build" /I "$(ILUSRC)\runtime\kernel" /I "$(ILUSRC)\GSS\kernel" /D\
 "NDEBUG" /D "WIN32" /D "_WINDOWS" /D "ILU_BUILDING_RUNTIME" \
  /D "RNI" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
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
BSC32_FLAGS=/nologo /o"$(OUTDIR)\ilujava_ms.bsc" 
BSC32_SBRS= \
    
LINK32=link.exe
LINK32_FLAGS=$(ILUHOME)\lib\ilu32.lib $(ILUHOME)\lib\gss.lib $(JAVA_HOME)\lib\i386\msjava.lib\
 wsock32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib\
 advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib\
 odbccp32.lib /nologo /subsystem:windows /dll /incremental:no\
 /pdb:"$(OUTDIR)\ilujava_ms.pdb" /machine:I386 /out:"$(ILUHOME)\bin\ilujava_ms.dll"\
 /implib:"$(OUTDIR)\ilujava_ms.lib" 
LINK32_OBJS= \
    "$(INTDIR)\IluJava_GCClient.obj" \
    "$(INTDIR)\IluJava_IluAlarmTech.obj" \
    "$(INTDIR)\IluJava_IluCall.obj" \
    "$(INTDIR)\IluJava_IluClassRep.obj" \
    "$(INTDIR)\IluJava_IluConnOps.obj" \
    "$(INTDIR)\IluJava_IluDebug.obj" \
    "$(INTDIR)\IluJava_IluInit.obj" \
    "$(INTDIR)\IluJava_IluOInt.obj" \
    "$(INTDIR)\IluJava_IluPickle.obj" \
    "$(INTDIR)\IluJava_IluPort.obj" \
    "$(INTDIR)\IluJava_IluRT0.obj" \
    "$(INTDIR)\IluJava_IluServer.obj" \
    "$(INTDIR)\IluJava_JMon.obj" \
    "$(INTDIR)\IluJava_JOps.obj" \
    "$(INTDIR)\IluJava_LockTech.obj" \
    "$(INTDIR)\IluJava_RNI.obj" \
    "$(INTDIR)\IluJava_selectwt.obj" \
    "$(INTDIR)\ilunative.obj"

"$(ILUHOME)\bin\ilujava_ms.dll" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
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

ALL : "$(ILUHOME)\bin\ilujava_ms.dll"

!ELSE 

ALL : "$(ILUHOME)\bin\ilujava_ms.dll"

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
    -@erase "$(INTDIR)\IluJava_IluPickle.obj"
    -@erase "$(INTDIR)\IluJava_IluPort.obj"
    -@erase "$(INTDIR)\IluJava_IluRT0.obj"
    -@erase "$(INTDIR)\IluJava_IluServer.obj"
    -@erase "$(INTDIR)\IluJava_JMon.obj"
    -@erase "$(INTDIR)\IluJava_JOps.obj"
    -@erase "$(INTDIR)\IluJava_LockTech.obj"
    -@erase "$(INTDIR)\IluJava_RNI.obj"
    -@erase "$(INTDIR)\IluJava_selectwt.obj"
    -@erase "$(INTDIR)\ilunative.obj"
    -@erase "$(INTDIR)\vc50.idb"
    -@erase "$(INTDIR)\vc50.pdb"
    -@erase "$(OUTDIR)\ilujava_ms.bsc"
    -@erase "$(OUTDIR)\ilujava_ms.exp"
    -@erase "$(OUTDIR)\ilujava_ms.lib"
    -@erase "$(OUTDIR)\ilujava_ms.pdb"
    -@erase "$(ILUHOME)\bin\ilujava_ms.dll"
    -@erase "$(ILUHOME)\bin\ilujava_ms.ilk"
    -@erase "$(ILUHOME)\browsefiles\IluJava_GCClient.sbr"
    -@erase "$(ILUHOME)\browsefiles\IluJava_IluAlarmTech.sbr"
    -@erase "$(ILUHOME)\browsefiles\IluJava_IluCall.sbr"
    -@erase "$(ILUHOME)\browsefiles\IluJava_IluClassRep.sbr"
    -@erase "$(ILUHOME)\browsefiles\IluJava_IluConnOps.sbr"
    -@erase "$(ILUHOME)\browsefiles\IluJava_IluDebug.sbr"
    -@erase "$(ILUHOME)\browsefiles\IluJava_IluInit.sbr"
    -@erase "$(ILUHOME)\browsefiles\IluJava_IluOInt.sbr"
    -@erase "$(ILUHOME)\browsefiles\IluJava_IluPickle.sbr"
    -@erase "$(ILUHOME)\browsefiles\IluJava_IluPort.sbr"
    -@erase "$(ILUHOME)\browsefiles\IluJava_IluRT0.sbr"
    -@erase "$(ILUHOME)\browsefiles\IluJava_IluServer.sbr"
    -@erase "$(ILUHOME)\browsefiles\IluJava_JMon.sbr"
    -@erase "$(ILUHOME)\browsefiles\IluJava_JOps.sbr"
    -@erase "$(ILUHOME)\browsefiles\IluJava_LockTech.sbr"
    -@erase "$(ILUHOME)\browsefiles\IluJava_RNI.sbr"
    -@erase "$(ILUHOME)\browsefiles\IluJava_selectwt.sbr"
    -@erase "$(ILUHOME)\browsefiles\ilunative.sbr"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MD /W3 /Gm /GX /Zi /Od /I "$(JAVA_HOME)\include" /I\
 "$(ILUSRC)\runtime\java" /I "$(ILUSRC)\runtime\java\build" /I\
 "$(ILUSRC)\runtime\kernel" /I "$(ILUSRC)\GSS\kernel" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D\
 "ILU_BUILDING_RUNTIME" \
 /D "RNI" $(ILU_DEBUG_JFLAGS)\
 /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
CPP_OBJS=.\WinDebug/
CPP_SBRS=$(ILUHOME)\browsefiles/

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
BSC32_FLAGS=/nologo /o"$(OUTDIR)\ilujava_ms.bsc" 
BSC32_SBRS= \
    "$(ILUHOME)\browsefiles\IluJava_GCClient.sbr" \
    "$(ILUHOME)\browsefiles\IluJava_IluAlarmTech.sbr" \
    "$(ILUHOME)\browsefiles\IluJava_IluCall.sbr" \
    "$(ILUHOME)\browsefiles\IluJava_IluClassRep.sbr" \
    "$(ILUHOME)\browsefiles\IluJava_IluConnOps.sbr" \
    "$(ILUHOME)\browsefiles\IluJava_IluDebug.sbr" \
    "$(ILUHOME)\browsefiles\IluJava_IluInit.sbr" \
    "$(ILUHOME)\browsefiles\IluJava_IluOInt.sbr" \
    "$(ILUHOME)\browsefiles\IluJava_IluPickle.sbr" \
    "$(ILUHOME)\browsefiles\IluJava_IluPort.sbr" \
    "$(ILUHOME)\browsefiles\IluJava_IluRT0.sbr" \
    "$(ILUHOME)\browsefiles\IluJava_IluServer.sbr" \
    "$(ILUHOME)\browsefiles\IluJava_JMon.sbr" \
    "$(ILUHOME)\browsefiles\IluJava_JOps.sbr" \
    "$(ILUHOME)\browsefiles\IluJava_LockTech.sbr" \
    "$(ILUHOME)\browsefiles\IluJava_RNI.sbr" \
    "$(ILUHOME)\browsefiles\IluJava_selectwt.sbr" \
    "$(ILUHOME)\browsefiles\ilunative.sbr"

"$(OUTDIR)\ilujava_ms.bsc" : "$(OUTDIR)" $(BSC32_SBRS)
    $(BSC32) @<<
  $(BSC32_FLAGS) $(BSC32_SBRS)
<<

LINK32=link.exe
LINK32_FLAGS=$(ILUHOME)\lib\ilu32.lib $(ILUHOME)\lib\gss.lib $(JAVA_HOME)\lib\i386\msjava.lib\
 wsock32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib\
 advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib\
 odbccp32.lib /nologo /subsystem:windows /dll /incremental:yes\
 /pdb:"$(OUTDIR)\ilujava_ms.pdb" /debug /machine:I386\
 /out:"$(ILUHOME)\bin\ilujava_ms.dll" /implib:"$(OUTDIR)\ilujava_ms.lib" /pdbtype:sept 
LINK32_OBJS= \
    "$(INTDIR)\IluJava_GCClient.obj" \
    "$(INTDIR)\IluJava_IluAlarmTech.obj" \
    "$(INTDIR)\IluJava_IluCall.obj" \
    "$(INTDIR)\IluJava_IluClassRep.obj" \
    "$(INTDIR)\IluJava_IluConnOps.obj" \
    "$(INTDIR)\IluJava_IluDebug.obj" \
    "$(INTDIR)\IluJava_IluInit.obj" \
    "$(INTDIR)\IluJava_IluOInt.obj" \
    "$(INTDIR)\IluJava_IluPickle.obj" \
    "$(INTDIR)\IluJava_IluPort.obj" \
    "$(INTDIR)\IluJava_IluRT0.obj" \
    "$(INTDIR)\IluJava_IluServer.obj" \
    "$(INTDIR)\IluJava_JMon.obj" \
    "$(INTDIR)\IluJava_JOps.obj" \
    "$(INTDIR)\IluJava_LockTech.obj" \
    "$(INTDIR)\IluJava_RNI.obj" \
    "$(INTDIR)\IluJava_selectwt.obj" \
    "$(INTDIR)\ilunative.obj"

"$(ILUHOME)\bin\ilujava_ms.dll" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
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
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluexceptionrep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilugcclient.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilumethodrep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluobject.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluobjecttable.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluoint.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilupassport.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilupickle.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluport.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilubatcher.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilurt0.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluserver.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluserverrelocationinfo.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluserializationcontext.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilusurrogateobject.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilutransportinfo.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilutypecode.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilutyperep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluwpbase.h"\
    "$(ILUSRC)\runtime\java\ilujava_common.h"\
    "$(ILUSRC)\runtime\java\ilujava_includes.h"\
    "$(ILUSRC)\runtime\java\ilujava_jmem.h"\
    "$(ILUSRC)\runtime\java\ilujava_jstubs.h"\
    "$(ILUSRC)\runtime\java\ilujava_jtypes.h"\
    "$(ILUSRC)\runtime\java\ilujava_rni.h"\
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
    "$(JAVA_HOME)\include\native.h"\
    

"$(INTDIR)\IluJava_GCClient.obj" : $(SOURCE) $(DEP_CPP_ILUJA) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_ILUJA=\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilucall.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluclassrep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluexceptionrep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilugcclient.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilumethodrep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluobject.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluobjecttable.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluoint.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilupassport.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilupickle.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluport.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilubatcher.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilurt0.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluserver.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluserverrelocationinfo.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluserializationcontext.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilusurrogateobject.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilutransportinfo.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilutypecode.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilutyperep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluwpbase.h"\
    "$(ILUSRC)\runtime\java\ilujava_common.h"\
    "$(ILUSRC)\runtime\java\ilujava_includes.h"\
    "$(ILUSRC)\runtime\java\ilujava_jmem.h"\
    "$(ILUSRC)\runtime\java\ilujava_jstubs.h"\
    "$(ILUSRC)\runtime\java\ilujava_jtypes.h"\
    "$(ILUSRC)\runtime\java\ilujava_rni.h"\
    "$(ILUSRC)\runtime\java\ilunative.h"\
    "$(ILUSRC)\runtime\kernel\ilubasic.h"\
    "$(ILUSRC)\runtime\kernel\iluerror.h"\
    "$(ILUSRC)\runtime\kernel\iluerrs.h"\
    "$(ILUSRC)\runtime\kernel\iluhash.h"\
    "$(ILUSRC)\runtime\kernel\ilutpcod.h"\
    "$(ILUSRC)\runtime\kernel\ilutypes.h"\
    "$(ILUSRC)\runtime\kernel\iluwin.h"\
    "$(ILUSRC)\runtime\kernel\iluxport.h"\
    "$(JAVA_HOME)\include\native.h"\
    

"$(INTDIR)\IluJava_GCClient.obj"\
    "$(ILUHOME)\browsefiles\IluJava_GCClient.sbr" : $(SOURCE) $(DEP_CPP_ILUJA)\
 "$(INTDIR)"
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
    "$(ILUSRC)\runtime\java\ilujava_rni.h"\
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
    "$(JAVA_HOME)\include\native.h"\
    

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
    "$(ILUSRC)\runtime\java\ilujava_rni.h"\
    "$(ILUSRC)\runtime\java\ilunative.h"\
    "$(ILUSRC)\runtime\kernel\ilubasic.h"\
    "$(ILUSRC)\runtime\kernel\iluerror.h"\
    "$(ILUSRC)\runtime\kernel\iluerrs.h"\
    "$(ILUSRC)\runtime\kernel\iluhash.h"\
    "$(ILUSRC)\runtime\kernel\ilutpcod.h"\
    "$(ILUSRC)\runtime\kernel\ilutypes.h"\
    "$(ILUSRC)\runtime\kernel\iluwin.h"\
    "$(ILUSRC)\runtime\kernel\iluxport.h"\
    "$(JAVA_HOME)\include\native.h"\
    

"$(INTDIR)\IluJava_IluAlarmTech.obj"\
    "$(ILUHOME)\browsefiles\IluJava_IluAlarmTech.sbr" : $(SOURCE)\
 $(DEP_CPP_ILUJAV) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=$(ILUSRC)\runtime\java\IluJava_IluCall.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_ILUJAVA=\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilucall.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluclassrep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluexceptionrep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilumethodrep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluobject.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluobjecttable.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluoint.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilupassport.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilupickle.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluport.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilubatcher.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilurt0.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluserver.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluserverrelocationinfo.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluserializationcontext.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilusurrogateobject.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilutransportinfo.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilutypecode.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilutyperep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluwpbase.h"\
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
    "$(ILUSRC)\runtime\java\ilujava_rni.h"\
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
    "$(JAVA_HOME)\include\native.h"\
    

"$(INTDIR)\IluJava_IluCall.obj" : $(SOURCE) $(DEP_CPP_ILUJAVA) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_ILUJAVA=\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilucall.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluclassrep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluexceptionrep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilumethodrep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluobject.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluobjecttable.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluoint.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilupassport.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilupickle.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluport.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilubatcher.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilurt0.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluserver.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluserverrelocationinfo.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluserializationcontext.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilusurrogateobject.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilutransportinfo.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilutypecode.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilutyperep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluwpbase.h"\
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
    "$(ILUSRC)\runtime\java\ilujava_rni.h"\
    "$(ILUSRC)\runtime\java\ilunative.h"\
    "$(ILUSRC)\runtime\kernel\ilubasic.h"\
    "$(ILUSRC)\runtime\kernel\iluerror.h"\
    "$(ILUSRC)\runtime\kernel\iluerrs.h"\
    "$(ILUSRC)\runtime\kernel\iluhash.h"\
    "$(ILUSRC)\runtime\kernel\ilutpcod.h"\
    "$(ILUSRC)\runtime\kernel\ilutypes.h"\
    "$(ILUSRC)\runtime\kernel\iluwin.h"\
    "$(ILUSRC)\runtime\kernel\iluxport.h"\
    "$(JAVA_HOME)\include\native.h"\
    

"$(INTDIR)\IluJava_IluCall.obj" "$(ILUHOME)\browsefiles\IluJava_IluCall.sbr"\
 : $(SOURCE) $(DEP_CPP_ILUJAVA) "$(INTDIR)"
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
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilupickle.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluport.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilubatcher.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilurt0.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluserver.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluserverrelocationinfo.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluserializationcontext.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilusurrogateobject.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilutransportinfo.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilutypecode.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilutyperep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluwpbase.h"\
    "$(ILUSRC)\runtime\java\ilujava_common.h"\
    "$(ILUSRC)\runtime\java\ilujava_includes.h"\
    "$(ILUSRC)\runtime\java\ilujava_jarrays.h"\
    "$(ILUSRC)\runtime\java\ilujava_jgc.h"\
    "$(ILUSRC)\runtime\java\ilujava_jmem.h"\
    "$(ILUSRC)\runtime\java\ilujava_jstubs.h"\
    "$(ILUSRC)\runtime\java\ilujava_jtypes.h"\
    "$(ILUSRC)\runtime\java\ilujava_rni.h"\
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
    "$(JAVA_HOME)\include\native.h"\
    

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
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilupickle.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluport.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilubatcher.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilurt0.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluserver.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluserverrelocationinfo.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluserializationcontext.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilusurrogateobject.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilutransportinfo.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilutypecode.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilutyperep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluwpbase.h"\
    "$(ILUSRC)\runtime\java\ilujava_common.h"\
    "$(ILUSRC)\runtime\java\ilujava_includes.h"\
    "$(ILUSRC)\runtime\java\ilujava_jarrays.h"\
    "$(ILUSRC)\runtime\java\ilujava_jgc.h"\
    "$(ILUSRC)\runtime\java\ilujava_jmem.h"\
    "$(ILUSRC)\runtime\java\ilujava_jstubs.h"\
    "$(ILUSRC)\runtime\java\ilujava_jtypes.h"\
    "$(ILUSRC)\runtime\java\ilujava_rni.h"\
    "$(ILUSRC)\runtime\java\ilunative.h"\
    "$(ILUSRC)\runtime\kernel\ilubasic.h"\
    "$(ILUSRC)\runtime\kernel\iluerror.h"\
    "$(ILUSRC)\runtime\kernel\iluerrs.h"\
    "$(ILUSRC)\runtime\kernel\iluhash.h"\
    "$(ILUSRC)\runtime\kernel\ilutpcod.h"\
    "$(ILUSRC)\runtime\kernel\ilutypes.h"\
    "$(ILUSRC)\runtime\kernel\iluwin.h"\
    "$(ILUSRC)\runtime\kernel\iluxport.h"\
    "$(JAVA_HOME)\include\native.h"\
    

"$(INTDIR)\IluJava_IluClassRep.obj"\
    "$(ILUHOME)\browsefiles\IluJava_IluClassRep.sbr" : $(SOURCE)\
 $(DEP_CPP_ILUJAVA_) "$(INTDIR)"
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
    "$(ILUSRC)\runtime\java\ilujava_rni.h"\
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
    "$(JAVA_HOME)\include\native.h"\
    

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
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilusurrogateconnection.h"\
    "$(ILUSRC)\runtime\java\ilujava_common.h"\
    "$(ILUSRC)\runtime\java\ilujava_includes.h"\
    "$(ILUSRC)\runtime\java\ilujava_jgc.h"\
    "$(ILUSRC)\runtime\java\ilujava_jmem.h"\
    "$(ILUSRC)\runtime\java\ilujava_jtypes.h"\
    "$(ILUSRC)\runtime\java\ilujava_rni.h"\
    "$(ILUSRC)\runtime\java\ilunative.h"\
    "$(ILUSRC)\runtime\kernel\ilubasic.h"\
    "$(ILUSRC)\runtime\kernel\iluerror.h"\
    "$(ILUSRC)\runtime\kernel\iluerrs.h"\
    "$(ILUSRC)\runtime\kernel\iluhash.h"\
    "$(ILUSRC)\runtime\kernel\ilutpcod.h"\
    "$(ILUSRC)\runtime\kernel\ilutypes.h"\
    "$(ILUSRC)\runtime\kernel\iluwin.h"\
    "$(ILUSRC)\runtime\kernel\iluxport.h"\
    "$(JAVA_HOME)\include\native.h"\
    

"$(INTDIR)\IluJava_IluConnOps.obj"\
    "$(ILUHOME)\browsefiles\IluJava_IluConnOps.sbr" : $(SOURCE)\
 $(DEP_CPP_ILUJAVA_I) "$(INTDIR)"
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
    "$(ILUSRC)\runtime\java\ilujava_jgc.h"\
    "$(ILUSRC)\runtime\java\ilujava_jmem.h"\
    "$(ILUSRC)\runtime\java\ilujava_jtypes.h"\
    "$(ILUSRC)\runtime\java\ilujava_rni.h"\
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
    "$(JAVA_HOME)\include\native.h"\
    

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
    "$(ILUSRC)\runtime\java\ilujava_jgc.h"\
    "$(ILUSRC)\runtime\java\ilujava_jmem.h"\
    "$(ILUSRC)\runtime\java\ilujava_jtypes.h"\
    "$(ILUSRC)\runtime\java\ilujava_rni.h"\
    "$(ILUSRC)\runtime\java\ilunative.h"\
    "$(ILUSRC)\runtime\kernel\ilubasic.h"\
    "$(ILUSRC)\runtime\kernel\iluerror.h"\
    "$(ILUSRC)\runtime\kernel\iluerrs.h"\
    "$(ILUSRC)\runtime\kernel\iluhash.h"\
    "$(ILUSRC)\runtime\kernel\ilutpcod.h"\
    "$(ILUSRC)\runtime\kernel\ilutypes.h"\
    "$(ILUSRC)\runtime\kernel\iluwin.h"\
    "$(ILUSRC)\runtime\kernel\iluxport.h"\
    "$(JAVA_HOME)\include\native.h"\
    

"$(INTDIR)\IluJava_IluDebug.obj"\
    "$(ILUHOME)\browsefiles\IluJava_IluDebug.sbr" : $(SOURCE)\
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
    "$(ILUSRC)\runtime\java\ilujava_rni.h"\
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
    "$(JAVA_HOME)\include\native.h"\
    

"$(INTDIR)\IluJava_IluInit.obj" : $(SOURCE) $(DEP_CPP_ILUJAVA_ILU) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_ILUJAVA_ILU=\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluinit2.h"\
    "$(ILUSRC)\runtime\java\ilujava_common.h"\
    "$(ILUSRC)\runtime\java\ilujava_includes.h"\
    "$(ILUSRC)\runtime\java\ilujava_jmem.h"\
    "$(ILUSRC)\runtime\java\ilujava_jtypes.h"\
    "$(ILUSRC)\runtime\java\ilujava_rni.h"\
    "$(ILUSRC)\runtime\java\ilunative.h"\
    "$(ILUSRC)\runtime\kernel\ilubasic.h"\
    "$(ILUSRC)\runtime\kernel\iluerror.h"\
    "$(ILUSRC)\runtime\kernel\iluerrs.h"\
    "$(ILUSRC)\runtime\kernel\iluhash.h"\
    "$(ILUSRC)\runtime\kernel\ilutpcod.h"\
    "$(ILUSRC)\runtime\kernel\ilutypes.h"\
    "$(ILUSRC)\runtime\kernel\iluwin.h"\
    "$(ILUSRC)\runtime\kernel\iluxport.h"\
    "$(JAVA_HOME)\include\native.h"\
    

"$(INTDIR)\IluJava_IluInit.obj" "$(ILUHOME)\browsefiles\IluJava_IluInit.sbr"\
 : $(SOURCE) $(DEP_CPP_ILUJAVA_ILU) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=$(ILUSRC)\runtime\java\IluJava_IluOInt.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_ILUJAVA_ILUO=\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilucall.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluclassrep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluexceptionrep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilumethodrep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluobject.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluobjecttable.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluoint.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilupassport.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilupickle.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluport.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilubatcher.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilurt0.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluserver.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluserverrelocationinfo.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluserializationcontext.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilusurrogateobject.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilutransportinfo.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilutypecode.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilutyperep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluwpbase.h"\
    "$(ILUSRC)\runtime\java\ilujava_common.h"\
    "$(ILUSRC)\runtime\java\ilujava_includes.h"\
    "$(ILUSRC)\runtime\java\ilujava_jgc.h"\
    "$(ILUSRC)\runtime\java\ilujava_jmem.h"\
    "$(ILUSRC)\runtime\java\ilujava_jmon.h"\
    "$(ILUSRC)\runtime\java\ilujava_jstubs.h"\
    "$(ILUSRC)\runtime\java\ilujava_jtypes.h"\
    "$(ILUSRC)\runtime\java\ilujava_ops.h"\
    "$(ILUSRC)\runtime\java\ilujava_rni.h"\
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
    "$(JAVA_HOME)\include\native.h"\
    

"$(INTDIR)\IluJava_IluOInt.obj" : $(SOURCE) $(DEP_CPP_ILUJAVA_ILUO) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_ILUJAVA_ILUO=\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilucall.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluclassrep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluexceptionrep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilumethodrep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluobject.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluobjecttable.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluoint.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilupassport.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilupickle.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluport.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilubatcher.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilurt0.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluserver.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluserverrelocationinfo.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluserializationcontext.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilusurrogateobject.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilutransportinfo.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilutypecode.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilutyperep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluwpbase.h"\
    "$(ILUSRC)\runtime\java\ilujava_common.h"\
    "$(ILUSRC)\runtime\java\ilujava_includes.h"\
    "$(ILUSRC)\runtime\java\ilujava_jgc.h"\
    "$(ILUSRC)\runtime\java\ilujava_jmem.h"\
    "$(ILUSRC)\runtime\java\ilujava_jmon.h"\
    "$(ILUSRC)\runtime\java\ilujava_jstubs.h"\
    "$(ILUSRC)\runtime\java\ilujava_jtypes.h"\
    "$(ILUSRC)\runtime\java\ilujava_ops.h"\
    "$(ILUSRC)\runtime\java\ilujava_rni.h"\
    "$(ILUSRC)\runtime\java\ilunative.h"\
    "$(ILUSRC)\runtime\kernel\ilubasic.h"\
    "$(ILUSRC)\runtime\kernel\iluerror.h"\
    "$(ILUSRC)\runtime\kernel\iluerrs.h"\
    "$(ILUSRC)\runtime\kernel\iluhash.h"\
    "$(ILUSRC)\runtime\kernel\ilutpcod.h"\
    "$(ILUSRC)\runtime\kernel\ilutypes.h"\
    "$(ILUSRC)\runtime\kernel\iluwin.h"\
    "$(ILUSRC)\runtime\kernel\iluxport.h"\
    "$(JAVA_HOME)\include\native.h"\
    

"$(INTDIR)\IluJava_IluOInt.obj" "$(ILUHOME)\browsefiles\IluJava_IluOInt.sbr"\
 : $(SOURCE) $(DEP_CPP_ILUJAVA_ILUO) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=$(ILUSRC)\runtime\java\IluJava_IluPickle.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_ILUJAVA_ILUP=\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilucall.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluclassrep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluexceptionrep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilumethodrep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluobject.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluobjecttable.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluoint.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilupassport.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilupickle.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluport.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilubatcher.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilurt0.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluserver.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluserverrelocationinfo.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluserializationcontext.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilusurrogateobject.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilutransportinfo.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilutypecode.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilutyperep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluwpbase.h"\
    "$(ILUSRC)\runtime\java\ilujava_callmacros.h"\
    "$(ILUSRC)\runtime\java\ilujava_common.h"\
    "$(ILUSRC)\runtime\java\ilujava_includes.h"\
    "$(ILUSRC)\runtime\java\ilujava_jarrays.h"\
    "$(ILUSRC)\runtime\java\ilujava_jgc.h"\
    "$(ILUSRC)\runtime\java\ilujava_jmem.h"\
    "$(ILUSRC)\runtime\java\ilujava_jops.h"\
    "$(ILUSRC)\runtime\java\ilujava_jstubs.h"\
    "$(ILUSRC)\runtime\java\ilujava_jtypes.h"\
    "$(ILUSRC)\runtime\java\ilujava_ops.h"\
    "$(ILUSRC)\runtime\java\ilujava_rni.h"\
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
    "$(JAVA_HOME)\include\native.h"\
    

"$(INTDIR)\IluJava_IluPickle.obj" : $(SOURCE) $(DEP_CPP_ILUJAVA_ILUP)\
 "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_ILUJAVA_ILUP=\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilucall.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluclassrep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluexceptionrep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilumethodrep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluobject.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluobjecttable.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluoint.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilupassport.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilupickle.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluport.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilubatcher.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilurt0.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluserver.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluserverrelocationinfo.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluserializationcontext.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilusurrogateobject.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilutransportinfo.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilutypecode.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilutyperep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluwpbase.h"\
    "$(ILUSRC)\runtime\java\ilujava_callmacros.h"\
    "$(ILUSRC)\runtime\java\ilujava_common.h"\
    "$(ILUSRC)\runtime\java\ilujava_includes.h"\
    "$(ILUSRC)\runtime\java\ilujava_jarrays.h"\
    "$(ILUSRC)\runtime\java\ilujava_jgc.h"\
    "$(ILUSRC)\runtime\java\ilujava_jmem.h"\
    "$(ILUSRC)\runtime\java\ilujava_jops.h"\
    "$(ILUSRC)\runtime\java\ilujava_jstubs.h"\
    "$(ILUSRC)\runtime\java\ilujava_jtypes.h"\
    "$(ILUSRC)\runtime\java\ilujava_ops.h"\
    "$(ILUSRC)\runtime\java\ilujava_rni.h"\
    "$(ILUSRC)\runtime\java\ilunative.h"\
    "$(ILUSRC)\runtime\kernel\ilubasic.h"\
    "$(ILUSRC)\runtime\kernel\iluerror.h"\
    "$(ILUSRC)\runtime\kernel\iluerrs.h"\
    "$(ILUSRC)\runtime\kernel\iluhash.h"\
    "$(ILUSRC)\runtime\kernel\ilutpcod.h"\
    "$(ILUSRC)\runtime\kernel\ilutypes.h"\
    "$(ILUSRC)\runtime\kernel\iluwin.h"\
    "$(ILUSRC)\runtime\kernel\iluxport.h"\
    "$(JAVA_HOME)\include\native.h"\
    

"$(INTDIR)\IluJava_IluPickle.obj"\
    "$(ILUHOME)\browsefiles\IluJava_IluPickle.sbr" : $(SOURCE)\
 $(DEP_CPP_ILUJAVA_ILUP) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=$(ILUSRC)\runtime\java\IluJava_IluPort.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_ILUJAVA_ILUPO=\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilucall.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluclassrep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluexceptionrep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilumethodrep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluobject.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluobjecttable.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluoint.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilupassport.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilupickle.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluport.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilubatcher.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilurt0.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilusbh.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluserver.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluserverrelocationinfo.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluserializationcontext.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilusurrogateobject.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilutransportinfo.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilutypecode.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilutyperep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluwpbase.h"\
    "$(ILUSRC)\runtime\java\ilujava_common.h"\
    "$(ILUSRC)\runtime\java\ilujava_includes.h"\
    "$(ILUSRC)\runtime\java\ilujava_jarrays.h"\
    "$(ILUSRC)\runtime\java\ilujava_jgc.h"\
    "$(ILUSRC)\runtime\java\ilujava_jmem.h"\
    "$(ILUSRC)\runtime\java\ilujava_jstubs.h"\
    "$(ILUSRC)\runtime\java\ilujava_jtypes.h"\
    "$(ILUSRC)\runtime\java\ilujava_rni.h"\
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
    "$(JAVA_HOME)\include\native.h"\
    

"$(INTDIR)\IluJava_IluPort.obj" : $(SOURCE) $(DEP_CPP_ILUJAVA_ILUPO)\
 "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_ILUJAVA_ILUPO=\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilucall.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluclassrep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluexceptionrep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilumethodrep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluobject.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluobjecttable.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluoint.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilupassport.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilupickle.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluport.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilubatcher.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilurt0.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilusbh.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluserver.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluserverrelocationinfo.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluserializationcontext.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilusurrogateobject.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilutransportinfo.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilutypecode.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilutyperep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluwpbase.h"\
    "$(ILUSRC)\runtime\java\ilujava_common.h"\
    "$(ILUSRC)\runtime\java\ilujava_includes.h"\
    "$(ILUSRC)\runtime\java\ilujava_jarrays.h"\
    "$(ILUSRC)\runtime\java\ilujava_jgc.h"\
    "$(ILUSRC)\runtime\java\ilujava_jmem.h"\
    "$(ILUSRC)\runtime\java\ilujava_jstubs.h"\
    "$(ILUSRC)\runtime\java\ilujava_jtypes.h"\
    "$(ILUSRC)\runtime\java\ilujava_rni.h"\
    "$(ILUSRC)\runtime\java\ilunative.h"\
    "$(ILUSRC)\runtime\kernel\ilubasic.h"\
    "$(ILUSRC)\runtime\kernel\iluerror.h"\
    "$(ILUSRC)\runtime\kernel\iluerrs.h"\
    "$(ILUSRC)\runtime\kernel\iluhash.h"\
    "$(ILUSRC)\runtime\kernel\ilutpcod.h"\
    "$(ILUSRC)\runtime\kernel\ilutypes.h"\
    "$(ILUSRC)\runtime\kernel\iluwin.h"\
    "$(ILUSRC)\runtime\kernel\iluxport.h"\
    "$(JAVA_HOME)\include\native.h"\
    

"$(INTDIR)\IluJava_IluPort.obj" "$(ILUHOME)\browsefiles\IluJava_IluPort.sbr"\
 : $(SOURCE) $(DEP_CPP_ILUJAVA_ILUPO) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=$(ILUSRC)\runtime\java\IluJava_IluRT0.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_ILUJAVA_ILUR=\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilucall.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluclassrep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluexceptionrep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilumethodrep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluobject.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluobjecttable.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluoint.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilupassport.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilupickle.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluport.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilubatcher.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilurt0.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluserver.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluserverrelocationinfo.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluserializationcontext.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilusurrogateobject.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilutransportinfo.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilutypecode.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilutyperep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluwpbase.h"\
    "$(ILUSRC)\runtime\java\ilujava_common.h"\
    "$(ILUSRC)\runtime\java\ilujava_includes.h"\
    "$(ILUSRC)\runtime\java\ilujava_jgc.h"\
    "$(ILUSRC)\runtime\java\ilujava_jmem.h"\
    "$(ILUSRC)\runtime\java\ilujava_jstubs.h"\
    "$(ILUSRC)\runtime\java\ilujava_jtypes.h"\
    "$(ILUSRC)\runtime\java\ilujava_ops.h"\
    "$(ILUSRC)\runtime\java\ilujava_rni.h"\
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
    "$(JAVA_HOME)\include\native.h"\
    

"$(INTDIR)\IluJava_IluRT0.obj" : $(SOURCE) $(DEP_CPP_ILUJAVA_ILUR) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_ILUJAVA_ILUR=\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilucall.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluclassrep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluexceptionrep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilumethodrep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluobject.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluobjecttable.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluoint.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilupassport.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilupickle.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluport.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilubatcher.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilurt0.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluserver.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluserverrelocationinfo.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluserializationcontext.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilusurrogateobject.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilutransportinfo.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilutypecode.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilutyperep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluwpbase.h"\
    "$(ILUSRC)\runtime\java\ilujava_common.h"\
    "$(ILUSRC)\runtime\java\ilujava_includes.h"\
    "$(ILUSRC)\runtime\java\ilujava_jgc.h"\
    "$(ILUSRC)\runtime\java\ilujava_jmem.h"\
    "$(ILUSRC)\runtime\java\ilujava_jstubs.h"\
    "$(ILUSRC)\runtime\java\ilujava_jtypes.h"\
    "$(ILUSRC)\runtime\java\ilujava_ops.h"\
    "$(ILUSRC)\runtime\java\ilujava_rni.h"\
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
    "$(JAVA_HOME)\include\native.h"\
    

"$(INTDIR)\IluJava_IluRT0.obj"  "$(ILUHOME)\browsefiles\IluJava_IluRT0.sbr" : \
$(SOURCE) $(DEP_CPP_ILUJAVA_ILUR) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=$(ILUSRC)\runtime\java\IluJava_IluServer.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_ILUJAVA_ILUS=\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilucall.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluclassrep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluexceptionrep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilumethodrep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluobject.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluobjecttable.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluoint.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilupassport.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilupickle.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluport.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilubatcher.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilurt0.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluserver.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluserverrelocationinfo.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluserializationcontext.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilusurrogateobject.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilutransportinfo.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilutypecode.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilutyperep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluwpbase.h"\
    "$(ILUSRC)\runtime\java\ilujava_common.h"\
    "$(ILUSRC)\runtime\java\ilujava_includes.h"\
    "$(ILUSRC)\runtime\java\ilujava_jgc.h"\
    "$(ILUSRC)\runtime\java\ilujava_jmem.h"\
    "$(ILUSRC)\runtime\java\ilujava_jmon.h"\
    "$(ILUSRC)\runtime\java\ilujava_jstubs.h"\
    "$(ILUSRC)\runtime\java\ilujava_jtypes.h"\
    "$(ILUSRC)\runtime\java\ilujava_rni.h"\
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
    "$(JAVA_HOME)\include\native.h"\
    

"$(INTDIR)\IluJava_IluServer.obj" : $(SOURCE) $(DEP_CPP_ILUJAVA_ILUS)\
 "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_ILUJAVA_ILUS=\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilucall.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluclassrep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluexceptionrep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilumethodrep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluobject.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluobjecttable.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluoint.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilupassport.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilupickle.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluport.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilubatcher.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilurt0.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluserver.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluserverrelocationinfo.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluserializationcontext.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilusurrogateobject.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilutransportinfo.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilutypecode.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_ilutyperep.h"\
    "$(ILUSRC)\runtime\java\build\xerox_ilu_iluwpbase.h"\
    "$(ILUSRC)\runtime\java\ilujava_common.h"\
    "$(ILUSRC)\runtime\java\ilujava_includes.h"\
    "$(ILUSRC)\runtime\java\ilujava_jgc.h"\
    "$(ILUSRC)\runtime\java\ilujava_jmem.h"\
    "$(ILUSRC)\runtime\java\ilujava_jmon.h"\
    "$(ILUSRC)\runtime\java\ilujava_jstubs.h"\
    "$(ILUSRC)\runtime\java\ilujava_jtypes.h"\
    "$(ILUSRC)\runtime\java\ilujava_rni.h"\
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
    "$(JAVA_HOME)\include\native.h"\
    

"$(INTDIR)\IluJava_IluServer.obj"\
    "$(ILUHOME)\browsefiles\IluJava_IluServer.sbr" : $(SOURCE)\
 $(DEP_CPP_ILUJAVA_ILUS) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=$(ILUSRC)\runtime\java\IluJava_JMon.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_ILUJAVA_J=\
    "$(ILUSRC)\runtime\java\ilujava_includes.h"\
    "$(ILUSRC)\runtime\java\ilujava_jmem.h"\
    "$(ILUSRC)\runtime\java\ilujava_jmon.h"\
    "$(ILUSRC)\runtime\java\ilujava_jtypes.h"\
    "$(ILUSRC)\runtime\java\ilujava_rni.h"\
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
    "$(JAVA_HOME)\include\native.h"\
    

"$(INTDIR)\IluJava_JMon.obj" : $(SOURCE) $(DEP_CPP_ILUJAVA_J) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_ILUJAVA_J=\
    "$(ILUSRC)\runtime\java\ilujava_includes.h"\
    "$(ILUSRC)\runtime\java\ilujava_jmem.h"\
    "$(ILUSRC)\runtime\java\ilujava_jmon.h"\
    "$(ILUSRC)\runtime\java\ilujava_jtypes.h"\
    "$(ILUSRC)\runtime\java\ilujava_rni.h"\
    "$(ILUSRC)\runtime\kernel\ilubasic.h"\
    "$(ILUSRC)\runtime\kernel\iluerror.h"\
    "$(ILUSRC)\runtime\kernel\iluerrs.h"\
    "$(ILUSRC)\runtime\kernel\iluhash.h"\
    "$(ILUSRC)\runtime\kernel\ilutpcod.h"\
    "$(ILUSRC)\runtime\kernel\ilutypes.h"\
    "$(ILUSRC)\runtime\kernel\iluwin.h"\
    "$(ILUSRC)\runtime\kernel\iluxport.h"\
    "$(JAVA_HOME)\include\native.h"\
    

"$(INTDIR)\IluJava_JMon.obj"    "$(ILUHOME)\browsefiles\IluJava_JMon.sbr" : \
$(SOURCE) $(DEP_CPP_ILUJAVA_J) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=$(ILUSRC)\runtime\java\IluJava_JOps.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_ILUJAVA_JO=\
    "$(ILUSRC)\runtime\java\ilujava_includes.h"\
    "$(ILUSRC)\runtime\java\ilujava_jarrays.h"\
    "$(ILUSRC)\runtime\java\ilujava_jops.h"\
    "$(ILUSRC)\runtime\java\ilujava_jtypes.h"\
    "$(ILUSRC)\runtime\java\ilujava_rni.h"\
    "$(ILUSRC)\runtime\kernel\ilubasic.h"\
    "$(ILUSRC)\runtime\kernel\iluerror.h"\
    "$(ILUSRC)\runtime\kernel\iluerrs.h"\
    "$(ILUSRC)\runtime\kernel\iluhash.h"\
    "$(ILUSRC)\runtime\kernel\ilumac.h"\
    "$(ILUSRC)\runtime\kernel\ilutpcod.h"\
    "$(ILUSRC)\runtime\kernel\ilutypes.h"\
    "$(ILUSRC)\runtime\kernel\iluwin.h"\
    "$(ILUSRC)\runtime\kernel\iluxport.h"\
    "$(JAVA_HOME)\include\native.h"\
    

"$(INTDIR)\IluJava_JOps.obj" : $(SOURCE) $(DEP_CPP_ILUJAVA_JO) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_ILUJAVA_JO=\
    "$(ILUSRC)\runtime\java\ilujava_includes.h"\
    "$(ILUSRC)\runtime\java\ilujava_jarrays.h"\
    "$(ILUSRC)\runtime\java\ilujava_jops.h"\
    "$(ILUSRC)\runtime\java\ilujava_jtypes.h"\
    "$(ILUSRC)\runtime\java\ilujava_rni.h"\
    "$(ILUSRC)\runtime\kernel\ilubasic.h"\
    "$(ILUSRC)\runtime\kernel\iluerror.h"\
    "$(ILUSRC)\runtime\kernel\iluerrs.h"\
    "$(ILUSRC)\runtime\kernel\iluhash.h"\
    "$(ILUSRC)\runtime\kernel\ilutpcod.h"\
    "$(ILUSRC)\runtime\kernel\ilutypes.h"\
    "$(ILUSRC)\runtime\kernel\iluwin.h"\
    "$(ILUSRC)\runtime\kernel\iluxport.h"\
    "$(JAVA_HOME)\include\native.h"\
    

"$(INTDIR)\IluJava_JOps.obj"    "$(ILUHOME)\browsefiles\IluJava_JOps.sbr" : \
$(SOURCE) $(DEP_CPP_ILUJAVA_JO) "$(INTDIR)"
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
    "$(ILUSRC)\runtime\java\ilujava_rni.h"\
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
    "$(JAVA_HOME)\include\native.h"\
    

"$(INTDIR)\IluJava_LockTech.obj" : $(SOURCE) $(DEP_CPP_ILUJAVA_L) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_ILUJAVA_L=\
    "$(ILUSRC)\runtime\java\ilujava_common.h"\
    "$(ILUSRC)\runtime\java\ilujava_includes.h"\
    "$(ILUSRC)\runtime\java\ilujava_jmem.h"\
    "$(ILUSRC)\runtime\java\ilujava_jmon.h"\
    "$(ILUSRC)\runtime\java\ilujava_jtypes.h"\
    "$(ILUSRC)\runtime\java\ilujava_rni.h"\
    "$(ILUSRC)\runtime\java\ilunative.h"\
    "$(ILUSRC)\runtime\kernel\ilubasic.h"\
    "$(ILUSRC)\runtime\kernel\iluerror.h"\
    "$(ILUSRC)\runtime\kernel\iluerrs.h"\
    "$(ILUSRC)\runtime\kernel\iluhash.h"\
    "$(ILUSRC)\runtime\kernel\ilutpcod.h"\
    "$(ILUSRC)\runtime\kernel\ilutypes.h"\
    "$(ILUSRC)\runtime\kernel\iluwin.h"\
    "$(ILUSRC)\runtime\kernel\iluxport.h"\
    "$(JAVA_HOME)\include\native.h"\
    

"$(INTDIR)\IluJava_LockTech.obj"\
    "$(ILUHOME)\browsefiles\IluJava_LockTech.sbr" : $(SOURCE)\
 $(DEP_CPP_ILUJAVA_L) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=$(ILUSRC)\runtime\java\IluJava_RNI.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_ILUJAVA_R=\
    "$(ILUSRC)\runtime\java\ilujava_common.h"\
    "$(ILUSRC)\runtime\java\ilujava_includes.h"\
    "$(ILUSRC)\runtime\java\ilujava_jarrays.h"\
    "$(ILUSRC)\runtime\java\ilujava_jmem.h"\
    "$(ILUSRC)\runtime\java\ilujava_jtypes.h"\
    "$(ILUSRC)\runtime\java\ilujava_rni.h"\
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
    "$(JAVA_HOME)\include\native.h"\
    

"$(INTDIR)\IluJava_RNI.obj" : $(SOURCE) $(DEP_CPP_ILUJAVA_R) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_ILUJAVA_R=\
    "$(ILUSRC)\runtime\java\ilujava_common.h"\
    "$(ILUSRC)\runtime\java\ilujava_includes.h"\
    "$(ILUSRC)\runtime\java\ilujava_jarrays.h"\
    "$(ILUSRC)\runtime\java\ilujava_jmem.h"\
    "$(ILUSRC)\runtime\java\ilujava_jtypes.h"\
    "$(ILUSRC)\runtime\java\ilujava_rni.h"\
    "$(ILUSRC)\runtime\java\ilunative.h"\
    "$(ILUSRC)\runtime\kernel\ilubasic.h"\
    "$(ILUSRC)\runtime\kernel\iluerror.h"\
    "$(ILUSRC)\runtime\kernel\iluerrs.h"\
    "$(ILUSRC)\runtime\kernel\iluhash.h"\
    "$(ILUSRC)\runtime\kernel\ilutpcod.h"\
    "$(ILUSRC)\runtime\kernel\ilutypes.h"\
    "$(ILUSRC)\runtime\kernel\iluwin.h"\
    "$(ILUSRC)\runtime\kernel\iluxport.h"\
    "$(JAVA_HOME)\include\native.h"\
    

"$(INTDIR)\IluJava_RNI.obj" "$(ILUHOME)\browsefiles\IluJava_RNI.sbr" : \
$(SOURCE) $(DEP_CPP_ILUJAVA_R) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=$(ILUSRC)\runtime\java\IluJava_selectwt.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_ILUJAVA_S=\
    "$(ILUSRC)\runtime\java\ilujava_common.h"\
    "$(ILUSRC)\runtime\java\ilujava_includes.h"\
    "$(ILUSRC)\runtime\java\ilujava_jmem.h"\
    "$(ILUSRC)\runtime\java\ilujava_jtypes.h"\
    "$(ILUSRC)\runtime\java\ilujava_rni.h"\
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
    "$(JAVA_HOME)\include\native.h"\
    {$(INCLUDE)}"sys\types.h"\
    

"$(INTDIR)\IluJava_selectwt.obj" : $(SOURCE) $(DEP_CPP_ILUJAVA_S) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_ILUJAVA_S=\
    "$(ILUSRC)\runtime\java\ilujava_common.h"\
    "$(ILUSRC)\runtime\java\ilujava_includes.h"\
    "$(ILUSRC)\runtime\java\ilujava_jmem.h"\
    "$(ILUSRC)\runtime\java\ilujava_jtypes.h"\
    "$(ILUSRC)\runtime\java\ilujava_rni.h"\
    "$(ILUSRC)\runtime\java\ilunative.h"\
    "$(ILUSRC)\runtime\kernel\ilubasic.h"\
    "$(ILUSRC)\runtime\kernel\iluerror.h"\
    "$(ILUSRC)\runtime\kernel\iluerrs.h"\
    "$(ILUSRC)\runtime\kernel\iluhash.h"\
    "$(ILUSRC)\runtime\kernel\ilutpcod.h"\
    "$(ILUSRC)\runtime\kernel\ilutypes.h"\
    "$(ILUSRC)\runtime\kernel\iluwin.h"\
    "$(ILUSRC)\runtime\kernel\iluxport.h"\
    "$(JAVA_HOME)\include\native.h"\
    

"$(INTDIR)\IluJava_selectwt.obj"\
    "$(ILUHOME)\browsefiles\IluJava_selectwt.sbr" : $(SOURCE)\
 $(DEP_CPP_ILUJAVA_S) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=$(ILUSRC)\runtime\java\ilunative.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_ILUNA=\
    "$(ILUSRC)\runtime\java\ilujava_includes.h"\
    "$(ILUSRC)\runtime\java\ilujava_jtypes.h"\
    "$(ILUSRC)\runtime\java\ilujava_rni.h"\
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
    "$(JAVA_HOME)\include\native.h"\
    

"$(INTDIR)\ilunative.obj" : $(SOURCE) $(DEP_CPP_ILUNA) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_ILUNA=\
    "$(ILUSRC)\runtime\java\ilujava_includes.h"\
    "$(ILUSRC)\runtime\java\ilujava_jtypes.h"\
    "$(ILUSRC)\runtime\java\ilujava_rni.h"\
    "$(ILUSRC)\runtime\java\ilunative.h"\
    "$(ILUSRC)\runtime\kernel\ilubasic.h"\
    "$(ILUSRC)\runtime\kernel\iluerror.h"\
    "$(ILUSRC)\runtime\kernel\iluerrs.h"\
    "$(ILUSRC)\runtime\kernel\iluhash.h"\
    "$(ILUSRC)\runtime\kernel\ilutpcod.h"\
    "$(ILUSRC)\runtime\kernel\ilutypes.h"\
    "$(ILUSRC)\runtime\kernel\iluwin.h"\
    "$(ILUSRC)\runtime\kernel\iluxport.h"\
    "$(JAVA_HOME)\include\native.h"\
    

"$(INTDIR)\ilunative.obj"   "$(ILUHOME)\browsefiles\ilunative.sbr" : $(SOURCE)\
 $(DEP_CPP_ILUNA) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 


!ENDIF 

